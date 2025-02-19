using System;
using System.Diagnostics;
using System.Threading;
using Buckle.CodeAnalysis;
using Buckle.CodeAnalysis.Syntax;
using Buckle.CodeAnalysis.Text;
using Buckle.Diagnostics;
using Diagnostics;
using Microsoft.CodeAnalysis.PooledObjects;
using Shared;

namespace Buckle;

/// <summary>
/// Handles compiling and handling a single <see cref="CompilerState" />.
/// Multiple can be created and run asynchronously.
/// </summary>
public sealed class Compiler {
    private const int SuccessExitCode = 0;
    private const int ErrorExitCode = 1;
    private const int FatalExitCode = 2;
    // Eventually have these automatically calculated to be optimal
    private const int InterpreterMaxTextLength = 4096;
    private const int EvaluatorMaxTextLength = 4096 * 4;

    private CompilationOptions _options => new CompilationOptions(
        state.buildMode,
        state.projectType,
        state.arguments,
        false,
        !state.noOut,
        state.references
    );

    /// <summary>
    /// Creates a new <see cref="Compiler" />, state needs to be set separately.
    /// </summary>
    public Compiler(CompilerState state) {
        this.state = state;
    }

    /// <summary>
    /// Compiler specific state that determines what to compile and how.
    /// Required to compile.
    /// </summary>
    public CompilerState state { get; set; }

    /// <summary>
    /// The name of the compiler (usually displayed with diagnostics).
    /// </summary>
    public string me { get; set; }

    /// <summary>
    /// The diagnostics from the most recent compiler operation.
    /// </summary>
    public BelteDiagnosticQueue diagnostics { get; private set; } = new BelteDiagnosticQueue();

    /// <summary>
    /// Handles compiling, assembling, and linking of a set of files.
    /// </summary>
    /// <returns>Error code, 0 = success.</returns>
    public int Compile() {
        lock (state) lock (me) {
                diagnostics.Clear();

                if (state.buildMode is BuildMode.AutoRun or BuildMode.Interpret or BuildMode.Evaluate or BuildMode.Execute)
                    diagnostics = InternalInterpreter();
                else
                    diagnostics = InternalCompiler();

                return CalculateExitCode(diagnostics);
            }
    }

    private static int CalculateExitCode(BelteDiagnosticQueue diagnostics) {
        var worst = SuccessExitCode;

        foreach (Diagnostic diagnostic in diagnostics) {
            if (diagnostic.info.severity == DiagnosticSeverity.Error)
                worst = ErrorExitCode;
        }

        return worst;
    }

    private BelteDiagnosticQueue InternalInterpreter() {
        var diagnostics = new BelteDiagnosticQueue();
        var timer = state.verboseMode ? Stopwatch.StartNew() : null;
        var textLength = 0;
        var textsCount = 0;

        foreach (var task in state.tasks) {
            if (task.fileContent.text is not null) {
                textLength += task.fileContent.text.Length;
                textsCount++;
            }
        }

        var buildMode = state.buildMode != BuildMode.AutoRun
            ? state.buildMode
            : textLength switch {
                // ! Temporary, `-i` will not use `--script` until it allows entry points such as `Main`
                // <= InterpreterMaxTextLength when textsCount == 1 => BuildMode.Interpret,
                <= EvaluatorMaxTextLength => BuildMode.Evaluate,
                // ! Temporary, `-i` will not use `--execute` until it is implemented
                // _ => BuildMode.Execute
                _ => BuildMode.Evaluate,
            };

        if (buildMode is BuildMode.Evaluate or BuildMode.Execute) {
            var syntaxTrees = CreateSyntaxTrees(CompilerStage.Finished);
            var previous = CompilerHelpers.LoadLibraries(_options);
            var compilation = Compilation.Create(state.moduleName, _options, previous, syntaxTrees);

            if (state.noOut)
                return compilation.GetParseDiagnostics();

            LogParseTime(timer, syntaxTrees.Length);

            void Wrapper(object parameter) {
                if (buildMode == BuildMode.Evaluate) {
                    var result = compilation.Evaluate((ValueWrapper<bool>)parameter, state.verboseMode);
                    diagnostics.PushRange(result.diagnostics);
                } else {
                    diagnostics.PushRange(compilation.Execute());
                }
            }

            InternalInterpreterStart(Wrapper);
        } else {
            Debug.Assert(state.tasks.Length == 1, "multiple tasks while in script mode");

            ref var task = ref state.tasks[0];
            var sourceText = new StringText(task.inputFileName, task.fileContent.text);
            var syntaxTree = new SyntaxTree(sourceText, SourceCodeKind.Regular);
            task.stage = CompilerStage.Finished;

            var compilation = Compilation.CreateScript(state.moduleName, _options, syntaxTree);

            if (state.noOut)
                return compilation.GetParseDiagnostics();

            LogParseTime(timer, 1);

            void Wrapper(object parameter) {
                var result = compilation.Interpret((ValueWrapper<bool>)parameter);
                diagnostics.PushRange(result.diagnostics);
            }

            InternalInterpreterStart(Wrapper);
        }

        LogCompilationTime(timer);

        return diagnostics;
    }

    private BelteDiagnosticQueue InternalCompiler() {
        var diagnostics = new BelteDiagnosticQueue();
        var timer = state.verboseMode ? Stopwatch.StartNew() : null;
        var syntaxTrees = CreateSyntaxTrees(CompilerStage.Compiled);

        var compilation = Compilation.Create(
            state.moduleName,
            _options,
            CompilerHelpers.LoadLibraries(_options),
            syntaxTrees
        );

        if (state.noOut)
            return diagnostics;

        LogParseTime(timer, syntaxTrees.Length);

        var result = compilation.Emit(state.outputFilename, state.verboseMode);
        diagnostics.PushRange(result);

        LogCompilationTime(timer);

        return diagnostics;
    }

    private SyntaxTree[] CreateSyntaxTrees(CompilerStage stageToSet) {
        var builder = ArrayBuilder<SyntaxTree>.GetInstance();
        var tasks = state.tasks;

        for (var i = 0; i < tasks.Length; i++) {
            ref var task = ref tasks[i];

            if (task.stage == CompilerStage.Raw) {
                var syntaxTree = SyntaxTree.Load(task.inputFileName, task.fileContent.text);
                builder.Add(syntaxTree);
                task.stage = stageToSet;
            }
        }

        return builder.ToArrayAndFree();
    }

    private void LogParseTime(Stopwatch timer, int count) {
        Log(timer, $"Loaded {count} syntax tree in {timer.ElapsedMilliseconds} ms");
    }

    private void LogCompilationTime(Stopwatch timer) {
        Log(timer, $"Total compilation time: {timer.ElapsedMilliseconds} ms");
    }

    private void Log(Stopwatch timer, string message) {
        if (state.verboseMode) {
            timer.Stop();
            diagnostics.Push(new BelteDiagnostic(DiagnosticSeverity.Debug, message));
            timer.Start();
        }
    }

    private void InternalInterpreterStart(Action<object> wrapper) {
        ValueWrapper<bool> abort = false;

        void CtrlCHandler(object sender, ConsoleCancelEventArgs args) {
            if (state.buildMode != BuildMode.Execute) {
                abort.Value = true;
                args.Cancel = true;
            }
        }

        Console.CancelKeyPress += new ConsoleCancelEventHandler(CtrlCHandler);

        var wrapperReference = new ParameterizedThreadStart(wrapper);
        var wrapperThread = new Thread(wrapperReference) {
            Name = "Compiler.InternalInterpreterStart"
        };

        wrapperThread.Start(abort);

        while (wrapperThread.IsAlive)
            ;
    }
}
