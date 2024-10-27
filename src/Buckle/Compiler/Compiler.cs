using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Threading;
using Buckle.CodeAnalysis;
using Buckle.CodeAnalysis.Evaluating;
using Buckle.CodeAnalysis.Syntax;
using Buckle.CodeAnalysis.Text;
using Buckle.Diagnostics;
using Diagnostics;
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

    private int CalculateExitCode(BelteDiagnosticQueue diagnostics) {
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

        var buildMode = state.buildMode == BuildMode.AutoRun ? textLength switch {
            // ! Temporary, `-i` will not use `--script` until it allows entry points such as `Main`
            // <= InterpreterMaxTextLength when textsCount == 1 => BuildMode.Interpret,
            <= EvaluatorMaxTextLength => BuildMode.Evaluate,
            // ! Temporary, `-i` will not use `--execute` until it is implemented
            // _ => BuildMode.Execute
            _ => BuildMode.Evaluate,
        } : state.buildMode;

        if (buildMode is BuildMode.Evaluate or BuildMode.Execute) {
            var syntaxTrees = new List<SyntaxTree>();

            for (var i = 0; i < state.tasks.Length; i++) {
                ref var task = ref state.tasks[i];

                if (task.stage == CompilerStage.Raw) {
                    var syntaxTree = SyntaxTree.Load(task.inputFileName, task.fileContent.text);
                    syntaxTrees.Add(syntaxTree);
                    task.stage = CompilerStage.Finished;
                }
            }

            var compilation = Compilation.Create(
                state.moduleName,
                _options,
                CompilerHelpers.LoadLibraries(_options),
                syntaxTrees.ToArray()
            );

            if (state.noOut)
                return compilation.GetParseDiagnostics();

            if (state.verboseMode) {
                timer.Stop();
                diagnostics.Push(new BelteDiagnostic(
                    DiagnosticSeverity.Debug,
                    $"Loaded {syntaxTrees.Count} syntax trees in {timer.ElapsedMilliseconds} ms"
                ));
                timer.Start();
            }

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

            var sourceText = new StringText(state.tasks[0].inputFileName, state.tasks[0].fileContent.text);
            var syntaxTree = new SyntaxTree(sourceText, SourceCodeKind.Regular);

            state.tasks[0].stage = CompilerStage.Finished;

            var options = _options;
            options.isScript = true;
            var compilation = Compilation.Create(state.moduleName, options, syntaxTree);
            EvaluationResult result = null;

            if (state.verboseMode && !state.noOut) {
                timer.Stop();
                diagnostics.Push(new BelteDiagnostic(
                    DiagnosticSeverity.Debug,
                    $"Loaded 1 syntax tree in {timer.ElapsedMilliseconds} ms"
                ));
                timer.Start();
            }

            void Wrapper(object parameter) {
                result = compilation.Interpret((ValueWrapper<bool>)parameter);
            }

            InternalInterpreterStart(Wrapper);

            diagnostics.Move(result?.diagnostics);
        }

        if (state.verboseMode && !state.noOut) {
            timer.Stop();
            diagnostics.Push(new BelteDiagnostic(
                DiagnosticSeverity.Debug,
                $"Total compilation time: {timer.ElapsedMilliseconds} ms"
            ));
        }

        return diagnostics;
    }

    private BelteDiagnosticQueue InternalCompiler() {
        var diagnostics = new BelteDiagnosticQueue();
        var timer = state.verboseMode ? Stopwatch.StartNew() : null;
        var syntaxTrees = new List<SyntaxTree>();

        for (var i = 0; i < state.tasks.Length; i++) {
            ref var task = ref state.tasks[i];

            if (task.stage == CompilerStage.Raw) {
                var syntaxTree = SyntaxTree.Load(task.inputFileName, task.fileContent.text);
                syntaxTrees.Add(syntaxTree);
                task.stage = CompilerStage.Compiled;
            }
        }

        var compilation = Compilation.Create(
            state.moduleName,
            _options,
            CompilerHelpers.LoadLibraries(_options),
            syntaxTrees.ToArray()
        );

        if (state.noOut)
            return diagnostics;

        if (state.verboseMode) {
            timer.Stop();
            diagnostics.Push(new BelteDiagnostic(
                DiagnosticSeverity.Debug,
                $"Loaded {syntaxTrees.Count} syntax trees in {timer.ElapsedMilliseconds} ms"
            ));
            timer.Start();
        }

        var result = compilation.Emit(
            state.outputFilename,
            state.verboseMode
        );

        diagnostics.Move(result);

        if (state.verboseMode) {
            timer.Stop();
            diagnostics.Push(new BelteDiagnostic(
                DiagnosticSeverity.Debug,
                $"Total compilation time: {timer.ElapsedMilliseconds} ms"
            ));
        }

        return diagnostics;
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
