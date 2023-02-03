using System.Collections.Generic;
using System.Linq;
using Buckle.CodeAnalysis;
using Buckle.CodeAnalysis.Evaluating;
using Buckle.CodeAnalysis.Preprocessing;
using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;
using Buckle.Diagnostics;
using Diagnostics;

namespace Buckle;

/// <summary>
/// Handles compiling and handling a single <see cref="CompilerState" />.
/// Multiple can be created and run asynchronously.
/// </summary>
public sealed class Compiler {
    private const int SuccessExitCode = 0;
    private const int ErrorExitCode = 1;
    private const int FatalExitCode = 2;

    /// <summary>
    /// Creates a new <see cref="Compiler" />, state needs to be set separately.
    /// </summary>
    public Compiler() {
        diagnostics = new BelteDiagnosticQueue();
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
    /// Where the diagnostics are stored for the compiler before being displayed or logged.
    /// </summary>
    public BelteDiagnosticQueue diagnostics { get; set; }

    /// <summary>
    /// Handles preprocessing, compiling, assembling, and linking of a set of files.
    /// </summary>
    /// <returns>Error code, 0 = success.</returns>
    public int Compile() {
        int err;

        InternalPreprocessor();

        err = CheckErrors();
        if (err != SuccessExitCode)
            return err;

        if (state.finishStage == CompilerStage.Preprocessed)
            return SuccessExitCode;

        if (state.buildMode == BuildMode.Interpret) {
            InternalInterpreter();
            return CheckErrors();
        } else {
            InternalCompiler();
            return CheckErrors();
        }
    }

    private int CheckErrors() {
        var worst = SuccessExitCode;

        foreach (Diagnostic diagnostic in diagnostics)
            if (diagnostic.info.severity == DiagnosticType.Error)
                worst = ErrorExitCode;

        return worst;
    }

    private void InternalPreprocessor() {
        var preprocessor = new Preprocessor();

        for (int i=0; i<state.tasks.Length; i++) {
            ref FileState task = ref state.tasks[i];

            if (task.stage == CompilerStage.Raw)
                task.stage = CompilerStage.Preprocessed;

            var text = preprocessor.PreprocessText(task.inputFilename, task.fileContent.text);
            task.fileContent.text = text;
        }

        diagnostics.Move(preprocessor.diagnostics);
    }

    private void InternalInterpreter() {
        diagnostics.Clear(DiagnosticType.Warning);
        var syntaxTrees = new List<SyntaxTree>();

        for (int i=0; i<state.tasks.Length; i++) {
            ref FileState task = ref state.tasks[i];

            if (task.stage == CompilerStage.Preprocessed) {
                var syntaxTree = SyntaxTree.Load(task.inputFilename, task.fileContent.text);
                syntaxTrees.Add(syntaxTree);
                task.stage = CompilerStage.Compiled;
            }
        }

        var compilation = Compilation.Create(false, syntaxTrees.ToArray());
        diagnostics.Move(compilation.diagnostics);

        if (!state.options.Contains("all") && !state.options.Contains("error"))
            diagnostics = diagnostics.FilterOut(DiagnosticType.Warning);

        if ((diagnostics.FilterOut(DiagnosticType.Warning).Any()) ||
            (diagnostics.Any() && state.options.Contains("error")))
            return;

        var _ = false; // Unused, just to satisfy ref parameter
        var result = compilation.Evaluate(
            new Dictionary<VariableSymbol, EvaluatorObject>(), ref _, state.options.Contains("error")
        );

        if (!state.options.Contains("all") && !state.options.Contains("error"))
            diagnostics.Move(result.diagnostics.FilterOut(DiagnosticType.Warning));
        else
            diagnostics.Move(result.diagnostics);
    }

    private void InternalCompiler() {
        var syntaxTrees = new List<SyntaxTree>();

        for (int i=0; i<state.tasks.Length; i++) {
            ref FileState task = ref state.tasks[i];

            if (task.stage == CompilerStage.Preprocessed) {
                var syntaxTree = SyntaxTree.Load(task.inputFilename, task.fileContent.text);
                syntaxTrees.Add(syntaxTree);
                task.stage = CompilerStage.Compiled;
            }
        }

        var compilation = Compilation.Create(state.buildMode == BuildMode.CSharpTranspile, syntaxTrees.ToArray());
        var result = compilation.Emit(
            state.buildMode, state.moduleName, state.references, state.outputFilename,
            state.options.Contains("error"), state.finishStage
        );

        diagnostics.Move(result);
    }
}
