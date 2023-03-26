using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Threading;
using Buckle.CodeAnalysis.Binding;
using Buckle.CodeAnalysis.Emitting;
using Buckle.CodeAnalysis.Evaluating;
using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;
using Buckle.Diagnostics;

namespace Buckle.CodeAnalysis;

/// <summary>
/// Handles evaluation of program, and keeps track of Symbols.
/// </summary>
public sealed class Compilation {
    private BoundGlobalScope _globalScope;
    private bool _transpilerMode;

    private Compilation(bool isScript, Compilation previous, bool transpilerMode, params SyntaxTree[] syntaxTrees) {
        this.isScript = isScript;
        this.previous = previous;
        _transpilerMode = transpilerMode;
        diagnostics = new BelteDiagnosticQueue();

        foreach (var syntaxTree in syntaxTrees)
            diagnostics.Move(syntaxTree.diagnostics);

        this.syntaxTrees = syntaxTrees.ToImmutableArray<SyntaxTree>();
    }

    /// <summary>
    /// Diagnostics relating to the <see cref="Compilation" />.
    /// </summary>
    public BelteDiagnosticQueue diagnostics { get; set; }

    /// <summary>
    /// The main method/entry point of the program.
    /// </summary>
    internal MethodSymbol mainMethod => globalScope.mainMethod;

    /// <summary>
    /// All MethodSymbols in the global scope.
    /// </summary>
    internal ImmutableArray<MethodSymbol> methods => globalScope.methods;

    /// <summary>
    /// All VariableSymbols in the global scope.
    /// </summary>
    internal ImmutableArray<VariableSymbol> variables => globalScope.variables;

    /// <summary>
    /// All TypeSymbols in the global scope
    /// </summary>
    internal ImmutableArray<TypeSymbol> types => globalScope.types;

    /// <summary>
    /// The SyntaxTrees of the parsed source files.
    /// </summary>
    internal ImmutableArray<SyntaxTree> syntaxTrees { get; }

    /// <summary>
    /// Previous <see cref="Compilation" />.
    /// </summary>
    internal Compilation previous { get; }

    /// <summary>
    /// If the compilation is a script to run top down versus being an application with an entry point.
    /// </summary>
    internal bool isScript { get; }

    /// <summary>
    /// The global scope (top level) of the program, contains Symbols.
    /// </summary>
    internal BoundGlobalScope globalScope {
        get {
            if (_globalScope == null) {
                var tempScope = Binder.BindGlobalScope(isScript, previous?.globalScope, syntaxTrees, _transpilerMode);
                // Makes assignment thread-safe, if multiple threads try to initialize they use whoever did it first
                Interlocked.CompareExchange(ref _globalScope, tempScope, null);
            }

            return _globalScope;
        }
    }

    /// <summary>
    /// Creates a new <see cref="Compilation" /> with SyntaxTrees.
    /// </summary>
    /// <param name="transpilerMode">
    /// If the compiler output mode is a transpiler. Affects certain optimizations.
    /// </param>
    /// <param name="syntaxTrees">SyntaxTrees to use during compilation.</param>
    /// <returns>New <see cref="Compilation" />.</returns>
    internal static Compilation Create(bool transpilerMode = false, params SyntaxTree[] syntaxTrees) {
        return new Compilation(false, null, transpilerMode, syntaxTrees);
    }

    /// <summary>
    /// Creates a new script <see cref="Compilation" /> with SyntaxTrees, and the previous <see cref="Compilation" />.
    /// </summary>
    /// <param name="previous">Previous <see cref="Compilation" />.</param>
    /// <param name="syntaxTrees">SyntaxTrees to use during compilation.</param>
    /// <returns>.</returns>
    internal static Compilation CreateScript(Compilation previous, params SyntaxTree[] syntaxTrees) {
        return new Compilation(true, previous, false, syntaxTrees);
    }

    /// <summary>
    /// Gets all Symbols across submissions (only global scope).
    /// </summary>
    /// <returns>All Symbols (checks all previous Compilations).</returns>
    internal IEnumerable<Symbol> GetSymbols() => GetSymbols<Symbol>();

    /// <summary>
    /// Gets all Symbols of certain child type across submissions (only global scope).
    /// </summary>
    /// <typeparam name="T">Type of <see cref="Symbol" /> to get.</typeparam>
    /// <returns>Found symbols.</returns>
    internal IEnumerable<T> GetSymbols<T>() where T : Symbol {
        var submission = this;
        var seenSymbolNames = new HashSet<string>();
        var builtins = BuiltinMethods.GetAll();

        while (submission != null) {
            foreach (var method in submission.methods)
                if (seenSymbolNames.Add(method.SignatureNoReturnNoParameterNames()) && method is T)
                    yield return method as T;

            foreach (var builtin in builtins)
                if (seenSymbolNames.Add(builtin.SignatureNoReturnNoParameterNames()) && builtin is T)
                    yield return builtin as T;

            foreach (var variable in submission.variables)
                if (seenSymbolNames.Add(variable.name) && variable is T)
                    yield return variable as T;

            foreach (var @type in submission.types)
                if (seenSymbolNames.Add(@type.name) && @type is T)
                    yield return @type as T;

            submission = submission.previous;
        }
    }

    /// <summary>
    /// Evaluates SyntaxTrees.
    /// </summary>
    /// <param name="variables">Existing variables to add to the scope.</param>
    /// <param name="abort">External flag used to cancel evaluation.</param>
    /// <returns>Result of evaluation (see <see cref="EvaluationResult" />).</returns>
    internal EvaluationResult Evaluate(Dictionary<VariableSymbol, EvaluatorObject> variables, ref bool abort) {
        if (globalScope.diagnostics.Errors().Any())
            return new EvaluationResult(null, false, globalScope.diagnostics, null);

        var program = GetProgram();
        // * Only for debugging purposes
        // CreateCfg(program);

        if (program.diagnostics.Errors().Any())
            return new EvaluationResult(null, false, program.diagnostics, null);

        diagnostics.Move(program.diagnostics);
        var eval = new Evaluator(program, variables);
        var evalResult = eval.Evaluate(ref abort, out var hasValue);

        if (eval.hasPrint)
            Console.WriteLine();

        diagnostics.Move(eval.diagnostics);
        var result = new EvaluationResult(evalResult, hasValue, diagnostics, eval.exceptions);
        return result;
    }

    /// <summary>
    /// Emits the program to an assembly.
    /// </summary>
    /// <param name="buildMode">Which emitter to use.</param>
    /// <param name="moduleName">Application name.</param>
    /// <param name="references">All external references (.NET).</param>
    /// <param name="outputPath">Where to put the application once assembled.</param>
    /// <param name="finishStage">
    /// What stage to finish at (only applicable if <param name="buildMode" /> is set to
    /// <see cref="BuildMode.Independent" />.
    /// </param>
    /// <returns>Diagnostics.</returns>
    internal BelteDiagnosticQueue Emit(
        BuildMode buildMode, string moduleName, string[] references, string outputPath, CompilerStage finishStage) {
        foreach (var syntaxTree in syntaxTrees)
            diagnostics.Move(syntaxTree.diagnostics);

        if (diagnostics.Errors().Any())
            return diagnostics;

        var program = GetProgram();
        program.diagnostics.Move(diagnostics);

        if (program.diagnostics.Errors().Any())
            return program.diagnostics;

        if (buildMode == BuildMode.Dotnet)
            return ILEmitter.Emit(program, moduleName, references, outputPath);
        else if (buildMode == BuildMode.CSharpTranspile)
            return CSharpEmitter.Emit(program, outputPath);
        else
            diagnostics.Push(Fatal.Unsupported.IndependentCompilation());

        return diagnostics;
    }

    /// <summary>
    /// Emits the program to a string.
    /// </summary>
    /// <param name="buildMode">Which emitter to use.</param>
    /// <param name="moduleName">
    /// Name of the module. If <param name="buildMode" /> is set to <see cref="BuildMode.CSharpTranspile" /> this is
    /// used as the namespace name instead.
    /// </param>
    /// <param name="references">
    /// .NET references, only applicable if <param name="buildMode" /> is set to <see cref="BuildMode.Dotnet" />.
    /// </param>
    /// <returns>Emitted program as a string. Diagnostics must be accessed manually off of this.</returns>
    internal string EmitToString(BuildMode buildMode, string moduleName, string[] references = null) {
        foreach (var syntaxTree in syntaxTrees)
            diagnostics.Move(syntaxTree.diagnostics);

        if (diagnostics.Errors().Any())
            return null;

        var program = GetProgram();
        program.diagnostics.Move(diagnostics);

        if (program.diagnostics.Errors().Any())
            return null;

        if (buildMode == BuildMode.CSharpTranspile) {
            var content = CSharpEmitter.Emit(program, moduleName, out var emitterDiagnostics);
            diagnostics.Move(emitterDiagnostics);
            return content;
        } else if (buildMode == BuildMode.Dotnet) {
            var content = ILEmitter.Emit(program, moduleName, references, out var emitterDiagnostics);
            diagnostics.Move(emitterDiagnostics);
            return content;
        }

        return null;
    }

    /// <summary>
    /// Gets the previous <see cref="BoundProgram" />, and binds a new one.
    /// </summary>
    /// <returns>Newly bound <see cref="BoundProgram" />.</returns>
    internal BoundProgram GetProgram() {
        var _previous = previous == null ? null : previous.GetProgram();
        return Binder.BindProgram(isScript, _previous, globalScope, _transpilerMode);
    }

    private static void CreateCfg(BoundProgram program) {
        var appPath = Environment.GetCommandLineArgs()[0];
        var appDirectory = Path.GetDirectoryName(appPath);
        var cfgPath = Path.Combine(appDirectory, "cfg.dot");
        var cfgStatement = program.scriptMethod == null && program.mainMethod == null
            ? null
            : program.scriptMethod == null
                ? program.methodBodies[program.mainMethod]
                : program.methodBodies[program.scriptMethod];

        if (cfgStatement != null) {
            var cfg = ControlFlowGraph.Create(cfgStatement);

            using (var streamWriter = new StreamWriter(cfgPath))
                cfg.WriteTo(streamWriter);
        }
    }
}
