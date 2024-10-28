using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.IO;
using System.Threading;
using Buckle.CodeAnalysis.Binding;
using Buckle.CodeAnalysis.Emitting;
using Buckle.CodeAnalysis.Evaluating;
using Buckle.CodeAnalysis.FlowAnalysis;
using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;
using Buckle.CodeAnalysis.Text;
using Buckle.Diagnostics;
using Buckle.Libraries;
using Diagnostics;
using Microsoft.CodeAnalysis.PooledObjects;
using Shared;

namespace Buckle.CodeAnalysis;

/// <summary>
/// Handles evaluation of program, and keeps track of Symbols.
/// </summary>
public sealed class Compilation {
    private readonly SyntaxManager _syntax;
    private readonly ImmutableDictionary<SyntaxTree, int> _ordinalMap;
    private NamespaceSymbol _lazyGlobalNamespace;
    private WeakReference<BinderFactory>[] _binderFactories;
    private BelteDiagnosticQueue _lazyDeclarationDiagnostics;

    private Compilation(
        string assemblyName,
        CompilationOptions options,
        Compilation previous,
        SyntaxManager syntax) {
        this.assemblyName = assemblyName;
        this.options = options;
        this.previous = previous;
        _syntax = syntax;
    }

    public string assemblyName { get; }

    public INamespaceSymbol globalNamespace => globalNamespaceInternal;

    public CompilationOptions options { get; }

    public Compilation previous { get; }

    internal BelteDiagnosticQueue declarationDiagnostics {
        get {
            if (_lazyDeclarationDiagnostics is null)
                Interlocked.CompareExchange(ref _lazyDeclarationDiagnostics, new BelteDiagnosticQueue(), null);

            return _lazyDeclarationDiagnostics;
        }
    }

    internal ImmutableArray<SyntaxTree> syntaxTrees => _syntax.state.syntaxTrees;

    internal bool keepLookingForCorTypes => CorLibrary.StillLookingForSpecialTypes();

    internal NamespaceSymbol globalNamespaceInternal {
        get {
            if (_lazyGlobalNamespace is null) {
                var extent = new NamespaceExtent(this);
                var mergedDeclarations = MergeDeclarations();
                var result = new GlobalNamespaceSymbol(extent, mergedDeclarations);
                Interlocked.CompareExchange(ref _lazyGlobalNamespace, result, null);
            }

            return _lazyGlobalNamespace;
        }
    }

    public static Compilation Create(string assemblyName, CompilationOptions options, params SyntaxTree[] syntaxTrees) {
        return Create(assemblyName, options, null, syntaxTrees);
    }

    public static Compilation Create(
        string assemblyName,
        CompilationOptions options,
        Compilation previous,
        params SyntaxTree[] syntaxTrees) {
        return Create(assemblyName, options, previous, (IEnumerable<SyntaxTree>)syntaxTrees);
    }

    public static Compilation CreateScript(
        string assemblyName,
        CompilationOptions options,
        SyntaxTree syntaxTree = null,
        Compilation previous = null) {
        options.isScript = true;
        var syntaxTress = syntaxTree is null ? null : (IEnumerable<SyntaxTree>)[syntaxTree];
        return Create(assemblyName, options, previous, syntaxTress);
    }

    public EvaluationResult Evaluate(ValueWrapper<bool> abort, bool logTime = false) {
        return Evaluate([], abort, logTime);
    }

    public EvaluationResult Evaluate(
        Dictionary<IDataContainerSymbol, EvaluatorObject> globals,
        ValueWrapper<bool> abort,
        bool logTime = false) {
        var timer = logTime ? Stopwatch.StartNew() : null;
        var builder = GetDiagnostics();

#if DEBUG
        if (options.enableOutput) {
            CreateCfg(program);
            CreateBoundProgram(program);
        }
#endif

        if (logTime) {
            timer.Stop();
            builder.Push(new BelteDiagnostic(
                DiagnosticSeverity.Debug,
                $"Bound the program in {timer.ElapsedMilliseconds} ms"
            ));
            timer.Restart();
        }

        if (builder.AnyErrors())
            return EvaluationResult.Failed(builder);

        var eval = new Evaluator(program, globals, options.arguments);
        var evalResult = eval.Evaluate(abort, out var hasValue);

        if (logTime) {
            timer.Stop();
            builder.Push(new BelteDiagnostic(
                DiagnosticSeverity.Debug,
                $"Evaluated the program in {timer.ElapsedMilliseconds} ms"
            ));
        }

        var result = new EvaluationResult(
            evalResult,
            hasValue,
            builder,
            eval.exceptions,
            eval.lastOutputWasPrint,
            eval.containsIO
        );

        return result;
    }

    public BelteDiagnosticQueue Emit(string outputPath, bool logTime = false) {
        var timer = logTime ? Stopwatch.StartNew() : null;
        var builder = GetDiagnostics();

        if (logTime) {
            timer.Stop();
            builder.Push(new BelteDiagnostic(
                DiagnosticSeverity.Debug,
                $"Compiled in {timer.ElapsedMilliseconds} ms"
            ));
            timer.Restart();
        }

        if (builder.AnyErrors())
            return builder;

        if (options.buildMode == BuildMode.Dotnet)
            // return ILEmitter.Emit(program, moduleName, references, outputPath);
            builder.Push(Fatal.Unsupported.DotnetCompilation());
        else if (options.buildMode == BuildMode.CSharpTranspile)
            return CSharpEmitter.Emit(program, outputPath);
        else if (options.buildMode == BuildMode.Independent)
            builder.Push(Fatal.Unsupported.IndependentCompilation());

        if (logTime && options.buildMode is BuildMode.Dotnet or BuildMode.CSharpTranspile) {
            timer.Stop();
            builder.Push(new BelteDiagnostic(
                DiagnosticSeverity.Debug,
                $"Emitted the program in {timer.ElapsedMilliseconds} ms"
            ));
        }

        return builder;
    }

    public BelteDiagnosticQueue Execute() {
        var builder = GetDiagnostics();

        if (builder.AnyErrors())
            return builder;

#if DEBUG
        if (options.enableOutput) {
            CreateCfg(program);
            CreateBoundProgram(program);
        }
#endif

        Executor.Execute(program, options.arguments);
        return builder;
    }

    public EvaluationResult Interpret(ValueWrapper<bool> abort) {
        return Interpreter.Interpret(_syntax.syntaxTrees[0], options, abort);
    }

    public string EmitToString(out BelteDiagnosticQueue diagnostics, BuildMode? alternateBuildMode = null) {
        var buildMode = alternateBuildMode ?? options.buildMode;
        diagnostics = GetDiagnostics();

        if (diagnostics.AnyErrors())
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

    public BelteDiagnosticQueue GetParseDiagnostics() {
        return GetDiagnostics(true, false, false);
    }

    public BelteDiagnosticQueue GetDeclarationDiagnostics() {
        return GetDiagnostics(false, true, false);
    }

    public BelteDiagnosticQueue GetMethodBodyDiagnostics() {
        return GetDiagnostics(false, false, true);
    }

    public BelteDiagnosticQueue GetDiagnostics() {
        return GetDiagnostics(true, true, true);
    }

    internal int CompareSourceLocations(SyntaxReference syntax1, SyntaxReference syntax2) {
        var comparison = CompareSyntaxTreeOrdering(syntax1.syntaxTree, syntax2.syntaxTree);

        if (comparison != 0)
            return comparison;

        return syntax1.span.start - syntax2.span.start;
    }

    internal int CompareSourceLocations(
        SyntaxReference syntax1,
        TextLocation location1,
        SyntaxReference syntax2,
        TextLocation location2) {
        var comparison = CompareSyntaxTreeOrdering(syntax1.syntaxTree, syntax2.syntaxTree);

        if (comparison != 0)
            return comparison;

        return location1.span.start - location2.span.start;
    }

    internal int CompareSyntaxTreeOrdering(SyntaxTree tree1, SyntaxTree tree2) {
        if (tree1 == tree2)
            return 0;

        return GetSyntaxTreeOrdinal(tree1) - GetSyntaxTreeOrdinal(tree2);
    }

    internal void RegisterDeclaredSpecialType(NamedTypeSymbol type) {
        // TODO Maybe make the CorLibrary not static?
        CorLibrary.RegisterDeclaredSpecialType(type);
    }

    internal Binder GetBinder(BelteSyntaxNode syntax) {
        return GetBinderFactory(syntax.syntaxTree).GetBinder(syntax);
    }

    internal BinderFactory GetBinderFactory(SyntaxTree syntaxTree) {
        var treeOrdinal = _ordinalMap[syntaxTree];
        var binderFactories = _binderFactories;

        if (binderFactories is null) {
            binderFactories = new WeakReference<BinderFactory>[syntaxTrees.Length];
            binderFactories = Interlocked.CompareExchange(ref _binderFactories, binderFactories, null)
                ?? binderFactories;
        }

        var previousWeakReference = binderFactories[treeOrdinal];

        if (previousWeakReference is not null && previousWeakReference.TryGetTarget(out var previousFactory))
            return previousFactory;

        return AddNewFactory(syntaxTree, ref binderFactories[treeOrdinal]);
    }

    internal int GetSyntaxTreeOrdinal(SyntaxTree syntaxTree) {
        return _syntax.state.ordinalMap[syntaxTree];
    }

    private Compilation AddSyntaxTrees(IEnumerable<SyntaxTree> trees) {
        ArgumentNullException.ThrowIfNull(trees);

        if (trees.IsEmpty())
            return this;

        var externalSyntaxTrees = PooledHashSet<SyntaxTree>.GetInstance();
        var syntax = _syntax;
        externalSyntaxTrees.AddAll(syntax.syntaxTrees);

        var i = 0;

        foreach (var tree in trees) {
            if (tree is null)
                throw new ArgumentNullException($"{nameof(trees)}[{i}]");

            if (externalSyntaxTrees.Contains(tree))
                throw new ArgumentException("Syntax tree already present", $"{nameof(trees)}[{i}]");

            externalSyntaxTrees.Add(tree);
            i++;
        }

        externalSyntaxTrees.Free();

        if (options.isScript && i > 1)
            throw new ArgumentException("Script can have at most 1 syntax tree", nameof(trees));

        syntax = syntax.AddSyntaxTrees(trees);
        return Update(syntax);
    }

    private Compilation Update(SyntaxManager syntax) {
        return new Compilation(assemblyName, options, previous, syntax);
    }

    private BinderFactory AddNewFactory(SyntaxTree syntaxTree, ref WeakReference<BinderFactory> slot) {
        var newFactory = new BinderFactory(this, syntaxTree);
        var newWeakReference = new WeakReference<BinderFactory>(newFactory);

        while (true) {
            var previousWeakReference = slot;

            if (previousWeakReference is not null && previousWeakReference.TryGetTarget(out var previousFactory))
                return previousFactory;

            if (Interlocked.CompareExchange(ref slot!, newWeakReference, previousWeakReference)
                == previousWeakReference) {
                return newFactory;
            }
        }
    }

    private static Compilation Create(
        string assemblyName,
        CompilationOptions options,
        Compilation previous,
        IEnumerable<SyntaxTree> syntaxTrees) {
        var compilation = new Compilation(
            assemblyName,
            options,
            previous,
            new SyntaxManager([], null)
        );

        if (syntaxTrees is not null)
            compilation = compilation.AddSyntaxTrees(syntaxTrees);

        return compilation;
    }

    private BelteDiagnosticQueue GetDiagnostics(bool includeParse, bool includeDeclaration, bool includeMethods) {
        var builder = new BelteDiagnosticQueue();

        if (includeParse) {
            foreach (var syntaxTree in _syntax.syntaxTrees)
                builder.PushRange(syntaxTree.GetDiagnostics());
        }

        if (includeDeclaration) {
            globalNamespaceInternal.ForceComplete(null);
            builder.PushRange(declarationDiagnostics);
        }

        if (includeMethods) {
            // TODO
        }

        return builder;
    }

    private ImmutableArray<MemberDeclarationSyntax> MergeDeclarations() {
        var builder = ArrayBuilder<MemberDeclarationSyntax>.GetInstance();

        foreach (var tree in _syntax.syntaxTrees) {
            var compilationUnit = tree.GetCompilationUnitRoot();
            builder.AddRange(compilationUnit.members);
        }

        return builder.ToImmutableAndFree();
    }

    private static void CreateCfg(BoundProgram program) {
        var cfgPath = GetProjectPath("cfg.dot");
        var cfgStatement = program.entryPoint is null ? null : program.methodBodies[program.entryPoint];

        if (cfgStatement is not null) {
            var cfg = ControlFlowGraph.Create(cfgStatement);

            using var streamWriter = new StreamWriter(cfgPath);
            cfg.WriteTo(streamWriter);
        }
    }

    private static string GetProjectPath(string fileName) {
        var appPath = Environment.GetCommandLineArgs()[0];
        var appDirectory = Path.GetDirectoryName(appPath);
        return Path.Combine(appDirectory, fileName);
    }
}
