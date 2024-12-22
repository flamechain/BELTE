using System;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using Buckle.CodeAnalysis.Binding;
using Buckle.CodeAnalysis.Syntax;
using Buckle.CodeAnalysis.Text;

namespace Buckle.CodeAnalysis.Symbols;

internal sealed class SynthesizedEntryPoint : SynthesizedInstanceMethodSymbol {
    private readonly BlockStatementSyntax _synthesizedBody;
    private readonly MethodDeclarationSyntax _synthesizedDeclaration;

    private WeakReference<ExecutableCodeBinder> _weakBodyBinder;
    private TypeWithAnnotations _returnType;

    internal SynthesizedEntryPoint(
        Symbol containingSymbol,
        TypeWithAnnotations returnType,
        ImmutableArray<GlobalStatementSyntax> syntax,
        CompilationUnitSyntax syntaxNode) {
        this.containingSymbol = containingSymbol;
        _returnType = returnType;

        statements = syntax;

        // var bodyBuilder = ArrayBuilder<StatementSyntax>.GetInstance();

        // foreach (var statement in syntax)
        //     bodyBuilder.Add(statement.statement);

        // _synthesizedBody = SyntaxFactory.BlockWithParent(bodyBuilder.ToArrayAndFree());
        // _synthesizedDeclaration = SyntaxFactory.MethodWithParent(_synthesizedBody);
        syntaxReference = new SyntaxReference(syntaxNode);
        this.syntaxNode = syntaxNode;
    }

    public override string name => WellKnownMemberNames.EntryPointMethodName;

    public override ImmutableArray<TemplateParameterSymbol> templateParameters => [];

    public override ImmutableArray<BoundExpression> templateConstraints => [];

    public override ImmutableArray<TypeOrConstant> templateArguments => [];

    public override RefKind refKind => RefKind.None;

    public override bool returnsVoid => returnTypeWithAnnotations.IsVoidType();

    public override MethodKind methodKind => MethodKind.Ordinary;

    public override int arity => 0;

    internal override Symbol containingSymbol { get; }

    internal override TypeWithAnnotations returnTypeWithAnnotations => _returnType;

    internal override ImmutableArray<ParameterSymbol> parameters => [];

    internal override int parameterCount => 0;

    internal override bool hidesBaseMethodsByName => false;

    internal override bool hasSpecialName => false;

    internal override bool isDeclaredConst => false;

    internal override Accessibility declaredAccessibility => Accessibility.Public;

    internal override bool isStatic => false;

    internal override bool isVirtual => false;

    internal override bool isAbstract => false;

    internal override bool isOverride => false;

    internal override bool isSealed => false;

    internal override bool requiresInstanceReceiver => false;

    internal override SyntaxReference syntaxReference { get; }

    internal override TextLocation location => null;

    // internal MethodDeclarationSyntax body => _synthesizedDeclaration;

    internal SyntaxTree syntaxTree => syntaxNode.syntaxTree;

    internal SyntaxNode syntaxNode { get; }

    internal ImmutableArray<GlobalStatementSyntax> statements { get; }

    internal ExecutableCodeBinder TryGetBodyBinder() {
        // var inMethodBinder = declaringCompilation.GetBinder(_synthesizedBody);
        // return inMethodBinder is null ? null : new ExecutableCodeBinder(_synthesizedDeclaration, this, inMethodBinder);
        return GetBodyBinder();
    }

    internal ExecutableCodeBinder GetBodyBinder() {
        ref var weakBinder = ref _weakBodyBinder;

        while (true) {
            var previousWeakReference = weakBinder;

            if (previousWeakReference is not null && previousWeakReference.TryGetTarget(out var previousBinder))
                return previousBinder;

            var newBinder = CreateBodyBinder();

            if (Interlocked.CompareExchange(ref weakBinder, new WeakReference<ExecutableCodeBinder>(newBinder), previousWeakReference) == previousWeakReference) {
                return newBinder;
            }
        }
    }

    private ExecutableCodeBinder CreateBodyBinder() {
        var compilation = declaringCompilation;
        var syntaxNode = this.syntaxNode;
        Binder result = new EndBinder(compilation, syntaxTree.text);
        var globalNamespace = compilation.globalNamespaceInternal;
        result = new InContainerBinder(globalNamespace, result);
        result = new InContainerBinder(containingType, result);
        result = new InMethodBinder(this, result);
        return new ExecutableCodeBinder(syntaxNode, this, result);
    }

    internal void CorrectReturnType(TypeWithAnnotations type) {
        _returnType = type;
    }

    internal static SynthesizedEntryPoint GetEntryPoint(
        Compilation compilation,
        CompilationUnitSyntax compilationUnit) {
        var type = GetSimpleProgramNamedTypeSymbol(compilation);

        if (type is null)
            return null;

        var entryPoints = type.GetMembers(WellKnownMemberNames.EntryPointMethodName).OfType<SynthesizedEntryPoint>();

        foreach (var entryPoint in entryPoints) {
            if (entryPoint.syntaxTree == compilationUnit.syntaxTree && entryPoint.syntaxNode == compilationUnit)
                return entryPoint;
        }

        return null;
    }

    private static NamedTypeSymbol GetSimpleProgramNamedTypeSymbol(Compilation compilation) {
        return compilation.globalNamespaceInternal
            .GetTypeMembers(WellKnownMemberNames.TopLevelStatementsEntryPointTypeName)
            .SingleOrDefault(s => s.isSimpleProgram);
    }
}
