using System.Collections.Immutable;
using Buckle.CodeAnalysis.Binding;
using Buckle.CodeAnalysis.Syntax;
using Buckle.CodeAnalysis.Text;
using Microsoft.CodeAnalysis.PooledObjects;

namespace Buckle.CodeAnalysis.Symbols;

internal sealed class SynthesizedEntryPoint : MethodSymbol {
    private readonly BlockStatementSyntax _synthesizedBody;
    private readonly MethodDeclarationSyntax _synthesizedDeclaration;

    private TypeWithAnnotations _returnType;

    internal SynthesizedEntryPoint(
        Symbol containingSymbol,
        TypeWithAnnotations returnType,
        ImmutableArray<GlobalStatementSyntax> syntax) {
        this.containingSymbol = containingSymbol;
        _returnType = returnType;

        var bodyBuilder = ArrayBuilder<StatementSyntax>.GetInstance();

        foreach (var statement in syntax)
            bodyBuilder.Add(statement.statement);

        _synthesizedBody = SyntaxFactory.BlockWithParent(bodyBuilder.ToArrayAndFree());
        _synthesizedDeclaration = SyntaxFactory.MethodWithParent(_synthesizedBody);
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

    internal override SyntaxReference syntaxReference => null;

    internal override TextLocation location => null;

    internal MethodDeclarationSyntax body => _synthesizedDeclaration;

    internal ExecutableCodeBinder TryGetBodyBinder() {
        var inMethodBinder = declaringCompilation.GetBinder(_synthesizedBody);
        return inMethodBinder is null ? null : new ExecutableCodeBinder(_synthesizedDeclaration, this, inMethodBinder);
    }

    internal void CorrectReturnType(TypeWithAnnotations type) {
        _returnType = type;
    }
}
