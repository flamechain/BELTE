using System.Collections.Immutable;
using Buckle.CodeAnalysis.Binding;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Symbols;

internal sealed class ErrorMethodSymbol : MethodSymbol {
    internal static readonly ErrorMethodSymbol UnknownMethod
        = new ErrorMethodSymbol(ErrorTypeSymbol.UnknownResultType, ErrorTypeSymbol.UnknownResultType, "");

    private readonly TypeSymbol _containingType;

    internal ErrorMethodSymbol(TypeSymbol containingType, TypeSymbol returnType, string name) {
        _containingType = containingType;
        returnTypeWithAnnotations = new TypeWithAnnotations(returnType);
        this.name = name;
    }

    public override string name { get; }

    public override ImmutableArray<TemplateParameterSymbol> templateParameters => [];

    public override ImmutableArray<TypeOrConstant> templateArguments => [];

    public override ImmutableArray<BoundExpression> templateConstraints => [];

    internal override bool hasSpecialName => false;

    internal override bool isSealed => false;

    internal override bool isAbstract => false;

    internal override bool isOverride => false;

    internal override bool isVirtual => false;

    internal override bool isStatic => false;

    internal override Accessibility declaredAccessibility => Accessibility.Public;

    internal override SyntaxReference syntaxReference => null;

    internal override Symbol containingSymbol => _containingType;

    internal override bool isDeclaredConst => false;

    internal override int parameterCount => 0;

    internal override ImmutableArray<ParameterSymbol> parameters => [];

    internal override RefKind refKind => RefKind.None;

    internal override TypeWithAnnotations returnTypeWithAnnotations { get; }

    internal override bool returnsVoid => returnType.IsVoidType();

    internal override bool hidesBaseMethodsByName => false;

    internal override int arity => 0;

    internal override MethodKind methodKind => name switch {
        WellKnownMemberNames.InstanceConstructorName => MethodKind.Constructor,
        _ => MethodKind.Ordinary,
    };
}
