using System.Collections.Immutable;
using Buckle.CodeAnalysis.Binding;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Symbols;

internal sealed class SynthesizedGlobalMethodSymbol : MethodSymbol {
    internal SynthesizedGlobalMethodSymbol(
        string name,
        ImmutableArray<ParameterSymbol> parameters,
        TypeWithAnnotations type) {
        this.name = name;
        this.parameters = parameters;
        returnTypeWithAnnotations = type;
    }

    public override string name { get; }

    public override ImmutableArray<TemplateParameterSymbol> templateParameters => [];

    public override ImmutableArray<TypeOrConstant> templateArguments => [];

    public override ImmutableArray<BoundExpression> templateConstraints => [];

    internal override TypeWithAnnotations returnTypeWithAnnotations { get; }

    internal override ImmutableArray<ParameterSymbol> parameters { get; }

    internal override Symbol containingSymbol => null;

    internal override bool hidesBaseMethodsByName => false;

    internal override bool isDeclaredConst => false;

    internal override SyntaxReference syntaxReference => null;

    internal override int arity => 0;

    internal override bool isAbstract => false;

    internal override bool isVirtual => false;

    internal override bool isOverride => false;

    internal override bool isStatic => false;

    internal override bool isSealed => false;

    internal override RefKind refKind => RefKind.None;

    internal override bool returnsVoid => returnTypeWithAnnotations.IsVoidType();

    internal override MethodKind methodKind => MethodKind.Builtin;

    internal override bool hasSpecialName => false;

    internal override Accessibility declaredAccessibility => Accessibility.Public;
}
