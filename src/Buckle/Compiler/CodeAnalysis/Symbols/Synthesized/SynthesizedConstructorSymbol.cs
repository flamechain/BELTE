using System.Collections.Immutable;
using Buckle.CodeAnalysis.Binding;
using Buckle.CodeAnalysis.Syntax;
using Buckle.Libraries;

namespace Buckle.CodeAnalysis.Symbols;

internal sealed class SynthesizedConstructorSymbol : MethodSymbol {
    internal SynthesizedConstructorSymbol(NamedTypeSymbol containingType) {
        this.containingType = containingType;
    }

    public override string name => WellKnownMemberNames.InstanceConstructorName;

    public override ImmutableArray<TemplateParameterSymbol> templateParameters => [];

    public override ImmutableArray<BoundExpression> templateConstraints => [];

    public override ImmutableArray<TypeOrConstant> templateArguments => [];

    public override RefKind refKind => RefKind.None;

    public override bool returnsVoid => true;

    public override MethodKind methodKind => MethodKind.Constructor;

    public override int arity => 0;

    internal override TypeWithAnnotations returnTypeWithAnnotations
        => new TypeWithAnnotations(CorLibrary.GetSpecialType(SpecialType.Void));

    internal override ImmutableArray<ParameterSymbol> parameters => [];

    internal override int parameterCount => 0;

    internal override bool hidesBaseMethodsByName => false;

    internal override bool hasSpecialName => true;

    internal override bool isDeclaredConst => false;

    internal override Accessibility declaredAccessibility
        => containingType.isAbstract ? Accessibility.Protected : Accessibility.Public;

    internal override Symbol containingSymbol => containingType;

    internal override NamedTypeSymbol containingType { get; }

    internal override bool isStatic => false;

    internal override bool isVirtual => false;

    internal override bool isAbstract => false;

    internal override bool isOverride => false;

    internal override bool isSealed => false;

    internal override SyntaxReference syntaxReference => null;

    internal override bool isImplicitlyDeclared => true;

    internal override LexicalSortKey GetLexicalSortKey() {
        return LexicalSortKey.SynthesizedCtor;
    }
}
