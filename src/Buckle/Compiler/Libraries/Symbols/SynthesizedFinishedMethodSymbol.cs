using System.Collections.Immutable;
using Buckle.CodeAnalysis.Binding;
using Buckle.CodeAnalysis.Symbols;

namespace Buckle.Libraries;

internal sealed class SynthesizedFinishedMethodSymbol : WrappedMethodSymbol {
    internal SynthesizedFinishedMethodSymbol(
        MethodSymbol underlyingMethod,
        Symbol containingSymbol,
        ImmutableArray<ParameterSymbol>? parameters = null)
        : base(underlyingMethod) {
        this.containingSymbol = containingSymbol;
        this.parameters = parameters ?? underlyingMethod.parameters;
    }

    public override ImmutableArray<TemplateParameterSymbol> templateParameters => [];

    public override ImmutableArray<BoundExpression> templateConstraints => [];

    public override ImmutableArray<TypeOrConstant> templateArguments => [];

    internal override TypeWithAnnotations returnTypeWithAnnotations => underlyingMethod.returnTypeWithAnnotations;

    internal override ImmutableArray<ParameterSymbol> parameters { get; }

    internal override Symbol containingSymbol { get; }
}
