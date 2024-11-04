using System.Collections.Immutable;
using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

internal abstract class BoundEqualsValue : BoundInitializer {
    private protected BoundEqualsValue(ImmutableArray<DataContainerSymbol> locals, BoundExpression value) {
        this.locals = locals;
        this.value = value;
    }

    internal ImmutableArray<DataContainerSymbol> locals { get; }

    internal BoundExpression value { get; }
}
