using System.Collections.Immutable;
using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="Syntax.InitializerListExpressionSyntax" />.
/// </summary>
internal sealed class BoundInitializerListExpression : BoundExpression {
    internal BoundInitializerListExpression(ImmutableArray<BoundExpression> items, TypeSymbol type) {
        this.items = items;
        this.type = type;
        constantValue = ConstantFolding.FoldInitializerList(this.items);
    }

    internal BoundInitializerListExpression(ConstantValue constantValue, TypeSymbol type) {
        items = ImmutableArray<BoundExpression>.Empty;
        this.type = type;
        this.constantValue = constantValue;
    }

    internal override BoundNodeKind kind => BoundNodeKind.InitializerListExpression;

    internal override TypeSymbol type { get; }

    internal override ConstantValue constantValue { get; }

    internal ImmutableArray<BoundExpression> items { get; }
}
