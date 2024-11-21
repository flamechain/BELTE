using System.Collections.Immutable;
using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="Syntax.InitializerDictionaryExpressionSyntax" />.
/// Doesn't survive compilation.
/// </summary>
internal sealed class BoundInitializerDictionaryExpression : BoundExpression {
    internal BoundInitializerDictionaryExpression(
        ImmutableArray<(BoundExpression, BoundExpression)> items,
        TypeSymbol type) {
        this.items = items;
        this.type = type;
    }

    internal override BoundNodeKind kind => BoundNodeKind.InitializerDictionaryExpression;

    internal override TypeSymbol type { get; }

    internal override ConstantValue constantValue { get; }

    internal ImmutableArray<(BoundExpression, BoundExpression)> items { get; }
}
