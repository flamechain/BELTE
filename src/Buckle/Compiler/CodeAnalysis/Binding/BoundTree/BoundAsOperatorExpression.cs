using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="BinaryExpressionSyntax" />.
/// </summary>
internal sealed class BoundAsExpression : BoundExpression {
    internal BoundAsExpression(BoundExpression left, BoundExpression right, TypeSymbol type) {
        this.left = left;
        this.right = right;
        this.type = type;
    }

    internal override BoundNodeKind kind => BoundNodeKind.AsExpression;

    internal override TypeSymbol type { get; }

    internal BoundExpression left { get; }

    internal BoundExpression right { get; }
}
