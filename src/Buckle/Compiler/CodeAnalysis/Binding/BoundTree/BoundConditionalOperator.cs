using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="TernaryExpressionSyntax" />.
/// </summary>
internal sealed class BoundConditionalExpression : BoundExpression {
    internal BoundConditionalExpression(
        BoundExpression left,
        BoundExpression center,
        BoundExpression right,
        TypeSymbol type) {
        this.left = left;
        this.center = center;
        this.right = right;
        this.type = type;
        constantValue = ConstantFolding.FoldConditional(left, center, right);
    }

    internal override BoundNodeKind kind => BoundNodeKind.ConditionalExpression;

    internal override TypeSymbol type { get; }

    internal override ConstantValue constantValue { get; }

    internal BoundExpression left { get; }

    internal BoundExpression center { get; }

    internal BoundExpression right { get; }
}
