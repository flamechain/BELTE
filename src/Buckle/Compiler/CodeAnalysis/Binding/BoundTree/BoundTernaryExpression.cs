using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="TernaryExpressionSyntax" />.
/// </summary>
internal sealed class BoundTernaryExpression : BoundExpression {
    internal BoundTernaryExpression(
        BoundExpression left,
        BoundTernaryOperator op,
        BoundExpression center,
        BoundExpression right) {
        this.left = left;
        this.op = op;
        this.center = center;
        this.right = right;
        type = op.type;
        constantValue = ConstantFolding.FoldTernary(this.left, this.op, this.center, this.right);
    }

    internal override BoundNodeKind kind => BoundNodeKind.TernaryExpression;

    internal override TypeSymbol type { get; }

    internal override ConstantValue constantValue { get; }

    internal BoundExpression left { get; }

    internal BoundTernaryOperator op { get; }

    internal BoundExpression center { get; }

    internal BoundExpression right { get; }
}
