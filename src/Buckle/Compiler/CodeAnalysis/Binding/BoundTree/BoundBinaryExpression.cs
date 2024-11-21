using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="BinaryExpressionSyntax" />.
/// </summary>
internal sealed class BoundBinaryExpression : BoundExpression {
    internal BoundBinaryExpression(
        BoundExpression left,
        BoundBinaryOperator op,
        BoundExpression right) {
        this.left = left;
        this.op = op;
        this.right = right;
        type = op.type;
        constantValue = ConstantFolding.FoldBinary(this.left, this.op, this.right);
    }

    internal override BoundNodeKind kind => BoundNodeKind.BinaryExpression;

    internal override TypeSymbol type { get; }

    internal override ConstantValue constantValue { get; }

    internal BoundExpression left { get; }

    internal BoundBinaryOperator op { get; }

    internal BoundExpression right { get; }
}
