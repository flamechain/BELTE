using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a parser <see cref="Syntax.AssignmentExpressionSyntax" />.
/// Doesn't survive lowering.
/// </summary>
internal sealed class BoundCompoundAssignmentExpression : BoundExpression {
    internal BoundCompoundAssignmentExpression(
        BoundExpression left,
        BoundBinaryOperator op,
        BoundExpression right) {
        this.left = left;
        this.op = op;
        this.right = right;
        type = right.type;
    }

    internal override BoundNodeKind kind => BoundNodeKind.CompoundAssignmentExpression;

    internal override TypeSymbol type { get; }

    internal BoundExpression left { get; }

    internal BoundBinaryOperator op { get; }

    internal BoundExpression right { get; }
}
