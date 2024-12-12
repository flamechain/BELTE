using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a parser <see cref="Syntax.AssignmentExpressionSyntax" />.
/// Doesn't survive lowering.
/// </summary>
internal sealed class BoundCompoundAssignmentExpression : BoundExpression {
    internal BoundCompoundAssignmentExpression(
        BoundExpression left,
        BoundExpression right,
        BinaryOperatorKind opKind,
        TypeSymbol type) {
        this.left = left;
        this.right = right;
        this.opKind = opKind;
        this.type = type;
    }

    internal override BoundNodeKind kind => BoundNodeKind.CompoundAssignmentExpression;

    internal override TypeSymbol type { get; }

    internal BoundExpression left { get; }

    internal BinaryOperatorKind opKind { get; }

    internal BoundExpression right { get; }
}
