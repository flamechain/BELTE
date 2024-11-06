using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// A bound assignment expression, bound from a <see cref="Syntax.AssignmentExpressionSyntax" />.
/// </summary>
internal sealed class BoundAssignmentExpression : BoundExpression {
    internal BoundAssignmentExpression(BoundExpression left, BoundExpression right, TypeSymbol type) {
        this.left = left;
        this.right = right;
        this.type = type;
    }

    internal BoundExpression left { get; }

    internal BoundExpression right { get; }

    internal override BoundNodeKind kind => BoundNodeKind.AssignmentExpression;

    internal override TypeSymbol type { get; }
}
