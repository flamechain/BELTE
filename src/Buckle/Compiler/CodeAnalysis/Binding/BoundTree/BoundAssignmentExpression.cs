using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="Syntax.AssignmentExpressionSyntax" />.
/// </summary>
internal sealed class BoundAssignmentExpression : BoundExpression {
    internal BoundAssignmentExpression(BoundExpression left, BoundExpression right, bool isRef, TypeSymbol type) {
        this.left = left;
        this.right = right;
        this.type = type;
        this.isRef = isRef;
    }

    internal override BoundNodeKind kind => BoundNodeKind.AssignmentExpression;

    internal override TypeSymbol type { get; }

    internal BoundExpression left { get; }

    internal BoundExpression right { get; }

    internal bool isRef { get; }
}
