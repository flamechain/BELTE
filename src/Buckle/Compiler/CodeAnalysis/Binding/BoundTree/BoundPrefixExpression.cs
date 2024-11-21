using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="Syntax.PrefixExpressionSyntax" />.
/// Doesn't survive lowering unless the operator was overloaded.
/// </summary>
internal sealed class BoundPrefixExpression : BoundExpression {
    internal BoundPrefixExpression(BoundPrefixOperator op, BoundExpression operand) {
        this.op = op;
        this.operand = operand;
        type = op.type;
    }

    internal override BoundNodeKind kind => BoundNodeKind.PrefixExpression;

    internal override TypeSymbol type { get; }

    internal BoundPrefixOperator op { get; }

    internal BoundExpression operand { get; }
}
