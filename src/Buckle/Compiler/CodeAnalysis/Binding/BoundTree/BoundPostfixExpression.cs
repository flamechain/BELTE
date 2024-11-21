using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="Syntax.PostfixExpressionSyntax" />.
/// Doesn't survive lowering unless the operator was overloaded.
/// </summary>
internal sealed class BoundPostfixExpression : BoundExpression {
    internal BoundPostfixExpression(BoundExpression operand, BoundPostfixOperator op, bool isOwnStatement) {
        this.operand = operand;
        this.op = op;
        this.isOwnStatement = isOwnStatement;
        type = op.type;
    }

    internal override BoundNodeKind kind => BoundNodeKind.PostfixExpression;

    internal override TypeSymbol type { get; }

    internal BoundExpression operand { get; }

    internal BoundPostfixOperator op { get; }

    /// <summary>
    /// If the expression is an expression statement, as if this is the case the <see cref="Lowering.Lowerer" /> has to
    /// do less.
    /// </summary>
    internal bool isOwnStatement { get; }
}
