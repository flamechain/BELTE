using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// A bound conditional access expression, bound from a <see cref="Syntax.MemberAccessExpressionSyntax" />.
/// </summary>
internal sealed class BoundConditionalAccessExpression : BoundExpression {
    internal BoundConditionalAccessExpression(
        TypeSymbol type,
        BoundExpression receiver,
        BoundExpression accessExpression) {
        this.type = type;
        this.receiver = receiver;
        this.accessExpression = accessExpression;
    }

    internal override BoundNodeKind kind => BoundNodeKind.ConditionalAccessExpression;

    internal override TypeSymbol type { get; }

    internal override ConstantValue constantValue => accessExpression.constantValue;

    internal BoundExpression receiver { get; }

    internal BoundExpression accessExpression { get; }
}
