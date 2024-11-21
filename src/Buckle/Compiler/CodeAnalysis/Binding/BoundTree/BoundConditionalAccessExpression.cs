using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="Syntax.MemberAccessExpressionSyntax" />.
/// Doesn't survive lowering.
/// </summary>
internal sealed class BoundConditionalAccessExpression : BoundExpression {
    internal BoundConditionalAccessExpression(
        TypeSymbol type,
        BoundExpression receiver,
        BoundExpression accessExpression) {
        this.type = type;
        this.receiver = receiver;
        this.accessExpression = accessExpression;
        constantValue = accessExpression.constantValue;
    }

    internal override BoundNodeKind kind => BoundNodeKind.ConditionalAccessExpression;

    internal override TypeSymbol type { get; }

    internal override ConstantValue constantValue { get; }

    internal BoundExpression receiver { get; }

    internal BoundExpression accessExpression { get; }
}
