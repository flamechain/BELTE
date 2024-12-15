using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="Syntax.CastExpressionSyntax" />.
/// </summary>
internal sealed class BoundCastExpression : BoundExpression {
    internal BoundCastExpression(
        TypeSymbol type,
        BoundExpression operand,
        Conversion conversion,
        ConstantValue constantValue) {
        this.type = type;
        this.operand = operand;
        this.constantValue = constantValue;
        this.conversion = conversion;
    }

    internal override BoundNodeKind kind => BoundNodeKind.CastExpression;

    internal override ConstantValue constantValue { get; }

    internal override TypeSymbol type { get; }

    internal BoundExpression operand { get; }

    internal Conversion conversion { get; }
}
