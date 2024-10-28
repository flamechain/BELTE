using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// A bound cast expression, bound from a <see cref="Syntax.CastExpressionSyntax" />.
/// </summary>
internal sealed class BoundCastExpression : BoundExpression {
    internal BoundCastExpression(
        TypeSymbol type,
        BoundExpression operand,
        ConversionKind conversionKind,
        ConstantValue constantValue) {
        this.type = type;
        this.operand = operand;
        this.constantValue = constantValue;
        this.conversionKind = conversionKind;
    }

    internal BoundExpression operand { get; }

    internal override BoundNodeKind kind => BoundNodeKind.CastExpression;

    internal override ConstantValue constantValue { get; }

    internal override TypeSymbol type { get; }

    internal ConversionKind conversionKind { get; }
}
