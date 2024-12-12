using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="UnaryExpressionSyntax" />.
/// </summary>
internal sealed class BoundUnaryExpression : BoundExpression {
    internal BoundUnaryExpression(BoundExpression operand, UnaryOperatorKind opKind, TypeSymbol type) {
        this.operand = operand;
        this.opKind = opKind;
        this.type = type;
        constantValue = ConstantFolding.FoldUnary(operand, opKind, type);
    }

    internal override BoundNodeKind kind => BoundNodeKind.UnaryExpression;

    internal override TypeSymbol type { get; }

    internal override ConstantValue constantValue { get; }

    internal UnaryOperatorKind opKind { get; }

    internal BoundExpression operand { get; }
}
