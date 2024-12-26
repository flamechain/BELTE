using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="BinaryExpressionSyntax" />.
/// </summary>
internal sealed class BoundBinaryExpression : BoundExpression {
    internal BoundBinaryExpression(
        BoundExpression left,
        BoundExpression right,
        BinaryOperatorKind opKind,
        TypeSymbol type,
        ConstantValue constantValue) {
        this.left = left;
        this.right = right;
        this.opKind = opKind;
        this.type = type;
        this.constantValue = constantValue;
    }

    internal override BoundNodeKind kind => BoundNodeKind.BinaryExpression;

    internal override TypeSymbol type { get; }

    internal override ConstantValue constantValue { get; }

    internal BoundExpression left { get; }

    internal BinaryOperatorKind opKind { get; }

    internal BoundExpression right { get; }
}
