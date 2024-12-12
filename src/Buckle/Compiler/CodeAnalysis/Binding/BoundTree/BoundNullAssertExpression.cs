using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="UnaryExpressionSyntax" />.
/// </summary>
internal sealed class BoundNullAssertExpression : BoundExpression {
    internal BoundNullAssertExpression(BoundExpression operand, TypeSymbol type) {
        this.operand = operand;
        this.type = type;
        constantValue = ConstantFolding.FoldNullAssert(operand);
    }

    internal override BoundNodeKind kind => BoundNodeKind.NullAssertExpression;

    internal override TypeSymbol type { get; }

    internal override ConstantValue constantValue { get; }

    internal BoundExpression operand { get; }
}
