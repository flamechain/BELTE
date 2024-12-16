using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="BinaryExpressionSyntax" />.
/// </summary>
internal sealed class BoundNullCoalescingExpression : BoundExpression {
    internal BoundNullCoalescingExpression(BoundExpression left, BoundExpression right, TypeSymbol type) {
        this.left = left;
        this.right = right;
        this.type = type;
        constantValue = ConstantFolding.FoldNullCoalescing(left, right, type);
    }

    internal override BoundNodeKind kind => BoundNodeKind.NullCoalescingExpression;

    internal override TypeSymbol type { get; }

    internal override ConstantValue constantValue { get; }

    internal BoundExpression left { get; }

    internal BoundExpression right { get; }
}
