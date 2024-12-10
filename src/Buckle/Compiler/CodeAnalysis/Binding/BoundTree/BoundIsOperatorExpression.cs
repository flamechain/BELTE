using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="BinaryExpressionSyntax" />.
/// </summary>
internal sealed class BoundIsExpression : BoundExpression {
    internal BoundIsExpression(BoundExpression left, BoundExpression right, TypeSymbol type) {
        this.left = left;
        this.right = right;
        this.type = type;
        constantValue = ConstantFolding.FoldIs(left, right);
    }

    internal override BoundNodeKind kind => BoundNodeKind.IsExpression;

    internal override TypeSymbol type { get; }

    internal override ConstantValue constantValue { get; }

    internal BoundExpression left { get; }

    internal BoundExpression right { get; }
}
