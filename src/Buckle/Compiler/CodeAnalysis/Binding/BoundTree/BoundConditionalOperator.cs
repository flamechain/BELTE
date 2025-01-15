using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="TernaryExpressionSyntax" />.
/// </summary>
internal sealed class BoundConditionalExpression : BoundExpression {
    internal BoundConditionalExpression(
        SyntaxNode syntax,
        BoundExpression left,
        BoundExpression center,
        BoundExpression right,
        TypeSymbol type,
        bool hasErrors = false)
        : base(BoundKind.ConditionalExpression, syntax, type, hasErrors) {
        this.left = left;
        this.center = center;
        this.right = right;
        constantValue = ConstantFolding.FoldConditional(left, center, right, type);
    }

    internal override ConstantValue constantValue { get; }

    internal BoundExpression left { get; }

    internal BoundExpression center { get; }

    internal BoundExpression right { get; }
}
