using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="BinaryExpressionSyntax" />.
/// </summary>
internal sealed class BoundNullCoalescingExpression : BoundExpression {
    internal BoundNullCoalescingExpression(
        SyntaxNode syntax,
        BoundExpression left,
        BoundExpression right,
        TypeSymbol type,
        ConstantValue constantValue,
        bool hasErrors = false)
        : base(BoundKind.NullCoalescingExpression, syntax, type, hasErrors) {
        this.left = left;
        this.right = right;
        this.constantValue = constantValue;
    }

    internal override ConstantValue constantValue { get; }

    internal BoundExpression left { get; }

    internal BoundExpression right { get; }
}
