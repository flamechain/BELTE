using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="BinaryExpressionSyntax" />.
/// </summary>
internal sealed class BoundAsExpression : BoundExpression {
    internal BoundAsExpression(
        SyntaxNode syntax,
        BoundExpression left,
        BoundExpression right,
        TypeSymbol type,
        bool hasErrors = false)
        : base(BoundKind.AsExpression, syntax, type, hasErrors) {
        this.left = left;
        this.right = right;
    }

    internal BoundExpression left { get; }

    internal BoundExpression right { get; }
}
