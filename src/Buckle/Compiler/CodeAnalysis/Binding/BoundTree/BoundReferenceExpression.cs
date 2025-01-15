using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="ReferenceExpressionSyntax" />.
/// </summary>
internal sealed class BoundReferenceExpression : BoundExpression {
    internal BoundReferenceExpression(
        SyntaxNode syntax,
        BoundExpression expression,
        TypeSymbol type,
        bool hasErrors = false)
        : base(BoundKind.ReferenceExpression, syntax, type, hasErrors) {
        this.expression = expression;
    }

    internal BoundExpression expression { get; }
}
