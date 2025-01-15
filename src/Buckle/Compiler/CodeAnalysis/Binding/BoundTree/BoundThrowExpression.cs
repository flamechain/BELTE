using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="ThrowExpressionSyntax" />.
/// </summary>
internal sealed class BoundThrowExpression : BoundExpression {
    internal BoundThrowExpression(SyntaxNode syntax, BoundExpression exception, TypeSymbol type, bool hasErrors = false)
        : base(BoundKind.ThrowExpression, syntax, type, hasErrors) {
        this.exception = exception;
    }

    internal BoundExpression exception { get; }
}
