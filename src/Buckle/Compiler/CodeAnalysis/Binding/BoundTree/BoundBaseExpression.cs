using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="BaseExpressionSyntax" />.
/// </summary>
internal sealed class BoundBaseExpression : BoundExpression {
    internal BoundBaseExpression(SyntaxNode syntax, TypeSymbol type, bool hasErrors = false)
        : base(BoundKind.BaseExpression, syntax, type, hasErrors) { }
}
