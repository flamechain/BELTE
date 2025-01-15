using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="ThisExpressionSyntax" />.
/// </summary>
internal sealed class BoundThisExpression : BoundExpression {
    internal BoundThisExpression(SyntaxNode syntax, TypeSymbol type, bool hasErrors = false)
        : base(BoundKind.ThisExpression, syntax, type, hasErrors) { }
}
