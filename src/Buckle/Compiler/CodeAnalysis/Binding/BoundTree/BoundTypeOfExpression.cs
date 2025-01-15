using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="TypeOfExpressionSyntax" />.
/// </summary>
internal sealed class BoundTypeOfExpression : BoundExpression {
    internal BoundTypeOfExpression(SyntaxNode syntax, TypeSymbol type, bool hasErrors = false)
        : base(BoundKind.TypeOfExpression, syntax, type, hasErrors) { }
}
