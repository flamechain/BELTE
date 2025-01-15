using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="EmptyExpressionSyntax" />.
/// Converted to NOP statements when emitting.
/// </summary>
internal sealed class BoundEmptyExpression : BoundExpression {
    internal BoundEmptyExpression(SyntaxNode syntax) : base(BoundKind.EmptyExpression, syntax, null, false) { }
}
