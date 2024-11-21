using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="Syntax.EmptyExpressionSyntax" />.
/// Converted to NOP statements when emitting.
/// </summary>
internal sealed class BoundEmptyExpression : BoundExpression {
    internal BoundEmptyExpression() { }

    internal override BoundNodeKind kind => BoundNodeKind.EmptyExpression;

    internal override TypeSymbol type => null;
}
