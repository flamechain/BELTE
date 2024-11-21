using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="Syntax.BaseExpressionSyntax" />.
/// </summary>
internal sealed class BoundBaseExpression : BoundExpression {
    internal BoundBaseExpression(TypeSymbol type) {
        this.type = type;
    }

    internal override BoundNodeKind kind => BoundNodeKind.BaseExpression;

    internal override TypeSymbol type { get; }
}
