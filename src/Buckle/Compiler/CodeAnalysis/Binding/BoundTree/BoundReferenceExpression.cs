using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="Syntax.ReferenceExpressionSyntax" />.
/// </summary>
internal sealed class BoundReferenceExpression : BoundExpression {
    internal BoundReferenceExpression(BoundExpression expression, TypeSymbol type) {
        this.expression = expression;
        this.type = type;
    }

    internal override BoundNodeKind kind => BoundNodeKind.ReferenceExpression;

    internal override TypeSymbol type { get; }

    internal BoundExpression expression { get; }
}
