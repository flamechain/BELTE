using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="Syntax.ThrowExpressionSyntax" />.
/// </summary>
internal sealed class BoundThrowExpression : BoundExpression {
    internal BoundThrowExpression(BoundExpression exception) {
        this.exception = exception;
        type = exception.type;
    }

    internal override BoundNodeKind kind => BoundNodeKind.ThrowExpression;

    internal override TypeSymbol type { get; }

    internal BoundExpression exception { get; }
}
