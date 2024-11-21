
namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="Syntax.ExpressionStatementSyntax" />.
/// </summary>
internal sealed class BoundExpressionStatement : BoundStatement {
    internal BoundExpressionStatement(BoundExpression expression) {
        this.expression = expression;
    }

    internal override BoundNodeKind kind => BoundNodeKind.ExpressionStatement;

    internal BoundExpression expression { get; }
}
