
namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="Syntax.ReturnStatementSyntax" />.
/// </summary>
internal sealed class BoundReturnStatement : BoundStatement {
    internal BoundReturnStatement(BoundExpression expression) {
        this.expression = expression;
    }

    internal override BoundNodeKind kind => BoundNodeKind.ReturnStatement;

    internal BoundExpression expression { get; }
}
