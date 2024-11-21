
namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="Syntax.IfStatementSyntax" />.
/// Doesn't survive lowering.
/// </summary>
internal sealed class BoundIfStatement : BoundStatement {
    internal BoundIfStatement(BoundExpression condition, BoundStatement then, BoundStatement elseStatement) {
        this.condition = condition;
        this.then = then;
        this.elseStatement = elseStatement;
    }

    internal override BoundNodeKind kind => BoundNodeKind.IfStatement;

    internal BoundExpression condition { get; }

    internal BoundStatement then { get; }

    internal BoundStatement elseStatement { get; }
}
