
namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="Syntax.TryStatementSyntax" />.
/// <see cref="Syntax.CatchClauseSyntax" /> and <see cref="Syntax.FinallyClauseSyntax" /> are bound into just their
/// bodies.
/// </summary>
internal sealed class BoundTryStatement : BoundStatement {
    internal BoundTryStatement(
        BoundBlockStatement body,
        BoundBlockStatement catchBody,
        BoundBlockStatement finallyBody) {
        this.body = body;
        this.catchBody = catchBody;
        this.finallyBody = finallyBody;
    }

    internal override BoundNodeKind kind => BoundNodeKind.TryStatement;

    internal BoundBlockStatement body { get; }

    internal BoundBlockStatement catchBody { get; }

    internal BoundBlockStatement finallyBody { get; }
}
