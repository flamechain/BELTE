using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="TryStatementSyntax" />.
/// <see cref="CatchClauseSyntax" /> and <see cref="FinallyClauseSyntax" /> are bound into just their bodies.
/// </summary>
internal sealed class BoundTryStatement : BoundStatement {
    internal BoundTryStatement(
        SyntaxNode syntax,
        BoundBlockStatement body,
        BoundBlockStatement catchBody,
        BoundBlockStatement finallyBody,
        bool hasErrors = false)
        : base(BoundKind.TryStatement, syntax, hasErrors) {
        this.body = body;
        this.catchBody = catchBody;
        this.finallyBody = finallyBody;
    }

    internal BoundBlockStatement body { get; }

    internal BoundBlockStatement catchBody { get; }

    internal BoundBlockStatement finallyBody { get; }
}
