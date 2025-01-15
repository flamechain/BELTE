using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="ContinueStatementSyntax" />.
/// Only used when transpiling, as most lowering is skipped and gotos are not created.
/// </summary>
internal sealed class BoundContinueStatement : BoundStatement {
    internal BoundContinueStatement(SyntaxNode syntax, bool hasErrors = false)
        : base(BoundKind.ContinueStatement, syntax, hasErrors) { }
}
