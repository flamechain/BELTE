using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="BreakStatementSyntax" />.
/// Only used when transpiling, as most lowering is skipped and gotos are not created.
/// </summary>
internal sealed class BoundBreakStatement : BoundStatement {
    internal BoundBreakStatement(SyntaxNode syntax, bool hasErrors = false)
        : base(BoundKind.BreakStatement, syntax, hasErrors) { }
}
