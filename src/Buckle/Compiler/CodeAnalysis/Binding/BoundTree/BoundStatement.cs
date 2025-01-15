using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// A bound statement, bound from a <see cref="StatementSyntax" />.
/// </summary>
internal abstract class BoundStatement : BoundNode {
    private protected BoundStatement(BoundKind kind, SyntaxNode syntax, bool hasErrors)
        : base(kind, syntax, hasErrors) { }
}
