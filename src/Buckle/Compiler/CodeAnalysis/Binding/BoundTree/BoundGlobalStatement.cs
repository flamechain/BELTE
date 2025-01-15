using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="GlobalStatementSyntax" />.
/// Doesn't survive compilation.
/// </summary>
internal sealed class BoundGlobalStatement : BoundInitializer {
    internal BoundGlobalStatement(SyntaxNode syntax, BoundStatement statement, bool hasErrors = false)
        : base(BoundKind.GlobalStatement, syntax, hasErrors) {
        this.statement = statement;
    }

    internal BoundStatement statement { get; }
}
