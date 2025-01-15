using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="LocalDeclarationStatementSyntax" />.
/// </summary>
internal sealed class BoundLocalDeclarationStatement : BoundStatement {
    internal BoundLocalDeclarationStatement(
        SyntaxNode syntax,
        BoundDataContainerDeclaration declaration,
        bool hasErrors = false)
        : base(BoundKind.LocalDeclarationStatement, syntax, hasErrors) {
        this.declaration = declaration;
    }

    internal BoundDataContainerDeclaration declaration { get; }
}
