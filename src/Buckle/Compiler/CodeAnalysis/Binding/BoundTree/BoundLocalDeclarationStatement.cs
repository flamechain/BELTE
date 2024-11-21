
namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="Syntax.LocalDeclarationStatementSyntax" />.
/// </summary>
internal sealed class BoundLocalDeclarationStatement : BoundStatement {
    internal BoundLocalDeclarationStatement(BoundDataContainerDeclaration declaration) {
        this.declaration = declaration;
    }

    internal override BoundNodeKind kind => BoundNodeKind.LocalDeclarationStatement;

    internal BoundDataContainerDeclaration declaration { get; }
}
