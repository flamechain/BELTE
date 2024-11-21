
namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="Syntax.ContinueStatementSyntax" />.
/// Only used when transpiling, as most lowering is skipped and gotos are not created.
/// </summary>
internal sealed class BoundContinueStatement : BoundStatement {
    internal override BoundNodeKind kind => BoundNodeKind.ContinueStatement;
}
