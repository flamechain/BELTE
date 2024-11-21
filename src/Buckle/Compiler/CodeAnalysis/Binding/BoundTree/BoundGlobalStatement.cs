
namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="Syntax.GlobalStatementSyntax" />.
/// Doesn't survive compilation.
/// </summary>
internal sealed class BoundGlobalStatement : BoundInitializer {
    internal BoundGlobalStatement(BoundStatement statement) {
        this.statement = statement;
    }

    internal override BoundNodeKind kind => BoundNodeKind.GlobalStatement;

    internal BoundStatement statement { get; }
}
