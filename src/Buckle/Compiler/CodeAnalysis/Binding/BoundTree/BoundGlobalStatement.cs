
namespace Buckle.CodeAnalysis.Binding;

internal sealed class BoundGlobalStatement : BoundInitializer {
    internal BoundGlobalStatement(BoundStatement statement) {
        this.statement = statement;
    }

    internal override BoundNodeKind kind => BoundNodeKind.GlobalStatement;

    internal BoundStatement statement { get; }
}
