using System.Diagnostics;

namespace Buckle.CodeAnalysis.Binding;

internal abstract partial class BoundTreeVisitor {
    private protected BoundTreeVisitor() { }

    [DebuggerHidden]
    internal virtual BoundNode Visit(BoundNode node) {
        if (node is not null)
            return node.Accept(this);

        return null;
    }

    [DebuggerHidden]
    internal virtual BoundNode DefaultVisit(BoundNode node) {
        return null;
    }
}
