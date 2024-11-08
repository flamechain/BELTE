
namespace Buckle.CodeAnalysis.Binding;

internal abstract class BoundMethodBodyBase : BoundNode {
    private protected BoundMethodBodyBase(BoundBlockStatement body) {
        this.body = body;
    }

    internal BoundBlockStatement body { get; }
}
