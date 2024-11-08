
namespace Buckle.CodeAnalysis.Binding;

internal sealed class BoundNonConstructorMethodBody : BoundMethodBodyBase {
    internal BoundNonConstructorMethodBody(BoundBlockStatement body) : base(body) { }

    internal override BoundNodeKind kind => BoundNodeKind.NonConstructorMethodBody;
}
