
using System.Collections.Immutable;
using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

internal sealed class BoundConstructorMethodBody : BoundMethodBodyBase {
    internal BoundConstructorMethodBody(
        ImmutableArray<DataContainerSymbol> locals,
        BoundStatement initializer,
        BoundBlockStatement body) : base(body) {
        this.locals = locals;
        this.initializer = initializer;
    }

    internal override BoundNodeKind kind => BoundNodeKind.ConstructorMethodBody;

    internal ImmutableArray<DataContainerSymbol> locals { get; }

    internal BoundStatement initializer { get; }
}
