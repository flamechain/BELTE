using System.Collections.Immutable;

namespace Buckle.CodeAnalysis.Binding;

internal partial class Binder {
    internal struct ProcessedFieldInitializers {
        internal ImmutableArray<BoundInitializer> boundinitializers { get; set; }
        internal BoundStatement loweredInitializers { get; set; }
        internal bool hasErrors { get; set; }
    }
}
