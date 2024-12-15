using Buckle.Diagnostics;

namespace Buckle.CodeAnalysis.Symbols;

// TODO This will be populated when necessary
internal sealed class UnsupportedMetadataTypeSymbol : ErrorTypeSymbol {
    internal UnsupportedMetadataTypeSymbol() { }

    internal override bool mangleName => false;

    internal override BelteDiagnostic error => null;
}
