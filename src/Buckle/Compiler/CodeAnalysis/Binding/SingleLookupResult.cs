using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

internal readonly struct SingleLookupResult {
    internal readonly LookupResultKind kind;
    internal readonly Symbol symbol;

    internal SingleLookupResult(LookupResultKind kind, Symbol symbol) {
        this.kind = kind;
        this.symbol = symbol;
    }
}
