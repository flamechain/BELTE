using Buckle.CodeAnalysis.Symbols;
using Microsoft.CodeAnalysis.PooledObjects;

namespace Buckle.CodeAnalysis.Binding;

internal sealed class LookupResult {
    private readonly ObjectPool<LookupResult> _pool;

    private LookupResult(ObjectPool<LookupResult> pool) {
        _pool = pool;
        kind = LookupResultKind.Empty;
        symbols = [];
    }

    internal LookupResultKind kind { get; private set; }

    internal ArrayBuilder<Symbol> symbols { get; }

    internal void MergeEqual(LookupResult other) {
        if (kind > other.kind)
            return;
        else if (other.kind > kind)
            SetFrom(other);
        else if (kind != LookupResultKind.Viable)
            return;
        else
            symbols.AddRange(other.symbols);
    }

    internal void MergeEqual(SingleLookupResult result) {
        if (result.kind > kind)
            SetFrom(result);
        else if (kind == result.kind && result.symbol is not null)
            symbols.Add(result.symbol);
    }

    internal void SetFrom(LookupResult other) {
        kind = other.kind;
        symbols.Clear();
        symbols.AddRange(other.symbols);
    }

    internal void SetFrom(SingleLookupResult other) {
        kind = other.kind;
        symbols.Clear();
        symbols.Add(other.symbol);
    }
}
