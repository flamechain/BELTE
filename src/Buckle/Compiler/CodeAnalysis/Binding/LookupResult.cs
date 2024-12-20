using Buckle.CodeAnalysis.Symbols;
using Buckle.Diagnostics;
using Microsoft.CodeAnalysis.PooledObjects;

namespace Buckle.CodeAnalysis.Binding;

internal sealed class LookupResult {
    private static readonly ObjectPool<LookupResult> PoolInstance = CreatePool();
    private readonly ObjectPool<LookupResult> _pool;

    private LookupResult(ObjectPool<LookupResult> pool) {
        _pool = pool;
        kind = LookupResultKind.Empty;
        symbols = [];
    }

    internal LookupResultKind kind { get; private set; }

    internal ArrayBuilder<Symbol> symbols { get; }

    internal Symbol singleSymbolOrDefault => (symbols.Count == 1) ? symbols[0] : null;

    internal bool isMultiViable => kind == LookupResultKind.Viable;

    internal BelteDiagnostic error { get; private set; }

    internal bool isClear => kind == LookupResultKind.Empty && error is null && symbols.Count == 0;

    internal static SingleLookupResult Good(Symbol symbol) {
        return new SingleLookupResult(LookupResultKind.Viable, symbol, null);
    }

    internal void Clear() {
        kind = LookupResultKind.Empty;
        symbols.Clear();
        error = null;
    }

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

    internal void MergePrioritized(LookupResult other) {
        if (other.kind > kind)
            SetFrom(other);
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

    internal static ObjectPool<LookupResult> CreatePool() {
        ObjectPool<LookupResult> pool = null;
        pool = new ObjectPool<LookupResult>(() => new LookupResult(pool), 128);
        return pool;
    }

    internal static LookupResult GetInstance() {
        return PoolInstance.Allocate();
    }

    internal void Free() {
        Clear();
        _pool?.Free(this);
    }
}
