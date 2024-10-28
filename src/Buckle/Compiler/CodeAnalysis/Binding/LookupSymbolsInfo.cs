using System.Collections.Generic;
using Buckle.CodeAnalysis.Symbols;
using Microsoft.CodeAnalysis.PooledObjects;

namespace Buckle.CodeAnalysis.Binding;

internal sealed partial class LookupSymbolsInfo {
    private static readonly ObjectPool<LookupSymbolsInfo> Pool
        = new ObjectPool<LookupSymbolsInfo>(() => new LookupSymbolsInfo(), 64);

    private readonly Dictionary<string, UniqueSymbolOrArities> _nameMap;

    private LookupSymbolsInfo() { }

    internal string filterName { get; set; }

    internal void AddSymbol(Symbol symbol, string name, int arity) {
        if (!_nameMap.TryGetValue(name, out var pair)) {
            pair = new UniqueSymbolOrArities(arity, symbol);
            _nameMap.Add(name, pair);
        } else {
            pair.AddSymbol(symbol, arity);
            _nameMap[name] = pair;
        }
    }

    internal void Clear() {
        _nameMap.Clear();
        filterName = null;
    }

    internal void Free() {
        Clear();
        Pool.Free(this);
    }

    internal static LookupSymbolsInfo GetInstance() {
        return Pool.Allocate();
    }
}
