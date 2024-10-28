using System;
using System.Collections.Immutable;
using Buckle.Utilities;
using Microsoft.CodeAnalysis.PooledObjects;

namespace Buckle.CodeAnalysis.Symbols;

internal abstract class NamespaceOrTypeSymbol : Symbol, INamespaceOrTypeSymbol {
    protected static readonly ObjectPool<PooledDictionary<ReadOnlyMemory<char>, object>> NameToObjectPool =
        PooledDictionary<ReadOnlyMemory<char>, object>.CreatePool(ReadOnlyMemoryOfCharComparer.Instance);

    internal bool isNamespace => kind == SymbolKind.Namespace;

    internal bool isType => !isNamespace;

    internal sealed override bool isOverride => false;

    internal sealed override bool isVirtual => false;

    internal abstract ImmutableArray<Symbol> GetMembers();

    internal abstract ImmutableArray<Symbol> GetMembers(string name);

    internal virtual ImmutableArray<Symbol> GetMembersUnordered() => GetMembers();

    internal abstract ImmutableArray<NamedTypeSymbol> GetTypeMembers();

    internal ImmutableArray<NamedTypeSymbol> GetTypeMembers(string name)
        => GetTypeMembers(name.AsMemory());

    internal abstract ImmutableArray<NamedTypeSymbol> GetTypeMembers(ReadOnlyMemory<char> name);

    internal virtual ImmutableArray<NamedTypeSymbol> GetTypeMembersUnordered() => GetTypeMembers();

    ImmutableArray<ISymbol> INamespaceOrTypeSymbol.GetMembers() => GetMembers().Cast<Symbol, ISymbol>();

    ImmutableArray<ISymbol> INamespaceOrTypeSymbol.GetMembers(string name) => GetMembers(name).Cast<Symbol, ISymbol>();

    ImmutableArray<INamedTypeSymbol> INamespaceOrTypeSymbol.GetTypeMembers()
        => GetTypeMembers().Cast<NamedTypeSymbol, INamedTypeSymbol>();

    ImmutableArray<INamedTypeSymbol> INamespaceOrTypeSymbol.GetTypeMembers(string name)
        => GetTypeMembers(name.AsMemory()).Cast<NamedTypeSymbol, INamedTypeSymbol>();
}
