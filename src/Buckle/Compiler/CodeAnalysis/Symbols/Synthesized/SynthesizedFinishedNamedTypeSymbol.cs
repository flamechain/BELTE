using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using Buckle.CodeAnalysis.Binding;
using Buckle.Utilities;

namespace Buckle.CodeAnalysis.Symbols;

internal sealed class SynthesizedFinishedNamedTypeSymbol : WrappedNamedTypeSymbol {
    private ImmutableArray<Symbol> _allMembers;
    private Dictionary<ReadOnlyMemory<char>, ImmutableArray<NamespaceOrTypeSymbol>> _nameToMembersMap;
    private Dictionary<ReadOnlyMemory<char>, ImmutableArray<NamedTypeSymbol>> _nameToTypeMembersMap;
    private bool _allMembersIsSorted;

    internal SynthesizedFinishedNamedTypeSymbol(
        NamedTypeSymbol underlyingType,
        Symbol containingSymbol,
        ImmutableArray<Symbol>? members = null)
        : base(underlyingType) {
        this.containingSymbol = containingSymbol;
        _allMembers = members ?? underlyingType.GetMembers();
    }

    public override ImmutableArray<TemplateParameterSymbol> templateParameters => [];

    public override ImmutableArray<BoundExpression> templateConstraints => [];

    public override ImmutableArray<TypeOrConstant> templateArguments => [];

    internal override IEnumerable<string> memberNames => GetMembers().Select(m => m.name);

    internal override NamedTypeSymbol constructedFrom => this;

    internal override NamedTypeSymbol baseType => underlyingNamedType.baseType;

    internal override Symbol containingSymbol { get; }

    internal override NamedTypeSymbol GetDeclaredBaseType(ConsList<TypeSymbol> basesBeingResolved) {
        return baseType;
    }

    internal override ImmutableArray<Symbol> GetMembers() {
        if (_allMembersIsSorted)
            return _allMembers;

        var allMembers = _allMembers;

        if (allMembers.Length > 1) {
            allMembers = allMembers.Sort(LexicalOrderSymbolComparer.Instance);
            ImmutableInterlocked.InterlockedExchange(ref _allMembers, allMembers);
        }

        _allMembersIsSorted = true;
        return allMembers;
    }

    internal override ImmutableArray<Symbol> GetMembers(string name) {
        return GetNameToMembersMap().TryGetValue(name.AsMemory(), out var members)
            ? members.Cast<NamespaceOrTypeSymbol, Symbol>()
            : [];
    }

    internal override ImmutableArray<NamedTypeSymbol> GetTypeMembers() {
        return GetNameToTypeMembersMap().Flatten(LexicalOrderSymbolComparer.Instance);
    }

    internal override ImmutableArray<NamedTypeSymbol> GetTypeMembers(ReadOnlyMemory<char> name) {
        return GetNameToTypeMembersMap().TryGetValue(name, out var members) ? members : [];
    }

    private Dictionary<ReadOnlyMemory<char>, ImmutableArray<NamespaceOrTypeSymbol>> GetNameToMembersMap() {
        if (_nameToMembersMap is null)
            Interlocked.CompareExchange(ref _nameToMembersMap, MakeNameToMembersMap(), null);

        return _nameToMembersMap;
    }

    private Dictionary<ReadOnlyMemory<char>, ImmutableArray<NamedTypeSymbol>> GetNameToTypeMembersMap() {
        if (_nameToTypeMembersMap is null) {
            Interlocked.CompareExchange(
                ref _nameToTypeMembersMap,
                ImmutableArrayExtensions
                    .GetTypesFromMemberMap<ReadOnlyMemory<char>, NamespaceOrTypeSymbol, NamedTypeSymbol>(
                        GetNameToMembersMap(),
                        ReadOnlyMemoryOfCharComparer.Instance
                    ),
                null
            );
        }

        return _nameToTypeMembersMap;
    }

    private Dictionary<ReadOnlyMemory<char>, ImmutableArray<NamespaceOrTypeSymbol>> MakeNameToMembersMap() {
        var builder = NameToObjectPool.Allocate();

        foreach (var symbol in _allMembers)
            ImmutableArrayExtensions.AddToMultiValueDictionaryBuilder(builder, symbol.name.AsMemory(), symbol);

        var result = new Dictionary<ReadOnlyMemory<char>, ImmutableArray<NamespaceOrTypeSymbol>>(
            builder.Count,
            ReadOnlyMemoryOfCharComparer.Instance
        );

        ImmutableArrayExtensions.CreateNameToMembersMap<
                ReadOnlyMemory<char>,
                NamespaceOrTypeSymbol,
                NamedTypeSymbol,
                NamespaceSymbol
            >(builder, result);

        builder.Free();
        return result;
    }
}
