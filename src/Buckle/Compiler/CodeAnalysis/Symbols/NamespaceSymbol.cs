using System;
using System.Collections.Immutable;

namespace Buckle.CodeAnalysis.Symbols;

internal abstract class NamespaceSymbol : NamespaceOrTypeSymbol, INamespaceSymbol {
    public sealed override SymbolKind kind => SymbolKind.Namespace;

    public virtual bool isGlobalNamespace => containingNamespace is null;

    public NamespaceKind namespaceKind => extent.kind;

    public Compilation containingCompilation
        => namespaceKind == NamespaceKind.Compilation ? extent.compilation : null;

    internal sealed override bool isImplicitlyDeclared => isGlobalNamespace;

    internal sealed override NamedTypeSymbol containingType => null;

    internal sealed override bool isStatic => true;

    internal sealed override bool isAbstract => false;

    internal sealed override bool isSealed => false;

    internal sealed override Accessibility declaredAccessibility => Accessibility.Public;

    internal abstract NamespaceExtent extent { get; }

    internal abstract ImmutableArray<Symbol> GetMembers(ReadOnlyMemory<char> name);

    internal sealed override ImmutableArray<Symbol> GetMembers(string name)
        => GetMembers(name.AsMemory());
}
