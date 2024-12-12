using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

internal class InContainerBinder : Binder {
    internal InContainerBinder(NamespaceOrTypeSymbol container, Binder next) : base(next) {
        this.container = container;
    }

    internal NamespaceOrTypeSymbol container { get; }

    internal override Symbol containingMember => container;

    internal override bool IsAccessibleHelper(
        Symbol symbol,
        TypeSymbol accessThroughType,
        out bool failedThroughTypeCheck) {
        failedThroughTypeCheck = false;
        return IsSymbolAccessibleConditional(symbol, compilation.globalNamespaceInternal);
    }

    private protected override SourceDataContainerSymbol LookupLocal(SyntaxToken identifier) {
        return null;
    }

    private protected override LocalFunctionSymbol LookupLocalFunction(SyntaxToken identifier) {
        return null;
    }
}
