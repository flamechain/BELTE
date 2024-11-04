using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis;

internal sealed class TypeCompilationState {
    internal TypeCompilationState(NamedTypeSymbol type, Compilation compilation) {
        this.type = type;
        this.compilation = compilation;
    }

    internal Compilation compilation { get; }

    internal NamedTypeSymbol type { get; }
}
