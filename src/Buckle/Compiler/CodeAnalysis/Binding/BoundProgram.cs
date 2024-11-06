using System.Collections.Immutable;
using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

internal sealed class BoundProgram {
    internal BoundProgram(
        ImmutableDictionary<MethodSymbol, BoundBlockStatement> methodBodies,
        ImmutableArray<NamedTypeSymbol> types,
        MethodSymbol entryPoint) {
        this.methodBodies = methodBodies;
        this.types = types;
        this.entryPoint = entryPoint;
    }

    internal ImmutableDictionary<MethodSymbol, BoundBlockStatement> methodBodies { get; }

    internal ImmutableArray<NamedTypeSymbol> types { get; }

    internal MethodSymbol entryPoint { get; }
}
