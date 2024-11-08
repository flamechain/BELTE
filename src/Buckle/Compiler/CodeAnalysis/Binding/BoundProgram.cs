using System.Collections.Immutable;
using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

internal sealed class BoundProgram {
    internal BoundProgram(
        ImmutableDictionary<MethodSymbol, BoundBlockStatement> methodBodies,
        ImmutableArray<NamedTypeSymbol> types,
        MethodSymbol entryPoint,
        BoundProgram previous = null) {
        this.methodBodies = methodBodies;
        this.types = types;
        this.entryPoint = entryPoint;
        this.previous = previous;
    }

    internal ImmutableDictionary<MethodSymbol, BoundBlockStatement> methodBodies { get; }

    internal ImmutableArray<NamedTypeSymbol> types { get; }

    internal MethodSymbol entryPoint { get; }

    internal BoundProgram previous { get; }
}
