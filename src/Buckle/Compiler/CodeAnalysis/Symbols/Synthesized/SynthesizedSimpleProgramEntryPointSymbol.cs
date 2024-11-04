
using System.Collections.Immutable;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Symbols;

internal sealed class SynthesizedEntryPoint : MethodSymbol {
    internal SynthesizedEntryPoint(Symbol containingSymbol, ImmutableArray<GlobalStatementSyntax> syntax) {
        this.containingSymbol = containingSymbol;
    }

    internal override Symbol containingSymbol { get; }
}
