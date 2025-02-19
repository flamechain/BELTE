using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Symbols;

internal static class SymbolExtensions {
    internal static BelteSyntaxNode GetNonNullSyntaxNode(this Symbol symbol) {
        if (symbol is not null) {
            var reference = symbol.syntaxReference;

            if (reference is null && symbol.isImplicitlyDeclared) {
                var containingSymbol = symbol.containingSymbol;

                if (containingSymbol is not null)
                    reference = containingSymbol.syntaxReference;
            }

            if (reference is not null)
                return (BelteSyntaxNode)reference.node;
        }

        return SyntaxTree.Dummy.GetRoot();
    }

    internal static int GetArity(this Symbol symbol) {
        if (symbol is not null) {
            switch (symbol.kind) {
                case SymbolKind.NamedType:
                    return ((NamedTypeSymbol)symbol).arity;
                case SymbolKind.Method:
                    return ((MethodSymbol)symbol).arity;
            }
        }

        return 0;
    }
}
