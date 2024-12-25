using Buckle.Utilities;

namespace Buckle.CodeAnalysis.Symbols;

internal static class SymbolKindExtensions {
    internal static int ToSortOrder(this SymbolKind kind) {
        return kind switch {
            SymbolKind.Field => 0,
            SymbolKind.Method => 1,
            SymbolKind.NamedType => 2,
            SymbolKind.ArrayType => 3,
            SymbolKind.ErrorType => 4,
            SymbolKind.Label => 5,
            SymbolKind.Local => 6,
            SymbolKind.Parameter => 7,
            SymbolKind.TemplateParameter => 8,
            _ => throw ExceptionUtilities.UnexpectedValue(kind),
        };
    }
}
