using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

internal static class OperatorFacts {
    internal static bool NoUserDefinedOperators(TypeSymbol type) {
        switch (type.typeKind) {
            case TypeKind.Class:
            case TypeKind.Struct:
            case TypeKind.TemplateParameter:
                break;
            default:
                return true;
        }

        switch (type.specialType) {
            case SpecialType.Any:
            case SpecialType.Array:
            case SpecialType.Bool:
            case SpecialType.Char:
            case SpecialType.Decimal:
            case SpecialType.Int:
            case SpecialType.String:
            case SpecialType.Void:
                return true;
        }

        return false;
    }
}
