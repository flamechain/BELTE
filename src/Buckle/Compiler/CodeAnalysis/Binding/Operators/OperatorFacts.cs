using Buckle.CodeAnalysis.Symbols;
using Buckle.Libraries;

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

    internal static bool IsValidObjectEquality(
        Conversions conversions,
        TypeSymbol leftType,
        bool leftIsNull,
        TypeSymbol rightType,
        bool rightIsNull) {
        if ((leftType is not null) && leftType.IsTemplateParameter()) {
            if (leftType.isPrimitiveType || (!leftType.isObjectType && !rightIsNull))
                return false;

            leftType = ((TemplateParameterSymbol)leftType).effectiveBaseClass;
        }

        if ((rightType is not null) && rightType.IsTemplateParameter()) {
            if (rightType.isPrimitiveType || (!rightType.isObjectType && !leftIsNull))
                return false;

            rightType = ((TemplateParameterSymbol)rightType).effectiveBaseClass;
        }

        var leftIsObjectType = (leftType is not null) && leftType.isObjectType;

        if (!leftIsObjectType && !leftIsNull)
            return false;

        var rightIsObjectType = (rightType is not null) && rightType.isObjectType;

        if (!rightIsObjectType && !rightIsNull)
            return false;

        if (leftIsNull || rightIsNull)
            return true;

        var leftConversion = conversions.ClassifyConversionFromType(leftType, rightType);

        if (leftConversion.isIdentity || leftConversion.isReference)
            return true;

        var rightConversion = conversions.ClassifyConversionFromType(rightType, leftType);

        if (rightConversion.isIdentity || rightConversion.isReference)
            return true;

        return false;
    }

    internal static BinaryOperatorSignature GetSignature(BinaryOperatorKind kind) {
        var left = LeftType(kind);

        switch (kind.Operator()) {
            case BinaryOperatorKind.Multiplication:
            case BinaryOperatorKind.Division:
            case BinaryOperatorKind.Subtraction:
            case BinaryOperatorKind.Modulo:
            case BinaryOperatorKind.And:
            case BinaryOperatorKind.Or:
            case BinaryOperatorKind.Power:
            case BinaryOperatorKind.Xor:
                return new BinaryOperatorSignature(kind, left, left, left);
            case BinaryOperatorKind.Addition:
                return new BinaryOperatorSignature(kind, left, RightType(kind), ReturnType(kind));
            case BinaryOperatorKind.LeftShift:
            case BinaryOperatorKind.RightShift:
            case BinaryOperatorKind.UnsignedRightShift:
                TypeSymbol rightType = CorLibrary.GetSpecialType(SpecialType.Int);
                return new BinaryOperatorSignature(kind, left, rightType, left);
            case BinaryOperatorKind.Equal:
            case BinaryOperatorKind.NotEqual:
            case BinaryOperatorKind.GreaterThan:
            case BinaryOperatorKind.LessThan:
            case BinaryOperatorKind.GreaterThanOrEqual:
            case BinaryOperatorKind.LessThanOrEqual:
                return new BinaryOperatorSignature(kind, left, left, CorLibrary.GetSpecialType(SpecialType.Bool));
        }

        return new BinaryOperatorSignature(kind, left, RightType(kind), ReturnType(kind));
    }

    private static TypeSymbol LeftType(BinaryOperatorKind kind) {
        return kind.OperandTypes() switch {
            BinaryOperatorKind.Int => CorLibrary.GetSpecialType(SpecialType.Int),
            BinaryOperatorKind.Decimal => CorLibrary.GetSpecialType(SpecialType.Decimal),
            BinaryOperatorKind.Bool => CorLibrary.GetSpecialType(SpecialType.Bool),
            BinaryOperatorKind.Object => CorLibrary.GetSpecialType(SpecialType.Object),
            BinaryOperatorKind.String => CorLibrary.GetSpecialType(SpecialType.String),
            _ => null,
        };
    }

    private static TypeSymbol RightType(BinaryOperatorKind kind) {
        return kind.OperandTypes() switch {
            BinaryOperatorKind.Int => CorLibrary.GetSpecialType(SpecialType.Int),
            BinaryOperatorKind.Decimal => CorLibrary.GetSpecialType(SpecialType.Decimal),
            BinaryOperatorKind.Bool => CorLibrary.GetSpecialType(SpecialType.Bool),
            BinaryOperatorKind.String => CorLibrary.GetSpecialType(SpecialType.String),
            BinaryOperatorKind.Object => CorLibrary.GetSpecialType(SpecialType.Object),
            _ => null,
        };
    }

    private static TypeSymbol ReturnType(BinaryOperatorKind kind) {
        return kind.OperandTypes() switch {
            BinaryOperatorKind.Int => CorLibrary.GetSpecialType(SpecialType.Int),
            BinaryOperatorKind.Decimal => CorLibrary.GetSpecialType(SpecialType.Decimal),
            BinaryOperatorKind.Bool => CorLibrary.GetSpecialType(SpecialType.Bool),
            BinaryOperatorKind.Object => CorLibrary.GetSpecialType(SpecialType.Object),
            BinaryOperatorKind.String => CorLibrary.GetSpecialType(SpecialType.String),
            _ => null,
        };
    }
}
