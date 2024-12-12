using System;
using System.Collections.Immutable;
using Buckle.CodeAnalysis.Symbols;
using Buckle.Utilities;
using Microsoft.CodeAnalysis.PooledObjects;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Folds/evaluates simple BoundExpressions during compile time.
/// </summary>
internal static class ConstantFolding {
    internal static ConstantValue FoldBinary(
        BoundExpression left,
        BoundExpression right,
        BinaryOperatorKind opKind,
        TypeSymbol type) {
        var leftConstant = left.constantValue;
        var rightConstant = right.constantValue;

        if (opKind == BinaryOperatorKind.Error)
            return null;

        // With and/or operators allow one side to be null
        if (opKind == BinaryOperatorKind.ConditionalAnd) {
            if ((leftConstant is not null && leftConstant.value is not null && !(bool)leftConstant.value) ||
                (rightConstant is not null && rightConstant.value is not null && !(bool)rightConstant.value)) {
                return new ConstantValue(false);
            }
        }

        if (opKind == BinaryOperatorKind.ConditionalOr) {
            if ((leftConstant is not null && leftConstant.value is not null && (bool)leftConstant.value) ||
                (rightConstant is not null && rightConstant.value is not null && (bool)rightConstant.value)) {
                return new ConstantValue(true);
            }
        }

        if (ConstantValue.IsNull(leftConstant) || ConstantValue.IsNull(rightConstant))
            return new ConstantValue(null);

        if (leftConstant is null || rightConstant is null)
            return null;

        var leftValue = leftConstant.value;
        var rightValue = rightConstant.value;
        var specialType = type.specialType;

        leftValue = LiteralUtilities.Cast(leftValue, type);
        rightValue = LiteralUtilities.Cast(rightValue, type);

        switch (opKind) {
            case BinaryOperatorKind.Addition:
                if (specialType == SpecialType.Int)
                    return new ConstantValue((int)leftValue + (int)rightValue);
                else if (specialType == SpecialType.String)
                    return new ConstantValue((string)leftValue + (string)rightValue);
                else
                    return new ConstantValue((double)leftValue + (double)rightValue);
            case BinaryOperatorKind.Subtraction:
                if (specialType == SpecialType.Int)
                    return new ConstantValue((int)leftValue - (int)rightValue);
                else
                    return new ConstantValue((double)leftValue - (double)rightValue);
            case BinaryOperatorKind.Multiplication:
                if (specialType == SpecialType.Int)
                    return new ConstantValue((int)leftValue * (int)rightValue);
                else
                    return new ConstantValue((double)leftValue * (double)rightValue);
            case BinaryOperatorKind.Division:
                if (specialType == SpecialType.Int) {
                    if ((int)rightValue != 0)
                        return new ConstantValue((int)leftValue / (int)rightValue);
                } else {
                    if ((double)rightValue != 0)
                        return new ConstantValue((double)leftValue / (double)rightValue);
                }

                return null;
            case BinaryOperatorKind.Power:
                if (specialType == SpecialType.Int)
                    return new ConstantValue((int)Math.Pow((int)leftValue, (int)rightValue));
                else
                    return new ConstantValue((double)Math.Pow((double)leftValue, (double)rightValue));
            case BinaryOperatorKind.ConditionalAnd:
                return new ConstantValue((bool)leftValue && (bool)rightValue);
            case BinaryOperatorKind.ConditionalOr:
                return new ConstantValue((bool)leftValue || (bool)rightValue);
            case BinaryOperatorKind.Equal:
                return new ConstantValue(Equals(leftValue, rightValue));
            case BinaryOperatorKind.NotEqual:
                return new ConstantValue(!Equals(leftValue, rightValue));
            case BinaryOperatorKind.LessThan:
                if (specialType == SpecialType.Int)
                    return new ConstantValue((int)leftValue < (int)rightValue);
                else
                    return new ConstantValue((double)leftValue < (double)rightValue);
            case BinaryOperatorKind.GreaterThan:
                if (specialType == SpecialType.Int)
                    return new ConstantValue((int)leftValue > (int)rightValue);
                else
                    return new ConstantValue((double)leftValue > (double)rightValue);
            case BinaryOperatorKind.LessThanOrEqual:
                if (specialType == SpecialType.Int)
                    return new ConstantValue((int)leftValue <= (int)rightValue);
                else
                    return new ConstantValue((double)leftValue <= (double)rightValue);
            case BinaryOperatorKind.GreaterThanOrEqual:
                if (specialType == SpecialType.Int)
                    return new ConstantValue((int)leftValue >= (int)rightValue);
                else
                    return new ConstantValue((double)leftValue >= (double)rightValue);
            case BinaryOperatorKind.And:
                if (specialType == SpecialType.Int)
                    return new ConstantValue((int)leftValue & (int)rightValue);
                else
                    return new ConstantValue((bool)leftValue & (bool)rightValue);
            case BinaryOperatorKind.Or:
                if (specialType == SpecialType.Int)
                    return new ConstantValue((int)leftValue | (int)rightValue);
                else
                    return new ConstantValue((bool)leftValue | (bool)rightValue);
            case BinaryOperatorKind.Xor:
                if (specialType == SpecialType.Int)
                    return new ConstantValue((int)leftValue ^ (int)rightValue);
                else
                    return new ConstantValue((bool)leftValue ^ (bool)rightValue);
            case BinaryOperatorKind.LeftShift:
                return new ConstantValue((int)leftValue << (int)rightValue);
            case BinaryOperatorKind.RightShift:
                return new ConstantValue((int)leftValue >> (int)rightValue);
            case BinaryOperatorKind.UnsignedRightShift:
                return new ConstantValue((int)leftValue >>> (int)rightValue);
            case BinaryOperatorKind.Modulo:
                if (specialType == SpecialType.Int)
                    return new ConstantValue((int)leftValue % (int)rightValue);
                else
                    return new ConstantValue((double)leftValue % (double)rightValue);
            default:
                throw ExceptionUtilities.UnexpectedValue(opKind);
        }
    }

    internal static ConstantValue FoldNullCoalescing(BoundExpression left, BoundExpression right) {
        var leftConstant = left.constantValue;
        var rightConstant = right.constantValue;

        if (leftConstant is not null && leftConstant.value is not null)
            return new ConstantValue(leftConstant.value);

        if (leftConstant is not null && leftConstant.value is null && rightConstant is not null)
            return new ConstantValue(rightConstant.value);

        return null;
    }

    internal static ConstantValue FoldIs(BoundExpression left, BoundExpression right) {
        // TODO Should be able to expand this to cover some `is object` or `is primitive` expressions to
        var leftConstant = left.constantValue;
        var rightConstant = right.constantValue;

        if (ConstantValue.IsNull(leftConstant) && ConstantValue.IsNull(rightConstant))
            return new ConstantValue(true);

        if (ConstantValue.IsNotNull(leftConstant) && ConstantValue.IsNull(rightConstant))
            return new ConstantValue(false);

        return null;
    }

    internal static ConstantValue FoldIsnt(BoundExpression left, BoundExpression right) {
        var leftConstant = left.constantValue;
        var rightConstant = right.constantValue;

        if (ConstantValue.IsNull(leftConstant) && ConstantValue.IsNull(rightConstant))
            return new ConstantValue(false);

        if (ConstantValue.IsNotNull(leftConstant) && ConstantValue.IsNull(rightConstant))
            return new ConstantValue(true);

        return null;
    }

    internal static ConstantValue FoldNullAssert(BoundExpression operand) {
        if (ConstantValue.IsNotNull(operand.constantValue))
            return operand.constantValue;

        return null;
    }

    internal static ConstantValue FoldUnary(BoundExpression operand, UnaryOperatorKind opKind, TypeSymbol type) {
        var operandSpecialType = operand.type.specialType;

        if (operand.constantValue is null || opKind == UnaryOperatorKind.Error)
            return null;

        var value = operand.constantValue.value;

        if (value is null)
            return new ConstantValue(null);

        switch (opKind) {
            case UnaryOperatorKind.UnaryPlus:
                if (operandSpecialType == SpecialType.Int)
                    return new ConstantValue((int)operand.constantValue.value);
                else
                    return new ConstantValue((double)operand.constantValue.value);
            case UnaryOperatorKind.UnaryMinus:
                if (operandSpecialType == SpecialType.Int)
                    return new ConstantValue(-(int)operand.constantValue.value);
                else
                    return new ConstantValue(-(double)operand.constantValue.value);
            case UnaryOperatorKind.LogicalNegation:
                return new ConstantValue(!(bool)operand.constantValue.value);
            case UnaryOperatorKind.BitwiseComplement:
                return new ConstantValue(~(int)operand.constantValue.value);
            default:
                throw ExceptionUtilities.UnexpectedValue(opKind);
        }
    }

    /// <summary>
    /// Folds a <see cref="BoundConditionalOperatorExpression" /> (if possible).
    /// </summary>
    /// <param name="left">Left operand.</param>
    /// <param name="center">Center operand.</param>
    /// <param name="right">Right operand.</param>
    /// <returns><see cref="ConstantValue" />, returns null if folding is not possible.</returns>
    internal static ConstantValue FoldConditional(
        BoundExpression left,
        BoundExpression center,
        BoundExpression right) {
        if (ConstantValue.IsNotNull(left.constantValue) &&
            (bool)left.constantValue.value &&
            center.constantValue is not null) {
            return new ConstantValue(center.constantValue.value);
        }

        if (ConstantValue.IsNotNull(left.constantValue) &&
            !(bool)left.constantValue.value &&
            right.constantValue is not null) {
            return new ConstantValue(right.constantValue.value);
        }

        return null;
    }

    /// <summary>
    /// Folds a <see cref="BoundCastExpression" /> (if possible).
    /// </summary>
    /// <param name="expression">Expression operand.</param>
    /// <param name="type">Casting to type.</param>
    /// <returns><see cref="ConstantValue" />, returns null if folding is not possible.</returns>
    internal static ConstantValue FoldCast(BoundExpression expression, TypeWithAnnotations type) {
        if (expression.constantValue is null)
            return null;

        if (expression.constantValue.value is null && !type.isNullable)
            return null;

        // TODO Does this need to be inside a try-catch?
        var castedValue = LiteralUtilities.Cast(expression.constantValue.value, type);
        return new ConstantValue(castedValue);
    }

    /// <summary>
    /// Folds a <see cref="BoundInitializerListExpression" /> (if possible).
    /// </summary>
    /// <param name="items">Initializer list contents.</param>
    /// <returns><see cref="ConstantValue" />, returns null if folding is not possible.</returns>
    internal static ConstantValue FoldInitializerList(ImmutableArray<BoundExpression> items) {
        var foldedItems = ArrayBuilder<ConstantValue>.GetInstance();

        foreach (var item in items) {
            if (item.constantValue is not null)
                foldedItems.Add(item.constantValue);
            else
                return null;
        }

        return new ConstantValue(foldedItems.ToImmutableAndFree());
    }

    /// <summary>
    /// Folds a <see cref="BoundArrayAccessExpression"/> (if possible).
    /// </summary>
    /// <param name="expression">The expression being indexed.</param>
    /// <param name="index">The index.</param>
    /// <returns>The constant item at the index, if constant.</returns>
    internal static ConstantValue FoldIndex(BoundExpression expression, BoundExpression index) {
        if (expression.constantValue is null || index.constantValue is null)
            return null;

        var array = (ImmutableArray<ConstantValue>)expression.constantValue.value;
        var item = array[(int)index.constantValue.value];

        return new ConstantValue(item.value);
    }
}
