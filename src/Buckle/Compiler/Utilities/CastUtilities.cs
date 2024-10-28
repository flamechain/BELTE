using System;
using Buckle.CodeAnalysis.Binding;
using Buckle.CodeAnalysis.Symbols;

namespace Buckle.Utilities;

// TODO This needs to be rewritten

/// <summary>
/// Utilities helping casting values, and utilities related to the <see cref="Convert" /> class.
/// </summary>
internal static class CastUtilities {
    /// <summary>
    /// Casts a value to another type based on the given target type.
    /// </summary>
    /// <param name="value">What to cast.</param>
    /// <param name="type">The target type of the value.</param>
    /// <returns>The casted value, does not wrap conversion exceptions.</returns>
    internal static object Cast(object value, BoundType targetType) {
        if (value is null && !targetType.isNullable)
            throw new NullReferenceException();

        if (value is null)
            return null;

        var typeSymbol = targetType?.typeSymbol;

        if (typeSymbol == TypeSymbol.Bool) {
            return Convert.ToBoolean(value);
        } else if (typeSymbol == TypeSymbol.Int) {
            // Prevents bankers rounding from Convert.ToInt32, instead always truncate (no rounding)
            if (value.IsFloatingPoint())
                value = Math.Truncate(Convert.ToDouble(value));

            return Convert.ToInt32(value);
        } else if (typeSymbol == TypeSymbol.Decimal) {
            return Convert.ToDouble(value);
        } else if (typeSymbol == TypeSymbol.String) {
            return Convert.ToString(value);
        } else if (typeSymbol == TypeSymbol.Char) {
            return Convert.ToChar(value);
        } else {
            return value;
        }
    }

    internal static object CastIgnoringNull(object value, BoundType targetType) {
        var typeSymbol = targetType?.typeSymbol;

        if (typeSymbol == TypeSymbol.Bool) {
            return Convert.ToBoolean(value);
        } else if (typeSymbol == TypeSymbol.Int) {
            if (value.IsFloatingPoint())
                value = Math.Truncate(Convert.ToDouble(value));

            return Convert.ToInt32(value);
        } else if (typeSymbol == TypeSymbol.Decimal) {
            return Convert.ToDouble(value);
        } else if (typeSymbol == TypeSymbol.String) {
            return Convert.ToString(value);
        } else if (typeSymbol == TypeSymbol.Char) {
            return Convert.ToChar(value);
        } else {
            return value;
        }
    }
}
