using System.Collections.Generic;
using static Buckle.Libraries.LibraryHelpers;

namespace Buckle.CodeAnalysis.Symbols;

/// <summary>
/// All builtin methods used by the compiler with no Standard implementation.
/// </summary>
internal static class BuiltinMethods {
    /// <summary>
    /// RandInt method, gets a random integer with a maximum (minimum is always 0).
    /// </summary>
    internal static readonly MethodSymbol RandInt = Method(
        "RandInt",
        SpecialType.Int,
        [("max", SpecialType.Int)]
    );

    /// <summary>
    /// Converts an integer into a base 16 representation.
    /// Optionally adds the '0x' prefix.
    /// </summary>
    internal static readonly MethodSymbol Hex = Method(
        "Hex",
        SpecialType.String,
        [("value", SpecialType.Int, null), ("prefix", SpecialType.Bool, true)]
    );

    /// <summary>
    /// Converts an integer into a base 16 representation.
    /// Optionally adds the '0x' prefix.
    /// </summary>
    internal static readonly MethodSymbol NullableHex = Method(
        "Hex",
        SpecialType.String,
        true,
        [("value", SpecialType.Int, true, null), ("prefix", SpecialType.Bool, false, false)]
    );

    /// <summary>
    /// Converts a string of length 1 to the appropriate ASCII code of the character.
    /// </summary>
    internal static readonly MethodSymbol Ascii = Method(
        "Ascii",
        SpecialType.Int,
        [("char", SpecialType.String)]
    );

    /// <summary>
    /// Converts a string of length 1 to the appropriate ASCII code of the character.
    /// </summary>
    internal static readonly MethodSymbol NullableAscii = Method(
        "Ascii",
        SpecialType.Int,
        true,
        [("char", SpecialType.String, true)]
    );

    /// <summary>
    /// Converts an integer to the appropriate character using ASCII codes.
    /// Opposite of <see cref="Ascii">.
    /// </summary>
    internal static readonly MethodSymbol Char = Method(
        "Char",
        SpecialType.String,
        [("ascii", SpecialType.Int)]
    );

    /// <summary>
    /// Converts an integer to the appropriate character using ASCII codes.
    /// Opposite of <see cref="Ascii">.
    /// </summary>
    internal static readonly MethodSymbol NullableChar = Method(
        "Char",
        SpecialType.String,
        true,
        [("ascii", SpecialType.Int, true)]
    );

    /// <summary>
    /// Gets the length of the given array. If given a non-array, returns null.
    /// </summary>
    internal static readonly MethodSymbol LengthNull = Method(
        "Length",
        SpecialType.Int,
        true,
        [("array", SpecialType.Any, true)]
    );

    /// <summary>
    /// Gets the length of the given array.
    /// </summary>
    internal static readonly MethodSymbol Length = Method(
        "Length",
        SpecialType.Int,
        [("array", SpecialType.Any)]
    );

    /// <summary>
    /// LowLevel only.
    /// Converts a truly generic type into a generic primitive.
    /// </summary>
    internal static readonly MethodSymbol ToAny = Method(
        "ToAny",
        SpecialType.Any,
        true,
        [("primitive", SpecialType.Any, true)]
    );

    /// <summary>
    /// LowLevel only.
    /// Converts a truly generic type into a generic object.
    /// </summary>
    internal static readonly MethodSymbol ToObject = Method(
        "ToObject",
        SpecialType.Any,
        true,
        [("object", SpecialType.Any, true)]
    );

    /// <summary>
    /// Gets the hash of a primitive or object.
    /// </summary>
    internal static readonly new MethodSymbol GetHashCode = Method(
        "GetHashCode",
        SpecialType.Int,
        [("value", SpecialType.Any, true)]
    );

    /// <summary>
    /// Gets all public builtin methods.
    /// </summary>
    /// <returns>All public builtins, calling code should not depend on order.</returns>
    internal static IEnumerable<MethodSymbol> GetAll()
        => [
            RandInt,
            Hex,
            NullableHex,
            Ascii,
            NullableAscii,
            Char,
            NullableChar,
            LengthNull,
            Length,
            ToAny,
            ToObject,
            GetHashCode
        ];
}
