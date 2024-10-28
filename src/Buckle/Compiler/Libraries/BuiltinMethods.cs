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
        [("max", SpecialType.Int)],
        SpecialType.Int
    );

    /// <summary>
    /// Converts an integer into a base 16 representation.
    /// Optionally adds the '0x' prefix.
    /// </summary>
    internal static readonly MethodSymbol Hex = Method(
        "Hex",
        [
            ("value", SpecialType.Int, null),
            ("prefix", SpecialType.Bool, true),
        ],
        SpecialType.String
    );

    /// <summary>
    /// Converts an integer into a base 16 representation.
    /// Optionally adds the '0x' prefix.
    /// </summary>
    internal static readonly MethodSymbol NullableHex = Method(
        "Hex",
        [
            ("value", SpecialType.Int, true, null),
            ("prefix", SpecialType.Bool, false, false),
        ],
        SpecialType.String,
        true
    );

    /// <summary>
    /// Converts a string of length 1 to the appropriate ASCII code of the character.
    /// </summary>
    internal static readonly MethodSymbol Ascii = Method(
        "Ascii",
        [("char", SpecialType.String)],
        SpecialType.Int
    );

    /// <summary>
    /// Converts a string of length 1 to the appropriate ASCII code of the character.
    /// </summary>
    internal static readonly MethodSymbol NullableAscii = Method(
        "Ascii",
        [("char", SpecialType.String, true)],
        SpecialType.Int,
        true
    );

    /// <summary>
    /// Converts an integer to the appropriate character using ASCII codes.
    /// Opposite of <see cref="Ascii">.
    /// </summary>
    internal static readonly MethodSymbol Char = Method(
        "Char",
        [("ascii", SpecialType.Int)],
        SpecialType.String
    );

    /// <summary>
    /// Converts an integer to the appropriate character using ASCII codes.
    /// Opposite of <see cref="Ascii">.
    /// </summary>
    internal static readonly MethodSymbol NullableChar = Method(
        "Char",
        [("ascii", SpecialType.Int, true)],
        SpecialType.String,
        true
    );

    /// <summary>
    /// Gets the length of the given array. If given a non-array, returns null.
    /// </summary>
    internal static readonly MethodSymbol LengthNull = Method(
        "Length",
        [("array", SpecialType.Any, true)],
        SpecialType.Int,
        true
    );

    /// <summary>
    /// Gets the length of the given array.
    /// </summary>
    internal static readonly MethodSymbol Length = Method(
        "Length",
        [("array", SpecialType.Any)],
        SpecialType.Int
    );

    /// <summary>
    /// LowLevel only.
    /// Converts a truly generic type into a generic primitive.
    /// </summary>
    internal static readonly MethodSymbol ToAny = Method(
        "ToAny",
        [("primitive", SpecialType.Any, true)],
        SpecialType.Any,
        true
    );

    /// <summary>
    /// LowLevel only.
    /// Converts a truly generic type into a generic object.
    /// </summary>
    internal static readonly MethodSymbol ToObject = Method(
        "ToObject",
        [("object", SpecialType.Any, true)],
        SpecialType.Any,
        true
    );

    /// <summary>
    /// LowLevel only.
    /// Checks if two objects values equal.
    /// </summary>
    internal static readonly MethodSymbol ObjectsEqual = Method(
        "ObjectsEqual",
        [
            ("x", SpecialType.Object, true),
            ("y", SpecialType.Object, true)
        ],
        SpecialType.Bool,
        true
    );

    /// <summary>
    /// LowLevel only.
    /// Checks if two references refer to the same object.
    /// </summary>
    internal static readonly MethodSymbol ObjectReferencesEqual = Method(
        "ObjectReferencesEqual",
        [
            ("x", SpecialType.Object, true, null, RefKind.Ref),
            ("y", SpecialType.Object, true, null, RefKind.Ref)
        ],
        SpecialType.Bool,
        true
    );

    /// <summary>
    /// Gets the hash of a primitive or object.
    /// </summary>
    internal static readonly new MethodSymbol GetHashCode = Method(
        "GetHashCode",
        [("value", SpecialType.Any, true)],
        SpecialType.Int
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
