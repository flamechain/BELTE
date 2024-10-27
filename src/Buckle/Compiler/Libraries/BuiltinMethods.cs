using System.Collections.Generic;
using System.Linq;
using Buckle.Libraries;
using Microsoft.CodeAnalysis.PooledObjects;

namespace Buckle.CodeAnalysis.Symbols;

/// <summary>
/// All builtin methods used by the compiler with no Standard implementation.
/// </summary>
internal static class BuiltinMethods {
    /// <summary>
    /// RandInt method, gets a random integer with a maximum (minimum is always 0).
    /// </summary>
    internal static readonly MethodSymbol RandInt = CreateBuiltinMethod(
        "RandInt",
        [("max", SpecialType.Int)],
        SpecialType.Int
    );

    /// <summary>
    /// Value method, gets non nullable value from nullable item (throws if item is null).
    /// Any type overload.
    /// </summary>
    internal static readonly MethodSymbol ValueAny = CreateBuiltinMethod(
        "Value",
        [("value", SpecialType.Any, true)],
        SpecialType.Any
    );

    /// <summary>
    /// Value method, gets non nullable value from nullable item (throws if item is null).
    /// Bool type overload.
    /// </summary>
    internal static readonly MethodSymbol ValueBool = CreateBuiltinMethod(
        "Value",
        [("value", SpecialType.Bool, true)],
        SpecialType.Bool
    );

    /// <summary>
    /// Value method, gets non nullable value from nullable item (throws if item is null).
    /// Decimal type overload.
    /// </summary>
    internal static readonly MethodSymbol ValueDecimal = CreateBuiltinMethod(
        "Value",
        [("value", SpecialType.Decimal, true)],
        SpecialType.Decimal
    );

    /// <summary>
    /// Value method, gets non nullable value from nullable item (throws if item is null).
    /// Integer type overload.
    /// </summary>
    internal static readonly MethodSymbol ValueInt = CreateBuiltinMethod(
        "Value",
        [("value", SpecialType.Int, true)],
        SpecialType.Int
    );

    /// <summary>
    /// Value method, gets non nullable value from nullable item (throws if item is null).
    /// String type overload.
    /// </summary>
    internal static readonly MethodSymbol ValueString = CreateBuiltinMethod(
        "Value",
        [("value", SpecialType.String, true)],
        SpecialType.String
    );

    /// <summary>
    /// Value method, gets non nullable value from nullable item (throws if item is null).
    /// Char type overload.
    /// </summary>
    internal static readonly MethodSymbol ValueChar = CreateBuiltinMethod(
        "Value",
        [("value", SpecialType.Char, true)],
        SpecialType.Char
    );

    /// <summary>
    /// Checks if nullable item has a value (otherwise it is null).
    /// Any type overload.
    /// </summary>
    internal static readonly MethodSymbol HasValueAny = CreateBuiltinMethod(
        "HasValue",
        [("value", SpecialType.Any, true)],
        SpecialType.Bool
    );

    /// <summary>
    /// Checks if nullable item has a value (otherwise it is null).
    /// Bool type overload.
    /// </summary>
    internal static readonly MethodSymbol HasValueBool = CreateBuiltinMethod(
        "HasValue",
        [("value", SpecialType.Bool, true)],
        SpecialType.Bool
    );

    /// <summary>
    /// Checks if nullable item has a value (otherwise it is null).
    /// Decimal type overload.
    /// </summary>
    internal static readonly MethodSymbol HasValueDecimal = CreateBuiltinMethod(
        "HasValue",
        [("value", SpecialType.Decimal, true)],
        SpecialType.Bool
    );

    /// <summary>
    /// Checks if nullable item has a value (otherwise it is null).
    /// Int type overload.
    /// </summary>
    internal static readonly MethodSymbol HasValueInt = CreateBuiltinMethod(
        "HasValue",
        [("value", SpecialType.Int, true)],
        SpecialType.Bool
    );

    /// <summary>
    /// Checks if nullable item has a value (otherwise it is null).
    /// String type overload.
    /// </summary>
    internal static readonly MethodSymbol HasValueString = CreateBuiltinMethod(
        "HasValue",
        [("value", SpecialType.String, true)],
        SpecialType.Bool
    );

    /// <summary>
    /// Checks if nullable item has a value (otherwise it is null).
    /// Char type overload.
    /// </summary>
    internal static readonly MethodSymbol HasValueChar = CreateBuiltinMethod(
        "HasValue",
        [("value", SpecialType.Char, true)],
        SpecialType.Bool
    );

    /// <summary>
    /// Converts an integer into a base 16 representation.
    /// Optionally adds the '0x' prefix.
    /// </summary>
    internal static readonly MethodSymbol Hex = CreateBuiltinMethod(
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
    internal static readonly MethodSymbol NullableHex = CreateBuiltinMethod(
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
    internal static readonly MethodSymbol Ascii = CreateBuiltinMethod(
        "Ascii",
        [("char", SpecialType.String)],
        SpecialType.Int
    );

    /// <summary>
    /// Converts a string of length 1 to the appropriate ASCII code of the character.
    /// </summary>
    internal static readonly MethodSymbol NullableAscii = CreateBuiltinMethod(
        "Ascii",
        [("char", SpecialType.String, true)],
        SpecialType.Int,
        true
    );

    /// <summary>
    /// Converts an integer to the appropriate character using ASCII codes.
    /// Opposite of <see cref="Ascii">.
    /// </summary>
    internal static readonly MethodSymbol Char = CreateBuiltinMethod(
        "Char",
        [("ascii", SpecialType.Int)],
        SpecialType.String
    );

    /// <summary>
    /// Converts an integer to the appropriate character using ASCII codes.
    /// Opposite of <see cref="Ascii">.
    /// </summary>
    internal static readonly MethodSymbol NullableChar = CreateBuiltinMethod(
        "Char",
        [("ascii", SpecialType.Int, true)],
        SpecialType.String,
        true
    );

    /// <summary>
    /// Gets the length of the given array. If given a non-array, returns null.
    /// </summary>
    internal static readonly MethodSymbol LengthNull = CreateBuiltinMethod(
        "Length",
        [("array", SpecialType.Any, true)],
        SpecialType.Int,
        true
    );

    /// <summary>
    /// Gets the length of the given array.
    /// </summary>
    internal static readonly MethodSymbol Length = CreateBuiltinMethod(
        "Length",
        [("array", SpecialType.Any)],
        SpecialType.Int
    );

    /// <summary>
    /// LowLevel only.
    /// Converts a truly generic type into a generic primitive.
    /// </summary>
    internal static readonly MethodSymbol ToAny = CreateBuiltinMethod(
        "ToAny",
        [("primitive", SpecialType.Any, true)],
        SpecialType.Any,
        true
    );

    /// <summary>
    /// LowLevel only.
    /// Converts a truly generic type into a generic object.
    /// </summary>
    internal static readonly MethodSymbol ToObject = CreateBuiltinMethod(
        "ToObject",
        [("object", SpecialType.Any, true)],
        SpecialType.Any,
        true
    );

    /// <summary>
    /// LowLevel only.
    /// Checks if two objects values equal.
    /// </summary>
    internal static readonly MethodSymbol ObjectsEqual = CreateBuiltinMethod(
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
    internal static readonly MethodSymbol ObjectReferencesEqual = CreateBuiltinMethod(
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
    internal static readonly new MethodSymbol GetHashCode = CreateBuiltinMethod(
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
            ObjectsEqual,
            ObjectReferencesEqual,
            GetHashCode
        ];


    private static SynthesizedGlobalMethodSymbol CreateBuiltinMethod(
        string name,
        IEnumerable<(string name, SpecialType type, bool isNullable, object defaultValue)> parameters,
        SpecialType type,
        bool isNullable) {
        return CreateBuiltinMethod(
            name,
            parameters.Select<(string name, SpecialType type, bool isNullable, object defaultValue),
                              (string, SpecialType, bool, object, RefKind)>(
                p => (p.name, p.type, p.isNullable, p.defaultValue, RefKind.None)
            ),
            type,
            isNullable
        );
    }

    private static SynthesizedGlobalMethodSymbol CreateBuiltinMethod(
        string name,
        IEnumerable<(string name, SpecialType type, object defaultValue)> parameters,
        SpecialType type) {
        return CreateBuiltinMethod(
            name,
            parameters.Select<(string name, SpecialType type, object defaultValue),
                              (string, SpecialType, bool, object, RefKind)>(
                p => (p.name, p.type, false, p.defaultValue, RefKind.None)
            ),
            type,
            false
        );
    }

    private static SynthesizedGlobalMethodSymbol CreateBuiltinMethod(
        string name,
        IEnumerable<(string name, SpecialType type, bool isNullable)> parameters,
        SpecialType type) {
        return CreateBuiltinMethod(
            name,
            parameters.Select<(string name, SpecialType type, bool isNullable),
                              (string, SpecialType, bool, object, RefKind)>(
                p => (p.name, p.type, p.isNullable, null, RefKind.None)
            ),
            type,
            false
        );
    }

    private static SynthesizedGlobalMethodSymbol CreateBuiltinMethod(
        string name,
        IEnumerable<(string name, SpecialType type, bool isNullable)> parameters,
        SpecialType type,
        bool isNullable) {
        return CreateBuiltinMethod(
            name,
            parameters.Select<(string name, SpecialType type, bool isNullable),
                              (string, SpecialType, bool, object, RefKind)>(
                p => (p.name, p.type, p.isNullable, null, RefKind.None)
            ),
            type,
            isNullable
        );
    }

    private static SynthesizedGlobalMethodSymbol CreateBuiltinMethod(
        string name,
        IEnumerable<(string name, SpecialType type)> parameters,
        SpecialType type) {
        return CreateBuiltinMethod(
            name,
            parameters.Select<(string name, SpecialType type), (string, SpecialType, bool, object, RefKind)>(
                p => (p.name, p.type, false, null, RefKind.None)
            ),
            type,
            false
        );
    }

    private static SynthesizedGlobalMethodSymbol CreateBuiltinMethod(
        string name,
        IEnumerable<(string name, SpecialType type, bool isNullable, object defaultValue, RefKind refKind)> parameters,
        SpecialType type,
        bool isNullable) {
        var builder = ArrayBuilder<ParameterSymbol>.GetInstance();
        var i = 0;

        foreach (var parameter in parameters) {
            var parameterType = CorLibrary.GetSpecialType(parameter.type);
            var parameterTypeWithAnnotations = new TypeWithAnnotations(parameterType, parameter.isNullable);
            var constantValue = parameter.defaultValue is null ? null : new ConstantValue(parameter.defaultValue);

            var synthesizedParameter = SynthesizedParameterSymbol.Create(
                null,
                parameterTypeWithAnnotations,
                i,
                parameter.refKind,
                parameter.name,
                defaultValue: constantValue
            );

            i++;
        }

        var returnType = CorLibrary.GetSpecialType(type);
        var returnTypeWithAnnotations = new TypeWithAnnotations(returnType, isNullable);

        return new SynthesizedGlobalMethodSymbol(name, builder.ToImmutableAndFree(), returnTypeWithAnnotations);
    }
}
