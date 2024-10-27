using System;
using System.Collections.Concurrent;
using System.Diagnostics;
using System.Threading;
using Buckle.CodeAnalysis.Symbols;

namespace Buckle.Libraries;

internal sealed class CorLibrary {
    private static readonly CorLibrary Instance = new CorLibrary();

    private const int TotalSpecialTypes = 12;

    private readonly ConcurrentDictionary<SpecialType, NamedTypeSymbol> _specialTypes = [];

    private int _registeredSpecialTypes;

    private CorLibrary() {
        RegisterCorTypes();
    }

    internal static NamedTypeSymbol GetSpecialType(SpecialType specialType) {
        return Instance.GetSpecialTypeCore(specialType);
    }

    internal static void RegisterDeclaredSpecialType(NamedTypeSymbol type) {
        Instance.RegisterSpecialType(type);
    }

    internal static bool StillLookingForSpecialTypes() {
        return Instance._registeredSpecialTypes < TotalSpecialTypes;
    }

    private NamedTypeSymbol GetSpecialTypeCore(SpecialType specialType) {
        if (specialType is SpecialType.None or SpecialType.Nullable)
            throw new ArgumentException($"Cannot retrieve special type {specialType}");

        if (!_specialTypes.TryGetValue(specialType, out var result))
            throw new ArgumentException($"Special type {specialType} has not been registered");

        return result;
    }

    private void RegisterSpecialType(NamedTypeSymbol type) {
        var specialType = type.specialType;

        if (specialType == SpecialType.None)
            throw new ArgumentException($"Cannot register type {type} because it is not a special type");

        if (specialType == SpecialType.Nullable)
            throw new ArgumentException($"Cannot register special type {specialType}");

        if (!_specialTypes.TryAdd(specialType, type))
            throw new ArgumentException($"Special type {specialType} was already registered");

        Interlocked.Increment(ref _registeredSpecialTypes);

        if (_registeredSpecialTypes > TotalSpecialTypes)
            throw new UnreachableException($"Registered more special types than there are special types");
    }

    private void RegisterCorTypes() {
        RegisterSpecialType(new PrimitiveTypeSymbol("any", SpecialType.Any));
        RegisterSpecialType(new PrimitiveTypeSymbol("int", SpecialType.Int));
        RegisterSpecialType(new PrimitiveTypeSymbol("bool", SpecialType.Bool));
        RegisterSpecialType(new PrimitiveTypeSymbol("char", SpecialType.Char));
        RegisterSpecialType(new PrimitiveTypeSymbol("string", SpecialType.String));
        RegisterSpecialType(new PrimitiveTypeSymbol("decimal", SpecialType.Decimal));
        RegisterSpecialType(new PrimitiveTypeSymbol("type", SpecialType.Type));
        RegisterSpecialType(new PrimitiveTypeSymbol("void", SpecialType.Void));
        RegisterSpecialType(new PrimitiveTypeSymbol("Array", SpecialType.Array));
    }
}
