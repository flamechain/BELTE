using System;
using System.Collections.Concurrent;
using System.Diagnostics;
using System.Threading;
using Buckle.CodeAnalysis.Symbols;

namespace Buckle.Libraries;

internal sealed class CorLibrary {
    private static readonly CorLibrary Instance = new CorLibrary();

    private const int TotalSpecialTypes = 13;

    private readonly ConcurrentDictionary<SpecialType, NamedTypeSymbol> _specialTypes = [];

    private int _registeredSpecialTypes;
    private bool _complete = false;

    private CorLibrary() {
        RegisterPrimitiveCorTypes();
    }

    internal static NamedTypeSymbol GetSpecialType(SpecialType specialType) {
        Instance.EnsureCorLibraryIsComplete();
        return Instance.GetSpecialTypeCore(specialType);
    }

    internal static void RegisterDeclaredSpecialType(NamedTypeSymbol type) {
        Instance.EnsureCorLibraryIsComplete();
        Instance.RegisterSpecialType(type);
    }

    internal static bool StillLookingForSpecialTypes() {
        Instance.EnsureCorLibraryIsComplete();
        return Instance._registeredSpecialTypes < TotalSpecialTypes;
    }

    private void EnsureCorLibraryIsComplete() {
        if (!_complete) {
            _complete = true;
            RegisterNonPrimitiveCorTypes();
        }
    }

    private NamedTypeSymbol GetSpecialTypeCore(SpecialType specialType) {
        if (!_specialTypes.TryGetValue(specialType, out var result))
            throw new ArgumentException($"Special type {specialType} has not been registered");

        return result;
    }

    private void RegisterSpecialType(NamedTypeSymbol type) {
        var specialType = type.specialType;

        if (specialType == SpecialType.None)
            throw new ArgumentException($"Cannot register type {type} because it is not a special type");

        if (!_specialTypes.TryAdd(specialType, type))
            throw new ArgumentException($"Special type {specialType} was already registered");

        Interlocked.Increment(ref _registeredSpecialTypes);

        if (_registeredSpecialTypes > TotalSpecialTypes)
            throw new UnreachableException($"Registered more special types than there are special types");
    }

    private void RegisterPrimitiveCorTypes() {
        RegisterSpecialType(new PrimitiveTypeSymbol("any", SpecialType.Any));
        RegisterSpecialType(new PrimitiveTypeSymbol("int", SpecialType.Int));
        RegisterSpecialType(new PrimitiveTypeSymbol("bool", SpecialType.Bool));
        RegisterSpecialType(new PrimitiveTypeSymbol("char", SpecialType.Char));
        RegisterSpecialType(new PrimitiveTypeSymbol("string", SpecialType.String));
        RegisterSpecialType(new PrimitiveTypeSymbol("decimal", SpecialType.Decimal));
        RegisterSpecialType(new PrimitiveTypeSymbol("type", SpecialType.Type));
        RegisterSpecialType(new PrimitiveTypeSymbol("void", SpecialType.Void));
    }

    private void RegisterNonPrimitiveCorTypes() {
        RegisterSpecialType(new PrimitiveTypeSymbol("Array", SpecialType.Array));
        RegisterSpecialType(new PrimitiveTypeSymbol("Nullable", SpecialType.Nullable, 1));
    }
}
