using System;
using System.Collections.Concurrent;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Threading;
using Buckle.CodeAnalysis.Binding;
using Buckle.CodeAnalysis.Symbols;
using Microsoft.CodeAnalysis.PooledObjects;

namespace Buckle.Libraries;

internal sealed class CorLibrary {
    private static readonly CorLibrary Instance = new CorLibrary();

    private const int TotalSpecialTypes = 13 - 2; // TODO remove -2 after adding List and Dict

    private readonly ConcurrentDictionary<SpecialType, NamedTypeSymbol> _specialTypes = [];

    private ImmutableArray<UnaryOperatorSignature>[] _builtInUnaryOperators;
    private ImmutableArray<BinaryOperatorSignature>[][] _builtInBinaryOperators;
    private int _registeredSpecialTypes;
    private bool _complete = false;

    private CorLibrary() {
        RegisterPrimitiveCorTypes();
    }

    #region Public Model

    internal static NamedTypeSymbol GetSpecialType(SpecialType specialType) {
        Instance.EnsureCorLibraryIsComplete();
        return Instance.GetSpecialTypeCore(specialType);
    }

    internal static NamedTypeSymbol GetNullableType(SpecialType specialType) {
        Instance.EnsureCorLibraryIsComplete();
        return Instance.GetNullableTypeCore(specialType);
    }

    internal static void RegisterDeclaredSpecialType(NamedTypeSymbol type) {
        Instance.EnsureCorLibraryIsComplete();
        Instance.RegisterSpecialType(type);
    }

    internal static bool StillLookingForSpecialTypes() {
        Instance.EnsureCorLibraryIsComplete();
        return Instance._registeredSpecialTypes < TotalSpecialTypes;
    }

    internal static void GetAllBuiltInBinaryOperators(
        BinaryOperatorKind kind,
        ArrayBuilder<BinaryOperatorSignature> operators) {
        Instance.EnsureCorLibraryIsComplete();
        Instance.EnsureBuiltInBinaryOperators();
        Instance.GetBinaryOpertors(kind, operators);
    }

    internal static void GetAllBuiltInUnaryOperators(
        BinaryOperatorKind kind,
        ArrayBuilder<BinaryOperatorSignature> operators) {
        Instance.EnsureCorLibraryIsComplete();
        Instance.EnsureBuiltInUnaryOperators();
        Instance.GetUnaryOpertors(kind, operators);
    }

    #endregion

    #region Types

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

    private NamedTypeSymbol GetNullableTypeCore(SpecialType specialType) {
        return GetSpecialTypeCore(SpecialType.Nullable)
            .Construct([new TypeOrConstant(GetSpecialTypeCore(specialType), false)]);
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

    #endregion

    #region Operators

    private void EnsureBuiltInSimpleOperators() {
        if (_builtInUnaryOperators is null) {
            var allOperators = new ImmutableArray<UnaryOperatorSignature>[] {
                    GetSignaturesFromUnaryOperatorKinds(new []
                    {
                        (int)UnaryOperatorKind.SBytePostfixIncrement,
                        (int)UnaryOperatorKind.BytePostfixIncrement,
                        (int)UnaryOperatorKind.ShortPostfixIncrement,
                        (int)UnaryOperatorKind.UShortPostfixIncrement,
                        (int)UnaryOperatorKind.IntPostfixIncrement,
                        (int)UnaryOperatorKind.UIntPostfixIncrement,
                        (int)UnaryOperatorKind.LongPostfixIncrement,
                        (int)UnaryOperatorKind.ULongPostfixIncrement,
                        (int)UnaryOperatorKind.NIntPostfixIncrement,
                        (int)UnaryOperatorKind.NUIntPostfixIncrement,
                        (int)UnaryOperatorKind.CharPostfixIncrement,
                        (int)UnaryOperatorKind.FloatPostfixIncrement,
                        (int)UnaryOperatorKind.DoublePostfixIncrement,
                        (int)UnaryOperatorKind.DecimalPostfixIncrement,
                        (int)UnaryOperatorKind.LiftedSBytePostfixIncrement,
                        (int)UnaryOperatorKind.LiftedBytePostfixIncrement,
                        (int)UnaryOperatorKind.LiftedShortPostfixIncrement,
                        (int)UnaryOperatorKind.LiftedUShortPostfixIncrement,
                        (int)UnaryOperatorKind.LiftedIntPostfixIncrement,
                        (int)UnaryOperatorKind.LiftedUIntPostfixIncrement,
                        (int)UnaryOperatorKind.LiftedLongPostfixIncrement,
                        (int)UnaryOperatorKind.LiftedULongPostfixIncrement,
                        (int)UnaryOperatorKind.LiftedNIntPostfixIncrement,
                        (int)UnaryOperatorKind.LiftedNUIntPostfixIncrement,
                        (int)UnaryOperatorKind.LiftedCharPostfixIncrement,
                        (int)UnaryOperatorKind.LiftedFloatPostfixIncrement,
                        (int)UnaryOperatorKind.LiftedDoublePostfixIncrement,
                        (int)UnaryOperatorKind.LiftedDecimalPostfixIncrement,
                    }),
            };
        }
    }

    private void EnsureBuiltInBinaryOperators() {
        if (_builtInBinaryOperators is null) {
            var conditionalOperators = new ImmutableArray<BinaryOperatorSignature>[] {
                [], //multiplication
                [], //addition
                [], //subtraction
                [], //division
                [], //modulo
                [], //left shift
                [], //right shift
                [], //equal
                [], //not equal
                [], //greater than
                [], //less than
                [], //greater than or equal
                [], //less than or equal
                [], //unsigned right shift
                [GetSignature(BinaryOperatorKind.BoolConditionalAnd)], //and
                [GetSignature(BinaryOperatorKind.BoolConditionalOr)], //or
                [], //xor
                [], //power
            };

            var nonConditionalOperators = new ImmutableArray<BinaryOperatorSignature>[] {
                GetSignaturesFromBinaryOperatorKinds([
                    (int)BinaryOperatorKind.IntMultiplication,
                    (int)BinaryOperatorKind.DecimalMultiplication,
                ]),
                GetSignaturesFromBinaryOperatorKinds([
                    (int)BinaryOperatorKind.IntAddition,
                    (int)BinaryOperatorKind.DecimalAddition,
                    (int)BinaryOperatorKind.StringConcatenation,
                ]),
                GetSignaturesFromBinaryOperatorKinds([
                    (int)BinaryOperatorKind.IntSubtraction,
                    (int)BinaryOperatorKind.DecimalSubtraction,
                ]),
                GetSignaturesFromBinaryOperatorKinds([
                    (int)BinaryOperatorKind.IntDivision,
                    (int)BinaryOperatorKind.DecimalDivision,
                ]),
                GetSignaturesFromBinaryOperatorKinds([
                    (int)BinaryOperatorKind.IntModulo,
                    (int)BinaryOperatorKind.DecimalModulo,
                ]),
                GetSignaturesFromBinaryOperatorKinds([
                    (int)BinaryOperatorKind.IntLeftShift,
                ]),
                GetSignaturesFromBinaryOperatorKinds([
                    (int)BinaryOperatorKind.IntRightShift,
                ]),
                GetSignaturesFromBinaryOperatorKinds([
                    (int)BinaryOperatorKind.IntEqual,
                    (int)BinaryOperatorKind.DecimalEqual,
                    (int)BinaryOperatorKind.BoolEqual,
                    (int)BinaryOperatorKind.ObjectEqual,
                    (int)BinaryOperatorKind.StringEqual,
                    (int)BinaryOperatorKind.CharEqual,
                    (int)BinaryOperatorKind.TypeEqual,
                ]),
                GetSignaturesFromBinaryOperatorKinds([
                    (int)BinaryOperatorKind.IntNotEqual,
                    (int)BinaryOperatorKind.DecimalNotEqual,
                    (int)BinaryOperatorKind.BoolNotEqual,
                    (int)BinaryOperatorKind.ObjectNotEqual,
                    (int)BinaryOperatorKind.StringNotEqual,
                    (int)BinaryOperatorKind.CharNotEqual,
                    (int)BinaryOperatorKind.TypeNotEqual,
                ]),
                GetSignaturesFromBinaryOperatorKinds([
                    (int)BinaryOperatorKind.IntGreaterThan,
                    (int)BinaryOperatorKind.DecimalGreaterThan,
                ]),
                GetSignaturesFromBinaryOperatorKinds([
                    (int)BinaryOperatorKind.IntLessThan,
                    (int)BinaryOperatorKind.DecimalLessThan,
                ]),
                GetSignaturesFromBinaryOperatorKinds([
                    (int)BinaryOperatorKind.IntGreaterThanOrEqual,
                    (int)BinaryOperatorKind.DecimalGreaterThanOrEqual,
                ]),
                GetSignaturesFromBinaryOperatorKinds([
                    (int)BinaryOperatorKind.IntLessThanOrEqual,
                    (int)BinaryOperatorKind.DecimalLessThanOrEqual,
                ]),
                GetSignaturesFromBinaryOperatorKinds([
                    (int)BinaryOperatorKind.IntUnsignedRightShift,
                ]),
                GetSignaturesFromBinaryOperatorKinds([
                    (int)BinaryOperatorKind.IntAnd,
                    (int)BinaryOperatorKind.BoolAnd,
                ]),
                GetSignaturesFromBinaryOperatorKinds([
                    (int)BinaryOperatorKind.IntOr,
                    (int)BinaryOperatorKind.BoolOr,
                ]),
                GetSignaturesFromBinaryOperatorKinds([
                    (int)BinaryOperatorKind.IntXor,
                    (int)BinaryOperatorKind.BoolXor,
                ]),
                GetSignaturesFromBinaryOperatorKinds([
                    (int)BinaryOperatorKind.IntPower,
                    (int)BinaryOperatorKind.DecimalPower,
                ]),
            };

            var allOperators = new[] { nonConditionalOperators, conditionalOperators };
            Interlocked.CompareExchange(ref _builtInBinaryOperators, allOperators, null);
        }
    }

    private BinaryOperatorSignature GetSignature(BinaryOperatorKind kind) {
        var left = TypeFromKind(kind);

        switch (kind.Operator()) {
            case BinaryOperatorKind.Multiplication:
            case BinaryOperatorKind.Division:
            case BinaryOperatorKind.Subtraction:
            case BinaryOperatorKind.Modulo:
            case BinaryOperatorKind.And:
            case BinaryOperatorKind.Or:
            case BinaryOperatorKind.Xor:
                return new BinaryOperatorSignature(kind, left, left, left);
            case BinaryOperatorKind.Addition:
                return new BinaryOperatorSignature(kind, left, TypeFromKind(kind), TypeFromKind(kind));
            case BinaryOperatorKind.LeftShift:
            case BinaryOperatorKind.RightShift:
            case BinaryOperatorKind.UnsignedRightShift:
                var rightType = GetNullableTypeCore(SpecialType.Int);
                return new BinaryOperatorSignature(kind, left, rightType, left);
            case BinaryOperatorKind.Equal:
            case BinaryOperatorKind.NotEqual:
            case BinaryOperatorKind.GreaterThan:
            case BinaryOperatorKind.LessThan:
            case BinaryOperatorKind.GreaterThanOrEqual:
            case BinaryOperatorKind.LessThanOrEqual:
                return new BinaryOperatorSignature(kind, left, left, GetNullableTypeCore(SpecialType.Bool));
        }

        return new BinaryOperatorSignature(kind, left, TypeFromKind(kind), TypeFromKind(kind));
    }

    internal UnaryOperatorSignature GetSignature(UnaryOperatorKind kind) {
        TypeSymbol opType;
        switch (kind.OperandTypes()) {
            case UnaryOperatorKind.SByte: opType = _compilation.GetSpecialType(SpecialType.System_SByte); break;
            case UnaryOperatorKind.Byte: opType = _compilation.GetSpecialType(SpecialType.System_Byte); break;
            case UnaryOperatorKind.Short: opType = _compilation.GetSpecialType(SpecialType.System_Int16); break;
            case UnaryOperatorKind.UShort: opType = _compilation.GetSpecialType(SpecialType.System_UInt16); break;
            case UnaryOperatorKind.Int: opType = _compilation.GetSpecialType(SpecialType.System_Int32); break;
            case UnaryOperatorKind.UInt: opType = _compilation.GetSpecialType(SpecialType.System_UInt32); break;
            case UnaryOperatorKind.Long: opType = _compilation.GetSpecialType(SpecialType.System_Int64); break;
            case UnaryOperatorKind.ULong: opType = _compilation.GetSpecialType(SpecialType.System_UInt64); break;
            case UnaryOperatorKind.NInt: opType = _compilation.CreateNativeIntegerTypeSymbol(signed: true); break;
            case UnaryOperatorKind.NUInt: opType = _compilation.CreateNativeIntegerTypeSymbol(signed: false); break;
            case UnaryOperatorKind.Char: opType = _compilation.GetSpecialType(SpecialType.System_Char); break;
            case UnaryOperatorKind.Float: opType = _compilation.GetSpecialType(SpecialType.System_Single); break;
            case UnaryOperatorKind.Double: opType = _compilation.GetSpecialType(SpecialType.System_Double); break;
            case UnaryOperatorKind.Decimal: opType = _compilation.GetSpecialType(SpecialType.System_Decimal); break;
            case UnaryOperatorKind.Bool: opType = _compilation.GetSpecialType(SpecialType.System_Boolean); break;
            default: throw ExceptionUtilities.UnexpectedValue(kind.OperandTypes());
        }

        if (kind.IsLifted()) {
            opType = _compilation.GetOrCreateNullableType(opType);
        }

        return new UnaryOperatorSignature(kind, opType, opType);
    }

    private TypeSymbol TypeFromKind(BinaryOperatorKind kind) {
        return kind.OperandTypes() switch {
            BinaryOperatorKind.Int => GetNullableTypeCore(SpecialType.Int),
            BinaryOperatorKind.Decimal => GetNullableTypeCore(SpecialType.Decimal),
            BinaryOperatorKind.Bool => GetNullableTypeCore(SpecialType.Bool),
            BinaryOperatorKind.Object => GetNullableTypeCore(SpecialType.Object),
            BinaryOperatorKind.String => GetNullableTypeCore(SpecialType.String),
            _ => null,
        };
    }

    private ImmutableArray<BinaryOperatorSignature> GetSignaturesFromBinaryOperatorKinds(int[] operatorKinds) {
        var builder = ArrayBuilder<BinaryOperatorSignature>.GetInstance();

        foreach (var kind in operatorKinds)
            builder.Add(GetSignature((BinaryOperatorKind)kind));

        return builder.ToImmutableAndFree();
    }

    private ImmutableArray<UnaryOperatorSignature> GetSignaturesFromUnaryOperatorKinds(int[] operatorKinds) {
        var builder = ArrayBuilder<UnaryOperatorSignature>.GetInstance();
        foreach (var kind in operatorKinds) {
            builder.Add(GetSignature((UnaryOperatorKind)kind));
        }

        return builder.ToImmutableAndFree();
    }

    private void GetBinaryOpertors(BinaryOperatorKind kind, ArrayBuilder<BinaryOperatorSignature> operators) {
        foreach (var op in _builtInBinaryOperators[kind.IsConditional() ? 1 : 0][kind.OperatorIndex()])
            operators.Add(op);
    }

    #endregion

}
