using System;

namespace Buckle.CodeAnalysis.Binding;

[Flags]
internal enum UnaryOperatorKind : int {
    Error = 0x00000000,

    TypeMask = 0x000000FF,

    Int = 0x00000001,
    Char = 0x00000002,
    Decimal = 0x00000003,
    Bool = 0x00000004,
    _Object = 0x00000005,
    _String = 0x00000006,
    _Type = 0x00000007,
    _NullableNull = 0x00000008,
    Any = 0x00000009,
    UserDefined = 0x0000000A,

    OpMask = 0x0000FF00,

    PostfixIncrement = 0x00001000,
    PostfixDecrement = 0x00001100,
    PrefixIncrement = 0x00001200,
    PrefixDecrement = 0x00001300,
    UnaryPlus = 0x00001400,
    UnaryMinus = 0x00001500,
    LogicalNegation = 0x00001600,
    BitwiseComplement = 0x00001700,
    True = 0x00001800,
    False = 0x00001900,

    _Conditional = 0x00010000,

    IntPostfixIncrement = Int | PostfixIncrement,
    DecimalPostfixIncrement = Decimal | PostfixIncrement,
    UserDefinedPostfixIncrement = UserDefined | PostfixIncrement,

    IntPrefixIncrement = Int | PrefixIncrement,
    DecimalPrefixIncrement = Decimal | PrefixIncrement,
    UserDefinedPrefixIncrement = UserDefined | PrefixIncrement,

    IntPostfixDecrement = Int | PostfixDecrement,
    DecimalPostfixDecrement = Decimal | PostfixDecrement,
    UserDefinedPostfixDecrement = UserDefined | PostfixDecrement,

    IntPrefixDecrement = Int | PrefixDecrement,
    DecimalPrefixDecrement = Decimal | PrefixDecrement,
    UserDefinedPrefixDecrement = UserDefined | PrefixDecrement,

    IntUnaryPlus = Int | UnaryPlus,
    DecimalUnaryPlus = Decimal | UnaryPlus,
    UserDefinedUnaryPlus = UserDefined | UnaryPlus,

    IntUnaryMinus = Int | UnaryMinus,
    DecimalUnaryMinus = Decimal | UnaryMinus,
    UserDefinedUnaryMinus = UserDefined | UnaryMinus,

    BoolLogicalNegation = Bool | LogicalNegation,
    UserDefinedLogicalNegation = UserDefined | LogicalNegation,

    IntBitwiseComplement = Int | BitwiseComplement,
    UserDefinedBitwiseComplement = UserDefined | BitwiseComplement,

    UserDefinedTrue = UserDefined | True,
    UserDefinedFalse = UserDefined | False,
}
