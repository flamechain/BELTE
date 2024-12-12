using System;

namespace Buckle.CodeAnalysis.Binding;

[Flags]
internal enum BinaryOperatorKind : int {
    Error = 0x00000000,

    TypeMask = UnaryOperatorKind.TypeMask,

    Int = UnaryOperatorKind.Int,
    Char = UnaryOperatorKind.Char,
    Decimal = UnaryOperatorKind.Decimal,
    Bool = UnaryOperatorKind.Bool,
    Object = UnaryOperatorKind._Object,
    String = UnaryOperatorKind._String,
    Type = UnaryOperatorKind._Type,
    NullableNull = UnaryOperatorKind._NullableNull,
    Any = UnaryOperatorKind.Any,
    UserDefined = UnaryOperatorKind.UserDefined,

    OpMask = UnaryOperatorKind.OpMask,

    Multiplication = 0x00001000,
    Addition = 0x00001100,
    Subtraction = 0x00001200,
    Division = 0x00001300,
    Modulo = 0x00001400,
    LeftShift = 0x00001500,
    RightShift = 0x00001600,
    Equal = 0x00001700,
    NotEqual = 0x00001800,
    GreaterThan = 0x00001900,
    LessThan = 0x00001A00,
    GreaterThanOrEqual = 0x00001B00,
    LessThanOrEqual = 0x00001C00,
    UnsignedRightShift = 0x00001D00,
    And = 0x00001E00,
    Or = 0x00001F00,
    Xor = 0x00002000,
    Power = 0x00002100,

    Conditional = UnaryOperatorKind._Conditional,

    IntMultiplication = Int | Multiplication,
    DecimalMultiplication = Decimal | Multiplication,
    UserDefinedMultiplication = UserDefined | Multiplication,

    IntAddition = Int | Addition,
    DecimalAddition = Decimal | Addition,
    StringConcatenation = String | Addition,
    UserDefinedAddition = UserDefined | Addition,

    IntSubtraction = Int | Subtraction,
    DecimalSubtraction = Decimal | Subtraction,
    UserDefinedSubtraction = UserDefined | Subtraction,

    IntDivision = Int | Division,
    DecimalDivision = Decimal | Division,
    UserDefinedDivision = UserDefined | Division,

    IntModulo = Int | Modulo,
    DecimalModulo = Decimal | Modulo,
    UserDefinedModulo = UserDefined | Modulo,

    IntLeftShift = Int | LeftShift,
    UserDefinedLeftShift = UserDefined | LeftShift,

    IntRightShift = Int | RightShift,
    UserDefinedRightShift = UserDefined | RightShift,

    IntUnsignedRightShift = Int | UnsignedRightShift,
    UserDefinedUnsignedRightShift = UserDefined | UnsignedRightShift,

    IntEqual = Int | Equal,
    DecimalEqual = Decimal | Equal,
    BoolEqual = Bool | Equal,
    CharEqual = Char | Equal,
    NullableNullEqual = NullableNull | Equal,
    UserDefinedEqual = UserDefined | Equal,
    StringEqual = String | Equal,
    ObjectEqual = Object | Equal,
    TypeEqual = Type | Equal,

    IntNotEqual = Int | NotEqual,
    DecimalNotEqual = Decimal | NotEqual,
    BoolNotEqual = Bool | NotEqual,
    CharNotEqual = Char | NotEqual,
    NullableNullNotEqual = NullableNull | NotEqual,
    UserDefinedNotEqual = UserDefined | NotEqual,
    StringNotEqual = String | NotEqual,
    ObjectNotEqual = Object | NotEqual,
    TypeNotEqual = Type | NotEqual,

    IntLessThan = Int | LessThan,
    DecimalLessThan = Decimal | LessThan,
    UserDefinedLessThan = UserDefined | LessThan,

    IntGreaterThan = Int | GreaterThan,
    DecimalGreaterThan = Decimal | GreaterThan,
    UserDefinedGreaterThan = UserDefined | GreaterThan,

    IntLessThanOrEqual = Int | LessThanOrEqual,
    DecimalLessThanOrEqual = Decimal | LessThanOrEqual,
    UserDefinedLessThanOrEqual = UserDefined | LessThanOrEqual,

    IntGreaterThanOrEqual = Int | GreaterThanOrEqual,
    DecimalGreaterThanOrEqual = Decimal | GreaterThanOrEqual,
    UserDefinedGreaterThanOrEqual = UserDefined | GreaterThanOrEqual,

    IntAnd = Int | And,
    BoolAnd = Bool | And,
    UserDefinedAnd = UserDefined | And,

    ConditionalAnd = And | Conditional,
    BoolConditionalAnd = Bool | ConditionalAnd,
    UserDefinedConditionalAnd = UserDefined | ConditionalAnd,

    IntOr = Int | Or,
    BoolOr = Bool | Or,
    UserDefinedOr = UserDefined | Or,

    ConditionalOr = Or | Conditional,
    BoolConditionalOr = Bool | ConditionalOr,
    UserDefinedConditionalOr = UserDefined | ConditionalOr,

    IntXor = Int | Xor,
    BoolXor = Bool | Xor,
    UserDefinedXor = UserDefined | Xor,

    IntPower = Int | Power,
    DecimalPower = Decimal | Power,
    UserDefinedPower = UserDefined | Power,
}
