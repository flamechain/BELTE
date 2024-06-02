
namespace Buckle.Diagnostics;

/// <summary>
/// All codes used to represent each possible error and warning uniquely.
/// </summary>
public enum DiagnosticCode : int {
    // Never modify these codes after a release, as that would break backwards compatibility.
    // Instead mark unused errors and warnings in the docs, and append new errors and warnings to use new codes.

    // 0 is reserved for exceptions
    WRN_AlwaysValue = 1,
    WRN_NullDeference = 2,
    ERR_InvalidReference = 3,
    ERR_InvalidType = 4,
    ERR_BadCharacter = 5,
    ERR_UnexpectedToken = 6,
    ERR_CannotConvertImplicitly = 7,
    ERR_InvalidUnaryOperatorUse = 8,
    ERR_NamedBeforeUnnamed = 9,
    ERR_NamedArgumentTwice = 10,
    ERR_InvalidBinaryOperatorUse = 11,
    ERR_GlobalStatementsInMultipleFiles = 12,
    ERR_ParameterAlreadyDeclared = 13,
    ERR_InvalidMain = 14,
    ERR_NoSuchParameter = 15,
    ERR_MainAndGlobals = 16,
    ERR_UndefinedSymbol = 17,
    ERR_MethodAlreadyDeclared = 18,
    ERR_NotAllPathsReturn = 19,
    ERR_CannotConvert = 20,
    ERR_VariableAlreadyDeclared = 21,
    ERR_ConstantAssignment = 22,
    ERR_AmbiguousElse = 23,
    ERR_NoValue = 24,
    ERR_CannotApplyIndexing = 25,
    WRN_UnreachableCode = 26,
    ERR_UnterminatedString = 27,
    ERR_UndefinedMethod = 28,
    ERR_IncorrectArgumentCount = 29,
    ERR_TypeAlreadyDeclared = 30,
    ERR_DuplicateAttribute = 31,
    ERR_CannotCallNonMethod = 32,
    ERR_InvalidExpressionStatement = 33,
    ERR_UnknownType = 34,
    ERR_InvalidBreakOrContinue = 35,
    ERR_ReturnOutsideMethod = 36,
    ERR_UnexpectedReturnValue = 37,
    ERR_MissingReturnValue = 38,
    ERR_NotAVariable = 39,
    ERR_NoInitOnImplicit = 40,
    ERR_UnterminatedComment = 41,
    ERR_NullAssignOnImplicit = 42,
    ERR_EmptyInitializerListOnImplicit = 43,
    ERR_ImpliedDimensions = 44,
    ERR_CannotUseImplicit = 45,
    ERR_NoCatchOrFinally = 46,
    ERR_MemberMustBeStatic = 47,
    ERR_ExpectedOverloadableOperator = 48,
    ERR_ReferenceWrongInitialization = 49,
    ERR_WrongInitializationReference = 50,
    ERR_UnknownAttribute = 51,
    ERR_NullAssignNotNull = 52,
    ERR_ImpliedReference = 53,
    ERR_ReferenceToConstant = 54,
    ERR_VoidVariable = 55,
    ERR_ExpectedToken = 56,
    ERR_NoMethodOverload = 57,
    ERR_AmbiguousMethodOverload = 58,
    ERR_CannotIncrement = 59,
    ERR_InvalidTernaryOperatorUse = 60,
    ERR_NoSuchMember = 61,
    ERR_CannotAssign = 62,
    ERR_CannotOverloadNested = 63,
    ERR_ConstantToNonConstantReference = 64,
    ERR_InvalidPrefixUse = 65,
    ERR_InvalidPostfixUse = 66,
    ERR_ParameterAlreadySpecified = 67,
    ERR_DefaultMustBeConstant = 68,
    ERR_DefaultBeforeNoDefault = 69,
    ERR_ConstantAndVariable = 70,
    ERR_VariableUsingTypeName = 71,
    ERR_CannotImplyNull = 72,
    ERR_CannotConvertNull = 73,
    ERR_ModifierAlreadyApplied = 74,
    ERR_CannotUseRef = 75,
    ERR_DivideByZero = 76,
    ERR_NameUsedInEnclosingScope = 77,
    ERR_NullInitializerListOnImplicit = 78,
    ERR_UnrecognizedEscapeSequence = 79,
    ERR_PrimitivesDoNotHaveMembers = 80,
    ERR_CannotConstructPrimitive = 81,
    ERR_NoTemplateOverload = 82,
    ERR_AmbiguousTemplateOverload = 83,
    ERR_CannotUseStruct = 84,
    ERR_CannotUseThis = 85,
    ERR_MemberIsInaccessible = 86,
    ERR_NoConstructorOverload = 87,
    ERR_InvalidModifier = 88,
    ERR_InvalidInstanceReference = 89,
    ERR_InvalidStaticReference = 90,
    ERR_CannotInitializeInStructs = 91,
    ERR_MultipleMains = 92,
    ERR_InvalidAttributes = 93,
    ERR_TemplateNotExpected = 94,
    ERR_TemplateMustBeConstant = 95,
    ERR_CannotReferenceNonField = 96,
    ERR_CannotUseType = 97,
    ERR_StaticConstructor = 98,
    ERR_StaticVariable = 99,
    ERR_CannotConstructStatic = 100,
    ERR_ConflictingModifiers = 101,
    ERR_AssignmentInConstMethod = 102,
    ERR_NonConstantCallInConstant = 103,
    ERR_NonConstantCallOnConstant = 104,
    ERR_CannotBeRefAndConstexpr = 105,
    ERR_NotConstantExpression = 106,
    ERR_CannotReturnStatic = 107,
    ERR_IncorrectOperatorParameterCount = 108,
    ERR_OperatorMustBePublicAndStatic = 109,
    ERR_StaticOperator = 110,
    ERR_OperatorAtLeastOneClassParameter = 111,
    ERR_OperatorMustReturnClass = 112,
    ERR_IndexOperatorFirstParameter = 113,
    ERR_ArrayOutsideOfLowLevelContext = 114,
    ERR_EmptyCharacterLiteral = 115,
    ERR_CharacterLiteralTooLong = 116,
    ERR_NoInitOnNonNullable = 117,
    ERR_CannotBePrivateAndVirtualOrAbstract = 118,
    ERR_NoSuitableOverrideTarget = 119,
    ERR_OverrideCannotChangeAccessibility = 120,
    ERR_CannotDerivePrimitive = 121,
    ERR_UnknownTemplate = 122,
    ERR_CannotExtendCheckNonType = 123,
    ERR_ConstraintIsNotConstant = 124,
    ERR_StructTakesNoArguments = 125,
    ERR_ExtendConstraintFailed = 126,
    ERR_ConstraintWasNull = 127,
    ERR_ConstraintFailed = 128,
    ERR_CannotOverride = 129,

    // Carving out >=9000 for unsupported errors
    UNS_GlobalReturnValue = 9000,
    UNS_Assembling = 9001,
    UNS_Linking = 9002,
    UNS_IndependentCompilation = 9003,
    UNS_DotnetCompilation = 9004,
    UNS_OverloadedPostfix = 9005,
    UNS_CSharpTranspilation = 9006,
}
