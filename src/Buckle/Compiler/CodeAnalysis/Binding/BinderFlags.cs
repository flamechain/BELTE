using System;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// A specific location for binding.
/// </summary>
[Flags]
internal enum BinderFlags : uint {
    None,
    LowLevelContext = 1 << 0,
    IgnoreAccessibility = 1 << 1,
    TemplateConstraintsClause = 1 << 2,
    SuppressConstraintChecks = 1 << 3,
    SuppressTemplateArgumentBinding = 1 << 4,
    ParameterDefaultValue = 1 << 5,
    FieldInitializer = 1 << 6,
    ConstructorInitializer = 1 << 7,
    ObjectInitializerMember = 1 << 8,

    InCatchBlock = 1 << 9,
    InFinallyBlock = 1 << 10,
    InTryBlockOfTryCatch = 1 << 11,
    InNestedFinallyBlock = 1 << 12,

    AllClearedAtExecutableCodeBoundary = InCatchBlock | InFinallyBlock | InTryBlockOfTryCatch | InNestedFinallyBlock,
}
