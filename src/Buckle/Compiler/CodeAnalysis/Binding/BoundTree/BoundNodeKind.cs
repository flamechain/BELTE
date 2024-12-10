
namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Types of bound nodes.
/// </summary>
internal enum BoundNodeKind : byte {
    ConditionalExpression,
    BinaryExpression,
    UnaryExpression,
    LiteralExpression,
    DataContainerExpression,
    AssignmentExpression,
    EmptyExpression,
    ErrorExpression,
    CallExpression,
    ArrayAccessExpression,
    CastExpression,
    CompoundAssignmentExpression,
    ReferenceExpression,
    TypeOfExpression,
    ObjectCreationExpression,
    ArrayCreationExpression,
    FieldAccessExpression,
    ConditionalAccessExpression,
    ThisExpression,
    BaseExpression,
    ExtendExpression,
    ThrowExpression,
    InitializerListExpression,
    InitializerDictionaryExpression,
    TypeExpression,
    ParameterExpression,
    IsExpression,
    IsntExpression,
    AsExpression,
    NullCoalescingExpression,

    BlockStatement,
    ExpressionStatement,
    LocalDeclarationStatement,
    IfStatement,
    WhileStatement,
    ForStatement,
    GotoStatement,
    LabelStatement,
    ConditionalGotoStatement,
    DoWhileStatement,
    TryStatement,
    ReturnStatement,
    NopStatement,
    BreakStatement,
    ContinueStatement,

    DataContainerDeclaration,
    FieldEqualsValue,
    ParameterEqualsValue,
    TemplateParameterEqualsValue,
    GlobalStatement,
    MethodGroup,
    ConstructorMethodBody,
    NonConstructorMethodBody,
}
