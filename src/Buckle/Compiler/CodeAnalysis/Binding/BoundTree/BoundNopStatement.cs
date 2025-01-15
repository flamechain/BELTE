using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Used to replace <see cref="EmptyExpressionSyntax" />, as debugging symbols, and placeholders.
/// Additionally used to mark the start and end of exception handlers in the <see cref="Emitting.ILEmitter" />.
/// </summary>
internal sealed class BoundNopStatement : BoundStatement {
    internal BoundNopStatement(SyntaxNode syntax, bool hasErrors = false)
        : base(BoundKind.NopStatement, syntax, hasErrors) { }
}
