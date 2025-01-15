using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="MemberAccessExpressionSyntax" />.
/// Doesn't survive lowering.
/// </summary>
internal sealed class BoundConditionalAccessExpression : BoundExpression {
    internal BoundConditionalAccessExpression(
        SyntaxNode syntax,
        BoundExpression receiver,
        BoundExpression accessExpression,
        TypeSymbol type,
        bool hasErrors = false)
        : base(BoundKind.ConditionalAccessExpression, syntax, type, hasErrors) {
        this.receiver = receiver;
        this.accessExpression = accessExpression;
        constantValue = accessExpression.constantValue;
    }

    internal override ConstantValue constantValue { get; }

    internal BoundExpression receiver { get; }

    internal BoundExpression accessExpression { get; }
}
