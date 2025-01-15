using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="IndexExpressionSyntax" />.
/// </summary>
internal sealed class BoundArrayAccessExpression : BoundExpression {
    internal BoundArrayAccessExpression(
        SyntaxNode syntax,
        BoundExpression receiver,
        BoundExpression index,
        TypeSymbol type,
        ConstantValue constantValue,
        bool hasErrors = false)
        : base(BoundKind.ArrayAccessExpression, syntax, type, hasErrors) {
        this.receiver = receiver;
        this.index = index;
        this.constantValue = constantValue;
    }

    internal override ConstantValue constantValue { get; }

    internal BoundExpression receiver { get; }

    internal BoundExpression index { get; }
}
