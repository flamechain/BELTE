using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="UnaryExpressionSyntax" />.
/// </summary>
internal sealed class BoundUnaryExpression : BoundExpression {
    internal BoundUnaryExpression(
        SyntaxNode syntax,
        BoundExpression operand,
        UnaryOperatorKind opKind,
        TypeSymbol type,
        ConstantValue constantValue,
        bool hasErrors = false)
        : base(BoundKind.UnaryExpression, syntax, type, hasErrors) {
        this.operand = operand;
        this.opKind = opKind;
        this.constantValue = constantValue;
    }

    internal override ConstantValue constantValue { get; }

    internal UnaryOperatorKind opKind { get; }

    internal BoundExpression operand { get; }
}
