using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="BinaryExpressionSyntax" />.
/// </summary>
internal sealed class BoundBinaryExpression : BoundExpression {
    internal BoundBinaryExpression(
        SyntaxNode syntax,
        BoundExpression left,
        BoundExpression right,
        BinaryOperatorKind opKind,
        TypeSymbol type,
        ConstantValue constantValue,
        bool hasErrors = false)
        : base(BoundKind.BinaryExpression, syntax, type, hasErrors) {
        this.left = left;
        this.right = right;
        this.opKind = opKind;
        this.constantValue = constantValue;
    }

    internal override ConstantValue constantValue { get; }

    internal BoundExpression left { get; }

    internal BinaryOperatorKind opKind { get; }

    internal BoundExpression right { get; }
}
