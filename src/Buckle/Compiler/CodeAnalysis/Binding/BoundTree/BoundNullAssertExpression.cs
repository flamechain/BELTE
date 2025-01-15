using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="UnaryExpressionSyntax" />.
/// </summary>
internal sealed class BoundNullAssertExpression : BoundExpression {
    internal BoundNullAssertExpression(
        SyntaxNode syntax,
        BoundExpression operand,
        TypeSymbol type,
        ConstantValue constantValue,
        bool hasErrors = false)
        : base(BoundKind.NullAssertExpression, syntax, type, hasErrors) {
        this.operand = operand;
        this.constantValue = constantValue;
    }

    internal override ConstantValue constantValue { get; }

    internal BoundExpression operand { get; }
}
