using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="CastExpressionSyntax" />.
/// </summary>
internal sealed class BoundCastExpression : BoundExpression {
    internal BoundCastExpression(
        SyntaxNode syntax,
        BoundExpression operand,
        Conversion conversion,
        TypeSymbol type,
        ConstantValue constantValue,
        bool hasErrors = false)
        : base(BoundKind.CastExpression, syntax, type, hasErrors) {
        this.operand = operand;
        this.constantValue = constantValue;
        this.conversion = conversion;
    }

    internal override ConstantValue constantValue { get; }

    internal BoundExpression operand { get; }

    internal Conversion conversion { get; }
}
