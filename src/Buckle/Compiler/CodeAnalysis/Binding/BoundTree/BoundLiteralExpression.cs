using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="LiteralExpressionSyntax" />.
/// </summary>
internal sealed class BoundLiteralExpression : BoundExpression {
    internal BoundLiteralExpression(
        SyntaxNode syntax,
        ConstantValue constantValue,
        TypeSymbol type,
        bool hasErrors = false)
        : base(BoundKind.LiteralExpression, syntax, type, hasErrors) {
        this.constantValue = constantValue;
    }

    internal override ConstantValue constantValue { get; }

    internal object value => constantValue.value;
}
