using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="NameSyntax" />.
/// </summary>
internal sealed class BoundParameterExpression : BoundExpression {
    internal BoundParameterExpression(
        SyntaxNode syntax,
        ParameterSymbol parameter,
        TypeSymbol type,
        ConstantValue constantValue,
        bool hasErrors = false)
        : base(BoundKind.ParameterExpression, syntax, type, hasErrors) {
        this.parameter = parameter;
        this.constantValue = constantValue;
    }

    internal override ConstantValue constantValue { get; }

    internal ParameterSymbol parameter { get; }
}
