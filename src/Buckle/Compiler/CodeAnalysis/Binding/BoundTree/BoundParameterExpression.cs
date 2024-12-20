using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="Syntax.NameSyntax" />.
/// </summary>
internal sealed class BoundParameterExpression : BoundExpression {
    internal BoundParameterExpression(ParameterSymbol parameter) {
        this.parameter = parameter;
        type = parameter.type;
        constantValue = parameter.explicitDefaultConstantValue;
    }

    internal override BoundNodeKind kind => BoundNodeKind.ParameterExpression;

    internal override ConstantValue constantValue { get; }

    internal override TypeSymbol type { get; }

    internal ParameterSymbol parameter { get; }
}
