using System.Collections.Immutable;
using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="Syntax.AssignmentExpressionSyntax" /> or <see cref="Syntax.ParameterSyntax" />.
/// </summary>
internal sealed class BoundParameterEqualsValue : BoundEqualsValue {
    internal BoundParameterEqualsValue(
        ParameterSymbol parameter,
        ImmutableArray<DataContainerSymbol> locals,
        BoundExpression value)
        : base(locals, value) {
        this.parameter = parameter;
    }

    internal override BoundNodeKind kind => BoundNodeKind.ParameterEqualsValue;

    internal ParameterSymbol parameter { get; }
}
