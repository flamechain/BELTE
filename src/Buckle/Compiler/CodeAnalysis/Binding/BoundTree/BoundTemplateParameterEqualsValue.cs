using System.Collections.Immutable;
using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="Syntax.TemplateParameterListSyntax" />.
/// </summary>
internal sealed class BoundTemplateParameterEqualsValue : BoundEqualsValue {
    internal BoundTemplateParameterEqualsValue(
        TemplateParameterSymbol parameter,
        ImmutableArray<DataContainerSymbol> locals,
        BoundExpression value)
        : base(locals, value) {
        this.parameter = parameter;
    }

    internal override BoundNodeKind kind => BoundNodeKind.TemplateParameterEqualsValue;

    internal TemplateParameterSymbol parameter { get; }
}
