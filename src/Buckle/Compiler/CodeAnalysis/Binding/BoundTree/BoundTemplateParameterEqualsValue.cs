using System.Collections.Immutable;
using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="TemplateParameterListSyntax" />.
/// </summary>
internal sealed class BoundTemplateParameterEqualsValue : BoundEqualsValue {
    internal BoundTemplateParameterEqualsValue(
        SyntaxNode syntax,
        TemplateParameterSymbol parameter,
        ImmutableArray<DataContainerSymbol> locals,
        BoundExpression value,
        bool hasErrors = false)
        : base(BoundKind.TemplateParameterEqualsValue, syntax, locals, value, hasErrors) {
        this.parameter = parameter;
    }

    internal TemplateParameterSymbol parameter { get; }
}
