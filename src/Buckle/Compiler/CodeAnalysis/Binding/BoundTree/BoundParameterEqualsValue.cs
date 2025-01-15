using System.Collections.Immutable;
using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="AssignmentExpressionSyntax" /> or <see cref="ParameterSyntax" />.
/// </summary>
internal sealed class BoundParameterEqualsValue : BoundEqualsValue {
    internal BoundParameterEqualsValue(
        SyntaxNode syntax,
        ParameterSymbol parameter,
        ImmutableArray<DataContainerSymbol> locals,
        BoundExpression value,
        bool hasErrors = false)
        : base(BoundKind.ParameterEqualsValue, syntax, locals, value, hasErrors) {
        this.parameter = parameter;
    }

    internal ParameterSymbol parameter { get; }
}
