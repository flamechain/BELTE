using System.Collections.Immutable;
using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="AssignmentExpressionSyntax" />.
/// </summary>
internal sealed class BoundFieldEqualsValue : BoundEqualsValue {
    internal BoundFieldEqualsValue(
        SyntaxNode syntax,
        FieldSymbol field,
        ImmutableArray<DataContainerSymbol> locals,
        BoundExpression value,
        bool hasErrors = false)
        : base(BoundKind.FieldEqualsValue, syntax, locals, value, hasErrors) {
        this.field = field;
    }

    internal FieldSymbol field { get; }
}
