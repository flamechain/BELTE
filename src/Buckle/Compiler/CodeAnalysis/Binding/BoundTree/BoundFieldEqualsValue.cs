using System.Collections.Immutable;
using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="Syntax.AssignmentExpressionSyntax" />.
/// </summary>
internal sealed class BoundFieldEqualsValue : BoundEqualsValue {
    internal BoundFieldEqualsValue(FieldSymbol field, ImmutableArray<DataContainerSymbol> locals, BoundExpression value)
        : base(locals, value) {
        this.field = field;
    }

    internal override BoundNodeKind kind => BoundNodeKind.FieldEqualsValue;

    internal FieldSymbol field { get; }
}
