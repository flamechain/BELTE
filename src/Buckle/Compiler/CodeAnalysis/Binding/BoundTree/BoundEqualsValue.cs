using System.Collections.Immutable;
using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

internal abstract class BoundEqualsValue : BoundInitializer {
    private protected BoundEqualsValue(
        BoundKind kind,
        SyntaxNode syntax,
        ImmutableArray<DataContainerSymbol> locals,
        BoundExpression value,
        bool hasErrors)
        : base(kind, syntax, hasErrors) {
        this.locals = locals;
        this.value = value;
    }

    internal ImmutableArray<DataContainerSymbol> locals { get; }

    internal BoundExpression value { get; }
}
