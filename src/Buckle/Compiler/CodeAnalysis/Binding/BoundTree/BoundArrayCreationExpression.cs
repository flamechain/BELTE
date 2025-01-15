using System.Collections.Immutable;
using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="ObjectCreationExpressionSyntax" />.
/// </summary>
internal sealed class BoundArrayCreationExpression : BoundExpression {
    internal BoundArrayCreationExpression(
        SyntaxNode syntax,
        ImmutableArray<BoundExpression> sizes,
        TypeSymbol type,
        bool hasErrors = false)
        : base(BoundKind.ArrayCreationExpression, syntax, type, hasErrors) {
        this.sizes = sizes;
    }

    internal ImmutableArray<BoundExpression> sizes { get; }
}
