using System.Collections.Immutable;
using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="InitializerDictionaryExpressionSyntax" />.
/// Doesn't survive compilation.
/// </summary>
internal sealed class BoundInitializerDictionaryExpression : BoundExpression {
    internal BoundInitializerDictionaryExpression(
        SyntaxNode syntax,
        ImmutableArray<(BoundExpression, BoundExpression)> items,
        TypeSymbol type,
        bool hasErrors = false)
        : base(BoundKind.InitializerDictionaryExpression, syntax, type, hasErrors) {
        this.items = items;
    }

    internal override ConstantValue constantValue { get; }

    internal ImmutableArray<(BoundExpression, BoundExpression)> items { get; }
}
