using System.Collections.Immutable;
using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="InitializerListExpressionSyntax" />.
/// </summary>
internal sealed class BoundInitializerListExpression : BoundExpression {
    internal BoundInitializerListExpression(
        SyntaxNode syntax,
        ImmutableArray<BoundExpression> items,
        TypeSymbol type,
        ConstantValue constantValue = null,
        bool hasErrors = false)
        : base(BoundKind.InitializerListExpression, syntax, type, hasErrors) {
        this.items = items;
        this.constantValue = constantValue;
    }

    internal override ConstantValue constantValue { get; }

    internal ImmutableArray<BoundExpression> items { get; }
}
