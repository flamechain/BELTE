using System.Collections.Immutable;
using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="ObjectCreationExpressionSyntax" />.
/// </summary>
internal sealed class BoundObjectCreationExpression : BoundExpression {
    internal BoundObjectCreationExpression(
        SyntaxNode syntax,
        MethodSymbol constructor,
        ImmutableArray<BoundExpression> arguments,
        TypeSymbol type,
        bool hasErrors = false)
        : base(BoundKind.ObjectCreationExpression, syntax, type, hasErrors) {
        this.constructor = constructor;
        this.arguments = arguments;
    }

    internal MethodSymbol constructor { get; }

    internal ImmutableArray<BoundExpression> arguments { get; }
}
