using System.Collections.Immutable;
using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="Syntax.CallExpressionSyntax" />.
/// </summary>
internal sealed class BoundCallExpression : BoundExpression {
    internal BoundCallExpression(
        BoundExpression expression,
        MethodSymbol method,
        ImmutableArray<BoundExpression> arguments) {
        this.expression = expression;
        this.method = method;
        this.arguments = arguments;
        type = method.returnType;
    }

    internal override BoundNodeKind kind => BoundNodeKind.CallExpression;

    internal override TypeSymbol type { get; }

    internal BoundExpression expression { get; }

    internal MethodSymbol method { get; }

    internal ImmutableArray<BoundExpression> arguments { get; }
}
