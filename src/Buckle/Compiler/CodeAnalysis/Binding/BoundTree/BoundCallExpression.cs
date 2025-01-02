using System.Collections.Immutable;
using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="Syntax.CallExpressionSyntax" />.
/// </summary>
internal sealed class BoundCallExpression : BoundExpression {
    internal BoundCallExpression(
        BoundExpression receiver,
        MethodSymbol method,
        ImmutableArray<BoundExpression> arguments,
        ImmutableArray<RefKind> argumentRefKinds) {
        this.receiver = receiver;
        this.method = method;
        this.arguments = arguments;
        this.argumentRefKinds = argumentRefKinds;
        type = method.returnType;
    }

    internal override BoundNodeKind kind => BoundNodeKind.CallExpression;

    internal override TypeSymbol type { get; }

    internal BoundExpression receiver { get; }

    internal MethodSymbol method { get; }

    internal ImmutableArray<BoundExpression> arguments { get; }

    internal ImmutableArray<RefKind> argumentRefKinds { get; }
}
