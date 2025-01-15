using System.Collections.Immutable;
using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="CallExpressionSyntax" />.
/// </summary>
internal sealed class BoundCallExpression : BoundExpression {
    internal BoundCallExpression(
        SyntaxNode syntax,
        BoundExpression receiver,
        MethodSymbol method,
        ImmutableArray<BoundExpression> arguments,
        ImmutableArray<RefKind> argumentRefKinds,
        TypeSymbol type,
        bool hasErrors = false)
        : base(BoundKind.CallExpression, syntax, type, hasErrors) {
        this.receiver = receiver;
        this.method = method;
        this.arguments = arguments;
        this.argumentRefKinds = argumentRefKinds;
    }

    internal BoundExpression receiver { get; }

    internal MethodSymbol method { get; }

    internal ImmutableArray<BoundExpression> arguments { get; }

    internal ImmutableArray<RefKind> argumentRefKinds { get; }
}
