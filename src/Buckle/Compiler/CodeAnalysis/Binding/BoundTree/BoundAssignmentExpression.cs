using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="AssignmentExpressionSyntax" />.
/// </summary>
internal sealed class BoundAssignmentExpression : BoundExpression {
    internal BoundAssignmentExpression(
        SyntaxNode syntax,
        BoundExpression left,
        BoundExpression right,
        bool isRef,
        TypeSymbol type,
        bool hasErrors = false)
        : base(BoundKind.AssignmentExpression, syntax, type, hasErrors) {
        this.left = left;
        this.right = right;
        this.isRef = isRef;
    }

    internal BoundExpression left { get; }

    internal BoundExpression right { get; }

    internal bool isRef { get; }
}
