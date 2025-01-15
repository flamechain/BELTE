using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a parser <see cref="AssignmentExpressionSyntax" />.
/// Doesn't survive lowering.
/// </summary>
internal sealed class BoundCompoundAssignmentExpression : BoundExpression {
    internal BoundCompoundAssignmentExpression(
        SyntaxNode syntax,
        BoundExpression left,
        BoundExpression right,
        BinaryOperatorKind opKind,
        TypeSymbol type,
        bool hasErrors = false)
        : base(BoundKind.CompoundAssignmentExpression, syntax, type, hasErrors) {
        this.left = left;
        this.right = right;
        this.opKind = opKind;
    }

    internal BoundExpression left { get; }

    internal BinaryOperatorKind opKind { get; }

    internal BoundExpression right { get; }
}
