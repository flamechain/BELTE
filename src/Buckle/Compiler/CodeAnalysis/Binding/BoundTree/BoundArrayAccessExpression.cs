using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="Syntax.IndexExpressionSyntax" />.
/// </summary>
internal sealed class BoundArrayAccessExpression : BoundExpression {
    internal BoundArrayAccessExpression(
        BoundExpression receiver,
        BoundExpression index,
        TypeSymbol type) {
        this.receiver = receiver;
        this.index = index;
        this.type = type;
        constantValue = ConstantFolding.FoldIndex(receiver, index, type);
    }

    internal override BoundNodeKind kind => BoundNodeKind.ArrayAccessExpression;

    internal override ConstantValue constantValue { get; }

    internal override TypeSymbol type { get; }

    internal BoundExpression receiver { get; }

    internal BoundExpression index { get; }
}
