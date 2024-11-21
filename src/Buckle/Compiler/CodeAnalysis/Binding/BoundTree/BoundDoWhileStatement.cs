using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="Syntax.DoWhileStatementSyntax" />.
/// Doesn't survive lowering.
/// </summary>
internal sealed class BoundDoWhileStatement : BoundLoopStatement {
    internal BoundDoWhileStatement(
        BoundStatement body,
        BoundExpression condition,
        SynthesizedLabelSymbol breakLabel,
        SynthesizedLabelSymbol continueLabel)
        : base(breakLabel, continueLabel) {
        this.body = body;
        this.condition = condition;
    }

    internal override BoundNodeKind kind => BoundNodeKind.DoWhileStatement;

    internal BoundStatement body { get; }

    internal BoundExpression condition { get; }
}
