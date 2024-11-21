using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="Syntax.WhileStatementSyntax" />.
/// </summary>
internal sealed class BoundWhileStatement : BoundLoopStatement {
    internal BoundWhileStatement(
        BoundExpression condition,
        BoundStatement body,
        SynthesizedLabelSymbol breakLabel,
        SynthesizedLabelSymbol continueLabel)
        : base(breakLabel, continueLabel) {
        this.condition = condition;
        this.body = body;
    }

    internal override BoundNodeKind kind => BoundNodeKind.WhileStatement;

    internal BoundExpression condition { get; }

    internal BoundStatement body { get; }
}
