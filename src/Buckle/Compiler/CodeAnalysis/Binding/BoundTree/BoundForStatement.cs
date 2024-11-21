using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="Syntax.ForStatementSyntax" />.
/// Doesn't survive lowering.
/// </summary>
internal sealed class BoundForStatement : BoundLoopStatement {
    internal BoundForStatement(
        BoundStatement initializer,
        BoundExpression condition,
        BoundExpression step,
        BoundStatement body,
        SynthesizedLabelSymbol breakLabel,
        SynthesizedLabelSymbol continueLabel)
        : base(breakLabel, continueLabel) {
        this.initializer = initializer;
        this.condition = condition;
        this.step = step;
        this.body = body;
    }

    internal override BoundNodeKind kind => BoundNodeKind.ForStatement;

    internal BoundStatement initializer { get; }

    internal BoundExpression condition { get; }

    internal BoundExpression step { get; }

    internal BoundStatement body { get; }
}
