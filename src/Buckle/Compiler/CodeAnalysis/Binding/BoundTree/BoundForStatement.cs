using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="ForStatementSyntax" />.
/// Doesn't survive lowering.
/// </summary>
internal sealed class BoundForStatement : BoundLoopStatement {
    internal BoundForStatement(
        SyntaxNode syntax,
        BoundStatement initializer,
        BoundExpression condition,
        BoundExpression step,
        BoundStatement body,
        SynthesizedLabelSymbol breakLabel,
        SynthesizedLabelSymbol continueLabel,
        bool hasErrors = false)
        : base(BoundKind.ForStatement, syntax, breakLabel, continueLabel, hasErrors) {
        this.initializer = initializer;
        this.condition = condition;
        this.step = step;
        this.body = body;
    }

    internal BoundStatement initializer { get; }

    internal BoundExpression condition { get; }

    internal BoundExpression step { get; }

    internal BoundStatement body { get; }
}
