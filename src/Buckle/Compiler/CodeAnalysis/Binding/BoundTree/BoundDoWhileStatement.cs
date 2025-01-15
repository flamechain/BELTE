using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="DoWhileStatementSyntax" />.
/// Doesn't survive lowering.
/// </summary>
internal sealed class BoundDoWhileStatement : BoundLoopStatement {
    internal BoundDoWhileStatement(
        SyntaxNode syntax,
        BoundStatement body,
        BoundExpression condition,
        SynthesizedLabelSymbol breakLabel,
        SynthesizedLabelSymbol continueLabel,
        bool hasErrors = false)
        : base(BoundKind.DoWhileStatement, syntax, breakLabel, continueLabel, hasErrors) {
        this.body = body;
        this.condition = condition;
    }

    internal BoundStatement body { get; }

    internal BoundExpression condition { get; }
}
