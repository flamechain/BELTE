using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="WhileStatementSyntax" />.
/// </summary>
internal sealed class BoundWhileStatement : BoundLoopStatement {
    internal BoundWhileStatement(
        SyntaxNode syntax,
        BoundExpression condition,
        BoundStatement body,
        SynthesizedLabelSymbol breakLabel,
        SynthesizedLabelSymbol continueLabel,
        bool hasErrors = false)
        : base(BoundKind.WhileStatement, syntax, breakLabel, continueLabel, hasErrors) {
        this.condition = condition;
        this.body = body;
    }

    internal BoundExpression condition { get; }

    internal BoundStatement body { get; }
}
