using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="IfStatementSyntax" />.
/// Doesn't survive lowering.
/// </summary>
internal sealed class BoundIfStatement : BoundStatement {
    internal BoundIfStatement(
        SyntaxNode syntax,
        BoundExpression condition,
        BoundStatement then,
        BoundStatement elseStatement,
        bool hasErrors = false)
        : base(BoundKind.IfStatement, syntax, hasErrors) {
        this.condition = condition;
        this.then = then;
        this.elseStatement = elseStatement;
    }

    internal BoundExpression condition { get; }

    internal BoundStatement then { get; }

    internal BoundStatement elseStatement { get; }
}
