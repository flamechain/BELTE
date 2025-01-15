using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="ExpressionStatementSyntax" />.
/// </summary>
internal sealed class BoundExpressionStatement : BoundStatement {
    internal BoundExpressionStatement(SyntaxNode syntax, BoundExpression expression, bool hasErrors)
        : base(BoundKind.ExpressionStatement, syntax, hasErrors) {
        this.expression = expression;
    }

    internal BoundExpression expression { get; }
}
