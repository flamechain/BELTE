using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="ReturnStatementSyntax" />.
/// </summary>
internal sealed class BoundReturnStatement : BoundStatement {
    internal BoundReturnStatement(
        SyntaxNode syntax,
        RefKind refKind,
        BoundExpression expression,
        bool hasErrors = false)
        : base(BoundKind.ReturnStatement, syntax, hasErrors) {
        this.refKind = refKind;
        this.expression = expression;
    }

    internal RefKind refKind { get; }

    internal BoundExpression expression { get; }
}
