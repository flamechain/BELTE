using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="Syntax.ReturnStatementSyntax" />.
/// </summary>
internal sealed class BoundReturnStatement : BoundStatement {
    internal BoundReturnStatement(RefKind refKind, BoundExpression expression) {
        this.refKind = refKind;
        this.expression = expression;
    }

    internal override BoundNodeKind kind => BoundNodeKind.ReturnStatement;

    internal RefKind refKind { get; }

    internal BoundExpression expression { get; }
}
