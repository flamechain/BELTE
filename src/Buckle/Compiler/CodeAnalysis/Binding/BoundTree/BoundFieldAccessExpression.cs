using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="MemberAccessExpressionSyntax" />.
/// </summary>
internal sealed class BoundFieldAccessExpression : BoundExpression {
    internal BoundFieldAccessExpression(
        SyntaxNode syntax,
        BoundExpression receiver,
        FieldSymbol field,
        TypeSymbol type,
        ConstantValue constant,
        bool hasErrors = false)
        : base(BoundKind.FieldAccessExpression, syntax, type, hasErrors) {
        this.receiver = receiver;
        this.field = field;
        constantValue = constant;
    }

    internal override ConstantValue constantValue { get; }

    internal BoundExpression receiver { get; }

    internal FieldSymbol field { get; }
}
