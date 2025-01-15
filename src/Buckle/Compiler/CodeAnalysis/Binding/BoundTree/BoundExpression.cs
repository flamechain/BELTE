using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="ExpressionSyntax" />.
/// Many expressions have a possible constant value, used for <see cref="ConstantFolding" />.
/// If folding is not possible, <see cref="constantValue" /> is null.
/// </summary>
internal abstract class BoundExpression : BoundNode {
    private protected BoundExpression(BoundKind kind, SyntaxNode syntax, TypeSymbol type, bool hasErrors)
        : base(kind, syntax, hasErrors) {
        this.type = type;
    }

    internal TypeSymbol type { get; }

    internal virtual ConstantValue constantValue => null;

    internal RefKind GetRefKind() {
        return kind switch {
            BoundKind.DataContainerExpression => ((BoundDataContainerExpression)this).dataContainer.refKind,
            BoundKind.ParameterExpression => ((BoundParameterExpression)this).parameter.refKind,
            BoundKind.FieldAccessExpression => ((BoundFieldAccessExpression)this).field.refKind,
            BoundKind.CallExpression => ((BoundCallExpression)this).method.refKind,
            _ => RefKind.None,
        };
    }

    internal bool IsLiteralNull() {
        return kind == BoundKind.LiteralExpression && ConstantValue.IsNull(constantValue);
    }
}
