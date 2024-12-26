using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="Syntax.ExpressionSyntax" />.
/// Many expressions have a possible constant value, used for <see cref="ConstantFolding" />.
/// If folding is not possible, <see cref="constantValue" /> is null.
/// </summary>
internal abstract class BoundExpression : BoundNode {
    internal abstract TypeSymbol type { get; }

    internal virtual ConstantValue constantValue => null;

    internal RefKind GetRefKind() {
        return kind switch {
            BoundNodeKind.DataContainerExpression => ((BoundDataContainerExpression)this).dataContainer.refKind,
            BoundNodeKind.ParameterExpression => ((BoundParameterExpression)this).parameter.refKind,
            BoundNodeKind.FieldAccessExpression => ((BoundFieldAccessExpression)this).field.refKind,
            BoundNodeKind.CallExpression => ((BoundCallExpression)this).method.refKind,
            _ => RefKind.None,
        };
    }

    internal bool IsLiteralNull() {
        return kind == BoundNodeKind.LiteralExpression && ConstantValue.IsNull(constantValue);
    }
}
