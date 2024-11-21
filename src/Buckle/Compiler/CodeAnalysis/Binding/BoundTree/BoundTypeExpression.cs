using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="Syntax.NameSyntax" />.
/// </summary>
internal sealed class BoundTypeExpression : BoundExpression {
    internal BoundTypeExpression(TypeSymbol type) {
        this.type = type;
    }

    internal override BoundNodeKind kind => BoundNodeKind.TypeExpression;

    internal override TypeSymbol type { get; }
}
