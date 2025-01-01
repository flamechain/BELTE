using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="Syntax.NameSyntax" />.
/// </summary>
internal sealed class BoundTypeExpression : BoundExpression {
    internal BoundTypeExpression(TypeSymbol type) {
        this.type = type;
    }

    internal BoundTypeExpression(TypeSymbol type, TypeWithAnnotations typeWithAnnotations) {
        this.type = type;
        this.typeWithAnnotations = typeWithAnnotations;
    }

    internal override BoundNodeKind kind => BoundNodeKind.TypeExpression;

    internal override TypeSymbol type { get; }

    internal TypeWithAnnotations typeWithAnnotations { get; }
}
