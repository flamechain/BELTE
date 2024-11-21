using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Represents binding an expression that failed. The type is the best approximation by the compiler.
/// </summary>
internal sealed class BoundErrorExpression : BoundExpression {
    internal BoundErrorExpression(TypeSymbol type) {
        this.type = type;
    }

    internal override BoundNodeKind kind => BoundNodeKind.ErrorExpression;

    internal override TypeSymbol type { get; }
}
