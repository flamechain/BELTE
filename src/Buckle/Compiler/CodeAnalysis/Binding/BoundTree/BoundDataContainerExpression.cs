using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="Syntax.NameSyntax" />.
/// </summary>
internal sealed class BoundDataContainerExpression : BoundExpression {
    internal BoundDataContainerExpression(DataContainerSymbol dataContainer, ConstantValue constantValue = null) {
        this.dataContainer = dataContainer;
        this.constantValue = constantValue;
        type = dataContainer.type;
    }

    internal override BoundNodeKind kind => BoundNodeKind.DataContainerExpression;

    internal override TypeSymbol type { get; }

    internal override ConstantValue constantValue { get; }

    internal DataContainerSymbol dataContainer { get; }
}
