using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="Syntax.VariableDeclarationSyntax" />.
/// </summary>
internal sealed class BoundDataContainerDeclaration : BoundNode {
    internal BoundDataContainerDeclaration(DataContainerSymbol dataContainer, BoundExpression initializer) {
        this.dataContainer = dataContainer;
        this.initializer = initializer;
    }

    internal override BoundNodeKind kind => BoundNodeKind.DataContainerDeclaration;

    internal DataContainerSymbol dataContainer { get; }

    internal BoundExpression initializer { get; }
}
