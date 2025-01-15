using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="VariableDeclarationSyntax" />.
/// </summary>
internal sealed class BoundDataContainerDeclaration : BoundNode {
    internal BoundDataContainerDeclaration(
        SyntaxNode syntax,
        DataContainerSymbol dataContainer,
        BoundExpression initializer,
        bool hasErrors = false)
        : base(BoundKind.DataContainerDeclaration, syntax, hasErrors) {
        this.dataContainer = dataContainer;
        this.initializer = initializer;
    }

    internal DataContainerSymbol dataContainer { get; }

    internal BoundExpression initializer { get; }
}
