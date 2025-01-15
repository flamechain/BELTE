using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="NameSyntax" />.
/// </summary>
internal sealed class BoundDataContainerExpression : BoundExpression {
    internal BoundDataContainerExpression(
        SyntaxNode syntax,
        DataContainerSymbol dataContainer,
        TypeSymbol type,
        ConstantValue constantValue = null,
        bool hasErrors = false)
        : base(BoundKind.DataContainerDeclaration, syntax, type, hasErrors) {
        this.dataContainer = dataContainer;
        this.constantValue = constantValue;
    }

    internal override ConstantValue constantValue { get; }

    internal DataContainerSymbol dataContainer { get; }
}
