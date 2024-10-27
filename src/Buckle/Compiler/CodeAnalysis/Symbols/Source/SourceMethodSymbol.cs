using System.Collections.Immutable;
using Buckle.CodeAnalysis.Binding;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Symbols;

internal abstract class SourceMethodSymbol : MethodSymbol {
    private protected SourceMethodSymbol(SyntaxReference syntaxReference) {
        this.syntaxReference = syntaxReference;
    }

    internal sealed override bool hidesBaseMethodsByName => false;

    internal override SyntaxReference syntaxReference { get; }

    internal override bool hasSpecialName => methodKind switch {
        MethodKind.Constructor => true,
        _ => false,
    };

    internal virtual Binder outerBinder => null;

    internal virtual Binder withTemplateParametersBinder => null;

    internal BelteSyntaxNode syntaxNode => (BelteSyntaxNode)syntaxReference.node;

    internal SyntaxTree syntaxTree => syntaxReference.syntaxTree;

    internal abstract ImmutableArray<ImmutableArray<TypeWithAnnotations>> GetTypeParameterConstraintTypes();

    internal abstract ImmutableArray<TypeParameterConstraintKinds> GetTypeParameterConstraintKinds();

    private protected BelteSyntaxNode GetInMethodSyntaxNode() {
        return syntaxNode switch {
            ConstructorDeclarationSyntax constructor
                => constructor.constructorInitializer ?? (BelteSyntaxNode)constructor.body,
            BaseMethodDeclarationSyntax method => method.body,
            LocalFunctionStatementSyntax localFunction => localFunction.body,
            ClassDeclarationSyntax classDeclaration => classDeclaration,
            _ => null,
        };
    }
}
