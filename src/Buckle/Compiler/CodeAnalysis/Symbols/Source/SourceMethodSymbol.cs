using System.Collections.Immutable;
using Buckle.CodeAnalysis.Binding;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Symbols;

internal abstract class SourceMethodSymbol : MethodSymbol {
    private protected SourceMethodSymbol(SyntaxReference syntaxReference) {
        this.syntaxReference = syntaxReference;
    }

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

    internal abstract ImmutableArray<ImmutableArray<TypeWithAnnotations>> GetTypeParameterConstraintKinds();
}
