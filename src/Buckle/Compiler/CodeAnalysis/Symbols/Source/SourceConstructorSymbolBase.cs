
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Symbols;

internal abstract class SourceConstructorSymbolBase : SourceMemberMethodSymbol {
    private protected SourceConstructorSymbolBase(
        SourceMemberContainerTypeSymbol containingType,
        BelteSyntaxNode syntax,
        (DeclarationModifiers declarationModifiers, Flags flags) modifiersAndFlags)
        : base(containingType, new SyntaxReference(syntax), modifiersAndFlags) { }

    // TODO
}
