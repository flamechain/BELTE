using Buckle.CodeAnalysis.Syntax;
using Buckle.Diagnostics;

namespace Buckle.CodeAnalysis.Symbols;

internal abstract class SourceUserDefinedOperatorSymbolBase : SourceOrdinaryMethodOrUserDefinedOperatorSymbol {
    private protected SourceUserDefinedOperatorSymbolBase(
        MethodKind methodKind,
        string name,
        SourceMemberContainerTypeSymbol containingType,
        BelteSyntaxNode syntax,
        DeclarationModifiers modifiers,
        bool hasAnyBody,
        BelteDiagnosticQueue diagnostics)
        : base(containingType, new SyntaxReference(syntax), (modifiers, MakeFlags(methodKind, RefKind.None, modifiers, false, false))) {
        this.name = name;
    }

    public sealed override string name { get; }
}
