using Buckle.CodeAnalysis.Syntax;
using Buckle.Diagnostics;

namespace Buckle.CodeAnalysis.Symbols;

internal sealed class SourceConstructorSymbol : SourceConstructorSymbolBase {

    private SourceConstructorSymbol(
        SourceMemberContainerTypeSymbol containingType,
        ConstructorDeclarationSyntax syntax,
        BelteDiagnosticQueue diagnostics)
        : base(
            containingType,
            syntax,
            MakeModifiersAndFlags(
                containingType,
                syntax,
                syntax.constructorInitializer.thisOrBaseKeyword.kind == SyntaxKind.ThisKeyword,
                diagnostics,
                out bool hasErrors
            )
        ) {

    }

    internal static SourceConstructorSymbol CreateConstructorSymbol(
        SourceMemberContainerTypeSymbol containingType,
        ConstructorDeclarationSyntax syntax,
        BelteDiagnosticQueue diagnostics) {
        // Eventually this will distinguish static and instance constructors
        return new SourceConstructorSymbol(containingType, syntax, diagnostics);
    }

    private static (DeclarationModifiers, Flags) MakeModifiersAndFlags(
        NamedTypeSymbol containingType,
        ConstructorDeclarationSyntax syntax,
        bool hasThisInitializer,
        BelteDiagnosticQueue diagnostics,
        out bool modifierErrors) {
        var hasAnyBody = syntax.body is not null;

        var declarationModifiers = MakeModifiers(
            containingType,
            syntax,
            hasAnyBody,
            diagnostics,
            out modifierErrors
        );

        var flags = new Flags(
            MethodKind.Constructor,
            RefKind.None,
            declarationModifiers,
            true,
            true,
            hasAnyBody,
            hasThisInitializer
        );

        return (declarationModifiers, flags);
    }

    private static DeclarationModifiers MakeModifiers(
        NamedTypeSymbol containingType,
        ConstructorDeclarationSyntax syntax,
        bool hasBody,
        BelteDiagnosticQueue diagnostics,
        out bool modifierErrors) {
        var defaultAccess = DeclarationModifiers.Private;
        var allowedModifiers = DeclarationModifiers.AccessibilityMask;

        var mods = ModifierHelpers.CreateAndCheckNonTypeMemberModifiers(
            syntax.modifiers,
            defaultAccess,
            allowedModifiers,
            syntax.constructorKeyword.location,
            diagnostics,
            out modifierErrors
        );

        return mods;
    }
}
