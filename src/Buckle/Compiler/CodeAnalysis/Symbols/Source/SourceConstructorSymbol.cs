using Buckle.CodeAnalysis.Binding;
using Buckle.CodeAnalysis.Syntax;
using Buckle.CodeAnalysis.Text;
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
                out var hasErrors
            )
        ) {
        var hasAnyBody = _flags.hasAnyBody;
        var location = syntax.constructorKeyword.location;

        ModifierHelpers.CheckAccessibility(_modifiers, diagnostics, location);

        if (!hasErrors)
            CheckModifiers(hasAnyBody, location, diagnostics);
    }

    private protected override bool _allowRef => true;

    internal static SourceConstructorSymbol CreateConstructorSymbol(
        SourceMemberContainerTypeSymbol containingType,
        ConstructorDeclarationSyntax syntax,
        BelteDiagnosticQueue diagnostics) {
        // Eventually this will distinguish static and instance constructors
        return new SourceConstructorSymbol(containingType, syntax, diagnostics);
    }

    internal ConstructorDeclarationSyntax GetSyntax() {
        return (ConstructorDeclarationSyntax)syntaxReference.node;
    }

    internal override ExecutableCodeBinder TryGetBodyBinder(
        BinderFactory binderFactory = null,
        bool ignoreAccessibility = false) {
        return TryGetBodyBinderFromSyntax(binderFactory, ignoreAccessibility);
    }

    private protected override ParameterListSyntax GetParameterList() {
        return GetSyntax().parameterList;
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

    private void CheckModifiers(bool hasBody, TextLocation location, BelteDiagnosticQueue diagnostics) {
        if (!hasBody) ;
        // TODO Does this ever happen? Or does the Parser catch this
        // diagnostics.Add(ErrorCode.ERR_ConcreteMissingBody, location, this);
        else if (containingType.isSealed && declaredAccessibility == Accessibility.Protected && !isOverride)
            diagnostics.Push(Warning.ProtectedMemberInSealedType(location, containingType, this));
        else if (containingType.isStatic)
            diagnostics.Push(Error.ConstructorInStaticClass(location));
    }
}
