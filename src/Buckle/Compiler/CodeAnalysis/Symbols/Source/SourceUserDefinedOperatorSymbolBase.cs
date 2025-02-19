using System.Collections.Immutable;
using Buckle.CodeAnalysis.Binding;
using Buckle.CodeAnalysis.Syntax;
using Buckle.CodeAnalysis.Text;
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
        : base(
            containingType,
            new SyntaxReference(syntax),
            (modifiers, new Flags(methodKind, RefKind.None, modifiers, false, false, hasAnyBody, false))
        ) {
        this.name = name;
        var location = ((OperatorDeclarationSyntax)syntaxReference.node).operatorToken.location;

        if (containingType.isStatic) {
            diagnostics.Push(Error.OperatorInStaticClass(location));
            return;
        }

        if (declaredAccessibility != Accessibility.Public || !isStatic)
            diagnostics.Push(Error.OperatorMustBePublicAndStatic(location));

        if (hasAnyBody && isAbstract)
            diagnostics.Push(Error.AbstractCannotHaveBody(location, this));

        if (!hasAnyBody && !isAbstract)
            diagnostics.Push(Error.NonAbstractMustHaveBody(location, this));

        ModifierHelpers.CheckAccessibility(_modifiers, diagnostics, location);
    }

    public sealed override string name { get; }

    public sealed override ImmutableArray<TemplateParameterSymbol> templateParameters => [];

    public sealed override ImmutableArray<BoundExpression> templateConstraints => [];

    internal sealed override ImmutableArray<TypeParameterConstraintKinds> GetTypeParameterConstraintKinds() {
        return [];
    }

    internal sealed override ImmutableArray<ImmutableArray<TypeWithAnnotations>> GetTypeParameterConstraintTypes() {
        return [];
    }

    private protected override void MethodChecks(BelteDiagnosticQueue diagnostics) {
        var syntax = (OperatorDeclarationSyntax)syntaxReference.node;
        var (returnType, parameters) = MakeParametersAndBindReturnType(syntax, syntax.returnType, diagnostics);

        MethodChecks(returnType, parameters, diagnostics);

        if (containingType.isStatic)
            return;

        CheckValueParameters(diagnostics);
        // TODO
        // CheckOperatorSignatures(diagnostics);
    }

    private void CheckValueParameters(BelteDiagnosticQueue diagnostics) {
        foreach (var parameter in parameters) {
            if (parameter.refKind != RefKind.None) {
                // TODO
                // diagnostics.Push(Error.OperatorCannotHaveRefParameters(parameter.syntaxReference.location));
                break;
            }
        }
    }

    private protected (TypeWithAnnotations ReturnType, ImmutableArray<ParameterSymbol> Parameters)
        MakeParametersAndBindReturnType(
            BaseMethodDeclarationSyntax declarationSyntax,
            TypeSyntax returnTypeSyntax,
            BelteDiagnosticQueue diagnostics) {
        TypeWithAnnotations returnType;
        ImmutableArray<ParameterSymbol> parameters;

        var binder = declaringCompilation.GetBinderFactory(declarationSyntax.syntaxTree)
            .GetBinder(returnTypeSyntax, declarationSyntax, this);

        var signatureBinder = binder.WithAdditionalFlags(BinderFlags.SuppressConstraintChecks);

        parameters = ParameterHelpers.MakeParameters(
            signatureBinder,
            this,
            declarationSyntax.parameterList.parameters,
            diagnostics,
            true,
            isVirtual || isAbstract
        ).Cast<SourceParameterSymbol, ParameterSymbol>();

        returnType = signatureBinder.BindType(returnTypeSyntax, diagnostics);

        return (returnType, parameters);
    }

    private protected static DeclarationModifiers MakeDeclarationModifiers(
        BaseMethodDeclarationSyntax syntax,
        TextLocation location,
        BelteDiagnosticQueue diagnostics) {
        var defaultAccess = DeclarationModifiers.Private;
        var allowedModifiers = DeclarationModifiers.Static
            | DeclarationModifiers.LowLevel
            | DeclarationModifiers.AccessibilityMask;

        var result = ModifierHelpers.CreateAndCheckNonTypeMemberModifiers(
            syntax.modifiers,
            defaultAccess,
            allowedModifiers,
            location,
            diagnostics,
            out _
        );

        return result;
    }
}
