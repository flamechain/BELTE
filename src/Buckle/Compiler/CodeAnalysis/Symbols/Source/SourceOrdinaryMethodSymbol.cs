using System.Collections.Immutable;
using Buckle.CodeAnalysis.Binding;
using Buckle.CodeAnalysis.Syntax;
using Buckle.CodeAnalysis.Text;
using Buckle.Diagnostics;

namespace Buckle.CodeAnalysis.Symbols;

internal abstract partial class SourceOrdinaryMethodSymbol : SourceOrdinaryMethodSymbolBase {
    private SourceOrdinaryMethodSymbol(
        NamedTypeSymbol containingType,
        string name,
        MethodDeclarationSyntax syntax,
        MethodKind methodKind,
        BelteDiagnosticQueue diagnostics)
        : base(
            containingType,
            name,
            syntax,
            MakeModifiersAndFlags(syntax, methodKind, diagnostics, out var hasExplicitAccessModifier)) {
        // TODO Eventually will want to have Symbol.CompilationAllowsUnsafe()
        // CheckLowlevelModifier(_modifiers, diagnostics);
        this.hasExplicitAccessModifier = hasExplicitAccessModifier;
        var hasAnyBody = syntax.body is not null;
        var location = syntax.identifier.location;

        if (hasAnyBody)
            CheckModifiersForBody(location, diagnostics);

        ModifierHelpers.CheckAccessibility(_modifiers, diagnostics, location);

        if (syntax.templateParameterList is null)
            ReportErrorIfHasConstraints(syntax.constraintClauseList, diagnostics);
    }

    internal bool hasExplicitAccessModifier { get; }

    private protected sealed override TextLocation _returnTypeLocation => GetSyntax().returnType.location;

    internal static SourceOrdinaryMethodSymbol CreateMethodSymbol(
        NamedTypeSymbol containingType,
        MethodDeclarationSyntax syntax,
        BelteDiagnosticQueue diagnostics) {
        var name = syntax.identifier.text;

        return syntax.templateParameterList is null
            ? new SourceSimpleOrdinaryMethodSymbol(containingType, name, syntax, MethodKind.Ordinary, diagnostics)
            : new SourceComplexOrdinaryMethodSymbol(containingType, name, syntax, MethodKind.Ordinary, diagnostics);
    }

    internal sealed override ExecutableCodeBinder TryGetBodyBinder(
        BinderFactory binderFactory = null,
        bool ignoreAccessibility = false) {
        return TryGetBodyBinderFromSyntax(binderFactory, ignoreAccessibility);
    }

    internal MethodDeclarationSyntax GetSyntax() {
        return (MethodDeclarationSyntax)syntaxNode;
    }

    private protected sealed override int GetParameterCountFromSyntax() {
        return GetSyntax().parameterList.parameters.Count;
    }

    private protected override void MethodChecks(BelteDiagnosticQueue diagnostics) {
        var (returnType, parameters, declaredConstraints) = MakeParametersAndBindReturnType(diagnostics);
        var overriddenMethod = MethodChecks(returnType, parameters, diagnostics);

        if (!declaredConstraints.IsDefault && overriddenMethod is not null) {
            // TODO
        }

        CheckModifiers(GetSyntax().identifier.location, diagnostics);
    }

    private static (DeclarationModifiers, Flags) MakeModifiersAndFlags(
        MethodDeclarationSyntax syntax,
        MethodKind methodKind,
        BelteDiagnosticQueue diagnostics,
        out bool hasExplicitAccessMod) {
        (var declarationModifiers, hasExplicitAccessMod) = MakeModifiers(syntax, diagnostics);

        var flags = new Flags(
            methodKind,
            syntax.returnType.GetRefKind(),
            declarationModifiers,
            false,
            false,
            syntax.body is not null
        );

        return (declarationModifiers, flags);
    }

    private static (DeclarationModifiers mods, bool hasExplicitAccessMod) MakeModifiers(
        MethodDeclarationSyntax syntax,
        BelteDiagnosticQueue diagnostics) {
        var defaultAccess = DeclarationModifiers.Private;

        var allowedModifiers = DeclarationModifiers.LowLevel
            | DeclarationModifiers.New
            | DeclarationModifiers.Sealed
            | DeclarationModifiers.Abstract
            | DeclarationModifiers.Static
            | DeclarationModifiers.Virtual
            | DeclarationModifiers.Const
            | DeclarationModifiers.AccessibilityMask
            | DeclarationModifiers.Override;

        bool hasExplicitAccessMod;
        var mods = MakeDeclarationModifiers(syntax, allowedModifiers, diagnostics);

        if ((mods & DeclarationModifiers.AccessibilityMask) == 0) {
            hasExplicitAccessMod = false;
            mods |= defaultAccess;
        } else {
            hasExplicitAccessMod = true;
        }

        return (mods, hasExplicitAccessMod);
    }

    private static DeclarationModifiers MakeDeclarationModifiers(
        MethodDeclarationSyntax syntax,
        DeclarationModifiers allowedModifiers,
        BelteDiagnosticQueue diagnostics) {
        return ModifierHelpers.CreateAndCheckNonTypeMemberModifiers(
            syntax.modifiers,
            DeclarationModifiers.None,
            allowedModifiers,
            syntax.identifier.location,
            diagnostics,
            out _
        );
    }

    private (TypeWithAnnotations returnType,
        ImmutableArray<ParameterSymbol> parameters,
        ImmutableArray<TypeParameterConstraintClause> declaredConstraints)
        MakeParametersAndBindReturnType(BelteDiagnosticQueue diagnostics) {
        var syntax = (MethodDeclarationSyntax)syntaxNode;
        var returnTypeSyntax = syntax.returnType;
        var withTemplateParametersBinder = declaringCompilation
            .GetBinderFactory(syntax.syntaxTree)
            .GetBinder(returnTypeSyntax, syntax, this);

        var signatureBinder = withTemplateParametersBinder.WithAdditionalFlagsAndContainingMember(
            BinderFlags.SuppressConstraintChecks,
            this
        );

        var parameters = ParameterHelpers.MakeParameters(
            signatureBinder,
            this,
            syntax.parameterList.parameters,
            diagnostics,
            true,
            isVirtual || isAbstract
        ).Cast<SourceParameterSymbol, ParameterSymbol>();

        returnTypeSyntax = returnTypeSyntax.SkipRef(out _);
        var returnType = signatureBinder.BindType(returnTypeSyntax, diagnostics);

        ImmutableArray<TypeParameterConstraintClause> declaredConstraints = default;

        if (arity != 0 && isOverride) {
            if (syntax.constraintClauseList.constraintClauses.Count > 0) {
                declaredConstraints = signatureBinder
                    .WithAdditionalFlags(BinderFlags.TemplateConstraintsClause | BinderFlags.SuppressConstraintChecks)
                    .BindTypeParameterConstraintClauses(
                        this,
                        templateParameters,
                        syntax.templateParameterList,
                        syntax.constraintClauseList.constraintClauses,
                        diagnostics
                    );
            }

            foreach (var parameter in parameters)
                ForceMethodTemplateParameters(parameter.typeWithAnnotations, this, declaredConstraints);

            ForceMethodTemplateParameters(returnType, this, declaredConstraints);
        }

        return (returnType, parameters, declaredConstraints);

        static void ForceMethodTemplateParameters(
            TypeWithAnnotations type,
            SourceOrdinaryMethodSymbol method,
            ImmutableArray<TypeParameterConstraintClause> declaredConstraints) {
            if (type.type is TemplateParameterSymbol t && (object)t.declaringMethod == method) {
                var asPrimitive = declaredConstraints.IsDefault ||
                    (declaredConstraints[t.ordinal].constraints & (TypeParameterConstraintKinds.Object)) == 0;

                // TODO Add this if Nullable<T> becomes the way to handle nullable types
                // type.TryForceResolve(asPrimitive);
            }
        }
    }

    private void CheckModifiers(TextLocation location, BelteDiagnosticQueue diagnostics) {
        // TODO Need to verify which of these are already handled by ModifierHelpers
        /*
        if (declaredAccessibility == Accessibility.Private && (isVirtual || isAbstract || isOverride)) {
            diagnostics.Add(ErrorCode.ERR_VirtualPrivate, location, this);
        } else if (isOverride && (IsNew || IsVirtual)) {
            // A member '{0}' marked as override cannot be marked as new or virtual
            diagnostics.Add(ErrorCode.ERR_OverrideNotNew, location, this);
        } else if (IsSealed && !IsOverride && !IsAbstract) {
            // '{0}' cannot be sealed because it is not an override
            diagnostics.Add(ErrorCode.ERR_SealedNonOverride, location, this);
        } else if (IsSealed && ContainingType.TypeKind == TypeKind.Struct) {
            // The modifier '{0}' is not valid for this item
            diagnostics.Add(ErrorCode.ERR_BadMemberFlag, location, SyntaxFacts.GetText(SyntaxKind.SealedKeyword));
        } else if (ReturnType.IsStatic) {
            // '{0}': static types cannot be used as return types
            diagnostics.Add(ErrorFacts.GetStaticClassReturnCode(ContainingType.IsInterfaceType()), location, ReturnType);
        } else if (IsAbstract && IsExtern) {
            diagnostics.Add(ErrorCode.ERR_AbstractAndExtern, location, this);
        } else if (IsAbstract && IsSealed && !isExplicitInterfaceImplementationInInterface) {
            diagnostics.Add(ErrorCode.ERR_AbstractAndSealed, location, this);
        } else if (IsAbstract && IsVirtual) {
            diagnostics.Add(ErrorCode.ERR_AbstractNotVirtual, location, this.Kind.Localize(), this);
        } else if (IsAbstract && ContainingType.TypeKind == TypeKind.Struct) {
            // The modifier '{0}' is not valid for this item
            diagnostics.Add(ErrorCode.ERR_BadMemberFlag, location, SyntaxFacts.GetText(SyntaxKind.AbstractKeyword));
        } else if (IsVirtual && ContainingType.TypeKind == TypeKind.Struct) {
            // The modifier '{0}' is not valid for this item
            diagnostics.Add(ErrorCode.ERR_BadMemberFlag, location, SyntaxFacts.GetText(SyntaxKind.VirtualKeyword));
        } else if (IsStatic && IsDeclaredReadOnly) {
            // Static member '{0}' cannot be marked 'readonly'.
            diagnostics.Add(ErrorCode.ERR_StaticMemberCantBeReadOnly, location, this);
        } else if (IsAbstract && !ContainingType.IsAbstract && (ContainingType.TypeKind == TypeKind.Class || ContainingType.TypeKind == TypeKind.Submission)) {
            // '{0}' is abstract but it is contained in non-abstract type '{1}'
            diagnostics.Add(ErrorCode.ERR_AbstractInConcreteClass, location, this, ContainingType);
        } else if (IsVirtual && ContainingType.IsSealed) {
            // '{0}' is a new virtual member in sealed type '{1}'
            diagnostics.Add(ErrorCode.ERR_NewVirtualInSealed, location, this, ContainingType);
        } else if (!HasAnyBody && IsAsync) {
            diagnostics.Add(ErrorCode.ERR_BadAsyncLacksBody, location);
        } else if (!HasAnyBody && !IsExtern && !IsAbstract && !IsPartial && !IsExpressionBodied) {
            diagnostics.Add(ErrorCode.ERR_ConcreteMissingBody, location, this);
        } else if (ContainingType.IsSealed && this.DeclaredAccessibility.HasProtected() && !this.IsOverride) {
            diagnostics.Add(AccessCheck.GetProtectedMemberInSealedTypeError(ContainingType), location, this);
        } else if (ContainingType.IsStatic && !IsStatic) {
            diagnostics.Add(ErrorCode.ERR_InstanceMemberInStaticClass, location, Name);
        } else if (isVararg && (IsGenericMethod || ContainingType.IsGenericType || Parameters.Length > 0 && Parameters[Parameters.Length - 1].IsParams)) {
            diagnostics.Add(ErrorCode.ERR_BadVarargs, location);
        } else if (isVararg && IsAsync) {
            diagnostics.Add(ErrorCode.ERR_VarargsAsync, location);
        }
        */
    }
}
