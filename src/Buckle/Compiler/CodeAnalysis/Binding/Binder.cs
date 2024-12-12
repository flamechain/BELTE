using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading;
using Buckle.CodeAnalysis.Lowering;
using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;
using Buckle.CodeAnalysis.Text;
using Buckle.Diagnostics;
using Buckle.Utilities;
using Microsoft.CodeAnalysis.PooledObjects;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Binds a <see cref="Syntax.InternalSyntax.LanguageParser" /> output into a immutable "bound" tree. This is where most
/// error checking happens. The <see cref="Lowerer" /> is also called here to simplify the code and convert control of
/// flow into gotos and labels. Dead code is also removed here, as well as other optimizations.
/// </summary>
internal partial class Binder {

    #region Internal Model

    private protected OverloadResolution _lazyOverloadResolution;
    private protected Conversions _lazyConversions;

    internal Binder(Compilation compilation) {
        flags = compilation.options.topLevelBinderFlags;
        this.compilation = compilation;
    }

    internal Binder(Binder next) {
        this.next = next;
        flags = next.flags;
        _lazyConversions = conversions;
    }

    private protected Binder(Binder next, BinderFlags flags) {
        this.next = next;
        this.flags = flags;
        compilation = next.compilation;
    }

    internal virtual SyntaxNode scopeDesignator => null;

    internal virtual bool isLocalFunctionsScopeBinder => false;

    internal virtual bool isLabelsScopeBinder => false;

    internal virtual Symbol containingMember => next.containingMember;

    internal virtual SynthesizedLabelSymbol breakLabel => next.breakLabel;

    internal virtual SynthesizedLabelSymbol continueLabel => next.continueLabel;

    internal virtual bool inMethod => next.inMethod;

    internal virtual DataContainerSymbol localInProgress => next.localInProgress;

    internal virtual ConstantFieldsInProgress constantFieldsInProgress => next.constantFieldsInProgress;

    internal virtual BoundExpression conditionalReceiverExpression => next.conditionalReceiverExpression;

    internal virtual ConsList<FieldSymbol> fieldsBeingBound => next.fieldsBeingBound;

    internal virtual ImmutableArray<DataContainerSymbol> locals => [];

    internal virtual ImmutableArray<LocalFunctionSymbol> localFunctions => [];

    internal virtual ImmutableArray<LabelSymbol> labels => [];

    internal NamedTypeSymbol containingType => containingMember switch {
        null => null,
        NamedTypeSymbol namedType => namedType,
        _ => containingMember.containingType
    };

    internal Compilation compilation { get; }

    internal BinderFlags flags { get; }

    internal Conversions conversions {
        get {
            if (_lazyConversions is null)
                Interlocked.CompareExchange(ref _lazyConversions, new Conversions(this), null);

            return _lazyConversions;
        }
    }

    internal OverloadResolution overloadResolution {
        get {
            if (_lazyOverloadResolution is null)
                Interlocked.CompareExchange(ref _lazyOverloadResolution, new OverloadResolution(this), null);

            return _lazyOverloadResolution;
        }
    }

    internal Binder next { get; }

    private protected virtual bool _inExecutableBinder => false;

    internal virtual Binder GetBinder(SyntaxNode node) {
        return next.GetBinder(node);
    }

    internal virtual ImmutableArray<DataContainerSymbol> GetDeclaredLocalsForScope(SyntaxNode scopeDesignator) {
        return next.GetDeclaredLocalsForScope(scopeDesignator);
    }

    internal virtual ImmutableArray<LocalFunctionSymbol> GetDeclaredLocalFunctionsForScope(
        BelteSyntaxNode scopeDesignator) {
        return next.GetDeclaredLocalFunctionsForScope(scopeDesignator);
    }

    internal virtual BoundForStatement BindForParts(BelteDiagnosticQueue diagnostics, Binder originalBinder) {
        return next.BindForParts(diagnostics, originalBinder);
    }

    internal virtual BoundWhileStatement BindWhileParts(BelteDiagnosticQueue diagnostics, Binder originalBinder) {
        return next.BindWhileParts(diagnostics, originalBinder);
    }

    internal virtual BoundDoWhileStatement BindDoWhileParts(BelteDiagnosticQueue diagnostics, Binder originalBinder) {
        return next.BindDoWhileParts(diagnostics, originalBinder);
    }

    private protected virtual SourceDataContainerSymbol LookupLocal(SyntaxToken identifier) {
        return next.LookupLocal(identifier);
    }

    private protected virtual LocalFunctionSymbol LookupLocalFunction(SyntaxToken identifier) {
        return next.LookupLocalFunction(identifier);
    }

    private protected virtual bool IsUnboundTypeAllowed(TemplateNameSyntax syntax) {
        return next.IsUnboundTypeAllowed(syntax);
    }

    private bool IsSymbolAccessible(Symbol symbol, NamedTypeSymbol within, TypeSymbol throughType = null) {
        return flags.Includes(BinderFlags.IgnoreAccessibility) ||
            AccessCheck.IsSymbolAccessible(symbol, within, throughType);
    }

    private bool IsSymbolAccessible(
        Symbol symbol,
        NamedTypeSymbol within,
        TypeSymbol throughType,
        out bool failedThroughTypeCheck) {
        if (flags.Includes(BinderFlags.IgnoreAccessibility)) {
            failedThroughTypeCheck = false;
            return true;
        }

        return AccessCheck.IsSymbolAccessible(
            symbol,
            within,
            throughType,
            out failedThroughTypeCheck
        );
    }

    internal Binder WithAdditionalFlags(BinderFlags flags) {
        return this.flags.Includes(flags)
            ? this
            : new Binder(this, this.flags | flags);
    }

    internal Binder WithContainingMember(Symbol containing) {
        return new BinderWithContainingMember(this, containing);
    }

    internal Binder WithAdditionalFlagsAndContainingMember(BinderFlags flags, Symbol containing) {
        return this.flags.Includes(flags)
            ? new BinderWithContainingMember(this, containing)
            : new BinderWithContainingMember(this, this.flags | flags, containing);
    }

    internal BoundExpression WrapWithVariablesIfAny(BelteSyntaxNode scopeDesignator, BoundExpression expression) {
        var locals = GetDeclaredLocalsForScope(scopeDesignator);

        // TODO What is BoundSequence
        // return locals.IsEmpty
        //     ? expression
        //     : new BoundSequence(scopeDesignator, locals, ImmutableArray<BoundExpression>.Empty, expression, getType()) { WasCompilerGenerated = true };
        return expression;
    }

    internal BoundStatement WrapWithVariablesIfAny(BelteSyntaxNode scopeDesignator, BoundStatement statement) {
        var locals = GetDeclaredLocalsForScope(scopeDesignator);

        if (locals.IsEmpty)
            return statement;

        return new BoundBlockStatement([statement], locals, []);
    }

    #endregion

    #region Constraints

    internal ImmutableArray<TypeParameterConstraintClause> GetDefaultTypeParameterConstraintClauses(
        TemplateParameterListSyntax templateParameterList) {
        var builder = ArrayBuilder<TypeParameterConstraintClause>.GetInstance(
            templateParameterList.parameters.Count,
            GetDefaultTypeParameterConstraintClause()
        );

        return builder.ToImmutable();
    }

    internal TypeParameterConstraintClause GetDefaultTypeParameterConstraintClause() {
        return TypeParameterConstraintClause.Empty;
    }

    internal ImmutableArray<TypeParameterConstraintClause> BindTypeParameterConstraintClauses(
        Symbol containingSymbol,
        ImmutableArray<TemplateParameterSymbol> templateParameters,
        TemplateParameterListSyntax templateParameterList,
        SyntaxList<TemplateConstraintClauseSyntax> clauses,
        BelteDiagnosticQueue diagnostics) {
        var n = templateParameters.Length;
        var names = new Dictionary<string, int>(n, StringOrdinalComparer.Instance);

        foreach (var templateParameter in templateParameters) {
            var name = templateParameter.name;

            if (!names.ContainsKey(name))
                names.Add(name, names.Count);
        }

        var results = ArrayBuilder<TypeParameterConstraintClause>.GetInstance(n, fillWithValue: null);
        var syntaxNodes = ArrayBuilder<ArrayBuilder<TemplateConstraintClauseSyntax>>
            .GetInstance(n, fillWithValue: null);

        foreach (var clause in clauses) {
            if (clause.expressionConstraint is not null)
                continue;

            var name = clause.extendConstraint is null
                ? clause.isConstraint.name.identifier
                : clause.extendConstraint.name.identifier;

            if (names.TryGetValue(name.text, out var ordinal)) {
                if (syntaxNodes[ordinal] is null)
                    syntaxNodes[ordinal] = ArrayBuilder<TemplateConstraintClauseSyntax>.GetInstance();

                syntaxNodes[ordinal].Add(clause);
            } else {
                diagnostics.Push(Error.UnknownTemplate(name.location, containingSymbol.name, name.text));
            }
        }

        foreach (var parameter in templateParameters) {
            names.TryGetValue(parameter.name, out var ordinal);

            if (syntaxNodes[ordinal] is not null) {
                var constraintClause = BindTypeParameterConstraints(templateParameterList.parameters[ordinal], syntaxNodes[ordinal], diagnostics);
                results[ordinal] = constraintClause;
            }
        }

        for (var i = 0; i < n; i++) {
            if (results[i] is null)
                results[i] = GetDefaultTypeParameterConstraintClause();
        }

        foreach (var typeConstraintsSyntaxes in syntaxNodes)
            typeConstraintsSyntaxes?.Free();

        syntaxNodes.Free();

        return results.ToImmutableAndFree();
    }

    private TypeParameterConstraintClause BindTypeParameterConstraints(
        ParameterSyntax templateParameter,
        ArrayBuilder<TemplateConstraintClauseSyntax> constraints,
        BelteDiagnosticQueue diagnostics) {
        // TODO
        // Need to bind type first to make sure this template is a type parameter
        return null;
    }

    #endregion

    #region Symbols

    internal TypeWithAnnotations BindType(
        ExpressionSyntax syntax,
        BelteDiagnosticQueue diagnostics,
        ConsList<TypeSymbol> basesBeingResolved = null) {
        // TODO
        return null;
    }

    internal TypeWithAnnotations BindTypeOrImplicitType(
        TypeSyntax syntax,
        BelteDiagnosticQueue diagnostics,
        out bool isImplicitlyTyped) {
        // TODO
        isImplicitlyTyped = false;
        return null;
    }

    internal NamedTypeSymbol CreateErrorType(string name = "") {
        return new ExtendedErrorTypeSymbol(compilation, name, 0, null);
    }

    #endregion

    #region Expressions

    internal BoundExpression BindExpression(ExpressionSyntax node, BelteDiagnosticQueue diagnostics) {
        return BindExpressionInternal(node, diagnostics, false, false);
    }

    internal BoundExpression BindValue(
        ExpressionSyntax node,
        BelteDiagnosticQueue diagnostics,
        BindValueKind valueKind) {
        var result = BindExpressionInternal(node, diagnostics, false, false);
        return CheckValue(result, valueKind, diagnostics);
    }

    internal BoundExpression BindDataContainerInitializerValue(
        EqualsValueClauseSyntax initializer,
        RefKind refKind,
        TypeSymbol varType,
        BelteDiagnosticQueue diagnostics) {
        if (initializer is null)
            return null;

        // IsInitializerRefKindValid(initializer, initializer, refKind, diagnostics, out var valueKind, out var value);
        // BoundExpression boundInitializer = BindPossibleArrayInitializer(value, varType, valueKind, diagnostics);
        // boundInitializer = GenerateConversionForAssignment(varType, boundInitializer, diagnostics);
        // return boundInitializer;
        return null;
    }

    internal BoundExpression BindInferredDataContainerInitializer(
        BelteDiagnosticQueue diagnostics,
        RefKind refKind,
        EqualsValueClauseSyntax initializer,
        BelteSyntaxNode errorSyntax) {
        // IsInitializerRefKindValid(initializer, initializer, refKind, diagnostics, out var valueKind, out var value);
        // return BindInferredVariableInitializer(diagnostics, value, valueKind, errorSyntax);
        return null;
    }

    internal Binder CreateBinderForParameterDefaultValue(Symbol parameter, EqualsValueClauseSyntax defaultValueSyntax) {
        var binder = new LocalScopeBinder(
            WithAdditionalFlagsAndContainingMember(BinderFlags.ParameterDefaultValue, parameter.containingSymbol)
        );

        return new ExecutableCodeBinder(defaultValueSyntax, parameter.containingSymbol, binder);
    }

    internal BoundExpression BindConstructorInitializer(
        ArgumentListSyntax initializerArgumentList,
        MethodSymbol constructor,
        BelteDiagnosticQueue diagnostics) {
        Binder argumentListBinder = null;

        if (initializerArgumentList is not null)
            argumentListBinder = GetBinder(initializerArgumentList);

        var result = (argumentListBinder ?? this)
            .BindConstructorInitializerCore(initializerArgumentList, constructor, diagnostics);

        if (argumentListBinder is not null) {
            result = argumentListBinder.WrapWithVariablesIfAny(initializerArgumentList, result);
        }

        return result;
    }

    internal BoundEqualsValue BindParameterDefaultValue(
        EqualsValueClauseSyntax defaultValueSyntax,
        Symbol parameter,
        BelteDiagnosticQueue diagnostics,
        out BoundExpression valueBeforeConversion) {
        var defaultValueBinder = GetBinder(defaultValueSyntax);
        valueBeforeConversion = defaultValueBinder.BindValue(
            defaultValueSyntax.value,
            diagnostics,
            BindValueKind.RValue
        );

        var isTemplate = parameter is TemplateParameterSymbol;

        var parameterType = parameter is ParameterSymbol p
            ? p.type
            : (parameter as TemplateParameterSymbol).underlyingType.type;

        var locals = defaultValueBinder.GetDeclaredLocalsForScope(defaultValueSyntax);
        var value = defaultValueBinder.GenerateConversionForAssignment(
            parameterType,
            valueBeforeConversion,
            diagnostics,
            ConversionForAssignmentFlags.DefaultParameter
        );

        if (isTemplate)
            return new BoundTemplateParameterEqualsValue((TemplateParameterSymbol)parameter, locals, value);
        else
            return new BoundParameterEqualsValue((ParameterSymbol)parameter, locals, value);
    }

    internal BoundFieldEqualsValue BindFieldInitializer(
        FieldSymbol field,
        EqualsValueClauseSyntax initializer,
        BelteDiagnosticQueue diagnostics) {
        if (initializer is null)
            return null;

        var initializerBinder = GetBinder(initializer);
        var result = initializerBinder.BindVariableOrAutoPropInitializerValue(
            initializer,
            field.refKind,
            field.GetFieldType(initializerBinder.fieldsBeingBound).type,
            diagnostics
        );

        return new BoundFieldEqualsValue(field, initializerBinder.GetDeclaredLocalsForScope(initializer), result);
    }

    internal BoundExpression BindVariableOrAutoPropInitializerValue(
        EqualsValueClauseSyntax initializerOpt,
        RefKind refKind,
        TypeSymbol varType,
        BelteDiagnosticQueue diagnostics) {
        if (initializerOpt is null)
            return null;

        IsInitializerRefKindValid(initializerOpt, initializerOpt, refKind, diagnostics, out var valueKind, out var value);
        var initializer = BindPossibleArrayInitializer(value, varType, valueKind, diagnostics);
        initializer = GenerateConversionForAssignment(varType, initializer, diagnostics);
        return initializer;
    }

    internal static BoundCallExpression GenerateBaseParameterlessConstructorInitializer(
        MethodSymbol constructor,
        BelteDiagnosticQueue diagnostics) {
        var containingType = constructor.containingType;
        var baseType = containingType.baseType;
        MethodSymbol baseConstructor = null;
        var resultKind = LookupResultKind.Viable;
        var errorLocation = constructor.location;

        foreach (var ctor in baseType.constructors) {
            if (ctor.parameterCount == 0) {
                baseConstructor = ctor;
                break;
            }
        }

        if (!AccessCheck.IsSymbolAccessible(baseConstructor, containingType)) {
            // TODO bad access error
            resultKind = LookupResultKind.Inaccessible;
        }

        var receiver = new BoundThisExpression(containingType);

        return new BoundCallExpression(receiver, baseConstructor, []);
    }

    private static bool IsInitializerRefKindValid(
        EqualsValueClauseSyntax initializer,
        BelteSyntaxNode node,
        RefKind variableRefKind,
        BelteDiagnosticQueue diagnostics,
        out BindValueKind valueKind,
        out ExpressionSyntax value) {
        var expressionRefKind = RefKind.None;
        value = initializer?.value.UnwrapRefExpression(out expressionRefKind);

        if (variableRefKind == RefKind.None) {
            valueKind = BindValueKind.RValue;
            if (expressionRefKind == RefKind.Ref) {
                // Error(diagnostics, ErrorCode.ERR_InitializeByValueVariableWithReference, node);
                // TODO error
                return false;
            }
        } else {
            valueKind = variableRefKind == RefKind.RefConst
                ? BindValueKind.RefConst
                : BindValueKind.RefOrOut;

            if (initializer == null) {
                // Error(diagnostics, ErrorCode.ERR_ByReferenceVariableMustBeInitialized, node);
                // TODO error
                return false;
            } else if (expressionRefKind != RefKind.Ref) {
                // Error(diagnostics, ErrorCode.ERR_InitializeByReferenceVariableWithValue, node);
                // TODO error
                return false;
            }
        }

        return true;
    }

    private BoundExpression BindPossibleArrayInitializer(
        ExpressionSyntax node,
        TypeSymbol destinationType,
        BindValueKind valueKind,
        BelteDiagnosticQueue diagnostics) {
        if (node.kind != SyntaxKind.InitializerListExpression)
            return BindValue(node, diagnostics, valueKind);

        BoundExpression result;
        if (destinationType.kind == SymbolKind.ArrayType) {
            // result = BindArrayCreationWithInitializer(diagnostics, null,
            //     (InitializerExpressionSyntax)node, (ArrayTypeSymbol)destinationType,
            //     ImmutableArray<BoundExpression>.Empty);

            // TODO
            result = null;
        } else {
            // result = BindUnexpectedArrayInitializer((InitializerExpressionSyntax)node, diagnostics, ErrorCode.ERR_ArrayInitToNonArrayType);
            result = null;
        }

        return CheckValue(result, valueKind, diagnostics);
    }

    private BoundExpression CheckValue(
        BoundExpression expression,
        BindValueKind kind,
        BelteDiagnosticQueue diagnostics) {
        // TODO
        return null;
    }

    private BoundExpression BindConstructorInitializerCore(
        ArgumentListSyntax initializerArgumentListOpt,
        MethodSymbol constructor,
        BelteDiagnosticQueue diagnostics) {
        // TODO
        return null;
    }

    private BoundExpression BindExpressionInternal(
        ExpressionSyntax node,
        BelteDiagnosticQueue diagnostics,
        bool called,
        bool indexed) {
        switch (node.kind) {
            /*
            case SyntaxKind.LiteralExpression:
                return BindLiteralExpression((LiteralExpressionSyntax)expression);
            case SyntaxKind.InitializerListExpression:
                return BindInitializerListExpression((InitializerListExpressionSyntax)expression, initializerListType);
            case SyntaxKind.InitializerDictionaryExpression:
                return BindInitializerDictionaryExpression((InitializerDictionaryExpressionSyntax)expression);
            case SyntaxKind.UnaryExpression:
                return BindUnaryExpression((UnaryExpressionSyntax)expression);
            case SyntaxKind.BinaryExpression:
                return BindBinaryExpression((BinaryExpressionSyntax)expression);
            case SyntaxKind.TernaryExpression:
                return BindTernaryExpression((TernaryExpressionSyntax)expression);
            case SyntaxKind.ParenthesizedExpression:
                return BindParenExpression((ParenthesisExpressionSyntax)expression);
            case SyntaxKind.AssignmentExpression:
                return BindAssignmentExpression((AssignmentExpressionSyntax)expression);
            case SyntaxKind.CallExpression:
                return BindCallExpression((CallExpressionSyntax)expression);
            case SyntaxKind.IndexExpression:
                return BindIndexExpression((IndexExpressionSyntax)expression);
            case SyntaxKind.EmptyExpression:
                return BindEmptyExpression((EmptyExpressionSyntax)expression);
            case SyntaxKind.PostfixExpression:
                return BindPostfixExpression((PostfixExpressionSyntax)expression, ownStatement);
            case SyntaxKind.PrefixExpression:
                return BindPrefixExpression((PrefixExpressionSyntax)expression);
            case SyntaxKind.ReferenceExpression:
                return BindReferenceExpression((ReferenceExpressionSyntax)expression);
            case SyntaxKind.CastExpression:
                return BindCastExpression((CastExpressionSyntax)expression);
            case SyntaxKind.TypeOfExpression:
                return BindTypeOfExpression((TypeOfExpressionSyntax)expression);
            case SyntaxKind.NameOfExpression:
                return BindNameOfExpression((NameOfExpressionSyntax)expression);
            case SyntaxKind.ObjectCreationExpression:
                return BindObjectCreationExpression((ObjectCreationExpressionSyntax)expression);
            case SyntaxKind.ThisExpression:
                return BindThisExpression((ThisExpressionSyntax)expression);
            case SyntaxKind.BaseExpression:
                return BindBaseExpression((BaseExpressionSyntax)expression);
            case SyntaxKind.ThrowExpression:
                return BindThrowExpression((ThrowExpressionSyntax)expression);
            case SyntaxKind.TemplateName:
            case SyntaxKind.IdentifierName:
                return BindIdentifier((SimpleNameSyntax)expression, called, allowTypes);
            case SyntaxKind.MemberAccessExpression:
                return BindMemberAccessExpression((MemberAccessExpressionSyntax)expression, called);
            case SyntaxKind.QualifiedName:
                return BindQualifiedName((QualifiedNameSyntax)expression, called);
            case SyntaxKind.NonNullableType when allowTypes:
            case SyntaxKind.ReferenceType when allowTypes:
            case SyntaxKind.ArrayType when allowTypes:
                return BindType((TypeSyntax)expression, explicitly: true);
                */
            default:
                throw new BelteInternalException($"BindExpressionInternal: unexpected syntax '{node.kind}'");
        }
    }

    #endregion

    #region Lookup

    internal virtual void LookupSymbolsInSingleBinder(
        LookupResult result,
        string name,
        int arity,
        ConsList<TypeSymbol> basesBeingResolved,
        LookupOptions options,
        Binder originalBinder,
        bool diagnose) { }

    internal virtual void AddLookupSymbolsInfoInSingleBinder(
        LookupSymbolsInfo info,
        LookupOptions options,
        Binder originalBinder) { }

    internal bool CanAddLookupSymbolInfo(
        Symbol symbol,
        LookupOptions options,
        LookupSymbolsInfo info,
        TypeSymbol accessThroughType) {
        var name = symbol.name;

        if (!info.CanBeAdded(name))
            return false;

        if ((options & LookupOptions.NamespacesOrTypesOnly) != 0 && symbol is not NamespaceOrTypeSymbol) {
            return false;
        } else if ((options & LookupOptions.MustBeInvocableIfMember) != 0 && IsNonInvocableMember(symbol)) {
            return false;
        } else if (!IsAccessible(symbol, RefineAccessThroughType(options, accessThroughType))) {
            return false;
        } else if (!IsInScopeOfAssociatedSyntaxTree(symbol)) {
            return false;
        } else if ((options & LookupOptions.MustBeInstance) != 0 && !IsInstance(symbol)) {
            return false;
        } else if ((options & LookupOptions.MustNotBeInstance) != 0 && IsInstance(symbol)) {
            return false;
        } else if ((options & LookupOptions.MustNotBeNamespace) != 0 && (symbol.kind == SymbolKind.Namespace)) {
            return false;
        } else {
            return true;
        }
    }

    internal SingleLookupResult CheckViability(
        Symbol symbol,
        int arity,
        LookupOptions options,
        TypeSymbol accessThroughType,
        bool diagnose,
        ConsList<TypeSymbol> basesBeingResolved = null) {
        // TODO
        return default;
    }

    internal bool IsNonInvocableMember(Symbol symbol) {
        switch (symbol.kind) {
            case SymbolKind.Method:
            case SymbolKind.Field:
            case SymbolKind.NamedType:
                return !IsInvocableMember(symbol);
            default:
                return false;
        }
    }

    internal bool IsAccessible(
        Symbol symbol,
        TypeSymbol accessThroughType = null,
        ConsList<TypeSymbol> basesBeingResolved = null) {
        return IsAccessible(symbol, accessThroughType, out _, basesBeingResolved);
    }

    internal bool IsAccessible(
        Symbol symbol,
        TypeSymbol accessThroughType,
        out bool failedThroughTypeCheck,
        ConsList<TypeSymbol> basesBeingResolved = null) {
        if (flags.Includes(BinderFlags.IgnoreAccessibility)) {
            failedThroughTypeCheck = false;
            return true;
        }

        return IsAccessibleHelper(symbol, accessThroughType, out failedThroughTypeCheck);
    }

    internal virtual bool IsAccessibleHelper(
        Symbol symbol,
        TypeSymbol accessThroughType,
        out bool failedThroughTypeCheck) {
        return next.IsAccessibleHelper(symbol, accessThroughType, out failedThroughTypeCheck);
    }

    internal static bool IsSymbolAccessibleConditional(Symbol symbol, Symbol within) {
        return AccessCheck.IsSymbolAccessible(symbol, within);
    }

    internal bool IsSymbolAccessibleConditional(
        Symbol symbol,
        NamedTypeSymbol within,
        TypeSymbol throughTypeOpt,
        out bool failedThroughTypeCheck) {
        if (flags.Includes(BinderFlags.IgnoreAccessibility)) {
            failedThroughTypeCheck = false;
            return true;
        }

        return AccessCheck.IsSymbolAccessible(symbol, within, throughTypeOpt, out failedThroughTypeCheck);
    }

    private bool IsInvocableMember(Symbol symbol) {
        switch (symbol.kind) {
            case SymbolKind.Method:
                return true;
        }

        return false;
    }

    private static bool IsInstance(Symbol symbol) {
        switch (symbol.kind) {
            case SymbolKind.Field:
            case SymbolKind.Method:
                return symbol.RequiresInstanceReceiver();
            default:
                return false;
        }
    }

    private static TypeSymbol RefineAccessThroughType(LookupOptions options, TypeSymbol accessThroughType) {
        return ((options & LookupOptions.UseBaseReferenceAccessibility) != 0)
            ? null
            : accessThroughType;
    }

    private bool IsInScopeOfAssociatedSyntaxTree(Symbol symbol) {
        while (symbol is not null)
            symbol = symbol.containingType;

        if (symbol is null)
            return true;

        if ((object)symbol.declaringCompilation != compilation)
            return false;

        // TODO Is checking compilation good enough?
        // Might want to create a system of FileIdentifier s instead of comparing texts

        var symbolText = symbol.syntaxReference?.syntaxTree?.text;

        if (symbolText is null)
            return false;

        var binderText = GetEndText();

        return binderText == symbolText;

        SourceText GetEndText() {
            for (var binder = this; binder is not null; binder = binder.next) {
                if (binder is EndBinder lastBinder)
                    return lastBinder.associatedText;
            }

            throw ExceptionUtilities.Unreachable();
        }
    }

    #endregion

    #region Statements

    internal virtual BoundStatement BindStatement(StatementSyntax node, BelteDiagnosticQueue diagnostics) {
        switch (node.kind) {
            /*
            case SyntaxKind.BlockStatement:
                return BindBlockStatement((BlockStatementSyntax)syntax);
            case SyntaxKind.ExpressionStatement:
                return BindExpressionStatement((ExpressionStatementSyntax)syntax);
            case SyntaxKind.LocalDeclarationStatement:
                var statement = BindLocalDeclarationStatement((LocalDeclarationStatementSyntax)syntax);
                _shadowingVariable = null;
                return statement;
            case SyntaxKind.IfStatement:
                return BindIfStatement((IfStatementSyntax)syntax);
            case SyntaxKind.WhileStatement:
                return BindWhileStatement((WhileStatementSyntax)syntax);
            case SyntaxKind.ForStatement:
                return BindForStatement((ForStatementSyntax)syntax);
            case SyntaxKind.DoWhileStatement:
                return BindDoWhileStatement((DoWhileStatementSyntax)syntax);
            case SyntaxKind.TryStatement:
                return BindTryStatement((TryStatementSyntax)syntax);
            case SyntaxKind.BreakStatement:
                return BindBreakStatement((BreakStatementSyntax)syntax);
            case SyntaxKind.ContinueStatement:
                return BindContinueStatement((ContinueStatementSyntax)syntax);
            case SyntaxKind.ReturnStatement:
                return BindReturnStatement((ReturnStatementSyntax)syntax);
            case SyntaxKind.LocalFunctionStatement:
                return new BoundBlockStatement([]);
            */
            default:
                throw new BelteInternalException($"BindStatementInternal: unexpected syntax '{node.kind}'");
        }
    }

    internal BoundExpression GenerateConversionForAssignment(
        TypeSymbol targetType,
        BoundExpression expression,
        BelteDiagnosticQueue diagnostics,
        ConversionForAssignmentFlags flags = ConversionForAssignmentFlags.None) {
        return GenerateConversionForAssignment(targetType, expression, diagnostics, out _, flags);
    }

    internal BoundExpression GenerateConversionForAssignment(
        TypeSymbol targetType,
        BoundExpression expression,
        BelteDiagnosticQueue diagnostics,
        out Conversion conversion,
        ConversionForAssignmentFlags flags = ConversionForAssignmentFlags.None) {
        if (expression is BoundErrorExpression)
            diagnostics = BelteDiagnosticQueue.Discarded;

        conversion = (flags & ConversionForAssignmentFlags.IncrementAssignment) == 0
            ? conversions.ClassifyConversionFromExpression(expression, targetType)
            : conversions.ClassifyConversionFromType(expression.type, targetType);

        if ((flags & ConversionForAssignmentFlags.RefAssignment) != 0) {
            if (conversion.kind != ConversionKind.Identity) {
                // Error(diagnostics, ErrorCode.ERR_RefAssignmentMustHaveIdentityConversion, expression.Syntax, targetType);
                // TODO
            } else {
                return expression;
            }
        } else if (!conversion.exists ||
              ((flags & ConversionForAssignmentFlags.CompoundAssignment) == 0
                ? !conversion.isImplicit
                : (conversion.isExplicit && (flags & ConversionForAssignmentFlags.PredefinedOperator) == 0))) {
            if ((flags & ConversionForAssignmentFlags.DefaultParameter) == 0) {
                // GenerateImplicitConversionError(diagnostics, expression.syntax, conversion, expression, targetType);
                // TODO
            }

            diagnostics = BelteDiagnosticQueue.Discarded;
        }

        return new BoundCastExpression(targetType, expression, conversion.kind, null);
        // return CreateConversion(expression.Syntax, expression, conversion, isCast: false, conversionGroupOpt: null, targetType, diagnostics);
        // TODO Consider this helper method
    }

    internal virtual BoundNode BindMethodBody(BelteSyntaxNode syntax, BelteDiagnosticQueue diagnostics) {
        switch (syntax) {
            case BaseMethodDeclarationSyntax method:
                if (method.kind == SyntaxKind.ConstructorDeclaration)
                    return BindConstructorBody((ConstructorDeclarationSyntax)method, diagnostics);

                return BindMethodBody(method, method.body, diagnostics);
            default:
                throw ExceptionUtilities.UnexpectedValue(syntax.kind);
        }
    }

    private BoundNode BindConstructorBody(ConstructorDeclarationSyntax constructor, BelteDiagnosticQueue diagnostics) {
        var initializer = constructor.constructorInitializer;
        var bodyBinder = GetBinder(constructor);

        var initializerCall = initializer is null
            ? bodyBinder.BindImplicitConstructorInitializer(constructor, diagnostics)
            : bodyBinder.BindConstructorInitializer(initializer, diagnostics);

        var body = (BoundBlockStatement)bodyBinder.BindStatement(constructor.body, diagnostics);
        var locals = bodyBinder.GetDeclaredLocalsForScope(constructor);

        return new BoundConstructorMethodBody(locals, initializerCall, body);
    }

    private BoundNode BindMethodBody(
        BelteSyntaxNode declaration,
        BlockStatementSyntax body,
        BelteDiagnosticQueue diagnostics) {
        if (body is null)
            return null;

        return new BoundNonConstructorMethodBody((BoundBlockStatement)BindStatement(body, diagnostics));
    }

    #endregion

    #region Initializers

    internal static void BindFieldInitializers(
        Compilation compilation,
        ImmutableArray<ImmutableArray<FieldInitializer>> fieldInitializers,
        BelteDiagnosticQueue diagnostics,
        ref ProcessedFieldInitializers processedInitializers) {
        // TODO
    }

    internal BoundExpressionStatement BindImplicitConstructorInitializer(
        SyntaxNode syntax,
        BelteDiagnosticQueue diagnostics) {
        var call = BindImplicitConstructorInitializer((MethodSymbol)containingMember, diagnostics, compilation);
        return new BoundExpressionStatement(call);
    }

    internal static BoundExpression BindImplicitConstructorInitializer(
        MethodSymbol constructor,
        BelteDiagnosticQueue diagnostics,
        Compilation compilation
        ) {
        // TODO
        return null;
    }

    internal virtual BoundExpressionStatement BindConstructorInitializer(
        ConstructorInitializerSyntax initializer,
        BelteDiagnosticQueue diagnostics) {
        var call = GetBinder(initializer)
            .BindConstructorInitializer(initializer.argumentList, (MethodSymbol)containingMember, diagnostics);

        return new BoundExpressionStatement(call);
    }

    #endregion
}
