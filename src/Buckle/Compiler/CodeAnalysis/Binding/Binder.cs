using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using Buckle.CodeAnalysis.Lowering;
using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;
using Buckle.CodeAnalysis.Text;
using Buckle.Diagnostics;
using Buckle.Libraries;
using Buckle.Utilities;
using Diagnostics;
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
        compilation = next.compilation;
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

    internal virtual bool isInMethodBody => next.isInMethodBody;

    internal virtual bool isNestedFunctionBinder => false;

    internal virtual bool isInsideNameof => next.isInsideNameof;

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

    private protected virtual SyntaxNode _enclosingNameofArgument => next._enclosingNameofArgument;

    private protected virtual bool _inExecutableBinder => false;

    private protected bool _inConstructorInitializer => flags.Includes(BinderFlags.ConstructorInitializer);

    internal bool inFieldInitializer => flags.Includes(BinderFlags.FieldInitializer);

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
                var constraintClause = BindTypeParameterConstraints(
                    templateParameterList.parameters[ordinal],
                    syntaxNodes[ordinal],
                    diagnostics
                );

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
        // TODO templates
        // Need to bind type first to make sure this template is a type parameter
        return null;
    }

    #endregion

    #region Symbols

    internal TypeWithAnnotations BindType(
        ExpressionSyntax syntax,
        BelteDiagnosticQueue diagnostics,
        ConsList<TypeSymbol> basesBeingResolved = null) {
        TypeWithAnnotations nonNullableType;

        switch (syntax.kind) {
            case SyntaxKind.NonNullableType:
                return BindNonNullable();
            case SyntaxKind.IdentifierName:
                nonNullableType = BindNonTemplateSimpleType(
                    (IdentifierNameSyntax)syntax,
                    diagnostics,
                    basesBeingResolved,
                    null
                );

                break;
            case SyntaxKind.TemplateName:
                nonNullableType = BindTemplateSimpleType(
                    (TemplateNameSyntax)syntax,
                    diagnostics,
                    basesBeingResolved,
                    null
                );

                break;
            case SyntaxKind.QualifiedName: {
                    var node = (QualifiedNameSyntax)syntax;
                    nonNullableType = BindQualifiedName(node.left, node.right, diagnostics, basesBeingResolved);
                    break;
                }
            case SyntaxKind.MemberAccessExpression: {
                    var node = (MemberAccessExpressionSyntax)syntax;
                    nonNullableType = BindQualifiedName(node.expression, node.name, diagnostics, basesBeingResolved);
                    break;
                }
            case SyntaxKind.ArrayType:
                nonNullableType = BindArrayType(
                    (ArrayTypeSyntax)syntax,
                    diagnostics,
                    false,
                    basesBeingResolved
                );

                break;
            case SyntaxKind.ReferenceType: {
                    var referenceTypeSyntax = (ReferenceTypeSyntax)syntax;
                    var refToken = referenceTypeSyntax.refKeyword;
                    diagnostics.Push(Error.UnexpectedToken(refToken.location, refToken.kind));
                    return BindType(referenceTypeSyntax.type, diagnostics, basesBeingResolved);
                }
            default:
                return new TypeWithAnnotations(CreateErrorType());
        }

        if (nonNullableType.specialType == SpecialType.Void)
            return nonNullableType;

        return nonNullableType.SetIsAnnotated();

        TypeWithAnnotations BindNonNullable() {
            var nonNullableSyntax = (NonNullableTypeSyntax)syntax;
            var nullableType = BindType(nonNullableSyntax.type, diagnostics, basesBeingResolved);
            return new TypeWithAnnotations(nullableType.type.GetNullableUnderlyingType(), false);
        }
    }

    private NamespaceOrTypeSymbol GetContainingNamespaceOrType(Symbol symbol) {
        return symbol.ContainingNamespaceOrType() ?? compilation.globalNamespaceInternal;
    }

    private BestSymbolInfo GetBestSymbolInfo(ArrayBuilder<Symbol> symbols, out BestSymbolInfo secondBest) {
        var first = default(BestSymbolInfo);
        var second = default(BestSymbolInfo);

        for (var i = 0; i < symbols.Count; i++) {
            var symbol = symbols[i];
            BestSymbolLocation location;

            if (symbol.kind == SymbolKind.Namespace) {
                location = BestSymbolLocation.None;
                var ns = (NamespaceSymbol)symbol;

                var current = GetLocation(compilation, ns);

                if (BestSymbolInfo.IsSecondLocationBetter(location, current)) {
                    location = current;

                    if (location == BestSymbolLocation.FromSourceModule)
                        break;
                }
            } else {
                location = GetLocation(compilation, symbol);
            }

            var third = new BestSymbolInfo(location, i);

            if (BestSymbolInfo.Sort(ref second, ref third))
                BestSymbolInfo.Sort(ref first, ref second);
        }

        secondBest = second;

        return first;
    }

    private static BestSymbolLocation GetLocation(Compilation compilation, Symbol symbol) {
        if (symbol.declaringCompilation == compilation)
            return BestSymbolLocation.FromSourceModule;
        else if (symbol.declaringCompilation is not null)
            return BestSymbolLocation.FromAddedModule;
        else
            return BestSymbolLocation.FromCorLibrary;
    }

    private TypeWithAnnotations BindArrayType(
        ArrayTypeSyntax node,
        BelteDiagnosticQueue diagnostics,
        bool permitDimensions,
        ConsList<TypeSymbol> basesBeingResolved) {
        var type = BindType(node.elementType, diagnostics, basesBeingResolved);

        if (type.isStatic)
            diagnostics.Push(Error.ArrayOfStaticType(node.elementType.location, type.type));

        for (var i = node.rankSpecifiers.Count - 1; i >= 0; i--) {
            var rankSpecifier = node.rankSpecifiers[i];
            var dimension = rankSpecifier.size;

            if (!permitDimensions && dimension is not null) {
                // Error(diagnostics, ErrorCode.ERR_ArraySizeInDeclaration, rankSpecifier);
                // TODO error
            }

            // TODO need to error check the size, should allow dynamic?
            var size = (int)BindExpression(dimension, diagnostics).constantValue.value;
            var array = ArrayTypeSymbol.CreateArray(type, size);
            type = new TypeWithAnnotations(array);
        }

        return type;
    }

    private protected TypeWithAnnotations BindNonTemplateSimpleType(
        IdentifierNameSyntax node,
        BelteDiagnosticQueue diagnostics,
        ConsList<TypeSymbol> basesBeingResolved,
        NamespaceOrTypeSymbol qualifier) {
        var name = node.identifier.text;

        if (string.IsNullOrWhiteSpace(name)) {
            var error = Error.UndefinedSymbol(node.location, name);

            return new TypeWithAnnotations(
                new ExtendedErrorTypeSymbol(compilation.globalNamespaceInternal, name, 0, error)
            );
        }

        // TODO template
        // var errorResult = CreateErrorIfLookupOnTypeParameter(node.Parent, qualifierOpt, identifierValueText, 0, diagnostics);
        // if (errorResult is not null) {
        //     return TypeWithAnnotations.Create(errorResult);
        // }

        if (qualifier is null) {
            var specialType = SpecialTypes.GetTypeFromMetadataName(string.Concat("global::", name));

            if (specialType != SpecialType.None)
                return new TypeWithAnnotations(CorLibrary.GetSpecialType(specialType));
        }

        var result = LookupResult.GetInstance();
        var options = LookupOptions.NamespacesOrTypesOnly;

        LookupSymbolsSimpleName(result, qualifier, name, 0, basesBeingResolved, options, true);

        var bindingResult = ResultSymbol(result, name, 0, node, diagnostics, out _, qualifier, options);

        result.Free();
        return new TypeWithAnnotations((TypeSymbol)bindingResult);
    }

    private TypeWithAnnotations BindQualifiedName(
        ExpressionSyntax leftName,
        SimpleNameSyntax rightName,
        BelteDiagnosticQueue diagnostics,
        ConsList<TypeSymbol> basesBeingResolved) {
        var left = BindType(leftName, diagnostics, basesBeingResolved).type;

        var isLeftUnboundTemplateType = left.kind == SymbolKind.NamedType &&
            ((NamedTypeSymbol)left).isUnboundTemplateType;

        if (isLeftUnboundTemplateType)
            left = ((NamedTypeSymbol)left).originalDefinition;

        var right = BindSimpleType(rightName, diagnostics, basesBeingResolved, left);

        if (isLeftUnboundTemplateType)
            return ConvertToUnbound();

        return right;

        TypeWithAnnotations ConvertToUnbound() {
            var namedTypeRight = right.type as NamedTypeSymbol;

            if (namedTypeRight is not null && namedTypeRight.isTemplateType)
                return new TypeWithAnnotations(namedTypeRight.AsUnboundTemplateType(), right.isNullable);

            return right;
        }
    }

    private TypeWithAnnotations BindSimpleType(
        SimpleNameSyntax node,
        BelteDiagnosticQueue diagnostics,
        ConsList<TypeSymbol> basesBeingResolved,
        NamespaceOrTypeSymbol qualifier = null) {
        return node.kind switch {
            SyntaxKind.IdentifierName
                => BindNonTemplateSimpleType((IdentifierNameSyntax)node, diagnostics, basesBeingResolved, qualifier),
            SyntaxKind.TemplateName
                => BindTemplateSimpleType((TemplateNameSyntax)node, diagnostics, basesBeingResolved, qualifier),
            _ => new TypeWithAnnotations(
                new ExtendedErrorTypeSymbol(qualifier ?? compilation.globalNamespaceInternal, "", 0, null)
            )
        };
    }

    private TypeWithAnnotations BindTemplateSimpleType(
        TemplateNameSyntax node,
        BelteDiagnosticQueue diagnostics,
        ConsList<TypeSymbol> basesBeingResolved,
        NamespaceOrTypeSymbol qualifier) {
        // TODO template
        return new TypeWithAnnotations(CreateErrorType());
    }

    internal TypeWithAnnotations BindTypeOrImplicitType(
        TypeSyntax syntax,
        BelteDiagnosticQueue diagnostics,
        out bool isImplicitlyTyped) {
        if (syntax.isImplicitlyTyped) {
            isImplicitlyTyped = true;
            return new TypeWithAnnotations(null, true);
        } else {
            isImplicitlyTyped = false;
            return BindType(syntax, diagnostics);
        }
    }

    private ImmutableArray<(string, TypeOrConstant)> BindTemplateArguments(
        SeparatedSyntaxList<ArgumentSyntax> templateArguments,
        BelteDiagnosticQueue diagnostics,
        ConsList<TypeSymbol> basesBeingResolved = null) {
        var arguments = ArrayBuilder<(string, TypeOrConstant)>.GetInstance(templateArguments.Count);

        foreach (var argumentSyntax in templateArguments)
            arguments.Add(BindTemplateArgument(argumentSyntax, diagnostics, basesBeingResolved));

        return arguments.ToImmutableAndFree();
    }

    private (string, TypeOrConstant) BindTemplateArgument(
        ArgumentSyntax templateArgument,
        BelteDiagnosticQueue diagnostics,
        ConsList<TypeSymbol> basesBeingResolved = null) {
        var name = templateArgument.identifier.text;
        var value = BindExpression(templateArgument.expression, diagnostics);

        // TODO Implement normal arguments first
        // return (name, value);
        return (null, null);
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
            defaultValueSyntax.value,
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
        initializer = GenerateConversionForAssignment(varType, initializer, diagnostics, initializerOpt.value);
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
            diagnostics.Push(Error.MemberIsInaccessible(constructor.location, baseConstructor));
            resultKind = LookupResultKind.Inaccessible;
        }

        var receiver = new BoundThisExpression(containingType);
        return new BoundCallExpression(receiver, baseConstructor, [], []);
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
                diagnostics.Push(Error.InitializeByValueWithByReference(node.location));
                return false;
            }
        } else {
            valueKind = variableRefKind == RefKind.RefConst
                ? BindValueKind.RefConst
                : BindValueKind.RefOrOut;

            if (initializer is null) {
                // Error(diagnostics, ErrorCode.ERR_ByReferenceVariableMustBeInitialized, node);
                // TODO should we error here?
                return false;
            } else if (expressionRefKind != RefKind.Ref) {
                diagnostics.Push(Error.InitializeByReferenceWithByValue(node.location));
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

            // TODO arrays
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
        // TODO check value
        return expression;
    }

    private BoundExpression BindConstructorInitializerCore(
        ArgumentListSyntax initializerArgumentListOpt,
        MethodSymbol constructor,
        BelteDiagnosticQueue diagnostics) {
        // TODO constructors
        return null;
    }

    private BoundExpression BindExpressionInternal(
        ExpressionSyntax node,
        BelteDiagnosticQueue diagnostics,
        bool called,
        bool indexed) {
        return node.kind switch {
            SyntaxKind.LiteralExpression => BindLiteralExpression((LiteralExpressionSyntax)node, diagnostics),
            SyntaxKind.ThisExpression => BindThisExpression((ThisExpressionSyntax)node, diagnostics),
            SyntaxKind.BaseExpression => BindBaseExpression((BaseExpressionSyntax)node, diagnostics),
            SyntaxKind.EmptyExpression => BindEmptyExpression((EmptyExpressionSyntax)node, diagnostics),
            SyntaxKind.CallExpression => BindCallExpression((CallExpressionSyntax)node, diagnostics),
            SyntaxKind.QualifiedName => BindQualifiedName((QualifiedNameSyntax)node, diagnostics),
            SyntaxKind.ReferenceType => BindReferenceType((ReferenceTypeSyntax)node, diagnostics),
            SyntaxKind.NonNullableType => ErrorExpression(null), // TODO Confirm this is not reachable without err
            SyntaxKind.ParenthesizedExpression => BindParenthesisExpression((ParenthesisExpressionSyntax)node, diagnostics),
            SyntaxKind.MemberAccessExpression => BindMemberAccess((MemberAccessExpressionSyntax)node, called, indexed, diagnostics),
            SyntaxKind.IdentifierName or SyntaxKind.TemplateName => BindIdentifier((SimpleNameSyntax)node, called, indexed, diagnostics),
            SyntaxKind.BinaryExpression => BindBinaryExpression((BinaryExpressionSyntax)node, diagnostics),
            // SyntaxKind.UnaryExpression => BindUnaryExpression((UnaryExpressionSyntax)node, diagnostics),
            // SyntaxKind.PrefixExpression => BindPrefixExpression((PrefixExpressionSyntax)node, diagnostics),
            // SyntaxKind.PostfixExpression => BindPostfixExpression((PostfixExpressionSyntax)node, diagnostics),
            // SyntaxKind.TernaryExpression => BindTernaryExpression((TernaryExpressionSyntax)node, diagnostics),
            SyntaxKind.AssignmentExpression => BindAssignmentExpression((AssignmentExpressionSyntax)node, diagnostics),
            /*
            case SyntaxKind.InitializerListExpression:
                return BindInitializerListExpression((InitializerListExpressionSyntax)expression, initializerListType);
            case SyntaxKind.InitializerDictionaryExpression:
                return BindInitializerDictionaryExpression((InitializerDictionaryExpressionSyntax)expression);
            case SyntaxKind.IndexExpression:
                return BindIndexExpression((IndexExpressionSyntax)expression);
                return BindReferenceExpression((ReferenceExpressionSyntax)expression);
            case SyntaxKind.CastExpression:
                return BindCastExpression((CastExpressionSyntax)expression);
            case SyntaxKind.TypeOfExpression:
                return BindTypeOfExpression((TypeOfExpressionSyntax)expression);
            case SyntaxKind.NameOfExpression:
                return BindNameOfExpression((NameOfExpressionSyntax)expression);
            case SyntaxKind.ObjectCreationExpression:
                return BindObjectCreationExpression((ObjectCreationExpressionSyntax)expression);
            case SyntaxKind.ThrowExpression:
                return BindThrowExpression((ThrowExpressionSyntax)expression);
            case SyntaxKind.ArrayType when allowTypes:
                return BindType((TypeSyntax)expression, explicitly: true);
                */
            _ => throw ExceptionUtilities.UnexpectedValue(node.kind),
        };
    }

    private BoundErrorExpression ErrorExpression(BoundExpression expression) {
        // This is factored out into a method for debugging purposes
        // TODO consider storing the entire node
        return new BoundErrorExpression(expression?.type ?? CreateErrorType("?"));
    }

    private BoundExpression BindBinaryExpression(BinaryExpressionSyntax node, BelteDiagnosticQueue diagnostics) {
        if (IsSimpleBinaryOperator(node))
            return BindSimpleBinaryOperator(node, diagnostics);
        else if (node.operatorToken.kind is SyntaxKind.IsKeyword or SyntaxKind.IsntKeyword)
            return BindIsOperator(node, diagnostics);
        else if (node.operatorToken.kind == SyntaxKind.AsKeyword)
            return BindAsOperator(node, diagnostics);
        else if (node.operatorToken.kind is SyntaxKind.PipePipeToken or SyntaxKind.AmpersandAmpersandToken)
            return BindConditionalOperator(node, diagnostics);
        else if (node.operatorToken.kind == SyntaxKind.QuestionQuestionToken)
            return BindNullCoalescingOperator(node, diagnostics);

        throw ExceptionUtilities.Unreachable();
    }

    private BoundExpression BindIsOperator(BinaryExpressionSyntax node, BelteDiagnosticQueue diagnostics) {
        var isIsntOperator = node.operatorToken.kind == SyntaxKind.IsntKeyword;
        var resultType = (TypeSymbol)CorLibrary.GetSpecialType(SpecialType.Bool);
        var operand = BindValue(node.left, diagnostics, BindValueKind.RValue);

        if (node.right is LiteralExpressionSyntax l && l.token.value is null) {
            if (ConstantValue.IsNotNull(operand.constantValue)) {
                diagnostics.Push(Warning.AlwaysValue(node.location, isIsntOperator));
                return new BoundLiteralExpression(new ConstantValue(isIsntOperator, SpecialType.Bool), resultType);
            }

            var boundRight = BindLiteralExpression(l, diagnostics);
            return isIsntOperator
                ? new BoundIsntExpression(operand, boundRight, resultType)
                : new BoundIsExpression(operand, boundRight, resultType);
        }

        var targetTypeWithAnnotations = BindType(node.right, diagnostics);
        var targetType = targetTypeWithAnnotations.type;
        var boundType = new BoundTypeExpression(targetType, targetTypeWithAnnotations);

        if ((operand.type.isObjectType && targetType.isPrimitiveType) ||
            (operand.type.isPrimitiveType && targetType.isObjectType) ||
            targetType.isStatic) {
            diagnostics.Push(Warning.NeverGivenType(node.location, targetType));
            return new BoundLiteralExpression(new ConstantValue(isIsntOperator, SpecialType.Bool), resultType);
        }

        if (ConstantValue.IsNull(operand.constantValue) ||
            operand.kind == BoundNodeKind.MethodGroup ||
            operand.type.IsVoidType()) {
            diagnostics.Push(Warning.AlwaysValue(node.location, isIsntOperator));
            return new BoundLiteralExpression(new ConstantValue(isIsntOperator, SpecialType.Bool), resultType);
        }

        var operandType = operand.type;
        var conversion = conversions.ClassifyBuiltInConversion(operandType, targetType);
        var cast = CreateConversion(node.left, operand, conversion, false, targetType, diagnostics);
        return isIsntOperator
            ? new BoundIsntExpression(cast, boundType, resultType)
            : new BoundIsExpression(cast, boundType, resultType);
    }

    private BoundExpression BindAsOperator(BinaryExpressionSyntax node, BelteDiagnosticQueue diagnostics) {
        var operand = BindValue(node.left, diagnostics, BindValueKind.RValue);
        var targetTypeWithAnnotations = BindType(node.right, diagnostics);
        var targetType = targetTypeWithAnnotations.type;
        var boundType = new BoundTypeExpression(targetType, targetTypeWithAnnotations);
        var resultType = targetType;

        if ((operand.type.isObjectType && targetType.isPrimitiveType) ||
            (operand.type.isPrimitiveType && targetType.isObjectType) ||
            targetType.isStatic) {
            diagnostics.Push(Warning.NeverGivenType(node.location, targetType));
            return new BoundLiteralExpression(new ConstantValue(false, SpecialType.Bool), resultType);
        }

        var operandType = operand.type;
        var conversion = conversions.ClassifyBuiltInConversion(operandType, targetType);
        var cast = CreateConversion(node.left, operand, conversion, false, targetType, diagnostics);
        return new BoundAsExpression(cast, boundType, resultType);
    }

    private BoundExpression BindNullCoalescingOperator(BinaryExpressionSyntax node, BelteDiagnosticQueue diagnostics) {
        var leftOperand = BindValue(node.left, diagnostics, BindValueKind.RValue);
        var rightOperand = BindValue(node.right, diagnostics, BindValueKind.RValue);

        var optLeftType = leftOperand.type;   // "A"
        var optRightType = rightOperand.type; // "B"
        var isLeftNullable = optLeftType is not null && optLeftType.IsNullableType();
        var optLeftType0 = isLeftNullable ? optLeftType.GetNullableUnderlyingType() : optLeftType; // "A0"

        if (leftOperand.kind == BoundNodeKind.MethodGroup)
            return new BoundNullCoalescingExpression(leftOperand, rightOperand, CreateErrorType());

        if (isLeftNullable) {
            var rightConversion = conversions.ClassifyImplicitConversionFromExpression(rightOperand, optLeftType0);

            if (rightConversion.exists) {
                var convertedRightOperand = CreateConversion(
                    node.right,
                    rightOperand,
                    rightConversion,
                    false,
                    optLeftType0,
                    diagnostics
                );

                return new BoundNullCoalescingExpression(leftOperand, convertedRightOperand, optLeftType0);
            }
        }

        if (optLeftType is not null) {
            var rightConversion = conversions.ClassifyImplicitConversionFromExpression(rightOperand, optLeftType);

            if (rightConversion.exists) {
                var convertedRightOperand = CreateConversion(
                    node.right,
                    rightOperand,
                    rightConversion,
                    false,
                    optLeftType,
                    diagnostics
                );

                return new BoundNullCoalescingExpression(leftOperand, convertedRightOperand, optLeftType);
            }
        }

        if (optRightType is not null) {
            Conversion leftConversionClassification;

            if (isLeftNullable) {
                leftConversionClassification = conversions.ClassifyImplicitConversionFromType(optLeftType0, optRightType);

                if (leftConversionClassification.exists) {
                    var leftConversion = CreateConversion(
                        node,
                        leftOperand,
                        leftConversionClassification,
                        false,
                        optRightType,
                        diagnostics
                    );

                    return new BoundNullCoalescingExpression(leftConversion, rightOperand, optRightType);
                }
            } else {
                leftConversionClassification = conversions.ClassifyImplicitConversionFromExpression(
                    leftOperand,
                    optRightType
                );

                if (leftConversionClassification.exists) {
                    var leftConversion = CreateConversion(
                        node,
                        leftOperand,
                        leftConversionClassification,
                        false,
                        optRightType,
                        diagnostics
                    );

                    return new BoundNullCoalescingExpression(leftConversion, rightOperand, optRightType);
                }
            }
        }

        return new BoundNullCoalescingExpression(leftOperand, rightOperand, CreateErrorType());
    }

    private BoundExpression BindConditionalOperator(BinaryExpressionSyntax node, BelteDiagnosticQueue diagnostics) {
        var binary = node;
        ExpressionSyntax child;

        while (true) {
            child = binary.left;

            if (child is not BinaryExpressionSyntax childAsBinary ||
                (childAsBinary.operatorToken.kind is not SyntaxKind.PipePipeToken and
                    not SyntaxKind.AmpersandAmpersandToken)) {
                break;
            }

            binary = childAsBinary;
        }

        var left = BindValue(child, diagnostics, BindValueKind.RValue);

        do {
            binary = (BinaryExpressionSyntax)child.parent;
            var right = BindValue(binary.right, diagnostics, BindValueKind.RValue);
            left = BindConditionalOperator(binary, left, right, diagnostics);
            child = binary;
        } while ((object)child != node);

        return left;
    }

    private BoundExpression BindConditionalOperator(
        BinaryExpressionSyntax node,
        BoundExpression left,
        BoundExpression right,
        BelteDiagnosticQueue diagnostics) {
        var kind = SyntaxKindToBinaryOperatorKind(node.operatorToken.kind);

        if (left.type is not null && left.type.specialType == SpecialType.Bool &&
            right.type is not null && right.type.specialType == SpecialType.Bool) {
            var constantValue = ConstantFolding.FoldBinary(left, right, kind | BinaryOperatorKind.Bool, left.type);
            return new BoundBinaryExpression(left, right, kind | BinaryOperatorKind.Bool, left.type, constantValue);
        }

        var best = BinaryOperatorOverloadResolution(
            kind,
            left,
            right,
            node,
            diagnostics,
            out var lookupResult,
            out var originalUserDefinedOperators
        );

        if (!best.hasValue) {
            ReportBinaryOperatorError(node, diagnostics, node.operatorToken, left, right, lookupResult);
        } else {
            var signature = best.signature;
            var bothBool = signature.leftType.specialType == SpecialType.Bool &&
                signature.rightType.specialType == SpecialType.Bool;

            if (!bothBool) {
                ReportBinaryOperatorError(node, diagnostics, node.operatorToken, left, right, lookupResult);
            } else if (bothBool) {
                var resultLeft = CreateConversion(node.left, left, best.leftConversion, false, signature.leftType, diagnostics);
                var resultRight = CreateConversion(node.right, right, best.rightConversion, false, signature.rightType, diagnostics);
                var resultKind = kind | signature.kind.OperandTypes();
                return new BoundBinaryExpression(resultLeft, resultRight, resultKind, signature.returnType, null);
            }
        }

        return new BoundBinaryExpression(left, right, kind, CreateErrorType(), null);
    }

    private bool IsSimpleBinaryOperator(ExpressionSyntax node) {
        if (node is BinaryExpressionSyntax b) {
            switch (b.operatorToken.kind) {
                case SyntaxKind.PlusToken:
                case SyntaxKind.MinusToken:
                case SyntaxKind.AsteriskToken:
                case SyntaxKind.SlashToken:
                case SyntaxKind.PercentToken:
                case SyntaxKind.EqualsEqualsToken:
                case SyntaxKind.ExclamationEqualsToken:
                case SyntaxKind.LessThanLessThanToken:
                case SyntaxKind.GreaterThanGreaterThanToken:
                case SyntaxKind.GreaterThanGreaterThanGreaterThanToken:
                case SyntaxKind.AsteriskAsteriskToken:
                case SyntaxKind.AmpersandToken:
                case SyntaxKind.CaretToken:
                case SyntaxKind.PipeToken:
                case SyntaxKind.LessThanToken:
                case SyntaxKind.GreaterThanToken:
                case SyntaxKind.LessThanEqualsToken:
                case SyntaxKind.GreaterThanEqualsToken:
                    return true;
                case SyntaxKind.IsKeyword:
                case SyntaxKind.IsntKeyword:
                case SyntaxKind.AsKeyword:
                case SyntaxKind.AmpersandAmpersandToken:
                case SyntaxKind.PipePipeToken:
                case SyntaxKind.QuestionQuestionToken:
                default:
                    return false;
            }
        }

        return false;
    }

    private BoundExpression BindSimpleBinaryOperator(BinaryExpressionSyntax node, BelteDiagnosticQueue diagnostics) {
        var syntaxNodes = ArrayBuilder<BinaryExpressionSyntax>.GetInstance();

        ExpressionSyntax current = node;
        while (IsSimpleBinaryOperator(current)) {
            var binOp = (BinaryExpressionSyntax)current;
            syntaxNodes.Push(binOp);
            current = binOp.left;
        }

        var result = BindExpression(current, diagnostics);

        if (node.operatorToken.kind == SyntaxKind.MinusToken && current.kind == SyntaxKind.ParenthesizedExpression) {
            if (result.kind == BoundNodeKind.TypeExpression
                && !(((ParenthesisExpressionSyntax)current).expression.kind == SyntaxKind.ParenthesizedExpression)) {
                // TODO what is this error
                // Error(diagnostics, ErrorCode.ERR_PossibleBadNegCast, node);
            } else if (result.kind == BoundNodeKind.ErrorExpression) {
                var parenthesizedExpression = (ParenthesisExpressionSyntax)current;

                if (parenthesizedExpression.expression.kind == SyntaxKind.IdentifierName
                    && ((IdentifierNameSyntax)parenthesizedExpression.expression).identifier.text == "dynamic") {
                    // TODO what is this error
                    // Error(diagnostics, ErrorCode.ERR_PossibleBadNegCast, node);
                }
            }
        }

        while (syntaxNodes.Count > 0) {
            var syntaxNode = syntaxNodes.Pop();
            var bindValueKind = GetBinaryAssignmentKind(syntaxNode.operatorToken.kind);
            var left = CheckValue(result, bindValueKind, diagnostics);
            var right = BindValue(syntaxNode.right, diagnostics, BindValueKind.RValue);
            var boundOp = BindSimpleBinaryOperator(syntaxNode, diagnostics, left, right);
            result = boundOp;
        }

        syntaxNodes.Free();
        return result;
    }

    private void ReportBinaryOperatorError(
        ExpressionSyntax node,
        BelteDiagnosticQueue diagnostics,
        SyntaxToken operatorToken,
        BoundExpression left,
        BoundExpression right,
        LookupResultKind resultKind) {
        var isEquality = operatorToken.kind is SyntaxKind.EqualsEqualsToken or SyntaxKind.ExclamationEqualsToken;

        switch (resultKind) {
            case LookupResultKind.Ambiguous:
                diagnostics.Push(
                    Error.AmbiguousBinaryOperator(node.location, operatorToken.text, left.type, right.type)
                );

                break;
            default:
                diagnostics.Push(
                    Error.InvalidBinaryOperatorUse(node.location, operatorToken.text, left.type, right.type)
                );

                break;
        }
    }

    private BoundExpression BindSimpleBinaryOperator(
        BinaryExpressionSyntax node,
        BelteDiagnosticQueue diagnostics,
        BoundExpression left,
        BoundExpression right) {
        var kind = SyntaxKindToBinaryOperatorKind(node.operatorToken.kind);

        var leftType = left.type;
        var rightType = right.type;

        var foundOperator = BindSimpleBinaryOperatorParts(
            node,
            diagnostics,
            left,
            right,
            kind,
            out var resultKind,
            out var originalUserDefinedOperators,
            out var signature,
            out var best
        );

        var resultOperatorKind = signature.kind;
        var hasErrors = false;

        if (!foundOperator) {
            ReportBinaryOperatorError(node, diagnostics, node.operatorToken, left, right, resultKind);
            resultOperatorKind &= ~BinaryOperatorKind.TypeMask;
            hasErrors = true;
        }

        var resultType = signature.returnType;
        var resultLeft = left;
        var resultRight = right;
        ConstantValue resultConstant = null;

        if (foundOperator && (resultOperatorKind.OperandTypes() != BinaryOperatorKind.NullableNull)) {
            resultLeft = CreateConversion(
                node.left,
                left,
                best.leftConversion,
                false,
                signature.leftType,
                diagnostics
            );

            resultRight = CreateConversion(
                node.right,
                right,
                best.rightConversion,
                false,
                signature.rightType,
                diagnostics
            );

            resultConstant = ConstantFolding.FoldBinary(resultLeft, resultRight, resultOperatorKind, resultType);
        }

        return new BoundBinaryExpression(
            resultLeft,
            resultRight,
            resultOperatorKind,
            resultType,
            resultConstant
        );
    }

    private bool BindSimpleBinaryOperatorParts(
        BinaryExpressionSyntax node,
        BelteDiagnosticQueue diagnostics,
        BoundExpression left,
        BoundExpression right,
        BinaryOperatorKind kind,
        out LookupResultKind resultKind,
        out ImmutableArray<MethodSymbol> originalUserDefinedOperators,
        out BinaryOperatorSignature resultSignature,
        out BinaryOperatorAnalysisResult best) {
        bool foundOperator;
        best = BinaryOperatorOverloadResolution(
            kind,
            left,
            right,
            node,
            diagnostics,
            out resultKind,
            out originalUserDefinedOperators
        );

        if (!best.hasValue) {
            resultSignature = new BinaryOperatorSignature(kind, null, null, CreateErrorType());
            foundOperator = false;
        } else {
            var signature = best.signature;
            var isObjectEquality = signature.kind is BinaryOperatorKind.ObjectEqual or
                BinaryOperatorKind.ObjectNotEqual;

            var leftNull = left.IsLiteralNull();
            var rightNull = right.IsLiteralNull();

            var leftType = left.type;
            var rightType = right.type;

            var isNullableEquality = signature.method is null &&
                (signature.kind.Operator() == BinaryOperatorKind.Equal ||
                signature.kind.Operator() == BinaryOperatorKind.NotEqual) &&
                (leftNull && rightType is not null && rightType.IsNullableType() ||
                    rightNull && leftType is not null && leftType.IsNullableType());

            if (isNullableEquality) {
                resultSignature = new BinaryOperatorSignature(
                    kind | BinaryOperatorKind.NullableNull,
                    null,
                    null,
                    CorLibrary.GetSpecialType(SpecialType.Bool)
                );

                foundOperator = true;
            } else {
                resultSignature = signature;
                foundOperator = !isObjectEquality ||
                    OperatorFacts.IsValidObjectEquality(conversions, leftType, leftNull, rightType, rightNull);
            }
        }

        return foundOperator;
    }

    private BinaryOperatorAnalysisResult BinaryOperatorOverloadResolution(
        BinaryOperatorKind kind,
        BoundExpression left,
        BoundExpression right,
        BelteSyntaxNode node,
        BelteDiagnosticQueue diagnostics,
        out LookupResultKind resultKind,
        out ImmutableArray<MethodSymbol> originalUserDefinedOperators) {
        var result = BinaryOperatorOverloadResolutionResult.GetInstance();
        overloadResolution.BinaryOperatorOverloadResolution(kind, left, right, result);
        var possiblyBest = result.best;

        if (result.results.Any()) {
            var builder = ArrayBuilder<MethodSymbol>.GetInstance();

            foreach (var analysisResult in result.results) {
                var method = analysisResult.signature.method;

                if (method is not null)
                    builder.Add(method);
            }

            originalUserDefinedOperators = builder.ToImmutableAndFree();

            if (possiblyBest.hasValue)
                resultKind = LookupResultKind.Viable;
            else if (result.AnyValid())
                resultKind = LookupResultKind.Ambiguous;
            else
                resultKind = LookupResultKind.OverloadResolutionFailure;
        } else {
            originalUserDefinedOperators = [];
            resultKind = possiblyBest.hasValue ? LookupResultKind.Viable : LookupResultKind.Empty;
        }

        result.Free();
        return possiblyBest;
    }

    internal static BinaryOperatorKind SyntaxKindToBinaryOperatorKind(SyntaxKind kind) {
        switch (kind) {
            case SyntaxKind.AsteriskToken:
            case SyntaxKind.AsteriskEqualsToken:
                return BinaryOperatorKind.Multiplication;
            case SyntaxKind.SlashEqualsToken:
            case SyntaxKind.SlashToken:
                return BinaryOperatorKind.Division;
            case SyntaxKind.PercentEqualsToken:
            case SyntaxKind.PercentToken:
                return BinaryOperatorKind.Modulo;
            case SyntaxKind.PlusEqualsToken:
            case SyntaxKind.PlusToken:
                return BinaryOperatorKind.Addition;
            case SyntaxKind.MinusEqualsToken:
            case SyntaxKind.MinusToken:
                return BinaryOperatorKind.Subtraction;
            case SyntaxKind.GreaterThanGreaterThanEqualsToken:
            case SyntaxKind.GreaterThanGreaterThanToken:
                return BinaryOperatorKind.RightShift;
            case SyntaxKind.GreaterThanGreaterThanGreaterThanEqualsToken:
            case SyntaxKind.GreaterThanGreaterThanGreaterThanToken:
                return BinaryOperatorKind.UnsignedRightShift;
            case SyntaxKind.LessThanLessThanEqualsToken:
            case SyntaxKind.LessThanLessThanToken:
                return BinaryOperatorKind.LeftShift;
            case SyntaxKind.EqualsEqualsToken:
                return BinaryOperatorKind.Equal;
            case SyntaxKind.ExclamationEqualsToken:
                return BinaryOperatorKind.NotEqual;
            case SyntaxKind.GreaterThanToken:
                return BinaryOperatorKind.GreaterThan;
            case SyntaxKind.LessThanToken:
                return BinaryOperatorKind.LessThan;
            case SyntaxKind.GreaterThanEqualsToken:
                return BinaryOperatorKind.GreaterThanOrEqual;
            case SyntaxKind.LessThanEqualsToken:
                return BinaryOperatorKind.LessThanOrEqual;
            case SyntaxKind.AmpersandEqualsToken:
            case SyntaxKind.AmpersandToken:
                return BinaryOperatorKind.And;
            case SyntaxKind.PipeEqualsToken:
            case SyntaxKind.PipeToken:
                return BinaryOperatorKind.Or;
            case SyntaxKind.CaretEqualsToken:
            case SyntaxKind.CaretToken:
                return BinaryOperatorKind.Xor;
            case SyntaxKind.AmpersandAmpersandToken:
                return BinaryOperatorKind.ConditionalAnd;
            case SyntaxKind.PipePipeToken:
                return BinaryOperatorKind.ConditionalOr;
            default:
                throw ExceptionUtilities.UnexpectedValue(kind);
        }
    }

    private static BindValueKind GetBinaryAssignmentKind(SyntaxKind kind) {
        switch (kind) {
            case SyntaxKind.EqualsToken:
                return BindValueKind.Assignable;
            case SyntaxKind.PlusEqualsToken:
            case SyntaxKind.MinusEqualsToken:
            case SyntaxKind.AsteriskEqualsToken:
            case SyntaxKind.SlashEqualsToken:
            case SyntaxKind.AmpersandEqualsToken:
            case SyntaxKind.PipeEqualsToken:
            case SyntaxKind.CaretEqualsToken:
            case SyntaxKind.AsteriskAsteriskEqualsToken:
            case SyntaxKind.LessThanLessThanEqualsToken:
            case SyntaxKind.GreaterThanGreaterThanEqualsToken:
            case SyntaxKind.GreaterThanGreaterThanGreaterThanEqualsToken:
            case SyntaxKind.PercentEqualsToken:
            case SyntaxKind.QuestionQuestionEqualsToken:
                return BindValueKind.CompoundAssignment;
            default:
                return BindValueKind.RValue;
        }
    }

    private BoundExpression BindAssignmentExpression(
        AssignmentExpressionSyntax node,
        BelteDiagnosticQueue diagnostics) {
        if (node.assignmentToken.kind != SyntaxKind.EqualsToken)
            return BindCompoundAssignment(node, diagnostics);

        var rhsExpr = node.right.UnwrapRefExpression(out var refKind);
        var isRef = refKind == RefKind.Ref;
        var lhsKind = isRef ? BindValueKind.RefAssignable : BindValueKind.Assignable;
        var op1 = BindValue(node.left, diagnostics, lhsKind);
        var rhsKind = isRef ? GetRequiredRHSValueKindForRefAssignment(op1) : BindValueKind.RValue;
        var op2 = BindValue(rhsExpr, diagnostics, rhsKind);
        return BindAssignment(node.right, op1, op2, isRef, diagnostics);
    }

    private BoundExpression BindCompoundAssignment(AssignmentExpressionSyntax node, BelteDiagnosticQueue diagnostics) {
        var left = BindValue(node.left, diagnostics, GetBinaryAssignmentKind(node.kind));
        var right = BindValue(node.right, diagnostics, BindValueKind.RValue);
        var kind = SyntaxKindToBinaryOperatorKind(node.assignmentToken.kind);

        var best = BinaryOperatorOverloadResolution(
            kind,
            left,
            right,
            node,
            diagnostics,
            out var resultKind,
            out var originalUserDefinedOperators
        );

        if (!best.hasValue) {
            diagnostics.Push(
                Error.InvalidBinaryOperatorUse(node.location, node.assignmentToken.text, left.type, right.type)
            );

            return ErrorExpression(left);
        }

        var bestSignature = best.signature;
        var rightConverted = CreateConversion(
            node.right,
            right,
            best.rightConversion,
            false,
            bestSignature.rightType,
            diagnostics
        );

        var isPredefinedOperator = !bestSignature.kind.IsUserDefined();
        var leftType = left.type;
        var finalPlaceholder = new BoundTypeExpression(bestSignature.returnType);

        var finalConversion = GenerateConversionForAssignment(
            leftType,
            finalPlaceholder,
            diagnostics,
            node,
            ConversionForAssignmentFlags.CompoundAssignment |
                (isPredefinedOperator
                    ? ConversionForAssignmentFlags.PredefinedOperator
                    : ConversionForAssignmentFlags.None)
        );

        if (finalConversion is not BoundCastExpression final) {
            if ((object)finalConversion != finalPlaceholder) {
                finalPlaceholder = null;
                finalConversion = null;
            }
        } else if (final.conversion.isExplicit && isPredefinedOperator && !kind.IsShift()) {
            var rightToLeftConversion = conversions.ClassifyConversionFromExpression(right, leftType);

            if (!rightToLeftConversion.isImplicit || !rightToLeftConversion.exists)
                GenerateImplicitConversionError(diagnostics, node, rightToLeftConversion, right, leftType);
        }

        return new BoundCompoundAssignmentExpression(left, rightConverted, best.signature.kind, leftType);
    }

    private BoundAssignmentExpression BindAssignment(
        SyntaxNode node,
        BoundExpression op1,
        BoundExpression op2,
        bool isRef,
        BelteDiagnosticQueue diagnostics) {
        var conversion = GenerateConversionForAssignment(
            op1.type,
            op2,
            diagnostics,
            node,
            isRef ? ConversionForAssignmentFlags.RefAssignment : ConversionForAssignmentFlags.None
        );

        op2 = conversion;
        var type = op1.type;

        return new BoundAssignmentExpression(op1, op2, isRef, type);
    }

    private static BindValueKind GetRequiredRHSValueKindForRefAssignment(BoundExpression boundLeft) {
        var rhsKind = BindValueKind.RefersToLocation;
        var lhsRefKind = boundLeft.GetRefKind();

        if (lhsRefKind is RefKind.Ref)
            rhsKind |= BindValueKind.Assignable;

        return rhsKind;
    }

    private BoundExpression BindIdentifier(
        SimpleNameSyntax node,
        bool called,
        bool indexed,
        BelteDiagnosticQueue diagnostics) {
        BoundExpression expression = null;
        var hasTemplateArguments = node.arity > 0;
        var templateArgumentList = node is TemplateNameSyntax t ? t.templateArgumentList : default;

        var lookupResult = LookupResult.GetInstance();
        var name = node.identifier.text;
        LookupIdentifier(lookupResult, node, called);

        if (lookupResult.kind != LookupResultKind.Empty) {
            var members = ArrayBuilder<Symbol>.GetInstance();
            var symbol = GetSymbolOrMethodGroup(
                lookupResult,
                node,
                name,
                node.arity,
                members,
                diagnostics,
                out var isError,
                null
            );

            if (symbol is null) {
                var receiver = SynthesizeMethodGroupReceiver(node, members);
                // expression = ConstructBoundMemberGroupAndReportOmittedTypeArguments(
                //     node,
                //     typeArgumentList,
                //     typeArgumentsWithAnnotations,
                //     receiver,
                //     name,
                //     members,
                //     lookupResult,
                //     receiver != null ? BoundMethodGroupFlags.HasImplicitReceiver : BoundMethodGroupFlags.None,
                //     isError,
                //     diagnostics);

                // ReportSimpleProgramLocalReferencedOutsideOfTopLevelStatement(node, members[0], diagnostics);
                // TODO error template
            } else {
                var isNamedType = symbol.kind is SymbolKind.NamedType or SymbolKind.ErrorType;

                if (hasTemplateArguments && isNamedType) {
                    // symbol = ConstructNamedTypeUnlessTemplateArgumentOmitted(node, (NamedTypeSymbol)symbol, typeArgumentList, typeArgumentsWithAnnotations, diagnostics);
                    // TODO template
                }

                expression = BindNonMethod(node, symbol, diagnostics, lookupResult.kind, indexed, isError);

                if (!isNamedType && (hasTemplateArguments || node.kind == SyntaxKind.TemplateName))
                    expression = ErrorExpression(expression);
            }

            members.Free();
        } else {
            expression = ErrorExpression(null);

            if (lookupResult.error is not null)
                diagnostics.Push(lookupResult.error);
            else
                diagnostics.Push(Error.UndefinedSymbol(node.location, name));
        }

        lookupResult.Free();
        return expression;
    }

    private BoundExpression SynthesizeMethodGroupReceiver(BelteSyntaxNode syntax, ArrayBuilder<Symbol> members) {
        var currentType = containingType;

        if (currentType is null)
            return null;

        var declaringType = members[0].containingType;

        if (currentType.IsEqualToOrDerivedFrom(declaringType, TypeCompareKind.ConsiderEverything))
            return new BoundThisExpression(currentType);

        return null;
    }

    private BoundExpression BindNonMethod(
        SimpleNameSyntax node,
        Symbol symbol,
        BelteDiagnosticQueue diagnostics,
        LookupResultKind resultKind,
        bool indexed,
        bool isError) {

        switch (symbol.kind) {
            case SymbolKind.Local: {
                    var localSymbol = (DataContainerSymbol)symbol;
                    TypeSymbol type;
                    bool isNullableUnknown;

                    if (IsUsedBeforeDeclaration(node, localSymbol)) {
                        FieldSymbol possibleField;
                        var lookupResult = LookupResult.GetInstance();

                        LookupMembersInType(
                            lookupResult,
                            containingType,
                            localSymbol.name,
                            arity: 0,
                            basesBeingResolved: null,
                            options: LookupOptions.Default,
                            originalBinder: this,
                            diagnose: false
                        );

                        possibleField = lookupResult.singleSymbolOrDefault as FieldSymbol;
                        lookupResult.Free();

                        if (possibleField is not null) {
                            diagnostics.Push(Error.LocalUsedBeforeDeclarationAndHidesField(
                                node.location,
                                localSymbol,
                                possibleField
                            ));
                        } else {
                            diagnostics.Push(Error.LocalUsedBeforeDeclaration(node.location, localSymbol));
                        }

                        type = new ExtendedErrorTypeSymbol(
                            compilation,
                            "var",
                            0,
                            error: null,
                            variableUsedBeforeDeclaration: true
                        );

                        isNullableUnknown = true;
                    } else if (localSymbol is SourceDataContainerSymbol { isImplicitlyTyped: true } &&
                        localSymbol.forbiddenZone?.Contains(node) == true) {
                        diagnostics.Push(localSymbol.forbiddenDiagnostic);

                        type = new ExtendedErrorTypeSymbol(
                            compilation,
                            "var",
                            0,
                            error: null,
                            variableUsedBeforeDeclaration: true
                        );

                        isNullableUnknown = true;
                    } else {
                        type = localSymbol.type;
                        isNullableUnknown = false;

                        if (IsBadLocalOrParameterCapture(localSymbol, type, localSymbol.refKind)) {
                            isError = true;
                            // TODO is this a reachable error?
                        }
                    }

                    var constantValueOpt = localSymbol.isConst && !isInsideNameof && !type.IsErrorType()
                        ? localSymbol.GetConstantValue(node, localInProgress, diagnostics)
                        : null;

                    return new BoundDataContainerExpression(localSymbol, constantValue: constantValueOpt);
                }
            case SymbolKind.Parameter: {
                    var parameter = (ParameterSymbol)symbol;

                    if (IsBadLocalOrParameterCapture(parameter, parameter.type, parameter.refKind)) {
                        isError = true;
                        // TODO is this a reachable error?
                    }

                    return new BoundParameterExpression(parameter);
                }
            case SymbolKind.NamedType:
            case SymbolKind.ErrorType:
            case SymbolKind.TemplateParameter:
                return new BoundTypeExpression((TypeSymbol)symbol);
            case SymbolKind.Field: {
                    var receiver = SynthesizeReceiver(node, symbol, diagnostics);
                    return BindFieldAccess(
                        node,
                        receiver,
                        (FieldSymbol)symbol,
                        diagnostics,
                        resultKind,
                        indexed,
                        isError
                    );
                }
            default:
                throw ExceptionUtilities.UnexpectedValue(symbol.kind);
        }

        static bool IsUsedBeforeDeclaration(SimpleNameSyntax node, DataContainerSymbol localSymbol) {
            if (!localSymbol.hasSourceLocation)
                return false;

            var declaration = localSymbol.syntaxReference.node;

            if (node.span.start >= declaration.span.start)
                return false;

            return node.syntaxTree == declaration.syntaxTree;
        }
    }

    private bool IsBadLocalOrParameterCapture(Symbol symbol, TypeSymbol type, RefKind refKind) {
        if (refKind != RefKind.None) {
            if (containingMember is MethodSymbol containingMethod &&
                (object)symbol.containingSymbol != containingMethod) {
                return (containingMethod.methodKind == MethodKind.LocalFunction) && !isInsideNameof;
            }
        }

        return false;
    }

    private BoundExpression SynthesizeReceiver(SyntaxNode node, Symbol member, BelteDiagnosticQueue diagnostics) {
        if (!member.RequiresInstanceReceiver())
            return null;

        var currentType = containingType;
        var declaringType = member.containingType;

        if (currentType.IsEqualToOrDerivedFrom(declaringType, TypeCompareKind.ConsiderEverything)) {
            var hasErrors = false;

            if (!isInsideNameof) {
                BelteDiagnostic diagnosticInfoOpt = null;

                if (inFieldInitializer) {
                    diagnostics.Push(Error.CannotUseThis(node.location));
                } else if (_inConstructorInitializer) {
                    diagnostics.Push(Error.InstanceRequired(node.location, member));
                } else {
                    var containingMember = this.containingMember;

                    var locationIsInstanceMember = !containingMember.isStatic &&
                        (containingMember.kind != SymbolKind.NamedType);

                    if (!locationIsInstanceMember)
                        diagnostics.Push(Error.InstanceRequired(node.location, member));
                }

                diagnosticInfoOpt ??= GetDiagnosticIfRefOrOutThisParameterCaptured(node.location);
                hasErrors = diagnosticInfoOpt is not null;

                if (hasErrors && !isInsideNameof)
                    diagnostics.Push(diagnosticInfoOpt);
            }

            return new BoundThisExpression(currentType ?? CreateErrorType());
        } else {
            return null;
        }
    }

    private void LookupIdentifier(LookupResult lookupResult, SimpleNameSyntax node, bool called) {
        LookupIdentifier(lookupResult, node.identifier.text, node.arity, called);
    }

    private void LookupIdentifier(LookupResult lookupResult, string name, int arity, bool called) {
        var options = LookupOptions.AllMethodsOnArityZero;

        if (called)
            options |= LookupOptions.MustBeInvocableIfMember;

        if (!isInMethodBody && !isInsideNameof)
            options |= LookupOptions.MustNotBeMethodTemplateParameter;

        LookupSymbolsWithFallback(lookupResult, name, arity, options: options);
    }

    private BoundExpression BindMemberAccess(
        MemberAccessExpressionSyntax node,
        bool called,
        bool indexed,
        BelteDiagnosticQueue diagnostics) {
        var boundLeft = BindExpression(node.expression, diagnostics);
        return BindMemberAccessWithBoundLeft(
            node,
            boundLeft,
            node.name,
            node.operatorToken,
            called,
            indexed,
            diagnostics
        );
    }

    private BoundExpression BindReferenceType(ReferenceTypeSyntax node, BelteDiagnosticQueue diagnostics) {
        diagnostics.Push(Error.UnexpectedToken(node.refKeyword.location, node.refKeyword.kind));
        return new BoundTypeExpression(CreateErrorType("ref"));
    }

    private BoundExpression BindQualifiedName(QualifiedNameSyntax node, BelteDiagnosticQueue diagnostics) {
        // TODO Some languages allow "Color Color" member access where the instance name is the same as the type name
        // In which case we would need a special handler for this "BindLeftOfPotentialColorColorMemberAccess"
        // however, currently we disallow that naming convention
        var left = BindExpression(node.left, diagnostics);
        return BindMemberAccessWithBoundLeft(node, left, node.right, node.period, false, false, diagnostics);
    }

    private BoundExpression BindMemberAccessWithBoundLeft(
        ExpressionSyntax node,
        BoundExpression boundLeft,
        SimpleNameSyntax right,
        SyntaxToken operatorToken,
        bool called,
        bool indexed,
        BelteDiagnosticQueue diagnostics) {
        boundLeft = MakeMemberAccessValue(boundLeft, diagnostics);
        var leftType = boundLeft.type;

        if (leftType is not null && leftType.IsVoidType()) {
            // TODO error cannot access void, is this a reachable error?
            return ErrorExpression(boundLeft);
        }

        var lookupResult = LookupResult.GetInstance();

        try {
            var options = LookupOptions.AllMethodsOnArityZero;

            if (called)
                options |= LookupOptions.MustBeInvocableIfMember;

            var templateArgumentsSyntax = right.kind == SyntaxKind.TemplateName
                ? ((TemplateNameSyntax)right).templateArgumentList.arguments
                : default;

            var templateArguments = templateArgumentsSyntax.Count > 0
                ? BindTemplateArguments(templateArgumentsSyntax, diagnostics)
                : default;

            var rightName = right.identifier.text;
            var rightArity = right.arity;
            BoundExpression result;

            switch (boundLeft.kind) {
                case BoundNodeKind.TypeExpression:
                    if (leftType.typeKind == TypeKind.TemplateParameter) {
                        LookupMembersWithFallback(
                            lookupResult,
                            leftType,
                            rightName,
                            rightArity,
                            null,
                            options | LookupOptions.MustNotBeInstance | LookupOptions.MustBeAbstractOrVirtual
                        );

                        if (lookupResult.isMultiViable) {
                            return BindMemberOfType(
                                node,
                                right,
                                rightName,
                                rightArity,
                                indexed,
                                boundLeft,
                                templateArgumentsSyntax,
                                templateArguments,
                                lookupResult,
                                BoundMethodGroupFlags.None,
                                diagnostics
                            );
                        } else if (lookupResult.isClear) {
                            // Error(diagnostics, ErrorCode.ERR_LookupInTypeVariable, boundLeft.Syntax, leftType);
                            // return BadExpression(node, LookupResultKind.NotAValue, boundLeft);
                            // TODO error, is this a reachable error?
                            return ErrorExpression(boundLeft);
                        }
                    } else if (_enclosingNameofArgument == node) {
                        return BindInstanceMemberAccess(
                            node,
                            right,
                            boundLeft,
                            rightName,
                            rightArity,
                            templateArgumentsSyntax,
                            templateArguments,
                            called,
                            indexed,
                            diagnostics
                        );
                    } else {
                        LookupMembersWithFallback(lookupResult, leftType, rightName, rightArity, null, options);

                        if (lookupResult.isMultiViable) {
                            return BindMemberOfType(
                                node,
                                right,
                                rightName,
                                rightArity,
                                indexed,
                                boundLeft,
                                templateArgumentsSyntax,
                                templateArguments,
                                lookupResult,
                                BoundMethodGroupFlags.None,
                                diagnostics
                            );
                        }
                    }

                    break;
                default:
                    if (boundLeft.IsLiteralNull()) {
                        diagnostics.Push(Error.InvalidUnaryOperatorUse(
                            node.location,
                            operatorToken.text,
                            CreateErrorType("<null>")
                        ));

                        return ErrorExpression(boundLeft);
                    } else if (leftType is not null) {
                        boundLeft = CheckValue(boundLeft, BindValueKind.RValue, diagnostics);
                        return BindInstanceMemberAccess(
                            node,
                            right,
                            boundLeft,
                            rightName,
                            rightArity,
                            templateArgumentsSyntax,
                            templateArguments,
                            called,
                            indexed,
                            diagnostics
                        );
                    }

                    break;
            }

            BindMemberAccessReportError(node, right, rightName, boundLeft, lookupResult.error, diagnostics);

            return BindMemberAccessBadResult(
                node,
                rightName,
                boundLeft,
                lookupResult.error,
                lookupResult.symbols.ToImmutable(),
                lookupResult.kind
            );
        } finally {
            lookupResult.Free();
        }
    }

    private BoundExpression BindMemberAccessBadResult(
        SyntaxNode node,
        string nameString,
        BoundExpression boundLeft,
        BelteDiagnostic lookupError,
        ImmutableArray<Symbol> symbols,
        LookupResultKind lookupKind) {
        // TODO Could give BoundMethodGroup and ErrorExpression A LOT more information here like
        // the lookupError, symbols, lookupKind
        if (symbols.Length > 0 && symbols[0].kind == SymbolKind.Method) {
            var builder = ArrayBuilder<MethodSymbol>.GetInstance();

            foreach (var s in symbols)
                if (s is MethodSymbol m) builder.Add(m);

            var methods = builder.ToImmutableAndFree();

            return new BoundMethodGroup(
                nameString,
                methods,
                []
            );
        }

        return ErrorExpression(boundLeft);
    }

    private void BindMemberAccessReportError(
        SyntaxNode node,
        SyntaxNode name,
        string plainName,
        BoundExpression boundLeft,
        BelteDiagnostic lookupError,
        BelteDiagnosticQueue diagnostics) {
        if (lookupError is not null) {
            diagnostics.Push(lookupError);
        } else {
            diagnostics.Push(Error.NoSuchMember(name.location, boundLeft.type, plainName));

            // if (boundLeft.type is null) {
            // TODO is this any different than the above case?
            // } else if (boundLeft.kind is BoundNodeKind.TypeExpression or BoundNodeKind.BaseExpression) {
            //     Error(diagnostics, ErrorCode.ERR_NoSuchMember, name, boundLeft.type, plainName);
        }
    }

    private BoundExpression BindInstanceMemberAccess(
        SyntaxNode node,
        SyntaxNode right,
        BoundExpression boundLeft,
        string rightName,
        int rightArity,
        SeparatedSyntaxList<ArgumentSyntax> templateArgumentsSyntax,
        ImmutableArray<(string, TypeOrConstant)> templateArguments,
        bool called,
        bool indexed,
        BelteDiagnosticQueue diagnostics) {
        // TODO instance member access
        return null;
    }

    private BoundExpression BindMemberOfType(
        SyntaxNode node,
        SyntaxNode right,
        string plainName,
        int arity,
        bool indexed,
        BoundExpression left,
        SeparatedSyntaxList<ArgumentSyntax> templateArgumentsSyntax,
        ImmutableArray<(string, TypeOrConstant)> templateArguments,
        LookupResult lookupResult,
        BoundMethodGroupFlags methodGroupFlags,
        BelteDiagnosticQueue diagnostics) {
        var members = ArrayBuilder<Symbol>.GetInstance();
        BoundExpression result;
        var symbol = GetSymbolOrMethodGroup(
            lookupResult,
            right,
            plainName,
            arity,
            members,
            diagnostics,
            out var wasError,
            qualifier: left is BoundTypeExpression typeExpr ? typeExpr.type : null
        );

        if (symbol is null) {
            result = null;
            // TODO templates
            // result = ConstructBoundMemberGroupAndReportOmittedTypeArguments(
            //     node,
            //     typeArgumentsSyntax,
            //     typeArgumentsWithAnnotations,
            //     left,
            //     plainName,
            //     members,
            //     lookupResult,
            //     methodGroupFlags,
            //     wasError,
            //     diagnostics);
        } else {
            // TODO ensure this is not needed
            // left = ReplaceTypeOrValueReceiver(left, symbol.isStatic || symbol.kind == SymbolKind.NamedType, diagnostics);

            switch (symbol.kind) {
                case SymbolKind.NamedType:
                case SymbolKind.ErrorType:
                    if (IsInstanceReceiver(left) && !wasError) {
                        diagnostics.Push(Error.NoInstanceRequired(right.location, plainName, symbol));
                        wasError = true;
                    }

                    var type = (NamedTypeSymbol)symbol;
                    if (!templateArguments.IsDefault) {
                        // type = ConstructNamedTypeUnlessTypeArgumentOmitted(right, type, typeArgumentsSyntax, typeArgumentsWithAnnotations, diagnostics);
                        // TODO templates
                    }

                    result = new BoundTypeExpression(type, null);
                    break;
                case SymbolKind.Field:
                    result = BindFieldAccess(
                        node,
                        left,
                        (FieldSymbol)symbol,
                        diagnostics,
                        lookupResult.kind,
                        indexed,
                        wasError
                    );

                    break;
                default:
                    throw ExceptionUtilities.UnexpectedValue(symbol.kind);
            }
        }

        members.Free();
        return result;
    }

    private Symbol GetSymbolOrMethodGroup(
        LookupResult result,
        SyntaxNode node,
        string plainName,
        int arity,
        ArrayBuilder<Symbol> methodGroup,
        BelteDiagnosticQueue diagnostics,
        out bool wasError,
        NamespaceOrTypeSymbol qualifier) {
        node = GetNameSyntax(node) ?? node;
        wasError = false;
        Symbol other = null;

        foreach (var symbol in result.symbols) {
            var kind = symbol.kind;
            if (methodGroup.Count > 0) {
                var existingKind = methodGroup[0].kind;

                if (existingKind != kind) {
                    if (existingKind == SymbolKind.Method) {
                        other = symbol;
                        continue;
                    }

                    other = methodGroup[0];
                    methodGroup.Clear();
                }
            }

            if (kind == SymbolKind.Method) {
                methodGroup.Add(symbol);
            } else {
                other = symbol;
            }
        }

        if ((methodGroup.Count > 0) && methodGroup[0].kind == SymbolKind.Method) {
            if ((methodGroup[0].kind == SymbolKind.Method) || (other is null)) {
                if (result.error is not null) {
                    diagnostics.Push(result.error);
                    wasError = result.error.info.severity == DiagnosticSeverity.Error;
                }

                return null;
            }
        }

        methodGroup.Clear();
        return ResultSymbol(result, plainName, arity, node, diagnostics, out wasError, qualifier);
    }

    private static NameSyntax GetNameSyntax(SyntaxNode syntax) {
        return GetNameSyntax(syntax, out _);
    }

    internal static NameSyntax GetNameSyntax(SyntaxNode syntax, out string nameString) {
        nameString = "";

        while (true) {
            switch (syntax.kind) {
                case SyntaxKind.ParenthesizedExpression:
                    syntax = ((ParenthesisExpressionSyntax)syntax).expression;
                    continue;
                case SyntaxKind.CastExpression:
                    syntax = ((CastExpressionSyntax)syntax).expression;
                    continue;
                case SyntaxKind.MemberAccessExpression:
                    return ((MemberAccessExpressionSyntax)syntax).name;
                default:
                    return syntax as NameSyntax;
            }
        }
    }

    private protected BoundExpression BindFieldAccess(
        SyntaxNode node,
        BoundExpression receiver,
        FieldSymbol fieldSymbol,
        BelteDiagnosticQueue diagnostics,
        LookupResultKind resultKind,
        bool indexed,
        bool hasErrors) {
        var hasError = false;

        if (!hasError)
            hasError = CheckInstanceOrStatic(node, receiver, fieldSymbol, ref resultKind, diagnostics);

        ConstantValue constantValueOpt = null;

        if (fieldSymbol.isConst && !isInsideNameof) {
            constantValueOpt = fieldSymbol.GetConstantValue(constantFieldsInProgress);

            if (constantValueOpt == ConstantValue.Unset)
                constantValueOpt = null;
        }

        if (!fieldSymbol.isStatic) {
            // WarnOnAccessOfOffDefault(node, receiver, diagnostics);
            // TODO warning?
        }

        // TODO error
        // if (!IsBadBaseAccess(node, receiver, fieldSymbol, diagnostics)) {
        //     CheckReceiverAndRuntimeSupportForSymbolAccess(node, receiver, fieldSymbol, diagnostics);
        // }

        var fieldType = fieldSymbol.GetFieldType(fieldsBeingBound).type;
        return new BoundFieldAccessExpression(receiver, fieldSymbol, fieldType, constantValueOpt);
    }

    private bool CheckInstanceOrStatic(
        SyntaxNode node,
        BoundExpression receiver,
        Symbol symbol,
        ref LookupResultKind resultKind,
        BelteDiagnosticQueue diagnostics) {
        var instanceReceiver = IsInstanceReceiver(receiver);

        if (!symbol.RequiresInstanceReceiver()) {
            if (instanceReceiver) {
                if (!isInsideNameof) {
                    // TODO error
                    // var error = flags.Includes(BinderFlags.ObjectInitializerMember) ?
                    //     ErrorCode.ERR_StaticMemberInObjectInitializer :
                    //     ErrorCode.ERR_ObjectProhibited;
                    // Error(diagnostics, errorCode, node, symbol);
                } else {
                    return false;
                }

                resultKind = LookupResultKind.StaticInstanceMismatch;
                return true;
            }
        } else {
            if (!instanceReceiver && !isInsideNameof) {
                diagnostics.Push(Error.InstanceRequired(node.location, symbol));
                resultKind = LookupResultKind.StaticInstanceMismatch;
                return true;
            }
        }

        return false;
    }

    private static bool IsInstanceReceiver(BoundExpression receiver) {
        return receiver is not null && receiver.kind == BoundNodeKind.TypeExpression;
    }

    private BoundExpression MakeMemberAccessValue(BoundExpression expression, BelteDiagnosticQueue diagnostics) {
        switch (expression.kind) {
            case BoundNodeKind.MethodGroup: {
                    /*
                        var methodGroup = (BoundMethodGroup)expression;
                        var resolution = ResolveMethodGroup(methodGroup, null);
                        diagnostics.PushRange(resolution.Diagnostics);

                        if (resolution.methodGroup is not null && !resolution.hasAnyErrors) {
                            var method = resolution.methodGroup.methods[0];
                            // Error(diagnostics, ErrorCode.ERR_BadSKunknown, methodGroup.NameSyntax, method, MessageID.IDS_SK_METHOD.Localize());
                            // TODO error
                        }

                        // expression = this.BindMemberAccessBadResult(methodGroup);
                        expression = new BoundErrorExpression(expression.type);
                        resolution.Free();
                        return expression;
                        */
                    // TODO do we even need a special case here?
                    return expression;
                }
            default:
                return expression;
        }
    }

    private BoundExpression BindMethodGroup(
        ExpressionSyntax node,
        bool called,
        bool indexed,
        BelteDiagnosticQueue diagnostics) {
        switch (node.kind) {
            case SyntaxKind.IdentifierName:
            case SyntaxKind.TemplateName:
                return BindIdentifier((SimpleNameSyntax)node, called, indexed, diagnostics);
            case SyntaxKind.MemberAccessExpression:
                return BindMemberAccess((MemberAccessExpressionSyntax)node, called, indexed, diagnostics);
            case SyntaxKind.ParenthesizedExpression:
                return BindMethodGroup(((ParenthesisExpressionSyntax)node).expression, false, false, diagnostics);
            default:
                return BindExpressionInternal(node, diagnostics, called, indexed);
        }
    }

    private BoundExpression BindCallExpression(CallExpressionSyntax node, BelteDiagnosticQueue diagnostics) {
        BoundExpression result;
        var analyzedArguments = AnalyzedArguments.GetInstance();

        if (ReceiverIsInvocation(node, out var nested)) {
            var invocations = ArrayBuilder<CallExpressionSyntax>.GetInstance();

            invocations.Push(node);
            node = nested;

            while (ReceiverIsInvocation(node, out nested)) {
                invocations.Push(node);
                node = nested;
            }

            var boundExpression = BindMethodGroup(node.expression, true, false, diagnostics);

            while (true) {
                result = BindArgumentsAndInvocation(node, boundExpression, analyzedArguments, diagnostics);

                if (!invocations.TryPop(out node))
                    break;

                var memberAccess = (MemberAccessExpressionSyntax)node.expression;
                analyzedArguments.Clear();
                boundExpression = BindMemberAccessWithBoundLeft(
                    memberAccess,
                    result,
                    memberAccess.name,
                    memberAccess.operatorToken,
                    true,
                    false,
                    diagnostics
                );
            }

            invocations.Free();
        } else {
            var boundExpression = BindMethodGroup(node.expression, true, false, diagnostics);
            result = BindArgumentsAndInvocation(node, boundExpression, analyzedArguments, diagnostics);
        }

        analyzedArguments.Free();
        return result;

        BoundExpression BindArgumentsAndInvocation(
            CallExpressionSyntax node,
            BoundExpression boundExpression,
            AnalyzedArguments analyzedArguments,
            BelteDiagnosticQueue diagnostics) {
            boundExpression = CheckValue(boundExpression, BindValueKind.RValueOrMethodGroup, diagnostics);
            var name = boundExpression.kind == BoundNodeKind.MethodGroup ? GetName(node.expression) : null;
            BindArgumentsAndNames(node.argumentList, diagnostics, analyzedArguments);
            return BindCallExpression(node, node.expression, name, boundExpression, analyzedArguments, diagnostics);
        }

        static bool ReceiverIsInvocation(CallExpressionSyntax node, out CallExpressionSyntax nested) {
            if (node.expression is MemberAccessExpressionSyntax {
                expression: CallExpressionSyntax receiver,
                kind: SyntaxKind.MemberAccessExpression
            }) {
                nested = receiver;
                return true;
            }

            nested = null;
            return false;
        }
    }

    private BoundExpression BindCallExpression(
        SyntaxNode node,
        SyntaxNode expression,
        string methodName,
        BoundExpression boundExpression,
        AnalyzedArguments analyzedArguments,
        BelteDiagnosticQueue diagnostics) {
        BoundExpression result;

        if (boundExpression.kind == BoundNodeKind.MethodGroup) {
            result = BindMethodGroupInvocation(
                node,
                expression,
                methodName,
                (BoundMethodGroup)boundExpression,
                analyzedArguments,
                diagnostics
            );
        } else {
            diagnostics.Push(Error.CannotCallNonMethod(expression.location, methodName));
            result = CreateBadCall(node, boundExpression, analyzedArguments);
        }

        return result;
    }

    internal MethodGroupResolution ResolveMethodGroup(
        BoundMethodGroup node,
        SyntaxNode expression,
        string methodName,
        AnalyzedArguments analyzedArguments,
        RefKind returnRefKind = default,
        TypeSymbol returnType = null) {
        var methodResolution = ResolveMethodGroupInternal(
            node,
            expression,
            methodName,
            analyzedArguments,
            returnRefKind,
            returnType
        );

        if (methodResolution.isEmpty && !methodResolution.hasAnyErrors) {
            var diagnostics = BelteDiagnosticQueue.GetInstance();
            diagnostics.AddRange(methodResolution.diagnostics);
            BindMemberAccessReportError(node, diagnostics);

            return new MethodGroupResolution(methodResolution.MethodGroup, methodResolution.otherSymbol, methodResolution.overloadResolutionResult, methodResolution.analyzedArguments, methodResolution.resultKind, diagnostics.ToReadOnlyAndFree());
        }

        return methodResolution;
    }

    private MethodGroupResolution ResolveMethodGroupInternal(
        BoundMethodGroup methodGroup,
        SyntaxNode expression,
        string methodName,
        AnalyzedArguments analyzedArguments,
        ref CompoundUseSiteInfo<AssemblySymbol> useSiteInfo,
        OverloadResolution.Options options,
        RefKind returnRefKind = default,
        TypeSymbol returnType = null,
        in CallingConventionInfo callingConvention = default) {
        var methodResolution = ResolveDefaultMethodGroup(
            methodGroup, analyzedArguments, ref useSiteInfo,
            options,
            returnRefKind, returnType, callingConvention);

        // If the method group's receiver is dynamic then there is no point in looking for extension methods;
        // it's going to be a dynamic invocation.
        if (!methodGroup.SearchExtensionMethods || methodResolution.HasAnyApplicableMethod || methodGroup.MethodGroupReceiverIsDynamic()) {
            return methodResolution;
        }

        var extensionMethodResolution = BindExtensionMethod(
            expression, methodName, analyzedArguments, methodGroup.ReceiverOpt, methodGroup.TypeArgumentsOpt, options,
            returnRefKind: returnRefKind, returnType: returnType, withDependencies: useSiteInfo.AccumulatesDependencies,
            in callingConvention);
        bool preferExtensionMethodResolution = false;

        if (extensionMethodResolution.HasAnyApplicableMethod) {
            preferExtensionMethodResolution = true;
        } else if (extensionMethodResolution.IsEmpty) {
            preferExtensionMethodResolution = false;
        } else if (methodResolution.IsEmpty) {
            preferExtensionMethodResolution = true;
        } else {
            // At this point, both method group resolutions are non-empty but neither contains any applicable method.
            // Choose the MethodGroupResolution with the better (i.e. less worse) result kind.

            Debug.Assert(!methodResolution.HasAnyApplicableMethod);
            Debug.Assert(!extensionMethodResolution.HasAnyApplicableMethod);
            Debug.Assert(!methodResolution.IsEmpty);
            Debug.Assert(!extensionMethodResolution.IsEmpty);

            LookupResultKind methodResultKind = methodResolution.ResultKind;
            LookupResultKind extensionMethodResultKind = extensionMethodResolution.ResultKind;
            if (methodResultKind != extensionMethodResultKind &&
                methodResultKind == extensionMethodResultKind.WorseResultKind(methodResultKind)) {
                preferExtensionMethodResolution = true;
            }
        }

        if (preferExtensionMethodResolution) {
            methodResolution.Free();
            Debug.Assert(!extensionMethodResolution.IsEmpty);
            return extensionMethodResolution;  //NOTE: the first argument of this MethodGroupResolution could be a BoundTypeOrValueExpression
        }

        extensionMethodResolution.Free();

        return methodResolution;
    }

    private BoundExpression BindMethodGroupInvocation(
        SyntaxNode syntax,
        SyntaxNode expression,
        string methodName,
        BoundMethodGroup methodGroup,
        AnalyzedArguments analyzedArguments,
        BelteDiagnosticQueue diagnostics) {
        BoundExpression result = null;

        var resolution = ResolveMethodGroup(
            methodGroup,
            expression,
            methodName,
            analyzedArguments
        );

        diagnostics.AddRange(resolution.diagnostics);

        if (resolution.hasAnyErrors) {
            ImmutableArray<MethodSymbol> originalMethods;
            LookupResultKind resultKind;
            ImmutableArray<TypeWithAnnotations> typeArguments;
            if (resolution.overloadResolutionResult != null) {
                originalMethods = GetOriginalMethods(resolution.overloadResolutionResult);
                resultKind = resolution.MethodGroup.ResultKind;
                typeArguments = resolution.MethodGroup.TypeArguments.ToImmutable();
            } else {
                originalMethods = methodGroup.Methods;
                resultKind = methodGroup.ResultKind;
                typeArguments = methodGroup.TypeArgumentsOpt;
            }

            result = CreateBadCall(
                syntax,
                methodName,
                methodGroup.ReceiverOpt,
                originalMethods,
                resultKind,
                typeArguments,
                analyzedArguments,
                invokedAsExtensionMethod: resolution.IsExtensionMethodGroup,
                isDelegate: false);
        } else if (!resolution.isEmpty) {
            // We're checking resolution.ResultKind, rather than methodGroup.HasErrors
            // to better handle the case where there's a problem with the receiver
            // (e.g. inaccessible), but the method group resolved correctly (e.g. because
            // it's actually an accessible static method on a base type).
            // CONSIDER: could check for error types amongst method group type arguments.
            if (resolution.resultKind != LookupResultKind.Viable) {
                if (resolution.MethodGroup != null) {
                    // we want to force any unbound lambda arguments to cache an appropriate conversion if possible; see 9448.
                    result = BindInvocationExpressionContinued(
                        syntax, expression, methodName, resolution.overloadResolutionResult, resolution.analyzedArguments,
                        resolution.MethodGroup, delegateTypeOpt: null, diagnostics: BindingDiagnosticBag.Discarded, queryClause: queryClause);
                }

                // Since the resolution is non-empty and has no diagnostics, the LookupResultKind in its MethodGroup is uninteresting.
                result = CreateBadCall(syntax, methodGroup, methodGroup.ResultKind, analyzedArguments);
            } else {
                // If overload resolution found one or more applicable methods and at least one argument
                // was dynamic then treat this as a dynamic call.
                if (resolution.analyzedArguments.HasDynamicArgument &&
                    resolution.overloadResolutionResult.HasAnyApplicableMember) {
                    // Note that the runtime binder may consider candidates that haven't passed compile-time final validation
                    // and an ambiguity error may be reported. Also additional checks are performed in runtime final validation
                    // that are not performed at compile-time.
                    // Only if the set of final applicable candidates is empty we know for sure the call will fail at runtime.
                    var finalApplicableCandidates = GetCandidatesPassingFinalValidation(syntax, resolution.overloadResolutionResult,
                                                                                        methodGroup.ReceiverOpt,
                                                                                        methodGroup.TypeArgumentsOpt,
                                                                                        invokedAsExtensionMethod: resolution.IsExtensionMethodGroup,
                                                                                        diagnostics);

                    if (finalApplicableCandidates.Length == 0) {
                        result = CreateBadCall(syntax, methodGroup, methodGroup.ResultKind, analyzedArguments);
                    } else if (finalApplicableCandidates.Length == 1) {
                        Debug.Assert(finalApplicableCandidates[0].IsApplicable);

                        result = TryEarlyBindSingleCandidateInvocationWithDynamicArgument(syntax, expression, methodName, methodGroup, diagnostics, queryClause, resolution, finalApplicableCandidates[0]);

                        if (result is null && finalApplicableCandidates[0].LeastOverriddenMember.MethodKind != MethodKind.LocalFunction) {
                            ReportMemberNotSupportedByDynamicDispatch(syntax, finalApplicableCandidates[0], diagnostics);
                        }
                    }

                    if (result is null) {
                        Debug.Assert(finalApplicableCandidates.Length > 0);

                        if (resolution.IsExtensionMethodGroup) {
                            // error CS1973: 'T' has no applicable method named 'M' but appears to have an
                            // extension method by that name. Extension methods cannot be dynamically dispatched. Consider
                            // casting the dynamic arguments or calling the extension method without the extension method
                            // syntax.

                            // We found an extension method, so the instance associated with the method group must have
                            // existed and had a type.
                            Debug.Assert(methodGroup.InstanceOpt != null && (object)methodGroup.InstanceOpt.Type != null);

                            Error(diagnostics, ErrorCode.ERR_BadArgTypeDynamicExtension, syntax, methodGroup.InstanceOpt.Type, methodGroup.Name);
                            result = CreateBadCall(syntax, methodGroup, methodGroup.ResultKind, analyzedArguments);
                        } else {
                            ReportDynamicInvocationWarnings(syntax, methodGroup, diagnostics, finalApplicableCandidates);

                            result = BindDynamicInvocation(syntax, methodGroup, resolution.analyzedArguments, finalApplicableCandidates.SelectAsArray(r => r.Member), diagnostics, queryClause);
                        }
                    }
                } else {
                    result = BindInvocationExpressionContinued(
                        syntax, expression, methodName, resolution.overloadResolutionResult, resolution.analyzedArguments,
                        resolution.MethodGroup, delegateTypeOpt: null, diagnostics: diagnostics, queryClause: queryClause);
                }
            }
        } else {
            result = CreateBadCall(syntax, methodGroup, analyzedArguments);
        }

        resolution.Free();
        return result;
    }

    private BoundCallExpression CreateBadCall(
        SyntaxNode node,
        BoundExpression expr,
        AnalyzedArguments analyzedArguments) {
        var returnType = new ExtendedErrorTypeSymbol(compilation, "", 0, null);
        var methodContainer = expr.type ?? containingType;
        var method = new ErrorMethodSymbol(methodContainer, returnType, "");

        var args = analyzedArguments.arguments.ToImmutable();
        var argRefKinds = analyzedArguments.refKinds.ToImmutable();

        return new BoundCallExpression(expr, method, args, argRefKinds);
    }

    private void BindArgumentsAndNames(
        ArgumentListSyntax argumentList,
        BelteDiagnosticQueue diagnostics,
        AnalyzedArguments result) {
        if (argumentList is null)
            return;

        var hadError = false;

        foreach (var argumentSyntax in argumentList.arguments)
            BindArgumentAndName(result, diagnostics, ref hadError, argumentSyntax);
    }

    private void BindArgumentAndName(
        AnalyzedArguments result,
        BelteDiagnosticQueue diagnostics,
        ref bool hadError,
        ArgumentSyntax argumentSyntax) {
        var refKind = argumentSyntax.refKeyword is null ? RefKind.None : RefKind.Ref;
        var boundArgument = BindValue(
            argumentSyntax.expression,
            diagnostics,
            refKind == RefKind.None ? BindValueKind.RValue : BindValueKind.RefOrOut
        );

        BindArgumentAndName(
            result,
            diagnostics,
            argumentSyntax,
            boundArgument,
            argumentSyntax.identifier,
            refKind
        );
    }

    private void BindArgumentAndName(
        AnalyzedArguments result,
        BelteDiagnosticQueue diagnostics,
        BelteSyntaxNode argumentSyntax,
        BoundExpression boundArgumentExpression,
        SyntaxToken identifier,
        RefKind refKind) {
        var hasRefKinds = result.refKinds.Any();

        if (refKind != RefKind.None) {
            if (!hasRefKinds) {
                hasRefKinds = true;
                var argCount = result.arguments.Count;

                for (var i = 0; i < argCount; ++i)
                    result.refKinds.Add(RefKind.None);
            }
        }

        if (hasRefKinds)
            result.refKinds.Add(refKind);

        var hasNames = result.names.Any();

        if (identifier is not null) {
            if (!hasNames) {
                var argCount = result.arguments.Count;

                for (var i = 0; i < argCount; ++i)
                    result.names.Add(null);
            }

            result.AddName(identifier);
        } else if (hasNames) {
            diagnostics.Push(Error.NamedBeforeUnnamed(argumentSyntax.location));
            result.names.Add(null);
        }

        result.arguments.Add(boundArgumentExpression);
    }

    private static string GetName(ExpressionSyntax syntax) {
        var nameSyntax = GetNameSyntax(syntax, out var nameString);

        if (nameSyntax is not null)
            return nameSyntax.GetUnqualifiedName().identifier.text;

        return nameString;
    }

    private BoundExpression BindParenthesisExpression(
        ParenthesisExpressionSyntax node,
        BelteDiagnosticQueue diagnostics) {
        var value = BindExpression(node.expression, diagnostics);
        CheckNotType(value, node.expression.location, diagnostics);
        return value;
    }

    private static bool CheckNotType(
        BoundExpression expression,
        TextLocation location,
        BelteDiagnosticQueue diagnostics) {
        switch (expression.kind) {
            case BoundNodeKind.TypeExpression:
                diagnostics.Push(
                    Error.CannotUseType(location, ((BoundTypeExpression)expression).type)
                );

                return false;
            default:
                return true;
        }
    }

    private BoundEmptyExpression BindEmptyExpression(EmptyExpressionSyntax node, BelteDiagnosticQueue diagnostics) {
        return new BoundEmptyExpression();
    }

    private BoundLiteralExpression BindLiteralExpression(
        LiteralExpressionSyntax node,
        BelteDiagnosticQueue diagnostics) {
        var value = node.token.value;

        if (value is null)
            return new BoundLiteralExpression(new ConstantValue(null, SpecialType.None), null);

        var specialType = SpecialTypeExtensions.SpecialTypeFromLiteralValue(value);
        var constantValue = new ConstantValue(value, specialType);
        var type = CorLibrary.GetSpecialType(specialType);
        return new BoundLiteralExpression(constantValue, type);
    }

    private BoundThisExpression BindThisExpression(ThisExpressionSyntax node, BelteDiagnosticQueue diagnostics) {
        if (!HasThis(true, out var inStaticContext)) {
            if (inStaticContext)
                diagnostics.Push(Error.CannotUseThisInStaticMethod(node.location));
            else
                diagnostics.Push(Error.CannotUseThis(node.location));
        } else {
            IsRefOrOutThisParameterCaptured(node.keyword, diagnostics);
        }

        return new BoundThisExpression(containingType);
    }

    private BoundBaseExpression BindBaseExpression(BaseExpressionSyntax node, BelteDiagnosticQueue diagnostics) {
        var hasErrors = false;
        TypeSymbol baseType = containingType?.baseType;

        if (!HasThis(true, out var inStaticContext)) {
            if (inStaticContext)
                diagnostics.Push(Error.CannotUseBaseInStaticMethod(node.location));
            else
                diagnostics.Push(Error.CannotUseBase(node.location));

            hasErrors = true;
        } else if (baseType is null) {
            diagnostics.Push(Error.NoBaseClass(node.location, containingType));
            hasErrors = true;
        } else if (containingType is null || node.parent is null ||
            (node.parent.kind != SyntaxKind.MemberAccessExpression && node.parent.kind != SyntaxKind.IndexExpression)) {
            diagnostics.Push(Error.CannotUseBase(node.location));
            hasErrors = true;
        } else if (IsRefOrOutThisParameterCaptured(node.keyword, diagnostics)) {
            hasErrors = true;
        }

        return new BoundBaseExpression(baseType);
    }

    internal bool HasThis(bool isExplicit, out bool inStaticContext) {
        var member = containingMember;

        if (member?.isStatic == true) {
            inStaticContext = member.kind == SymbolKind.Field || member.kind == SymbolKind.Method;
            return false;
        }

        inStaticContext = false;

        if (_inConstructorInitializer)
            return false;

        if (inFieldInitializer)
            return false;

        return !isExplicit;
    }

    private bool IsRefOrOutThisParameterCaptured(SyntaxNodeOrToken thisOrBaseToken, BelteDiagnosticQueue diagnostics) {
        if (GetDiagnosticIfRefOrOutThisParameterCaptured(thisOrBaseToken.location) is { } diagnostic) {
            diagnostics.Push(diagnostic);
            return true;
        }

        return false;
    }

    private BelteDiagnostic GetDiagnosticIfRefOrOutThisParameterCaptured(TextLocation location) {
        var thisSymbol = containingMember.EnclosingThisSymbol();

        if (thisSymbol is not null &&
            thisSymbol.containingSymbol != containingMember &&
            thisSymbol.refKind != RefKind.None) {
            // TODO error, confirm this is the right one
            return Error.CannotUseThis(location);
        }

        return null;
    }

    #endregion

    #region Lookup

    internal Symbol ResultSymbol(
        LookupResult result,
        string simpleName,
        int arity,
        SyntaxNode where,
        BelteDiagnosticQueue diagnostics,
        out bool wasError,
        NamespaceOrTypeSymbol qualifier,
        LookupOptions options = default) {
        var symbols = result.symbols;
        wasError = false;

        if (result.isMultiViable) {
            if (symbols.Count > 1) {
                symbols.Sort(ConsistentSymbolOrder.Instance);
                var originalSymbols = symbols.ToImmutable();

                var best = GetBestSymbolInfo(symbols, out var secondBest);
                // TODO Could check for conflicting imports here
                var first = symbols[best.index];
                var second = symbols[secondBest.index];

                if (best.isFromSourceModule && !secondBest.isFromSourceModule)
                    return first;

                BelteDiagnostic error = null;
                bool reportError;

                if (first != second && NameAndArityMatchRecursively(first, second)) {
                    reportError = !(best.isFromSourceModule && secondBest.isFromSourceModule);

                    if (first.kind == SymbolKind.NamedType && second.kind == SymbolKind.NamedType) {
                        if (first.originalDefinition == second.originalDefinition) {
                            reportError = true;

                            error = Error.AmbiguousReference(
                                where.location,
                                (where as NameSyntax).ErrorDisplayName() ?? simpleName,
                                first,
                                second
                            );
                        } else {
                            // TODO is this a reachable error?
                            // ErrorCode.ERR_SameFullNameAggAgg: The type '{1}' exists in both '{0}' and '{2}'
                            // info = new CSDiagnosticInfo(ErrorCode.ERR_SameFullNameAggAgg, originalSymbols,
                            //     new object[] { first.ContainingAssembly, first, second.ContainingAssembly });

                            if (secondBest.isFromAddedModule) {
                                reportError = false;
                            }
                        }
                    } else if (first.kind == SymbolKind.Namespace && second.kind == SymbolKind.NamedType) {
                        // TODO is this a reachable error?
                        // ErrorCode.ERR_SameFullNameNsAgg: The namespace '{1}' in '{0}' conflicts with the type '{3}' in '{2}'
                        // info = new CSDiagnosticInfo(ErrorCode.ERR_SameFullNameNsAgg, originalSymbols,
                        //     new object[] { GetContainingAssembly(first), first, second.ContainingAssembly, second });

                        // Do not report this error if namespace is declared in source and the type is declared in added module,
                        // we already reported declaration error about this name collision.
                        if (best.isFromSourceModule && secondBest.isFromAddedModule)
                            reportError = false;
                    } else if (first.kind == SymbolKind.NamedType && second.kind == SymbolKind.Namespace) {
                        if (!secondBest.isFromCompilation || secondBest.isFromSourceModule) {
                            // TODO is this a reachable error?
                            // ErrorCode.ERR_SameFullNameNsAgg: The namespace '{1}' in '{0}' conflicts with the type '{3}' in '{2}'
                            // info = new CSDiagnosticInfo(ErrorCode.ERR_SameFullNameNsAgg, originalSymbols,
                            //     new object[] { GetContainingAssembly(second), second, first.ContainingAssembly, first });
                        } else {
                            // TODO is this a reachable error?
                            // ErrorCode.ERR_SameFullNameThisAggThisNs: The type '{1}' in '{0}' conflicts with the namespace '{3}' in '{2}'
                            // object arg0;

                            // if (best.isFromSourceModule) {
                            //     arg0 = first.GetFirstLocation().SourceTree.FilePath;
                            // } else {
                            //     Debug.Assert(best.IsFromAddedModule);
                            //     arg0 = first.ContainingModule;
                            // }

                            // ModuleSymbol arg2 = second.ContainingModule;

                            // // Merged namespaces that span multiple modules don't have a containing module,
                            // // so just use module with the smallest ordinal from the containing assembly.
                            // if ((object)arg2 == null) {
                            //     foreach (NamespaceSymbol ns in ((NamespaceSymbol)second).ConstituentNamespaces) {
                            //         if (ns.ContainingAssembly == Compilation.Assembly) {
                            //             ModuleSymbol module = ns.ContainingModule;

                            //             if ((object)arg2 == null || arg2.Ordinal > module.Ordinal) {
                            //                 arg2 = module;
                            //             }
                            //         }
                            //     }
                            // }

                            // Debug.Assert(arg2.ContainingAssembly == Compilation.Assembly);

                            // info = new CSDiagnosticInfo(ErrorCode.ERR_SameFullNameThisAggThisNs, originalSymbols,
                            //     new object[] { arg0, first, arg2, second });
                        }
                    } else {
                        diagnostics.Push(Error.AmbiguousReference(
                            where.location,
                            (where as NameSyntax).ErrorDisplayName() ?? simpleName,
                            first,
                            second
                        ));

                        reportError = true;
                    }
                } else {
                    reportError = true;

                    if (first is NamespaceOrTypeSymbol && second is NamespaceOrTypeSymbol) {
                        error = Error.AmbiguousReference(
                            where.location,
                            (where as NameSyntax).ErrorDisplayName() ?? simpleName,
                            first,
                            second
                        );
                    } else {
                        error = Error.AmbiguousMember(where.location, first, second);
                    }
                }

                wasError = true;

                if (reportError && error is not null)
                    diagnostics.Push(error);

                return new ExtendedErrorTypeSymbol(
                    GetContainingNamespaceOrType(symbols[0]),
                    originalSymbols,
                    LookupResultKind.Ambiguous,
                    error,
                    arity);
            } else {
                var singleResult = symbols[0];
                // TODO check if void can appear hear, would need error

                if (singleResult.kind == SymbolKind.ErrorType) {
                    var errorType = (ErrorTypeSymbol)singleResult;

                    if (errorType.unreported) {
                        var error = errorType.error;
                        diagnostics.Push(error);

                        singleResult = new ExtendedErrorTypeSymbol(
                            GetContainingNamespaceOrType(errorType),
                            errorType.name,
                            errorType.arity,
                            error,
                            false
                        );
                    }
                }

                return singleResult;
            }
        }

        wasError = true;

        if (result.kind == LookupResultKind.Empty) {
            var error = Error.UndefinedSymbol(where.location, simpleName);

            return new ExtendedErrorTypeSymbol(
                qualifier ?? compilation.globalNamespaceInternal,
                simpleName,
                arity,
                error
            );
        }

        if (result.error is not null && (qualifier is null || qualifier.kind != SymbolKind.ErrorType))
            diagnostics.Push(result.error);

        if ((symbols.Count > 1) || (symbols[0] is NamespaceOrTypeSymbol) || result.kind == LookupResultKind.NotAType) {
            return new ExtendedErrorTypeSymbol(
                GetContainingNamespaceOrType(symbols[0]),
                symbols.ToImmutable(),
                result.kind,
                result.error,
                arity
            );
        }

        return symbols[0];
    }

    private static bool NameAndArityMatchRecursively(Symbol x, Symbol y) {
        while (true) {
            if (IsRoot(x))
                return IsRoot(y);

            if (IsRoot(y))
                return false;

            if (x.name != y.name || x.GetArity() != y.GetArity())
                return false;

            x = x.containingSymbol;
            y = y.containingSymbol;
        }

        static bool IsRoot(Symbol symbol) {
            return symbol is null || symbol is NamespaceSymbol { isGlobalNamespace: true };
        }
    }

    private Binder LookupSymbolsWithFallback(
        LookupResult result,
        string name,
        int arity,
        ConsList<TypeSymbol> basesBeingResolved = null,
        LookupOptions options = LookupOptions.Default) {
        var binder = LookupSymbolsInternal(result, name, arity, basesBeingResolved, options, false);

        if (result.kind != LookupResultKind.Viable && result.kind != LookupResultKind.Empty) {
            result.Clear();
            LookupSymbolsInternal(result, name, arity, basesBeingResolved, options, true);
        }

        return binder;
    }

    internal void LookupSymbolsSimpleName(
        LookupResult result,
        NamespaceOrTypeSymbol qualifier,
        string plainName,
        int arity,
        ConsList<TypeSymbol> basesBeingResolved,
        LookupOptions options,
        bool diagnose) {
        if (qualifier is null)
            LookupSymbolsInternal(result, plainName, arity, basesBeingResolved, options, diagnose);
        else
            LookupMembersInternal(result, qualifier, plainName, arity, basesBeingResolved, options, this, diagnose);
    }

    private void LookupMembersWithFallback(
        LookupResult result,
        NamespaceOrTypeSymbol namespaceOrType,
        string name,
        int arity,
        ConsList<TypeSymbol> basesBeingResolved = null,
        LookupOptions options = LookupOptions.Default) {
        LookupMembersInternal(
            result,
            namespaceOrType,
            name,
            arity,
            basesBeingResolved,
            options,
            this,
            false
        );

        if (!result.isMultiViable && !result.isClear) {
            result.Clear();
            LookupMembersInternal(
                result,
                namespaceOrType,
                name,
                arity,
                basesBeingResolved,
                options,
                this,
                true
            );
        }
    }

    private protected void LookupMembersInternal(
        LookupResult result,
        NamespaceOrTypeSymbol namespaceOrType,
        string name,
        int arity,
        ConsList<TypeSymbol> basesBeingResolved,
        LookupOptions options,
        Binder originalBinder,
        bool diagnose) {
        if (namespaceOrType.isNamespace) {
            LookupMembersInNamespace(
                result,
                (NamespaceSymbol)namespaceOrType,
                name,
                arity,
                options,
                originalBinder,
                diagnose
            );
        } else {
            LookupMembersInType(
                result,
                (TypeSymbol)namespaceOrType,
                name,
                arity,
                basesBeingResolved,
                options,
                originalBinder,
                diagnose
            );
        }
    }

    private protected void LookupMembersInType(
        LookupResult result,
        TypeSymbol type,
        string name,
        int arity,
        ConsList<TypeSymbol> basesBeingResolved,
        LookupOptions options,
        Binder originalBinder,
        bool diagnose) {
        switch (type.typeKind) {
            case TypeKind.TemplateParameter:
                LookupMembersInTemplateParameter(
                    result,
                    (TemplateParameterSymbol)type,
                    name,
                    arity,
                    basesBeingResolved,
                    options,
                    originalBinder,
                    diagnose
                );

                break;
            case TypeKind.Class:
            case TypeKind.Struct:
            case TypeKind.Array:
                LookupMembersInClass(
                    result,
                    type,
                    name,
                    arity,
                    basesBeingResolved,
                    options,
                    originalBinder,
                    diagnose
                );

                break;
            case TypeKind.Error:
                LookupMembersInErrorType(
                    result,
                    (ErrorTypeSymbol)type,
                    name,
                    arity,
                    basesBeingResolved,
                    options,
                    originalBinder,
                    diagnose
                );

                break;
            default:
                throw ExceptionUtilities.UnexpectedValue(type.typeKind);
        }
    }

    private void LookupMembersInClass(
        LookupResult result,
        TypeSymbol type,
        string name,
        int arity,
        ConsList<TypeSymbol> basesBeingResolved,
        LookupOptions options,
        Binder originalBinder,
        bool diagnose) {
        LookupMembersInClass(result, type, name, arity, basesBeingResolved, options, originalBinder, type, diagnose);
    }

    private void LookupMembersInTemplateParameter(
        LookupResult current,
        TemplateParameterSymbol templateParameter,
        string name,
        int arity,
        ConsList<TypeSymbol> basesBeingResolved,
        LookupOptions options,
        Binder originalBinder,
        bool diagnose) {
        if ((options & LookupOptions.NamespacesOrTypesOnly) != 0)
            return;

        LookupMembersInClass(
            current,
            templateParameter.effectiveBaseClass,
            name,
            arity,
            basesBeingResolved,
            options,
            originalBinder,
            diagnose
        );
    }

    private void LookupMembersInClass(
        LookupResult result,
        TypeSymbol type,
        string name,
        int arity,
        ConsList<TypeSymbol> basesBeingResolved,
        LookupOptions options,
        Binder originalBinder,
        TypeSymbol accessThroughType,
        bool diagnose) {
        var currentType = type;
        var temp = LookupResult.GetInstance();

        PooledHashSet<NamedTypeSymbol> visited = null;

        while (currentType is not null) {
            temp.Clear();

            LookupMembersWithoutInheritance(
                temp,
                currentType,
                name,
                arity,
                options,
                originalBinder,
                accessThroughType,
                diagnose,
                basesBeingResolved
            );

            MergeHidingLookupResults(result, temp, basesBeingResolved);
            var tempHidesMethod = temp.isMultiViable && temp.symbols[0].kind != SymbolKind.Method;

            if (result.isMultiViable && (tempHidesMethod || temp.symbols[0].kind != SymbolKind.Method))
                break;

            if (basesBeingResolved is not null && basesBeingResolved.ContainsReference(type.originalDefinition)) {
                var other = GetNearestOtherSymbol(basesBeingResolved, type);
                var error = Error.CircularBase(type.location, type, other);
                var errorType = new ExtendedErrorTypeSymbol(compilation, name, arity, error, unreported: true);
                result.SetFrom(LookupResult.Good(errorType));
            }

            currentType = currentType.GetNextBaseType(basesBeingResolved, ref visited);
        }

        visited?.Free();
        temp.Free();
    }

    private static Symbol GetNearestOtherSymbol(ConsList<TypeSymbol> list, TypeSymbol type) {
        var other = type;

        for (; list is not null && list != ConsList<TypeSymbol>.Empty; list = list.tail) {
            if (TypeSymbol.Equals(list.head, type.originalDefinition, TypeCompareKind.ConsiderEverything)) {
                if (TypeSymbol.Equals(other, type, TypeCompareKind.ConsiderEverything) &&
                    list.tail is not null &&
                    list.tail != ConsList<TypeSymbol>.Empty) {
                    other = list.tail.head;
                }

                break;
            } else {
                other = list.head;
            }
        }

        return other;
    }

    private void MergeHidingLookupResults(
        LookupResult resultHiding,
        LookupResult resultHidden,
        ConsList<TypeSymbol> basesBeingResolved) {
        if (resultHiding.isMultiViable && resultHidden.isMultiViable) {
            var hidingSymbols = resultHiding.symbols;
            var hidingCount = hidingSymbols.Count;
            var hiddenSymbols = resultHidden.symbols;
            var hiddenCount = hiddenSymbols.Count;

            for (var i = 0; i < hiddenCount; i++) {
                var sym = hiddenSymbols[i];

                for (var j = 0; j < hidingCount; j++) {
                    var hidingSym = hidingSymbols[j];

                    if (hidingSym.kind != SymbolKind.Method || sym.kind != SymbolKind.Method)
                        goto symIsHidden;
                }

                hidingSymbols.Add(sym);
symIsHidden:;
            }
        } else {
            resultHiding.MergePrioritized(resultHidden);
        }
    }

    private protected static void LookupMembersWithoutInheritance(
        LookupResult result,
        TypeSymbol type,
        string name,
        int arity,
        LookupOptions options,
        Binder originalBinder,
        TypeSymbol accessThroughType,
        bool diagnose,
        ConsList<TypeSymbol> basesBeingResolved) {
        var members = GetCandidateMembers(type, name, options, originalBinder);

        foreach (var member in members) {
            var resultOfThisMember = originalBinder.CheckViability(
                member,
                arity,
                options,
                accessThroughType,
                diagnose,
                basesBeingResolved
            );

            result.MergeEqual(resultOfThisMember);
        }
    }

    private void LookupMembersInErrorType(
        LookupResult result,
        ErrorTypeSymbol errorType,
        string name,
        int arity,
        ConsList<TypeSymbol> basesBeingResolved,
        LookupOptions options,
        Binder originalBinder,
        bool diagnose) {
        if (!errorType.candidateSymbols.IsDefault && errorType.candidateSymbols.Length == 1) {
            if (errorType.resultKind == LookupResultKind.Inaccessible) {
                if (errorType.candidateSymbols[0] is TypeSymbol candidateType) {
                    LookupMembersInType(
                        result,
                        candidateType,
                        name,
                        arity,
                        basesBeingResolved,
                        options,
                        originalBinder,
                        diagnose
                    );

                    return;
                }
            }
        }

        result.Clear();
    }


    private static void LookupMembersInNamespace(
        LookupResult result,
        NamespaceSymbol ns,
        string name,
        int arity,
        LookupOptions options,
        Binder originalBinder,
        bool diagnose) {
        var members = GetCandidateMembers(ns, name, options, originalBinder);

        foreach (var member in members) {
            var resultOfThisMember = originalBinder.CheckViability(member, arity, options, null, diagnose);
            result.MergeEqual(resultOfThisMember);
        }
    }

    internal static ImmutableArray<Symbol> GetCandidateMembers(
        NamespaceOrTypeSymbol nsOrType,
        string name,
        LookupOptions options,
        Binder originalBinder) {
        if ((options & LookupOptions.NamespacesOrTypesOnly) != 0 && nsOrType is TypeSymbol)
            return nsOrType.GetTypeMembers(name).Cast<NamedTypeSymbol, Symbol>();
        else
            return nsOrType.GetMembers(name);
    }

    internal static ImmutableArray<Symbol> GetCandidateMembers(
        NamespaceOrTypeSymbol nsOrType,
        LookupOptions options,
        Binder originalBinder) {
        if ((options & LookupOptions.NamespacesOrTypesOnly) != 0 && nsOrType is TypeSymbol)
            return StaticCast<Symbol>.From(nsOrType.GetTypeMembersUnordered());
        else
            return nsOrType.GetMembersUnordered();
    }

    private Binder LookupSymbolsInternal(
        LookupResult result,
        string name,
        int arity,
        ConsList<TypeSymbol> basesBeingResolved,
        LookupOptions options,
        bool diagnose) {
        Binder binder = null;

        for (var scope = this; scope is not null && !result.isMultiViable; scope = scope.next) {
            if (binder is not null) {
                var tmp = LookupResult.GetInstance();
                scope.LookupSymbolsInSingleBinder(tmp, name, arity, basesBeingResolved, options, this, diagnose);
                result.MergeEqual(tmp);
                tmp.Free();
            } else {
                scope.LookupSymbolsInSingleBinder(result, name, arity, basesBeingResolved, options, this, diagnose);

                if (!result.isClear)
                    binder = scope;
            }
        }

        return binder;
    }

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

    internal void AddMemberLookupSymbolsInfo(
        LookupSymbolsInfo result,
        NamespaceOrTypeSymbol namespaceOrType,
        LookupOptions options,
        Binder originalBinder) {
        if (namespaceOrType.isNamespace)
            AddMemberLookupSymbolsInfoInNamespace(result, (NamespaceSymbol)namespaceOrType, options, originalBinder);
        else
            AddMemberLookupSymbolsInfoInType(result, (TypeSymbol)namespaceOrType, options, originalBinder);
    }

    private void AddMemberLookupSymbolsInfoInType(
        LookupSymbolsInfo result,
        TypeSymbol type,
        LookupOptions options,
        Binder originalBinder) {
        switch (type.typeKind) {
            case TypeKind.TemplateParameter:
                AddMemberLookupSymbolsInfoInTemplateParameter(
                    result,
                    (TemplateParameterSymbol)type,
                    options,
                    originalBinder
                );

                break;
            case TypeKind.Class:
            case TypeKind.Struct:
            case TypeKind.Array:
                AddMemberLookupSymbolsInfoInClass(result, type, options, originalBinder, type);
                break;
        }
    }

    private void AddMemberLookupSymbolsInfoInTemplateParameter(
        LookupSymbolsInfo result,
        TemplateParameterSymbol type,
        LookupOptions options,
        Binder originalBinder) {
        var effectiveBaseClass = type.effectiveBaseClass;
        AddMemberLookupSymbolsInfoInClass(result, effectiveBaseClass, options, originalBinder, effectiveBaseClass);
    }

    private void AddMemberLookupSymbolsInfoInClass(
        LookupSymbolsInfo result,
        TypeSymbol type,
        LookupOptions options,
        Binder originalBinder,
        TypeSymbol accessThroughType) {
        PooledHashSet<NamedTypeSymbol> visited = null;

        while (type is not null && !type.IsVoidType()) {
            AddMemberLookupSymbolsInfoWithoutInheritance(result, type, options, originalBinder, accessThroughType);
            type = type.GetNextBaseType(null, ref visited);
        }

        visited?.Free();
    }

    private static void AddMemberLookupSymbolsInfoWithoutInheritance(
        LookupSymbolsInfo result,
        TypeSymbol type,
        LookupOptions options,
        Binder originalBinder,
        TypeSymbol accessThroughType) {
        var candidateMembers = result.filterName is not null
            ? GetCandidateMembers(type, result.filterName, options, originalBinder)
            : GetCandidateMembers(type, options, originalBinder);

        foreach (var symbol in candidateMembers) {
            if (originalBinder.CanAddLookupSymbolInfo(symbol, options, result, accessThroughType))
                result.AddSymbol(symbol, symbol.name, symbol.GetArity());
        }
    }

    private static void AddMemberLookupSymbolsInfoInNamespace(
        LookupSymbolsInfo result,
        NamespaceSymbol ns,
        LookupOptions options,
        Binder originalBinder) {
        var candidateMembers = result.filterName is not null
            ? GetCandidateMembers(ns, result.filterName, options, originalBinder)
            : GetCandidateMembers(ns, options, originalBinder);

        foreach (var symbol in candidateMembers) {
            if (originalBinder.CanAddLookupSymbolInfo(symbol, options, result, null))
                result.AddSymbol(symbol, symbol.name, symbol.GetArity());
        }
    }

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
        BelteDiagnostic error;

        if ((options & LookupOptions.MustNotBeParameter) != 0 && symbol is ParameterSymbol) {
            return LookupResult.Empty();
        } else if (!IsInScopeOfAssociatedSyntaxTree(symbol)) {
            return LookupResult.Empty();
        } else if ((options & (LookupOptions.MustNotBeInstance | LookupOptions.MustBeAbstractOrVirtual)) ==
            (LookupOptions.MustNotBeInstance | LookupOptions.MustBeAbstractOrVirtual) &&
            (symbol is not TypeSymbol && IsInstance(symbol) || !(symbol.isAbstract || symbol.isVirtual))) {
            return LookupResult.Empty();
            // } else if (WrongArity(symbol, arity, diagnose, options, out diagInfo)) {
            //     return LookupResult.WrongArity(symbol, diagInfo);
        } else if ((options & LookupOptions.NamespacesOrTypesOnly) != 0 && symbol is not NamespaceOrTypeSymbol) {
            return LookupResult.NotTypeOrNamespace(symbol, symbol, diagnose);
        } else if ((options & LookupOptions.MustBeInvocableIfMember) != 0
              && IsNonInvocableMember(symbol)) {
            return LookupResult.NotInvocable(symbol, symbol, diagnose);
        } else if (!IsAccessible(
            symbol,
            RefineAccessThroughType(options, accessThroughType),
            out var inaccessibleViaQualifier,
            basesBeingResolved)) {
            if (!diagnose)
                error = null;
            else if (inaccessibleViaQualifier)
                error = Error.InvalidProtectedAccess(symbol.location, accessThroughType, containingType);
            else
                error = Error.MemberIsInaccessible(symbol.location, symbol);

            return LookupResult.Inaccessible(symbol, error);
        } else if ((options & LookupOptions.MustBeInstance) != 0 && !IsInstance(symbol)) {
            error = Error.InstanceRequired(symbol.location, symbol);
            return LookupResult.StaticInstanceMismatch(symbol, error);
        } else if ((options & LookupOptions.MustNotBeInstance) != 0 && IsInstance(symbol)) {
            error = Error.NoInstanceRequired(symbol.location, symbol);
            return LookupResult.StaticInstanceMismatch(symbol, error);
        } else if ((options & LookupOptions.MustNotBeNamespace) != 0 && symbol.kind == SymbolKind.Namespace) {
            // TODO what is this error?
            // diagInfo = diagnose ? new CSDiagnosticInfo(ErrorCode.ERR_BadSKunknown, symbol, symbol.GetKindText()) : null;
            return LookupResult.NotTypeOrNamespace(symbol, null);
        } else {
            return LookupResult.Good(symbol);
        }
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

    internal BoundStatement BindStatement(StatementSyntax node, BelteDiagnosticQueue diagnostics) {
        return node.kind switch {
            SyntaxKind.BlockStatement => BindBlockStatement((BlockStatementSyntax)node, diagnostics),
            SyntaxKind.ReturnStatement => BindReturnStatement((ReturnStatementSyntax)node, diagnostics),
            SyntaxKind.ExpressionStatement => BindExpressionStatement((ExpressionStatementSyntax)node, diagnostics),
            SyntaxKind.LocalDeclarationStatement => BindLocalDeclarationStatement((LocalDeclarationStatementSyntax)node, diagnostics),
            /*
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
            case SyntaxKind.LocalFunctionStatement:
                return new BoundBlockStatement([]);
            */
            _ => throw ExceptionUtilities.UnexpectedValue(node.kind),
        };
    }

    private BoundLocalDeclarationStatement BindLocalDeclarationStatement(
        LocalDeclarationStatementSyntax node,
        BelteDiagnosticQueue diagnostics) {
        var typeSyntax = node.declaration.type.SkipRef(out _);
        var isConst = node.isConst;
        var isConstExpr = node.isConstExpr;

        var declarationType = BindVariableTypeWithAnnotations(
            node.declaration,
            diagnostics,
            typeSyntax,
            ref isConst,
            ref isConstExpr,
            out var isImplicitlyTyped
        );

        var kind = isConstExpr
            ? DataContainerDeclarationKind.ConstantExpression
            : (isConst ? DataContainerDeclarationKind.Constant : DataContainerDeclarationKind.Variable);

        return BindVariableDeclaration(
            kind,
            isImplicitlyTyped,
            node.declaration,
            typeSyntax,
            declarationType,
            diagnostics,
            true,
            node
        );
    }

    private protected BoundLocalDeclarationStatement BindVariableDeclaration(
        DataContainerDeclarationKind kind,
        bool isImplicitlyTyped,
        VariableDeclarationSyntax declaration,
        TypeSyntax typeSyntax,
        TypeWithAnnotations declarationType,
        BelteDiagnosticQueue diagnostics,
        bool includeBoundType,
        BelteSyntaxNode associatedSyntaxNode = null) {
        return BindVariableDeclaration(
            LocateDeclaredVariableSymbol(declaration, typeSyntax),
            kind,
            isImplicitlyTyped,
            declaration,
            typeSyntax,
            declarationType,
            diagnostics,
            includeBoundType,
            associatedSyntaxNode
        );
    }

    private SourceDataContainerSymbol LocateDeclaredVariableSymbol(
        VariableDeclarationSyntax declaration,
        TypeSyntax typeSyntax) {
        return LocateDeclaredVariableSymbol(
            declaration.identifier,
            typeSyntax,
            declaration.initializer,
            DataContainerDeclarationKind.Variable
        );
    }

    private SourceDataContainerSymbol LocateDeclaredVariableSymbol(
        SyntaxToken identifier,
        TypeSyntax typeSyntax,
        EqualsValueClauseSyntax equalsValue,
        DataContainerDeclarationKind kind) {
        var localSymbol = LookupLocal(identifier) ?? SourceDataContainerSymbol.MakeLocal(
            containingMember,
            this,
            false,
            typeSyntax,
            identifier,
            kind,
            equalsValue
        );

        return localSymbol;
    }

    private protected BoundLocalDeclarationStatement BindVariableDeclaration(
        SourceDataContainerSymbol localSymbol,
        DataContainerDeclarationKind kind,
        bool isImplicitlyTyped,
        VariableDeclarationSyntax declaration,
        TypeSyntax typeSyntax,
        TypeWithAnnotations declarationType,
        BelteDiagnosticQueue diagnostics,
        bool includeBoundType,
        BelteSyntaxNode associatedSyntaxNode = null) {
        var localDiagnostics = BelteDiagnosticQueue.GetInstance();
        associatedSyntaxNode ??= declaration;

        var nameConflict = localSymbol.scopeBinder.ValidateDeclarationNameConflictsInScope(localSymbol, diagnostics);
        var hasErrors = false;
        var equalsClauseSyntax = declaration.initializer;

        if (!IsInitializerRefKindValid(
            equalsClauseSyntax,
            declaration,
            localSymbol.refKind,
            diagnostics,
            out var valueKind,
            out var value)) {
            hasErrors = true;
        }

        BoundExpression initializerOpt;
        if (isImplicitlyTyped) {
            initializerOpt = BindInferredVariableInitializer(diagnostics, value, valueKind, declaration);
            var initializerType = initializerOpt?.type;

            if (initializerType is not null) {
                declarationType = new TypeWithAnnotations(initializerType);

                if (declarationType.IsVoidType()) {
                    // Error(localDiagnostics, ErrorCode.ERR_ImplicitlyTypedVariableAssignedBadValue, declarator, declTypeOpt.Type);
                    // TODO is this a reachable error?
                    declarationType = new TypeWithAnnotations(CreateErrorType("var"));
                    hasErrors = true;
                }

                if (!declarationType.type.IsErrorType()) {
                    if (declarationType.isStatic) {
                        diagnostics.Push(Error.CannotInitializeVarWithStaticClass(
                            typeSyntax.location,
                            initializerType
                        ));

                        hasErrors = true;
                    }
                }
            } else {
                declarationType = new TypeWithAnnotations(CreateErrorType("var"));
                hasErrors = true;
            }
        } else {
            if (equalsClauseSyntax is null) {
                initializerOpt = null;
            } else {
                initializerOpt = BindPossibleArrayInitializer(value, declarationType.type, valueKind, diagnostics);
                initializerOpt = GenerateConversionForAssignment(
                    declarationType.type,
                    initializerOpt,
                    localDiagnostics,
                    equalsClauseSyntax.value,
                    localSymbol.refKind != RefKind.None
                        ? ConversionForAssignmentFlags.RefAssignment
                        : ConversionForAssignmentFlags.None
                );
            }
        }

        localSymbol.SetTypeWithAnnotations(declarationType);

        if (kind == DataContainerDeclarationKind.Constant && initializerOpt is not null) {
            var constantValueDiagnostics = localSymbol.GetConstantValueDiagnostics(initializerOpt);
            diagnostics.PushRange(constantValueDiagnostics);
            hasErrors = constantValueDiagnostics.AnyErrors();
        }

        diagnostics.PushRangeAndFree(localDiagnostics);
        BoundTypeExpression boundDeclType = null;

        if (includeBoundType) {
            var invalidDimensions = ArrayBuilder<BoundExpression>.GetInstance();

            typeSyntax.VisitRankSpecifiers((rankSpecifier, args) => {
                var _ = false;
                var size = args.binder.BindArrayDimension(rankSpecifier.size, args.diagnostics, ref _);
                if (size is not null)
                    args.invalidDimensions.Add(size);
            }, (binder: this, invalidDimensions, diagnostics));

            boundDeclType = new BoundTypeExpression(declarationType.type);
        }

        return new BoundLocalDeclarationStatement(new BoundDataContainerDeclaration(localSymbol, initializerOpt));
    }

    internal BoundExpression BindInferredVariableInitializer(
        BelteDiagnosticQueue diagnostics,
        RefKind refKind,
        EqualsValueClauseSyntax initializer,
        BelteSyntaxNode errorSyntax) {
        IsInitializerRefKindValid(initializer, initializer, refKind, diagnostics, out var valueKind, out var value);
        return BindInferredVariableInitializer(diagnostics, value, valueKind, errorSyntax);
    }

    private protected BoundExpression BindInferredVariableInitializer(
        BelteDiagnosticQueue diagnostics,
        ExpressionSyntax initializer,
        BindValueKind valueKind,
        BelteSyntaxNode errorSyntax) {
        if (initializer is null) {
            diagnostics.Push(Error.NoInitOnImplicit(errorSyntax.location));
            return null;
        }

        // TODO Implicit initializer lists/dictionaries
        // if (initializer.kind == SyntaxKind.InitializerListExpression) {
        //     var result = BindUnexpectedArrayInitializer((InitializerExpressionSyntax)initializer,
        //         diagnostics, ErrorCode.ERR_ImplicitlyTypedVariableAssignedArrayInitializer, errorSyntax);

        //     return CheckValue(result, valueKind, diagnostics);
        // }

        return BindValue(initializer, diagnostics, valueKind);
    }

    private BoundExpression BindArrayDimension(
        ExpressionSyntax dimension,
        BelteDiagnosticQueue diagnostics,
        ref bool hasErrors) {
        return BindValue(dimension, diagnostics, BindValueKind.RValue);
    }

    internal bool ValidateDeclarationNameConflictsInScope(Symbol symbol, BelteDiagnosticQueue diagnostics) {
        var location = GetLocation(symbol);
        return ValidateNameConflictsInScope(symbol, location, symbol.name, diagnostics);
    }

    private TextLocation GetLocation(Symbol symbol) {
        return symbol.location ?? symbol.containingSymbol.location;
    }

    private bool ValidateNameConflictsInScope(
        Symbol symbol,
        TextLocation location,
        string name,
        BelteDiagnosticQueue diagnostics) {
        if (string.IsNullOrEmpty(name))
            return false;

        for (var binder = this; binder is not null; binder = binder.next) {
            if (binder is InContainerBinder)
                return false;

            var scope = binder as LocalScopeBinder;
            if (scope?.EnsureSingleDefinition(symbol, name, location, diagnostics) == true)
                return true;

            if (binder.isNestedFunctionBinder)
                return false;

            if (binder.IsLastBinderWithinMember())
                return false;
        }

        return false;
    }

    private bool IsLastBinderWithinMember() {
        var containingMember = this.containingMember;
        return (containingMember?.kind) switch {
            null or SymbolKind.NamedType or SymbolKind.Namespace => true,
            _ => containingMember.containingSymbol?.kind == SymbolKind.NamedType &&
                                next?.containingMember != containingMember,
        };
    }

    private TypeWithAnnotations BindVariableTypeWithAnnotations(
        BelteSyntaxNode declarationNode,
        BelteDiagnosticQueue diagnostics,
        TypeSyntax typeSyntax,
        ref bool isConst,
        ref bool isConstExpr,
        out bool isImplicitlyTyped) {
        var declType = BindTypeOrImplicitType(typeSyntax.SkipRef(out _), diagnostics, out isImplicitlyTyped);

        if (!isImplicitlyTyped) {
            if (declType.isStatic)
                diagnostics.Push(Error.StaticDataContainer(declarationNode.location));
        }

        return declType;
    }

    private BoundBlockStatement BindBlockStatement(BlockStatementSyntax node, BelteDiagnosticQueue diagnostics) {
        var binder = GetBinder(node);
        return binder.BindBlockParts(node, diagnostics);
    }

    private BoundBlockStatement BindBlockParts(BlockStatementSyntax node, BelteDiagnosticQueue diagnostics) {
        var syntaxStatements = node.statements;
        var nStatements = syntaxStatements.Count;

        var boundStatements = ArrayBuilder<BoundStatement>.GetInstance(nStatements);

        for (var i = 0; i < nStatements; i++) {
            var boundStatement = BindStatement(syntaxStatements[i], diagnostics);
            boundStatements.Add(boundStatement);
        }

        var locals = GetDeclaredLocalsForScope(node);
        var localFunctions = GetDeclaredLocalFunctionsForScope(node);

        return new BoundBlockStatement(boundStatements.ToImmutableAndFree(), locals, localFunctions);
    }

    private BoundReturnStatement BindReturnStatement(ReturnStatementSyntax node, BelteDiagnosticQueue diagnostics) {
        var expressionSyntax = node.expression.UnwrapRefExpression(out var refKind);
        BoundExpression argument = null;

        if (expressionSyntax is not null) {
            var requiredValueKind = GetRequiredReturnValueKind(refKind);
            argument = BindValue(expressionSyntax, diagnostics, requiredValueKind);
        }

        var returnType = GetCurrentReturnType(out var signatureRefKind);
        var hasErrors = false;

        if (returnType is not null && refKind != RefKind.None != (signatureRefKind != RefKind.None)) {
            if (refKind == RefKind.None)
                diagnostics.Push(Error.MustNotHaveRefReturn(node.keyword.location));
            else
                diagnostics.Push(Error.MustHaveRefReturn(node.keyword.location));

            hasErrors = true;
        }

        if (argument is not null)
            hasErrors |= argument.type is not null && argument.type.IsErrorType();

        if (hasErrors)
            return new BoundReturnStatement(refKind, argument);

        if (returnType is not null) {
            if (returnType.IsVoidType()) {
                if (argument is not null && containingMember is not SynthesizedEntryPoint) {
                    hasErrors = true;
                    diagnostics.Push(Error.UnexpectedReturnValue(node.keyword.location));
                    // TODO confirm this error has enough info, maybe include containingMember?
                }
            } else {
                if (argument is null) {
                    if (containingMember is not SynthesizedEntryPoint) {
                        hasErrors = true;
                        diagnostics.Push(Error.MissingReturnValue(node.keyword.location));
                    }
                } else {
                    argument = CreateReturnConversion(node, diagnostics, argument, signatureRefKind, returnType);
                }
            }
        } else {
            if (argument?.type is not null &&
                argument.type.IsVoidType() &&
                containingMember is not SynthesizedEntryPoint) {
                diagnostics.Push(Error.UnexpectedReturnValue(node.expression.location));
                hasErrors = true;
            }
        }

        return new BoundReturnStatement(refKind, argument);
    }

    private BoundExpressionStatement BindExpressionStatement(
        ExpressionStatementSyntax node,
        BelteDiagnosticQueue diagnostics) {
        var expression = BindValue(node.expression, diagnostics, BindValueKind.RValue);

        if (!compilation.options.isScript) {
            if (expression is not BoundCallExpression
                          and not BoundAssignmentExpression
                          and not BoundErrorExpression
                          and not BoundEmptyExpression
                          and not BoundCompoundAssignmentExpression
                          and not BoundThrowExpression) {
                diagnostics.Push(Error.InvalidExpressionStatement(node.location));
            }
        }

        return new BoundExpressionStatement(expression);
    }

    private BindValueKind GetRequiredReturnValueKind(RefKind refKind) {
        var requiredValueKind = BindValueKind.RValue;

        if (refKind != RefKind.None) {
            GetCurrentReturnType(out var signatureRefKind);
            requiredValueKind = signatureRefKind == RefKind.Ref ? BindValueKind.RefReturn : BindValueKind.RefConst;
        }

        return requiredValueKind;
    }

    private protected virtual TypeSymbol GetCurrentReturnType(out RefKind refKind) {
        if (containingMember is MethodSymbol symbol) {
            refKind = symbol.refKind;
            return symbol.returnType;
        }

        refKind = RefKind.None;
        return null;
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
        // TODO field initializers
    }

    internal BoundExpressionStatement BindImplicitConstructorInitializer(
        SyntaxNode syntax,
        BelteDiagnosticQueue diagnostics) {
        var call = BindImplicitConstructorInitializer((MethodSymbol)containingMember, diagnostics, compilation);

        if (call is null)
            return null;

        return new BoundExpressionStatement(call);
    }

    internal static BoundExpression BindImplicitConstructorInitializer(
        MethodSymbol constructor,
        BelteDiagnosticQueue diagnostics,
        Compilation compilation
        ) {
        var containingType = constructor.containingType;
        var baseType = containingType.baseType;

        if (baseType is not null) {
            if (baseType.specialType == SpecialType.Object)
                return GenerateBaseParameterlessConstructorInitializer(constructor, diagnostics);
            else if (baseType.IsErrorType() || baseType.isStatic)
                return null;
        }

        if (containingType.IsStructType())
            return null;

        Binder outerBinder;

        if (constructor is not SourceMemberMethodSymbol sourceConstructor) {
            var containerNode = constructor.GetNonNullSyntaxNode();

            if (containerNode is CompilationUnitSyntax)
                containerNode = containingType.syntaxReference.node as TypeDeclarationSyntax;

            var binderFactory = compilation.GetBinderFactory(containerNode.syntaxTree);
            outerBinder = binderFactory.GetInTypeBodyBinder((TypeDeclarationSyntax)containerNode);
        } else {
            var binderFactory = compilation.GetBinderFactory(sourceConstructor.syntaxTree);

            outerBinder = sourceConstructor.syntaxNode switch {
                ConstructorDeclarationSyntax ctorDecl => binderFactory.GetBinder(ctorDecl.parameterList),
                TypeDeclarationSyntax typeDecl => binderFactory.GetInTypeBodyBinder(typeDecl),
                _ => throw ExceptionUtilities.Unreachable(),
            };
        }

        var initializersBinder = outerBinder.WithAdditionalFlagsAndContainingMember(
            BinderFlags.ConstructorInitializer,
            constructor
        );

        return initializersBinder.BindConstructorInitializer(null, constructor, diagnostics);
    }

    internal virtual BoundExpressionStatement BindConstructorInitializer(
        ConstructorInitializerSyntax initializer,
        BelteDiagnosticQueue diagnostics) {
        var call = GetBinder(initializer)
            .BindConstructorInitializer(initializer.argumentList, (MethodSymbol)containingMember, diagnostics);

        return new BoundExpressionStatement(call);
    }

    #endregion

    #region Conversions

    internal BoundExpression CreateReturnConversion(
        SyntaxNode node,
        BelteDiagnosticQueue diagnostics,
        BoundExpression argument,
        RefKind returnRefKind,
        TypeSymbol returnType) {
        var conversion = conversions.ClassifyConversionFromExpression(argument, returnType);

        if (argument is not BoundErrorExpression) {
            if (returnRefKind != RefKind.None) {
                if (conversion.kind != ConversionKind.Identity) {
                    // TODO ref return must have identity conversion?
                } else {
                    return argument;
                }
            }
        }

        return CreateConversion(node, argument, conversion, isCast: false, returnType, diagnostics);
    }

    internal BoundExpression GenerateConversionForAssignment(
        TypeSymbol targetType,
        BoundExpression expression,
        BelteDiagnosticQueue diagnostics,
        SyntaxNode syntax,
        ConversionForAssignmentFlags flags = ConversionForAssignmentFlags.None) {
        return GenerateConversionForAssignment(targetType, expression, diagnostics, out _, syntax, flags);
    }

    internal BoundExpression GenerateConversionForAssignment(
        TypeSymbol targetType,
        BoundExpression expression,
        BelteDiagnosticQueue diagnostics,
        out Conversion conversion,
        SyntaxNode syntax,
        ConversionForAssignmentFlags flags = ConversionForAssignmentFlags.None) {
        if (expression is BoundErrorExpression)
            diagnostics = BelteDiagnosticQueue.Discarded;

        conversion = (flags & ConversionForAssignmentFlags.IncrementAssignment) == 0
            ? conversions.ClassifyConversionFromExpression(expression, targetType)
            : conversions.ClassifyConversionFromType(expression.type, targetType);

        if ((flags & ConversionForAssignmentFlags.RefAssignment) != 0) {
            if (conversion.kind != ConversionKind.Identity) {
                // Error(diagnostics, ErrorCode.ERR_RefAssignmentMustHaveIdentityConversion, expression.Syntax, targetType);
                // TODO ref assignment must have identity conversion?
            } else {
                return expression;
            }
        } else if (!conversion.exists ||
              ((flags & ConversionForAssignmentFlags.CompoundAssignment) == 0
                ? !conversion.isImplicit
                : (conversion.isExplicit && (flags & ConversionForAssignmentFlags.PredefinedOperator) == 0))) {
            if ((flags & ConversionForAssignmentFlags.DefaultParameter) == 0)
                GenerateImplicitConversionError(diagnostics, syntax, conversion, expression, targetType);

            diagnostics = BelteDiagnosticQueue.Discarded;
        }

        return CreateConversion(syntax, expression, conversion, false, targetType, diagnostics);
    }

    private protected void GenerateImplicitConversionError(
        BelteDiagnosticQueue diagnostics,
        SyntaxNode syntax,
        Conversion conversion,
        BoundExpression operand,
        TypeSymbol targetType) {
        if (targetType.typeKind == TypeKind.Error)
            return;

        if (targetType.IsVoidType()) {
            diagnostics.Push(Error.NoImplicitConversion(syntax.location, operand.type, targetType));
            return;
        }

        switch (operand.kind) {
            case BoundNodeKind.ErrorExpression:
                return;
            case BoundNodeKind.MethodGroup:
                diagnostics.Push(Error.MethodGroupCannotBeUsedAsValue(syntax.location, (BoundMethodGroup)operand));
                return;
            case BoundNodeKind.LiteralExpression:
                if (ConstantValue.IsNull(operand.constantValue)) {
                    if (targetType.isPrimitiveType) {
                        // TODO what is this error
                        // diagnostics.Push(Error.ValueCannotBeNull(syntax.location, targetType));
                        return;
                    }
                }

                break;
            case BoundNodeKind.ConditionalExpression: {
                    var conditionalOperator = (BoundConditionalExpression)operand;
                    var reportedError = false;
                    TryConversion(conditionalOperator.center, ref reportedError);
                    TryConversion(conditionalOperator.right, ref reportedError);
                    return;
                }

                void TryConversion(BoundExpression expr, ref bool reportedError) {
                    var conversion = conversions.ClassifyImplicitConversionFromExpression(expr, targetType);

                    if (!conversion.isImplicit || !conversion.exists) {
                        GenerateImplicitConversionError(diagnostics, syntax, conversion, expr, targetType);
                        reportedError = true;
                    }
                }
        }

        var sourceType = operand.type;

        if (sourceType is not null) {
            GenerateImplicitConversionError(
                diagnostics,
                syntax,
                conversion,
                sourceType,
                targetType,
                operand.constantValue
            );

            return;
        }
    }

    private protected static void GenerateImplicitConversionError(
        BelteDiagnosticQueue diagnostics,
        SyntaxNode syntax,
        Conversion conversion,
        TypeSymbol sourceType,
        TypeSymbol targetType,
        ConstantValue sourceConstantValueOpt = null) {
        if (!sourceType.ContainsErrorType() && !targetType.ContainsErrorType()) {
            if (conversion.kind == ConversionKind.ExplicitNullable &&
                conversion.underlyingConversions.FirstOrDefault().isIdentity) {
                diagnostics.Push(Error.CannotConvertImplicitlyNullable(syntax.location, sourceType, targetType));
            } else if (conversion.isExplicit) {
                diagnostics.Push(Error.CannotConvertImplicitly(syntax.location, sourceType, targetType));
            } else {
                diagnostics.Push(Error.CannotConvert(syntax.location, sourceType, targetType));
            }
        }
    }

    internal BoundExpression CreateConversion(
        SyntaxNode node,
        BoundExpression source,
        Conversion conversion,
        bool isCast,
        TypeSymbol destination,
        BelteDiagnosticQueue diagnostics,
        bool hasErrors = false) {
        if (conversion.isIdentity &&
            !isCast &&
            (source.IsLiteralNull() || source.type.Equals(destination, TypeCompareKind.IgnoreNullability))) {
            return source;
        }

        // TODO method group conversion
        // if (conversion.isMethodGroup)
        //     return CreateMethodGroupConversion(node, source, conversion, isCast, destination, diagnostics);

        var constantValue = ConstantFolding.FoldCast(source, new TypeWithAnnotations(destination));

        return new BoundCastExpression(destination, source, conversion, constantValue);
    }

    #endregion
}
