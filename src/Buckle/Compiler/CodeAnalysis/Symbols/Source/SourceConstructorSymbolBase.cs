using System.Collections.Immutable;
using Buckle.CodeAnalysis.Binding;
using Buckle.CodeAnalysis.Syntax;
using Buckle.Diagnostics;
using Buckle.Libraries;

namespace Buckle.CodeAnalysis.Symbols;

internal abstract class SourceConstructorSymbolBase : SourceMemberMethodSymbol {
    private protected ImmutableArray<ParameterSymbol> _lazyParameters;
    private TypeWithAnnotations _lazyReturnType;

    private protected SourceConstructorSymbolBase(
        SourceMemberContainerTypeSymbol containingType,
        BelteSyntaxNode syntax,
        (DeclarationModifiers declarationModifiers, Flags flags) modifiersAndFlags)
        : base(containingType, new SyntaxReference(syntax), modifiersAndFlags) { }

    public sealed override string name => WellKnownMemberNames.InstanceConstructorName;

    public sealed override ImmutableArray<TemplateParameterSymbol> templateParameters => [];

    public sealed override ImmutableArray<BoundExpression> templateConstraints => [];

    internal sealed override int parameterCount {
        get {
            if (_lazyParameters.IsDefault)
                return GetParameterList().parameters.Count;

            return _lazyParameters.Length;
        }
    }

    internal sealed override ImmutableArray<ParameterSymbol> parameters {
        get {
            LazyMethodChecks();
            return _lazyParameters;
        }
    }

    internal sealed override TypeWithAnnotations returnTypeWithAnnotations {
        get {
            LazyMethodChecks();
            return _lazyReturnType;
        }
    }

    private protected abstract bool _allowRef { get; }

    internal sealed override void AfterAddingTypeMembersChecks(BelteDiagnosticQueue diagnostics) {
        base.AfterAddingTypeMembersChecks(diagnostics);

        foreach (var parameter in parameters)
            parameter.type.CheckAllConstraints(declaringCompilation, parameter.syntaxReference.location, diagnostics);
    }

    internal sealed override ImmutableArray<ImmutableArray<TypeWithAnnotations>> GetTypeParameterConstraintTypes() {
        return [];
    }

    internal sealed override ImmutableArray<TypeParameterConstraintKinds> GetTypeParameterConstraintKinds() {
        return [];
    }

    private protected abstract ParameterListSyntax GetParameterList();

    private protected sealed override void MethodChecks(BelteDiagnosticQueue diagnostics) {
        var syntax = (BelteSyntaxNode)syntaxReference.node;
        var binderFactory = declaringCompilation.GetBinderFactory(syntax.syntaxTree);
        var parameterList = GetParameterList();

        var bodyBinder = binderFactory.GetBinder(parameterList, syntax, this).WithContainingMember(this);
        var signatureBinder = bodyBinder.WithAdditionalFlagsAndContainingMember(BinderFlags.SuppressConstraintChecks, this);

        _lazyParameters = ParameterHelpers.MakeParameters(
            signatureBinder,
            this,
            parameterList.parameters,
            diagnostics,
            _allowRef,
            false
        ).Cast<SourceParameterSymbol, ParameterSymbol>();

        _lazyReturnType = new TypeWithAnnotations(CorLibrary.GetSpecialType(SpecialType.Void));

        CheckEffectiveAccessibility(_lazyReturnType, _lazyParameters, diagnostics);
    }
}
