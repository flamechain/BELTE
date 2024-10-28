using System.Threading;
using Buckle.CodeAnalysis.Binding;
using Buckle.CodeAnalysis.Syntax;
using Buckle.Diagnostics;
using Buckle.Utilities;

namespace Buckle.CodeAnalysis.Symbols;

internal partial class SourceDataContainerSymbol : DataContainerSymbol {
    private readonly TypeSyntax _typeSyntax;

    private TypeWithAnnotations _type;

    private SourceDataContainerSymbol(
        Symbol containingSymbol,
        Binder scopeBinder,
        bool allowRefKind,
        TypeSyntax typeSyntax,
        SyntaxToken identifierToken,
        DataContainerDeclarationKind declarationKind) {
        this.containingSymbol = containingSymbol;
        this.scopeBinder = scopeBinder;
        this.declarationKind = declarationKind;
        this.identifierToken = identifierToken;
        _typeSyntax = typeSyntax;

        if (allowRefKind) {
            typeSyntax.SkipRef(out var refKind);
            this.refKind = refKind;
        }

        scope = refKind == RefKind.None ? ScopedKind.Value : ScopedKind.Ref;
    }

    public override string name => identifierToken.text;

    public override RefKind refKind { get; }

    internal Binder scopeBinder { get; }

    internal override Symbol containingSymbol { get; }

    internal override SyntaxNode scopeDesignator => scopeBinder.scopeDesignator;

    internal override DataContainerDeclarationKind declarationKind { get; }

    internal override ScopedKind scope { get; }

    internal override SyntaxToken identifierToken { get; }

    internal override bool hasSourceLocation => true;

    internal override SyntaxReference syntaxReference => new SyntaxReference(GetDeclarationSyntax());

    internal override bool isCompilerGenerated => false;

    internal override TypeWithAnnotations typeWithAnnotations {
        get {
            if (_type is null) {
                var localType = GetTypeSymbol();
                SetTypeWithAnnotations(localType);
            }

            return _type;
        }
    }

    internal bool isImplicitlyTyped {
        get {
            if (_typeSyntax is null)
                return true;

            var typeSyntax = _typeSyntax.SkipRef(out _);

            if (typeSyntax.isImplicitlyTyped) {
                scopeBinder.BindTypeOrImplicitType(
                    typeSyntax,
                    BelteDiagnosticQueue.Discarded,
                    out var result
                );

                return result;
            }

            return false;
        }
    }

    internal static SourceDataContainerSymbol MakeLocal(
        Symbol containingSymbol,
        Binder scopeBinder,
        bool allowRefKind,
        TypeSyntax typeSyntax,
        SyntaxToken identifierToken,
        DataContainerDeclarationKind declarationKind,
        EqualsValueClauseSyntax initializer,
        Binder initializerBinder = null) {
        return MakeDataContainer(
            containingSymbol,
            scopeBinder,
            allowRefKind,
            typeSyntax,
            identifierToken,
            declarationKind,
            initializer,
            initializerBinder ?? scopeBinder
        );
    }

    internal static SourceDataContainerSymbol MakeGlobal(
        NamespaceSymbol globalNamespace,
        TypeSyntax typeSyntax,
        SyntaxToken identifierToken,
        DataContainerDeclarationKind declarationKind,
        EqualsValueClauseSyntax initializer,
        Binder initializerBinder) {
        return MakeDataContainer(
            globalNamespace,
            initializerBinder,
            true,
            typeSyntax,
            identifierToken,
            declarationKind,
            initializer,
            initializerBinder
        );
    }

    internal sealed override SyntaxNode GetDeclarationSyntax() {
        return identifierToken.parent;
    }

    internal override ConstantValue GetConstantValue(
        SyntaxNode node,
        DataContainerSymbol inProgress,
        BelteDiagnosticQueue diagnostics) {
        return null;
    }

    internal override BelteDiagnostic[] GetConstantValueDiagnostics(BoundExpression boundInitValue) {
        return [];
    }

    internal void SetTypeWithAnnotations(TypeWithAnnotations newType) {
        if (_type is null)
            Interlocked.CompareExchange(ref _type, newType, null);
    }

    private protected virtual TypeWithAnnotations InferTypeOfImplicit(BelteDiagnosticQueue diagnostics) {
        return _type;
    }

    private static SourceDataContainerSymbol MakeDataContainer(
        Symbol containingSymbol,
        Binder scopeBinder,
        bool allowRefKind,
        TypeSyntax typeSyntax,
        SyntaxToken identifierToken,
        DataContainerDeclarationKind declarationKind,
        EqualsValueClauseSyntax initializer,
        Binder initializerBinder) {
        return initializer is null
            ? new SourceDataContainerSymbol(
                containingSymbol,
                scopeBinder,
                allowRefKind,
                typeSyntax,
                identifierToken,
                declarationKind
              )
            : new SourceDataContainerWithInitializerSymbol(
                containingSymbol,
                scopeBinder,
                typeSyntax,
                identifierToken,
                initializer,
                initializerBinder,
                declarationKind
              );
    }

    private TypeWithAnnotations GetTypeSymbol() {
        var diagnostics = BelteDiagnosticQueue.Discarded;

        bool isImplicitlyTyped;
        TypeWithAnnotations declarationType;

        if (_typeSyntax is null) {
            isImplicitlyTyped = true;
            declarationType = default;
        } else {
            declarationType = scopeBinder.BindTypeOrImplicitType(
                _typeSyntax.SkipRef(out _),
                diagnostics,
                out isImplicitlyTyped
            );
        }

        if (isImplicitlyTyped) {
            var inferredType = InferTypeOfImplicit(diagnostics);

            if (inferredType.hasType && !inferredType.IsVoidType())
                declarationType = inferredType;
            else
                declarationType = new TypeWithAnnotations(scopeBinder.CreateErrorType("var"));
        }

        return declarationType;
    }

    internal sealed override bool Equals(Symbol obj, TypeCompareKind compareKind) {
        if ((object)obj == this)
            return true;

        return obj is SourceDataContainerSymbol symbol
            && symbol.identifierToken.Equals(identifierToken)
            && symbol.containingSymbol.Equals(containingSymbol, compareKind);
    }

    public sealed override int GetHashCode() {
        return Hash.Combine(identifierToken.GetHashCode(), containingSymbol.GetHashCode());
    }
}
