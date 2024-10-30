using System;
using Buckle.CodeAnalysis.Binding;
using Buckle.CodeAnalysis.Syntax;
using Buckle.Diagnostics;

namespace Buckle.CodeAnalysis.Symbols;

internal sealed class SubstitutedLocalSymbol : DataContainerSymbol {
    private readonly DataContainerSymbol _originalLocal;

    internal SubstitutedLocalSymbol(
        DataContainerSymbol originalVariable,
        TypeWithAnnotations type,
        Symbol containingSymbol) {
        _originalLocal = originalVariable;
        typeWithAnnotations = type;
        this.containingSymbol = containingSymbol;
    }

    public override string name => _originalLocal.name;

    public override RefKind refKind => _originalLocal.refKind;

    internal override Symbol containingSymbol { get; }

    internal override TypeWithAnnotations typeWithAnnotations { get; }

    internal override DataContainerDeclarationKind declarationKind => _originalLocal.declarationKind;

    internal override SyntaxNode scopeDesignator => _originalLocal.scopeDesignator;

    internal override SyntaxReference syntaxReference => _originalLocal.syntaxReference;

    internal override bool hasSourceLocation => _originalLocal.hasSourceLocation;

    internal override SyntaxToken identifierToken => _originalLocal.identifierToken;

    internal override bool isCompilerGenerated => _originalLocal.isCompilerGenerated;

    internal override ScopedKind scope => throw new InvalidOperationException();

    internal override SyntaxNode GetDeclarationSyntax() {
        return _originalLocal.GetDeclarationSyntax();
    }

    internal override ConstantValue GetConstantValue(
        SyntaxNode node,
        DataContainerSymbol inProgress,
        BelteDiagnosticQueue diagnostics) {
        return _originalLocal.GetConstantValue(node, inProgress, diagnostics);
    }

    internal override BelteDiagnostic[] GetConstantValueDiagnostics(BoundExpression boundInitValue) {
        return _originalLocal.GetConstantValueDiagnostics(boundInitValue);
    }
}
