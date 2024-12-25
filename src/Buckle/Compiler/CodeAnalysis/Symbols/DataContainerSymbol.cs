using Buckle.CodeAnalysis.Binding;
using Buckle.CodeAnalysis.Syntax;
using Buckle.Diagnostics;

namespace Buckle.CodeAnalysis.Symbols;

internal abstract class DataContainerSymbol : Symbol, IDataContainerSymbol {
    public sealed override SymbolKind kind => SymbolKind.Local;

    public bool isConst => declarationKind == DataContainerDeclarationKind.Constant;

    public bool isConstExpr => declarationKind == DataContainerDeclarationKind.ConstantExpression;

    public bool isNullable => typeWithAnnotations.isNullable;

    public bool isRef => refKind != RefKind.None;

    public abstract RefKind refKind { get; }

    public bool hasConstantValue {
        get {
            if (!isConstExpr)
                return false;

            var constant = GetConstantValue(null, null, null);
            return constant is not null;
        }
    }

    public object constantValue {
        get {
            if (!isConstExpr)
                return false;

            var constant = GetConstantValue(null, null, null);
            return constant?.value;
        }
    }

    internal sealed override bool isSealed => false;

    internal sealed override bool isAbstract => false;

    internal sealed override bool isOverride => false;

    internal sealed override bool isVirtual => false;

    internal sealed override bool isStatic => false;

    internal sealed override Accessibility declaredAccessibility => Accessibility.NotApplicable;

    internal abstract SyntaxToken identifierToken { get; }

    internal abstract TypeWithAnnotations typeWithAnnotations { get; }

    internal abstract DataContainerDeclarationKind declarationKind { get; }

    internal abstract bool hasSourceLocation { get; }

    internal abstract bool isCompilerGenerated { get; }

    internal abstract SyntaxNode scopeDesignator { get; }

    internal abstract ScopedKind scope { get; }

    internal virtual bool isWritableVariable => declarationKind switch {
        DataContainerDeclarationKind.Constant or DataContainerDeclarationKind.ConstantExpression => false,
        _ => true,
    };

    internal TypeSymbol type => typeWithAnnotations.type;

    internal bool isGlobal => containingSymbol is SynthesizedEntryPoint;

    internal abstract ConstantValue GetConstantValue(
        SyntaxNode node,
        DataContainerSymbol inProgress,
        BelteDiagnosticQueue diagnostics);

    internal abstract BelteDiagnosticQueue GetConstantValueDiagnostics(BoundExpression boundInitValue);

    internal abstract SyntaxNode GetDeclarationSyntax();

    ITypeSymbol IDataContainerSymbol.type => type;
}
