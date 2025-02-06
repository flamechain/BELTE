using Buckle.Diagnostics;

namespace Buckle.CodeAnalysis.Symbols;

internal abstract class SourceFieldSymbol : FieldSymbolWithModifiers {
    private protected SourceFieldSymbol(NamedTypeSymbol containingType) {
        this.containingType = containingType;
    }

    public abstract override string name { get; }

    internal sealed override Symbol containingSymbol => containingType;

    internal override NamedTypeSymbol containingType { get; }

    internal sealed override bool requiresCompletion => true;

    internal bool isNew => (_modifiers & DeclarationModifiers.New) != 0;

    private protected void CheckAccessibility(BelteDiagnosticQueue diagnostics) {
        ModifierHelpers.CheckAccessibility(_modifiers, diagnostics, errorLocation);
    }

    private protected void ReportModifiersDiagnostics(BelteDiagnosticQueue diagnostics) {
        // if (containingType.isSealed && declaredAccessibility == Accessibility.Protected) {
        //     diagnostics.Add(AccessCheck.GetProtectedMemberInSealedTypeError(containingType), ErrorLocation, this);
        // } else if (IsVolatile && IsReadOnly) {
        //     diagnostics.Add(ErrorCode.ERR_VolatileAndReadonly, ErrorLocation, this);
        // } else if (containingType.IsStatic && !IsStatic) {
        //     diagnostics.Add(ErrorCode.ERR_InstanceMemberInStaticClass, ErrorLocation, this);
        // } else if (!IsStatic && !IsReadOnly && containingType.IsReadOnly) {
        //     diagnostics.Add(ErrorCode.ERR_FieldsInRoStruct, ErrorLocation);
        // }

        // TODO
    }
}
