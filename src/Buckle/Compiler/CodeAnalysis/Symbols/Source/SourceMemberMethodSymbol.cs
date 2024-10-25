using Buckle.CodeAnalysis.Syntax;
using Buckle.Diagnostics;
using Buckle.Utilities;

namespace Buckle.CodeAnalysis.Symbols;

internal abstract class SourceMemberMethodSymbol : SourceMethodSymbol {
    private protected struct Flags {
        private int _flags;

        private const int MethodKindOffset = 0;
        private const int MethodKindSize = 5;
        private const int MethodKindMask = (1 << MethodKindSize) - 1;

        private const int RefKindOffset = MethodKindOffset + MethodKindSize;
        private const int RefKindSize = 3;
        private const int RefKindMask = (1 << RefKindSize) - 1;

        private const int ReturnsVoidOffset = RefKindOffset + RefKindSize;
        private const int ReturnsVoidSize = 2;

        private const int HasAnyBodyOffset = ReturnsVoidOffset + ReturnsVoidSize;
        private const int HasAnyBodySize = 1;

        private const int HasAnyBodyBit = 1 << HasAnyBodyOffset;
        private const int ReturnsVoidBit = 1 << ReturnsVoidOffset;
        private const int ReturnsVoidIsSetBit = 1 << ReturnsVoidOffset + 1;

        internal readonly bool returnsVoid {
            get {
                var bits = _flags;
                var value = (bits & ReturnsVoidBit) != 0;
                return value;
            }
        }

        internal readonly MethodKind methodKind => (MethodKind)((_flags >> MethodKindOffset) & MethodKindMask);

        internal readonly RefKind refKind => (RefKind)((_flags >> RefKindOffset) & RefKindMask);

        internal readonly bool hasAnyBody => (_flags & HasAnyBodyBit) != 0;

        internal void SetReturnsVoid(bool value) {
            ThreadSafeFlagOperations.Set(ref _flags, ReturnsVoidIsSetBit | (value ? ReturnsVoidBit : 0));
        }


    }
    private protected readonly DeclarationModifiers _modifiers;

    private protected SymbolCompletionState _state;

    private OverriddenOrHiddenMembersResult _lazyOverriddenOrHiddenMembers;

    private protected SourceMemberMethodSymbol(
        NamedTypeSymbol containingType,
        SyntaxReference syntaxReference,
        DeclarationModifiers modifiers)
        : base(syntaxReference) {
        this.containingType = containingType;
        _modifiers = modifiers;
    }

    internal sealed override Symbol containingSymbol => containingType;

    internal override NamedTypeSymbol containingType { get; }

    internal override bool returnsVoid { get; }

    // This allows synthesized methods to also perform method checks without having a conflicting lock
    private protected virtual object _methodChecksLockObject => syntaxReference;

    private protected abstract void MethodChecks(BelteDiagnosticQueue diagnostics);

    private protected void LazyMethodChecks() {
        if (!_state.HasComplete(CompletionParts.FinishMethodChecks)) {

            var lockObject = _methodChecksLockObject;

            lock (lockObject) {
                if (_state.NotePartComplete(CompletionParts.StartMethodChecks)) {
                    var diagnostics = BelteDiagnosticQueue.GetInstance();

                    try {
                        MethodChecks(diagnostics);
                        AddDeclarationDiagnostics(diagnostics);
                    } finally {
                        _state.NotePartComplete(CompletionParts.FinishMethodChecks);
                        diagnostics.Free();
                    }
                }
            }
        }
    }
}
