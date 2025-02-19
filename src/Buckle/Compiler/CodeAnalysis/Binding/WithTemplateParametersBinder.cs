using System.Collections.Generic;
using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

internal abstract class WithTemplateParametersBinder : Binder {
    internal WithTemplateParametersBinder(Binder next) : base(next) { }

    private protected abstract Dictionary<string, List<TemplateParameterSymbol>> _templateParameterMap { get; }

    private protected virtual LookupOptions _lookupMask => LookupOptions.MustBeInvocableIfMember;

    internal override void LookupSymbolsInSingleBinder(
        LookupResult result,
        string name,
        int arity,
        ConsList<TypeSymbol> basesBeingResolved,
        LookupOptions options,
        Binder originalBinder,
        bool diagnose) {
        if ((options & _lookupMask) != 0)
            return;

        foreach (var templateParameter in _templateParameterMap[name])
            result.MergeEqual(originalBinder.CheckViability(templateParameter, arity, options, null, diagnose));
    }

    private protected bool CanConsiderTypeParameters(LookupOptions options) {
        return (options & (_lookupMask | LookupOptions.MustBeInstance)) == 0;
    }
}
