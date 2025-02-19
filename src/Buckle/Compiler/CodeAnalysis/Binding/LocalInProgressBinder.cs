using System.Threading;
using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

internal sealed class LocalInProgressBinder : Binder {
    internal readonly EqualsValueClauseSyntax initializerSyntax;
    private DataContainerSymbol _localSymbol;

    internal LocalInProgressBinder(EqualsValueClauseSyntax initializerSyntax, Binder next)
        : base(next) {
        this.initializerSyntax = initializerSyntax;
    }

    internal override DataContainerSymbol localInProgress => _localSymbol;

    internal void SetLocalSymbol(DataContainerSymbol local) {
        Interlocked.CompareExchange(ref _localSymbol, local, null);
    }
}
