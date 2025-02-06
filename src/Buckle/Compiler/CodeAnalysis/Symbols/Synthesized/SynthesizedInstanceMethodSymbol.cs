using System.Threading;

namespace Buckle.CodeAnalysis.Symbols;

internal abstract class SynthesizedInstanceMethodSymbol : MethodSymbol {
    private ParameterSymbol _lazyThisParameter;

    internal override bool TryGetThisParameter(out ParameterSymbol thisParameter) {
        if (_lazyThisParameter is null)
            Interlocked.CompareExchange(ref _lazyThisParameter, new ThisParameterSymbol(this), null);

        thisParameter = _lazyThisParameter;
        return true;
    }
}
