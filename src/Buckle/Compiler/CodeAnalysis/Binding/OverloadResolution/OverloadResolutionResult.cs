using System.Collections.Immutable;
using Buckle.CodeAnalysis.Symbols;
using Buckle.Utilities;
using Microsoft.CodeAnalysis.PooledObjects;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// The results of the <see cref="OverloadResolution" />. Describes if it succeeded, and which method was picked if it
/// succeeded.
/// </summary>
internal sealed class OverloadResolutionResult<T> where T : Symbol {
    private static readonly ObjectPool<OverloadResolutionResult<T>> Pool = CreatePool();

    private ThreeState _bestResultState;
    private MemberResolutionResult<T> _bestResult;

    internal readonly ArrayBuilder<MemberResolutionResult<T>> resultsBuilder;

    internal OverloadResolutionResult() {
        resultsBuilder = [];
    }

    internal bool succeeded {
        get {
            EnsureBestResultLoaded();
            return _bestResultState == ThreeState.True && _bestResult.result.isValid;
        }
    }

    internal MemberResolutionResult<T> bestResult {
        get {
            EnsureBestResultLoaded();
            return _bestResult;
        }
    }

    internal ImmutableArray<MemberResolutionResult<T>> results => resultsBuilder.ToImmutable();

    internal bool hasAnyApplicableMember {
        get {
            foreach (var result in resultsBuilder) {
                if (result.result.isApplicable)
                    return true;
            }

            return false;
        }
    }

    internal void Clear() {
        _bestResult = default;
        _bestResultState = ThreeState.Unknown;
        resultsBuilder.Clear();
    }

    internal static OverloadResolutionResult<T> GetInstance() {
        return Pool.Allocate();
    }

    internal void Free() {
        Clear();
        Pool.Free(this);
    }

    private static ObjectPool<OverloadResolutionResult<T>> CreatePool() {
        ObjectPool<OverloadResolutionResult<T>> pool = null;
        pool = new ObjectPool<OverloadResolutionResult<T>>(() => new OverloadResolutionResult<T>(), 10);
        return pool;
    }

    private void EnsureBestResultLoaded() {
        if (!_bestResultState.HasValue())
            _bestResultState = TryGetBestResult(resultsBuilder, out _bestResult);
    }

    private static ThreeState TryGetBestResult(
        ArrayBuilder<MemberResolutionResult<T>> allResults,
        out MemberResolutionResult<T> best) {
        best = default;
        var haveBest = ThreeState.False;

        foreach (var pair in allResults) {
            if (pair.result.isValid) {
                if (haveBest == ThreeState.True) {
                    best = default;
                    return ThreeState.False;
                }

                haveBest = ThreeState.True;
                best = pair;
            }
        }

        return haveBest;
    }
}
