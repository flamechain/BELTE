using Microsoft.CodeAnalysis.PooledObjects;

namespace Buckle.CodeAnalysis.Binding;

internal sealed class BinaryOperatorOverloadResolutionResult {
    internal static readonly ObjectPool<BinaryOperatorOverloadResolutionResult> Pool = CreatePool();

    internal readonly ArrayBuilder<BinaryOperatorAnalysisResult> results;

    private BinaryOperatorOverloadResolutionResult() {
        results = new ArrayBuilder<BinaryOperatorAnalysisResult>(10);
    }

    internal bool AnyValid() {
        foreach (var result in results) {
            if (result.isValid)
                return true;
        }

        return false;
    }

    internal bool SingleValid() {
        var oneValid = false;

        foreach (var result in results) {
            if (result.isValid) {
                if (oneValid)
                    return false;

                oneValid = true;
            }
        }

        return oneValid;
    }

    internal BinaryOperatorAnalysisResult best {
        get {
            BinaryOperatorAnalysisResult best = default;

            foreach (var result in results) {
                if (result.isValid) {
                    if (best.isValid)
                        return default;

                    best = result;
                }
            }

            return best;
        }
    }

    internal static BinaryOperatorOverloadResolutionResult GetInstance() {
        return Pool.Allocate();
    }

    internal void Free() {
        Clear();
        Pool.Free(this);
    }

    internal void Clear() {
        results.Clear();
    }

    private static ObjectPool<BinaryOperatorOverloadResolutionResult> CreatePool() {
        ObjectPool<BinaryOperatorOverloadResolutionResult> pool = null;

        pool = new ObjectPool<BinaryOperatorOverloadResolutionResult>(
            () => new BinaryOperatorOverloadResolutionResult(), 10
        );

        return pool;
    }
}
