using System;
using System.Collections.Immutable;
using Microsoft.CodeAnalysis.PooledObjects;

namespace Buckle.CodeAnalysis;

internal static class ArrayBuilderExtensions {
    // These methods allow using an array builder as a stack
    internal static void Push<T>(this ArrayBuilder<T> builder, T e) {
        builder.Add(e);
    }

    internal static T Pop<T>(this ArrayBuilder<T> builder) {
        var e = builder.Peek();
        builder.RemoveAt(builder.Count - 1);
        return e;
    }

    internal static bool TryPop<T>(this ArrayBuilder<T> builder, out T result) {
        if (builder.Count > 0) {
            result = builder.Pop();
            return true;
        }

        result = default;
        return false;
    }

    internal static T Peek<T>(this ArrayBuilder<T> builder) {
        return builder[^1];
    }

    internal static bool All<T>(this ArrayBuilder<T> builder, Func<T, bool> predicate) {
        foreach (var item in builder) {
            if (!predicate(item))
                return false;
        }

        return true;
    }

    internal static ImmutableArray<U> ToDowncastedImmutableAndFree<T, U>(this ArrayBuilder<T> builder) where U : T {
        var result = builder.ToDowncastedImmutable<U>();
        builder.Free();
        return result;
    }
}
