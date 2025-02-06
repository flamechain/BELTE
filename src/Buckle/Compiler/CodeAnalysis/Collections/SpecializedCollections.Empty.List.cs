using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;

namespace Buckle.CodeAnalysis;

internal static partial class SpecializedCollections {
    private partial class Empty {
        internal class List<T> : Collection<T>, IList<T>, IReadOnlyList<T> {
            public static readonly List<T> Instance = new();

            private protected List() { }

            public new int IndexOf(T item) {
                return -1;
            }

            public new void Insert(int index, T item) {
                throw new NotSupportedException();
            }

            public new void RemoveAt(int index) {
                throw new NotSupportedException();
            }

            public new T this[int index] {
                get {
                    throw new ArgumentOutOfRangeException(nameof(index));
                }

                set {
                    throw new NotSupportedException();
                }
            }
        }
    }
}
