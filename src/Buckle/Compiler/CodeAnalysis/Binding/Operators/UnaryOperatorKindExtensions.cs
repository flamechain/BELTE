
namespace Buckle.CodeAnalysis.Binding;

internal static class UnaryOperatorKindExtensions {
    internal static int OperatorIndex(this UnaryOperatorKind kind) {
        return ((int)kind.Operator() >> 8) - 16;
    }

    internal static UnaryOperatorKind Operator(this UnaryOperatorKind kind) {
        return kind & UnaryOperatorKind.OpMask;
    }
}
