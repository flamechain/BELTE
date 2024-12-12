
namespace Buckle.CodeAnalysis.Binding;

internal static class BinaryOperatorKindExtensions {
    internal static int OperatorIndex(this BinaryOperatorKind kind) {
        return ((int)kind.Operator() >> 8) - 16;
    }

    internal static BinaryOperatorKind Operator(this BinaryOperatorKind kind) {
        return kind & BinaryOperatorKind.OpMask;
    }

    internal static bool IsConditional(this BinaryOperatorKind kind) {
        return 0 != (kind & BinaryOperatorKind.Conditional);
    }
}
