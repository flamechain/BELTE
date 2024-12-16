using System.Runtime.CompilerServices;
using Buckle.CodeAnalysis.Symbols;
using Buckle.Diagnostics;

namespace Buckle.CodeAnalysis;

/// <summary>
/// Constant value at compile time.
/// </summary>
internal partial class ConstantValue {
    internal static ConstantValue Unset => ConstantValueNull.Uninitialized;

    private protected ConstantValue() { }

    internal ConstantValue(object value) {
        this.value = value;
        specialType = SpecialTypeExtensions.SpecialTypeFromLiteralValue(value);
    }

    internal ConstantValue(object value, SpecialType specialType) {
        this.value = value;
        this.specialType = specialType;
    }

    internal ConstantValue(object value, SpecialType specialType, BelteDiagnostic[] diagnostics) {
        this.value = value;
        this.specialType = specialType;
        this.diagnostics = diagnostics;
    }

    internal object value { get; }

    internal SpecialType specialType { get; }

    internal BelteDiagnostic[] diagnostics { get; }

    internal static bool IsNull(ConstantValue constant) {
        if (constant is not null && constant.value is null)
            return true;

        return false;
    }

    internal static bool IsNotNull(ConstantValue constant) {
        if (constant is not null && constant.value is not null)
            return true;

        return false;
    }

    public override int GetHashCode() {
        return RuntimeHelpers.GetHashCode(this);
    }

    public override bool Equals(object obj) {
        return Equals(obj as ConstantValue);
    }

    public bool Equals(ConstantValue other) {
        if (other is null)
            return false;

        return value == other.value;
    }

    public static bool operator ==(ConstantValue left, ConstantValue right) {
        if (right is null)
            return left is null;

        return (object)left == right || right.Equals(left);
    }

    public static bool operator !=(ConstantValue left, ConstantValue right) {
        if (right is null)
            return left is not null;

        return (object)left != right && !right.Equals(left);
    }
}
