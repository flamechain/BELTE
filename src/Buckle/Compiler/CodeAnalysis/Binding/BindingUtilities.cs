using Buckle.CodeAnalysis.Symbols;
using Buckle.Utilities;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Utilities used during the binding step.
/// </summary>
internal static class BindingUtilities {
    /// <summary>
    /// Gets the root-most <see cref="VariableSymbol"/> from an assignment right hand.
    /// </summary>
    internal static DataContainerSymbol GetAssignedVariableSymbol(BoundExpression expression) {
        if (expression is BoundDataContainerExpression v)
            return v.variable;
        if (expression is BoundFieldAccessExpression f)
            return f.field;
        if (expression is BoundArrayAccessExpression i)
            return GetAssignedVariableSymbol(i.receiver);

        throw ExceptionUtilities.Unreachable();
    }
}
