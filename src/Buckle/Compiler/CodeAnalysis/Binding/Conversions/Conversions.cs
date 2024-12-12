using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

internal sealed class Conversions {
    // TODO This class could be much more complex
    private readonly Binder _binder;

    internal Conversions(Binder binder) {
        _binder = binder;
    }

    internal Conversion ClassifyConversionFromExpression(BoundExpression sourceExpression, TypeSymbol target) {
        return Conversion.Classify(sourceExpression.type, target);
    }

    internal Conversion ClassifyConversionFromType(TypeSymbol source, TypeSymbol target) {
        return Conversion.Classify(source, target);
    }
}
