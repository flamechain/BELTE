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
    public Conversion ClassifyImplicitConversionFromExpression(BoundExpression sourceExpression, TypeSymbol target) {
        var conversion = Conversion.Classify(sourceExpression.type, target);

        if (conversion.isImplicit)
            return conversion;

        return Conversion.None;
        // var sourceType = sourceExpression.Type;

        // if (sourceType is { } && HasIdentityConversionInternal(sourceType, destination))
        //     return Conversion.Identity;

        // var conversion = ClassifyImplicitBuiltInConversionFromExpression(sourceExpression, sourceType, destination);

        // if (conversion.Exists)
        //     return conversion;

        // if (sourceType is { }) {
        //     // Try using the short-circuit "fast-conversion" path.
        //     Conversion fastConversion = FastClassifyConversion(sourceType, destination);
        //     if (fastConversion.Exists) {
        //         if (fastConversion.IsImplicit) {
        //             return fastConversion;
        //         }
        //     } else {
        //         conversion = ClassifyImplicitBuiltInConversionSlow(sourceType, destination, ref useSiteInfo);
        //         if (conversion.Exists) {
        //             return conversion;
        //         }
        //     }
        // } else if (sourceExpression.GetFunctionType() is { } sourceFunctionType) {
        //     if (HasImplicitFunctionTypeConversion(sourceFunctionType, destination, ref useSiteInfo)) {
        //         return Conversion.FunctionType;
        //     }
        // }

        // conversion = GetImplicitUserDefinedConversion(sourceExpression, sourceType, destination, ref useSiteInfo);
        // if (conversion.Exists) {
        //     return conversion;
        // }

        // // The switch expression conversion is "lowest priority", so that if there is a conversion from the expression's
        // // type it will be preferred over the switch expression conversion.  Technically, we would want the language
        // // specification to say that the switch expression conversion only "exists" if there is no implicit conversion
        // // from the type, and we accomplish that by making it lowest priority.  The same is true for the conditional
        // // expression conversion.
        // conversion = GetSwitchExpressionConversion(sourceExpression, destination, ref useSiteInfo);
        // if (conversion.Exists) {
        //     return conversion;
        // }
        // return GetConditionalExpressionConversion(sourceExpression, destination, ref useSiteInfo);
    }
}
