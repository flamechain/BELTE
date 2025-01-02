
using System.Collections.Immutable;

namespace Buckle.CodeAnalysis.Binding;

internal struct MemberAnalysisResult {
    internal readonly ImmutableArray<Conversion> conversions;
    internal readonly BitVector BadArgumentsOpt;
    internal readonly ImmutableArray<int> ArgsToParamsOpt;
    internal readonly ImmutableArray<TypeParameterDiagnosticInfo> ConstraintFailureDiagnostics;

    internal readonly int BadParameter;
    internal readonly MemberResolutionKind Kind;
    internal readonly TypeWithAnnotations DefinitionParamsElementTypeOpt;
    internal readonly TypeWithAnnotations ParamsElementTypeOpt;

    /// <summary>
    /// Omit ref feature for COM interop: We can pass arguments by value for ref parameters if we are invoking a method/property on an instance of a COM imported type.
    /// This property returns a flag indicating whether we had any ref omitted argument for the given call.
    /// </summary>
    internal readonly bool HasAnyRefOmittedArgument;

    private MemberAnalysisResult(
        MemberResolutionKind kind,
        BitVector badArgumentsOpt = default,
        ImmutableArray<int> argsToParamsOpt = default,
        ImmutableArray<Conversion> conversionsOpt = default,
        int missingParameter = -1,
        bool hasAnyRefOmittedArgument = false,
        ImmutableArray<TypeParameterDiagnosticInfo> constraintFailureDiagnosticsOpt = default,
        TypeWithAnnotations definitionParamsElementTypeOpt = default,
        TypeWithAnnotations paramsElementTypeOpt = default) {
        Debug.Assert(kind != MemberResolutionKind.ApplicableInExpandedForm || definitionParamsElementTypeOpt.HasType);
        Debug.Assert(kind != MemberResolutionKind.ApplicableInExpandedForm || paramsElementTypeOpt.HasType);

        this.Kind = kind;
        this.DefinitionParamsElementTypeOpt = definitionParamsElementTypeOpt;
        this.ParamsElementTypeOpt = paramsElementTypeOpt;
        this.BadArgumentsOpt = badArgumentsOpt;
        this.ArgsToParamsOpt = argsToParamsOpt;
        this.conversions = conversionsOpt;
        this.BadParameter = missingParameter;
        this.HasAnyRefOmittedArgument = hasAnyRefOmittedArgument;
        this.ConstraintFailureDiagnostics = constraintFailureDiagnosticsOpt.NullToEmpty();
    }

    internal override bool Equals(object obj) {
        throw ExceptionUtilities.Unreachable();
    }

    internal override int GetHashCode() {
        throw ExceptionUtilities.Unreachable();
    }

    internal Conversion ConversionForArg(int arg) {
        if (this.conversions.IsDefault) {
            return Conversion.Identity;
        }

        return this.conversions[arg];
    }

    internal int ParameterFromArgument(int arg) {
        Debug.Assert(arg >= 0);
        if (ArgsToParamsOpt.IsDefault) {
            return arg;
        }
        Debug.Assert(arg < ArgsToParamsOpt.Length);
        return ArgsToParamsOpt[arg];
    }

    internal int FirstBadArgument => BadArgumentsOpt.TrueBits().First();

    // A method may be applicable, but worse than another method.
    internal bool IsApplicable {
        get {
            switch (this.Kind) {
                case MemberResolutionKind.ApplicableInNormalForm:
                case MemberResolutionKind.ApplicableInExpandedForm:
                case MemberResolutionKind.Worse:
                case MemberResolutionKind.Worst:
                    return true;
                default:
                    return false;
            }
        }
    }

    internal bool IsValid {
        get {
            switch (this.Kind) {
                case MemberResolutionKind.ApplicableInNormalForm:
                case MemberResolutionKind.ApplicableInExpandedForm:
                    return true;
                default:
                    return false;
            }
        }
    }

    /// <remarks>
    /// Returns false for <see cref="MemberResolutionKind.UnsupportedMetadata"/>
    /// because those diagnostics are only reported if no other candidates are
    /// available.
    /// </remarks>
    internal bool HasUseSiteDiagnosticToReportFor(Symbol symbol) {
        // There is a use site diagnostic to report here, but it is not reported
        // just because this member was a candidate - only if it "wins".
        return !SuppressUseSiteDiagnosticsForKind(this.Kind) &&
            (object)symbol != null && symbol.GetUseSiteInfo().DiagnosticInfo != null;
    }

    private static bool SuppressUseSiteDiagnosticsForKind(MemberResolutionKind kind) {
        switch (kind) {
            case MemberResolutionKind.UnsupportedMetadata:
                return true;
            case MemberResolutionKind.NoCorrespondingParameter:
            case MemberResolutionKind.NoCorrespondingNamedParameter:
            case MemberResolutionKind.DuplicateNamedArgument:
            case MemberResolutionKind.NameUsedForPositional:
            case MemberResolutionKind.RequiredParameterMissing:
            case MemberResolutionKind.LessDerived:
                // Dev12 checks all of these things before considering use site diagnostics.
                // That is, use site diagnostics are suppressed for candidates rejected for these reasons.
                return true;
            default:
                return false;
        }
    }

    internal static MemberAnalysisResult ArgumentParameterMismatch(ArgumentAnalysisResult argAnalysis) {
        switch (argAnalysis.Kind) {
            case ArgumentAnalysisResultKind.NoCorrespondingParameter:
                return NoCorrespondingParameter(argAnalysis.ArgumentPosition);
            case ArgumentAnalysisResultKind.NoCorrespondingNamedParameter:
                return NoCorrespondingNamedParameter(argAnalysis.ArgumentPosition);
            case ArgumentAnalysisResultKind.DuplicateNamedArgument:
                return DuplicateNamedArgument(argAnalysis.ArgumentPosition);
            case ArgumentAnalysisResultKind.RequiredParameterMissing:
                return RequiredParameterMissing(argAnalysis.ParameterPosition);
            case ArgumentAnalysisResultKind.NameUsedForPositional:
                return NameUsedForPositional(argAnalysis.ArgumentPosition);
            case ArgumentAnalysisResultKind.BadNonTrailingNamedArgument:
                return BadNonTrailingNamedArgument(argAnalysis.ArgumentPosition);
            default:
                throw ExceptionUtilities.UnexpectedValue(argAnalysis.Kind);
        }
    }

    internal static MemberAnalysisResult NameUsedForPositional(int argumentPosition) {
        return new MemberAnalysisResult(
            MemberResolutionKind.NameUsedForPositional,
            badArgumentsOpt: CreateBadArgumentsWithPosition(argumentPosition));
    }

    internal static MemberAnalysisResult BadNonTrailingNamedArgument(int argumentPosition) {
        return new MemberAnalysisResult(
            MemberResolutionKind.BadNonTrailingNamedArgument,
            badArgumentsOpt: CreateBadArgumentsWithPosition(argumentPosition));
    }

    internal static MemberAnalysisResult NoCorrespondingParameter(int argumentPosition) {
        return new MemberAnalysisResult(
            MemberResolutionKind.NoCorrespondingParameter,
            badArgumentsOpt: CreateBadArgumentsWithPosition(argumentPosition));
    }

    internal static MemberAnalysisResult NoCorrespondingNamedParameter(int argumentPosition) {
        return new MemberAnalysisResult(
            MemberResolutionKind.NoCorrespondingNamedParameter,
            badArgumentsOpt: CreateBadArgumentsWithPosition(argumentPosition));
    }

    internal static MemberAnalysisResult DuplicateNamedArgument(int argumentPosition) {
        return new MemberAnalysisResult(
            MemberResolutionKind.DuplicateNamedArgument,
            badArgumentsOpt: CreateBadArgumentsWithPosition(argumentPosition));
    }

    internal static BitVector CreateBadArgumentsWithPosition(int argumentPosition) {
        var badArguments = BitVector.Create(argumentPosition + 1);
        badArguments[argumentPosition] = true;
        return badArguments;
    }

    internal static MemberAnalysisResult RequiredParameterMissing(int parameterPosition) {
        return new MemberAnalysisResult(
            MemberResolutionKind.RequiredParameterMissing,
            missingParameter: parameterPosition);
    }

    internal static MemberAnalysisResult UseSiteError() {
        return new MemberAnalysisResult(MemberResolutionKind.UseSiteError);
    }

    internal static MemberAnalysisResult UnsupportedMetadata() {
        return new MemberAnalysisResult(MemberResolutionKind.UnsupportedMetadata);
    }

    internal static MemberAnalysisResult BadArgumentConversions(ImmutableArray<int> argsToParamsOpt, BitVector badArguments, ImmutableArray<Conversion> conversions, TypeWithAnnotations definitionParamsElementTypeOpt, TypeWithAnnotations paramsElementTypeOpt) {
        Debug.Assert(conversions.Length != 0);
        Debug.Assert(badArguments.TrueBits().Any());
        return new MemberAnalysisResult(
            MemberResolutionKind.BadArgumentConversion,
            badArguments,
            argsToParamsOpt,
            conversions,
            definitionParamsElementTypeOpt: definitionParamsElementTypeOpt,
            paramsElementTypeOpt: paramsElementTypeOpt);
    }

    internal static MemberAnalysisResult InaccessibleTypeArgument() {
        return new MemberAnalysisResult(MemberResolutionKind.InaccessibleTypeArgument);
    }

    internal static MemberAnalysisResult TypeInferenceFailed() {
        return new MemberAnalysisResult(MemberResolutionKind.TypeInferenceFailed);
    }

    internal static MemberAnalysisResult TypeInferenceExtensionInstanceArgumentFailed() {
        return new MemberAnalysisResult(MemberResolutionKind.TypeInferenceExtensionInstanceArgument);
    }

    internal static MemberAnalysisResult StaticInstanceMismatch() {
        return new MemberAnalysisResult(MemberResolutionKind.StaticInstanceMismatch);
    }

    internal static MemberAnalysisResult ConstructedParameterFailedConstraintsCheck(int parameterPosition) {
        return new MemberAnalysisResult(
            MemberResolutionKind.ConstructedParameterFailedConstraintCheck,
            missingParameter: parameterPosition);
    }

    internal static MemberAnalysisResult WrongRefKind() {
        return new MemberAnalysisResult(MemberResolutionKind.WrongRefKind);
    }

    internal static MemberAnalysisResult WrongReturnType() {
        return new MemberAnalysisResult(MemberResolutionKind.WrongReturnType);
    }

    internal static MemberAnalysisResult LessDerived() {
        return new MemberAnalysisResult(MemberResolutionKind.LessDerived);
    }

    internal static MemberAnalysisResult NormalForm(ImmutableArray<int> argsToParamsOpt, ImmutableArray<Conversion> conversions, bool hasAnyRefOmittedArgument) {
        return new MemberAnalysisResult(MemberResolutionKind.ApplicableInNormalForm, BitVector.Null, argsToParamsOpt, conversions, hasAnyRefOmittedArgument: hasAnyRefOmittedArgument);
    }

    internal static MemberAnalysisResult ExpandedForm(ImmutableArray<int> argsToParamsOpt, ImmutableArray<Conversion> conversions, bool hasAnyRefOmittedArgument, TypeWithAnnotations definitionParamsElementType, TypeWithAnnotations paramsElementType) {
        return new MemberAnalysisResult(
            MemberResolutionKind.ApplicableInExpandedForm, BitVector.Null, argsToParamsOpt, conversions,
            hasAnyRefOmittedArgument: hasAnyRefOmittedArgument, definitionParamsElementTypeOpt: definitionParamsElementType, paramsElementTypeOpt: paramsElementType);
    }

    internal static MemberAnalysisResult Worse() {
        return new MemberAnalysisResult(MemberResolutionKind.Worse);
    }

    internal static MemberAnalysisResult Worst() {
        return new MemberAnalysisResult(MemberResolutionKind.Worst);
    }

    internal static MemberAnalysisResult ConstraintFailure(ImmutableArray<TypeParameterDiagnosticInfo> constraintFailureDiagnostics) {
        return new MemberAnalysisResult(MemberResolutionKind.ConstraintFailure, constraintFailureDiagnosticsOpt: constraintFailureDiagnostics);
    }

    internal static MemberAnalysisResult WrongCallingConvention() {
        return new MemberAnalysisResult(MemberResolutionKind.WrongCallingConvention);
    }
}
