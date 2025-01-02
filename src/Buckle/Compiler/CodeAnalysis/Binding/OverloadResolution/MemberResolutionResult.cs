using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

internal readonly struct MemberResolutionResult<T> where T : Symbol {
    private readonly T _member;
    private readonly T _leastOverriddenMember;
    private readonly MemberAnalysisResult _result;

    /// <summary>
    /// At least one type argument was inferred from a function type.
    /// </summary>
    internal readonly bool HasTypeArgumentInferredFromFunctionType;

    internal MemberResolutionResult(T member, T leastOverriddenMember, MemberAnalysisResult result, bool hasTypeArgumentInferredFromFunctionType) {
        _member = member;
        _leastOverriddenMember = leastOverriddenMember;
        _result = result;
        HasTypeArgumentInferredFromFunctionType = hasTypeArgumentInferredFromFunctionType;
    }

    internal MemberResolutionResult<T> WithResult(MemberAnalysisResult result) {
        return new MemberResolutionResult<T>(Member, LeastOverriddenMember, result, HasTypeArgumentInferredFromFunctionType);
    }

    internal bool IsNull {
        get { return (object)_member == null; }
    }

    internal bool IsNotNull {
        get { return (object)_member != null; }
    }

    /// <summary>
    /// The member considered during overload resolution.
    /// </summary>
    public T Member {
        get { return _member; }
    }

    /// <summary>
    /// The least overridden member that is accessible from the call site that performed overload resolution.
    /// Typically a virtual or abstract method (but not necessarily).
    /// </summary>
    /// <remarks>
    /// The member whose parameter types and params modifiers were considered during overload resolution.
    /// </remarks>
    internal T LeastOverriddenMember {
        get { return _leastOverriddenMember; }
    }

    /// <summary>
    /// Indicates why the compiler accepted or rejected the member during overload resolution.
    /// </summary>
    public MemberResolutionKind Resolution {
        get {
            return Result.Kind;
        }
    }

    /// <summary>
    /// Returns true if the compiler accepted this member as the sole correct result of overload resolution.
    /// </summary>
    public bool IsValid {
        get {
            return Result.IsValid;
        }
    }

    public bool IsApplicable {
        get {
            return Result.IsApplicable;
        }
    }

    internal MemberResolutionResult<T> Worse() {
        return WithResult(MemberAnalysisResult.Worse());
    }

    internal MemberResolutionResult<T> Worst() {
        return WithResult(MemberAnalysisResult.Worst());
    }

    internal bool HasUseSiteDiagnosticToReport {
        get {
            return _result.HasUseSiteDiagnosticToReportFor(_member);
        }
    }

    /// <summary>
    /// The result of member analysis.
    /// </summary>
    internal MemberAnalysisResult Result {
        get { return _result; }
    }

    T IMemberResolutionResultWithPriority<T>.MemberWithPriority => LeastOverriddenMember;

    public override bool Equals(object? obj) {
        throw new NotSupportedException();
    }

    public override int GetHashCode() {
        throw new NotSupportedException();
    }
}
