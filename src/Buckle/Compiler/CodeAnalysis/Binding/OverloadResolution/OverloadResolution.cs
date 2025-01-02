using System.Collections.Generic;
using Buckle.CodeAnalysis.Symbols;
using Microsoft.CodeAnalysis.PooledObjects;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Resolves overloads to find the best overload.
/// </summary>
internal sealed partial class OverloadResolution {
    private readonly Binder _binder;

    /// <summary>
    /// Creates an <see cref="OverloadResolution" />, uses a Binders diagnostics.
    /// </summary>
    /// <param name="binder"><see cref="Binder" /> to use diagnostics from.</param>
    internal OverloadResolution(Binder binder) {
        _binder = binder;
    }

    internal void BinaryOperatorOverloadResolution(
        BinaryOperatorKind kind,
        BoundExpression left,
        BoundExpression right,
        BinaryOperatorOverloadResolutionResult result) {
        EasyOut(kind, left, right, result);

        if (result.results.Count > 0)
            return;

        return;

        void EasyOut(
            BinaryOperatorKind kind,
            BoundExpression left,
            BoundExpression right,
            BinaryOperatorOverloadResolutionResult result) {
            var underlyingKind = kind & ~BinaryOperatorKind.Conditional;
            BinaryOperatorEasyOut(underlyingKind, left, right, result);
        }
    }

    internal void MethodOverloadResolution<T>(
        ArrayBuilder<T> members,
        ArrayBuilder<TypeOrConstant> templateArguments,
        BoundExpression receiver,
        AnalyzedArguments arguments,
        OverloadResolutionResult<T> result,
        RefKind returnRefKind = default,
        TypeSymbol returnType = null)
        where T : Symbol {
        var results = result.resultsBuilder;

        var checkOverriddenOrHidden = !members.All(
            static m => m.containingSymbol is NamedTypeSymbol { baseType.specialType: SpecialType.Object }
        );

        PerformMemberOverloadResolution(
            results,
            members,
            templateArguments,
            receiver,
            arguments,
            completeResults: false,
            returnRefKind,
            returnType,
            checkOverriddenOrHidden: checkOverriddenOrHidden
        );

        if (!SingleValidResult(results)) {
            result.Clear();

            PerformMemberOverloadResolution(
                results,
                members,
                templateArguments,
                receiver,
                arguments,
                completeResults: true,
                returnRefKind,
                returnType,
                checkOverriddenOrHidden: checkOverriddenOrHidden
            );
        }
    }

    private void PerformMemberOverloadResolution<T>(
        ArrayBuilder<MemberResolutionResult<T>> results,
        ArrayBuilder<T> members,
        ArrayBuilder<TypeOrConstant> templateArguments,
        BoundExpression receiver,
        AnalyzedArguments arguments,
        bool completeResults,
        RefKind returnRefKind,
        TypeSymbol returnType,
        bool checkOverriddenOrHidden)
        where T : Symbol {
        // TODO The following is bare bones to compile, just takes the first available method
        Dictionary<NamedTypeSymbol, ArrayBuilder<T>> containingTypeMap = null;

        if (checkOverriddenOrHidden && members.Count > 50)
            containingTypeMap = PartitionMembersByContainingType(members);

        for (var i = 0; i < members.Count; i++) {
            AddMemberToCandidateSet(
                members[i],
                results,
                members,
                templateArguments,
                arguments,
                completeResults,
                containingTypeMap,
                checkOverriddenOrHidden: checkOverriddenOrHidden
            );
        }
    }

    private void AddMemberToCandidateSet<T>(
        T member,
        ArrayBuilder<MemberResolutionResult<T>> results,
        ArrayBuilder<T> members,
        ArrayBuilder<TypeOrConstant> templateArguments,
        AnalyzedArguments arguments,
        bool completeResults,
        Dictionary<NamedTypeSymbol, ArrayBuilder<T>> containingTypeMap,
        bool checkOverriddenOrHidden = true)
        where T : Symbol {
        // TODO This method is bare bones to compile (no error checking)
        var leastOverriddenMember = (T)member.GetLeastOverriddenMember(_binder.containingType);
        results.Add(new MemberResolutionResult<T>(member, leastOverriddenMember, MemberAnalysisResult.Applicable([], [], false), false));
    }

    private static Dictionary<NamedTypeSymbol, ArrayBuilder<T>> PartitionMembersByContainingType<T>(
        ArrayBuilder<T> members) where T : Symbol {
        var containingTypeMap = new Dictionary<NamedTypeSymbol, ArrayBuilder<T>>();

        for (var i = 0; i < members.Count; i++) {
            var member = members[i];
            var containingType = member.containingType;

            if (!containingTypeMap.TryGetValue(containingType, out var builder)) {
                builder = ArrayBuilder<T>.GetInstance();
                containingTypeMap[containingType] = builder;
            }

            builder.Add(member);
        }

        return containingTypeMap;
    }

    private static bool SingleValidResult<TMember>(ArrayBuilder<MemberResolutionResult<TMember>> results)
        where TMember : Symbol {
        var oneValid = false;
        foreach (var result in results) {
            if (result.isValid) {
                if (oneValid)
                    return false;

                oneValid = true;
            }
        }

        return oneValid;
    }
}
