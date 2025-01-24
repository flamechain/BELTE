using System;
using System.Collections.Immutable;
using System.Runtime.CompilerServices;
using Buckle.Libraries;
using Buckle.Utilities;
using Microsoft.CodeAnalysis.PooledObjects;

namespace Buckle.CodeAnalysis.Symbols;

/// <summary>
/// A type symbol. This is just the base type name, not a full <see cref="Binding.BoundType" />.
/// </summary>
internal abstract class TypeSymbol : NamespaceOrTypeSymbol, ITypeSymbol {
    internal const string ImplicitTypeName = "<invalid-global-code>";

    private static readonly Func<TypeSymbol, TemplateParameterSymbol, bool, bool> ContainsTemplateParameterPredicate =
        (type, parameter, unused) => type.typeKind == TypeKind.TemplateParameter &&
        (parameter is null || Equals(type, parameter, TypeCompareKind.ConsiderEverything));

    public override SymbolKind kind => SymbolKind.NamedType;

    public abstract TypeKind typeKind { get; }

    public abstract bool isPrimitiveType { get; }

    public abstract bool isObjectType { get; }

    public virtual SpecialType specialType => SpecialType.None;

    internal new TypeSymbol originalDefinition => _originalTypeSymbolDefinition;

    private protected virtual TypeSymbol _originalTypeSymbolDefinition => this;

    private protected sealed override Symbol _originalSymbolDefinition => _originalTypeSymbolDefinition;

    internal abstract NamedTypeSymbol baseType { get; }

    internal TypeSymbol EffectiveType() {
        return typeKind == TypeKind.TemplateParameter ? ((TemplateParameterSymbol)this).effectiveBaseClass : this;
    }

    internal bool IsDerivedFrom(TypeSymbol type, TypeCompareKind compareKind) {
        if ((object)this == type)
            return false;

        var current = baseType;

        while ((object)current is not null) {
            if (type.Equals(current, compareKind))
                return true;

            current = current.baseType;
        }

        return false;
    }

    internal bool IsEqualToOrDerivedFrom(TypeSymbol type, TypeCompareKind compareKind) {
        return Equals(type, compareKind) || IsDerivedFrom(type, compareKind);
    }

    internal int TypeToIndex() {
        switch (specialType) {
            case SpecialType.Any: return 0;
            case SpecialType.String: return 1;
            case SpecialType.Bool: return 2;
            case SpecialType.Char: return 3;
            case SpecialType.Int: return 4;
            case SpecialType.Decimal: return 5;
            case SpecialType.Type: return 6;
            case SpecialType.Object: return 7;
            case SpecialType.Nullable:
                var underlyingType = GetNullableUnderlyingType();

                switch (underlyingType.specialType) {
                    case SpecialType.Any: return 8;
                    case SpecialType.String: return 9;
                    case SpecialType.Bool: return 10;
                    case SpecialType.Char: return 11;
                    case SpecialType.Int: return 12;
                    case SpecialType.Decimal: return 13;
                    case SpecialType.Type: return 14;
                    case SpecialType.Object: return 15;
                }

                goto default;
            default: return -1;
        }
    }

    internal bool IsErrorType() {
        return kind == SymbolKind.ErrorType;
    }

    internal bool IsClassType() {
        return typeKind == TypeKind.Class;
    }

    internal bool IsStructType() {
        return typeKind == TypeKind.Struct;
    }

    internal bool IsTemplateParameter() {
        return typeKind == TypeKind.TemplateParameter;
    }

    internal bool IsPrimitiveType() {
        return specialType.IsPrimitiveType();
    }

    internal bool IsVoidType() {
        // TODO Use originalDefinition here?
        return specialType == SpecialType.Void;
    }

    internal bool ContainsErrorType() {
        var result = VisitType(
            (type, unused1, unused2) => type.IsErrorType(),
            (object?)null,
            canDigThroughNullable: true
        );

        return result is not null;
    }

    internal bool ContainsTemplateParameter(TemplateParameterSymbol parameter = null) {
        var result = VisitType(ContainsTemplateParameterPredicate, parameter);
        return result is not null;
    }

    internal TypeSymbol VisitType<T>(
        Func<TypeSymbol, T, bool, bool> predicate,
        T arg,
        bool canDigThroughNullable = false) {
        return TypeWithAnnotationsExtensions.VisitType(null, this, null, predicate, arg, canDigThroughNullable);
    }

    internal bool IsAtLeastAsVisibleAs(Symbol symbol) {
        return typeKind switch {
            TypeKind.Class or TypeKind.Struct => symbol.declaredAccessibility switch {
                Accessibility.Public => declaredAccessibility is Accessibility.Public,
                Accessibility.Protected => declaredAccessibility is Accessibility.Public or Accessibility.Protected,
                _ => true,
            },
            _ => true,
        };
    }

    internal TypeSymbol GetNextBaseType(
        ConsList<TypeSymbol> basesBeingResolved,
        ref PooledHashSet<NamedTypeSymbol> visited) {
        switch (typeKind) {
            case TypeKind.TemplateParameter:
                return ((TemplateParameterSymbol)this).effectiveBaseClass;
            case TypeKind.Class:
            case TypeKind.Struct:
            case TypeKind.Error:
                return GetNextDeclaredBase((NamedTypeSymbol)this, basesBeingResolved, ref visited);
            case TypeKind.Array:
                return baseType;
            default:
                throw ExceptionUtilities.UnexpectedValue(typeKind);
        }
    }

    private static TypeSymbol GetNextDeclaredBase(
        NamedTypeSymbol type,
        ConsList<TypeSymbol> basesBeingResolved,
        ref PooledHashSet<NamedTypeSymbol> visited) {
        if (basesBeingResolved is not null && basesBeingResolved.ContainsReference(type.originalDefinition))
            return null;

        if (type.specialType == SpecialType.Object) {
            type.SetKnownToHaveNoDeclaredBaseCycles();
            return null;
        }

        var nextType = type.GetDeclaredBaseType(basesBeingResolved);

        if (nextType is null) {
            SetKnownToHaveNoDeclaredBaseCycles(ref visited);
            return GetDefaultBaseOrNull(type);
        }

        var origType = type.originalDefinition;
        if (nextType.knownToHaveNoDeclaredBaseCycles) {
            origType.SetKnownToHaveNoDeclaredBaseCycles();
            SetKnownToHaveNoDeclaredBaseCycles(ref visited);
        } else {
            visited ??= PooledHashSet<NamedTypeSymbol>.GetInstance();
            visited.Add(origType);

            if (visited.Contains(nextType.originalDefinition))
                return GetDefaultBaseOrNull(type);
        }

        return nextType;
    }

    private static void SetKnownToHaveNoDeclaredBaseCycles(ref PooledHashSet<NamedTypeSymbol> visited) {
        if (visited is not null) {
            foreach (var v in visited)
                v.SetKnownToHaveNoDeclaredBaseCycles();

            visited.Free();
            visited = null;
        }
    }

    private static NamedTypeSymbol GetDefaultBaseOrNull(NamedTypeSymbol type) {
        switch (type.typeKind) {
            case TypeKind.Class:
            case TypeKind.Error:
                return CorLibrary.GetSpecialType(SpecialType.Object);
            case TypeKind.Struct:
                return null;
            default:
                throw ExceptionUtilities.UnexpectedValue(type.typeKind);
        }
    }

    internal bool IsPossiblyNullableTypeTemplateParameter() {
        return this is TemplateParameterSymbol t && t.underlyingType.isNullable;
    }

    internal TypeSymbol GetNullableUnderlyingType() {
        return GetNullableUnderlyingTypeWithAnnotations().type;
    }

    internal TypeWithAnnotations GetNullableUnderlyingTypeWithAnnotations() {
        return ((NamedTypeSymbol)this).templateArguments[0].type;
    }

    internal TypeSymbol StrippedType() {
        return this.IsNullableType() ? GetNullableUnderlyingType() : this;
    }

    internal bool InheritsFromIgnoringConstruction(NamedTypeSymbol baseType) {
        var current = this;

        while (current is not null) {
            if ((object)current == baseType)
                return true;

            current = current.baseType?.originalDefinition;
        }

        return false;
    }

    internal virtual bool Equals(TypeSymbol other, TypeCompareKind compareKind) {
        return ReferenceEquals(this, other);
    }

    internal sealed override bool Equals(Symbol other, TypeCompareKind compareKind) {
        if (other is not TypeSymbol otherAsType)
            return false;

        return Equals(otherAsType, compareKind);
    }

    internal static bool Equals(TypeSymbol left, TypeSymbol right, TypeCompareKind compareKind) {
        if (left is null)
            return right is null;

        return left.Equals(right, compareKind);
    }

    public override int GetHashCode() {
        return RuntimeHelpers.GetHashCode(this);
    }

    [Obsolete("Use 'TypeSymbol.Equals(TypeSymbol, TypeSymbol, TypeCompareKind)' method.", true)]
    public static bool operator ==(TypeSymbol left, TypeSymbol right)
        => throw ExceptionUtilities.Unreachable();

    [Obsolete("Use 'TypeSymbol.Equals(TypeSymbol, TypeSymbol, TypeCompareKind)' method.", true)]
    public static bool operator !=(TypeSymbol left, TypeSymbol right)
        => throw ExceptionUtilities.Unreachable();

    [Obsolete("Use 'TypeSymbol.Equals(TypeSymbol, TypeSymbol, TypeCompareKind)' method.", true)]
    public static bool operator ==(Symbol left, TypeSymbol right)
        => throw ExceptionUtilities.Unreachable();

    [Obsolete("Use 'TypeSymbol.Equals(TypeSymbol, TypeSymbol, TypeCompareKind)' method.", true)]
    public static bool operator !=(Symbol left, TypeSymbol right)
        => throw ExceptionUtilities.Unreachable();

    [Obsolete("Use 'TypeSymbol.Equals(TypeSymbol, TypeSymbol, TypeCompareKind)' method.", true)]
    public static bool operator ==(TypeSymbol left, Symbol right)
        => throw ExceptionUtilities.Unreachable();

    [Obsolete("Use 'TypeSymbol.Equals(TypeSymbol, TypeSymbol, TypeCompareKind)' method.", true)]
    public static bool operator !=(TypeSymbol left, Symbol right)
        => throw ExceptionUtilities.Unreachable();

    public ImmutableArray<ISymbol> GetMembersPublic() {
        return [.. GetMembers()];
    }

    INamedTypeSymbol ITypeSymbol.baseType => baseType;
}
