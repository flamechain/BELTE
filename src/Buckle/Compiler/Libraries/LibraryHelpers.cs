using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Buckle.CodeAnalysis;
using Buckle.CodeAnalysis.Symbols;
using Buckle.Utilities;
using Microsoft.CodeAnalysis.PooledObjects;

namespace Buckle.Libraries;

internal static class LibraryHelpers {
    internal static SynthesizedFieldSymbol ConstExprField(string name, SpecialType specialType, object constantValue) {
        var type = CorLibrary.GetSpecialType(specialType);
        return new SynthesizedFieldSymbol(null, type, name, true, false, true, false, true, constantValue);
    }

    internal static SynthesizedFinishedNamedTypeSymbol StaticClass(string name, ImmutableArray<Symbol> members) {
        return Class(name, members, DeclarationModifiers.Static);
    }

    internal static SynthesizedFinishedNamedTypeSymbol Class(
        string name,
        ImmutableArray<Symbol> members,
        DeclarationModifiers modifiers) {
        var namedType = new SynthesizedSimpleNamedTypeSymbol(
            name,
            TypeKind.Class,
            CorLibrary.GetSpecialType(SpecialType.Object),
            DeclarationModifiers.Public | modifiers
        );

        var builder = ArrayBuilder<Symbol>.GetInstance();

        foreach (var member in members) {
            switch (member) {
                case MethodSymbol method:
                    builder.Add(new SynthesizedFinishedMethodSymbol(method, namedType, null));
                    break;
                case NamedTypeSymbol type:
                    builder.Add(new SynthesizedFinishedNamedTypeSymbol(type, namedType, null));
                    break;
                case FieldSymbol field:
                    builder.Add(new SynthesizedFieldSymbol(
                        namedType,
                        field.type,
                        field.name,
                        field.declaredAccessibility == Accessibility.Public,
                        field.isConst,
                        field.isConstExpr,
                        field.isStatic,
                        field.hasConstantValue,
                        field.constantValue
                    ));
                    break;
                default:
                    throw ExceptionUtilities.UnexpectedValue(member.kind);
            }
        }

        return new SynthesizedFinishedNamedTypeSymbol(namedType, null, builder.ToImmutableAndFree());
    }

    internal static SynthesizedFinishedMethodSymbol StaticMethod(string name, SpecialType type) {
        return Method(name, type, false, [], DeclarationModifiers.Static);
    }

    internal static SynthesizedFinishedMethodSymbol StaticMethod(
        string name,
        SpecialType type,
        IEnumerable<(string name, SpecialType type)> parameters) {
        return Method(
            name,
            type,
            false,
            parameters.Select<(string name, SpecialType type),
                              (string, SpecialType, bool, object, RefKind)>(
                p => (p.name, p.type, false, null, RefKind.None)
            ),
            DeclarationModifiers.Static
        );
    }

    internal static SynthesizedFinishedMethodSymbol StaticMethod(
        string name,
        SpecialType type,
        IEnumerable<(string name, SpecialType type, bool isNullable)> parameters) {
        return Method(
            name,
            type,
            false,
            parameters.Select<(string name, SpecialType type, bool isNullable),
                              (string, SpecialType, bool, object, RefKind)>(
                p => (p.name, p.type, p.isNullable, null, RefKind.None)
            ),
            DeclarationModifiers.Static
        );
    }

    internal static SynthesizedFinishedMethodSymbol Method(
        string name,
        SpecialType type,
        bool isNullable,
        IEnumerable<(string name, SpecialType type, bool isNullable, object defaultValue)> parameters) {
        return Method(
            name,
            type,
            isNullable,
            parameters.Select<(string name, SpecialType type, bool isNullable, object defaultValue),
                              (string, SpecialType, bool, object, RefKind)>(
                p => (p.name, p.type, p.isNullable, p.defaultValue, RefKind.None)
            ),
            DeclarationModifiers.None
        );
    }

    internal static SynthesizedFinishedMethodSymbol Method(
        string name,
        SpecialType type,
        IEnumerable<(string name, SpecialType type, object defaultValue)> parameters) {
        return Method(
            name,
            type,
            false,
            parameters.Select<(string name, SpecialType type, object defaultValue),
                              (string, SpecialType, bool, object, RefKind)>(
                p => (p.name, p.type, false, p.defaultValue, RefKind.None)
            ),
            DeclarationModifiers.None
        );
    }

    internal static SynthesizedFinishedMethodSymbol Method(
        string name,
        SpecialType type,
        IEnumerable<(string name, SpecialType type, bool isNullable)> parameters) {
        return Method(
            name,
            type,
            false,
            parameters.Select<(string name, SpecialType type, bool isNullable),
                              (string, SpecialType, bool, object, RefKind)>(
                p => (p.name, p.type, p.isNullable, null, RefKind.None)
            ),
            DeclarationModifiers.None
        );
    }

    internal static SynthesizedFinishedMethodSymbol Method(
        string name,
        SpecialType type,
        bool isNullable,
        IEnumerable<(string name, SpecialType type, bool isNullable)> parameters) {
        return Method(
            name,
            type,
            isNullable,
            parameters.Select<(string name, SpecialType type, bool isNullable),
                              (string, SpecialType, bool, object, RefKind)>(
                p => (p.name, p.type, p.isNullable, null, RefKind.None)
            ),
            DeclarationModifiers.None
        );
    }

    internal static SynthesizedFinishedMethodSymbol Method(
        string name,
        SpecialType type,
        IEnumerable<(string name, SpecialType type)> parameters) {
        return Method(
            name,
            type,
            false,
            parameters.Select<(string name, SpecialType type),
                              (string, SpecialType, bool, object, RefKind)>(
                p => (p.name, p.type, false, null, RefKind.None)
            ),
            DeclarationModifiers.None
        );
    }

    internal static SynthesizedFinishedMethodSymbol Method(
        string name,
        SpecialType type,
        bool isNullable,
        IEnumerable<(string name, SpecialType type, bool isNullable, object defaultValue, RefKind refKind)> parameters,
        DeclarationModifiers modifiers) {
        var returnType = CorLibrary.GetSpecialType(type);
        var returnTypeWithAnnotations = new TypeWithAnnotations(returnType, isNullable);
        var method = new SynthesizedSimpleOrdinaryMethodSymbol(
            name,
            returnTypeWithAnnotations,
            RefKind.None,
            DeclarationModifiers.Public | modifiers
        );

        var builder = ArrayBuilder<ParameterSymbol>.GetInstance();
        var i = 0;

        foreach (var parameter in parameters) {
            var parameterType = CorLibrary.GetSpecialType(parameter.type);
            var parameterTypeWithAnnotations = new TypeWithAnnotations(parameterType, parameter.isNullable);
            var constantValue = parameter.defaultValue is null ? null : new ConstantValue(parameter.defaultValue);

            var synthesizedParameter = SynthesizedParameterSymbol.Create(
                method,
                parameterTypeWithAnnotations,
                i,
                parameter.refKind,
                parameter.name,
                defaultValue: constantValue
            );

            i++;
        }

        return new SynthesizedFinishedMethodSymbol(method, null, builder.ToImmutableAndFree());
    }
}
