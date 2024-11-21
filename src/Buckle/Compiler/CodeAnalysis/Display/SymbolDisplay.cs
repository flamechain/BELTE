using System.Collections.Immutable;
using Buckle.CodeAnalysis.Binding;
using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;
using Buckle.Utilities;
using static Buckle.CodeAnalysis.Display.DisplayTextSegment;

namespace Buckle.CodeAnalysis.Display;

/// <summary>
/// Prints a <see cref="Symbol" />.
/// </summary>
public static class SymbolDisplay {
    /// <summary>
    /// Generates a rich text representation of a single <see cref="Symbol" />.
    /// </summary>
    /// <param name="symbol"><see cref="Symbol" /> to convert to rich text.</param>
    /// <returns>New <see cref="DisplayText" /> representing the <see cref="Symbol" />.</returns>
    internal static DisplayText ToDisplayText(Symbol symbol, SymbolDisplayFormat format = null) {
        var text = new DisplayText();
        AppendToDisplayText(text, symbol, format);
        return text;
    }

    internal static ImmutableArray<DisplayTextSegment> ToDisplaySegments(
        Symbol symbol,
        SymbolDisplayFormat format = null) {
        return ToDisplayText(symbol, format).segments.ToImmutableArray();
    }

    internal static string ToDisplayString(Symbol symbol, SymbolDisplayFormat format = null) {
        return ToDisplayText(symbol, format).ToString();
    }

    /// <summary>
    /// Adds a single <see cref="Symbol" /> to an existing <see cref="DisplayText" />.
    /// </summary>
    /// <param name="text"><see cref="DisplayText" /> to add to.</param>
    /// <param name="symbol"><see cref="Symbol" /> to add (not modified).</param>
    public static void AppendToDisplayText(DisplayText text, ISymbol symbol, SymbolDisplayFormat format = null) {
        format ??= SymbolDisplayFormat.ErrorMessageFormat;

        switch (symbol.kind) {
            case SymbolKind.Method:
                DisplayMethod(text, (MethodSymbol)symbol, format);
                break;
            case SymbolKind.Local:
            case SymbolKind.Global:
                DisplayDataContainer(text, (DataContainerSymbol)symbol, format);
                break;
            case SymbolKind.Parameter:
                DisplayParameter(text, (ParameterSymbol)symbol, format);
                break;
            case SymbolKind.Field:
                DisplayField(text, (FieldSymbol)symbol, format);
                break;
            case SymbolKind.NamedType:
                DisplayNamedType(text, (NamedTypeSymbol)symbol, format);
                break;
            case SymbolKind.TemplateParameter:
                DisplayTemplateParameter(text, (TemplateParameterSymbol)symbol, format);
                break;
            case SymbolKind.ArrayType:
            case SymbolKind.ErrorType:
                DisplayType(text, (TypeSymbol)symbol, format);
                break;
            default:
                throw ExceptionUtilities.UnexpectedValue(symbol.kind);
        }
    }

    public static void DisplayType(DisplayText text, ITypeSymbol type, SymbolDisplayFormat format = null) {
        format ??= SymbolDisplayFormat.ErrorMessageFormat;

        if (type is ArrayTypeSymbol array) {
            DisplayType(text, array.elementType, format);

            for (var i = 0; i < array.rank; i++) {
                text.Write(CreatePunctuation(SyntaxKind.OpenBracketToken));
                text.Write(CreateLiteral(array.sizes[i].ToString()));
                text.Write(CreatePunctuation(SyntaxKind.CloseBracketToken));
            }
        } else if (type is NamedTypeSymbol namedType) {
            text.Write(CreateType(namedType.name));

            if (namedType.arity > 0) {
                text.Write(CreatePunctuation(SyntaxKind.LessThanToken));

                var isFirst = true;

                foreach (var argument in namedType.templateArguments) {
                    if (isFirst) {
                        isFirst = false;
                    } else {
                        text.Write(CreatePunctuation(SyntaxKind.CommaToken));
                        text.Write(CreateSpace());
                    }

                    if (argument.isConstant)
                        DisplayText.DisplayConstant(text, argument.constant);
                    else
                        DisplayTypeWithAnnotations(text, argument.type, format);
                }

                text.Write(CreatePunctuation(SyntaxKind.GreaterThanToken));
            }
        }
    }

    internal static void DisplayTypeWithAnnotations(
        DisplayText text,
        TypeWithAnnotations type,
        SymbolDisplayFormat format) {
        DisplayType(text, type.type, format);

        if (type.isNullable)
            text.Write(CreatePunctuation(SyntaxKind.QuestionToken));
    }

    private static void DisplayContainedNames(DisplayText text, ISymbol symbol, SymbolDisplayFormat format) {
        if ((format.qualificationStyle & SymbolDisplayQualificationStyle.IncludeGlobalNamespace) != 0) {
            text.Write(CreateType("global"));
            text.Write(CreatePunctuation("::"));
        }

        var currentSymbol = symbol as Symbol;
        var includeTypes = (format.qualificationStyle & SymbolDisplayQualificationStyle.IncludeContainingTypes) != 0;
        var includeNamespaces =
            (format.qualificationStyle & SymbolDisplayQualificationStyle.IncludeContainingNamespaces) != 0;

        var nameText = new DisplayText();

        while (currentSymbol is not null) {
            if ((includeTypes && currentSymbol is TypeSymbol) ||
                (includeNamespaces && currentSymbol is NamespaceSymbol)) {
                if (currentSymbol is NamespaceSymbol ns && ns.isGlobalNamespace)
                    break;

                nameText.Write(CreatePunctuation(SyntaxKind.PeriodToken));
                nameText.Write(CreateType(currentSymbol.containingType.name));
                currentSymbol = currentSymbol.containingSymbol;
            } else {
                break;
            }
        }

        nameText.segments.Reverse();
        text.Write(nameText.segments);
    }

    private static void DisplayField(DisplayText text, FieldSymbol field, SymbolDisplayFormat format) {
        if ((format.memberOptions & SymbolDisplayMemberOptions.IncludeAccessibility) != 0)
            DisplayAccessibility(text, field);

        if ((format.memberOptions & SymbolDisplayMemberOptions.IncludeModifiers) != 0) {
            DisplayModifiers(text, field);
            DisplayConstExprRef(text, field.isConst, field.isConstExpr, field.refKind);
            text.Write(CreateSpace());
        }

        if ((format.memberOptions & SymbolDisplayMemberOptions.IncludeType) != 0) {
            DisplayTypeWithAnnotations(text, field.typeWithAnnotations, format);
            text.Write(CreateSpace());
        }

        if ((format.memberOptions & SymbolDisplayMemberOptions.IncludeContainingType) != 0)
            DisplayContainedNames(text, field, format);

        text.Write(CreateIdentifier(field.name));
    }

    private static void DisplayParameter(DisplayText text, ParameterSymbol parameter, SymbolDisplayFormat format) {
        var needSpace = false;

        if ((format.parameterOptions & SymbolDisplayParameterOptions.IncludeModifiers) != 0) {
            DisplayConstExprRef(text, false, false, parameter.refKind);
            needSpace = true;
        }

        if ((format.parameterOptions & SymbolDisplayParameterOptions.IncludeType) != 0) {
            if (needSpace)
                text.Write(CreateSpace());

            DisplayTypeWithAnnotations(text, parameter.typeWithAnnotations, format);
            needSpace = true;
        }

        if ((format.parameterOptions & SymbolDisplayParameterOptions.IncludeName) != 0) {
            if (needSpace)
                text.Write(CreateSpace());

            text.Write(CreateIdentifier(parameter.name));
            needSpace = true;
        }

        if ((format.parameterOptions & SymbolDisplayParameterOptions.IncludeDefaultValue) != 0 &&
            parameter.hasExplicitDefaultValue) {
            if (needSpace) {
                text.Write(CreateSpace());
                text.Write(CreatePunctuation(SyntaxKind.EqualsToken));
                text.Write(CreateSpace());
            }

            DisplayText.DisplayConstant(text, parameter.explicitDefaultConstantValue);
        }
    }

    private static void DisplayTemplateParameter(
        DisplayText text,
        TemplateParameterSymbol templateParameter,
        SymbolDisplayFormat format) {
        if ((format.parameterOptions & SymbolDisplayParameterOptions.IncludeType) != 0) {
            DisplayTypeWithAnnotations(text, templateParameter.underlyingType, format);
            text.Write(CreateSpace());
        }

        if ((format.parameterOptions & SymbolDisplayParameterOptions.IncludeName) != 0)
            text.Write(CreateIdentifier(templateParameter.name));

        if ((format.templateOptions & SymbolDisplayTemplateOptions.IncludeTemplateConstraints) != 0) {
            var constraints = templateParameter.constraintTypes;

            if (constraints.Length > 0) {
                text.Write(CreateSpace());
                text.Write(CreateKeyword(SyntaxKind.ExtendsKeyword));
                text.Write(CreateSpace());

                var isFirst = true;

                foreach (var constraint in constraints) {
                    if (isFirst) {
                        isFirst = false;
                    } else {
                        text.Write(CreatePunctuation(SyntaxKind.CommaToken));
                        text.Write(CreateSpace());
                    }

                    DisplayTypeWithAnnotations(text, constraint, format);
                }
            }
        }

        if ((format.parameterOptions & SymbolDisplayParameterOptions.IncludeDefaultValue) != 0) {
            text.Write(CreateSpace());
            text.Write(CreatePunctuation(SyntaxKind.EqualsToken));
            text.Write(CreateSpace());

            var defaultValue = templateParameter.defaultValue;

            if (defaultValue.isConstant)
                DisplayText.DisplayConstant(text, defaultValue.constant);
            else
                DisplayTypeWithAnnotations(text, defaultValue.type, format);
        }
    }

    private static void DisplayDataContainer(
        DisplayText text,
        DataContainerSymbol dataContainer,
        SymbolDisplayFormat format) {
        if ((format.memberOptions & SymbolDisplayMemberOptions.IncludeModifiers) != 0) {
            DisplayConstExprRef(text, dataContainer.isConst, dataContainer.isConstExpr, dataContainer.refKind);
            text.Write(CreateSpace());
        }

        if ((format.memberOptions & SymbolDisplayMemberOptions.IncludeType) != 0) {
            DisplayTypeWithAnnotations(text, dataContainer.typeWithAnnotations, format);
            text.Write(CreateSpace());
        }

        text.Write(CreateIdentifier(dataContainer.name));
    }

    private static void DisplayTemplateParameters(
        DisplayText text,
        ImmutableArray<TemplateParameterSymbol> templateParameters,
        SymbolDisplayFormat format) {
        if ((format.templateOptions & SymbolDisplayTemplateOptions.IncludeTemplateParameters) != 0 &&
            templateParameters.Length > 0) {
            text.Write(CreatePunctuation(SyntaxKind.LessThanToken));
            var first = true;

            foreach (var templateParameter in templateParameters) {
                if (first)
                    first = false;
                else
                    text.Write(CreatePunctuation(", "));

                DisplayTemplateParameter(text, templateParameter, format);
            }

            text.Write(CreatePunctuation(SyntaxKind.GreaterThanToken));
        }
    }

    private static void DisplayTemplateConstraints(
        DisplayText text,
        ImmutableArray<BoundExpression> templateConstraints,
        SymbolDisplayFormat format) {
        if ((format.templateOptions & SymbolDisplayTemplateOptions.IncludeTemplateConstraints) != 0 &&
            templateConstraints.Length > 0) {
            text.Write(CreateSpace());
            text.Write(CreateKeyword(SyntaxKind.WhereKeyword));
            text.Write(CreateSpace());
            text.Write(CreatePunctuation(SyntaxKind.OpenBraceToken));
            text.Write(CreateSpace());

            foreach (var constraint in templateConstraints) {
                DisplayText.DisplayNode(text, constraint);
                text.Write(CreatePunctuation(SyntaxKind.SemicolonToken));
                text.Write(CreateSpace());
            }

            text.Write(CreatePunctuation(SyntaxKind.CloseBraceToken));
        }
    }

    private static void DisplayMethod(DisplayText text, MethodSymbol method, SymbolDisplayFormat format) {
        if ((format.memberOptions & SymbolDisplayMemberOptions.IncludeAccessibility) != 0)
            DisplayAccessibility(text, method);

        if ((format.memberOptions & SymbolDisplayMemberOptions.IncludeModifiers) != 0) {
            DisplayModifiers(text, method);
            DisplayConstExprRef(text, method.isEffectivelyConst, false, method.refKind);
        }

        if ((format.memberOptions & SymbolDisplayMemberOptions.IncludeType) != 0) {
            DisplayTypeWithAnnotations(text, method.returnTypeWithAnnotations, format);
            text.Write(CreateSpace());
        }

        if ((format.memberOptions & SymbolDisplayMemberOptions.IncludeContainingType) != 0)
            DisplayContainedNames(text, method, format);

        text.Write(CreateIdentifier(method.name));

        DisplayTemplateParameters(text, method.templateParameters, format);

        if ((format.memberOptions & SymbolDisplayMemberOptions.IncludeParameters) != 0) {
            text.Write(CreatePunctuation(SyntaxKind.OpenParenToken));

            for (var i = 0; i < method.parameterCount; i++) {
                if (i > 0) {
                    text.Write(CreatePunctuation(SyntaxKind.CommaToken));
                    text.Write(CreateSpace());
                }

                DisplayParameter(text, method.parameters[i], format);
            }

            text.Write(CreatePunctuation(SyntaxKind.CloseParenToken));
        }

        DisplayTemplateConstraints(text, method.templateConstraints, format);
    }

    private static void DisplayNamedType(DisplayText text, NamedTypeSymbol namedType, SymbolDisplayFormat format) {
        if ((format.memberOptions & SymbolDisplayMemberOptions.IncludeAccessibility) != 0)
            DisplayAccessibility(text, namedType);

        if ((format.memberOptions & SymbolDisplayMemberOptions.IncludeModifiers) != 0)
            DisplayModifiers(text, namedType);

        if (format.includeKeywords) {
            switch (namedType.typeKind) {
                case TypeKind.Class:
                    text.Write(CreateKeyword(SyntaxKind.ClassKeyword));
                    break;
                case TypeKind.Struct:
                    text.Write(CreateKeyword(SyntaxKind.StructKeyword));
                    break;
                case TypeKind.Primitive:
                    text.Write(CreateKeyword(SyntaxKind.PrimitiveKeyword));
                    break;
                default:
                    throw ExceptionUtilities.UnexpectedValue(namedType.typeKind);
            }

            text.Write(CreateSpace());
        }

        DisplayContainedNames(text, namedType, format);
        text.Write(CreateIdentifier(namedType.name));

        DisplayTemplateParameters(text, namedType.templateParameters, format);

        text.Write(CreateSpace());
        text.Write(CreateKeyword(SyntaxKind.ExtendsKeyword));
        text.Write(CreateSpace());

        DisplayType(text, namedType.baseType, format);
        DisplayTemplateConstraints(text, namedType.templateConstraints, format);
    }

    private static void DisplayModifiers(DisplayText text, Symbol symbol) {
        if (symbol.isStatic) {
            text.Write(CreateKeyword(SyntaxKind.StaticKeyword));
            text.Write(CreateSpace());
        }

        if (symbol.isSealed) {
            text.Write(CreateKeyword(SyntaxKind.SealedKeyword));
            text.Write(CreateSpace());
        }

        if (symbol.isVirtual) {
            text.Write(CreateKeyword(SyntaxKind.VirtualKeyword));
            text.Write(CreateSpace());
        }

        if (symbol.isAbstract) {
            text.Write(CreateKeyword(SyntaxKind.AbstractKeyword));
            text.Write(CreateSpace());
        }

        if (symbol.isOverride) {
            text.Write(CreateKeyword(SyntaxKind.OverrideKeyword));
            text.Write(CreateSpace());
        }
    }

    private static void DisplayAccessibility(DisplayText text, Symbol symbol) {
        if (symbol.declaredAccessibility == Accessibility.Public) {
            text.Write(CreateKeyword(SyntaxKind.PublicKeyword));
            text.Write(CreateSpace());
        } else if (symbol.declaredAccessibility == Accessibility.Protected) {
            text.Write(CreateKeyword(SyntaxKind.ProtectedKeyword));
            text.Write(CreateSpace());
        } else if (symbol.declaredAccessibility == Accessibility.Private) {
            text.Write(CreateKeyword(SyntaxKind.PrivateKeyword));
            text.Write(CreateSpace());
        }
    }

    private static void DisplayConstExprRef(DisplayText text, bool isConst, bool isConstExpr, RefKind refKind) {
        var needSpace = false;

        if (isConst) {
            text.Write(CreateKeyword(SyntaxKind.ConstKeyword));
            needSpace = true;
        }

        if (isConstExpr) {
            text.Write(CreateKeyword(SyntaxKind.ConstexprKeyword));
            needSpace = true;
        }

        if (refKind != RefKind.None) {
            if (needSpace)
                text.Write(CreateSpace());

            text.Write(CreateKeyword(SyntaxKind.RefKeyword));
            needSpace = true;
        }

        if (refKind == RefKind.RefConst) {
            if (needSpace)
                text.Write(CreateSpace());

            text.Write(CreateKeyword(SyntaxKind.ConstKeyword));
        }
    }
}
