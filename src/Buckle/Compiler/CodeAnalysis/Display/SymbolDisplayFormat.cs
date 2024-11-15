
namespace Buckle.CodeAnalysis.Display;

public sealed class SymbolDisplayFormat {
    public static readonly SymbolDisplayFormat ErrorMessageFormat = new SymbolDisplayFormat(
        qualificationStyle: SymbolDisplayQualificationStyle.IncludeContainingTypes,
        templateOptions: SymbolDisplayTemplateOptions.IncludeTemplateParameters,
        memberOptions: SymbolDisplayMemberOptions.IncludeParameters | SymbolDisplayMemberOptions.IncludeContainingType,
        parameterOptions: SymbolDisplayParameterOptions.IncludeModifiers | SymbolDisplayParameterOptions.IncludeType,
        includeKeywords: false
    );

    public static readonly SymbolDisplayFormat QualifiedNameFormat = new SymbolDisplayFormat(
        qualificationStyle: SymbolDisplayQualificationStyle.Everything,
        templateOptions: SymbolDisplayTemplateOptions.IncludeTemplateParameters,
        memberOptions: SymbolDisplayMemberOptions.None,
        parameterOptions: SymbolDisplayParameterOptions.IncludeModifiers | SymbolDisplayParameterOptions.IncludeType,
        includeKeywords: false
    );

    public static readonly SymbolDisplayFormat Everything = new SymbolDisplayFormat(
        qualificationStyle:
            SymbolDisplayQualificationStyle.IncludeContainingTypes |
            SymbolDisplayQualificationStyle.IncludeContainingNamespaces,
        templateOptions: SymbolDisplayTemplateOptions.Everything,
        memberOptions: SymbolDisplayMemberOptions.Everything,
        parameterOptions: SymbolDisplayParameterOptions.Everything,
        includeKeywords: true
    );

    private SymbolDisplayFormat(
        SymbolDisplayQualificationStyle qualificationStyle,
        SymbolDisplayTemplateOptions templateOptions,
        SymbolDisplayMemberOptions memberOptions,
        SymbolDisplayParameterOptions parameterOptions,
        bool includeKeywords) {
        this.qualificationStyle = qualificationStyle;
        this.templateOptions = templateOptions;
        this.memberOptions = memberOptions;
        this.parameterOptions = parameterOptions;
        this.includeKeywords = includeKeywords;
    }

    internal SymbolDisplayQualificationStyle qualificationStyle { get; }

    internal SymbolDisplayTemplateOptions templateOptions { get; }

    internal SymbolDisplayMemberOptions memberOptions { get; }

    internal SymbolDisplayParameterOptions parameterOptions { get; }

    internal bool includeKeywords { get; }
}
