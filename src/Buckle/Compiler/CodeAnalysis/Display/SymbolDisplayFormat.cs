
namespace Buckle.CodeAnalysis.Display;

public sealed class SymbolDisplayFormat {
    public static readonly SymbolDisplayFormat ErrorMessageFormat = new SymbolDisplayFormat(
        qualificationStyle: SymbolDisplayQualificationStyle.IncludeContainingTypes,
        templateOptions: SymbolDisplayTemplateOptions.IncludeTemplateParameters,
        memberOptions: SymbolDisplayMemberOptions.IncludeParameters | SymbolDisplayMemberOptions.IncludeContainingType,
        parameterOptions: SymbolDisplayParameterOptions.IncludeModifiers | SymbolDisplayParameterOptions.IncludeType,
        miscellaneousOptions: SymbolDisplayMiscellaneousOptions.SimplifyNullable
    );

    public static readonly SymbolDisplayFormat QualifiedNameFormat = new SymbolDisplayFormat(
        qualificationStyle: SymbolDisplayQualificationStyle.Everything,
        templateOptions: SymbolDisplayTemplateOptions.IncludeTemplateParameters,
        memberOptions: SymbolDisplayMemberOptions.None,
        parameterOptions: SymbolDisplayParameterOptions.IncludeModifiers | SymbolDisplayParameterOptions.IncludeType,
        miscellaneousOptions: SymbolDisplayMiscellaneousOptions.None
    );

    public static readonly SymbolDisplayFormat Everything = new SymbolDisplayFormat(
        qualificationStyle:
            SymbolDisplayQualificationStyle.IncludeContainingTypes |
            SymbolDisplayQualificationStyle.IncludeContainingNamespaces,
        templateOptions: SymbolDisplayTemplateOptions.Everything,
        memberOptions: SymbolDisplayMemberOptions.Everything,
        parameterOptions: SymbolDisplayParameterOptions.Everything,
        miscellaneousOptions: SymbolDisplayMiscellaneousOptions.None
    );

    public static readonly SymbolDisplayFormat DebuggerDisplay = new SymbolDisplayFormat(
        qualificationStyle: SymbolDisplayQualificationStyle.None,
        templateOptions: SymbolDisplayTemplateOptions.Everything,
        memberOptions: SymbolDisplayMemberOptions.IncludeParameters,
        parameterOptions: SymbolDisplayParameterOptions.Everything,
        miscellaneousOptions: SymbolDisplayMiscellaneousOptions.IncludeKeywords
    );

    private SymbolDisplayFormat(
        SymbolDisplayQualificationStyle qualificationStyle,
        SymbolDisplayTemplateOptions templateOptions,
        SymbolDisplayMemberOptions memberOptions,
        SymbolDisplayParameterOptions parameterOptions,
        SymbolDisplayMiscellaneousOptions miscellaneousOptions) {
        this.qualificationStyle = qualificationStyle;
        this.templateOptions = templateOptions;
        this.memberOptions = memberOptions;
        this.parameterOptions = parameterOptions;
        this.miscellaneousOptions = miscellaneousOptions;
    }

    internal SymbolDisplayQualificationStyle qualificationStyle { get; }

    internal SymbolDisplayTemplateOptions templateOptions { get; }

    internal SymbolDisplayMemberOptions memberOptions { get; }

    internal SymbolDisplayParameterOptions parameterOptions { get; }

    internal SymbolDisplayMiscellaneousOptions miscellaneousOptions { get; }
}
