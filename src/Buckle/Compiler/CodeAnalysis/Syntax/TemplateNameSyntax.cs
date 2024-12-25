
namespace Buckle.CodeAnalysis.Syntax;

public sealed partial class TemplateNameSyntax {
    internal override string ErrorDisplayName() {
        return identifier.text;
    }
}
