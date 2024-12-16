
namespace Buckle.CodeAnalysis.Syntax;

public abstract partial class SimpleNameSyntax {
    public int arity => this is TemplateNameSyntax t ? t.templateArgumentList.arguments.Count : 0;
}
