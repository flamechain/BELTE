
namespace Buckle.CodeAnalysis.Syntax.InternalSyntax;

internal abstract partial class TypeSyntax {
    internal bool isImplicitlyTyped
        => IsIdentifierName("var") || IsIdentifierName("const") || IsIdentifierName("constexpr");

    private bool IsIdentifierName(string id) {
        return this is IdentifierNameSyntax name && name.identifier.text == id;
    }
}
