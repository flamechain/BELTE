
namespace Buckle.CodeAnalysis.Symbols;

/// <summary>
/// Type of type symbol.
/// </summary>
public enum TypeKind : byte {
    Array,
    Class,
    Struct,
    Primitive,
    TemplateParameter,
    Error,
}
