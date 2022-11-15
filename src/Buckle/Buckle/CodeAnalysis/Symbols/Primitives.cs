
namespace Buckle.CodeAnalysis.Symbols;

/// <summary>
/// A type symbol. This is just the type name, not a full type clause.
/// </summary>
internal class TypeSymbol : Symbol {
    /// <summary>
    /// Error type (meaning something went wrong, not an actual type).
    /// </summary>
    internal static readonly TypeSymbol Error = new TypeSymbol("?");

    /// <summary>
    /// Integer type (any whole number, signed).
    /// </summary>
    internal static readonly TypeSymbol Int = new TypeSymbol("int");

    /// <summary>
    /// Decimal type (any decimal number, precision TBD).
    /// </summary>
    internal static readonly TypeSymbol Decimal = new TypeSymbol("decimal");

    /// <summary>
    /// Boolean type (true/false).
    /// </summary>
    internal static readonly TypeSymbol Bool = new TypeSymbol("bool");

    /// <summary>
    /// String type.
    /// </summary>
    internal static readonly TypeSymbol String = new TypeSymbol("string");

    /// <summary>
    /// Any type (effectively the object type).
    /// </summary>
    internal static readonly TypeSymbol Any = new TypeSymbol("any");

    /// <summary>
    /// Void type (lack of type, exclusively used in function declarations).
    /// </summary>
    internal static readonly TypeSymbol Void = new TypeSymbol("void");

    internal override SymbolType type => SymbolType.Type;

    /// <summary>
    /// Creates a new type symbol.
    /// Use predefined type symbols if possible.
    /// </summary>
    /// <param name="name">Name of type</param>
    internal TypeSymbol(string name) : base(name) { }
}
