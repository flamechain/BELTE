using Buckle.CodeAnalysis.Symbols;
using static Buckle.Libraries.LibraryHelpers;

namespace Buckle.Libraries.Standard;

internal static partial class StandardLibrary {
    internal static NamedTypeSymbol Console = StaticClass("Console", [
        StaticClass("Color", [
            ConstExprField("Black", SpecialType.Int, 0),
            ConstExprField("DarkBlue", SpecialType.Int, 1),
            ConstExprField("DarkGreen", SpecialType.Int, 2),
            ConstExprField("DarkCyan", SpecialType.Int, 3),
            ConstExprField("DarkRed", SpecialType.Int, 4),
            ConstExprField("DarkMagenta", SpecialType.Int, 5),
            ConstExprField("DarkYellow", SpecialType.Int, 6),
            ConstExprField("Gray", SpecialType.Int, 7),
            ConstExprField("DarkGray", SpecialType.Int, 8),
            ConstExprField("Blue", SpecialType.Int, 9),
            ConstExprField("Green", SpecialType.Int, 10),
            ConstExprField("Cyan", SpecialType.Int, 11),
            ConstExprField("Red", SpecialType.Int, 12),
            ConstExprField("Magenta", SpecialType.Int, 13),
            ConstExprField("Yellow", SpecialType.Int, 14),
            ConstExprField("White", SpecialType.Int, 15)
        ]),
        StaticMethod("GetWidth", SpecialType.Int),
        StaticMethod("GetHeight", SpecialType.Int),
        StaticMethod("Input", SpecialType.String),
        StaticMethod("PrintLine", SpecialType.Void),
        StaticMethod("PrintLine", SpecialType.Void, [("message", SpecialType.String, true)]),
        StaticMethod("PrintLine", SpecialType.Void, [("value", SpecialType.Any, true)]),
        StaticMethod("PrintLine", SpecialType.Void, [("object", SpecialType.Object, true)]),
        StaticMethod("Print", SpecialType.Void, [("message", SpecialType.String, true)]),
        StaticMethod("Print", SpecialType.Void, [("value", SpecialType.Any, true)]),
        StaticMethod("Print", SpecialType.Void, [("object", SpecialType.Object, true)]),
        StaticMethod("ResetColor", SpecialType.Void),
        StaticMethod("SetForegroundColor", SpecialType.Void, [("color", SpecialType.Int)]),
        StaticMethod("SetBackgroundColor", SpecialType.Void, [("color", SpecialType.Int)]),
        StaticMethod("SetCursorPosition", SpecialType.Void, [
            ("left", SpecialType.Int, true), ("top", SpecialType.Int, true)]),
    ]);

    // TODO Finish copying these over
    internal static NamedTypeSymbol Directory = StaticClass("Directory", []);

    internal static NamedTypeSymbol File = StaticClass("File", []);

    internal static NamedTypeSymbol Math = StaticClass("Math", []);

    // TODO Should probably reevaluate what to do about method implementations
    // The method map was efficient but very messy to setup
    // Will be some sort of map, but hopefully less messy (make sure to lock to avoid race conditions)
    // Easiest way is to just create unique string names? Then give SynthesizedFinishedMethodSymbol its own field
    // Set hasSpecialName to true? (Only do this after implemented what this field is actually for)
    // Call it `corName` or `stlName` or `standardLibraryName` or something
    // Could also just attach all of the map data (C# string and Evaluator function) directly on the method symbol to
    // Avoid needing to do a lookup. This is NOT less space efficient because we shouldn't be creating multiple copies
    // Of these symbols. Could then in the Evaluator just be like
    // "Is the ContainingSymbol Console, Math, etc.? Ok now I will as cast this to the Synthesized and invoke the
    // function"
    // Or "Is the ContainingSymbol Console, Math, etc.? Ok now I will lookup the map using the unique string name"
}
