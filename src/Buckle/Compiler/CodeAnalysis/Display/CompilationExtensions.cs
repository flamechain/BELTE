using System.Collections.Immutable;
using System.Linq;
using Buckle.CodeAnalysis.Binding;
using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;
using static Buckle.CodeAnalysis.Display.DisplayTextSegment;

namespace Buckle.CodeAnalysis.Display;

/// <summary>
/// Extensions on the <see cref="Compilation" /> class, adding the ability to emit the tree to a display.
/// </summary>
public static class CompilationExtensions {
    /// <summary>
    /// Emits the parse tree of the compilation.
    /// </summary>
    /// <param name="text">Out.</param>
    public static void EmitTree(this Compilation compilation, DisplayText text) {
        var program = compilation.boundProgram;
        var isFirst = true;

        foreach (var type in program.types) {
            if (isFirst)
                isFirst = false;
            else
                text.Write(CreateLine());

            EmitTree(compilation, type, text);
        }

        foreach (var pair in program.methodBodies) {
            if (isFirst)
                isFirst = false;
            else
                text.Write(CreateLine());

            EmitTree(compilation, pair.Key, text);
        }
    }

    /// <summary>
    /// Emits the parse tree of a single <see cref="Symbol" />.
    /// </summary>
    /// <param name="symbol"><see cref="Symbol" /> to be the root of the <see cref="SyntaxTree" /> displayed.</param>
    /// <param name="text">Out.</param>
    public static void EmitTree(this Compilation compilation, ISymbol symbol, DisplayText text) {
        var program = compilation.boundProgram;
        EmitTree(symbol, text, program);
    }

    public static ImmutableArray<IDataContainerSymbol> GetMethodLocals(IMethodSymbol method) {
        // TODO need to skip every other
        if (method is SourceMethodSymbol s)
            return s.outerBinder.next.locals.Where((x, i) => i % 2 == 0).ToImmutableArray().CastArray<IDataContainerSymbol>();
        else if (method is SynthesizedEntryPoint e)
            return e.programBinder.locals.Where((x, i) => i % 2 == 0).ToImmutableArray().CastArray<IDataContainerSymbol>();
        else
            return [];
    }

    internal static void EmitTree(ISymbol symbol, DisplayText text, BoundProgram program) {
        if (symbol is MethodSymbol method) {
            SymbolDisplay.AppendToDisplayText(text, method, SymbolDisplayFormat.Everything);

            if (program.TryGetMethodBodyIncludingParents(method, out var body)) {
                text.Write(CreateSpace());
                DisplayText.DisplayNode(text, body);
            } else {
                text.Write(CreatePunctuation(SyntaxKind.SemicolonToken));
                text.Write(CreateLine());
            }
        } else if (symbol is NamedTypeSymbol namedType) {
            SymbolDisplay.AppendToDisplayText(text, symbol, SymbolDisplayFormat.Everything);
            WriteTypeMembers(namedType);
        } else if (symbol is DataContainerSymbol v) {
            SymbolDisplay.AppendToDisplayText(text, v, SymbolDisplayFormat.Everything);

            if (v.type is NamedTypeSymbol s)
                WriteTypeMembers(s);
            else
                text.Write(CreateLine());
        }

        void WriteTypeMembers(NamedTypeSymbol type) {
            var members = type.GetMembers();

            text.Write(CreateSpace());
            text.Write(CreatePunctuation(SyntaxKind.OpenBraceToken));
            text.Write(CreateSpace());
            text.indent++;

            foreach (var member in members) {
                text.Write(CreateLine());
                SymbolDisplay.AppendToDisplayText(text, member, SymbolDisplayFormat.Everything);
                text.Write(CreateLine());
            }

            text.indent--;
            text.Write(CreatePunctuation(SyntaxKind.CloseBraceToken));
            text.Write(CreateLine());
        }
    }
}
