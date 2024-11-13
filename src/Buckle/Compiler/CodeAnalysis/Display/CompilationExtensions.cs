using System.Linq;
using Buckle.CodeAnalysis.Binding;
using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;
using Buckle.Diagnostics;
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
        var entryPoint = compilation.entryPoint;

        if (entryPoint is not null) {
            EmitTree(compilation, entryPoint, text);
        } else {
            var program = compilation.boundProgram;

            foreach (var pair in program.methodBodies.OrderBy(p => p.Key.name))
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

    internal static void EmitTree(ISymbol symbol, DisplayText text, BoundProgram program) {
        if (symbol is MethodSymbol method) {
            SymbolDisplay.AppendToDisplayText(text, method);

            if (program.TryGetMethodBodyIncludingParents(method, out var body)) {
                text.Write(CreateSpace());
                DisplayText.DisplayNode(text, body);
            } else {
                text.Write(CreatePunctuation(SyntaxKind.SemicolonToken));
                text.Write(CreateLine());
            }
        } else if (symbol is NamedTypeSymbol namedType) {
            SymbolDisplay.AppendToDisplayText(text, symbol);
            WriteTypeMembers(namedType);
        } else if (symbol is DataContainerSymbol v) {
            SymbolDisplay.AppendToDisplayText(text, v);

            if (v.type is NamedTypeSymbol s)
                WriteTypeMembers(s);
            else
                text.Write(CreateLine());
        }

        void WriteTypeMembers(NamedTypeSymbol type, bool writeEnding = true) {
            try {
                var members = type.GetMembersPublic();

                text.Write(CreateSpace());
                text.Write(CreatePunctuation(SyntaxKind.OpenBraceToken));
                text.Write(CreateSpace());
                text.indent++;

                foreach (var field in members.OfType<FieldSymbol>()) {
                    text.Write(CreateLine());
                    SymbolDisplay.AppendToDisplayText(text, field);
                }

                if (members.OfType<FieldSymbol>().Any())
                    text.Write(CreateLine());

                foreach (var method in members.OfType<MethodSymbol>()) {
                    text.Write(CreateLine());
                    SymbolDisplay.AppendToDisplayText(text, method);
                    text.Write(CreateLine());
                }

                foreach (var typeMember in members.OfType<TypeSymbol>()) {
                    text.Write(CreateLine());
                    SymbolDisplay.AppendToDisplayText(text, typeMember);
                    text.Write(CreateLine());
                }

                text.indent--;
                text.Write(CreatePunctuation(SyntaxKind.CloseBraceToken));
                text.Write(CreateLine());
            } catch (BelteInternalException) {
                if (writeEnding) {
                    text.Write(CreatePunctuation(SyntaxKind.SemicolonToken));
                    text.Write(CreateLine());
                }
            }
        }
    }
}
