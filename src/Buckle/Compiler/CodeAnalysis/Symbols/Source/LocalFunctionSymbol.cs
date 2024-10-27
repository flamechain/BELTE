using Buckle.CodeAnalysis.Binding;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Symbols;

internal sealed class LocalFunctionSymbol : SourceMethodSymbol {
    internal LocalFunctionSymbol(Binder binder, Symbol containingSymbol, LocalFunctionStatementSyntax syntax)
        : base(new SyntaxReference(syntax)) {

    }
}
