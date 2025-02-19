using Buckle.CodeAnalysis.Binding;
using Buckle.CodeAnalysis.Symbols;
using Buckle.Diagnostics;

namespace Buckle.CodeAnalysis.Lowering;

internal sealed class LocalFunctionRewriter {
    internal static BoundStatement Rewrite(
        BoundStatement loweredBody,
        NamedTypeSymbol thisType,
        MethodSymbol method,
        TypeCompilationState state,
        BelteDiagnosticQueue diagnostics) {
        return null;
    }
}
