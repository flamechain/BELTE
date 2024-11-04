using Buckle.CodeAnalysis.Symbols;
using Buckle.Diagnostics;
using Microsoft.CodeAnalysis.PooledObjects;

namespace Buckle.CodeAnalysis.Binding;

internal sealed class BoundProgram {
    private BoundProgram() { }

    internal static BoundProgram Create(Compilation compilation, BelteDiagnosticQueue diagnostics) {
        var globalNamespace = compilation.globalNamespaceInternal;
        var builder = ArrayBuilder<MethodSymbol>.GetInstance();

        foreach (var member in globalNamespace.GetMembers(WellKnownMemberNames.EntryPointMethodName)) {
            if (member is MethodSymbol m && HasEntryPointSignature(m))
                builder.Add(m);
        }

        var entryPointCandidates = builder.ToImmutableAndFree();
        MethodSymbol entryPoint = null;

        if (entryPointCandidates.Length == 0) {
            diagnostics.Push(Error.NoSuitableEntryPoint());
        } else if (entryPointCandidates.Length == 1) {
            entryPoint = entryPointCandidates[0];
        } else {
            diagnostics.Push(Error.MultipleMains(entryPointCandidates[0].location));
        }


    }

    private static bool HasEntryPointSignature(MethodSymbol method) {
        var returnType = method.returnType;

        if (returnType.specialType != SpecialType.Int && !returnType.IsVoidType())
            return false;

        if (method.refKind != RefKind.None)
            return false;

        if (method.parameterCount == 0)
            return true;

        if (method.parameterCount > 1)
            return false;

        if (!method.parameterRefKinds.IsDefault)
            return false;

        var firstType = method.parameters[0].type;

        if (firstType.specialType != SpecialType.List)
            return false;

        var elementType = ((NamedTypeSymbol)firstType).templateArguments[0].type;

        if (elementType.specialType != SpecialType.String)
            return false;

        return true;
    }
}
