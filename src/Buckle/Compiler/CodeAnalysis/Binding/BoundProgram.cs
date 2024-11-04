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
            // TODO no entry point error
        } else if (entryPointCandidates.Length == 1) {
            entryPoint = entryPointCandidates[0];

            if (!HasEntryPointSignature(entryPoint))
                diagnostics.Push(Error.InvalidMain(entryPoint.))

            if ((!entryPoint.returnsVoid && entryPoint.returnType.specialType != SpecialType.Int) ||
                entryPoint.returnTypeIsNullable) {
                diagnostics.Push(Error.InvalidMain(entryPoint.returnType.syntaxReference.location));
            }

            if (entryPoint.parameterCount > 0) {
                var invalidSignature = entryPoint.parameterCount != 1 ||
                    entryPoint.parameters[0].type.specialType != SpecialType.List;

                if (!invalidSignature) {
                    var parameterTemplates = entryPoint.parameters[0].type;

                    invalidSignature = invalidSignature || parameterTemplates.Length != 1 || parameterTemplates[0].type
                }
            }

            var argsType = new BoundType(
                binder._scope.LookupSymbol<ClassSymbol>(WellKnownTypeNames.List),
                isNullable: false,
                templateArguments: [new TypeOrConstant(BoundType.String)],
                arity: 1
            );

            if (entryPoint.parameters.Any()) {
                if (entryPoint.parameters.Length != 1 ||
                    entryPoint.parameters[0].name != "args" ||
                    !(entryPoint.parameters[0].type?.Equals(argsType) ?? false)) {
                    binder.diagnostics.Push(Error.InvalidMain(location));
                }
            }
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
    }
}
