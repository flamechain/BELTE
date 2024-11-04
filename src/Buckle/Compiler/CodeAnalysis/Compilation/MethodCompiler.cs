using Buckle.CodeAnalysis.Binding;
using Buckle.CodeAnalysis.Symbols;
using Buckle.Diagnostics;
using Microsoft.CodeAnalysis.PooledObjects;

namespace Buckle.CodeAnalysis;

internal sealed class MethodCompiler {
    private readonly Compilation _compilation;
    private readonly BelteDiagnosticQueue _diagnostics;
    private readonly MethodSymbol _entryPoint;

    private MethodCompiler(Compilation compilation, BelteDiagnosticQueue diagnostics, MethodSymbol entryPoint) {
        _compilation = compilation;
        _diagnostics = diagnostics;
        _entryPoint = entryPoint;
    }

    internal static BoundProgram CompileMethodBodies(Compilation compilation, BelteDiagnosticQueue diagnostics) {
        var globalNamespace = compilation.globalNamespaceInternal;
        var entryPoint = GetEntryPoint(globalNamespace, diagnostics);

        var methodCompiler = new MethodCompiler(compilation, diagnostics, entryPoint);

        methodCompiler.CompileNamespace(globalNamespace);
        return methodCompiler.CreateBoundProgram();
    }

    private static MethodSymbol GetEntryPoint(NamespaceSymbol globalNamespace, BelteDiagnosticQueue diagnostics) {
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

        return entryPoint;
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

    private BoundProgram CreateBoundProgram() {

    }

    private void CompileNamespace(NamespaceSymbol globalNamespace) {
        foreach (var member in globalNamespace.GetMembersUnordered()) {
            switch (member) {
                case NamedTypeSymbol n:
                    CompileNamedType(n);
                    break;
            }
        }
    }

    private void CompileNamedType(NamedTypeSymbol namedType) {
        var state = new TypeCompilationState(namedType, _compilation);
        var members = namedType.GetMembers();
        var processedInitializers = new Binder.ProcessedFieldInitializers();

        var sourceType = namedType as SourceMemberContainerTypeSymbol;

        if (sourceType is not null) {
            Binder.BindFieldInitializers(
                _compilation,
                sourceType.initializers,
                _diagnostics,
                ref processedInitializers
            );
        }

        for (var ordinal = 0; ordinal < members.Length; ordinal++) {
            var member = members[ordinal];

            switch (member) {
                case NamedTypeSymbol n:
                    CompileNamedType(n);
                    break;
                case MethodSymbol m:
                    var initializers = m.methodKind == MethodKind.Constructor ? processedInitializers : default;
                    CompileMethod(m, ordinal, ref initializers, state);
                    break;
                case FieldSymbol f:
                    if (f.isConstExpr) {
                        var constantValue = f.GetConstantValue(ConstantFieldsInProgress.Empty);

                        if (constantValue is null)
                            // TODO error
                            ;
                    }

                    break;
            }
        }
    }

    private void CompileMethod(
        MethodSymbol method,
        int ordinal,
        ref Binder.ProcessedFieldInitializers processedInitializers,
        TypeCompilationState state) {
        var sourceMethod = method as SourceMethodSymbol;

        if (method.isAbstract)
            return;


    }
}
