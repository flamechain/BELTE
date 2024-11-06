using System.Collections.Generic;
using System.Collections.Immutable;
using Buckle.CodeAnalysis.Binding;
using Buckle.CodeAnalysis.Lowering;
using Buckle.CodeAnalysis.Symbols;
using Buckle.Diagnostics;
using Microsoft.CodeAnalysis.PooledObjects;

namespace Buckle.CodeAnalysis;

internal sealed class MethodCompiler {
    private readonly Compilation _compilation;
    private readonly bool _emitting;
    private readonly BelteDiagnosticQueue _diagnostics;
    private readonly MethodSymbol _entryPoint;
    private readonly Dictionary<MethodSymbol, BoundBlockStatement> _methodBodies;
    private readonly ArrayBuilder<NamedTypeSymbol> _types;

    private MethodCompiler(
        Compilation compilation,
        BelteDiagnosticQueue diagnostics,
        MethodSymbol entryPoint,
        bool emitting) {
        _compilation = compilation;
        _diagnostics = diagnostics;
        _entryPoint = entryPoint;
        _emitting = emitting;
        _types = ArrayBuilder<NamedTypeSymbol>.GetInstance();
        _methodBodies = [];
    }

    internal static BoundProgram CompileMethodBodies(
        Compilation compilation,
        BelteDiagnosticQueue diagnostics,
        bool emitting) {
        var globalNamespace = compilation.globalNamespaceInternal;
        var entryPoint = GetEntryPoint(globalNamespace, diagnostics);

        var methodCompiler = new MethodCompiler(compilation, diagnostics, entryPoint, emitting);

        methodCompiler.CompileNamespace(globalNamespace);
        return methodCompiler.CreateBoundProgram();
    }

    private static MethodSymbol GetEntryPoint(NamespaceSymbol globalNamespace, BelteDiagnosticQueue diagnostics) {
        var builder = ArrayBuilder<MethodSymbol>.GetInstance();
        var globalsCount = 0;

        foreach (var member in globalNamespace.GetMembers(WellKnownMemberNames.EntryPointMethodName)) {
            if (member is MethodSymbol m && HasEntryPointSignature(m)) {
                if (m is SynthesizedEntryPoint)
                    globalsCount++;

                builder.Add(m);
            }
        }

        var entryPointCandidates = builder.ToImmutableAndFree();
        MethodSymbol entryPoint = null;

        if (entryPointCandidates.Length == 0) {
            diagnostics.Push(Error.NoSuitableEntryPoint());
        } else if (entryPointCandidates.Length == 1) {
            entryPoint = entryPointCandidates[0];
        } else {
            if (entryPointCandidates.Length > globalsCount + 1)
                diagnostics.Push(Error.MultipleMains(entryPointCandidates[0].location));

            if (globalsCount > 0 && entryPointCandidates.Length > globalsCount)
                diagnostics.Push(Error.MainAndGlobals(entryPointCandidates[0].location));

            if (globalsCount > 1) {
                for (var i = 0; i < entryPointCandidates.Length; i++) {
                    if (entryPointCandidates[i] is SynthesizedEntryPoint s)
                        diagnostics.Push(Error.GlobalStatementsInMultipleFiles(s.location));
                }
            }
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
        return new BoundProgram(null, _types.ToImmutableAndFree(), _entryPoint);
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
        _types.Add(namedType);

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
                    if (f.isConstExpr)
                        f.GetConstantValue(ConstantFieldsInProgress.Empty);

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

        var currentDiagnostics = BelteDiagnosticQueue.GetInstance();
        ImmutableArray<BoundStatement>? analyzedInitializers = null;

        var includeInitializers = method.IncludeFieldInitializersInBody();
        var includeNonEmptyInitializers = includeInitializers &&
            !processedInitializers.boundInitializers.IsDefaultOrEmpty;

        if (includeNonEmptyInitializers && processedInitializers.loweredInitializers is null)
            analyzedInitializers = InitializerRewriter.RewriteConstructor(processedInitializers.boundInitializers);

        var body = BindMethodBody(
            method,
            state,
            currentDiagnostics,
            includeInitializers,
            analyzedInitializers
        );

        if (!_emitting || currentDiagnostics.AnyErrors()) {
            _diagnostics.PushRangeAndFree(currentDiagnostics);
            _methodBodies.Add(method, body);
            return;
        }

        var loweredBody = Lowerer.Lower(method, body);
    }
}
