using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading;
using Buckle.CodeAnalysis.Syntax;
using Buckle.CodeAnalysis.Text;
using Buckle.Diagnostics;
using Buckle.Libraries;
using Buckle.Utilities;
using Microsoft.CodeAnalysis.PooledObjects;

namespace Buckle.CodeAnalysis.Symbols;

internal sealed class GlobalNamespaceSymbol : NamespaceSymbol {
    private readonly ImmutableArray<MemberDeclarationSyntax> _declarations;

    private SymbolCompletionState _state;
    private ImmutableArray<Symbol> _lazyAllMembers;
    private ImmutableArray<NamedTypeSymbol> _lazyTypeMembersUnordered;
    private Dictionary<ReadOnlyMemory<char>, ImmutableArray<NamespaceOrTypeSymbol>> _nameToMembersMap;
    private Dictionary<ReadOnlyMemory<char>, ImmutableArray<NamedTypeSymbol>> _nameToTypeMembersMap;
    private bool _lazyAllMembersIsSorted;

    internal GlobalNamespaceSymbol(NamespaceExtent extent, ImmutableArray<MemberDeclarationSyntax> mergedDeclarations) {
        this.extent = extent;
        _declarations = mergedDeclarations;
    }

    public override string name => "global";

    internal override NamespaceExtent extent { get; }

    internal override SyntaxReference syntaxReference => null;

    internal override TextLocation location => null;

    internal override Symbol containingSymbol => null;

    internal override void ForceComplete(TextLocation location) {
        while (true) {
            var incompletePart = _state.nextIncompletePart;

            switch (incompletePart) {
                case CompletionParts.NameToMembersMap:
                    _ = GetNameToMembersMap();
                    break;
                case CompletionParts.MembersCompleted: {
                        var members = GetMembers();
                        var allCompleted = true;

                        foreach (var member in members) {
                            member.ForceComplete(location);
                            allCompleted = allCompleted && member.HasComplete(CompletionParts.All);
                        }

                        if (allCompleted) {
                            _state.NotePartComplete(CompletionParts.MembersCompleted);
                            break;
                        } else {
                            goto done;
                        }
                    }
                case CompletionParts.None:
                    return;
                default:
                    _state.NotePartComplete(CompletionParts.All & ~CompletionParts.NamespaceSymbolAll);
                    break;
            }

            _state.SpinWaitComplete(incompletePart);
        }

done:
        _state.SpinWaitComplete(CompletionParts.NamespaceSymbolAll);
    }

    internal override bool HasComplete(CompletionParts part) {
        return _state.HasComplete(part);
    }

    internal override ImmutableArray<Symbol> GetMembersUnordered() {
        var result = _lazyAllMembers;

        if (result.IsDefault) {
            var members = StaticCast<Symbol>.From(GetNameToMembersMap().Flatten());
            ImmutableInterlocked.InterlockedInitialize(ref _lazyAllMembers, members);
            result = _lazyAllMembers;
        }

        return result;
    }

    internal override ImmutableArray<Symbol> GetMembers() {
        if (_lazyAllMembersIsSorted)
            return _lazyAllMembers;

        var allMembers = GetMembersUnordered();

        if (allMembers.Length > 1) {
            allMembers = allMembers.Sort(LexicalOrderSymbolComparer.Instance);
            ImmutableInterlocked.InterlockedExchange(ref _lazyAllMembers, allMembers);
        }

        _lazyAllMembersIsSorted = true;
        return allMembers;
    }

    internal override ImmutableArray<Symbol> GetMembers(ReadOnlyMemory<char> name) {
        return GetNameToMembersMap().TryGetValue(name, out var members)
            ? members.Cast<NamespaceOrTypeSymbol, Symbol>()
            : [];
    }

    internal override ImmutableArray<NamedTypeSymbol> GetTypeMembersUnordered() {
        if (_lazyTypeMembersUnordered.IsDefault) {
            var members = GetNameToTypeMembersMap().Flatten();
            ImmutableInterlocked.InterlockedInitialize(ref _lazyTypeMembersUnordered, members);
        }

        return _lazyTypeMembersUnordered;
    }

    internal override ImmutableArray<NamedTypeSymbol> GetTypeMembers() {
        return GetNameToTypeMembersMap().Flatten(LexicalOrderSymbolComparer.Instance);
    }

    internal override ImmutableArray<NamedTypeSymbol> GetTypeMembers(ReadOnlyMemory<char> name) {
        return GetNameToTypeMembersMap().TryGetValue(name, out var members) ? members : [];
    }

    private Dictionary<ReadOnlyMemory<char>, ImmutableArray<NamespaceOrTypeSymbol>> GetNameToMembersMap() {
        if (_nameToMembersMap is null) {
            var diagnostics = BelteDiagnosticQueue.GetInstance();

            if (Interlocked.CompareExchange(ref _nameToMembersMap, MakeNameToMembersMap(diagnostics), null) is null) {
                AddDeclarationDiagnostics(diagnostics);
                RegisterDeclaredCorTypes();

                _state.NotePartComplete(CompletionParts.NameToMembersMap);
            }

            diagnostics.Free();
        }

        return _nameToMembersMap;
    }

    private Dictionary<ReadOnlyMemory<char>, ImmutableArray<NamedTypeSymbol>> GetNameToTypeMembersMap() {
        if (_nameToTypeMembersMap is null) {
            Interlocked.CompareExchange(
                ref _nameToTypeMembersMap,
                ImmutableArrayExtensions
                    .GetTypesFromMemberMap<ReadOnlyMemory<char>, NamespaceOrTypeSymbol, NamedTypeSymbol>(
                        GetNameToMembersMap(),
                        ReadOnlyMemoryOfCharComparer.Instance
                    ),
                null
            );
        }

        return _nameToTypeMembersMap;
    }

    private Dictionary<ReadOnlyMemory<char>, ImmutableArray<NamespaceOrTypeSymbol>> MakeNameToMembersMap(
        BelteDiagnosticQueue diagnostics) {
        var builder = NameToObjectPool.Allocate();
        var globals = new Dictionary<SourceText, ArrayBuilder<GlobalStatementSyntax>>();
        var methods = new List<MethodDeclarationSyntax>();

        foreach (var declaration in _declarations) {
            if (declaration is GlobalStatementSyntax g) {
                var sourceText = g.location.text;

                if (globals.TryGetValue(sourceText, out var value)) {
                    value.Add(g);
                } else {
                    var globalsBuilder = ArrayBuilder<GlobalStatementSyntax>.GetInstance();
                    globalsBuilder.Add(g);
                    globals.Add(sourceText, globalsBuilder);
                }
            } else if (declaration is MethodDeclarationSyntax m) {
                methods.Add(m);
            } else {
                var symbol = BuildSymbol(declaration, diagnostics);
                ImmutableArrayExtensions.AddToMultiValueDictionaryBuilder(builder, symbol.name.AsMemory(), symbol);
            }
        }

        BuildProgram(diagnostics, builder, globals, methods);

        var result = new Dictionary<ReadOnlyMemory<char>, ImmutableArray<NamespaceOrTypeSymbol>>(
            builder.Count,
            ReadOnlyMemoryOfCharComparer.Instance
        );

        ImmutableArrayExtensions.CreateNameToMembersMap<
                ReadOnlyMemory<char>,
                NamespaceOrTypeSymbol,
                NamedTypeSymbol,
                NamespaceSymbol
            >(builder, result);

        builder.Free();
        return result;
    }

    private void BuildProgram(
        BelteDiagnosticQueue diagnostics,
        PooledDictionary<ReadOnlyMemory<char>, object> builder,
        Dictionary<SourceText, ArrayBuilder<GlobalStatementSyntax>> globals,
        List<MethodDeclarationSyntax> methods) {
        if (globals.Count > 0 || methods.Count > 0) {
            var returnType = new TypeWithAnnotations(CorLibrary.GetSpecialType(SpecialType.Void));
            var program = new SynthesizedProgram(
                this,
                WellKnownMemberNames.TopLevelStatementsEntryPointTypeName,
                TypeKind.Class,
                CorLibrary.GetSpecialType(SpecialType.Object),
                DeclarationModifiers.Static
            );

            var membersBuilder = ArrayBuilder<Symbol>.GetInstance();
            var localFunctionStatements = ArrayBuilder<GlobalStatementSyntax>.GetInstance();

            foreach (var method in methods) {
                var localFunction = SyntaxFactory.GlobalStatement(
                    SyntaxFactory.List<AttributeListSyntax>(),
                    null,
                    SyntaxFactory.LocalFunctionStatement(
                        method.attributeLists,
                        method.modifiers,
                        method.returnType,
                        method.identifier,
                        method.parameterList,
                        method.body
                    )
                );

                localFunctionStatements.Add(localFunction);
            }

            var attributedMethods = false;

            foreach (var keyValuePair in globals) {
                var statements = keyValuePair.Value;

                if (!attributedMethods) {
                    attributedMethods = true;
                    statements.AddRange(localFunctionStatements.ToImmutableAndFree());
                }

                var entryPoint = new SynthesizedEntryPoint(
                    program,
                    returnType,
                    statements.ToImmutableAndFree(),
                    _declarations[0].syntaxTree.GetCompilationUnitRoot()
                );

                membersBuilder.Add(entryPoint);
            }

            if (!attributedMethods) {
                var emptyEntryPoint = new SynthesizedEntryPoint(
                    program,
                    returnType,
                    localFunctionStatements.ToImmutableAndFree(),
                    _declarations[0].syntaxTree.GetCompilationUnitRoot()
                );

                membersBuilder.Add(emptyEntryPoint);
            }

            program.FinishProgram(membersBuilder.ToImmutableAndFree());
            ImmutableArrayExtensions.AddToMultiValueDictionaryBuilder(builder, program.name.AsMemory(), program);
        }
    }

    private NamespaceOrTypeSymbol BuildSymbol(MemberDeclarationSyntax declaration, BelteDiagnosticQueue diagnostics) {
        switch (declaration.kind) {
            case SyntaxKind.StructDeclaration:
            case SyntaxKind.ClassDeclaration:
                return new SourceNamedTypeSymbol(this, (TypeDeclarationSyntax)declaration, diagnostics);
            default:
                throw ExceptionUtilities.UnexpectedValue(declaration.kind);
        }
    }

    private void RegisterDeclaredCorTypes() {
        if (Compilation.KeepLookingForCorTypes) {
            foreach (var array in _nameToMembersMap.Values) {
                foreach (var member in array) {
                    if (member is NamedTypeSymbol type && type.specialType != SpecialType.None) {
                        containingCompilation.RegisterDeclaredSpecialType(type);

                        if (!Compilation.KeepLookingForCorTypes)
                            return;
                    }
                }
            }
        }
    }
}
