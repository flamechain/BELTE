using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using Buckle.CodeAnalysis.Binding;
using Buckle.CodeAnalysis.Syntax;
using Buckle.Utilities;
using Microsoft.CodeAnalysis.PooledObjects;

namespace Buckle.CodeAnalysis.Symbols;

internal abstract class NamedTypeSymbol : TypeSymbol, ITypeSymbolWithMembers, ISymbolWithTemplates {
    protected readonly DeclarationModifiers _declarationModifiers;
    protected List<Symbol> _lazyMembers;
    protected Dictionary<string, ImmutableArray<Symbol>> _lazyMembersDictionary;

    internal NamedTypeSymbol(
        ImmutableArray<ParameterSymbol> templateParameters,
        ImmutableArray<BoundExpression> templateConstraints,
        ImmutableArray<Symbol> symbols,
        TypeDeclarationSyntax declaration,
        DeclarationModifiers modifiers,
        Accessibility accessibility)
        : base(declaration?.identifier?.text, accessibility) {
        members = symbols;
        this.declaration = declaration;
        this.templateParameters = templateParameters;
        this.templateConstraints = templateConstraints;
        _declarationModifiers = modifiers;

        foreach (var member in members)
            member.SetContainingType(this);
    }

    public override SymbolKind kind => SymbolKind.Type;

    public override bool isStatic => (_declarationModifiers & DeclarationModifiers.Static) != 0;

    public override bool isAbstract => (_declarationModifiers & DeclarationModifiers.Abstract) != 0;

    public override bool isSealed => (_declarationModifiers & DeclarationModifiers.Sealed) != 0;

    public override bool isVirtual => false;

    public override bool isOverride => false;

    public ImmutableArray<MethodSymbol> constructors => GetConstructors();

    public ImmutableArray<ParameterSymbol> templateParameters { get; set; }

    public ImmutableArray<BoundExpression> templateConstraints { get; set; }

    internal ImmutableArray<Symbol> members { get; private set; }

    internal override int arity => templateParameters.Length;

    internal bool isLowLevel => (_declarationModifiers & DeclarationModifiers.LowLevel) != 0;

    internal TypeDeclarationSyntax declaration { get; }

    public ImmutableArray<Symbol> GetMembers(string name) {
        if (_lazyMembersDictionary is null || _lazyMembers is null)
            ConstructLazyMembersDictionary();

        return _lazyMembersDictionary.TryGetValue(name, out var result) ? result : ImmutableArray<Symbol>.Empty;
    }

    public ImmutableArray<ISymbol> GetMembers() {
        if (_lazyMembers is null)
            ConstructLazyMembers();

        return _lazyMembers.ToImmutableArray<ISymbol>();
    }

    /// <summary>
    /// Gets a string representation of the type signature without template parameter names.
    /// </summary>
    public string Signature() {
        var signature = new StringBuilder($"{name}<");
        var isFirst = true;

        foreach (var parameter in templateParameters) {
            if (isFirst)
                isFirst = false;
            else
                signature.Append(", ");

            signature.Append(parameter.type);
        }

        signature.Append('>');

        return signature.ToString();
    }

    internal void UpdateInternals(
        ImmutableArray<ParameterSymbol> templateParameters,
        ImmutableArray<BoundExpression> templateConstraints,
        ImmutableArray<Symbol> symbols) {
        this.templateParameters = templateParameters;
        this.templateConstraints = templateConstraints;
        members = symbols;

        foreach (var member in members)
            member.SetContainingType(this);

        _lazyMembers = null;
    }

    private ImmutableArray<MethodSymbol> GetConstructors() {
        var candidates = GetMembers(WellKnownMemberNames.InstanceConstructorName);

        if (candidates.IsEmpty)
            return [];

        var constructors = ArrayBuilder<MethodSymbol>.GetInstance();

        foreach (var candidate in candidates) {
            if (candidate is MethodSymbol method && candidate.containingType == this)
                constructors.Add(method);
        }

        return constructors.ToImmutableAndFree();
    }

    protected virtual void ConstructLazyMembers() {
        _lazyMembers = members.ToList();
    }

    private void ConstructLazyMembersDictionary() {
        if (_lazyMembers is null)
            ConstructLazyMembers();

        _lazyMembersDictionary = _lazyMembers.ToImmutableArray()
            .ToDictionary(m => m.name, StringOrdinalComparer.Instance);
    }
}
