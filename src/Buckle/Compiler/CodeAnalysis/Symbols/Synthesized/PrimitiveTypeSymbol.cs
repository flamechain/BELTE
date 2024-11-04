using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using Buckle.CodeAnalysis.Binding;
using Buckle.CodeAnalysis.Syntax;
using Buckle.CodeAnalysis.Text;

namespace Buckle.CodeAnalysis.Symbols;

internal sealed class PrimitiveTypeSymbol : NamedTypeSymbol {
    internal PrimitiveTypeSymbol(string name, SpecialType specialType) {
        this.name = name;
        this.specialType = specialType;
    }

    public override string name { get; }

    public override ImmutableArray<TemplateParameterSymbol> templateParameters => [];

    public override ImmutableArray<TypeOrConstant> templateArguments => [];

    public override ImmutableArray<BoundExpression> templateConstraints => [];

    public override SpecialType specialType { get; }

    public override TypeKind typeKind => TypeKind.Primitive;

    public override int arity => 0;

    internal override bool mangleName => false;

    internal override Symbol containingSymbol => null;

    internal override NamedTypeSymbol constructedFrom => this;

    internal override Accessibility declaredAccessibility => Accessibility.Public;

    internal override bool isAbstract => false;

    internal override bool isStatic => false;

    internal override bool isSealed => false;

    internal override NamedTypeSymbol baseType => throw new InvalidOperationException();

    internal override SyntaxReference syntaxReference => throw new InvalidOperationException();

    internal override TextLocation location => null;

    internal override IEnumerable<string> memberNames => throw new InvalidOperationException();

    internal override bool isImplicitlyDeclared => true;

    internal override NamedTypeSymbol GetDeclaredBaseType(ConsList<TypeSymbol> basesBeingResolved) {
        throw new InvalidOperationException();
    }

    internal override ImmutableArray<Symbol> GetMembers() {
        throw new InvalidOperationException();
    }

    internal override ImmutableArray<Symbol> GetMembers(string name) {
        throw new InvalidOperationException();
    }

    internal override ImmutableArray<NamedTypeSymbol> GetTypeMembers() {
        throw new InvalidOperationException();
    }

    internal override ImmutableArray<NamedTypeSymbol> GetTypeMembers(ReadOnlyMemory<char> name) {
        throw new InvalidOperationException();
    }
}
