using System.Collections.Immutable;
using Buckle.CodeAnalysis.Binding;
using Buckle.CodeAnalysis.Syntax;
using Buckle.CodeAnalysis.Text;

namespace Buckle.CodeAnalysis.Symbols;

internal sealed class SynthesizedEntryPoint : MethodSymbol {
    internal SynthesizedEntryPoint(
        Symbol containingSymbol,
        TypeWithAnnotations returnType,
        ImmutableArray<GlobalStatementSyntax> syntax) {
        this.containingSymbol = containingSymbol;
        returnTypeWithAnnotations = returnType;
    }

    public override string name => WellKnownMemberNames.EntryPointMethodName;

    public override ImmutableArray<TemplateParameterSymbol> templateParameters => [];

    public override ImmutableArray<BoundExpression> templateConstraints => [];

    public override ImmutableArray<TypeOrConstant> templateArguments => [];

    public override RefKind refKind => RefKind.None;

    public override bool returnsVoid => returnTypeWithAnnotations.IsVoidType();

    public override MethodKind methodKind => MethodKind.Ordinary;

    public override int arity => 0;

    internal override Symbol containingSymbol { get; }

    internal override TypeWithAnnotations returnTypeWithAnnotations { get; }

    internal override ImmutableArray<ParameterSymbol> parameters => [];

    internal override bool hidesBaseMethodsByName => false;

    internal override bool hasSpecialName => false;

    internal override bool isDeclaredConst => false;

    internal override Accessibility declaredAccessibility => Accessibility.Public;
    // TODO

    internal override bool isStatic => throw new System.NotImplementedException();

    internal override bool isVirtual => throw new System.NotImplementedException();

    internal override bool isAbstract => throw new System.NotImplementedException();

    internal override bool isOverride => throw new System.NotImplementedException();

    internal override bool isSealed => throw new System.NotImplementedException();

    internal override SyntaxReference syntaxReference => throw new System.NotImplementedException();

    internal override TextLocation location => throw new System.NotImplementedException();
}
