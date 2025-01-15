using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="NameSyntax" />.
/// </summary>
internal sealed class BoundTypeExpression : BoundExpression {
    internal BoundTypeExpression(
        SyntaxNode syntax,
        TypeSymbol type,
        TypeWithAnnotations typeWithAnnotations,
        bool hasErrors = false)
        : base(BoundKind.TypeExpression, syntax, type, hasErrors) {
        this.typeWithAnnotations = typeWithAnnotations;
    }

    internal TypeWithAnnotations typeWithAnnotations { get; }
}
