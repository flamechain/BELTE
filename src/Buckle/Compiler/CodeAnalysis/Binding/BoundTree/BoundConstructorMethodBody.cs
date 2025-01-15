using System.Collections.Immutable;
using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Generated by the compiler, not present in the final tree.
/// </summary>
internal sealed class BoundConstructorMethodBody : BoundMethodBodyBase {
    internal BoundConstructorMethodBody(
        SyntaxNode syntax,
        ImmutableArray<DataContainerSymbol> locals,
        BoundStatement initializer,
        BoundBlockStatement body,
        bool hasErrors = false)
        : base(BoundKind.ConstructorMethodBody, syntax, body, hasErrors) {
        this.locals = locals;
        this.initializer = initializer;
    }

    internal ImmutableArray<DataContainerSymbol> locals { get; }

    internal BoundStatement initializer { get; }
}
