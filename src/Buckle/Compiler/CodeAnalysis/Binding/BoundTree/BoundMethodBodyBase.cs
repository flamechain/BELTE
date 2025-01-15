using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

internal abstract class BoundMethodBodyBase : BoundNode {
    private protected BoundMethodBodyBase(BoundKind kind, SyntaxNode syntax, BoundBlockStatement body, bool hasErrors)
        : base(kind, syntax, hasErrors) {
        this.body = body;
    }

    internal BoundBlockStatement body { get; }
}
