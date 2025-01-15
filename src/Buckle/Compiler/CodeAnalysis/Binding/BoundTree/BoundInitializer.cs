using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

internal abstract class BoundInitializer : BoundNode {
    internal BoundInitializer(BoundKind kind, SyntaxNode syntax, bool hasErrors) : base(kind, syntax, hasErrors) { }
}
