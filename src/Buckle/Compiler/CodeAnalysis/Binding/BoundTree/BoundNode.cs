using System.Diagnostics;
using Buckle.CodeAnalysis.Display;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound node, gets created from a <see cref="Syntax.SyntaxNode" />.
/// </summary>
[DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
internal abstract class BoundNode {
    internal abstract BoundNodeKind kind { get; }

    public override string ToString() {
        return DisplayText.DisplayNode(this).ToString();
    }

    private string GetDebuggerDisplay() {
        return ToString();
    }
}
