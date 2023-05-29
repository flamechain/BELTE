
namespace Buckle.CodeAnalysis.Text;

/// <summary>
/// Describes a single change when a particular span is replaced with new text.
/// </summary>
public readonly struct TextChange {
    /// <summary>
    /// Creates a new instance of <see cref="TextChange" />.
    /// </summary>
    /// <param name="span">The original span of the changed text.</param>
    /// <param name="newText">The new text.</param>
    public TextChange(TextSpan span, string newText) {
        this.span = span;
        this.newText = newText;
    }

    /// <summary>
    /// The original span of the changed text.
    /// </summary>
    public TextSpan span { get; }

    /// <summary>
    /// The new text.
    /// </summary>
    public string newText { get; }
}
