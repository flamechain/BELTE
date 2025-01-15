using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

internal class BoundValuePlaceholder : BoundExpression {
    internal BoundValuePlaceholder(SyntaxNode syntax, TypeSymbol type, bool hasErrors = false)
        : base(BoundKind.ValuePlaceholder, syntax, type, hasErrors) { }
}
