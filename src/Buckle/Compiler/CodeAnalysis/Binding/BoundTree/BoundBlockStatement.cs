using System.Collections.Immutable;
using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="BlockStatementSyntax" />.
/// Contains the local scope.
/// </summary>
internal sealed class BoundBlockStatement : BoundStatement {
    internal BoundBlockStatement(
        SyntaxNode syntax,
        ImmutableArray<BoundStatement> statements,
        ImmutableArray<DataContainerSymbol> locals,
        ImmutableArray<LocalFunctionSymbol> localFunctions,
        bool hasErrors = false)
        : base(BoundKind.BlockStatement, syntax, hasErrors) {
        this.statements = statements;
        this.locals = locals;
        this.localFunctions = localFunctions;
    }

    internal ImmutableArray<BoundStatement> statements { get; }

    internal ImmutableArray<DataContainerSymbol> locals { get; }

    internal ImmutableArray<LocalFunctionSymbol> localFunctions { get; }
}
