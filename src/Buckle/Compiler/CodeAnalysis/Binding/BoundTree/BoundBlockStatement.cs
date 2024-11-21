using System.Collections.Immutable;
using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Bound from a <see cref="Syntax.BlockStatementSyntax" />.
/// Contains the local scope.
/// </summary>
internal sealed class BoundBlockStatement : BoundStatement {
    internal BoundBlockStatement(
        ImmutableArray<BoundStatement> statements,
        ImmutableArray<DataContainerSymbol> locals,
        ImmutableArray<LocalFunctionSymbol> localFunctions) {
        this.statements = statements;
        this.locals = locals;
        this.localFunctions = localFunctions;
    }

    internal override BoundNodeKind kind => BoundNodeKind.BlockStatement;

    internal ImmutableArray<BoundStatement> statements { get; }

    internal ImmutableArray<DataContainerSymbol> locals { get; }

    internal ImmutableArray<LocalFunctionSymbol> localFunctions { get; }
}
