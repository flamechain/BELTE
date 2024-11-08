using System.Collections.Immutable;
using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// A bound block statement, bound from a <see cref="Syntax.BlockStatementSyntax" />.
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

    internal ImmutableArray<BoundStatement> statements { get; }

    internal ImmutableArray<DataContainerSymbol> locals { get; }

    internal ImmutableArray<LocalFunctionSymbol> localFunctions { get; }

    internal override BoundNodeKind kind => BoundNodeKind.BlockStatement;
}
