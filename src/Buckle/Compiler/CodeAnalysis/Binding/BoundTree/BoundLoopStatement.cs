using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// A base type for bound loop types (<see cref="BoundForStatement" />, <see cref="BoundWhileStatement" />,
/// <see cref="BoundDoWhileStatement" />).
/// Uses labels for gotos as the <see cref="Lowering.Lowerer" /> rewrites all control of flow to gotos.
/// </summary>
internal abstract class BoundLoopStatement : BoundStatement {
    private protected BoundLoopStatement(SynthesizedLabelSymbol breakLabel, SynthesizedLabelSymbol continueLabel) {
        this.breakLabel = breakLabel;
        this.continueLabel = continueLabel;
    }

    internal SynthesizedLabelSymbol breakLabel { get; }

    internal SynthesizedLabelSymbol continueLabel { get; }
}
