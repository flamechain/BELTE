using System.Collections.Generic;
using System.Linq;
using Buckle.CodeAnalysis.Binding;
using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;
using Buckle.Diagnostics;
using Buckle.Libraries;

namespace Buckle.CodeAnalysis.FlowAnalysis;

/// <summary>
/// Builds a <see cref="ControlFlowGraph" /> from BasicBlocks and BasicBlockBranches.
/// </summary>
internal sealed partial class ControlFlowGraphBuilder {
    private readonly Dictionary<BoundStatement, BasicBlock> _blockFromStatement = [];
    private readonly Dictionary<LabelSymbol, BasicBlock> _blockFromLabel = [];
    private readonly List<ControlFlowBranch> _branches = [];
    private readonly BasicBlock _start = new BasicBlock(true);
    private readonly BasicBlock _end = new BasicBlock(false);

    internal ControlFlowGraph Build(List<BasicBlock> blocks) {
        var basicBlockBuilder = new BasicBlockBuilder();

        if (blocks.Count == 0)
            Connect(_start, _end);
        else
            Connect(_start, blocks[0]);

        foreach (var block in blocks) {
            foreach (var statement in block.statements) {
                _blockFromStatement.Add(statement, block);

                if (statement is BoundLabelStatement labelStatement)
                    _blockFromLabel.Add(labelStatement.label, block);
            }
        }

        for (var i = 0; i < blocks.Count; i++) {
            var current = blocks[i];
            var next = i == blocks.Count - 1 ? _end : blocks[i + 1];

            foreach (var statement in current.statements) {
                var isLastStatement = statement == current.statements.Last();

                switch (statement.kind) {
                    case BoundKind.GotoStatement:
                        var gs = (BoundGotoStatement)statement;
                        var toBlock = _blockFromLabel[gs.label];

                        Connect(current, toBlock);

                        break;
                    case BoundKind.ConditionalGotoStatement:
                        var cgs = (BoundConditionalGotoStatement)statement;
                        var thenBlock = _blockFromLabel[cgs.label];
                        var elseBlock = next;
                        var negatedCondition = Negate(cgs.condition);
                        var thenCondition = cgs.jumpIfTrue ? cgs.condition : negatedCondition;
                        var elseCondition = cgs.jumpIfTrue ? negatedCondition : cgs.condition;

                        Connect(current, thenBlock, thenCondition);
                        Connect(current, elseBlock, elseCondition);

                        break;
                    case BoundKind.ReturnStatement:
                    case BoundKind.ExpressionStatement
                        when (statement as BoundExpressionStatement).expression is BoundThrowExpression:
                        Connect(current, _end);
                        break;
                    case BoundKind.NopStatement:
                    case BoundKind.ExpressionStatement:
                    case BoundKind.LocalDeclarationStatement:
                    case BoundKind.TryStatement:
                    case BoundKind.LabelStatement:
                        if (isLastStatement)
                            Connect(current, next);

                        break;
                    default:
                        throw new BelteInternalException($"Build: unexpected statement '{statement.kind}'");
                }
            }
        }

        void Scan() {
            foreach (var block in blocks) {
                if (block.incoming.Count == 0) {
                    RemoveBlock(blocks, block);
                    Scan();

                    return;
                }
            }
        }

        Scan();

        blocks.Insert(0, _start);
        blocks.Add(_end);

        return new ControlFlowGraph(_start, _end, blocks, _branches);
    }

    private void RemoveBlock(List<BasicBlock> blocks, BasicBlock block) {
        blocks.Remove(block);

        foreach (var branch in block.incoming) {
            branch.from.outgoing.Remove(branch);
            _branches.Remove(branch);
        }

        foreach (var branch in block.outgoing) {
            branch.to.incoming.Remove(branch);
            _branches.Remove(branch);
        }
    }

    private BoundExpression Negate(BoundExpression condition) {
        if (ConstantValue.IsNull(condition.constantValue))
            return condition;

        var boolType = CorLibrary.GetSpecialType(SpecialType.Bool);

        if (condition is BoundLiteralExpression literal) {
            var value = (bool)literal.value;

            return new BoundLiteralExpression(!value, boolType);
        }

        var opKind = UnaryOperatorEasyOut.OpKind(UnaryOperatorKind.LogicalNegation, boolType);

        return new BoundUnaryExpression(condition, opKind, boolType);
    }

    private void Connect(BasicBlock from, BasicBlock to, BoundExpression condition = null) {
        if (ConstantValue.IsNull(condition?.constantValue))
            return;

        if (condition is BoundLiteralExpression l) {
            var value = (bool)l.value;

            if (value)
                condition = null;
            else
                return;
        }

        var branch = new ControlFlowBranch(from, to, condition);

        from.outgoing.Add(branch);
        to.incoming.Add(branch);
        _branches.Add(branch);
    }
}
