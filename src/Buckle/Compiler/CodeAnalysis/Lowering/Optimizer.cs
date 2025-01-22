using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Buckle.CodeAnalysis.Binding;
using Buckle.CodeAnalysis.FlowAnalysis;
using Buckle.CodeAnalysis.Symbols;
using static Buckle.CodeAnalysis.Binding.BoundFactory;

namespace Buckle.CodeAnalysis.Lowering;

/// <summary>
/// Optimizes BoundExpressions and BoundStatements.
/// </summary>
internal sealed class Optimizer : BoundTreeRewriter {
    /// <summary>
    /// Optimizes a <see cref="BoundStatement" />.
    /// </summary>
    /// <param name="statement"><see cref="BoundStatement" /> to optimize.</param>
    /// <param name="removeDeadCode">If the compiler is transpiling skip this part of optimizing.</param>
    /// <returns>Optimized <param name="statement" />.</returns>
    internal static BoundStatement Optimize(BoundStatement statement, bool removeDeadCode) {
        var optimizer = new Optimizer();
        var optimizedStatement = optimizer.Visit(statement);

        if (statement is BoundBlockStatement && removeDeadCode)
            return RemoveDeadCode(optimizedStatement as BoundBlockStatement);
        else
            return (BoundStatement)optimizedStatement;
    }

    internal override BoundNode VisitConditionalGotoStatement(BoundConditionalGotoStatement statement) {
        /*

        goto <label> if <condition>

        ----> <condition> is constant true

        goto <label>

        ----> <condition> is constant false

        ;

        */
        if (ConstantValue.IsNotNull(statement.condition.constantValue)) {
            var condition = (bool)statement.condition.constantValue.value;
            condition = statement.jumpIfTrue ? condition : !condition;

            if (condition)
                return Visit(Goto(statement.syntax, statement.label));
            else
                return Visit(Nop());
        }

        return base.VisitConditionalGotoStatement(statement);
    }

    internal override BoundNode VisitConditionalOperator(BoundConditionalOperator expression) {
        /*

        <left> <op> <center> <op> <right>

        ----> <left> is constant true

        (<center>)

        ----> <left> is constant false

       (<right>)

        */
        if (ConstantValue.IsNotNull(expression.left.constantValue) && (bool)expression.left.constantValue.value)
            return Visit(expression.center);

        if (ConstantValue.IsNotNull(expression.left.constantValue) && !(bool)expression.left.constantValue.value)
            return Visit(expression.right);

        return base.VisitConditionalOperator(expression);
    }

    internal override BoundNode VisitAssignmentOperator(BoundAssignmentOperator expression) {
        /*

        <left> = <right>

        ----> <right> is ref <left>

        ;

        ----> <right> is the same as <left>

        ;

        */
        var left = expression.left;
        var right = expression.right is BoundReferenceExpression r ? r.expression : expression.right;
        // TODO Expand this to cover more cases
        var canSimplify = left is BoundDataContainerExpression ld &&
            right is BoundDataContainerExpression rd &&
            ld.dataContainer.Equals(rd.dataContainer);

        if (canSimplify)
            return new BoundEmptyExpression(expression.syntax, expression.type);

        return base.VisitAssignmentOperator(expression);
    }

    internal override BoundNode VisitArrayAccessExpression(BoundArrayAccessExpression expression) {
        /*

        <expression>[<index>]

        ----> <index> is constant, return item directly

        (<expression>[<index>])

        */
        if (expression.index.constantValue is null || expression.receiver is not BoundInitializerList i)
            return base.VisitArrayAccessExpression(expression);

        var index = (int)expression.index.constantValue.value;
        return Visit(i.items[index]);
    }

    internal override BoundNode VisitCallExpression(BoundCallExpression expression) {
        /*

        <method>(<arguments>)

        ----> <method> is Length and argument is a constant list

        <length of constant list>

        */
        var method = expression.method;
        var arguments = expression.arguments;

        if (method == BuiltinMethods.Length && arguments[0].constantValue is not null) {
            var constantList = arguments[0].constantValue.value as ImmutableArray<ConstantValue>?;

            if (constantList.HasValue) {
                return VisitLiteralExpression(
                    new BoundLiteralExpression(
                        expression.syntax,
                        new ConstantValue(constantList.Value.Length),
                        method.returnType
                    )
                );
            }
        }

        return base.VisitCallExpression(expression);
    }

    private static BoundBlockStatement RemoveDeadCode(BoundBlockStatement block) {
        var controlFlow = ControlFlowGraph.Create(block);
        var reachableStatements = new HashSet<BoundStatement>(controlFlow.blocks.SelectMany(b => b.statements));

        var builder = block.statements.ToBuilder();
        for (var i = builder.Count - 1; i >= 0; i--) {
            if (!reachableStatements.Contains(builder[i]))
                builder.RemoveAt(i);
        }

        return new BoundBlockStatement(block.syntax, builder.ToImmutable(), block.locals, block.localFunctions);
    }
}
