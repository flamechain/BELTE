using System;
using System.Collections.Generic;
using System.Linq;
using Buckle.CodeAnalysis.Binding;
using Buckle.CodeAnalysis.Symbols;
using Buckle.Libraries.Standard;
using Microsoft.CodeAnalysis.PooledObjects;
using static Buckle.CodeAnalysis.Binding.BoundFactory;

namespace Buckle.CodeAnalysis.Lowering;

/// <summary>
/// Lowers statements to be simpler and use less language features.
/// </summary>
internal sealed class Lowerer : BoundTreeRewriter {
    private readonly Expander _expander;
    private int _labelCount;

    private Lowerer(MethodSymbol container) {
        _expander = new Expander(container);
    }

    /// <summary>
    /// Lowers a <see cref="MethodSymbol" />.
    /// </summary>
    /// <param name="method">Method to lower.</param>
    /// <param name="statement">Method body.</param>
    /// <returns>Lowered method body (same type).</returns>
    internal static BoundBlockStatement Lower(MethodSymbol method, BoundStatement statement) {
        var lowerer = new Lowerer(method);

        var optimizedStatement = Optimizer.Optimize(statement, false);
        var expandedStatement = lowerer._expander.Expand(optimizedStatement);
        var rewrittenStatement = (BoundStatement)lowerer.Visit(expandedStatement);
        var block = Flatten(method, rewrittenStatement);
        var optimizedBlock = Optimizer.Optimize(block, true) as BoundBlockStatement;

        return optimizedBlock;
    }

    internal override BoundNode VisitIfStatement(BoundIfStatement statement) {
        /*

        if <condition>
            <then>

        ---->

        gotoFalse <condition> end
        <then>
        end:

        ==============================

        if <condition>
            <then>
        else
            <elseStatement>

        ---->

        gotoFalse <condition> else
        <then>
        goto end
        else:
        <elseStatement>
        end:

        */
        var syntax = statement.syntax;

        if (statement.alternative is null) {
            var endLabel = GenerateLabel();

            return VisitBlockStatement(
                Block(syntax,
                    GotoIfNot(syntax,
                        @goto: endLabel,
                        @ifNot: statement.condition
                    ),
                    statement.consequence,
                    Label(syntax, endLabel)
                )
            );
        } else {
            var elseLabel = GenerateLabel();
            var endLabel = GenerateLabel();

            return VisitBlockStatement(
                Block(syntax,
                    GotoIfNot(syntax,
                        @goto: elseLabel,
                        @ifNot: statement.condition
                    ),
                    statement.consequence,
                    Goto(syntax, endLabel),
                    Label(syntax, elseLabel),
                    statement.alternative,
                    Label(syntax, endLabel)
                )
            );
        }
    }

    internal override BoundNode VisitWhileStatement(BoundWhileStatement statement) {
        /*

        while <condition>
            <body>

        ---->

        continue:
        gotoFalse <condition> end
        <body>
        goto continue
        break:

        */
        var syntax = statement.syntax;
        var continueLabel = statement.continueLabel;
        var breakLabel = statement.breakLabel;

        return VisitBlockStatement(
            Block(syntax,
                Label(syntax, continueLabel),
                GotoIfNot(syntax,
                    @goto: breakLabel,
                    @ifNot: statement.condition
                ),
                statement.body,
                Goto(syntax, continueLabel),
                Label(syntax, breakLabel)
            )
        );
    }

    internal override BoundNode VisitDoWhileStatement(BoundDoWhileStatement statement) {
        /*

        do
            <body>
        while <condition>

        ---->

        continue:
        <body>
        gotoTrue <condition> continue
        break:

        */
        var syntax = statement.syntax;
        var continueLabel = statement.continueLabel;
        var breakLabel = statement.breakLabel;

        return VisitBlockStatement(
            Block(syntax,
                Label(syntax, continueLabel),
                statement.body,
                GotoIf(syntax,
                    @goto: continueLabel,
                    @if: statement.condition
                ),
                Label(syntax, breakLabel)
            )
        );
    }

    internal override BoundNode VisitForStatement(BoundForStatement statement) {
        /*

        for (<initializer> <condition>; <step>)
            <body>

        ---->

        {
            <initializer>
            while (<condition>) {
                <body>
            continue:
                <step>;
            }
        }

        */
        var syntax = statement.syntax;
        var continueLabel = statement.continueLabel;
        var breakLabel = statement.breakLabel;
        var condition = statement.condition.kind == BoundKind.EmptyExpression
            ? Literal(syntax, true)
            : statement.condition;

        return Visit(
            _expander.Expand(
                Block(syntax,
                    statement.initializer,
                    While(syntax,
                        condition,
                        Block(syntax,
                            statement.body,
                            Label(syntax, continueLabel),
                            Statement(syntax, statement.step)
                        ),
                        breakLabel,
                        GenerateLabel()
                    )
                )
            )
        );
    }

    internal override BoundNode VisitBinaryOperator(BoundBinaryOperator expression) {
        /*

        <left> <op> <right>

        ----> <op> is '**'

        (Math.Pow(<left>, <right>))

        ----> <left> is nullable and <right> is nullable

        ((HasValue(<left>) && HasValue(<right>)) ? Value(<left>) <op> Value(<right>) : null)

        ----> <left> is nullable

        (HasValue(<left>) ? Value(<left>) <op> <right> : null)

        ----> <right> is nullable

        (<right> isnt null ? <left> <op> Value(<right>) : null)

        */
        var syntax = expression.syntax;

        if (expression.opKind == BinaryOperatorKind.Power) {
            var powMethod = expression.left.type.IsNullableType() || expression.right.type.IsNullableType()
                ? StandardLibrary.Math.GetMembers()[46]
                : StandardLibrary.Math.GetMembers()[47];

            return VisitCallExpression(
                Call(syntax, powMethod as MethodSymbol, [expression.left, expression.right])
            );
        }

        if (expression.left.type.IsNullableType() &&
            expression.right.type.IsNullableType() &&
            expression.left.constantValue is null &&
            expression.right.constantValue is null) {
            return VisitConditionalOperator(
                Conditional(syntax,
                    @if: And(syntax,
                        HasValue(syntax, expression.left),
                        HasValue(syntax, expression.right)
                    ),
                    @then: Binary(syntax,
                        Value(syntax, expression.left, expression.left.type.GetNullableUnderlyingType()),
                        expression.opKind,
                        Value(syntax, expression.right, expression.right.type.GetNullableUnderlyingType()),
                        expression.type
                    ),
                    @else: Literal(syntax, null, expression.type),
                    expression.type
                )
            );
        }

        if (expression.left.type.IsNullableType() && expression.left.constantValue is null) {
            return VisitConditionalOperator(
                Conditional(syntax,
                    @if: HasValue(syntax, expression.left),
                    @then: Binary(syntax,
                        Value(syntax, expression.left, expression.left.type.GetNullableUnderlyingType()),
                        expression.opKind,
                        expression.right,
                        expression.type
                    ),
                    @else: Literal(syntax, null, expression.type),
                    expression.type
                )
            );
        }

        if (expression.right.type.IsNullableType() && expression.right.constantValue is null) {
            return VisitConditionalOperator(
                Conditional(syntax,
                    @if: HasValue(syntax, expression.right),
                    @then: Binary(syntax,
                        expression.left,
                        expression.opKind,
                        Value(syntax, expression.right, expression.right.type.GetNullableUnderlyingType()),
                        expression.type
                    ),
                    @else: Literal(syntax, null, expression.type),
                    expression.type
                )
            );
        }

        return base.VisitBinaryOperator(expression);
    }

    internal override BoundNode VisitNullCoalescingOperator(BoundNullCoalescingOperator expression) {
        /*

        <left> ?? <right>

        ---->

        (HasValue(<left>) ? Value(<left>) : <right>)

        */
        var syntax = expression.syntax;

        return VisitConditionalOperator(
            Conditional(syntax,
                @if: HasValue(syntax, expression.left),
                @then: Value(syntax, expression.left, expression.left.type),
                @else: expression.right,
                expression.type
            )
        );
    }

    internal override BoundNode VisitUnaryOperator(BoundUnaryOperator expression) {
        /*

        <op> <operand>

        ----> <op> is +

        <operand>

        ----> <operand> is nullable

        (HasValue(<operand>) ? <op> Value(<operand>) : null)

        ----> <op> is '++'

        (<operand> += 1)

        ----> <op> is '--'

        (<operand> -= 1)

        ----> <op> is '++' and <isOwnStatement>

        (<operand> += 1)

        ----> <op> is '++'

        ((<operand> += 1) - 1)

        ----> <op> is '--' and <isOwnStatement>

        (<operand> -= 1)

        ----> <op> is '--'

        ((<operand> -= 1) + 1)

        */
        var syntax = expression.syntax;

        if (expression.opKind == UnaryOperatorKind.UnaryPlus)
            return Visit(expression.operand);

        if (expression.opKind == UnaryOperatorKind.PrefixIncrement)
            return Visit(Increment(syntax, expression.operand));
        else if (expression.opKind == UnaryOperatorKind.PrefixDecrement)
            return Visit(Decrement(syntax, expression.operand));

        if (expression.opKind == UnaryOperatorKind.PostfixIncrement ||
            expression.opKind == UnaryOperatorKind.PostfixDecrement) {
            var assignment = expression.opKind == UnaryOperatorKind.PostfixIncrement
                ? Increment(syntax, expression.operand)
                : Decrement(syntax, expression.operand);

            // TODO Probably add a BoundIncrementExpression to handle isOwnStatement
            // Should ABSOLUTELY be using the expander for this kind of operation however, not undoing an increment
            // if (expression.isOwnStatement) {
            //     return VisitExpression(assignment);
            // } else {
            //     var reversal = expression.op.opKind == UnaryOperatorKind.PostfixIncrement
            //         ? Subtract(assignment, Literal(1))
            //         : Add(assignment, Literal(1));

            //     return VisitExpression(reversal);
            // }
            return Visit(assignment);
        }

        if (expression.operand.type.IsNullableType()) {
            return VisitConditionalOperator(
                Conditional(syntax,
                    @if: HasValue(syntax, expression.operand),
                    @then: Unary(syntax,
                        expression.opKind,
                        Value(syntax, expression.operand, expression.operand.type.GetNullableUnderlyingType()),
                        expression.type
                    ),
                    @else: Literal(syntax, null, expression.type),
                    expression.type
                )
            );
        }

        return base.VisitUnaryOperator(expression);
    }

    internal override BoundNode VisitCastExpression(BoundCastExpression expression) {
        /*

        (<type>)<expression>

        ----> <expression> is nullable and <type> is not nullable

        (<type>)Value(<expression>)

        ----> <expression> is nullable and <type> is not nullable and <expression>.type and <type> are otherwise equal

        Value(<expression>)

        ----> <expression> is not nullable and <type> is nullable and <expression>.type and <type> are otherwise equal

        <expression>

        */
        var syntax = expression.syntax;
        var operand = expression.operand;
        var type = expression.type;
        var operandType = operand.type;

        if (operandType.IsNullableType() && !type.IsNullableType()) {
            if (type.Equals(operandType))
                return Visit(Value(syntax, operand, operandType.GetNullableUnderlyingType()));

            return base.VisitCastExpression(
                Cast(syntax,
                    type,
                    Value(syntax, operand, operandType.GetNullableUnderlyingType()),
                    Conversion.ExplicitNullable,
                    operand.constantValue
                )
            );
        }

        if (operandType?.Equals(type) ?? false)
            return Visit(operand);

        return base.VisitCastExpression(expression);
    }

    internal override BoundNode VisitCallExpression(BoundCallExpression expression) {
        /*

        <method>(<parameters>)

        ---->

        (<method>(<parameters>))

        Now parameters do not have compiler generated '$' symbols in their name

        ----> <method> is 'Value' and <parameter> is not nullable

        <parameter>

        ----> <method> is 'HasValue' and <parameter> is not nullable

        true

        ----> is static access

        (<method>(<parameters>))

        Method operand rewritten to exclude TypeOf expression

        */
        var syntax = expression.syntax;
        var method = expression.method;
        // var parameters = ArrayBuilder<ParameterSymbol>.GetInstance();

        if (method.name == "Value" && !expression.arguments[0].type.IsNullableType())
            return Visit(expression.arguments[0]);
        else if (method.name == "HasValue" && !expression.arguments[0].type.IsNullableType())
            return Literal(syntax, true, expression.type);

        /*
        var parametersChanged = false;

        foreach (var oldParameter in method.parameters) {
            var name = oldParameter.name.StartsWith("$")
                ? oldParameter.name.Substring(1)
                : oldParameter.name;

            if (name == oldParameter.name) {
                parameters.Add(oldParameter);
                continue;
            }

            TODO Check if we even need this
            var parameter = new ParameterSymbol(
                name, oldParameter.type, oldParameter.ordinal, oldParameter.defaultValue
            );

            parametersChanged = true;
            parameters.Add(parameter);
        }
        */

        ArrayBuilder<BoundExpression> builder = null;

        for (var i = 0; i < expression.arguments.Length; i++) {
            var oldArgument = expression.arguments[i];
            var newArgument = (BoundExpression)Visit(oldArgument);

            if (newArgument != oldArgument) {
                if (builder is null) {
                    builder = ArrayBuilder<BoundExpression>.GetInstance(expression.arguments.Length);

                    for (var j = 0; j < i; j++)
                        builder.Add(expression.arguments[j]);
                }
            }

            builder?.Add(newArgument);
        }

        // var newMethod = parametersChanged
        //     ? method.UpdateParameters(parameters.ToImmutableAndFree())
        //     : method;

        var arguments = builder is null ? expression.arguments : builder.ToImmutableAndFree();

        return base.VisitCallExpression(
            new BoundCallExpression(
                syntax,
                expression.receiver,
                method,
                arguments,
                expression.argumentRefKinds,
                expression.defaultArguments,
                expression.resultKind,
                expression.type
            )
        );
    }

    internal override BoundNode VisitCompoundAssignmentOperator(BoundCompoundAssignmentOperator expression) {
        /*

        <left> <op>= <right>

        ---->

        (<left> = <left> <op> <right>)

        */
        var syntax = expression.syntax;

        return VisitAssignmentOperator(
            Assignment(syntax,
                expression.left,
                Binary(syntax,
                    expression.left,
                    expression.op.kind,
                    expression.right,
                    expression.type
                ),
                false,
                expression.type
            )
        );
    }

    private static BoundBlockStatement Flatten(MethodSymbol method, BoundStatement statement) {
        var syntax = statement.syntax;
        var statementsBuilder = ArrayBuilder<BoundStatement>.GetInstance();
        var localsBuilder = ArrayBuilder<DataContainerSymbol>.GetInstance();
        var functionsBuilder = ArrayBuilder<LocalFunctionSymbol>.GetInstance();

        var stack = new Stack<BoundStatement>();
        stack.Push(statement);

        while (stack.Count > 0) {
            var current = stack.Pop();

            if (current is BoundBlockStatement block) {
                localsBuilder.AddRange(block.locals);
                functionsBuilder.AddRange(block.localFunctions);

                foreach (var s in block.statements.Reverse())
                    stack.Push(s);
            } else {
                statementsBuilder.Add(current);
            }
        }

        if (method.returnsVoid) {
            if (statementsBuilder.Count == 0 || CanFallThrough(statementsBuilder.Last()))
                statementsBuilder.Add(new BoundReturnStatement(syntax, RefKind.None, null));
        }

        return new BoundBlockStatement(
            syntax,
            statementsBuilder.ToImmutableAndFree(),
            localsBuilder.ToImmutableAndFree(),
            functionsBuilder.ToImmutableAndFree()
        );
    }

    private static bool CanFallThrough(BoundStatement boundStatement) {
        return boundStatement.kind != BoundKind.ReturnStatement &&
            boundStatement.kind != BoundKind.GotoStatement;
    }

    private SynthesizedLabelSymbol GenerateLabel() {
        return new SynthesizedLabelSymbol($"Label{++_labelCount}");
    }
}
