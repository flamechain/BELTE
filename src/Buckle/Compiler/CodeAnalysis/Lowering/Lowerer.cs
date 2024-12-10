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
        var rewrittenStatement = lowerer.RewriteStatement(expandedStatement);
        var block = Flatten(method, rewrittenStatement);
        var optimizedBlock = Optimizer.Optimize(block, true) as BoundBlockStatement;

        return optimizedBlock;
    }

    private protected override BoundStatement RewriteIfStatement(BoundIfStatement statement) {
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
        if (statement.elseStatement is null) {
            var endLabel = GenerateLabel();

            return RewriteStatement(
                Block(
                    GotoIfNot(
                        @goto: endLabel,
                        @ifNot: statement.condition
                    ),
                    statement.then,
                    Label(endLabel)
                )
            );
        } else {
            var elseLabel = GenerateLabel();
            var endLabel = GenerateLabel();

            return RewriteStatement(
                Block(
                    GotoIfNot(
                        @goto: elseLabel,
                        @ifNot: statement.condition
                    ),
                    statement.then,
                    Goto(endLabel),
                    Label(elseLabel),
                    statement.elseStatement,
                    Label(endLabel)
                )
            );
        }
    }

    private protected override BoundStatement RewriteWhileStatement(BoundWhileStatement statement) {
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
        var continueLabel = statement.continueLabel;
        var breakLabel = statement.breakLabel;

        return RewriteStatement(
            Block(
                Label(continueLabel),
                GotoIfNot(
                    @goto: breakLabel,
                    @ifNot: statement.condition
                ),
                statement.body,
                Goto(continueLabel),
                Label(breakLabel)
            )
        );
    }

    private protected override BoundStatement RewriteDoWhileStatement(BoundDoWhileStatement statement) {
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
        var continueLabel = statement.continueLabel;
        var breakLabel = statement.breakLabel;

        return RewriteStatement(
            Block(
                Label(continueLabel),
                statement.body,
                GotoIf(
                    @goto: continueLabel,
                    @if: statement.condition
                ),
                Label(breakLabel)
            )
        );
    }

    private protected override BoundStatement RewriteForStatement(BoundForStatement statement) {
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
        var continueLabel = statement.continueLabel;
        var breakLabel = statement.breakLabel;
        var condition = statement.condition.kind == BoundNodeKind.EmptyExpression
            ? Literal(true)
            : statement.condition;

        return RewriteStatement(
            _expander.Expand(
                Block(
                    statement.initializer,
                    While(
                        condition,
                        Block(
                            statement.body,
                            Label(continueLabel),
                            Statement(statement.step)
                        ),
                        breakLabel,
                        GenerateLabel()
                    )
                )
            )
        );
    }

    private protected override BoundExpression RewriteBinaryExpression(BoundBinaryExpression expression) {
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
        if (expression.op.opKind == BoundBinaryOperatorKind.Power) {
            var powMethod = expression.left.type.IsNullableType() || expression.right.type.IsNullableType()
                ? StandardLibrary.Math.GetMembers()[46]
                : StandardLibrary.Math.GetMembers()[47];

            return RewriteExpression(
                Call(powMethod as MethodSymbol, [expression.left, expression.right])
            );
        }

        if (expression.left.type.IsNullableType() &&
            expression.right.type.IsNullableType() &&
            expression.left.constantValue is null &&
            expression.right.constantValue is null) {
            return RewriteExpression(
                NullConditional(
                    @if: And(
                        HasValue(expression.left),
                        HasValue(expression.right)
                    ),
                    @then: Binary(
                        Value(expression.left),
                        expression.op,
                        Value(expression.right)
                    ),
                    @else: Literal(null, expression.type)
                )
            );
        }

        if (expression.left.type.IsNullableType() && expression.left.constantValue is null) {
            return RewriteExpression(
                NullConditional(
                    @if: HasValue(expression.left),
                    @then: Binary(
                        Value(expression.left),
                        expression.op,
                        expression.right
                    ),
                    @else: Literal(null, expression.type)
                )
            );
        }

        if (expression.right.type.IsNullableType() && expression.right.constantValue is null) {
            return RewriteExpression(
                NullConditional(
                    @if: HasValue(expression.right),
                    @then: Binary(
                        expression.left,
                        expression.op,
                        Value(expression.right)
                    ),
                    @else: Literal(null, expression.type)
                )
            );
        }

        return base.RewriteBinaryExpression(expression);
    }

    private protected override BoundExpression RewriteNullCoalescingExpression(
        BoundNullCoalescingExpression expression) {
        /*

        <left> ?? <right>

        ---->

        (HasValue(<left>) ? Value(<left>) : <right>)

        */
        return RewriteExpression(
            NullConditional(
                @if: HasValue(expression.left),
                @then: Value(expression.left),
                @else: expression.right
            )
        );
    }

    private protected override BoundExpression RewriteUnaryExpression(BoundUnaryExpression expression) {
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
        if (expression.op.opKind == BoundUnaryOperatorKind.NumericalIdentity)
            return RewriteExpression(expression.operand);

        if (expression.op.opKind == BoundPrefixOperatorKind.Increment)
            return RewriteExpression(Increment(expression.operand));
        else if (expression.op.opKind == BoundPrefixOperatorKind.Decrement)
            return RewriteExpression(Decrement(expression.operand));

        if (expression.op.opKind == BoundPostfixOperatorKind.Increment ||
            expression.op.opKind == BoundPostfixOperatorKind.Decrement) {
            var assignment = expression.op.opKind == BoundPostfixOperatorKind.Increment
                ? Increment(expression.operand)
                : Decrement(expression.operand);

            if (expression.isOwnStatement) {
                return RewriteExpression(assignment);
            } else {
                var reversal = expression.op.opKind == BoundPostfixOperatorKind.Increment
                    ? Subtract(assignment, Literal(1))
                    : Add(assignment, Literal(1));

                return RewriteExpression(reversal);
            }
        }

        if (expression.operand.type.IsNullableType()) {
            return RewriteExpression(
                NullConditional(
                    @if: HasValue(expression.operand),
                    @then: Unary(
                        expression.op,
                        Value(expression.operand)
                    ),
                    @else: Literal(null, expression.type)
                )
            );
        }

        return base.RewriteUnaryExpression(expression);
    }

    private protected override BoundExpression RewriteCastExpression(BoundCastExpression expression) {
        /*

        (<type>)<expression>

        ----> <expression> is nullable and <type> is not nullable

        (<type>)Value(<expression>)

        ----> <expression> is nullable and <type> is not nullable and <expression>.type and <type> are otherwise equal

        Value(<expression>)

        ----> <expression> is not nullable and <type> is nullable and <expression>.type and <type> are otherwise equal

        <expression>

        */
        var operand = expression.operand;
        var type = expression.type;
        var operandType = operand.type;

        if (operandType.IsNullableType() && !type.IsNullableType()) {
            if (type.Equals(operandType))
                return RewriteExpression(Value(operand));

            return base.RewriteCastExpression(
                Cast(
                    type,
                    Value(operand),
                    ConversionKind.ExplicitNullable,
                    operand.constantValue
                )
            );
        }

        if (operandType.Equals(type))
            return RewriteExpression(operand);

        return base.RewriteCastExpression(expression);
    }

    private protected override BoundExpression RewriteCallExpression(BoundCallExpression expression) {
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
        var method = expression.method;
        // var parameters = ArrayBuilder<ParameterSymbol>.GetInstance();

        if (method.name == "Value" && !expression.arguments[0].type.IsNullableType())
            return RewriteExpression(expression.arguments[0]);
        else if (method.name == "HasValue" && !expression.arguments[0].type.IsNullableType())
            return new BoundLiteralExpression(true, expression.type);

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
            var newArgument = RewriteExpression(oldArgument);

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

        return base.RewriteCallExpression(new BoundCallExpression(expression.expression, method, arguments));
    }

    private protected override BoundExpression RewriteCompoundAssignmentExpression(
        BoundCompoundAssignmentExpression expression) {
        /*

        <left> <op>= <right>

        ---->

        (<left> = <left> <op> <right>)

        */
        return RewriteExpression(
            Assignment(
                expression.left,
                Binary(
                    expression.left,
                    expression.op,
                    expression.right
                )
            )
        );
    }

    private static BoundBlockStatement Flatten(MethodSymbol method, BoundStatement statement) {
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
                statementsBuilder.Add(new BoundReturnStatement(null));
        }

        return new BoundBlockStatement(
            statementsBuilder.ToImmutableAndFree(),
            localsBuilder.ToImmutableAndFree(),
            functionsBuilder.ToImmutableAndFree()
        );
    }

    private static bool CanFallThrough(BoundStatement boundStatement) {
        return boundStatement.kind != BoundNodeKind.ReturnStatement &&
            boundStatement.kind != BoundNodeKind.GotoStatement;
    }

    private SynthesizedLabelSymbol GenerateLabel() {
        return new SynthesizedLabelSymbol($"Label{++_labelCount}");
    }
}
