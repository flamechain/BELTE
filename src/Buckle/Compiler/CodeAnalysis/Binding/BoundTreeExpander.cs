using System.Collections.Generic;
using System.Collections.Immutable;
using Buckle.Diagnostics;
using Microsoft.CodeAnalysis.PooledObjects;
using static Buckle.CodeAnalysis.Binding.BoundFactory;

namespace Buckle.CodeAnalysis.Binding;

/// <summary>
/// Rewrites BoundStatements and all child BoundStatements, allowing expansion.
/// </summary>
internal abstract class BoundTreeExpander {
    private protected static BoundStatement Simplify(List<BoundStatement> statements) {
        if (statements.Count == 1)
            return statements[0];
        else
            return Block(statements.ToArray());
    }

    private protected virtual List<BoundStatement> ExpandStatement(BoundStatement statement) {
        return statement.kind switch {
            BoundNodeKind.NopStatement => ExpandNopStatement((BoundNopStatement)statement),
            BoundNodeKind.BlockStatement => ExpandBlockStatement((BoundBlockStatement)statement),
            BoundNodeKind.LocalDeclarationStatement
                => ExpandLocalDeclarationStatement((BoundLocalDeclarationStatement)statement),
            BoundNodeKind.IfStatement => ExpandIfStatement((BoundIfStatement)statement),
            BoundNodeKind.WhileStatement => ExpandWhileStatement((BoundWhileStatement)statement),
            BoundNodeKind.ForStatement => ExpandForStatement((BoundForStatement)statement),
            BoundNodeKind.ExpressionStatement => ExpandExpressionStatement((BoundExpressionStatement)statement),
            BoundNodeKind.LabelStatement => ExpandLabelStatement((BoundLabelStatement)statement),
            BoundNodeKind.GotoStatement => ExpandGotoStatement((BoundGotoStatement)statement),
            BoundNodeKind.ConditionalGotoStatement
                => ExpandConditionalGotoStatement((BoundConditionalGotoStatement)statement),
            BoundNodeKind.DoWhileStatement => ExpandDoWhileStatement((BoundDoWhileStatement)statement),
            BoundNodeKind.ReturnStatement => ExpandReturnStatement((BoundReturnStatement)statement),
            BoundNodeKind.TryStatement => ExpandTryStatement((BoundTryStatement)statement),
            BoundNodeKind.BreakStatement => ExpandBreakStatement((BoundBreakStatement)statement),
            BoundNodeKind.ContinueStatement => ExpandContinueStatement((BoundContinueStatement)statement),
            _ => throw new BelteInternalException($"ExpandStatement: unexpected expression type '{statement.kind}'"),
        };
    }

    private protected virtual List<BoundStatement> ExpandNopStatement(BoundNopStatement statement) {
        return [statement];
    }

    private protected virtual List<BoundStatement> ExpandBlockStatement(BoundBlockStatement statement) {
        var statements = new List<BoundStatement>();

        foreach (var childStatement in statement.statements)
            statements.AddRange(ExpandStatement(childStatement));

        return new List<BoundStatement>() { Block(statements.ToArray()) };
    }

    private protected virtual List<BoundStatement> ExpandLocalDeclarationStatement(
        BoundLocalDeclarationStatement statement) {
        var statements = ExpandExpression(statement.declaration.initializer, out var replacement);

        if (statements.Count > 0) {
            statements.Add(new BoundLocalDeclarationStatement(
                new BoundDataContainerDeclaration(statement.declaration.dataContainer, replacement)
            ));

            return statements;
        }

        return [statement];
    }

    private protected virtual List<BoundStatement> ExpandIfStatement(BoundIfStatement statement) {
        var statements = ExpandExpression(statement.condition, out var conditionReplacement);

        statements.Add(
            new BoundIfStatement(
                conditionReplacement,
                Simplify(ExpandStatement(statement.then)),
                statement.elseStatement is not null ? Simplify(ExpandStatement(statement.elseStatement)) : null
            )
        );

        return statements;
    }

    private protected virtual List<BoundStatement> ExpandWhileStatement(BoundWhileStatement statement) {
        var statements = ExpandExpression(statement.condition, out var conditionReplacement);

        statements.Add(
            new BoundWhileStatement(
                conditionReplacement,
                Simplify(ExpandStatement(statement.body)),
                statement.breakLabel,
                statement.continueLabel
            )
        );

        return statements;
    }

    private protected virtual List<BoundStatement> ExpandForStatement(BoundForStatement statement) {
        // For loops have to be expanded after they have been lowered
        return [statement];
    }

    private protected virtual List<BoundStatement> ExpandExpressionStatement(BoundExpressionStatement statement) {
        var statements = ExpandExpression(statement.expression, out var replacement);

        if (statements.Count != 0) {
            statements.Add(new BoundExpressionStatement(replacement));
            return statements;
        }

        return [statement];
    }

    private protected virtual List<BoundStatement> ExpandLabelStatement(BoundLabelStatement statement) {
        return [statement];
    }

    private protected virtual List<BoundStatement> ExpandGotoStatement(BoundGotoStatement statement) {
        return [statement];
    }

    private protected virtual List<BoundStatement> ExpandConditionalGotoStatement(BoundConditionalGotoStatement statement) {
        return [statement];
    }

    private protected virtual List<BoundStatement> ExpandDoWhileStatement(BoundDoWhileStatement statement) {
        var statements = ExpandExpression(statement.condition, out var conditionReplacement);

        statements.Add(
            new BoundDoWhileStatement(
                Simplify(ExpandStatement(statement.body)),
                conditionReplacement,
                statement.breakLabel,
                statement.continueLabel
            )
        );

        return statements;
    }

    private protected virtual List<BoundStatement> ExpandReturnStatement(BoundReturnStatement statement) {
        if (statement.expression is null)
            return [statement];

        var statements = ExpandExpression(statement.expression, out var replacement);

        if (statements.Count != 0) {
            statements.Add(new BoundReturnStatement(statement.refKind, replacement));
            return statements;
        }

        return [statement];
    }

    private protected virtual List<BoundStatement> ExpandTryStatement(BoundTryStatement statement) {
        return new List<BoundStatement>() {
            new BoundTryStatement(
                Simplify(ExpandStatement(statement.body)) as BoundBlockStatement,
                statement.catchBody is not null ?
                    Simplify(ExpandStatement(statement.catchBody)) as BoundBlockStatement
                    : null,
                statement.finallyBody is not null ?
                    Simplify(ExpandStatement(statement.finallyBody)) as BoundBlockStatement
                    : null
            )
        };
    }

    private protected virtual List<BoundStatement> ExpandBreakStatement(BoundBreakStatement statement) {
        return [statement];
    }

    private protected virtual List<BoundStatement> ExpandContinueStatement(BoundContinueStatement statement) {
        return [statement];
    }

    private protected virtual List<BoundStatement> ExpandExpression(
        BoundExpression expression,
        out BoundExpression replacement) {
        switch (expression.kind) {
            case BoundNodeKind.LiteralExpression:
                return ExpandLiteralExpression((BoundLiteralExpression)expression, out replacement);
            case BoundNodeKind.InitializerListExpression:
                return ExpandInitializerListExpression((BoundInitializerListExpression)expression, out replacement);
            case BoundNodeKind.InitializerDictionaryExpression:
                return ExpandInitializerDictionaryExpression(
                    (BoundInitializerDictionaryExpression)expression, out replacement
                );
            case BoundNodeKind.DataContainerExpression:
                return ExpandDataContainerExpression((BoundDataContainerExpression)expression, out replacement);
            case BoundNodeKind.AssignmentExpression:
                return ExpandAssignmentExpression((BoundAssignmentExpression)expression, out replacement);
            case BoundNodeKind.UnaryExpression:
                return ExpandUnaryExpression((BoundUnaryExpression)expression, out replacement);
            case BoundNodeKind.BinaryExpression:
                return ExpandBinaryExpression((BoundBinaryExpression)expression, out replacement);
            case BoundNodeKind.AsExpression:
                return ExpandAsExpression((BoundAsExpression)expression, out replacement);
            case BoundNodeKind.IsExpression:
                return ExpandIsExpression((BoundIsExpression)expression, out replacement);
            case BoundNodeKind.IsntExpression:
                return ExpandIsntExpression((BoundIsntExpression)expression, out replacement);
            case BoundNodeKind.NullCoalescingExpression:
                return ExpandNullCoalescingExpression((BoundNullCoalescingExpression)expression, out replacement);
            case BoundNodeKind.NullAssertExpression:
                return ExpandNullAssertExpression((BoundNullAssertExpression)expression, out replacement);
            case BoundNodeKind.EmptyExpression:
                return ExpandEmptyExpression((BoundEmptyExpression)expression, out replacement);
            case BoundNodeKind.ErrorExpression:
                return ExpandErrorExpression((BoundErrorExpression)expression, out replacement);
            case BoundNodeKind.CallExpression:
                return ExpandCallExpression((BoundCallExpression)expression, out replacement);
            case BoundNodeKind.CastExpression:
                return ExpandCastExpression((BoundCastExpression)expression, out replacement);
            case BoundNodeKind.ArrayAccessExpression:
                return ExpandArrayAccessExpression((BoundArrayAccessExpression)expression, out replacement);
            case BoundNodeKind.CompoundAssignmentExpression:
                return ExpandCompoundAssignmentExpression(
                    (BoundCompoundAssignmentExpression)expression, out replacement
                );
            case BoundNodeKind.ReferenceExpression:
                return ExpandReferenceExpression((BoundReferenceExpression)expression, out replacement);
            case BoundNodeKind.TypeOfExpression:
                return ExpandTypeOfExpression((BoundTypeOfExpression)expression, out replacement);
            case BoundNodeKind.ConditionalExpression:
                return ExpandConditionalExpression((BoundConditionalExpression)expression, out replacement);
            case BoundNodeKind.ObjectCreationExpression:
                return ExpandObjectCreationExpression((BoundObjectCreationExpression)expression, out replacement);
            case BoundNodeKind.ArrayCreationExpression:
                return ExpandArrayCreationExpression((BoundArrayCreationExpression)expression, out replacement);
            case BoundNodeKind.FieldAccessExpression:
                return ExpandFieldAccessExpression((BoundFieldAccessExpression)expression, out replacement);
            case BoundNodeKind.ConditionalAccessExpression:
                return ExpandConditionalAccessExpression((BoundConditionalAccessExpression)expression, out replacement);
            case BoundNodeKind.ThisExpression:
                return ExpandThisExpression((BoundThisExpression)expression, out replacement);
            case BoundNodeKind.BaseExpression:
                return ExpandBaseExpression((BoundBaseExpression)expression, out replacement);
            case BoundNodeKind.ThrowExpression:
                return ExpandThrowExpression((BoundThrowExpression)expression, out replacement);
            case BoundNodeKind.TypeExpression:
                return ExpandTypeExpression((BoundTypeExpression)expression, out replacement);
            case BoundNodeKind.ParameterExpression:
                return ExpandParameterExpression((BoundParameterExpression)expression, out replacement);
            default:
                throw new BelteInternalException($"ExpandExpression: unexpected expression type '{expression.kind}'");
        }
    }

    private protected virtual List<BoundStatement> ExpandParameterExpression(
        BoundParameterExpression expression,
        out BoundExpression replacement) {
        replacement = expression;
        return [];
    }

    private protected virtual List<BoundStatement> ExpandTypeExpression(
        BoundTypeExpression expression,
        out BoundExpression replacement) {
        replacement = expression;
        return [];
    }

    private protected virtual List<BoundStatement> ExpandThisExpression(
        BoundThisExpression expression,
        out BoundExpression replacement) {
        replacement = expression;
        return [];
    }

    private protected virtual List<BoundStatement> ExpandBaseExpression(
        BoundBaseExpression expression,
        out BoundExpression replacement) {
        replacement = expression;
        return [];
    }

    private protected virtual List<BoundStatement> ExpandThrowExpression(
        BoundThrowExpression expression,
        out BoundExpression replacement) {
        replacement = expression;
        return [];
    }

    private protected virtual List<BoundStatement> ExpandBinaryExpression(
        BoundBinaryExpression expression,
        out BoundExpression replacement) {
        var statements = ExpandExpression(expression.left, out var leftReplacement);
        statements.AddRange(ExpandExpression(expression.right, out var rightReplacement));

        if (statements.Count != 0) {
            replacement = new BoundBinaryExpression(
                leftReplacement,
                rightReplacement,
                expression.opKind,
                expression.type,
                expression.constantValue
            );

            return statements;
        }

        replacement = expression;
        return [];
    }

    private protected virtual List<BoundStatement> ExpandAsExpression(
        BoundAsExpression expression,
        out BoundExpression replacement) {
        var statements = ExpandExpression(expression.left, out var leftReplacement);
        statements.AddRange(ExpandExpression(expression.right, out var rightReplacement));

        if (statements.Count != 0) {
            replacement = new BoundAsExpression(leftReplacement, rightReplacement, expression.type);
            return statements;
        }

        replacement = expression;
        return [];
    }

    private protected virtual List<BoundStatement> ExpandIsExpression(
        BoundIsExpression expression,
        out BoundExpression replacement) {
        var statements = ExpandExpression(expression.left, out var leftReplacement);
        statements.AddRange(ExpandExpression(expression.right, out var rightReplacement));

        if (statements.Count != 0) {
            replacement = new BoundIsExpression(leftReplacement, rightReplacement, expression.type);
            return statements;
        }

        replacement = expression;
        return [];
    }

    private protected virtual List<BoundStatement> ExpandIsntExpression(
        BoundIsntExpression expression,
        out BoundExpression replacement) {
        var statements = ExpandExpression(expression.left, out var leftReplacement);
        statements.AddRange(ExpandExpression(expression.right, out var rightReplacement));

        if (statements.Count != 0) {
            replacement = new BoundIsntExpression(leftReplacement, rightReplacement, expression.type);
            return statements;
        }

        replacement = expression;
        return [];
    }

    private protected virtual List<BoundStatement> ExpandNullCoalescingExpression(
        BoundNullCoalescingExpression expression,
        out BoundExpression replacement) {
        var statements = ExpandExpression(expression.left, out var leftReplacement);
        statements.AddRange(ExpandExpression(expression.right, out var rightReplacement));

        if (statements.Count != 0) {
            replacement = new BoundNullCoalescingExpression(leftReplacement, rightReplacement, expression.type);
            return statements;
        }

        replacement = expression;
        return [];
    }

    private protected virtual List<BoundStatement> ExpandInitializerListExpression(
        BoundInitializerListExpression expression,
        out BoundExpression replacement) {
        var statements = new List<BoundStatement>();
        var replacementItems = ArrayBuilder<BoundExpression>.GetInstance();

        foreach (var item in expression.items) {
            statements.AddRange(ExpandExpression(item, out var itemReplacement));
            replacementItems.Add(itemReplacement);
        }

        if (statements.Count != 0) {
            replacement = new BoundInitializerListExpression(replacementItems.ToImmutableAndFree(), expression.type);
            return statements;
        }

        replacement = expression;
        return [];
    }

    private protected virtual List<BoundStatement> ExpandInitializerDictionaryExpression(
        BoundInitializerDictionaryExpression expression,
        out BoundExpression replacement) {
        replacement = expression;
        return [];
    }

    private protected virtual List<BoundStatement> ExpandLiteralExpression(
        BoundLiteralExpression expression,
        out BoundExpression replacement) {
        replacement = expression;
        return [];
    }

    private protected virtual List<BoundStatement> ExpandDataContainerExpression(
        BoundDataContainerExpression expression,
        out BoundExpression replacement) {
        replacement = expression;
        return [];
    }

    private protected virtual List<BoundStatement> ExpandAssignmentExpression(
        BoundAssignmentExpression expression,
        out BoundExpression replacement) {
        var statements = ExpandExpression(expression.left, out var leftReplacement);
        statements.AddRange(ExpandExpression(expression.right, out var rightReplacement));

        if (statements.Count != 0) {
            replacement = new BoundAssignmentExpression(
                leftReplacement,
                rightReplacement,
                expression.isRef,
                expression.type
            );

            return statements;
        }

        replacement = expression;
        return [];
    }

    private protected virtual List<BoundStatement> ExpandUnaryExpression(
        BoundUnaryExpression expression,
        out BoundExpression replacement) {
        var statements = ExpandExpression(expression.operand, out var operandReplacement);

        if (statements.Count != 0) {
            replacement = new BoundUnaryExpression(operandReplacement, expression.opKind, expression.type);
            return statements;
        }

        replacement = expression;
        return [];
    }

    private protected virtual List<BoundStatement> ExpandNullAssertExpression(
        BoundNullAssertExpression expression,
        out BoundExpression replacement) {
        var statements = ExpandExpression(expression.operand, out var operandReplacement);

        if (statements.Count != 0) {
            replacement = new BoundNullAssertExpression(operandReplacement, expression.type);
            return statements;
        }

        replacement = expression;
        return [];
    }

    private protected virtual List<BoundStatement> ExpandEmptyExpression(
        BoundEmptyExpression expression,
        out BoundExpression replacement) {
        replacement = expression;
        return [];
    }

    private protected virtual List<BoundStatement> ExpandErrorExpression(
        BoundErrorExpression expression,
        out BoundExpression replacement) {
        replacement = expression;
        return [];
    }

    private protected virtual List<BoundStatement> ExpandCallExpression(
        BoundCallExpression expression,
        out BoundExpression replacement) {
        var statements = ExpandExpression(expression.expression, out var expressionReplacement);
        statements.AddRange(ExpandArguments(expression.arguments, out var argumentsReplacement));

        replacement = new BoundCallExpression(
            expressionReplacement,
            expression.method,
            argumentsReplacement
        );

        return statements;
    }

    private List<BoundStatement> ExpandArguments(
        ImmutableArray<BoundExpression> arguments,
        out ImmutableArray<BoundExpression> replacement) {
        var statements = new List<BoundStatement>();
        var replacementArguments = ArrayBuilder<BoundExpression>.GetInstance();

        foreach (var argument in arguments) {
            statements.AddRange(ExpandExpression(argument, out var argumentReplacement));
            replacementArguments.Add(argumentReplacement);
        }

        replacement = replacementArguments.ToImmutableAndFree();
        return statements;
    }

    private protected virtual List<BoundStatement> ExpandCastExpression(
        BoundCastExpression expression,
        out BoundExpression replacement) {
        var statements = ExpandExpression(expression.operand, out var expressionReplacement);

        if (statements.Count != 0) {
            replacement = new BoundCastExpression(
                expression.type,
                expressionReplacement,
                expression.conversion,
                expression.constantValue
            );

            return statements;
        }

        replacement = expression;
        return [];
    }

    private protected virtual List<BoundStatement> ExpandArrayAccessExpression(
        BoundArrayAccessExpression expression,
        out BoundExpression replacement) {
        var statements = ExpandExpression(expression.receiver, out var operandReplacement);
        statements.AddRange(ExpandExpression(expression.index, out var indexReplacement));

        if (statements.Count != 0) {
            replacement = new BoundArrayAccessExpression(operandReplacement, indexReplacement, expression.type);
            return statements;
        }

        replacement = expression;
        return [];
    }

    private protected virtual List<BoundStatement> ExpandCompoundAssignmentExpression(
        BoundCompoundAssignmentExpression expression,
        out BoundExpression replacement) {
        var statements = ExpandExpression(expression.left, out var leftReplacement);
        statements.AddRange(ExpandExpression(expression.right, out var rightReplacement));

        if (statements.Count != 0) {
            replacement = new BoundCompoundAssignmentExpression(
                leftReplacement,
                rightReplacement,
                expression.opKind,
                expression.type
            );

            return statements;
        }

        replacement = expression;
        return [];
    }

    private protected virtual List<BoundStatement> ExpandReferenceExpression(
        BoundReferenceExpression expression,
        out BoundExpression replacement) {
        replacement = expression;
        return [];
    }

    private protected virtual List<BoundStatement> ExpandTypeOfExpression(
        BoundTypeOfExpression expression,
        out BoundExpression replacement) {
        replacement = expression;
        return [];
    }

    private protected virtual List<BoundStatement> ExpandConditionalExpression(
        BoundConditionalExpression expression,
        out BoundExpression replacement) {
        var statements = ExpandExpression(expression.left, out var leftReplacement);
        statements.AddRange(ExpandExpression(expression.center, out var centerReplacement));
        statements.AddRange(ExpandExpression(expression.right, out var rightReplacement));

        if (statements.Count != 0) {
            replacement = new BoundConditionalExpression(
                leftReplacement,
                centerReplacement,
                rightReplacement,
                expression.type
            );

            return statements;
        }

        replacement = expression;
        return [];
    }

    private protected virtual List<BoundStatement> ExpandObjectCreationExpression(
        BoundObjectCreationExpression expression,
        out BoundExpression replacement) {
        var statements = ExpandArguments(expression.arguments, out var argumentsReplacement);

        replacement = new BoundObjectCreationExpression(
            expression.type,
            expression.constructor,
            argumentsReplacement
        );

        return statements;
    }

    private protected virtual List<BoundStatement> ExpandArrayCreationExpression(
        BoundArrayCreationExpression expression,
        out BoundExpression replacement) {
        var statements = ExpandArguments(expression.sizes, out var sizesReplacement);

        replacement = new BoundArrayCreationExpression(
            expression.type,
            sizesReplacement
        );

        return statements;
    }

    private protected virtual List<BoundStatement> ExpandFieldAccessExpression(
        BoundFieldAccessExpression expression,
        out BoundExpression replacement) {
        var statements = ExpandExpression(expression.receiver, out var receiverReplacement);

        if (statements.Count != 0) {
            replacement = new BoundFieldAccessExpression(
                receiverReplacement,
                expression.field,
                expression.type,
                expression.constantValue
            );

            return statements;
        }

        replacement = expression;
        return [];
    }

    private protected virtual List<BoundStatement> ExpandConditionalAccessExpression(
        BoundConditionalAccessExpression expression,
        out BoundExpression replacement) {
        var statements = ExpandExpression(expression.receiver, out var receiverReplacement);
        statements.AddRange(ExpandExpression(expression.accessExpression, out var accessReplacement));

        if (statements.Count != 0) {
            replacement = new BoundConditionalAccessExpression(expression.type, receiverReplacement, accessReplacement);
            return statements;
        }

        replacement = expression;
        return [];
    }
}
