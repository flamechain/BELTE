using System.Collections.Generic;
using System.Linq;
using Buckle.CodeAnalysis.Binding;
using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;
using Buckle.Utilities;
using static Buckle.CodeAnalysis.Binding.BoundFactory;

namespace Buckle.CodeAnalysis.Lowering;

/// <summary>
/// Expands expressions to make them simpler to handle by the <see cref="Lowerer" />.
/// </summary>
internal sealed class Expander : BoundTreeExpander {
    private readonly List<string> _localNames = [];
    private readonly MethodSymbol _container;

    private int _tempCount = 0;
    private int _compoundAssignmentDepth = 0;
    private int _operatorDepth = 0;

    internal Expander(MethodSymbol container) {
        _container = container;
    }

    internal BoundStatement Expand(BoundStatement statement) {
        return Simplify(ExpandStatement(statement));
    }

    private protected override List<BoundStatement> ExpandLocalDeclarationStatement(
        BoundLocalDeclarationStatement statement) {
        _localNames.Add(statement.declaration.dataContainer.name);
        return base.ExpandLocalDeclarationStatement(statement);
    }

    private protected override List<BoundStatement> ExpandCompoundAssignmentExpression(
        BoundCompoundAssignmentExpression expression,
        out BoundExpression replacement) {
        _compoundAssignmentDepth++;

        if (_compoundAssignmentDepth > 1) {
            var statements = ExpandExpression(expression.left, out var leftReplacement);
            statements.AddRange(ExpandExpression(expression.right, out var rightReplacement));

            statements.Add(
                new BoundExpressionStatement(
                    new BoundCompoundAssignmentExpression(leftReplacement, expression.op, rightReplacement)
                )
            );

            replacement = leftReplacement;
            _compoundAssignmentDepth--;
            return statements;
        }

        var baseStatements = base.ExpandCompoundAssignmentExpression(expression, out replacement);
        _compoundAssignmentDepth--;
        return baseStatements;
    }

    private protected override List<BoundStatement> ExpandCallExpression(
        BoundCallExpression expression,
        out BoundExpression replacement) {
        if (_operatorDepth > 0) {
            var statements = ExpandCallExpressionInternal(expression, out var callReplacement);
            var tempLocal = GenerateTempLocal(expression.type);

            statements.Add(new BoundLocalDeclarationStatement(
                new BoundDataContainerDeclaration(tempLocal, callReplacement)
            ));

            replacement = new BoundDataContainerExpression(tempLocal);

            return statements;
        }

        return ExpandCallExpressionInternal(expression, out replacement);
    }

    private List<BoundStatement> ExpandCallExpressionInternal(
        BoundCallExpression expression,
        out BoundExpression replacement) {
        /*
        TODO What did this do
        if (_transpilerMode && expression.method.containingType.Equals(StandardLibrary.Math)) {
            var statements = ExpandExpression(expression.expression, out var expressionReplacement);
            var replacementArguments = ArrayBuilder<BoundExpression>.GetInstance();

            foreach (var argument in expression.arguments) {
                var tempLocal = GenerateTempLocal(argument.type);
                statements.AddRange(ExpandExpression(argument, out var argumentReplacement));
                statements.Add(new BoundLocalDeclarationStatement(
                    new BoundDataContainerDeclaration(tempLocal, argumentReplacement)
                ));

                replacementArguments.Add(new BoundDataContainerExpression(tempLocal));
            }

            replacement = new BoundCallExpression(
                expressionReplacement,
                expression.method,
                replacementArguments.ToImmutableAndFree()
            );

            return statements;
        }
        */

        return base.ExpandCallExpression(expression, out replacement);
    }

    private protected override List<BoundStatement> ExpandBinaryExpression(
        BoundBinaryExpression expression,
        out BoundExpression replacement) {
        _operatorDepth++;

        if (_operatorDepth > 1) {
            var statements = ExpandExpression(expression.left, out var leftReplacement);
            statements.AddRange(ExpandExpression(expression.right, out var rightReplacement));

            var tempLocal = GenerateTempLocal(expression.type);

            statements.Add(
                new BoundLocalDeclarationStatement(new BoundDataContainerDeclaration(
                    tempLocal,
                    new BoundBinaryExpression(leftReplacement, expression.op, rightReplacement)
                ))
            );

            replacement = new BoundDataContainerExpression(tempLocal);
            _operatorDepth--;
            return statements;
        }

        var baseStatements = base.ExpandBinaryExpression(expression, out replacement);
        _operatorDepth--;
        return baseStatements;
    }

    private protected override List<BoundStatement> ExpandConditionalExpression(
        BoundConditionalExpression expression,
        out BoundExpression replacement) {
        _operatorDepth++;

        if (_operatorDepth > 1) {
            var statements = ExpandExpression(expression.left, out var leftReplacement);
            statements.AddRange(ExpandExpression(expression.center, out var centerReplacement));
            statements.AddRange(ExpandExpression(expression.right, out var rightReplacement));

            var tempLocal = GenerateTempLocal(expression.type);

            statements.Add(
                new BoundLocalDeclarationStatement(new BoundDataContainerDeclaration(
                    tempLocal,
                    new BoundConditionalExpression(
                        leftReplacement,
                        centerReplacement,
                        rightReplacement,
                        expression.type
                    )
                ))
            );

            replacement = new BoundDataContainerExpression(tempLocal);
            _operatorDepth--;
            return statements;
        }

        var baseStatements = base.ExpandConditionalExpression(expression, out replacement);
        _operatorDepth--;
        return baseStatements;
    }

    private protected override List<BoundStatement> ExpandInitializerDictionaryExpression(
        BoundInitializerDictionaryExpression expression,
        out BoundExpression replacement) {
        // TODO Add a way where if _operatorDepth == 0 a temp local isn't made if this is a variable initializer
        var dictionaryType = expression.type as NamedTypeSymbol;
        var tempLocal = GenerateTempLocal(expression.type);
        var statements = new List<BoundStatement>() {
            new BoundLocalDeclarationStatement(new BoundDataContainerDeclaration(
                tempLocal,
                new BoundObjectCreationExpression(
                    expression.type,
                    dictionaryType.constructors[0],
                    []
                )
            ))
        };

        foreach (var pair in expression.items) {
            statements.Add(new BoundExpressionStatement(new BoundCallExpression(
                new BoundDataContainerExpression(tempLocal),
                dictionaryType.GetMembers("Add").Single() as MethodSymbol,
                [pair.Item1, pair.Item2]
            )));
        }

        replacement = new BoundDataContainerExpression(tempLocal);
        return statements;
    }

    private protected override List<BoundStatement> ExpandConditionalAccessExpression(
        BoundConditionalAccessExpression expression,
        out BoundExpression replacement) {
        var receiver = expression.receiver;
        var access = expression.accessExpression;
        var tempLocal = GenerateTempLocal(receiver.type);
        var statements = ExpandExpression(receiver, out var receiverReplacement);

        statements.Add(new BoundLocalDeclarationStatement(
            new BoundDataContainerDeclaration(tempLocal, receiverReplacement))
        );

        var receiverLocal = new BoundDataContainerExpression(tempLocal);

        BoundExpression newAccess;

        if (access is BoundFieldAccessExpression f) {
            newAccess = new BoundFieldAccessExpression(receiverLocal, f.field, f.type, f.constantValue);
        } else if (access is BoundArrayAccessExpression a) {
            statements.AddRange(ExpandExpression(a.index, out var indexReplacement));
            newAccess = new BoundArrayAccessExpression(receiverLocal, indexReplacement, a.type);
        } else {
            throw ExceptionUtilities.Unreachable();
        }

        replacement = new BoundConditionalExpression(
            HasValue(receiver),
            newAccess,
            new BoundLiteralExpression(value: null, access.type),
            access.type
        );

        return statements;
    }

    private SynthesizedDataContainerSymbol GenerateTempLocal(TypeSymbol type) {
        string name;

        do {
            name = $"temp{_tempCount++}";
        } while (_localNames.Contains(name));

        return new SynthesizedDataContainerSymbol(_container, new TypeWithAnnotations(type), name);
    }
}
