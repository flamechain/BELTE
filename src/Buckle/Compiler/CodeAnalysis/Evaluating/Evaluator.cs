using System;
using System.Collections.Generic;
using Buckle.CodeAnalysis.Binding;
using Buckle.CodeAnalysis.Symbols;
using Buckle.Diagnostics;
using Buckle.Libraries;
using Buckle.Utilities;
using Shared;

namespace Buckle.CodeAnalysis.Evaluating;

/// <summary>
/// Evaluates BoundStatements inline similar to an interpreter.
/// </summary>
internal sealed class Evaluator {
    private readonly BoundProgram _program;
    private readonly Dictionary<IDataContainerSymbol, EvaluatorObject> _globals;
    private readonly Stack<Dictionary<Symbol, EvaluatorObject>> _locals;
    private readonly Stack<EvaluatorObject> _enclosingTypes;

    private EvaluatorObject _lastValue;
    private bool _hasValue;

    /// <summary>
    /// Creates an <see cref="Evaluator" /> that can evaluate a <see cref="BoundProgram" /> (provided globals).
    /// </summary>
    /// <param name="program"><see cref="BoundProgram" />.</param>
    /// <param name="globals">Globals.</param>
    /// <param name="arguments">Runtime arguments.</param>
    internal Evaluator(
        BoundProgram program,
        Dictionary<IDataContainerSymbol, EvaluatorObject> globals,
        string[] arguments) {
        _globals = globals;
        _program = program;
        _enclosingTypes = new Stack<EvaluatorObject>();
        _locals = new Stack<Dictionary<Symbol, EvaluatorObject>>();
    }

    /// <summary>
    /// If the last output to the terminal was a `Print`, and not a `PrintLine`, meaning the caller might want to write
    /// an extra line to prevent formatting problems.
    /// </summary>
    internal bool lastOutputWasPrint { get; private set; }

    /// <summary>
    /// If the submission contains File/Directory IO.
    /// </summary>
    internal bool containsIO { get; private set; }

    /// <summary>
    /// All thrown exceptions during evaluation.
    /// </summary>
    internal List<Exception> exceptions { get; set; }

    /// <summary>
    /// Evaluate the provided <see cref="BoundProgram" />.
    /// </summary>
    /// <param name="abort">External flag used to cancel evaluation.</param>
    /// <param name="hasValue">If the evaluation had a returned result.</param>
    /// <returns>Result of <see cref="BoundProgram" /> (if applicable).</returns>
    internal object Evaluate(ValueWrapper<bool> abort, out bool hasValue) {
        var entryPoint = _program.entryPoint;
        var entryPointBody = _program.methodBodies[entryPoint];
        var result = EvaluateStatement(entryPointBody, abort, out _);
        hasValue = _hasValue;
        return Value(result);
    }

    private object Value(EvaluatorObject evaluatorObject) {
        return evaluatorObject.value;
    }

    private EvaluatorObject Copy(EvaluatorObject evaluatorObject) {
        return new EvaluatorObject(evaluatorObject.value, evaluatorObject.type);
    }

    private EvaluatorObject EvaluateStatement(
        BoundBlockStatement block,
        ValueWrapper<bool> abort,
        out bool hasReturn,
        bool insideTry = false) {
        _hasValue = false;
        hasReturn = false;

        try {
            var labelToIndex = new Dictionary<LabelSymbol, int>();

            for (var i = 0; i < block.statements.Length; i++) {
                if (block.statements[i] is BoundLabelStatement l)
                    labelToIndex.Add(l.label, i + 1);
            }

            var index = 0;

            while (index < block.statements.Length) {
                if (abort)
                    throw new BelteThreadException();

                var s = block.statements[index];

                switch (s.kind) {
                    case BoundNodeKind.ExpressionStatement:
                        EvaluateExpressionStatement((BoundExpressionStatement)s, abort);
                        index++;
                        break;
                    case BoundNodeKind.ReturnStatement:
                        var returnStatement = (BoundReturnStatement)s;
                        _lastValue = returnStatement.expression is null
                            ? EvaluatorObject.Null
                            : Copy(EvaluateExpression(returnStatement.expression, abort));

                        _hasValue = returnStatement.expression is not null and not BoundEmptyExpression;
                        hasReturn = true;

                        return _lastValue;
                    default:
                        throw ExceptionUtilities.UnexpectedValue(s.kind);
                }
            }

            return _lastValue;
        } catch (Exception e) when (e is not BelteInternalException) {
            // TODO temp for exception tracing, delete
            // } catch (BelteInternalException e) {
            if (abort)
                return EvaluatorObject.Null;

            if (insideTry)
                throw;

            exceptions.Add(e);
            lastOutputWasPrint = false;
            _hasValue = false;

            if (!Console.IsOutputRedirected) {
                // TODO Move this logic to the Repl
                if (Console.CursorLeft != 0)
                    Console.WriteLine();

                var previous = Console.ForegroundColor;
                Console.ForegroundColor = ConsoleColor.Red;
                Console.Write($"Unhandled exception ({e.GetType()}): ");
                Console.ForegroundColor = previous;
                Console.WriteLine(e.Message);
            }

            return EvaluatorObject.Null;
        }
    }

    private void EvaluateExpressionStatement(BoundExpressionStatement statement, ValueWrapper<bool> abort) {
        _lastValue = EvaluateExpression(statement.expression, abort);
    }

    private EvaluatorObject EvaluateExpression(BoundExpression expression, ValueWrapper<bool> abort) {
        if (expression.constantValue is not null)
            return EvaluateConstant(expression.constantValue);

        switch (expression.kind) {
            case BoundNodeKind.EmptyExpression:
                return EvaluatorObject.Null;
            case BoundNodeKind.ThisExpression:
                return EvaluateThisExpression();
            case BoundNodeKind.BaseExpression:
                return EvaluateBaseExpression();
            default:
                throw new BelteInternalException($"EvaluateExpression: unexpected node '{expression.kind}'");
        }
    }

    private EvaluatorObject EvaluateConstant(ConstantValue constantValue) {
        // TODO is this clarity worth the performance loss?
        if (constantValue.specialType == SpecialType.None)
            return new EvaluatorObject(constantValue.value, null);

        var type = CorLibrary.GetSpecialType(constantValue.specialType);
        return new EvaluatorObject(constantValue.value, type);
    }

    private EvaluatorObject EvaluateThisExpression() {
        return _enclosingTypes.Peek();
    }

    private EvaluatorObject EvaluateBaseExpression() {
        return _enclosingTypes.Peek();
    }
}
