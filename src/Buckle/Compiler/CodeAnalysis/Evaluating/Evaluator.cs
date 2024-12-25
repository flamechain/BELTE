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
        _locals.Push([]);
        exceptions = [];
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

    private void Create(DataContainerSymbol symbol, EvaluatorObject value) {
        if (symbol.isGlobal) {
            var set = false;

            foreach (var global in _globals) {
                if (global.Key.name == symbol.name) {
                    _globals.Remove(global.Key);
                    _globals[symbol] = Copy(value);
                    set = true;
                    break;
                }
            }

            if (!set)
                _globals[symbol] = Copy(value);
        } else {
            var locals = _locals.Peek();
            var set = false;

            foreach (var local in locals) {
                if (local.Key.name == symbol.name) {
                    locals.Remove(local.Key);
                    locals[symbol] = Copy(value);
                    set = true;
                    break;
                }
            }

            if (!set)
                locals[symbol] = Copy(value);
        }
    }

    private EvaluatorObject Copy(EvaluatorObject evaluatorObject) {
        if (evaluatorObject.reference is not null && !evaluatorObject.isExplicitReference)
            return Copy(Get(evaluatorObject.reference));
        else if (evaluatorObject.reference is not null)
            return new EvaluatorObject(evaluatorObject.reference, isExplicitReference: true);
        else if (evaluatorObject.members is not null)
            return new EvaluatorObject(Copy(evaluatorObject.members), evaluatorObject.type);
        else
            return new EvaluatorObject(evaluatorObject.value, evaluatorObject.type);
    }

    private Dictionary<Symbol, EvaluatorObject> Copy(Dictionary<Symbol, EvaluatorObject> members) {
        var newMembers = new Dictionary<Symbol, EvaluatorObject>();

        foreach (var member in members)
            newMembers.Add(member.Key, Copy(member.Value));

        return newMembers;
    }

    private EvaluatorObject Get(Symbol symbol) {
        if (symbol is DataContainerSymbol d && d.isGlobal) {
            if (_globals.TryGetValue(d, out var evaluatorObject))
                return evaluatorObject;
        } else {
            foreach (var frame in _locals) {
                if (frame.TryGetValue(symbol, out var evaluatorObject))
                    return evaluatorObject;
            }
        }

        throw ExceptionUtilities.Unreachable();
    }

    private EvaluatorObject GetFromScopeWithFallback(
        DataContainerSymbol variable,
        Dictionary<IDataContainerSymbol, EvaluatorObject> scope) {
        if (scope.TryGetValue(variable, out var evaluatorObject))
            return evaluatorObject;

        return Get(variable);
    }

    private EvaluatorObject Dereference(EvaluatorObject reference, bool dereferenceOnExplicit = true) {
        while (reference.isReference) {
            if (!dereferenceOnExplicit && reference.isExplicitReference)
                break;

            reference = Get(reference.reference);
        }

        return reference;
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
                    case BoundNodeKind.LocalDeclarationStatement:
                        EvaluateLocalDeclarationStatement((BoundLocalDeclarationStatement)s, abort);
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
            // } catch (Exception e) when (e is not BelteInternalException) {
            // TODO temp for exception tracing, delete
        } catch (BelteInternalException e) {
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

    private void EvaluateLocalDeclarationStatement(BoundLocalDeclarationStatement statement, ValueWrapper<bool> abort) {
        var value = EvaluateExpression(statement.declaration.initializer, abort);
        _lastValue = default;
        Create(statement.declaration.dataContainer, value);
    }

    private EvaluatorObject EvaluateExpression(BoundExpression expression, ValueWrapper<bool> abort) {
        if (expression.constantValue is not null)
            return EvaluateConstant(expression.constantValue);

        return expression.kind switch {
            BoundNodeKind.EmptyExpression => EvaluatorObject.Null,
            BoundNodeKind.ThisExpression => EvaluateThisExpression(),
            BoundNodeKind.BaseExpression => EvaluateBaseExpression(),
            BoundNodeKind.CastExpression => EvaluateCastExpression((BoundCastExpression)expression, abort),
            BoundNodeKind.DataContainerExpression => EvaluateDataContainerExpression((BoundDataContainerExpression)expression),
            BoundNodeKind.ParameterExpression => EvaluateParameterExpression((BoundParameterExpression)expression),
            BoundNodeKind.FieldAccessExpression => EvaluateFieldAccessExpression((BoundFieldAccessExpression)expression, abort),
            _ => throw new BelteInternalException($"EvaluateExpression: unexpected node '{expression.kind}'"),
        };
    }

    private EvaluatorObject EvaluateConstant(ConstantValue constantValue) {
        // TODO is this clarity worth the performance loss?
        if (constantValue.specialType == SpecialType.None)
            return new EvaluatorObject(constantValue.value, null);

        var type = CorLibrary.GetSpecialType(constantValue.specialType);
        return new EvaluatorObject(constantValue.value, type);
    }

    private EvaluatorObject EvaluateDataContainerExpression(BoundDataContainerExpression expression) {
        return new EvaluatorObject(expression.dataContainer);
    }

    private EvaluatorObject EvaluateParameterExpression(BoundParameterExpression expression) {
        return new EvaluatorObject(expression.parameter);
    }

    private EvaluatorObject EvaluateFieldAccessExpression(
        BoundFieldAccessExpression expression,
        ValueWrapper<bool> abort) {
        var operand = Dereference(EvaluateExpression(expression.receiver, abort), true);
        return operand.members[expression.field];
    }

    private EvaluatorObject EvaluateThisExpression() {
        return _enclosingTypes.Peek();
    }

    private EvaluatorObject EvaluateBaseExpression() {
        return _enclosingTypes.Peek();
    }

    private EvaluatorObject EvaluateCastExpression(BoundCastExpression expression, ValueWrapper<bool> abort) {
        var value = EvaluateExpression(expression.operand, abort);
        return EvaluateCast(value, expression.operand.type, expression.type);
    }

    private EvaluatorObject EvaluateCast(EvaluatorObject value, TypeSymbol source, TypeSymbol target) {
        var dereferenced = Dereference(value);

        if (dereferenced.members is null) {
            var valueValue = Value(value);

            if (target.specialType != SpecialType.Nullable && value is null)
                throw new NullReferenceException();

            if (source.Equals(target, TypeCompareKind.IgnoreNullability))
                return value;

            return new EvaluatorObject(SpecialTypeCast(valueValue, target.specialType), target);
        }

        if (dereferenced.type.InheritsFromIgnoringConstruction((NamedTypeSymbol)target))
            return value;

        throw new InvalidCastException();
    }

    private static object SpecialTypeCast(object value, SpecialType target) {
        switch (target) {
            case SpecialType.Int:
                if (value.IsFloatingPoint())
                    value = Math.Truncate(Convert.ToDouble(value));

                return Convert.ToInt32(value);
            case SpecialType.Decimal:
                return Convert.ToDouble(value);
            case SpecialType.Bool:
                return Convert.ToBoolean(value);
            case SpecialType.String:
                return Convert.ToString(value);
            case SpecialType.Char:
                return Convert.ToChar(value);
            default:
                return value;
        }
    }
}
