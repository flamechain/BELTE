using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
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

        if (entryPoint is null) {
            hasValue = false;
            return null;
        }

        var entryPointBody = _program.methodBodies[entryPoint];
        EnterClassScope(new EvaluatorObject([], entryPoint.containingType));
        var result = EvaluateStatement(entryPointBody, abort, out _);
        hasValue = _hasValue;
        return Value(result);
    }

    #region Internal Model

    private object Value(EvaluatorObject value, bool traceCollections = false) {
        if (value.isReference)
            return GetVariableValue(value.reference, traceCollections);
        else if (value.value is EvaluatorObject e)
            return Value(e, traceCollections);
        else if (value.value is EvaluatorObject[] && traceCollections)
            return CollectionValue(value.value as EvaluatorObject[]);
        else if (traceCollections && value.value is null && value.members is not null)
            return DictionaryValue(value.members, value.type);
        else
            return value.value;
    }

    private Dictionary<object, object> DictionaryValue(
        Dictionary<Symbol, EvaluatorObject> value,
        TypeSymbol containingType) {
        var dictionary = new Dictionary<object, object>();

        foreach (var pair in value) {
            if (pair.Key is FieldSymbol) {
                var name = pair.Key.containingType.Equals(containingType)
                    ? pair.Key.name
                    : $"{pair.Key.containingType.name}.{pair.Key.name}";

                dictionary.Add(name, Value(pair.Value, true));
            }
        }

        return dictionary;
    }

    private object[] CollectionValue(EvaluatorObject[] value) {
        var builder = new object[value.Length];

        for (var i = 0; i < value.Length; i++)
            builder[i] = Value(value[i], true);

        return builder;
    }

    private object GetVariableValue(Symbol variable, bool traceCollections = false) {
        var value = Get(variable);

        try {
            return Value(value, traceCollections);
        } catch (BelteInternalException) {
            throw new BelteEvaluatorException(
                $"Reference cannot be deferred (what it was referencing was likely redefined)"
            );
        }
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
            return new EvaluatorObject(evaluatorObject.reference, evaluatorObject.type, isExplicitReference: true);
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

    private void Assign(EvaluatorObject left, EvaluatorObject right) {
        right = Dereference(right, false);
        left = Dereference(left, false);

        if (right.isExplicitReference) {
            left.reference = right.reference;
            return;
        } else if (left.isExplicitReference) {
            left = Dereference(left);
        }

        if (right.members is null)
            left.members = null;

        if (right.value is null && right.members != null)
            left.members = Copy(right.members);
        else
            left.value = Value(right);

        left.type = right.type;
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

    private void EnterClassScope(EvaluatorObject @class) {
        var classLocalBuffer = new Dictionary<Symbol, EvaluatorObject>();

        foreach (var member in @class.members) {
            if (member.Key is FieldSymbol f) {
                // If the symbol is already present it could be outdated and should be replaced
                // If it isn't outdated no harm in replacing it
                classLocalBuffer.Remove(f);
                classLocalBuffer.Add(f, member.Value);
            }
        }

        _enclosingTypes.Push(@class);
        _locals.Push(classLocalBuffer);
    }

    private void ExitClassScope() {
        _enclosingTypes.Pop();
        _locals.Pop();
    }

    #endregion

    #region Statements

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
                    case BoundKind.ExpressionStatement:
                        EvaluateExpressionStatement((BoundExpressionStatement)s, abort);
                        index++;
                        break;
                    case BoundKind.LocalDeclarationStatement:
                        EvaluateLocalDeclarationStatement((BoundLocalDeclarationStatement)s, abort);
                        index++;
                        break;
                    case BoundKind.ReturnStatement:
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

    #endregion

    #region Expressions

    private EvaluatorObject EvaluateExpression(BoundExpression expression, ValueWrapper<bool> abort) {
        if (expression.constantValue is not null)
            return EvaluateConstant(expression.constantValue);

        return expression.kind switch {
            BoundKind.EmptyExpression => EvaluatorObject.Null,
            BoundKind.ThisExpression => EvaluateThisExpression(),
            BoundKind.BaseExpression => EvaluateBaseExpression(),
            BoundKind.CastExpression => EvaluateCastExpression((BoundCastExpression)expression, abort),
            BoundKind.DataContainerExpression => EvaluateDataContainerExpression((BoundDataContainerExpression)expression),
            BoundKind.ParameterExpression => EvaluateParameterExpression((BoundParameterExpression)expression),
            BoundKind.FieldAccessExpression => EvaluateFieldAccessExpression((BoundFieldAccessExpression)expression, abort),
            BoundKind.AssignmentOperator => EvaluateAssignmentOperator((BoundAssignmentOperator)expression, abort),
            BoundKind.BinaryOperator => EvaluateBinaryOperator((BoundBinaryOperator)expression, abort),
            BoundKind.NullAssertExpression => EvaluateNullAssertExpression((BoundNullAssertExpression)expression, abort),
            BoundKind.AsOperator => EvaluateAsOperator((BoundAsOperator)expression, abort),
            BoundKind.IsOperator => EvaluateIsOperator((BoundIsOperator)expression, abort),
            BoundKind.IsntOperator => EvaluateIsntOperator((BoundIsntOperator)expression, abort),
            BoundKind.ConditionalOperator => EvaluateConditionalOperator((BoundConditionalOperator)expression, abort),
            BoundKind.CallExpression => EvaluateCallExpression((BoundCallExpression)expression, abort),
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

    private EvaluatorObject EvaluateCallExpression(BoundCallExpression expression, ValueWrapper<bool> abort) {
        if (CheckStandardMap(
            expression.method,
            expression.arguments,
            abort,
            out var result,
            out var printed,
            out var io)) {
            lastOutputWasPrint = printed;
            containsIO = io;

            if (result is EvaluatorObject e)
                return e;

            return new EvaluatorObject(result, expression.method.returnType);
        }

        return InvokeMethod(expression.method, expression.arguments, abort, expression.receiver);
    }

    private EvaluatorObject InvokeMethod(
        MethodSymbol method,
        ImmutableArray<BoundExpression> arguments,
        ValueWrapper<bool> abort,
        BoundExpression receiver) {
        var receiverObject = default(EvaluatorObject);

        if (receiver is not null && receiver is not BoundEmptyExpression) {
            receiverObject = EvaluateExpression(receiver, abort);
            var dereferencedReceiver = Dereference(receiverObject);

            if (dereferencedReceiver.members is null)
                throw new NullReferenceException();
        }

        if (method.isAbstract || method.isVirtual) {
            var type = Dereference(receiverObject).type;
            var newMethod = type
                .GetMembers()
                .Where(s => s is MethodSymbol m && m == method && m.isOverride)
                .First() as MethodSymbol;

            if (newMethod is not null)
                method = newMethod;
        }

        var locals = new Dictionary<Symbol, EvaluatorObject>();
        // AddTemplatesToLocals(method.templateParameters, method.templateArguments, locals, abort);

        for (var i = 0; i < arguments.Length; i++) {
            var parameter = method.parameters[i];
            var value = EvaluateExpression(arguments[i], abort);

            while (parameter.refKind != RefKind.None && value.isReference)
                value = Get(value.reference);

            locals.Add(parameter, Copy(value));
        }

        _locals.Push(locals);

        _program.TryGetMethodBodyIncludingParents(method, out var statement);
        // var templateConstantDepth = _templateConstantDepth;
        var enteredScope = false;

        // !
        if (receiverObject is not null /*&& (receiver.isReference || expression is BoundObjectCreationExpression)*/) {
            // On an expression such as 'myInstance.Method()', we need to enter the 'myInstance' class scope
            // in case 'Method' uses 'this'
            // If what we get here is not a reference, it is a static accession and the needed scoped members have
            // already been pushed by 'EvaluateType'.
            receiverObject = Dereference(receiverObject);

            if (receiverObject.members is not null) {
                EnterClassScope(receiverObject);
                enteredScope = true;
            }
        }

        var result = EvaluateStatement(statement, abort, out _);

        // while (_templateConstantDepth > templateConstantDepth) {
        //     _templateConstantDepth--;
        //     _locals.Pop();
        // }

        _locals.Pop();

        if (enteredScope)
            ExitClassScope();

        return result;
    }

    private EvaluatorObject EvaluateDataContainerExpression(BoundDataContainerExpression expression) {
        return new EvaluatorObject(expression.dataContainer, expression.dataContainer.type);
    }

    private EvaluatorObject EvaluateConditionalOperator(
        BoundConditionalOperator expression,
        ValueWrapper<bool> abort) {
        var left = EvaluateExpression(expression.left, abort);
        var leftValue = Value(left);

        if ((bool)leftValue)
            return EvaluateExpression(expression.center, abort);
        else
            return EvaluateExpression(expression.right, abort);
    }

    private EvaluatorObject EvaluateNullAssertExpression(
        BoundNullAssertExpression expression,
        ValueWrapper<bool> abort) {
        var value = Dereference(EvaluateExpression(expression.operand, abort));

        if (value.members is null && Value(value) is null && expression.operand.type.specialType != SpecialType.Type)
            throw new NullReferenceException();

        return Copy(value);
    }

    private EvaluatorObject EvaluateAsOperator(BoundAsOperator expression, ValueWrapper<bool> abort) {
        var left = EvaluateExpression(expression.left, abort);
        var leftValue = Value(left);
        var dereferenced = Dereference(left);

        if (dereferenced.members is null)
            return new EvaluatorObject(leftValue, expression.type);

        if (dereferenced.type.InheritsFromIgnoringConstruction((NamedTypeSymbol)expression.right.type))
            return left;

        return EvaluatorObject.Null;
    }

    private EvaluatorObject EvaluateIsOperator(BoundIsOperator expression, ValueWrapper<bool> abort) {
        var left = EvaluateExpression(expression.left, abort);
        var right = expression.right;
        var leftValue = Value(left);
        var dereferenced = Dereference(left);

        if (right.IsLiteralNull()) {
            if (left.members is null && leftValue is null &&
                (expression.left.type.specialType != SpecialType.Type || left.type is null)) {
                return new EvaluatorObject(true, expression.type);
            }

            return new EvaluatorObject(false, expression.type);
        }

        if (leftValue is null && dereferenced.members is null)
            return new EvaluatorObject(false, expression.type);

        if (dereferenced.members is null) {
            return new EvaluatorObject(
                (right.type.StrippedType().specialType == SpecialType.Any) ||
                (SpecialTypeExtensions.SpecialTypeFromLiteralValue(leftValue) == right.type.specialType),
                expression.type
            );
        }

        if (dereferenced.type.InheritsFromIgnoringConstruction((NamedTypeSymbol)expression.right.type))
            return new EvaluatorObject(true, expression.type);
        else
            return new EvaluatorObject(false, expression.type);
    }

    private EvaluatorObject EvaluateIsntOperator(BoundIsntOperator expression, ValueWrapper<bool> abort) {
        var left = EvaluateExpression(expression.left, abort);
        var right = expression.right;
        var leftValue = Value(left);
        var dereferenced = Dereference(left);

        if (right.IsLiteralNull()) {
            if (left.members is null && leftValue is null &&
                (expression.left.type.specialType != SpecialType.Type || left.type is null)) {
                return new EvaluatorObject(false, expression.type);
            }

            return new EvaluatorObject(true, expression.type);
        }

        if (leftValue is null && dereferenced.members is null)
            return new EvaluatorObject(false, expression.type);

        if (dereferenced.members is null) {
            return new EvaluatorObject(
                !((right.type.StrippedType().specialType == SpecialType.Any) ||
                (SpecialTypeExtensions.SpecialTypeFromLiteralValue(leftValue) == right.type.specialType)),
                expression.type
            );
        }

        if (dereferenced.type.InheritsFromIgnoringConstruction((NamedTypeSymbol)expression.right.type))
            return new EvaluatorObject(false, expression.type);
        else
            return new EvaluatorObject(true, expression.type);
    }

    private EvaluatorObject EvaluateBinaryOperator(BoundBinaryOperator expression, ValueWrapper<bool> abort) {
        var left = EvaluateExpression(expression.left, abort);
        var leftValue = Value(left);
        var opKind = expression.opKind & BinaryOperatorKind.OpMask;

        if (opKind == BinaryOperatorKind.ConditionalAnd) {
            if (leftValue is null || !(bool)leftValue)
                return new EvaluatorObject(false, expression.type);

            var shortCircuitRight = EvaluateExpression(expression.right, abort);
            var shortCircuitRightValue = Value(shortCircuitRight);

            if (shortCircuitRightValue is null || !(bool)shortCircuitRightValue)
                return new EvaluatorObject(false, expression.type);

            return new EvaluatorObject(true, expression.type);
        }

        if (opKind == BinaryOperatorKind.ConditionalOr) {
            if (leftValue != null && (bool)leftValue)
                return new EvaluatorObject(true, expression.type);

            var shortCircuitRight = EvaluateExpression(expression.right, abort);
            var shortCircuitRightValue = Value(shortCircuitRight);

            if (shortCircuitRightValue != null && (bool)shortCircuitRightValue)
                return new EvaluatorObject(true, expression.type);

            return new EvaluatorObject(false, expression.type);
        }

        var right = EvaluateExpression(expression.right, abort);
        var rightValue = Value(right);

        if (opKind is BinaryOperatorKind.Equal or BinaryOperatorKind.NotEqual
            && expression.left.type.specialType == SpecialType.Type) {
            if ((leftValue as TypeSymbol).Equals(rightValue as TypeSymbol))
                return new EvaluatorObject(opKind == BinaryOperatorKind.Equal, expression.type);
            else
                return new EvaluatorObject(opKind == BinaryOperatorKind.NotEqual, expression.type);
        }

        if (leftValue is null || rightValue is null)
            return EvaluatorObject.Null;

        var expressionType = expression.type.specialType;
        var leftType = expression.left.type.specialType;
        object result;

        switch (opKind) {
            case BinaryOperatorKind.Addition:
                if (expressionType == SpecialType.Int)
                    result = (int)leftValue + (int)rightValue;
                else if (expressionType == SpecialType.String)
                    result = (string)leftValue + (string)rightValue;
                else
                    result = Convert.ToDouble(leftValue) + Convert.ToDouble(rightValue);

                break;
            case BinaryOperatorKind.Subtraction:
                result = expressionType == SpecialType.Int
                    ? (int)leftValue - (int)rightValue
                    : Convert.ToDouble(leftValue) - Convert.ToDouble(rightValue);

                break;
            case BinaryOperatorKind.Multiplication:
                result = expressionType == SpecialType.Int
                    ? (int)leftValue * (int)rightValue
                    : Convert.ToDouble(leftValue) * Convert.ToDouble(rightValue);

                break;
            case BinaryOperatorKind.Division:
                result = expressionType == SpecialType.Int
                    ? (int)leftValue / (int)rightValue
                    : Convert.ToDouble(leftValue) / Convert.ToDouble(rightValue);

                break;
            case BinaryOperatorKind.Equal:
                result = Equals(leftValue, rightValue);
                break;
            case BinaryOperatorKind.NotEqual:
                result = !Equals(leftValue, rightValue);
                break;
            case BinaryOperatorKind.LessThan:
                result = leftType == SpecialType.Int
                    ? (int)leftValue < (int)rightValue
                    : Convert.ToDouble(leftValue) < Convert.ToDouble(rightValue);

                break;
            case BinaryOperatorKind.GreaterThan:
                result = leftType == SpecialType.Int
                    ? (int)leftValue > (int)rightValue
                    : Convert.ToDouble(leftValue) > Convert.ToDouble(rightValue);

                break;
            case BinaryOperatorKind.LessThanOrEqual:
                result = leftType == SpecialType.Int
                    ? (int)leftValue <= (int)rightValue
                    : Convert.ToDouble(leftValue) <= Convert.ToDouble(rightValue);

                break;
            case BinaryOperatorKind.GreaterThanOrEqual:
                result = leftType == SpecialType.Int
                    ? (int)leftValue >= (int)rightValue
                    : Convert.ToDouble(leftValue) >= Convert.ToDouble(rightValue);

                break;
            case BinaryOperatorKind.And:
                result = expressionType == SpecialType.Int
                    ? (int)leftValue & (int)rightValue
                    : (bool)leftValue & (bool)rightValue;

                break;
            case BinaryOperatorKind.Or:
                result = expressionType == SpecialType.Int
                    ? (int)leftValue | (int)rightValue
                    : (bool)leftValue | (bool)rightValue;

                break;
            case BinaryOperatorKind.Xor:
                result = expressionType == SpecialType.Int
                    ? (int)leftValue ^ (int)rightValue
                    : (bool)leftValue ^ (bool)rightValue;

                break;
            case BinaryOperatorKind.LeftShift:
                result = (int)leftValue << (int)rightValue;
                break;
            case BinaryOperatorKind.RightShift:
                result = (int)leftValue >> (int)rightValue;
                break;
            case BinaryOperatorKind.UnsignedRightShift:
                result = (int)leftValue >>> (int)rightValue;
                break;
            case BinaryOperatorKind.Modulo:
                result = expressionType == SpecialType.Int
                    ? (int)leftValue % (int)rightValue
                    : Convert.ToDouble(leftValue) % Convert.ToDouble(rightValue);

                break;
            default:
                throw ExceptionUtilities.UnexpectedValue(expression.opKind);
        }

        return new EvaluatorObject(result, expression.type);
    }

    private EvaluatorObject EvaluateAssignmentOperator(
        BoundAssignmentOperator expression,
        ValueWrapper<bool> abort) {
        var left = EvaluateExpression(expression.left, abort);
        var right = EvaluateExpression(expression.right, abort);
        Assign(left, right);
        return right;
    }

    private EvaluatorObject EvaluateParameterExpression(BoundParameterExpression expression) {
        return new EvaluatorObject(expression.parameter, expression.parameter.type);
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

    #endregion

    private bool CheckStandardMap(
        MethodSymbol method,
        ImmutableArray<BoundExpression> arguments,
        ValueWrapper<bool> abort,
        out object result,
        out bool printed,
        out bool io) {
        // TODO
        printed = false;
        io = false;
        result = null;
        return false;
    }
}
