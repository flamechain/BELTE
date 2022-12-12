using System;
using System.Collections.Generic;
using Buckle.Diagnostics;
using Buckle.CodeAnalysis.Binding;
using Buckle.CodeAnalysis.Symbols;
using Buckle.Utils;

namespace Buckle.CodeAnalysis;

/// <summary>
/// Encased object that can also be a reference to a symbol.
/// </summary>
internal sealed class EvaluatorObject {
    /// <summary>
    /// Creates an EvaluatorObject with a value (not a reference).
    /// In this case EvaluatorObject acts purely as an Object wrapper.
    /// </summary>
    /// <param name="value">Value to store</param>
    internal EvaluatorObject(object value) {
        this.value = value;
        this.isReference = false;
        this.reference = null;
    }

    /// <summary>
    /// Creates an EvaluatorObjet without a value, and instead a reference to a VariableSymbol.
    /// Note that it is not an actual C# reference, just a copy of a symbol stored in the locals or globals dictionary.
    /// </summary>
    /// <param name="reference">Variable to reference (not an explicit reference, passed by reference by default)</param>
    internal EvaluatorObject(VariableSymbol reference) {
        this.value = null;

        if (reference == null) {
            this.isReference = false;
            this.reference = null;
        } else {
            this.isReference = true;
            this.reference = reference;
        }
    }

    /// <summary>
    /// Value of object, only applicable if isReference is set to false.
    /// </summary>
    internal object value { get; set; }

    /// <summary>
    /// If this is to be treated as a reference. If so, value is set to null but ignored.
    /// If value is set to null and isReference is false,
    /// Then it treats value as being the value null, not lacking a value.
    /// </summary>
    internal bool isReference { get; set; }

    /// <summary>
    /// Reference to a symbol stored in the locals or globals dictionary.
    /// Not explicitly a reference, but is passed by reference by default.
    /// </summary>
    /// <value></value>
    internal VariableSymbol reference { get; set; }
}

/// <summary>
/// Evaluates statements as an interpreter, inline.
/// </summary>
internal sealed class Evaluator {
    private readonly BoundProgram program_;
    private readonly Dictionary<VariableSymbol, EvaluatorObject> globals_;
    private readonly Dictionary<FunctionSymbol, BoundBlockStatement> functions_ =
        new Dictionary<FunctionSymbol, BoundBlockStatement>();
    private readonly Stack<Dictionary<VariableSymbol, EvaluatorObject>> locals_ =
        new Stack<Dictionary<VariableSymbol, EvaluatorObject>>();
    private EvaluatorObject lastValue_;
    private Random random_;
    private bool hasPrint_ = false;

    /// <summary>
    /// Creates an evaluator that can evaluate a program (provided globals).
    /// </summary>
    /// <param name="program">Program</param>
    /// <param name="globals">Globals</param>
    internal Evaluator(BoundProgram program, Dictionary<VariableSymbol, EvaluatorObject> globals) {
        diagnostics = new BelteDiagnosticQueue();
        program_ = program;
        globals_ = globals;
        locals_.Push(new Dictionary<VariableSymbol, EvaluatorObject>());

        var current = program;
        while (current != null) {
            foreach (var (function, body) in current.functionBodies)
                functions_.Add(function, body);

            current = current.previous;
        }
    }

    /// <summary>
    /// If it has a Print statement, adds a line break to avoid formatting issues (mostly with the REPL).
    /// </summary>
    internal bool hasPrint {
        get {
            return hasPrint_;
        } set {
            hasPrint_ = value;
        }
    }

    /// <summary>
    /// Diagnostics specific to the evaluator.
    /// </summary>
    internal BelteDiagnosticQueue diagnostics { get; set; }

    /// <summary>
    /// Evaluate the provided program.
    /// </summary>
    /// <returns>Result of program (if applicable)</returns>
    internal object Evaluate() {
        var function = program_.mainFunction ?? program_.scriptFunction;
        if (function == null)
            return null;

        var body = LookupMethod(function);
        var result = EvaluateStatement(body);

        if (result.isReference)
            return GetVariableValue(result.reference);
        else
            return result.value;
    }

    private object GetVariableValue(VariableSymbol variable) {
        EvaluatorObject value = null;

        if (variable.type == SymbolType.GlobalVariable) {
            value = globals_[variable];
        } else {
            var locals = locals_.Peek();
            value = locals[variable];
        }

        if (value.isReference)
            return GetVariableValue(value.reference);
        else if (value.value is EvaluatorObject)
            return Value(value.value as EvaluatorObject);
        else
            return value.value;
    }

    private object Value(EvaluatorObject value) {
        if (value.isReference)
            return GetVariableValue(value.reference);
        else if (value.value is EvaluatorObject)
            return Value(value.value as EvaluatorObject);
        else
            return value.value;
    }

    private EvaluatorObject EvaluateStatement(BoundBlockStatement statement) {
        try {
            var labelToIndex = new Dictionary<BoundLabel, int>();

            for (int i=0; i<statement.statements.Length; i++) {
                if (statement.statements[i] is BoundLabelStatement l)
                    labelToIndex.Add(l.label, i + 1);
            }

            var index = 0;
            while (index < statement.statements.Length) {
                var s = statement.statements[index];

                switch (s.type) {
                    case BoundNodeType.NopStatement:
                        index++;
                        break;
                    case BoundNodeType.ExpressionStatement:
                        EvaluateExpressionStatement((BoundExpressionStatement)s);
                        index++;
                        break;
                    case BoundNodeType.VariableDeclarationStatement:
                        EvaluateVariableDeclarationStatement((BoundVariableDeclarationStatement)s);
                        index++;
                        break;
                    case BoundNodeType.GotoStatement:
                        var gs = (BoundGotoStatement)s;
                        index = labelToIndex[gs.label];
                        break;
                    case BoundNodeType.ConditionalGotoStatement:
                        var cgs = (BoundConditionalGotoStatement)s;
                        var condition = (bool)EvaluateExpression(cgs.condition).value;

                        if (condition == cgs.jumpIfTrue)
                            index = labelToIndex[cgs.label];
                        else
                            index++;

                        break;
                    case BoundNodeType.LabelStatement:
                        index++;
                        break;
                    case BoundNodeType.ReturnStatement:
                        var returnStatement = (BoundReturnStatement)s;
                        var lastValue_ = returnStatement.expression == null
                            ? null
                            : EvaluateExpression(returnStatement.expression);

                        return lastValue_;
                    default:
                        throw new Exception($"EvaluateStatement: unexpected statement '{s.type}'");
                }
            }

            return lastValue_;
        } catch (Exception e) {
            var previous = Console.ForegroundColor;
            Console.ForegroundColor = ConsoleColor.Red;
            Console.Write("Unhandled exception: ");
            Console.ForegroundColor = previous;
            Console.WriteLine(e.Message);
            return new EvaluatorObject(null);
        }
    }

    private void EvaluateExpressionStatement(BoundExpressionStatement statement) {
        lastValue_ = EvaluateExpression(statement.expression);
    }

    private void EvaluateVariableDeclarationStatement(BoundVariableDeclarationStatement statement) {
        var value = EvaluateExpression(statement.initializer);
        lastValue_ = null;
        Assign(statement.variable, value);
    }

    private void Assign(VariableSymbol variable, EvaluatorObject value) {
        if (variable.type == SymbolType.GlobalVariable) {
            var currentValue = globals_.ContainsKey(variable) ? globals_[variable] : null;

            if (currentValue != null && currentValue.isReference && !value.isReference)
                Assign(currentValue.reference, value);
            else
                globals_[variable] = value;
        } else {
            var locals = locals_.Peek();
            var currentValue = locals.ContainsKey(variable) ? locals[variable] : null;

            if (currentValue != null && currentValue.isReference && !value.isReference)
                Assign(currentValue.reference, value);
            else
                locals[variable] = value;
        }
    }

    private EvaluatorObject EvaluateExpression(BoundExpression node) {
        if (node.constantValue != null)
            return EvaluateConstantExpression(node);

        switch (node.type) {
            case BoundNodeType.LiteralExpression:
                if (node is BoundInitializerListExpression il)
                    return new EvaluatorObject(EvaluateInitializerListExpression(il));
                else
                    goto default;
            case BoundNodeType.VariableExpression:
                return EvaluateVariableExpression((BoundVariableExpression)node);
            case BoundNodeType.AssignmentExpression:
                return EvaluateAssignmentExpresion((BoundAssignmentExpression)node);
            case BoundNodeType.UnaryExpression:
                return EvaluateUnaryExpression((BoundUnaryExpression)node);
            case BoundNodeType.BinaryExpression:
                return EvaluateBinaryExpression((BoundBinaryExpression)node);
            case BoundNodeType.CallExpression:
                return EvaluateCallExpression((BoundCallExpression)node);
            case BoundNodeType.CastExpression:
                return EvaluateCastExpression((BoundCastExpression)node);
            case BoundNodeType.IndexExpression:
                return EvaluateIndexExpression((BoundIndexExpression)node);
            case BoundNodeType.ReferenceExpression:
                return EvaluateReferenceExpression((BoundReferenceExpression)node);
            case BoundNodeType.TypeofExpression:
                return EvaluateTypeofExpression((BoundTypeofExpression)node);
            case BoundNodeType.EmptyExpression:
                return new EvaluatorObject(null);
            default:
                throw new Exception($"EvaluateExpression: unexpected node '{node.type}'");
        }
    }

    private EvaluatorObject EvaluateTypeofExpression(BoundTypeofExpression node) {
        // TODO Implement typeof and type types
        return new EvaluatorObject(null);
    }

    private EvaluatorObject EvaluateReferenceExpression(BoundReferenceExpression node) {
        return new EvaluatorObject(node.variable);
    }

    private EvaluatorObject EvaluateIndexExpression(BoundIndexExpression node) {
        var variable = EvaluateExpression(node.expression);
        var index = EvaluateExpression(node.index);

        return ((EvaluatorObject[])Value(variable))[(int)Value(index)];
    }

    private EvaluatorObject[] EvaluateInitializerListExpression(BoundInitializerListExpression node) {
        var builder = new List<EvaluatorObject>();

        foreach (var item in node.items) {
            EvaluatorObject value = EvaluateExpression(item);
            builder.Add(value);
        }

        return builder.ToArray();
    }

    private EvaluatorObject EvaluateCastExpression(BoundCastExpression node) {
        var value = EvaluateExpression(node.expression);

        return EvaluateCast(value, node.typeClause);
    }

    private EvaluatorObject EvaluateCast(EvaluatorObject value, BoundTypeClause typeClause) {
        if (Value(value) == null)
            return new EvaluatorObject(null);

        var type = typeClause.lType;

        if (type == TypeSymbol.Any) {
            return value;
        } else if (type == TypeSymbol.Bool) {
            return new EvaluatorObject(Convert.ToBoolean(Value(value)));
        } else if (type == TypeSymbol.Int) {
            if (Value(value).IsFloatingPoint())
                value = new EvaluatorObject(Math.Truncate(Convert.ToDouble(Value(value))));

            return new EvaluatorObject(Convert.ToInt32(Value(value)));
        } else if (type == TypeSymbol.String) {
            return new EvaluatorObject(Convert.ToString(Value(value)));
        } else if (type == TypeSymbol.Decimal) {
            return new EvaluatorObject(Convert.ToDouble(Value(value)));
        }

        throw new Exception($"EvaluateCast: unexpected type '{typeClause}'");
    }

    private EvaluatorObject EvaluateCallExpression(BoundCallExpression node) {
        if (MethodsMatch(node.function, BuiltinFunctions.Input)) {
            return new EvaluatorObject(Console.ReadLine());
        } else if (MethodsMatch(node.function, BuiltinFunctions.Print)) {
            var message = EvaluateExpression(node.arguments[0]);
            Console.Write(Value(message));
            hasPrint = true;
        } else if (MethodsMatch(node.function, BuiltinFunctions.PrintLine)) {
            var message = EvaluateExpression(node.arguments[0]);
            Console.WriteLine(Value(message));
        } else if (MethodsMatch(node.function, BuiltinFunctions.RandInt)) {
            var max = (int)Value(EvaluateExpression(node.arguments[0]));

            if (random_ == null)
                random_ = new Random();

            return new EvaluatorObject(random_.Next(max));
        } else if (node.function.name == "Value") {
            EvaluatorObject? value = EvaluateExpression(node.arguments[0]);

            if (Value(value) == null)
                throw new NullReferenceException();

            return new EvaluatorObject(Value(value));
        } else if (MethodsMatch(node.function, BuiltinFunctions.HasValue)) {
            EvaluatorObject? value = EvaluateExpression(node.arguments[0]);

            if (Value(value) == null)
                return new EvaluatorObject(false);

            return new EvaluatorObject(true);
        } else {
            var locals = new Dictionary<VariableSymbol, EvaluatorObject>();

            for (int i=0; i<node.arguments.Length; i++) {
                var parameter = node.function.parameters[i];
                var value = EvaluateExpression(node.arguments[i]);
                locals.Add(parameter, value);
            }

            locals_.Push(locals);
            var statement = LookupMethod(node.function);
            var result = EvaluateStatement(statement);
            locals_.Pop();

            return result;
        }

        return new EvaluatorObject(null);
    }

    private BoundBlockStatement LookupMethod(FunctionSymbol function) {
        foreach (var pair in functions_)
            if (MethodsMatch(pair.Key, function))
                return pair.Value;

        throw new Exception($"LookupMethod: could not find method '{function.name}'");
    }

    private bool MethodsMatch(FunctionSymbol left, FunctionSymbol right) {
        if (left.name == right.name && left.parameters.Length == right.parameters.Length) {
            var parametersMatch = true;

            for (int i=0; i<left.parameters.Length; i++) {
                var checkParameter = left.parameters[i];
                var parameter = right.parameters[i];

                if (checkParameter.name != parameter.name || checkParameter.typeClause != parameter.typeClause)
                    parametersMatch = false;
            }

            if (parametersMatch)
                return true;
        }

        return false;
    }

    private EvaluatorObject EvaluateConstantExpression(BoundExpression syntax) {
        return EvaluateCast(new EvaluatorObject(syntax.constantValue.value), syntax.typeClause);
    }

    private EvaluatorObject EvaluateVariableExpression(BoundVariableExpression syntax) {
        if (syntax.variable.type == SymbolType.GlobalVariable)
            return globals_[syntax.variable];

        var locals = locals_.Peek();
        return locals[syntax.variable];
    }

    private EvaluatorObject EvaluateAssignmentExpresion(BoundAssignmentExpression syntax) {
        var value = EvaluateExpression(syntax.expression);
        Assign(syntax.variable, value);

        return value;
    }

    private EvaluatorObject EvaluateUnaryExpression(BoundUnaryExpression syntax) {
        var operand = EvaluateExpression(syntax.operand);

        if (Value(operand) == null)
            return new EvaluatorObject(null);

        switch (syntax.op.opType) {
            case BoundUnaryOperatorType.NumericalIdentity:
                if (syntax.operand.typeClause.lType == TypeSymbol.Int)
                    return new EvaluatorObject((int)Value(operand));
                else
                    return new EvaluatorObject((double)Value(operand));
            case BoundUnaryOperatorType.NumericalNegation:
                if (syntax.operand.typeClause.lType == TypeSymbol.Int)
                    return new EvaluatorObject(-(int)Value(operand));
                else
                    return new EvaluatorObject(-(double)Value(operand));
            case BoundUnaryOperatorType.BooleanNegation:
                return new EvaluatorObject(!(bool)Value(operand));
            case BoundUnaryOperatorType.BitwiseCompliment:
                return new EvaluatorObject(~(int)Value(operand));
            default:
                throw new Exception($"EvaluateUnaryExpression: unknown unary operator '{syntax.op}'");
        }
    }

    private EvaluatorObject EvaluateBinaryExpression(BoundBinaryExpression syntax) {
        var left = EvaluateExpression(syntax.left);
        var right = EvaluateExpression(syntax.right);

        var leftValue = Value(left);
        var rightValue = Value(right);

        if (leftValue == null || rightValue == null)
            return new EvaluatorObject(null);

        var syntaxType = syntax.typeClause.lType;
        var leftType = syntax.left.typeClause.lType;

        switch (syntax.op.opType) {
            case BoundBinaryOperatorType.Addition:
                if (syntaxType == TypeSymbol.Int)
                    return new EvaluatorObject((int)leftValue + (int)rightValue);
                else if (syntaxType == TypeSymbol.String)
                    return new EvaluatorObject((string)leftValue + (string)rightValue);
                else
                    return new EvaluatorObject((double)leftValue + (double)rightValue);
            case BoundBinaryOperatorType.Subtraction:
                if (syntaxType == TypeSymbol.Int)
                    return new EvaluatorObject((int)leftValue - (int)rightValue);
                else
                    return new EvaluatorObject((double)leftValue - (double)rightValue);
            case BoundBinaryOperatorType.Multiplication:
                if (syntaxType == TypeSymbol.Int)
                    return new EvaluatorObject((int)leftValue * (int)rightValue);
                else
                    return new EvaluatorObject((double)leftValue * (double)rightValue);
            case BoundBinaryOperatorType.Division:
                if (syntaxType == TypeSymbol.Int)
                    return new EvaluatorObject((int)leftValue / (int)rightValue);
                else
                    return new EvaluatorObject((double)leftValue / (double)rightValue);
            case BoundBinaryOperatorType.Power:
                if (syntaxType == TypeSymbol.Int)
                    return new EvaluatorObject((int)Math.Pow((int)leftValue, (int)rightValue));
                else
                    return new EvaluatorObject((double)Math.Pow((double)leftValue, (double)rightValue));
            case BoundBinaryOperatorType.ConditionalAnd:
                return new EvaluatorObject((bool)leftValue && (bool)rightValue);
            case BoundBinaryOperatorType.ConditionalOr:
                return new EvaluatorObject((bool)leftValue || (bool)rightValue);
            case BoundBinaryOperatorType.EqualityEquals:
                return new EvaluatorObject(Equals(leftValue, rightValue));
            case BoundBinaryOperatorType.EqualityNotEquals:
                return new EvaluatorObject(!Equals(leftValue, rightValue));
            case BoundBinaryOperatorType.LessThan:
                if (leftType == TypeSymbol.Int)
                    return new EvaluatorObject((int)leftValue < (int)rightValue);
                else
                    return new EvaluatorObject((double)leftValue < (double)rightValue);
            case BoundBinaryOperatorType.GreaterThan:
                if (leftType == TypeSymbol.Int)
                    return new EvaluatorObject((int)leftValue > (int)rightValue);
                else
                    return new EvaluatorObject((double)leftValue > (double)rightValue);
            case BoundBinaryOperatorType.LessOrEqual:
                if (leftType == TypeSymbol.Int)
                    return new EvaluatorObject((int)leftValue <= (int)rightValue);
                else
                    return new EvaluatorObject((double)leftValue <= (double)rightValue);
            case BoundBinaryOperatorType.GreatOrEqual:
                if (leftType == TypeSymbol.Int)
                    return new EvaluatorObject((int)leftValue >= (int)rightValue);
                else
                    return new EvaluatorObject((double)leftValue >= (double)rightValue);
            case BoundBinaryOperatorType.LogicalAnd:
                if (syntaxType == TypeSymbol.Int)
                    return new EvaluatorObject((int)leftValue & (int)rightValue);
                else
                    return new EvaluatorObject((bool)leftValue & (bool)rightValue);
            case BoundBinaryOperatorType.LogicalOr:
                if (syntaxType == TypeSymbol.Int)
                    return new EvaluatorObject((int)leftValue | (int)rightValue);
                else
                    return new EvaluatorObject((bool)leftValue | (bool)rightValue);
            case BoundBinaryOperatorType.LogicalXor:
                if (syntaxType == TypeSymbol.Int)
                    return new EvaluatorObject((int)leftValue ^ (int)rightValue);
                else
                    return new EvaluatorObject((bool)leftValue ^ (bool)rightValue);
            case BoundBinaryOperatorType.LeftShift:
                return new EvaluatorObject((int)leftValue << (int)rightValue);
            case BoundBinaryOperatorType.RightShift:
                return new EvaluatorObject((int)leftValue >> (int)rightValue);
            case BoundBinaryOperatorType.UnsignedRightShift:
                return new EvaluatorObject((int)leftValue >>> (int)rightValue);
            case BoundBinaryOperatorType.Modulo:
                if (syntaxType == TypeSymbol.Int)
                    return new EvaluatorObject((int)leftValue % (int)rightValue);
                else
                    return new EvaluatorObject((double)leftValue % (double)rightValue);
            default:
                throw new Exception($"EvaluateBinaryExpression: unknown binary operator '{syntax.op}'");
        }
    }
}
