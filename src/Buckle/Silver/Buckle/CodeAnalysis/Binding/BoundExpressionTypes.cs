using System;
using System.Collections.Immutable;
using Buckle.CodeAnalysis.Syntax;
using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

internal enum BoundBinaryOperatorType {
    Invalid,
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Power,
    LogicalAnd,
    LogicalOr,
    LogicalXor,
    LeftShift,
    RightShift,
    ConditionalAnd,
    ConditionalOr,
    EqualityEquals,
    EqualityNotEquals,
    LessThan,
    GreaterThan,
    LessOrEqual,
    GreatOrEqual,
}

internal sealed class BoundBinaryOperator {
    public SyntaxType type { get; }
    public BoundBinaryOperatorType opType { get; }
    public TypeSymbol leftType { get; }
    public TypeSymbol rightType { get; }
    public TypeSymbol resultType { get; }

    private BoundBinaryOperator(
        SyntaxType type_, BoundBinaryOperatorType opType_,
        TypeSymbol leftType_, TypeSymbol rightType_, TypeSymbol resultType_) {
        type = type_;
        opType = opType_;
        leftType = leftType_;
        rightType = rightType_;
        resultType = resultType_;
    }

    private BoundBinaryOperator(
        SyntaxType type, BoundBinaryOperatorType opType, TypeSymbol operandType, TypeSymbol resultType)
        : this(type, opType, operandType, operandType, resultType) { }

    private BoundBinaryOperator(SyntaxType type, BoundBinaryOperatorType opType, TypeSymbol lType)
        : this(type, opType, lType, lType, lType) { }

    internal static BoundBinaryOperator[] operators_ = {
        // integers
        new BoundBinaryOperator(SyntaxType.PLUS_TOKEN, BoundBinaryOperatorType.Addition, TypeSymbol.Int),
        new BoundBinaryOperator(SyntaxType.MINUS_TOKEN, BoundBinaryOperatorType.Subtraction, TypeSymbol.Int),
        new BoundBinaryOperator(SyntaxType.ASTERISK_TOKEN, BoundBinaryOperatorType.Multiplication, TypeSymbol.Int),
        new BoundBinaryOperator(SyntaxType.SLASH_TOKEN, BoundBinaryOperatorType.Division, TypeSymbol.Int),
        new BoundBinaryOperator(SyntaxType.ASTERISK_ASTERISK_TOKEN, BoundBinaryOperatorType.Power, TypeSymbol.Int),
        new BoundBinaryOperator(SyntaxType.AMPERSAND_TOKEN, BoundBinaryOperatorType.LogicalAnd, TypeSymbol.Int),
        new BoundBinaryOperator(SyntaxType.PIPE_TOKEN, BoundBinaryOperatorType.LogicalOr, TypeSymbol.Int),
        new BoundBinaryOperator(SyntaxType.CARET_TOKEN, BoundBinaryOperatorType.LogicalXor, TypeSymbol.Int),
        new BoundBinaryOperator(
            SyntaxType.LESS_THAN_LESS_THAN_TOKEN, BoundBinaryOperatorType.LeftShift, TypeSymbol.Int),
        new BoundBinaryOperator(
            SyntaxType.GREATER_THAN_GREATER_THAN_TOKEN, BoundBinaryOperatorType.RightShift, TypeSymbol.Int),
        new BoundBinaryOperator(SyntaxType.EQUALS_EQUALS_TOKEN,
            BoundBinaryOperatorType.EqualityEquals, TypeSymbol.Int, TypeSymbol.Bool),
        new BoundBinaryOperator(SyntaxType.EXCLAMATION_EQUALS_TOKEN, BoundBinaryOperatorType.EqualityNotEquals,
            TypeSymbol.Int, TypeSymbol.Bool),
        new BoundBinaryOperator(
            SyntaxType.LESS_THAN_TOKEN, BoundBinaryOperatorType.LessThan, TypeSymbol.Int, TypeSymbol.Bool),
        new BoundBinaryOperator(
            SyntaxType.GREATER_THAN_TOKEN, BoundBinaryOperatorType.GreaterThan, TypeSymbol.Int, TypeSymbol.Bool),
        new BoundBinaryOperator(
            SyntaxType.LESS_THAN_EQUALS_TOKEN, BoundBinaryOperatorType.LessOrEqual, TypeSymbol.Int, TypeSymbol.Bool),
        new BoundBinaryOperator(SyntaxType.GREATER_THAN_EQUALS_TOKEN,
            BoundBinaryOperatorType.GreatOrEqual, TypeSymbol.Int, TypeSymbol.Bool),

        // boolean
        new BoundBinaryOperator(
            SyntaxType.AMPERSAND_AMPERSAND_TOKEN, BoundBinaryOperatorType.ConditionalAnd, TypeSymbol.Bool),
        new BoundBinaryOperator(SyntaxType.PIPE_PIPE_TOKEN, BoundBinaryOperatorType.ConditionalOr, TypeSymbol.Bool),
        new BoundBinaryOperator(SyntaxType.AMPERSAND_TOKEN, BoundBinaryOperatorType.LogicalAnd, TypeSymbol.Bool),
        new BoundBinaryOperator(SyntaxType.PIPE_TOKEN, BoundBinaryOperatorType.LogicalOr, TypeSymbol.Bool),
        new BoundBinaryOperator(SyntaxType.CARET_TOKEN, BoundBinaryOperatorType.LogicalXor, TypeSymbol.Bool),
        new BoundBinaryOperator(
            SyntaxType.EQUALS_EQUALS_TOKEN, BoundBinaryOperatorType.EqualityEquals, TypeSymbol.Bool),
        new BoundBinaryOperator(
            SyntaxType.EXCLAMATION_EQUALS_TOKEN, BoundBinaryOperatorType.EqualityNotEquals, TypeSymbol.Bool),
        new BoundBinaryOperator(SyntaxType.EXCLAMATION_EQUALS_TOKEN, BoundBinaryOperatorType.EqualityNotEquals,
            TypeSymbol.Any, TypeSymbol.Bool),
        new BoundBinaryOperator(SyntaxType.EQUALS_EQUALS_TOKEN, BoundBinaryOperatorType.EqualityEquals,
            TypeSymbol.Any, TypeSymbol.Bool),

        // string
        new BoundBinaryOperator(SyntaxType.PLUS_TOKEN, BoundBinaryOperatorType.Addition, TypeSymbol.String),
        new BoundBinaryOperator(SyntaxType.EQUALS_EQUALS_TOKEN,
            BoundBinaryOperatorType.EqualityEquals, TypeSymbol.String, TypeSymbol.Bool),
        new BoundBinaryOperator(SyntaxType.EXCLAMATION_EQUALS_TOKEN, BoundBinaryOperatorType.EqualityNotEquals,
            TypeSymbol.String, TypeSymbol.Bool),

        // decimal
        new BoundBinaryOperator(SyntaxType.PLUS_TOKEN, BoundBinaryOperatorType.Addition, TypeSymbol.Decimal),
        new BoundBinaryOperator(SyntaxType.MINUS_TOKEN, BoundBinaryOperatorType.Subtraction, TypeSymbol.Decimal),
        new BoundBinaryOperator(SyntaxType.ASTERISK_TOKEN, BoundBinaryOperatorType.Multiplication, TypeSymbol.Decimal),
        new BoundBinaryOperator(SyntaxType.SLASH_TOKEN, BoundBinaryOperatorType.Division, TypeSymbol.Decimal),
        new BoundBinaryOperator(SyntaxType.ASTERISK_ASTERISK_TOKEN, BoundBinaryOperatorType.Power, TypeSymbol.Decimal),
        new BoundBinaryOperator(SyntaxType.EQUALS_EQUALS_TOKEN,
            BoundBinaryOperatorType.EqualityEquals, TypeSymbol.Decimal, TypeSymbol.Bool),
        new BoundBinaryOperator(SyntaxType.EXCLAMATION_EQUALS_TOKEN, BoundBinaryOperatorType.EqualityNotEquals,
            TypeSymbol.Decimal, TypeSymbol.Bool),
        new BoundBinaryOperator(
            SyntaxType.LESS_THAN_TOKEN, BoundBinaryOperatorType.LessThan, TypeSymbol.Decimal, TypeSymbol.Bool),
        new BoundBinaryOperator(
            SyntaxType.GREATER_THAN_TOKEN, BoundBinaryOperatorType.GreaterThan, TypeSymbol.Decimal, TypeSymbol.Bool),
        new BoundBinaryOperator(SyntaxType.LESS_THAN_EQUALS_TOKEN,
            BoundBinaryOperatorType.LessOrEqual, TypeSymbol.Decimal, TypeSymbol.Bool),
        new BoundBinaryOperator(SyntaxType.GREATER_THAN_EQUALS_TOKEN,
            BoundBinaryOperatorType.GreatOrEqual, TypeSymbol.Decimal, TypeSymbol.Bool),

        // any
        new BoundBinaryOperator(SyntaxType.EXCLAMATION_EQUALS_TOKEN, BoundBinaryOperatorType.EqualityNotEquals,
            TypeSymbol.Any, TypeSymbol.Bool),
        new BoundBinaryOperator(SyntaxType.EQUALS_EQUALS_TOKEN, BoundBinaryOperatorType.EqualityEquals,
            TypeSymbol.Any, TypeSymbol.Bool),
    };

    public static BoundBinaryOperator Bind(SyntaxType type, TypeSymbol leftType, TypeSymbol rightType) {
        foreach (var op in operators_) {
            var leftIsCorrect = Cast.Classify(leftType, op.leftType).isImplicit;
            var rightIsCorrect = Cast.Classify(rightType, op.rightType).isImplicit;

            if (op.type == type && leftIsCorrect && rightIsCorrect)
                return op;
        }

        return null;
    }
}

internal sealed class BoundBinaryExpression : BoundExpression {
    public override BoundNodeType type => BoundNodeType.BinaryExpression;
    public override TypeSymbol lType => op.resultType;
    public override BoundConstant constantValue { get; }
    public BoundExpression left { get; }
    public BoundBinaryOperator op { get; }
    public BoundExpression right { get; }

    public BoundBinaryExpression(BoundExpression left_, BoundBinaryOperator op_, BoundExpression right_) {
        left = left_;
        op = op_;
        right = right_;
        constantValue = ConstantFolding.Fold(left, op, right);
    }
}

internal enum BoundUnaryOperatorType {
    Invalid,
    NumericalIdentity,
    NumericalNegation,
    BooleanNegation,
    BitwiseCompliment,
}

internal sealed class BoundUnaryOperator {
    public SyntaxType type { get; }
    public BoundUnaryOperatorType opType { get; }
    public TypeSymbol operandType { get; }
    public TypeSymbol resultType { get; }

    private BoundUnaryOperator(
        SyntaxType type_, BoundUnaryOperatorType opType_, TypeSymbol operandType_, TypeSymbol resultType_) {
        type = type_;
        opType = opType_;
        operandType = operandType_;
        resultType = resultType_;
    }

    private BoundUnaryOperator(SyntaxType type, BoundUnaryOperatorType opType, TypeSymbol operandType)
        : this(type, opType, operandType, operandType) { }

    internal static BoundUnaryOperator[] operators_ = {
        // integer
        new BoundUnaryOperator(SyntaxType.PLUS_TOKEN, BoundUnaryOperatorType.NumericalIdentity, TypeSymbol.Int),
        new BoundUnaryOperator(SyntaxType.MINUS_TOKEN, BoundUnaryOperatorType.NumericalNegation, TypeSymbol.Int),
        new BoundUnaryOperator(SyntaxType.TILDE_TOKEN, BoundUnaryOperatorType.BitwiseCompliment, TypeSymbol.Int),

        // boolean
        new BoundUnaryOperator(SyntaxType.EXCLAMATION_TOKEN, BoundUnaryOperatorType.BooleanNegation, TypeSymbol.Bool),

        // decimal
        new BoundUnaryOperator(SyntaxType.PLUS_TOKEN, BoundUnaryOperatorType.NumericalIdentity, TypeSymbol.Decimal),
        new BoundUnaryOperator(SyntaxType.MINUS_TOKEN, BoundUnaryOperatorType.NumericalNegation, TypeSymbol.Decimal),
    };

    public static BoundUnaryOperator Bind(SyntaxType type, TypeSymbol operandType) {
        foreach (var op in operators_) {
            var operandIsCorrect = Cast.Classify(operandType, op.operandType).isImplicit;

            if (op.type == type && operandIsCorrect)
                return op;
        }

        return null;
    }
}

internal sealed class BoundUnaryExpression : BoundExpression {
    public override BoundNodeType type => BoundNodeType.UnaryExpression;
    public override TypeSymbol lType => op.resultType;
    public override BoundConstant constantValue { get; }
    public BoundUnaryOperator op { get; }
    public BoundExpression operand { get; }

    public BoundUnaryExpression(BoundUnaryOperator op_, BoundExpression operand_) {
        op = op_;
        operand = operand_;
        constantValue = ConstantFolding.ComputeConstant(op, operand);
    }
}

internal abstract class BoundExpression : BoundNode {
    public abstract TypeSymbol lType { get; }
    public virtual BoundConstant constantValue => null;
}

internal sealed class BoundLiteralExpression : BoundExpression {
    public override BoundNodeType type => BoundNodeType.LiteralExpression;
    public override TypeSymbol lType { get; }
    public override BoundConstant constantValue { get; }
    public object value => constantValue.value;

    public BoundLiteralExpression(object value_) {
        if (value_ is bool)
            lType = TypeSymbol.Bool;
        else if (value_ is int)
            lType = TypeSymbol.Int;
        else if (value_ is string)
            lType = TypeSymbol.String;
        else if (value_ is float)
            lType = TypeSymbol.Decimal;
        else if (value_ == null)
            lType = null;
        else
            throw new Exception($"unexpected literal '{value_}' of type '{value_.GetType()}'");

        constantValue = new BoundConstant(value_);
    }
}

internal sealed class BoundVariableExpression : BoundExpression {
    public VariableSymbol variable { get; }
    public override TypeSymbol lType => variable.lType;
    public override BoundNodeType type => BoundNodeType.VariableExpression;
    public override BoundConstant constantValue => variable.constantValue;

    public BoundVariableExpression(VariableSymbol variable_) {
        variable = variable_;
    }
}

internal sealed class BoundAssignmentExpression : BoundExpression {
    public VariableSymbol variable { get; }
    public BoundExpression expression { get; }
    public override BoundNodeType type => BoundNodeType.AssignmentExpression;
    public override TypeSymbol lType => expression.lType;

    public BoundAssignmentExpression(VariableSymbol variable_, BoundExpression expression_) {
        variable = variable_;
        expression = expression_;
    }
}

internal sealed class BoundEmptyExpression : BoundExpression {
    public override BoundNodeType type => BoundNodeType.EmptyExpression;
    public override TypeSymbol lType => null;

    public BoundEmptyExpression() { }
}

internal sealed class BoundErrorExpression : BoundExpression {
    public override BoundNodeType type => BoundNodeType.ErrorExpression;
    public override TypeSymbol lType => TypeSymbol.Error;

    public BoundErrorExpression() { }
}

internal sealed class BoundCallExpression : BoundExpression {
    public FunctionSymbol function { get; }
    public ImmutableArray<BoundExpression> arguments { get; }
    public override BoundNodeType type => BoundNodeType.CallExpression;
    public override TypeSymbol lType => function.lType;

    public BoundCallExpression(FunctionSymbol function_, ImmutableArray<BoundExpression> arguments_) {
        function = function_;
        arguments = arguments_;
    }
}

internal sealed class BoundIndexExpression : BoundExpression {
    public VariableSymbol variable { get; }
    public BoundExpression index { get; }
    public override BoundNodeType type => BoundNodeType.IndexExpression;
    public override TypeSymbol lType => ((CollectionTypeSymbol)variable.lType).itemType;

    public BoundIndexExpression(VariableSymbol variable_, BoundExpression index_) {
        variable = variable_;
        index = index_;
    }
}

internal sealed class BoundInitializerListExpression : BoundExpression {
    public ImmutableArray<BoundExpression> items { get; }
    public TypeSymbol itemLType { get; }
    public override BoundNodeType type => BoundNodeType.LiteralExpression;
    public override TypeSymbol lType => TypeSymbol.Collection;

    public BoundInitializerListExpression(ImmutableArray<BoundExpression> items_, TypeSymbol itemLType_) {
        items = items_;
        itemLType = itemLType_;
    }
}

internal sealed class BoundCastExpression : BoundExpression {
    public BoundExpression expression { get; }
    public override BoundNodeType type => BoundNodeType.CastExpression;
    public override TypeSymbol lType { get; }

    public BoundCastExpression(TypeSymbol lType_, BoundExpression expression_) {
        lType = lType_;
        expression = expression_;
    }
}

internal sealed class BoundCompoundAssignmentExpression : BoundExpression {
    public VariableSymbol variable { get; }
    public BoundBinaryOperator op { get; }
    public BoundExpression expression { get; }
    public override BoundNodeType type => BoundNodeType.CompoundAssignmentExpression;
    public override TypeSymbol lType => expression.lType;

    public BoundCompoundAssignmentExpression(
        VariableSymbol variable_, BoundBinaryOperator op_, BoundExpression expression_) {
        variable = variable_;
        op = op_;
        expression = expression_;
    }
}
