using System.Collections.Immutable;
using System.Linq;
using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;
using Buckle.Libraries;

namespace Buckle.CodeAnalysis.Binding;

internal static partial class BoundFactory {
    internal static BoundNopStatement Nop() {
        return new BoundNopStatement();
    }

    internal static BoundLiteralExpression Literal(object value, TypeSymbol type = null) {
        if (type is not null)
            return new BoundLiteralExpression(value, type);

        var specialType = LiteralUtilities.AssumeTypeFromLiteral(value);
        return new BoundLiteralExpression(value, CorLibrary.GetSpecialType(specialType));
    }

    internal static BoundBlockStatement Block(params BoundStatement[] statements) {
        return new BoundBlockStatement(ImmutableArray.Create(statements), [], []);
    }

    internal static BoundBlockStatement Block() {
        return new BoundBlockStatement([], [], []);
    }

    internal static BoundLabelStatement Label(LabelSymbol label) {
        return new BoundLabelStatement(label);
    }

    internal static BoundGotoStatement Goto(LabelSymbol label) {
        return new BoundGotoStatement(label);
    }

    internal static BoundConditionalGotoStatement GotoIf(LabelSymbol @goto, BoundExpression @if) {
        return new BoundConditionalGotoStatement(@goto, @if, true);
    }

    internal static BoundConditionalGotoStatement GotoIfNot(LabelSymbol @goto, BoundExpression @ifNot) {
        return new BoundConditionalGotoStatement(@goto, @ifNot, false);
    }

    internal static BoundExpressionStatement Statement(BoundExpression expression) {
        return new BoundExpressionStatement(expression);
    }

    internal static BoundWhileStatement While(
        BoundExpression condition,
        BoundStatement body,
        SynthesizedLabelSymbol breakLabel,
        SynthesizedLabelSymbol continueLabel) {
        return new BoundWhileStatement(condition, body, breakLabel, continueLabel);
    }

    internal static BoundCallExpression Call(MethodSymbol method, params BoundExpression[] arguments) {
        return new BoundCallExpression(new BoundEmptyExpression(), method, ImmutableArray.Create(arguments));
    }

    internal static BoundCastExpression Cast(
        TypeSymbol type,
        BoundExpression expression,
        ConversionKind conversionKind,
        ConstantValue constant) {
        return new BoundCastExpression(type, expression, conversionKind, constant);
    }

    internal static BoundMemberAccessExpression MemberAccess(
        BoundExpression left,
        BoundExpression right,
        bool isStaticAccess = false) {
        return new BoundMemberAccessExpression(left, right, false, isStaticAccess);
    }

    internal static BoundArrayAccessExpression Index(BoundExpression operand, BoundExpression index) {
        return new BoundIndexExpression(operand, index, false);
    }

    internal static BoundTernaryExpression NullConditional(
        BoundExpression @if, BoundExpression @then, BoundExpression @else) {
        var op = BoundTernaryOperator.Bind(
            SyntaxKind.QuestionToken, SyntaxKind.ColonToken, @if.type, @then.type, @else.type
        );

        return new BoundTernaryExpression(@if, op, @then, @else);
    }

    internal static BoundAssignmentExpression Assignment(BoundExpression left, BoundExpression right) {
        return new BoundAssignmentExpression(left, right);
    }

    internal static BoundCompoundAssignmentExpression Increment(BoundExpression operand) {
        var value = new BoundTypeWrapper(BoundType.Int, new ConstantValue(1));
        var op = BoundBinaryOperator.Bind(SyntaxKind.PlusToken, operand.type, value.type);
        return new BoundCompoundAssignmentExpression(operand, op, value);
    }

    internal static BoundCompoundAssignmentExpression Decrement(BoundExpression operand) {
        var value = new BoundTypeWrapper(BoundType.Int, new ConstantValue(1));
        var op = BoundBinaryOperator.Bind(SyntaxKind.MinusToken, operand.type, value.type);
        return new BoundCompoundAssignmentExpression(operand, op, value);
    }

    internal static BoundUnaryExpression Unary(BoundUnaryOperator op, BoundExpression operand) {
        return new BoundUnaryExpression(op, operand);
    }

    internal static BoundUnaryExpression Not(BoundExpression operand) {
        var op = BoundUnaryOperator.Bind(SyntaxKind.ExclamationToken, operand.type);
        return new BoundUnaryExpression(op, operand);
    }

    internal static BoundBinaryExpression Binary(BoundExpression left, BoundBinaryOperator op, BoundExpression right) {
        return new BoundBinaryExpression(left, op, right);
    }

    internal static BoundBinaryExpression Add(BoundExpression left, BoundExpression right) {
        var op = BoundBinaryOperator.Bind(SyntaxKind.PlusToken, left.type, right.type);
        return new BoundBinaryExpression(left, op, right);
    }

    internal static BoundBinaryExpression Subtract(BoundExpression left, BoundExpression right) {
        var op = BoundBinaryOperator.Bind(SyntaxKind.MinusToken, left.type, right.type);
        return new BoundBinaryExpression(left, op, right);
    }

    internal static BoundBinaryExpression And(BoundExpression left, BoundExpression right) {
        var op = BoundBinaryOperator.Bind(SyntaxKind.AmpersandAmpersandToken, left.type, right.type);
        return new BoundBinaryExpression(left, op, right);
    }

    internal static BoundExpression Value(BoundExpression expression) {
        var op = BoundPostfixOperator.Operators.First(o => o.opKind == BoundPostfixOperatorKind.NullAssert);
        return new BoundPostfixExpression(expression, op, false);
    }

    internal static BoundExpression HasValue(BoundExpression expression) {
        var op = BoundBinaryOperator.Operators.First(o => o.opKind == BoundBinaryOperatorKind.Isnt);
        return new BoundBinaryExpression(expression, op, new BoundLiteralExpression(value: null, expression.type));
    }
}
