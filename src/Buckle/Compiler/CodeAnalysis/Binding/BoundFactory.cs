using System.Collections.Immutable;
using Buckle.CodeAnalysis.Symbols;
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
        Conversion conversion,
        ConstantValue constant) {
        return new BoundCastExpression(type, expression, conversion, constant);
    }

    internal static BoundConditionalExpression Conditional(
        BoundExpression @if,
        BoundExpression @then,
        BoundExpression @else,
        TypeSymbol type) {
        return new BoundConditionalExpression(@if, @then, @else, type);
    }

    internal static BoundAssignmentExpression Assignment(BoundExpression left, BoundExpression right, TypeSymbol type) {
        return new BoundAssignmentExpression(left, right, type);
    }

    internal static BoundCompoundAssignmentExpression Increment(BoundExpression operand) {
        var opKind = BinaryOperatorEasyOut.OpKind(BinaryOperatorKind.Addition, operand.type, operand.type);
        return new BoundCompoundAssignmentExpression(operand, Literal(1, operand.type), opKind, operand.type);
    }

    internal static BoundCompoundAssignmentExpression Decrement(BoundExpression operand) {
        var opKind = BinaryOperatorEasyOut.OpKind(BinaryOperatorKind.Subtraction, operand.type, operand.type);
        return new BoundCompoundAssignmentExpression(operand, Literal(1, operand.type), opKind, operand.type);
    }

    internal static BoundUnaryExpression Unary(UnaryOperatorKind opKind, BoundExpression operand, TypeSymbol type) {
        return new BoundUnaryExpression(operand, opKind, type);
    }

    internal static BoundBinaryExpression Binary(
        BoundExpression left,
        BinaryOperatorKind opKind,
        BoundExpression right,
        TypeSymbol type) {
        return new BoundBinaryExpression(left, right, opKind, type);
    }

    internal static BoundBinaryExpression And(BoundExpression left, BoundExpression right) {
        return new BoundBinaryExpression(
            left,
            right,
            BinaryOperatorKind.BoolConditionalAnd,
            CorLibrary.GetSpecialType(SpecialType.Bool)
        );
    }

    internal static BoundNullAssertExpression Value(BoundExpression expression, TypeSymbol type) {
        return new BoundNullAssertExpression(expression, type);
    }

    internal static BoundIsntExpression HasValue(BoundExpression expression) {
        return new BoundIsntExpression(
            expression,
            Literal(null, expression.type),
            CorLibrary.GetSpecialType(SpecialType.Bool)
        );
    }
}
