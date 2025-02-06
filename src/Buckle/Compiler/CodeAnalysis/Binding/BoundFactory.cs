using System.Collections.Immutable;
using System.Linq;
using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;
using Buckle.Libraries;

namespace Buckle.CodeAnalysis.Binding;

internal static partial class BoundFactory {
    internal static BoundNopStatement Nop() {
        return new BoundNopStatement(null);
    }

    internal static BoundLiteralExpression Literal(SyntaxNode syntax, object value, TypeSymbol type = null) {
        if (type is not null)
            return new BoundLiteralExpression(syntax, new ConstantValue(value, type.specialType), type);

        var specialType = LiteralUtilities.AssumeTypeFromLiteral(value);
        return new BoundLiteralExpression(
            syntax,
            new ConstantValue(value, specialType),
            CorLibrary.GetSpecialType(specialType)
        );
    }

    internal static BoundBlockStatement Block(SyntaxNode syntax, params BoundStatement[] statements) {
        return new BoundBlockStatement(syntax, ImmutableArray.Create(statements), [], []);
    }

    internal static BoundLabelStatement Label(SyntaxNode syntax, LabelSymbol label) {
        return new BoundLabelStatement(syntax, label);
    }

    internal static BoundGotoStatement Goto(SyntaxNode syntax, LabelSymbol label) {
        return new BoundGotoStatement(syntax, label);
    }

    internal static BoundConditionalGotoStatement GotoIf(SyntaxNode syntax, LabelSymbol @goto, BoundExpression @if) {
        return new BoundConditionalGotoStatement(syntax, @goto, @if, true);
    }

    internal static BoundConditionalGotoStatement GotoIfNot(
        SyntaxNode syntax,
        LabelSymbol @goto,
        BoundExpression @ifNot) {
        return new BoundConditionalGotoStatement(syntax, @goto, @ifNot, false);
    }

    internal static BoundExpressionStatement Statement(SyntaxNode syntax, BoundExpression expression) {
        return new BoundExpressionStatement(syntax, expression);
    }

    internal static BoundWhileStatement While(
        SyntaxNode syntax,
        BoundExpression condition,
        BoundStatement body,
        SynthesizedLabelSymbol breakLabel,
        SynthesizedLabelSymbol continueLabel) {
        return new BoundWhileStatement(syntax, condition, body, breakLabel, continueLabel);
    }

    internal static BoundCallExpression Call(
        SyntaxNode syntax,
        MethodSymbol method,
        params BoundExpression[] arguments) {
        return new BoundCallExpression(
            syntax,
            null,
            method,
            ImmutableArray.Create(arguments),
            ImmutableArray.CreateRange(Enumerable.Repeat(RefKind.None, arguments.Length)),
            BitVector.Empty,
            LookupResultKind.Viable,
            method.returnType
        );
    }

    internal static BoundCastExpression Cast(
        SyntaxNode syntax,
        TypeSymbol type,
        BoundExpression expression,
        Conversion conversion,
        ConstantValue constant) {
        return new BoundCastExpression(syntax, expression, conversion, constant, type);
    }

    internal static BoundConditionalOperator Conditional(
        SyntaxNode syntax,
        BoundExpression @if,
        BoundExpression @then,
        BoundExpression @else,
        TypeSymbol type) {
        return new BoundConditionalOperator(syntax, @if, false, @then, @else, null, type);
    }

    internal static BoundAssignmentOperator Assignment(
        SyntaxNode syntax,
        BoundExpression left,
        BoundExpression right,
        bool isRef,
        TypeSymbol type) {
        return new BoundAssignmentOperator(syntax, left, right, isRef, type);
    }

    internal static BoundCompoundAssignmentOperator Increment(SyntaxNode syntax, BoundExpression operand) {
        var opKind = OverloadResolution.BinOpEasyOut.OpKind(BinaryOperatorKind.Addition, operand.type, operand.type);
        var opSignature = new BinaryOperatorSignature(opKind, operand.type, operand.type, operand.type);
        return new BoundCompoundAssignmentOperator(
            syntax,
            operand,
            Literal(syntax, 1, operand.type),
            opSignature,
            null,
            null,
            null,
            null,
            LookupResultKind.Viable,
            [],
            operand.type
        );
    }

    internal static BoundCompoundAssignmentOperator Decrement(SyntaxNode syntax, BoundExpression operand) {
        var opKind = OverloadResolution.BinOpEasyOut.OpKind(BinaryOperatorKind.Subtraction, operand.type, operand.type);
        var opSignature = new BinaryOperatorSignature(opKind, operand.type, operand.type, operand.type);
        return new BoundCompoundAssignmentOperator(
            syntax,
            operand,
            Literal(syntax, 1, operand.type),
            opSignature,
            null,
            null,
            null,
            null,
            LookupResultKind.Viable,
            [],
            operand.type
        );
    }

    internal static BoundUnaryOperator Unary(
        SyntaxNode syntax,
        UnaryOperatorKind opKind,
        BoundExpression operand,
        TypeSymbol type) {
        return new BoundUnaryOperator(syntax, operand, opKind, null, type);
    }

    internal static BoundBinaryOperator Binary(
        SyntaxNode syntax,
        BoundExpression left,
        BinaryOperatorKind opKind,
        BoundExpression right,
        TypeSymbol type) {
        return new BoundBinaryOperator(syntax, left, right, opKind, null, type);
    }

    internal static BoundBinaryOperator And(SyntaxNode syntax, BoundExpression left, BoundExpression right) {
        return new BoundBinaryOperator(
            syntax,
            left,
            right,
            BinaryOperatorKind.BoolConditionalAnd,
            null,
            CorLibrary.GetSpecialType(SpecialType.Bool)
        );
    }

    internal static BoundNullAssertOperator Value(SyntaxNode syntax, BoundExpression expression, TypeSymbol type) {
        return new BoundNullAssertOperator(syntax, expression, null, type);
    }

    internal static BoundIsOperator HasValue(SyntaxNode syntax, BoundExpression expression) {
        return new BoundIsOperator(
            syntax,
            expression,
            Literal(syntax, null, expression.type),
            true,
            null,
            CorLibrary.GetSpecialType(SpecialType.Bool)
        );
    }
}
