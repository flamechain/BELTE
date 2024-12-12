using System.Collections.Generic;
using System.Collections.Immutable;
using System.Text;
using Buckle.CodeAnalysis.Authoring;
using Buckle.CodeAnalysis.Binding;
using Buckle.CodeAnalysis.Symbols;
using Buckle.CodeAnalysis.Syntax;
using Buckle.Utilities;
using static Buckle.CodeAnalysis.Display.DisplayTextSegment;

namespace Buckle.CodeAnalysis.Display;

/// <summary>
/// Represents a piece of text with classifications for segments of text.
/// Can be multiple lines.
/// </summary>
public sealed class DisplayText {
    internal readonly List<DisplayTextSegment> segments;

    private bool _writeIndent = true;

    public DisplayText() {
        segments = [];
        indent = 0;
    }

    /// <summary>
    /// Determines the indentation to use at any given moment.
    /// Usually represented as tabs.
    /// </summary>
    internal int indent { get; set; }

    /// <summary>
    /// Converts the <see cref="DisplayText" /> into a raw string, scrapping all the Classifications.
    /// </summary>
    public override string ToString() {
        var builder = new StringBuilder();

        foreach (var segment in segments)
            builder.Append(segment.text);

        return builder.ToString();
    }

    /// <summary>
    /// Returns the contents of this, and then clears this.
    /// </summary>
    /// <returns>The contents before clearing.</returns>
    public ImmutableArray<DisplayTextSegment> Flush() {
        var array = ImmutableArray.CreateRange(segments);
        segments.Clear();

        return array;
    }

    /// <summary>
    /// Appends a <see cref="DisplayTextSegment" /> to the end of this <see cref="DisplayText" />.
    /// </summary>
    /// <param name="segment"><see cref="DisplayTextSegment" /> to append.</param>
    public void Write(DisplayTextSegment segment) {
        if (_writeIndent) {
            _writeIndent = false;

            for (var i = 0; i < indent; i++)
                segments.Add(CreateIndent());
        }

        segments.Add(segment);

        if (segment.classification == Classification.Line)
            _writeIndent = true;
    }

    /// <summary>
    /// Appends multiple <see cref="DisplayTextSegment" /> to the end of this <see cref="DisplayText" />.
    /// </summary>
    /// <param name="segments"><see cref="DisplayTextSegment" />s to append.</param>
    public void Write(IEnumerable<DisplayTextSegment> segments) {
        foreach (var segment in segments)
            Write(segment);
    }

    /// <summary>
    /// Generates a <see cref="DisplayText" /> off of a single <see cref="BoundNode" />'s tree.
    /// </summary>
    /// <param name="node"><see cref="BoundNode" /> to generate a text from.</param>
    /// <return>Generated text.</return>
    internal static DisplayText DisplayNode(BoundNode node) {
        var text = new DisplayText();
        DisplayNode(text, node);

        return text;
    }

    /// <summary>
    /// Appends a <see cref="BoundNode" /> to the end of an existing <see cref="DisplayText" />.
    /// </summary>
    /// <param name="text">Existing text.</param>
    /// <param name="node"><see cref="BoundNode" /> to append.</param>
    internal static void DisplayNode(DisplayText text, BoundNode node) {
        if (node is BoundExpression be && be.constantValue is not null) {
            DisplayConstant(text, be.constantValue);
            return;
        }

        switch (node.kind) {
            case BoundNodeKind.NopStatement:
                DisplayNopStatement(text);
                break;
            case BoundNodeKind.BlockStatement:
                DisplayBlockStatement(text, (BoundBlockStatement)node);
                break;
            case BoundNodeKind.ExpressionStatement:
                DisplayExpressionStatement(text, (BoundExpressionStatement)node);
                break;
            case BoundNodeKind.LocalDeclarationStatement:
                DisplayLocalDeclarationStatement(text, (BoundLocalDeclarationStatement)node);
                break;
            case BoundNodeKind.IfStatement:
                DisplayIfStatement(text, (BoundIfStatement)node);
                break;
            case BoundNodeKind.WhileStatement:
                DisplayWhileStatement(text, (BoundWhileStatement)node);
                break;
            case BoundNodeKind.ForStatement:
                DisplayForStatement(text, (BoundForStatement)node);
                break;
            case BoundNodeKind.GotoStatement:
                DisplayGotoStatement(text, (BoundGotoStatement)node);
                break;
            case BoundNodeKind.LabelStatement:
                DisplayLabelStatement(text, (BoundLabelStatement)node);
                break;
            case BoundNodeKind.ConditionalGotoStatement:
                DisplayConditionalGotoStatement(text, (BoundConditionalGotoStatement)node);
                break;
            case BoundNodeKind.DoWhileStatement:
                DisplayDoWhileStatement(text, (BoundDoWhileStatement)node);
                break;
            case BoundNodeKind.ReturnStatement:
                DisplayReturnStatement(text, (BoundReturnStatement)node);
                break;
            case BoundNodeKind.TryStatement:
                DisplayTryStatement(text, (BoundTryStatement)node);
                break;
            case BoundNodeKind.TypeExpression:
                DisplayTypeExpression(text, (BoundTypeExpression)node);
                break;
            case BoundNodeKind.BreakStatement:
                DisplayBreakStatement(text);
                break;
            case BoundNodeKind.ContinueStatement:
                DisplayContinueStatement(text);
                break;
            case BoundNodeKind.GlobalStatement:
                DisplayGlobalStatement(text, (BoundGlobalStatement)node);
                break;
            case BoundNodeKind.ArrayAccessExpression:
                DisplayArrayAccessExpression(text, (BoundArrayAccessExpression)node);
                break;
            case BoundNodeKind.ReferenceExpression:
                DisplayReferenceExpression(text, (BoundReferenceExpression)node);
                break;
            case BoundNodeKind.UnaryExpression:
                DisplayUnaryExpression(text, (BoundUnaryExpression)node);
                break;
            case BoundNodeKind.InitializerListExpression:
                DisplayInitializerListExpression(text, (BoundInitializerListExpression)node);
                break;
            case BoundNodeKind.BinaryExpression:
                DisplayBinaryExpression(text, (BoundBinaryExpression)node);
                break;
            case BoundNodeKind.DataContainerExpression:
                DisplayDataContainer(text, (BoundDataContainerExpression)node);
                break;
            case BoundNodeKind.AssignmentExpression:
                DisplayAssignmentExpression(text, (BoundAssignmentExpression)node);
                break;
            case BoundNodeKind.CompoundAssignmentExpression:
                DisplayCompoundAssignmentExpression(text, (BoundCompoundAssignmentExpression)node);
                break;
            case BoundNodeKind.EmptyExpression:
                DisplayEmptyExpression(text);
                break;
            case BoundNodeKind.ErrorExpression:
                DisplayErrorExpression(text, (BoundErrorExpression)node);
                break;
            case BoundNodeKind.CallExpression:
                DisplayCallExpression(text, (BoundCallExpression)node);
                break;
            case BoundNodeKind.CastExpression:
                DisplayCastExpression(text, (BoundCastExpression)node);
                break;
            case BoundNodeKind.TypeOfExpression:
                DisplayTypeOfExpression(text, (BoundTypeOfExpression)node);
                break;
            case BoundNodeKind.ObjectCreationExpression:
                DisplayObjectCreationExpression(text, (BoundObjectCreationExpression)node);
                break;
            case BoundNodeKind.ArrayCreationExpression:
                DisplayArrayCreationExpression(text, (BoundArrayCreationExpression)node);
                break;
            case BoundNodeKind.FieldAccessExpression:
                DisplayFieldAccessExpression(text, (BoundFieldAccessExpression)node);
                break;
            case BoundNodeKind.ConditionalAccessExpression:
                DisplayConditionalAccessExpression(text, (BoundConditionalAccessExpression)node);
                break;
            case BoundNodeKind.ThisExpression:
                DisplayThisExpression(text);
                break;
            case BoundNodeKind.BaseExpression:
                DisplayBaseExpression(text);
                break;
            case BoundNodeKind.ThrowExpression:
                DisplayThrowExpression(text, (BoundThrowExpression)node);
                break;
            case BoundNodeKind.AsExpression:
                DisplayAsExpression(text, (BoundAsExpression)node);
                break;
            case BoundNodeKind.IsExpression:
                DisplayIsExpression(text, (BoundIsExpression)node);
                break;
            case BoundNodeKind.IsntExpression:
                DisplayIsntExpression(text, (BoundIsntExpression)node);
                break;
            case BoundNodeKind.NullCoalescingExpression:
                DisplayNullCoalescingExpression(text, (BoundNullCoalescingExpression)node);
                break;
            case BoundNodeKind.NullAssertExpression:
                DisplayNullAssertExpression(text, (BoundNullAssertExpression)node);
                break;
            case BoundNodeKind.ConditionalExpression:
                DisplayConditionalExpression(text, (BoundConditionalExpression)node);
                break;
            case BoundNodeKind.DataContainerDeclaration:
                DisplayDataContainerDeclaration(text, (BoundDataContainerDeclaration)node);
                break;
            case BoundNodeKind.FieldEqualsValue:
                DisplayFieldEqualsValue(text, (BoundFieldEqualsValue)node);
                break;
            default:
                throw ExceptionUtilities.UnexpectedValue(node.kind);
        }
    }

    /// <summary>
    /// Renders a <see cref="ConstantValue" /> and appends it to the given <see cref="DisplayText" />.
    /// </summary>
    internal static void DisplayConstant(DisplayText text, ConstantValue constant) {
        if (constant.value is ImmutableArray<ConstantValue> il) {
            text.Write(CreatePunctuation(SyntaxKind.OpenBraceToken));
            var isFirst = true;

            foreach (var item in il) {
                if (isFirst) {
                    isFirst = false;
                } else {
                    text.Write(CreatePunctuation(SyntaxKind.CommaToken));
                    text.Write(CreateSpace());
                }

                DisplayConstant(text, item);
            }

            text.Write(CreatePunctuation(SyntaxKind.CloseBraceToken));
        } else {
            DisplayLiteralExpressionCore(text, constant.value);
        }
    }

    /// <summary>
    /// Formats a literal into a string representation.
    /// </summary>
    internal static string FormatLiteral(object value) {
        var text = new DisplayText();
        DisplayLiteralExpressionCore(text, value);
        return text.ToString();
    }

    private static void DisplayLiteralExpressionCore(DisplayText text, object value) {
        if (value is null) {
            text.Write(CreateLiteral("null"));
            return;
        }

        var specialType = LiteralUtilities.AssumeTypeFromLiteral(value);

        if (specialType == SpecialType.String)
            DisplayStringLiteral(value.ToString(), false);
        else if (specialType == SpecialType.Char)
            DisplayStringLiteral(value.ToString(), true);
        else
            text.Write(CreateLiteral(value.ToString().ToLower()));

        void DisplayStringLiteral(string value, bool isCharacter) {
            var stringBuilder = new StringBuilder(isCharacter ? "'" : "\"");

            foreach (var c in value) {
                switch (c) {
                    case '\a':
                        text.Write(CreateString(stringBuilder.ToString()));
                        stringBuilder.Clear();
                        text.Write(CreateEscape("\\a"));
                        break;
                    case '\b':
                        text.Write(CreateString(stringBuilder.ToString()));
                        stringBuilder.Clear();
                        text.Write(CreateEscape("\\b"));
                        break;
                    case '\f':
                        text.Write(CreateString(stringBuilder.ToString()));
                        stringBuilder.Clear();
                        text.Write(CreateEscape("\\f"));
                        break;
                    case '\n':
                        text.Write(CreateString(stringBuilder.ToString()));
                        stringBuilder.Clear();
                        text.Write(CreateEscape("\\n"));
                        break;
                    case '\r':
                        text.Write(CreateString(stringBuilder.ToString()));
                        stringBuilder.Clear();
                        text.Write(CreateEscape("\\r"));
                        break;
                    case '\t':
                        text.Write(CreateString(stringBuilder.ToString()));
                        stringBuilder.Clear();
                        text.Write(CreateEscape("\\t"));
                        break;
                    case '\v':
                        text.Write(CreateString(stringBuilder.ToString()));
                        stringBuilder.Clear();
                        text.Write(CreateEscape("\\v"));
                        break;
                    case '\"':
                        text.Write(CreateString(stringBuilder.ToString()));
                        stringBuilder.Clear();
                        text.Write(CreateEscape("\\\""));
                        break;
                    case '\\':
                        text.Write(CreateString(stringBuilder.ToString()));
                        stringBuilder.Clear();
                        text.Write(CreateEscape("\\\\"));
                        break;
                    default:
                        stringBuilder.Append(c);
                        break;
                }
            }

            stringBuilder.Append(isCharacter ? '\'' : '"');
            text.Write(CreateString(stringBuilder.ToString()));
        }
    }

    private static void DisplayTypeExpression(DisplayText text, BoundTypeExpression node) {
        SymbolDisplay.DisplayType(text, node.type);
    }

    private static void DisplayBreakStatement(DisplayText text) {
        text.Write(CreateKeyword(SyntaxKind.BreakKeyword));
        text.Write(CreateLine());
    }

    private static void DisplayContinueStatement(DisplayText text) {
        text.Write(CreateKeyword(SyntaxKind.ContinueKeyword));
        text.Write(CreateLine());
    }

    private static void DisplayGlobalStatement(DisplayText text, BoundGlobalStatement node) {
        DisplayNode(text, node.statement);
    }

    private static void DisplayNopStatement(DisplayText text) {
        text.Write(CreateKeyword("nop"));
        text.Write(CreateLine());
    }

    private static void DisplayBlockStatement(DisplayText text, BoundBlockStatement node, bool newLine = true) {
        text.Write(CreatePunctuation(SyntaxKind.OpenBraceToken));
        text.Write(CreateLine());

        text.indent++;

        foreach (var statement in node.statements)
            DisplayNode(text, statement);

        text.indent--;
        text.Write(CreatePunctuation(SyntaxKind.CloseBraceToken));

        if (newLine)
            text.Write(CreateLine());
    }

    private static void DisplayTryStatement(DisplayText text, BoundTryStatement node) {
        text.Write(CreateKeyword(SyntaxKind.TryKeyword));
        text.Write(CreateSpace());
        DisplayBlockStatement(text, node.body, false);

        if (node.catchBody is not null) {
            text.Write(CreateSpace());
            text.Write(CreateKeyword(SyntaxKind.CatchKeyword));
            text.Write(CreateSpace());
            DisplayBlockStatement(text, node.catchBody, false);
        }

        if (node.finallyBody is not null) {
            text.Write(CreateSpace());
            text.Write(CreateKeyword(SyntaxKind.FinallyKeyword));
            text.Write(CreateSpace());
            DisplayBlockStatement(text, node.finallyBody, false);
        }

        text.Write(CreateLine());
    }

    private static void DisplayReturnStatement(DisplayText text, BoundReturnStatement node) {
        text.Write(CreateKeyword(SyntaxKind.ReturnKeyword));

        if (node.expression is not null) {
            text.Write(CreateSpace());
            DisplayNode(text, node.expression);
        }

        text.Write(CreateLine());
    }

    private static void DisplayNestedStatement(DisplayText text, BoundStatement node) {
        var needsIndentation = node is not BoundBlockStatement;

        if (needsIndentation)
            text.indent++;

        DisplayNode(text, node);

        if (needsIndentation)
            text.indent--;
    }

    private static void DisplayDoWhileStatement(DisplayText text, BoundDoWhileStatement node) {
        text.Write(CreateKeyword(SyntaxKind.DoKeyword));
        text.Write(CreateSpace());
        text.Write(CreatePunctuation(SyntaxKind.OpenBraceToken));
        text.Write(CreateLine());
        DisplayNestedStatement(text, node.body);
        text.Write(CreatePunctuation(SyntaxKind.CloseBraceToken));
        text.Write(CreateSpace());
        text.Write(CreateKeyword(SyntaxKind.WhileKeyword));
        text.Write(CreateSpace());
        text.Write(CreatePunctuation(SyntaxKind.OpenParenToken));
        DisplayNode(text, node.condition);
        text.Write(CreatePunctuation(SyntaxKind.CloseParenToken));
        text.Write(CreateLine());
    }

    private static void DisplayConditionalGotoStatement(DisplayText text, BoundConditionalGotoStatement node) {
        text.Write(CreateKeyword("goto"));
        text.Write(CreateSpace());
        text.Write(CreateIdentifier(node.label.name));
        text.Write(CreateSpace());
        text.Write(CreateKeyword(node.jumpIfTrue ? "if" : "unless"));
        text.Write(CreateSpace());
        DisplayNode(text, node.condition);
        text.Write(CreateLine());
    }

    private static void DisplayLabelStatement(DisplayText text, BoundLabelStatement node) {
        var unindent = text.indent > 0;
        if (unindent)
            text.indent--;

        text.Write(CreatePunctuation(node.label.name));
        text.Write(CreatePunctuation(SyntaxKind.ColonToken));
        text.Write(CreateLine());

        if (unindent)
            text.indent++;
    }

    private static void DisplayGotoStatement(DisplayText text, BoundGotoStatement node) {
        text.Write(CreateKeyword("goto"));
        text.Write(CreateSpace());
        text.Write(CreateIdentifier(node.label.name));
        text.Write(CreateLine());
    }

    private static void DisplayForStatement(DisplayText text, BoundForStatement node) {
        text.Write(CreateKeyword(SyntaxKind.ForKeyword));
        text.Write(CreateSpace());
        text.Write(CreatePunctuation(SyntaxKind.OpenParenToken));
        DisplayNode(text, node.initializer);
        text.Write(CreateSpace());
        DisplayNode(text, node.condition);
        text.Write(CreateSpace());
        DisplayNode(text, node.step);
        text.Write(CreatePunctuation(SyntaxKind.CloseParenToken));
        text.Write(CreateSpace());
        text.Write(CreatePunctuation(SyntaxKind.OpenBraceToken));
        text.Write(CreateLine());
        DisplayNestedStatement(text, node.body);
        text.Write(CreatePunctuation(SyntaxKind.CloseBraceToken));
        text.Write(CreateLine());
    }

    private static void DisplayWhileStatement(DisplayText text, BoundWhileStatement node) {
        text.Write(CreateKeyword(SyntaxKind.WhileKeyword));
        text.Write(CreateSpace());
        text.Write(CreatePunctuation(SyntaxKind.OpenParenToken));
        DisplayNode(text, node.condition);
        text.Write(CreatePunctuation(SyntaxKind.CloseParenToken));
        text.Write(CreateSpace());
        text.Write(CreatePunctuation(SyntaxKind.OpenBraceToken));
        text.Write(CreateLine());
        DisplayNestedStatement(text, node.body);
        text.Write(CreatePunctuation(SyntaxKind.CloseBraceToken));
        text.Write(CreateLine());
    }

    private static void DisplayIfStatement(DisplayText text, BoundIfStatement node) {
        text.Write(CreateKeyword(SyntaxKind.IfKeyword));
        text.Write(CreateSpace());
        text.Write(CreatePunctuation(SyntaxKind.OpenParenToken));
        DisplayNode(text, node.condition);
        text.Write(CreatePunctuation(SyntaxKind.CloseParenToken));
        text.Write(CreateSpace());
        text.Write(CreatePunctuation(SyntaxKind.OpenBraceToken));
        text.Write(CreateLine());
        DisplayNestedStatement(text, node.then);
        text.Write(CreatePunctuation(SyntaxKind.CloseBraceToken));

        if (node.elseStatement is not null) {
            text.Write(CreateSpace());
            text.Write(CreateKeyword(SyntaxKind.ElseKeyword));
            text.Write(CreateSpace());
            text.Write(CreatePunctuation(SyntaxKind.OpenBraceToken));
            text.Write(CreateLine());
            DisplayNestedStatement(text, node.elseStatement);
            text.Write(CreatePunctuation(SyntaxKind.CloseBraceToken));
        }

        text.Write(CreateLine());
    }

    private static void DisplayLocalDeclarationStatement(DisplayText text, BoundLocalDeclarationStatement node) {
        DisplayNode(text, node.declaration);
    }

    private static void DisplayDataContainerDeclaration(DisplayText text, BoundDataContainerDeclaration node) {
        var dataContainer = node.dataContainer;

        SymbolDisplay.DisplayTypeWithAnnotations(
            text,
            dataContainer.typeWithAnnotations,
            SymbolDisplayFormat.Everything
        );

        text.Write(CreateSpace());
        text.Write(CreateIdentifier(dataContainer.name));
        text.Write(CreateSpace());
        text.Write(CreatePunctuation(SyntaxKind.EqualsToken));
        text.Write(CreateSpace());
        DisplayNode(text, node.initializer);
        text.Write(CreateLine());
    }

    private static void DisplayExpressionStatement(DisplayText text, BoundExpressionStatement node) {
        if (node.expression is BoundEmptyExpression)
            return;

        DisplayNode(text, node.expression);
        text.Write(CreateLine());
    }

    private static void DisplayFieldAccessExpression(
        DisplayText text,
        BoundFieldAccessExpression node,
        bool conditional = false) {
        DisplayNode(text, node.receiver);
        text.Write(CreatePunctuation(conditional ? SyntaxKind.QuestionPeriodToken : SyntaxKind.PeriodToken));
        SymbolDisplay.AppendToDisplayText(text, node.field, SymbolDisplayFormat.Everything);
    }

    private static void DisplayArrayAccessExpression(
        DisplayText text,
        BoundArrayAccessExpression node,
        bool conditional = false) {
        DisplayNode(text, node.receiver);
        text.Write(CreatePunctuation(conditional ? SyntaxKind.QuestionOpenBracketToken : SyntaxKind.OpenBracketToken));
        DisplayNode(text, node.index);
        text.Write(CreatePunctuation(SyntaxKind.CloseBracketToken));
    }

    private static void DisplayConditionalAccessExpression(DisplayText text, BoundConditionalAccessExpression node) {
        DisplayNode(text, node.receiver);
        var accessExpression = node.accessExpression;

        switch (accessExpression) {
            case BoundArrayAccessExpression a:
                DisplayArrayAccessExpression(text, a, true);
                break;
            case BoundFieldAccessExpression f:
                DisplayFieldAccessExpression(text, f, true);
                break;
            default:
                throw ExceptionUtilities.UnexpectedValue(accessExpression.kind);
        }
    }

    private static void DisplayObjectCreationExpression(DisplayText text, BoundObjectCreationExpression node) {
        text.Write(CreateKeyword(SyntaxKind.NewKeyword));
        text.Write(CreateSpace());
        SymbolDisplay.DisplayType(text, node.type);
        DisplayArguments(text, node.arguments);
    }

    private static void DisplayArrayCreationExpression(DisplayText text, BoundArrayCreationExpression node) {
        text.Write(CreateKeyword(SyntaxKind.NewKeyword));
        text.Write(CreateSpace());
        SymbolDisplay.DisplayType(text, node.type);

        text.Write(CreatePunctuation(SyntaxKind.OpenBracketToken));

        var isFirst = true;

        foreach (var size in node.sizes) {
            if (isFirst) {
                isFirst = false;
            } else {
                text.Write(CreatePunctuation(SyntaxKind.CommaToken));
                text.Write(CreateSpace());
            }

            DisplayNode(text, size);
        }

        text.Write(CreatePunctuation(SyntaxKind.CloseBracketToken));
    }

    private static void DisplayThisExpression(DisplayText text) {
        text.Write(CreateKeyword(SyntaxKind.ThisKeyword));
    }

    private static void DisplayBaseExpression(DisplayText text) {
        text.Write(CreateKeyword(SyntaxKind.BaseKeyword));
    }

    private static void DisplayThrowExpression(DisplayText text, BoundThrowExpression node) {
        text.Write(CreateKeyword(SyntaxKind.ThrowKeyword));
        text.Write(CreateSpace());
        DisplayNode(text, node.exception);
    }

    private static void DisplayConditionalExpression(DisplayText text, BoundConditionalExpression node) {
        text.Write(CreatePunctuation(SyntaxKind.OpenParenToken));
        DisplayNode(text, node.left);
        text.Write(CreateSpace());
        text.Write(CreatePunctuation(SyntaxKind.QuestionToken));
        text.Write(CreateSpace());
        DisplayNode(text, node.center);
        text.Write(CreateSpace());
        text.Write(CreatePunctuation(SyntaxKind.ColonToken));
        text.Write(CreateSpace());
        DisplayNode(text, node.right);
        text.Write(CreatePunctuation(SyntaxKind.CloseParenToken));
    }

    private static void DisplayTypeOfExpression(DisplayText text, BoundTypeOfExpression node) {
        text.Write(CreateKeyword(SyntaxKind.TypeOfKeyword));
        text.Write(CreatePunctuation(SyntaxKind.OpenParenToken));
        SymbolDisplay.DisplayType(text, node.type);
        text.Write(CreatePunctuation(SyntaxKind.CloseParenToken));
    }

    private static void DisplayReferenceExpression(DisplayText text, BoundReferenceExpression node) {
        text.Write(CreateKeyword(SyntaxKind.RefKeyword));
        text.Write(CreateSpace());
        DisplayNode(text, node.expression);
    }

    private static void DisplayCastExpression(DisplayText text, BoundCastExpression node) {
        text.Write(CreatePunctuation(SyntaxKind.OpenParenToken));
        SymbolDisplay.DisplayType(text, node.type);
        text.Write(CreatePunctuation(SyntaxKind.CloseParenToken));
        DisplayNode(text, node.operand);
    }

    private static void DisplayCallExpression(DisplayText text, BoundCallExpression node) {
        if (node.expression is not BoundEmptyExpression) {
            DisplayNode(text, node.expression);
            text.Write(CreatePunctuation(SyntaxKind.PeriodToken));
        }

        text.Write(CreateIdentifier(node.method.name));
        DisplayArguments(text, node.arguments);
    }

    private static void DisplayArguments(DisplayText text, ImmutableArray<BoundExpression> arguments) {
        text.Write(CreatePunctuation(SyntaxKind.OpenParenToken));

        var isFirst = true;
        foreach (var argument in arguments) {
            if (isFirst) {
                isFirst = false;
            } else {
                text.Write(CreatePunctuation(SyntaxKind.CommaToken));
                text.Write(CreateSpace());
            }

            DisplayNode(text, argument);
        }

        text.Write(CreatePunctuation(SyntaxKind.CloseParenToken));
    }

    private static void DisplayInitializerListExpression(DisplayText text, BoundInitializerListExpression node) {
        text.Write(CreatePunctuation(SyntaxKind.OpenBraceToken));

        var isFirst = true;

        foreach (var item in node.items) {
            if (isFirst) {
                isFirst = false;
            } else {
                text.Write(CreatePunctuation(SyntaxKind.CommaToken));
                text.Write(CreateSpace());
            }

            DisplayNode(text, item);
        }

        text.Write(CreatePunctuation(SyntaxKind.CloseBraceToken));
    }

    private static void DisplayErrorExpression(DisplayText text, BoundErrorExpression node) {
        text.Write(CreatePunctuation(SyntaxKind.OpenBracketToken));
        text.Write(CreateKeyword(SyntaxKind.QuestionToken));
        text.Write(CreateSpace());
        SymbolDisplay.DisplayType(text, node.type, SymbolDisplayFormat.Everything);
        text.Write(CreatePunctuation(SyntaxKind.CloseBracketToken));
    }

    private static void DisplayEmptyExpression(DisplayText _) { }

    private static void DisplayAssignmentExpression(DisplayText text, BoundAssignmentExpression node) {
        DisplayNode(text, node.left);
        text.Write(CreateSpace());
        text.Write(CreatePunctuation(SyntaxKind.EqualsToken));
        text.Write(CreateSpace());
        DisplayNode(text, node.right);
    }

    private static void DisplayCompoundAssignmentExpression(DisplayText text, BoundCompoundAssignmentExpression node) {
        DisplayNode(text, node.left);
        text.Write(CreateSpace());
        text.Write(CreatePunctuation(node.op.kind));
        text.Write(CreateSpace());
        DisplayNode(text, node.right);
    }

    private static void DisplayBinaryAdjacentExpression(
        DisplayText text,
        BoundExpression left,
        BoundExpression right,
        SyntaxKind op) {
        text.Write(CreatePunctuation(SyntaxKind.OpenParenToken));
        DisplayNode(text, left);
        text.Write(CreateSpace());
        text.Write(CreatePunctuation(op));
        text.Write(CreateSpace());
        DisplayNode(text, right);
        text.Write(CreatePunctuation(SyntaxKind.CloseParenToken));
    }

    private static void DisplayBinaryExpression(DisplayText text, BoundBinaryExpression node) {
        DisplayBinaryAdjacentExpression(text, node.left, node.right, node.op.kind);
    }

    private static void DisplayIsExpression(DisplayText text, BoundIsExpression node) {
        DisplayBinaryAdjacentExpression(text, node.left, node.right, SyntaxKind.IsKeyword);
    }

    private static void DisplayIsntExpression(DisplayText text, BoundIsntExpression node) {
        DisplayBinaryAdjacentExpression(text, node.left, node.right, SyntaxKind.IsntKeyword);
    }

    private static void DisplayAsExpression(DisplayText text, BoundAsExpression node) {
        DisplayBinaryAdjacentExpression(text, node.left, node.right, SyntaxKind.AsKeyword);
    }

    private static void DisplayNullCoalescingExpression(DisplayText text, BoundNullCoalescingExpression node) {
        DisplayBinaryAdjacentExpression(text, node.left, node.right, SyntaxKind.QuestionQuestionToken);
    }

    private static void DisplayDataContainer(DisplayText text, BoundDataContainerExpression node) {
        SymbolDisplay.AppendToDisplayText(text, node.dataContainer, SymbolDisplayFormat.Everything);
    }

    private static void DisplayUnaryExpression(DisplayText text, BoundUnaryExpression node) {
        text.Write(CreatePunctuation(node.op.kind));
        DisplayNode(text, node.operand);
    }

    private static void DisplayNullAssertExpression(DisplayText text, BoundNullAssertExpression node) {
        DisplayNode(text, node.operand);
        text.Write(CreatePunctuation(SyntaxKind.ExclamationToken));
    }

    private static void DisplayFieldEqualsValue(DisplayText text, BoundFieldEqualsValue node) {
        SymbolDisplay.AppendToDisplayText(text, node.field, SymbolDisplayFormat.Everything);
        text.Write(CreateSpace());
        text.Write(CreatePunctuation(SyntaxKind.EqualsToken));
        text.Write(CreateSpace());
        DisplayNode(text, node.value);
        text.Write(CreateLine());
    }
}
