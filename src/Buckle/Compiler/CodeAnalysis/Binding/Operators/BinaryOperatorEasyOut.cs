using Buckle.CodeAnalysis.Symbols;

namespace Buckle.CodeAnalysis.Binding;

internal sealed partial class OverloadResolution {
    internal static class BinOpEasyOut {
        private const BinaryOperatorKind ERR = BinaryOperatorKind.Error;
        private const BinaryOperatorKind ANY = BinaryOperatorKind.Any;
        private const BinaryOperatorKind OBJ = BinaryOperatorKind.Object;
        private const BinaryOperatorKind STR = BinaryOperatorKind.String;
        private const BinaryOperatorKind INT = BinaryOperatorKind.Int;
        private const BinaryOperatorKind DEC = BinaryOperatorKind.Decimal;
        private const BinaryOperatorKind BOL = BinaryOperatorKind.Bool;
        private const BinaryOperatorKind CHR = BinaryOperatorKind.Char;
        private const BinaryOperatorKind TYP = BinaryOperatorKind.Type;

        private static readonly BinaryOperatorKind[,] Arithmetic = {
            // Y <op> X:
            //          any  str  bool chr  int  dec  type obj
            /*  any */{ ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR },
            /*  str */{ ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR },
            /* bool */{ ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR },
            /*  chr */{ ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR },
            /*  int */{ ERR, ERR, ERR, ERR, INT, DEC, ERR, ERR },
            /*  dec */{ ERR, ERR, ERR, ERR, DEC, DEC, ERR, ERR },
            /* type */{ ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR },
            /*  obj */{ ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR }
        };

        private static readonly BinaryOperatorKind[,] Addition = {
            // Y + X:
            //          any  str  bool chr  int  dec  type obj
            /*  any */{ ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR },
            /*  str */{ ERR, STR, ERR, ERR, ERR, ERR, ERR, ERR },
            /* bool */{ ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR },
            /*  chr */{ ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR },
            /*  int */{ ERR, ERR, ERR, ERR, INT, DEC, ERR, ERR },
            /*  dec */{ ERR, ERR, ERR, ERR, DEC, DEC, ERR, ERR },
            /* type */{ ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR },
            /*  obj */{ ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR }
        };

        private static readonly BinaryOperatorKind[,] Shift = {
            // Y <op> X:
            //          any  str  bool chr  int  dec  type obj
            /*  any */{ ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR },
            /*  str */{ ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR },
            /* bool */{ ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR },
            /*  chr */{ ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR },
            /*  int */{ ERR, ERR, ERR, ERR, INT, ERR, ERR, ERR },
            /*  dec */{ ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR },
            /* type */{ ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR },
            /*  obj */{ ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR }
        };

        private static readonly BinaryOperatorKind[,] Equality = {
            // Y <op> X:
            //          any  str  bool chr  int  dec  type obj
            /*  any */{ ANY, ANY, ANY, ANY, ANY, ANY, ANY, ANY },
            /*  str */{ ANY, STR, ERR, ERR, ERR, ERR, ERR, ERR },
            /* bool */{ ANY, ERR, BOL, ERR, ERR, ERR, ERR, ERR },
            /*  chr */{ ANY, ERR, ERR, CHR, ERR, ERR, ERR, ERR },
            /*  int */{ ANY, ERR, ERR, ERR, INT, DEC, ERR, ERR },
            /*  dec */{ ANY, ERR, ERR, ERR, DEC, DEC, ERR, ERR },
            /* type */{ ANY, ERR, ERR, ERR, ERR, ERR, TYP, ERR },
            /*  obj */{ ANY, ERR, ERR, ERR, ERR, ERR, ERR, OBJ }
        };

        private static readonly BinaryOperatorKind[,] Logical = {
            // Y <op> X:
            //          any  str  bool chr  int  dec  type obj
            /*  any */{ ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR },
            /*  str */{ ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR },
            /* bool */{ ERR, ERR, BOL, ERR, ERR, ERR, ERR, ERR },
            /*  chr */{ ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR },
            /*  int */{ ERR, ERR, ERR, ERR, INT, ERR, ERR, ERR },
            /*  dec */{ ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR },
            /* type */{ ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR },
            /*  obj */{ ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR }
        };

        private static readonly BinaryOperatorKind[][,] Operators = [
            Arithmetic,
            Addition,
            Arithmetic,
            Arithmetic,
            Arithmetic,
            Shift,
            Shift,
            Equality,
            Equality,
            Arithmetic,
            Arithmetic,
            Arithmetic,
            Arithmetic,
            Shift,
            Logical,
            Logical,
            Logical,
            Arithmetic
        ];

        internal static BinaryOperatorKind OpKind(BinaryOperatorKind kind, TypeSymbol left, TypeSymbol right) {
            var leftIndex = left.StrippedType().TypeToIndex();

            if (leftIndex < 0)
                return BinaryOperatorKind.Error;

            var rightIndex = right.StrippedType().TypeToIndex();

            if (rightIndex < 0)
                return BinaryOperatorKind.Error;

            var result = BinaryOperatorKind.Error;

            if (!kind.IsConditional() ||
                (leftIndex == (int)BinaryOperatorKind.Bool && rightIndex == (int)BinaryOperatorKind.Bool)) {
                result = Operators[kind.OperatorIndex()][leftIndex, rightIndex];
            }

            return result == BinaryOperatorKind.Error ? result : result | kind;
        }
    }

    private void BinaryOperatorEasyOut(
        BinaryOperatorKind kind,
        BoundExpression left,
        BoundExpression right,
        BinaryOperatorOverloadResolutionResult result) {
        var leftType = left.type;

        if (leftType is null)
            return;

        var rightType = right.type;

        if (rightType is null)
            return;

        var easyOut = BinOpEasyOut.OpKind(kind, leftType, rightType);

        if (easyOut == BinaryOperatorKind.Error)
            return;

        var signature = OperatorFacts.GetSignature(easyOut);
        var leftConversion = Conversions.FastClassifyConversion(leftType, signature.leftType);
        var rightConversion = Conversions.FastClassifyConversion(rightType, signature.rightType);

        result.results.Add(BinaryOperatorAnalysisResult.Applicable(signature, leftConversion, rightConversion));
    }
}
