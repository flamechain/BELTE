using Xunit;

namespace Buckle.Tests.CodeAnalysis;

public sealed partial class EvaluatorTests {
    [Fact]
    public void Evaluator_Reports_Warning_BU0001_AlwaysValue() {
        var text = @"
            var x = [null > 3];
        ";

        var diagnostics = @"
            expression will always result to 'null'
        ";

        AssertDiagnostics(text, diagnostics, true);
    }

    [Fact]
    public void Evaluator_Reports_Warning_BU0002_NullDeference() {
        var text = @"
            struct A {
                int num;
            }

            void MyFunc(A a) {
                a[.]num = 3;
            }
        ";

        var diagnostics = @"
            deference of a possibly null value
        ";

        AssertDiagnostics(text, diagnostics, true);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0004_InvalidType() {
        var text = @"
            int x = [99999999999999999];
        ";

        var diagnostics = @"
            '99999999999999999' is not a valid 'int'
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0005_BadCharacter() {
        var text = @"
            [#];
        ";

        var diagnostics = @"
            unknown character '#'
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0006_UnexpectedToken() {
        var text = @"
            if [=](true) {}
        ";

        var diagnostics = @"
            unexpected token '='
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0007_CannotConvertImplicitly() {
        var text = @"
            string x = [3];
        ";

        var diagnostics = @"
            cannot convert from type 'int' to 'string'. An explicit conversion exists (are you missing a cast?)
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0008_InvalidUnaryOperatorUse() {
        var text = @"
            [-]false;
        ";

        var diagnostics = @"
            unary operator '-' is not defined for type 'bool'
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0009_NamedBeforeUnnamed() {
        var text = @"
            Print([x]: 1, 3);
        ";

        var diagnostics = @"
            all named arguments must come after any unnamed arguments
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0010_NamedArgumentTwice() {
        var text = @"
            Print(x: 1, [x]: 3);
        ";

        var diagnostics = @"
            named argument 'x' cannot be specified multiple times
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0011_InvalidBinaryOperatorUse() {
        var text = @"
            false [+] 3;
        ";

        var diagnostics = @"
            binary operator '+' is not defined for types 'bool' and 'int'
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0013_ParameterAlreadyDeclared() {
        var text = @"
            void myFunc(int x, [int x]) { }
        ";

        var diagnostics = @"
            cannot reuse parameter name 'x'; parameter names must be unique
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0015_NoSuchParameter() {
        var text = @"
            Print([msg]: ""test"");
        ";

        var diagnostics = @"
            function 'Print' does not have a parameter named 'msg'
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0017_UndefinedSymbol() {
        var text = @"
            int x = [y];
        ";

        var diagnostics = @"
            undefined symbol 'y'
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0018_MethodAlreadyDeclared() {
        var text = @"
            void myFunc() { }

            void [myFunc]() { }
        ";

        var diagnostics = @"
            redefinition of method 'myFunc'
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0019_NotAllPathsReturn() {
        var text = @"
            int [myFunc]() { }
        ";

        var diagnostics = @"
            not all code paths return a value
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0020_CannotConvert() {
        var text = @"
            struct A {
                int num;
            }

            bool x = [A()];
        ";

        var diagnostics = @"
            cannot convert from type '[NotNull]A' to 'bool'
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0020_InvalidArgumentType() {
        var text = @"
            void myFunc(int a, bool b) { }
            myFunc(3, [5]);
        ";

        var diagnostics = @"
            argument 2: cannot convert from type 'int' to 'bool'
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0021_VariableAlreadyDeclared() {
        var text = @"
            var x = 5;
            var [x] = 7;
        ";

        var diagnostics = @"
            variable 'x' is already declared in this scope
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0022_ConstantAssignment() {
        var text = @"
            const int x = 5;
            x [=] 4;
        ";

        var diagnostics = @"
            'x' cannot be assigned to as it is a constant
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0023_AmbiguousElse() {
        var text = @"
            if (true)
                if (true)
                    PrintLine();
            [else]
                PrintLine();
        ";

        var diagnostics = @"
            ambiguous what if-statement this else-clause belongs to; use curly braces
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0024_NoValue() {
        var text = @"
            int x = [PrintLine()];
        ";

        var diagnostics = @"
            expression must have a value
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0025_CannotApplyIndexing() {
        var text = @"
            int x = 3;
            int y = [x\[0\]];
        ";

        var diagnostics = @"
            cannot apply indexing with [] to an expression of type 'int'
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Warning_BU0026_UnreachableCode() {
        var text = @"
            if (false) {
                [PrintLine();]
                PrintLine();
            }
        ";

        var diagnostics = @"
            unreachable code
        ";

        AssertDiagnostics(text, diagnostics, true);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0027_UnterminatedString() {
        var text = @"
            string x = [""];[]
        ";

        var diagnostics = @"
            unterminated string literal
            expected ';' at end of input
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0028_UndefinedFunction() {
        var text = @"
            string x = [myFunc]();
        ";

        var diagnostics = @"
            undefined function 'myFunc'
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0029_IncorrectArgumentCount() {
        var text = @"
            void myFunc() { }
            myFunc([3]);
        ";

        var diagnostics = @"
            function 'myFunc' expects 0 arguments, got 1
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0030_StructAlreadyDeclared() {
        var text = @"
            struct A { }

            struct [A] { }
        ";

        var diagnostics = @"
            struct 'A' has already been declared in this scope
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0031_DuplicateAttribute() {
        var text = @"
            \[NotNull\]\[[NotNull]\]int a = 3;
        ";

        var diagnostics = @"
            attribute 'NotNull' has already been applied
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0032_CannotCallNonFunction() {
        var text = @"
            int x = 3;
            int y = [x]();
        ";

        var diagnostics = @"
            called object 'x' is not a function
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0033_InvalidExpressionStatement() {
        var text = @"
            void myFunc() {
                [5 + 3;]
            }
        ";

        var diagnostics = @"
            only assignment and call expressions can be used as a statement
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0034_UnknownType() {
        var text = @"
            [MyType] x;
        ";

        var diagnostics = @"
            unknown type 'MyType'
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0035_InvalidBreakOrContinue() {
        var text = @"
            [break];
        ";

        var diagnostics = @"
            break statements can only be used within a loop
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0037_UnexpectedReturnValue() {
        var text = @"
            void myFunc() {
                [return] 3;
            }
        ";

        var diagnostics = @"
            cannot return a value in a function returning void
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0038_MissingReturnValue() {
        var text = @"
            int myFunc() {
                [return];
            }
        ";

        var diagnostics = @"
            cannot return without a value in a function returning non-void
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0039_NotAVariable() {
        var text = @"
            void myFunc() { }

            int x = [myFunc] + 3;
        ";

        var diagnostics = @"
            function 'myFunc' cannot be used as a variable
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0040_NoInitOnImplicit() {
        var text = @"
            var [x];
        ";

        var diagnostics = @"
            implicitly-typed variable must have initializer
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0041_UnterminatedComment() {
        var text = @"
            [/*]
        ";

        var diagnostics = @"
            unterminated multi-line comment
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0042_NullAssignOnImplicit() {
        var text = @"
            var x = [null];
        ";

        var diagnostics = @"
            cannot initialize an implicitly-typed variable with 'null'
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0043_EmptyInitializerListOnImplicit() {
        var text = @"
            var x = [{}];
        ";

        var diagnostics = @"
            cannot initialize an implicitly-typed variable with an empty initializer list
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0044_ImpliedDimensions() {
        var text = @"
            var[\[\]] x = {1, 2, 3};
        ";

        var diagnostics = @"
            collection dimensions on implicitly-typed variables are inferred making them not necessary in this context
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0045_CannotUseImplicit() {
        var text = @"
            [var] myFunc() { }
        ";

        var diagnostics = @"
            cannot use implicit-typing in this context
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0046_NoCatchOrFinally() {
        var text = @"
            try { [}]
        ";

        var diagnostics = @"
            try statement must have a catch or finally
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0047_ExpectedMethodName() {
        var text = @"
            [PrintLine()]();
        ";

        var diagnostics = @"
            expected method name
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0048_ReferenceNoInitialization() {
        var text = @"
            ref int [x];
        ";

        var diagnostics = @"
            a declaration of a by-reference variable must have an initializer
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0049_ReferenceWrongInitialization() {
        var text = @"
            int x = 3;
            ref int y [=] x;
        ";

        var diagnostics = @"
            a by-reference variable must be initialized with a reference
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0050_WrongInitializationReference() {
        var text = @"
            int x = 3;
            int y [=] ref x;
        ";

        var diagnostics = @"
            cannot initialize a by-value variable with a reference
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0051_UnknownAttribute() {
        var text = @"
            \[[MyAttrib]\]int x;
        ";

        var diagnostics = @"
            unknown attribute 'MyAttrib'
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0052_NullAssignNotNull() {
        var text = @"
            \[NotNull\]int x = [null];
        ";

        var diagnostics = @"
            cannot assign 'null' to a non-nullable variable
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0053_ImpliedReference() {
        var text = @"
            var x = 3;
            [ref] var y = ref x;
        ";

        var diagnostics = @"
            implicitly-typed variables infer reference types making the 'ref' keyword not necessary in this context
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0054_ReferenceToConstant() {
        var text = @"
            const int x = 3;
            ref int y [=] ref x;
        ";

        var diagnostics = @"
            cannot assign a reference to a constant to a by-reference variable expecting a reference to a variable
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0055_VoidVariable() {
        var text = @"
            [void] a;
        ";

        var diagnostics = @"
            cannot use void as a type
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0056_ExpectedToken() {
        var text = @"
            struct [{]
                int num;
            }
        ";

        var diagnostics = @"
            expected identifier
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0057_NoOverload() {
        var text = @"
            void myFunc(int a) { }

            void myFunc(string a) { }

            [myFunc](false);
        ";

        var diagnostics = @"
            no overload for function 'myFunc' matches parameter list
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0058_AmbiguousOverload() {
        var text = @"
            void myFunc(int a) { }

            void myFunc(string a) { }

            [myFunc](null);
        ";

        var diagnostics = @"
            function call is ambiguous between 'void myFunc(int a)' and 'void myFunc(string a)'
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0059_CannotIncrement() {
        var text = @"
            [1]++;
        ";

        var diagnostics = @"
            the operand of an increment or decrement operator must be a variable, field, or indexer
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0060_InvalidTernaryOperatorUse() {
        var text = @"
            3 [?] 4 : 6;
        ";

        var diagnostics = @"
            ternary operator '?:' is not defined for types 'int', 'int', and 'int'
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0061_NoSuchMember() {
        var text = @"
            int a = 3;
            a.[Max];
        ";

        var diagnostics = @"
            'int' contains no such member 'Max'
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0062_CannotAssign() {
        var text = @"
            [3] = 45;
        ";

        var diagnostics = @"
            left side of assignment operation must be a variable, field, or indexer
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0063_CannotOverloadNested() {
        var text = @"
            void myFunc() {
                void myFunc2(int a) { }

                void [myFunc2](string a) { }
            }
        ";

        var diagnostics = @"
            cannot overload nested functions; nested function 'myFunc2' has already been defined
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0064_ConstantToNonConstantReference() {
        var text = @"
            int x = 3;
            ref const int y [=] ref x;
        ";

        var diagnostics = @"
            cannot assign a reference to a variable to a by-reference variable expecting a reference to a constant
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0065_InvalidPrefixUse() {
        var text = @"
            bool a = false;
            [++]a;
        ";

        var diagnostics = @"
            prefix operator '++' is not defined for type 'bool'
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_BU0066_InvalidPostfixUse() {
        var text = @"
            bool a = false;
            a[++];
        ";

        var diagnostics = @"
            postfix operator '++' is not defined for type 'bool'
        ";

        AssertDiagnostics(text, diagnostics);
    }

    [Fact]
    public void Evaluator_Reports_Error_Unsupported_BU9004_CannotInitialize() {
        var text = @"
            struct A {
                int num [=] 3;
            }
        ";

        var diagnostics = @"
            cannot initialize declared symbol in this context
        ";

        AssertDiagnostics(text, diagnostics);
    }
}
