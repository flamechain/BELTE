using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Text.RegularExpressions;
using Buckle.CodeAnalysis.Text;

// * Needs to use the lexer, parser, and constant evaluator to evaluate statements like #run, #if, #elif, and #define

namespace Buckle;

public abstract class PreprocessLine {
    public TextLine text;

    public PreprocessLine() { }
}

public sealed class PreprocessIf : PreprocessLine {
    // includes elif, else, and end
}

public sealed class PreprocessPragma : PreprocessLine { }

public sealed class PreprocessDefine : PreprocessLine { }

public sealed class PreprocessUndefine : PreprocessLine { }

public sealed class PreprocessWarning : PreprocessLine { }

public sealed class PreprocessError : PreprocessLine { }

public sealed class PreprocessRun : PreprocessLine { }

public sealed class PreprocessSand : PreprocessLine { }

public sealed class PreprocessFile {
    public ImmutableArray<PreprocessLine> lines;

    private PreprocessFile() { }

    public static PreprocessFile Parse(ImmutableArray<TextLine> lines) {
        var preprocessFile = new PreprocessFile();

        var builder = ImmutableArray.CreateBuilder<PreprocessLine>();
        preprocessFile.lines = builder.ToImmutable();

        // ! just so it compiles
        return preprocessFile;
    }
}

public class Preprocessor {
    private Dictionary<string, string> symbols = new Dictionary<string, string>();

    public string PreprocessText(string fileName, string text) {
        var sourceText = SourceText.From(text, fileName);
        var lines = sourceText.lines;

        var preprocessFile = PreprocessFile.Parse(lines);

        foreach (var line in preprocessFile.lines) {

        }

        // ! just so it compiles
        return text;
    }
}
