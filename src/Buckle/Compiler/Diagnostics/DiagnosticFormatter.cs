using System;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;
using Buckle.CodeAnalysis.Text;
using Diagnostics;

namespace Buckle.Diagnostics;

public static partial class DiagnosticFormatter {
    private readonly struct DisplayParts {
        private readonly List<(string text, ConsoleColor color)> _parts;

        public DisplayParts() {
            _parts = [];
        }

        public override string ToString() {
            var builder = new StringBuilder();

            foreach (var (text, _) in _parts)
                builder.Append(text);

            return builder.ToString();
        }

        internal void Write() {
            var initialColor = Console.ForegroundColor;

            foreach (var (text, color) in _parts) {
                Console.ForegroundColor = color;
                Console.Write(text);
            }

            Console.ForegroundColor = initialColor;
        }

        internal void Add(string text, ConsoleColor color) {
            _parts.Add((text, color));
        }
    }

    public static string Format(BelteDiagnostic diagnostic) {
        return ToDisplayParts(diagnostic).ToString();
    }

    public static void PrettyPrint(BelteDiagnostic diagnostic, ConsoleColor? foregroundColor = null) {
        ToDisplayParts(diagnostic, foregroundColor).Write();
    }

    private static DisplayParts ToDisplayParts(BelteDiagnostic diagnostic, ConsoleColor? foregroundColor = null) {
        var displayParts = new DisplayParts();
        var initialColor = foregroundColor ?? Console.ForegroundColor;

        var span = diagnostic.location.span;
        var text = diagnostic.location.text;

        var lineNumber = text.GetLineIndex(span.start);
        var line = text.GetLine(lineNumber);
        var column = span.start - line.start + 1;
        var lineText = line.ToString();

        var fileName = diagnostic.location.fileName;

        if (!string.IsNullOrEmpty(fileName))
            displayParts.Add($"{fileName}:", initialColor);

        displayParts.Add($"{lineNumber + 1}:{column}:", initialColor);

        var highlightColor = ConsoleColor.White;
        var severity = diagnostic.info.severity;

        switch (severity) {
            case DiagnosticSeverity.Debug:
                highlightColor = ConsoleColor.Gray;
                displayParts.Add(" debug", highlightColor);
                break;
            case DiagnosticSeverity.Info:
                highlightColor = ConsoleColor.Yellow;
                displayParts.Add(" info", highlightColor);
                break;
            case DiagnosticSeverity.Warning:
                highlightColor = ConsoleColor.Magenta;
                displayParts.Add(" warning", highlightColor);
                break;
            case DiagnosticSeverity.Error:
                highlightColor = ConsoleColor.Red;
                displayParts.Add(" error", highlightColor);
                break;
            case DiagnosticSeverity.Fatal:
                highlightColor = ConsoleColor.Red;
                displayParts.Add(" fatal", highlightColor);
                break;
        }

        if (diagnostic.info.code is not null && diagnostic.info.code > 0)
            displayParts.Add($" {diagnostic.info}: ", highlightColor);
        else
            displayParts.Add(": ", highlightColor);

        displayParts.Add($"{diagnostic.message}\n", initialColor);

        if (text.IsAtEndOfInput(span))
            return displayParts;

        var prefixSpan = TextSpan.FromBounds(line.start, span.start);
        var suffixSpan = span.end > line.end
            ? TextSpan.FromBounds(line.end, line.end)
            : TextSpan.FromBounds(span.end, line.end);

        var prefix = text.ToString(prefixSpan);
        var focus = text.ToString(span);
        var suffix = text.ToString(suffixSpan);

        displayParts.Add($" {prefix}", initialColor);
        displayParts.Add(focus, highlightColor);
        displayParts.Add($"{suffix}\n", initialColor);

        var markerPrefix = " " + MyRegex().Replace(prefix, " ");
        var marker = "^";

        if (span.length > 0 && column != lineText.Length)
            marker += new string('~', span.length - 1);

        displayParts.Add($"{markerPrefix}{marker}\n", highlightColor);

        if (diagnostic.suggestions.Length > 0) {
            var firstSuggestion = diagnostic.suggestions[0].Replace("%", focus);
            displayParts.Add($"{markerPrefix}{firstSuggestion}\n", ConsoleColor.Green);

            for (var i = 1; i < diagnostic.suggestions.Length; i++) {
                var suggestion = diagnostic.suggestions[i].Replace("%", focus);
                displayParts.Add(string.Concat(markerPrefix.AsSpan(0, markerPrefix.Length - 3), "or "), initialColor);
                displayParts.Add($"{suggestion}\n", ConsoleColor.Green);
            }
        }

        return displayParts;
    }

    [GeneratedRegex(@"\S")]
    private static partial Regex MyRegex();
}
