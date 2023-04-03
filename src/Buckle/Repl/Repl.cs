using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading;
using Buckle;
using Buckle.CodeAnalysis.Text;
using Buckle.Diagnostics;

namespace Repl;

/// <summary>
/// Overrides default console handling and controls all keystrokes. Adds framework for meta commands and submissions.
/// </summary>
public abstract partial class Repl {
    /// <summary>
    /// The width of a tab or '\t' character in spaces.
    /// </summary>
    internal const int TabWidth = 4;

    /// <summary>
    /// Handles outputting <see cref="Repl" /> text to an out.
    /// </summary>
    internal OutputCapture _writer = new OutputCapture();

    /// <summary>
    /// <see cref="Compiler" /> object representing entirety of compilation.
    /// </summary>
    internal Compiler handle;

    /// <summary>
    /// Particular <see cref="Repl.DiagnosticHandle" /> used to handle diagnostics in the <see cref="Repl" />.
    /// </summary>
    internal DiagnosticHandle diagnosticHandle;

    private readonly List<string> _submissionHistory = new List<string>();
    private readonly List<MetaCommand> _metaCommands = new List<MetaCommand>();

    protected List<TextChange> _changes = new List<TextChange>();
    protected bool _abortEvaluation;
    protected bool _showTime;

    private int _submissionHistoryIndex;
    private bool _done;
    private bool _evaluate;

    protected Repl(Compiler handle, DiagnosticHandle diagnosticHandle) {
        this.handle = handle;
        this.diagnosticHandle = diagnosticHandle;
        InitializeMetaCommands();
    }

    /// <summary>
    /// Callback to handle Diagnostics, be it logging or displaying to the console.
    /// </summary>
    /// <param name="compiler"><see cref="Compiler" /> object representing entirety of compilation.</param>
    /// <param name="me">Display name of the program.</param>
    /// <param name="textColor">Color to display Diagnostics (if displaying).</param>
    /// <returns>C-Style error code of most severe Diagnostic.</returns>
    public delegate int DiagnosticHandle(
        Compiler compiler, string me = null, ConsoleColor textColor = ConsoleColor.White);

    private delegate void LineRenderHandler(IReadOnlyList<string> lines, int lineIndex);

    /// <summary>
    /// <see cref="Repl" /> specific state used by child classes.
    /// </summary>
    internal abstract object _state { get; set; }

    /// <summary>
    /// Resets all state.
    /// </summary>
    internal virtual void ResetState() {
        _done = false;
        _evaluate = true;
        _showTime = false;
        _changes = new List<TextChange>();
    }

    /// <summary>
    /// Run loop of the <see cref="Repl" /> (exited with ctrl + c or entering blank lines).
    /// Does not initialize <see cref="Repl" />, only runs it.
    /// </summary>
    public void Run() {
        string text;
        bool broke;

        void EvaluateSubmissionWrapper() {
            EvaluateSubmission(text);
        }

        void ctrlCHandler(object sender, ConsoleCancelEventArgs args) {
            _abortEvaluation = true;
            args.Cancel = true;
            broke = true;
        }

        while (true) {
            text = EditSubmission();

            if (string.IsNullOrEmpty(text))
                return;

            if (_evaluate) {
                if (!text.Contains(Environment.NewLine) && text.StartsWith('#')) {
                    EvaluateReplCommand(text);
                } else {
                    var evaluateSubmissionReference = new ThreadStart(EvaluateSubmissionWrapper);
                    var evaluateSubmissionThread = new Thread(evaluateSubmissionReference);
                    _abortEvaluation = false;
                    broke = false;
                    Console.CancelKeyPress += new ConsoleCancelEventHandler(ctrlCHandler);
                    var startTime = DateTime.Now;
                    evaluateSubmissionThread.Start();

                    while (evaluateSubmissionThread.IsAlive) ;

                    if (broke || _showTime) {
                        var finishWord = broke ? "Aborted" : "Finished";
                        var seconds = (DateTime.Now - startTime).TotalSeconds;
                        seconds = seconds > 1 ? (int)seconds : Math.Round(seconds, 3);

                        var secondWord = seconds < 1
                            ? (seconds * 1000 == 1 ? "millisecond" : "milliseconds")
                            : (seconds == 1 ? "second" : "seconds");

                        seconds = seconds < 1 ? seconds * 1000 : seconds;

                        var previous = Console.ForegroundColor;
                        Console.ForegroundColor = ConsoleColor.DarkGray;
                        _writer.WriteLine($"{finishWord} after {seconds} {secondWord}");
                        Console.ForegroundColor = previous;
                    }
                }
            }

            _evaluate = true;

            _submissionHistory.Add(text);
            _submissionHistoryIndex = 0;
        }
    }

    /// <summary>
    /// Reloads <see cref="Repl" /> to start accepting submissions again.
    /// </summary>
    internal void ReviveDocument() {
        // TODO Redisplay previous submissions
        Console.Clear();
    }

    /// <summary>
    /// Safely terminates an in-progress submission without evaluating it for a result.
    /// </summary>
    internal virtual void SpecialEscapeSequence() {
        _done = true;
        _evaluate = false;
    }

    /// <summary>
    /// Gets all previous submissions submitted in current instance (does not access previous instances' submissions).
    /// </summary>
    /// <returns>Internal representation of submissions.</returns>
    internal List<string> GetSubmissionHistory() {
        return _submissionHistory;
    }

    protected virtual void RenderLine(IReadOnlyList<string> lines, int lineIndex) {
        _writer.Write(lines[lineIndex]);
    }

    protected void ClearHistory() {
        _submissionHistory.Clear();
    }

    protected abstract bool IsCompleteSubmission(string text);
    protected abstract void EvaluateSubmission(string text);
    protected abstract void EvaluateSubmission();

    [MetaCommand("help", "Shows this document")]
    protected void EvaluateHelp() {
        var maxLength = _metaCommands
            .Max(
                mc => mc.name.Length +
                string.Join(" ", mc.method.GetParameters().SelectMany(p => p.Name).ToList()).Length +
                string.Join(" ", mc.method.GetParameters().SelectMany(p => p.DefaultValue.ToString()).ToList()).Length);

        foreach (var metaCommand in _metaCommands.OrderBy(mc => mc.name)) {
            var name = metaCommand.name;
            var args = "";

            foreach (var parameter in metaCommand.method.GetParameters()) {
                if (parameter.HasDefaultValue)
                    args += $" <{parameter.Name}={parameter.DefaultValue}>";
                else
                    args += $" <{parameter.Name}>";
            }

            Console.ForegroundColor = ConsoleColor.DarkGray;
            _writer.Write("#");
            Console.ResetColor();
            _writer.Write(name);
            Console.ForegroundColor = ConsoleColor.DarkGray;
            _writer.Write($"{args.PadRight(maxLength - name.Length)}  {metaCommand.description}");
            Console.ResetColor();
            _writer.WriteLine();
        }
    }

    [MetaCommand("state", "Dumps the current state of the Repl")]
    protected void EvaluateState() {
        var fields = _state.GetType()
            .GetFields(BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Public);

        var maxLength = fields.Max(f => f.Name.Length);
        var previous = Console.ForegroundColor;

        foreach (var field in fields) {
            var paddedName = field.Name.PadRight(maxLength);
            Console.ResetColor();
            _writer.Write($"{paddedName}  ");
            Console.ForegroundColor = ConsoleColor.DarkGray;
            _writer.Write(field.GetValue(_state));
            Console.ResetColor();
            _writer.WriteLine();
        }
    }

    private string EditSubmission() {
        _done = false;

        var document = new ObservableCollection<string>() { "" };
        var view = new SubmissionView(RenderLine, document, _writer);

        while (!_done) {
            // Allow ctrl + c to exit at all times except getting user input
            // This allows custom behavior, but still keeps ctrl + c protection if app freezes
            Console.TreatControlCAsInput = true;
            var key = Console.ReadKey(true);
            Console.TreatControlCAsInput = false;
            HandleKey(key, document, view);
        }

        view.currentLine = document.Count - 1;
        view.currentCharacter = document[view.currentLine].Length;

        _writer.WriteLine();

        return string.Join(Environment.NewLine, document);
    }

    private void InitializeMetaCommands() {
        var methods = GetType()
            .GetMethods(BindingFlags.NonPublic | BindingFlags.Static |
                BindingFlags.Instance | BindingFlags.FlattenHierarchy
            );

        foreach (var method in methods) {
            var attribute = method.GetCustomAttribute<MetaCommandAttribute>();

            if (attribute == null)
                continue;

            var metaCommand = new MetaCommand(attribute.name, attribute.description, method);
            _metaCommands.Add(metaCommand);
        }
    }

    private void HandleKey(ConsoleKeyInfo key, ObservableCollection<string> document, SubmissionView view) {
        if (key.Modifiers == default(ConsoleModifiers)) {
            switch (key.Key) {
                case ConsoleKey.Enter:
                    HandleEnter(document, view);
                    break;
                case ConsoleKey.LeftArrow:
                    HandleLeftArrow(document, view);
                    break;
                case ConsoleKey.RightArrow:
                    HandleRightArrow(document, view);
                    break;
                case ConsoleKey.UpArrow:
                    HandleUpArrow(document, view);
                    break;
                case ConsoleKey.DownArrow:
                    HandleDownArrow(document, view);
                    break;
                case ConsoleKey.Backspace:
                    HandleBackspace(document, view);
                    break;
                case ConsoleKey.Delete:
                    HandleDelete(document, view);
                    break;
                case ConsoleKey.Home:
                    HandleHome(document, view);
                    break;
                case ConsoleKey.End:
                    HandleEnd(document, view);
                    break;
                case ConsoleKey.Tab:
                    HandleTab(document, view);
                    break;
                case ConsoleKey.Escape:
                    HandleEscape(document, view);
                    break;
                case ConsoleKey.PageUp:
                    HandlePageUp(document, view);
                    break;
                case ConsoleKey.PageDown:
                    HandlePageDown(document, view);
                    break;
                default:
                    break;
            }
        } else if (key.Modifiers == (ConsoleModifiers.Control & ConsoleModifiers.Shift)) {
            switch (key.Key) {
                case ConsoleKey.Backspace:
                    HandleControlBackspace(document, view);
                    break;
                case ConsoleKey.Delete:
                    HandleControlDelete(document, view);
                    break;
                case ConsoleKey.Enter:
                    HandleControlShiftEnter(document, view);
                    break;
                case ConsoleKey.LeftArrow:
                    HandleControlLeftArrow(document, view);
                    break;
                case ConsoleKey.RightArrow:
                    HandleControlRightArrow(document, view);
                    break;
                default:
                    break;
            }
        } else if (key.Modifiers == ConsoleModifiers.Control) {
            switch (key.Key) {
                case ConsoleKey.Backspace:
                    HandleControlBackspace(document, view);
                    break;
                case ConsoleKey.Delete:
                    HandleControlDelete(document, view);
                    break;
                case ConsoleKey.Enter:
                    HandleControlEnter(document, view);
                    break;
                case ConsoleKey.LeftArrow:
                    HandleControlLeftArrow(document, view);
                    break;
                case ConsoleKey.RightArrow:
                    HandleControlRightArrow(document, view);
                    break;
                case ConsoleKey.C:
                    HandleControlC(document, view);
                    break;
                default:
                    break;
            }
        } else if (key.Modifiers == ConsoleModifiers.Shift) {
            switch (key.Key) {
                case ConsoleKey.Enter:
                    InsertLine(document, view);
                    break;
                case ConsoleKey.Backspace:
                    HandleBackspace(document, view);
                    break;
                case ConsoleKey.Delete:
                    HandleDelete(document, view);
                    break;
                case ConsoleKey.Tab:
                    HandleShiftTab(document, view);
                    break;
                case ConsoleKey.LeftArrow:
                    HandleLeftArrow(document, view);
                    break;
                case ConsoleKey.RightArrow:
                    HandleRightArrow(document, view);
                    break;
                default:
                    break;
            }
        } else if (key.Modifiers == ConsoleModifiers.Alt) {
            switch (key.Key) {
                case ConsoleKey.Enter:
                    HandleAltEnter(document, view);
                    break;
                default:
                    break;
            }
        }

        if (key.Key != ConsoleKey.Backspace && key.KeyChar >= ' ')
            HandleTyping(document, view, key.KeyChar.ToString());
    }

    private void HandleControlShiftEnter(ObservableCollection<string> document, SubmissionView view) {
        SpecialEscapeSequence();
    }

    private void HandleAltEnter(ObservableCollection<string> document, SubmissionView view) {
        SpecialEscapeSequence();
    }

    private void HandleControlC(ObservableCollection<string> document, SubmissionView view) {
        if (!_done)
            SpecialEscapeSequence();
        else
            // Normal ctrl + c behavior
            System.Environment.Exit(1);
    }

    private void HandleShiftTab(ObservableCollection<string> document, SubmissionView view) {
        var line = document[view.currentLine];
        var whitespace = line.Length - line.TrimStart().Length;
        var remainingSpaces = whitespace % TabWidth;

        if (remainingSpaces == 0 && whitespace > 0)
            remainingSpaces = 4;

        AddChange(document, view.currentLine, 0, line.Length, line.Substring(remainingSpaces));
        document[view.currentLine] = line.Substring(remainingSpaces);
        view.currentCharacter -= remainingSpaces;

        view.currentTypingTabbing--;
    }

    private void HandleControlRightArrow(ObservableCollection<string> document, SubmissionView view) {
        var line = document[view.currentLine];

        if (view.currentCharacter <= line.Length - 1) {
            var offset = GetWordBoundaryFront(document, view);
            view.currentCharacter += offset;
        }
    }

    private void HandleControlDelete(ObservableCollection<string> document, SubmissionView view) {
        var lineIndex = view.currentLine;
        var line = document[lineIndex];
        var start = view.currentCharacter;

        if (start >= line.Length) {
            if (view.currentLine == document.Count - 1)
                return;

            var nextLine = document[view.currentLine + 1];
            AddChange(document, view.currentLine, document[view.currentLine].Length, 0, nextLine);
            document[view.currentLine] += nextLine;
            AddRemoveLineChange(document, view.currentLine + 1);
            document.RemoveAt(view.currentLine + 1);

            return;
        }

        var offset = GetWordBoundaryFront(document, view, strict: true);
        var before = line.Substring(0, start);
        var after = line.Substring(start + offset);

        if (ContainsOpening(line.Substring(start, offset)))
            view.currentBlockTabbing.Clear();

        AddChange(document, lineIndex, 0, line.Length, before + after);
        document[lineIndex] = before + after;
    }

    private void HandleControlLeftArrow(ObservableCollection<string> document, SubmissionView view) {
        if (view.currentCharacter > 0) {
            var offset = GetWordBoundaryBack(document, view);
            view.currentCharacter -= offset;
        }
    }

    private void HandleControlBackspace(ObservableCollection<string> document, SubmissionView view) {
        var start = view.currentCharacter;

        if (start == 0) {
            if (view.currentLine == 0)
                return;

            var currentLine = document[view.currentLine];
            var previousLine = document[view.currentLine - 1];
            AddRemoveLineChange(document, view.currentLine);
            document.RemoveAt(view.currentLine);
            view.currentLine--;
            AddChange(document, view.currentLine, 0, previousLine.Length, previousLine + currentLine);
            document[view.currentLine] = previousLine + currentLine;
            view.currentCharacter = previousLine.Length;
        } else {
            var offset = GetWordBoundaryBack(document, view, strict: true);

            var lineIndex = view.currentLine;
            var line = document[lineIndex];
            var before = line.Substring(0, start - offset);
            var after = line.Substring(start);

            if (ContainsOpening(line.Substring(start - offset, offset)))
                view.currentBlockTabbing.Clear();

            AddChange(document, lineIndex, 0, line.Length, before + after);
            document[lineIndex] = before + after;
            view.currentCharacter -= offset;
        }
    }

    private bool ContainsOpening(string line) {
        return line.Contains('{') || line.Contains('(') || line.Contains('[');
    }

    private int GetWordBoundaryFront(ObservableCollection<string> document, SubmissionView view, bool strict = false) {
        var line = document[view.currentLine];
        var maxLength = line.Length - 1;
        var start = view.currentCharacter;
        var offset = 0;

        char GetChar(int extraOffset = 0) {
            return line.Substring(GetPos(extraOffset), 1).Single();
        }

        int GetPos(int extraOffset = 0) {
            return start + offset + extraOffset;
        }

        var current = GetChar();

        bool IsTokenName(char current) {
            if (char.IsLetterOrDigit(current) || current == '_')
                return true;

            return false;
        }

        if (!strict) {
            while (char.IsWhiteSpace(current)) {
                if (GetPos() > maxLength)
                    return offset;

                offset++;

                if (GetPos() > maxLength)
                    return offset;

                current = GetChar();
            }

            if (GetPos() < maxLength) {
                if (char.IsPunctuation(current) && IsTokenName(GetChar(1)))
                    offset++;
            }
        } else {
            if (GetPos() < maxLength) {
                if (char.IsWhiteSpace(current)) {
                    if (!char.IsWhiteSpace(GetChar(1))) {
                        offset++;
                    } else {
                        while (char.IsWhiteSpace(current)) {
                            if (GetPos() > maxLength)
                                return offset;

                            offset++;

                            if (GetPos() > maxLength)
                                return offset;

                            current = GetChar();
                        }

                        return offset;
                    }
                }
            }
        }

        current = GetChar();

        if (char.IsLetterOrDigit(current)) {
            while (GetPos() <= maxLength) {
                offset++;

                if (GetPos() > maxLength)
                    break;

                current = GetChar();

                if (!IsTokenName(current))
                    break;
            }

            return offset;
        } else if (char.IsPunctuation(current)) {
            while (GetPos() <= maxLength) {
                offset++;

                if (GetPos() > maxLength)
                    break;

                current = GetChar();

                if (!char.IsPunctuation(current))
                    break;
            }
        }

        return offset;
    }

    private int GetWordBoundaryBack(ObservableCollection<string> document, SubmissionView view, bool strict = false) {
        var line = document[view.currentLine];
        var start = view.currentCharacter;
        var offset = 1;

        char GetChar(int extraOffset = 0) {
            return line.Substring(GetPos(extraOffset), 1).Single();
        }

        int GetPos(int extraOffset = 0) {
            return start - offset - extraOffset;
        }

        var current = GetChar();

        bool IsTokenName(char current) {
            if (char.IsLetterOrDigit(current) || current == '_')
                return true;

            return false;
        }

        if (!strict) {
            while (char.IsWhiteSpace(current)) {
                offset++;

                if (GetPos() == 0)
                    return offset;

                current = GetChar();

                if (GetPos() == 0)
                    return offset;
            }

            if (GetPos() > 1) {
                if (char.IsPunctuation(current) && IsTokenName(GetChar(1)))
                    offset++;
            }
        } else {
            if (GetPos() > 1) {
                if (char.IsWhiteSpace(current)) {
                    if (!char.IsWhiteSpace(GetChar(1))) {
                        offset++;
                    } else {
                        while (GetPos() > 0) {
                            offset++;

                            if (GetPos(1) < 0)
                                break;

                            var previous = GetChar(1);

                            if (!char.IsWhiteSpace(previous))
                                break;
                        }

                        return offset;
                    }
                }
            }
        }

        current = GetChar();

        if (char.IsLetterOrDigit(current)) {
            while (GetPos() > 0) {
                offset++;

                if (GetPos(1) < 0)
                    break;

                var previous = GetChar(1);

                if (!IsTokenName(previous))
                    break;
            }

            return offset;
        } else if (char.IsPunctuation(current)) {
            while (GetPos() > 0) {
                offset++;

                if (GetPos(1) < 0)
                    break;

                var previous = GetChar(1);

                if (!char.IsPunctuation(previous))
                    break;
            }
        }

        return offset;
    }

    private void HandlePageDown(ObservableCollection<string> document, SubmissionView view) {
        _submissionHistoryIndex++;

        if (_submissionHistoryIndex > _submissionHistory.Count - 1)
            _submissionHistoryIndex = 0;

        UpdateDocumentFromHistory(document, view);
    }

    private void HandlePageUp(ObservableCollection<string> document, SubmissionView view) {
        _submissionHistoryIndex--;

        if (_submissionHistoryIndex < 0)
            _submissionHistoryIndex = _submissionHistory.Count - 1;

        UpdateDocumentFromHistory(document, view);
    }

    private void UpdateDocumentFromHistory(ObservableCollection<string> document, SubmissionView view) {
        if (_submissionHistory.Count == 0)
            return;

        AddClearChange(document);
        document.Clear();

        var historyItem = _submissionHistory[_submissionHistoryIndex];
        var lines = historyItem.Split(Environment.NewLine);

        foreach (var line in lines) {
            AddInsertLineChange(document, document.Count, line);
            document.Add(line);
        }

        view.currentLine = document.Count - 1;
        view.currentCharacter = document[view.currentLine].Length;
    }

    private void HandleEscape(ObservableCollection<string> document, SubmissionView view) {
        AddClearChange(document);
        document.Clear();
        document.Add(string.Empty);
        view.currentLine = 0;
        view.currentCharacter = 0;
    }

    private void HandleTab(ObservableCollection<string> document, SubmissionView view) {
        var start = view.currentCharacter;
        var remainingSpaces = TabWidth - start % TabWidth;
        var line = document[view.currentLine];
        var newLine = line.Insert(start, new string(' ', remainingSpaces));
        AddChange(document, view.currentLine, 0, line.Length, newLine);
        document[view.currentLine] = newLine;
        view.currentCharacter += remainingSpaces;

        view.currentTypingTabbing++;
    }

    private void HandleHome(ObservableCollection<string> document, SubmissionView view) {
        view.currentCharacter = 0;
    }

    private void HandleEnd(ObservableCollection<string> document, SubmissionView view) {
        view.currentCharacter = document[view.currentLine].Length;
    }

    private void HandleDelete(ObservableCollection<string> document, SubmissionView view) {
        var lineIndex = view.currentLine;
        var line = document[lineIndex];
        var start = view.currentCharacter;

        if (start >= line.Length) {
            if (view.currentLine == document.Count - 1)
                return;

            var nextLine = document[view.currentLine + 1];
            AddChange(document, view.currentLine, document[view.currentLine].Length, 0, nextLine);
            document[view.currentLine] += nextLine;
            AddRemoveLineChange(document, view.currentLine + 1);
            document.RemoveAt(view.currentLine + 1);

            return;
        }

        var before = line.Substring(0, start);
        var after = line.Substring(start + 1);

        if (ContainsOpening(line.Substring(start, 1)))
            view.currentBlockTabbing.Clear();

        AddChange(document, lineIndex, 0, line.Length, before + after);
        document[lineIndex] = before + after;
    }

    private void HandleBackspace(ObservableCollection<string> document, SubmissionView view) {
        var start = view.currentCharacter;

        if (start == 0) {
            if (view.currentLine == 0)
                return;

            var currentLine = document[view.currentLine];
            var previousLine = document[view.currentLine - 1];
            AddRemoveLineChange(document, view.currentLine);
            document.RemoveAt(view.currentLine);
            view.currentLine--;
            AddChange(document, view.currentLine, 0, previousLine.Length, previousLine + currentLine);
            document[view.currentLine] = previousLine + currentLine;
            view.currentCharacter = previousLine.Length;
        } else {
            var lineIndex = view.currentLine;
            var line = document[lineIndex];

            var offset = GetTabBoundaryBack(document, view);

            var before = line.Substring(0, start - offset);
            var after = line.Substring(start);

            if (ContainsOpening(line.Substring(start - offset, offset)))
                view.currentBlockTabbing.Clear();

            AddChange(document, lineIndex, 0, line.Length, before + after);
            document[lineIndex] = before + after;
            view.currentCharacter -= offset;
        }
    }

    private int GetTabBoundaryBack(ObservableCollection<string> document, SubmissionView view) {
        var line = document[view.currentLine];
        var maxOffset = line.Length % TabWidth;

        if (maxOffset == 0)
            maxOffset = TabWidth;

        var start = view.currentCharacter;
        var offset = 1;

        while (offset < maxOffset) {
            offset++;

            if (start - offset - 1 < 0)
                break;

            var previous = line.Substring(start - offset - 1, 1).Single();

            if (!char.IsWhiteSpace(previous))
                break;
        }

        if (offset <= start)
            if (string.IsNullOrWhiteSpace(line.Substring(start - offset, offset)))
                return offset;

        return 1;
    }

    private void HandleControlEnter(ObservableCollection<string> document, SubmissionView view) {
        _done = true;
    }

    private void HandleTyping(ObservableCollection<string> document, SubmissionView view, string text) {
        if (document[view.currentLine].Length >= Console.WindowWidth - 3)
            return;

        var lineIndex = view.currentLine;

        Dictionary<char, char> pairs = new Dictionary<char, char>(){
            {'{', '}'},
            {'[', ']'},
            {'(', ')'}
        };

        if (text == "{" || text == "(" || text == "[")
            view.currentBlockTabbing.Push((pairs[text.Single()], view.currentTypingTabbing));

        if ((text == "}" || text == ")" || text == "]")) {
            var foundPair = false;

            if (view.currentBlockTabbing.Count > 0) {
                var targetTabbing = view.currentBlockTabbing.Pop();

                while (targetTabbing.Item1 != text.Single()) {
                    if (view.currentBlockTabbing.Count == 0)
                        break;

                    targetTabbing = view.currentBlockTabbing.Pop();
                }

                if (targetTabbing.Item1 == text.Single()) {
                    foundPair = true;

                    if (string.IsNullOrWhiteSpace(document[lineIndex])) {
                        for (int i = view.currentTypingTabbing; i > targetTabbing.Item2; i--)
                            HandleShiftTab(document, view);
                    }
                }
            }

            if (!foundPair && string.IsNullOrWhiteSpace(document[lineIndex])) {
                AddChange(document, lineIndex, 0, document[lineIndex].Length, "");
                document[lineIndex] = "";
                view.currentCharacter = 0;
            }
        }

        var start = view.currentCharacter;
        AddChange(document, lineIndex, start, 0, text);
        document[lineIndex] = document[lineIndex].Insert(start, text);
        view.currentCharacter += text.Length;
    }

    private void HandleDownArrow(ObservableCollection<string> document, SubmissionView view) {
        if (view.currentLine < document.Count - 1)
            view.currentLine++;
    }

    private void HandleUpArrow(ObservableCollection<string> document, SubmissionView view) {
        if (view.currentLine > 0)
            view.currentLine--;
    }

    private void HandleRightArrow(ObservableCollection<string> document, SubmissionView view) {
        var line = document[view.currentLine];

        if (view.currentCharacter <= line.Length - 1)
            view.currentCharacter++;
    }

    private void HandleLeftArrow(ObservableCollection<string> document, SubmissionView view) {
        if (view.currentCharacter > 0)
            view.currentCharacter--;
    }

    private void HandleEnter(ObservableCollection<string> document, SubmissionView view) {
        var submissionText = string.Join(Environment.NewLine, document);

        if (submissionText.StartsWith('#') || IsCompleteSubmission(submissionText)) {
            _done = true;
            return;
        }

        InsertLine(document, view);
    }

    private void InsertLine(ObservableCollection<string> document, SubmissionView view) {
        var beginning = document[view.currentLine].Substring(0, view.currentCharacter);
        var remainder = document[view.currentLine].Substring(view.currentCharacter);
        AddChange(document, view.currentLine, 0, document[view.currentLine].Length, beginning);
        document[view.currentLine] = beginning;

        var lineIndex = view.currentLine + 1;
        AddInsertLineChange(document, lineIndex, remainder);
        document.Insert(lineIndex, remainder);
        view.currentCharacter = 0;
        view.currentLine = lineIndex;

        var previousLine = document[view.currentLine - 1];
        var whitespace = (previousLine.Length - previousLine.TrimStart().Length) / TabWidth;

        if (previousLine.Length > 0 && ContainsOpening(previousLine[^1].ToString())) {
            view.currentTypingTabbing++;
            whitespace++;
        }

        HandleTyping(document, view, new String(' ', whitespace * TabWidth));
    }

    private void AddChange(
        ObservableCollection<string> document, int lineIndex, int startIndex, int oldLength, string newText) {
        var position = startIndex;

        for (int i = 0; i < lineIndex; i++) {
            position += document[i].Length + Environment.NewLine.Length;

            if (i > 0)
                position += Environment.NewLine.Length;
        }

        _changes.Add(new TextChange(new TextSpan(position, oldLength), newText));
    }

    private void AddClearChange(ObservableCollection<string> document) {
        var length = 0;

        foreach (var line in document)
            length += line.Length + Environment.NewLine.Length;

        _changes.Add(new TextChange(new TextSpan(0, length), ""));
    }

    private void AddInsertLineChange(ObservableCollection<string> document, int lineIndex, string newText) {
        var position = 0;

        for (int i = 0; i < lineIndex; i++) {
            position += document[i].Length;

            if (i > 0)
                position += Environment.NewLine.Length;
        }

        _changes.Add(new TextChange(new TextSpan(position, 0), newText + Environment.NewLine));
    }

    private void AddRemoveLineChange(ObservableCollection<string> document, int lineIndex) {
        var position = 0;

        for (int i = 0; i < lineIndex; i++) {
            position += document[i].Length;

            if (i > 0)
                position += Environment.NewLine.Length;
        }

        _changes.Add(
            new TextChange(new TextSpan(position, document[lineIndex].Length + Environment.NewLine.Length), "")
        );
    }

    private void EvaluateReplCommand(string line) {
        var position = 1;
        var sb = new StringBuilder();
        var inQuotes = false;
        var args = new List<string>();

        while (position < line.Length) {
            var c = line[position];
            var l = position + 1 >= line.Length ? '\0' : line[position + 1];

            if (char.IsWhiteSpace(c)) {
                if (!inQuotes) {
                    var arg = sb.ToString();

                    if (!string.IsNullOrWhiteSpace(arg))
                        args.Add(arg);

                    sb.Clear();
                } else {
                    sb.Append(c);
                }
            } else if (c == '\"') {
                if (!inQuotes) {
                    inQuotes = true;
                } else if (l == '\"') {
                    sb.Append(c);
                    position++;
                } else if (inQuotes) {
                    inQuotes = false;
                }
            } else {
                sb.Append(c);
            }

            position++;
        }

        args.Add(sb.ToString());

        var commandName = args.FirstOrDefault();

        if (args.Count > 0)
            args.RemoveAt(0);

        var command = _metaCommands.SingleOrDefault(mc => mc.name == commandName);

        if (command == null) {
            handle.diagnostics.Push(new BelteDiagnostic(global::Repl.Diagnostics.Error.UnknownReplCommand(line)));

            if (diagnosticHandle != null)
                diagnosticHandle(handle, "repl");
            else
                handle.diagnostics.Clear();

            return;
        }

        var parameters = command.method.GetParameters();

        if (args.Count != parameters.Length) {
            if (args.Count == command.method.GetParameters()
                .Where(t => !t.HasDefaultValue).ToArray().Length) {
                foreach (var parameter in command.method.GetParameters().Where(p => p.HasDefaultValue))
                    args.Add(parameter.DefaultValue.ToString());
            } else {
                var parameterNames = string.Join(" ", parameters.Select(p => $"<{p.Name}>"));
                handle.diagnostics.Push(
                    new BelteDiagnostic(global::Repl.Diagnostics.Error.WrongArgumentCount(command.name, parameterNames))
                );

                if (diagnosticHandle != null)
                    diagnosticHandle(handle, "repl");
                else
                    handle.diagnostics.Clear();

                return;
            }
        }

        var instance = command.method.IsStatic ? null : this;
        command.method.Invoke(instance, args.ToArray());
    }
}
