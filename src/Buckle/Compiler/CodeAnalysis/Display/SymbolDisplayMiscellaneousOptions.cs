using System;

namespace Buckle.CodeAnalysis.Display;

[Flags]
internal enum SymbolDisplayMiscellaneousOptions : byte {
    None = 0,
    IncludeKeywords = 1 << 0,
    SimplifyNullable = 1 << 1,
}
