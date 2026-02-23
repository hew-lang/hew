//! Pretty error rendering with source spans and ANSI colors.
//!
//! Produces Rust/Elm-style diagnostics with `^^^` underlines pointing at the
//! relevant source location.

use std::ops::Range;

// ANSI color helpers
const RED: &str = "\x1b[1;31m";
const YELLOW: &str = "\x1b[1;33m";
const BLUE: &str = "\x1b[1;34m";
const CYAN: &str = "\x1b[1;36m";
const BOLD: &str = "\x1b[1m";
const RESET: &str = "\x1b[0m";

/// A secondary note attached to a diagnostic.
#[derive(Debug)]
pub struct DiagnosticNote<'a> {
    pub span: &'a Range<usize>,
    pub message: &'a str,
}

/// Render a diagnostic message with source context and span underline.
///
/// Prints to stderr:
/// ```text
/// filename:line:col: error: message
///   line_num | source line
///            | ^^^^^^^^^^^
///   = note: ...
///   = help: ...
/// ```
pub fn render_diagnostic(
    source: &str,
    filename: &str,
    span: &Range<usize>,
    message: &str,
    notes: &[DiagnosticNote<'_>],
    suggestions: &[String],
) {
    let (line, col) = offset_to_line_col(source, span.start);

    // Header: filename:line:col: error: message
    eprintln!("{BOLD}{filename}:{line}:{col}:{RESET} {RED}error{RESET}{BOLD}: {message}{RESET}");

    render_source_underline(source, span, line);

    // Secondary notes with their own spans
    for note in notes {
        let (note_line, note_col) = offset_to_line_col(source, note.span.start);
        eprintln!(
            "{BOLD}{filename}:{note_line}:{note_col}:{RESET} {CYAN}note{RESET}{BOLD}: {}{RESET}",
            note.message
        );
        render_source_underline(source, note.span, note_line);
    }

    // Suggestions
    for suggestion in suggestions {
        eprintln!("  {CYAN}= help{RESET}: {suggestion}");
    }
}

/// Render a warning message with source context and span underline.
///
/// Same layout as [`render_diagnostic`] but prints `warning` in yellow.
pub fn render_warning(
    source: &str,
    filename: &str,
    span: &Range<usize>,
    message: &str,
    notes: &[DiagnosticNote<'_>],
    suggestions: &[String],
) {
    let (line, col) = offset_to_line_col(source, span.start);

    eprintln!(
        "{BOLD}{filename}:{line}:{col}:{RESET} {YELLOW}warning{RESET}{BOLD}: {message}{RESET}"
    );

    render_source_underline(source, span, line);

    for note in notes {
        let (note_line, note_col) = offset_to_line_col(source, note.span.start);
        eprintln!(
            "{BOLD}{filename}:{note_line}:{note_col}:{RESET} {CYAN}note{RESET}{BOLD}: {}{RESET}",
            note.message
        );
        render_source_underline(source, note.span, note_line);
    }

    for suggestion in suggestions {
        eprintln!("  {CYAN}= help{RESET}: {suggestion}");
    }
}

/// Render the source line and `^^^` underline for a span.
fn render_source_underline(source: &str, span: &Range<usize>, line: usize) {
    let lines: Vec<&str> = source.lines().collect();

    if line == 0 {
        return;
    }

    // Handle EOF / empty file — show a marker line.
    if line > lines.len() {
        let line_num = line.to_string();
        let padding = " ".repeat(line_num.len());
        eprintln!(" {BLUE}{line_num} |{RESET}");
        eprintln!(" {padding} {BLUE}|{RESET} {RED}^{RESET}");
        return;
    }

    let source_line = lines[line - 1];
    // Strip trailing \r for CRLF files so underline widths align with display.
    let display_line = source_line.strip_suffix('\r').unwrap_or(source_line);
    let line_num = line.to_string();
    let padding = " ".repeat(line_num.len());

    // Print the source line
    eprintln!(" {BLUE}{line_num} |{RESET} {display_line}");

    // Compute underline position within the line using character counts,
    // not byte offsets, so multi-byte UTF-8 characters align correctly.
    let line_start = line_start_offset(source, line);
    let start_byte = span
        .start
        .saturating_sub(line_start)
        .min(display_line.len());
    let start_chars = display_line
        .get(..start_byte)
        .map_or(0, |s| s.chars().count());

    // For multi-line spans, underline to end of first line; for empty spans, show one caret.
    let end_byte = if span.start == span.end {
        start_byte + 1
    } else {
        let line_end = line_start + display_line.len();
        span.end.min(line_end).saturating_sub(line_start)
    };
    let end_chars = display_line
        .get(..end_byte.min(display_line.len()))
        .map_or(start_chars + 1, |s| s.chars().count());

    let underline_len = end_chars.saturating_sub(start_chars).max(1);

    eprintln!(
        " {padding} {BLUE}|{RESET} {}{RED}{}{RESET}",
        " ".repeat(start_chars),
        "^".repeat(underline_len),
    );
}

/// Convert a byte offset to a 1-based (line, column) pair.
/// Handles both `\n` and `\r\n` line endings; `\r` before `\n` is not counted
/// as a column so that the column number matches the displayed line content.
fn offset_to_line_col(source: &str, offset: usize) -> (usize, usize) {
    let offset = offset.min(source.len());
    let mut line = 1;
    let mut col = 1;
    let bytes = source.as_bytes();

    for (i, ch) in source.char_indices() {
        if i >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 1;
        } else if ch == '\r' && bytes.get(i + 1) == Some(&b'\n') {
            // Skip \r in a \r\n pair — the \n will advance the line.
        } else {
            col += 1;
        }
    }

    (line, col)
}

/// Return the byte offset of the start of a 1-based line number.
fn line_start_offset(source: &str, line: usize) -> usize {
    let mut current_line = 1;
    for (i, ch) in source.char_indices() {
        if current_line == line {
            return i;
        }
        if ch == '\n' {
            current_line += 1;
        }
    }
    // Past end — return source length
    source.len()
}
