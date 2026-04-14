//! Pretty error rendering with source spans and ANSI colours.
//!
//! Produces Rust/Elm-style diagnostics with `^^^` underlines pointing at the
//! relevant source location.

use std::cell::RefCell;
use std::collections::HashMap;
use std::io::IsTerminal;
use std::ops::Range;

// ---------------------------------------------------------------------------
// Thread-local diagnostic capture
// ---------------------------------------------------------------------------
//
// When active, all `diag_println` calls append to a string buffer instead of
// writing to stderr.  Used by `hew eval --json` to collect diagnostic text
// into the JSON run contract without altering the normal (non-JSON) path.

thread_local! {
    static DIAG_CAPTURE: RefCell<Option<String>> = const { RefCell::new(None) };
}

/// Activate per-thread diagnostic capture.
///
/// All subsequent [`diag_println`] calls on this thread append to an internal
/// buffer instead of writing to stderr.  Call [`finish_diagnostic_capture`] to
/// retrieve the accumulated text and deactivate capture.
///
/// Capture is not re-entrant: calling this while capture is already active
/// resets the buffer.
pub(crate) fn start_diagnostic_capture() {
    DIAG_CAPTURE.with(|c| *c.borrow_mut() = Some(String::new()));
}

/// Deactivate per-thread diagnostic capture and return the accumulated text.
///
/// Returns an empty string if capture was not active.
pub(crate) fn finish_diagnostic_capture() -> String {
    DIAG_CAPTURE.with(|c| c.borrow_mut().take().unwrap_or_default())
}

/// Write a diagnostic line to the active capture buffer, or to stderr if no
/// capture is active.
fn diag_println(s: &str) {
    DIAG_CAPTURE.with(|c| {
        if let Some(ref mut buf) = *c.borrow_mut() {
            buf.push_str(s);
            buf.push('\n');
        } else {
            eprintln!("{s}");
        }
    });
}

/// Emit a plain diagnostic line through the capture-aware sink.
pub(crate) fn emit_plain_diagnostic_line(s: &str) {
    diag_println(s);
}

// ANSI colour helpers
const RED: &str = "\x1b[1;31m";
const YELLOW: &str = "\x1b[1;33m";
const BLUE: &str = "\x1b[1;34m";
const CYAN: &str = "\x1b[1;36m";
const BOLD: &str = "\x1b[1m";
const RESET: &str = "\x1b[0m";

struct DiagnosticPalette {
    red: &'static str,
    yellow: &'static str,
    blue: &'static str,
    cyan: &'static str,
    bold: &'static str,
    reset: &'static str,
}

fn diagnostic_capture_active() -> bool {
    DIAG_CAPTURE.with(|c| c.borrow().is_some())
}

fn use_ansi_diagnostics() -> bool {
    !diagnostic_capture_active()
        && std::env::var_os("NO_COLOR").is_none()
        && std::io::stderr().is_terminal()
}

fn diagnostic_palette() -> DiagnosticPalette {
    if use_ansi_diagnostics() {
        DiagnosticPalette {
            red: RED,
            yellow: YELLOW,
            blue: BLUE,
            cyan: CYAN,
            bold: BOLD,
            reset: RESET,
        }
    } else {
        DiagnosticPalette {
            red: "",
            yellow: "",
            blue: "",
            cyan: "",
            bold: "",
            reset: "",
        }
    }
}

/// A secondary note attached to a diagnostic.
#[derive(Debug)]
pub struct DiagnosticNote<'a> {
    pub span: &'a Range<usize>,
    pub message: &'a str,
}

pub(crate) type ModuleSourceMap = HashMap<String, (String, String)>;

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
    let palette = diagnostic_palette();
    let (line, col) = offset_to_line_col(source, span.start);

    // Header: filename:line:col: error: message
    diag_println(&format!(
        "{bold}{filename}:{line}:{col}:{reset} {red}error{reset}{bold}: {message}{reset}",
        bold = palette.bold,
        red = palette.red,
        reset = palette.reset,
    ));

    render_source_underline(source, span, line, &palette);

    // Secondary notes with their own spans
    for note in notes {
        let (note_line, note_col) = offset_to_line_col(source, note.span.start);
        diag_println(&format!(
            "{bold}{filename}:{note_line}:{note_col}:{reset} {cyan}note{reset}{bold}: {message}{reset}",
            bold = palette.bold,
            cyan = palette.cyan,
            message = note.message,
            reset = palette.reset,
        ));
        render_source_underline(source, note.span, note_line, &palette);
    }

    // Suggestions
    for suggestion in suggestions {
        diag_println(&format!(
            "  {cyan}= help{reset}: {suggestion}",
            cyan = palette.cyan,
            reset = palette.reset,
        ));
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
    let palette = diagnostic_palette();
    let (line, col) = offset_to_line_col(source, span.start);

    diag_println(&format!(
        "{bold}{filename}:{line}:{col}:{reset} {yellow}warning{reset}{bold}: {message}{reset}",
        bold = palette.bold,
        reset = palette.reset,
        yellow = palette.yellow,
    ));

    render_source_underline(source, span, line, &palette);

    for note in notes {
        let (note_line, note_col) = offset_to_line_col(source, note.span.start);
        diag_println(&format!(
            "{bold}{filename}:{note_line}:{note_col}:{reset} {cyan}note{reset}{bold}: {message}{reset}",
            bold = palette.bold,
            cyan = palette.cyan,
            message = note.message,
            reset = palette.reset,
        ));
        render_source_underline(source, note.span, note_line, &palette);
    }

    for suggestion in suggestions {
        diag_println(&format!(
            "  {cyan}= help{reset}: {suggestion}",
            cyan = palette.cyan,
            reset = palette.reset,
        ));
    }
}

/// Render an error diagnostic where notes are provided as `(span, message)` pairs.
///
/// Convenience wrapper around [`render_diagnostic`] for callers that hold notes as
/// raw `(Range<usize>, String)` tuples rather than [`DiagnosticNote`] slices.
pub fn render_diagnostic_with_raw_notes(
    source: &str,
    filename: &str,
    span: &Range<usize>,
    message: &str,
    raw_notes: &[(Range<usize>, String)],
    suggestions: &[String],
) {
    let notes: Vec<DiagnosticNote<'_>> = raw_notes
        .iter()
        .map(|(s, msg)| DiagnosticNote {
            span: s,
            message: msg.as_str(),
        })
        .collect();
    render_diagnostic(source, filename, span, message, &notes, suggestions);
}

/// Render a warning diagnostic where notes are provided as `(span, message)` pairs.
///
/// Convenience wrapper around [`render_warning`] for callers that hold notes as
/// raw `(Range<usize>, String)` tuples rather than [`DiagnosticNote`] slices.
pub fn render_warning_with_raw_notes(
    source: &str,
    filename: &str,
    span: &Range<usize>,
    message: &str,
    raw_notes: &[(Range<usize>, String)],
    suggestions: &[String],
) {
    let notes: Vec<DiagnosticNote<'_>> = raw_notes
        .iter()
        .map(|(s, msg)| DiagnosticNote {
            span: s,
            message: msg.as_str(),
        })
        .collect();
    render_warning(source, filename, span, message, &notes, suggestions);
}

/// Build a map from dotted module path to `(source_text, display_filename)` for
/// every non-root module in the program that has an on-disk source file.
///
/// // WASM-TODO: `std::fs` is unavailable in WASM / no-fs contexts, so this
/// // map is empty there and non-root diagnostics fall back to root-source
/// // rendering until the WASM diagnostic pass grows a source-provider hook.
pub(crate) fn build_module_source_map(program: &hew_parser::ast::Program) -> ModuleSourceMap {
    let Some(ref module_graph) = program.module_graph else {
        return ModuleSourceMap::new();
    };

    let mut map = ModuleSourceMap::new();
    for mod_id in &module_graph.topo_order {
        if *mod_id == module_graph.root {
            continue;
        }
        let Some(module) = module_graph.modules.get(mod_id) else {
            continue;
        };
        let Some(path) = module.source_paths.first() else {
            continue;
        };
        if let Ok(text) = std::fs::read_to_string(path) {
            map.insert(mod_id.path.join("."), (text, path.display().to_string()));
        }
    }
    map
}

fn type_diagnostic_source<'a>(
    root_source: &'a str,
    root_filename: &'a str,
    diagnostic: &hew_types::TypeError,
    module_source_map: &'a ModuleSourceMap,
) -> (&'a str, &'a str) {
    if let Some(ref mod_name) = diagnostic.source_module {
        if let Some((mod_src, mod_file)) = module_source_map.get(mod_name.as_str()) {
            return (mod_src.as_str(), mod_file.as_str());
        }
    }
    (root_source, root_filename)
}

/// Render parser diagnostics using the shared CLI diagnostic layout.
pub fn render_parse_diagnostics(source: &str, filename: &str, errors: &[hew_parser::ParseError]) {
    for err in errors {
        let hints: Vec<String> = err.hint.iter().cloned().collect();
        match err.severity {
            hew_parser::Severity::Warning => {
                render_warning(source, filename, &err.span, &err.message, &[], &hints);
            }
            hew_parser::Severity::Error => {
                render_diagnostic(source, filename, &err.span, &err.message, &[], &hints);
            }
        }
    }
}

/// Render type-check diagnostics using the shared CLI diagnostic layout.
pub fn render_type_diagnostics(source: &str, filename: &str, diagnostics: &[hew_types::TypeError]) {
    let module_source_map = ModuleSourceMap::new();
    render_type_diagnostics_with_sources(source, filename, diagnostics, &module_source_map);
}

/// Render type-check diagnostics, routing non-root diagnostics to their source
/// modules when `source_module` attribution is available.
pub(crate) fn render_type_diagnostics_with_sources(
    root_source: &str,
    root_filename: &str,
    diagnostics: &[hew_types::TypeError],
    module_source_map: &ModuleSourceMap,
) {
    for diagnostic in diagnostics {
        let (source, filename) =
            type_diagnostic_source(root_source, root_filename, diagnostic, module_source_map);
        match diagnostic.severity {
            hew_types::error::Severity::Warning => render_warning_with_raw_notes(
                source,
                filename,
                &diagnostic.span,
                &diagnostic.message,
                &diagnostic.notes,
                &diagnostic.suggestions,
            ),
            hew_types::error::Severity::Error => render_diagnostic_with_raw_notes(
                source,
                filename,
                &diagnostic.span,
                &diagnostic.message,
                &diagnostic.notes,
                &diagnostic.suggestions,
            ),
        }
    }
}

/// Render the source line and `^^^` underline for a span.
fn render_source_underline(
    source: &str,
    span: &Range<usize>,
    line: usize,
    palette: &DiagnosticPalette,
) {
    let lines: Vec<&str> = source.lines().collect();

    if line == 0 {
        return;
    }

    // Handle EOF / empty file — show a marker line.
    if line > lines.len() {
        let line_num = line.to_string();
        let padding = " ".repeat(line_num.len());
        diag_println(&format!(
            " {blue}{line_num} |{reset}",
            blue = palette.blue,
            reset = palette.reset,
        ));
        diag_println(&format!(
            " {padding} {blue}|{reset} {red}^{reset}",
            blue = palette.blue,
            red = palette.red,
            reset = palette.reset,
        ));
        return;
    }

    let source_line = lines[line - 1];
    // Strip trailing \r for CRLF files so underline widths align with display.
    let display_line = source_line.strip_suffix('\r').unwrap_or(source_line);
    let line_num = line.to_string();
    let padding = " ".repeat(line_num.len());

    // Print the source line
    diag_println(&format!(
        " {blue}{line_num} |{reset} {display_line}",
        blue = palette.blue,
        reset = palette.reset,
    ));

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

    diag_println(&format!(
        " {padding} {blue}|{reset} {}{red}{}{reset}",
        " ".repeat(start_chars),
        "^".repeat(underline_len),
        blue = palette.blue,
        red = palette.red,
        reset = palette.reset,
    ));
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

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_type_error() -> hew_types::TypeError {
        hew_types::TypeError::new(
            hew_types::error::TypeErrorKind::UndefinedFunction,
            0..4,
            "cannot find function `oops` in this scope",
        )
    }

    #[test]
    fn type_diagnostic_source_prefers_attributed_module_source() {
        let mut diagnostic = sample_type_error();
        diagnostic.source_module = Some("dep".to_string());

        let mut module_source_map = ModuleSourceMap::new();
        module_source_map.insert(
            "dep".to_string(),
            ("pub fn oops() {}\n".to_string(), "dep.hew".to_string()),
        );

        let (source, filename) = type_diagnostic_source(
            "fn main() {}\n",
            "main.hew",
            &diagnostic,
            &module_source_map,
        );

        assert_eq!(source, "pub fn oops() {}\n");
        assert_eq!(filename, "dep.hew");
    }

    #[test]
    fn type_diagnostic_source_falls_back_to_root_when_module_missing() {
        let mut diagnostic = sample_type_error();
        diagnostic.source_module = Some("dep".to_string());
        let module_source_map = ModuleSourceMap::new();

        let (source, filename) = type_diagnostic_source(
            "fn main() {}\n",
            "main.hew",
            &diagnostic,
            &module_source_map,
        );

        assert_eq!(source, "fn main() {}\n");
        assert_eq!(filename, "main.hew");
    }

    #[test]
    fn captured_diagnostics_strip_ansi_sequences() {
        start_diagnostic_capture();
        render_diagnostic("oops()\n", "main.hew", &(0..4), "bad call", &[], &[]);
        let captured = finish_diagnostic_capture();

        assert!(
            !captured.contains("\u{1b}["),
            "captured diagnostics must not contain ANSI escapes: {captured:?}"
        );
        assert!(captured.contains("main.hew:1:1: error: bad call"));
    }
}
