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

const ROOT_SOURCE_CONTEXT_UNAVAILABLE: &str =
    "source context unavailable: root source not attached to frontend diagnostic";

/// Map an HIR diagnostic kind to a user-visible prefix string.
pub(crate) fn hir_diagnostic_prefix(kind: &hew_hir::HirDiagnosticKind) -> &'static str {
    match kind {
        hew_hir::HirDiagnosticKind::NotYetImplemented { .. } => "E_NOT_YET_IMPLEMENTED",
        hew_hir::HirDiagnosticKind::ConstIntegerEvaluation { .. } => "E_CONST_INITIALIZER",
        hew_hir::HirDiagnosticKind::TuplePatternArityMismatch { .. }
        | hew_hir::HirDiagnosticKind::TuplePatternNonTupleValue => "E_TUPLE_PATTERN_MISMATCH",
        hew_hir::HirDiagnosticKind::EnumVariantConstructorShapeMismatch { .. }
        | hew_hir::HirDiagnosticKind::EnumVariantConstructorMissingField { .. }
        | hew_hir::HirDiagnosticKind::EnumVariantConstructorUnknownField { .. }
        | hew_hir::HirDiagnosticKind::EnumVariantConstructorArityMismatch { .. } => {
            "E_ENUM_VARIANT_CONSTRUCTOR"
        }
        _ => "E_HIR",
    }
}

/// Build the user-facing message for an HIR diagnostic.
///
/// For a `NotYetImplemented` gap, the message frames the gap as a current
/// compiler limitation — not a problem with the user's code — and never leaks
/// the Rust `{:?}` field names (`construct`, `owning_pass`). Other kinds fall
/// back to the stable prefix plus any attached note.
pub(crate) fn hir_diagnostic_user_message(diagnostic: &hew_hir::HirDiagnostic) -> String {
    if let hew_hir::HirDiagnosticKind::NotYetImplemented { construct, .. } = &diagnostic.kind {
        let mut message = format!(
            "`{construct}` is not yet supported by the Hew compiler \
             (this is a current Hew limitation, not your code)"
        );
        if !diagnostic.note.is_empty() {
            message.push_str(": ");
            message.push_str(&diagnostic.note);
        }
        return message;
    }

    let prefix = hir_diagnostic_prefix(&diagnostic.kind);
    if diagnostic.note.is_empty() {
        prefix.to_string()
    } else {
        format!("{prefix}: {}", diagnostic.note)
    }
}

/// Which diagnostic channel a codegen-front failure reports on.
///
/// `hew-codegen-rs/src` is fenced (D342): the classification lives here in
/// `hew-cli`, beside [`codegen_diagnostic_prefix`], rather than as a method
/// on `CodegenError` itself. `LlvmVerify`/`FailClosed`/`FailClosedAt` are the
/// compiler disagreeing with the LLVM module it built (Internal).
/// `Unsupported`/`UnsupportedAt` are legal Hew
/// this backend does not lower yet (Limitation). `Link`/`Io`/`TargetSetup`/
/// `Llvm` are the build environment (a missing linker, a full disk, an
/// unresolvable target triple) — nobody's channel but the user's, since
/// there is no fourth channel for "the environment is wrong".
#[must_use]
pub(crate) fn codegen_channel(
    error: &hew_codegen_rs::CodegenError,
) -> hew_types::error::DiagChannel {
    use hew_types::error::DiagChannel;
    match error {
        hew_codegen_rs::CodegenError::LlvmVerify(_)
        | hew_codegen_rs::CodegenError::FailClosed(_)
        | hew_codegen_rs::CodegenError::FailClosedAt { .. } => DiagChannel::Internal,
        hew_codegen_rs::CodegenError::Unsupported(_)
        | hew_codegen_rs::CodegenError::UnsupportedAt { .. } => DiagChannel::Limitation,
        hew_codegen_rs::CodegenError::Link(_)
        | hew_codegen_rs::CodegenError::Io(_)
        | hew_codegen_rs::CodegenError::TargetSetup { .. }
        | hew_codegen_rs::CodegenError::Llvm(_) => DiagChannel::User,
    }
}

/// Render a codegen-emit error (`emit_module` failure path).
///
/// FAIL-CLOSED RENDER RULE: renders a `^^^` caret only when the error carries a
/// source span, `source_path` can be read, and the span's start byte is within
/// that source's length.  Any of those conditions failing degrades to a bare
/// `E_NOT_YET_IMPLEMENTED:` plain-line — never a caret against the wrong source.
///
/// CROSS-MODULE SAFETY: the span is attached upstream ONLY for a function
/// carrying `SourceOrigin::RootUnit`, so it provably indexes the root source
/// named by `source_path`.  A non-root function reaches here spanless (the span
/// was stripped at `build_module_for_target`), so its error renders bare.  The
/// bounds check is belt-and-braces; the carried origin is the attribution.
pub(crate) fn render_codegen_emit_error(
    error: &hew_codegen_rs::CodegenError,
    source_path: Option<&std::path::Path>,
) {
    let prefix = codegen_channel(error).prefix();
    if let Some((span_start, span_end)) = error.span() {
        if let Some(path) = source_path {
            if let Ok(text) = std::fs::read_to_string(path) {
                let start = span_start as usize;
                let end = span_end as usize;
                if start < text.len() {
                    let filename = path.to_str().unwrap_or("<unknown>");
                    let span = start..end.min(text.len());
                    render_diagnostic(
                        &text,
                        filename,
                        &span,
                        &format!("{prefix}{error}"),
                        &[],
                        &[],
                    );
                    return;
                }
            }
        }
    }
    emit_plain_diagnostic_line(&format!("{prefix}E_NOT_YET_IMPLEMENTED: {error}"));
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

fn should_use_ansi_output(
    stderr_is_terminal: bool,
    no_color_set: bool,
    diagnostic_capture_active: bool,
) -> bool {
    !diagnostic_capture_active && !no_color_set && stderr_is_terminal
}

pub(crate) fn use_ansi_diagnostics() -> bool {
    should_use_ansi_output(
        std::io::stderr().is_terminal(),
        std::env::var_os("NO_COLOR").is_some(),
        diagnostic_capture_active(),
    )
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
    pub source: &'a str,
    pub filename: &'a str,
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
        let (note_line, note_col) = offset_to_line_col(note.source, note.span.start);
        diag_println(&format!(
            "{bold}{note_filename}:{note_line}:{note_col}:{reset} {cyan}note{reset}{bold}: {message}{reset}",
            bold = palette.bold,
            cyan = palette.cyan,
            message = note.message,
            note_filename = note.filename,
            reset = palette.reset,
        ));
        render_source_underline(note.source, note.span, note_line, &palette);
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
        let (note_line, note_col) = offset_to_line_col(note.source, note.span.start);
        diag_println(&format!(
            "{bold}{note_filename}:{note_line}:{note_col}:{reset} {cyan}note{reset}{bold}: {message}{reset}",
            bold = palette.bold,
            cyan = palette.cyan,
            message = note.message,
            note_filename = note.filename,
            reset = palette.reset,
        ));
        render_source_underline(note.source, note.span, note_line, &palette);
    }

    for suggestion in suggestions {
        diag_println(&format!(
            "  {cyan}= help{reset}: {suggestion}",
            cyan = palette.cyan,
            reset = palette.reset,
        ));
    }
}

/// Print diagnostic-only `info[HEW-PERF-001]` stack-allocation hints to stderr.
///
/// One line per [`hew_types::check::StackHint`], formatted as:
///
/// ```text
/// <file>:<line>:<col>: info[HEW-PERF-001]: binding `<name>` (<class>) could be stack-allocated
/// ```
///
/// Severity is `info`. This function never affects exit code or stdout. It is
/// invoked only when the user passes `--show-stack-hints` to `hew check` or
/// `hew run`. The diagnostic code (`HEW-PERF-001`) and the `file:line:col`
/// prefix are stable UX surface; the trailing message text is best-effort.
pub fn print_stack_hints(source: &str, filename: &str, hints: &[hew_types::check::StackHint]) {
    let palette = diagnostic_palette();
    for hint in hints {
        let (line, col) = offset_to_line_col(source, hint.span_key.start);
        let class_label = stack_hint_alloc_class_label(&hint.alloc_class);
        let name = if hint.binding_name.is_empty() {
            // Bindings produced by destructuring patterns have no single name;
            // render a placeholder so the line stays well-formed.
            "<destructured>"
        } else {
            hint.binding_name.as_str()
        };
        diag_println(&format!(
            "{bold}{filename}:{line}:{col}:{reset} {blue}info[HEW-PERF-001]{reset}{bold}: \
             binding `{name}` ({class_label}) could be stack-allocated{reset}",
            bold = palette.bold,
            blue = palette.blue,
            reset = palette.reset,
        ));
    }
}

fn stack_hint_alloc_class_label(class: &hew_types::check::AllocationClass) -> &'static str {
    use hew_types::check::AllocationClass;
    // Stable display labels matching the AllocationClass discriminant names
    // documented on the type. `Stack` and `Indeterminate` should never reach
    // this function (the walker filters them out before recording a hint),
    // but we render them defensively rather than panicking.
    match class {
        AllocationClass::Vec => "Vec",
        AllocationClass::String => "string",
        AllocationClass::HashMap => "HashMap",
        AllocationClass::HashSet => "HashSet",
        AllocationClass::Rc => "Rc",
        AllocationClass::ClosureEnv => "ClosureEnv",
        AllocationClass::Stack => "Stack",
        AllocationClass::Indeterminate => "Indeterminate",
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
            source,
            filename,
            span: s,
            message: msg.as_str(),
        })
        .collect();
    render_diagnostic(source, filename, span, message, &notes, suggestions);
}

/// Build a map from dotted module path to `(source_text, display_filename)` for
/// every non-root module in the program that has an on-disk source file.
///
/// // WASM-TODO(diagnostic-source-map): `std::fs` is unavailable in WASM / no-fs contexts, so this
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

/// Notes for the internal-compiler-error channel: the program is not at
/// fault, so the note says so and asks for a bug report.
fn push_lowering_invariant_notes(notes: &mut Vec<String>, block: Option<u32>) {
    if let Some(block) = block {
        notes.push(format!("block: {block}"));
    }
    notes.push(
        "this is a defect in the Hew compiler, not in your program; please report it at \
         https://github.com/hew-lang/hew/issues with this message and, if you can, the \
         source file"
            .to_string(),
    );
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
        let notes = diagnostic
            .notes
            .iter()
            .map(|(span, message, source_module)| {
                let (note_source, note_filename) = source_module
                    .as_deref()
                    .and_then(|module| module_source_map.get(module))
                    .map_or((source, filename), |(source, filename)| {
                        (source.as_str(), filename.as_str())
                    });
                DiagnosticNote {
                    source: note_source,
                    filename: note_filename,
                    span,
                    message,
                }
            })
            .collect::<Vec<_>>();
        // See `crate::compile::render_frontend_type_diagnostic` for the
        // matching prefix on the primary `hew check`/`compile` path — kept
        // in lockstep so the two TypeError renderers never disagree about
        // one diagnostic's channel prefix.
        let message = format!(
            "{}{}",
            diagnostic.kind.channel().prefix(),
            diagnostic.message
        );
        match diagnostic.severity {
            hew_types::error::Severity::Warning => render_warning(
                source,
                filename,
                &diagnostic.span,
                &message,
                &notes,
                &diagnostic.suggestions,
            ),
            hew_types::error::Severity::Error => render_diagnostic(
                source,
                filename,
                &diagnostic.span,
                &message,
                &notes,
                &diagnostic.suggestions,
            ),
        }
    }
}

fn hir_source_context_unavailable_note(diagnostic: &hew_hir::HirDiagnostic) -> String {
    diagnostic.source_module.as_ref().map_or_else(
        || ROOT_SOURCE_CONTEXT_UNAVAILABLE.to_string(),
        |module| format!("source context unavailable: module '{module}' not in module_source_map"),
    )
}

fn hir_diagnostic_message(diagnostic: &hew_hir::HirDiagnostic) -> String {
    // For a `NotYetImplemented` gap, keep the stable `E_NOT_YET_IMPLEMENTED`
    // family code (grep-able by tooling) but use the limitation-framed body and
    // never leak the Rust `{:?}` field names. Other kinds fall back to the
    // stable prefix plus any attached note.
    if matches!(
        diagnostic.kind,
        hew_hir::HirDiagnosticKind::NotYetImplemented { .. }
    ) {
        return format!(
            "{}: {}",
            hir_diagnostic_prefix(&diagnostic.kind),
            hir_diagnostic_user_message(diagnostic)
        );
    }
    let prefix = hir_diagnostic_prefix(&diagnostic.kind);
    if diagnostic.note.is_empty() {
        prefix.to_string()
    } else {
        format!("{prefix}: {}", diagnostic.note)
    }
}

/// Render a HIR diagnostic using source context when the frontend was able to
/// resolve the diagnostic's source module. Non-root source-map misses are
/// rendered explicitly rather than falling back to the root file.
///
/// Routes through the JSON sink when `--format=json` is active. The `HIR kind`
/// note uses the stable kind string (never the Rust `{:?}` struct payload), so
/// no Debug payload reaches user output on either path.
pub(crate) fn render_hir_diagnostic(
    source: Option<&str>,
    filename: Option<&str>,
    diagnostic: &hew_hir::HirDiagnostic,
) {
    if crate::diagnostic_json::json_output_active() {
        crate::diagnostic_json::push_json_diagnostic(crate::diagnostic_json::from_hir_diagnostic(
            source, filename, diagnostic,
        ));
        return;
    }

    let channel = diagnostic.kind.channel();
    let message = format!("{}{}", channel.prefix(), hir_diagnostic_message(diagnostic));
    let kind_note = format!("HIR kind: {}", diagnostic.kind.kind_string());
    let mut suggestions = vec![kind_note];
    if channel == hew_types::error::DiagChannel::Internal {
        push_lowering_invariant_notes(&mut suggestions, None);
    }
    if let (Some(source), Some(filename)) = (source, filename) {
        render_diagnostic_with_raw_notes(
            source,
            filename,
            &diagnostic.span,
            &message,
            &diagnostic.secondary_spans,
            &suggestions,
        );
        return;
    }

    emit_plain_diagnostic_line(&format!("error: {message}"));
    for note in &suggestions {
        emit_plain_diagnostic_line(&format!("  = note: {note}"));
    }
    emit_plain_diagnostic_line(&format!(
        "  = note: {}",
        hir_source_context_unavailable_note(diagnostic)
    ));
    for (_, label) in &diagnostic.secondary_spans {
        emit_plain_diagnostic_line(&format!("  = note: {label}"));
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
pub(crate) fn offset_to_line_col(source: &str, offset: usize) -> (usize, usize) {
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
    fn type_diagnostic_note_uses_its_own_module_source() {
        let mut diagnostic = sample_type_error();
        diagnostic.source_module = Some("std.net".to_string());
        diagnostic.notes.push((
            6..17,
            "first declaration is here".to_string(),
            Some("std.stream".to_string()),
        ));

        let mut module_source_map = ModuleSourceMap::new();
        module_source_map.insert(
            "std.net".to_string(),
            ("net_call()\n".to_string(), "std/net/net.hew".to_string()),
        );
        module_source_map.insert(
            "std.stream".to_string(),
            (
                "first\nstream_decl\n".to_string(),
                "std/stream.hew".to_string(),
            ),
        );

        start_diagnostic_capture();
        render_type_diagnostics_with_sources(
            "fn main() {}\n",
            "main.hew",
            &[diagnostic],
            &module_source_map,
        );
        let captured = finish_diagnostic_capture();

        assert!(captured.contains("std/net/net.hew:1:1: error"));
        assert!(captured.contains("std/stream.hew:2:1: note: first declaration is here"));
        assert!(captured.contains("stream_decl"));
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

    #[test]
    fn hir_source_map_miss_reports_unavailable_note() {
        let diagnostic = hew_hir::HirDiagnostic::new(
            hew_hir::HirDiagnosticKind::UnresolvedInferenceVar,
            0..1,
            "probe",
        )
        .with_source_module(Some("dep".to_string()));

        start_diagnostic_capture();
        render_hir_diagnostic(None, None, &diagnostic);
        let captured = finish_diagnostic_capture();

        assert!(captured.contains("error: E_HIR: probe"));
        assert!(captured.contains("HIR kind: UnresolvedInferenceVar"));
        assert!(
            captured.contains("source context unavailable: module 'dep' not in module_source_map")
        );
    }

    #[test]
    fn hir_secondary_spans_use_primary_source_context() {
        let diagnostic = hew_hir::HirDiagnostic::new(
            hew_hir::HirDiagnosticKind::UnresolvedInferenceVar,
            0..4,
            "primary",
        )
        .with_secondary_spans(vec![(
            5..9,
            "secondary uses primary source module".to_string(),
        )]);

        start_diagnostic_capture();
        render_hir_diagnostic(Some("abcd\nefgh\n"), Some("dep.hew"), &diagnostic);
        let captured = finish_diagnostic_capture();

        assert!(captured.contains("dep.hew:1:1: error: E_HIR: primary"));
        assert!(captured.contains("dep.hew:2:1: note: secondary uses primary source module"));
        assert!(captured.contains("efgh"));
    }

    #[test]
    fn user_syntax_hir_diagnostics_have_specific_codes() {
        let tuple = hew_hir::HirDiagnostic::new(
            hew_hir::HirDiagnosticKind::TuplePatternArityMismatch {
                expected: 2,
                actual: 1,
            },
            0..1,
            "tuple pattern element count does not match tuple value arity",
        );
        let constructor = hew_hir::HirDiagnostic::new(
            hew_hir::HirDiagnosticKind::EnumVariantConstructorArityMismatch {
                variant: "Pair".to_string(),
                expected: 2,
                actual: 1,
            },
            0..1,
            "tuple-variant constructor called with the wrong number of arguments",
        );

        assert_eq!(
            hir_diagnostic_prefix(&tuple.kind),
            "E_TUPLE_PATTERN_MISMATCH"
        );
        assert_eq!(
            hir_diagnostic_prefix(&constructor.kind),
            "E_ENUM_VARIANT_CONSTRUCTOR"
        );
        assert_eq!(tuple.kind.kind_string(), "TuplePatternArityMismatch");
        assert_eq!(
            constructor.kind.kind_string(),
            "EnumVariantConstructorArityMismatch"
        );
        assert_eq!(
            hir_diagnostic_message(&tuple),
            "E_TUPLE_PATTERN_MISMATCH: tuple pattern element count does not match tuple value arity"
        );
        assert_eq!(
            hir_diagnostic_message(&constructor),
            "E_ENUM_VARIANT_CONSTRUCTOR: tuple-variant constructor called with the wrong number of arguments"
        );
    }

    #[test]
    fn ansi_output_requires_tty_without_no_color_or_capture() {
        assert!(should_use_ansi_output(true, false, false));
        assert!(!should_use_ansi_output(false, false, false));
        assert!(!should_use_ansi_output(true, true, false));
        assert!(!should_use_ansi_output(true, false, true));
    }
}
