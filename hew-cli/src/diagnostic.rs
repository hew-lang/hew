//! Pretty error rendering with source spans and ANSI colours.
//!
//! Produces Rust/Elm-style diagnostics with `^^^` underlines pointing at the
//! relevant source location.

use std::cell::RefCell;
use std::collections::HashMap;
use std::io::IsTerminal;
use std::ops::Range;
use std::sync::atomic::{AtomicUsize, Ordering};

#[path = "diagnostic_renderer.rs"]
mod renderer;
pub(crate) use renderer::{render_ownership, LineIndexScope};

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

// ---------------------------------------------------------------------------
// Error limit
// ---------------------------------------------------------------------------

/// Errors (and, separately, warnings) printed per run unless `--error-limit`
/// says otherwise.
pub(crate) const DEFAULT_ERROR_LIMIT: usize = 100;

static ERROR_LIMIT: AtomicUsize = AtomicUsize::new(DEFAULT_ERROR_LIMIT);

/// Set how many errors and warnings are printed; `0` prints them all.
pub(crate) fn set_error_limit(limit: usize) {
    ERROR_LIMIT.store(limit, Ordering::Relaxed);
}

/// The diagnostics to print: the first `limit` errors and the first `limit`
/// warnings, in their original order. A note after the last one reports what
/// was left out.
pub(crate) fn limit_diagnostics<T>(items: &[T], is_error: impl Fn(&T) -> bool) -> Vec<&T> {
    limit_diagnostics_at(ERROR_LIMIT.load(Ordering::Relaxed), items, is_error)
}

fn limit_diagnostics_at<T>(limit: usize, items: &[T], is_error: impl Fn(&T) -> bool) -> Vec<&T> {
    if limit == 0 || items.len() <= limit {
        return items.iter().collect();
    }
    let (mut errors, mut warnings) = (0usize, 0usize);
    let (mut hidden_errors, mut hidden_warnings) = (0usize, 0usize);
    let shown = items
        .iter()
        .filter(|item| {
            let (seen, hidden) = if is_error(item) {
                (&mut errors, &mut hidden_errors)
            } else {
                (&mut warnings, &mut hidden_warnings)
            };
            *seen += 1;
            if *seen <= limit {
                return true;
            }
            *hidden += 1;
            false
        })
        .collect();
    if hidden_errors + hidden_warnings > 0 {
        diag_println(&format!(
            "note: {hidden_errors} more errors and {hidden_warnings} more warnings not shown; \
             `--error-limit 0` shows everything"
        ));
    }
    shown
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
    diag_println(&hew_compile::source_path_policy().display_text(s));
}

pub(crate) fn display_filename(filename: &str) -> String {
    if filename.starts_with('<') && filename.ends_with('>') {
        return filename.to_string();
    }
    hew_compile::display_source_path(std::path::Path::new(filename))
        .display()
        .to_string()
}

const ROOT_SOURCE_CONTEXT_UNAVAILABLE: &str =
    "source context unavailable: root source not attached to frontend diagnostic";

/// Map an HIR diagnostic kind to a user-visible prefix string.
pub(crate) fn hir_diagnostic_prefix(kind: &hew_hir::HirDiagnosticKind) -> &'static str {
    match kind {
        hew_hir::HirDiagnosticKind::ConstIntegerEvaluation { .. }
        | hew_hir::HirDiagnosticKind::ConstDependencyCycle { .. } => "E_CONST_INITIALIZER",
        hew_hir::HirDiagnosticKind::TuplePatternArityMismatch { .. }
        | hew_hir::HirDiagnosticKind::TuplePatternNonTupleValue => "E_TUPLE_PATTERN_MISMATCH",
        hew_hir::HirDiagnosticKind::EnumVariantConstructorShapeMismatch { .. }
        | hew_hir::HirDiagnosticKind::EnumVariantConstructorMissingField { .. }
        | hew_hir::HirDiagnosticKind::EnumVariantConstructorUnknownField { .. }
        | hew_hir::HirDiagnosticKind::EnumVariantConstructorArityMismatch { .. } => {
            "E_ENUM_VARIANT_CONSTRUCTOR"
        }
        hew_hir::HirDiagnosticKind::RecursiveLambdaActorHandle { .. } => "E_RECURSIVE_LAMBDA_ACTOR",
        _ => "E_HIR",
    }
}

/// Build the user-facing message for an HIR diagnostic.
///
pub(crate) fn hir_diagnostic_user_message(diagnostic: &hew_hir::HirDiagnostic) -> String {
    if let hew_hir::HirDiagnosticKind::RecursiveLambdaActorHandle { name } = &diagnostic.kind {
        return format!(
            "{}: this lambda actor names `{name}`, its own handle, so its state would own the \
             way to reach it: {}",
            hir_diagnostic_prefix(&diagnostic.kind),
            diagnostic.note
        );
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

/// The stable code for a codegen-emit failure, classified the same three ways
/// as [`codegen_channel`] (#3189): `LlvmVerify`/`FailClosed`/`FailClosedAt`
/// are the compiler disagreeing with the LLVM module it built, `Unsupported`/
/// `UnsupportedAt` identify a Hew construct outside this backend's lowering
/// surface, and the rest describe the build environment.
#[must_use]
pub(crate) fn codegen_error_code(error: &hew_codegen_rs::CodegenError) -> &'static str {
    use hew_types::error::DiagChannel;
    match codegen_channel(error) {
        DiagChannel::Internal => "E_CODEGEN_FAIL_CLOSED",
        DiagChannel::Limitation => "E_CODEGEN_UNSUPPORTED",
        DiagChannel::User => "E_CODEGEN_ENV",
    }
}

/// Render an emission failure against its retained authored source, when available.
/// Foreign or unavailable source locations remain unpositioned.
pub(crate) fn render_codegen_emit_error(
    error: &hew_codegen_rs::CodegenError,
    source: Option<(&str, &str)>,
) {
    let prefix = codegen_channel(error).prefix();
    let code = codegen_error_code(error);
    if let (Some((span_start, span_end)), Some((text, filename))) = (error.span(), source) {
        let start = span_start as usize;
        let end = span_end as usize;
        if start < text.len() {
            let span = start..end.min(text.len());
            render_diagnostic(
                text,
                filename,
                &span,
                &format!("{prefix}{code}: {error}"),
                &[],
                &[],
            );
            return;
        }
    }
    emit_plain_diagnostic_line(&format!("{prefix}{code}: {error}"));
}

/// Render the category selected by semantic lowering. Root-only spans follow
/// the same source identity rule as code generation errors.
pub(crate) fn render_lowering_error(
    error: &hew_sir::SirLoweringError,
    span: Option<&Range<usize>>,
    source: &str,
    filename: &str,
) -> hew_types::error::DiagChannel {
    use hew_sir::SirLoweringError;
    use hew_types::error::DiagChannel;
    let (channel, code) = match error {
        SirLoweringError::Internal(_) => (DiagChannel::Internal, "E_SIR_ICE"),
        SirLoweringError::Ownership { kind, .. } => (DiagChannel::User, kind.code()),
        SirLoweringError::Limit(_) => (DiagChannel::Limitation, "E_LIMIT_SPECIALIZATION"),
    };
    let message = error.message();
    let prefix = channel.prefix();
    if let Some(span) = span {
        if span.start < source.len() {
            if crate::diagnostic_json::json_output_active() {
                crate::diagnostic_json::push_json_diagnostic(
                    crate::diagnostic_json::coded_span_diagnostic(
                        code, message, channel, source, filename, span,
                    ),
                );
            } else {
                let end = span.end.min(source.len());
                render_diagnostic(
                    source,
                    filename,
                    &(span.start..end),
                    &format!("{prefix}{code}: {message}"),
                    &[],
                    &[],
                );
            }
            return channel;
        }
    }
    if crate::diagnostic_json::json_output_active() {
        crate::diagnostic_json::push_json_diagnostic(
            crate::diagnostic_json::coded_message_diagnostic(code, message, channel),
        );
    } else {
        emit_plain_diagnostic_line(&format!("{prefix}{code}: {message}"));
    }
    channel
}

// ANSI colour helpers
const RED: &str = "\x1b[1;31m";
const YELLOW: &str = "\x1b[1;33m";
const BLUE: &str = "\x1b[1;34m";
const CYAN: &str = "\x1b[1;36m";
const BOLD: &str = "\x1b[1m";
const RESET: &str = "\x1b[0m";

#[derive(serde::Serialize)]
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

pub(crate) type ModuleSourceMap = HashMap<String, (std::sync::Arc<str>, String)>;

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
    render_diagnostic_impl(source, filename, span, message, notes, suggestions, None);
}

/// Render a source diagnostic with its stable error code in the header.
pub fn render_coded_diagnostic(
    source: &str,
    filename: &str,
    span: &Range<usize>,
    code: &str,
    message: &str,
    notes: &[DiagnosticNote<'_>],
    suggestions: &[String],
) {
    render_diagnostic_impl(
        source,
        filename,
        span,
        message,
        notes,
        suggestions,
        Some(code),
    );
}

fn render_diagnostic_impl(
    source: &str,
    filename: &str,
    span: &Range<usize>,
    message: &str,
    notes: &[DiagnosticNote<'_>],
    suggestions: &[String],
    code: Option<&str>,
) {
    let level = code.map_or_else(|| "error".to_string(), |code| format!("error[{code}]"));
    render_presentation(source, filename, span, message, &level, notes, suggestions);
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
    render_presentation(
        source,
        filename,
        span,
        message,
        "warning",
        notes,
        suggestions,
    );
}

fn render_presentation(
    source: &str,
    filename: &str,
    span: &Range<usize>,
    message: &str,
    level: &str,
    notes: &[DiagnosticNote<'_>],
    suggestions: &[String],
) {
    let rendered = renderer::render(
        &DiagnosticNote {
            source,
            filename,
            span,
            message,
        },
        level,
        notes,
        suggestions,
        &diagnostic_palette(),
    );
    DIAG_CAPTURE.with(|capture| {
        if let Some(buffer) = capture.borrow_mut().as_mut() {
            buffer.push_str(&rendered);
        } else {
            eprint!("{rendered}");
        }
    });
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
    let filename = display_filename(filename);
    let palette = diagnostic_palette();
    // `source` and `filename` describe only the selected root module. Imported
    // hints have offsets relative to their own files, so rendering them here
    // would falsely attribute them to the root (and may point beyond its EOF).
    for hint in hints.iter().filter(|hint| hint.span_key.module_idx == 0) {
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
            return (mod_src.as_ref(), mod_file.as_str());
        }
    }
    (root_source, root_filename)
}

/// Render parser diagnostics using the shared CLI diagnostic layout.
pub fn render_parse_diagnostics(source: &str, filename: &str, errors: &[hew_parser::ParseError]) {
    let _lines = LineIndexScope::new();
    for err in limit_diagnostics(errors, |err| err.severity == hew_parser::Severity::Error) {
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
    let _lines = LineIndexScope::new();
    for diagnostic in limit_diagnostics(diagnostics, |diagnostic| {
        diagnostic.severity == hew_types::error::Severity::Error
    }) {
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
                        (source.as_ref(), filename.as_str())
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
    if let hew_hir::HirDiagnosticKind::RecursiveLambdaActorHandle { name } = &diagnostic.kind {
        return format!(
            "{}: this lambda actor names `{name}`, its own handle, so its state would own the \
             way to reach it: {}",
            hir_diagnostic_prefix(&diagnostic.kind),
            diagnostic.note
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
            ("pub fn oops() {}\n".into(), "dep.hew".to_string()),
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
            ("net_call()\n".into(), "std/net/net.hew".to_string()),
        );
        module_source_map.insert(
            "std.stream".to_string(),
            ("first\nstream_decl\n".into(), "std/stream.hew".to_string()),
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
    fn error_limit_keeps_the_first_errors_and_warnings_in_order() {
        start_diagnostic_capture();
        // (is_error, id)
        let items = [
            (true, 1),
            (false, 2),
            (true, 3),
            (true, 4),
            (false, 5),
            (false, 6),
        ];
        let shown: Vec<i32> = limit_diagnostics_at(2, &items, |item| item.0)
            .into_iter()
            .map(|item| item.1)
            .collect();
        let note = finish_diagnostic_capture();
        assert_eq!(shown, [1, 2, 3, 5]);
        assert!(
            note.contains("1 more errors and 1 more warnings not shown"),
            "{note}"
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
