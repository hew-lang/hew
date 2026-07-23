//! Machine-readable JSON diagnostics for `hew check`, `hew run`, and
//! `hew compile`.
//!
//! This is the `--format=json` surface. It reuses the same diagnostic
//! substrate that already feeds the LSP: each diagnostic carries a stable
//! `code` (the `kind` discriminant), a `severity`, a `file`, a byte-and-line
//! `span`, the user-facing `message`, secondary `notes`, and machine-checked
//! `fixes` computed by [`hew_analysis::code_actions::build_code_actions`] — the
//! exact engine the LSP uses for quick-fixes.
//!
//! ## Output contract
//!
//! When the active output format is [`OutputFormat::Json`], diagnostic render
//! sites append a [`JsonDiagnostic`] to a per-thread accumulator instead of
//! writing text to stderr. After the command's diagnostic phase completes, the
//! caller flushes the accumulator with [`flush_json_diagnostics`], which prints
//! a single JSON array to stdout. A clean program prints `[]`.
//!
//! The thread-local design mirrors the existing `DIAG_CAPTURE` mechanism in
//! [`crate::diagnostic`]: it lets every existing render call site participate in
//! JSON output without threading a `format` parameter through the entire
//! compile call graph.

use std::cell::RefCell;
use std::ops::Range;

use serde::Serialize;

use crate::args::DiagnosticFormat;
use crate::diagnostic::offset_to_line_col;

// ---------------------------------------------------------------------------
// Output-format + JSON accumulator thread-locals
// ---------------------------------------------------------------------------

thread_local! {
    /// The active diagnostic output format for the current thread. Defaults to
    /// `Text`; set once at the top of each `check`/`run`/`compile` command.
    static OUTPUT_FORMAT: RefCell<OutputFormat> = const { RefCell::new(OutputFormat::Text) };

    /// Accumulator for structured diagnostics when JSON output is active.
    static JSON_DIAGNOSTICS: RefCell<Vec<JsonDiagnostic>> = const { RefCell::new(Vec::new()) };
}

/// The diagnostic output mode for a command invocation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum OutputFormat {
    /// Human-readable rendering with source context (the default).
    Text,
    /// Machine-readable JSON array on stdout.
    Json,
}

impl From<DiagnosticFormat> for OutputFormat {
    fn from(format: DiagnosticFormat) -> Self {
        match format {
            DiagnosticFormat::Text => OutputFormat::Text,
            DiagnosticFormat::Json => OutputFormat::Json,
        }
    }
}

/// Set the active output format for the current thread.
pub(crate) fn set_output_format(format: OutputFormat) {
    OUTPUT_FORMAT.with(|f| *f.borrow_mut() = format);
}

/// Returns `true` when JSON diagnostic collection is active on this thread.
pub(crate) fn json_output_active() -> bool {
    OUTPUT_FORMAT.with(|f| *f.borrow() == OutputFormat::Json)
}

/// Append a structured diagnostic to the per-thread JSON accumulator.
pub(crate) fn push_json_diagnostic(diagnostic: JsonDiagnostic) {
    JSON_DIAGNOSTICS.with(|d| d.borrow_mut().push(diagnostic));
}

/// Flush the accumulated JSON diagnostics to stdout as a single array and
/// clear the accumulator. A clean program prints `[]`.
///
/// Called once at the end of a command's diagnostic phase, only when JSON
/// output is active.
pub(crate) fn flush_json_diagnostics() {
    let diagnostics = JSON_DIAGNOSTICS.with(|d| std::mem::take(&mut *d.borrow_mut()));
    // The array always serializes; `JsonDiagnostic` has no failing variants.
    match serde_json::to_string_pretty(&diagnostics) {
        Ok(rendered) => println!("{rendered}"),
        Err(error) => {
            // Fail closed: never silently drop diagnostics. Emit a minimal
            // valid JSON envelope describing the serialization failure so a
            // consuming agent still sees machine-parseable output.
            println!(
                "[{{\"code\":\"E_JSON_SERIALIZE\",\"severity\":\"error\",\"message\":{}}}]",
                serde_json::to_string(&error.to_string())
                    .unwrap_or_else(|_| "\"diagnostic serialization failed\"".to_string())
            );
        }
    }
}

// ---------------------------------------------------------------------------
// JSON schema
// ---------------------------------------------------------------------------

/// A source span expressed in both 1-based line/column and byte offsets.
///
/// Line/column is what a human or agent reads; byte offsets match the
/// `OffsetSpan` that the analysis/code-action substrate operates on, so a fix
/// edit can be applied directly against the source bytes.
#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub(crate) struct JsonSpan {
    pub start_line: usize,
    pub start_col: usize,
    pub end_line: usize,
    pub end_col: usize,
    pub start_byte: usize,
    pub end_byte: usize,
}

impl JsonSpan {
    fn from_range(source: &str, span: &Range<usize>) -> Self {
        let (start_line, start_col) = offset_to_line_col(source, span.start);
        let (end_line, end_col) = offset_to_line_col(source, span.end);
        Self {
            start_line,
            start_col,
            end_line,
            end_col,
            start_byte: span.start,
            end_byte: span.end,
        }
    }

    /// A zero span for diagnostics whose source context is unavailable.
    fn zero() -> Self {
        Self {
            start_line: 0,
            start_col: 0,
            end_line: 0,
            end_col: 0,
            start_byte: 0,
            end_byte: 0,
        }
    }
}

/// A secondary note attached to a diagnostic, optionally located.
#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub(crate) struct JsonNote {
    pub message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub span: Option<JsonSpan>,
}

/// A single text edit within a machine-checked fix.
#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub(crate) struct JsonEdit {
    pub span: JsonSpan,
    pub new_text: String,
}

/// A machine-checked fix the compiler can apply (mirrors the LSP quick-fix /
/// `CodeAction`).
#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub(crate) struct JsonFix {
    pub title: String,
    pub edits: Vec<JsonEdit>,
}

/// A structured, serializable diagnostic.
///
/// The shape mirrors the LSP `Diagnostic` substrate: `code` is the stable
/// `kind` discriminant, `source` names the compiler stage, `span` locates the
/// primary site, and `fixes` carries the same edits the LSP offers as
/// quick-fixes.
#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub(crate) struct JsonDiagnostic {
    /// Stable diagnostic code — the `kind` discriminant (e.g. `Mismatch`,
    /// `UndefinedVariable`, `NotYetImplemented`). Never a Rust `{:?}` payload.
    pub code: String,
    /// `"error"` or `"warning"`.
    pub severity: String,
    /// Compiler stage that emitted the diagnostic (`hew-parser`, `hew-types`,
    /// `hew-hir`, `hew-mir`).
    pub source: String,
    /// Source file the diagnostic is attributed to.
    pub file: String,
    /// Primary source span.
    pub span: JsonSpan,
    /// User-facing message — never a Rust `{:?}` Debug payload.
    pub message: String,
    /// Secondary notes (related locations / context).
    pub notes: Vec<JsonNote>,
    /// Machine-checked fixes the compiler can apply.
    pub fixes: Vec<JsonFix>,
}

// ---------------------------------------------------------------------------
// Conversions from compiler error types
// ---------------------------------------------------------------------------

const SEVERITY_ERROR: &str = "error";
const SEVERITY_WARNING: &str = "warning";

fn fixes_from_code_actions(
    source: &str,
    kind: Option<&str>,
    message: &str,
    span: &Range<usize>,
    suggestions: &[String],
) -> Vec<JsonFix> {
    let info = hew_analysis::code_actions::DiagnosticInfo {
        kind: kind.map(str::to_string),
        message: message.to_string(),
        span: hew_analysis::OffsetSpan {
            start: span.start,
            end: span.end,
        },
        suggestions: suggestions.to_vec(),
    };
    hew_analysis::code_actions::build_code_actions(source, &[info])
        .into_iter()
        .map(|action| JsonFix {
            title: action.title,
            edits: action
                .edits
                .into_iter()
                .map(|edit| JsonEdit {
                    span: JsonSpan::from_range(source, &(edit.span.start..edit.span.end)),
                    new_text: edit.new_text,
                })
                .collect(),
        })
        .collect()
}

/// Build a [`JsonDiagnostic`] from a free-form message that carries no source
/// span (e.g. import-resolution failures, manifest errors). The code is the
/// generic `E_MESSAGE` family and the span is zero.
pub(crate) fn message_diagnostic(message: &str) -> JsonDiagnostic {
    JsonDiagnostic {
        code: "E_MESSAGE".to_string(),
        severity: SEVERITY_ERROR.to_string(),
        source: "hew".to_string(),
        file: String::new(),
        span: JsonSpan::zero(),
        message: message.to_string(),
        notes: Vec::new(),
        fixes: Vec::new(),
    }
}

/// Build a [`JsonDiagnostic`] from a parser diagnostic.
pub(crate) fn from_parse_error(
    source: &str,
    filename: &str,
    error: &hew_parser::ParseError,
) -> JsonDiagnostic {
    let severity = match error.severity {
        hew_parser::Severity::Warning => SEVERITY_WARNING,
        hew_parser::Severity::Error => SEVERITY_ERROR,
    };
    let notes = error
        .hint
        .iter()
        .map(|hint| JsonNote {
            message: hint.clone(),
            span: None,
        })
        .collect();
    JsonDiagnostic {
        code: error.kind.as_kind_str().to_string(),
        severity: severity.to_string(),
        source: "hew-parser".to_string(),
        file: filename.to_string(),
        span: JsonSpan::from_range(source, &error.span),
        message: error.message.clone(),
        notes,
        fixes: Vec::new(),
    }
}

/// Build a [`JsonDiagnostic`] from a type-checker diagnostic, including any
/// machine-checked fixes the code-action engine can derive.
pub(crate) fn from_type_error(
    source: &str,
    filename: &str,
    error: &hew_types::TypeError,
) -> JsonDiagnostic {
    let severity = match error.severity {
        hew_types::error::Severity::Warning => SEVERITY_WARNING,
        hew_types::error::Severity::Error => SEVERITY_ERROR,
    };
    let code = error.kind.as_kind_str();
    let notes = error
        .notes
        .iter()
        .map(|(span, message)| JsonNote {
            message: message.clone(),
            span: Some(JsonSpan::from_range(source, span)),
        })
        .collect();
    let fixes = fixes_from_code_actions(
        source,
        Some(code),
        &error.message,
        &error.span,
        &error.suggestions,
    );
    JsonDiagnostic {
        code: code.to_string(),
        severity: severity.to_string(),
        source: "hew-types".to_string(),
        file: filename.to_string(),
        span: JsonSpan::from_range(source, &error.span),
        message: error.message.clone(),
        notes,
        fixes,
    }
}

/// Build a [`JsonDiagnostic`] from a HIR diagnostic. The `message` is the
/// user-facing limitation text (never a Rust `{:?}` payload).
pub(crate) fn from_hir_diagnostic(
    source: Option<&str>,
    filename: Option<&str>,
    diagnostic: &hew_hir::HirDiagnostic,
) -> JsonDiagnostic {
    let span = source.map_or_else(JsonSpan::zero, |src| {
        JsonSpan::from_range(src, &diagnostic.span)
    });
    let notes = source.map_or_else(Vec::new, |src| {
        diagnostic
            .secondary_spans
            .iter()
            .map(|(span, label)| JsonNote {
                message: label.clone(),
                span: Some(JsonSpan::from_range(src, span)),
            })
            .collect()
    });
    JsonDiagnostic {
        code: crate::diagnostic::hir_diagnostic_kind_string(&diagnostic.kind),
        severity: SEVERITY_ERROR.to_string(),
        source: "hew-hir".to_string(),
        file: filename.unwrap_or("<unknown>").to_string(),
        span,
        message: crate::diagnostic::hir_diagnostic_user_message(diagnostic),
        notes,
        fixes: Vec::new(),
    }
}

/// Build a [`JsonDiagnostic`] from a MIR diagnostic, located at its primary
/// site when source context is available.
#[allow(
    clippy::too_many_arguments,
    reason = "renders one MIR diagnostic: source/filename/span/code/message/notes/\
              context-notes plus the advisory-severity flag; each is an independent \
              input the JSON record carries verbatim"
)]
pub(crate) fn from_mir_diagnostic(
    source: Option<&str>,
    filename: Option<&str>,
    span: Option<&Range<usize>>,
    code: &str,
    message: &str,
    notes: &[(Range<usize>, String)],
    context_notes: &[String],
    is_advisory: bool,
) -> JsonDiagnostic {
    let primary_span = match (source, span) {
        (Some(src), Some(span)) => JsonSpan::from_range(src, span),
        _ => JsonSpan::zero(),
    };
    let mut json_notes: Vec<JsonNote> = Vec::new();
    if let Some(src) = source {
        for (span, message) in notes {
            json_notes.push(JsonNote {
                message: message.clone(),
                span: Some(JsonSpan::from_range(src, span)),
            });
        }
    }
    for note in context_notes {
        json_notes.push(JsonNote {
            message: note.clone(),
            span: None,
        });
    }
    // Advisory MIR diagnostics (obligation under-release leaks) carry the
    // `warning` severity — a compile-time diagnostic that does not fail the
    // build; all other MIR diagnostics are the hard `error` family.
    let severity = if is_advisory {
        SEVERITY_WARNING
    } else {
        SEVERITY_ERROR
    };
    JsonDiagnostic {
        code: code.to_string(),
        severity: severity.to_string(),
        source: "hew-mir".to_string(),
        file: filename.unwrap_or("<unknown>").to_string(),
        span: primary_span,
        message: message.to_string(),
        notes: json_notes,
        fixes: Vec::new(),
    }
}

/// Build a [`JsonDiagnostic`] for a MIR-stage lint warning.
///
/// Unlike [`from_mir_diagnostic`] — which is always the hard `error` family —
/// a lint carries its configured severity: `warning` by default, `error` when
/// promoted by `--deny`. The `code` is the lint's stable name (`dead_store`),
/// matching the `-A/-W/-D` selector and the HIR-stage lint codes.
pub(crate) fn from_mir_lint(
    source: &str,
    filename: &str,
    span: &Range<usize>,
    code: &str,
    message: &str,
    is_error: bool,
) -> JsonDiagnostic {
    JsonDiagnostic {
        code: code.to_string(),
        severity: if is_error {
            SEVERITY_ERROR
        } else {
            SEVERITY_WARNING
        }
        .to_string(),
        source: "hew-mir".to_string(),
        file: filename.to_string(),
        span: JsonSpan::from_range(source, span),
        message: message.to_string(),
        notes: Vec::new(),
        fixes: Vec::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn type_error_serializes_with_stable_code_and_no_debug_payload() {
        let source = "fn main() {\n    let x: i64 = \"oops\";\n}\n";
        let error = hew_types::TypeError::new(
            hew_types::error::TypeErrorKind::Mismatch {
                expected: "i64".to_string(),
                actual: "string".to_string(),
            },
            29..35,
            "type mismatch: expected `i64`, found `string`",
        );
        let diag = from_type_error(source, "main.hew", &error);
        assert_eq!(diag.code, "Mismatch");
        assert_eq!(diag.severity, "error");
        assert_eq!(diag.file, "main.hew");
        assert_eq!(diag.span.start_line, 2);
        assert!(diag.message.contains("type mismatch"));
        let rendered = serde_json::to_string(&diag).expect("serialize");
        assert!(
            !rendered.contains("{ "),
            "no debug struct payload: {rendered}"
        );
    }

    #[test]
    fn type_error_carries_code_action_fix_for_undefined_variable() {
        let source = "fn main() {\n    let y = foob;\n}\n";
        let foob_start = source.find("foob").expect("foob present");
        let mut error = hew_types::TypeError::new(
            hew_types::error::TypeErrorKind::UndefinedVariable,
            foob_start..foob_start + 4,
            "undefined variable `foob`",
        );
        error.suggestions = vec!["`food`".to_string()];
        let diag = from_type_error(source, "main.hew", &error);
        assert_eq!(diag.code, "UndefinedVariable");
        assert_eq!(diag.fixes.len(), 1, "expected one code-action fix");
        assert_eq!(diag.fixes[0].edits[0].new_text, "food");
    }

    #[test]
    fn hir_not_yet_implemented_message_reads_as_limitation_not_debug() {
        let diagnostic = hew_hir::HirDiagnostic::new(
            hew_hir::HirDiagnosticKind::NotYetImplemented {
                construct: "expression".to_string(),
                owning_pass: "slice-2".to_string(),
            },
            0..4,
            "",
        );
        let diag = from_hir_diagnostic(Some("abcd"), Some("main.hew"), &diagnostic);
        assert_eq!(diag.code, "NotYetImplemented");
        assert!(
            !diag.message.contains("owning_pass"),
            "message must not leak the Rust field name: {}",
            diag.message
        );
        assert!(
            !diag.message.contains('{'),
            "message must not be a Debug struct payload: {}",
            diag.message
        );
        assert!(
            diag.message.contains("current Hew limitation"),
            "message should read as a limitation: {}",
            diag.message
        );
    }

    #[test]
    fn mir_diagnostic_without_source_uses_zero_span() {
        let diag = from_mir_diagnostic(
            None,
            None,
            None,
            "NotYetImplemented",
            "MIR lowering for functional-update override aliasing the consumed base \
             is not implemented yet",
            &[],
            &["MIR kind: NotYetImplemented".to_string()],
            false,
        );
        assert_eq!(diag.code, "NotYetImplemented");
        assert_eq!(diag.severity, SEVERITY_ERROR);
        assert_eq!(diag.span, JsonSpan::zero());
        assert!(!diag.message.contains("SiteId"));
        assert_eq!(diag.notes.len(), 1);
    }

    #[test]
    fn advisory_mir_diagnostic_carries_warning_severity() {
        let diag = from_mir_diagnostic(
            None,
            None,
            None,
            "ObligationUnderReleased",
            "obligation balance in `decode`: owned value `out` is never released",
            &[],
            &["MIR kind: ObligationUnderReleased".to_string()],
            true,
        );
        assert_eq!(
            diag.severity, SEVERITY_WARNING,
            "an advisory MIR diagnostic (under-release leak) must carry the \
             `warning` severity, not `error`"
        );
    }
}
