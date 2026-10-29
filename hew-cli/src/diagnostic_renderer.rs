//! Same-process adapter for the ahead-of-time Hew presentation component.

use hew_cabi::host_error::{HewError, HewText};
use serde::Serialize;
use std::cell::RefCell;
use std::collections::HashMap;

use super::{DiagnosticNote, DiagnosticPalette};

// The checked export and its C declaration are generated together in build.rs.
// This synchronous string -> string signature requires no scheduler handle.
#[allow(
    improper_ctypes,
    reason = "HewError is an opaque C owner; its Rust contents never cross the boundary"
)]
unsafe extern "C" {
    fn diagnostic_render_text(
        input: *const HewText,
        output: *mut *mut HewText,
        error: *mut *mut HewError,
    ) -> i32;
    fn ownership_render_text(
        input: *const HewText,
        output: *mut *mut HewText,
        error: *mut *mut HewError,
    ) -> i32;
}

pub(crate) fn render_ownership(input: &str) -> String {
    // SAFETY: build.rs checks this generated export's borrowed-string contract.
    unsafe { crate::host_text::call(input, ownership_render_text) }
}

#[derive(Serialize)]
struct Label<'a> {
    filename: String,
    line: usize,
    text: &'a str,
    start: i64,
    end: i64,
    message: String,
}

/// Line tables by source identity (address and length).
type LineTables = HashMap<(usize, usize), Vec<usize>>;

thread_local! {
    /// Line tables for the sources of the render loop in progress, keyed by
    /// the address and length of the borrowed source text.
    static LINE_STARTS: RefCell<Option<LineTables>> = const { RefCell::new(None) };
}

/// Builds each source's line table once while it is alive, so a loop that
/// renders many diagnostics does not rescan the file from the start for each.
/// The loop borrows its sources throughout, which keeps the address keys valid.
/// Outside a scope each label builds a table of its own.
pub(crate) struct LineIndexScope {
    owner: bool,
}

impl LineIndexScope {
    pub(crate) fn new() -> Self {
        let owner = LINE_STARTS.with(|cell| {
            let mut cell = cell.borrow_mut();
            let owner = cell.is_none();
            if owner {
                *cell = Some(HashMap::new());
            }
            owner
        });
        Self { owner }
    }
}

impl Drop for LineIndexScope {
    fn drop(&mut self) {
        if self.owner {
            LINE_STARTS.with(|cell| *cell.borrow_mut() = None);
        }
    }
}

/// Byte offset at which each line of `source` starts (a final empty line
/// included when the source ends in a newline).
fn line_starts(source: &str) -> Vec<usize> {
    std::iter::once(0)
        .chain(
            source
                .bytes()
                .enumerate()
                .filter_map(|(at, byte)| (byte == b'\n').then_some(at + 1)),
        )
        .collect()
}

fn with_line_starts<R>(source: &str, use_starts: impl FnOnce(&[usize]) -> R) -> R {
    LINE_STARTS.with(|cell| match cell.borrow_mut().as_mut() {
        Some(tables) => {
            let key = (source.as_ptr() as usize, source.len());
            use_starts(tables.entry(key).or_insert_with(|| line_starts(source)))
        }
        None => use_starts(&line_starts(source)),
    })
}

impl<'a> From<&DiagnosticNote<'a>> for Label<'a> {
    fn from(note: &DiagnosticNote<'a>) -> Self {
        let source = note.source;
        let offset = note.span.start.min(source.len());
        // The line holding the span start, newline included. Line starts
        // follow ASCII newlines, so each slice lands on a character boundary.
        let (line, line_start, line_end) = with_line_starts(source, |starts| {
            let line = starts.partition_point(|&start| start <= offset);
            (
                line,
                starts[line - 1],
                starts.get(line).copied().unwrap_or(source.len()),
            )
        });
        let relative = |position: usize| {
            i64::try_from(position).unwrap_or(i64::MAX) - i64::try_from(line_start).unwrap_or(0)
        };
        Self {
            filename: super::display_filename(note.filename),
            line,
            text: &source[line_start..line_end],
            start: relative(note.span.start),
            end: relative(note.span.end),
            message: hew_compile::source_path_policy().display_text(note.message),
        }
    }
}

#[derive(Serialize)]
struct Presentation<'a> {
    primary: Label<'a>,
    level: &'a str,
    notes: Vec<Label<'a>>,
    suggestions: &'a [String],
    palette: &'a DiagnosticPalette,
}

pub(super) fn render(
    primary: &DiagnosticNote<'_>,
    level: &str,
    notes: &[DiagnosticNote<'_>],
    suggestions: &[String],
    palette: &DiagnosticPalette,
) -> String {
    let input = serde_json::to_string(&Presentation {
        primary: primary.into(),
        level,
        notes: notes.iter().map(Label::from).collect(),
        suggestions,
        palette,
    })
    .expect("diagnostic presentation is serializable");
    // SAFETY: build.rs checks this generated export's borrowed-string contract.
    unsafe { crate::host_text::call(&input, diagnostic_render_text) }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ownership_locations_share_a_forward_unicode_scan() {
        let entry = |offset| {
            serde_json::json!({
                "offset": offset,
                "detail": {"Send": {
                    "index": 1,
                    "ty": "string",
                    "action": "move",
                    "preparation": "retain"
                }}
            })
        };
        let input = serde_json::json!({
            "filename": "<snapshot>",
            "source": "é 🦫;\r\nnext\n",
            "entries": [entry(0), entry(7), entry(7), entry(10), entry(15)]
        });
        let rendered = render_ownership(&input.to_string());
        let positions = rendered
            .lines()
            .map(|line| {
                let mut fields = line.splitn(4, ':');
                assert_eq!(fields.next(), Some("<snapshot>"));
                (
                    fields.next().unwrap().parse::<u32>().unwrap(),
                    fields.next().unwrap().parse::<u32>().unwrap(),
                )
            })
            .collect::<Vec<_>>();
        assert_eq!(positions, [(1, 1), (1, 4), (1, 4), (2, 1), (3, 1)]);
    }

    #[test]
    fn unicode_crlf_notes_and_help_keep_the_source_presentation() {
        let primary = DiagnosticNote {
            source: "first\r\nélève 🦫 bad\r\n",
            filename: "<input>",
            span: &(20..23),
            message: "wrong value",
        };
        let note = DiagnosticNote {
            source: "declared()\n",
            filename: "<declaration>",
            span: &(0..8),
            message: "declared here",
        };
        assert_eq!(
            render(
                &primary,
                "error[E_TYPE]",
                &[note],
                &["replace `bad` with `good`".into()],
                &super::super::diagnostic_palette(),
            ),
            "<input>:2:9: error[E_TYPE]: wrong value\n \
             2 | élève 🦫 bad\n   |         ^^^\n\
             <declaration>:1:1: note: declared here\n \
             1 | declared()\n   | ^^^^^^^^\n  = help: replace `bad` with `good`\n"
        );
    }

    #[test]
    fn long_lines_show_a_window_around_the_span() {
        let source = format!("{}BAD{}\nnext\n", "a".repeat(500), "b".repeat(500));
        let note = DiagnosticNote {
            source: &source,
            filename: "<input>",
            span: &(500..503),
            message: "wrong",
        };
        let rendered = render(
            &note,
            "error",
            &[],
            &[],
            &super::super::diagnostic_palette(),
        );
        let mut lines = rendered.lines();
        assert_eq!(lines.next(), Some("<input>:1:501: error: wrong"));
        let shown = lines.next().unwrap();
        let carets = lines.next().unwrap();
        assert!(shown.contains("...") && shown.contains("BAD"), "{shown}");
        assert!(shown.len() < 200, "{} bytes", shown.len());
        // The carets sit under `BAD` in the windowed text.
        let under = carets.find('^').unwrap();
        assert_eq!(&shown[under..under + 3], "BAD", "{shown}\n{carets}");
        assert_eq!(carets.matches('^').count(), 3);
    }

    #[test]
    fn a_span_on_a_later_line_reports_that_line_and_column() {
        let source = "one\ntwo\nthree three\n";
        let note = DiagnosticNote {
            source,
            filename: "<input>",
            span: &(14..19),
            message: "wrong",
        };
        let rendered = render(
            &note,
            "error",
            &[],
            &[],
            &super::super::diagnostic_palette(),
        );
        assert_eq!(
            rendered,
            "<input>:3:7: error: wrong\n 3 | three three\n   |       ^^^^^\n"
        );
    }

    #[test]
    fn eof_and_split_scalar_spans_remain_visible() {
        for (source, span, expected) in [
            ("", 0..0, "<input>:1:1: warning: end\n 1 |\n   | ^\n"),
            ("a\n", 2..2, "<input>:2:1: warning: end\n 2 |\n   | ^\n"),
            ("é", 1..2, "<input>:1:2: warning: end\n 1 | é\n   | ^\n"),
        ] {
            let label = DiagnosticNote {
                source,
                filename: "<input>",
                span: &span,
                message: "end",
            };
            assert_eq!(
                render(
                    &label,
                    "warning",
                    &[],
                    &[],
                    &super::super::diagnostic_palette()
                ),
                expected
            );
        }
    }

    #[test]
    fn supplied_palette_colours_warning_note_and_help() {
        let label = DiagnosticNote {
            source: "x",
            filename: "<input>",
            span: &(0..1),
            message: "check this",
        };
        let palette = DiagnosticPalette {
            red: "\x1b[1;31m",
            yellow: "\x1b[1;33m",
            blue: "\x1b[1;34m",
            cyan: "\x1b[1;36m",
            bold: "\x1b[1m",
            reset: "\x1b[0m",
        };
        let text = render(
            &label,
            "warning",
            &[DiagnosticNote { ..label }],
            &["try this".into()],
            &palette,
        );
        assert!(text.starts_with(
            "\x1b[1m<input>:1:1:\x1b[0m \x1b[1;33mwarning\x1b[0m\x1b[1m: check this\x1b[0m\n"
        ));
        assert!(text.contains("\x1b[1;31m^\x1b[0m"));
        assert!(text.contains("\x1b[1;36mnote\x1b[0m"));
        assert!(text.ends_with("  \x1b[1;36m= help\x1b[0m: try this\n"));
    }
}
