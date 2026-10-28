//! Same-process adapter for the ahead-of-time Hew presentation component.

use hew_cabi::host_error::{HewError, HewText};
use serde::Serialize;

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
    source: &'a str,
    filename: String,
    start: i64,
    end: i64,
    message: String,
}

impl<'a> From<&DiagnosticNote<'a>> for Label<'a> {
    fn from(note: &DiagnosticNote<'a>) -> Self {
        Self {
            source: note.source,
            filename: super::display_filename(note.filename),
            start: i64::try_from(note.span.start).unwrap_or(i64::MAX),
            end: i64::try_from(note.span.end).unwrap_or(i64::MAX),
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
