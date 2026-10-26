//! Same-process adapter for the ahead-of-time Hew presentation component.

use hew_cabi::host_error::{HewError, HewText};
use hew_runtime::host_api::{
    hew_host_error_message, hew_host_error_release, hew_host_text_data, hew_host_text_from_utf8,
    hew_host_text_release,
};
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

struct Text(*mut HewText);

impl Drop for Text {
    fn drop(&mut self) {
        // SAFETY: this adapter owns the one release obligation for this text.
        unsafe { hew_host_text_release(self.0) };
    }
}

fn checked(status: i32, error: *mut HewError) {
    if status != 0 {
        let mut len = 0;
        // SAFETY: a failed generated call publishes a live error owner. The
        // borrowed bytes are copied before releasing that owner.
        let message = unsafe {
            let data = hew_host_error_message(error, &raw mut len);
            let message =
                String::from_utf8_lossy(std::slice::from_raw_parts(data, len)).into_owned();
            hew_host_error_release(error);
            message
        };
        panic!("diagnostic presentation failed: {message}");
    }
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
    let mut source = std::ptr::null_mut();
    let mut output = std::ptr::null_mut();
    let mut error = std::ptr::null_mut();
    // SAFETY: input is valid borrowed UTF-8; all owner/error slots are empty
    // and disjoint. The generated export borrows source and publishes output.
    unsafe {
        checked(
            hew_host_text_from_utf8(input.as_ptr(), input.len(), &raw mut source, &raw mut error),
            error,
        );
        let source = Text(source);
        checked(
            diagnostic_render_text(source.0, &raw mut output, &raw mut error),
            error,
        );
        let output = Text(output);
        let mut len = 0;
        let data = hew_host_text_data(output.0, &raw mut len);
        // The host text contract guarantees valid UTF-8 for the owner's life.
        std::str::from_utf8(std::slice::from_raw_parts(data, len))
            .expect("renderer returns UTF-8")
            .to_owned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
