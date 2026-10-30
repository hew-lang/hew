//! Same-process adapter for the ahead-of-time Hew presentation component.

use hew_cabi::host_error::{HewError, HewText};
use serde::Serialize;
use std::cell::{OnceCell, RefCell};
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
    column: usize,
    text: &'a str,
    start: i64,
    end: i64,
    message: String,
}

/// Bytes of the span's line sent on each side of the span start. The
/// component shows at most its `WINDOW` (160 bytes) of a line, so a larger
/// excerpt renders exactly as the whole line would, while a diagnostic on a
/// very long line costs a bounded amount instead of the line's length.
const CONTEXT: usize = 256;

/// Line tables by source identity (address and length).
type LineIndexes = HashMap<(usize, usize), LineIndex>;

thread_local! {
    /// Line tables for the sources of the render loop in progress, keyed by
    /// the address and length of the borrowed source text.
    static LINE_INDEXES: RefCell<Option<LineIndexes>> = const { RefCell::new(None) };
}

/// Builds each source's line table once while it is alive, so a loop that
/// renders many diagnostics does not rescan the file from the start for each.
/// The loop borrows its sources throughout, which keeps the address keys valid.
/// Outside a scope each lookup builds a table of its own.
pub(crate) struct LineIndexScope {
    owner: bool,
}

impl LineIndexScope {
    pub(crate) fn new() -> Self {
        let owner = LINE_INDEXES.with(|cell| {
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
            LINE_INDEXES.with(|cell| *cell.borrow_mut() = None);
        }
    }
}

/// Bytes between samples of the running column count.
const COLUMN_BLOCK: usize = 64;

/// Where lines start and how many columns precede each block of a source, so
/// both the line and the column of any offset cost a bounded amount, and the
/// snapshot hash each machine-readable diagnostic carries.
struct LineIndex {
    /// Byte offset at which each line starts (a final empty line included
    /// when the source ends in a newline).
    starts: Vec<usize>,
    /// Column-advancing bytes before each multiple of [`COLUMN_BLOCK`].
    blocks: Vec<usize>,
    hash: OnceCell<String>,
}

/// Whether the byte at `at` advances the column: it starts a scalar and is
/// neither a newline nor the carriage return of a CRLF terminator.
fn advances_column(bytes: &[u8], at: usize) -> bool {
    match bytes[at] {
        b'\n' => false,
        b'\r' => bytes.get(at + 1) != Some(&b'\n'),
        byte => byte & 0xC0 != 0x80,
    }
}

impl LineIndex {
    fn new(source: &str) -> Self {
        let bytes = source.as_bytes();
        let mut starts = vec![0];
        let mut blocks = Vec::with_capacity(bytes.len() / COLUMN_BLOCK + 1);
        let mut columns = 0;
        for at in 0..bytes.len() {
            if at.is_multiple_of(COLUMN_BLOCK) {
                blocks.push(columns);
            }
            if bytes[at] == b'\n' {
                starts.push(at + 1);
            }
            columns += usize::from(advances_column(bytes, at));
        }
        if bytes.len().is_multiple_of(COLUMN_BLOCK) {
            blocks.push(columns);
        }
        Self {
            starts,
            blocks,
            hash: OnceCell::new(),
        }
    }

    /// Column-advancing bytes in `source[..offset]`.
    fn columns_before(&self, source: &str, offset: usize) -> usize {
        let block = offset / COLUMN_BLOCK;
        let bytes = source.as_bytes();
        self.blocks[block]
            + (block * COLUMN_BLOCK..offset)
                .filter(|&at| advances_column(bytes, at))
                .count()
    }

    /// The one-based line holding `offset`, and that line's byte range with
    /// its newline.
    fn line(&self, source: &str, offset: usize) -> (usize, usize, usize) {
        let line = self.starts.partition_point(|&start| start <= offset);
        let end = self.starts.get(line).copied().unwrap_or(source.len());
        (line, self.starts[line - 1], end)
    }

    /// One-based line and column of `offset`, clamped to the source. A CRLF
    /// carriage return takes no column, matching the displayed line.
    fn line_col(&self, source: &str, offset: usize) -> (usize, usize) {
        let offset = offset.min(source.len());
        let (line, start, _) = self.line(source, offset);
        (
            line,
            1 + self.columns_before(source, offset) - self.columns_before(source, start),
        )
    }
}

fn with_line_index<R>(source: &str, use_index: impl FnOnce(&LineIndex) -> R) -> R {
    LINE_INDEXES.with(|cell| match cell.borrow_mut().as_mut() {
        Some(indexes) => {
            let key = (source.as_ptr() as usize, source.len());
            use_index(indexes.entry(key).or_insert_with(|| LineIndex::new(source)))
        }
        None => use_index(&LineIndex::new(source)),
    })
}

/// One-based line and column of a byte offset, through the active scope's
/// line table when there is one.
pub(crate) fn offset_to_line_col(source: &str, offset: usize) -> (usize, usize) {
    with_line_index(source, |index| index.line_col(source, offset))
}

/// The SHA-256 identity of `source`, hashed once per source in a scope.
pub(crate) fn source_hash(source: &str) -> String {
    with_line_index(source, |index| {
        index
            .hash
            .get_or_init(|| hew_analysis::code_actions::source_hash(source))
            .clone()
    })
}

impl<'a> Label<'a> {
    /// The label for `note`, carrying at most `context` bytes of its line on
    /// each side of the span start.
    fn new(note: &DiagnosticNote<'a>, context: usize) -> Self {
        let source = note.source;
        let offset = note.span.start.min(source.len());
        let ((line, column), line_start, line_end) = with_line_index(source, |index| {
            let (_, start, end) = index.line(source, offset);
            (index.line_col(source, offset), start, end)
        });
        // The excerpt keeps the newline only when it reaches the line end.
        // Line starts follow ASCII newlines, so they are character boundaries.
        let mut from = offset.saturating_sub(context).max(line_start);
        while !source.is_char_boundary(from) {
            from -= 1;
        }
        let mut to = offset.saturating_add(context).min(line_end);
        while !source.is_char_boundary(to) {
            to += 1;
        }
        let relative = |position: usize| {
            i64::try_from(position).unwrap_or(i64::MAX) - i64::try_from(from).unwrap_or(0)
        };
        Self {
            filename: super::display_filename(note.filename),
            line,
            column,
            text: &source[from..to],
            start: relative(note.span.start),
            end: relative(note.span.end),
            message: hew_compile::source_path_policy().display_text(note.message),
        }
    }
}

impl<'a> From<&DiagnosticNote<'a>> for Label<'a> {
    fn from(note: &DiagnosticNote<'a>) -> Self {
        Self::new(note, CONTEXT)
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

    /// Renders `note` with `context` bytes of its line on each side of the
    /// span start.
    fn render_excerpt(note: &DiagnosticNote<'_>, context: usize) -> String {
        let input = serde_json::to_string(&Presentation {
            primary: Label::new(note, context),
            level: "error",
            notes: Vec::new(),
            suggestions: &[],
            palette: &super::super::diagnostic_palette(),
        })
        .unwrap();
        // SAFETY: build.rs checks this generated export's borrowed-string contract.
        unsafe { crate::host_text::call(&input, diagnostic_render_text) }
    }

    /// Long lines mixing multibyte scalars and carriage returns, ending in
    /// CRLF, LF and EOF.
    fn long_line_sources() -> Vec<String> {
        let mixed = "aé🦫\rb".repeat(90);
        vec![
            format!("{}\r\nnext\n", "x".repeat(700)),
            format!("{mixed}\r\n{mixed}\n"),
            format!("{}\r{}", "y".repeat(400), "é".repeat(200)),
        ]
    }

    #[test]
    fn a_line_excerpt_renders_exactly_as_the_whole_line() {
        for source in long_line_sources() {
            for start in 0..=source.len() {
                for width in [0, 1, 3, 300] {
                    let span = start..(start + width).min(source.len());
                    let note = DiagnosticNote {
                        source: &source,
                        filename: "<input>",
                        span: &span,
                        message: "wrong",
                    };
                    assert_eq!(
                        render_excerpt(&note, CONTEXT),
                        render_excerpt(&note, usize::MAX),
                        "span {span:?} of {source:?}"
                    );
                }
            }
        }
    }

    #[test]
    fn indexed_columns_count_scalars_and_skip_the_crlf_return() {
        // One-based line and column by a scan from the start of the source.
        fn scanned(source: &str, offset: usize) -> (usize, usize) {
            let (mut line, mut column) = (1, 1);
            let bytes = source.as_bytes();
            for (at, ch) in source.char_indices() {
                if at >= offset {
                    break;
                }
                if ch == '\n' {
                    line += 1;
                    column = 1;
                } else if !(ch == '\r' && bytes.get(at + 1) == Some(&b'\n')) {
                    column += 1;
                }
            }
            (line, column)
        }
        for source in long_line_sources()
            .into_iter()
            .chain([String::new(), "\n\r\n".into()])
        {
            let index = LineIndex::new(&source);
            for offset in 0..=source.len() + 2 {
                assert_eq!(
                    index.line_col(&source, offset),
                    scanned(&source, offset.min(source.len())),
                    "offset {offset} of {source:?}"
                );
            }
        }
    }

    #[test]
    fn a_label_on_a_long_line_carries_a_bounded_excerpt() {
        let source = format!("{}BAD{}\n", "a".repeat(1 << 20), "b".repeat(1 << 20));
        let span = (1 << 20)..(1 << 20) + 3;
        let note = DiagnosticNote {
            source: &source,
            filename: "<input>",
            span: &span,
            message: "wrong",
        };
        let label = Label::from(&note);
        assert!(
            label.text.len() <= 2 * CONTEXT,
            "{} bytes",
            label.text.len()
        );
        assert_eq!((label.line, label.column), (1, (1 << 20) + 1));
        let context = i64::try_from(CONTEXT).unwrap();
        assert_eq!((label.start, label.end), (context, context + 3));
        assert_eq!(&label.text[CONTEXT..CONTEXT + 3], "BAD");
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
