//! `--explain-cow` diagnostic renderer.
//!
//! Reads the `actor_send_aliasing` side table from the type-checker output and
//! prints one block per actor send site, showing whether the mailbox crossed
//! the boundary via a refcount alias or a deep copy. Default off; opt-in for
//! Phase α.
//!
//! Format mirrors `-gcflags=-m` (Go escape analysis):
//!
//! ```text
//! src/handler.hew:42:9: send — COPY (Copy type)
//! src/handler.hew:58:13: send — ALIAS
//! ```
//!
//! `COPY` lines carry the precise reason the type checker rejected the alias
//! path so users see *why* a given send fell off the fast path:
//!
//! - `(non-identifier expression)` — the arg was not a bare binding
//!   (field access, projection, freshly-constructed value).
//! - `(Copy type)` — arg's resolved type implements the `Copy` marker.
//! - `(stdlib Drop)` — arg's resolved type implements the
//!   stdlib-registered `Drop` marker.
//! - `(user impl Drop)` — arg's resolved type carries a user
//!   `impl Drop for T`.

use hew_serialize::{ActorSendAliasingData, ActorSendAliasingEntry, ActorSendCopyReasonData};

/// Render `--explain-cow` output for all entries to the provided writer.
///
/// `source` is the full source text of the file; `filename` is the label
/// used in the output header (typically the path passed to `hew check`).
pub fn render_explain_cow(
    entries: &[ActorSendAliasingEntry],
    source: &str,
    filename: &str,
    out: &mut dyn std::io::Write,
) {
    for entry in entries {
        let (line, col) = crate::diagnostic::offset_to_line_col(source, entry.start);
        let (kind_label, reason) = match entry.kind {
            ActorSendAliasingData::Alias => ("ALIAS", String::new()),
            ActorSendAliasingData::Copy { reason } => {
                ("COPY", format!(" ({})", reason_label(reason)))
            }
        };
        // Print: filename:line:col: send — KIND (reason)
        let _ = writeln!(out, "{filename}:{line}:{col}: send — {kind_label}{reason}");
    }
}

/// Map a wire-side `ActorSendCopyReasonData` to the human-facing label
/// shown in `--explain-cow` output.
fn reason_label(reason: ActorSendCopyReasonData) -> &'static str {
    match reason {
        ActorSendCopyReasonData::NotIdentifier => "non-identifier expression",
        ActorSendCopyReasonData::CopyType => "Copy type",
        ActorSendCopyReasonData::StdlibDrop => "stdlib Drop",
        ActorSendCopyReasonData::UserDrop => "user impl Drop",
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_entry(start: usize, end: usize, kind: ActorSendAliasingData) -> ActorSendAliasingEntry {
        ActorSendAliasingEntry { start, end, kind }
    }

    fn make_copy(
        start: usize,
        end: usize,
        reason: hew_serialize::ActorSendCopyReasonData,
    ) -> ActorSendAliasingEntry {
        make_entry(start, end, ActorSendAliasingData::Copy { reason })
    }

    #[test]
    fn render_copy_entry_includes_copy_type_reason() {
        let entries = vec![make_copy(
            4,
            5,
            hew_serialize::ActorSendCopyReasonData::CopyType,
        )];
        let source = "abc\nxyz";
        let mut out = Vec::new();
        render_explain_cow(&entries, source, "test.hew", &mut out);
        let output = String::from_utf8(out).unwrap();
        assert!(
            output.contains("COPY"),
            "expected COPY in output, got: {output:?}"
        );
        assert!(
            output.contains("test.hew:2:1"),
            "expected test.hew:2:1 in output, got: {output:?}"
        );
        assert!(
            output.contains("Copy type"),
            "Copy-typed reason should render as `Copy type`, got: {output:?}"
        );
    }

    #[test]
    fn render_copy_entry_renders_each_reason_distinctly() {
        // Every reason variant must produce a distinct, descriptive
        // suffix; this guards against a regression where every Copy
        // site rendered with the same placeholder reason.
        let cases = [
            (
                hew_serialize::ActorSendCopyReasonData::NotIdentifier,
                "non-identifier expression",
            ),
            (
                hew_serialize::ActorSendCopyReasonData::CopyType,
                "Copy type",
            ),
            (
                hew_serialize::ActorSendCopyReasonData::StdlibDrop,
                "stdlib Drop",
            ),
            (
                hew_serialize::ActorSendCopyReasonData::UserDrop,
                "user impl Drop",
            ),
        ];
        for (reason, expected_label) in cases {
            let entries = vec![make_copy(0, 1, reason)];
            let mut out = Vec::new();
            render_explain_cow(&entries, "x", "f.hew", &mut out);
            let output = String::from_utf8(out).unwrap();
            assert!(
                output.contains(expected_label),
                "expected `{expected_label}` for {reason:?}, got: {output:?}"
            );
        }
    }

    #[test]
    fn render_alias_entry_no_reason() {
        let entries = vec![make_entry(0, 1, ActorSendAliasingData::Alias)];
        let source = "xyz";
        let mut out = Vec::new();
        render_explain_cow(&entries, source, "handler.hew", &mut out);
        let output = String::from_utf8(out).unwrap();
        assert!(output.contains("ALIAS"), "expected ALIAS in output");
        assert!(
            !output.contains(" ("),
            "alias entries must not include a parenthesised reason, got: {output:?}"
        );
    }

    #[test]
    fn render_uses_shared_diagnostic_line_map_for_crlf() {
        // Regression: the local byte-based mapper miscounted CRLF line
        // endings.  The shared `diagnostic::offset_to_line_col` is
        // CRLF-aware; sanity-check that an offset on the second line
        // of a CRLF source resolves to line 2.
        let source = "abc\r\nxyz";
        // 'x' is at byte offset 5 in "abc\r\nxyz".
        let entries = vec![make_copy(
            5,
            6,
            hew_serialize::ActorSendCopyReasonData::CopyType,
        )];
        let mut out = Vec::new();
        render_explain_cow(&entries, source, "f.hew", &mut out);
        let output = String::from_utf8(out).unwrap();
        assert!(
            output.contains("f.hew:2:1"),
            "CRLF: 'x' should be 2:1, got: {output:?}"
        );
    }
}
