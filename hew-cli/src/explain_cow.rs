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

use std::collections::HashMap;

use hew_types::check::{ActorSendAliasing, ActorSendCopyReason, SpanKey};

/// Render `--explain-cow` output for all entries to the provided writer.
///
/// `source` is the full source text of the file; `filename` is the label
/// used in the output header (typically the path passed to `hew check`).
///
/// Entries are iterated in `(start, end)` order so the output is stable
/// regardless of the underlying map's iteration order.
pub fn render_explain_cow(
    entries: &HashMap<SpanKey, ActorSendAliasing>,
    source: &str,
    filename: &str,
    out: &mut dyn std::io::Write,
) {
    let mut sorted: Vec<(&SpanKey, &ActorSendAliasing)> = entries.iter().collect();
    sorted.sort_by_key(|(k, _)| (k.start, k.end));

    for (span, decision) in sorted {
        let (line, col) = crate::diagnostic::offset_to_line_col(source, span.start);
        let (kind_label, reason) = match decision {
            ActorSendAliasing::Alias => ("ALIAS", String::new()),
            ActorSendAliasing::Copy(reason) => ("COPY", format!(" ({})", reason_label(*reason))),
        };
        // Print: filename:line:col: send — KIND (reason)
        let _ = writeln!(out, "{filename}:{line}:{col}: send — {kind_label}{reason}");
    }
}

/// Map a native `ActorSendCopyReason` to the human-facing label shown in
/// `--explain-cow` output.
fn reason_label(reason: ActorSendCopyReason) -> &'static str {
    match reason {
        ActorSendCopyReason::NotIdentifier => "non-identifier expression",
        ActorSendCopyReason::CopyType => "Copy type",
        ActorSendCopyReason::StdlibDrop => "stdlib Drop",
        ActorSendCopyReason::UserDrop => "user impl Drop",
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn map_of(
        entries: Vec<(usize, usize, ActorSendAliasing)>,
    ) -> HashMap<SpanKey, ActorSendAliasing> {
        entries
            .into_iter()
            .map(|(start, end, decision)| {
                (
                    SpanKey {
                        start,
                        end,
                        module_idx: 0,
                    },
                    decision,
                )
            })
            .collect()
    }

    #[test]
    fn render_copy_entry_includes_copy_type_reason() {
        let entries = map_of(vec![(
            4,
            5,
            ActorSendAliasing::Copy(ActorSendCopyReason::CopyType),
        )]);
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
                ActorSendCopyReason::NotIdentifier,
                "non-identifier expression",
            ),
            (ActorSendCopyReason::CopyType, "Copy type"),
            (ActorSendCopyReason::StdlibDrop, "stdlib Drop"),
            (ActorSendCopyReason::UserDrop, "user impl Drop"),
        ];
        for (reason, expected_label) in cases {
            let entries = map_of(vec![(0, 1, ActorSendAliasing::Copy(reason))]);
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
        let entries = map_of(vec![(0, 1, ActorSendAliasing::Alias)]);
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
        let entries = map_of(vec![(
            5,
            6,
            ActorSendAliasing::Copy(ActorSendCopyReason::CopyType),
        )]);
        let mut out = Vec::new();
        render_explain_cow(&entries, source, "f.hew", &mut out);
        let output = String::from_utf8(out).unwrap();
        assert!(
            output.contains("f.hew:2:1"),
            "CRLF: 'x' should be 2:1, got: {output:?}"
        );
    }

    #[test]
    fn render_emits_entries_in_span_order() {
        // Map iteration order is unspecified; the renderer must sort so the
        // output is byte-stable regardless of insertion order.
        let entries = map_of(vec![
            (10, 11, ActorSendAliasing::Alias),
            (0, 1, ActorSendAliasing::Copy(ActorSendCopyReason::CopyType)),
            (5, 6, ActorSendAliasing::Alias),
        ]);
        let mut out = Vec::new();
        render_explain_cow(&entries, "0123456789abc", "f.hew", &mut out);
        let output = String::from_utf8(out).unwrap();
        let copy_pos = output.find("COPY").expect("COPY present");
        let first_alias = output.find("ALIAS").expect("ALIAS present");
        assert!(
            copy_pos < first_alias,
            "entry at span 0..1 must render before later spans, got: {output:?}"
        );
    }
}
