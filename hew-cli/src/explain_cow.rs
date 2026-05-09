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
//! src/handler.hew:42:9: send to `Order` — COPY (feature gate disabled in α)
//!
//! src/handler.hew:58:13: send to `i64` — COPY (Copy type)
//! ```
//!
//! Phase α records `Copy` at every site; a later commit enables the `Alias`
//! path. The renderer prints the reason so users understand why the alias path
//! has not fired yet.

use hew_serialize::{ActorSendAliasingData, ActorSendAliasingEntry};

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
    let line_map = build_line_map(source);
    for entry in entries {
        let (line, col) = span_to_line_col(&line_map, entry.start);
        let kind_label = match entry.kind {
            ActorSendAliasingData::Alias => "ALIAS",
            ActorSendAliasingData::Copy => "COPY",
        };
        let reason = match entry.kind {
            ActorSendAliasingData::Alias => String::new(),
            ActorSendAliasingData::Copy => " (feature gate disabled in α)".to_string(),
        };
        // Print: filename:line:col: send — KIND (reason)
        let _ = writeln!(out, "{filename}:{line}:{col}: send — {kind_label}{reason}");
    }
}

/// Build a line-start-offset map from source text.
///
/// `map[i]` is the byte offset of the first character on line `i+1`.
/// Line numbers are 1-based; line 1 starts at offset 0.
fn build_line_map(source: &str) -> Vec<usize> {
    let mut map = vec![0usize];
    for (i, &byte) in source.as_bytes().iter().enumerate() {
        if byte == b'\n' {
            map.push(i + 1);
        }
    }
    map
}

/// Convert a byte offset to (line, col), both 1-based.
fn span_to_line_col(line_map: &[usize], offset: usize) -> (usize, usize) {
    let line_idx = line_map
        .partition_point(|&start| start <= offset)
        .saturating_sub(1);
    let line = line_idx + 1;
    let col = offset - line_map[line_idx] + 1;
    (line, col)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_entry(start: usize, end: usize, kind: ActorSendAliasingData) -> ActorSendAliasingEntry {
        ActorSendAliasingEntry { start, end, kind }
    }

    #[test]
    fn line_col_first_line_first_col() {
        let map = build_line_map("abc\ndef");
        assert_eq!(span_to_line_col(&map, 0), (1, 1));
    }

    #[test]
    fn line_col_second_line() {
        // "abc\ndef" — 'd' is at offset 4
        let map = build_line_map("abc\ndef");
        assert_eq!(span_to_line_col(&map, 4), (2, 1));
    }

    #[test]
    fn line_col_mid_line() {
        // "abc\ndef" — 'e' is at offset 5
        let map = build_line_map("abc\ndef");
        assert_eq!(span_to_line_col(&map, 5), (2, 2));
    }

    #[test]
    fn render_copy_entry_includes_reason() {
        let entries = vec![make_entry(4, 5, ActorSendAliasingData::Copy)];
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
            output.contains("feature gate disabled"),
            "expected reason in output, got: {output:?}"
        );
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
            !output.contains("feature gate"),
            "alias entries must not include a reason"
        );
    }
}
