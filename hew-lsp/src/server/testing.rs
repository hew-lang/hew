//! Map the CLI test event stream back to editor locations.

use std::collections::HashMap;
use std::path::Path;

use dashmap::DashMap;
use hew_analysis::util::compute_line_offsets;
use serde_json::Value;
use tower_lsp_server::ls_types::{Diagnostic, DiagnosticSeverity, Uri as Url};

use super::analysis::open_document_uri;
use super::uri::FileUriExt;
use super::{offset_range_to_lsp, DocumentState};

pub(super) struct TestFailure {
    pub uri: Url,
    pub selector: String,
    pub diagnostic: Diagnostic,
    pub seed: Option<String>,
}

// One completed run is published under the server's analysis lock.
#[derive(Default)]
pub(super) struct TestFailureBatch {
    closed: HashMap<Url, (String, Vec<usize>)>,
    offsets: HashMap<Url, HashMap<String, usize>>,
}

impl TestFailureBatch {
    pub(super) fn failure_from_event(
        &mut self,
        event: &Value,
        documents: &DashMap<Url, DocumentState>,
    ) -> Option<TestFailure> {
        if event.get("event")?.as_str()? != "test_finished"
            || event.get("outcome")?.as_str()? != "failed"
        {
            return None;
        }
        let selector = event.get("selector")?.as_str()?;
        let (file, name) = selector.rsplit_once("::")?;
        let path = Path::new(file);
        let uri = open_document_uri(&Url::from_checked_file_path(path)?, documents);
        let open = documents.get(&uri);
        let (source, lines) = if let Some(document) = open.as_deref() {
            (document.source.as_str(), document.line_offsets.as_slice())
        } else {
            let snapshot = match self.closed.entry(uri.clone()) {
                std::collections::hash_map::Entry::Occupied(entry) => entry.into_mut(),
                std::collections::hash_map::Entry::Vacant(entry) => {
                    let text = std::fs::read_to_string(path).ok()?;
                    let lines = compute_line_offsets(&text);
                    entry.insert((text, lines))
                }
            };
            (snapshot.0.as_str(), snapshot.1.as_slice())
        };
        let offset = site_offset(event, path, source, lines).or_else(|| {
            let offsets = self.offsets.entry(uri.clone()).or_insert_with(|| {
                let parsed;
                let program = if let Some(document) = open.as_deref() {
                    &document.parse_result.program
                } else {
                    parsed = hew_parser::parse(source);
                    &parsed.program
                };
                let mut offsets = HashMap::new();
                for test in hew_analysis::test_discovery::discover_tests(program) {
                    offsets.entry(test.name).or_insert(test.span.start);
                }
                offsets
            });
            offsets.get(name).copied()
        })?;
        let end = source[offset..]
            .chars()
            .next()
            .map_or(offset, |character| offset + character.len_utf8());
        let range = offset_range_to_lsp(source, lines, offset, end);
        let mut message = event
            .get("message")
            .and_then(Value::as_str)
            .or_else(|| event.get("kind").and_then(Value::as_str))
            .unwrap_or("Test failed")
            .to_string();
        if let Some(assertion) = event.pointer("/report/assertion") {
            if let (Some(left), Some(right)) = (
                assertion.get("left").and_then(Value::as_str),
                assertion.get("right").and_then(Value::as_str),
            ) {
                if !message.contains("left:") && !message.contains("right:") {
                    use std::fmt::Write;
                    let _ = write!(message, "\nleft: {left}\nright: {right}");
                }
            }
        }
        let seed = event
            .pointer("/report/seed")
            .and_then(Value::as_str)
            .map(str::to_string);
        Some(TestFailure {
            uri,
            selector: selector.to_string(),
            diagnostic: Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::ERROR),
                source: Some("hew test".to_string()),
                message,
                data: Some(serde_json::json!({ "selector": selector })),
                ..Default::default()
            },
            seed,
        })
    }
}

/// The byte offset in `path` of the failure site the runtime reported, when
/// the site lies in that file. The site table records a project-relative
/// path, a one-based line and a one-based Unicode scalar column.
fn site_offset(event: &Value, path: &Path, source: &str, lines: &[usize]) -> Option<usize> {
    let site = event.pointer("/report/site")?;
    let file = site.get("file")?.as_str()?;
    if !path.ends_with(file) {
        return None;
    }
    let line = usize::try_from(site.get("line")?.as_u64()?).ok()?;
    let col = usize::try_from(site.get("col")?.as_u64()?).ok()?;
    let start = *lines.get(line.checked_sub(1)?)?;
    let text = source.get(start..)?.split('\n').next()?;
    let column = col.checked_sub(1)?;
    let byte = text
        .char_indices()
        .nth(column)
        .map(|(byte, _)| byte)
        .or_else(|| (text.chars().count() == column).then_some(text.len()))?;
    Some(start + byte)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn failed_tests_in_one_run_share_a_closed_file_snapshot() {
        let file =
            std::env::temp_dir().join(format!("hew-lsp-test-batch-{}.hew", std::process::id()));
        let source = "#[test]\nfn first() {}\n\n#[test]\nfn second() {}\n";
        std::fs::write(&file, source).unwrap();
        let event = |name: &str| {
            serde_json::json!({
                "event": "test_finished", "outcome": "failed",
                "selector": format!("{}::{name}", file.display()),
            })
        };
        let documents = DashMap::new();
        let mut batch = TestFailureBatch::default();
        let first = batch
            .failure_from_event(&event("first"), &documents)
            .unwrap();
        std::fs::write(&file, format!("\n{source}")).unwrap();
        let second = batch
            .failure_from_event(&event("second"), &documents)
            .unwrap();
        let next = TestFailureBatch::default()
            .failure_from_event(&event("second"), &documents)
            .unwrap();
        std::fs::remove_file(file).unwrap();
        assert!(first.diagnostic.range.start.line < second.diagnostic.range.start.line);
        assert_eq!(
            next.diagnostic.range.start.line,
            second.diagnostic.range.start.line + 1
        );
    }

    #[test]
    fn failure_site_columns_count_unicode_scalars() {
        let source = "first\n  é 🦊 panic()\n";
        let lines = compute_line_offsets(source);
        let event =
            serde_json::json!({"report": {"site": {"file": "sample.hew", "line": 2, "col": 7}}});
        let offset = site_offset(&event, Path::new("sample.hew"), source, &lines).unwrap();
        assert!(source[offset..].starts_with("panic"));
    }
}
