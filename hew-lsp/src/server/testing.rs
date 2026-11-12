//! Map the CLI test event stream back to editor locations.

use std::path::Path;

use dashmap::DashMap;
use hew_analysis::util::compute_line_offsets;
use serde_json::Value;
use tower_lsp_server::ls_types::{Diagnostic, DiagnosticSeverity, Uri as Url};

use super::analysis::{open_document_uri, source_for_path};
use super::uri::FileUriExt;
use super::{offset_range_to_lsp, DocumentState};

pub(super) struct TestFailure {
    pub uri: Url,
    pub selector: String,
    pub diagnostic: Diagnostic,
    pub seed: Option<String>,
}

pub(super) fn failure_from_event(
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
    let source = source_for_path(path, documents)?;
    let lines = compute_line_offsets(&source);
    let offset = site_offset(event, path, &source, &lines).or_else(|| {
        let parsed = hew_parser::parse(&source);
        hew_analysis::test_discovery::discover_tests(&parsed.program)
            .into_iter()
            .find(|test| test.name == name)
            .map(|test| test.span.start)
    })?;
    let end = source[offset..]
        .chars()
        .next()
        .map_or(offset, |character| offset + character.len_utf8());
    let range = offset_range_to_lsp(&source, &lines, offset, end);
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
    fn failure_site_columns_count_unicode_scalars() {
        let source = "first\n  é 🦊 panic()\n";
        let lines = compute_line_offsets(source);
        let event =
            serde_json::json!({"report": {"site": {"file": "sample.hew", "line": 2, "col": 7}}});
        let offset = site_offset(&event, Path::new("sample.hew"), source, &lines).unwrap();
        assert!(source[offset..].starts_with("panic"));
    }
}
