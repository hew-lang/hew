//! Map the CLI test event stream back to editor locations.

use std::path::Path;

use dashmap::DashMap;
use hew_analysis::util::compute_line_offsets;
use serde_json::Value;
use tower_lsp_server::lsp_types::{Diagnostic, DiagnosticSeverity, Uri as Url};

use super::analysis::source_for_path;
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
    let uri = Url::from_file_path(path)?;
    let source = source_for_path(path, documents)?;
    let lines = compute_line_offsets(&source);
    let offset = event
        .pointer("/report/site_offset")
        .and_then(Value::as_u64)
        .and_then(|value| usize::try_from(value).ok())
        .or_else(|| {
            let parsed = hew_parser::parse(&source);
            hew_analysis::test_discovery::discover_tests(&parsed.program)
                .into_iter()
                .find(|test| test.name == name)
                .map(|test| test.span.start)
        })?;
    let range = offset_range_to_lsp(&source, &lines, offset, offset.saturating_add(1));
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
