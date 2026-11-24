use std::io;
use std::io::Write;
use std::sync::Mutex;

use super::*;

#[derive(Clone, Default)]
struct SharedLogWriter {
    buffer: Arc<Mutex<Vec<u8>>>,
}

impl SharedLogWriter {
    fn contents(&self) -> String {
        String::from_utf8(self.buffer.lock().unwrap().clone()).unwrap()
    }
}

struct SharedLogGuard(Arc<Mutex<Vec<u8>>>);

impl Write for SharedLogGuard {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.lock().unwrap().extend_from_slice(buf);
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

impl<'a> tracing_subscriber::fmt::MakeWriter<'a> for SharedLogWriter {
    type Writer = SharedLogGuard;

    fn make_writer(&'a self) -> Self::Writer {
        SharedLogGuard(self.buffer.clone())
    }
}

fn capture_logs<T>(f: impl FnOnce() -> T) -> (T, String) {
    let writer = SharedLogWriter::default();
    let subscriber = tracing_subscriber::fmt()
        .with_writer(writer.clone())
        .with_ansi(false)
        .without_time()
        .finish();
    let result = tracing::subscriber::with_default(subscriber, f);
    (result, writer.contents())
}

// ── Code action tests ───────────────────────────────────────────

fn diagnostic_with_fixes(uri: &Url, source: &str, kind: &str) -> Diagnostic {
    analyzed_diagnostics(uri, source)
        .into_iter()
        .find(|diagnostic| {
            diagnostic
                .data
                .as_ref()
                .and_then(|data| data.get("kind"))
                .and_then(Value::as_str)
                == Some(kind)
        })
        .unwrap_or_else(|| panic!("missing {kind} diagnostic"))
}

fn action_edit(action: &CodeActionOrCommand) -> &tower_lsp_server::ls_types::TextDocumentEdit {
    let CodeActionOrCommand::CodeAction(action) = action else {
        panic!("expected code action")
    };
    let Some(tower_lsp_server::ls_types::DocumentChanges::Edits(edits)) = action
        .edit
        .as_ref()
        .and_then(|edit| edit.document_changes.as_ref())
    else {
        panic!("expected versioned document edit")
    };
    &edits[0]
}

#[test]
fn code_actions_use_producer_spans_and_utf16_versions() {
    let source = "fn main() -> i32 { let counter = 42; let _ = \"😀\"; counte }";
    let uri = Url::parse("file:///test.hew").unwrap();
    let doc = make_doc(source);
    let diagnostic = diagnostic_with_fixes(&uri, source, "UndefinedVariable");
    let info = serde_json::from_value::<hew_analysis::code_actions::DiagnosticInfo>(
        diagnostic.data.clone().unwrap(),
    )
    .unwrap();
    let producer = hew_analysis::code_actions::build_code_actions(source, &[info]);
    let actions = lsp_code_actions_for_diagnostic(&uri, &doc, &diagnostic, None, Some(7));
    assert_eq!(actions.len(), producer.len());
    let edit = action_edit(&actions[0]);
    assert_eq!(edit.text_document.version, Some(7));
    let OneOf::Left(text) = &edit.edits[0] else {
        panic!("expected text edit")
    };
    let authored = &producer[0].edits[0];
    assert_eq!(
        position_to_offset(source, &doc.line_offsets, text.range.start),
        authored.span.start
    );
    assert_eq!(
        position_to_offset(source, &doc.line_offsets, text.range.end),
        authored.span.end
    );
    assert_eq!(text.new_text, authored.new_text);
    assert_eq!(&source[authored.span.start..authored.span.end], "counte");
}

#[test]
fn code_actions_for_non_exhaustive_match_preserve_placeholder_choice() {
    let source = "enum Colour { Red; Blue; } fn label(colour: Colour) -> string { match colour { .Red => \"red\" } }";
    let uri = Url::parse("file:///test.hew").unwrap();
    let diagnostic = diagnostic_with_fixes(&uri, source, "NonExhaustiveMatch");
    let info = serde_json::from_value::<hew_analysis::code_actions::DiagnosticInfo>(
        diagnostic.data.unwrap(),
    )
    .unwrap();
    assert!(hew_analysis::code_actions::machine_applicable_edits(
        source,
        std::slice::from_ref(&info)
    )
    .unwrap()
    .is_empty());
    let actions = hew_analysis::code_actions::build_code_actions(source, &[info]);
    assert_eq!(actions.len(), 1);
    assert_eq!(actions[0].title, "Add missing match arms");
    let updated = hew_analysis::code_actions::apply_edits(source, &actions[0].edits).unwrap();
    assert!(updated.contains(".Blue"));
    assert!(hew_parser::parse(&updated).errors.is_empty(), "{updated}");
    assert!(
        !analyzed_diagnostics(&uri, &updated)
            .iter()
            .any(|diagnostic| diagnostic.severity == Some(DiagnosticSeverity::ERROR)),
        "{updated}"
    );
}

#[test]
fn code_actions_keep_unused_import_source_action_and_kind_filters() {
    let source = "import std.string;\nfn main() {}\n";
    let uri = Url::parse("file:///test.hew").unwrap();
    let doc = make_doc(source);
    let diagnostic = diagnostic_with_fixes(&uri, source, "UnusedImport");
    let source_only = lsp_code_actions_for_diagnostic(
        &uri,
        &doc,
        &diagnostic,
        Some(&[remove_unused_imports_kind()]),
        Some(2),
    );
    assert_eq!(source_only.len(), 1);
    let CodeActionOrCommand::CodeAction(action) = &source_only[0] else {
        panic!("expected action")
    };
    assert_eq!(action.kind, Some(remove_unused_imports_kind()));
    let edit = action_edit(&source_only[0]);
    let OneOf::Left(text) = &edit.edits[0] else {
        panic!("expected text edit")
    };
    assert_eq!(text.new_text, "");
    let quick = lsp_code_actions_for_diagnostic(
        &uri,
        &doc,
        &diagnostic,
        Some(&[CodeActionKind::QUICKFIX]),
        Some(2),
    );
    assert_eq!(quick.len(), 1);
    let CodeActionOrCommand::CodeAction(action) = &quick[0] else {
        panic!("expected action")
    };
    assert_eq!(action.kind, Some(CodeActionKind::QUICKFIX));
}

#[test]
fn code_actions_refuse_stale_foreign_and_malformed_facts() {
    let source = "fn main() -> i32 { let counter = 42; counte }";
    let uri = Url::parse("file:///test.hew").unwrap();
    let diagnostic = diagnostic_with_fixes(&uri, source, "UndefinedVariable");
    assert!(lsp_code_actions_for_diagnostic(
        &uri,
        &make_doc(&format!("// shift\n{source}")),
        &diagnostic,
        None,
        Some(2)
    )
    .is_empty());
    let other = Url::parse("file:///other.hew").unwrap();
    assert!(
        lsp_code_actions_for_diagnostic(&other, &make_doc(source), &diagnostic, None, Some(2))
            .is_empty()
    );
    let mut malformed = diagnostic;
    malformed.data.as_mut().unwrap()["fixes"] = json!("broken");
    let (actions, logs) = capture_logs(|| {
        lsp_code_actions_for_diagnostic(&uri, &make_doc(source), &malformed, None, Some(2))
    });
    assert!(actions.is_empty());
    assert!(
        logs.contains("invalid structured diagnostic fixes"),
        "{logs}"
    );
}

#[test]
fn code_action_response_uses_current_authored_buffer_and_version() {
    let source = "fn main() -> i32 { let counter = 42; counte }";
    let uri = Url::parse("file:///test.hew").unwrap();
    let diagnostic = diagnostic_with_fixes(&uri, source, "UndefinedVariable");
    let documents = DashMap::new();
    documents.insert(uri.clone(), make_doc(source));
    let params = CodeActionParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        range: diagnostic.range,
        context: CodeActionContext {
            diagnostics: vec![diagnostic],
            only: None,
            trigger_kind: None,
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
    };
    let mut open = HashMap::from([(
        uri.clone(),
        OpenDocument {
            source: source.into(),
            version: 9,
        },
    )]);
    let actions = code_action_response(&documents, &open, &params);
    assert!(!actions.is_empty());
    assert_eq!(action_edit(&actions[0]).text_document.version, Some(9));
    open.get_mut(&uri).unwrap().source = format!("// pending authored edit\n{source}");
    open.get_mut(&uri).unwrap().version = 10;
    assert!(code_action_response(&documents, &open, &params).is_empty());
}

#[tokio::test]
async fn code_action_refuses_did_change_before_debounced_analysis() {
    let source = "fn main() -> i32 { let counter = 42; counte }";
    let uri = Url::parse("file:///test.hew").unwrap();
    let diagnostic = diagnostic_with_fixes(&uri, source, "UndefinedVariable");
    let (service, _socket) = tower_lsp_server::LspService::new(|client| {
        HewLanguageServer::new_with_options(client, Vec::new())
    });
    let server = service.inner();
    server.documents.insert(uri.clone(), make_doc(source));
    server.open_documents.write().unwrap().insert(
        uri.clone(),
        OpenDocument {
            source: source.into(),
            version: 1,
        },
    );
    let params = CodeActionParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        range: diagnostic.range,
        context: CodeActionContext {
            diagnostics: vec![diagnostic],
            only: None,
            trigger_kind: None,
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
    };
    let actions = handlers::language_features::code_action(server, &params);
    assert_eq!(action_edit(&actions[0]).text_document.version, Some(1));
    handlers::text_sync::did_change(
        server,
        tower_lsp_server::ls_types::DidChangeTextDocumentParams {
            text_document: tower_lsp_server::ls_types::VersionedTextDocumentIdentifier {
                uri,
                version: 2,
            },
            content_changes: vec![tower_lsp_server::ls_types::TextDocumentContentChangeEvent {
                range: None,
                range_length: None,
                text: format!("// shifted\n{source}"),
            }],
        },
    )
    .await;
    assert!(handlers::language_features::code_action(server, &params).is_empty());
}

#[test]
fn code_action_unknown_document_warns_but_empty_request_is_quiet() {
    let uri = Url::parse("file:///test.hew").unwrap();
    let params = CodeActionParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        range: Range::default(),
        context: CodeActionContext {
            diagnostics: vec![],
            only: None,
            trigger_kind: None,
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
    };
    let (actions, logs) =
        capture_logs(|| code_action_response(&DashMap::new(), &HashMap::new(), &params));
    assert!(actions.is_empty());
    assert!(logs.contains("unknown document"));
    let source = "fn main() {}";
    let documents = DashMap::new();
    documents.insert(uri.clone(), make_doc(source));
    let open = HashMap::from([(
        uri,
        OpenDocument {
            source: source.into(),
            version: 1,
        },
    )]);
    let (actions, logs) = capture_logs(|| code_action_response(&documents, &open, &params));
    assert!(actions.is_empty());
    assert!(logs.is_empty(), "{logs}");
}
