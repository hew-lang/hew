use std::path::PathBuf;

use serde_json::Value;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    InitializeParams, InitializeResult, MessageType, ServerCapabilities,
};

use super::super::{
    build_server_capabilities, close_document_and_dependents, normalize_workspace_root,
    HewLanguageServer,
};

fn extract_workspace_roots(params: &InitializeParams) -> Vec<PathBuf> {
    let mut roots = Vec::with_capacity(params.workspace_folders.as_ref().map_or(1, Vec::len));
    if let Some(folders) = &params.workspace_folders {
        for folder in folders {
            if let Ok(path) = folder.uri.to_file_path() {
                roots.push(normalize_workspace_root(path));
            }
        }
    }
    if roots.is_empty() {
        if let Some(root_uri) = &params.root_uri {
            if let Ok(path) = root_uri.to_file_path() {
                roots.push(normalize_workspace_root(path));
            }
        }
    }
    #[expect(deprecated, reason = "LSP root_path is a fallback for older clients")]
    if roots.is_empty() {
        if let Some(root_path) = &params.root_path {
            roots.push(normalize_workspace_root(PathBuf::from(root_path)));
        }
    }
    roots.sort();
    roots.dedup();
    roots
}

fn decode_server_capabilities(caps_json: Value) -> std::result::Result<ServerCapabilities, String> {
    serde_json::from_value(caps_json).map_err(|error| error.to_string())
}

fn build_initialize_result(capabilities: &ServerCapabilities) -> Result<InitializeResult> {
    use tower_lsp::jsonrpc::{Error, ErrorCode};

    let mut caps_json = serde_json::to_value(capabilities).map_err(|error| Error {
        code: ErrorCode::InternalError,
        message: format!("failed to encode LSP server capabilities: {error}").into(),
        data: None,
    })?;
    if let serde_json::Value::Object(ref mut map) = caps_json {
        map.insert("typeHierarchyProvider".to_string(), serde_json::json!(true));
    }
    build_initialize_result_from_caps_json(caps_json)
}

pub(crate) fn build_initialize_result_from_caps_json(caps_json: Value) -> Result<InitializeResult> {
    use tower_lsp::jsonrpc::{Error, ErrorCode};

    let capabilities = decode_server_capabilities(caps_json).map_err(|error| Error {
        code: ErrorCode::InternalError,
        message: format!("failed to decode LSP server capabilities: {error}").into(),
        data: None,
    })?;

    Ok(InitializeResult {
        capabilities,
        ..Default::default()
    })
}

pub(crate) fn initialize(
    server: &HewLanguageServer,
    params: &InitializeParams,
) -> Result<InitializeResult> {
    if let Ok(mut roots) = server.workspace_roots.write() {
        *roots = extract_workspace_roots(params);
    }
    let capabilities = build_server_capabilities();
    build_initialize_result(&capabilities)
}

pub(crate) async fn initialized(server: &HewLanguageServer) {
    server
        .client
        .log_message(MessageType::INFO, "Hew language server initialized")
        .await;
}

pub(crate) fn shutdown(_: &HewLanguageServer) {}

pub(crate) async fn did_open(server: &HewLanguageServer, params: DidOpenTextDocumentParams) {
    let uri = params.text_document.uri;
    let source = params.text_document.text;
    server.reanalyze(&uri, &source).await;
}

pub(crate) async fn did_change(server: &HewLanguageServer, params: DidChangeTextDocumentParams) {
    let uri = params.text_document.uri;
    if let Some(change) = params.content_changes.into_iter().last() {
        server.reanalyze(&uri, &change.text).await;
    }
}

pub(crate) async fn did_close(server: &HewLanguageServer, params: DidCloseTextDocumentParams) {
    for (updated_uri, diagnostics) in
        close_document_and_dependents(&params.text_document.uri, &server.documents)
    {
        server
            .client
            .publish_diagnostics(updated_uri, diagnostics, None)
            .await;
    }
}
