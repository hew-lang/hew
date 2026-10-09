use std::path::PathBuf;

use serde_json::Value;
use tower_lsp_server::jsonrpc::Result;
use tower_lsp_server::ls_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    InitializeParams, InitializeResult, MessageType, ServerCapabilities,
};

use super::super::uri::FileUriExt;
use super::super::{
    build_server_capabilities, close_document_and_dependents, HewLanguageServer, OpenDocument,
};

/// Roots keep the editor's spelling: every URI the server reports for a file
/// found under a root is built from it. Containment and identity checks
/// resolve both sides physically where they compare.
fn extract_workspace_roots(params: &InitializeParams) -> Vec<PathBuf> {
    let mut roots = Vec::with_capacity(params.workspace_folders.as_ref().map_or(1, Vec::len));
    if let Some(folders) = &params.workspace_folders {
        for folder in folders {
            if let Some(path) = folder.uri.to_checked_file_path() {
                roots.push(path.into_owned());
            }
        }
    }
    if roots.is_empty() {
        #[allow(
            deprecated,
            reason = "root_uri remains the LSP fallback for older clients"
        )]
        {
            if let Some(root_uri) = &params.root_uri {
                if let Some(path) = root_uri.to_checked_file_path() {
                    roots.push(path.into_owned());
                }
            }
        }
    }
    #[expect(deprecated, reason = "LSP root_path is a fallback for older clients")]
    if roots.is_empty() {
        if let Some(root_path) = &params.root_path {
            roots.push(PathBuf::from(root_path));
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
    use tower_lsp_server::jsonrpc::{Error, ErrorCode};

    // Serialise to Value so that the test helper `build_initialize_result_from_caps_json`
    // can share the decode path.  The `experimental.typeHierarchyProvider` field set in
    // `experimental` is a standard `ServerCapabilities` field in ls-types,
    // so the type hierarchy advertisement survives this round-trip.
    let caps_json = serde_json::to_value(capabilities).map_err(|error| Error {
        code: ErrorCode::InternalError,
        message: format!("failed to encode LSP server capabilities: {error}").into(),
        data: None,
    })?;
    build_initialize_result_from_caps_json(caps_json)
}

pub(crate) fn build_initialize_result_from_caps_json(caps_json: Value) -> Result<InitializeResult> {
    use tower_lsp_server::jsonrpc::{Error, ErrorCode};

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
    if let Ok(mut supported) = server.rename_document_changes.write() {
        *supported = params
            .capabilities
            .workspace
            .as_ref()
            .and_then(|workspace| workspace.workspace_edit.as_ref())
            .and_then(|edit| edit.document_changes)
            .unwrap_or(false);
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

#[expect(
    clippy::unused_async,
    reason = "called from an async LanguageServer impl; reanalyze is now sync (spawns internally)"
)]
pub(crate) async fn did_open(server: &HewLanguageServer, params: DidOpenTextDocumentParams) {
    let uri = params.text_document.uri;
    let source = params.text_document.text;
    if let Ok(mut open) = server.open_documents.write() {
        open.insert(
            uri.clone(),
            OpenDocument {
                source: source.clone(),
                version: params.text_document.version,
            },
        );
    }
    server.reanalyze(&uri, &source);
}

#[expect(
    clippy::unused_async,
    reason = "called from an async LanguageServer impl; reanalyze is now sync (spawns internally)"
)]
pub(crate) async fn did_change(server: &HewLanguageServer, params: DidChangeTextDocumentParams) {
    let uri = params.text_document.uri;
    if let Some(change) = params.content_changes.into_iter().last() {
        if let Ok(mut open) = server.open_documents.write() {
            open.insert(
                uri.clone(),
                OpenDocument {
                    source: change.text.clone(),
                    version: params.text_document.version,
                },
            );
        }
        server.reanalyze(&uri, &change.text);
    }
}

pub(crate) async fn did_close(server: &HewLanguageServer, params: DidCloseTextDocumentParams) {
    if let Ok(mut open) = server.open_documents.write() {
        open.remove(&params.text_document.uri);
    }
    server.test_diagnostics.remove(&params.text_document.uri);
    let published = close_document_and_dependents(
        &params.text_document.uri,
        &server.documents,
        &server.extra_pkg_paths,
    );
    for (updated_uri, diagnostics) in
        super::super::with_test_diagnostics(published, &server.test_diagnostics)
    {
        server
            .client
            .publish_diagnostics(updated_uri, diagnostics, None)
            .await;
    }
}
