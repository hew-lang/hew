use serde_json::Value;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    CodeLens, CodeLensParams, ExecuteCommandParams, MessageType, SymbolInformation,
    WorkspaceSymbolParams,
};

use super::super::{
    build_code_lenses, collect_project_workspace_symbols, non_empty, HewLanguageServer,
    RUN_TEST_COMMAND,
};

pub(crate) fn extract_run_test_name(arguments: &[Value]) -> Option<String> {
    let first = arguments.first()?;
    match first {
        Value::String(name) if !name.is_empty() => Some(name.clone()),
        Value::Object(map) => map
            .get("name")
            .and_then(Value::as_str)
            .filter(|name| !name.is_empty())
            .map(str::to_string),
        _ => None,
    }
}

pub(crate) fn code_lens(
    server: &HewLanguageServer,
    params: &CodeLensParams,
) -> Option<Vec<CodeLens>> {
    let uri = &params.text_document.uri;
    let doc = server.documents.get(uri)?;

    let lenses = build_code_lenses(&doc.source, &doc.line_offsets, &doc.parse_result);
    non_empty(lenses)
}

pub(crate) fn symbol(
    server: &HewLanguageServer,
    params: &WorkspaceSymbolParams,
) -> Option<Vec<SymbolInformation>> {
    let roots = server
        .workspace_roots
        .read()
        .map_or_else(|_| Vec::new(), |roots| roots.clone());
    let symbols = collect_project_workspace_symbols(&server.documents, &roots, &params.query);
    non_empty(symbols)
}

pub(crate) async fn execute_command(
    server: &HewLanguageServer,
    params: ExecuteCommandParams,
) -> Result<Option<Value>> {
    match params.command.as_str() {
        RUN_TEST_COMMAND => {
            let Some(test_name) = extract_run_test_name(&params.arguments) else {
                server
                    .client
                    .show_message(
                        MessageType::ERROR,
                        "Cannot run Hew test: missing test name argument.",
                    )
                    .await;
                return Ok(None);
            };
            server.run_test_command(&test_name).await
        }
        other => {
            server
                .client
                .log_message(
                    MessageType::WARNING,
                    format!("Unsupported command `{other}`"),
                )
                .await;
            Ok(None)
        }
    }
}
