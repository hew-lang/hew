use serde_json::Value;
use tower_lsp_server::jsonrpc::Result;
use tower_lsp_server::ls_types::{
    CodeLens, CodeLensParams, ExecuteCommandParams, MessageType, SymbolInformation,
    WorkspaceSymbolParams,
};

use super::super::uri::FileUriExt;
use super::super::workspace::build_code_lenses_with_seeds;
use super::super::workspace::normalize_workspace_path;
use super::super::{
    collect_project_workspace_symbols, non_empty, HewLanguageServer, GENERATE_COMMAND,
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

fn extract_run_test_seed(arguments: &[Value]) -> Option<&str> {
    arguments.first()?.get("seed")?.as_str()
}

pub(crate) fn code_lens(
    server: &HewLanguageServer,
    params: &CodeLensParams,
) -> Option<Vec<CodeLens>> {
    let uri = &params.text_document.uri;
    let doc = server.documents.get(uri)?;

    let path = uri.to_checked_file_path()?;
    let file = normalize_workspace_path(&path).display().to_string();
    let lenses = build_code_lenses_with_seeds(
        &doc.source,
        &doc.line_offsets,
        &doc.parse_result,
        doc.expanded_program.as_deref(),
        &file,
        Some(&server.test_seeds),
    );
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
        GENERATE_COMMAND => {
            let Some(root) = params.arguments.first().and_then(Value::as_str) else {
                return Err(tower_lsp_server::jsonrpc::Error::invalid_params(
                    "missing generator package directory",
                ));
            };
            let mut command = tokio::process::Command::new(super::super::hew_cli_executable());
            command.args(["generate", root]).kill_on_drop(true);
            let output = tokio::time::timeout(std::time::Duration::from_mins(10), command.output())
                .await
                .map_err(|_| super::super::internal_error("generation command timed out"))?
                .map_err(|error| {
                    super::super::internal_error(format!("cannot run generation: {error}"))
                })?;
            let message = format!(
                "{}{}",
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr)
            );
            server
                .client
                .show_message(
                    if output.status.success() {
                        MessageType::INFO
                    } else {
                        MessageType::ERROR
                    },
                    message,
                )
                .await;
            if output.status.success() {
                let _analysis = server.analysis_jobs.lock().await;
                let documents = server
                    .documents
                    .iter()
                    .map(|document| (document.key().clone(), document.source.clone()))
                    .collect::<Vec<_>>();
                for (uri, source) in documents {
                    server.reanalyze(&uri, &source);
                }
            }
            Ok(Some(
                serde_json::json!({ "success": output.status.success() }),
            ))
        }
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
            server
                .run_test_command(&test_name, extract_run_test_seed(&params.arguments))
                .await
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
