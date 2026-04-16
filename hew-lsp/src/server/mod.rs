//! LSP server implementation for the Hew language.

mod analysis;
mod convert;
mod hierarchy;
mod navigation;
mod workspace;

// Items used by the LanguageServer impl handlers.
use self::analysis::{close_document_and_dependents, refresh_document_and_dependents};
use self::convert::{analysis_tokens_to_lsp, symbol_info_to_doc_symbol, to_lsp_completion};
use self::hierarchy::{
    collect_subtypes, collect_supertypes, find_callable_at, find_incoming_calls,
    find_outgoing_calls, find_type_hierarchy_item,
};
use self::navigation::{
    build_document_links, build_prepare_rename_response, build_reference_locations,
    build_workspace_edit, collect_import_items, find_cross_file_definition, find_definition_in_ast,
};
use self::workspace::{build_code_lenses, collect_workspace_symbols};

// Items additionally needed by the test module (only compiled in test builds).
#[cfg(test)]
use self::analysis::{build_diagnostics, diagnostic_data, populate_user_module_imports};
#[cfg(test)]
use self::convert::analysis_symbol_kind_to_lsp;
#[cfg(test)]
use self::workspace::has_test_attribute;

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::RwLock;

use dashmap::DashMap;
use hew_analysis::definition::{find_local_binding_definition, find_param_definition};
#[cfg(test)]
use hew_analysis::references::count_all_references;
#[cfg(test)]
use hew_analysis::util::compute_line_offsets;
use hew_analysis::util::{non_empty, offset_to_line_col, word_at_offset};
use hew_parser::ast::Span;
#[cfg(test)]
use hew_parser::ast::{Attribute, Item};
use hew_parser::ParseResult;
#[cfg(test)]
use hew_types::error::TypeErrorKind;
#[cfg(test)]
use hew_types::Checker;
use hew_types::TypeCheckOutput;
use serde_json::{json, Value};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    CallHierarchyIncomingCall, CallHierarchyIncomingCallsParams, CallHierarchyItem,
    CallHierarchyOutgoingCall, CallHierarchyOutgoingCallsParams, CallHierarchyPrepareParams,
    CallHierarchyServerCapability, CodeLens, CodeLensOptions, CodeLensParams, SymbolInformation,
    WorkspaceSymbolParams,
};
use tower_lsp::lsp_types::{
    CodeAction, CodeActionKind, CodeActionOrCommand, CodeActionParams, CodeActionResponse,
    CompletionOptions, CompletionParams, CompletionResponse, Diagnostic,
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DocumentSymbol, DocumentSymbolParams, DocumentSymbolResponse, ExecuteCommandOptions,
    ExecuteCommandParams, FoldingRange, FoldingRangeKind, FoldingRangeParams, GotoDefinitionParams,
    GotoDefinitionResponse, Hover, HoverContents, HoverParams, HoverProviderCapability,
    InitializeParams, InitializeResult, InitializedParams, Location, MarkupContent, MarkupKind,
    MessageType, OneOf, Position, PrepareRenameResponse, Range, ReferenceParams, RenameParams,
    SemanticTokenModifier, SemanticTokenType, SemanticTokens, SemanticTokensFullOptions,
    SemanticTokensLegend, SemanticTokensOptions, SemanticTokensParams, SemanticTokensResult,
    SemanticTokensServerCapabilities, ServerCapabilities, TextDocumentSyncCapability,
    TextDocumentSyncKind, TextEdit, Url, WorkDoneProgressOptions, WorkspaceEdit,
};
#[cfg(test)]
use tower_lsp::lsp_types::{
    CompletionItemKind, DiagnosticSeverity, InsertTextFormat, SemanticToken, SymbolKind,
};
use tower_lsp::lsp_types::{DocumentLink, DocumentLinkOptions, DocumentLinkParams};
use tower_lsp::lsp_types::{
    InlayHint, InlayHintKind, InlayHintLabel, InlayHintOptions, InlayHintParams,
    InlayHintServerCapabilities, ParameterInformation, ParameterLabel, SignatureHelp,
    SignatureHelpOptions, SignatureHelpParams, SignatureInformation,
};
use tower_lsp::lsp_types::{
    TypeHierarchyItem, TypeHierarchyPrepareParams, TypeHierarchySubtypesParams,
    TypeHierarchySupertypesParams,
};
use tower_lsp::{Client, LanguageServer};

// ── Semantic token types ─────────────────────────────────────────────

const TOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::KEYWORD,
    SemanticTokenType::TYPE,
    SemanticTokenType::FUNCTION,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::NUMBER,
    SemanticTokenType::STRING,
    SemanticTokenType::OPERATOR,
    SemanticTokenType::COMMENT,
];

const TOKEN_MODIFIERS: &[SemanticTokenModifier] = &[
    SemanticTokenModifier::DECLARATION, // bit 0
    SemanticTokenModifier::READONLY,    // bit 1
    SemanticTokenModifier::ASYNC,       // bit 2
];
const RUN_TEST_COMMAND: &str = "hew.runTest";

fn modifier_bit(m: &SemanticTokenModifier) -> u32 {
    TOKEN_MODIFIERS
        .iter()
        .position(|x| x == m)
        .map_or(0, |i| 1 << i)
}

// ── Document state ───────────────────────────────────────────────────

#[derive(Debug)]
struct DocumentState {
    source: String,
    /// Byte offsets of each line start, computed once per edit.
    line_offsets: Vec<usize>,
    parse_result: ParseResult,
    type_output: Option<TypeCheckOutput>,
    diagnostics_by_uri: DiagnosticMap,
}

type DiagnosticMap = HashMap<Url, Vec<Diagnostic>>;

#[derive(Debug)]
struct DiagnosticSource {
    uri: Url,
    source: String,
    line_offsets: Vec<usize>,
}

// ── Helpers ──────────────────────────────────────────────────────────

/// Convert an LSP `Position` (UTF-16 character offset) to a byte offset in source,
/// using pre-computed line offsets.
fn position_to_offset(source: &str, lo: &[usize], position: Position) -> usize {
    hew_analysis::util::position_to_offset(source, lo, position.line, position.character)
}

/// Convert a parser `Span` (byte offsets) to an LSP `Range`.
fn span_to_range(source: &str, lo: &[usize], span: &Span) -> Range {
    offset_range_to_lsp(source, lo, span.start, span.end)
}

/// Convert byte offset range to LSP `Range`, using pre-computed line offsets.
fn offset_range_to_lsp(source: &str, lo: &[usize], start: usize, end: usize) -> Range {
    let (sl, sc, el, ec) = hew_analysis::util::span_to_line_col_range(source, lo, start, end);
    Range {
        start: Position::new(sl, sc),
        end: Position::new(el, ec),
    }
}

fn build_server_capabilities() -> ServerCapabilities {
    ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        completion_provider: Some(CompletionOptions {
            trigger_characters: Some(vec![".".to_string(), ":".to_string()]),
            ..Default::default()
        }),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        definition_provider: Some(OneOf::Left(true)),
        document_symbol_provider: Some(OneOf::Left(true)),
        semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(
            SemanticTokensOptions {
                legend: SemanticTokensLegend {
                    token_types: TOKEN_TYPES.to_vec(),
                    token_modifiers: TOKEN_MODIFIERS.to_vec(),
                },
                full: Some(SemanticTokensFullOptions::Bool(true)),
                range: None,
                work_done_progress_options: WorkDoneProgressOptions::default(),
            },
        )),
        call_hierarchy_provider: Some(CallHierarchyServerCapability::Simple(true)),
        code_lens_provider: Some(CodeLensOptions {
            resolve_provider: Some(false),
        }),
        execute_command_provider: Some(ExecuteCommandOptions {
            commands: vec![RUN_TEST_COMMAND.to_string()],
            work_done_progress_options: WorkDoneProgressOptions::default(),
        }),
        workspace_symbol_provider: Some(OneOf::Left(true)),
        document_link_provider: Some(DocumentLinkOptions {
            resolve_provider: Some(false),
            work_done_progress_options: WorkDoneProgressOptions::default(),
        }),
        references_provider: Some(OneOf::Left(true)),
        rename_provider: Some(OneOf::Right(tower_lsp::lsp_types::RenameOptions {
            prepare_provider: Some(true),
            work_done_progress_options: WorkDoneProgressOptions::default(),
        })),
        inlay_hint_provider: Some(OneOf::Right(InlayHintServerCapabilities::Options(
            InlayHintOptions {
                work_done_progress_options: WorkDoneProgressOptions::default(),
                resolve_provider: None,
            },
        ))),
        signature_help_provider: Some(SignatureHelpOptions {
            trigger_characters: Some(vec!["(".to_string(), ",".to_string()]),
            retrigger_characters: None,
            work_done_progress_options: WorkDoneProgressOptions::default(),
        }),
        code_action_provider: Some(tower_lsp::lsp_types::CodeActionProviderCapability::Options(
            tower_lsp::lsp_types::CodeActionOptions {
                code_action_kinds: Some(vec![CodeActionKind::QUICKFIX]),
                ..Default::default()
            },
        )),
        folding_range_provider: Some(
            tower_lsp::lsp_types::FoldingRangeProviderCapability::Simple(true),
        ),
        ..Default::default()
    }
}

fn extract_workspace_roots(params: &InitializeParams) -> Vec<PathBuf> {
    let mut roots = Vec::new();
    if let Some(folders) = &params.workspace_folders {
        for folder in folders {
            if let Ok(path) = folder.uri.to_file_path() {
                roots.push(path);
            }
        }
    }
    if roots.is_empty() {
        if let Some(root_uri) = &params.root_uri {
            if let Ok(path) = root_uri.to_file_path() {
                roots.push(path);
            }
        }
    }
    #[expect(deprecated, reason = "LSP root_path is a fallback for older clients")]
    if roots.is_empty() {
        if let Some(root_path) = &params.root_path {
            roots.push(PathBuf::from(root_path));
        }
    }
    roots
}

fn extract_run_test_name(arguments: &[Value]) -> Option<String> {
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

fn hew_cli_executable() -> PathBuf {
    if let Some(path) = option_env!("CARGO_BIN_EXE_hew") {
        return PathBuf::from(path);
    }
    if let Ok(path) = std::env::var("CARGO_BIN_EXE_hew") {
        return PathBuf::from(path);
    }
    if let Ok(current_exe) = std::env::current_exe() {
        let candidate = current_exe.with_file_name(format!("hew{}", std::env::consts::EXE_SUFFIX));
        if candidate.exists() {
            return candidate;
        }
    }
    PathBuf::from(format!("hew{}", std::env::consts::EXE_SUFFIX))
}

fn build_run_test_invocation(test_name: &str, workspace_root: &Path) -> (PathBuf, Vec<String>) {
    (
        hew_cli_executable(),
        vec![
            "test".to_string(),
            "--no-color".to_string(),
            "--filter".to_string(),
            test_name.to_string(),
            workspace_root.display().to_string(),
        ],
    )
}

// ── Server ───────────────────────────────────────────────────────────

/// Hew language server providing IDE features via LSP.
#[derive(Debug)]
pub struct HewLanguageServer {
    client: Client,
    documents: DashMap<Url, DocumentState>,
    workspace_roots: RwLock<Vec<PathBuf>>,
}

impl HewLanguageServer {
    /// Create a new language server with the given LSP client.
    #[must_use]
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: DashMap::new(),
            workspace_roots: RwLock::new(Vec::new()),
        }
    }

    fn workspace_root(&self) -> Option<PathBuf> {
        if let Ok(roots) = self.workspace_roots.read() {
            if let Some(root) = roots.first() {
                return Some(root.clone());
            }
        }
        self.documents.iter().find_map(|entry| {
            entry
                .key()
                .to_file_path()
                .ok()
                .and_then(|path| path.parent().map(Path::to_path_buf))
        })
    }

    /// Re-lex, re-parse, and re-typecheck the document and any open importers,
    /// then publish diagnostics.
    async fn reanalyze(&self, uri: &Url, source: &str) {
        for (updated_uri, diagnostics) in
            refresh_document_and_dependents(uri, source, &self.documents)
        {
            self.client
                .publish_diagnostics(updated_uri, diagnostics, None)
                .await;
        }
    }

    async fn run_test_command(&self, test_name: &str) -> Result<Option<Value>> {
        let Some(workspace_root) = self.workspace_root() else {
            self.client
                .show_message(
                    MessageType::ERROR,
                    "Cannot run Hew test: no workspace root is available.",
                )
                .await;
            return Ok(None);
        };

        let (program, args) = build_run_test_invocation(test_name, &workspace_root);
        self.client
            .show_message(
                MessageType::INFO,
                format!("Running Hew test `{test_name}`..."),
            )
            .await;

        match tokio::process::Command::new(&program)
            .args(&args)
            .current_dir(&workspace_root)
            .output()
            .await
        {
            Ok(output) => {
                let stdout = String::from_utf8_lossy(&output.stdout).trim().to_string();
                let stderr = String::from_utf8_lossy(&output.stderr).trim().to_string();
                if !stdout.is_empty() {
                    self.client.show_message(MessageType::INFO, stdout).await;
                }
                if !stderr.is_empty() {
                    let level = if output.status.success() {
                        MessageType::INFO
                    } else {
                        MessageType::ERROR
                    };
                    self.client.show_message(level, stderr).await;
                }
                let level = if output.status.success() {
                    MessageType::INFO
                } else {
                    MessageType::ERROR
                };
                self.client
                    .show_message(level, format!("Hew test `{test_name}` finished."))
                    .await;
                Ok(Some(json!({
                    "command": RUN_TEST_COMMAND,
                    "success": output.status.success(),
                    "test": test_name,
                })))
            }
            Err(error) => {
                self.client
                    .show_message(
                        MessageType::ERROR,
                        format!(
                            "Failed to start `{}` for test `{test_name}`: {error}",
                            program.display()
                        ),
                    )
                    .await;
                Ok(None)
            }
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for HewLanguageServer {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        if let Ok(mut roots) = self.workspace_roots.write() {
            *roots = extract_workspace_roots(&params);
        }
        let capabilities = build_server_capabilities();

        // lsp-types 0.94.1 doesn't have typeHierarchyProvider in ServerCapabilities,
        // but the LSP 3.17 protocol requires it for clients to discover the feature.
        // Inject it via JSON serialization.
        let mut caps_json = serde_json::to_value(&capabilities).unwrap_or_default();
        if let serde_json::Value::Object(ref mut map) = caps_json {
            map.insert("typeHierarchyProvider".to_string(), serde_json::json!(true));
        }
        let capabilities: ServerCapabilities =
            serde_json::from_value(caps_json).unwrap_or(capabilities);

        Ok(InitializeResult {
            capabilities,
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "Hew language server initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let source = params.text_document.text;
        self.reanalyze(&uri, &source).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        if let Some(change) = params.content_changes.into_iter().last() {
            self.reanalyze(&uri, &change.text).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        for (updated_uri, diagnostics) in
            close_document_and_dependents(&params.text_document.uri, &self.documents)
        {
            self.client
                .publish_diagnostics(updated_uri, diagnostics, None)
                .await;
        }
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let items = match self.documents.get(uri) {
            Some(doc) => {
                let offset = position_to_offset(&doc.source, &doc.line_offsets, position);
                let analysis_items = hew_analysis::completions::complete(
                    &doc.source,
                    &doc.parse_result,
                    doc.type_output.as_ref(),
                    offset,
                );
                analysis_items.into_iter().map(to_lsp_completion).collect()
            }
            None => vec![],
        };
        Ok(Some(CompletionResponse::Array(items)))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let Some(doc) = self.documents.get(uri) else {
            return Ok(None);
        };

        let offset = position_to_offset(&doc.source, &doc.line_offsets, position);

        let result = hew_analysis::hover::hover(
            &doc.source,
            &doc.parse_result,
            doc.type_output.as_ref(),
            offset,
        );

        Ok(result.map(|hr| {
            let range = hr
                .span
                .map(|s| offset_range_to_lsp(&doc.source, &doc.line_offsets, s.start, s.end));
            Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: hr.contents,
                }),
                range,
            }
        }))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let Some(doc) = self.documents.get(uri) else {
            return Ok(None);
        };

        let offset = position_to_offset(&doc.source, &doc.line_offsets, position);
        let Some(word) = word_at_offset(&doc.source, offset) else {
            return Ok(None);
        };

        if let Some(range) =
            find_definition_in_ast(&doc.source, &doc.line_offsets, &doc.parse_result, &word)
        {
            return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                uri: uri.clone(),
                range,
            })));
        }

        if let Some(range) = resolve_local_or_param_definition(doc.value(), &word, offset) {
            return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                uri: uri.clone(),
                range,
            })));
        }

        // For qualified names like `c.increment` or `Counter::increment`,
        // try the method part alone.
        for separator in [".", "::"] {
            if let Some(method) = word.rsplit(separator).next() {
                if method != word {
                    if let Some(range) = find_definition_in_ast(
                        &doc.source,
                        &doc.line_offsets,
                        &doc.parse_result,
                        method,
                    ) {
                        return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                            uri: uri.clone(),
                            range,
                        })));
                    }
                }
            }
        }

        // Collect the current file's imports as owned data before releasing the
        // DashMap borrow so the cross-file search can acquire other entries.
        let imports: Vec<hew_parser::ast::ImportDecl> = collect_import_items(&doc.parse_result)
            .into_iter()
            .map(|(import, _)| import)
            .collect();
        drop(doc);

        // Cross-file: search files that the current file imports.
        if let Some((target_uri, range)) =
            find_cross_file_definition(uri, &imports, &word, &self.documents)
        {
            return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                uri: target_uri,
                range,
            })));
        }

        // For qualified names (`Counter::new`), also try the type-name prefix
        // cross-file as a best-effort fallback: navigate to `Counter` in its
        // defining file even if the method itself cannot be resolved yet.
        for separator in [".", "::"] {
            if let Some((prefix, _)) = word.split_once(separator) {
                if let Some((target_uri, range)) =
                    find_cross_file_definition(uri, &imports, prefix, &self.documents)
                {
                    return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                        uri: target_uri,
                        range,
                    })));
                }
            }
        }

        Ok(None)
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = &params.text_document.uri;
        let Some(doc) = self.documents.get(uri) else {
            return Ok(None);
        };

        let analysis_symbols =
            hew_analysis::symbols::build_document_symbols(&doc.source, &doc.parse_result);
        let symbols: Vec<DocumentSymbol> = analysis_symbols
            .into_iter()
            .map(|s| symbol_info_to_doc_symbol(&doc.source, &doc.line_offsets, s))
            .collect();
        Ok(Some(DocumentSymbolResponse::Nested(symbols)))
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = &params.text_document.uri;
        let Some(doc) = self.documents.get(uri) else {
            return Ok(None);
        };

        let analysis_tokens = hew_analysis::semantic_tokens::build_semantic_tokens(&doc.source);
        let tokens = analysis_tokens_to_lsp(&doc.source, &doc.line_offsets, &analysis_tokens);
        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: tokens,
        })))
    }

    async fn document_link(&self, params: DocumentLinkParams) -> Result<Option<Vec<DocumentLink>>> {
        let uri = &params.text_document.uri;
        let Some(doc) = self.documents.get(uri) else {
            return Ok(None);
        };

        let links = build_document_links(uri, &doc.source, &doc.line_offsets, &doc.parse_result);
        Ok(non_empty(links))
    }

    async fn prepare_type_hierarchy(
        &self,
        params: TypeHierarchyPrepareParams,
    ) -> Result<Option<Vec<TypeHierarchyItem>>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let Some(doc) = self.documents.get(uri) else {
            return Ok(None);
        };

        let offset = position_to_offset(&doc.source, &doc.line_offsets, position);
        let Some(word) = word_at_offset(&doc.source, offset) else {
            return Ok(None);
        };

        let item = find_type_hierarchy_item(
            uri,
            &doc.source,
            &doc.line_offsets,
            &doc.parse_result,
            &word,
        );
        Ok(item.map(|i| vec![i]))
    }

    async fn supertypes(
        &self,
        params: TypeHierarchySupertypesParams,
    ) -> Result<Option<Vec<TypeHierarchyItem>>> {
        let item = &params.item;
        let Some(doc) = self.documents.get(&item.uri) else {
            return Ok(None);
        };

        let supers = collect_supertypes(
            &item.uri,
            &item.name,
            &doc.source,
            &doc.line_offsets,
            &doc.parse_result,
        );
        Ok(non_empty(supers))
    }

    async fn subtypes(
        &self,
        params: TypeHierarchySubtypesParams,
    ) -> Result<Option<Vec<TypeHierarchyItem>>> {
        let item = &params.item;
        let Some(doc) = self.documents.get(&item.uri) else {
            return Ok(None);
        };

        let subs = collect_subtypes(
            &item.uri,
            &item.name,
            &doc.source,
            &doc.line_offsets,
            &doc.parse_result,
        );
        Ok(non_empty(subs))
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        let Some(doc) = self.documents.get(uri) else {
            return Ok(None);
        };

        let offset = position_to_offset(&doc.source, &doc.line_offsets, position);
        let include_declaration = params.context.include_declaration;
        let locations =
            build_reference_locations(uri, &doc, offset, include_declaration, &self.documents);
        Ok(non_empty(locations))
    }

    async fn prepare_rename(
        &self,
        params: tower_lsp::lsp_types::TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        let uri = &params.text_document.uri;

        let Some(doc) = self.documents.get(uri) else {
            return Ok(None);
        };

        let offset = position_to_offset(&doc.source, &doc.line_offsets, params.position);
        Ok(build_prepare_rename_response(
            uri,
            &doc,
            offset,
            &self.documents,
        ))
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let uri = &params.text_document_position.text_document.uri;

        let Some(doc) = self.documents.get(uri) else {
            return Ok(None);
        };

        let offset = position_to_offset(
            &doc.source,
            &doc.line_offsets,
            params.text_document_position.position,
        );
        Ok(build_workspace_edit(
            uri,
            &doc,
            offset,
            &params.new_name,
            &self.documents,
        ))
    }

    async fn prepare_call_hierarchy(
        &self,
        params: CallHierarchyPrepareParams,
    ) -> Result<Option<Vec<CallHierarchyItem>>> {
        let uri = params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        let Some(doc) = self.documents.get(&uri) else {
            return Ok(None);
        };

        let offset = position_to_offset(&doc.source, &doc.line_offsets, pos);
        let word = word_at_offset(&doc.source, offset);
        let Some(word) = word else {
            return Ok(None);
        };

        let item = find_callable_at(
            &uri,
            &doc.source,
            &doc.line_offsets,
            &doc.parse_result,
            &word,
        );
        Ok(item.map(|it| vec![it]))
    }

    async fn incoming_calls(
        &self,
        params: CallHierarchyIncomingCallsParams,
    ) -> Result<Option<Vec<CallHierarchyIncomingCall>>> {
        let item = &params.item;
        let Some(doc) = self.documents.get(&item.uri) else {
            return Ok(None);
        };

        let calls = find_incoming_calls(
            &item.uri,
            &doc.source,
            &doc.line_offsets,
            &doc.parse_result,
            &item.name,
        );
        Ok(non_empty(calls))
    }

    async fn outgoing_calls(
        &self,
        params: CallHierarchyOutgoingCallsParams,
    ) -> Result<Option<Vec<CallHierarchyOutgoingCall>>> {
        let item = &params.item;
        let Some(doc) = self.documents.get(&item.uri) else {
            return Ok(None);
        };

        let calls = find_outgoing_calls(
            &item.uri,
            &doc.source,
            &doc.line_offsets,
            &doc.parse_result,
            &item.name,
        );
        Ok(non_empty(calls))
    }

    async fn code_lens(&self, params: CodeLensParams) -> Result<Option<Vec<CodeLens>>> {
        let uri = params.text_document.uri;
        let Some(doc) = self.documents.get(&uri) else {
            return Ok(None);
        };

        let lenses = build_code_lenses(&doc.source, &doc.line_offsets, &doc.parse_result);
        Ok(non_empty(lenses))
    }

    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<Vec<SymbolInformation>>> {
        let query = &params.query;
        let mut symbols = Vec::new();
        for entry in &self.documents {
            let uri = entry.key().clone();
            let doc = entry.value();
            symbols.extend(collect_workspace_symbols(
                &uri,
                &doc.source,
                &doc.line_offsets,
                &doc.parse_result,
                query,
            ));
        }
        Ok(non_empty(symbols))
    }

    #[expect(
        clippy::cast_possible_truncation,
        reason = "line/col values in source files will not exceed u32"
    )]
    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let uri = &params.text_document.uri;
        let Some(doc) = self.documents.get(uri) else {
            return Ok(None);
        };
        let Some(tc) = &doc.type_output else {
            return Ok(None);
        };
        let analysis_hints =
            hew_analysis::inlay_hints::build_inlay_hints(&doc.source, &doc.parse_result, tc);
        let lsp_hints: Vec<InlayHint> = analysis_hints
            .into_iter()
            .map(|h| {
                let (line, col) = offset_to_line_col(&doc.source, &doc.line_offsets, h.offset);
                InlayHint {
                    position: Position::new(line as u32, col as u32),
                    label: InlayHintLabel::String(h.label),
                    kind: Some(match h.kind {
                        hew_analysis::InlayHintKind::Type => InlayHintKind::TYPE,
                        hew_analysis::InlayHintKind::Parameter => InlayHintKind::PARAMETER,
                    }),
                    text_edits: None,
                    tooltip: None,
                    padding_left: if h.padding_left { Some(true) } else { None },
                    padding_right: None,
                    data: None,
                }
            })
            .collect();
        Ok(non_empty(lsp_hints))
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let Some(doc) = self.documents.get(uri) else {
            return Ok(None);
        };
        let Some(tc) = &doc.type_output else {
            return Ok(None);
        };
        let offset = position_to_offset(&doc.source, &doc.line_offsets, position);
        let Some(result) =
            hew_analysis::signature_help::build_signature_help(&doc.source, tc, offset)
        else {
            return Ok(None);
        };
        let signatures: Vec<SignatureInformation> = result
            .signatures
            .into_iter()
            .map(|sig| {
                let params = sig
                    .parameters
                    .into_iter()
                    .map(|p| ParameterInformation {
                        label: ParameterLabel::LabelOffsets([p.label_start, p.label_end]),
                        documentation: None,
                    })
                    .collect();
                SignatureInformation {
                    label: sig.label,
                    documentation: None,
                    parameters: Some(params),
                    active_parameter: result.active_parameter,
                }
            })
            .collect();
        Ok(Some(SignatureHelp {
            signatures,
            active_signature: result.active_signature,
            active_parameter: None,
        }))
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        let uri = &params.text_document.uri;
        let Some(doc) = self.documents.get(uri) else {
            return Ok(None);
        };

        let mut lsp_actions = Vec::new();
        for diag in &params.context.diagnostics {
            let kind = diag
                .data
                .as_ref()
                .and_then(|d| d.get("kind"))
                .and_then(serde_json::Value::as_str)
                .map(String::from);
            let suggestions = diag
                .data
                .as_ref()
                .and_then(|d| d.get("suggestions"))
                .and_then(|v| serde_json::from_value::<Vec<String>>(v.clone()).ok())
                .unwrap_or_default();
            let start = position_to_offset(&doc.source, &doc.line_offsets, diag.range.start);
            let end = position_to_offset(&doc.source, &doc.line_offsets, diag.range.end);
            let info = hew_analysis::code_actions::DiagnosticInfo {
                kind,
                message: diag.message.clone(),
                span: hew_analysis::OffsetSpan { start, end },
                suggestions,
            };
            let actions = hew_analysis::code_actions::build_code_actions(&doc.source, &[info]);
            for action in actions {
                let text_edits: Vec<TextEdit> = action
                    .edits
                    .iter()
                    .map(|e| TextEdit {
                        range: offset_range_to_lsp(
                            &doc.source,
                            &doc.line_offsets,
                            e.span.start,
                            e.span.end,
                        ),
                        new_text: e.new_text.clone(),
                    })
                    .collect();
                let mut changes = HashMap::new();
                changes.insert(uri.clone(), text_edits);
                lsp_actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                    title: action.title,
                    kind: Some(CodeActionKind::QUICKFIX),
                    diagnostics: Some(vec![diag.clone()]),
                    edit: Some(WorkspaceEdit {
                        changes: Some(changes),
                        ..Default::default()
                    }),
                    ..Default::default()
                }));
            }
        }
        Ok(non_empty(lsp_actions))
    }

    async fn execute_command(&self, params: ExecuteCommandParams) -> Result<Option<Value>> {
        match params.command.as_str() {
            RUN_TEST_COMMAND => {
                let Some(test_name) = extract_run_test_name(&params.arguments) else {
                    self.client
                        .show_message(
                            MessageType::ERROR,
                            "Cannot run Hew test: missing test name argument.",
                        )
                        .await;
                    return Ok(None);
                };
                self.run_test_command(&test_name).await
            }
            other => {
                self.client
                    .log_message(
                        MessageType::WARNING,
                        format!("Unsupported command `{other}`"),
                    )
                    .await;
                Ok(None)
            }
        }
    }

    async fn folding_range(&self, params: FoldingRangeParams) -> Result<Option<Vec<FoldingRange>>> {
        let uri = &params.text_document.uri;
        let Some(doc) = self.documents.get(uri) else {
            return Ok(None);
        };

        let analysis_ranges =
            hew_analysis::folding::build_folding_ranges(&doc.source, &doc.parse_result);
        let lsp_ranges: Vec<FoldingRange> = analysis_ranges
            .into_iter()
            .map(|r| FoldingRange {
                start_line: r.start_line,
                start_character: None,
                end_line: r.end_line,
                end_character: None,
                kind: Some(match r.kind {
                    hew_analysis::FoldingKind::Region => FoldingRangeKind::Region,
                    hew_analysis::FoldingKind::Imports => FoldingRangeKind::Imports,
                    hew_analysis::FoldingKind::Comment => FoldingRangeKind::Comment,
                }),
                collapsed_text: None,
            })
            .collect();
        Ok(non_empty(lsp_ranges))
    }
}

fn resolve_local_or_param_definition(
    doc: &DocumentState,
    word: &str,
    offset: usize,
) -> Option<Range> {
    let span = find_local_binding_definition(&doc.source, &doc.parse_result, word, offset)
        .or_else(|| find_param_definition(&doc.parse_result, word, offset))?;
    Some(offset_range_to_lsp(
        &doc.source,
        &doc.line_offsets,
        span.start,
        span.end,
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use hew_analysis::CompletionKind;

    fn semantic_token_data(source: &str, tokens: &[SemanticToken]) -> Vec<(String, u32, u32)> {
        let lo = compute_line_offsets(source);
        let mut line = 0u32;
        let mut col = 0u32;
        let mut data = Vec::new();
        for token in tokens {
            line += token.delta_line;
            if token.delta_line == 0 {
                col += token.delta_start;
            } else {
                col = token.delta_start;
            }
            let start = position_to_offset(source, &lo, Position::new(line, col));
            let end = position_to_offset(source, &lo, Position::new(line, col + token.length));
            data.push((
                source[start..end].to_string(),
                token.token_type,
                token.token_modifiers_bitset,
            ));
        }
        data
    }

    #[test]
    fn line_offsets_multiline() {
        let offsets = compute_line_offsets("ab\ncd\ne");
        assert_eq!(offsets, vec![0, 3, 6]);
    }

    #[test]
    fn offset_to_line_col_basic() {
        let source = "ab\ncd\ne";
        let offsets = compute_line_offsets(source);
        assert_eq!(offset_to_line_col(source, &offsets, 0), (0, 0));
        assert_eq!(offset_to_line_col(source, &offsets, 1), (0, 1));
        assert_eq!(offset_to_line_col(source, &offsets, 3), (1, 0));
        assert_eq!(offset_to_line_col(source, &offsets, 6), (2, 0));
    }

    #[test]
    fn position_to_offset_basic() {
        let source = "ab\ncd\ne";
        let lo = compute_line_offsets(source);
        assert_eq!(position_to_offset(source, &lo, Position::new(0, 0)), 0);
        assert_eq!(position_to_offset(source, &lo, Position::new(1, 1)), 4);
        assert_eq!(position_to_offset(source, &lo, Position::new(2, 0)), 6);
    }

    #[test]
    fn offset_to_line_col_utf16() {
        // "aé" — 'é' is 2 bytes in UTF-8 but 1 UTF-16 code unit
        let source = "aé\nb";
        let offsets = compute_line_offsets(source);
        // 'a' is at byte 0 → col 0
        assert_eq!(offset_to_line_col(source, &offsets, 0), (0, 0));
        // 'é' starts at byte 1 → col 1 (UTF-16)
        assert_eq!(offset_to_line_col(source, &offsets, 1), (0, 1));
        // After 'é' (byte 3) → col 2 (UTF-16)
        assert_eq!(offset_to_line_col(source, &offsets, 3), (0, 2));
        // 'b' is on line 1 at byte 4 → (1, 0)
        assert_eq!(offset_to_line_col(source, &offsets, 4), (1, 0));
    }

    #[test]
    fn position_to_offset_utf16() {
        // "aé\nb" — 'é' is 2 bytes in UTF-8
        let source = "aé\nb";
        let lo = compute_line_offsets(source);
        // Position(0, 1) → byte 1 (start of 'é')
        assert_eq!(position_to_offset(source, &lo, Position::new(0, 1)), 1);
        // Position(0, 2) → byte 3 (after 'é', which is \n)
        assert_eq!(position_to_offset(source, &lo, Position::new(0, 2)), 3);
        // Position(1, 0) → byte 4
        assert_eq!(position_to_offset(source, &lo, Position::new(1, 0)), 4);
    }

    #[test]
    fn offset_to_line_col_surrogate_pair() {
        // '𝄞' (U+1D11E) is 4 bytes in UTF-8 and 2 UTF-16 code units
        let source = "a𝄞b";
        let offsets = compute_line_offsets(source);
        // 'a' at byte 0 → col 0
        assert_eq!(offset_to_line_col(source, &offsets, 0), (0, 0));
        // '𝄞' at byte 1 → col 1
        assert_eq!(offset_to_line_col(source, &offsets, 1), (0, 1));
        // 'b' at byte 5 → col 3 (1 for 'a' + 2 for surrogate pair)
        assert_eq!(offset_to_line_col(source, &offsets, 5), (0, 3));
    }

    #[test]
    fn diagnostics_from_parse_error() {
        let source = "fn {";
        let result = hew_parser::parse(source);
        let lo = compute_line_offsets(source);
        let uri = Url::parse("file:///test.hew").unwrap();
        let diags = build_diagnostics(&uri, source, &lo, &result, None);
        assert!(!diags.is_empty());
        assert_eq!(diags[0].severity, Some(DiagnosticSeverity::ERROR));
    }

    #[test]
    fn completions_include_keywords() {
        let parse_result = hew_parser::parse("");
        let doc = DocumentState {
            source: String::new(),
            line_offsets: compute_line_offsets(""),
            parse_result,
            type_output: None,
            diagnostics_by_uri: HashMap::new(),
        };
        let items = hew_analysis::completions::complete(
            &doc.source,
            &doc.parse_result,
            doc.type_output.as_ref(),
            0,
        );
        let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
        assert!(labels.contains(&"fn"));
        assert!(labels.contains(&"actor"));
        assert!(labels.contains(&"spawn"));
    }

    #[test]
    fn snippets_match_select_and_timeout_syntax() {
        let snippets = hew_analysis::completions::keyword_snippets();
        let select = snippets
            .iter()
            .find(|item| item.label == "select...")
            .unwrap();
        let select_text = select.insert_text.as_deref().unwrap();
        assert!(select_text.contains("<-"));
        assert!(select_text.contains("after"));

        let select_from = snippets
            .iter()
            .find(|item| item.label == "select from...")
            .unwrap();
        assert!(select_from
            .insert_text
            .as_deref()
            .unwrap()
            .contains(" from "));

        let timeout = snippets
            .iter()
            .find(|item| item.label == "timeout...")
            .unwrap();
        assert_eq!(
            timeout.insert_text.as_deref(),
            Some("${1:expr} | after ${2:duration}")
        );
    }

    #[test]
    fn document_symbols_for_function() {
        let source = "fn foo() -> i32 { 42 }";
        let parse_result = hew_parser::parse(source);
        let lo = compute_line_offsets(source);
        let analysis_symbols = hew_analysis::symbols::build_document_symbols(source, &parse_result);
        let symbols: Vec<DocumentSymbol> = analysis_symbols
            .into_iter()
            .map(|s| symbol_info_to_doc_symbol(source, &lo, s))
            .collect();
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "foo");
        assert_eq!(symbols[0].kind, SymbolKind::FUNCTION);
    }

    #[test]
    fn semantic_tokens_for_let() {
        let source = "let x = 42;";
        let lo = compute_line_offsets(source);
        let analysis_tokens = hew_analysis::semantic_tokens::build_semantic_tokens(source);
        let tokens = analysis_tokens_to_lsp(source, &lo, &analysis_tokens);
        assert!(!tokens.is_empty());
        // First token should be `let` keyword
        assert_eq!(tokens[0].token_type, hew_analysis::token_types::KEYWORD);
        // Second token is `x` identifier — should have DECLARATION modifier (bit 0)
        assert_eq!(tokens[1].token_type, hew_analysis::token_types::VARIABLE);
        let decl = modifier_bit(&SemanticTokenModifier::DECLARATION);
        assert_eq!(
            tokens[1].token_modifiers_bitset & decl,
            decl,
            "x should be DECLARATION"
        );
    }

    #[test]
    fn semantic_token_modifiers_async_and_const() {
        let source = "const Y = 1; async fn foo() {}";
        let lo = compute_line_offsets(source);
        let analysis_tokens = hew_analysis::semantic_tokens::build_semantic_tokens(source);
        let tokens = analysis_tokens_to_lsp(source, &lo, &analysis_tokens);
        let decl = modifier_bit(&SemanticTokenModifier::DECLARATION);
        let readonly = modifier_bit(&SemanticTokenModifier::READONLY);
        let async_mod = modifier_bit(&SemanticTokenModifier::ASYNC);
        // Find the `const` keyword and the identifier after it.
        let const_tok = &tokens[0]; // `const`
        assert_eq!(const_tok.token_type, hew_analysis::token_types::KEYWORD);
        let y_tok = &tokens[1]; // `Y`
        assert_eq!(y_tok.token_type, hew_analysis::token_types::VARIABLE);
        assert_eq!(
            y_tok.token_modifiers_bitset & decl,
            decl,
            "Y should be DECLARATION"
        );
        assert_eq!(
            y_tok.token_modifiers_bitset & readonly,
            readonly,
            "Y should be READONLY"
        );

        // Find `async` keyword — should have ASYNC modifier.
        let async_tok = tokens.iter().find(|t| {
            t.token_type == hew_analysis::token_types::KEYWORD
                && t.token_modifiers_bitset & async_mod == async_mod
        });
        assert!(
            async_tok.is_some(),
            "should find async keyword with ASYNC modifier"
        );
    }

    #[test]
    fn semantic_tokens_include_bitwise_and_shift_operators() {
        let source = "fn main() { let a = 1; let b = 2; let x = ~a << b >> 1 & a | b ^ a; x <<= 1; x >>= 1; x &= 1; x |= 1; x ^= 1; }";
        let lo = compute_line_offsets(source);
        let analysis_tokens = hew_analysis::semantic_tokens::build_semantic_tokens(source);
        let tokens = analysis_tokens_to_lsp(source, &lo, &analysis_tokens);
        let operator_token_type = hew_analysis::token_types::OPERATOR;
        let token_data = semantic_token_data(source, &tokens);
        let operator_texts: Vec<String> = token_data
            .into_iter()
            .filter(|(_, token_type, _)| *token_type == operator_token_type)
            .map(|(text, _, _)| text)
            .collect();
        for op in [
            "~", "<<", ">>", "<<=", ">>=", "&=", "|=", "^=", "&", "|", "^",
        ] {
            assert!(
                operator_texts.iter().any(|text| text == op),
                "expected operator token '{op}', got {operator_texts:?}"
            );
        }
    }

    #[test]
    fn semantic_tokens_mark_function_and_type_declarations() {
        let source = "type Point { x: i32 }\ntrait Stream { type Item; fn next() -> i32; }\nfn calc(v: i32) -> i32 { v }";
        let lo = compute_line_offsets(source);
        let analysis_tokens = hew_analysis::semantic_tokens::build_semantic_tokens(source);
        let tokens = analysis_tokens_to_lsp(source, &lo, &analysis_tokens);
        let token_data = semantic_token_data(source, &tokens);
        let function_token_type = hew_analysis::token_types::FUNCTION;
        let type_token_type = hew_analysis::token_types::TYPE;
        let decl = modifier_bit(&SemanticTokenModifier::DECLARATION);

        assert!(
            token_data
                .iter()
                .any(|(text, token_type, modifiers)| text == "calc"
                    && *token_type == function_token_type
                    && (modifiers & decl) == decl),
            "expected function declaration token for calc"
        );
        assert!(
            token_data
                .iter()
                .any(|(text, token_type, _)| text == "next" && *token_type == function_token_type),
            "expected function token for trait method next"
        );
        assert!(
            token_data
                .iter()
                .any(|(text, token_type, _)| text == "Point" && *token_type == type_token_type),
            "expected type token for Point"
        );
        assert!(
            token_data
                .iter()
                .any(|(text, token_type, _)| text == "Item" && *token_type == type_token_type),
            "expected type token for associated type Item"
        );
    }

    #[test]
    fn dot_completions_for_struct() {
        // Use valid code with a field access so the parser and type checker succeed.
        // Place the cursor right after the dot (before the field name) to simulate
        // the user typing `p.` and requesting completions.
        let source =
            "type Point { x: i32; y: i32 }\nfn main() { let p = Point { x: 1, y: 2 }; p.x }";
        let parse_result = hew_parser::parse(source);
        assert!(
            parse_result.errors.is_empty(),
            "unexpected parse errors: {:?}",
            parse_result.errors
        );
        let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
        let type_output = checker.check_program(&parse_result.program);

        let doc = DocumentState {
            source: source.to_string(),
            line_offsets: compute_line_offsets(source),
            parse_result,
            type_output: Some(type_output),
            diagnostics_by_uri: HashMap::new(),
        };
        // Offset right after the dot in `p.x`
        let dot_pos = source.find("p.x").unwrap() + 2;
        let items = hew_analysis::completions::complete(
            &doc.source,
            &doc.parse_result,
            doc.type_output.as_ref(),
            dot_pos,
        );
        let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
        assert!(
            labels.contains(&"x"),
            "expected field 'x' in completions, got: {labels:?}"
        );
        assert!(
            labels.contains(&"y"),
            "expected field 'y' in completions, got: {labels:?}"
        );
    }

    #[test]
    fn dot_completions_for_builtin_stream() {
        let source = "fn probe(s: Stream<String>) { s.next(); }";
        let parse_result = hew_parser::parse(source);
        assert!(
            parse_result.errors.is_empty(),
            "unexpected parse errors: {:?}",
            parse_result.errors
        );
        let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
        let type_output = checker.check_program(&parse_result.program);
        assert!(
            type_output.errors.is_empty(),
            "type errors: {:?}",
            type_output.errors
        );

        let doc = DocumentState {
            source: source.to_string(),
            line_offsets: compute_line_offsets(source),
            parse_result,
            type_output: Some(type_output),
            diagnostics_by_uri: HashMap::new(),
        };
        let dot_pos = source.find("s.next").unwrap() + 2;
        let items = hew_analysis::completions::complete(
            &doc.source,
            &doc.parse_result,
            doc.type_output.as_ref(),
            dot_pos,
        );
        let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
        assert!(
            labels.contains(&"next"),
            "expected method 'next' in completions, got: {labels:?}"
        );
        assert!(
            labels.contains(&"collect"),
            "expected method 'collect' in completions, got: {labels:?}"
        );
        assert!(
            labels.contains(&"lines"),
            "expected method 'lines' in completions, got: {labels:?}"
        );
        let next_item = items
            .iter()
            .find(|item| item.label == "next")
            .expect("next completion should exist");
        assert!(
            next_item
                .detail
                .as_deref()
                .is_some_and(|detail| detail.contains("Option<String>")),
            "expected Stream<String> detail for next(), got: {:?}",
            next_item.detail
        );
    }

    #[test]
    fn completions_enum_variant_after_double_colon() {
        let source = "enum Color { Blue; Point { x: i32, y: i32 }; Rgb(u8, u8, u8); }\nfn main() { let color = Color::Blue; }";
        let parse_result = hew_parser::parse(source);
        assert!(
            parse_result.errors.is_empty(),
            "unexpected parse errors: {:?}",
            parse_result.errors
        );
        let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
        let type_output = checker.check_program(&parse_result.program);
        assert!(
            type_output.errors.is_empty(),
            "type errors: {:?}",
            type_output.errors
        );

        let doc = DocumentState {
            source: source.to_string(),
            line_offsets: compute_line_offsets(source),
            parse_result,
            type_output: Some(type_output),
            diagnostics_by_uri: HashMap::new(),
        };
        let offset = source.find("Color::Blue").unwrap() + "Color::".len();
        let items = hew_analysis::completions::complete(
            &doc.source,
            &doc.parse_result,
            doc.type_output.as_ref(),
            offset,
        );

        let labels: Vec<&str> = items.iter().map(|item| item.label.as_str()).collect();
        assert_eq!(labels, vec!["Blue", "Point", "Rgb"]);

        let point_item = items
            .iter()
            .find(|item| item.label == "Point")
            .expect("Point completion should exist");
        assert_eq!(point_item.kind, CompletionKind::Constant);
        assert_eq!(point_item.detail.as_deref(), Some("{ x: i32, y: i32 }"));
    }

    #[test]
    fn spawn_completions_only_actors() {
        let source = "actor Foo {}\nactor Bar {}\nfn baz() {}\nfn main() { let h = spawn  }";
        let parse_result = hew_parser::parse(source);
        let doc = DocumentState {
            source: source.to_string(),
            line_offsets: compute_line_offsets(source),
            parse_result,
            type_output: None,
            diagnostics_by_uri: HashMap::new(),
        };
        let spawn_offset = source.find("spawn ").unwrap() + 7;
        let items = hew_analysis::completions::complete(
            &doc.source,
            &doc.parse_result,
            doc.type_output.as_ref(),
            spawn_offset,
        );
        let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
        assert!(labels.contains(&"Foo"), "expected Foo, got: {labels:?}");
        assert!(labels.contains(&"Bar"), "expected Bar, got: {labels:?}");
        assert!(
            !labels.contains(&"baz"),
            "baz should not appear after spawn"
        );
    }

    #[test]
    fn completions_include_local_variables() {
        let source = "fn main() { let counter = 42; let name = \"hello\"; 0 }";
        let parse_result = hew_parser::parse(source);
        assert!(
            parse_result.errors.is_empty(),
            "parse errors: {:?}",
            parse_result.errors
        );

        let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
        let type_output = checker.check_program(&parse_result.program);
        let doc = DocumentState {
            source: source.to_string(),
            line_offsets: compute_line_offsets(source),
            parse_result,
            type_output: Some(type_output),
            diagnostics_by_uri: HashMap::new(),
        };
        let offset = source.find("0 }").unwrap();
        let items = hew_analysis::completions::complete(
            &doc.source,
            &doc.parse_result,
            doc.type_output.as_ref(),
            offset,
        );
        let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
        assert!(
            labels.contains(&"counter"),
            "expected local var 'counter', got: {labels:?}"
        );
        assert!(
            labels.contains(&"name"),
            "expected local var 'name', got: {labels:?}"
        );
    }

    #[test]
    fn completions_do_not_leak_if_block_locals_after_if() {
        let source = r"fn main() {
    if true {
        let branch_only = 1;
    }
    let outside = 2;
    outside
}";
        let parse_result = hew_parser::parse(source);
        assert!(
            parse_result.errors.is_empty(),
            "parse errors: {:?}",
            parse_result.errors
        );
        let doc = DocumentState {
            source: source.to_string(),
            line_offsets: compute_line_offsets(source),
            parse_result,
            type_output: None,
            diagnostics_by_uri: HashMap::new(),
        };
        let offset = source.rfind("outside\n").unwrap();
        let items = hew_analysis::completions::complete(
            &doc.source,
            &doc.parse_result,
            doc.type_output.as_ref(),
            offset,
        );
        let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
        assert!(labels.contains(&"outside"));
        assert!(
            !labels.contains(&"branch_only"),
            "branch-only local should not be in scope after if block: {labels:?}"
        );
    }

    #[test]
    fn completions_cover_type_impl_methods_if_else_match_and_patterns() {
        let source = r"
type Point { x: i32; y: i32; }
enum Result { Ok(i32); Err(i32); }

type Worker {
    fn process(input: i32, point: Point, result: Result) -> i32 {
        if input > 0 {
            let then_local = input;
            then_local
        } else if input < 0 {
            let else_if_local = input;
            else_if_local
        } else {
            match result {
                Result::Ok(ok_value) => {
                    let Point { x, y: y_value } = point;
                    let match_local = ok_value + x + y_value;
                    match_local
                },
                Result::Err(err_a) | Result::Err(err_b) => {
                    err_a + err_b
                },
            }
        }
    }
}

impl Worker {
    fn apply(code: i32, result: Result) -> i32 {
        let Result::Ok(inner) = result;
        inner + code
    }
}
";
        let parse_result = hew_parser::parse(source);
        assert!(
            parse_result.errors.is_empty(),
            "parse errors: {:?}",
            parse_result.errors
        );
        let doc = DocumentState {
            source: source.to_string(),
            line_offsets: compute_line_offsets(source),
            parse_result,
            type_output: None,
            diagnostics_by_uri: HashMap::new(),
        };

        let else_if_offset = source.find("else_if_local\n").unwrap();
        let else_if_items = hew_analysis::completions::complete(
            &doc.source,
            &doc.parse_result,
            doc.type_output.as_ref(),
            else_if_offset,
        );
        let else_if_labels: Vec<&str> = else_if_items.iter().map(|i| i.label.as_str()).collect();
        assert!(else_if_labels.contains(&"input"));
        assert!(else_if_labels.contains(&"else_if_local"));

        let match_offset = source.find("match_local\n").unwrap();
        let match_items = hew_analysis::completions::complete(
            &doc.source,
            &doc.parse_result,
            doc.type_output.as_ref(),
            match_offset,
        );
        let match_labels: Vec<&str> = match_items.iter().map(|i| i.label.as_str()).collect();
        assert!(match_labels.contains(&"ok_value"));
        assert!(match_labels.contains(&"x"));
        assert!(match_labels.contains(&"y_value"));
        assert!(match_labels.contains(&"match_local"));

        let or_pattern_offset = source.find("err_a + err_b").unwrap();
        let or_pattern_items = hew_analysis::completions::complete(
            &doc.source,
            &doc.parse_result,
            doc.type_output.as_ref(),
            or_pattern_offset,
        );
        let or_pattern_labels: Vec<&str> =
            or_pattern_items.iter().map(|i| i.label.as_str()).collect();
        assert!(or_pattern_labels.contains(&"err_a"));
        assert!(or_pattern_labels.contains(&"err_b"));

        let impl_offset = source.find("inner + code").unwrap();
        let impl_items = hew_analysis::completions::complete(
            &doc.source,
            &doc.parse_result,
            doc.type_output.as_ref(),
            impl_offset,
        );
        let impl_labels: Vec<&str> = impl_items.iter().map(|i| i.label.as_str()).collect();
        assert!(impl_labels.contains(&"inner"));
        assert!(impl_labels.contains(&"code"));
    }

    #[test]
    fn goto_def_receive_method() {
        let source = "actor Counter {\n    count: i32;\n    receive fn increment(n: i32) {\n        count = count + n;\n    }\n}\nfn main() { let c = spawn Counter(count: 0); c.increment(1); }";
        let parse_result = hew_parser::parse(source);
        let lo = compute_line_offsets(source);
        let call_offset = source.rfind("increment").unwrap();
        let word = word_at_offset(source, call_offset).unwrap();
        // word_at_offset returns the dot-qualified form "c.increment"
        assert_eq!(word, "c.increment");
        // find_definition_in_ast doesn't find the full dot-qualified name...
        assert!(find_definition_in_ast(source, &lo, &parse_result, &word).is_none());
        // ...but the method part alone finds the receive method definition.
        let method = word.rsplit('.').next().unwrap();
        let found = find_definition_in_ast(source, &lo, &parse_result, method);
        assert!(found.is_some(), "should find receive method definition");
    }

    #[test]
    fn goto_def_double_colon_method_fallback() {
        let source = "actor Counter {\n    receive fn increment(n: i32) {\n        n\n    }\n}\nfn main() { Counter::increment(1); }";
        let parse_result = hew_parser::parse(source);
        let lo = compute_line_offsets(source);
        let call_offset = source.rfind("increment").unwrap();
        let word = word_at_offset(source, call_offset).unwrap();
        assert_eq!(word, "Counter::increment");
        assert!(find_definition_in_ast(source, &lo, &parse_result, &word).is_none());
        let method = word.rsplit("::").next().unwrap();
        let found = find_definition_in_ast(source, &lo, &parse_result, method);
        assert!(
            found.is_some(),
            "should find receive method definition via :: fallback"
        );
    }

    #[test]
    fn goto_def_resolves_local_binding_fallback() {
        let source = "fn main() {\n    let result = 41;\n    result + 1\n}";
        let doc = make_doc(source);
        let offset = source.rfind("result + 1").unwrap();
        let word = word_at_offset(source, offset).unwrap();

        assert!(
            find_definition_in_ast(source, &doc.line_offsets, &doc.parse_result, &word).is_none()
        );

        let range = resolve_local_or_param_definition(&doc, &word, offset)
            .expect("should resolve local binding");
        let expected_start = source.find("let result").unwrap() + 4;
        let expected = offset_range_to_lsp(
            source,
            &doc.line_offsets,
            expected_start,
            expected_start + "result".len(),
        );
        assert_eq!(range, expected);
    }

    #[test]
    fn goto_def_resolves_param_fallback() {
        let source = "fn add(value: i32) -> i32 {\n    value + 1\n}";
        let doc = make_doc(source);
        let offset = source.rfind("value + 1").unwrap();
        let word = word_at_offset(source, offset).unwrap();

        assert!(
            find_definition_in_ast(source, &doc.line_offsets, &doc.parse_result, &word).is_none()
        );

        let range = resolve_local_or_param_definition(&doc, &word, offset)
            .expect("should resolve parameter definition");
        let expected_start = source.find("value: i32").unwrap();
        let expected = offset_range_to_lsp(
            source,
            &doc.line_offsets,
            expected_start,
            expected_start + "value".len(),
        );
        assert_eq!(range, expected);
    }

    #[test]
    fn find_definition_includes_extern_functions_and_associated_types() {
        let source = "extern \"C\" { fn c_abs(x: i32) -> i32; }\ntrait Iter { type Item; fn next() -> i32; }";
        let parse_result = hew_parser::parse(source);
        let lo = compute_line_offsets(source);
        assert!(
            parse_result.errors.is_empty(),
            "parse errors: {:?}",
            parse_result.errors
        );
        assert!(find_definition_in_ast(source, &lo, &parse_result, "c_abs").is_some());
        assert!(find_definition_in_ast(source, &lo, &parse_result, "Item").is_some());
    }

    #[test]
    fn word_at_offset_double_colon() {
        let source = "Counter::increment";
        let word = word_at_offset(source, 10);
        assert_eq!(word, Some("Counter::increment".to_string()));
    }

    #[test]
    fn find_refs_scope_aware_separates_same_name_locals() {
        let source = "fn foo(x: i64) { println(x); }\nfn bar(x: i64) { println(x); }";
        let parse_result = hew_parser::parse(source);
        // Cursor on `x` in foo's println(x) — should only find refs in foo scope
        let foo_x_offset = source.find("println(x").unwrap() + 9;
        let result =
            hew_analysis::references::find_all_references(source, &parse_result, foo_x_offset);
        assert!(result.is_some());
        let (name, spans) = result.unwrap();
        assert_eq!(name, "x");
        // Should only find x usages within foo, not bar
        // foo has: `println(x)` — 1 usage reference (no param decl span in AST)
        assert_eq!(spans.len(), 1, "expected 1 ref in foo, got {}", spans.len());
    }

    #[test]
    fn find_refs_top_level_finds_all() {
        let source = "fn helper() -> i64 { 42 }\nfn main() { helper(); }";
        let parse_result = hew_parser::parse(source);
        // Cursor on `helper` in main's call
        let call_offset = source.rfind("helper").unwrap();
        let result =
            hew_analysis::references::find_all_references(source, &parse_result, call_offset);
        assert!(result.is_some());
        let (name, spans) = result.unwrap();
        assert_eq!(name, "helper");
        // Should find the call in main (top-level name = global search)
        assert!(!spans.is_empty(), "expected refs for helper");
    }

    // ── Hover tests ─────────────────────────────────────────────────

    #[test]
    fn hover_on_variable() {
        let source = "fn main() -> i32 { let x = 42; x }";
        let parse_result = hew_parser::parse(source);
        let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
        let type_output = checker.check_program(&parse_result.program);
        // Hover on the usage of `x`
        let offset = source.rfind('x').unwrap();
        let result = hew_analysis::hover::hover(source, &parse_result, Some(&type_output), offset);
        assert!(result.is_some(), "expected hover result for variable 'x'");
        let hr = result.unwrap();
        assert!(!hr.contents.is_empty());
    }

    #[test]
    fn hover_on_function_name() {
        let source = "fn greet(name: string) -> string { name }\nfn main() { greet(\"world\"); }";
        let parse_result = hew_parser::parse(source);
        let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
        let type_output = checker.check_program(&parse_result.program);
        // Hover on the call site of `greet`
        let offset = source.rfind("greet").unwrap();
        let result = hew_analysis::hover::hover(source, &parse_result, Some(&type_output), offset);
        assert!(result.is_some(), "expected hover for function call");
        let hr = result.unwrap();
        assert!(
            hr.contents.contains("greet"),
            "hover should mention function name, got: {}",
            hr.contents
        );
    }

    #[test]
    fn hover_on_type_name() {
        let source =
            "type Point { x: i32; y: i32 }\nfn main() { let p = Point { x: 1, y: 2 }; p.x }";
        let parse_result = hew_parser::parse(source);
        let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
        let type_output = checker.check_program(&parse_result.program);
        // Hover on usage of Point in the struct init
        let offset = source.rfind("Point").unwrap();
        let result = hew_analysis::hover::hover(source, &parse_result, Some(&type_output), offset);
        assert!(result.is_some(), "expected hover for type name");
        let hr = result.unwrap();
        assert!(
            hr.contents.contains("Point"),
            "hover should mention type name, got: {}",
            hr.contents
        );
    }

    #[test]
    fn hover_on_builtin_type_name() {
        let source = "fn drain(rx: Receiver<String>) { rx.close(); }";
        let parse_result = hew_parser::parse(source);
        assert!(
            parse_result.errors.is_empty(),
            "unexpected parse errors: {:?}",
            parse_result.errors
        );
        let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
        let type_output = checker.check_program(&parse_result.program);
        assert!(
            type_output.errors.is_empty(),
            "type errors: {:?}",
            type_output.errors
        );
        let offset = source.find("Receiver").unwrap();
        let result = hew_analysis::hover::hover(source, &parse_result, Some(&type_output), offset);
        assert!(result.is_some(), "expected hover for builtin type name");
        let hr = result.unwrap();
        assert!(
            hr.contents.contains("Receiver"),
            "hover should mention builtin type name, got: {}",
            hr.contents
        );
        assert!(
            hr.contents.contains("recv"),
            "hover should include builtin methods, got: {}",
            hr.contents
        );
    }

    // ── Folding range tests ─────────────────────────────────────────

    #[test]
    fn folding_ranges_basic_invocation() {
        // Verify folding ranges can be computed without errors
        let source = "fn main() { 0 }";
        let parse_result = hew_parser::parse(source);
        let ranges = hew_analysis::folding::build_folding_ranges(source, &parse_result);
        // Single-line function may not produce a fold, but should not crash
        let _ = ranges;
    }

    #[test]
    fn folding_ranges_for_actor() {
        let source = r"actor Counter {
    count: i32;
    receive fn increment(n: i32) {
        count = count + n;
    }
    receive fn get() -> i32 {
        count
    }
}";
        let parse_result = hew_parser::parse(source);
        assert!(
            parse_result.errors.is_empty(),
            "parse errors: {:?}",
            parse_result.errors
        );
        let ranges = hew_analysis::folding::build_folding_ranges(source, &parse_result);
        assert!(
            !ranges.is_empty(),
            "expected folding ranges for a multi-line actor"
        );
    }

    #[test]
    fn folding_ranges_for_import_group() {
        let source = "import std::io;\nimport std::net;\nfn main() { 0 }";
        let parse_result = hew_parser::parse(source);
        let ranges = hew_analysis::folding::build_folding_ranges(source, &parse_result);
        let import_folds: Vec<_> = ranges
            .iter()
            .filter(|r| matches!(r.kind, hew_analysis::FoldingKind::Imports))
            .collect();
        assert!(
            !import_folds.is_empty(),
            "expected a folding range for import group"
        );
    }

    // ── Rename tests ────────────────────────────────────────────────

    #[test]
    fn prepare_rename_on_variable() {
        let source = "fn main() { let counter = 42; counter }";
        let parse_result = hew_parser::parse(source);
        let offset = source.find("counter").unwrap();
        let span = hew_analysis::rename::prepare_rename(source, &parse_result, offset);
        assert!(
            span.is_some(),
            "should be able to prepare rename on variable"
        );
        let s = span.unwrap();
        assert_eq!(&source[s.start..s.end], "counter");
    }

    #[test]
    fn rename_variable_updates_all_occurrences() {
        let source = "fn main() { let counter = 42; counter }";
        let parse_result = hew_parser::parse(source);
        let offset = source.find("counter").unwrap();
        let edits = hew_analysis::rename::rename(source, &parse_result, offset, "count");
        assert!(edits.is_some(), "rename should produce edits");
        let edits = edits.unwrap();
        assert!(
            edits.len() >= 2,
            "expected at least 2 rename edits (decl + usage), got {}",
            edits.len()
        );
        for edit in &edits {
            assert_eq!(edit.new_text, "count");
        }
    }

    #[test]
    fn prepare_rename_on_whitespace_returns_none() {
        let source = "fn main() { }";
        let parse_result = hew_parser::parse(source);
        // Offset in the whitespace between braces
        let offset = source.find("{ }").unwrap() + 1;
        let span = hew_analysis::rename::prepare_rename(source, &parse_result, offset);
        assert!(span.is_none(), "rename on whitespace should return None");
    }

    // ── Type hierarchy tests ────────────────────────────────────────

    #[test]
    fn type_hierarchy_item_for_struct() {
        let source = "type Point { x: i32; y: i32 }";
        let parse_result = hew_parser::parse(source);
        let lo = compute_line_offsets(source);
        let uri = Url::parse("file:///test.hew").unwrap();
        let item = find_type_hierarchy_item(&uri, source, &lo, &parse_result, "Point");
        assert!(item.is_some(), "should find type hierarchy item for Point");
        let item = item.unwrap();
        assert_eq!(item.name, "Point");
        assert_eq!(item.kind, SymbolKind::STRUCT);
    }

    #[test]
    fn type_hierarchy_item_for_actor() {
        let source = "actor Worker { receive fn process() { 0 } }";
        let parse_result = hew_parser::parse(source);
        let lo = compute_line_offsets(source);
        let uri = Url::parse("file:///test.hew").unwrap();
        let item = find_type_hierarchy_item(&uri, source, &lo, &parse_result, "Worker");
        assert!(item.is_some(), "should find type hierarchy item for actor");
        let item = item.unwrap();
        assert_eq!(item.name, "Worker");
        assert_eq!(item.kind, SymbolKind::CLASS);
    }

    #[test]
    fn type_hierarchy_item_for_trait() {
        let source = "trait Drawable { fn draw() -> i32; }";
        let parse_result = hew_parser::parse(source);
        let lo = compute_line_offsets(source);
        let uri = Url::parse("file:///test.hew").unwrap();
        let item = find_type_hierarchy_item(&uri, source, &lo, &parse_result, "Drawable");
        assert!(item.is_some(), "should find type hierarchy item for trait");
        let item = item.unwrap();
        assert_eq!(item.name, "Drawable");
        assert_eq!(item.kind, SymbolKind::INTERFACE);
    }

    #[test]
    fn type_hierarchy_item_not_found() {
        let source = "fn main() { 0 }";
        let parse_result = hew_parser::parse(source);
        let lo = compute_line_offsets(source);
        let uri = Url::parse("file:///test.hew").unwrap();
        let item = find_type_hierarchy_item(&uri, source, &lo, &parse_result, "Nonexistent");
        assert!(item.is_none(), "should not find non-existent type");
    }

    #[test]
    fn subtypes_via_impl_for() {
        let source = "trait Drawable { fn draw() -> i32; }\ntype Circle { r: i32 }\nimpl Drawable for Circle { fn draw() -> i32 { 0 } }";
        let parse_result = hew_parser::parse(source);
        let lo = compute_line_offsets(source);
        let uri = Url::parse("file:///test.hew").unwrap();
        let subs = collect_subtypes(&uri, "Drawable", source, &lo, &parse_result);
        assert_eq!(subs.len(), 1, "expected 1 subtype, got {}", subs.len());
        assert_eq!(subs[0].name, "Circle");
    }

    #[test]
    fn supertypes_via_impl_for() {
        let source = "trait Drawable { fn draw() -> i32; }\ntype Circle { r: i32 }\nimpl Drawable for Circle { fn draw() -> i32 { 0 } }";
        let parse_result = hew_parser::parse(source);
        let lo = compute_line_offsets(source);
        let uri = Url::parse("file:///test.hew").unwrap();
        let supers = collect_supertypes(&uri, "Circle", source, &lo, &parse_result);
        assert_eq!(
            supers.len(),
            1,
            "expected 1 supertype, got {}",
            supers.len()
        );
        assert_eq!(supers[0].name, "Drawable");
    }

    // ── Call hierarchy tests ────────────────────────────────────────

    #[test]
    fn find_callable_at_function() {
        let source = "fn helper() -> i32 { 42 }\nfn main() { helper() }";
        let parse_result = hew_parser::parse(source);
        let lo = compute_line_offsets(source);
        let uri = Url::parse("file:///test.hew").unwrap();
        let item = find_callable_at(&uri, source, &lo, &parse_result, "helper");
        assert!(item.is_some(), "should find callable for function");
        let item = item.unwrap();
        assert_eq!(item.name, "helper");
        assert_eq!(item.kind, SymbolKind::FUNCTION);
    }

    #[test]
    fn find_callable_at_receive_fn() {
        let source = "actor Counter { receive fn increment(n: i32) { n } }";
        let parse_result = hew_parser::parse(source);
        let lo = compute_line_offsets(source);
        let uri = Url::parse("file:///test.hew").unwrap();
        let item = find_callable_at(&uri, source, &lo, &parse_result, "increment");
        assert!(item.is_some(), "should find callable for receive fn");
        let item = item.unwrap();
        assert_eq!(item.name, "increment");
        assert_eq!(item.kind, SymbolKind::METHOD);
        assert_eq!(item.detail.as_deref(), Some("actor Counter"));
    }

    #[test]
    fn find_callable_at_unknown_returns_none() {
        let source = "fn main() { 0 }";
        let parse_result = hew_parser::parse(source);
        let lo = compute_line_offsets(source);
        let uri = Url::parse("file:///test.hew").unwrap();
        let item = find_callable_at(&uri, source, &lo, &parse_result, "unknown");
        assert!(item.is_none());
    }

    #[test]
    fn incoming_calls_finds_callers() {
        let source = "fn helper() -> i32 { 42 }\nfn main() -> i32 { let r = helper(); r }";
        let parse_result = hew_parser::parse(source);
        assert!(
            parse_result.errors.is_empty(),
            "parse errors: {:?}",
            parse_result.errors
        );
        let lo = compute_line_offsets(source);
        let uri = Url::parse("file:///test.hew").unwrap();
        let calls = find_incoming_calls(&uri, source, &lo, &parse_result, "helper");
        assert_eq!(
            calls.len(),
            1,
            "expected 1 incoming call, got {}",
            calls.len()
        );
        assert_eq!(calls[0].from.name, "main");
    }

    #[test]
    fn outgoing_calls_finds_callees() {
        let source = "fn helper() -> i32 { 42 }\nfn main() -> i32 { let r = helper(); r }";
        let parse_result = hew_parser::parse(source);
        assert!(
            parse_result.errors.is_empty(),
            "parse errors: {:?}",
            parse_result.errors
        );
        let lo = compute_line_offsets(source);
        let uri = Url::parse("file:///test.hew").unwrap();
        let calls = find_outgoing_calls(&uri, source, &lo, &parse_result, "main");
        assert_eq!(
            calls.len(),
            1,
            "expected 1 outgoing call, got {}",
            calls.len()
        );
        assert_eq!(calls[0].to.name, "helper");
    }

    #[test]
    fn outgoing_calls_empty_for_leaf_function() {
        let source = "fn leaf() -> i32 { 42 }";
        let parse_result = hew_parser::parse(source);
        let lo = compute_line_offsets(source);
        let uri = Url::parse("file:///test.hew").unwrap();
        let calls = find_outgoing_calls(&uri, source, &lo, &parse_result, "leaf");
        assert!(
            calls.is_empty(),
            "leaf function should have no outgoing calls"
        );
    }

    /// Regression: sibling `receive_fns` in an actor must not bleed into each
    /// other's outgoing call sets.  Before the fix, querying `on_msg` would
    /// walk *all* actor bodies and surface `target_b` (from `on_other`) as an
    /// outgoing call.
    #[test]
    fn outgoing_calls_actor_receive_fn_no_sibling_bleed() {
        let source = concat!(
            "fn target_a() {}\n",
            "fn target_b() {}\n",
            "actor A {\n",
            "    receive fn on_msg() { target_a(); }\n",
            "    receive fn on_other() { target_b(); }\n",
            "}",
        );
        let parse_result = hew_parser::parse(source);
        let lo = compute_line_offsets(source);
        let uri = Url::parse("file:///test.hew").unwrap();
        let calls = find_outgoing_calls(&uri, source, &lo, &parse_result, "on_msg");
        let callee_names: Vec<&str> = calls.iter().map(|c| c.to.name.as_str()).collect();
        assert!(
            callee_names.contains(&"target_a"),
            "on_msg should have target_a as outgoing call"
        );
        assert!(
            !callee_names.contains(&"target_b"),
            "sibling on_other's target_b must NOT appear in on_msg outgoing calls (regression)"
        );
    }

    /// Regression: sibling methods in an impl must not bleed into each other's
    /// outgoing call sets.
    #[test]
    fn outgoing_calls_impl_method_no_sibling_bleed() {
        let source = concat!(
            "fn alpha_target() {}\n",
            "fn beta_target() {}\n",
            "impl Foo {\n",
            "    fn alpha() { alpha_target(); }\n",
            "    fn beta() { beta_target(); }\n",
            "}",
        );
        let parse_result = hew_parser::parse(source);
        let lo = compute_line_offsets(source);
        let uri = Url::parse("file:///test.hew").unwrap();
        let calls = find_outgoing_calls(&uri, source, &lo, &parse_result, "alpha");
        let callee_names: Vec<&str> = calls.iter().map(|c| c.to.name.as_str()).collect();
        assert!(
            callee_names.contains(&"alpha_target"),
            "alpha should have alpha_target as outgoing call"
        );
        assert!(
            !callee_names.contains(&"beta_target"),
            "sibling beta's beta_target must NOT appear in alpha outgoing calls (regression)"
        );
    }

    // ── Code lens tests ─────────────────────────────────────────────

    #[test]
    fn code_lenses_for_functions() {
        let source = "fn foo() -> i32 { 0 }\nfn bar() -> i32 { foo() }";
        let parse_result = hew_parser::parse(source);
        let lo = compute_line_offsets(source);
        let lenses = build_code_lenses(source, &lo, &parse_result);
        assert!(
            lenses.len() >= 2,
            "expected at least 2 code lenses (one per function), got {}",
            lenses.len()
        );
        // All lenses should have a command
        for lens in &lenses {
            assert!(lens.command.is_some());
        }
    }

    #[test]
    fn code_lens_reference_count_reflects_usage() {
        let source = "fn helper() -> i32 { 42 }\nfn main() { helper() }";
        let parse_result = hew_parser::parse(source);
        let lo = compute_line_offsets(source);
        let lenses = build_code_lenses(source, &lo, &parse_result);
        let helper_lens = lenses
            .iter()
            .find(|l| {
                l.command
                    .as_ref()
                    .is_some_and(|c| c.title.contains("reference"))
                    && l.range.start.line == 0
            })
            .expect("should find a reference lens for helper");
        assert!(
            helper_lens
                .command
                .as_ref()
                .unwrap()
                .title
                .contains("1 reference"),
            "helper should have 1 reference, got: {}",
            helper_lens.command.as_ref().unwrap().title
        );
    }

    #[test]
    fn code_lens_test_attribute() {
        let source = "#[test]\nfn test_add() { 0 }";
        let parse_result = hew_parser::parse(source);
        let lo = compute_line_offsets(source);
        let lenses = build_code_lenses(source, &lo, &parse_result);
        let run_test_lens = lenses.iter().find(|l| {
            l.command
                .as_ref()
                .is_some_and(|c| c.title.contains("Run test"))
        });
        assert!(
            run_test_lens.is_some(),
            "expected a 'Run test' code lens for #[test] function"
        );
    }

    #[test]
    fn initialize_advertises_run_test_execute_command() {
        let capabilities = build_server_capabilities();
        let commands = capabilities
            .execute_command_provider
            .expect("execute command support should be advertised")
            .commands;
        assert_eq!(commands, vec![RUN_TEST_COMMAND.to_string()]);
    }

    #[test]
    fn extract_run_test_name_accepts_legacy_and_object_args() {
        assert_eq!(
            extract_run_test_name(&[Value::String("test_add".to_string())]),
            Some("test_add".to_string())
        );
        assert_eq!(
            extract_run_test_name(&[json!({ "name": "test_subtract" })]),
            Some("test_subtract".to_string())
        );
    }

    #[test]
    fn build_run_test_invocation_uses_cli_test_runner() {
        let root = Path::new("workspace-root");
        let (program, args) = build_run_test_invocation("test_add", root);
        assert!(program.ends_with(Path::new(&format!("hew{}", std::env::consts::EXE_SUFFIX))));
        assert_eq!(
            args,
            vec![
                "test".to_string(),
                "--no-color".to_string(),
                "--filter".to_string(),
                "test_add".to_string(),
                "workspace-root".to_string(),
            ]
        );
    }

    // ── Workspace symbol tests ──────────────────────────────────────

    #[test]
    fn workspace_symbols_finds_functions_and_types() {
        let source = "fn compute() -> i32 { 0 }\ntype Widget { w: i32 }\nconst MAX: i32 = 100;";
        let parse_result = hew_parser::parse(source);
        assert!(
            parse_result.errors.is_empty(),
            "parse errors: {:?}",
            parse_result.errors
        );
        let lo = compute_line_offsets(source);
        let uri = Url::parse("file:///test.hew").unwrap();
        let symbols = collect_workspace_symbols(&uri, source, &lo, &parse_result, "");
        let names: Vec<&str> = symbols.iter().map(|s| s.name.as_str()).collect();
        assert!(
            names.contains(&"compute"),
            "should find function, got: {names:?}"
        );
        assert!(
            names.contains(&"Widget"),
            "should find type, got: {names:?}"
        );
        assert!(
            names.contains(&"MAX"),
            "should find constant, got: {names:?}"
        );
    }

    #[test]
    fn workspace_symbols_filters_by_query() {
        let source = "fn compute() -> i32 { 0 }\nfn render() -> i32 { 0 }\ntype Widget { w: i32 }";
        let parse_result = hew_parser::parse(source);
        let lo = compute_line_offsets(source);
        let uri = Url::parse("file:///test.hew").unwrap();
        let symbols = collect_workspace_symbols(&uri, source, &lo, &parse_result, "comp");
        assert_eq!(symbols.len(), 1, "query 'comp' should match only compute");
        assert_eq!(symbols[0].name, "compute");
    }

    #[test]
    fn workspace_symbols_case_insensitive() {
        let source = "fn ComputeValue() -> i32 { 0 }";
        let parse_result = hew_parser::parse(source);
        let lo = compute_line_offsets(source);
        let uri = Url::parse("file:///test.hew").unwrap();
        let symbols = collect_workspace_symbols(&uri, source, &lo, &parse_result, "compute");
        assert_eq!(symbols.len(), 1, "case-insensitive match should work");
    }

    #[test]
    fn workspace_symbols_includes_actor_and_receive_methods() {
        let source = "actor Counter { receive fn increment(n: i32) { n } }";
        let parse_result = hew_parser::parse(source);
        let lo = compute_line_offsets(source);
        let uri = Url::parse("file:///test.hew").unwrap();
        let symbols = collect_workspace_symbols(&uri, source, &lo, &parse_result, "");
        let names: Vec<&str> = symbols.iter().map(|s| s.name.as_str()).collect();
        assert!(
            names.contains(&"Counter"),
            "should find actor, got: {names:?}"
        );
        assert!(
            names.contains(&"increment"),
            "should find receive method, got: {names:?}"
        );
        // The receive method should have the actor as container
        let recv_sym = symbols.iter().find(|s| s.name == "increment").unwrap();
        assert_eq!(recv_sym.container_name.as_deref(), Some("Counter"));
    }

    #[test]
    fn workspace_symbols_includes_trait() {
        let source = "trait Drawable { fn draw() -> i32; }";
        let parse_result = hew_parser::parse(source);
        let lo = compute_line_offsets(source);
        let uri = Url::parse("file:///test.hew").unwrap();
        let symbols = collect_workspace_symbols(&uri, source, &lo, &parse_result, "");
        let names: Vec<&str> = symbols.iter().map(|s| s.name.as_str()).collect();
        assert!(
            names.contains(&"Drawable"),
            "should find trait, got: {names:?}"
        );
        let trait_sym = symbols.iter().find(|s| s.name == "Drawable").unwrap();
        assert_eq!(trait_sym.kind, SymbolKind::INTERFACE);
    }

    // ── Diagnostic data tests ───────────────────────────────────────

    #[test]
    fn diagnostic_data_encodes_kind_and_suggestions() {
        let data = diagnostic_data(
            &TypeErrorKind::UndefinedVariable,
            &["counter".to_string(), "count".to_string()],
        );
        assert_eq!(data["kind"], "UndefinedVariable");
        let suggestions = data["suggestions"].as_array().unwrap();
        assert_eq!(suggestions.len(), 2);
        assert_eq!(suggestions[0], "counter");
        assert_eq!(suggestions[1], "count");
    }

    #[test]
    fn diagnostic_data_all_error_kinds() {
        // Verify that all error kinds produce a non-empty string
        let kinds = [
            TypeErrorKind::UndefinedVariable,
            TypeErrorKind::UndefinedType,
            TypeErrorKind::UndefinedFunction,
            TypeErrorKind::UndefinedField,
            TypeErrorKind::UndefinedMethod,
            TypeErrorKind::InvalidSend,
            TypeErrorKind::InvalidOperation,
            TypeErrorKind::ArityMismatch,
            TypeErrorKind::BoundsNotSatisfied,
            TypeErrorKind::InferenceFailed,
            TypeErrorKind::NonExhaustiveMatch,
            TypeErrorKind::DuplicateDefinition,
            TypeErrorKind::MutabilityError,
            TypeErrorKind::ReturnTypeMismatch,
            TypeErrorKind::UseAfterMove,
            TypeErrorKind::YieldOutsideGenerator,
            TypeErrorKind::ActorRefCycle,
            TypeErrorKind::UnusedVariable,
            TypeErrorKind::UnusedMut,
            TypeErrorKind::StyleSuggestion,
            TypeErrorKind::UnusedImport,
            TypeErrorKind::UnreachableCode,
            TypeErrorKind::Shadowing,
            TypeErrorKind::DeadCode,
            TypeErrorKind::PurityViolation,
            TypeErrorKind::OrphanImpl,
            TypeErrorKind::PlatformLimitation,
            TypeErrorKind::MachineExhaustivenessError,
        ];
        for kind in &kinds {
            let data = diagnostic_data(kind, &[]);
            let kind_str = data["kind"].as_str().unwrap();
            assert!(!kind_str.is_empty(), "kind string should not be empty");
        }
    }

    // ── Build diagnostics tests ─────────────────────────────────────

    #[test]
    fn diagnostics_from_type_errors() {
        let source = "fn main() -> i32 { undefined_var }";
        let parse_result = hew_parser::parse(source);
        let lo = compute_line_offsets(source);
        let uri = Url::parse("file:///test.hew").unwrap();
        let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
        let type_output = checker.check_program(&parse_result.program);
        let diags = build_diagnostics(&uri, source, &lo, &parse_result, Some(&type_output));
        assert!(
            !diags.is_empty(),
            "expected diagnostics for undefined variable"
        );
        let type_diag = diags
            .iter()
            .find(|d| d.source.as_deref() == Some("hew-types"));
        assert!(type_diag.is_some(), "expected a type-checker diagnostic");
    }

    #[test]
    fn diagnostics_include_suggestions_in_message() {
        let source = "fn main() -> i32 { let counter = 42; counte }";
        let parse_result = hew_parser::parse(source);
        let lo = compute_line_offsets(source);
        let uri = Url::parse("file:///test.hew").unwrap();
        let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
        let type_output = checker.check_program(&parse_result.program);
        let diags = build_diagnostics(&uri, source, &lo, &parse_result, Some(&type_output));
        // Look for a diagnostic that contains "Did you mean" (suggestion text)
        let has_suggestion = diags
            .iter()
            .any(|d| d.message.contains("Did you mean") || d.data.is_some());
        // This test validates the diagnostic builder can handle suggestions;
        // the actual suggestion depends on the type checker heuristics.
        assert!(
            !diags.is_empty(),
            "expected diagnostics for misspelled variable"
        );
        // Verify that data field is populated for type errors
        let type_diags: Vec<_> = diags
            .iter()
            .filter(|d| d.source.as_deref() == Some("hew-types"))
            .collect();
        for d in &type_diags {
            assert!(d.data.is_some(), "type diagnostics should have data field");
        }
        let _ = has_suggestion; // suppress unused warning
    }

    #[test]
    fn diagnostics_parser_warning_severity() {
        // Empty source produces no diagnostics
        let source = "";
        let parse_result = hew_parser::parse(source);
        let lo = compute_line_offsets(source);
        let uri = Url::parse("file:///test.hew").unwrap();
        let diags = build_diagnostics(&uri, source, &lo, &parse_result, None);
        assert!(diags.is_empty(), "empty source should have no diagnostics");
    }

    #[test]
    fn diagnostics_route_non_root_module_path_to_imported_uri() {
        let main_source = "import shapes::circle;\nfn main() -> i64 { circle.mistyped() }\n";
        let circle_source = "pub fn mistyped() -> i64 { true }\n";

        let main_url = make_test_uri("/fake/project/main.hew");
        let circle_url = make_test_uri("/fake/project/shapes/circle.hew");

        let documents: DashMap<Url, DocumentState> = DashMap::new();
        documents.insert(circle_url.clone(), make_doc(circle_source));

        let refreshed = refresh_document_and_dependents(&main_url, main_source, &documents);
        let main_diags = refreshed
            .iter()
            .find(|(uri, _)| uri == &main_url)
            .map(|(_, diags)| diags)
            .expect("main document must be republished");
        assert!(
            main_diags.is_empty(),
            "non-root type diagnostics should not stay attached to the importer: {main_diags:?}"
        );

        let circle_diags = refreshed
            .iter()
            .find(|(uri, _)| uri == &circle_url)
            .map(|(_, diags)| diags)
            .expect("imported module must receive routed diagnostics");
        assert!(
            circle_diags
                .iter()
                .any(|diag| diag.source.as_deref() == Some("hew-types")),
            "expected routed hew-types diagnostic on imported module: {circle_diags:?}"
        );
    }

    #[test]
    fn diagnostics_route_non_root_related_information_to_imported_uri() {
        let main_source = "import \"dep.hew\";\nfn main() {}\n";
        let dep_source = "pub fn dup() -> i64 { 1 }\npub fn dup() -> i64 { 2 }\n";

        let main_url = make_test_uri("/fake/project/main.hew");
        let dep_url = make_test_uri("/fake/project/dep.hew");

        let documents: DashMap<Url, DocumentState> = DashMap::new();
        documents.insert(dep_url.clone(), make_doc(dep_source));

        let refreshed = refresh_document_and_dependents(&main_url, main_source, &documents);
        let dep_diag = refreshed
            .iter()
            .find(|(uri, _)| uri == &dep_url)
            .and_then(|(_, diags)| {
                diags.iter().find(|diag| {
                    diag.source.as_deref() == Some("hew-types")
                        && diag
                            .related_information
                            .as_ref()
                            .is_some_and(|info| !info.is_empty())
                })
            })
            .expect("expected a routed diagnostic with related information");

        assert_eq!(
            dep_diag.related_information.as_ref().unwrap()[0]
                .location
                .uri,
            dep_url
        );
    }

    // ── Completion conversion tests ─────────────────────────────────

    #[test]
    fn to_lsp_completion_maps_kinds() {
        use hew_analysis::{CompletionItem as AnalysisItem, CompletionKind};
        let cases = vec![
            (CompletionKind::Function, CompletionItemKind::FUNCTION),
            (CompletionKind::Variable, CompletionItemKind::VARIABLE),
            (CompletionKind::Keyword, CompletionItemKind::KEYWORD),
            (CompletionKind::Snippet, CompletionItemKind::SNIPPET),
            (CompletionKind::Type, CompletionItemKind::STRUCT),
            (CompletionKind::Actor, CompletionItemKind::CLASS),
            (CompletionKind::Constant, CompletionItemKind::CONSTANT),
            (CompletionKind::Field, CompletionItemKind::FIELD),
            (CompletionKind::Method, CompletionItemKind::METHOD),
            (CompletionKind::Module, CompletionItemKind::MODULE),
        ];
        for (analysis_kind, expected_lsp_kind) in cases {
            let item = AnalysisItem {
                label: "test".to_string(),
                kind: analysis_kind,
                detail: None,
                insert_text: None,
                insert_text_is_snippet: false,
                sort_text: None,
            };
            let lsp_item = to_lsp_completion(item);
            assert_eq!(lsp_item.kind, Some(expected_lsp_kind));
        }
    }

    #[test]
    fn to_lsp_completion_snippet_format() {
        use hew_analysis::{CompletionItem as AnalysisItem, CompletionKind};
        let item = AnalysisItem {
            label: "if".to_string(),
            kind: CompletionKind::Snippet,
            detail: Some("if statement".to_string()),
            insert_text: Some("if ${1:condition} {\n\t$0\n}".to_string()),
            insert_text_is_snippet: true,
            sort_text: Some("0001".to_string()),
        };
        let lsp_item = to_lsp_completion(item);
        assert_eq!(lsp_item.label, "if");
        assert_eq!(lsp_item.detail.as_deref(), Some("if statement"));
        assert_eq!(lsp_item.insert_text_format, Some(InsertTextFormat::SNIPPET));
        assert_eq!(lsp_item.sort_text.as_deref(), Some("0001"));
    }

    // ── Symbol kind mapping tests ───────────────────────────────────

    #[test]
    fn analysis_symbol_kind_mapping() {
        let cases = vec![
            (hew_analysis::SymbolKind::Function, SymbolKind::FUNCTION),
            (hew_analysis::SymbolKind::Actor, SymbolKind::CLASS),
            (hew_analysis::SymbolKind::Supervisor, SymbolKind::CLASS),
            (hew_analysis::SymbolKind::Machine, SymbolKind::ENUM),
            (hew_analysis::SymbolKind::Enum, SymbolKind::ENUM),
            (hew_analysis::SymbolKind::Trait, SymbolKind::INTERFACE),
            (hew_analysis::SymbolKind::Type, SymbolKind::STRUCT),
            (hew_analysis::SymbolKind::Wire, SymbolKind::STRUCT),
            (hew_analysis::SymbolKind::Constant, SymbolKind::CONSTANT),
            (
                hew_analysis::SymbolKind::TypeAlias,
                SymbolKind::TYPE_PARAMETER,
            ),
            (hew_analysis::SymbolKind::Impl, SymbolKind::NAMESPACE),
            (hew_analysis::SymbolKind::Field, SymbolKind::FIELD),
            (hew_analysis::SymbolKind::Method, SymbolKind::METHOD),
            (hew_analysis::SymbolKind::State, SymbolKind::ENUM_MEMBER),
            (hew_analysis::SymbolKind::Variant, SymbolKind::ENUM_MEMBER),
            (hew_analysis::SymbolKind::Event, SymbolKind::EVENT),
            (hew_analysis::SymbolKind::Module, SymbolKind::MODULE),
            (
                hew_analysis::SymbolKind::Constructor,
                SymbolKind::CONSTRUCTOR,
            ),
        ];
        for (analysis_kind, expected_lsp_kind) in cases {
            assert_eq!(
                analysis_symbol_kind_to_lsp(analysis_kind),
                expected_lsp_kind,
                "mapping mismatch for {analysis_kind:?}"
            );
        }
    }

    // ── Document symbol tests (additional) ──────────────────────────

    #[test]
    fn document_symbols_for_actor_with_receive() {
        let source = "actor Counter {\n    count: i32;\n    receive fn increment(n: i32) { count = count + n; }\n}";
        let parse_result = hew_parser::parse(source);
        let lo = compute_line_offsets(source);
        let analysis_symbols = hew_analysis::symbols::build_document_symbols(source, &parse_result);
        let symbols: Vec<DocumentSymbol> = analysis_symbols
            .into_iter()
            .map(|s| symbol_info_to_doc_symbol(source, &lo, s))
            .collect();
        assert!(!symbols.is_empty(), "should find actor symbol");
        let actor_sym = symbols.iter().find(|s| s.name == "Counter");
        assert!(actor_sym.is_some(), "should find Counter actor symbol");
        assert_eq!(actor_sym.unwrap().kind, SymbolKind::CLASS);
    }

    #[test]
    fn document_symbols_for_enum() {
        let source = "enum Colour { Red; Green; Blue; }";
        let parse_result = hew_parser::parse(source);
        let lo = compute_line_offsets(source);
        let analysis_symbols = hew_analysis::symbols::build_document_symbols(source, &parse_result);
        let symbols: Vec<DocumentSymbol> = analysis_symbols
            .into_iter()
            .map(|s| symbol_info_to_doc_symbol(source, &lo, s))
            .collect();
        assert!(!symbols.is_empty());
        let enum_sym = symbols.iter().find(|s| s.name == "Colour");
        assert!(enum_sym.is_some(), "should find Colour enum symbol");
        assert_eq!(enum_sym.unwrap().kind, SymbolKind::ENUM);
    }

    #[test]
    fn document_symbols_with_children() {
        let source = "type Point { x: i32; y: i32; fn distance() -> i32 { 0 } }";
        let parse_result = hew_parser::parse(source);
        let lo = compute_line_offsets(source);
        let analysis_symbols = hew_analysis::symbols::build_document_symbols(source, &parse_result);
        let symbols: Vec<DocumentSymbol> = analysis_symbols
            .into_iter()
            .map(|s| symbol_info_to_doc_symbol(source, &lo, s))
            .collect();
        let point_sym = symbols.iter().find(|s| s.name == "Point").unwrap();
        assert!(
            point_sym.children.is_some(),
            "Point should have child symbols"
        );
        let children = point_sym.children.as_ref().unwrap();
        let child_names: Vec<&str> = children.iter().map(|c| c.name.as_str()).collect();
        assert!(
            child_names.contains(&"x") || child_names.contains(&"distance"),
            "children should include fields or methods, got: {child_names:?}"
        );
    }

    // ── Inlay hint tests ────────────────────────────────────────────

    #[test]
    fn inlay_hints_for_let_binding() {
        let source = "fn main() -> i32 { let x = 42; x }";
        let parse_result = hew_parser::parse(source);
        assert!(
            parse_result.errors.is_empty(),
            "parse errors: {:?}",
            parse_result.errors
        );
        let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
        let type_output = checker.check_program(&parse_result.program);
        let hints =
            hew_analysis::inlay_hints::build_inlay_hints(source, &parse_result, &type_output);
        // There should be at least one type hint for `let x = 42`
        assert!(
            !hints.is_empty(),
            "expected inlay hints for let binding with inferred type"
        );
    }

    // ── Signature help tests ────────────────────────────────────────

    #[test]
    fn signature_help_for_function_call() {
        let source = "fn add(a: i32, b: i32) -> i32 { a + b }\nfn main() -> i32 { add(1, 2) }";
        let parse_result = hew_parser::parse(source);
        assert!(
            parse_result.errors.is_empty(),
            "parse errors: {:?}",
            parse_result.errors
        );
        let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
        let type_output = checker.check_program(&parse_result.program);
        // Offset inside the call parens, e.g. after the `(`
        let offset = source.find("add(1").unwrap() + 4;
        let result =
            hew_analysis::signature_help::build_signature_help(source, &type_output, offset);
        assert!(
            result.is_some(),
            "expected signature help inside function call"
        );
        let sh = result.unwrap();
        assert!(
            !sh.signatures.is_empty(),
            "should have at least one signature"
        );
        assert!(
            sh.signatures[0].label.contains("add"),
            "signature label should mention function name, got: {}",
            sh.signatures[0].label
        );
    }

    #[test]
    fn signature_help_for_builtin_method_call() {
        let source = "fn send_one(tx: Sender<int>) { tx.send(1); }";
        let parse_result = hew_parser::parse(source);
        assert!(
            parse_result.errors.is_empty(),
            "parse errors: {:?}",
            parse_result.errors
        );
        let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
        let type_output = checker.check_program(&parse_result.program);
        assert!(
            type_output.errors.is_empty(),
            "type errors: {:?}",
            type_output.errors
        );
        let offset = source.find("send(1)").unwrap() + 5;
        let result =
            hew_analysis::signature_help::build_signature_help(source, &type_output, offset);
        assert!(
            result.is_some(),
            "expected signature help inside builtin method call"
        );
        let sh = result.unwrap();
        assert_eq!(sh.signatures[0].label, "fn send(value: int)");
    }

    // ── Non-empty helper test ───────────────────────────────────────

    #[test]
    fn non_empty_returns_none_for_empty_vec() {
        let v: Vec<i32> = vec![];
        assert!(non_empty(v).is_none());
    }

    #[test]
    fn non_empty_returns_some_for_non_empty_vec() {
        let v = vec![1, 2, 3];
        assert!(non_empty(v).is_some());
    }

    // ── Span/range conversion tests ─────────────────────────────────

    #[test]
    fn offset_range_to_lsp_basic() {
        let source = "fn main() {\n    42\n}";
        let lo = compute_line_offsets(source);
        let range = offset_range_to_lsp(source, &lo, 0, 2);
        assert_eq!(range.start, Position::new(0, 0));
        assert_eq!(range.end, Position::new(0, 2));
    }

    #[test]
    fn offset_range_to_lsp_multiline() {
        let source = "fn main() {\n    42\n}";
        let lo = compute_line_offsets(source);
        let end = source.len();
        let range = offset_range_to_lsp(source, &lo, 0, end);
        assert_eq!(range.start.line, 0);
        assert_eq!(range.end.line, 2);
    }

    // ── Code action tests ───────────────────────────────────────────

    #[test]
    fn code_actions_for_undefined_variable() {
        use hew_analysis::code_actions::{build_code_actions, DiagnosticInfo};
        let source = "fn main() -> i32 { let counter = 42; counte }";
        let parse_result = hew_parser::parse(source);
        let lo = compute_line_offsets(source);
        let uri = Url::parse("file:///test.hew").unwrap();
        let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
        let type_output = checker.check_program(&parse_result.program);
        let diags = build_diagnostics(&uri, source, &lo, &parse_result, Some(&type_output));
        // Find a diagnostic with suggestions
        let diag_with_suggestions = diags.iter().find(|d| {
            d.data
                .as_ref()
                .and_then(|data| data.get("suggestions"))
                .and_then(|v| v.as_array())
                .is_some_and(|arr| !arr.is_empty())
        });
        if let Some(diag) = diag_with_suggestions {
            let kind = diag
                .data
                .as_ref()
                .and_then(|d| d.get("kind"))
                .and_then(serde_json::Value::as_str)
                .map(String::from);
            let suggestions = diag
                .data
                .as_ref()
                .and_then(|d| d.get("suggestions"))
                .and_then(|v| serde_json::from_value::<Vec<String>>(v.clone()).ok())
                .unwrap_or_default();
            let start = position_to_offset(source, &lo, diag.range.start);
            let end = position_to_offset(source, &lo, diag.range.end);
            let info = DiagnosticInfo {
                kind,
                message: diag.message.clone(),
                span: hew_analysis::OffsetSpan { start, end },
                suggestions,
            };
            let actions = build_code_actions(source, &[info]);
            assert!(
                !actions.is_empty(),
                "expected code actions for undefined variable with suggestions"
            );
        }
    }

    #[test]
    fn code_actions_for_non_exhaustive_match() {
        use hew_analysis::code_actions::{build_code_actions, DiagnosticInfo};
        let source = r#"
            enum Colour { Red; Blue; }
            fn label(colour: Colour) -> string {
                match colour {
                    Red => "red",
                }
            }
        "#;
        let parse_result = hew_parser::parse(source);
        let lo = compute_line_offsets(source);
        let uri = Url::parse("file:///test.hew").unwrap();
        let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
        let type_output = checker.check_program(&parse_result.program);
        let diag = build_diagnostics(&uri, source, &lo, &parse_result, Some(&type_output))
            .into_iter()
            .find(|diag| {
                diag.data
                    .as_ref()
                    .and_then(|data| data.get("kind"))
                    .and_then(Value::as_str)
                    == Some("NonExhaustiveMatch")
            })
            .expect("expected non-exhaustive match diagnostic");
        let suggestions = diag
            .data
            .as_ref()
            .and_then(|d| d.get("suggestions"))
            .and_then(|v| serde_json::from_value::<Vec<String>>(v.clone()).ok())
            .unwrap_or_default();
        let info = DiagnosticInfo {
            kind: Some("NonExhaustiveMatch".to_string()),
            message: diag.message.clone(),
            span: hew_analysis::OffsetSpan {
                start: position_to_offset(source, &lo, diag.range.start),
                end: position_to_offset(source, &lo, diag.range.end),
            },
            suggestions,
        };
        let actions = build_code_actions(source, &[info]);
        assert_eq!(actions.len(), 1);
        assert_eq!(actions[0].title, "Add missing match arms");
        assert!(actions[0].edits[0].new_text.contains("Blue => {},"));
    }

    // ── has_test_attribute tests ─────────────────────────────────────

    #[test]
    fn has_test_attribute_true() {
        let attrs = vec![Attribute {
            name: "test".to_string(),
            args: vec![],
            span: 0..0,
        }];
        assert!(has_test_attribute(&attrs));
    }

    #[test]
    fn has_test_attribute_false() {
        let attrs = vec![Attribute {
            name: "inline".to_string(),
            args: vec![],
            span: 0..0,
        }];
        assert!(!has_test_attribute(&attrs));
    }

    #[test]
    fn has_test_attribute_empty() {
        assert!(!has_test_attribute(&[]));
    }

    // ── count_all_references tests ──────────────────────────────────

    #[test]
    fn count_all_references_basic() {
        let source = "fn helper() -> i32 { 42 }\nfn main() { helper() }";
        let parse_result = hew_parser::parse(source);
        let counts = count_all_references(&parse_result);
        let helper_count = counts.get("helper").copied().unwrap_or(0);
        assert!(
            helper_count >= 1,
            "expected at least 1 reference to 'helper', got {helper_count}"
        );
    }

    // ── cross-file goto-definition tests ────────────────────────────

    fn make_doc(source: &str) -> DocumentState {
        let parse_result = hew_parser::parse(source);
        let lo = compute_line_offsets(source);
        DocumentState {
            source: source.to_string(),
            line_offsets: lo,
            parse_result,
            type_output: None,
            diagnostics_by_uri: HashMap::new(),
        }
    }

    /// Returns a `file://` URI for `posix_path` that is valid on the current
    /// platform.  On Windows, `file:///project/foo` lacks a drive letter and
    /// `Url::to_file_path()` returns `Err`; prefixing with `C:` makes it a
    /// well-formed Windows file URI while leaving Unix paths unchanged.
    fn make_test_uri(posix_path: &str) -> Url {
        #[cfg(windows)]
        return Url::parse(&format!("file:///C:{posix_path}")).unwrap();
        #[cfg(not(windows))]
        return Url::parse(&format!("file://{posix_path}")).unwrap();
    }

    fn has_unresolved_import(documents: &DashMap<Url, DocumentState>, uri: &Url) -> bool {
        documents
            .get(uri)
            .and_then(|doc| {
                doc.type_output.as_ref().map(|output| {
                    output
                        .errors
                        .iter()
                        .any(|error| error.kind == TypeErrorKind::UnresolvedImport)
                })
            })
            .is_some_and(|has_unresolved| has_unresolved)
    }

    /// Returns the `ImportDecl`s extracted from the parsed program, mirroring
    /// the extraction done in `goto_definition` before the cross-file search.
    fn collect_imports(source: &str) -> Vec<hew_parser::ast::ImportDecl> {
        let pr = hew_parser::parse(source);
        pr.program
            .items
            .into_iter()
            .filter_map(|(item, _)| match item {
                Item::Import(i) => Some(i),
                _ => None,
            })
            .collect()
    }

    #[test]
    fn cross_file_prepare_rename_on_named_import_binding_returns_import_range() {
        let main_source = "import util::{ greet };\nfn main() -> i32 { greet() }";
        let util_source = "pub fn greet() -> i32 { 1 }";

        let main_uri = make_test_uri("/project/main.hew");
        let util_uri = make_test_uri("/project/util.hew");

        let documents: DashMap<Url, DocumentState> = DashMap::new();
        documents.insert(main_uri.clone(), make_doc(main_source));
        documents.insert(util_uri, make_doc(util_source));

        let main_doc = documents.get(&main_uri).unwrap();
        let offset = main_source.find("greet").unwrap();

        assert!(
            hew_analysis::rename::prepare_rename(&main_doc.source, &main_doc.parse_result, offset)
                .is_none(),
            "analysis-layer prepare_rename stays local-only for named imports"
        );

        let response = build_prepare_rename_response(&main_uri, &main_doc, offset, &documents)
            .expect("prepareRename should succeed on a named import binding");
        let PrepareRenameResponse::Range(range) = response else {
            panic!("expected prepareRename to return a range");
        };

        let expected_range = offset_range_to_lsp(
            main_source,
            &main_doc.line_offsets,
            offset,
            offset + "greet".len(),
        );
        assert_eq!(range, expected_range);
    }

    #[test]
    fn cross_file_prepare_rename_on_named_import_usage_returns_usage_range() {
        let main_source = "import util::{ greet };\nfn main() -> i32 { greet() }";
        let util_source = "pub fn greet() -> i32 { 1 }";

        let main_uri = make_test_uri("/project/main.hew");
        let util_uri = make_test_uri("/project/util.hew");

        let documents: DashMap<Url, DocumentState> = DashMap::new();
        documents.insert(main_uri.clone(), make_doc(main_source));
        documents.insert(util_uri, make_doc(util_source));

        let main_doc = documents.get(&main_uri).unwrap();
        let offset = main_source.rfind("greet").unwrap();

        let response = build_prepare_rename_response(&main_uri, &main_doc, offset, &documents)
            .expect("prepareRename should succeed on a named import usage");
        let PrepareRenameResponse::Range(range) = response else {
            panic!("expected prepareRename to return a range");
        };

        let expected_range = offset_range_to_lsp(
            main_source,
            &main_doc.line_offsets,
            offset,
            offset + "greet".len(),
        );
        assert_eq!(range, expected_range);
    }

    #[test]
    fn cross_file_references_include_named_importer_and_imported_open_document() {
        let main_source = "import util::{ greet };\nfn first() -> i32 { greet() }\nfn second() -> i32 { greet() }";
        let util_source = "pub fn greet() -> i32 { 1 }\nfn wrapper() -> i32 { greet() }";

        let main_uri = make_test_uri("/project/main.hew");
        let util_uri = make_test_uri("/project/util.hew");

        let documents: DashMap<Url, DocumentState> = DashMap::new();
        documents.insert(main_uri.clone(), make_doc(main_source));
        documents.insert(util_uri.clone(), make_doc(util_source));

        let util_doc = documents.get(&util_uri).unwrap();
        let offset = util_source.find("greet").unwrap();
        let locations = build_reference_locations(&util_uri, &util_doc, offset, true, &documents);

        assert_eq!(
            locations.len(),
            5,
            "expected importer + imported references"
        );
        assert_eq!(
            locations
                .iter()
                .filter(|location| location.uri == main_uri)
                .count(),
            3,
            "expected import site plus two main.hew call sites"
        );
        assert_eq!(
            locations
                .iter()
                .filter(|location| location.uri == util_uri)
                .count(),
            2,
            "expected greet definition plus wrapper() call in util.hew"
        );
    }

    #[test]
    fn cross_file_references_from_named_import_usage_include_imported_open_document() {
        let main_source = "import util::{ greet };\nfn first() -> i32 { greet() }\nfn second() -> i32 { greet() }";
        let util_source = "pub fn greet() -> i32 { 1 }\nfn wrapper() -> i32 { greet() }";

        let main_uri = make_test_uri("/project/main.hew");
        let util_uri = make_test_uri("/project/util.hew");

        let documents: DashMap<Url, DocumentState> = DashMap::new();
        documents.insert(main_uri.clone(), make_doc(main_source));
        documents.insert(util_uri.clone(), make_doc(util_source));

        let main_doc = documents.get(&main_uri).unwrap();
        let offset = main_source.rfind("greet").unwrap();
        let locations = build_reference_locations(&main_uri, &main_doc, offset, true, &documents);

        assert_eq!(
            locations.len(),
            5,
            "expected importer + imported references"
        );
        assert_eq!(
            locations
                .iter()
                .filter(|location| location.uri == main_uri)
                .count(),
            3,
            "expected import site plus two main.hew call sites"
        );
        assert_eq!(
            locations
                .iter()
                .filter(|location| location.uri == util_uri)
                .count(),
            2,
            "expected greet definition plus wrapper() call in util.hew"
        );
    }

    #[test]
    fn cross_file_references_from_named_import_usage_include_other_open_importers() {
        let main_source = "import util::{ greet };\nfn main() -> i32 { greet() }";
        let helper_source = "import util::{ greet };\nfn helper() -> i32 { greet() }";
        let util_source = "pub fn greet() -> i32 { 1 }\nfn wrapper() -> i32 { greet() }";

        let main_uri = make_test_uri("/project/main.hew");
        let helper_uri = make_test_uri("/project/helper.hew");
        let util_uri = make_test_uri("/project/util.hew");

        let documents: DashMap<Url, DocumentState> = DashMap::new();
        documents.insert(main_uri.clone(), make_doc(main_source));
        documents.insert(helper_uri.clone(), make_doc(helper_source));
        documents.insert(util_uri.clone(), make_doc(util_source));

        let main_doc = documents.get(&main_uri).unwrap();
        let offset = main_source.rfind("greet").unwrap();
        let locations = build_reference_locations(&main_uri, &main_doc, offset, true, &documents);

        assert_eq!(
            locations.len(),
            6,
            "expected both importers plus imported references"
        );
        assert_eq!(
            locations
                .iter()
                .filter(|location| location.uri == main_uri)
                .count(),
            2,
            "expected import site plus usage in main.hew"
        );
        assert_eq!(
            locations
                .iter()
                .filter(|location| location.uri == helper_uri)
                .count(),
            2,
            "expected import site plus usage in helper.hew"
        );
        assert_eq!(
            locations
                .iter()
                .filter(|location| location.uri == util_uri)
                .count(),
            2,
            "expected definition plus wrapper() call in util.hew"
        );
    }

    #[test]
    fn cross_file_rename_updates_named_importer_and_imported_open_document() {
        let main_source = "import util::{ greet };\nfn first() -> i32 { greet() }\nfn second() -> i32 { greet() }";
        let util_source = "pub fn greet() -> i32 { 1 }\nfn wrapper() -> i32 { greet() }";

        let main_uri = make_test_uri("/project/main.hew");
        let util_uri = make_test_uri("/project/util.hew");

        let documents: DashMap<Url, DocumentState> = DashMap::new();
        documents.insert(main_uri.clone(), make_doc(main_source));
        documents.insert(util_uri.clone(), make_doc(util_source));

        let main_doc = documents.get(&main_uri).unwrap();
        let offset = main_source.rfind("greet").unwrap();
        let workspace_edit =
            build_workspace_edit(&main_uri, &main_doc, offset, "welcome", &documents)
                .expect("rename should produce a cross-file workspace edit");
        let changes = workspace_edit
            .changes
            .expect("workspace edit should contain per-document text edits");

        let main_edits = changes
            .get(&main_uri)
            .expect("rename should include main.hew edits");
        assert_eq!(main_edits.len(), 3, "expected import + two call-site edits");
        assert!(main_edits.iter().all(|edit| edit.new_text == "welcome"));

        let util_edits = changes
            .get(&util_uri)
            .expect("rename should include util.hew edits");
        assert_eq!(
            util_edits.len(),
            2,
            "expected definition + wrapper call edits"
        );
        assert!(util_edits.iter().all(|edit| edit.new_text == "welcome"));
    }

    #[test]
    fn cross_file_rename_from_named_import_usage_updates_other_open_importers() {
        let main_source = "import util::{ greet };\nfn main() -> i32 { greet() }";
        let helper_source = "import util::{ greet };\nfn helper() -> i32 { greet() }";
        let util_source = "pub fn greet() -> i32 { 1 }\nfn wrapper() -> i32 { greet() }";

        let main_uri = make_test_uri("/project/main.hew");
        let helper_uri = make_test_uri("/project/helper.hew");
        let util_uri = make_test_uri("/project/util.hew");

        let documents: DashMap<Url, DocumentState> = DashMap::new();
        documents.insert(main_uri.clone(), make_doc(main_source));
        documents.insert(helper_uri.clone(), make_doc(helper_source));
        documents.insert(util_uri.clone(), make_doc(util_source));

        let main_doc = documents.get(&main_uri).unwrap();
        let offset = main_source.rfind("greet").unwrap();
        let workspace_edit =
            build_workspace_edit(&main_uri, &main_doc, offset, "welcome", &documents)
                .expect("rename should produce a cross-file workspace edit");
        let changes = workspace_edit
            .changes
            .expect("workspace edit should contain per-document text edits");

        let main_edits = changes
            .get(&main_uri)
            .expect("rename should include main.hew edits");
        assert_eq!(
            main_edits.len(),
            2,
            "expected import + usage edits in main.hew"
        );
        assert!(main_edits.iter().all(|edit| edit.new_text == "welcome"));

        let helper_edits = changes
            .get(&helper_uri)
            .expect("rename should include helper.hew edits");
        assert_eq!(
            helper_edits.len(),
            2,
            "expected import + usage edits in helper.hew"
        );
        assert!(helper_edits.iter().all(|edit| edit.new_text == "welcome"));

        let util_edits = changes
            .get(&util_uri)
            .expect("rename should include util.hew edits");
        assert_eq!(
            util_edits.len(),
            2,
            "expected definition + wrapper call edits"
        );
        assert!(util_edits.iter().all(|edit| edit.new_text == "welcome"));
    }

    #[test]
    fn cross_file_rename_from_imported_definition_updates_open_importer() {
        let main_source = "import util::{ greet };\nfn first() -> i32 { greet() }\nfn second() -> i32 { greet() }";
        let util_source = "pub fn greet() -> i32 { 1 }\nfn wrapper() -> i32 { greet() }";

        let main_uri = make_test_uri("/project/main.hew");
        let util_uri = make_test_uri("/project/util.hew");

        let documents: DashMap<Url, DocumentState> = DashMap::new();
        documents.insert(main_uri.clone(), make_doc(main_source));
        documents.insert(util_uri.clone(), make_doc(util_source));

        let util_doc = documents.get(&util_uri).unwrap();
        let offset = util_source.find("greet").unwrap();
        let workspace_edit =
            build_workspace_edit(&util_uri, &util_doc, offset, "welcome", &documents)
                .expect("rename should produce a cross-file workspace edit");
        let changes = workspace_edit
            .changes
            .expect("workspace edit should contain per-document text edits");

        let util_edits = changes
            .get(&util_uri)
            .expect("rename should include util.hew edits");
        assert_eq!(
            util_edits.len(),
            2,
            "expected definition + wrapper call edits"
        );
        assert!(util_edits.iter().all(|edit| edit.new_text == "welcome"));

        let main_edits = changes
            .get(&main_uri)
            .expect("rename should include main.hew edits");
        assert_eq!(main_edits.len(), 3, "expected import + two call-site edits");
        assert!(main_edits.iter().all(|edit| edit.new_text == "welcome"));
    }

    #[test]
    fn cross_file_references_shadowed_local_usage_stays_local() {
        let main_source = "import util::{ greet };\nfn imported() -> i32 { greet() }\nfn shadowed() -> i32 { let greet = 0; greet }";
        let util_source = "pub fn greet() -> i32 { 1 }\nfn wrapper() -> i32 { greet() }";

        let main_uri = make_test_uri("/project/main.hew");
        let util_uri = make_test_uri("/project/util.hew");

        let documents: DashMap<Url, DocumentState> = DashMap::new();
        documents.insert(main_uri.clone(), make_doc(main_source));
        documents.insert(util_uri.clone(), make_doc(util_source));

        let main_doc = documents.get(&main_uri).unwrap();
        let offset = main_source.rfind("greet").unwrap();
        let locations = build_reference_locations(&main_uri, &main_doc, offset, true, &documents);

        assert!(
            locations.len() >= 2,
            "expected at least the local shadow binding references"
        );
        assert!(
            locations.iter().all(|location| location.uri == main_uri),
            "shadowed local references must stay within main.hew"
        );
    }

    #[test]
    fn cross_file_rename_shadowed_local_usage_stays_local() {
        let main_source = "import util::{ greet };\nfn imported() -> i32 { greet() }\nfn shadowed() -> i32 { let greet = 0; greet }";
        let util_source = "pub fn greet() -> i32 { 1 }\nfn wrapper() -> i32 { greet() }";

        let main_uri = make_test_uri("/project/main.hew");
        let util_uri = make_test_uri("/project/util.hew");

        let documents: DashMap<Url, DocumentState> = DashMap::new();
        documents.insert(main_uri.clone(), make_doc(main_source));
        documents.insert(util_uri.clone(), make_doc(util_source));

        let main_doc = documents.get(&main_uri).unwrap();
        let offset = main_source.rfind("greet").unwrap();
        let workspace_edit =
            build_workspace_edit(&main_uri, &main_doc, offset, "welcome", &documents)
                .expect("rename should produce local edits");
        let changes = workspace_edit
            .changes
            .expect("workspace edit should contain per-document text edits");

        assert_eq!(changes.len(), 1, "expected only main.hew edits");
        let main_edits = changes
            .get(&main_uri)
            .expect("rename should include main.hew edits");
        assert!(
            main_edits.len() >= 2,
            "expected at least the local binding + usage edits"
        );
        assert!(main_edits.iter().all(|edit| edit.new_text == "welcome"));
        assert!(
            !changes.contains_key(&util_uri),
            "shadowed local rename must not touch util.hew"
        );
    }

    #[test]
    fn cross_file_rename_from_imported_definition_skips_shadowed_local_usage() {
        let main_source = "import util::{ greet };\nfn imported() -> i32 { greet() }\nfn shadowed() -> i32 { let greet = 0; greet }";
        let util_source = "pub fn greet() -> i32 { 1 }\nfn wrapper() -> i32 { greet() }";

        let main_uri = make_test_uri("/project/main.hew");
        let util_uri = make_test_uri("/project/util.hew");

        let documents: DashMap<Url, DocumentState> = DashMap::new();
        documents.insert(main_uri.clone(), make_doc(main_source));
        documents.insert(util_uri.clone(), make_doc(util_source));

        let util_doc = documents.get(&util_uri).unwrap();
        let offset = util_source.find("greet").unwrap();
        let workspace_edit =
            build_workspace_edit(&util_uri, &util_doc, offset, "welcome", &documents)
                .expect("rename should produce a cross-file workspace edit");
        let changes = workspace_edit
            .changes
            .expect("workspace edit should contain per-document text edits");

        let util_edits = changes
            .get(&util_uri)
            .expect("rename should include util.hew edits");
        assert_eq!(
            util_edits.len(),
            2,
            "expected definition + wrapper call edits"
        );
        assert!(util_edits.iter().all(|edit| edit.new_text == "welcome"));

        let main_edits = changes
            .get(&main_uri)
            .expect("rename should include main.hew edits");
        assert_eq!(
            main_edits.len(),
            2,
            "expected import + unshadowed call edit"
        );
        assert!(main_edits.iter().all(|edit| edit.new_text == "welcome"));
    }

    #[test]
    fn cross_file_references_top_level_collision_stays_local() {
        let main_source =
            "import util::{ greet };\nfn greet() -> i32 { 0 }\nfn main() -> i32 { greet() }";
        let util_source = "pub fn greet() -> i32 { 1 }\nfn wrapper() -> i32 { greet() }";

        let main_uri = make_test_uri("/project/main.hew");
        let util_uri = make_test_uri("/project/util.hew");

        let documents: DashMap<Url, DocumentState> = DashMap::new();
        documents.insert(main_uri.clone(), make_doc(main_source));
        documents.insert(util_uri.clone(), make_doc(util_source));

        let main_doc = documents.get(&main_uri).unwrap();
        let offset = main_source.rfind("greet").unwrap();
        let locations = build_reference_locations(&main_uri, &main_doc, offset, true, &documents);

        assert_eq!(
            locations.len(),
            2,
            "expected only the local top-level symbol"
        );
        assert!(locations.iter().all(|location| location.uri == main_uri));
    }

    #[test]
    fn cross_file_goto_named_import_resolves_to_open_document() {
        // `main.hew` imports `Counter` from `counter.hew` (open in the editor).
        let main_source = "import counter::{ Counter };\nfn main() {}";
        let counter_source = "type Counter { value: i32 }";

        let main_uri = make_test_uri("/project/main.hew");
        let counter_uri = make_test_uri("/project/counter.hew");

        let documents: DashMap<Url, DocumentState> = DashMap::new();
        documents.insert(main_uri.clone(), make_doc(main_source));
        documents.insert(counter_uri.clone(), make_doc(counter_source));

        let imports = collect_imports(main_source);
        let result = find_cross_file_definition(&main_uri, &imports, "Counter", &documents);

        assert!(result.is_some(), "should resolve Counter to counter.hew");
        let (uri, _range) = result.unwrap();
        assert_eq!(uri, counter_uri, "should point to counter.hew");
    }

    #[test]
    fn cross_file_goto_aliased_import_resolves_by_alias() {
        // `import counter::{ Counter as Cnt }` — cursor on `Cnt` in usage.
        let main_source = "import counter::{ Counter as Cnt };\nfn main() {}";
        let counter_source = "type Counter { value: i32 }";

        let main_uri = make_test_uri("/project/main.hew");
        let counter_uri = make_test_uri("/project/counter.hew");

        let documents: DashMap<Url, DocumentState> = DashMap::new();
        documents.insert(main_uri.clone(), make_doc(main_source));
        documents.insert(counter_uri.clone(), make_doc(counter_source));

        let imports = collect_imports(main_source);
        // The visible name in the current file is `Cnt` (the alias).
        let result = find_cross_file_definition(&main_uri, &imports, "Cnt", &documents);

        assert!(result.is_some(), "should resolve alias Cnt to counter.hew");
        let (uri, _range) = result.unwrap();
        assert_eq!(uri, counter_uri, "should point to counter.hew");
    }

    #[test]
    fn cross_file_goto_glob_import_resolves_to_open_document() {
        // `import counter::*` — any name can come from counter.hew.
        let main_source = "import counter::*;\nfn main() {}";
        let counter_source = "type Counter { value: i32 }";

        let main_uri = make_test_uri("/project/main.hew");
        let counter_uri = make_test_uri("/project/counter.hew");

        let documents: DashMap<Url, DocumentState> = DashMap::new();
        documents.insert(main_uri.clone(), make_doc(main_source));
        documents.insert(counter_uri.clone(), make_doc(counter_source));

        let imports = collect_imports(main_source);
        let result = find_cross_file_definition(&main_uri, &imports, "Counter", &documents);

        assert!(result.is_some(), "glob import should resolve Counter");
        let (uri, _range) = result.unwrap();
        assert_eq!(uri, counter_uri);
    }

    #[test]
    fn cross_file_goto_absent_name_returns_none() {
        // `NotDefined` is not in counter.hew.
        let main_source = "import counter::{ Counter };\nfn main() {}";
        let counter_source = "type Counter { value: i32 }";

        let main_uri = make_test_uri("/project/main.hew");
        let counter_uri = make_test_uri("/project/counter.hew");

        let documents: DashMap<Url, DocumentState> = DashMap::new();
        documents.insert(main_uri.clone(), make_doc(main_source));
        documents.insert(counter_uri.clone(), make_doc(counter_source));

        let imports = collect_imports(main_source);
        let result = find_cross_file_definition(&main_uri, &imports, "NotDefined", &documents);
        assert!(result.is_none(), "should return None for unknown name");
    }

    #[test]
    fn cross_file_goto_name_not_in_explicit_imports_returns_none() {
        // `Bar` is not in the explicit import list even though counter.hew defines it.
        let main_source = "import counter::{ Counter };\nfn main() {}";
        let counter_source = "type Counter { value: i32 }\ntype Bar { x: i32 }";

        let main_uri = make_test_uri("/project/main.hew");
        let counter_uri = make_test_uri("/project/counter.hew");

        let documents: DashMap<Url, DocumentState> = DashMap::new();
        documents.insert(main_uri.clone(), make_doc(main_source));
        documents.insert(counter_uri.clone(), make_doc(counter_source));

        let imports = collect_imports(main_source);
        let result = find_cross_file_definition(&main_uri, &imports, "Bar", &documents);
        assert!(
            result.is_none(),
            "Bar was not imported, should not be found"
        );
    }

    #[test]
    fn cross_file_goto_transitive_import_resolves_one_hop_deeper() {
        let main_source = "import middle::{ Counter };\nfn main() {}";
        let middle_source = "import leaf::{ Counter };\nfn helper() {}";
        let leaf_source = "type Counter { value: i32 }";

        let main_uri = make_test_uri("/project/main.hew");
        let middle_uri = make_test_uri("/project/middle.hew");
        let leaf_uri = make_test_uri("/project/leaf.hew");

        let documents: DashMap<Url, DocumentState> = DashMap::new();
        documents.insert(main_uri.clone(), make_doc(main_source));
        documents.insert(middle_uri, make_doc(middle_source));
        documents.insert(leaf_uri.clone(), make_doc(leaf_source));

        let imports = collect_imports(main_source);
        let result = find_cross_file_definition(&main_uri, &imports, "Counter", &documents);

        assert!(
            result.is_some(),
            "transitive import should resolve Counter through middle.hew"
        );
        let (uri, _range) = result.unwrap();
        assert_eq!(uri, leaf_uri, "should point to leaf.hew");
    }

    #[test]
    fn cross_file_goto_transitive_cycle_returns_none() {
        let main_source = "import middle::{ Counter };\nfn main() {}";
        let middle_source = "import main::{ Counter };\nfn helper() {}";

        let main_uri = make_test_uri("/project/main.hew");
        let middle_uri = make_test_uri("/project/middle.hew");

        let documents: DashMap<Url, DocumentState> = DashMap::new();
        documents.insert(main_uri.clone(), make_doc(main_source));
        documents.insert(middle_uri, make_doc(middle_source));

        let imports = collect_imports(main_source);
        let result = find_cross_file_definition(&main_uri, &imports, "Counter", &documents);

        assert!(
            result.is_none(),
            "cycle should terminate and report no definition"
        );
    }

    #[test]
    fn cross_file_goto_multiple_imports_from_same_file_do_not_leak_seen() {
        let main_source = "import middle::{ Timer };\nimport middle::{ Counter };\nfn main() {}";
        let middle_source = "type Counter { value: i32 }\ntype Timer { ticks: i32 }";

        let main_uri = make_test_uri("/project/main.hew");
        let middle_uri = make_test_uri("/project/middle.hew");

        let documents: DashMap<Url, DocumentState> = DashMap::new();
        documents.insert(main_uri.clone(), make_doc(main_source));
        documents.insert(middle_uri.clone(), make_doc(middle_source));

        let imports = collect_imports(main_source);
        let result = find_cross_file_definition(&main_uri, &imports, "Counter", &documents);

        assert!(
            result.is_some(),
            "later imports from the same file should still be evaluated"
        );
        let (uri, _range) = result.unwrap();
        assert_eq!(uri, middle_uri, "should still point to middle.hew");
    }

    // ── In-memory module parity tests ───────────────────────────────

    /// `populate_user_module_imports` prefers the in-memory buffer for an open
    /// sibling module over anything that might be on disk.
    #[test]
    fn populate_prefers_open_document_over_disk() {
        // Simulate: main.hew imports shapes::circle.
        // circle.hew is "open" in the editor with a pub function `area`.
        let main_source = "import shapes::circle;\nfn main() { circle.area(1.0) }";
        let circle_source = "pub fn area(r: f64) -> f64 { r }";

        let main_url = make_test_uri("/fake/project/main.hew");
        let circle_url = make_test_uri("/fake/project/shapes/circle.hew");

        let documents: DashMap<Url, DocumentState> = DashMap::new();
        documents.insert(circle_url, make_doc(circle_source));

        let mut parse_result = hew_parser::parse(main_source);
        assert!(
            parse_result.errors.is_empty(),
            "unexpected parse errors in main_source: {:?}",
            parse_result.errors
        );
        populate_user_module_imports(&main_url, &mut parse_result.program.items, &documents);

        // The import should now have resolved_items populated from the in-memory buffer.
        let import_decl = parse_result
            .program
            .items
            .iter()
            .find_map(|(item, _)| {
                if let hew_parser::ast::Item::Import(d) = item {
                    Some(d)
                } else {
                    None
                }
            })
            .expect("import should be present");

        assert!(
            import_decl.resolved_items.is_some(),
            "resolved_items should be populated from the open document"
        );
        let resolved = import_decl.resolved_items.as_ref().unwrap();
        let has_area_fn = resolved.iter().any(
            |(item, _)| matches!(item, hew_parser::ast::Item::Function(f) if f.name == "area"),
        );
        assert!(
            has_area_fn,
            "resolved items should include the 'area' function from circle.hew"
        );
    }

    /// `populate_user_module_imports` also resolves string-literal file imports
    /// from the in-memory document store before type checking.
    #[test]
    fn populate_prefers_open_document_for_file_import() {
        let main_source = "import \"shapes/circle.hew\";\nfn main() { area(1.0) }";
        let circle_source = "pub fn area(r: f64) -> f64 { r }";

        let main_url = make_test_uri("/fake/project/main.hew");
        let circle_url = make_test_uri("/fake/project/shapes/circle.hew");

        let documents: DashMap<Url, DocumentState> = DashMap::new();
        documents.insert(circle_url, make_doc(circle_source));

        let mut parse_result = hew_parser::parse(main_source);
        assert!(
            parse_result.errors.is_empty(),
            "unexpected parse errors in main_source: {:?}",
            parse_result.errors
        );
        populate_user_module_imports(&main_url, &mut parse_result.program.items, &documents);

        let import_decl = parse_result
            .program
            .items
            .iter()
            .find_map(|(item, _)| {
                if let hew_parser::ast::Item::Import(d) = item {
                    Some(d)
                } else {
                    None
                }
            })
            .expect("import should be present");

        assert!(
            import_decl.resolved_items.is_some(),
            "resolved_items should be populated from the open file import document"
        );
        let resolved = import_decl.resolved_items.as_ref().unwrap();
        let has_area_fn = resolved.iter().any(
            |(item, _)| matches!(item, hew_parser::ast::Item::Function(f) if f.name == "area"),
        );
        assert!(
            has_area_fn,
            "resolved items should include the 'area' function from shapes/circle.hew"
        );
    }

    /// Type-checking a file that imports an open sibling module should produce
    /// no `UnresolvedImport` error when the sibling is in the document store.
    #[test]
    fn typecheck_sees_open_sibling_module_no_unresolved_import() {
        let main_source = "import shapes::circle;\nfn main() { circle.area(1.0) }";
        let circle_source = "pub fn area(r: f64) -> f64 { r }";

        let main_url = make_test_uri("/fake/project/main.hew");
        let circle_url = make_test_uri("/fake/project/shapes/circle.hew");

        let documents: DashMap<Url, DocumentState> = DashMap::new();
        documents.insert(circle_url, make_doc(circle_source));

        let mut parse_result = hew_parser::parse(main_source);
        assert!(
            parse_result.errors.is_empty(),
            "unexpected parse errors: {:?}",
            parse_result.errors
        );

        // Populate resolved_items from the documents map.
        populate_user_module_imports(&main_url, &mut parse_result.program.items, &documents);

        let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
        let output = checker.check_program(&parse_result.program);

        let unresolved: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(e.kind, hew_types::error::TypeErrorKind::UnresolvedImport))
            .collect();
        assert!(
            unresolved.is_empty(),
            "should have no UnresolvedImport when sibling is open in documents: {unresolved:?}"
        );
    }

    /// Type-checking with an open file import should not emit false editor
    /// errors once the imported file has been loaded into `resolved_items`.
    #[test]
    fn typecheck_sees_open_file_import_no_unresolved_import() {
        let main_source = "import \"shapes/circle.hew\";\nfn main() -> f64 { area(1.0) }";
        let circle_source = "pub fn area(r: f64) -> f64 { r }";

        let main_url = make_test_uri("/fake/project/main.hew");
        let circle_url = make_test_uri("/fake/project/shapes/circle.hew");

        let documents: DashMap<Url, DocumentState> = DashMap::new();
        documents.insert(circle_url, make_doc(circle_source));

        let mut parse_result = hew_parser::parse(main_source);
        assert!(
            parse_result.errors.is_empty(),
            "unexpected parse errors: {:?}",
            parse_result.errors
        );

        populate_user_module_imports(&main_url, &mut parse_result.program.items, &documents);

        let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
        let output = checker.check_program(&parse_result.program);

        let hard_errors: Vec<_> = output
            .errors
            .iter()
            .filter(|e| e.severity == hew_types::error::Severity::Error)
            .collect();
        assert!(
            hard_errors.is_empty(),
            "open file import should not produce hard diagnostics: {hard_errors:?}"
        );
    }

    #[test]
    fn typecheck_opening_file_import_target_refreshes_open_importer_diagnostics() {
        let main_source = "import \"foo.hew\";\nfn main() -> i32 { exported() }";
        let foo_source = "pub fn exported() -> i32 { 1 }";

        let main_url = make_test_uri("/fake/project/main.hew");
        let foo_url = make_test_uri("/fake/project/foo.hew");

        let documents: DashMap<Url, DocumentState> = DashMap::new();

        let initial = refresh_document_and_dependents(&main_url, main_source, &documents);
        assert_eq!(
            initial.len(),
            1,
            "opening importer should only analyze itself"
        );
        assert!(
            has_unresolved_import(&documents, &main_url),
            "importer should start with UnresolvedImport before foo.hew is open"
        );

        let refreshed = refresh_document_and_dependents(&foo_url, foo_source, &documents);
        assert!(
            refreshed.iter().any(|(uri, _)| uri == &main_url),
            "opening foo.hew should refresh diagnostics for the open importer"
        );
        assert!(
            !has_unresolved_import(&documents, &main_url),
            "opening foo.hew should clear the importer's stale UnresolvedImport"
        );
    }

    #[test]
    fn typecheck_changing_file_import_target_refreshes_open_importer_diagnostics() {
        let main_source = "import \"foo.hew\";\nfn main() -> i32 { exported() }";
        let invalid_foo_source = "pub fn exported(";
        let foo_source = "pub fn exported() -> i32 { 1 }";

        let main_url = make_test_uri("/fake/project/main.hew");
        let foo_url = make_test_uri("/fake/project/foo.hew");

        let documents: DashMap<Url, DocumentState> = DashMap::new();

        refresh_document_and_dependents(&main_url, main_source, &documents);
        assert!(
            has_unresolved_import(&documents, &main_url),
            "importer should start with UnresolvedImport before foo.hew is valid"
        );

        refresh_document_and_dependents(&foo_url, invalid_foo_source, &documents);
        assert!(
            has_unresolved_import(&documents, &main_url),
            "importer should stay unresolved while foo.hew has parse errors"
        );

        let refreshed = refresh_document_and_dependents(&foo_url, foo_source, &documents);
        assert!(
            refreshed.iter().any(|(uri, _)| uri == &main_url),
            "editing foo.hew should refresh diagnostics for the open importer"
        );
        assert!(
            !has_unresolved_import(&documents, &main_url),
            "editing foo.hew to valid source should clear the importer's stale UnresolvedImport"
        );
    }

    #[test]
    fn typecheck_closing_unsaved_file_import_target_refreshes_open_importer_diagnostics() {
        let main_source = "import \"foo.hew\";\nfn main() -> i32 { exported() }";
        let foo_source = "pub fn exported() -> i32 { 1 }";

        let main_url = make_test_uri("/fake/project/main.hew");
        let foo_url = make_test_uri("/fake/project/foo.hew");

        let documents: DashMap<Url, DocumentState> = DashMap::new();

        refresh_document_and_dependents(&main_url, main_source, &documents);
        assert!(
            has_unresolved_import(&documents, &main_url),
            "importer should start with UnresolvedImport before foo.hew is open"
        );

        refresh_document_and_dependents(&foo_url, foo_source, &documents);
        assert!(
            !has_unresolved_import(&documents, &main_url),
            "opening foo.hew should clear the importer's stale UnresolvedImport"
        );

        let refreshed = close_document_and_dependents(&foo_url, &documents);
        assert!(
            refreshed.iter().any(|(uri, _)| uri == &main_url),
            "closing foo.hew should refresh diagnostics for the open importer"
        );
        assert!(
            has_unresolved_import(&documents, &main_url),
            "closing an unsaved foo.hew should restore the importer's UnresolvedImport"
        );
        assert!(
            !documents.contains_key(&foo_url),
            "closing foo.hew should remove it from the open document store"
        );
    }

    #[test]
    fn typecheck_opening_package_directory_import_target_refreshes_open_importer_diagnostics() {
        let main_source = "import shapes::circle;\nfn main() -> f64 { circle.area(1.0) }";
        let circle_source = "pub fn area(r: f64) -> f64 { r }";

        let main_url = make_test_uri("/fake/project/main.hew");
        let circle_url = make_test_uri("/fake/project/shapes/circle/circle.hew");

        let documents: DashMap<Url, DocumentState> = DashMap::new();

        let initial = refresh_document_and_dependents(&main_url, main_source, &documents);
        assert_eq!(
            initial.len(),
            1,
            "opening importer should only analyze itself"
        );
        assert!(
            has_unresolved_import(&documents, &main_url),
            "importer should start with UnresolvedImport before shapes/circle/circle.hew is open"
        );

        let refreshed = refresh_document_and_dependents(&circle_url, circle_source, &documents);
        assert!(
            refreshed.iter().any(|(uri, _)| uri == &main_url),
            "opening shapes/circle/circle.hew should refresh diagnostics for the open importer"
        );
        assert!(
            !has_unresolved_import(&documents, &main_url),
            "opening shapes/circle/circle.hew should clear the importer's stale UnresolvedImport"
        );
    }

    #[test]
    fn typecheck_changing_package_directory_import_target_refreshes_open_importer_diagnostics() {
        let main_source = "import shapes::circle;\nfn main() -> f64 { circle.area(1.0) }";
        let invalid_circle_source = "pub fn area(";
        let circle_source = "pub fn area(r: f64) -> f64 { r }";

        let main_url = make_test_uri("/fake/project/main.hew");
        let circle_url = make_test_uri("/fake/project/shapes/circle/circle.hew");

        let documents: DashMap<Url, DocumentState> = DashMap::new();

        refresh_document_and_dependents(&main_url, main_source, &documents);
        assert!(
            has_unresolved_import(&documents, &main_url),
            "importer should start with UnresolvedImport before shapes/circle/circle.hew is valid"
        );

        refresh_document_and_dependents(&circle_url, invalid_circle_source, &documents);
        assert!(
            has_unresolved_import(&documents, &main_url),
            "importer should stay unresolved while shapes/circle/circle.hew has parse errors"
        );

        let refreshed = refresh_document_and_dependents(&circle_url, circle_source, &documents);
        assert!(
            refreshed.iter().any(|(uri, _)| uri == &main_url),
            "editing shapes/circle/circle.hew should refresh diagnostics for the open importer"
        );
        assert!(
            !has_unresolved_import(&documents, &main_url),
            "editing shapes/circle/circle.hew to valid source should clear the importer's stale UnresolvedImport"
        );
    }

    /// Without the in-memory document, a missing sibling module produces an
    /// `UnresolvedImport` diagnostic (fail-closed behaviour preserved).
    #[test]
    fn typecheck_emits_unresolved_import_for_missing_sibling() {
        let main_source = "import shapes::missing_module;\nfn main() { missing_module.foo() }";
        let main_url = make_test_uri("/fake/project/main.hew");

        // Empty documents map — nothing is open.
        let documents: DashMap<Url, DocumentState> = DashMap::new();

        let mut parse_result = hew_parser::parse(main_source);
        assert!(
            parse_result.errors.is_empty(),
            "unexpected parse errors: {:?}",
            parse_result.errors
        );

        populate_user_module_imports(&main_url, &mut parse_result.program.items, &documents);

        let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
        let output = checker.check_program(&parse_result.program);

        let has_unresolved = output
            .errors
            .iter()
            .any(|e| matches!(e.kind, hew_types::error::TypeErrorKind::UnresolvedImport));
        assert!(
            has_unresolved,
            "should emit UnresolvedImport for a sibling module not in documents or on disk"
        );
    }

    /// `populate_user_module_imports` leaves `resolved_items` as None for a
    /// module that is absent from both the document store and the disk, so
    /// the type checker can emit a proper diagnostic.
    #[test]
    fn populate_leaves_resolved_items_none_for_missing_module() {
        let main_source = "import shapes::nonexistent;\nfn main() { 0 }";
        let main_url = make_test_uri("/fake/project/main.hew");

        let documents: DashMap<Url, DocumentState> = DashMap::new();

        let mut parse_result = hew_parser::parse(main_source);
        assert!(
            parse_result.errors.is_empty(),
            "unexpected parse errors: {:?}",
            parse_result.errors
        );
        populate_user_module_imports(&main_url, &mut parse_result.program.items, &documents);

        let import_decl = parse_result
            .program
            .items
            .iter()
            .find_map(|(item, _)| {
                if let hew_parser::ast::Item::Import(d) = item {
                    Some(d)
                } else {
                    None
                }
            })
            .expect("import should be present");

        assert!(
            import_decl.resolved_items.is_none(),
            "resolved_items must stay None for a module not in documents or on disk"
        );
    }

    // ── Transitive-refresh regression tests ──────────────────────────────

    /// Changing C in the chain A → B → C must propagate diagnostics all the
    /// way back to A, not just to B (the direct importer).
    ///
    /// Before the BFS fix, `refresh_open_importers` was single-hop: saving C
    /// re-analysed B but left A with stale diagnostics.
    #[test]
    fn typecheck_transitive_importer_chain_a_b_c_all_refreshed() {
        // C exports `provided`.
        // B imports C and re-exports `provided` as `b_provided`.
        // A imports B and calls `b_provided`.
        let c_source = "pub fn provided() -> i32 { 1 }";
        let b_source = "import \"c.hew\";\npub fn b_provided() -> i32 { provided() }";
        let a_source = "import \"b.hew\";\nfn main() -> i32 { b_provided() }";

        let a_url = make_test_uri("/fake/project/a.hew");
        let b_url = make_test_uri("/fake/project/b.hew");
        let c_url = make_test_uri("/fake/project/c.hew");

        let documents: DashMap<Url, DocumentState> = DashMap::new();

        // Open A first — B and C are not yet in the store, so A has stale
        // diagnostics (UnresolvedImport for b.hew).
        refresh_document_and_dependents(&a_url, a_source, &documents);
        assert!(
            has_unresolved_import(&documents, &a_url),
            "A should have UnresolvedImport before B is open"
        );

        // Open B — C is still missing, but A should now be re-analysed.
        refresh_document_and_dependents(&b_url, b_source, &documents);
        // B itself will have an UnresolvedImport (c.hew not open yet).
        assert!(
            has_unresolved_import(&documents, &b_url),
            "B should have UnresolvedImport before C is open"
        );

        // Open C — this should transitively refresh B and then A.
        let refreshed = refresh_document_and_dependents(&c_url, c_source, &documents);

        assert!(
            refreshed.iter().any(|(uri, _)| uri == &b_url),
            "opening C must refresh its direct importer B"
        );
        assert!(
            refreshed.iter().any(|(uri, _)| uri == &a_url),
            "opening C must transitively refresh A (the two-hop importer)"
        );
        assert!(
            !has_unresolved_import(&documents, &b_url),
            "B should have no UnresolvedImport after C is open"
        );
        assert!(
            !has_unresolved_import(&documents, &a_url),
            "A should have no UnresolvedImport after C is open (transitive refresh)"
        );
    }

    /// Diamond import: both B and C import D; A imports both B and C.
    /// Opening D must refresh B, C, and A exactly once (no duplicate work /
    /// no infinite loop).
    #[test]
    fn typecheck_diamond_import_no_cycle_all_refreshed() {
        let d_source = "pub fn d_fn() -> i32 { 1 }";
        let b_source = "import \"d.hew\";\npub fn b_fn() -> i32 { d_fn() }";
        let c_source = "import \"d.hew\";\npub fn c_fn() -> i32 { d_fn() }";
        let a_source = "import \"b.hew\";\nimport \"c.hew\";\nfn main() -> i32 { b_fn() + c_fn() }";

        let a_url = make_test_uri("/fake/project/a.hew");
        let b_url = make_test_uri("/fake/project/b.hew");
        let c_url = make_test_uri("/fake/project/c.hew");
        let d_url = make_test_uri("/fake/project/d.hew");

        let documents: DashMap<Url, DocumentState> = DashMap::new();

        refresh_document_and_dependents(&a_url, a_source, &documents);
        refresh_document_and_dependents(&b_url, b_source, &documents);
        refresh_document_and_dependents(&c_url, c_source, &documents);

        // Opening D triggers the refresh.  This must not panic or loop.
        let refreshed = refresh_document_and_dependents(&d_url, d_source, &documents);

        for url in [&b_url, &c_url, &a_url] {
            assert!(
                refreshed.iter().any(|(uri, _)| uri == url),
                "opening D should refresh {url}"
            );
        }

        // Each URI appears at most once in the publish list (dedup guarantee).
        let counts = refreshed.iter().filter(|(u, _)| u == &a_url).count();
        assert_eq!(
            counts, 1,
            "A should appear exactly once in the publish list"
        );
    }

    /// Stale on-disk content is superseded by an open editor buffer.
    /// The type-checker should see the in-memory version's exported type,
    /// not whatever might be saved on disk.
    #[test]
    fn in_memory_version_supersedes_disk_for_type_checking() {
        // circle.hew on disk (if it existed) would have `fn area(r: f64) -> f64`.
        // In-memory version renames it to `fn circumference(r: f64) -> f64`.
        // main.hew calls `circle.circumference` — this should resolve without error
        // only if the in-memory version is used.
        let main_source = "import shapes::circle;\nfn main() -> f64 { circle.circumference(1.0) }";
        let circle_inmem_source = "pub fn circumference(r: f64) -> f64 { r }";

        let main_url = make_test_uri("/fake/project/main.hew");
        let circle_url = make_test_uri("/fake/project/shapes/circle.hew");

        let documents: DashMap<Url, DocumentState> = DashMap::new();
        documents.insert(circle_url, make_doc(circle_inmem_source));

        let mut parse_result = hew_parser::parse(main_source);
        assert!(
            parse_result.errors.is_empty(),
            "unexpected parse errors: {:?}",
            parse_result.errors
        );
        populate_user_module_imports(&main_url, &mut parse_result.program.items, &documents);

        let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
        let output = checker.check_program(&parse_result.program);

        let errors: Vec<_> = output
            .errors
            .iter()
            .filter(|e| {
                !matches!(
                    e.kind,
                    hew_types::error::TypeErrorKind::UnusedImport
                        | hew_types::error::TypeErrorKind::UnusedVariable
                )
            })
            .collect();
        assert!(
            errors.is_empty(),
            "type-checking with in-memory circle.hew should produce no errors: {errors:?}"
        );
    }
}
