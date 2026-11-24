//! LSP server implementation for the Hew language.

mod analysis;
mod capabilities;
mod context;
mod convert;
pub use context::AnalysisContextParams;
mod generation;
mod handlers;
mod hierarchy;
mod navigation;
mod project_rename;
mod test_runner;
mod testing;
#[cfg(test)]
mod tests;
mod uri;
mod workspace;

// Items used by the LanguageServer impl handlers and sibling submodules.
use self::analysis::{close_document_and_dependents, refresh_document_and_dependents};
use self::capabilities::{
    build_server_capabilities, modifier_bit, GENERATE_COMMAND, REMOVE_UNUSED_IMPORTS_KIND,
    RUN_TEST_COMMAND,
};
use self::convert::{analysis_tokens_to_lsp, symbol_info_to_doc_symbol, to_lsp_completion};
use self::hierarchy::{
    collect_subtypes, collect_supertypes, find_callable_at, find_incoming_calls,
    find_outgoing_calls, find_type_hierarchy_item,
};
use self::navigation::{
    build_document_links, build_reference_locations, collect_import_items,
    find_cross_file_definition, find_definition_in_ast, find_stdlib_definition,
};
use self::test_runner::{hew_cli_executable, with_test_diagnostics};
use self::uri::{source_file_key, FileUriExt};
use self::workspace::{collect_project_workspace_symbols, test_inventory};

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
#[cfg(test)]
use std::str::FromStr;
use std::sync::{Arc, RwLock};

use dashmap::DashMap;
use hew_analysis::util::{non_empty, offset_to_line_col, word_at_offset};
use hew_parser::ast::Span;
use hew_parser::ParseResult;
use hew_types::TypeCheckOutput;
use serde_json::{json, Value};
use tower_lsp_server::jsonrpc::Result;
use tower_lsp_server::ls_types::request::{
    GotoImplementationParams, GotoImplementationResponse, GotoTypeDefinitionParams,
    GotoTypeDefinitionResponse,
};
use tower_lsp_server::ls_types::{
    CallHierarchyIncomingCall, CallHierarchyIncomingCallsParams, CallHierarchyItem,
    CallHierarchyOutgoingCall, CallHierarchyOutgoingCallsParams, CallHierarchyPrepareParams,
    CodeActionParams, CodeActionResponse, CodeLens, CodeLensParams, CompletionParams,
    CompletionResponse, Diagnostic, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, DocumentDiagnosticParams, DocumentDiagnosticReportResult,
    DocumentFormattingParams, DocumentHighlight, DocumentHighlightParams, DocumentLink,
    DocumentLinkParams, DocumentSymbolParams, DocumentSymbolResponse, ExecuteCommandParams,
    FoldingRange, FoldingRangeParams, GotoDefinitionParams, GotoDefinitionResponse, Hover,
    HoverParams, InitializeParams, InitializeResult, InitializedParams, InlayHint, InlayHintParams,
    Location, MessageType, Position, PrepareRenameResponse, Range, ReferenceParams, RenameParams,
    SelectionRange, SelectionRangeParams, SemanticTokensParams, SemanticTokensResult,
    SignatureHelp, SignatureHelpParams, TextDocumentIdentifier, TextEdit, TypeHierarchyItem,
    TypeHierarchyPrepareParams, TypeHierarchySubtypesParams, TypeHierarchySupertypesParams,
    Uri as Url, WorkspaceEdit, WorkspaceSymbolParams, WorkspaceSymbolResponse,
};
use tower_lsp_server::{Client, LanguageServer};

/// Omitting position requests the complete expanded document. Existing clients
/// receive text; metadata is opt-in to preserve the original request contract.
#[derive(Debug, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ExpandMacroParams {
    text_document: TextDocumentIdentifier,
    position: Option<Position>,
    #[serde(default)]
    include_metadata: bool,
}

/// Parameters for the `hew/tests` editor request. Omitting the document lists
/// tests across the open workspace.
#[derive(Debug, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TestsParams {
    text_document: Option<TextDocumentIdentifier>,
}

// ── Document state ───────────────────────────────────────────────────

#[derive(Debug)]
struct DocumentState {
    expanded_program: Option<Box<hew_parser::ast::Program>>,
    source: String,
    /// Byte offsets of each line start, computed once per edit.
    line_offsets: Vec<usize>,
    parse_result: ParseResult,
    type_output: Option<TypeCheckOutput>,
    /// Files the resolved module graph reaches, so an edit to one of them
    /// re-analyses this document. `None` when the frontend stopped before
    /// resolution completed, which leaves the dependency set unknown.
    dependency_uris: Option<Vec<Url>>,
    diagnostics_by_uri: DiagnosticMap,
}

type DiagnosticMap = HashMap<Url, Vec<Diagnostic>>;

/// Authored text arrives before debounced analysis. Rename snapshots this map
/// so pending edits and their client versions are never replaced by old facts.
#[derive(Debug, Clone, PartialEq, Eq)]
struct OpenDocument {
    source: String,
    version: i32,
}

/// Keep the test parsing entry point over `Uri`'s `FromStr` implementation.
#[cfg(test)]
pub(crate) trait UriParse: Sized + FromStr {
    fn parse(input: &str) -> std::result::Result<Self, <Self as FromStr>::Err> {
        Self::from_str(input)
    }
}

#[cfg(test)]
impl UriParse for Url {}

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

fn internal_error(message: impl Into<String>) -> tower_lsp_server::jsonrpc::Error {
    use tower_lsp_server::jsonrpc::{Error, ErrorCode};

    Error {
        code: ErrorCode::InternalError,
        message: message.into().into(),
        data: None,
    }
}

// ── Server ───────────────────────────────────────────────────────────

/// Per-document debounce state: the generation counter incremented on each
/// keystroke.  A spawned analysis task compares against this before doing
/// work — if the value has changed since the task was created the edit has
/// been superseded and the task exits without publishing stale diagnostics.
type AnalysisVersions = Arc<DashMap<Url, u64>>;

/// Debounce window: how long an analysis task waits before checking whether
/// it is still the most recent edit.  100 ms is short enough to feel live on
/// typical files while coalescing rapid-fire keystrokes into a single run.
const DEBOUNCE_MS: u64 = 100;

/// Hew language server providing IDE features via LSP.
#[derive(Debug)]
pub struct HewLanguageServer {
    client: Client,
    /// Document states shared with spawned analysis tasks via `Arc`.
    documents: Arc<DashMap<Url, DocumentState>>,
    open_documents: Arc<RwLock<HashMap<Url, OpenDocument>>>,
    /// Advanced under the `open_documents` write lock on every editor change,
    /// so a running rename can recheck its snapshot only after an edit.
    open_generation: Arc<std::sync::atomic::AtomicU64>,
    analysis_contexts: RwLock<context::AnalysisContexts>,
    rename_document_changes: RwLock<bool>,
    watched_files_supported: RwLock<bool>,
    diagnostic_refresh_supported: RwLock<bool>,
    workspace_roots: RwLock<Vec<PathBuf>>,
    /// One CPU-heavy rename job per server, including cancelled requests whose
    /// blocking computation is still running.
    rename_jobs: Arc<tokio::sync::Semaphore>,
    /// Per-URI generation counters used by the debounced analysis path.
    analysis_versions: AnalysisVersions,
    /// Completed generations include refreshes of all importing documents.
    completed_analysis_versions: AnalysisVersions,
    /// Serialize source scheduling, diagnostic publication, close and pull snapshots.
    analysis_jobs: Arc<tokio::sync::Mutex<()>>,
    diagnostic_reports: RwLock<handlers::editor::DiagnosticReports>,
    /// Failures from the CLI test stream, kept separate from source diagnostics.
    test_diagnostics: Arc<DashMap<Url, Vec<Diagnostic>>>,
    /// A failed deterministic test's last seed, keyed by its exact selector.
    test_seeds: Arc<DashMap<String, String>>,
    test_run_versions: Arc<DashMap<Url, u64>>,
    /// Extra package-search directories passed via `--pkg-path` (or the
    /// `hew.pkgPath` vscode setting).  Appended to the default search roots
    /// so that `hew check --pkg-path DIR` and editor diagnostics agree on
    /// which `hew::…` imports resolve.
    extra_pkg_paths: Vec<PathBuf>,
}

impl HewLanguageServer {
    /// Create a new language server with the given LSP client.
    ///
    /// Use the same optional package search directory as `hew check --pkg-path`.
    #[must_use]
    pub fn new_with_options(client: Client, extra_pkg_paths: Vec<PathBuf>) -> Self {
        Self {
            client,
            documents: Arc::new(DashMap::new()),
            open_documents: Arc::new(RwLock::new(HashMap::new())),
            open_generation: Arc::new(std::sync::atomic::AtomicU64::new(0)),
            analysis_contexts: RwLock::new(HashMap::new()),
            rename_document_changes: RwLock::new(false),
            watched_files_supported: RwLock::new(false),
            diagnostic_refresh_supported: RwLock::new(false),
            workspace_roots: RwLock::new(Vec::new()),
            rename_jobs: Arc::new(tokio::sync::Semaphore::new(1)),
            analysis_versions: Arc::new(DashMap::new()),
            completed_analysis_versions: Arc::new(DashMap::new()),
            analysis_jobs: Arc::new(tokio::sync::Mutex::new(())),
            diagnostic_reports: RwLock::new(handlers::editor::DiagnosticReports::default()),
            test_diagnostics: Arc::new(DashMap::new()),
            test_seeds: Arc::new(DashMap::new()),
            test_run_versions: Arc::new(DashMap::new()),
            extra_pkg_paths,
        }
    }

    /// Create a new language server with no extra package paths.
    ///
    /// Equivalent to `new_with_options(client, vec![])`.  Satisfies the
    /// `tower_lsp::LspService::new(HewLanguageServer::new)` closure contract.
    #[must_use]
    pub fn new(client: Client) -> Self {
        Self::new_with_options(client, vec![])
    }

    /// Record an editor change; the caller holds the `open_documents` write lock.
    fn advance_open_generation(&self) {
        self.open_generation
            .fetch_add(1, std::sync::atomic::Ordering::Release);
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
                .to_checked_file_path()
                .and_then(|path| path.parent().map(Path::to_path_buf))
        })
    }

    /// Show the innermost macro expansion at the requested source position.
    ///
    /// # Errors
    ///
    /// The protocol requires a result; absent documents and expansions return null.
    #[expect(
        clippy::unused_async,
        reason = "custom_method requires an async handler"
    )]
    pub async fn expand_macro(&self, params: ExpandMacroParams) -> Result<Option<Value>> {
        let Some(doc) = self.documents.get(&params.text_document.uri) else {
            return Ok(None);
        };
        let Some(program) = doc.expanded_program.as_deref() else {
            return Ok(None);
        };
        let Some(path) = params.text_document.uri.to_checked_file_path() else {
            return Ok(None);
        };
        let path = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
        let offset = params
            .position
            .map(|position| position_to_offset(&doc.source, &doc.line_offsets, position));
        if params.include_metadata {
            Ok(hew_analysis::expansion::inspect(program, &path, offset).map(|view| json!(view)))
        } else {
            Ok(hew_analysis::expansion::render(program, &path, offset).map(Value::String))
        }
    }

    /// Return stable identities and ranges from the same discovery used by
    /// `hew test`. An omitted document requests the workspace inventory.
    ///
    /// # Errors
    ///
    /// The LSP request signature returns `Result`; discovery itself cannot fail.
    #[expect(
        clippy::unused_async,
        reason = "custom_method requires an async handler"
    )]
    pub async fn tests(&self, params: TestsParams) -> Result<Value> {
        let roots = self
            .workspace_roots
            .read()
            .map_or_else(|_| Vec::new(), |roots| roots.clone());
        Ok(Value::Array(test_inventory(
            &self.documents,
            &roots,
            params.text_document.as_ref().map(|document| &document.uri),
        )))
    }

    /// Re-lex, re-parse, and re-typecheck the document and any open importers,
    /// then publish diagnostics.
    ///
    /// Rapid successive edits are coalesced: a short debounce window lets the
    /// caller issue multiple `reanalyze` calls before the first analysis task
    /// runs.  Each call increments a per-URI generation counter; the spawned
    /// task checks that counter after sleeping and exits without doing work if a
    /// newer edit has arrived in the meantime.
    ///
    /// The function itself is synchronous — analysis runs in a `tokio::spawn`
    /// task so the handler path returns immediately.
    fn reanalyze(&self, uri: &Url, source: &str) {
        // Callers hold analysis_jobs while scheduling. Watchers and generation
        // refreshes must preserve a newer authored buffer over analyzed text.
        let source = self
            .open_documents
            .read()
            .ok()
            .and_then(|open| open.get(uri).map(|document| document.source.clone()))
            .unwrap_or_else(|| source.to_owned());
        self.test_diagnostics.remove(uri);
        if let Some(path) = uri.to_checked_file_path() {
            let prefix = format!("{}::", workspace::normalize_workspace_path(&path).display());
            self.test_seeds
                .retain(|selector, _| !selector.starts_with(&prefix));
        }
        // Increment the generation counter for this URI and capture the new value.
        let version = {
            let mut entry = self.analysis_versions.entry(uri.clone()).or_insert(0);
            *entry += 1;
            *entry
        };

        // Clone the shared state the spawned task needs.
        let client = self.client.clone();
        let documents = Arc::clone(&self.documents);
        let test_diagnostics = Arc::clone(&self.test_diagnostics);
        let versions = Arc::clone(&self.analysis_versions);
        let completed = Arc::clone(&self.completed_analysis_versions);
        let analysis_jobs = Arc::clone(&self.analysis_jobs);
        let refresh_supported = self
            .diagnostic_refresh_supported
            .read()
            .is_ok_and(|supported| *supported);
        let uri = uri.clone();
        let extra_pkg_paths = self.extra_pkg_paths.clone();

        tokio::spawn(async move {
            // Debounce: wait for the window to expire before running the pipeline.
            tokio::time::sleep(tokio::time::Duration::from_millis(DEBOUNCE_MS)).await;

            let analysis = analysis_jobs.lock().await;
            // Bail if a newer edit has already superseded this one.
            if versions.get(&uri).map(|v| *v) != Some(version) {
                return;
            }

            let published =
                refresh_document_and_dependents(&uri, &source, &documents, &extra_pkg_paths);
            completed.insert(uri.clone(), version);
            for (updated_uri, diagnostics) in with_test_diagnostics(published, &test_diagnostics) {
                client
                    .publish_diagnostics(updated_uri, diagnostics, None)
                    .await;
            }
            drop(analysis);
            if refresh_supported {
                let _ = client.workspace_diagnostic_refresh().await;
            }
        });
    }

    async fn refresh_pull_diagnostics(&self) {
        let supported = self
            .diagnostic_refresh_supported
            .read()
            .is_ok_and(|supported| *supported);
        if supported {
            let _ = self.client.workspace_diagnostic_refresh().await;
        }
    }
}

impl LanguageServer for HewLanguageServer {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        handlers::text_sync::initialize(self, &params)
    }

    async fn initialized(&self, _: InitializedParams) {
        handlers::text_sync::initialized(self).await;
    }

    async fn shutdown(&self) -> Result<()> {
        handlers::text_sync::shutdown(self);
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        handlers::text_sync::did_open(self, params).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        handlers::text_sync::did_change(self, params).await;
    }

    async fn did_change_watched_files(
        &self,
        params: tower_lsp_server::ls_types::DidChangeWatchedFilesParams,
    ) {
        let changed: HashSet<_> = params.changes.into_iter().map(|event| event.uri).collect();
        let changed_paths = changed
            .iter()
            .filter_map(uri::FileUriExt::to_checked_file_path)
            .map(|path| workspace::normalize_workspace_path(&path))
            .collect::<HashSet<_>>();
        let changed_keys: HashSet<_> = changed.iter().map(source_file_key).collect();
        let _analysis = self.analysis_jobs.lock().await;
        let dependents: Vec<_> = self
            .documents
            .iter()
            .filter_map(|document| {
                (document.dependency_uris.as_ref().is_some_and(|inputs| {
                    inputs
                        .iter()
                        .any(|uri| changed_keys.contains(&source_file_key(uri)))
                }) || document
                    .key()
                    .to_checked_file_path()
                    .is_some_and(|path| generation::affected_by(&path, &changed_paths)))
                .then(|| (document.key().clone(), document.source.clone()))
            })
            .collect();
        for (uri, source) in dependents {
            self.reanalyze(&uri, &source);
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        handlers::text_sync::did_close(self, params).await;
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        Ok(Some(handlers::language_features::completion(self, &params)))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        Ok(handlers::language_features::hover(self, &params))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        Ok(handlers::navigation::goto_definition(self, &params))
    }

    async fn goto_type_definition(
        &self,
        params: GotoTypeDefinitionParams,
    ) -> Result<Option<GotoTypeDefinitionResponse>> {
        Ok(handlers::editor::type_definition(self, &params))
    }

    async fn goto_implementation(
        &self,
        params: GotoImplementationParams,
    ) -> Result<Option<GotoImplementationResponse>> {
        Ok(handlers::editor::implementations(self, &params))
    }

    async fn document_highlight(
        &self,
        params: DocumentHighlightParams,
    ) -> Result<Option<Vec<DocumentHighlight>>> {
        Ok(handlers::editor::document_highlights(self, &params))
    }

    async fn selection_range(
        &self,
        params: SelectionRangeParams,
    ) -> Result<Option<Vec<SelectionRange>>> {
        Ok(handlers::editor::selection_ranges(self, &params))
    }

    async fn diagnostic(
        &self,
        params: DocumentDiagnosticParams,
    ) -> Result<DocumentDiagnosticReportResult> {
        handlers::editor::diagnostic(self, &params).await
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        Ok(handlers::language_features::document_symbol(self, &params))
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        handlers::language_features::semantic_tokens_full(self, &params)
    }

    async fn document_link(&self, params: DocumentLinkParams) -> Result<Option<Vec<DocumentLink>>> {
        Ok(handlers::language_features::document_link(self, &params))
    }

    async fn prepare_type_hierarchy(
        &self,
        params: TypeHierarchyPrepareParams,
    ) -> Result<Option<Vec<TypeHierarchyItem>>> {
        Ok(handlers::hierarchy::prepare_type_hierarchy(self, &params))
    }

    async fn supertypes(
        &self,
        params: TypeHierarchySupertypesParams,
    ) -> Result<Option<Vec<TypeHierarchyItem>>> {
        Ok(handlers::hierarchy::supertypes(self, &params))
    }

    async fn subtypes(
        &self,
        params: TypeHierarchySubtypesParams,
    ) -> Result<Option<Vec<TypeHierarchyItem>>> {
        Ok(handlers::hierarchy::subtypes(self, &params))
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        Ok(handlers::navigation::references(self, &params))
    }

    async fn prepare_rename(
        &self,
        params: tower_lsp_server::ls_types::TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        let _analysis = self.analysis_jobs.lock().await;
        Ok(handlers::navigation::prepare_rename(self, &params))
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        handlers::navigation::rename(self, &params).await
    }

    async fn prepare_call_hierarchy(
        &self,
        params: CallHierarchyPrepareParams,
    ) -> Result<Option<Vec<CallHierarchyItem>>> {
        Ok(handlers::hierarchy::prepare_call_hierarchy(self, &params))
    }

    async fn incoming_calls(
        &self,
        params: CallHierarchyIncomingCallsParams,
    ) -> Result<Option<Vec<CallHierarchyIncomingCall>>> {
        Ok(handlers::hierarchy::incoming_calls(self, &params))
    }

    async fn outgoing_calls(
        &self,
        params: CallHierarchyOutgoingCallsParams,
    ) -> Result<Option<Vec<CallHierarchyOutgoingCall>>> {
        Ok(handlers::hierarchy::outgoing_calls(self, &params))
    }

    async fn code_lens(&self, params: CodeLensParams) -> Result<Option<Vec<CodeLens>>> {
        Ok(handlers::workspace::code_lens(self, &params))
    }

    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<WorkspaceSymbolResponse>> {
        Ok(handlers::workspace::symbol(self, &params).map(Into::into))
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        Ok(handlers::language_features::inlay_hint(self, &params))
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        Ok(handlers::language_features::signature_help(self, &params))
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        Ok(Some(handlers::language_features::code_action(
            self, &params,
        )))
    }

    async fn execute_command(&self, params: ExecuteCommandParams) -> Result<Option<Value>> {
        handlers::workspace::execute_command(self, params).await
    }

    async fn folding_range(&self, params: FoldingRangeParams) -> Result<Option<Vec<FoldingRange>>> {
        Ok(handlers::language_features::folding_range(self, &params))
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        match handlers::language_features::formatting(self, &params) {
            Ok(edits) => Ok(edits),
            Err(message) => {
                self.client
                    .show_message(MessageType::WARNING, message)
                    .await;
                Ok(None)
            }
        }
    }
}
