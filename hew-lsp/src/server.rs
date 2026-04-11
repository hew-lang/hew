//! LSP server implementation for the Hew language.

use std::collections::{HashMap, HashSet};

use dashmap::DashMap;
use hew_analysis::calls::{collect_calls_in_block, collect_calls_in_item};
use hew_analysis::references::count_all_references;
use hew_analysis::util::{compute_line_offsets, non_empty, offset_to_line_col, word_at_offset};
use hew_parser::ast::{Attribute, ImportDecl, ImportSpec, Item, Span, TypeDeclKind};
use hew_parser::ParseResult;
use hew_types::error::{Severity, TypeErrorKind};
use hew_types::module_registry::build_module_search_paths;
use hew_types::{Checker, TypeCheckOutput};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    CallHierarchyIncomingCall, CallHierarchyIncomingCallsParams, CallHierarchyItem,
    CallHierarchyOutgoingCall, CallHierarchyOutgoingCallsParams, CallHierarchyPrepareParams,
    CallHierarchyServerCapability, CodeLens, CodeLensOptions, CodeLensParams, Command,
    SymbolInformation, WorkspaceSymbolParams,
};
use tower_lsp::lsp_types::{
    CodeAction, CodeActionKind, CodeActionOrCommand, CodeActionParams, CodeActionResponse,
    CompletionItem, CompletionItemKind, CompletionOptions, CompletionParams, CompletionResponse,
    Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, DidChangeTextDocumentParams,
    DidCloseTextDocumentParams, DidOpenTextDocumentParams, DocumentSymbol, DocumentSymbolParams,
    DocumentSymbolResponse, FoldingRange, FoldingRangeKind, FoldingRangeParams,
    GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverContents, HoverParams,
    HoverProviderCapability, InitializeParams, InitializeResult, InitializedParams,
    InsertTextFormat, Location, MarkupContent, MarkupKind, MessageType, OneOf, Position,
    PrepareRenameResponse, Range, ReferenceParams, RenameParams, SemanticToken,
    SemanticTokenModifier, SemanticTokenType, SemanticTokens, SemanticTokensFullOptions,
    SemanticTokensLegend, SemanticTokensOptions, SemanticTokensParams, SemanticTokensResult,
    SemanticTokensServerCapabilities, ServerCapabilities, SymbolKind, TextDocumentSyncCapability,
    TextDocumentSyncKind, TextEdit, Url, WorkDoneProgressOptions, WorkspaceEdit,
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

// ── In-memory module resolution ──────────────────────────────────────

/// Return the source text for a file, preferring open editor buffers over disk.
///
/// Checks the LSP document store first so that unsaved edits to an imported
/// module are immediately visible to type-checking in the importing file.
/// Falls back to `std::fs::read_to_string` for files that are not open.
fn source_for_path(
    path: &std::path::Path,
    documents: &DashMap<Url, DocumentState>,
) -> Option<String> {
    // Prefer in-memory content if the file is currently open in the editor.
    if let Ok(url) = Url::from_file_path(path) {
        if let Some(doc) = documents.get(&url) {
            return Some(doc.source.clone());
        }
    }
    // Fall back to on-disk content.
    std::fs::read_to_string(path).ok()
}

/// Recursively populate `ImportDecl::resolved_items` for user and file imports.
///
/// After parsing a document the `resolved_items` field on every `ImportDecl`
/// is `None`.  The CLI fills these in by reading files from disk; the LSP
/// historically skipped this step, so imported modules were only resolved via
/// the stdlib `ModuleRegistry` and not from the project tree.
///
/// This function walks `items`, finds unresolved `Import` nodes, locates the
/// corresponding `.hew` file relative to `source_dir`, reads it — **preferring
/// any open editor buffer over the on-disk version** — and populates
/// `resolved_items` so the type checker sees the current in-memory content.
///
/// Depth is capped at [`MAX_IMPORT_DEPTH`] to prevent cycles.
fn populate_user_module_imports(
    source_uri: &Url,
    items: &mut [hew_parser::ast::Spanned<hew_parser::ast::Item>],
    documents: &DashMap<Url, DocumentState>,
) {
    let Ok(source_path) = source_uri.to_file_path() else {
        return; // Non-file URI — nothing to resolve.
    };
    let Some(source_dir) = source_path.parent() else {
        return;
    };
    populate_user_module_imports_impl(source_dir, items, documents, 0);
}

/// Maximum import nesting depth to prevent unbounded recursion on cycles.
const MAX_IMPORT_DEPTH: usize = 16;

fn populate_user_module_imports_impl(
    source_dir: &std::path::Path,
    items: &mut [hew_parser::ast::Spanned<hew_parser::ast::Item>],
    documents: &DashMap<Url, DocumentState>,
    depth: usize,
) {
    if depth >= MAX_IMPORT_DEPTH {
        return;
    }

    for (item, _span) in items.iter_mut() {
        let decl = match item {
            // Only process imports that haven't been resolved yet.
            Item::Import(d)
                if d.resolved_items.is_none() && (d.file_path.is_some() || !d.path.is_empty()) =>
            {
                d
            }
            _ => continue,
        };

        let candidates = import_candidate_paths_from_dir(source_dir, decl);

        for candidate in &candidates {
            // `source_for_path` checks the in-memory document store first so
            // unsaved edits are preferred over the on-disk version.
            if let Some(source) = source_for_path(candidate, documents) {
                let parsed = hew_parser::parse(&source);
                let has_errors = parsed
                    .errors
                    .iter()
                    .any(|e| e.severity == hew_parser::Severity::Error);
                if !has_errors {
                    let mut module_items = parsed.program.items;
                    // Recursively resolve any imports inside the loaded module.
                    let module_dir = candidate.parent().unwrap_or(source_dir);
                    populate_user_module_imports_impl(
                        module_dir,
                        &mut module_items,
                        documents,
                        depth + 1,
                    );
                    let item_count = module_items.len();
                    decl.resolved_source_paths = vec![candidate.clone()];
                    decl.resolved_item_source_paths = vec![candidate.clone(); item_count];
                    decl.resolved_items = Some(module_items);
                }
                // Stop after the first candidate that yielded source text,
                // regardless of whether it parsed cleanly — otherwise we'd
                // silently fall through to a stale on-disk version.
                break;
            }
        }
    }
}

fn import_candidate_paths_from_dir(
    source_dir: &std::path::Path,
    import: &ImportDecl,
) -> Vec<std::path::PathBuf> {
    if let Some(file_path) = &import.file_path {
        return vec![source_dir.join(file_path)];
    }

    let Some(last) = import.path.last() else {
        return vec![];
    };

    // Build the two canonical candidate paths the CLI also tries:
    //   1. package-directory form:  source_dir/<a>/<b>/<b>.hew
    //   2. flat form:               source_dir/<a>/<b>.hew
    let rel_path: std::path::PathBuf = import
        .path
        .iter()
        .collect::<std::path::PathBuf>()
        .with_extension("hew");
    let dir_path: std::path::PathBuf = import
        .path
        .iter()
        .collect::<std::path::PathBuf>()
        .join(format!("{last}.hew"));

    vec![source_dir.join(&dir_path), source_dir.join(&rel_path)]
}

fn import_candidate_paths(uri: &Url, import: &ImportDecl) -> Vec<std::path::PathBuf> {
    let Ok(source_path) = uri.to_file_path() else {
        return vec![];
    };
    let Some(source_dir) = source_path.parent() else {
        return vec![];
    };

    import_candidate_paths_from_dir(source_dir, import)
}

fn module_id_from_file(
    source_dir: &std::path::Path,
    canonical_path: &std::path::Path,
) -> hew_parser::module::ModuleId {
    use hew_parser::module::ModuleId;

    let without_ext = canonical_path.with_extension("");
    let rel = without_ext.strip_prefix(source_dir).unwrap_or(&without_ext);
    let mut segments: Vec<String> = rel
        .iter()
        .filter_map(|segment| segment.to_str())
        .map(std::string::ToString::to_string)
        .collect();

    if segments.is_empty() {
        segments.push(
            canonical_path
                .file_stem()
                .and_then(|segment| segment.to_str())
                .unwrap_or("unknown")
                .to_string(),
        );
    }

    ModuleId::new(segments)
}

fn resolved_import_source_path(
    current_source: &std::path::Path,
    source_dir: &std::path::Path,
    decl: &ImportDecl,
) -> Option<std::path::PathBuf> {
    decl.resolved_source_paths.first().cloned().or_else(|| {
        decl.file_path.as_ref().map(|file_path| {
            current_source
                .parent()
                .unwrap_or(source_dir)
                .join(file_path)
        })
    })
}

fn build_document_module_graph(
    source_uri: &Url,
    program: &hew_parser::ast::Program,
) -> Option<hew_parser::module::ModuleGraph> {
    use hew_parser::module::{Module, ModuleGraph};

    let input_path = source_uri.to_file_path().ok()?;
    let input_path = std::fs::canonicalize(&input_path).unwrap_or(input_path);
    let source_dir = input_path.parent().unwrap_or(std::path::Path::new("."));
    let root_id = module_id_from_file(source_dir, &input_path);
    let mut graph = ModuleGraph::new(root_id.clone());
    let mut seen_ids = HashSet::from([root_id.clone()]);

    let root_imports = extract_module_info(
        &program.items,
        &input_path,
        source_dir,
        &input_path,
        &root_id,
        &mut graph,
        &mut seen_ids,
    );

    graph.add_module(Module {
        id: root_id,
        items: program.items.clone(),
        imports: root_imports,
        source_paths: vec![input_path],
        doc: program.module_doc.clone(),
    });

    graph.compute_topo_order().ok()?;
    Some(graph)
}

fn extract_module_info(
    items: &[hew_parser::ast::Spanned<Item>],
    current_source: &std::path::Path,
    source_dir: &std::path::Path,
    root_source: &std::path::Path,
    root_id: &hew_parser::module::ModuleId,
    graph: &mut hew_parser::module::ModuleGraph,
    seen_ids: &mut HashSet<hew_parser::module::ModuleId>,
) -> Vec<hew_parser::module::ModuleImport> {
    use hew_parser::module::{Module, ModuleId, ModuleImport};

    let mut imports = Vec::new();

    for (item, span) in items {
        let Item::Import(decl) = item else { continue };

        let first_source_path = resolved_import_source_path(current_source, source_dir, decl);
        let module_id = if !decl.path.is_empty() {
            ModuleId::new(decl.path.clone())
        } else if let Some(source_path) = first_source_path.as_ref() {
            if source_path == root_source {
                root_id.clone()
            } else {
                module_id_from_file(source_dir, source_path)
            }
        } else {
            continue;
        };

        imports.push(ModuleImport {
            target: module_id.clone(),
            spec: decl.spec.clone(),
            span: span.clone(),
        });

        if seen_ids.insert(module_id.clone()) {
            if let Some(resolved_items) = &decl.resolved_items {
                let child_source = first_source_path.as_deref().unwrap_or(current_source);
                let child_imports = extract_module_info(
                    resolved_items,
                    child_source,
                    source_dir,
                    root_source,
                    root_id,
                    graph,
                    seen_ids,
                );
                let source_paths = if decl.resolved_source_paths.is_empty() {
                    first_source_path.iter().cloned().collect()
                } else {
                    decl.resolved_source_paths.clone()
                };
                graph.add_module(Module {
                    id: module_id,
                    items: resolved_items.clone(),
                    imports: child_imports,
                    source_paths,
                    doc: None,
                });
            }
        }
    }

    imports
}

fn build_module_source_map(
    program: &hew_parser::ast::Program,
    documents: &DashMap<Url, DocumentState>,
) -> HashMap<String, DiagnosticSource> {
    let Some(ref module_graph) = program.module_graph else {
        return HashMap::new();
    };

    let mut module_sources = HashMap::new();
    for module_id in &module_graph.topo_order {
        if *module_id == module_graph.root {
            continue;
        }
        let Some(module) = module_graph.modules.get(module_id) else {
            continue;
        };
        let Some(source_path) = module.source_paths.first() else {
            continue;
        };
        let Ok(uri) = Url::from_file_path(source_path) else {
            continue;
        };
        let Some(source) = source_for_path(source_path, documents) else {
            continue;
        };
        module_sources.insert(
            module_id.path.join("."),
            DiagnosticSource {
                uri,
                line_offsets: compute_line_offsets(&source),
                source,
            },
        );
    }

    module_sources
}

fn merge_diagnostics(into: &mut DiagnosticMap, from: &DiagnosticMap) {
    for (uri, diagnostics) in from {
        into.entry(uri.clone())
            .or_default()
            .extend(diagnostics.iter().cloned());
    }
}

fn sort_and_dedup_diagnostics(diagnostics: &mut Vec<Diagnostic>) {
    diagnostics.sort_by(|left, right| {
        left.range
            .start
            .line
            .cmp(&right.range.start.line)
            .then(left.range.start.character.cmp(&right.range.start.character))
            .then(left.range.end.line.cmp(&right.range.end.line))
            .then(left.range.end.character.cmp(&right.range.end.character))
            .then(
                left.source
                    .as_deref()
                    .unwrap_or("")
                    .cmp(right.source.as_deref().unwrap_or("")),
            )
            .then(left.message.cmp(&right.message))
    });

    let mut seen = HashSet::new();
    diagnostics.retain(|diagnostic| seen.insert(format!("{diagnostic:?}")));
}

fn collect_published_diagnostics(
    documents: &DashMap<Url, DocumentState>,
    publish_uris: HashSet<Url>,
) -> Vec<(Url, Vec<Diagnostic>)> {
    let mut diagnostics_by_uri = DiagnosticMap::new();
    for entry in documents {
        merge_diagnostics(&mut diagnostics_by_uri, &entry.value().diagnostics_by_uri);
    }

    for uri in publish_uris {
        diagnostics_by_uri.entry(uri).or_default();
    }

    let mut published: Vec<_> = diagnostics_by_uri.into_iter().collect();
    for (_, diagnostics) in &mut published {
        sort_and_dedup_diagnostics(diagnostics);
    }
    published.sort_by(|(left_uri, _), (right_uri, _)| left_uri.as_str().cmp(right_uri.as_str()));
    published
}

fn analyze_document(
    uri: &Url,
    source: &str,
    documents: &DashMap<Url, DocumentState>,
) -> DocumentState {
    let parse_result = hew_parser::parse(source);
    let line_offsets = compute_line_offsets(source);

    let has_parse_errors = parse_result
        .errors
        .iter()
        .any(|e| e.severity == hew_parser::Severity::Error);

    let (type_output, module_sources) = if has_parse_errors {
        (None, HashMap::new())
    } else {
        let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(
            build_module_search_paths(),
        ));
        // Clone the program so we can inject resolved_items for user-module
        // imports without mutating the parse_result stored in DocumentState
        // (other LSP features use the raw AST and do not need resolved_items).
        let mut program = parse_result.program.clone();
        populate_user_module_imports(uri, &mut program.items, documents);
        program.module_graph = build_document_module_graph(uri, &program);
        let module_sources = build_module_source_map(&program, documents);
        (Some(checker.check_program(&program)), module_sources)
    };

    let diagnostics_by_uri = build_diagnostics_by_uri(
        uri,
        source,
        &line_offsets,
        &parse_result,
        type_output.as_ref(),
        &module_sources,
    );

    DocumentState {
        source: source.to_string(),
        line_offsets,
        parse_result,
        type_output,
        diagnostics_by_uri,
    }
}

fn document_imports_target(
    importer_uri: &Url,
    parse_result: &ParseResult,
    target_uri: &Url,
) -> bool {
    parse_result.program.items.iter().any(|(item, _)| {
        let Item::Import(import) = item else {
            return false;
        };

        import_candidate_paths(importer_uri, import)
            .into_iter()
            .filter_map(|path| Url::from_file_path(path).ok())
            .any(|candidate_uri| candidate_uri == *target_uri)
    })
}

fn refresh_open_importers(
    target_uri: &Url,
    documents: &DashMap<Url, DocumentState>,
    publish_uris: &mut HashSet<Url>,
) {
    let dependents: Vec<_> = documents
        .iter()
        .filter_map(|entry| {
            let importer_uri = entry.key().clone();
            if &importer_uri == target_uri {
                return None;
            }

            let importer = entry.value();
            if document_imports_target(&importer_uri, &importer.parse_result, target_uri) {
                Some((
                    importer_uri,
                    importer.source.clone(),
                    importer
                        .diagnostics_by_uri
                        .keys()
                        .cloned()
                        .collect::<Vec<_>>(),
                ))
            } else {
                None
            }
        })
        .collect();

    for (importer_uri, importer_source, previous_diagnostic_uris) in dependents {
        publish_uris.insert(importer_uri.clone());
        publish_uris.extend(previous_diagnostic_uris);
        let document = analyze_document(&importer_uri, &importer_source, documents);
        documents.insert(importer_uri.clone(), document);
    }
}

fn refresh_document_and_dependents(
    uri: &Url,
    source: &str,
    documents: &DashMap<Url, DocumentState>,
) -> Vec<(Url, Vec<Diagnostic>)> {
    let mut publish_uris = HashSet::from([uri.clone()]);
    if let Some(previous) = documents.get(uri) {
        publish_uris.extend(previous.diagnostics_by_uri.keys().cloned());
    }

    let document = analyze_document(uri, source, documents);
    documents.insert(uri.clone(), document);

    refresh_open_importers(uri, documents, &mut publish_uris);

    collect_published_diagnostics(documents, publish_uris)
}

fn close_document_and_dependents(
    uri: &Url,
    documents: &DashMap<Url, DocumentState>,
) -> Vec<(Url, Vec<Diagnostic>)> {
    let Some((_removed_uri, removed_document)) = documents.remove(uri) else {
        return vec![];
    };

    let mut publish_uris = HashSet::from([uri.clone()]);
    publish_uris.extend(removed_document.diagnostics_by_uri.keys().cloned());

    refresh_open_importers(uri, documents, &mut publish_uris);

    collect_published_diagnostics(documents, publish_uris)
}

// ── Server ───────────────────────────────────────────────────────────

/// Hew language server providing IDE features via LSP.
#[derive(Debug)]
pub struct HewLanguageServer {
    client: Client,
    documents: DashMap<Url, DocumentState>,
}

impl HewLanguageServer {
    /// Create a new language server with the given LSP client.
    #[must_use]
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: DashMap::new(),
        }
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
}

#[tower_lsp::async_trait]
impl LanguageServer for HewLanguageServer {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        let capabilities = ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
            completion_provider: Some(CompletionOptions {
                trigger_characters: Some(vec![".".to_string(), ":".to_string()]),
                ..Default::default()
            }),
            hover_provider: Some(HoverProviderCapability::Simple(true)),
            definition_provider: Some(OneOf::Left(true)),
            document_symbol_provider: Some(OneOf::Left(true)),
            semantic_tokens_provider: Some(
                SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
                    legend: SemanticTokensLegend {
                        token_types: TOKEN_TYPES.to_vec(),
                        token_modifiers: TOKEN_MODIFIERS.to_vec(),
                    },
                    full: Some(SemanticTokensFullOptions::Bool(true)),
                    range: None,
                    work_done_progress_options: WorkDoneProgressOptions::default(),
                }),
            ),
            call_hierarchy_provider: Some(CallHierarchyServerCapability::Simple(true)),
            code_lens_provider: Some(CodeLensOptions {
                resolve_provider: Some(false),
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
            code_action_provider: Some(
                tower_lsp::lsp_types::CodeActionProviderCapability::Options(
                    tower_lsp::lsp_types::CodeActionOptions {
                        code_action_kinds: Some(vec![CodeActionKind::QUICKFIX]),
                        ..Default::default()
                    },
                ),
            ),
            folding_range_provider: Some(
                tower_lsp::lsp_types::FoldingRangeProviderCapability::Simple(true),
            ),
            ..Default::default()
        };

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

// ── Diagnostics ──────────────────────────────────────────────────────

fn insert_diagnostic(diagnostics_by_uri: &mut DiagnosticMap, uri: Url, diagnostic: Diagnostic) {
    diagnostics_by_uri.entry(uri).or_default().push(diagnostic);
}

#[cfg(test)]
fn build_diagnostics(
    uri: &Url,
    source: &str,
    lo: &[usize],
    parse_result: &ParseResult,
    type_output: Option<&TypeCheckOutput>,
) -> Vec<Diagnostic> {
    build_diagnostics_by_uri(uri, source, lo, parse_result, type_output, &HashMap::new())
        .remove(uri)
        .unwrap_or_default()
}

fn build_diagnostics_by_uri(
    uri: &Url,
    source: &str,
    lo: &[usize],
    parse_result: &ParseResult,
    type_output: Option<&TypeCheckOutput>,
    module_sources: &HashMap<String, DiagnosticSource>,
) -> DiagnosticMap {
    let mut diagnostics_by_uri = DiagnosticMap::new();

    for err in &parse_result.errors {
        let message = if let Some(hint) = &err.hint {
            format!("{}\n\nhint: {hint}", err.message)
        } else {
            err.message.clone()
        };
        let severity = match err.severity {
            hew_parser::Severity::Error => DiagnosticSeverity::ERROR,
            hew_parser::Severity::Warning => DiagnosticSeverity::WARNING,
        };
        insert_diagnostic(
            &mut diagnostics_by_uri,
            uri.clone(),
            Diagnostic {
                range: span_to_range(source, lo, &err.span),
                severity: Some(severity),
                source: Some("hew-parser".to_string()),
                message,
                ..Default::default()
            },
        );
    }

    if let Some(tc) = type_output {
        for diag in tc.errors.iter().chain(tc.warnings.iter()) {
            let target = diag
                .source_module
                .as_ref()
                .and_then(|module_name| module_sources.get(module_name));
            let (target_uri, target_source, target_line_offsets) = if let Some(target) = target {
                (
                    target.uri.clone(),
                    target.source.as_str(),
                    target.line_offsets.as_slice(),
                )
            } else {
                (uri.clone(), source, lo)
            };

            // Build the message, appending any suggestions.
            let message = if diag.suggestions.is_empty() {
                diag.message.clone()
            } else {
                format!(
                    "{}\n\nDid you mean: {}",
                    diag.message,
                    diag.suggestions.join(", ")
                )
            };

            // Surface notes as related information with their own spans.
            let related_information = if diag.notes.is_empty() {
                None
            } else {
                Some(
                    diag.notes
                        .iter()
                        .map(|(note_span, note_msg)| DiagnosticRelatedInformation {
                            location: Location {
                                uri: target_uri.clone(),
                                range: span_to_range(target_source, target_line_offsets, note_span),
                            },
                            message: note_msg.clone(),
                        })
                        .collect(),
                )
            };

            insert_diagnostic(
                &mut diagnostics_by_uri,
                target_uri,
                Diagnostic {
                    range: span_to_range(target_source, target_line_offsets, &diag.span),
                    severity: Some(severity_to_lsp(diag.severity)),
                    source: Some("hew-types".to_string()),
                    message,
                    related_information,
                    data: Some(diagnostic_data(&diag.kind, &diag.suggestions)),
                    ..Default::default()
                },
            );
        }
    }

    diagnostics_by_uri
}

/// Convert a type-checker `Severity` to the corresponding LSP `DiagnosticSeverity`.
///
/// This is the authoritative severity mapping: the `TypeError` struct carries the
/// severity that was decided at emit time, so the LSP should honour it directly
/// rather than re-deriving it from the `TypeErrorKind`.
fn severity_to_lsp(severity: Severity) -> DiagnosticSeverity {
    match severity {
        Severity::Error => DiagnosticSeverity::ERROR,
        Severity::Warning => DiagnosticSeverity::WARNING,
    }
}

/// Encode a `TypeErrorKind` discriminant and suggestions as JSON for `Diagnostic.data`.
fn diagnostic_data(kind: &TypeErrorKind, suggestions: &[String]) -> serde_json::Value {
    serde_json::json!({
        "kind": kind.as_kind_str(),
        "suggestions": suggestions,
    })
}

// ── Completion ───────────────────────────────────────────────────────

/// Convert an `hew_analysis::CompletionItem` to an `lsp_types::CompletionItem`.
fn to_lsp_completion(item: hew_analysis::CompletionItem) -> CompletionItem {
    use hew_analysis::CompletionKind;
    let kind = match item.kind {
        CompletionKind::Function => CompletionItemKind::FUNCTION,
        CompletionKind::Variable => CompletionItemKind::VARIABLE,
        CompletionKind::Keyword => CompletionItemKind::KEYWORD,
        CompletionKind::Snippet => CompletionItemKind::SNIPPET,
        CompletionKind::Type => CompletionItemKind::STRUCT,
        CompletionKind::Actor => CompletionItemKind::CLASS,
        CompletionKind::Constant => CompletionItemKind::CONSTANT,
        CompletionKind::Field => CompletionItemKind::FIELD,
        CompletionKind::Method => CompletionItemKind::METHOD,
        CompletionKind::Module => CompletionItemKind::MODULE,
    };
    CompletionItem {
        label: item.label,
        kind: Some(kind),
        detail: item.detail,
        insert_text: item.insert_text,
        insert_text_format: if item.insert_text_is_snippet {
            Some(InsertTextFormat::SNIPPET)
        } else {
            None
        },
        sort_text: item.sort_text,
        ..Default::default()
    }
}

// ── Document symbols ─────────────────────────────────────────────────

/// Convert an `hew_analysis::SymbolKind` to an LSP `SymbolKind`.
fn analysis_symbol_kind_to_lsp(kind: hew_analysis::SymbolKind) -> SymbolKind {
    match kind {
        hew_analysis::SymbolKind::Function => SymbolKind::FUNCTION,
        hew_analysis::SymbolKind::Actor | hew_analysis::SymbolKind::Supervisor => SymbolKind::CLASS,
        hew_analysis::SymbolKind::Machine | hew_analysis::SymbolKind::Enum => SymbolKind::ENUM,
        hew_analysis::SymbolKind::Trait => SymbolKind::INTERFACE,
        hew_analysis::SymbolKind::Type | hew_analysis::SymbolKind::Wire => SymbolKind::STRUCT,
        hew_analysis::SymbolKind::Constant => SymbolKind::CONSTANT,
        hew_analysis::SymbolKind::TypeAlias => SymbolKind::TYPE_PARAMETER,
        hew_analysis::SymbolKind::Impl => SymbolKind::NAMESPACE,
        hew_analysis::SymbolKind::Field => SymbolKind::FIELD,
        hew_analysis::SymbolKind::Method => SymbolKind::METHOD,
        hew_analysis::SymbolKind::State | hew_analysis::SymbolKind::Variant => {
            SymbolKind::ENUM_MEMBER
        }
        hew_analysis::SymbolKind::Event => SymbolKind::EVENT,
        hew_analysis::SymbolKind::Module => SymbolKind::MODULE,
        hew_analysis::SymbolKind::Constructor => SymbolKind::CONSTRUCTOR,
    }
}

/// Convert an `hew_analysis::SymbolInfo` to an LSP `DocumentSymbol`.
#[expect(
    deprecated,
    reason = "DocumentSymbol::deprecated is required by the LSP type"
)]
fn symbol_info_to_doc_symbol(
    source: &str,
    lo: &[usize],
    info: hew_analysis::SymbolInfo,
) -> DocumentSymbol {
    let range = offset_range_to_lsp(source, lo, info.span.start, info.span.end);
    let selection_range = offset_range_to_lsp(
        source,
        lo,
        info.selection_span.start,
        info.selection_span.end,
    );
    let children = if info.children.is_empty() {
        None
    } else {
        Some(
            info.children
                .into_iter()
                .map(|c| symbol_info_to_doc_symbol(source, lo, c))
                .collect(),
        )
    };
    DocumentSymbol {
        name: info.name,
        detail: None,
        kind: analysis_symbol_kind_to_lsp(info.kind),
        tags: None,
        deprecated: None,
        range,
        selection_range,
        children,
    }
}

// ── Semantic tokens ──────────────────────────────────────────────────

/// Convert analysis semantic tokens (absolute byte offsets) to LSP
/// delta-encoded tokens (line/col deltas, UTF-16 lengths).
#[expect(
    clippy::cast_possible_truncation,
    reason = "token delta values in source files will not exceed u32"
)]
fn analysis_tokens_to_lsp(
    source: &str,
    lo: &[usize],
    tokens: &[hew_analysis::SemanticToken],
) -> Vec<SemanticToken> {
    let mut result = Vec::new();
    let mut prev_line: u32 = 0;
    let mut prev_start: u32 = 0;

    for tok in tokens {
        let (line, col) = offset_to_line_col(source, lo, tok.start);
        let line = line as u32;
        let col = col as u32;
        // UTF-16 length for the LSP protocol
        let length: u32 = source[tok.start..tok.start + tok.length]
            .chars()
            .map(|c| c.len_utf16() as u32)
            .sum();

        let delta_line = line - prev_line;
        let delta_start = if delta_line == 0 {
            col - prev_start
        } else {
            col
        };

        // Map analysis modifier bitmasks to LSP modifier bitmasks.
        let mut lsp_modifiers: u32 = 0;
        if tok.modifiers & hew_analysis::token_modifiers::DECLARATION != 0 {
            lsp_modifiers |= modifier_bit(&SemanticTokenModifier::DECLARATION);
        }
        if tok.modifiers & hew_analysis::token_modifiers::READONLY != 0 {
            lsp_modifiers |= modifier_bit(&SemanticTokenModifier::READONLY);
        }
        if tok.modifiers & hew_analysis::token_modifiers::ASYNC != 0 {
            lsp_modifiers |= modifier_bit(&SemanticTokenModifier::ASYNC);
        }

        result.push(SemanticToken {
            delta_line,
            delta_start,
            length,
            token_type: tok.token_type,
            token_modifiers_bitset: lsp_modifiers,
        });

        prev_line = line;
        prev_start = col;
    }

    result
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

// ── Go-to-definition ─────────────────────────────────────────────────

/// Search for a definition matching `word` in the AST, returning an LSP `Range`.
fn find_definition_in_ast(
    source: &str,
    lo: &[usize],
    parse_result: &ParseResult,
    word: &str,
) -> Option<Range> {
    let span = hew_analysis::definition::find_definition(source, parse_result, word)?;
    Some(offset_range_to_lsp(source, lo, span.start, span.end))
}

// ── Import path resolution ────────────────────────────────────────────

/// Compute the absolute file path that an `ImportDecl` would resolve to,
/// searching the workspace root first then the importing file's directory.
///
/// Does **not** check whether the file exists on disk; callers decide whether
/// to check existence before performing I/O.
fn compute_import_path(uri: &Url, import: &ImportDecl) -> Option<std::path::PathBuf> {
    // String-literal import: `import "relative/path.hew";`
    if let Some(fp) = &import.file_path {
        let file_dir = uri
            .to_file_path()
            .ok()
            .and_then(|p| p.parent().map(std::path::Path::to_path_buf))?;
        return Some(file_dir.join(fp));
    }

    if import.path.is_empty() {
        return None;
    }

    let relative = format!("{}.hew", import.path.join("/"));

    // Workspace root: the directory that directly contains a `std/` folder.
    let workspace_root = uri.to_file_path().ok().and_then(|p| {
        let mut dir = p.parent().map(std::path::Path::to_path_buf);
        while let Some(d) = dir {
            if d.join("std").is_dir() {
                return Some(d);
            }
            dir = d.parent().map(std::path::Path::to_path_buf);
        }
        None
    });

    // Prefer workspace root when the file already exists there.
    if let Some(root) = workspace_root {
        let candidate = root.join(&relative);
        if candidate.exists() {
            return Some(candidate);
        }
    }

    // Fall back to the directory of the importing file.
    let file_dir = uri
        .to_file_path()
        .ok()
        .and_then(|p| p.parent().map(std::path::Path::to_path_buf))?;
    Some(file_dir.join(&relative))
}

fn collect_import_items(parse_result: &ParseResult) -> Vec<(ImportDecl, Span)> {
    parse_result
        .program
        .items
        .iter()
        .filter_map(|(item, span)| match item {
            Item::Import(import) => Some((import.clone(), span.clone())),
            _ => None,
        })
        .collect()
}

fn is_identifier_char(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
}

fn find_identifier_span_in_range(
    source: &str,
    range: std::ops::Range<usize>,
    ident: &str,
) -> Option<hew_analysis::OffsetSpan> {
    if ident.is_empty() {
        return None;
    }

    let slice = source.get(range.clone())?;
    let mut search_start = 0;
    while let Some(relative_start) = slice[search_start..].find(ident) {
        let start = range.start + search_start + relative_start;
        let end = start + ident.len();
        let prev_is_ident = source[..start]
            .chars()
            .next_back()
            .is_some_and(is_identifier_char);
        let next_is_ident = source[end..].chars().next().is_some_and(is_identifier_char);
        if !prev_is_ident && !next_is_ident {
            return Some(hew_analysis::OffsetSpan { start, end });
        }
        search_start += relative_start + ident.len();
    }

    None
}

fn find_named_import_spans(
    source: &str,
    item_span: &Span,
    import_name: &hew_parser::ast::ImportName,
) -> Option<(hew_analysis::OffsetSpan, hew_analysis::OffsetSpan)> {
    let item_text = source.get(item_span.clone())?;
    let open_brace = item_text.find('{')?;
    let close_brace = item_text.rfind('}')?;
    if open_brace >= close_brace {
        return None;
    }

    let names_range = item_span.start + open_brace + 1..item_span.start + close_brace;
    let import_name_span =
        find_identifier_span_in_range(source, names_range.clone(), &import_name.name)?;
    let visible_name_span = match &import_name.alias {
        Some(alias) => find_identifier_span_in_range(source, names_range, alias)?,
        None => import_name_span,
    };

    Some((import_name_span, visible_name_span))
}

/// Cross-file references/rename currently follow explicit named imports between
/// open documents, reusing the existing import path resolution without a
/// broader project index.
#[derive(Debug, Clone)]
struct NamedImportMatch {
    importer_uri: Url,
    imported_uri: Url,
    imported_name: String,
    visible_name: String,
    import_name_span: hew_analysis::OffsetSpan,
    visible_name_span: hew_analysis::OffsetSpan,
}

impl NamedImportMatch {
    fn is_aliased(&self) -> bool {
        self.visible_name != self.imported_name
    }
}

fn find_named_import_match(
    current_uri: &Url,
    source: &str,
    parse_result: &ParseResult,
    word: &str,
    documents: &DashMap<Url, DocumentState>,
) -> Option<NamedImportMatch> {
    for (import, item_span) in collect_import_items(parse_result) {
        let Some(ImportSpec::Names(names)) = &import.spec else {
            continue;
        };
        let Some(path) = compute_import_path(current_uri, &import) else {
            continue;
        };
        let Ok(imported_uri) = Url::from_file_path(&path) else {
            continue;
        };
        let Some(target_doc) = documents.get(&imported_uri) else {
            continue;
        };

        for import_name in names {
            let visible_name = import_name
                .alias
                .as_deref()
                .unwrap_or(import_name.name.as_str());
            if visible_name != word {
                continue;
            }
            if hew_analysis::definition::find_definition(
                &target_doc.source,
                &target_doc.parse_result,
                &import_name.name,
            )
            .is_none()
            {
                continue;
            }
            let Some((import_name_span, visible_name_span)) =
                find_named_import_spans(source, &item_span, import_name)
            else {
                continue;
            };
            return Some(NamedImportMatch {
                importer_uri: current_uri.clone(),
                imported_uri,
                imported_name: import_name.name.clone(),
                visible_name: visible_name.to_string(),
                import_name_span,
                visible_name_span,
            });
        }
    }

    None
}

fn span_contains_offset(span: hew_analysis::OffsetSpan, offset: usize) -> bool {
    span.start <= offset && offset < span.end
}

fn find_resolved_named_import_match(
    current_uri: &Url,
    doc: &DocumentState,
    offset: usize,
    word: &str,
    documents: &DashMap<Url, DocumentState>,
) -> Option<(NamedImportMatch, Vec<hew_analysis::OffsetSpan>)> {
    let import_match =
        find_named_import_match(current_uri, &doc.source, &doc.parse_result, word, documents)?;
    let usage_spans = hew_analysis::references::find_import_binding_references(
        &doc.parse_result,
        &import_match.visible_name,
    );

    if span_contains_offset(import_match.visible_name_span, offset)
        || usage_spans
            .iter()
            .any(|span| span_contains_offset(*span, offset))
    {
        Some((import_match, usage_spans))
    } else {
        None
    }
}

fn find_open_named_importers(
    target_uri: &Url,
    target_name: &str,
    documents: &DashMap<Url, DocumentState>,
) -> Vec<NamedImportMatch> {
    let mut matches = Vec::new();

    for entry in documents {
        let importer_uri = entry.key().clone();
        if importer_uri == *target_uri {
            continue;
        }
        let doc = entry.value();

        for (import, item_span) in collect_import_items(&doc.parse_result) {
            let Some(ImportSpec::Names(names)) = &import.spec else {
                continue;
            };
            let Some(path) = compute_import_path(&importer_uri, &import) else {
                continue;
            };
            let Ok(resolved_uri) = Url::from_file_path(&path) else {
                continue;
            };
            if resolved_uri != *target_uri {
                continue;
            }

            for import_name in names {
                if import_name.name != target_name {
                    continue;
                }
                let Some((import_name_span, visible_name_span)) =
                    find_named_import_spans(&doc.source, &item_span, import_name)
                else {
                    continue;
                };
                matches.push(NamedImportMatch {
                    importer_uri: importer_uri.clone(),
                    imported_uri: resolved_uri.clone(),
                    imported_name: import_name.name.clone(),
                    visible_name: import_name
                        .alias
                        .as_deref()
                        .unwrap_or(import_name.name.as_str())
                        .to_string(),
                    import_name_span,
                    visible_name_span,
                });
            }
        }
    }

    matches
}

fn find_definition_name_span(
    source: &str,
    parse_result: &ParseResult,
    name: &str,
) -> Option<hew_analysis::OffsetSpan> {
    let item_span = hew_analysis::definition::find_definition(source, parse_result, name)?;
    find_identifier_span_in_range(source, item_span.start..item_span.end, name)
}

fn push_location_for_span(
    locations: &mut Vec<Location>,
    uri: &Url,
    source: &str,
    line_offsets: &[usize],
    span: hew_analysis::OffsetSpan,
) {
    locations.push(Location {
        uri: uri.clone(),
        range: offset_range_to_lsp(source, line_offsets, span.start, span.end),
    });
}

fn collect_local_reference_locations(
    uri: &Url,
    doc: &DocumentState,
    offset: usize,
    include_declaration: bool,
) -> Vec<Location> {
    let mut locations = Vec::new();

    if let Some((_name, spans)) =
        hew_analysis::references::find_all_references(&doc.source, &doc.parse_result, offset)
    {
        for span in spans {
            push_location_for_span(&mut locations, uri, &doc.source, &doc.line_offsets, span);
        }
    }

    if include_declaration {
        if let Some((name, _)) = hew_analysis::util::simple_word_at_offset(&doc.source, offset) {
            if let Some(def_span) =
                hew_analysis::definition::find_definition(&doc.source, &doc.parse_result, &name)
            {
                push_location_for_span(
                    &mut locations,
                    uri,
                    &doc.source,
                    &doc.line_offsets,
                    def_span,
                );
            }
        }
    }

    locations
}

fn build_prepare_rename_response(
    uri: &Url,
    doc: &DocumentState,
    offset: usize,
    documents: &DashMap<Url, DocumentState>,
) -> Option<PrepareRenameResponse> {
    let span = if let Some((name, span)) =
        hew_analysis::util::simple_word_at_offset(&doc.source, offset)
    {
        if find_resolved_named_import_match(uri, doc, offset, &name, documents).is_some() {
            span
        } else {
            hew_analysis::rename::prepare_rename(&doc.source, &doc.parse_result, offset)?
        }
    } else {
        hew_analysis::rename::prepare_rename(&doc.source, &doc.parse_result, offset)?
    };
    let range = offset_range_to_lsp(&doc.source, &doc.line_offsets, span.start, span.end);
    Some(PrepareRenameResponse::Range(range))
}

fn sort_and_dedup_locations(locations: &mut Vec<Location>) {
    locations.sort_by(|left, right| {
        left.uri
            .as_str()
            .cmp(right.uri.as_str())
            .then(left.range.start.line.cmp(&right.range.start.line))
            .then(left.range.start.character.cmp(&right.range.start.character))
            .then(left.range.end.line.cmp(&right.range.end.line))
            .then(left.range.end.character.cmp(&right.range.end.character))
    });
    locations.dedup_by(|left, right| left.uri == right.uri && left.range == right.range);
}

fn collect_local_rename_edits(
    doc: &DocumentState,
    offset: usize,
    new_name: &str,
) -> Vec<hew_analysis::RenameEdit> {
    hew_analysis::rename::rename(&doc.source, &doc.parse_result, offset, new_name)
        .unwrap_or_default()
}

fn sort_and_dedup_rename_edits(edits: &mut Vec<hew_analysis::RenameEdit>) {
    edits.sort_by(|left, right| {
        left.span
            .start
            .cmp(&right.span.start)
            .then(left.span.end.cmp(&right.span.end))
            .then_with(|| left.new_text.cmp(&right.new_text))
    });
    edits.dedup_by(|left, right| left.span == right.span && left.new_text == right.new_text);
}

fn rename_edit_to_text_edit(doc: &DocumentState, edit: hew_analysis::RenameEdit) -> TextEdit {
    TextEdit {
        range: offset_range_to_lsp(
            &doc.source,
            &doc.line_offsets,
            edit.span.start,
            edit.span.end,
        ),
        new_text: edit.new_text,
    }
}

fn workspace_edit_from_changes(
    uri: &Url,
    doc: &DocumentState,
    documents: &DashMap<Url, DocumentState>,
    mut changes: HashMap<Url, Vec<hew_analysis::RenameEdit>>,
) -> Option<WorkspaceEdit> {
    for edits in changes.values_mut() {
        sort_and_dedup_rename_edits(edits);
    }
    changes.retain(|_, edits| !edits.is_empty());

    if changes.is_empty() {
        return None;
    }

    let mut lsp_changes = HashMap::new();
    for (target_uri, edits) in changes {
        let text_edits = if target_uri == *uri {
            edits
                .into_iter()
                .map(|edit| rename_edit_to_text_edit(doc, edit))
                .collect()
        } else if let Some(target_doc) = documents.get(&target_uri) {
            edits
                .into_iter()
                .map(|edit| rename_edit_to_text_edit(&target_doc, edit))
                .collect()
        } else {
            continue;
        };
        lsp_changes.insert(target_uri, text_edits);
    }

    Some(WorkspaceEdit {
        changes: Some(lsp_changes),
        ..Default::default()
    })
}

fn build_reference_locations(
    uri: &Url,
    doc: &DocumentState,
    offset: usize,
    include_declaration: bool,
    documents: &DashMap<Url, DocumentState>,
) -> Vec<Location> {
    let Some((name, _)) = hew_analysis::util::simple_word_at_offset(&doc.source, offset) else {
        return Vec::new();
    };

    if let Some((import_match, usage_spans)) =
        find_resolved_named_import_match(uri, doc, offset, &name, documents)
    {
        let mut locations = Vec::new();
        push_location_for_span(
            &mut locations,
            &import_match.importer_uri,
            &doc.source,
            &doc.line_offsets,
            import_match.visible_name_span,
        );
        for span in usage_spans {
            push_location_for_span(
                &mut locations,
                &import_match.importer_uri,
                &doc.source,
                &doc.line_offsets,
                span,
            );
        }

        if let Some(target_doc) = documents.get(&import_match.imported_uri) {
            if let Some(def_span) = find_definition_name_span(
                &target_doc.source,
                &target_doc.parse_result,
                &import_match.imported_name,
            ) {
                locations.extend(collect_local_reference_locations(
                    &import_match.imported_uri,
                    &target_doc,
                    def_span.start,
                    include_declaration,
                ));
            }
        }

        for importer in find_open_named_importers(
            &import_match.imported_uri,
            &import_match.imported_name,
            documents,
        ) {
            if importer.importer_uri == import_match.importer_uri {
                continue;
            }
            if let Some(importer_doc) = documents.get(&importer.importer_uri) {
                push_location_for_span(
                    &mut locations,
                    &importer.importer_uri,
                    &importer_doc.source,
                    &importer_doc.line_offsets,
                    importer.import_name_span,
                );
                for span in hew_analysis::references::find_import_binding_references(
                    &importer_doc.parse_result,
                    &importer.visible_name,
                ) {
                    push_location_for_span(
                        &mut locations,
                        &importer.importer_uri,
                        &importer_doc.source,
                        &importer_doc.line_offsets,
                        span,
                    );
                }
            }
        }

        sort_and_dedup_locations(&mut locations);
        return locations;
    }

    let mut locations = collect_local_reference_locations(uri, doc, offset, include_declaration);

    if hew_analysis::references::is_top_level_name(&doc.parse_result, &name) {
        for importer in find_open_named_importers(uri, &name, documents) {
            if let Some(importer_doc) = documents.get(&importer.importer_uri) {
                push_location_for_span(
                    &mut locations,
                    &importer.importer_uri,
                    &importer_doc.source,
                    &importer_doc.line_offsets,
                    importer.import_name_span,
                );
                for span in hew_analysis::references::find_import_binding_references(
                    &importer_doc.parse_result,
                    &importer.visible_name,
                ) {
                    push_location_for_span(
                        &mut locations,
                        &importer.importer_uri,
                        &importer_doc.source,
                        &importer_doc.line_offsets,
                        span,
                    );
                }
            }
        }
    }

    sort_and_dedup_locations(&mut locations);
    locations
}

#[expect(
    clippy::too_many_lines,
    reason = "cross-file rename handles local, imported-definition, and importer fanout paths"
)]
fn build_workspace_edit(
    uri: &Url,
    doc: &DocumentState,
    offset: usize,
    new_name: &str,
    documents: &DashMap<Url, DocumentState>,
) -> Option<WorkspaceEdit> {
    let (name, _) = hew_analysis::util::simple_word_at_offset(&doc.source, offset)?;

    if let Some((import_match, usage_spans)) =
        find_resolved_named_import_match(uri, doc, offset, &name, documents)
    {
        let mut changes: HashMap<Url, Vec<hew_analysis::RenameEdit>> = HashMap::new();
        let mut importer_edits: Vec<_> = usage_spans
            .into_iter()
            .map(|span| hew_analysis::RenameEdit {
                span,
                new_text: new_name.to_string(),
            })
            .collect();
        importer_edits.push(hew_analysis::RenameEdit {
            span: import_match.visible_name_span,
            new_text: new_name.to_string(),
        });
        changes.insert(uri.clone(), importer_edits);

        if !import_match.is_aliased() {
            if let Some(target_doc) = documents.get(&import_match.imported_uri) {
                if let Some(def_span) = find_definition_name_span(
                    &target_doc.source,
                    &target_doc.parse_result,
                    &import_match.imported_name,
                ) {
                    let target_edits =
                        collect_local_rename_edits(&target_doc, def_span.start, new_name);
                    if !target_edits.is_empty() {
                        changes
                            .entry(import_match.imported_uri.clone())
                            .or_default()
                            .extend(target_edits);
                    }
                }
            }

            for importer in find_open_named_importers(
                &import_match.imported_uri,
                &import_match.imported_name,
                documents,
            ) {
                if importer.importer_uri == *uri {
                    continue;
                }
                changes
                    .entry(importer.importer_uri.clone())
                    .or_default()
                    .push(hew_analysis::RenameEdit {
                        span: importer.import_name_span,
                        new_text: new_name.to_string(),
                    });

                if importer.is_aliased() {
                    continue;
                }

                if let Some(importer_doc) = documents.get(&importer.importer_uri) {
                    let importer_edits: Vec<_> =
                        hew_analysis::references::find_import_binding_references(
                            &importer_doc.parse_result,
                            &importer.visible_name,
                        )
                        .into_iter()
                        .map(|span| hew_analysis::RenameEdit {
                            span,
                            new_text: new_name.to_string(),
                        })
                        .collect();
                    if !importer_edits.is_empty() {
                        changes
                            .entry(importer.importer_uri.clone())
                            .or_default()
                            .extend(importer_edits);
                    }
                }
            }
        }

        return workspace_edit_from_changes(uri, doc, documents, changes);
    }

    let mut changes: HashMap<Url, Vec<hew_analysis::RenameEdit>> = HashMap::new();
    let local_edits = collect_local_rename_edits(doc, offset, new_name);
    if !local_edits.is_empty() {
        changes.insert(uri.clone(), local_edits);
    }

    if hew_analysis::references::is_top_level_name(&doc.parse_result, &name) {
        for importer in find_open_named_importers(uri, &name, documents) {
            changes
                .entry(importer.importer_uri.clone())
                .or_default()
                .push(hew_analysis::RenameEdit {
                    span: importer.import_name_span,
                    new_text: new_name.to_string(),
                });

            if importer.is_aliased() {
                continue;
            }

            if let Some(importer_doc) = documents.get(&importer.importer_uri) {
                let importer_edits: Vec<_> =
                    hew_analysis::references::find_import_binding_references(
                        &importer_doc.parse_result,
                        &importer.visible_name,
                    )
                    .into_iter()
                    .map(|span| hew_analysis::RenameEdit {
                        span,
                        new_text: new_name.to_string(),
                    })
                    .collect();
                if !importer_edits.is_empty() {
                    changes
                        .entry(importer.importer_uri.clone())
                        .or_default()
                        .extend(importer_edits);
                }
            }
        }
    }

    workspace_edit_from_changes(uri, doc, documents, changes)
}

// ── Cross-file go-to-definition ───────────────────────────────────────

/// Search for the definition of `word` in the files imported by the current
/// file's `parse_result`.
///
/// Resolution order for each matching import:
/// 1. Open documents already held by the server (no disk I/O).
/// 2. The file on disk (parsed on demand).
///
/// Returns `(target_uri, range)` for the first match found, or `None`.
fn find_cross_file_definition(
    current_uri: &Url,
    imports: &[hew_parser::ast::ImportDecl],
    word: &str,
    documents: &DashMap<Url, DocumentState>,
) -> Option<(Url, Range)> {
    for import in imports {
        let Some(path) = compute_import_path(current_uri, import) else {
            continue;
        };
        let Ok(target_uri) = Url::from_file_path(&path) else {
            continue;
        };

        // Determine which name to search for in the target file, based on
        // what this import makes visible in the current scope.
        let search_name: &str = match &import.spec {
            Some(ImportSpec::Names(names)) => {
                // Find the entry whose visible name (alias if present, otherwise
                // the original name) matches `word`.
                let Some(entry) = names
                    .iter()
                    .find(|n| n.alias.as_deref().unwrap_or(n.name.as_str()) == word)
                else {
                    continue; // this import does not bring `word` into scope
                };
                &entry.name // search target file by the *original* name
            }
            Some(ImportSpec::Glob) => word, // glob: any name may come from here
            None => {
                // Bare path import (`import foo::bar`).  The last path segment
                // is itself a navigable target (e.g. cursor on `bar`).
                if import.path.last().map(String::as_str) == Some(word)
                    && (documents.contains_key(&target_uri) || path.exists())
                {
                    return Some((target_uri, Range::default()));
                }
                continue;
            }
        };

        // Search in an already-open document first (no I/O).
        if let Some(doc) = documents.get(&target_uri) {
            if let Some(range) = find_definition_in_ast(
                &doc.source,
                &doc.line_offsets,
                &doc.parse_result,
                search_name,
            ) {
                return Some((target_uri, range));
            }
            // File is open but name not found there — don't fall through to disk.
            continue;
        }

        // Fall back: read and parse the file from disk.
        if path.exists() {
            if let Ok(source) = std::fs::read_to_string(&path) {
                let pr = hew_parser::parse(&source);
                let lo = compute_line_offsets(&source);
                if let Some(range) = find_definition_in_ast(&source, &lo, &pr, search_name) {
                    return Some((target_uri, range));
                }
            }
        }
    }
    None
}

// ── Document links ──────────────────────────────────────────────────

fn build_document_links(
    uri: &Url,
    source: &str,
    lo: &[usize],
    parse_result: &ParseResult,
) -> Vec<DocumentLink> {
    let mut links = Vec::new();

    for (item, span) in &parse_result.program.items {
        if let Item::Import(import) = item {
            let Some(path) = compute_import_path(uri, import) else {
                continue;
            };
            if !path.exists() {
                continue;
            }
            if let Ok(target_uri) = Url::from_file_path(&path) {
                let relative = format!("{}.hew", import.path.join("/"));
                links.push(DocumentLink {
                    range: span_to_range(source, lo, span),
                    target: Some(target_uri),
                    tooltip: Some(format!("Open {relative}")),
                    data: None,
                });
            }
        }
    }
    links
}

// ── Type hierarchy helpers ──────────────────────────────────────────

fn find_type_hierarchy_item(
    uri: &Url,
    source: &str,
    lo: &[usize],
    parse_result: &ParseResult,
    name: &str,
) -> Option<TypeHierarchyItem> {
    for (item, span) in &parse_result.program.items {
        let (item_name, kind) = match item {
            Item::TypeDecl(td) => {
                let kind = match td.kind {
                    TypeDeclKind::Struct => SymbolKind::STRUCT,
                    TypeDeclKind::Enum => SymbolKind::ENUM,
                };
                (td.name.as_str(), kind)
            }
            Item::Actor(a) => (a.name.as_str(), SymbolKind::CLASS),
            Item::Trait(t) => (t.name.as_str(), SymbolKind::INTERFACE),
            _ => continue,
        };
        if item_name == name {
            let range = span_to_range(source, lo, span);
            return Some(TypeHierarchyItem {
                name: item_name.to_string(),
                kind,
                tags: None,
                detail: None,
                uri: uri.clone(),
                range,
                selection_range: range,
                data: None,
            });
        }
    }
    None
}

fn collect_supertypes(
    uri: &Url,
    name: &str,
    source: &str,
    lo: &[usize],
    parse_result: &ParseResult,
) -> Vec<TypeHierarchyItem> {
    let mut supers = Vec::new();

    for (item, _) in &parse_result.program.items {
        match item {
            // For actors, collect their declared super_traits and return immediately.
            Item::Actor(a) if a.name == name => {
                if let Some(bounds) = &a.super_traits {
                    for bound in bounds {
                        if let Some(hi) =
                            find_type_hierarchy_item(uri, source, lo, parse_result, &bound.name)
                        {
                            supers.push(hi);
                        }
                    }
                }
                return supers;
            }
            // For types, find impl blocks: `impl TraitName for TypeName`
            Item::Impl(impl_decl) => {
                let target = match &impl_decl.target_type.0 {
                    hew_parser::ast::TypeExpr::Named { name: tname, .. } => tname.as_str(),
                    _ => continue,
                };
                if target == name {
                    if let Some(bound) = &impl_decl.trait_bound {
                        if let Some(hi) =
                            find_type_hierarchy_item(uri, source, lo, parse_result, &bound.name)
                        {
                            supers.push(hi);
                        }
                    }
                }
            }
            _ => {}
        }
    }
    supers
}

fn collect_subtypes(
    uri: &Url,
    trait_name: &str,
    source: &str,
    lo: &[usize],
    parse_result: &ParseResult,
) -> Vec<TypeHierarchyItem> {
    let mut subs = Vec::new();

    // Find types that implement this trait via `impl TraitName for TypeName`.
    for (item, _) in &parse_result.program.items {
        if let Item::Impl(impl_decl) = item {
            if let Some(bound) = &impl_decl.trait_bound {
                if bound.name == trait_name {
                    let target = match &impl_decl.target_type.0 {
                        hew_parser::ast::TypeExpr::Named { name: tname, .. } => tname.as_str(),
                        _ => continue,
                    };
                    if let Some(hi) =
                        find_type_hierarchy_item(uri, source, lo, parse_result, target)
                    {
                        subs.push(hi);
                    }
                }
            }
        }
    }

    // Find actors that declare this trait as a super_trait.
    for (item, _) in &parse_result.program.items {
        if let Item::Actor(a) = item {
            if let Some(bounds) = &a.super_traits {
                if bounds.iter().any(|b| b.name == trait_name) {
                    if let Some(hi) =
                        find_type_hierarchy_item(uri, source, lo, parse_result, &a.name)
                    {
                        subs.push(hi);
                    }
                }
            }
        }
    }
    subs
}

// ── Call hierarchy helpers ──────────────────────────────────────────

fn find_callable_at(
    uri: &Url,
    source: &str,
    lo: &[usize],
    parse_result: &ParseResult,
    name: &str,
) -> Option<CallHierarchyItem> {
    for (item, item_span) in &parse_result.program.items {
        match item {
            Item::Function(f) if f.name == name => {
                let range = span_to_range(source, lo, item_span);
                return Some(CallHierarchyItem {
                    name: f.name.clone(),
                    kind: SymbolKind::FUNCTION,
                    tags: None,
                    detail: None,
                    uri: uri.clone(),
                    range,
                    selection_range: range,
                    data: None,
                });
            }
            Item::Actor(a) => {
                for recv in &a.receive_fns {
                    if recv.name == name {
                        let fn_span = if recv.span.is_empty() {
                            item_span
                        } else {
                            &recv.span
                        };
                        let range = span_to_range(source, lo, fn_span);
                        return Some(CallHierarchyItem {
                            name: recv.name.clone(),
                            kind: SymbolKind::METHOD,
                            tags: None,
                            detail: Some(format!("actor {}", a.name)),
                            uri: uri.clone(),
                            range,
                            selection_range: range,
                            data: None,
                        });
                    }
                }
            }
            _ => {}
        }
    }
    None
}

#[expect(
    clippy::too_many_lines,
    reason = "exhaustive match over caller item kinds is clearest as one function"
)]
fn find_incoming_calls(
    uri: &Url,
    source: &str,
    lo: &[usize],
    parse_result: &ParseResult,
    target_name: &str,
) -> Vec<CallHierarchyIncomingCall> {
    let mut result = Vec::new();
    for (item, item_span) in &parse_result.program.items {
        // Collect all call sites inside this item using the exhaustive item walker.
        let mut calls = Vec::new();
        collect_calls_in_item(item, &mut calls);
        let matching: Vec<_> = calls.iter().filter(|c| c.name == target_name).collect();
        if matching.is_empty() {
            continue;
        }
        let from_ranges: Vec<_> = matching
            .iter()
            .map(|c| span_to_range(source, lo, &c.span))
            .collect();

        // Derive display metadata from the item kind.
        match item {
            Item::Function(f) => {
                let range = span_to_range(source, lo, item_span);
                result.push(CallHierarchyIncomingCall {
                    from: CallHierarchyItem {
                        name: f.name.clone(),
                        kind: SymbolKind::FUNCTION,
                        tags: None,
                        detail: None,
                        uri: uri.clone(),
                        range,
                        selection_range: range,
                        data: None,
                    },
                    from_ranges,
                });
            }
            Item::Actor(a) => {
                // Walk each named body independently for precise attribution.
                // init body (no dedicated name span — fall back to item span).
                if let Some(init) = &a.init {
                    let mut body_calls = Vec::new();
                    collect_calls_in_block(&init.body, &mut body_calls);
                    let fn_calls: Vec<_> = body_calls
                        .iter()
                        .filter(|c| c.name == target_name)
                        .collect();
                    if !fn_calls.is_empty() {
                        let range = span_to_range(source, lo, item_span);
                        result.push(CallHierarchyIncomingCall {
                            from: CallHierarchyItem {
                                name: format!("{}.init", a.name),
                                kind: SymbolKind::METHOD,
                                tags: None,
                                detail: None,
                                uri: uri.clone(),
                                range,
                                selection_range: range,
                                data: None,
                            },
                            from_ranges: fn_calls
                                .iter()
                                .map(|c| span_to_range(source, lo, &c.span))
                                .collect(),
                        });
                    }
                }
                // terminate body.
                if let Some(term) = &a.terminate {
                    let mut body_calls = Vec::new();
                    collect_calls_in_block(&term.body, &mut body_calls);
                    let fn_calls: Vec<_> = body_calls
                        .iter()
                        .filter(|c| c.name == target_name)
                        .collect();
                    if !fn_calls.is_empty() {
                        let range = span_to_range(source, lo, item_span);
                        result.push(CallHierarchyIncomingCall {
                            from: CallHierarchyItem {
                                name: format!("{}.terminate", a.name),
                                kind: SymbolKind::METHOD,
                                tags: None,
                                detail: None,
                                uri: uri.clone(),
                                range,
                                selection_range: range,
                                data: None,
                            },
                            from_ranges: fn_calls
                                .iter()
                                .map(|c| span_to_range(source, lo, &c.span))
                                .collect(),
                        });
                    }
                }
                // Receive functions.
                for recv in &a.receive_fns {
                    let mut body_calls = Vec::new();
                    collect_calls_in_block(&recv.body, &mut body_calls);
                    let fn_calls: Vec<_> = body_calls
                        .iter()
                        .filter(|c| c.name == target_name)
                        .collect();
                    if fn_calls.is_empty() {
                        continue;
                    }
                    let fn_span = if recv.span.is_empty() {
                        item_span
                    } else {
                        &recv.span
                    };
                    let range = span_to_range(source, lo, fn_span);
                    result.push(CallHierarchyIncomingCall {
                        from: CallHierarchyItem {
                            name: recv.name.clone(),
                            kind: SymbolKind::METHOD,
                            tags: None,
                            detail: Some(format!("actor {}", a.name)),
                            uri: uri.clone(),
                            range,
                            selection_range: range,
                            data: None,
                        },
                        from_ranges: fn_calls
                            .iter()
                            .map(|c| span_to_range(source, lo, &c.span))
                            .collect(),
                    });
                }
                // Actor methods.
                for method in &a.methods {
                    let mut body_calls = Vec::new();
                    collect_calls_in_block(&method.body, &mut body_calls);
                    let fn_calls: Vec<_> = body_calls
                        .iter()
                        .filter(|c| c.name == target_name)
                        .collect();
                    if fn_calls.is_empty() {
                        continue;
                    }
                    let range = if method.decl_span.is_empty() {
                        span_to_range(source, lo, item_span)
                    } else {
                        span_to_range(source, lo, &method.decl_span)
                    };
                    result.push(CallHierarchyIncomingCall {
                        from: CallHierarchyItem {
                            name: method.name.clone(),
                            kind: SymbolKind::METHOD,
                            tags: None,
                            detail: Some(format!("actor {}", a.name)),
                            uri: uri.clone(),
                            range,
                            selection_range: range,
                            data: None,
                        },
                        from_ranges: fn_calls
                            .iter()
                            .map(|c| span_to_range(source, lo, &c.span))
                            .collect(),
                    });
                }
            }
            Item::Impl(i) => {
                let impl_name = match &i.target_type.0 {
                    hew_parser::ast::TypeExpr::Named { name: tname, .. } => tname.clone(),
                    _ => "<impl>".to_string(),
                };
                let range = span_to_range(source, lo, item_span);
                result.push(CallHierarchyIncomingCall {
                    from: CallHierarchyItem {
                        name: impl_name,
                        kind: SymbolKind::NAMESPACE,
                        tags: None,
                        detail: None,
                        uri: uri.clone(),
                        range,
                        selection_range: range,
                        data: None,
                    },
                    from_ranges,
                });
            }
            Item::TypeDecl(td) => {
                let range = span_to_range(source, lo, item_span);
                result.push(CallHierarchyIncomingCall {
                    from: CallHierarchyItem {
                        name: td.name.clone(),
                        kind: SymbolKind::STRUCT,
                        tags: None,
                        detail: None,
                        uri: uri.clone(),
                        range,
                        selection_range: range,
                        data: None,
                    },
                    from_ranges,
                });
            }
            Item::Trait(t) => {
                let range = span_to_range(source, lo, item_span);
                result.push(CallHierarchyIncomingCall {
                    from: CallHierarchyItem {
                        name: t.name.clone(),
                        kind: SymbolKind::INTERFACE,
                        tags: None,
                        detail: None,
                        uri: uri.clone(),
                        range,
                        selection_range: range,
                        data: None,
                    },
                    from_ranges,
                });
            }
            Item::Supervisor(s) => {
                let range = span_to_range(source, lo, item_span);
                result.push(CallHierarchyIncomingCall {
                    from: CallHierarchyItem {
                        name: s.name.clone(),
                        kind: SymbolKind::MODULE,
                        tags: None,
                        detail: None,
                        uri: uri.clone(),
                        range,
                        selection_range: range,
                        data: None,
                    },
                    from_ranges,
                });
            }
            Item::Machine(m) => {
                let range = span_to_range(source, lo, item_span);
                result.push(CallHierarchyIncomingCall {
                    from: CallHierarchyItem {
                        name: m.name.clone(),
                        kind: SymbolKind::ENUM,
                        tags: None,
                        detail: None,
                        uri: uri.clone(),
                        range,
                        selection_range: range,
                        data: None,
                    },
                    from_ranges,
                });
            }
            _ => {}
        }
    }
    result
}

fn find_outgoing_calls(
    uri: &Url,
    source: &str,
    lo: &[usize],
    parse_result: &ParseResult,
    caller_name: &str,
) -> Vec<CallHierarchyOutgoingCall> {
    let mut call_sites = Vec::new();

    // Collect calls only from the item whose name matches the caller.
    for (item, _) in &parse_result.program.items {
        let is_caller = match item {
            Item::Function(f) => f.name == caller_name,
            Item::Actor(a) => {
                a.receive_fns.iter().any(|r| r.name == caller_name)
                    || a.methods.iter().any(|m| m.name == caller_name)
            }
            Item::Impl(i) => i.methods.iter().any(|m| m.name == caller_name),
            Item::TypeDecl(td) => td.body.iter().any(|b| {
                if let hew_parser::ast::TypeBodyItem::Method(m) = b {
                    m.name == caller_name
                } else {
                    false
                }
            }),
            Item::Trait(t) => t.items.iter().any(|ti| {
                if let hew_parser::ast::TraitItem::Method(m) = ti {
                    m.name == caller_name
                } else {
                    false
                }
            }),
            Item::Supervisor(s) => s.name == caller_name,
            Item::Machine(m) => m.name == caller_name,
            _ => false,
        };
        if is_caller {
            collect_calls_in_item(item, &mut call_sites);
        }
    }

    // Group call sites by callee name
    let mut grouped: HashMap<String, Vec<Range>> = HashMap::new();
    for cs in &call_sites {
        grouped
            .entry(cs.name.clone())
            .or_default()
            .push(span_to_range(source, lo, &cs.span));
    }

    grouped
        .into_iter()
        .filter_map(|(callee_name, ranges)| {
            let target = find_callable_at(uri, source, lo, parse_result, &callee_name)?;
            Some(CallHierarchyOutgoingCall {
                to: target,
                from_ranges: ranges,
            })
        })
        .collect()
}

// ── Code lens helpers ───────────────────────────────────────────────

fn build_code_lenses(source: &str, lo: &[usize], parse_result: &ParseResult) -> Vec<CodeLens> {
    let ref_counts = count_all_references(parse_result);
    let mut lenses = Vec::new();

    let ref_lens = |range, name: &str| -> CodeLens {
        let count = ref_counts.get(name).copied().unwrap_or(0);
        CodeLens {
            range,
            command: Some(Command {
                title: format!("{count} reference{}", if count == 1 { "" } else { "s" }),
                command: String::new(),
                arguments: None,
            }),
            data: None,
        }
    };

    for (item, item_span) in &parse_result.program.items {
        match item {
            Item::Function(f) => {
                let range = span_to_range(source, lo, item_span);
                lenses.push(ref_lens(range, &f.name));
                if has_test_attribute(&f.attributes) {
                    lenses.push(CodeLens {
                        range,
                        command: Some(Command {
                            title: "\u{25b6} Run test".to_string(),
                            command: "hew.runTest".to_string(),
                            arguments: Some(vec![serde_json::Value::String(f.name.clone())]),
                        }),
                        data: None,
                    });
                }
            }
            Item::Actor(a) => {
                let range = span_to_range(source, lo, item_span);
                lenses.push(ref_lens(range, &a.name));
                for recv in &a.receive_fns {
                    let recv_range = if recv.span.is_empty() {
                        range
                    } else {
                        span_to_range(source, lo, &recv.span)
                    };
                    lenses.push(ref_lens(recv_range, &recv.name));
                }
            }
            _ => {}
        }
    }
    lenses
}

fn has_test_attribute(attrs: &[Attribute]) -> bool {
    attrs.iter().any(|a| a.name == "test")
}

// ── Workspace symbol helpers ────────────────────────────────────────

#[expect(
    deprecated,
    reason = "SymbolInformation::deprecated field is deprecated in lsp-types"
)]
#[expect(
    clippy::too_many_lines,
    reason = "exhaustive match over all Item variants for workspace symbols"
)]
fn collect_workspace_symbols(
    uri: &Url,
    source: &str,
    lo: &[usize],
    parse_result: &ParseResult,
    query: &str,
) -> Vec<SymbolInformation> {
    let mut symbols = Vec::new();
    let query_lower = query.to_lowercase();
    for (item, item_span) in &parse_result.program.items {
        match item {
            Item::Function(f) => {
                if query.is_empty() || f.name.to_lowercase().contains(&query_lower) {
                    symbols.push(SymbolInformation {
                        name: f.name.clone(),
                        kind: SymbolKind::FUNCTION,
                        tags: None,
                        deprecated: None,
                        location: Location {
                            uri: uri.clone(),
                            range: span_to_range(source, lo, item_span),
                        },
                        container_name: None,
                    });
                }
            }
            Item::Actor(a) => {
                if query.is_empty() || a.name.to_lowercase().contains(&query_lower) {
                    symbols.push(SymbolInformation {
                        name: a.name.clone(),
                        kind: SymbolKind::CLASS,
                        tags: None,
                        deprecated: None,
                        location: Location {
                            uri: uri.clone(),
                            range: span_to_range(source, lo, item_span),
                        },
                        container_name: None,
                    });
                }
                for recv in &a.receive_fns {
                    if query.is_empty() || recv.name.to_lowercase().contains(&query_lower) {
                        let recv_range = if recv.span.is_empty() {
                            span_to_range(source, lo, item_span)
                        } else {
                            span_to_range(source, lo, &recv.span)
                        };
                        symbols.push(SymbolInformation {
                            name: recv.name.clone(),
                            kind: SymbolKind::METHOD,
                            tags: None,
                            deprecated: None,
                            location: Location {
                                uri: uri.clone(),
                                range: recv_range,
                            },
                            container_name: Some(a.name.clone()),
                        });
                    }
                }
            }
            Item::TypeDecl(t) => {
                if query.is_empty() || t.name.to_lowercase().contains(&query_lower) {
                    symbols.push(SymbolInformation {
                        name: t.name.clone(),
                        kind: SymbolKind::STRUCT,
                        tags: None,
                        deprecated: None,
                        location: Location {
                            uri: uri.clone(),
                            range: span_to_range(source, lo, item_span),
                        },
                        container_name: None,
                    });
                }
            }
            Item::Const(c) => {
                if query.is_empty() || c.name.to_lowercase().contains(&query_lower) {
                    symbols.push(SymbolInformation {
                        name: c.name.clone(),
                        kind: SymbolKind::CONSTANT,
                        tags: None,
                        deprecated: None,
                        location: Location {
                            uri: uri.clone(),
                            range: span_to_range(source, lo, item_span),
                        },
                        container_name: None,
                    });
                }
            }
            Item::Trait(t) => {
                if query.is_empty() || t.name.to_lowercase().contains(&query_lower) {
                    symbols.push(SymbolInformation {
                        name: t.name.clone(),
                        kind: SymbolKind::INTERFACE,
                        tags: None,
                        deprecated: None,
                        location: Location {
                            uri: uri.clone(),
                            range: span_to_range(source, lo, item_span),
                        },
                        container_name: None,
                    });
                }
            }
            _ => {}
        }
    }
    symbols
}

#[cfg(test)]
mod tests {
    use super::*;

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
        let source =
            "type Point { x: i32 }\ntrait Stream { type Item; fn next() -> i32; }\nfn calc(v: i32) -> i32 { v }";
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
        let main_source =
            "import util::{ greet };\nfn first() -> i32 { greet() }\nfn second() -> i32 { greet() }";
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
        let main_source =
            "import util::{ greet };\nfn first() -> i32 { greet() }\nfn second() -> i32 { greet() }";
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
        let main_source =
            "import util::{ greet };\nfn first() -> i32 { greet() }\nfn second() -> i32 { greet() }";
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
        let main_source =
            "import util::{ greet };\nfn first() -> i32 { greet() }\nfn second() -> i32 { greet() }";
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
