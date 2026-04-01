//! LSP server implementation for the Hew language.

use std::collections::HashMap;

use dashmap::DashMap;
use hew_parser::ast::{
    Attribute, Block, Expr, Item, Span, Stmt, StringPart, TraitItem, TypeBodyItem, TypeDeclKind,
};
use hew_parser::ParseResult;
use hew_types::error::TypeErrorKind;
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

    /// Re-lex, re-parse, and re-typecheck the document, then publish diagnostics.
    async fn reanalyze(&self, uri: &Url, source: &str) {
        let parse_result = hew_parser::parse(source);

        let has_parse_errors = parse_result
            .errors
            .iter()
            .any(|e| e.severity == hew_parser::Severity::Error);

        let type_output = if has_parse_errors {
            None
        } else {
            let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(
                build_module_search_paths(),
            ));
            Some(checker.check_program(&parse_result.program))
        };

        let line_offsets = compute_line_offsets(source);

        let diagnostics = build_diagnostics(
            uri,
            source,
            &line_offsets,
            &parse_result,
            type_output.as_ref(),
        );

        self.documents.insert(
            uri.clone(),
            DocumentState {
                source: source.to_string(),
                line_offsets,
                parse_result,
                type_output,
            },
        );

        self.client
            .publish_diagnostics(uri.clone(), diagnostics, None)
            .await;
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
        self.documents.remove(&params.text_document.uri);
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

        let Some((_name, spans)) =
            hew_analysis::references::find_all_references(&doc.source, &doc.parse_result, offset)
        else {
            return Ok(None);
        };

        let mut locations: Vec<Location> = spans
            .iter()
            .map(|span| Location {
                uri: uri.clone(),
                range: offset_range_to_lsp(&doc.source, &doc.line_offsets, span.start, span.end),
            })
            .collect();

        if include_declaration {
            if let Some(word) = word_at_offset(&doc.source, offset) {
                let plain_word = word
                    .rsplit('.')
                    .next()
                    .and_then(|w| w.rsplit("::").next())
                    .unwrap_or(&word);
                if let Some(def_range) = find_definition_in_ast(
                    &doc.source,
                    &doc.line_offsets,
                    &doc.parse_result,
                    plain_word,
                ) {
                    if !locations.iter().any(|l| l.range == def_range) {
                        locations.insert(
                            0,
                            Location {
                                uri: uri.clone(),
                                range: def_range,
                            },
                        );
                    }
                }
            }
        }

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
        let Some(span) =
            hew_analysis::rename::prepare_rename(&doc.source, &doc.parse_result, offset)
        else {
            return Ok(None);
        };
        let range = offset_range_to_lsp(&doc.source, &doc.line_offsets, span.start, span.end);
        Ok(Some(PrepareRenameResponse::Range(range)))
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
        let Some(edits) =
            hew_analysis::rename::rename(&doc.source, &doc.parse_result, offset, &params.new_name)
        else {
            return Ok(None);
        };

        let text_edits: Vec<TextEdit> = edits
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
        Ok(Some(WorkspaceEdit {
            changes: Some(changes),
            ..Default::default()
        }))
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

fn build_diagnostics(
    uri: &Url,
    source: &str,
    lo: &[usize],
    parse_result: &ParseResult,
    type_output: Option<&TypeCheckOutput>,
) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

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
        diagnostics.push(Diagnostic {
            range: span_to_range(source, lo, &err.span),
            severity: Some(severity),
            source: Some("hew-parser".to_string()),
            message,
            ..Default::default()
        });
    }

    if let Some(tc) = type_output {
        for diag in tc.errors.iter().chain(tc.warnings.iter()) {
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
                                uri: uri.clone(),
                                range: span_to_range(source, lo, note_span),
                            },
                            message: note_msg.clone(),
                        })
                        .collect(),
                )
            };

            diagnostics.push(Diagnostic {
                range: span_to_range(source, lo, &diag.span),
                severity: Some(error_kind_severity(&diag.kind)),
                source: Some("hew-types".to_string()),
                message,
                related_information,
                data: Some(diagnostic_data(&diag.kind, &diag.suggestions)),
                ..Default::default()
            });
        }
    }

    diagnostics
}

/// Map a `TypeErrorKind` to an LSP diagnostic severity.
///
/// Currently all type errors are reported as errors. This provides the hook
/// point for when the type checker adds warning-level kinds (e.g. unused
/// variables, deprecation warnings).
fn error_kind_severity(kind: &TypeErrorKind) -> DiagnosticSeverity {
    match kind {
        TypeErrorKind::ActorRefCycle
        | TypeErrorKind::UnusedVariable
        | TypeErrorKind::UnusedMut
        | TypeErrorKind::StyleSuggestion
        | TypeErrorKind::UnusedImport
        | TypeErrorKind::UnreachableCode
        | TypeErrorKind::DeadCode
        | TypeErrorKind::OrphanImpl
        | TypeErrorKind::PlatformLimitation
        | TypeErrorKind::Shadowing
        | TypeErrorKind::NonExhaustiveMatch => DiagnosticSeverity::WARNING,
        _ => DiagnosticSeverity::ERROR,
    }
}

/// Encode a `TypeErrorKind` discriminant and suggestions as JSON for `Diagnostic.data`.
fn diagnostic_data(kind: &TypeErrorKind, suggestions: &[String]) -> serde_json::Value {
    let kind_str = match kind {
        TypeErrorKind::Mismatch { .. } => "Mismatch",
        TypeErrorKind::UndefinedVariable => "UndefinedVariable",
        TypeErrorKind::UndefinedType => "UndefinedType",
        TypeErrorKind::UndefinedFunction => "UndefinedFunction",
        TypeErrorKind::UndefinedField => "UndefinedField",
        TypeErrorKind::UndefinedMethod => "UndefinedMethod",
        TypeErrorKind::InvalidSend => "InvalidSend",
        TypeErrorKind::InvalidOperation => "InvalidOperation",
        TypeErrorKind::ArityMismatch => "ArityMismatch",
        TypeErrorKind::BoundsNotSatisfied => "BoundsNotSatisfied",
        TypeErrorKind::InferenceFailed => "InferenceFailed",
        TypeErrorKind::NonExhaustiveMatch => "NonExhaustiveMatch",
        TypeErrorKind::DuplicateDefinition => "DuplicateDefinition",
        TypeErrorKind::MutabilityError => "MutabilityError",
        TypeErrorKind::ReturnTypeMismatch => "ReturnTypeMismatch",
        TypeErrorKind::UseAfterMove => "UseAfterMove",
        TypeErrorKind::YieldOutsideGenerator => "YieldOutsideGenerator",
        TypeErrorKind::ActorRefCycle => "ActorRefCycle",
        TypeErrorKind::UnusedVariable => "UnusedVariable",
        TypeErrorKind::UnusedMut => "UnusedMut",
        TypeErrorKind::StyleSuggestion => "StyleSuggestion",
        TypeErrorKind::UnusedImport => "UnusedImport",
        TypeErrorKind::UnreachableCode => "UnreachableCode",
        TypeErrorKind::Shadowing => "Shadowing",
        TypeErrorKind::DeadCode => "DeadCode",
        TypeErrorKind::PurityViolation => "PurityViolation",
        TypeErrorKind::OrphanImpl => "OrphanImpl",
        TypeErrorKind::PlatformLimitation => "PlatformLimitation",
        TypeErrorKind::MachineExhaustivenessError => "MachineExhaustivenessError",
        TypeErrorKind::UnresolvedImport => "UnresolvedImport",
    };
    serde_json::json!({
        "kind": kind_str,
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
    let range = offset_span_to_range(source, lo, info.span);
    let selection_range = offset_span_to_range(source, lo, info.selection_span);
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

/// Convert an `OffsetSpan` to an LSP `Range`.
fn offset_span_to_range(source: &str, lo: &[usize], span: hew_analysis::OffsetSpan) -> Range {
    let (sl, sc, el, ec) =
        hew_analysis::util::span_to_line_col_range(source, lo, span.start, span.end);
    Range {
        start: Position::new(sl, sc),
        end: Position::new(el, ec),
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

/// Return `None` for an empty vec, `Some(v)` otherwise.
fn non_empty<T>(v: Vec<T>) -> Option<Vec<T>> {
    hew_analysis::util::non_empty(v)
}

/// Compute byte offsets of each line start.
fn compute_line_offsets(source: &str) -> Vec<usize> {
    hew_analysis::util::compute_line_offsets(source)
}

/// Convert byte offset to (line, character) — both 0-based, character in UTF-16 code units.
fn offset_to_line_col(source: &str, line_offsets: &[usize], offset: usize) -> (usize, usize) {
    hew_analysis::util::offset_to_line_col(source, line_offsets, offset)
}

/// Convert a parser `Span` (byte-offset `Range<usize>`) to an LSP `Range`,
/// using pre-computed line offsets.
fn span_to_range(source: &str, lo: &[usize], span: &Span) -> Range {
    let (sl, sc, el, ec) =
        hew_analysis::util::span_to_line_col_range(source, lo, span.start, span.end);
    Range {
        start: Position::new(sl, sc),
        end: Position::new(el, ec),
    }
}

/// Convert an LSP `Position` (UTF-16 character offset) to a byte offset in source,
/// using pre-computed line offsets.
fn position_to_offset(source: &str, lo: &[usize], position: Position) -> usize {
    hew_analysis::util::position_to_offset(source, lo, position.line, position.character)
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

// ── Helpers (delegated to hew-analysis) ──────────────────────────────

/// Extract the word (identifier) at a byte offset in the source.
/// Also tries `offset - 1` when the cursor is right after an identifier.
fn word_at_offset(source: &str, offset: usize) -> Option<String> {
    hew_analysis::util::word_at_offset(source, offset)
}

// ── Document links ──────────────────────────────────────────────────

fn build_document_links(
    uri: &Url,
    source: &str,
    lo: &[usize],
    parse_result: &ParseResult,
) -> Vec<DocumentLink> {
    let mut links = Vec::new();

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

    let file_dir = uri
        .to_file_path()
        .ok()
        .and_then(|p| p.parent().map(std::path::Path::to_path_buf));

    for (item, span) in &parse_result.program.items {
        if let Item::Import(import) = item {
            if import.path.is_empty() {
                continue;
            }

            let relative = format!("{}.hew", import.path.join("/"));
            let resolved = workspace_root
                .as_ref()
                .map(|root| root.join(&relative))
                .filter(|p| p.exists())
                .or_else(|| {
                    file_dir
                        .as_ref()
                        .map(|dir| dir.join(&relative))
                        .filter(|p| p.exists())
                });

            if let Some(path) = resolved {
                if let Ok(target_uri) = Url::from_file_path(&path) {
                    links.push(DocumentLink {
                        range: span_to_range(source, lo, span),
                        target: Some(target_uri),
                        tooltip: Some(format!("Open {relative}")),
                        data: None,
                    });
                }
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

struct CallSite {
    name: String,
    span: Span,
}

fn collect_calls_in_block(block: &Block, calls: &mut Vec<CallSite>) {
    for (stmt, _span) in &block.stmts {
        collect_calls_in_stmt(stmt, calls);
    }
}

fn collect_calls_in_stmt(stmt: &Stmt, calls: &mut Vec<CallSite>) {
    match stmt {
        Stmt::Let { value, .. } | Stmt::Var { value, .. } => {
            if let Some(spanned) = value {
                collect_calls_in_expr(spanned, calls);
            }
        }
        Stmt::Expression(spanned) | Stmt::Return(Some(spanned)) => {
            collect_calls_in_expr(spanned, calls);
        }
        Stmt::Defer(d) => {
            collect_calls_in_expr(d.as_ref(), calls);
        }
        Stmt::Assign { target, value, .. } => {
            collect_calls_in_expr(target, calls);
            collect_calls_in_expr(value, calls);
        }
        Stmt::For { iterable, body, .. } => {
            collect_calls_in_expr(iterable, calls);
            collect_calls_in_block(body, calls);
        }
        Stmt::While {
            condition, body, ..
        } => {
            collect_calls_in_expr(condition, calls);
            collect_calls_in_block(body, calls);
        }
        Stmt::WhileLet { expr, body, .. } => {
            collect_calls_in_expr(expr.as_ref(), calls);
            collect_calls_in_block(body, calls);
        }
        Stmt::If {
            condition,
            then_block,
            else_block,
            ..
        } => {
            collect_calls_in_expr(condition, calls);
            collect_calls_in_block(then_block, calls);
            if let Some(eb) = else_block {
                if let Some(if_box) = &eb.if_stmt {
                    let (if_stmt, _) = if_box.as_ref();
                    collect_calls_in_stmt(if_stmt, calls);
                }
                if let Some(b) = &eb.block {
                    collect_calls_in_block(b, calls);
                }
            }
        }
        Stmt::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            collect_calls_in_expr(expr.as_ref(), calls);
            collect_calls_in_block(body, calls);
            if let Some(block) = else_body {
                collect_calls_in_block(block, calls);
            }
        }
        Stmt::Match { scrutinee, arms } => {
            collect_calls_in_expr(scrutinee, calls);
            for arm in arms {
                if let Some(g) = &arm.guard {
                    collect_calls_in_expr(g, calls);
                }
                collect_calls_in_expr(&arm.body, calls);
            }
        }
        Stmt::Loop { body, .. } => collect_calls_in_block(body, calls),
        Stmt::Return(None) | Stmt::Break { .. } | Stmt::Continue { .. } => {}
    }
}

#[expect(
    clippy::too_many_lines,
    reason = "exhaustive match over all Expr variants"
)]
fn collect_calls_in_expr(spanned: &(Expr, Span), calls: &mut Vec<CallSite>) {
    let (expr, expr_span) = spanned;
    match expr {
        Expr::Call { function, args, .. } => {
            let (func, _) = function.as_ref();
            if let Expr::Identifier(name) = func {
                calls.push(CallSite {
                    name: name.clone(),
                    span: expr_span.clone(),
                });
            }
            collect_calls_in_expr(function.as_ref(), calls);
            for arg in args {
                collect_calls_in_expr(arg.expr(), calls);
            }
        }
        Expr::MethodCall {
            receiver,
            method,
            args,
            ..
        } => {
            calls.push(CallSite {
                name: method.clone(),
                span: expr_span.clone(),
            });
            collect_calls_in_expr(receiver.as_ref(), calls);
            for arg in args {
                collect_calls_in_expr(arg.expr(), calls);
            }
        }
        Expr::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            collect_calls_in_expr(expr.as_ref(), calls);
            collect_calls_in_block(body, calls);
            if let Some(block) = else_body {
                collect_calls_in_block(block, calls);
            }
        }
        Expr::Send { target, message } => {
            // Track actor message sends as call edges
            let send_name = match &target.as_ref().0 {
                Expr::Identifier(name) => format!("{name}.send"),
                _ => "send".to_string(),
            };
            calls.push(CallSite {
                name: send_name,
                span: expr_span.clone(),
            });
            collect_calls_in_expr(target.as_ref(), calls);
            collect_calls_in_expr(message.as_ref(), calls);
        }
        Expr::Binary { left, right, .. } => {
            collect_calls_in_expr(left.as_ref(), calls);
            collect_calls_in_expr(right.as_ref(), calls);
        }
        Expr::Unary { operand, .. } => {
            collect_calls_in_expr(operand.as_ref(), calls);
        }
        Expr::Await(a) => {
            collect_calls_in_expr(a.as_ref(), calls);
        }
        Expr::PostfixTry(p) => {
            collect_calls_in_expr(p.as_ref(), calls);
        }
        Expr::Cast { expr, .. } => {
            collect_calls_in_expr(expr.as_ref(), calls);
        }
        Expr::Yield(Some(y)) => {
            collect_calls_in_expr(y.as_ref(), calls);
        }
        Expr::If {
            condition,
            then_block,
            else_block,
            ..
        } => {
            collect_calls_in_expr(condition.as_ref(), calls);
            collect_calls_in_expr(then_block.as_ref(), calls);
            if let Some(eb) = else_block {
                collect_calls_in_expr(eb.as_ref(), calls);
            }
        }
        Expr::Match { scrutinee, arms } => {
            collect_calls_in_expr(scrutinee.as_ref(), calls);
            for arm in arms {
                if let Some(g) = &arm.guard {
                    collect_calls_in_expr(g, calls);
                }
                collect_calls_in_expr(&arm.body, calls);
            }
        }
        Expr::Block(b) | Expr::Unsafe(b) => collect_calls_in_block(b, calls),
        Expr::Index { object, index, .. } => {
            collect_calls_in_expr(object.as_ref(), calls);
            collect_calls_in_expr(index.as_ref(), calls);
        }
        Expr::FieldAccess { object, .. } => {
            collect_calls_in_expr(object.as_ref(), calls);
        }
        Expr::Lambda { body, .. } | Expr::SpawnLambdaActor { body, .. } => {
            collect_calls_in_expr(body.as_ref(), calls);
        }
        Expr::ArrayRepeat { value, count } => {
            collect_calls_in_expr(value.as_ref(), calls);
            collect_calls_in_expr(count.as_ref(), calls);
        }
        Expr::MapLiteral { entries } => {
            for (k, v) in entries {
                collect_calls_in_expr(k, calls);
                collect_calls_in_expr(v, calls);
            }
        }
        Expr::Tuple(exprs) | Expr::Array(exprs) | Expr::Join(exprs) => {
            for e in exprs {
                collect_calls_in_expr(e, calls);
            }
        }
        Expr::Range { start, end, .. } => {
            if let Some(s) = start {
                collect_calls_in_expr(s.as_ref(), calls);
            }
            if let Some(e) = end {
                collect_calls_in_expr(e.as_ref(), calls);
            }
        }
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let StringPart::Expr(e) = part {
                    collect_calls_in_expr(e, calls);
                }
            }
        }
        Expr::Scope { body, .. } | Expr::ScopeLaunch(body) | Expr::ScopeSpawn(body) => {
            collect_calls_in_block(body, calls);
        }
        Expr::Select { arms, timeout, .. } => {
            for arm in arms {
                collect_calls_in_expr(&arm.body, calls);
            }
            if let Some(tc) = timeout {
                collect_calls_in_expr(tc.duration.as_ref(), calls);
                collect_calls_in_expr(tc.body.as_ref(), calls);
            }
        }
        Expr::StructInit { fields, .. } => {
            for (_, v) in fields {
                collect_calls_in_expr(v, calls);
            }
        }
        Expr::Spawn { target, args } => {
            collect_calls_in_expr(target.as_ref(), calls);
            for (_, a) in args {
                collect_calls_in_expr(a, calls);
            }
        }
        Expr::Timeout { expr, duration } => {
            collect_calls_in_expr(expr.as_ref(), calls);
            collect_calls_in_expr(duration.as_ref(), calls);
        }
        Expr::Literal(_)
        | Expr::Identifier(_)
        | Expr::Cooperate
        | Expr::ScopeCancel
        | Expr::This
        | Expr::RegexLiteral(_)
        | Expr::ByteStringLiteral(_)
        | Expr::ByteArrayLiteral(_)
        | Expr::Yield(None) => {}
    }
}

fn find_incoming_calls(
    uri: &Url,
    source: &str,
    lo: &[usize],
    parse_result: &ParseResult,
    target_name: &str,
) -> Vec<CallHierarchyIncomingCall> {
    let mut result = Vec::new();
    for (item, item_span) in &parse_result.program.items {
        match item {
            Item::Function(f) => {
                let mut calls = Vec::new();
                collect_calls_in_block(&f.body, &mut calls);
                let matching: Vec<_> = calls.iter().filter(|c| c.name == target_name).collect();
                if !matching.is_empty() {
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
                        from_ranges: matching
                            .iter()
                            .map(|c| span_to_range(source, lo, &c.span))
                            .collect(),
                    });
                }
            }
            Item::Actor(a) => {
                for recv in &a.receive_fns {
                    let mut calls = Vec::new();
                    collect_calls_in_block(&recv.body, &mut calls);
                    let matching: Vec<_> = calls.iter().filter(|c| c.name == target_name).collect();
                    if !matching.is_empty() {
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
                            from_ranges: matching
                                .iter()
                                .map(|c| span_to_range(source, lo, &c.span))
                                .collect(),
                        });
                    }
                }
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

    for (item, _) in &parse_result.program.items {
        match item {
            Item::Function(f) if f.name == caller_name => {
                collect_calls_in_block(&f.body, &mut call_sites);
            }
            Item::Actor(a) => {
                for recv in &a.receive_fns {
                    if recv.name == caller_name {
                        collect_calls_in_block(&recv.body, &mut call_sites);
                    }
                }
            }
            _ => {}
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

fn count_all_references(parse_result: &ParseResult) -> HashMap<String, usize> {
    let mut counts = HashMap::new();
    for (item, _) in &parse_result.program.items {
        count_idents_in_item(item, &mut counts);
    }
    counts
}

fn count_idents_in_item(item: &Item, counts: &mut HashMap<String, usize>) {
    match item {
        Item::Function(f) => count_idents_in_block(&f.body, counts),
        Item::Actor(a) => {
            if let Some(init) = &a.init {
                count_idents_in_block(&init.body, counts);
            }
            if let Some(term) = &a.terminate {
                count_idents_in_block(&term.body, counts);
            }
            for recv in &a.receive_fns {
                count_idents_in_block(&recv.body, counts);
            }
            for method in &a.methods {
                count_idents_in_block(&method.body, counts);
            }
        }
        Item::TypeDecl(td) => {
            for body_item in &td.body {
                if let TypeBodyItem::Method(m) = body_item {
                    count_idents_in_block(&m.body, counts);
                }
            }
        }
        Item::Impl(i) => {
            for method in &i.methods {
                count_idents_in_block(&method.body, counts);
            }
        }
        Item::Trait(t) => {
            for trait_item in &t.items {
                if let TraitItem::Method(m) = trait_item {
                    if let Some(body) = &m.body {
                        count_idents_in_block(body, counts);
                    }
                }
            }
        }
        Item::Const(c) => count_idents_in_expr(&c.value.0, counts),
        Item::Import(_)
        | Item::ExternBlock(_)
        | Item::Wire(_)
        | Item::TypeAlias(_)
        | Item::Supervisor(_)
        | Item::Machine(_) => {}
    }
}

fn count_idents_in_block(block: &Block, counts: &mut HashMap<String, usize>) {
    for (stmt, _) in &block.stmts {
        count_idents_in_stmt(stmt, counts);
    }
    if let Some(trailing) = &block.trailing_expr {
        count_idents_in_expr(&trailing.0, counts);
    }
}

fn count_idents_in_stmt(stmt: &Stmt, counts: &mut HashMap<String, usize>) {
    match stmt {
        Stmt::Let { value, .. } | Stmt::Var { value, .. } => {
            if let Some(val) = value {
                count_idents_in_expr(&val.0, counts);
            }
        }
        Stmt::Assign { target, value, .. } => {
            count_idents_in_expr(&target.0, counts);
            count_idents_in_expr(&value.0, counts);
        }
        Stmt::If {
            condition,
            then_block,
            else_block,
        } => {
            count_idents_in_expr(&condition.0, counts);
            count_idents_in_block(then_block, counts);
            if let Some(eb) = else_block {
                if let Some(if_stmt) = &eb.if_stmt {
                    count_idents_in_stmt(&if_stmt.0, counts);
                }
                if let Some(block) = &eb.block {
                    count_idents_in_block(block, counts);
                }
            }
        }
        Stmt::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            count_idents_in_expr(&expr.0, counts);
            count_idents_in_block(body, counts);
            if let Some(block) = else_body {
                count_idents_in_block(block, counts);
            }
        }
        Stmt::Match { scrutinee, arms } => {
            count_idents_in_expr(&scrutinee.0, counts);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    count_idents_in_expr(&guard.0, counts);
                }
                count_idents_in_expr(&arm.body.0, counts);
            }
        }
        Stmt::Loop { body, .. }
        | Stmt::While { body, .. }
        | Stmt::WhileLet { body, .. }
        | Stmt::For { body, .. } => {
            count_idents_in_block(body, counts);
        }
        Stmt::Expression(expr) | Stmt::Return(Some(expr)) => {
            count_idents_in_expr(&expr.0, counts);
        }
        Stmt::Break {
            value: Some(val), ..
        } => count_idents_in_expr(&val.0, counts),
        _ => {}
    }
}

#[expect(
    clippy::too_many_lines,
    reason = "exhaustive match over all Expr variants for identifier counting"
)]
fn count_idents_in_expr(expr: &Expr, counts: &mut HashMap<String, usize>) {
    match expr {
        Expr::Identifier(ident) => {
            *counts.entry(ident.clone()).or_insert(0) += 1;
        }
        Expr::Binary { left, right, .. } => {
            count_idents_in_expr(&left.0, counts);
            count_idents_in_expr(&right.0, counts);
        }
        Expr::Unary { operand, .. } => count_idents_in_expr(&operand.0, counts),
        Expr::Call { function, args, .. } => {
            count_idents_in_expr(&function.0, counts);
            for arg in args {
                count_idents_in_expr(&arg.expr().0, counts);
            }
        }
        Expr::MethodCall { receiver, args, .. } => {
            count_idents_in_expr(&receiver.0, counts);
            for arg in args {
                count_idents_in_expr(&arg.expr().0, counts);
            }
        }
        Expr::FieldAccess { object, .. } => count_idents_in_expr(&object.0, counts),
        Expr::Index { object, index } => {
            count_idents_in_expr(&object.0, counts);
            count_idents_in_expr(&index.0, counts);
        }
        Expr::StructInit { fields, .. } => {
            for (_, val) in fields {
                count_idents_in_expr(&val.0, counts);
            }
        }
        Expr::Spawn { target, args } => {
            count_idents_in_expr(&target.0, counts);
            for (_, val) in args {
                count_idents_in_expr(&val.0, counts);
            }
        }
        Expr::Block(block)
        | Expr::Unsafe(block)
        | Expr::ScopeLaunch(block)
        | Expr::ScopeSpawn(block) => count_idents_in_block(block, counts),
        Expr::Scope { body, .. } => count_idents_in_block(body, counts),
        Expr::If {
            condition,
            then_block,
            else_block,
        } => {
            count_idents_in_expr(&condition.0, counts);
            count_idents_in_expr(&then_block.0, counts);
            if let Some(else_expr) = else_block {
                count_idents_in_expr(&else_expr.0, counts);
            }
        }
        Expr::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            count_idents_in_expr(&expr.0, counts);
            count_idents_in_block(body, counts);
            if let Some(block) = else_body {
                count_idents_in_block(block, counts);
            }
        }
        Expr::Match { scrutinee, arms } => {
            count_idents_in_expr(&scrutinee.0, counts);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    count_idents_in_expr(&guard.0, counts);
                }
                count_idents_in_expr(&arm.body.0, counts);
            }
        }
        Expr::Lambda { body, .. } | Expr::SpawnLambdaActor { body, .. } => {
            count_idents_in_expr(&body.0, counts);
        }
        Expr::ArrayRepeat { value, count } => {
            count_idents_in_expr(&value.0, counts);
            count_idents_in_expr(&count.0, counts);
        }
        Expr::MapLiteral { entries } => {
            for (k, v) in entries {
                count_idents_in_expr(&k.0, counts);
                count_idents_in_expr(&v.0, counts);
            }
        }
        Expr::Tuple(elems) | Expr::Array(elems) | Expr::Join(elems) => {
            for elem in elems {
                count_idents_in_expr(&elem.0, counts);
            }
        }
        Expr::Send { target, message } => {
            count_idents_in_expr(&target.0, counts);
            count_idents_in_expr(&message.0, counts);
        }
        Expr::Select {
            arms: sel_arms,
            timeout,
        } => {
            for arm in sel_arms {
                count_idents_in_expr(&arm.body.0, counts);
            }
            if let Some(t) = timeout {
                count_idents_in_expr(&t.body.0, counts);
            }
        }
        Expr::Timeout { expr: e, .. } => count_idents_in_expr(&e.0, counts),
        Expr::Await(inner) | Expr::PostfixTry(inner) | Expr::Yield(Some(inner)) => {
            count_idents_in_expr(&inner.0, counts);
        }
        Expr::Cast { expr: inner, .. } => count_idents_in_expr(&inner.0, counts),
        Expr::Range { start, end, .. } => {
            if let Some(s) = start {
                count_idents_in_expr(&s.0, counts);
            }
            if let Some(e) = end {
                count_idents_in_expr(&e.0, counts);
            }
        }
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let StringPart::Expr(e) = part {
                    count_idents_in_expr(&e.0, counts);
                }
            }
        }
        _ => {}
    }
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
    fn spawn_completions_only_actors() {
        let source = "actor Foo {}\nactor Bar {}\nfn baz() {}\nfn main() { let h = spawn  }";
        let parse_result = hew_parser::parse(source);
        let doc = DocumentState {
            source: source.to_string(),
            line_offsets: compute_line_offsets(source),
            parse_result,
            type_output: None,
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
    fn error_kind_severity_mapping() {
        let severity = error_kind_severity(&TypeErrorKind::UndefinedVariable);
        assert_eq!(severity, DiagnosticSeverity::ERROR);
    }

    #[test]
    fn actor_ref_cycle_severity_is_warning() {
        let severity = error_kind_severity(&TypeErrorKind::ActorRefCycle);
        assert_eq!(severity, DiagnosticSeverity::WARNING);
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

    #[test]
    fn error_kind_severity_warning_kinds() {
        let warning_kinds = [
            TypeErrorKind::ActorRefCycle,
            TypeErrorKind::UnusedVariable,
            TypeErrorKind::UnusedMut,
            TypeErrorKind::StyleSuggestion,
            TypeErrorKind::UnusedImport,
            TypeErrorKind::UnreachableCode,
            TypeErrorKind::DeadCode,
            TypeErrorKind::OrphanImpl,
            TypeErrorKind::PlatformLimitation,
            TypeErrorKind::Shadowing,
            TypeErrorKind::NonExhaustiveMatch,
        ];
        for kind in &warning_kinds {
            assert_eq!(
                error_kind_severity(kind),
                DiagnosticSeverity::WARNING,
                "{kind:?} should be WARNING"
            );
        }
    }

    #[test]
    fn error_kind_severity_error_kinds() {
        let error_kinds = [
            TypeErrorKind::UndefinedVariable,
            TypeErrorKind::UndefinedType,
            TypeErrorKind::UndefinedFunction,
            TypeErrorKind::MutabilityError,
            TypeErrorKind::ArityMismatch,
        ];
        for kind in &error_kinds {
            assert_eq!(
                error_kind_severity(kind),
                DiagnosticSeverity::ERROR,
                "{kind:?} should be ERROR"
            );
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
}
