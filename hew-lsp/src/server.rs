//! LSP server implementation for the Hew language.

use std::collections::{HashMap, HashSet};

use dashmap::DashMap;
use hew_lexer::Token;
use hew_parser::ast::{
    ActorDecl, Attribute, Block, Expr, Item, Pattern, Span, Stmt, StringPart, TraitItem,
    TypeBodyItem, TypeDeclKind,
};
use hew_parser::ParseResult;
use hew_types::check::{FnSig, SpanKey};
use hew_types::error::TypeErrorKind;
use hew_types::{Checker, Ty, TypeCheckOutput};
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

// ── Document state ───────────────────────────────────────────────────

#[derive(Debug)]
struct DocumentState {
    source: String,
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
            let mut checker = Checker::new();
            Some(checker.check_program(&parse_result.program))
        };

        let diagnostics = build_diagnostics(uri, source, &parse_result, type_output.as_ref());

        self.documents.insert(
            uri.clone(),
            DocumentState {
                source: source.to_string(),
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
                let offset = position_to_offset(&doc.source, position);
                build_completions_at(&doc, offset)
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

        let offset = position_to_offset(&doc.source, position);

        // Find the word under the cursor for function/type lookup.
        let word = word_at_offset(&doc.source, offset);

        let Some(type_output) = &doc.type_output else {
            return Ok(None);
        };

        // Check if the word is a known function — show its full signature.
        if let Some(word) = &word {
            if let Some(sig) = type_output.fn_sigs.get(word.as_str()) {
                let hover_text = format_fn_signature(word, sig);
                return Ok(Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: hover_text,
                    }),
                    range: None,
                }));
            }

            // Check if the word is a known type definition.
            if let Some(type_def) = type_output.type_defs.get(word.as_str()) {
                let hover_text = format_type_def_hover(type_def);
                return Ok(Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: hover_text,
                    }),
                    range: None,
                }));
            }
        }

        // Fall back to narrowest expression type that covers this offset.
        let mut best: Option<(&SpanKey, &Ty)> = None;
        for (span_key, ty) in &type_output.expr_types {
            if span_key.start <= offset && offset <= span_key.end {
                match best {
                    Some((prev, _))
                        if (span_key.end - span_key.start) < (prev.end - prev.start) =>
                    {
                        best = Some((span_key, ty));
                    }
                    None => {
                        best = Some((span_key, ty));
                    }
                    _ => {}
                }
            }
        }

        Ok(best.map(|(span_key, ty)| {
            let range = offset_range_to_lsp(&doc.source, span_key.start, span_key.end);
            let snippet = &doc.source[span_key.start..span_key.end];
            let value = if let Ty::Function { params, ret } = ty {
                let param_list: Vec<String> = params.iter().map(ToString::to_string).collect();
                format!(
                    "```hew\nfn {snippet}({}) -> {ret}\n```",
                    param_list.join(", ")
                )
            } else {
                format!("```hew\n{snippet}: {ty}\n```")
            };
            Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value,
                }),
                range: Some(range),
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

        let offset = position_to_offset(&doc.source, position);
        let Some(word) = word_at_offset(&doc.source, offset) else {
            return Ok(None);
        };

        if let Some(range) = find_definition_in_ast(&doc.source, &doc.parse_result, &word) {
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
                    if let Some(range) =
                        find_definition_in_ast(&doc.source, &doc.parse_result, method)
                    {
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

        let symbols = build_document_symbols(&doc.source, &doc.parse_result);
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

        let tokens = build_semantic_tokens(&doc.source, &doc.parse_result);
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

        let links = build_document_links(uri, &doc.source, &doc.parse_result);
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

        let offset = position_to_offset(&doc.source, position);
        let Some(word) = word_at_offset(&doc.source, offset) else {
            return Ok(None);
        };

        let item = find_type_hierarchy_item(uri, &doc.source, &doc.parse_result, &word);
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

        let supers = collect_supertypes(&item.uri, &item.name, &doc.source, &doc.parse_result);
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

        let subs = collect_subtypes(&item.uri, &item.name, &doc.source, &doc.parse_result);
        Ok(non_empty(subs))
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        let Some(doc) = self.documents.get(uri) else {
            return Ok(None);
        };

        let offset = position_to_offset(&doc.source, position);
        let include_declaration = params.context.include_declaration;

        let Some((_name, spans)) = find_all_references(&doc.source, &doc.parse_result, offset)
        else {
            return Ok(None);
        };

        let mut locations: Vec<Location> = spans
            .iter()
            .map(|span| Location {
                uri: uri.clone(),
                range: span_to_range(&doc.source, span),
            })
            .collect();

        if include_declaration {
            if let Some(word) = word_at_offset(&doc.source, offset) {
                let plain_word = word
                    .rsplit('.')
                    .next()
                    .and_then(|w| w.rsplit("::").next())
                    .unwrap_or(&word);
                if let Some(def_range) =
                    find_definition_in_ast(&doc.source, &doc.parse_result, plain_word)
                {
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
        let position = params.position;

        let Some(doc) = self.documents.get(uri) else {
            return Ok(None);
        };

        let offset = position_to_offset(&doc.source, position);
        let Some(word) = word_at_offset(&doc.source, offset) else {
            return Ok(None);
        };

        if word.contains('.') || word.contains("::") {
            return Ok(None);
        }

        if find_all_references(&doc.source, &doc.parse_result, offset).is_none() {
            return Ok(None);
        }

        let range = word_range_at_offset(&doc.source, offset);
        Ok(range.map(PrepareRenameResponse::Range))
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let new_name = &params.new_name;

        let Some(doc) = self.documents.get(uri) else {
            return Ok(None);
        };

        let offset = position_to_offset(&doc.source, position);
        let Some((name, spans)) = find_all_references(&doc.source, &doc.parse_result, offset)
        else {
            return Ok(None);
        };

        let mut edits: Vec<TextEdit> = spans
            .iter()
            .map(|span| TextEdit {
                range: span_to_range(&doc.source, span),
                new_text: new_name.clone(),
            })
            .collect();

        if let Some(def_range) = find_definition_in_ast(&doc.source, &doc.parse_result, &name) {
            if !edits.iter().any(|e| e.range == def_range) {
                edits.push(TextEdit {
                    range: def_range,
                    new_text: new_name.clone(),
                });
            }
        }

        if edits.is_empty() {
            return Ok(None);
        }

        edits.sort_by(|a, b| {
            a.range
                .start
                .line
                .cmp(&b.range.start.line)
                .then(a.range.start.character.cmp(&b.range.start.character))
        });
        edits.dedup_by(|a, b| a.range == b.range);

        let mut changes = HashMap::new();
        changes.insert(uri.clone(), edits);
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

        let offset = position_to_offset(&doc.source, pos);
        let word = word_at_offset(&doc.source, offset);
        let Some(word) = word else {
            return Ok(None);
        };

        let item = find_callable_at(&uri, &doc.source, &doc.parse_result, &word);
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

        let calls = find_incoming_calls(&item.uri, &doc.source, &doc.parse_result, &item.name);
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

        let calls = find_outgoing_calls(&item.uri, &doc.source, &doc.parse_result, &item.name);
        Ok(non_empty(calls))
    }

    async fn code_lens(&self, params: CodeLensParams) -> Result<Option<Vec<CodeLens>>> {
        let uri = params.text_document.uri;
        let Some(doc) = self.documents.get(&uri) else {
            return Ok(None);
        };

        let lenses = build_code_lenses(&doc.source, &doc.parse_result);
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
                &doc.parse_result,
                query,
            ));
        }
        Ok(non_empty(symbols))
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let uri = &params.text_document.uri;
        let Some(doc) = self.documents.get(uri) else {
            return Ok(None);
        };
        let Some(tc) = &doc.type_output else {
            return Ok(None);
        };
        let hints = build_inlay_hints(&doc.source, &doc.parse_result, tc);
        Ok(non_empty(hints))
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
        let offset = position_to_offset(&doc.source, position);
        Ok(build_signature_help(&doc.source, tc, offset))
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        let uri = &params.text_document.uri;
        let Some(doc) = self.documents.get(uri) else {
            return Ok(None);
        };

        let actions = build_code_actions(uri, &doc.source, &params.context.diagnostics);
        Ok(non_empty(actions))
    }

    async fn folding_range(&self, params: FoldingRangeParams) -> Result<Option<Vec<FoldingRange>>> {
        let uri = &params.text_document.uri;
        let Some(doc) = self.documents.get(uri) else {
            return Ok(None);
        };

        let ranges = build_folding_ranges(&doc.source, &doc.parse_result);
        Ok(non_empty(ranges))
    }
}

// ── Diagnostics ──────────────────────────────────────────────────────

fn build_diagnostics(
    uri: &Url,
    source: &str,
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
            range: span_to_range(source, &err.span),
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
                                range: span_to_range(source, note_span),
                            },
                            message: note_msg.clone(),
                        })
                        .collect(),
                )
            };

            diagnostics.push(Diagnostic {
                range: span_to_range(source, &diag.span),
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
        | TypeErrorKind::Shadowing
        | TypeErrorKind::DeadCode
        | TypeErrorKind::OrphanImpl
        | TypeErrorKind::PlatformLimitation => DiagnosticSeverity::WARNING,
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
    };
    serde_json::json!({
        "kind": kind_str,
        "suggestions": suggestions,
    })
}

// ── Code Actions ─────────────────────────────────────────────────────

/// Build code actions from LSP diagnostics by inspecting their `.data` field.
fn build_code_actions(
    uri: &Url,
    source: &str,
    diagnostics: &[Diagnostic],
) -> Vec<CodeActionOrCommand> {
    let mut actions = Vec::new();
    for diag in diagnostics {
        let Some(data) = diag.data.as_ref() else {
            continue;
        };
        let kind_str = data.get("kind").and_then(serde_json::Value::as_str);
        let suggestions: Vec<String> = data
            .get("suggestions")
            .and_then(|v| serde_json::from_value(v.clone()).ok())
            .unwrap_or_default();

        match kind_str {
            Some("UndefinedVariable" | "UndefinedFunction") => {
                for suggestion in &suggestions {
                    let name = extract_suggestion_name(suggestion);
                    actions.push(CodeActionOrCommand::CodeAction(make_replace_action(
                        uri,
                        diag,
                        &format!("Replace with `{name}`"),
                        diag.range,
                        name,
                    )));
                }
            }
            Some("MutabilityError") => {
                // Extract the variable name from "cannot assign to immutable variable `name`"
                let var_name = diag.message.split('`').nth(1);
                if let Some(let_range) = find_let_keyword(source, diag.range, var_name) {
                    actions.push(CodeActionOrCommand::CodeAction(make_replace_action(
                        uri,
                        diag,
                        "Change `let` to `var`",
                        let_range,
                        "var".to_string(),
                    )));
                }
            }
            Some("UnusedVariable") => {
                // Extract the variable name from the message (format: "unused variable: `name`")
                if let Some(name) = diag.message.split('`').nth(1) {
                    if !name.starts_with('_') {
                        let prefixed = format!("_{name}");
                        // Find the exact position of the variable name near the diagnostic start
                        let start_offset = position_to_offset(source, diag.range.start);
                        let end_offset = position_to_offset(source, diag.range.end);
                        let region = &source[start_offset..end_offset];
                        if let Some(rel) = region.find(name) {
                            let abs_start = start_offset + rel;
                            let abs_end = abs_start + name.len();
                            let line_offsets = compute_line_offsets(source);
                            let (sl, sc) = offset_to_line_col(source, &line_offsets, abs_start);
                            let (el, ec) = offset_to_line_col(source, &line_offsets, abs_end);
                            #[expect(
                                clippy::cast_possible_truncation,
                                reason = "line/col values fit u32"
                            )]
                            let name_range = Range {
                                start: Position::new(sl as u32, sc as u32),
                                end: Position::new(el as u32, ec as u32),
                            };
                            actions.push(CodeActionOrCommand::CodeAction(make_replace_action(
                                uri,
                                diag,
                                "Prefix with `_`",
                                name_range,
                                prefixed,
                            )));
                        }
                    }
                }
            }
            _ => {}
        }
    }
    actions
}

/// Extract a suggested name from ``Did you mean \`foo\`?`` style strings.
fn extract_suggestion_name(suggestion: &str) -> String {
    if let Some(start) = suggestion.find('`') {
        if let Some(end) = suggestion[start + 1..].find('`') {
            return suggestion[start + 1..start + 1 + end].to_string();
        }
    }
    suggestion.to_string()
}

/// Create a quickfix `CodeAction` that replaces text at `edit_range` with `new_text`.
fn make_replace_action(
    uri: &Url,
    diagnostic: &Diagnostic,
    title: &str,
    edit_range: Range,
    new_text: String,
) -> CodeAction {
    let mut changes = HashMap::new();
    changes.insert(
        uri.clone(),
        vec![TextEdit {
            range: edit_range,
            new_text,
        }],
    );
    CodeAction {
        title: title.to_string(),
        kind: Some(CodeActionKind::QUICKFIX),
        diagnostics: Some(vec![diagnostic.clone()]),
        edit: Some(WorkspaceEdit {
            changes: Some(changes),
            ..Default::default()
        }),
        ..Default::default()
    }
}

/// Find the `let` keyword range for a binding near the diagnostic range.
///
/// When `var_name` is provided, verifies the found `let` is followed by that
/// variable name to avoid matching an unrelated declaration.
#[expect(
    clippy::cast_possible_truncation,
    reason = "line/column values will not exceed u32"
)]
fn find_let_keyword(source: &str, diag_range: Range, var_name: Option<&str>) -> Option<Range> {
    let line_offsets = compute_line_offsets(source);
    let diag_start = position_to_offset(source, diag_range.start);
    let search_start = diag_start.saturating_sub(200);
    let search_region = &source[search_start..diag_start];
    // Search backwards for all `let ` occurrences, closest first
    for (rel_pos, _) in search_region.rmatch_indices("let ") {
        let abs_pos = search_start + rel_pos;
        // Verify this `let` declares the expected variable
        if let Some(name) = var_name {
            let after_let = &source[abs_pos + 4..];
            let trimmed = after_let.trim_start();
            if !trimmed.starts_with(name) {
                continue;
            }
        }
        let (sl, sc) = offset_to_line_col(source, &line_offsets, abs_pos);
        let (el, ec) = offset_to_line_col(source, &line_offsets, abs_pos + 3);
        return Some(Range {
            start: Position::new(sl as u32, sc as u32),
            end: Position::new(el as u32, ec as u32),
        });
    }
    None
}

// ── Folding Ranges ───────────────────────────────────────────────────

/// Build folding ranges from the AST.
fn build_folding_ranges(source: &str, parse_result: &ParseResult) -> Vec<FoldingRange> {
    let line_offsets = compute_line_offsets(source);
    let mut ranges = Vec::new();
    collect_import_groups(
        source,
        &line_offsets,
        &parse_result.program.items,
        &mut ranges,
    );
    for (item, span) in &parse_result.program.items {
        collect_item_folding(source, &line_offsets, item, span, &mut ranges);
    }
    ranges
}

#[expect(
    clippy::cast_possible_truncation,
    reason = "line values will not exceed u32"
)]
fn collect_import_groups(
    source: &str,
    line_offsets: &[usize],
    items: &[(Item, Span)],
    ranges: &mut Vec<FoldingRange>,
) {
    let mut import_start: Option<usize> = None;
    let mut import_end: Option<usize> = None;
    let mut count = 0u32;
    for (item, span) in items {
        if matches!(item, Item::Import(_)) {
            let (sl, _) = offset_to_line_col(source, line_offsets, span.start);
            let (el, _) = offset_to_line_col(source, line_offsets, span.end);
            if import_start.is_none() {
                import_start = Some(sl);
            }
            import_end = Some(el);
            count += 1;
        } else {
            if count >= 2 {
                if let (Some(s), Some(e)) = (import_start, import_end) {
                    ranges.push(FoldingRange {
                        start_line: s as u32,
                        start_character: None,
                        end_line: e as u32,
                        end_character: None,
                        kind: Some(FoldingRangeKind::Imports),
                        collapsed_text: None,
                    });
                }
            }
            import_start = None;
            import_end = None;
            count = 0;
        }
    }
    if count >= 2 {
        if let (Some(s), Some(e)) = (import_start, import_end) {
            ranges.push(FoldingRange {
                start_line: s as u32,
                start_character: None,
                end_line: e as u32,
                end_character: None,
                kind: Some(FoldingRangeKind::Imports),
                collapsed_text: None,
            });
        }
    }
}

#[expect(
    clippy::cast_possible_truncation,
    reason = "line values will not exceed u32"
)]
fn add_region(source: &str, line_offsets: &[usize], span: &Span, ranges: &mut Vec<FoldingRange>) {
    let (sl, _) = offset_to_line_col(source, line_offsets, span.start);
    let (el, _) = offset_to_line_col(source, line_offsets, span.end.saturating_sub(1));
    if el > sl {
        ranges.push(FoldingRange {
            start_line: sl as u32,
            start_character: None,
            end_line: el as u32,
            end_character: None,
            kind: Some(FoldingRangeKind::Region),
            collapsed_text: None,
        });
    }
}

fn collect_item_folding(
    source: &str,
    lo: &[usize],
    item: &Item,
    span: &Span,
    r: &mut Vec<FoldingRange>,
) {
    match item {
        Item::Function(f) => {
            add_region(source, lo, span, r);
            collect_block_folding(source, lo, &f.body, r);
        }
        Item::Actor(a) => {
            add_region(source, lo, span, r);
            collect_actor_folding(source, lo, a, r);
        }
        Item::TypeDecl(_)
        | Item::Trait(_)
        | Item::Impl(_)
        | Item::Wire(_)
        | Item::ExternBlock(_)
        | Item::Supervisor(_) => {
            add_region(source, lo, span, r);
        }
        Item::Import(_) | Item::Const(_) | Item::TypeAlias(_) => {}
    }
}

fn collect_actor_folding(source: &str, lo: &[usize], actor: &ActorDecl, r: &mut Vec<FoldingRange>) {
    if let Some(init) = &actor.init {
        collect_block_folding(source, lo, &init.body, r);
    }
    for recv in &actor.receive_fns {
        add_region(source, lo, &recv.span, r);
        collect_block_folding(source, lo, &recv.body, r);
    }
    for method in &actor.methods {
        collect_block_folding(source, lo, &method.body, r);
    }
}

fn collect_block_folding(source: &str, lo: &[usize], block: &Block, r: &mut Vec<FoldingRange>) {
    for (stmt, span) in &block.stmts {
        collect_stmt_folding(source, lo, stmt, span, r);
    }
    if let Some(expr) = &block.trailing_expr {
        collect_expr_folding(source, lo, &expr.0, &expr.1, r);
    }
}

fn collect_stmt_folding(
    source: &str,
    lo: &[usize],
    stmt: &Stmt,
    span: &Span,
    r: &mut Vec<FoldingRange>,
) {
    match stmt {
        Stmt::If {
            then_block,
            else_block,
            ..
        } => {
            add_region(source, lo, span, r);
            collect_block_folding(source, lo, then_block, r);
            if let Some(eb) = else_block {
                if let Some(block) = &eb.block {
                    collect_block_folding(source, lo, block, r);
                }
                if let Some(if_stmt) = &eb.if_stmt {
                    collect_stmt_folding(source, lo, &if_stmt.0, &if_stmt.1, r);
                }
            }
        }
        Stmt::IfLet {
            body, else_body, ..
        } => {
            add_region(source, lo, span, r);
            collect_block_folding(source, lo, body, r);
            if let Some(block) = else_body {
                collect_block_folding(source, lo, block, r);
            }
        }
        Stmt::Match { arms, .. } => {
            add_region(source, lo, span, r);
            for arm in arms {
                collect_expr_folding(source, lo, &arm.body.0, &arm.body.1, r);
            }
        }
        Stmt::For { body, .. } | Stmt::While { body, .. } | Stmt::Loop { body, .. } => {
            add_region(source, lo, span, r);
            collect_block_folding(source, lo, body, r);
        }
        Stmt::Expression(expr) => {
            collect_expr_folding(source, lo, &expr.0, &expr.1, r);
        }
        Stmt::Let { value, .. } | Stmt::Var { value, .. } => {
            if let Some(v) = value {
                collect_expr_folding(source, lo, &v.0, &v.1, r);
            }
        }
        _ => {}
    }
}

fn collect_expr_folding(
    source: &str,
    lo: &[usize],
    expr: &Expr,
    span: &Span,
    r: &mut Vec<FoldingRange>,
) {
    match expr {
        Expr::Block(block) => {
            add_region(source, lo, span, r);
            collect_block_folding(source, lo, block, r);
        }
        Expr::If {
            then_block,
            else_block,
            ..
        } => {
            add_region(source, lo, span, r);
            collect_expr_folding(source, lo, &then_block.0, &then_block.1, r);
            if let Some(eb) = else_block {
                collect_expr_folding(source, lo, &eb.0, &eb.1, r);
            }
        }
        Expr::IfLet {
            body, else_body, ..
        } => {
            add_region(source, lo, span, r);
            collect_block_folding(source, lo, body, r);
            if let Some(block) = else_body {
                collect_block_folding(source, lo, block, r);
            }
        }
        Expr::Match { arms, .. } => {
            add_region(source, lo, span, r);
            for arm in arms {
                collect_expr_folding(source, lo, &arm.body.0, &arm.body.1, r);
            }
        }
        Expr::Lambda { body, .. } | Expr::SpawnLambdaActor { body, .. } => {
            add_region(source, lo, span, r);
            collect_expr_folding(source, lo, &body.0, &body.1, r);
        }
        Expr::Scope { body, .. }
        | Expr::Unsafe(body)
        | Expr::ScopeLaunch(body)
        | Expr::ScopeSpawn(body) => {
            add_region(source, lo, span, r);
            collect_block_folding(source, lo, body, r);
        }
        Expr::Cast { expr: inner, .. } => {
            collect_expr_folding(source, lo, &inner.0, &inner.1, r);
        }
        _ => {}
    }
}

// ── Completion ───────────────────────────────────────────────────────

/// Hew language keywords for completion.
/// All language keywords, sourced from the lexer (single source of truth).
const KEYWORDS: &[&str] = hew_lexer::ALL_KEYWORDS;

fn build_completions_at(doc: &DocumentState, offset: usize) -> Vec<CompletionItem> {
    if let Some(items) = try_dot_completions(doc, offset) {
        return items;
    }

    if let Some(items) = try_spawn_completions(doc, offset) {
        return items;
    }

    let mut items: Vec<CompletionItem> = KEYWORDS
        .iter()
        .map(|kw| CompletionItem {
            label: (*kw).to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            ..Default::default()
        })
        .collect();

    // Snippet completions for control-flow keywords.
    items.extend(keyword_snippets());

    // Add identifiers from AST items.
    for (item, _span) in &doc.parse_result.program.items {
        if let Some((name, kind)) = item_name_and_kind(item) {
            items.push(CompletionItem {
                label: name,
                kind: Some(kind),
                ..Default::default()
            });
        }
    }

    // Add type definitions and function signatures from the type checker.
    if let Some(tc) = &doc.type_output {
        for name in tc.type_defs.keys() {
            items.push(CompletionItem {
                label: name.clone(),
                kind: Some(CompletionItemKind::CLASS),
                ..Default::default()
            });
        }
        for (name, sig) in &tc.fn_sigs {
            items.push(fn_sig_completion(name, sig));
        }
    }

    // Collect local variables visible at cursor offset.
    items.extend(collect_locals_at(&doc.parse_result, offset));

    // Deduplicate by label, preferring items with detail (richer completions).
    let mut seen = HashSet::new();
    items.retain(|item| seen.insert(item.label.clone()));

    items
}

/// If the cursor is right after a `.`, find the receiver type and offer its methods/fields.
fn try_dot_completions(doc: &DocumentState, offset: usize) -> Option<Vec<CompletionItem>> {
    let bytes = doc.source.as_bytes();
    // Walk backwards past any whitespace to find the dot.
    let mut dot_pos = offset;
    while dot_pos > 0 && bytes[dot_pos - 1].is_ascii_whitespace() {
        dot_pos -= 1;
    }
    if dot_pos == 0 || bytes[dot_pos - 1] != b'.' {
        return None;
    }
    let receiver_end = dot_pos - 1;
    let tc = doc.type_output.as_ref()?;
    let receiver_ty = find_receiver_type(tc, receiver_end)?;
    let type_name = type_name_of(receiver_ty)?;
    let type_def = tc.type_defs.get(&type_name)?;

    let mut items = Vec::new();
    for (field_name, field_ty) in &type_def.fields {
        items.push(CompletionItem {
            label: field_name.clone(),
            kind: Some(CompletionItemKind::FIELD),
            detail: Some(field_ty.to_string()),
            ..Default::default()
        });
    }
    for (method_name, sig) in &type_def.methods {
        items.push(fn_sig_completion(method_name, sig));
    }
    // For actors, also check fn_sigs for "TypeName::method" patterns.
    let prefix = format!("{type_name}::");
    for (name, sig) in &tc.fn_sigs {
        if let Some(method) = name.strip_prefix(&prefix) {
            items.push(fn_sig_completion(method, sig));
        }
    }
    let mut seen = HashSet::new();
    items.retain(|item| seen.insert(item.label.clone()));
    Some(items)
}

/// Find the type of the receiver expression ending at `end_offset` (byte offset of the last
/// character of the expression, just before the dot).
fn find_receiver_type(tc: &TypeCheckOutput, end_offset: usize) -> Option<&Ty> {
    let mut best: Option<(&SpanKey, &Ty)> = None;
    for (span_key, ty) in &tc.expr_types {
        // The span's end should be at or very close to the position just before the dot.
        if span_key.end <= end_offset && span_key.end + 1 >= end_offset {
            match best {
                Some((prev, _)) if span_key.end > prev.end => {
                    best = Some((span_key, ty));
                }
                Some((prev, _))
                    if span_key.end == prev.end
                        && (span_key.end - span_key.start) < (prev.end - prev.start) =>
                {
                    best = Some((span_key, ty));
                }
                None => {
                    best = Some((span_key, ty));
                }
                _ => {}
            }
        }
    }
    best.map(|(_, ty)| ty)
}

/// Extract the type name from a `Ty`, following `ActorRef` wrappers.
fn type_name_of(ty: &Ty) -> Option<String> {
    match ty {
        Ty::Named { name, .. } => Some(name.clone()),
        Ty::ActorRef(inner) => type_name_of(inner),
        _ => None,
    }
}

/// If the cursor is right after `spawn `, offer only actor and supervisor names.
fn try_spawn_completions(doc: &DocumentState, offset: usize) -> Option<Vec<CompletionItem>> {
    let before = &doc.source[..offset];
    let trimmed = before.trim_end();
    if !trimmed.ends_with("spawn") {
        return None;
    }

    let mut items = Vec::new();
    for (item, _span) in &doc.parse_result.program.items {
        match item {
            Item::Actor(a) => {
                items.push(CompletionItem {
                    label: a.name.clone(),
                    kind: Some(CompletionItemKind::CLASS),
                    detail: Some("actor".to_string()),
                    ..Default::default()
                });
            }
            Item::Supervisor(s) => {
                items.push(CompletionItem {
                    label: s.name.clone(),
                    kind: Some(CompletionItemKind::CLASS),
                    detail: Some("supervisor".to_string()),
                    ..Default::default()
                });
            }
            _ => {}
        }
    }

    Some(items)
}

/// Collect local variable names from function/actor bodies that are in scope at `offset`.
fn collect_locals_at(parse_result: &ParseResult, offset: usize) -> Vec<CompletionItem> {
    let mut locals = Vec::new();

    for (item, span) in &parse_result.program.items {
        // Skip items that don't contain the cursor. Treat empty/unset spans
        // (the parser doesn't always record item spans) as containing everything.
        let span_covers = span_contains_offset(span, offset);
        if !span_covers {
            continue;
        }
        match item {
            Item::Function(f) => {
                for p in &f.params {
                    locals.push(local_completion(&p.name));
                }
                collect_locals_from_block(&f.body, offset, &mut locals);
            }
            Item::Actor(a) => {
                for field in &a.fields {
                    locals.push(local_completion(&field.name));
                }
                for recv in &a.receive_fns {
                    for p in &recv.params {
                        locals.push(local_completion(&p.name));
                    }
                    collect_locals_from_block(&recv.body, offset, &mut locals);
                }
                for method in &a.methods {
                    for p in &method.params {
                        locals.push(local_completion(&p.name));
                    }
                    collect_locals_from_block(&method.body, offset, &mut locals);
                }
            }
            Item::TypeDecl(td) => {
                for body_item in &td.body {
                    if let TypeBodyItem::Method(method) = body_item {
                        for p in &method.params {
                            locals.push(local_completion(&p.name));
                        }
                        collect_locals_from_block(&method.body, offset, &mut locals);
                    }
                }
            }
            Item::Impl(i) => {
                for method in &i.methods {
                    for p in &method.params {
                        locals.push(local_completion(&p.name));
                    }
                    collect_locals_from_block(&method.body, offset, &mut locals);
                }
            }
            _ => {}
        }
    }

    locals
}

fn span_contains_offset(span: &Span, offset: usize) -> bool {
    span.is_empty() || (span.start <= offset && offset <= span.end)
}

fn collect_locals_from_block(block: &Block, offset: usize, locals: &mut Vec<CompletionItem>) {
    for (stmt, span) in &block.stmts {
        if span.start > offset {
            break;
        }
        collect_locals_from_stmt(stmt, span, offset, locals);
    }
}

fn collect_locals_from_stmt(
    stmt: &Stmt,
    stmt_span: &Span,
    offset: usize,
    locals: &mut Vec<CompletionItem>,
) {
    let in_stmt_scope = span_contains_offset(stmt_span, offset);
    match stmt {
        Stmt::Let { pattern, .. } => {
            collect_pattern_names(&pattern.0, locals);
        }
        Stmt::Var { name, .. } => {
            locals.push(local_completion(name));
        }
        Stmt::For { pattern, body, .. } => {
            if in_stmt_scope {
                collect_pattern_names(&pattern.0, locals);
                collect_locals_from_block(body, offset, locals);
            }
        }
        Stmt::Loop { body, .. } | Stmt::While { body, .. } => {
            if in_stmt_scope {
                collect_locals_from_block(body, offset, locals);
            }
        }
        Stmt::If {
            then_block,
            else_block,
            ..
        } => {
            if in_stmt_scope {
                collect_locals_from_block(then_block, offset, locals);
                if let Some(eb) = else_block {
                    if let Some(if_stmt) = &eb.if_stmt {
                        collect_locals_from_stmt(&if_stmt.0, &if_stmt.1, offset, locals);
                    }
                    if let Some(block) = &eb.block {
                        collect_locals_from_block(block, offset, locals);
                    }
                }
            }
        }
        Stmt::IfLet {
            pattern,
            body,
            else_body,
            ..
        } => {
            if in_stmt_scope {
                collect_pattern_names(&pattern.0, locals);
                collect_locals_from_block(body, offset, locals);
                if let Some(block) = else_body {
                    collect_locals_from_block(block, offset, locals);
                }
            }
        }
        Stmt::Match { arms, .. } => {
            if in_stmt_scope {
                for arm in arms {
                    if span_contains_offset(&arm.body.1, offset) {
                        collect_pattern_names(&arm.pattern.0, locals);
                        collect_locals_from_expr(&arm.body.0, offset, locals);
                    }
                }
            }
        }
        Stmt::Defer(expr) => {
            collect_locals_from_expr(&expr.0, offset, locals);
        }
        _ => {}
    }
}

fn collect_locals_from_expr(expr: &Expr, offset: usize, locals: &mut Vec<CompletionItem>) {
    match expr {
        Expr::Block(block)
        | Expr::Unsafe(block)
        | Expr::ScopeLaunch(block)
        | Expr::ScopeSpawn(block) => {
            collect_locals_from_block(block, offset, locals);
        }
        Expr::Scope { binding, body } => {
            if let Some(name) = binding {
                locals.push(local_completion(name));
            }
            collect_locals_from_block(body, offset, locals);
        }
        Expr::If {
            then_block,
            else_block,
            ..
        } => {
            if span_contains_offset(&then_block.1, offset) {
                collect_locals_from_expr(&then_block.0, offset, locals);
            }
            if let Some(else_expr) = else_block {
                if span_contains_offset(&else_expr.1, offset) {
                    collect_locals_from_expr(&else_expr.0, offset, locals);
                }
            }
        }
        Expr::IfLet {
            body, else_body, ..
        } => {
            collect_locals_from_block(body, offset, locals);
            if let Some(block) = else_body {
                collect_locals_from_block(block, offset, locals);
            }
        }
        Expr::Match { arms, .. } => {
            for arm in arms {
                if span_contains_offset(&arm.body.1, offset) {
                    collect_pattern_names(&arm.pattern.0, locals);
                    collect_locals_from_expr(&arm.body.0, offset, locals);
                }
            }
        }
        Expr::PostfixTry(inner) => {
            collect_locals_from_expr(&inner.0, offset, locals);
        }
        Expr::Cast { expr: inner, .. } => {
            collect_locals_from_expr(&inner.0, offset, locals);
        }
        Expr::Select { arms, timeout } => {
            for arm in arms {
                if span_contains_offset(&arm.body.1, offset) {
                    collect_pattern_names(&arm.binding.0, locals);
                    collect_locals_from_expr(&arm.body.0, offset, locals);
                }
            }
            if let Some(tc) = timeout {
                if span_contains_offset(&tc.body.1, offset) {
                    collect_locals_from_expr(&tc.body.0, offset, locals);
                }
            }
        }
        _ => {}
    }
}

fn collect_pattern_names(pattern: &Pattern, locals: &mut Vec<CompletionItem>) {
    match pattern {
        Pattern::Identifier(name) => locals.push(local_completion(name)),
        Pattern::Constructor { patterns, .. } | Pattern::Tuple(patterns) => {
            for (p, _) in patterns {
                collect_pattern_names(p, locals);
            }
        }
        Pattern::Struct { fields, .. } => {
            for field in fields {
                if let Some((pattern, _)) = &field.pattern {
                    collect_pattern_names(pattern, locals);
                } else {
                    locals.push(local_completion(&field.name));
                }
            }
        }
        Pattern::Or(left, right) => {
            collect_pattern_names(&left.0, locals);
            collect_pattern_names(&right.0, locals);
        }
        Pattern::Literal(_) | Pattern::Wildcard => {}
    }
}

fn local_completion(name: &str) -> CompletionItem {
    CompletionItem {
        label: name.to_string(),
        kind: Some(CompletionItemKind::VARIABLE),
        ..Default::default()
    }
}

/// Build a completion item from a function signature.
fn fn_sig_completion(name: &str, sig: &FnSig) -> CompletionItem {
    let param_list: Vec<String> = sig.params.iter().map(ToString::to_string).collect();
    let detail = if sig.return_type == Ty::Unit {
        format!("fn {}({})", name, param_list.join(", "))
    } else {
        format!(
            "fn {}({}) -> {}",
            name,
            param_list.join(", "),
            sig.return_type
        )
    };
    CompletionItem {
        label: name.to_string(),
        kind: Some(CompletionItemKind::FUNCTION),
        detail: Some(detail),
        ..Default::default()
    }
}

/// Snippet completions for common language constructs.
fn keyword_snippets() -> Vec<CompletionItem> {
    let snippets = [
        (
            "fn",
            "fn ${1:name}(${2:params}) {\n\t$0\n}",
            "fn name(params) { ... }",
        ),
        ("if", "if ${1:condition} {\n\t$0\n}", "if condition { ... }"),
        (
            "if else",
            "if ${1:condition} {\n\t$2\n} else {\n\t$0\n}",
            "if condition { ... } else { ... }",
        ),
        (
            "for",
            "for ${1:item} in ${2:collection} {\n\t$0\n}",
            "for item in collection { ... }",
        ),
        (
            "while",
            "while ${1:condition} {\n\t$0\n}",
            "while condition { ... }",
        ),
        (
            "match",
            "match ${1:value} {\n\t${2:pattern} => $0,\n}",
            "match value { pattern => ... }",
        ),
        ("actor", "actor ${1:Name} {\n\t$0\n}", "actor Name { ... }"),
        (
            "type",
            "type ${1:Name} {\n\t$0\n}",
            "type Name { ... }",
        ),
        ("enum", "enum ${1:Name} {\n\t$0\n}", "enum Name { ... }"),
        ("impl", "impl ${1:Type} {\n\t$0\n}", "impl Type { ... }"),
        (
            "spawn",
            "let ${1:handle} = spawn ${2:Actor}(${3:field}: ${4:value});",
            "let handle = spawn Actor(field: value);",
        ),
        (
            "receive",
            "receive ${1:name}(${2:params}) {\n\t$0\n}",
            "receive name(params) { ... }",
        ),
        ("loop", "loop {\n\t$0\n}", "loop { ... }"),
        (
            "async fn",
            "async fn ${1:name}(${2:params}) {\n\t$0\n}",
            "async fn name(params) { ... }",
        ),
        (
            "supervisor",
            "supervisor ${1:Name} {\n\t$0\n}",
            "supervisor Name { ... }",
        ),
        ("trait", "trait ${1:Name} {\n\t$0\n}", "trait Name { ... }"),
        (
            "spawn lambda",
            "let ${1:handle} = spawn (${2:param}: ${3:Type}) => {\n\t$0\n};",
            "let handle = spawn (param: Type) => { ... };",
        ),
        (
            "select",
            "select {\n\t${1:binding} <- ${2:source} => ${3:expr},\n\tafter ${4:duration} => ${0:timeout_expr},\n}",
            "select { pattern <- source => expr, after duration => expr }",
        ),
        (
            "select from",
            "select {\n\t${1:binding} from ${2:source} => ${3:expr},\n\tafter ${4:duration} => ${0:timeout_expr},\n}",
            "select { pattern from source => expr, after duration => expr }",
        ),
        (
            "timeout",
            "${1:expr} | after ${2:duration}",
            "expr | after duration",
        ),
        (
            "defer",
            "defer ${0:expr};",
            "defer expr;",
        ),
        (
            "scope",
            "scope |${1:cancel}| {\n\t$0\n}",
            "scope |cancel| { ... }",
        ),
    ];
    snippets
        .into_iter()
        .map(|(label, snippet, detail)| CompletionItem {
            label: format!("{label}..."),
            kind: Some(CompletionItemKind::SNIPPET),
            insert_text: Some(snippet.to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            detail: Some(detail.to_string()),
            sort_text: Some(format!("0_{label}")),
            ..Default::default()
        })
        .collect()
}

fn item_name_and_kind(item: &Item) -> Option<(String, CompletionItemKind)> {
    match item {
        Item::Function(f) => Some((f.name.clone(), CompletionItemKind::FUNCTION)),
        Item::Actor(a) => Some((a.name.clone(), CompletionItemKind::CLASS)),
        Item::Supervisor(s) => Some((s.name.clone(), CompletionItemKind::CLASS)),
        Item::Trait(t) => Some((t.name.clone(), CompletionItemKind::INTERFACE)),
        Item::Const(c) => Some((c.name.clone(), CompletionItemKind::CONSTANT)),
        Item::TypeDecl(td) => {
            let kind = match td.kind {
                TypeDeclKind::Struct => CompletionItemKind::STRUCT,
                TypeDeclKind::Enum => CompletionItemKind::ENUM,
            };
            Some((td.name.clone(), kind))
        }
        Item::Wire(w) => Some((w.name.clone(), CompletionItemKind::STRUCT)),
        Item::TypeAlias(ta) => Some((ta.name.clone(), CompletionItemKind::TYPE_PARAMETER)),
        Item::Import(_) | Item::Impl(_) | Item::ExternBlock(_) => None,
    }
}

// ── Document symbols ─────────────────────────────────────────────────

#[expect(
    clippy::too_many_lines,
    reason = "match arms over all Item variants with children is clearest as one function"
)]
fn build_document_symbols(source: &str, parse_result: &ParseResult) -> Vec<DocumentSymbol> {
    let mut symbols = Vec::new();

    for (item, span) in &parse_result.program.items {
        let range = span_to_range(source, span);
        let sym = match item {
            Item::Function(f) => make_symbol(&f.name, SymbolKind::FUNCTION, range),
            Item::Actor(a) => {
                let mut sym = make_symbol(&a.name, SymbolKind::CLASS, range);
                let mut children = Vec::new();
                if a.init.is_some() {
                    children.push(make_symbol("init", SymbolKind::CONSTRUCTOR, range));
                }
                for recv in &a.receive_fns {
                    let recv_range = if recv.span.is_empty() {
                        range
                    } else {
                        span_to_range(source, &recv.span)
                    };
                    children.push(make_symbol(&recv.name, SymbolKind::METHOD, recv_range));
                }
                for method in &a.methods {
                    children.push(make_symbol(&method.name, SymbolKind::METHOD, range));
                }
                sym.children = non_empty(children);
                sym
            }
            Item::Supervisor(s) => make_symbol(&s.name, SymbolKind::CLASS, range),
            Item::Trait(t) => {
                let mut sym = make_symbol(&t.name, SymbolKind::INTERFACE, range);
                let children: Vec<DocumentSymbol> = t
                    .items
                    .iter()
                    .map(|item| match item {
                        TraitItem::Method(m) => make_symbol(&m.name, SymbolKind::METHOD, range),
                        TraitItem::AssociatedType { name, .. } => {
                            make_symbol(name, SymbolKind::TYPE_PARAMETER, range)
                        }
                    })
                    .collect();
                sym.children = non_empty(children);
                sym
            }
            Item::Impl(i) => {
                let name = match &i.trait_bound {
                    Some(tb) => format!("impl {} for ...", tb.name),
                    None => "impl".to_string(),
                };
                let mut sym = make_symbol(&name, SymbolKind::NAMESPACE, range);
                let children: Vec<DocumentSymbol> = i
                    .methods
                    .iter()
                    .map(|m| make_symbol(&m.name, SymbolKind::METHOD, range))
                    .collect();
                sym.children = non_empty(children);
                sym
            }
            Item::Const(c) => make_symbol(&c.name, SymbolKind::CONSTANT, range),
            Item::TypeDecl(td) => {
                let kind = match td.kind {
                    TypeDeclKind::Struct => SymbolKind::STRUCT,
                    TypeDeclKind::Enum => SymbolKind::ENUM,
                };
                let mut sym = make_symbol(&td.name, kind, range);
                let children: Vec<DocumentSymbol> = td
                    .body
                    .iter()
                    .filter_map(|item| match item {
                        TypeBodyItem::Variant(v) => {
                            Some(make_symbol(&v.name, SymbolKind::ENUM_MEMBER, range))
                        }
                        TypeBodyItem::Method(m) => {
                            Some(make_symbol(&m.name, SymbolKind::METHOD, range))
                        }
                        TypeBodyItem::Field { .. } => None,
                    })
                    .collect();
                sym.children = non_empty(children);
                sym
            }
            Item::Wire(w) => make_symbol(&w.name, SymbolKind::STRUCT, range),
            Item::TypeAlias(ta) => make_symbol(&ta.name, SymbolKind::TYPE_PARAMETER, range),
            Item::ExternBlock(eb) => {
                let mut sym =
                    make_symbol(&format!("extern \"{}\"", eb.abi), SymbolKind::MODULE, range);
                let children: Vec<DocumentSymbol> = eb
                    .functions
                    .iter()
                    .map(|f| make_symbol(&f.name, SymbolKind::FUNCTION, range))
                    .collect();
                sym.children = non_empty(children);
                sym
            }
            Item::Import(i) => {
                let name = i.path.join("::");
                make_symbol(&name, SymbolKind::MODULE, range)
            }
        };
        symbols.push(sym);
    }
    symbols
}

#[expect(
    deprecated,
    reason = "DocumentSymbol::deprecated is required by the LSP type"
)]
fn make_symbol(name: &str, kind: SymbolKind, range: Range) -> DocumentSymbol {
    DocumentSymbol {
        name: name.to_string(),
        detail: None,
        kind,
        tags: None,
        deprecated: None,
        range,
        selection_range: range,
        children: None,
    }
}

// ── Semantic tokens ──────────────────────────────────────────────────

#[expect(
    clippy::cast_possible_truncation,
    reason = "TOKEN_TYPES has 8 entries, well within u32"
)]
fn token_type_index(tt: &SemanticTokenType) -> u32 {
    TOKEN_TYPES.iter().position(|t| t == tt).unwrap_or(0) as u32
}

fn classify_token(token: &Token<'_>) -> Option<SemanticTokenType> {
    if token.is_keyword() {
        return Some(SemanticTokenType::KEYWORD);
    }
    if token.is_operator() {
        return Some(SemanticTokenType::OPERATOR);
    }
    match token {
        // Literals
        Token::Integer(_) | Token::Float(_) | Token::Duration(_) => Some(SemanticTokenType::NUMBER),
        Token::StringLit(_)
        | Token::RawString(_)
        | Token::InterpolatedString(_)
        | Token::RegexLiteral(_) => Some(SemanticTokenType::STRING),

        // Identifiers
        Token::Identifier(_) | Token::Label(_) => Some(SemanticTokenType::VARIABLE),

        // Comments (doc comments are emitted as tokens, unlike regular comments)
        Token::DocComment(_) | Token::InnerDocComment(_) => Some(SemanticTokenType::COMMENT),

        // Skip delimiters, punctuation, errors, block comments
        _ => None,
    }
}

/// Keywords that introduce a named declaration — the identifier immediately
/// following one of these tokens is a declaration site.
fn is_decl_keyword(token: &Token<'_>) -> bool {
    token.is_decl_keyword()
}

fn is_function_decl_context(prev: Option<&Token<'_>>) -> bool {
    matches!(prev, Some(Token::Fn | Token::Receive))
}

fn is_type_decl_context(prev: Option<&Token<'_>>) -> bool {
    prev.is_some_and(Token::is_type_decl_keyword)
}

#[expect(
    clippy::cast_possible_truncation,
    reason = "token delta values in source files will not exceed u32"
)]
fn build_semantic_tokens(source: &str, _parse_result: &ParseResult) -> Vec<SemanticToken> {
    let lexer_tokens = hew_lexer::lex(source);
    let line_offsets = compute_line_offsets(source);
    let mut result = Vec::new();
    let mut prev_line: u32 = 0;
    let mut prev_start: u32 = 0;

    for (i, (token, span)) in lexer_tokens.iter().enumerate() {
        let Some(mut sem_type) = classify_token(token) else {
            continue;
        };
        if matches!(token, Token::Identifier(_)) {
            let prev = i
                .checked_sub(1)
                .and_then(|idx| lexer_tokens.get(idx).map(|(prev, _)| prev));
            if is_function_decl_context(prev) {
                sem_type = SemanticTokenType::FUNCTION;
            } else if is_type_decl_context(prev) {
                sem_type = SemanticTokenType::TYPE;
            }
        }

        let (line, col) = offset_to_line_col(source, &line_offsets, span.start);
        let line = line as u32;
        let col = col as u32;
        // Length in UTF-16 code units for the token text
        let length: u32 = source[span.start..span.end]
            .chars()
            .map(|c| c.len_utf16() as u32)
            .sum();

        let delta_line = line - prev_line;
        let delta_start = if delta_line == 0 {
            col - prev_start
        } else {
            col
        };

        // Compute modifier bitset from lexer context.
        let mut modifiers: u32 = 0;

        // Mark identifiers after declaration keywords as DECLARATION.
        if matches!(token, Token::Identifier(_)) {
            if i > 0 && is_decl_keyword(&lexer_tokens[i - 1].0) {
                modifiers |= 1 << 0; // DECLARATION
            }
            // Identifiers after `const` are also READONLY.
            if i > 0 && matches!(lexer_tokens[i - 1].0, Token::Const) {
                modifiers |= 1 << 1; // READONLY
            }
        }

        // Mark `async` keyword with ASYNC modifier.
        if matches!(token, Token::Async) {
            modifiers |= 1 << 2; // ASYNC
        }

        result.push(SemanticToken {
            delta_line,
            delta_start,
            length,
            token_type: token_type_index(&sem_type),
            token_modifiers_bitset: modifiers,
        });

        prev_line = line;
        prev_start = col;
    }

    result
}

// ── Helpers ──────────────────────────────────────────────────────────

/// Return `None` for an empty vec, `Some(v)` otherwise.
fn non_empty<T>(v: Vec<T>) -> Option<Vec<T>> {
    if v.is_empty() {
        None
    } else {
        Some(v)
    }
}

/// Compute byte offsets of each line start.
fn compute_line_offsets(source: &str) -> Vec<usize> {
    let mut offsets = vec![0];
    for (i, ch) in source.bytes().enumerate() {
        if ch == b'\n' {
            offsets.push(i + 1);
        }
    }
    offsets
}

/// Convert byte offset to (line, character) — both 0-based, character in UTF-16 code units.
fn offset_to_line_col(source: &str, line_offsets: &[usize], offset: usize) -> (usize, usize) {
    let line = line_offsets
        .partition_point(|&o| o <= offset)
        .saturating_sub(1);
    let line_start = line_offsets[line];
    let byte_col = offset - line_start;
    // Count UTF-16 code units from line start to offset
    let col_utf16 = source[line_start..line_start + byte_col]
        .chars()
        .map(char::len_utf16)
        .sum();
    (line, col_utf16)
}

/// Convert a parser `Span` (byte-offset `Range<usize>`) to an LSP `Range`.
#[expect(
    clippy::cast_possible_truncation,
    reason = "line/column values in source files will not exceed u32"
)]
fn span_to_range(source: &str, span: &Span) -> Range {
    let line_offsets = compute_line_offsets(source);
    let (start_line, start_col) = offset_to_line_col(source, &line_offsets, span.start);
    let (end_line, end_col) = offset_to_line_col(source, &line_offsets, span.end);
    Range {
        start: Position::new(start_line as u32, start_col as u32),
        end: Position::new(end_line as u32, end_col as u32),
    }
}

/// Convert an LSP `Position` (UTF-16 character offset) to a byte offset in source.
fn position_to_offset(source: &str, position: Position) -> usize {
    let line_offsets = compute_line_offsets(source);
    let line = position.line as usize;
    if line < line_offsets.len() {
        let line_start = line_offsets[line];
        let line_end = line_offsets.get(line + 1).copied().unwrap_or(source.len());
        let line_text = &source[line_start..line_end];
        let mut utf16_count = 0u32;
        let target = position.character;
        for (byte_idx, ch) in line_text.char_indices() {
            if utf16_count >= target {
                return line_start + byte_idx;
            }
            #[expect(
                clippy::cast_possible_truncation,
                reason = "len_utf16() returns 1 or 2, always fits in u32"
            )]
            {
                utf16_count += ch.len_utf16() as u32;
            }
        }
        // If we've consumed all chars, return end of line text
        line_end
    } else {
        source.len()
    }
}

/// Convert byte offset range to LSP `Range`.
#[expect(
    clippy::cast_possible_truncation,
    reason = "line/column values in source files will not exceed u32"
)]
fn offset_range_to_lsp(source: &str, start: usize, end: usize) -> Range {
    let line_offsets = compute_line_offsets(source);
    let (sl, sc) = offset_to_line_col(source, &line_offsets, start);
    let (el, ec) = offset_to_line_col(source, &line_offsets, end);
    Range {
        start: Position::new(sl as u32, sc as u32),
        end: Position::new(el as u32, ec as u32),
    }
}

// ── Go-to-definition ─────────────────────────────────────────────────

/// Search for a definition matching `word` in the AST, including nested items.
fn find_definition_in_ast(source: &str, parse_result: &ParseResult, word: &str) -> Option<Range> {
    for (item, span) in &parse_result.program.items {
        // Top-level name matching.
        let name = match item {
            Item::Function(f) => Some(&f.name),
            Item::Actor(a) => Some(&a.name),
            Item::Supervisor(s) => Some(&s.name),
            Item::Trait(t) => Some(&t.name),
            Item::Const(c) => Some(&c.name),
            Item::TypeDecl(td) => Some(&td.name),
            Item::Wire(w) => Some(&w.name),
            Item::TypeAlias(ta) => Some(&ta.name),
            _ => None,
        };
        if name.is_some_and(|n| n == word) {
            return Some(span_to_range(source, span));
        }

        // Search inside actors for receive methods and methods.
        if let Item::Actor(a) = item {
            for recv in &a.receive_fns {
                if recv.name == word {
                    return Some(span_to_range(source, span));
                }
            }
            for method in &a.methods {
                if method.name == word {
                    return Some(span_to_range(source, span));
                }
            }
        }

        // Search inside TypeDecl for variants and methods.
        if let Item::TypeDecl(td) = item {
            for body_item in &td.body {
                match body_item {
                    TypeBodyItem::Variant(v) if v.name == word => {
                        return Some(span_to_range(source, span));
                    }
                    TypeBodyItem::Method(m) if m.name == word => {
                        return Some(span_to_range(source, span));
                    }
                    _ => {}
                }
            }
        }

        // Search inside Trait for method and associated type definitions.
        if let Item::Trait(t) = item {
            for trait_item in &t.items {
                match trait_item {
                    TraitItem::Method(m) if m.name == word => {
                        return Some(span_to_range(source, span));
                    }
                    TraitItem::AssociatedType { name, .. } if name == word => {
                        return Some(span_to_range(source, span));
                    }
                    _ => {}
                }
            }
        }

        // Search inside Impl for methods.
        if let Item::Impl(i) = item {
            for method in &i.methods {
                if method.name == word {
                    return Some(span_to_range(source, span));
                }
            }
        }

        // Search inside extern blocks for function declarations.
        if let Item::ExternBlock(extern_block) = item {
            for function in &extern_block.functions {
                if function.name == word {
                    return Some(span_to_range(source, span));
                }
            }
        }
    }
    None
}

// ── Find all references ──────────────────────────────────────────────

/// Find the simple identifier name at the given byte offset (no `dot/::` qualifiers).
fn simple_word_at_offset(source: &str, offset: usize) -> Option<(String, Span)> {
    if offset >= source.len() {
        return None;
    }
    let bytes = source.as_bytes();
    let is_ident = |b: u8| b.is_ascii_alphanumeric() || b == b'_';

    let probe = if is_ident(bytes[offset]) {
        offset
    } else if offset > 0 && is_ident(bytes[offset - 1]) {
        offset - 1
    } else {
        return None;
    };

    let start = probe
        - source[..probe]
            .bytes()
            .rev()
            .take_while(|b| is_ident(*b))
            .count();
    let end = probe + source[probe..].bytes().take_while(|b| is_ident(*b)).count();
    let word = &source[start..end];
    if word.is_empty() {
        None
    } else {
        Some((word.to_string(), start..end))
    }
}

/// Return the LSP range of the simple identifier at `offset`.
fn word_range_at_offset(source: &str, offset: usize) -> Option<Range> {
    let (_word, span) = simple_word_at_offset(source, offset)?;
    Some(span_to_range(source, &span))
}

/// Check if a name matches a top-level item definition (function, actor, type, etc.).
fn is_top_level_name(parse_result: &ParseResult, name: &str) -> bool {
    parse_result.program.items.iter().any(|(item, _)| {
        // Check item-level names (functions, actors, types, etc.)
        let item_name = match item {
            Item::Function(f) => Some(f.name.as_str()),
            Item::Actor(a) => Some(a.name.as_str()),
            Item::Supervisor(s) => Some(s.name.as_str()),
            Item::Trait(t) => Some(t.name.as_str()),
            Item::Const(c) => Some(c.name.as_str()),
            Item::TypeDecl(td) => Some(td.name.as_str()),
            Item::Wire(w) => Some(w.name.as_str()),
            Item::TypeAlias(ta) => Some(ta.name.as_str()),
            Item::Import(_) | Item::ExternBlock(_) | Item::Impl(_) => None,
        };
        if item_name == Some(name) {
            return true;
        }
        // Also treat actor receive handler names and field names as global —
        // they can be referenced via message sends from any scope.
        if let Item::Actor(a) = item {
            if a.receive_fns.iter().any(|r| r.name == name) {
                return true;
            }
            if a.fields.iter().any(|f| f.name == name) {
                return true;
            }
        }
        false
    })
}

/// Returns the span of the enclosing scope for the cursor offset. For actors,
/// narrows to a specific receive fn when possible. Falls back to the enclosing
/// item span.
fn find_defining_scope(parse_result: &ParseResult, offset: usize) -> Option<Span> {
    for (item, item_span) in &parse_result.program.items {
        if !item_span.contains(&offset) {
            continue;
        }
        if let Item::Actor(a) = item {
            for recv in &a.receive_fns {
                if !recv.span.is_empty() && recv.span.contains(&offset) {
                    return Some(recv.span.clone());
                }
            }
        }
        return Some(item_span.clone());
    }
    None
}

/// Walk the entire AST collecting all spans where `name` appears as an identifier reference.
/// Uses scope-aware resolution: for local variables, only collects references that refer
/// to the same binding as the one at the cursor position.
fn find_all_references(
    source: &str,
    parse_result: &ParseResult,
    offset: usize,
) -> Option<(String, Vec<Span>)> {
    let (name, _) = simple_word_at_offset(source, offset)?;

    let mut spans = Vec::new();
    for (item, _span) in &parse_result.program.items {
        collect_refs_in_item(item, &name, &mut spans);
    }

    // For top-level names (functions, actors, types, receive handlers, fields),
    // return all references globally.
    if is_top_level_name(parse_result, &name) {
        if spans.is_empty() {
            return None;
        }
        return Some((name, spans));
    }

    // For local variables/parameters, restrict to the enclosing function/handler scope.
    if let Some(scope) = find_defining_scope(parse_result, offset) {
        spans.retain(|s| s.start >= scope.start && s.end <= scope.end);
    }

    // Further refine: if the name is shadowed within the scope (e.g., a for-loop
    // re-declares `x`), only keep references that share the same binding.
    // Strategy: find all binding (declaration) spans for `name` within the scope,
    // then determine which binding the cursor belongs to and keep only its references.
    if spans.len() > 1 {
        let binding_spans: Vec<&Span> = spans
            .iter()
            .filter(|s| is_binding_span(source, s, &name))
            .collect();

        // If there are multiple bindings of the same name (shadowing), disambiguate.
        if binding_spans.len() > 1 {
            // Find which binding region the cursor falls in.
            // Each binding "owns" references from its declaration to the next binding
            // (or end of scope).
            let mut sorted_bindings: Vec<usize> = binding_spans.iter().map(|s| s.start).collect();
            sorted_bindings.sort_unstable();

            // Find the binding region containing the cursor offset.
            let mut region_start = 0;
            let mut region_end = usize::MAX;
            for (i, &bstart) in sorted_bindings.iter().enumerate() {
                if bstart <= offset {
                    region_start = bstart;
                    region_end = sorted_bindings.get(i + 1).copied().unwrap_or(usize::MAX);
                }
            }

            spans.retain(|s| s.start >= region_start && s.start < region_end);
        }
    }

    if spans.is_empty() {
        None
    } else {
        Some((name, spans))
    }
}

/// Heuristic: check if a span is a binding (declaration) site for a variable name.
/// Looks at the source text preceding the identifier to detect let/var/for keywords.
fn is_binding_span(source: &str, span: &Span, _name: &str) -> bool {
    if span.start < 2 || span.start > source.len() {
        return false;
    }
    let prefix_start = span.start.saturating_sub(20);
    let prefix = source.get(prefix_start..span.start).unwrap_or("");
    let trimmed = prefix.trim_end();
    trimmed.ends_with("let") || trimmed.ends_with("var") || trimmed.ends_with("for")
}

fn collect_refs_in_item(item: &Item, name: &str, spans: &mut Vec<Span>) {
    match item {
        Item::Function(f) => {
            collect_refs_in_block(&f.body, name, spans);
        }
        Item::Actor(a) => {
            if let Some(init) = &a.init {
                collect_refs_in_block(&init.body, name, spans);
            }
            for recv in &a.receive_fns {
                collect_refs_in_block(&recv.body, name, spans);
            }
            for method in &a.methods {
                collect_refs_in_block(&method.body, name, spans);
            }
        }
        Item::TypeDecl(td) => {
            for body_item in &td.body {
                if let TypeBodyItem::Method(m) = body_item {
                    collect_refs_in_block(&m.body, name, spans);
                }
            }
        }
        Item::Impl(i) => {
            for method in &i.methods {
                collect_refs_in_block(&method.body, name, spans);
            }
        }
        Item::Trait(t) => {
            for trait_item in &t.items {
                if let TraitItem::Method(m) = trait_item {
                    if let Some(body) = &m.body {
                        collect_refs_in_block(body, name, spans);
                    }
                }
            }
        }
        Item::Const(c) => {
            collect_refs_in_expr(&c.value.0, &c.value.1, name, spans);
        }
        Item::Import(_)
        | Item::ExternBlock(_)
        | Item::Wire(_)
        | Item::TypeAlias(_)
        | Item::Supervisor(_) => {}
    }
}

fn collect_refs_in_block(block: &Block, name: &str, spans: &mut Vec<Span>) {
    for (stmt, _span) in &block.stmts {
        collect_refs_in_stmt(stmt, name, spans);
    }
    if let Some(trailing) = &block.trailing_expr {
        collect_refs_in_expr(&trailing.0, &trailing.1, name, spans);
    }
}

fn collect_refs_in_stmt(stmt: &Stmt, name: &str, spans: &mut Vec<Span>) {
    match stmt {
        Stmt::Let { pattern, value, .. } => {
            collect_refs_in_pattern(&pattern.0, &pattern.1, name, spans);
            if let Some(val) = value {
                collect_refs_in_expr(&val.0, &val.1, name, spans);
            }
        }
        Stmt::Var { value, .. } => {
            if let Some(val) = value {
                collect_refs_in_expr(&val.0, &val.1, name, spans);
            }
        }
        Stmt::Assign { target, value, .. } => {
            collect_refs_in_expr(&target.0, &target.1, name, spans);
            collect_refs_in_expr(&value.0, &value.1, name, spans);
        }
        Stmt::If {
            condition,
            then_block,
            else_block,
        } => {
            collect_refs_in_expr(&condition.0, &condition.1, name, spans);
            collect_refs_in_block(then_block, name, spans);
            if let Some(eb) = else_block {
                if let Some(if_stmt) = &eb.if_stmt {
                    collect_refs_in_stmt(&if_stmt.0, name, spans);
                }
                if let Some(block) = &eb.block {
                    collect_refs_in_block(block, name, spans);
                }
            }
        }
        Stmt::IfLet {
            pattern,
            expr,
            body,
            else_body,
        } => {
            collect_refs_in_pattern(&pattern.0, &pattern.1, name, spans);
            collect_refs_in_expr(&expr.0, &expr.1, name, spans);
            collect_refs_in_block(body, name, spans);
            if let Some(block) = else_body {
                collect_refs_in_block(block, name, spans);
            }
        }
        Stmt::Match { scrutinee, arms } => {
            collect_refs_in_expr(&scrutinee.0, &scrutinee.1, name, spans);
            for arm in arms {
                collect_refs_in_pattern(&arm.pattern.0, &arm.pattern.1, name, spans);
                if let Some(guard) = &arm.guard {
                    collect_refs_in_expr(&guard.0, &guard.1, name, spans);
                }
                collect_refs_in_expr(&arm.body.0, &arm.body.1, name, spans);
            }
        }
        Stmt::Loop { body, .. } => {
            collect_refs_in_block(body, name, spans);
        }
        Stmt::While {
            condition, body, ..
        } => {
            collect_refs_in_expr(&condition.0, &condition.1, name, spans);
            collect_refs_in_block(body, name, spans);
        }
        Stmt::For {
            pattern,
            iterable,
            body,
            ..
        } => {
            collect_refs_in_pattern(&pattern.0, &pattern.1, name, spans);
            collect_refs_in_expr(&iterable.0, &iterable.1, name, spans);
            collect_refs_in_block(body, name, spans);
        }
        Stmt::Return(Some(val)) => {
            collect_refs_in_expr(&val.0, &val.1, name, spans);
        }
        Stmt::Defer(expr) => {
            collect_refs_in_expr(&expr.0, &expr.1, name, spans);
        }
        Stmt::Expression(expr) => {
            collect_refs_in_expr(&expr.0, &expr.1, name, spans);
        }
        Stmt::Break { value: Some(v), .. } => {
            collect_refs_in_expr(&v.0, &v.1, name, spans);
        }
        Stmt::Return(None) | Stmt::Break { value: None, .. } | Stmt::Continue { .. } => {}
    }
}

#[expect(
    clippy::too_many_lines,
    reason = "match arms over all Expr variants is clearest as one function"
)]
fn collect_refs_in_expr(expr: &Expr, span: &Span, name: &str, spans: &mut Vec<Span>) {
    match expr {
        Expr::Identifier(ident) if ident == name => {
            spans.push(span.clone());
        }
        Expr::Binary { left, right, .. } => {
            collect_refs_in_expr(&left.0, &left.1, name, spans);
            collect_refs_in_expr(&right.0, &right.1, name, spans);
        }
        Expr::Unary { operand, .. } => {
            collect_refs_in_expr(&operand.0, &operand.1, name, spans);
        }
        Expr::Call { function, args, .. } => {
            collect_refs_in_expr(&function.0, &function.1, name, spans);
            for arg in args {
                let e = arg.expr();
                collect_refs_in_expr(&e.0, &e.1, name, spans);
            }
        }
        Expr::MethodCall { receiver, args, .. } => {
            collect_refs_in_expr(&receiver.0, &receiver.1, name, spans);
            for arg in args {
                let e = arg.expr();
                collect_refs_in_expr(&e.0, &e.1, name, spans);
            }
        }
        Expr::FieldAccess { object, .. } => {
            collect_refs_in_expr(&object.0, &object.1, name, spans);
        }
        Expr::Index { object, index } => {
            collect_refs_in_expr(&object.0, &object.1, name, spans);
            collect_refs_in_expr(&index.0, &index.1, name, spans);
        }
        Expr::StructInit { fields, .. } => {
            for (_, val) in fields {
                collect_refs_in_expr(&val.0, &val.1, name, spans);
            }
        }
        Expr::Spawn { target, args } => {
            collect_refs_in_expr(&target.0, &target.1, name, spans);
            for (_, val) in args {
                collect_refs_in_expr(&val.0, &val.1, name, spans);
            }
        }
        Expr::Block(block)
        | Expr::Unsafe(block)
        | Expr::ScopeLaunch(block)
        | Expr::ScopeSpawn(block) => {
            collect_refs_in_block(block, name, spans);
        }
        Expr::Scope { body, .. } => {
            collect_refs_in_block(body, name, spans);
        }
        Expr::If {
            condition,
            then_block,
            else_block,
        } => {
            collect_refs_in_expr(&condition.0, &condition.1, name, spans);
            collect_refs_in_expr(&then_block.0, &then_block.1, name, spans);
            if let Some(eb) = else_block {
                collect_refs_in_expr(&eb.0, &eb.1, name, spans);
            }
        }
        Expr::IfLet {
            pattern,
            expr,
            body,
            else_body,
        } => {
            collect_refs_in_pattern(&pattern.0, &pattern.1, name, spans);
            collect_refs_in_expr(&expr.0, &expr.1, name, spans);
            collect_refs_in_block(body, name, spans);
            if let Some(block) = else_body {
                collect_refs_in_block(block, name, spans);
            }
        }
        Expr::Match { scrutinee, arms } => {
            collect_refs_in_expr(&scrutinee.0, &scrutinee.1, name, spans);
            for arm in arms {
                collect_refs_in_pattern(&arm.pattern.0, &arm.pattern.1, name, spans);
                if let Some(guard) = &arm.guard {
                    collect_refs_in_expr(&guard.0, &guard.1, name, spans);
                }
                collect_refs_in_expr(&arm.body.0, &arm.body.1, name, spans);
            }
        }
        Expr::Lambda { body, .. } | Expr::SpawnLambdaActor { body, .. } => {
            collect_refs_in_expr(&body.0, &body.1, name, spans);
        }
        Expr::ArrayRepeat { value, count } => {
            collect_refs_in_expr(&value.0, &value.1, name, spans);
            collect_refs_in_expr(&count.0, &count.1, name, spans);
        }
        Expr::Tuple(elems) | Expr::Array(elems) | Expr::Join(elems) => {
            for elem in elems {
                collect_refs_in_expr(&elem.0, &elem.1, name, spans);
            }
        }
        Expr::Send { target, message } => {
            collect_refs_in_expr(&target.0, &target.1, name, spans);
            collect_refs_in_expr(&message.0, &message.1, name, spans);
        }
        Expr::Select { arms, timeout } => {
            for arm in arms {
                collect_refs_in_pattern(&arm.binding.0, &arm.binding.1, name, spans);
                collect_refs_in_expr(&arm.source.0, &arm.source.1, name, spans);
                collect_refs_in_expr(&arm.body.0, &arm.body.1, name, spans);
            }
            if let Some(to) = timeout {
                collect_refs_in_expr(&to.duration.0, &to.duration.1, name, spans);
                collect_refs_in_expr(&to.body.0, &to.body.1, name, spans);
            }
        }
        Expr::Timeout { expr: e, duration } => {
            collect_refs_in_expr(&e.0, &e.1, name, spans);
            collect_refs_in_expr(&duration.0, &duration.1, name, spans);
        }
        Expr::Await(inner) | Expr::PostfixTry(inner) | Expr::Yield(Some(inner)) => {
            collect_refs_in_expr(&inner.0, &inner.1, name, spans);
        }
        Expr::Cast { expr: inner, .. } => {
            collect_refs_in_expr(&inner.0, &inner.1, name, spans);
        }
        Expr::Range { start, end, .. } => {
            if let Some(s) = start {
                collect_refs_in_expr(&s.0, &s.1, name, spans);
            }
            if let Some(e) = end {
                collect_refs_in_expr(&e.0, &e.1, name, spans);
            }
        }
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let StringPart::Expr(e) = part {
                    collect_refs_in_expr(&e.0, &e.1, name, spans);
                }
            }
        }
        _ => {}
    }
}

fn collect_refs_in_pattern(pattern: &Pattern, span: &Span, name: &str, spans: &mut Vec<Span>) {
    match pattern {
        Pattern::Identifier(ident) if ident == name => {
            spans.push(span.clone());
        }
        Pattern::Constructor { patterns, .. } | Pattern::Tuple(patterns) => {
            for (p, s) in patterns {
                collect_refs_in_pattern(p, s, name, spans);
            }
        }
        Pattern::Struct { fields, .. } => {
            for field in fields {
                if let Some((p, s)) = &field.pattern {
                    collect_refs_in_pattern(p, s, name, spans);
                }
            }
        }
        Pattern::Or(left, right) => {
            collect_refs_in_pattern(&left.0, &left.1, name, spans);
            collect_refs_in_pattern(&right.0, &right.1, name, spans);
        }
        _ => {}
    }
}

// ── Hover / go-to-definition helpers ─────────────────────────────────

/// Extract the word (identifier) at a byte offset in the source.
/// Also tries `offset - 1` when the cursor is right after an identifier.
fn word_at_offset(source: &str, offset: usize) -> Option<String> {
    if let Some(w) = word_at_offset_exact(source, offset) {
        return Some(w);
    }
    // Cursor may be right after the identifier end — try one position back.
    if offset > 0 {
        return word_at_offset_exact(source, offset - 1);
    }
    None
}

/// Extract the identifier at exactly `offset`, or try to extend it with a
/// preceding dot-separated qualifier (e.g. `http.listen` → `"http.listen"`).
fn word_at_offset_exact(source: &str, offset: usize) -> Option<String> {
    if offset >= source.len() {
        return None;
    }
    let bytes = source.as_bytes();
    let is_ident = |b: u8| b.is_ascii_alphanumeric() || b == b'_';
    if !is_ident(bytes[offset]) {
        return None;
    }
    let start = source[..offset]
        .bytes()
        .rev()
        .take_while(|b| is_ident(*b))
        .count();
    let end = source[offset..]
        .bytes()
        .take_while(|b| is_ident(*b))
        .count();
    let word_start = offset - start;
    let word_end = offset + end;
    let word = &source[word_start..word_end];
    if word.is_empty() {
        return None;
    }

    // Check for double-colon-qualified prefix (e.g. `Counter::increment`).
    if word_start >= 3 && bytes[word_start - 1] == b':' && bytes[word_start - 2] == b':' {
        let prefix_end = word_start - 2;
        let prefix_len = source[..prefix_end]
            .bytes()
            .rev()
            .take_while(|b| is_ident(*b))
            .count();
        if prefix_len > 0 {
            let qualified = &source[prefix_end - prefix_len..word_end];
            return Some(qualified.to_string());
        }
    }

    // Check for dot-qualified prefix (e.g. `http.listen`).
    if word_start >= 2 && bytes[word_start - 1] == b'.' {
        let prefix_end = word_start - 1;
        let prefix_len = source[..prefix_end]
            .bytes()
            .rev()
            .take_while(|b| is_ident(*b))
            .count();
        if prefix_len > 0 {
            let qualified = &source[prefix_end - prefix_len..word_end];
            return Some(qualified.to_string());
        }
    }

    Some(word.to_string())
}

/// Format a function signature for hover display.
fn format_fn_signature(name: &str, sig: &FnSig) -> String {
    let pure_prefix = if sig.is_pure { "pure " } else { "" };
    let async_prefix = if sig.is_async { "async " } else { "" };
    let param_list: Vec<String> = sig.params.iter().map(ToString::to_string).collect();
    let ret = if sig.return_type == Ty::Unit {
        String::new()
    } else {
        format!(" -> {}", sig.return_type)
    };
    let code = format!(
        "```hew\n{pure_prefix}{async_prefix}fn {name}({}){ret}\n```",
        param_list.join(", ")
    );
    if let Some(doc) = &sig.doc_comment {
        format!("{doc}\n\n---\n\n{code}")
    } else {
        code
    }
}

/// Format a function signature as a single inline line (for embedding in type hover).
fn format_fn_signature_inline(name: &str, sig: &FnSig) -> String {
    let pure_prefix = if sig.is_pure { "pure " } else { "" };
    let async_prefix = if sig.is_async { "async " } else { "" };
    let param_list: Vec<String> = sig.params.iter().map(ToString::to_string).collect();
    if sig.return_type == Ty::Unit {
        format!(
            "{pure_prefix}{async_prefix}fn {name}({})",
            param_list.join(", ")
        )
    } else {
        format!(
            "{pure_prefix}{async_prefix}fn {name}({}) -> {}",
            param_list.join(", "),
            sig.return_type
        )
    }
}

/// Format a type definition for hover display.
fn format_type_def_hover(type_def: &hew_types::check::TypeDef) -> String {
    use hew_types::check::TypeDefKind;
    use std::fmt::Write;
    let kind_str = match type_def.kind {
        TypeDefKind::Struct => "type",
        TypeDefKind::Enum => "enum",
        TypeDefKind::Actor => "actor",
    };
    let type_params = if type_def.type_params.is_empty() {
        String::new()
    } else {
        format!("<{}>", type_def.type_params.join(", "))
    };
    let mut parts = format!("```hew\n{kind_str} {}{type_params}", type_def.name);
    let has_body = !type_def.fields.is_empty()
        || !type_def.variants.is_empty()
        || !type_def.methods.is_empty();
    if has_body {
        parts.push_str(" {\n");
        for (field_name, field_ty) in &type_def.fields {
            let _ = writeln!(parts, "    {field_name}: {field_ty},");
        }
        for (variant_name, payload) in &type_def.variants {
            match payload {
                hew_types::VariantDef::Unit => {
                    let _ = writeln!(parts, "    {variant_name},");
                }
                hew_types::VariantDef::Tuple(types) => {
                    let types: Vec<String> = types.iter().map(ToString::to_string).collect();
                    let _ = writeln!(parts, "    {variant_name}({}),", types.join(", "));
                }
                hew_types::VariantDef::Struct(fields) => {
                    let fields: Vec<String> =
                        fields.iter().map(|(n, t)| format!("{n}: {t}")).collect();
                    let _ = writeln!(parts, "    {variant_name} {{ {} }},", fields.join(", "));
                }
            }
        }
        for (method_name, sig) in &type_def.methods {
            let _ = writeln!(
                parts,
                "    {}",
                format_fn_signature_inline(method_name, sig)
            );
        }
        parts.push('}');
    }
    parts.push_str("\n```");
    if let Some(doc) = &type_def.doc_comment {
        format!("{doc}\n\n---\n\n{parts}")
    } else {
        parts
    }
}

// ── Document links ──────────────────────────────────────────────────

fn build_document_links(uri: &Url, source: &str, parse_result: &ParseResult) -> Vec<DocumentLink> {
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
                        range: span_to_range(source, span),
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
            let range = span_to_range(source, span);
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
    parse_result: &ParseResult,
) -> Vec<TypeHierarchyItem> {
    let mut supers = Vec::new();

    // For actors, collect their declared super_traits.
    for (item, _) in &parse_result.program.items {
        if let Item::Actor(a) = item {
            if a.name == name {
                if let Some(bounds) = &a.super_traits {
                    for bound in bounds {
                        if let Some(hi) =
                            find_type_hierarchy_item(uri, source, parse_result, &bound.name)
                        {
                            supers.push(hi);
                        }
                    }
                }
                return supers;
            }
        }
    }

    // For types, find impl blocks: `impl TraitName for TypeName`
    for (item, _) in &parse_result.program.items {
        if let Item::Impl(impl_decl) = item {
            let target = match &impl_decl.target_type.0 {
                hew_parser::ast::TypeExpr::Named { name: tname, .. } => tname.as_str(),
                _ => continue,
            };
            if target == name {
                if let Some(bound) = &impl_decl.trait_bound {
                    if let Some(hi) =
                        find_type_hierarchy_item(uri, source, parse_result, &bound.name)
                    {
                        supers.push(hi);
                    }
                }
            }
        }
    }
    supers
}

fn collect_subtypes(
    uri: &Url,
    trait_name: &str,
    source: &str,
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
                    if let Some(hi) = find_type_hierarchy_item(uri, source, parse_result, target) {
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
                    if let Some(hi) = find_type_hierarchy_item(uri, source, parse_result, &a.name) {
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
    parse_result: &ParseResult,
    name: &str,
) -> Option<CallHierarchyItem> {
    for (item, item_span) in &parse_result.program.items {
        match item {
            Item::Function(f) if f.name == name => {
                let range = span_to_range(source, item_span);
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
                        let range = span_to_range(source, fn_span);
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
            collect_calls_in_block(body, calls)
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
        | Expr::RegexLiteral(_)
        | Expr::Yield(None) => {}
    }
}

fn find_incoming_calls(
    uri: &Url,
    source: &str,
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
                    let range = span_to_range(source, item_span);
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
                            .map(|c| span_to_range(source, &c.span))
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
                        let range = span_to_range(source, fn_span);
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
                                .map(|c| span_to_range(source, &c.span))
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
            .push(span_to_range(source, &cs.span));
    }

    grouped
        .into_iter()
        .filter_map(|(callee_name, ranges)| {
            let target = find_callable_at(uri, source, parse_result, &callee_name)?;
            Some(CallHierarchyOutgoingCall {
                to: target,
                from_ranges: ranges,
            })
        })
        .collect()
}

// ── Code lens helpers ───────────────────────────────────────────────

fn build_code_lenses(source: &str, parse_result: &ParseResult) -> Vec<CodeLens> {
    let mut lenses = Vec::new();
    for (item, item_span) in &parse_result.program.items {
        match item {
            Item::Function(f) => {
                let range = span_to_range(source, item_span);
                let ref_count = count_references(parse_result, &f.name);
                lenses.push(CodeLens {
                    range,
                    command: Some(Command {
                        title: format!(
                            "{ref_count} reference{}",
                            if ref_count == 1 { "" } else { "s" }
                        ),
                        command: String::new(),
                        arguments: None,
                    }),
                    data: None,
                });
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
                let range = span_to_range(source, item_span);
                let ref_count = count_references(parse_result, &a.name);
                lenses.push(CodeLens {
                    range,
                    command: Some(Command {
                        title: format!(
                            "{ref_count} reference{}",
                            if ref_count == 1 { "" } else { "s" }
                        ),
                        command: String::new(),
                        arguments: None,
                    }),
                    data: None,
                });
                for recv in &a.receive_fns {
                    let recv_range = if recv.span.is_empty() {
                        range
                    } else {
                        span_to_range(source, &recv.span)
                    };
                    let recv_ref_count = count_references(parse_result, &recv.name);
                    lenses.push(CodeLens {
                        range: recv_range,
                        command: Some(Command {
                            title: format!(
                                "{recv_ref_count} reference{}",
                                if recv_ref_count == 1 { "" } else { "s" }
                            ),
                            command: String::new(),
                            arguments: None,
                        }),
                        data: None,
                    });
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

fn count_references(parse_result: &ParseResult, name: &str) -> usize {
    let mut spans = Vec::new();
    for (item, _) in &parse_result.program.items {
        collect_refs_in_item(item, name, &mut spans);
    }
    spans.len()
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
                            range: span_to_range(source, item_span),
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
                            range: span_to_range(source, item_span),
                        },
                        container_name: None,
                    });
                }
                for recv in &a.receive_fns {
                    if query.is_empty() || recv.name.to_lowercase().contains(&query_lower) {
                        let recv_range = if recv.span.is_empty() {
                            span_to_range(source, item_span)
                        } else {
                            span_to_range(source, &recv.span)
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
                            range: span_to_range(source, item_span),
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
                            range: span_to_range(source, item_span),
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
                            range: span_to_range(source, item_span),
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

// ── Inlay hints ─────────────────────────────────────────────────────

/// Build inlay hints for unannotated bindings and closures.
fn build_inlay_hints(
    source: &str,
    parse_result: &ParseResult,
    tc: &TypeCheckOutput,
) -> Vec<InlayHint> {
    let mut hints = Vec::new();
    for (item, _span) in &parse_result.program.items {
        collect_inlay_hints_from_item(source, item, tc, &mut hints);
    }
    hints
}

fn collect_inlay_hints_from_item(
    source: &str,
    item: &Item,
    tc: &TypeCheckOutput,
    hints: &mut Vec<InlayHint>,
) {
    match item {
        Item::Function(f) => collect_inlay_hints_from_block(source, &f.body, tc, hints),
        Item::Actor(a) => {
            for recv in &a.receive_fns {
                collect_inlay_hints_from_block(source, &recv.body, tc, hints);
            }
            for method in &a.methods {
                collect_inlay_hints_from_block(source, &method.body, tc, hints);
            }
        }
        Item::TypeDecl(td) => {
            for body_item in &td.body {
                if let TypeBodyItem::Method(method) = body_item {
                    collect_inlay_hints_from_block(source, &method.body, tc, hints);
                }
            }
        }
        Item::Impl(i) => {
            for method in &i.methods {
                collect_inlay_hints_from_block(source, &method.body, tc, hints);
            }
        }
        _ => {}
    }
}

fn collect_inlay_hints_from_block(
    source: &str,
    block: &Block,
    tc: &TypeCheckOutput,
    hints: &mut Vec<InlayHint>,
) {
    for (stmt, _span) in &block.stmts {
        collect_inlay_hints_from_stmt(source, stmt, tc, hints);
    }
}

#[expect(
    clippy::cast_possible_truncation,
    reason = "line/col values in source files will not exceed u32"
)]
fn collect_inlay_hints_from_stmt(
    source: &str,
    stmt: &Stmt,
    tc: &TypeCheckOutput,
    hints: &mut Vec<InlayHint>,
) {
    match stmt {
        Stmt::Let { pattern, ty, value } => {
            if ty.is_none() {
                if let Some(value_expr) = value {
                    let span_key = SpanKey {
                        start: value_expr.1.start,
                        end: value_expr.1.end,
                    };
                    if let Some(inferred_ty) = tc.expr_types.get(&span_key) {
                        let name_end = pattern.1.end;
                        let line_offsets = compute_line_offsets(source);
                        let (line, col) = offset_to_line_col(source, &line_offsets, name_end);
                        hints.push(InlayHint {
                            position: Position::new(line as u32, col as u32),
                            label: InlayHintLabel::String(format!(": {inferred_ty}")),
                            kind: Some(InlayHintKind::TYPE),
                            text_edits: None,
                            tooltip: None,
                            padding_left: None,
                            padding_right: None,
                            data: None,
                        });
                    }
                }
            }
            if let Some(value_expr) = value {
                collect_inlay_hints_from_expr(source, &value_expr.0, tc, hints);
            }
        }
        Stmt::Var {
            ty, value, name, ..
        } => {
            if ty.is_none() {
                if let Some(value_expr) = value {
                    let span_key = SpanKey {
                        start: value_expr.1.start,
                        end: value_expr.1.end,
                    };
                    if let Some(inferred_ty) = tc.expr_types.get(&span_key) {
                        let name_end = find_var_name_end(source, &value_expr.1, name);
                        let line_offsets = compute_line_offsets(source);
                        let (line, col) = offset_to_line_col(source, &line_offsets, name_end);
                        hints.push(InlayHint {
                            position: Position::new(line as u32, col as u32),
                            label: InlayHintLabel::String(format!(": {inferred_ty}")),
                            kind: Some(InlayHintKind::TYPE),
                            text_edits: None,
                            tooltip: None,
                            padding_left: None,
                            padding_right: None,
                            data: None,
                        });
                    }
                }
            }
            if let Some(value_expr) = value {
                collect_inlay_hints_from_expr(source, &value_expr.0, tc, hints);
            }
        }
        Stmt::For { body, .. } | Stmt::Loop { body, .. } | Stmt::While { body, .. } => {
            collect_inlay_hints_from_block(source, body, tc, hints);
        }
        Stmt::If {
            then_block,
            else_block,
            ..
        } => {
            collect_inlay_hints_from_block(source, then_block, tc, hints);
            if let Some(eb) = else_block {
                if let Some(if_stmt) = &eb.if_stmt {
                    collect_inlay_hints_from_stmt(source, &if_stmt.0, tc, hints);
                }
                if let Some(block) = &eb.block {
                    collect_inlay_hints_from_block(source, block, tc, hints);
                }
            }
        }
        Stmt::IfLet {
            body, else_body, ..
        } => {
            collect_inlay_hints_from_block(source, body, tc, hints);
            if let Some(block) = else_body {
                collect_inlay_hints_from_block(source, block, tc, hints);
            }
        }
        Stmt::Match { arms, .. } => {
            for arm in arms {
                collect_inlay_hints_from_expr(source, &arm.body.0, tc, hints);
            }
        }
        Stmt::Expression(expr) => {
            collect_inlay_hints_from_expr(source, &expr.0, tc, hints);
        }
        _ => {}
    }
}

#[expect(
    clippy::cast_possible_truncation,
    reason = "line/col values in source files will not exceed u32"
)]
fn collect_inlay_hints_from_expr(
    source: &str,
    expr: &Expr,
    tc: &TypeCheckOutput,
    hints: &mut Vec<InlayHint>,
) {
    match expr {
        Expr::Lambda {
            return_type, body, ..
        } => {
            if return_type.is_none() {
                let span_key = SpanKey {
                    start: body.1.start,
                    end: body.1.end,
                };
                if let Some(body_ty) = tc.expr_types.get(&span_key) {
                    let hint_pos = body.1.start;
                    let line_offsets = compute_line_offsets(source);
                    let (line, col) = offset_to_line_col(source, &line_offsets, hint_pos);
                    hints.push(InlayHint {
                        position: Position::new(line as u32, col as u32),
                        label: InlayHintLabel::String(format!("-> {body_ty} ")),
                        kind: Some(InlayHintKind::TYPE),
                        text_edits: None,
                        tooltip: None,
                        padding_left: Some(true),
                        padding_right: None,
                        data: None,
                    });
                }
            }
            collect_inlay_hints_from_expr(source, &body.0, tc, hints);
        }
        Expr::Block(block)
        | Expr::Unsafe(block)
        | Expr::ScopeLaunch(block)
        | Expr::ScopeSpawn(block) => {
            collect_inlay_hints_from_block(source, block, tc, hints);
        }
        Expr::Scope { body, .. } => {
            collect_inlay_hints_from_block(source, body, tc, hints);
        }
        Expr::If {
            then_block,
            else_block,
            ..
        } => {
            collect_inlay_hints_from_expr(source, &then_block.0, tc, hints);
            if let Some(else_expr) = else_block {
                collect_inlay_hints_from_expr(source, &else_expr.0, tc, hints);
            }
        }
        Expr::IfLet {
            body, else_body, ..
        } => {
            collect_inlay_hints_from_block(source, body, tc, hints);
            if let Some(block) = else_body {
                collect_inlay_hints_from_block(source, block, tc, hints);
            }
        }
        Expr::Match { arms, .. } => {
            for arm in arms {
                collect_inlay_hints_from_expr(source, &arm.body.0, tc, hints);
            }
        }
        Expr::Cast { expr: inner, .. } => {
            collect_inlay_hints_from_expr(source, &inner.0, tc, hints);
        }
        _ => {}
    }
}

/// Find the end offset of the variable name in a `var name = ...` statement.
fn find_var_name_end(source: &str, value_span: &Span, name: &str) -> usize {
    let before_eq = &source[..value_span.start];
    if let Some(eq_pos) = before_eq.rfind('=') {
        let trimmed = source[..eq_pos].trim_end();
        if trimmed.ends_with(name) {
            return trimmed.len();
        }
    }
    value_span.start.saturating_sub(3)
}

// ── Signature help ──────────────────────────────────────────────────

/// Build signature help at the given offset.
#[expect(
    clippy::cast_possible_truncation,
    reason = "active parameter index fits in u32"
)]
fn build_signature_help(
    source: &str,
    tc: &TypeCheckOutput,
    offset: usize,
) -> Option<SignatureHelp> {
    let (fn_name, active_param) = find_call_context(source, offset)?;
    let sig = find_fn_sig(&fn_name, tc)?;

    let label = format_sig_label(&fn_name, sig);
    let params: Vec<ParameterInformation> = sig
        .param_names
        .iter()
        .zip(&sig.params)
        .map(|(name, ty)| ParameterInformation {
            label: ParameterLabel::Simple(format!("{name}: {ty}")),
            documentation: None,
        })
        .collect();

    Some(SignatureHelp {
        signatures: vec![SignatureInformation {
            label,
            documentation: None,
            parameters: Some(params),
            active_parameter: Some(active_param as u32),
        }],
        active_signature: Some(0),
        active_parameter: None,
    })
}

/// Find the function name and active parameter index at the cursor offset.
fn find_call_context(source: &str, offset: usize) -> Option<(String, usize)> {
    let bytes = &source.as_bytes()[..offset];
    let mut depth: i32 = 0;
    let mut comma_count: usize = 0;
    let mut i = bytes.len();

    while i > 0 {
        i -= 1;
        match bytes[i] {
            b')' | b']' | b'}' => depth += 1,
            b'(' => {
                if depth == 0 {
                    let fn_name = extract_fn_name_before(source, i)?;
                    return Some((fn_name, comma_count));
                }
                depth -= 1;
            }
            b'[' | b'{' => {
                if depth == 0 {
                    return None;
                }
                depth -= 1;
            }
            b',' if depth == 0 => comma_count += 1,
            _ => {}
        }
    }
    None
}

/// Extract the function/method name immediately before the `(` at `paren_pos`.
fn extract_fn_name_before(source: &str, paren_pos: usize) -> Option<String> {
    let before = source[..paren_pos].trim_end();
    if before.is_empty() {
        return None;
    }
    let bytes = before.as_bytes();
    let end = bytes.len();
    let mut start = end;

    while start > 0 {
        let ch = bytes[start - 1];
        if ch.is_ascii_alphanumeric() || ch == b'_' || ch == b'.' || ch == b':' {
            start -= 1;
        } else {
            break;
        }
    }

    if start == end {
        return None;
    }

    Some(before[start..end].to_string())
}

/// Find a function signature by name, checking `fn_sigs`, `type_defs` methods, and qualified names.
fn find_fn_sig<'a>(name: &str, tc: &'a TypeCheckOutput) -> Option<&'a FnSig> {
    if let Some(sig) = tc.fn_sigs.get(name) {
        return Some(sig);
    }

    // For method calls like `receiver.method`, try the method part.
    if let Some(method) = name.rsplit('.').next() {
        if method != name {
            for type_def in tc.type_defs.values() {
                if let Some(sig) = type_def.methods.get(method) {
                    return Some(sig);
                }
            }
            for (sig_name, sig) in &tc.fn_sigs {
                if sig_name.ends_with(&format!("::{method}")) {
                    return Some(sig);
                }
            }
        }
    }

    // Try just the last component as a plain function name.
    let last = name.rsplit(['.', ':']).find(|s| !s.is_empty())?;
    if last != name {
        if let Some(sig) = tc.fn_sigs.get(last) {
            return Some(sig);
        }
    }

    None
}

/// Format signature label like `fn name(param1: Type, param2: Type) -> RetType`.
fn format_sig_label(name: &str, sig: &FnSig) -> String {
    let pure_prefix = if sig.is_pure { "pure " } else { "" };
    let async_prefix = if sig.is_async { "async " } else { "" };
    let params: Vec<String> = sig
        .param_names
        .iter()
        .zip(&sig.params)
        .map(|(n, t)| format!("{n}: {t}"))
        .collect();
    let ret = if sig.return_type == Ty::Unit {
        String::new()
    } else {
        format!(" -> {}", sig.return_type)
    };
    let display_name = name
        .rsplit(['.', ':'])
        .find(|s| !s.is_empty())
        .unwrap_or(name);
    format!(
        "{pure_prefix}{async_prefix}fn {display_name}({}){ret}",
        params.join(", ")
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn semantic_token_data(source: &str, tokens: &[SemanticToken]) -> Vec<(String, u32, u32)> {
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
            let start = position_to_offset(source, Position::new(line, col));
            let end = position_to_offset(source, Position::new(line, col + token.length));
            data.push((
                source[start..end].to_string(),
                token.token_type,
                token.token_modifiers_bitset,
            ));
        }
        data
    }

    #[test]
    fn line_offsets_empty() {
        assert_eq!(compute_line_offsets(""), vec![0]);
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
        assert_eq!(position_to_offset(source, Position::new(0, 0)), 0);
        assert_eq!(position_to_offset(source, Position::new(1, 1)), 4);
        assert_eq!(position_to_offset(source, Position::new(2, 0)), 6);
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
        // Position(0, 1) → byte 1 (start of 'é')
        assert_eq!(position_to_offset(source, Position::new(0, 1)), 1);
        // Position(0, 2) → byte 3 (after 'é', which is \n)
        assert_eq!(position_to_offset(source, Position::new(0, 2)), 3);
        // Position(1, 0) → byte 4
        assert_eq!(position_to_offset(source, Position::new(1, 0)), 4);
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
        let result = hew_parser::parse("fn {");
        let uri = Url::parse("file:///test.hew").unwrap();
        let diags = build_diagnostics(&uri, "fn {", &result, None);
        assert!(!diags.is_empty());
        assert_eq!(diags[0].severity, Some(DiagnosticSeverity::ERROR));
    }

    #[test]
    fn completions_include_keywords() {
        let parse_result = hew_parser::parse("");
        let doc = DocumentState {
            source: String::new(),
            parse_result,
            type_output: None,
        };
        let items = build_completions_at(&doc, 0);
        let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
        assert!(labels.contains(&"fn"));
        assert!(labels.contains(&"actor"));
        assert!(labels.contains(&"spawn"));
    }

    #[test]
    fn snippets_match_select_and_timeout_syntax() {
        let snippets = keyword_snippets();
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
        let symbols = build_document_symbols(source, &parse_result);
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "foo");
        assert_eq!(symbols[0].kind, SymbolKind::FUNCTION);
    }

    #[test]
    fn semantic_tokens_for_let() {
        let source = "let x = 42;";
        let parse_result = hew_parser::parse(source);
        let tokens = build_semantic_tokens(source, &parse_result);
        assert!(!tokens.is_empty());
        // First token should be `let` keyword
        assert_eq!(
            tokens[0].token_type,
            token_type_index(&SemanticTokenType::KEYWORD)
        );
        // Second token is `x` identifier — should have DECLARATION modifier (bit 0)
        assert_eq!(
            tokens[1].token_type,
            token_type_index(&SemanticTokenType::VARIABLE)
        );
        assert_eq!(
            tokens[1].token_modifiers_bitset & 1,
            1,
            "x should be DECLARATION"
        );
    }

    #[test]
    fn semantic_token_modifiers_async_and_const() {
        let source = "const Y = 1; async fn foo() {}";
        let parse_result = hew_parser::parse(source);
        let tokens = build_semantic_tokens(source, &parse_result);
        // Find the `const` keyword and the identifier after it.
        let const_tok = &tokens[0]; // `const`
        assert_eq!(
            const_tok.token_type,
            token_type_index(&SemanticTokenType::KEYWORD)
        );
        let y_tok = &tokens[1]; // `Y`
        assert_eq!(
            y_tok.token_type,
            token_type_index(&SemanticTokenType::VARIABLE)
        );
        assert_eq!(
            y_tok.token_modifiers_bitset & 1,
            1,
            "Y should be DECLARATION"
        );
        assert_eq!(y_tok.token_modifiers_bitset & 2, 2, "Y should be READONLY");

        // Find `async` keyword — should have ASYNC modifier.
        let async_tok = tokens.iter().find(|t| {
            t.token_type == token_type_index(&SemanticTokenType::KEYWORD)
                && t.token_modifiers_bitset & 4 == 4
        });
        assert!(
            async_tok.is_some(),
            "should find async keyword with ASYNC modifier"
        );
    }

    #[test]
    fn semantic_tokens_include_bitwise_and_shift_operators() {
        let source = "fn main() { let a = 1; let b = 2; let x = ~a << b >> 1 & a | b ^ a; x <<= 1; x >>= 1; x &= 1; x |= 1; x ^= 1; }";
        let parse_result = hew_parser::parse(source);
        let tokens = build_semantic_tokens(source, &parse_result);
        let operator_token_type = token_type_index(&SemanticTokenType::OPERATOR);
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
        let parse_result = hew_parser::parse(source);
        let tokens = build_semantic_tokens(source, &parse_result);
        let token_data = semantic_token_data(source, &tokens);
        let function_token_type = token_type_index(&SemanticTokenType::FUNCTION);
        let type_token_type = token_type_index(&SemanticTokenType::TYPE);

        assert!(
            token_data
                .iter()
                .any(|(text, token_type, modifiers)| text == "calc"
                    && *token_type == function_token_type
                    && (modifiers & 1) == 1),
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
        let mut checker = Checker::new();
        let type_output = checker.check_program(&parse_result.program);

        let doc = DocumentState {
            source: source.to_string(),
            parse_result,
            type_output: Some(type_output),
        };
        // Offset right after the dot in `p.x`
        let dot_pos = source.find("p.x").unwrap() + 2;
        let items = build_completions_at(&doc, dot_pos);
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
            parse_result,
            type_output: None,
        };
        let spawn_offset = source.find("spawn ").unwrap() + 7;
        let items = build_completions_at(&doc, spawn_offset);
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

        let mut checker = Checker::new();
        let type_output = checker.check_program(&parse_result.program);
        let doc = DocumentState {
            source: source.to_string(),
            parse_result,
            type_output: Some(type_output),
        };
        let offset = source.find("0 }").unwrap();
        let items = build_completions_at(&doc, offset);
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
        let source = r#"fn main() {
    if true {
        let branch_only = 1;
    }
    let outside = 2;
    outside
}"#;
        let parse_result = hew_parser::parse(source);
        assert!(
            parse_result.errors.is_empty(),
            "parse errors: {:?}",
            parse_result.errors
        );
        let doc = DocumentState {
            source: source.to_string(),
            parse_result,
            type_output: None,
        };
        let offset = source.rfind("outside\n").unwrap();
        let items = build_completions_at(&doc, offset);
        let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
        assert!(labels.contains(&"outside"));
        assert!(
            !labels.contains(&"branch_only"),
            "branch-only local should not be in scope after if block: {labels:?}"
        );
    }

    #[test]
    fn completions_cover_type_impl_methods_if_else_match_and_patterns() {
        let source = r#"
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
"#;
        let parse_result = hew_parser::parse(source);
        assert!(
            parse_result.errors.is_empty(),
            "parse errors: {:?}",
            parse_result.errors
        );
        let doc = DocumentState {
            source: source.to_string(),
            parse_result,
            type_output: None,
        };

        let else_if_offset = source.find("else_if_local\n").unwrap();
        let else_if_items = build_completions_at(&doc, else_if_offset);
        let else_if_labels: Vec<&str> = else_if_items.iter().map(|i| i.label.as_str()).collect();
        assert!(else_if_labels.contains(&"input"));
        assert!(else_if_labels.contains(&"else_if_local"));

        let match_offset = source.find("match_local\n").unwrap();
        let match_items = build_completions_at(&doc, match_offset);
        let match_labels: Vec<&str> = match_items.iter().map(|i| i.label.as_str()).collect();
        assert!(match_labels.contains(&"ok_value"));
        assert!(match_labels.contains(&"x"));
        assert!(match_labels.contains(&"y_value"));
        assert!(match_labels.contains(&"match_local"));

        let or_pattern_offset = source.find("err_a + err_b").unwrap();
        let or_pattern_items = build_completions_at(&doc, or_pattern_offset);
        let or_pattern_labels: Vec<&str> =
            or_pattern_items.iter().map(|i| i.label.as_str()).collect();
        assert!(or_pattern_labels.contains(&"err_a"));
        assert!(or_pattern_labels.contains(&"err_b"));

        let impl_offset = source.find("inner + code").unwrap();
        let impl_items = build_completions_at(&doc, impl_offset);
        let impl_labels: Vec<&str> = impl_items.iter().map(|i| i.label.as_str()).collect();
        assert!(impl_labels.contains(&"inner"));
        assert!(impl_labels.contains(&"code"));
    }

    #[test]
    fn goto_def_receive_method() {
        let source = "actor Counter {\n    count: i32;\n    receive fn increment(n: i32) {\n        count = count + n;\n    }\n}\nfn main() { let c = spawn Counter(count: 0); c.increment(1); }";
        let parse_result = hew_parser::parse(source);
        let call_offset = source.rfind("increment").unwrap();
        let word = word_at_offset(source, call_offset).unwrap();
        // word_at_offset returns the dot-qualified form "c.increment"
        assert_eq!(word, "c.increment");
        // find_definition_in_ast doesn't find the full dot-qualified name...
        assert!(find_definition_in_ast(source, &parse_result, &word).is_none());
        // ...but the method part alone finds the receive method definition.
        let method = word.rsplit('.').next().unwrap();
        let found = find_definition_in_ast(source, &parse_result, method);
        assert!(found.is_some(), "should find receive method definition");
    }

    #[test]
    fn goto_def_double_colon_method_fallback() {
        let source = "actor Counter {\n    receive fn increment(n: i32) {\n        n\n    }\n}\nfn main() { Counter::increment(1); }";
        let parse_result = hew_parser::parse(source);
        let call_offset = source.rfind("increment").unwrap();
        let word = word_at_offset(source, call_offset).unwrap();
        assert_eq!(word, "Counter::increment");
        assert!(find_definition_in_ast(source, &parse_result, &word).is_none());
        let method = word.rsplit("::").next().unwrap();
        let found = find_definition_in_ast(source, &parse_result, method);
        assert!(
            found.is_some(),
            "should find receive method definition via :: fallback"
        );
    }

    #[test]
    fn find_definition_includes_extern_functions_and_associated_types() {
        let source = "extern \"C\" { fn c_abs(x: i32) -> i32; }\ntrait Iter { type Item; fn next() -> i32; }";
        let parse_result = hew_parser::parse(source);
        assert!(
            parse_result.errors.is_empty(),
            "parse errors: {:?}",
            parse_result.errors
        );
        assert!(find_definition_in_ast(source, &parse_result, "c_abs").is_some());
        assert!(find_definition_in_ast(source, &parse_result, "Item").is_some());
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
        let result = find_all_references(source, &parse_result, foo_x_offset);
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
        let result = find_all_references(source, &parse_result, call_offset);
        assert!(result.is_some());
        let (name, spans) = result.unwrap();
        assert_eq!(name, "helper");
        // Should find the call in main (top-level name = global search)
        assert!(!spans.is_empty(), "expected refs for helper");
    }
}
