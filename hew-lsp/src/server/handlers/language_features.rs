use std::collections::HashMap;

use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    CodeAction, CodeActionKind, CodeActionOrCommand, CodeActionParams, CodeActionResponse,
    CompletionParams, CompletionResponse, Diagnostic, DocumentLink, DocumentLinkParams,
    DocumentSymbol, DocumentSymbolParams, DocumentSymbolResponse, FoldingRange, FoldingRangeKind,
    FoldingRangeParams, Hover, HoverContents, HoverParams, InlayHint, InlayHintKind,
    InlayHintLabel, InlayHintParams, InlayHintTooltip, MarkupContent, MarkupKind,
    ParameterInformation, ParameterLabel, Position, SemanticTokens, SemanticTokensParams,
    SemanticTokensResult, SignatureHelp, SignatureHelpParams, SignatureInformation, TextEdit,
    WorkspaceEdit,
};

use super::super::{
    analysis_tokens_to_lsp, build_document_links, internal_error, non_empty, offset_range_to_lsp,
    offset_to_line_col, position_to_offset, symbol_info_to_doc_symbol, to_lsp_completion,
    DocumentState, HewLanguageServer,
};

pub(crate) fn remove_unused_imports_kind() -> CodeActionKind {
    CodeActionKind::from(super::super::REMOVE_UNUSED_IMPORTS_KIND)
}

fn code_action_kind_matches_filter(
    kind: &CodeActionKind,
    requested_kinds: Option<&[CodeActionKind]>,
) -> bool {
    let Some(requested_kinds) = requested_kinds else {
        return true;
    };
    requested_kinds.iter().any(|requested| {
        kind.as_str() == requested.as_str()
            || (kind.as_str().starts_with(requested.as_str())
                && kind.as_str().as_bytes().get(requested.as_str().len()) == Some(&b'.'))
    })
}

pub(crate) fn lsp_inlay_hint_from_analysis(
    source: &str,
    line_offsets: &[usize],
    hint: hew_analysis::InlayHint,
) -> InlayHint {
    let (line, col) = offset_to_line_col(source, line_offsets, hint.offset);
    let tooltip = hint.label.clone();
    InlayHint {
        position: Position::new(
            u32::try_from(line).expect("line offsets fit in u32"),
            u32::try_from(col).expect("column offsets fit in u32"),
        ),
        label: InlayHintLabel::String(hint.label),
        kind: Some(match hint.kind {
            hew_analysis::InlayHintKind::Type => InlayHintKind::TYPE,
            hew_analysis::InlayHintKind::Parameter => InlayHintKind::PARAMETER,
        }),
        text_edits: None,
        tooltip: Some(InlayHintTooltip::String(tooltip)),
        padding_left: if hint.padding_left { Some(true) } else { None },
        padding_right: None,
        data: None,
    }
}

pub(crate) fn lsp_signature_help_from_analysis(
    result: hew_analysis::SignatureHelpResult,
) -> SignatureHelp {
    let active_parameter = result.active_parameter;
    let active_signature = result.active_signature;
    let signatures = result
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
                active_parameter,
            }
        })
        .collect();
    SignatureHelp {
        signatures,
        active_signature,
        active_parameter,
    }
}

pub(crate) fn lsp_code_actions_for_diagnostic(
    uri: &tower_lsp::lsp_types::Url,
    doc: &DocumentState,
    diag: &Diagnostic,
    requested_kinds: Option<&[CodeActionKind]>,
) -> Vec<CodeActionOrCommand> {
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
        kind: kind.clone(),
        message: diag.message.clone(),
        span: hew_analysis::OffsetSpan { start, end },
        suggestions,
    };
    let actions = hew_analysis::code_actions::build_code_actions(&doc.source, &[info]);
    let mut lsp_actions = Vec::new();

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
        let edit = WorkspaceEdit {
            changes: Some(changes),
            ..Default::default()
        };

        if code_action_kind_matches_filter(&CodeActionKind::QUICKFIX, requested_kinds) {
            lsp_actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                title: action.title.clone(),
                kind: Some(CodeActionKind::QUICKFIX),
                diagnostics: Some(vec![diag.clone()]),
                edit: Some(edit.clone()),
                ..Default::default()
            }));
        }

        if kind.as_deref() == Some("UnusedImport")
            && action.title == "Remove unused import"
            && code_action_kind_matches_filter(&remove_unused_imports_kind(), requested_kinds)
        {
            lsp_actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                title: action.title,
                kind: Some(remove_unused_imports_kind()),
                diagnostics: Some(vec![diag.clone()]),
                edit: Some(edit),
                ..Default::default()
            }));
        }
    }

    lsp_actions
}

pub(crate) fn completion(
    server: &HewLanguageServer,
    params: &CompletionParams,
) -> CompletionResponse {
    let uri = &params.text_document_position.text_document.uri;
    let position = params.text_document_position.position;
    let items = match server.documents.get(uri) {
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
    CompletionResponse::Array(items)
}

pub(crate) fn hover(server: &HewLanguageServer, params: &HoverParams) -> Option<Hover> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;

    let doc = server.documents.get(uri)?;
    let offset = position_to_offset(&doc.source, &doc.line_offsets, position);

    let result = hew_analysis::hover::hover(
        &doc.source,
        &doc.parse_result,
        doc.type_output.as_ref(),
        offset,
    )?;

    let range = result
        .span
        .map(|s| offset_range_to_lsp(&doc.source, &doc.line_offsets, s.start, s.end));
    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: result.contents,
        }),
        range,
    })
}

pub(crate) fn document_symbol(
    server: &HewLanguageServer,
    params: &DocumentSymbolParams,
) -> Option<DocumentSymbolResponse> {
    let uri = &params.text_document.uri;
    let doc = server.documents.get(uri)?;

    let analysis_symbols =
        hew_analysis::symbols::build_document_symbols(&doc.source, &doc.parse_result);
    let symbols: Vec<DocumentSymbol> = analysis_symbols
        .into_iter()
        .map(|s| symbol_info_to_doc_symbol(&doc.source, &doc.line_offsets, s))
        .collect();
    Some(DocumentSymbolResponse::Nested(symbols))
}

pub(crate) fn semantic_tokens_full(
    server: &HewLanguageServer,
    params: &SemanticTokensParams,
) -> Result<Option<SemanticTokensResult>> {
    let uri = &params.text_document.uri;
    let Some(doc) = server.documents.get(uri) else {
        return Ok(None);
    };

    let analysis_tokens = hew_analysis::semantic_tokens::build_semantic_tokens(&doc.source);
    let tokens = analysis_tokens_to_lsp(&doc.source, &doc.line_offsets, &analysis_tokens)
        .map_err(|error| internal_error(error.message()))?;
    Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
        result_id: None,
        data: tokens,
    })))
}

pub(crate) fn document_link(
    server: &HewLanguageServer,
    params: &DocumentLinkParams,
) -> Option<Vec<DocumentLink>> {
    let uri = &params.text_document.uri;
    let doc = server.documents.get(uri)?;

    let links = build_document_links(uri, &doc.source, &doc.line_offsets, &doc.parse_result);
    non_empty(links)
}

pub(crate) fn inlay_hint(
    server: &HewLanguageServer,
    params: &InlayHintParams,
) -> Option<Vec<InlayHint>> {
    let uri = &params.text_document.uri;
    let doc = server.documents.get(uri)?;
    let tc = doc.type_output.as_ref()?;

    let analysis_hints =
        hew_analysis::inlay_hints::build_inlay_hints(&doc.source, &doc.parse_result, tc);
    let lsp_hints: Vec<InlayHint> = analysis_hints
        .into_iter()
        .map(|hint| lsp_inlay_hint_from_analysis(&doc.source, &doc.line_offsets, hint))
        .collect();
    non_empty(lsp_hints)
}

pub(crate) fn signature_help(
    server: &HewLanguageServer,
    params: &SignatureHelpParams,
) -> Option<SignatureHelp> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;
    let doc = server.documents.get(uri)?;
    let tc = doc.type_output.as_ref()?;
    let offset = position_to_offset(&doc.source, &doc.line_offsets, position);
    let result = hew_analysis::signature_help::build_signature_help(&doc.source, tc, offset)?;
    Some(lsp_signature_help_from_analysis(result))
}

pub(crate) fn code_action(
    server: &HewLanguageServer,
    params: &CodeActionParams,
) -> Option<CodeActionResponse> {
    let uri = &params.text_document.uri;
    let doc = server.documents.get(uri)?;

    let mut lsp_actions = Vec::new();
    for diag in &params.context.diagnostics {
        lsp_actions.extend(lsp_code_actions_for_diagnostic(
            uri,
            &doc,
            diag,
            params.context.only.as_deref(),
        ));
    }
    non_empty(lsp_actions)
}

pub(crate) fn folding_range(
    server: &HewLanguageServer,
    params: &FoldingRangeParams,
) -> Option<Vec<FoldingRange>> {
    let uri = &params.text_document.uri;
    let doc = server.documents.get(uri)?;

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
    non_empty(lsp_ranges)
}
