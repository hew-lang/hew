use std::collections::HashMap;

use dashmap::DashMap;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    CodeAction, CodeActionKind, CodeActionOrCommand, CodeActionParams, CodeActionResponse,
    CompletionItem, CompletionItemKind, CompletionParams, CompletionResponse, Diagnostic,
    DocumentFormattingParams, DocumentLink, DocumentLinkParams, DocumentSymbol,
    DocumentSymbolParams, DocumentSymbolResponse, Documentation, FoldingRange, FoldingRangeKind,
    FoldingRangeParams, Hover, HoverContents, HoverParams, InlayHint, InlayHintKind,
    InlayHintLabel, InlayHintParams, InlayHintTooltip, InsertTextFormat, MarkupContent, MarkupKind,
    ParameterInformation, ParameterLabel, Position, SemanticTokens, SemanticTokensParams,
    SemanticTokensResult, SignatureHelp, SignatureHelpParams, SignatureInformation, TextEdit, Url,
    WorkspaceEdit,
};
use tracing::warn;

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
            let documentation = sig.documentation.map(|doc| {
                Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: doc,
                })
            });
            SignatureInformation {
                label: sig.label,
                documentation,
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
    let suggestions = diagnostic_suggestions(diag);
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

fn diagnostic_suggestions(diag: &Diagnostic) -> Vec<String> {
    let Some(raw_suggestions) = diag.data.as_ref().and_then(|data| data.get("suggestions")) else {
        return vec![];
    };

    match serde_json::from_value::<Vec<String>>(raw_suggestions.clone()) {
        Ok(suggestions) => suggestions,
        Err(error) => {
            warn!(
                target: "hew-lsp",
                diagnostic_message = %diag.message,
                error = %error,
                "failed to deserialize code-action suggestions; returning empty suggestion list"
            );
            vec![]
        }
    }
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
            let mut items: Vec<_> = analysis_items.into_iter().map(to_lsp_completion).collect();
            append_concurrency_snippets(&mut items, &doc.source, offset);
            items
        }
        None => vec![],
    };
    CompletionResponse::Array(items)
}

pub(super) fn append_concurrency_snippets(
    items: &mut Vec<CompletionItem>,
    source: &str,
    offset: usize,
) {
    if !should_offer_concurrency_snippets(source, offset) {
        return;
    }
    for (label, insert_text, detail) in [
        (
            "channel.new...",
            "let (${1:tx}, ${2:rx}): (channel.Sender<${3:string}>, channel.Receiver<${3:string}>) = channel.new(${4:capacity});",
            "let (tx, rx) = channel.new(capacity);",
        ),
        (
            "await rx.recv...",
            "match await ${1:rx}.recv() {\n\tSome(${2:value}) => ${3:expr},\n\tNone => ${0:closed_expr},\n}",
            "await rx.recv()",
        ),
        (
            "select rx.recv...",
            "select {\n\t${1:item} from ${2:rx}.recv() => ${3:expr},\n\tafter ${4:duration} => ${0:timeout_expr},\n}",
            "select { item from rx.recv() => ..., after duration => ... }",
        ),
    ] {
        if items.iter().any(|item| item.label == label) {
            continue;
        }
        items.push(CompletionItem {
            label: label.to_string(),
            kind: Some(CompletionItemKind::SNIPPET),
            detail: Some(detail.to_string()),
            insert_text: Some(insert_text.to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            sort_text: Some(format!("0_{label}")),
            ..Default::default()
        });
    }
}

fn should_offer_concurrency_snippets(source: &str, offset: usize) -> bool {
    if offset > source.len() {
        return false;
    }

    let line_start = source[..offset].rfind('\n').map_or(0, |idx| idx + 1);
    let prefix = &source[line_start..offset];
    let mut in_string = false;
    let mut escaped = false;
    let mut chars = prefix.chars().peekable();
    while let Some(ch) = chars.next() {
        if in_string {
            if escaped {
                escaped = false;
            } else if ch == '\\' {
                escaped = true;
            } else if ch == '"' {
                in_string = false;
            }
            continue;
        }
        if ch == '"' {
            in_string = true;
            continue;
        }
        if ch == '/' && chars.peek() == Some(&'/') {
            return false;
        }
    }
    if in_string {
        return false;
    }

    let trimmed = prefix.trim_end();
    let mut boundary = trimmed.len();
    while boundary > 0 {
        let ch = trimmed[..boundary]
            .chars()
            .next_back()
            .expect("boundary is not empty");
        if ch == '_' || ch.is_ascii_alphanumeric() {
            boundary -= ch.len_utf8();
        } else {
            break;
        }
    }

    trimmed[..boundary]
        .chars()
        .next_back()
        .is_none_or(|ch| ch != '.')
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
) -> CodeActionResponse {
    code_action_response(&server.documents, params)
}

pub(crate) fn code_action_response(
    documents: &DashMap<Url, DocumentState>,
    params: &CodeActionParams,
) -> CodeActionResponse {
    let uri = &params.text_document.uri;
    let Some(doc) = documents.get(uri) else {
        warn!(
            target: "hew-lsp",
            uri = %uri,
            "code action requested for unknown document; returning empty response"
        );
        return vec![];
    };

    let mut lsp_actions = Vec::new();
    for diag in &params.context.diagnostics {
        lsp_actions.extend(lsp_code_actions_for_diagnostic(
            uri,
            &doc,
            diag,
            params.context.only.as_deref(),
        ));
    }
    lsp_actions
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

pub(crate) fn formatting(
    server: &HewLanguageServer,
    params: &DocumentFormattingParams,
) -> Option<Vec<TextEdit>> {
    let uri = &params.text_document.uri;
    let doc = server.documents.get(uri)?;
    let formatted = hew_parser::fmt::format_source(&doc.source, &doc.parse_result.program);
    if formatted == doc.source {
        // Already canonical: signal success with an empty edit list.
        return Some(vec![]);
    }
    // Whole-document replacement. Use offset_range_to_lsp — not doc.source.len() as raw bytes —
    // so that the LSP Position::character field is UTF-16 code units, as the spec requires.
    let range = offset_range_to_lsp(&doc.source, &doc.line_offsets, 0, doc.source.len());
    Some(vec![TextEdit {
        range,
        new_text: formatted,
    }])
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn concurrency_snippets_cover_channel_recv_and_select_surfaces() {
        let mut items = Vec::new();
        append_concurrency_snippets(&mut items, "", 0);

        let labels: Vec<_> = items.iter().map(|item| item.label.as_str()).collect();
        assert!(labels.contains(&"channel.new..."));
        assert!(labels.contains(&"await rx.recv..."));
        assert!(labels.contains(&"select rx.recv..."));

        let snippets: Vec<_> = items
            .iter()
            .map(|item| item.insert_text.as_deref().unwrap_or_default())
            .collect();
        assert!(snippets.iter().any(|text| text.contains("channel.new(")));
        assert!(snippets
            .iter()
            .any(|text| text.contains("await ${1:rx}.recv()")));
        assert!(snippets
            .iter()
            .any(|text| text.contains("from ${2:rx}.recv()")));

        let original_len = items.len();
        append_concurrency_snippets(&mut items, "", 0);
        assert_eq!(items.len(), original_len, "snippets must not duplicate");
    }

    #[test]
    fn concurrency_snippets_skip_member_string_and_comment_contexts() {
        for (source, offset) in [
            ("fn main() { rx. }", "fn main() { rx.".len()),
            ("fn main() { rx.rec }", "fn main() { rx.rec".len()),
            ("fn main() { \"rx", "fn main() { \"rx".len()),
            ("fn main() { // rx", "fn main() { // rx".len()),
        ] {
            let mut items = Vec::new();
            append_concurrency_snippets(&mut items, source, offset);
            assert!(
                items.is_empty(),
                "concurrency snippets should be gated out at offset {offset} in {source:?}"
            );
        }
    }
}
