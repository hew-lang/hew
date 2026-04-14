use hew_analysis::util::offset_to_line_col;
use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, DocumentSymbol, InsertTextFormat, SemanticToken,
    SemanticTokenModifier, SymbolKind,
};

use super::{modifier_bit, offset_range_to_lsp};

// ── Completion ───────────────────────────────────────────────────────

/// Convert an `hew_analysis::CompletionItem` to an `lsp_types::CompletionItem`.
pub(super) fn to_lsp_completion(item: hew_analysis::CompletionItem) -> CompletionItem {
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
pub(super) fn analysis_symbol_kind_to_lsp(kind: hew_analysis::SymbolKind) -> SymbolKind {
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
pub(super) fn symbol_info_to_doc_symbol(
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
pub(super) fn analysis_tokens_to_lsp(
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
