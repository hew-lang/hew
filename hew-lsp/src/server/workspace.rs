use hew_analysis::references::count_all_references;
use hew_parser::ast::{Attribute, Item};
use hew_parser::ParseResult;
use tower_lsp::lsp_types::{CodeLens, Command, Location, SymbolInformation, SymbolKind, Url};

use super::span_to_range;

// ── Code lens helpers ───────────────────────────────────────────────

pub(super) fn build_code_lenses(
    source: &str,
    lo: &[usize],
    parse_result: &ParseResult,
) -> Vec<CodeLens> {
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

pub(super) fn has_test_attribute(attrs: &[Attribute]) -> bool {
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
pub(super) fn collect_workspace_symbols(
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
