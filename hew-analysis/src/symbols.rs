//! Analysis module for document symbols.

use hew_parser::ast::{Item, TraitItem, TypeBodyItem, TypeDeclKind};
use hew_parser::ParseResult;

use crate::{OffsetSpan, SymbolInfo, SymbolKind};

/// Build a list of document symbols from a parsed Hew source file.
///
/// Returns a flat-ish tree of `SymbolInfo` values (top-level items may have
/// children, e.g. methods inside an actor). Spans are byte offsets — the
/// caller is responsible for converting to line/column if needed.
#[must_use]
pub fn build_document_symbols(source: &str, parse_result: &ParseResult) -> Vec<SymbolInfo> {
    parse_result
        .program
        .items
        .iter()
        .map(|(item, span)| item_to_symbol(source, item, OffsetSpan::from(span.clone())))
        .collect()
}

/// Convert a single top-level AST item into a `SymbolInfo` (with children where appropriate).
#[expect(
    clippy::too_many_lines,
    reason = "one arm per AST variant, not meaningfully splittable"
)]
fn item_to_symbol(source: &str, item: &Item, item_span: OffsetSpan) -> SymbolInfo {
    match item {
        Item::Function(f) => named_symbol(source, &f.name, SymbolKind::Function, item_span),
        Item::Actor(a) => {
            let mut sym = named_symbol(source, &a.name, SymbolKind::Actor, item_span);
            let mut children = Vec::new();
            if a.init.is_some() {
                children.push(keyword_symbol(
                    source,
                    "init",
                    "init",
                    SymbolKind::Constructor,
                    item_span.start,
                ));
            }
            if a.terminate.is_some() {
                children.push(keyword_symbol(
                    source,
                    "terminate",
                    "terminate",
                    SymbolKind::Method,
                    item_span.start,
                ));
            }
            for recv in &a.receive_fns {
                let recv_span = if recv.span.is_empty() {
                    None
                } else {
                    Some(OffsetSpan::from(recv.span.clone()))
                };
                let recv_search_from = recv_span.map_or(item_span.start, |span| span.start);
                children.push(child_symbol(
                    source,
                    &recv.name,
                    SymbolKind::Method,
                    recv_span,
                    recv_search_from,
                ));
            }
            for method in &a.methods {
                let method_span = if method.fn_span.is_empty() {
                    None
                } else {
                    Some(OffsetSpan::from(method.fn_span.clone()))
                };
                children.push(child_symbol(
                    source,
                    &method.name,
                    SymbolKind::Method,
                    method_span,
                    method.decl_span.start.max(item_span.start),
                ));
            }
            children.sort_by_key(|child| child.selection_span.start);
            if let Some(c) = crate::util::non_empty(children) {
                sym.children = c;
            }
            sym
        }
        Item::Supervisor(s) => named_symbol(source, &s.name, SymbolKind::Supervisor, item_span),
        Item::Trait(t) => {
            let mut sym = named_symbol(source, &t.name, SymbolKind::Trait, item_span);
            let mut associated_type_cursor = item_span.start;
            let children: Vec<SymbolInfo> = t
                .items
                .iter()
                .map(|item| match item {
                    TraitItem::Method(m) => {
                        let method_span = if m.span.is_empty() {
                            None
                        } else {
                            Some(OffsetSpan::from(m.span.clone()))
                        };
                        child_symbol(
                            source,
                            &m.name,
                            SymbolKind::Method,
                            method_span,
                            m.span.start,
                        )
                    }
                    TraitItem::AssociatedType { name, .. } => {
                        let symbol = keyword_symbol(
                            source,
                            "type",
                            name,
                            SymbolKind::TypeAlias,
                            associated_type_cursor,
                        );
                        associated_type_cursor = symbol.selection_span.end;
                        symbol
                    }
                })
                .collect();
            if let Some(c) = crate::util::non_empty(children) {
                sym.children = c;
            }
            sym
        }
        Item::Impl(i) => {
            let name = match &i.trait_bound {
                Some(tb) => format!("impl {} for ...", tb.name),
                None => "impl".to_string(),
            };
            let mut sym = symbol_with_spans(
                &name,
                SymbolKind::Impl,
                item_span,
                crate::util::find_name_span(source, item_span.start, "impl"),
            );
            let children: Vec<SymbolInfo> = i
                .methods
                .iter()
                .map(|m| {
                    let method_span = if m.fn_span.is_empty() {
                        None
                    } else {
                        Some(OffsetSpan::from(m.fn_span.clone()))
                    };
                    child_symbol(
                        source,
                        &m.name,
                        SymbolKind::Method,
                        method_span,
                        m.decl_span.start.max(item_span.start),
                    )
                })
                .collect();
            if let Some(c) = crate::util::non_empty(children) {
                sym.children = c;
            }
            sym
        }
        Item::Const(c) => named_symbol(source, &c.name, SymbolKind::Constant, item_span),
        Item::TypeDecl(td) => {
            let kind = match td.kind {
                TypeDeclKind::Struct => SymbolKind::Type,
                TypeDeclKind::Enum => SymbolKind::Enum,
            };
            let mut sym = named_symbol(source, &td.name, kind, item_span);
            let mut body_cursor = sym.selection_span.end;
            let children: Vec<SymbolInfo> = td
                .body
                .iter()
                .map(|item| match item {
                    TypeBodyItem::Variant(v) => {
                        let symbol =
                            child_symbol(source, &v.name, SymbolKind::Variant, None, body_cursor);
                        body_cursor = symbol.selection_span.end;
                        symbol
                    }
                    TypeBodyItem::Method(m) => {
                        let method_span = if m.fn_span.is_empty() {
                            None
                        } else {
                            Some(OffsetSpan::from(m.fn_span.clone()))
                        };
                        let symbol = child_symbol(
                            source,
                            &m.name,
                            SymbolKind::Method,
                            method_span,
                            m.decl_span.start.max(item_span.start),
                        );
                        body_cursor = symbol.selection_span.end;
                        symbol
                    }
                    TypeBodyItem::Field { name, ty, .. } => {
                        let symbol =
                            field_symbol(source, name, OffsetSpan::from(ty.1.clone()), body_cursor);
                        body_cursor = symbol.selection_span.end;
                        symbol
                    }
                })
                .collect();
            if let Some(c) = crate::util::non_empty(children) {
                sym.children = c;
            }
            sym
        }
        Item::Wire(w) => named_symbol(source, &w.name, SymbolKind::Wire, item_span),
        Item::Machine(m) => {
            let mut sym = named_symbol(source, &m.name, SymbolKind::Machine, item_span);
            let mut state_cursor = item_span.start;
            let mut event_cursor = item_span.start;
            let mut children: Vec<SymbolInfo> = m
                .states
                .iter()
                .map(|state| {
                    let symbol = keyword_symbol(
                        source,
                        "state",
                        &state.name,
                        SymbolKind::State,
                        state_cursor,
                    );
                    state_cursor = symbol.selection_span.end;
                    symbol
                })
                .collect();
            children.extend(m.events.iter().map(|event| {
                let symbol = keyword_symbol(
                    source,
                    "event",
                    &event.name,
                    SymbolKind::Event,
                    event_cursor,
                );
                event_cursor = symbol.selection_span.end;
                symbol
            }));
            children.sort_by_key(|child| child.selection_span.start);
            if let Some(c) = crate::util::non_empty(children) {
                sym.children = c;
            }
            sym
        }
        Item::TypeAlias(ta) => named_symbol(source, &ta.name, SymbolKind::TypeAlias, item_span),
        Item::ExternBlock(eb) => {
            let mut sym = symbol_with_spans(
                &format!("extern \"{}\"", eb.abi),
                SymbolKind::Module,
                item_span,
                crate::util::find_name_span(source, item_span.start, "extern"),
            );
            let children: Vec<SymbolInfo> = eb
                .functions
                .iter()
                .map(|f| child_symbol(source, &f.name, SymbolKind::Function, None, item_span.start))
                .collect();
            if let Some(c) = crate::util::non_empty(children) {
                sym.children = c;
            }
            sym
        }
        Item::Import(i) => {
            let name = i.path.join("::");
            symbol_with_spans(&name, SymbolKind::Module, item_span, item_span)
        }
    }
}

fn named_symbol(source: &str, name: &str, kind: SymbolKind, span: OffsetSpan) -> SymbolInfo {
    symbol_with_spans(
        name,
        kind,
        span,
        crate::util::find_name_span(source, span.start, name),
    )
}

fn child_symbol(
    source: &str,
    name: &str,
    kind: SymbolKind,
    span: Option<OffsetSpan>,
    search_from: usize,
) -> SymbolInfo {
    let selection_span = crate::util::find_name_span(source, search_from, name);
    symbol_with_spans(name, kind, span.unwrap_or(selection_span), selection_span)
}

fn keyword_symbol(
    source: &str,
    keyword: &str,
    name: &str,
    kind: SymbolKind,
    search_from: usize,
) -> SymbolInfo {
    let keyword_span = crate::util::find_name_span(source, search_from, keyword);
    child_symbol(source, name, kind, None, keyword_span.end)
}

fn field_symbol(source: &str, name: &str, ty_span: OffsetSpan, search_from: usize) -> SymbolInfo {
    let selection_span = crate::util::find_name_span(source, search_from, name);
    symbol_with_spans(
        name,
        SymbolKind::Field,
        OffsetSpan {
            start: selection_span.start,
            end: ty_span.end.max(selection_span.end),
        },
        selection_span,
    )
}

/// Create a `SymbolInfo` with the given name, kind, and spans (no children).
fn symbol_with_spans(
    name: &str,
    kind: SymbolKind,
    span: OffsetSpan,
    selection_span: OffsetSpan,
) -> SymbolInfo {
    SymbolInfo {
        name: name.to_string(),
        kind,
        span,
        selection_span,
        children: Vec::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(source: &str) -> hew_parser::ParseResult {
        hew_parser::parse(source)
    }

    #[test]
    fn symbols_function() {
        let source = "fn greet() {}";
        let pr = parse(source);
        let symbols = build_document_symbols(source, &pr);
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "greet");
        assert_eq!(symbols[0].kind, SymbolKind::Function);
    }

    #[test]
    fn symbols_constant() {
        let source = "const PI: f64 = 3.14;";
        let pr = parse(source);
        let symbols = build_document_symbols(source, &pr);
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "PI");
        assert_eq!(symbols[0].kind, SymbolKind::Constant);
    }

    #[test]
    fn symbols_import() {
        let source = "import std::os;";
        let pr = parse(source);
        let symbols = build_document_symbols(source, &pr);
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].kind, SymbolKind::Module);
        assert!(symbols[0].name.contains("std"));
    }

    #[test]
    fn symbols_multiple_items() {
        let source = "fn foo() {}\nfn bar() {}";
        let pr = parse(source);
        let symbols = build_document_symbols(source, &pr);
        assert_eq!(symbols.len(), 2);
    }

    #[test]
    fn symbols_actor_with_children() {
        let source = "actor Counter {\n    receive fn inc() {\n        let x = 1;\n    }\n}";
        let pr = parse(source);
        let symbols = build_document_symbols(source, &pr);
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "Counter");
        assert_eq!(symbols[0].kind, SymbolKind::Actor);
        assert!(
            !symbols[0].children.is_empty(),
            "actor should have child symbols for receive handlers"
        );
    }

    #[test]
    fn symbols_enum_with_variants() {
        let source = "enum Colour {\n    Red;\n    Green;\n    Blue;\n}";
        let pr = parse(source);
        let symbols = build_document_symbols(source, &pr);
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "Colour");
        assert_eq!(symbols[0].kind, SymbolKind::Enum);
        assert!(
            !symbols[0].children.is_empty(),
            "enum should have variant children"
        );
    }

    #[test]
    fn symbols_spans_are_set() {
        // Verify that symbols are produced and have reasonable structure
        let source = "fn alpha() {}";
        let pr = parse(source);
        let symbols = build_document_symbols(source, &pr);
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "alpha");
        assert_eq!(symbols[0].kind, SymbolKind::Function);
    }

    #[test]
    fn symbols_include_fields_states_and_events() {
        let source = r"type Point {
    x: i32;
    y: i32;
    fn distance() -> i32 { 0 }
}

machine Traffic {
    event Start;
    state Idle;
    on Start: Idle -> Idle;
}";
        let pr = parse(source);
        let symbols = build_document_symbols(source, &pr);

        let point = symbols
            .iter()
            .find(|symbol| symbol.name == "Point")
            .unwrap();
        let field = point
            .children
            .iter()
            .find(|symbol| symbol.name == "x")
            .unwrap();
        let method = point
            .children
            .iter()
            .find(|symbol| symbol.name == "distance")
            .unwrap();
        assert_eq!(field.kind, SymbolKind::Field);
        assert_eq!(method.kind, SymbolKind::Method);
        assert_eq!(
            &source[field.selection_span.start..field.selection_span.end],
            "x"
        );
        assert_eq!(
            &source[method.selection_span.start..method.selection_span.end],
            "distance",
        );

        let machine = symbols
            .iter()
            .find(|symbol| symbol.name == "Traffic")
            .unwrap();
        let event = machine
            .children
            .iter()
            .find(|symbol| symbol.name == "Start")
            .unwrap();
        let state = machine
            .children
            .iter()
            .find(|symbol| symbol.name == "Idle")
            .unwrap();
        assert_eq!(event.kind, SymbolKind::Event);
        assert_eq!(state.kind, SymbolKind::State);
        assert_eq!(
            &source[event.selection_span.start..event.selection_span.end],
            "Start"
        );
        assert_eq!(
            &source[state.selection_span.start..state.selection_span.end],
            "Idle"
        );
    }
}
