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
    // `source` is accepted for future use (e.g. extracting doc comments) but
    // currently unused — suppress the warning.
    let _ = source;

    parse_result
        .program
        .items
        .iter()
        .map(|(item, span)| item_to_symbol(item, OffsetSpan::from(span.clone())))
        .collect()
}

/// Convert a single top-level AST item into a `SymbolInfo` (with children where appropriate).
#[expect(
    clippy::too_many_lines,
    reason = "one arm per AST variant, not meaningfully splittable"
)]
fn item_to_symbol(item: &Item, item_span: OffsetSpan) -> SymbolInfo {
    match item {
        Item::Function(f) => make_symbol(&f.name, SymbolKind::Function, item_span),
        Item::Actor(a) => {
            let mut sym = make_symbol(&a.name, SymbolKind::Actor, item_span);
            let mut children = Vec::new();
            if a.init.is_some() {
                children.push(make_symbol("init", SymbolKind::Constructor, item_span));
            }
            if a.terminate.is_some() {
                children.push(make_symbol("terminate", SymbolKind::Method, item_span));
            }
            for recv in &a.receive_fns {
                let recv_span = if recv.span.is_empty() {
                    item_span
                } else {
                    OffsetSpan::from(recv.span.clone())
                };
                children.push(make_symbol(&recv.name, SymbolKind::Method, recv_span));
            }
            for method in &a.methods {
                children.push(make_symbol(&method.name, SymbolKind::Method, item_span));
            }
            if let Some(c) = crate::util::non_empty(children) {
                sym.children = c;
            }
            sym
        }
        Item::Supervisor(s) => make_symbol(&s.name, SymbolKind::Supervisor, item_span),
        Item::Trait(t) => {
            let mut sym = make_symbol(&t.name, SymbolKind::Trait, item_span);
            let children: Vec<SymbolInfo> = t
                .items
                .iter()
                .map(|item| match item {
                    TraitItem::Method(m) => make_symbol(&m.name, SymbolKind::Method, item_span),
                    TraitItem::AssociatedType { name, .. } => {
                        make_symbol(name, SymbolKind::TypeAlias, item_span)
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
            let mut sym = make_symbol(&name, SymbolKind::Impl, item_span);
            let children: Vec<SymbolInfo> = i
                .methods
                .iter()
                .map(|m| make_symbol(&m.name, SymbolKind::Method, item_span))
                .collect();
            if let Some(c) = crate::util::non_empty(children) {
                sym.children = c;
            }
            sym
        }
        Item::Const(c) => make_symbol(&c.name, SymbolKind::Constant, item_span),
        Item::TypeDecl(td) => {
            let kind = match td.kind {
                TypeDeclKind::Struct => SymbolKind::Type,
                TypeDeclKind::Enum => SymbolKind::Enum,
            };
            let mut sym = make_symbol(&td.name, kind, item_span);
            let children: Vec<SymbolInfo> = td
                .body
                .iter()
                .filter_map(|item| match item {
                    TypeBodyItem::Variant(v) => {
                        Some(make_symbol(&v.name, SymbolKind::Variant, item_span))
                    }
                    TypeBodyItem::Method(m) => {
                        Some(make_symbol(&m.name, SymbolKind::Method, item_span))
                    }
                    TypeBodyItem::Field { .. } => None,
                })
                .collect();
            if let Some(c) = crate::util::non_empty(children) {
                sym.children = c;
            }
            sym
        }
        Item::Wire(w) => make_symbol(&w.name, SymbolKind::Wire, item_span),
        Item::Machine(m) => make_symbol(&m.name, SymbolKind::Machine, item_span),
        Item::TypeAlias(ta) => make_symbol(&ta.name, SymbolKind::TypeAlias, item_span),
        Item::ExternBlock(eb) => {
            let mut sym = make_symbol(
                &format!("extern \"{}\"", eb.abi),
                SymbolKind::Module,
                item_span,
            );
            let children: Vec<SymbolInfo> = eb
                .functions
                .iter()
                .map(|f| make_symbol(&f.name, SymbolKind::Function, item_span))
                .collect();
            if let Some(c) = crate::util::non_empty(children) {
                sym.children = c;
            }
            sym
        }
        Item::Import(i) => {
            let name = i.path.join("::");
            make_symbol(&name, SymbolKind::Module, item_span)
        }
    }
}

/// Create a `SymbolInfo` with the given name, kind, and span (no children).
fn make_symbol(name: &str, kind: SymbolKind, span: OffsetSpan) -> SymbolInfo {
    SymbolInfo {
        name: name.to_string(),
        kind,
        span,
        selection_span: span,
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
}
