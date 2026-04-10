//! Go-to-definition analysis: find the definition site of an identifier in the AST.

use hew_parser::ast::{Item, TraitItem, TypeBodyItem};
use hew_parser::ParseResult;

use crate::OffsetSpan;

/// Search for a definition matching `word` in the AST, including nested items.
///
/// Returns the byte-offset span of the **name identifier** within the defining
/// item, not the span of the whole item. Returns `None` if no matching
/// definition is found.
#[must_use]
pub fn find_definition(source: &str, parse_result: &ParseResult, word: &str) -> Option<OffsetSpan> {
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
            Item::Machine(m) => Some(&m.name),
            _ => None,
        };
        if name.is_some_and(|n| n == word) {
            return Some(crate::util::find_name_span(source, span.start, word));
        }

        // Search inside actors for receive methods and methods.
        if let Item::Actor(a) = item {
            for recv in &a.receive_fns {
                if recv.name == word {
                    // Use the receive fn's own span when available; fall back to
                    // the enclosing item span.
                    let search_from = if recv.span.is_empty() {
                        span.start
                    } else {
                        recv.span.start
                    };
                    return Some(crate::util::find_name_span(source, search_from, word));
                }
            }
            for method in &a.methods {
                if method.name == word {
                    return Some(crate::util::find_name_span(source, span.start, word));
                }
            }
        }

        // Search inside TypeDecl for variants and methods.
        if let Item::TypeDecl(td) = item {
            for body_item in &td.body {
                match body_item {
                    TypeBodyItem::Variant(v) if v.name == word => {
                        return Some(crate::util::find_name_span(source, span.start, word));
                    }
                    TypeBodyItem::Method(m) if m.name == word => {
                        return Some(crate::util::find_name_span(source, span.start, word));
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
                        return Some(crate::util::find_name_span(source, span.start, word));
                    }
                    TraitItem::AssociatedType { name, .. } if name == word => {
                        return Some(crate::util::find_name_span(source, span.start, word));
                    }
                    _ => {}
                }
            }
        }

        // Search inside Impl for methods.
        if let Item::Impl(i) = item {
            for method in &i.methods {
                if method.name == word {
                    return Some(crate::util::find_name_span(source, span.start, word));
                }
            }
        }

        // Search inside extern blocks for function declarations.
        if let Item::ExternBlock(extern_block) = item {
            for function in &extern_block.functions {
                if function.name == word {
                    return Some(crate::util::find_name_span(source, span.start, word));
                }
            }
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(source: &str) -> hew_parser::ParseResult {
        hew_parser::parse(source)
    }

    #[test]
    fn definition_finds_machine_type() {
        let source = "machine TrafficLight { state Green; state Red; }";
        let pr = parse(source);
        let result = find_definition(source, &pr, "TrafficLight");
        assert!(
            result.is_some(),
            "go-to-definition should resolve machine type name"
        );
        // Span must cover "TrafficLight", not the whole item.
        let span = result.unwrap();
        assert_eq!(&source[span.start..span.end], "TrafficLight");
    }

    #[test]
    fn definition_machine_name_not_confused_with_state() {
        let source = "machine TrafficLight { state Green; state Red; }";
        let pr = parse(source);
        // State names are not top-level items; only the machine name resolves.
        let result = find_definition(source, &pr, "Green");
        assert!(
            result.is_none(),
            "machine state names are not top-level definition sites"
        );
    }

    #[test]
    fn definition_lands_on_name_not_keyword() {
        let source = "fn greet() {}";
        let pr = parse(source);
        let result = find_definition(source, &pr, "greet").expect("should find greet");
        assert_eq!(&source[result.start..result.end], "greet");
    }

    #[test]
    fn definition_actor_name() {
        let source = "actor Counter { receive fn inc() {} }";
        let pr = parse(source);
        let result = find_definition(source, &pr, "Counter").expect("should find Counter");
        assert_eq!(&source[result.start..result.end], "Counter");
    }

    #[test]
    fn definition_receive_fn_name() {
        let source = "actor Counter { receive fn inc() {} }";
        let pr = parse(source);
        let result = find_definition(source, &pr, "inc").expect("should find inc");
        assert_eq!(&source[result.start..result.end], "inc");
    }

    #[test]
    fn definition_type_alias_name() {
        let source = "type Foo = i32;";
        let pr = parse(source);
        let result = find_definition(source, &pr, "Foo").expect("should find Foo");
        assert_eq!(&source[result.start..result.end], "Foo");
    }
}
