//! Go-to-definition analysis: find the definition site of an identifier in the AST.

use hew_parser::ast::{Item, TraitItem, TypeBodyItem};
use hew_parser::ParseResult;

use crate::OffsetSpan;

/// Search for a definition matching `word` in the AST, including nested items.
///
/// Returns the byte-offset span of the item that defines the given name, or
/// `None` if no matching definition is found.
#[must_use]
pub fn find_definition(
    _source: &str,
    parse_result: &ParseResult,
    word: &str,
) -> Option<OffsetSpan> {
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
            return Some(OffsetSpan {
                start: span.start,
                end: span.end,
            });
        }

        // Search inside actors for receive methods and methods.
        if let Item::Actor(a) = item {
            for recv in &a.receive_fns {
                if recv.name == word {
                    return Some(OffsetSpan {
                        start: span.start,
                        end: span.end,
                    });
                }
            }
            for method in &a.methods {
                if method.name == word {
                    return Some(OffsetSpan {
                        start: span.start,
                        end: span.end,
                    });
                }
            }
        }

        // Search inside TypeDecl for variants and methods.
        if let Item::TypeDecl(td) = item {
            for body_item in &td.body {
                match body_item {
                    TypeBodyItem::Variant(v) if v.name == word => {
                        return Some(OffsetSpan {
                            start: span.start,
                            end: span.end,
                        });
                    }
                    TypeBodyItem::Method(m) if m.name == word => {
                        return Some(OffsetSpan {
                            start: span.start,
                            end: span.end,
                        });
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
                        return Some(OffsetSpan {
                            start: span.start,
                            end: span.end,
                        });
                    }
                    TraitItem::AssociatedType { name, .. } if name == word => {
                        return Some(OffsetSpan {
                            start: span.start,
                            end: span.end,
                        });
                    }
                    _ => {}
                }
            }
        }

        // Search inside Impl for methods.
        if let Item::Impl(i) = item {
            for method in &i.methods {
                if method.name == word {
                    return Some(OffsetSpan {
                        start: span.start,
                        end: span.end,
                    });
                }
            }
        }

        // Search inside extern blocks for function declarations.
        if let Item::ExternBlock(extern_block) = item {
            for function in &extern_block.functions {
                if function.name == word {
                    return Some(OffsetSpan {
                        start: span.start,
                        end: span.end,
                    });
                }
            }
        }
    }
    None
}
