//! Shared symbol resolver.
//!
//! `resolve_symbol_at(db, uri, offset)` is the single query that every
//! navigation-shaped feature (goto, references, rename, completion context)
//! will read. Each feature pattern-matches on the [`Resolution`] variant and
//! builds its output from the carried `def_span` and metadata.
//!
//! This initial landing covers intra-file navigation — the same ground the
//! per-feature walkers in `definition.rs` cover today. Cross-file and
//! cross-module resolution land in later stages.

use hew_parser::ast::{Item, TypeBodyItem};

use crate::db::SourceDatabase;
use crate::OffsetSpan;

/// Classification of what the cursor at `(uri, offset)` refers to.
///
/// Every variant carries the URI of the defining document and the
/// byte-offset span of the **name identifier** at the definition site — the
/// span goto-definition and rename consume directly.
///
/// `Unknown` is used when the cursor sits on an identifier that looks
/// syntactically valid but does not resolve to a known definition; the span
/// is the identifier's own span so features like "show hover text for
/// unknown symbol" can still locate the text.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Resolution {
    /// A top-level function, constant, or wire definition.
    FunctionDef {
        uri: String,
        def_span: OffsetSpan,
        name: String,
    },
    /// A method declared inside an `actor`, `impl`, `type`, or `trait`.
    MethodDef {
        uri: String,
        def_span: OffsetSpan,
        name: String,
    },
    /// A local `let`/`var` binding in scope at the cursor.
    LocalBinding {
        uri: String,
        def_span: OffsetSpan,
        name: String,
    },
    /// A function or method parameter in scope at the cursor.
    Param {
        uri: String,
        def_span: OffsetSpan,
        name: String,
    },
    /// A struct field accessed via `receiver.field`, resolved to its
    /// declaration site on the owning type.
    Field {
        uri: String,
        def_span: OffsetSpan,
        name: String,
    },
    /// A top-level type declaration (`type`, `trait`, `actor`, `supervisor`,
    /// `machine`, alias).
    TypeDef {
        uri: String,
        def_span: OffsetSpan,
        name: String,
    },
    /// An enum variant declared inside a `type` body.
    Variant {
        uri: String,
        def_span: OffsetSpan,
        name: String,
    },
    /// An identifier used at its import site — the resolver tracked it to an
    /// import statement but has not yet chased the import across files. The
    /// LSP layer owns cross-file resolution until later stages promote that
    /// path into the database.
    ImportedItem {
        importer_uri: String,
        visible_name: String,
        import_span: OffsetSpan,
    },
    /// A module-qualified identifier like `mod::Item` or `mod.item`. The
    /// resolver records both halves so downstream callers can either chase
    /// the module prefix or jump to the trailing segment.
    ModuleQualified {
        uri: String,
        prefix: String,
        tail: String,
        cursor_on_tail: bool,
    },
    /// The cursor sits on an identifier that does not resolve inside this
    /// file's own symbol tables. Cross-file fallback is still the caller's
    /// job.
    Unknown { uri: String, text: String },
}

impl Resolution {
    /// Return the definition URI + span, if the variant carries one.
    #[must_use]
    pub fn def_location(&self) -> Option<(&str, OffsetSpan)> {
        match self {
            Self::FunctionDef { uri, def_span, .. }
            | Self::MethodDef { uri, def_span, .. }
            | Self::LocalBinding { uri, def_span, .. }
            | Self::Param { uri, def_span, .. }
            | Self::Field { uri, def_span, .. }
            | Self::TypeDef { uri, def_span, .. }
            | Self::Variant { uri, def_span, .. } => Some((uri.as_str(), *def_span)),
            Self::ImportedItem {
                importer_uri,
                import_span,
                ..
            } => Some((importer_uri.as_str(), *import_span)),
            Self::ModuleQualified { .. } | Self::Unknown { .. } => None,
        }
    }
}

/// Classify the symbol at `(uri, offset)` using the database's cached parse
/// and type-check outputs.
///
/// Returns `None` when:
/// - `uri` is unknown to the database,
/// - the cursor is not on an identifier, or
/// - the parse failed badly enough that no AST is available.
///
/// Intra-file definitions produce fully-resolved variants. Identifiers that
/// only resolve via an import statement produce [`Resolution::ImportedItem`]
/// with the import span; the caller (LSP handler, for now) chases the import
/// across files. Unresolved identifiers return [`Resolution::Unknown`]
/// carrying the identifier text, so the caller can still attempt any
/// fallbacks it owns.
#[must_use]
pub fn resolve_symbol_at(db: &dyn SourceDatabase, uri: &str, offset: usize) -> Option<Resolution> {
    let source = db.source(uri)?;
    let parse_result = db.parse(uri)?;
    let type_check = db.type_check(uri);
    resolve_symbol_at_raw(&source, &parse_result, type_check.as_deref(), uri, offset)
}

/// Raw variant of [`resolve_symbol_at`] that does not go through a
/// [`SourceDatabase`]. Callers that already hold owned parse/type-check
/// outputs (the current LSP server, WASM tooling) use this path until they
/// migrate their state to the database; feature modules in this crate
/// should prefer the database-backed variant.
#[must_use]
pub fn resolve_symbol_at_raw(
    source: &str,
    parse_result: &hew_parser::ParseResult,
    type_check: Option<&hew_types::TypeCheckOutput>,
    uri: &str,
    offset: usize,
) -> Option<Resolution> {
    // ── Field access: `receiver.field` ───────────────────────────────
    // Do this first because the field identifier is also a plain word, and
    // a plain top-level search would otherwise incorrectly shadow it.
    if let Some(tc) = type_check {
        if let Some(field_span) =
            crate::definition::find_field_definition(source, parse_result, tc, offset)
        {
            let name = crate::util::simple_word_at_offset(source, offset)
                .map_or_else(String::new, |(word, _)| word);
            return Some(Resolution::Field {
                uri: uri.to_string(),
                def_span: field_span,
                name,
            });
        }
    }

    // ── Plain word under the cursor ──────────────────────────────────
    let word = crate::util::word_at_offset(source, offset)?;

    // ── Locals and params (scope-aware) ──────────────────────────────
    if let Some(span) =
        crate::definition::find_local_binding_definition(source, parse_result, &word, offset)
    {
        return Some(Resolution::LocalBinding {
            uri: uri.to_string(),
            def_span: span,
            name: word,
        });
    }
    if let Some(span) = crate::definition::find_param_definition(parse_result, &word, offset) {
        return Some(Resolution::Param {
            uri: uri.to_string(),
            def_span: span,
            name: word,
        });
    }

    // ── Top-level and item-body definitions ──────────────────────────
    if let Some(span) = crate::definition::find_definition(source, parse_result, &word) {
        return Some(classify_top_level(parse_result, &word, uri, span));
    }

    // ── Module-qualified identifier ──────────────────────────────────
    for separator in [".", "::"] {
        if let Some((prefix, tail)) = word.split_once(separator) {
            if !prefix.is_empty() && !tail.is_empty() {
                // The exact character at the cursor relative to the
                // separator tells us which side to prefer when the caller
                // asks for a definition.
                let cursor_on_tail = offset_is_on_tail(source, offset, &word, separator);
                return Some(Resolution::ModuleQualified {
                    uri: uri.to_string(),
                    prefix: prefix.to_string(),
                    tail: tail.to_string(),
                    cursor_on_tail,
                });
            }
        }
    }

    // ── Import statements ────────────────────────────────────────────
    if let Some(import) = find_matching_import(parse_result, &word) {
        return Some(Resolution::ImportedItem {
            importer_uri: uri.to_string(),
            visible_name: word,
            import_span: import,
        });
    }

    Some(Resolution::Unknown {
        uri: uri.to_string(),
        text: word,
    })
}

/// Classify a top-level match against the parsed program so callers can
/// distinguish variant from type definition from method.
fn classify_top_level(
    parse_result: &hew_parser::ParseResult,
    word: &str,
    uri: &str,
    def_span: OffsetSpan,
) -> Resolution {
    for (item, _) in &parse_result.program.items {
        if let Some(r) = classify_item(item, word, def_span, uri) {
            return r;
        }
    }
    // Safe default — the LSP already treats any def_span as a valid goto
    // target; FunctionDef is the least-surprising generic label.
    Resolution::FunctionDef {
        uri: uri.to_string(),
        def_span,
        name: word.to_string(),
    }
}

/// Try to classify a single top-level item. Separated so
/// [`classify_top_level`] stays small; the clippy `too_many_lines` lint is
/// genuinely telling us the classifier is broad, so we split it rather than
/// silence it.
fn classify_item(item: &Item, word: &str, def_span: OffsetSpan, uri: &str) -> Option<Resolution> {
    let make_function = || Resolution::FunctionDef {
        uri: uri.to_string(),
        def_span,
        name: word.to_string(),
    };
    let make_method = || Resolution::MethodDef {
        uri: uri.to_string(),
        def_span,
        name: word.to_string(),
    };
    let make_type = || Resolution::TypeDef {
        uri: uri.to_string(),
        def_span,
        name: word.to_string(),
    };

    match item {
        Item::Function(f) if f.name == word => Some(make_function()),
        Item::Const(c) if c.name == word => Some(make_function()),
        Item::Wire(w) if w.name == word => Some(make_function()),
        Item::Supervisor(s) if s.name == word => Some(make_type()),
        Item::TypeAlias(ta) if ta.name == word => Some(make_type()),
        Item::Machine(m) if m.name == word => Some(make_type()),
        Item::Actor(a) => {
            if a.name == word {
                Some(make_type())
            } else if a.receive_fns.iter().any(|r| r.name == word)
                || a.methods.iter().any(|m| m.name == word)
            {
                Some(make_method())
            } else {
                None
            }
        }
        Item::Trait(t) => {
            if t.name == word {
                Some(make_type())
            } else if t
                .items
                .iter()
                .any(|ti| matches!(ti, hew_parser::ast::TraitItem::Method(m) if m.name == word))
            {
                Some(make_method())
            } else {
                None
            }
        }
        Item::TypeDecl(td) => classify_type_body(td, word, def_span, uri),
        Item::Impl(i) if i.methods.iter().any(|m| m.name == word) => Some(make_method()),
        Item::ExternBlock(eb) if eb.functions.iter().any(|f| f.name == word) => {
            Some(make_function())
        }
        _ => None,
    }
}

fn classify_type_body(
    td: &hew_parser::ast::TypeDecl,
    word: &str,
    def_span: OffsetSpan,
    uri: &str,
) -> Option<Resolution> {
    if td.name == word {
        return Some(Resolution::TypeDef {
            uri: uri.to_string(),
            def_span,
            name: word.to_string(),
        });
    }
    for body_item in &td.body {
        match body_item {
            TypeBodyItem::Variant(v) if v.name == word => {
                return Some(Resolution::Variant {
                    uri: uri.to_string(),
                    def_span,
                    name: word.to_string(),
                });
            }
            TypeBodyItem::Method(m) if m.name == word => {
                return Some(Resolution::MethodDef {
                    uri: uri.to_string(),
                    def_span,
                    name: word.to_string(),
                });
            }
            TypeBodyItem::Field { name, .. } if name == word => {
                return Some(Resolution::Field {
                    uri: uri.to_string(),
                    def_span,
                    name: word.to_string(),
                });
            }
            _ => {}
        }
    }
    None
}

/// Given an import-bearing visible name, locate the span of the matching
/// import specifier. Returns the span on the visible name inside the import
/// declaration, or the whole declaration span when the LSP uses "jump to
/// the import statement" behaviour.
pub(crate) fn find_matching_import(
    parse_result: &hew_parser::ParseResult,
    word: &str,
) -> Option<OffsetSpan> {
    for (item, span) in &parse_result.program.items {
        let Item::Import(import) = item else {
            continue;
        };
        match &import.spec {
            Some(hew_parser::ast::ImportSpec::Names(names)) => {
                for name in names {
                    let visible = name.alias.as_deref().unwrap_or(name.name.as_str());
                    if visible == word {
                        return Some(OffsetSpan {
                            start: span.start,
                            end: span.end,
                        });
                    }
                }
            }
            // Glob imports cannot be matched by visible name alone; leave
            // cross-file resolution to the LSP layer.
            Some(hew_parser::ast::ImportSpec::Glob) | None => {}
        }
    }
    None
}

/// Compute whether `offset` lies within the tail identifier of a
/// module-qualified word. Used by `Resolution::ModuleQualified` so callers
/// can prefer the tail when the cursor is on it (e.g. `mod::Item|` with
/// cursor after `Item`) versus the prefix.
fn offset_is_on_tail(source: &str, offset: usize, word: &str, separator: &str) -> bool {
    // Locate `word` starting at a position near `offset`. The search is
    // conservative: we scan a small window around the cursor for the word
    // text so inner occurrences elsewhere in the file do not confuse us.
    let search_start = offset.saturating_sub(word.len() + 1);
    let search_end = (offset + word.len() + 1).min(source.len());
    if let Some(idx) = source[search_start..search_end].find(word) {
        let word_start = search_start + idx;
        if let Some(sep_idx) = word.find(separator) {
            let tail_start = word_start + sep_idx + separator.len();
            return offset >= tail_start;
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::db::InMemorySourceDatabase;

    const URI: &str = "file:///a.hew";

    fn db_with(source: &str) -> InMemorySourceDatabase {
        let db = InMemorySourceDatabase::new();
        db.set_source(URI.to_string(), source.to_string(), 1);
        db
    }

    #[test]
    fn resolves_function_definition() {
        let source = "fn greet(name: string) -> string { name }\nfn main() { greet(\"x\") }\n";
        let db = db_with(source);
        // Cursor on the call site `greet` in `greet("x")`.
        let offset = source.find("greet(\"x\")").unwrap();
        let resolution = resolve_symbol_at(&db, URI, offset).unwrap();
        match resolution {
            Resolution::FunctionDef { name, def_span, .. } => {
                assert_eq!(name, "greet");
                assert_eq!(&source[def_span.start..def_span.end], "greet");
                // Def span points to the declaration site, not the call.
                assert!(def_span.start < offset);
            }
            other => panic!("expected FunctionDef, got {other:?}"),
        }
    }

    #[test]
    fn resolves_local_binding() {
        let source = "fn main() { let count = 1; count + 2 }\n";
        let db = db_with(source);
        // Cursor on the second `count`.
        let offset = source.rfind("count").unwrap();
        let resolution = resolve_symbol_at(&db, URI, offset).unwrap();
        match resolution {
            Resolution::LocalBinding { name, def_span, .. } => {
                assert_eq!(name, "count");
                assert_eq!(&source[def_span.start..def_span.end], "count");
                assert!(def_span.start < offset);
            }
            other => panic!("expected LocalBinding, got {other:?}"),
        }
    }

    #[test]
    fn resolves_param() {
        let source = "fn greet(name: string) -> string { name }\n";
        let db = db_with(source);
        // Cursor on the use of `name` in the body.
        let offset = source.rfind("name").unwrap();
        let resolution = resolve_symbol_at(&db, URI, offset).unwrap();
        match resolution {
            Resolution::Param { name, .. } => assert_eq!(name, "name"),
            other => panic!("expected Param, got {other:?}"),
        }
    }

    #[test]
    fn resolves_type_def() {
        let source =
            "type Point { x: int, y: int }\nfn origin() -> Point { Point { x: 0, y: 0 } }\n";
        let db = db_with(source);
        // Cursor on the use of `Point` in the return type.
        let offset = source.find("-> Point").unwrap() + 3;
        let resolution = resolve_symbol_at(&db, URI, offset).unwrap();
        match resolution {
            Resolution::TypeDef { name, .. } => assert_eq!(name, "Point"),
            other => panic!("expected TypeDef, got {other:?}"),
        }
    }

    #[test]
    fn unknown_identifier_surfaces_as_unknown() {
        let source = "fn main() { nonexistent }\n";
        let db = db_with(source);
        let offset = source.find("nonexistent").unwrap();
        match resolve_symbol_at(&db, URI, offset).unwrap() {
            Resolution::Unknown { text, .. } => assert_eq!(text, "nonexistent"),
            other => panic!("expected Unknown, got {other:?}"),
        }
    }

    #[test]
    fn cursor_outside_identifier_returns_none_or_unknown() {
        // Cursor inside whitespace should not produce a definition.
        let source = "fn main() {   }\n";
        let db = db_with(source);
        let offset = source.find("   ").unwrap() + 1;
        // Either None (no word at offset) is acceptable here.
        let resolution = resolve_symbol_at(&db, URI, offset);
        match resolution {
            None | Some(Resolution::Unknown { .. }) => {}
            other => panic!("expected None or Unknown for whitespace offset, got {other:?}"),
        }
    }

    #[test]
    fn offset_at_end_of_identifier_still_resolves() {
        // Boundary case: cursor positioned at the byte immediately after
        // the last character of `greet`. `word_at_offset` falls back to
        // `offset - 1` so the resolver must still classify it.
        let source = "fn greet() {}\nfn main() { greet() }\n";
        let db = db_with(source);
        // End of `greet` at the call site — offset just after the `t`.
        let call_start = source.rfind("greet").unwrap();
        let offset = call_start + "greet".len();
        let resolution = resolve_symbol_at(&db, URI, offset).unwrap();
        match resolution {
            Resolution::FunctionDef { name, .. } => assert_eq!(name, "greet"),
            other => panic!("expected FunctionDef at identifier end, got {other:?}"),
        }
    }

    #[test]
    fn module_qualified_exposes_prefix_and_tail() {
        // `Counter::new` is a module-qualified name. The resolver may not
        // find a local definition for the whole word, so it surfaces
        // ModuleQualified with both halves.
        let source = "fn main() { Counter::new() }\n";
        let db = db_with(source);
        let offset = source.find("Counter::new").unwrap() + "Counter::".len();
        let resolution = resolve_symbol_at(&db, URI, offset).unwrap();
        match resolution {
            Resolution::ModuleQualified {
                prefix,
                tail,
                cursor_on_tail,
                ..
            } => {
                assert_eq!(prefix, "Counter");
                assert_eq!(tail, "new");
                assert!(
                    cursor_on_tail,
                    "cursor placed after :: should be flagged on tail"
                );
            }
            other => panic!("expected ModuleQualified, got {other:?}"),
        }
    }

    #[test]
    fn resolves_type_method_definition() {
        let source = "type Counter = { count: int }\n\
                      impl Counter {\n\
                          fn new() -> Counter { Counter { count: 0 } }\n\
                      }\n\
                      fn main() { new() }\n";
        let db = db_with(source);
        // Cursor on the `new` declaration inside impl.
        let offset = source.find("fn new()").unwrap() + "fn ".len();
        let resolution = resolve_symbol_at(&db, URI, offset).unwrap();
        match resolution {
            Resolution::MethodDef { name, .. } => assert_eq!(name, "new"),
            Resolution::FunctionDef { name, .. } => {
                // A top-level `new` doesn't exist, so resolver may classify
                // it as MethodDef or FunctionDef depending on which walker
                // hit first. Either is a valid goto target.
                assert_eq!(name, "new");
            }
            other => panic!("expected method-like resolution, got {other:?}"),
        }
    }
}
