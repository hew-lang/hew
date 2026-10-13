//! Expansion of parsed declarative macro fragments before semantic checking.

mod attributes;
mod matcher;
mod tokens;
mod transcribe;

use hew_parser::ast::Span;

pub use matcher::{Capture, Captures, Pattern};
pub use transcribe::{transcribe, Generated, OriginKind, TokenOrigin};

#[derive(Debug, Clone)]
pub struct ExpansionError {
    pub message: String,
    pub span: Span,
}

mod control;
mod registry;

use std::cell::RefCell;
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::Arc;

use hew_parser::ast::{Item, Program, Spanned};
use hew_parser::macro_syntax::{SharedExpander, TokenContexts};
use hew_parser::parser::Parser;
use hew_parser::{ParseDiagnosticKind, ParseError, Severity};

/// Expand every module through the same ordinary parser used for source input.
///
/// # Errors
///
/// Returns the source and diagnostic for invalid macro definitions or expansions.
///
/// # Panics
///
/// Panics if the module graph does not contain its root module.
#[allow(
    clippy::result_large_err,
    reason = "cold diagnostics retain the parser error without a separate allocation"
)]
pub fn expand_program(
    program: &mut Program,
    sources: BTreeMap<PathBuf, String>,
) -> Result<(), (PathBuf, ParseError)> {
    let Some(graph) = program.module_graph.as_mut() else {
        return Ok(());
    };
    let registry = Rc::new(RefCell::new(registry::Registry::new(graph, sources)));
    registry
        .borrow_mut()
        .collect(graph)
        .map_err(|(path, span, message)| {
            (
                path,
                ParseError {
                    message,
                    span,
                    hint: None,
                    severity: Severity::Error,
                    kind: ParseDiagnosticKind::Other,
                },
            )
        })?;
    let mut cache = BTreeMap::new();
    let mut replacements = BTreeMap::new();
    for module in graph.modules.values() {
        let mut items = Vec::new();
        let mut item_sources = Vec::new();
        for (index, item) in module.items.iter().enumerate() {
            let Some(source) = graph
                .item_source(&module.id, index)
                .or_else(|| module.source_paths.first())
            else {
                items.push(item.clone());
                continue;
            };
            let expanded = expand_item(item, source, &registry, &mut cache)?;
            item_sources.extend(std::iter::repeat_n(source.clone(), expanded.len()));
            items.extend(expanded);
        }
        replacements.insert(module.id.clone(), (items, item_sources));
    }
    for (id, (items, item_sources)) in replacements {
        graph.modules.get_mut(&id).expect("known module").items = items;
        graph.item_sources.insert(id.dotted(), item_sources);
        for import in &mut graph.modules.get_mut(&id).expect("known module").imports {
            registry
                .borrow()
                .strip_macro_names(&import.target, &mut import.spec);
        }
    }
    program.items.clone_from(&graph.modules[&graph.root].items);
    program.expansions = std::mem::take(&mut registry.borrow_mut().map);
    if program.expansions.expansions.is_empty() {
        program.expansions.sources.clear();
    }
    Ok(())
}

type ExpandedItems = BTreeMap<(PathBuf, usize, usize), Vec<Spanned<Item>>>;

#[allow(
    clippy::result_large_err,
    reason = "cold diagnostics use the same parser error as expand_program"
)]
fn expand_item(
    item: &Spanned<Item>,
    source: &Path,
    registry: &Rc<RefCell<registry::Registry>>,
    cache: &mut ExpandedItems,
) -> Result<Vec<Spanned<Item>>, (PathBuf, ParseError)> {
    let key = (source.to_path_buf(), item.1.start, item.1.end);
    if let Some(expanded) = cache.get(&key) {
        return Ok(expanded.clone());
    }
    let expanded = match &item.0 {
        Item::Macro(_) => Vec::new(),
        Item::Import(import) => {
            let mut import = import.clone();
            registry.borrow().strip_import(&mut import, source, &item.1);
            if let Some(resolved) = &import.resolved_items {
                let mut items = Vec::new();
                let mut sources = Vec::new();
                for (index, imported) in resolved.iter().enumerate() {
                    let path = import
                        .resolved_item_source_paths
                        .get(index)
                        .or_else(|| import.resolved_source_paths.first())
                        .map_or(source, PathBuf::as_path);
                    let expanded = expand_item(imported, path, registry, cache)?;
                    sources.extend(std::iter::repeat_n(path.to_path_buf(), expanded.len()));
                    items.extend(expanded);
                }
                import.resolved_items = Some(Arc::new(items));
                import.resolved_item_source_paths = sources;
            }
            vec![(Item::Import(import), item.1.clone())]
        }
        _ => {
            let text = registry
                .borrow()
                .map
                .sources
                .get(source)
                .and_then(|text| text.get(item.1.clone()))
                .map(str::to_owned);
            if let Some(text) = text.filter(|text| text.contains('!') || text.contains("#[")) {
                let before = registry.borrow().map.expansions.len();
                let expander: SharedExpander = registry.clone();
                let result =
                    Parser::with_contexts(&text, item.1.start, Arc::new(TokenContexts::default()))
                        .with_expander(expander, source.to_path_buf())
                        .finish_expanded();
                if let Some(error) = result
                    .errors
                    .into_iter()
                    .find(|error| error.severity == Severity::Error)
                {
                    let mut error = error;
                    let path = if let Some((path, span)) =
                        registry.borrow().map.source_origin(&error.span)
                    {
                        error.span = span;
                        path
                    } else {
                        source.to_path_buf()
                    };
                    return Err((path, error));
                }
                if registry.borrow().map.expansions.len() == before {
                    let unchanged = vec![item.clone()];
                    cache.insert(key, unchanged.clone());
                    return Ok(unchanged);
                }
                let mut items = result.program.items;
                if let [expanded] = items.as_mut_slice() {
                    preserve_doc_comment(&item.0, &mut expanded.0);
                }
                let registry = registry.borrow();
                let mut control = control::Control::new(&registry.map);
                for (item, span) in &mut items {
                    if let Err(error) = control.item(item, span) {
                        let (path, span) = registry
                            .map
                            .source_origin(&error.span)
                            .unwrap_or_else(|| (source.to_path_buf(), error.span));
                        return Err((
                            path,
                            ParseError {
                                message: error.message,
                                span,
                                hint: None,
                                severity: Severity::Error,
                                kind: ParseDiagnosticKind::Other,
                            },
                        ));
                    }
                }
                items
            } else {
                vec![item.clone()]
            }
        }
    };
    cache.insert(key, expanded.clone());
    Ok(expanded)
}

fn preserve_doc_comment(original: &Item, expanded: &mut Item) {
    match (original, expanded) {
        (Item::Function(a), Item::Function(b)) => b.doc_comment.clone_from(&a.doc_comment),
        (Item::Const(a), Item::Const(b)) => b.doc_comment.clone_from(&a.doc_comment),
        (Item::TypeDecl(a), Item::TypeDecl(b)) => b.doc_comment.clone_from(&a.doc_comment),
        (Item::TypeAlias(a), Item::TypeAlias(b)) => b.doc_comment.clone_from(&a.doc_comment),
        (Item::Record(a), Item::Record(b)) => b.doc_comment.clone_from(&a.doc_comment),
        (Item::Trait(a), Item::Trait(b)) => b.doc_comment.clone_from(&a.doc_comment),
        (Item::Actor(a), Item::Actor(b)) => b.doc_comment.clone_from(&a.doc_comment),
        _ => {}
    }
}
