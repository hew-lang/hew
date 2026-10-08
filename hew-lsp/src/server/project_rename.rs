//! Atomic project edits for exported top-level functions.
//!
//! Each frontend run mints its own IDs. Only the physical source and exact
//! declaration occurrence cross that boundary; rendered names never identify
//! references. The proposed overlay is checked again to detect binding capture.

use std::collections::{BTreeMap, HashMap};
use std::path::{Path, PathBuf};

use hew_analysis::{OffsetSpan, RenameEdit, RenameError};
use hew_parser::ast::{ImportDecl, ImportSpec, Item};
use hew_types::check::scope::Resolution;
use hew_types::{DeclarationKind, TypeCheckOutput};
use tower_lsp_server::ls_types::{
    DocumentChanges, OneOf, OptionalVersionedTextDocumentIdentifier, Position,
    PrepareRenameResponse, TextDocumentEdit, Uri, WorkspaceEdit,
};

use super::navigation::find_named_import_spans;
use super::uri::FileUriExt;
use super::{offset_range_to_lsp, DocumentState, HewLanguageServer, OpenDocument};

#[derive(Debug, Clone, PartialEq, Eq)]
struct Function {
    source: PathBuf,
    item: std::ops::Range<usize>,
    ordinal: u32,
    name: String,
}

fn physical_path(path: &Path) -> PathBuf {
    resolved_physical_path(path).unwrap_or_else(|_| path.to_path_buf())
}

/// Resolve existing parents even when an editor has opened a new unsaved file.
/// Appending an unresolved leaf to a lexical symlink path would misstate its
/// workspace ownership. Resolve components before interpreting a later `..`.
fn resolved_physical_path(path: &Path) -> std::io::Result<PathBuf> {
    use std::path::Component;
    if let Ok(path) = std::fs::canonicalize(path) {
        return Ok(path);
    }
    let absolute = if path.is_absolute() {
        path.to_path_buf()
    } else {
        std::env::current_dir()?.join(path)
    };
    let mut resolved = PathBuf::new();
    for component in absolute.components() {
        match component {
            Component::CurDir => {}
            Component::ParentDir => {
                resolved.pop();
            }
            Component::Prefix(_) | Component::RootDir => resolved.push(component.as_os_str()),
            Component::Normal(_) => {
                resolved.push(component.as_os_str());
                match std::fs::canonicalize(&resolved) {
                    Ok(path) => resolved = path,
                    Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
                        if std::fs::symlink_metadata(&resolved)
                            .is_ok_and(|metadata| metadata.file_type().is_symlink())
                        {
                            return Err(error);
                        }
                    }
                    Err(error) => return Err(error),
                }
            }
        }
    }
    Ok(resolved)
}

fn function(output: &TypeCheckOutput, resolution: Resolution) -> Option<Function> {
    let (Resolution::Def(id) | Resolution::Member(id)) = resolution else {
        return None;
    };
    if output.defs.kind(id) != DeclarationKind::Function || !output.defs.visibility(id).is_pub() {
        return None;
    }
    let target = hew_analysis::identity::declaration_target(output, resolution)?;
    Some(Function {
        source: physical_path(target.source.as_deref()?),
        item: target.occurrence.span(),
        ordinal: target.occurrence.ordinal(),
        name: target.name,
    })
}

/// Declaration tokens are not expressions and need not have a resolution.
fn selected_function(uri: &Uri, checked: &Checked, offset: usize) -> Option<Function> {
    let doc = &checked.doc;
    let output = doc.type_output.as_ref()?;
    let (_, word) = hew_analysis::util::simple_word_at_offset(&doc.source, offset)?;
    if let Some((span, resolution)) = resolution_for_token(output, word) {
        if let Some(target) = checked.function(resolution) {
            // Renaming a named import's visible alias remains a local operation.
            let span = reference_leaf(checked, span)?;
            if span.start <= offset
                && offset <= span.end
                && doc.source.get(span.start..span.end) == Some(target.name.as_str())
            {
                return Some(target.clone());
            }
        }
    }
    for (_, id) in output.defs.declarations() {
        let resolution = Resolution::Def(id);
        let Some(target) = checked.function(resolution) else {
            continue;
        };
        let query_path = resolved_physical_path(&uri.to_checked_file_path()?).ok()?;
        if query_path != target.source {
            continue;
        }
        let declaration = hew_analysis::identity::declaration_target(output, resolution)?;
        let Some(span) = hew_analysis::identity::declaration_name_span(
            &doc.source,
            &doc.parse_result,
            &declaration,
        ) else {
            continue;
        };
        if span.start <= offset && offset <= span.end {
            return Some(target.clone());
        }
    }
    None
}

/// Resolve the complete authored token, preferring its exact checker fact.
/// Prelude signature facts can share root index 0 and overlap only part of a
/// user's token. A containing offset alone must not select those foreign facts.
/// Exact local/field facts still win over a surrounding function callee, so
/// same-typed binding capture remains a refusal.
fn resolution_for_token(
    output: &TypeCheckOutput,
    token: OffsetSpan,
) -> Option<(OffsetSpan, Resolution)> {
    let exact = hew_types::check::SpanKey {
        start: token.start,
        end: token.end,
        module_idx: 0,
    };
    if let Some(resolution) = output.resolutions.get(&exact) {
        return Some((token, *resolution));
    }
    output
        .resolutions
        .iter()
        .filter(|(key, _)| key.module_idx == 0 && key.start <= token.start && token.end <= key.end)
        .min_by_key(|(key, _)| (key.end - key.start, key.start, key.end))
        .map(|(key, resolution)| {
            (
                OffsetSpan {
                    start: key.start,
                    end: key.end,
                },
                *resolution,
            )
        })
}

#[derive(Debug, Clone)]
struct Source {
    uri: Uri,
    path: PathBuf,
    text: String,
}

fn refused(path: &Path, message: impl Into<String>) -> RenameError {
    RenameError::Io {
        path: path.display().to_string(),
        message: message.into(),
    }
}

struct Checked {
    doc: DocumentState,
    imports: Vec<(ImportDecl, hew_parser::ast::Span)>,
    imports_complete: bool,
    functions: HashMap<hew_types::DefId, Function>,
    tokens: Vec<(TokenRole, OffsetSpan)>,
    dependencies: Vec<PathBuf>,
    failure: Option<String>,
    resolved: bool,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum TokenRole {
    Identifier,
    Dot,
    Other,
}

impl Checked {
    fn function(&self, resolution: Resolution) -> Option<&Function> {
        let (Resolution::Def(id) | Resolution::Member(id)) = resolution else {
            return None;
        };
        self.functions.get(&id)
    }
}

/// Recover authored imports through their physical graph provenance.
fn checked_imports(
    source: &Source,
    program: &hew_parser::ast::Program,
    parse_result: &hew_parser::ParseResult,
) -> (Vec<(ImportDecl, hew_parser::ast::Span)>, bool) {
    let raw_imports = super::navigation::collect_import_items(parse_result);
    let imports: Vec<_> = raw_imports
        .iter()
        .filter_map(|(raw, span)| {
            let matching = |item: &Item, resolved_span: &hew_parser::ast::Span| {
                if let Item::Import(import) = item {
                    if resolved_span == span
                        && import.path == raw.path
                        && import.file_path == raw.file_path
                    {
                        return Some((import.clone(), span.clone()));
                    }
                }
                None
            };
            program
                .module_graph
                .as_ref()
                .and_then(|graph| {
                    graph.modules.iter().find_map(|(id, module)| {
                        module.items.iter().enumerate().find_map(
                            |(index, (item, resolved_span))| {
                                let item_source = graph
                                    .item_source(id, index)
                                    .or_else(|| module.source_paths.first())?;
                                (physical_path(item_source) == source.path)
                                    .then(|| matching(item, resolved_span))
                                    .flatten()
                            },
                        )
                    })
                })
                .or_else(|| {
                    program
                        .items
                        .iter()
                        .find_map(|(item, resolved_span)| matching(item, resolved_span))
                })
        })
        .collect();
    let imports_complete = imports.len() == raw_imports.len();
    (imports, imports_complete)
}

/// Stop at the shared frontend's checker, without HIR/SIR diagnostic lowering.
fn check(source: &Source, overlay: &hew_compile::DocumentSet, pkg_paths: &[PathBuf]) -> Checked {
    let options = hew_compile::FrontendOptions {
        documents: overlay.clone(),
        pkg_path: pkg_paths.first().cloned(),
        ..Default::default()
    };
    let mut state = hew_compile::run_source_frontend(
        &source.text,
        &source.path.display().to_string(),
        &options,
    );
    let dependencies = state
        .program
        .module_graph
        .as_ref()
        .map_or_else(Vec::new, |graph| {
            graph
                .modules
                .values()
                .flat_map(|module| &module.source_paths)
                .map(|path| physical_path(path))
                .collect()
        });
    let (imports, imports_complete) = checked_imports(
        source,
        &state.program,
        state
            .parse_result
            .as_ref()
            .expect("source frontend parses its root"),
    );
    let resolved = state.typecheck_result.is_some();
    let mut output = state.typecheck_result.take().and_then(|result| result.tco);
    let index = state
        .program
        .module_graph
        .as_ref()
        .and_then(|graph| graph.file_span_indices().path_index(&source.path));
    if let (Some(output), Some(index)) = (output.as_mut(), index) {
        hew_analysis::identity::focus_file(output, index);
    }
    let functions = output.as_ref().map_or_else(HashMap::new, |output| {
        output
            .defs
            .declarations()
            .filter_map(|(_, id)| {
                function(output, Resolution::Def(id)).map(|function| (id, function))
            })
            .collect()
    });
    let tokens = hew_lexer::lex(&source.text)
        .into_iter()
        .filter_map(|(token, span)| {
            if matches!(token, hew_lexer::Token::DocComment(_)) {
                return None;
            }
            let role = match token {
                hew_lexer::Token::Identifier(_) => TokenRole::Identifier,
                hew_lexer::Token::Dot => TokenRole::Dot,
                _ => TokenRole::Other,
            };
            Some((
                role,
                OffsetSpan {
                    start: span.start,
                    end: span.end,
                },
            ))
        })
        .collect();
    Checked {
        failure: state.stopped.map(|failure| failure.message),
        resolved,
        dependencies,
        imports,
        imports_complete,
        functions,
        tokens,
        doc: DocumentState {
            source: source.text.clone(),
            line_offsets: hew_analysis::util::compute_line_offsets(&source.text),
            parse_result: state.parse_result.expect("source frontend parses its root"),
            type_output: output,
            dependency_uris: None,
            diagnostics_by_uri: HashMap::new(),
        },
    }
}

fn imported_function(checked: &Checked, import: &ImportDecl, name: &str) -> Option<Function> {
    checked.functions.values().find_map(|target| {
        (target.name == name
            && import
                .resolved_source_paths
                .iter()
                .any(|path| physical_path(path) == target.source))
        .then(|| target.clone())
    })
}

fn selected_import_function(checked: &Checked, offset: usize) -> Option<Function> {
    for (import, span) in &checked.imports {
        let Some(ImportSpec::Names(names)) = &import.spec else {
            continue;
        };
        for name in names {
            let Some((imported_span, _)) = find_named_import_spans(&checked.doc.source, span, name)
            else {
                continue;
            };
            if imported_span.start <= offset && offset <= imported_span.end {
                return imported_function(checked, import, name.name.name.as_str());
            }
        }
    }
    None
}

fn within(path: &Path, roots: &[PathBuf]) -> bool {
    roots.iter().any(|root| path.starts_with(root))
}

fn editable(path: &Path, roots: &[PathBuf]) -> bool {
    roots.iter().any(|root| {
        path.strip_prefix(root).ok().is_some_and(|relative| {
            !relative.components().any(|component| {
                super::workspace::should_skip_workspace_dir(Path::new(component.as_os_str()))
            })
        })
    })
}

fn source_snapshot(
    documents: &HashMap<Uri, OpenDocument>,
) -> Result<(BTreeMap<PathBuf, Source>, hew_compile::DocumentSet), RenameError> {
    let mut sources = BTreeMap::new();
    let mut overlay = hew_compile::DocumentSet::new();
    let mut entries: Vec<_> = documents.iter().collect();
    entries.sort_by_key(|(uri, _)| uri.as_str());
    for (uri, document) in entries {
        if let Some(path) = uri.to_checked_file_path() {
            let physical = resolved_physical_path(&path)
                .map_err(|error| RenameError::from((path.clone().into_owned(), error)))?;
            overlay.insert(path.into_owned(), document.source.clone());
            let path = physical;
            overlay.insert(path.clone(), document.source.clone());
            if sources.contains_key(&path) {
                return Err(refused(
                    &path,
                    "multiple open URIs name the same source; close duplicate views before renaming",
                ));
            }
            sources.insert(
                path.clone(),
                Source {
                    uri: uri.clone(),
                    path,
                    text: document.source.clone(),
                },
            );
        }
    }
    Ok((sources, overlay))
}

fn project_sources(
    roots: &[PathBuf],
    open: &BTreeMap<PathBuf, Source>,
    overlay: &mut hew_compile::DocumentSet,
) -> Result<BTreeMap<PathBuf, Source>, RenameError> {
    let mut sources: BTreeMap<_, _> = open
        .iter()
        .filter(|(path, _)| editable(path, roots))
        .map(|(path, source)| (path.clone(), source.clone()))
        .collect();
    for root in roots {
        super::workspace::for_each_hew_file(root, |path| -> Result<(), RenameError> {
            let path = physical_path(path);
            if sources.contains_key(&path) {
                return Ok(());
            }
            let text = std::fs::read_to_string(&path)
                .map_err(|error| RenameError::from((path.clone(), error)))?;
            let uri = Uri::from_checked_file_path(&path)
                .ok_or_else(|| refused(&path, "source path has no file URI"))?;
            overlay.insert(path.clone(), text.clone());
            sources.insert(path.clone(), Source { uri, path, text });
            Ok(())
        })?;
    }
    Ok(sources)
}

#[derive(Default)]
struct Occurrences {
    edits: Vec<RenameEdit>,
    /// Includes alias calls whose spelling is intentionally unchanged.
    references: Vec<OffsetSpan>,
}

/// Syntax only distinguishes the import binding role. The checker still owns
/// the declaration identity; a qualified member never renames an alias.
fn alias_at(checked: &Checked, target: &Function, span: OffsetSpan) -> bool {
    let token_index = checked
        .tokens
        .partition_point(|(_, token)| token.start < span.start);
    let qualified = token_index > 0 && checked.tokens[token_index - 1].0 == TokenRole::Dot;
    if qualified {
        return false;
    }
    checked.imports.iter().any(|(import, item_span)| {
        let Some(ImportSpec::Names(names)) = &import.spec else {
            return false;
        };
        names.iter().any(|name| {
            let Some(alias) = name.alias else {
                return false;
            };
            if imported_function(checked, import, name.name.name.as_str()).as_ref() != Some(target)
            {
                return false;
            }
            let Some((original, visible)) =
                find_named_import_spans(&checked.doc.source, item_span, name)
            else {
                return false;
            };
            if span == original {
                return false;
            }
            span == visible
                || (span.start >= item_span.end
                    && checked.doc.source.get(span.start..span.end) == Some(alias.name.as_str()))
        })
    })
}

/// The checker may publish the complete callee path as well as its member.
/// Keep its checked identity, but edit only the authored identifier at the end
/// of that path. Generic arguments follow the path and are separate tokens.
fn reference_leaf(checked: &Checked, span: OffsetSpan) -> Option<OffsetSpan> {
    let start = checked
        .tokens
        .partition_point(|(_, token)| token.end <= span.start);
    let mut tokens = checked.tokens[start..]
        .iter()
        .take_while(|(_, token)| token.start < span.end);
    let (role, first) = tokens.next()?;
    if *role != TokenRole::Identifier || first.start < span.start || first.end > span.end {
        return None;
    }
    let mut leaf = *first;
    while let Some((role, _)) = tokens.next() {
        if *role != TokenRole::Dot {
            break;
        }
        let (role, token) = tokens.next()?;
        if *role != TokenRole::Identifier || token.end > span.end {
            return None;
        }
        leaf = *token;
    }
    Some(leaf)
}

fn occurrences(checked: &Checked, target: &Function) -> Result<Occurrences, RenameError> {
    let mut found = Occurrences::default();
    let output = checked.doc.type_output.as_ref().ok_or_else(|| {
        refused(
            &target.source,
            "checker did not publish declaration identities",
        )
    })?;
    for (key, resolution) in &output.resolutions {
        if key.module_idx != 0 || checked.function(*resolution) != Some(target) {
            continue;
        }
        let raw = OffsetSpan {
            start: key.start,
            end: key.end,
        };
        let span = reference_leaf(checked, raw).ok_or_else(|| {
            refused(
                &target.source,
                "could not locate the authored token for a checked function reference",
            )
        })?;
        found.references.push(span);
        if checked.doc.source.get(span.start..span.end) == Some(target.name.as_str())
            && !alias_at(checked, target, span)
        {
            found.edits.push(RenameEdit {
                span,
                new_text: String::new(),
            });
        }
    }
    found.references.sort_by_key(|span| (span.start, span.end));
    found.references.dedup();
    for (import, span) in &checked.imports {
        let Some(ImportSpec::Names(names)) = &import.spec else {
            continue;
        };
        for name in names {
            if imported_function(checked, import, name.name.name.as_str()).as_ref() != Some(target)
            {
                continue;
            }
            let (span, _) =
                find_named_import_spans(&checked.doc.source, span, name).ok_or_else(|| {
                    refused(
                        &target.source,
                        "could not locate an imported function token",
                    )
                })?;
            found.edits.push(RenameEdit {
                span,
                new_text: String::new(),
            });
        }
    }
    Ok(found)
}

fn declaration_span(checked: &Checked, target: &Function) -> Option<OffsetSpan> {
    let output = checked.doc.type_output.as_ref()?;
    output.defs.declarations().find_map(|(_, id)| {
        let resolution = Resolution::Def(id);
        if function(output, resolution).as_ref() != Some(target) {
            return None;
        }
        hew_analysis::identity::declaration_name_span(
            &checked.doc.source,
            &checked.doc.parse_result,
            &hew_analysis::identity::declaration_target(output, resolution)?,
        )
    })
}

fn apply(source: &str, edits: &[RenameEdit]) -> String {
    let mut changed = source.to_string();
    for edit in edits.iter().rev() {
        changed.replace_range(edit.span.start..edit.span.end, &edit.new_text);
    }
    changed
}

fn translated(span: OffsetSpan, edits: &[RenameEdit]) -> OffsetSpan {
    let mut start = span.start;
    let mut end = span.end;
    for edit in edits {
        if edit.span.end <= span.start {
            start = start + edit.new_text.len() - (edit.span.end - edit.span.start);
            end = end + edit.new_text.len() - (edit.span.end - edit.span.start);
        } else if edit.span == span {
            end = start + edit.new_text.len();
        }
    }
    OffsetSpan { start, end }
}

fn selected(checked: &Checked, uri: &Uri, offset: usize) -> Option<Function> {
    let target = selected_import_function(checked, offset)
        .or_else(|| selected_function(uri, checked, offset))?;
    let (_, span) = hew_analysis::util::simple_word_at_offset(&checked.doc.source, offset)?;
    (!alias_at(checked, &target, span)).then_some(target)
}

fn query_source(snapshot: &HashMap<Uri, OpenDocument>, uri: &Uri) -> Option<Source> {
    let document = snapshot.get(uri)?;
    Some(Source {
        uri: uri.clone(),
        path: uri.to_checked_file_path().map_or_else(
            || PathBuf::from("./untitled.hew"),
            |path| physical_path(&path),
        ),
        text: document.source.clone(),
    })
}

/// Legacy non-function targets still use their existing syntax queries, but
/// their peers must have the same fresh authored text as the project planner.
fn legacy_documents(snapshot: &HashMap<Uri, OpenDocument>) -> dashmap::DashMap<Uri, DocumentState> {
    snapshot
        .iter()
        .map(|(uri, document)| {
            (
                uri.clone(),
                DocumentState {
                    source: document.source.clone(),
                    line_offsets: hew_analysis::util::compute_line_offsets(&document.source),
                    parse_result: hew_parser::parse(&document.source),
                    type_output: None,
                    dependency_uris: None,
                    diagnostics_by_uri: HashMap::new(),
                },
            )
        })
        .collect()
}

/// A syntactic local or parameter has no external consumers. An incomplete
/// exported/imported target must never fall through to declaration-only edits.
fn potential_external_target(checked: &Checked, offset: usize) -> bool {
    let Some((name, word)) = hew_analysis::util::simple_word_at_offset(&checked.doc.source, offset)
    else {
        return false;
    };
    // An explicit alias is its own local binding, including a self-alias whose
    // spelling happens to equal the imported function's name.
    if checked.imports.iter().any(|(import, item_span)| {
        let Some(ImportSpec::Names(names)) = &import.spec else {
            return false;
        };
        names.iter().any(|name| {
            name.alias.is_some()
                && find_named_import_spans(&checked.doc.source, item_span, name)
                    .is_some_and(|(_, visible)| visible.start <= offset && offset <= visible.end)
        })
    }) {
        return false;
    }
    if let Some(output) = &checked.doc.type_output {
        if resolution_for_token(output, word)
            .and_then(|(span, resolution)| {
                checked.function(resolution).and_then(|target| {
                    reference_leaf(checked, span).map(|leaf| alias_at(checked, target, leaf))
                })
            })
            .unwrap_or(false)
        {
            return false;
        }
    }
    if hew_analysis::definition::find_local_binding_definition(
        &checked.doc.source,
        &checked.doc.parse_result,
        &name,
        offset,
    )
    .is_some()
        || hew_analysis::definition::find_param_definition(&checked.doc.parse_result, &name, offset)
            .is_some()
    {
        return false;
    }
    if hew_analysis::rename::is_local_non_function_reference(
        &checked.doc.source,
        &checked.doc.parse_result,
        offset,
    ) {
        return false;
    }
    // A parsed non-function declaration remains eligible for the legacy
    // syntactic rename even when an unrelated import could not be resolved.
    if let Some(span) = hew_analysis::definition::find_definition(
        &checked.doc.source,
        &checked.doc.parse_result,
        &name,
    ) {
        if span.start <= offset
            && offset <= span.end
            && checked
                .doc
                .parse_result
                .program
                .items
                .iter()
                .any(|(item, item_span)| {
                    item_span.start <= span.start
                        && span.end <= item_span.end
                        && !matches!(item, Item::Function(function) if function.visibility.is_pub())
                })
        {
            return false;
        }
    }
    checked
        .doc
        .parse_result
        .program
        .items
        .iter()
        .any(|(item, _)| match item {
            Item::Function(function) => {
                function.visibility.is_pub() && function.name.name.as_str() == name
            }
            Item::Import(_) => {
                !checked.resolved
                    || checked.doc.type_output.is_none()
                    || checked.failure.is_some()
                    || !checked.imports_complete
            }
            _ => false,
        })
}

/// An exported declaration cannot fall back to a syntax-only edit if checking
/// failed to identify it. The parser owns its exact name-token span.
fn exported_declaration_at(checked: &Checked, offset: usize) -> bool {
    checked
        .doc
        .parse_result
        .program
        .items
        .iter()
        .any(|(item, _)| {
            let Item::Function(function) = item else {
                return false;
            };
            function.visibility.is_pub()
                && function.decl_span.start <= offset
                && offset <= function.decl_span.end
        })
}

pub(super) fn prepare(
    server: &HewLanguageServer,
    uri: &Uri,
    position: Position,
) -> Option<PrepareRenameResponse> {
    let snapshot = server.open_documents.read().ok()?.clone();
    let (_, mut overlay) = source_snapshot(&snapshot).ok()?;
    let query = query_source(&snapshot, uri)?;
    overlay.insert(query.path.clone(), query.text.clone());
    let checked = check(&query, &overlay, &server.extra_pkg_paths);
    let offset = super::position_to_offset(&query.text, &checked.doc.line_offsets, position);
    if selected(&checked, uri, offset).is_some() {
        uri.to_checked_file_path()?;
        return (|| {
            let (_, span) = hew_analysis::util::simple_word_at_offset(&query.text, offset)?;
            Some(PrepareRenameResponse::Range(offset_range_to_lsp(
                &query.text,
                &checked.doc.line_offsets,
                span.start,
                span.end,
            )))
        })();
    }
    if (!checked.resolved
        || checked.doc.type_output.is_none()
        || !checked.imports_complete
        || checked.failure.is_some()
        || uri.to_checked_file_path().is_none())
        && potential_external_target(&checked, offset)
    {
        return None;
    }
    if exported_declaration_at(&checked, offset) {
        return None;
    }
    super::navigation::build_prepare_rename_response(
        uri,
        &checked.doc,
        offset,
        &legacy_documents(&snapshot),
    )
}

pub(super) fn rename(
    server: &HewLanguageServer,
    uri: &Uri,
    position: Position,
    new_name: &str,
) -> Result<Option<WorkspaceEdit>, RenameError> {
    let snapshot = server
        .open_documents
        .read()
        .map_err(|_| {
            refused(
                Path::new("workspace"),
                "open source snapshot is unavailable",
            )
        })?
        .clone();
    let (mut open, mut overlay) = source_snapshot(&snapshot)?;
    let Some(query) = query_source(&snapshot, uri) else {
        return Ok(None);
    };
    let path = query.path.clone();
    overlay.insert(path.clone(), query.text.clone());
    if uri.to_checked_file_path().is_some() {
        open.insert(path.clone(), query.clone());
    }
    let checked = check(&query, &overlay, &server.extra_pkg_paths);
    let offset = super::position_to_offset(&query.text, &checked.doc.line_offsets, position);
    if (!checked.resolved
        || checked.doc.type_output.is_none()
        || !checked.imports_complete
        || checked.failure.is_some()
        || uri.to_checked_file_path().is_none())
        && potential_external_target(&checked, offset)
    {
        return Err(refused(
            &path,
            format!(
                "cannot prove rename target identity: {}",
                checked.failure.as_deref().unwrap_or("incomplete checking")
            ),
        ));
    }
    let mut edit = if let Some(target) = selected(&checked, uri, offset) {
        let roots = server
            .workspace_roots
            .read()
            .map_err(|_| refused(&path, "workspace roots are unavailable"))?
            .clone();
        plan(
            &query,
            &target,
            new_name,
            &open,
            overlay,
            &roots,
            &server.extra_pkg_paths,
        )?
    } else {
        if exported_declaration_at(&checked, offset) {
            return Err(refused(
                &path,
                "cannot prove exported function declaration identity",
            ));
        }
        super::navigation::plan_workspace_rename(
            uri,
            &checked.doc,
            offset,
            new_name,
            &legacy_documents(&snapshot),
        )?
    };
    let current = server
        .open_documents
        .read()
        .map_err(|_| refused(&path, "open source snapshot is unavailable"))?;
    if *current != snapshot {
        return Err(refused(
            &path,
            "open documents changed during rename; retry the request",
        ));
    }
    let document_changes = *server
        .rename_document_changes
        .read()
        .map_err(|_| refused(&path, "client edit capabilities are unavailable"))?;
    if document_changes {
        if let Some(edit) = edit.as_mut() {
            version_document_edits(edit, &current);
        }
    }
    Ok(edit)
}

/// Preserve client versions while ordering the returned document edits.
fn version_document_edits(edit: &mut WorkspaceEdit, open: &HashMap<Uri, OpenDocument>) {
    let Some(changes) = edit.changes.take() else {
        return;
    };
    let mut changes: Vec<_> = changes.into_iter().collect();
    changes.sort_by(|(left, _), (right, _)| left.as_str().cmp(right.as_str()));
    edit.document_changes = Some(DocumentChanges::Edits(
        changes
            .into_iter()
            .map(|(uri, edits)| {
                let version = open.get(&uri).map(|document| document.version);
                TextDocumentEdit {
                    text_document: OptionalVersionedTextDocumentIdentifier { uri, version },
                    edits: edits.into_iter().map(OneOf::Left).collect(),
                }
            })
            .collect(),
    ));
}

#[expect(
    clippy::too_many_lines,
    reason = "atomic discover, plan and verify phases remain one transaction"
)]
fn plan(
    query: &Source,
    target: &Function,
    new_name: &str,
    open: &BTreeMap<PathBuf, Source>,
    mut overlay: hew_compile::DocumentSet,
    workspace_roots: &[PathBuf],
    pkg_paths: &[PathBuf],
) -> Result<Option<WorkspaceEdit>, RenameError> {
    hew_analysis::rename::validate_new_name(new_name)?;
    if target.name == new_name {
        return Ok(None);
    }
    let mut roots: Vec<_> = workspace_roots
        .iter()
        .map(|path| physical_path(path))
        .collect();
    if roots.is_empty() {
        if let Some(root) = super::workspace::find_workspace_root_for_uri(&query.uri) {
            roots.push(physical_path(&root));
        }
    }
    if !within(&query.path, &roots) || !within(&target.source, &roots) {
        return Err(refused(&target.source, "exported function rename requires its declaration and request inside configured workspace roots"));
    }
    let sources = project_sources(&roots, open, &mut overlay)?;
    let mut changes: BTreeMap<PathBuf, Occurrences> = BTreeMap::new();
    let mut definition = None;
    for (path, source) in &sources {
        let checked = check(source, &overlay, pkg_paths);
        if !checked.resolved {
            return Err(refused(
                path,
                format!(
                    "cannot prove rename completeness: {}",
                    checked
                        .failure
                        .as_deref()
                        .unwrap_or("incomplete import resolution")
                ),
            ));
        }
        let relevant = path == &target.source
            || checked
                .dependencies
                .iter()
                .any(|path| path == &target.source);
        if !relevant {
            continue;
        }
        if let Some(failure) = &checked.failure {
            return Err(refused(
                path,
                format!("cannot prove rename completeness in a function consumer: {failure}"),
            ));
        }
        if !checked.imports_complete {
            return Err(refused(path, "cannot prove rename completeness: resolved import source provenance is unavailable"));
        }
        let mut found = occurrences(&checked, target)?;
        if path == &target.source {
            let span = declaration_span(&checked, target).ok_or_else(|| {
                refused(
                    path,
                    "selected declaration no longer matches checked source",
                )
            })?;
            found.edits.push(RenameEdit {
                span,
                new_text: String::new(),
            });
            definition = Some(span);
        }
        for edit in &mut found.edits {
            edit.new_text = new_name.to_string();
        }
        super::navigation::sort_and_dedup_rename_edits(&mut found.edits);
        if !found.edits.is_empty() || !found.references.is_empty() {
            changes.insert(path.clone(), found);
        }
    }
    let definition = definition.ok_or_else(|| {
        refused(
            &target.source,
            "function declaration is outside the editable project",
        )
    })?;
    let mut proposed = sources.clone();
    for (path, found) in &changes {
        let source = proposed
            .get_mut(path)
            .expect("edits belong to discovered sources");
        source.text = apply(&source.text, &found.edits);
        // Missing leaves reached through a symlink cannot canonicalize in the
        // resolver. Keep its original URI lookup coherent with the physical key.
        if let Some(original) = source.uri.to_checked_file_path() {
            overlay.insert(original.into_owned(), source.text.clone());
        }
        overlay.insert(path.clone(), source.text.clone());
    }
    let new_definition = translated(definition, &changes[&target.source].edits);
    let defining_source = &proposed[&target.source];
    let checked_definition = check(defining_source, &overlay, pkg_paths);
    if let Some(failure) = &checked_definition.failure {
        return Err(refused(
            &target.source,
            format!("rename would introduce a conflict: {failure}"),
        ));
    }
    let new_target = selected_function(
        &defining_source.uri,
        &checked_definition,
        new_definition.start,
    )
    .ok_or_else(|| {
        refused(
            &target.source,
            "renamed function has no checked declaration identity",
        )
    })?;
    verify_references(
        &target.source,
        &checked_definition,
        &changes[&target.source],
        &new_target,
    )?;
    for (path, found) in &changes {
        if path == &target.source {
            continue;
        }
        let checked = check(&proposed[path], &overlay, pkg_paths);
        if let Some(failure) = &checked.failure {
            return Err(refused(
                path,
                format!("rename would introduce a conflict: {failure}"),
            ));
        }
        verify_references(path, &checked, found, &new_target)?;
    }
    // A changed or newly created closed consumer invalidates the proof too,
    // even when the first snapshot contained no references in that file.
    let current = project_sources(&roots, open, &mut hew_compile::DocumentSet::new())?;
    if current.len() != sources.len()
        || current.iter().any(|(path, source)| {
            sources
                .get(path)
                .is_none_or(|original| source.text != original.text)
        })
    {
        return Err(refused(
            &query.path,
            "project sources changed during rename; retry the request",
        ));
    }
    let mut lsp_changes = HashMap::new();
    for (path, found) in changes {
        if found.edits.is_empty() {
            continue;
        }
        let source = &sources[&path];
        let offsets = hew_analysis::util::compute_line_offsets(&source.text);
        let edits = found
            .edits
            .into_iter()
            .map(|edit| tower_lsp_server::ls_types::TextEdit {
                range: offset_range_to_lsp(&source.text, &offsets, edit.span.start, edit.span.end),
                new_text: edit.new_text,
            })
            .collect();
        lsp_changes.insert(source.uri.clone(), edits);
    }
    Ok(Some(WorkspaceEdit {
        changes: Some(lsp_changes),
        ..Default::default()
    }))
}

fn verify_references(
    path: &Path,
    checked: &Checked,
    found: &Occurrences,
    target: &Function,
) -> Result<(), RenameError> {
    let output = checked.doc.type_output.as_ref().ok_or_else(|| {
        refused(
            path,
            "renamed consumer has no checked declaration identities",
        )
    })?;
    for original in &found.references {
        let span = translated(*original, &found.edits);
        let resolved = resolution_for_token(output, span)
            .and_then(|(_, resolution)| checked.function(resolution));
        if resolved != Some(target) {
            return Err(refused(
                path,
                "rename would capture a function reference with another binding",
            ));
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    struct Project(PathBuf);

    impl Project {
        fn new(files: &[(&str, &str)]) -> Self {
            let unique = std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos();
            let root = std::env::temp_dir().join(format!(
                "hew-function-rename-{}-{unique}",
                std::process::id()
            ));
            for (name, text) in files {
                let path = root.join(name);
                std::fs::create_dir_all(path.parent().unwrap()).unwrap();
                std::fs::write(path, text).unwrap();
            }
            Self(root)
        }

        fn rename(&self, file: &str, new_name: &str) -> Result<Option<WorkspaceEdit>, RenameError> {
            let path = physical_path(&self.0.join(file));
            let source = Source {
                uri: Uri::from_checked_file_path(&path).unwrap(),
                text: std::fs::read_to_string(&path).unwrap(),
                path: path.clone(),
            };
            let mut overlay = hew_compile::DocumentSet::new();
            overlay.insert(path.clone(), source.text.clone());
            let checked = check(&source, &overlay, &[]);
            let offset = source.text.find("fn greet").unwrap() + 3;
            let target =
                selected(&checked, &source.uri, offset).expect("checked exported function");
            plan(
                &source,
                &target,
                new_name,
                &BTreeMap::from([(path, source.clone())]),
                overlay,
                std::slice::from_ref(&self.0),
                &[],
            )
        }
    }

    impl Drop for Project {
        fn drop(&mut self) {
            let _ = std::fs::remove_dir_all(&self.0);
        }
    }

    #[test]
    fn directory_peer_function_keeps_its_physical_file_identity() {
        let project = Project::new(&[
            (
                "greeting/greeting.hew",
                "pub fn other() -> string { \"other\" }\n",
            ),
            ("greeting/b.hew", "pub fn greet() -> string { \"hello\" }\n"),
            (
                "main.hew",
                "import greeting;\nfn main() { println(greeting.greet()); }\n",
            ),
        ]);
        let changes = project
            .rename("greeting/b.hew", "salute")
            .unwrap()
            .unwrap()
            .changes
            .unwrap();
        assert_eq!(changes.len(), 2);
        let peer = Uri::from_checked_file_path(project.0.join("greeting/b.hew")).unwrap();
        let main = Uri::from_checked_file_path(project.0.join("main.hew")).unwrap();
        assert_eq!(changes[&peer].len(), 1);
        assert_eq!(changes[&main].len(), 1);
    }

    #[test]
    fn same_typed_parameter_capture_is_refused() {
        let project = Project::new(&[
            ("greeting.hew", "pub fn greet() -> string { \"hello\" }\n"),
            ("main.hew", "import greeting.{ greet };\nfn helper(salute: fn() -> string) -> string { greet() }\n"),
        ]);
        let error = project.rename("greeting.hew", "salute").unwrap_err();
        assert!(
            matches!(&error, RenameError::Io { message, .. } if message.contains("capture")),
            "{error:?}"
        );
    }

    #[test]
    fn unrelated_type_error_is_not_a_function_consumer() {
        let project = Project::new(&[
            ("greeting.hew", "pub fn greet() -> string { \"hello\" }\n"),
            (
                "main.hew",
                "import greeting;\nfn main() { println(greeting.greet()); }\n",
            ),
            (
                "unrelated.hew",
                "fn broken() { let answer: i64 = \"wrong\"; }\n",
            ),
        ]);
        let changes = project
            .rename("greeting.hew", "salute")
            .unwrap()
            .unwrap()
            .changes
            .unwrap();
        assert_eq!(changes.len(), 2);
    }

    #[test]
    fn imported_original_is_not_an_earlier_entries_alias() {
        let project = Project::new(&[
            ("greeting.hew", "pub fn first() -> string { \"first\" }\npub fn greet() -> string { \"hello\" }\n"),
            ("main.hew", "import greeting.{ first as greet, /* preserve this alias */ greet as other };\nfn main() { println(greet()); println(other()); }\n"),
        ]);
        let changes = project
            .rename("greeting.hew", "salute")
            .unwrap()
            .unwrap()
            .changes
            .unwrap();
        let main = Uri::from_checked_file_path(project.0.join("main.hew")).unwrap();
        let edits = &changes[&main];
        assert_eq!(edits.len(), 1);
        let source = std::fs::read_to_string(project.0.join("main.hew")).unwrap();
        let offsets = hew_analysis::util::compute_line_offsets(&source);
        let start = super::super::position_to_offset(&source, &offsets, edits[0].range.start);
        let end = super::super::position_to_offset(&source, &offsets, edits[0].range.end);
        let updated = format!(
            "{}{}{}",
            &source[..start],
            edits[0].new_text,
            &source[end..]
        );
        assert!(updated.contains("first as greet, /* preserve this alias */ salute as other"));
        assert!(updated.contains("println(greet()); println(other());"));
    }

    #[cfg(unix)]
    #[test]
    fn missing_file_identity_resolves_symlinks_and_parent_components() {
        let project = Project::new(&[("inside.hew", "fn main() {}")]);
        let outside = Project::new(&[("sentinel.hew", "fn main() {}")]);
        std::os::unix::fs::symlink(&outside.0, project.0.join("linked")).unwrap();
        assert_eq!(
            physical_path(&project.0.join("linked/new.hew")),
            outside.0.join("new.hew")
        );
        assert_eq!(
            physical_path(&project.0.join("missing/../new.hew")),
            project.0.join("new.hew")
        );
        assert_eq!(
            physical_path(&project.0.join("linked/missing/../new.hew")),
            outside.0.join("new.hew")
        );
    }

    #[cfg(unix)]
    #[test]
    fn duplicate_open_file_views_are_refused_even_with_equal_text() {
        let project = Project::new(&[("greeting.hew", "pub fn greet() -> string { \"hello\" }\n")]);
        let physical = project.0.join("greeting.hew");
        let alias = project.0.join("alias.hew");
        std::os::unix::fs::symlink(&physical, &alias).unwrap();
        let text = std::fs::read_to_string(&physical).unwrap();
        let snapshot = HashMap::from([
            (
                Uri::from_checked_file_path(&physical).unwrap(),
                OpenDocument {
                    source: text.clone(),
                    version: 2,
                },
            ),
            (
                Uri::from_checked_file_path(&alias).unwrap(),
                OpenDocument {
                    source: text,
                    version: 7,
                },
            ),
        ]);
        assert!(source_snapshot(&snapshot).is_err());
    }
}
