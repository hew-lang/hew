use std::collections::{HashMap, HashSet, VecDeque};

use dashmap::DashMap;
use hew_analysis::util::compute_line_offsets;
use hew_parser::ast::{ImportDecl, Item};
use hew_parser::ParseResult;
use hew_types::error::{Severity, TypeErrorKind};
use hew_types::module_registry::build_module_search_paths;
use hew_types::{Checker, TypeCheckOutput};
use tower_lsp::lsp_types::{
    Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, DiagnosticTag, Location, Url,
};

use super::{DiagnosticMap, DiagnosticSource, DocumentState};

// ── In-memory module resolution ──────────────────────────────────────

/// Return the source text for a file, preferring open editor buffers over disk.
///
/// Checks the LSP document store first so that unsaved edits to an imported
/// module are immediately visible to type-checking in the importing file.
/// Falls back to `std::fs::read_to_string` for files that are not open.
pub(super) fn source_for_path(
    path: &std::path::Path,
    documents: &DashMap<Url, DocumentState>,
) -> Option<String> {
    // Prefer in-memory content if the file is currently open in the editor.
    if let Ok(url) = Url::from_file_path(path) {
        if let Some(doc) = documents.get(&url) {
            return Some(doc.source.clone());
        }
    }
    // Fall back to on-disk content.
    std::fs::read_to_string(path).ok()
}

/// Recursively populate `ImportDecl::resolved_items` for user and file imports.
///
/// After parsing a document the `resolved_items` field on every `ImportDecl`
/// is `None`.  The CLI fills these in by reading files from disk; the LSP
/// historically skipped this step, so imported modules were only resolved via
/// the stdlib `ModuleRegistry` and not from the project tree.
///
/// This function walks `items`, finds unresolved `Import` nodes, locates the
/// corresponding `.hew` file relative to `source_dir`, reads it — **preferring
/// any open editor buffer over the on-disk version** — and populates
/// `resolved_items` so the type checker sees the current in-memory content.
///
/// Depth is capped at [`MAX_IMPORT_DEPTH`] to prevent cycles.
pub(super) fn populate_user_module_imports(
    source_uri: &Url,
    items: &mut [hew_parser::ast::Spanned<hew_parser::ast::Item>],
    documents: &DashMap<Url, DocumentState>,
) {
    let Ok(source_path) = source_uri.to_file_path() else {
        return; // Non-file URI — nothing to resolve.
    };
    let Some(source_dir) = source_path.parent() else {
        return;
    };
    populate_user_module_imports_impl(source_dir, items, documents, 0);
}

/// Maximum import nesting depth to prevent unbounded recursion on cycles.
const MAX_IMPORT_DEPTH: usize = 16;

pub(super) fn populate_user_module_imports_impl(
    source_dir: &std::path::Path,
    items: &mut [hew_parser::ast::Spanned<hew_parser::ast::Item>],
    documents: &DashMap<Url, DocumentState>,
    depth: usize,
) {
    if depth >= MAX_IMPORT_DEPTH {
        return;
    }

    for (item, _span) in items.iter_mut() {
        let decl = match item {
            // Only process imports that haven't been resolved yet.
            Item::Import(d)
                if d.resolved_items.is_none() && (d.file_path.is_some() || !d.path.is_empty()) =>
            {
                d
            }
            _ => continue,
        };

        let candidates = import_candidate_paths_from_dir(source_dir, decl);

        for candidate in &candidates {
            // `source_for_path` checks the in-memory document store first so
            // unsaved edits are preferred over the on-disk version.
            if let Some(source) = source_for_path(candidate, documents) {
                let parsed = hew_parser::parse(&source);
                let has_errors = parsed
                    .errors
                    .iter()
                    .any(|e| e.severity == hew_parser::Severity::Error);
                if !has_errors {
                    let mut module_items = parsed.program.items;
                    // Recursively resolve any imports inside the loaded module.
                    let module_dir = candidate.parent().unwrap_or(source_dir);
                    populate_user_module_imports_impl(
                        module_dir,
                        &mut module_items,
                        documents,
                        depth + 1,
                    );
                    let item_count = module_items.len();
                    decl.resolved_source_paths = vec![candidate.clone()];
                    decl.resolved_item_source_paths = vec![candidate.clone(); item_count];
                    decl.resolved_items = Some(module_items);
                }
                // Stop after the first candidate that yielded source text,
                // regardless of whether it parsed cleanly — otherwise we'd
                // silently fall through to a stale on-disk version.
                break;
            }
        }
    }
}

pub(super) fn import_candidate_paths_from_dir(
    source_dir: &std::path::Path,
    import: &ImportDecl,
) -> Vec<std::path::PathBuf> {
    if let Some(file_path) = &import.file_path {
        return vec![source_dir.join(file_path)];
    }

    let Some(last) = import.path.last() else {
        return vec![];
    };

    // Build the two canonical candidate paths the CLI also tries:
    //   1. package-directory form:  source_dir/<a>/<b>/<b>.hew
    //   2. flat form:               source_dir/<a>/<b>.hew
    let rel_path: std::path::PathBuf = import
        .path
        .iter()
        .collect::<std::path::PathBuf>()
        .with_extension("hew");
    let dir_path: std::path::PathBuf = import
        .path
        .iter()
        .collect::<std::path::PathBuf>()
        .join(format!("{last}.hew"));

    vec![source_dir.join(&dir_path), source_dir.join(&rel_path)]
}

pub(super) fn import_candidate_paths(uri: &Url, import: &ImportDecl) -> Vec<std::path::PathBuf> {
    let Ok(source_path) = uri.to_file_path() else {
        return vec![];
    };
    let Some(source_dir) = source_path.parent() else {
        return vec![];
    };

    import_candidate_paths_from_dir(source_dir, import)
}

pub(super) fn module_id_from_file(
    source_dir: &std::path::Path,
    canonical_path: &std::path::Path,
) -> hew_parser::module::ModuleId {
    use hew_parser::module::ModuleId;

    let without_ext = canonical_path.with_extension("");
    let rel = without_ext.strip_prefix(source_dir).unwrap_or(&without_ext);
    let mut segments: Vec<String> = rel
        .iter()
        .filter_map(|segment| segment.to_str())
        .map(std::string::ToString::to_string)
        .collect();

    if segments.is_empty() {
        segments.push(
            canonical_path
                .file_stem()
                .and_then(|segment| segment.to_str())
                .unwrap_or("unknown")
                .to_string(),
        );
    }

    ModuleId::new(segments)
}

pub(super) fn resolved_import_source_path(
    current_source: &std::path::Path,
    source_dir: &std::path::Path,
    decl: &ImportDecl,
) -> Option<std::path::PathBuf> {
    decl.resolved_source_paths.first().cloned().or_else(|| {
        decl.file_path.as_ref().map(|file_path| {
            current_source
                .parent()
                .unwrap_or(source_dir)
                .join(file_path)
        })
    })
}

#[derive(Debug)]
pub(super) struct DanglingImport {
    source_path: std::path::PathBuf,
    span: hew_parser::ast::Span,
    module_id: hew_parser::module::ModuleId,
}

#[derive(Debug)]
pub(super) struct ModuleGraphBuild {
    graph: hew_parser::module::ModuleGraph,
    dangling_imports: Vec<DanglingImport>,
}

#[derive(Debug)]
pub(super) struct ModuleGraphCycle {
    graph: hew_parser::module::ModuleGraph,
    cycle: hew_parser::module::CycleError,
    dangling_imports: Vec<DanglingImport>,
}

#[derive(Debug)]
pub(super) enum ModuleGraphBuildResult {
    Ready(ModuleGraphBuild),
    Cycle(ModuleGraphCycle),
}

pub(super) fn build_document_module_graph(
    source_uri: &Url,
    program: &hew_parser::ast::Program,
) -> Option<ModuleGraphBuildResult> {
    use hew_parser::module::{Module, ModuleGraph};

    let input_path = source_uri.to_file_path().ok()?;
    let input_path = std::fs::canonicalize(&input_path).unwrap_or(input_path);
    let source_dir = input_path.parent().unwrap_or(std::path::Path::new("."));
    let root_id = module_id_from_file(source_dir, &input_path);
    let mut graph = ModuleGraph::new(root_id.clone());
    let mut seen_ids = HashSet::from([root_id.clone()]);
    let mut dangling_imports = Vec::new();

    let root_imports = extract_module_info(
        &program.items,
        &input_path,
        source_dir,
        &input_path,
        &root_id,
        &mut graph,
        &mut seen_ids,
        &mut dangling_imports,
    );

    graph.add_module(Module {
        id: root_id,
        items: program.items.clone(),
        imports: root_imports,
        source_paths: vec![input_path],
        doc: program.module_doc.clone(),
    });

    match graph.compute_topo_order() {
        Ok(()) => Some(ModuleGraphBuildResult::Ready(ModuleGraphBuild {
            graph,
            dangling_imports,
        })),
        Err(cycle) => Some(ModuleGraphBuildResult::Cycle(ModuleGraphCycle {
            graph,
            cycle,
            dangling_imports,
        })),
    }
}

#[expect(
    clippy::too_many_arguments,
    reason = "module graph construction threads shared graph state and dangling import output"
)]
pub(super) fn extract_module_info(
    items: &[hew_parser::ast::Spanned<Item>],
    current_source: &std::path::Path,
    source_dir: &std::path::Path,
    root_source: &std::path::Path,
    root_id: &hew_parser::module::ModuleId,
    graph: &mut hew_parser::module::ModuleGraph,
    seen_ids: &mut HashSet<hew_parser::module::ModuleId>,
    dangling_imports: &mut Vec<DanglingImport>,
) -> Vec<hew_parser::module::ModuleImport> {
    use hew_parser::module::{Module, ModuleId, ModuleImport};

    let mut imports = Vec::new();

    for (item, span) in items {
        let Item::Import(decl) = item else { continue };

        let first_source_path = resolved_import_source_path(current_source, source_dir, decl);
        let module_id = if !decl.path.is_empty() {
            ModuleId::new(decl.path.clone())
        } else if let Some(source_path) = first_source_path.as_ref() {
            if source_path == root_source {
                root_id.clone()
            } else {
                module_id_from_file(source_dir, source_path)
            }
        } else {
            continue;
        };

        imports.push(ModuleImport {
            target: module_id.clone(),
            spec: decl.spec.clone(),
            span: span.clone(),
        });

        if seen_ids.insert(module_id.clone()) {
            if let Some(resolved_items) = &decl.resolved_items {
                let child_source = first_source_path.as_deref().unwrap_or(current_source);
                let child_imports = extract_module_info(
                    resolved_items,
                    child_source,
                    source_dir,
                    root_source,
                    root_id,
                    graph,
                    seen_ids,
                    dangling_imports,
                );
                let source_paths = if decl.resolved_source_paths.is_empty() {
                    first_source_path.iter().cloned().collect()
                } else {
                    decl.resolved_source_paths.clone()
                };
                graph.add_module(Module {
                    id: module_id,
                    items: resolved_items.clone(),
                    imports: child_imports,
                    source_paths,
                    doc: None,
                });
            } else {
                dangling_imports.push(DanglingImport {
                    source_path: current_source.to_path_buf(),
                    span: span.clone(),
                    module_id,
                });
            }
        }
    }

    imports
}

pub(super) fn build_module_source_map(
    program: &hew_parser::ast::Program,
    documents: &DashMap<Url, DocumentState>,
) -> HashMap<String, DiagnosticSource> {
    let Some(ref module_graph) = program.module_graph else {
        return HashMap::new();
    };

    let mut module_sources = HashMap::new();
    for module_id in &module_graph.topo_order {
        if *module_id == module_graph.root {
            continue;
        }
        let Some(module) = module_graph.modules.get(module_id) else {
            continue;
        };
        let Some(source_path) = module.source_paths.first() else {
            continue;
        };
        let Ok(uri) = Url::from_file_path(source_path) else {
            continue;
        };
        let Some(source) = source_for_path(source_path, documents) else {
            continue;
        };
        module_sources.insert(
            module_id.path.join("."),
            DiagnosticSource {
                uri,
                line_offsets: compute_line_offsets(&source),
                source,
            },
        );
    }

    module_sources
}

pub(super) fn build_dangling_import_diagnostics(
    source: &str,
    line_offsets: &[usize],
    source_uri: &Url,
    dangling_imports: &[DanglingImport],
    documents: &DashMap<Url, DocumentState>,
) -> DiagnosticMap {
    let mut diagnostics_by_uri = DiagnosticMap::new();

    for dangling_import in dangling_imports {
        let Ok(uri) = Url::from_file_path(&dangling_import.source_path) else {
            continue;
        };
        let message = format!(
            "unresolved import '{}'",
            dangling_import.module_id.path.join(".")
        );
        let diagnostic = if uri == *source_uri {
            Diagnostic {
                range: super::span_to_range(source, line_offsets, &dangling_import.span),
                severity: Some(DiagnosticSeverity::ERROR),
                source: Some("hew-lsp".to_string()),
                message,
                ..Default::default()
            }
        } else if let Some(doc) = documents.get(&uri) {
            Diagnostic {
                range: super::span_to_range(&doc.source, &doc.line_offsets, &dangling_import.span),
                severity: Some(DiagnosticSeverity::ERROR),
                source: Some("hew-lsp".to_string()),
                message,
                ..Default::default()
            }
        } else {
            let Some(target_source) = source_for_path(&dangling_import.source_path, documents)
            else {
                continue;
            };
            let target_line_offsets = compute_line_offsets(&target_source);
            Diagnostic {
                range: super::span_to_range(
                    &target_source,
                    &target_line_offsets,
                    &dangling_import.span,
                ),
                severity: Some(DiagnosticSeverity::ERROR),
                source: Some("hew-lsp".to_string()),
                message,
                ..Default::default()
            }
        };
        insert_diagnostic(&mut diagnostics_by_uri, uri, diagnostic);
    }

    diagnostics_by_uri
}

pub(super) fn build_module_cycle_diagnostics(
    source: &str,
    line_offsets: &[usize],
    source_uri: &Url,
    cycle: &ModuleGraphCycle,
    documents: &DashMap<Url, DocumentState>,
) -> DiagnosticMap {
    let mut diagnostics_by_uri = DiagnosticMap::new();
    let message = format!("{}; falling back to per-file analysis", cycle.cycle);

    for window in cycle.cycle.cycle.windows(2) {
        let [module_id, target_id] = window else {
            continue;
        };
        let Some(module) = cycle.graph.modules.get(module_id) else {
            continue;
        };
        let Some(import) = module
            .imports
            .iter()
            .find(|import| import.target == *target_id)
        else {
            continue;
        };
        let Some(source_path) = module.source_paths.first() else {
            continue;
        };
        let Ok(uri) = Url::from_file_path(source_path) else {
            continue;
        };

        let diagnostic = if uri == *source_uri {
            Diagnostic {
                range: super::span_to_range(source, line_offsets, &import.span),
                severity: Some(DiagnosticSeverity::ERROR),
                source: Some("hew-lsp".to_string()),
                message: message.clone(),
                ..Default::default()
            }
        } else if let Some(doc) = documents.get(&uri) {
            Diagnostic {
                range: super::span_to_range(&doc.source, &doc.line_offsets, &import.span),
                severity: Some(DiagnosticSeverity::ERROR),
                source: Some("hew-lsp".to_string()),
                message: message.clone(),
                ..Default::default()
            }
        } else {
            let Some(target_source) = source_for_path(source_path, documents) else {
                continue;
            };
            let target_line_offsets = compute_line_offsets(&target_source);
            Diagnostic {
                range: super::span_to_range(&target_source, &target_line_offsets, &import.span),
                severity: Some(DiagnosticSeverity::ERROR),
                source: Some("hew-lsp".to_string()),
                message: message.clone(),
                ..Default::default()
            }
        };
        insert_diagnostic(&mut diagnostics_by_uri, uri, diagnostic);
    }

    diagnostics_by_uri
}

pub(super) fn merge_diagnostics(into: &mut DiagnosticMap, from: &DiagnosticMap) {
    for (uri, diagnostics) in from {
        into.entry(uri.clone())
            .or_default()
            .extend(diagnostics.iter().cloned());
    }
}

pub(super) fn sort_and_dedup_diagnostics(diagnostics: &mut Vec<Diagnostic>) {
    diagnostics.sort_by(|left, right| {
        left.range
            .start
            .line
            .cmp(&right.range.start.line)
            .then(left.range.start.character.cmp(&right.range.start.character))
            .then(left.range.end.line.cmp(&right.range.end.line))
            .then(left.range.end.character.cmp(&right.range.end.character))
            .then(
                left.source
                    .as_deref()
                    .unwrap_or("")
                    .cmp(right.source.as_deref().unwrap_or("")),
            )
            .then(left.message.cmp(&right.message))
    });

    let mut seen = HashSet::new();
    diagnostics.retain(|diagnostic| seen.insert(format!("{diagnostic:?}")));
}

pub(super) fn collect_published_diagnostics(
    documents: &DashMap<Url, DocumentState>,
    publish_uris: HashSet<Url>,
) -> Vec<(Url, Vec<Diagnostic>)> {
    let mut diagnostics_by_uri = DiagnosticMap::new();
    for entry in documents {
        merge_diagnostics(&mut diagnostics_by_uri, &entry.value().diagnostics_by_uri);
    }

    for uri in publish_uris {
        diagnostics_by_uri.entry(uri).or_default();
    }

    let mut published: Vec<_> = diagnostics_by_uri.into_iter().collect();
    for (_, diagnostics) in &mut published {
        sort_and_dedup_diagnostics(diagnostics);
    }
    published.sort_by(|(left_uri, _), (right_uri, _)| left_uri.as_str().cmp(right_uri.as_str()));
    published
}

pub(super) fn analyze_document(
    uri: &Url,
    source: &str,
    documents: &DashMap<Url, DocumentState>,
) -> DocumentState {
    let parse_result = hew_parser::parse(source);
    let line_offsets = compute_line_offsets(source);

    let has_parse_errors = parse_result
        .errors
        .iter()
        .any(|e| e.severity == hew_parser::Severity::Error);

    let (type_output, module_sources, dangling_import_diagnostics, cycle_diagnostics) =
        if has_parse_errors {
            (None, HashMap::new(), HashMap::new(), HashMap::new())
        } else {
            let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(
                build_module_search_paths(),
            ));
            // Clone the program so we can inject resolved_items for user-module
            // imports without mutating the parse_result stored in DocumentState
            // (other LSP features use the raw AST and do not need resolved_items).
            let mut program = parse_result.program.clone();
            populate_user_module_imports(uri, &mut program.items, documents);
            let module_graph_build = build_document_module_graph(uri, &program);
            let (module_graph, dangling_import_diagnostics, cycle_diagnostics) =
                match module_graph_build {
                    Some(ModuleGraphBuildResult::Ready(build)) => (
                        Some(build.graph),
                        build_dangling_import_diagnostics(
                            source,
                            &line_offsets,
                            uri,
                            &build.dangling_imports,
                            documents,
                        ),
                        HashMap::new(),
                    ),
                    Some(ModuleGraphBuildResult::Cycle(cycle)) => (
                        None,
                        build_dangling_import_diagnostics(
                            source,
                            &line_offsets,
                            uri,
                            &cycle.dangling_imports,
                            documents,
                        ),
                        build_module_cycle_diagnostics(
                            source,
                            &line_offsets,
                            uri,
                            &cycle,
                            documents,
                        ),
                    ),
                    None => (None, HashMap::new(), HashMap::new()),
                };
            program.module_graph = module_graph;
            let module_sources = build_module_source_map(&program, documents);
            (
                Some(checker.check_program(&program)),
                module_sources,
                dangling_import_diagnostics,
                cycle_diagnostics,
            )
        };

    let mut diagnostics_by_uri = build_diagnostics_by_uri(
        uri,
        source,
        &line_offsets,
        &parse_result,
        type_output.as_ref(),
        &module_sources,
    );
    merge_diagnostics(&mut diagnostics_by_uri, &dangling_import_diagnostics);
    merge_diagnostics(&mut diagnostics_by_uri, &cycle_diagnostics);

    DocumentState {
        source: source.to_string(),
        line_offsets,
        parse_result,
        type_output,
        diagnostics_by_uri,
    }
}

fn build_reverse_importer_index(documents: &DashMap<Url, DocumentState>) -> HashMap<Url, Vec<Url>> {
    let mut index: HashMap<Url, Vec<Url>> = HashMap::with_capacity(documents.len());

    for entry in documents {
        let importer_uri = entry.key().clone();
        let parse_result = &entry.value().parse_result;
        for (item, _) in &parse_result.program.items {
            let Item::Import(import) = item else {
                continue;
            };
            for candidate_uri in import_candidate_paths(&importer_uri, import)
                .into_iter()
                .filter_map(|path| Url::from_file_path(path).ok())
            {
                index
                    .entry(candidate_uri)
                    .or_default()
                    .push(importer_uri.clone());
            }
        }
    }

    index
}

pub(super) fn refresh_open_importers(
    target_uri: &Url,
    documents: &DashMap<Url, DocumentState>,
    publish_uris: &mut HashSet<Url>,
) {
    // BFS over the open-document importer graph so that transitive importers
    // (e.g. A -> B -> C when C changes) are also re-analysed.
    // `visited` tracks every URI we have already queued or processed so that
    // diamond imports and import cycles don't cause infinite loops.
    let reverse_importer_index = build_reverse_importer_index(documents);
    let mut visited: HashSet<Url> = HashSet::from([target_uri.clone()]);
    let mut queue: VecDeque<Url> = VecDeque::from([target_uri.clone()]);

    while let Some(current) = queue.pop_front() {
        let dependents: Vec<_> = reverse_importer_index
            .get(&current)
            .into_iter()
            .flat_map(|uris| uris.iter())
            .filter_map(|importer_uri| {
                if visited.contains(importer_uri) {
                    return None;
                }
                let importer = documents.get(importer_uri)?;
                Some((
                    importer_uri.clone(),
                    importer.source.clone(),
                    importer
                        .diagnostics_by_uri
                        .keys()
                        .cloned()
                        .collect::<Vec<_>>(),
                ))
            })
            .collect();

        for (importer_uri, importer_source, previous_diagnostic_uris) in dependents {
            visited.insert(importer_uri.clone());
            publish_uris.insert(importer_uri.clone());
            publish_uris.extend(previous_diagnostic_uris);
            let document = analyze_document(&importer_uri, &importer_source, documents);
            documents.insert(importer_uri.clone(), document);
            // Enqueue this importer so its own importers are refreshed too.
            queue.push_back(importer_uri);
        }
    }
}

pub(super) fn refresh_document_and_dependents(
    uri: &Url,
    source: &str,
    documents: &DashMap<Url, DocumentState>,
) -> Vec<(Url, Vec<Diagnostic>)> {
    let mut publish_uris = HashSet::from([uri.clone()]);
    if let Some(previous) = documents.get(uri) {
        publish_uris.extend(previous.diagnostics_by_uri.keys().cloned());
    }

    let document = analyze_document(uri, source, documents);
    documents.insert(uri.clone(), document);

    refresh_open_importers(uri, documents, &mut publish_uris);

    collect_published_diagnostics(documents, publish_uris)
}

pub(super) fn close_document_and_dependents(
    uri: &Url,
    documents: &DashMap<Url, DocumentState>,
) -> Vec<(Url, Vec<Diagnostic>)> {
    let Some((_removed_uri, removed_document)) = documents.remove(uri) else {
        return vec![];
    };

    let mut publish_uris = HashSet::from([uri.clone()]);
    publish_uris.extend(removed_document.diagnostics_by_uri.keys().cloned());

    refresh_open_importers(uri, documents, &mut publish_uris);

    collect_published_diagnostics(documents, publish_uris)
}

// ── Diagnostics ──────────────────────────────────────────────────────

pub(super) fn insert_diagnostic(
    diagnostics_by_uri: &mut DiagnosticMap,
    uri: Url,
    diagnostic: Diagnostic,
) {
    diagnostics_by_uri.entry(uri).or_default().push(diagnostic);
}

#[cfg(test)]
pub(super) fn build_diagnostics(
    uri: &Url,
    source: &str,
    lo: &[usize],
    parse_result: &ParseResult,
    type_output: Option<&TypeCheckOutput>,
) -> Vec<Diagnostic> {
    build_diagnostics_by_uri(uri, source, lo, parse_result, type_output, &HashMap::new())
        .remove(uri)
        .unwrap_or_default()
}

pub(super) fn build_diagnostics_by_uri(
    uri: &Url,
    source: &str,
    lo: &[usize],
    parse_result: &ParseResult,
    type_output: Option<&TypeCheckOutput>,
    module_sources: &HashMap<String, DiagnosticSource>,
) -> DiagnosticMap {
    let mut diagnostics_by_uri = DiagnosticMap::new();

    for err in &parse_result.errors {
        let message = if let Some(hint) = &err.hint {
            format!("{}\n\nhint: {hint}", err.message)
        } else {
            err.message.clone()
        };
        let severity = match err.severity {
            hew_parser::Severity::Error => DiagnosticSeverity::ERROR,
            hew_parser::Severity::Warning => DiagnosticSeverity::WARNING,
        };
        insert_diagnostic(
            &mut diagnostics_by_uri,
            uri.clone(),
            Diagnostic {
                range: super::span_to_range(source, lo, &err.span),
                severity: Some(severity),
                source: Some("hew-parser".to_string()),
                message,
                ..Default::default()
            },
        );
    }

    if let Some(tc) = type_output {
        for diag in tc.errors.iter().chain(tc.warnings.iter()) {
            let target = diag
                .source_module
                .as_ref()
                .and_then(|module_name| module_sources.get(module_name));
            let (target_uri, target_source, target_line_offsets) = if let Some(target) = target {
                (
                    target.uri.clone(),
                    target.source.as_str(),
                    target.line_offsets.as_slice(),
                )
            } else {
                (uri.clone(), source, lo)
            };

            // Build the message, appending any suggestions.
            let message = if diag.suggestions.is_empty() {
                diag.message.clone()
            } else {
                format!(
                    "{}\n\nDid you mean: {}",
                    diag.message,
                    diag.suggestions.join(", ")
                )
            };

            // Surface notes as related information with their own spans.
            let related_information = if diag.notes.is_empty() {
                None
            } else {
                Some(
                    diag.notes
                        .iter()
                        .map(|(note_span, note_msg)| DiagnosticRelatedInformation {
                            location: Location {
                                uri: target_uri.clone(),
                                range: super::span_to_range(
                                    target_source,
                                    target_line_offsets,
                                    note_span,
                                ),
                            },
                            message: note_msg.clone(),
                        })
                        .collect(),
                )
            };

            insert_diagnostic(
                &mut diagnostics_by_uri,
                target_uri,
                Diagnostic {
                    range: super::span_to_range(target_source, target_line_offsets, &diag.span),
                    severity: Some(severity_to_lsp(diag.severity)),
                    tags: unnecessary_diagnostic_tags(&diag.kind),
                    source: Some("hew-types".to_string()),
                    message,
                    related_information,
                    data: Some(diagnostic_data(&diag.kind, &diag.suggestions)),
                    ..Default::default()
                },
            );
        }
    }

    diagnostics_by_uri
}

/// Convert a type-checker `Severity` to the corresponding LSP `DiagnosticSeverity`.
///
/// This is the authoritative severity mapping: the `TypeError` struct carries the
/// severity that was decided at emit time, so the LSP should honour it directly
/// rather than re-deriving it from the `TypeErrorKind`.
pub(super) fn severity_to_lsp(severity: Severity) -> DiagnosticSeverity {
    match severity {
        Severity::Error => DiagnosticSeverity::ERROR,
        Severity::Warning => DiagnosticSeverity::WARNING,
    }
}

/// Encode a `TypeErrorKind` discriminant and suggestions as JSON for `Diagnostic.data`.
pub(super) fn diagnostic_data(kind: &TypeErrorKind, suggestions: &[String]) -> serde_json::Value {
    serde_json::json!({
        "kind": kind.as_kind_str(),
        "suggestions": suggestions,
    })
}

fn unnecessary_diagnostic_tags(kind: &TypeErrorKind) -> Option<Vec<DiagnosticTag>> {
    match kind {
        TypeErrorKind::UnusedVariable
        | TypeErrorKind::UnusedMut
        | TypeErrorKind::UnusedImport
        | TypeErrorKind::DeadCode
        | TypeErrorKind::UnreachableCode => Some(vec![DiagnosticTag::UNNECESSARY]),
        _ => None,
    }
}
