use std::collections::{HashMap, HashSet, VecDeque};

use dashmap::DashMap;
use hew_analysis::util::compute_line_offsets;
use hew_hir::{
    lower_program_host_target, verify_hir, HirDiagnostic, HirDiagnosticKind, ResolutionCtx,
};
use hew_parser::ast::{ImportDecl, Item};
use hew_parser::{ParseDiagnosticKind, ParseResult};
use hew_types::error::{Severity, TypeErrorKind};
use hew_types::module_registry::{build_module_search_paths, build_module_search_paths_for};
use hew_types::{Checker, LintId, LintSources, TypeCheckOutput};
use tower_lsp::lsp_types::{
    Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, DiagnosticTag, Location,
    NumberOrString, Url,
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
/// is `None`. The LSP must resolve imported modules from the project tree, not
/// just through the stdlib `ModuleRegistry`.
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
    extra_pkg_paths: &[std::path::PathBuf],
) -> Vec<AmbiguousImport> {
    let Ok(source_path) = source_uri.to_file_path() else {
        return Vec::new(); // Non-file URI — nothing to resolve.
    };
    let Some(source_dir) = source_path.parent() else {
        return Vec::new();
    };
    let mut search_roots = build_module_search_paths_for(Some(&source_path));
    // Append explicit package-search paths (from `--pkg-path` / `hew.pkgPath`
    // setting) so that `hew check --pkg-path DIR` and LSP import resolution
    // agree on which modules resolve.  Appended after stdlib roots so that
    // a user package does not shadow the standard library.
    for pkg_path in extra_pkg_paths {
        if pkg_path.exists() && !search_roots.contains(pkg_path) {
            search_roots.push(pkg_path.clone());
        }
    }
    let mut ambiguities = Vec::new();
    populate_user_module_imports_impl(
        &source_path,
        source_dir,
        &search_roots,
        items,
        documents,
        0,
        &mut ambiguities,
        extra_pkg_paths,
    );
    ambiguities
}

/// Maximum import nesting depth to prevent unbounded recursion on cycles.
const MAX_IMPORT_DEPTH: usize = 16;

/// An import whose dotted path resolves to more than one distinct module file.
///
/// Mirrors the compiler's fail-closed resolver (`hew-compile/src/lib.rs`):
/// when a module exists both as a workspace-local file and in a stdlib/global
/// search root (or in both the package-directory and flat candidate forms),
/// the import is ambiguous and must be rejected rather than silently bound to
/// whichever candidate happens to come first.
#[derive(Debug)]
pub(super) struct AmbiguousImport {
    source_path: std::path::PathBuf,
    span: hew_parser::ast::Span,
    module: String,
    paths: Vec<std::path::PathBuf>,
}

#[expect(
    clippy::too_many_arguments,
    reason = "mirrors the compiler's resolution signature; the parameters are all distinct and structural grouping would obscure the parity with hew-compile/src/lib.rs"
)]
pub(super) fn populate_user_module_imports_impl(
    current_source: &std::path::Path,
    source_dir: &std::path::Path,
    search_roots: &[std::path::PathBuf],
    items: &mut [hew_parser::ast::Spanned<hew_parser::ast::Item>],
    documents: &DashMap<Url, DocumentState>,
    depth: usize,
    ambiguities: &mut Vec<AmbiguousImport>,
    extra_pkg_paths: &[std::path::PathBuf],
) {
    if depth >= MAX_IMPORT_DEPTH {
        return;
    }

    for (item, span) in items.iter_mut() {
        let decl = match item {
            // Only process imports that haven't been resolved yet.
            Item::Import(d)
                if d.resolved_items.is_none() && (d.file_path.is_some() || !d.path.is_empty()) =>
            {
                d
            }
            _ => continue,
        };

        // String-literal file imports (`import "foo.hew"`) are resolved
        // LOCAL-ONLY, exactly as the compiler does: it canonicalizes only
        // `source_dir.join(file_path)` and errors if that single local path is
        // absent (hew-compile/src/lib.rs). Search-root (stdlib/global)
        // expansion and ambiguity collection apply ONLY to dotted module
        // imports (`decl.file_path.is_none()`); a missing local file import must
        // fail the compiler's way, never resolve from a stdlib root, and a
        // local+root pair must never be reported ambiguous.
        let is_dotted = decl.file_path.is_none();

        let mut candidates = import_candidate_paths_from_dir(source_dir, decl);
        if is_dotted {
            // Project-local candidates (resolved relative to the importing
            // file's directory) come first, followed by stdlib/library
            // candidates rooted at the module search paths, so
            // `import std::text::regex` resolves to the checkout's `std/` tree
            // exactly as the compiler's module graph does.
            for root in search_roots {
                candidates.extend(import_candidate_paths_from_dir(root, decl));
            }

            // For explicit package-search roots (--pkg-path / hew.pkgPath),
            // mirror the compiler's extra-candidate generation: leading-segment
            // stripping and `hew::` prefix stripping are applied so that
            // `import hew::template` finds `<root>/template/template.hew`
            // just as `hew check --pkg-path <root>` does
            // (hew-compile/src/lib.rs:1085-1110).
            for pkg_path in extra_pkg_paths {
                candidates.extend(import_candidate_paths_from_pkg_path(pkg_path, decl));
            }

            // Fail closed on ambiguity, matching the compiler's resolver
            // (`resolve_file_imports_internal` in hew-compile/src/lib.rs):
            // collect every candidate that resolves on disk, deduplicate by
            // canonical path, and if more than one distinct module would
            // resolve, reject the import instead of silently picking the
            // workspace-local shadow. This keeps the LSP and the compiler in
            // agreement — the LSP must not offer tooling or diagnostics for a
            // module the compiler refuses to bind.
            let mut distinct: Vec<std::path::PathBuf> = candidates
                .iter()
                .filter_map(|candidate| candidate.canonicalize().ok())
                .collect();
            distinct.sort();
            distinct.dedup();

            if distinct.len() > 1 {
                ambiguities.push(AmbiguousImport {
                    source_path: current_source.to_path_buf(),
                    span: span.clone(),
                    module: decl.path.join("::"),
                    paths: distinct,
                });
                // Leave `resolved_items` as `None` so the import is treated as
                // unresolved downstream — the LSP surfaces the ambiguity
                // instead of binding to one candidate.
                continue;
            }
        }

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
                        candidate,
                        module_dir,
                        search_roots,
                        &mut module_items,
                        documents,
                        depth + 1,
                        ambiguities,
                        extra_pkg_paths,
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

/// Generate the full set of candidate paths for an import against an
/// explicit package-search root (a `--pkg-path` / `hew.pkgPath` directory).
///
/// This mirrors the candidate generation the compiler performs at
/// `hew-compile/src/lib.rs` for `ctx.extra_pkg_path`:
///
/// 1. Full-path candidates — `<root>/<a>/<b>/<b>.hew` and `<root>/<a>/<b>.hew`
///    (same as `import_candidate_paths_from_dir`).
/// 2. Leading-segment-stripped candidates — for any import with more than one
///    segment, `<root>/<b>/<b>.hew` and `<root>/<b>.hew` (compiler lines
///    1088-1098).  This lets `import acme::widgets` find `<root>/widgets.hew`.
/// 3. `hew::`-prefix-stripped candidates — for imports whose first segment is
///    `hew`, `<root>/<tail>/<last>.hew` and `<root>/<tail>.hew` (compiler lines
///    1102-1110).  This is what makes `import hew::template` resolve to
///    `<root>/template/template.hew` rather than the wrong full-path
///    `<root>/hew/template/template.hew`.
///
/// The candidates are ordered to match the compiler's insertion order so that
/// the first on-disk hit is the same file the compiler would choose.
pub(super) fn import_candidate_paths_from_pkg_path(
    pkg_root: &std::path::Path,
    import: &ImportDecl,
) -> Vec<std::path::PathBuf> {
    let Some(last) = import.path.last() else {
        return vec![];
    };

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

    let mut candidates = vec![pkg_root.join(&dir_path), pkg_root.join(&rel_path)];

    // Leading-segment-stripped candidates (compiler lines 1088-1098).
    if import.path.len() > 1 {
        let rest_dir = import.path[1..]
            .iter()
            .collect::<std::path::PathBuf>()
            .join(format!("{last}.hew"));
        let rest_flat = import.path[1..]
            .iter()
            .collect::<std::path::PathBuf>()
            .with_extension("hew");
        candidates.push(pkg_root.join(&rest_dir));
        candidates.push(pkg_root.join(&rest_flat));
    }

    // `hew::`-prefix-stripped candidates (compiler lines 1102-1110).
    let module_str = import.path.join("::");
    if module_str.starts_with("hew::") && import.path.len() > 1 {
        let tail = import.path[1..].iter().collect::<std::path::PathBuf>();
        let tail_last = import.path.last().expect("path is non-empty");
        let tail_dir = tail.join(format!("{tail_last}.hew"));
        let tail_rel = tail.with_extension("hew");
        candidates.push(pkg_root.join(&tail_dir));
        candidates.push(pkg_root.join(&tail_rel));
    }

    candidates
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

    graph
        .add_module(Module {
            id: root_id,
            items: program.items.clone(),
            imports: root_imports,
            source_paths: vec![input_path],
            doc: program.module_doc.clone(),
        })
        .expect("root module id is unique");

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
                graph
                    .add_module(Module {
                        id: module_id,
                        items: resolved_items.clone(),
                        imports: child_imports,
                        source_paths,
                        doc: None,
                    })
                    .expect("seen_ids prevents duplicate insertion");
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
        let dangling_code = Some(NumberOrString::String("UnresolvedImport".to_string()));
        let diagnostic = if uri == *source_uri {
            Diagnostic {
                range: super::span_to_range(source, line_offsets, &dangling_import.span),
                severity: Some(DiagnosticSeverity::ERROR),
                code: dangling_code,
                source: Some("hew-lsp".to_string()),
                message,
                ..Default::default()
            }
        } else if let Some(doc) = documents.get(&uri) {
            Diagnostic {
                range: super::span_to_range(&doc.source, &doc.line_offsets, &dangling_import.span),
                severity: Some(DiagnosticSeverity::ERROR),
                code: dangling_code,
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
                code: dangling_code,
                source: Some("hew-lsp".to_string()),
                message,
                ..Default::default()
            }
        };
        insert_diagnostic(&mut diagnostics_by_uri, uri, diagnostic);
    }

    diagnostics_by_uri
}

pub(super) fn build_ambiguous_import_diagnostics(
    source: &str,
    line_offsets: &[usize],
    source_uri: &Url,
    ambiguities: &[AmbiguousImport],
    documents: &DashMap<Url, DocumentState>,
) -> DiagnosticMap {
    let mut diagnostics_by_uri = DiagnosticMap::new();

    for ambiguity in ambiguities {
        let Ok(uri) = Url::from_file_path(&ambiguity.source_path) else {
            continue;
        };
        // Message mirrors the compiler's fail-closed resolver so the LSP and
        // `hew check` report the same ambiguity (hew-compile/src/lib.rs).
        let paths = ambiguity
            .paths
            .iter()
            .map(|p| p.display().to_string())
            .collect::<Vec<_>>()
            .join("` and `");
        let message = format!(
            "import `{}` is ambiguous: both `{paths}` exist. Rename or remove one to resolve the ambiguity.",
            ambiguity.module
        );
        let code = Some(NumberOrString::String("AmbiguousImport".to_string()));
        let diagnostic = if uri == *source_uri {
            Diagnostic {
                range: super::span_to_range(source, line_offsets, &ambiguity.span),
                severity: Some(DiagnosticSeverity::ERROR),
                code,
                source: Some("hew-lsp".to_string()),
                message,
                ..Default::default()
            }
        } else if let Some(doc) = documents.get(&uri) {
            Diagnostic {
                range: super::span_to_range(&doc.source, &doc.line_offsets, &ambiguity.span),
                severity: Some(DiagnosticSeverity::ERROR),
                code,
                source: Some("hew-lsp".to_string()),
                message,
                ..Default::default()
            }
        } else {
            let Some(target_source) = source_for_path(&ambiguity.source_path, documents) else {
                continue;
            };
            let target_line_offsets = compute_line_offsets(&target_source);
            Diagnostic {
                range: super::span_to_range(&target_source, &target_line_offsets, &ambiguity.span),
                severity: Some(DiagnosticSeverity::ERROR),
                code,
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

        let cycle_code = Some(NumberOrString::String("ModuleCycle".to_string()));
        let diagnostic = if uri == *source_uri {
            Diagnostic {
                range: super::span_to_range(source, line_offsets, &import.span),
                severity: Some(DiagnosticSeverity::ERROR),
                code: cycle_code,
                source: Some("hew-lsp".to_string()),
                message: message.clone(),
                ..Default::default()
            }
        } else if let Some(doc) = documents.get(&uri) {
            Diagnostic {
                range: super::span_to_range(&doc.source, &doc.line_offsets, &import.span),
                severity: Some(DiagnosticSeverity::ERROR),
                code: cycle_code,
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
                code: cycle_code,
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

#[expect(
    clippy::too_many_lines,
    reason = "analyze_document sequences the full document analysis pipeline: import resolution, module-graph build, type-check, and diagnostic assembly"
)]
pub(super) fn analyze_document(
    uri: &Url,
    source: &str,
    documents: &DashMap<Url, DocumentState>,
    extra_pkg_paths: &[std::path::PathBuf],
) -> DocumentState {
    let parse_result = hew_parser::parse(source);
    let line_offsets = compute_line_offsets(source);

    let has_parse_errors = parse_result
        .errors
        .iter()
        .any(|e| e.severity == hew_parser::Severity::Error);

    let (
        type_output,
        hir_diagnostics,
        module_sources,
        dangling_import_diagnostics,
        cycle_diagnostics,
        ambiguous_import_diagnostics,
    ) = if has_parse_errors {
        (
            None,
            Vec::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
        )
    } else {
        // Build the module registry with both the standard search paths and any
        // explicit package-search roots so the type-checker sees handle types,
        // drop types, and module-qualified call resolution for packages supplied
        // via `--pkg-path` / `hew.pkgPath` (F3: previously only the import
        // resolver received the extra paths; the type-checker's registry missed
        // them, making `--pkg-path` packages invisible to type analysis even
        // after their imports resolved).
        let mut registry_search_paths = build_module_search_paths();
        for pkg_path in extra_pkg_paths {
            if pkg_path.exists() && !registry_search_paths.contains(pkg_path) {
                registry_search_paths.push(pkg_path.clone());
            }
        }
        let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(
            registry_search_paths,
        ));
        // Clone the program so we can inject resolved_items for user-module
        // imports without mutating the parse_result stored in DocumentState
        // (other LSP features use the raw AST and do not need resolved_items).
        let mut program = parse_result.program.clone();
        let ambiguous_imports =
            populate_user_module_imports(uri, &mut program.items, documents, extra_pkg_paths);
        let ambiguous_import_diagnostics = build_ambiguous_import_diagnostics(
            source,
            &line_offsets,
            uri,
            &ambiguous_imports,
            documents,
        );
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
                    build_module_cycle_diagnostics(source, &line_offsets, uri, &cycle, documents),
                ),
                None => (None, HashMap::new(), HashMap::new()),
            };
        program.module_graph = module_graph;
        let module_sources = build_module_source_map(&program, documents);
        let mut lint_sources = LintSources::new();
        lint_sources.set_root(source.to_string());
        for (module_name, module_source) in &module_sources {
            lint_sources.set_module(module_name.clone(), module_source.source.clone());
        }
        checker.set_lint_sources(lint_sources);
        let type_output = checker.check_program(&program);
        let hir_diagnostics = if type_output.errors.is_empty() {
            collect_hir_diagnostics(&program, &type_output)
        } else {
            Vec::new()
        };
        (
            Some(type_output),
            hir_diagnostics,
            module_sources,
            dangling_import_diagnostics,
            cycle_diagnostics,
            ambiguous_import_diagnostics,
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
    let hir_lsp_diagnostics = build_hir_lsp_diagnostics(
        uri,
        source,
        &line_offsets,
        &module_sources,
        &hir_diagnostics,
    );
    merge_diagnostics(&mut diagnostics_by_uri, &hir_lsp_diagnostics);
    merge_diagnostics(&mut diagnostics_by_uri, &dangling_import_diagnostics);
    merge_diagnostics(&mut diagnostics_by_uri, &cycle_diagnostics);
    merge_diagnostics(&mut diagnostics_by_uri, &ambiguous_import_diagnostics);

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
    extra_pkg_paths: &[std::path::PathBuf],
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
            let document =
                analyze_document(&importer_uri, &importer_source, documents, extra_pkg_paths);
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
    extra_pkg_paths: &[std::path::PathBuf],
) -> Vec<(Url, Vec<Diagnostic>)> {
    let mut publish_uris = HashSet::from([uri.clone()]);
    if let Some(previous) = documents.get(uri) {
        publish_uris.extend(previous.diagnostics_by_uri.keys().cloned());
    }

    let document = analyze_document(uri, source, documents, extra_pkg_paths);
    documents.insert(uri.clone(), document);

    refresh_open_importers(uri, documents, &mut publish_uris, extra_pkg_paths);

    collect_published_diagnostics(documents, publish_uris)
}

pub(super) fn close_document_and_dependents(
    uri: &Url,
    documents: &DashMap<Url, DocumentState>,
    extra_pkg_paths: &[std::path::PathBuf],
) -> Vec<(Url, Vec<Diagnostic>)> {
    let Some((_removed_uri, removed_document)) = documents.remove(uri) else {
        return vec![];
    };

    let mut publish_uris = HashSet::from([uri.clone()]);
    publish_uris.extend(removed_document.diagnostics_by_uri.keys().cloned());

    refresh_open_importers(uri, documents, &mut publish_uris, extra_pkg_paths);

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
                code: Some(NumberOrString::String(err.kind.as_kind_str().to_string())),
                source: Some("hew-parser".to_string()),
                message,
                data: Some(parse_diagnostic_data(&err.kind)),
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
                    code: Some(NumberOrString::String(diag.kind.as_kind_str().to_string())),
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

fn collect_hir_diagnostics(
    program: &hew_parser::ast::Program,
    type_output: &TypeCheckOutput,
) -> Vec<HirDiagnostic> {
    let lower_output = lower_program_host_target(program, type_output, &ResolutionCtx);
    dedup_hir_diagnostics(lower_output.diagnostics, verify_hir(&lower_output.module))
}

fn dedup_hir_diagnostics(
    mut lower_diagnostics: Vec<HirDiagnostic>,
    verifier_diagnostics: Vec<HirDiagnostic>,
) -> Vec<HirDiagnostic> {
    for diagnostic in verifier_diagnostics {
        let already_present = lower_diagnostics
            .iter()
            .any(|existing| existing.kind == diagnostic.kind && existing.span == diagnostic.span);
        if !already_present {
            lower_diagnostics.push(diagnostic);
        }
    }
    lower_diagnostics
}

fn build_hir_lsp_diagnostics(
    root_uri: &Url,
    root_source: &str,
    root_line_offsets: &[usize],
    module_sources: &HashMap<String, DiagnosticSource>,
    hir_diagnostics: &[HirDiagnostic],
) -> DiagnosticMap {
    let mut diagnostics_by_uri = DiagnosticMap::new();

    for diagnostic in hir_diagnostics {
        let unavailable_source = diagnostic
            .source_module
            .as_ref()
            .filter(|module_name| !module_sources.contains_key(*module_name));
        let target = diagnostic
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
            (root_uri.clone(), root_source, root_line_offsets)
        };
        let base_message = hir_diagnostic_message(&diagnostic.kind, &diagnostic.note);
        let message = if let Some(module_name) = unavailable_source {
            format!("[module '{module_name}' source unavailable] {base_message}")
        } else {
            base_message
        };
        let range = if unavailable_source.is_some() {
            zero_range()
        } else {
            super::span_to_range(target_source, target_line_offsets, &diagnostic.span)
        };
        let related_information = if diagnostic.secondary_spans.is_empty() {
            None
        } else {
            Some(
                diagnostic
                    .secondary_spans
                    .iter()
                    .map(|(span, label)| DiagnosticRelatedInformation {
                        location: Location {
                            uri: target_uri.clone(),
                            range: if unavailable_source.is_some() {
                                zero_range()
                            } else {
                                super::span_to_range(target_source, target_line_offsets, span)
                            },
                        },
                        message: label.clone(),
                    })
                    .collect(),
            )
        };
        insert_diagnostic(
            &mut diagnostics_by_uri,
            target_uri,
            Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::ERROR),
                code: Some(NumberOrString::String(hir_diagnostic_kind_string(
                    &diagnostic.kind,
                ))),
                source: Some("hew-hir".to_string()),
                message,
                related_information,
                data: Some(hir_diagnostic_data(&diagnostic.kind)),
                ..Default::default()
            },
        );
    }

    diagnostics_by_uri
}

fn hir_diagnostic_message(kind: &HirDiagnosticKind, note: &str) -> String {
    let base = match kind {
        HirDiagnosticKind::NotYetImplemented {
            construct,
            owning_pass,
        } => format!("not yet implemented: {construct} (planned: {owning_pass})"),
        _ => hir_diagnostic_kind_string(kind),
    };

    if note.is_empty() {
        base
    } else {
        format!("{base}\n\nnote: {note}")
    }
}

fn hir_diagnostic_data(kind: &HirDiagnosticKind) -> serde_json::Value {
    serde_json::json!({
        "kind": hir_diagnostic_kind_string(kind),
        "source": "hir",
    })
}

fn hir_diagnostic_kind_string(kind: &HirDiagnosticKind) -> String {
    let debug = format!("{kind:?}");
    let end = debug.find([' ', '{', '(']).unwrap_or(debug.len());
    debug[..end].to_string()
}

fn zero_range() -> tower_lsp::lsp_types::Range {
    tower_lsp::lsp_types::Range::new(
        tower_lsp::lsp_types::Position::new(0, 0),
        tower_lsp::lsp_types::Position::new(0, 0),
    )
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

/// Encode a `ParseDiagnosticKind` discriminant as JSON for `Diagnostic.data`.
///
/// The shape mirrors `diagnostic_data` so editor extensions can handle both
/// parse and type errors with the same `data` consumer.
pub(super) fn parse_diagnostic_data(kind: &ParseDiagnosticKind) -> serde_json::Value {
    serde_json::json!({
        "kind": kind.as_kind_str(),
    })
}

fn unnecessary_diagnostic_tags(kind: &TypeErrorKind) -> Option<Vec<DiagnosticTag>> {
    match kind {
        TypeErrorKind::UnusedVariable
        | TypeErrorKind::UnusedMut
        | TypeErrorKind::UnusedImport
        | TypeErrorKind::UnreachableCode
        | TypeErrorKind::Lint(LintId::DeadCode) => Some(vec![DiagnosticTag::UNNECESSARY]),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use hew_hir::{HirItem, HirModule, HirNodeId};
    use hew_parser::ast::Span;
    use tower_lsp::lsp_types::Position;

    use super::*;

    #[test]
    fn hir_root_diagnostics_render_to_root_uri() {
        let source = "fn main() {}\n";
        let line_offsets = compute_line_offsets(source);
        let uri = Url::parse("file:///test.hew").unwrap();
        let diagnostics = vec![HirDiagnostic::new(
            HirDiagnosticKind::TaskCannotEscape,
            3..7,
            "task handles must stay inside fork bodies",
        )];

        let by_uri =
            build_hir_lsp_diagnostics(&uri, source, &line_offsets, &HashMap::new(), &diagnostics);
        let rendered = by_uri
            .get(&uri)
            .expect("root diagnostics must stay on the root URI");
        let diagnostic = rendered.first().expect("expected one HIR diagnostic");

        assert_eq!(diagnostic.source.as_deref(), Some("hew-hir"));
        assert_eq!(diagnostic.severity, Some(DiagnosticSeverity::ERROR));
        assert_eq!(diagnostic.range.start, Position::new(0, 3));
        assert_eq!(diagnostic.range.end, Position::new(0, 7));
        assert_eq!(
            diagnostic
                .data
                .as_ref()
                .and_then(|data| data.get("kind"))
                .and_then(serde_json::Value::as_str),
            Some("TaskCannotEscape")
        );
    }

    #[test]
    fn hir_non_root_diagnostics_route_to_imported_uri() {
        let root_uri = Url::parse("file:///project/main.hew").unwrap();
        let helper_uri = Url::parse("file:///project/helper.hew").unwrap();
        let root_source = "fn main() {}\n";
        let helper_source = "fn helper() { bogus }\n";
        let diagnostics = vec![HirDiagnostic::new(
            HirDiagnosticKind::UnresolvedSymbol {
                name: "bogus".to_string(),
            },
            14..19,
            "imported helper body references an unresolved name",
        )
        .with_source_module(Some("helper".to_string()))];
        let module_sources = HashMap::from([(
            "helper".to_string(),
            DiagnosticSource {
                uri: helper_uri.clone(),
                source: helper_source.to_string(),
                line_offsets: compute_line_offsets(helper_source),
            },
        )]);

        let by_uri = build_hir_lsp_diagnostics(
            &root_uri,
            root_source,
            &compute_line_offsets(root_source),
            &module_sources,
            &diagnostics,
        );
        let helper_diags = by_uri
            .get(&helper_uri)
            .expect("imported-module HIR diagnostics must route to the imported URI");
        let diagnostic = helper_diags
            .first()
            .expect("expected one imported-module diagnostic");

        assert_eq!(diagnostic.range.start, Position::new(0, 14));
        assert_eq!(diagnostic.range.end, Position::new(0, 19));
    }

    #[test]
    fn hir_source_map_miss_fails_closed_to_root_zero_range() {
        let root_uri = Url::parse("file:///project/main.hew").unwrap();
        let root_source = "fn main() {}\n";
        let diagnostics = vec![HirDiagnostic::new(
            HirDiagnosticKind::UnresolvedSymbol {
                name: "phantom".to_string(),
            },
            99..120,
            "phantom import body references an unresolved name",
        )
        .with_source_module(Some("phantom".to_string()))];

        let by_uri = build_hir_lsp_diagnostics(
            &root_uri,
            root_source,
            &compute_line_offsets(root_source),
            &HashMap::new(),
            &diagnostics,
        );
        let root_diags = by_uri
            .get(&root_uri)
            .expect("source-map miss must still publish a diagnostic on the root URI");
        let diagnostic = root_diags
            .first()
            .expect("expected one fail-closed diagnostic");

        assert_eq!(diagnostic.range, zero_range());
        assert!(diagnostic
            .message
            .starts_with("[module 'phantom' source unavailable]"));
    }

    #[test]
    fn hir_secondary_spans_render_as_related_information() {
        let root_uri = Url::parse("file:///project/main.hew").unwrap();
        let root_source = "fn main() { abc }\nfn helper() {}\n";
        let diagnostic = HirDiagnostic::new(
            HirDiagnosticKind::MachineEffectParityViolation {
                machine_name: "M".to_string(),
                state_name: "S".to_string(),
                field_name: "field".to_string(),
                transition_event: "tick".to_string(),
                is_entry_conflict: true,
            },
            12..15,
            "transition conflicts with state entry",
        )
        .with_secondary_spans(vec![(21..27, "conflicting state entry".to_string())]);

        let by_uri = build_hir_lsp_diagnostics(
            &root_uri,
            root_source,
            &compute_line_offsets(root_source),
            &HashMap::new(),
            &[diagnostic],
        );
        let rendered = by_uri
            .get(&root_uri)
            .and_then(|diagnostics| diagnostics.first())
            .expect("expected one root diagnostic");
        let related = rendered
            .related_information
            .as_ref()
            .expect("secondary spans should render as related information");

        assert_eq!(related.len(), 1);
        assert_eq!(related[0].location.uri, root_uri);
        assert_eq!(related[0].location.range.start, Position::new(1, 3));
    }

    #[test]
    fn verifier_only_hir_diagnostics_are_rendered() {
        let source = "fn main() -> i64 { 1 }\n";
        let line_offsets = compute_line_offsets(source);
        let uri = Url::parse("file:///test.hew").unwrap();
        let span = 19..20;

        let combined =
            dedup_hir_diagnostics(Vec::new(), verify_hir(&duplicate_node_module(span.clone())));
        assert_eq!(combined.len(), 1, "expected one verifier-only diagnostic");

        let by_uri =
            build_hir_lsp_diagnostics(&uri, source, &line_offsets, &HashMap::new(), &combined);
        let rendered = by_uri
            .get(&uri)
            .and_then(|diagnostics| diagnostics.first())
            .expect("verifier-only HIR diagnostic should render to LSP");

        assert_eq!(rendered.source.as_deref(), Some("hew-hir"));
        assert_eq!(
            rendered
                .data
                .as_ref()
                .and_then(|data| data.get("kind"))
                .and_then(serde_json::Value::as_str),
            Some("DuplicateNodeId")
        );
    }

    #[test]
    fn duplicate_verifier_hir_diagnostics_are_suppressed_by_kind_and_span() {
        let span = 19..20;
        let lower_diagnostic = HirDiagnostic::new(
            HirDiagnosticKind::DuplicateNodeId { id: HirNodeId(1) },
            span.clone(),
            "lowering already reported duplicate HIR node id",
        );

        let combined = dedup_hir_diagnostics(
            vec![lower_diagnostic.clone()],
            verify_hir(&duplicate_node_module(span)),
        );

        assert_eq!(
            combined.len(),
            1,
            "duplicate verifier diagnostic should be suppressed"
        );
        assert_eq!(combined[0], lower_diagnostic);
    }

    #[test]
    fn analyze_document_skips_hir_when_typecheck_fails() {
        let uri = Url::parse("file:///test.hew").unwrap();
        let document = analyze_document(&uri, "fn main() -> i32 { missing }", &DashMap::new(), &[]);

        let root_diags = document
            .diagnostics_by_uri
            .get(&uri)
            .expect("root document should always have a diagnostics entry");

        assert!(
            root_diags
                .iter()
                .all(|diagnostic| diagnostic.source.as_deref() != Some("hew-hir")),
            "HIR diagnostics must not run when type checking reports errors: {root_diags:?}"
        );
    }

    #[test]
    fn analyze_document_publishes_hir_diagnostics_after_successful_typecheck() {
        let main_uri = Url::parse("file:///project/main.hew").unwrap();
        let document = analyze_document(
            &main_uri,
            "fn main() { let r = select { }; }\n",
            &DashMap::new(),
            &[],
        );
        let root_diags = document
            .diagnostics_by_uri
            .get(&main_uri)
            .expect("root document should always have a diagnostics entry");

        assert!(
            root_diags
                .iter()
                .any(|diagnostic| diagnostic.source.as_deref() == Some("hew-hir")),
            "expected HIR diagnostics after successful typecheck: {root_diags:?}"
        );
    }

    #[test]
    fn analyze_document_reports_comment_text_direction_lint() {
        let uri = Url::parse("file:///project/main.hew").unwrap();
        let document = analyze_document(&uri, "// \u{202E}\nfn main() {}\n", &DashMap::new(), &[]);
        let root_diags = document
            .diagnostics_by_uri
            .get(&uri)
            .expect("root document should always have diagnostics");

        assert!(
            root_diags.iter().any(|diagnostic| {
                diagnostic.source.as_deref() == Some("hew-types")
                    && diagnostic.severity == Some(DiagnosticSeverity::ERROR)
                    && diagnostic.code
                        == Some(NumberOrString::String(
                            "text_direction_codepoint_in_comment".to_string(),
                        ))
            }),
            "expected text_direction_codepoint_in_comment diagnostic: {root_diags:?}"
        );
    }

    #[test]
    fn analyze_document_honours_comment_lint_allow_directive() {
        let uri = Url::parse("file:///project/main.hew").unwrap();
        let source =
            "// hew:allow(text_direction_codepoint_in_comment)\n// \u{202E}\nfn main() {}\n";
        let document = analyze_document(&uri, source, &DashMap::new(), &[]);
        let root_diags = document
            .diagnostics_by_uri
            .get(&uri)
            .map(|diagnostics| diagnostics.as_slice())
            .unwrap_or(&[]);

        assert!(
            !root_diags.iter().any(|diagnostic| {
                diagnostic.code
                    == Some(NumberOrString::String(
                        "text_direction_codepoint_in_comment".to_string(),
                    ))
            }),
            "allow directive must suppress the source lint: {root_diags:?}"
        );
    }

    fn duplicate_node_module(span: Span) -> HirModule {
        let source = "fn main() -> i64 { 1 }\n";
        let parse_result = hew_parser::parse(source);
        let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
        let type_output = checker.check_program(&parse_result.program);
        let mut module =
            lower_program_host_target(&parse_result.program, &type_output, &ResolutionCtx).module;
        let HirItem::Function(function) = module.items.first_mut().expect("expected one function")
        else {
            panic!("expected a function item")
        };
        function.body.span = span.clone();
        if let Some(tail) = function.body.tail.as_mut() {
            tail.node = function.body.node;
            tail.span = span;
        }
        module
    }

    // ── Diagnostic.code tests ────────────────────────────────────────────

    #[test]
    fn type_diagnostic_code_is_set_to_kind_string() {
        // A type error (UndefinedVariable) should produce a diagnostic whose
        // `code` field is `Some(NumberOrString::String("UndefinedVariable"))`.
        let uri = Url::parse("file:///test.hew").unwrap();
        let source = "fn main() { missing_name }\n";
        let lo = compute_line_offsets(source);
        let parse_result = hew_parser::parse(source);
        let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
        let tc = checker.check_program(&parse_result.program);

        let diagnostics = build_diagnostics(&uri, source, &lo, &parse_result, Some(&tc));
        let type_diag = diagnostics
            .iter()
            .find(|d| d.source.as_deref() == Some("hew-types"))
            .expect("expected at least one hew-types diagnostic");

        assert!(
            type_diag.code.is_some(),
            "type diagnostic must carry a code; got None"
        );
        let code_str = match &type_diag.code {
            Some(NumberOrString::String(s)) => s.as_str(),
            other => panic!("expected NumberOrString::String, got {other:?}"),
        };
        assert_eq!(code_str, "UndefinedVariable");
    }

    #[test]
    fn parse_diagnostic_code_is_set_to_kind_string() {
        // A parse error should produce a diagnostic whose `code` field is
        // `Some(NumberOrString::String(<kind-string>))`.
        let uri = Url::parse("file:///test.hew").unwrap();
        let source = "fn main() { let x = ; }\n"; // missing expression after `=`
        let lo = compute_line_offsets(source);
        let parse_result = hew_parser::parse(source);

        // Source must contain at least one parse error for the test to be useful.
        assert!(
            !parse_result.errors.is_empty(),
            "test source must produce a parse error"
        );

        let diagnostics = build_diagnostics(&uri, source, &lo, &parse_result, None);
        let parse_diag = diagnostics
            .iter()
            .find(|d| d.source.as_deref() == Some("hew-parser"))
            .expect("expected at least one hew-parser diagnostic");

        assert!(
            parse_diag.code.is_some(),
            "parse diagnostic must carry a code; got None"
        );
        assert!(
            matches!(&parse_diag.code, Some(NumberOrString::String(_))),
            "parse diagnostic code must be a string variant"
        );
    }

    #[test]
    fn hir_diagnostic_code_is_set_to_kind_string() {
        // An HIR diagnostic should produce a Diagnostic whose `code` field
        // is `Some(NumberOrString::String("TaskCannotEscape"))`.
        let source = "fn main() {}\n";
        let lo = compute_line_offsets(source);
        let uri = Url::parse("file:///test.hew").unwrap();
        let diagnostics = vec![HirDiagnostic::new(
            HirDiagnosticKind::TaskCannotEscape,
            3..7,
            "",
        )];

        let by_uri = build_hir_lsp_diagnostics(&uri, source, &lo, &HashMap::new(), &diagnostics);
        let rendered = by_uri.get(&uri).expect("root diagnostic expected");
        let diag = rendered.first().expect("expected one HIR diagnostic");

        assert!(
            diag.code.is_some(),
            "HIR diagnostic must carry a code; got None"
        );
        match &diag.code {
            Some(NumberOrString::String(s)) => {
                assert_eq!(s, "TaskCannotEscape");
            }
            other => panic!("expected NumberOrString::String, got {other:?}"),
        }
    }

    // ── New v0.5 surface coverage (imported-stdlib method tooling) ───────
    //
    // These tests exercise the LSP coverage that depends on std-module
    // inlining in `populate_user_module_imports`: hover, inlay hints,
    // signature help, completion, and the absence of false "no method"
    // diagnostics for method surfaces on imported stdlib types. Before the
    // inlining fix, imported stdlib types (regex.Pattern, net.Listener, …)
    // were never registered in `type_defs`/`fn_sigs`, so completion and
    // signature help silently returned nothing and the checker's narrower
    // `ModuleRegistry` handle-method table even raised false diagnostics for
    // valid method calls (e.g. `regex.Pattern.captures`).

    /// Anchor a synthetic document at the repository root so tier-2 module
    /// search resolves the in-worktree `std/` tree (no env mutation, so the
    /// test is parallel-safe).
    fn analyze_repo_rooted(file_stem: &str, source: &str) -> DocumentState {
        let repo_root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("hew-lsp has a parent (repo root)")
            .to_path_buf();
        let path = repo_root.join(format!("{file_stem}.hew"));
        let uri = Url::from_file_path(&path).expect("repo-rooted path is absolute");
        let docs: DashMap<Url, DocumentState> = DashMap::new();
        analyze_document(&uri, source, &docs, &[])
    }

    fn surface_hover(doc: &DocumentState, source: &str, needle: &str) -> String {
        let off = source.find(needle).expect("needle present") + needle.len() - 1;
        hew_analysis::hover::hover(
            &doc.source,
            &doc.parse_result,
            doc.type_output.as_ref(),
            off,
        )
        .map(|h| h.contents)
        .unwrap_or_default()
    }

    fn surface_sighelp(doc: &DocumentState, source: &str, call: &str) -> Vec<String> {
        let cp = source.find(call).expect("call present");
        let paren = cp + source[cp..].find('(').expect("call has open paren") + 1;
        hew_analysis::signature_help::build_signature_help(
            &doc.source,
            doc.type_output.as_ref().expect("type output present"),
            paren,
        )
        .map(|s| s.signatures.into_iter().map(|x| x.label).collect())
        .unwrap_or_default()
    }

    fn surface_completions(doc: &DocumentState, source: &str, receiver_dot: &str) -> Vec<String> {
        let cp = source.find(receiver_dot).expect("receiver present");
        let dot = cp + source[cp..].find('.').expect("receiver has dot") + 1;
        hew_analysis::completions::complete(
            &doc.source,
            &doc.parse_result,
            doc.type_output.as_ref(),
            dot,
        )
        .into_iter()
        .map(|i| i.label)
        .collect()
    }

    fn surface_inlays(doc: &DocumentState) -> Vec<String> {
        hew_analysis::inlay_hints::build_inlay_hints(
            &doc.source,
            &doc.parse_result,
            doc.type_output.as_ref().expect("type output present"),
        )
        .into_iter()
        .map(|h| h.label)
        .collect()
    }

    fn hard_type_diagnostic(doc: &DocumentState) -> Option<String> {
        doc.type_output.as_ref().and_then(|tc| {
            tc.errors
                .iter()
                .find(|e| e.severity == Severity::Error)
                .map(|e| e.message.clone())
        })
    }

    #[test]
    fn coverage_regex_captures_surface() {
        // `captures` returns the stdlib `CaptureMatches` type and lives only in
        // the impl block (NOT the ModuleRegistry handle table), so it is the
        // sharpest regression guard for the inlining fix.
        let source = "import std::text::regex;\n\
                      fn probe(s: string) {\n\
                      \x20   let one = regex.new(\"a\");\n\
                      \x20   let caps = one.captures(s);\n\
                      \x20   one.free();\n\
                      }\n";
        let doc = analyze_repo_rooted("lsp_cov_regex", source);

        assert_eq!(
            hard_type_diagnostic(&doc),
            None,
            "regex.Pattern.captures must not raise a hard type diagnostic",
        );
        assert!(
            surface_hover(&doc, source, "one.captures").contains("CaptureMatches"),
            "hover over captures() should report the CaptureMatches return type",
        );
        let sig = surface_sighelp(&doc, source, "one.captures");
        assert!(
            sig.iter()
                .any(|s| s.contains("captures(input: string)") && s.contains("CaptureMatches")),
            "signature help should label captures(input: string) -> CaptureMatches, got: {sig:?}",
        );
        let completions = surface_completions(&doc, source, "one.captures");
        for expected in [
            "captures",
            "capture",
            "find_all",
            "find_all_submatch",
            "find",
        ] {
            assert!(
                completions.contains(&expected.to_string()),
                "completion on regex.Pattern should include `{expected}`, got: {completions:?}",
            );
        }
        assert!(
            surface_inlays(&doc)
                .iter()
                .any(|l| l.contains("CaptureMatches")),
            "inlay hint for `caps` should show CaptureMatches",
        );
    }

    #[test]
    fn coverage_http_accept_surface() {
        // The HTTP async server's `await listener.accept()` carrier surface
        // (NEW-2): receiver type is the imported `net.Listener`.
        let source = "import std::net;\n\
                      fn probe(addr: string) {\n\
                      \x20   let ln = net.listen(addr);\n\
                      \x20   let conn = ln.accept();\n\
                      }\n";
        let doc = analyze_repo_rooted("lsp_cov_net", source);

        assert_eq!(hard_type_diagnostic(&doc), None);
        assert!(
            surface_hover(&doc, source, "ln.accept").contains("Connection"),
            "hover over accept() should report the Connection return type",
        );
        let sig = surface_sighelp(&doc, source, "ln.accept");
        assert!(
            sig.iter()
                .any(|s| s.contains("accept()") && s.contains("Connection")),
            "signature help should label accept() -> Connection, got: {sig:?}",
        );
        let completions = surface_completions(&doc, source, "ln.accept");
        for expected in ["accept", "close"] {
            assert!(
                completions.contains(&expected.to_string()),
                "completion on net.Listener should include `{expected}`, got: {completions:?}",
            );
        }
        assert!(
            surface_inlays(&doc)
                .iter()
                .any(|l| l.contains("Connection")),
            "inlay hint for `conn` should show the Connection type",
        );
    }

    #[test]
    fn coverage_tls_ffi_result_surface() {
        // BUG-NET-3 std/net/tls FFI-result surface (free-function form, the
        // shape the accept fixture exercises: `tls.read(stream, n)`).
        let source = "import std::net::tls;\n\
                      fn probe() {\n\
                      \x20   let stream = tls.connect(\"h\", 443);\n\
                      \x20   let chunk = tls.read(stream, 16);\n\
                      }\n";
        let doc = analyze_repo_rooted("lsp_cov_tls", source);

        assert_eq!(hard_type_diagnostic(&doc), None);
        let hover = surface_hover(&doc, source, "tls.read");
        assert!(
            hover.contains("Result<bytes,") && hover.contains("NetError"),
            "hover over tls.read should report Result<bytes, net.NetError>, got: {hover}",
        );
        let sig = surface_sighelp(&doc, source, "tls.read");
        assert!(
            sig.iter()
                .any(|s| s.contains("read(") && s.contains("Result<bytes,")),
            "signature help should label tls.read -> Result<bytes, net.NetError>, got: {sig:?}",
        );
        assert!(
            surface_inlays(&doc)
                .iter()
                .any(|l| l.contains("Result<bytes,") && l.contains("NetError")),
            "inlay hint for `chunk` should show the Result<bytes, net.NetError> type",
        );
    }

    #[test]
    fn coverage_text_template_surface() {
        let source = "import std::text::template;\n\
                      fn probe(ctx: template.Ctx) {\n\
                      \x20   let out = template.render(\"hi\", ctx);\n\
                      }\n";
        let doc = analyze_repo_rooted("lsp_cov_template", source);

        assert_eq!(hard_type_diagnostic(&doc), None);
        let sig = surface_sighelp(&doc, source, "template.render");
        assert!(
            sig.iter()
                .any(|s| s.contains("render(") && s.contains("Result<string,")),
            "signature help should label template.render -> Result<string, ...>, got: {sig:?}",
        );
        assert!(
            surface_inlays(&doc)
                .iter()
                .any(|l| l.contains("Result<string,")),
            "inlay hint for `out` should show the Result<string, string> type",
        );
    }

    #[test]
    fn coverage_text_unicode_surface() {
        let source = "import std::text::unicode;\n\
                      fn probe(cp: i64) {\n\
                      \x20   let up = unicode.is_upper(cp);\n\
                      }\n";
        let doc = analyze_repo_rooted("lsp_cov_unicode", source);

        assert_eq!(hard_type_diagnostic(&doc), None);
        let sig = surface_sighelp(&doc, source, "unicode.is_upper");
        assert!(
            sig.iter()
                .any(|s| s.contains("is_upper(") && s.contains("bool")),
            "signature help should label unicode.is_upper -> bool, got: {sig:?}",
        );
        assert!(
            surface_inlays(&doc).iter().any(|l| l.contains("bool")),
            "inlay hint for `up` should show the bool type",
        );
    }

    #[test]
    fn coverage_typed_stream_await_recv_surface() {
        // NEW-7 typed streams: `await stream.recv()` over Stream<bytes>. The
        // Stream/Sink surface is builtin, so it needs no std inlining — this
        // test pins the await-suspending primitive coverage alongside the
        // imported surfaces.
        let source = "fn probe(s: Stream<bytes>) {\n\
                      \x20   let item = await s.recv();\n\
                      }\n";
        let doc = analyze_repo_rooted("lsp_cov_stream", source);

        assert_eq!(hard_type_diagnostic(&doc), None);
        assert!(
            surface_hover(&doc, source, "s.recv").contains("Option<bytes>"),
            "hover over await s.recv() should report Option<bytes>",
        );
        let sig = surface_sighelp(&doc, source, "s.recv");
        assert!(
            sig.iter()
                .any(|s| s.contains("recv()") && s.contains("Option<bytes>")),
            "signature help should label recv() -> Option<bytes>, got: {sig:?}",
        );
        let completions = surface_completions(&doc, source, "s.recv");
        for expected in ["recv", "try_recv"] {
            assert!(
                completions.contains(&expected.to_string()),
                "completion on Stream<bytes> should include `{expected}`, got: {completions:?}",
            );
        }
        assert!(
            surface_inlays(&doc)
                .iter()
                .any(|l| l.contains("Option<bytes>")),
            "inlay hint for `item` should show Option<bytes>",
        );
    }

    #[test]
    fn coverage_channel_recv_await_surface() {
        let source = "import std::channel::channel;\n\
                      actor Worker {\n\
                      \x20   receive fn run(unused: i64) {\n\
                      \x20       let (tx, rx): (channel.Sender<string>, channel.Receiver<string>) = channel.new(4);\n\
                      \x20       tx.send(\"ping\");\n\
                      \x20       tx.close();\n\
                      \x20       let item = await rx.recv();\n\
                      \x20   }\n\
                      }\n";
        let doc = analyze_repo_rooted("lsp_cov_channel_recv", source);

        assert_eq!(hard_type_diagnostic(&doc), None);
        assert!(
            surface_hover(&doc, source, "rx.recv").contains("Option<string>"),
            "hover over await rx.recv() should report Option<string>",
        );
        let sig = surface_sighelp(&doc, source, "rx.recv");
        assert!(
            sig.iter()
                .any(|s| s.contains("recv()") && s.contains("Option<string>")),
            "signature help should label recv() -> Option<string>, got: {sig:?}",
        );
        let completions = surface_completions(&doc, source, "rx.recv");
        for expected in ["recv", "try_recv", "close"] {
            assert!(
                completions.contains(&expected.to_string()),
                "completion on channel.Receiver<string> should include `{expected}`, got: {completions:?}",
            );
        }
        assert!(
            surface_inlays(&doc)
                .iter()
                .any(|l| l.contains("Option<string>")),
            "inlay hint for `item` should show Option<string>",
        );
    }

    #[test]
    fn coverage_remote_ask_surface() {
        let source = "record Job {\n\
                      \x20   n: i32,\n\
                      }\n\
                      actor Worker {\n\
                      \x20   receive fn run(job: Job) -> i64 { 21 }\n\
                      }\n\
                      impl ActorMsg for Worker {\n\
                      \x20   type Msg = Job;\n\
                      \x20   type Reply = i64;\n\
                      }\n\
                      actor Caller {\n\
                      \x20   receive fn run(peer: RemotePid<Worker>) {\n\
                      \x20       let result = peer.ask(Job { n: 9 }, 250);\n\
                      \x20   }\n\
                      }\n";
        let doc = analyze_repo_rooted("lsp_cov_remote_ask", source);

        assert_eq!(hard_type_diagnostic(&doc), None);
        let hover = surface_hover(&doc, source, "peer.ask");
        assert!(
            hover.contains("Result<i64,") && hover.contains("AskError"),
            "hover over RemotePid.ask should report Result<i64, AskError>, got: {hover}",
        );
        let sig = surface_sighelp(&doc, source, "peer.ask");
        assert!(
            sig.iter()
                .any(|s| s.contains("ask(") && s.contains("Result<Worker::Reply, AskError>")),
            "signature help should label ask(...) -> Result<Worker::Reply, AskError>, got: {sig:?}",
        );
        let completions = surface_completions(&doc, source, "peer.ask");
        for expected in ["ask", "send"] {
            assert!(
                completions.contains(&expected.to_string()),
                "completion on RemotePid<Worker> should include `{expected}`, got: {completions:?}",
            );
        }
        assert!(
            surface_inlays(&doc)
                .iter()
                .any(|l| l.contains("Result<i64,") && l.contains("AskError")),
            "inlay hint for `result` should show Result<i64, AskError>",
        );
    }

    #[test]
    fn coverage_scanner_surface() {
        let source = "import std::io::scanner;\n\
                      fn probe() {\n\
                      \x20   var sc = scanner.from_string(\"one two\");\n\
                      \x20   sc = scanner.with_split(sc, SplitMode::SplitWords);\n\
                      \x20   sc = scanner.scan(sc);\n\
                      \x20   let ok = scanner.has_next(sc);\n\
                      \x20   let token = scanner.text(sc);\n\
                      \x20   let words = scanner.words(\"one two\");\n\
                      }\n";
        let doc = analyze_repo_rooted("lsp_cov_scanner", source);

        assert_eq!(hard_type_diagnostic(&doc), None);
        assert!(
            surface_hover(&doc, source, "scanner.from_string").contains("Scanner"),
            "hover over scanner.from_string should report Scanner",
        );
        let sig = surface_sighelp(&doc, source, "scanner.with_split");
        assert!(
            sig.iter()
                .any(|s| s.contains("with_split(") && s.contains("Scanner")),
            "signature help should label with_split(...) -> Scanner, got: {sig:?}",
        );
        let completions = surface_completions(&doc, source, "scanner.from_string");
        for expected in [
            "from_string",
            "scan",
            "has_next",
            "text",
            "words",
            "SplitWords",
        ] {
            assert!(
                completions.contains(&expected.to_string()),
                "completion on std::io::scanner should include `{expected}`, got: {completions:?}",
            );
        }
        let inlays = surface_inlays(&doc);
        for expected in ["bool", "string", "Vec<string>"] {
            assert!(
                inlays.iter().any(|l| l.contains(expected)),
                "scanner inlays should include {expected}, got: {inlays:?}",
            );
        }
    }

    // ── LSP-vs-compiler resolver agreement (fail-closed on ambiguity) ────

    /// Build a uniquely-named temporary workspace from (relative-path,
    /// content) pairs and return its root. Mirrors the env-mutation-free,
    /// parallel-safe pattern used by the navigation tests.
    fn make_temp_workspace_dir(files: &[(&str, &str)]) -> std::path::PathBuf {
        static COUNTER: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
        let (secs, nanos) = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map_or((0u64, 0u32), |d| (d.as_secs(), d.subsec_nanos()));
        let root = std::env::temp_dir().join(format!(
            "hew-lsp-ambig-{:x}-{:x}-{}-{}",
            secs,
            nanos,
            std::process::id(),
            COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed),
        ));
        for (rel, content) in files {
            let path = root.join(rel);
            if let Some(parent) = path.parent() {
                std::fs::create_dir_all(parent).expect("create workspace dir");
            }
            std::fs::write(&path, content).expect("write workspace file");
        }
        root
    }

    /// A dotted import `a::b` that resolves BOTH to the package-directory form
    /// (`a/b/b.hew`) and the flat form (`a/b.hew`) is ambiguous. The compiler's
    /// resolver (`resolve_file_imports_internal`, hew-compile/src/lib.rs:1045-
    /// 1064) fails closed on exactly this shape: it collects every candidate
    /// that canonicalizes, deduplicates by canonical path, and rejects the
    /// import when more than one distinct path resolves. Before this fix the LSP
    /// stopped at the first readable candidate and silently bound one — so it
    /// would offer tooling/diagnostics for code the compiler rejects.
    ///
    /// This test pins the agreement: the LSP must NOT silently bind a candidate
    /// (the import stays unresolved) and must surface an ambiguity diagnostic
    /// whose message matches the compiler's wording.
    #[test]
    fn ambiguous_import_fails_closed_like_compiler() {
        let lib = "pub fn val() -> i64 { 1 }\n";
        let main_src = "import a::b;\n\nfn main() -> i64 { 0 }\n";
        let root = make_temp_workspace_dir(&[
            ("a/b/b.hew", lib),
            ("a/b.hew", lib),
            ("main.hew", main_src),
        ]);
        let main_uri =
            Url::from_file_path(root.join("main.hew")).expect("workspace path is absolute");

        let docs: DashMap<Url, DocumentState> = DashMap::new();
        let doc = analyze_document(&main_uri, main_src, &docs, &[]);

        let diags = doc
            .diagnostics_by_uri
            .get(&main_uri)
            .expect("diagnostics for the main document");
        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("is ambiguous") && d.message.contains("a::b")),
            "expected a fail-closed ambiguity diagnostic for `a::b` matching the compiler, got: {:?}",
            diags.iter().map(|d| &d.message).collect::<Vec<_>>()
        );

        // The import must be left UNRESOLVED — no silent local-first pick.
        let mut program = hew_parser::parse(main_src).program;
        let ambiguities = populate_user_module_imports(&main_uri, &mut program.items, &docs, &[]);
        assert_eq!(
            ambiguities.len(),
            1,
            "exactly one ambiguous import expected, got {ambiguities:?}"
        );
        let left_unresolved = program.items.iter().any(|(item, _)| {
            matches!(item, Item::Import(d) if d.path.join("::") == "a::b" && d.resolved_items.is_none())
        });
        assert!(
            left_unresolved,
            "ambiguous import must remain unresolved (fail closed), not silently bound"
        );

        let _ = std::fs::remove_dir_all(&root);
    }

    /// Negative control: when only ONE candidate form exists, the import is
    /// unambiguous and the LSP resolves it (no ambiguity, no fail-closed). This
    /// is the precise complement of `ambiguous_import_fails_closed_like_compiler`
    /// and guards against the fail-closed check over-rejecting valid imports —
    /// matching the compiler, which binds the single resolving candidate.
    #[test]
    fn unambiguous_import_resolves_without_failing_closed() {
        let lib = "pub fn val() -> i64 { 1 }\n";
        let main_src = "import a::b;\n\nfn main() -> i64 { 0 }\n";
        // Only the flat form exists this time.
        let root = make_temp_workspace_dir(&[("a/b.hew", lib), ("main.hew", main_src)]);
        let main_uri =
            Url::from_file_path(root.join("main.hew")).expect("workspace path is absolute");

        let docs: DashMap<Url, DocumentState> = DashMap::new();
        let mut program = hew_parser::parse(main_src).program;
        let ambiguities = populate_user_module_imports(&main_uri, &mut program.items, &docs, &[]);
        assert!(
            ambiguities.is_empty(),
            "single-candidate import must not be flagged ambiguous, got {ambiguities:?}"
        );
        let resolved = program.items.iter().any(|(item, _)| {
            matches!(item, Item::Import(d) if d.path.join("::") == "a::b" && d.resolved_items.is_some())
        });
        assert!(
            resolved,
            "unambiguous import must resolve (fail-closed check must not over-reject)"
        );

        let _ = std::fs::remove_dir_all(&root);
    }

    /// String-literal file imports (`import "lib.hew"`) are resolved
    /// LOCAL-ONLY, exactly as the compiler does: it canonicalizes only
    /// `source_dir.join(file_path)` and errors if that single local path is
    /// absent (hew-compile/src/lib.rs:939-949). Search-root (stdlib/global)
    /// expansion must NOT apply to file imports — a file import that is missing
    /// locally must fail the compiler's way (stay unresolved), never resolve
    /// from a stdlib/global root.
    #[test]
    fn file_import_resolves_local_only_not_from_search_root() {
        let lib = "pub fn val() -> i64 { 1 }\n";
        let main_src = "import \"lib.hew\";\n\nfn main() -> i64 { 0 }\n";
        // `lib.hew` exists ONLY under the search root, never next to `main.hew`.
        let local = make_temp_workspace_dir(&[("main.hew", main_src)]);
        let root = make_temp_workspace_dir(&[("lib.hew", lib)]);
        let main_path = local.join("main.hew");

        let docs: DashMap<Url, DocumentState> = DashMap::new();
        let mut program = hew_parser::parse(main_src).program;
        let mut ambiguities = Vec::new();
        populate_user_module_imports_impl(
            &main_path,
            &local,
            std::slice::from_ref(&root),
            &mut program.items,
            &docs,
            0,
            &mut ambiguities,
            &[],
        );

        assert!(
            ambiguities.is_empty(),
            "file import must never be reported ambiguous from a search root, got {ambiguities:?}"
        );
        let unresolved = program.items.iter().any(|(item, _)| {
            matches!(item, Item::Import(d) if d.file_path.as_deref() == Some("lib.hew") && d.resolved_items.is_none())
        });
        assert!(
            unresolved,
            "missing local file import must stay unresolved (compiler fails on the single local path), not resolve from a stdlib/global root"
        );

        let _ = std::fs::remove_dir_all(&local);
        let _ = std::fs::remove_dir_all(&root);
    }

    /// A file import present BOTH locally and under a search root must bind the
    /// LOCAL file and must NOT be reported ambiguous — file imports are
    /// local-only, so the search-root copy is invisible to the resolver. This
    /// is the complement of `file_import_resolves_local_only_not_from_search_root`
    /// and guards against the dotted-import ambiguity logic leaking into file
    /// imports.
    #[test]
    fn file_import_local_shadow_is_not_ambiguous() {
        let local_lib = "pub fn val() -> i64 { 1 }\n";
        let root_lib = "pub fn val() -> i64 { 2 }\n";
        let main_src = "import \"lib.hew\";\n\nfn main() -> i64 { 0 }\n";
        let local = make_temp_workspace_dir(&[("main.hew", main_src), ("lib.hew", local_lib)]);
        let root = make_temp_workspace_dir(&[("lib.hew", root_lib)]);
        let main_path = local.join("main.hew");

        let docs: DashMap<Url, DocumentState> = DashMap::new();
        let mut program = hew_parser::parse(main_src).program;
        let mut ambiguities = Vec::new();
        populate_user_module_imports_impl(
            &main_path,
            &local,
            std::slice::from_ref(&root),
            &mut program.items,
            &docs,
            0,
            &mut ambiguities,
            &[],
        );

        assert!(
            ambiguities.is_empty(),
            "a local+root file-import pair must not be reported ambiguous, got {ambiguities:?}"
        );
        let bound_local = program.items.iter().any(|(item, _)| {
            matches!(
                item,
                Item::Import(d)
                    if d.file_path.as_deref() == Some("lib.hew")
                        && d.resolved_source_paths == vec![local.join("lib.hew")]
            )
        });
        assert!(
            bound_local,
            "file import must bind the LOCAL file, never the search-root copy"
        );

        let _ = std::fs::remove_dir_all(&local);
        let _ = std::fs::remove_dir_all(&root);
    }

    /// `extra_pkg_paths` allows an import that does not resolve in the default
    /// search roots to be found in an explicitly-added directory, mirroring
    /// `hew check --pkg-path DIR`.
    ///
    /// The LSP appends `extra_pkg_paths` to its search roots and generates
    /// candidate paths as `<root>/<import_path>.hew` and
    /// `<root>/<import_path>/<last>.hew` — the same flat-path logic as for any
    /// other search root.  The test uses `acme::widgets` (a plain two-segment
    /// path) so the candidate is `<pkg_dir>/acme/widgets.hew`.
    #[test]
    fn extra_pkg_path_resolves_import_from_added_search_root() {
        let pkg_source = "pub fn widget_fn() -> i64 { 42 }\n";
        let main_src = "import acme::widgets;\nfn main() -> i64 { 0 }\n";

        // Package dir has acme/widgets.hew (flat-form candidate).
        let pkg_dir = make_temp_workspace_dir(&[("acme/widgets.hew", pkg_source)]);
        let project_dir = make_temp_workspace_dir(&[("main.hew", main_src)]);
        let main_uri =
            Url::from_file_path(project_dir.join("main.hew")).expect("project path is absolute");

        let docs: DashMap<Url, DocumentState> = DashMap::new();

        // Without the extra pkg path, the import is unresolved.
        let mut program = hew_parser::parse(main_src).program;
        let _ambiguities = populate_user_module_imports(&main_uri, &mut program.items, &docs, &[]);
        let without_extra = program.items.iter().any(|(item, _)| {
            matches!(item, Item::Import(d) if d.path.join("::") == "acme::widgets" && d.resolved_items.is_none())
        });
        assert!(
            without_extra,
            "without --pkg-path the import must not resolve from an unknown directory"
        );

        // With the extra pkg path, the import resolves.
        let mut program2 = hew_parser::parse(main_src).program;
        let ambiguities2 = populate_user_module_imports(
            &main_uri,
            &mut program2.items,
            &docs,
            std::slice::from_ref(&pkg_dir),
        );
        assert!(
            ambiguities2.is_empty(),
            "extra_pkg_path import must not be reported ambiguous: {ambiguities2:?}"
        );
        let with_extra = program2.items.iter().any(|(item, _)| {
            matches!(item, Item::Import(d) if d.path.join("::") == "acme::widgets" && d.resolved_items.is_some())
        });
        assert!(
            with_extra,
            "with --pkg-path, acme::widgets must resolve from the added package directory"
        );

        let _ = std::fs::remove_dir_all(&pkg_dir);
        let _ = std::fs::remove_dir_all(&project_dir);
    }

    /// Parity test: `import hew::template` must resolve against `--pkg-path`
    /// identically to `hew check --pkg-path DIR`.
    ///
    /// The compiler strips the `hew::` prefix when building candidates against
    /// `extra_pkg_path` (`hew-compile/src/lib.rs:1102-1110`), so it resolves
    /// `<pkg_root>/template/template.hew` for `import hew::template`.  The LSP
    /// must do the same; if it only generates the full-path candidate
    /// `<pkg_root>/hew/template/template.hew` the import silently stays
    /// unresolved while `hew check` accepts it — the lying-LSP class this change
    /// exists to eliminate.
    ///
    /// Layout mirrors how `hew package add hew::template` would install into a
    /// local package directory: `<pkg_root>/template/template.hew` with a bare
    /// Hew source file (no manifest needed for this resolution layer).
    #[test]
    fn extra_pkg_path_resolves_hew_prefixed_import_with_prefix_stripping() {
        let template_src = "pub fn apply() -> i64 { 1 }\n";
        let main_src = "import hew::template;\nfn main() -> i64 { 0 }\n";

        // Package dir layout: template/template.hew (directory form after
        // hew:: prefix is stripped, matching `hew-compile/src/lib.rs:1104-1105`).
        let pkg_dir = make_temp_workspace_dir(&[("template/template.hew", template_src)]);
        let project_dir = make_temp_workspace_dir(&[("main.hew", main_src)]);
        let main_uri =
            Url::from_file_path(project_dir.join("main.hew")).expect("project path is absolute");

        let docs: DashMap<Url, DocumentState> = DashMap::new();

        // Without --pkg-path: import must remain unresolved (no local template/ tree).
        let mut program = hew_parser::parse(main_src).program;
        let _ambiguities = populate_user_module_imports(&main_uri, &mut program.items, &docs, &[]);
        let without_extra = program.items.iter().any(|(item, _)| {
            matches!(item, Item::Import(d) if d.path.join("::") == "hew::template" && d.resolved_items.is_none())
        });
        assert!(
            without_extra,
            "without --pkg-path, hew::template must not resolve — no local template/ tree"
        );

        // With --pkg-path pointing at pkg_dir: the hew:: prefix is stripped and
        // the import resolves to <pkg_dir>/template/template.hew, exactly as
        // `hew check --pkg-path <pkg_dir>` does.
        let mut program2 = hew_parser::parse(main_src).program;
        let ambiguities2 = populate_user_module_imports(
            &main_uri,
            &mut program2.items,
            &docs,
            std::slice::from_ref(&pkg_dir),
        );
        assert!(
            ambiguities2.is_empty(),
            "hew::template with --pkg-path must not be reported ambiguous: {ambiguities2:?}"
        );
        let with_extra = program2.items.iter().any(|(item, _)| {
            matches!(item, Item::Import(d) if d.path.join("::") == "hew::template" && d.resolved_items.is_some())
        });
        assert!(
            with_extra,
            "with --pkg-path, import hew::template must resolve via hew:: prefix stripping \
             (candidate: <pkg_dir>/template/template.hew), matching hew check --pkg-path behaviour"
        );

        let _ = std::fs::remove_dir_all(&pkg_dir);
        let _ = std::fs::remove_dir_all(&project_dir);
    }
}
