use std::collections::HashSet;
use std::path::{Path, PathBuf};

use dashmap::DashMap;
use hew_analysis::references::count_all_references;
use hew_analysis::symbols::build_document_symbols;
use hew_analysis::{util::compute_line_offsets, SymbolInfo};
use hew_parser::ast::{Attribute, Item};
use hew_parser::ParseResult;
use tower_lsp::lsp_types::{CodeLens, Command, Location, SymbolInformation, Url};

use super::analysis::source_for_path;
use super::convert::analysis_symbol_kind_to_lsp;
use super::{offset_range_to_lsp, span_to_range, DocumentState};

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

pub(super) fn collect_workspace_symbols(
    uri: &Url,
    source: &str,
    lo: &[usize],
    analysis_symbols: &[SymbolInfo],
    query: &str,
) -> Vec<SymbolInformation> {
    let mut symbols = Vec::new();
    let query_lower = query.to_lowercase();
    collect_symbol_matches(
        uri,
        source,
        lo,
        analysis_symbols,
        &query_lower,
        None,
        &mut symbols,
    );
    symbols
}

pub(super) fn collect_project_workspace_symbols(
    documents: &DashMap<Url, DocumentState>,
    workspace_roots: &[PathBuf],
    query: &str,
) -> Vec<SymbolInformation> {
    let mut symbols = Vec::new();
    let mut seen_paths = HashSet::new();

    for path in workspace_symbol_paths(documents, workspace_roots) {
        let normalized_path = normalize_workspace_path(&path);
        if !seen_paths.insert(normalized_path.clone()) {
            continue;
        }
        let Ok(uri) = Url::from_file_path(&normalized_path) else {
            continue;
        };

        if let Some(doc) = documents.get(&uri) {
            let analysis_symbols = build_document_symbols(&doc.source, &doc.parse_result);
            symbols.extend(collect_workspace_symbols(
                &uri,
                &doc.source,
                &doc.line_offsets,
                &analysis_symbols,
                query,
            ));
            continue;
        }

        let Some(source) = source_for_path(&normalized_path, documents) else {
            continue;
        };
        let parse_result = hew_parser::parse(&source);
        let line_offsets = compute_line_offsets(&source);
        let analysis_symbols = build_document_symbols(&source, &parse_result);
        symbols.extend(collect_workspace_symbols(
            &uri,
            &source,
            &line_offsets,
            &analysis_symbols,
            query,
        ));
    }

    symbols
}

#[expect(
    deprecated,
    reason = "SymbolInformation::deprecated field is deprecated in lsp-types"
)]
fn collect_symbol_matches(
    uri: &Url,
    source: &str,
    lo: &[usize],
    symbols: &[SymbolInfo],
    query_lower: &str,
    container_name: Option<&str>,
    out: &mut Vec<SymbolInformation>,
) {
    for symbol in symbols {
        if query_lower.is_empty() || symbol.name.to_lowercase().contains(query_lower) {
            out.push(SymbolInformation {
                name: symbol.name.clone(),
                kind: analysis_symbol_kind_to_lsp(symbol.kind),
                tags: None,
                deprecated: None,
                location: Location {
                    uri: uri.clone(),
                    range: offset_range_to_lsp(
                        source,
                        lo,
                        symbol.selection_span.start,
                        symbol.selection_span.end,
                    ),
                },
                container_name: container_name.map(str::to_string),
            });
        }
        collect_symbol_matches(
            uri,
            source,
            lo,
            &symbol.children,
            query_lower,
            Some(&symbol.name),
            out,
        );
    }
}

fn workspace_symbol_paths(
    documents: &DashMap<Url, DocumentState>,
    workspace_roots: &[PathBuf],
) -> Vec<PathBuf> {
    let mut paths = Vec::new();

    if workspace_roots.is_empty() {
        paths.extend(open_document_paths(documents));
        paths.sort();
        return paths;
    }

    for root in workspace_roots {
        paths.extend(collect_hew_files(root));
    }
    paths.extend(
        open_document_paths(documents)
            .into_iter()
            .filter(|path| path_is_under_workspace_root(path, workspace_roots)),
    );
    paths.sort();
    paths
}

fn open_document_paths(documents: &DashMap<Url, DocumentState>) -> Vec<PathBuf> {
    let mut paths: Vec<PathBuf> = documents
        .iter()
        .filter_map(|entry| entry.key().to_file_path().ok())
        .collect();
    paths.sort();
    paths
}

fn collect_hew_files(root: &Path) -> Vec<PathBuf> {
    // Wrapper error type that is `From<(PathBuf, io::Error)>` but discards both,
    // matching the original swallow-on-error semantics of workspace-symbol scan.
    struct Ignored;
    impl From<(PathBuf, std::io::Error)> for Ignored {
        fn from(_: (PathBuf, std::io::Error)) -> Self {
            Ignored
        }
    }

    let mut files = Vec::new();
    // I/O errors are silently ignored here — workspace-symbol scans are best-effort.
    // Rename disk scans use for_each_hew_file directly and propagate errors.
    let _ = for_each_hew_file::<Ignored, _>(root, |path| {
        files.push(path.to_path_buf());
        Ok(())
    });
    files
}

pub(super) fn should_skip_workspace_dir(path: &Path) -> bool {
    matches!(
        path.file_name().and_then(|name| name.to_str()),
        Some(".git" | "target" | ".worktree" | ".worktrees" | "worktrees")
    )
}

/// Find the workspace root for a URI: the nearest ancestor directory that
/// contains a `std/` subdirectory.  Returns `None` if no such ancestor exists.
///
/// This is the canonical workspace-root heuristic; both the import-path
/// resolver (`compute_import_path`) and the rename disk-scan use it.
pub(super) fn find_workspace_root_for_uri(uri: &Url) -> Option<PathBuf> {
    let mut dir = uri
        .to_file_path()
        .ok()
        .and_then(|p| p.parent().map(Path::to_path_buf));
    while let Some(d) = dir {
        if d.join("std").is_dir() {
            return Some(d);
        }
        dir = d.parent().map(Path::to_path_buf);
    }
    None
}

/// Walk every `*.hew` file under `root`, calling `f` for each one.
///
/// Traversal rules:
/// - Uses `symlink_metadata` so that symlinked directories are **never**
///   followed.  This prevents cycle-induced stack overflows on hostile
///   workspace layouts (issue #1290).
/// - Applies `should_skip_workspace_dir` to prune `.git`, `target`,
///   `.worktree`, `.worktrees`, and `worktrees` directories.
/// - I/O errors on `read_dir` or `symlink_metadata` are propagated as
///   `E` via `From<(PathBuf, io::Error)>`, carrying the path that
///   triggered the error so the caller can include it in the user-visible
///   message (#1288).
///
/// Returns `Ok(())` when all reachable files have been visited without
/// error, or `Err(e)` on the first I/O or visitor error.
pub(super) fn for_each_hew_file<E, F>(root: &Path, mut f: F) -> Result<(), E>
where
    E: From<(PathBuf, std::io::Error)>,
    F: FnMut(&Path) -> Result<(), E>,
{
    let mut stack = vec![root.to_path_buf()];

    while let Some(path) = stack.pop() {
        let metadata = std::fs::symlink_metadata(&path).map_err(|e| E::from((path.clone(), e)))?;

        // Symlinks are never followed — this is what prevents directory-symlink
        // cycles (issue #1290). `is_symlink()` is true for both file and dir
        // symlinks; we skip both so no I/O is done through a symlink.
        if metadata.is_symlink() {
            continue;
        }

        if metadata.is_file() {
            if path.extension().and_then(|ext| ext.to_str()) == Some("hew") {
                f(&path)?;
            }
            continue;
        }

        if !metadata.is_dir() {
            continue;
        }

        if should_skip_workspace_dir(&path) {
            continue;
        }

        let entries = std::fs::read_dir(&path).map_err(|e| E::from((path.clone(), e)))?;
        let mut children: Vec<PathBuf> = entries.flatten().map(|e| e.path()).collect();
        children.sort();
        // Push in reverse so we pop in sorted order (deterministic, mirrors
        // the existing collect_hew_files stack order).
        for child in children.into_iter().rev() {
            stack.push(child);
        }
    }

    Ok(())
}

fn path_is_under_workspace_root(path: &Path, workspace_roots: &[PathBuf]) -> bool {
    let normalized_path = normalize_workspace_path(path);
    workspace_roots
        .iter()
        .map(|root| normalize_workspace_path(root))
        .any(|root| normalized_path.starts_with(root))
}

fn normalize_workspace_path(path: &Path) -> PathBuf {
    std::fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf())
}
