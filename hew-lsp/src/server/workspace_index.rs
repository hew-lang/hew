//! Cached, incrementally maintained workspace index for cross-file navigation,
//! workspace-wide references, and unopened-importer rename checks.
//!
//! Replaces per-request `O(#files)` disk rescans with lazy per-root population
//! and incremental updates on editor lifecycle events (`didOpen`, `didChange`,
//! `didSave`, `didClose`, `didChangeWatchedFiles`), paired with lightweight
//! `stat`/hash staleness checks so external disk edits are never served stale.

use std::collections::{BTreeMap, HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};
use std::time::{Duration, SystemTime};

use dashmap::DashMap;
use hew_analysis::util::compute_line_offsets;
use hew_analysis::{OffsetSpan, RenameError};
use hew_parser::ast::{ImportDecl, ImportSpec, Span};
use hew_parser::ParseResult;
use tower_lsp_server::ls_types::{FileChangeType, FileEvent, Range, Uri as Url};

use super::navigation::{
    collect_import_items, compute_import_uri, find_named_import_spans, NamedImportMatch,
};
use super::uri::{resolved_physical_path, same_source_file, source_file_key, FileUriExt};
use super::workspace::{
    find_workspace_root_for_uri, normalize_workspace_path, should_skip_workspace_dir,
};
use super::{offset_range_to_lsp, DocumentState};

/// Files whose mtime is within this window of when we indexed them are
/// verified by content hash on staleness sync so rapid external edits inside a
/// single filesystem timestamp tick cannot alias a cached entry (mirrors Git's
/// racy-clean rule).
const RACY_CLEAN_WINDOW: Duration = Duration::from_secs(2);

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
#[cfg(test)]
pub(super) struct WorkspaceIndexStats {
    pub(super) disk_parses: u64,
    pub(super) buffer_parses: u64,
    pub(super) root_walks: u64,
    pub(super) staleness_checks: u64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct DiskMetadata {
    modified: Option<SystemTime>,
    len: u64,
    #[cfg(unix)]
    ctime: i64,
    #[cfg(unix)]
    ctime_nsec: i64,
    #[cfg(unix)]
    ino: u64,
    #[cfg(unix)]
    mode: u32,
}

impl DiskMetadata {
    fn from_fs(meta: &std::fs::Metadata) -> Self {
        #[cfg(unix)]
        use std::os::unix::fs::MetadataExt as _;

        Self {
            modified: meta.modified().ok(),
            len: meta.len(),
            #[cfg(unix)]
            ctime: meta.ctime(),
            #[cfg(unix)]
            ctime_nsec: meta.ctime_nsec(),
            #[cfg(unix)]
            ino: meta.ino(),
            #[cfg(unix)]
            mode: meta.mode(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct DiskStamp {
    meta: DiskMetadata,
    content_hash: u64,
    verified_at: SystemTime,
}

impl DiskStamp {
    fn is_racy(&self) -> bool {
        self.meta.modified.is_none_or(|mtime| {
            !self
                .verified_at
                .duration_since(mtime)
                .is_ok_and(|age| age >= RACY_CLEAN_WINDOW)
        })
    }
}

#[derive(Debug, Clone)]
enum FileOrigin {
    Open {
        /// `true` when updated directly by LSP editor lifecycle notifications
        /// (`didOpen`/`didChange`/`didSave`); `false` when populated from a
        /// standalone `DashMap` fallback. Prevents debounced `DocumentState`
        /// snapshots from downgrading fresher `didChange` buffers.
        managed: bool,
        content_hash: u64,
        source: Arc<str>,
    },
    Disk {
        stamp: DiskStamp,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct IndexedNamedImport {
    target_key: Url,
    imported_uri: Url,
    imported_name: String,
    visible_name: String,
    import_name_span: OffsetSpan,
    visible_name_span: OffsetSpan,
    has_alias: bool,
}

#[derive(Debug, Clone)]
struct IndexedFile {
    /// Canonical file URI key (`source_file_key(&uri)`).
    key: Url,
    /// Editor-facing URI spelling.
    uri: Url,
    /// Deterministic sort key matching lexicographic URI ordering.
    sort_key: String,
    /// Resolved physical path on disk, if file-backed.
    path: Option<PathBuf>,
    origin: FileOrigin,
    /// Raw imports retained so newly opened unsaved buffers can resolve
    /// previously unresolved import edges without re-parsing the file.
    raw_imports: Vec<(ImportDecl, Span)>,
    has_unresolved_imports: bool,
    resolved_imports: Vec<IndexedNamedImport>,
    definitions: HashMap<String, Range>,
    identifiers: HashSet<String>,
}

#[derive(Debug, Default)]
struct IndexedRoot {
    /// Canonical keys of `.hew` files currently discovered on disk under this root.
    disk_keys: HashSet<Url>,
}

/// Candidate file returned by an inverted identifier lookup.
#[derive(Debug, Clone)]
pub(super) struct ReferenceCandidate {
    pub(super) uri: Url,
    pub(super) path: Option<PathBuf>,
    pub(super) open_source: Option<Arc<str>>,
    pub(super) definition_range: Option<Range>,
}

/// Unopened importer file returned for conflict checks.
#[derive(Debug, Clone)]
pub(super) struct UnopenedImporterFile {
    pub(super) uri: Url,
    pub(super) path: PathBuf,
}

#[derive(Debug, Default)]
struct IndexState {
    /// Workspace roots configured by LSP `initialize` (`spelled`, `canonical`).
    configured_roots: Vec<(PathBuf, PathBuf)>,
    /// Lazily populated per-root disk tracking, keyed by canonical root path.
    roots: HashMap<PathBuf, IndexedRoot>,
    /// Per-file indexed metadata, keyed by canonical `source_file_key(&uri)`.
    files: HashMap<Url, IndexedFile>,
    /// Reverse importer graph: `target_canonical_uri -> importing canonical URIs`.
    importers: HashMap<Url, HashSet<Url>>,
    /// Top-level / nested definition map:
    /// `symbol_name -> (sort_key -> (canonical_key, editor_uri, range))`.
    definitions: HashMap<String, BTreeMap<String, (Url, Url, Range)>>,
    /// Inverted identifier index: `ident -> canonical URIs containing ident`.
    identifiers: HashMap<String, HashSet<Url>>,
    #[cfg(test)]
    stats: WorkspaceIndexStats,
}

/// Cached, incrementally updated workspace index stored on `HewLanguageServer`.
#[derive(Debug, Default)]
pub(super) struct WorkspaceIndex {
    state: RwLock<IndexState>,
}

impl WorkspaceIndex {
    #[must_use]
    pub(super) fn new() -> Self {
        Self::default()
    }

    #[cfg(test)]
    pub(super) fn stats(&self) -> WorkspaceIndexStats {
        self.state
            .read()
            .map_or_else(|_| WorkspaceIndexStats::default(), |state| state.stats)
    }

    /// Record workspace roots from LSP `initialize` without eagerly walking disk.
    pub(super) fn set_roots(&self, roots: &[PathBuf]) {
        if let Ok(mut state) = self.state.write() {
            state.configured_roots = roots
                .iter()
                .map(|root| (root.clone(), normalize_workspace_path(root)))
                .collect();
        }
    }

    /// Resolve the workspace root governing `uri`: prefers the nearest ancestor
    /// containing `std/`, then falls back to the most specific configured
    /// workspace root containing `uri`.
    pub(super) fn resolve_root_for_uri(&self, uri: &Url) -> Option<PathBuf> {
        if let Some(root) = find_workspace_root_for_uri(uri) {
            return Some(root);
        }
        let path = uri.to_checked_file_path()?;
        let physical = resolved_physical_path(&path).unwrap_or_else(|_| path.into_owned());
        let state = self.state.read().ok()?;
        state
            .configured_roots
            .iter()
            .filter(|(_, canonical)| physical.starts_with(canonical))
            .max_by_key(|(_, canonical)| canonical.as_os_str().len())
            .map(|(spelled, _)| spelled.clone())
    }

    /// Incrementally index or update an open editor buffer (`didOpen` / `didChange`).
    pub(super) fn upsert_open_document(&self, uri: &Url, source: &str, open_uris: &[Url]) {
        let key = source_file_key(uri);
        let hash = content_hash(source);

        // Fast path: unchanged managed open buffer content.
        if let Ok(state) = self.state.read() {
            if let Some(existing) = state.files.get(&key) {
                if matches!(
                    existing.origin,
                    FileOrigin::Open {
                        managed: true,
                        content_hash: existing_hash,
                        ..
                    } if existing_hash == hash
                ) && existing.uri == *uri
                {
                    return;
                }
            }
        }

        let parse_result = hew_parser::parse(source);
        let line_offsets = compute_line_offsets(source);
        let indexed = build_indexed_file(
            uri,
            source,
            &line_offsets,
            &parse_result,
            FileOrigin::Open {
                managed: true,
                content_hash: hash,
                source: Arc::from(source),
            },
            open_uris,
        );

        if let Ok(mut state) = self.state.write() {
            let was_present = state.files.contains_key(&key);
            state.insert_file(indexed);
            #[cfg(test)]
            {
                state.stats.buffer_parses += 1;
            }
            if !was_present {
                state.re_resolve_unresolved_imports(open_uris);
            }
        }
    }

    /// Ensure any document present in `documents` that is not managed via LSP
    /// open/change notifications is indexed and up to date (supports standalone
    /// tests that insert into or update `DashMap<Url, DocumentState>` directly).
    ///
    /// Snapshots existing open state and releases `self.state` BEFORE iterating
    /// `documents` so the lock order never inverts against callers that hold a
    /// `DashMap` shard guard while querying the index.
    pub(super) fn sync_untracked_open_documents(&self, documents: &DashMap<Url, DocumentState>) {
        if documents.is_empty() {
            return;
        }
        // Value is `None` for LSP-managed buffers (never overwrite from
        // debounced `DocumentState`) and `Some(hash)` for unmanaged fallback
        // entries (refresh if the `DashMap` entry's content hash changed).
        let existing_open: HashMap<Url, Option<u64>> = {
            let Ok(state) = self.state.read() else {
                return;
            };
            state
                .files
                .iter()
                .filter_map(|(key, file)| match file.origin {
                    FileOrigin::Open { managed: true, .. } => Some((key.clone(), None)),
                    FileOrigin::Open {
                        managed: false,
                        content_hash,
                        ..
                    } => Some((key.clone(), Some(content_hash))),
                    FileOrigin::Disk { .. } => None,
                })
                .collect()
        };

        let open_uris: Vec<Url> = documents.iter().map(|entry| entry.key().clone()).collect();
        let mut to_insert = Vec::new();
        for entry in documents {
            let uri = entry.key();
            let key = source_file_key(uri);
            let doc = entry.value();
            let doc_hash = content_hash(&doc.source);
            if let Some(tracked) = existing_open.get(&key) {
                match tracked {
                    None => continue,
                    Some(prev_hash) if *prev_hash == doc_hash => continue,
                    Some(_) => {}
                }
            }
            to_insert.push(build_indexed_file(
                uri,
                &doc.source,
                &doc.line_offsets,
                &doc.parse_result,
                FileOrigin::Open {
                    managed: false,
                    content_hash: doc_hash,
                    source: Arc::from(doc.source.as_str()),
                },
                &open_uris,
            ));
        }
        if to_insert.is_empty() {
            return;
        }
        if let Ok(mut state) = self.state.write() {
            for file in to_insert {
                if state.files.get(&file.key).is_some_and(|existing| {
                    matches!(existing.origin, FileOrigin::Open { managed: true, .. })
                }) {
                    continue;
                }
                state.insert_file(file);
            }
            state.re_resolve_unresolved_imports(&open_uris);
        }
    }

    /// Update the index on `textDocument/didSave`.
    pub(super) fn did_save(
        &self,
        uri: &Url,
        saved_text: Option<&str>,
        is_open: bool,
        open_uris: &[Url],
    ) {
        if let Some(text) = saved_text {
            if is_open {
                self.upsert_open_document(uri, text, open_uris);
                return;
            }
        }
        if !is_open {
            self.refresh_single_disk_uri(uri, open_uris);
        }
    }

    /// Drop open-buffer overlay on `textDocument/didClose`, falling back to the
    /// file on disk if it exists, or removing the file from the index if it was
    /// unsaved / deleted.
    pub(super) fn close_document(&self, uri: &Url, open_uris: &[Url]) {
        let key = source_file_key(uri);
        if let Some(path) = uri.to_checked_file_path() {
            if let Ok(meta) = std::fs::metadata(&path) {
                if meta.is_file() {
                    if let Ok(source) = std::fs::read_to_string(&path) {
                        let hash = content_hash(&source);
                        let parse_result = hew_parser::parse(&source);
                        let line_offsets = compute_line_offsets(&source);
                        let indexed = build_indexed_file(
                            uri,
                            &source,
                            &line_offsets,
                            &parse_result,
                            FileOrigin::Disk {
                                stamp: DiskStamp {
                                    meta: DiskMetadata::from_fs(&meta),
                                    content_hash: hash,
                                    verified_at: SystemTime::now(),
                                },
                            },
                            open_uris,
                        );
                        if let Ok(mut state) = self.state.write() {
                            let was_present = state.files.contains_key(&key);
                            state.insert_file(indexed);
                            #[cfg(test)]
                            {
                                state.stats.disk_parses += 1;
                            }
                            state.record_disk_key_under_matching_roots(&key, &path);
                            if !was_present {
                                state.re_resolve_unresolved_imports(open_uris);
                            }
                        }
                        return;
                    }
                }
            }
        }

        if let Ok(mut state) = self.state.write() {
            state.remove_file(&key);
            for root in state.roots.values_mut() {
                root.disk_keys.remove(&key);
            }
        }
    }

    /// Incrementally update the index on `workspace/didChangeWatchedFiles`.
    pub(super) fn handle_watched_files(&self, events: &[FileEvent], open_uris: &[Url]) {
        for event in events {
            let key = source_file_key(&event.uri);
            let is_open = self.state.read().is_ok_and(|state| {
                state
                    .files
                    .get(&key)
                    .is_some_and(|file| matches!(file.origin, FileOrigin::Open { .. }))
            });
            if is_open {
                continue;
            }
            if event.typ == FileChangeType::DELETED {
                if let Ok(mut state) = self.state.write() {
                    state.remove_file(&key);
                    for root in state.roots.values_mut() {
                        root.disk_keys.remove(&key);
                    }
                }
                continue;
            }
            self.refresh_single_disk_uri(&event.uri, open_uris);
        }
    }

    fn is_under_skipped_workspace_dir(&self, uri: &Url, path: &Path) -> bool {
        let Some(root) = self.resolve_root_for_uri(uri) else {
            return false;
        };
        let canonical_root = normalize_workspace_path(&root);
        let physical = resolved_physical_path(path).unwrap_or_else(|_| path.to_path_buf());
        let Ok(relative) = physical.strip_prefix(&canonical_root) else {
            return false;
        };
        relative.parent().is_some_and(|parent| {
            parent
                .components()
                .any(|component| should_skip_workspace_dir(Path::new(component.as_os_str())))
        })
    }

    fn refresh_single_disk_uri(&self, uri: &Url, open_uris: &[Url]) {
        let Some(path) = uri.to_checked_file_path() else {
            return;
        };
        if path.extension().and_then(|ext| ext.to_str()) != Some("hew") {
            return;
        }
        if self.is_under_skipped_workspace_dir(uri, &path) {
            return;
        }
        let key = source_file_key(uri);
        let Ok(meta) = std::fs::symlink_metadata(&path) else {
            if let Ok(mut state) = self.state.write() {
                if state
                    .files
                    .get(&key)
                    .is_some_and(|file| matches!(file.origin, FileOrigin::Disk { .. }))
                {
                    state.remove_file(&key);
                    for root in state.roots.values_mut() {
                        root.disk_keys.remove(&key);
                    }
                }
            }
            return;
        };
        if meta.is_symlink() || !meta.is_file() {
            return;
        }
        let Ok(source) = std::fs::read_to_string(&path) else {
            return;
        };
        let hash = content_hash(&source);
        let disk_meta = DiskMetadata::from_fs(&meta);

        // Skip re-parse if cached disk entry has identical hash.
        if let Ok(mut state) = self.state.write() {
            if let Some(existing) = state.files.get_mut(&key) {
                if let FileOrigin::Disk { stamp } = &mut existing.origin {
                    if stamp.content_hash == hash {
                        stamp.meta = disk_meta;
                        stamp.verified_at = SystemTime::now();
                        return;
                    }
                } else {
                    return;
                }
            }
        }

        let parse_result = hew_parser::parse(&source);
        let line_offsets = compute_line_offsets(&source);
        let indexed = build_indexed_file(
            uri,
            &source,
            &line_offsets,
            &parse_result,
            FileOrigin::Disk {
                stamp: DiskStamp {
                    meta: disk_meta,
                    content_hash: hash,
                    verified_at: SystemTime::now(),
                },
            },
            open_uris,
        );
        if let Ok(mut state) = self.state.write() {
            if state
                .files
                .get(&key)
                .is_some_and(|file| matches!(file.origin, FileOrigin::Open { .. }))
            {
                return;
            }
            let was_present = state.files.contains_key(&key);
            state.insert_file(indexed);
            #[cfg(test)]
            {
                state.stats.disk_parses += 1;
            }
            state.record_disk_key_under_matching_roots(&key, &path);
            if !was_present {
                state.re_resolve_unresolved_imports(open_uris);
            }
        }
    }

    /// Look up the first workspace definition for `word` under `root` whose
    /// canonical URI is not in `excluded_keys`.
    pub(super) fn find_definition_in_root(
        &self,
        root: &Path,
        word: &str,
        excluded_keys: &HashSet<Url>,
        open_uris: &[Url],
    ) -> Option<(Url, Range)> {
        let mut state = self.state.write().ok()?;
        let canonical_root = normalize_workspace_path(root);
        let _ = state.sync_root(root, &canonical_root, false, open_uris);

        let root_disk_keys = state.roots.get(&canonical_root).map(|r| &r.disk_keys);
        let candidates = state.definitions.get(word)?;
        for (canonical_key, editor_uri, range) in candidates.values() {
            if excluded_keys.contains(canonical_key) {
                continue;
            }
            let in_root = root_disk_keys.is_some_and(|keys| keys.contains(canonical_key))
                || state
                    .files
                    .get(canonical_key)
                    .and_then(|file| file.path.as_deref())
                    .is_some_and(|path| path.starts_with(&canonical_root));
            if in_root {
                return Some((editor_uri.clone(), *range));
            }
        }
        None
    }

    /// Return all files under `root` that contain identifier `name` according
    /// to the inverted identifier index, sorted deterministically by URI.
    pub(super) fn reference_candidates_in_root(
        &self,
        root: &Path,
        name: &str,
        open_uris: &[Url],
    ) -> Vec<ReferenceCandidate> {
        let Ok(mut state) = self.state.write() else {
            return Vec::new();
        };
        let canonical_root = normalize_workspace_path(root);
        let _ = state.sync_root(root, &canonical_root, false, open_uris);

        let Some(keys) = state.identifiers.get(name) else {
            return Vec::new();
        };
        let root_disk_keys = state.roots.get(&canonical_root).map(|r| &r.disk_keys);
        let mut candidates: Vec<ReferenceCandidate> = keys
            .iter()
            .filter_map(|key| {
                let file = state.files.get(key)?;
                let in_root = root_disk_keys.is_some_and(|disk| disk.contains(key))
                    || file
                        .path
                        .as_deref()
                        .is_some_and(|path| path.starts_with(&canonical_root));
                if !in_root {
                    return None;
                }
                let open_source = match &file.origin {
                    FileOrigin::Open { source, .. } => Some(Arc::clone(source)),
                    FileOrigin::Disk { .. } => None,
                };
                Some(ReferenceCandidate {
                    uri: file.uri.clone(),
                    path: file.path.clone(),
                    open_source,
                    definition_range: file.definitions.get(name).copied(),
                })
            })
            .collect();
        candidates.sort_by(|left, right| left.uri.as_str().cmp(right.uri.as_str()));
        candidates
    }

    /// Return all unopened disk files under `root` that import `renamed_name`
    /// non-aliased from `definition_uri`. Propagates filesystem I/O errors as
    /// `RenameError::Io` so conflict detection stays fail-closed (#1288).
    pub(super) fn unopened_conflict_importers(
        &self,
        root: &Path,
        definition_uri: &Url,
        renamed_name: &str,
        open_uris: &HashSet<Url>,
    ) -> Result<Vec<UnopenedImporterFile>, RenameError> {
        let mut state = self.state.write().map_err(|_| RenameError::Io {
            path: root.display().to_string(),
            message: "workspace index lock poisoned".to_string(),
        })?;
        let open_list: Vec<Url> = open_uris.iter().cloned().collect();
        let canonical_root = normalize_workspace_path(root);
        state.sync_root(root, &canonical_root, true, &open_list)?;

        let def_key = source_file_key(definition_uri);
        let Some(importer_keys) = state.importers.get(&def_key) else {
            return Ok(Vec::new());
        };
        let root_disk_keys = state.roots.get(&canonical_root).map(|r| &r.disk_keys);
        let mut result = Vec::new();
        for importer_key in importer_keys {
            if !root_disk_keys.is_some_and(|disk| disk.contains(importer_key)) {
                continue;
            }
            let Some(file) = state.files.get(importer_key) else {
                continue;
            };
            if open_uris
                .iter()
                .any(|open| same_source_file(open, &file.uri))
                || same_source_file(&file.uri, definition_uri)
            {
                continue;
            }
            let imports_nonaliased = file.resolved_imports.iter().any(|imp| {
                imp.target_key == def_key && imp.imported_name == renamed_name && !imp.has_alias
            });
            if !imports_nonaliased {
                continue;
            }
            if let Some(path) = &file.path {
                result.push(UnopenedImporterFile {
                    uri: file.uri.clone(),
                    path: path.clone(),
                });
            }
        }
        result.sort_by(|left, right| left.uri.as_str().cmp(right.uri.as_str()));
        Ok(result)
    }

    /// Return pre-indexed [`NamedImportMatch`] records for all unopened disk
    /// files under `root` importing `renamed_name` (aliased or non-aliased)
    /// from `definition_uri`. Propagates I/O errors as `RenameError::Io`.
    pub(super) fn unopened_named_importers(
        &self,
        root: &Path,
        definition_uri: &Url,
        renamed_name: &str,
        open_uris: &HashSet<Url>,
    ) -> Result<Vec<NamedImportMatch>, RenameError> {
        let mut state = self.state.write().map_err(|_| RenameError::Io {
            path: root.display().to_string(),
            message: "workspace index lock poisoned".to_string(),
        })?;
        let open_list: Vec<Url> = open_uris.iter().cloned().collect();
        let canonical_root = normalize_workspace_path(root);
        state.sync_root(root, &canonical_root, true, &open_list)?;

        let def_key = source_file_key(definition_uri);
        let Some(importer_keys) = state.importers.get(&def_key) else {
            return Ok(Vec::new());
        };
        let root_disk_keys = state.roots.get(&canonical_root).map(|r| &r.disk_keys);
        let mut matches = Vec::new();
        for importer_key in importer_keys {
            if !root_disk_keys.is_some_and(|disk| disk.contains(importer_key)) {
                continue;
            }
            let Some(file) = state.files.get(importer_key) else {
                continue;
            };
            if open_uris
                .iter()
                .any(|open| same_source_file(open, &file.uri))
                || same_source_file(&file.uri, definition_uri)
            {
                continue;
            }
            for imp in &file.resolved_imports {
                if imp.target_key == def_key && imp.imported_name == renamed_name {
                    matches.push(NamedImportMatch {
                        importer_uri: file.uri.clone(),
                        imported_uri: imp.imported_uri.clone(),
                        imported_name: imp.imported_name.clone(),
                        visible_name: imp.visible_name.clone(),
                        import_name_span: imp.import_name_span,
                        visible_name_span: imp.visible_name_span,
                        has_alias: imp.has_alias,
                    });
                }
            }
        }
        matches.sort_by(|left, right| {
            left.importer_uri
                .as_str()
                .cmp(right.importer_uri.as_str())
                .then(
                    left.import_name_span
                        .start
                        .cmp(&right.import_name_span.start),
                )
        });
        Ok(matches)
    }
}

impl IndexState {
    fn remove_file(&mut self, key: &Url) {
        let Some(old) = self.files.remove(key) else {
            return;
        };
        for imp in &old.resolved_imports {
            if let Some(importers) = self.importers.get_mut(&imp.target_key) {
                importers.remove(key);
                if importers.is_empty() {
                    self.importers.remove(&imp.target_key);
                }
            }
        }
        for symbol in old.definitions.keys() {
            if let Some(entries) = self.definitions.get_mut(symbol) {
                entries.remove(&old.sort_key);
                if entries.is_empty() {
                    self.definitions.remove(symbol);
                }
            }
        }
        for ident in &old.identifiers {
            if let Some(uris) = self.identifiers.get_mut(ident) {
                uris.remove(key);
                if uris.is_empty() {
                    self.identifiers.remove(ident);
                }
            }
        }
    }

    fn insert_file(&mut self, file: IndexedFile) {
        let key = file.key.clone();
        self.remove_file(&key);

        for imp in &file.resolved_imports {
            self.importers
                .entry(imp.target_key.clone())
                .or_default()
                .insert(key.clone());
        }
        for (symbol, range) in &file.definitions {
            self.definitions.entry(symbol.clone()).or_default().insert(
                file.sort_key.clone(),
                (key.clone(), file.uri.clone(), *range),
            );
        }
        for ident in &file.identifiers {
            self.identifiers
                .entry(ident.clone())
                .or_default()
                .insert(key.clone());
        }
        self.files.insert(key, file);
    }

    fn record_disk_key_under_matching_roots(&mut self, key: &Url, path: &Path) {
        let physical = resolved_physical_path(path).unwrap_or_else(|_| path.to_path_buf());
        for (canonical_root, root_state) in &mut self.roots {
            if physical.starts_with(canonical_root) {
                root_state.disk_keys.insert(key.clone());
            }
        }
    }

    /// Re-resolve import targets for files that previously had unresolved
    /// imports (for example, when an unsaved imported buffer is opened later).
    fn re_resolve_unresolved_imports(&mut self, open_uris: &[Url]) {
        let keys_to_refresh: Vec<Url> = self
            .files
            .values()
            .filter(|file| file.has_unresolved_imports)
            .map(|file| file.key.clone())
            .collect();
        for key in keys_to_refresh {
            let Some(existing) = self.files.get(&key) else {
                continue;
            };
            let source_opt = match &existing.origin {
                FileOrigin::Open { source, .. } => Some(source.to_string()),
                FileOrigin::Disk { .. } => existing
                    .path
                    .as_deref()
                    .and_then(|path| std::fs::read_to_string(path).ok()),
            };
            let Some(source) = source_opt else {
                continue;
            };
            let (resolved, has_unresolved) =
                resolve_named_imports(&existing.uri, &source, &existing.raw_imports, open_uris);
            if resolved == existing.resolved_imports
                && has_unresolved == existing.has_unresolved_imports
            {
                continue;
            }
            let mut updated = existing.clone();
            updated.resolved_imports = resolved;
            updated.has_unresolved_imports = has_unresolved;
            self.insert_file(updated);
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum SyncAction {
    Clean,
    VerifyHash(u64),
    Missing,
}

impl IndexState {
    fn classify_disk_sync(&self, key: &Url, meta: &DiskMetadata) -> SyncAction {
        match self.files.get(key) {
            Some(file) => match &file.origin {
                FileOrigin::Open { .. } => SyncAction::Clean,
                FileOrigin::Disk { stamp } => {
                    if stamp.meta == *meta && !stamp.is_racy() {
                        SyncAction::Clean
                    } else {
                        SyncAction::VerifyHash(stamp.content_hash)
                    }
                }
            },
            None => SyncAction::Missing,
        }
    }

    /// Lazily populate `root` on first access, or perform lightweight stat/hash
    /// staleness sync on subsequent accesses so external disk edits, creations,
    /// deletions, and permission changes are reflected without re-parsing
    /// unchanged files.
    fn sync_root(
        &mut self,
        root: &Path,
        canonical_root: &Path,
        strict_io: bool,
        open_uris: &[Url],
    ) -> Result<(), RenameError> {
        let is_initial = !self.roots.contains_key(canonical_root);
        let discovered = discover_hew_files(root, strict_io)?;

        let mut current_disk_keys = HashSet::with_capacity(discovered.len());
        let mut to_upsert: Vec<IndexedFile> = Vec::new();

        for (spelled_path, meta) in discovered {
            let Some(uri) = Url::from_checked_file_path(&spelled_path) else {
                continue;
            };
            let key = source_file_key(&uri);
            current_disk_keys.insert(key.clone());

            match self.classify_disk_sync(&key, &meta) {
                SyncAction::Clean => {}
                SyncAction::VerifyHash(prev_hash) => {
                    let source = match std::fs::read_to_string(&spelled_path) {
                        Ok(source) => source,
                        Err(err) if strict_io => {
                            return Err(RenameError::from((spelled_path, err)));
                        }
                        Err(_) => {
                            self.remove_file(&key);
                            continue;
                        }
                    };
                    let hash = content_hash(&source);
                    if hash == prev_hash {
                        if let Some(file) = self.files.get_mut(&key) {
                            if let FileOrigin::Disk { stamp } = &mut file.origin {
                                stamp.meta = meta;
                                stamp.verified_at = SystemTime::now();
                            }
                        }
                        continue;
                    }
                    to_upsert.push(parse_disk_indexed_file(
                        &uri, &source, meta, hash, open_uris,
                    ));
                }
                SyncAction::Missing => {
                    let source = match std::fs::read_to_string(&spelled_path) {
                        Ok(source) => source,
                        Err(err) if strict_io => {
                            return Err(RenameError::from((spelled_path, err)));
                        }
                        Err(_) => continue,
                    };
                    let hash = content_hash(&source);
                    to_upsert.push(parse_disk_indexed_file(
                        &uri, &source, meta, hash, open_uris,
                    ));
                }
            }
        }

        // Remove disk entries that disappeared from disk (never touching open buffers).
        if let Some(prev_root) = self.roots.get(canonical_root) {
            let removed_keys: Vec<Url> = prev_root
                .disk_keys
                .difference(&current_disk_keys)
                .cloned()
                .collect();
            for key in removed_keys {
                if self
                    .files
                    .get(&key)
                    .is_some_and(|file| matches!(file.origin, FileOrigin::Disk { .. }))
                {
                    self.remove_file(&key);
                }
            }
        }

        #[cfg(test)]
        {
            self.stats.disk_parses += to_upsert.len() as u64;
            if is_initial {
                self.stats.root_walks += 1;
            } else {
                self.stats.staleness_checks += 1;
            }
        }
        #[cfg(not(test))]
        let _ = is_initial;

        let mut added_new_file = false;
        for file in to_upsert {
            if !self.files.contains_key(&file.key) {
                added_new_file = true;
            }
            self.insert_file(file);
        }

        self.roots.insert(
            canonical_root.to_path_buf(),
            IndexedRoot {
                disk_keys: current_disk_keys,
            },
        );
        if added_new_file {
            self.re_resolve_unresolved_imports(open_uris);
        }
        Ok(())
    }
}

fn parse_disk_indexed_file(
    uri: &Url,
    source: &str,
    meta: DiskMetadata,
    hash: u64,
    open_uris: &[Url],
) -> IndexedFile {
    let parse_result = hew_parser::parse(source);
    let line_offsets = compute_line_offsets(source);
    build_indexed_file(
        uri,
        source,
        &line_offsets,
        &parse_result,
        FileOrigin::Disk {
            stamp: DiskStamp {
                meta,
                content_hash: hash,
                verified_at: SystemTime::now(),
            },
        },
        open_uris,
    )
}

/// Walk `root` for `.hew` files returning `(path, DiskMetadata)` without
/// reading or parsing file contents.
///
/// Mirrors `workspace::for_each_hew_file` / `workspace::collect_hew_files`:
/// - The root itself may be a symlink; intra-tree symlinks are skipped (#1290).
/// - Skips `.git`, `target`, `.worktree`, `.worktrees`, and `worktrees`.
/// - When `strict_io` is `true`, propagates I/O errors as `RenameError::Io`
///   carrying the failing path (#1288). When `false`, skips unreadable entries
///   best-effort so one bad subtree never blocks navigation.
fn discover_hew_files(
    root: &Path,
    strict_io: bool,
) -> Result<Vec<(PathBuf, DiskMetadata)>, RenameError> {
    let canonical_root = std::fs::canonicalize(root).unwrap_or_else(|_| root.to_path_buf());
    let mut files = Vec::new();
    let mut stack = vec![root.to_path_buf()];
    let mut visited = HashSet::new();
    visited.insert(canonical_root);
    let mut first = true;

    while let Some(path) = stack.pop() {
        let metadata = if first {
            match std::fs::metadata(&path) {
                Ok(meta) => meta,
                Err(err) if strict_io => return Err(RenameError::from((path, err))),
                Err(_) => {
                    first = false;
                    continue;
                }
            }
        } else {
            match std::fs::symlink_metadata(&path) {
                Ok(meta) => meta,
                Err(err) if strict_io => return Err(RenameError::from((path, err))),
                Err(_) => continue,
            }
        };

        if !first && metadata.is_symlink() {
            continue;
        }
        first = false;

        if metadata.is_file() {
            if path.extension().and_then(|ext| ext.to_str()) == Some("hew") {
                files.push((path, DiskMetadata::from_fs(&metadata)));
            }
            continue;
        }

        if !metadata.is_dir() || should_skip_workspace_dir(&path) {
            continue;
        }

        let entries = match std::fs::read_dir(&path) {
            Ok(entries) => entries,
            Err(err) if strict_io => return Err(RenameError::from((path, err))),
            Err(_) => continue,
        };
        let mut children = Vec::new();
        for entry in entries {
            match entry {
                Ok(entry) => children.push(entry.path()),
                Err(err) if strict_io => return Err(RenameError::from((path, err))),
                Err(_) => {}
            }
        }
        children.sort();
        for child in children.into_iter().rev() {
            if let Ok(child_meta) = std::fs::symlink_metadata(&child) {
                if child_meta.is_symlink() {
                    continue;
                }
            }
            if let Ok(canonical_child) = std::fs::canonicalize(&child) {
                if visited.insert(canonical_child) {
                    stack.push(child);
                }
            } else {
                stack.push(child);
            }
        }
    }

    Ok(files)
}

fn build_indexed_file(
    uri: &Url,
    source: &str,
    line_offsets: &[usize],
    parse_result: &ParseResult,
    origin: FileOrigin,
    open_uris: &[Url],
) -> IndexedFile {
    let key = source_file_key(uri);
    let path = uri
        .to_checked_file_path()
        .map(|p| resolved_physical_path(&p).unwrap_or_else(|_| p.into_owned()));

    let mut definitions = HashMap::new();
    for (name, span) in hew_analysis::definition::collect_definitions(source, parse_result) {
        definitions
            .entry(name)
            .or_insert_with(|| offset_range_to_lsp(source, line_offsets, span.start, span.end));
    }

    let identifiers = extract_identifiers(source, &definitions);
    let raw_imports = collect_import_items(parse_result);
    let (resolved_imports, has_unresolved_imports) =
        resolve_named_imports(uri, source, &raw_imports, open_uris);

    IndexedFile {
        key,
        sort_key: uri.as_str().to_owned(),
        uri: uri.clone(),
        path,
        origin,
        raw_imports,
        has_unresolved_imports,
        resolved_imports,
        definitions,
        identifiers,
    }
}

fn resolve_named_imports(
    file_uri: &Url,
    source: &str,
    raw_imports: &[(ImportDecl, Span)],
    open_uris: &[Url],
) -> (Vec<IndexedNamedImport>, bool) {
    let mut resolved = Vec::new();
    let mut has_unresolved = false;

    for (import, item_span) in raw_imports {
        let Some(ImportSpec::Names(names)) = &import.spec else {
            continue;
        };
        let Some(resolved_uri) = compute_import_uri(file_uri, import, open_uris.iter().cloned())
        else {
            has_unresolved = true;
            continue;
        };
        let target_key = source_file_key(&resolved_uri);
        for import_name in names {
            let Some((import_name_span, visible_name_span)) =
                find_named_import_spans(source, item_span, import_name)
            else {
                continue;
            };
            let visible_name = import_name.alias.unwrap_or(import_name.name);
            resolved.push(IndexedNamedImport {
                target_key: target_key.clone(),
                imported_uri: resolved_uri.clone(),
                imported_name: import_name.name.to_string(),
                visible_name: visible_name.to_string(),
                import_name_span,
                visible_name_span,
                has_alias: import_name.alias.is_some(),
            });
        }
    }

    (resolved, has_unresolved)
}

fn extract_identifiers(source: &str, definitions: &HashMap<String, Range>) -> HashSet<String> {
    let mut idents = HashSet::new();
    for (token, _) in hew_lexer::lex(source) {
        match token {
            hew_lexer::Token::Identifier(name) => {
                idents.insert(name.to_owned());
            }
            hew_lexer::Token::InterpolatedString(raw) => {
                collect_identifier_words(raw, &mut idents);
            }
            _ => {}
        }
    }
    for name in definitions.keys() {
        idents.insert(name.clone());
    }
    idents
}

fn collect_identifier_words(text: &str, out: &mut HashSet<String>) {
    let mut start: Option<usize> = None;
    for (idx, ch) in text.char_indices() {
        let is_ident = ch.is_alphanumeric() || ch == '_';
        match (start, is_ident) {
            (None, true) if !ch.is_ascii_digit() => start = Some(idx),
            (Some(s), false) => {
                out.insert(text[s..idx].to_owned());
                start = None;
            }
            _ => {}
        }
    }
    if let Some(s) = start {
        out.insert(text[s..].to_owned());
    }
}

fn content_hash(source: &str) -> u64 {
    let mut hash: u64 = 0xcbf2_9ce4_8422_2325;
    for byte in source.as_bytes() {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(0x0100_0000_01b3);
    }
    hash
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_temp_dir(label: &str) -> PathBuf {
        static COUNTER: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
        let (secs, nanos) = SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map_or((0u64, 0u32), |d| (d.as_secs(), d.subsec_nanos()));
        let root = std::env::temp_dir().join(format!(
            "hew-ws-index-{label}-{secs:x}-{nanos:x}-{}-{}",
            std::process::id(),
            COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed),
        ));
        std::fs::create_dir_all(root.join("std")).expect("create workspace std");
        root
    }

    fn write_file(root: &Path, rel: &str, content: &str) -> (PathBuf, Url) {
        let path = root.join(rel);
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent).expect("create parent");
        }
        std::fs::write(&path, content).expect("write fixture");
        let uri = Url::from_checked_file_path(&path).expect("valid file URI");
        (path, uri)
    }

    #[test]
    fn lazy_population_and_cache_hits_avoid_reparsing_unchanged_files() {
        let root = make_temp_dir("cache-hit");
        let (_, builtins_uri) = write_file(
            &root,
            "std/builtins.hew",
            "pub fn println(value: dyn Display) {}\n",
        );
        let (_, main_uri) = write_file(&root, "main.hew", "fn main() {\n    println(42);\n}\n");
        let (_, other_uri) = write_file(&root, "other.hew", "pub fn helper() -> i64 { 1 }\n");

        let index = WorkspaceIndex::new();
        assert_eq!(index.stats().disk_parses, 0, "index must populate lazily");

        let excluded = HashSet::from([source_file_key(&main_uri)]);
        let found = index
            .find_definition_in_root(&root, "println", &excluded, &[])
            .expect("should resolve println from std/builtins.hew");
        assert!(same_source_file(&found.0, &builtins_uri));
        assert_eq!(index.stats().root_walks, 1);
        assert_eq!(index.stats().disk_parses, 3);

        // Second lookup and reference lookup must reuse the cached index with
        // zero additional parses.
        let found_again = index
            .find_definition_in_root(&root, "helper", &excluded, &[])
            .expect("should resolve helper from other.hew");
        assert!(same_source_file(&found_again.0, &other_uri));
        let refs = index.reference_candidates_in_root(&root, "println", &[]);
        assert_eq!(
            refs.len(),
            2,
            "only main.hew and std/builtins.hew mention println"
        );
        assert_eq!(
            index.stats().disk_parses,
            3,
            "unchanged files must not be re-parsed on subsequent queries"
        );
        assert_eq!(index.stats().root_walks, 1);
        assert_eq!(index.stats().staleness_checks, 2);

        let _ = std::fs::remove_dir_all(&root);
    }

    #[test]
    fn incremental_open_buffer_edit_and_close_fallback_to_disk() {
        let root = make_temp_dir("open-close");
        let (_, lib_uri) = write_file(&root, "lib.hew", "pub fn disk_sym() -> i64 { 1 }\n");
        let (_, main_uri) = write_file(&root, "main.hew", "fn main() {}\n");

        let index = WorkspaceIndex::new();
        let excluded = HashSet::from([source_file_key(&main_uri)]);

        // Populate lazily from disk.
        assert!(index
            .find_definition_in_root(&root, "disk_sym", &excluded, &[])
            .is_some());
        let initial_disk_parses = index.stats().disk_parses;

        // Open lib.hew and edit in memory without saving to disk.
        index.upsert_open_document(
            &lib_uri,
            "pub fn live_sym() -> i64 { 2 }\n",
            std::slice::from_ref(&lib_uri),
        );
        assert!(
            index
                .find_definition_in_root(
                    &root,
                    "live_sym",
                    &excluded,
                    std::slice::from_ref(&lib_uri)
                )
                .is_some(),
            "open buffer edit must be indexed immediately"
        );
        assert!(
            index
                .find_definition_in_root(
                    &root,
                    "disk_sym",
                    &excluded,
                    std::slice::from_ref(&lib_uri)
                )
                .is_none(),
            "stale disk definition must be replaced by open buffer"
        );
        assert_eq!(
            index.stats().disk_parses,
            initial_disk_parses,
            "open buffer edit must not trigger disk re-parses"
        );

        // Close lib.hew without saving — index must fall back to the disk file.
        index.close_document(&lib_uri, &[]);
        assert!(
            index
                .find_definition_in_root(&root, "disk_sym", &excluded, &[])
                .is_some(),
            "closing an unsaved buffer must restore the disk definition"
        );
        assert!(
            index
                .find_definition_in_root(&root, "live_sym", &excluded, &[])
                .is_none(),
            "unsaved buffer definition must disappear after close"
        );

        let _ = std::fs::remove_dir_all(&root);
    }

    #[test]
    fn external_disk_edit_create_and_delete_are_detected_without_watcher() {
        let root = make_temp_dir("external-disk");
        let (lib_path, lib_uri) = write_file(&root, "lib.hew", "pub fn alpha_fn() -> i64 { 1 }\n");
        let (_, main_uri) = write_file(&root, "main.hew", "fn main() {}\n");

        let index = WorkspaceIndex::new();
        let excluded = HashSet::from([source_file_key(&main_uri)]);

        assert!(index
            .find_definition_in_root(&root, "alpha_fn", &excluded, &[])
            .is_some());
        assert_eq!(index.stats().disk_parses, 2);

        // Overwrite lib.hew externally with the exact same byte length inside
        // the same filesystem timestamp tick.
        std::fs::write(&lib_path, "pub fn omega_fn() -> i64 { 1 }\n").expect("overwrite lib.hew");
        let found = index
            .find_definition_in_root(&root, "omega_fn", &excluded, &[])
            .expect("external same-length disk edit must invalidate cached entry");
        assert!(same_source_file(&found.0, &lib_uri));
        assert!(index
            .find_definition_in_root(&root, "alpha_fn", &excluded, &[])
            .is_none());
        assert_eq!(
            index.stats().disk_parses,
            3,
            "only the modified file should be re-parsed"
        );

        // Create a brand new file externally.
        let (extra_path, extra_uri) =
            write_file(&root, "extra.hew", "pub fn brand_new() -> i64 { 9 }\n");
        let found_new = index
            .find_definition_in_root(&root, "brand_new", &excluded, &[])
            .expect("externally created file must be indexed");
        assert!(same_source_file(&found_new.0, &extra_uri));
        assert_eq!(index.stats().disk_parses, 4);

        // Delete the file externally.
        std::fs::remove_file(&extra_path).expect("remove extra.hew");
        assert!(
            index
                .find_definition_in_root(&root, "brand_new", &excluded, &[])
                .is_none(),
            "deleted disk file must be evicted from the index"
        );

        let _ = std::fs::remove_dir_all(&root);
    }

    #[test]
    fn importer_graph_tracks_disk_importers_and_watched_file_updates() {
        let root = make_temp_dir("importers");
        let (_, util_uri) = write_file(&root, "util.hew", "pub fn greet() -> i64 { 1 }\n");
        let (consumer_path, consumer_uri) = write_file(
            &root,
            "consumer.hew",
            "import util.{ greet };\nfn run() -> i64 { greet() }\n",
        );
        let (_, unrelated_uri) = write_file(&root, "unrelated.hew", "fn noop() {}\n");

        let index = WorkspaceIndex::new();
        let open_uris = HashSet::from([util_uri.clone()]);

        let importers = index
            .unopened_conflict_importers(&root, &util_uri, "greet", &open_uris)
            .expect("importer lookup should succeed");
        assert_eq!(importers.len(), 1);
        assert!(same_source_file(&importers[0].uri, &consumer_uri));
        assert!(!same_source_file(&importers[0].uri, &unrelated_uri));

        // Remove the import from consumer.hew and notify via watched-files hook.
        std::fs::write(&consumer_path, "fn run() -> i64 { 0 }\n").expect("update consumer");
        index.handle_watched_files(
            &[FileEvent {
                uri: consumer_uri.clone(),
                typ: FileChangeType::CHANGED,
            }],
            std::slice::from_ref(&util_uri),
        );
        let importers_after = index
            .unopened_conflict_importers(&root, &util_uri, "greet", &open_uris)
            .expect("importer lookup after update");
        assert!(
            importers_after.is_empty(),
            "watched-file edit removing import must clear importer graph edge"
        );

        let _ = std::fs::remove_dir_all(&root);
    }

    #[test]
    fn external_file_creation_resolves_previously_unresolved_importers() {
        // Case A: target file created externally and discovered via sync_root.
        let root_a = make_temp_dir("late-target-sync");
        let (_, importer_a) = write_file(
            &root_a,
            "importer.hew",
            "import util.{ greet };\nfn run() -> i64 { greet() }\n",
        );

        let index_a = WorkspaceIndex::new();
        // Warm index before util.hew exists — importer.hew has an unresolved import.
        assert!(index_a
            .find_definition_in_root(&root_a, "greet", &HashSet::new(), &[])
            .is_none());

        // Now create util.hew on disk; sync_root must re-resolve importer.hew's edge.
        let (_, util_a) = write_file(&root_a, "util.hew", "pub fn greet() -> i64 { 1 }\n");
        let open_a = HashSet::from([util_a.clone()]);
        let conflicts_a = index_a
            .unopened_conflict_importers(&root_a, &util_a, "greet", &open_a)
            .expect("conflict importer query should succeed");
        assert_eq!(conflicts_a.len(), 1);
        assert!(same_source_file(&conflicts_a[0].uri, &importer_a));

        let named_a = index_a
            .unopened_named_importers(&root_a, &util_a, "greet", &open_a)
            .expect("named importer query should succeed");
        assert_eq!(named_a.len(), 1);
        assert!(same_source_file(&named_a[0].importer_uri, &importer_a));
        let _ = std::fs::remove_dir_all(&root_a);

        // Case B: target file created and announced via didChangeWatchedFiles.
        let root_b = make_temp_dir("late-target-watched");
        let (_, importer_b) = write_file(
            &root_b,
            "importer.hew",
            "import util.{ greet };\nfn run() -> i64 { greet() }\n",
        );

        let index_b = WorkspaceIndex::new();
        assert!(index_b
            .find_definition_in_root(&root_b, "greet", &HashSet::new(), &[])
            .is_none());

        let (_, util_b) = write_file(&root_b, "util.hew", "pub fn greet() -> i64 { 1 }\n");
        index_b.handle_watched_files(
            &[FileEvent {
                uri: util_b.clone(),
                typ: FileChangeType::CREATED,
            }],
            &[],
        );
        let open_b = HashSet::from([util_b.clone()]);
        let conflicts_b = index_b
            .unopened_conflict_importers(&root_b, &util_b, "greet", &open_b)
            .expect("watched creation must re-resolve importer edge");
        assert_eq!(conflicts_b.len(), 1);
        assert!(same_source_file(&conflicts_b[0].uri, &importer_b));
        let _ = std::fs::remove_dir_all(&root_b);
    }

    #[test]
    fn watched_files_ignores_skipped_workspace_directories() {
        // Place workspace root inside an outer directory named `worktrees` to
        // verify only components relative to the workspace root are pruned.
        let outer = make_temp_dir("outer-parent");
        let root = outer.join("worktrees").join("project");
        std::fs::create_dir_all(root.join("std")).expect("create project std");

        let (_, real_uri) = write_file(&root, "src/real.hew", "pub fn real_fn() -> i64 { 1 }\n");
        let (_, target_uri) = write_file(
            &root,
            "target/generated.hew",
            "pub fn target_fn() -> i64 { 2 }\n",
        );
        let (_, inner_wt_uri) = write_file(
            &root,
            "worktrees/nested.hew",
            "pub fn nested_wt_fn() -> i64 { 3 }\n",
        );

        let index = WorkspaceIndex::new();
        index.set_roots(std::slice::from_ref(&root));
        index.handle_watched_files(
            &[
                FileEvent {
                    uri: target_uri,
                    typ: FileChangeType::CREATED,
                },
                FileEvent {
                    uri: inner_wt_uri,
                    typ: FileChangeType::CREATED,
                },
            ],
            &[],
        );
        assert_eq!(
            index.stats().disk_parses,
            0,
            "watched-file events under target/ and worktrees/ must not be parsed"
        );

        index.handle_watched_files(
            &[FileEvent {
                uri: real_uri.clone(),
                typ: FileChangeType::CREATED,
            }],
            &[],
        );
        assert_eq!(
            index.stats().disk_parses,
            1,
            "normal source file inside a repo under an external worktrees/ parent must be indexed"
        );
        let found = index
            .find_definition_in_root(&root, "real_fn", &HashSet::new(), &[])
            .expect("real_fn should be found");
        assert!(same_source_file(&found.0, &real_uri));
        assert!(index
            .find_definition_in_root(&root, "target_fn", &HashSet::new(), &[])
            .is_none());
        assert!(index
            .find_definition_in_root(&root, "nested_wt_fn", &HashSet::new(), &[])
            .is_none());

        let _ = std::fs::remove_dir_all(&outer);
    }

    #[test]
    fn sync_untracked_open_documents_refreshes_unmanaged_and_preserves_managed() {
        let root = make_temp_dir("sync-untracked");
        let (_, a_uri) = write_file(&root, "a.hew", "pub fn disk_a() -> i64 { 1 }\n");
        let (_, b_uri) = write_file(&root, "b.hew", "pub fn disk_b() -> i64 { 1 }\n");

        let make_state = |src: &str| DocumentState {
            expanded_program: None,
            source: src.to_owned(),
            line_offsets: compute_line_offsets(src),
            parse_result: hew_parser::parse(src),
            type_output: None,
            dependency_uris: None,
            diagnostics_by_uri: HashMap::new(),
        };

        let documents = DashMap::new();
        documents.insert(a_uri.clone(), make_state("pub fn v1_a() -> i64 { 1 }\n"));
        documents.insert(b_uri.clone(), make_state("pub fn stale_b() -> i64 { 1 }\n"));

        let index = WorkspaceIndex::new();
        // b.hew is managed via LSP didChange with fresher content than `documents`.
        index.upsert_open_document(
            &b_uri,
            "pub fn fresh_b() -> i64 { 2 }\n",
            &[a_uri.clone(), b_uri.clone()],
        );

        // First sync indexes unmanaged a.hew (v1_a) without downgrading managed b.hew.
        index.sync_untracked_open_documents(&documents);
        assert!(index
            .find_definition_in_root(&root, "v1_a", &HashSet::new(), &[])
            .is_some());
        assert!(index
            .find_definition_in_root(&root, "fresh_b", &HashSet::new(), &[])
            .is_some());
        assert!(index
            .find_definition_in_root(&root, "stale_b", &HashSet::new(), &[])
            .is_none());

        // Direct DashMap update to unmanaged a.hew must refresh via content_hash.
        documents.insert(a_uri.clone(), make_state("pub fn v2_a() -> i64 { 3 }\n"));
        index.sync_untracked_open_documents(&documents);
        assert!(index
            .find_definition_in_root(&root, "v2_a", &HashSet::new(), &[])
            .is_some());
        assert!(index
            .find_definition_in_root(&root, "v1_a", &HashSet::new(), &[])
            .is_none());

        let _ = std::fs::remove_dir_all(&root);
    }
}
