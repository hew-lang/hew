//! Incremental query framework for analysis.
//!
//! [`SourceDatabase`] is the single choke point through which feature handlers
//! (hover, goto, references, rename, …) read parse and type-check output.
//! Callers set source text via [`SourceDatabase::set_source`]; queries like
//! [`SourceDatabase::parse`] and [`SourceDatabase::type_check`] memoise results
//! keyed by a monotonically increasing document version.
//!
//! The default in-memory implementation, [`InMemorySourceDatabase`], is what
//! analysis unit tests and the WASM tooling use. The LSP server provides its
//! own impl that bridges the existing `DashMap<Url, DocumentState>` into this
//! trait so hover/goto/etc. route through the same choke point as the tests.
//!
//! # Versioning and invalidation
//!
//! Each document has a `u64` version pulled from a database-wide atomic
//! counter. Setting a document's source bumps the version, which clears the
//! memoised parse and type-check outputs for that URI. Other documents are
//! untouched — invalidation is per-URI.
//!
//! # URI keys
//!
//! URIs are opaque strings. The trait does not depend on `url::Url` so
//! analysis unit tests can use `"file:///a.hew"` directly without pulling in
//! another dependency. The LSP layer converts `tower_lsp::lsp_types::Url` to
//! `String` via `Url::as_str().to_owned()` at the boundary.

use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{Arc, OnceLock, RwLock, RwLockReadGuard, RwLockWriteGuard};

use hew_parser::ParseResult;
use hew_types::module_registry::{build_module_search_paths, ModuleRegistry};
use hew_types::{Checker, TypeCheckOutput};

use crate::util::compute_line_offsets;

/// Opaque document identifier. In practice this is an LSP URI string, but the
/// trait does not constrain the format.
pub type DocumentUri = String;

/// Monotonically-increasing version number for a document. A new value is
/// issued by the database on every [`SourceDatabase::set_source`] call; it is
/// used as the cache key for derived queries.
pub type DocumentVersion = u64;

/// Read-only view of analysis inputs and derived queries.
///
/// Feature handlers must go through this trait rather than walking the AST
/// directly, so that caching and invalidation are centralised.
pub trait SourceDatabase: Send + Sync {
    /// Install or replace the source text for a document. Bumps the document's
    /// version and invalidates its cached derived queries.
    fn set_source(&self, uri: DocumentUri, text: String, version: i32);

    /// Remove a document from the database, dropping its cached outputs.
    fn invalidate(&self, uri: &str);

    /// Return the current source text for a document, or `None` if no source
    /// is installed.
    fn source(&self, uri: &str) -> Option<Arc<str>>;

    /// Return the current document version. Reads that observe a later
    /// version than the one they were derived from must be considered stale.
    fn document_version(&self, uri: &str) -> Option<DocumentVersion>;

    /// Return pre-computed line-offset table for the current source.
    fn line_offsets(&self, uri: &str) -> Option<Arc<[usize]>>;

    /// Parse the current source and memoise the result.
    fn parse(&self, uri: &str) -> Option<Arc<ParseResult>>;

    /// Type-check the document and memoise the result. Returns `None` when
    /// type-checking is skipped (e.g. the source failed to parse) or the URI
    /// is unknown.
    fn type_check(&self, uri: &str) -> Option<Arc<TypeCheckOutput>>;
}

/// Internal per-document entry. `OnceLock` memoises the derived outputs at
/// the current version; `set_source` replaces the whole entry so a later
/// version's derived outputs cannot observe an older source.
#[derive(Debug)]
struct SourceEntry {
    source: Arc<str>,
    version: DocumentVersion,
    /// The `version: i32` most recently supplied by the LSP layer; recorded
    /// for callers that need to round-trip it back out (e.g. when emitting
    /// versioned diagnostics). The database itself keys caches on
    /// [`Self::version`], not this field.
    external_version: i32,
    line_offsets: OnceLock<Arc<[usize]>>,
    parse: OnceLock<Arc<ParseResult>>,
    /// `OnceLock<Option<Arc<_>>>` so "not yet computed" (`OnceLock::get() is
    /// None`) is distinct from "computed and unavailable" (`Some(None)`,
    /// e.g. the source failed to parse).
    type_check: OnceLock<Option<Arc<TypeCheckOutput>>>,
}

impl SourceEntry {
    fn new(source: String, version: DocumentVersion, external_version: i32) -> Self {
        Self {
            source: Arc::from(source),
            version,
            external_version,
            line_offsets: OnceLock::new(),
            parse: OnceLock::new(),
            type_check: OnceLock::new(),
        }
    }
}

/// Default in-memory implementation of [`SourceDatabase`].
///
/// Parses lazily on the first [`parse`](SourceDatabase::parse) call; type-
/// checks a single document in isolation (no cross-module import resolution).
/// The LSP server uses its own trait implementation that reuses the existing
/// cross-module analysis pipeline; this impl is intended for analysis unit
/// tests and single-file tooling (WASM, CLI probes).
#[derive(Debug, Default)]
pub struct InMemorySourceDatabase {
    /// Per-URI state behind a single `RwLock`. Granularity is coarse but the
    /// access pattern is dominated by reads through `OnceLock`, so contention
    /// is limited to `set_source` / `invalidate` — both rare.
    entries: RwLock<HashMap<DocumentUri, Arc<SourceEntry>>>,
    /// Global version counter. Each `set_source` bumps it once.
    next_version: AtomicU64,
    /// Warn once if a poisoned lock is recovered so the operator has a visible
    /// signal without spamming every subsequent analysis request.
    poison_recovery_warned: AtomicBool,
}

impl InMemorySourceDatabase {
    /// Construct an empty database.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    fn warn_on_poison_recovery(&self, mode: &str) {
        if !self.poison_recovery_warned.swap(true, Ordering::SeqCst) {
            tracing::warn!("source db {mode} lock poisoned; recovering");
        }
    }

    fn read_entries(&self) -> RwLockReadGuard<'_, HashMap<DocumentUri, Arc<SourceEntry>>> {
        match self.entries.read() {
            Ok(guard) => guard,
            Err(poisoned) => {
                self.warn_on_poison_recovery("read");
                poisoned.into_inner()
            }
        }
    }

    fn write_entries(&self) -> RwLockWriteGuard<'_, HashMap<DocumentUri, Arc<SourceEntry>>> {
        match self.entries.write() {
            Ok(guard) => guard,
            Err(poisoned) => {
                self.warn_on_poison_recovery("write");
                poisoned.into_inner()
            }
        }
    }

    /// Return the caller-supplied `version: i32` that was installed with the
    /// current source, if any. Useful when the LSP layer needs to stamp
    /// diagnostics with the document version.
    #[must_use]
    pub fn external_version(&self, uri: &str) -> Option<i32> {
        self.read_entries()
            .get(uri)
            .map(|entry| entry.external_version)
    }

    fn entry(&self, uri: &str) -> Option<Arc<SourceEntry>> {
        self.read_entries().get(uri).cloned()
    }

    #[cfg(test)]
    fn poison_recovery_warned_for_test(&self) -> bool {
        self.poison_recovery_warned.load(Ordering::SeqCst)
    }
}

impl SourceDatabase for InMemorySourceDatabase {
    fn set_source(&self, uri: DocumentUri, text: String, version: i32) {
        let new_version = self.next_version.fetch_add(1, Ordering::SeqCst);
        let entry = Arc::new(SourceEntry::new(text, new_version, version));
        self.write_entries().insert(uri, entry);
    }

    fn invalidate(&self, uri: &str) {
        self.write_entries().remove(uri);
    }

    fn source(&self, uri: &str) -> Option<Arc<str>> {
        self.entry(uri).map(|e| Arc::clone(&e.source))
    }

    fn document_version(&self, uri: &str) -> Option<DocumentVersion> {
        self.entry(uri).map(|e| e.version)
    }

    fn line_offsets(&self, uri: &str) -> Option<Arc<[usize]>> {
        let entry = self.entry(uri)?;
        Some(Arc::clone(entry.line_offsets.get_or_init(|| {
            Arc::from(compute_line_offsets(&entry.source).into_boxed_slice())
        })))
    }

    fn parse(&self, uri: &str) -> Option<Arc<ParseResult>> {
        let entry = self.entry(uri)?;
        Some(Arc::clone(
            entry
                .parse
                .get_or_init(|| Arc::new(hew_parser::parse(&entry.source))),
        ))
    }

    fn type_check(&self, uri: &str) -> Option<Arc<TypeCheckOutput>> {
        let entry = self.entry(uri)?;
        let cached = entry.type_check.get_or_init(|| {
            let parse_result = Arc::clone(
                entry
                    .parse
                    .get_or_init(|| Arc::new(hew_parser::parse(&entry.source))),
            );
            let has_errors = parse_result
                .errors
                .iter()
                .any(|e| e.severity == hew_parser::Severity::Error);
            if has_errors {
                return None;
            }
            let mut checker = Checker::new(ModuleRegistry::new(build_module_search_paths()));
            let output = checker.check_program(&parse_result.program);
            Some(Arc::new(output))
        });
        cached.as_ref().map(Arc::clone)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[cfg(not(target_arch = "wasm32"))]
    use std::thread;

    const URI: &str = "file:///a.hew";

    fn db_with(source: &str) -> InMemorySourceDatabase {
        let db = InMemorySourceDatabase::new();
        db.set_source(URI.to_string(), source.to_string(), 1);
        db
    }

    #[test]
    fn parse_is_memoised_within_a_version() {
        let db = db_with("fn a() {}");
        let first = db.parse(URI).unwrap();
        let second = db.parse(URI).unwrap();
        assert!(
            Arc::ptr_eq(&first, &second),
            "parse must return the same Arc on repeated reads"
        );
    }

    #[test]
    fn set_source_bumps_version_and_invalidates_parse_cache() {
        let db = db_with("fn a() {}");
        let v1 = db.document_version(URI).unwrap();
        let parse1 = db.parse(URI).unwrap();

        db.set_source(URI.to_string(), "fn b() {}".to_string(), 2);

        let v2 = db.document_version(URI).unwrap();
        let parse2 = db.parse(URI).unwrap();
        assert_ne!(v1, v2, "version must advance on set_source");
        assert!(
            !Arc::ptr_eq(&parse1, &parse2),
            "parse cache must invalidate on set_source"
        );
    }

    #[test]
    fn invalidate_drops_entry() {
        let db = db_with("fn a() {}");
        assert!(db.parse(URI).is_some());
        db.invalidate(URI);
        assert!(db.parse(URI).is_none());
        assert!(db.source(URI).is_none());
        assert!(db.document_version(URI).is_none());
    }

    #[test]
    fn parse_failure_caches_the_failure_shape() {
        // An input with lexer-level errors still produces a `ParseResult` —
        // what we care about is that it is memoised rather than re-run.
        let db = db_with("fn !!broken");
        let first = db.parse(URI).unwrap();
        let second = db.parse(URI).unwrap();
        assert!(Arc::ptr_eq(&first, &second));
        assert!(
            !first.errors.is_empty(),
            "broken source should yield at least one parse diagnostic"
        );
    }

    #[test]
    fn type_check_returns_none_on_parse_error_and_caches_it() {
        let db = db_with("fn !!broken");
        assert!(db.type_check(URI).is_none());
        // Calling again must not re-run the check.
        assert!(db.type_check(URI).is_none());
    }

    #[test]
    fn type_check_is_memoised_when_successful() {
        let db = db_with("fn main() {}");
        let first = db.type_check(URI);
        let second = db.type_check(URI);
        match (first, second) {
            (Some(a), Some(b)) => {
                assert!(
                    Arc::ptr_eq(&a, &b),
                    "type_check must return the same Arc on repeated reads"
                );
            }
            (a, b) => panic!("expected both type_check calls to succeed; got {a:?} vs {b:?}"),
        }
    }

    #[test]
    fn line_offsets_are_memoised() {
        let db = db_with("fn a() {}\nfn b() {}\n");
        let first = db.line_offsets(URI).unwrap();
        let second = db.line_offsets(URI).unwrap();
        assert!(Arc::ptr_eq(&first, &second));
        assert_eq!(&*first, &[0, 10, 20]);
    }

    #[test]
    fn unknown_uri_returns_none() {
        let db = InMemorySourceDatabase::new();
        assert!(db.source("file:///missing.hew").is_none());
        assert!(db.parse("file:///missing.hew").is_none());
        assert!(db.type_check("file:///missing.hew").is_none());
        assert!(db.line_offsets("file:///missing.hew").is_none());
    }

    #[test]
    fn external_version_round_trips() {
        let db = db_with("fn a() {}");
        assert_eq!(db.external_version(URI), Some(1));
        db.set_source(URI.to_string(), "fn a() {}".to_string(), 42);
        assert_eq!(db.external_version(URI), Some(42));
    }

    #[test]
    fn independent_uris_do_not_share_cache() {
        let db = InMemorySourceDatabase::new();
        db.set_source("file:///a.hew".to_string(), "fn a() {}".to_string(), 1);
        db.set_source("file:///b.hew".to_string(), "fn b() {}".to_string(), 1);
        let a = db.parse("file:///a.hew").unwrap();
        let b = db.parse("file:///b.hew").unwrap();
        assert!(!Arc::ptr_eq(&a, &b));
    }

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn recovers_from_poisoned_entries_lock_and_warns_once() {
        let db = Arc::new(db_with("fn a() {}"));
        let db_for_writer = Arc::clone(&db);
        let joiner = thread::spawn(move || {
            let _guard = db_for_writer
                .entries
                .write()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            panic!("intentional poison");
        });
        assert!(joiner.join().is_err(), "writer thread must panic");
        assert!(
            db.entries.is_poisoned(),
            "writer panic must poison the lock"
        );
        assert!(
            !db.poison_recovery_warned_for_test(),
            "warn-once flag should be clear before recovery"
        );

        assert_eq!(
            db.external_version(URI),
            Some(1),
            "reads must recover and return the preserved entry"
        );
        assert!(
            db.poison_recovery_warned_for_test(),
            "first recovery should flip the warn-once flag"
        );
        assert_eq!(
            db.source(URI).as_deref(),
            Some("fn a() {}"),
            "subsequent reads must keep succeeding after recovery"
        );

        db.set_source(URI.to_string(), "fn b() {}".to_string(), 2);
        assert_eq!(
            db.source(URI).as_deref(),
            Some("fn b() {}"),
            "writes must also recover after the poison"
        );
        assert_eq!(db.external_version(URI), Some(2));
    }
}
