//! Interned, provenance-invariant compile-time identity.
//!
//! The `IdentityTable` is the single minting authority for module and source
//! declaration identity in
//! a compile. It is owned by the checker, minted once at `check_program`
//! entry from the module graph, and published in `TypeCheckOutput` so later
//! pipeline stages consume identity instead of re-deriving it from spellings.
//!
//! A dual-imported or root-vs-imported source resolves to one module identity,
//! and an exact source declaration resolves to one `DefId`. Downstream code
//! must carry these identities rather than reconstructing them from names.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::{DefId, NominalId};

/// Closed classification of source declarations and source-owned child bodies.
///
/// Together with [`DeclarationOccurrence`], this distinguishes declarations
/// which share an enclosing item span (actor and machine children). Adding a
/// new source declaration form therefore requires an explicit identity design
/// choice instead of silently falling back to a name-derived identity.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DeclarationKind {
    Const,
    Function,
    ExternFunction,
    Type,
    TypeAlias,
    Record,
    Trait,
    TraitMethod,
    TypeMethod,
    ImplMethod,
    Actor,
    ActorInit,
    ActorReceive,
    ActorMethod,
    Supervisor,
    SupervisorBootstrap,
    Machine,
    MachineState,
    MachineEvent,
    MachineStateEntry,
    MachineStateExit,
    MachineTransition,
}

/// Exact source occurrence of a declaration.
///
/// `item_start..item_end` identifies the top-level parsed item in one source
/// module. `ordinal` is zero for the item itself and is the source-order index
/// among children of the same [`DeclarationKind`]. It is deliberately not a
/// display name: renamed import routes and aliases must converge.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DeclarationOccurrence {
    module: Option<ModuleId>,
    item_start: usize,
    item_end: usize,
    synthetic_item_ordinal: u32,
    kind: DeclarationKind,
    ordinal: u32,
}

impl DeclarationOccurrence {
    #[must_use]
    pub fn new(
        module: Option<ModuleId>,
        item_span: &std::ops::Range<usize>,
        kind: DeclarationKind,
        ordinal: usize,
    ) -> Self {
        Self::new_with_synthetic_ordinal(module, item_span, 0, kind, ordinal)
    }

    /// Construct an occurrence with an explicit discriminator for source-less
    /// AST inventories whose spans are synthetic (`0..0`). Real source spans
    /// deliberately ignore this value so route-dependent module assembly
    /// order can never split one physical declaration.
    ///
    /// # Panics
    ///
    /// Panics if either ordinal exceeds the compiler's `u32` occurrence
    /// representation.
    #[must_use]
    pub fn new_with_synthetic_ordinal(
        module: Option<ModuleId>,
        item_span: &std::ops::Range<usize>,
        synthetic_item_ordinal: usize,
        kind: DeclarationKind,
        ordinal: usize,
    ) -> Self {
        Self {
            module,
            item_start: item_span.start,
            item_end: item_span.end,
            synthetic_item_ordinal: if item_span.is_empty() {
                u32::try_from(synthetic_item_ordinal)
                    .expect("more than u32::MAX synthetic source declarations in one module")
            } else {
                0
            },
            kind,
            ordinal: u32::try_from(ordinal)
                .expect("more than u32::MAX same-kind child declarations in one item"),
        }
    }

    #[must_use]
    pub fn module(self) -> Option<ModuleId> {
        self.module
    }

    #[must_use]
    pub fn kind(self) -> DeclarationKind {
        self.kind
    }

    /// The parsed item's source span, for diagnostics that point back at an
    /// already-established declaration.
    #[must_use]
    pub fn span(self) -> std::ops::Range<usize> {
        self.item_start..self.item_end
    }
}

/// Two distinct source declarations claimed one canonical path.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DeclarationIdentityError {
    PathAlreadyDeclared {
        path: String,
        established_occurrence: DeclarationOccurrence,
        conflicting_occurrence: DeclarationOccurrence,
    },
}

impl std::fmt::Display for DeclarationIdentityError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::PathAlreadyDeclared { path, .. } => {
                write!(
                    f,
                    "declaration path `{path}` was claimed by two source declarations"
                )
            }
        }
    }
}

impl std::error::Error for DeclarationIdentityError {}

/// Unforgeable witness that a declaration path was resolved canonically.
///
/// Only this module can create a witness, so even crate-local code cannot
/// bypass the minting authority by calling `DefId::from_minted_path` directly.
#[derive(Debug)]
pub(crate) struct MintingAuthority(());

/// Mint a declaration identity from the canonical path determined by the
/// resolver/checker. This crate-private entry point is the only production
/// constructor that accepts a bare declaration path.
#[must_use]
pub(crate) fn mint_def_id(full_path: impl Into<String>) -> DefId {
    DefId::from_minted_path(full_path, MintingAuthority(()))
}

/// Mint a nominal identity from the canonical path determined by the
/// resolver/checker.
#[must_use]
pub(crate) fn mint_nominal_id(full_path: impl Into<String>) -> NominalId {
    NominalId::from_minted_declaration(mint_def_id(full_path))
}

/// The one migration escape hatch for downstream code that still reconstructs
/// identities. It exists only behind the intentionally alarming public
/// `legacy_reconstruct_from_full_path` methods on the identity types.
#[must_use]
pub(crate) fn legacy_reconstruct_def_id(full_path: impl Into<String>) -> DefId {
    mint_def_id(full_path)
}

#[must_use]
pub(crate) fn legacy_reconstruct_nominal_id(full_path: impl Into<String>) -> NominalId {
    mint_nominal_id(full_path)
}

#[cfg(any(test, feature = "test"))]
#[must_use]
pub(crate) fn test_def_id(full_path: impl Into<String>) -> DefId {
    mint_def_id(full_path)
}

#[cfg(any(test, feature = "test"))]
#[must_use]
pub(crate) fn test_nominal_id(full_path: impl Into<String>) -> NominalId {
    mint_nominal_id(full_path)
}

/// Interned handle for one source module, independent of how the module was
/// reached (root unit, import, alias, dual-import). Index into the owning
/// [`IdentityTable`]; meaningless across tables.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId(u32);

#[derive(Debug, Clone)]
struct ModuleEntry {
    /// Canonical dotted render (`std.process`, `mymod`). For every graph
    /// module this is its canonical dotted path; for the root unit it is the
    /// identity the same file would carry when imported (its file stem), so
    /// root and import compiles of one source mint one spelling.
    canonical_path: String,
}

const SYNTHETIC_ROOT_PATH: &str = "#synthetic-root";

#[derive(Debug, Clone)]
struct DeclarationEntry {
    occurrence: DeclarationOccurrence,
    declaration: DefId,
}

/// The per-compile identity interner. One instance per `check_program` run;
/// published in `TypeCheckOutput`.
#[derive(Debug, Clone, Default)]
pub(crate) struct IdentityTable {
    entries: Vec<ModuleEntry>,
    by_source: HashMap<PathBuf, ModuleId>,
    by_path: HashMap<String, ModuleId>,
    root: Option<ModuleId>,
    declarations: Vec<DeclarationEntry>,
    declarations_by_occurrence: HashMap<DeclarationOccurrence, usize>,
    declarations_by_path: HashMap<String, usize>,
}

impl IdentityTable {
    #[must_use]
    pub(crate) fn new() -> Self {
        Self::default()
    }

    /// Canonicalize a source path for interning. Falls back to the path as
    /// given when the file is not resolvable on this filesystem (synthetic
    /// test paths, embedded sources): identity must be deterministic, never
    /// dependent on `canonicalize` succeeding.
    fn intern_source_key(source: &Path) -> PathBuf {
        std::fs::canonicalize(source).unwrap_or_else(|_| source.to_path_buf())
    }

    /// Mint (or resolve) the identity of a graph module with canonical dotted
    /// path `canonical_path`. Dedupe axis is the canonical source: a second
    /// spelling reaching the same source resolves to the existing identity.
    pub(crate) fn mint_module(&mut self, canonical_path: &str, sources: &[PathBuf]) -> ModuleId {
        let source_key = sources.first().map(|s| Self::intern_source_key(s));
        if let Some(key) = &source_key {
            if let Some(existing) = self.by_source.get(key) {
                return *existing;
            }
        }
        if let Some(existing) = self.by_path.get(canonical_path) {
            return *existing;
        }
        self.insert(canonical_path.to_string(), source_key)
    }

    /// Mint the ROOT compilation unit's identity from its canonical source.
    ///
    /// Provenance invariance: if the root source is also reachable as a graph
    /// module (directly checking an importable file), the root REUSES that
    /// module's identity — the same declaration minted through either route
    /// carries one identity. Otherwise the render is the file stem, exactly
    /// the identity the file would carry when imported as a single-file
    /// module.
    ///
    /// String-keyed collision guard: while fn-sig registries remain
    /// string-keyed (until stage C interns declaration IDs), a root stem that
    /// collides with a DIFFERENT module's dotted path would merge two key
    /// namespaces. Disambiguate with a `#root` suffix — `#` cannot appear in
    /// a dotted import path, and the root identity is never displayed (root
    /// diagnostics render bare leaves).
    ///
    /// Returns `None` when the root has no source (synthetic roots, unit-test
    /// programs without source paths). Callers must establish the explicit
    /// synthetic-root occurrence authority instead.
    pub(crate) fn mint_root_module(&mut self, sources: &[PathBuf]) -> Option<ModuleId> {
        let source = sources.first()?;
        let source_key = Self::intern_source_key(source);
        if let Some(existing) = self.by_source.get(&source_key) {
            self.root = Some(*existing);
            return self.root;
        }
        let stem = source.file_stem()?.to_str()?;
        if stem.is_empty() {
            return None;
        }
        let render: String = stem
            .chars()
            .map(|c| {
                if c.is_ascii_alphanumeric() || c == '_' {
                    c
                } else {
                    '_'
                }
            })
            .collect();
        let render = if self.by_path.contains_key(&render) {
            format!("{render}#root")
        } else {
            render
        };
        let id = self.insert(render, Some(source_key));
        self.root = Some(id);
        self.root
    }

    /// Establish a non-colliding occurrence authority for a source-less root.
    /// Its display/canonical-name projection remains absent: the reserved
    /// path exists only to distinguish declaration occurrences.
    pub(crate) fn mint_synthetic_root(&mut self) -> ModuleId {
        if let Some(root) = self.root {
            return root;
        }
        let id = self.insert(SYNTHETIC_ROOT_PATH.to_string(), None);
        self.root = Some(id);
        id
    }

    fn insert(&mut self, canonical_path: String, canonical_source: Option<PathBuf>) -> ModuleId {
        let id = ModuleId(
            u32::try_from(self.entries.len()).expect("more than u32::MAX modules in one compile"),
        );
        if let Some(key) = canonical_source {
            self.by_source.insert(key, id);
        }
        self.by_path.insert(canonical_path.clone(), id);
        self.entries.push(ModuleEntry { canonical_path });
        id
    }

    /// Mint (or resolve) the identity of one SOURCE FILE of a directory
    /// module (rc1-F1 stage C). A directory module assembles its primary
    /// file plus every peer `.hew` file, and one file can be reached both as
    /// a peer and as its own imported module (`pkg/aaa.hew` via `import pkg`
    /// and via `import pkg::aaa`); the dedupe axis is the canonical source,
    /// so every route mints ONE identity for the file. When the file is not
    /// already a minted module's primary source, its render is
    /// `{assembler}.{stem}` — exactly the identity the file carries when
    /// imported directly. A render collision with a DIFFERENT module's path
    /// is disambiguated fail-closed (`#file` suffix, appended until the
    /// render is genuinely unique — a THIRD colliding file must not
    /// re-derive the second's suffixed render): a false split diagnoses
    /// loudly, a false merge would equate two declarations.
    pub(crate) fn mint_source_file_module(&mut self, assembler: &str, source: &Path) -> ModuleId {
        let source_key = Self::intern_source_key(source);
        if let Some(existing) = self.by_source.get(&source_key) {
            return *existing;
        }
        let stem: String = source
            .file_stem()
            .and_then(|stem| stem.to_str())
            .unwrap_or("file")
            .chars()
            .map(|c| {
                if c.is_ascii_alphanumeric() || c == '_' {
                    c
                } else {
                    '_'
                }
            })
            .collect();
        let mut render = format!("{assembler}.{stem}");
        while self.by_path.contains_key(&render) {
            render.push_str("#file");
        }
        self.insert(render, Some(source_key))
    }

    /// The minted module identity of a canonical source file, when any mint
    /// recorded that source.
    #[must_use]
    pub fn module_for_source(&self, source: &Path) -> Option<ModuleId> {
        self.by_source
            .get(&Self::intern_source_key(source))
            .copied()
    }

    /// Resolve a graph module's canonical dotted path to its interned identity.
    #[must_use]
    pub fn module_for_path(&self, canonical_path: &str) -> Option<ModuleId> {
        self.by_path.get(canonical_path).copied()
    }

    /// Canonical dotted render of a source file's minted identity.
    #[must_use]
    pub fn module_path_for_source(&self, source: &Path) -> Option<&str> {
        self.module_for_source(source)
            .map(|id| self.module_path(id))
    }

    /// Canonical dotted render of a minted module.
    #[must_use]
    pub fn module_path(&self, id: ModuleId) -> &str {
        &self.entries[id.0 as usize].canonical_path
    }

    /// The root compilation unit's identity, when one was minted.
    #[must_use]
    pub fn root_module(&self) -> Option<ModuleId> {
        self.root
    }

    /// Canonical dotted render of the root unit, when one was minted.
    #[must_use]
    pub fn root_module_path(&self) -> Option<&str> {
        self.root
            .map(|id| self.module_path(id))
            .filter(|path| *path != SYNTHETIC_ROOT_PATH)
    }

    /// Establish exactly one identity for a source declaration.
    ///
    /// Repeating the same claim is idempotent (the same source may be visited
    /// through multiple import routes). One source occurrence may be reachable
    /// under more than one canonical path — a directory module's peer file
    /// publishes its declarations both as `{assembler}.{name}` and, when the
    /// file is importable in its own right, as `{file}.{name}` — so a second
    /// spelling of an ESTABLISHED occurrence records another way to look the
    /// same declaration up. It never mints a second identity, and the
    /// declaration keeps the path it was first established under.
    ///
    /// Two DIFFERENT occurrences claiming one path is the dangerous direction:
    /// it would equate two declarations, so it is rejected.
    pub(crate) fn declare(
        &mut self,
        occurrence: DeclarationOccurrence,
        canonical_path: impl Into<String>,
    ) -> Result<DefId, DeclarationIdentityError> {
        let canonical_path = canonical_path.into();
        if let Some(&index) = self.declarations_by_occurrence.get(&occurrence) {
            let established = self.declarations[index].declaration.clone();
            if established.full_path() == canonical_path {
                return Ok(established);
            }
            match self.declarations_by_path.get(&canonical_path) {
                Some(&claimed) if claimed != index => {
                    return Err(DeclarationIdentityError::PathAlreadyDeclared {
                        path: canonical_path,
                        established_occurrence: self.declarations[claimed].occurrence,
                        conflicting_occurrence: occurrence,
                    });
                }
                Some(_) => {}
                None => {
                    self.declarations_by_path.insert(canonical_path, index);
                }
            }
            return Ok(established);
        }
        if let Some(&index) = self.declarations_by_path.get(&canonical_path) {
            let established = &self.declarations[index];
            return Err(DeclarationIdentityError::PathAlreadyDeclared {
                path: canonical_path,
                established_occurrence: established.occurrence,
                conflicting_occurrence: occurrence,
            });
        }

        let declaration = mint_def_id(canonical_path.clone());
        let index = self.declarations.len();
        self.declarations.push(DeclarationEntry {
            occurrence,
            declaration: declaration.clone(),
        });
        self.declarations_by_occurrence.insert(occurrence, index);
        self.declarations_by_path.insert(canonical_path, index);
        Ok(declaration)
    }

    /// Resolve an exact source occurrence while the checker still owns the
    /// mutable authority.
    #[must_use]
    pub(crate) fn declaration(&self, occurrence: DeclarationOccurrence) -> Option<&DefId> {
        self.declarations_by_occurrence
            .get(&occurrence)
            .map(|&index| &self.declarations[index].declaration)
    }

    /// Resolve a canonical declaration path established by the checker.
    /// This is a name-resolution index into the same authority, not a mint.
    #[must_use]
    pub fn declaration_by_path(&self, canonical_path: &str) -> Option<&DefId> {
        self.declarations_by_path
            .get(canonical_path)
            .map(|&index| &self.declarations[index].declaration)
    }

    /// Whether this source module already contributed declaration rows.
    ///
    /// Registry mirrors can re-parse source that the module graph already
    /// inventoried. They are lookup adapters, not a second source authority;
    /// callers use this predicate to avoid claiming a second set of spans for
    /// the same declarations.
    #[must_use]
    pub(crate) fn module_has_declarations(&self, module: ModuleId) -> bool {
        self.declarations
            .iter()
            .any(|entry| entry.occurrence.module() == Some(module))
    }

    /// Consume and freeze the checker-owned minting table for downstream
    /// publication. No mutable authority coexists with an accepted view.
    #[must_use]
    pub(crate) fn freeze(self) -> IdentityView {
        IdentityView {
            entries: self.entries,
            by_source: self.by_source,
            by_path: self.by_path,
            root: self.root,
            declarations: self.declarations,
            declarations_by_occurrence: self.declarations_by_occurrence,
            declarations_by_path: self.declarations_by_path,
        }
    }
}

/// Immutable checker-to-lowering identity handoff.
///
/// This type intentionally exposes lookup only. Downstream crates cannot mint
/// modules or declarations, nor turn a display spelling into a new `DefId`.
#[derive(Debug, Clone, Default)]
pub struct IdentityView {
    entries: Vec<ModuleEntry>,
    by_source: HashMap<PathBuf, ModuleId>,
    by_path: HashMap<String, ModuleId>,
    root: Option<ModuleId>,
    declarations: Vec<DeclarationEntry>,
    declarations_by_occurrence: HashMap<DeclarationOccurrence, usize>,
    declarations_by_path: HashMap<String, usize>,
}

impl IdentityView {
    #[must_use]
    pub fn module_for_source(&self, source: &Path) -> Option<ModuleId> {
        self.by_source
            .get(&IdentityTable::intern_source_key(source))
            .copied()
    }

    #[must_use]
    pub fn module_for_path(&self, canonical_path: &str) -> Option<ModuleId> {
        self.by_path.get(canonical_path).copied()
    }

    #[must_use]
    pub fn module_path(&self, id: ModuleId) -> &str {
        &self.entries[id.0 as usize].canonical_path
    }

    #[must_use]
    pub fn module_path_for_source(&self, source: &Path) -> Option<&str> {
        self.module_for_source(source)
            .map(|module| self.module_path(module))
    }

    #[must_use]
    pub fn root_module(&self) -> Option<ModuleId> {
        self.root
    }

    #[must_use]
    pub fn root_module_path(&self) -> Option<&str> {
        self.root
            .map(|module| self.module_path(module))
            .filter(|path| *path != SYNTHETIC_ROOT_PATH)
    }

    #[must_use]
    pub fn declaration(&self, occurrence: DeclarationOccurrence) -> Option<&DefId> {
        self.declarations_by_occurrence
            .get(&occurrence)
            .map(|&index| &self.declarations[index].declaration)
    }

    #[must_use]
    pub fn declaration_by_path(&self, canonical_path: &str) -> Option<&DefId> {
        self.declarations_by_path
            .get(canonical_path)
            .map(|&index| &self.declarations[index].declaration)
    }

    #[must_use]
    pub fn nominal(&self, occurrence: DeclarationOccurrence) -> Option<NominalId> {
        self.declaration(occurrence)
            .cloned()
            .map(NominalId::from_minted_declaration)
    }

    /// Every established declaration with its exact source occurrence, in
    /// mint order. Read-only: tests and tooling inventory the table with it;
    /// nothing downstream may derive a new identity from the rows.
    pub fn declarations(&self) -> impl Iterator<Item = (DeclarationOccurrence, &DefId)> {
        self.declarations
            .iter()
            .map(|entry| (entry.occurrence, &entry.declaration))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn same_source_root_and_module_intern_one_identity() {
        let mut table = IdentityTable::new();
        let source = PathBuf::from("/nonexistent/oracle_mod.hew");
        let module = table.mint_module("oracle_mod", std::slice::from_ref(&source));
        let root = table
            .mint_root_module(std::slice::from_ref(&source))
            .expect("root with a source mints an identity");
        assert_eq!(module, root, "one source = one identity, however reached");
        assert_eq!(table.module_path(root), "oracle_mod");
    }

    #[test]
    fn peer_file_mints_one_identity_across_assembly_routes() {
        let mut table = IdentityTable::new();
        let pkg_hew = PathBuf::from("/nonexistent/pkg/pkg.hew");
        let aaa_hew = PathBuf::from("/nonexistent/pkg/aaa.hew");
        table.mint_module("pkg", &[pkg_hew.clone(), aaa_hew.clone()]);
        let as_peer = table.mint_source_file_module("pkg", &aaa_hew);
        let as_module = table.mint_module("pkg.aaa", std::slice::from_ref(&aaa_hew));
        assert_eq!(as_peer, as_module, "one file = one identity, either route");
        assert_eq!(table.module_path(as_peer), "pkg.aaa");
        assert_eq!(table.module_path_for_source(&pkg_hew), Some("pkg"));
        assert_eq!(table.module_path_for_source(&aaa_hew), Some("pkg.aaa"));
    }

    #[test]
    fn source_file_render_colliding_with_distinct_module_is_disambiguated() {
        let mut table = IdentityTable::new();
        table.mint_module("pkg.aaa", &[PathBuf::from("/elsewhere/aaa.hew")]);
        let peer = table.mint_source_file_module("pkg", &PathBuf::from("/nonexistent/pkg/aaa.hew"));
        assert_eq!(
            table.module_path(peer),
            "pkg.aaa#file",
            "distinct sources must never merge under one render"
        );
    }

    /// Three distinct files whose stems sanitize to ONE render must mint
    /// three DISTINCT identities. A single-application disambiguator
    /// re-derives the same suffixed string for the third file and silently
    /// overwrites the second's `by_path` slot — a false merge on the very
    /// axis this table exists to make injective.
    #[test]
    fn repeated_render_collisions_mint_distinct_identities() {
        let mut table = IdentityTable::new();
        let a = table.mint_source_file_module("pkg", &PathBuf::from("/nonexistent/pkg/a-b.hew"));
        let b = table.mint_source_file_module("pkg", &PathBuf::from("/nonexistent/pkg/a+b.hew"));
        let c = table.mint_source_file_module("pkg", &PathBuf::from("/nonexistent/pkg/a.b.hew"));
        assert_ne!(a, b);
        assert_ne!(b, c);
        assert_ne!(a, c);
        let renders = [
            table.module_path(a).to_string(),
            table.module_path(b).to_string(),
            table.module_path(c).to_string(),
        ];
        let unique: std::collections::HashSet<&String> = renders.iter().collect();
        assert_eq!(
            unique.len(),
            3,
            "every colliding file must keep its own render, got {renders:?}"
        );
        for (id, render) in [(a, &renders[0]), (b, &renders[1]), (c, &renders[2])] {
            assert_eq!(
                table.module_path(id),
                render,
                "render lookup must stay stable after later mints"
            );
        }
    }

    #[test]
    fn root_stem_colliding_with_distinct_module_path_is_disambiguated() {
        let mut table = IdentityTable::new();
        table.mint_module("util", &[PathBuf::from("/imported/util.hew")]);
        let root = table
            .mint_root_module(&[PathBuf::from("/elsewhere/util.hew")])
            .expect("root mints");
        assert_eq!(
            table.module_path(root),
            "util#root",
            "distinct sources must not merge string-keyed namespaces"
        );
    }

    #[test]
    fn sourceless_root_has_occurrence_identity_but_keeps_bare_namespace() {
        let mut table = IdentityTable::new();
        assert_eq!(table.mint_root_module(&[]), None);
        assert_eq!(table.mint_synthetic_root(), table.root_module().unwrap());
        assert_eq!(table.root_module_path(), None);
    }

    #[test]
    fn declaration_routes_converge_on_source_occurrence() {
        let mut table = IdentityTable::new();
        let source = PathBuf::from("/nonexistent/pkg/worker.hew");
        let imported = table.mint_module("pkg.worker", std::slice::from_ref(&source));
        let rooted = table
            .mint_root_module(std::slice::from_ref(&source))
            .expect("source-backed root");
        assert_eq!(imported, rooted);
        let occurrence =
            DeclarationOccurrence::new(Some(imported), &(10..40), DeclarationKind::Function, 0);
        let first = table
            .declare(occurrence, "pkg.worker.run")
            .expect("first claim");
        let second = table
            .declare(occurrence, "pkg.worker.run")
            .expect("same route-independent claim");
        assert_eq!(first, second);
        assert_eq!(table.freeze().declaration(occurrence), Some(&first));
    }

    #[test]
    fn same_leaf_in_different_modules_has_distinct_identity() {
        let mut table = IdentityTable::new();
        let left = table.mint_module("left", &[PathBuf::from("/nonexistent/left.hew")]);
        let right = table.mint_module("right", &[PathBuf::from("/nonexistent/right.hew")]);
        let left_occurrence =
            DeclarationOccurrence::new(Some(left), &(0..8), DeclarationKind::Type, 0);
        let right_occurrence =
            DeclarationOccurrence::new(Some(right), &(0..8), DeclarationKind::Type, 0);
        let left_id = table.declare(left_occurrence, "left.Item").unwrap();
        let right_id = table.declare(right_occurrence, "right.Item").unwrap();
        assert_ne!(left_id, right_id);
    }

    #[test]
    fn a_second_path_for_one_occurrence_is_another_spelling_not_another_identity() {
        let mut table = IdentityTable::new();
        let module = table.mint_module("m", &[PathBuf::from("/nonexistent/m.hew")]);
        let occurrence =
            DeclarationOccurrence::new(Some(module), &(0..8), DeclarationKind::Function, 0);
        let established = table.declare(occurrence, "m.run").unwrap();
        // A peer file reached through its assembler and through its own import
        // publishes two keys for one physical declaration.
        assert_eq!(table.declare(occurrence, "pkg.m.run").unwrap(), established);
        let view = table.freeze();
        assert_eq!(view.declaration_by_path("m.run"), Some(&established));
        assert_eq!(view.declaration_by_path("pkg.m.run"), Some(&established));
        assert_eq!(
            established.full_path(),
            "m.run",
            "the declaration keeps the path it was established under"
        );
    }

    #[test]
    fn two_declarations_claiming_one_path_fail_closed() {
        let mut table = IdentityTable::new();
        let module = table.mint_module("m", &[PathBuf::from("/nonexistent/m.hew")]);
        let first = DeclarationOccurrence::new(Some(module), &(0..8), DeclarationKind::Function, 0);
        let second =
            DeclarationOccurrence::new(Some(module), &(9..17), DeclarationKind::Function, 0);
        table.declare(first, "m.run").unwrap();
        assert!(matches!(
            table.declare(second, "m.run"),
            Err(DeclarationIdentityError::PathAlreadyDeclared { .. })
        ));
        assert!(table.freeze().declaration(second).is_none());
        // A second occurrence must not be able to steal a spelling either.
        let mut table = IdentityTable::new();
        let module = table.mint_module("m", &[PathBuf::from("/nonexistent/m.hew")]);
        let first = DeclarationOccurrence::new(Some(module), &(0..8), DeclarationKind::Function, 0);
        let second =
            DeclarationOccurrence::new(Some(module), &(9..17), DeclarationKind::Function, 0);
        table.declare(first, "m.run").unwrap();
        table.declare(second, "m.stop").unwrap();
        assert!(matches!(
            table.declare(second, "m.run"),
            Err(DeclarationIdentityError::PathAlreadyDeclared { .. })
        ));
    }
}
