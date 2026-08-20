//! Interned, provenance-invariant compile-time identity (rc1-F1 stage A).
//!
//! The `IdentityTable` is the single minting authority for module identity in
//! a compile. It is owned by the checker, minted once at `check_program`
//! entry from the module graph, and published in `TypeCheckOutput` so later
//! pipeline stages consume identity instead of re-deriving it from spellings.
//!
//! Stage A scope: `ModuleId` interning (dedicated by canonical source, so a
//! dual-imported or root-vs-imported source resolves to ONE identity) and the
//! canonical fn-sig key mint for the root compilation unit. Later stages
//! (B-F of the identity-substrate lane) extend the table to declaration
//! `DefId`s are minted here and threaded through HIR/MIR/codegen. Downstream
//! code must carry these identities rather than reconstructing them from names.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::{DefId, NominalId};

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

/// The per-compile identity interner. One instance per `check_program` run;
/// published in `TypeCheckOutput`.
#[derive(Debug, Clone, Default)]
pub struct IdentityTable {
    entries: Vec<ModuleEntry>,
    by_source: HashMap<PathBuf, ModuleId>,
    by_path: HashMap<String, ModuleId>,
    root: Option<ModuleId>,
}

impl IdentityTable {
    #[must_use]
    pub fn new() -> Self {
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
    pub fn mint_module(&mut self, canonical_path: &str, sources: &[PathBuf]) -> ModuleId {
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
    /// programs without source paths): such a root keeps the legacy bare
    /// namespace by design.
    pub fn mint_root_module(&mut self, sources: &[PathBuf]) -> Option<ModuleId> {
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
    pub fn mint_source_file_module(&mut self, assembler: &str, source: &Path) -> ModuleId {
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
        self.root.map(|id| self.module_path(id))
    }

    /// Canonical fn-sig identity of a free function declared by the ROOT
    /// unit: `{root}.{name}` — the same key the declaration mints when its
    /// module is imported. `None` when no root identity was minted (bare
    /// legacy namespace) or when `name` is not a bare free-function spelling.
    #[must_use]
    pub fn root_fn_identity(&self, name: &str) -> Option<String> {
        if name.contains('.') || name.contains("::") {
            return None;
        }
        let root = self.root_module_path()?;
        Some(format!("{root}.{name}"))
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
    fn sourceless_root_keeps_bare_namespace() {
        let mut table = IdentityTable::new();
        assert_eq!(table.mint_root_module(&[]), None);
        assert_eq!(table.root_module_path(), None);
        assert_eq!(table.root_fn_identity("helper"), None);
    }

    #[test]
    fn root_fn_identity_rejects_non_free_fn_spellings() {
        let mut table = IdentityTable::new();
        table
            .mint_root_module(&[PathBuf::from("/x/prog.hew")])
            .expect("root mints");
        assert_eq!(
            table.root_fn_identity("helper").as_deref(),
            Some("prog.helper")
        );
        assert_eq!(table.root_fn_identity("Type::method"), None);
        assert_eq!(table.root_fn_identity("mod.helper"), None);
    }
}
