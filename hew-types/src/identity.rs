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
//! `DefId`s minted here and thread the IDs through HIR/MIR/codegen; at that
//! point `DefId::new`/`NominalId::new` become mint-restricted to this module.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

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
