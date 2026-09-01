//! Module graph types for the Hew compiler.
//!
//! Represents the structure of a multi-module compilation: each source file
//! becomes a [`Module`], and the edges between them (imports) form a
//! [`ModuleGraph`].  The graph carries a topological ordering so that
//! downstream passes can process modules in dependency order.

use std::collections::{BTreeMap, HashMap};
use std::fmt;
use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

use crate::ast::{ImportSpec, Item, Span, Spanned};

// ── ModuleId ─────────────────────────────────────────────────────────

/// Unique identifier for a module, based on its path segments
/// (e.g. `["std", "net", "http"]` for `std::net::http`).
#[derive(Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Serialize, Deserialize)]
pub struct ModuleId {
    pub path: Vec<String>,
}

impl ModuleId {
    #[must_use]
    pub fn new(path: Vec<String>) -> Self {
        Self { path }
    }

    /// Create a root module id (empty path).
    #[must_use]
    pub fn root() -> Self {
        Self { path: Vec::new() }
    }
}

impl fmt::Display for ModuleId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.path.is_empty() {
            write!(f, "(root)")
        } else {
            write!(f, "{}", self.path.join("::"))
        }
    }
}

// ── Module ───────────────────────────────────────────────────────────

/// A single module in the module graph.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Module {
    pub id: ModuleId,
    /// Items defined directly in this module.
    pub items: Vec<Spanned<Item>>,
    /// Imports declared in this module.
    pub imports: Vec<ModuleImport>,
    /// Source file paths (one for single-file modules, multiple for directory modules).
    pub source_paths: Vec<PathBuf>,
    /// Module-level documentation.
    pub doc: Option<String>,
}

// ── ModuleImport ─────────────────────────────────────────────────────

/// A resolved import within a module.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ModuleImport {
    pub target: ModuleId,
    pub spec: Option<ImportSpec>,
    pub span: Span,
}

// ── DuplicateModule ──────────────────────────────────────────────────

/// Error produced when a module is inserted into the graph more than once.
#[derive(Debug, Clone, PartialEq)]
pub struct DuplicateModule {
    /// The id of the module that was already present.
    pub id: ModuleId,
}

impl fmt::Display for DuplicateModule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "duplicate module `{}`", self.id)
    }
}

impl std::error::Error for DuplicateModule {}

// ── CycleError ───────────────────────────────────────────────────────

/// Error produced when the module graph contains an import cycle.
#[derive(Debug, Clone)]
pub struct CycleError {
    /// The cycle as a list of module ids (first == last).
    pub cycle: Vec<ModuleId>,
    /// The source span of each import statement on the cycle path.
    ///
    /// `import_spans[i]` is the span of the import in `cycle[i]` that
    /// introduces the edge to `cycle[i + 1]`.  The slice has the same length
    /// as `cycle` minus one (there is no incoming edge for the last element,
    /// which repeats the first module id to close the cycle).
    pub import_spans: Vec<Span>,
}

impl fmt::Display for CycleError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "import cycle detected: ")?;
        for (i, id) in self.cycle.iter().enumerate() {
            if i > 0 {
                write!(f, " -> ")?;
            }
            write!(f, "{id}")?;
        }
        Ok(())
    }
}

impl std::error::Error for CycleError {}

// ── ModuleGraph ──────────────────────────────────────────────────────

/// The complete module graph for a compilation.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ModuleGraph {
    /// All modules in the graph, keyed by their ID.
    ///
    /// An ordered map on purpose: several consumers iterate it (the topo-order
    /// DFS seed, collision scans in the checker and HIR lowering, the JSON
    /// dump), and every one of them must see the same order on every run. A
    /// hash-seeded map here made function order, `BindingId`s and diagnostic
    /// order drift between compiles of one unchanged program.
    ///
    /// Custom serialization converts `ModuleId` keys to strings (JSON requires
    /// string keys).  Format: `"std::net::http"` or `"(root)"` for the root.
    #[serde(
        serialize_with = "serialize_module_map",
        deserialize_with = "deserialize_module_map"
    )]
    pub modules: BTreeMap<ModuleId, Module>,
    /// The root module (entry point).
    pub root: ModuleId,
    /// Topological order for processing (dependencies before dependents).
    ///
    /// Where the import DAG leaves freedom (a diamond, unrelated siblings) the
    /// order is fixed by `ModuleId` ordering of the DFS seeds, so it is the
    /// same for every graph built from the same modules.
    pub topo_order: Vec<ModuleId>,
    /// Per-item defining source file for each module, keyed by the module's
    /// dotted path and parallel to that module's `items` vector. A directory
    /// module assembles its primary file plus every peer `.hew` file; the
    /// item spans stay file-relative byte offsets with no file identity of
    /// their own, so this table is the only sound way to attribute an
    /// assembled item to its declaring file. Absent entries (older callers,
    /// hand-built graphs) mean "every item comes from the module's first
    /// source path".
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub item_sources: HashMap<String, Vec<PathBuf>>,
}

// ── FileSpanIndices ──────────────────────────────────────────────────

/// One span-key discriminator per SOURCE FILE in a module graph.
///
/// The checker keys every recorded fact by `(byte range, index)`. A byte range
/// is an offset into one FILE, so the index must discriminate files: two files
/// with an expression at the same offset would otherwise share a key and the
/// second would silently overwrite the first's type.
///
/// A per-MODULE index is not sufficient. A directory module assembles its entry
/// file plus every peer `.hew` file into ONE module whose items keep their own
/// file-relative spans, so two peer files with same-length declarations collide.
/// That produced a codegen abort — `mm/dog.hew` and `mm/cat.hew` each declaring
/// `pub type X { name: string }` plus a constructor made `make_cat` build a
/// `Dog`, and the Move type check rejected the mismatch. Whether the collision
/// bites depends only on byte offsets, so renaming a field appeared to "fix" it.
///
/// Every consumer that stamps or reads a span key must derive its index from
/// this one allocation. Numbering is 1-based over the non-root modules in
/// topological order, with a module occupying as many consecutive indices as it
/// has source files; the root compilation unit keeps index 0.
#[derive(Debug, Clone, Default)]
pub struct FileSpanIndices {
    module_base: HashMap<ModuleId, u32>,
    item_index: HashMap<ModuleId, Vec<u32>>,
    by_path: HashMap<PathBuf, u32>,
    module_name: HashMap<u32, String>,
}

impl FileSpanIndices {
    /// The index of a module's ENTRY file — the fallback for any item whose
    /// defining file was not recorded.
    #[must_use]
    pub fn module_base(&self, id: &ModuleId) -> Option<u32> {
        self.module_base.get(id).copied()
    }

    /// The index for `modules[id].items[item_idx]`, attributed to the file that
    /// actually declares it.
    #[must_use]
    pub fn item_index(&self, id: &ModuleId, item_idx: usize) -> Option<u32> {
        self.item_index
            .get(id)
            .and_then(|indices| indices.get(item_idx).copied())
            .or_else(|| self.module_base(id))
    }

    /// The index for a canonical source path. First writer wins, matching the
    /// module walk: a file reachable both on its own and as a directory
    /// module's peer keeps the identity of whichever module was checked first.
    #[must_use]
    pub fn path_index(&self, path: &Path) -> Option<u32> {
        self.by_path.get(path).copied()
    }

    /// The dotted module path that owns an index.
    #[must_use]
    pub fn module_name(&self, index: u32) -> Option<&str> {
        self.module_name.get(&index).map(String::as_str)
    }
}

impl ModuleGraph {
    #[must_use]
    pub fn new(root: ModuleId) -> Self {
        Self {
            modules: BTreeMap::new(),
            root,
            topo_order: Vec::new(),
            item_sources: HashMap::new(),
        }
    }

    /// The defining source file of `modules[id].items[item_idx]`, when
    /// per-item attribution was recorded and disagrees are impossible: the
    /// recorded vector must be parallel to the module's items.
    #[must_use]
    pub fn item_source(&self, id: &ModuleId, item_idx: usize) -> Option<&PathBuf> {
        self.item_sources.get(&id.path.join("."))?.get(item_idx)
    }

    /// Allocate one span-key discriminator per SOURCE FILE in the graph.
    ///
    /// See [`FileSpanIndices`] for why the unit is a file and not a module.
    #[must_use]
    pub fn file_span_indices(&self) -> FileSpanIndices {
        let mut indices = FileSpanIndices::default();
        let mut next: u32 = 0;
        for module_id in &self.topo_order {
            if *module_id == self.root {
                continue;
            }
            let Some(module) = self.modules.get(module_id) else {
                // Deliberately does NOT advance the counter: an id in
                // `topo_order` with no module entry (a dangling import that
                // survived to lowering) must not shift the numbering on one
                // consumer only.
                continue;
            };
            let dotted = module_id.path.join(".");
            let mut files: Vec<PathBuf> = Vec::new();
            for path in &module.source_paths {
                if !files.contains(path) {
                    files.push(path.clone());
                }
            }
            let base = next + 1;
            // A module with no recorded source paths still owns exactly one
            // index, so the numbering is independent of path bookkeeping.
            let file_count = files.len().max(1);
            for (offset, path) in files.iter().enumerate() {
                let idx = base + u32::try_from(offset).unwrap_or(0);
                indices.by_path.entry(path.clone()).or_insert(idx);
            }
            for offset in 0..file_count {
                let idx = base + u32::try_from(offset).unwrap_or(0);
                indices.module_name.insert(idx, dotted.clone());
            }
            next += u32::try_from(file_count).unwrap_or(1);
            indices.module_base.insert(module_id.clone(), base);

            // Per-item indices. An item with no recorded source (or one naming
            // a file outside `source_paths`) belongs to the module's entry
            // file, which is `base`.
            let sources = self.item_sources.get(&dotted);
            let item_indices: Vec<u32> = (0..module.items.len())
                .map(|item_idx| {
                    sources
                        .and_then(|paths| paths.get(item_idx))
                        .and_then(|path| {
                            files
                                .iter()
                                .position(|candidate| candidate == path)
                                .map(|offset| base + u32::try_from(offset).unwrap_or(0))
                        })
                        .unwrap_or(base)
                })
                .collect();
            indices.item_index.insert(module_id.clone(), item_indices);
        }
        indices
    }

    /// Insert a module into the graph.
    ///
    /// # Errors
    ///
    /// Returns [`DuplicateModule`] if a module with the same id was already
    /// present. The existing entry is left unchanged.
    pub fn add_module(&mut self, module: Module) -> Result<(), DuplicateModule> {
        use std::collections::btree_map::Entry;
        match self.modules.entry(module.id.clone()) {
            Entry::Vacant(e) => {
                e.insert(module);
                Ok(())
            }
            Entry::Occupied(_) => Err(DuplicateModule { id: module.id }),
        }
    }

    /// Return the direct dependencies (import targets) of a module.
    #[must_use]
    pub fn dependencies(&self, id: &ModuleId) -> Vec<&ModuleId> {
        self.modules
            .get(id)
            .map(|m| m.imports.iter().map(|imp| &imp.target).collect())
            .unwrap_or_default()
    }

    /// Compute a topological ordering of the module graph via DFS.
    /// Returns `Err(CycleError)` if a cycle is detected.
    #[expect(clippy::missing_errors_doc, reason = "internal API")]
    pub fn compute_topo_order(&mut self) -> Result<(), CycleError> {
        #[derive(Clone, Copy, PartialEq)]
        enum Mark {
            Temporary,
            Permanent,
        }

        fn visit(
            id: &ModuleId,
            entry_span: Span,
            modules: &BTreeMap<ModuleId, Module>,
            marks: &mut HashMap<ModuleId, Mark>,
            order: &mut Vec<ModuleId>,
            stack: &mut Vec<(ModuleId, Span)>,
        ) -> Result<(), CycleError> {
            match marks.get(id) {
                Some(Mark::Permanent) => return Ok(()),
                Some(Mark::Temporary) => {
                    // Build cycle path from the stack.
                    let start = stack.iter().position(|(s, _)| s == id).unwrap_or(0);
                    let cycle: Vec<ModuleId> = stack[start..]
                        .iter()
                        .map(|(m, _)| m.clone())
                        .chain(std::iter::once(id.clone()))
                        .collect();
                    let import_spans: Vec<Span> = stack[start + 1..]
                        .iter()
                        .map(|(_, sp)| sp.clone())
                        .chain(std::iter::once(entry_span.clone()))
                        .collect();
                    return Err(CycleError {
                        cycle,
                        import_spans,
                    });
                }
                None => {}
            }

            marks.insert(id.clone(), Mark::Temporary);
            stack.push((id.clone(), entry_span));

            if let Some(module) = modules.get(id) {
                for imp in &module.imports {
                    visit(&imp.target, imp.span.clone(), modules, marks, order, stack)?;
                }
            }

            stack.pop();
            marks.insert(id.clone(), Mark::Permanent);
            order.push(id.clone());
            Ok(())
        }

        let mut marks: HashMap<ModuleId, Mark> = HashMap::new();
        let mut order: Vec<ModuleId> = Vec::new();

        // Collect keys up-front to avoid borrow issues. `modules` is ordered,
        // so the seeds (and with them the order among unrelated modules) are
        // the same on every run.
        let ids: Vec<ModuleId> = self.modules.keys().cloned().collect();

        for id in &ids {
            if !marks.contains_key(id) {
                visit(
                    id,
                    0..0,
                    &self.modules,
                    &mut marks,
                    &mut order,
                    &mut Vec::new(),
                )?;
            }
        }

        self.topo_order = order;
        Ok(())
    }
}

// ── ModuleId ↔ String map serialization ─────────────────────────────
//
// JSON requires object keys to be strings.  `ModuleId` is a struct, so
// serde_json refuses to serialize `BTreeMap<ModuleId, _>` by default.
// These helpers convert keys via `Display` / `FromStr`-style parsing.

fn serialize_module_map<S>(
    map: &BTreeMap<ModuleId, Module>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    use serde::ser::SerializeMap;
    let mut ser_map = serializer.serialize_map(Some(map.len()))?;
    for (k, v) in map {
        ser_map.serialize_entry(&k.to_string(), v)?;
    }
    ser_map.end()
}

fn deserialize_module_map<'de, D>(deserializer: D) -> Result<BTreeMap<ModuleId, Module>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let string_map: BTreeMap<String, Module> = BTreeMap::deserialize(deserializer)?;
    Ok(string_map
        .into_iter()
        .map(|(k, v)| {
            let id = if k == "(root)" {
                ModuleId::root()
            } else {
                ModuleId::new(k.split("::").map(String::from).collect())
            };
            (id, v)
        })
        .collect())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn module(id: &str, deps: &[&str]) -> Module {
        Module {
            id: ModuleId::new(vec![id.to_string()]),
            items: Vec::new(),
            imports: deps
                .iter()
                .map(|d| ModuleImport {
                    target: ModuleId::new(vec![d.to_string()]),
                    spec: None,
                    span: 0..0,
                })
                .collect(),
            source_paths: Vec::new(),
            doc: None,
        }
    }

    #[test]
    fn display_module_id() {
        let id = ModuleId::new(vec!["std".into(), "net".into(), "http".into()]);
        assert_eq!(id.to_string(), "std::net::http");
        assert_eq!(ModuleId::root().to_string(), "(root)");
    }

    #[test]
    fn topo_order_linear() {
        let mut g = ModuleGraph::new(ModuleId::new(vec!["a".into()]));
        g.add_module(module("a", &["b"])).unwrap();
        g.add_module(module("b", &["c"])).unwrap();
        g.add_module(module("c", &[])).unwrap();
        g.compute_topo_order().unwrap();
        let names: Vec<&str> = g.topo_order.iter().map(|id| id.path[0].as_str()).collect();
        // c before b before a
        assert_eq!(names, vec!["c", "b", "a"]);
    }

    #[test]
    fn topo_order_diamond() {
        let mut g = ModuleGraph::new(ModuleId::new(vec!["a".into()]));
        g.add_module(module("a", &["b", "c"])).unwrap();
        g.add_module(module("b", &["d"])).unwrap();
        g.add_module(module("c", &["d"])).unwrap();
        g.add_module(module("d", &[])).unwrap();
        g.compute_topo_order().unwrap();
        let pos = |name: &str| {
            g.topo_order
                .iter()
                .position(|id| id.path[0] == name)
                .unwrap()
        };
        assert!(pos("d") < pos("b"));
        assert!(pos("d") < pos("c"));
        assert!(pos("b") < pos("a"));
        assert!(pos("c") < pos("a"));
    }

    /// A diamond leaves the DFS free to visit `b` or `c` first. That freedom
    /// must resolve the same way for every graph built from the same modules:
    /// HIR lowering mints `BindingId`s and orders function bodies by walking
    /// `topo_order`, so a different order here changes every downstream dump
    /// and diagnostic. Twenty fresh graphs give a hash-seeded map essentially
    /// no chance of agreeing by luck.
    #[test]
    fn topo_order_diamond_is_identical_across_fresh_graphs() {
        let orders: Vec<Vec<String>> = (0..20)
            .map(|_| {
                let mut g = ModuleGraph::new(ModuleId::new(vec!["a".into()]));
                g.add_module(module("a", &["b", "c"])).unwrap();
                g.add_module(module("b", &["d"])).unwrap();
                g.add_module(module("c", &["d"])).unwrap();
                g.add_module(module("d", &[])).unwrap();
                g.compute_topo_order().unwrap();
                g.topo_order.iter().map(ModuleId::to_string).collect()
            })
            .collect();
        for order in &orders[1..] {
            assert_eq!(
                order, &orders[0],
                "topological order drifted between graphs"
            );
        }
    }

    #[test]
    fn cycle_detected() {
        let mut g = ModuleGraph::new(ModuleId::new(vec!["a".into()]));
        g.add_module(module("a", &["b"])).unwrap();
        g.add_module(module("b", &["a"])).unwrap();
        let err = g.compute_topo_order().unwrap_err();
        assert!(err.to_string().contains("import cycle detected"));
    }

    #[test]
    fn dependencies() {
        let mut g = ModuleGraph::new(ModuleId::new(vec!["a".into()]));
        g.add_module(module("a", &["b", "c"])).unwrap();
        g.add_module(module("b", &[])).unwrap();
        g.add_module(module("c", &[])).unwrap();
        let deps = g.dependencies(&ModuleId::new(vec!["a".into()]));
        let names: Vec<&str> = deps.iter().map(|id| id.path[0].as_str()).collect();
        assert!(names.contains(&"b"));
        assert!(names.contains(&"c"));
        assert_eq!(deps.len(), 2);
    }

    #[test]
    fn cycle_error_carries_import_spans() {
        // Build a two-module cycle with distinct import spans so we can
        // verify each span is threaded into CycleError.import_spans.
        let mut g = ModuleGraph::new(ModuleId::new(vec!["a".into()]));

        let module_a = Module {
            id: ModuleId::new(vec!["a".into()]),
            items: vec![],
            imports: vec![ModuleImport {
                target: ModuleId::new(vec!["b".into()]),
                spec: None,
                span: 10..20,
            }],
            source_paths: vec![],
            doc: None,
        };
        let module_b = Module {
            id: ModuleId::new(vec!["b".into()]),
            items: vec![],
            imports: vec![ModuleImport {
                target: ModuleId::new(vec!["a".into()]),
                spec: None,
                span: 30..40,
            }],
            source_paths: vec![],
            doc: None,
        };

        g.add_module(module_a).unwrap();
        g.add_module(module_b).unwrap();

        let err = g.compute_topo_order().unwrap_err();

        // cycle contains a → b → a (first == last).
        assert_eq!(err.cycle.len(), 3);
        // import_spans has one entry per edge — two for the a→b→a cycle.
        assert_eq!(
            err.import_spans.len(),
            err.cycle.len() - 1,
            "import_spans should have one span per cycle edge"
        );
        // At least one of the spans must be non-empty (from our fixtures).
        let has_real_span = err.import_spans.iter().any(|s| !s.is_empty());
        assert!(
            has_real_span,
            "cycle error should carry import spans: {err}"
        );
        // The dummy root-entry span (0..0) must never appear in import_spans.
        assert!(
            !err.import_spans.iter().any(|s| s.start == 0 && s.end == 0),
            "dummy 0..0 span must not appear in import_spans: {:?}",
            err.import_spans
        );
        // Both fixture edge spans must be present. Their order depends on
        // which seed the DFS starts from, which this test does not pin.
        assert!(
            err.import_spans.contains(&(10..20)),
            "a→b import span 10..20 must appear in import_spans: {:?}",
            err.import_spans
        );
        assert!(
            err.import_spans.contains(&(30..40)),
            "b→a import span 30..40 must appear in import_spans: {:?}",
            err.import_spans
        );
    }

    #[test]
    fn duplicate_module_detected() {
        let mut g = ModuleGraph::new(ModuleId::new(vec!["a".into()]));
        g.add_module(module("a", &[])).unwrap();

        let err = g.add_module(module("a", &[])).unwrap_err();
        assert_eq!(err.id, ModuleId::new(vec!["a".into()]));
        assert!(
            err.to_string().contains("duplicate module"),
            "error message should describe the collision: {err}"
        );

        // Original entry must be unchanged.
        assert!(g.modules.contains_key(&ModuleId::new(vec!["a".into()])));
    }
}
