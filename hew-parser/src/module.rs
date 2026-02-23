//! Module graph types for the Hew compiler.
//!
//! Represents the structure of a multi-module compilation: each source file
//! becomes a [`Module`], and the edges between them (imports) form a
//! [`ModuleGraph`].  The graph carries a topological ordering so that
//! downstream passes can process modules in dependency order.

use std::collections::HashMap;
use std::fmt;
use std::path::PathBuf;

use serde::{Deserialize, Serialize};

use crate::ast::{ImportSpec, Item, Span, Spanned};

// ── ModuleId ─────────────────────────────────────────────────────────

/// Unique identifier for a module, based on its path segments
/// (e.g. `["std", "net", "http"]` for `std::net::http`).
#[derive(Debug, Clone, Hash, Eq, PartialEq, Serialize, Deserialize)]
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
    /// Source file path (if file-backed).
    pub source_path: Option<PathBuf>,
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

// ── CycleError ───────────────────────────────────────────────────────

/// Error produced when the module graph contains an import cycle.
#[derive(Debug, Clone)]
pub struct CycleError {
    /// The cycle as a list of module ids (first == last).
    pub cycle: Vec<ModuleId>,
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
    pub modules: HashMap<ModuleId, Module>,
    /// The root module (entry point).
    pub root: ModuleId,
    /// Topological order for processing (dependencies before dependents).
    pub topo_order: Vec<ModuleId>,
}

impl ModuleGraph {
    #[must_use]
    pub fn new(root: ModuleId) -> Self {
        Self {
            modules: HashMap::new(),
            root,
            topo_order: Vec::new(),
        }
    }

    pub fn add_module(&mut self, module: Module) {
        self.modules.insert(module.id.clone(), module);
    }

    #[must_use]
    pub fn get_module(&self, id: &ModuleId) -> Option<&Module> {
        self.modules.get(id)
    }

    pub fn get_module_mut(&mut self, id: &ModuleId) -> Option<&mut Module> {
        self.modules.get_mut(id)
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
    pub fn compute_topo_order(&mut self) -> Result<(), CycleError> {
        #[derive(Clone, Copy, PartialEq)]
        enum Mark {
            Temporary,
            Permanent,
        }

        let mut marks: HashMap<ModuleId, Mark> = HashMap::new();
        let mut order: Vec<ModuleId> = Vec::new();

        // Collect keys up-front to avoid borrow issues.
        let ids: Vec<ModuleId> = self.modules.keys().cloned().collect();

        for id in &ids {
            if !marks.contains_key(id) {
                visit(id, &self.modules, &mut marks, &mut order, &mut Vec::new())?;
            }
        }

        self.topo_order = order;
        return Ok(());

        fn visit(
            id: &ModuleId,
            modules: &HashMap<ModuleId, Module>,
            marks: &mut HashMap<ModuleId, Mark>,
            order: &mut Vec<ModuleId>,
            stack: &mut Vec<ModuleId>,
        ) -> Result<(), CycleError> {
            match marks.get(id) {
                Some(Mark::Permanent) => return Ok(()),
                Some(Mark::Temporary) => {
                    // Build cycle path from the stack.
                    let start = stack.iter().position(|s| s == id).unwrap_or(0);
                    let mut cycle: Vec<ModuleId> = stack[start..].to_vec();
                    cycle.push(id.clone());
                    return Err(CycleError { cycle });
                }
                None => {}
            }

            marks.insert(id.clone(), Mark::Temporary);
            stack.push(id.clone());

            if let Some(module) = modules.get(id) {
                for imp in &module.imports {
                    visit(&imp.target, modules, marks, order, stack)?;
                }
            }

            stack.pop();
            marks.insert(id.clone(), Mark::Permanent);
            order.push(id.clone());
            Ok(())
        }
    }
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
            source_path: None,
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
        g.add_module(module("a", &["b"]));
        g.add_module(module("b", &["c"]));
        g.add_module(module("c", &[]));
        g.compute_topo_order().unwrap();
        let names: Vec<&str> = g.topo_order.iter().map(|id| id.path[0].as_str()).collect();
        // c before b before a
        assert_eq!(names, vec!["c", "b", "a"]);
    }

    #[test]
    fn topo_order_diamond() {
        let mut g = ModuleGraph::new(ModuleId::new(vec!["a".into()]));
        g.add_module(module("a", &["b", "c"]));
        g.add_module(module("b", &["d"]));
        g.add_module(module("c", &["d"]));
        g.add_module(module("d", &[]));
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

    #[test]
    fn cycle_detected() {
        let mut g = ModuleGraph::new(ModuleId::new(vec!["a".into()]));
        g.add_module(module("a", &["b"]));
        g.add_module(module("b", &["a"]));
        let err = g.compute_topo_order().unwrap_err();
        assert!(err.to_string().contains("import cycle detected"));
    }

    #[test]
    fn dependencies() {
        let mut g = ModuleGraph::new(ModuleId::new(vec!["a".into()]));
        g.add_module(module("a", &["b", "c"]));
        g.add_module(module("b", &[]));
        g.add_module(module("c", &[]));
        let deps = g.dependencies(&ModuleId::new(vec!["a".into()]));
        let names: Vec<&str> = deps.iter().map(|id| id.path[0].as_str()).collect();
        assert!(names.contains(&"b"));
        assert!(names.contains(&"c"));
        assert_eq!(deps.len(), 2);
    }
}
