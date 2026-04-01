//! On-demand module loader and cache.
//!
//! Replaces the baked-in `stdlib_generated.rs` tables. Discovers modules
//! by searching the filesystem and parsing `.hew` files at user compile time.

use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

use crate::stdlib_loader::{load_module, module_short_name, ModuleInfo};

/// On-demand module loader and cache.
///
/// Replaces the baked-in `stdlib_generated.rs` tables. Discovers modules
/// by searching the filesystem and parsing `.hew` files at user compile time.
#[derive(Debug)]
pub struct ModuleRegistry {
    /// Cached module info, keyed by full module path (e.g. `std::encoding::json`).
    modules: HashMap<String, ModuleInfo>,
    /// Ordered search paths for module resolution.
    search_paths: Vec<PathBuf>,
    /// Accumulated handle types from all loaded modules.
    handle_types: HashSet<String>,
    /// Accumulated drop types from all loaded modules.
    drop_types: HashSet<String>,
}

/// Build the default module search-path list used by both the CLI and LSP.
///
/// The search order is:
/// 1. `HEWPATH` environment variable (colon-separated; each entry is the parent of `std/`)
/// 2. `HEW_STD` environment variable (path to the `std/` directory itself)
/// 3. FHS installed location: `<prefix>/share/hew` relative to the compiler binary
/// 4. Dev fallback: repo root (when `std/` exists two levels above the binary)
#[must_use]
pub fn build_module_search_paths() -> Vec<PathBuf> {
    let mut paths = Vec::new();

    // HEWPATH environment variable (colon-separated; each entry is the parent of std/)
    if let Ok(hewpath) = std::env::var("HEWPATH") {
        for p in hewpath.split(':') {
            let path = PathBuf::from(p);
            if path.exists() {
                paths.push(path);
            }
        }
    }

    // HEW_STD environment variable (points directly to the std/ directory;
    // module loader needs its parent as the search root)
    if let Ok(hew_std) = std::env::var("HEW_STD") {
        let std_path = PathBuf::from(&hew_std);
        if std_path.exists() {
            if let Some(parent) = std_path.parent() {
                let parent = parent.to_path_buf();
                if !paths.contains(&parent) {
                    paths.push(parent);
                }
            }
        }
    }

    // FHS installed location: <prefix>/share/hew (parent of std/)
    // e.g. /usr/local/bin/hew → /usr/local/share/hew → contains std/
    if let Ok(exe) = std::env::current_exe() {
        if let Some(exe_dir) = exe.parent() {
            let share_hew = exe_dir.join("../share/hew");
            if share_hew.join("std").exists() && !paths.contains(&share_hew) {
                paths.push(share_hew);
            }
        }
    }

    // Dev fallback: repo root (when std/ exists two levels above the binary)
    if let Ok(exe) = std::env::current_exe() {
        if let Some(exe_dir) = exe.parent() {
            let repo_root = exe_dir.join("../..");
            if repo_root.join("std").exists() {
                paths.push(repo_root);
            }
        }
    }

    paths
}

#[derive(Debug)]
pub enum ModuleError {
    NotFound {
        module_path: String,
        searched: Vec<PathBuf>,
    },
    ParseError {
        module_path: String,
        file_path: PathBuf,
    },
}

impl std::fmt::Display for ModuleError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ModuleError::NotFound {
                module_path,
                searched,
            } => {
                writeln!(f, "error: module `{module_path}` not found")?;
                writeln!(f)?;
                writeln!(f, "  searched:")?;
                for path in searched {
                    writeln!(f, "    {}", path.display())?;
                }
                Ok(())
            }
            ModuleError::ParseError {
                module_path,
                file_path,
            } => {
                write!(
                    f,
                    "error: module `{module_path}` has parse errors in {}",
                    file_path.display()
                )
            }
        }
    }
}

impl ModuleRegistry {
    /// Create a new registry with the given search paths.
    ///
    /// Search paths are tried in order during module resolution — first match wins.
    #[must_use]
    pub fn new(search_paths: Vec<PathBuf>) -> Self {
        Self {
            modules: HashMap::new(),
            search_paths,
            handle_types: HashSet::new(),
            drop_types: HashSet::new(),
        }
    }

    /// Load a module by its full path (e.g. `std::encoding::json`).
    ///
    /// If the module is already cached, returns the cached version.
    /// Otherwise, iterates search paths and delegates to `stdlib_loader::load_module`
    /// for resolution and parsing.
    ///
    /// On success, the module's handle types and drop types are accumulated into
    /// the registry-wide sets.
    ///
    /// # Errors
    ///
    /// Returns [`ModuleError::NotFound`] if no search path contains the module,
    /// or [`ModuleError::ParseError`] if the module file exists but cannot be parsed.
    pub fn load(&mut self, module_path: &str) -> Result<&ModuleInfo, ModuleError> {
        // Already cached — return it.
        if self.modules.contains_key(module_path) {
            return Ok(&self.modules[module_path]);
        }

        // Try each search path in order.
        for search_path in &self.search_paths {
            if let Some(info) = load_module(module_path, search_path) {
                // Accumulate handle types and drop types.
                for ht in &info.handle_types {
                    self.handle_types.insert(ht.clone());
                }
                for dt in &info.drop_types {
                    self.drop_types.insert(dt.clone());
                }

                self.modules.insert(module_path.to_string(), info);
                return Ok(&self.modules[module_path]);
            }
        }

        Err(ModuleError::NotFound {
            module_path: module_path.to_string(),
            searched: self.search_paths.clone(),
        })
    }

    /// Return cached module info if it has already been loaded.
    #[must_use]
    pub fn get(&self, module_path: &str) -> Option<&ModuleInfo> {
        self.modules.get(module_path)
    }

    /// Check if a fully-qualified name is a handle type across all loaded modules.
    #[must_use]
    pub fn is_handle_type(&self, name: &str) -> bool {
        self.handle_types.contains(name)
    }

    /// Check if a fully-qualified name is a drop type across all loaded modules.
    #[must_use]
    pub fn is_drop_type(&self, name: &str) -> bool {
        self.drop_types.contains(name)
    }

    /// If an unqualified name matches a handle type, return the fully-qualified form.
    ///
    /// For example, "Value" -> Some("json.Value") after loading the json module.
    /// Returns `None` if no match is found.
    #[must_use]
    pub fn qualify_handle_type(&self, name: &str) -> Option<String> {
        self.handle_types
            .iter()
            .find(|ht| ht.rsplit('.').next() == Some(name))
            .cloned()
    }

    /// Return all handle types from all loaded modules.
    #[must_use]
    pub fn all_handle_types(&self) -> Vec<String> {
        self.handle_types.iter().cloned().collect()
    }

    /// Resolve a module-qualified call to a C symbol.
    ///
    /// Given a module short name (e.g. "json") and a method name (e.g. "parse"),
    /// searches all loaded modules for one whose short name matches, then looks
    /// up the method in that module's `clean_names`.
    #[must_use]
    pub fn resolve_module_call(&self, module_short: &str, method: &str) -> Option<String> {
        for (module_path, info) in &self.modules {
            let short = module_short_name(module_path);
            if short == module_short {
                for (clean, c_sym) in &info.clean_names {
                    if clean == method {
                        return Some(c_sym.clone());
                    }
                }
            }
        }
        None
    }

    /// Resolve a handle method to its C symbol.
    ///
    /// Searches all loaded modules' `handle_methods` for a match on
    /// `(handle_type, method)`.
    #[must_use]
    pub fn resolve_handle_method(&self, handle_type: &str, method: &str) -> Option<String> {
        for info in self.modules.values() {
            for ((ty, m), c_sym) in &info.handle_methods {
                if ty == handle_type && m == method {
                    return Some(c_sym.clone());
                }
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_root() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .to_path_buf()
    }

    fn registry() -> ModuleRegistry {
        ModuleRegistry::new(vec![test_root()])
    }

    #[test]
    fn load_json_module() {
        let mut reg = registry();
        let info = reg.load("std::encoding::json").unwrap();
        assert!(!info.functions.is_empty(), "json should have functions");
        assert!(
            info.handle_types.contains(&"json.Value".to_string()),
            "json should declare json.Value"
        );
    }

    #[test]
    fn load_caches_result() {
        let mut reg = registry();
        reg.load("std::encoding::json").unwrap();
        // Second call should return cached.
        let info = reg.get("std::encoding::json");
        assert!(info.is_some(), "should be cached after load");
    }

    #[test]
    fn load_nonexistent_returns_not_found() {
        let mut reg = registry();
        let err = reg.load("std::does::not::exist").unwrap_err();
        match err {
            ModuleError::NotFound {
                module_path,
                searched,
            } => {
                assert_eq!(module_path, "std::does::not::exist");
                assert_eq!(searched.len(), 1);
                assert_eq!(searched[0], test_root());
            }
            ModuleError::ParseError { .. } => panic!("expected NotFound, got ParseError"),
        }
    }

    #[test]
    fn handle_types_accumulated() {
        let mut reg = registry();
        reg.load("std::encoding::json").unwrap();
        assert!(
            reg.is_handle_type("json.Value"),
            "json.Value should be a handle type"
        );
        assert!(
            !reg.is_handle_type("http.Request"),
            "http.Request should not be loaded yet"
        );

        // Load another module — types accumulate.
        reg.load("std::net::http").unwrap();
        assert!(reg.is_handle_type("json.Value"), "json.Value still present");
        assert!(
            reg.is_handle_type("http.Request"),
            "http.Request now present"
        );
    }

    #[test]
    fn drop_types_accumulated() {
        let mut reg = registry();
        reg.load("std::net::http").unwrap();
        assert!(
            reg.is_drop_type("http.Request"),
            "http.Request should be a drop type"
        );
    }

    #[test]
    fn resolve_module_call_json_parse() {
        let mut reg = registry();
        reg.load("std::encoding::json").unwrap();
        let c_sym = reg.resolve_module_call("json", "parse");
        assert!(c_sym.is_some(), "should resolve json.parse");
    }

    #[test]
    fn resolve_handle_method_json_value() {
        let mut reg = registry();
        reg.load("std::encoding::json").unwrap();
        // json.Value should have handle methods from its impl block.
        let info = reg.get("std::encoding::json").unwrap();
        if !info.handle_methods.is_empty() {
            let ((ty, method), _) = &info.handle_methods[0];
            let c_sym = reg.resolve_handle_method(ty, method);
            assert!(c_sym.is_some(), "should resolve handle method");
        }
    }

    #[test]
    fn qualify_handle_type_works() {
        let mut reg = registry();
        reg.load("std::encoding::json").unwrap();
        let qualified = reg.qualify_handle_type("Value");
        assert_eq!(
            qualified,
            Some("json.Value".to_string()),
            "should qualify 'Value' to 'json.Value'"
        );
        assert_eq!(
            reg.qualify_handle_type("NonExistent"),
            None,
            "unknown type should return None"
        );
    }

    #[test]
    fn all_handle_types_returns_loaded() {
        let mut reg = registry();
        reg.load("std::encoding::json").unwrap();
        let all = reg.all_handle_types();
        assert!(
            all.contains(&"json.Value".to_string()),
            "all_handle_types should include json.Value"
        );
    }
}
