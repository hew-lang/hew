//! On-demand module loader and cache.
//!
//! Replaces the baked-in `stdlib_generated.rs` tables. Discovers modules
//! by searching the filesystem and parsing `.hew` files at user compile time.

use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

use hew_parser::module::ModuleId;

use crate::stdlib_loader::{load_module_checked, ModuleInfo};

/// On-demand module loader and cache.
///
/// Replaces the baked-in `stdlib_generated.rs` tables. Discovers modules
/// by searching the filesystem and parsing `.hew` files at user compile time.
#[derive(Debug)]
pub struct ModuleRegistry {
    /// Cached module info, keyed by module id (e.g. `["std", "encoding", "json"]`).
    modules: HashMap<ModuleId, ModuleInfo>,
    /// Ordered search paths for module resolution.
    search_paths: Vec<PathBuf>,
    /// Accumulated handle types from all loaded modules.
    handle_types: HashSet<String>,
    /// Accumulated drop types from all loaded modules.
    drop_types: HashSet<String>,
    /// Accumulated drop functions from all loaded modules: `type_name` → C func name.
    drop_funcs: HashMap<String, String>,
}

/// Walk up the directory tree from `from`, returning the first ancestor directory
/// that is a Hew repository root (identified by containing `std/builtins.hew`).
///
/// Returns `None` if no such ancestor exists, which is the normal case for
/// external Hew projects compiled with an installed binary.
#[must_use]
pub fn find_enclosing_hew_root(from: &std::path::Path) -> Option<PathBuf> {
    let start = if from.is_dir() {
        from.to_path_buf()
    } else {
        from.parent()?.to_path_buf()
    };

    let mut current = start.as_path();
    loop {
        if current.join("std").join("builtins.hew").exists() {
            return Some(current.to_path_buf());
        }
        match current.parent() {
            Some(parent) => current = parent,
            None => return None,
        }
    }
}

/// Build the stdlib search-path list, applying exclusive precedence tiers.
///
/// Each tier is tried in order; if a tier produces at least one valid path,
/// that tier's result is returned immediately — lower tiers are not consulted.
///
/// **Tier 1 — explicit override (env vars):**
/// If `HEWPATH` (colon-separated; each entry is the parent of `std/`) or
/// `HEW_STD` (points directly at a `std/` directory) is set, only those paths
/// are returned.  All other sources are ignored.
///
/// **Tier 2 — in-worktree (developing Hew itself):**
/// Walk up from `source_hint` (if provided) or from cwd, looking for an
/// enclosing Hew checkout root (a directory that contains `std/builtins.hew`).
/// If found, only that root is returned.  This fixes the cross-worktree
/// contamination: a file inside worktree-A resolves std from A only, even
/// when the binary was built in a sibling worktree-B.
///
/// **Tier 3 — installed / external project:**
/// FHS (`<exe>/../share/hew`), XDG (`~/.local/share/hew/std` parent),
/// `~/.hew`, `/usr/local/share/hew`, `/usr/share/hew`, and the dev-build
/// fallback (two levels above the binary for `cargo run`-style invocations).
/// Multiple roots are allowed here; the first match wins at module-load time.
#[must_use]
pub fn build_module_search_paths_for(source_hint: Option<&std::path::Path>) -> Vec<PathBuf> {
    // --- Tier 1: explicit env-var override ---
    let mut tier1: Vec<PathBuf> = Vec::new();

    if let Ok(hewpath) = std::env::var("HEWPATH") {
        for p in hewpath.split(':') {
            let path = PathBuf::from(p);
            if path.exists() {
                tier1.push(path);
            }
        }
    }

    if let Ok(hew_std) = std::env::var("HEW_STD") {
        let std_path = PathBuf::from(&hew_std);
        if std_path.exists() {
            if let Some(parent) = std_path.parent() {
                let parent = parent.to_path_buf();
                if !tier1.contains(&parent) {
                    tier1.push(parent);
                }
            }
        }
    }

    if !tier1.is_empty() {
        return tier1;
    }

    // --- Tier 2: enclosing Hew checkout (in-worktree dev) ---
    //
    // Walk up from source_hint first, then cwd.  Using source_hint ensures the
    // file being compiled determines which worktree's std/ is used, not the
    // process cwd (which could be a different worktree or an external dir).
    let tier2_probe = source_hint.and_then(find_enclosing_hew_root).or_else(|| {
        std::env::current_dir()
            .ok()
            .and_then(|cwd| find_enclosing_hew_root(&cwd))
    });

    if let Some(root) = tier2_probe {
        return vec![root];
    }

    // --- Tier 3: installed binary / external project ---
    let mut tier3: Vec<PathBuf> = Vec::new();

    // FHS: <exe>/../share/hew
    if let Ok(exe) = std::env::current_exe() {
        if let Some(exe_dir) = exe.parent() {
            let share_hew = exe_dir.join("../share/hew");
            if share_hew.join("std").exists() && !tier3.contains(&share_hew) {
                tier3.push(share_hew);
            }
        }
    }

    // XDG: ~/.local/share/hew
    if let Some(home) = std::env::var_os("HOME") {
        let xdg_hew = PathBuf::from(home).join(".local/share/hew");
        if xdg_hew.join("std").exists() && !tier3.contains(&xdg_hew) {
            tier3.push(xdg_hew);
        }
    }

    // ~/.hew
    if let Some(home) = std::env::var_os("HOME") {
        let dot_hew = PathBuf::from(home).join(".hew");
        if dot_hew.join("std").exists() && !tier3.contains(&dot_hew) {
            tier3.push(dot_hew);
        }
    }

    // System-wide FHS locations
    for prefix in &["/usr/local/share/hew", "/usr/share/hew"] {
        let p = PathBuf::from(prefix);
        if p.join("std").exists() && !tier3.contains(&p) {
            tier3.push(p);
        }
    }

    // Dev fallback: two levels above the binary (e.g. target/debug/hew → repo root).
    // Only reached for an installed/external project when tier-2 found nothing —
    // i.e. a developer binary compiling a project outside any Hew checkout.
    // WHY: `cargo run` builds land in target/debug/ so exe/../.. is the repo root.
    // WHEN obsolete: when Hew has a proper install prefix and std is co-installed.
    // WHAT the real solution is: install std alongside the binary under share/hew.
    if let Ok(exe) = std::env::current_exe() {
        if let Some(exe_dir) = exe.parent() {
            if let Some(repo_root) = exe_dir.parent().and_then(|p| p.parent()) {
                if repo_root.join("std").exists() && !tier3.contains(&repo_root.to_path_buf()) {
                    tier3.push(repo_root.to_path_buf());
                }
            }
        }
    }

    tier3
}

/// Build the default module search-path list used by both the CLI and LSP.
///
/// This is a context-free wrapper around [`build_module_search_paths_for`]
/// with no source hint.  Callers that have a source file path should prefer
/// [`build_module_search_paths_for`] so that tier-2 (in-worktree) resolution
/// can anchor to the correct Hew checkout.
#[must_use]
pub fn build_module_search_paths() -> Vec<PathBuf> {
    build_module_search_paths_for(None)
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
        line: usize,
        column: usize,
        message: String,
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
                line,
                column,
                message,
            } => {
                write!(
                    f,
                    "error: module `{module_path}` has parse error in {}:{line}:{column}: {message}",
                    file_path.display(),
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
            drop_funcs: HashMap::new(),
        }
    }

    pub(crate) fn has_search_paths(&self) -> bool {
        !self.search_paths.is_empty()
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
        let id = ModuleId::new(module_path.split("::").map(String::from).collect());

        // Already cached — return it.
        if self.modules.contains_key(&id) {
            return Ok(&self.modules[&id]);
        }

        // Try each search path in order.
        for search_path in &self.search_paths {
            if let Some(info) = load_module_checked(module_path, search_path)? {
                // Accumulate handle types and drop types.
                for ht in &info.handle_types {
                    self.handle_types.insert(ht.clone());
                }
                for dt in &info.drop_types {
                    self.drop_types.insert(dt.clone());
                }
                for (ty, func) in &info.drop_funcs {
                    self.drop_funcs.insert(ty.clone(), func.clone());
                }

                self.modules.insert(id.clone(), info);
                return Ok(&self.modules[&id]);
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
        let id = ModuleId::new(module_path.split("::").map(String::from).collect());
        self.modules.get(&id)
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

    /// Return the C drop function for a fully-qualified type name, if known.
    ///
    /// Only populated for types with an `impl Drop` block whose `fn drop` body
    /// is a direct C call (the common stdlib pattern).
    #[must_use]
    pub fn drop_func_for(&self, type_name: &str) -> Option<&str> {
        self.drop_funcs.get(type_name).map(String::as_str)
    }

    /// Return all `(type_name, c_drop_func)` pairs from all loaded modules.
    #[must_use]
    pub fn all_drop_funcs(&self) -> Vec<(String, String)> {
        self.drop_funcs
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect()
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
        for (id, info) in &self.modules {
            let short = id.path.last().map_or("", String::as_str);
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
    ///
    /// Accepts either the fully-qualified handle type name (`json.Value`) or
    /// an unqualified short name (`Value`) that the loaded registry can
    /// qualify with its existing short-name lookup.
    #[must_use]
    pub fn resolve_handle_method(&self, handle_type: &str, method: &str) -> Option<String> {
        self.resolve_handle_method_sig(handle_type, method)
            .map(|(c_sym, _, _)| c_sym)
    }

    /// Resolve a handle method to its C symbol and extracted signature.
    ///
    /// Returns `(c_symbol, param_types, return_type)` for trivial extracted
    /// handle methods.
    #[must_use]
    pub fn resolve_handle_method_sig(
        &self,
        handle_type: &str,
        method: &str,
    ) -> Option<(String, Vec<crate::ty::Ty>, crate::ty::Ty)> {
        for info in self.modules.values() {
            for hm in &info.handle_methods {
                if hm.type_name == handle_type && hm.method_name == method {
                    return Some((
                        hm.c_symbol.clone(),
                        hm.params.clone(),
                        hm.return_type.clone(),
                    ));
                }
            }
        }
        self.qualify_handle_type(handle_type)
            .and_then(|qualified| self.resolve_handle_method_sig(&qualified, method))
    }

    /// Seed a fully-qualified handle type name for unit tests.
    ///
    /// Bypasses module loading so tests can populate `handle_types` without
    /// requiring real `.hew` module files on disk.
    #[cfg(test)]
    pub(crate) fn insert_handle_type_for_test(&mut self, qualified_name: String) {
        self.handle_types.insert(qualified_name);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn test_root() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .to_path_buf()
    }

    fn registry() -> ModuleRegistry {
        ModuleRegistry::new(vec![test_root()])
    }

    struct TestDir {
        root: PathBuf,
    }

    impl TestDir {
        fn new(prefix: &str) -> Self {
            let unique = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_nanos();
            let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                .parent()
                .unwrap()
                .join("target/test-workdirs")
                .join(format!("{prefix}-{}-{unique}", std::process::id()));
            fs::create_dir_all(&root).unwrap();
            Self { root }
        }
    }

    impl Drop for TestDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.root);
        }
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
    fn load_malformed_module_returns_parse_error() {
        let broken_dir = TestDir::new("module-registry-broken");
        let broken_std = broken_dir.root.join("std");
        fs::create_dir_all(&broken_std).unwrap();
        let broken_file = broken_std.join("broken.hew");
        fs::write(&broken_file, "pub fn broken() {\n    @\n}\n").unwrap();

        let fallback_dir = TestDir::new("module-registry-fallback");
        let fallback_std = fallback_dir.root.join("std");
        fs::create_dir_all(&fallback_std).unwrap();
        fs::write(
            fallback_std.join("broken.hew"),
            "pub fn broken(value: i32) -> i32 { value }\n",
        )
        .unwrap();

        let mut reg = ModuleRegistry::new(vec![broken_dir.root.clone(), fallback_dir.root.clone()]);
        let err = reg.load("std::broken").unwrap_err();
        match err {
            ModuleError::ParseError {
                module_path,
                file_path,
                line,
                column,
                message,
            } => {
                assert_eq!(module_path, "std::broken");
                assert_eq!(file_path, broken_file);
                assert_eq!((line, column), (2, 5));
                assert!(
                    !message.is_empty(),
                    "parse error should preserve the parser message"
                );
            }
            ModuleError::NotFound { .. } => panic!("expected ParseError, got NotFound"),
        }
        assert!(
            reg.get("std::broken").is_none(),
            "malformed modules must not be cached or loaded from later search paths"
        );
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
        reg.load("std::encoding::json").unwrap();
        assert!(
            !reg.is_drop_type("json.Value"),
            "json.Value should not be a drop type"
        );
        reg.load("std::net::http").unwrap();
        assert!(
            !reg.is_drop_type("http.Request"),
            "http.Request should not be a drop type"
        );
        reg.load("std::process").unwrap();
        assert!(
            reg.is_drop_type("process.Child"),
            "process.Child should be a drop type"
        );
        assert!(
            !reg.is_drop_type("http.Server"),
            "http.Server should not be a drop type"
        );
        reg.load("std::text::regex").unwrap();
        assert!(
            !reg.is_drop_type("regex.Pattern"),
            "regex.Pattern should not be a drop type"
        );
    }

    #[test]
    fn drop_funcs_accumulated() {
        let mut reg = registry();
        reg.load("std::encoding::json").unwrap();
        assert_eq!(
            reg.drop_func_for("json.Value"),
            None,
            "json.Value should not have a drop func"
        );
        reg.load("std::net::http").unwrap();
        assert_eq!(
            reg.drop_func_for("http.Request"),
            None,
            "http.Request should not have a drop func"
        );
        reg.load("std::process").unwrap();
        assert_eq!(
            reg.drop_func_for("process.Child"),
            Some("hew_process_drop"),
            "process.Child drop func should be hew_process_drop"
        );
        assert_eq!(
            reg.drop_func_for("http.Server"),
            None,
            "http.Server should not have a drop func"
        );
        reg.load("std::text::regex").unwrap();
        assert_eq!(
            reg.drop_func_for("regex.Pattern"),
            None,
            "regex.Pattern should not have a drop func"
        );
        let all = reg.all_drop_funcs();
        assert!(!all.is_empty(), "should have at least one drop func");
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
            let hm = &info.handle_methods[0];
            let c_sym = reg.resolve_handle_method(&hm.type_name, &hm.method_name);
            assert!(c_sym.is_some(), "should resolve handle method");
        }
    }

    #[test]
    fn resolve_handle_method_accepts_short_handle_name() {
        let mut reg = registry();
        reg.load("std::encoding::json").unwrap();
        let info = reg.get("std::encoding::json").unwrap();
        if let Some(hm) = info.handle_methods.first() {
            let short = hm
                .type_name
                .rsplit('.')
                .next()
                .expect("qualified handle type should have short name");
            let c_sym = reg.resolve_handle_method(short, &hm.method_name);
            assert_eq!(
                c_sym.as_deref(),
                Some(hm.c_symbol.as_str()),
                "short handle name should resolve to the same C symbol"
            );
        }
    }

    #[test]
    fn resolve_handle_method_sig_returns_process_child_signature() {
        let mut reg = registry();
        reg.load("std::process").unwrap();

        let sig = reg
            .resolve_handle_method_sig("process.Child", "wait")
            .expect("process.Child.wait should resolve");
        assert_eq!(sig.0, "hew_process_wait");
        assert_eq!(sig.1, Vec::<crate::ty::Ty>::new());
        assert_eq!(sig.2, crate::ty::Ty::I64);

        let short_sig = reg
            .resolve_handle_method_sig("Child", "kill")
            .expect("short handle name should resolve");
        assert_eq!(short_sig.0, "hew_process_kill");
        assert_eq!(short_sig.1, Vec::<crate::ty::Ty>::new());
        assert_eq!(short_sig.2, crate::ty::Ty::I64);
    }

    #[test]
    fn resolve_handle_method_sig_returns_listener_close_and_request_free() {
        let mut reg = registry();
        reg.load("std::net").unwrap();
        reg.load("std::net::http").unwrap();

        let listener_close = reg
            .resolve_handle_method_sig("net.Listener", "close")
            .expect("net.Listener.close should resolve");
        assert_eq!(listener_close.0, "hew_tcp_listener_close");
        assert_eq!(listener_close.1, Vec::<crate::ty::Ty>::new());
        assert_eq!(listener_close.2, crate::ty::Ty::Unit);

        let request_free = reg
            .resolve_handle_method_sig("http.Request", "free")
            .expect("http.Request.free should resolve");
        assert_eq!(request_free.0, "hew_http_request_free");
        assert_eq!(request_free.1, Vec::<crate::ty::Ty>::new());
        assert_eq!(request_free.2, crate::ty::Ty::Unit);
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

    // ── search path precedence tests ──────────────────────────────────────────

    /// A `TestHewTree` creates a minimal fake Hew checkout on disk:
    /// a root directory with `std/builtins.hew` (the worktree marker).
    struct TestHewTree {
        dir: TestDir,
    }

    impl TestHewTree {
        fn new(prefix: &str) -> Self {
            let dir = TestDir::new(prefix);
            let std_dir = dir.root.join("std");
            fs::create_dir_all(&std_dir).unwrap();
            fs::write(std_dir.join("builtins.hew"), "// fake builtins\n").unwrap();
            Self { dir }
        }

        fn root(&self) -> &PathBuf {
            &self.dir.root
        }
    }

    /// HEWPATH set → tier-1 returns exactly those paths, no dev/cwd leakage.
    #[test]
    fn tier1_hewpath_returns_only_hewpath_entries() {
        let tree_a = TestHewTree::new("sp-hewpath-a");
        let tree_b = TestHewTree::new("sp-hewpath-b");

        // Set HEWPATH to tree_a only.
        let prev_hewpath = std::env::var("HEWPATH").ok();
        let prev_hew_std = std::env::var("HEW_STD").ok();
        // SAFETY: test process is single-threaded for env mutation; cargo test
        // runs each test fn sequentially within a thread.
        unsafe {
            std::env::set_var("HEWPATH", tree_a.root().to_str().unwrap());
            std::env::remove_var("HEW_STD");
        }

        let paths = build_module_search_paths_for(Some(tree_b.root()));

        // Restore env.
        // SAFETY: same single-threaded env-mutation guarantee as the set above.
        unsafe {
            match prev_hewpath {
                Some(v) => std::env::set_var("HEWPATH", v),
                None => std::env::remove_var("HEWPATH"),
            }
            match prev_hew_std {
                Some(v) => std::env::set_var("HEW_STD", v),
                None => std::env::remove_var("HEW_STD"),
            }
        }

        // Must contain exactly tree_a (canonicalized comparison).
        let canon_a = tree_a.root().canonicalize().unwrap();
        let canon_b = tree_b.root().canonicalize().unwrap();
        let got_canon: Vec<_> = paths.iter().filter_map(|p| p.canonicalize().ok()).collect();
        assert!(
            got_canon.contains(&canon_a),
            "HEWPATH entry should appear in result"
        );
        assert!(
            !got_canon.contains(&canon_b),
            "source_hint tree must not leak in when HEWPATH is set"
        );
    }

    /// `HEW_STD` set → tier-1 returns parent of that std/, no other sources.
    #[test]
    fn tier1_hew_std_returns_only_hew_std_parent() {
        let tree_a = TestHewTree::new("sp-hew-std-a");
        let tree_b = TestHewTree::new("sp-hew-std-b");
        let std_a = tree_a.root().join("std");

        let prev_hewpath = std::env::var("HEWPATH").ok();
        let prev_hew_std = std::env::var("HEW_STD").ok();
        // SAFETY: test process is single-threaded for env mutation.
        unsafe {
            std::env::remove_var("HEWPATH");
            std::env::set_var("HEW_STD", std_a.to_str().unwrap());
        }

        let paths = build_module_search_paths_for(Some(tree_b.root()));

        // SAFETY: same single-threaded env-mutation guarantee as the set above.
        unsafe {
            match prev_hewpath {
                Some(v) => std::env::set_var("HEWPATH", v),
                None => std::env::remove_var("HEWPATH"),
            }
            match prev_hew_std {
                Some(v) => std::env::set_var("HEW_STD", v),
                None => std::env::remove_var("HEW_STD"),
            }
        }

        let canon_a = tree_a.root().canonicalize().unwrap();
        let canon_b = tree_b.root().canonicalize().unwrap();
        let got_canon: Vec<_> = paths.iter().filter_map(|p| p.canonicalize().ok()).collect();
        assert!(
            got_canon.contains(&canon_a),
            "HEW_STD parent should appear in result"
        );
        assert!(
            !got_canon.contains(&canon_b),
            "source_hint tree must not leak in when HEW_STD is set"
        );
    }

    /// In-worktree (tier-2): source hint inside a Hew checkout resolves to
    /// that checkout's root only — a sibling checkout with a different std/ must
    /// not appear.  This is the contamination-repro oracle.
    #[test]
    fn tier2_source_inside_worktree_resolves_own_root_only() {
        let tree_a = TestHewTree::new("sp-worktree-a");
        let tree_b = TestHewTree::new("sp-worktree-b");

        // Write a dummy source file inside tree_a.
        let src_dir = tree_a.root().join("src");
        fs::create_dir_all(&src_dir).unwrap();
        let src_file = src_dir.join("main.hew");
        fs::write(&src_file, "// dummy\n").unwrap();

        let prev_hewpath = std::env::var("HEWPATH").ok();
        let prev_hew_std = std::env::var("HEW_STD").ok();
        // SAFETY: test process is single-threaded for env mutation.
        unsafe {
            std::env::remove_var("HEWPATH");
            std::env::remove_var("HEW_STD");
        }

        // Pass a source hint pointing inside tree_a.
        let paths = build_module_search_paths_for(Some(&src_file));

        // SAFETY: same single-threaded env-mutation guarantee as the set above.
        unsafe {
            match prev_hewpath {
                Some(v) => std::env::set_var("HEWPATH", v),
                None => std::env::remove_var("HEWPATH"),
            }
            match prev_hew_std {
                Some(v) => std::env::set_var("HEW_STD", v),
                None => std::env::remove_var("HEW_STD"),
            }
        }

        assert_eq!(
            paths.len(),
            1,
            "tier-2 must return exactly one root, got: {paths:?}"
        );
        let canon_result = paths[0].canonicalize().unwrap();
        let canon_a = tree_a.root().canonicalize().unwrap();
        let canon_b = tree_b.root().canonicalize().unwrap();
        assert_eq!(
            canon_result, canon_a,
            "source inside tree_a must resolve to tree_a root"
        );
        assert_ne!(
            canon_result, canon_b,
            "tree_b must never appear when source is inside tree_a"
        );
    }

    /// `find_enclosing_hew_root`: a path inside a Hew checkout finds the root.
    #[test]
    fn find_enclosing_hew_root_finds_ancestor_with_marker() {
        let tree = TestHewTree::new("sp-find-root");
        let nested = tree.root().join("a/b/c");
        fs::create_dir_all(&nested).unwrap();

        let result = find_enclosing_hew_root(&nested);
        assert!(result.is_some(), "should find an enclosing root");
        let canon_result = result.unwrap().canonicalize().unwrap();
        let canon_tree = tree.root().canonicalize().unwrap();
        assert_eq!(canon_result, canon_tree);
    }

    /// `find_enclosing_hew_root`: a directory tree with no `std/builtins.hew`
    /// anywhere returns None.  We test with a self-contained temp tree that is
    /// itself rooted (no further parent walk needed) by creating it under
    /// a `TestHewTree`'s `other/` sub-dir whose ancestry is the fake tree, not the
    /// real repo — so the walk hits the fake root (which has no marker for this
    /// sub-path) and stops.
    ///
    /// Actually: test a *flat* `TestDir` that has no `std/builtins.hew` and whose
    /// parent chain does not include the real repo root.  We achieve that by
    /// creating the dir directly under the OS temp dir so the walk never
    /// reaches the Hew repo root.
    #[test]
    fn find_enclosing_hew_root_returns_none_outside_checkout() {
        use std::time::{SystemTime, UNIX_EPOCH};
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        // Use the OS temp dir so the ancestor walk never reaches the Hew repo.
        let outside = std::env::temp_dir().join(format!(
            "hew-test-no-root-{}-{}",
            std::process::id(),
            unique
        ));
        fs::create_dir_all(&outside).unwrap();

        let result = find_enclosing_hew_root(&outside);

        // Clean up.
        let _ = fs::remove_dir_all(&outside);

        assert!(
            result.is_none(),
            "should return None for a directory outside any Hew checkout, got: {result:?}"
        );
    }

    /// Tier-3 fallback: with no env vars and a source path that has no
    /// enclosing Hew root, the result should be non-empty (the dev-binary
    /// fallback or global paths cover external projects).
    ///
    /// Use the OS temp dir as the "external project" so the walk never finds
    /// the Hew checkout root and tier-2 cannot fire.
    #[test]
    fn tier3_fallback_nonempty_for_external_project() {
        use std::time::{SystemTime, UNIX_EPOCH};
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let external = std::env::temp_dir().join(format!(
            "hew-test-external-{}-{}",
            std::process::id(),
            unique
        ));
        fs::create_dir_all(&external).unwrap();

        let prev_hewpath = std::env::var("HEWPATH").ok();
        let prev_hew_std = std::env::var("HEW_STD").ok();
        // SAFETY: test process is single-threaded for env mutation.
        unsafe {
            std::env::remove_var("HEWPATH");
            std::env::remove_var("HEW_STD");
        }

        let paths = build_module_search_paths_for(Some(&external));

        // Restore env and clean up temp dir.
        // SAFETY: same single-threaded env-mutation guarantee as the set above.
        unsafe {
            match prev_hewpath {
                Some(v) => std::env::set_var("HEWPATH", v),
                None => std::env::remove_var("HEWPATH"),
            }
            match prev_hew_std {
                Some(v) => std::env::set_var("HEW_STD", v),
                None => std::env::remove_var("HEW_STD"),
            }
        }
        let _ = fs::remove_dir_all(&external);

        // The dev-binary fallback (exe/../..) resolves to the actual Hew checkout
        // in this test environment, so tier-3 should be non-empty.
        assert!(
            !paths.is_empty(),
            "tier-3 should return at least the dev-build fallback path"
        );
    }
}
