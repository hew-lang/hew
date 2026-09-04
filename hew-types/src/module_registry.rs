//! On-demand module loader and cache.
//!
//! Replaces the baked-in `stdlib_generated.rs` tables. Discovers modules
//! by searching the filesystem and parsing `.hew` files at user compile time.

use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

use hew_parser::ast::Item;
use hew_parser::module::ModuleId;

use crate::stdlib_loader::{load_module_checked, ModuleInfo};

/// Parsed module data that may be reused across checker runs.
#[derive(Debug, Default)]
struct ModuleParseCache {
    modules: HashMap<ModuleId, ModuleInfo>,
}

/// Module declarations and derived metadata visible to one checked program.
#[derive(Debug, Clone, Default)]
struct ProgramModuleState {
    modules: HashMap<ModuleId, ModuleInfo>,
    handle_types: HashSet<String>,
    resource_wrapper_types: HashSet<String>,
    drop_types: HashSet<String>,
    drop_funcs: HashMap<String, String>,
}

/// On-demand module loader with a persistent parse cache and per-program state.
///
/// Replaces the baked-in `stdlib_generated.rs` tables. Discovers modules
/// by searching the filesystem and parsing `.hew` files at user compile time.
#[derive(Debug)]
pub struct ModuleRegistry {
    cache: ModuleParseCache,
    configured: ProgramModuleState,
    active: ProgramModuleState,
    /// Ordered search paths for module resolution.
    search_paths: Vec<PathBuf>,
    /// Compiler-owned root that may confer stdlib-only authority.
    ///
    /// This is resolved exclusively from the running compiler's installation
    /// or development-binary layout. It is deliberately independent of
    /// `search_paths`, which may contain project-, cwd-, or environment-owned
    /// roots.
    compiler_stdlib_root: Option<PathBuf>,
}

/// Parse a canonical dotted module identity at the registry boundary.
fn module_id_from_identity(module_path: &str) -> ModuleId {
    ModuleId::new(
        module_path
            .split('.')
            .filter(|segment| !segment.is_empty())
            .map(String::from)
            .collect(),
    )
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

/// Resolve the standard-library root owned by the running compiler binary.
///
/// Installed binaries use `<bin>/../share/hew`; development binaries (and
/// their `deps/`-nested Rust test executables) resolve from the compile-time
/// source tree, walking up from `CARGO_MANIFEST_DIR` rather than a fixed
/// parent-count offset from the executable's runtime path — that offset
/// breaks whenever `CARGO_TARGET_DIR` points out of tree. No project path,
/// cwd, or environment override participates in this decision. If neither
/// layout has the compiler's `std/builtins.hew`, authority is unavailable
/// and callers must fail closed.
#[must_use]
pub fn compiler_stdlib_root() -> Option<PathBuf> {
    let executable = std::env::current_exe().ok()?.canonicalize().ok()?;
    compiler_stdlib_root_for_executable(&executable)
}

fn compiler_stdlib_root_for_executable(executable: &std::path::Path) -> Option<PathBuf> {
    compiler_stdlib_root_impl(executable, std::path::Path::new(env!("CARGO_MANIFEST_DIR")))
}

/// Resolve the compiler-owned stdlib root from an installed-layout probe and
/// a source-tree anchor.
///
/// `manifest_dir` anchors the development tier. It must be a compile-time
/// constant (`env!("CARGO_MANIFEST_DIR")`) rather than derived from
/// `executable`'s runtime path: a fixed parent-count offset from the
/// executable assumes `target/<profile>[/deps]` sits directly under the
/// workspace root, which breaks whenever `CARGO_TARGET_DIR` points out of
/// tree — the executable then lives an arbitrary depth below the actual
/// source tree (#3086). The manifest directory is baked into the binary at
/// compile time and always names this crate's own directory in the source
/// tree, independent of where cargo placed the build output.
fn compiler_stdlib_root_impl(
    executable: &std::path::Path,
    manifest_dir: &std::path::Path,
) -> Option<PathBuf> {
    let executable_dir = executable.parent()?;
    let installed = executable_dir.parent()?.join("share/hew");
    if installed.join("std/builtins.hew").is_file() {
        return installed.canonicalize().ok();
    }

    find_enclosing_hew_root(manifest_dir)
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

    // Compiler-owned installed or development layout. This resolver is also
    // the sole source of stdlib authority inside `ModuleRegistry`.
    if let Some(root) = compiler_stdlib_root() {
        tier3.push(root);
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

/// Return the canonical dotted stdlib owner for an exact shipped source file.
///
/// Package directories are owned by their primary `{name}.hew` source, so a
/// peer file in that directory has the same owner. A directory without such a
/// primary source leaves each `.hew` file as its own module.
#[must_use]
pub fn canonical_stdlib_module_for_source(source_file: &std::path::Path) -> Option<String> {
    let input_canonical = std::fs::canonicalize(source_file).ok()?;

    build_module_search_paths_for(Some(source_file))
        .into_iter()
        .find_map(|root| {
            let root_canonical = std::fs::canonicalize(root).ok()?;
            let relative = input_canonical.strip_prefix(&root_canonical).ok()?;
            if relative
                .extension()
                .is_none_or(|extension| extension != "hew")
                || relative
                    .components()
                    .next()
                    .is_none_or(|component| component.as_os_str() != "std")
            {
                return None;
            }

            let parent = relative.parent()?;
            let parent_name = parent.file_name()?.to_str()?;
            let primary = root_canonical
                .join(parent)
                .join(format!("{parent_name}.hew"));
            let module_path = if primary.is_file() {
                parent.to_path_buf()
            } else {
                relative.with_extension("")
            };
            let dotted = module_path
                .iter()
                .map(|component| component.to_str())
                .collect::<Option<Vec<_>>>()?
                .join(".");

            is_canonical_stdlib_module_source(&input_canonical, &dotted).then_some(dotted)
        })
}

/// Whether `source_file` is the canonical source selected for `dotted_module`
/// by Hew's stdlib search-path authority.
///
/// This compares canonical paths, not filenames or generic arity. It is used
/// when a shipped stdlib file is compiled directly as a root unit but its
/// source-declared carrier types still need their standard-library identity.
#[must_use]
pub fn is_canonical_stdlib_module_source(
    source_file: &std::path::Path,
    dotted_module: &str,
) -> bool {
    canonical_stdlib_module_source_in_roots(
        source_file,
        dotted_module,
        &build_module_search_paths_for(Some(source_file)),
    )
}

fn canonical_stdlib_module_source_in_roots(
    source_file: &std::path::Path,
    dotted_module: &str,
    roots: &[PathBuf],
) -> bool {
    let Ok(input_canonical) = std::fs::canonicalize(source_file) else {
        return false;
    };
    let segments = dotted_module.split('.').collect::<Vec<_>>();
    let Some(last) = segments.last() else {
        return false;
    };
    let rel = segments.iter().collect::<PathBuf>();
    let candidates = [rel.join(format!("{last}.hew")), rel.with_extension("hew")];

    roots.iter().any(|root| {
        candidates.iter().any(|candidate| {
            std::fs::canonicalize(root.join(candidate))
                .is_ok_and(|canonical| canonical == input_canonical)
        }) || {
            // Directory modules are assembled from their primary source
            // plus peer `.hew` files.  A direct check of one such peer
            // must retain the same canonical module authority as an
            // import of the directory; comparing its canonical parent to
            // the trusted stdlib module directory keeps this provenance
            // path-based rather than granting it from a filename.
            input_canonical.extension().is_some_and(|ext| ext == "hew")
                && input_canonical.parent().is_some_and(|parent| {
                    std::fs::canonicalize(root.join(&rel))
                        .is_ok_and(|module_dir| module_dir == parent)
                })
        }
    })
}

/// Return the declaration owner selected by an import's resolved source.
///
/// `std::channel::channel` is the legacy filesystem spelling for
/// `std/channel/channel.hew`; the repeated basename does not introduce a
/// second source module. Collapse it only when the resolved path is the exact
/// shipped channel source, so an identically-spelled user package retains its
/// own nominal owner.
#[must_use]
pub fn canonical_source_module_identity(
    requested_dotted: &str,
    source_paths: &[PathBuf],
) -> String {
    if requested_dotted == "std.channel.channel"
        && source_paths
            .iter()
            .any(|source| is_canonical_stdlib_module_source(source, "std.channel"))
    {
        "std.channel".to_string()
    } else {
        requested_dotted.to_string()
    }
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
    fn module_info_declares_nominal(info: &ModuleInfo, leaf: &str) -> bool {
        info.source_items.iter().any(|(item, _)| match item {
            Item::TypeDecl(decl) => decl.name == leaf,
            Item::Record(decl) => decl.name == leaf,
            _ => false,
        })
    }

    /// Resolve the canonical owner of a type declared by an exact imported
    /// source without relying on dependency load order. Signature
    /// normalization may run before the importer's own dependencies have
    /// entered the module cache. Returning the owner (rather than a boolean)
    /// is essential for physical-path aliases such as
    /// `std.channel.channel`: declaration proof and the identity it publishes
    /// must come from the same selected source.
    fn exact_module_source_type_owner(&self, owner: &str, leaf: &str) -> Option<String> {
        let module_id = module_id_from_identity(owner);
        if let Some(info) = self
            .active
            .modules
            .get(&module_id)
            .or_else(|| self.cache.modules.get(&module_id))
        {
            if !Self::module_info_declares_nominal(info, leaf) {
                return None;
            }
            let source_paths = info.source_path.iter().cloned().collect::<Vec<_>>();
            return Some(canonical_source_module_identity(owner, &source_paths));
        }
        let loader_path = module_id_from_identity(owner).path.join("::");
        self.search_paths.iter().find_map(|search_path| {
            let info = load_module_checked(&loader_path, search_path)
                .ok()
                .flatten()?;
            if !Self::module_info_declares_nominal(&info, leaf) {
                return None;
            }
            let source_paths = info.source_path.iter().cloned().collect::<Vec<_>>();
            Some(canonical_source_module_identity(owner, &source_paths))
        })
    }

    fn receiver_spellings(info: &ModuleInfo, method_receiver: bool) -> Vec<&str> {
        let mut spellings = info
            .handle_types
            .iter()
            .map(String::as_str)
            .collect::<Vec<_>>();
        if method_receiver {
            spellings.extend(info.resource_wrapper_types.iter().map(String::as_str));
            spellings.extend(
                info.handle_methods
                    .iter()
                    .map(|method| method.type_name.as_str()),
            );
        }
        spellings.sort_unstable();
        spellings.dedup();
        spellings
    }

    /// Resolve one receiver spelling to the exact loaded declaration that
    /// owns it. A full source owner selects only that module. A legacy
    /// extracted spelling is admitted only when exactly one loaded module
    /// declares it.
    fn registry_receiver_declaration(
        &self,
        name: &str,
        method_receiver: bool,
    ) -> Option<(&ModuleId, &ModuleInfo, String)> {
        let (owner, leaf) = name.rsplit_once('.')?;
        if owner.contains('.') {
            let module_id = module_id_from_identity(owner);
            let (stored_id, info) = self.active.modules.get_key_value(&module_id)?;
            let mut matches = Self::receiver_spellings(info, method_receiver)
                .into_iter()
                .filter(|spelling| crate::short_name(spelling) == leaf)
                .map(str::to_string)
                .collect::<Vec<_>>();
            matches.sort_unstable();
            matches.dedup();
            return match matches.as_slice() {
                [only] => Some((stored_id, info, only.clone())),
                _ => None,
            };
        }

        let mut matches = self
            .active
            .modules
            .iter()
            .filter(|(_, info)| {
                Self::receiver_spellings(info, method_receiver)
                    .into_iter()
                    .any(|spelling| spelling == name)
            })
            .map(|(module_id, info)| (module_id, info, name.to_string()))
            .collect::<Vec<_>>();
        matches.sort_unstable_by(|left, right| left.0.path.cmp(&right.0.path));
        match matches.as_slice() {
            [only] => Some((only.0, only.1, only.2.clone())),
            _ => None,
        }
    }

    /// Resolve a source-canonical handle identity to the registry's extracted
    /// spelling. The loader's extracted surfaces retain their historical
    /// short module owner (`net.Listener`), while Hew source declarations use
    /// the full module graph owner (`std.net.Listener`).
    ///
    /// This is deliberately an exact `(loaded module path, type leaf)` join:
    /// `std.net.Listener` may project to the `std::net` extraction, but
    /// `acme.net.Listener` and a bare `Listener` never do. It is a bridge
    /// between two representations of one loaded declaration, not a
    /// same-leaf fallback across modules.
    fn registry_handle_spelling(&self, name: &str) -> Option<String> {
        self.registry_receiver_declaration(name, false)
            .map(|(_, _, spelling)| spelling)
    }

    /// Resolve an exact source-owned receiver type to the registry spelling
    /// used by extracted method signatures. Unlike `registry_handle_spelling`,
    /// this also covers fielded resource wrappers: they may publish method
    /// signatures, but must never be classified as fieldless opaque handles.
    fn registry_method_receiver_spelling(&self, name: &str) -> Option<String> {
        self.registry_receiver_declaration(name, true)
            .map(|(_, _, spelling)| spelling)
    }

    /// Create a new registry with the given search paths.
    ///
    /// Search paths are tried in order during module resolution — first match wins.
    #[must_use]
    pub fn new(search_paths: Vec<PathBuf>) -> Self {
        Self {
            cache: ModuleParseCache::default(),
            configured: ProgramModuleState::default(),
            active: ProgramModuleState::default(),
            search_paths,
            compiler_stdlib_root: compiler_stdlib_root(),
        }
    }

    /// Retain parsed module data while discarding every resolution surface from
    /// the completed program. Explicit caller configuration is re-seeded.
    pub(crate) fn for_new_program(self) -> Self {
        let Self {
            cache,
            configured,
            active: _,
            search_paths,
            compiler_stdlib_root,
        } = self;
        let active = configured.clone();
        Self {
            cache,
            configured,
            active,
            search_paths,
            compiler_stdlib_root,
        }
    }

    pub(crate) fn has_search_paths(&self) -> bool {
        !self.search_paths.is_empty()
    }

    /// Whether `source_file` is the exact source for `dotted_module` below the
    /// running compiler's own stdlib root.
    ///
    /// Module search paths are intentionally excluded: projects, cwd, `HEWPATH`,
    /// and `HEW_STD` may affect resolution but cannot confer compiler authority.
    /// A missing compiler-owned root returns `false`.
    pub(crate) fn source_has_stdlib_authority(
        &self,
        source_file: &std::path::Path,
        dotted_module: &str,
    ) -> bool {
        self.compiler_stdlib_root.as_ref().is_some_and(|root| {
            canonical_stdlib_module_source_in_roots(
                source_file,
                dotted_module,
                std::slice::from_ref(root),
            )
        })
    }

    /// Load a module by its full path (e.g. `std::encoding::json`).
    ///
    /// If the module is already cached, returns the cached version.
    /// Otherwise, iterates search paths and delegates to `stdlib_loader::load_module`
    /// for resolution and parsing.
    ///
    /// On success, the module's handle types and drop types are accumulated into
    /// the active program's registry sets.
    ///
    /// # Errors
    ///
    /// Returns [`ModuleError::NotFound`] if no search path contains the module,
    /// or [`ModuleError::ParseError`] if the module file exists but cannot be parsed.
    ///
    /// # Panics
    ///
    /// Panics (fail-closed) if a newly loaded module makes a fielded
    /// `#[resource]` handle-wrapper share its short name with a fieldless
    /// `#[opaque]` handle in the loaded set — an internal stdlib-authoring
    /// invariant that would otherwise let handle-method dispatch misclassify the
    /// wrapper as an opaque handle. The current stdlib satisfies it.
    pub fn load(&mut self, module_path: &str) -> Result<&ModuleInfo, ModuleError> {
        let id = module_id_from_identity(module_path);
        let loader_path = id.path.join("::");

        if self.active.modules.contains_key(&id) {
            return Ok(&self.active.modules[&id]);
        }
        if let Some(info) = self.cache.modules.get(&id).cloned() {
            return Ok(self.activate_module(&id, info));
        }

        for search_path in &self.search_paths {
            if let Some(info) = load_module_checked(&loader_path, search_path)? {
                let source_paths = info.source_path.iter().cloned().collect::<Vec<_>>();
                let canonical_owner =
                    canonical_source_module_identity(&id.path.join("."), &source_paths);
                let canonical_id = module_id_from_identity(&canonical_owner);
                self.cache
                    .modules
                    .insert(canonical_id.clone(), info.clone());
                return Ok(self.activate_module(&canonical_id, info));
            }
        }

        Err(ModuleError::NotFound {
            module_path: module_path.to_string(),
            searched: self.search_paths.clone(),
        })
    }

    fn activate_module(&mut self, id: &ModuleId, info: ModuleInfo) -> &ModuleInfo {
        self.active
            .handle_types
            .extend(info.handle_types.iter().cloned());
        self.active
            .resource_wrapper_types
            .extend(info.resource_wrapper_types.iter().cloned());
        self.active
            .drop_types
            .extend(info.drop_types.iter().cloned());
        self.active
            .drop_funcs
            .extend(info.drop_funcs.iter().cloned());

        if let Some((wrapper, handle)) = crate::stdlib_loader::resource_wrapper_shadowing_handle(
            &self.active.handle_types,
            &self.active.resource_wrapper_types,
        ) {
            panic!(
                "stdlib invariant violated: #[resource] handle-wrapper `{wrapper}` \
               shares its short name with fieldless #[opaque] handle `{handle}` — \
               rename one so handle-method dispatch cannot misclassify the wrapper \
               as an opaque handle"
            );
        }

        self.active.modules.insert(id.clone(), info);
        &self.active.modules[id]
    }

    /// Return cached module info if it has already been loaded.
    #[must_use]
    pub fn get(&self, module_path: &str) -> Option<&ModuleInfo> {
        let id = module_id_from_identity(module_path);
        self.active.modules.get(&id)
    }

    /// Check if a fully-qualified name is a handle type across all loaded modules.
    #[must_use]
    pub fn is_handle_type(&self, name: &str) -> bool {
        self.registry_handle_spelling(name).is_some()
    }

    /// Resolve an opaque handle spelling to its exact loaded source identity.
    #[must_use]
    pub fn canonical_handle_type_identity(&self, name: &str) -> Option<String> {
        let (module_id, _, spelling) = self.registry_receiver_declaration(name, false)?;
        Some(format!(
            "{}.{}",
            module_id.path.join("."),
            crate::short_name(&spelling)
        ))
    }

    /// Whether `name` is an exact source receiver represented by a loaded
    /// registry module. This includes fielded resource wrappers as well as
    /// fieldless opaque handles.
    #[must_use]
    pub fn is_method_receiver_type(&self, name: &str) -> bool {
        self.registry_method_receiver_spelling(name).is_some()
    }

    /// Resolve a registry receiver spelling to its exact loaded source owner.
    /// Returns `None` when more than one loaded module could own the extracted
    /// spelling, preserving fail-closed nominal identity.
    #[must_use]
    pub fn canonical_method_receiver_identity(&self, name: &str) -> Option<String> {
        let (module_id, _, spelling) = self.registry_receiver_declaration(name, true)?;
        let leaf = crate::short_name(&spelling);
        Some(format!("{}.{leaf}", module_id.path.join(".")))
    }

    /// Resolve an owned registry receiver to its exact loaded source identity.
    ///
    /// Ownership metadata is extracted under the registry spelling
    /// (`regex.Pattern`), while source annotations carry the complete owner
    /// (`std.text.regex.Pattern`). This joins only those two representations of
    /// the same loaded declaration; it never recovers an owner from a leaf.
    #[must_use]
    pub fn canonical_owned_type_identity(&self, name: &str) -> Option<String> {
        let (module_id, info, spelling) = self.registry_receiver_declaration(name, true)?;
        (info.handle_types.contains(&spelling)
            || info.resource_wrapper_types.contains(&spelling)
            || info.drop_types.contains(&spelling)
            || info.drop_funcs.iter().any(|(ty, _)| ty == &spelling))
        .then(|| {
            format!(
                "{}.{}",
                module_id.path.join("."),
                crate::short_name(&spelling)
            )
        })
    }

    /// Project a legacy registry signature type into its exact source owner.
    ///
    /// Extracted ABI signatures historically use the loaded module's final
    /// path segment (`regex.Pattern`). The projection is authorized only when
    /// that same loaded source declares the requested type leaf. A foreign
    /// `regex.Other` spelling therefore cannot acquire `std.text.regex` as an
    /// owner merely because its prefix has the same text.
    #[must_use]
    pub fn canonical_registry_signature_type_identity(
        &self,
        name: &str,
        canonical_owner: &str,
    ) -> Option<String> {
        let info = self
            .active
            .modules
            .get(&module_id_from_identity(canonical_owner))?;
        let extracted_owner = canonical_owner
            .rsplit_once('.')
            .map_or(canonical_owner, |(_, leaf)| leaf);
        if let Some(leaf) = name.strip_prefix(&format!("{extracted_owner}.")) {
            if !leaf.contains('.') && Self::module_info_declares_nominal(info, leaf) {
                return Some(format!("{canonical_owner}.{leaf}"));
            }
        }

        let (binding, leaf) = name.split_once('.')?;
        if leaf.contains('.') {
            return None;
        }
        info.source_items.iter().find_map(|(item, _)| {
            let Item::Import(import) = item else {
                return None;
            };
            let import_binding = import
                .module_alias
                .as_deref()
                .or_else(|| import.path.last().map(String::as_str))?;
            if import_binding != binding {
                return None;
            }
            let imported_owner = import.path.join(".");
            self.exact_module_source_type_owner(&imported_owner, leaf)
                .map(|canonical_owner| format!("{canonical_owner}.{leaf}"))
        })
    }

    /// Recursively project every declaration-proven registry signature type
    /// into the exact source owner of the loaded module.
    #[must_use]
    pub fn canonicalize_registry_signature_ty(
        &self,
        ty: &crate::ty::Ty,
        canonical_owner: &str,
    ) -> crate::ty::Ty {
        let mapped = ty.map_children_pub(&|child| {
            self.canonicalize_registry_signature_ty(child, canonical_owner)
        });
        let crate::ty::Ty::Named {
            name,
            args,
            builtin,
        } = mapped
        else {
            return mapped;
        };
        let name = self
            .canonical_registry_signature_type_identity(&name, canonical_owner)
            .unwrap_or(name);
        crate::ty::Ty::Named {
            name,
            args,
            builtin,
        }
    }

    /// Check if a fully-qualified name is a drop type across all loaded modules.
    #[must_use]
    pub fn is_drop_type(&self, name: &str) -> bool {
        self.active.drop_types.contains(name)
    }

    /// Return the C drop function for a fully-qualified type name, if known.
    ///
    /// Only populated for types with an `impl Drop` block whose `fn drop` body
    /// is a direct C call (the common stdlib pattern).
    #[must_use]
    pub fn drop_func_for(&self, type_name: &str) -> Option<&str> {
        self.active.drop_funcs.get(type_name).map(String::as_str)
    }

    /// Return all `(type_name, c_drop_func)` pairs from all loaded modules.
    #[must_use]
    pub fn all_drop_funcs(&self) -> Vec<(String, String)> {
        self.active
            .drop_funcs
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect()
    }

    /// Return a known handle declaration only when its full path is supplied.
    ///
    /// A leaf spelling has no authority to select a module declaration.
    #[must_use]
    pub fn qualify_handle_type(&self, name: &str) -> Option<String> {
        self.canonical_handle_type_identity(name)
    }

    /// Return all handle types from all loaded modules.
    #[must_use]
    pub fn all_handle_types(&self) -> Vec<String> {
        self.active.handle_types.iter().cloned().collect()
    }

    /// Resolve a module-qualified call to a C symbol.
    ///
    /// A full module path (e.g. `std::encoding::json`) selects that exact loaded
    /// module. Import aliases are resolved to that owner by the checker before
    /// this boundary; a leaf spelling has no authority even when it happens to
    /// be unique among currently loaded modules.
    #[must_use]
    pub fn resolve_module_call(&self, module_path: &str, method: &str) -> Option<String> {
        let symbol_for = |info: &ModuleInfo| {
            info.clean_names
                .iter()
                .find(|(clean, _)| clean == method)
                .map(|(_, c_sym)| c_sym.clone())
        };

        let exact_id = module_id_from_identity(module_path);
        self.active.modules.get(&exact_id).and_then(symbol_for)
    }

    /// Resolve a handle method to its C symbol.
    ///
    /// Searches all loaded modules' `handle_methods` for a match on
    /// `(handle_type, method)`.
    ///
    /// Requires the fully-qualified handle declaration name (`json.Value`).
    #[must_use]
    pub fn resolve_handle_method(&self, handle_type: &str, method: &str) -> Option<String> {
        self.resolve_handle_method_sig(handle_type, method)
            .map(|(c_sym, _, _, _)| c_sym)
    }

    /// Whether a registry-visible handle method must dispatch through its Hew
    /// impl instead of rewriting directly to the extracted C symbol.
    #[must_use]
    pub fn handle_method_dispatches_through_impl(&self, handle_type: &str, method: &str) -> bool {
        let Some((_, info, spelling)) = self.registry_receiver_declaration(handle_type, true)
        else {
            return false;
        };
        info.handle_methods.iter().any(|hm| {
            hm.type_name == spelling && hm.method_name == method && hm.dispatch_through_impl
        })
    }

    /// Resolve a handle method to its C symbol and extracted signature.
    ///
    /// Returns `(c_symbol, param_types, return_type, canonical_owner)` for a
    /// trivial extracted handle method selected from one exact loaded module.
    #[must_use]
    pub fn resolve_handle_method_sig(
        &self,
        handle_type: &str,
        method: &str,
    ) -> Option<(String, Vec<crate::ty::Ty>, crate::ty::Ty, String)> {
        let (module_id, info, spelling) = self.registry_receiver_declaration(handle_type, true)?;
        let canonical_owner = module_id.path.join(".");
        let hm = info
            .handle_methods
            .iter()
            .find(|hm| hm.type_name == spelling && hm.method_name == method)?;
        let params = hm
            .params
            .iter()
            .map(|ty| self.canonicalize_registry_signature_ty(ty, &canonical_owner))
            .collect();
        let return_type =
            self.canonicalize_registry_signature_ty(&hm.return_type, &canonical_owner);
        Some((hm.c_symbol.clone(), params, return_type, canonical_owner))
    }

    /// Seed a fully-qualified handle type name for unit tests.
    ///
    /// Bypasses module loading so tests can populate `handle_types` without
    /// requiring real `.hew` module files on disk.
    #[cfg(test)]
    pub(crate) fn insert_handle_type_for_test(&mut self, qualified_name: String) {
        self.active.handle_types.insert(qualified_name.clone());
        let owner = qualified_name
            .rsplit_once('.')
            .map_or("test_handles", |(owner, _)| owner);
        let info = self
            .active
            .modules
            .entry(module_id_from_identity(owner))
            .or_insert_with(|| ModuleInfo {
                source_path: None,
                source_items: Vec::new(),
                functions: Vec::new(),
                clean_names: Vec::new(),
                handle_types: Vec::new(),
                handle_methods: Vec::new(),
                wrapper_fns: Vec::new(),
                drop_types: Vec::new(),
                resource_wrapper_types: Vec::new(),
                drop_funcs: Vec::new(),
                unsupported_type_signatures: Vec::new(),
            });
        if !info.handle_types.contains(&qualified_name) {
            info.handle_types.push(qualified_name);
        }
        self.configured = self.active.clone();
    }

    #[cfg(test)]
    pub(crate) fn insert_module_info_for_test(&mut self, canonical_owner: &str, info: ModuleInfo) {
        self.active
            .modules
            .insert(module_id_from_identity(canonical_owner), info);
        self.configured = self.active.clone();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::time::{SystemTime, UNIX_EPOCH};

    #[test]
    fn canonical_stdlib_owner_follows_flat_package_and_peer_layouts() {
        let stdlib = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../std");
        assert_eq!(
            canonical_stdlib_module_for_source(&stdlib.join("string.hew")).as_deref(),
            Some("std.string")
        );
        assert_eq!(
            canonical_stdlib_module_for_source(&stdlib.join("net/http/http.hew")).as_deref(),
            Some("std.net.http")
        );
        assert_eq!(
            canonical_stdlib_module_for_source(&stdlib.join("net/http/http_client.hew")).as_deref(),
            Some("std.net.http")
        );
        assert_eq!(
            canonical_stdlib_module_for_source(&stdlib.join("io/scanner.hew")).as_deref(),
            Some("std.io.scanner")
        );
    }

    #[test]
    fn canonical_stdlib_owner_rejects_a_user_lookalike() {
        let user_dir = TestDir::new("module-registry-user-stdlib-lookalike");
        let user_source = user_dir.root.join("std/string.hew");
        fs::create_dir_all(user_source.parent().expect("lookalike has a parent"))
            .expect("create lookalike directory");
        fs::write(&user_source, "pub fn len() -> i64 { 0 }\n").expect("write lookalike source");

        assert_eq!(canonical_stdlib_module_for_source(&user_source), None);
    }

    #[test]
    fn channel_repeated_basename_maps_only_exact_shipped_source_to_canonical_owner() {
        let shipped = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("..")
            .join("std/channel/channel.hew");
        assert_eq!(
            canonical_source_module_identity("std.channel.channel", &[shipped]),
            "std.channel"
        );

        let user_lookalike = std::env::temp_dir().join("user/std/channel/channel.hew");
        assert_eq!(
            canonical_source_module_identity("std.channel.channel", &[user_lookalike]),
            "std.channel.channel",
            "a same-spelled user module must retain its own nominal owner"
        );
        assert_eq!(
            canonical_source_module_identity("std.channel", &[]),
            "std.channel",
            "the canonical owner is already stable"
        );
    }

    #[test]
    fn channel_legacy_request_resolves_source_before_consulting_canonical_cache() {
        let mut reg = registry();
        reg.load("std.channel")
            .expect("prime the canonical shipped channel owner");
        let shipped = reg
            .get("std.channel")
            .and_then(|info| info.source_path.clone())
            .expect("canonical cache entry has a source path");

        let user_dir = TestDir::new("module-registry-user-channel-lookalike");
        let user_channel_dir = user_dir.root.join("std/channel");
        fs::create_dir_all(&user_channel_dir).expect("create user channel module directory");
        let user_source = user_channel_dir.join("channel.hew");
        fs::write(
            &user_source,
            "pub type Sender { marker: i64; }\npub type Receiver { marker: i64; }\n",
        )
        .expect("write user channel lookalike");

        // Model a new importer resolution context while preserving the cache.
        // The legacy request must resolve this source before deciding whether
        // its repeated basename denotes the canonical shipped owner.
        reg.search_paths = vec![user_dir.root.clone()];
        let loaded = reg
            .load("std.channel.channel")
            .expect("load the user lookalike through the legacy spelling");
        assert_eq!(loaded.source_path.as_deref(), Some(user_source.as_path()));
        assert_eq!(
            reg.get("std.channel.channel")
                .and_then(|info| info.source_path.as_deref()),
            Some(user_source.as_path()),
            "the user source keeps the nested nominal owner"
        );
        assert_eq!(
            reg.get("std.channel")
                .and_then(|info| info.source_path.as_deref()),
            Some(shipped.as_path()),
            "resolving a lookalike must not replace the proven shipped owner"
        );
    }

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
    fn compiler_stdlib_root_prefers_installed_layout_over_source_anchor() {
        let installed = TestDir::new("compiler-stdlib-installed");
        let installed_root = installed.root.join("share/hew");
        fs::create_dir_all(installed_root.join("std")).unwrap();
        fs::write(
            installed_root.join("std/builtins.hew"),
            "// installed compiler std\n",
        )
        .unwrap();
        let installed_executable = installed.root.join("bin/hew");
        // The manifest_dir anchor is irrelevant here: the installed tier
        // wins whenever the executable's own `../share/hew` layout exists.
        let unrelated_manifest_dir = TestDir::new("compiler-stdlib-unrelated-manifest");
        assert_eq!(
            compiler_stdlib_root_impl(&installed_executable, &unrelated_manifest_dir.root),
            installed_root.canonicalize().ok()
        );
    }

    #[test]
    fn compiler_stdlib_root_anchors_development_tier_on_source_tree_not_executable_offset() {
        // A development stdlib root is discovered by walking up from the
        // compile-time manifest directory, never from a fixed parent-count
        // offset baked onto the executable's runtime location. Proof: an
        // executable path nested arbitrarily deep below an unrelated
        // directory (simulating an out-of-tree CARGO_TARGET_DIR) still
        // resolves correctly because only manifest_dir determines the
        // development tier.
        let development = TestDir::new("compiler-stdlib-development");
        fs::create_dir_all(development.root.join("std")).unwrap();
        fs::write(
            development.root.join("std/builtins.hew"),
            "// development compiler std\n",
        )
        .unwrap();
        let child_manifest_dir = development.root.join("some-crate");
        fs::create_dir_all(&child_manifest_dir).unwrap();

        let out_of_tree_executable = TestDir::new("compiler-stdlib-out-of-tree-targets")
            .root
            .join("deeply/nested/unrelated/path/deps/hew_types-abc123");
        assert_eq!(
            compiler_stdlib_root_impl(&out_of_tree_executable, &child_manifest_dir),
            development.root.canonicalize().ok(),
            "the development tier must resolve from manifest_dir, not the executable path"
        );
    }

    #[test]
    fn compiler_stdlib_root_fails_closed_without_either_layout() {
        // `TestDir` lives under this checkout's own `target/`, so the
        // ancestor walk would otherwise find this repo's real
        // `std/builtins.hew` and defeat the fail-closed proof. Anchor the
        // manifest_dir outside the checkout, exactly like
        // `find_enclosing_hew_root_returns_none_outside_checkout` does.
        let missing = TestDir::new("compiler-stdlib-missing");
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let missing_manifest_dir = std::env::temp_dir().join(format!(
            "hew-test-compiler-stdlib-missing-manifest-{}-{unique}",
            std::process::id()
        ));
        fs::create_dir_all(&missing_manifest_dir).unwrap();
        let result =
            compiler_stdlib_root_impl(&missing.root.join("bin/hew"), &missing_manifest_dir);
        let _ = fs::remove_dir_all(&missing_manifest_dir);
        assert_eq!(
            result, None,
            "a compiler without its own stdlib layout must fail closed"
        );
    }

    #[test]
    fn load_json_module() {
        let mut reg = registry();
        let info = reg.load("std.encoding.json").unwrap();
        assert!(!info.functions.is_empty(), "json should have functions");
        assert!(
            info.handle_types.contains(&"json.Value".to_string()),
            "json should declare json.Value"
        );
    }

    #[test]
    fn load_caches_result() {
        let mut reg = registry();
        reg.load("std.encoding.json").unwrap();
        // Second call should return cached.
        let info = reg.get("std.encoding.json");
        assert!(info.is_some(), "should be cached after load");
    }

    #[test]
    fn load_nonexistent_returns_not_found() {
        let mut reg = registry();
        let err = reg.load("std.does.not.exist").unwrap_err();
        match err {
            ModuleError::NotFound {
                module_path,
                searched,
            } => {
                assert_eq!(module_path, "std.does.not.exist");
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
        let err = reg.load("std.broken").unwrap_err();
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
            reg.get("std.broken").is_none(),
            "malformed modules must not be cached or loaded from later search paths"
        );
    }

    #[test]
    fn handle_types_accumulated() {
        let mut reg = registry();
        reg.load("std.encoding.json").unwrap();
        assert!(
            reg.is_handle_type("json.Value"),
            "json.Value should be a handle type"
        );
        assert!(
            !reg.is_handle_type("http.Request"),
            "http.Request should not be loaded yet"
        );

        // Load another module — types accumulate.
        reg.load("std.net.http").unwrap();
        assert!(reg.is_handle_type("json.Value"), "json.Value still present");
        assert!(
            !reg.is_handle_type("http.Request"),
            "http.Request is a fielded resource wrapper, not an opaque handle"
        );
    }

    #[test]
    fn drop_types_accumulated() {
        let mut reg = registry();
        reg.load("std.encoding.json").unwrap();
        assert!(
            reg.is_drop_type("json.Value"),
            "json.Value is a `#[resource]` handle, so it is a drop type"
        );
        reg.load("std.net.http").unwrap();
        assert!(
            reg.is_drop_type("http.Request"),
            "http.Request is a `#[resource]` handle, so it is a drop type"
        );
        reg.load("std.process").unwrap();
        assert!(
            reg.is_drop_type("process.Child"),
            "process.Child should be a drop type"
        );
        assert!(
            reg.is_drop_type("http.Server"),
            "http.Server is now a closeable opaque resource"
        );
        reg.load("std.text.regex").unwrap();
        assert!(
            reg.is_drop_type("regex.Pattern"),
            "regex.Pattern is a `#[resource]` handle, so it is a drop type"
        );
    }

    #[test]
    fn drop_funcs_accumulated() {
        let mut reg = registry();
        reg.load("std.encoding.json").unwrap();
        assert_eq!(
            reg.drop_func_for("json.Value"),
            Some("hew_json_free"),
            "json.Value.close directly forwards to its sole raw disposer"
        );
        reg.load("std.net.http").unwrap();
        assert_eq!(
            reg.drop_func_for("http.Request"),
            None,
            "http.Request should not have a drop func"
        );
        reg.load("std.process").unwrap();
        assert_eq!(
            reg.drop_func_for("process.Child"),
            None,
            "process.Child is a fielded resource record; its generated drop \
             dispatches through Child::close instead of a direct opaque-handle \
             registry drop function"
        );
        assert!(
            reg.is_drop_type("process.Child"),
            "process.Child must remain a drop type even without a direct \
             opaque-handle drop function"
        );
        let process_source = include_str!("../../std/process.hew");
        assert!(
            process_source.contains("fn close(child: Child)"),
            "process.Child must retain its source-level resource close method"
        );
        assert_eq!(
            process_source
                .matches("hew_process_drop(child.handle)")
                .count(),
            1,
            "process.Child::close must release its wrapped ChildHandle exactly once"
        );
        assert_eq!(
            reg.drop_func_for("http.Server"),
            Some("hew_http_server_close"),
            "http.Server must use its sole raw disposer"
        );
        reg.load("std.text.regex").unwrap();
        assert_eq!(
            reg.drop_func_for("regex.Pattern"),
            None,
            "regex.Pattern should not have a drop func"
        );
        let all = reg.all_drop_funcs();
        for expected in [
            ("json.Value".to_string(), "hew_json_free".to_string()),
            (
                "http.Server".to_string(),
                "hew_http_server_close".to_string(),
            ),
            (
                "process.ProcessResultHandle".to_string(),
                "hew_process_result_free".to_string(),
            ),
        ] {
            assert!(
                all.contains(&expected),
                "missing direct disposer {expected:?}: {all:?}"
            );
        }
    }

    #[test]
    fn registry_signature_owner_projection_requires_exact_source_declaration() {
        let mut reg = registry();
        reg.load("std.text.regex").unwrap();

        assert_eq!(
            reg.canonical_registry_signature_type_identity("regex.Pattern", "std.text.regex"),
            Some("std.text.regex.Pattern".to_string()),
            "the loaded Pattern declaration authorizes its legacy registry spelling"
        );
        assert_eq!(
            reg.canonical_registry_signature_type_identity("regex.Foreign", "std.text.regex"),
            None,
            "a foreign regex.X spelling must not inherit the loaded module owner"
        );
        assert_eq!(
            reg.canonical_registry_signature_type_identity("regex.Pattern", "vendor.regex"),
            None,
            "a declaration loaded for one exact owner cannot authorize another"
        );
    }

    #[test]
    fn imported_registry_signature_uses_source_resolved_physical_alias_owner() {
        let fixture = TestDir::new("registry-signature-channel-physical-alias");
        fs::write(
            fixture.root.join("signature_importer.hew"),
            "import std.channel.channel as ch;\n",
        )
        .expect("write signature importer");

        let mut search_paths = vec![fixture.root.clone()];
        search_paths.push(test_root());
        let mut reg = ModuleRegistry::new(search_paths);
        reg.load("signature_importer")
            .expect("load physical-alias importer");

        assert_eq!(
            reg.canonical_registry_signature_type_identity("ch.Sender", "signature_importer",),
            Some("std.channel.Sender".to_string()),
            "the selected shipped source collapses its repeated physical basename"
        );
        assert_eq!(
            reg.canonical_registry_signature_type_identity("ch.Foreign", "signature_importer",),
            None,
            "an imported qualifier cannot authorize a type absent from that exact source"
        );
    }

    #[test]
    fn imported_registry_signature_user_lookalike_is_order_independent() {
        let mut reg = registry();
        reg.load("std.channel")
            .expect("prime canonical shipped channel cache");

        let fixture = TestDir::new("registry-signature-channel-user-lookalike");
        let channel_dir = fixture.root.join("std/channel");
        fs::create_dir_all(&channel_dir).expect("create user channel path");
        fs::write(
            channel_dir.join("channel.hew"),
            "pub type Sender { marker: i64; }\n",
        )
        .expect("write user channel lookalike");
        fs::write(
            fixture.root.join("signature_importer.hew"),
            "import std.channel.channel as ch;\n",
        )
        .expect("write user signature importer");

        // Model a later importer with a different exact resolution context.
        // The already-cached shipped canonical owner must not grant authority
        // to this same-spelled user source or rewrite it back to std.channel.
        reg.search_paths = vec![fixture.root.clone()];
        reg.load("signature_importer")
            .expect("load user-lookalike importer");

        assert_eq!(
            reg.canonical_registry_signature_type_identity("ch.Sender", "signature_importer",),
            Some("std.channel.channel.Sender".to_string()),
            "the user source retains its nested nominal owner despite shipped-cache order"
        );
        assert_eq!(
            reg.canonical_registry_signature_type_identity("ch.Receiver", "signature_importer",),
            None,
            "the shipped Receiver declaration must not leak through the canonical cache"
        );
    }

    #[test]
    fn registry_signature_owner_projection_accepts_exact_record_declarations_only() {
        let fixture = TestDir::new("registry-signature-record-owner");
        fs::write(
            fixture.root.join("record_owner.hew"),
            "pub type Packet { value: i64 }\n",
        )
        .expect("write record owner");
        fs::write(
            fixture.root.join("record_importer.hew"),
            "import record_owner as packets;\n",
        )
        .expect("write record importer");
        fs::write(
            fixture.root.join("lookalike.hew"),
            "pub type Packet { other: string }\n",
        )
        .expect("write record lookalike");

        let mut reg = ModuleRegistry::new(vec![fixture.root.clone()]);
        reg.load("record_owner").expect("load record owner");
        reg.load("lookalike").expect("load record lookalike");
        reg.load("record_importer").expect("load record importer");

        assert_eq!(
            reg.canonical_registry_signature_type_identity("record_owner.Packet", "record_owner"),
            Some("record_owner.Packet".to_string()),
            "an exact same-module record declaration authorizes its owner"
        );
        assert_eq!(
            reg.canonical_registry_signature_type_identity("packets.Packet", "record_importer"),
            Some("record_owner.Packet".to_string()),
            "an imported record signature uses the exact resolved declaration owner"
        );
        assert_eq!(
            reg.canonical_registry_signature_type_identity("packets.Missing", "record_importer"),
            None,
            "an absent nominal cannot borrow the imported record owner"
        );
        assert_eq!(
            reg.canonical_registry_signature_type_identity("lookalike.Packet", "record_importer"),
            None,
            "a loaded same-leaf lookalike has no authority without an exact import binding"
        );
    }

    #[test]
    fn resolve_module_call_json_parse() {
        let mut reg = registry();
        reg.load("std.encoding.json").unwrap();
        let c_sym = reg.resolve_module_call("std.encoding.json", "parse");
        assert!(
            c_sym.is_some(),
            "should resolve the exact json module owner"
        );
        assert_eq!(
            reg.resolve_module_call("json", "parse"),
            None,
            "the json leaf must not recover the loaded module owner"
        );
    }

    #[test]
    fn resolve_module_call_uses_exact_paths_and_rejects_all_short_names() {
        fn module_info(method: &str, c_symbol: &str) -> ModuleInfo {
            ModuleInfo {
                source_path: None,
                source_items: Vec::new(),
                functions: Vec::new(),
                clean_names: vec![(method.to_string(), c_symbol.to_string())],
                handle_types: Vec::new(),
                handle_methods: Vec::new(),
                wrapper_fns: Vec::new(),
                drop_types: Vec::new(),
                resource_wrapper_types: Vec::new(),
                drop_funcs: Vec::new(),
                unsupported_type_signatures: Vec::new(),
            }
        }

        let mut reg = ModuleRegistry::new(Vec::new());
        reg.active.modules.insert(
            ModuleId::new(vec!["vendor_a".into(), "nested".into(), "shared".into()]),
            module_info("run", "vendor_a_shared_run"),
        );
        reg.active.modules.insert(
            ModuleId::new(vec!["vendor_b".into(), "nested".into(), "shared".into()]),
            module_info("run", "vendor_b_shared_run"),
        );
        reg.active.modules.insert(
            ModuleId::new(vec!["vendor_c".into(), "nested".into(), "unique".into()]),
            module_info("run", "vendor_c_unique_run"),
        );

        assert_eq!(
            reg.resolve_module_call("vendor_a.nested.shared", "run"),
            Some("vendor_a_shared_run".to_string())
        );
        assert_eq!(
            reg.resolve_module_call("vendor_b.nested.shared", "run"),
            Some("vendor_b_shared_run".to_string())
        );
        assert_eq!(
            reg.resolve_module_call("vendor_a.nested.shared", "run"),
            Some("vendor_a_shared_run".to_string()),
            "a canonical dotted owner must select the exact registry module"
        );
        assert_eq!(
            reg.resolve_module_call("shared", "run"),
            None,
            "an ambiguous leaf must not select the hash map's first match"
        );
        assert_eq!(
            reg.resolve_module_call("unique", "run"),
            None,
            "even an unambiguous leaf has no module-selection authority"
        );
        assert_eq!(
            reg.resolve_module_call("missing.nested.unique", "run"),
            None,
            "a qualified miss must not fall back to its unique leaf"
        );
        assert_eq!(reg.resolve_module_call("missing", "run"), None);
    }

    #[test]
    fn resolve_handle_method_json_value() {
        let mut reg = registry();
        reg.load("std.encoding.json").unwrap();
        // json.Value should have handle methods from its impl block.
        let info = reg.get("std.encoding.json").unwrap();
        if !info.handle_methods.is_empty() {
            let hm = &info.handle_methods[0];
            let c_sym = reg.resolve_handle_method(&hm.type_name, &hm.method_name);
            assert!(c_sym.is_some(), "should resolve handle method");
        }
    }

    #[test]
    fn resolve_handle_method_rejects_short_handle_name() {
        let mut reg = registry();
        reg.load("std.encoding.json").unwrap();
        let info = reg.get("std.encoding.json").unwrap();
        if let Some(hm) = info.handle_methods.first() {
            let short = crate::short_name(&hm.type_name);
            let c_sym = reg.resolve_handle_method(short, &hm.method_name);
            assert!(
                c_sym.is_none(),
                "short handle name must not retry a qualified declaration"
            );
        }
    }

    #[test]
    fn same_legacy_receiver_spelling_never_cross_wires_loaded_modules() {
        fn shared_info(c_symbol: &str, dispatch_through_impl: bool) -> ModuleInfo {
            let parsed = hew_parser::parse("pub type Pattern { value: i32; }\n");
            assert!(parsed.errors.is_empty());
            ModuleInfo {
                source_path: None,
                source_items: parsed.program.items,
                functions: Vec::new(),
                clean_names: Vec::new(),
                handle_types: vec!["regex.Pattern".to_string()],
                handle_methods: vec![crate::stdlib_loader::HandleMethod {
                    type_name: "regex.Pattern".to_string(),
                    method_name: "clone_for_test".to_string(),
                    c_symbol: c_symbol.to_string(),
                    params: vec![
                        crate::ty::Ty::option(crate::ty::Ty::named("regex.Pattern", vec![])),
                        crate::ty::Ty::named("regex.Foreign", vec![]),
                    ],
                    return_type: crate::ty::Ty::option(crate::ty::Ty::named(
                        "regex.Pattern",
                        vec![],
                    )),
                    dispatch_through_impl,
                }],
                wrapper_fns: Vec::new(),
                drop_types: vec!["regex.Pattern".to_string()],
                resource_wrapper_types: Vec::new(),
                drop_funcs: Vec::new(),
                unsupported_type_signatures: Vec::new(),
            }
        }

        let mut reg = ModuleRegistry::new(Vec::new());
        reg.active.modules.insert(
            module_id_from_identity("vendor_a.text.regex"),
            shared_info("vendor_a_clone", true),
        );
        reg.active.modules.insert(
            module_id_from_identity("vendor_b.text.regex"),
            shared_info("vendor_b_clone", false),
        );

        let a = reg
            .resolve_handle_method_sig("vendor_a.text.regex.Pattern", "clone_for_test")
            .expect("exact vendor_a receiver should resolve");
        assert_eq!(a.0, "vendor_a_clone");
        assert_eq!(a.3, "vendor_a.text.regex");
        assert_eq!(
            a.1[0],
            crate::ty::Ty::option(crate::ty::Ty::named("vendor_a.text.regex.Pattern", vec![])),
            "nested signature positions must retain the selected source owner"
        );
        assert_eq!(
            a.1[1],
            crate::ty::Ty::named("regex.Foreign", vec![]),
            "an undeclared foreign regex.X type must not inherit the owner"
        );
        assert_eq!(
            a.2,
            crate::ty::Ty::option(crate::ty::Ty::named("vendor_a.text.regex.Pattern", vec![]))
        );

        let b = reg
            .resolve_handle_method_sig("vendor_b.text.regex.Pattern", "clone_for_test")
            .expect("exact vendor_b receiver should resolve");
        assert_eq!(b.0, "vendor_b_clone");
        assert_eq!(b.3, "vendor_b.text.regex");
        assert!(reg.handle_method_dispatches_through_impl(
            "vendor_a.text.regex.Pattern",
            "clone_for_test"
        ));
        assert!(!reg.handle_method_dispatches_through_impl(
            "vendor_b.text.regex.Pattern",
            "clone_for_test"
        ));

        assert_eq!(
            reg.resolve_handle_method_sig("regex.Pattern", "clone_for_test"),
            None,
            "an ambiguous legacy receiver must fail closed"
        );
        assert_eq!(
            reg.canonical_owned_type_identity("regex.Pattern"),
            None,
            "ambiguous ownership metadata must fail closed too"
        );
        assert!(!reg.handle_method_dispatches_through_impl("regex.Pattern", "clone_for_test"));
    }

    #[test]
    fn fielded_process_child_does_not_publish_a_short_handle_alias() {
        let mut reg = registry();
        reg.load("std.process").unwrap();

        // The loader retains qualified imported-signature metadata for normal
        // named-type/trait method resolution.
        let sig = reg
            .resolve_handle_method_sig("process.Child", "wait")
            .expect("qualified process.Child.wait imported signature should resolve");
        assert_eq!(sig.0, "hew_process_wait");
        assert_eq!(sig.1, Vec::<crate::ty::Ty>::new());
        assert_eq!(sig.2, crate::ty::Ty::I64);

        assert_eq!(
            reg.resolve_handle_method_sig("Child", "kill"),
            None,
            "fielded process.Child is a named resource record, not an opaque \
             handle that publishes an unqualified handle-registry alias"
        );
    }

    #[test]
    fn resolve_handle_method_sig_returns_listener_and_request_close() {
        let mut reg = registry();
        reg.load("std.net").unwrap();
        reg.load("std.net.http").unwrap();

        let listener_close = reg
            .resolve_handle_method_sig("net.Listener", "close")
            .expect("net.Listener.close should resolve");
        assert_eq!(listener_close.0, "hew_tcp_listener_close");
        assert_eq!(listener_close.1, Vec::<crate::ty::Ty>::new());
        assert_eq!(listener_close.2, crate::ty::Ty::Unit);

        let request_close = reg
            .resolve_handle_method_sig("http.Request", "close")
            .expect("http.Request.close should be extracted");
        assert_eq!(request_close.0, "hew_http_request_free");
        assert_eq!(request_close.1, Vec::<crate::ty::Ty>::new());
        assert_eq!(request_close.2, crate::ty::Ty::Unit);

        assert_eq!(
            reg.canonical_registry_signature_type_identity("net.NetError", "std.net.http"),
            Some("std.net.NetError".to_string()),
            "a source-qualified imported type must use the import's exact declaration owner"
        );
    }

    #[test]
    fn qualify_handle_type_requires_canonical_name() {
        let mut reg = registry();
        reg.load("std.encoding.json").unwrap();
        assert_eq!(
            reg.qualify_handle_type("Value"),
            None,
            "a bare leaf must not select a loaded handle declaration"
        );
        assert_eq!(
            reg.qualify_handle_type("json.Value"),
            Some("std.encoding.json.Value".to_string()),
            "a unique legacy registry spelling projects to its source identity"
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
        reg.load("std.encoding.json").unwrap();
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

    #[test]
    fn missing_compiler_stdlib_root_confers_no_authority() {
        let project = TestHewTree::new("missing-compiler-authority");
        let mut registry = ModuleRegistry::new(vec![project.root().clone()]);
        registry.compiler_stdlib_root = None;
        assert!(
            !registry.source_has_stdlib_authority(
                &project.root().join("std/builtins.hew"),
                "std.builtins"
            ),
            "project search paths cannot replace unavailable compiler authority"
        );
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
