use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt;
use std::ops::Range;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use hew_parser::ast::{ImportDecl, Item, Program, Spanned};
use hew_parser::module::ModulePath;
use serde::{de::DeserializeOwned, Deserialize};

/// Build identity shared by the compiler driver and its frontend consumers.
pub const VERSION: &str = env!("HEW_VERSION");

/// Stack budget for native threads running the recursive compiler pipeline.
pub const COMPILER_STACK_SIZE: usize = 64 * 1024 * 1024;

mod compile_time;
mod coverage;
mod host_admission;
pub use coverage::{SourceCoverageCounter, SourceCoverageMap};
mod host;
pub mod live;
mod source_paths;
pub use source_paths::{
    display_source_path, register_source_path, source_path_policy, SourcePathPolicy,
    SourcePathScope,
};

/// Source entries checked against their host-operation admission profile.
/// Each deterministic test is checked separately from its real-time siblings.
#[derive(Debug, Clone, Default)]
pub enum HostAdmission {
    #[default]
    Off,
    ProcessEntry,
    Tests(Vec<hew_types::DeclarationOccurrence>),
    /// Build-time generation, starting at `main`, with restricted host access.
    Generate,
}

fn require_host_typecheck(options: &FrontendOptions) -> Result<(), FrontendFailure> {
    if options.no_typecheck && !matches!(options.host_admission, HostAdmission::Off) {
        if matches!(options.host_admission, HostAdmission::Generate) {
            return Err(FrontendFailure::coded_message(
                "E_GENERATE_TYPECHECK_REQUIRED",
                "generator host-operation admission requires type checking",
            ));
        }
        return Err(FrontendFailure::coded_message(
            "E_DETERMINISTIC_TYPECHECK_REQUIRED",
            "deterministic host-operation admission requires type checking",
        ));
    }
    Ok(())
}

#[derive(Debug, Clone, Default)]
#[allow(
    clippy::struct_excessive_bools,
    reason = "each flag is an independent, orthogonal frontend toggle \
              (no_typecheck/warnings_as_errors/enable_wasm_target/repl_fragment) \
              queried separately at distinct pipeline stages — collapsing into a \
              state enum would force unrelated flags to share variants and add \
              per-flag matches at every read site"
)]
pub struct FrontendOptions {
    /// Compiler-owned retained types for an interactive submission.
    pub session_type_seed: Option<Arc<hew_types::session::SessionTypeSeed>>,
    pub no_typecheck: bool,
    pub enable_wasm_target: bool,
    pub pkg_path: Option<PathBuf>,
    /// Select a project directory for manifest and lockfile resolution.
    /// When unset, use the nearest manifest above the input file, or the
    /// input's directory when it belongs to no package.
    pub project_dir: Option<PathBuf>,
    /// Exact roots used to resolve `std.*` modules. When unset, use `HEW_STD`
    /// or the standard library shipped with the compiler. In-memory hosts can
    /// set these roots to match their retained source paths.
    pub module_search_paths: Option<Vec<PathBuf>>,
    /// Treat warning-severity diagnostics as hard errors.
    ///
    /// When `true`, [`check_file`], [`check_program`], [`compile_file`], and
    /// [`compile_program`] all fail with [`FrontendFailure`] when the pipeline
    /// produces any warning-severity diagnostic.  Mirrors `--deny warnings`
    /// semantics and is checked uniformly at the end of each pipeline's
    /// success arm so no path silently swallows warnings.
    pub warnings_as_errors: bool,
    /// Suppress the completeness lints that assume a whole, finished program.
    ///
    /// The `hew eval` REPL compiles a synthetic fragment — accumulated session
    /// statements wrapped in a generated `main` — where a binding used only on
    /// a later line, a helper called only later, or an import staged for a
    /// future input all look "unused" or "dead" to a whole-program checker but
    /// are not. When `true`, the `DeadCode`, `UnusedImport`, `UnusedVariable`,
    /// and `UnusedMut` lints are skipped. Eval-only: `hew check`/`hew build`
    /// leave it `false` and keep emitting them.
    pub repl_fragment: bool,
    /// Per-lint reporting levels for the semantic lint sweep, built from the
    /// CLI `--allow` / `--warn` / `--deny` flags. Installed on the checker via
    /// [`hew_types::Checker::set_lint_levels`] before `check_program`. Defaults
    /// to every lint's built-in level ([`hew_types::LintLevels::from_defaults`]).
    pub lint_levels: hew_types::LintLevels,
    /// Exact root declaration selected as the process entry. File test
    /// discovery records this occurrence before checker identities exist.
    pub entry_selection: Option<hew_types::DeclarationOccurrence>,
    /// Exact test roots for one compiled dispatcher, in selection order.
    pub test_entry_selections: Vec<hew_types::DeclarationOccurrence>,
    /// Compile-time host-operation admission for the selected execution profile.
    pub host_admission: HostAdmission,
    /// The sole deterministic production peer for a selected `_test.hew`
    /// root. Arbitrary sibling discovery is intentionally not supported.
    pub companion: Option<PathBuf>,
    /// Resolve imports without the project's `hew.lock`. `hew publish`
    /// checks a package against the dependencies it has just installed,
    /// which the package's own lock does not govern and which consumers never
    /// see.
    pub ignore_lockfile: bool,
    /// Open editor buffers that override on-disk content for this run.
    ///
    /// Every source read the frontend performs consults this set first, so an
    /// unsaved buffer checks against its saved siblings. Empty for the CLI.
    pub documents: SourceSet,
}

/// Source text that overrides the filesystem for one frontend run.
///
/// The LSP, the browser analysis surface and the REPL all check buffers that
/// either have no file behind them or differ from the file on disk. They hand
/// the driver this set instead of running a frontend of their own.
#[derive(Debug, Clone)]
pub struct SourceSet {
    sources: BTreeMap<PathBuf, SourceContent>,
    /// One context world for compiler-prepared syntax retained across live inputs.
    prepared_expansions: Arc<hew_parser::macro_syntax::ExpansionMap>,
    filesystem: bool,
    reads: Arc<std::sync::Mutex<BTreeMap<PathBuf, Option<String>>>>,
    dependency_roots: Arc<std::sync::Mutex<HashSet<PathBuf>>>,
    /// Successful reads retained only within one explicit frontend admission.
    admitted: Option<Arc<std::sync::Mutex<BTreeMap<PathBuf, SourceContent>>>>,
}

impl Default for SourceSet {
    fn default() -> Self {
        Self {
            sources: BTreeMap::new(),
            prepared_expansions: Arc::default(),
            filesystem: true,
            reads: Arc::default(),
            dependency_roots: Arc::default(),
            admitted: None,
        }
    }
}

#[derive(Debug, Clone)]
enum SourceContent {
    Text(Arc<str>),
    Bytes(Arc<[u8]>),
    Prepared {
        source: Arc<str>,
        program: Arc<Program>,
    },
}

impl SourceContent {
    fn bytes(&self) -> &[u8] {
        match self {
            Self::Text(source) | Self::Prepared { source, .. } => source.as_bytes(),
            Self::Bytes(bytes) => bytes,
        }
    }
}

impl SourceSet {
    /// Whether missing documents are read from the host filesystem.
    #[must_use]
    pub fn uses_filesystem(&self) -> bool {
        self.filesystem
    }

    #[must_use]
    pub fn is_file(&self, path: &Path) -> bool {
        self.contains(path) || (self.filesystem && path.is_file())
    }
    #[must_use]
    pub fn in_memory() -> Self {
        Self {
            filesystem: false,
            ..Self::default()
        }
    }

    /// Start a fresh read epoch while preserving the caller's supplied overlays.
    /// Clones inside the run share admitted bytes; later runs never reuse them.
    fn for_frontend(&self) -> Self {
        Self {
            sources: self.sources.clone(),
            prepared_expansions: Arc::clone(&self.prepared_expansions),
            filesystem: self.filesystem,
            reads: Arc::default(),
            dependency_roots: Arc::default(),
            admitted: Some(Arc::default()),
        }
    }

    fn input_identity(&self, path: &Path) -> PathBuf {
        if self.filesystem {
            buffer_identity(path).unwrap_or_else(|| path.to_path_buf())
        } else {
            compile_time::normalize(path)
        }
    }

    fn read_content(&self, path: &Path, text: bool) -> std::io::Result<SourceContent> {
        use sha2::{Digest, Sha256};
        use std::fmt::Write as _;
        let identity = self.input_identity(path);
        let mut admitted = self
            .admitted
            .as_ref()
            .map(|table| table.lock().expect("source admission table poisoned"));
        if let Some(content) = admitted
            .as_ref()
            .and_then(|table| table.get(path).or_else(|| table.get(&identity)))
        {
            return Ok(content.clone());
        }
        self.reads
            .lock()
            .expect("source input table poisoned")
            .insert(identity.clone(), None);
        let content = if let Some(content) = self.content(path) {
            content.clone()
        } else if self.filesystem {
            let bytes = std::fs::read(path)?;
            if text {
                match String::from_utf8(bytes) {
                    Ok(source) => SourceContent::Text(source.into()),
                    Err(error) => SourceContent::Bytes(error.into_bytes().into()),
                }
            } else {
                SourceContent::Bytes(bytes.into())
            }
        } else {
            return Err(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                "compilation input was not supplied",
            ));
        };
        let fingerprint = Sha256::digest(content.bytes()).iter().fold(
            String::with_capacity(64),
            |mut fingerprint, byte| {
                write!(fingerprint, "{byte:02x}").expect("writing to a String cannot fail");
                fingerprint
            },
        );
        self.reads
            .lock()
            .expect("source input table poisoned")
            .insert(identity.clone(), Some(fingerprint));
        if let Some(admitted) = admitted.as_mut() {
            admitted.insert(identity, content.clone());
            admitted.insert(path.to_path_buf(), content.clone());
        }
        Ok(content)
    }

    /// Read an input and record its content fingerprint.
    ///
    /// # Errors
    /// Returns an I/O error if the input cannot be read or was not supplied.
    ///
    /// # Panics
    /// Panics if a shared input table's mutex is poisoned.
    pub fn read_bytes(&self, path: &Path) -> std::io::Result<Vec<u8>> {
        self.read_content(path, false)
            .map(|content| content.bytes().to_vec())
    }

    /// Read shared UTF-8 text, retaining the exact admitted bytes for this run.
    ///
    /// # Errors
    /// Returns an I/O error for missing inputs or invalid UTF-8.
    ///
    /// # Panics
    /// Panics if a shared input table's mutex is poisoned.
    pub fn read_text(&self, path: &Path) -> std::io::Result<Arc<str>> {
        match self.read_content(path, true)? {
            SourceContent::Text(source) | SourceContent::Prepared { source, .. } => Ok(source),
            SourceContent::Bytes(bytes) => {
                let source: Arc<str> = std::str::from_utf8(&bytes)
                    .map_err(|error| std::io::Error::new(std::io::ErrorKind::InvalidData, error))?
                    .into();
                if let Some(admitted) = &self.admitted {
                    let mut admitted = admitted.lock().expect("source admission table poisoned");
                    admitted.insert(
                        self.input_identity(path),
                        SourceContent::Text(Arc::clone(&source)),
                    );
                    admitted.insert(path.to_path_buf(), SourceContent::Text(Arc::clone(&source)));
                }
                Ok(source)
            }
        }
    }

    fn admitted_content(&self, path: &Path) -> Option<SourceContent> {
        let admitted = self
            .admitted
            .as_ref()?
            .lock()
            .expect("source admission table poisoned");
        admitted
            .get(path)
            .or_else(|| admitted.get(&self.input_identity(path)))
            .cloned()
    }

    /// UTF-8 text from an already observed input; never reads new content.
    fn admitted_text(&self, path: &Path) -> Option<Arc<str>> {
        match self.admitted_content(path)? {
            SourceContent::Text(source) | SourceContent::Prepared { source, .. } => Some(source),
            SourceContent::Bytes(bytes) => std::str::from_utf8(&bytes).ok().map(Arc::from),
        }
    }

    /// Display an observed asset even when its bytes are not valid source text.
    fn admitted_diagnostic_text(&self, path: &Path) -> Option<Arc<str>> {
        match self.admitted_content(path)? {
            SourceContent::Text(source) | SourceContent::Prepared { source, .. } => Some(source),
            SourceContent::Bytes(bytes) => {
                Some(Arc::from(String::from_utf8_lossy(&bytes).as_ref()))
            }
        }
    }

    /// Mutating an admitted clone begins a new provider, preserving the old run.
    fn leave_admission(&mut self) {
        if self.admitted.take().is_some() {
            self.reads = Arc::default();
            self.dependency_roots = Arc::default();
        }
    }

    /// Return the inputs observed by this frontend run.
    ///
    /// # Panics
    ///
    /// Panics if the shared input table's mutex is poisoned.
    #[must_use]
    pub fn inputs(&self) -> Vec<hew_types::compile_time::CompilationInput> {
        let dependency_roots = self
            .dependency_roots
            .lock()
            .expect("source dependency table poisoned");
        self.reads
            .lock()
            .expect("source input table poisoned")
            .iter()
            .map(|(path, sha256)| hew_types::compile_time::CompilationInput {
                path: path.clone(),
                display_path: path.to_string_lossy().replace('\\', "/"),
                sha256: sha256.clone(),
                dependency_root: dependency_roots
                    .iter()
                    .filter(|root| path.starts_with(root))
                    .max_by_key(|root| root.components().count())
                    .cloned(),
            })
            .collect()
    }
    pub fn insert_bytes(&mut self, path: impl Into<PathBuf>, bytes: impl Into<Vec<u8>>) {
        self.leave_admission();
        let mut path = path.into();
        if !self.filesystem {
            path = compile_time::normalize(&path);
        }
        let bytes = SourceContent::Bytes(bytes.into().into());
        if let Some(canonical) = self.filesystem.then(|| buffer_identity(&path)).flatten() {
            self.sources.insert(canonical, bytes.clone());
        }
        self.sources.insert(path, bytes);
    }

    #[must_use]
    pub fn get_bytes(&self, path: &Path) -> Option<&[u8]> {
        match self.content(path)? {
            SourceContent::Text(text) => Some(text.as_bytes()),
            SourceContent::Bytes(bytes) => Some(bytes),
            SourceContent::Prepared { source, .. } => Some(source.as_bytes()),
        }
    }
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Record `source` as the current content of `path`.
    ///
    /// The canonical spelling is recorded alongside the given one because the
    /// import resolver canonicalizes every candidate before loading it.
    pub fn insert(&mut self, path: impl Into<PathBuf>, source: impl Into<String>) {
        self.leave_admission();
        let mut path = path.into();
        if !self.filesystem {
            path = compile_time::normalize(&path);
        }
        let source = SourceContent::Text(source.into().into());
        if let Some(canonical) = self.filesystem.then(|| buffer_identity(&path)).flatten() {
            if canonical != path {
                self.sources.insert(canonical, source.clone());
            }
        }
        self.sources.insert(path, source);
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.sources.is_empty()
    }

    fn contains(&self, path: &Path) -> bool {
        self.content(path).is_some()
            || self.admitted.as_ref().is_some_and(|table| {
                let admitted = table.lock().expect("source admission table poisoned");
                admitted.contains_key(path) || admitted.contains_key(&self.input_identity(path))
            })
    }

    /// The documents directly inside `dir`, so a host without a filesystem
    /// lists a directory module's peers from the sources it was given.
    fn files_in(&self, dir: &Path) -> Vec<PathBuf> {
        let cwd = std::env::current_dir().unwrap_or_default();
        let relative_dir = compile_time::normalize(dir.strip_prefix(&cwd).unwrap_or(dir));
        self.sources
            .keys()
            .filter(|path| {
                path.parent() == Some(dir) || path.parent() == Some(relative_dir.as_path())
            })
            .map(|path| dir.join(path.file_name().expect("a source file has a name")))
            .collect()
    }

    /// The recorded content of `path`, under its given or canonical spelling.
    #[must_use]
    pub fn get(&self, path: &Path) -> Option<&str> {
        match self.content(path)? {
            SourceContent::Text(text) => Some(text),
            SourceContent::Bytes(bytes) => std::str::from_utf8(bytes).ok(),
            SourceContent::Prepared { source, .. } => Some(source),
        }
    }

    /// Retain compiler-prepared syntax with its original diagnostic source.
    /// Session declaration snapshots keep expansion hygiene and source spans;
    /// rendering and reparsing their text would discard those identities.
    pub(crate) fn insert_prepared(&mut self, path: PathBuf, source: String, mut program: Program) {
        if !program.expansions.templates.is_empty() || !program.expansions.expansions.is_empty() {
            self.prepared_expansions = Arc::new(std::mem::take(&mut program.expansions));
        }
        self.insert(path.clone(), source.clone());
        let content = SourceContent::Prepared {
            source: source.into(),
            program: program.into(),
        };
        if let Some(canonical) = self.filesystem.then(|| buffer_identity(&path)).flatten() {
            self.sources.insert(canonical, content.clone());
        }
        self.sources.insert(
            if self.filesystem {
                path
            } else {
                compile_time::normalize(&path)
            },
            content,
        );
    }

    fn retain_prepared_expansions(&mut self, expansions: hew_parser::macro_syntax::ExpansionMap) {
        self.prepared_expansions = Arc::new(expansions);
    }

    fn parse_document(&self, path: &Path, source: &str) -> hew_parser::ParseResult {
        if let Some(SourceContent::Prepared {
            source: retained,
            program,
        }) = self.content(path)
        {
            if retained.as_ref() == source {
                return hew_parser::ParseResult {
                    program: (**program).clone(),
                    errors: Vec::new(),
                };
            }
        }
        hew_parser::parse(source)
    }

    fn content(&self, path: &Path) -> Option<&SourceContent> {
        if self.sources.is_empty() {
            return None;
        }
        if let Some(source) = self.sources.get(path) {
            return Some(source);
        }
        if !self.filesystem {
            if let Some(source) = self.sources.get(&compile_time::normalize(path)) {
                return Some(source);
            }
        }
        if self.filesystem {
            if let Some(source) = buffer_identity(path).and_then(|path| self.sources.get(&path)) {
                return Some(source);
            }
        }
        std::env::current_dir()
            .ok()
            .and_then(|cwd| {
                path.strip_prefix(cwd)
                    .ok()
                    .and_then(|path| self.sources.get(path))
            })
            .or_else(|| {
                path.strip_prefix("/")
                    .ok()
                    .and_then(|path| self.sources.get(path))
            })
    }
}

/// Resolve the existing prefix of an input, including missing directories.
fn buffer_identity(path: &Path) -> Option<PathBuf> {
    for ancestor in path.ancestors() {
        if let Ok(canonical) = ancestor.canonicalize() {
            return Some(compile_time::normalize(
                &canonical.join(path.strip_prefix(ancestor).ok()?),
            ));
        }
    }
    None
}

/// The overlay used when a caller supplies no [`FrontendOptions`].
static EMPTY_DOCUMENTS: std::sync::LazyLock<SourceSet> =
    std::sync::LazyLock::new(SourceSet::default);

/// The canonical path of an import candidate, or `None` when nothing supplies
/// it. An open buffer stands in for a file the filesystem does not have, so an
/// editor can check a document set that is not on disk.
fn resolve_candidate(documents: &SourceSet, candidate: &Path) -> Option<PathBuf> {
    if !documents.filesystem {
        return documents
            .contains(candidate)
            .then(|| compile_time::normalize(candidate));
    }
    match candidate.canonicalize() {
        Ok(canonical) => Some(canonical),
        Err(_) => documents
            .contains(candidate)
            .then(|| candidate.to_path_buf()),
    }
}

/// Read a source file, preferring an open buffer over the file on disk.
fn read_source(documents: &SourceSet, path: &Path) -> std::io::Result<Arc<str>> {
    documents.read_text(path)
}

/// Target facts that must stay coupled while a source is lowered.
///
/// This deliberately contains only facts passed by existing hosts: the HIR
/// architecture, MIR pointer width, and optional backend target triple.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SessionTarget {
    pub hir_arch: hew_hir::TargetArch,
    pub pointer_width: hew_mir::PointerWidth,
    pub codegen_triple: Option<String>,
}

impl SessionTarget {
    #[must_use]
    pub fn native() -> Self {
        Self {
            hir_arch: hew_hir::TargetArch::host(),
            pointer_width: hew_mir::PointerWidth::Bits64,
            codegen_triple: None,
        }
    }

    /// The browser sandbox VM.
    ///
    /// The VM interprets verified semantics with its own deterministic
    /// scheduler and has no machine layout at all, so this is not a codegen
    /// target and carries no triple. It is pinned to a 64-bit architecture
    /// because `isize` and `usize` are 64-bit in the VM, matching the native
    /// execution the parity oracle compares against, and pinned to one exact
    /// architecture so a browser result does not vary with the host that built
    /// the wasm package.
    #[must_use]
    pub fn browser() -> Self {
        Self {
            hir_arch: hew_hir::TargetArch::X86_64,
            pointer_width: hew_mir::PointerWidth::Bits64,
            codegen_triple: None,
        }
    }

    /// The `wasm32-wasi` codegen target.
    #[must_use]
    pub fn wasi() -> Self {
        Self {
            hir_arch: hew_hir::TargetArch::Wasm32,
            pointer_width: hew_mir::PointerWidth::Bits32,
            codegen_triple: None,
        }
    }
}

/// What a session runs after the SIR is lowered and verified.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum CheckSet {
    /// Verify, then run the SIR optimization passes and verify their result:
    /// the SIR a build, run or `--explain-cow` report consumes.
    #[default]
    Build,
    /// Verify the lowered SIR only. Nothing in a diagnostics-only check reads
    /// the optimized form.
    Check,
}

/// Policy used when exposing diagnostics produced by a compilation session.
#[derive(Debug, Clone, Default)]
pub struct DiagnosticPolicy {
    pub warnings_as_errors: bool,
    pub lint_levels: hew_types::LintLevels,
}

/// The shared semantic compilation boundary. Every host enters through this
/// type with its target facts and resolved compilation roots explicit.
#[derive(Debug, Clone)]
pub struct Session {
    pub target: SessionTarget,
    pub checks: CheckSet,
    pub diagnostic_policy: DiagnosticPolicy,
}

impl Session {
    #[must_use]
    pub fn new(target: SessionTarget, diagnostic_policy: DiagnosticPolicy) -> Self {
        Self {
            target,
            checks: CheckSet::Build,
            diagnostic_policy,
        }
    }

    #[must_use]
    pub fn from_frontend_options(target: SessionTarget, options: &FrontendOptions) -> Self {
        Self::new(
            target,
            DiagnosticPolicy {
                warnings_as_errors: options.warnings_as_errors,
                lint_levels: options.lint_levels.clone(),
            },
        )
    }

    /// Lower a checked source program through the shared semantic boundary.
    ///
    /// # Errors
    ///
    /// Returns HIR lowering or semantic verification diagnostics.
    pub fn lower_program(
        &self,
        program: &hew_parser::ast::Program,
        tco: &hew_types::TypeCheckOutput,
    ) -> Result<SessionOutput, SessionError> {
        let module = self.lower_hir_program(program, tco)?;
        let mut roots = Self::source_roots(program, tco)?;
        roots.extend(tco.test_entry_plans.iter().map(|plan| plan.entry));
        self.lower_hir_module(&module, tco, &roots)
    }

    /// Lower checked declarations and generated test entries without entering SIR.
    ///
    /// # Errors
    /// Returns HIR lowering or test-entry installation diagnostics.
    pub fn lower_hir_program(
        &self,
        program: &hew_parser::ast::Program,
        tco: &hew_types::TypeCheckOutput,
    ) -> Result<hew_hir::HirModule, SessionError> {
        let mut lowered =
            hew_hir::lower_program(program, tco, &hew_hir::ResolutionCtx, self.target.hir_arch);
        if !lowered.diagnostics.is_empty() {
            return Err(SessionError::Hir(lowered.diagnostics));
        }
        if !tco.test_entry_plans.is_empty() {
            hew_hir::test_entry::install_test_entry_plans(
                &mut lowered.module,
                &tco.test_entry_plans,
            )
            .map_err(|message| SessionError::Lowering {
                callable: None,
                error: hew_sir::SirLoweringError::Internal(message),
                span: None,
            })?;
        }
        Ok(lowered.module)
    }

    /// Select concrete declarations exported by this checked source module.
    ///
    /// Root-source `pub` and `package` monomorphic functions require bodies.
    /// Imported declarations are not roots. Generic exports are templates:
    /// concrete caller demand supplies their specializations. These roots do
    /// not establish a public C calling convention or a stable embedding ABI.
    /// The checker-selected process entry is added by semantic lowering.
    ///
    /// # Errors
    ///
    /// Returns an error if a selected declaration lacks checker identity.
    pub fn source_roots(
        program: &hew_parser::ast::Program,
        tco: &hew_types::TypeCheckOutput,
    ) -> Result<Vec<hew_types::DefId>, SessionError> {
        let program = tco
            .normalized_program
            .as_ref()
            .map_or(program, |normalized| &normalized.program);
        // A file import's public declarations remain imports, with their own
        // source identities, rather than authored root exports.
        let root_items = program
            .module_graph
            .as_ref()
            .and_then(|graph| graph.modules.get(&graph.root))
            .map_or(program.items.as_slice(), |root| root.items.as_slice());
        root_items
            .iter()
            .enumerate()
            .filter_map(|(ordinal, (item, span))| {
                let hew_parser::ast::Item::Function(function) = item else {
                    return None;
                };
                if !function.visibility.is_pub()
                    || function
                        .type_params
                        .as_ref()
                        .is_some_and(|params| !params.is_empty())
                {
                    return None;
                }
                let occurrence = hew_types::DeclarationOccurrence::new_with_synthetic_ordinal(
                    tco.defs.root_module(),
                    span,
                    ordinal,
                    hew_types::DeclarationKind::Function,
                    0,
                );
                Some(
                    tco.defs
                        .declaration(occurrence)
                        .ok_or_else(|| SessionError::Lowering {
                            callable: None,
                            error: hew_sir::SirLoweringError::Internal(format!(
                                "exported source declaration at {span:?} has no checked identity"
                            )),
                            span: Some(span.clone()),
                        }),
                )
            })
            .collect()
    }

    /// Verify and canonicalize ownership semantics shared by every host.
    /// `roots` are resolved concrete declarations, in addition to the selected
    /// process entry. Source consumers use [`Self::source_roots`] to preserve
    /// the same export policy as [`Self::lower_program`].
    ///
    /// # Errors
    ///
    /// Returns the first failing semantic boundary with its diagnostics.
    pub fn lower_hir_module(
        &self,
        module: &hew_hir::HirModule,
        tco: &hew_types::TypeCheckOutput,
        roots: &[hew_types::DefId],
    ) -> Result<SessionOutput, SessionError> {
        self.lower_analyzed_hir(hew_hir::analyze_hir(module), tco, roots)
    }

    /// Reuse structural and source facts for an unchanged HIR module.
    ///
    /// # Errors
    /// Returns HIR diagnostics or a failure of semantic lowering or verification.
    pub fn lower_analyzed_hir(
        &self,
        analysis: hew_hir::HirAnalysis<'_>,
        tco: &hew_types::TypeCheckOutput,
        roots: &[hew_types::DefId],
    ) -> Result<SessionOutput, SessionError> {
        let module = analysis.module();
        if !analysis.diagnostics().is_empty() {
            return Err(SessionError::Hir(analysis.diagnostics().to_vec()));
        }
        let sir =
            hew_sir::lower_analyzed_module_with_roots(analysis, tco, roots).map_err(|errors| {
                let Some(error) = errors.into_iter().next() else {
                    return SessionError::Lowering {
                        callable: None,
                        error: hew_sir::SirLoweringError::Internal(
                            "root selection failed without reporting a cause".into(),
                        ),
                        span: None,
                    };
                };
                SessionError::Lowering {
                    callable: None,
                    error: error.reason.with_context(&format!(
                        "SIR root `{}`",
                        module.defs.path(error.declaration)
                    )),
                    span: None,
                }
            })?;
        let mut sir =
            hew_sir::VerifiedModule::new(sir).map_err(SessionError::from_semantic_diagnostics)?;
        let semantic = &sir.lowered().module;
        require_complete_semantics(sir.lowered(), module.entry_exit_plan.is_some())?;
        if semantic.test_entries.len() != module.test_entry_plans.len()
            || semantic
                .test_entries
                .iter()
                .zip(&module.test_entry_plans)
                .any(|(entry, plan)| entry.declaration != plan.entry)
        {
            return Err(SessionError::Lowering {
                callable: None,
                error: hew_sir::SirLoweringError::Internal(
                    "semantic test dispatcher does not match checked entry plans".into(),
                ),
                span: None,
            });
        }
        let body_index = semantic.function_index();
        let mut compiled_roots = roots
            .iter()
            .map(|declaration| {
                let callable = semantic
                    .callable_for_declaration(declaration)
                    .ok_or_else(|| SessionError::Lowering {
                        callable: None,
                        error: hew_sir::SirLoweringError::Internal(format!(
                            "selected declaration {declaration:?} has no semantic callable"
                        )),
                        span: None,
                    })?
                    .id;
                require_semantic_body(semantic, &body_index, callable)?;
                Ok(callable)
            })
            .collect::<Result<Vec<_>, SessionError>>()?;
        if let Some(entry) = semantic.entry_callable {
            compiled_roots.push(entry);
        }
        for entry in &semantic.test_entries {
            require_semantic_body(semantic, &body_index, entry.callable)?;
            compiled_roots.push(entry.callable);
        }
        compiled_roots.sort_unstable();
        compiled_roots.dedup();
        if self.checks == CheckSet::Build {
            let mut lowered = sir.into_lowered();
            hew_sir::canonicalize_module_constant_cfg(&mut lowered.module)
                .map_err(SessionError::Semantic)?;
            hew_sir::transfer_module_dead_local_reads(&mut lowered.module);
            sir = hew_sir::VerifiedModule::new(lowered)
                .map_err(SessionError::from_semantic_diagnostics)?;
        }
        Ok(SessionOutput {
            sir,
            compiled_roots,
        })
    }
}

/// Verified semantic compilation result, independent of the execution host.
#[derive(Debug)]
pub struct SessionOutput {
    sir: hew_sir::VerifiedModule,
    compiled_roots: Vec<hew_sir::CallableId>,
}

impl SessionOutput {
    /// Resolved entry and export roots whose complete call closures were lowered.
    #[must_use]
    pub fn compiled_roots(&self) -> &[hew_sir::CallableId] {
        &self.compiled_roots
    }

    /// Inspect the verified semantic result without invalidating it.
    #[must_use]
    pub fn semantics(&self) -> &hew_sir::LoweredModule {
        self.sir.lowered()
    }

    /// Consume the session result and relinquish its verification guarantee.
    #[must_use]
    pub fn into_semantics(self) -> hew_sir::LoweredModule {
        self.sir.into_lowered()
    }

    /// Prepare one physical type inventory tied to this checked semantic input.
    #[must_use]
    pub fn physical_plan(&self) -> hew_mir::physical::PhysicalPlan<'_> {
        hew_mir::physical::PhysicalPlan::new(self.sir.checked())
    }

    /// Realize verified semantics using the backend's measured target layouts.
    ///
    /// # Errors
    ///
    /// Returns a physical lowering or verification diagnostic.
    pub fn lower_physical(
        &self,
        target: hew_mir::PhysicalTarget,
    ) -> Result<hew_mir::VerifiedPhysicalModule, hew_mir::PhysicalError> {
        self.physical_plan().lower(target)
    }
}

/// Diagnostics from the shared semantic compilation boundary.
#[derive(Debug)]
pub enum SessionError {
    Hir(Vec<hew_hir::HirDiagnostic>),
    Semantic(Vec<hew_sir::SirDiagnostic>),
    /// Source ownership obligations diagnosed by the semantic lifetime flow.
    Ownership(Vec<hew_sir::SirDiagnostic>),
    Selection(String),
    Lowering {
        callable: Option<hew_sir::CallableId>,
        error: hew_sir::SirLoweringError,
        /// The provoking construct's span, when SIR could attribute one
        /// (#3384). `None` for a session-level refusal with no declaration
        /// of its own (a missing entry, an unresolved root).
        span: Option<Range<usize>>,
    },
}

impl SessionError {
    fn from_semantic_diagnostics(diagnostics: Vec<hew_sir::SirDiagnostic>) -> Self {
        if diagnostics.iter().all(|diagnostic| {
            matches!(
                diagnostic.kind,
                hew_sir::SirDiagnosticKind::UnconsumedLinear { .. }
                    | hew_sir::SirDiagnosticKind::InvalidGeneratorSend { .. }
            )
        }) {
            Self::Ownership(diagnostics)
        } else {
            Self::Semantic(diagnostics)
        }
    }
}

impl std::fmt::Display for SessionError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Hir(diagnostics) => write!(formatter, "HIR verification failed: {diagnostics:?}"),
            Self::Ownership(diagnostics) => {
                write!(formatter, "unconsumed linear values: {diagnostics:?}")
            }
            Self::Semantic(diagnostics) => {
                write!(formatter, "SIR verification failed: {diagnostics:?}")
            }
            Self::Lowering { error, .. } => error.fmt(formatter),
            Self::Selection(message) => formatter.write_str(message),
        }
    }
}

impl std::error::Error for SessionError {}

/// Headers without bodies are valid SIR, but a selected executable must close
/// every demanded body. Join by resolved identity, never by emitted names.
fn require_complete_semantics(
    sir: &hew_sir::LoweredModule,
    entry_required: bool,
) -> Result<(), SessionError> {
    if entry_required && sir.module.entry_callable.is_none() {
        return Err(SessionError::Lowering {
            callable: None,
            error: hew_sir::SirLoweringError::Internal(
                "the selected process entry has no semantic callable".into(),
            ),
            span: None,
        });
    }
    for (callable, status) in &sir.callable_statuses {
        if let hew_sir::SirLoweringStatus::Failed { reason, span } = status {
            let name = sir
                .module
                .callable(*callable)
                .map_or("<unknown>", |item| item.symbol.as_str());
            return Err(SessionError::Lowering {
                callable: Some(*callable),
                error: reason
                    .clone()
                    .with_context(&format!("semantic lowering of `{name}`")),
                span: span.clone(),
            });
        }
    }
    let body_index = sir.module.function_index();
    for callable in sir
        .module
        .structural_display
        .values()
        .filter_map(|render| render.display)
    {
        require_semantic_body(&sir.module, &body_index, callable)?;
    }
    for plan in sir.module.value_capabilities.values() {
        if let Some(callable) = plan.callable {
            require_semantic_body(&sir.module, &body_index, callable)?;
        }
    }
    if let Some(entry) = sir.module.entry_callable {
        require_semantic_body(&sir.module, &body_index, entry)?;
    }
    for function in &sir.module.functions {
        for block in &function.blocks {
            if let hew_sir::SemTerminator::Call { callee, .. } = &block.terminator {
                require_semantic_body(&sir.module, &body_index, *callee)?;
            }
        }
    }
    Ok(())
}

fn require_semantic_body(
    module: &hew_sir::SemModule,
    body_index: &hew_sir::SemFunctionIndex<'_>,
    callable: hew_sir::CallableId,
) -> Result<(), SessionError> {
    if body_index.function(callable).is_none() {
        let name = module
            .callable(callable)
            .map_or("<unknown>", |item| item.symbol.as_str());
        return Err(SessionError::Lowering {
            callable: Some(callable),
            error: hew_sir::SirLoweringError::Internal(format!(
                "required semantic callable `{name}` has no body"
            )),
            span: None,
        });
    }
    Ok(())
}

#[cfg(test)]
mod session_completion_tests {
    use super::*;

    fn complete_module() -> hew_sir::LoweredModule {
        let program = parse_source(
            "fn leaf() -> i64 { 7 } fn unused() -> i64 { 9 } fn main() -> i64 { leaf() }",
            "completion.hew",
        )
        .unwrap();
        let mut checker =
            hew_types::Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
        let tco = checker.check_program(&program);
        assert!(tco.errors.is_empty(), "{:?}", tco.errors);
        Session::new(SessionTarget::native(), DiagnosticPolicy::default())
            .lower_program(&program, &tco)
            .unwrap()
            .into_semantics()
    }

    /// `unused` is declared and never called. Demand reaches neither its
    /// header nor its body, and that absence is not an incomplete lowering:
    /// the gate asks for the entry, the callees the bodies name and the
    /// selected capability plans, not for every declaration in the source.
    #[test]
    fn a_declaration_the_entry_never_reaches_does_not_make_a_module_incomplete() {
        let sir = complete_module();
        assert!(
            sir.module
                .callables
                .iter()
                .all(|callable| !callable.symbol.contains("unused")),
            "an uncalled declaration must not be admitted a callable: {:?}",
            sir.module.callables
        );
        require_complete_semantics(&sir, true).unwrap();
    }

    #[test]
    fn selected_entry_must_have_a_callable_and_body() {
        let mut sir = complete_module();
        let entry = sir.module.entry_callable.unwrap();
        sir.module
            .functions
            .retain(|function| function.callable != entry);
        assert!(matches!(
            require_complete_semantics(&sir, true),
            Err(SessionError::Lowering { callable: Some(id), error: hew_sir::SirLoweringError::Internal(_), .. }) if id == entry
        ));
        sir.module.entry_callable = None;
        assert!(matches!(
            require_complete_semantics(&sir, true),
            Err(SessionError::Lowering {
                callable: None,
                error: hew_sir::SirLoweringError::Internal(_),
                ..
            })
        ));
    }

    #[test]
    fn demanded_callee_must_have_a_body() {
        let mut sir = complete_module();
        let callee = sir
            .module
            .functions
            .iter()
            .flat_map(|function| &function.blocks)
            .find_map(|block| match block.terminator {
                hew_sir::SemTerminator::Call { callee, .. } => Some(callee),
                _ => None,
            })
            .expect("the source contains a direct call");
        sir.module
            .functions
            .retain(|function| function.callable != callee);
        assert!(matches!(
            require_complete_semantics(&sir, true),
            Err(SessionError::Lowering { callable: Some(id), error: hew_sir::SirLoweringError::Internal(_), .. }) if id == callee
        ));
    }

    #[test]
    fn lowering_refusal_cannot_be_hidden_by_a_verified_body() {
        let mut sir = complete_module();
        let entry = sir.module.entry_callable.unwrap();
        let (_, status) = sir
            .callable_statuses
            .iter_mut()
            .find(|(callable, _)| *callable == entry)
            .unwrap();
        *status = hew_sir::SirLoweringStatus::Failed {
            reason: hew_sir::SirLoweringError::Ownership {
                kind: hew_sir::SirOwnershipError::ConsumeBorrowed,
                message: "deliberate ownership refusal".into(),
            },
            span: None,
        };
        assert!(matches!(
            require_complete_semantics(&sir, true),
            Err(SessionError::Lowering { callable: Some(id), error, .. })
                if id == entry && matches!(error, hew_sir::SirLoweringError::Ownership { kind: hew_sir::SirOwnershipError::ConsumeBorrowed, .. }) && error.message().contains("deliberate ownership refusal")
        ));
    }

    #[test]
    fn source_exports_select_only_concrete_root_declarations() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(
            dir.path().join("library.hew"),
            "pub fn imported() -> i64 { 99 }",
        )
        .unwrap();
        let source = dir.path().join("exports.hew");
        std::fs::write(&source,
            "import library; pub fn answer() -> i64 { helper() } package fn neighbour() -> i64 { 8 } fn helper() -> i64 { 42 } pub fn generic<T>(value: T) -> T { value }").unwrap();
        let state =
            run_file_frontend_to_typecheck(source.to_str().unwrap(), &FrontendOptions::default())
                .unwrap();
        let tco = state.typecheck_result.tco.as_ref().unwrap();
        let output = Session::new(SessionTarget::native(), DiagnosticPolicy::default())
            .lower_program(&state.program, tco)
            .unwrap();
        let module = &output.semantics().module;
        assert!(module.entry_callable.is_none());
        assert_eq!(output.compiled_roots().len(), 2);
        // `SemFunction::name` carries the emitted symbol (`__hew_fn_answer`);
        // the declaration is the source-name authority, and selection is about
        // which declarations compile.
        let mut bodies = module
            .callables
            .iter()
            .filter(|callable| callable.source_origin == hew_sir::FunctionSourceOrigin::RootUnit)
            .map(|callable| module.defs.path(callable.declaration))
            .collect::<Vec<_>>();
        bodies.sort_unstable();
        assert_eq!(
            bodies,
            ["exports.answer", "exports.helper", "exports.neighbour"]
        );
        for root in output.compiled_roots() {
            assert!(module.function_index().function(*root).is_some());
        }
    }
}

#[derive(Debug, Clone)]
pub enum FrontendDiagnosticKind {
    Message(FrontendMessageDiagnostic),
    Parse(hew_parser::ParseError),
    Type(hew_types::TypeError),
    Hir(hew_hir::HirDiagnostic),
}

/// A frontend failure that has no parser/type/HIR payload but still needs a
/// stable machine-readable identity (for example package-module resolution).
///
/// `span`/`source` are `None` for the ordinary message-only sites (the code
/// carries the identity, but there is nowhere in the source to point at —
/// `E_IMPORT_AMBIGUOUS` spans two files, `E_PACKAGE_ROOT_MISSING` names a
/// missing file on disk). A site that *does* have a location — today just
/// `E_MODULE_NOT_FOUND`, pointing at the offending `import` — sets both so
/// the renderers can locate it instead of falling back to a zero span.
#[derive(Debug, Clone)]
pub struct FrontendMessageDiagnostic {
    pub code: String,
    pub message: String,
    pub span: Option<Range<usize>>,
    pub source: Option<Arc<str>>,
    /// Secondary locations, each with its own file and span — an import
    /// cycle's remaining edges, one per module on the path. Empty for every
    /// other message-only site.
    pub notes: Vec<FrontendMessageNote>,
    /// `= help:` lines rendered after the primary location and its notes.
    pub help: Vec<String>,
}

/// One secondary location on a [`FrontendMessageDiagnostic`] that points into
/// a *different* file than the primary span — the shape `hew-cli`'s
/// diagnostic renderer already expects for a primary/note split, but that
/// [`FrontendMessageDiagnostic`] had no way to carry until the import-cycle
/// diagnostic needed one note per remaining cycle edge.
#[derive(Debug, Clone)]
pub struct FrontendMessageNote {
    pub message: String,
    pub span: Range<usize>,
    pub source: Arc<str>,
    pub filename: String,
}

#[derive(Debug, Clone)]
pub struct FrontendDiagnostic {
    pub source: Option<Arc<str>>,
    pub filename: Option<String>,
    /// Per-note source text and filename when a secondary span belongs to a
    /// different module than the primary diagnostic.
    pub note_sources: Vec<Option<(Arc<str>, String)>>,
    pub kind: FrontendDiagnosticKind,
}

impl FrontendDiagnostic {
    fn message(message: impl Into<String>) -> Self {
        Self {
            source: None,
            filename: None,
            note_sources: Vec::new(),
            kind: FrontendDiagnosticKind::Message(FrontendMessageDiagnostic {
                code: "E_MESSAGE".to_string(),
                message: message.into(),
                span: None,
                source: None,
                notes: Vec::new(),
                help: Vec::new(),
            }),
        }
    }

    fn coded_message(code: &str, message: impl Into<String>) -> Self {
        Self {
            source: None,
            filename: None,
            note_sources: Vec::new(),
            kind: FrontendDiagnosticKind::Message(FrontendMessageDiagnostic {
                code: code.to_string(),
                message: message.into(),
                span: None,
                source: None,
                notes: Vec::new(),
                help: Vec::new(),
            }),
        }
    }

    /// A coded message that also locates itself in source — the one shape
    /// [`FrontendMessageDiagnostic`] needs to render like a `Parse`/`Type`
    /// diagnostic instead of a zero-span message. `filename` lands on the
    /// existing outer field (the same one `parse`/`type_` already populate)
    /// rather than a new one, so the renderers' `(source, filename)` pattern
    /// keeps working unchanged. See [`FrontendFailure::coded_message_at`],
    /// its only caller.
    fn coded_message_at(
        code: &str,
        message: impl Into<String>,
        span: Range<usize>,
        source: impl Into<Arc<str>>,
        filename: &str,
    ) -> Self {
        Self {
            source: None,
            filename: Some(filename.to_string()),
            note_sources: Vec::new(),
            kind: FrontendDiagnosticKind::Message(FrontendMessageDiagnostic {
                code: code.to_string(),
                message: message.into(),
                span: Some(span),
                source: Some(source.into()),
                notes: Vec::new(),
                help: Vec::new(),
            }),
        }
    }

    /// [`Self::coded_message_at`], with secondary same-diagnostic locations
    /// (each carrying its own file) and trailing `= help:` lines. Used only by
    /// the import-cycle diagnostic, whose remaining edges each live in a
    /// different module's source file.
    fn coded_message_with_notes(
        code: &str,
        message: impl Into<String>,
        span: Range<usize>,
        source: impl Into<Arc<str>>,
        filename: &str,
        notes: Vec<FrontendMessageNote>,
        help: Vec<String>,
    ) -> Self {
        Self {
            source: None,
            filename: Some(filename.to_string()),
            note_sources: Vec::new(),
            kind: FrontendDiagnosticKind::Message(FrontendMessageDiagnostic {
                code: code.to_string(),
                message: message.into(),
                span: Some(span),
                source: Some(source.into()),
                notes,
                help,
            }),
        }
    }

    fn parse(
        source: impl Into<Arc<str>>,
        filename: &str,
        diagnostic: hew_parser::ParseError,
    ) -> Self {
        Self {
            source: Some(source.into()),
            filename: Some(filename.to_string()),
            note_sources: Vec::new(),
            kind: FrontendDiagnosticKind::Parse(diagnostic),
        }
    }

    fn type_(
        source: Arc<str>,
        filename: &str,
        diagnostic: hew_types::TypeError,
        module_source_map: &SourceSnapshot,
    ) -> Self {
        let note_sources = diagnostic
            .notes
            .iter()
            .map(|(_, _, source_module)| {
                source_module
                    .as_deref()
                    .and_then(|module| module_source_map.source(module))
            })
            .collect();
        Self {
            source: Some(source),
            filename: Some(filename.to_string()),
            note_sources,
            kind: FrontendDiagnosticKind::Type(diagnostic),
        }
    }

    fn hir(
        source: Option<Arc<str>>,
        filename: Option<&str>,
        diagnostic: hew_hir::HirDiagnostic,
    ) -> Self {
        Self {
            source,
            filename: filename.map(str::to_string),
            note_sources: Vec::new(),
            kind: FrontendDiagnosticKind::Hir(diagnostic),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FrontendFailure {
    pub message: String,
    pub diagnostics: Vec<FrontendDiagnostic>,
    /// Inputs observed before this frontend request stopped.
    pub inputs: Vec<hew_types::compile_time::CompilationInput>,
}

impl FrontendFailure {
    fn new(message: impl Into<String>, diagnostics: Vec<FrontendDiagnostic>) -> Self {
        Self {
            message: message.into(),
            diagnostics,
            inputs: Vec::new(),
        }
    }

    fn message_only(message: impl Into<String>) -> Self {
        Self::new(message, Vec::new())
    }

    fn coded_message(code: &str, message: impl Into<String>) -> Self {
        let message = message.into();
        Self::new(
            message.clone(),
            vec![FrontendDiagnostic::coded_message(code, message)],
        )
    }

    /// [`FrontendFailure::coded_message`], but the diagnostic also carries a
    /// span and the source it indexes into, so a renderer can point at it
    /// instead of showing a bare message. Used only by `E_MODULE_NOT_FOUND`,
    /// which can locate the offending `import` statement.
    fn coded_message_at(
        code: &str,
        message: impl Into<String>,
        span: Range<usize>,
        source: &str,
        filename: &str,
    ) -> Self {
        let message = message.into();
        Self::new(
            message.clone(),
            vec![FrontendDiagnostic::coded_message_at(
                code, message, span, source, filename,
            )],
        )
    }

    /// [`Self::coded_message_at`], with secondary cross-file locations and
    /// help lines. See [`FrontendDiagnostic::coded_message_with_notes`].
    fn coded_message_with_notes(
        code: &str,
        message: impl Into<String>,
        span: Range<usize>,
        source: &str,
        filename: &str,
        notes: Vec<FrontendMessageNote>,
        help: Vec<String>,
    ) -> Self {
        let message = message.into();
        Self::new(
            message.clone(),
            vec![FrontendDiagnostic::coded_message_with_notes(
                code, message, span, source, filename, notes, help,
            )],
        )
    }
}

impl fmt::Display for FrontendFailure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.message.fmt(f)
    }
}

impl std::error::Error for FrontendFailure {}

fn is_warning_diagnostic(d: &FrontendDiagnostic) -> bool {
    match &d.kind {
        FrontendDiagnosticKind::Type(e) => e.severity == hew_types::error::Severity::Warning,
        FrontendDiagnosticKind::Parse(e) => e.severity == hew_parser::Severity::Warning,
        FrontendDiagnosticKind::Message(_) | FrontendDiagnosticKind::Hir(_) => false,
    }
}

/// Compare source paths across canonical and editor-provided spellings.
#[must_use]
pub fn paths_name_same_file(left: &Path, right: &Path) -> bool {
    left == right
        || match (std::fs::canonicalize(left), std::fs::canonicalize(right)) {
            (Ok(left), Ok(right)) => left == right,
            _ => false,
        }
}

fn configured_stdlib_roots(options: &FrontendOptions) -> Vec<PathBuf> {
    options
        .module_search_paths
        .clone()
        .unwrap_or_else(hew_types::module_registry::stdlib_search_paths)
        .into_iter()
        .map(|root| root.join("std"))
        .collect()
}

fn path_is_below(path: &Path, root: &Path) -> bool {
    match (std::fs::canonicalize(path), std::fs::canonicalize(root)) {
        (Ok(path), Ok(root)) => path.starts_with(root),
        _ => path
            .strip_prefix(".")
            .unwrap_or(path)
            .starts_with(root.strip_prefix(".").unwrap_or(root)),
    }
}

/// Remove diagnostics owned by an imported standard-library source before a
/// user-facing pipeline returns them. Users cannot act on compiler-shipped
/// implementation sites. A direct check of a file below `std/` deliberately
/// retains its diagnostics so the stdlib source gate remains authoritative.
fn retain_user_facing_diagnostics(
    root_filename: &str,
    stdlib_roots: &[PathBuf],
    diagnostics: &mut Vec<FrontendDiagnostic>,
) {
    diagnostics
        .retain(|diagnostic| !is_stdlib_owned_diagnostic(root_filename, stdlib_roots, diagnostic));
}

/// Whether a diagnostic belongs to an imported standard-library source rather
/// than the file being checked.
fn is_stdlib_owned_diagnostic(
    root_filename: &str,
    stdlib_roots: &[PathBuf],
    diagnostic: &FrontendDiagnostic,
) -> bool {
    let Some(filename) = diagnostic.filename.as_deref() else {
        return false;
    };
    let diagnostic_path = Path::new(filename);
    !paths_name_same_file(Path::new(root_filename), diagnostic_path)
        && stdlib_roots
            .iter()
            .any(|stdlib_root| path_is_below(diagnostic_path, stdlib_root))
}

/// If `options.warnings_as_errors` is set and `diagnostics` contains any
/// warning-severity entry, return a `FrontendFailure` that includes all
/// accumulated diagnostics.  Otherwise return `Ok(())`.
///
/// Call this in the success arm of every top-level pipeline function
/// (`check_file`, `check_program`) so the behaviour is uniform across all
/// public entry points.
fn fail_on_warning_diagnostics(
    diagnostics: Vec<FrontendDiagnostic>,
    options: &FrontendOptions,
) -> Result<Vec<FrontendDiagnostic>, FrontendFailure> {
    if options.warnings_as_errors && diagnostics.iter().any(is_warning_diagnostic) {
        return Err(FrontendFailure::new(
            "warnings treated as errors",
            diagnostics,
        ));
    }
    Ok(diagnostics)
}

#[derive(Debug, Clone, Default)]
pub struct CheckOutput {
    pub inputs: Vec<hew_types::compile_time::CompilationInput>,
    pub diagnostics: Vec<FrontendDiagnostic>,
    /// Diagnostic-only stack-allocation hints emitted by the checker's
    /// escape-analysis pass. Surfaced behind `hew check --show-stack-hints`.
    /// Empty when type-checking failed before the walker ran.
    pub stack_hints: Vec<hew_types::check::StackHint>,
    /// Source content of the checked file, used for line/column mapping in
    /// `--explain-cow` output. Empty when type-checking is skipped.
    /// Source text of the checked file, retained so the CLI can render
    /// `--show-stack-hints` / `--explain-cow` lines with `file:line:col` attribution.
    /// Empty when the input could not be loaded.
    pub source: String,
}

#[derive(Clone, Debug)]
pub struct ResolvedImport {
    items: std::sync::Arc<Vec<Spanned<Item>>>,
    item_source_paths: Vec<PathBuf>,
    source_paths: Vec<PathBuf>,
}

#[derive(Debug)]
pub struct ImportResolutionContext<'a> {
    pub in_progress_imports: HashSet<PathBuf>,
    pub resolved_imports: HashMap<PathBuf, ResolvedImport>,
    pub manifest_deps: Option<&'a [String]>,
    pub extra_pkg_path: Option<&'a Path>,
    pub locked_versions: Option<&'a [(String, String)]>,
    pub package_name: Option<&'a str>,
    pub project_dir: &'a Path,
    pub module_search_paths: Option<&'a [PathBuf]>,
    /// Open buffers that override on-disk content while resolving imports.
    pub documents: &'a SourceSet,
}

#[derive(Debug)]
struct LockedPackageCheck {
    package_dir: PathBuf,
    name: String,
    version: String,
}

#[derive(Debug)]
pub struct TypeCheckResult {
    /// Exact admitted source text and routing retained for later compiler stages.
    pub sources: Arc<SourceSnapshot>,
    pub tco: Option<hew_types::check::TypeCheckOutput>,
    pub module_registry: hew_types::module_registry::ModuleRegistry,
}

struct ProjectContext {
    source: Arc<str>,
    project_dir: PathBuf,
    manifest_deps: Option<Vec<String>>,
    package_name: Option<String>,
    locked_versions: Option<Vec<(String, String)>>,
}

/// Source text and routing from one frontend admission, shared by diagnostic stages.
#[derive(Debug)]
pub struct SourceSnapshot {
    root_source: Arc<str>,
    root_filename: String,
    modules: HashMap<String, (Arc<str>, String)>,
    documents: SourceSet,
}

impl SourceSnapshot {
    /// Input fingerprints observed in this admission, including failed reads.
    #[must_use]
    pub fn inputs(&self) -> Vec<hew_types::compile_time::CompilationInput> {
        self.documents.inputs()
    }

    /// The checked root text and its diagnostic label.
    #[must_use]
    pub fn root(&self) -> (&str, &str) {
        (&self.root_source, &self.root_filename)
    }

    /// Authored module and per-file aliases, all referring to admitted text.
    #[must_use]
    pub fn modules(&self) -> &HashMap<String, (Arc<str>, String)> {
        &self.modules
    }

    fn source(&self, origin: &str) -> Option<(Arc<str>, String)> {
        self.modules.get(origin).cloned().or_else(|| {
            self.documents
                .admitted_diagnostic_text(Path::new(origin))
                .map(|source| (source, display_path(Path::new(origin))))
        })
    }
}

#[must_use]
pub fn line_map_from_source(source: &str) -> Vec<usize> {
    let mut map = vec![0usize];
    let bytes = source.as_bytes();
    for (i, &byte) in bytes.iter().enumerate() {
        if byte == b'\n' {
            map.push(i + 1);
        }
    }
    map
}

fn merge_prior_diagnostics(
    mut prior: Vec<FrontendDiagnostic>,
    mut failure: FrontendFailure,
) -> FrontendFailure {
    prior.extend(failure.diagnostics);
    failure.diagnostics = prior;
    failure
}

/// The module imports of a package's root file that its manifest does not
/// declare and nothing local supplies.
///
/// An import that resolves from the standard library, from the package's
/// own name or beside the importing file needs no declaration; anything else
/// must be a declared dependency, whether or not it is installed.
fn undeclared_imports(
    source_file: &Path,
    items: &[Spanned<Item>],
    ctx: &ImportResolutionContext<'_>,
) -> Result<Vec<String>, FrontendFailure> {
    let Some(manifest_deps) = ctx.manifest_deps else {
        return Ok(Vec::new());
    };
    let std_roots = stdlib_roots(ctx.module_search_paths);
    let anchor = importer_anchor(source_file, ctx, &std_roots)?;
    let mut errors = Vec::new();
    for (item, _) in items {
        let Item::Import(decl) = item else { continue };
        if decl.file_path.is_some() || decl.path.segments.is_empty() {
            continue;
        }
        let segments = import_segments(&decl.path);
        // The package's own name is local: a module it does not have is
        // reported as not found, not as an undeclared dependency.
        let names_own_package = matches!(&anchor,
            hew_types::module_registry::ModuleAnchor::Package { name: Some(package), .. }
                if package_relative(&decl.path, package).is_some());
        if names_own_package
            || is_builtin_module(&segments.join("::"))
            || declares_dependency(Some(manifest_deps), &segments)
        {
            continue;
        }
        let resolves_locally = module_candidates(&decl.path, source_file, &anchor, ctx, &std_roots)
            .candidates
            .iter()
            .any(|entry| {
                entry.2 != CandidateRoot::Dependency
                    && resolve_module_candidate(ctx, &std_roots, entry)
                        .is_ok_and(|found| found.is_some())
            });
        if !resolves_locally {
            let source_module = segments.join(".");
            errors.push(format!(
                "Error: module `{source_module}` is not declared in hew.toml\n  hint: add it with `hew add {source_module}`"
            ));
        }
    }
    Ok(errors)
}

fn is_builtin_module(module_path: &str) -> bool {
    module_path.starts_with("std::")
        || module_path.starts_with("hew::")
        || module_path.starts_with("ecosystem::")
}

/// The closest existing `std` module to a mistyped import's last path
/// segment: an exact leaf match (`std.server` -> `std.net.http.server`, a missed or
/// mis-nested path segment) when one exists, else a near-miss typo
/// (`std.servr` -> `std.net.http.server`).
///
/// Near-miss matching compares leaf names (the last dotted segment), not
/// full dotted paths: `std.servr` and `std.net.http.server` differ by far more than
/// [`hew_types::error::find_similar`]'s length-scaled Levenshtein threshold
/// allows, but `htp` and `http` — the part the user actually mistyped — are
/// one edit apart. An exact leaf match is checked first because
/// `find_similar` filters exact matches out entirely (it exists to catch
/// spelling, not path mistakes) — without this, the single most likely
/// real-world slip (the right module name, wrong or missing nesting) would
/// get no suggestion at all. Returns `None` when neither finds anything.
fn nearest_std_module(leaf: &str, search_paths: &[PathBuf]) -> Option<String> {
    let mut modules = Vec::new();
    for root in search_paths {
        let std_dir = root.join("std");
        if std_dir.is_dir() {
            collect_std_module_names(&std_dir, &mut modules);
        }
    }
    let module_leaf = |module: &ModulePath| module.segments.last().map_or("", |leaf| leaf.as_str());
    if let Some(exact) = modules.iter().find(|module| module_leaf(module) == leaf) {
        return Some(exact.dotted());
    }
    let leaves: Vec<&str> = modules.iter().map(module_leaf).collect();
    let best_leaf = hew_types::error::find_similar(leaf, leaves.iter().copied())
        .into_iter()
        .next()?;
    modules
        .iter()
        .find(|module| module_leaf(module) == best_leaf)
        .map(ModulePath::dotted)
}

/// Recursively collect every `std.…` module path reachable under `std_dir`
/// into `out`.
///
/// Mirrors the two entry-file shapes the import resolver's own per-import
/// candidate list already assumes (`dir_path`/`rel_path` above: a directory
/// names its module through a same-named `<dir>/<dir-name>.hew`, or a flat
/// `<name>.hew` file sits directly at that path) — this walks the whole
/// tree once instead of probing one path, so "did you mean" has a full
/// candidate list to compare a typo against. Cold path (only runs once
/// import resolution has already failed), so no caching: the stdlib tree is
/// a few hundred files, and this only walks it on a diagnostic.
fn collect_std_module_names(std_dir: &Path, out: &mut Vec<ModulePath>) {
    let mut segments = vec!["std".to_string()];
    collect_module_names_at(std_dir, &mut segments, out);
}

fn collect_module_names_at(dir: &Path, segments: &mut Vec<String>, out: &mut Vec<ModulePath>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            let Some(name) = path.file_name().and_then(std::ffi::OsStr::to_str) else {
                continue;
            };
            segments.push(name.to_string());
            collect_module_names_at(&path, segments, out);
            segments.pop();
        } else if path.extension().is_some_and(|ext| ext == "hew") {
            let Some(stem) = path.file_stem().and_then(std::ffi::OsStr::to_str) else {
                continue;
            };
            if segments.last().map(String::as_str) == Some(stem) {
                // `<dir>/<dir-name>.hew` — the directory's own canonical entry.
                out.push(ModulePath::new(segments.iter()));
            } else {
                // A flat file: its own name is the trailing path segment.
                segments.push(stem.to_string());
                out.push(ModulePath::new(segments.iter()));
                segments.pop();
            }
        }
    }
}

fn load_project_context(
    input: &str,
    options: Option<&FrontendOptions>,
    source_override: Option<&str>,
) -> Result<ProjectContext, FrontendFailure> {
    // A directory is a package root, not a source file. The CLI resolves
    // package forms through the manifest before calling in here; anything else
    // reaching this point gets a real diagnostic rather than the raw
    // `Is a directory` OS error a bare read would surface.
    let documents = options.map_or(&*EMPTY_DOCUMENTS, |options| &options.documents);
    let source = if let Some(source) = source_override {
        documents
            .get(Path::new(input))
            .map_or_else(|| Arc::from(source), Arc::from)
    } else {
        if documents.filesystem && Path::new(input).is_dir() {
            return Err(FrontendFailure::message_only(format!(
                "Error: {input} is a directory, not a .hew source file\n  \
                 hint: a package directory is built with `hew build {input}`"
            )));
        }
        read_source(documents, Path::new(input)).map_err(|e| {
            FrontendFailure::message_only(format!("Error: cannot read {input}: {e}"))
        })?
    };
    let input_dir = Path::new(input).parent().unwrap_or(Path::new("."));
    let project_dir = options
        .and_then(|options| options.project_dir.clone())
        .or_else(|| {
            input_dir
                .ancestors()
                .find(|dir| documents.is_file(&dir.join("hew.toml")))
                .map(Path::to_path_buf)
        })
        .unwrap_or_else(|| input_dir.to_path_buf());
    let (manifest_deps, package_name) = load_manifest_metadata_from(&project_dir, documents)?;
    Ok(ProjectContext {
        source,
        project_dir: project_dir.clone(),
        manifest_deps,
        package_name,
        locked_versions: if options.is_some_and(|options| options.ignore_lockfile) {
            None
        } else {
            load_lockfile_from(&project_dir, documents)?
        },
    })
}

fn file_import(file_path: String) -> Spanned<Item> {
    (
        Item::Import(ImportDecl {
            path: hew_parser::ast::Path {
                segments: Vec::new(),
            },
            spec: None,
            selection_trailing_comma: false,
            module_alias: None,
            file_path: Some(file_path),
            resolved_items: None,
            resolved_item_source_paths: Vec::new(),
            resolved_source_paths: Vec::new(),
        }),
        0..0,
    )
}

fn project_context_for_program(
    source: &str,
    options: &FrontendOptions,
) -> Result<ProjectContext, FrontendFailure> {
    match &options.project_dir {
        Some(dir) => {
            let (manifest_deps, package_name) =
                load_manifest_metadata_from(dir, &options.documents)?;
            Ok(ProjectContext {
                source: source.into(),
                project_dir: dir.clone(),
                manifest_deps,
                package_name,
                locked_versions: if options.ignore_lockfile {
                    None
                } else {
                    load_lockfile_from(dir, &options.documents)?
                },
            })
        }
        None => Ok(ProjectContext {
            source: source.into(),
            project_dir: std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")),
            manifest_deps: None,
            package_name: None,
            locked_versions: None,
        }),
    }
}

fn parse_source_with_diagnostics(
    source: &str,
    input: &str,
) -> Result<(Program, Vec<FrontendDiagnostic>), FrontendFailure> {
    let result = hew_parser::parse(source);
    let diagnostics = result
        .errors
        .iter()
        .cloned()
        .map(|diagnostic| FrontendDiagnostic::parse(source, input, diagnostic))
        .collect::<Vec<_>>();
    if result
        .errors
        .iter()
        .any(|error| error.severity == hew_parser::Severity::Error)
    {
        return Err(FrontendFailure::new("parsing failed", diagnostics));
    }
    Ok((result.program, diagnostics))
}

/// Parse Hew source into an AST program.
///
/// # Errors
///
/// Returns [`FrontendFailure`] when parsing reports any error-severity
/// diagnostic for the supplied source.
pub fn parse_source(source: &str, input: &str) -> Result<Program, FrontendFailure> {
    parse_source_with_diagnostics(source, input).map(|(program, _)| program)
}

fn resolve_imports_internal(
    program: &mut Program,
    source: &str,
    input: &str,
    project: &ProjectContext,
    options: &FrontendOptions,
    diagnostics: &mut Vec<FrontendDiagnostic>,
) -> Result<(), FrontendFailure> {
    inject_implicit_imports(&mut program.items, source);

    let input_path = Path::new(input);
    inject_prelude_module_loads(&mut program.items, input_path);
    let mut import_ctx = ImportResolutionContext {
        in_progress_imports: HashSet::new(),
        resolved_imports: HashMap::new(),
        manifest_deps: project.manifest_deps.as_deref(),
        extra_pkg_path: options.pkg_path.as_deref(),
        locked_versions: project.locked_versions.as_deref(),
        package_name: project.package_name.as_deref(),
        project_dir: &project.project_dir,
        module_search_paths: options.module_search_paths.as_deref(),
        documents: &options.documents,
    };
    let input_canonical = resolve_candidate(&options.documents, input_path)
        .unwrap_or_else(|| input_path.to_path_buf());
    let undeclared = undeclared_imports(&input_canonical, &program.items, &import_ctx)?;
    if !undeclared.is_empty() {
        return Err(FrontendFailure::new(
            "undeclared dependencies",
            undeclared
                .into_iter()
                .map(FrontendDiagnostic::message)
                .collect(),
        ));
    }
    let module_graph = build_module_graph_with_diagnostics(
        input_path,
        &mut program.items,
        program.module_doc.clone(),
        &mut import_ctx,
        diagnostics,
    )?;
    program.module_graph = Some(module_graph);
    let mut sources = BTreeMap::new();
    for module in program
        .module_graph
        .as_ref()
        .expect("graph was built")
        .modules
        .values()
    {
        for path in &module.source_paths {
            if let Ok(text) = read_source(&options.documents, path) {
                sources.insert(path.clone(), text.to_string());
            }
        }
    }
    sources.insert(input_canonical, source.to_string());
    if !options.documents.prepared_expansions.templates.is_empty()
        || !options.documents.prepared_expansions.expansions.is_empty()
    {
        program.expansions = (*options.documents.prepared_expansions).clone();
    }
    if let Err((path, error)) = hew_expand::expand_program(program, sources) {
        let text = options
            .documents
            .admitted_text(&path)
            .unwrap_or_else(|| Arc::from(source));
        return Err(FrontendFailure::new(
            "macro expansion failed",
            vec![FrontendDiagnostic::parse(text, &display_path(&path), error)],
        ));
    }
    Ok(())
}

/// Strip Windows' extended-length verbatim prefix (`\\?\`, or `\\?\UNC\` for
/// a UNC share) from a path's rendered form (#3416).
///
/// `Path::canonicalize()` returns this form on Windows so a map key dedupes
/// reliably against symlinks and relative spellings, but a user never typed
/// it and a diagnostic must not show it. This only ever changes the display
/// string a caller renders; the canonical path itself remains the identity
/// key everywhere it is used for routing or lookup.
fn display_path(path: &Path) -> String {
    let text = path.display().to_string();
    text.strip_prefix(r"\\?\UNC\")
        .map(|rest| format!(r"\\{rest}"))
        .or_else(|| text.strip_prefix(r"\\?\").map(str::to_string))
        .unwrap_or(text)
}

fn build_module_source_map(
    program: &Program,
    root_source: &str,
    root_filename: &str,
    documents: &SourceSet,
) -> SourceSnapshot {
    let root_source = documents
        .read_text(Path::new(root_filename))
        .unwrap_or_else(|_| Arc::from(root_source));
    let mut snapshot = SourceSnapshot {
        root_source,
        root_filename: root_filename.to_string(),
        modules: HashMap::new(),
        documents: documents.clone(),
    };
    let Some(ref module_graph) = program.module_graph else {
        return snapshot;
    };

    let map = &mut snapshot.modules;
    for mod_id in &module_graph.topo_order {
        if *mod_id == module_graph.root {
            continue;
        }
        let Some(module) = module_graph.modules.get(mod_id) else {
            continue;
        };
        let Some(path) = module.source_paths.first() else {
            // A prelude module attached without a search path carries its
            // compiled-in source instead.
            if let [std, leaf] = mod_id.segments.as_slice() {
                if let Some((_, text)) = hew_types::stdlib_authority::IMPLICIT_PRELUDE_STD_SOURCES
                    .iter()
                    .find(|(name, _)| std.as_str() == "std" && *name == leaf.as_str())
                {
                    map.insert(
                        mod_id.dotted(),
                        (Arc::from(*text), format!("std/{leaf}.hew")),
                    );
                }
            }
            continue;
        };
        if let Ok(text) = read_source(documents, path) {
            map.insert(mod_id.dotted(), (text, display_path(path)));
        }
        // Per-file routing entries (rc1-F1 stage C): a directory module's
        // item spans are file-relative offsets, so the checker routes a
        // diagnostic on a peer-file item by the file's own path token. Every
        // source file of every module resolves under that token. The map key
        // stays the canonical spelling (identity, joined against elsewhere);
        // only the rendered label is stripped.
        for path in &module.source_paths {
            let key = path.display().to_string();
            if map.contains_key(&key) {
                continue;
            }
            if let Ok(text) = read_source(documents, path) {
                map.insert(key, (text, display_path(path)));
            }
        }
    }
    snapshot
}

fn macro_type_origin(
    program: &Program,
    mut diagnostic: hew_types::TypeError,
) -> hew_types::TypeError {
    // Expansion attribution may fall back to the whole invocation. Such an
    // origin is useful for reporting, but is not an exact editable range.
    diagnostic.fixes.retain(|fix| {
        fix.edits.iter().all(|edit| {
            program
                .expansions
                .source_origin(&(edit.span.start..edit.span.end))
                .is_none()
        })
    });
    if let Some((path, span)) = program.expansions.source_origin(&diagnostic.span) {
        diagnostic.fixes.clear();
        if let Some((invocation_path, invocation_span)) =
            program.expansions.invocation_origin(&diagnostic.span)
        {
            diagnostic.notes.push((
                invocation_span,
                "in this macro invocation".into(),
                Some(invocation_path.display().to_string()),
            ));
        }
        diagnostic.span = span;
        diagnostic.source_module = Some(path.display().to_string());
    }
    for (span, _, source) in &mut diagnostic.notes {
        if let Some((path, original)) = program.expansions.source_origin(span) {
            *span = original;
            *source = Some(path.display().to_string());
        }
    }
    diagnostic
}

fn type_diagnostic_to_frontend(
    diagnostic: hew_types::TypeError,
    sources: &SourceSnapshot,
) -> FrontendDiagnostic {
    let (source, filename) = diagnostic
        .source_module
        .as_deref()
        .and_then(|origin| sources.source(origin))
        .unwrap_or_else(|| {
            (
                Arc::clone(&sources.root_source),
                sources.root_filename.clone(),
            )
        });
    FrontendDiagnostic::type_(source, &filename, diagnostic, sources)
}

fn hir_diagnostic_to_frontend(
    diagnostic: hew_hir::HirDiagnostic,
    sources: &SourceSnapshot,
) -> FrontendDiagnostic {
    let source = match diagnostic.source_module.as_deref() {
        None => Some((
            Arc::clone(&sources.root_source),
            sources.root_filename.clone(),
        )),
        Some(module) => sources.source(module),
    };
    match source {
        Some((source, filename)) => {
            FrontendDiagnostic::hir(Some(source), Some(&filename), diagnostic)
        }
        None => FrontendDiagnostic::hir(None, None, diagnostic),
    }
}

/// Route HIR diagnostics through the exact source context admitted by the frontend.
/// Unknown foreign origins remain unavailable rather than using the root file.
#[must_use]
pub fn hir_diagnostics_to_frontend(
    diagnostics: Vec<hew_hir::HirDiagnostic>,
    sources: &SourceSnapshot,
) -> Vec<FrontendDiagnostic> {
    diagnostics
        .into_iter()
        .map(|diagnostic| hir_diagnostic_to_frontend(diagnostic, sources))
        .collect()
}

/// Attribute ownership findings using the same admitted source text as type checking.
#[must_use]
pub fn ownership_diagnostics_to_frontend(
    diagnostics: Vec<hew_sir::SirDiagnostic>,
    sources: &SourceSnapshot,
) -> Vec<FrontendDiagnostic> {
    let source_for = |origin: &hew_sir::FunctionSourceOrigin| match origin {
        hew_sir::FunctionSourceOrigin::RootUnit => Some((
            Arc::clone(&sources.root_source),
            sources.root_filename.clone(),
        )),
        hew_sir::FunctionSourceOrigin::Foreign(module) => sources.source(module),
        hew_sir::FunctionSourceOrigin::Unknown => None,
    };
    diagnostics
        .into_iter()
        .map(|diagnostic| {
            if let hew_sir::SirDiagnosticKind::InvalidGeneratorSend { message, site } =
                &diagnostic.kind
            {
                return source_for(&site.source).map_or_else(
                    || FrontendDiagnostic::coded_message("InvalidSend", message.clone()),
                    |(source, filename)| FrontendDiagnostic {
                        source: Some(source),
                        filename: Some(filename),
                        note_sources: Vec::new(),
                        kind: FrontendDiagnosticKind::Type(hew_types::TypeError::new(
                            hew_types::error::TypeErrorKind::InvalidSend,
                            site.span.clone(),
                            message.clone(),
                        )),
                    },
                );
            }
            let hew_sir::SirDiagnosticKind::UnconsumedLinear {
                binding,
                span,
                source_origin,
                ..
            } = diagnostic.kind
            else {
                unreachable!("SessionError::Ownership contains source ownership findings");
            };
            let message = binding.map_or_else(
                || "linear value must be consumed before this normal exit".to_string(),
                |binding| {
                    format!("linear value `{binding}` must be consumed before this normal exit")
                },
            );
            source_for(&source_origin).map_or_else(
                || FrontendDiagnostic::coded_message("E_MUST_CONSUME", message.clone()),
                |(source, filename)| {
                    FrontendDiagnostic::coded_message_at(
                        "E_MUST_CONSUME",
                        message.clone(),
                        span,
                        source,
                        &filename,
                    )
                },
            )
        })
        .collect()
}

/// Resolve the checker's module search paths for one type-checking pass: the
/// configured paths, or the standard-library root.
fn checker_search_paths(options: &FrontendOptions) -> Vec<PathBuf> {
    options
        .module_search_paths
        .clone()
        .unwrap_or_else(hew_types::module_registry::stdlib_search_paths)
}

fn typecheck_program_with_diagnostics(
    program: &Program,
    source: &str,
    input: &str,
    options: &FrontendOptions,
    entry_selection: Option<hew_types::DeclarationOccurrence>,
) -> (TypeCheckResult, Vec<FrontendDiagnostic>) {
    typecheck_program_with_dependency_cache(program, source, input, options, entry_selection, None)
}

fn typecheck_program_with_dependency_cache(
    program: &Program,
    source: &str,
    input: &str,
    options: &FrontendOptions,
    entry_selection: Option<hew_types::DeclarationOccurrence>,
    cache: Option<&mut hew_types::check::DependencyAnalysisCache>,
) -> (TypeCheckResult, Vec<FrontendDiagnostic>) {
    let search_paths = checker_search_paths(options);
    let module_registry = hew_types::module_registry::ModuleRegistry::new(search_paths);

    let module_source_map = Arc::new(build_module_source_map(
        program,
        source,
        input,
        &options.documents,
    ));
    if options.no_typecheck {
        return (
            TypeCheckResult {
                sources: module_source_map,
                tco: None,
                module_registry,
            },
            Vec::new(),
        );
    }

    let mut checker = hew_types::Checker::new(module_registry);
    if let Some(seed) = &options.session_type_seed {
        checker.set_session_type_seed(seed.clone());
    }
    checker.set_input_provider(Arc::new(compile_time::Inputs::new(
        input,
        options.documents.clone(),
        hew_types::module_registry::stdlib_search_paths(),
    )));
    if options.enable_wasm_target {
        checker.enable_wasm_target();
    }
    if options.repl_fragment {
        checker.set_repl_fragment();
    }
    if !options.test_entry_selections.is_empty() {
        checker.set_test_entry_selections(options.test_entry_selections.clone());
        if let Some(module) = canonical_direct_stdlib_module_for_source(Path::new(input)) {
            checker.set_test_entry_module(module);
        }
    } else if let Some(entry_selection) = entry_selection {
        checker.set_entry_selection(entry_selection);
    }
    checker.set_lint_levels(options.lint_levels.clone());
    // Install source text so the lint sweep can resolve in-source
    // `// hew:allow(...)` directives. The root source owns the entry file's
    // spans; each non-root module owns its own (built from the same source map
    // the diagnostic renderer uses below).
    let mut lint_sources = hew_types::LintSources::new();
    lint_sources.set_root(Arc::clone(&module_source_map.root_source));
    for (module, (module_source, _filename)) in module_source_map.modules() {
        lint_sources.set_module(module.clone(), Arc::clone(module_source));
    }
    checker.set_lint_sources(lint_sources);
    let tco = if let Some(cache) = cache {
        let sources = module_source_map
            .modules()
            .iter()
            .map(|(module, (text, filename))| (module.clone(), Arc::clone(text), filename.clone()))
            .collect();
        cache.check_program(&mut checker, program, sources)
    } else {
        checker.check_program(program)
    };
    let mut diagnostics = tco
        .errors
        .iter()
        .cloned()
        .map(|diagnostic| {
            type_diagnostic_to_frontend(macro_type_origin(program, diagnostic), &module_source_map)
        })
        .collect::<Vec<_>>();
    diagnostics.extend(tco.warnings.iter().cloned().map(|diagnostic| {
        type_diagnostic_to_frontend(macro_type_origin(program, diagnostic), &module_source_map)
    }));

    let module_registry = checker.into_module_registry();
    (
        TypeCheckResult {
            sources: module_source_map,
            tco: Some(tco),
            module_registry,
        },
        diagnostics,
    )
}

/// Type-check a parsed program after import resolution.
///
/// This is a low-level primitive that expects imports to have been resolved
/// before the call.  For a project-aware check that handles manifest
/// validation and import resolution automatically, use [`check_program`] or
/// [`check_file`].
///
/// # Errors
///
/// Returns [`FrontendFailure`] when type checking reports any hard errors.
pub fn typecheck_program(
    program: &Program,
    source: &str,
    input: &str,
    options: &FrontendOptions,
) -> Result<TypeCheckResult, FrontendFailure> {
    let mut options = options.clone();
    options.documents.insert(input, source);
    options.documents = options.documents.for_frontend();
    let _ = options.documents.read_bytes(Path::new(input));
    let options = &options;
    require_host_typecheck(options)?;
    let (result, mut diagnostics) =
        typecheck_program_with_diagnostics(program, source, input, options, None);
    if type_check_failed(&result) {
        return Err(FrontendFailure::new("type errors found", diagnostics));
    }
    if let Some(output) = result.tco.as_ref() {
        let refused = host_admission::check(program, output, source, input, options);
        if !refused.is_empty() {
            diagnostics.extend(refused);
            return Err(FrontendFailure::new(
                "host operations refused by the execution profile",
                diagnostics,
            ));
        }
    }
    Ok(result)
}

/// Whether the checker reported hard errors for this run.
fn type_check_failed(result: &TypeCheckResult) -> bool {
    result
        .tco
        .as_ref()
        .is_some_and(|tco| !tco.errors.is_empty())
}

/// Resolve imports and type-check an already-parsed in-memory program.
///
/// This is the in-memory counterpart to [`check_file`]: it runs the same
/// project-aware pipeline (manifest validation, import resolution, type
/// checking) without needing a file on disk.
///
/// Set [`FrontendOptions::project_dir`] to anchor dependency resolution and
/// manifest validation to a specific project directory.  When `None` the
/// current working directory is used and manifest validation is skipped.
///
/// # Errors
///
/// Returns [`FrontendFailure`] when manifest loading, import resolution, or
/// type checking fails.
pub fn check_program(
    mut program: Program,
    source: &str,
    source_label: &str,
    options: &FrontendOptions,
) -> Result<CheckOutput, FrontendFailure> {
    let mut options = options.clone();
    options.documents.insert(source_label, source);
    options.documents = options.documents.for_frontend();
    let _ = options.documents.read_bytes(Path::new(source_label));
    let options = &options;
    require_host_typecheck(options)?;
    let project = project_context_for_program(source, options)?;
    let mut diagnostics = Vec::new();

    if let Err(failure) = resolve_imports_internal(
        &mut program,
        source,
        source_label,
        &project,
        options,
        &mut diagnostics,
    ) {
        return Err(merge_prior_diagnostics(diagnostics, failure));
    }

    let (tcr, type_diagnostics) =
        typecheck_program_with_diagnostics(&program, source, source_label, options, None);
    diagnostics.extend(type_diagnostics);
    if type_check_failed(&tcr) {
        return Err(FrontendFailure::new("type errors found", diagnostics));
    }
    if let Some(output) = tcr.tco.as_ref() {
        let refused = host_admission::check(&program, output, source, source_label, options);
        if !refused.is_empty() {
            diagnostics.extend(refused);
            return Err(FrontendFailure::new(
                "host operations refused by the execution profile",
                diagnostics,
            ));
        }
    }
    let diagnostics = fail_on_warning_diagnostics(diagnostics, options)?;
    let stack_hints = tcr
        .tco
        .as_ref()
        .map(|tco| tco.stack_hints.clone())
        .unwrap_or_default();
    Ok(CheckOutput {
        inputs: tcr
            .tco
            .as_ref()
            .map(|output| output.inputs.clone())
            .unwrap_or_default(),
        diagnostics,
        stack_hints,
        source: source.to_string(),
    })
}

pub fn inject_implicit_imports(items: &mut Vec<Spanned<Item>>, source: &str) {
    let existing = items
        .iter()
        .filter_map(|(item, _)| {
            if let Item::Import(decl) = item {
                if !decl.path.segments.is_empty() {
                    return Some(import_segments(&decl.path).join("::"));
                }
            }
            None
        })
        .collect::<HashSet<_>>();

    let mut needed: Vec<&[&str]> = Vec::new();
    if source_contains_regex_literal(source) {
        let path: &[&str] = &["std", "text", "regex"];
        let key = path.join("::");
        if !existing.contains(&key) {
            needed.push(path);
        }
    }

    let mut seen = HashSet::new();
    for path in needed {
        let key = path.join("::");
        if seen.insert(key) {
            items.push((
                Item::Import(ImportDecl {
                    path: hew_parser::ast::Path::from_spellings(path),
                    spec: None,
                    selection_trailing_comma: false,
                    module_alias: None,
                    file_path: None,
                    resolved_items: None,
                    resolved_item_source_paths: Vec::new(),
                    resolved_source_paths: Vec::new(),
                }),
                0..0,
            ));
        }
    }
}

/// Load the modules whose declarations the prelude publishes. `monitor`
/// returns a `MonitorRef`, so `std.link_monitor` belongs to every program that
/// could name one; the import selects no names, so it loads the declarations
/// without binding anything. A standard-library module checked directly is
/// the prelude's own floor and loads nothing: `std.link_monitor` would import
/// itself, and `std.failure` would close a cycle through it. Stream's ordinary
/// source methods also belong to every program, including programs that obtain
/// a Stream from an actor without importing std.stream. Loading it through the
/// normal resolver retains its transitive codec declarations and identities.
fn inject_prelude_module_loads(items: &mut Vec<Spanned<Item>>, input: &Path) {
    if canonical_direct_stdlib_module_for_source(input).is_some() {
        return;
    }
    for leaf in hew_types::stdlib_authority::IMPLICIT_PRELUDE_MODULE_LOADS {
        let path = ["std", leaf];
        let already_imported = items.iter().any(
            |(item, _)| matches!(item, Item::Import(decl) if import_segments(&decl.path) == path),
        );
        if already_imported {
            continue;
        }
        items.push((
            Item::Import(ImportDecl {
                path: hew_parser::ast::Path::from_spellings(&path),
                spec: Some(hew_parser::ast::ImportSpec::Names(Vec::new())),
                selection_trailing_comma: false,
                module_alias: None,
                file_path: None,
                resolved_items: None,
                resolved_item_source_paths: Vec::new(),
                resolved_source_paths: Vec::new(),
            }),
            0..0,
        ));
    }
}

fn source_contains_regex_literal(source: &str) -> bool {
    hew_lexer::Lexer::new(source)
        .any(|(token, _)| matches!(token, hew_lexer::Token::RegexLiteral(_)))
}

/// A quoted file's identity is relative to the root source directory. Parent
/// steps and exceptional filesystem components occupy disjoint namespaces,
/// while ordinary Hew file and directory names remain readable.
fn module_id_from_file(source_dir: &Path, canonical_path: &Path) -> hew_parser::module::ModulePath {
    use std::path::Component;

    let base: Vec<_> = source_dir.components().collect();
    let target: Vec<_> = canonical_path.components().collect();
    // Compare the full filename: stripping its extension here would confuse
    // a sibling `app.hew` with the root directory named `app`.
    let shared = base.iter().zip(&target).take_while(|(a, b)| a == b).count();
    let mut segments: Vec<String> = if shared == 0 {
        Vec::new()
    } else {
        base[shared..]
            .iter()
            .filter(|component| matches!(component, Component::Normal(_)))
            .map(|_| "super".to_string())
            .collect()
    };
    for (index, component) in target.iter().enumerate().skip(shared) {
        let segment = match component {
            Component::Normal(name) if index + 1 == target.len() => {
                if canonical_path
                    .extension()
                    .is_some_and(|extension| extension == "hew")
                {
                    file_module_segment(canonical_path.file_stem().expect("source filename"))
                } else {
                    escaped_file_module_segment("source", name)
                }
            }
            Component::Normal(name) => file_module_segment(name),
            Component::Prefix(prefix) => escaped_file_module_segment("volume", prefix.as_os_str()),
            Component::RootDir => "__hew_file_root".to_string(),
            Component::ParentDir => "super".to_string(),
            Component::CurDir => continue,
        };
        segments.push(segment);
    }
    hew_parser::module::ModulePath::new(segments)
}

fn file_module_segment(component: &std::ffi::OsStr) -> String {
    if let Some(name) = component.to_str() {
        let mut tokens = hew_lexer::Lexer::new(name);
        if name != "super"
            && !name.starts_with("__hew_file_")
            && matches!(tokens.next(), Some((hew_lexer::Token::Identifier(_), span)) if span.start == 0 && span.end == name.len())
            && tokens.next().is_none()
        {
            return name.to_string();
        }
    }
    escaped_file_module_segment("name", component)
}

fn escaped_file_module_segment(kind: &str, component: &std::ffi::OsStr) -> String {
    use std::fmt::Write;

    let mut segment = format!("__hew_file_{kind}_");
    for byte in component.as_encoded_bytes() {
        write!(segment, "{byte:02x}").expect("writing to a String cannot fail");
    }
    segment
}

/// The standard-library roots a run resolves `std.*` from, in the spelling
/// resolved sources are compared in.
fn stdlib_roots(module_search_paths: Option<&[PathBuf]>) -> Vec<PathBuf> {
    module_search_paths
        .map_or_else(
            hew_types::module_registry::stdlib_search_paths,
            <[PathBuf]>::to_vec,
        )
        .into_iter()
        .map(|root| compile_time::normalize(&root.canonicalize().unwrap_or(root)))
        .collect()
}

/// The module path a `[package] name` spells. A package name is a dotted
/// module path (older manifests separate segments with `::`); the manifest is
/// the one place it is read as text.
fn package_module_path(name: &str) -> ModulePath {
    ModulePath::new(
        name.split("::")
            .flat_map(|part| part.split('.'))
            .filter(|segment| !segment.is_empty()),
    )
}

/// The anchor of `file` (spec 3.5.1): the std root for a file under
/// `<std root>/std/`, otherwise the nearest manifest if it declares `[package]`.
/// A virtual workspace is a search boundary, not a package anchor. Rooted
/// in-memory source identities can name a supplied package without a host
/// filesystem; loose relative buffers have no package of their own.
fn anchor_for(
    file: &Path,
    std_roots: &[PathBuf],
    documents: &SourceSet,
) -> Result<hew_types::module_registry::ModuleAnchor, FrontendFailure> {
    use hew_types::module_registry::ModuleAnchor;
    if let Some(anchor) = hew_types::module_registry::std_anchor_for(file, std_roots) {
        return Ok(anchor);
    }
    let Some(dir) = file
        .parent()
        .filter(|_| file.is_absolute() || (!documents.filesystem && file.has_root()))
        .and_then(|dir| {
            dir.ancestors()
                .find(|dir| documents.is_file(&dir.join("hew.toml")))
        })
    else {
        return Ok(ModuleAnchor::Loose);
    };
    let Some(package) = load_manifest_from(dir, documents)?.and_then(|manifest| manifest.package)
    else {
        return Ok(ModuleAnchor::Loose);
    };
    Ok(ModuleAnchor::Package {
        dir: dir.to_path_buf(),
        name: Some(package_module_path(&package.name)),
    })
}

/// The anchor imports written in `file` resolve against. A file outside any
/// package compiled with an explicit project (`--project-dir`, an editor
/// workspace) imports as a member of that project's package.
fn importer_anchor(
    file: &Path,
    ctx: &ImportResolutionContext<'_>,
    std_roots: &[PathBuf],
) -> Result<hew_types::module_registry::ModuleAnchor, FrontendFailure> {
    use hew_types::module_registry::ModuleAnchor;
    match anchor_for(file, std_roots, ctx.documents)? {
        ModuleAnchor::Loose => Ok(match ctx.package_name {
            Some(name) => ModuleAnchor::Package {
                dir: ctx.project_dir.to_path_buf(),
                name: Some(package_module_path(name)),
            },
            None => ModuleAnchor::Loose,
        }),
        anchor => Ok(anchor),
    }
}

/// The module `file` belongs to (spec 3.5.1), with directories listed from
/// the filesystem and the open documents together.
fn membership_of(
    file: &Path,
    std_roots: &[PathBuf],
    documents: &SourceSet,
) -> Result<hew_types::module_registry::ModuleMembership, FrontendFailure> {
    let anchor = anchor_for(file, std_roots, documents)?;
    let list_dir = |dir: &Path| {
        let mut files = if documents.filesystem {
            hew_types::module_registry::filesystem_dir_listing(dir)
        } else {
            Vec::new()
        };
        for path in documents.files_in(dir) {
            if !files.contains(&path) {
                files.push(path);
            }
        }
        files
    };
    Ok(hew_types::module_registry::module_membership(
        &anchor, file, &list_dir,
    ))
}

/// Which module the source file at `path` belongs to: the one answer the
/// import resolver, `hew check`, the language server, `hew test` and
/// migration share (spec 3.5.1). `None` when the file's manifest is invalid;
/// a frontend run over it reports why.
#[must_use]
pub fn module_membership(
    path: &Path,
    options: &FrontendOptions,
) -> Option<hew_types::module_registry::ModuleMembership> {
    let path = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
    let std_roots = stdlib_roots(options.module_search_paths.as_deref());
    membership_of(&path, &std_roots, &options.documents).ok()
}

/// The source file a module import written in `importer` names, from the
/// import resolver's own search roots; `None` when nothing, or more than one
/// file, answers. Editor navigation follows imports through this rather than
/// a search of its own.
#[must_use]
pub fn resolve_module_import(
    importer: &Path,
    path: &hew_parser::ast::Path,
    options: &FrontendOptions,
) -> Option<PathBuf> {
    if path.segments.is_empty() {
        return None;
    }
    let importer = importer
        .canonicalize()
        .unwrap_or_else(|_| importer.to_path_buf());
    let project_dir = options.project_dir.clone().unwrap_or_else(|| {
        let dir = importer.parent().unwrap_or(Path::new("."));
        dir.ancestors()
            .find(|dir| options.documents.is_file(&dir.join("hew.toml")))
            .unwrap_or(dir)
            .to_path_buf()
    });
    let (manifest_deps, package_name) =
        load_manifest_metadata_from(&project_dir, &options.documents).ok()?;
    let locked_versions = if options.ignore_lockfile {
        None
    } else {
        load_lockfile_from(&project_dir, &options.documents).ok()?
    };
    let ctx = ImportResolutionContext {
        in_progress_imports: HashSet::new(),
        resolved_imports: HashMap::new(),
        manifest_deps: manifest_deps.as_deref(),
        extra_pkg_path: options.pkg_path.as_deref(),
        locked_versions: locked_versions.as_deref(),
        package_name: package_name.as_deref(),
        project_dir: &project_dir,
        module_search_paths: options.module_search_paths.as_deref(),
        documents: &options.documents,
    };
    let std_roots = stdlib_roots(ctx.module_search_paths);
    let anchor = anchor_for(&importer, &std_roots, &options.documents).ok()?;
    let mut resolved = module_candidates(path, &importer, &anchor, &ctx, &std_roots)
        .candidates
        .iter()
        .filter_map(|entry| {
            resolve_module_candidate(&ctx, &std_roots, entry)
                .ok()
                .flatten()
        })
        .collect::<Vec<_>>();
    resolved.sort();
    resolved.dedup();
    let [found] = resolved.as_slice() else {
        return None;
    };
    Some(found.clone())
}

/// How a requested file becomes the root of a frontend run.
#[derive(Clone, Copy, PartialEq, Eq)]
enum RootSelection {
    /// A directory-module entry or peer is checked as its whole module.
    Module,
    /// The file is the program root as written. A build keeps it: its root
    /// is where `main` and the compiled entry points are selected.
    AsWritten,
}

/// The label of the root that checks a directory module through an import.
/// It names no source, so it can never be the module's entry or a peer.
const DIRECTORY_MODULE_ROOT_LABEL: &str = "(directory module)";

/// Check the directory module whose entry is `entry` from a root that holds
/// nothing but a file import of that entry.
///
/// The import resolver assembles the entry and every peer into one module and
/// attributes each item to its own file, so diagnostics, deep checks and
/// migration facts are the ones any importer of the module gets. The root is
/// anchored in the module's directory, so project discovery and relative
/// imports behave as they do for the requested file. Root-only lints (unused
/// private items) do not run on an imported module.
fn run_directory_module_frontend(
    entry: &Path,
    input: &str,
    options: &FrontendOptions,
) -> DocumentFrontendState {
    let label = entry
        .with_file_name(DIRECTORY_MODULE_ROOT_LABEL)
        .display()
        .to_string();
    let empty = hew_parser::parse("");
    let mut state = DocumentFrontendState {
        source: Arc::from(""),
        program: empty.program,
        parse_result: None,
        diagnostics: Vec::new(),
        typecheck_result: None,
        stopped: None,
    };
    // The requested document keeps its own text and parse for the host; an
    // open buffer stands in for its file while the module is assembled.
    let source = match read_source(&options.documents, Path::new(input)) {
        Ok(source) => source,
        Err(error) => {
            return state.stop(FrontendFailure::message_only(format!(
                "Error: cannot read {input}: {error}"
            )));
        }
    };
    state.parse_result = Some(hew_parser::parse(&source));
    state.source = source;
    let project = match load_project_context(&label, Some(options), Some("")) {
        Ok(project) => project,
        Err(failure) => return state.stop(failure),
    };
    let Some(entry_name) = entry.file_name().and_then(|name| name.to_str()) else {
        return state.stop(FrontendFailure::message_only(format!(
            "Error: directory module entry {} has no file name",
            entry.display()
        )));
    };
    if let Some(module) = canonical_direct_stdlib_module_for_source(entry) {
        state.program.items.push((
            Item::Import(ImportDecl {
                path: hew_parser::ast::Path::from_spellings(
                    &module
                        .segments
                        .iter()
                        .map(|part| part.as_str())
                        .collect::<Vec<_>>(),
                ),
                spec: None,
                selection_trailing_comma: false,
                module_alias: None,
                file_path: None,
                resolved_items: None,
                resolved_item_source_paths: Vec::new(),
                resolved_source_paths: Vec::new(),
            }),
            0..0,
        ));
    } else {
        state
            .program
            .items
            .push(file_import(entry_name.to_string()));
    }
    run_frontend_after_parse(state, &project, &label, options, None)
}

/// The shape a dotted import path was turned into a candidate file with: the
/// directory form `a/b/b.hew` or the flat form `a/b.hew`. Which one a module
/// resolved through is the only thing that separates the entry-file spelling of
/// a directory module from a nested module that repeats its own name, so the
/// resolver carries the form rather than reading it back off the path.
#[derive(Clone, Copy, PartialEq, Eq)]
enum CandidateForm {
    Directory,
    Flat,
}

/// The spelled segments of a module import path. Mapping a module path onto
/// source files is the one place a segment is needed as text.
fn import_segments(path: &hew_parser::ast::Path) -> Vec<&'static str> {
    path.segments
        .iter()
        .map(|(segment, _)| segment.name.as_str())
        .collect()
}

fn canonical_direct_stdlib_module_for_source(
    source_file: &Path,
) -> Option<hew_parser::module::ModulePath> {
    hew_types::module_registry::canonical_stdlib_module_for_source(source_file)
}

/// Render a module-graph [`CycleError`](hew_parser::module::CycleError) into a
/// positioned diagnostic: the first edge on the cycle path becomes the
/// diagnostic's primary location, every remaining edge becomes a note in path
/// order (each pointing into the module that declares that import), and a
/// help line steers the fix.
///
/// A cycle where every module's entry file lives in the same directory is the
/// directory-module shape described in spec 3.5.1 — the fix is to promote
/// that directory to a directory module rather than importing between its
/// files. That needs the directory to lie strictly below a package or std
/// anchor: the anchor directory itself holds otherwise unrelated top-level
/// modules, and a loose directory first needs a package. Otherwise the fix is
/// a shared module both sides import.
///
/// Falls back to the bare chain message (former behaviour) if a cycle member
/// is missing from `graph` or its source file cannot be re-read; both should
/// be unreachable since every cycle member was inserted into `graph` before
/// `compute_topo_order` ran and its source was just parsed.
fn cycle_error_to_frontend_failure(
    graph: &hew_parser::module::ModuleGraph,
    cycle_err: &hew_parser::module::CycleError,
    std_roots: &[PathBuf],
    documents: &SourceSet,
) -> FrontendFailure {
    let chain = cycle_err.to_string();
    let edge_count = cycle_err.import_spans.len();

    let mut locations: Vec<(PathBuf, Arc<str>, Range<usize>, String)> =
        Vec::with_capacity(edge_count);
    for i in 0..edge_count {
        let from_module = &cycle_err.cycle[i];
        let to_module = &cycle_err.cycle[i + 1];
        let Some(source_path) = graph
            .modules
            .get(from_module)
            .and_then(|module| module.source_paths.first())
        else {
            return FrontendFailure::message_only(chain);
        };
        let Ok(source) = read_source(documents, source_path) else {
            return FrontendFailure::message_only(chain);
        };
        let label = match (i == 0, i + 1 == edge_count) {
            (true, true) => format!(
                "import cycle: `{from_module}` imports `{to_module}`, closing the cycle on itself"
            ),
            (true, false) => format!("import cycle: `{from_module}` imports `{to_module}` here"),
            (false, true) => {
                format!("`{from_module}` imports `{to_module}` here, closing the cycle")
            }
            (false, false) => format!("`{from_module}` imports `{to_module}` here"),
        };
        locations.push((
            source_path.clone(),
            source,
            cycle_err.import_spans[i].clone(),
            label,
        ));
    }

    let shared_dir = locations[0].0.parent();
    let same_directory = shared_dir.is_some()
        && locations
            .windows(2)
            .all(|pair| pair[0].0.parent() == pair[1].0.parent());
    let anchor = anchor_for(&locations[0].0, std_roots, documents)
        .unwrap_or(hew_types::module_registry::ModuleAnchor::Loose);
    let below_anchor = anchor.dir().is_some_and(|dir| {
        shared_dir.is_some_and(|shared| shared != dir && shared.starts_with(dir))
    });
    let is_loose = matches!(anchor, hew_types::module_registry::ModuleAnchor::Loose);
    let help = if same_directory && (below_anchor || is_loose) {
        let dir_name = locations[0]
            .0
            .parent()
            .and_then(Path::file_name)
            .and_then(|name| name.to_str())
            .unwrap_or("<dir>");
        let package = if is_loose {
            "; directory modules belong to a package, so run `hew init` first"
        } else {
            ""
        };
        format!(
            "these modules share one directory; make `{dir_name}/{dir_name}.hew` the entry and \
             let the others be peers (spec 3.5.1), then drop the imports between them{package}"
        )
    } else {
        "move the shared declarations into a module both sides import".to_string()
    };

    let (first_path, first_source, first_span, first_label) = locations[0].clone();
    let notes = locations[1..]
        .iter()
        .map(|(path, source, span, label)| FrontendMessageNote {
            message: label.clone(),
            span: span.clone(),
            source: Arc::clone(source),
            filename: path.display().to_string(),
        })
        .collect();

    FrontendFailure::coded_message_with_notes(
        "E_IMPORT_CYCLE",
        first_label,
        first_span,
        &first_source,
        &first_path.display().to_string(),
        notes,
        vec![help],
    )
}

fn rewrite_direct_stdlib_module_root(
    module_graph: &mut hew_parser::module::ModuleGraph,
    items: &mut Vec<Spanned<Item>>,
    source_file: &Path,
    std_roots: &[PathBuf],
    documents: &SourceSet,
) -> Result<(), FrontendFailure> {
    use hew_parser::module::Module;

    let Some(stdlib_id) = canonical_direct_stdlib_module_for_source(source_file) else {
        return Ok(());
    };

    let original_root = module_graph.root.clone();
    let Some(mut stdlib_module) = module_graph.modules.remove(&original_root) else {
        return Ok(());
    };

    stdlib_module.id = stdlib_id.clone();
    module_graph.root = ModulePath::root();
    module_graph.modules.insert(stdlib_id, stdlib_module);
    module_graph
        .add_module(Module {
            id: module_graph.root.clone(),
            items: Vec::new(),
            imports: Vec::new(),
            source_paths: Vec::new(),
            doc: None,
        })
        .expect("synthetic floor-check root is unique");
    module_graph.compute_topo_order().map_err(|cycle_err| {
        cycle_error_to_frontend_failure(module_graph, &cycle_err, std_roots, documents)
    })?;
    items.clear();

    Ok(())
}

fn build_module_graph_with_diagnostics(
    source_file: &Path,
    items: &mut Vec<Spanned<Item>>,
    module_doc: Option<String>,
    ctx: &mut ImportResolutionContext<'_>,
    diagnostics: &mut Vec<FrontendDiagnostic>,
) -> Result<hew_parser::module::ModuleGraph, FrontendFailure> {
    use hew_parser::module::{Module, ModuleGraph};

    let input_canonical =
        resolve_candidate(ctx.documents, source_file).unwrap_or_else(|| source_file.to_path_buf());
    let source_dir = input_canonical.parent().unwrap_or(Path::new("."));

    ctx.in_progress_imports.insert(input_canonical.clone());
    let resolve_result = resolve_file_imports_internal(&input_canonical, items, ctx, diagnostics);
    ctx.in_progress_imports.remove(&input_canonical);
    resolve_result?;

    let root_id = module_id_from_file(source_dir, &input_canonical);
    let mut graph = ModuleGraph::new(root_id.clone());
    let mut seen_ids: HashSet<ModulePath> = HashSet::from([root_id.clone()]);

    let root_imports = extract_module_info(
        items,
        &input_canonical,
        source_dir,
        &input_canonical,
        &root_id,
        ctx.documents,
        &mut graph,
        &mut seen_ids,
    );

    let root_module = Module {
        id: root_id,
        items: items.clone(),
        imports: root_imports,
        source_paths: vec![input_canonical.clone()],
        doc: module_doc,
    };
    graph
        .add_module(root_module)
        .expect("root module id is unique");

    if let Err(cycle_err) = graph.compute_topo_order() {
        return Err(cycle_error_to_frontend_failure(
            &graph,
            &cycle_err,
            &stdlib_roots(ctx.module_search_paths),
            ctx.documents,
        ));
    }

    add_prelude_std_modules(&mut graph, |name| {
        resolve_prelude_std_source(ctx, name).map(|(path, source)| (Some(path), source))
    })?;

    rewrite_direct_stdlib_module_root(
        &mut graph,
        items,
        &input_canonical,
        &stdlib_roots(ctx.module_search_paths),
        ctx.documents,
    )?;

    // Canonical module IDs may share a final component. Reject only when two
    // whole-module imports in the SAME source scope publish the same surface
    // binding for different canonical paths. Distinct module aliases are
    // unambiguous; named/glob symbol bindings remain checker-owned.
    if let Err(msg) = check_ambiguous_module_import_bindings(&graph) {
        return Err(FrontendFailure::message_only(msg));
    }

    // Reject a single module declaring two actors with one name.  Cross-module
    // duplicates are LEGAL: actor identity is the qualified (defining-module,
    // name) pair end-to-end — the checker emits `bank.Account`'s own
    // actor-handle type, MIR
    // layouts key on the dotted name, and native symbols mangle through
    // `bank$Account` — so `spawn bank.Account { ... }` and `spawn
    // store.Account(...)` bind their own handlers/state/drop glue.  Within one
    // module there is no qualifier left to tell two same-named actors apart,
    // so that case stays a hard error.
    if let Err(msg) = check_duplicate_actor_layout_names(&graph) {
        return Err(FrontendFailure::message_only(msg));
    }

    Ok(graph)
}

/// Give a single-source program, parsed without import resolution, the same
/// standard-library modules [`build_module_graph`] adds to every program, so
/// an editor analysis lowers builtin-type methods exactly as a build does. It
/// has no module search path, so the sources are the ones compiled in.
///
/// # Panics
///
/// Never in practice: the root module is added to a freshly created graph,
/// and the compiled-in prelude sources parse.
pub fn attach_prelude_std_modules(program: &mut Program) {
    use hew_parser::module::{Module, ModuleGraph};
    let graph = program.module_graph.get_or_insert_with(|| {
        let root = ModulePath::root();
        let mut graph = ModuleGraph::new(root.clone());
        graph
            .add_module(Module {
                id: root.clone(),
                items: program.items.clone(),
                imports: Vec::new(),
                source_paths: Vec::new(),
                doc: program.module_doc.clone(),
            })
            .expect("a fresh graph has no root module");
        graph.topo_order.push(root);
        graph
    });
    add_prelude_std_modules(graph, |name| {
        let (_, source) = hew_types::stdlib_authority::IMPLICIT_PRELUDE_STD_SOURCES
            .iter()
            .find(|(leaf, _)| *leaf == name)
            .expect("every prelude module has a compiled-in source");
        Ok((None, Arc::from(*source)))
    })
    .expect("the compiled-in prelude sources parse");
}

/// Methods on builtin types are always available, like `.len()`: the
/// standard-library modules that own them join every program as ordinary
/// modules unless the program already imports them. A filesystem-selected
/// builtins source enters the graph in full, allowing the checker and HIR to
/// share its declaration occurrences. When analysis uses compiled-in sources,
/// the checker's embedded builtin registration already owns those declarations.
fn add_prelude_std_modules(
    graph: &mut hew_parser::module::ModuleGraph,
    mut source_for: impl FnMut(&str) -> Result<(Option<PathBuf>, Arc<str>), FrontendFailure>,
) -> Result<(), FrontendFailure> {
    use hew_parser::module::Module;
    for (name, _) in hew_types::stdlib_authority::IMPLICIT_PRELUDE_STD_SOURCES {
        let id = ModulePath::new(["std", name]);
        if graph.modules.contains_key(&id) {
            continue;
        }
        let (path, source) = source_for(name)?;
        if name == "builtins" && path.is_none() {
            continue;
        }
        let filename = path
            .as_ref()
            .map_or_else(|| format!("std/{name}.hew"), |path| display_path(path));
        let parsed = hew_parser::parse(&source);
        if parsed
            .errors
            .iter()
            .any(|error| error.severity == hew_parser::Severity::Error)
        {
            return Err(FrontendFailure::new(
                format!("Error: the standard library source {filename} does not parse"),
                parsed
                    .errors
                    .into_iter()
                    .map(|error| FrontendDiagnostic::parse(Arc::clone(&source), &filename, error))
                    .collect(),
            ));
        }
        graph
            .add_module(Module {
                id: id.clone(),
                items: parsed.program.items,
                imports: Vec::new(),
                source_paths: path.into_iter().collect(),
                doc: None,
            })
            .expect("prelude module absence was checked");
        graph.topo_order.push(id);
    }
    Ok(())
}

/// Resolve the prelude source `std/<name>.hew` from the standard-library
/// root a `std` import uses: the configured search path, or
/// [`hew_types::module_registry::stdlib_search_paths`]. The module carries the
/// path its diagnostics belong to.
fn resolve_prelude_std_source(
    ctx: &ImportResolutionContext<'_>,
    name: &str,
) -> Result<(PathBuf, Arc<str>), FrontendFailure> {
    let search_paths = ctx.module_search_paths.map_or_else(
        hew_types::module_registry::stdlib_search_paths,
        <[PathBuf]>::to_vec,
    );
    search_paths
        .into_iter()
        .find_map(|root| {
            let candidate = root.join("std").join(format!("{name}.hew"));
            let path = resolve_candidate(ctx.documents, &candidate)?;
            let source = read_source(ctx.documents, &path).ok()?;
            Some((path, source))
        })
        .ok_or_else(|| {
            let tried = if ctx.module_search_paths.is_some() {
                String::new()
            } else {
                let candidates = hew_types::module_registry::compiler_stdlib_root_candidates()
                    .iter()
                    .map(|root| display_path(&root.join("std")))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!(" (tried: {candidates})")
            };
            FrontendFailure::message_only(format!(
                "Error: std not found: `std/{name}.hew` is not in the toolchain's standard \
                 library{tried}; set HEW_STD to a std/ directory"
            ))
        })
}

fn check_ambiguous_module_import_bindings(
    graph: &hew_parser::module::ModuleGraph,
) -> Result<(), String> {
    for (owner_id, module) in &graph.modules {
        let mut seen: HashMap<String, String> = HashMap::new();
        for (item, _) in &module.items {
            let Item::Import(import) = item else {
                continue;
            };
            if import.path.segments.is_empty() || import.spec.is_some() {
                continue;
            }
            let source = import_segments(&import.path).join(".");
            let binding = import
                .module_alias
                .or_else(|| import.path.last())
                .expect("non-file module imports have a path");
            if let Some(existing) = seen.insert(binding.to_string(), source.clone()) {
                if existing != source {
                    return Err(format!(
                        "Error: module `{}` imports both `{existing}` and `{source}` \
                         under the ambiguous binding `{binding}`. \
                         Give one import a distinct module alias.",
                        owner_id.to_string().replace("::", ".")
                    ));
                }
            }
        }
    }
    Ok(())
}

/// Reject a single module (or the root program) declaring two actors with
/// the same name.
///
/// Actor identity is the qualified `(defining-module, name)` pair, so
/// same-named actors from DIFFERENT modules are legal and keep distinct
/// layouts, handle types, and native symbols.  Within one module the
/// qualified identities collide — `bank.Account` twice — and no spawn
/// spelling could tell them apart, so that shape stays a hard error.
fn check_duplicate_actor_layout_names(
    graph: &hew_parser::module::ModuleGraph,
) -> Result<(), String> {
    for mod_id in &graph.topo_order {
        let Some(module) = graph.modules.get(mod_id) else {
            continue;
        };
        let mut seen: HashSet<&str> = HashSet::new();
        for (item, _) in &module.items {
            let Item::Actor(actor) = item else { continue };
            if !seen.insert(actor.name.name.as_str()) {
                let owner = describe_actor_module(mod_id, graph);
                return Err(format!(
                    "Error: {owner} declares two actors named `{}`; the \
                     qualified actor identity is (module, name), so two \
                     declarations in one module cannot be told apart. Rename \
                     one of the actors.",
                    actor.name
                ));
            }
        }
    }
    Ok(())
}

/// Render a module id for the duplicate-actor diagnostic, naming the root
/// program explicitly instead of the bare `(root)` placeholder.
fn describe_actor_module(
    id: &hew_parser::module::ModulePath,
    graph: &hew_parser::module::ModuleGraph,
) -> String {
    if *id == graph.root {
        "the root program".to_string()
    } else {
        format!("module `{id}`")
    }
}

/// Resolve imports and build a module graph rooted at `source_file`.
///
/// # Errors
///
/// Returns [`FrontendFailure`] when import resolution or cycle detection fails.
pub fn build_module_graph(
    source_file: &Path,
    items: &mut Vec<Spanned<Item>>,
    module_doc: Option<String>,
    ctx: &mut ImportResolutionContext<'_>,
) -> Result<hew_parser::module::ModuleGraph, FrontendFailure> {
    let mut diagnostics = Vec::new();
    build_module_graph_with_diagnostics(source_file, items, module_doc, ctx, &mut diagnostics)
}

/// The graph node already assembled from `source`, if the walk reached that
/// file under an earlier spelling. The two spellings arrive through different
/// candidate roots, but import resolution records every resolved source path
/// canonically, so the paths compare directly.
fn graph_module_for_source(
    graph: &hew_parser::module::ModuleGraph,
    source: &Path,
) -> Option<hew_parser::module::ModulePath> {
    graph
        .modules
        .iter()
        .find(|(_, module)| module.source_paths.first().map(PathBuf::as_path) == Some(source))
        .map(|(module_id, _)| module_id.clone())
}

#[expect(
    clippy::too_many_arguments,
    reason = "one module-graph frame: the source being walked, its directory, \
              the root it belongs to, the open documents, and the graph and \
              seen-id state the walk threads; grouping them would hide which \
              of the three paths each argument comes from"
)]
fn extract_module_info(
    items: &[Spanned<Item>],
    current_source: &Path,
    source_dir: &Path,
    root_source: &Path,
    root_id: &hew_parser::module::ModulePath,
    documents: &SourceSet,
    graph: &mut hew_parser::module::ModuleGraph,
    seen_ids: &mut HashSet<hew_parser::module::ModulePath>,
) -> Vec<hew_parser::module::ModuleImport> {
    use hew_parser::module::{Module, ModuleImport};

    let mut imports = Vec::new();

    for (item, span) in items {
        let Item::Import(decl) = item else { continue };

        let (module_id, first_source_path) = if !decl.path.segments.is_empty() {
            // One source is one module, however the import spelled it: a
            // package-qualified `probe.lib` and a directory-relative `lib`
            // reach the same file, and a second graph node would have the
            // checker register those declarations twice under two owners.
            let existing = decl
                .resolved_source_paths
                .first()
                .and_then(|source| graph_module_for_source(graph, source));
            let module_id = existing.unwrap_or_else(|| {
                hew_types::module_registry::canonical_source_module_identity(
                    &ModulePath::new(import_segments(&decl.path)),
                    &decl.resolved_source_paths,
                )
            });
            (module_id, None)
        } else if let Some(file_path) = &decl.file_path {
            let resolved = current_source
                .parent()
                .unwrap_or(source_dir)
                .join(file_path);
            let canonical = resolve_candidate(documents, &resolved).unwrap_or(resolved);
            let module_id = if canonical == root_source {
                root_id.clone()
            } else {
                module_id_from_file(source_dir, &canonical)
            };
            (module_id, Some(canonical))
        } else {
            continue;
        };

        imports.push(ModuleImport {
            target: module_id.clone(),
            spec: decl.spec.clone(),
            span: span.clone(),
        });

        if seen_ids.insert(module_id.clone()) {
            if let Some(resolved) = &decl.resolved_items {
                let child_source = first_source_path.as_deref().unwrap_or(current_source);
                let child_imports = extract_module_info(
                    resolved,
                    child_source,
                    source_dir,
                    root_source,
                    root_id,
                    documents,
                    graph,
                    seen_ids,
                );
                let source_paths = if decl.resolved_source_paths.is_empty() {
                    first_source_path.into_iter().collect()
                } else {
                    decl.resolved_source_paths.clone()
                };
                // Per-item file attribution for directory-assembled modules:
                // `resolved_item_source_paths` is built parallel to the
                // resolved items, so record it only when that parallelism
                // holds (an absent entry means "first source path").
                if decl.resolved_item_source_paths.len() == resolved.len() {
                    graph
                        .item_sources
                        .insert(module_id.dotted(), decl.resolved_item_source_paths.clone());
                }
                let module = Module {
                    id: module_id,
                    items: resolved.as_ref().clone(),
                    imports: child_imports,
                    source_paths,
                    doc: None,
                };
                graph
                    .add_module(module)
                    .expect("seen_ids prevents duplicate insertion");
            }
        }
    }

    imports
}

/// Where an import candidate comes from, in search order (spec 3.5.3).
#[derive(Clone, Copy, PartialEq, Eq)]
enum CandidateRoot {
    /// The standard-library root, for `std.*` imports only.
    Std,
    /// The importing file's own package, reached through its whole name.
    Package,
    /// The importing file's directory.
    Relative,
    /// An installed or locked dependency, or a `--pkg-path` root.
    Dependency,
}

/// Every file one module import may name.
struct ModuleCandidates {
    candidates: Vec<(PathBuf, CandidateForm, CandidateRoot)>,
    locked_project_candidates: Vec<(PathBuf, LockedPackageCheck)>,
    installed_package_dir: Option<PathBuf>,
}

/// The part of an import path below the package `package`, when the path
/// starts with the package's whole dotted name.
fn package_relative<'p>(
    path: &'p hew_parser::ast::Path,
    package: &ModulePath,
) -> Option<&'p [Spanned<hew_parser::ast::Ident>]> {
    let prefix_len = package.segments.len();
    (path.segments.len() >= prefix_len
        && path
            .segments
            .iter()
            .zip(&package.segments)
            .all(|((segment, _), package_segment)| segment.name == *package_segment))
    .then(|| &path.segments[prefix_len..])
}

/// Whether the manifest declares the dependency an import path names: the
/// dependency itself or a module inside it.
fn declares_dependency(manifest_deps: Option<&[String]>, segments: &[&str]) -> bool {
    manifest_deps.is_some_and(|deps| {
        deps.iter().any(|dependency| {
            let declared = dependency
                .split("::")
                .flat_map(|part| part.split('.'))
                .collect::<Vec<_>>();
            segments.starts_with(&declared)
        })
    })
}

/// The directory form `a/b/c/c.hew` and flat form `a/b/c.hew` of a module
/// path below `base`.
fn module_forms(base: &Path, segments: &[&str]) -> [(PathBuf, CandidateForm); 2] {
    let relative = segments.iter().collect::<PathBuf>();
    let last = segments.last().copied().unwrap_or_default();
    [
        (
            base.join(&relative).join(last).with_extension("hew"),
            CandidateForm::Directory,
        ),
        (
            base.join(relative.with_extension("hew")),
            CandidateForm::Flat,
        ),
    ]
}

/// The files `import a.b.c` written in `importer` may name, in search order
/// (spec 3.5.3):
///
/// 1. `std.*` resolves from the standard-library root only.
/// 2. A path starting with the importing package's whole name resolves inside
///    that package: the bare name is its root module `<leaf>.hew`.
/// 3. Paths relative to the importing file's directory; the directory form
///    only when the importer belongs to a package.
/// 4. Dependencies, then the `--pkg-path`, `hew.` and `ecosystem.` roots.
///
/// The working directory and the std root are not searched for user modules.
fn module_candidates(
    path: &hew_parser::ast::Path,
    importer: &Path,
    anchor: &hew_types::module_registry::ModuleAnchor,
    ctx: &ImportResolutionContext<'_>,
    std_roots: &[PathBuf],
) -> ModuleCandidates {
    use hew_types::module_registry::ModuleAnchor;
    let segments = import_segments(path);
    let module_str = segments.join("::");
    let mut out = ModuleCandidates {
        candidates: Vec::new(),
        locked_project_candidates: Vec::new(),
        installed_package_dir: None,
    };
    let push = |candidates: &mut Vec<(PathBuf, CandidateForm, CandidateRoot)>,
                forms: [(PathBuf, CandidateForm); 2],
                root: CandidateRoot,
                directory: bool| {
        for (candidate, form) in forms {
            if directory || form == CandidateForm::Flat {
                candidates.push((candidate, form, root));
            }
        }
    };

    if module_str.starts_with("std::") {
        for root in std_roots {
            push(
                &mut out.candidates,
                module_forms(root, &segments),
                CandidateRoot::Std,
                true,
            );
        }
        return out;
    }

    if let ModuleAnchor::Package {
        dir,
        name: Some(package),
    } = anchor
    {
        if let Some(rest) = package_relative(path, package) {
            if rest.is_empty() {
                let leaf = package.segments.last().map_or("", |leaf| leaf.as_str());
                out.candidates.push((
                    dir.join(leaf).with_extension("hew"),
                    CandidateForm::Flat,
                    CandidateRoot::Package,
                ));
            } else {
                let rest = rest
                    .iter()
                    .map(|(segment, _)| segment.name.as_str())
                    .collect::<Vec<_>>();
                push(
                    &mut out.candidates,
                    module_forms(dir, &rest),
                    CandidateRoot::Package,
                    true,
                );
            }
        }
    }

    let importer_dir = importer.parent().unwrap_or(Path::new("."));
    push(
        &mut out.candidates,
        module_forms(importer_dir, &segments),
        CandidateRoot::Relative,
        true,
    );
    if let ModuleAnchor::Package { dir, .. } = anchor {
        if importer_dir != dir.as_path() {
            push(
                &mut out.candidates,
                module_forms(dir, &segments),
                CandidateRoot::Package,
                true,
            );
        }
    }

    dependency_candidates(&mut out, &segments, ctx);
    out
}

/// Step 4 of [`module_candidates`]: the project's installed and locked
/// packages, then the `--pkg-path`, `hew.` and `ecosystem.` roots.
fn dependency_candidates(
    out: &mut ModuleCandidates,
    segments: &[&str],
    ctx: &ImportResolutionContext<'_>,
) {
    let module_str = segments.join("::");
    let source_module = segments.join(".");
    let [(dir_rel, _), (flat_rel, _)] = module_forms(Path::new(""), segments);
    let module_dir = segments.iter().collect::<PathBuf>();
    let packages = ctx.project_dir.join(".hew/packages");
    let locked_version = ctx
        .locked_versions
        .and_then(|locked| {
            locked
                .iter()
                .find(|(name, _)| name == &module_str || name == &source_module)
        })
        .map(|(_, version)| version.as_str());
    if let Some(version) = locked_version {
        let last = segments.last().copied().unwrap_or_default();
        let versioned_rel = module_dir.join(version).join(last).with_extension("hew");
        // The version directory sits between the module and its entry file,
        // so this is a package root, never a flat file.
        out.candidates.push((
            packages.join(&versioned_rel),
            CandidateForm::Directory,
            CandidateRoot::Dependency,
        ));
        if let Some(pkg) = ctx.extra_pkg_path {
            out.candidates.push((
                pkg.join(&versioned_rel),
                CandidateForm::Directory,
                CandidateRoot::Dependency,
            ));
        }
    }
    out.candidates.push((
        packages.join(&flat_rel),
        CandidateForm::Flat,
        CandidateRoot::Dependency,
    ));
    let project_package_dir = packages.join(&module_dir);
    if declares_dependency(ctx.manifest_deps, segments) {
        out.installed_package_dir = Some(project_package_dir.clone());
    }
    let project_package_entry = packages.join(&dir_rel);
    if let Some(version) = locked_version {
        out.locked_project_candidates.push((
            project_package_entry.clone(),
            LockedPackageCheck {
                package_dir: project_package_dir,
                name: source_module.clone(),
                version: version.to_string(),
            },
        ));
    }
    out.candidates.push((
        project_package_entry,
        CandidateForm::Directory,
        CandidateRoot::Dependency,
    ));

    if let Some(pkg) = ctx.extra_pkg_path {
        out.candidates.extend(
            module_forms(pkg, segments)
                .into_iter()
                .map(|(candidate, form)| (candidate, form, CandidateRoot::Dependency)),
        );
        // The `--pkg-path` root holds `hew.` and `ecosystem.` packages below
        // their namespace, and others below their first segment.
        if segments.len() > 1 {
            out.candidates.extend(
                module_forms(pkg, &segments[1..])
                    .into_iter()
                    .map(|(candidate, form)| (candidate, form, CandidateRoot::Dependency)),
            );
        }
    }
}

/// Whether a module path repeats its last segment, the spelling that names a
/// directory module's entry file (`pkg.dir.dir` for `pkg/dir/dir.hew`).
fn repeats_last_segment(segments: &[&str]) -> bool {
    matches!(segments, [.., parent, last] if parent == last)
}

/// The file a module-import candidate names, if it exists and the spelling
/// may reach it. Beside the importer, the directory form `a/b/b.hew` names a
/// module only when that file roots one: a directory module's entry, or a
/// package directory's own `b.hew`. A loose `b/b.hew` is a file in a namespace
/// directory, never a directory module (spec 3.5.1).
fn resolve_module_candidate(
    ctx: &ImportResolutionContext<'_>,
    std_roots: &[PathBuf],
    (candidate, form, root): &(PathBuf, CandidateForm, CandidateRoot),
) -> Result<Option<PathBuf>, FrontendFailure> {
    use hew_types::module_registry::{MembershipRole, ModuleAnchor};
    let Some(canonical) = resolve_candidate(ctx.documents, candidate) else {
        return Ok(None);
    };
    if *form == CandidateForm::Flat || *root != CandidateRoot::Relative {
        return Ok(Some(canonical));
    }
    let membership = membership_of(&canonical, std_roots, ctx.documents)?;
    let roots_a_module = membership.role == MembershipRole::Entry
        || matches!(&membership.anchor, ModuleAnchor::Package { dir, .. }
            if canonical.parent() == Some(dir.as_path()));
    Ok(roots_a_module.then_some(canonical))
}

/// A positioned resolver diagnostic on the import at `span`, or a plain one
/// when the importing file can no longer be read.
fn import_failure(
    ctx: &ImportResolutionContext<'_>,
    source_file: &Path,
    span: &Range<usize>,
    code: &'static str,
    message: String,
) -> FrontendFailure {
    match read_source(ctx.documents, source_file) {
        Ok(module_source) => FrontendFailure::coded_message_at(
            code,
            message,
            span.clone(),
            &module_source,
            &source_file.display().to_string(),
        ),
        Err(_) => FrontendFailure::coded_message(code, message),
    }
}

/// Why a module import found nothing, beyond the paths it tried: a directory
/// module beside a file that belongs to no package, or a module that sits in
/// a parent directory of the importing file.
fn module_not_found_hints(
    importer: &Path,
    anchor: &hew_types::module_registry::ModuleAnchor,
    segments: &[&str],
    ctx: &ImportResolutionContext<'_>,
) -> String {
    use hew_types::module_registry::ModuleAnchor;
    let importer_dir = importer.parent().unwrap_or(Path::new("."));
    let [(directory_form, _), (flat_form, _)] = module_forms(Path::new(""), segments);
    let source_module = segments.join(".");
    if matches!(anchor, ModuleAnchor::Loose)
        && resolve_candidate(ctx.documents, &importer_dir.join(&directory_form)).is_some()
    {
        return format!(
            "\n  note: {} forms a directory module, and directory modules belong to a package\n  \
             help: run `hew init` in this directory, or import `{source_module}.{}` for the entry file alone",
            display_path(&directory_form),
            segments.last().copied().unwrap_or_default(),
        );
    }
    let Some(found) = importer_dir.ancestors().skip(1).find_map(|dir| {
        [&directory_form, &flat_form]
            .into_iter()
            .map(|form| dir.join(form))
            .find(|candidate| resolve_candidate(ctx.documents, candidate).is_some())
    }) else {
        return String::new();
    };
    let help = match anchor {
        ModuleAnchor::Package {
            dir,
            name: Some(package),
        } if found.starts_with(dir) => {
            let below = found
                .strip_prefix(dir)
                .unwrap_or(&found)
                .with_extension("");
            let mut module = package.segments.iter().map(|segment| segment.as_str()).collect::<Vec<_>>();
            let below = below
                .iter()
                .filter_map(|segment| segment.to_str())
                .collect::<Vec<_>>();
            // `dir/dir.hew` is reached as `dir`.
            let below = if repeats_last_segment(&below) {
                &below[..below.len() - 1]
            } else {
                &below[..]
            };
            if below != [module.last().copied().unwrap_or_default()] {
                module.extend(below);
            }
            format!("import `{}` names it through the package", module.join("."))
        }
        _ => "imports are relative to the importing file; a package (`hew init`) names its modules from its root".to_string(),
    };
    format!(
        "\n  note: `{}` is in a parent directory of the importing file\n  help: {help}",
        display_path(&found)
    )
}

#[expect(
    clippy::too_many_lines,
    reason = "sequential import resolution steps for file and module imports"
)]
fn resolve_file_imports_internal(
    source_file: &Path,
    items: &mut [Spanned<Item>],
    ctx: &mut ImportResolutionContext<'_>,
    diagnostics: &mut Vec<FrontendDiagnostic>,
) -> Result<(), FrontendFailure> {
    let source_dir = source_file
        .parent()
        .expect("source file should have a parent directory");

    let import_indices = items
        .iter()
        .enumerate()
        .filter_map(|(index, (item, _))| {
            if let Item::Import(decl) = item {
                if decl.file_path.is_some() || !decl.path.segments.is_empty() {
                    return Some(index);
                }
            }
            None
        })
        .collect::<Vec<_>>();
    if import_indices.is_empty() {
        return Ok(());
    }

    let std_roots = stdlib_roots(ctx.module_search_paths);
    let anchor = importer_anchor(source_file, ctx, &std_roots)?;

    for idx in &import_indices {
        let canonical = match &items[*idx].0 {
            Item::Import(decl) if decl.file_path.is_some() => {
                let file_path = decl.file_path.as_ref().expect("checked above");
                let resolved = source_dir.join(file_path);
                if let Some(canonical) = resolve_candidate(ctx.documents, &resolved) {
                    canonical
                } else {
                    return Err(FrontendFailure::message_only(format!(
                        "Error: imported file not found: {file_path} (resolved to {})",
                        resolved.display()
                    )));
                }
            }
            Item::Import(decl) if !decl.path.segments.is_empty() => {
                let segments = import_segments(&decl.path);
                let source_module = segments.join(".");
                let is_std_import = segments.join("::").starts_with("std::");
                let ModuleCandidates {
                    candidates,
                    locked_project_candidates,
                    installed_package_dir,
                } = module_candidates(&decl.path, source_file, &anchor, ctx, &std_roots);

                // Collect ALL candidates that resolve, then deduplicate by canonical path.
                // If two or more distinct canonical paths resolve, the import is ambiguous —
                // fail-closed rather than silently picking the first match.
                let mut resolved: Vec<(PathBuf, CandidateForm, CandidateRoot)> = Vec::new();
                for entry in &candidates {
                    let (candidate, form, origin) = entry;
                    if let Some(canonical) = resolve_module_candidate(ctx, &std_roots, entry)? {
                        if let Some((_, check)) = locked_project_candidates
                            .iter()
                            .find(|(locked_candidate, _)| locked_candidate == candidate)
                        {
                            verify_locked_project_package(check, ctx.documents)?;
                        }
                        // One file reached by both shapes is a directory module
                        // named by its directory: the directory candidate wins.
                        match resolved.iter_mut().find(|(path, _, _)| *path == canonical) {
                            Some((_, existing, _)) => {
                                if *form == CandidateForm::Directory {
                                    *existing = CandidateForm::Directory;
                                }
                            }
                            None => resolved.push((canonical, *form, *origin)),
                        }
                    }
                }
                resolved.sort_by(|(left, _, _), (right, _, _)| left.cmp(right));

                if resolved.len() > 1 {
                    let paths = resolved
                        .iter()
                        .map(|(path, _, _)| path.display().to_string())
                        .collect::<Vec<_>>()
                        .join("` and `");
                    return Err(FrontendFailure::coded_message(
                        "E_IMPORT_AMBIGUOUS",
                        format!(
                            "Error: import `{source_module}` is ambiguous: both `{paths}` exist.\n  Rename or remove one to resolve the ambiguity."
                        ),
                    ));
                }

                if let Some((canonical, form, origin)) = resolved.into_iter().next() {
                    use hew_types::module_registry::{MembershipRole, ModuleAnchor};
                    let membership = membership_of(&canonical, &std_roots, ctx.documents)?;
                    if origin == CandidateRoot::Dependency {
                        if let (
                            ModuleAnchor::Package { dir: importer, .. },
                            ModuleAnchor::Package {
                                dir,
                                name: Some(name),
                            },
                        ) = (&anchor, &membership.anchor)
                        {
                            let (dependencies, _) =
                                load_manifest_metadata_from(importer, ctx.documents)?;
                            if dependencies.is_some_and(|deps| deps.contains(&name.dotted())) {
                                ctx.documents
                                    .dependency_roots
                                    .lock()
                                    .expect("source dependency table poisoned")
                                    .insert(dir.clone());
                            }
                        }
                    }
                    let directory_module = || segments[..segments.len() - 1].join(".");
                    match membership.role {
                        MembershipRole::Entry
                            if form == CandidateForm::Flat && repeats_last_segment(&segments) =>
                        {
                            // A directory module is spelled by its directory,
                            // and its entry file adds no second module (spec
                            // 3.5.1). Accepting both spellings would let one
                            // compilation reach one source under two names.
                            return Err(import_failure(
                                ctx,
                                source_file,
                                &items[*idx].1,
                                "E_ENTRY_FILE_IMPORT",
                                format!(
                                    "cannot import `{source_module}`: `{}` is a directory module and its entry file is not a module of its own; import `{}` instead",
                                    directory_module(),
                                    directory_module(),
                                ),
                            ));
                        }
                        MembershipRole::Peer if segments.len() >= 2 => {
                            if matches!(membership.anchor, ModuleAnchor::Std { .. }) {
                                // The shipped std's directory peers stay
                                // importable by file (`std.net.http.codec.request`,
                                // D461); they load through the directory's
                                // entry so the module is complete however it
                                // was reached.
                                membership.entry
                            } else {
                                // A user peer has no identity of its own: it
                                // shares the directory module's namespace, so
                                // loading it alone would miss the siblings it
                                // refers to.
                                return Err(import_failure(
                                    ctx,
                                    source_file,
                                    &items[*idx].1,
                                    "E_PEER_IMPORT",
                                    format!(
                                        "cannot import `{source_module}` directly: peer files are reached through the directory module; import `{}` instead",
                                        directory_module(),
                                    ),
                                ));
                            }
                        }
                        _ => canonical,
                    }
                } else {
                    let last = *segments.last().expect("path is non-empty");
                    if let Some(package_dir) = installed_package_dir.filter(|dir| dir.is_dir()) {
                        let expected = package_dir.join(last).with_extension("hew");
                        return Err(FrontendFailure::coded_message(
                            "E_PACKAGE_ROOT_MISSING",
                            format!(
                                "Error: installed dependency `{source_module}` has no canonical root module `{last}.hew` at {}\n  hint: a library package exposes `<package-name>.hew`; create `{}` and set `[package] main = \"{last}.hew\"` (new packages get this layout from `hew init --lib {source_module}`)",
                                package_dir.display(),
                                expected.display(),
                            ),
                        ));
                    }
                    let tried = candidates
                        .iter()
                        .map(|(candidate, _, _)| display_path(candidate))
                        .collect::<Vec<_>>()
                        .join(", ");
                    let hint = if declares_dependency(ctx.manifest_deps, &segments) {
                        "\n  hint: this dependency is declared in hew.toml — run `hew install`"
                            .to_string()
                    } else if is_std_import {
                        nearest_std_module(last, &std_roots)
                            .map(|nearest| format!("\n  did you mean `{nearest}`?"))
                            .unwrap_or_default()
                    } else {
                        let hints = module_not_found_hints(source_file, &anchor, &segments, ctx);
                        if hints.is_empty() && ctx.manifest_deps.is_some() {
                            "\n  hint: add this module to [dependencies] in hew.toml".to_string()
                        } else {
                            hints
                        }
                    };
                    let message = if is_std_import && std_roots.is_empty() {
                        // No std root at all: the toolchain's std is missing,
                        // not this one module.
                        let probed = hew_types::module_registry::compiler_stdlib_root_candidates()
                            .iter()
                            .map(|root| display_path(&root.join("std")))
                            .collect::<Vec<_>>()
                            .join(", ");
                        format!(
                            "std not found: module `{source_module}` needs the toolchain's \
                             standard library (tried: {probed}); set HEW_STD to a std/ directory"
                        )
                    } else {
                        format!("module `{source_module}` not found (tried: {tried}){hint}")
                    };
                    return Err(import_failure(
                        ctx,
                        source_file,
                        &items[*idx].1,
                        "E_MODULE_NOT_FOUND",
                        message,
                    ));
                }
            }
            _ => continue,
        };

        let Some(resolved_import) =
            resolve_completed_import_internal(&canonical, ctx, &items[*idx].0, diagnostics)?
        else {
            continue;
        };

        if let Item::Import(decl) = &mut items[*idx].0 {
            decl.resolved_items = Some(resolved_import.items.clone());
            decl.resolved_item_source_paths
                .clone_from(&resolved_import.item_source_paths);
            decl.resolved_source_paths
                .clone_from(&resolved_import.source_paths);
        }
    }

    Ok(())
}

fn resolve_completed_import_internal(
    canonical: &Path,
    ctx: &mut ImportResolutionContext<'_>,
    import_item: &Item,
    diagnostics: &mut Vec<FrontendDiagnostic>,
) -> Result<Option<ResolvedImport>, FrontendFailure> {
    if let Some(cached) = ctx.resolved_imports.get(canonical) {
        return Ok(Some(cached.clone()));
    }
    if ctx.in_progress_imports.contains(canonical) {
        return Ok(None);
    }

    ctx.in_progress_imports.insert(canonical.to_path_buf());
    let resolved = build_resolved_import_internal(canonical, ctx, import_item, diagnostics);
    ctx.in_progress_imports.remove(canonical);

    match resolved {
        Ok(resolved_import) => {
            ctx.resolved_imports
                .insert(canonical.to_path_buf(), resolved_import.clone());
            Ok(Some(resolved_import))
        }
        Err(error) => Err(error),
    }
}

fn build_resolved_import_internal(
    canonical: &Path,
    ctx: &mut ImportResolutionContext<'_>,
    import_item: &Item,
    diagnostics: &mut Vec<FrontendDiagnostic>,
) -> Result<ResolvedImport, FrontendFailure> {
    let std_roots = stdlib_roots(ctx.module_search_paths);
    let membership = membership_of(canonical, &std_roots, ctx.documents)?;
    let peer_files = if membership.role == hew_types::module_registry::MembershipRole::Entry {
        membership.peers.clone()
    } else {
        Vec::new()
    };

    let mut import_items = parse_and_resolve_file_internal(canonical, ctx, diagnostics)?;
    let mut import_item_source_paths = vec![canonical.to_path_buf(); import_items.len()];
    let mut source_paths = vec![canonical.to_path_buf()];

    for peer in &peer_files {
        let peer_canonical = peer.canonicalize().unwrap_or_else(|_| peer.clone());
        let Some(peer_resolved) =
            resolve_completed_import_internal(&peer_canonical, ctx, import_item, diagnostics)?
        else {
            continue;
        };
        import_item_source_paths.extend(std::iter::repeat_n(
            peer_canonical.clone(),
            peer_resolved.items.len(),
        ));
        import_items.extend(peer_resolved.items.iter().cloned());
        source_paths.push(peer_canonical);
    }

    if !peer_files.is_empty() {
        check_duplicate_pub_names(&import_items, &import_item_source_paths, &membership)
            .map_err(FrontendFailure::message_only)?;
    }

    Ok(ResolvedImport {
        items: import_items.into(),
        item_source_paths: import_item_source_paths,
        source_paths,
    })
}

fn parse_and_resolve_file_internal(
    canonical: &Path,
    ctx: &mut ImportResolutionContext<'_>,
    diagnostics: &mut Vec<FrontendDiagnostic>,
) -> Result<Vec<Spanned<Item>>, FrontendFailure> {
    let source = read_source(ctx.documents, canonical).map_err(|e| {
        FrontendFailure::message_only(format!(
            "Error reading imported file '{}': {e}",
            canonical.display()
        ))
    })?;

    let result = ctx.documents.parse_document(canonical, &source);
    let display_path = canonical.display().to_string();
    let parse_diagnostics = result
        .errors
        .iter()
        .cloned()
        .map(|diagnostic| FrontendDiagnostic::parse(Arc::clone(&source), &display_path, diagnostic))
        .collect::<Vec<_>>();

    if result
        .errors
        .iter()
        .any(|error| error.severity == hew_parser::Severity::Error)
    {
        return Err(FrontendFailure::new(
            format!("parsing failed in imported file '{}'", canonical.display()),
            parse_diagnostics,
        ));
    }

    diagnostics.extend(parse_diagnostics);
    let mut import_items = result.program.items;
    resolve_file_imports_internal(canonical, &mut import_items, ctx, diagnostics)?;
    Ok(import_items)
}

/// Reject two `pub` declarations of one name across the files of a directory
/// module, naming both files and why they share a namespace.
fn check_duplicate_pub_names(
    items: &[Spanned<Item>],
    item_sources: &[PathBuf],
    membership: &hew_types::module_registry::ModuleMembership,
) -> Result<(), String> {
    use hew_parser::ast::Visibility;

    // Only `Visibility::Pub` items are checked here — intentionally.
    //
    // `Visibility::Package` items are scoped to the package boundary: two
    // modules within the same package can each define `package fn foo()` in
    // their own namespace without creating a global API conflict.  The
    // duplicate-name guard exists to catch clashes in the *globally-exported*
    // interface (i.e. items a downstream package could import by name), which
    // only `pub` items contribute to.
    let mut seen: Vec<(hew_parser::ast::Symbol, &Path)> = Vec::new();
    for ((item, _), source) in items.iter().zip(item_sources) {
        let name = match item {
            Item::Function(f) if f.visibility == Visibility::Pub => f.name.name,
            Item::TypeAlias(t) if t.visibility == Visibility::Pub => t.name.name,
            Item::TypeDecl(t) if t.visibility == Visibility::Pub => t.name.name,
            Item::Actor(a) if a.visibility == Visibility::Pub => a.name.name,
            Item::Trait(t) if t.visibility == Visibility::Pub => t.name.name,
            Item::Const(c) if c.visibility == Visibility::Pub => c.name.name,
            _ => continue,
        };
        if let Some((_, first)) = seen.iter().find(|(seen_name, _)| *seen_name == name) {
            // A file below the anchor is shown as its package-relative path,
            // spelled with `/` on every platform like the module path it is.
            let shown = |path: &Path| match membership
                .anchor
                .dir()
                .and_then(|dir| path.strip_prefix(dir).ok())
            {
                Some(relative) => relative
                    .iter()
                    .map(|component| component.to_string_lossy())
                    .collect::<Vec<_>>()
                    .join("/"),
                None => display_path(path),
            };
            let entry = shown(&membership.entry);
            let directory = membership.entry.parent().map_or_else(String::new, &shown);
            return Err(format!(
                "Error: duplicate pub name `{name}` in module `{}`: declared in `{}` and in `{}`\n  \
                 note: `{directory}/` is a directory module because `{entry}` exists, so its files share one namespace",
                membership.module.dotted(),
                shown(first),
                shown(source),
            ));
        }
        seen.push((name, source));
    }
    Ok(())
}

/// Intermediate state produced by the shared file-frontend driver after
/// loading, parsing, import resolution, and type-checking have all succeeded.
///
/// Current consumers:
/// - [`check_file`] — stops here; does not continue into enrichment.
/// - [`compile_file`] — continues into enrichment and codegen-metadata assembly.
/// - `lower_file_to_mir` (slice 2, v0.5 compile path) — will route through
///   [`run_file_frontend_to_typecheck`] instead of duplicating the frontend.
///
/// **Do not construct a divergent wrapper.** Frontend variants must route
/// through the shared private driver used by
/// [`run_file_frontend_to_typecheck`]. A parallel driver that duplicates load
/// → parse → import-resolution → type-check is always wrong.
#[allow(
    missing_debug_implementations,
    reason = "transient pipeline value; Debug not required by any current consumer"
)]
pub struct FileFrontendState {
    pub program: Program,
    pub diagnostics: Vec<FrontendDiagnostic>,
    pub typecheck_result: TypeCheckResult,
    pub source: Arc<str>,
}

#[allow(
    missing_debug_implementations,
    reason = "transient pipeline value; Debug not required by any current consumer"
)]
pub struct ProgramFrontendState {
    pub program: Program,
    pub diagnostics: Vec<FrontendDiagnostic>,
    pub typecheck_result: TypeCheckResult,
    pub source: Arc<str>,
}

/// Shared frontend driver for on-disk source files.
///
/// Runs load → parse → import-resolution → type-check and returns the
/// intermediate [`FileFrontendState`]. Current consumers are [`check_file`]
/// (stops here) and [`compile_file`] (continues into enrichment and
/// codegen-metadata assembly via `finish_compile`).
///
/// **Do not construct a divergent wrapper.** Frontend variants must route
/// through the private shared driver below; a parallel driver that duplicates
/// load → parse → import-resolution → type-check is always wrong.
///
/// # Errors
///
/// Returns [`FrontendFailure`] when project loading, parsing, import
/// resolution, or type-checking fails.
pub fn run_file_frontend_to_typecheck(
    input: &str,
    options: &FrontendOptions,
) -> Result<FileFrontendState, FrontendFailure> {
    run_document_frontend_from(input, None, options, RootSelection::AsWritten).into_result()
}

/// Load and expand a source file without requiring the expanded program to type-check.
///
/// # Errors
///
/// Returns the shared frontend's loading, parsing or expansion failure.
pub fn run_file_frontend_to_expansion(
    input: &str,
    options: &FrontendOptions,
) -> Result<FileFrontendState, FrontendFailure> {
    let options = FrontendOptions {
        no_typecheck: true,
        ..options.clone()
    };
    run_file_frontend_to_typecheck(input, &options)
}

/// What the shared frontend produced for one document.
///
/// The editor surfaces need the diagnostics and whatever the pipeline managed
/// to build, not a single fatal failure, so this never reports an error by
/// itself: [`Self::stopped`] carries the very [`FrontendFailure`] the fallible
/// entry points return, and the artefacts built before that point stay
/// available for hover, completion and navigation.
#[allow(
    missing_debug_implementations,
    reason = "transient pipeline value; Debug not required by any current consumer"
)]
pub struct DocumentFrontendState {
    /// The root buffer's text, from the document set or from disk.
    pub source: Arc<str>,
    /// The root buffer's parse. `None` when the host supplied an already
    /// parsed program through [`run_program_frontend_to_typecheck`].
    pub parse_result: Option<hew_parser::ParseResult>,
    /// The program after import resolution.
    pub program: Program,
    /// Every diagnostic the run produced, including those of the failure that
    /// stopped it.
    pub diagnostics: Vec<FrontendDiagnostic>,
    /// The checker output. Present whenever type-checking ran, including when
    /// it reported errors.
    pub typecheck_result: Option<TypeCheckResult>,
    /// The stage failure that ended the run, if any.
    pub stopped: Option<FrontendFailure>,
}

impl DocumentFrontendState {
    fn stop(mut self, failure: FrontendFailure) -> Self {
        let failure = merge_prior_diagnostics(std::mem::take(&mut self.diagnostics), failure);
        self.diagnostics.clone_from(&failure.diagnostics);
        self.stopped = Some(failure);
        self
    }

    fn into_result(self) -> Result<FileFrontendState, FrontendFailure> {
        if let Some(failure) = self.stopped {
            return Err(failure);
        }
        Ok(FileFrontendState {
            program: self.program,
            diagnostics: self.diagnostics,
            typecheck_result: self
                .typecheck_result
                .expect("a completed frontend run has a type-check result"),
            source: self.source,
        })
    }
}

/// Run the shared frontend over a document that may not match its file.
///
/// Same load → parse → import resolution → builtins preload → manifest
/// validation → type check as [`run_file_frontend_to_typecheck`]. `input`
/// names the document; every source read consults `options.documents` first,
/// so an open buffer checks against its saved siblings.
#[must_use]
pub fn run_document_frontend(input: &str, options: &FrontendOptions) -> DocumentFrontendState {
    run_document_frontend_from(input, None, options, RootSelection::Module)
}

/// [`run_document_frontend`] for a buffer with no file behind it.
///
/// `label` names the buffer in diagnostics and anchors module resolution.
#[must_use]
pub fn run_source_frontend(
    source: &str,
    label: &str,
    options: &FrontendOptions,
) -> DocumentFrontendState {
    run_document_frontend_from(label, Some(source), options, RootSelection::Module)
}

/// Frontend checks sharing immutable dependency analysis within one request.
///
/// Supplied overlays are frozen when the batch is created. Each call admits
/// fresh disk inputs; the dependency key includes their exact text. It resolves
/// imports using its genuine source root and forks mutable semantic state from
/// a dependency-only checkpoint when independence is proved. Uncertain root
/// shapes use the ordinary full frontend. Use a separate batch for proposed
/// source text; no cache or inference state survives this object's lifetime.
#[derive(Debug)]
pub struct SourceAnalysisBatch {
    options: FrontendOptions,
    dependencies: hew_types::check::DependencyAnalysisCache,
}

impl SourceAnalysisBatch {
    #[must_use]
    pub fn new(options: FrontendOptions) -> Self {
        Self {
            options,
            dependencies: hew_types::check::DependencyAnalysisCache::default(),
        }
    }

    /// Admit fresh disk inputs and check them against this batch's frozen overlays.
    #[must_use]
    pub fn run_source_frontend(&mut self, source: &str, label: &str) -> DocumentFrontendState {
        run_document_frontend_with_dependency_cache(
            label,
            Some(source),
            &self.options,
            RootSelection::Module,
            Some(&mut self.dependencies),
        )
    }

    /// Dependency-only checker pipelines run in this immutable batch.
    #[must_use]
    pub fn dependency_bootstraps(&self) -> usize {
        self.dependencies.bootstraps()
    }

    /// Roots using an already retained dependency checkpoint.
    #[must_use]
    pub fn dependency_cache_hits(&self) -> usize {
        self.dependencies.cache_hits()
    }

    /// Number of real roots analysed from a dependency checkpoint.
    #[must_use]
    pub fn reused_roots(&self) -> usize {
        self.dependencies.reused_roots()
    }
}

fn run_document_frontend_from(
    input: &str,
    source_override: Option<&str>,
    options: &FrontendOptions,
    roots: RootSelection,
) -> DocumentFrontendState {
    run_document_frontend_with_dependency_cache(input, source_override, options, roots, None)
}

fn run_document_frontend_with_dependency_cache(
    input: &str,
    source_override: Option<&str>,
    options: &FrontendOptions,
    roots: RootSelection,
    cache: Option<&mut hew_types::check::DependencyAnalysisCache>,
) -> DocumentFrontendState {
    let mut admitted_options = options.clone();
    if let Some(source) = source_override {
        admitted_options.documents.insert(input, source);
    }
    admitted_options.documents = admitted_options.documents.for_frontend();
    let options = &admitted_options;
    let mut state = run_admitted_document_frontend(input, source_override, options, roots, cache);
    if let Some(failure) = &mut state.stopped {
        failure.inputs = options.documents.inputs();
    }
    state
}

fn run_admitted_document_frontend(
    input: &str,
    source_override: Option<&str>,
    options: &FrontendOptions,
    roots: RootSelection,
    cache: Option<&mut hew_types::check::DependencyAnalysisCache>,
) -> DocumentFrontendState {
    if roots == RootSelection::Module {
        if let Some(membership) = module_membership(Path::new(input), options)
            .filter(|m| m.checks_as_directory_module() || m.is_package_root_module())
        {
            return run_directory_module_frontend(&membership.entry, input, options);
        }
    }
    let project = match load_project_context(input, Some(options), source_override) {
        Ok(project) => project,
        Err(failure) => {
            let empty = hew_parser::parse("");
            return DocumentFrontendState {
                source: Arc::from(""),
                program: empty.program.clone(),
                parse_result: Some(empty),
                diagnostics: Vec::new(),
                typecheck_result: None,
                stopped: None,
            }
            .stop(failure);
        }
    };

    let parse_result = options
        .documents
        .parse_document(Path::new(input), &project.source);
    let diagnostics = parse_result
        .errors
        .iter()
        .cloned()
        .map(|diagnostic| FrontendDiagnostic::parse(Arc::clone(&project.source), input, diagnostic))
        .collect::<Vec<_>>();
    let parse_failed = parse_result
        .errors
        .iter()
        .any(|error| error.severity == hew_parser::Severity::Error);
    let mut state = DocumentFrontendState {
        source: project.source.clone(),
        program: parse_result.program.clone(),
        parse_result: Some(parse_result),
        diagnostics,
        typecheck_result: None,
        stopped: None,
    };
    if parse_failed {
        return state.stop(FrontendFailure::message_only("parsing failed"));
    }

    let entry_selection = options.entry_selection;
    let companion = options.companion.as_deref();
    if let Some(companion) = companion {
        // First, as an import written at the top of the file: the test file's
        // own declarations (a trait impl) resolve against it.
        state
            .program
            .items
            .insert(0, file_import(companion.display().to_string()));
    }

    run_frontend_after_parse_with_dependency_cache(
        state,
        &project,
        input,
        options,
        entry_selection,
        cache,
    )
}

/// The frontend stages every host shares once a program exists: import
/// resolution, the builtins preload, manifest validation and type-checking.
fn run_frontend_after_parse(
    state: DocumentFrontendState,
    project: &ProjectContext,
    input: &str,
    options: &FrontendOptions,
    entry_selection: Option<hew_types::DeclarationOccurrence>,
) -> DocumentFrontendState {
    run_frontend_after_parse_with_dependency_cache(
        state,
        project,
        input,
        options,
        entry_selection,
        None,
    )
}

fn run_frontend_after_parse_with_dependency_cache(
    mut state: DocumentFrontendState,
    project: &ProjectContext,
    input: &str,
    options: &FrontendOptions,
    entry_selection: Option<hew_types::DeclarationOccurrence>,
    cache: Option<&mut hew_types::check::DependencyAnalysisCache>,
) -> DocumentFrontendState {
    if let Err(failure) = require_host_typecheck(options) {
        return state.stop(failure);
    }
    if let Err(failure) = resolve_imports_internal(
        &mut state.program,
        &project.source,
        input,
        project,
        options,
        &mut state.diagnostics,
    ) {
        return state.stop(failure);
    }

    let (typecheck_result, type_diagnostics) = typecheck_program_with_dependency_cache(
        &state.program,
        &project.source,
        input,
        options,
        entry_selection,
        cache,
    );
    state.diagnostics.extend(type_diagnostics);
    let type_check_failed = type_check_failed(&typecheck_result);
    state.typecheck_result = Some(typecheck_result);
    if type_check_failed {
        // A std source's warnings are not the user's to act on; its errors
        // stay, because they are why the check failed.
        let stdlib_roots = configured_stdlib_roots(options);
        state.diagnostics.retain(|diagnostic| {
            !is_warning_diagnostic(diagnostic)
                || !is_stdlib_owned_diagnostic(input, &stdlib_roots, diagnostic)
        });
        return state.stop(FrontendFailure::message_only("type errors found"));
    }

    if let Some(output) = state
        .typecheck_result
        .as_ref()
        .and_then(|result| result.tco.as_ref())
    {
        let refused =
            host_admission::check(&state.program, output, &project.source, input, options);
        if !refused.is_empty() {
            state.diagnostics.extend(refused);
            return state.stop(FrontendFailure::message_only(
                "host operations refused by the execution profile",
            ));
        }
    }

    let stdlib_roots = configured_stdlib_roots(options);
    retain_user_facing_diagnostics(input, &stdlib_roots, &mut state.diagnostics);
    state
}

/// Shared frontend driver for already-parsed in-memory programs.
///
/// Runs import-resolution → type-check and returns the resolved program plus
/// checker output so non-msgpack backends can lower through HIR/MIR without
/// duplicating the frontend pipeline.
///
/// # Errors
///
/// Returns [`FrontendFailure`] when manifest loading, import resolution, or
/// type-checking fails.
pub fn run_program_frontend_to_typecheck(
    program: Program,
    source: &str,
    source_label: &str,
    options: &FrontendOptions,
) -> Result<ProgramFrontendState, FrontendFailure> {
    let state = run_program_frontend(program, source, source_label, options);
    let file_state = state.into_result()?;
    let diagnostics = fail_on_warning_diagnostics(file_state.diagnostics, options)?;
    Ok(ProgramFrontendState {
        program: file_state.program,
        diagnostics,
        typecheck_result: file_state.typecheck_result,
        source: file_state.source,
    })
}

/// [`run_program_frontend_to_typecheck`] without the fatal failure, for hosts
/// that need the diagnostics and artefacts of a run that could not complete.
#[must_use]
pub fn run_program_frontend(
    program: Program,
    source: &str,
    source_label: &str,
    options: &FrontendOptions,
) -> DocumentFrontendState {
    let mut admitted_options = options.clone();
    admitted_options.documents.insert(source_label, source);
    admitted_options.documents = admitted_options.documents.for_frontend();
    let options = &admitted_options;
    let project = match project_context_for_program(source, options) {
        Ok(project) => project,
        Err(failure) => {
            return DocumentFrontendState {
                source: source.into(),
                parse_result: None,
                program,
                diagnostics: Vec::new(),
                typecheck_result: None,
                stopped: None,
            }
            .stop(failure);
        }
    };
    let state = DocumentFrontendState {
        source: source.into(),
        parse_result: None,
        program,
        diagnostics: Vec::new(),
        typecheck_result: None,
        stopped: None,
    };
    run_frontend_after_parse(state, &project, source_label, options, None)
}

/// Parse, resolve imports, and type-check a Hew source file.
///
/// A directory-module entry or peer checks its whole module (see
/// [`module_membership`]).
///
/// # Errors
///
/// Returns [`FrontendFailure`] when parsing, import resolution, or type
/// checking fails.
pub fn check_file(input: &str, options: &FrontendOptions) -> Result<CheckOutput, FrontendFailure> {
    let (output, _) = check_file_with_state(input, options)?;
    Ok(output)
}

/// Parse, resolve imports, type-check, and return both the public check output
/// and the frontend state needed by HIR/MIR-only consumers.
///
/// # Errors
///
/// Returns [`FrontendFailure`] when parsing, import resolution, type checking,
/// or `warnings_as_errors` promotion fails.
pub fn check_file_with_state(
    input: &str,
    options: &FrontendOptions,
) -> Result<(CheckOutput, FileFrontendState), FrontendFailure> {
    let state =
        run_document_frontend_from(input, None, options, RootSelection::Module).into_result()?;
    let diagnostics = fail_on_warning_diagnostics(state.diagnostics.clone(), options).map_err(
        |mut failure| {
            failure.inputs = state.typecheck_result.sources.inputs();
            failure
        },
    )?;
    let stack_hints = state
        .typecheck_result
        .tco
        .as_ref()
        .map(|tco| tco.stack_hints.clone())
        .unwrap_or_default();
    let output = CheckOutput {
        inputs: state
            .typecheck_result
            .tco
            .as_ref()
            .map(|output| output.inputs.clone())
            .unwrap_or_default(),
        diagnostics,
        stack_hints,
        source: state.source.to_string(),
    };
    Ok((output, state))
}

/// The workspace release version. Development display identities also include
/// their Git count and commit. Every crate in the workspace shares this base.
pub const COMPILER_VERSION: &str = env!("CARGO_PKG_VERSION");

/// Whether a `[package] hew` compiler range admits this compiler. The one
/// reader of that range: the frontend, `hew publish`, `hew check` and the
/// package resolver all ask it.
/// Ordinary ranges describe the compiler's feature version; explicit
/// prerelease bounds compare its full release version.
///
/// # Errors
///
/// Returns the parse error when `range` is not a semver range.
///
/// # Panics
///
/// Never: [`COMPILER_VERSION`] is the workspace's semver version.
pub fn compiler_supports(range: &str) -> Result<bool, semver::Error> {
    let range = semver::VersionReq::parse(range)?;
    let compiler =
        semver::Version::parse(COMPILER_VERSION).expect("the workspace version is semver");
    Ok(compiler_requirement_matches(&range, compiler))
}

fn compiler_requirement_matches(range: &semver::VersionReq, mut compiler: semver::Version) -> bool {
    if range.comparators.iter().all(|bound| bound.pre.is_empty()) {
        compiler.pre = semver::Prerelease::EMPTY;
    }
    range.matches(&compiler)
}

#[cfg(test)]
mod compiler_range_tests {
    #[test]
    fn feature_ranges_and_explicit_prerelease_bounds() {
        for (requirement, compiler, admitted) in [
            (">=0.9.3", "0.10.0-rc1", true),
            ("<0.10.0", "0.10.0-rc1", false),
            ("=0.9.3", "0.10.0-rc1", false),
            (">=0.10.0-rc2", "0.10.0-rc1", false),
            (">=0.10.0-rc1", "0.10.0-rc1", true),
            (">=0.10.0-rc2", "0.10.0", true),
            (">=0.9.3", "0.9.2", false),
        ] {
            let requirement = semver::VersionReq::parse(requirement).unwrap();
            let compiler = semver::Version::parse(compiler).unwrap();
            assert_eq!(
                super::compiler_requirement_matches(&requirement, compiler),
                admitted
            );
        }
    }
}

/// Hew language editions the compiler accepts. Sources in a package whose
/// `hew.toml` names an edition outside this set are rejected before parsing.
const SUPPORTED_EDITIONS: &[&str] = &["2026"];

/// Edition assumed when `hew.toml` is absent or omits the `edition` field.
const DEFAULT_EDITION: &str = "2026";

fn default_edition() -> String {
    DEFAULT_EDITION.to_string()
}

#[derive(Debug, Deserialize)]
struct PackageSection {
    name: String,
    #[serde(default)]
    version: Option<String>,
    #[serde(default = "default_edition")]
    edition: String,
    #[serde(default)]
    hew: Option<String>,
}

/// Table form of a `hew.toml` dependency: `{ version = "^1.0", path = "...",
/// features = [...], optional = true }`. The field set mirrors hew-pkg's `DepTable`
/// so the compiler parses exactly the manifests the package manager accepts.
/// Only dependency *names* (the map keys) are used by the compiler, so these
/// values are parsed for cross-tool compatibility and are otherwise unused.
#[derive(Debug, Deserialize)]
#[allow(
    dead_code,
    reason = "manifest compatibility fields are parsed but not all consumed by the compiler"
)]
struct DepTable {
    /// Absent for a path dependency, as in the package manager's table.
    #[serde(default = "any_version")]
    version: String,
    #[serde(default)]
    path: Option<String>,
    #[serde(default)]
    features: Option<Vec<String>>,
    #[serde(default)]
    optional: Option<bool>,
    #[serde(default)]
    default_features: Option<bool>,
    #[serde(default)]
    registry: Option<String>,
}

fn any_version() -> String {
    "*".to_string()
}

/// A `hew.toml` dependency value: a bare version string (`"^1.0"`) or a detailed
/// table. Untagged to match the package manager's `DepSpec` so the compiler no
/// longer rejects table/path/feature dependencies that `hew install` accepts.
#[derive(Debug, Deserialize)]
#[serde(untagged)]
#[allow(
    dead_code,
    reason = "manifest compatibility variants preserve package-manager dependency syntax"
)]
enum DepSpec {
    Version(String),
    Table(DepTable),
}

#[derive(Debug, Deserialize)]
struct TomlManifest {
    package: Option<PackageSection>,
    #[serde(default)]
    dependencies: BTreeMap<String, DepSpec>,
}

#[derive(Debug, Deserialize)]
struct HewTomlLock {
    #[serde(default)]
    package: Vec<LockedEntry>,
}

#[derive(Debug, Deserialize)]
struct LockedEntry {
    name: String,
    version: String,
}

fn load_optional_toml_from<T: DeserializeOwned>(
    path: &Path,
    sources: &SourceSet,
) -> Result<Option<T>, FrontendFailure> {
    let text = match read_source(sources, path) {
        Ok(text) => text,
        // An optional project file is absent both when the directory does not
        // hold one and when the host has no filesystem to hold it: the browser
        // compiles a buffer with no project behind it, and wasm32 reports that
        // as `Unsupported` rather than `NotFound`.
        Err(err)
            if matches!(
                err.kind(),
                std::io::ErrorKind::NotFound | std::io::ErrorKind::Unsupported
            ) =>
        {
            return Ok(None);
        }
        Err(err) => {
            return Err(FrontendFailure::message_only(format!(
                "Error: cannot read {}: {err}",
                path.display()
            )));
        }
    };
    toml::from_str(&text).map(Some).map_err(|err| {
        FrontendFailure::message_only(format!("Error: cannot parse {}: {err}", path.display()))
    })
}

#[cfg(test)]
fn load_manifest(dir: &Path) -> Result<Option<TomlManifest>, FrontendFailure> {
    load_manifest_from(dir, &EMPTY_DOCUMENTS)
}

fn load_manifest_from(
    dir: &Path,
    sources: &SourceSet,
) -> Result<Option<TomlManifest>, FrontendFailure> {
    let path = dir.join("hew.toml");
    let manifest: Option<TomlManifest> = load_optional_toml_from(&path, sources)?;
    if let Some(m) = &manifest {
        if let Some(package) = &m.package {
            if let Some(requirement) = &package.hew {
                let supported = compiler_supports(requirement).map_err(|error| {
                    FrontendFailure::coded_message(
                        "E_COMPILER_VERSION",
                        format!(
                            "{} has an invalid package.hew requirement: {error}",
                            path.display()
                        ),
                    )
                })?;
                if !supported {
                    return Err(FrontendFailure::coded_message(
                        "E_COMPILER_VERSION",
                        format!(
                            "{} requires Hew {requirement}; this compiler implements Hew \
                             {COMPILER_VERSION}",
                            path.display()
                        ),
                    ));
                }
            }
            if !SUPPORTED_EDITIONS.contains(&package.edition.as_str()) {
                return Err(FrontendFailure::message_only(format!(
                    "Error: E_UNSUPPORTED_EDITION: {} declares edition = \"{}\", which this compiler does not support (supported: {:?})",
                    path.display(),
                    package.edition,
                    SUPPORTED_EDITIONS
                )));
            }
        }
    }
    Ok(manifest)
}

fn verify_locked_project_package(
    check: &LockedPackageCheck,
    sources: &SourceSet,
) -> Result<(), FrontendFailure> {
    let Some(manifest) = load_manifest_from(&check.package_dir, sources)? else {
        return Err(FrontendFailure::message_only(format!(
            "Error: locked package `{}` resolved through `{}` is missing hew.toml\n  hint: run `hew install` to refresh .hew/packages",
            check.name,
            check.package_dir.display()
        )));
    };
    let Some(package) = manifest.package else {
        return Err(FrontendFailure::message_only(format!(
            "Error: locked package `{}` resolved through `{}` has no [package] section\n  hint: run `hew install` to refresh .hew/packages",
            check.name,
            check.package_dir.display()
        )));
    };
    if package.name != check.name || package.version.as_deref() != Some(check.version.as_str()) {
        let found = package.version.as_deref().map_or_else(
            || format!("{}@<missing-version>", package.name),
            |version| format!("{}@{version}", package.name),
        );
        return Err(FrontendFailure::message_only(format!(
            "Error: locked package `{}` resolved through `{}` does not match hew.lock (expected {}@{}, found {found})\n  hint: run `hew install` to refresh .hew/packages",
            check.name,
            check.package_dir.display(),
            check.name,
            check.version
        )));
    }
    Ok(())
}

#[cfg(test)]
fn load_manifest_metadata(
    dir: &Path,
) -> Result<(Option<Vec<String>>, Option<String>), FrontendFailure> {
    load_manifest_metadata_from(dir, &EMPTY_DOCUMENTS)
}

fn load_manifest_metadata_from(
    dir: &Path,
    sources: &SourceSet,
) -> Result<(Option<Vec<String>>, Option<String>), FrontendFailure> {
    match load_manifest_from(dir, sources)? {
        Some(TomlManifest {
            package,
            dependencies,
        }) => Ok((
            Some(dependencies.into_keys().collect()),
            package.map(|package| package.name),
        )),
        None => Ok((None, None)),
    }
}

#[cfg(test)]
fn load_lockfile(dir: &Path) -> Result<Option<Vec<(String, String)>>, FrontendFailure> {
    load_lockfile_from(dir, &EMPTY_DOCUMENTS)
}

fn load_lockfile_from(
    dir: &Path,
    sources: &SourceSet,
) -> Result<Option<Vec<(String, String)>>, FrontendFailure> {
    let path = dir.join("hew.lock");
    let Some(lock) = load_optional_toml_from::<HewTomlLock>(&path, sources)? else {
        return Ok(None);
    };
    Ok(Some(
        lock.package
            .into_iter()
            .map(|entry| (entry.name, entry.version))
            .collect(),
    ))
}

#[cfg(test)]
fn load_package_name(dir: &Path) -> Result<Option<String>, FrontendFailure> {
    Ok(load_manifest(dir)?.and_then(|manifest| manifest.package.map(|package| package.name)))
}

#[cfg(test)]
fn load_dependencies(dir: &Path) -> Result<Option<Vec<String>>, FrontendFailure> {
    Ok(load_manifest(dir)?.map(|manifest| manifest.dependencies.into_keys().collect()))
}

#[cfg(test)]
mod tests {
    use super::{
        build_module_graph, check_file, check_file_with_state, check_program, checker_search_paths,
        display_path, hir_diagnostics_to_frontend, load_dependencies, load_lockfile,
        load_package_name, parse_source, retain_user_facing_diagnostics, run_document_frontend,
        run_file_frontend_to_typecheck, run_source_frontend, DiagnosticPolicy, FrontendDiagnostic,
        FrontendDiagnosticKind, FrontendOptions, ImportResolutionContext, Session, SessionTarget,
        SourceSet,
    };
    use hew_parser::ast::Item;
    use std::collections::{HashMap, HashSet};
    use std::fs::{self, File};
    use std::io::Write;
    use std::path::Path;

    #[test]
    fn embedded_prelude_lowers_vector_get_and_println_once() {
        let mut parsed = hew_parser::parse(
            "fn main() { let values = [1, 2]; println(values.get(0).expect(\"missing\")); \
             let text = \"a😀b\"; println(text.len()); println(text.find_from(\"b\", 1).expect(\"missing\")); }",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        super::attach_prelude_std_modules(&mut parsed.program);
        let mut checker =
            hew_types::Checker::new(hew_types::module_registry::ModuleRegistry::new(Vec::new()));
        let facts = checker.check_program(&parsed.program);
        assert!(facts.errors.is_empty(), "{:?}", facts.errors);
        let lowered =
            hew_hir::lower_program_host_target(&parsed.program, &facts, &hew_hir::ResolutionCtx);
        assert!(lowered.diagnostics.is_empty(), "{:?}", lowered.diagnostics);
    }

    fn write_toml(dir: &Path, content: &str) {
        let mut file = File::create(dir.join("hew.toml")).expect("create hew.toml");
        file.write_all(content.as_bytes()).expect("write hew.toml");
    }

    fn write_lockfile(dir: &Path, content: &str) {
        let mut file = File::create(dir.join("hew.lock")).expect("create hew.lock");
        file.write_all(content.as_bytes()).expect("write hew.lock");
    }

    /// A quoted import above the root file's directory is named relative to
    /// it, never by the absolute path of the checkout.
    #[test]
    fn file_module_ids_are_relative_to_the_root_directory() {
        let root = Path::new("/work/project/app");
        let id = |file: &str| super::module_id_from_file(root, Path::new(file)).dotted();
        assert_eq!(id("/work/project/app/main.hew"), "main");
        assert_eq!(id("/work/project/app/x/util.hew"), "x.util");
        assert_eq!(id("/work/project/a/util.hew"), "super.a.util");
        assert_eq!(id("/work/other/b/util.hew"), "super.super.other.b.util");
    }

    #[test]
    fn file_module_ids_preserve_exceptional_components_and_relocation() {
        let root = Path::new("/work/project/app");
        let id = |file: &str| super::module_id_from_file(root, Path::new(file));
        assert_ne!(
            id("/work/project/util.hew"),
            id("/work/project/app/super/util.hew")
        );
        assert_ne!(
            id("/work/project/app/super/util.hew"),
            id("/work/project/app/__hew_file_name_7375706572/util.hew")
        );
        assert_ne!(
            id("/work/project/app/a.b/util.hew"),
            id("/work/project/app/a/b/util.hew")
        );
        assert_ne!(id("/work/project/app.hew"), id("/work/project/app/app.hew"));
        assert_eq!(id("/work/project/app.hew").dotted(), "super.app");
        assert_ne!(
            id("/work/project/app/util.hew"),
            id("/work/project/app/util.txt")
        );
        assert_ne!(
            id("/work/project/app/util.hew"),
            id("/work/project/app/util")
        );
        assert_eq!(
            id("/work/project/lib/util.hew"),
            super::module_id_from_file(
                Path::new("/another/checkout/app"),
                Path::new("/another/checkout/lib/util.hew")
            )
        );
    }

    #[cfg(unix)]
    #[test]
    fn file_module_ids_preserve_non_utf8_components() {
        use std::os::unix::ffi::OsStringExt;
        let root = Path::new("/work/app");
        let file = |byte| {
            root.join(std::ffi::OsString::from_vec(vec![byte]))
                .join("util.hew")
        };
        let first = super::module_id_from_file(root, &file(0xff));
        assert_ne!(first, super::module_id_from_file(root, &file(0xfe)));
        assert_ne!(
            first,
            super::module_id_from_file(root, &root.join("util.hew"))
        );
    }

    #[cfg(windows)]
    #[test]
    fn file_module_ids_preserve_windows_volume_and_unc_prefixes() {
        let root = Path::new(r"C:\work\app");
        let id = |file: &str| super::module_id_from_file(root, Path::new(file));
        assert_ne!(id(r"D:\other\util.hew"), id(r"E:\other\util.hew"));
        assert_ne!(id(r"\\host\one\util.hew"), id(r"\\host\two\util.hew"));
        assert_ne!(
            id(r"D:\other\util.hew"),
            id(r"C:\work\app\D\other\util.hew")
        );
        assert_eq!(id(r"C:\work\lib\util.hew").dotted(), "super.lib.util");
    }

    /// A diagnostic label must name the file the way the user did, never
    /// `canonicalize()`'s Windows extended-length verbatim form (#3416).
    #[test]
    fn display_path_strips_windows_verbatim_prefix() {
        assert_eq!(
            display_path(Path::new(r"\\?\D:\a\hew\hew\tests\token.hew")),
            r"D:\a\hew\hew\tests\token.hew",
        );
        assert_eq!(
            display_path(Path::new(r"\\?\UNC\server\share\token.hew")),
            r"\\server\share\token.hew",
        );
        // Negative control: a path never canonicalized on Windows, and every
        // POSIX path, must render unchanged.
        assert_eq!(
            display_path(Path::new("/a/hew/tests/token.hew")),
            "/a/hew/tests/token.hew",
        );
    }

    /// An unsaved buffer checks against its saved siblings: the driver reads
    /// the open document for `lib.hew` and the file on disk for everything
    /// else.
    #[test]
    fn an_open_buffer_overrides_the_file_on_disk() {
        let dir = tempfile::tempdir().expect("create document-overlay fixture");
        write_source(dir.path(), "lib.hew", "pub fn answer() -> i64 { 1 }\n");
        let input = write_source(
            dir.path(),
            "main.hew",
            "import \"lib.hew\";\n\nfn main() { println(answer() + bonus()); }\n",
        );

        // Negative control: the saved `lib.hew` has no `bonus`.
        let saved = run_document_frontend(&input, &FrontendOptions::default());
        assert!(
            saved.stopped.is_some(),
            "the saved sibling declares no `bonus`: {:#?}",
            saved.diagnostics
        );

        let mut documents = SourceSet::new();
        documents.insert(
            dir.path().join("lib.hew"),
            "pub fn answer() -> i64 { 1 }\npub fn bonus() -> i64 { 2 }\n",
        );
        let options = FrontendOptions {
            documents,
            ..FrontendOptions::default()
        };
        let open = run_document_frontend(&input, &options);
        assert!(
            open.stopped.is_none(),
            "the open buffer declares `bonus`: {:#?}",
            open.diagnostics
        );
    }

    #[test]
    fn imported_external_impl_credits_direct_and_umbrella_import_routes() {
        let dir = tempfile::tempdir().unwrap();
        write_source(
            dir.path(),
            "methods.hew",
            "impl NodeConfig { pub fn answer() -> i64 { 42 } }",
        );
        write_source(dir.path(), "umbrella.hew", "import methods;");
        write_source(dir.path(), "idle.hew", "pub fn unrelated() -> i64 { 0 }");
        for import in [
            "import methods;",
            "import umbrella;",
            "import umbrella as tools;",
        ] {
            let source =
                format!("{import}\nimport idle;\nfn main() -> i64 {{ NodeConfig.answer() }}");
            let input = write_source(dir.path(), "main.hew", &source);
            let state = run_document_frontend(&input, &FrontendOptions::default());
            assert!(state.stopped.is_none(), "{:?}", state.diagnostics);
            let output = state
                .typecheck_result
                .as_ref()
                .and_then(|result| result.tco.as_ref())
                .unwrap();
            let unused: Vec<_> = output
                .warnings
                .iter()
                .filter(|warning| warning.kind == hew_types::error::TypeErrorKind::UnusedImport)
                .map(|warning| warning.message.as_str())
                .collect();
            assert_eq!(
                unused,
                ["unused import: `idle`"],
                "{import}: {:?}",
                output.warnings
            );
        }
        let input = write_source(
            dir.path(),
            "main.hew",
            "fn main() -> i64 { NodeConfig.answer() }",
        );
        let missing = run_document_frontend(&input, &FrontendOptions::default());
        assert!(
            missing.stopped.is_some(),
            "the supplying import is required"
        );
    }

    /// A buffer with no file behind it runs the same frontend, including the
    /// implicit `std.text.regex` import a regex literal needs.
    #[test]
    fn a_buffer_without_a_file_gets_the_implicit_regex_import() {
        let source = "fn main() { let pattern = re\"a+\"; println(pattern.is_match(\"aaa\")); }\n";
        let state = run_source_frontend(source, "<buffer>", &FrontendOptions::default());
        assert!(
            state.stopped.is_none(),
            "the driver injects the regex import: {:#?}",
            state.diagnostics
        );
    }

    /// The editors need the checker output of a run that reported errors, not
    /// only the failure.
    #[test]
    fn a_stopped_run_still_carries_its_checker_output() {
        let state = run_source_frontend(
            "fn main() { let x: i64 = \"text\"; println(x); }\n",
            "<buffer>",
            &FrontendOptions::default(),
        );
        let stopped = state.stopped.as_ref().expect("the assignment is ill-typed");
        assert_eq!(stopped.message, "type errors found");
        let tco = state
            .typecheck_result
            .as_ref()
            .and_then(|result| result.tco.as_ref())
            .expect("a type-checked run keeps its checker output");
        assert!(!tco.errors.is_empty());
        assert!(state.parse_result.is_some());
    }

    /// Make `dir` a package, the anchor directory modules need (spec 3.5.1).
    fn write_package_manifest(dir: &Path) {
        fs::write(dir.join("hew.toml"), "[package]\nname = \"app\"\n").expect("write manifest");
    }

    fn write_source(dir: &Path, name: &str, content: &str) -> String {
        let path = dir.join(name);
        let mut file = File::create(&path).expect("create source file");
        file.write_all(content.as_bytes())
            .expect("write source file");
        path.display().to_string()
    }

    #[test]
    fn selected_occurrence_never_falls_back_to_authored_main() {
        let dir = tempfile::tempdir().expect("create selected-entry fixture");
        let source = "fn main() {}\n\n#[test]\nfn selected_test() {}\n";
        let input = write_source(dir.path(), "entry_test.hew", source);
        let program = parse_source(source, &input).expect("parse selected-entry fixture");
        let selection = program
            .items
            .iter()
            .enumerate()
            .find_map(|(item_ordinal, (item, span))| match item {
                Item::Function(function)
                    if function
                        .attributes
                        .iter()
                        .any(|attribute| attribute.name == "test") =>
                {
                    Some(
                        hew_types::DeclarationOccurrence::new_with_synthetic_ordinal(
                            None,
                            span,
                            item_ordinal,
                            hew_types::DeclarationKind::Function,
                            0,
                        ),
                    )
                }
                _ => None,
            })
            .expect("selected test occurrence");

        let options = FrontendOptions {
            entry_selection: Some(selection),
            ..FrontendOptions::default()
        };
        let state = run_file_frontend_to_typecheck(&input, &options)
            .expect("selected-entry fixture must type-check");

        let tco = state.typecheck_result.tco.expect("typecheck output");
        assert_eq!(
            tco.defs
                .display(tco.entry_exit_plan.expect("selected entry plan").entry),
            "selected_test",
            "a present selection must not fall back to authored main"
        );
    }

    #[test]
    fn selected_occurrence_survives_directory_module_entry_import() {
        let dir = tempfile::tempdir().expect("create directory-module fixture");
        fs::write(dir.path().join("hew.toml"), "[package]\nname = \"app\"\n")
            .expect("write manifest");
        let module_dir = dir.path().join("greeting");
        fs::create_dir(&module_dir).expect("create module directory");
        write_source(
            &module_dir,
            "greeting.hew",
            "pub trait Greeter {\n    fn greet(self);\n}\n",
        );
        let source = concat!(
            "type Dog {}\n",
            "impl Greeter for Dog {\n    fn greet(self) {}\n}\n",
            "#[test]\n",
            "fn selected_test() {}\n",
        );
        let input = write_source(&module_dir, "dog_test.hew", source);
        let program = parse_source(source, &input).expect("parse selected-entry fixture");
        let selection = program
            .items
            .iter()
            .enumerate()
            .find_map(|(item_ordinal, (item, span))| match item {
                Item::Function(function) if function.name.name.as_str() == "selected_test" => Some(
                    hew_types::DeclarationOccurrence::new_with_synthetic_ordinal(
                        None,
                        span,
                        item_ordinal,
                        hew_types::DeclarationKind::Function,
                        0,
                    ),
                ),
                _ => None,
            })
            .expect("selected test occurrence");

        let state = run_file_frontend_to_typecheck(
            &input,
            &FrontendOptions {
                project_dir: Some(dir.path().to_path_buf()),
                entry_selection: Some(selection),
                companion: super::module_membership(Path::new(&input), &FrontendOptions::default())
                    .and_then(|membership| {
                        let input = Path::new(&input).canonicalize().ok()?;
                        membership.test_companion(&input).map(Path::to_path_buf)
                    }),
                ..FrontendOptions::default()
            },
        )
        .expect("selected occurrence must survive the module companion import");

        let tco = state.typecheck_result.tco.expect("typecheck output");
        assert_eq!(
            tco.defs
                .display(tco.entry_exit_plan.expect("selected entry plan").entry),
            "selected_test"
        );
    }

    #[test]
    fn selected_stdlib_tests_keep_their_canonical_source_module() {
        let input = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .join("std/concurrency/lifecycle.hew")
            .display()
            .to_string();
        let source = fs::read_to_string(&input).unwrap();
        let program = parse_source(&source, &input).unwrap();
        let selection = program
            .items
            .iter()
            .enumerate()
            .find_map(|(ordinal, (item, span))| {
                let Item::Function(function) = item else {
                    return None;
                };
                (function.name.name.as_str() == "lifecycle_i64_happy_path_state_names").then(|| {
                    hew_types::DeclarationOccurrence::new_with_synthetic_ordinal(
                        None,
                        span,
                        ordinal,
                        hew_types::DeclarationKind::Function,
                        0,
                    )
                })
            })
            .expect("inline lifecycle test fixture");
        let state = run_file_frontend_to_typecheck(
            &input,
            &FrontendOptions {
                test_entry_selections: vec![selection],
                host_admission: crate::HostAdmission::Tests(vec![selection]),
                ..FrontendOptions::default()
            },
        )
        .expect("inline std test must remain selectable after graph-root rewriting");
        let output = state.typecheck_result.tco.unwrap();
        assert_eq!(output.test_entry_plans.len(), 1);
        assert_eq!(
            output.defs.path(output.test_entry_plans[0].entry),
            "std.concurrency.lifecycle.lifecycle_i64_happy_path_state_names"
        );
        assert!(output.entry_exit_plan.is_none());
        Session::new(SessionTarget::native(), DiagnosticPolicy::default())
            .lower_program(&state.program, &output)
            .expect("selected private std test and its helpers must lower through SIR");
    }

    #[test]
    fn missing_selected_occurrence_is_a_type_error() {
        let dir = tempfile::tempdir().expect("create selected-entry fixture");
        let source = "fn main() {}\n\n#[test]\nfn selected_test() {}\n";
        let input = write_source(dir.path(), "entry_test.hew", source);
        let missing = hew_types::DeclarationOccurrence::new(
            None,
            &(source.len() + 1..source.len() + 2),
            hew_types::DeclarationKind::Function,
            0,
        );

        let options = FrontendOptions {
            entry_selection: Some(missing),
            ..FrontendOptions::default()
        };
        let Err(failure) = run_file_frontend_to_typecheck(&input, &options) else {
            panic!("a missing selected entry must fail closed");
        };

        assert!(failure.diagnostics.iter().any(|diagnostic| {
            matches!(
                &diagnostic.kind,
                FrontendDiagnosticKind::Type(error)
                    if error.message.contains("selected process entry occurrence")
            )
        }));
    }

    #[test]
    fn selected_occurrence_from_another_module_is_rejected() {
        let dir = tempfile::tempdir().expect("create selected-entry fixture");
        write_source(dir.path(), "helper.hew", "pub fn value() -> i64 { 1 }\n");
        let source =
            "import helper;\n\nfn main() {}\n\n#[test]\nfn selected_test() { assert(true); }\n";
        let input = write_source(dir.path(), "entry_test.hew", source);
        let initial = run_file_frontend_to_typecheck(&input, &FrontendOptions::default())
            .expect("fixture must establish module identities");
        let helper_module = initial
            .typecheck_result
            .tco
            .as_ref()
            .expect("typecheck output")
            .defs
            .module_for_path("helper")
            .expect("imported helper module identity");
        let program = parse_source(source, &input).expect("parse selected-entry fixture");
        let foreign_selection = program
            .items
            .iter()
            .enumerate()
            .find_map(|(item_ordinal, (item, span))| match item {
                Item::Function(function) if function.name.name.as_str() == "selected_test" => Some(
                    hew_types::DeclarationOccurrence::new_with_synthetic_ordinal(
                        Some(helper_module),
                        span,
                        item_ordinal,
                        hew_types::DeclarationKind::Function,
                        0,
                    ),
                ),
                _ => None,
            })
            .expect("selected test occurrence");
        let options = FrontendOptions {
            entry_selection: Some(foreign_selection),
            ..FrontendOptions::default()
        };

        let Err(failure) = run_file_frontend_to_typecheck(&input, &options) else {
            panic!("a foreign-module occurrence must not select a root function");
        };

        assert!(failure.diagnostics.iter().any(|diagnostic| {
            matches!(
                &diagnostic.kind,
                FrontendDiagnosticKind::Type(error)
                    if error.message.contains("selected process entry occurrence")
            )
        }));
    }

    /// A `std/` beside the source file is not the standard library: a source
    /// inside a directory shaped like a Hew checkout still resolves std from
    /// the toolchain's root, never from that directory.
    #[test]
    fn checker_search_paths_ignore_a_std_beside_the_source() {
        let root = tempfile::tempdir().expect("create lookalike checkout root");
        fs::create_dir_all(root.path().join("std")).expect("create std dir");
        fs::write(root.path().join("std/builtins.hew"), "// marker\n")
            .expect("write builtins marker");
        write_source(root.path(), "main.hew", "fn main() {}\n");

        let paths = checker_search_paths(&FrontendOptions::default());

        assert!(
            !paths.contains(&root.path().to_path_buf()),
            "a std beside the source must not be selected: {paths:?}"
        );
    }

    /// A diamond of module imports (`main` -> `left`, `right` -> `base`)
    /// leaves the module DFS free to visit `left` or `right` first. HIR
    /// lowering walks `topo_order` to order function bodies and to mint
    /// `BindingId`s, so that choice must be the same on every compile: the
    /// raw MIR dump (function order, binding and site ids) must be
    /// byte-identical across repeated in-process compiles of one program.
    #[test]
    fn diamond_module_imports_lower_identically_across_repeated_compiles() {
        let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("hew-compile has a workspace parent")
            .to_path_buf();
        let dir = tempfile::tempdir().expect("create temp dir");
        write_source(
            dir.path(),
            "base.hew",
            "pub fn base_value() -> i64 {\n    let x = 100;\n    let y = x + 1;\n    y\n}\n",
        );
        write_source(
            dir.path(),
            "left.hew",
            "import base;\n\npub fn left_value() -> i64 {\n    let a = base.base_value();\n    let b = a + 1;\n    b\n}\n",
        );
        write_source(
            dir.path(),
            "right.hew",
            "import base;\n\npub fn right_value() -> i64 {\n    let c = base.base_value();\n    let d = c + 2;\n    d\n}\n",
        );
        let input = write_source(
            dir.path(),
            "main.hew",
            "import left;\nimport right;\n\nfn main() {\n    let l = left.left_value();\n    let r = right.right_value();\n    let total = l + r;\n    println(f\"{total}\");\n}\n",
        );

        let session = super::Session::new(
            super::SessionTarget::native(),
            super::DiagnosticPolicy::default(),
        );
        let dumps: Vec<String> = (0..20)
            .map(|_| {
                let state = run_file_frontend_to_typecheck(
                    &input,
                    &FrontendOptions {
                        module_search_paths: Some(vec![repo_root.clone()]),
                        ..FrontendOptions::default()
                    },
                )
                .expect("diamond fixture must type-check");
                let tco = state
                    .typecheck_result
                    .tco
                    .as_ref()
                    .expect("type checking was enabled");
                let hir = hew_hir::lower_program(
                    &state.program,
                    tco,
                    &hew_hir::ResolutionCtx,
                    hew_hir::TargetArch::host(),
                );
                assert!(
                    hir.diagnostics.is_empty(),
                    "diamond fixture must lower without HIR diagnostics: {:#?}",
                    hir.diagnostics
                );
                let output = session
                    .lower_hir_module(&hir.module, tco, &[])
                    .expect("diamond fixture must verify");
                hew_sir::dump_lowering(output.semantics())
            })
            .collect();
        assert!(
            dumps[0].contains("fn __hew_fn_left$left_value")
                && dumps[0].contains("fn __hew_fn_right$right_value"),
            "dump must contain both imported functions:\n{}",
            dumps[0]
        );
        for (run, dump) in dumps.iter().enumerate().skip(1) {
            assert_eq!(
                dump, &dumps[0],
                "raw MIR dump of run {run} differs from run 0"
            );
        }
    }

    #[test]
    fn user_surface_removes_imported_stdlib_diagnostics() {
        let dir = tempfile::tempdir().expect("create diagnostic boundary fixture");
        let root = write_source(dir.path(), "main.hew", "fn main() {}\n");
        let std_dir = dir.path().join("toolchain").join("std");
        fs::create_dir_all(&std_dir).expect("create synthetic stdlib directory");
        let std_source = write_source(&std_dir, "arena.hew", "// synthetic stdlib\n");

        let mut user = FrontendDiagnostic::message("user warning");
        user.filename = Some(root.clone());
        let mut stdlib = FrontendDiagnostic::message("stdlib warning");
        stdlib.filename = Some(std_source);
        let mut diagnostics = vec![user, stdlib];

        retain_user_facing_diagnostics(&root, &[std_dir], &mut diagnostics);

        assert_eq!(diagnostics.len(), 1);
        assert_eq!(diagnostics[0].filename.as_deref(), Some(root.as_str()));
    }

    #[test]
    fn memory_floor_calls_are_rejected_at_the_source_boundary() {
        let dir = tempfile::tempdir().unwrap();
        let input = write_source(
            dir.path(),
            "main.hew",
            "import std.mem; fn main() { let pointer = mem.alloc(8, 8); mem.dealloc(pointer, 8, 8); }",
        );
        let failure = check_file(&input, &FrontendOptions::default())
            .expect_err("raw memory primitives are not a source-language API");
        assert!(failure.diagnostics.iter().any(|diagnostic| matches!(
            &diagnostic.kind,
            FrontendDiagnosticKind::Type(error)
                if matches!(&error.kind, hew_types::error::TypeErrorKind::IntrinsicOutsideFloor { intrinsic_key, .. }
                    if intrinsic_key == "mem.alloc")
        )), "{:?}", failure.diagnostics);
    }

    #[test]
    fn direct_memory_floor_intrinsics_remain_declarations() {
        let input = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .join("std/mem/mem.hew");
        let state =
            run_file_frontend_to_typecheck(input.to_str().unwrap(), &FrontendOptions::default())
                .expect("memory floor signatures typecheck");
        let output = state.typecheck_result.tco.unwrap();
        Session::new(SessionTarget::native(), DiagnosticPolicy::default())
            .lower_program(&state.program, &output)
            .expect("memory floor declarations do not manufacture executable bodies");
    }

    #[test]
    fn direct_stdlib_check_retains_source_diagnostics() {
        let dir = tempfile::tempdir().expect("create direct stdlib fixture");
        let std_dir = dir.path().join("std");
        fs::create_dir(&std_dir).expect("create stdlib directory");
        let std_source = write_source(&std_dir, "arena.hew", "// synthetic stdlib\n");
        let mut diagnostic = FrontendDiagnostic::message("stdlib warning");
        diagnostic.filename = Some(std_source.clone());
        let mut diagnostics = vec![diagnostic];

        retain_user_facing_diagnostics(&std_source, &[std_dir], &mut diagnostics);

        assert_eq!(diagnostics.len(), 1);
        assert_eq!(
            diagnostics[0].filename.as_deref(),
            Some(std_source.as_str())
        );
    }

    #[test]
    fn user_std_directory_is_not_treated_as_the_configured_stdlib() {
        let dir = tempfile::tempdir().expect("create diagnostic boundary fixture");
        let root = write_source(dir.path(), "main.hew", "fn main() {}\n");
        let configured_std = dir.path().join("toolchain").join("std");
        fs::create_dir_all(&configured_std).expect("create configured stdlib directory");
        let user_std = dir.path().join("project").join("std");
        fs::create_dir_all(&user_std).expect("create user std directory");
        let user_source = write_source(&user_std, "helpers.hew", "// user source\n");
        let mut diagnostic = FrontendDiagnostic::message("user warning");
        diagnostic.filename = Some(user_source.clone());
        let mut diagnostics = vec![diagnostic];

        retain_user_facing_diagnostics(&root, &[configured_std], &mut diagnostics);

        assert_eq!(diagnostics.len(), 1);
        assert_eq!(
            diagnostics[0].filename.as_deref(),
            Some(user_source.as_str())
        );
    }

    #[test]
    fn checking_directory_module_peer_loads_entry_namespace() {
        let dir = tempfile::tempdir().expect("create directory-module fixture");
        write_package_manifest(dir.path());
        let module_dir = dir.path().join("greeting");
        fs::create_dir(&module_dir).expect("create module directory");
        write_source(
            &module_dir,
            "greeting.hew",
            "pub trait Greeter {\n    fn name(self) -> string;\n    fn greet(self) -> string { self.name() }\n}\n",
        );
        let peer = write_source(
            &module_dir,
            "dog.hew",
            "pub type Dog {\n    label: string;\n}\n\nimpl Greeter for Dog {\n    fn name(self) -> string {\n        self.label\n    }\n}\n\npub fn describe(d: Dog) -> string {\n    d.greet()\n}\n",
        );

        let result = check_file(
            &peer,
            &FrontendOptions {
                project_dir: Some(dir.path().to_path_buf()),
                ..FrontendOptions::default()
            },
        );
        assert!(
            result.is_ok(),
            "a directly checked peer must share its directory module entry: {:#?}",
            result.err()
        );
    }

    /// A package with a `forge` directory module whose entry uses a peer's
    /// function and whose peer uses the entry's types, plus a package-local
    /// `util` module the peer imports.
    fn forge_package(peer_extra: &str) -> (tempfile::TempDir, String, String) {
        let dir = tempfile::tempdir().expect("create package fixture");
        fs::write(
            dir.path().join("hew.toml"),
            "[package]\nname = \"app\"\nversion = \"0.1.0\"\nedition = \"2026\"\n",
        )
        .expect("write manifest");
        let forge = dir.path().join("forge");
        fs::create_dir_all(&forge).expect("create module directory");
        write_source(
            dir.path(),
            "util.hew",
            "pub fn twice(x: i64) -> i64 {\n    x * 2\n}\n",
        );
        let entry = write_source(
            &forge,
            "forge.hew",
            "pub type ForgeConfig {\n    timeout: i64;\n}\n\npub fn describe(config: ForgeConfig) -> string {\n    ado_name(config)\n}\n",
        );
        let peer = write_source(
            &forge,
            "ado.hew",
            &format!(
                "import app.util;\n\npub fn ado_name(config: ForgeConfig) -> string {{\n    f\"ado:{{util.twice(config.timeout)}}\"\n}}\n{peer_extra}"
            ),
        );
        write_source(&forge, "ado_test.hew", "not a peer, never parsed(\n");
        (dir, entry, peer)
    }

    #[test]
    fn module_membership_assigns_entries_peers_and_tests() {
        use hew_types::module_registry::MembershipRole;
        let (dir, entry, peer) = forge_package("");
        let canonical_entry = Path::new(&entry).canonicalize().expect("entry exists");
        let membership = |path: &Path| {
            super::module_membership(path, &FrontendOptions::default()).expect("valid manifest")
        };
        let of_entry = membership(Path::new(&entry));
        assert_eq!(of_entry.role, MembershipRole::Entry);
        assert_eq!(of_entry.module, super::ModulePath::new(["app", "forge"]));
        let of_peer = membership(Path::new(&peer));
        assert_eq!(of_peer.role, MembershipRole::Peer);
        assert_eq!(of_peer.entry, canonical_entry);
        let forge = dir.path().join("forge");
        let of_test = membership(&forge.join("ado_test.hew"));
        assert_eq!(of_test.role, MembershipRole::Test);
        assert_eq!(of_test.entry, canonical_entry);
        let of_util = membership(&dir.path().join("util.hew"));
        assert_eq!(of_util.role, MembershipRole::Single);
        assert_eq!(of_util.module, super::ModulePath::new(["app", "util"]));
    }

    #[test]
    fn virtual_workspaces_preserve_loose_file_membership() {
        let dir = tempfile::tempdir().expect("create loose-file fixture");
        let model = dir.path().join("model");
        fs::create_dir(&model).expect("create same-stem directory");
        let entry = write_source(&model, "model.hew", "pub fn answer() -> i64 { 42 }\n");
        let peer = write_source(&model, "peer.hew", "fn broken() -> i64 { \"wrong\" }\n");
        for manifest in [None, Some("[workspace]\nmembers = []\n")] {
            if let Some(manifest) = manifest {
                write_toml(dir.path(), manifest);
            }
            check_file(&entry, &FrontendOptions::default())
                .expect("loose entry must not include a sibling's type error");
            let failure = check_file(&peer, &FrontendOptions::default())
                .expect_err("checking the loose sibling must report its own type error");
            assert!(failure.diagnostics.iter().any(|diagnostic| matches!(
                &diagnostic.kind,
                FrontendDiagnosticKind::Type(error)
                    if matches!(error.kind, hew_types::error::TypeErrorKind::Mismatch { .. })
            )));
        }

        write_package_manifest(dir.path());
        let failure = check_file(&entry, &FrontendOptions::default())
            .expect_err("an actual package's entry must include its peer's type error");
        assert!(failure.diagnostics.iter().any(|diagnostic| {
            diagnostic
                .filename
                .as_deref()
                .is_some_and(|file| Path::new(file).ends_with(Path::new("model").join("peer.hew")))
                && matches!(
                    &diagnostic.kind,
                    FrontendDiagnosticKind::Type(error)
                        if matches!(error.kind, hew_types::error::TypeErrorKind::Mismatch { .. })
                )
        }));
    }

    #[test]
    fn checking_a_directory_module_file_checks_the_whole_module() {
        let (_dir, entry, peer) = forge_package("");
        for input in [&entry, &peer] {
            let result = check_file(input, &FrontendOptions::default());
            assert!(
                result.is_ok(),
                "{input} must check with its entry, peers and package imports: {:#?}",
                result.err().map(|failure| failure.diagnostics)
            );
        }
    }

    #[test]
    fn checking_a_directory_module_entry_reports_errors_in_its_peers() {
        let (_dir, entry, peer) = forge_package("\nfn broken() -> i64 {\n    \"no\"\n}\n");
        for input in [&entry, &peer] {
            let failure = check_file(input, &FrontendOptions::default())
                .expect_err("the peer's type error must fail the module check");
            let mismatches = failure
                .diagnostics
                .iter()
                .filter(|diagnostic| {
                    matches!(&diagnostic.kind, FrontendDiagnosticKind::Type(error)
                        if matches!(error.kind, hew_types::error::TypeErrorKind::Mismatch { .. }))
                })
                .collect::<Vec<_>>();
            assert_eq!(mismatches.len(), 1, "{:#?}", failure.diagnostics);
            let file = mismatches[0].filename.as_deref().expect("routed to a file");
            assert!(
                Path::new(file).ends_with("forge/ado.hew"),
                "the error belongs to the peer, not {file}"
            );
        }
    }

    /// The std directory module a peer import loads, as source paths below
    /// `root`, from a run that resolves the std at `root` through `documents`.
    fn std_http_sources(root: &Path, documents: &SourceSet) -> Vec<std::path::PathBuf> {
        let dir = tempfile::tempdir().expect("create importer fixture");
        let input = write_source(
            dir.path(),
            "main.hew",
            "import std.net.http.codec.request;\n\nfn main() {}\n",
        );
        let source = fs::read_to_string(&input).expect("read importer fixture");
        let mut program = parse_source(&source, &input).expect("parse importer fixture");
        let roots = [root.to_path_buf()];
        let mut ctx = ImportResolutionContext {
            in_progress_imports: HashSet::new(),
            resolved_imports: HashMap::new(),
            manifest_deps: None,
            extra_pkg_path: None,
            locked_versions: None,
            package_name: None,
            project_dir: dir.path(),
            module_search_paths: Some(&roots),
            documents,
        };
        let graph = build_module_graph(
            Path::new(&input),
            &mut program.items,
            program.module_doc.clone(),
            &mut ctx,
        )
        .expect("the std peer import should build a module graph");
        let client = root.join("std/net/http/codec/request.hew");
        let owners = graph
            .modules
            .values()
            .filter(|module| {
                module
                    .source_paths
                    .iter()
                    .any(|path| path.ends_with("std/net/http/codec/request.hew"))
            })
            .collect::<Vec<_>>();
        assert_eq!(owners.len(), 1, "one graph node holds {}", client.display());
        let root = root.canonicalize().unwrap_or_else(|_| root.to_path_buf());
        owners[0]
            .source_paths
            .iter()
            .map(|path| {
                path.strip_prefix(&root)
                    .expect("a std source is below its root")
                    .to_path_buf()
            })
            .collect()
    }

    /// A host with no filesystem (the browser) lists a std directory module's
    /// peers from the sources it was given, so its module graph is the native
    /// one: the same entry and peers in the same order.
    #[test]
    fn std_directory_module_peers_come_from_documents_without_a_filesystem() {
        let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("hew-compile lives below the repository root")
            .to_path_buf();
        let native = std_http_sources(&repo_root, &SourceSet::new());

        let virtual_root = std::env::temp_dir().join("hew-std-without-a-filesystem");
        assert!(
            !virtual_root.exists(),
            "the virtual std root must not exist on disk"
        );
        let mut documents = SourceSet::new();
        let mut pending = vec![repo_root.join("std")];
        while let Some(dir) = pending.pop() {
            for entry in fs::read_dir(&dir).expect("read the std tree").flatten() {
                let path = entry.path();
                if path.is_dir() {
                    pending.push(path);
                } else if path.extension().is_some_and(|extension| extension == "hew") {
                    let relative = path.strip_prefix(&repo_root).expect("below the repository");
                    documents.insert(
                        virtual_root.join(relative),
                        fs::read_to_string(&path).expect("read a std source"),
                    );
                }
            }
        }
        let in_memory = std_http_sources(&virtual_root, &documents);

        assert_eq!(
            native.len(),
            2,
            "std.net.http.codec is its entry and request peer: {native:?}"
        );
        assert_eq!(native[0], Path::new("std/net/http/codec/codec.hew"));
        assert_eq!(in_memory, native);
    }

    #[test]
    fn embedded_directory_modules_resolve_peers_from_relative_roots() {
        let repo = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("repository root");
        let root = Path::new("./hew-embedded-stdlib");
        assert!(!root.exists(), "the source root must remain virtual");
        let mut documents = SourceSet::in_memory();
        let mut pending = vec![repo.join("std")];
        while let Some(dir) = pending.pop() {
            for entry in fs::read_dir(dir)
                .expect("standard-library sources")
                .flatten()
            {
                let path = entry.path();
                if path.is_dir() {
                    pending.push(path);
                } else if path.extension().is_some_and(|extension| extension == "hew") {
                    documents.insert(
                        root.join(path.strip_prefix(repo).expect("repository source")),
                        fs::read_to_string(path).expect("source text"),
                    );
                }
            }
        }
        for import in ["std.net.http.codec", "std.net.http.codec.request"] {
            let source = format!("import {import}; fn main() {{}}");
            let mut program = parse_source(&source, "playground.hew").expect("import source");
            let roots = [root.to_path_buf()];
            let mut context = ImportResolutionContext {
                in_progress_imports: HashSet::new(),
                resolved_imports: HashMap::new(),
                manifest_deps: None,
                extra_pkg_path: None,
                locked_versions: None,
                package_name: None,
                project_dir: Path::new("."),
                module_search_paths: Some(&roots),
                documents: &documents,
            };
            let graph = build_module_graph(
                Path::new("playground.hew"),
                &mut program.items,
                program.module_doc.clone(),
                &mut context,
            )
            .expect("embedded directory module graph");
            let owners = graph
                .modules
                .values()
                .filter(|module| {
                    module
                        .source_paths
                        .iter()
                        .any(|path| path.ends_with("std/net/http/codec/codec.hew"))
                })
                .collect::<Vec<_>>();
            assert_eq!(owners.len(), 1, "one owner for the codec entry");
            assert!(
                owners[0]
                    .source_paths
                    .iter()
                    .any(|path| { path.ends_with("std/net/http/codec/request.hew") }),
                "the entry and private peer helpers must share one module: {:?}",
                owners[0].source_paths
            );
        }
    }

    fn manifest_errors(manifest: &str, files: &[(&str, &str)], main: &str) -> Vec<String> {
        let dir = tempfile::tempdir().expect("create package fixture");
        fs::write(dir.path().join("hew.toml"), manifest).expect("write manifest");
        for (path, source) in files {
            let path = dir.path().join(path);
            fs::create_dir_all(path.parent().expect("a fixture file has a parent"))
                .expect("create fixture directory");
            fs::write(path, source).expect("write fixture source");
        }
        let input = write_source(dir.path(), "main.hew", main);
        let program = parse_source(main, &input).expect("parse package fixture");
        let (manifest_deps, package_name) =
            super::load_manifest_metadata(dir.path()).expect("valid manifest");
        let documents = SourceSet::new();
        let ctx = ImportResolutionContext {
            in_progress_imports: HashSet::new(),
            resolved_imports: HashMap::new(),
            manifest_deps: manifest_deps.as_deref(),
            extra_pkg_path: None,
            locked_versions: None,
            package_name: package_name.as_deref(),
            project_dir: dir.path(),
            module_search_paths: None,
            documents: &documents,
        };
        let input = Path::new(&input).canonicalize().expect("main exists");
        super::undeclared_imports(&input, &program.items, &ctx).expect("valid manifests")
    }

    #[test]
    fn an_undeclared_dependency_is_named_with_its_fix() {
        let errors = manifest_errors(
            "[package]\nname = \"app\"\n",
            &[],
            "import mylib.utils;\nimport mylib.other;\nfn main() {}\n",
        );
        assert_eq!(
            errors.len(),
            2,
            "every undeclared import is reported: {errors:?}"
        );
        assert!(errors[0].contains("`mylib.utils` is not declared in hew.toml"));
        assert!(errors[0].contains("hew add mylib.utils"));
    }

    #[test]
    fn a_declared_dependency_covers_its_modules() {
        let errors = manifest_errors(
            "[package]\nname = \"app\"\n\n[dependencies]\n\"acme.http\" = \"1.0\"\n",
            &[],
            "import acme.http;\nimport acme.http.client;\nimport acme.other;\nfn main() {}\n",
        );
        assert_eq!(errors.len(), 1, "{errors:?}");
        assert!(errors[0].contains("`acme.other`"), "{errors:?}");
    }

    #[test]
    fn std_file_and_package_local_imports_need_no_declaration() {
        let errors = manifest_errors(
            "[package]\nname = \"meshcore.broker\"\n",
            &[
                ("wire.hew", "pub fn w() -> i64 { 7 }\n"),
                ("helpers.hew", "pub fn h() -> i64 { 1 }\n"),
                (
                    "greeting/greeting.hew",
                    "pub fn hello() -> string { \"hi\" }\n",
                ),
                ("lib.hew", "pub fn l() -> i64 { 2 }\n"),
            ],
            "import std.fs;\nimport \"lib.hew\";\nimport meshcore.broker.wire;\nimport helpers;\nimport greeting;\nfn main() {}\n",
        );
        assert!(errors.is_empty(), "{errors:?}");
    }

    #[test]
    fn a_dotted_package_imports_itself_but_not_a_shorter_prefix() {
        let errors = manifest_errors(
            "[package]\nname = \"meshcore.broker\"\n",
            &[("wire.hew", "pub fn w() -> i64 { 7 }\n")],
            "import meshcore.broker.wire;\nimport meshcore.wire;\nfn main() {}\n",
        );
        assert_eq!(errors.len(), 1, "{errors:?}");
        assert!(errors[0].contains("`meshcore.wire`"), "{errors:?}");
    }

    #[test]
    fn stdlib_directory_peer_imports_share_one_complete_module_owner() {
        let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("hew-compile lives below the repository root")
            .to_path_buf();

        for imports in [
            "import std.net.http.codec;\nimport std.net.http.codec.request;\n",
            "import std.net.http.codec.request;\nimport std.net.http.codec;\n",
            "import std.net.http.codec.request;\n",
        ] {
            let dir = tempfile::tempdir().expect("create module-owner fixture");
            let input = write_source(
                dir.path(),
                "main.hew",
                &format!("{imports}\nfn main() {{}}\n"),
            );
            let source = fs::read_to_string(&input).expect("read module-owner fixture");
            let mut program = parse_source(&source, &input).expect("parse module-owner fixture");
            let documents = SourceSet::new();
            let mut ctx = ImportResolutionContext {
                in_progress_imports: HashSet::new(),
                resolved_imports: HashMap::new(),
                manifest_deps: None,
                extra_pkg_path: None,
                locked_versions: None,
                package_name: None,
                project_dir: dir.path(),
                module_search_paths: Some(std::slice::from_ref(&repo_root)),
                documents: &documents,
            };

            let graph = build_module_graph(
                Path::new(&input),
                &mut program.items,
                program.module_doc.clone(),
                &mut ctx,
            )
            .expect("stdlib peer imports should build a module graph");
            let http_id = hew_parser::module::ModulePath::new(["std", "net", "http", "codec"]);
            let peer_id =
                hew_parser::module::ModulePath::new(["std", "net", "http", "codec", "request"]);
            let http = graph
                .modules
                .get(&http_id)
                .expect("the canonical std.net.http.codec module should be present");
            assert!(
                !graph.modules.contains_key(&peer_id),
                "the peer must not become a second graph owner: {:?}",
                graph.modules.keys().collect::<Vec<_>>()
            );
            assert!(
                http.source_paths
                    .iter()
                    .any(|path| path.ends_with("std/net/http/codec/codec.hew")),
                "the canonical module must retain its entry source: {:?}",
                http.source_paths
            );
            assert!(
                http.source_paths
                    .iter()
                    .any(|path| path.ends_with("std/net/http/codec/request.hew")),
                "the canonical module must retain its peer source: {:?}",
                http.source_paths
            );
            assert!(
                http.items.iter().any(|(item, _)| matches!(
                    item,
                    Item::TypeDecl(decl) if decl.name.name.as_str() == "Response"
                )),
                "peer-only imports must load the complete package item set"
            );
        }
    }

    #[test]
    fn imported_http_and_json_preserve_checked_return_identity() {
        let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("hew-compile lives below the repository root");
        let dir = tempfile::tempdir().expect("create stdlib return fixture");
        let input = write_source(
            dir.path(),
            "main.hew",
            "import std.net.http.codec;\nimport std.net.http.codec.request;\nimport std.encoding.json;\nfn main() {}\n",
        );
        let state = run_file_frontend_to_typecheck(
            &input,
            &FrontendOptions {
                module_search_paths: Some(vec![repo_root.to_path_buf()]),
                ..FrontendOptions::default()
            },
        )
        .expect("stdlib imports must type-check");
        let hir = hew_hir::lower_program(
            &state.program,
            state.typecheck_result.tco.as_ref().expect("checked output"),
            &hew_hir::ResolutionCtx,
            hew_hir::TargetArch::host(),
        );
        assert!(
            hir.diagnostics.is_empty(),
            "imported return expressions must retain their checked identities: {:#?}",
            hir.diagnostics
        );
    }

    #[test]
    fn typecheck_top_level_networking_demos() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("repository root");
        for relative in ["examples/http_server.hew", "examples/static_server.hew"] {
            check_file(
                &root.join(relative).display().to_string(),
                &FrontendOptions::default(),
            )
            .unwrap_or_else(|failure| panic!("{relative}: {failure:#?}"));
        }
    }

    #[test]
    fn direct_stdlib_directory_sources_check_their_complete_owner() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("repository root");
        for relative in [
            "std/net/http/codec/codec.hew",
            "std/net/http/codec/request.hew",
        ] {
            check_file(
                &root.join(relative).display().to_string(),
                &FrontendOptions::default(),
            )
            .unwrap_or_else(|failure| panic!("{relative}: {failure:#?}"));
        }
    }

    #[test]
    fn direct_stdlib_directory_check_reports_invalid_peer_buffer() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("repository root");
        let peer = root.join("std/net/http/codec/request.hew");
        let original = fs::read_to_string(&peer).expect("read peer");
        let mut options = FrontendOptions::default();
        options.documents.insert(
            peer.display().to_string(),
            format!("{original}\npub fn invalid_peer_value() -> bool {{ 1 }}\n"),
        );
        let failure = check_file(
            &root
                .join("std/net/http/codec/codec.hew")
                .display()
                .to_string(),
            &options,
        )
        .expect_err("checking the entry must check its peer's body");
        assert!(failure.diagnostics.iter().any(|diagnostic| {
            matches!(&diagnostic.kind, FrontendDiagnosticKind::Type(error) if matches!(error.kind, hew_types::error::TypeErrorKind::Mismatch { .. }))
                && diagnostic.filename.as_deref().is_some_and(|file| Path::new(file).ends_with("codec/request.hew"))
        }), "{failure:#?}");
    }

    #[test]
    fn check_file_accepts_shipped_directory_peer_reimports() {
        let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("hew-compile lives below the repository root")
            .to_path_buf();
        let dir = tempfile::tempdir().expect("create peer-reimport fixture");
        let input = write_source(
            dir.path(),
            "main.hew",
            "import std.net.http.codec;\n\
             import std.net.http.codec.request;\n\n\
             fn main() {}\n",
        );

        check_file(
            &input,
            &FrontendOptions {
                project_dir: Some(dir.path().to_path_buf()),
                module_search_paths: Some(vec![repo_root]),
                ..FrontendOptions::default()
            },
        )
        .expect("reimporting a shipped directory peer must not duplicate declarations");
    }

    /// Reverses 10ec5abd6 (`fix(modules): limit peer promotion to shipped
    /// stdlib`), which let a user package import one of its own directory
    /// peers directly and kept it as an isolated module. That isolated
    /// parse has no access to its sibling declarations — spec 3.5.1 merges
    /// every peer into the directory module's one namespace — so any
    /// reference to a sibling surfaced downstream as a bare "undefined
    /// function"/"undefined variable" with no hint that the fix is to
    /// import the directory module instead. Refusing the import outright,
    /// naming the directory module to use, is the actionable diagnostic;
    /// the isolated-parse path is no longer reachable.
    #[test]
    fn user_directory_peer_import_is_refused() {
        let dir = tempfile::tempdir().expect("create module-owner fixture");
        write_package_manifest(dir.path());
        let module_dir = dir.path().join("greeting");
        fs::create_dir(&module_dir).expect("create module directory");
        write_source(&module_dir, "greeting.hew", "pub fn entry() -> i64 { 1 }\n");
        write_source(&module_dir, "dog.hew", "pub fn bark() -> i64 { 2 }\n");
        let input = write_source(
            dir.path(),
            "main.hew",
            "import greeting.dog;\n\nfn main() {}\n",
        );
        let source = fs::read_to_string(&input).expect("read module-owner fixture");
        let mut program = parse_source(&source, &input).expect("parse module-owner fixture");
        let documents = SourceSet::new();
        let mut ctx = ImportResolutionContext {
            in_progress_imports: HashSet::new(),
            resolved_imports: HashMap::new(),
            manifest_deps: None,
            extra_pkg_path: None,
            locked_versions: None,
            package_name: None,
            project_dir: dir.path(),
            module_search_paths: None,
            documents: &documents,
        };

        let failure = build_module_graph(
            Path::new(&input),
            &mut program.items,
            program.module_doc.clone(),
            &mut ctx,
        )
        .expect_err("importing a directory peer directly must be refused");

        let FrontendDiagnosticKind::Message(inner) = &failure.diagnostics[0].kind else {
            panic!(
                "expected a Message diagnostic, got {:?}",
                failure.diagnostics[0].kind
            );
        };
        assert_eq!(inner.code, "E_PEER_IMPORT");
        assert!(
            inner.message.contains("greeting"),
            "message should name the directory module to import instead: {}",
            inner.message
        );
    }

    /// Two peer files of one directory module that claim the same assembled
    /// name are a duplicate definition. The module graph rejects a duplicate
    /// PUB name before checking, but a private one reaches the checker — where
    /// registration visits each file on its own and never sees the collision,
    /// because only the ASSEMBLED path collides. The identity table is the
    /// only place that can report it, and it must: otherwise the user gets the
    /// internal "identity table has no declaration" wording from whichever
    /// consumer asks for the refused declaration first.
    #[test]
    fn directory_module_peers_claiming_one_name_report_a_duplicate_definition() {
        let dir = tempfile::tempdir().expect("create directory-module fixture");
        write_package_manifest(dir.path());
        let module_dir = dir.path().join("shapes");
        fs::create_dir(&module_dir).expect("create module directory");
        write_source(
            &module_dir,
            "shapes.hew",
            "type Point {\n    x: i64;\n}\n\npub fn ax() -> i64 {\n    let p = Point { x: 1 };\n    p.x\n}\n",
        );
        write_source(
            &module_dir,
            "circle.hew",
            "type Point {\n    y: i64;\n}\n\npub fn by() -> i64 {\n    let p = Point { y: 2 };\n    p.y\n}\n",
        );
        let input = write_source(
            dir.path(),
            "main.hew",
            "import shapes;\n\nfn main() { println(shapes.ax() + shapes.by()); }\n",
        );

        let failure = check_file(
            &input,
            &FrontendOptions {
                project_dir: Some(dir.path().to_path_buf()),
                ..FrontendOptions::default()
            },
        )
        .expect_err("two peers claiming `shapes.Point` must not type-check");
        let duplicates: Vec<&FrontendDiagnostic> = failure
            .diagnostics
            .iter()
            .filter(|diagnostic| {
                matches!(
                    &diagnostic.kind,
                    FrontendDiagnosticKind::Type(error)
                        if error.kind == hew_types::error::TypeErrorKind::DuplicateDefinition
                )
            })
            .collect();
        assert_eq!(
            duplicates.len(),
            1,
            "the collision must be reported exactly once: {:#?}",
            failure.diagnostics
        );
        let rendered = format!("{duplicates:#?}");
        assert!(
            rendered.contains("`Point` is defined multiple times")
                && rendered.contains("shapes.hew")
                && rendered.contains("circle.hew"),
            "the diagnostic must name both peer files: {rendered}"
        );
        assert!(
            !format!("{:#?}", failure.diagnostics).contains("identity table has no"),
            "the internal refusal wording must not reach the user: {:#?}",
            failure.diagnostics
        );
    }

    /// A file import is its own module: a private type it declares and a root
    /// type of the same name are distinct declarations, not a collision.
    #[test]
    fn a_root_type_and_a_private_file_import_type_share_a_name() {
        let dir = tempfile::tempdir().expect("create file-import fixture");
        write_source(
            dir.path(),
            "lib.hew",
            "type Point {\n    x: i64;\n}\n\npub fn lib_point() -> i64 {\n    let p = Point { x: 1 };\n    p.x\n}\n",
        );
        let input = write_source(
            dir.path(),
            "main.hew",
            "import \"lib.hew\";\n\ntype Point {\n    y: i64;\n}\n\nfn main() {\n    let p = Point { y: 2 };\n    println(p.y + lib_point());\n}\n",
        );

        check_file(&input, &FrontendOptions::default())
            .expect("a private file-import type does not collide with a root type");
    }

    /// Negative control for the rule above: an `extern "C"` symbol declared by
    /// two peer files of one directory module is a redeclaration, not a
    /// redefinition — the linker binds every call to one implementation and
    /// the extern table resolves the second against the established contract.
    /// It must keep type-checking, and both declarations must resolve.
    #[test]
    fn directory_module_peers_may_redeclare_one_extern_symbol() {
        let dir = tempfile::tempdir().expect("create directory-module fixture");
        write_package_manifest(dir.path());
        let module_dir = dir.path().join("clock");
        fs::create_dir(&module_dir).expect("create module directory");
        write_source(
            &module_dir,
            "clock.hew",
            "extern \"C\" {\n    fn hew_time_now_millis() -> i64;\n}\n             pub fn now() -> i64 { unsafe { hew_time_now_millis() } }\n",
        );
        write_source(
            &module_dir,
            "stamp.hew",
            "extern \"C\" {\n    fn hew_time_now_millis() -> i64;\n}\n             pub fn stamp() -> i64 { unsafe { hew_time_now_millis() } }\n",
        );
        let input = write_source(
            dir.path(),
            "main.hew",
            "import clock;\n\nfn main() { println(clock.now() + clock.stamp()); }\n",
        );

        let result = check_file(
            &input,
            &FrontendOptions {
                project_dir: Some(dir.path().to_path_buf()),
                ..FrontendOptions::default()
            },
        );
        assert!(
            result.is_ok(),
            "one C symbol declared by two peers is a redeclaration: {:#?}",
            result.err()
        );
    }

    #[test]
    fn no_manifest_returns_none() {
        let dir = tempfile::tempdir().expect("create temp dir");
        assert!(load_dependencies(dir.path())
            .expect("missing manifest should not error")
            .is_none());
    }

    #[test]
    fn package_name_loaded() {
        let dir = tempfile::tempdir().expect("create temp dir");
        write_toml(dir.path(), "[package]\nname = \"myapp\"\n");
        assert_eq!(
            load_package_name(dir.path()).expect("valid manifest should load"),
            Some("myapp".to_string())
        );
    }

    #[test]
    fn package_name_missing_section() {
        let dir = tempfile::tempdir().expect("create temp dir");
        write_toml(dir.path(), "[dependencies]\n");
        assert_eq!(
            load_package_name(dir.path()).expect("valid manifest should load"),
            None
        );
    }

    #[test]
    fn source_roots_keep_authored_exports_beside_file_imports() {
        let dir = tempfile::tempdir().expect("create source-root fixture");
        write_source(
            dir.path(),
            "helper.hew",
            "fn hidden() -> string { \"owned\".to_upper() } pub fn imported() -> string { hidden() }",
        );
        let input = write_source(
            dir.path(),
            "main.hew",
            "import \"helper.hew\"; pub fn exported() -> string { imported() } fn main() {}",
        );
        let state = run_file_frontend_to_typecheck(&input, &FrontendOptions::default()).unwrap();
        let tco = state.typecheck_result.tco.as_ref().unwrap();
        let roots = Session::source_roots(&state.program, tco).unwrap();
        assert_eq!(roots.len(), 1, "file imports are not implicit root exports");
        assert_eq!(
            tco.defs.name(roots[0]),
            hew_types::Symbol::intern("exported")
        );
        let output = Session::new(SessionTarget::native(), DiagnosticPolicy::default())
            .lower_program(&state.program, tco)
            .expect("an uncalled root export must retain its imported helper closure");
        let module = &output.semantics().module;
        for leaf in ["exported", "imported", "hidden"] {
            let declaration = tco
                .defs
                .declarations()
                .map(|(_, declaration)| declaration)
                .find(|declaration| tco.defs.name(*declaration) == hew_types::Symbol::intern(leaf))
                .unwrap();
            let callable = module
                .callable_for_declaration(&declaration)
                .expect("export helper must be retained");
            assert!(module.function_index().function(callable.id).is_some());
        }
    }

    /// A file-imported item lives in its file's module-graph entry only. The
    /// checker mints ONE declaration for it, and that identity is the one the
    /// call site and the lowered HIR item carry.
    #[test]
    fn file_import_declares_one_identity_per_item() {
        let dir = tempfile::tempdir().expect("create file-import fixture dir");
        write_source(
            dir.path(),
            "helper.hew",
            "pub fn helper_value() -> i64 { 7 }\n",
        );
        let input = write_source(
            dir.path(),
            "main.hew",
            "import \"helper.hew\";\n\nfn main() -> i64 { helper_value() }\n",
        );
        let state = run_file_frontend_to_typecheck(&input, &FrontendOptions::default())
            .expect("file-import fixture must type-check");
        let tco = state
            .typecheck_result
            .tco
            .as_ref()
            .expect("type checking was enabled");

        let is_helper = |item: &Item| matches!(item, Item::Function(f) if f.name.name.as_str() == "helper_value");
        let in_graph = state.program.module_graph.as_ref().is_some_and(|graph| {
            graph
                .modules
                .values()
                .any(|module| module.items.iter().any(|(item, _)| is_helper(item)))
        });
        let in_root_items = state.program.items.iter().any(|(item, _)| is_helper(item));
        assert!(
            in_graph && !in_root_items,
            "the helper belongs to its file's module, not the root \
             (graph: {in_graph}, root items: {in_root_items})"
        );

        let minted: Vec<hew_types::DefId> = tco
            .defs
            .declarations()
            .map(|(_, declaration)| declaration)
            .filter(|declaration| {
                tco.defs.name(*declaration) == hew_types::Symbol::intern("helper_value")
            })
            .collect();
        assert_eq!(
            minted.len(),
            1,
            "one physical declaration must mint exactly one DefId: {minted:?}"
        );

        let call_target = tco
            .direct_call_targets
            .values()
            .find_map(|target| match target {
                hew_types::check::CallTarget::User(declaration)
                    if tco.defs.name(*declaration) == hew_types::Symbol::intern("helper_value") =>
                {
                    Some(*declaration)
                }
                _ => None,
            })
            .expect("main's call must resolve to the helper's user target");
        assert_eq!(call_target, minted[0]);

        let hir = hew_hir::lower_program(
            &state.program,
            tco,
            &hew_hir::ResolutionCtx,
            hew_hir::TargetArch::host(),
        );
        assert!(
            hir.diagnostics.is_empty(),
            "file-import fixture must lower without HIR diagnostics: {:#?}",
            hir.diagnostics
        );
        let lowered: Vec<_> = hir
            .module
            .items
            .iter()
            .filter_map(|item| match item {
                hew_hir::HirItem::Function(function) if function.declaration == minted[0] => {
                    Some(function)
                }
                _ => None,
            })
            .collect();
        assert_eq!(
            lowered.len(),
            1,
            "HIR must lower the helper once, under the checker-minted identity"
        );
        assert_eq!(lowered[0].name, "helper$helper_value");
    }

    #[test]
    fn selective_machine_import_tracks_bare_state_references() {
        let dir = tempfile::tempdir().expect("create machine import fixture");
        write_source(
            dir.path(),
            "m.hew",
            "pub machine Light { events { Flip; } state On; state Off; \
             on Flip: On => Off; on Flip: Off => On; }",
        );
        for (body, expects_unused) in [
            ("var s = Light.On; s = Light.Off; print(\"ok\");", false),
            ("print(\"ok\");", true),
        ] {
            let input = write_source(
                dir.path(),
                "main.hew",
                &format!("import m.{{ Light }};\nfn main() {{ {body} }}\n"),
            );
            let state = run_file_frontend_to_typecheck(&input, &FrontendOptions::default())
                .expect("machine import fixture must type-check");
            let facts = state.typecheck_result.tco.as_ref().expect("type facts");
            assert!(facts.errors.is_empty(), "{:#?}", facts.errors);
            assert_eq!(
                facts.warnings.iter().any(|warning| {
                    warning.kind == hew_types::error::TypeErrorKind::UnusedImport
                }),
                expects_unused,
                "bare state references must credit only a used import: {:#?}",
                facts.warnings
            );
        }
    }

    #[test]
    fn quoted_file_imports_keep_private_function_symbols_by_source() {
        for (imports, main_body, expected) in [
            (
                "import \"a.hew\";",
                "fn main() { assert(f() == 1); }",
                vec!["a$f", "a$main", "main"],
            ),
            (
                "import \"a.hew\"; import \"b.hew\";",
                "fn main() { assert(a_api() + b_api() == 3); }",
                vec!["a$helper", "b$helper", "a$a_api", "b$b_api", "main"],
            ),
        ] {
            let dir = tempfile::tempdir().expect("create quoted import fixture");
            if expected.len() == 3 {
                write_source(
                    dir.path(),
                    "a.hew",
                    "pub fn f() -> i64 { main(); 1 } fn main() {}",
                );
            } else {
                write_source(
                    dir.path(),
                    "a.hew",
                    "fn helper() -> i64 { 1 } pub fn a_api() -> i64 { helper() }",
                );
                write_source(
                    dir.path(),
                    "b.hew",
                    "fn helper() -> i64 { 2 } pub fn b_api() -> i64 { helper() }",
                );
            }
            let input = write_source(dir.path(), "main.hew", &format!("{imports}\n{main_body}\n"));
            let state = run_file_frontend_to_typecheck(&input, &FrontendOptions::default())
                .expect("quoted import fixture must type-check");
            let tco = state.typecheck_result.tco.as_ref().expect("type facts");
            let hir = hew_hir::lower_program(
                &state.program,
                tco,
                &hew_hir::ResolutionCtx,
                hew_hir::TargetArch::host(),
            );
            assert!(hir.diagnostics.is_empty(), "{:#?}", hir.diagnostics);
            let names: Vec<_> = hir
                .module
                .items
                .iter()
                .filter_map(|item| match item {
                    hew_hir::HirItem::Function(function) => Some(function.name.as_str()),
                    _ => None,
                })
                .collect();
            for symbol in expected {
                assert!(names.contains(&symbol), "missing `{symbol}` in {names:?}");
            }
        }
    }

    #[test]
    fn dotted_imported_impl_with_iterator_has_a_callable_body() {
        let dir = tempfile::tempdir().expect("create imported method fixture");
        write_source(
            dir.path(),
            "modp3.hew",
            "pub type T { xs: Vec<f64>; }\n\
             impl T { pub fn a(self) -> f64 { var sum = 0.0;\
                 for i in (0..self.xs.len()).rev() { sum += self.xs[i]; } sum } }\n\
             pub fn mk() -> T { T { xs: [1.0, 2.0] } }\n",
        );
        let input = write_source(
            dir.path(),
            "main.hew",
            "import modp3; fn main() { let value = modp3.mk(); assert(value.a() == 3.0); }",
        );
        let state = run_file_frontend_to_typecheck(&input, &FrontendOptions::default())
            .expect("imported method fixture must type-check");
        let tco = state.typecheck_result.tco.as_ref().expect("type facts");
        let declaration = tco
            .defs
            .declarations()
            .map(|(_, declaration)| declaration)
            .find(|declaration| {
                tco.defs.kind(*declaration) == hew_types::DeclarationKind::ImplMethod
                    && tco.defs.name(*declaration) == hew_types::Symbol::intern("a")
                    && tco.defs.path(*declaration).starts_with("modp3.")
            })
            .expect("checked method identity");
        assert!(
            tco.imported_impl_body_facts.contains_key(&declaration),
            "a Range adapter has no runtime callee and cannot suppress its enclosing body"
        );
        let output = Session::new(SessionTarget::native(), DiagnosticPolicy::default())
            .lower_program(&state.program, tco)
            .expect("selected imported method must have a callable body");
        let callable = output
            .semantics()
            .module
            .callable_for_declaration(&declaration)
            .expect("method body is emitted");
        assert!(output
            .semantics()
            .module
            .function_index()
            .function(callable.id)
            .is_some());
    }

    #[test]
    fn imported_private_externs_publish_exact_direct_call_symbols() {
        let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("hew-compile has a workspace parent");
        let input = repo_root.join("tests/pkg-import/imported_actor_ask_i32.hew");
        let state = run_file_frontend_to_typecheck(
            input.to_str().expect("fixture path is utf-8"),
            &FrontendOptions {
                pkg_path: Some(repo_root.join("tests/pkg-import/pkgs")),
                ..FrontendOptions::default()
            },
        )
        .expect("imported actor fixture must type-check");
        let tco = state
            .typecheck_result
            .tco
            .as_ref()
            .expect("type checking was enabled");
        let hir = hew_hir::lower_program(
            &state.program,
            tco,
            &hew_hir::ResolutionCtx,
            hew_hir::TargetArch::host(),
        );
        assert!(
            hir.diagnostics.is_empty(),
            "imported actor fixture must lower without HIR diagnostics: {:#?}",
            hir.diagnostics
        );
        let symbols = hew_hir::dispatch::build_direct_call_symbol_index(&hir.module.items);
        for name in [
            "hew_testffi_count32",
            "hew_testffi_count64",
            "hew_testffi_name",
            "hew_testffi_query",
        ] {
            let declaration = tco
                .defs
                .lookup_path(&format!("hew.testffi.{name}"))
                .expect("declared extern");
            assert_eq!(
                symbols.get(&declaration),
                Some(&name.to_string()),
                "imported private extern `{declaration:?}` must have a canonical HIR direct-call symbol"
            );
        }
    }

    #[test]
    #[expect(
        clippy::too_many_lines,
        reason = "the import-order regression keeps both declaration-owner permutations in one proof"
    )]
    fn mixed_file_and_package_impls_keep_declaration_owned_dispatch_in_both_import_orders() {
        // A file import shares the root checker declaration namespace, while
        // its emitted HIR body retains the source-file symbol owner. A package
        // import retains a package-qualified owner at both boundaries. The two
        // sources intentionally declare same-leaf `TestResult` / `TestResultMethods`
        // impls. The checker must select the root declaration for `local.tag()`
        // and the package declaration for `r.rows()`; HIR's direct-call index
        // must then project each declaration to its distinct emitted body.
        let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("hew-compile has a workspace parent");
        let fixture_dir = repo_root.join("tests/pkg-import");
        let input = fixture_dir.join("mixed_import_impl_collision.hew");
        let package_path = fixture_dir.join("pkgs");

        let source = fs::read_to_string(&input).expect("read mixed-import fixture");
        let reversed_source = source.replacen(
            "import hew.testffi;\n\nimport \"mixed_import_impl_collision_lib.hew\";",
            "import \"mixed_import_impl_collision_lib.hew\";\n\nimport hew.testffi;",
            1,
        );
        assert_ne!(
            reversed_source, source,
            "fixture imports must be reversed for the second frontend pass"
        );
        let reversed_dir = tempfile::tempdir().expect("create reversed-import fixture dir");
        let reversed_input = write_source(
            reversed_dir.path(),
            "mixed_import_impl_collision.hew",
            &reversed_source,
        );
        fs::copy(
            fixture_dir.join("mixed_import_impl_collision_lib.hew"),
            reversed_dir
                .path()
                .join("mixed_import_impl_collision_lib.hew"),
        )
        .expect("copy mixed-import library");

        for fixture in [
            input.to_str().expect("fixture path is utf-8"),
            reversed_input.as_str(),
        ] {
            let state = run_file_frontend_to_typecheck(
                fixture,
                &FrontendOptions {
                    pkg_path: Some(package_path.clone()),
                    ..FrontendOptions::default()
                },
            )
            .expect("mixed-import fixture must type-check");
            let tco = state
                .typecheck_result
                .tco
                .as_ref()
                .expect("type checking was enabled");
            let root_tag = tco
                .defs
                .lookup_path(
                    "mixed_import_impl_collision_lib.TestResult::<impl \
                     mixed_import_impl_collision_lib.TestResultMethods for \
                     mixed_import_impl_collision_lib.TestResult>::tag",
                )
                .expect("declared root impl method");
            let package_rows = tco
                .defs
                .lookup_path(
                    "hew.testffi.TestResult::<impl hew.testffi.TestResultMethods for hew.testffi.TestResult>::rows",
                )
                .expect("declared package impl method");

            assert_eq!(
                tco.impl_method_declaration_ids.get("TestResult::tag"),
                Some(&root_tag),
                "the flattened file-import implementation must retain a root declaration ID"
            );
            assert_eq!(
                tco.impl_method_declaration_ids
                    .get("hew.testffi.TestResult::rows"),
                Some(&package_rows),
                "the package implementation must retain its package-qualified declaration ID"
            );
            assert!(
                tco.method_call_rewrites.values().any(|rewrite| matches!(
                    rewrite,
                    hew_types::check::MethodCallRewrite::RewriteToFunction {
                        target: hew_types::check::CallTarget::ImplMethod(declaration),
                        ..
                    } if declaration == &root_tag
                )),
                "local TestResult.tag() must select the root file-import declaration: {:#?}",
                tco.method_call_rewrites
            );
            assert!(
                tco.method_call_rewrites.values().any(|rewrite| matches!(
                    rewrite,
                    hew_types::check::MethodCallRewrite::RewriteToFunction {
                        target: hew_types::check::CallTarget::ImplMethod(declaration),
                        ..
                    } if declaration == &package_rows
                )),
                "package TestResult.rows() must select the package declaration: {:#?}",
                tco.method_call_rewrites
            );

            let hir = hew_hir::lower_program(
                &state.program,
                tco,
                &hew_hir::ResolutionCtx,
                hew_hir::TargetArch::host(),
            );
            assert!(
                hir.diagnostics.is_empty(),
                "mixed imports must lower without declaration/body lookup diagnostics: {:#?}",
                hir.diagnostics
            );
            let symbols = hew_hir::dispatch::build_direct_call_symbol_index(&hir.module.items);
            assert_eq!(
                symbols.get(&root_tag),
                Some(&"mixed_import_impl_collision_lib.TestResult::tag".to_string())
            );
            assert_eq!(
                symbols.get(&package_rows),
                Some(&"hew.testffi.TestResult::rows".to_string())
            );
        }
    }

    #[test]
    fn imported_generic_impl_bodies_publish_each_checker_owned_declaration() {
        // `privslot` deliberately combines all three conditions that
        // make a body lookup tempting to recover from a leaf spelling: its
        // module-private generic `Slot<T>` is nested in a public `Store<T>`,
        // and the consumer dispatches two inherent methods after the root body
        // was lowered.  The checker declaration is the sole identity handoff;
        // every emitted impl function must retain that exact declaration and
        // appear in the direct-body index before MIR begins.
        let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("hew-compile has a workspace parent");
        let input = repo_root.join("tests/pkg-import/private_generic_record_vec_element.hew");
        let state = run_file_frontend_to_typecheck(
            input.to_str().expect("fixture path is utf-8"),
            &FrontendOptions {
                pkg_path: Some(repo_root.join("tests/pkg-import/pkgs")),
                ..FrontendOptions::default()
            },
        )
        .expect("private generic-record fixture must type-check");
        let tco = state
            .typecheck_result
            .tco
            .as_ref()
            .expect("type checking was enabled");
        let expected = [
            (
                "hew.privslot.Store::add",
                tco.defs
                    .lookup_path("hew.privslot.Store::<impl inherent for hew.privslot.Store<T>>::add")
                    .expect("declared impl method"),
            ),
            (
                "hew.privslot.Store::generation_at",
                tco.defs
                    .lookup_path(
                        "hew.privslot.Store::<impl inherent for hew.privslot.Store<T>>::generation_at",
                    )
                    .expect("declared impl method"),
            ),
        ];
        for (symbol, declaration) in &expected {
            assert_eq!(
                tco.impl_method_declaration_ids.get(*symbol),
                Some(declaration),
                "checker must publish the canonical full-source symbol key `{symbol}`"
            );
        }

        let hir = hew_hir::lower_program(
            &state.program,
            tco,
            &hew_hir::ResolutionCtx,
            hew_hir::TargetArch::host(),
        );
        assert!(
            hir.diagnostics.is_empty(),
            "generic imported impl methods must lower without body-plan diagnostics: {:#?}",
            hir.diagnostics
        );
        let direct_symbols = hew_hir::dispatch::build_direct_call_symbol_index(&hir.module.items);
        for (symbol, declaration) in &expected {
            assert_eq!(
                direct_symbols.get(declaration),
                Some(&(*symbol).to_string()),
                "every emitted imported impl body must carry its checker-owned declaration"
            );
        }

        let roots = Session::source_roots(&state.program, tco).unwrap();
        let output = Session::new(SessionTarget::native(), DiagnosticPolicy::default())
            .lower_hir_module(&hir.module, tco, &roots)
            .expect("imported generic impl calls must complete shared semantic lowering");
        let module = &output.semantics().module;
        for (_, declaration) in expected {
            let key = hew_sir::SirInstanceKey {
                template: hew_sir::GenericTemplateId { declaration },
                type_args: vec![hew_types::ResolvedTy::String],
            };
            let callable = module
                .callable_for_instance(&key)
                .expect("each imported method must retain its exact string specialization");
            assert!(
                module.function_index().function(callable.id).is_some(),
                "the requested imported specialization must have a semantic body"
            );
        }
    }

    #[test]
    fn local_generic_impl_calls_reuse_exact_semantic_specializations() {
        let dir = tempfile::tempdir().unwrap();
        let input = write_source(
            dir.path(),
            "main.hew",
            r#"type Holder<T> {
    value: T;
}

impl<T> Holder<T> {
    fn get(self) -> T {
        self.value
    }
}

fn main() -> i64 {
    let numbers = Holder { value: 7 };
    let words = Holder { value: "kept" };
    numbers.get() + numbers.get() + words.get().len() + words.get().len()
}
"#,
        );
        let state = run_file_frontend_to_typecheck(&input, &FrontendOptions::default()).unwrap();
        let tco = state.typecheck_result.tco.as_ref().unwrap();
        let declaration = tco.impl_method_declaration_ids["Holder::get"];
        let output = Session::new(SessionTarget::native(), DiagnosticPolicy::default())
            .lower_program(&state.program, tco)
            .expect("local generic impl calls must complete shared semantic lowering");
        let module = &output.semantics().module;
        let index = module.function_index();
        let entry = index.function(module.entry_callable.unwrap()).unwrap();
        assert_eq!(
            module
                .callables
                .iter()
                .filter(|callable| callable.declaration == declaration)
                .count(),
            2,
            "one method declaration must have exactly its two demanded specializations"
        );
        for argument in [hew_types::ResolvedTy::I64, hew_types::ResolvedTy::String] {
            let key = hew_sir::SirInstanceKey {
                template: hew_sir::GenericTemplateId { declaration },
                type_args: vec![argument.clone()],
            };
            let callable = module.callable_for_instance(&key).expect(
                "the instance key must retain the checker declaration and concrete argument",
            );
            assert_eq!(callable.signature.return_ty, argument);
            assert!(
                index.function(callable.id).is_some(),
                "the exact specialization must have a body"
            );
            let calls = entry.blocks.iter().filter(|block| matches!(
                block.terminator, hew_sir::SemTerminator::Call { callee, .. } if callee == callable.id
            )).count();
            assert_eq!(
                calls, 2,
                "repeated source calls must reuse the same semantic callable"
            );
        }
    }

    #[test]
    fn nested_generic_free_calls_keep_exact_direct_symbols_across_all_module_origins() {
        // Every invocation sits in a closure body, which lowers through a child
        // MIR builder.  Exercise all body origins that may be the selected
        // generic declaration: root, file import, package import,
        // and two modules with the same final path component.  The same-leaf
        // pair makes a linker-name or leaf-name recovery observably unsound.
        let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("hew-compile has a workspace parent");
        let dir = tempfile::tempdir().expect("create generic-free-call fixture dir");
        write_source(
            dir.path(),
            "file_helpers.hew",
            "pub fn file_first<T>(xs: [T]) -> T { xs[0] }\n",
        );
        write_source(
            dir.path(),
            "alpha.hew",
            "pub fn first<T>(xs: [T]) -> T { xs[0] }\n",
        );
        fs::create_dir_all(dir.path().join("beta")).expect("create same-leaf module directory");
        write_source(
            dir.path(),
            "beta/alpha.hew",
            "pub fn first<T>(xs: [T]) -> T { xs[0] }\n",
        );
        let input = write_source(
            dir.path(),
            "main.hew",
            r#"
import "file_helpers.hew";
import hew.genhelpers;
import alpha as flat_alpha;
import beta.alpha as nested_alpha;

fn root_first<T>(xs: [T]) -> T { xs[0] }

fn main() {
    let root = || root_first([1, 2]);
    let file = || file_helpers.file_first([3, 4]);
    let imported_pkg = || genhelpers.first([5, 6]);
    let flat = || flat_alpha.first([7, 8]);
    let nested = || nested_alpha.first([9, 10]);
    println(root());
    println(file());
    println(imported_pkg());
    println(flat());
    println(nested());
}
"#,
        );
        let state = run_file_frontend_to_typecheck(
            &input,
            &FrontendOptions {
                pkg_path: Some(repo_root.join("tests/pkg-import/pkgs")),
                ..FrontendOptions::default()
            },
        )
        .expect("generic free-call fixture must type-check");
        let tco = state
            .typecheck_result
            .tco
            .as_ref()
            .expect("type checking was enabled");
        let hir = hew_hir::lower_program(
            &state.program,
            tco,
            &hew_hir::ResolutionCtx,
            hew_hir::TargetArch::host(),
        );
        assert!(
            hir.diagnostics.is_empty(),
            "all generic free-call origins must lower cleanly: {:#?}",
            hir.diagnostics
        );
        let symbols = hew_hir::dispatch::build_direct_call_symbol_index(&hir.module.items);
        // The root unit's declarations carry its own module identity — the
        // entry file's stem — not a bare leaf; only the EMITTED symbol stays
        // bare. Pinning the identity here is what keeps a same-leaf pair from
        // sharing a body symbol.
        let expected = [
            ("main.root_first", "root_first"),
            // A file import is a module: its body symbol is qualified by it.
            ("file_helpers.file_first", "file_helpers$file_first"),
            ("hew.genhelpers.first", "hew$genhelpers$first"),
            ("alpha.first", "alpha$first"),
            ("beta.alpha.first", "beta$alpha$first"),
        ];
        let declared = |path: &str| tco.defs.lookup_path(path).expect("declared");
        for (path, symbol) in expected {
            assert_eq!(
                symbols.get(&declared(path)),
                Some(&symbol.to_string()),
                "generic declaration `{path}` must retain its exact emitted body symbol"
            );
        }
        assert_ne!(
            symbols.get(&declared("alpha.first")),
            symbols.get(&declared("beta.alpha.first")),
            "same-leaf generic functions must not share a direct-call symbol"
        );
    }

    #[test]
    fn self_qualified_module_type_keeps_its_full_owner_through_typed_hir() {
        // The package fixture names Meter both bare and through its own
        // lexical leaf (`selfqualtype.Meter`) while its real owner is the
        // full module-graph path `hew.selfqualtype`. This checks every
        // handoff: checker signature, HIR declaration/parameter, and HIR field access must carry that same exact owner. A short-name fallback would
        // falsely pass the fixture only until a same-leaf package is present.
        let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("hew-compile has a workspace parent");
        let input = repo_root.join("tests/pkg-import/self_qualified_type_identity.hew");
        let state = run_file_frontend_to_typecheck(
            input.to_str().expect("fixture path is utf-8"),
            &FrontendOptions {
                pkg_path: Some(repo_root.join("tests/pkg-import/pkgs")),
                ..FrontendOptions::default()
            },
        )
        .expect("self-qualified package fixture must type-check");
        let tco = state
            .typecheck_result
            .tco
            .as_ref()
            .expect("type checking was enabled");
        let expected = "hew.selfqualtype.Meter";
        assert!(
            matches!(
                tco.sigs()
                    .get("hew.selfqualtype.read")
                    .expect("checker must retain imported read signature")
                    .params
                    .as_slice(),
                [hew_types::CallableParameter { ty: hew_types::Ty::Named { head, .. }, .. }] if head.spelling() == expected
            ),
            "checker parameter type must be the complete module owner: {:#?}",
            tco.sigs().get("hew.selfqualtype.read")
        );

        let hir = hew_hir::lower_program(
            &state.program,
            tco,
            &hew_hir::ResolutionCtx,
            hew_hir::TargetArch::host(),
        );
        assert!(
            hir.diagnostics.is_empty(),
            "self-qualified package fixture must lower without HIR diagnostics: {:#?}",
            hir.diagnostics
        );
        let meter_decl = hir
            .module
            .items
            .iter()
            .find_map(|item| match item {
                hew_hir::HirItem::TypeDecl(decl)
                    if decl.qualified_name(&hir.module.defs) == expected =>
                {
                    Some(decl)
                }
                _ => None,
            })
            .expect("HIR must retain Meter under its full module owner");
        assert!(
            matches!(meter_decl.fields.as_slice(), [field] if field.name == "v" && field.ty == hew_types::ResolvedTy::I64),
            "HIR Meter field must retain its declared shape: {meter_decl:#?}"
        );
        let read = hir
            .module
            .items
            .iter()
            .find_map(|item| match item {
                hew_hir::HirItem::Function(function)
                    if tco.defs.path(function.declaration) == "hew.selfqualtype.read" =>
                {
                    Some(function)
                }
                _ => None,
            })
            .expect("HIR must emit the imported read body");
        assert!(
            matches!(read.params.as_slice(), [param] if param.name == "m" && matches!(&param.ty, hew_types::ResolvedTy::Named { head, .. } if head.registry_key() == expected)),
            "HIR read parameter must retain the full self-qualified owner: {read:#?}"
        );
    }

    #[test]
    fn same_named_actor_replies_use_their_exact_import_owner_in_either_order() {
        // `replysend.Reply` has an i64 field; `replynonsend.Reply` carries
        // Rc<i64>. The actor method signatures spell both replies bare, so the
        // ask Send gate must translate the actor's lexical module binding to
        // the full source owner before marker lookup. Reversing imports proves
        // the result is not a last-writer-wins bare marker row.
        let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("hew-compile has a workspace parent");
        let fixture_dir = repo_root.join("tests/pkg-import");
        let input = fixture_dir.join("samename_reply_reject.hew");
        let source = fs::read_to_string(&input).expect("read same-name reply fixture");
        let reversed = source.replacen(
            "import hew.replysend;\n\nimport hew.replynonsend;",
            "import hew.replynonsend;\n\nimport hew.replysend;",
            1,
        );
        assert_ne!(reversed, source, "fixture must contain both imports");
        let temp = tempfile::tempdir().expect("create reversed reply fixture dir");
        let reversed_input = write_source(temp.path(), "samename_reply_reject.hew", &reversed);

        for fixture in [input.to_string_lossy().into_owned(), reversed_input] {
            let failure = check_file(
                &fixture,
                &FrontendOptions {
                    pkg_path: Some(fixture_dir.join("pkgs")),
                    ..FrontendOptions::default()
                },
            )
            .expect_err("the Rc-backed reply must be rejected at the Send gate");
            let invalid_send: Vec<_> = failure
                .diagnostics
                .iter()
                .filter(|diagnostic| {
                    matches!(
                        &diagnostic.kind,
                        FrontendDiagnosticKind::Type(error)
                            if error.kind == hew_types::error::TypeErrorKind::InvalidSend
                                && error.message.contains("E_DUPLEX_NON_SEND")
                    )
                })
                .collect();
            assert_eq!(
                invalid_send.len(),
                1,
                "only the non-Send reply must fail regardless of import order: {:#?}",
                failure.diagnostics
            );
            assert!(
                failure
                    .diagnostics
                    .iter()
                    .all(|diagnostic| !format!("{diagnostic:#?}").contains("D10 violation")),
                "the checker-owned Send gate must reject before any codegen D10 fallback: {:#?}",
                failure.diagnostics
            );
        }
    }

    #[test]
    fn same_leaf_package_functions_publish_distinct_direct_body_symbols() {
        // `left::render` and `right::render` intentionally share the final
        // module component and the generic free-function leaves
        // `render_value`/`default_value`.  The checker-selected declaration
        // IDs must each project to an emitted HIR body; a linker-name lookup
        // or a partial impl-only projection drops these User calls before MIR.
        let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("hew-compile has a workspace parent");
        let input = repo_root.join("tests/pkg-import/canonical_same_leaf_nested.hew");
        let state = run_file_frontend_to_typecheck(
            input.to_str().expect("fixture path is utf-8"),
            &FrontendOptions {
                pkg_path: Some(repo_root.join("tests/pkg-import/pkgs")),
                ..FrontendOptions::default()
            },
        )
        .expect("same-leaf fixture must type-check");
        let tco = state
            .typecheck_result
            .tco
            .as_ref()
            .expect("type checking was enabled");
        let hir = hew_hir::lower_program(
            &state.program,
            tco,
            &hew_hir::ResolutionCtx,
            hew_hir::TargetArch::host(),
        );
        assert!(
            hir.diagnostics.is_empty(),
            "same-leaf fixture must lower without HIR diagnostics: {:#?}",
            hir.diagnostics
        );
        let symbols = hew_hir::dispatch::build_direct_call_symbol_index(&hir.module.items);
        let ids = [
            "left.render.render_value",
            "right.render.render_value",
            "left.render.default_value",
            "right.render.default_value",
        ]
        .map(|path| tco.defs.lookup_path(path).expect("declared"));
        let projected: Vec<_> = ids
            .iter()
            .map(|id| {
                symbols.get(id).cloned().unwrap_or_else(|| {
                    panic!(
                        "checker declaration `{}` has no emitted HIR body symbol; \n                         render symbols: {:#?}",
                        tco.defs.path(*id),
                        symbols
                            .iter()
                            .filter(|(candidate, _)| tco.defs.path(**candidate).ends_with("render_value")
                                || tco.defs.path(**candidate).ends_with("default_value"))
                            .collect::<Vec<_>>()
                    )
                })
            })
            .collect();
        assert_ne!(projected[0], projected[1]);
        assert_ne!(projected[2], projected[3]);

        // Each trait's `provided` body is materialised for a generic `Box<T>`
        // impl. Its trait-method ID is the static lookup key, but its concrete
        // default-body declaration must drive the monomorphisation and remain
        // distinct for these same-leaf packages.
        let lowered = hew_sir::lower_module(&hir.module, tco);
        assert!(
            hew_sir::verify_module(&lowered.module).is_empty(),
            "{:#?}",
            lowered.statuses
        );
        let defaults: Vec<_> = lowered
            .module
            .callables
            .iter()
            .filter(|callable| {
                tco.defs
                    .path(callable.declaration)
                    .contains("::<default impl ")
                    && tco.defs.path(callable.declaration).ends_with("::provided")
            })
            .collect();
        assert_eq!(
            defaults.len(),
            2,
            "each same-leaf default must have a concrete SIR callable: {defaults:#?}"
        );
        for owner in ["left", "right"] {
            assert!(defaults.iter().any(|callable| tco.defs.path(callable.declaration) == format!("{owner}.render.Box::<default impl {owner}.render.Render for {owner}.render.Box<T>>::provided")));
        }
        assert_ne!(defaults[0].symbol, defaults[1].symbol);
        for callable in defaults {
            let hew_sir::CallableInstance::Generic(key) = &callable.instance else {
                panic!("default requires an exact generic instance");
            };
            let template = lowered
                .module
                .generic_templates
                .iter()
                .find(|template| template.id == key.template)
                .expect("selected generic template");
            assert_eq!(
                callable.symbol,
                hew_hir::monomorph::function_monomorph_symbol(&template.symbol, &key.type_args)
            );
        }
    }

    #[test]
    fn imported_string_length_uses_the_shared_semantic_contract() {
        let dir = tempfile::tempdir().unwrap();
        write_source(
            dir.path(),
            "library.hew",
            "pub fn echo_len(value: string) -> i64 { value.len() }",
        );
        let input = write_source(
            dir.path(),
            "main.hew",
            "import library; fn main() -> i64 { library.echo_len(\"hello\") }",
        );
        let state = run_file_frontend_to_typecheck(&input, &FrontendOptions::default()).unwrap();
        let output = Session::new(SessionTarget::native(), DiagnosticPolicy::default())
            .lower_program(&state.program, state.typecheck_result.tco.as_ref().unwrap())
            .unwrap();
        let module = &output.semantics().module;
        let entry = module
            .function_index()
            .function(module.entry_callable.unwrap())
            .unwrap();
        let callee = entry
            .blocks
            .iter()
            .find_map(|block| match block.terminator {
                hew_sir::SemTerminator::Call { callee, .. } => Some(callee),
                _ => None,
            })
            .expect("entry must call the imported body");
        let body = module.function_index().function(callee).unwrap();
        assert_eq!(module.defs.path(body.declaration), "library.echo_len");
        assert!(
            body.blocks
                .iter()
                .any(|block| matches!(block.terminator, hew_sir::SemTerminator::RtCall { .. })),
            "the imported body must use the verified runtime-call contract"
        );
    }

    #[test]
    fn edition_2026_is_accepted() {
        let dir = tempfile::tempdir().expect("create temp dir");
        write_toml(
            dir.path(),
            "[package]\nname = \"editpkg\"\nedition = \"2026\"\n",
        );
        assert_eq!(
            load_package_name(dir.path()).expect("edition 2026 should load"),
            Some("editpkg".to_string())
        );
    }

    #[test]
    fn missing_edition_defaults_to_current() {
        let dir = tempfile::tempdir().expect("create temp dir");
        write_toml(dir.path(), "[package]\nname = \"defaultpkg\"\n");
        assert_eq!(
            load_package_name(dir.path()).expect("missing edition should default"),
            Some("defaultpkg".to_string())
        );
    }

    #[test]
    fn unsupported_edition_is_rejected() {
        let dir = tempfile::tempdir().expect("create temp dir");
        write_toml(
            dir.path(),
            "[package]\nname = \"futurepkg\"\nedition = \"2027\"\n",
        );
        let err = load_package_name(dir.path()).expect_err("edition 2027 must be rejected");
        assert!(
            err.message.contains("E_UNSUPPORTED_EDITION"),
            "missing structured code: {}",
            err.message
        );
        assert!(
            err.message.contains("2027"),
            "missing edition in message: {}",
            err.message
        );
    }

    #[test]
    fn package_name_no_manifest() {
        let dir = tempfile::tempdir().expect("create temp dir");
        assert_eq!(
            load_package_name(dir.path()).expect("missing manifest should not error"),
            None
        );
    }

    #[test]
    fn manifest_no_deps_returns_some_empty() {
        let dir = tempfile::tempdir().expect("create temp dir");
        write_toml(dir.path(), "[package]\nname = \"foo\"\n");
        let deps = load_dependencies(dir.path())
            .expect("manifest should load")
            .expect("manifest should be present");
        assert!(deps.is_empty());
    }

    #[test]
    fn manifest_with_deps_returns_keys() {
        let dir = tempfile::tempdir().expect("create temp dir");
        write_toml(
            dir.path(),
            "[dependencies]\nstd_utils = \"1.0\"\nmath = \"0.2\"\n",
        );
        let mut deps = load_dependencies(dir.path())
            .expect("manifest should load")
            .expect("manifest should be present");
        deps.sort();
        assert_eq!(deps, vec!["math", "std_utils"]);
    }

    #[test]
    fn manifest_with_table_deps_returns_keys() {
        let dir = tempfile::tempdir().expect("create temp dir");
        // Table / path / feature dependency forms are accepted by the package manager; the
        // compiler must parse them too (it only needs the dependency names).
        write_toml(
            dir.path(),
            "[dependencies]\n\"hew::math::stats\" = { version = \"^0.1.0\" }\nlocal = { version = \"0.1.0\", path = \"../local\" }\nweb = { version = \"1.0\", features = [\"tls\"], optional = true }\n",
        );
        let mut deps = load_dependencies(dir.path())
            .expect("manifest should load")
            .expect("manifest should be present");
        deps.sort();
        assert_eq!(deps, vec!["hew::math::stats", "local", "web"]);
    }

    #[test]
    fn a_path_dependency_needs_no_version() {
        let dir = tempfile::tempdir().expect("create temp dir");
        write_toml(
            dir.path(),
            "[dependencies]\n\"acme.local\" = { path = \"../local\" }\n",
        );
        let deps = load_dependencies(dir.path())
            .expect("manifest should load")
            .expect("manifest should be present");
        assert_eq!(deps, vec!["acme.local"]);
    }

    #[test]
    fn manifest_invalid_toml_returns_err() {
        let dir = tempfile::tempdir().expect("create temp dir");
        write_toml(dir.path(), "this is not valid toml {{{\n");
        let err = load_dependencies(dir.path()).expect_err("invalid manifest should error");
        assert!(err.message.contains("cannot parse"), "{}", err.message);
        assert!(err.message.contains("hew.toml"), "{}", err.message);
    }

    #[test]
    fn no_lockfile_returns_none() {
        let dir = tempfile::tempdir().expect("create temp dir");
        assert!(load_lockfile(dir.path())
            .expect("missing lockfile should not error")
            .is_none());
    }

    #[test]
    fn empty_lockfile_returns_some_empty() {
        let dir = tempfile::tempdir().expect("create temp dir");
        write_lockfile(dir.path(), "# empty\n");
        let entries = load_lockfile(dir.path())
            .expect("lockfile should parse")
            .expect("lockfile should be present");
        assert!(entries.is_empty());
    }

    #[test]
    fn lockfile_with_packages() {
        let dir = tempfile::tempdir().expect("create temp dir");
        write_lockfile(
            dir.path(),
            "[[package]]\nname = \"ecosystem::db::postgres\"\nversion = \"1.0.0\"\n\n\
             [[package]]\nname = \"std::net::http\"\nversion = \"2.1.0\"\n",
        );
        let mut entries = load_lockfile(dir.path())
            .expect("lockfile should parse")
            .expect("lockfile should be present");
        entries.sort();
        assert_eq!(
            entries,
            vec![
                ("ecosystem::db::postgres".to_string(), "1.0.0".to_string()),
                ("std::net::http".to_string(), "2.1.0".to_string()),
            ]
        );
    }

    #[test]
    fn lockfile_ignores_extra_fields() {
        let dir = tempfile::tempdir().expect("create temp dir");
        write_lockfile(
            dir.path(),
            "[[package]]\nname = \"mypkg\"\nversion = \"0.1.0\"\nchecksum = \"sha256:abc\"\n",
        );
        let entries = load_lockfile(dir.path())
            .expect("lockfile should parse")
            .expect("lockfile should be present");
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0], ("mypkg".to_string(), "0.1.0".to_string()));
    }

    #[test]
    fn lockfile_invalid_toml_returns_err() {
        let dir = tempfile::tempdir().expect("create temp dir");
        write_lockfile(dir.path(), "this is not valid toml {{{\n");
        let err = load_lockfile(dir.path()).expect_err("invalid lockfile should error");
        assert!(err.message.contains("cannot parse"), "{}", err.message);
        assert!(err.message.contains("hew.lock"), "{}", err.message);
    }

    #[test]
    fn check_file_fails_closed_on_invalid_manifest() {
        let dir = tempfile::tempdir().expect("create temp dir");
        write_toml(dir.path(), "this is not valid toml {{{\n");
        let input = dir.path().join("main.hew");
        fs::write(&input, "").expect("write main.hew");

        let err = check_file(
            input.to_str().expect("utf-8 path"),
            &FrontendOptions::default(),
        )
        .expect_err("invalid manifest should fail closed");
        assert!(err.message.contains("cannot parse"), "{}", err.message);
        assert!(err.message.contains("hew.toml"), "{}", err.message);
    }

    #[test]
    fn check_file_fails_closed_on_invalid_lockfile() {
        let dir = tempfile::tempdir().expect("create temp dir");
        write_toml(dir.path(), "[package]\nname = \"myapp\"\n");
        write_lockfile(dir.path(), "this is not valid toml {{{\n");
        let input = dir.path().join("main.hew");
        fs::write(&input, "").expect("write main.hew");

        let err = check_file(
            input.to_str().expect("utf-8 path"),
            &FrontendOptions::default(),
        )
        .expect_err("invalid lockfile should fail closed");
        assert!(err.message.contains("cannot parse"), "{}", err.message);
        assert!(err.message.contains("hew.lock"), "{}", err.message);
    }

    #[test]
    fn check_file_preserves_warnings_without_werror() {
        let dir = tempfile::tempdir().expect("create temp dir");
        let input = write_source(dir.path(), "main.hew", "fn main() { let unused = 42; }\n");

        let result = check_file(&input, &FrontendOptions::default()).expect("check should succeed");

        assert!(
            result.diagnostics.iter().any(super::is_warning_diagnostic),
            "expected warning diagnostics, got: {:?}",
            result.diagnostics
        );
    }

    #[test]
    fn check_file_fails_when_warnings_are_errors() {
        let dir = tempfile::tempdir().expect("create temp dir");
        let input = write_source(dir.path(), "main.hew", "fn main() { let unused = 42; }\n");

        let failure = check_file(
            &input,
            &FrontendOptions {
                warnings_as_errors: true,
                ..Default::default()
            },
        )
        .expect_err("warnings should fail when warnings_as_errors is enabled");

        assert_eq!(failure.message, "warnings treated as errors");
        assert!(
            failure.diagnostics.iter().any(super::is_warning_diagnostic),
            "expected warning diagnostics, got: {:?}",
            failure.diagnostics
        );
    }

    #[test]
    fn check_file_rejects_direct_non_floor_intrinsic_source() {
        let dir = tempfile::tempdir().expect("create temp dir");
        let input = write_source(
            dir.path(),
            "math.hew",
            r#"#[intrinsic("math.abs")] pub fn abs<T: Num>(x: T) -> T;"#,
        );

        let failure = check_file(&input, &FrontendOptions::default())
            .expect_err("non-floor direct file must not declare intrinsics");
        assert!(
            failure.diagnostics.iter().any(|diagnostic| matches!(
                &diagnostic.kind,
                FrontendDiagnosticKind::Type(error)
                    if matches!(
                        &error.kind,
                        hew_types::error::TypeErrorKind::IntrinsicOutsideFloor {
                            intrinsic_key,
                            ..
                        } if intrinsic_key == "math.abs"
                    )
            )),
            "expected IntrinsicOutsideFloor for temp math.hew, got: {:?}",
            failure.diagnostics
        );
    }

    #[test]
    #[allow(
        clippy::too_many_lines,
        reason = "this provenance integration test keeps positive and spoofed-source controls together"
    )]
    fn direct_std_stream_provenance_is_exact_to_the_shipped_source() {
        let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("hew-compile lives below the repository root");
        let shipped = repo_root.join("std/stream.hew");
        assert_eq!(
            super::canonical_direct_stdlib_module_for_source(&shipped)
                .map(|module| module.dotted()),
            Some("std.stream".to_string()),
            "direct compilation of the shipped stream module must retain std.stream identity"
        );

        let dir = tempfile::tempdir().expect("create temp dir");
        let user_stream = write_source(
            dir.path(),
            "stream.hew",
            "type Sink<T> {\n    value: T;\n}\n\ntype Stream<T> {\n    value: T;\n}\n",
        );
        assert!(
            super::canonical_direct_stdlib_module_for_source(Path::new(&user_stream)).is_none(),
            "a same-named user file must not acquire compiler-owned std.stream provenance"
        );

        let shipped_net = repo_root.join("std/net/net.hew");
        assert_eq!(
            super::canonical_direct_stdlib_module_for_source(&shipped_net)
                .map(|module| module.dotted()),
            Some("std.net".to_string()),
            "direct compilation of the shipped TCP module must retain std.net identity"
        );

        let shipped_lifecycle = repo_root.join("std/concurrency/lifecycle.hew");
        assert_eq!(
            super::canonical_direct_stdlib_module_for_source(&shipped_lifecycle)
                .map(|module| module.dotted()),
            Some("std.concurrency.lifecycle".to_string()),
            "a direct check of a shipped nested std module must retain its identity"
        );
        fs::create_dir_all(dir.path().join("concurrency")).expect("create user module dir");
        let user_lifecycle = write_source(
            &dir.path().join("concurrency"),
            "lifecycle.hew",
            "pub type Marker {}\n",
        );
        assert!(
            super::canonical_direct_stdlib_module_for_source(Path::new(&user_lifecycle)).is_none(),
            "a same-named user file must not acquire std.concurrency.lifecycle provenance"
        );
        let user_net = write_source(dir.path(), "net.hew", "fn main() {}\n");
        assert!(
            super::canonical_direct_stdlib_module_for_source(Path::new(&user_net)).is_none(),
            "a same-named user file must not acquire compiler-owned std.net provenance"
        );

        let std_net_state = run_file_frontend_to_typecheck(
            shipped_net.to_str().expect("std/net path is UTF-8"),
            &FrontendOptions::default(),
        )
        .expect("the shipped std.net source should type-check directly");
        let std_net_tco = std_net_state
            .typecheck_result
            .tco
            .as_ref()
            .expect("successful std.net check has type output");
        let std_net_hir = hew_hir::lower_program(
            &std_net_state.program,
            std_net_tco,
            &hew_hir::ResolutionCtx,
            hew_hir::TargetArch::host(),
        );
        assert!(
            std_net_hir.diagnostics.is_empty(),
            "the canonical direct std.net graph must retain the typed TCP borrow authority: {:#?}",
            std_net_hir.diagnostics
        );

        let spoof = write_source(
            dir.path(),
            "spoof.hew",
            r#"
#[resource]
#[opaque]
type Foo {}
impl Foo { fn close(consume self) {} }
extern "C" { fn hew_tcp_read(foo: Foo); }
"#,
        );
        let Err(spoof) = run_file_frontend_to_typecheck(&spoof, &FrontendOptions::default()) else {
            panic!("a user Foo must not inherit std.net.Connection's borrow row");
        };
        assert!(
            spoof.diagnostics.iter().any(|diagnostic| matches!(
                &diagnostic.kind,
                FrontendDiagnosticKind::Type(error)
                    if error.kind == hew_types::error::TypeErrorKind::BoundaryResourceMustConsume
                        && error.message.contains("`hew_tcp_read`")
            )),
            "a user Foo must not inherit std.net.Connection's borrow row: {:#?}",
            spoof.diagnostics
        );
    }

    #[test]
    fn root_named_connection_import_survives_transitive_first_registration() {
        let dir = tempfile::tempdir().expect("create temp dir");
        write_source(
            dir.path(),
            "helper.hew",
            "import std.net;\n\npub fn marker() -> i64 { 1 }\n",
        );
        let input = write_source(
            dir.path(),
            "main.hew",
            "import helper;\n\
             import std.net.{Connection};\n\n\
             fn close_connection(consume conn: Connection) { conn.close(); }\n\
             fn main() { let _ = helper.marker(); }\n",
        );

        check_file(&input, &FrontendOptions::default()).expect(
            "the root's named Connection binding and builtin close dispatch must survive when helper registered std::net transitively first",
        );
    }

    /// Two whole-module imports that publish the same source binding in one
    /// scope are genuinely ambiguous and must be rejected. Their canonical
    /// module IDs remain distinct; the conflict is solely the unaliased
    /// `alpha` surface binding both imports would create.
    #[test]
    fn check_file_rejects_ambiguous_unaliased_module_binding() {
        let dir = tempfile::tempdir().expect("create temp dir");
        // Two modules whose short name (last path segment) is the same `alpha`:
        // a flat `alpha.hew` and a nested `beta/alpha.hew`.
        write_source(dir.path(), "alpha.hew", "pub fn val() -> i64 { 1 }\n");
        fs::create_dir_all(dir.path().join("beta")).expect("create beta dir");
        write_source(dir.path(), "beta/alpha.hew", "pub fn val() -> i64 { 2 }\n");
        let input = write_source(
            dir.path(),
            "main.hew",
            "import alpha;\nimport beta.alpha;\n\nfn main() -> i64 { 0 }\n",
        );

        let failure = check_file(&input, &FrontendOptions::default())
            .expect_err("an ambiguous unaliased module binding must fail closed");
        assert!(
            failure.message.contains("ambiguous binding"),
            "expected an ambiguous module-binding diagnostic, got: {}",
            failure.message
        );
        assert!(
            failure.message.contains("alpha"),
            "diagnostic should name the colliding binding `alpha`, got: {}",
            failure.message
        );
    }

    /// Positive control: canonical module IDs may share their final component
    /// when the source gives them distinct whole-module aliases.
    #[test]
    fn check_file_accepts_same_leaf_modules_with_distinct_aliases() {
        let dir = tempfile::tempdir().expect("create temp dir");
        write_source(dir.path(), "alpha.hew", "pub fn val() -> i64 { 1 }\n");
        fs::create_dir_all(dir.path().join("beta")).expect("create beta dir");
        write_source(dir.path(), "beta/alpha.hew", "pub fn val() -> i64 { 2 }\n");
        let input = write_source(
            dir.path(),
            "main.hew",
            "import alpha as flat_alpha;\n\
             import beta.alpha as nested_alpha;\n\n\
             fn main() -> i64 { flat_alpha.val() + nested_alpha.val() }\n",
        );

        check_file(&input, &FrontendOptions::default())
            .expect("distinct aliases for same-leaf canonical modules must be accepted");
    }

    #[test]
    fn package_directory_import_excludes_adjacent_test_files_from_public_surface() {
        let dir = tempfile::tempdir().expect("create temp dir");
        let pkg_root = dir.path().join("packages");
        let sqlite_dir = pkg_root.join("db/sqlite");
        fs::create_dir_all(&sqlite_dir).expect("create package directory");
        write_source(&sqlite_dir, "sqlite.hew", "pub fn marker() -> i64 { 1 }\n");
        write_source(
            &sqlite_dir,
            "sqlite_test.hew",
            "import \"sqlite.hew\";\n\npub fn test_marker() -> i64 { sqlite.marker() }\n",
        );
        let input = write_source(
            dir.path(),
            "main.hew",
            "import hew.db.sqlite;\n\nfn main() -> i64 { sqlite.marker() }\n",
        );

        check_file(
            &input,
            &FrontendOptions {
                pkg_path: Some(pkg_root),
                ..Default::default()
            },
        )
        .expect("package import should ignore adjacent _test.hew imports");
    }

    #[test]
    fn explicit_file_import_of_test_file_still_resolves_relative_imports() {
        let dir = tempfile::tempdir().expect("create temp dir");
        write_source(dir.path(), "sqlite.hew", "pub fn marker() -> i64 { 1 }\n");
        write_source(
            dir.path(),
            "sqlite_test.hew",
            "import \"sqlite.hew\";\n\npub fn test_marker() -> i64 { 1 }\n",
        );
        let input = write_source(
            dir.path(),
            "main.hew",
            "import \"sqlite_test.hew\";\n\nfn main() -> i64 { 0 }\n",
        );

        check_file(&input, &FrontendOptions::default())
            .expect("explicit file import should keep _test.hew semantics");
    }

    #[test]
    fn std_import_does_not_fall_back_to_pkg_path_tail() {
        let dir = tempfile::tempdir().expect("create temp dir");
        let pkg_root = dir.path().join("packages");
        fs::create_dir_all(&pkg_root).expect("create package root");
        write_source(&pkg_root, "bogus.hew", "pub fn marker() -> i64 { 1 }\n");
        let input = write_source(
            dir.path(),
            "main.hew",
            "import std.bogus;\n\nfn main() {}\n",
        );

        let failure = check_file(
            &input,
            &FrontendOptions {
                no_typecheck: true,
                pkg_path: Some(pkg_root.clone()),
                ..Default::default()
            },
        )
        .expect_err("std.bogus must not resolve from --pkg-path/bogus.hew");

        assert!(
            failure.message.contains("module `std.bogus` not found"),
            "expected std.bogus to fail closed, got: {}",
            failure.message
        );
        let stripped_pkg_candidate = pkg_root.join("bogus.hew").display().to_string();
        assert!(
            !failure.message.contains(&stripped_pkg_candidate),
            "std. imports must not try stripped --pkg-path tail candidate `{stripped_pkg_candidate}`: {}",
            failure.message
        );
    }

    #[test]
    fn std_import_does_not_resolve_from_pkg_path_std_root() {
        let dir = tempfile::tempdir().expect("create temp dir");
        let pkg_root = dir.path().join("packages");
        let fake_std_dir = pkg_root.join("std");
        fs::create_dir_all(&fake_std_dir).expect("create fake std package dir");
        write_source(&fake_std_dir, "bogus.hew", "pub fn marker() -> i64 { 1 }\n");
        let input = write_source(
            dir.path(),
            "main.hew",
            "import std.bogus;\n\nfn main() {}\n",
        );

        let failure = check_file(
            &input,
            &FrontendOptions {
                no_typecheck: true,
                pkg_path: Some(pkg_root.clone()),
                ..Default::default()
            },
        )
        .expect_err("std.bogus must not resolve from --pkg-path/std/bogus.hew");

        assert!(
            failure.message.contains("module `std.bogus` not found"),
            "expected std.bogus to fail closed, got: {}",
            failure.message
        );
        let fake_std_candidate = fake_std_dir.join("bogus.hew").display().to_string();
        assert!(
            !failure.message.contains(&fake_std_candidate),
            "std. imports must not try --pkg-path std-root candidate `{fake_std_candidate}`: {}",
            failure.message
        );
    }

    #[test]
    fn std_import_does_not_resolve_from_package_cache_std_root() {
        let dir = tempfile::tempdir().expect("create temp dir");
        let pkg_std_dir = dir.path().join(".hew/packages/std");
        fs::create_dir_all(&pkg_std_dir).expect("create fake .hew std package dir");
        write_source(&pkg_std_dir, "bogus.hew", "pub fn marker() -> i64 { 1 }\n");
        let input = write_source(
            dir.path(),
            "main.hew",
            "import std.bogus;\n\nfn main() {}\n",
        );

        let failure = check_file(
            &input,
            &FrontendOptions {
                no_typecheck: true,
                project_dir: Some(dir.path().to_path_buf()),
                ..Default::default()
            },
        )
        .expect_err("std.bogus must not resolve from .hew/packages/std/bogus.hew");

        assert!(
            failure.message.contains("module `std.bogus` not found"),
            "expected std.bogus to fail closed, got: {}",
            failure.message
        );
        let fake_std_candidate = pkg_std_dir.join("bogus.hew").display().to_string();
        assert!(
            !failure.message.contains(&fake_std_candidate),
            "std. imports must not try .hew std-root candidate `{fake_std_candidate}`: {}",
            failure.message
        );
    }

    #[test]
    fn std_import_prefers_compiler_std_over_pkg_path_tail_collision() {
        let repo_root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("hew-compile lives under repo root");

        let dir = tempfile::tempdir().expect("create temp dir");
        let pkg_root = dir.path().join("packages");
        fs::create_dir_all(&pkg_root).expect("create package root");
        write_source(&pkg_root, "fs.hew", "pub fn marker() -> i64 { 1 }\n");
        let input = write_source(dir.path(), "main.hew", "import std.fs;\n\nfn main() {}\n");

        let (_output, state) = check_file_with_state(
            &input,
            &FrontendOptions {
                no_typecheck: true,
                pkg_path: Some(pkg_root.clone()),
                project_dir: Some(repo_root.to_path_buf()),
                ..Default::default()
            },
        )
        .expect("std::fs must resolve from compiler std without package-tail ambiguity");

        let import = state
            .program
            .items
            .iter()
            .find_map(|item| match &item.0 {
                Item::Import(import) if import.path.to_string() == "std.fs" => Some(import),
                _ => None,
            })
            .expect("std::fs import should remain in the program");
        assert_eq!(
            import.resolved_source_paths.len(),
            1,
            "std::fs should resolve to exactly one source path"
        );
        let resolved = &import.resolved_source_paths[0];
        assert!(
            resolved.ends_with("std/fs.hew"),
            "std::fs should resolve to compiler std/fs.hew, got {}",
            resolved.display()
        );
        assert!(
            !resolved.starts_with(&pkg_root),
            "std::fs must not resolve from colliding --pkg-path file {}",
            resolved.display()
        );
    }

    #[test]
    fn explicit_module_search_paths_do_not_fall_back_to_process_layout() {
        let dir = tempfile::tempdir().expect("create temp dir");
        let stdlib_root = dir.path().join("compiler-resources");
        let stdlib_dir = stdlib_root.join("std");
        fs::create_dir_all(&stdlib_dir).expect("create explicit stdlib root");
        // Supply only the frontend's declared floor; no process-layout source
        // may fill a missing module behind this explicit search root.
        for leaf in hew_types::stdlib_authority::IMPLICIT_PRELUDE_STD_SOURCES
            .iter()
            .map(|(leaf, _)| *leaf)
            .chain(hew_types::stdlib_authority::IMPLICIT_PRELUDE_MODULE_LOADS)
        {
            write_source(
                &stdlib_dir,
                &format!("{leaf}.hew"),
                "// explicit prelude marker\n",
            );
        }
        let expected = Path::new(&write_source(
            &stdlib_dir,
            "fs.hew",
            "pub fn explicit_marker() -> i64 { 1 }\n",
        ))
        .canonicalize()
        .expect("canonical explicit stdlib module");
        let project_dir = dir.path().join("external-project");
        fs::create_dir(&project_dir).expect("create external project");
        let input = write_source(&project_dir, "main.hew", "import std.fs;\n\nfn main() {}\n");

        let (_output, state) = check_file_with_state(
            &input,
            &FrontendOptions {
                no_typecheck: true,
                project_dir: Some(project_dir),
                module_search_paths: Some(vec![stdlib_root]),
                ..Default::default()
            },
        )
        .expect("the explicit stdlib root should resolve independently of cwd");

        let resolved = state
            .program
            .items
            .iter()
            .find_map(|item| match &item.0 {
                Item::Import(import) if import.path.to_string() == "std.fs" => {
                    import.resolved_source_paths.first()
                }
                _ => None,
            })
            .expect("std::fs should resolve from the explicit root");
        assert_eq!(resolved, &expected);
    }

    #[test]
    fn non_builtin_import_still_uses_pkg_path_tail_fallback() {
        let dir = tempfile::tempdir().expect("create temp dir");
        let pkg_root = dir.path().join("packages");
        fs::create_dir_all(&pkg_root).expect("create package root");
        let package_file = Path::new(&write_source(
            &pkg_root,
            "fs.hew",
            "pub fn marker() -> i64 { 1 }\n",
        ))
        .canonicalize()
        .expect("canonical package file");
        let input = write_source(dir.path(), "main.hew", "import mypkg.fs;\n\nfn main() {}\n");

        let (_output, state) = check_file_with_state(
            &input,
            &FrontendOptions {
                no_typecheck: true,
                pkg_path: Some(pkg_root),
                ..Default::default()
            },
        )
        .expect("non-builtin package imports should still use stripped tail fallback");

        let import = state
            .program
            .items
            .iter()
            .find_map(|item| match &item.0 {
                Item::Import(import) if import.path.to_string() == "mypkg.fs" => Some(import),
                _ => None,
            })
            .expect("mypkg::fs import should remain in the program");
        assert_eq!(
            import.resolved_source_paths,
            vec![package_file],
            "mypkg::fs should resolve through --pkg-path/fs.hew"
        );
    }

    #[test]
    fn hew_package_layout_still_uses_explicit_pkg_path_tail_fallback() {
        let dir = tempfile::tempdir().expect("create temp dir");
        let pkg_root = dir.path().join("packages");
        let sqlite_dir = pkg_root.join("db/sqlite");
        fs::create_dir_all(&sqlite_dir).expect("create package directory");
        let package_file = Path::new(&write_source(
            &sqlite_dir,
            "sqlite.hew",
            "pub fn marker() -> i64 { 1 }\n",
        ))
        .canonicalize()
        .expect("canonical package file");
        let input = write_source(
            dir.path(),
            "main.hew",
            "import hew.db.sqlite;\n\nfn main() {}\n",
        );

        let (_output, state) = check_file_with_state(
            &input,
            &FrontendOptions {
                no_typecheck: true,
                pkg_path: Some(pkg_root),
                ..Default::default()
            },
        )
        .expect("hew:: package-layout import should keep using its explicit fallback");

        let import = state
            .program
            .items
            .iter()
            .find_map(|item| match &item.0 {
                Item::Import(import) if import.path.to_string() == "hew.db.sqlite" => Some(import),
                _ => None,
            })
            .expect("hew::db::sqlite import should remain in the program");
        assert_eq!(
            import.resolved_source_paths,
            vec![package_file],
            "hew::db::sqlite should resolve through the explicit hew:: package-layout fallback"
        );
    }

    #[test]
    fn ecosystem_package_layout_still_uses_explicit_pkg_path_tail_fallback() {
        let dir = tempfile::tempdir().expect("create temp dir");
        let pkg_root = dir.path().join("packages");
        let postgres_dir = pkg_root.join("db/postgres");
        fs::create_dir_all(&postgres_dir).expect("create package directory");
        let package_file = Path::new(&write_source(
            &postgres_dir,
            "postgres.hew",
            "pub fn marker() -> i64 { 1 }\n",
        ))
        .canonicalize()
        .expect("canonical package file");
        let input = write_source(
            dir.path(),
            "main.hew",
            "import ecosystem.db.postgres;\n\nfn main() {}\n",
        );

        let (_output, state) = check_file_with_state(
            &input,
            &FrontendOptions {
                no_typecheck: true,
                pkg_path: Some(pkg_root),
                ..Default::default()
            },
        )
        .expect("ecosystem:: package-layout import should keep using its explicit fallback");

        let import = state
            .program
            .items
            .iter()
            .find_map(|item| match &item.0 {
                Item::Import(import) if import.path.to_string() == "ecosystem.db.postgres" => {
                    Some(import)
                }
                _ => None,
            })
            .expect("ecosystem::db::postgres import should remain in the program");
        assert_eq!(
            import.resolved_source_paths,
            vec![package_file],
            "ecosystem::db::postgres should resolve through the explicit ecosystem:: package-layout fallback"
        );
    }

    #[test]
    fn module_import_with_actor_path_segment_resolves() {
        let dir = tempfile::tempdir().expect("create temp dir");
        let actor_dir = dir.path().join("actor");
        fs::create_dir_all(&actor_dir).expect("create actor module dir");
        write_source(&actor_dir, "monitor.hew", "pub fn ping() -> i64 { 1 }\n");
        let input = write_source(
            dir.path(),
            "main.hew",
            "import actor.monitor;\n\nfn main() -> i64 { 0 }\n",
        );

        let (_output, state) = check_file_with_state(
            &input,
            &FrontendOptions {
                no_typecheck: true,
                ..Default::default()
            },
        )
        .expect("actor path segment import should resolve");

        let Item::Import(import) = &state.program.items[0].0 else {
            panic!("expected import item");
        };
        assert_eq!(import.path.to_string(), "actor.monitor");
        assert!(import
            .resolved_items
            .as_ref()
            .is_some_and(|items| !items.is_empty()));
        assert_eq!(import.resolved_source_paths.len(), 1);
    }

    /// Two different modules each exporting a `pub actor` with the same bare
    /// name are LEGAL: actor identity is the qualified (module, name) pair —
    /// `bank.Account` and `store.Account` keep distinct checker entries, MIR
    /// layouts, and native symbols — so the program checks cleanly.
    #[test]
    fn check_file_accepts_duplicate_exported_actor_names_across_modules() {
        let dir = tempfile::tempdir().expect("create temp dir");
        write_source(
            dir.path(),
            "bank.hew",
            "pub actor Account {\n    var n: i64 = 0;\n    receive fn who() -> i64 {\n        1\n    }\n}\n",
        );
        write_source(
            dir.path(),
            "store.hew",
            "pub actor Account {\n    var n: i64 = 0;\n    receive fn who() -> i64 {\n        2\n    }\n}\n",
        );
        let input = write_source(
            dir.path(),
            "main.hew",
            "import bank;\nimport store;\n\nfn main() -> i64 { 0 }\n",
        );

        check_file(&input, &FrontendOptions::default())
            .expect("same-named pub actors from distinct modules must coexist");
    }

    /// A root-local actor sharing a bare name with an imported `pub actor` is
    /// LEGAL: the bare reference resolves local-first to the root actor and
    /// `spawn bank.Account { ... }` routes to the package actor's qualified
    /// layout — neither shadows the other.
    #[test]
    fn check_file_accepts_root_actor_sharing_name_with_imported_actor() {
        let dir = tempfile::tempdir().expect("create temp dir");
        write_source(
            dir.path(),
            "bank.hew",
            "pub actor Account {\n    var n: i64 = 0;\n    receive fn who() -> i64 {\n        1\n    }\n}\n",
        );
        let input = write_source(
            dir.path(),
            "main.hew",
            "import bank;\n\nactor Account {\n    var n: i64 = 0;\n    receive fn who() -> i64 {\n        2\n    }\n}\n\nfn main() -> i64 {\n    0\n}\n",
        );

        check_file(&input, &FrontendOptions::default())
            .expect("root and imported same-named actors must coexist");
    }

    /// One module declaring two same-named actors stays a hard error: both
    /// would claim the same qualified (module, name) identity, and no spawn
    /// spelling could tell them apart.
    #[test]
    fn check_file_rejects_same_module_duplicate_actor_names() {
        let dir = tempfile::tempdir().expect("create temp dir");
        write_source(
            dir.path(),
            "bank.hew",
            "pub actor Account {\n    var n: i64 = 0;\n    receive fn who() -> i64 {\n        1\n    }\n}\n\npub actor Account {\n    var n: i64 = 0;\n    receive fn who() -> i64 {\n        2\n    }\n}\n",
        );
        let input = write_source(
            dir.path(),
            "main.hew",
            "import bank;\n\nfn main() -> i64 { 0 }\n",
        );

        let failure = check_file(&input, &FrontendOptions::default())
            .expect_err("two same-named actors in one module must fail closed");
        assert!(
            failure.message.contains("two actors named `Account`"),
            "expected a same-module duplicate-actor diagnostic, got: {}",
            failure.message
        );
    }

    /// Negative control: two modules exporting actors with DISTINCT bare names
    /// compile cleanly — the duplicate-actor guard must not over-reject.
    #[test]
    fn check_file_accepts_distinct_exported_actor_names() {
        let dir = tempfile::tempdir().expect("create temp dir");
        write_source(
            dir.path(),
            "bank.hew",
            "pub actor Account {\n    var n: i64 = 0;\n    receive fn who() -> i64 {\n        1\n    }\n}\n",
        );
        write_source(
            dir.path(),
            "store.hew",
            "pub actor Register {\n    var n: i64 = 0;\n    receive fn who() -> i64 {\n        2\n    }\n}\n",
        );
        let input = write_source(
            dir.path(),
            "main.hew",
            "import bank;\nimport store;\n\nfn main() -> i64 { 0 }\n",
        );

        check_file(&input, &FrontendOptions::default())
            .expect("distinct exported actor names must be accepted");
    }

    /// Negative control for the file-import happy path: a single `pub actor`
    /// reached via `import "counter.hew"` must NOT be flagged: it lives in
    /// exactly one module.
    #[test]
    fn check_file_accepts_single_file_imported_actor() {
        let dir = tempfile::tempdir().expect("create temp dir");
        write_source(
            dir.path(),
            "counter.hew",
            "pub actor Counter {\n    var n: i64 = 0;\n    receive fn bump() -> i64 {\n        n = n + 1;\n        n\n    }\n}\n",
        );
        let input = write_source(
            dir.path(),
            "main.hew",
            "import \"counter.hew\";\n\nfn main() -> i64 { 0 }\n",
        );

        check_file(&input, &FrontendOptions::default())
            .expect("a single file-imported actor must be accepted");
    }

    /// A *private* (non-pub) imported actor must not be spawnable via its
    /// module qualifier, and in particular `spawn secret.Account` must NOT
    /// silently route to a same-named root actor. The duplicate-actor graph
    /// guard deliberately ignores private actors (they never enter the layout
    /// set), so the fail-closed behaviour here comes from the type checker:
    /// module-qualified spawn is gated on the actor being a `pub` export of the
    /// module (`module_type_exports`), which private actors are excluded from at
    /// registration. Without the gate the qualifier is stripped to bare
    /// `Account` and routes to the root actor -- a privacy and correctness hole.
    #[test]
    fn check_file_rejects_spawn_of_private_imported_actor() {
        let dir = tempfile::tempdir().expect("create temp dir");
        write_source(
            dir.path(),
            "secret.hew",
            // No `pub`: the actor is private to its module.
            "actor Account {\n    var n: i64 = 0;\n    receive fn id() -> i64 {\n        999\n    }\n}\n",
        );
        let input = write_source(
            dir.path(),
            "main.hew",
            "import secret;\n\nactor Account {\n    var n: i64 = 0;\n    receive fn id() -> i64 {\n        111\n    }\n}\n\nfn main() {\n    let a = spawn secret.Account;\n}\n",
        );

        let failure = check_file(&input, &FrontendOptions::default())
            .expect_err("spawn of a private imported actor must fail closed");
        // The detailed diagnostic is a typed error in `diagnostics`; the
        // top-level `message` is the generic "type errors found" summary.
        let has_export_diag = failure.diagnostics.iter().any(|diagnostic| {
            matches!(
                &diagnostic.kind,
                FrontendDiagnosticKind::Type(error)
                    if error.message.contains("has no exported actor or supervisor `Account`")
                        && error.message.contains("secret")
            )
        });
        assert!(
            has_export_diag,
            "expected a fail-closed `has no exported actor or supervisor `Account`` diagnostic \
             naming `secret`, got: {:?}",
            failure.diagnostics
        );
    }

    /// A public *non-actor* type export (e.g. `pub type Account`) must not
    /// satisfy a module-qualified spawn. `module_type_exports` membership is
    /// insufficient -- it also holds public structs/enums/records -- so the
    /// spawn gate requires the qualified definition to be `TypeDefKind::Actor`.
    /// Without that, `spawn secret.Account` would strip the qualifier to bare
    /// `Account` and route to a same-named root actor.
    #[test]
    fn check_file_rejects_spawn_of_non_actor_module_export() {
        let dir = tempfile::tempdir().expect("create temp dir");
        write_source(
            dir.path(),
            "secret.hew",
            // A public NON-actor type that shares the actor's bare name.
            "pub type Account {\n    balance: i64;\n}\n",
        );
        let input = write_source(
            dir.path(),
            "main.hew",
            "import secret;\n\nactor Account {\n    var n: i64 = 0;\n    receive fn id() -> i64 {\n        111\n    }\n}\n\nfn main() {\n    let a = spawn secret.Account;\n}\n",
        );

        let failure = check_file(&input, &FrontendOptions::default())
            .expect_err("spawn of a non-actor module export must fail closed");
        let has_export_diag = failure.diagnostics.iter().any(|diagnostic| {
            matches!(
                &diagnostic.kind,
                FrontendDiagnosticKind::Type(error)
                    if error.message.contains("has no exported actor or supervisor `Account`")
                        && error.message.contains("secret")
            )
        });
        assert!(
            has_export_diag,
            "expected a fail-closed `has no exported actor or supervisor `Account`` diagnostic \
             naming `secret`, got: {:?}",
            failure.diagnostics
        );
    }

    // ── check_program tests ───────────────────────────────────────────────

    #[test]
    fn check_program_no_manifest_accepts_simple_program() {
        let dir = tempfile::tempdir().expect("create temp dir");
        let source = "fn main() { let x: i32 = 1; }\n";
        let program = parse_source(source, "main.hew").expect("source should parse");
        let options = FrontendOptions {
            project_dir: Some(dir.path().to_path_buf()),
            ..Default::default()
        };

        let result = check_program(program, source, "main.hew", &options);
        assert!(result.is_ok(), "valid program should pass: {result:?}");
    }

    #[test]
    fn check_program_rejects_undeclared_dependency() {
        let dir = tempfile::tempdir().expect("create temp dir");
        // Manifest with an empty [dependencies] section — no deps declared.
        write_toml(dir.path(), "[package]\nname = \"myapp\"\n[dependencies]\n");

        // Use a user-space module (no std::/hew::/ecosystem:: prefix) so
        // validate_imports_against_manifest actually checks it.
        let source = "import mylib.utils;\nfn main() {}\n";
        let program = parse_source(source, "main.hew").expect("source should parse");
        let options = FrontendOptions {
            project_dir: Some(dir.path().to_path_buf()),
            ..Default::default()
        };

        let err = check_program(program, source, "main.hew", &options)
            .expect_err("undeclared dep should fail");
        assert!(
            err.message.contains("undeclared"),
            "expected undeclared-dep error, got: {}",
            err.message
        );
    }

    #[test]
    fn check_program_fails_closed_on_invalid_manifest() {
        let dir = tempfile::tempdir().expect("create temp dir");
        write_toml(dir.path(), "this is not valid toml {{{\n");

        let source = "fn main() {}\n";
        let program = parse_source(source, "main.hew").expect("source should parse");
        let options = FrontendOptions {
            project_dir: Some(dir.path().to_path_buf()),
            ..Default::default()
        };

        let err = check_program(program, source, "main.hew", &options)
            .expect_err("invalid manifest should fail closed");
        assert!(err.message.contains("cannot parse"), "{}", err.message);
        assert!(err.message.contains("hew.toml"), "{}", err.message);
    }

    #[test]
    fn check_program_fails_closed_on_invalid_lockfile() {
        let dir = tempfile::tempdir().expect("create temp dir");
        write_toml(dir.path(), "[package]\nname = \"myapp\"\n");
        write_lockfile(dir.path(), "this is not valid toml {{{\n");

        let source = "fn main() {}\n";
        let program = parse_source(source, "main.hew").expect("source should parse");
        let options = FrontendOptions {
            project_dir: Some(dir.path().to_path_buf()),
            ..Default::default()
        };

        let err = check_program(program, source, "main.hew", &options)
            .expect_err("invalid lockfile should fail closed");
        assert!(err.message.contains("cannot parse"), "{}", err.message);
        assert!(err.message.contains("hew.lock"), "{}", err.message);
    }

    #[test]
    fn check_program_catches_type_error() {
        let dir = tempfile::tempdir().expect("create temp dir");
        // No manifest — no import validation.
        let source = "fn main() { let x: i32 = true; }\n";
        let program = parse_source(source, "main.hew").expect("source should parse");
        let options = FrontendOptions {
            project_dir: Some(dir.path().to_path_buf()),
            ..Default::default()
        };

        let err = check_program(program, source, "main.hew", &options)
            .expect_err("type error should fail");
        assert!(
            err.message.contains("type error"),
            "expected type-error message, got: {}",
            err.message
        );
    }

    // Unreachable code after a return statement generates a type Warning.
    const SOURCE_WITH_WARNING: &str = "fn main() { return; let _x: i32 = 1; }\n";

    #[test]
    fn check_program_warnings_as_errors_fails_on_warning() {
        let dir = tempfile::tempdir().expect("create temp dir");
        let source = SOURCE_WITH_WARNING;
        let program = parse_source(source, "main.hew").expect("source should parse");
        let options = FrontendOptions {
            project_dir: Some(dir.path().to_path_buf()),
            warnings_as_errors: true,
            ..Default::default()
        };

        let err = check_program(program, source, "main.hew", &options)
            .expect_err("warnings_as_errors should promote warning to failure");
        assert!(
            err.message.contains("warnings treated as errors"),
            "expected warnings-as-errors message, got: {}",
            err.message
        );
        assert!(
            !err.diagnostics.is_empty(),
            "failure should carry the warning diagnostics"
        );
    }

    #[test]
    fn check_program_warnings_ok_without_flag() {
        let dir = tempfile::tempdir().expect("create temp dir");
        let source = SOURCE_WITH_WARNING;
        let program = parse_source(source, "main.hew").expect("source should parse");
        let options = FrontendOptions {
            project_dir: Some(dir.path().to_path_buf()),
            warnings_as_errors: false,
            ..Default::default()
        };

        // Without the flag, warnings should be collected but not fail the check.
        let output = check_program(program, source, "main.hew", &options)
            .expect("warnings should not fail when flag is off");
        assert!(
            !output.diagnostics.is_empty(),
            "warning diagnostic should still be present in output"
        );
    }

    #[test]
    fn check_file_warnings_as_errors_parity() {
        let dir = tempfile::tempdir().expect("create temp dir");
        let input = dir.path().join("main.hew");
        fs::write(&input, SOURCE_WITH_WARNING).expect("write main.hew");
        let options = FrontendOptions {
            warnings_as_errors: true,
            ..Default::default()
        };

        let err = check_file(input.to_str().expect("utf-8 path"), &options)
            .expect_err("check_file with warnings_as_errors should fail on warning");
        assert!(
            err.message.contains("warnings treated as errors"),
            "expected warnings-as-errors message, got: {}",
            err.message
        );
    }

    /// A directory module's item spans are file-relative byte offsets, so a
    /// diagnostic on a peer-file item must route to THAT file. Two peer files
    /// declaring one C symbol with conflicting signatures: the error names
    /// the peer file (`pkg/aaa.hew`), the note names the minting file
    /// (`pkg/pkg.hew`) — never the peer's span rendered against the primary.
    #[test]
    fn extern_conflict_in_peer_file_routes_to_the_declaring_file() {
        let dir = tempfile::tempdir().expect("create temp dir");
        write_package_manifest(dir.path());
        let pkg_dir = dir.path().join("pkg");
        fs::create_dir(&pkg_dir).expect("create pkg dir");
        let main = write_source(
            dir.path(),
            "main.hew",
            "import pkg;\n\nfn main() {\n    print(\"{pkg.a(\\\"x\\\")}\");\n}\n",
        );
        fs::write(
            pkg_dir.join("pkg.hew"),
            "extern \"C\" {\n    #[extern_symbol(hew_bytes_from_str)]\n    fn alpha(x: string) -> bytes;\n}\n\npub fn a(v: string) -> i64 { unsafe { alpha(v).len() } }\n",
        )
        .expect("write pkg.hew");
        fs::write(
            pkg_dir.join("aaa.hew"),
            "extern \"C\" {\n    #[extern_symbol(hew_bytes_from_str)]\n    fn betaa(x: i64) -> bytes;\n}\n\npub fn b(v: i64) -> i64 { unsafe { betaa(v).len() } }\n",
        )
        .expect("write aaa.hew");

        let err = check_file(&main, &FrontendOptions::default())
            .expect_err("conflicting extern declarations must fail the check");
        let conflict = err
            .diagnostics
            .iter()
            .find(|d| match &d.kind {
                FrontendDiagnosticKind::Type(t) => t.message.contains("conflicting declarations"),
                _ => false,
            })
            .expect("conflict diagnostic present");
        assert!(
            conflict
                .filename
                .as_deref()
                .is_some_and(|f| f.ends_with("aaa.hew")),
            "conflict must route to the declaring peer file, got {:?}",
            conflict.filename
        );
        assert!(
            conflict
                .note_sources
                .first()
                .and_then(|n| n.as_ref())
                .is_some_and(|(_, f)| f.ends_with("pkg.hew")),
            "note must route to the minting file, got {:?}",
            conflict.note_sources.first()
        );
    }

    #[test]
    fn state_constructor_error_in_peer_routes_to_its_source_line() {
        let dir = tempfile::tempdir().expect("create temp dir");
        write_package_manifest(dir.path());
        let workflow_dir = dir.path().join("workflow");
        fs::create_dir(&workflow_dir).expect("create workflow dir");
        let main = write_source(
            dir.path(),
            "main.hew",
            "import workflow.{Workflow};\nfn main() {}\n",
        );
        write_source(
            &workflow_dir,
            "workflow.hew",
            "pub type Marker {\n    value: i64;\n}\n",
        );
        let peer_source = concat!(
            "pub machine Workflow {\n",
            "    events { Crash; }\n",
            "    state Ready;\n",
            "    state Faulted { code: i64; }\n",
            "    on Crash: Ready => .Faulted {\n",
            "        wrong: 1\n",
            "    }\n",
            "    default { state }\n",
            "}\n",
            "\n",
            "pub fn deferred_error() {\n",
            "    let value: Vec<_> = [];\n",
            "}\n",
        );
        write_source(&workflow_dir, "state.hew", peer_source);
        let peer_suffix = Path::new("workflow").join("state.hew");

        let failure = check_file(&main, &FrontendOptions::default())
            .expect_err("the deliberate state-field error must reject the fixture");
        let diagnostic = failure
            .diagnostics
            .iter()
            .find_map(|diagnostic| match &diagnostic.kind {
                FrontendDiagnosticKind::Type(error)
                    if error.message.contains("no field `wrong`") =>
                {
                    Some((diagnostic, error))
                }
                _ => None,
            })
            .expect("state constructor field diagnostic");
        assert!(
            diagnostic
                .0
                .filename
                .as_deref()
                .is_some_and(|filename| Path::new(filename).ends_with(&peer_suffix)),
            "diagnostic must name the declaring peer, got {:?}",
            diagnostic.0.filename
        );
        let line = peer_source[..diagnostic.1.span.start]
            .bytes()
            .filter(|byte| *byte == b'\n')
            .count()
            + 1;
        assert_eq!(line, 5, "diagnostic must point at the deliberate error");

        let inference = failure
            .diagnostics
            .iter()
            .find_map(|diagnostic| match &diagnostic.kind {
                FrontendDiagnosticKind::Type(error)
                    if error.message.contains("cannot infer type") =>
                {
                    Some((diagnostic, error))
                }
                _ => None,
            })
            .expect("deferred inference diagnostic");
        assert!(
            inference
                .0
                .filename
                .as_deref()
                .is_some_and(|filename| Path::new(filename).ends_with(&peer_suffix)),
            "deferred diagnostic must name the declaring peer, got {:?}",
            inference.0.filename
        );
        let inference_line = peer_source[..inference.1.span.start]
            .bytes()
            .filter(|byte| *byte == b'\n')
            .count()
            + 1;
        assert_eq!(
            inference_line, 12,
            "deferred diagnostic must point at the inference error"
        );
    }

    #[test]
    fn hir_diagnostic_routes_to_imported_module_source() {
        let dir = tempfile::tempdir().expect("create temp dir");
        let main = write_source(
            dir.path(),
            "main.hew",
            "import \"dep.hew\";\nfn main() {}\n",
        );
        fs::write(dir.path().join("dep.hew"), "pub fn dep_entry() {}\n").expect("write dep.hew");
        let state = run_file_frontend_to_typecheck(&main, &FrontendOptions::default())
            .expect("frontend should accept fixture");

        fs::remove_file(dir.path().join("dep.hew")).unwrap();
        let diagnostics = hir_diagnostics_to_frontend(
            vec![hew_hir::HirDiagnostic::new(
                hew_hir::HirDiagnosticKind::ConstIntegerEvaluation {
                    class: "arithmetic-overflow".to_string(),
                },
                0..3,
                "probe",
            )
            .with_source_module(Some("dep".to_string()))],
            &state.typecheck_result.sources,
        );

        assert_eq!(diagnostics.len(), 1);
        assert!(
            diagnostics[0]
                .filename
                .as_deref()
                .is_some_and(|filename| filename.ends_with("dep.hew")),
            "expected dep.hew filename, got {:?}",
            diagnostics[0].filename
        );
        assert_eq!(
            diagnostics[0].source.as_deref(),
            Some("pub fn dep_entry() {}\n")
        );
    }

    #[test]
    fn hir_diagnostic_source_map_miss_does_not_fallback_to_root() {
        let source = "fn main() {}\n";
        let program = parse_source(source, "main.hew").expect("source should parse");

        let diagnostics = hir_diagnostics_to_frontend(
            vec![hew_hir::HirDiagnostic::new(
                hew_hir::HirDiagnosticKind::UnresolvedInferenceVar,
                0..2,
                "probe",
            )
            .with_source_module(Some("missing".to_string()))],
            &super::build_module_source_map(&program, source, "main.hew", &SourceSet::new()),
        );

        assert_eq!(diagnostics.len(), 1);
        assert!(diagnostics[0].source.is_none());
        assert!(diagnostics[0].filename.is_none());
        match &diagnostics[0].kind {
            FrontendDiagnosticKind::Hir(diagnostic) => {
                assert_eq!(diagnostic.source_module.as_deref(), Some("missing"));
            }
            other => panic!("expected HIR diagnostic, got {other:?}"),
        }
    }

    /// `std::log` ships `pub const JSON: i64 = 1` and `pub const TEXT: i64 = 0`
    /// in its Hew source layer.  The stdlib registration path routes these through
    /// `register_stdlib_hew_items` registers these constants so `log.JSON` /
    /// `log.TEXT` are available to the type checker.
    ///
    /// This test verifies the real stdlib const resolution works end-to-end: the
    /// source goes through import resolution (which populates `resolved_items` on
    /// the import decl) and type checking (which must find the const in env via
    /// `check_field_access`).  Regression guard for the
    /// `register_stdlib_hew_items` const arm.
    #[test]
    fn stdlib_log_module_consts_resolve() {
        // CARGO_MANIFEST_DIR is `hew-compile/`; the repo root is one level up.
        // That root contains `std/` so the module registry's tier-2 walk finds it.
        let repo_root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("hew-compile lives under repo root");

        let dir = tempfile::tempdir().expect("create temp dir");
        let source = concat!(
            "import std.log;\n",
            "fn main() {\n",
            "    log.set_format(log.JSON);\n",
            "    log.set_format(log.TEXT);\n",
            "    log.info(\"ok\");\n",
            "}\n",
        );
        let input = write_source(dir.path(), "main.hew", source);

        let options = FrontendOptions {
            project_dir: Some(repo_root.to_path_buf()),
            ..Default::default()
        };

        let result = check_file(&input, &options);
        assert!(
            result.is_ok(),
            "log.JSON and log.TEXT should resolve cleanly; got: {:#?}",
            result.err()
        );
    }

    /// Importing `std::fs` and `std::path` together uses a per-module
    /// discriminator in `SpanKey`:
    ///
    /// * Defect A — `hew check`: `unsupported unary - for operand i64 -> string`
    ///   at `std/path.hew:227` (ordinary `return -1;`).  The negation was
    ///   mis-typed as `-> string` because `std/fs.hew` has a string literal at
    ///   the same byte offset as `path.hew`'s negation expression, and both
    ///   shared the same `SpanKey` in the flat `expr_types` map.
    ///
    /// * Defect B — `hew compile`: `Instr::StringLit dest is not a pointer type:
    ///   dest_ty=i64` because the same collision made codegen see an i64 type
    ///   where a pointer-to-string was required.
    ///
    /// The fix adds `module_idx: u32` to `SpanKey` so each non-root module gets
    /// a distinct 1-based index and byte-offset collisions across files are
    /// impossible.
    ///
    /// Regression guard: if this test starts failing, re-examine
    /// `SpanKey::in_module` stamping in the checker and HIR lowering.
    /// Cross-root std resolution: source file is inside a fake Hew checkout root
    /// (has its own `std/builtins.hew` and `std/fs.hew`), while the process cwd
    /// is the real repo root (also a Hew checkout).  Before the fix, the compiler
    /// built a cwd candidate pointing at the repo's `std/fs.hew` AND a Tier-2
    /// candidate pointing at the fake root's `std/fs.hew`, producing two distinct
    /// canonical paths → "import `std::fs` is ambiguous".
    ///
    /// After the fix, `cwd_crosses_root` suppresses the cwd candidates when the
    /// source file's enclosing root differs from the cwd's root → single candidate
    /// from the source file's own root → no ambiguity.
    ///
    /// This is the dogfood repro: `cd <main-checkout> && hew check <worktree>/…`
    #[test]
    fn cross_root_std_import_not_ambiguous() {
        // Cargo sets cwd to the workspace root during nextest, which is a real Hew
        // checkout (contains std/builtins.hew).  Confirm before asserting.
        let cwd = std::env::current_dir().expect("cwd accessible");
        if !cwd.join("std").join("builtins.hew").exists() {
            // Running outside the repo (e.g. in CI with a relocated test binary).
            // Skip rather than fail — the guard logic is still covered by the unit
            // test for find_enclosing_hew_root in hew-types.
            return;
        }

        // Build a second, completely separate fake Hew checkout root in a tempdir.
        let fake_root = tempfile::tempdir().expect("create fake checkout root");
        let std_dir = fake_root.path().join("std");
        fs::create_dir_all(&std_dir).expect("create std dir");
        fs::write(std_dir.join("builtins.hew"), "// fake builtins\n")
            .expect("write fake builtins.hew");
        fs::write(
            std_dir.join("fs.hew"),
            "pub fn read_to_string(path: string) -> Result<string> { ask \"stub\" }\n",
        )
        .expect("write fake fs.hew");

        // Source file lives inside the fake root.
        let src_dir = fake_root.path().join("examples");
        fs::create_dir_all(&src_dir).expect("create examples dir");
        let source = "import std.fs;\n\nfn main() -> i64 { 0 }\n";
        let input = write_source(&src_dir, "prog.hew", source);

        let options = FrontendOptions {
            project_dir: Some(fake_root.path().to_path_buf()),
            ..Default::default()
        };

        // Must not produce an ambiguity error.  The compile may fail for
        // semantic reasons (stub body, NYI, etc.) — but NOT with "is ambiguous".
        let result = check_file(&input, &options);
        let err_str = result
            .as_ref()
            .err()
            .map(|e| format!("{e:?}"))
            .unwrap_or_default();
        assert!(
            !err_str.contains("is ambiguous"),
            "cross-root std::fs import must not be ambiguous; cwd={} fake_root={}: {err_str}",
            cwd.display(),
            fake_root.path().display(),
        );
    }

    /// Gap regression: source is inside a Hew root, but the process cwd is
    /// OUTSIDE any Hew root yet contains its own `std/fs.hew`.
    ///
    /// Before the widened guard, `cwd_hew_root = None` caused the old
    /// `(Some(sr), Some(cr)) if sr != cr` match to fail, so the cwd candidates
    /// were added on top of the Tier-2 candidates from the source root →
    /// "import `std::fs` is ambiguous".
    ///
    /// After the fix the guard is `source_hew_root.is_some() && cwd_hew_root !=
    /// source_hew_root`, where `None != Some(x)` suppresses the cwd candidates
    /// → single candidate from the source root → no ambiguity.
    #[test]
    fn source_in_root_cwd_outside_any_root_with_std_not_ambiguous() {
        // Build a fake Hew checkout root that is the source's home.
        let fake_root = tempfile::tempdir().expect("create fake checkout root");
        let std_dir = fake_root.path().join("std");
        fs::create_dir_all(&std_dir).expect("create std dir");
        fs::write(std_dir.join("builtins.hew"), "// fake builtins\n")
            .expect("write fake builtins.hew");
        fs::write(
            std_dir.join("fs.hew"),
            "pub fn read_to_string(path: string) -> Result<string> { ask \"stub\" }\n",
        )
        .expect("write fake fs.hew");

        // Source file lives inside the fake root.
        let src_dir = fake_root.path().join("examples");
        fs::create_dir_all(&src_dir).expect("create examples dir");
        let source = "import std.fs;\n\nfn main() -> i64 { 0 }\n";
        let input = write_source(&src_dir, "prog.hew", source);

        // A separate tempdir that also contains std/fs.hew but is NOT a Hew
        // root (no builtins.hew, so find_enclosing_hew_root returns None).
        let outside_dir = tempfile::tempdir().expect("create outside dir");
        let outside_std = outside_dir.path().join("std");
        fs::create_dir_all(&outside_std).expect("create outside std dir");
        fs::write(
            outside_std.join("fs.hew"),
            "// outside-root stray std/fs.hew\n",
        )
        .expect("write outside fs.hew");

        // Override cwd via FrontendOptions so the test does not depend on the
        // real process cwd (which varies by runner).
        let options = FrontendOptions {
            project_dir: Some(fake_root.path().to_path_buf()),
            // Pass the outside dir as the working directory for resolution.
            // The resolver reads std::env::current_dir() directly, so we set
            // the process cwd for the duration of this test.
            ..Default::default()
        };

        // We cannot safely change the process cwd (not thread-safe), so we
        // instead verify the guard logic directly: construct the inputs that
        // the resolver would see and assert the correct outcome.
        //
        // The source file path IS inside fake_root (find_enclosing_hew_root →
        // Some(fake_root)), while outside_dir has no builtins.hew
        // (find_enclosing_hew_root → None).  With the widened guard,
        // None != Some(fake_root) → cwd_crosses_root = true → suppress cwd.
        let source_root =
            hew_types::module_registry::find_enclosing_hew_root(std::path::Path::new(&input));
        let outside_root = hew_types::module_registry::find_enclosing_hew_root(outside_dir.path());
        assert!(
            source_root.is_some(),
            "source file must be detected inside a Hew root"
        );
        assert!(
            outside_root.is_none(),
            "outside dir must NOT be detected as a Hew root (no builtins.hew)"
        );
        // The widened predicate: source has root, cwd root differs (None ≠ Some)
        // → cwd candidates suppressed.
        let cwd_crosses_root = source_root.is_some() && outside_root != source_root;
        assert!(
            cwd_crosses_root,
            "guard must fire when source has a root and cwd has none"
        );

        // Also verify end-to-end: compile does NOT produce ambiguity.
        let result = check_file(&input, &options);
        let err_str = result
            .as_ref()
            .err()
            .map(|e| format!("{e:?}"))
            .unwrap_or_default();
        assert!(
            !err_str.contains("is ambiguous"),
            "source-in-root cwd-outside-root std::fs import must not be ambiguous: {err_str}"
        );
    }

    #[test]
    fn cross_module_span_key_collision_unary_minus_and_string_lit_do_not_collide() {
        let repo_root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("hew-compile lives under repo root");

        let dir = tempfile::tempdir().expect("create temp dir");
        // Import both std::fs and std::path — the two modules whose functions
        // have byte-offset-colliding sub-expressions of different types.
        // A plain function call exercises path resolution without needing
        // full stdlib ABI support for the imported functions.
        let source = concat!(
            "import std.path;\n",
            "import std.fs;\n",
            "\n",
            "fn main() -> i64 { 0 }\n",
        );
        let input = write_source(dir.path(), "main.hew", source);

        let options = FrontendOptions {
            project_dir: Some(repo_root.to_path_buf()),
            ..Default::default()
        };

        let result = check_file(&input, &options);
        assert!(
            result.is_ok(),
            "importing std::path and std::fs together must not produce \
             cross-module SpanKey collisions; got: {:#?}",
            result.err()
        );
    }

    #[test]
    fn imported_machine_step_signature_keeps_its_event_declaration() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
        let source = root.join("tests/core-acceptance/cases/machine-import-values.hew");
        let state = run_document_frontend(source.to_str().unwrap(), &FrontendOptions::default());
        let checked = state
            .typecheck_result
            .as_ref()
            .unwrap()
            .tco
            .as_ref()
            .unwrap();
        assert!(state.stopped.is_none(), "{:?}", state.stopped);
        let mut event_owners = std::collections::HashSet::new();
        for sig in checked.fn_sigs.values() {
            let Some(method) = &sig.impl_method else {
                continue;
            };
            if method.name.as_str() != "step"
                || method
                    .receiver
                    .is_none_or(|id| checked.defs.name(id).as_str() != "Gate")
            {
                continue;
            }
            let Some(hew_types::CallableParameter {
                ty: hew_types::Ty::Named { head, .. },
                ..
            }) = sig.params.first()
            else {
                continue;
            };
            let event = head
                .declaration(&checked.defs)
                .expect("resolved step event");
            assert_eq!(checked.defs.owner(event.declaration()), method.receiver);
            event_owners.insert(event);
        }
        assert_eq!(
            event_owners.len(),
            2,
            "the two Gate declarations keep separate events"
        );
    }

    #[test]
    fn imported_function_can_spawn_and_wire_module_actors() {
        let dir = tempfile::tempdir().expect("create temp project");
        write_source(
            dir.path(),
            "actor_factory.hew",
            include_str!("../../tests/core-acceptance/cases/pipeline_support/actors.hew"),
        );
        let input = write_source(
            dir.path(),
            "main.hew",
            "import actor_factory;\nfn main() {\n    let source = actor_factory.run();\n    source.push(21, \"probe\").expect(\"push\");\n}\n",
        );
        let state = run_file_frontend_to_typecheck(
            &input,
            &FrontendOptions {
                project_dir: Some(dir.path().to_path_buf()),
                ..FrontendOptions::default()
            },
        )
        .unwrap_or_else(|failure| panic!("frontend failed: {failure:#?}"));
        let typecheck = state
            .typecheck_result
            .tco
            .as_ref()
            .expect("fixture must typecheck");
        let hir = hew_hir::lower_program(
            &state.program,
            typecheck,
            &hew_hir::ResolutionCtx,
            hew_hir::TargetArch::host(),
        );
        assert!(
            hir.diagnostics.is_empty(),
            "imported actor bodies must lower: {:#?}",
            hir.diagnostics
        );
        for actor in ["actor_factory.Stage", "actor_factory.Source"] {
            assert!(
                hir.module.items.iter().any(|item| matches!(
                    item, hew_hir::HirItem::Actor(decl) if hir.module.defs.path(decl.declaration) == actor
                )),
                "missing imported actor declaration `{actor}`"
            );
        }
    }

    /// A cycle between two files in the SAME directory renders one positioned
    /// location per import on the path (the header pointing at the first
    /// edge, a note per remaining edge, in path order) and steers the fix
    /// toward the directory-module form.
    #[test]
    fn import_cycle_in_same_directory_renders_positions_and_directory_help() {
        let dir = tempfile::tempdir().expect("create cycle fixture");
        let input = write_source(
            dir.path(),
            "a.hew",
            "import \"b.hew\";\npub fn noop_a() {}\n",
        );
        write_source(
            dir.path(),
            "b.hew",
            "import \"a.hew\";\npub fn noop_b() {}\n",
        );

        let failure =
            check_file(&input, &FrontendOptions::default()).expect_err("cycle must be rejected");
        assert_eq!(failure.diagnostics.len(), 1);
        let FrontendDiagnosticKind::Message(inner) = &failure.diagnostics[0].kind else {
            panic!(
                "expected a Message diagnostic, got {:?}",
                failure.diagnostics[0].kind
            );
        };

        assert_eq!(inner.code, "E_IMPORT_CYCLE");
        // Primary location: the first edge, at `a.hew`'s `import "b.hew";`.
        assert!(crate::paths_name_same_file(
            Path::new(
                failure.diagnostics[0]
                    .filename
                    .as_deref()
                    .expect("cycle source filename")
            ),
            Path::new(&input),
        ));
        let primary_span = inner
            .span
            .clone()
            .expect("cycle diagnostic must carry a span");
        let primary_source = inner
            .source
            .as_deref()
            .expect("cycle diagnostic must carry source");
        assert_eq!(primary_source[primary_span].trim_end(), "import \"b.hew\";");
        assert!(
            inner.message.contains('`') && inner.message.contains("imports"),
            "primary message should label the edge it introduces: {}",
            inner.message
        );

        // One note for the closing edge, in `b.hew`, labelled as closing the cycle.
        assert_eq!(inner.notes.len(), 1);
        assert!(inner.notes[0].filename.ends_with("b.hew"));
        assert_eq!(
            inner.notes[0].source[inner.notes[0].span.clone()].trim_end(),
            "import \"a.hew\";"
        );
        assert!(
            inner.notes[0].message.contains("closing the cycle"),
            "closing edge should say so: {}",
            inner.notes[0].message
        );

        assert_eq!(inner.help.len(), 1);
        assert!(
            inner.help[0].contains("share one directory")
                && inner.help[0].contains("spec 3.5.1")
                && inner.help[0].contains("hew init"),
            "a loose same-directory cycle should recommend a package's directory-module form: {}",
            inner.help[0]
        );
    }

    /// A cycle spanning two DIFFERENT directories recommends moving the
    /// shared declarations into a module both sides import instead.
    #[test]
    fn import_cycle_across_directories_recommends_a_shared_module() {
        let dir = tempfile::tempdir().expect("create cross-directory cycle fixture");
        let near = dir.path().join("near");
        let far = dir.path().join("far");
        fs::create_dir(&near).expect("create near directory");
        fs::create_dir(&far).expect("create far directory");
        let input = write_source(
            &near,
            "a.hew",
            "import \"../far/b.hew\";\npub fn noop_a() {}\n",
        );
        write_source(
            &far,
            "b.hew",
            "import \"../near/a.hew\";\npub fn noop_b() {}\n",
        );

        let failure = check_file(&input, &FrontendOptions::default())
            .expect_err("cross-directory cycle must be rejected");
        let FrontendDiagnosticKind::Message(inner) = &failure.diagnostics[0].kind else {
            panic!(
                "expected a Message diagnostic, got {:?}",
                failure.diagnostics[0].kind
            );
        };

        assert_eq!(inner.code, "E_IMPORT_CYCLE");
        assert_eq!(inner.help.len(), 1);
        assert_eq!(
            inner.help[0],
            "move the shared declarations into a module both sides import"
        );
    }
}

#[cfg(test)]
mod source_analysis_batch_tests {
    use super::*;
    use std::fs;

    fn test_options() -> FrontendOptions {
        FrontendOptions {
            module_search_paths: Some(vec![PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                .parent()
                .unwrap()
                .to_path_buf()]),
            ..FrontendOptions::default()
        }
    }

    #[test]
    fn repeated_requests_refresh_disk_dependencies_and_retained_sources() {
        let dir = tempfile::tempdir().unwrap();
        let dep = dir.path().join("greeting.hew");
        let label = dir.path().join("consumer.hew");
        let label = label.to_str().unwrap();
        let old = "pub fn greet() -> i32 { 7 }";
        fs::write(&dep, old).unwrap();
        let options = test_options();
        let mut batch = SourceAnalysisBatch::new(options.clone());
        let source = "import greeting; fn consumer() -> i32 { greeting.greet() }";
        let first = assert_equivalent(source, label, &options, &mut batch);
        assert!(first.stopped.is_none(), "{:?}", first.diagnostics);
        let retained = &first.typecheck_result.as_ref().unwrap().sources;
        fs::write(&dep, "pub fn greet() -> string { \"changed\" }").unwrap();
        let changed = assert_equivalent(source, label, &options, &mut batch);
        assert!(
            changed.stopped.is_some(),
            "updated dependency must reject the integer caller"
        );
        assert_eq!(batch.dependency_bootstraps(), 2);
        fs::remove_file(&dep).unwrap();
        let deleted = run_source_frontend(source, label, &options);
        assert!(deleted.stopped.is_some());
        let (text, _) = retained.source("greeting").unwrap();
        assert_eq!(&*text, old);
        assert!(retained.documents.is_file(&dep));
        assert_eq!(&*retained.documents.read_text(&dep).unwrap(), old);

        let error = hew_types::TypeError::new(
            hew_types::error::TypeErrorKind::InvalidSend,
            0..3,
            "retained location",
        )
        .with_note_source(0..3, "dependency declaration", Some("greeting".into()));
        let diagnostic = type_diagnostic_to_frontend(error, retained);
        assert_eq!(diagnostic.source.as_deref(), Some(source));
        assert_eq!(&*diagnostic.note_sources[0].as_ref().unwrap().0, old);
        let diagnostic = ownership_diagnostics_to_frontend(
            vec![hew_sir::SirDiagnostic {
                function: "greet".into(),
                callable: None,
                kind: hew_sir::SirDiagnosticKind::UnconsumedLinear {
                    binding: Some("value".into()),
                    span: 0..3,
                    source_origin: hew_sir::FunctionSourceOrigin::Foreign("greeting".into()),
                },
            }],
            retained,
        );
        let FrontendDiagnosticKind::Message(message) = &diagnostic[0].kind else {
            panic!("expected positioned ownership message");
        };
        assert_eq!(message.source.as_deref(), Some(old));
    }

    #[test]
    fn source_epoch_keeps_binary_inputs_until_the_next_request() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("included.txt");
        fs::write(&path, b"original").unwrap();
        let options = SourceSet::new();
        let admitted = options.for_frontend();
        assert_eq!(admitted.read_bytes(&path).unwrap(), b"original");
        fs::write(&path, b"changed").unwrap();
        assert_eq!(&*admitted.read_text(&path).unwrap(), "original");
        let fresh = options.for_frontend();
        assert_eq!(&*fresh.read_text(&path).unwrap(), "changed");
        assert_ne!(admitted.inputs()[0].sha256, fresh.inputs()[0].sha256);
        fs::remove_file(&path).unwrap();
        assert_eq!(admitted.read_bytes(&path).unwrap(), b"original");
        assert!(!options.for_frontend().is_file(&path));

        fs::write(&path, [0xff, 0x00]).unwrap();
        let binary = options.for_frontend();
        assert_eq!(binary.read_bytes(&path).unwrap(), [0xff, 0x00]);
        fs::remove_file(&path).unwrap();
        assert!(binary.is_file(&path));
        assert_eq!(
            binary.read_text(&path).unwrap_err().kind(),
            std::io::ErrorKind::InvalidData
        );
        assert!(binary.admitted_text(&path).is_none());
        let snapshot = SourceSnapshot {
            root_source: "fn main() {}".into(),
            root_filename: "main.hew".into(),
            modules: HashMap::new(),
            documents: binary,
        };
        let diagnostic = type_diagnostic_to_frontend(
            hew_types::TypeError::new(
                hew_types::error::TypeErrorKind::InvalidOperation,
                0..1,
                "invalid embedded data",
            )
            .with_source_input(&path),
            &snapshot,
        );
        assert_eq!(
            diagnostic.filename.as_deref(),
            Some(display_path(&path).as_str())
        );
        assert_eq!(diagnostic.source.as_deref(), Some("\u{fffd}\0"));
        assert_eq!(snapshot.documents.read_bytes(&path).unwrap(), [0xff, 0x00]);
    }

    fn identity(output: &hew_types::TypeCheckOutput, id: hew_types::DefId) -> String {
        let site = output.defs.site(id);
        let path = output
            .defs
            .module(id)
            .and_then(|module| output.defs.module_source(module));
        format!(
            "{path:?}:{:?}:{:?}:{:?}:{}",
            site.map(hew_types::DeclarationOccurrence::span),
            site.map(hew_types::DeclarationOccurrence::kind),
            site.map(hew_types::DeclarationOccurrence::ordinal),
            output.defs.name(id)
        )
    }

    fn physical_declarations(output: &hew_types::TypeCheckOutput) -> Vec<String> {
        let mut declarations = output
            .defs
            .declarations()
            .filter_map(|(site, id)| {
                output
                    .defs
                    .module_source(site.module()?)
                    .map(|_| identity(output, id))
            })
            .collect::<Vec<_>>();
        declarations.sort();
        declarations
    }

    fn checked_resolutions(output: &hew_types::TypeCheckOutput) -> Vec<String> {
        use hew_types::check::scope::Resolution;
        let mut local_sites = HashMap::new();
        for (span, resolution) in &output.resolutions {
            if let Resolution::Local(binding) = resolution {
                let span = format!("{span:?}");
                local_sites
                    .entry(*binding)
                    .and_modify(|previous: &mut String| {
                        if span < *previous {
                            previous.clone_from(&span);
                        }
                    })
                    .or_insert(span);
            }
        }
        let mut resolutions = output
            .resolutions
            .iter()
            .map(|(span, resolution)| {
                let target = match resolution {
                    Resolution::Def(id) => format!("def:{}", identity(output, *id)),
                    Resolution::Member(id) => format!("member:{}", identity(output, *id)),
                    Resolution::Nominal(id) => {
                        format!("nominal:{}", identity(output, id.declaration()))
                    }
                    Resolution::Param(id) => {
                        format!("param:{}:{}", identity(output, id.owner), id.index)
                    }
                    Resolution::Field(id, index) => {
                        format!("field:{}:{index}", identity(output, id.declaration()))
                    }
                    Resolution::Variant(head, index) => {
                        let owner = head.declaration(&output.defs).map_or_else(
                            || format!("builtin:{:?}", head.builtin()),
                            |id| identity(output, id.declaration()),
                        );
                        format!("variant:{owner}:{index}")
                    }
                    Resolution::Local(id) => format!("local:{}", local_sites[id]),
                    Resolution::Module(id) => format!(
                        "module:{:?}:{}",
                        output.defs.module_source(*id),
                        output.defs.module_path(*id)
                    ),
                    Resolution::Builtin(kind) => format!("builtin:{kind:?}"),
                };
                format!("{span:?}:{target}")
            })
            .collect::<Vec<_>>();
        resolutions.sort();
        resolutions
    }

    fn effect_identity(
        output: &hew_types::TypeCheckOutput,
        body: &hew_types::ty::EffectBody,
    ) -> String {
        use hew_types::ty::EffectBody;
        match body {
            EffectBody::Declaration(id) => format!("declaration:{}", identity(output, *id)),
            EffectBody::Generator(id) => format!("generator:{}", identity(output, *id)),
            EffectBody::GeneratorBlock(span) => format!("generator:{span:?}"),
            EffectBody::Closure(span) => format!("closure:{span:?}"),
        }
    }

    fn checked_type(output: &hew_types::TypeCheckOutput, ty: &hew_types::Ty) -> String {
        use hew_types::ty::TypeHead;
        use hew_types::Ty;
        match ty {
            Ty::Named { head, args } => {
                let head = match head {
                    TypeHead::Nominal(head) => {
                        format!("nominal:{}", identity(output, head.id.declaration()))
                    }
                    TypeHead::Actor(head) => {
                        format!("actor:{}", identity(output, head.id.declaration()))
                    }
                    TypeHead::Param(head) => format!(
                        "param:{}:{}",
                        identity(output, head.id.owner),
                        head.id.index
                    ),
                    TypeHead::Builtin(kind) => format!("builtin:{kind:?}"),
                    TypeHead::Unresolved(name) => format!("unresolved:{name}"),
                };
                format!(
                    "{head}<{:?}>",
                    args.iter()
                        .map(|ty| checked_type(output, ty))
                        .collect::<Vec<_>>()
                )
            }
            Ty::Function {
                capabilities,
                params,
                ret,
            } => format!(
                "function:{capabilities:?}:{:?}:{}",
                params
                    .iter()
                    .map(|parameter| (parameter.ownership, checked_type(output, &parameter.ty)))
                    .collect::<Vec<_>>(),
                checked_type(output, ret)
            ),
            Ty::Closure {
                capabilities,
                params,
                ret,
                captures,
                identity: body,
            } => format!(
                "closure:{capabilities:?}:{:?}:{}:{:?}:{}",
                params
                    .iter()
                    .map(|parameter| (parameter.ownership, checked_type(output, &parameter.ty)))
                    .collect::<Vec<_>>(),
                checked_type(output, ret),
                captures
                    .iter()
                    .map(|ty| checked_type(output, ty))
                    .collect::<Vec<_>>(),
                effect_identity(output, body)
            ),
            Ty::Var(_) => "inference-variable".to_string(),
            _ => ty.to_string(),
        }
    }

    fn finalized_facts(output: &hew_types::TypeCheckOutput) -> Vec<String> {
        let mut facts = output
            .expr_types
            .iter()
            .map(|(span, ty)| format!("expr:{span:?}:{}", checked_type(output, ty)))
            .collect::<Vec<_>>();
        facts.extend(output.fn_sigs.iter().map(|(id, signature)| {
            format!(
                "signature:{}:{:?}:{:?}:{}",
                identity(output, *id),
                signature.param_names,
                signature
                    .params
                    .iter()
                    .map(|parameter| (parameter.ownership, checked_type(output, &parameter.ty)))
                    .collect::<Vec<_>>(),
                checked_type(output, &signature.return_type)
            )
        }));
        facts.extend(
            output
                .suspension_effects
                .bodies
                .iter()
                .map(|(body, effect)| {
                    format!("body-effect:{}:{effect:?}", effect_identity(output, body))
                }),
        );
        facts.extend(
            output
                .suspension_effects
                .calls
                .iter()
                .map(|(span, effect)| format!("call-effect:{span:?}:{effect:?}")),
        );
        facts.extend(
            output
                .suspension_effects
                .fork_transfers
                .iter()
                .map(|(span, transfer)| format!("fork:{span:?}:{transfer:?}")),
        );
        facts.extend(
            output
                .closure_escape_facts
                .iter()
                .map(|(span, escape)| format!("escape:{span:?}:{escape:?}")),
        );
        facts.sort();
        facts
    }

    fn assert_equivalent(
        source: &str,
        label: &str,
        options: &FrontendOptions,
        batch: &mut SourceAnalysisBatch,
    ) -> DocumentFrontendState {
        let fresh = run_source_frontend(source, label, options);
        let shared = batch.run_source_frontend(source, label);
        assert_eq!(fresh.stopped.is_some(), shared.stopped.is_some());
        assert_eq!(
            format!("{:?}", fresh.diagnostics),
            format!("{:?}", shared.diagnostics)
        );
        let fresh = fresh
            .typecheck_result
            .as_ref()
            .and_then(|result| result.tco.as_ref())
            .unwrap_or_else(|| panic!("frontend did not reach checking: {:?}", fresh.diagnostics));
        let checked = shared
            .typecheck_result
            .as_ref()
            .and_then(|result| result.tco.as_ref())
            .unwrap_or_else(|| {
                panic!(
                    "shared frontend did not reach checking: {:?}",
                    shared.diagnostics
                )
            });
        assert_eq!(physical_declarations(fresh), physical_declarations(checked));
        assert_eq!(checked_resolutions(fresh), checked_resolutions(checked));
        assert_eq!(finalized_facts(fresh), finalized_facts(checked));
        shared
    }

    #[test]
    fn invalid_dependencies_bootstrap_once_and_keep_complete_diagnostics() {
        let dir = tempfile::tempdir().unwrap();
        fs::write(
            dir.path().join("greeting.hew"),
            "pub fn identity<T>(value: T) -> T { value + true }",
        )
        .unwrap();
        let options = test_options();
        let mut batch = SourceAnalysisBatch::new(options.clone());
        // Keep the same dependency traversal order for this exact-snapshot
        // memoization check. Roots on opposite sides of std's lexical position
        // can select a different valid non-root order and require another seed.
        for (name, source) in [
            (
                "consumer_integer.hew",
                "import greeting; fn main() -> i64 { greeting.identity(1) }",
            ),
            (
                "consumer_string.hew",
                "import greeting; fn main() -> string { greeting.identity(\"one\") }",
            ),
        ] {
            let label = dir.path().join(name);
            let state = assert_equivalent(source, label.to_str().unwrap(), &options, &mut batch);
            assert!(state.stopped.is_some());
        }
        assert_eq!(batch.dependency_bootstraps(), 1);
        assert_eq!(batch.reused_roots(), 0);
    }

    #[test]
    fn directory_generated_displays_keep_final_file_indices() {
        let dir = tempfile::tempdir().unwrap();
        fs::write(dir.path().join("hew.toml"), "[package]\nname = \"app\"\n").unwrap();
        let catalog = dir.path().join("catalog");
        fs::create_dir(&catalog).unwrap();
        fs::write(
            catalog.join("catalog.hew"),
            "pub fn first(value: i64 = 11) -> i64 { value }",
        )
        .unwrap();
        fs::write(
            catalog.join("other.hew"),
            "pub fn other(value: i32 = 22) -> i32 { value }",
        )
        .unwrap();
        let errors = catalog.join("errors.hew");
        fs::write(&errors, "pub enum Fault { Broken; }\nimpl Error for Fault {}\npub fn message() -> string { f\"{Fault.Broken}\" }").unwrap();
        let errors = errors.canonicalize().unwrap();
        let mut options = test_options();
        options.project_dir = Some(dir.path().to_path_buf());
        let mut batch = SourceAnalysisBatch::new(options.clone());
        let source = "import catalog; fn value() -> i64 { catalog.first() + (catalog.other() as i64) } fn main() { println(catalog.message()); }";
        for name in ["consumer_first.hew", "consumer_second.hew"] {
            let label = dir.path().join(name);
            let state = assert_equivalent(source, label.to_str().unwrap(), &options, &mut batch);
            assert!(state.stopped.is_none(), "{:?}", state.diagnostics);
            let checked = state
                .typecheck_result
                .as_ref()
                .unwrap()
                .tco
                .as_ref()
                .unwrap();
            let normalized = checked
                .normalized_program
                .as_ref()
                .expect("Error generates Display");
            let graph = normalized.program.module_graph.as_ref().unwrap();
            let root_path = graph.modules[&graph.root].source_paths.first().unwrap();
            assert_eq!(checked.file_span_indices.path_index(root_path), Some(0));
            let (module, source_module) = graph
                .modules
                .iter()
                .find(|(_, module)| module.source_paths.contains(&errors))
                .unwrap();
            let error_index = checked.file_span_indices.path_index(&errors).unwrap();
            let error_items: Vec<_> = source_module
                .items
                .iter()
                .enumerate()
                .filter(|(ordinal, _)| graph.item_source(module, *ordinal) == Some(&errors))
                .collect();
            assert!(
                error_items.len() > 3,
                "generated Display stays in the declaring peer"
            );
            for (ordinal, _) in error_items {
                assert_eq!(
                    checked.file_span_indices.item_index(module, ordinal),
                    Some(error_index)
                );
            }
            let lowered = hew_hir::lower_program(
                &state.program,
                checked,
                &hew_hir::ResolutionCtx,
                hew_hir::TargetArch::host(),
            );
            assert!(lowered.diagnostics.is_empty(), "{:?}", lowered.diagnostics);
            assert!(hew_hir::analyze_hir(&lowered.module)
                .diagnostics()
                .is_empty());
        }
        // User impl declarations deliberately use full checking, not the
        // independent-function dependency checkpoint.
        assert_eq!(batch.reused_roots(), 0);
    }

    #[test]
    fn cached_directory_defaults_keep_final_file_indices() {
        let dir = tempfile::tempdir().unwrap();
        fs::write(dir.path().join("hew.toml"), "[package]\nname = \"app\"\n").unwrap();
        let catalog = dir.path().join("catalog");
        fs::create_dir(&catalog).unwrap();
        fs::write(
            catalog.join("catalog.hew"),
            "pub fn first(value: i64 = 11) -> i64 { value }",
        )
        .unwrap();
        fs::write(
            catalog.join("other.hew"),
            "pub fn other(value: i32 = 22) -> i32 { value }",
        )
        .unwrap();
        let first = catalog.join("catalog.hew").canonicalize().unwrap();
        let other = catalog.join("other.hew").canonicalize().unwrap();
        let mut options = test_options();
        options.project_dir = Some(dir.path().to_path_buf());
        let mut batch = SourceAnalysisBatch::new(options.clone());
        let source =
            "import catalog; fn main() { println(catalog.first()); println(catalog.other()); }";
        for name in ["consumer_first.hew", "consumer_second.hew"] {
            let label = dir.path().join(name);
            let state = assert_equivalent(source, label.to_str().unwrap(), &options, &mut batch);
            assert!(state.stopped.is_none(), "{:?}", state.diagnostics);
            let checked = state
                .typecheck_result
                .as_ref()
                .unwrap()
                .tco
                .as_ref()
                .unwrap();
            let graph = state.program.module_graph.as_ref().unwrap();
            let root = graph.modules[&graph.root].source_paths.first().unwrap();
            assert_eq!(checked.file_span_indices.path_index(root), Some(0));
            let first_index = checked.file_span_indices.path_index(&first).unwrap();
            let other_index = checked.file_span_indices.path_index(&other).unwrap();
            assert_ne!(first_index, other_index);
            let lowered = hew_hir::lower_program(
                &state.program,
                checked,
                &hew_hir::ResolutionCtx,
                hew_hir::TargetArch::host(),
            );
            assert!(lowered.diagnostics.is_empty(), "{:?}", lowered.diagnostics);
            assert!(hew_hir::analyze_hir(&lowered.module)
                .diagnostics()
                .is_empty());
        }
        assert_eq!(batch.dependency_bootstraps(), 1);
        assert_eq!(batch.dependency_cache_hits(), 1);
        assert_eq!(batch.reused_roots(), 2);
    }

    #[test]
    fn cached_builtin_iterator_effects_match_the_complete_frontend() {
        let dir = tempfile::tempdir().unwrap();
        let options = test_options();
        let mut batch = SourceAnalysisBatch::new(options.clone());
        let source = r#"
fn cached_consumer_first<I>(var it: I) -> i64 where I: Iterator<Item = i64> {
    match it.next() { .Some(value) => value, .None => 0 }
}
fn cached_consumer_ready() -> i64 {
    let row: Vec<i64> = [1, 2];
    cached_consumer_first(row.iter())
}
fn main() {
    let callback: fn() -> i64 = cached_consumer_ready;
    let grid: Vec<Vec<i64>> = [[1, 2], [3, 4]];
    let sums = grid.iter().map(|row: Vec<i64>| row.iter().fold(0, |a: i64, b: i64| a + b)).collect();
    println(f"{callback()}");
    println(f"{sums[0] + sums[1]}");
}
"#;
        for name in ["first.hew", "second.hew"] {
            let label = dir.path().join(name);
            let state = assert_equivalent(source, label.to_str().unwrap(), &options, &mut batch);
            assert!(state.stopped.is_none(), "{:?}", state.diagnostics);
        }
        assert_eq!(batch.reused_roots(), 2);
        assert_eq!(batch.dependency_bootstraps(), 1);
        assert_eq!(batch.dependency_cache_hits(), 1);
    }

    #[test]
    fn nested_collect_defaults_preserve_effects_with_cached_dependencies() {
        let dir = tempfile::tempdir().unwrap();
        let options = test_options();
        let source =
            include_str!("../../tests/core-acceptance/cases/nested-collect-literal-effects.hew");
        let suspending = source.replace(
            "|a: i64, b: i64| a + b",
            "|a: i64, b: i64| { sleep(1ms); a + b }",
        );
        for (source, must_suspend) in [(source, false), (suspending.as_str(), true)] {
            let mut batch = SourceAnalysisBatch::new(options.clone());
            for name in ["first.hew", "second.hew"] {
                let label = dir.path().join(name);
                let state =
                    assert_equivalent(source, label.to_str().unwrap(), &options, &mut batch);
                if must_suspend {
                    let output = state
                        .typecheck_result
                        .as_ref()
                        .unwrap()
                        .tco
                        .as_ref()
                        .unwrap();
                    assert!(
                        output.errors.iter().any(|error| error
                            .message
                            .contains("closure suspends via `sleep(...)`")),
                        "{:?}",
                        state.diagnostics
                    );
                } else {
                    assert!(state.stopped.is_none(), "{:?}", state.diagnostics);
                }
            }
            assert_eq!(batch.reused_roots(), 2);
            assert_eq!(batch.dependency_bootstraps(), 1);
            assert_eq!(batch.dependency_cache_hits(), 1);
        }
    }

    #[test]
    fn independent_roots_reuse_dependencies_with_fresh_alias_scopes() {
        let dir = tempfile::tempdir().unwrap();
        fs::write(
            dir.path().join("greeting.hew"),
            "fn hidden<T>(value: T) -> T { value } pub fn greet(value: i32 = 7) -> i32 { hidden(value) }\n",
        )
        .unwrap();
        let options = test_options();
        let mut batch = SourceAnalysisBatch::new(options.clone());
        let sources = [
            "import greeting; fn consumer_whole_import() -> i32 { greeting.greet() } fn main() {}",
            "import greeting.{ greet as hello }; fn consumer_aliased_import() -> i32 { hello() } fn main() {}",
            "import greeting.{ greet }; fn consumer_named_import() -> i32 { greet() } fn main() {}",
            "import greeting.{ greet as invoke_greeting }; fn consumer_generic_root<T>(value: T) -> T { value } fn main() { let result = consumer_generic_root(invoke_greeting()); }",
        ];
        for (index, source) in sources.iter().enumerate() {
            let label = dir.path().join(format!("consumer_{index}.hew"));
            assert_equivalent(source, label.to_str().unwrap(), &options, &mut batch);
        }
        assert_eq!(
            batch.reused_roots(),
            4,
            "all ordinary mixed-import roots must reuse the checkpoint"
        );
        assert_eq!(
            batch.dependency_bootstraps(),
            1,
            "distinct physical consumer names share one dependency bootstrap"
        );
        assert_eq!(
            batch.dependency_cache_hits(),
            3,
            "first seed construction is not a cache hit"
        );
    }

    #[test]
    fn dependency_lookup_overlap_and_new_generic_guard_use_full_checker() {
        let dir = tempfile::tempdir().unwrap();
        fs::write(
            dir.path().join("greeting.hew"),
            "fn helper() -> i32 { 7 } pub fn greet() -> i32 { helper() }\n",
        )
        .unwrap();
        let options = test_options();
        let mut batch = SourceAnalysisBatch::new(options.clone());
        for source in [
            "import greeting; fn helper() -> i32 { 9 } fn main() { greeting.greet(); }",
            "import greeting.{ greet as helper }; fn main() { helper(); }",
            "import greeting; fn println() {} fn main() { greeting.greet(); }",
            "import greeting; fn generic<BrandNew>(value: BrandNew) -> BrandNew { value } fn main() { greeting.greet(); }",
        ] {
            let label = dir.path().join("consumer.hew");
            assert_equivalent(source, label.to_str().unwrap(), &options, &mut batch);
        }
        assert_eq!(batch.reused_roots(), 0);
    }

    #[test]
    fn imported_nominal_and_generic_private_signatures_preserve_full_frontend() {
        let dir = tempfile::tempdir().unwrap();
        fs::write(dir.path().join("greeting.hew"), "pub type Thing { value: i32; } fn identity<T>(value: T) -> T { value } pub fn greet(value: Thing) -> i32 { identity(value.value) }\n").unwrap();
        let options = test_options();
        let mut batch = SourceAnalysisBatch::new(options.clone());
        let label = dir.path().join("consumer.hew");
        let source = "import greeting.{ Thing as Data, greet as call }; fn consume(value: Data) -> i32 { call(value) } fn main() {}";
        let state = assert_equivalent(source, label.to_str().unwrap(), &options, &mut batch);
        assert!(state.stopped.is_none(), "{:?}", state.diagnostics);
        assert_eq!(
            batch.reused_roots(),
            0,
            "nominal registration order remains on the complete checker"
        );
    }

    #[test]
    fn separate_source_epochs_and_real_roots_never_share_dependency_state() {
        let first = tempfile::tempdir().unwrap();
        let second = tempfile::tempdir().unwrap();
        fs::write(
            first.path().join("greeting.hew"),
            "pub fn greet() -> i32 { 7 }",
        )
        .unwrap();
        fs::write(
            second.path().join("greeting.hew"),
            "pub fn greet() -> string { \"second\" }",
        )
        .unwrap();
        let mut options = test_options();
        let mut batch = SourceAnalysisBatch::new(options.clone());
        let first_label = first.path().join("consumer.hew");
        assert_equivalent(
            "import greeting; fn integer_consumer() -> i32 { greeting.greet() }",
            first_label.to_str().unwrap(),
            &options,
            &mut batch,
        );
        let second_label = second.path().join("consumer.hew");
        assert_equivalent(
            "import greeting; fn string_consumer() -> string { greeting.greet() }",
            second_label.to_str().unwrap(),
            &options,
            &mut batch,
        );
        options.documents.insert(
            first.path().join("greeting.hew"),
            "pub fn greet() -> string { \"changed\" }".to_string(),
        );
        let mut proposed = SourceAnalysisBatch::new(options.clone());
        assert_equivalent(
            "import greeting; fn integer_consumer() -> string { greeting.greet() }",
            first_label.to_str().unwrap(),
            &options,
            &mut proposed,
        );
        // The old immutable snapshot is still valid and independently usable.
        let original_options = test_options();
        assert_equivalent(
            "import greeting; fn integer_consumer() -> i32 { greeting.greet() }",
            first_label.to_str().unwrap(),
            &original_options,
            &mut batch,
        );
        assert_eq!(batch.reused_roots(), 3);
        assert_eq!(proposed.reused_roots(), 1);
    }
    #[test]
    fn inferred_import_signatures_keep_checked_constraints_on_full_frontend() {
        let dir = tempfile::tempdir().unwrap();
        let options = test_options();
        for (dependency, source) in [
            (
                "pub fn greet() -> _ { true }",
                "import greeting; fn consumer() -> i32 { greeting.greet() }",
            ),
            (
                "pub fn greet(value: _) -> bool { value }",
                "import greeting; fn consumer() -> bool { greeting.greet(1) }",
            ),
            (
                "pub fn greet() -> Option<_> { Some(true) }",
                "import greeting; fn consumer() -> Option<i32> { greeting.greet() }",
            ),
            (
                "fn hidden() -> _ { true } pub fn greet() -> i32 { 7 }",
                "import greeting; fn main() {}",
            ),
        ] {
            fs::write(dir.path().join("greeting.hew"), dependency).unwrap();
            let mut batch = SourceAnalysisBatch::new(options.clone());
            for name in ["first.hew", "second.hew"] {
                let label = dir.path().join(name);
                assert_equivalent(source, label.to_str().unwrap(), &options, &mut batch);
            }
            assert_eq!(
                batch.reused_roots(),
                0,
                "signature inference must retain full dependency/root registration order"
            );
            assert_eq!(batch.dependency_bootstraps(), 0);
        }
    }

    #[test]
    fn deferred_closure_and_effect_facts_finalize_independently_for_each_root() {
        let dir = tempfile::tempdir().unwrap();
        fs::write(
            dir.path().join("greeting.hew"),
            "pub fn greet() -> i32 { let call: fn(i32) -> i32 = |value: _| value + 1; call(6) }",
        )
        .unwrap();
        let options = test_options();
        let mut batch = SourceAnalysisBatch::new(options.clone());
        for (name, source) in [
            (
                "first.hew",
                "import greeting; fn first() -> i32 { greeting.greet() }",
            ),
            (
                "second.hew",
                "import greeting.{ greet as invoke_greeting }; fn second() -> i32 { let local: fn[suspends]() -> i32 = || invoke_greeting(); local() }",
            ),
        ] {
            let label = dir.path().join(name);
            let state = assert_equivalent(source, label.to_str().unwrap(), &options, &mut batch);
            assert!(state.stopped.is_none(), "{:?}", state.diagnostics);
            let output = state
                .typecheck_result
                .as_ref()
                .unwrap()
                .tco
                .as_ref()
                .unwrap();
            assert!(!output.closure_escape_facts.is_empty());
            assert!(!output.suspension_effects.calls.is_empty());
        }
        assert_eq!(batch.dependency_bootstraps(), 1);
        assert_eq!(batch.dependency_cache_hits(), 1);
    }
}

#[cfg(test)]
mod source_anchor_tests {
    use super::*;
    use hew_types::module_registry::ModuleAnchor;

    #[test]
    fn rooted_in_memory_sources_use_the_supplied_package_manifest() {
        // wasm32-unknown-unknown has rooted paths but no absolute host paths.
        // Source identity belongs to the provider, not host filesystem support.
        let root = Path::new("/hew-browser-anchor");
        let mut documents = SourceSet::in_memory();
        documents.insert(root.join("hew.toml"), "[package]\nname = \"app\"\n");
        let anchor = anchor_for(&root.join("lib.hew"), &[], &documents).unwrap();
        assert!(
            matches!(anchor, ModuleAnchor::Package { dir, name: Some(name) }
            if dir == root && name == package_module_path("app"))
        );
    }

    #[test]
    fn loose_buffers_keep_relative_import_semantics_with_either_provider() {
        for mut documents in [SourceSet::in_memory(), SourceSet::new()] {
            documents.insert("user/hew.toml", "[package]\nname = \"user\"\n");
            assert!(matches!(
                anchor_for(Path::new("user/nav.hew"), &[], &documents).unwrap(),
                ModuleAnchor::Loose
            ));
        }
    }
}
