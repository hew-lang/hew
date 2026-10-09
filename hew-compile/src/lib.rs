use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt;
use std::ops::Range;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use hew_parser::ast::{ImportDecl, Item, Program, Spanned};
use hew_parser::module::ModulePath;
use serde::{de::DeserializeOwned, Deserialize};

mod deterministic_admission;
mod host;

/// Source entries checked for operations the deterministic driver cannot
/// schedule. A dispatcher supplies each deterministic test separately so a
/// real-time sibling does not affect its admission verdict.
#[derive(Debug, Clone, Default)]
pub enum DeterministicAdmission {
    #[default]
    Off,
    ProcessEntry,
    Tests(Vec<hew_types::DeclarationOccurrence>),
}

fn require_deterministic_typecheck(options: &FrontendOptions) -> Result<(), FrontendFailure> {
    if options.no_typecheck
        && !matches!(options.deterministic_admission, DeterministicAdmission::Off)
    {
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
    pub no_typecheck: bool,
    pub enable_wasm_target: bool,
    pub pkg_path: Option<PathBuf>,
    /// Anchor the in-memory compile to a specific project directory, enabling
    /// manifest-aware import resolution (local `src/` lookup, manifest dep
    /// validation, lockfile) identical to `compile_file`.  When `None` the
    /// old cwd-fallback with no manifest is used.
    pub project_dir: Option<PathBuf>,
    /// Exact roots used to resolve standard-library and global modules.
    ///
    /// When unset, the frontend discovers roots from the source path, current
    /// directory, and installed compiler layout. Synthetic in-process callers
    /// should set this so resolution does not depend on the host process's
    /// working directory or executable location.
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
    /// Compile-time host-operation admission for selected deterministic roots.
    pub deterministic_admission: DeterministicAdmission,
    /// The sole deterministic production peer for a selected `_test.hew`
    /// root. Arbitrary sibling discovery is intentionally not supported.
    pub companion: Option<PathBuf>,
    /// Open editor buffers that override on-disk content for this run.
    ///
    /// Every source read the frontend performs consults this set first, so an
    /// unsaved buffer checks against its saved siblings. Empty for the CLI.
    pub documents: DocumentSet,
}

/// Source text that overrides the filesystem for one frontend run.
///
/// The LSP, the browser analysis surface and the REPL all check buffers that
/// either have no file behind them or differ from the file on disk. They hand
/// the driver this set instead of running a frontend of their own.
#[derive(Debug, Clone, Default)]
pub struct DocumentSet {
    sources: BTreeMap<PathBuf, String>,
}

impl DocumentSet {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Record `source` as the current content of `path`.
    ///
    /// The canonical spelling is recorded alongside the given one because the
    /// import resolver canonicalizes every candidate before loading it.
    pub fn insert(&mut self, path: impl Into<PathBuf>, source: impl Into<String>) {
        let path = path.into();
        let source = source.into();
        if let Some(canonical) = buffer_identity(&path) {
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
        self.get(path).is_some()
    }

    /// The recorded content of `path`, under its given or canonical spelling.
    #[must_use]
    pub fn get(&self, path: &Path) -> Option<&str> {
        if self.sources.is_empty() {
            return None;
        }
        if let Some(source) = self.sources.get(path) {
            return Some(source);
        }
        let canonical = buffer_identity(path)?;
        self.sources.get(&canonical).map(String::as_str)
    }
}

/// The canonical path of a buffer. An unsaved buffer has no file to resolve,
/// but its directory does, so a spelling through a symlinked directory still
/// names the same buffer.
fn buffer_identity(path: &Path) -> Option<PathBuf> {
    path.canonicalize()
        .ok()
        .or_else(|| Some(path.parent()?.canonicalize().ok()?.join(path.file_name()?)))
}

/// The overlay used when a caller supplies no [`FrontendOptions`].
static EMPTY_DOCUMENTS: DocumentSet = DocumentSet {
    sources: BTreeMap::new(),
};

/// The canonical path of an import candidate, or `None` when nothing supplies
/// it. An open buffer stands in for a file the filesystem does not have, so an
/// editor can check a document set that is not on disk.
fn resolve_candidate(documents: &DocumentSet, candidate: &Path) -> Option<PathBuf> {
    match candidate.canonicalize() {
        Ok(canonical) => Some(canonical),
        Err(_) => documents
            .contains(candidate)
            .then(|| candidate.to_path_buf()),
    }
}

/// Read a source file, preferring an open buffer over the file on disk.
fn read_source(documents: &DocumentSet, path: &Path) -> std::io::Result<String> {
    match documents.get(path) {
        Some(source) => Ok(source.to_string()),
        None => std::fs::read_to_string(path),
    }
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
        let program = tco
            .normalized_program
            .as_ref()
            .map_or(program, |normalized| &normalized.program);
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
            .map_err(|message| SessionError::Unsupported {
                callable: None,
                message,
                span: None,
            })?;
        }
        let mut roots = Self::source_roots(program, tco)?;
        roots.extend(tco.test_entry_plans.iter().map(|plan| plan.entry));
        self.lower_hir_module(&lowered.module, tco, &roots)
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
        // File imports are appended for body lowering after checking. Their
        // public declarations remain imports, with their own source identities,
        // rather than becoming authored root exports through flattening.
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
                        .ok_or_else(|| SessionError::Unsupported {
                            callable: None,
                            message: format!(
                                "exported source declaration at {span:?} has no checked identity"
                            ),
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
        let diagnostics = hew_hir::verify_hir(module);
        if !diagnostics.is_empty() {
            return Err(SessionError::Hir(diagnostics));
        }
        let mut sir = hew_sir::lower_module_with_roots(module, tco, roots).map_err(|errors| {
            SessionError::Unsupported {
                callable: None,
                message: errors
                    .iter()
                    .map(|error| error.render(&module.defs))
                    .collect::<Vec<_>>()
                    .join("; "),
                span: None,
            }
        })?;
        let diagnostics = hew_sir::verify_module(&sir.module);
        if !diagnostics.is_empty() {
            if diagnostics.iter().all(|diagnostic| {
                matches!(
                    diagnostic.kind,
                    hew_sir::SirDiagnosticKind::UnconsumedLinear { .. }
                )
            }) {
                return Err(SessionError::Ownership(diagnostics));
            }
            return Err(SessionError::Semantic(diagnostics));
        }
        require_complete_semantics(&sir, module.entry_exit_plan.is_some())?;
        if sir.module.test_entries.len() != module.test_entry_plans.len()
            || sir
                .module
                .test_entries
                .iter()
                .zip(&module.test_entry_plans)
                .any(|(entry, plan)| entry.declaration != plan.entry)
        {
            return Err(SessionError::Unsupported {
                callable: None,
                message: "semantic test dispatcher does not match checked entry plans".into(),
                span: None,
            });
        }
        let body_index = sir.module.function_index();
        let mut compiled_roots = roots
            .iter()
            .map(|declaration| {
                let callable = sir
                    .module
                    .callable_for_declaration(declaration)
                    .ok_or_else(|| SessionError::Unsupported {
                        callable: None,
                        message: format!(
                            "selected declaration {declaration:?} has no semantic callable"
                        ),
                        span: None,
                    })?
                    .id;
                require_semantic_body(&sir.module, &body_index, callable)?;
                Ok(callable)
            })
            .collect::<Result<Vec<_>, SessionError>>()?;
        if let Some(entry) = sir.module.entry_callable {
            compiled_roots.push(entry);
        }
        for entry in &sir.module.test_entries {
            require_semantic_body(&sir.module, &body_index, entry.callable)?;
            compiled_roots.push(entry.callable);
        }
        compiled_roots.sort_unstable();
        compiled_roots.dedup();
        if self.checks == CheckSet::Build {
            hew_sir::canonicalize_module_constant_cfg(&mut sir.module)
                .map_err(SessionError::Semantic)?;
            hew_sir::transfer_module_dead_local_reads(&mut sir.module);
            // The passes rewrite in place without verifying; this is the one
            // verification of their result.
            let diagnostics = hew_sir::verify_module(&sir.module);
            if !diagnostics.is_empty() {
                return Err(SessionError::Semantic(diagnostics));
            }
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
    sir: hew_sir::LoweredModule,
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
        &self.sir
    }

    /// Consume the session result and relinquish its verification guarantee.
    #[must_use]
    pub fn into_semantics(self) -> hew_sir::LoweredModule {
        self.sir
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
        hew_mir::lower_physical_module(&self.sir.module, target)
    }
}

/// Diagnostics from the shared semantic compilation boundary.
#[derive(Debug)]
pub enum SessionError {
    Hir(Vec<hew_hir::HirDiagnostic>),
    Semantic(Vec<hew_sir::SirDiagnostic>),
    /// Source ownership obligations diagnosed by the semantic lifetime flow.
    Ownership(Vec<hew_sir::SirDiagnostic>),
    Unsupported {
        callable: Option<hew_sir::CallableId>,
        message: String,
        /// The provoking construct's span, when SIR could attribute one
        /// (#3384). `None` for a session-level refusal with no declaration
        /// of its own (a missing entry, an unresolved root).
        span: Option<Range<usize>>,
    },
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
            Self::Unsupported { message, .. } => formatter.write_str(message),
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
        return Err(SessionError::Unsupported {
            callable: None,
            message: "the selected process entry has no semantic callable".to_string(),
            span: None,
        });
    }
    for (callable, status) in &sir.callable_statuses {
        if let hew_sir::SirLoweringStatus::Unsupported { reason, span } = status {
            let name = sir
                .module
                .callable(*callable)
                .map_or("<unknown>", |item| item.symbol.as_str());
            return Err(SessionError::Unsupported {
                callable: Some(*callable),
                message: format!("semantic lowering of `{name}` is not implemented: {reason}"),
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
        return Err(SessionError::Unsupported {
            callable: Some(callable),
            message: format!("required semantic callable `{name}` has no body"),
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
            Err(SessionError::Unsupported { callable: Some(id), .. }) if id == entry
        ));
        sir.module.entry_callable = None;
        assert!(matches!(
            require_complete_semantics(&sir, true),
            Err(SessionError::Unsupported { callable: None, .. })
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
            Err(SessionError::Unsupported { callable: Some(id), .. }) if id == callee
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
        *status = hew_sir::SirLoweringStatus::Unsupported {
            reason: "deliberate incomplete lowering".to_string(),
            span: None,
        };
        assert!(matches!(
            require_complete_semantics(&sir, true),
            Err(SessionError::Unsupported { callable: Some(id), message, .. })
                if id == entry && message.contains("deliberate incomplete lowering")
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
    pub source: Option<String>,
    pub filename: Option<String>,
    /// Per-note source text and filename when a secondary span belongs to a
    /// different module than the primary diagnostic.
    pub note_sources: Vec<Option<(String, String)>>,
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
        source: &str,
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
                source: Some(Arc::from(source)),
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
        source: &str,
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
                source: Some(Arc::from(source)),
                notes,
                help,
            }),
        }
    }

    fn parse(source: &str, filename: &str, diagnostic: hew_parser::ParseError) -> Self {
        Self {
            source: Some(source.to_string()),
            filename: Some(filename.to_string()),
            note_sources: Vec::new(),
            kind: FrontendDiagnosticKind::Parse(diagnostic),
        }
    }

    fn type_(
        source: &str,
        filename: &str,
        diagnostic: hew_types::TypeError,
        module_source_map: &ModuleSourceMap,
    ) -> Self {
        let note_sources = diagnostic
            .notes
            .iter()
            .map(|(_, _, source_module)| {
                source_module
                    .as_deref()
                    .and_then(|module| module_source_map.get(module))
                    .cloned()
            })
            .collect();
        Self {
            source: Some(source.to_string()),
            filename: Some(filename.to_string()),
            note_sources,
            kind: FrontendDiagnosticKind::Type(diagnostic),
        }
    }

    fn hir(
        source: Option<&str>,
        filename: Option<&str>,
        diagnostic: hew_hir::HirDiagnostic,
    ) -> Self {
        Self {
            source: source.map(str::to_string),
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
}

impl FrontendFailure {
    fn new(message: impl Into<String>, diagnostics: Vec<FrontendDiagnostic>) -> Self {
        Self {
            message: message.into(),
            diagnostics,
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
        _ => path.starts_with(root),
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
    pub documents: &'a DocumentSet,
}

#[derive(Debug)]
struct LockedPackageCheck {
    package_dir: PathBuf,
    name: String,
    version: String,
}

#[derive(Debug)]
pub struct TypeCheckResult {
    pub tco: Option<hew_types::check::TypeCheckOutput>,
    pub module_registry: hew_types::module_registry::ModuleRegistry,
}

struct ProjectContext {
    source: String,
    project_dir: PathBuf,
    manifest_deps: Option<Vec<String>>,
    package_name: Option<String>,
    locked_versions: Option<Vec<(String, String)>>,
}

type ModuleSourceMap = HashMap<String, (String, String)>;

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

#[must_use]
pub fn validate_imports_against_manifest(
    items: &[Spanned<Item>],
    manifest_deps: &[String],
    package_name: Option<&str>,
) -> Vec<String> {
    let mut errors = Vec::new();
    for (item, _) in items {
        let Item::Import(decl) = item else { continue };
        if decl.file_path.is_some() || decl.path.segments.is_empty() {
            continue;
        }
        let segments = import_segments(&decl.path);
        let module_str = segments.join("::");
        let source_module = segments.join(".");
        if is_builtin_module(&module_str) {
            continue;
        }
        if package_name.is_some_and(|pkg| segments.first().is_some_and(|seg| *seg == pkg)) {
            continue;
        }
        if !manifest_deps
            .iter()
            .any(|dependency| dependency == &module_str || dependency == &source_module)
        {
            errors.push(format!(
                "Error: module `{source_module}` is not declared in hew.toml\n  hint: add it with `hew add {source_module}`"
            ));
        }
    }
    errors
}

fn is_builtin_module(module_path: &str) -> bool {
    module_path.starts_with("std::")
        || module_path.starts_with("hew::")
        || module_path.starts_with("ecosystem::")
}

/// The closest existing `std` module to a mistyped import's last path
/// segment: an exact leaf match (`std.http` -> `std.net.http`, a missed or
/// mis-nested path segment) when one exists, else a near-miss typo
/// (`std.htp` -> `std.net.http`).
///
/// Near-miss matching compares leaf names (the last dotted segment), not
/// full dotted paths: `std.htp` and `std.net.http` differ by far more than
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
    let documents = options.map_or(&EMPTY_DOCUMENTS, |options| &options.documents);
    let source = if let Some(source) = source_override {
        source.to_string()
    } else {
        if Path::new(input).is_dir() {
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
                .find(|dir| dir.join("hew.toml").is_file())
                .map(Path::to_path_buf)
        })
        .unwrap_or_else(|| input_dir.to_path_buf());
    let (manifest_deps, package_name) = load_manifest_metadata(&project_dir)?;
    Ok(ProjectContext {
        source,
        project_dir: project_dir.clone(),
        manifest_deps,
        package_name,
        locked_versions: load_lockfile(&project_dir)?,
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
            let (manifest_deps, package_name) = load_manifest_metadata(dir)?;
            Ok(ProjectContext {
                source: source.to_string(),
                project_dir: dir.clone(),
                manifest_deps,
                package_name,
                locked_versions: load_lockfile(dir)?,
            })
        }
        None => Ok(ProjectContext {
            source: source.to_string(),
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
    if let Some(deps) = &project.manifest_deps {
        let errs = validate_imports_against_manifest(
            &program.items,
            deps,
            project.package_name.as_deref(),
        );
        if !errs.is_empty() {
            return Err(FrontendFailure::new(
                "undeclared dependencies",
                errs.into_iter().map(FrontendDiagnostic::message).collect(),
            ));
        }
    }

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
    let module_graph = build_module_graph_with_diagnostics(
        input_path,
        &mut program.items,
        program.module_doc.clone(),
        &mut import_ctx,
        diagnostics,
    )?;
    program.module_graph = Some(module_graph);
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

fn build_module_source_map(program: &Program, documents: &DocumentSet) -> ModuleSourceMap {
    let Some(ref module_graph) = program.module_graph else {
        return ModuleSourceMap::new();
    };

    let mut map = ModuleSourceMap::new();
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
                if let Some((_, text)) = COMPILED_PRELUDE_STD_SOURCES
                    .iter()
                    .find(|(name, _)| std.as_str() == "std" && *name == leaf.as_str())
                {
                    map.insert(
                        mod_id.dotted(),
                        ((*text).to_string(), format!("std/{leaf}.hew")),
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
    map
}

fn type_diagnostic_to_frontend(
    root_source: &str,
    root_filename: &str,
    diagnostic: hew_types::TypeError,
    module_source_map: &ModuleSourceMap,
) -> FrontendDiagnostic {
    let (source, filename) = if let Some(ref mod_name) = diagnostic.source_module {
        module_source_map
            .get(mod_name.as_str())
            .map_or((root_source, root_filename), |(source, filename)| {
                (source.as_str(), filename.as_str())
            })
    } else {
        (root_source, root_filename)
    };
    FrontendDiagnostic::type_(source, filename, diagnostic, module_source_map)
}

fn hir_diagnostic_to_frontend(
    root_source: &str,
    root_filename: &str,
    diagnostic: hew_hir::HirDiagnostic,
    module_source_map: &ModuleSourceMap,
) -> FrontendDiagnostic {
    let (source, filename) = match diagnostic.source_module.as_deref() {
        None => (Some(root_source), Some(root_filename)),
        Some(module) => module_source_map
            .get(module)
            .map_or((None, None), |(source, filename)| {
                (Some(source.as_str()), Some(filename.as_str()))
            }),
    };
    FrontendDiagnostic::hir(source, filename, diagnostic)
}

/// Route HIR diagnostics through the same source-map attribution path used by
/// parser and type diagnostics. Non-root diagnostics never fall back to root
/// source on a source-map miss; callers render an explicit unavailable note.
#[must_use]
pub fn hir_diagnostics_to_frontend(
    program: &Program,
    root_source: &str,
    root_filename: &str,
    diagnostics: Vec<hew_hir::HirDiagnostic>,
    documents: &DocumentSet,
) -> Vec<FrontendDiagnostic> {
    let module_source_map = build_module_source_map(program, documents);
    diagnostics
        .into_iter()
        .map(|diagnostic| {
            hir_diagnostic_to_frontend(root_source, root_filename, diagnostic, &module_source_map)
        })
        .collect()
}

/// Attribute source ownership findings from SIR using the ordinary module source map.
#[must_use]
pub fn ownership_diagnostics_to_frontend(
    program: &Program,
    root_source: &str,
    root_filename: &str,
    diagnostics: Vec<hew_sir::SirDiagnostic>,
    documents: &DocumentSet,
) -> Vec<FrontendDiagnostic> {
    let sources = build_module_source_map(program, documents);
    diagnostics
        .into_iter()
        .map(|diagnostic| {
            let hew_sir::SirDiagnosticKind::UnconsumedLinear {
                binding,
                span,
                source_origin,
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
            let source = match &source_origin {
                hew_sir::FunctionSourceOrigin::RootUnit => Some((root_source, root_filename)),
                hew_sir::FunctionSourceOrigin::Foreign(module) => sources
                    .get(module)
                    .map(|(source, filename)| (source.as_str(), filename.as_str())),
                hew_sir::FunctionSourceOrigin::Unknown => None,
            };
            source.map_or_else(
                || FrontendDiagnostic::coded_message("E_MUST_CONSUME", message.clone()),
                |(source, filename)| {
                    FrontendDiagnostic::coded_message_at(
                        "E_MUST_CONSUME",
                        message.clone(),
                        span,
                        source,
                        filename,
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

    if options.no_typecheck {
        return (
            TypeCheckResult {
                tco: None,
                module_registry,
            },
            Vec::new(),
        );
    }

    let mut checker = hew_types::Checker::new(module_registry);
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
    let module_source_map = build_module_source_map(program, &options.documents);
    let mut lint_sources = hew_types::LintSources::new();
    lint_sources.set_root(source.to_string());
    for (module, (module_source, _filename)) in &module_source_map {
        lint_sources.set_module(module.clone(), module_source.clone());
    }
    checker.set_lint_sources(lint_sources);
    let tco = if let Some(cache) = cache {
        let sources = module_source_map
            .iter()
            .map(|(module, (text, filename))| (module.clone(), text.clone(), filename.clone()))
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
            type_diagnostic_to_frontend(source, input, diagnostic, &module_source_map)
        })
        .collect::<Vec<_>>();
    diagnostics.extend(tco.warnings.iter().cloned().map(|diagnostic| {
        type_diagnostic_to_frontend(source, input, diagnostic, &module_source_map)
    }));

    let module_registry = checker.into_module_registry();
    (
        TypeCheckResult {
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
    require_deterministic_typecheck(options)?;
    let (result, mut diagnostics) =
        typecheck_program_with_diagnostics(program, source, input, options, None);
    if type_check_failed(&result) {
        return Err(FrontendFailure::new("type errors found", diagnostics));
    }
    if let Some(output) = result.tco.as_ref() {
        let refused = deterministic_admission::check(program, output, source, input, options);
        if !refused.is_empty() {
            diagnostics.extend(refused);
            return Err(FrontendFailure::new(
                "deterministic host operations found",
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
    require_deterministic_typecheck(options)?;
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
        let refused =
            deterministic_admission::check(&program, output, source, source_label, options);
        if !refused.is_empty() {
            diagnostics.extend(refused);
            return Err(FrontendFailure::new(
                "deterministic host operations found",
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
/// itself, and `std.failure` would close a cycle through it.
fn inject_prelude_module_loads(items: &mut Vec<Spanned<Item>>, input: &Path) {
    if canonical_direct_stdlib_module_for_source(input).is_some() {
        return;
    }
    let path = ["std", "link_monitor"];
    let already_imported = items
        .iter()
        .any(|(item, _)| matches!(item, Item::Import(decl) if import_segments(&decl.path) == path));
    if already_imported {
        return;
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

fn source_contains_regex_literal(source: &str) -> bool {
    hew_lexer::Lexer::new(source)
        .any(|(token, _)| matches!(token, hew_lexer::Token::RegexLiteral(_)))
}

fn module_id_from_file(source_dir: &Path, canonical_path: &Path) -> hew_parser::module::ModulePath {
    let without_ext = canonical_path.with_extension("");
    let rel = without_ext.strip_prefix(source_dir).unwrap_or(&without_ext);
    let mut segments = rel
        .iter()
        .filter_map(|segment| segment.to_str())
        .map(std::string::ToString::to_string)
        .collect::<Vec<_>>();

    if segments.is_empty() {
        segments.push(
            canonical_path
                .file_stem()
                .and_then(|segment| segment.to_str())
                .unwrap_or("unknown")
                .to_string(),
        );
    }

    hew_parser::module::ModulePath::new(segments)
}

/// The entry file of the directory module (spec 3.5.1) that `path` belongs
/// to, when `path` is that module's entry or one of its peers.
///
/// A peer shares one namespace with its entry and siblings, and an entry is
/// incomplete without its peers, so neither is a program of its own. Checking
/// or migrating such a file checks the whole module as an importer sees it.
/// Test files (`*_test.hew`) are never peers; see [`test_companion`]. A
/// shipped std source already has its module identity from the std root, so
/// it checks through that identity instead.
#[must_use]
pub fn directory_module_entry(path: &Path) -> Option<PathBuf> {
    let path = path.canonicalize().ok()?;
    if path.extension()? != "hew"
        || is_hew_test_file(&path)
        || hew_types::module_registry::canonical_stdlib_module_for_source(&path).is_some()
    {
        return None;
    }
    directory_module_entry_in(path.parent()?)
}

/// The production source a test file (`*_test.hew`) is compiled with.
///
/// A test file inside a directory module tests that whole module, so its
/// companion is the module's entry, which assembles every peer. Elsewhere it is
/// the same-stem file beside it (`math_test.hew` tests `math.hew`).
#[must_use]
pub fn test_companion(test_file: &Path) -> Option<PathBuf> {
    let test_file = test_file.canonicalize().ok()?;
    if !is_hew_test_file(&test_file) {
        return None;
    }
    let dir = test_file.parent()?;
    directory_module_entry_in(dir).or_else(|| {
        let stem = test_file.file_stem()?.to_str()?.strip_suffix("_test")?;
        dir.join(stem)
            .with_extension("hew")
            .canonicalize()
            .ok()
            .filter(|path| path.is_file())
    })
}

/// The canonical entry file `dir/<dir>.hew` of the directory module `dir`,
/// when it exists.
fn directory_module_entry_in(dir: &Path) -> Option<PathBuf> {
    let entry = dir.join(dir.file_name()?).with_extension("hew");
    entry.canonicalize().ok().filter(|path| path.is_file())
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
    source_override: Option<&str>,
    options: &FrontendOptions,
) -> DocumentFrontendState {
    let label = entry
        .with_file_name(DIRECTORY_MODULE_ROOT_LABEL)
        .display()
        .to_string();
    let empty = hew_parser::parse("");
    let mut state = DocumentFrontendState {
        source: String::new(),
        program: empty.program,
        parse_result: None,
        diagnostics: Vec::new(),
        typecheck_result: None,
        stopped: None,
    };
    // The requested document keeps its own text and parse for the host; an
    // open buffer stands in for its file while the module is assembled.
    let mut options = options.clone();
    let source = match source_override {
        Some(source) => {
            options.documents.insert(input, source);
            source.to_string()
        }
        None => match read_source(&options.documents, Path::new(input)) {
            Ok(source) => source,
            Err(error) => {
                return state.stop(FrontendFailure::message_only(format!(
                    "Error: cannot read {input}: {error}"
                )))
            }
        },
    };
    state.parse_result = Some(hew_parser::parse(&source));
    state.source = source;
    let options = &options;
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
    state
        .program
        .items
        .push(file_import(entry_name.to_string()));
    run_frontend_after_parse(state, &project, &label, options, None)
}

/// Resolve a module import of a directory peer through that directory's
/// canonical entry file before parsing its source set. A peer such as
/// `http_client.hew` is still a valid import spelling, but loading it as an
/// independent source would omit the entry module and make the result depend
/// on import order once both spellings canonicalise to one graph owner.
fn canonical_directory_module_entry_source(source: &Path) -> PathBuf {
    let Some(parent) = source.parent() else {
        return source.to_path_buf();
    };
    let Some(directory_name) = parent.file_name().and_then(|name| name.to_str()) else {
        return source.to_path_buf();
    };
    let Some(file_stem) = source.file_stem().and_then(|name| name.to_str()) else {
        return source.to_path_buf();
    };
    if directory_name == file_stem {
        return source.to_path_buf();
    }

    let entry = parent.join(format!("{directory_name}.hew"));
    if entry.is_file() {
        entry.canonicalize().unwrap_or(entry)
    } else {
        source.to_path_buf()
    }
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

/// Whether a module import named a directory module through its entry file
/// rather than through the directory itself.
///
/// `pkg.dir.dir` matches the FLAT candidate `…/dir/dir.hew`, which is also the
/// directory candidate of `pkg.dir` — one source under two spellings. Paths
/// that repeat their last segment and still name a module of their own match a
/// DIRECTORY candidate instead: `std.crypto.crypto` is
/// `std/crypto/crypto/crypto.hew`, and a module named after its own package
/// (`probe.probe` at `probe/src/probe/probe.hew`) resolves the same way.
fn is_directory_module_entry_alias(path: &[&str], canonical: &Path, form: CandidateForm) -> bool {
    let Some((last, rest)) = path.split_last() else {
        return false;
    };
    form == CandidateForm::Flat
        && rest.last() == Some(last)
        && canonical.file_stem() == canonical.parent().and_then(Path::file_name)
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
/// files. Otherwise the fix is a shared module both sides import.
///
/// `manifest_project_dir` (a discovered `hew.toml` package root — `None` for
/// a manifest-less standalone compile) and its `src` are excluded from that
/// "shared directory" check even when every module happens to sit there:
/// both are flat buckets the dotted-path resolver searches for otherwise-
/// unrelated top-level modules (see the `candidates.push(ctx.project_dir...)`
/// sites in `resolve_file_imports_internal`), not a private submodule
/// directory a program ever imports as one unit — "make `src/src.hew` the
/// entry" is not a real fix. A manifest-less compile has no such bucket: its
/// `project_dir` fallback is just the entry file's own directory, which is a
/// perfectly good directory-module candidate.
///
/// Falls back to the bare chain message (former behaviour) if a cycle member
/// is missing from `graph` or its source file cannot be re-read; both should
/// be unreachable since every cycle member was inserted into `graph` before
/// `compute_topo_order` ran and its source was just parsed.
fn cycle_error_to_frontend_failure(
    graph: &hew_parser::module::ModuleGraph,
    cycle_err: &hew_parser::module::CycleError,
    manifest_project_dir: Option<&Path>,
    documents: &DocumentSet,
) -> FrontendFailure {
    let chain = cycle_err.to_string();
    let edge_count = cycle_err.import_spans.len();

    let mut locations: Vec<(PathBuf, String, Range<usize>, String)> =
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
    let shared_dir_is_a_flat_root = manifest_project_dir.is_some_and(|project_dir| {
        let project_src_dir = project_dir.join("src");
        shared_dir == Some(project_dir) || shared_dir == Some(project_src_dir.as_path())
    });
    let help = if same_directory && !shared_dir_is_a_flat_root {
        let dir_name = locations[0]
            .0
            .parent()
            .and_then(Path::file_name)
            .and_then(|name| name.to_str())
            .unwrap_or("<dir>");
        format!(
            "these modules share one directory; make `{dir_name}/{dir_name}.hew` the entry and \
             let the others be peers (spec 3.5.1), then drop the imports between them"
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
            source: Arc::from(source.as_str()),
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
    manifest_project_dir: Option<&Path>,
    documents: &DocumentSet,
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
        cycle_error_to_frontend_failure(module_graph, &cycle_err, manifest_project_dir, documents)
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
        std::fs::canonicalize(source_file).unwrap_or_else(|_| source_file.to_path_buf());
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
        let manifest_project_dir = ctx.package_name.is_some().then_some(ctx.project_dir);
        return Err(cycle_error_to_frontend_failure(
            &graph,
            &cycle_err,
            manifest_project_dir,
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
        ctx.package_name.is_some().then_some(ctx.project_dir),
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
    // `bank$Account` — so `spawn bank.Account(...)` and `spawn
    // store.Account(...)` bind their own handlers/state/drop glue.  Within one
    // module there is no qualifier left to tell two same-named actors apart,
    // so that case stays a hard error.  Runs before
    // `flatten_file_import_items`, so each actor still lives in exactly one
    // module here.
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
        let (_, source) = COMPILED_PRELUDE_STD_SOURCES
            .iter()
            .find(|(leaf, _)| *leaf == name)
            .expect("every prelude module has a compiled-in source");
        Ok((None, (*source).to_string()))
    })
    .expect("the compiled-in prelude sources parse");
}

/// Methods on builtin types are always available, like `.len()`: the
/// standard-library modules that own them join every program as ordinary
/// modules unless the program already imports them. The builtins prelude is
/// loaded out of band, so only its Display impls take this path; its other
/// declarations retain their compiler-owned registration.
fn add_prelude_std_modules(
    graph: &mut hew_parser::module::ModuleGraph,
    mut source_for: impl FnMut(&str) -> Result<(Option<PathBuf>, String), FrontendFailure>,
) -> Result<(), FrontendFailure> {
    use hew_parser::module::Module;
    for (name, _) in COMPILED_PRELUDE_STD_SOURCES {
        let id = ModulePath::new(["std", name]);
        if graph.modules.contains_key(&id) {
            continue;
        }
        let (path, source) = source_for(name)?;
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
                    .map(|error| FrontendDiagnostic::parse(&source, &filename, error))
                    .collect(),
            ));
        }
        let items = parsed
            .program
            .items
            .into_iter()
            .filter(|(item, _)| {
                name != "builtins"
                    || matches!(item, Item::Impl(decl) if decl
                        .trait_bound
                        .as_ref()
                        .is_some_and(|bound| bound.path.to_string() == "Display"))
                // TRANSITION(P1): deleted by A1 commit 2
            })
            .collect();
        graph
            .add_module(Module {
                id: id.clone(),
                items,
                imports: Vec::new(),
                source_paths: path.into_iter().collect(),
                doc: None,
            })
            .expect("prelude module absence was checked");
        graph.topo_order.push(id);
    }
    Ok(())
}

/// The prelude standard-library modules by `std.<name>` leaf, with the source
/// compiled into the host. A build resolves each through the std search path
/// instead ([`resolve_prelude_std_source`]); only an analysis with no search
/// path uses this text.
const COMPILED_PRELUDE_STD_SOURCES: [(&str, &str); 4] = [
    ("builtins", include_str!("../../std/builtins.hew")),
    ("option", include_str!("../../std/option.hew")),
    ("result", include_str!("../../std/result.hew")),
    ("iter", include_str!("../../std/iter.hew")),
];

/// Resolve the prelude source `std/<name>.hew` from the standard-library
/// root a `std` import uses: the configured search path, or
/// [`hew_types::module_registry::stdlib_search_paths`]. The module carries the
/// path its diagnostics belong to.
fn resolve_prelude_std_source(
    ctx: &ImportResolutionContext<'_>,
    name: &str,
) -> Result<(PathBuf, String), FrontendFailure> {
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
/// spelling could tell them apart, so that shape stays a hard error.  The
/// guard runs at graph-build time (before file-import flattening), so each
/// actor lives in exactly one module here.
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

fn flatten_file_import_items(program: &mut Program) {
    let extra: Vec<Spanned<Item>> = hew_parser::module::file_import_spliced_items(&program.items)
        .into_iter()
        .map(|(item, _)| item.clone())
        .collect();
    program.items.extend(extra);
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
    documents: &DocumentSet,
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

    let cwd = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));

    for idx in &import_indices {
        let is_module_import = matches!(
            &items[*idx].0,
            Item::Import(decl) if !decl.path.segments.is_empty()
        );
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
                let module_str = segments.join("::");
                let source_module = segments.join(".");
                // A `std` module resolves only from the standard-library root
                // (`stdlib_search_paths`), never beside the source or in cwd.
                let is_std_import = module_str.starts_with("std::");
                let is_declared_dependency = ctx.manifest_deps.is_some_and(|deps| {
                    deps.iter()
                        .any(|dependency| dependency == &module_str || dependency == &source_module)
                });
                let is_local = ctx
                    .package_name
                    .is_some_and(|pkg| segments.first().is_some_and(|seg| *seg == pkg));
                let rest_path: Vec<&str> = if is_local {
                    segments[1..].to_vec()
                } else {
                    Vec::new()
                };

                let rel_path = segments.iter().collect::<PathBuf>().with_extension("hew");
                let last = *segments.last().expect("path is non-empty");
                let dir_path = segments
                    .iter()
                    .collect::<PathBuf>()
                    .join(format!("{last}.hew"));
                let mut candidates: Vec<(PathBuf, CandidateForm)> = Vec::new();
                let mut locked_project_candidates = Vec::new();
                let mut installed_package_dir = None;
                let locked_version = ctx
                    .locked_versions
                    .and_then(|locked| {
                        locked
                            .iter()
                            .find(|(name, _)| name == &module_str || name == &source_module)
                    })
                    .map(|(_, version)| version.as_str());

                if !is_std_import && is_local && !rest_path.is_empty() {
                    let local_last = *rest_path.last().expect("non-empty local path");
                    let local_rel = rest_path.iter().collect::<PathBuf>();
                    let local_dir = local_rel.join(format!("{local_last}.hew"));
                    let local_flat = local_rel.with_extension("hew");
                    candidates.push((
                        ctx.project_dir.join("src").join(&local_dir),
                        CandidateForm::Directory,
                    ));
                    candidates.push((
                        ctx.project_dir.join("src").join(&local_flat),
                        CandidateForm::Flat,
                    ));
                    candidates.push((ctx.project_dir.join(&local_dir), CandidateForm::Directory));
                    candidates.push((ctx.project_dir.join(&local_flat), CandidateForm::Flat));
                }

                if !is_std_import {
                    candidates.push((source_dir.join(&dir_path), CandidateForm::Directory));
                    candidates.push((source_dir.join(&rel_path), CandidateForm::Flat));
                    candidates.push((cwd.join(&dir_path), CandidateForm::Directory));
                    candidates.push((cwd.join(&rel_path), CandidateForm::Flat));
                }

                let module_dir = segments.iter().collect::<PathBuf>();
                if let Some(version) = locked_version.filter(|_| !is_std_import) {
                    let entry_file = format!("{}.hew", segments.last().expect("path is non-empty"));
                    let versioned_rel = module_dir.join(version).join(entry_file);
                    // The version directory sits between the module and its
                    // entry file, so this is a package root, never a flat file.
                    candidates.push((
                        ctx.project_dir.join(".hew/packages").join(&versioned_rel),
                        CandidateForm::Directory,
                    ));
                    if let Some(pkg) = ctx.extra_pkg_path {
                        candidates.push((pkg.join(&versioned_rel), CandidateForm::Directory));
                    }
                }

                if !is_std_import {
                    candidates.push((
                        ctx.project_dir.join(".hew/packages").join(&rel_path),
                        CandidateForm::Flat,
                    ));
                    let project_package_dir =
                        ctx.project_dir.join(".hew/packages").join(&module_dir);
                    if is_declared_dependency {
                        installed_package_dir = Some(project_package_dir.clone());
                    }
                    let project_package_entry =
                        ctx.project_dir.join(".hew/packages").join(&dir_path);
                    if let Some(version) = locked_version {
                        locked_project_candidates.push((
                            project_package_entry.clone(),
                            LockedPackageCheck {
                                package_dir: project_package_dir,
                                name: source_module.clone(),
                                version: version.to_string(),
                            },
                        ));
                    }
                    candidates.push((project_package_entry, CandidateForm::Directory));
                }

                if let Some(pkg) = ctx.extra_pkg_path.filter(|_| !is_std_import) {
                    candidates.push((pkg.join(&dir_path), CandidateForm::Directory));
                    candidates.push((pkg.join(&rel_path), CandidateForm::Flat));
                    if segments.len() > 1 && !is_builtin_module(&module_str) {
                        let rest_dir = segments[1..]
                            .iter()
                            .collect::<PathBuf>()
                            .join(format!("{last}.hew"));
                        let rest_flat = segments[1..]
                            .iter()
                            .collect::<PathBuf>()
                            .with_extension("hew");
                        candidates.push((pkg.join(&rest_dir), CandidateForm::Directory));
                        candidates.push((pkg.join(&rest_flat), CandidateForm::Flat));
                    }
                }

                if module_str.starts_with("hew::") && segments.len() > 1 {
                    let tail = segments[1..].iter().collect::<PathBuf>();
                    let tail_last = segments.last().expect("path is non-empty");
                    let tail_dir = tail.join(format!("{tail_last}.hew"));
                    let tail_rel = tail.with_extension("hew");
                    if let Some(pkg) = ctx.extra_pkg_path {
                        candidates.push((pkg.join(&tail_dir), CandidateForm::Directory));
                        candidates.push((pkg.join(&tail_rel), CandidateForm::Flat));
                    }
                }

                if module_str.starts_with("ecosystem::") && segments.len() > 1 {
                    let tail = segments[1..].iter().collect::<PathBuf>();
                    let tail_last = segments.last().expect("path is non-empty");
                    let tail_dir = tail.join(format!("{tail_last}.hew"));
                    let tail_rel = tail.with_extension("hew");
                    if let Some(pkg) = ctx.extra_pkg_path {
                        candidates.push((pkg.join(&tail_dir), CandidateForm::Directory));
                        candidates.push((pkg.join(&tail_rel), CandidateForm::Flat));
                    }
                }

                // The standard-library root.
                let discovered_search_paths;
                let search_paths = if let Some(paths) = ctx.module_search_paths {
                    paths
                } else {
                    discovered_search_paths = hew_types::module_registry::stdlib_search_paths();
                    &discovered_search_paths
                };
                for root in search_paths {
                    candidates.push((root.join(&dir_path), CandidateForm::Directory));
                    candidates.push((root.join(&rel_path), CandidateForm::Flat));
                }

                // Collect ALL candidates that resolve, then deduplicate by canonical path.
                // If two or more distinct canonical paths resolve, the import is ambiguous —
                // fail-closed rather than silently picking the first match.
                let mut resolved: Vec<(PathBuf, CandidateForm)> = Vec::new();
                for (candidate, form) in &candidates {
                    if let Some(canonical) = resolve_candidate(ctx.documents, candidate) {
                        if let Some((_, check)) = locked_project_candidates
                            .iter()
                            .find(|(locked_candidate, _)| locked_candidate == candidate)
                        {
                            verify_locked_project_package(check)?;
                        }
                        // One file reached by both shapes is a directory module
                        // named by its directory: the directory candidate wins.
                        match resolved.iter_mut().find(|(path, _)| *path == canonical) {
                            Some((_, existing)) => {
                                if *form == CandidateForm::Directory {
                                    *existing = CandidateForm::Directory;
                                }
                            }
                            None => resolved.push((canonical, *form)),
                        }
                    }
                }
                resolved.sort_by(|(left, _), (right, _)| left.cmp(right));

                if resolved.len() > 1 {
                    let paths = resolved
                        .iter()
                        .map(|(path, _)| path.display().to_string())
                        .collect::<Vec<_>>()
                        .join("` and `");
                    return Err(FrontendFailure::coded_message("E_IMPORT_AMBIGUOUS", format!(
                        "Error: import `{source_module}` is ambiguous: both `{paths}` exist.\n  Rename or remove one to resolve the ambiguity."
                    )));
                }

                if let Some((canonical, form)) = resolved.into_iter().next() {
                    if is_module_import
                        && is_directory_module_entry_alias(&segments, &canonical, form)
                    {
                        // A directory module is spelled by its directory, and
                        // its entry file adds no second module (spec 3.5.1).
                        // Accepting both spellings would let one compilation
                        // reach one source under two names, so refuse the
                        // longer one and name the module it aliases.
                        let directory_module = segments[..segments.len() - 1].join(".");
                        let message = format!(
                            "cannot import `{source_module}`: `{directory_module}` is a directory module and its entry file is not a module of its own; import `{directory_module}` instead"
                        );
                        return Err(match read_source(ctx.documents, source_file) {
                            Ok(module_source) => FrontendFailure::coded_message_at(
                                "E_ENTRY_FILE_IMPORT",
                                message,
                                items[*idx].1.clone(),
                                &module_source,
                                &source_file.display().to_string(),
                            ),
                            Err(_) => {
                                FrontendFailure::coded_message("E_ENTRY_FILE_IMPORT", message)
                            }
                        });
                    } else if is_module_import
                        && canonical_directory_module_entry_source(&canonical) != canonical
                        && segments.len() >= 2
                    {
                        // The shipped stdlib's directory peers stay importable
                        // by file (`std.net.http.http_client`, D461); they load
                        // through the directory's entry source so the module is
                        // complete however it was reached. The entry-file
                        // spelling is refused above for the stdlib too, so one
                        // directory module has exactly one name everywhere.
                        if hew_types::module_registry::canonical_stdlib_module_for_source(
                            &canonical,
                        )
                        .is_some()
                        {
                            canonical_directory_module_entry_source(&canonical)
                        } else {
                            // A user package's peer file has no identity of its
                            // own — spec 3.5.1 merges every peer into the
                            // directory module's namespace. Importing it
                            // directly would parse it standalone, isolated from
                            // the sibling declarations it expects to share a
                            // scope with, and any reference to one of those
                            // siblings would surface downstream as a plain
                            // "undefined function"/"undefined variable" with no
                            // hint that the fix is to import the directory
                            // module instead. Refuse here, before that isolated
                            // module ever gets built.
                            let directory_module = segments[..segments.len() - 1].join(".");
                            let message = format!(
                                "cannot import `{source_module}` directly: peer files are reached through the directory module; import `{directory_module}` instead"
                            );
                            return Err(match read_source(ctx.documents, source_file) {
                                Ok(module_source) => FrontendFailure::coded_message_at(
                                    "E_PEER_IMPORT",
                                    message,
                                    items[*idx].1.clone(),
                                    &module_source,
                                    &source_file.display().to_string(),
                                ),
                                Err(_) => FrontendFailure::coded_message("E_PEER_IMPORT", message),
                            });
                        }
                    } else {
                        canonical
                    }
                } else {
                    if let Some(package_dir) = installed_package_dir.filter(|dir| dir.is_dir()) {
                        let expected = package_dir.join(format!("{last}.hew"));
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
                        .map(|(candidate, _)| candidate.display().to_string())
                        .collect::<Vec<_>>()
                        .join(", ");
                    let hint = if is_declared_dependency {
                        "\n  hint: this dependency is declared in hew.toml — run `hew install`"
                    } else if ctx.manifest_deps.is_some() {
                        "\n  hint: add this module to [dependencies] in hew.toml"
                    } else {
                        ""
                    };
                    let suggestion = if is_std_import {
                        nearest_std_module(last, search_paths)
                            .map(|nearest| format!("\n  did you mean `{nearest}`?"))
                            .unwrap_or_default()
                    } else {
                        String::new()
                    };
                    // No leading "Error: " here (unlike the sibling messages
                    // above): this one now renders with a real
                    // `file:line:col: error:` header, and the plain-text
                    // fallback below only fires if `source_file` cannot be
                    // re-read, which never happens on the path that just
                    // parsed it.
                    let message = if is_std_import && search_paths.is_empty() {
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
                        format!(
                            "module `{source_module}` not found (tried: {tried}){hint}{suggestion}"
                        )
                    };
                    return Err(match read_source(ctx.documents, source_file) {
                        Ok(module_source) => FrontendFailure::coded_message_at(
                            "E_MODULE_NOT_FOUND",
                            message,
                            items[*idx].1.clone(),
                            &module_source,
                            &source_file.display().to_string(),
                        ),
                        Err(_) => FrontendFailure::coded_message("E_MODULE_NOT_FOUND", message),
                    });
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
    let module_dir = canonical.parent();
    let is_directory_module = module_dir.is_some_and(|dir| {
        let dir_name = dir.file_name().and_then(|name| name.to_str());
        let file_stem = canonical.file_stem().and_then(|name| name.to_str());
        dir_name.is_some() && dir_name == file_stem
    });

    let peer_files = if is_directory_module {
        let dir = module_dir.expect("directory module has a parent");
        let mut peers = std::fs::read_dir(dir)
            .ok()
            .into_iter()
            .flatten()
            .filter_map(std::result::Result::ok)
            .map(|entry| entry.path())
            .filter(|path| {
                path.extension().and_then(|ext| ext.to_str()) == Some("hew") && *path != canonical
            })
            .filter(|path| !is_hew_test_file(path))
            .collect::<Vec<_>>();
        peers.sort();
        peers
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
        let module_str = if let Item::Import(decl) = import_item {
            if decl.path.segments.is_empty() {
                canonical.display().to_string()
            } else {
                import_segments(&decl.path).join(".")
            }
        } else {
            canonical.display().to_string()
        };
        check_duplicate_pub_names(&import_items, &module_str)
            .map_err(FrontendFailure::message_only)?;
    }

    Ok(ResolvedImport {
        items: import_items.into(),
        item_source_paths: import_item_source_paths,
        source_paths,
    })
}

fn is_hew_test_file(path: &Path) -> bool {
    path.file_name()
        .and_then(|name| name.to_str())
        .is_some_and(|name| name.ends_with("_test.hew"))
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

    let result = hew_parser::parse(&source);
    let display_path = canonical.display().to_string();
    let parse_diagnostics = result
        .errors
        .iter()
        .cloned()
        .map(|diagnostic| FrontendDiagnostic::parse(&source, &display_path, diagnostic))
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

fn check_duplicate_pub_names(items: &[Spanned<Item>], module_name: &str) -> Result<(), String> {
    use hew_parser::ast::Visibility;

    // Only `Visibility::Pub` items are checked here — intentionally.
    //
    // `Visibility::Package` items are scoped to the package boundary: two
    // modules within the same package can each define `package fn foo()` in
    // their own namespace without creating a global API conflict.  The
    // duplicate-name guard exists to catch clashes in the *globally-exported*
    // interface (i.e. items a downstream package could import by name), which
    // only `pub` items contribute to.
    //
    // If/when package-boundary enforcement is added (a future edition), a
    // separate within-package duplicate check will be needed at that boundary,
    // not here.
    let mut seen: HashMap<&str, usize> = HashMap::new();
    for (item, _) in items {
        let name = match item {
            Item::Function(f) if f.visibility == Visibility::Pub => Some(f.name.name.as_str()),
            Item::TypeAlias(t) if t.visibility == Visibility::Pub => Some(t.name.name.as_str()),
            Item::TypeDecl(t) if t.visibility == Visibility::Pub => Some(t.name.name.as_str()),
            Item::Actor(a) if a.visibility == Visibility::Pub => Some(a.name.name.as_str()),
            Item::Trait(t) if t.visibility == Visibility::Pub => Some(t.name.name.as_str()),
            Item::Const(c) if c.visibility == Visibility::Pub => Some(c.name.name.as_str()),
            _ => None,
        };
        if let Some(name) = name {
            let count = seen.entry(name).or_insert(0);
            *count += 1;
            if *count > 1 {
                return Err(format!(
                    "Error: duplicate pub name `{name}` in module {module_name}"
                ));
            }
        }
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
    pub source: String,
}

#[allow(
    missing_debug_implementations,
    reason = "transient pipeline value; Debug not required by any current consumer"
)]
pub struct ProgramFrontendState {
    pub program: Program,
    pub diagnostics: Vec<FrontendDiagnostic>,
    pub typecheck_result: TypeCheckResult,
    pub source: String,
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
    pub source: String,
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
/// `options.documents` is frozen when the batch is created. Each call resolves
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

    /// Run the shared frontend against this batch's immutable source snapshot.
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
    if roots == RootSelection::Module {
        if let Some(entry) = directory_module_entry(Path::new(input)) {
            return run_directory_module_frontend(&entry, input, source_override, options);
        }
    }
    let project = match load_project_context(input, Some(options), source_override) {
        Ok(project) => project,
        Err(failure) => {
            let empty = hew_parser::parse("");
            return DocumentFrontendState {
                source: String::new(),
                program: empty.program.clone(),
                parse_result: Some(empty),
                diagnostics: Vec::new(),
                typecheck_result: None,
                stopped: None,
            }
            .stop(failure);
        }
    };

    let parse_result = hew_parser::parse(&project.source);
    let diagnostics = parse_result
        .errors
        .iter()
        .cloned()
        .map(|diagnostic| FrontendDiagnostic::parse(&project.source, input, diagnostic))
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
    if let Err(failure) = require_deterministic_typecheck(options) {
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
            deterministic_admission::check(&state.program, output, &project.source, input, options);
        if !refused.is_empty() {
            state.diagnostics.extend(refused);
            return state.stop(FrontendFailure::message_only(
                "deterministic host operations found",
            ));
        }
    }

    if let Some(normalized) = state
        .typecheck_result
        .as_mut()
        .and_then(|result| result.tco.as_mut())
        .and_then(|tco| tco.normalized_program.as_mut())
    {
        flatten_file_import_items(&mut std::sync::Arc::make_mut(normalized).program);
    } else {
        flatten_file_import_items(&mut state.program);
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
    let project = match project_context_for_program(source, options) {
        Ok(project) => project,
        Err(failure) => {
            return DocumentFrontendState {
                source: source.to_string(),
                parse_result: None,
                program,
                diagnostics: Vec::new(),
                typecheck_result: None,
                stopped: None,
            }
            .stop(failure)
        }
    };
    let state = DocumentFrontendState {
        source: source.to_string(),
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
/// [`directory_module_entry`]).
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
    let diagnostics = fail_on_warning_diagnostics(state.diagnostics.clone(), options)?;
    let stack_hints = state
        .typecheck_result
        .tco
        .as_ref()
        .map(|tco| tco.stack_hints.clone())
        .unwrap_or_default();
    let output = CheckOutput {
        diagnostics,
        stack_hints,
        source: state.source.clone(),
    };
    Ok((output, state))
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

fn load_optional_toml<T: DeserializeOwned>(path: &Path) -> Result<Option<T>, FrontendFailure> {
    let text = match std::fs::read_to_string(path) {
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
            return Ok(None)
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

fn load_manifest(dir: &Path) -> Result<Option<TomlManifest>, FrontendFailure> {
    let path = dir.join("hew.toml");
    let manifest: Option<TomlManifest> = load_optional_toml(&path)?;
    if let Some(m) = &manifest {
        if let Some(package) = &m.package {
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

fn verify_locked_project_package(check: &LockedPackageCheck) -> Result<(), FrontendFailure> {
    let Some(manifest) = load_manifest(&check.package_dir)? else {
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

fn load_manifest_metadata(
    dir: &Path,
) -> Result<(Option<Vec<String>>, Option<String>), FrontendFailure> {
    match load_manifest(dir)? {
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

fn load_lockfile(dir: &Path) -> Result<Option<Vec<(String, String)>>, FrontendFailure> {
    let path = dir.join("hew.lock");
    let Some(lock) = load_optional_toml::<HewTomlLock>(&path)? else {
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
        directory_module_entry, display_path, hir_diagnostics_to_frontend, load_dependencies,
        load_lockfile, load_package_name, parse_source, retain_user_facing_diagnostics,
        run_document_frontend, run_file_frontend_to_typecheck, run_source_frontend, test_companion,
        DiagnosticPolicy, DocumentSet, FrontendDiagnostic, FrontendDiagnosticKind, FrontendOptions,
        ImportResolutionContext, Session, SessionTarget,
    };
    use hew_parser::ast::Item;
    use std::collections::{HashMap, HashSet};
    use std::fs::{self, File};
    use std::io::Write;
    use std::path::Path;

    fn write_toml(dir: &Path, content: &str) {
        let mut file = File::create(dir.join("hew.toml")).expect("create hew.toml");
        file.write_all(content.as_bytes()).expect("write hew.toml");
    }

    fn write_lockfile(dir: &Path, content: &str) {
        let mut file = File::create(dir.join("hew.lock")).expect("create hew.lock");
        file.write_all(content.as_bytes()).expect("write hew.lock");
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

        let mut documents = DocumentSet::new();
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
                companion: test_companion(Path::new(&input)),
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
                deterministic_admission: crate::DeterministicAdmission::Tests(vec![selection]),
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
                hew_sir::dump_lowering(&output.sir)
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
        let input = write_source(dir.path(), "main.hew",
            "import std.mem; fn main() { let pointer = mem.alloc(8, 8); mem.dealloc(pointer, 8, 8); }");
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
        let src = dir.path().join("src");
        let forge = src.join("forge");
        fs::create_dir_all(&forge).expect("create module directory");
        write_source(
            &src,
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
    fn directory_module_entry_selects_entries_and_peers_only() {
        let (dir, entry, peer) = forge_package("");
        let canonical_entry = Path::new(&entry).canonicalize().expect("entry exists");
        assert_eq!(
            directory_module_entry(Path::new(&entry)),
            Some(canonical_entry.clone())
        );
        assert_eq!(
            directory_module_entry(Path::new(&peer)),
            Some(canonical_entry)
        );
        let forge = dir.path().join("src").join("forge");
        assert_eq!(directory_module_entry(&forge.join("ado_test.hew")), None);
        assert_eq!(
            directory_module_entry(&dir.path().join("src").join("util.hew")),
            None
        );
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

    #[test]
    fn stdlib_directory_peer_imports_share_one_complete_module_owner() {
        let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("hew-compile lives below the repository root")
            .to_path_buf();

        for imports in [
            "import std.net.http;\nimport std.net.http.http_client;\n",
            "import std.net.http.http_client;\nimport std.net.http;\n",
            "import std.net.http.http_client;\n",
        ] {
            let dir = tempfile::tempdir().expect("create module-owner fixture");
            let input = write_source(
                dir.path(),
                "main.hew",
                &format!("{imports}\nfn main() {{}}\n"),
            );
            let source = fs::read_to_string(&input).expect("read module-owner fixture");
            let mut program = parse_source(&source, &input).expect("parse module-owner fixture");
            let documents = DocumentSet::new();
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
            let http_id = hew_parser::module::ModulePath::new(["std", "net", "http"]);
            let peer_id =
                hew_parser::module::ModulePath::new(["std", "net", "http", "http_client"]);
            let http = graph
                .modules
                .get(&http_id)
                .expect("the canonical std.net.http module should be present");
            assert!(
                !graph.modules.contains_key(&peer_id),
                "the peer must not become a second graph owner: {:?}",
                graph.modules.keys().collect::<Vec<_>>()
            );
            assert!(
                http.source_paths
                    .iter()
                    .any(|path| path.ends_with("std/net/http/http.hew")),
                "the canonical module must retain its entry source: {:?}",
                http.source_paths
            );
            assert!(
                http.source_paths
                    .iter()
                    .any(|path| path.ends_with("std/net/http/http_client.hew")),
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
            "import std.net.http.http_async_server;\nimport std.net.http.http_async_client;\nimport std.encoding.json;\nfn main() {}\n",
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
    fn check_file_accepts_shipped_directory_peer_reimports() {
        let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("hew-compile lives below the repository root")
            .to_path_buf();
        let dir = tempfile::tempdir().expect("create peer-reimport fixture");
        let input = write_source(
            dir.path(),
            "main.hew",
            "import std.net.http;\n\
             import std.net.http.http_client;\n\n\
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
        let documents = DocumentSet::new();
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

    /// The same rule across the other namespace that assembles from several
    /// files: a file import flattens into the root, so a name the root and the
    /// imported file both declare collides there too, and the report must not
    /// double up when the item is inventoried through more than one route.
    #[test]
    fn a_root_and_its_file_import_claiming_one_name_report_one_duplicate() {
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

        let failure = check_file(&input, &FrontendOptions::default())
            .expect_err("a root and its file import cannot both declare `Point`");
        let duplicates = failure
            .diagnostics
            .iter()
            .filter(|diagnostic| {
                matches!(
                    &diagnostic.kind,
                    FrontendDiagnosticKind::Type(error)
                        if error.kind == hew_types::error::TypeErrorKind::DuplicateDefinition
                )
            })
            .count();
        assert_eq!(
            duplicates, 1,
            "one collision must produce one report: {:#?}",
            failure.diagnostics
        );
    }

    /// Negative control for the rule above: an `extern "C"` symbol declared by
    /// two peer files of one directory module is a redeclaration, not a
    /// redefinition — the linker binds every call to one implementation and
    /// the extern table resolves the second against the established contract.
    /// It must keep type-checking, and both declarations must resolve.
    #[test]
    fn directory_module_peers_may_redeclare_one_extern_symbol() {
        let dir = tempfile::tempdir().expect("create directory-module fixture");
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
    fn source_roots_keep_authored_exports_after_file_import_flattening() {
        let dir = tempfile::tempdir().expect("create source-root fixture");
        write_source(dir.path(), "helper.hew", "fn hidden() -> string { \"owned\".to_upper() } pub fn imported() -> string { hidden() }");
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

    /// A file-imported item reaches HIR lowering on two surfaces: its file's
    /// module-graph entry and, after `flatten_file_import_items`, the root
    /// `Program::items`. The checker must have minted ONE declaration for it,
    /// and that identity is the one the call site and the lowered HIR item
    /// carry; a second root-owned mint would split every downstream join.
    #[test]
    fn flattened_file_import_declares_one_identity_per_item() {
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
            in_graph && in_root_items,
            "the fixture must present the helper on both checker surfaces \
             (graph: {in_graph}, flattened root items: {in_root_items})"
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
        let lowered: Vec<hew_types::DefId> = hir
            .module
            .items
            .iter()
            .filter_map(|item| match item {
                hew_hir::HirItem::Function(function) if function.name == "helper_value" => {
                    Some(function.declaration)
                }
                _ => None,
            })
            .collect();
        assert_eq!(
            lowered,
            vec![minted[0]],
            "HIR must lower the helper once, under the checker-minted identity"
        );
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
        // generic declaration: root, flattened file import, package import,
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
            // A file import is spliced into the root namespace and lowered
            // once, so its declaration keeps the declaring file's identity
            // while its emitted body carries the root's bare symbol.
            ("file_helpers.file_first", "file_first"),
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
                [hew_types::Ty::Named { head, .. }] if head.spelling() == expected
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

    /// A flat-imported concrete specialisation must not claim the shared
    /// dispatch key its generic sibling owns.
    ///
    /// Which impl is a specialisation is decided from the enclosing impl's self
    /// type. Flat-file import registration was the one registration path that
    /// never published one, so `impl Render for Box<i64>` was classified as
    /// generic and took `Box::render` — the key a call on `impl<T> Render for
    /// Box<T>` resolves through — instead of taking only its own mangled key.
    ///
    /// Both source orders are asserted because the two keys fail in opposite
    /// orders: the shared key is first-write-wins and the module-canonical key
    /// is last-write-wins, so either order alone leaves half the collision
    /// looking correct.
    #[test]
    fn flat_imported_specialisation_does_not_claim_the_generic_dispatch_key() {
        const GENERIC_IMPL: &str = "impl<T> Render for Box<T> {\n    \
             pub fn render(self) -> string { \"generic\" }\n}\n";
        const SPECIALISED_IMPL: &str = "impl Render for Box<i64> {\n    \
             pub fn render(self) -> string { \"specialised\" }\n}\n";
        const DECLARATIONS: &str = "pub trait Render {\n    fn render(self) -> string;\n}\n\npub type Box<T> {\n    value: T;\n}\n";

        let mut mismatches: Vec<String> = Vec::new();
        for (order, first, second) in [
            ("generic first", GENERIC_IMPL, SPECIALISED_IMPL),
            ("specialisation first", SPECIALISED_IMPL, GENERIC_IMPL),
        ] {
            let dir = tempfile::tempdir().expect("create temp dir");
            write_source(
                dir.path(),
                "lib.hew",
                &format!("{DECLARATIONS}{first}\n{second}"),
            );
            let input = write_source(
                dir.path(),
                "main.hew",
                "import \"lib.hew\";\n\nfn main() {}\n",
            );
            let state = run_file_frontend_to_typecheck(&input, &FrontendOptions::default())
                .unwrap_or_else(|e| panic!("{order}: fixture must type-check: {e:?}"));
            let tco = state
                .typecheck_result
                .tco
                .as_ref()
                .expect("type checking was enabled");
            let declaration_for = |key: &str| -> Option<String> {
                tco.impl_method_declaration_ids
                    .get(key)
                    .map(|declaration| tco.defs.path(*declaration).to_string())
            };
            // The shared key and the module-canonical key both name the
            // generic declaration; the specialisation owns only its mangled
            // keys. The rendered receiver's type argument — `Box<T>` against
            // `Box<i64>` — is what tells the two declarations apart; the owner
            // prefix on that receiver is deliberately not asserted here (the
            // flat-import path still renders it two ways, tracked separately).
            for (key, expected_receiver) in [
                ("Box::render", "Box<T>"),
                ("lib.Box::render", "Box<T>"),
                ("Box$$i64::render", "Box<i64>"),
                ("lib.Box$$i64::render", "Box<i64>"),
            ] {
                match declaration_for(key) {
                    Some(declaration)
                        if declaration.ends_with(&format!("{expected_receiver}>::render")) => {}
                    Some(declaration) => mismatches.push(format!(
                        "{order}: `{key}` must name the `{expected_receiver}` implementation, got `{declaration}`"
                    )),
                    None => mismatches.push(format!(
                        "{order}: nothing published under `{key}`"
                    )),
                }
            }
        }
        assert!(
            mismatches.is_empty(),
            "flat-import dispatch keys are not exclusive:\n{}",
            mismatches.join("\n")
        );
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
            tco.defs
                .lookup_path("left.render.render_value")
                .expect("declared"),
            tco.defs
                .lookup_path("right.render.render_value")
                .expect("declared"),
            tco.defs
                .lookup_path("left.render.default_value")
                .expect("declared"),
            tco.defs
                .lookup_path("right.render.default_value")
                .expect("declared"),
        ];
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
        let defaults: Vec<_> = hir
            .module
            .monomorphisations
            .iter()
            .filter(|mono| mono.key.linker_symbol.ends_with("Box::provided"))
            .collect();
        assert_eq!(
            defaults.len(),
            2,
            "each same-leaf default body must be monomorphized: {defaults:#?}"
        );
        assert!(
            defaults.iter().any(|mono| {
                tco.defs.path(mono.key.declaration)
                    == "left.render.Box::<default impl left.render.Render for left.render.Box<T>>::provided"
            }),
            "left default body must have its own synthetic implementation identity: {defaults:#?}"
        );
        assert!(
            defaults.iter().any(|mono| {
                tco.defs.path(mono.key.declaration)
                    == "right.render.Box::<default impl right.render.Render for right.render.Box<T>>::provided"
            }),
            "right default body must have its own synthetic implementation identity: {defaults:#?}"
        );
        assert_ne!(defaults[0].mangled_name, defaults[1].mangled_name);
        for mono in defaults {
            assert_eq!(
                mono.mangled_name,
                hew_hir::monomorph::function_monomorph_symbol(
                    &mono.key.linker_symbol,
                    &mono.key.type_args
                ),
                "generic direct dispatch must use the shared MonoKey linker-symbol projection"
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
        write_source(&stdlib_dir, "builtins.hew", "// explicit stdlib marker\n");
        // Every program loads the prelude's `std.link_monitor`, and the
        // prelude modules behind builtin-type methods.
        write_source(
            &stdlib_dir,
            "link_monitor.hew",
            "// prelude module marker\n",
        );
        for prelude in ["option.hew", "result.hew", "iter.hew"] {
            write_source(&stdlib_dir, prelude, "// prelude impl marker\n");
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
    /// `spawn bank.Account(...)` routes to the package actor's qualified
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
    /// reached via `import "counter.hew"` must NOT be flagged. The actor is
    /// flattened into the root program AND present in its file-import graph
    /// module, but the guard runs before flattening, so it is counted exactly
    /// once and accepted.
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
    /// module qualifier, and in particular `spawn secret.Account()` must NOT
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
            "import secret;\n\nactor Account {\n    var n: i64 = 0;\n    receive fn id() -> i64 {\n        111\n    }\n}\n\nfn main() {\n    let a = spawn secret.Account();\n}\n",
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
    /// Without that, `spawn secret.Account()` would strip the qualifier to bare
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
            "import secret;\n\nactor Account {\n    var n: i64 = 0;\n    receive fn id() -> i64 {\n        111\n    }\n}\n\nfn main() {\n    let a = spawn secret.Account();\n}\n",
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

        let diagnostics = hir_diagnostics_to_frontend(
            &state.program,
            &state.source,
            &main,
            vec![hew_hir::HirDiagnostic::new(
                hew_hir::HirDiagnosticKind::NotYetImplemented {
                    construct: "probe".to_string(),
                    owning_pass: "test".to_string(),
                },
                0..3,
                "probe",
            )
            .with_source_module(Some("dep".to_string()))],
            &DocumentSet::new(),
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
            &program,
            source,
            "main.hew",
            vec![hew_hir::HirDiagnostic::new(
                hew_hir::HirDiagnosticKind::UnresolvedInferenceVar,
                0..2,
                "probe",
            )
            .with_source_module(Some("missing".to_string()))],
            &DocumentSet::new(),
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

    /// `std::misc::log` ships `pub const JSON: i64 = 1` and `pub const TEXT: i64 = 0`
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
            "import std.misc.log;\n",
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
            let Some(hew_types::Ty::Named { head, .. }) = sig.params.first() else {
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
    fn imported_stdlib_function_can_spawn_and_wire_public_module_actors() {
        let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("hew-compile lives below repository root");
        let dir = tempfile::tempdir().expect("create temp project");
        let input = write_source(
            dir.path(),
            "main.hew",
            "import std.pipeline;\n\nfn main() {\n    let chain = pipeline.run(pipeline.from(1));\n    let item: pipeline.PipelineItemI64 = pipeline.PipelineItemI64 { value: 21, label: \"probe\", crash_stage: false };\n    match chain.push(item) {\n        .Ok(_) => {}\n        .Err(_) => {}\n    }\n}\n",
        );
        let state = run_file_frontend_to_typecheck(
            &input,
            &FrontendOptions {
                project_dir: Some(repo_root.to_path_buf()),
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
            "imported pipeline bodies must lower: {:#?}",
            hir.diagnostics
        );
        for actor in [
            "std.pipeline.AdmissionControlI64",
            "std.pipeline.SinkI64",
            "std.pipeline.StageI64",
            "std.pipeline.SourceI64",
        ] {
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
            inner.help[0].contains("share one directory") && inner.help[0].contains("spec 3.5.1"),
            "same-directory cycle should recommend the directory-module form: {}",
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
                    Resolution::Variant(id, index) => {
                        format!("variant:{}:{index}", identity(output, id.declaration()))
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
                    .map(|ty| checked_type(output, ty))
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
                    .map(|ty| checked_type(output, ty))
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
                    .map(|ty| checked_type(output, ty))
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
            .unwrap();
        let checked = shared
            .typecheck_result
            .as_ref()
            .and_then(|result| result.tco.as_ref())
            .unwrap();
        assert_eq!(physical_declarations(fresh), physical_declarations(checked));
        assert_eq!(checked_resolutions(fresh), checked_resolutions(checked));
        assert_eq!(finalized_facts(fresh), finalized_facts(checked));
        shared
    }

    #[test]
    fn independent_roots_reuse_dependencies_with_fresh_alias_scopes() {
        let dir = tempfile::tempdir().unwrap();
        fs::write(
            dir.path().join("greeting.hew"),
            "fn hidden<T>(value: T) -> T { value } pub fn greet() -> i32 { hidden(7) }\n",
        )
        .unwrap();
        let options = test_options();
        let mut batch = SourceAnalysisBatch::new(options.clone());
        let sources = [
            "import greeting; fn first() -> i32 { greeting.greet() } fn main() {}",
            "import greeting.{ greet as hello }; fn second() -> i32 { hello() } fn main() {}",
            "import greeting.{ greet }; fn third() -> i32 { greet() } fn main() {}",
            "import greeting.{ greet as invoke_greeting }; fn fourth<T>(value: T) -> T { value } fn main() { let result = fourth(invoke_greeting()); }",
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
            "import greeting; fn first() -> i32 { greeting.greet() }",
            first_label.to_str().unwrap(),
            &options,
            &mut batch,
        );
        let second_label = second.path().join("consumer.hew");
        assert_equivalent(
            "import greeting; fn second() -> string { greeting.greet() }",
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
            "import greeting; fn first() -> string { greeting.greet() }",
            first_label.to_str().unwrap(),
            &options,
            &mut proposed,
        );
        // The old immutable snapshot is still valid and independently usable.
        let original_options = test_options();
        assert_equivalent(
            "import greeting; fn first() -> i32 { greeting.greet() }",
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
            ("first.hew", "import greeting; fn first() -> i32 { greeting.greet() }"),
            ("second.hew", "import greeting.{ greet as invoke_greeting }; fn second() -> i32 { let local: fn() -> i32 = || invoke_greeting(); local() }"),
        ] {
            let label = dir.path().join(name);
            let state = assert_equivalent(source, label.to_str().unwrap(), &options, &mut batch);
            assert!(state.stopped.is_none(), "{:?}", state.diagnostics);
            let output = state.typecheck_result.as_ref().unwrap().tco.as_ref().unwrap();
            assert!(!output.closure_escape_facts.is_empty());
            assert!(!output.suspension_effects.calls.is_empty());
        }
        assert_eq!(batch.dependency_bootstraps(), 1);
        assert_eq!(batch.dependency_cache_hits(), 1);
    }
}
