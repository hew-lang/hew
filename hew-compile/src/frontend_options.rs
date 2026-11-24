//! Frontend configuration options and host-operation admission settings.

use std::path::PathBuf;
use std::sync::Arc;

use crate::diagnostics::FrontendFailure;
use crate::source_set::SourceSet;

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

pub(crate) fn require_host_typecheck(options: &FrontendOptions) -> Result<(), FrontendFailure> {
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
    /// Exact roots used to resolve `std.*` modules, and the only roots whose
    /// sources are the canonical standard library. When unset, use `HEW_STD`
    /// or the standard library shipped with the compiler. In-memory hosts can
    /// set these roots to match their retained source paths. Read them
    /// through [`FrontendOptions::std_roots`].
    ///
    /// Every root given here is trusted as the standard library: the std
    /// authority is exactly these roots, with no other root consulted.
    pub module_search_paths: Option<Vec<PathBuf>>,
    /// Treat warning-severity diagnostics as hard errors.
    ///
    /// When `true`, [`crate::check_file`], [`crate::check_program`], [`compile_file`], and
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

impl FrontendOptions {
    /// The standard-library roots of this run: `module_search_paths` when
    /// given, otherwise `HEW_STD` or the compiler's shipped std. Each root
    /// holds a `std/` directory. Import resolution, the checker's canonical-std
    /// provenance and diagnostic ownership all read this one answer. Every
    /// returned root is trusted as the std; the std authority is exactly
    /// these roots.
    #[must_use]
    pub fn std_roots(&self) -> Vec<PathBuf> {
        self.module_search_paths
            .clone()
            .unwrap_or_else(hew_types::module_registry::stdlib_search_paths)
    }
}
