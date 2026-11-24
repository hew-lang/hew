//! Shared frontend compilation drivers for files, documents, and in-memory programs.

use std::path::Path;
use std::sync::Arc;

use hew_parser::ast::{ImportDecl, Item, Program};

use crate::compile_time;
use crate::diagnostics::{
    build_module_source_map, configured_stdlib_roots, fail_on_warning_diagnostics,
    is_stdlib_owned_diagnostic, is_warning_diagnostic, macro_type_origin, merge_prior_diagnostics,
    retain_user_facing_diagnostics, type_diagnostic_to_frontend, CheckOutput, FrontendDiagnostic,
    FrontendFailure, SourceSnapshot,
};
use crate::frontend_options::{require_host_typecheck, FrontendOptions};
use crate::host_admission;
use crate::manifest::{load_project_context, project_context_for_program, ProjectContext};
use crate::module_graph::resolve_imports_internal;
use crate::module_loader::{
    canonical_direct_stdlib_module_for_source, file_import, module_membership,
};
use crate::source_set::read_source;

#[derive(Debug)]
pub struct TypeCheckResult {
    /// Exact admitted source text and routing retained for later compiler stages.
    pub sources: Arc<SourceSnapshot>,
    pub tco: Option<hew_types::check::TypeCheckOutput>,
    pub module_registry: hew_types::module_registry::ModuleRegistry,
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
    let std_roots = options.std_roots();
    let module_registry = hew_types::module_registry::ModuleRegistry::new(std_roots.clone());

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
        std_roots.clone(),
    )));
    if options.enable_wasm_target {
        checker.enable_wasm_target();
    }
    if options.repl_fragment {
        checker.set_repl_fragment();
    }
    if !options.test_entry_selections.is_empty() {
        checker.set_test_entry_selections(options.test_entry_selections.clone());
        if let Some(module) =
            canonical_direct_stdlib_module_for_source(Path::new(input), &std_roots)
        {
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
            type_diagnostic_to_frontend(
                macro_type_origin(program, &tco.defs, diagnostic),
                &module_source_map,
            )
        })
        .collect::<Vec<_>>();
    diagnostics.extend(tco.warnings.iter().cloned().map(|diagnostic| {
        type_diagnostic_to_frontend(
            macro_type_origin(program, &tco.defs, diagnostic),
            &module_source_map,
        )
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
    if let Some(module) = canonical_direct_stdlib_module_for_source(entry, &options.std_roots()) {
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

    pub(crate) fn into_result(self) -> Result<FileFrontendState, FrontendFailure> {
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

    /// Dependency-only finalizations recorded for this batch's checkpoints.
    #[must_use]
    pub fn dependency_finalizations(&self) -> usize {
        self.dependencies.finalizations()
    }

    /// Finalization passes roots extended from their checkpoint's memo.
    #[must_use]
    pub fn finalization_reuse(&self) -> hew_types::check::FinalizationReuse {
        self.dependencies.finalization_reuse()
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
/// [`crate::module_membership`]).
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
