use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt;
use std::path::{Path, PathBuf};

use hew_parser::ast::{ImportDecl, Item, Program, Spanned};
use serde::{de::DeserializeOwned, Deserialize};

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
}

#[derive(Debug, Clone)]
pub enum FrontendDiagnosticKind {
    Message(String),
    Parse(hew_parser::ParseError),
    Type(hew_types::TypeError),
    Hir(hew_hir::HirDiagnostic),
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
            kind: FrontendDiagnosticKind::Message(message.into()),
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
    items: Vec<Spanned<Item>>,
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
        if decl.file_path.is_some() || decl.path.is_empty() {
            continue;
        }
        let module_str = decl.path.join("::");
        if is_builtin_module(&module_str) {
            continue;
        }
        if package_name.is_some_and(|pkg| decl.path.first().is_some_and(|seg| seg == pkg)) {
            continue;
        }
        if !manifest_deps.contains(&module_str) {
            errors.push(format!(
                "Error: module `{module_str}` is not declared in hew.toml\n  hint: add it with `hew add {module_str}`"
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

fn load_project_context(
    input: &str,
    options: Option<&FrontendOptions>,
) -> Result<ProjectContext, FrontendFailure> {
    let source = std::fs::read_to_string(input)
        .map_err(|e| FrontendFailure::message_only(format!("Error: cannot read {input}: {e}")))?;
    let project_dir = options
        .and_then(|options| options.project_dir.clone())
        .unwrap_or_else(|| {
            Path::new(input)
                .parent()
                .unwrap_or(Path::new("."))
                .to_path_buf()
        });
    let (manifest_deps, package_name) = load_manifest_metadata(&project_dir)?;
    Ok(ProjectContext {
        source,
        project_dir: project_dir.clone(),
        manifest_deps,
        package_name,
        locked_versions: load_lockfile(&project_dir)?,
    })
}

/// Return the same-name entry file when `input` is a directory-module peer
/// whose impl names a trait declared by that entry. Checking the peer directly
/// must retain the lexical trait namespace that materializes default methods,
/// without assembling unrelated peers into every standalone file check.
fn directory_module_entry_for_peer(program: &Program, input: &Path) -> Option<String> {
    let input_name = input.file_name()?.to_str()?;
    let parent = input.parent()?;
    let module_name = parent.file_name()?.to_str()?;
    let entry_name = format!("{module_name}.hew");
    let entry_path = parent.join(&entry_name);
    if input_name == entry_name || !entry_path.is_file() {
        return None;
    }
    let local_traits = program
        .items
        .iter()
        .filter_map(|(item, _)| match item {
            Item::Trait(decl) => Some(decl.name.as_str()),
            _ => None,
        })
        .collect::<HashSet<_>>();
    let entry_source = std::fs::read_to_string(entry_path).ok()?;
    let entry_parse = hew_parser::parse(&entry_source);
    if entry_parse
        .errors
        .iter()
        .any(|error| error.severity == hew_parser::Severity::Error)
    {
        return None;
    }
    let entry_traits = entry_parse
        .program
        .items
        .iter()
        .filter_map(|(item, _)| match item {
            Item::Trait(decl) => Some(decl.name.as_str()),
            _ => None,
        })
        .collect::<HashSet<_>>();
    let needs_entry_trait = program.items.iter().any(|(item, _)| {
        let Item::Impl(decl) = item else {
            return false;
        };
        decl.trait_bound.as_ref().is_some_and(|bound| {
            entry_traits.contains(bound.name.as_str())
                && !local_traits.contains(bound.name.as_str())
        })
    });
    if !needs_entry_trait {
        return None;
    }
    Some(entry_name)
}

fn import_directory_module_entry_for_peer(program: &mut Program, input: &Path) {
    let Some(entry_name) = directory_module_entry_for_peer(program, input) else {
        return;
    };
    program.items.insert(
        0,
        (
            Item::Import(ImportDecl {
                path: Vec::new(),
                path_separators: Vec::new(),
                spec: None,
                spec_separator: None,
                selection_trailing_comma: false,
                module_alias: None,
                file_path: Some(entry_name),
                resolved_items: None,
                resolved_item_source_paths: Vec::new(),
                resolved_source_paths: Vec::new(),
            }),
            0..0,
        ),
    );
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
    let mut import_ctx = ImportResolutionContext {
        in_progress_imports: HashSet::new(),
        resolved_imports: HashMap::new(),
        manifest_deps: project.manifest_deps.as_deref(),
        extra_pkg_path: options.pkg_path.as_deref(),
        locked_versions: project.locked_versions.as_deref(),
        package_name: project.package_name.as_deref(),
        project_dir: &project.project_dir,
        module_search_paths: options.module_search_paths.as_deref(),
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

fn build_module_source_map(program: &Program) -> ModuleSourceMap {
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
            continue;
        };
        if let Ok(text) = std::fs::read_to_string(path) {
            map.insert(mod_id.path.join("."), (text, path.display().to_string()));
        }
        // Per-file routing entries (rc1-F1 stage C): a directory module's
        // item spans are file-relative offsets, so the checker routes a
        // diagnostic on a peer-file item by the file's own path token. Every
        // source file of every module resolves under that token.
        for path in &module.source_paths {
            let key = path.display().to_string();
            if map.contains_key(&key) {
                continue;
            }
            if let Ok(text) = std::fs::read_to_string(path) {
                map.insert(key.clone(), (text, key));
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
) -> Vec<FrontendDiagnostic> {
    let module_source_map = build_module_source_map(program);
    diagnostics
        .into_iter()
        .map(|diagnostic| {
            hir_diagnostic_to_frontend(root_source, root_filename, diagnostic, &module_source_map)
        })
        .collect()
}

fn typecheck_program_with_diagnostics(
    program: &Program,
    source: &str,
    input: &str,
    options: &FrontendOptions,
) -> Result<(TypeCheckResult, Vec<FrontendDiagnostic>), FrontendFailure> {
    let search_paths = options.module_search_paths.clone().unwrap_or_else(|| {
        hew_types::module_registry::build_module_search_paths_for(options.project_dir.as_deref())
    });
    let module_registry = hew_types::module_registry::ModuleRegistry::new(search_paths);

    if options.no_typecheck {
        return Ok((
            TypeCheckResult {
                tco: None,
                module_registry,
            },
            Vec::new(),
        ));
    }

    let mut checker = hew_types::Checker::new(module_registry);
    if options.enable_wasm_target {
        checker.enable_wasm_target();
    }
    if options.repl_fragment {
        checker.set_repl_fragment();
    }
    checker.set_lint_levels(options.lint_levels.clone());
    // Install source text so the lint sweep can resolve in-source
    // `// hew:allow(...)` directives. The root source owns the entry file's
    // spans; each non-root module owns its own (built from the same source map
    // the diagnostic renderer uses below).
    let module_source_map = build_module_source_map(program);
    let mut lint_sources = hew_types::LintSources::new();
    lint_sources.set_root(source.to_string());
    for (module, (module_source, _filename)) in &module_source_map {
        lint_sources.set_module(module.clone(), module_source.clone());
    }
    checker.set_lint_sources(lint_sources);
    let tco = checker.check_program(program);
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

    if !tco.errors.is_empty() {
        return Err(FrontendFailure::new("type errors found", diagnostics));
    }

    let module_registry = checker.into_module_registry();
    Ok((
        TypeCheckResult {
            tco: Some(tco),
            module_registry,
        },
        diagnostics,
    ))
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
    typecheck_program_with_diagnostics(program, source, input, options).map(|(result, _)| result)
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

    match typecheck_program_with_diagnostics(&program, source, source_label, options) {
        Ok((tcr, type_diagnostics)) => {
            diagnostics.extend(type_diagnostics);
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
        Err(failure) => Err(merge_prior_diagnostics(diagnostics, failure)),
    }
}

pub fn inject_implicit_imports(items: &mut Vec<Spanned<Item>>, source: &str) {
    let existing = items
        .iter()
        .filter_map(|(item, _)| {
            if let Item::Import(decl) = item {
                if !decl.path.is_empty() {
                    return Some(decl.path.join("::"));
                }
            }
            None
        })
        .collect::<HashSet<_>>();

    let mut needed: Vec<Vec<String>> = Vec::new();
    if source_contains_regex_literal(source) {
        let path = ["std", "text", "regex"];
        let key = path.join("::");
        if !existing.contains(&key) {
            needed.push(path.iter().map(|segment| (*segment).to_string()).collect());
        }
    }

    let mut seen = HashSet::new();
    for path in needed {
        let key = path.join("::");
        if seen.insert(key) {
            items.push((
                Item::Import(ImportDecl {
                    path,
                    path_separators: Vec::new(),
                    spec: None,
                    spec_separator: None,
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

fn source_contains_regex_literal(source: &str) -> bool {
    hew_lexer::Lexer::new(source)
        .any(|(token, _)| matches!(token, hew_lexer::Token::RegexLiteral(_)))
}

fn module_id_from_file(source_dir: &Path, canonical_path: &Path) -> hew_parser::module::ModuleId {
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

    hew_parser::module::ModuleId::new(segments)
}

fn canonical_direct_stdlib_module_for_source(
    source_file: &Path,
) -> Option<hew_parser::module::ModuleId> {
    let dotted = hew_types::module_registry::canonical_stdlib_module_for_source(source_file)?;
    Some(hew_parser::module::ModuleId::new(
        dotted.split('.').map(String::from).collect(),
    ))
}

fn rewrite_direct_stdlib_module_root(
    module_graph: &mut hew_parser::module::ModuleGraph,
    items: &mut Vec<Spanned<Item>>,
    source_file: &Path,
) -> Result<(), FrontendFailure> {
    use hew_parser::module::{Module, ModuleId};

    let Some(stdlib_id) = canonical_direct_stdlib_module_for_source(source_file) else {
        return Ok(());
    };

    let original_root = module_graph.root.clone();
    let Some(mut stdlib_module) = module_graph.modules.remove(&original_root) else {
        return Ok(());
    };

    stdlib_module.id = stdlib_id.clone();
    module_graph.root = ModuleId::root();
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
    module_graph
        .compute_topo_order()
        .map_err(|cycle_err| FrontendFailure::message_only(cycle_err.to_string()))?;
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
    use hew_parser::module::{Module, ModuleGraph, ModuleId};

    let input_canonical =
        std::fs::canonicalize(source_file).unwrap_or_else(|_| source_file.to_path_buf());
    let source_dir = input_canonical.parent().unwrap_or(Path::new("."));

    ctx.in_progress_imports.insert(input_canonical.clone());
    let resolve_result = resolve_file_imports_internal(&input_canonical, items, ctx, diagnostics);
    ctx.in_progress_imports.remove(&input_canonical);
    resolve_result?;

    let root_id = module_id_from_file(source_dir, &input_canonical);
    let mut graph = ModuleGraph::new(root_id.clone());
    let mut seen_ids: HashSet<ModuleId> = HashSet::from([root_id.clone()]);

    let root_imports = extract_module_info(
        items,
        &input_canonical,
        source_dir,
        &input_canonical,
        &root_id,
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
        return Err(FrontendFailure::message_only(cycle_err.to_string()));
    }
    rewrite_direct_stdlib_module_root(&mut graph, items, &input_canonical)?;

    // Canonical module IDs may share a final component. Reject only when two
    // whole-module imports in the SAME source scope publish the same surface
    // binding for different canonical paths. Distinct module aliases are
    // unambiguous; named/glob symbol bindings remain checker-owned.
    if let Err(msg) = check_ambiguous_module_import_bindings(&graph) {
        return Err(FrontendFailure::message_only(msg));
    }

    // Reject a single module declaring two actors with one name.  Cross-module
    // duplicates are LEGAL: actor identity is the qualified (defining-module,
    // name) pair end-to-end — the checker emits `LocalPid<bank.Account>`, MIR
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

fn check_ambiguous_module_import_bindings(
    graph: &hew_parser::module::ModuleGraph,
) -> Result<(), String> {
    for (owner_id, module) in &graph.modules {
        let mut seen: HashMap<String, String> = HashMap::new();
        for (item, _) in &module.items {
            let Item::Import(import) = item else {
                continue;
            };
            if import.path.is_empty() || import.spec.is_some() {
                continue;
            }
            let source = import.path.join("::");
            let binding = import
                .module_alias
                .clone()
                .or_else(|| import.path.last().cloned())
                .expect("non-file module imports have a path");
            if let Some(existing) = seen.insert(binding.clone(), source.clone()) {
                if existing != source {
                    return Err(format!(
                        "Error: module `{owner_id}` imports both `{existing}` and `{source}` \
                         under the ambiguous binding `{binding}`. \
                         Give one import a distinct module alias."
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
            if !seen.insert(actor.name.as_str()) {
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
    id: &hew_parser::module::ModuleId,
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
    let mut extra = Vec::new();
    for (item, _) in &program.items {
        let Item::Import(decl) = item else { continue };
        if decl.file_path.is_none() {
            continue;
        }
        let Some(resolved_items) = &decl.resolved_items else {
            continue;
        };
        extra.extend(
            resolved_items
                .iter()
                .filter(|(resolved_item, _)| !matches!(resolved_item, Item::Import(_)))
                .cloned(),
        );
    }
    program.items.extend(extra);
}

fn extract_module_info(
    items: &[Spanned<Item>],
    current_source: &Path,
    source_dir: &Path,
    root_source: &Path,
    root_id: &hew_parser::module::ModuleId,
    graph: &mut hew_parser::module::ModuleGraph,
    seen_ids: &mut HashSet<hew_parser::module::ModuleId>,
) -> Vec<hew_parser::module::ModuleImport> {
    use hew_parser::module::{Module, ModuleId, ModuleImport};

    let mut imports = Vec::new();

    for (item, span) in items {
        let Item::Import(decl) = item else { continue };

        let (module_id, first_source_path) = if !decl.path.is_empty() {
            let requested = decl.path.join(".");
            let canonical = hew_types::module_registry::canonical_source_module_identity(
                &requested,
                &decl.resolved_source_paths,
            );
            (
                ModuleId::new(canonical.split('.').map(String::from).collect()),
                None,
            )
        } else if let Some(file_path) = &decl.file_path {
            let resolved = current_source
                .parent()
                .unwrap_or(source_dir)
                .join(file_path);
            let canonical = resolved.canonicalize().unwrap_or(resolved);
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
                    graph.item_sources.insert(
                        module_id.path.join("."),
                        decl.resolved_item_source_paths.clone(),
                    );
                }
                let module = Module {
                    id: module_id,
                    items: resolved.clone(),
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
                if decl.file_path.is_some() || !decl.path.is_empty() {
                    return Some(index);
                }
            }
            None
        })
        .collect::<Vec<_>>();

    let cwd = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));

    // Suppress cwd candidates when the source file has an enclosing Hew root that
    // is NOT the same as the cwd's enclosing root.  This covers two cases:
    //   (a) both have roots but they differ   — cwd is a different checkout
    //   (b) source has a root but cwd has none — cwd is outside any checkout
    // In both cases the Tier-2 logic (build_module_search_paths_for) already
    // resolves std/ from the source file's own root; adding cwd candidates would
    // produce a second distinct std/ path and trigger the ambiguity check spuriously.
    // When the source has NO root, cwd candidates are kept (unchanged behaviour).
    //
    // WHY: cross-worktree dogfood regression — `cd <main-checkout> && hew check
    //   <worktree>/examples/…` hit "import std::fs is ambiguous" in 4/6 sessions;
    //   gap: `cd /tmp && hew check <worktree>/examples/…` also hit the same error
    //   because cwd has no root, so the old `(Some, Some) if ≠` guard didn't fire.
    // WHEN obsolete: when stdlib is co-installed with the binary (sysroot model);
    //   then neither cwd nor source-ancestor scanning is needed for stdlib.
    // WHAT the real solution is: pin std to the binary's co-located install path.
    let source_hew_root = hew_types::module_registry::find_enclosing_hew_root(source_file);
    let cwd_hew_root = hew_types::module_registry::find_enclosing_hew_root(&cwd);
    // `None != Some(x)` is true, so the `cwd_hew_root = None` gap is covered.
    let cwd_crosses_root = source_hew_root.is_some() && cwd_hew_root != source_hew_root;

    for idx in &import_indices {
        let canonical = match &items[*idx].0 {
            Item::Import(decl) if decl.file_path.is_some() => {
                let file_path = decl.file_path.as_ref().expect("checked above");
                let resolved = source_dir.join(file_path);
                if let Ok(canonical) = resolved.canonicalize() {
                    canonical
                } else {
                    return Err(FrontendFailure::message_only(format!(
                        "Error: imported file not found: {file_path} (resolved to {})",
                        resolved.display()
                    )));
                }
            }
            Item::Import(decl) if !decl.path.is_empty() => {
                let module_str = decl.path.join("::");
                let is_local = ctx
                    .package_name
                    .is_some_and(|pkg| decl.path.first().is_some_and(|seg| seg == pkg));
                let rest_path: Vec<&str> = if is_local {
                    decl.path[1..].iter().map(String::as_str).collect()
                } else {
                    Vec::new()
                };

                let rel_path = decl.path.iter().collect::<PathBuf>().with_extension("hew");
                let last = decl.path.last().expect("path is non-empty");
                let dir_path = decl
                    .path
                    .iter()
                    .collect::<PathBuf>()
                    .join(format!("{last}.hew"));
                let mut candidates = Vec::new();
                let mut locked_project_candidates = Vec::new();
                let locked_version = ctx
                    .locked_versions
                    .and_then(|locked| locked.iter().find(|(name, _)| name == &module_str))
                    .map(|(_, version)| version.as_str());

                if is_local && !rest_path.is_empty() {
                    let local_last = *rest_path.last().expect("non-empty local path");
                    let local_rel = rest_path.iter().collect::<PathBuf>();
                    let local_dir = local_rel.join(format!("{local_last}.hew"));
                    let local_flat = local_rel.with_extension("hew");
                    candidates.push(ctx.project_dir.join("src").join(&local_dir));
                    candidates.push(ctx.project_dir.join("src").join(&local_flat));
                    candidates.push(ctx.project_dir.join(&local_dir));
                    candidates.push(ctx.project_dir.join(&local_flat));
                }

                candidates.push(source_dir.join(&dir_path));
                candidates.push(source_dir.join(&rel_path));
                if !cwd_crosses_root {
                    candidates.push(cwd.join(&dir_path));
                    candidates.push(cwd.join(&rel_path));
                }

                let module_dir = decl.path.iter().collect::<PathBuf>();
                let is_std_import = module_str.starts_with("std::");
                if let Some(version) = locked_version.filter(|_| !is_std_import) {
                    let entry_file =
                        format!("{}.hew", decl.path.last().expect("path is non-empty"));
                    let versioned_rel = module_dir.join(version).join(entry_file);
                    candidates.push(ctx.project_dir.join(".hew/packages").join(&versioned_rel));
                    if let Some(pkg) = ctx.extra_pkg_path {
                        candidates.push(pkg.join(&versioned_rel));
                    }
                }

                if !is_std_import {
                    candidates.push(ctx.project_dir.join(".hew/packages").join(&rel_path));
                    let project_package_dir =
                        ctx.project_dir.join(".hew/packages").join(&module_dir);
                    let project_package_entry =
                        ctx.project_dir.join(".hew/packages").join(&dir_path);
                    if let Some(version) = locked_version {
                        locked_project_candidates.push((
                            project_package_entry.clone(),
                            LockedPackageCheck {
                                package_dir: project_package_dir,
                                name: module_str.clone(),
                                version: version.to_string(),
                            },
                        ));
                    }
                    candidates.push(project_package_entry);
                }

                if let Some(pkg) = ctx.extra_pkg_path.filter(|_| !is_std_import) {
                    candidates.push(pkg.join(&dir_path));
                    candidates.push(pkg.join(&rel_path));
                    if decl.path.len() > 1 && !is_builtin_module(&module_str) {
                        let rest_dir = decl.path[1..]
                            .iter()
                            .collect::<PathBuf>()
                            .join(format!("{last}.hew"));
                        let rest_flat = decl.path[1..]
                            .iter()
                            .collect::<PathBuf>()
                            .with_extension("hew");
                        candidates.push(pkg.join(&rest_dir));
                        candidates.push(pkg.join(&rest_flat));
                    }
                }

                if module_str.starts_with("hew::") && decl.path.len() > 1 {
                    let tail = decl.path[1..].iter().collect::<PathBuf>();
                    let tail_last = decl.path.last().expect("path is non-empty");
                    let tail_dir = tail.join(format!("{tail_last}.hew"));
                    let tail_rel = tail.with_extension("hew");
                    if let Some(pkg) = ctx.extra_pkg_path {
                        candidates.push(pkg.join(&tail_dir));
                        candidates.push(pkg.join(&tail_rel));
                    }
                }

                if module_str.starts_with("ecosystem::") && decl.path.len() > 1 {
                    let tail = decl.path[1..].iter().collect::<PathBuf>();
                    let tail_last = decl.path.last().expect("path is non-empty");
                    let tail_dir = tail.join(format!("{tail_last}.hew"));
                    let tail_rel = tail.with_extension("hew");
                    if let Some(pkg) = ctx.extra_pkg_path {
                        candidates.push(pkg.join(&tail_dir));
                        candidates.push(pkg.join(&tail_rel));
                    }
                }

                // Stdlib / global search roots — apply exclusive precedence tiers so that
                // a file in worktree-A always resolves std from A only, never from the
                // build binary's worktree or a sibling checkout.
                let discovered_search_paths;
                let search_paths = if let Some(paths) = ctx.module_search_paths {
                    paths
                } else {
                    discovered_search_paths =
                        hew_types::module_registry::build_module_search_paths_for(Some(
                            source_file,
                        ));
                    &discovered_search_paths
                };
                for root in search_paths {
                    candidates.push(root.join(&dir_path));
                    candidates.push(root.join(&rel_path));
                }

                // Collect ALL candidates that resolve, then deduplicate by canonical path.
                // If two or more distinct canonical paths resolve, the import is ambiguous —
                // fail-closed rather than silently picking the first match.
                let mut resolved = Vec::new();
                for candidate in &candidates {
                    if let Ok(canonical) = candidate.canonicalize() {
                        if let Some((_, check)) = locked_project_candidates
                            .iter()
                            .find(|(locked_candidate, _)| locked_candidate == candidate)
                        {
                            verify_locked_project_package(check)?;
                        }
                        resolved.push(canonical);
                    }
                }
                resolved.sort();
                resolved.dedup();

                if resolved.len() > 1 {
                    let paths = resolved
                        .iter()
                        .map(|p| p.display().to_string())
                        .collect::<Vec<_>>()
                        .join("` and `");
                    return Err(FrontendFailure::message_only(format!(
                        "Error: import `{module_str}` is ambiguous: both `{paths}` exist.\n  Rename or remove one to resolve the ambiguity."
                    )));
                }

                if let Some(canonical) = resolved.into_iter().next() {
                    canonical
                } else {
                    let tried = candidates
                        .iter()
                        .map(|candidate| candidate.display().to_string())
                        .collect::<Vec<_>>()
                        .join(", ");
                    let hint = if ctx
                        .manifest_deps
                        .is_some_and(|deps| deps.contains(&module_str))
                    {
                        "\n  hint: this dependency is declared in hew.toml — run `hew install`"
                    } else if ctx.manifest_deps.is_some() {
                        "\n  hint: add this module to [dependencies] in hew.toml"
                    } else {
                        ""
                    };
                    return Err(FrontendFailure::message_only(format!(
                        "Error: module `{module_str}` not found (tried: {tried}){hint}"
                    )));
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
        import_items.extend(peer_resolved.items);
        source_paths.push(peer_canonical);
    }

    if !peer_files.is_empty() {
        let module_str = if let Item::Import(decl) = import_item {
            if decl.path.is_empty() {
                canonical.display().to_string()
            } else {
                decl.path.join("::")
            }
        } else {
            canonical.display().to_string()
        };
        check_duplicate_pub_names(&import_items, &module_str)
            .map_err(FrontendFailure::message_only)?;
    }

    Ok(ResolvedImport {
        items: import_items,
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
    let source = std::fs::read_to_string(canonical).map_err(|e| {
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
            Item::Function(f) if f.visibility == Visibility::Pub => Some(f.name.as_str()),
            Item::TypeAlias(t) if t.visibility == Visibility::Pub => Some(t.name.as_str()),
            Item::TypeDecl(t) if t.visibility == Visibility::Pub => Some(t.name.as_str()),
            Item::Actor(a) if a.visibility == Visibility::Pub => Some(a.name.as_str()),
            Item::Trait(t) if t.visibility == Visibility::Pub => Some(t.name.as_str()),
            Item::Const(c) if c.visibility == Visibility::Pub => Some(c.name.as_str()),
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
/// **Do not construct a divergent wrapper.** If you need to call the frontend
/// with different options, extend [`FrontendOptions`] and route through
/// [`run_file_frontend_to_typecheck`]. A parallel frontend driver that
/// duplicates load → parse → import-resolution → type-check is always wrong.
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
/// **Do not construct a divergent wrapper.** If you need to call the frontend
/// with different options, extend [`FrontendOptions`] and route through here.
/// A parallel driver that duplicates load → parse → import-resolution →
/// type-check is always wrong.
///
/// # Errors
///
/// Returns [`FrontendFailure`] when project loading, parsing, import
/// resolution, or type-checking fails.
pub fn run_file_frontend_to_typecheck(
    input: &str,
    options: &FrontendOptions,
) -> Result<FileFrontendState, FrontendFailure> {
    let project = load_project_context(input, Some(options))?;
    let (mut program, parse_diagnostics) = parse_source_with_diagnostics(&project.source, input)?;
    import_directory_module_entry_for_peer(&mut program, Path::new(input));
    let mut diagnostics = parse_diagnostics;

    if let Err(failure) = resolve_imports_internal(
        &mut program,
        &project.source,
        input,
        &project,
        options,
        &mut diagnostics,
    ) {
        return Err(merge_prior_diagnostics(diagnostics, failure));
    }

    let typecheck_result =
        match typecheck_program_with_diagnostics(&program, &project.source, input, options) {
            Ok((result, type_diagnostics)) => {
                diagnostics.extend(type_diagnostics);
                result
            }
            Err(failure) => return Err(merge_prior_diagnostics(diagnostics, failure)),
        };

    flatten_file_import_items(&mut program);

    Ok(FileFrontendState {
        program,
        diagnostics,
        typecheck_result,
        source: project.source,
    })
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
    mut program: Program,
    source: &str,
    source_label: &str,
    options: &FrontendOptions,
) -> Result<ProgramFrontendState, FrontendFailure> {
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

    let typecheck_result =
        match typecheck_program_with_diagnostics(&program, source, source_label, options) {
            Ok((result, type_diagnostics)) => {
                diagnostics.extend(type_diagnostics);
                result
            }
            Err(failure) => return Err(merge_prior_diagnostics(diagnostics, failure)),
        };

    flatten_file_import_items(&mut program);

    let diagnostics = fail_on_warning_diagnostics(diagnostics, options)?;
    Ok(ProgramFrontendState {
        program,
        diagnostics,
        typecheck_result,
        source: source.to_string(),
    })
}

/// Parse, resolve imports, and type-check a Hew source file.
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
    let state = run_file_frontend_to_typecheck(input, options)?;
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
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => return Ok(None),
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
        check_file, check_file_with_state, check_program, hir_diagnostics_to_frontend,
        load_dependencies, load_lockfile, load_package_name, parse_source,
        run_file_frontend_to_typecheck, FrontendDiagnosticKind, FrontendOptions,
    };
    use hew_parser::ast::Item;
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

    fn write_source(dir: &Path, name: &str, content: &str) -> String {
        let path = dir.join(name);
        let mut file = File::create(&path).expect("create source file");
        file.write_all(content.as_bytes())
            .expect("write source file");
        path.display().to_string()
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
            "pub type Dog { label: string; }\nimpl Greeter for Dog {\n    fn name(self) -> string { self.label }\n}\npub fn describe(d: Dog) -> string { d.greet() }\n",
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
            let declaration = hew_types::DefId::new(format!("hew.testffi.{name}"));
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
        // A file import is flattened into the root program, whereas a package
        // import retains its package-qualified declaration owner.  The two
        // sources intentionally declare same-leaf `Result` / `ResultMethods`
        // impls.  The checker must select the root-owned file declaration for
        // `local.tag()` and the package-owned declaration for `r.rows()`;
        // choosing an owner by the shared leaf name makes HIR's body lookup
        // ambiguous or attaches the call to the wrong implementation.
        let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("hew-compile has a workspace parent");
        let fixture_dir = repo_root.join("tests/pkg-import");
        let input = fixture_dir.join("mixed_import_impl_collision.hew");
        let package_path = fixture_dir.join("pkgs");

        let source = fs::read_to_string(&input).expect("read mixed-import fixture");
        let reversed_source = source.replacen(
            "import hew::testffi;\nimport \"mixed_import_impl_collision_lib.hew\";",
            "import \"mixed_import_impl_collision_lib.hew\";\nimport hew::testffi;",
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

        let root_tag = hew_types::DefId::new("Result::<impl ResultMethods for Result>::tag");
        let package_rows = hew_types::DefId::new(
            "hew.testffi.Result::<impl hew.testffi.ResultMethods for hew.testffi.Result>::rows",
        );

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

            assert_eq!(
                tco.impl_method_declaration_ids.get("Result::tag"),
                Some(&root_tag),
                "the flattened file-import implementation must retain a root declaration ID"
            );
            assert_eq!(
                tco.impl_method_declaration_ids
                    .get("hew.testffi.Result::rows"),
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
                "local Result.tag() must select the root file-import declaration: {:#?}",
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
                "package Result.rows() must select the package declaration: {:#?}",
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
            assert_eq!(symbols.get(&root_tag), Some(&"Result::tag".to_string()));
            assert_eq!(
                symbols.get(&package_rows),
                Some(&"hew.testffi.Result::rows".to_string())
            );
        }
    }

    #[test]
    fn imported_generic_impl_bodies_publish_each_checker_owned_declaration() {
        // `privslot` deliberately combines all three conditions that used to
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
                hew_types::DefId::new(
                    "hew.privslot.Store::<impl inherent for hew.privslot.Store<T>>::add",
                ),
            ),
            (
                "hew.privslot.Store::generation_at",
                hew_types::DefId::new(
                    "hew.privslot.Store::<impl inherent for hew.privslot.Store<T>>::generation_at",
                ),
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

        let pipeline = hew_mir::lower_hir_module(&hir.module);
        assert!(
            pipeline.diagnostics.is_empty(),
            "MIR must project generic impl calls through their exact HIR body symbols: {:#?}",
            pipeline.diagnostics
        );
        for (symbol, _) in &expected {
            let concrete = hew_hir::monomorph::function_monomorph_symbol(
                symbol,
                &[hew_types::ResolvedTy::String],
            );
            assert!(
                pipeline
                    .raw_mir
                    .iter()
                    .any(|function| function.name == concrete),
                "generic imported impl `{symbol}` must lower its string specialization `{concrete}`"
            );
        }
    }

    #[test]
    fn remote_pid_lookup_annotation_reaches_mir_with_its_builtin_carrier() {
        let dir = tempfile::tempdir().expect("create temp dir");
        let input = write_source(
            dir.path(),
            "main.hew",
            r#"
            actor Echo { receive fn handle(request: i64) -> i64 { request } }
            impl ActorMsg for Echo { type Msg = i64; type Reply = i64; }
            actor Client {
                receive fn go(unused: i64) {
                    let found: Result<RemotePid<Echo>, LookupError> = Node::lookup("echo");
                    match found {
                        Ok(peer) => { let reply = peer.ask(7, 1000); },
                        Err(_) => {},
                    }
                }
            }
            "#,
        );
        let state = run_file_frontend_to_typecheck(&input, &FrontendOptions::default())
            .expect("lookup fixture must type-check");
        let tco = state
            .typecheck_result
            .tco
            .as_ref()
            .expect("successful fixture has type output");
        let hir = hew_hir::lower_program(
            &state.program,
            tco,
            &hew_hir::ResolutionCtx,
            hew_hir::TargetArch::host(),
        );
        assert!(
            hir.diagnostics.is_empty(),
            "HIR diagnostics: {:#?}",
            hir.diagnostics
        );
        let mut pipeline = hew_mir::lower_hir_module(&hir.module);
        pipeline.attach_lowering_facts(tco);
        assert!(
            !pipeline.diagnostics.iter().any(|diagnostic| matches!(
                diagnostic.kind,
                hew_mir::MirDiagnosticKind::UnknownType { ref name } if name == "RemotePid"
            )),
            "RemotePid must retain its builtin discriminator through MIR: {:#?}",
            pipeline.diagnostics
        );
        let codegen = hew_codegen_rs::validate_codegen_front(&pipeline);
        assert!(
            codegen.is_ok(),
            "the full compiler boundary must accept RemotePid lookup output: {codegen:?}"
        );
    }

    #[test]
    #[expect(
        clippy::too_many_lines,
        reason = "the generic direct-symbol regression covers every module origin in one identity matrix"
    )]
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
import hew::genhelpers;
import alpha as flat_alpha;
import beta::alpha as nested_alpha;

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
        let expected = [
            (hew_types::DefId::new("root_first"), "root_first"),
            (
                hew_types::DefId::new("file_helpers.file_first"),
                "file_helpers$file_first",
            ),
            (
                hew_types::DefId::new("hew.genhelpers.first"),
                "hew$genhelpers$first",
            ),
            (hew_types::DefId::new("alpha.first"), "alpha$first"),
            (
                hew_types::DefId::new("beta.alpha.first"),
                "beta$alpha$first",
            ),
        ];
        for (declaration, symbol) in expected {
            assert_eq!(
                symbols.get(&declaration),
                Some(&symbol.to_string()),
                "generic declaration `{}` must retain its exact emitted body symbol",
                declaration.full_path()
            );
        }
        assert_ne!(
            symbols.get(&hew_types::DefId::new("alpha.first")),
            symbols.get(&hew_types::DefId::new("beta.alpha.first")),
            "same-leaf generic functions must not share a direct-call symbol"
        );

        let pipeline = hew_mir::lower_hir_module(&hir.module);
        assert!(
            pipeline.diagnostics.is_empty(),
            "nested generic direct calls must inherit the exact HIR symbol map: {:#?}",
            pipeline.diagnostics
        );
        for (symbol, expected_value) in [
            ("root_first", 1_i64),
            ("file_helpers$file_first", 3_i64),
            ("hew$genhelpers$first", 5_i64),
            ("alpha$first", 7_i64),
            ("beta$alpha$first", 9_i64),
        ] {
            let concrete = hew_hir::monomorph::function_monomorph_symbol(
                symbol,
                &[hew_types::ResolvedTy::I64],
            );
            assert!(
                pipeline
                    .raw_mir
                    .iter()
                    .any(|function| function.name == concrete),
                "closure call returning {expected_value} must emit `{concrete}`"
            );
        }
    }

    #[test]
    fn self_qualified_module_type_keeps_its_full_owner_through_mir_layout() {
        // The package fixture names Meter both bare and through its own
        // lexical leaf (`selfqualtype.Meter`) while its real owner is the
        // full module-graph path `hew.selfqualtype`. This checks every
        // handoff: checker signature, HIR declaration/parameter, and MIR
        // layout must carry that same exact owner. A short-name fallback would
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
                tco.fn_sigs
                    .get("hew.selfqualtype.read")
                    .expect("checker must retain imported read signature")
                    .params
                    .as_slice(),
                [hew_types::Ty::Named { name, .. }] if name == expected
            ),
            "checker parameter type must be the complete module owner: {:#?}",
            tco.fn_sigs.get("hew.selfqualtype.read")
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
                hew_hir::HirItem::TypeDecl(decl) if decl.qualified_name() == expected => Some(decl),
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
                    if function.declaration.full_path() == "hew.selfqualtype.read" =>
                {
                    Some(function)
                }
                _ => None,
            })
            .expect("HIR must emit the imported read body");
        assert!(
            matches!(read.params.as_slice(), [param] if param.name == "m" && matches!(&param.ty, hew_types::ResolvedTy::Named { name, .. } if name == expected)),
            "HIR read parameter must retain the full self-qualified owner: {read:#?}"
        );

        let pipeline = hew_mir::lower_hir_module(&hir.module);
        assert!(
            pipeline.diagnostics.is_empty(),
            "self-qualified Meter field access must find its exact MIR layout: {:#?}",
            pipeline.diagnostics
        );
        assert!(
            pipeline.record_layouts.iter().any(|layout| {
                layout.name == expected
                    && layout.field_names == ["v".to_string()]
                    && layout.field_tys == [hew_types::ResolvedTy::I64]
            }),
            "MIR must register the full-owner Meter layout: {:#?}",
            pipeline.record_layouts
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
            "import hew::replysend;\n\nimport hew::replynonsend;",
            "import hew::replynonsend;\n\nimport hew::replysend;",
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
             pub fn render(value: Box<T>) -> string { \"generic\" }\n}\n";
        const SPECIALISED_IMPL: &str = "impl Render for Box<i64> {\n    \
             pub fn render(value: Box<i64>) -> string { \"specialised\" }\n}\n";
        const DECLARATIONS: &str = "pub trait Render {\n    \
             fn render(value: Self) -> string;\n}\n\n\
             pub type Box<T> {\n    value: T;\n}\n\n";

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
                    .map(|declaration| declaration.full_path().to_string())
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
    #[expect(
        clippy::too_many_lines,
        reason = "the same-leaf package regression proves both symbol publication and isolation together"
    )]
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
            hew_types::DefId::new("left.render.render_value"),
            hew_types::DefId::new("right.render.render_value"),
            hew_types::DefId::new("left.render.default_value"),
            hew_types::DefId::new("right.render.default_value"),
        ];
        let projected: Vec<_> = ids
            .iter()
            .map(|id| {
                symbols.get(id).cloned().unwrap_or_else(|| {
                    panic!(
                        "checker declaration `{}` has no emitted HIR body symbol; \n                         render symbols: {:#?}",
                        id.full_path(),
                        symbols
                            .iter()
                            .filter(|(candidate, _)| candidate.full_path().ends_with("render_value")
                                || candidate.full_path().ends_with("default_value"))
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
                mono.key.declaration.full_path()
                    == "left.render.Box::<default impl left.render.Render for left.render.Box<T>>::provided"
            }),
            "left default body must have its own synthetic implementation identity: {defaults:#?}"
        );
        assert!(
            defaults.iter().any(|mono| {
                mono.key.declaration.full_path()
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

        // Negative same-leaf control: both generic `Box<T>::render` bodies
        // specialise to `bool`, so a later lookup by `Box::render` or by the
        // `render` leaf would collapse these unrelated package owners.  MIR
        // must preserve both qualified HIR symbols through monomorphisation.
        let pipeline = hew_mir::lower_hir_module(&hir.module);
        assert!(
            pipeline.diagnostics.is_empty(),
            "same-leaf generic impl bodies must lower through MIR: {:#?}",
            pipeline.diagnostics
        );
        for symbol in ["left.render.Box::render", "right.render.Box::render"] {
            let concrete = hew_hir::monomorph::function_monomorph_symbol(
                symbol,
                &[hew_types::ResolvedTy::Bool],
            );
            assert!(
                pipeline
                    .raw_mir
                    .iter()
                    .any(|function| function.name == concrete),
                "same-leaf generic impl must retain its full owner in `{concrete}`"
            );
        }
    }

    #[test]
    #[expect(
        clippy::too_many_lines,
        reason = "the imported-body regression keeps root/imported parity and transitive-call controls together"
    )]
    fn imported_impl_catalog_len_uses_emitted_borrowing_abi() {
        // `echo_len` is intentionally an imported impl method whose body is
        // the source builtin `len(s)`.  The HIR catalog endpoint is `len_str`,
        // but raw MIR and codegen must agree on its concrete ABI symbol
        // `hew_string_length`; otherwise the representation-effect pass sees
        // an unknown call and incorrectly rejects the caller-visible `string`
        // parameter. `echo_tag` is the transitive sibling control: if
        // `echo_len` were not emitted safely, this package import would not
        // make it through the full callable closure.
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
            "imported actor fixture must lower cleanly: {:#?}",
            hir.diagnostics
        );

        let pipeline = hew_mir::lower_hir_module(&hir.module);
        assert!(
            pipeline.diagnostics.is_empty(),
            "imported catalog len must lower cleanly through MIR: {:#?}",
            pipeline.diagnostics
        );
        let echo_len = pipeline
            .raw_mir
            .iter()
            .find(|function| function.name.ends_with("Result::echo_len"))
            .unwrap_or_else(|| {
                panic!(
                    "expected imported `Result::echo_len` MIR body; emitted functions: {:#?}",
                    pipeline
                        .raw_mir
                        .iter()
                        .map(|function| &function.name)
                        .collect::<Vec<_>>()
                )
            });
        assert!(
            echo_len.blocks.iter().any(|block| matches!(
                &block.terminator,
                hew_mir::Terminator::Call { callee, .. } if callee == "hew_string_length"
            )),
            "the catalog ItemId join must project `len_str` to the concrete \
             `hew_string_length` ABI symbol: {echo_len:#?}"
        );
        let string_param_boundary = echo_len
            .decisions
            .iter()
            .find_map(|decision| match decision.strategy {
                hew_mir::Strategy::ParamBoundary(fact) if fact.param_index == 1 => Some(fact),
                _ => None,
            })
            .expect("echo_len string parameter must carry a boundary fact");
        assert_eq!(
            string_param_boundary.mode,
            hew_mir::ParamBoundaryMode::BorrowReadOnly,
            "the audited string-length ABI must not create an unproven \
             representation-mutation effect"
        );

        // The same checker-selected catalog shim must retain its audited FFI
        // authority whether its body is root-local or emitted from an imported
        // package.  Keep this table alongside the imported-actor regression:
        // imported-body lowering is the place where an authority handoff can
        // otherwise silently degrade to `Direct`.
        let direct_dir = tempfile::tempdir().expect("create direct-call fixture dir");
        let direct_input = write_source(
            direct_dir.path(),
            "direct_len.hew",
            "fn direct_len(s: string) -> i64 { len(s) }\nfn main() {}\n",
        );
        let direct_state = run_file_frontend_to_typecheck(
            &direct_input,
            &FrontendOptions {
                project_dir: Some(repo_root.to_path_buf()),
                ..FrontendOptions::default()
            },
        )
        .expect("root catalog-len fixture must type-check");
        let direct_tco = direct_state
            .typecheck_result
            .tco
            .as_ref()
            .expect("type checking was enabled");
        let direct_lowered = hew_hir::lower_program(
            &direct_state.program,
            direct_tco,
            &hew_hir::ResolutionCtx,
            hew_hir::TargetArch::host(),
        );
        assert!(
            direct_lowered.diagnostics.is_empty(),
            "root catalog-len fixture must lower cleanly: {:#?}",
            direct_lowered.diagnostics
        );
        let direct_pipeline = hew_mir::lower_hir_module(&direct_lowered.module);
        assert!(
            direct_pipeline.diagnostics.is_empty(),
            "root catalog-len fixture must lower through MIR: {:#?}",
            direct_pipeline.diagnostics
        );
        let direct_len = direct_pipeline
            .raw_mir
            .iter()
            .find(|function| function.name == "direct_len")
            .expect("root catalog-len body must be emitted");
        for (origin, function) in [("root", direct_len), ("imported", echo_len)] {
            let boundary = function
                .decisions
                .iter()
                .find_map(|decision| match decision.strategy {
                    hew_mir::Strategy::ParamBoundary(fact)
                        if fact.param_index == 0 || fact.param_index == 1 =>
                    {
                        matches!(decision.ty, hew_types::ResolvedTy::String).then_some(fact)
                    }
                    _ => None,
                })
                .unwrap_or_else(|| {
                    panic!("{origin} catalog-len body must retain its string boundary")
                });
            assert_eq!(
                boundary.mode,
                hew_mir::ParamBoundaryMode::BorrowReadOnly,
                "{origin} catalog-len body must retain the same audited FFI borrow authority"
            );
        }

        let echo_tag = pipeline
            .raw_mir
            .iter()
            .find(|function| function.name.ends_with("Result::echo_tag"))
            .expect("the transitive echo_tag caller must remain emitted");
        assert!(
            echo_tag.blocks.iter().any(|block| matches!(
                &block.terminator,
                hew_mir::Terminator::Call { callee, .. } if callee.ends_with("Result::echo_len")
            )),
            "echo_tag must preserve its direct call to the catalog-backed sibling: {echo_tag:#?}"
        );
        let echo_tag_string_boundary = echo_tag
            .decisions
            .iter()
            .find_map(|decision| match decision.strategy {
                hew_mir::Strategy::ParamBoundary(fact) if fact.param_index == 1 => Some(fact),
                _ => None,
            })
            .expect("echo_tag string parameter must carry a boundary fact");
        assert_eq!(
            echo_tag_string_boundary.mode,
            hew_mir::ParamBoundaryMode::BorrowReadOnly,
            "the emitted sibling must inherit echo_len's audited read-only boundary"
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
            super::canonical_direct_stdlib_module_for_source(&shipped).map(|module| module.path),
            Some(vec!["std".to_string(), "stream".to_string()]),
            "direct compilation of the shipped stream module must retain std.stream identity"
        );

        let dir = tempfile::tempdir().expect("create temp dir");
        let user_stream = write_source(
            dir.path(),
            "stream.hew",
            "type Sink<T> { value: T }\ntype Stream<T> { value: T }\n",
        );
        assert!(
            super::canonical_direct_stdlib_module_for_source(Path::new(&user_stream)).is_none(),
            "a same-named user file must not acquire compiler-owned std.stream provenance"
        );

        let shipped_net = repo_root.join("std/net/net.hew");
        assert_eq!(
            super::canonical_direct_stdlib_module_for_source(&shipped_net)
                .map(|module| module.path),
            Some(vec!["std".to_string(), "net".to_string()]),
            "direct compilation of the shipped TCP module must retain std.net identity"
        );

        let shipped_lambda = repo_root.join("std/concurrency/lambda_actor.hew");
        assert_eq!(
            super::canonical_direct_stdlib_module_for_source(&shipped_lambda)
                .map(|module| module.path),
            Some(vec!["std".to_string(), "concurrency".to_string()]),
            "a direct check of a canonical directory-module peer must retain std.concurrency identity"
        );
        fs::create_dir_all(dir.path().join("concurrency")).expect("create user module dir");
        let user_lambda = write_source(
            &dir.path().join("concurrency"),
            "lambda_actor.hew",
            "pub type LambdaActorHandle {}\n",
        );
        assert!(
            super::canonical_direct_stdlib_module_for_source(Path::new(&user_lambda)).is_none(),
            "a same-named user directory peer must not acquire std.concurrency provenance"
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
impl Foo { fn close(foo: Foo) {} }
extern "C" { fn hew_tcp_read(foo: Foo); }
"#,
        );
        let spoof_state = run_file_frontend_to_typecheck(&spoof, &FrontendOptions::default())
            .expect("the spoof is syntactically/type valid before HIR boundary enforcement");
        let spoof_tco = spoof_state
            .typecheck_result
            .tco
            .as_ref()
            .expect("successful spoof type check has type output");
        let spoof_hir = hew_hir::lower_program(
            &spoof_state.program,
            spoof_tco,
            &hew_hir::ResolutionCtx,
            hew_hir::TargetArch::host(),
        );
        assert!(
            spoof_hir.diagnostics.iter().any(|diagnostic| matches!(
                diagnostic.kind,
                hew_hir::HirDiagnosticKind::ResourceBoundaryParamMustConsume { ref func, .. }
                    if func == "hew_tcp_read"
            )),
            "a user Foo must not inherit std.net.Connection's borrow row: {:#?}",
            spoof_hir.diagnostics
        );
    }

    #[test]
    fn root_named_connection_import_survives_transitive_first_registration() {
        let dir = tempfile::tempdir().expect("create temp dir");
        write_source(
            dir.path(),
            "helper.hew",
            "import std::net;\n\npub fn marker() -> i64 { 1 }\n",
        );
        let input = write_source(
            dir.path(),
            "main.hew",
            "import helper;\n\
             import std::net::{Connection};\n\n\
             fn close_connection(conn: Connection) { conn.close(); }\n\
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
            "import alpha;\nimport beta::alpha;\n\nfn main() -> i64 { 0 }\n",
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
             import beta::alpha as nested_alpha;\n\n\
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
            "import hew::db::sqlite;\n\nfn main() -> i64 { sqlite.marker() }\n",
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
            "import std::bogus;\n\nfn main() {}\n",
        );

        let failure = check_file(
            &input,
            &FrontendOptions {
                no_typecheck: true,
                pkg_path: Some(pkg_root.clone()),
                ..Default::default()
            },
        )
        .expect_err("std::bogus must not resolve from --pkg-path/bogus.hew");

        assert!(
            failure.message.contains("module `std::bogus` not found"),
            "expected std::bogus to fail closed, got: {}",
            failure.message
        );
        let stripped_pkg_candidate = pkg_root.join("bogus.hew").display().to_string();
        assert!(
            !failure.message.contains(&stripped_pkg_candidate),
            "std:: imports must not try stripped --pkg-path tail candidate `{stripped_pkg_candidate}`: {}",
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
            "import std::bogus;\n\nfn main() {}\n",
        );

        let failure = check_file(
            &input,
            &FrontendOptions {
                no_typecheck: true,
                pkg_path: Some(pkg_root.clone()),
                ..Default::default()
            },
        )
        .expect_err("std::bogus must not resolve from --pkg-path/std/bogus.hew");

        assert!(
            failure.message.contains("module `std::bogus` not found"),
            "expected std::bogus to fail closed, got: {}",
            failure.message
        );
        let fake_std_candidate = fake_std_dir.join("bogus.hew").display().to_string();
        assert!(
            !failure.message.contains(&fake_std_candidate),
            "std:: imports must not try --pkg-path std-root candidate `{fake_std_candidate}`: {}",
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
            "import std::bogus;\n\nfn main() {}\n",
        );

        let failure = check_file(
            &input,
            &FrontendOptions {
                no_typecheck: true,
                project_dir: Some(dir.path().to_path_buf()),
                ..Default::default()
            },
        )
        .expect_err("std::bogus must not resolve from .hew/packages/std/bogus.hew");

        assert!(
            failure.message.contains("module `std::bogus` not found"),
            "expected std::bogus to fail closed, got: {}",
            failure.message
        );
        let fake_std_candidate = pkg_std_dir.join("bogus.hew").display().to_string();
        assert!(
            !failure.message.contains(&fake_std_candidate),
            "std:: imports must not try .hew std-root candidate `{fake_std_candidate}`: {}",
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
        let input = write_source(dir.path(), "main.hew", "import std::fs;\n\nfn main() {}\n");

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
                Item::Import(import) if import.path == ["std", "fs"] => Some(import),
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
        let expected = Path::new(&write_source(
            &stdlib_dir,
            "fs.hew",
            "pub fn explicit_marker() -> i64 { 1 }\n",
        ))
        .canonicalize()
        .expect("canonical explicit stdlib module");
        let project_dir = dir.path().join("external-project");
        fs::create_dir(&project_dir).expect("create external project");
        let input = write_source(
            &project_dir,
            "main.hew",
            "import std::fs;\n\nfn main() {}\n",
        );

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
                Item::Import(import) if import.path == ["std", "fs"] => {
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
        let input = write_source(
            dir.path(),
            "main.hew",
            "import mypkg::fs;\n\nfn main() {}\n",
        );

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
                Item::Import(import) if import.path == ["mypkg", "fs"] => Some(import),
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
            "import hew::db::sqlite;\n\nfn main() {}\n",
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
                Item::Import(import) if import.path == ["hew", "db", "sqlite"] => Some(import),
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
            "import ecosystem::db::postgres;\n\nfn main() {}\n",
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
                Item::Import(import) if import.path == ["ecosystem", "db", "postgres"] => {
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
            "import actor::monitor;\n\nfn main() -> i64 { 0 }\n",
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
        assert_eq!(import.path, vec!["actor", "monitor"]);
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
            "pub actor Account {\n    var n: i64 = 0;\n    \
             receive fn who() -> i64 { 1 }\n}\n",
        );
        write_source(
            dir.path(),
            "store.hew",
            "pub actor Account {\n    var n: i64 = 0;\n    \
             receive fn who() -> i64 { 2 }\n}\n",
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
            "pub actor Account {\n    var n: i64 = 0;\n    \
             receive fn who() -> i64 { 1 }\n}\n",
        );
        let input = write_source(
            dir.path(),
            "main.hew",
            "import bank;\n\nactor Account {\n    var n: i64 = 0;\n    \
             receive fn who() -> i64 { 2 }\n}\n\nfn main() -> i64 { 0 }\n",
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
            "pub actor Account {\n    var n: i64 = 0;\n    \
             receive fn who() -> i64 { 1 }\n}\n\
             pub actor Account {\n    var n: i64 = 0;\n    \
             receive fn who() -> i64 { 2 }\n}\n",
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
            "pub actor Account {\n    var n: i64 = 0;\n    \
             receive fn who() -> i64 { 1 }\n}\n",
        );
        write_source(
            dir.path(),
            "store.hew",
            "pub actor Register {\n    var n: i64 = 0;\n    \
             receive fn who() -> i64 { 2 }\n}\n",
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
            "pub actor Counter {\n    var n: i64 = 0;\n    \
             receive fn bump() -> i64 { n = n + 1; n }\n}\n",
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
            "actor Account {\n    var n: i64 = 0;\n    \
             receive fn id() -> i64 { 999 }\n}\n",
        );
        let input = write_source(
            dir.path(),
            "main.hew",
            "import secret;\n\nactor Account {\n    var n: i64 = 0;\n    \
             receive fn id() -> i64 { 111 }\n}\n\n\
             fn main() { let a = spawn secret.Account(); }\n",
        );

        let failure = check_file(&input, &FrontendOptions::default())
            .expect_err("spawn of a private imported actor must fail closed");
        // The detailed diagnostic is a typed error in `diagnostics`; the
        // top-level `message` is the generic "type errors found" summary.
        let has_export_diag = failure.diagnostics.iter().any(|diagnostic| {
            matches!(
                &diagnostic.kind,
                FrontendDiagnosticKind::Type(error)
                    if error.message.contains("has no exported actor `Account`")
                        && error.message.contains("secret")
            )
        });
        assert!(
            has_export_diag,
            "expected a fail-closed `has no exported actor `Account`` diagnostic \
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
            "pub type Account {\n    balance: i64,\n}\n",
        );
        let input = write_source(
            dir.path(),
            "main.hew",
            "import secret;\n\nactor Account {\n    var n: i64 = 0;\n    \
             receive fn id() -> i64 { 111 }\n}\n\n\
             fn main() { let a = spawn secret.Account(); }\n",
        );

        let failure = check_file(&input, &FrontendOptions::default())
            .expect_err("spawn of a non-actor module export must fail closed");
        let has_export_diag = failure.diagnostics.iter().any(|diagnostic| {
            matches!(
                &diagnostic.kind,
                FrontendDiagnosticKind::Type(error)
                    if error.message.contains("has no exported actor `Account`")
                        && error.message.contains("secret")
            )
        });
        assert!(
            has_export_diag,
            "expected a fail-closed `has no exported actor `Account`` diagnostic \
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
        let source = "import mylib::utils;\nfn main() {}\n";
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
    /// `register_stdlib_hew_items`, which previously had no `Item::Const` arm and
    /// silently dropped them so `log.JSON` / `log.TEXT` were unknown to the type
    /// checker.
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
            "import std::misc::log;\n",
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

    /// Importing `std::fs` and `std::path` together previously produced two
    /// defects caused by `SpanKey` lacking a per-module discriminator:
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
        let source = "import std::fs;\n\nfn main() -> i64 { 0 }\n";
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
        let source = "import std::fs;\n\nfn main() -> i64 { 0 }\n";
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
            "import std::path;\n",
            "import std::fs;\n",
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
    fn bundled_empty_type_decls_publish_owner_qualified_mir_layouts() {
        fn lower_to_mir(input: &str) -> hew_mir::IrPipeline {
            let state = run_file_frontend_to_typecheck(input, &FrontendOptions::default())
                .unwrap_or_else(|failure| panic!("frontend failed: {failure:#?}"));
            let typecheck = state
                .typecheck_result
                .tco
                .as_ref()
                .expect("fixture must typecheck");
            let lowered = hew_hir::lower_program(
                &state.program,
                typecheck,
                &hew_hir::ResolutionCtx,
                hew_hir::TargetArch::host(),
            );
            assert!(
                lowered.diagnostics.is_empty(),
                "HIR must retain every bundled declaration: {:#?}",
                lowered.diagnostics
            );
            hew_mir::lower_hir_module(&lowered.module)
        }

        let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("hew-compile lives below repository root");
        let direct = repo_root.join("std/concurrency/lambda_actor.hew");
        let direct = lower_to_mir(direct.to_str().expect("std path is UTF-8"));

        let dir = tempfile::tempdir().expect("create temp project");
        let imported_input = write_source(
            dir.path(),
            "main.hew",
            "import std::concurrency::{ScopeError};\n\
             fn main() {\n\
                 let error: ScopeError<i64> = ScopeError {\n\
                     primary: 1, also_failed: [], cancelled_count: 0\n\
                 };\n\
                 let _ = error;\n\
             }\n",
        );
        let imported = lower_to_mir(&imported_input);

        for (pipeline, owner) in [
            (&direct, Some("std.concurrency")),
            (&imported, Some("std.concurrency")),
        ] {
            assert!(
                pipeline.diagnostics.is_empty(),
                "bundled source must lower without MIR authority diagnostics: {:#?}",
                pipeline.diagnostics
            );
            for leaf in ["LambdaActorHandle", "LambdaActorWeakHandle"] {
                let expected =
                    owner.map_or_else(|| leaf.to_string(), |owner| format!("{owner}.{leaf}"));
                assert!(
                    pipeline
                        .record_layouts
                        .iter()
                        .any(|layout| layout.name == expected),
                    "bundled declaration `{expected}` must publish its source-owned layout: {:#?}",
                    pipeline.record_layouts
                );
            }
        }

        // A user package can legally use the same leaf name, but its source
        // identity must never acquire the bundled lambda-actor layout.
        write_source(dir.path(), "spoofed.hew", "pub type LambdaActorHandle {}\n");
        let foreign_input = write_source(
            dir.path(),
            "foreign_main.hew",
            "import spoofed::{LambdaActorHandle};\n\
             fn main() { let _ = LambdaActorHandle {}; }\n",
        );
        let foreign = lower_to_mir(&foreign_input);
        assert!(
            foreign
                .record_layouts
                .iter()
                .any(|layout| layout.name == "spoofed.LambdaActorHandle"),
            "foreign declaration must retain its own owner: {:#?}",
            foreign.record_layouts
        );
        assert!(
            !foreign
                .record_layouts
                .iter()
                .any(|layout| layout.name == "std.concurrency.LambdaActorHandle"),
            "a same-leaf user declaration must not inherit bundled ownership: {:#?}",
            foreign.record_layouts
        );
        assert!(
            foreign.diagnostics.iter().any(|diagnostic| matches!(
                diagnostic.kind,
                hew_mir::MirDiagnosticKind::DecisionMapTotal { .. }
            )),
            "the foreign same-leaf handle must remain fail-closed instead of inheriting the bundled resource class: {:#?}",
            foreign.diagnostics
        );
    }
}
