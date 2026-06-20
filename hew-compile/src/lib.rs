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
    pub kind: FrontendDiagnosticKind,
}

impl FrontendDiagnostic {
    fn message(message: impl Into<String>) -> Self {
        Self {
            source: None,
            filename: None,
            kind: FrontendDiagnosticKind::Message(message.into()),
        }
    }

    fn parse(source: &str, filename: &str, diagnostic: hew_parser::ParseError) -> Self {
        Self {
            source: Some(source.to_string()),
            filename: Some(filename.to_string()),
            kind: FrontendDiagnosticKind::Parse(diagnostic),
        }
    }

    fn type_(source: &str, filename: &str, diagnostic: hew_types::TypeError) -> Self {
        Self {
            source: Some(source.to_string()),
            filename: Some(filename.to_string()),
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
    /// Alias-vs-copy decision per actor send site, keyed by source span.
    ///
    /// Populated when type-checking succeeds and the type-check output is
    /// available. Empty when `FrontendOptions::no_typecheck` is set or when
    /// type errors prevent the checker from completing.
    ///
    /// Cloned directly from `TypeCheckOutput::actor_send_aliasing`; consumed
    /// by `--explain-cow` rendering in `hew check`.
    pub actor_send_aliasing:
        HashMap<hew_types::check::SpanKey, hew_types::check::ActorSendAliasing>,
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
                "Error: module `{module_str}` is not declared in hew.toml\n  hint: add it with `adze add {module_str}`"
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
    FrontendDiagnostic::type_(source, filename, diagnostic)
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
    let search_paths =
        hew_types::module_registry::build_module_search_paths_for(options.project_dir.as_deref());
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
    let tco = checker.check_program(program);
    let module_source_map = build_module_source_map(program);
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
            let actor_send_aliasing = tcr
                .tco
                .as_ref()
                .map(|tco| tco.actor_send_aliasing.clone())
                .unwrap_or_default();
            Ok(CheckOutput {
                diagnostics,
                stack_hints,
                actor_send_aliasing,
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
                    spec: None,
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

fn canonical_floor_module_for_source(source_file: &Path) -> Option<hew_parser::module::ModuleId> {
    let input_canonical = std::fs::canonicalize(source_file).ok()?;
    let search_roots = hew_types::module_registry::build_module_search_paths_for(Some(source_file));

    for dotted in hew_types::check::intrinsic_floor_modules() {
        let segments = dotted.split('.').collect::<Vec<_>>();
        let Some(last) = segments.last() else {
            continue;
        };
        let rel = segments.iter().collect::<PathBuf>();
        let candidates = [rel.join(format!("{last}.hew")), rel.with_extension("hew")];

        for root in &search_roots {
            for candidate in &candidates {
                let Ok(candidate_canonical) = std::fs::canonicalize(root.join(candidate)) else {
                    continue;
                };
                if candidate_canonical == input_canonical {
                    return Some(hew_parser::module::ModuleId::new(
                        segments
                            .iter()
                            .map(|segment| (*segment).to_string())
                            .collect(),
                    ));
                }
            }
        }
    }

    None
}

fn rewrite_direct_floor_module_root(
    module_graph: &mut hew_parser::module::ModuleGraph,
    items: &mut Vec<Spanned<Item>>,
    source_file: &Path,
) -> Result<(), FrontendFailure> {
    use hew_parser::module::{Module, ModuleId};

    let Some(floor_id) = canonical_floor_module_for_source(source_file) else {
        return Ok(());
    };

    let original_root = module_graph.root.clone();
    let Some(mut floor_module) = module_graph.modules.remove(&original_root) else {
        return Ok(());
    };

    floor_module.id = floor_id.clone();
    module_graph.root = ModuleId::root();
    module_graph.modules.insert(floor_id, floor_module);
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
    rewrite_direct_floor_module_root(&mut graph, items, &input_canonical)?;

    // Reject programs where two different module imports share the same short
    // name (the last path segment).  HIR keys all cross-module fn registrations
    // by `module_short.fn_name`; two modules with the same short name would
    // silently collide in the fn-registry.  Fail closed here so HIR never sees
    // the ambiguity.  The richer "did you mean foo::util or bar::util?"
    // suggestion is deferred to a v0.6 diagnostic-quality lane.
    if let Err(msg) = check_duplicate_short_module_names(&graph) {
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

fn check_duplicate_short_module_names(
    graph: &hew_parser::module::ModuleGraph,
) -> Result<(), String> {
    let mut seen: HashMap<&str, &hew_parser::module::ModuleId> = HashMap::new();
    for mod_id in &graph.topo_order {
        if *mod_id == graph.root {
            continue;
        }
        let short = mod_id.path.last().map_or("", String::as_str);
        if let Some(existing) = seen.insert(short, mod_id) {
            return Err(format!(
                "Error: two imported modules share the short name `{short}`: \
                 `{existing}` and `{mod_id}`. \
                 Rename one of the imports or use file-path import syntax."
            ));
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
            (ModuleId::new(decl.path.clone()), None)
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

                candidates.extend([
                    source_dir.join(&dir_path),
                    cwd.join(&dir_path),
                    source_dir.join(&rel_path),
                    cwd.join(&rel_path),
                ]);

                if let Some(version) = ctx
                    .locked_versions
                    .and_then(|locked| locked.iter().find(|(name, _)| name == &module_str))
                    .map(|(_, version)| version.as_str())
                {
                    let module_dir = decl.path.iter().collect::<PathBuf>();
                    let entry_file =
                        format!("{}.hew", decl.path.last().expect("path is non-empty"));
                    let versioned_rel = module_dir.join(version).join(entry_file);
                    candidates.push(ctx.project_dir.join(".adze/packages").join(&versioned_rel));
                    if let Some(pkg) = ctx.extra_pkg_path {
                        candidates.push(pkg.join(&versioned_rel));
                    }
                }

                candidates.push(ctx.project_dir.join(".adze/packages").join(&rel_path));
                candidates.push(ctx.project_dir.join(".adze/packages").join(&dir_path));

                if let Some(pkg) = ctx.extra_pkg_path {
                    candidates.push(pkg.join(&dir_path));
                    candidates.push(pkg.join(&rel_path));
                    if decl.path.len() > 1 {
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

                // Stdlib / global search roots — apply exclusive precedence tiers so that
                // a file in worktree-A always resolves std from A only, never from the
                // build binary's worktree or a sibling checkout.
                for root in
                    hew_types::module_registry::build_module_search_paths_for(Some(source_file))
                {
                    candidates.push(root.join(&dir_path));
                    candidates.push(root.join(&rel_path));
                }

                // Collect ALL candidates that resolve, then deduplicate by canonical path.
                // If two or more distinct canonical paths resolve, the import is ambiguous —
                // fail-closed rather than silently picking the first match.
                let mut resolved: Vec<std::path::PathBuf> = candidates
                    .iter()
                    .filter_map(|candidate| candidate.canonicalize().ok())
                    .collect();
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
                        "\n  hint: this dependency is declared in hew.toml — run `adze install`"
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
    let actor_send_aliasing = state
        .typecheck_result
        .tco
        .as_ref()
        .map(|tco| tco.actor_send_aliasing.clone())
        .unwrap_or_default();
    let output = CheckOutput {
        diagnostics,
        stack_hints,
        actor_send_aliasing,
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
    #[serde(default = "default_edition")]
    edition: String,
}

/// Table form of a `hew.toml` dependency: `{ version = "^1.0", path = "...",
/// features = [...], optional = true }`. The field set mirrors adze's `DepTable`
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
/// table. Untagged to match adze's `DepSpec` so the compiler no longer rejects
/// table/path/feature dependencies that `adze install` accepts.
#[derive(Debug, Deserialize)]
#[serde(untagged)]
#[allow(
    dead_code,
    reason = "manifest compatibility variants preserve adze-compatible dependency syntax"
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
struct AdzeTomlLock {
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
            )))
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
    let path = dir.join("adze.lock");
    let Some(lock) = load_optional_toml::<AdzeTomlLock>(&path)? else {
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
        let mut file = File::create(dir.join("adze.lock")).expect("create adze.lock");
        file.write_all(content.as_bytes()).expect("write adze.lock");
    }

    fn write_source(dir: &Path, name: &str, content: &str) -> String {
        let path = dir.join(name);
        let mut file = File::create(&path).expect("create source file");
        file.write_all(content.as_bytes())
            .expect("write source file");
        path.display().to_string()
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
        // Table / path / feature dependency forms are accepted by adze; the
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
        assert!(err.message.contains("adze.lock"), "{}", err.message);
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
        assert!(err.message.contains("adze.lock"), "{}", err.message);
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

    /// Two imported modules resolving to the same short name (last path
    /// segment) must be REJECTED. HIR keys every cross-module fn registration
    /// by `module_short.fn_name`; admitting `alpha` and `beta::alpha` together
    /// would let two distinct `alpha::val` functions silently collide in the
    /// fn-registry and miscompile. The guard lives in
    /// `check_duplicate_short_module_names` (called from
    /// `build_module_graph_with_diagnostics`); this test is the regression
    /// pin for it (the run.sh fixture exercises the CLI, not the crate).
    #[test]
    fn check_file_rejects_duplicate_short_module_names() {
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
            .expect_err("colliding short module names must fail closed");
        assert!(
            failure.message.contains("short name"),
            "expected a duplicate-short-name diagnostic, got: {}",
            failure.message
        );
        assert!(
            failure.message.contains("alpha"),
            "diagnostic should name the colliding short name `alpha`, got: {}",
            failure.message
        );
    }

    /// Negative control for the duplicate-short-name guard: two imported
    /// modules with DISTINCT short names compile cleanly — the guard must not
    /// over-reject. Same layout as the rejection case but the second module's
    /// short name is `gamma`, not `alpha`.
    #[test]
    fn check_file_accepts_distinct_short_module_names() {
        let dir = tempfile::tempdir().expect("create temp dir");
        write_source(dir.path(), "alpha.hew", "pub fn val() -> i64 { 1 }\n");
        fs::create_dir_all(dir.path().join("beta")).expect("create beta dir");
        write_source(dir.path(), "beta/gamma.hew", "pub fn val() -> i64 { 2 }\n");
        let input = write_source(
            dir.path(),
            "main.hew",
            "import alpha;\nimport beta::gamma;\n\nfn main() -> i64 { 0 }\n",
        );

        // A successful `check_file` is the precise complement of the rejection:
        // the duplicate-short-name guard fails the whole pipeline with `Err`,
        // so reaching `Ok` proves the guard did not fire on distinct names.
        check_file(&input, &FrontendOptions::default())
            .expect("distinct short module names must be accepted");
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
        assert!(err.message.contains("adze.lock"), "{}", err.message);
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
}
