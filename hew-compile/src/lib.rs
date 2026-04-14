use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt;
use std::path::{Path, PathBuf};

use hew_parser::ast::{ImportDecl, Item, Program, Spanned};
use hew_serialize::{
    build_assign_target_kind_entries, build_assign_target_shape_entries,
    build_lowering_fact_entries, build_method_call_receiver_kind_entries, serialize_to_json,
    serialize_to_msgpack, AssignTargetKindEntry, AssignTargetShapeEntry, ExprTypeEntry,
    LoweringFactEntry, MethodCallReceiverKindEntry, TypeExprConversionError,
    TypeExprConversionKind,
};
use serde::{de::DeserializeOwned, Deserialize};

#[derive(Debug, Clone, Default)]
pub struct FrontendOptions {
    pub no_typecheck: bool,
    pub warnings_as_errors: bool,
    pub enable_wasm_target: bool,
    pub pkg_path: Option<PathBuf>,
    /// Anchor the in-memory compile to a specific project directory, enabling
    /// manifest-aware import resolution (local `src/` lookup, manifest dep
    /// validation, lockfile) identical to `compile_file`.  When `None` the
    /// old cwd-fallback with no manifest is used.
    pub project_dir: Option<PathBuf>,
}

#[derive(Debug, Clone)]
pub enum FrontendDiagnosticKind {
    Message(String),
    Parse(hew_parser::ParseError),
    Type(hew_types::TypeError),
    InferredType {
        error: TypeExprConversionError,
        fatal: bool,
    },
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

    fn inferred(
        source: Option<String>,
        filename: Option<String>,
        error: TypeExprConversionError,
        fatal: bool,
    ) -> Self {
        Self {
            source,
            filename,
            kind: FrontendDiagnosticKind::InferredType { error, fatal },
        }
    }

    fn is_warning(&self) -> bool {
        match &self.kind {
            FrontendDiagnosticKind::Message(_) => false,
            FrontendDiagnosticKind::Parse(diagnostic) => {
                diagnostic.severity == hew_parser::Severity::Warning
            }
            FrontendDiagnosticKind::Type(diagnostic) => {
                diagnostic.severity == hew_types::error::Severity::Warning
            }
            FrontendDiagnosticKind::InferredType { fatal, .. } => !fatal,
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

fn fail_on_warning_diagnostics(
    diagnostics: Vec<FrontendDiagnostic>,
    options: &FrontendOptions,
) -> Result<Vec<FrontendDiagnostic>, FrontendFailure> {
    if options.warnings_as_errors && diagnostics.iter().any(FrontendDiagnostic::is_warning) {
        Err(FrontendFailure::new(
            "warnings treated as errors",
            diagnostics,
        ))
    } else {
        Ok(diagnostics)
    }
}

#[derive(Debug, Clone, Default)]
pub struct CheckOutput {
    pub diagnostics: Vec<FrontendDiagnostic>,
}

#[derive(Debug)]
pub struct FrontendArtifacts {
    pub diagnostics: Vec<FrontendDiagnostic>,
    pub source: String,
    pub source_label: String,
    pub program: Program,
    pub expr_type_entries: Vec<ExprTypeEntry>,
    pub method_call_receiver_kinds: Vec<MethodCallReceiverKindEntry>,
    pub assign_target_kinds: Vec<AssignTargetKindEntry>,
    pub assign_target_shapes: Vec<AssignTargetShapeEntry>,
    pub lowering_facts: Vec<LoweringFactEntry>,
    pub handle_types: Vec<String>,
    pub handle_type_repr: HashMap<String, String>,
    pub drop_funcs: Vec<(String, String)>,
    pub abs_source_path: Option<String>,
    pub line_map: Option<Vec<usize>>,
}

impl FrontendArtifacts {
    #[must_use]
    pub fn to_msgpack(&self) -> Vec<u8> {
        serialize_to_msgpack(
            &self.program,
            self.expr_type_entries.clone(),
            self.method_call_receiver_kinds.clone(),
            self.assign_target_kinds.clone(),
            self.assign_target_shapes.clone(),
            self.lowering_facts.clone(),
            self.handle_types.clone(),
            self.handle_type_repr.clone(),
            self.drop_funcs.clone(),
            self.abs_source_path.as_deref(),
            self.line_map.as_deref(),
        )
    }

    #[must_use]
    pub fn to_json(&self) -> String {
        serialize_to_json(
            &self.program,
            self.expr_type_entries.clone(),
            self.method_call_receiver_kinds.clone(),
            self.assign_target_kinds.clone(),
            self.assign_target_shapes.clone(),
            self.lowering_facts.clone(),
            self.handle_types.clone(),
            self.handle_type_repr.clone(),
            self.drop_funcs.clone(),
            self.abs_source_path.as_deref(),
            self.line_map.as_deref(),
        )
    }
}

#[derive(Debug)]
pub struct FrontendSerializedOutput<T> {
    pub diagnostics: Vec<FrontendDiagnostic>,
    pub data: T,
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

struct CodegenMetadata {
    handle_types: Vec<String>,
    handle_type_repr: HashMap<String, String>,
    drop_funcs: Vec<(String, String)>,
    abs_source_path: Option<String>,
    line_map: Option<Vec<usize>>,
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

fn load_project_context(input: &str) -> Result<ProjectContext, FrontendFailure> {
    let source = std::fs::read_to_string(input)
        .map_err(|e| FrontendFailure::message_only(format!("Error: cannot read {input}: {e}")))?;
    let project_dir = Path::new(input)
        .parent()
        .unwrap_or(Path::new("."))
        .to_path_buf();
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

fn typecheck_program_with_diagnostics(
    program: &Program,
    source: &str,
    input: &str,
    options: &FrontendOptions,
) -> Result<(TypeCheckResult, Vec<FrontendDiagnostic>), FrontendFailure> {
    let search_paths = hew_types::module_registry::build_module_search_paths();
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
        Ok((_, type_diagnostics)) => {
            diagnostics.extend(type_diagnostics);
            Ok(CheckOutput { diagnostics })
        }
        Err(failure) => Err(merge_prior_diagnostics(diagnostics, failure)),
    }
}

fn inferred_type_serialization_diagnostic_is_fatal(error: &TypeExprConversionError) -> bool {
    matches!(
        error.kind(),
        TypeExprConversionKind::UnresolvedVar
            | TypeExprConversionKind::ErrorSentinel
            | TypeExprConversionKind::LiteralKind
            | TypeExprConversionKind::MethodCallRewriteFailed
            | TypeExprConversionKind::Unsupported
    )
}

enum DiagnosticSourceHint {
    Root,
    Imported(PathBuf),
    UnknownImported,
}

fn diagnostic_source_hint(
    span: &hew_parser::ast::Span,
    imported_item_sources: &[(hew_parser::ast::Span, Option<PathBuf>)],
) -> DiagnosticSourceHint {
    match imported_item_sources
        .iter()
        .filter(|(item_span, _)| item_span.start <= span.start && item_span.end >= span.end)
        .min_by_key(|(item_span, _)| item_span.end.saturating_sub(item_span.start))
    {
        Some((_, Some(path))) => DiagnosticSourceHint::Imported(path.clone()),
        Some((_, None)) => DiagnosticSourceHint::UnknownImported,
        None => DiagnosticSourceHint::Root,
    }
}

fn diagnostic_span_key(
    error: &TypeExprConversionError,
    root_filename: &str,
    imported_item_sources: &[(hew_parser::ast::Span, Option<PathBuf>)],
) -> Option<(String, usize, usize)> {
    let span = error.span()?;
    match diagnostic_source_hint(span, imported_item_sources) {
        DiagnosticSourceHint::Imported(path) => {
            Some((path.display().to_string(), span.start, span.end))
        }
        DiagnosticSourceHint::UnknownImported => None,
        DiagnosticSourceHint::Root => Some((root_filename.to_string(), span.start, span.end)),
    }
}

#[expect(
    clippy::implicit_hasher,
    reason = "the CLI/frontend dedup set uses the default hasher today"
)]
pub fn collect_new_inferred_type_diagnostics<'a>(
    diagnostics: &'a [TypeExprConversionError],
    root_filename: &str,
    imported_item_sources: &[(hew_parser::ast::Span, Option<PathBuf>)],
    seen_spans: &mut HashSet<(String, usize, usize)>,
) -> Vec<&'a TypeExprConversionError> {
    diagnostics
        .iter()
        .filter(|diagnostic| {
            match diagnostic_span_key(diagnostic, root_filename, imported_item_sources) {
                Some(key) => seen_spans.insert(key),
                None => true,
            }
        })
        .collect()
}

fn inferred_type_diagnostic(
    source: &str,
    input: &str,
    imported_item_sources: &[(hew_parser::ast::Span, Option<PathBuf>)],
    error: &TypeExprConversionError,
    fatal: bool,
) -> FrontendDiagnostic {
    if let Some(span) = error.span() {
        match diagnostic_source_hint(span, imported_item_sources) {
            DiagnosticSourceHint::Imported(path) => FrontendDiagnostic::inferred(
                std::fs::read_to_string(&path).ok(),
                Some(path.display().to_string()),
                error.clone(),
                fatal,
            ),
            DiagnosticSourceHint::UnknownImported => {
                FrontendDiagnostic::inferred(None, None, error.clone(), fatal)
            }
            DiagnosticSourceHint::Root => FrontendDiagnostic::inferred(
                Some(source.to_string()),
                Some(input.to_string()),
                error.clone(),
                fatal,
            ),
        }
    } else {
        FrontendDiagnostic::inferred(None, None, error.clone(), fatal)
    }
}

#[expect(
    clippy::type_complexity,
    reason = "threads serializer side tables together with collected diagnostics"
)]
fn enrich_program_ast_with_diagnostics(
    program: &mut Program,
    tco: Option<&hew_types::check::TypeCheckOutput>,
    module_registry: &hew_types::module_registry::ModuleRegistry,
    source: &str,
    input: &str,
) -> Result<
    (
        (Vec<ExprTypeEntry>, Vec<MethodCallReceiverKindEntry>),
        Vec<FrontendDiagnostic>,
    ),
    FrontendFailure,
> {
    let root_module_item_count = program.module_graph.as_ref().and_then(|graph| {
        graph
            .modules
            .get(&graph.root)
            .map(|module| module.items.len())
    });
    let imported_item_sources = flatten_import_items(program);
    let mut diagnostics = Vec::new();

    let (expr_type_map, method_call_receiver_kinds) = if let Some(tco) = tco {
        let mut seen_inferred_type_diagnostics = HashSet::new();
        let mut fatal_inferred_type_diagnostic = false;
        let enrich_diagnostics = hew_serialize::enrich_program(program, tco, module_registry)
            .map_err(|e| {
                FrontendFailure::message_only(format!("Error: cannot enrich inferred types: {e}"))
            })?;

        for diagnostic in collect_new_inferred_type_diagnostics(
            enrich_diagnostics.diagnostics(),
            input,
            &imported_item_sources,
            &mut seen_inferred_type_diagnostics,
        ) {
            let fatal = inferred_type_serialization_diagnostic_is_fatal(diagnostic);
            fatal_inferred_type_diagnostic |= fatal;
            diagnostics.push(inferred_type_diagnostic(
                source,
                input,
                &imported_item_sources,
                diagnostic,
                fatal,
            ));
        }

        if let Some(ref mut module_graph) = program.module_graph {
            if let Some(root_module) = module_graph.modules.get_mut(&module_graph.root) {
                if let Some(root_len) = root_module_item_count {
                    root_module.items = program.items.iter().take(root_len).cloned().collect();
                } else {
                    root_module.items.clone_from(&program.items);
                }
            }
            for (id, module) in &mut module_graph.modules {
                if *id != module_graph.root {
                    hew_serialize::normalize_items_types(&mut module.items, module_registry);
                }
            }
        }

        let expr_type_map_build = hew_serialize::build_expr_type_map(tco);
        let method_call_receiver_kinds = build_method_call_receiver_kind_entries(program, tco);

        for diagnostic in collect_new_inferred_type_diagnostics(
            expr_type_map_build.diagnostics(),
            input,
            &imported_item_sources,
            &mut seen_inferred_type_diagnostics,
        ) {
            let fatal = inferred_type_serialization_diagnostic_is_fatal(diagnostic);
            fatal_inferred_type_diagnostic |= fatal;
            diagnostics.push(inferred_type_diagnostic(
                source,
                input,
                &imported_item_sources,
                diagnostic,
                fatal,
            ));
        }

        if fatal_inferred_type_diagnostic {
            return Err(FrontendFailure::new(
                "inferred type serialization failed",
                diagnostics,
            ));
        }

        (expr_type_map_build.entries, method_call_receiver_kinds)
    } else {
        (Vec::new(), Vec::new())
    };

    hew_parser::tail_call::mark_tail_calls(program);
    Ok(((expr_type_map, method_call_receiver_kinds), diagnostics))
}

/// Enrich a program with inferred types and serializer side tables.
///
/// # Errors
///
/// Returns [`FrontendFailure`] when enrichment fails or inferred-type
/// serialization must fail closed.
pub fn enrich_program_ast(
    program: &mut Program,
    tco: Option<&hew_types::check::TypeCheckOutput>,
    module_registry: &hew_types::module_registry::ModuleRegistry,
    source: &str,
    input: &str,
) -> Result<(Vec<ExprTypeEntry>, Vec<MethodCallReceiverKindEntry>), FrontendFailure> {
    enrich_program_ast_with_diagnostics(program, tco, module_registry, source, input)
        .map(|(result, _)| result)
}

fn build_codegen_metadata(
    module_registry: &hew_types::module_registry::ModuleRegistry,
    input: &str,
    source: &str,
) -> CodegenMetadata {
    let handle_types = module_registry.all_handle_types();
    let handle_type_repr = handle_types
        .iter()
        .filter(|ty| hew_types::stdlib::handle_type_representation(ty) != "handle")
        .map(|ty| {
            (
                ty.clone(),
                hew_types::stdlib::handle_type_representation(ty).to_string(),
            )
        })
        .collect();
    let abs_source_path = Some(
        std::fs::canonicalize(input)
            .map_or_else(|_| input.to_string(), |path| path.display().to_string()),
    );
    let line_map = Some(line_map_from_source(source));
    let drop_funcs = module_registry.all_drop_funcs();
    CodegenMetadata {
        handle_types,
        handle_type_repr,
        drop_funcs,
        abs_source_path,
        line_map,
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

#[expect(
    clippy::ptr_arg,
    reason = "items are cloned into module graph, needs Vec"
)]
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
        source_paths: vec![input_canonical],
        doc: module_doc,
    };
    graph.add_module(root_module);

    if let Err(cycle_err) = graph.compute_topo_order() {
        return Err(FrontendFailure::message_only(cycle_err.to_string()));
    }

    Ok(graph)
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
                graph.add_module(module);
            }
        }
    }

    imports
}

fn flatten_import_items(program: &mut Program) -> Vec<(hew_parser::ast::Span, Option<PathBuf>)> {
    let mut extra = Vec::new();
    let mut imported_item_sources = Vec::new();
    for (item, _) in &mut program.items {
        if let Item::Import(decl) = item {
            if let Some(resolved) = decl.resolved_items.take() {
                let mut item_source_paths =
                    std::mem::take(&mut decl.resolved_item_source_paths).into_iter();
                for resolved_item in resolved {
                    let source_path = item_source_paths.next();
                    if !matches!(&resolved_item.0, Item::Import(_)) {
                        imported_item_sources.push((resolved_item.1.clone(), source_path));
                        extra.push(resolved_item);
                    }
                }
            }
        }
    }
    program.items.extend(extra);
    imported_item_sources
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
                let exe_dir = std::env::current_exe()
                    .ok()
                    .and_then(|path| path.parent().map(Path::to_path_buf));
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
                    candidates.push(cwd.join(".adze/packages").join(&versioned_rel));
                    if let Some(pkg) = ctx.extra_pkg_path {
                        candidates.push(pkg.join(&versioned_rel));
                    }
                }

                candidates.push(cwd.join(".adze/packages").join(&rel_path));
                candidates.push(cwd.join(".adze/packages").join(&dir_path));

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

                if let Ok(hew_std) = std::env::var("HEW_STD") {
                    let std_root = PathBuf::from(hew_std);
                    if let Some(parent) = std_root.parent() {
                        candidates.push(parent.join(&dir_path));
                        candidates.push(parent.join(&rel_path));
                    }
                }

                if let Some(ref exe) = exe_dir {
                    let fhs_root = exe.join("../share/hew");
                    candidates.push(fhs_root.join(&dir_path));
                    candidates.push(fhs_root.join(&rel_path));
                }
                if let Some(ref exe) = exe_dir {
                    if let Some(project_root) = exe.parent().and_then(|path| path.parent()) {
                        candidates.push(project_root.join(&dir_path));
                        candidates.push(project_root.join(&rel_path));
                    }
                }

                if let Some(canonical) = candidates
                    .iter()
                    .find_map(|candidate| candidate.canonicalize().ok())
                {
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
/// Consumed by either `check_file` (phase stop here) or `compile_file`
/// (continues into enrichment and codegen-metadata assembly).
struct FileFrontendState {
    program: Program,
    diagnostics: Vec<FrontendDiagnostic>,
    typecheck_result: TypeCheckResult,
    source: String,
}

/// Shared frontend driver for on-disk source files.
///
/// Runs load → parse → import-resolution → type-check and returns the
/// intermediate [`FileFrontendState`].  Both `check_file` and `compile_file`
/// call this helper; `check_file` stops here while `compile_file` continues
/// into enrichment and codegen-metadata assembly via `finish_compile`.
fn run_file_frontend_to_typecheck(
    input: &str,
    options: &FrontendOptions,
) -> Result<FileFrontendState, FrontendFailure> {
    let project = load_project_context(input)?;
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

    Ok(FileFrontendState {
        program,
        diagnostics,
        typecheck_result,
        source: project.source,
    })
}

/// Shared enrichment and codegen-metadata assembly stage.
///
/// Takes the already type-checked program state and runs AST enrichment,
/// side-table construction, and metadata assembly, returning the final
/// [`FrontendArtifacts`].  Used by both `compile_file` and `compile_program`.
fn finish_compile(
    mut program: Program,
    mut diagnostics: Vec<FrontendDiagnostic>,
    typecheck_result: &TypeCheckResult,
    source: String,
    source_label: String,
) -> Result<FrontendArtifacts, FrontendFailure> {
    let ((expr_type_entries, method_call_receiver_kinds), enrich_diagnostics) =
        match enrich_program_ast_with_diagnostics(
            &mut program,
            typecheck_result.tco.as_ref(),
            &typecheck_result.module_registry,
            &source,
            &source_label,
        ) {
            Ok(result) => result,
            Err(failure) => return Err(merge_prior_diagnostics(diagnostics, failure)),
        };
    diagnostics.extend(enrich_diagnostics);

    let assign_target_kinds = typecheck_result.tco.as_ref().map_or_else(Vec::new, |tco| {
        build_assign_target_kind_entries(&program, tco)
    });
    let assign_target_shapes = typecheck_result.tco.as_ref().map_or_else(Vec::new, |tco| {
        build_assign_target_shape_entries(&program, tco)
    });
    let lowering_facts = typecheck_result
        .tco
        .as_ref()
        .map_or_else(Vec::new, |tco| build_lowering_fact_entries(&program, tco));
    let metadata =
        build_codegen_metadata(&typecheck_result.module_registry, &source_label, &source);

    Ok(FrontendArtifacts {
        diagnostics,
        source,
        source_label,
        program,
        expr_type_entries,
        method_call_receiver_kinds,
        assign_target_kinds,
        assign_target_shapes,
        lowering_facts,
        handle_types: metadata.handle_types,
        handle_type_repr: metadata.handle_type_repr,
        drop_funcs: metadata.drop_funcs,
        abs_source_path: metadata.abs_source_path,
        line_map: metadata.line_map,
    })
}

/// Parse, resolve imports, and type-check a Hew source file.
///
/// # Errors
///
/// Returns [`FrontendFailure`] when parsing, import resolution, or type
/// checking fails.
pub fn check_file(input: &str, options: &FrontendOptions) -> Result<CheckOutput, FrontendFailure> {
    let state = run_file_frontend_to_typecheck(input, options)?;
    let diagnostics = fail_on_warning_diagnostics(state.diagnostics, options)?;
    Ok(CheckOutput { diagnostics })
}

/// Run the full frontend pipeline for an on-disk source file.
///
/// # Errors
///
/// Returns [`FrontendFailure`] when any frontend stage fails.
pub fn compile_file(
    input: &str,
    options: &FrontendOptions,
) -> Result<FrontendArtifacts, FrontendFailure> {
    let FileFrontendState {
        program,
        diagnostics,
        typecheck_result,
        source,
    } = run_file_frontend_to_typecheck(input, options)?;
    let frontend = finish_compile(
        program,
        diagnostics,
        &typecheck_result,
        source,
        input.to_string(),
    )?;
    fail_on_warning_diagnostics(frontend.diagnostics.clone(), options)?;
    Ok(frontend)
}

/// Run the full frontend pipeline for an already-parsed in-memory program.
///
/// # Errors
///
/// Returns [`FrontendFailure`] when import resolution, type checking, or
/// enrichment fails.
pub fn compile_program(
    mut program: Program,
    source: &str,
    source_label: &str,
    options: &FrontendOptions,
) -> Result<FrontendArtifacts, FrontendFailure> {
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

    let frontend = finish_compile(
        program,
        diagnostics,
        &typecheck_result,
        source.to_string(),
        source_label.to_string(),
    )?;
    fail_on_warning_diagnostics(frontend.diagnostics.clone(), options)?;
    Ok(frontend)
}

/// Run the full frontend pipeline for a source file and serialize to msgpack.
///
/// # Errors
///
/// Returns [`FrontendFailure`] when any frontend stage fails.
pub fn compile_file_to_msgpack(
    input: &str,
    options: &FrontendOptions,
) -> Result<FrontendSerializedOutput<Vec<u8>>, FrontendFailure> {
    let output = compile_file(input, options)?;
    let data = output.to_msgpack();
    Ok(FrontendSerializedOutput {
        diagnostics: output.diagnostics,
        data,
    })
}

/// Run the full frontend pipeline for an in-memory program and serialize to
/// msgpack.
///
/// # Errors
///
/// Returns [`FrontendFailure`] when any frontend stage fails.
pub fn compile_program_to_msgpack(
    program: Program,
    source: &str,
    source_label: &str,
    options: &FrontendOptions,
) -> Result<FrontendSerializedOutput<Vec<u8>>, FrontendFailure> {
    let output = compile_program(program, source, source_label, options)?;
    let data = output.to_msgpack();
    Ok(FrontendSerializedOutput {
        diagnostics: output.diagnostics,
        data,
    })
}

#[derive(Debug, Deserialize)]
struct PackageSection {
    name: String,
}

#[derive(Debug, Deserialize)]
struct TomlManifest {
    package: Option<PackageSection>,
    #[serde(default)]
    dependencies: BTreeMap<String, String>,
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
    load_optional_toml(&dir.join("hew.toml"))
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
        check_file, check_program, compile_file, load_dependencies, load_lockfile,
        load_package_name, parse_source, FrontendOptions,
    };
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
            result
                .diagnostics
                .iter()
                .any(super::FrontendDiagnostic::is_warning),
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
            failure
                .diagnostics
                .iter()
                .any(super::FrontendDiagnostic::is_warning),
            "expected warning diagnostics, got: {:?}",
            failure.diagnostics
        );
    }

    #[test]
    fn compile_file_fails_when_warnings_are_errors() {
        let dir = tempfile::tempdir().expect("create temp dir");
        let input = write_source(dir.path(), "main.hew", "fn main() { let unused = 42; }\n");

        let failure = compile_file(
            &input,
            &FrontendOptions {
                warnings_as_errors: true,
                ..Default::default()
            },
        )
        .expect_err("frontend compile should fail when warnings_as_errors is enabled");

        assert_eq!(failure.message, "warnings treated as errors");
        assert!(
            failure
                .diagnostics
                .iter()
                .any(super::FrontendDiagnostic::is_warning),
            "expected warning diagnostics, got: {:?}",
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
}
