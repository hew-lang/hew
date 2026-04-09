//! Build command: parse, type-check, serialize to `MessagePack`, invoke the
//! embedded MLIR/LLVM backend, and link the final executable.

use std::collections::{HashMap, HashSet};
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};

use hew_parser::ast::{ImportDecl, Item, Spanned};

#[cfg(hew_embedded_codegen)]
use std::ffi::{CStr, CString};

use crate::target::TargetSpec;

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(
    clippy::enum_variant_names,
    reason = "variants match C API enum naming"
)]
enum EmbeddedCodegenMode {
    EmitMlir = 0,
    EmitLlvm = 1,
    EmitObject = 2,
}

#[cfg(hew_embedded_codegen)]
#[repr(C)]
#[derive(Debug, Clone, Copy)]
struct EmbeddedCodegenOptions {
    mode: u32,
    debug_info: u8,
    output_path: *const std::ffi::c_char,
    target_triple: *const std::ffi::c_char,
}

#[cfg(hew_embedded_codegen)]
#[repr(C)]
#[derive(Debug)]
struct EmbeddedCodegenBuffer {
    data: *mut std::ffi::c_char,
    len: usize,
}

#[cfg(hew_embedded_codegen)]
unsafe extern "C" {
    fn hew_codegen_compile_msgpack(
        data: *const u8,
        size: usize,
        options: *const EmbeddedCodegenOptions,
        text_output: *mut EmbeddedCodegenBuffer,
    ) -> std::ffi::c_int;
    fn hew_codegen_buffer_free(buffer: EmbeddedCodegenBuffer);
    fn hew_codegen_last_error() -> *const std::ffi::c_char;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum CodegenMode {
    #[default]
    LinkExecutable,
    EmitAst,
    EmitJson,
    EmitMsgpack,
    EmitMlir,
    EmitLlvm,
    EmitObj,
}

impl CodegenMode {
    fn embedded_mode(self) -> Option<EmbeddedCodegenMode> {
        match self {
            Self::LinkExecutable | Self::EmitAst | Self::EmitJson | Self::EmitMsgpack => None,
            Self::EmitMlir => Some(EmbeddedCodegenMode::EmitMlir),
            Self::EmitLlvm => Some(EmbeddedCodegenMode::EmitLlvm),
            Self::EmitObj => Some(EmbeddedCodegenMode::EmitObject),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct CompileOptions {
    pub no_typecheck: bool,
    pub codegen_mode: CodegenMode,
    pub target: Option<String>,
    pub extra_libs: Vec<String>,
    /// Build with debug info (no optimizations, no stripping).
    pub debug: bool,
    /// Override the package search directory (default: `.adze/packages/`).
    pub pkg_path: Option<PathBuf>,
}

/// Shared context for recursive import resolution.
///
/// Groups the parameters that remain constant across recursive calls to
/// [`resolve_file_imports()`] and [`parse_and_resolve_file()`], plus the
/// in-progress/cached import state shared across recursive resolution.
#[derive(Clone)]
struct ResolvedImport {
    items: Vec<Spanned<Item>>,
    item_source_paths: Vec<PathBuf>,
    source_paths: Vec<PathBuf>,
}

struct ImportResolutionContext<'a> {
    /// Canonical paths of files currently being resolved (prevents cycles).
    in_progress_imports: HashSet<PathBuf>,
    /// Fully resolved imports keyed by canonical path so sibling imports can
    /// reuse prior resolution without reopening fail-closed unresolved paths.
    resolved_imports: HashMap<PathBuf, ResolvedImport>,
    /// Dependencies declared in `hew.toml` (`None` in script mode).
    manifest_deps: Option<&'a [String]>,
    /// Override for the package search directory (default: `.adze/packages/`).
    extra_pkg_path: Option<&'a Path>,
    /// Locked dependency versions from the lockfile.
    locked_versions: Option<&'a [(String, String)]>,
    /// Name of the current package (from `hew.toml`).
    package_name: Option<&'a str>,
    /// Root directory of the project.
    project_dir: &'a Path,
}

/// Project metadata loaded from the source file and manifest.
struct ProjectContext {
    source: String,
    project_dir: PathBuf,
    manifest_deps: Option<Vec<String>>,
    package_name: Option<String>,
    locked_versions: Option<Vec<(String, String)>>,
}

/// Result of the type-checking stage.
struct TypeCheckResult {
    tco: Option<hew_types::check::TypeCheckOutput>,
    module_registry: hew_types::module_registry::ModuleRegistry,
}

/// Metadata required by the codegen serialization stage.
struct CodegenMetadata {
    handle_types: Vec<String>,
    handle_type_repr: std::collections::HashMap<String, String>,
    /// C drop functions for stdlib handle types with `impl Drop`.
    drop_funcs: Vec<(String, String)>,
    abs_source_path: Option<String>,
    line_map: Option<Vec<usize>>,
}

/// Build a line map: a Vec where entry\[i\] is the byte offset of the start of line (i+1).
/// Line 1 always starts at offset 0. Handles both `\n` and `\r\n` line endings.
fn line_map_from_source(source: &str) -> Vec<usize> {
    let mut map = vec![0usize]; // line 1 starts at byte 0
    let bytes = source.as_bytes();
    for (i, &byte) in bytes.iter().enumerate() {
        if byte == b'\n' {
            map.push(i + 1); // next line starts after the newline
        }
    }
    map
}

fn render_inferred_type_serialization_diagnostic(
    source: &str,
    filename: &str,
    imported_item_sources: &[(hew_parser::ast::Span, Option<PathBuf>)],
    error: &hew_serialize::TypeExprConversionError,
    fatal: bool,
) {
    let headline = if fatal {
        "cannot serialize inferred type for code generation"
    } else {
        "cannot serialize inferred type for code generation; omitting inferred serializer data"
    };
    if let Some(span) = error.span() {
        let detail = error.to_string();
        let notes = [super::diagnostic::DiagnosticNote {
            span,
            message: detail.as_str(),
        }];
        let suggestions = [String::from(
            "extend the Ty -> TypeExpr conversion in hew-serialize/src/enrich.rs or add an explicit type/coercion before codegen",
        )];
        match diagnostic_source_hint(span, imported_item_sources) {
            DiagnosticSourceHint::Imported(path) => match std::fs::read_to_string(&path) {
                Ok(imported_source) => {
                    if fatal {
                        super::diagnostic::render_diagnostic(
                            &imported_source,
                            &path.display().to_string(),
                            span,
                            headline,
                            &notes,
                            &suggestions,
                        );
                    } else {
                        super::diagnostic::render_warning(
                            &imported_source,
                            &path.display().to_string(),
                            span,
                            headline,
                            &notes,
                            &suggestions,
                        );
                    }
                }
                Err(_) => eprintln!(
                    "{}: cannot serialize inferred type for code generation in {} at {}..{}: {error}",
                    if fatal { "error" } else { "warning" },
                    path.display(),
                    span.start,
                    span.end
                ),
            },
            DiagnosticSourceHint::UnknownImported => eprintln!(
                "{}: cannot serialize inferred type for code generation in an imported module at {}..{}: {error}",
                if fatal { "error" } else { "warning" },
                span.start,
                span.end
            ),
            DiagnosticSourceHint::Root => {
                if fatal {
                    super::diagnostic::render_diagnostic(
                        source,
                        filename,
                        span,
                        headline,
                        &notes,
                        &suggestions,
                    );
                } else {
                    super::diagnostic::render_warning(
                        source,
                        filename,
                        span,
                        headline,
                        &notes,
                        &suggestions,
                    );
                }
            }
        }
    } else {
        eprintln!(
            "{}: cannot serialize inferred type for code generation: {error}",
            if fatal { "error" } else { "warning" }
        );
    }
}

fn inferred_type_serialization_diagnostic_is_fatal(
    error: &hew_serialize::TypeExprConversionError,
) -> bool {
    matches!(
        error.kind(),
        hew_serialize::TypeExprConversionKind::UnresolvedVar
            | hew_serialize::TypeExprConversionKind::ErrorSentinel
            | hew_serialize::TypeExprConversionKind::LiteralKind
            | hew_serialize::TypeExprConversionKind::MethodCallRewriteFailed
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
    error: &hew_serialize::TypeExprConversionError,
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

fn collect_new_inferred_type_diagnostics<'a>(
    diagnostics: &'a [hew_serialize::TypeExprConversionError],
    root_filename: &str,
    imported_item_sources: &[(hew_parser::ast::Span, Option<PathBuf>)],
    seen_spans: &mut HashSet<(String, usize, usize)>,
) -> Vec<&'a hew_serialize::TypeExprConversionError> {
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

/// Build search paths for module resolution.
///
/// Delegates to the shared implementation in `hew_types::module_registry` so
/// that the CLI and LSP always use the same search-path logic.
fn build_module_search_paths() -> Vec<PathBuf> {
    hew_types::module_registry::build_module_search_paths()
}

// ---------------------------------------------------------------------------
// Pipeline stage helpers
// ---------------------------------------------------------------------------

/// **Stage 1 — Setup.** Read the source file and load project metadata from
/// the manifest (`hew.toml`), lockfile, and package name.
fn load_project_context(input: &str) -> Result<ProjectContext, String> {
    let source =
        std::fs::read_to_string(input).map_err(|e| format!("Error: cannot read {input}: {e}"))?;
    let project_dir = Path::new(input)
        .parent()
        .unwrap_or(Path::new("."))
        .to_path_buf();
    let manifest_deps = super::manifest::load_dependencies(&project_dir);
    let package_name = super::manifest::load_package_name(&project_dir);
    let locked_versions = super::manifest::load_lockfile(&project_dir);
    Ok(ProjectContext {
        source,
        project_dir,
        manifest_deps,
        package_name,
        locked_versions,
    })
}

/// Render a single parser diagnostic, dispatching to the error or warning
/// renderer based on the error's severity.
fn render_parse_diagnostic(source: &str, filename: &str, err: &hew_parser::ParseError) {
    let hints: Vec<String> = err.hint.iter().cloned().collect();
    match err.severity {
        hew_parser::Severity::Warning => {
            super::diagnostic::render_warning(
                source,
                filename,
                &err.span,
                &err.message,
                &[],
                &hints,
            );
        }
        hew_parser::Severity::Error => {
            super::diagnostic::render_diagnostic(
                source,
                filename,
                &err.span,
                &err.message,
                &[],
                &hints,
            );
        }
    }
}

/// **Stage 2 — Parse.** Run the parser and render any diagnostics. Returns
/// the parsed program or an error if there were parse errors.
fn parse_source(source: &str, input: &str) -> Result<hew_parser::ast::Program, String> {
    let result = hew_parser::parse(source);
    if !result.errors.is_empty() {
        for err in &result.errors {
            render_parse_diagnostic(source, input, err);
        }
        if result
            .errors
            .iter()
            .any(|e| e.severity == hew_parser::Severity::Error)
        {
            return Err("parsing failed".into());
        }
    }
    Ok(result.program)
}

/// **Stage 3 — Import resolution.** Validate manifest imports, inject
/// synthetic imports for implicit stdlib dependencies, and build the module
/// graph by recursively resolving file and module-path imports.
fn resolve_imports(
    program: &mut hew_parser::ast::Program,
    source: &str,
    input: &str,
    project: &ProjectContext,
    options: &CompileOptions,
) -> Result<(), String> {
    if let Some(deps) = &project.manifest_deps {
        let errs = validate_imports_against_manifest(
            &program.items,
            deps,
            project.package_name.as_deref(),
        );
        if !errs.is_empty() {
            for e in &errs {
                eprintln!("{e}");
            }
            return Err("undeclared dependencies".into());
        }
    }

    // Inject synthetic imports for features that implicitly depend on stdlib
    // modules (wire types, regex literals). Must happen BEFORE resolve_file_imports
    // so the imports get their resolved_items populated.
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
    let module_graph = build_module_graph(
        input_path,
        &mut program.items,
        program.module_doc.clone(),
        &mut import_ctx,
    )
    .map_err(|errs| errs.join("\n"))?;
    program.module_graph = Some(module_graph);

    Ok(())
}

/// **Stage 4 — Type-check.** Run the type checker, render errors and warnings,
/// and return the type-check output together with the module registry.
///
/// Returns `Err` if any type errors are found.
fn typecheck_program(
    program: &hew_parser::ast::Program,
    source: &str,
    input: &str,
    target: &TargetSpec,
    options: &CompileOptions,
) -> Result<TypeCheckResult, String> {
    let search_paths = build_module_search_paths();
    let module_registry = hew_types::module_registry::ModuleRegistry::new(search_paths);

    if options.no_typecheck {
        return Ok(TypeCheckResult {
            tco: None,
            module_registry,
        });
    }

    let mut checker = hew_types::Checker::new(module_registry);
    if target.is_wasm() {
        checker.enable_wasm_target();
    }
    let tco = checker.check_program(program);
    let has_errors = !tco.errors.is_empty();

    // Build a map from dotted module name → (source_text, display_filename) so
    // diagnostics from non-root module bodies are rendered against the correct
    // source file rather than the root compilation unit.
    let module_source_map = super::diagnostic::build_module_source_map(program);
    super::diagnostic::render_type_diagnostics_with_sources(
        source,
        input,
        &tco.errors,
        &module_source_map,
    );
    super::diagnostic::render_type_diagnostics_with_sources(
        source,
        input,
        &tco.warnings,
        &module_source_map,
    );

    if has_errors {
        return Err("type errors found".into());
    }

    let module_registry = checker.into_module_registry();
    Ok(TypeCheckResult {
        tco: Some(tco),
        module_registry,
    })
}

/// **Stage 5 — Enrich.** Flatten imported items into the top-level program,
/// enrich the AST with inferred type information, sync enriched items back
/// to the module graph, and mark tail-call positions.
fn enrich_program_ast(
    program: &mut hew_parser::ast::Program,
    tco: Option<&hew_types::check::TypeCheckOutput>,
    module_registry: &hew_types::module_registry::ModuleRegistry,
    source: &str,
    input: &str,
) -> Result<
    (
        Vec<hew_serialize::ExprTypeEntry>,
        Vec<hew_serialize::MethodCallReceiverKindEntry>,
    ),
    String,
> {
    let root_module_item_count = program
        .module_graph
        .as_ref()
        .and_then(|mg| mg.modules.get(&mg.root).map(|module| module.items.len()));

    // Flatten resolved_items from module imports into top-level items.
    // Flattening must happen before enrichment so that normalize_all_types
    // and enrich_fn_decl process the imported functions too.
    let imported_item_sources = flatten_import_items(program);

    let (expr_type_map, method_call_receiver_kinds) = if let Some(tco) = tco {
        let mut seen_inferred_type_diagnostics = HashSet::new();
        let mut fatal_inferred_type_diagnostic = false;
        let enrich_diagnostics = hew_serialize::enrich_program(program, tco, module_registry)
            .map_err(|e| format!("Error: cannot enrich inferred types: {e}"))?;

        // These diagnostics come from inferred-type enrichment and serializer
        // preparation. Unresolved/error/literal-kind leaks are fatal because
        // codegen must not guess or silently default them late.
        for diagnostic in collect_new_inferred_type_diagnostics(
            enrich_diagnostics.diagnostics(),
            input,
            &imported_item_sources,
            &mut seen_inferred_type_diagnostics,
        ) {
            let fatal = inferred_type_serialization_diagnostic_is_fatal(diagnostic);
            fatal_inferred_type_diagnostic |= fatal;
            render_inferred_type_serialization_diagnostic(
                source,
                input,
                &imported_item_sources,
                diagnostic,
                fatal,
            );
        }
        // Sync enriched items back to module graph root so C++ codegen uses
        // enriched (type-annotated) items rather than the pre-enrichment clone.
        if let Some(ref mut mg) = program.module_graph {
            if let Some(root_module) = mg.modules.get_mut(&mg.root) {
                // Imported module items are already present in their own
                // module_graph nodes. Keep the root module scoped to its
                // original items so codegen doesn't duplicate imported bodies
                // under the root module path.
                if let Some(root_len) = root_module_item_count {
                    root_module.items = program.items.iter().take(root_len).cloned().collect();
                } else {
                    root_module.items.clone_from(&program.items);
                }
            }
            // Normalize types in non-root modules so imported items use the
            // serializer-ready TypeExpr forms during codegen.
            for (id, module) in &mut mg.modules {
                if *id != mg.root {
                    hew_serialize::normalize_items_types(&mut module.items, module_registry);
                }
            }
        }
        let expr_type_map_build = hew_serialize::build_expr_type_map(tco);
        let method_call_receiver_kinds =
            hew_serialize::build_method_call_receiver_kind_entries(program, tco);

        for diagnostic in collect_new_inferred_type_diagnostics(
            expr_type_map_build.diagnostics(),
            input,
            &imported_item_sources,
            &mut seen_inferred_type_diagnostics,
        ) {
            let fatal = inferred_type_serialization_diagnostic_is_fatal(diagnostic);
            fatal_inferred_type_diagnostic |= fatal;
            render_inferred_type_serialization_diagnostic(
                source,
                input,
                &imported_item_sources,
                diagnostic,
                fatal,
            );
        }
        if fatal_inferred_type_diagnostic {
            return Err("inferred type serialization failed".into());
        }
        (expr_type_map_build.entries, method_call_receiver_kinds)
    } else {
        (Vec::new(), Vec::new())
    };

    // Mark tail calls (purely syntactic, must run after enrichment so that
    // MethodCall→Call rewrites are already in place).
    hew_parser::tail_call::mark_tail_calls(program);

    Ok((expr_type_map, method_call_receiver_kinds))
}

/// **Stage 6 — Codegen metadata.** Build handle type metadata for C++ codegen
/// and preserve source metadata (source path + line map) for diagnostics in
/// all builds. `--debug` still controls DWARF emission and optimization level.
fn build_codegen_metadata(
    module_registry: &hew_types::module_registry::ModuleRegistry,
    input: &str,
    source: &str,
    _options: &CompileOptions,
) -> CodegenMetadata {
    let handle_types = module_registry.all_handle_types();
    let handle_type_repr: std::collections::HashMap<String, String> = handle_types
        .iter()
        .filter(|t| hew_types::stdlib::handle_type_representation(t) != "handle")
        .map(|t| {
            (
                t.clone(),
                hew_types::stdlib::handle_type_representation(t).to_string(),
            )
        })
        .collect();

    let path = std::fs::canonicalize(input)
        .map_or_else(|_| input.to_string(), |p| p.display().to_string());
    let abs_source_path = Some(path);
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

/// **Stage 7 — Link.** Compile the serialized AST to an object file via the
/// embedded MLIR/LLVM backend, then link the result into a final executable.
fn compile_and_link(
    ast_data: &[u8],
    input: &str,
    output: Option<&str>,
    target: &TargetSpec,
    options: &CompileOptions,
) -> Result<String, String> {
    let obj_temp = tempfile::Builder::new()
        .prefix("hew_")
        .suffix(target.object_suffix())
        .tempfile()
        .map_err(|e| format!("Error: cannot create temp file: {e}"))?
        .into_temp_path();
    let obj_path = obj_temp.display().to_string();

    run_embedded_codegen(
        ast_data,
        EmbeddedCodegenMode::EmitObject,
        target,
        options,
        Some(&obj_path),
    )?;

    let default_output = default_output_name(input, target.executable_suffix(), "a.out");
    let output_path = output.unwrap_or(&default_output);
    super::link::link_executable(
        &obj_path,
        output_path,
        target,
        &options.extra_libs,
        options.debug,
    )?;

    // obj_temp (TempPath) auto-deletes on drop
    Ok(output_path.to_string())
}

fn default_output_name(input: &str, suffix: &str, fallback: &str) -> String {
    let name = Path::new(input)
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or(fallback)
        .to_string();
    if !suffix.is_empty() && !name.ends_with(suffix) {
        format!("{name}{suffix}")
    } else {
        name
    }
}

/// Write binary output to a file or stdout.
fn write_output(output: Option<&str>, data: &[u8]) -> Result<String, String> {
    if let Some(output_path) = output {
        fs::write(output_path, data)
            .map_err(|e| format!("Error: cannot write output to {output_path}: {e}"))?;
        Ok(output_path.to_string())
    } else {
        std::io::stdout()
            .write_all(data)
            .map_err(|e| format!("Error: cannot write output: {e}"))?;
        Ok(String::new())
    }
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Run the full compilation pipeline for a `.hew` source file.
///
/// When `check_only` is `true` the pipeline stops after type-checking and no
/// binary is produced.
///
/// # Errors
///
/// Returns a human-readable message when any pipeline stage fails.
pub fn compile(
    input: &str,
    output: Option<&str>,
    check_only: bool,
    options: &CompileOptions,
) -> Result<String, String> {
    let target = TargetSpec::from_requested(options.target.as_deref())?;

    // 1. Setup — read source, load manifest
    let project = load_project_context(input)?;

    // 2. Parse
    let mut program = parse_source(&project.source, input)?;

    // 3. Import resolution — validate, inject implicit imports, build module graph
    resolve_imports(&mut program, &project.source, input, &project, options)?;

    // 4. Type-check
    let TypeCheckResult {
        tco,
        module_registry,
    } = typecheck_program(&program, &project.source, input, &target, options)?;

    if check_only {
        return Ok(String::new());
    }

    // 5. Enrich AST with inferred types, flatten imports, mark tail calls
    let (expr_type_map, method_call_receiver_kinds) = enrich_program_ast(
        &mut program,
        tco.as_ref(),
        &module_registry,
        &project.source,
        input,
    )?;

    // Early exit: dump enriched AST as JSON
    if options.codegen_mode == CodegenMode::EmitAst {
        let json = serde_json::to_string_pretty(&program)
            .map_err(|e| format!("Error: cannot serialize AST: {e}"))?;
        println!("{json}");
        return Ok(String::new());
    }

    // 6. Build codegen metadata and serialize
    let meta = build_codegen_metadata(&module_registry, input, &project.source, options);
    let assign_target_kinds = tco.as_ref().map_or_else(Vec::new, |tco| {
        hew_serialize::build_assign_target_kind_entries(&program, tco)
    });
    let assign_target_shapes = tco.as_ref().map_or_else(Vec::new, |tco| {
        hew_serialize::build_assign_target_shape_entries(&program, tco)
    });

    if options.codegen_mode == CodegenMode::EmitJson {
        let json = hew_serialize::serialize_to_json(
            &program,
            expr_type_map,
            method_call_receiver_kinds,
            assign_target_kinds,
            assign_target_shapes,
            meta.handle_types,
            meta.handle_type_repr,
            meta.drop_funcs,
            meta.abs_source_path.as_deref(),
            meta.line_map.as_deref(),
        );
        println!("{json}");
        return Ok(String::new());
    }

    let ast_data = hew_serialize::serialize_to_msgpack(
        &program,
        expr_type_map,
        method_call_receiver_kinds,
        assign_target_kinds,
        assign_target_shapes,
        meta.handle_types,
        meta.handle_type_repr,
        meta.drop_funcs,
        meta.abs_source_path.as_deref(),
        meta.line_map.as_deref(),
    );

    if options.codegen_mode == CodegenMode::EmitMsgpack {
        return write_output(output, &ast_data);
    }

    // Early exit: emit MLIR/LLVM/object without linking
    if let Some(mode) = options.codegen_mode.embedded_mode() {
        if options.codegen_mode == CodegenMode::EmitObj {
            let default_output = default_output_name(input, target.object_suffix(), "output");
            let output_path = output.unwrap_or(&default_output);
            let _ = run_embedded_codegen(&ast_data, mode, &target, options, Some(output_path))?;
            return Ok(output_path.to_string());
        }

        let text_output = run_embedded_codegen(&ast_data, mode, &target, options, None)?;
        return write_output(output, &text_output);
    }

    if !target.can_link_with_host_tools() {
        return Err(target.unsupported_native_link_error());
    }

    // 7. Codegen to object file and link executable
    compile_and_link(&ast_data, input, output, &target, options)
}

/// Run the full post-parse compile pipeline for source that has already been
/// parsed in-process, skipping only the file-read step.
///
/// This is the fast path for `hew eval`: the REPL has already parsed the
/// source and shown type errors to the user; we run the remaining stages
/// **in the same order as `compile()`** — resolve imports, typecheck (with
/// fully-resolved stdlib), enrich, serialize, link — so that implicit-stdlib
/// type information (e.g. `regex.Pattern` struct layout) is available to the
/// enrichment and codegen passes.
///
/// `source` is the Hew source string used for diagnostic metadata and
/// implicit-import detection.  `source_label` is a synthetic path label used
/// wherever a file name is needed (e.g. `"<repl>"`).  `output_path` is where
/// the linked executable should be written.
///
/// # Why not reuse the caller's `TypeCheckOutput`?
///
/// The REPL's in-process typecheck runs on the raw parsed AST — before
/// `inject_implicit_imports` and `resolve_file_imports` have run.  Passing
/// that stale `tco` to `enrich_program_ast` causes type-annotation mismatches
/// (e.g. `hew_regex_new` call site vs. declaration type) that produce invalid
/// MLIR.  Running a fresh typecheck here, after import resolution, is the
/// minimal correct fix.
///
/// # Errors
///
/// Returns a human-readable message when any pipeline stage fails.
pub(crate) fn compile_from_source_checked(
    program: hew_parser::ast::Program,
    source: &str,
    source_label: &str,
    output_path: &str,
    options: &CompileOptions,
) -> Result<(), String> {
    let target = TargetSpec::from_requested(options.target.as_deref())?;

    // Synthesise a minimal ProjectContext: no manifest, CWD as project root.
    // This is sufficient for the REPL's self-contained synthetic programs.
    let project = ProjectContext {
        source: source.to_string(),
        project_dir: std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")),
        manifest_deps: None,
        package_name: None,
        locked_versions: None,
    };

    // Stage 3 — resolve imports: inject implicit stdlib imports (regex, wire
    // types) and build the module graph, resolving any stdlib file imports.
    // This MUST run before typecheck so the checker sees the stdlib items
    // (e.g. the concrete struct layout of regex.Pattern).
    let mut program = program;
    resolve_imports(&mut program, source, source_label, &project, options)?;

    // Stage 4 — typecheck with fully resolved imports.
    let TypeCheckResult {
        tco,
        module_registry,
    } = typecheck_program(&program, source, source_label, &target, options)?;

    // Stage 5 — enrich AST with inferred types, flatten imports, mark tail calls.
    let (expr_type_map, method_call_receiver_kinds) = enrich_program_ast(
        &mut program,
        tco.as_ref(),
        &module_registry,
        source,
        source_label,
    )?;

    // Stage 6 — build codegen metadata.
    let meta = build_codegen_metadata(&module_registry, source_label, source, options);
    let assign_target_kinds = tco.as_ref().map_or_else(Vec::new, |tco| {
        hew_serialize::build_assign_target_kind_entries(&program, tco)
    });
    let assign_target_shapes = tco.as_ref().map_or_else(Vec::new, |tco| {
        hew_serialize::build_assign_target_shape_entries(&program, tco)
    });

    // Serialize to msgpack.
    let ast_data = hew_serialize::serialize_to_msgpack(
        &program,
        expr_type_map,
        method_call_receiver_kinds,
        assign_target_kinds,
        assign_target_shapes,
        meta.handle_types,
        meta.handle_type_repr,
        meta.drop_funcs,
        meta.abs_source_path.as_deref(),
        meta.line_map.as_deref(),
    );

    if !target.can_link_with_host_tools() {
        return Err(target.unsupported_native_link_error());
    }

    // Stage 7 — codegen to object file and link executable.
    compile_and_link(&ast_data, source_label, Some(output_path), &target, options)?;

    Ok(())
}

// ---------------------------------------------------------------------------
// Embedded backend bridge
// ---------------------------------------------------------------------------

#[cfg(not(hew_embedded_codegen))]
fn run_embedded_codegen(
    ast_data: &[u8],
    mode: EmbeddedCodegenMode,
    target: &TargetSpec,
    options: &CompileOptions,
    output_path: Option<&str>,
) -> Result<Vec<u8>, String> {
    let _ = (ast_data, mode, target, options, output_path);
    Err(embedded_codegen_unavailable_error())
}

#[cfg(hew_embedded_codegen)]
fn run_embedded_codegen(
    ast_data: &[u8],
    mode: EmbeddedCodegenMode,
    target: &TargetSpec,
    options: &CompileOptions,
    output_path: Option<&str>,
) -> Result<Vec<u8>, String> {
    let output_path = output_path
        .map(|path| {
            CString::new(path).map_err(|_| format!("Error: output path contains NUL: {path}"))
        })
        .transpose()?;
    let target_triple = target
        .codegen_triple()
        .map(|target| {
            CString::new(target).map_err(|_| format!("Error: target triple contains NUL: {target}"))
        })
        .transpose()?;

    let ffi_options = EmbeddedCodegenOptions {
        mode: mode as u32,
        debug_info: u8::from(options.debug),
        output_path: output_path
            .as_ref()
            .map_or(std::ptr::null(), |path| path.as_ptr()),
        target_triple: target_triple
            .as_ref()
            .map_or(std::ptr::null(), |target| target.as_ptr()),
    };
    let mut buffer = EmbeddedCodegenBuffer {
        data: std::ptr::null_mut(),
        len: 0,
    };

    // SAFETY: hew_codegen_compile_msgpack is the C API entry point; ast_data
    // points to a valid msgpack buffer, ffi_options is stack-allocated and valid
    // for the call duration, buffer is an out-param filled by the callee.
    let status = unsafe {
        hew_codegen_compile_msgpack(
            ast_data.as_ptr(),
            ast_data.len(),
            &raw const ffi_options,
            &raw mut buffer,
        )
    };
    if status != 0 {
        return Err(last_embedded_codegen_error());
    }

    if buffer.data.is_null() || buffer.len == 0 {
        return Ok(Vec::new());
    }

    // SAFETY: buffer.data was allocated by the C codegen and is valid for
    // buffer.len bytes. We copy into owned Rust memory before freeing.
    let bytes = unsafe {
        let slice = std::slice::from_raw_parts(buffer.data.cast::<u8>(), buffer.len);
        let owned = slice.to_vec();
        hew_codegen_buffer_free(buffer);
        owned
    };
    Ok(bytes)
}

#[cfg(not(hew_embedded_codegen))]
fn embedded_codegen_unavailable_error() -> String {
    "embedded MLIR/LLVM codegen is unavailable in this build of hew; rebuild with LLVM/MLIR configured (set LLVM_PREFIX or LLVM_DIR/MLIR_DIR, and use HEW_EMBED_STATIC=1 when only static LLVM/MLIR libraries are available)".into()
}

#[cfg(hew_embedded_codegen)]
fn last_embedded_codegen_error() -> String {
    // SAFETY: hew_codegen_last_error returns either null or a valid C string
    // that remains valid until the next codegen call.
    let error = unsafe { hew_codegen_last_error() };
    if error.is_null() {
        "embedded codegen failed".into()
    } else {
        // SAFETY: non-null return is a valid NUL-terminated C string.
        unsafe { CStr::from_ptr(error) }
            .to_string_lossy()
            .into_owned()
    }
}

/// Validate that every module-path import in `items` is either a stdlib module
/// or declared in `manifest_deps`.  Returns a list of error strings.
pub(crate) fn validate_imports_against_manifest(
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
        // Skip stdlib and ecosystem modules — they don't need manifest entries
        if is_builtin_module(&module_str) {
            continue;
        }
        // Skip local project imports — they resolve to project source files
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

/// Returns `true` if the module path is a built-in stdlib or ecosystem module.
fn is_builtin_module(module_path: &str) -> bool {
    module_path.starts_with("std::")
        || module_path.starts_with("hew::")
        || module_path.starts_with("ecosystem::")
}

fn module_id_from_file(source_dir: &Path, canonical_path: &Path) -> hew_parser::module::ModuleId {
    use hew_parser::module::ModuleId;

    let without_ext = canonical_path.with_extension("");
    let rel = without_ext.strip_prefix(source_dir).unwrap_or(&without_ext);
    let mut segments: Vec<String> = rel
        .iter()
        .filter_map(|s| s.to_str())
        .map(std::string::ToString::to_string)
        .collect();

    if segments.is_empty() {
        segments.push(
            canonical_path
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("unknown")
                .to_string(),
        );
    }

    ModuleId::new(segments)
}

/// Build a [`ModuleGraph`] from the parsed program items.
///
/// This resolves all file and module-path imports (reusing the existing
/// `resolve_file_imports` logic), constructs [`Module`] nodes and import
/// edges, and computes a topological ordering.  The graph is stored on
/// `program.module_graph` for serialisation; the existing `resolved_items`
/// on each `ImportDecl` also stay populated because imported items are not
/// serialised directly and the enrichment/codegen pipeline still consumes the
/// flattened top-level item list.
#[expect(
    clippy::ptr_arg,
    reason = "items are cloned into module graph, needs Vec"
)]
fn build_module_graph(
    source_file: &Path,
    items: &mut Vec<Spanned<Item>>,
    module_doc: Option<String>,
    ctx: &mut ImportResolutionContext<'_>,
) -> Result<hew_parser::module::ModuleGraph, Vec<String>> {
    use hew_parser::module::{Module, ModuleGraph, ModuleId};

    let input_canonical =
        std::fs::canonicalize(source_file).unwrap_or_else(|_| source_file.to_path_buf());
    let source_dir = input_canonical.parent().unwrap_or(Path::new("."));

    // Phase 1: resolve imports so imported definitions remain available for
    // flattening into the top-level item list before enrichment/codegen.
    ctx.in_progress_imports.insert(input_canonical.clone());
    let resolve_result = resolve_file_imports(&input_canonical, items, ctx).map_err(|e| vec![e]);
    ctx.in_progress_imports.remove(&input_canonical);
    resolve_result?;

    // Phase 2: build the module graph from the resolved data.
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

    // Detect import cycles via topological sort.
    if let Err(cycle_err) = graph.compute_topo_order() {
        return Err(vec![cycle_err.to_string()]);
    }

    Ok(graph)
}

/// Walk `items` and build [`Module`] nodes + [`ModuleImport`] edges for every
/// resolved import, recursing into transitive dependencies.
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

        // Derive ModuleId from the import declaration.
        let (module_id, first_source_path) = if !decl.path.is_empty() {
            (ModuleId::new(decl.path.clone()), None)
        } else if let Some(fp) = &decl.file_path {
            let resolved = current_source.parent().unwrap_or(source_dir).join(fp);
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

        // Add the module node if not already present (handles diamond deps).
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
                // Use resolved_source_paths from ImportDecl if available
                // (populated for directory modules with multiple peer files).
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

/// Extract function/const/impl items from `ImportDecl::resolved_items` and
/// promote them to top-level program items. This makes imported pure-Hew
/// module functions visible to the C++ codegen (which only sees serialized
/// top-level items, since `resolved_items` is `#[serde(skip)]`).
fn flatten_import_items(
    program: &mut hew_parser::ast::Program,
) -> Vec<(hew_parser::ast::Span, Option<PathBuf>)> {
    let mut extra: Vec<Spanned<Item>> = Vec::new();
    let mut imported_item_sources = Vec::new();
    for (item, _span) in &mut program.items {
        if let Item::Import(decl) = item {
            if let Some(resolved) = decl.resolved_items.take() {
                let mut item_source_paths =
                    std::mem::take(&mut decl.resolved_item_source_paths).into_iter();
                for resolved_item in resolved {
                    let source_path = item_source_paths.next();
                    // Extract all non-import items so the C++ codegen sees them.
                    // Import items from the resolved file are not re-exported.
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

/// Recursively resolve `import "file.hew"` and module-path import declarations.
///
/// * `manifest_deps` — `Some(deps)` in package mode (validate against manifest),
///   `None` in script mode (no validation).
/// * `extra_pkg_path` — optional override for the package search root
///   (default: `<cwd>/.adze/packages/`).
#[expect(
    clippy::too_many_lines,
    reason = "sequential import resolution steps for file and module imports"
)]
fn resolve_file_imports(
    source_file: &Path,
    items: &mut [Spanned<Item>],
    ctx: &mut ImportResolutionContext<'_>,
) -> Result<(), String> {
    let source_dir = source_file
        .parent()
        .expect("source file should have a parent directory");

    let import_indices: Vec<usize> = items
        .iter()
        .enumerate()
        .filter_map(|(i, (item, _span))| {
            if let Item::Import(decl) = item {
                if decl.file_path.is_some() {
                    return Some(i);
                }
                if !decl.path.is_empty() {
                    return Some(i);
                }
            }
            None
        })
        .collect();

    let cwd = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));

    for idx in &import_indices {
        let canonical = match &items[*idx].0 {
            Item::Import(decl) if decl.file_path.is_some() => {
                let file_path = decl.file_path.as_ref().unwrap();
                let resolved = source_dir.join(file_path);
                if let Ok(c) = resolved.canonicalize() {
                    c
                } else {
                    return Err(format!(
                        "Error: imported file not found: {file_path} (resolved to {})",
                        resolved.display()
                    ));
                }
            }
            Item::Import(decl) if !decl.path.is_empty() => {
                let module_str = decl.path.join("::");
                // Check if this is a local project import (first segment matches package name).
                let is_local = ctx
                    .package_name
                    .is_some_and(|pkg| decl.path.first().is_some_and(|seg| seg == pkg));
                let rest_path: Vec<&str> = if is_local {
                    decl.path[1..].iter().map(String::as_str).collect()
                } else {
                    Vec::new()
                };

                let rel_path: PathBuf = decl.path.iter().collect::<PathBuf>().with_extension("hew");
                // Also try package-directory form: std/encoding/json/json.hew
                let last = decl.path.last().expect("path is non-empty");
                let dir_path: PathBuf = decl
                    .path
                    .iter()
                    .collect::<PathBuf>()
                    .join(format!("{last}.hew"));
                let exe_dir = std::env::current_exe()
                    .ok()
                    .and_then(|p| p.parent().map(std::path::Path::to_path_buf));
                let mut candidates = Vec::new();

                // Local project imports resolve relative to the project root
                if is_local && !rest_path.is_empty() {
                    let local_last = *rest_path.last().unwrap();
                    let local_rel: PathBuf = rest_path.iter().collect();
                    let local_dir = local_rel.join(format!("{local_last}.hew"));
                    let local_flat = local_rel.with_extension("hew");
                    // src/ subdirectory (preferred)
                    candidates.push(ctx.project_dir.join("src").join(&local_dir));
                    candidates.push(ctx.project_dir.join("src").join(&local_flat));
                    // project root
                    candidates.push(ctx.project_dir.join(&local_dir));
                    candidates.push(ctx.project_dir.join(&local_flat));
                }

                // Standard candidates for non-local imports
                // (also serve as fallback for local imports)
                candidates.extend([
                    // Directory form first (preferred for packages)
                    source_dir.join(&dir_path),
                    cwd.join(&dir_path),
                    // Flat form
                    source_dir.join(&rel_path),
                    cwd.join(&rel_path),
                ]);
                // Versioned package paths from lockfile (tried before unversioned)
                if let Some(version) = ctx
                    .locked_versions
                    .and_then(|lv| lv.iter().find(|(n, _)| n == &module_str))
                    .map(|(_, v)| v.as_str())
                {
                    let module_dir: PathBuf = decl.path.iter().collect();
                    let entry_file =
                        format!("{}.hew", decl.path.last().expect("path is non-empty"));
                    let versioned_rel = module_dir.join(version).join(entry_file);
                    candidates.push(cwd.join(".adze/packages").join(&versioned_rel));
                    if let Some(pkg) = ctx.extra_pkg_path {
                        candidates.push(pkg.join(&versioned_rel));
                    }
                }
                // Unversioned package paths (fallback)
                candidates.push(cwd.join(".adze/packages").join(&rel_path));
                candidates.push(cwd.join(".adze/packages").join(&dir_path));
                // Custom package path (--pkg-path flag)
                // Try full path and also path with first segment stripped (e.g. `hew::db::sqlite`
                // lives at `{pkg}/db/sqlite/sqlite.hew`, not `{pkg}/hew/db/sqlite/sqlite.hew`).
                if let Some(pkg) = ctx.extra_pkg_path {
                    candidates.push(pkg.join(&dir_path));
                    candidates.push(pkg.join(&rel_path));
                    if decl.path.len() > 1 {
                        let rest_dir: PathBuf = decl.path[1..]
                            .iter()
                            .collect::<PathBuf>()
                            .join(format!("{last}.hew"));
                        let rest_flat: PathBuf = decl.path[1..]
                            .iter()
                            .collect::<PathBuf>()
                            .with_extension("hew");
                        candidates.push(pkg.join(&rest_dir));
                        candidates.push(pkg.join(&rest_flat));
                    }
                }
                // For hew:: ecosystem modules with a custom pkg_path, also try
                // without the leading "hew" segment. This supports local ecosystem
                // checkouts where files live at db/sqlite/sqlite.hew rather than
                // hew/db/sqlite/sqlite.hew (the adze-installed layout).
                if module_str.starts_with("hew::") && decl.path.len() > 1 {
                    let tail: PathBuf = decl.path[1..].iter().collect();
                    let tail_last = decl.path.last().expect("path is non-empty");
                    let tail_dir = tail.join(format!("{tail_last}.hew"));
                    let tail_rel = tail.with_extension("hew");
                    if let Some(pkg) = ctx.extra_pkg_path {
                        candidates.push(pkg.join(&tail_dir));
                        candidates.push(pkg.join(&tail_rel));
                    }
                }
                // HEW_STD env var: points to std/ directory; parent is the search root
                if let Ok(hew_std) = std::env::var("HEW_STD") {
                    let std_root = PathBuf::from(hew_std);
                    if let Some(parent) = std_root.parent() {
                        candidates.push(parent.join(&dir_path));
                        candidates.push(parent.join(&rel_path));
                    }
                }
                // FHS installed location: <prefix>/share/hew (contains std/)
                // e.g. /usr/local/bin/hew → /usr/local/share/hew → std/
                if let Some(ref exe) = exe_dir {
                    let fhs_root = exe.join("../share/hew");
                    candidates.push(fhs_root.join(&dir_path));
                    candidates.push(fhs_root.join(&rel_path));
                }
                // Dev fallback: two levels above the binary (repo root when std/ is at repo root)
                if let Some(ref exe) = exe_dir {
                    if let Some(project_root) = exe.parent().and_then(|p| p.parent()) {
                        candidates.push(project_root.join(&dir_path));
                        candidates.push(project_root.join(&rel_path));
                    }
                }
                if let Some(c) = candidates.iter().find_map(|p| p.canonicalize().ok()) {
                    c
                } else {
                    let tried = candidates
                        .iter()
                        .map(|p| p.display().to_string())
                        .collect::<Vec<_>>()
                        .join(", ");
                    // Hint based on whether we're in package mode.
                    let hint = if ctx.manifest_deps.is_some_and(|d| d.contains(&module_str)) {
                        "\n  hint: this dependency is declared in hew.toml — run `adze install`"
                    } else if ctx.manifest_deps.is_some() {
                        "\n  hint: add this module to [dependencies] in hew.toml"
                    } else {
                        ""
                    };
                    return Err(format!(
                        "Error: module `{module_str}` not found (tried: {tried}){hint}"
                    ));
                }
            }
            _ => continue,
        };

        let Some(resolved_import) = resolve_completed_import(&canonical, ctx, &items[*idx].0)?
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

fn resolve_completed_import(
    canonical: &Path,
    ctx: &mut ImportResolutionContext<'_>,
    import_item: &Item,
) -> Result<Option<ResolvedImport>, String> {
    if let Some(cached) = ctx.resolved_imports.get(canonical) {
        return Ok(Some(cached.clone()));
    }
    if ctx.in_progress_imports.contains(canonical) {
        return Ok(None);
    }

    ctx.in_progress_imports.insert(canonical.to_path_buf());
    let resolved = build_resolved_import(canonical, ctx, import_item);
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

fn build_resolved_import(
    canonical: &Path,
    ctx: &mut ImportResolutionContext<'_>,
    import_item: &Item,
) -> Result<ResolvedImport, String> {
    // Check if this is a directory-form module (e.g. std/net/http/http.hew).
    // If so, collect all peer .hew files in that directory and merge their items.
    let module_dir = canonical.parent();
    let is_directory_module = module_dir.is_some_and(|dir| {
        let dir_name = dir.file_name().and_then(|n| n.to_str());
        let file_stem = canonical.file_stem().and_then(|n| n.to_str());
        dir_name.is_some() && dir_name == file_stem
    });

    let peer_files = if is_directory_module {
        let dir = module_dir.unwrap();
        let mut peers: Vec<PathBuf> = std::fs::read_dir(dir)
            .ok()
            .into_iter()
            .flatten()
            .filter_map(std::result::Result::ok)
            .map(|e| e.path())
            .filter(|p| p.extension().and_then(|e| e.to_str()) == Some("hew") && *p != canonical)
            .collect();
        peers.sort(); // deterministic order
        peers
    } else {
        Vec::new()
    };

    let mut import_items = parse_and_resolve_file(canonical, ctx)?;
    let mut import_item_source_paths = vec![canonical.to_path_buf(); import_items.len()];
    let mut source_paths = vec![canonical.to_path_buf()];

    // Parse and merge peer files for directory modules.
    for peer in &peer_files {
        let peer_canonical = peer.canonicalize().unwrap_or_else(|_| peer.clone());
        let Some(peer_resolved) = resolve_completed_import(&peer_canonical, ctx, import_item)?
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

    // Check for duplicate pub names in multi-file modules.
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
        check_duplicate_pub_names(&import_items, &module_str)?;
    }

    Ok(ResolvedImport {
        items: import_items,
        item_source_paths: import_item_source_paths,
        source_paths,
    })
}

/// Parse a single `.hew` file, report diagnostics, and recursively resolve its imports.
fn parse_and_resolve_file(
    canonical: &Path,
    ctx: &mut ImportResolutionContext<'_>,
) -> Result<Vec<Spanned<Item>>, String> {
    let source = std::fs::read_to_string(canonical)
        .map_err(|e| format!("Error reading imported file '{}': {e}", canonical.display()))?;

    let result = hew_parser::parse(&source);
    if !result.errors.is_empty() {
        let display_path = canonical.display().to_string();
        for err in &result.errors {
            render_parse_diagnostic(&source, &display_path, err);
        }
        if result
            .errors
            .iter()
            .any(|e| e.severity == hew_parser::Severity::Error)
        {
            return Err(format!(
                "parsing failed in imported file '{}'",
                canonical.display()
            ));
        }
    }

    let mut import_items = result.program.items;
    resolve_file_imports(canonical, &mut import_items, ctx)?;
    Ok(import_items)
}

/// Check for duplicate `pub` item names across files in a multi-file module.
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

/// Inject synthetic `import` declarations for features that implicitly depend on
/// stdlib modules:
///
/// * Wire types with `#[json(...)]` → `import std::encoding::json`
/// * Wire types with `#[yaml(...)]` → `import std::encoding::yaml`
/// * Regex literals (`re"..."`)     → `import std::text::regex`
///
/// This ensures the normal pipeline (type-check → extern synthesis → linking)
/// handles these dependencies with no special cases.
fn inject_implicit_imports(items: &mut Vec<Spanned<Item>>, source: &str) {
    // Collect already-imported module paths to avoid duplicates.
    let existing: HashSet<String> = items
        .iter()
        .filter_map(|(item, _)| {
            if let Item::Import(decl) = item {
                if !decl.path.is_empty() {
                    return Some(decl.path.join("::"));
                }
            }
            None
        })
        .collect();

    let mut needed: Vec<Vec<String>> = Vec::new();

    // Detect regex literals from lexer tokens so comments and docstrings do not
    // trigger phantom imports.
    if source_contains_regex_literal(source) {
        let path = ["std", "text", "regex"];
        let key = path.join("::");
        if !existing.contains(&key) {
            needed.push(path.iter().map(|s| (*s).to_string()).collect());
        }
    }

    // De-duplicate and inject.
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

#[cfg(test)]
mod tests {
    use super::*;
    use hew_parser::ast::{Block, Expr, FnDecl, Pattern, Program, Stmt, Visibility};
    use hew_types::{
        check::TypeCheckOutput, error::TypeErrorKind, module_registry::ModuleRegistry, Ty,
    };

    fn make_module_import(path: &[&str]) -> Spanned<Item> {
        let decl = hew_parser::ast::ImportDecl {
            path: path.iter().map(ToString::to_string).collect(),
            spec: None,
            file_path: None,
            resolved_items: None,
            resolved_item_source_paths: Vec::new(),
            resolved_source_paths: Vec::new(),
        };
        (Item::Import(decl), 0..0)
    }

    fn make_file_import(file: &str) -> Spanned<Item> {
        let decl = hew_parser::ast::ImportDecl {
            path: vec![],
            spec: None,
            file_path: Some(file.to_string()),
            resolved_items: None,
            resolved_item_source_paths: Vec::new(),
            resolved_source_paths: Vec::new(),
        };
        (Item::Import(decl), 0..0)
    }

    struct TestFixtureDir {
        path: PathBuf,
    }

    impl TestFixtureDir {
        fn new(name: &str, files: &[(&str, &str)]) -> Self {
            let path = Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("target")
                .join("compile-tests")
                .join(name);
            let _ = fs::remove_dir_all(&path);
            fs::create_dir_all(&path).expect("fixture directory should be created");
            for (relative_path, contents) in files {
                let file_path = path.join(relative_path);
                if let Some(parent) = file_path.parent() {
                    fs::create_dir_all(parent).expect("fixture parent directories should exist");
                }
                fs::write(&file_path, contents).expect("fixture file should be written");
            }
            Self { path }
        }

        fn join(&self, relative_path: &str) -> PathBuf {
            self.path.join(relative_path)
        }
    }

    impl Drop for TestFixtureDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.path);
        }
    }

    fn find_file_import<'a>(items: &'a [Spanned<Item>], file: &str) -> &'a ImportDecl {
        items
            .iter()
            .find_map(|(item, _)| match item {
                Item::Import(decl) if decl.file_path.as_deref() == Some(file) => Some(decl),
                _ => None,
            })
            .unwrap_or_else(|| panic!("expected file import `{file}`"))
    }

    fn implicit_import_paths_for(source: &str) -> Vec<Vec<String>> {
        let mut program = parse_source(source, "test.hew").expect("source should parse");
        inject_implicit_imports(&mut program.items, source);
        program
            .items
            .iter()
            .filter_map(|(item, _)| {
                if let Item::Import(decl) = item {
                    Some(decl.path.clone())
                } else {
                    None
                }
            })
            .collect()
    }

    fn empty_tco() -> TypeCheckOutput {
        TypeCheckOutput {
            expr_types: HashMap::new(),
            assign_target_kinds: HashMap::new(),
            assign_target_shapes: HashMap::new(),
            errors: vec![],
            warnings: vec![],
            type_defs: HashMap::new(),
            fn_sigs: HashMap::new(),
            cycle_capable_actors: HashSet::new(),
            user_modules: HashSet::new(),
            call_type_args: HashMap::new(),
            method_call_receiver_kinds: HashMap::new(),
            method_call_rewrites: HashMap::new(),
        }
    }

    #[test]
    fn validate_no_manifest_allows_all() {
        // When manifest exists but has no deps, undeclared imports are flagged.
        let items = vec![make_module_import(&["mylib", "utils"])];
        let errs = validate_imports_against_manifest(&items, &[], None);
        assert_eq!(errs.len(), 1, "undeclared import should produce an error");
    }

    #[test]
    fn validate_declared_dep_is_ok() {
        let items = vec![make_module_import(&["mylib", "utils"])];
        let deps = vec!["mylib::utils".to_string()];
        let errs = validate_imports_against_manifest(&items, &deps, None);
        assert!(errs.is_empty());
    }

    #[test]
    fn validate_undeclared_dep_errors() {
        let items = vec![make_module_import(&["mylib", "utils"])];
        let deps: Vec<String> = vec!["mylib::other".to_string()];
        let errs = validate_imports_against_manifest(&items, &deps, None);
        assert_eq!(errs.len(), 1);
        assert!(errs[0].contains("mylib::utils"));
        assert!(errs[0].contains("adze add"));
    }

    #[test]
    fn validate_stdlib_import_is_always_ok() {
        // std::fs is a known stdlib module
        let items = vec![make_module_import(&["std", "fs"])];
        let deps: Vec<String> = vec![];
        let errs = validate_imports_against_manifest(&items, &deps, None);
        assert!(errs.is_empty(), "stdlib imports are always allowed");
    }

    #[test]
    fn validate_file_import_is_not_validated() {
        let items = vec![make_file_import("./lib.hew")];
        let deps: Vec<String> = vec![];
        let errs = validate_imports_against_manifest(&items, &deps, None);
        assert!(
            errs.is_empty(),
            "file-path imports are not subject to manifest validation"
        );
    }

    #[test]
    fn validate_multiple_imports_reports_all_errors() {
        let items = vec![
            make_module_import(&["mylib", "a"]),
            make_module_import(&["mylib", "b"]),
            make_module_import(&["mylib", "c"]),
        ];
        let deps = vec!["mylib::a".to_string()];
        let errs = validate_imports_against_manifest(&items, &deps, None);
        assert_eq!(errs.len(), 2);
    }

    #[test]
    fn validate_local_import_is_exempt() {
        // Imports matching the package name are local and skip manifest validation.
        let items = vec![make_module_import(&["myapp", "models"])];
        let errs = validate_imports_against_manifest(&items, &[], Some("myapp"));
        assert!(
            errs.is_empty(),
            "local imports should be exempt from manifest validation"
        );
    }

    #[test]
    fn inject_implicit_imports_ignores_regex_text_in_comments_and_docstrings() {
        let source = r#"//! Module docs mentioning re"[0-9]+"
/// Function docs mentioning re"[a-z]+"
fn main() {
    // Comment mentioning re"foo"
}
"#;

        let imports = implicit_import_paths_for(source);
        assert!(
            !imports
                .iter()
                .any(|path| path == &["std".to_string(), "text".to_string(), "regex".to_string()]),
            "comments and docstrings should not trigger regex auto-imports"
        );
    }

    #[test]
    fn inject_implicit_imports_adds_regex_import_for_regex_literals() {
        let source = r#"fn main() {
    let digits = re"[0-9]+";
}
"#;

        let imports = implicit_import_paths_for(source);
        assert!(
            imports
                .iter()
                .any(|path| path == &["std".to_string(), "text".to_string(), "regex".to_string()]),
            "regex literals should still trigger regex auto-imports"
        );
    }

    #[test]
    fn test_line_map_simple() {
        let map = line_map_from_source("hello\nworld\n");
        assert_eq!(map, vec![0, 6, 12]);
    }

    #[test]
    fn test_line_map_single_line() {
        let map = line_map_from_source("no newline");
        assert_eq!(map, vec![0]);
    }

    #[test]
    fn test_line_map_empty() {
        let map = line_map_from_source("");
        assert_eq!(map, vec![0]);
    }

    #[test]
    fn default_output_name_uses_target_specific_object_suffixes() {
        let darwin = TargetSpec::from_requested(Some("arm64-apple-darwin")).expect("darwin target");
        let linux =
            TargetSpec::from_requested(Some("x86_64-unknown-linux-gnu")).expect("linux target");
        let windows =
            TargetSpec::from_requested(Some("x86_64-pc-windows-gnu")).expect("windows target");

        assert_eq!(
            default_output_name("examples/main.hew", darwin.object_suffix(), "output"),
            "main.o"
        );
        assert_eq!(
            default_output_name("examples/main.hew", linux.object_suffix(), "output"),
            "main.o"
        );
        assert_eq!(
            default_output_name("examples/main.hew", windows.object_suffix(), "output"),
            "main.obj"
        );
    }

    #[test]
    fn inferred_type_diagnostics_deduplicate_same_span_across_passes() {
        let expr_span = 10..18;
        let mut program = Program {
            items: vec![(
                Item::Function(FnDecl {
                    attributes: vec![],
                    is_async: false,
                    is_generator: false,
                    visibility: Visibility::Private,
                    is_pure: false,
                    name: "foo".into(),
                    type_params: None,
                    params: vec![],
                    return_type: None,
                    where_clause: None,
                    body: Block {
                        stmts: vec![(
                            Stmt::Let {
                                pattern: (Pattern::Identifier("value".into()), expr_span.clone()),
                                ty: None,
                                value: Some((
                                    Expr::Identifier("count_up".into()),
                                    expr_span.clone(),
                                )),
                            },
                            expr_span.clone(),
                        )],
                        trailing_expr: None,
                    },
                    doc_comment: None,
                    decl_span: 0..0,
                }),
                0..0,
            )],
            module_doc: None,
            module_graph: None,
        };
        let mut tco = empty_tco();
        tco.expr_types.insert(
            hew_types::check::SpanKey {
                start: expr_span.start,
                end: expr_span.end,
            },
            Ty::generator(Ty::I32, Ty::Unit),
        );

        let registry = hew_types::module_registry::ModuleRegistry::new(vec![]);
        let enrich_diagnostics =
            hew_serialize::enrich_program(&mut program, &tco, &registry).unwrap();
        let expr_type_map_build = hew_serialize::build_expr_type_map(&tco);
        let mut seen = HashSet::new();
        let imported_item_sources = Vec::new();

        let first = collect_new_inferred_type_diagnostics(
            enrich_diagnostics.diagnostics(),
            "main.hew",
            &imported_item_sources,
            &mut seen,
        );
        let second = collect_new_inferred_type_diagnostics(
            expr_type_map_build.diagnostics(),
            "main.hew",
            &imported_item_sources,
            &mut seen,
        );

        assert_eq!(first.len(), 1);
        assert_eq!(second.len(), 0);
    }

    #[test]
    fn unresolved_inferred_types_abort_serialization() {
        let expr_span = 10..18;
        let mut program = Program {
            items: vec![(
                Item::Function(FnDecl {
                    attributes: vec![],
                    is_async: false,
                    is_generator: false,
                    visibility: Visibility::Private,
                    is_pure: false,
                    name: "foo".into(),
                    type_params: None,
                    params: vec![],
                    return_type: None,
                    where_clause: None,
                    body: Block {
                        stmts: vec![(
                            Stmt::Let {
                                pattern: (Pattern::Identifier("value".into()), expr_span.clone()),
                                ty: None,
                                value: Some((Expr::Identifier("input".into()), expr_span.clone())),
                            },
                            expr_span.clone(),
                        )],
                        trailing_expr: None,
                    },
                    doc_comment: None,
                    decl_span: 0..0,
                }),
                0..0,
            )],
            module_doc: None,
            module_graph: None,
        };
        let mut tco = empty_tco();
        tco.expr_types.insert(
            hew_types::check::SpanKey {
                start: expr_span.start,
                end: expr_span.end,
            },
            Ty::Var(hew_types::ty::TypeVar(7)),
        );

        let result = enrich_program_ast(
            &mut program,
            Some(&tco),
            &hew_types::module_registry::ModuleRegistry::new(vec![]),
            "fn foo() { let value = input; }",
            "main.hew",
        );

        assert!(
            matches!(result, Err(ref err) if err.contains("inferred type serialization failed")),
            "unresolved inferred types must abort serialization, got: {result:?}"
        );
    }

    #[test]
    fn literal_kind_inferred_types_abort_serialization() {
        let expr_span = 10..11;
        let mut program = Program {
            items: vec![(
                Item::Function(FnDecl {
                    attributes: vec![],
                    is_async: false,
                    is_generator: false,
                    visibility: Visibility::Private,
                    is_pure: false,
                    name: "foo".into(),
                    type_params: None,
                    params: vec![],
                    return_type: None,
                    where_clause: None,
                    body: Block {
                        stmts: vec![(
                            Stmt::Let {
                                pattern: (Pattern::Identifier("value".into()), expr_span.clone()),
                                ty: None,
                                value: Some((
                                    Expr::Literal(hew_parser::ast::Literal::Integer {
                                        value: 1,
                                        radix: hew_parser::ast::IntRadix::Decimal,
                                    }),
                                    expr_span.clone(),
                                )),
                            },
                            expr_span.clone(),
                        )],
                        trailing_expr: None,
                    },
                    doc_comment: None,
                    decl_span: 0..0,
                }),
                0..0,
            )],
            module_doc: None,
            module_graph: None,
        };
        let mut tco = empty_tco();
        tco.expr_types.insert(
            hew_types::check::SpanKey {
                start: expr_span.start,
                end: expr_span.end,
            },
            Ty::IntLiteral,
        );

        let result = enrich_program_ast(
            &mut program,
            Some(&tco),
            &hew_types::module_registry::ModuleRegistry::new(vec![]),
            "fn foo() { let value = 1; }",
            "main.hew",
        );

        assert!(
            matches!(result, Err(ref err) if err.contains("inferred type serialization failed")),
            "literal-kind inferred types must abort serialization, got: {result:?}"
        );
    }

    #[test]
    fn option_constructor_expected_context_serializes_after_literal_coercion() {
        let source = "fn make() -> Option<int> { Some(42) }\n";
        let mut program = parse_source(source, "main.hew").expect("source should parse");

        let mut checker = hew_types::Checker::new(ModuleRegistry::new(vec![]));
        let tco = checker.check_program(&program);
        assert!(
            tco.errors.is_empty(),
            "typecheck should succeed before enrichment: {:?}",
            tco.errors
        );

        let module_registry = checker.into_module_registry();
        let result = enrich_program_ast(
            &mut program,
            Some(&tco),
            &module_registry,
            source,
            "main.hew",
        );

        assert!(
            result.is_ok(),
            "expected constructor literal coercion to survive enrichment, got: {result:?}"
        );
    }

    #[test]
    fn unannotated_immutable_literal_bindings_serialize_after_output_materialization() {
        let source = "fn main() { let whole = 1; let frac = 2.0; }\n";
        let mut program = parse_source(source, "main.hew").expect("source should parse");

        let mut checker = hew_types::Checker::new(ModuleRegistry::new(vec![]));
        let tco = checker.check_program(&program);
        assert!(
            tco.errors.is_empty(),
            "typecheck should succeed before enrichment: {:?}",
            tco.errors
        );
        assert!(
            !tco.expr_types.values().any(Ty::is_numeric_literal),
            "checked output should materialize literal kinds before enrichment: {:?}",
            tco.expr_types
        );

        let module_registry = checker.into_module_registry();
        let result = enrich_program_ast(
            &mut program,
            Some(&tco),
            &module_registry,
            source,
            "main.hew",
        );

        assert!(
            result.is_ok(),
            "expected materialized literal bindings to survive enrichment, got: {result:?}"
        );
    }

    #[test]
    fn typecheck_rejects_unresolved_inferred_binding_before_enrichment() {
        let source = "fn main() {\n    let f = (x) => x;\n}\n";
        let program = parse_source(source, "main.hew").expect("source should parse");
        let target = TargetSpec::from_requested(None).expect("host target");

        let err = typecheck_program(
            &program,
            source,
            "main.hew",
            &target,
            &CompileOptions::default(),
        );
        let Err(err) = err else {
            panic!("typecheck should reject unresolved inferred bindings");
        };
        assert_eq!(err, "type errors found");

        let mut checker = hew_types::Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);
        assert!(
            output
                .errors
                .iter()
                .any(|error| matches!(error.kind, TypeErrorKind::InferenceFailed)),
            "expected typecheck-stage InferenceFailed, got: {:?}",
            output.errors
        );
    }

    #[test]
    fn build_module_graph_reuses_resolved_file_imports_for_diamond_deps() {
        let fixture = TestFixtureDir::new(
            "diamond-file-import-cache",
            &[
                (
                    "module_diamond.hew",
                    r#"import "diamond_base.hew";
import "diamond_left.hew";
import "diamond_right.hew";

fn main() {
    let b = base_value();
    let l = left_value();
    let r = right_value();
    println(f"{b}");
    println(f"{l}");
    println(f"{r}");
    println(f"{l + r}");
}
"#,
                ),
                ("diamond_base.hew", "pub fn base_value() -> int { 100 }\n"),
                (
                    "diamond_left.hew",
                    "import \"diamond_base.hew\";\npub fn left_value() -> int { base_value() + 1 }\n",
                ),
                (
                    "diamond_right.hew",
                    "import \"diamond_base.hew\";\npub fn right_value() -> int { base_value() + 2 }\n",
                ),
            ],
        );
        let root_path = fixture.join("module_diamond.hew");
        let root_label = root_path.display().to_string();
        let root_source = fs::read_to_string(&root_path).expect("root fixture should be readable");
        let mut program = parse_source(&root_source, &root_label).expect("fixture should parse");
        let mut ctx = ImportResolutionContext {
            in_progress_imports: HashSet::new(),
            resolved_imports: HashMap::new(),
            manifest_deps: None,
            extra_pkg_path: None,
            locked_versions: None,
            package_name: None,
            project_dir: &fixture.path,
        };

        let module_graph = build_module_graph(
            &root_path,
            &mut program.items,
            program.module_doc.clone(),
            &mut ctx,
        )
        .expect("diamond fixture should build a module graph");
        program.module_graph = Some(module_graph);

        let left_import = find_file_import(&program.items, "diamond_left.hew");
        let left_items = left_import
            .resolved_items
            .as_ref()
            .expect("left import should be resolved");
        assert!(
            find_file_import(left_items, "diamond_base.hew")
                .resolved_items
                .is_some(),
            "left branch should reuse resolved diamond_base import items"
        );

        let right_import = find_file_import(&program.items, "diamond_right.hew");
        let right_items = right_import
            .resolved_items
            .as_ref()
            .expect("right import should be resolved");
        assert!(
            find_file_import(right_items, "diamond_base.hew")
                .resolved_items
                .is_some(),
            "right branch should reuse resolved diamond_base import items"
        );

        let mut checker = hew_types::Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);
        let unresolved: Vec<_> = output
            .errors
            .iter()
            .filter(|error| error.kind == TypeErrorKind::UnresolvedImport)
            .collect();
        assert!(
            unresolved.is_empty(),
            "diamond fixture should not produce unresolved import errors: {unresolved:?}"
        );
        assert!(
            output.errors.is_empty(),
            "diamond fixture should fully type-check: {:?}",
            output.errors
        );
    }

    #[test]
    fn build_module_graph_keeps_transitive_file_imports_visible_inside_imported_modules() {
        let fixture = TestFixtureDir::new(
            "transitive-file-import-cache",
            &[
                (
                    "module_transitive.hew",
                    r#"import "trans_mid.hew";

fn main() {
    let x = mid_fn();
    println(x);
}
"#,
                ),
                (
                    "trans_mid.hew",
                    "import \"trans_base.hew\";\npub fn mid_fn() -> int { base_fn() + 1 }\n",
                ),
                ("trans_base.hew", "pub fn base_fn() -> int { 42 }\n"),
            ],
        );
        let root_path = fixture.join("module_transitive.hew");
        let root_label = root_path.display().to_string();
        let root_source = fs::read_to_string(&root_path).expect("root fixture should be readable");
        let mut program = parse_source(&root_source, &root_label).expect("fixture should parse");
        let mut ctx = ImportResolutionContext {
            in_progress_imports: HashSet::new(),
            resolved_imports: HashMap::new(),
            manifest_deps: None,
            extra_pkg_path: None,
            locked_versions: None,
            package_name: None,
            project_dir: &fixture.path,
        };

        let module_graph = build_module_graph(
            &root_path,
            &mut program.items,
            program.module_doc.clone(),
            &mut ctx,
        )
        .expect("transitive fixture should build a module graph");
        program.module_graph = Some(module_graph);

        let mut checker = hew_types::Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);
        assert!(
            output.errors.is_empty(),
            "transitive fixture should fully type-check: {:?}",
            output.errors
        );
    }

    #[test]
    #[expect(
        clippy::too_many_lines,
        reason = "fixture-based regression exercises the full import-enrichment sync path"
    )]
    fn enrich_program_ast_keeps_flattened_imports_out_of_root_module_graph_node() {
        let fixture = TestFixtureDir::new(
            "root-module-sync-skips-flattened-imports",
            &[
                (
                    "main.hew",
                    r#"import "dep.hew";

fn main() {
    println(dep_value());
}
"#,
                ),
                (
                    "dep.hew",
                    r"pub fn dep_value() -> int { helper() }

fn helper() -> int { 41 }
",
                ),
            ],
        );
        let root_path = fixture.join("main.hew");
        let root_label = root_path.display().to_string();
        let root_source = fs::read_to_string(&root_path).expect("root fixture should be readable");
        let mut program = parse_source(&root_source, &root_label).expect("fixture should parse");
        let mut ctx = ImportResolutionContext {
            in_progress_imports: HashSet::new(),
            resolved_imports: HashMap::new(),
            manifest_deps: None,
            extra_pkg_path: None,
            locked_versions: None,
            package_name: None,
            project_dir: &fixture.path,
        };

        let module_graph = build_module_graph(
            &root_path,
            &mut program.items,
            program.module_doc.clone(),
            &mut ctx,
        )
        .expect("fixture should build a module graph");
        let root_item_count = module_graph
            .modules
            .get(&module_graph.root)
            .expect("root module should exist")
            .items
            .len();
        program.module_graph = Some(module_graph);

        let mut checker = hew_types::Checker::new(ModuleRegistry::new(vec![]));
        let tco = checker.check_program(&program);
        assert!(
            tco.errors.is_empty(),
            "fixture should type-check before enrichment: {:?}",
            tco.errors
        );
        let module_registry = checker.into_module_registry();
        enrich_program_ast(
            &mut program,
            Some(&tco),
            &module_registry,
            &root_source,
            &root_label,
        )
        .expect("enrichment should succeed");

        let module_graph = program
            .module_graph
            .as_ref()
            .expect("program should keep module graph after enrichment");
        let root_module = module_graph
            .modules
            .get(&module_graph.root)
            .expect("root module should still exist");
        assert_eq!(
            root_module.items.len(),
            root_item_count,
            "root module graph node should keep only its original items"
        );

        let root_fn_names: Vec<_> = root_module
            .items
            .iter()
            .filter_map(|(item, _)| match item {
                Item::Function(function) => Some(function.name.as_str()),
                _ => None,
            })
            .collect();
        assert!(
            root_fn_names.contains(&"main"),
            "root module should keep its own main function"
        );
        assert!(
            !root_fn_names.contains(&"dep_value") && !root_fn_names.contains(&"helper"),
            "flattened imported functions should not be synced back into the root module"
        );

        let dep_module = module_graph
            .modules
            .values()
            .find(|module| {
                module.items.iter().any(|(item, _)| {
                    matches!(
                        item,
                        Item::Function(function)
                            if function.name == "dep_value" || function.name == "helper"
                    )
                })
            })
            .expect("dependency module should keep its own functions");
        let dep_fn_names: Vec<_> = dep_module
            .items
            .iter()
            .filter_map(|(item, _)| match item {
                Item::Function(function) => Some(function.name.as_str()),
                _ => None,
            })
            .collect();
        assert!(
            dep_fn_names.contains(&"dep_value") && dep_fn_names.contains(&"helper"),
            "dependency module should still own its imported bodies"
        );
    }

    // ── multi-file diagnostic source routing ───────────────────────────────────
    //
    // These tests prove that type errors originating in non-root module bodies
    // carry the correct `source_module` tag AND that the shared diagnostic
    // module-source map correctly maps module names to their source files.

    /// A type error in an imported module body must have `source_module` set to
    /// the imported module's dotted name, not `None`.
    ///
    /// Regression guard for the checker→CLI diagnostic-envelope path surfacing
    /// non-root module body errors with the right origin tag.
    #[test]
    fn non_root_module_body_error_carries_source_module_tag() {
        let fixture = TestFixtureDir::new(
            "non-root-body-diag-source-module",
            &[
                ("main.hew", "import \"dep.hew\";\n\nfn main() {}\n"),
                // dep.hew: fn mistyped() -> i64 { true }  — body returns bool
                ("dep.hew", "pub fn mistyped() -> i64 { true }\n"),
            ],
        );

        let root_path = fixture.join("main.hew");
        let root_label = root_path.display().to_string();
        let root_source = fs::read_to_string(&root_path).expect("root fixture must be readable");

        let mut program = parse_source(&root_source, &root_label).expect("fixture must parse");
        let mut ctx = ImportResolutionContext {
            in_progress_imports: HashSet::new(),
            resolved_imports: HashMap::new(),
            manifest_deps: None,
            extra_pkg_path: None,
            locked_versions: None,
            package_name: None,
            project_dir: &fixture.path,
        };
        let module_graph = build_module_graph(
            &root_path,
            &mut program.items,
            program.module_doc.clone(),
            &mut ctx,
        )
        .expect("fixture must build a module graph");
        program.module_graph = Some(module_graph);

        let mut checker = hew_types::Checker::new(ModuleRegistry::new(vec![]));
        let tco = checker.check_program(&program);

        assert!(
            !tco.errors.is_empty(),
            "expected type errors from non-root module body; got none"
        );
        // Every error must be tagged with the dep module name, not root.
        // Module name is derived from the file path: "dep.hew" → "dep".
        for err in &tco.errors {
            assert!(
                err.source_module.is_some(),
                "error from non-root module body must carry source_module; got None for: {err:?}"
            );
            let mod_name = err.source_module.as_deref().unwrap();
            assert!(
                mod_name.contains("dep"),
                "error source_module should contain 'dep'; got {mod_name:?} for: {err:?}"
            );
        }
    }

    /// `diagnostic::build_module_source_map` must populate an entry for each non-root
    /// module that has a source file on disk.
    #[test]
    fn build_module_source_map_populates_non_root_entries() {
        let fixture = TestFixtureDir::new(
            "source-map-populate",
            &[
                ("main.hew", "import \"util.hew\";\nfn main() {}\n"),
                ("util.hew", "pub fn helper() -> i64 { 1 }\n"),
            ],
        );

        let root_path = fixture.join("main.hew");
        let root_label = root_path.display().to_string();
        let root_source = fs::read_to_string(&root_path).expect("root fixture must be readable");

        let mut program = parse_source(&root_source, &root_label).expect("fixture must parse");
        let mut ctx = ImportResolutionContext {
            in_progress_imports: HashSet::new(),
            resolved_imports: HashMap::new(),
            manifest_deps: None,
            extra_pkg_path: None,
            locked_versions: None,
            package_name: None,
            project_dir: &fixture.path,
        };
        let module_graph = build_module_graph(
            &root_path,
            &mut program.items,
            program.module_doc.clone(),
            &mut ctx,
        )
        .expect("fixture must build a module graph");
        program.module_graph = Some(module_graph);

        let source_map = crate::diagnostic::build_module_source_map(&program);

        assert!(
            !source_map.is_empty(),
            "source map must contain at least the util module entry"
        );
        let has_util = source_map.keys().any(|k| k.contains("util"));
        assert!(
            has_util,
            "source map must contain an entry for the 'util' module; keys: {:?}",
            source_map.keys().collect::<Vec<_>>()
        );

        let (util_key, (util_src, _)) = source_map
            .iter()
            .find(|(k, _)| k.contains("util"))
            .expect("util entry must exist");
        assert!(
            util_src.contains("helper"),
            "source map entry for {util_key} must contain 'helper'; got: {util_src:?}"
        );
    }

    /// Root module errors must still have `source_module = None` even in a
    /// multi-file program.
    #[test]
    fn root_module_error_source_module_is_none_in_multi_file_program() {
        let fixture = TestFixtureDir::new(
            "root-error-no-source-module",
            &[
                // Root has the type error; dep is fine.
                ("main.hew", "import \"ok.hew\";\nfn bad() -> i64 { true }\n"),
                ("ok.hew", "pub fn fine() -> i64 { 1 }\n"),
            ],
        );

        let root_path = fixture.join("main.hew");
        let root_label = root_path.display().to_string();
        let root_source = fs::read_to_string(&root_path).expect("root fixture must be readable");

        let mut program = parse_source(&root_source, &root_label).expect("fixture must parse");
        let mut ctx = ImportResolutionContext {
            in_progress_imports: HashSet::new(),
            resolved_imports: HashMap::new(),
            manifest_deps: None,
            extra_pkg_path: None,
            locked_versions: None,
            package_name: None,
            project_dir: &fixture.path,
        };
        let module_graph = build_module_graph(
            &root_path,
            &mut program.items,
            program.module_doc.clone(),
            &mut ctx,
        )
        .expect("fixture must build a module graph");
        program.module_graph = Some(module_graph);

        let mut checker = hew_types::Checker::new(ModuleRegistry::new(vec![]));
        let tco = checker.check_program(&program);

        assert!(
            !tco.errors.is_empty(),
            "expected type errors from root module; got none"
        );
        for err in &tco.errors {
            assert!(
                err.source_module.is_none(),
                "root module error must have source_module = None; got {:?}",
                err.source_module
            );
        }
    }

    /// Registration-phase errors (e.g. duplicate function definitions) emitted
    /// by `collect_functions` for a non-root module must be tagged with that
    /// module's name, not left as `None`.
    #[test]
    fn registration_phase_error_tagged_with_non_root_module() {
        let fixture = TestFixtureDir::new(
            "reg-phase-source-module",
            &[
                ("main.hew", "import \"dep.hew\";\nfn main() {}\n"),
                // dep.hew has two functions with the same name — duplicate_definition
                // error emitted during collect_functions.
                (
                    "dep.hew",
                    "pub fn dup() -> i64 { 1 }\npub fn dup() -> i64 { 2 }\n",
                ),
            ],
        );

        let root_path = fixture.join("main.hew");
        let root_label = root_path.display().to_string();
        let root_source = fs::read_to_string(&root_path).expect("root fixture must be readable");

        let mut program = parse_source(&root_source, &root_label).expect("fixture must parse");
        let mut ctx = ImportResolutionContext {
            in_progress_imports: HashSet::new(),
            resolved_imports: HashMap::new(),
            manifest_deps: None,
            extra_pkg_path: None,
            locked_versions: None,
            package_name: None,
            project_dir: &fixture.path,
        };
        let module_graph = build_module_graph(
            &root_path,
            &mut program.items,
            program.module_doc.clone(),
            &mut ctx,
        )
        .expect("fixture must build a module graph");
        program.module_graph = Some(module_graph);

        let mut checker = hew_types::Checker::new(ModuleRegistry::new(vec![]));
        let tco = checker.check_program(&program);

        assert!(
            !tco.errors.is_empty(),
            "expected duplicate-definition error from dep module; got none"
        );
        let dep_errors: Vec<_> = tco
            .errors
            .iter()
            .filter(|e| e.source_module.as_deref() == Some("dep"))
            .collect();
        assert!(
            !dep_errors.is_empty(),
            "registration-phase error from dep.hew must have source_module = Some(\"dep\"); \
             errors: {:?}",
            tco.errors
                .iter()
                .map(|e| &e.source_module)
                .collect::<Vec<_>>()
        );
    }
}
