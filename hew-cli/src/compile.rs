//! Build command: parse, type-check, serialize to `MessagePack`, invoke
//! `hew-codegen`, and link the final executable.

use std::collections::HashSet;
use std::io::Write;
use std::path::{Path, PathBuf};

use hew_parser::ast::{ImportDecl, Item, Spanned};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum CodegenMode {
    #[default]
    LinkExecutable,
    EmitAst,
    EmitJson,
    EmitMlir,
    EmitLlvm,
    EmitObj,
}

impl CodegenMode {
    fn codegen_flag(self) -> Option<&'static str> {
        match self {
            Self::LinkExecutable | Self::EmitAst | Self::EmitJson => None,
            Self::EmitMlir => Some("--emit-mlir"),
            Self::EmitLlvm => Some("--emit-llvm"),
            Self::EmitObj => Some("--emit-obj"),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct CompileOptions {
    pub werror: bool,
    pub no_typecheck: bool,
    pub codegen_mode: CodegenMode,
    pub target: Option<String>,
    /// Build with debug info (no optimizations, no stripping).
    pub debug: bool,
    /// Override the package search directory (default: `.adze/packages/`).
    pub pkg_path: Option<PathBuf>,
    /// Extra static libraries to pass to the linker (via `--link-lib`).
    pub extra_link_libs: Vec<String>,
}

/// Run the full compilation pipeline for a `.hew` source file.
///
/// When `check_only` is `true` the pipeline stops after type-checking and no
/// binary is produced.
///
/// # Errors
///
/// Returns a human-readable message when any pipeline stage fails.
#[expect(
    clippy::too_many_lines,
    reason = "compilation pipeline has many sequential stages"
)]
pub fn compile(
    input: &str,
    output: Option<&str>,
    check_only: bool,
    options: &CompileOptions,
) -> Result<String, String> {
    let source =
        std::fs::read_to_string(input).map_err(|e| format!("Error: cannot read {input}: {e}"))?;

    // Detect hew.toml manifest (script mode if absent).
    let project_dir = Path::new(input).parent().unwrap_or(Path::new("."));
    let manifest_deps = super::manifest::load_dependencies(project_dir);
    let package_name = super::manifest::load_package_name(project_dir);

    // 1. Parse
    let result = hew_parser::parse(&source);
    if !result.errors.is_empty() {
        for err in &result.errors {
            let hints: Vec<String> = err.hint.iter().cloned().collect();
            match err.severity {
                hew_parser::Severity::Warning => super::diagnostic::render_warning(
                    &source,
                    input,
                    &err.span,
                    &err.message,
                    &[],
                    &hints,
                ),
                hew_parser::Severity::Error => super::diagnostic::render_diagnostic(
                    &source,
                    input,
                    &err.span,
                    &err.message,
                    &[],
                    &hints,
                ),
            }
        }
        if result
            .errors
            .iter()
            .any(|e| e.severity == hew_parser::Severity::Error)
        {
            return Err("parsing failed".into());
        }
    }

    let mut program = result.program;

    // 2. Validate manifest imports then resolve file-path imports
    if let Some(deps) = &manifest_deps {
        let errs = validate_imports_against_manifest(&program.items, deps, package_name.as_deref());
        if !errs.is_empty() {
            for e in &errs {
                eprintln!("{e}");
            }
            return Err("undeclared dependencies".into());
        }
    }

    let locked_versions = super::manifest::load_lockfile(project_dir);

    // Inject synthetic imports for features that implicitly depend on stdlib
    // modules (wire types, regex literals, core modules like log). Must happen
    // BEFORE resolve_file_imports so the imports get their resolved_items populated.
    inject_implicit_imports(&mut program.items, &source);

    let input_path = Path::new(input);
    let module_graph = build_module_graph(
        input_path,
        &mut program.items,
        program.module_doc.clone(),
        manifest_deps.as_deref(),
        options.pkg_path.as_deref(),
        locked_versions.as_deref(),
        package_name.as_deref(),
        project_dir,
    )
    .map_err(|errs| errs.join("\n"))?;
    program.module_graph = Some(module_graph);

    // Collect module-path imports for per-package staticlib linking.
    // Walk the entire module graph (not just root imports) so that
    // transitive dependencies like std::bench → std::time::datetime
    // are also linked.
    let mut imported_modules: Vec<String> = program
        .module_graph
        .as_ref()
        .map(|mg| {
            mg.modules
                .keys()
                .filter(|id| !id.path.is_empty())
                .map(|id| id.path.join("::"))
                .collect()
        })
        .unwrap_or_default();
    // Wire types always generate JSON encode/decode calls, so link the
    // JSON and YAML staticlibs even without an explicit import.
    if program.items.iter().any(|(item, _)| {
        matches!(item, Item::Wire(_)) || matches!(item, Item::TypeDecl(td) if td.wire.is_some())
    }) {
        for m in ["std::encoding::json", "std::encoding::yaml"] {
            if !imported_modules.contains(&m.to_string()) {
                imported_modules.push(m.to_string());
            }
        }
    }
    let mut extra_libs = super::link::find_package_libs(&imported_modules);
    extra_libs.extend(options.extra_link_libs.iter().cloned());

    // 3. Type-check
    let tco = if options.no_typecheck {
        None
    } else {
        let mut checker = hew_types::Checker::new();
        if options
            .target
            .as_deref()
            .is_some_and(|t| t.starts_with("wasm32"))
        {
            checker.enable_wasm_target();
        }
        let tco = checker.check_program(&program);
        let has_errors = !tco.errors.is_empty();

        for err in &tco.errors {
            let notes: Vec<super::diagnostic::DiagnosticNote<'_>> = err
                .notes
                .iter()
                .map(|(span, msg)| super::diagnostic::DiagnosticNote {
                    span,
                    message: msg.as_str(),
                })
                .collect();
            super::diagnostic::render_diagnostic(
                &source,
                input,
                &err.span,
                &err.message,
                &notes,
                &err.suggestions,
            );
        }

        // Render warnings (these don't block compilation).
        for warn in &tco.warnings {
            let notes: Vec<super::diagnostic::DiagnosticNote<'_>> = warn
                .notes
                .iter()
                .map(|(span, msg)| super::diagnostic::DiagnosticNote {
                    span,
                    message: msg.as_str(),
                })
                .collect();
            super::diagnostic::render_warning(
                &source,
                input,
                &warn.span,
                &warn.message,
                &notes,
                &warn.suggestions,
            );
        }

        if check_only {
            return if has_errors {
                Err("type errors found".into())
            } else {
                Ok(String::new())
            };
        }

        if has_errors {
            if options.werror {
                eprintln!("--Werror is accepted for compatibility; type errors are already fatal");
            }
            return Err("type errors found".into());
        }

        Some(tco)
    };

    if check_only {
        return Ok(String::new());
    }

    // 4. Flatten resolved_items from module imports into top-level items,
    // then enrich AST with inferred types and serialize to MessagePack.
    // Flattening must happen before enrichment so that normalize_all_types
    // and enrich_fn_decl process the imported functions too.
    flatten_import_items(&mut program);

    let expr_type_map = if let Some(tco) = &tco {
        hew_serialize::enrich_program(&mut program, tco);
        // Sync enriched items back to module graph root so C++ codegen uses
        // enriched (type-annotated) items rather than the pre-enrichment clone.
        if let Some(ref mut mg) = program.module_graph {
            if let Some(root_module) = mg.modules.get_mut(&mg.root) {
                root_module.items.clone_from(&program.items);
            }
            // Normalize types and rewrite builtin calls in non-root modules
            // so that TypeExpr::Named("Option", ..) → TypeExpr::Option(..)
            // and len(x) → x.len() etc.
            for (id, module) in &mut mg.modules {
                if *id != mg.root {
                    hew_serialize::normalize_items_types(&mut module.items);
                    hew_serialize::rewrite_builtin_calls(&mut module.items);
                }
            }
        }
        hew_serialize::build_expr_type_map(tco)
    } else {
        Vec::new()
    };

    // 4b. Mark tail calls (purely syntactic, must run after enrichment so that
    //     MethodCall→Call rewrites are already in place)
    hew_parser::tail_call::mark_tail_calls(&mut program);

    // 4c. If --emit-ast, dump enriched AST as JSON and return
    if options.codegen_mode == CodegenMode::EmitAst {
        let json = serde_json::to_string_pretty(&program)
            .map_err(|e| format!("Error: cannot serialize AST: {e}"))?;
        println!("{json}");
        return Ok(String::new());
    }

    // Build handle type metadata for C++ codegen (replaces hardcoded type lists)
    let handle_types = hew_types::stdlib::all_handle_types();
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

    // 4d. If --emit-json, dump the full TypedProgram (same as what codegen
    // receives via msgpack) as pretty-printed JSON and return.
    if options.codegen_mode == CodegenMode::EmitJson {
        let json = hew_serialize::serialize_to_json(
            &program,
            expr_type_map,
            handle_types,
            handle_type_repr,
        );
        println!("{json}");
        return Ok(String::new());
    }

    let ast_data = hew_serialize::serialize_to_msgpack(
        &program,
        expr_type_map,
        handle_types,
        handle_type_repr,
    );

    // 5. Invoke hew-codegen
    let codegen_bin = find_codegen_binary()?;
    if let Some(codegen_flag) = options.codegen_mode.codegen_flag() {
        let mut cmd = std::process::Command::new(&codegen_bin);
        cmd.arg(codegen_flag)
            .stdin(std::process::Stdio::piped())
            .stderr(std::process::Stdio::inherit());
        if let Some(target) = &options.target {
            cmd.arg(format!("--target={target}"));
        }
        if options.debug {
            cmd.arg("--debug");
        }
        if let Some(output_path) = output {
            cmd.arg("-o").arg(output_path);
            cmd.stdout(std::process::Stdio::null());
        } else {
            cmd.stdout(std::process::Stdio::inherit());
        }

        let mut child = cmd
            .spawn()
            .map_err(|e| format!("Error: cannot start hew-codegen: {e}"))?;

        child
            .stdin
            .take()
            .expect("stdin should be piped")
            .write_all(&ast_data)
            .map_err(|e| format!("Error: cannot write to hew-codegen: {e}"))?;

        let status = child
            .wait()
            .map_err(|e| format!("Error: hew-codegen failed: {e}"))?;
        if !status.success() {
            return Err("codegen failed".into());
        }

        return Ok(output.unwrap_or("").to_string());
    }

    let obj_temp = tempfile::Builder::new()
        .prefix("hew_")
        .suffix(".o")
        .tempfile()
        .map_err(|e| format!("Error: cannot create temp file: {e}"))?
        .into_temp_path();
    let obj_path = obj_temp.display().to_string();

    let mut cmd = std::process::Command::new(&codegen_bin);
    cmd.arg("--emit-obj")
        .arg("-o")
        .arg(&obj_path)
        .stdin(std::process::Stdio::piped())
        .stderr(std::process::Stdio::inherit());
    if let Some(target) = &options.target {
        cmd.arg(format!("--target={target}"));
    }
    if options.debug {
        cmd.arg("--debug");
    }
    let mut child = cmd
        .spawn()
        .map_err(|e| format!("Error: cannot start hew-codegen: {e}"))?;

    child
        .stdin
        .take()
        .expect("stdin should be piped")
        .write_all(&ast_data)
        .map_err(|e| format!("Error: cannot write to hew-codegen: {e}"))?;

    let status = child
        .wait()
        .map_err(|e| format!("Error: hew-codegen failed: {e}"))?;
    if !status.success() {
        return Err("codegen failed".into());
    }

    // 6. Link
    let default_output = Path::new(input)
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("a.out")
        .to_string();
    // Windows executables need .exe when compiling for native host
    #[cfg(target_os = "windows")]
    let default_output = if options.target.is_none() && !default_output.ends_with(".exe") {
        format!("{default_output}.exe")
    } else {
        default_output
    };
    let output_path = output.unwrap_or(&default_output);
    super::link::link_executable(
        &obj_path,
        output_path,
        options.target.as_deref(),
        options.debug,
        &extra_libs,
    )?;

    // obj_temp (TempPath) auto-deletes on drop

    Ok(output_path.to_string())
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

pub(crate) fn find_codegen_binary() -> Result<String, String> {
    // Allow explicit override via environment variable
    if let Ok(path) = std::env::var("HEW_CODEGEN") {
        let p = std::path::PathBuf::from(&path);
        if p.exists() {
            return Ok(path);
        }
        return Err(format!("Error: HEW_CODEGEN={path} does not exist"));
    }

    let exe = std::env::current_exe().map_err(|e| format!("cannot find self: {e}"))?;
    let exe_dir = exe.parent().expect("exe should have a parent directory");

    let codegen_name = if cfg!(target_os = "windows") {
        "hew-codegen.exe"
    } else {
        "hew-codegen"
    };
    let candidates = [
        // Same directory as the hew binary (installed layout)
        exe_dir.join(codegen_name),
        // Installed layout: <prefix>/lib/hew-codegen
        exe_dir.join(format!("../lib/{codegen_name}")),
        // Dev layout from target/debug/ or target/release/
        exe_dir.join(format!("../../hew-codegen/build/src/{codegen_name}")),
        // Dev layout from target/debug/deps/ (cargo test)
        exe_dir.join(format!("../../../hew-codegen/build/src/{codegen_name}")),
        // Windows MSVC: CMake puts binaries in Release/ or Debug/ subdirs
        exe_dir.join(format!(
            "../../hew-codegen/build/src/Release/{codegen_name}"
        )),
        exe_dir.join(format!("../../hew-codegen/build/src/Debug/{codegen_name}")),
        exe_dir.join(format!(
            "../../../hew-codegen/build/src/Release/{codegen_name}"
        )),
        exe_dir.join(format!(
            "../../../hew-codegen/build/src/Debug/{codegen_name}"
        )),
        // Sanitizer build
        exe_dir.join(format!(
            "../../hew-codegen/build-sanitizer/src/{codegen_name}"
        )),
    ];

    for c in &candidates {
        if c.exists() {
            return Ok(c
                .canonicalize()
                .unwrap_or_else(|_| c.clone())
                .display()
                .to_string());
        }
    }

    // Try PATH
    if std::process::Command::new("hew-codegen")
        .arg("--version")
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status()
        .is_ok()
    {
        return Ok("hew-codegen".into());
    }

    Err(
        "Error: cannot find hew-codegen binary. Build with: cd hew-codegen/build && cmake --build ."
            .into(),
    )
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
/// on each `ImportDecl` remain populated so that `flatten_import_items` can
/// continue to work as a temporary compatibility shim.
#[expect(
    clippy::too_many_arguments,
    reason = "module graph construction needs all context"
)]
fn build_module_graph(
    source_file: &Path,
    items: &mut Vec<Spanned<Item>>,
    module_doc: Option<String>,
    manifest_deps: Option<&[String]>,
    extra_pkg_path: Option<&Path>,
    locked_versions: Option<&[(String, String)]>,
    package_name: Option<&str>,
    project_dir: &Path,
) -> Result<hew_parser::module::ModuleGraph, Vec<String>> {
    use hew_parser::module::{Module, ModuleGraph, ModuleId};

    let input_canonical =
        std::fs::canonicalize(source_file).unwrap_or_else(|_| source_file.to_path_buf());
    let source_dir = input_canonical.parent().unwrap_or(Path::new("."));

    // Phase 1: resolve imports (populates resolved_items for flatten compat).
    let mut imported = HashSet::new();
    imported.insert(input_canonical.clone());
    resolve_file_imports(
        &input_canonical,
        items,
        &mut imported,
        manifest_deps,
        extra_pkg_path,
        locked_versions,
        package_name,
        project_dir,
    );

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
fn flatten_import_items(program: &mut hew_parser::ast::Program) {
    let mut extra: Vec<Spanned<Item>> = Vec::new();
    for (item, _span) in &mut program.items {
        if let Item::Import(decl) = item {
            if let Some(resolved) = decl.resolved_items.take() {
                for resolved_item in resolved {
                    // Extract all non-import items so the C++ codegen sees them.
                    // Import items from the resolved file are not re-exported.
                    if !matches!(&resolved_item.0, Item::Import(_)) {
                        extra.push(resolved_item);
                    }
                }
            }
        }
    }
    program.items.extend(extra);
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
#[expect(
    clippy::too_many_arguments,
    reason = "import resolution needs all context"
)]
fn resolve_file_imports(
    source_file: &Path,
    items: &mut Vec<Spanned<Item>>,
    imported: &mut HashSet<PathBuf>,
    manifest_deps: Option<&[String]>,
    extra_pkg_path: Option<&Path>,
    locked_versions: Option<&[(String, String)]>,
    package_name: Option<&str>,
    project_dir: &Path,
) {
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
                    eprintln!(
                        "Error: imported file not found: {file_path} (resolved to {})",
                        resolved.display()
                    );
                    std::process::exit(1);
                }
            }
            Item::Import(decl) if !decl.path.is_empty() => {
                let module_str = decl.path.join("::");
                // Check if this is a local project import (first segment matches package name).
                let is_local =
                    package_name.is_some_and(|pkg| decl.path.first().is_some_and(|seg| seg == pkg));
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
                    candidates.push(project_dir.join("src").join(&local_dir));
                    candidates.push(project_dir.join("src").join(&local_flat));
                    // project root
                    candidates.push(project_dir.join(&local_dir));
                    candidates.push(project_dir.join(&local_flat));
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
                if let Some(version) = locked_versions
                    .and_then(|lv| lv.iter().find(|(n, _)| n == &module_str))
                    .map(|(_, v)| v.as_str())
                {
                    let module_dir: PathBuf = decl.path.iter().collect();
                    let entry_file =
                        format!("{}.hew", decl.path.last().expect("path is non-empty"));
                    let versioned_rel = module_dir.join(version).join(entry_file);
                    candidates.push(cwd.join(".adze/packages").join(&versioned_rel));
                    if let Some(pkg) = extra_pkg_path {
                        candidates.push(pkg.join(&versioned_rel));
                    }
                }
                // Unversioned package paths (fallback)
                candidates.push(cwd.join(".adze/packages").join(&rel_path));
                // Custom package path (--pkg-path flag)
                if let Some(pkg) = extra_pkg_path {
                    candidates.push(pkg.join(&rel_path));
                }
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
                    let hint = if manifest_deps.is_some_and(|d| d.contains(&module_str)) {
                        "\n  hint: this dependency is declared in hew.toml — run `adze install`"
                    } else if manifest_deps.is_some() {
                        "\n  hint: add this module to [dependencies] in hew.toml"
                    } else {
                        ""
                    };
                    eprintln!("Error: module `{module_str}` not found (tried: {tried}){hint}");
                    std::process::exit(1);
                }
            }
            _ => continue,
        };

        if imported.contains(&canonical) {
            continue;
        }
        imported.insert(canonical.clone());

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
                .filter_map(|e| e.ok())
                .map(|e| e.path())
                .filter(|p| {
                    p.extension().and_then(|e| e.to_str()) == Some("hew") && *p != canonical
                })
                .collect();
            peers.sort(); // deterministic order
            peers
        } else {
            Vec::new()
        };

        let mut import_items = parse_and_resolve_file(
            &canonical,
            imported,
            manifest_deps,
            extra_pkg_path,
            locked_versions,
            package_name,
            project_dir,
        );

        // Parse and merge peer files for directory modules.
        for peer in &peer_files {
            let peer_canonical = peer.canonicalize().unwrap_or_else(|_| peer.clone());
            if imported.contains(&peer_canonical) {
                continue;
            }
            imported.insert(peer_canonical.clone());

            let mut peer_items = parse_and_resolve_file(
                &peer_canonical,
                imported,
                manifest_deps,
                extra_pkg_path,
                locked_versions,
                package_name,
                project_dir,
            );
            import_items.append(&mut peer_items);
        }

        // Check for duplicate pub names in multi-file modules.
        if !peer_files.is_empty() {
            if let Item::Import(decl) = &items[*idx].0 {
                let module_str = if !decl.path.is_empty() {
                    decl.path.join("::")
                } else {
                    canonical.display().to_string()
                };
                check_duplicate_pub_names(&import_items, &module_str);
            }
        }

        // Store resolved items and source paths on the ImportDecl instead of
        // flattening them into the parent's item list. This preserves module
        // boundaries so the type checker can register items under the module's namespace.
        if let Item::Import(decl) = &mut items[*idx].0 {
            decl.resolved_items = Some(import_items);
            let mut source_paths = vec![canonical.clone()];
            for peer in &peer_files {
                source_paths.push(peer.canonicalize().unwrap_or_else(|_| peer.clone()));
            }
            decl.resolved_source_paths = source_paths;
        }
    }
}

/// Parse a single `.hew` file, report diagnostics, and recursively resolve its imports.
fn parse_and_resolve_file(
    canonical: &Path,
    imported: &mut HashSet<PathBuf>,
    manifest_deps: Option<&[String]>,
    extra_pkg_path: Option<&Path>,
    locked_versions: Option<&[(String, String)]>,
    package_name: Option<&str>,
    project_dir: &Path,
) -> Vec<Spanned<Item>> {
    let source = match std::fs::read_to_string(canonical) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading imported file '{}': {e}", canonical.display());
            std::process::exit(1);
        }
    };

    let result = hew_parser::parse(&source);
    if !result.errors.is_empty() {
        let display_path = canonical.display().to_string();
        for err in &result.errors {
            let hints: Vec<String> = err.hint.iter().cloned().collect();
            match err.severity {
                hew_parser::Severity::Warning => {
                    super::diagnostic::render_warning(
                        &source,
                        &display_path,
                        &err.span,
                        &err.message,
                        &[],
                        &hints,
                    );
                }
                hew_parser::Severity::Error => {
                    super::diagnostic::render_diagnostic(
                        &source,
                        &display_path,
                        &err.span,
                        &err.message,
                        &[],
                        &hints,
                    );
                }
            }
        }
        if result
            .errors
            .iter()
            .any(|e| e.severity == hew_parser::Severity::Error)
        {
            std::process::exit(1);
        }
    }

    let mut import_items = result.program.items;
    resolve_file_imports(
        canonical,
        &mut import_items,
        imported,
        manifest_deps,
        extra_pkg_path,
        locked_versions,
        package_name,
        project_dir,
    );

    import_items
}

/// Check for duplicate `pub` item names across files in a multi-file module.
fn check_duplicate_pub_names(items: &[Spanned<Item>], module_name: &str) {
    use hew_parser::ast::Visibility;
    use std::collections::HashMap;

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
                eprintln!("Error: duplicate pub name `{name}` in module {module_name}");
                std::process::exit(1);
            }
        }
    }
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

    // Detect wire types that need JSON or YAML support.
    // Wire codegen always generates JSON encode/decode functions that call
    // hew_json_* symbols.  Don't add these as import items (which would pull
    // the whole stdlib module into the AST), just ensure the static libs
    // are linked by appending to imported_modules below.

    // Detect regex literals via source text. The `re"` prefix is the regex
    // literal syntax and is unambiguous; a false positive (e.g. inside a
    // comment) only adds an unused import — harmless.
    if source.contains("re\"") {
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
                    resolved_source_paths: Vec::new(),
                }),
                0..0,
            ));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_module_import(path: Vec<&str>) -> Spanned<Item> {
        let decl = hew_parser::ast::ImportDecl {
            path: path.iter().map(ToString::to_string).collect(),
            spec: None,
            file_path: None,
            resolved_items: None,
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
            resolved_source_paths: Vec::new(),
        };
        (Item::Import(decl), 0..0)
    }

    #[test]
    fn validate_no_manifest_allows_all() {
        // When manifest exists but has no deps, undeclared imports are flagged.
        let items = vec![make_module_import(vec!["mylib", "utils"])];
        let errs = validate_imports_against_manifest(&items, &[], None);
        assert_eq!(errs.len(), 1, "undeclared import should produce an error");
    }

    #[test]
    fn validate_declared_dep_is_ok() {
        let items = vec![make_module_import(vec!["mylib", "utils"])];
        let deps = vec!["mylib::utils".to_string()];
        let errs = validate_imports_against_manifest(&items, &deps, None);
        assert!(errs.is_empty());
    }

    #[test]
    fn validate_undeclared_dep_errors() {
        let items = vec![make_module_import(vec!["mylib", "utils"])];
        let deps: Vec<String> = vec!["mylib::other".to_string()];
        let errs = validate_imports_against_manifest(&items, &deps, None);
        assert_eq!(errs.len(), 1);
        assert!(errs[0].contains("mylib::utils"));
        assert!(errs[0].contains("adze add"));
    }

    #[test]
    fn validate_stdlib_import_is_always_ok() {
        // std::fs is a known stdlib module
        let items = vec![make_module_import(vec!["std", "fs"])];
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
            make_module_import(vec!["mylib", "a"]),
            make_module_import(vec!["mylib", "b"]),
            make_module_import(vec!["mylib", "c"]),
        ];
        let deps = vec!["mylib::a".to_string()];
        let errs = validate_imports_against_manifest(&items, &deps, None);
        assert_eq!(errs.len(), 2);
    }

    #[test]
    fn validate_local_import_is_exempt() {
        // Imports matching the package name are local and skip manifest validation.
        let items = vec![make_module_import(vec!["myapp", "models"])];
        let errs = validate_imports_against_manifest(&items, &[], Some("myapp"));
        assert!(
            errs.is_empty(),
            "local imports should be exempt from manifest validation"
        );
    }
}
