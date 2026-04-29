//! Build command: parse, type-check, serialize to `MessagePack`, invoke the
//! embedded MLIR/LLVM backend, and link the final executable.

use std::fmt;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};

#[cfg(test)]
use std::collections::{HashMap, HashSet};
#[cfg(hew_embedded_codegen)]
use std::ffi::{CStr, CString};

use hew_compile::{
    check_file as frontend_check_file, compile_file as frontend_compile_file,
    compile_program_to_msgpack as frontend_compile_program_to_msgpack, FrontendDiagnostic,
    FrontendDiagnosticKind, FrontendOptions,
};

#[cfg(test)]
use hew_compile::{
    build_module_graph as frontend_build_module_graph,
    collect_new_inferred_type_diagnostics as frontend_collect_new_inferred_type_diagnostics,
    enrich_program_ast as frontend_enrich_program_ast,
    inject_implicit_imports as frontend_inject_implicit_imports,
    line_map_from_source as frontend_line_map_from_source, parse_source as frontend_parse_source,
    typecheck_program as frontend_typecheck_program,
    validate_imports_against_manifest as frontend_validate_imports_against_manifest,
};
#[cfg(test)]
use hew_parser::ast::{ImportDecl, Item, Spanned};

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
    pub werror: bool,
    pub codegen_mode: CodegenMode,
    pub target: Option<String>,
    pub extra_libs: Vec<String>,
    /// Build with debug info (no optimizations, no stripping).
    pub debug: bool,
    /// Override the package search directory (default: `.adze/packages/`).
    pub pkg_path: Option<PathBuf>,
    /// Anchor an in-memory compile to a specific project directory so that
    /// manifest-aware import resolution matches `compile_file` behaviour.
    pub project_dir: Option<PathBuf>,
}

fn frontend_options(target: &TargetSpec, options: &CompileOptions) -> FrontendOptions {
    FrontendOptions {
        no_typecheck: options.no_typecheck,
        warnings_as_errors: options.werror,
        enable_wasm_target: target.is_wasm(),
        pkg_path: options.pkg_path.clone(),
        project_dir: options.project_dir.clone(),
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum CompileFromSourceError {
    DiagnosticsRendered,
    Message(String),
}

impl fmt::Display for CompileFromSourceError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::DiagnosticsRendered => f.write_str("diagnostics already rendered"),
            Self::Message(message) => message.fmt(f),
        }
    }
}

fn render_frontend_diagnostics(diagnostics: &[FrontendDiagnostic]) {
    for diagnostic in diagnostics {
        match &diagnostic.kind {
            FrontendDiagnosticKind::Message(message) => {
                crate::diagnostic::emit_plain_diagnostic_line(message);
            }
            FrontendDiagnosticKind::Parse(error) => {
                let suggestions: Vec<String> = error.hint.iter().cloned().collect();
                if let (Some(source), Some(filename)) =
                    (diagnostic.source.as_deref(), diagnostic.filename.as_deref())
                {
                    match error.severity {
                        hew_parser::Severity::Warning => crate::diagnostic::render_warning(
                            source,
                            filename,
                            &error.span,
                            &error.message,
                            &[],
                            &suggestions,
                        ),
                        hew_parser::Severity::Error => crate::diagnostic::render_diagnostic(
                            source,
                            filename,
                            &error.span,
                            &error.message,
                            &[],
                            &suggestions,
                        ),
                    }
                } else {
                    let level = match error.severity {
                        hew_parser::Severity::Warning => "warning",
                        hew_parser::Severity::Error => "error",
                    };
                    crate::diagnostic::emit_plain_diagnostic_line(&format!(
                        "{level}: {}",
                        error.message
                    ));
                }
            }
            FrontendDiagnosticKind::Type(error) => {
                if let (Some(source), Some(filename)) =
                    (diagnostic.source.as_deref(), diagnostic.filename.as_deref())
                {
                    match error.severity {
                        hew_types::error::Severity::Warning => {
                            crate::diagnostic::render_warning_with_raw_notes(
                                source,
                                filename,
                                &error.span,
                                &error.message,
                                &error.notes,
                                &error.suggestions,
                            );
                        }
                        hew_types::error::Severity::Error => {
                            crate::diagnostic::render_diagnostic_with_raw_notes(
                                source,
                                filename,
                                &error.span,
                                &error.message,
                                &error.notes,
                                &error.suggestions,
                            );
                        }
                    }
                } else {
                    let level = match error.severity {
                        hew_types::error::Severity::Warning => "warning",
                        hew_types::error::Severity::Error => "error",
                    };
                    crate::diagnostic::emit_plain_diagnostic_line(&format!(
                        "{level}: {}",
                        error.message
                    ));
                }
            }
            FrontendDiagnosticKind::InferredType { error, fatal } => {
                render_inferred_type_serialization_diagnostic(
                    diagnostic.source.as_deref(),
                    diagnostic.filename.as_deref(),
                    error,
                    *fatal,
                );
            }
        }
    }
}

fn render_inferred_type_serialization_diagnostic(
    source: Option<&str>,
    filename: Option<&str>,
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
        let notes = [crate::diagnostic::DiagnosticNote {
            span,
            message: detail.as_str(),
        }];
        let suggestions = [String::from(
            "extend the Ty -> TypeExpr conversion in hew-serialize/src/enrich.rs or add an explicit type/coercion before codegen",
        )];
        if let (Some(source), Some(filename)) = (source, filename) {
            if fatal {
                crate::diagnostic::render_diagnostic(
                    source,
                    filename,
                    span,
                    headline,
                    &notes,
                    &suggestions,
                );
            } else {
                crate::diagnostic::render_warning(
                    source,
                    filename,
                    span,
                    headline,
                    &notes,
                    &suggestions,
                );
            }
        } else if let Some(filename) = filename {
            crate::diagnostic::emit_plain_diagnostic_line(&format!(
                "{}: cannot serialize inferred type for code generation in {} at {}..{}: {error}",
                if fatal { "error" } else { "warning" },
                filename,
                span.start,
                span.end
            ));
        } else {
            crate::diagnostic::emit_plain_diagnostic_line(&format!(
                "{}: cannot serialize inferred type for code generation in an imported module at {}..{}: {error}",
                if fatal { "error" } else { "warning" },
                span.start,
                span.end
            ));
        }
    } else {
        crate::diagnostic::emit_plain_diagnostic_line(&format!(
            "{}: cannot serialize inferred type for code generation: {error}",
            if fatal { "error" } else { "warning" }
        ));
    }
}

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
    crate::link::link_executable(
        &obj_path,
        output_path,
        target,
        &options.extra_libs,
        options.debug,
    )?;

    Ok(output_path.to_string())
}

fn default_output_name(input: &str, suffix: &str, fallback: &str) -> String {
    let name = Path::new(input)
        .file_stem()
        .and_then(|stem| stem.to_str())
        .unwrap_or(fallback)
        .to_string();
    if !suffix.is_empty() && !name.ends_with(suffix) {
        format!("{name}{suffix}")
    } else {
        name
    }
}

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

pub fn compile(
    input: &str,
    output: Option<&str>,
    check_only: bool,
    options: &CompileOptions,
) -> Result<String, String> {
    let target = TargetSpec::from_requested(options.target.as_deref())?;
    let frontend_options = frontend_options(&target, options);

    if check_only {
        return match frontend_check_file(input, &frontend_options) {
            Ok(result) => {
                render_frontend_diagnostics(&result.diagnostics);
                Ok(String::new())
            }
            Err(failure) => {
                render_frontend_diagnostics(&failure.diagnostics);
                Err(failure.message)
            }
        };
    }

    let frontend = match frontend_compile_file(input, &frontend_options) {
        Ok(frontend) => frontend,
        Err(failure) => {
            render_frontend_diagnostics(&failure.diagnostics);
            return Err(failure.message);
        }
    };
    render_frontend_diagnostics(&frontend.diagnostics);

    if options.codegen_mode == CodegenMode::EmitAst {
        let json = serde_json::to_string_pretty(&frontend.program)
            .map_err(|e| format!("Error: cannot serialize AST: {e}"))?;
        println!("{json}");
        return Ok(String::new());
    }

    if options.codegen_mode == CodegenMode::EmitJson {
        println!("{}", frontend.to_json());
        return Ok(String::new());
    }

    let ast_data = frontend.to_msgpack();

    if options.codegen_mode == CodegenMode::EmitMsgpack {
        return write_output(output, &ast_data);
    }

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

    compile_and_link(&ast_data, input, output, &target, options)
}

/// Compile a parsed program through the frontend pipeline only, yielding the
/// MessagePack-encoded AST bytes ready for the JIT backend.
///
/// Renders frontend diagnostics as a side effect. Returns `Err` on compile
/// failure with the same semantics as [`compile_from_source_checked`].
pub(crate) fn frontend_to_msgpack(
    program: hew_parser::ast::Program,
    source: &str,
    source_label: &str,
    options: &CompileOptions,
) -> Result<Vec<u8>, CompileFromSourceError> {
    let target = TargetSpec::from_requested(options.target.as_deref())
        .map_err(CompileFromSourceError::Message)?;
    let frontend_options = frontend_options(&target, options);

    let frontend =
        match frontend_compile_program_to_msgpack(program, source, source_label, &frontend_options)
        {
            Ok(frontend) => frontend,
            Err(failure) => {
                render_frontend_diagnostics(&failure.diagnostics);
                return Err(if failure.diagnostics.is_empty() {
                    CompileFromSourceError::Message(failure.message)
                } else {
                    CompileFromSourceError::DiagnosticsRendered
                });
            }
        };
    render_frontend_diagnostics(&frontend.diagnostics);
    Ok(frontend.data)
}

pub(crate) fn compile_from_source_checked(
    program: hew_parser::ast::Program,
    source: &str,
    source_label: &str,
    output_path: &str,
    options: &CompileOptions,
) -> Result<(), CompileFromSourceError> {
    let target = TargetSpec::from_requested(options.target.as_deref())
        .map_err(CompileFromSourceError::Message)?;
    let frontend_options = frontend_options(&target, options);

    let frontend =
        match frontend_compile_program_to_msgpack(program, source, source_label, &frontend_options)
        {
            Ok(frontend) => frontend,
            Err(failure) => {
                render_frontend_diagnostics(&failure.diagnostics);
                return Err(if failure.diagnostics.is_empty() {
                    CompileFromSourceError::Message(failure.message)
                } else {
                    CompileFromSourceError::DiagnosticsRendered
                });
            }
        };
    render_frontend_diagnostics(&frontend.diagnostics);

    if !target.can_link_with_host_tools() {
        return Err(CompileFromSourceError::Message(
            target.unsupported_native_link_error(),
        ));
    }

    compile_and_link(
        &frontend.data,
        source_label,
        Some(output_path),
        &target,
        options,
    )
    .map_err(CompileFromSourceError::Message)?;
    Ok(())
}

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

    // SAFETY: the msgpack buffer and option pointers remain valid for the
    // duration of the FFI call, and `buffer` is an out-parameter.
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

    // SAFETY: the codegen bridge populated `buffer` with a valid allocation
    // that remains alive until freed with `hew_codegen_buffer_free`.
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
    // SAFETY: the codegen bridge returns either null or a valid C string until
    // the next codegen call.
    let error = unsafe { hew_codegen_last_error() };
    if error.is_null() {
        "embedded codegen failed".into()
    } else {
        // SAFETY: `error` is non-null and points to a NUL-terminated C string.
        unsafe { CStr::from_ptr(error) }
            .to_string_lossy()
            .into_owned()
    }
}

#[cfg(test)]
type ImportResolutionContext<'a> = hew_compile::ImportResolutionContext<'a>;

#[cfg(test)]
fn validate_imports_against_manifest(
    items: &[Spanned<Item>],
    manifest_deps: &[String],
    package_name: Option<&str>,
) -> Vec<String> {
    frontend_validate_imports_against_manifest(items, manifest_deps, package_name)
}

#[cfg(test)]
fn line_map_from_source(source: &str) -> Vec<usize> {
    frontend_line_map_from_source(source)
}

#[cfg(test)]
fn parse_source(source: &str, input: &str) -> Result<hew_parser::ast::Program, String> {
    frontend_parse_source(source, input).map_err(|failure| failure.message)
}

#[cfg(test)]
fn build_module_graph(
    source_file: &Path,
    items: &mut Vec<Spanned<Item>>,
    module_doc: Option<String>,
    ctx: &mut ImportResolutionContext<'_>,
) -> Result<hew_parser::module::ModuleGraph, Vec<String>> {
    frontend_build_module_graph(source_file, items, module_doc, ctx)
        .map_err(|failure| vec![failure.message])
}

#[cfg(test)]
fn typecheck_program(
    program: &hew_parser::ast::Program,
    source: &str,
    input: &str,
    target: &TargetSpec,
    options: &CompileOptions,
) -> Result<hew_compile::TypeCheckResult, String> {
    frontend_typecheck_program(program, source, input, &frontend_options(target, options))
        .map_err(|failure| failure.message)
}

#[cfg(test)]
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
    frontend_enrich_program_ast(program, tco, module_registry, source, input)
        .map_err(|failure| failure.message)
}

#[cfg(test)]
fn inject_implicit_imports(items: &mut Vec<Spanned<Item>>, source: &str) {
    frontend_inject_implicit_imports(items, source);
}

#[cfg(test)]
fn collect_new_inferred_type_diagnostics<'a>(
    diagnostics: &'a [hew_serialize::TypeExprConversionError],
    root_filename: &str,
    imported_item_sources: &[(hew_parser::ast::Span, Option<PathBuf>)],
    seen_spans: &mut HashSet<(String, usize, usize)>,
) -> Vec<&'a hew_serialize::TypeExprConversionError> {
    frontend_collect_new_inferred_type_diagnostics(
        diagnostics,
        root_filename,
        imported_item_sources,
        seen_spans,
    )
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
            lowering_facts: HashMap::new(),
            assign_target_kinds: HashMap::new(),
            assign_target_shapes: HashMap::new(),
            errors: vec![],
            warnings: vec![],
            type_defs: HashMap::new(),
            fn_sigs: HashMap::new(),
            handle_bearing_structs: std::collections::HashSet::new(),
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
                    fn_span: 0..0,
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
            Ty::option(Ty::Var(hew_types::ty::TypeVar(7))),
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
                    fn_span: 0..0,
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
                    fn_span: 0..0,
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

    /// Regression guard for issue #789: internal generator handle metadata must
    /// be suppressed before the CLI serializer boundary instead of surfacing as
    /// a fatal inferred-type diagnostic from `build_expr_type_map`.
    #[test]
    fn internal_generator_expr_type_entries_are_suppressed_before_boundary() {
        use hew_types::check::SpanKey;

        // Generator handles are internal metadata for native codegen surfaces.
        // The serializer rescue must drop them before expr-type-map emission so
        // the CLI boundary only reports truly user-visible unresolved/invalid
        // inferred types.
        let mut tco = empty_tco();
        tco.expr_types.insert(
            SpanKey { start: 0, end: 1 },
            Ty::generator(Ty::I32, Ty::String),
        );

        let build = hew_serialize::build_expr_type_map(&tco);
        assert!(
            build.diagnostics().is_empty(),
            "internal generator expr_types must be suppressed before the CLI boundary"
        );
        assert!(
            build.entries.is_empty(),
            "internal generator expr_types must not be serialized into expr_type_map entries"
        );
    }
}
