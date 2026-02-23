//! Build script for hew-types: generates stdlib match tables from .hew files.
//!
//! Walks `../std/` and `../ecosystem/` for `.hew` files, parses each with
//! `hew_parser`, and emits a generated Rust file containing the same lookup
//! tables that were previously handcoded in `stdlib.rs`.
//!
//! Module resolution follows a Go-style directory-as-module pattern: all `.hew`
//! files in the same directory belong to the same module. The module path is
//! derived from the directory structure.

use std::collections::BTreeMap;
use std::fmt::Write;
use std::path::{Path, PathBuf};

use hew_parser::ast::{
    Block, Expr, ExternFnDecl, FnDecl, ImplDecl, Item, Stmt, TypeBodyItem, TypeExpr,
};

fn main() {
    let manifest_dir =
        PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set"));
    let repo_root = manifest_dir
        .parent()
        .expect("hew-types should be in repo root");
    let out_dir = PathBuf::from(std::env::var("OUT_DIR").expect("OUT_DIR not set"));

    // Phase 1: Collect all .hew files grouped by directory
    let mut dir_files: BTreeMap<PathBuf, Vec<PathBuf>> = BTreeMap::new();

    for dir_name in &["std", "ecosystem"] {
        let dir = repo_root.join(dir_name);
        if dir.exists() {
            collect_hew_files(&dir, &mut dir_files);
        }
    }

    // Emit rerun-if-changed for source directories
    println!("cargo:rerun-if-changed={}", repo_root.join("std").display());
    println!(
        "cargo:rerun-if-changed={}",
        repo_root.join("ecosystem").display()
    );

    // Phase 2: For each directory, parse all files and merge into one module.
    //
    // Special case: files directly in a root dir (std/, ecosystem/) are each
    // their own module (flat layout: std/fs.hew → std::fs). Files in
    // subdirectories are merged into one module per directory (Go-style:
    // std/net/http/http.hew + http_client.hew → std::net::http).
    let mut modules: BTreeMap<String, ModuleData> = BTreeMap::new();

    for (dir_path, files) in &dir_files {
        let Ok(rel) = dir_path.strip_prefix(repo_root) else {
            continue;
        };
        let depth = rel.components().count();

        if depth <= 1 {
            // Root-level files: each file is its own module
            for file in files {
                println!("cargo:rerun-if-changed={}", file.display());

                let Ok(source) = std::fs::read_to_string(file) else {
                    continue;
                };

                let stem = file.file_stem().unwrap().to_str().unwrap();
                let module_path = extract_import_path(&source).unwrap_or_else(|| {
                    let root_name = rel
                        .components()
                        .next()
                        .unwrap()
                        .as_os_str()
                        .to_str()
                        .unwrap();
                    format!("{root_name}::{stem}")
                });

                let short_name = module_path
                    .rsplit("::")
                    .next()
                    .unwrap_or(&module_path)
                    .to_string();

                let result = hew_parser::parse(&source);
                if !result.errors.is_empty() {
                    continue;
                }

                let mut data = ModuleData {
                    module_path: module_path.clone(),
                    short_name,
                    functions: Vec::new(),
                    wrapper_fns: Vec::new(),
                    clean_names: Vec::new(),
                    handle_types: Vec::new(),
                    drop_types: Vec::new(),
                    handle_methods: Vec::new(),
                };
                let sn = data.short_name.clone();
                merge_module_data(&result.program, &sn, &mut data);

                if !data.functions.is_empty()
                    || !data.handle_types.is_empty()
                    || !data.wrapper_fns.is_empty()
                {
                    modules.insert(module_path, data);
                }
            }
        } else {
            // Subdirectory: all files merge into one module
            let Some(module_path) = resolve_module_path(dir_path, files, repo_root) else {
                continue;
            };

            let short_name = module_path
                .rsplit("::")
                .next()
                .unwrap_or(&module_path)
                .to_string();

            let mut data = ModuleData {
                module_path: module_path.clone(),
                short_name,
                functions: Vec::new(),
                wrapper_fns: Vec::new(),
                clean_names: Vec::new(),
                handle_types: Vec::new(),
                drop_types: Vec::new(),
                handle_methods: Vec::new(),
            };

            for file in files {
                println!("cargo:rerun-if-changed={}", file.display());

                let Ok(source) = std::fs::read_to_string(file) else {
                    continue;
                };

                let result = hew_parser::parse(&source);
                if !result.errors.is_empty() {
                    continue;
                }

                let sn = data.short_name.clone();
                merge_module_data(&result.program, &sn, &mut data);
            }

            if !data.functions.is_empty()
                || !data.handle_types.is_empty()
                || !data.wrapper_fns.is_empty()
            {
                modules.insert(module_path, data);
            }
        }
    }

    // Phase 3: Generate the output file
    let output = generate_code(&modules);
    let out_path = out_dir.join("stdlib_generated.rs");
    std::fs::write(&out_path, output).expect("failed to write stdlib_generated.rs");
}

/// All data extracted from a module (potentially spanning multiple .hew files).
struct ModuleData {
    module_path: String,
    short_name: String,
    /// Extern C function signatures: (`c_name`, `params_code`, `return_code`)
    functions: Vec<(String, Vec<String>, String)>,
    /// Wrapper `pub fn` signatures: (`method_name`, `params_code`, `return_code`).
    /// These capture the wrapper function's own signature (which may differ from
    /// the underlying extern C function it calls).
    wrapper_fns: Vec<(String, Vec<String>, String)>,
    /// Clean name mappings: (`user_name`, `c_symbol`)
    clean_names: Vec<(String, String)>,
    /// Handle type names, e.g. "json.Value"
    handle_types: Vec<String>,
    /// Types with `impl Drop` — these are move-only (not Copy)
    drop_types: Vec<String>,
    /// Handle method mappings: ((`qualified_type`, `method_name`), `c_symbol`)
    handle_methods: Vec<((String, String), String)>,
}

/// Recursively collect all .hew files, grouped by their parent directory.
fn collect_hew_files(dir: &Path, dir_files: &mut BTreeMap<PathBuf, Vec<PathBuf>>) {
    let mut entries: Vec<_> = std::fs::read_dir(dir)
        .expect("failed to read directory")
        .filter_map(std::result::Result::ok)
        .collect();
    entries.sort_by_key(std::fs::DirEntry::file_name);

    let mut has_hew_files = false;

    for entry in &entries {
        let path = entry.path();
        if path.is_dir() {
            collect_hew_files(&path, dir_files);
        } else if path.extension().is_some_and(|e| e == "hew") {
            // Skip builtins.hew
            if path.file_name().is_some_and(|f| f == "builtins.hew") {
                continue;
            }
            dir_files.entry(dir.to_path_buf()).or_default().push(path);
            has_hew_files = true;
        }
    }

    // Sort files within each directory for deterministic output
    if has_hew_files {
        if let Some(files) = dir_files.get_mut(dir) {
            files.sort();
        }
    }
}

/// Determine the module path for a directory containing .hew files.
///
/// Uses the `//! import PATH;` comment from any file in the directory as the
/// authoritative source, falling back to directory-path-based inference.
///
/// Directory-as-module rule: the module path is derived from the directory
/// relative to the repo root (e.g. `std/encoding/json/` → `std::encoding::json`).
///
/// For files directly under a root dir (e.g. `std/fs.hew` in `std/`),
/// a special case applies — each file is its own module, and the root dir
/// is NOT a module. This is handled by returning None for the root dirs
/// themselves when they only contain "loose" files.
fn resolve_module_path(dir: &Path, files: &[PathBuf], repo_root: &Path) -> Option<String> {
    // Try to extract module path from any file's `//! import` comment
    for file in files {
        if let Ok(source) = std::fs::read_to_string(file) {
            if let Some(path) = extract_import_path(&source) {
                return Some(path);
            }
        }
    }

    // Fall back to directory-based inference
    let rel = dir.strip_prefix(repo_root).ok()?;
    let segments: Vec<&str> = rel
        .components()
        .map(|c| c.as_os_str().to_str().unwrap())
        .collect();

    if segments.is_empty() {
        return None;
    }

    // For root dirs (just "std" or "ecosystem"), they're not modules themselves
    if segments.len() == 1 {
        return None;
    }

    Some(segments.join("::"))
}

/// Extract the first `//! import PATH;` from a source file.
fn extract_import_path(source: &str) -> Option<String> {
    for line in source.lines().take(20) {
        let trimmed = line.trim();
        if let Some(rest) = trimmed.strip_prefix("//! import ") {
            if let Some(path) = rest.strip_suffix(';') {
                let path = path.trim();
                if !path.is_empty() {
                    return Some(path.to_string());
                }
            }
        }
    }
    None
}

/// Parse a .hew program and merge its data into an existing `ModuleData`.
fn merge_module_data(program: &hew_parser::ast::Program, short_name: &str, data: &mut ModuleData) {
    // Collect extern "C" function signatures
    for (item, _span) in &program.items {
        if let Item::ExternBlock(block) = item {
            for func in &block.functions {
                let (params, ret) = extern_fn_sig_code(func, short_name);
                data.functions.push((func.name.clone(), params, ret));
            }
        }
    }

    // Collect handle types from `type` declarations.
    // Collect types that have `impl Drop` — these must NOT be treated as Copy
    // because they own resources and need move semantics for actor sends.
    let mut drop_types: std::collections::HashSet<String> = std::collections::HashSet::new();
    for (item, _span) in &program.items {
        if let Item::Impl(impl_decl) = item {
            if let Some(ref tb) = impl_decl.trait_bound {
                if tb.name == "Drop" {
                    if let TypeExpr::Named { ref name, .. } = impl_decl.target_type.0 {
                        drop_types.insert(name.clone());
                    }
                }
            }
        }
    }

    // Only types WITHOUT struct fields are opaque handles (e.g. http.Server,
    // regex.Pattern).  Types WITH fields (e.g. bench.Suite) are real structs
    // whose layout is defined in Hew and must be lowered as LLVM struct types,
    // not opaque pointers.
    // Note: types with `impl Drop` are STILL handle types (they need pointer
    // lowering) but are separately tracked as drop types to prevent Copy.
    for (item, _span) in &program.items {
        if let Item::TypeDecl(td) = item {
            let has_fields = td
                .body
                .iter()
                .any(|b| matches!(b, TypeBodyItem::Field { .. }));
            if !has_fields {
                let qualified = format!("{short_name}.{}", td.name);
                data.handle_types.push(qualified);
            }
        }
    }

    // Record qualified drop type names for the trait registry
    for dt in &drop_types {
        data.drop_types.push(format!("{short_name}.{dt}"));
    }

    // Collect clean name → C symbol mappings and wrapper fn signatures from `pub fn` declarations
    for (item, _span) in &program.items {
        if let Item::Function(fn_decl) = item {
            if fn_decl.is_pub {
                // Always capture the wrapper function's own signature for type checking.
                // This may differ from the underlying extern C function (e.g., `pub fn setup()` wraps
                // `hew_log_set_level(level: i32)` — the wrapper takes 0 args, the extern takes 1).
                let (params, ret) = wrapper_fn_sig_code(fn_decl, short_name);
                data.wrapper_fns.push((fn_decl.name.clone(), params, ret));

                // Clean name: use the C function target only for trivial pass-through
                // wrappers (same param count). Non-trivial wrappers (e.g., `setup()`
                // calling `hew_log_set_level(2)`) use identity mapping so the wrapper
                // Hew function is compiled and called instead.
                let c_target =
                    if let Some((target, call_arg_count)) = extract_call_target(&fn_decl.body) {
                        if call_arg_count == fn_decl.params.len() {
                            target
                        } else {
                            fn_decl.name.clone()
                        }
                    } else {
                        fn_decl.name.clone()
                    };
                data.clean_names.push((fn_decl.name.clone(), c_target));
            }
        }
    }

    // Collect handle method → C symbol mappings from `impl` blocks
    for (item, _span) in &program.items {
        if let Item::Impl(impl_decl) = item {
            extract_handle_methods(impl_decl, short_name, data);
        }
    }
}

/// Convert an extern function's parameter/return types to Rust source code strings.
fn extern_fn_sig_code(func: &ExternFnDecl, module_short: &str) -> (Vec<String>, String) {
    let params: Vec<String> = func
        .params
        .iter()
        .map(|p| type_expr_to_code(&p.ty.0, module_short))
        .collect();

    let ret = func.return_type.as_ref().map_or_else(
        || "Ty::Unit".to_string(),
        |rt| type_expr_to_code(&rt.0, module_short),
    );

    (params, ret)
}

/// Convert a wrapper `pub fn`'s parameter/return types to Rust source code strings.
/// Mirrors `extern_fn_sig_code()` but works on `FnDecl` params (which use `Param` not `ExternParam`).
fn wrapper_fn_sig_code(func: &FnDecl, module_short: &str) -> (Vec<String>, String) {
    let params: Vec<String> = func
        .params
        .iter()
        .map(|p| type_expr_to_code(&p.ty.0, module_short))
        .collect();

    let ret = func.return_type.as_ref().map_or_else(
        || "Ty::Unit".to_string(),
        |rt| type_expr_to_code(&rt.0, module_short),
    );

    (params, ret)
}

/// Convert a Hew type expression to Rust source code that constructs the corresponding `Ty`.
fn type_expr_to_code(texpr: &TypeExpr, module_short: &str) -> String {
    match texpr {
        TypeExpr::Named { name, type_args } => match name.as_str() {
            "String" | "string" => "Ty::String".to_string(),
            "i8" => "Ty::I8".to_string(),
            "i16" => "Ty::I16".to_string(),
            "i32" => "Ty::I32".to_string(),
            "i64" | "int" | "Int" => "Ty::I64".to_string(),
            "u8" => "Ty::U8".to_string(),
            "u16" => "Ty::U16".to_string(),
            "u32" => "Ty::U32".to_string(),
            "u64" => "Ty::U64".to_string(),
            "f32" => "Ty::F32".to_string(),
            "f64" => "Ty::F64".to_string(),
            "bool" => "Ty::Bool".to_string(),
            "char" => "Ty::Char".to_string(),
            "bytes" => "Ty::Named { name: \"bytes\".to_string(), args: vec![] }".to_string(),
            n if n.contains('.') => {
                let args_code = type_args_to_code(type_args, module_short);
                format!("Ty::Named {{ name: \"{n}\".to_string(), args: {args_code} }}")
            }
            // Option<T> → Ty::Option to match the type checker's internal repr
            "Option" => {
                if let Some(args) = type_args {
                    if let Some(first) = args.first() {
                        let inner = type_expr_to_code(&first.0, module_short);
                        return format!("Ty::Option(Box::new({inner}))");
                    }
                }
                "Ty::Named { name: \"Option\".to_string(), args: vec![] }".to_string()
            }
            // Result<O,E> → Ty::Result to match the type checker's internal repr
            "Result" => {
                if let Some(args) = type_args {
                    if args.len() >= 2 {
                        let ok = type_expr_to_code(&args[0].0, module_short);
                        let err = type_expr_to_code(&args[1].0, module_short);
                        return format!(
                            "Ty::Result {{ ok: Box::new({ok}), err: Box::new({err}) }}"
                        );
                    }
                }
                "Ty::Named { name: \"Result\".to_string(), args: vec![] }".to_string()
            }
            // Built-in generic/language types should not be module-qualified
            "Vec" | "HashMap" | "ActorRef" | "Actor" | "Task" | "Stream" | "Sink"
            | "StreamPair" => {
                let args_code = type_args_to_code(type_args, module_short);
                format!("Ty::Named {{ name: \"{name}\".to_string(), args: {args_code} }}")
            }
            other => {
                let args_code = type_args_to_code(type_args, module_short);
                format!(
                    "Ty::Named {{ name: \"{module_short}.{other}\".to_string(), args: {args_code} }}"
                )
            }
        },
        TypeExpr::Option(inner) => {
            let inner_code = type_expr_to_code(&inner.0, module_short);
            format!("Ty::Option(Box::new({inner_code}))")
        }
        TypeExpr::Result { ok, err } => {
            let ok_code = type_expr_to_code(&ok.0, module_short);
            let err_code = type_expr_to_code(&err.0, module_short);
            format!("Ty::Result {{ ok: Box::new({ok_code}), err: Box::new({err_code}) }}")
        }
        TypeExpr::Tuple(elems) => {
            let elem_codes: Vec<String> = elems
                .iter()
                .map(|(te, _)| type_expr_to_code(te, module_short))
                .collect();
            format!("Ty::Tuple(vec![{}])", elem_codes.join(", "))
        }
        TypeExpr::Array { element, size } => {
            let elem_code = type_expr_to_code(&element.0, module_short);
            format!("Ty::Array(Box::new({elem_code}), {size})")
        }
        TypeExpr::Slice(inner) => {
            let inner_code = type_expr_to_code(&inner.0, module_short);
            format!("Ty::Slice(Box::new({inner_code}))")
        }
        TypeExpr::Function {
            params,
            return_type,
        } => {
            let param_codes: Vec<String> = params
                .iter()
                .map(|(te, _)| type_expr_to_code(te, module_short))
                .collect();
            let ret_code = type_expr_to_code(&return_type.0, module_short);
            format!(
                "Ty::Function {{ params: vec![{}], ret: Box::new({ret_code}) }}",
                param_codes.join(", ")
            )
        }
        TypeExpr::Pointer {
            is_mutable,
            pointee,
        } => {
            let pointee_code = type_expr_to_code(&pointee.0, module_short);
            format!("Ty::Pointer {{ is_mutable: {is_mutable}, pointee: Box::new({pointee_code}) }}")
        }
        TypeExpr::TraitObject(bound) => {
            format!(
                "Ty::TraitObject {{ trait_name: \"{}\".to_string(), args: vec![] }}",
                bound.name
            )
        }
        TypeExpr::Infer => "Ty::Error".to_string(),
    }
}

fn type_args_to_code(
    type_args: &Option<Vec<(TypeExpr, hew_parser::ast::Span)>>,
    module_short: &str,
) -> String {
    match type_args {
        Some(args) if !args.is_empty() => {
            let arg_codes: Vec<String> = args
                .iter()
                .map(|(te, _)| type_expr_to_code(te, module_short))
                .collect();
            format!("vec![{}]", arg_codes.join(", "))
        }
        _ => "vec![]".to_string(),
    }
}

/// Extract the C function name and its call argument count from a wrapper body.
fn extract_call_target(body: &Block) -> Option<(String, usize)> {
    if let Some(trailing) = &body.trailing_expr {
        return call_target_from_expr(&trailing.0);
    }
    if let Some((stmt, _)) = body.stmts.last() {
        match stmt {
            Stmt::Expression(expr) | Stmt::Return(Some(expr)) => {
                return call_target_from_expr(&expr.0)
            }
            _ => {}
        }
    }
    None
}

fn call_target_from_expr(expr: &Expr) -> Option<(String, usize)> {
    match expr {
        Expr::Call { function, args, .. } => {
            if let Expr::Identifier(name) = &function.0 {
                return Some((name.clone(), args.len()));
            }
            None
        }
        Expr::Block(block) => extract_call_target(block),
        _ => None,
    }
}

fn extract_handle_methods(impl_decl: &ImplDecl, module_short: &str, data: &mut ModuleData) {
    let type_name = match &impl_decl.target_type.0 {
        TypeExpr::Named { name, .. } => {
            if name.contains('.') {
                name.clone()
            } else {
                format!("{module_short}.{name}")
            }
        }
        _ => return,
    };

    for method in &impl_decl.methods {
        if let Some((c_symbol, _arg_count)) = extract_call_target(&method.body) {
            data.handle_methods
                .push(((type_name.clone(), method.name.clone()), c_symbol));
        }
    }
}

// ---------------------------------------------------------------------------
// Code generation
// ---------------------------------------------------------------------------

fn generate_code(modules: &BTreeMap<String, ModuleData>) -> String {
    let mut out = String::with_capacity(64 * 1024);

    writeln!(
        out,
        "// AUTO-GENERATED by hew-types/build.rs — do not edit by hand."
    )
    .unwrap();
    writeln!(
        out,
        "// Regenerated automatically when any .hew file under std/ or ecosystem/ changes."
    )
    .unwrap();
    writeln!(out).unwrap();

    generate_stdlib_functions(&mut out, modules);
    generate_wrapper_fn_sigs(&mut out, modules);
    generate_module_short_name(&mut out, modules);
    generate_stdlib_clean_names(&mut out, modules);
    generate_resolve_module_call(&mut out, modules);
    generate_handle_types_for_module(&mut out, modules);
    generate_resolve_handle_method(&mut out, modules);
    generate_handle_types_static(&mut out, modules);
    generate_drop_types_static(&mut out, modules);

    out
}

fn generate_stdlib_functions(out: &mut String, modules: &BTreeMap<String, ModuleData>) {
    writeln!(out, "#[must_use]").unwrap();
    writeln!(
        out,
        "fn generated_stdlib_functions(module_path: &str) -> Option<Vec<StdlibFn>> {{"
    )
    .unwrap();
    writeln!(out, "    match module_path {{").unwrap();

    for data in modules.values() {
        if data.functions.is_empty() {
            continue;
        }
        writeln!(out, "        \"{}\" => Some(vec![", data.module_path).unwrap();
        for (c_name, params, ret) in &data.functions {
            let params_code = if params.is_empty() {
                "vec![]".to_string()
            } else {
                format!("vec![{}]", params.join(", "))
            };
            writeln!(
                out,
                "            (\"{c_name}\".into(), {params_code}, {ret}),"
            )
            .unwrap();
        }
        writeln!(out, "        ]),").unwrap();
    }

    writeln!(out, "        _ => None,").unwrap();
    writeln!(out, "    }}").unwrap();
    writeln!(out, "}}").unwrap();
    writeln!(out).unwrap();
}

/// Emit wrapper `pub fn` signatures under their clean method names.
/// These are separate from `stdlib_functions` (which only contains extern C sigs)
/// because `synthesize_stdlib_externs` in the enricher generates extern declarations
/// from `stdlib_functions` — wrapper fns must NOT get extern declarations.
fn generate_wrapper_fn_sigs(out: &mut String, modules: &BTreeMap<String, ModuleData>) {
    writeln!(out, "#[must_use]").unwrap();
    writeln!(
        out,
        "fn generated_wrapper_fn_sigs(module_path: &str) -> Option<Vec<StdlibFn>> {{"
    )
    .unwrap();
    writeln!(out, "    match module_path {{").unwrap();

    for data in modules.values() {
        if data.wrapper_fns.is_empty() {
            continue;
        }
        writeln!(out, "        \"{}\" => Some(vec![", data.module_path).unwrap();
        for (method_name, params, ret) in &data.wrapper_fns {
            let params_code = if params.is_empty() {
                "vec![]".to_string()
            } else {
                format!("vec![{}]", params.join(", "))
            };
            writeln!(
                out,
                "            (\"{method_name}\".into(), {params_code}, {ret}),"
            )
            .unwrap();
        }
        writeln!(out, "        ]),").unwrap();
    }

    writeln!(out, "        _ => None,").unwrap();
    writeln!(out, "    }}").unwrap();
    writeln!(out, "}}").unwrap();
    writeln!(out).unwrap();
}

fn generate_module_short_name(out: &mut String, modules: &BTreeMap<String, ModuleData>) {
    writeln!(out, "#[must_use]").unwrap();
    writeln!(
        out,
        "fn generated_module_short_name(module_path: &str) -> Option<&'static str> {{"
    )
    .unwrap();
    writeln!(out, "    match module_path {{").unwrap();

    for data in modules.values() {
        writeln!(
            out,
            "        \"{}\" => Some(\"{}\"),",
            data.module_path, data.short_name
        )
        .unwrap();
    }

    writeln!(out, "        _ => None,").unwrap();
    writeln!(out, "    }}").unwrap();
    writeln!(out, "}}").unwrap();
    writeln!(out).unwrap();
}

fn generate_stdlib_clean_names(out: &mut String, modules: &BTreeMap<String, ModuleData>) {
    writeln!(out, "#[must_use]").unwrap();
    writeln!(
        out,
        "#[expect(clippy::too_many_lines, reason = \"generated stdlib lookup table\")]"
    )
    .unwrap();
    writeln!(
        out,
        "fn generated_stdlib_clean_names(module_path: &str) -> Option<Vec<(&'static str, &'static str)>> {{"
    )
    .unwrap();
    writeln!(out, "    match module_path {{").unwrap();

    for data in modules.values() {
        if data.clean_names.is_empty() {
            if !data.functions.is_empty() {
                writeln!(out, "        \"{}\" => Some(vec![]),", data.module_path).unwrap();
            }
            continue;
        }
        writeln!(out, "        \"{}\" => Some(vec![", data.module_path).unwrap();
        for (clean, c_sym) in &data.clean_names {
            writeln!(out, "            (\"{clean}\", \"{c_sym}\"),").unwrap();
        }
        writeln!(out, "        ]),").unwrap();
    }

    writeln!(out, "        _ => None,").unwrap();
    writeln!(out, "    }}").unwrap();
    writeln!(out, "}}").unwrap();
    writeln!(out).unwrap();
}

fn generate_resolve_module_call(out: &mut String, modules: &BTreeMap<String, ModuleData>) {
    writeln!(out, "#[must_use]").unwrap();
    writeln!(
        out,
        "fn generated_resolve_module_call(module: &str, method: &str) -> Option<&'static str> {{"
    )
    .unwrap();
    writeln!(out, "    let module_path = match module {{").unwrap();

    for data in modules.values() {
        writeln!(
            out,
            "        \"{}\" => \"{}\",",
            data.short_name, data.module_path
        )
        .unwrap();
    }

    writeln!(out, "        _ => return None,").unwrap();
    writeln!(out, "    }};").unwrap();
    writeln!(
        out,
        "    let names = generated_stdlib_clean_names(module_path)?;"
    )
    .unwrap();
    writeln!(
        out,
        "    names.into_iter().find(|(clean, _)| *clean == method).map(|(_, c_sym)| c_sym)"
    )
    .unwrap();
    writeln!(out, "}}").unwrap();
    writeln!(out).unwrap();
}

fn generate_handle_types_for_module(out: &mut String, modules: &BTreeMap<String, ModuleData>) {
    writeln!(out, "#[must_use]").unwrap();
    writeln!(
        out,
        "fn generated_handle_types_for_module(module_path: &str) -> Vec<&'static str> {{"
    )
    .unwrap();
    writeln!(out, "    match module_path {{").unwrap();

    for data in modules.values() {
        if data.handle_types.is_empty() {
            continue;
        }
        let types: Vec<String> = data
            .handle_types
            .iter()
            .map(|t| format!("\"{t}\""))
            .collect();
        writeln!(
            out,
            "        \"{}\" => vec![{}],",
            data.module_path,
            types.join(", ")
        )
        .unwrap();
    }

    writeln!(out, "        _ => vec![],").unwrap();
    writeln!(out, "    }}").unwrap();
    writeln!(out, "}}").unwrap();
    writeln!(out).unwrap();
}

fn generate_resolve_handle_method(out: &mut String, modules: &BTreeMap<String, ModuleData>) {
    writeln!(out, "#[must_use]").unwrap();
    writeln!(
        out,
        "#[expect(clippy::too_many_lines, reason = \"generated handle method lookup table\")]"
    )
    .unwrap();
    writeln!(
        out,
        "fn generated_resolve_handle_method(handle_type: &str, method: &str) -> Option<&'static str> {{"
    )
    .unwrap();
    writeln!(out, "    match (handle_type, method) {{").unwrap();

    // Group handle methods by (type_name, c_symbol) to merge arms with identical bodies
    // and avoid clippy::match_same_arms warnings in generated code.
    let mut grouped: std::collections::BTreeMap<(&str, &str), Vec<&str>> =
        std::collections::BTreeMap::new();
    for data in modules.values() {
        for ((type_name, method_name), c_symbol) in &data.handle_methods {
            grouped
                .entry((type_name.as_str(), c_symbol.as_str()))
                .or_default()
                .push(method_name.as_str());
        }
    }
    for ((type_name, c_symbol), methods) in &grouped {
        let patterns: Vec<String> = methods
            .iter()
            .map(|m| format!("(\"{type_name}\", \"{m}\")"))
            .collect();
        writeln!(
            out,
            "        {} => Some(\"{c_symbol}\"),",
            patterns.join(" | ")
        )
        .unwrap();
    }

    writeln!(out, "        _ => None,").unwrap();
    writeln!(out, "    }}").unwrap();
    writeln!(out, "}}").unwrap();
    writeln!(out).unwrap();
}

fn generate_handle_types_static(out: &mut String, modules: &BTreeMap<String, ModuleData>) {
    let mut all_types: Vec<&str> = Vec::new();
    for data in modules.values() {
        for t in &data.handle_types {
            all_types.push(t);
        }
    }

    writeln!(out, "static GENERATED_HANDLE_TYPES: &[&str] = &[").unwrap();
    for t in &all_types {
        writeln!(out, "    \"{t}\",").unwrap();
    }
    writeln!(out, "];").unwrap();
}

fn generate_drop_types_static(out: &mut String, modules: &BTreeMap<String, ModuleData>) {
    let mut all_types: Vec<&str> = Vec::new();
    for data in modules.values() {
        for t in &data.drop_types {
            all_types.push(t);
        }
    }

    writeln!(out).unwrap();
    writeln!(out, "static GENERATED_DROP_TYPES: &[&str] = &[").unwrap();
    for t in &all_types {
        writeln!(out, "    \"{t}\",").unwrap();
    }
    writeln!(out, "];").unwrap();
}
