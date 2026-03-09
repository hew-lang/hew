//! AST-based stdlib module loader.
//!
//! Parses `.hew` files and extracts type information: function signatures,
//! clean name mappings, handle types, and handle method mappings.

use std::path::Path;

use hew_parser::ast::{
    Block, Expr, ExternFnDecl, FnDecl, ImplDecl, Item, Stmt, TypeBodyItem, TypeExpr,
};
use hew_parser::parse;

use crate::ty::Ty;

/// All type information extracted from a single `.hew` module file.
#[derive(Debug, Clone)]
pub struct ModuleInfo {
    /// C function signatures: (`c_name`, `param_types`, `return_type`).
    pub functions: Vec<(String, Vec<Ty>, Ty)>,
    /// Clean name mappings: (`user_name`, `c_symbol`).
    pub clean_names: Vec<(String, String)>,
    /// Handle type names, e.g. `"json.Value"`.
    pub handle_types: Vec<String>,
    /// Handle method mappings: ((`type_name`, `method_name`), `c_symbol`).
    pub handle_methods: Vec<((String, String), String)>,
    /// Wrapper `pub fn` signatures: (`method_name`, `param_types`, `return_type`).
    pub wrapper_fns: Vec<(String, Vec<Ty>, Ty)>,
    /// Types with `impl Drop` — move-only, not Copy.
    pub drop_types: Vec<String>,
}

/// Load type information for a module from its `.hew` file.
///
/// `module_path` is the `::` separated module path, e.g. `"std::encoding::json"`.
///
/// `root` is the workspace root (parent of `std/` and `ecosystem/`).
///
/// Returns `None` if the `.hew` file cannot be found or parsed.
#[must_use]
pub fn load_module(module_path: &str, root: &Path) -> Option<ModuleInfo> {
    let hew_path = resolve_hew_path(module_path, root)?;
    let source = std::fs::read_to_string(&hew_path).ok()?;
    let result = parse(&source);
    if !result.errors.is_empty() {
        return None;
    }

    let module_short = module_short_name(module_path);
    Some(extract_module_info(&result.program, &module_short))
}

/// Resolve a module path to a `.hew` file on disk.
///
/// Tries two forms:
/// 1. Package-directory form: `std/encoding/json/json.hew`
/// 2. Flat form: `std/encoding/json.hew`
///
/// For ecosystem modules (e.g. `ecosystem::db::postgres`), the root is
/// `ecosystem/` instead of `std/`.
fn resolve_hew_path(module_path: &str, root: &Path) -> Option<std::path::PathBuf> {
    let segments: Vec<&str> = module_path.split("::").collect();
    if segments.is_empty() {
        return None;
    }

    // Build the relative path from segments
    let rel: std::path::PathBuf = segments.iter().collect();

    // Try package-directory form first: std/encoding/json/json.hew
    let last = segments.last()?;
    let dir_form = rel.join(format!("{last}.hew"));
    let candidate = root.join(&dir_form);
    if candidate.exists() {
        return Some(candidate);
    }

    // Try flat form: std/encoding/json.hew
    let flat_form = rel.with_extension("hew");
    let candidate = root.join(&flat_form);
    if candidate.exists() {
        return Some(candidate);
    }

    None
}

/// Extract the short module name (last segment) from a module path.
#[must_use]
pub fn module_short_name(module_path: &str) -> String {
    module_path
        .rsplit("::")
        .next()
        .unwrap_or(module_path)
        .to_string()
}

/// Extract all type information from a parsed `.hew` program.
fn extract_module_info(program: &hew_parser::ast::Program, module_short: &str) -> ModuleInfo {
    let mut info = ModuleInfo {
        functions: Vec::new(),
        clean_names: Vec::new(),
        handle_types: Vec::new(),
        handle_methods: Vec::new(),
        wrapper_fns: Vec::new(),
        drop_types: Vec::new(),
    };

    for (item, _span) in &program.items {
        match item {
            Item::ExternBlock(block) => {
                for func in &block.functions {
                    let (params, ret) = extern_fn_sig(func, module_short);
                    info.functions.push((func.name.clone(), params, ret));
                }
            }
            // Only types WITHOUT struct fields are opaque handles.
            // Types with fields (e.g. Version { maj: i32; ... }) are real
            // structs, not opaque pointers.
            Item::TypeDecl(td) => {
                let has_fields = td
                    .body
                    .iter()
                    .any(|b| matches!(b, TypeBodyItem::Field { .. }));
                if !has_fields {
                    let qualified = format!("{module_short}.{}", td.name);
                    info.handle_types.push(qualified);
                }
            }
            Item::Function(fn_decl) if fn_decl.visibility.is_pub() => {
                // Extract wrapper function's own signature
                let (params, ret) = wrapper_fn_sig(fn_decl, module_short);
                info.wrapper_fns.push((fn_decl.name.clone(), params, ret));

                // Clean name mapping: use the C function target only for
                // trivial pass-through wrappers (same param count). Non-trivial
                // wrappers (e.g. `setup()` calling `hew_log_set_level(2)`) use
                // identity mapping so the wrapper Hew function is compiled and
                // called instead.
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
                info.clean_names.push((fn_decl.name.clone(), c_target));
            }
            Item::Impl(impl_decl) => {
                // Detect `impl Drop for T` — collect as drop types
                if let Some(ref tb) = impl_decl.trait_bound {
                    if tb.name == "Drop" {
                        if let TypeExpr::Named { ref name, .. } = impl_decl.target_type.0 {
                            let qualified = if name.contains('.') {
                                name.clone()
                            } else {
                                format!("{module_short}.{name}")
                            };
                            info.drop_types.push(qualified);
                        }
                    }
                }
                extract_handle_methods(impl_decl, module_short, &mut info);
            }
            _ => {}
        }
    }

    info
}

/// Convert an extern function declaration to type checker types.
fn extern_fn_sig(func: &ExternFnDecl, module_short: &str) -> (Vec<Ty>, Ty) {
    let params: Vec<Ty> = func
        .params
        .iter()
        .map(|p| type_expr_to_ty(&p.ty.0, module_short))
        .collect();

    let ret = func
        .return_type
        .as_ref()
        .map_or(Ty::Unit, |rt| type_expr_to_ty(&rt.0, module_short));

    (params, ret)
}

/// Convert a wrapper `pub fn` declaration to type checker types.
/// Mirrors `extern_fn_sig` but works on `FnDecl` params (`Param` not `ExternParam`).
fn wrapper_fn_sig(func: &FnDecl, module_short: &str) -> (Vec<Ty>, Ty) {
    let params: Vec<Ty> = func
        .params
        .iter()
        .map(|p| type_expr_to_ty(&p.ty.0, module_short))
        .collect();

    let ret = func
        .return_type
        .as_ref()
        .map_or(Ty::Unit, |rt| type_expr_to_ty(&rt.0, module_short));

    (params, ret)
}

/// Convert a Hew type expression to the type checker's `Ty`.
#[allow(
    clippy::too_many_lines,
    reason = "type mapping covers all primitive and generic variants"
)]
fn type_expr_to_ty(texpr: &TypeExpr, module_short: &str) -> Ty {
    match texpr {
        TypeExpr::Named { name, type_args } => {
            match name.as_str() {
                "String" | "string" => Ty::String,
                "i8" => Ty::I8,
                "i16" => Ty::I16,
                "i32" => Ty::I32,
                "i64" | "int" | "Int" => Ty::I64,
                "u8" => Ty::U8,
                "u16" => Ty::U16,
                "u32" => Ty::U32,
                "u64" => Ty::U64,
                "f32" => Ty::F32,
                "f64" => Ty::F64,
                "bool" => Ty::Bool,
                "char" => Ty::Char,
                "bytes" => Ty::Bytes,
                "duration" => Ty::Duration,
                // Option<T> → Ty::option() helper
                "Option" => {
                    if let Some(args) = type_args {
                        if let Some(first) = args.first() {
                            return Ty::option(type_expr_to_ty(&first.0, module_short));
                        }
                    }
                    Ty::Named {
                        name: "Option".to_string(),
                        args: vec![],
                    }
                }
                // Result<O, E> → Ty::result() helper
                "Result" => {
                    if let Some(args) = type_args {
                        if args.len() >= 2 {
                            return Ty::result(
                                type_expr_to_ty(&args[0].0, module_short),
                                type_expr_to_ty(&args[1].0, module_short),
                            );
                        }
                    }
                    Ty::Named {
                        name: "Result".to_string(),
                        args: vec![],
                    }
                }
                // Built-in generic/language types — do NOT module-qualify
                "Vec" | "HashMap" | "ActorRef" | "Actor" | "Task" | "Stream" | "Sink"
                | "StreamPair" => {
                    let args = type_args
                        .as_ref()
                        .map(|a| {
                            a.iter()
                                .map(|(te, _)| type_expr_to_ty(te, module_short))
                                .collect()
                        })
                        .unwrap_or_default();
                    Ty::Named {
                        name: name.clone(),
                        args,
                    }
                }
                // Qualified handle type like "json.Value"
                n if n.contains('.') => Ty::Named {
                    name: n.to_string(),
                    args: type_args
                        .as_ref()
                        .map(|args| {
                            args.iter()
                                .map(|(te, _)| type_expr_to_ty(te, module_short))
                                .collect()
                        })
                        .unwrap_or_default(),
                },
                // Unqualified type name — qualify with module short name
                other => Ty::Named {
                    name: format!("{module_short}.{other}"),
                    args: type_args
                        .as_ref()
                        .map(|args| {
                            args.iter()
                                .map(|(te, _)| type_expr_to_ty(te, module_short))
                                .collect()
                        })
                        .unwrap_or_default(),
                },
            }
        }
        TypeExpr::Option(inner) => Ty::option(type_expr_to_ty(&inner.0, module_short)),
        TypeExpr::Result { ok, err } => Ty::result(
            type_expr_to_ty(&ok.0, module_short),
            type_expr_to_ty(&err.0, module_short),
        ),
        TypeExpr::Tuple(elems) => Ty::Tuple(
            elems
                .iter()
                .map(|(te, _)| type_expr_to_ty(te, module_short))
                .collect(),
        ),
        TypeExpr::Array { element, size } => {
            Ty::Array(Box::new(type_expr_to_ty(&element.0, module_short)), *size)
        }
        TypeExpr::Slice(inner) => Ty::Slice(Box::new(type_expr_to_ty(&inner.0, module_short))),
        TypeExpr::Function {
            params,
            return_type,
        } => Ty::Function {
            params: params
                .iter()
                .map(|(te, _)| type_expr_to_ty(te, module_short))
                .collect(),
            ret: Box::new(type_expr_to_ty(&return_type.0, module_short)),
        },
        TypeExpr::Pointer {
            is_mutable,
            pointee,
        } => Ty::Pointer {
            is_mutable: *is_mutable,
            pointee: Box::new(type_expr_to_ty(&pointee.0, module_short)),
        },
        TypeExpr::TraitObject(bounds) => Ty::TraitObject {
            traits: bounds
                .iter()
                .map(|bound| crate::ty::TraitObjectBound {
                    trait_name: bound.name.clone(),
                    args: vec![],
                })
                .collect(),
        },
        TypeExpr::Infer => Ty::Error,
    }
}

/// Extract the C function name and argument count from a wrapper function's body.
///
/// Looks for a simple call expression like `hew_json_parse(s)` in the
/// function body and returns `(callee_name, arg_count)`.
fn extract_call_target(body: &Block) -> Option<(String, usize)> {
    // Check trailing expression first (most common case)
    if let Some(trailing) = &body.trailing_expr {
        return call_target_from_expr(&trailing.0);
    }

    // Check last statement
    if let Some((Stmt::Expression(expr) | Stmt::Return(Some(expr)), _)) = body.stmts.last() {
        return call_target_from_expr(&expr.0);
    }

    None
}

/// Extract the callee name and argument count from a call expression.
fn call_target_from_expr(expr: &Expr) -> Option<(String, usize)> {
    match expr {
        Expr::Call { function, args, .. } => {
            if let Expr::Identifier(name) = &function.0 {
                return Some((name.clone(), args.len()));
            }
            None
        }
        // Handle blocks that wrap a call (e.g. `{ hew_foo(x); }`)
        Expr::Block(block) => extract_call_target(block),
        _ => None,
    }
}

/// Extract handle method → C symbol mappings from an `impl` block.
///
/// For `impl FooMethods for Foo { fn bar(self: Foo) { hew_foo_bar(self); } }`,
/// produces `(("module.Foo", "bar"), "hew_foo_bar")`.
fn extract_handle_methods(impl_decl: &ImplDecl, module_short: &str, info: &mut ModuleInfo) {
    // Get the target type name
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
        // Skip the `self` parameter when matching — it's not part of the call
        if let Some((c_symbol, _arg_count)) = extract_call_target(&method.body) {
            info.handle_methods
                .push(((type_name.clone(), method.name.clone()), c_symbol));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    fn test_root() -> PathBuf {
        // Tests run from the workspace root
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .to_path_buf()
    }

    #[test]
    fn resolve_encoding_json() {
        let path = resolve_hew_path("std::encoding::json", &test_root());
        assert!(path.is_some(), "should find std/encoding/json/json.hew");
        assert!(
            path.unwrap().ends_with("std/encoding/json/json.hew"),
            "should use package-directory form"
        );
    }

    #[test]
    fn resolve_flat_fs() {
        let path = resolve_hew_path("std::fs", &test_root());
        assert!(path.is_some(), "should find std/fs.hew");
        assert!(
            path.unwrap().ends_with("std/fs.hew"),
            "should use flat form"
        );
    }

    #[test]
    fn load_json_module() {
        let info = load_module("std::encoding::json", &test_root());
        assert!(info.is_some(), "should load json module");
        let info = info.unwrap();

        // Should have extern functions
        assert!(
            !info.functions.is_empty(),
            "json module should have functions"
        );

        // Should have json.Value handle type
        assert!(
            info.handle_types.contains(&"json.Value".to_string()),
            "json module should declare json.Value handle type"
        );

        // Should have clean name mapping for "parse"
        let has_parse = info.clean_names.iter().any(|(clean, _)| clean == "parse");
        assert!(has_parse, "json module should have 'parse' clean name");
    }

    #[test]
    fn load_semaphore_module() {
        let info = load_module("std::semaphore", &test_root());
        assert!(info.is_some(), "should load semaphore module");
        let info = info.unwrap();

        // Should have semaphore.Semaphore handle type
        assert!(
            info.handle_types
                .contains(&"semaphore.Semaphore".to_string()),
            "semaphore module should declare Semaphore type"
        );

        // Should have handle methods
        let has_acquire = info
            .handle_methods
            .iter()
            .any(|((ty, method), _)| ty == "semaphore.Semaphore" && method == "acquire");
        assert!(
            has_acquire,
            "semaphore.Semaphore should have acquire method"
        );

        // Should have "new" clean name
        let has_new = info.clean_names.iter().any(|(clean, _)| clean == "new");
        assert!(has_new, "semaphore module should have 'new' clean name");
    }

    #[test]
    fn load_fs_module() {
        let info = load_module("std::fs", &test_root());
        assert!(info.is_some(), "should load fs module");
        let info = info.unwrap();
        assert!(
            !info.functions.is_empty(),
            "fs module should have extern functions"
        );
    }

    #[test]
    fn load_all_std_modules() {
        use crate::module_registry::ModuleRegistry;

        let root = test_root();
        let mut module_paths = Vec::new();
        discover_modules(&root.join("std"), &root, &mut module_paths);
        if root.join("ecosystem").exists() {
            discover_modules(&root.join("ecosystem"), &root, &mut module_paths);
        }

        assert!(
            !module_paths.is_empty(),
            "should discover modules under std/"
        );

        let mut registry = ModuleRegistry::new(vec![root]);
        for module_path in &module_paths {
            let result = registry.load(module_path);
            assert!(
                result.is_ok(),
                "should load module {module_path}: {:?}",
                result.err()
            );
        }

        assert!(
            module_paths.len() > 20,
            "should discover many modules, found {}",
            module_paths.len()
        );
    }

    /// Walk a directory tree and discover module paths from `.hew` files.
    ///
    /// Derives module paths from directory structure:
    /// - `std/encoding/json/` -> `std::encoding::json`
    /// - `std/fs.hew` -> `std::fs`
    fn discover_modules(
        dir: &std::path::Path,
        repo_root: &std::path::Path,
        paths: &mut Vec<String>,
    ) {
        let Ok(entries) = std::fs::read_dir(dir) else {
            return;
        };
        let mut sorted: Vec<_> = entries.filter_map(Result::ok).collect();
        sorted.sort_by_key(|e| e.file_name());

        let mut has_hew = false;
        for entry in &sorted {
            let path = entry.path();
            if path.is_dir() {
                discover_modules(&path, repo_root, paths);
            } else if path.extension().is_some_and(|e| e == "hew")
                && !path.file_name().is_some_and(|f| f == "builtins.hew")
            {
                has_hew = true;
            }
        }

        if has_hew {
            let Ok(rel) = dir.strip_prefix(repo_root) else {
                return;
            };
            let segments: Vec<&str> = rel
                .components()
                .map(|c| c.as_os_str().to_str().unwrap())
                .collect();

            if segments.len() >= 2 {
                // Subdirectory module: std/encoding/json → std::encoding::json
                paths.push(segments.join("::"));
            } else if segments.len() == 1 {
                // Root-level files: std/fs.hew → std::fs — each file is its own module
                for entry in &sorted {
                    let path = entry.path();
                    if path.extension().is_some_and(|e| e == "hew")
                        && !path.file_name().is_some_and(|f| f == "builtins.hew")
                    {
                        let stem = path.file_stem().unwrap().to_str().unwrap();
                        paths.push(format!("{}::{stem}", segments[0]));
                    }
                }
            }
        }
    }

    #[test]
    fn type_mapping_primitives() {
        let info = load_module("std::encoding::json", &test_root()).unwrap();

        // json module should have functions with String params/returns
        let has_string_fn = info
            .functions
            .iter()
            .any(|(_, params, _)| params.contains(&Ty::String));
        assert!(
            has_string_fn,
            "json module should have String-typed functions"
        );
    }

    #[test]
    fn wrapper_fns_extracted() {
        let info = load_module("std::misc::log", &test_root()).unwrap();

        assert!(
            !info.wrapper_fns.is_empty(),
            "log module should have wrapper fns"
        );

        // `pub fn setup()` takes no params and returns Unit
        let setup = info.wrapper_fns.iter().find(|(name, _, _)| name == "setup");
        assert!(setup.is_some(), "log module should have 'setup' wrapper fn");
        let (_, params, ret) = setup.unwrap();
        assert!(params.is_empty(), "setup() takes no parameters");
        assert_eq!(*ret, Ty::Unit, "setup() returns Unit");

        // `pub fn setup_level(level: i32)` takes one i32 param
        let setup_level = info
            .wrapper_fns
            .iter()
            .find(|(name, _, _)| name == "setup_level");
        assert!(
            setup_level.is_some(),
            "log module should have 'setup_level' wrapper fn"
        );
        let (_, params, _) = setup_level.unwrap();
        assert_eq!(params.len(), 1);
        assert_eq!(params[0], Ty::I32);
    }

    #[test]
    fn drop_types_extracted() {
        let info = load_module("std::net::http", &test_root()).unwrap();

        assert!(
            !info.drop_types.is_empty(),
            "http module should have drop types"
        );

        assert!(
            info.drop_types.contains(&"http.Request".to_string()),
            "http.Request should be a drop type, got: {:?}",
            info.drop_types
        );
    }

    #[test]
    fn struct_types_not_in_handle_types() {
        // semver.Version has struct fields — it should NOT be a handle type
        let info = load_module("std::text::semver", &test_root()).unwrap();

        assert!(
            !info.handle_types.contains(&"semver.Version".to_string()),
            "Version with struct fields should not be a handle type, got: {:?}",
            info.handle_types
        );
    }
}
