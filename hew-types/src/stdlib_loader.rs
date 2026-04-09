//! AST-based stdlib module loader.
//!
//! Parses `.hew` files and extracts type information: function signatures,
//! clean name mappings, handle types, and handle method mappings.

use std::path::Path;

use hew_parser::ast::{
    Block, Expr, ExternFnDecl, FnDecl, ImplDecl, Item, Stmt, TypeBodyItem, TypeDeclKind, TypeExpr,
};
use hew_parser::parse;

use crate::check::admissibility::signature_uses_unsupported_type;
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
    /// Drop function for each type with `impl Drop`: (`qualified_type_name`, `c_func_name`).
    ///
    /// Extracted from the body of `fn drop` inside `impl Drop for T` blocks.
    /// Only populated when the drop body is a single direct C call.
    pub drop_funcs: Vec<(String, String)>,
    /// Public / extern signatures that used unsupported slice annotations.
    ///
    /// The loader is used for registry-loaded modules (stdlib, ecosystem, and
    /// module-path imports). MLIR still has no slice lowering, so these
    /// signatures must be rejected before they are registered with the checker.
    pub unsupported_type_signatures: Vec<String>,
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
        drop_funcs: Vec::new(),
        unsupported_type_signatures: Vec::new(),
    };

    for (item, _span) in &program.items {
        match item {
            Item::ExternBlock(block) => {
                for func in &block.functions {
                    let (params, ret) = extern_fn_sig(func, module_short);
                    if signature_uses_unsupported_type(&params, &ret) {
                        info.unsupported_type_signatures
                            .push(format!("extern function `{}`", func.name));
                    }
                    info.functions.push((func.name.clone(), params, ret));
                }
            }
            // Only fieldless structs are opaque handles.
            // Enums (including fieldless enums) are real value types and must
            // not be lowered as handles when imported from stdlib Hew modules.
            Item::TypeDecl(td) => {
                let has_fields = td
                    .body
                    .iter()
                    .any(|b| matches!(b, TypeBodyItem::Field { .. }));
                if td.kind == TypeDeclKind::Struct && !has_fields {
                    let qualified = format!("{module_short}.{}", td.name);
                    info.handle_types.push(qualified);
                }
            }
            Item::Function(fn_decl) if fn_decl.visibility.is_pub() => {
                // Extract wrapper function's own signature
                let (params, ret) = wrapper_fn_sig(fn_decl, module_short);
                if signature_uses_unsupported_type(&params, &ret) {
                    info.unsupported_type_signatures
                        .push(format!("public function `{}`", fn_decl.name));
                }
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
                // Detect `impl Drop for T` — collect as drop types and extract
                // the C drop function name from the `fn drop` body.
                if let Some(ref tb) = impl_decl.trait_bound {
                    if tb.name == "Drop" {
                        if let TypeExpr::Named { ref name, .. } = impl_decl.target_type.0 {
                            let qualified = if name.contains('.') {
                                name.clone()
                            } else {
                                format!("{module_short}.{name}")
                            };
                            info.drop_types.push(qualified.clone());
                            // Extract the C drop function from `fn drop { ... }`.
                            if let Some(drop_method) =
                                impl_decl.methods.iter().find(|m| m.name == "drop")
                            {
                                if let Some((c_func, _)) = extract_call_target(&drop_method.body) {
                                    info.drop_funcs.push((qualified, c_func));
                                }
                            }
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
            // Primitive types never take type args; delegate entirely to the
            // canonical alias table so aliases like `str`, `uint`, `float`,
            // `Float`, `byte`, `Bool`, `Char`, `Bytes`, `Duration` are
            // recognised instead of falling through to module-qualification.
            let has_args = type_args.as_ref().is_some_and(|a| !a.is_empty());
            if !has_args {
                if let Some(prim) = Ty::from_name(name.as_str()) {
                    return prim;
                }
            }
            match name.as_str() {
                // Option<T> → Ty::option() helper
                "Option" => {
                    if let Some(args) = type_args {
                        if let Some(first) = args.first() {
                            return Ty::option(type_expr_to_ty(&first.0, module_short));
                        }
                    }
                    Ty::normalize_named("Option".to_string(), vec![])
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
                    Ty::normalize_named("Result".to_string(), vec![])
                }
                // Canonical named builtins — do NOT module-qualify.
                builtin if Ty::is_named_builtin(builtin) => {
                    let args = type_args
                        .as_ref()
                        .map(|a| {
                            a.iter()
                                .map(|(te, _)| type_expr_to_ty(te, module_short))
                                .collect()
                        })
                        .unwrap_or_default();
                    Ty::normalize_named(builtin.to_string(), args)
                }
                // Qualified handle type like "json.Value"
                n if n.contains('.') => Ty::normalize_named(
                    n.to_string(),
                    type_args
                        .as_ref()
                        .map(|args| {
                            args.iter()
                                .map(|(te, _)| type_expr_to_ty(te, module_short))
                                .collect()
                        })
                        .unwrap_or_default(),
                ),
                // Unqualified type name — qualify with module short name
                other => Ty::normalize_named(
                    format!("{module_short}.{other}"),
                    type_args
                        .as_ref()
                        .map(|args| {
                            args.iter()
                                .map(|(te, _)| type_expr_to_ty(te, module_short))
                                .collect()
                        })
                        .unwrap_or_default(),
                ),
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
        TypeExpr::Slice(_) | TypeExpr::Infer => Ty::Error,
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
    }
}

/// Extract the C function name and argument count from a wrapper function's body.
///
/// Looks for a simple call expression like `hew_json_parse(s)` in the
/// function body and returns `(callee_name, arg_count)`.
///
/// Only matches truly trivial pass-through bodies: a single call expression
/// with no preceding statements. Bodies like `let ext = f(x); g(ext)` are
/// NOT trivial — the prior statements transform the data before the call.
fn extract_call_target(body: &Block) -> Option<(String, usize)> {
    // Check trailing expression — but only if there are no preceding
    // statements. A body with statements before the trailing expr is doing
    // real work, not just forwarding to a C function.
    if let Some(trailing) = &body.trailing_expr {
        if body.stmts.is_empty() {
            return call_target_from_expr(&trailing.0);
        }
        return None;
    }

    // Check last statement — only when it's the sole statement.
    if body.stmts.len() == 1 {
        if let Some((Stmt::Expression(expr) | Stmt::Return(Some(expr)), _)) = body.stmts.last() {
            return call_target_from_expr(&expr.0);
        }
    }

    None
}

/// Extract the callee name and argument count from a call expression.
fn call_target_from_expr(expr: &Expr) -> Option<(String, usize)> {
    match expr {
        Expr::Call { function, args, .. } => {
            if let Expr::Identifier(name) = &function.0 {
                // Only treat as a simple C shim if every argument is a direct
                // identifier (i.e. a parameter forwarded unchanged). A compound
                // body such as `hew_bytes_to_string(hew_tcp_read(conn))` must
                // NOT be registered as a single-step C pass-through, because
                // the enricher would then rewrite `conn.read_string()` to
                // `hew_bytes_to_string(conn)` — dropping the inner call and
                // passing an i32 fd where bytes are expected.
                let all_direct = args
                    .iter()
                    .all(|arg| matches!(&arg.expr().0, Expr::Identifier(_)));
                if all_direct {
                    return Some((name.clone(), args.len()));
                }
                return None;
            }
            None
        }
        // Handle blocks or unsafe blocks that wrap a call
        Expr::Block(block) | Expr::Unsafe(block) => extract_call_target(block),
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
        assert!(
            info.drop_types.contains(&"json.Value".to_string()),
            "json module should mark json.Value as a drop type"
        );
        assert_eq!(
            info.drop_funcs
                .iter()
                .find(|(ty, _)| ty == "json.Value")
                .map(|(_, func)| func.as_str()),
            Some("hew_json_free"),
            "json.Value drop func should be hew_json_free"
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
    fn load_net_module_skips_nontrivial_handle_wrappers() {
        let info = load_module("std::net", &test_root());
        assert!(info.is_some(), "should load net module");
        let info = info.unwrap();

        let has_read = info.handle_methods.iter().any(|((ty, method), sym)| {
            ty == "net.Connection" && method == "read" && sym == "hew_tcp_read"
        });
        assert!(
            has_read,
            "net.Connection.read should rewrite to hew_tcp_read"
        );

        let rewrites_read_string = info.handle_methods.iter().any(|((ty, method), sym)| {
            ty == "net.Connection" && method == "read_string" && sym == "hew_bytes_to_string"
        });
        assert!(
            !rewrites_read_string,
            "net.Connection.read_string should remain a Hew wrapper, not alias hew_bytes_to_string"
        );

        let rewrites_write_string = info.handle_methods.iter().any(|((ty, method), sym)| {
            ty == "net.Connection" && method == "write_string" && sym == "hew_tcp_write"
        });
        assert!(
            !rewrites_write_string,
            "net.Connection.write_string should remain a Hew wrapper, not alias hew_tcp_write"
        );
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
        sorted.sort_by_key(std::fs::DirEntry::file_name);

        let mut has_hew = false;
        for entry in &sorted {
            let path = entry.path();
            if path.is_dir() {
                discover_modules(&path, repo_root, paths);
            } else if path.extension().is_some_and(|e| e == "hew")
                && path.file_name().is_none_or(|f| f != "builtins.hew")
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
                        && path.file_name().is_none_or(|f| f != "builtins.hew")
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
    fn channel_signatures_use_canonical_builtin_names() {
        let info = load_module("std::channel", &test_root()).unwrap();

        let clone_sig = info
            .functions
            .iter()
            .find(|(name, _, _)| name == "hew_channel_sender_clone")
            .expect("channel module should expose sender clone");
        assert_eq!(
            clone_sig.1,
            vec![Ty::normalize_named("Sender".to_string(), vec![])]
        );
        assert_eq!(
            clone_sig.2,
            Ty::normalize_named("Sender".to_string(), vec![])
        );

        let new_sig = info
            .wrapper_fns
            .iter()
            .find(|(name, _, _)| name == "new")
            .expect("channel module should expose new()");
        assert_eq!(
            new_sig.2,
            Ty::Tuple(vec![
                Ty::normalize_named("Sender".to_string(), vec![]),
                Ty::normalize_named("Receiver".to_string(), vec![]),
            ])
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

        assert!(
            info.drop_types.contains(&"http.Server".to_string()),
            "http.Server should be a drop type, got: {:?}",
            info.drop_types
        );
    }

    #[test]
    fn drop_funcs_extracted() {
        let info = load_module("std::net::http", &test_root()).unwrap();

        let request_drop = info.drop_funcs.iter().find(|(ty, _)| ty == "http.Request");
        assert!(
            request_drop.is_some(),
            "http.Request should have a drop func, got: {:?}",
            info.drop_funcs
        );
        assert_eq!(
            request_drop.unwrap().1,
            "hew_http_request_free",
            "http.Request drop func should be hew_http_request_free"
        );

        let server_drop = info.drop_funcs.iter().find(|(ty, _)| ty == "http.Server");
        assert!(
            server_drop.is_some(),
            "http.Server should have a drop func, got: {:?}",
            info.drop_funcs
        );
        assert_eq!(
            server_drop.unwrap().1,
            "hew_http_server_close",
            "http.Server drop func should be hew_http_server_close"
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

    #[test]
    fn enum_types_not_in_handle_types() {
        let info = load_module("std::fs", &test_root()).unwrap();

        assert!(
            !info.handle_types.contains(&"fs.IoError".to_string()),
            "fs.IoError enum should not be a handle type, got: {:?}",
            info.handle_types
        );
    }

    /// Directly exercises `type_expr_to_ty` for every alias that was previously
    /// missing from the partial local table and would have been mis-qualified.
    #[test]
    fn primitive_aliases_delegate_to_from_name() {
        use hew_parser::ast::TypeExpr;

        let module = "test";
        let cases: &[(&str, Ty)] = &[
            // aliases present in Ty::from_name but absent from the old local table
            ("str", Ty::String),
            ("uint", Ty::U64),
            ("usize", Ty::U64),
            ("float", Ty::F64),
            ("Float", Ty::F64),
            ("byte", Ty::U8),
            ("Bool", Ty::Bool),
            ("Char", Ty::Char),
            ("Bytes", Ty::Bytes),
            ("Duration", Ty::Duration),
            // canonical names that were already handled — must still work
            ("string", Ty::String),
            ("String", Ty::String),
            ("i64", Ty::I64),
            ("int", Ty::I64),
            ("Int", Ty::I64),
            ("isize", Ty::I64),
            ("u64", Ty::U64),
            ("f64", Ty::F64),
            ("bool", Ty::Bool),
            ("char", Ty::Char),
            ("bytes", Ty::Bytes),
            ("duration", Ty::Duration),
        ];

        for (alias, expected) in cases {
            let texpr = TypeExpr::Named {
                name: alias.to_string(),
                type_args: None,
            };
            let got = type_expr_to_ty(&texpr, module);
            assert_eq!(
                got, *expected,
                "alias `{alias}` should resolve to {expected:?}, got {got:?}"
            );
        }
    }

    /// Aliases must NOT be module-qualified: `str` → `Ty::String`, not
    /// `Ty::Named("test.str")`.
    #[test]
    fn primitive_aliases_are_not_module_qualified() {
        use hew_parser::ast::TypeExpr;

        let aliases = [
            "str", "uint", "usize", "isize", "float", "Float", "byte", "Bool", "Char", "Bytes",
            "Duration",
        ];
        for alias in aliases {
            let texpr = TypeExpr::Named {
                name: alias.to_string(),
                type_args: None,
            };
            let got = type_expr_to_ty(&texpr, "mymod");
            assert!(
                !matches!(got, Ty::Named { .. }),
                "alias `{alias}` must not become a Named type (was mis-qualified before fix), got {got:?}"
            );
        }
    }

    #[test]
    fn slice_type_exprs_fail_closed_in_loader() {
        use hew_parser::ast::TypeExpr;

        let texpr = TypeExpr::Slice(Box::new((
            TypeExpr::Named {
                name: "i32".to_string(),
                type_args: None,
            },
            0..0,
        )));

        assert_eq!(
            type_expr_to_ty(&texpr, "mymod"),
            Ty::Error,
            "slice annotations must not become Ty::Slice in registry-loaded signatures"
        );
    }

    #[test]
    fn module_info_tracks_public_slice_signatures_as_unsupported() {
        let result = parse("pub fn take(xs: [i32]) {}\n");
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );

        let info = extract_module_info(&result.program, "widgets");
        assert_eq!(
            info.unsupported_type_signatures,
            vec!["public function `take`".to_string()],
            "slice-bearing public signatures should be marked unsupported"
        );
        assert_eq!(
            info.wrapper_fns[0].1,
            vec![Ty::Error],
            "unsupported slice parameter should fail closed inside the loaded signature"
        );
    }

    #[test]
    fn non_trivial_wrapper_uses_identity_mapping() {
        // mime.from_path calls extract_extension() then from_ext() — it is
        // NOT a trivial pass-through and must use identity mapping so the
        // full wrapper body is compiled and called.
        let info = load_module("std::net::mime", &test_root()).unwrap();

        let mapping = info
            .clean_names
            .iter()
            .find(|(clean, _)| clean == "from_path");
        assert!(
            mapping.is_some(),
            "mime module should have clean_name entry for from_path"
        );
        let (_, c_target) = mapping.unwrap();
        assert_eq!(
            c_target, "from_path",
            "from_path must be identity-mapped (not forwarded to from_ext)"
        );
    }
}
