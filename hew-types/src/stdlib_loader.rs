//! AST-based stdlib module loader.
//!
//! Parses `.hew` files and extracts type information: function signatures,
//! clean name mappings, handle types, and handle method mappings.

use std::path::Path;

use hew_parser::ast::{Block, Expr, ExternFnDecl, ImplDecl, Item, Stmt, TypeExpr};
use hew_parser::parse;

use crate::ty::Ty;

/// All type information extracted from a single `.hew` module file.
#[derive(Debug, Clone)]
pub struct ModuleInfo {
    /// C function signatures: (c_name, param_types, return_type).
    pub functions: Vec<(String, Vec<Ty>, Ty)>,
    /// Clean name mappings: (user_name, c_symbol).
    pub clean_names: Vec<(String, String)>,
    /// Handle type names, e.g. `"json.Value"`.
    pub handle_types: Vec<String>,
    /// Handle method mappings: ((type_name, method_name), c_symbol).
    pub handle_methods: Vec<((String, String), String)>,
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
    };

    // Collect extern "C" function signatures
    for (item, _span) in &program.items {
        if let Item::ExternBlock(block) = item {
            for func in &block.functions {
                let (params, ret) = extern_fn_sig(func, module_short);
                info.functions.push((func.name.clone(), params, ret));
            }
        }
    }

    // Collect handle types from `type` declarations
    for (item, _span) in &program.items {
        if let Item::TypeDecl(td) = item {
            let qualified = format!("{module_short}.{}", td.name);
            info.handle_types.push(qualified);
        }
    }

    // Collect clean name → C symbol mappings from `pub fn` declarations
    for (item, _span) in &program.items {
        if let Item::Function(fn_decl) = item {
            if fn_decl.is_pub {
                if let Some(c_symbol) = extract_call_target(&fn_decl.body) {
                    info.clean_names.push((fn_decl.name.clone(), c_symbol));
                }
            }
        }
    }

    // Collect handle method → C symbol mappings from `impl` blocks
    for (item, _span) in &program.items {
        if let Item::Impl(impl_decl) = item {
            extract_handle_methods(impl_decl, module_short, &mut info);
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

/// Convert a Hew type expression to the type checker's `Ty`.
fn type_expr_to_ty(texpr: &TypeExpr, module_short: &str) -> Ty {
    match texpr {
        TypeExpr::Named { name, type_args } => {
            match name.as_str() {
                "String" => Ty::String,
                "i8" => Ty::I8,
                "i16" => Ty::I16,
                "i32" => Ty::I32,
                "i64" => Ty::I64,
                "u8" => Ty::U8,
                "u16" => Ty::U16,
                "u32" => Ty::U32,
                "u64" => Ty::U64,
                "f32" => Ty::F32,
                "f64" => Ty::F64,
                "bool" => Ty::Bool,
                "char" => Ty::Char,
                "bytes" => Ty::Named {
                    name: "bytes".to_string(),
                    args: vec![],
                },
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
        TypeExpr::Option(inner) => Ty::Option(Box::new(type_expr_to_ty(&inner.0, module_short))),
        TypeExpr::Result { ok, err } => Ty::Result {
            ok: Box::new(type_expr_to_ty(&ok.0, module_short)),
            err: Box::new(type_expr_to_ty(&err.0, module_short)),
        },
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
        TypeExpr::TraitObject(bound) => Ty::TraitObject {
            trait_name: bound.name.clone(),
            args: vec![],
        },
        TypeExpr::Infer => Ty::Error,
    }
}

/// Extract the C function name from a wrapper function's body.
///
/// Looks for a simple call expression like `hew_json_parse(s)` in the
/// function body and returns the callee name.
fn extract_call_target(body: &Block) -> Option<String> {
    // Check trailing expression first (most common case)
    if let Some(trailing) = &body.trailing_expr {
        return call_target_from_expr(&trailing.0);
    }

    // Check last statement
    if let Some((stmt, _)) = body.stmts.last() {
        match stmt {
            Stmt::Expression(expr) => return call_target_from_expr(&expr.0),
            Stmt::Return(Some(expr)) => return call_target_from_expr(&expr.0),
            _ => {}
        }
    }

    None
}

/// Extract the callee name from a call expression.
fn call_target_from_expr(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Call { function, .. } => {
            if let Expr::Identifier(name) = &function.0 {
                return Some(name.clone());
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
        if let Some(c_symbol) = extract_call_target(&method.body) {
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
        // Modules with extern "C" FFI — must have function signatures
        let ffi_modules = [
            "std::fs",
            "std::os",
            "std::net",
            "std::process",
            "std::string",
            "std::encoding::json",
            "std::misc::log",
            "std::misc::uuid",
            "std::time::datetime",
            "std::net::url",
            "std::path",
            "std::semaphore",
            "std::stream",
            "std::encoding::base64",
            "std::crypto::crypto",
            "std::encoding::compress",
            "std::encoding::yaml",
            "std::encoding::toml",
            "std::encoding::csv",
            "std::encoding::msgpack",
            "std::encoding::protobuf",
            "std::encoding::markdown",
            "std::crypto::jwt",
            "std::crypto::password",
            "std::crypto::encrypt",
            "std::net::http",
            "std::net::websocket",
            "std::net::smtp",
            "std::net::ipnet",
            "std::time::cron",
            "std::text::semver",
            "std::text::regex",
        ];

        let root = test_root();
        for module in &ffi_modules {
            let info = load_module(module, &root);
            assert!(info.is_some(), "should load module {module}");
            let info = info.unwrap();
            assert!(
                !info.functions.is_empty(),
                "module {module} should have functions"
            );
        }

        // Pure-Hew modules (no extern "C") — must load but have no FFI functions
        let pure_modules = [
            "std::encoding::hex",
            "std::encoding::wire",
            "std::net::mime",
        ];

        for module in &pure_modules {
            let info = load_module(module, &root);
            assert!(info.is_some(), "should load pure module {module}");
        }
    }

    #[test]
    fn type_mapping_primitives() {
        let info = load_module("std::encoding::base64", &test_root()).unwrap();

        // base64 module should have functions with String params/returns
        let has_string_fn = info
            .functions
            .iter()
            .any(|(_, params, _)| params.iter().any(|p| *p == Ty::String));
        assert!(
            has_string_fn,
            "base64 module should have String-typed functions"
        );
    }
}
