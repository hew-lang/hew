//! AST-based stdlib module loader.
//!
//! Parses `.hew` files and extracts type information: function signatures,
//! clean name mappings, handle types, and handle method mappings.

use std::collections::{HashMap, HashSet};
use std::path::Path;

use hew_parser::ast::{
    Block, Expr, ExternFnDecl, FnDecl, ImplDecl, Item, ResourceMarker, Stmt, TypeBodyItem,
    TypeDeclKind, TypeExpr,
};
use hew_parser::parse;

use crate::check::admissibility::signature_contains_error_type;
use crate::module_registry::ModuleError;
use crate::ty::Ty;

/// A C function signature extracted from an `extern` block.
#[derive(Debug, Clone)]
pub struct CFunction {
    /// The C symbol name (e.g. `"hew_json_parse"`).
    pub name: String,
    /// Parameter types.
    pub params: Vec<Ty>,
    /// Return type.
    pub return_type: Ty,
}

/// A wrapper `pub fn` signature from a stdlib module.
#[derive(Debug, Clone)]
pub struct WrapperFn {
    /// The Hew function name (e.g. `"parse"`).
    pub name: String,
    /// Function type parameters.
    pub type_params: Vec<String>,
    /// Trait bounds keyed by type parameter name.
    pub type_param_bounds: HashMap<String, Vec<String>>,
    /// Parameter types.
    pub params: Vec<Ty>,
    /// Return type.
    pub return_type: Ty,
}

/// A handle method mapping extracted from an `impl` block.
#[derive(Debug, Clone)]
pub struct HandleMethod {
    /// Fully-qualified handle type name (e.g. `"json.Value"`).
    pub type_name: String,
    /// Method name (e.g. `"get_string"`).
    pub method_name: String,
    /// The C symbol the method resolves to (e.g. `"hew_json_value_get_string"`).
    pub c_symbol: String,
    /// Parameter types (excluding `self`).
    pub params: Vec<Ty>,
    /// Return type.
    pub return_type: Ty,
}

/// All type information extracted from a single `.hew` module file.
#[derive(Debug, Clone)]
pub struct ModuleInfo {
    /// C function signatures from `extern` blocks.
    pub functions: Vec<CFunction>,
    /// Clean name mappings: (`user_name`, `c_symbol`).
    pub clean_names: Vec<(String, String)>,
    /// Handle type names, e.g. `"json.Value"`.
    pub handle_types: Vec<String>,
    /// Handle method mappings extracted from `impl` blocks.
    pub handle_methods: Vec<HandleMethod>,
    /// Wrapper `pub fn` signatures.
    pub wrapper_fns: Vec<WrapperFn>,
    /// Types with deterministic cleanup — move-only, not Copy.
    pub drop_types: Vec<String>,
    /// Cleanup function for each deterministic cleanup type:
    /// (`qualified_type_name`, `c_func_name`).
    ///
    /// Extracted from `#[resource]` `fn close` bodies (and legacy stdlib
    /// `impl Drop` bodies while old fixtures are being removed).
    /// Only populated when the drop body is a single direct C call.
    pub drop_funcs: Vec<(String, String)>,
    /// Public / extern signatures that used unsupported slice annotations.
    ///
    /// The loader is used for registry-loaded modules (stdlib, ecosystem, and
    /// module-path imports). The backend still has no slice lowering, so these
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
    load_module_checked(module_path, root).ok().flatten()
}

/// Load type information for a module, preserving parse errors for callers
/// that need to distinguish malformed modules from missing ones.
pub(crate) fn load_module_checked(
    module_path: &str,
    root: &Path,
) -> Result<Option<ModuleInfo>, ModuleError> {
    let Some(hew_path) = resolve_hew_path(module_path, root) else {
        return Ok(None);
    };
    let Ok(source) = std::fs::read_to_string(&hew_path) else {
        return Ok(None);
    };
    let result = parse(&source);
    if let Some(parse_error) = result.errors.first() {
        let (line, column) = offset_to_line_col(&source, parse_error.span.start);
        return Err(ModuleError::ParseError {
            module_path: module_path.to_string(),
            file_path: hew_path,
            line,
            column,
            message: parse_error.message.clone(),
        });
    }

    let module_short = module_short_name(module_path);
    Ok(Some(extract_module_info(&result.program, &module_short)))
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

fn offset_to_line_col(source: &str, offset: usize) -> (usize, usize) {
    let offset = offset.min(source.len());
    let mut line = 1;
    let mut col = 1;
    let bytes = source.as_bytes();

    for (i, ch) in source.char_indices() {
        if i >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 1;
        } else if ch == '\r' && bytes.get(i + 1) == Some(&b'\n') {
        } else {
            col += 1;
        }
    }

    (line, col)
}

/// Collect every extern (C-ABI) function name declared in the module's
/// `extern` blocks. Used to decide whether a pub wrapper's inner callee is a
/// real link-time C symbol (collapse permitted) or a same-module Hew helper
/// (identity mapping required so the wrapper itself is compiled and callable).
fn collect_extern_fn_names(program: &hew_parser::ast::Program) -> HashSet<String> {
    program
        .items
        .iter()
        .filter_map(|(item, _)| {
            if let Item::ExternBlock(block) = item {
                Some(block.functions.iter().map(|f| f.name.clone()))
            } else {
                None
            }
        })
        .flatten()
        .collect()
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

    // Pre-collect every extern (C-ABI) function name declared in the module.
    // A pub wrapper may only be collapsed to its inner callee — the trivial
    // pass-through optimisation in `clean_names` — when that callee is an extern
    // C symbol that exists at link time. A wrapper forwarding to a same-module
    // *Hew* helper (e.g. `pub fn encode(s) { encode_impl(s) }`) must instead map
    // to itself so the wrapper is compiled and called by its qualified name;
    // collapsing to the private helper's bare name yields an unresolvable callee
    // at the importer's call site.
    let extern_fn_names = collect_extern_fn_names(program);
    let resource_type_names = collect_resource_type_names(program, module_short);
    let wrapper_resource_fields = collect_wrapper_resource_fields(program, module_short);

    for (item, _span) in &program.items {
        match item {
            Item::ExternBlock(block) => {
                for func in &block.functions {
                    let (params, return_type) = extern_fn_sig(func, module_short);
                    if signature_contains_error_type(&params, &return_type) {
                        info.unsupported_type_signatures
                            .push(format!("extern function `{}`", func.name));
                    }
                    info.functions.push(CFunction {
                        name: func.name.clone(),
                        params,
                        return_type,
                    });
                }
            }
            // Only fieldless structs are opaque handles.
            // Enums (including fieldless enums) are real value types and must
            // not be lowered as handles when imported from stdlib Hew modules.
            Item::TypeDecl(td) => {
                let qualified = format!("{module_short}.{}", td.name);
                if td.resource_marker == ResourceMarker::Resource {
                    info.drop_types.push(qualified.clone());
                }
                let has_fields = td
                    .body
                    .iter()
                    .any(|b| matches!(b, TypeBodyItem::Field { .. }));
                if td.kind == TypeDeclKind::Struct && !has_fields {
                    info.handle_types.push(qualified);
                }
            }
            Item::Function(fn_decl) if fn_decl.visibility.is_pub() => {
                // Extract wrapper function's own signature
                let (type_params, type_param_bounds, params, return_type) =
                    wrapper_fn_sig(fn_decl, module_short);
                if signature_contains_error_type(&params, &return_type) {
                    info.unsupported_type_signatures
                        .push(format!("public function `{}`", fn_decl.name));
                }
                info.wrapper_fns.push(WrapperFn {
                    name: fn_decl.name.clone(),
                    type_params,
                    type_param_bounds,
                    params,
                    return_type,
                });

                // Clean name mapping: use the inner call target only for trivial
                // pass-through wrappers that forward to an extern C symbol with
                // the same arity. Non-trivial wrappers (different arg count, e.g.
                // `setup()` calling `hew_log_set_level(2)`) AND wrappers that
                // forward to a same-module Hew helper (e.g. `encode()` calling
                // `encode_impl()`) use identity mapping so the wrapper Hew
                // function is compiled and called by its qualified name. A Hew
                // helper is private to the module: collapsing the public wrapper
                // onto it produces a callee the importer cannot resolve.
                let c_target = if let Some((target, call_arg_count)) =
                    extract_call_target(&fn_decl.body)
                {
                    if call_arg_count == fn_decl.params.len() && extern_fn_names.contains(&target) {
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
                extract_impl_info(
                    impl_decl,
                    module_short,
                    &mut info,
                    &resource_type_names,
                    &wrapper_resource_fields,
                    &extern_fn_names,
                );
            }
            _ => {}
        }
    }

    info
}

/// Collect the fully-qualified names of every `#[resource]` type declared in
/// the module (e.g. `process.Child`). Used to recognise inherent `close`
/// methods on resource handles when scanning `impl` blocks.
fn collect_resource_type_names(
    program: &hew_parser::ast::Program,
    module_short: &str,
) -> HashSet<String> {
    program
        .items
        .iter()
        .filter_map(|(item, _)| {
            if let Item::TypeDecl(td) = item {
                (td.resource_marker == ResourceMarker::Resource)
                    .then(|| format!("{module_short}.{}", td.name))
            } else {
                None
            }
        })
        .collect()
}

/// Map every `#[resource]` type that carries fields (e.g.
/// `regex.Pattern { handle: PatternHandle }`) — a handle *wrapper*, as opposed to
/// a fieldless `#[resource] #[opaque]` handle (`process.Child`) — to its declared
/// field names. A wrapper is not itself a handle type, so its methods forward the
/// inner handle field and are safe to register in the handle-method table; the
/// field-name set constrains which `self.<field>` accesses count as a forward.
fn collect_wrapper_resource_fields(
    program: &hew_parser::ast::Program,
    module_short: &str,
) -> HashMap<String, HashSet<String>> {
    program
        .items
        .iter()
        .filter_map(|(item, _)| {
            let Item::TypeDecl(td) = item else {
                return None;
            };
            let field_names: HashSet<String> = td
                .body
                .iter()
                .filter_map(|b| match b {
                    TypeBodyItem::Field { name, .. } => Some(name.clone()),
                    _ => None,
                })
                .collect();
            (td.resource_marker == ResourceMarker::Resource && !field_names.is_empty())
                .then(|| (format!("{module_short}.{}", td.name), field_names))
        })
        .collect()
}

/// Resolve an `impl` block's target type to its fully-qualified name,
/// prefixing the module short name when the target is unqualified.
fn qualified_impl_type_name(impl_decl: &ImplDecl, module_short: &str) -> Option<String> {
    match &impl_decl.target_type.0 {
        TypeExpr::Named { name, .. } => {
            if name.contains('.') {
                Some(name.clone())
            } else {
                Some(format!("{module_short}.{name}"))
            }
        }
        _ => None,
    }
}

/// Extract resource/drop metadata from an `impl` block: inherent `close`
/// methods on `#[resource]` handles and the legacy `impl Drop` fallback,
/// then delegate handle-method extraction.
fn extract_impl_info(
    impl_decl: &ImplDecl,
    module_short: &str,
    info: &mut ModuleInfo,
    resource_type_names: &HashSet<String>,
    wrapper_resource_fields: &HashMap<String, HashSet<String>>,
    extern_fn_names: &HashSet<String>,
) {
    let impl_type_name = qualified_impl_type_name(impl_decl, module_short);
    // Detect `#[resource] type T` + `impl T { fn close(...) { ... } }`:
    // collect the C cleanup function for old registry consumers that
    // still query the drop-func table for owned handles.
    if impl_decl.trait_bound.is_none() {
        if let Some(qualified) = impl_type_name.as_ref() {
            if resource_type_names.contains(qualified) {
                if let Some(close_method) = impl_decl.methods.iter().find(|m| m.name == "close") {
                    if let Some((c_func, _)) = extract_call_target(&close_method.body) {
                        info.drop_funcs.push((qualified.clone(), c_func));
                    }
                }
            }
        }
    }
    // Legacy stdlib fallback: detect `impl Drop for T` and extract
    // the C drop function name from the `fn drop` body. User
    // programs are rejected by the checker before codegen; this path
    // only keeps older loader tests honest while stdlib migrates.
    if let Some(ref tb) = impl_decl.trait_bound {
        if tb.name == "Drop" {
            if let Some(qualified) = impl_type_name.as_ref() {
                info.drop_types.push(qualified.clone());
                // Extract the C drop function from `fn drop { ... }`.
                if let Some(drop_method) = impl_decl.methods.iter().find(|m| m.name == "drop") {
                    if let Some((c_func, _)) = extract_call_target(&drop_method.body) {
                        info.drop_funcs.push((qualified.clone(), c_func));
                    }
                }
            }
        }
    }
    extract_handle_methods(
        impl_decl,
        module_short,
        info,
        resource_type_names,
        wrapper_resource_fields,
        extern_fn_names,
    );
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
fn wrapper_fn_sig(
    func: &FnDecl,
    module_short: &str,
) -> (Vec<String>, HashMap<String, Vec<String>>, Vec<Ty>, Ty) {
    let (type_params, type_param_bounds) = wrapper_fn_type_params(func);
    let type_param_names: HashSet<String> = type_params.iter().cloned().collect();
    let params: Vec<Ty> = func
        .params
        .iter()
        .map(|p| type_expr_to_ty_with_params(&p.ty.0, module_short, &type_param_names))
        .collect();

    let ret = func.return_type.as_ref().map_or(Ty::Unit, |rt| {
        type_expr_to_ty_with_params(&rt.0, module_short, &type_param_names)
    });

    (type_params, type_param_bounds, params, ret)
}

fn wrapper_fn_type_params(func: &FnDecl) -> (Vec<String>, HashMap<String, Vec<String>>) {
    let mut type_params = Vec::new();
    let mut bounds = HashMap::new();
    if let Some(params) = &func.type_params {
        for param in params {
            type_params.push(param.name.clone());
            bounds.insert(
                param.name.clone(),
                param
                    .bounds
                    .iter()
                    .map(|bound| bound.name.clone())
                    .collect(),
            );
        }
    }
    let type_param_names: HashSet<String> = type_params.iter().cloned().collect();
    if let Some(where_clause) = &func.where_clause {
        for predicate in &where_clause.predicates {
            if let TypeExpr::Named { name, type_args } = &predicate.ty.0 {
                if type_args.as_ref().is_none_or(Vec::is_empty) && type_param_names.contains(name) {
                    bounds
                        .entry(name.clone())
                        .or_insert_with(Vec::new)
                        .extend(predicate.bounds.iter().map(|bound| bound.name.clone()));
                }
            }
        }
    }
    (type_params, bounds)
}

/// Convert a Hew type expression to the type checker's `Ty`.
#[allow(
    clippy::too_many_lines,
    reason = "type mapping covers all primitive and generic variants"
)]
fn type_expr_to_ty(texpr: &TypeExpr, module_short: &str) -> Ty {
    type_expr_to_ty_with_params(texpr, module_short, &HashSet::new())
}

#[allow(
    clippy::too_many_lines,
    reason = "type mapping covers all primitive and generic variants"
)]
fn type_expr_to_ty_with_params(
    texpr: &TypeExpr,
    module_short: &str,
    type_params: &HashSet<String>,
) -> Ty {
    match texpr {
        TypeExpr::Named { name, type_args } => {
            // Primitive types never take type args; delegate entirely to the
            // canonical primitive table so names like `string`, `bool`, `char`,
            // `bytes`, `duration`, `isize`, `usize` are recognised instead of
            // falling through to module-qualification.
            let has_args = type_args.as_ref().is_some_and(|a| !a.is_empty());
            if !has_args {
                if let Some(prim) = Ty::from_name(name.as_str()) {
                    return prim;
                }
            }
            match name.as_str() {
                param
                    if type_args.as_ref().is_none_or(Vec::is_empty)
                        && type_params.contains(param) =>
                {
                    Ty::Named {
                        builtin: None,
                        name: param.to_string(),
                        args: vec![],
                    }
                }
                // Option<T> → Ty::option() helper
                "Option" => {
                    if let Some(args) = type_args {
                        if let Some(first) = args.first() {
                            return Ty::option(type_expr_to_ty_with_params(
                                &first.0,
                                module_short,
                                type_params,
                            ));
                        }
                    }
                    Ty::normalize_named("Option".to_string(), vec![])
                }
                // Result<O, E> → Ty::result() helper
                "Result" => {
                    if let Some(args) = type_args {
                        if args.len() >= 2 {
                            return Ty::result(
                                type_expr_to_ty_with_params(&args[0].0, module_short, type_params),
                                type_expr_to_ty_with_params(&args[1].0, module_short, type_params),
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
                                .map(|(te, _)| {
                                    type_expr_to_ty_with_params(te, module_short, type_params)
                                })
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
                                .map(|(te, _)| {
                                    type_expr_to_ty_with_params(te, module_short, type_params)
                                })
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
                                .map(|(te, _)| {
                                    type_expr_to_ty_with_params(te, module_short, type_params)
                                })
                                .collect()
                        })
                        .unwrap_or_default(),
                ),
            }
        }
        TypeExpr::Option(inner) => Ty::option(type_expr_to_ty_with_params(
            &inner.0,
            module_short,
            type_params,
        )),
        TypeExpr::Result { ok, err } => Ty::result(
            type_expr_to_ty_with_params(&ok.0, module_short, type_params),
            type_expr_to_ty_with_params(&err.0, module_short, type_params),
        ),
        TypeExpr::Tuple(elems) if elems.is_empty() => Ty::Unit,
        TypeExpr::Tuple(elems) => Ty::Tuple(
            elems
                .iter()
                .map(|(te, _)| type_expr_to_ty_with_params(te, module_short, type_params))
                .collect(),
        ),
        TypeExpr::Array { element, size } => Ty::Array(
            Box::new(type_expr_to_ty_with_params(
                &element.0,
                module_short,
                type_params,
            )),
            *size,
        ),
        TypeExpr::Slice(element) => Ty::normalize_named(
            "Vec".to_string(),
            vec![type_expr_to_ty_with_params(
                &element.0,
                module_short,
                type_params,
            )],
        ),
        TypeExpr::Infer => Ty::Error,
        TypeExpr::Function {
            params,
            return_type,
        } => Ty::Function {
            params: params
                .iter()
                .map(|(te, _)| type_expr_to_ty_with_params(te, module_short, type_params))
                .collect(),
            ret: Box::new(type_expr_to_ty_with_params(
                &return_type.0,
                module_short,
                type_params,
            )),
        },
        TypeExpr::Pointer {
            is_mutable,
            pointee,
        } => Ty::Pointer {
            is_mutable: *is_mutable,
            pointee: Box::new(type_expr_to_ty_with_params(
                &pointee.0,
                module_short,
                type_params,
            )),
        },
        // `&T` immutable borrow — first-class no-retain shared reference.
        TypeExpr::Borrow(inner) => Ty::Borrow {
            pointee: Box::new(type_expr_to_ty_with_params(
                &inner.0,
                module_short,
                type_params,
            )),
        },
        TypeExpr::TraitObject(bounds) => Ty::TraitObject {
            traits: bounds
                .iter()
                .map(|bound| crate::ty::TraitObjectBound {
                    trait_name: bound.name.clone(),
                    args: vec![],
                    assoc_bindings: vec![],
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
                // identifier — or an identifier wrapped in an ABI-width cast
                // like `port as i32`. The cast form preserves arity and the
                // argument's identity; it is used at the stdlib `int` → C-ABI
                // narrowing seam (INTERNAL-ABI). A compound body such as
                // `hew_bytes_to_string(hew_tcp_read(conn))` must NOT be
                // registered as a single-step C pass-through, because the
                // enricher would then rewrite `conn.read_string()` to
                // `hew_bytes_to_string(conn)` — dropping the inner call and
                // passing an i32 fd where bytes are expected.
                let all_direct = args.iter().all(|arg| is_pass_through_arg(&arg.expr().0));
                if all_direct {
                    return Some((name.clone(), args.len()));
                }
                return None;
            }
            None
        }
        // Handle blocks or unsafe blocks that wrap a call
        Expr::Block(block) => extract_call_target(block),
        Expr::UnsafeBlock(block) => extract_call_target(block),
        Expr::Cast { expr, .. } => call_target_from_expr(&expr.0),
        _ => None,
    }
}

/// True for expressions that a stdlib C-shim may pass through unchanged:
/// a direct parameter identifier, or that identifier wrapped in a single
/// ABI-width cast (`ident as T`).
fn is_pass_through_arg(expr: &Expr) -> bool {
    match expr {
        Expr::Identifier(_) => true,
        Expr::Cast { expr, .. } => matches!(expr.0, Expr::Identifier(_)),
        _ => false,
    }
}

/// Metadata a resource-wrapper method needs before its body counts as a genuine
/// `self.<field>`-to-extern forward: the receiver parameter's name, the
/// wrapper's simple type name (for the clone idiom's struct literal), the
/// wrapper's declared field names, and the module's extern C symbols.
struct WrapperForward<'a> {
    receiver: &'a str,
    wrapper_simple_name: &'a str,
    field_names: &'a HashSet<String>,
    extern_fn_names: &'a HashSet<String>,
}

impl WrapperForward<'_> {
    /// True for `self.<handle_field>` — the receiver identifier followed by one
    /// of the wrapper's own declared fields. Rejects a field access on any other
    /// binding or a field the wrapper does not declare.
    fn is_receiver_handle_field(&self, object: &Expr, field: &str) -> bool {
        matches!(object, Expr::Identifier(id) if id == self.receiver)
            && self.field_names.contains(field)
    }
}

/// Recognise a `#[resource]` handle-wrapper method body and return the forwarded
/// C symbol + argument count.
///
/// Complements [`extract_call_target`] for the `#[resource] T { handle: H }`
/// idiom. A borrowing method on such a wrapper cannot pass the resource itself
/// across the FFI boundary (`ResourceBoundaryParamMustConsume`); instead it
/// forwards the inner non-resource handle field, either directly
/// (`hew_foo(self.handle, ...rest)`) or through the wrapper-constructing clone
/// idiom (`T { handle: hew_foo(self.handle, ...) }`), where the remaining args
/// are plain pass-throughs. The forward is validated against the wrapper's own
/// receiver, declared field, and type, and the callee must be one of the
/// module's extern symbols — so a body that calls a Hew helper, reads some other
/// binding's field, or builds an unrelated record is not accepted. Only ever
/// called for resource wrappers, so the returned symbol never feeds a
/// handle-method rewrite (that path is gated on `is_handle_type`, false for a
/// fielded wrapper); it supplies the checker's imported-method signature
/// fallback alone.
fn extract_handle_forwarding_target(body: &Block, ctx: &WrapperForward) -> Option<(String, usize)> {
    if let Some(trailing) = &body.trailing_expr {
        if body.stmts.is_empty() {
            return handle_forwarding_call_from_expr(&trailing.0, ctx);
        }
        return None;
    }
    if body.stmts.len() == 1 {
        if let Some((Stmt::Expression(expr) | Stmt::Return(Some(expr)), _)) = body.stmts.last() {
            return handle_forwarding_call_from_expr(&expr.0, ctx);
        }
    }
    None
}

/// Find a handle-forwarding extern call in a wrapper method's tail expression,
/// peeling `unsafe`/block/cast wrappers and the wrapper-constructing clone
/// literal. A call qualifies when the callee is one of the module's extern
/// symbols, every argument is either a plain pass-through (`is_pass_through_arg`)
/// or the receiver's own declared field (`self.handle`), and at least one
/// argument is such a field access. The clone idiom is accepted only when it
/// constructs the *same* wrapper type and initialises one of its declared fields
/// with such a forward.
fn handle_forwarding_call_from_expr(expr: &Expr, ctx: &WrapperForward) -> Option<(String, usize)> {
    match expr {
        Expr::Call { function, args, .. } => {
            let Expr::Identifier(name) = &function.0 else {
                return None;
            };
            if !ctx.extern_fn_names.contains(name) {
                return None;
            }
            let mut forwards_handle_field = false;
            for arg in args {
                match &arg.expr().0 {
                    Expr::FieldAccess { object, field }
                        if ctx.is_receiver_handle_field(&object.0, field) =>
                    {
                        forwards_handle_field = true;
                    }
                    e if is_pass_through_arg(e) => {}
                    _ => return None,
                }
            }
            forwards_handle_field.then(|| (name.clone(), args.len()))
        }
        Expr::Block(block) => extract_handle_forwarding_target(block, ctx),
        Expr::UnsafeBlock(block) => extract_handle_forwarding_target(block, ctx),
        Expr::Cast { expr, .. } => handle_forwarding_call_from_expr(&expr.0, ctx),
        Expr::StructInit {
            name,
            fields,
            base: None,
            ..
        } if name == ctx.wrapper_simple_name => fields.iter().find_map(|(field_name, value)| {
            ctx.field_names
                .contains(field_name)
                .then(|| handle_forwarding_call_from_expr(&value.0, ctx))
                .flatten()
        }),
        _ => None,
    }
}

/// Extract handle method → C symbol mappings from an `impl` block.
///
/// For `impl FooMethods for Foo { fn bar(self: Foo) { hew_foo_bar(self); } }`,
/// produces `(("module.Foo", "bar"), "hew_foo_bar")`.
fn extract_handle_methods(
    impl_decl: &ImplDecl,
    module_short: &str,
    info: &mut ModuleInfo,
    resource_type_names: &HashSet<String>,
    wrapper_resource_fields: &HashMap<String, HashSet<String>>,
    extern_fn_names: &HashSet<String>,
) {
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

    // A fielded `#[resource] T { handle: H }` wrapper (e.g. regex.Pattern) maps to
    // its declared field names. Its methods forward the inner handle field across
    // the FFI boundary rather than the resource itself; it is NOT a handle type,
    // so nothing it registers can feed the `is_handle_type`-gated rewrite.
    let wrapper_field_names = wrapper_resource_fields.get(&type_name);
    let is_resource_wrapper = wrapper_field_names.is_some();
    let wrapper_simple_name = type_name.rsplit('.').next().unwrap_or(type_name.as_str());

    for method in &impl_decl.methods {
        // A fieldless `#[resource]` handle (e.g. process.Child) IS a handle
        // type, so its inherent `close` stays out of the handle-method table —
        // registering it would trip the `is_handle_type`-gated rewrite. A
        // fielded wrapper's `close` is rewrite-safe and must register so the
        // importer's impl-less registry path can resolve `x.close()`.
        if method.name == "close"
            && resource_type_names.contains(&type_name)
            && !is_resource_wrapper
        {
            continue;
        }
        // A trivial C shim forwards `self` directly (`hew_foo(self, ...)`). A
        // `#[resource] T { handle: H }` wrapper cannot: a borrowing method may
        // not pass the resource itself across the FFI boundary
        // (`ResourceBoundaryParamMustConsume`), so it forwards the inner
        // non-resource handle field (`hew_foo(self.handle, ...)`). Recognise that
        // shape — validated against the receiver, the wrapper's declared field,
        // and the module's extern set — so wrapper methods register into the
        // handle-method table the importer's checker resolves against. Scoped
        // here — the shared `is_pass_through_arg` predicate (which also drives
        // `clean_names` and drop-func extraction) is left untouched. Skip the
        // `self` parameter when matching — it's not part of the call.
        let extracted = extract_call_target(&method.body).or_else(|| {
            let field_names = wrapper_field_names?;
            let receiver = method.params.first()?.name.as_str();
            let ctx = WrapperForward {
                receiver,
                wrapper_simple_name,
                field_names,
                extern_fn_names,
            };
            extract_handle_forwarding_target(&method.body, &ctx)
        });
        if let Some((c_symbol, _arg_count)) = extracted {
            let params = method
                .params
                .iter()
                .skip(1)
                .map(|param| type_expr_to_ty(&param.ty.0, module_short))
                .collect();
            let return_type = method
                .return_type
                .as_ref()
                .map_or(Ty::Unit, |rt| type_expr_to_ty(&rt.0, module_short));
            info.handle_methods.push(HandleMethod {
                type_name: type_name.clone(),
                method_name: method.name.clone(),
                c_symbol,
                params,
                return_type,
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::path::PathBuf;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn test_root() -> PathBuf {
        // Tests run from the workspace root
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .to_path_buf()
    }

    struct TestDir {
        root: PathBuf,
    }

    impl TestDir {
        fn new(prefix: &str) -> Self {
            let unique = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_nanos();
            let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                .parent()
                .unwrap()
                .join("target/test-workdirs")
                .join(format!("{prefix}-{}-{unique}", std::process::id()));
            fs::create_dir_all(&root).unwrap();
            Self { root }
        }
    }

    impl Drop for TestDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.root);
        }
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
            !info.drop_types.contains(&"json.Value".to_string()),
            "json.Value should not be a drop type after impl Drop removal"
        );
        assert!(
            info.drop_funcs.iter().all(|(ty, _)| ty != "json.Value"),
            "json.Value should not have a drop func, got: {:?}",
            info.drop_funcs
        );

        // Should have clean name mapping for "parse"
        let has_parse = info.clean_names.iter().any(|(clean, _)| clean == "parse");
        assert!(has_parse, "json module should have 'parse' clean name");
    }

    #[test]
    fn load_module_checked_reports_parse_error_location() {
        let dir = TestDir::new("stdlib-loader-parse-error");
        let std_dir = dir.root.join("std");
        fs::create_dir_all(&std_dir).unwrap();
        let file_path = std_dir.join("broken.hew");
        fs::write(&file_path, "pub fn broken() {\n    @\n}\n").unwrap();

        let err = load_module_checked("std::broken", &dir.root).unwrap_err();
        match err {
            ModuleError::ParseError {
                module_path,
                file_path: err_path,
                line,
                column,
                message,
            } => {
                assert_eq!(module_path, "std::broken");
                assert_eq!(err_path, file_path);
                assert_eq!((line, column), (2, 5));
                assert!(
                    !message.is_empty(),
                    "parse error should preserve the parser message"
                );
            }
            ModuleError::NotFound { .. } => panic!("expected ParseError, got NotFound"),
        }
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
            .any(|m| m.type_name == "semaphore.Semaphore" && m.method_name == "acquire");
        assert!(
            has_acquire,
            "semaphore.Semaphore should have acquire method"
        );

        // Should have "new" clean name
        let has_new = info.clean_names.iter().any(|(clean, _)| clean == "new");
        assert!(has_new, "semaphore module should have 'new' clean name");
    }

    #[test]
    fn load_http_module_registers_handle_methods_through_abi_width_casts() {
        // `impl RequestMethods for Request` wraps `status: int` in `status as i32`
        // before calling the C shim. Those casts are pure ABI-width narrowing and
        // must not prevent the method from registering as a handle pass-through —
        // otherwise the type checker reports `no method respond on http.Request`.
        let info =
            load_module("std::net::http", &test_root()).expect("should load std::net::http module");

        for method in ["respond", "respond_text", "respond_json", "respond_stream"] {
            let found = info
                .handle_methods
                .iter()
                .any(|m| m.type_name == "http.Request" && m.method_name == method);
            assert!(
                found,
                "http.Request.{method} should register as a handle method even when its C shim uses `status as i32`"
            );
        }
    }

    #[test]
    fn load_net_module_skips_nontrivial_handle_wrappers() {
        let info = load_module("std::net", &test_root());
        assert!(info.is_some(), "should load net module");
        let info = info.unwrap();

        let has_read = info.handle_methods.iter().any(|m| {
            m.type_name == "net.Connection"
                && m.method_name == "read"
                && m.c_symbol == "hew_tcp_read"
        });
        assert!(
            has_read,
            "net.Connection.read should rewrite to hew_tcp_read"
        );

        let rewrites_read_string = info.handle_methods.iter().any(|m| {
            m.type_name == "net.Connection"
                && m.method_name == "read_string"
                && m.c_symbol == "hew_bytes_to_string"
        });
        assert!(
            !rewrites_read_string,
            "net.Connection.read_string should remain a Hew wrapper, not alias hew_bytes_to_string"
        );

        let rewrites_write_string = info.handle_methods.iter().any(|m| {
            m.type_name == "net.Connection"
                && m.method_name == "write_string"
                && m.c_symbol == "hew_tcp_write"
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
    fn load_process_module_exposes_argv_first_surface() {
        let info = load_module("std::process", &test_root());
        assert!(info.is_some(), "should load process module");
        let info = info.unwrap();

        for name in ["try_run", "run", "try_run_argv", "run_argv", "start"] {
            assert!(
                info.wrapper_fns.iter().any(|f| f.name == name),
                "process module should expose `{name}`"
            );
        }

        for name in ["try_run_args", "run_args"] {
            assert!(
                info.wrapper_fns.iter().any(|f| f.name == name),
                "process module should retain legacy `{name}` wrapper for compatibility"
            );
        }
        assert!(
            info.drop_types.contains(&"process.Child".to_string()),
            "process module should mark process.Child as a drop type"
        );
        assert_eq!(
            info.drop_funcs
                .iter()
                .find(|(ty, _)| ty == "process.Child")
                .map(|(_, func)| func.as_str()),
            Some("hew_process_drop"),
            "process.Child drop func should be hew_process_drop"
        );
    }

    #[test]
    fn load_process_child_handle_methods_include_signatures() {
        let info = load_module("std::process", &test_root()).expect("should load process module");

        let wait = info
            .handle_methods
            .iter()
            .find(|m| m.type_name == "process.Child" && m.method_name == "wait")
            .expect("process.Child.wait should be extracted");
        assert_eq!(wait.c_symbol, "hew_process_wait");
        assert_eq!(wait.params, Vec::<Ty>::new());
        assert_eq!(wait.return_type, Ty::I64);

        let kill = info
            .handle_methods
            .iter()
            .find(|m| m.type_name == "process.Child" && m.method_name == "kill")
            .expect("process.Child.kill should be extracted");
        assert_eq!(kill.c_symbol, "hew_process_kill");
        assert_eq!(kill.params, Vec::<Ty>::new());
        assert_eq!(kill.return_type, Ty::I64);
    }

    #[test]
    fn load_net_and_http_handle_surface_methods_include_signatures() {
        let net_info = load_module("std::net", &test_root()).expect("should load net module");
        let close = net_info
            .handle_methods
            .iter()
            .find(|m| m.type_name == "net.Listener" && m.method_name == "close")
            .expect("net.Listener.close should be extracted");
        assert_eq!(close.c_symbol, "hew_tcp_listener_close");
        assert_eq!(close.params, Vec::<Ty>::new());
        assert_eq!(close.return_type, Ty::Unit);

        let http_info =
            load_module("std::net::http", &test_root()).expect("should load http module");
        let free = http_info
            .handle_methods
            .iter()
            .find(|m| m.type_name == "http.Request" && m.method_name == "free")
            .expect("http.Request.free should be extracted");
        assert_eq!(free.c_symbol, "hew_http_request_free");
        assert_eq!(free.params, Vec::<Ty>::new());
        assert_eq!(free.return_type, Ty::Unit);
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
                // Only push the directory as a module when the conventional
                // package-entry file exists (e.g. std/encoding/json/json.hew),
                // or when the flat form exists (e.g. std/io.hew alongside std/io/).
                // If neither exists, treat the individual .hew files inside the
                // directory as their own modules (namespace directory pattern):
                // e.g. std/actor/monitor.hew → std::actor::monitor.
                let last = *segments.last().unwrap();
                let dir_entry = dir.join(format!("{last}.hew"));
                let flat_entry = {
                    // repo_root/std/actor.hew
                    let mut p = repo_root.to_path_buf();
                    for seg in &segments {
                        p = p.join(seg);
                    }
                    p.with_extension("hew")
                };
                if dir_entry.exists() || flat_entry.exists() {
                    paths.push(segments.join("::"));
                } else {
                    // Namespace directory: discover each .hew file as its own module.
                    let prefix = segments.join("::");
                    for entry in &sorted {
                        let path = entry.path();
                        if path.extension().is_some_and(|e| e == "hew")
                            && path.file_name().is_none_or(|f| f != "builtins.hew")
                        {
                            let stem = path.file_stem().unwrap().to_str().unwrap();
                            paths.push(format!("{prefix}::{stem}"));
                        }
                    }
                }
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
            .any(|f| f.params.contains(&Ty::String));
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
            .find(|f| f.name == "hew_channel_sender_clone")
            .expect("channel module should expose sender clone");
        assert_eq!(
            clone_sig.params,
            vec![Ty::normalize_named("Sender".to_string(), vec![])]
        );
        assert_eq!(
            clone_sig.return_type,
            Ty::normalize_named("Sender".to_string(), vec![])
        );

        let new_sig = info
            .wrapper_fns
            .iter()
            .find(|f| f.name == "new")
            .expect("channel module should expose new()");
        assert_eq!(
            new_sig.return_type,
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
        let setup = info.wrapper_fns.iter().find(|f| f.name == "setup");
        assert!(setup.is_some(), "log module should have 'setup' wrapper fn");
        let setup = setup.unwrap();
        assert!(setup.params.is_empty(), "setup() takes no parameters");
        assert_eq!(setup.return_type, Ty::Unit, "setup() returns Unit");

        // `pub fn set_level(level: i64)` takes one i64 param
        let set_level = info.wrapper_fns.iter().find(|f| f.name == "set_level");
        assert!(
            set_level.is_some(),
            "log module should have 'set_level' wrapper fn"
        );
        let set_level = set_level.unwrap();
        assert_eq!(set_level.params.len(), 1);
        assert_eq!(set_level.params[0], Ty::I64);
    }

    #[test]
    fn drop_types_extracted() {
        let info = load_module("std::net::http", &test_root()).unwrap();

        assert!(
            !info.drop_types.contains(&"http.Request".to_string()),
            "http.Request should not be a drop type after impl Drop removal, got: {:?}",
            info.drop_types
        );

        assert!(
            !info.drop_types.contains(&"http.Server".to_string()),
            "http.Server should not be a drop type, got: {:?}",
            info.drop_types
        );
    }

    #[test]
    fn drop_funcs_extracted() {
        let info = load_module("std::net::http", &test_root()).unwrap();

        let request_drop = info.drop_funcs.iter().find(|(ty, _)| ty == "http.Request");
        assert!(
            request_drop.is_none(),
            "http.Request should not have a drop func, got: {:?}",
            info.drop_funcs
        );

        let server_drop = info.drop_funcs.iter().find(|(ty, _)| ty == "http.Server");
        assert!(
            server_drop.is_none(),
            "http.Server should not have a drop func, got: {:?}",
            info.drop_funcs
        );
    }

    #[test]
    fn json_module_no_drop_type_without_impl_drop() {
        let info = load_module("std::encoding::json", &test_root()).unwrap();

        assert!(
            !info.drop_types.contains(&"json.Value".to_string()),
            "json.Value should not be a drop type, got: {:?}",
            info.drop_types
        );
        assert!(
            info.drop_funcs.iter().all(|(ty, _)| ty != "json.Value"),
            "json.Value should not have a drop func, got: {:?}",
            info.drop_funcs
        );
    }

    #[test]
    fn regex_module_pattern_is_drop_resource_via_marker() {
        let info = load_module("std::text::regex", &test_root()).unwrap();

        // `Pattern` is a `#[resource]` handle wrapper, so it is a drop type —
        // scope exit runs its inherent `close()`.
        assert!(
            info.drop_types.contains(&"regex.Pattern".to_string()),
            "regex.Pattern is a `#[resource]`, so it should be a drop type, got: {:?}",
            info.drop_types
        );
        // No registry drop func: `close()` forwards the inner handle field
        // (`hew_regex_free(pat.handle)`), which the direct-C-shim extractor
        // deliberately does not collapse. Scope-exit drop resolves `close()`
        // through the impl block, not this legacy table.
        assert!(
            info.drop_funcs.iter().all(|(ty, _)| ty != "regex.Pattern"),
            "regex.Pattern close() forwards through its handle field, so no \
             direct drop func is registered, got: {:?}",
            info.drop_funcs
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

    /// Directly exercises `type_expr_to_ty` for canonical primitive names.
    /// Retired aliases (str, float, Float, byte, Bool, Char, Bytes) must not
    /// resolve — see `primitive_aliases_rejected` for the negative assertions.
    #[test]
    fn primitive_canonicals_delegate_to_from_name() {
        use hew_parser::ast::TypeExpr;

        let module = "test";
        let cases: &[(&str, Ty)] = &[
            // canonical names must still work
            ("string", Ty::String),
            ("i64", Ty::I64),
            ("isize", Ty::Isize),
            ("u64", Ty::U64),
            ("f64", Ty::F64),
            ("bool", Ty::Bool),
            ("char", Ty::Char),
            ("bytes", Ty::Bytes),
            ("duration", Ty::Duration),
        ];

        for (name, expected) in cases {
            let texpr = TypeExpr::Named {
                name: name.to_string(),
                type_args: None,
            };
            let got = type_expr_to_ty(&texpr, module);
            assert_eq!(
                got, *expected,
                "canonical `{name}` should resolve to {expected:?}, got {got:?}"
            );
        }
    }

    /// Retired aliases must NOT resolve to primitive types; they fall through
    /// to module-qualification (i.e. produce a Named type, not a primitive).
    #[test]
    fn primitive_aliases_rejected() {
        use hew_parser::ast::TypeExpr;

        let retired = ["str", "float", "Float", "byte", "Bool", "Char", "Bytes"];
        for alias in retired {
            let texpr = TypeExpr::Named {
                name: alias.to_string(),
                type_args: None,
            };
            let got = type_expr_to_ty(&texpr, "mymod");
            assert!(
                !matches!(
                    got,
                    Ty::String | Ty::F64 | Ty::U8 | Ty::Bool | Ty::Char | Ty::Bytes
                ),
                "retired alias `{alias}` must not resolve to a primitive type, got {got:?}"
            );
        }
    }

    #[test]
    fn slice_type_exprs_alias_to_vec_in_loader() {
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
            Ty::normalize_named("Vec".to_string(), vec![Ty::I32]),
            "slice annotations must alias to Vec<T> in registry-loaded signatures"
        );
    }

    #[test]
    fn module_info_accepts_public_slice_signatures_as_vec_aliases() {
        let result = parse("pub fn take(xs: [i32]) {}\n");
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );

        let info = extract_module_info(&result.program, "widgets");
        assert_eq!(
            info.unsupported_type_signatures,
            Vec::<String>::new(),
            "slice-bearing public signatures should be accepted as Vec aliases"
        );
        assert_eq!(
            info.wrapper_fns[0].params,
            vec![Ty::normalize_named("Vec".to_string(), vec![Ty::I32])],
            "slice parameter should be loaded as a Vec alias"
        );
    }

    fn handle_method_registered(info: &ModuleInfo, type_name: &str, method: &str) -> bool {
        info.handle_methods
            .iter()
            .any(|m| m.type_name == type_name && m.method_name == method)
    }

    #[test]
    fn wrapper_forward_rejects_non_receiver_field_access() {
        // `bad` forwards `other.handle` — a declared field, but on a *different*
        // binding than the receiver. Only a genuine `self.<field>` forward may
        // register; `good` (forwarding the receiver's own field) is the control.
        let result = parse(
            "#[resource]\n\
             pub type Wrap { handle: i64; }\n\
             impl Wrap {\n\
             \x20   fn good(w: Wrap) -> i64 { unsafe { c_use(w.handle) } }\n\
             \x20   fn bad(w: Wrap, other: Wrap) -> i64 { unsafe { c_use(other.handle) } }\n\
             }\n\
             extern \"C\" {\n\
             \x20   fn c_use(h: i64) -> i64;\n\
             }\n",
        );
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let info = extract_module_info(&result.program, "m");

        assert!(
            handle_method_registered(&info, "m.Wrap", "good"),
            "a genuine self.handle forward should register, got: {:?}",
            info.handle_methods
        );
        assert!(
            !handle_method_registered(&info, "m.Wrap", "bad"),
            "forwarding `other.handle` (not the receiver) must not register, got: {:?}",
            info.handle_methods
        );
    }

    #[test]
    fn wrapper_forward_rejects_foreign_clone_literal() {
        // `bad` builds an unrelated `Other` record in the clone position. The
        // clone idiom registers only when it constructs the SAME wrapper type;
        // `good` (constructing `Wrap`) is the control.
        let result = parse(
            "#[resource]\n\
             pub type Wrap { handle: i64; }\n\
             pub type Other { handle: i64; }\n\
             impl Wrap {\n\
             \x20   fn good(w: Wrap) -> Wrap { unsafe { Wrap { handle: c_clone(w.handle) } } }\n\
             \x20   fn bad(w: Wrap) -> Other { unsafe { Other { handle: c_clone(w.handle) } } }\n\
             }\n\
             extern \"C\" {\n\
             \x20   fn c_clone(h: i64) -> i64;\n\
             }\n",
        );
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let info = extract_module_info(&result.program, "m");

        assert!(
            handle_method_registered(&info, "m.Wrap", "good"),
            "the same-wrapper clone idiom should register, got: {:?}",
            info.handle_methods
        );
        assert!(
            !handle_method_registered(&info, "m.Wrap", "bad"),
            "a foreign record literal in the clone position must not register, got: {:?}",
            info.handle_methods
        );
    }

    #[test]
    fn wrapper_forward_rejects_non_extern_callee() {
        // `bad` forwards its handle to a Hew helper, not an extern C symbol, so
        // it is not a C-backed method; `good` (calling the extern `c_use`) is the
        // control.
        let result = parse(
            "#[resource]\n\
             pub type Wrap { handle: i64; }\n\
             impl Wrap {\n\
             \x20   fn good(w: Wrap) -> i64 { unsafe { c_use(w.handle) } }\n\
             \x20   fn bad(w: Wrap) -> i64 { helper(w.handle) }\n\
             }\n\
             fn helper(h: i64) -> i64 { h }\n\
             extern \"C\" {\n\
             \x20   fn c_use(h: i64) -> i64;\n\
             }\n",
        );
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let info = extract_module_info(&result.program, "m");

        assert!(
            handle_method_registered(&info, "m.Wrap", "good"),
            "forwarding to an extern C symbol should register, got: {:?}",
            info.handle_methods
        );
        assert!(
            !handle_method_registered(&info, "m.Wrap", "bad"),
            "forwarding to a non-extern Hew helper must not register, got: {:?}",
            info.handle_methods
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

    /// Pins the namespace-directory fix from `d0db9ada`.
    ///
    /// A directory that has no same-name entry file (e.g. `std/actor/` with no
    /// `std/actor/actor.hew` and no `std/actor.hew`) is a namespace directory.
    /// Each `.hew` file inside it must be discovered as its own module:
    /// `fake::ns::alpha` and `fake::ns::beta`, not `fake::ns`.
    #[test]
    fn namespace_directory_enumerates_each_hew_file_as_own_module() {
        let dir = TestDir::new("ns-discovery");
        let root = &dir.root;

        // Build a layout that matches std/actor/:
        //   <root>/fake/ns/alpha.hew
        //   <root>/fake/ns/beta.hew
        // No <root>/fake/ns/ns.hew, no <root>/fake/ns.hew.
        let ns_dir = root.join("fake").join("ns");
        fs::create_dir_all(&ns_dir).unwrap();
        fs::write(ns_dir.join("alpha.hew"), "pub fn alpha() {}\n").unwrap();
        fs::write(ns_dir.join("beta.hew"), "pub fn beta() {}\n").unwrap();

        let mut paths = Vec::new();
        discover_modules(&root.join("fake"), root, &mut paths);

        assert!(
            paths.contains(&"fake::ns::alpha".to_string()),
            "namespace dir: alpha.hew must become fake::ns::alpha, got: {paths:?}"
        );
        assert!(
            paths.contains(&"fake::ns::beta".to_string()),
            "namespace dir: beta.hew must become fake::ns::beta, got: {paths:?}"
        );
        assert!(
            !paths.contains(&"fake::ns".to_string()),
            "namespace dir must NOT produce fake::ns (no entry file), got: {paths:?}"
        );
    }
}
