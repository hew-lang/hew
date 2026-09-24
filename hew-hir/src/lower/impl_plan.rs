//! Impl-body symbol planning and builtin impl-program injection.

use super::*;
use hew_parser::ast::Ident;

/// Whether two linker spellings are compatibility projections of one
/// checker-owned impl declaration.
///
/// The checker publishes an imported impl under both its canonical owner
/// (`pkg.Type::method`) and, where the surface permits it, the bare dispatch
/// alias (`Type::method`).  Reaching the same source through two import paths
/// can therefore present both spellings to the HIR planner.  The declaration
/// key is the authority: both symbols must name its exact receiver and method.
/// A merely same-shaped symbol for another receiver remains a genuine
/// conflict and fails closed.
pub(super) fn impl_body_symbol_matches_declaration(
    declaration: &hew_types::DefId,
    symbol: &str,
) -> bool {
    let Some((receiver, declaration_tail)) = declaration.full_path().split_once("::<") else {
        return false;
    };
    let Some((_, method)) = declaration_tail.rsplit_once(">::") else {
        return false;
    };
    let Some((symbol_owner, symbol_method)) = symbol.rsplit_once("::") else {
        return false;
    };
    if symbol_method != method {
        return false;
    }
    let receiver_leaf = hew_types::short_name(receiver);
    symbol_owner == receiver
        || symbol_owner == receiver_leaf
        || symbol_owner
            .strip_prefix(receiver)
            .is_some_and(|suffix| suffix.starts_with("$$"))
        || symbol_owner
            .strip_prefix(receiver_leaf)
            .is_some_and(|suffix| suffix.starts_with("$$"))
}

pub(super) fn impl_body_symbols_alias_one_declaration(
    declaration: &hew_types::DefId,
    left: &str,
    right: &str,
) -> bool {
    left != right
        && impl_body_symbol_matches_declaration(declaration, left)
        && impl_body_symbol_matches_declaration(declaration, right)
}

pub(super) fn declaration_owned_impl_body_symbol<'a>(
    declaration: &hew_types::DefId,
    left: &'a str,
    right: &'a str,
) -> &'a str {
    let receiver = declaration
        .full_path()
        .split_once("::<")
        .map_or("", |(receiver, _)| receiver);
    let is_declaration_owned = |symbol: &str| {
        symbol.rsplit_once("::").is_some_and(|(owner, _)| {
            owner == receiver
                || owner
                    .strip_prefix(receiver)
                    .is_some_and(|suffix| suffix.starts_with("$$"))
        })
    };
    if is_declaration_owned(right) && !is_declaration_owned(left) {
        right
    } else {
        left
    }
}

pub(super) fn imported_impl_symbol_self_name(source_module: &str, source_name: &str) -> String {
    if source_name.contains('.') {
        source_name.to_string()
    } else {
        format!("{source_module}.{source_name}")
    }
}

pub(super) fn validate_impl_body_owner_alias(
    ctx: &mut LowerCtx,
    declaration_key: *const hew_parser::ast::ImplDecl,
    base_symbol_self_name: &str,
    planned: &[(hew_types::DefId, String)],
) -> bool {
    let Some(existing_owner) = ctx
        .impl_body_plan
        .symbol_self_names
        .get(&declaration_key)
        .cloned()
    else {
        return true;
    };
    if existing_owner == base_symbol_self_name {
        return true;
    }
    let aliases_existing_plan = !planned.is_empty()
        && planned.iter().all(|(declaration, symbol)| {
            ctx.impl_body_plan
                .symbols
                .get(declaration)
                .is_some_and(|existing| {
                    existing == symbol
                        || impl_body_symbols_alias_one_declaration(declaration, existing, symbol)
                })
        });
    if aliases_existing_plan {
        return true;
    }
    ctx.impl_body_plan
        .symbol_self_names
        .remove(&declaration_key);
    ctx.diagnostics.push(HirDiagnostic::new(
        HirDiagnosticKind::CheckerBoundaryViolation {
            name: "impl body owner".to_string(),
            reason: format!(
                "conflicting pre-lowering owners `{existing_owner}` and `{base_symbol_self_name}`"
            ),
        },
        0..0,
        "one implementation declaration selected two distinct canonical owners",
    ));
    false
}

pub(super) fn merge_planned_impl_body_symbols(
    ctx: &mut LowerCtx,
    planned: &[(hew_types::DefId, String)],
) -> bool {
    let mut conflict = false;
    for (declaration, symbol) in planned {
        if let Some(existing) = ctx
            .impl_body_plan
            .symbols
            .insert(declaration.clone(), symbol.clone())
        {
            if existing == *symbol {
                continue;
            }
            if impl_body_symbols_alias_one_declaration(declaration, &existing, symbol) {
                let selected = if ctx.impl_body_plan.compiler_selected.contains(declaration) {
                    existing
                } else {
                    declaration_owned_impl_body_symbol(declaration, &existing, symbol).to_string()
                };
                ctx.impl_body_plan
                    .symbols
                    .insert(declaration.clone(), selected);
                continue;
            }
            ctx.impl_body_plan.symbols.remove(declaration);
            conflict = true;
            ctx.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: format!("impl body `{}`", declaration.full_path()),
                    reason: format!(
                        "conflicting pre-lowering symbols `{existing}` and `{symbol}`"
                    ),
                },
                0..0,
                format!(
                    "implementation declaration `{}` selected two distinct emitted-body symbols: `{existing}` and `{symbol}`",
                    declaration.full_path()
                ),
            ));
        }
    }
    !conflict
}

pub(super) fn record_impl_body_owner(
    ctx: &mut LowerCtx,
    declaration_key: *const hew_parser::ast::ImplDecl,
    base_symbol_self_name: &str,
    planned: &[(hew_types::DefId, String)],
) {
    let selected_owner = planned
        .iter()
        .find_map(|(declaration, _)| {
            ctx.impl_body_plan
                .symbols
                .get(declaration)
                .and_then(|symbol| symbol.rsplit_once("::").map(|(owner, _)| owner.to_string()))
        })
        .unwrap_or_else(|| base_symbol_self_name.to_string());
    ctx.impl_body_plan
        .symbol_self_names
        .insert(declaration_key, selected_owner);
}

pub(super) fn plan_impl_block_symbols(
    ctx: &mut LowerCtx,
    impl_decl: &hew_parser::ast::ImplDecl,
    base_symbol_self_name: &str,
    skip_methods: &HashSet<String>,
) {
    if impl_decl.where_clause.is_some() && classify_unsupported_where_clause(impl_decl).is_some() {
        return;
    }
    let TypeExpr::Named {
        path: named_path,
        type_args,
    } = &impl_decl.target_type.0
    else {
        return;
    };
    let self_type_name = &named_path.to_string(); // TRANSITION(P1): deleted by A1 commit 2
    if impl_type_param_names(impl_decl)
        .iter()
        .any(|type_param| type_param == self_type_name)
    {
        return;
    }
    let impl_type_params = impl_type_param_names(impl_decl);
    let concrete_args: Vec<ResolvedTy> = if impl_type_params.is_empty() {
        type_args
            .as_deref()
            .unwrap_or(&[])
            .iter()
            .map(|arg| ctx.lower_type(arg))
            .collect()
    } else {
        Vec::new()
    };
    let symbol_self_name = if concrete_args.is_empty() {
        base_symbol_self_name.to_string()
    } else {
        crate::monomorph::mangle(base_symbol_self_name, &concrete_args)
    };
    let mut planned: Vec<(hew_types::DefId, String)> = Vec::new();
    for method in &impl_decl.methods {
        if skip_methods.contains(method.name.name.as_str()) {
            continue;
        }
        let symbol =
            crate::node::HirImplBlock::method_symbol(&symbol_self_name, method.name.name.as_str());
        let Some(declaration) = ctx.impl_method_declaration_ids.get(&symbol).cloned() else {
            continue;
        };
        planned.push((declaration, symbol));
    }
    // A trait default the impl does NOT override is materialised as its own
    // body by `lower_impl_block`, under a declaration id minted at that
    // synthesis boundary (`synthetic_default_impl_body_declaration`) — the
    // checker never saw a method declaration to publish into
    // `impl_method_declaration_ids`, so the explicit-method loop above cannot
    // reach it. Plan those ids on the same authority: an imported module's
    // bodies are emitted in the fourth pass, so a ROOT call to a materialised
    // default (`d.greet()` on a type from `import gm;`) is lowered before its
    // body exists and otherwise fails closed with `CallableUnsupportedInMir`.
    planned.extend(materialized_default_body_plan(
        ctx,
        impl_decl,
        &symbol_self_name,
    ));
    let declaration_key = impl_decl as *const _;
    if !validate_impl_body_owner_alias(ctx, declaration_key, base_symbol_self_name, &planned) {
        return;
    }
    if !merge_planned_impl_body_symbols(ctx, &planned) {
        ctx.impl_body_plan
            .symbol_self_names
            .remove(&declaration_key);
        return;
    }
    record_impl_body_owner(ctx, declaration_key, base_symbol_self_name, &planned);
}

/// The `(declaration, emitted symbol)` pairs for every trait default an impl
/// block materialises rather than overrides.
///
/// Mirrors the synthesis in `lower_impl_block` exactly — same owner key, same
/// non-overridden filter, same `synthetic_default_impl_body_declaration` mint,
/// same `method_symbol` — so the plan and the later emission cannot disagree.
pub(super) fn materialized_default_body_plan(
    ctx: &mut LowerCtx,
    impl_decl: &hew_parser::ast::ImplDecl,
    symbol_self_name: &str,
) -> Vec<(hew_types::DefId, String)> {
    let Some(trait_bound) = &impl_decl.trait_bound else {
        return Vec::new();
    };
    let Some(owner_key) = ctx.trait_declaration(&trait_bound.path.to_string()) else {
        // TRANSITION(P1): deleted by A1 commit 2
        return Vec::new();
    };
    let Some(defaults) = ctx.trait_defaults.get(&owner_key).cloned() else {
        return Vec::new();
    };
    let overridden: HashSet<&str> = impl_decl
        .methods
        .iter()
        .map(|m| m.name.name.as_str())
        .collect();
    // Planning is a read-only projection: `lower_impl_block` lowers this exact
    // target type again and owns every diagnostic that resolution produces.
    // Discard anything emitted here so the plan cannot duplicate one.
    let diagnostics_before = ctx.diagnostics.len();
    let self_ty = ctx.lower_type(&impl_decl.target_type);
    ctx.diagnostics.truncate(diagnostics_before);
    let mut out = Vec::new();
    for default_method in &defaults {
        if overridden.contains(default_method.method.name.name.as_str()) {
            continue;
        }
        let declaring_trait = &default_method.trait_id;
        let Some(declaration) = LowerCtx::synthetic_default_impl_body_declaration(
            declaring_trait,
            Some(&self_ty),
            default_method.method.name.name.as_str(),
        ) else {
            continue;
        };
        out.push((
            declaration,
            crate::node::HirImplBlock::method_symbol(
                symbol_self_name,
                default_method.method.name.name.as_str(),
            ),
        ));
    }
    out
}

pub(super) fn plan_imported_impl_bodies(
    ctx: &mut LowerCtx,
    program: &Program,
    file_import_module_idx: &HashMap<usize, u32>,
    file_import_modules: &HashSet<hew_parser::module::ModulePath>,
    preferred_modules: &HashSet<hew_parser::module::ModulePath>,
    span_indices: &hew_parser::module::FileSpanIndices,
    skip_imported_builtin_impls: bool,
) {
    let empty_skips = HashSet::new();
    // Source-order bodies include root declarations and flattened file imports.
    // They may be called before their tail-spliced item is emitted.
    for (item_idx, (item, _)) in program.items.iter().enumerate() {
        ctx.current_module_idx = file_import_module_idx
            .get(&item_idx)
            .copied()
            .unwrap_or_default();
        ctx.current_module_name = span_indices
            .module_name(ctx.current_module_idx)
            .map(str::to_string);
        if let Item::Impl(impl_decl) = item {
            if let TypeExpr::Named {
                path: named_path, ..
            } = &impl_decl.target_type.0
            {
                let name = &named_path.to_string(); // TRANSITION(P1): deleted by A1 commit 2
                let symbol_self_name = file_import_module_idx
                    .get(&item_idx)
                    .and_then(|module_idx| span_indices.module_name(*module_idx))
                    .map_or_else(
                        || name.clone(),
                        |module| imported_impl_symbol_self_name(module, name),
                    );
                plan_impl_block_symbols(ctx, impl_decl, &symbol_self_name, &empty_skips);
            }
        }
    }
    ctx.current_module_idx = 0;
    ctx.current_module_name = None;

    let Some(module_graph) = &program.module_graph else {
        return;
    };
    for module_id in &module_graph.topo_order {
        if *module_id == module_graph.root
            || file_import_modules.contains(module_id)
            || !module_graph.modules.contains_key(module_id)
        {
            continue;
        }
        let module = &module_graph.modules[module_id];
        let source_module = module_id.dotted();
        if skip_imported_builtin_impls && source_module == "std.builtins" {
            continue;
        }
        let previous_module = ctx.current_module_name.replace(source_module.clone());
        let previous_module_idx = ctx.current_module_idx;
        for (item_idx, (item, _)) in module.items.iter().enumerate() {
            ctx.current_module_idx = span_indices
                .item_index(module_id, item_idx)
                .unwrap_or_default();
            let Item::Impl(impl_decl) = item else {
                continue;
            };
            if item_is_duplicated_in_preferred_module(program, preferred_modules, module_id, item) {
                continue;
            }
            let TypeExpr::Named {
                path: named_path, ..
            } = &impl_decl.target_type.0
            else {
                continue;
            };
            let name = &named_path.to_string(); // TRANSITION(P1): deleted by A1 commit 2
            let skip_methods = ctx.imported_impl_skip_methods(impl_decl, &source_module);
            let base_symbol_self_name = imported_impl_symbol_self_name(&source_module, name);
            plan_impl_block_symbols(ctx, impl_decl, &base_symbol_self_name, &skip_methods);
        }
        ctx.current_module_idx = previous_module_idx;
        ctx.current_module_name = previous_module;
    }
}

/// Synthesize a `FnDecl` from a `TraitMethod` for HIR lowering purposes.
/// The resulting `FnDecl` carries the default body and the same signature
/// as the trait declaration. `current_impl_self_ty` in the lowering context
/// handles substituting `Self` for the concrete type.
pub(super) fn trait_method_to_fn_decl(method: &TraitMethod) -> FnDecl {
    FnDecl {
        origin: hew_parser::ast::DeclarationOrigin::Authored,
        attributes: vec![],
        is_generator: false,
        visibility: hew_parser::ast::Visibility::Private,
        name: method.name,
        type_params: method.type_params.clone(),
        params: method.params.clone(),
        return_type: method.return_type.clone(),
        where_clause: method.where_clause.clone(),
        // Only called for methods where `body.is_some()`; the unwrap is safe.
        body: method
            .body
            .clone()
            .expect("trait_method_to_fn_decl called on method without body"),
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
        intrinsic: None,
        consumes_self: false,
    }
}

/// Compact, user-facing rendering of a parser-side `TypeExpr` used by the
/// `ResourceCloseMustReturnUnit` diagnostic. Best-effort; complex / deeply
/// nested shapes degrade to a short description rather than a full pretty
/// print — the diagnostic's purpose is to name what the user wrote enough
/// to direct them to the fix, not to round-trip the AST.
pub(super) fn render_type_expr(ty: &TypeExpr) -> String {
    match ty {
        TypeExpr::QualifiedAssocPath(path) => format!(
            "<{} as {}>.{}",
            render_type_expr(&path.base.0),
            path.trait_path,
            path.members
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(".")
        ),
        TypeExpr::Named { path, type_args } => match type_args {
            Some(args) if !args.is_empty() => {
                let inner: Vec<String> = args.iter().map(|a| render_type_expr(&a.0)).collect();
                format!("{path}<{}>", inner.join(", "))
            }
            _ => path.to_string(),
        },
        TypeExpr::Result { ok, err }
        | TypeExpr::Fallible {
            success: ok,
            error: err,
        } => {
            format!(
                "Result<{}, {}>",
                render_type_expr(&ok.0),
                render_type_expr(&err.0)
            )
        }
        TypeExpr::Option(inner) => format!("Option<{}>", render_type_expr(&inner.0)),
        TypeExpr::Tuple(items) => {
            if items.is_empty() {
                "()".to_string()
            } else {
                let inner: Vec<String> = items.iter().map(|t| render_type_expr(&t.0)).collect();
                format!("({})", inner.join(", "))
            }
        }
        TypeExpr::Array { element, size } => {
            format!("[{}; {size}]", render_type_expr(&element.0))
        }
        TypeExpr::Slice(inner) => format!("[{}]", render_type_expr(&inner.0)),
        TypeExpr::Function {
            params,
            return_type,
            ..
        } => {
            let ps: Vec<String> = params.iter().map(|p| render_type_expr(&p.0)).collect();
            format!(
                "fn({}) -> {}",
                ps.join(", "),
                render_type_expr(&return_type.0)
            )
        }
        TypeExpr::ActorFn {
            params,
            return_type,
        } => {
            let ps: Vec<String> = params.iter().map(|p| render_type_expr(&p.0)).collect();
            format!(
                "actor({}) -> {}",
                ps.join(", "),
                render_type_expr(&return_type.0)
            )
        }
        TypeExpr::Pointer {
            is_mutable,
            pointee,
        } => {
            let m = if *is_mutable { "mut " } else { "" };
            format!("*{m}{}", render_type_expr(&pointee.0))
        }
        TypeExpr::TraitObject(_) => "dyn Trait".to_string(),
        TypeExpr::Borrow(inner) => format!("&{}", render_type_expr(&inner.0)),
        TypeExpr::Infer => "_".to_string(),
    }
}

pub(super) fn builtin_callable_impl_program() -> Option<Program> {
    let parsed = hew_parser::parse(BUILTINS_HEW_SOURCE);
    debug_assert!(
        parsed.errors.is_empty(),
        "std/builtins.hew failed to parse: {:?}",
        parsed.errors
    );
    if !parsed.errors.is_empty() {
        return None;
    }
    let items = parsed
        .program
        .items
        .into_iter()
        .filter(|(item, _)| {
            matches!(
                item,
                Item::Trait(_) | Item::TypeDecl(_) | Item::ExternBlock(_)
            ) || is_builtin_callable_impl(item)
        })
        .collect();
    Some(Program {
        items,
        module_doc: None,
        module_graph: None,
    })
}

pub(super) fn is_builtin_vec_iterator_impl(item: &Item) -> bool {
    let Item::Impl(impl_decl) = item else {
        return false;
    };
    let Some(trait_name) = impl_decl
        .trait_bound
        .as_ref()
        .map(|bound| bound.path.to_string())
    // TRANSITION(P1): deleted by A1 commit 2
    else {
        return false;
    };
    let TypeExpr::Named {
        path: named_path, ..
    } = &impl_decl.target_type.0
    else {
        return false;
    };
    let name = &named_path.to_string(); // TRANSITION(P1): deleted by A1 commit 2
    matches!(
        (trait_name.as_str(), name.as_str()),
        ("Iterator", "VecIter" | "HashMapIter" | "Generator") | ("IntoIterator", "Vec")
    )
}

/// Linker/metadata owner for one compiler-injected builtin impl target.
/// Synthetic cursors use their canonical std owner so a root user nominal
/// with the same leaf can emit an independent impl and method body. The input
/// comes only from the parsed `BUILTINS_HEW_SOURCE` program; ordinary source
/// declarations never call this projection.
pub(super) fn injected_builtin_impl_symbol_owner(source_name: &str) -> &str {
    match source_name {
        "VecIter" => "std.builtins.VecIter",
        "HashMapIter" => "std.builtins.HashMapIter",
        "ActorRequestOwner" => "std.builtins.ActorRequestOwner",
        _ => source_name,
    }
}

/// An inherent `std/builtins.hew` implementation with executable Hew bodies.
/// The embedded source is the authority: no receiver or method name selects
/// this path. Declarative runtime shims carry `#[extern_symbol]` and remain
/// metadata-only, while ordinary bodies are registered and lowered exactly as
/// user bodies are.
pub(super) fn is_builtin_source_body_impl(item: &Item) -> bool {
    matches!(item, Item::Impl(decl)
    if decl.trait_bound.is_none()
        && decl.type_params.is_none()
        && matches!(&decl.target_type.0, TypeExpr::Named { type_args: None, .. })
        && !decl.methods.is_empty()
        && decl.methods.iter().all(|method| {
            method.body.stmts.is_empty()
                && method.body.trailing_expr.is_some()
                && !method.attributes.iter().any(|attr| attr.name == "extern_symbol")
        }))
}

pub(super) fn is_builtin_receiver_impl(item: &Item) -> bool {
    is_builtin_vec_iterator_impl(item)
        || is_builtin_source_body_impl(item)
        || is_builtin_request_owner_impl(item)
}

pub(super) fn is_builtin_request_owner_impl(item: &Item) -> bool {
    matches!(item, Item::Impl(decl) if matches!(&decl.target_type.0,
        TypeExpr::Named { path, .. } if matches!(path.to_string().as_str(), "ActorRequestOwner" | "std.builtins.ActorRequestOwner")))
}

pub(super) fn is_builtin_callable_impl(item: &Item) -> bool {
    matches!(item, Item::Impl(impl_decl) if impl_decl.trait_bound.is_some())
        || is_builtin_source_body_impl(item)
        || is_builtin_request_owner_impl(item)
}

pub(super) fn impl_type_param_names(decl: &hew_parser::ast::ImplDecl) -> Vec<String> {
    decl.type_params
        .as_ref()
        .map(|params| params.iter().map(|param| param.name.to_string()).collect())
        .unwrap_or_default()
}

pub(super) fn check_builtin_callable_impl_program(
    program: &Program,
) -> Result<TypeCheckOutput, Box<HirDiagnostic>> {
    // The parsed embedded source uses private leaf spellings for its synthetic
    // cursor declarations. Type-check a projection whose impl targets carry
    // their exact compiler owner so declaration IDs and call facts cannot
    // collide with root user nominals of the same leaf. The executable HIR is
    // still lowered from the original source AST, preserving all source spans.
    let mut checker_program = program.clone();
    // These externs are already registered under their std.builtins owner.
    // Re-declaring them in the isolated checker's root would give a close
    // wrapper a different release identity from its lifecycle contract.
    checker_program
        .items
        .retain(|(item, _)| !matches!(item, Item::ExternBlock(_)));
    for (item, _) in &mut checker_program.items {
        let Item::Impl(impl_decl) = item else {
            continue;
        };
        canonicalize_injected_cursor_type_expr(&mut impl_decl.target_type.0);
        for alias in &mut impl_decl.type_aliases {
            canonicalize_injected_cursor_type_expr(&mut alias.ty.0);
        }
        for method in &mut impl_decl.methods {
            for param in &mut method.params {
                canonicalize_injected_cursor_type_expr(&mut param.ty.0);
            }
            if let Some(return_type) = &mut method.return_type {
                canonicalize_injected_cursor_type_expr(&mut return_type.0);
            }
        }
    }
    let mut checker =
        hew_types::Checker::new(hew_types::module_registry::ModuleRegistry::new(Vec::new()));
    let output = checker.check_embedded_builtins(&checker_program);
    if output.errors.is_empty() {
        return Ok(output);
    }

    let reason = output
        .errors
        .iter()
        .map(|error| error.message.as_str())
        .collect::<Vec<_>>()
        .join("; ");
    Err(Box::new(HirDiagnostic::new(
        HirDiagnosticKind::CheckerBoundaryViolation {
            name: "std/builtins.hew callable impls".to_string(),
            reason: reason.clone(),
        },
        0..0,
        format!("compiler-injected callable impls were not lowered: {reason}"),
    )))
}

pub(super) fn canonicalize_injected_cursor_type_expr(ty: &mut TypeExpr) {
    match ty {
        TypeExpr::QualifiedAssocPath(path) => {
            canonicalize_injected_cursor_type_expr(&mut path.base.0);
        }
        TypeExpr::Named { path, type_args } => {
            let name = path.to_string(); // TRANSITION(P1): deleted by A1 commit 2
            let canonical = injected_builtin_impl_symbol_owner(&name);
            if canonical != name {
                let span = path
                    .segments
                    .first()
                    .map(|(_, span)| span.clone())
                    .unwrap_or_default();
                path.segments = canonical
                    .split('.')
                    .map(|segment| (Ident::new(segment), span.clone()))
                    .collect();
            }
            if let Some(type_args) = type_args {
                for arg in type_args {
                    canonicalize_injected_cursor_type_expr(&mut arg.0);
                }
            }
        }
        TypeExpr::Result { ok, err }
        | TypeExpr::Fallible {
            success: ok,
            error: err,
        } => {
            canonicalize_injected_cursor_type_expr(&mut ok.0);
            canonicalize_injected_cursor_type_expr(&mut err.0);
        }
        TypeExpr::Option(inner)
        | TypeExpr::Slice(inner)
        | TypeExpr::Borrow(inner)
        | TypeExpr::Pointer { pointee: inner, .. } => {
            canonicalize_injected_cursor_type_expr(&mut inner.0);
        }
        TypeExpr::Tuple(elements) => {
            for element in elements {
                canonicalize_injected_cursor_type_expr(&mut element.0);
            }
        }
        TypeExpr::Array { element, .. } => {
            canonicalize_injected_cursor_type_expr(&mut element.0);
        }
        TypeExpr::Function {
            params,
            return_type,
            ..
        }
        | TypeExpr::ActorFn {
            params,
            return_type,
        } => {
            for param in params {
                canonicalize_injected_cursor_type_expr(&mut param.0);
            }
            canonicalize_injected_cursor_type_expr(&mut return_type.0);
        }
        TypeExpr::TraitObject(_) | TypeExpr::Infer => {}
    }
}
