//! Impl-body symbol planning and builtin impl-program injection.

use super::*;

/// Identify declarations whose old receiver-and-method linker spelling is
/// shared by another authored or materialised implementation body. This is a
/// physical symbol collision check over the checker's already-minted rows;
/// declaration selection still comes from exact occurrences and `DefId`.
pub(super) fn impl_method_symbol_collisions(
    defs: &hew_types::DefTable,
) -> HashSet<hew_types::DefId> {
    let mut groups: HashMap<(&str, &str, &str), Vec<hew_types::DefId>> = HashMap::new();
    for id in defs.ids() {
        if !matches!(
            defs.kind(id),
            hew_types::DeclarationKind::ImplMethod | hew_types::DeclarationKind::DefaultImplMethod
        ) {
            continue;
        }
        let path = defs.path(id);
        let parts = path
            .split_once("::<impl ")
            .or_else(|| path.split_once("::<default impl "));
        let Some((receiver, impl_and_method)) = parts else {
            continue;
        };
        let Some((impl_name, method)) = impl_and_method.rsplit_once(">::") else {
            continue;
        };
        let Some((_, declared_receiver)) = impl_name.split_once(" for ") else {
            continue;
        };
        groups
            .entry((receiver, declared_receiver, method))
            .or_default()
            .push(id);
    }
    groups
        .into_values()
        .filter(|ids| ids.len() > 1)
        .flatten()
        .collect()
}

impl LowerCtx {
    /// Preserve existing symbols for unique methods. When two checker
    /// declarations would share one symbol, add a reversible encoding of the
    /// declaration path to the owner portion of each symbol.
    pub(super) fn emitted_impl_method_symbol(
        &self,
        declaration: hew_types::DefId,
        symbol_owner: &str,
        method_name: &str,
    ) -> String {
        let symbol_owner = self.defs.owner(declaration).map_or_else(
            || symbol_owner.to_string(),
            |owner| self.defs.linkage_name(owner, symbol_owner),
        );
        match self.collision_owner_suffix(declaration) {
            Some(suffix) => crate::node::HirImplBlock::method_symbol(
                &format!("{symbol_owner}{suffix}"),
                method_name,
            ),
            None => crate::node::HirImplBlock::method_symbol(&symbol_owner, method_name),
        }
    }

    /// The reversible encoding of a declaration's path that
    /// `emitted_impl_method_symbol` appends to the owner of a method whose
    /// symbol another body shares.
    fn collision_owner_suffix(&self, declaration: hew_types::DefId) -> Option<String> {
        use std::fmt::Write as _;

        if !self.impl_method_symbol_collisions.contains(&declaration) {
            return None;
        }
        let mut encoded = String::with_capacity(self.defs.path(declaration).len() * 2 + 3);
        encoded.push_str("$i$");
        for byte in self.defs.path(declaration).bytes() {
            write!(&mut encoded, "{byte:02x}").expect("writing a symbol to String cannot fail");
        }
        Some(encoded)
    }
}

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
    defs: &hew_types::DefTable,
    declaration: hew_types::DefId,
    symbol: &str,
) -> bool {
    let Some((receiver, declaration_tail)) = defs.path(declaration).split_once("::<") else {
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
            .is_some_and(|suffix| suffix.starts_with("$$") || suffix.starts_with("$i$"))
        || symbol_owner
            .strip_prefix(receiver_leaf)
            .is_some_and(|suffix| suffix.starts_with("$$") || suffix.starts_with("$i$"))
}

pub(super) fn impl_body_symbols_alias_one_declaration(
    defs: &hew_types::DefTable,
    declaration: hew_types::DefId,
    left: &str,
    right: &str,
) -> bool {
    left != right
        && impl_body_symbol_matches_declaration(defs, declaration, left)
        && impl_body_symbol_matches_declaration(defs, declaration, right)
}

pub(super) fn declaration_owned_impl_body_symbol<'a>(
    defs: &hew_types::DefTable,
    declaration: hew_types::DefId,
    left: &'a str,
    right: &'a str,
) -> &'a str {
    let receiver = defs
        .path(declaration)
        .split_once("::<")
        .map_or("", |(receiver, _)| receiver);
    let is_declaration_owned = |symbol: &str| {
        symbol.rsplit_once("::").is_some_and(|(owner, _)| {
            owner == receiver
                || owner
                    .strip_prefix(receiver)
                    .is_some_and(|suffix| suffix.starts_with("$$") || suffix.starts_with("$i$"))
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
                        || impl_body_symbols_alias_one_declaration(
                            &ctx.defs,
                            *declaration,
                            existing,
                            symbol,
                        )
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
            .insert(*declaration, symbol.clone())
        {
            if existing == *symbol {
                continue;
            }
            if impl_body_symbols_alias_one_declaration(&ctx.defs, *declaration, &existing, symbol) {
                let selected = if ctx.impl_body_plan.compiler_selected.contains(declaration) {
                    existing
                } else {
                    declaration_owned_impl_body_symbol(&ctx.defs, *declaration, &existing, symbol)
                        .to_string()
                };
                ctx.impl_body_plan.symbols.insert(*declaration, selected);
                continue;
            }
            ctx.impl_body_plan.symbols.remove(declaration);
            conflict = true;
            ctx.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: format!("impl body `{}`", ctx.defs.path(*declaration)),
                    reason: format!(
                        "conflicting pre-lowering symbols `{existing}` and `{symbol}`"
                    ),
                },
                0..0,
                format!(
                    "implementation declaration `{}` selected two distinct emitted-body symbols: `{existing}` and `{symbol}`",
                    ctx.defs.path(*declaration)
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
    // The owner is the one every method's symbol is built from, so a
    // collision encoding a symbol carries is not part of it.
    let selected_owner = planned
        .iter()
        .find_map(|(declaration, _)| {
            let symbol = ctx.impl_body_plan.symbols.get(declaration)?;
            let (owner, _) = symbol.rsplit_once("::")?;
            let suffix = ctx.collision_owner_suffix(*declaration).unwrap_or_default();
            Some(
                owner
                    .strip_suffix(suffix.as_str())
                    .unwrap_or(owner)
                    .to_string(),
            )
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
    skip_methods: &HashSet<hew_types::DefId>,
) {
    let TypeExpr::Named { path: _, type_args } = &impl_decl.target_type.0 else {
        return;
    };
    if matches!(
        ctx.lower_type(&impl_decl.target_type),
        ResolvedTy::TypeParam { .. }
            | ResolvedTy::Named {
                head: hew_types::TypeHead::Param(_),
                ..
            }
    ) {
        return;
    }
    let impl_type_params = impl_type_parameters(ctx, impl_decl);
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
        let Some(declaration) =
            ctx.source_declaration(&method.fn_span, hew_types::DeclarationKind::ImplMethod, 0)
        else {
            continue;
        };
        if skip_methods.contains(&declaration) {
            continue;
        }
        let symbol = ctx.emitted_impl_method_symbol(
            declaration,
            &symbol_self_name,
            method.name.name.as_str(),
        );
        ctx.register_impl_method_fn_entry_at(method, &impl_type_params, &symbol);
        planned.push((declaration, symbol));
    }
    // A trait default the impl does NOT override is materialised as its own
    // body by `lower_impl_block`, under the checker's materialized-default
    // row (`synthetic_default_impl_body_declaration`) — the impl AST has no
    // method for the explicit-method loop above to reach. Plan those ids on the same authority: an imported module's
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
/// non-overridden filter, same `synthetic_default_impl_body_declaration` row,
/// same `method_symbol` — so the plan and the later emission cannot disagree.
pub(super) fn materialized_default_body_plan(
    ctx: &mut LowerCtx,
    impl_decl: &hew_parser::ast::ImplDecl,
    symbol_self_name: &str,
) -> Vec<(hew_types::DefId, String)> {
    let Some(trait_bound) = &impl_decl.trait_bound else {
        return Vec::new();
    };
    let Some(owner_key) = ctx.trait_declaration(&trait_bound.path) else {
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
        let Some(declaration) = ctx.synthetic_default_impl_body_declaration(
            *declaring_trait,
            Some(&self_ty),
            default_method.method.name.name,
        ) else {
            continue;
        };
        let symbol = ctx.emitted_impl_method_symbol(
            declaration,
            symbol_self_name,
            default_method.method.name.name.as_str(),
        );
        let method = trait_method_to_fn_decl(&default_method.method);
        let impl_parameters = impl_type_parameters(ctx, impl_decl);
        let previous_module = std::mem::replace(
            &mut ctx.current_module_name,
            default_method.source_module.clone(),
        );
        let previous_file =
            std::mem::replace(&mut ctx.current_module_idx, default_method.file_index);
        ctx.register_impl_method_fn_entry_at(&method, &impl_parameters, &symbol);
        ctx.current_module_name = previous_module;
        ctx.current_module_idx = previous_file;
        out.push((declaration, symbol));
    }
    out
}

pub(super) fn plan_imported_impl_bodies(
    ctx: &mut LowerCtx,
    program: &Program,
    preferred_modules: &HashSet<hew_parser::module::ModulePath>,
    span_indices: &hew_parser::module::FileSpanIndices,
    selected_builtin_graph_module: Option<&hew_parser::module::ModulePath>,
) {
    let empty_skips = HashSet::new();
    // Root impl bodies may be called before their item is emitted.
    for (item_idx, (item, _)) in program.items.iter().enumerate() {
        ctx.current_item_ordinal = item_idx;
        if let Item::Impl(impl_decl) = item {
            if let TypeExpr::Named {
                path: named_path, ..
            } = &impl_decl.target_type.0
            {
                let name = &named_path.to_string(); // TRANSITION(P1): deleted by A1 commit 2
                plan_impl_block_symbols(ctx, impl_decl, name, &empty_skips);
            }
        }
    }

    let Some(module_graph) = &program.module_graph else {
        return;
    };
    for module_id in &module_graph.topo_order {
        if *module_id == module_graph.root || !module_graph.modules.contains_key(module_id) {
            continue;
        }
        let module = &module_graph.modules[module_id];
        let source_module = module_id.dotted();
        if selected_builtin_graph_module == Some(module_id) {
            continue;
        }
        let previous_module = ctx.current_module_name.replace(source_module.clone());
        let previous_module_idx = ctx.current_module_idx;
        for (item_idx, (item, _)) in module.items.iter().enumerate() {
            ctx.current_item_ordinal = item_idx;
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
            let skip_methods = ctx.imported_impl_skip_methods(impl_decl);
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
        fn_span: method.span.clone(),
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
            let ps: Vec<String> = params
                .iter()
                .map(|parameter| {
                    format!(
                        "{}{}",
                        if parameter.ownership == hew_types::ParameterOwnership::Consume {
                            "consume "
                        } else {
                            ""
                        },
                        render_type_expr(&parameter.ty.0)
                    )
                })
                .collect();
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

/// Executable method availability follows checked declaration demands. A
/// static trait slot retains every matching receiver candidate until SIR
/// substitutes its receiver; a direct call names only its selected body.
#[derive(Default)]
pub(super) struct BuiltinMethodDemands {
    direct: HashSet<hew_types::DefId>,
    slots: HashSet<(hew_types::DefId, hew_types::DefId)>,
}

impl BuiltinMethodDemands {
    fn add(&mut self, target: &hew_types::CallTarget) {
        match target {
            hew_types::CallTarget::ImplMethod(declaration) => {
                self.direct.insert(*declaration);
            }
            hew_types::CallTarget::StaticTraitMethod {
                declaring_trait,
                method,
            } => {
                self.slots.insert((*declaring_trait, *method));
            }
            _ => {}
        }
    }

    pub(super) fn selects(&self, signature: &hew_types::check::FnSig) -> bool {
        signature.impl_method.as_ref().is_some_and(|method| {
            self.direct.contains(&method.declaration)
                || method
                    .trait_method
                    .is_some_and(|slot| self.slots.contains(&slot))
        })
    }

    fn add_output(&mut self, output: &TypeCheckOutput, owner: Option<hew_types::DefId>) {
        let mut bodies = HashSet::new();
        if let Some(owner) = owner {
            let mut pending = vec![hew_types::ty::EffectBody::Declaration(owner)];
            while let Some(body) = pending.pop() {
                if !bodies.insert(body.clone()) {
                    continue;
                }
                pending.extend(
                    output
                        .suspension_effects
                        .constructed_bodies
                        .get(&body)
                        .into_iter()
                        .flatten()
                        .cloned(),
                );
            }
        }
        let admits = |key: &SpanKey| {
            owner.is_none()
                || output
                    .suspension_effects
                    .call_owners
                    .get(key)
                    .is_some_and(|body| bodies.contains(body))
        };
        for (key, target) in &output.direct_call_targets {
            if admits(key) {
                self.add(target);
            }
        }
        for (key, rewrite) in &output.method_call_rewrites {
            if !admits(key) {
                continue;
            }
            match rewrite {
                hew_types::MethodCallRewrite::RewriteToFunction { target, .. }
                | hew_types::MethodCallRewrite::RewriteModuleQualifiedToFunction {
                    target, ..
                }
                | hew_types::MethodCallRewrite::StaticTraitDispatch { target, .. } => {
                    self.add(target);
                }
                _ => {}
            }
        }
        for (key, plan) in &output.for_loop_plans {
            if !admits(key) {
                continue;
            }
            for call in plan.into_iter.iter().chain(std::iter::once(&plan.next)) {
                self.add(&call.target);
            }
        }
        // An f-string calls Display implicitly. In a generic body such as
        // `Result<T, E: Display>::expect`, the concrete receiver is chosen at
        // monomorphisation, so retain every implementation of the checked
        // trait slot instead of looking for a source method-call rewrite.
        if !output.interpolation_display_types.is_empty() {
            if let Some(slot) = output.lang_items.display_method_identity() {
                self.slots.insert(slot);
            }
        }
    }

    pub(super) fn for_program(source: &TypeCheckOutput) -> Self {
        let mut demands = Self::default();
        demands.add_output(source, None);
        let mut expanded = HashSet::new();
        loop {
            let pending: Vec<_> = source
                .fn_sigs
                .iter()
                .filter(|(declaration, signature)| {
                    !expanded.contains(*declaration) && demands.selects(signature)
                })
                .map(|(declaration, _)| *declaration)
                .collect();
            if pending.is_empty() {
                break;
            }
            for declaration in pending {
                expanded.insert(declaration);
                demands.add_output(source, Some(declaration));
            }
        }
        demands
    }
}

/// Linker/metadata owner for one compiler-injected builtin impl target.
/// Synthetic cursors use their canonical std owner so a root user nominal
/// with the same leaf can emit an independent impl and method body. The input
/// comes only from the checker's selected trusted builtin program; ordinary
/// source declarations never call this projection.
pub(super) fn injected_builtin_impl_symbol_owner(source_name: &str) -> &str {
    match source_name {
        "VecIter" => "std.builtins.VecIter",
        "StringIter" => "std.builtins.StringIter",
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
        && matches!(&decl.target_type.0, TypeExpr::Named { .. })
        && !decl.methods.is_empty()
        && decl.methods.iter().all(|method| {
            !method.attributes.iter().any(|attr| attr.name == "extern_symbol")
        }))
}

pub(super) fn is_builtin_receiver_impl(item: &Item) -> bool {
    is_builtin_source_body_impl(item) || is_builtin_request_owner_impl(item)
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

pub(super) fn impl_type_parameters(
    ctx: &mut LowerCtx,
    decl: &hew_parser::ast::ImplDecl,
) -> Vec<hew_types::ParamHead> {
    ctx.source_type_parameters(
        &decl.target_type.1,
        decl.type_params.as_ref().map_or(0, Vec::len),
    )
}
