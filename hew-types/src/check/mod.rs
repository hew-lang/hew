//! Bidirectional type checker for Hew programs.

use crate::builtin_names::{builtin_named_type, builtin_named_types, BuiltinMethodRuntime};
use crate::error::{TypeError, TypeErrorKind};
use crate::module_registry::ModuleError;
use crate::traits::MarkerTrait;
use crate::ty::{Ty, TypeVar};
use crate::unify::unify;
use hew_parser::ast::{
    ActorDecl, ActorInit, AttributeArg, BinaryOp, Block, CallArg, ChildSpec, ConstDecl, Expr,
    ExternBlock, ExternFnDecl, FieldDecl, FnDecl, ImplDecl, ImportDecl, ImportSpec, Item,
    LambdaParam, Literal, MachineDecl, MatchArm, Param, Pattern, Program, ReceiveFnDecl,
    RecordDecl, RecordKind, Span, Spanned, Stmt, StringPart, SupervisorDecl, SupervisorStrategy,
    TraitDecl, TraitItem, TypeBodyItem, TypeDecl, TypeDeclKind, TypeExpr, TypeParam, UnaryOp,
    VariantKind, WhereClause, WireDecl, WireDeclKind,
};
use std::collections::{hash_map::Entry, HashMap, HashSet};
use std::sync::OnceLock;

pub(crate) mod admissibility;
mod calls;
mod coerce;
mod diagnostics;
mod expressions;
mod generics;
mod items;
mod methods;
mod patterns;
mod registration;
mod resolution;
mod statements;
#[cfg(test)]
mod tests;
mod types;
mod util;

pub use self::types::{
    ActorMethodKind, ActorSendAliasing, ActorSendCopyReason, ActorStateGuard, AllocationClass,
    AssignTargetKind, AssignTargetShape, Checker, ChildKind, ChildSlot, ClosureCaptureFact,
    ClosureCaptureMode, DynAssocBinding, DynCoercion, DynMethodCall, DynVtableEntry, DynVtableKey,
    ExecutionContextReader, FnSig, MachineMethodKind, MethodCallReceiverKind, MethodCallRewrite,
    SpanKey, StackHint, TypeCheckOutput, TypeDef, TypeDefKind, VariantDef,
};
use self::types::{
    ConstValue, DeferredBoundCheck, DeferredCastCheck, DeferredChannelMethodRewrite,
    DeferredHashMapAdmission, DeferredHashSetAdmission, DeferredInferenceHole,
    DeferredMonomorphicSite, DeferredVecAdmission, ImplAliasEntry, ImplAliasScope, ImportKey,
    IntegerTypeInfo, PendingLoweringFact, TraitAssociatedTypeInfo, TraitInfo,
    WasmUnsupportedFeature,
};
use self::util::{
    collect_unresolved_inference_vars, extract_float_literal_value, extract_integer_literal_value,
    first_infer_span_in_extern_fn, first_infer_span_in_type_expr, float_fits_type,
    integer_fits_type, integer_type_info, integer_type_range, is_float_literal, is_integer_literal,
    lookup_scoped_item, scoped_module_item_name,
};
use crate::lowering_facts::{LoweringFact, LoweringFactError};

static BUILTIN_FUNCTION_NAMES: OnceLock<HashSet<String>> = OnceLock::new();

#[must_use]
pub fn builtin_function_names() -> &'static HashSet<String> {
    BUILTIN_FUNCTION_NAMES.get_or_init(|| {
        let mut checker = Checker::default();
        checker.register_builtins();
        let mut names: HashSet<String> = checker
            .fn_sigs
            .keys()
            .filter(|name| !name.contains('.') && !name.contains("::"))
            .cloned()
            .collect();
        for builtin in builtin_named_types() {
            for method in builtin.methods {
                match method.runtime {
                    BuiltinMethodRuntime::None => {}
                    BuiltinMethodRuntime::Fixed(symbol) => {
                        if !symbol.contains('.') && !symbol.contains("::") {
                            names.insert(symbol.to_string());
                        }
                    }
                    BuiltinMethodRuntime::IntegerOverload {
                        default_symbol,
                        integer_symbol,
                    } => {
                        for symbol in [default_symbol, integer_symbol] {
                            if !symbol.contains('.') && !symbol.contains("::") {
                                names.insert(symbol.to_string());
                            }
                        }
                    }
                    BuiltinMethodRuntime::ElementOverload {
                        string_symbol,
                        bytes_symbol,
                    } => {
                        for symbol in [string_symbol, bytes_symbol] {
                            if !symbol.contains('.') && !symbol.contains("::") {
                                names.insert(symbol.to_string());
                            }
                        }
                    }
                }
            }
        }
        names
    })
}

fn resolve_builtin_result_output_type_args(ok_ty: Ty, err_ty: Ty) -> Option<(Ty, Ty)> {
    let ok_unresolved = ok_ty.has_inference_var();
    let err_unresolved = err_ty.has_inference_var();
    match (ok_unresolved, err_unresolved) {
        (false, false) => Some((ok_ty, err_ty)),
        (false, true) => Some((ok_ty.clone(), ok_ty)),
        (true, false) => Some((err_ty.clone(), err_ty)),
        (true, true) => None,
    }
}

fn patch_builtin_result_output_type(_ty: Ty, ok_ty: &Ty, err_ty: &Ty) -> Ty {
    Ty::result(ok_ty.clone(), err_ty.clone())
}

impl Checker {
    /// Log function names that accept keyword arguments for structured fields.
    const LOG_KWARGS_FUNCTIONS: &'static [&'static str] = &[
        // C extern (used by codegen interception for legacy compatibility)
        "hew_log_emit",
        // Wrapper function (clean) names from log.hew
        "error",
        "warn",
        "info",
        "debug",
        "trace",
    ];

    /// Pass 3: Check all bodies
    #[expect(
        clippy::too_many_lines,
        reason = "orchestrates the full check pipeline: non-root body pass, root pass, \
                  type resolution, warning emission, and deferred-hole drain; \
                  each phase is a distinct step and extracting further helpers \
                  would only obscure the pipeline order"
    )]
    pub fn check_program(&mut self, program: &Program) -> TypeCheckOutput {
        self.register_builtins();
        self.collect_types(program);
        self.collect_functions(program);

        // Check non-root module_graph bodies first (dependencies before dependents).
        // Mirrors the traversal order in collect_functions so every registered
        // signature has its body validated, not just the root module.
        // Body-level deferred inference holes (e.g. `as _` cast targets, lambda
        // parameter `_` types) produced here accumulate in
        // `self.deferred_inference_holes` and are drained by
        // `report_unresolved_inference_holes` at the end of check_program.
        if let Some(ref mg) = program.module_graph {
            for mod_id in &mg.topo_order {
                if *mod_id == mg.root {
                    continue;
                }
                if let Some(module) = mg.modules.get(mod_id) {
                    let module_name = mod_id.path.join(".");
                    self.current_module = Some(module_name.clone());
                    // Temporarily scope local_type_defs / local_trait_defs to
                    // this module so orphan-rule checks see module-local
                    // definitions and locally_non_generic works correctly.
                    let saved_local_type_defs = self.local_type_defs.clone();
                    let saved_local_trait_defs = self.local_trait_defs.clone();
                    for (item, _) in &module.items {
                        match item {
                            Item::TypeDecl(td) => {
                                self.local_type_defs.insert(td.name.clone());
                            }
                            Item::Trait(tr) => {
                                self.local_trait_defs.insert(tr.name.clone());
                            }
                            _ => {}
                        }
                    }

                    // Snapshot error/warning counts before body-checking this module.
                    // Everything emitted during the body check that still has
                    // `source_module: None` is tagged below with the module name,
                    // so the CLI can route it to the correct source file.
                    let err_before = self.errors.len();
                    let warn_before = self.warnings.len();

                    for (item, span) in &module.items {
                        self.check_item(item, span);
                    }

                    // Tag diagnostics that were not already tagged (e.g. by a
                    // nested call that already knew the origin).
                    for e in &mut self.errors[err_before..] {
                        if e.source_module.is_none() {
                            e.source_module = Some(module_name.clone());
                        }
                    }
                    for w in &mut self.warnings[warn_before..] {
                        if w.source_module.is_none() {
                            w.source_module = Some(module_name.clone());
                        }
                    }

                    self.local_type_defs = saved_local_type_defs;
                    self.local_trait_defs = saved_local_trait_defs;
                }
            }
            self.current_module = None;
        }

        for (item, span) in &program.items {
            self.check_item(item, span);
        }

        // Apply final substitutions to all recorded types
        let mut expr_types: HashMap<SpanKey, Ty> = self
            .expr_types
            .iter()
            .map(|(k, v)| (k.clone(), self.subst.resolve(v)))
            .collect();

        // Emit unused import warnings
        for (key, (import_span, stored_module)) in &self.import_spans {
            if !self.used_modules.borrow().contains(key) {
                self.warnings.push(TypeError {
                    severity: crate::error::Severity::Warning,
                    kind: TypeErrorKind::UnusedImport,
                    span: import_span.clone(),
                    message: format!("unused import: `{}`", key.short_name),
                    notes: vec![],
                    suggestions: vec!["remove this import".to_string()],
                    source_module: stored_module.clone(),
                });
            }
        }

        self.emit_dead_code_warnings();

        self.default_unconstrained_range_types(&expr_types);
        // Re-record range bound spans with their concrete element types
        // (resolved after inference + defaulting) and validate fits.
        self.apply_deferred_range_bound_types(&mut expr_types);
        // Drain deferred trait-bound checks after inference/defaulting settles,
        // but before unresolved-hole reporting so concrete bound failures stay
        // specific and unresolved holes remain authoritative.
        self.drain_deferred_bound_checks();

        let resolved_builtin_result_output_type_args: HashMap<SpanKey, (Ty, Ty)> =
            std::mem::take(&mut self.builtin_result_output_type_args)
                .into_iter()
                .filter_map(|(k, (ok_ty, err_ty))| {
                    let ok_ty = self.subst.resolve(&ok_ty).materialize_literal_defaults();
                    let err_ty = self.subst.resolve(&err_ty).materialize_literal_defaults();
                    resolve_builtin_result_output_type_args(ok_ty, err_ty).map(|args| (k, args))
                })
                .collect();

        // Also resolve inferred call type args so the enrichment layer can
        // fill in explicit type annotations for the codegen.
        let mut resolved_call_type_args: HashMap<SpanKey, Vec<Ty>> =
            std::mem::take(&mut self.call_type_args)
                .into_iter()
                .map(|(k, args)| {
                    let resolved: Vec<Ty> = args
                        .iter()
                        .map(|a| self.subst.resolve(a).materialize_literal_defaults())
                        .collect();
                    (k, resolved)
                })
                .collect();

        // Same boundary resolution for record-init type args.
        // Mirrors `resolved_call_type_args` so downstream consumers see
        // fully-resolved, literal-defaulted `Ty` values.
        let mut resolved_record_init_type_args: HashMap<SpanKey, Vec<Ty>> =
            std::mem::take(&mut self.record_init_type_args)
                .into_iter()
                .map(|(k, args)| {
                    let resolved: Vec<Ty> = args
                        .iter()
                        .map(|a| self.subst.resolve(a).materialize_literal_defaults())
                        .collect();
                    (k, resolved)
                })
                .collect();
        let resolved_closure_capture_facts = std::mem::take(&mut self.closure_capture_facts)
            .into_iter()
            .map(|(k, facts)| {
                let resolved = facts
                    .into_iter()
                    .map(|mut fact| {
                        fact.ty = self.subst.resolve(&fact.ty).materialize_literal_defaults();
                        fact
                    })
                    .collect();
                (k, resolved)
            })
            .collect();
        let resolved_actor_method_dispatch = std::mem::take(&mut self.actor_method_dispatch)
            .into_iter()
            .map(|(k, kind)| {
                let resolved_kind = match kind {
                    ActorMethodKind::Fire(method_id) => ActorMethodKind::Fire(method_id),
                    ActorMethodKind::Ask(method_id, reply_ty) => ActorMethodKind::Ask(
                        method_id,
                        self.subst.resolve(&reply_ty).materialize_literal_defaults(),
                    ),
                };
                (k, resolved_kind)
            })
            .collect();
        self.actor_method_dispatch = resolved_actor_method_dispatch;

        // Move data out of Checker — it is not used after check_program.
        // Resolve any remaining type variables in expr_types via the
        // substitution so the enrichment layer sees concrete types, then
        // materialize surviving literal kinds at the checked-output boundary.
        let mut resolved_expr_types: HashMap<SpanKey, Ty> = expr_types
            .into_iter()
            .map(|(k, v)| {
                let mut resolved = self.subst.resolve(&v).materialize_literal_defaults();
                if let Some((ok_ty, err_ty)) = resolved_builtin_result_output_type_args.get(&k) {
                    resolved = patch_builtin_result_output_type(resolved, ok_ty, err_ty);
                }
                (k, resolved)
            })
            .collect();

        let mut resolved_type_defs: HashMap<String, TypeDef> = std::mem::take(&mut self.type_defs)
            .into_iter()
            .map(|(name, type_def)| {
                let resolved = self.resolve_type_def(&type_def);
                (name, resolved)
            })
            .collect();

        let mut resolved_fn_sigs: HashMap<String, FnSig> = std::mem::take(&mut self.fn_sigs)
            .into_iter()
            .map(|(name, sig)| {
                let resolved = self.resolve_fn_sig(&sig);
                (name, resolved)
            })
            .collect();

        self.validate_checker_output_contract(
            &mut resolved_expr_types,
            &mut resolved_type_defs,
            &mut resolved_fn_sigs,
            &mut resolved_call_type_args,
            &mut resolved_record_init_type_args,
        );
        let mut resolved_lowering_facts = self.finalize_lowering_facts();
        admissibility::validate_lowering_facts_output_contract(
            &mut resolved_lowering_facts,
            &resolved_expr_types,
        );
        self.finalize_hashmap_admission();
        self.finalize_hashset_admission();
        self.finalize_vec_admission();
        self.finalize_channel_rewrites();

        self.report_unresolved_inference_holes(program);
        self.report_unresolved_monomorphic_sites();

        // Q87 slice 1: build the actor protocol descriptor side-table once
        // all handler signatures are resolved. Collisions (two `receive fn`s
        // hashing to the same msg_id) emit `ActorProtocolCollision`
        // diagnostics and the offending actor is **absent** from the map —
        // MIR/codegen treat a missing entry for an actor with handlers as
        // fail-closed. There is no fallback `enumerate()` path: the
        // descriptor is the only msg_id authority downstream.
        let actor_protocol_descriptors =
            build_actor_protocol_descriptors(program, &resolved_fn_sigs, &mut self.errors);

        let mut output = TypeCheckOutput {
            expr_types: resolved_expr_types,
            method_call_receiver_kinds: std::mem::take(&mut self.method_call_receiver_kinds),
            method_call_consumes_receiver: std::mem::take(&mut self.method_call_consumes_receiver),
            actor_send_aliasing: std::mem::take(&mut self.actor_send_aliasing),
            actor_handler_state_guards: std::mem::take(&mut self.actor_handler_state_guards),
            actor_max_heap: std::mem::take(&mut self.actor_max_heap),
            supervisor_child_slots: std::mem::take(&mut self.supervisor_child_slots),
            lowering_facts: resolved_lowering_facts,
            method_call_rewrites: std::mem::take(&mut self.method_call_rewrites),
            actor_method_dispatch: std::mem::take(&mut self.actor_method_dispatch),
            machine_method_dispatch: std::mem::take(&mut self.machine_method_dispatch),
            assign_target_kinds: std::mem::take(&mut self.assign_target_kinds),
            assign_target_shapes: std::mem::take(&mut self.assign_target_shapes),
            errors: std::mem::take(&mut self.errors),
            warnings: std::mem::take(&mut self.warnings),
            type_defs: resolved_type_defs,
            fn_sigs: resolved_fn_sigs,
            handle_bearing_structs: {
                // Flush any pending dirty registration before the set is moved
                // out — the output layer uses this set for codegen decisions.
                self.ensure_handle_bearing_fresh();
                std::mem::take(&mut self.handle_bearing_structs)
            },
            cycle_capable_actors: HashSet::new(),
            user_modules: std::mem::take(&mut self.user_modules),
            call_type_args: resolved_call_type_args,
            record_init_type_args: resolved_record_init_type_args,
            stack_hints: std::mem::take(&mut self.stack_hints),
            dyn_trait_coercions: std::mem::take(&mut self.dyn_trait_coercions),
            dyn_trait_method_calls: std::mem::take(&mut self.dyn_trait_method_calls),
            closure_capture_facts: resolved_closure_capture_facts,
            actor_protocol_descriptors,
            intrinsic_declarations: std::mem::take(&mut self.intrinsic_declarations),
        };

        // Detect actor reference cycles and emit warnings.
        let (cycle_capable, cycles) = crate::cycle::detect_actor_ref_cycles(&output.type_defs);
        for cycle_actors in &cycles {
            let desc = cycle_actors.join(" -> ");
            let span = cycle_actors
                .iter()
                .filter_map(|name| self.type_def_spans.get(name).cloned())
                .min_by_key(|span| span.start)
                .unwrap_or(0..0);
            output
                .warnings
                .push(TypeError::actor_ref_cycle(span, &desc));
        }
        output.cycle_capable_actors = cycle_capable;

        output
    }
}

/// Collect every `actor` declaration in the program (root items + each
/// module-graph module). The walk is read-only so it can run after the
/// checker has frozen its mutable state.
fn collect_program_actors(program: &Program) -> Vec<&ActorDecl> {
    let mut actors: Vec<&ActorDecl> = Vec::new();
    for (item, _) in &program.items {
        if let Item::Actor(ad) = item {
            actors.push(ad);
        }
    }
    if let Some(mg) = &program.module_graph {
        for module in mg.modules.values() {
            for (item, _) in &module.items {
                if let Item::Actor(ad) = item {
                    actors.push(ad);
                }
            }
        }
    }
    actors
}

/// Build [`ActorProtocolDescriptor`]s for every actor in the program, using
/// each `receive fn`'s resolved type signature (param types + return type)
/// drawn from `fn_sigs` keyed `"Actor::handler"`.
///
/// On collision: emits a `TypeErrorKind::ActorProtocolCollision` diagnostic
/// against the second-colliding handler's span and **omits** the actor from
/// the returned map. Downstream consumers must treat a missing entry
/// fail-closed.
///
/// On unresolved signatures (e.g. an upstream type error left a handler
/// param without a concrete `ResolvedTy`): silently skips the actor. The
/// surfacing diagnostic is already emitted elsewhere; piling on with a
/// derivative error here would be noise.
fn build_actor_protocol_descriptors(
    program: &Program,
    fn_sigs: &HashMap<String, crate::check::types::FnSig>,
    errors: &mut Vec<TypeError>,
) -> HashMap<String, crate::actor_protocol::ActorProtocolDescriptor> {
    let mut descriptors: HashMap<String, crate::actor_protocol::ActorProtocolDescriptor> =
        HashMap::new();
    for ad in collect_program_actors(program) {
        if ad.receive_fns.is_empty() {
            continue;
        }
        let mut specs: Vec<crate::actor_protocol::ActorHandlerSpec> =
            Vec::with_capacity(ad.receive_fns.len());
        let mut all_signatures_resolved = true;
        for rf in &ad.receive_fns {
            let key = format!("{}::{}", ad.name, rf.name);
            let Some(sig) = fn_sigs.get(&key) else {
                all_signatures_resolved = false;
                break;
            };
            let mut param_tys: Vec<crate::ResolvedTy> = Vec::with_capacity(sig.params.len());
            let mut any_unresolved = false;
            for p in &sig.params {
                if let Ok(rt) = crate::ResolvedTy::from_ty(p) {
                    param_tys.push(rt);
                } else {
                    any_unresolved = true;
                    break;
                }
            }
            if any_unresolved {
                all_signatures_resolved = false;
                break;
            }
            let Ok(return_ty) = crate::ResolvedTy::from_ty(&sig.return_type) else {
                all_signatures_resolved = false;
                break;
            };
            // Symbol mangling is owned by MIR/codegen; for slice 1 we record
            // a stable surface-derived symbol string so the descriptor row
            // is self-describing. Downstream consumers may continue to
            // derive their own emit name today; subsequent Q87 slices route
            // codegen through this `symbol` field.
            let symbol = format!("{}__{}", ad.name, rf.name);
            specs.push(crate::actor_protocol::ActorHandlerSpec {
                name: rf.name.clone(),
                param_tys,
                return_ty,
                symbol,
            });
        }
        if !all_signatures_resolved {
            // A handler signature failed to resolve; the underlying type
            // error is already in `errors`. Skip publishing a partial
            // descriptor — fail-closed downstream is preferable to a
            // half-populated protocol.
            continue;
        }

        match crate::actor_protocol::ActorProtocolDescriptor::from_handlers(ad.name.clone(), &specs)
        {
            Ok(descriptor) => {
                descriptors.insert(ad.name.clone(), descriptor);
            }
            Err(collision) => {
                // Pin the diagnostic to the second-colliding handler's span
                // so the user can jump straight to one of the two offenders.
                // The diagnostic message names both, and the hint mentions
                // the (not-yet-parseable) `#[msg_id(N)]` opt-in attribute
                // so the wording stays accurate when the later Q87 slice
                // lands the attribute.
                let span = ad
                    .receive_fns
                    .iter()
                    .find(|rf| rf.name == collision.handler_b)
                    .map_or(0..0, |rf| rf.span.clone());
                let message = format!(
                    "actor `{}` has two `receive fn`s with the same msg_id 0x{:08x}: `{}` and `{}`",
                    collision.actor_name,
                    collision.msg_id,
                    collision.handler_a,
                    collision.handler_b,
                );
                let mut err = TypeError::new(
                    TypeErrorKind::ActorProtocolCollision {
                        actor_name: collision.actor_name.clone(),
                        handler_a: collision.handler_a.clone(),
                        handler_b: collision.handler_b.clone(),
                        msg_id: collision.msg_id,
                    },
                    span,
                    message,
                );
                err = err.with_suggestion(format!(
                    "rename `{}` or `{}` so their fully-qualified names hash to distinct \
                     msg_ids; explicit `#[msg_id(N)]` pinning is reserved for a future \
                     release (not yet supported)",
                    collision.handler_a, collision.handler_b
                ));
                errors.push(err);
                // Fail-closed: descriptor is absent from the map.
            }
        }
    }
    descriptors
}
