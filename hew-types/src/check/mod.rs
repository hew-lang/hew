//! Bidirectional type checker for Hew programs.

use crate::builtin_names::{builtin_named_type, builtin_named_types, BuiltinMethodRuntime};
use crate::error::{TypeError, TypeErrorKind};
use crate::module_registry::ModuleError;
use crate::resolved_ty::{BoundaryError, ResolvedTy};
use crate::traits::MarkerTrait;
use crate::ty::{Ty, TypeVar};
use crate::unify::unify;
use hew_parser::ast::{
    ActorDecl, ActorInit, Attribute, AttributeArg, BinaryOp, Block, CallArg, ChildSpec, ConstDecl,
    Expr, ExternBlock, ExternFnDecl, FieldDecl, FnDecl, ImplDecl, ImportDecl, ImportSpec, Item,
    LambdaParam, Literal, MachineDecl, MatchArm, Param, Pattern, Program, ReceiveFnDecl,
    RecordDecl, RecordKind, RestartPolicy, Span, Spanned, Stmt, StringPart, SupervisorDecl,
    SupervisorStrategy, TraitBound, TraitDecl, TraitItem, TypeBodyItem, TypeDecl, TypeDeclKind,
    TypeExpr, TypeParam, UnaryOp, VariantKind, WhereClause, WireDecl, WireDeclKind,
};
use std::collections::{hash_map::Entry, HashMap, HashSet};
use std::sync::OnceLock;

pub(crate) mod admissibility;
mod calls;
mod closure_inference;
mod coerce;
pub mod const_eval;
mod diagnostics;
pub mod dispatch;
pub use self::dispatch::{
    Bound, CallAbiHint, HashMapMethod, HashSetMethod, ImplDef, ImplId, ImplRegistry, LookupError,
    MethodTarget, MethodTargetFamily, ResolvedCall, RuntimeAbi, TyPattern, VecMethod,
};
mod expressions;
mod generics;
mod items;
mod methods;
pub use self::methods::collection_dispatch_registry_for_tests;
mod patterns;
mod registration;
pub use registration::intrinsic_floor_modules;
mod resolution;
mod statements;
#[cfg(test)]
mod tests;
mod types;
mod util;
mod visibility;

use self::types::{
    ActorFieldInfo, ActorInitParamInfo, ConstValue, DeferredBoundCheck, DeferredCastCheck,
    DeferredChannelMethodRewrite, DeferredHashMapAdmission, DeferredHashSetAdmission,
    DeferredInferenceHole, DeferredMonomorphicSite, DeferredVecAdmission, ImplAliasEntry,
    ImplAliasScope, ImportKey, IndexContext, IntegerTypeInfo, PendingLoweringFact,
    TraitAssociatedTypeInfo, TraitInfo, TypeParamScope, WasmUnsupportedFeature,
};
pub use self::types::{
    ActorMethodKind, ActorSendAliasing, ActorSendCopyReason, ActorStateGuard, AllocationClass,
    ArmResolution, AssignTargetKind, AssignTargetShape, CaptureModeOrigin, Checker, ChildKind,
    ChildSlot, ClosureCaptureFact, ClosureCaptureMode, ClosureEscapeFact, ClosureEscapeKind,
    ClosureEscapeRule, DynAssocBinding, DynCoercion, DynMethodCall, DynVtableEntry, DynVtableKey,
    ExecutionContextReader, FnSig, MachineMethodKind, MathGenericOp, MethodCallReceiverKind,
    MethodCallRewrite, NumericMethodFamily, NumericMethodLowering, NumericMethodOp,
    NumericSignedness, NumericWidth, OptionResultMethod, PatternKind, PayloadBinding,
    PayloadVariantPattern, SpanKey, StackHint, TypeCheckOutput, TypeDef, TypeDefKind, VariantDef,
    VariantMatch, VecHigherOrderOp, WidthCastKind, WidthCastLowering, WireCodecDirection,
    WireFieldLayout, WireLayoutEntry, WireLayoutTable,
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

fn value_type_kind_label(kind: TypeDefKind) -> &'static str {
    match kind {
        TypeDefKind::Enum => "enum",
        TypeDefKind::Record => "record",
        TypeDefKind::Struct => "struct",
        TypeDefKind::Actor | TypeDefKind::Machine => "type",
    }
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

        // Build the actor protocol descriptor side-table BEFORE body checking.
        //
        // The descriptor maps each `receive fn` to its stable, hash-derived
        // `msg_id` (`SipHash-1-3("Actor::handler")`). Body checking needs this
        // map available because the active-mode `conn.attach(this)` coercion
        // (`LocalPid<Actor>` → `LocalPid<ConnectionHandler>`) consults
        // `actor_satisfies_handler_trait`, which reads
        // `self.actor_protocol_descriptors` to confirm an actor's `receive fn`s
        // structurally satisfy the handler trait. Building it after body
        // checking (the pre-Q90 placement) left the map empty during the
        // coercion, so the predicate always failed and the attach surface had
        // no working caller.
        //
        // `build_actor_protocol_descriptors` needs only `program` plus the
        // registered fn signatures, both of which `collect_functions` has
        // finalized by this point. Receive-fn parameter types are concrete
        // declared annotations (no inference vars to resolve), so the
        // descriptor built here is identical to one built from the post-
        // substitution `resolved_fn_sigs` snapshot later in the pipeline; the
        // published descriptor in `TypeCheckOutput` reads this same field
        // rather than rebuilding.
        //
        // `fn_sigs` is borrowed immutably while `errors` is borrowed mutably;
        // both are `self` fields, so swap `fn_sigs` out across the build to
        // keep the borrow checker happy without cloning the whole signature map.
        let fn_sigs_for_descriptors = std::mem::take(&mut self.fn_sigs);
        self.actor_protocol_descriptors =
            build_actor_protocol_descriptors(program, &fn_sigs_for_descriptors, &mut self.errors);
        self.fn_sigs = fn_sigs_for_descriptors;

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
                    // Assign a fresh 1-based index so `record_type` stamps a
                    // per-file discriminator onto `SpanKey`, preventing byte-
                    // offset collisions across module files.
                    self.current_module_idx += 1;
                    // Temporarily scope local_type_defs / local_trait_defs to
                    // this module so orphan-rule checks see module-local
                    // definitions and locally_non_generic works correctly.
                    let saved_local_type_defs = self.local_type_defs.clone();
                    let saved_source_type_defs = self.source_type_defs.clone();
                    let saved_local_trait_defs = self.local_trait_defs.clone();
                    for (item, _) in &module.items {
                        match item {
                            Item::TypeDecl(td) => {
                                self.local_type_defs.insert(td.name.clone());
                                self.source_type_defs.insert(td.name.clone());
                            }
                            Item::Machine(md) => {
                                // Parallel to the TypeDecl arm: seed the machine's
                                // name so orphan-rule and locally_non_generic checks
                                // inside the body pass see it as locally-defined.
                                // Also seed the synthesised `<Name>Event` companion
                                // type so event-typed parameters and bare event
                                // ctors in imported machine modules resolve as
                                // locally-non-generic.
                                self.local_type_defs.insert(md.name.clone());
                                self.source_type_defs.insert(md.name.clone());
                                let event_type_name = format!("{}Event", md.name);
                                self.local_type_defs.insert(event_type_name.clone());
                                self.source_type_defs.insert(event_type_name);
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
                    self.source_type_defs = saved_source_type_defs;
                    self.local_trait_defs = saved_local_trait_defs;
                }
            }
            self.current_module = None;
            // Restore to 0 so subsequent root-level checks (program.items below)
            // still use module_idx = 0 for their span keys.
            self.current_module_idx = 0;
        }

        for (item, span) in &program.items {
            self.check_item(item, span);
        }

        // Closure escape classification — runs after all bodies have
        // been type-checked. Walks each fn body (root + modules) looking
        // for closure literal sites and computes per-closure
        // `ClosureEscapeFact`. Conservative default: `Escapes` unless
        // positively proven `Local` or `Forked`.
        self.classify_closure_escapes(program);

        // Fail-closed defense: every closure literal in the program
        // must have BOTH a `ClosureCaptureFact` ledger AND a
        // `ClosureEscapeFact` by the time the checker hands off to
        // MIR-lowering. A missing entry is a structural bug in this
        // checker (the lambda site was never visited by `check_lambda`
        // or `classify_closure_escapes`) — not a user-code shape — so
        // we emit a hard diagnostic rather than letting MIR observe a
        // silently-defaulted closure.
        self.validate_closure_facts_complete(program);

        // Apply final substitutions to all recorded types
        let mut expr_types: HashMap<SpanKey, Ty> = self
            .expr_types
            .iter()
            .map(|(k, v)| (k.clone(), self.subst.resolve(v)))
            .collect();

        // Emit unused import warnings. A REPL fragment imports modules it will
        // reference on later inputs, so suppress this lint for eval fragments.
        if !self.repl_fragment {
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
        for cycle in crate::cycle::detect_recursive_value_type_cycles(&resolved_type_defs) {
            let span = self
                .type_def_spans
                .get(&cycle.edge.from)
                .cloned()
                .unwrap_or(0..0);
            let type_kind = resolved_type_defs
                .get(&cycle.edge.from)
                .map_or("type", |type_def| value_type_kind_label(type_def.kind));
            self.errors.push(TypeError::recursive_value_type(
                span,
                type_kind,
                &cycle.edge.from,
                &cycle.edge.member_desc,
                &cycle.edge.to,
            ));
        }
        // The layout-backed HashMap/HashSet admission finalizers below still
        // consult `self.type_defs` to prove named record hash-eligibility and
        // compute key/value ABI sizes. `resolved_type_defs` is the authoritative
        // post-substitution snapshot after the checked-output boundary pass, so
        // restore it into the checker before draining those deferred queues.
        self.type_defs = resolved_type_defs.clone();
        let mut resolved_lowering_facts = self.finalize_lowering_facts();
        admissibility::validate_lowering_facts_output_contract(
            &mut resolved_lowering_facts,
            &resolved_expr_types,
        );
        self.finalize_hashmap_admission();
        self.finalize_hashset_admission();
        self.finalize_vec_admission();
        self.finalize_channel_rewrites();

        // Prune any layout facts whose span is not in the validated expr_types map.
        // This prevents orphaned layout facts (from expressions that were pruned
        // by validate_checker_output_contract) from reaching codegen.
        self.hashmap_layout_facts
            .retain(|key, _| resolved_expr_types.contains_key(key));
        self.hashset_layout_facts
            .retain(|key, _| resolved_expr_types.contains_key(key));

        self.report_unresolved_inference_holes(program);
        self.report_unresolved_monomorphic_sites();

        // Q87 slice 1: the actor protocol descriptor side-table is the only
        // msg_id authority downstream. Collisions (two `receive fn`s hashing
        // to the same msg_id) emit `ActorProtocolCollision` diagnostics and
        // the offending actor is **absent** from the map — MIR/codegen treat a
        // missing entry for an actor with handlers as fail-closed. There is no
        // fallback `enumerate()` path.
        //
        // The map was built once before body checking (so the active-mode
        // `conn.attach(this)` coercion could read it) and cached in
        // `self.actor_protocol_descriptors`. Take it for the typed output here
        // rather than rebuilding: a rebuild would re-run collision detection
        // and double-emit the diagnostics.
        let actor_protocol_descriptors = std::mem::take(&mut self.actor_protocol_descriptors);

        // Compute the set of monomorphic builtin enum names that landed in
        // `type_defs` via internal pre-registration (e.g.
        // `register_builtins_hew_impls`) without a matching user-source
        // TypeDecl. Sandbox-WASM emit consults this to suppress its eager
        // `type_defs` sweep for builtin shapes the user did not author.
        let internal_builtin_enum_names: HashSet<String> = {
            use crate::builtin_enums::monomorphic_builtin_enums;
            monomorphic_builtin_enums()
                .iter()
                .filter(|spec| {
                    spec.suppress_from_sandbox_emit
                        && resolved_type_defs.contains_key(spec.name)
                        && !self.source_type_defs.contains(spec.name)
                })
                .map(|spec| spec.name.to_string())
                .collect()
        };

        // W4.047 P1.1 — build the typed `resolved_expr_types` handoff map.
        //
        // Run the single authorised `Ty -> ResolvedTy` conversion over every
        // surviving (post-contract, post-prune) `expr_types` entry. A
        // successful conversion proves the type is concrete and admissible;
        // the entry is stored in the typed map. A conversion *failure* is only
        // legitimate for a *covered* generic inference var — a pre-monomorphi-
        // zation type-parameter position that `validate_expr_output_contract`
        // deliberately retained (its unresolved vars are a subset of the
        // tracked holes) and that monomorphization resolves downstream. Those
        // spans are legitimately absent from the typed map.
        //
        // ANY OTHER conversion failure (a leaked `Ty::Error`, an unmaterialized
        // numeric literal, or a var-free unresolved associated projection) is a
        // fail-open totality gap: an inadmissible type the output contract
        // should already have pruned + diagnosed *before* the checker→HIR
        // handoff. The `debug_assert!` below is the totality net — it fires
        // loudly in debug/test/CI if such a type survives, and is compiled out
        // of release so this remains a pure, zero-behaviour-change substrate
        // add (HIR still drives lowering off `expr_types` in Phase 1).
        let resolved_expr_types_typed: HashMap<SpanKey, ResolvedTy> = {
            // The totality invariant holds for *accepted* programs only: "every
            // accepted expression span has a concrete ResolvedTy, OR the program
            // was rejected." A program that emitted hard errors is rejected and
            // hands off no CheckedProgram, so error-recovery placeholders
            // legitimately survive in its `expr_types`. (Errors are still in
            // `self.errors` here — they are moved into the output below.)
            let program_accepted = self.errors.is_empty();
            let mut typed = HashMap::with_capacity(resolved_expr_types.len());
            for (key, ty) in &resolved_expr_types {
                match ResolvedTy::from_ty(ty) {
                    Ok(resolved) => {
                        typed.insert(key.clone(), resolved);
                    }
                    Err(boundary_err) => {
                        // `ResolvedTy` cannot represent the four checker-internal
                        // states, so a conversion failure means the span is not
                        // concrete and is (correctly) omitted from the typed map.
                        // We classify the omission to keep the totality net honest:
                        //
                        //  - `UnresolvedInference` / `UnresolvedAssocProjection`:
                        //    a *covered* generic position (a type-parameter var or
                        //    an associated-type projection over one) in a
                        //    pre-monomorphization body. `validate_expr_output_
                        //    contract` deliberately retains these (their unresolved
                        //    vars are a subset of the tracked holes); monomorphi-
                        //    zation resolves them. Legitimately absent — no gap.
                        //
                        //  - `TaintedError`: an error-recovery placeholder. In a
                        //    *rejected* program this is expected. In an *accepted*
                        //    program it is an upstream checker bug (the contract
                        //    gate prunes leaked inference vars but not `Ty::Error`):
                        //    a span typed `Ty::Error` with no diagnostic, masked
                        //    downstream by the fail-open `.unwrap_or(Unit)`. The
                        //    typed map omits it (fail-closed absence, not a Unit
                        //    guess); the finding is reported for a dedicated lane
                        //    and Phase 3 converts the downstream miss to a hard
                        //    `CheckerBoundaryViolation`. KNOWN INSTANCE (W4.047):
                        //    `ScopeError<i64>.cancelled_count` resolves to
                        //    `Ty::Error` though `cancelled_count: i64` is declared.
                        //
                        //  - `UnmaterializedLiteral`: must NEVER survive —
                        //    `materialize_literal_defaults` ran at the boundary.
                        //    A leak here is a genuine literal-defaulting totality
                        //    hole, so it stays a hard (debug-only) assert.
                        debug_assert!(
                            !program_accepted
                                || !matches!(
                                    boundary_err,
                                    BoundaryError::UnmaterializedLiteral { .. }
                                ),
                            "W4.047 totality gap: surviving expr_types entry at \
                             {key:?} has type {ty:?} that fails ResolvedTy::from_ty \
                             ({boundary_err}) — an unmaterialized numeric literal \
                             crossed the checker->HIR handoff (literal-defaulting \
                             totality hole)"
                        );
                    }
                }
            }
            typed
        };

        // #1929 Stage 1: classify every concrete generic type-argument's
        // `Vec<T>` element ABI now, while `self.registry` (the `Copy` marker
        // authority) and the resolved `type_defs` (the `is_indirect` authority)
        // are both still in scope. MIR re-resolution of an element-typed Vec
        // method under a type parameter consults this verdict per
        // monomorphisation; computing it here keeps the single element→ABI
        // authority on the checker side rather than re-deriving it downstream.
        let vec_generic_element_abi = self.build_vec_generic_element_abi(
            &resolved_call_type_args,
            &resolved_record_init_type_args,
            &resolved_type_defs,
        );

        let mut output = TypeCheckOutput {
            expr_types: resolved_expr_types,
            resolved_expr_types: resolved_expr_types_typed,
            is_type_patterns: std::mem::take(&mut self.is_type_patterns),
            method_call_receiver_kinds: std::mem::take(&mut self.method_call_receiver_kinds),
            method_call_consumes_receiver: std::mem::take(&mut self.method_call_consumes_receiver),
            actor_send_aliasing: std::mem::take(&mut self.actor_send_aliasing),
            actor_handler_state_guards: std::mem::take(&mut self.actor_handler_state_guards),
            actor_max_heap: std::mem::take(&mut self.actor_max_heap),
            supervisor_child_slots: std::mem::take(&mut self.supervisor_child_slots),
            lowering_facts: resolved_lowering_facts,
            method_call_rewrites: std::mem::take(&mut self.method_call_rewrites),
            wire_layouts: std::mem::take(&mut self.wire_layouts),
            // W4.001 Stage A: substrate-only. Field is empty in Stage A
            // (no production populator); Stage B's resolver fills it.
            // See `check::dispatch` module docs and
            // `TypeCheckOutput::resolved_calls`.
            resolved_calls: std::mem::take(&mut self.resolved_calls),
            numeric_method_lowerings: std::mem::take(&mut self.numeric_method_lowerings),
            width_cast_lowerings: std::mem::take(&mut self.width_cast_lowerings),
            actor_method_dispatch: std::mem::take(&mut self.actor_method_dispatch),
            machine_method_dispatch: std::mem::take(&mut self.machine_method_dispatch),
            conn_await_reads: std::mem::take(&mut self.conn_await_reads),
            listener_await_accepts: std::mem::take(&mut self.listener_await_accepts),
            tail_ok_coercions: std::mem::take(&mut self.tail_ok_coercions),
            assign_target_kinds: std::mem::take(&mut self.assign_target_kinds),
            assign_target_shapes: std::mem::take(&mut self.assign_target_shapes),
            errors: std::mem::take(&mut self.errors),
            warnings: std::mem::take(&mut self.warnings),
            user_clone_record_seeds: std::mem::take(&mut self.user_clone_record_seeds),
            type_defs: resolved_type_defs,
            internal_builtin_enum_names,
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
            vec_generic_element_abi,
            record_init_type_args: resolved_record_init_type_args,
            stack_hints: std::mem::take(&mut self.stack_hints),
            dyn_trait_coercions: std::mem::take(&mut self.dyn_trait_coercions),
            dyn_trait_method_calls: std::mem::take(&mut self.dyn_trait_method_calls),
            closure_capture_facts: resolved_closure_capture_facts,
            closure_escape_facts: std::mem::take(&mut self.closure_escape_facts),
            actor_protocol_descriptors,
            intrinsic_declarations: std::mem::take(&mut self.intrinsic_declarations),
            pattern_resolutions: std::mem::take(&mut self.pending_pattern_resolutions)
                .into_iter()
                .map(|(k, mut arm)| {
                    // Resolve inference variables in every payload binding type.
                    for pb in &mut arm.payload_bindings {
                        pb.ty = self.subst.resolve(&pb.ty).materialize_literal_defaults();
                    }
                    (k, arm)
                })
                .collect(),
            lang_items: std::mem::take(&mut self.lang_items),
            hashmap_layout_facts: std::mem::take(&mut self.hashmap_layout_facts),
            hashset_layout_facts: std::mem::take(&mut self.hashset_layout_facts),
            actor_spawn_type_args: {
                // Resolve any lingering inference variables in the type args
                // before publishing to the output table.
                std::mem::take(&mut self.actor_spawn_type_args)
                    .into_iter()
                    .map(|(k, (name, args))| {
                        let resolved_args = args
                            .into_iter()
                            .map(|ty| self.subst.resolve(&ty).materialize_literal_defaults())
                            .collect();
                        (k, (name, resolved_args))
                    })
                    .collect()
            },
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

    /// Escape classifier. Walks the program AST after type-checking,
    /// identifies every closure literal, and records a
    /// `ClosureEscapeFact` keyed by the literal's span in
    /// `self.closure_escape_facts`. Conservative default: closures
    /// default to `Escapes` unless positively proven `Local` or
    /// `Forked`.
    fn classify_closure_escapes(&mut self, program: &Program) {
        for (item, _) in &program.items {
            self.classify_escapes_in_item(item);
        }
        // Mirror the per-module index assignment used during body checking
        // (topo order, skip root, 1-based index bumped only when the module
        // is present) so each `closure_escape_facts` entry is stamped with
        // the same `module_idx` the HIR consumer reads back via `mk_key` and
        // the fail-closed validator keys on. `current_module_idx` was reset
        // to 0 before this pass, so the root items above used idx 0.
        let module_order: Vec<_> = match &program.module_graph {
            Some(mg) => mg
                .topo_order
                .iter()
                .filter(|mod_id| **mod_id != mg.root)
                .cloned()
                .collect(),
            None => Vec::new(),
        };
        for mod_id in &module_order {
            if let Some(module) = program
                .module_graph
                .as_ref()
                .and_then(|mg| mg.modules.get(mod_id))
            {
                self.current_module_idx += 1;
                for (item, _) in &module.items {
                    self.classify_escapes_in_item(item);
                }
            }
        }
        if program.module_graph.is_some() {
            self.current_module_idx = 0;
        }
    }

    fn classify_escapes_in_item(&mut self, item: &Item) {
        match item {
            Item::Function(fn_decl) => {
                self.classify_escapes_in_block(&fn_decl.body, false);
            }
            Item::Impl(impl_decl) => {
                for method in &impl_decl.methods {
                    self.classify_escapes_in_block(&method.body, false);
                }
            }
            Item::Actor(actor) => {
                for method in &actor.methods {
                    self.classify_escapes_in_block(&method.body, false);
                }
                for rec in &actor.receive_fns {
                    self.classify_escapes_in_block(&rec.body, false);
                }
            }
            Item::Machine(machine) => {
                let _ = machine; // machine bodies traversed via transitions/entry/exit elsewhere
            }
            Item::Trait(trait_decl) => {
                for trait_item in &trait_decl.items {
                    if let TraitItem::Method(trait_method) = trait_item {
                        if let Some(body) = &trait_method.body {
                            self.classify_escapes_in_block(body, false);
                        }
                    }
                }
            }
            _ => {}
        }
    }

    fn classify_escapes_in_block(&mut self, block: &Block, in_fork: bool) {
        // For every let-statement whose value is a closure literal
        // (lambda or lambda-actor), classify using the rest of the
        // block as the context.
        for (i, (stmt, _)) in block.stmts.iter().enumerate() {
            if let Stmt::Let {
                pattern,
                value: Some((value_expr, lambda_span)),
                ..
            } = stmt
            {
                if let Pattern::Identifier(binding_name) = &pattern.0 {
                    if matches!(
                        value_expr,
                        Expr::Lambda { .. } | Expr::SpawnLambdaActor { .. }
                    ) {
                        let fact = closure_inference::classify_closure_escape_in_block(
                            &block.stmts,
                            block.trailing_expr.as_deref(),
                            i,
                            binding_name,
                            in_fork,
                        );
                        self.closure_escape_facts.insert(
                            SpanKey::in_module(lambda_span, self.current_module_idx),
                            fact,
                        );
                        self.maybe_emit_escape_advisory(lambda_span, fact);
                    }
                }
            }
        }
        // Recurse into every statement for nested blocks / nested
        // closures (anonymous lambdas, lambdas inside expressions).
        for (stmt, _) in &block.stmts {
            self.classify_escapes_in_stmt(stmt, in_fork);
        }
        if let Some(tail) = &block.trailing_expr {
            self.classify_escapes_in_expr(&tail.0, &tail.1, in_fork, AnonContext::Tail);
        }
    }

    #[allow(
        clippy::too_many_lines,
        reason = "Statement visitor traverses all stmt shapes; splitting \
                  by category would scatter related logic"
    )]
    fn classify_escapes_in_stmt(&mut self, stmt: &Stmt, in_fork: bool) {
        match stmt {
            Stmt::Let { value, .. } | Stmt::Var { value, .. } => {
                if let Some((e, span)) = value {
                    // `let f = || ...` was handled at the block level.
                    // Other shapes (block expr, struct init, …) descend
                    // here. The expression-position context says "value
                    // flows into a binding" — anonymous lambdas bound
                    // to a name get `Escapes` since the binding stores
                    // the closure for later use.
                    self.classify_escapes_in_expr(e, span, in_fork, AnonContext::StoredInBinding);
                }
            }
            Stmt::Assign { target, value, .. } => {
                self.classify_escapes_in_expr(&target.0, &target.1, in_fork, AnonContext::Other);
                self.classify_escapes_in_expr(
                    &value.0,
                    &value.1,
                    in_fork,
                    AnonContext::StoredInBinding,
                );
            }
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                self.classify_escapes_in_expr(
                    &condition.0,
                    &condition.1,
                    in_fork,
                    AnonContext::Other,
                );
                self.classify_escapes_in_block(then_block, in_fork);
                if let Some(eb) = else_block {
                    if let Some(b) = &eb.block {
                        self.classify_escapes_in_block(b, in_fork);
                    }
                    if let Some(if_stmt) = &eb.if_stmt {
                        self.classify_escapes_in_stmt(&if_stmt.0, in_fork);
                    }
                }
            }
            Stmt::IfLet {
                expr,
                body,
                else_body,
                ..
            } => {
                self.classify_escapes_in_expr(&expr.0, &expr.1, in_fork, AnonContext::Other);
                self.classify_escapes_in_block(body, in_fork);
                if let Some(b) = else_body {
                    self.classify_escapes_in_block(b, in_fork);
                }
            }
            Stmt::Match { scrutinee, arms } => {
                self.classify_escapes_in_expr(
                    &scrutinee.0,
                    &scrutinee.1,
                    in_fork,
                    AnonContext::Other,
                );
                for arm in arms {
                    if let Some((g, gs)) = &arm.guard {
                        self.classify_escapes_in_expr(g, gs, in_fork, AnonContext::Other);
                    }
                    self.classify_escapes_in_expr(
                        &arm.body.0,
                        &arm.body.1,
                        in_fork,
                        AnonContext::Other,
                    );
                }
            }
            Stmt::Loop { body, .. } => self.classify_escapes_in_block(body, in_fork),
            Stmt::For { iterable, body, .. } => {
                self.classify_escapes_in_expr(
                    &iterable.0,
                    &iterable.1,
                    in_fork,
                    AnonContext::Other,
                );
                self.classify_escapes_in_block(body, in_fork);
            }
            Stmt::While {
                condition, body, ..
            } => {
                self.classify_escapes_in_expr(
                    &condition.0,
                    &condition.1,
                    in_fork,
                    AnonContext::Other,
                );
                self.classify_escapes_in_block(body, in_fork);
            }
            Stmt::WhileLet { expr, body, .. } => {
                self.classify_escapes_in_expr(&expr.0, &expr.1, in_fork, AnonContext::Other);
                self.classify_escapes_in_block(body, in_fork);
            }
            Stmt::Break { value, .. } => {
                if let Some((e, s)) = value {
                    self.classify_escapes_in_expr(e, s, in_fork, AnonContext::Tail);
                }
            }
            Stmt::Continue { .. } => {}
            Stmt::Return(opt) => {
                if let Some((e, s)) = opt {
                    self.classify_escapes_in_expr(e, s, in_fork, AnonContext::Returned);
                }
            }
            Stmt::Defer(boxed) => {
                self.classify_escapes_in_expr(&boxed.0, &boxed.1, in_fork, AnonContext::Other);
            }
            Stmt::Expression((e, s)) => {
                self.classify_escapes_in_expr(e, s, in_fork, AnonContext::Other);
            }
        }
    }

    #[allow(
        clippy::too_many_lines,
        reason = "Expression visitor traverses the full Expr surface; \
                  arm-per-variant is the clearest form"
    )]
    fn classify_escapes_in_expr(
        &mut self,
        expr: &Expr,
        expr_span: &hew_parser::ast::Span,
        in_fork: bool,
        ctx: AnonContext,
    ) {
        match expr {
            Expr::Lambda { body, .. } | Expr::SpawnLambdaActor { body, .. } => {
                // Anonymous closure literal — classify by parent ctx.
                // When the literal is constructed *inside* a `fork { ... }`
                // body the closure's identity is tied to the forked task's
                // lifetime; this dominates the tail/stored/other contexts
                // the block walker would otherwise apply.
                let fact = if in_fork {
                    ClosureEscapeFact {
                        kind: ClosureEscapeKind::Forked,
                        rule: ClosureEscapeRule::InsideForkBlock,
                    }
                } else {
                    match ctx {
                        AnonContext::InForkBody => ClosureEscapeFact {
                            kind: ClosureEscapeKind::Forked,
                            rule: ClosureEscapeRule::InsideForkBlock,
                        },
                        AnonContext::Returned => ClosureEscapeFact {
                            kind: ClosureEscapeKind::Escapes,
                            rule: ClosureEscapeRule::Returned,
                        },
                        AnonContext::Tail => ClosureEscapeFact {
                            kind: ClosureEscapeKind::Escapes,
                            rule: ClosureEscapeRule::EscapesViaBlockValue,
                        },
                        AnonContext::PassedToHigherOrder => ClosureEscapeFact {
                            kind: ClosureEscapeKind::Escapes,
                            rule: ClosureEscapeRule::PassedToHigherOrder,
                        },
                        AnonContext::StoredInBinding => ClosureEscapeFact {
                            kind: ClosureEscapeKind::Escapes,
                            rule: ClosureEscapeRule::StoredOrSent,
                        },
                        AnonContext::Other => ClosureEscapeFact {
                            kind: ClosureEscapeKind::Escapes,
                            rule: ClosureEscapeRule::NoStaticBinding,
                        },
                    }
                };
                // Only insert if not already inserted by the let-bound
                // path (in which case the let-bound fact wins).
                self.closure_escape_facts
                    .entry(SpanKey::in_module(expr_span, self.current_module_idx))
                    .or_insert(fact);
                self.maybe_emit_escape_advisory(expr_span, fact);
                // Recurse into the body so nested closures inside this
                // lambda get classified too.
                self.classify_escapes_in_expr(
                    &body.0,
                    &body.1,
                    /* in_fork = */ false,
                    AnonContext::Other,
                );
            }
            Expr::ForkBlock { body } => {
                self.classify_escapes_in_block(body, /* in_fork = */ true);
            }
            Expr::ForkChild { expr, .. } => {
                self.classify_escapes_in_expr(
                    &expr.0,
                    &expr.1,
                    /* in_fork = */ true,
                    AnonContext::InForkBody,
                );
            }
            Expr::Scope { body } => {
                self.classify_escapes_in_block(body, /* in_fork = */ false);
            }
            Expr::ScopeDeadline { duration, body } => {
                self.classify_escapes_in_expr(
                    &duration.0,
                    &duration.1,
                    in_fork,
                    AnonContext::Other,
                );
                self.classify_escapes_in_block(body, in_fork);
            }
            Expr::Block(block) => self.classify_escapes_in_block(block, in_fork),
            Expr::If {
                condition,
                then_block,
                else_block,
            } => {
                self.classify_escapes_in_expr(
                    &condition.0,
                    &condition.1,
                    in_fork,
                    AnonContext::Other,
                );
                self.classify_escapes_in_expr(&then_block.0, &then_block.1, in_fork, ctx);
                if let Some(eb) = else_block {
                    self.classify_escapes_in_expr(&eb.0, &eb.1, in_fork, ctx);
                }
            }
            Expr::IfLet {
                expr,
                body,
                else_body,
                ..
            } => {
                self.classify_escapes_in_expr(&expr.0, &expr.1, in_fork, AnonContext::Other);
                self.classify_escapes_in_block(body, in_fork);
                if let Some(b) = else_body {
                    self.classify_escapes_in_block(b, in_fork);
                }
            }
            Expr::Match { scrutinee, arms } => {
                self.classify_escapes_in_expr(
                    &scrutinee.0,
                    &scrutinee.1,
                    in_fork,
                    AnonContext::Other,
                );
                for arm in arms {
                    if let Some((g, gs)) = &arm.guard {
                        self.classify_escapes_in_expr(g, gs, in_fork, AnonContext::Other);
                    }
                    self.classify_escapes_in_expr(&arm.body.0, &arm.body.1, in_fork, ctx);
                }
            }
            Expr::Call { function, args, .. } => {
                self.classify_escapes_in_expr(
                    &function.0,
                    &function.1,
                    in_fork,
                    AnonContext::Other,
                );
                for arg in args {
                    let (e, s) = arg.expr();
                    self.classify_escapes_in_expr(e, s, in_fork, AnonContext::PassedToHigherOrder);
                }
            }
            Expr::MethodCall { receiver, args, .. } => {
                self.classify_escapes_in_expr(
                    &receiver.0,
                    &receiver.1,
                    in_fork,
                    AnonContext::Other,
                );
                for arg in args {
                    let (e, s) = arg.expr();
                    self.classify_escapes_in_expr(e, s, in_fork, AnonContext::PassedToHigherOrder);
                }
            }
            Expr::Spawn { target, args, .. } => {
                self.classify_escapes_in_expr(&target.0, &target.1, in_fork, AnonContext::Other);
                for (_, (e, s)) in args {
                    self.classify_escapes_in_expr(e, s, in_fork, AnonContext::PassedToHigherOrder);
                }
            }
            Expr::StructInit { fields, base, .. } => {
                for (_, (e, s)) in fields {
                    self.classify_escapes_in_expr(e, s, in_fork, AnonContext::StoredInBinding);
                }
                if let Some(b) = base {
                    self.classify_escapes_in_expr(&b.0, &b.1, in_fork, AnonContext::Other);
                }
            }
            Expr::Tuple(items) | Expr::Array(items) => {
                for (e, s) in items {
                    self.classify_escapes_in_expr(e, s, in_fork, AnonContext::StoredInBinding);
                }
            }
            Expr::ArrayRepeat { value, count } => {
                self.classify_escapes_in_expr(
                    &value.0,
                    &value.1,
                    in_fork,
                    AnonContext::StoredInBinding,
                );
                self.classify_escapes_in_expr(&count.0, &count.1, in_fork, AnonContext::Other);
            }
            Expr::MapLiteral { entries } => {
                for ((k, ks), (v, vs)) in entries {
                    self.classify_escapes_in_expr(k, ks, in_fork, AnonContext::StoredInBinding);
                    self.classify_escapes_in_expr(v, vs, in_fork, AnonContext::StoredInBinding);
                }
            }
            Expr::Binary { left, right, .. } => {
                self.classify_escapes_in_expr(&left.0, &left.1, in_fork, AnonContext::Other);
                self.classify_escapes_in_expr(&right.0, &right.1, in_fork, AnonContext::Other);
            }
            Expr::Unary { operand, .. } | Expr::Clone(operand) => {
                self.classify_escapes_in_expr(&operand.0, &operand.1, in_fork, AnonContext::Other);
            }
            Expr::FieldAccess { object, .. } => {
                self.classify_escapes_in_expr(&object.0, &object.1, in_fork, AnonContext::Other);
            }
            Expr::Index { object, index } => {
                self.classify_escapes_in_expr(&object.0, &object.1, in_fork, AnonContext::Other);
                self.classify_escapes_in_expr(&index.0, &index.1, in_fork, AnonContext::Other);
            }
            Expr::Cast { expr, .. } => {
                self.classify_escapes_in_expr(&expr.0, &expr.1, in_fork, AnonContext::Other);
            }
            Expr::PostfixTry(inner) => {
                self.classify_escapes_in_expr(&inner.0, &inner.1, in_fork, ctx);
            }
            Expr::Range { start, end, .. } => {
                if let Some(s) = start {
                    self.classify_escapes_in_expr(&s.0, &s.1, in_fork, AnonContext::Other);
                }
                if let Some(e) = end {
                    self.classify_escapes_in_expr(&e.0, &e.1, in_fork, AnonContext::Other);
                }
            }
            Expr::Is { lhs, rhs } => {
                self.classify_escapes_in_expr(&lhs.0, &lhs.1, in_fork, AnonContext::Other);
                self.classify_escapes_in_expr(&rhs.0, &rhs.1, in_fork, AnonContext::Other);
            }
            Expr::Select { arms, timeout } => {
                for arm in arms {
                    self.classify_escapes_in_expr(
                        &arm.source.0,
                        &arm.source.1,
                        in_fork,
                        AnonContext::Other,
                    );
                    self.classify_escapes_in_expr(
                        &arm.body.0,
                        &arm.body.1,
                        in_fork,
                        AnonContext::Other,
                    );
                }
                if let Some(t) = timeout {
                    self.classify_escapes_in_expr(
                        &t.duration.0,
                        &t.duration.1,
                        in_fork,
                        AnonContext::Other,
                    );
                    self.classify_escapes_in_expr(
                        &t.body.0,
                        &t.body.1,
                        in_fork,
                        AnonContext::Other,
                    );
                }
            }
            Expr::Join(items) => {
                for (e, s) in items {
                    self.classify_escapes_in_expr(e, s, in_fork, AnonContext::PassedToHigherOrder);
                }
            }
            Expr::Timeout { expr, duration } => {
                self.classify_escapes_in_expr(&expr.0, &expr.1, in_fork, ctx);
                self.classify_escapes_in_expr(
                    &duration.0,
                    &duration.1,
                    in_fork,
                    AnonContext::Other,
                );
            }
            Expr::UnsafeBlock(block) => self.classify_escapes_in_block(block, in_fork),
            Expr::Yield(opt) => {
                if let Some(boxed) = opt {
                    self.classify_escapes_in_expr(
                        &boxed.0,
                        &boxed.1,
                        in_fork,
                        AnonContext::PassedToHigherOrder,
                    );
                }
            }
            Expr::Await(inner) => {
                self.classify_escapes_in_expr(&inner.0, &inner.1, in_fork, AnonContext::Other);
            }
            Expr::InterpolatedString(parts) => {
                for part in parts {
                    if let StringPart::Expr((e, s)) = part {
                        self.classify_escapes_in_expr(e, s, in_fork, AnonContext::Other);
                    }
                }
            }
            Expr::MachineEmit { fields, .. } => {
                for (_, (e, s)) in fields {
                    self.classify_escapes_in_expr(e, s, in_fork, AnonContext::PassedToHigherOrder);
                }
            }
            Expr::GenBlock { body } => self.classify_escapes_in_block(body, in_fork),
            Expr::Literal(_)
            | Expr::Identifier(_)
            | Expr::This
            | Expr::RegexLiteral(_)
            | Expr::ByteStringLiteral(_)
            | Expr::ByteArrayLiteral(_) => {}
        }
    }

    fn maybe_emit_escape_advisory(
        &mut self,
        lambda_span: &hew_parser::ast::Span,
        fact: ClosureEscapeFact,
    ) {
        // Advisory diagnostic when conservatively classified `Escapes`
        // AND the rule indicates restructuring could admit `Local`.
        // Emitted at warning severity (the diagnostic surface has no
        // Info level).
        if !matches!(fact.kind, ClosureEscapeKind::Escapes) {
            return;
        }
        // PassedToHigherOrder is intentionally excluded: inlining a let-bound
        // closure at its call site does not relieve the escape — an anonymous
        // closure in argument position is still classified PassedToHigherOrder
        // (via AnonContext::PassedToHigherOrder), so the advisory would fire
        // again.  Only rules where inlining genuinely admits Local are kept.
        let admit_local = matches!(
            fact.rule,
            ClosureEscapeRule::EscapesViaBlockValue | ClosureEscapeRule::NoStaticBinding
        );
        if !admit_local {
            return;
        }
        // One advisory per closure literal: the classifier visits the same
        // span more than once (let-bound block walk + anonymous-expression
        // walk; top-level item list + module graph for the entry module).
        // Gate on first-insert; distinct spans still warn independently.
        if !self
            .closure_escape_advisory_spans
            .insert(SpanKey::from(lambda_span))
        {
            return;
        }
        self.warnings.push(crate::error::TypeError {
            severity: crate::error::Severity::Warning,
            kind: TypeErrorKind::ClosureEscapeAdvisory {
                rule: format!("{:?}", fact.rule),
            },
            span: lambda_span.clone(),
            message: format!(
                "closure conservatively classified as escaping ({:?}); \
                 inlining the closure at its call site would admit `Local`",
                fact.rule
            ),
            notes: vec![],
            suggestions: vec![],
            source_module: None,
        });
    }

    /// Post-pass: walk the program once collecting every closure
    /// literal span, then verify `closure_capture_facts` and
    /// `closure_escape_facts` each carry an entry for that span. A
    /// missing entry trips the corresponding fail-closed diagnostic
    /// (`ClosureCaptureModeUnresolved` / `ClosureEscapeKindUnresolved`).
    ///
    /// The pass is a defensive contract enforcer for the
    /// checker→MIR-lowering boundary: it is intentionally noisy when
    /// the checker has a structural gap and silent when the contract
    /// holds. It does NOT classify or default; it only reports gaps.
    fn validate_closure_facts_complete(&mut self, program: &Program) {
        let mut sites: Vec<(Span, Option<String>, u32)> = Vec::new();
        collect_closure_literal_spans(program, &mut sites);
        let mut diagnostics = Vec::new();
        emit_unresolved_closure_diagnostics(
            &sites,
            &self.closure_capture_facts,
            &self.closure_escape_facts,
            &mut diagnostics,
        );
        for err in diagnostics {
            self.errors.push(err);
        }
    }
}

/// Walk a program's AST and append every closure literal span
/// (`Expr::Lambda` / `Expr::SpawnLambdaActor`) it finds, paired with
/// the capture-name when the literal is the value of a top-level
/// `let <name> = |...| ...` binding (None otherwise — for diagnostic
/// hint only) and the owning `module_idx` (0 for the root unit, N for
/// the N-th non-root module in topo order).
///
/// The `module_idx` MUST be assigned exactly as the checker assigns
/// `current_module_idx` while body-checking (`check_program`: iterate
/// `module_graph.topo_order`, skip the root, and bump a 1-based index
/// only when the module is actually present in `modules`). The
/// capture/escape facts for each closure are stamped with that same
/// index, so the fail-closed lookup below can only line up if this walk
/// reproduces the assignment site-for-site.
fn collect_closure_literal_spans(program: &Program, out: &mut Vec<(Span, Option<String>, u32)>) {
    let mut root_sites: Vec<(Span, Option<String>)> = Vec::new();
    for (item, _) in &program.items {
        collect_lambda_spans_in_item(item, &mut root_sites);
    }
    for (span, name) in root_sites {
        out.push((span, name, 0));
    }
    if let Some(mg) = &program.module_graph {
        let mut module_idx: u32 = 0;
        for mod_id in &mg.topo_order {
            if *mod_id == mg.root {
                continue;
            }
            if let Some(module) = mg.modules.get(mod_id) {
                module_idx += 1;
                let mut module_sites: Vec<(Span, Option<String>)> = Vec::new();
                for (item, _) in &module.items {
                    collect_lambda_spans_in_item(item, &mut module_sites);
                }
                for (span, name) in module_sites {
                    out.push((span, name, module_idx));
                }
            }
        }
    }
}

fn collect_lambda_spans_in_item(item: &Item, out: &mut Vec<(Span, Option<String>)>) {
    match item {
        Item::Function(fn_decl) => collect_lambda_spans_in_block(&fn_decl.body, out),
        Item::Impl(impl_decl) => {
            for method in &impl_decl.methods {
                collect_lambda_spans_in_block(&method.body, out);
            }
        }
        Item::Actor(actor) => {
            for method in &actor.methods {
                collect_lambda_spans_in_block(&method.body, out);
            }
            for rec in &actor.receive_fns {
                collect_lambda_spans_in_block(&rec.body, out);
            }
        }
        Item::Trait(trait_decl) => {
            for trait_item in &trait_decl.items {
                if let TraitItem::Method(trait_method) = trait_item {
                    if let Some(body) = &trait_method.body {
                        collect_lambda_spans_in_block(body, out);
                    }
                }
            }
        }
        _ => {}
    }
}

fn collect_lambda_spans_in_block(block: &Block, out: &mut Vec<(Span, Option<String>)>) {
    for (stmt, _) in &block.stmts {
        collect_lambda_spans_in_stmt(stmt, out);
    }
    if let Some(tail) = &block.trailing_expr {
        collect_lambda_spans_in_expr(&tail.0, &tail.1, out);
    }
}

#[allow(
    clippy::too_many_lines,
    clippy::match_same_arms,
    reason = "Statement visitor over the full Stmt surface; arm-per-variant \
              is the clearest form"
)]
fn collect_lambda_spans_in_stmt(stmt: &Stmt, out: &mut Vec<(Span, Option<String>)>) {
    match stmt {
        Stmt::Let { value, .. } | Stmt::Var { value, .. } => {
            if let Some((e, s)) = value {
                collect_lambda_spans_in_expr(e, s, out);
            }
        }
        Stmt::Assign { target, value, .. } => {
            collect_lambda_spans_in_expr(&target.0, &target.1, out);
            collect_lambda_spans_in_expr(&value.0, &value.1, out);
        }
        Stmt::If {
            condition,
            then_block,
            else_block,
        } => {
            collect_lambda_spans_in_expr(&condition.0, &condition.1, out);
            collect_lambda_spans_in_block(then_block, out);
            if let Some(eb) = else_block {
                if let Some(b) = &eb.block {
                    collect_lambda_spans_in_block(b, out);
                }
                if let Some(if_stmt) = &eb.if_stmt {
                    collect_lambda_spans_in_stmt(&if_stmt.0, out);
                }
            }
        }
        Stmt::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            collect_lambda_spans_in_expr(&expr.0, &expr.1, out);
            collect_lambda_spans_in_block(body, out);
            if let Some(b) = else_body {
                collect_lambda_spans_in_block(b, out);
            }
        }
        Stmt::Match { scrutinee, arms } => {
            collect_lambda_spans_in_expr(&scrutinee.0, &scrutinee.1, out);
            for arm in arms {
                if let Some((g, gs)) = &arm.guard {
                    collect_lambda_spans_in_expr(g, gs, out);
                }
                collect_lambda_spans_in_expr(&arm.body.0, &arm.body.1, out);
            }
        }
        Stmt::Loop { body, .. } => collect_lambda_spans_in_block(body, out),
        Stmt::For { iterable, body, .. } => {
            collect_lambda_spans_in_expr(&iterable.0, &iterable.1, out);
            collect_lambda_spans_in_block(body, out);
        }
        Stmt::While {
            condition, body, ..
        } => {
            collect_lambda_spans_in_expr(&condition.0, &condition.1, out);
            collect_lambda_spans_in_block(body, out);
        }
        Stmt::WhileLet { expr, body, .. } => {
            collect_lambda_spans_in_expr(&expr.0, &expr.1, out);
            collect_lambda_spans_in_block(body, out);
        }
        Stmt::Break { value, .. } => {
            if let Some((e, s)) = value {
                collect_lambda_spans_in_expr(e, s, out);
            }
        }
        Stmt::Continue { .. } => {}
        Stmt::Return(opt) => {
            if let Some((e, s)) = opt {
                collect_lambda_spans_in_expr(e, s, out);
            }
        }
        Stmt::Defer(boxed) => {
            collect_lambda_spans_in_expr(&boxed.0, &boxed.1, out);
        }
        Stmt::Expression((e, s)) => collect_lambda_spans_in_expr(e, s, out),
    }
}

#[allow(
    clippy::too_many_lines,
    clippy::match_same_arms,
    reason = "Expression visitor over the full Expr surface; arm-per-variant \
              is the clearest form"
)]
fn collect_lambda_spans_in_expr(
    expr: &Expr,
    expr_span: &Span,
    out: &mut Vec<(Span, Option<String>)>,
) {
    match expr {
        Expr::Lambda { body, .. } | Expr::SpawnLambdaActor { body, .. } => {
            out.push((expr_span.clone(), None));
            collect_lambda_spans_in_expr(&body.0, &body.1, out);
        }
        Expr::Block(block) => collect_lambda_spans_in_block(block, out),
        Expr::If {
            condition,
            then_block,
            else_block,
        } => {
            collect_lambda_spans_in_expr(&condition.0, &condition.1, out);
            collect_lambda_spans_in_expr(&then_block.0, &then_block.1, out);
            if let Some(eb) = else_block {
                collect_lambda_spans_in_expr(&eb.0, &eb.1, out);
            }
        }
        Expr::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            collect_lambda_spans_in_expr(&expr.0, &expr.1, out);
            collect_lambda_spans_in_block(body, out);
            if let Some(b) = else_body {
                collect_lambda_spans_in_block(b, out);
            }
        }
        Expr::Match { scrutinee, arms } => {
            collect_lambda_spans_in_expr(&scrutinee.0, &scrutinee.1, out);
            for arm in arms {
                if let Some((g, gs)) = &arm.guard {
                    collect_lambda_spans_in_expr(g, gs, out);
                }
                collect_lambda_spans_in_expr(&arm.body.0, &arm.body.1, out);
            }
        }
        Expr::Call { function, args, .. } => {
            collect_lambda_spans_in_expr(&function.0, &function.1, out);
            for arg in args {
                let (e, s) = arg.expr();
                collect_lambda_spans_in_expr(e, s, out);
            }
        }
        Expr::MethodCall { receiver, args, .. } => {
            collect_lambda_spans_in_expr(&receiver.0, &receiver.1, out);
            for arg in args {
                let (e, s) = arg.expr();
                collect_lambda_spans_in_expr(e, s, out);
            }
        }
        Expr::Spawn { target, args, .. } => {
            collect_lambda_spans_in_expr(&target.0, &target.1, out);
            for (_, (e, s)) in args {
                collect_lambda_spans_in_expr(e, s, out);
            }
        }
        Expr::StructInit { fields, base, .. } => {
            for (_, (e, s)) in fields {
                collect_lambda_spans_in_expr(e, s, out);
            }
            if let Some(b) = base {
                collect_lambda_spans_in_expr(&b.0, &b.1, out);
            }
        }
        Expr::Tuple(items) | Expr::Array(items) => {
            for (e, s) in items {
                collect_lambda_spans_in_expr(e, s, out);
            }
        }
        Expr::ArrayRepeat { value, count } => {
            collect_lambda_spans_in_expr(&value.0, &value.1, out);
            collect_lambda_spans_in_expr(&count.0, &count.1, out);
        }
        Expr::MapLiteral { entries } => {
            for ((k, ks), (v, vs)) in entries {
                collect_lambda_spans_in_expr(k, ks, out);
                collect_lambda_spans_in_expr(v, vs, out);
            }
        }
        Expr::Binary { left, right, .. } => {
            collect_lambda_spans_in_expr(&left.0, &left.1, out);
            collect_lambda_spans_in_expr(&right.0, &right.1, out);
        }
        Expr::Unary { operand, .. } | Expr::Clone(operand) => {
            collect_lambda_spans_in_expr(&operand.0, &operand.1, out);
        }
        Expr::FieldAccess { object, .. } => {
            collect_lambda_spans_in_expr(&object.0, &object.1, out);
        }
        Expr::Index { object, index } => {
            collect_lambda_spans_in_expr(&object.0, &object.1, out);
            collect_lambda_spans_in_expr(&index.0, &index.1, out);
        }
        Expr::Cast { expr, .. } => collect_lambda_spans_in_expr(&expr.0, &expr.1, out),
        Expr::PostfixTry(inner) => collect_lambda_spans_in_expr(&inner.0, &inner.1, out),
        Expr::Range { start, end, .. } => {
            if let Some(s) = start {
                collect_lambda_spans_in_expr(&s.0, &s.1, out);
            }
            if let Some(e) = end {
                collect_lambda_spans_in_expr(&e.0, &e.1, out);
            }
        }
        Expr::Is { lhs, rhs } => {
            collect_lambda_spans_in_expr(&lhs.0, &lhs.1, out);
            collect_lambda_spans_in_expr(&rhs.0, &rhs.1, out);
        }
        Expr::Scope { body } => collect_lambda_spans_in_block(body, out),
        Expr::ForkChild { expr, .. } => {
            collect_lambda_spans_in_expr(&expr.0, &expr.1, out);
        }
        Expr::ForkBlock { body } => collect_lambda_spans_in_block(body, out),
        Expr::ScopeDeadline { duration, body } => {
            collect_lambda_spans_in_expr(&duration.0, &duration.1, out);
            collect_lambda_spans_in_block(body, out);
        }
        Expr::Select { arms, timeout } => {
            for arm in arms {
                collect_lambda_spans_in_expr(&arm.source.0, &arm.source.1, out);
                collect_lambda_spans_in_expr(&arm.body.0, &arm.body.1, out);
            }
            if let Some(t) = timeout {
                collect_lambda_spans_in_expr(&t.duration.0, &t.duration.1, out);
                collect_lambda_spans_in_expr(&t.body.0, &t.body.1, out);
            }
        }
        Expr::Join(items) => {
            for (e, s) in items {
                collect_lambda_spans_in_expr(e, s, out);
            }
        }
        Expr::Timeout { expr, duration } => {
            collect_lambda_spans_in_expr(&expr.0, &expr.1, out);
            collect_lambda_spans_in_expr(&duration.0, &duration.1, out);
        }
        Expr::UnsafeBlock(block) => collect_lambda_spans_in_block(block, out),
        Expr::Yield(opt) => {
            if let Some(boxed) = opt {
                collect_lambda_spans_in_expr(&boxed.0, &boxed.1, out);
            }
        }
        Expr::Await(inner) => collect_lambda_spans_in_expr(&inner.0, &inner.1, out),
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let StringPart::Expr((e, s)) = part {
                    collect_lambda_spans_in_expr(e, s, out);
                }
            }
        }
        Expr::MachineEmit { fields, .. } => {
            for (_, (e, s)) in fields {
                collect_lambda_spans_in_expr(e, s, out);
            }
        }
        Expr::GenBlock { body } => collect_lambda_spans_in_block(body, out),
        Expr::Literal(_)
        | Expr::Identifier(_)
        | Expr::This
        | Expr::RegexLiteral(_)
        | Expr::ByteStringLiteral(_)
        | Expr::ByteArrayLiteral(_) => {}
    }
}

/// Fail-closed defense: for every collected closure literal span,
/// emit the corresponding `Unresolved` diagnostic if a fact is missing.
/// Pure function so unit tests can drive it with synthetic maps.
pub(crate) fn emit_unresolved_closure_diagnostics(
    sites: &[(Span, Option<String>, u32)],
    capture_facts: &HashMap<SpanKey, Vec<ClosureCaptureFact>>,
    escape_facts: &HashMap<SpanKey, ClosureEscapeFact>,
    out: &mut Vec<TypeError>,
) {
    for (span, hint_name, module_idx) in sites {
        // Key each site with its owning module index (assigned by
        // `collect_closure_literal_spans` to mirror the checker), so the
        // lookup matches the `SpanKey::in_module(span, current_module_idx)`
        // the capture/escape facts were stamped with.
        let key = SpanKey::in_module(span, *module_idx);
        if !capture_facts.contains_key(&key) {
            let name = hint_name.clone().unwrap_or_else(|| "<closure>".to_string());
            out.push(TypeError {
                severity: crate::error::Severity::Error,
                kind: TypeErrorKind::ClosureCaptureModeUnresolved { name: name.clone() },
                span: span.clone(),
                message: format!(
                    "internal: closure literal `{name}` reached checker output \
                     without a resolved capture-mode ledger; the \
                     checker→MIR contract requires every closure to \
                     have a `ClosureCaptureFact` set"
                ),
                notes: vec![],
                suggestions: vec![],
                source_module: None,
            });
        }
        if !escape_facts.contains_key(&key) {
            out.push(TypeError {
                severity: crate::error::Severity::Error,
                kind: TypeErrorKind::ClosureEscapeKindUnresolved,
                span: span.clone(),
                message: "internal: closure literal reached checker output without \
                     a resolved `ClosureEscapeKind`; the checker→MIR contract \
                     requires every closure literal to be classified"
                    .to_string(),
                notes: vec![],
                suggestions: vec![],
                source_module: None,
            });
        }
    }
}

/// Context label propagated to anonymous closure literals so they pick
/// up the right `ClosureEscapeFact` when no `let f = ...` binding
/// gives them a name.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AnonContext {
    /// Inside a `fork { ... }` body or `fork name = ...` child binding.
    InForkBody,
    /// In a `return expr` position.
    Returned,
    /// Tail expression of a block — escapes via block value.
    Tail,
    /// Argument to a call / method-call / spawn / yield / join.
    PassedToHigherOrder,
    /// RHS of a let/var/assign — stored in another binding.
    StoredInBinding,
    /// Anything else.
    Other,
}

/// Collect every `actor` declaration in the program (root items + each
/// module-graph module).
///
/// Returns a triple `(collision_identity, sigs_key, actor_decl)` per actor:
///
/// - `collision_identity`: the full-path dotted form used for the descriptor
///   map key and the cross-actor collision check.  Module path `["a","b"]` →
///   `"a.b.Alpha"`.  Full path ensures actors in DISTINCT nested modules with
///   an identical leaf component produce different identities so the collision
///   check sees them as separate actors.
/// - `sigs_key`: the module-short dotted form `"{leaf}.{Actor}"` (or bare
///   name for root actors) that `fn_sigs` is keyed with during registration
///   (`collect_functions` uses `current_module_short()` = the leaf segment).
///   Used only for `fn_sigs.get("{sigs_key}::{handler}")` look-ups inside
///   `build_actor_protocol_descriptors`.
///
/// The two fields differ for deeply-nested modules: `["a","b"].Alpha` has
/// `collision_identity = "a.b.Alpha"` but `sigs_key = "b.Alpha"`.  Root and
/// single-segment modules are the same for both.
///
/// The walk is read-only so it can run after the checker has frozen its
/// mutable state.
fn collect_program_actors(program: &Program) -> Vec<(String, String, &ActorDecl)> {
    let mut actors: Vec<(String, String, &ActorDecl)> = Vec::new();
    for (item, _) in &program.items {
        if let Item::Actor(ad) = item {
            // Root actors: both keys are the bare name.
            actors.push((ad.name.clone(), ad.name.clone(), ad));
        }
    }
    if let Some(mg) = &program.module_graph {
        for (mod_id, module) in &mg.modules {
            if *mod_id == mg.root {
                continue;
            }
            let module_full = mod_id.path.join(".");
            // The leaf segment is what `current_module_short()` (rsplit('.'))
            // returns during `collect_functions`; that is the prefix used when
            // fn_sigs keys are registered.
            let module_leaf = mod_id.path.last().map_or("", String::as_str);
            for (item, _) in &module.items {
                if let Item::Actor(ad) = item {
                    let collision_identity = if module_full.is_empty() {
                        ad.name.clone()
                    } else {
                        format!("{module_full}.{}", ad.name)
                    };
                    let sigs_key = if module_leaf.is_empty() {
                        ad.name.clone()
                    } else {
                        format!("{module_leaf}.{}", ad.name)
                    };
                    actors.push((collision_identity, sigs_key, ad));
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
    // Cross-actor collision tracking (defense-in-depth): every published
    // handler's `(msg_id, actor, handler, span)`. `msg_id` is the 32-bit
    // transport discriminant on every cross-node frame; two DISTINCT actors
    // sharing it leave the wire protocol ambiguous. The runtime keys its codec
    // registry by `(actor-type, msg_id)` so routing is correct in-process, but
    // refusing the collision at the source of truth keeps the wire unambiguous
    // for relays / mixed-binary peers (the `boundary-fail-closed` invariant).
    let mut cross_actor_seen: Vec<(u32, String, String, std::ops::Range<usize>)> = Vec::new();
    for (collision_identity, sigs_key, ad) in collect_program_actors(program) {
        if ad.receive_fns.is_empty() {
            continue;
        }
        let mut specs: Vec<crate::actor_protocol::ActorHandlerSpec> =
            Vec::with_capacity(ad.receive_fns.len());
        let mut all_signatures_resolved = true;
        for rf in &ad.receive_fns {
            // fn_sigs are keyed by the module-short identity (leaf segment).
            let key = format!("{sigs_key}::{}", rf.name);
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
            let symbol = format!("{sigs_key}__{}", rf.name);
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

        // Build the descriptor under `sigs_key` (module-short form) so that
        // downstream consumers — HIR lowerer, coercion checker — can look it
        // up via the same identity they registered actors under.
        // `collision_identity` (full-path form) is used ONLY for the
        // cross-actor collision check below, where its uniqueness across all
        // nested-module actors matters.
        match crate::actor_protocol::ActorProtocolDescriptor::from_handlers(
            sigs_key.clone(),
            &specs,
        ) {
            Ok(descriptor) => {
                // Record each handler's msg_id for the cross-actor pass.
                // Use `collision_identity` as the actor name so that two actors
                // in different nested modules with the same leaf segment (and
                // thus the same `sigs_key`, e.g. `b.Alpha`) appear as DISTINCT
                // actors in the collision check.
                for h in &descriptor.handlers {
                    let span = ad
                        .receive_fns
                        .iter()
                        .find(|rf| rf.name == h.name)
                        .map_or(0..0, |rf| rf.span.clone());
                    cross_actor_seen.push((
                        h.msg_id,
                        collision_identity.clone(),
                        h.name.clone(),
                        span,
                    ));
                }
                descriptors.insert(sigs_key.clone(), descriptor);
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

    report_cross_actor_msg_id_collisions(&cross_actor_seen, errors);

    descriptors
}

/// Defense-in-depth cross-actor pass: a `msg_id` shared by two DISTINCT actors
/// is a 32-bit cross-node wire-discriminant collision. Emits one
/// `CrossActorProtocolCollision` per colliding `msg_id` — the first
/// distinct-actor pair found — pinned to the later actor's handler span. `seen`
/// is every published handler's `(msg_id, actor, handler, span)` over the WHOLE
/// program's actor set (so cross-module collisions are caught); intra-actor dups
/// were already refused before this runs, so each entry is from a collision-free
/// (within-its-actor) descriptor.
fn report_cross_actor_msg_id_collisions(
    seen: &[(u32, String, String, std::ops::Range<usize>)],
    errors: &mut Vec<TypeError>,
) {
    let mut reported_msg_ids: Vec<u32> = Vec::new();
    for i in 0..seen.len() {
        let (msg_id_i, actor_i, handler_i, span_i) = &seen[i];
        if reported_msg_ids.contains(msg_id_i) {
            continue;
        }
        let Some((_, actor_j, handler_j, _)) = seen[..i]
            .iter()
            .find(|(mid, actor_j, _, _)| mid == msg_id_i && actor_j != actor_i)
        else {
            continue;
        };
        reported_msg_ids.push(*msg_id_i);
        let message = format!(
            "actors `{actor_j}` and `{actor_i}` have `receive fn`s with the same \
             cross-node msg_id 0x{msg_id_i:08x}: `{actor_j}::{handler_j}` and \
             `{actor_i}::{handler_i}` — the 32-bit wire discriminant is ambiguous"
        );
        let mut err = TypeError::new(
            TypeErrorKind::CrossActorProtocolCollision {
                actor_a: actor_j.clone(),
                handler_a: handler_j.clone(),
                actor_b: actor_i.clone(),
                handler_b: handler_i.clone(),
                msg_id: *msg_id_i,
            },
            span_i.clone(),
            message,
        );
        err = err.with_suggestion(format!(
            "rename `{actor_j}::{handler_j}` or `{actor_i}::{handler_i}` so their \
             fully-qualified names hash to distinct msg_ids; explicit `#[msg_id(N)]` \
             pinning is reserved for a future release (not yet supported)"
        ));
        errors.push(err);
    }
}
