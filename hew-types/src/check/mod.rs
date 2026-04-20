//! Bidirectional type checker for Hew programs.

use crate::builtin_names::builtin_named_type;
use crate::error::{TypeError, TypeErrorKind};
use crate::module_registry::ModuleError;
use crate::traits::MarkerTrait;
use crate::ty::{Ty, TypeVar};
use crate::unify::unify;
use hew_parser::ast::{
    ActorDecl, ActorInit, ActorTerminate, AttributeArg, BinaryOp, Block, CallArg, ConstDecl, Expr,
    ExternBlock, ExternFnDecl, FieldDecl, FnDecl, ImplDecl, ImportDecl, ImportSpec, Item,
    LambdaParam, Literal, MachineDecl, MatchArm, Param, Pattern, Program, ReceiveFnDecl, Span,
    Spanned, Stmt, StringPart, TraitDecl, TraitItem, TypeBodyItem, TypeDecl, TypeDeclKind,
    TypeExpr, TypeParam, UnaryOp, VariantKind, WhereClause, WireDecl, WireDeclKind,
};
use std::collections::{hash_map::Entry, HashMap, HashSet};

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
    AssignTargetKind, AssignTargetShape, Checker, FnSig, MethodCallReceiverKind, MethodCallRewrite,
    SpanKey, TypeCheckOutput, TypeDef, TypeDefKind, VariantDef,
};
use self::types::{
    ConstValue, DeferredCastCheck, DeferredChannelMethodRewrite, DeferredHashMapAdmission,
    DeferredHashSetAdmission, DeferredInferenceHole, DeferredMonomorphicSite, ImplAliasEntry,
    ImplAliasScope, ImportKey, IntegerTypeInfo, PendingLoweringFact, TraitAssociatedTypeInfo,
    TraitInfo, WasmUnsupportedFeature,
};
use self::util::{
    collect_unresolved_inference_vars, extract_float_literal_value, extract_integer_literal_value,
    first_infer_span_in_extern_fn, first_infer_span_in_type_expr, float_fits_type,
    integer_fits_type, integer_type_info, integer_type_range, is_float_literal, is_integer_literal,
    lookup_scoped_item, scoped_module_item_name,
};
use crate::lowering_facts::{LoweringFact, LoweringFactError};

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
        );
        let mut resolved_lowering_facts = self.finalize_lowering_facts();
        admissibility::validate_lowering_facts_output_contract(
            &mut resolved_lowering_facts,
            &resolved_expr_types,
        );
        self.finalize_hashmap_admission();
        self.finalize_hashset_admission();
        self.finalize_channel_rewrites();

        self.report_unresolved_inference_holes(program);
        self.report_unresolved_monomorphic_sites();

        let mut output = TypeCheckOutput {
            expr_types: resolved_expr_types,
            method_call_receiver_kinds: std::mem::take(&mut self.method_call_receiver_kinds),
            lowering_facts: resolved_lowering_facts,
            method_call_rewrites: std::mem::take(&mut self.method_call_rewrites),
            assign_target_kinds: std::mem::take(&mut self.assign_target_kinds),
            assign_target_shapes: std::mem::take(&mut self.assign_target_shapes),
            errors: std::mem::take(&mut self.errors),
            warnings: std::mem::take(&mut self.warnings),
            type_defs: resolved_type_defs,
            fn_sigs: resolved_fn_sigs,
            handle_bearing_structs: std::mem::take(&mut self.handle_bearing_structs),
            cycle_capable_actors: HashSet::new(),
            user_modules: std::mem::take(&mut self.user_modules),
            call_type_args: resolved_call_type_args,
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
