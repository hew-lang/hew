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
    AssignTargetKind, AssignTargetShape, Checker, FnSig, MethodCallReceiverKind, SpanKey,
    TypeCheckOutput, TypeDef, TypeDefKind, VariantDef,
};
use self::types::{
    ConstValue, DeferredCastCheck, DeferredInferenceHole, DeferredMonomorphicSite, ImplAliasEntry,
    ImplAliasScope, IntegerTypeInfo, TraitAssociatedTypeInfo, TraitInfo, WasmUnsupportedFeature,
};
use self::util::{
    collect_unresolved_inference_vars, extract_float_literal_value, extract_integer_literal_value,
    first_infer_span_in_extern_fn, first_infer_span_in_type_expr, float_fits_type,
    integer_fits_type, integer_type_info, integer_type_range, is_float_literal, is_integer_literal,
    lookup_scoped_item, scoped_module_item_name, ty_contains_rc_deep,
    ty_has_unresolved_inference_var,
};

fn resolve_builtin_result_output_type_args(ok_ty: Ty, err_ty: Ty) -> Option<(Ty, Ty)> {
    let ok_unresolved = ty_has_unresolved_inference_var(&ok_ty);
    let err_unresolved = ty_has_unresolved_inference_var(&err_ty);
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

fn normalize_synthetic_channel_handle_expr_type(ty: &Ty) -> Ty {
    match ty {
        Ty::Named { name, args } => {
            let normalized_args: Vec<Ty> = args
                .iter()
                .map(normalize_synthetic_channel_handle_expr_type)
                .collect();
            if matches!(
                builtin_named_type(name.as_str()),
                Some(kind) if kind.is_channel_handle()
            ) && matches!(normalized_args.as_slice(), [Ty::Var(_)])
            {
                return Ty::normalize_named(name.clone(), vec![]);
            }
            Ty::normalize_named(name.clone(), normalized_args)
        }
        Ty::Tuple(elems) => Ty::Tuple(
            elems
                .iter()
                .map(normalize_synthetic_channel_handle_expr_type)
                .collect(),
        ),
        Ty::Array(elem, size) => Ty::Array(
            Box::new(normalize_synthetic_channel_handle_expr_type(elem)),
            *size,
        ),
        Ty::Slice(elem) => Ty::Slice(Box::new(normalize_synthetic_channel_handle_expr_type(elem))),
        Ty::Function { params, ret } => Ty::Function {
            params: params
                .iter()
                .map(normalize_synthetic_channel_handle_expr_type)
                .collect(),
            ret: Box::new(normalize_synthetic_channel_handle_expr_type(ret)),
        },
        Ty::Closure {
            params,
            ret,
            captures,
        } => Ty::Closure {
            params: params
                .iter()
                .map(normalize_synthetic_channel_handle_expr_type)
                .collect(),
            ret: Box::new(normalize_synthetic_channel_handle_expr_type(ret)),
            captures: captures
                .iter()
                .map(normalize_synthetic_channel_handle_expr_type)
                .collect(),
        },
        Ty::Pointer {
            is_mutable,
            pointee,
        } => Ty::Pointer {
            is_mutable: *is_mutable,
            pointee: Box::new(normalize_synthetic_channel_handle_expr_type(pointee)),
        },
        _ => ty.clone(),
    }
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
        for (module_name, import_span) in &self.import_spans {
            if !self.used_modules.borrow().contains(module_name) {
                self.warnings.push(TypeError {
                    severity: crate::error::Severity::Warning,
                    kind: TypeErrorKind::UnusedImport,
                    span: import_span.clone(),
                    message: format!("unused import: `{module_name}`"),
                    notes: vec![],
                    suggestions: vec!["remove this import".to_string()],
                    source_module: None,
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
        let resolved_call_type_args: HashMap<SpanKey, Vec<Ty>> =
            std::mem::take(&mut self.call_type_args)
                .into_iter()
                .filter_map(|(k, args)| {
                    let resolved: Vec<Ty> = args
                        .iter()
                        .map(|a| self.subst.resolve(a).materialize_literal_defaults())
                        .collect();
                    if resolved
                        .iter()
                        .all(|ty| !ty_has_unresolved_inference_var(ty))
                    {
                        Some((k, resolved))
                    } else {
                        None
                    }
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

        // Fail-closed boundary: unresolved inference vars must not cross from
        // type-checking into serialized expr type metadata.
        // When another inference diagnostic path already covers the same hole
        // vars, suppress the fallback boundary error so we keep the more
        // specific message/span/source-module envelope.
        let mut covered_inference_vars = HashSet::new();
        for hole_vars in self
            .type_def_inference_holes
            .values()
            .chain(self.fn_sig_inference_holes.values())
            .chain(
                self.deferred_inference_holes
                    .iter()
                    .map(|hole| &hole.hole_vars),
            )
        {
            for hole_var in hole_vars {
                let resolved_hole = self.subst.resolve(&Ty::Var(*hole_var));
                collect_unresolved_inference_vars(&resolved_hole, &mut covered_inference_vars);
            }
        }
        for site in &self.deferred_monomorphic_sites {
            let resolved = self.subst.resolve(&site.ty);
            collect_unresolved_inference_vars(&resolved, &mut covered_inference_vars);
        }
        // Let-bound generic lambdas use fresh type vars as polymorphic
        // placeholders until a later call site instantiates them. Those vars
        // are not checker-output leaks and should not trigger the fail-closed
        // boundary fallback when the lambda remains unused.
        for poly_vars in self.lambda_poly_type_var_map.values() {
            for (_, poly_var) in poly_vars {
                let resolved_poly = self.subst.resolve(&Ty::Var(*poly_var));
                collect_unresolved_inference_vars(&resolved_poly, &mut covered_inference_vars);
            }
        }
        let mut seen_inference_spans: HashSet<SpanKey> = self
            .errors
            .iter()
            .filter(|e| e.kind == TypeErrorKind::InferenceFailed)
            .map(|e| SpanKey::from(&e.span))
            .collect();
        let mut leaked_expr_type_spans = Vec::new();
        for (span, ty) in &mut resolved_expr_types {
            let mut unresolved = HashSet::new();
            collect_unresolved_inference_vars(ty, &mut unresolved);
            if unresolved.is_empty() {
                continue;
            }
            if !unresolved.is_subset(&covered_inference_vars) {
                let normalized = normalize_synthetic_channel_handle_expr_type(ty);
                if normalized != *ty {
                    let mut normalized_unresolved = HashSet::new();
                    collect_unresolved_inference_vars(&normalized, &mut normalized_unresolved);
                    if normalized_unresolved.is_empty() {
                        *ty = normalized;
                        continue;
                    }
                }
            }
            leaked_expr_type_spans.push(span.clone());
            if unresolved.is_subset(&covered_inference_vars) {
                continue;
            }
            if seen_inference_spans.insert(span.clone()) {
                let mut err = TypeError::inference_failed(
                    Span {
                        start: span.start,
                        end: span.end,
                    },
                    "expression type at checker output boundary",
                );
                err.source_module = self.expr_type_source_modules.get(span).cloned().flatten();
                self.errors.push(err);
            }
        }
        for span in leaked_expr_type_spans {
            resolved_expr_types.remove(&span);
        }

        let resolved_type_defs: HashMap<String, TypeDef> = std::mem::take(&mut self.type_defs)
            .into_iter()
            .map(|(name, type_def)| (name, self.resolve_type_def(&type_def)))
            .collect();

        let resolved_fn_sigs: HashMap<String, FnSig> = std::mem::take(&mut self.fn_sigs)
            .into_iter()
            .map(|(name, sig)| (name, self.resolve_fn_sig(&sig)))
            .collect();

        self.report_unresolved_inference_holes(program);
        self.report_unresolved_monomorphic_sites();

        let mut output = TypeCheckOutput {
            expr_types: resolved_expr_types,
            method_call_receiver_kinds: std::mem::take(&mut self.method_call_receiver_kinds),
            assign_target_kinds: std::mem::take(&mut self.assign_target_kinds),
            assign_target_shapes: std::mem::take(&mut self.assign_target_shapes),
            errors: std::mem::take(&mut self.errors),
            warnings: std::mem::take(&mut self.warnings),
            type_defs: resolved_type_defs,
            fn_sigs: resolved_fn_sigs,
            cycle_capable_actors: HashSet::new(),
            user_modules: std::mem::take(&mut self.user_modules),
            call_type_args: resolved_call_type_args,
        };

        // Detect actor reference cycles and emit warnings.
        let (cycle_capable, cycles) = crate::cycle::detect_actor_ref_cycles(&output.type_defs);
        for cycle_actors in &cycles {
            let desc = cycle_actors.join(" -> ");
            output
                .warnings
                .push(TypeError::actor_ref_cycle(0..0, &desc));
        }
        output.cycle_capable_actors = cycle_capable;

        output
    }
}
