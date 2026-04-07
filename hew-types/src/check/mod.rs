//! Bidirectional type checker for Hew programs.

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

pub use self::types::{Checker, FnSig, SpanKey, TypeCheckOutput, TypeDef, TypeDefKind, VariantDef};
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
                    self.current_module = Some(module_name);
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
                    for (item, span) in &module.items {
                        self.check_item(item, span);
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
                });
            }
        }

        self.emit_dead_code_warnings();

        self.default_unconstrained_range_types(&expr_types);
        // Re-record range bound spans with their concrete element types
        // (resolved after inference + defaulting) and validate fits.
        self.apply_deferred_range_bound_types(&mut expr_types);

        // Move data out of Checker — it is not used after check_program.
        // Resolve any remaining type variables in expr_types via the
        // substitution so the enrichment layer sees concrete types.
        let resolved_expr_types: HashMap<SpanKey, Ty> = expr_types
            .into_iter()
            .map(|(k, v)| (k, self.subst.resolve(&v)))
            .collect();

        // Also resolve inferred call type args so the enrichment layer can
        // fill in explicit type annotations for the codegen.
        let resolved_call_type_args: HashMap<SpanKey, Vec<Ty>> =
            std::mem::take(&mut self.call_type_args)
                .into_iter()
                .map(|(k, args)| {
                    let resolved = args.iter().map(|a| self.subst.resolve(a)).collect();
                    (k, resolved)
                })
                .collect();

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
