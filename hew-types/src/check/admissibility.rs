use super::types::{VecCursorMode, VecIterationMode};
#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;
use crate::type_facts::CloneKind;
use crate::value_class::{ClassError, ValueClass};
use crate::BuiltinType;

/// How one value class reads in a diagnostic, so a refusal names the rule that
/// produced it rather than the shape the checker happened to walk.
fn class_description(class: ValueClass) -> &'static str {
    match class {
        ValueClass::BitCopy => "a bit-copyable value",
        ValueClass::View => "a non-owning view",
        ValueClass::CowValue => "a heap value",
        ValueClass::PersistentShare => "a shared value whose descriptor carries no copy slot",
        ValueClass::AffineResource => "an affine resource",
        ValueClass::Linear => "a linear value that must be consumed",
    }
}

pub(crate) fn signature_contains_error_type(params: &[Ty], ret: &Ty) -> bool {
    params.iter().any(ty_contains_error) || ty_contains_error(ret)
}

// ── Layout computation helpers (C-2c) ─────────────────────────────────────────

/// Return the compiler-owned fixed layout for a node identity aggregate.
#[must_use]
pub(crate) fn identity_aggregate_layout(ty: &Ty) -> Option<(usize, usize)> {
    match ty {
        Ty::Named {
            builtin: Some(BuiltinType::ChildRef | BuiltinType::NodeId),
            ..
        } => Some((16, 8)),
        Ty::Named {
            builtin: Some(BuiltinType::Location | BuiltinType::RemotePid),
            ..
        } => Some((32, 8)),
        _ => None,
    }
}

/// Round `offset` up to the next multiple of `align`.
///
/// `align` must be a power of two or 1.  Wrapping arithmetic is used so
/// overflow stays defined; callers that need overflow protection should
/// check the result against a known maximum.
fn align_up(offset: usize, align: usize) -> usize {
    if align <= 1 {
        return offset;
    }
    // align is a power of two (ensured by callers), so this is branch-free.
    (offset.wrapping_add(align - 1)) & !(align - 1)
}

/// Return `(size, align)` for a single Copy-eligible primitive or nested record.
///
/// Returns `None` for types that are not hash-eligible primitives or Copy records.
/// Only types that `ty_is_hash_eligible` would return `Eligible` for are expected
/// here; the function is conservative and returns `None` for everything else.
pub(crate) fn primitive_copy_layout(
    ty: &Ty,
    type_defs: &HashMap<String, TypeDef>,
) -> Option<(usize, usize)> {
    primitive_copy_layout_on_path(ty, type_defs, &mut HashSet::new())
}

/// `primitive_copy_layout` with the currently-expanded record declarations.
///
/// The key is the resolved declaration name rather than `Ty`'s display
/// spelling: recursive generics can revisit one declaration with ever-growing
/// arguments, and imported uses can spell a declaration with a module prefix.
/// Either way, a repeated nominal is an inline recursive layout and has no
/// finite Copy size.
fn primitive_copy_layout_on_path(
    ty: &Ty,
    type_defs: &HashMap<String, TypeDef>,
    visiting: &mut HashSet<String>,
) -> Option<(usize, usize)> {
    if let Some(layout) = identity_aggregate_layout(ty) {
        return Some(layout);
    }
    match ty {
        Ty::Bool | Ty::I8 | Ty::U8 => Some((1, 1)),
        Ty::I16 | Ty::U16 => Some((2, 2)),
        // f32 is hash-ineligible (caught by ty_is_hash_eligible) but included
        // for completeness so this function can serve as a general primitive sizer.
        Ty::I32 | Ty::U32 | Ty::Char | Ty::F32 => Some((4, 4)),
        // f64 is hash-ineligible but included for the same reason.
        Ty::I64 | Ty::U64 | Ty::Duration | Ty::F64 => Some((8, 8)),
        Ty::Array(elem, count) => {
            let (elem_size, elem_align) = primitive_copy_layout_on_path(elem, type_defs, visiting)?;
            let count = usize::try_from(*count).ok()?;
            Some((elem_size.checked_mul(count)?, elem_align))
        }
        Ty::Named { name, args, .. } => {
            let type_def = crate::check::types::type_def_for_spelling(type_defs, name)?;
            let visit_key = type_def.name.clone();
            if !visiting.insert(visit_key.clone()) {
                return None;
            }
            let layout = if args.is_empty() {
                compute_copy_record_layout_on_path(type_def, type_defs, visiting)
            } else {
                if type_def.type_params.len() != args.len() {
                    visiting.remove(&visit_key);
                    return None;
                }
                compute_copy_record_layout_with_args_on_path(type_def, args, type_defs, visiting)
            };
            visiting.remove(&visit_key);
            layout
        }
        _ => None,
    }
}

/// Compute a field layout while retaining the origin of direct type-parameter
/// members. A supplied type argument is already a finite concrete payload, so
/// it starts a fresh layout path (`Wrap<Wrap<i64>>` is not self-recursive). A
/// definition-level edge such as `Wrap<T> { next: Wrap<Wrap<T>> }` stays on
/// the active path and therefore still fails closed.
fn primitive_copy_layout_member_on_path(
    ty: &Ty,
    type_def: &TypeDef,
    type_args: &[Ty],
    type_defs: &HashMap<String, TypeDef>,
    visiting: &mut HashSet<String>,
) -> Option<(usize, usize)> {
    match ty {
        Ty::Named {
            name,
            args,
            builtin: None,
        } if args.is_empty() => {
            let type_param_index = type_def
                .type_params
                .iter()
                .position(|param| param == name)?;
            let type_arg = type_args.get(type_param_index)?;
            // A fresh parameter path is needed for finite Wrap<Wrap<i64>>,
            // but must not erase a declaration cycle through that parameter.
            // Reuse the checker termination proof before restarting the walk;
            // it also refuses cycles with growing generic arguments.
            let resolved_arg = ResolvedTy::from_ty(type_arg).ok()?;
            if !declaration_walk_terminates(&resolved_arg, type_defs) {
                return None;
            }
            primitive_copy_layout(type_arg, type_defs)
        }
        Ty::Array(elem, count) => {
            let (elem_size, elem_align) = primitive_copy_layout_member_on_path(
                elem, type_def, type_args, type_defs, visiting,
            )?;
            let count = usize::try_from(*count).ok()?;
            Some((elem_size.checked_mul(count)?, elem_align))
        }
        _ => {
            let subst: HashMap<String, Ty> = type_def
                .type_params
                .iter()
                .zip(type_args.iter())
                .map(|(param, arg)| (param.clone(), arg.clone()))
                .collect();
            let instantiated = ty.substitute_named_params_parallel(&subst);
            primitive_copy_layout_on_path(&instantiated, type_defs, visiting)
        }
    }
}

fn compute_copy_record_layout_on_path(
    type_def: &TypeDef,
    type_defs: &HashMap<String, TypeDef>,
    visiting: &mut HashSet<String>,
) -> Option<(usize, usize)> {
    compute_copy_record_layout_with_args_on_path(type_def, &[], type_defs, visiting)
}

fn compute_copy_record_layout_with_args_on_path(
    type_def: &TypeDef,
    type_args: &[Ty],
    type_defs: &HashMap<String, TypeDef>,
    visiting: &mut HashSet<String>,
) -> Option<(usize, usize)> {
    if type_def.fields.is_empty() {
        // Zero-size key is an ABI violation: `hew_hashmap_new_with_layout` aborts
        // when `key_layout.size == 0`.
        return None;
    }

    let mut offset: usize = 0;
    let mut max_align: usize = 1;

    // Walk fields in declaration order when available (populated by register_record_decl).
    // Fall back to alphabetical order for synthetic/test TypeDefs so layout tests
    // that build TypeDefs by hand still produce a deterministic result.
    let ordered_names: Vec<&String>;
    let mut alpha_sorted: Vec<&String>;
    let field_names: &[&String] = if type_def.field_order.is_empty() {
        alpha_sorted = type_def.fields.keys().collect();
        alpha_sorted.sort();
        &alpha_sorted
    } else {
        ordered_names = type_def.field_order.iter().collect();
        &ordered_names
    };

    for name in field_names {
        let field_ty = type_def.fields.get(*name)?;
        let (field_size, field_align) = primitive_copy_layout_member_on_path(
            field_ty, type_def, type_args, type_defs, visiting,
        )?;

        // Align the field start offset to the field's natural alignment.
        offset = align_up(offset, field_align);
        offset = offset.checked_add(field_size)?;
        if field_align > max_align {
            max_align = field_align;
        }
    }

    // Round the total size up to the struct's natural alignment.
    let total_size = align_up(offset, max_align);
    if total_size == 0 {
        return None;
    }

    Some((total_size, max_align))
}

/// Enforce the fail-closed output contract for `lowering_facts` after
/// [`Checker::finalize_lowering_facts`] has run.
///
/// Two conditions trigger removal of a [`LoweringFact`] entry:
///
/// 1. **Orphaned span** — the `SpanKey` is absent from the post-validation
///    `expr_types` map.  If the owning expression was pruned by
///    `validate_checker_output_contract` (leaked inference vars, cascading
///    `Ty::Error`, etc.) the corresponding lowering fact must also be dropped so
///    downstream serialization/codegen cannot observe a fact without a resolved
///    expression type.
///
/// 2. **Internally inconsistent fact** (defensive) — the `element_type` /
///    `abi_variant` pairing violates the checker-invariant.  In practice this
///    cannot occur through the normal construction path
///    (`LoweringFact::from_hashset_element_type`) but the check is kept as a
///    hard contract at the boundary so that any future serialization round-trip
///    or factory bypasses are caught at check time rather than in codegen.
///
/// Note: element types that resolve to `Ty::Error` are already handled earlier
/// in `finalize_lowering_facts` (silent drop, no new error).  The orphan-prune
/// here is a secondary defence for any path that might add facts after the main
/// validation pass.
pub(super) fn validate_lowering_facts_output_contract(
    lowering_facts: &mut HashMap<SpanKey, LoweringFact>,
    expr_types: &HashMap<SpanKey, Ty>,
) {
    use crate::lowering_facts::{HashSetAbi, HashSetElementType, LoweringKind};
    lowering_facts.retain(|key, fact| {
        // Condition 1: orphaned span.
        if !expr_types.contains_key(key) {
            return false;
        }
        // Condition 2: element_type ↔ abi_variant internal consistency.
        matches!(
            (fact.kind, fact.element_type, &fact.abi_variant),
            (
                LoweringKind::HashSet,
                HashSetElementType::I64 | HashSetElementType::U64,
                HashSetAbi::Int64
            ) | (
                LoweringKind::HashSet,
                HashSetElementType::Str,
                HashSetAbi::String
            )
        )
    });
}

fn ty_contains_error(ty: &Ty) -> bool {
    ty.contains_error()
}

fn variant_def_has_inference_var(variant: &VariantDef) -> bool {
    match variant {
        VariantDef::Unit => false,
        VariantDef::Tuple(fields) => fields.iter().any(Ty::has_inference_var),
        VariantDef::Struct(fields) => fields
            .iter()
            .map(|(_, field)| field)
            .any(Ty::has_inference_var),
    }
}

fn fn_sig_has_inference_var(sig: &FnSig) -> bool {
    sig.params.iter().any(Ty::has_inference_var) || sig.return_type.has_inference_var()
}

fn variant_def_contains_error_type(variant: &VariantDef) -> bool {
    match variant {
        VariantDef::Unit => false,
        VariantDef::Tuple(fields) => fields.iter().any(ty_contains_error),
        VariantDef::Struct(fields) => fields.iter().any(|(_, ty)| ty_contains_error(ty)),
    }
}

fn type_def_shape_contains_error_type(type_def: &TypeDef) -> bool {
    type_def.fields.values().any(ty_contains_error)
        || type_def
            .variants
            .values()
            .any(variant_def_contains_error_type)
}

fn type_def_shape_has_inference_var(type_def: &TypeDef) -> bool {
    type_def.fields.values().any(Ty::has_inference_var)
        || type_def
            .variants
            .values()
            .any(variant_def_has_inference_var)
}

#[derive(Clone, Copy)]
enum ConcreteCollectionKind {
    Vec,
    HashSet,
    HashMap,
}

impl ConcreteCollectionKind {
    fn validate_named_collection(
        self,
        checker: &mut Checker,
        builtin: Option<BuiltinType>,
        args: &[Ty],
        span: &Span,
    ) -> Option<bool> {
        match self {
            Self::Vec if builtin == Some(BuiltinType::Vec) && args.len() == 1 => {
                Some(checker.validate_vec_element_type(&args[0], span))
            }
            Self::HashSet if builtin == Some(BuiltinType::HashSet) && args.len() == 1 => {
                // Skip admission when the element type is still unresolved or
                // erroneous: Ty::Var is not yet decidable (inference may
                // resolve it), and Ty::Error already has an upstream
                // diagnostic.  The dedicated deferred-admission paths
                // (validate_hashset_element_type from method-call sites) and
                // the inference-holes path handle those cases with proper
                // authority and without duplication.
                let resolved = checker.subst.resolve(&args[0]);
                if matches!(resolved, Ty::Var(_) | Ty::Error) {
                    return Some(true);
                }
                Some(checker.validate_hashset_element_type(&args[0], span))
            }
            Self::HashMap if builtin == Some(BuiltinType::HashMap) && args.len() == 2 => {
                // Same: skip admission for undecidable/erroneous args.
                let resolved_key = checker.subst.resolve(&args[0]);
                let resolved_val = checker.subst.resolve(&args[1]);
                if matches!(resolved_key, Ty::Var(_) | Ty::Error)
                    || matches!(resolved_val, Ty::Var(_) | Ty::Error)
                {
                    return Some(true);
                }
                Some(checker.validate_hashmap_key_value_types(&args[0], &args[1], span))
            }
            _ => None,
        }
    }
}

impl Checker {
    pub(super) fn validate_checker_output_contract(
        &mut self,
        expr_types: &mut HashMap<SpanKey, Ty>,
        type_defs: &mut HashMap<String, TypeDef>,
        fn_sigs: &mut HashMap<String, FnSig>,
        call_type_args: &mut HashMap<SpanKey, Vec<Ty>>,
        record_init_type_args: &mut HashMap<SpanKey, Vec<Ty>>,
    ) {
        let covered_inference_vars = self.collect_output_contract_tracked_inference_vars();
        self.validate_expr_output_contract(expr_types, &covered_inference_vars);

        type_defs.retain(|_, type_def| {
            if type_def_shape_has_inference_var(type_def)
                || type_def_shape_contains_error_type(type_def)
            {
                return false;
            }
            type_def.methods.retain(|_, sig| {
                !fn_sig_has_inference_var(sig)
                    && !signature_contains_error_type(&sig.params, &sig.return_type)
            });
            true
        });
        self.validate_handle_types_no_field_overlap(type_defs);

        fn_sigs.retain(|_, sig| {
            !fn_sig_has_inference_var(sig)
                && !signature_contains_error_type(&sig.params, &sig.return_type)
        });
        Self::validate_call_type_args_output_contract(call_type_args, expr_types);
        Self::validate_record_init_type_args_output_contract(record_init_type_args, expr_types);
        self.validate_assign_target_output_contract();
        self.validate_method_call_output_contract(expr_types);
        self.validate_method_call_receiver_kinds_output_contract(type_defs, fn_sigs);
    }

    /// Validate that no type in `type_defs` is simultaneously registered as an
    /// opaque handle type in the module registry.
    ///
    /// A type cannot be both fieldless-opaque in the stdlib handle registry
    /// (`module_registry.handle_types`) and field-bearing in `TypeDef.fields`:
    /// the two representations are incompatible in codegen (the opaque-handle
    /// path versus the struct-layout path).  If both are present the
    /// opaque path silently wins, producing wrong codegen without a diagnostic.
    ///
    /// Only types with non-empty `fields` are checked — a user-declared type
    /// whose name coincidentally matches the short form of a stdlib handle type
    /// cannot trigger a false positive: user-declared names never contain `'.'`,
    /// so the only `type_defs` keys that can match a qualified handle name (e.g.
    /// `"tls.TlsStream"`) are those inserted by `register_qualified_type_alias`.
    /// That alias path is precisely the overlap scenario this check is meant to
    /// catch.
    pub(super) fn validate_handle_types_no_field_overlap(
        &mut self,
        type_defs: &mut HashMap<String, TypeDef>,
    ) {
        let conflicts: HashSet<String> = type_defs
            .iter()
            .filter(|(name, type_def)| {
                !type_def.fields.is_empty() && self.module_registry.is_handle_type(name)
            })
            .map(|(name, _)| name.clone())
            .collect();

        for name in &conflicts {
            let span = self
                .type_def_spans
                .get(name.as_str())
                .cloned()
                .unwrap_or(0..0);
            self.report_error(
                TypeErrorKind::InvalidOperation,
                &span,
                format!(
                    "type `{name}` is registered as an opaque handle but also declares \
                     fields; remove the fields or remove the handle registration"
                ),
            );
        }

        type_defs.retain(|k, _| !conflicts.contains(k));

        // Defensive: also prune the bare-alias twin (short form) if a qualified key
        // is removed. If `fake.Handle` conflicts and is removed, also remove `Handle`
        // so `lookup_user_type_def`'s fallback cannot resolve to a field-bearing entry.
        for name in &conflicts {
            if let Some((_, short)) = name.split_once('.') {
                // Current register_qualified_type_alias format is "{module_short}.{name}"; split_once is safe here. If multi-dot module paths are ever added, switch to rsplit_once or a dedicated helper.
                type_defs.remove(short);
            }
        }
    }

    fn collect_output_contract_tracked_inference_vars(&self) -> HashSet<TypeVar> {
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
        for admission in self.deferred_vec_admission.values() {
            let resolved = self.subst.resolve(&admission.elem_ty);
            collect_unresolved_inference_vars(&resolved, &mut covered_inference_vars);
        }
        for sig in self.lambda_poly_sig_map.values() {
            for poly_var in &sig.type_vars {
                let resolved_poly = self.subst.resolve(&Ty::Var(*poly_var));
                collect_unresolved_inference_vars(&resolved_poly, &mut covered_inference_vars);
            }
        }
        covered_inference_vars
    }

    pub(super) fn validate_expr_output_contract(
        &mut self,
        expr_types: &mut HashMap<SpanKey, Ty>,
        covered_inference_vars: &HashSet<TypeVar>,
    ) {
        let mut seen_inference_spans: HashSet<SpanKey> = self
            .errors
            .iter()
            .filter(|e| e.kind == TypeErrorKind::InferenceFailed)
            .map(|e| SpanKey::in_module(&e.span, self.current_module_idx))
            .collect();
        let mut leaked_expr_type_spans = Vec::new();
        for (span, ty) in expr_types.iter_mut() {
            let mut unresolved = HashSet::new();
            collect_unresolved_inference_vars(ty, &mut unresolved);
            if unresolved.is_empty() {
                continue;
            }
            leaked_expr_type_spans.push(span.clone());
            if unresolved.is_subset(covered_inference_vars) {
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
            expr_types.remove(&span);
        }
    }

    /// Validates `call_type_args` at the checker output boundary.
    ///
    /// Two conditions trigger removal:
    ///
    /// 1. **Orphaned span** — the `SpanKey` is absent from the post-validation
    ///    `expr_types` map, meaning the owning expression was pruned by
    ///    `validate_expr_output_contract` (leaked inference vars, cascading
    ///    errors, etc.).  This mirrors the fail-closed pruning already applied
    ///    to `method_call_receiver_kinds` / `method_call_rewrites`.
    /// 2. **Leaked inference variable** — any type argument still contains an
    ///    unresolved `Ty::Var`.  A call site whose type arguments are
    ///    unresolved must not cross the checker output boundary into codegen.
    fn validate_call_type_args_output_contract(
        call_type_args: &mut HashMap<SpanKey, Vec<Ty>>,
        expr_types: &HashMap<SpanKey, Ty>,
    ) {
        call_type_args.retain(|key, args| {
            expr_types.contains_key(key) && args.iter().all(|ty| !ty.has_inference_var())
        });
    }

    /// Validates `record_init_type_args` at the checker output boundary.
    ///
    /// Mirrors `validate_call_type_args_output_contract` for the record-init
    /// monomorphisation surface:
    ///
    /// 1. **Orphaned span** — the `SpanKey` is absent from the post-validation
    ///    `expr_types` map (the initialiser expression was pruned for leaked
    ///    inference vars or cascading errors). The associated type arguments
    ///    must not cross into HIR / MIR.
    /// 2. **Leaked inference variable** — any type argument still contains an
    ///    unresolved `Ty::Var`.  Downstream HIR monomorphisation requires
    ///    every arg to be fully concrete.
    fn validate_record_init_type_args_output_contract(
        record_init_type_args: &mut HashMap<SpanKey, Vec<Ty>>,
        expr_types: &HashMap<SpanKey, Ty>,
    ) {
        record_init_type_args.retain(|key, args| {
            expr_types.contains_key(key) && args.iter().all(|ty| !ty.has_inference_var())
        });
    }

    /// Prune `method_call_receiver_kinds` and `method_call_rewrites` entries
    /// whose `SpanKey` is absent from the validated `expr_types` map.
    ///
    /// `expr_types` here is the post-validation map produced by
    /// `validate_expr_output_contract` — any span that was pruned there (due
    /// to leaked inference vars, cascading errors, etc.) is authoritative
    /// evidence that the corresponding method-call side-table entry is orphaned
    /// and must not leak to the output.  This mirrors the fail-closed contract
    /// already applied to `assign_target_kinds` / `assign_target_shapes`.
    fn validate_method_call_output_contract(&mut self, expr_types: &HashMap<SpanKey, Ty>) {
        self.method_call_receiver_kinds
            .retain(|key, _| expr_types.contains_key(key));
        self.method_call_rewrites
            .retain(|key, _| expr_types.contains_key(key));
        self.actor_method_dispatch.retain(|key, dispatch| {
            if !expr_types.contains_key(key) {
                return false;
            }
            match dispatch {
                ActorMethodKind::Message { .. } => true,
                // Output-contract pruning ONLY: retain the dispatch entry when
                // the reply type is fully resolved and error-free. This is NOT
                // the reply-type admissibility gate — a non-Send reply (`Rc`,
                // etc.) is rejected at record time by `record_actor_method_dispatch`
                // (methods.rs, `E_DUPLEX_NON_SEND`), and any reply the codegen
                // reply-drop classifier cannot prove safe to drop fails closed
                // there (#1739). Do not add a Send/handle rejection here; this
                // pass runs after type-checking and only graduates the
                // side-table to a validated contract.
                ActorMethodKind::Ask { reply_ty, .. } => {
                    !reply_ty.has_inference_var() && !reply_ty.contains_error()
                }
                // Same output-contract pruning as `Ask`: retain only when the
                // stream element type is fully resolved and error-free.
                ActorMethodKind::StreamProducer(_, elem_ty) => {
                    !elem_ty.has_inference_var() && !elem_ty.contains_error()
                }
            }
        });
    }

    /// Validates `method_call_receiver_kinds` at the checker output boundary.
    ///
    /// This pass graduates the side-table from producer discipline to a validated
    /// contract by asserting that every surviving entry references a type or trait
    /// that still exists in the resolved program environment, then pruning any that
    /// do not.
    ///
    /// - `NamedTypeInstance { type_name }` entries are retained if the type is
    ///   present in the resolved `type_defs` (user-defined type), the name is
    ///   module-qualified (contains `'.'`, i.e., a stdlib handle type such as
    ///   `json.Value` or `http.Client` which live in the module registry rather
    ///   than `type_defs`), or the name is a generic type parameter from a
    ///   function signature (trait-bounded type-parameter dispatch records the
    ///   type-parameter name as a `NamedTypeInstance`).
    /// - `TraitObject { trait_name }` entries are retained only if the trait name
    ///   is still present in the checker's trait registry.
    pub(super) fn validate_method_call_receiver_kinds_output_contract(
        &mut self,
        type_defs: &HashMap<String, TypeDef>,
        fn_sigs: &HashMap<String, FnSig>,
    ) {
        // Collect known trait names before the mutable borrow on
        // `method_call_receiver_kinds` to avoid a split-borrow conflict.
        let known_trait_names: HashSet<String> = self.trait_defs.keys().cloned().collect();

        // Collect all type parameter names from the resolved function signatures
        // so we can retain `NamedTypeInstance` entries produced by trait-bounded
        // type-parameter method dispatch (e.g. `T` in `fn f<T: Show>(t: T)`).
        // NOTE: we receive `fn_sigs` as a parameter because the production path
        // drains `self.fn_sigs` via `std::mem::take` before this validator runs.
        let known_type_params: HashSet<&str> = fn_sigs
            .values()
            .flat_map(|sig| sig.type_params.iter().map(String::as_str))
            .collect();

        self.method_call_receiver_kinds
            .retain(|_, kind| match kind {
                MethodCallReceiverKind::LexicalBinding { binding_name } => !binding_name.is_empty(),
                MethodCallReceiverKind::ModuleBinding { module_name } => !module_name.is_empty(),
                MethodCallReceiverKind::EnumConstructorPath { type_name } => type_defs
                    .get(type_name)
                    .is_some_and(|type_def| type_def.kind == TypeDefKind::Enum),
                MethodCallReceiverKind::NamedTypeInstance { type_name } => {
                    type_defs.contains_key(type_name)
                        || type_name.contains('.')
                        || known_type_params.contains(type_name.as_str())
                }
                MethodCallReceiverKind::ActorInstance { actor_name } => type_defs
                    .get(actor_name)
                    .is_some_and(|type_def| type_def.kind == TypeDefKind::Actor),
                MethodCallReceiverKind::HandleInstance { type_name } => !type_name.is_empty(),
                MethodCallReceiverKind::TraitObject { trait_name } => {
                    known_trait_names.contains(trait_name)
                }
                MethodCallReceiverKind::StreamInstance { element_kind } => {
                    matches!(element_kind.as_str(), "" | "string" | "bytes")
                }
                MethodCallReceiverKind::PrimitiveTraitImpl {
                    trait_name,
                    canonical_receiver,
                } => {
                    // Retain only when the trait still exists and the canonical
                    // receiver key still matches one we'd produce today (i.e.
                    // the registration helper would still accept it).  This
                    // mirrors the producer discipline added in Stage A1 and
                    // prevents stale entries from leaking past the checker
                    // output boundary.
                    let trait_known = known_trait_names.contains(trait_name);
                    let receiver_known =
                        Self::is_known_primitive_or_builtin_canonical_key(canonical_receiver);
                    trait_known && receiver_known
                }
            });
    }

    /// Whether `key` is a canonical receiver key the registration helper
    /// (`canonical_primitive_or_builtin_key_from_name`) would emit today.
    /// Mirrors the closed-set producer logic so the validator can fail
    /// closed on unknown keys rather than allowing arbitrary strings to
    /// survive the output boundary.
    fn is_known_primitive_or_builtin_canonical_key(key: &str) -> bool {
        matches!(
            key,
            "i8" | "i16"
                | "i32"
                | "i64"
                | "u8"
                | "u16"
                | "u32"
                | "u64"
                | "f32"
                | "f64"
                | "bool"
                | "char"
                | "string"
                | "bytes"
                | "duration"
                | "Vec"
                | "HashMap"
                | "HashSet"
        )
    }

    fn validate_assign_target_output_contract(&mut self) {
        let valid_keys: HashSet<_> = self
            .assign_target_kinds
            .keys()
            .filter(|key| self.assign_target_shapes.contains_key(*key))
            .cloned()
            .collect();
        self.assign_target_kinds
            .retain(|key, _| valid_keys.contains(key));
        self.assign_target_shapes
            .retain(|key, _| valid_keys.contains(key));
    }

    pub(super) fn validate_stream_sink_element_type(
        &mut self,
        type_args: &[Ty],
        type_name: &str,
        method_name: &str,
        span: &Span,
    ) -> Option<Ty> {
        let _ = method_name;
        let inner = type_args
            .first()
            .cloned()
            .unwrap_or(Ty::Var(TypeVar::fresh()));
        // Unresolved type variables and error sentinels pass through so that
        // type inference can complete without generating a cascade of spurious
        // "not Wire" diagnostics on partially-inferred programs.
        if matches!(&inner, Ty::Var(_) | Ty::Error) {
            return Some(inner);
        }
        // A type is a valid Sink/Stream payload if and only if it implements
        // both the Encode and Decode marker traits (the "Wire capability").
        // implements_marker performs structural derivation — closures, raw
        // pointers, dyn-Trait, an actor handle, and other non-serialisable types
        // naturally fall out here without any explicit allowlist entry.
        let has_encode = self.registry.implements_marker(&inner, MarkerTrait::Encode);
        let has_decode = self.registry.implements_marker(&inner, MarkerTrait::Decode);
        if !has_encode || !has_decode {
            let mut missing = Vec::new();
            if !has_encode {
                missing.push("Encode".to_owned());
            }
            if !has_decode {
                missing.push("Decode".to_owned());
            }
            self.report_error(
                TypeErrorKind::SinkPayloadNotWire {
                    payload_ty: inner.user_facing().to_string(),
                    missing_traits: missing,
                },
                span,
                format!(
                    "`{type_name}<{}>` payload must implement Wire (Encode + Decode); \
                     the type does not satisfy the required marker traits",
                    inner.user_facing()
                ),
            );
            return None;
        }
        Some(inner)
    }

    pub(super) fn report_unlowerable_stream_codec_boundary(
        &mut self,
        type_name: &str,
        inner: &Ty,
        method: &str,
        span: &Span,
    ) -> Ty {
        self.report_error(
            TypeErrorKind::InvalidOperation,
            span,
            format!(
                "`{method}()` is not available on `{type_name}<{}>` yet; lowering/runtime support is not implemented",
                inner.user_facing()
            ),
        );
        Ty::Error
    }

    /// Concrete key operations come from the exact semantic type and selected
    /// impl bounds. A bare template parameter is governed by its declared bound.
    pub(super) fn collection_key_marker_available(&self, ty: &Ty, marker: MarkerTrait) -> bool {
        let capability = match marker {
            MarkerTrait::Hash => crate::ValueCapability::Hash,
            MarkerTrait::Eq => crate::ValueCapability::Eq,
            _ => return self.registry.implements_marker(ty, marker),
        };
        if let Ty::Named {
            name,
            args,
            builtin: None,
        } = ty
        {
            if args.is_empty() && self.is_type_param_in_scope(name) {
                return self.type_param_has_marker_bound(name, marker);
            }
        }
        let ty = self.subst.resolve(ty).materialize_literal_defaults();
        let Ok(resolved) =
            crate::ResolvedTy::from_ty_with_type_params(&ty, &self.current_type_param_names())
        else {
            return false;
        };
        crate::TypeFactService::new(self.type_fact_context(), BTreeMap::new())
            .capability_plan(&resolved, capability)
            .is_ok_and(|selection| selection.is_some())
    }

    pub(super) fn validate_collection_key_capabilities(
        &mut self,
        ty: &Ty,
        collection: &str,
        span: &Span,
    ) -> bool {
        let mut missing = Vec::new();
        for marker in [MarkerTrait::Hash, MarkerTrait::Eq] {
            if !self.collection_key_marker_available(ty, marker) {
                missing.push(marker.to_string());
            }
        }
        if missing.is_empty() {
            return true;
        }
        if !self.has_bounds_not_satisfied_at(span) {
            self.report_error(
                TypeErrorKind::BoundsNotSatisfied,
                span,
                format!(
                    "`{}` does not satisfy the required bounds for `{collection}` ({})",
                    ty.user_facing(),
                    missing.join(" + ")
                ),
            );
        }
        false
    }

    /// Opaque declarations are nominal. Imported uses must carry the exact
    /// owner, so a same-leaf foreign type cannot inherit opacity.
    fn is_user_opaque_type_name(&self, name: &str) -> bool {
        self.user_opaque_type_names.contains(name)
    }

    /// Tuple-record payloads deliberately leave `TypeDef::fields` empty
    /// because `.0`/`.1` access is not exposed. Their constructor signature is
    /// nevertheless the authoritative positional layout.
    pub(super) fn tuple_record_constructor_fields(
        &self,
        name: &str,
        type_def: &TypeDef,
    ) -> Vec<Ty> {
        if !matches!(type_def.kind, TypeDefKind::Struct | TypeDefKind::Record)
            || !type_def.fields.is_empty()
            || self.registry.is_resource(name)
        {
            return Vec::new();
        }
        self.fn_sigs
            .get(name)
            .and_then(|sig| {
                let Ty::Named {
                    name: return_name, ..
                } = &sig.return_type
                else {
                    return None;
                };
                (return_name == name).then(|| sig.params.clone())
            })
            .unwrap_or_default()
    }

    /// The §1.1 value class and clone kind of a collection element type.
    ///
    /// This is the one authority behind every element question the checker
    /// asks: whether a type may be a `Vec`, `HashMap`, `HashSet` or array
    /// element, whether it can be copied out of one, and whether the element
    /// carries an ownership obligation the collection has to release. It is
    /// the same rule SIR reads for ownership and physical MIR reads for the
    /// clone and destroy actions, so the checker and the backend cannot
    /// disagree about one element type.
    ///
    /// # Errors
    ///
    /// Returns the class rule's own refusal. A type the boundary cannot render
    /// as a [`ResolvedTy`] has no class either, and reports as
    /// [`ClassError::UnknownDeclaration`] on its spelling.
    pub(super) fn element_value_facts(
        &self,
        ty: &Ty,
    ) -> Result<(ValueClass, CloneKind), ClassError> {
        let resolved = self.subst.resolve(ty).materialize_literal_defaults();
        // The enclosing item's own parameters render as abstract parameters
        // rather than as user nominals, so an in-scope `T` reaches the class
        // rule as the parameter it is and refuses with `TypeParam` instead of
        // as a declaration nobody wrote.
        let rendered =
            ResolvedTy::from_ty_with_type_params(&resolved, &self.current_type_param_names())
                .map_err(|_| ClassError::UnknownDeclaration {
                    name: resolved.user_facing().to_string(),
                })?;
        crate::value_class::classify_ty(
            &rendered,
            &crate::value_class::ClassContext::new(&self.class_declarations()),
        )
    }

    /// Why this type cannot be a collection element at all, if it cannot.
    ///
    /// A type with a class is storable: `BitCopy` rides the plain layout
    /// family and every other class the owned-element descriptor, whose clone
    /// and destroy actions come from the same class row. So the only refusals
    /// left are the class rule's own: a compiler-internal name that is never
    /// the type of a value, a callable whose declared copy capability
    /// contradicts what it captures, a declaration whose members reach it at a
    /// growing instantiation, and a spelling with no declaration behind it. An
    /// abstract parameter is substituted before the element ABI is chosen, so
    /// it refuses nothing here.
    pub(super) fn element_admission_refusal(&self, ty: &Ty) -> Option<(TypeErrorKind, String)> {
        match self.element_value_facts(ty) {
            Ok(_) | Err(ClassError::TypeParam { .. }) => None,
            // A declaration with no finite member walk is the class rule's own
            // limit, and it keeps that kind wherever it surfaces so one
            // declaration produces one named refusal.
            Err(error @ ClassError::RecursiveInstantiation { .. }) => Some((
                TypeErrorKind::ClassRecursion,
                format!("E_LIMIT_CLASS_RECURSION: {error}"),
            )),
            Err(error) => Some((TypeErrorKind::InvalidOperation, error.to_string())),
        }
    }

    /// Does this element carry an ownership obligation the collection must
    /// release, so its slots ride the owned-element descriptor ABI?
    ///
    /// A `BitCopy` element is bits in the buffer and a `View` borrows storage
    /// it does not own; every other class owns something. A class the rule
    /// refuses owns nothing this collection can be asked to release, and the
    /// refusal itself is reported by the admission site.
    pub(super) fn element_owns_heap(&self, ty: &Ty) -> bool {
        matches!(
            self.element_value_facts(ty),
            Ok((class, _)) if !matches!(class, ValueClass::BitCopy | ValueClass::View)
        )
    }

    /// Why this element type cannot be copied out of a collection, if it
    /// cannot.
    ///
    /// One authority: the element's §1.1 clone kind
    /// ([`crate::value_class::classify_ty`]). `xs[i]`, a range slice, a
    /// `HashMap` value read, cloning iteration and `Vec.clone` all copy an
    /// element into an independent owner, so they admit exactly the element
    /// types the class table gives a copy path. An abstract parameter has no
    /// class until the instance service substitutes it and MIR's
    /// per-monomorphisation clone check answers there, so it blocks nothing
    /// here.
    pub(super) fn element_clone_blocker(&self, ty: &Ty) -> Option<String> {
        match self.element_value_facts(ty) {
            Ok((class, CloneKind::None)) => Some(format!(
                "`{}` ({})",
                self.subst
                    .resolve(ty)
                    .materialize_literal_defaults()
                    .user_facing(),
                class_description(class)
            )),
            Ok(_) | Err(ClassError::TypeParam { .. }) => None,
            Err(error) => Some(error.to_string()),
        }
    }

    /// How a `VecIter<T>` cursor produces each element.
    ///
    /// An element with a semantic clone is copied out per step, leaving the
    /// vector whole. An element without one — a `#[resource]` or `#[linear]`
    /// type, an opaque handle, a channel half, a generator, a trait object, an
    /// unbounded type parameter — is moved out instead, so `into_iter()` drains
    /// the vector and it ends empty. The cursor owns the vector, so an early
    /// exit releases whatever the drain did not reach.
    ///
    /// `None` means the element is not iterable at all and a diagnostic was
    /// reported.
    pub(super) fn vec_iter_element_mode(&mut self, ty: &Ty, span: &Span) -> Option<VecCursorMode> {
        let resolved = self.subst.resolve(ty).materialize_literal_defaults();
        if matches!(resolved, Ty::Error) {
            return None;
        }
        if self.clone_proven_element(&resolved) && self.element_clone_blocker(&resolved).is_none() {
            return Some(VecCursorMode::Clone);
        }
        let _ = span;
        Some(VecCursorMode::Take)
    }

    /// Record `span` as a cursor site that moves each element out, and report
    /// whether the cursor is admitted at all.
    pub(super) fn record_vec_iter_element_mode(&mut self, ty: &Ty, span: &Span) -> bool {
        match self.vec_iter_element_mode(ty, span) {
            Some(VecCursorMode::Take) => {
                self.owning_take_vec_cursors
                    .insert(SpanKey::in_module(span, self.current_module_idx));
                true
            }
            Some(VecCursorMode::Clone) => true,
            None => false,
        }
    }

    /// How `for value in vec` binds each element (D432).
    ///
    /// An element with a semantic clone is copied out per iteration, which is
    /// what every cursor form does today. An element without one — a
    /// `#[resource]` or `#[linear]` type, an opaque handle, a channel half, a
    /// generator — is bound as a loan of the slot the vector still owns: the
    /// body may read it and call its borrowing methods, and the owning removal
    /// is the way to move it out. A trait object stays refused here; its
    /// consuming iterator is the trait-objects surface.
    ///
    /// Inside a generic template the element is not a concrete layout yet, so
    /// the copy mode must be *proven from the bound*: an unbounded parameter is
    /// a resource at some monomorphisation, and the borrowed form is the one
    /// that is sound at every instantiation.
    ///
    /// `None` means the element is not iterable at all and a diagnostic was
    /// reported.
    pub(super) fn vec_iteration_element_mode(
        &mut self,
        ty: &Ty,
        span: &Span,
    ) -> Option<VecIterationMode> {
        let resolved = self.subst.resolve(ty).materialize_literal_defaults();
        if matches!(resolved, Ty::Error) {
            return None;
        }
        if self.clone_proven_element(&resolved) && self.element_clone_blocker(&resolved).is_none() {
            return Some(VecIterationMode::Clone);
        }
        let _ = span;
        Some(VecIterationMode::Borrow)
    }

    /// Whether every type-parameter occurrence in `ty` carries a `Clone` bound.
    ///
    /// [`Self::element_clone_blocker`] deliberately admits an unbounded
    /// parameter and defers it to MIR's per-monomorphisation clone check; that
    /// is right for admitting call sites and wrong for choosing a copy mode,
    /// because the template's loop shape is fixed before its instantiations are
    /// known. A parameter with no `Clone` bound therefore reads as clone-free
    /// and iterates by borrow at every monomorphisation.
    fn clone_proven_element(&self, ty: &Ty) -> bool {
        match ty {
            Ty::Named { name, args, .. } => {
                if self.is_type_param_in_scope(name)
                    && !self.type_param_has_marker_bound(name, MarkerTrait::Clone)
                {
                    return false;
                }
                args.iter().all(|arg| self.clone_proven_element(arg))
            }
            Ty::Tuple(items) => items.iter().all(|item| self.clone_proven_element(item)),
            Ty::Array(elem, _) | Ty::Slice(elem) => self.clone_proven_element(elem),
            _ => true,
        }
    }

    /// Checker boundary for `xs[a..b]` over `Vec<T>`.
    ///
    /// A Vec range slice is an independent `Vec<T>`: every selected element is
    /// copied into the result. An element with no clone therefore has no slice,
    /// and the consuming drain is the operation that moves elements out.
    pub(super) fn validate_vec_slice_element_clone_type(&mut self, ty: &Ty, span: &Span) -> bool {
        let resolved = self.subst.resolve(ty).materialize_literal_defaults();
        if matches!(resolved, Ty::Error) {
            return false;
        }
        let Some(blocker) = self.element_clone_blocker(&resolved) else {
            return true;
        };
        self.report_error(
            TypeErrorKind::InvalidOperation,
            span,
            format!(
                "E_ELEMENT_NO_COPY: `Vec<{}>` cannot be range-sliced: a slice copies each \
                 element into an independent `Vec`, but {blocker} has no copy operation; use \
                 an owning removal such as `pop()` to move the elements out instead",
                resolved.user_facing()
            ),
        );
        false
    }

    /// Checker boundary for `xs.get(i)` over `Vec<T>`.
    ///
    /// `get` reads a copy of the element out of the vector; the vector keeps
    /// its own. A concrete element with no copy operation therefore has no
    /// `get`, exactly as it has no `xs[i]` and no range slice: reading one out
    /// would hand the caller a second owner of a single-owner value. An
    /// unbounded type parameter is not concrete and keeps the borrowed read
    /// every instantiation of a generic body shares (spec §3.8.1).
    pub(super) fn validate_vec_get_element_clone_type(&mut self, ty: &Ty, span: &Span) -> bool {
        let resolved = self.subst.resolve(ty).materialize_literal_defaults();
        if matches!(resolved, Ty::Error) {
            return false;
        }
        let Some(blocker) = self.element_clone_blocker(&resolved) else {
            return true;
        };
        self.report_error(
            TypeErrorKind::InvalidOperation,
            span,
            format!(
                "E_ELEMENT_NO_COPY: `Vec<{}>` cannot be read with `get`: `get` copies the \
                 element out and leaves the vector's own in place, but {blocker} has no copy \
                 operation; use an owning removal such as `pop()` or `remove(i)`, or consuming \
                 iteration, to move the element out instead",
                resolved.user_facing()
            ),
        );
        false
    }

    /// Checker boundary for a `HashMap` operation that copies its values out:
    /// `m[k]`, `values()`, `entries()`, `clone()`, `into_iter()` and the
    /// `for (k, v) in m` desugar. A value with no clone stays in the map; the
    /// borrowed `get` reads it and the owning `remove` moves it out.
    pub(super) fn validate_hashmap_value_clone_type(
        &mut self,
        ty: &Ty,
        operation: &str,
        span: &Span,
    ) -> bool {
        let resolved = self.subst.resolve(ty);
        if matches!(resolved, Ty::Error) {
            return false;
        }
        // Inference is still in flight here; the obligation is checked once the
        // value type has settled.
        if resolved.has_inference_var() {
            self.deferred_hashmap_value_copy
                .entry(SpanKey::in_module(span, self.current_module_idx))
                .or_insert_with(|| super::types::DeferredHashMapValueCopy {
                    span: span.clone(),
                    val_ty: ty.clone(),
                    operation: operation.to_string(),
                    source_module: self.current_module.clone(),
                });
            return true;
        }
        if let Some(blocker) = self.element_clone_blocker(ty) {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!(
                    "E_ELEMENT_NO_COPY: `{operation}` copies each value out of the map, but \
                     the value type {blocker} has no copy operation; read it with `get(k)`, \
                     which borrows, or move it out with `remove(k)`"
                ),
            );
            return false;
        }
        true
    }

    pub(super) fn validate_hashmap_key_value_types(
        &mut self,
        key_ty: &Ty,
        val_ty: &Ty,
        span: &Span,
    ) -> bool {
        let resolved_key = self.subst.resolve(key_ty);
        let resolved_val = self.subst.resolve(val_ty);

        // Ty::Error: upstream already emitted a diagnostic; fail closed silently
        // to prevent cascading errors from admission logic.
        if matches!(resolved_key, Ty::Error) || matches!(resolved_val, Ty::Error) {
            return false;
        }

        // Registration sees legal forward references before their declarations
        // exist. Keep this obligation in the existing inference queue and check
        // it once the complete declaration graph and substitution are available.
        let type_param_bounds = self.current_type_param_bounds_map();
        self.deferred_hashmap_admission
            .entry(SpanKey::in_module(span, self.current_module_idx))
            .and_modify(|check| {
                for (name, bounds) in &type_param_bounds {
                    check
                        .type_param_bounds
                        .entry(name.clone())
                        .or_insert_with(|| bounds.clone());
                }
            })
            .or_insert_with(|| DeferredHashMapAdmission {
                span: span.clone(),
                key_ty: resolved_key.clone(),
                val_ty: resolved_val.clone(),
                source_module: self.current_module.clone(),
                type_param_bounds,
            });
        if !self.type_decls_registered
            || resolved_key.has_inference_var()
            || resolved_val.has_inference_var()
        {
            return true;
        }
        // Named keys wait until all impls are registered. Concrete operation
        // sites additionally prove the same capabilities through the resolver.
        if !self.validate_hashmap_value_shape(&resolved_val, span) {
            return false;
        }
        matches!(&resolved_key, Ty::Named { .. })
            || self.validate_collection_key_capabilities(&resolved_key, "Map", span)
    }

    /// A callable map value has no working ingress: its checked type carries a
    /// copy capability the map's value descriptor cannot name, so the runtime
    /// boundary would disagree with the declared value type. Refuse the shape
    /// here rather than at that boundary.
    fn validate_hashmap_value_shape(&mut self, val_ty: &Ty, span: &Span) -> bool {
        if !matches!(val_ty, Ty::Function { .. } | Ty::Closure { .. }) {
            return true;
        }
        let message = format!(
            "`HashMap<_, {}>` is not supported: a callable value has no map ingress; \
             store it in a record field or a `Vec` instead",
            val_ty.user_facing()
        );
        // The same annotation is admitted from both the declaration and the
        // operation that reads it; one diagnostic answers both.
        if !self.errors.iter().any(|error| error.message == message) {
            self.report_error(TypeErrorKind::InvalidOperation, span, message);
        }
        false
    }

    pub(super) fn validate_hashmap_owned_element_types(
        &mut self,
        key_ty: &Ty,
        val_ty: &Ty,
        span: &Span,
    ) -> bool {
        self.validate_hashmap_key_value_types(key_ty, val_ty, span)
    }

    pub(super) fn validate_hashset_element_type(&mut self, elem_ty: &Ty, span: &Span) -> bool {
        let resolved = self.subst.resolve(elem_ty);

        // Ty::Error: upstream already emitted a diagnostic; fail closed silently
        // to prevent cascading errors from admission logic.
        if matches!(resolved, Ty::Error) {
            return false;
        }

        if matches!(&resolved, Ty::Named { name, args, builtin: None }
            if args.is_empty() && self.is_type_param_in_scope(name))
        {
            return self.validate_collection_key_capabilities(&resolved, "Set", span);
        }

        // Ty::Var: inference is still in-flight at this call site.  Defer the
        // admission check until finalize_hashset_admission() runs after all
        // inference has settled, mirroring the HashMap deferred-admission pattern.
        if matches!(resolved, Ty::Var(_)) {
            self.deferred_hashset_admission
                .entry(SpanKey::in_module(span, self.current_module_idx))
                .or_insert_with(|| DeferredHashSetAdmission {
                    span: span.clone(),
                    elem_ty: elem_ty.clone(),
                    source_module: self.current_module.clone(),
                });
            return true; // optimistically admit; finalization will fail closed
        }

        if matches!(&resolved, Ty::Named { .. }) || !self.type_decls_registered {
            self.deferred_hashset_admission
                .entry(SpanKey::in_module(span, self.current_module_idx))
                .or_insert_with(|| DeferredHashSetAdmission {
                    span: span.clone(),
                    elem_ty: resolved,
                    source_module: self.current_module.clone(),
                });
            return true;
        }
        self.validate_collection_key_capabilities(&resolved, "Set", span)
    }

    /// Returns true if a `BoundsNotSatisfied` diagnostic has already been
    /// recorded at the exact span. Used by the collection-admissibility
    /// fail-closed paths to suppress duplicate emissions when the same
    /// type annotation is validated via multiple call sites (e.g. once
    /// from `validate_concrete_hashmap_type` and again from the
    /// right-hand-side expression's inferred-type validation).
    fn has_bounds_not_satisfied_at(&self, span: &Span) -> bool {
        let key = SpanKey::in_module(span, self.current_module_idx);
        self.errors.iter().any(|e| {
            matches!(e.kind, TypeErrorKind::BoundsNotSatisfied)
                && SpanKey::in_module(&e.span, self.current_module_idx) == key
        })
    }

    pub(super) fn validate_hashset_owned_element_type(
        &mut self,
        elem_ty: &Ty,
        span: &Span,
    ) -> bool {
        self.validate_hashset_element_type(elem_ty, span)
    }

    pub(super) fn instantiate_type_def_member(
        ty: &Ty,
        type_params: &[String],
        type_args: &[Ty],
    ) -> Ty {
        let map: HashMap<String, Ty> = type_params
            .iter()
            .zip(type_args.iter())
            .map(|(p, a)| (p.clone(), a.clone()))
            .collect();
        ty.substitute_named_params_parallel(&map)
    }

    pub(super) fn vec_element_has_copy_layout(&self, elem_ty: &Ty) -> bool {
        // Sender is pointer-width but not semantically Copy: duplicating an
        // endpoint must call `hew_channel_sender_clone` so its shared channel
        // refcount is retained. Keep it on the owned descriptor lane even if
        // the representation marker reports a flat pointer layout.
        if matches!(
            elem_ty,
            Ty::Named {
                builtin: Some(BuiltinType::Sender),
                ..
            }
        ) {
            return false;
        }
        self.registry.implements_marker(elem_ty, MarkerTrait::Copy)
            || primitive_copy_layout(elem_ty, &self.type_defs).is_some()
    }

    /// True when a Vec element type transitively carries a function/closure
    /// value INSIDE a composite (record field, enum variant payload, tuple
    /// member, Option/Result/Range argument). A direct `Vec<fn(...)>` element
    /// is the supported boxed-pair class and is NOT flagged here — the caller
    /// exempts the top-level Function/Closure shape. Composite elements ride
    /// the layout/owned byte-copy ABIs, which would shallow-copy the embedded
    /// pair and alias its sole-owner environment box, so they fail closed at
    /// admission.
    pub(super) fn vec_element_contains_fn_value(
        &self,
        ty: &Ty,
        visiting: &mut HashSet<String>,
    ) -> bool {
        let resolved = self.subst.resolve(ty);
        match &resolved {
            Ty::Function { .. } | Ty::Closure { .. } => true,
            Ty::Tuple(elems) => elems
                .iter()
                .any(|elem| self.vec_element_contains_fn_value(elem, visiting)),
            Ty::Array(inner, _) | Ty::Slice(inner) => {
                self.vec_element_contains_fn_value(inner, visiting)
            }
            Ty::Named {
                builtin: Some(BuiltinType::Range | BuiltinType::Option | BuiltinType::Result),
                args,
                ..
            } => args
                .iter()
                .any(|arg| self.vec_element_contains_fn_value(arg, visiting)),
            Ty::Named { name, args, .. } => {
                let Some(type_def) = self.lookup_type_def(name) else {
                    return false;
                };
                if visiting.contains(type_def.name.as_str()) {
                    return false;
                }
                visiting.insert(type_def.name.clone());
                let result = type_def.fields.values().any(|field_ty| {
                    let field_ty =
                        Self::instantiate_type_def_member(field_ty, &type_def.type_params, args);
                    self.vec_element_contains_fn_value(&field_ty, visiting)
                }) || type_def.variants.values().any(|variant| match variant {
                    VariantDef::Unit => false,
                    VariantDef::Tuple(tys) => tys.iter().any(|ty| {
                        let ty = Self::instantiate_type_def_member(ty, &type_def.type_params, args);
                        self.vec_element_contains_fn_value(&ty, visiting)
                    }),
                    VariantDef::Struct(fields) => fields.iter().any(|(_, ty)| {
                        let ty = Self::instantiate_type_def_member(ty, &type_def.type_params, args);
                        self.vec_element_contains_fn_value(&ty, visiting)
                    }),
                });
                visiting.remove(type_def.name.as_str());
                result
            }
            _ => false,
        }
    }

    pub(super) fn validate_resolved_vec_element_type(
        &mut self,
        resolved: &Ty,
        span: &Span,
    ) -> bool {
        if !self.validate_concrete_collection_types(resolved, span) {
            return false;
        }

        // Composite elements embedding a function value fail closed: the
        // layout/owned element ABIs byte-copy the element, which would
        // shallow-copy the closure pair and alias its sole-owner environment.
        // A direct Vec<fn(...)> element is the supported boxed-pair class.
        if !matches!(resolved, Ty::Function { .. } | Ty::Closure { .. }) {
            let mut visiting = HashSet::new();
            if self.vec_element_contains_fn_value(resolved, &mut visiting) {
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    format!(
                        "`Vec<{}>` is not supported: the element type contains a function \
                         value, and each closure environment has a sole owner — store \
                         the functions directly in a Vec<fn(...)> instead",
                        resolved.user_facing()
                    ),
                );
                return false;
            }
        }

        true
    }

    pub(super) fn validate_vec_element_type(&mut self, elem_ty: &Ty, span: &Span) -> bool {
        let resolved = self.subst.resolve(elem_ty);
        if resolved.contains_error() {
            return false;
        }

        if resolved.has_inference_var() {
            self.deferred_vec_admission
                .entry(SpanKey::in_module(span, self.current_module_idx))
                .or_insert_with(|| DeferredVecAdmission {
                    span: span.clone(),
                    elem_ty: elem_ty.clone(),
                    source_module: self.current_module.clone(),
                });
            return true;
        }

        self.validate_resolved_vec_element_type(&resolved, span)
    }

    fn validate_concrete_collection_type(
        &mut self,
        ty: &Ty,
        span: &Span,
        collection: ConcreteCollectionKind,
    ) -> bool {
        let resolved = self.subst.resolve(ty);
        match &resolved {
            Ty::Named { builtin, args, .. } => {
                if let Some(result) =
                    collection.validate_named_collection(self, *builtin, args, span)
                {
                    return result;
                }
                args.iter()
                    .all(|arg| self.validate_concrete_collection_type(arg, span, collection))
            }
            Ty::Tuple(elems) => elems
                .iter()
                .all(|elem| self.validate_concrete_collection_type(elem, span, collection)),
            Ty::Array(elem, _) | Ty::Slice(elem) => {
                self.validate_concrete_collection_type(elem, span, collection)
            }
            Ty::Function { params, ret, .. } => {
                params
                    .iter()
                    .all(|param| self.validate_concrete_collection_type(param, span, collection))
                    && self.validate_concrete_collection_type(ret, span, collection)
            }
            Ty::Closure {
                params,
                ret,
                captures,
                ..
            } => {
                params
                    .iter()
                    .all(|param| self.validate_concrete_collection_type(param, span, collection))
                    && self.validate_concrete_collection_type(ret, span, collection)
                    && captures.iter().all(|capture| {
                        self.validate_concrete_collection_type(capture, span, collection)
                    })
            }
            Ty::Pointer { pointee, .. } => {
                self.validate_concrete_collection_type(pointee, span, collection)
            }
            Ty::TraitObject { traits } => traits.iter().all(|bound| {
                bound
                    .args
                    .iter()
                    .all(|arg| self.validate_concrete_collection_type(arg, span, collection))
            }),
            _ => true,
        }
    }

    pub(super) fn validate_concrete_vec_type(&mut self, ty: &Ty, span: &Span) -> bool {
        self.validate_concrete_collection_type(ty, span, ConcreteCollectionKind::Vec)
    }

    pub(super) fn validate_concrete_hashset_type(&mut self, ty: &Ty, span: &Span) -> bool {
        self.validate_concrete_collection_type(ty, span, ConcreteCollectionKind::HashSet)
    }

    pub(super) fn validate_concrete_collection_types(&mut self, ty: &Ty, span: &Span) -> bool {
        let hashmap_ok = self.validate_concrete_hashmap_type(ty, span);
        let hashset_ok = self.validate_concrete_hashset_type(ty, span);
        let vec_ok = self.validate_concrete_vec_type(ty, span);
        hashmap_ok && hashset_ok && vec_ok
    }

    pub(super) fn make_vec_type(&mut self, elem_ty: Ty, span: &Span) -> Ty {
        let ty = Ty::Named {
            builtin: Some(BuiltinType::Vec),
            name: "Vec".to_string(),
            args: vec![elem_ty],
        };
        self.validate_concrete_vec_type(&ty, span);
        ty
    }

    pub(super) fn validate_concrete_hashmap_type(&mut self, ty: &Ty, span: &Span) -> bool {
        self.validate_concrete_collection_type(ty, span, ConcreteCollectionKind::HashMap)
    }

    #[allow(
        clippy::too_many_lines,
        reason = "the closed type-shape match keeps aggregate admission auditable in one place"
    )]
    fn rc_payload_clone_drop_supported(
        &self,
        ty: &Ty,
        visiting: &mut HashSet<String>,
        through_collection: bool,
    ) -> bool {
        let resolved = self.subst.resolve(ty).materialize_literal_defaults();
        match &resolved {
            Ty::Error | Ty::String | Ty::Bytes => true,
            Ty::Var(_)
            | Ty::AssocType { .. }
            | Ty::Slice(_)
            | Ty::Function { .. }
            | Ty::Closure { .. }
            | Ty::TraitObject { .. }
            | Ty::CancellationToken
            | Ty::Task(_)
            | Ty::Borrow { .. }
            | Ty::Pointer { .. } => false,
            Ty::Tuple(elems) => elems
                .iter()
                .all(|elem| self.rc_payload_clone_drop_supported(elem, visiting, false)),
            Ty::Array(elem, _) => self.rc_payload_clone_drop_supported(elem, visiting, false),
            Ty::Named {
                builtin: Some(BuiltinType::Rc | BuiltinType::Weak),
                args,
                ..
            } => args.len() == 1,
            Ty::Named {
                name,
                args,
                builtin: Some(builtin),
            } => match builtin {
                BuiltinType::Option | BuiltinType::Result => args
                    .iter()
                    .all(|arg| self.rc_payload_clone_drop_supported(arg, visiting, false)),
                BuiltinType::Vec | BuiltinType::HashSet => args.first().is_some_and(|arg| {
                    args.len() == 1 && self.rc_payload_clone_drop_supported(arg, visiting, true)
                }),
                BuiltinType::HashMap => {
                    args.len() == 2
                        && self.rc_payload_clone_drop_supported(&args[0], visiting, false)
                        && self.rc_payload_clone_drop_supported(&args[1], visiting, true)
                }
                _ => {
                    self.canonical_owned_handle_type_name(name).is_none()
                        && !self.is_user_opaque_type_name(name)
                        && self
                            .registry
                            .implements_marker(&resolved, MarkerTrait::Copy)
                }
            },
            Ty::Named {
                name,
                args,
                builtin: None,
            } => {
                if self.canonical_owned_handle_type_name(name).is_some()
                    || self.is_user_opaque_type_name(name)
                    || self.registry.is_linear(name)
                {
                    return false;
                }
                // A `#[resource]` releases through its own `close`, which the
                // shared allocation installs as the payload destructor. Its
                // fields belong to that close, not to this walk. `#[linear]`
                // stays refused: a shared handle can outlive every path that
                // would consume it.
                if self.registry.is_resource(name) {
                    return true;
                }
                let Some(type_def) = self.lookup_type_def(name) else {
                    return self
                        .registry
                        .implements_marker(&resolved, MarkerTrait::Copy);
                };
                if !matches!(
                    type_def.kind,
                    TypeDefKind::Record | TypeDefKind::Struct | TypeDefKind::Enum
                ) {
                    return false;
                }
                let visit_key = type_def.name.clone();
                if !visiting.insert(visit_key.clone()) {
                    return through_collection;
                }
                let fields_ok = type_def.fields.values().all(|field| {
                    let field =
                        Self::instantiate_type_def_member(field, &type_def.type_params, args);
                    self.rc_payload_clone_drop_supported(&field, visiting, false)
                });
                let variants_ok = fields_ok
                    && type_def.variants.values().all(|variant| match variant {
                        VariantDef::Unit => true,
                        VariantDef::Tuple(fields) => fields.iter().all(|field| {
                            let field = Self::instantiate_type_def_member(
                                field,
                                &type_def.type_params,
                                args,
                            );
                            self.rc_payload_clone_drop_supported(&field, visiting, false)
                        }),
                        VariantDef::Struct(fields) => fields.iter().all(|(_, field)| {
                            let field = Self::instantiate_type_def_member(
                                field,
                                &type_def.type_params,
                                args,
                            );
                            self.rc_payload_clone_drop_supported(&field, visiting, false)
                        }),
                    });
                visiting.remove(&visit_key);
                variants_ok
            }
            _ => self
                .registry
                .implements_marker(&resolved, MarkerTrait::Copy),
        }
    }

    pub(super) fn validate_rc_payload_type(&mut self, ty: &Ty, span: &Span) -> bool {
        let resolved = self.subst.resolve(ty);
        let unresolved_generic = matches!(&resolved, Ty::Var(_))
            || matches!(
                &resolved,
                Ty::Named {
                    name,
                    args,
                    builtin: None,
                } if args.is_empty() && self.is_type_param_in_scope(name)
            );
        if unresolved_generic {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!(
                    "`Rc<{}>` is not currently supported; Rc only accepts Copy payloads \
                     or concrete payloads with a proven aggregate clone/drop strategy; \
                     unresolved generic payloads fail closed",
                    resolved.user_facing()
                ),
            );
            return false;
        }
        if self.rc_payload_clone_drop_supported(&resolved, &mut HashSet::new(), false) {
            return true;
        }

        self.report_error(
            TypeErrorKind::InvalidOperation,
            span,
            format!(
                "`Rc<{}>` is not supported because its payload has no complete \
                 aggregate clone/drop strategy; use Copy values, strings, bytes, \
                 Rc/Weak handles, supported collections, tuples, arrays, records, \
                 or enums, and move opaque or affine resources outside the payload",
                resolved.user_facing()
            ),
        );
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::module_registry::ModuleRegistry;

    #[test]
    fn ty_contains_error_recurses_through_named_and_closure_types() {
        let ty = Ty::Closure {
            identity: crate::ty::EffectBody::Closure(crate::check::SpanKey {
                start: 0,
                end: 0,
                module_idx: 0,
            }),
            capabilities: crate::CallableCapabilities::default(),
            params: vec![Ty::normalize_named(
                "Result".to_string(),
                vec![Ty::I32, Ty::Tuple(vec![Ty::Error])],
            )],
            ret: Box::new(Ty::Bool),
            captures: vec![],
        };

        assert!(ty_contains_error(&ty));
    }

    #[test]
    fn signature_contains_error_type_flags_error_anywhere_in_signature() {
        let params = vec![Ty::I32];
        let ret = Ty::Function {
            capabilities: crate::CallableCapabilities::default(),
            params: vec![Ty::Tuple(vec![Ty::Error])],
            ret: Box::new(Ty::Bool),
        };

        assert!(signature_contains_error_type(&params, &ret));
        assert!(!signature_contains_error_type(&[Ty::I32], &Ty::Bool));
    }

    /// Regression guard for issue #789: `validate_checker_output_contract` must
    /// remove `fn_sigs` entries whose parameter or return types contain
    /// `Ty::Error` so they cannot propagate into serialization/codegen.
    #[test]
    fn validate_checker_output_contract_prunes_fn_sigs_with_error_type() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));

        let mut fn_sigs = HashMap::from([
            (
                "good_fn".to_string(),
                FnSig {
                    params: vec![Ty::I32],
                    return_type: Ty::Bool,
                    ..FnSig::default()
                },
            ),
            (
                "error_param_fn".to_string(),
                FnSig {
                    params: vec![Ty::Error],
                    return_type: Ty::I32,
                    ..FnSig::default()
                },
            ),
            (
                "error_return_fn".to_string(),
                FnSig {
                    params: vec![Ty::I32],
                    return_type: Ty::Error,
                    ..FnSig::default()
                },
            ),
        ]);

        let mut expr_types = HashMap::new();
        let mut type_defs = HashMap::new();
        let mut call_type_args = HashMap::new();
        let mut record_init_type_args = HashMap::new();
        checker.validate_checker_output_contract(
            &mut expr_types,
            &mut type_defs,
            &mut fn_sigs,
            &mut call_type_args,
            &mut record_init_type_args,
        );

        assert!(
            fn_sigs.contains_key("good_fn"),
            "clean signature must survive the contract check"
        );
        assert!(
            !fn_sigs.contains_key("error_param_fn"),
            "signature with Ty::Error in params must be pruned"
        );
        assert!(
            !fn_sigs.contains_key("error_return_fn"),
            "signature with Ty::Error as return type must be pruned"
        );
    }

    #[test]
    fn validate_checker_output_contract_prunes_unresolved_channel_signature_elements() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let normalized_param_var = TypeVar::fresh();
        let normalized_return_var = TypeVar::fresh();
        let leaked_param_var = TypeVar::fresh();
        let leaked_return_var = TypeVar::fresh();

        let mut fn_sigs = HashMap::from([
            (
                "good_fn".to_string(),
                FnSig {
                    params: vec![Ty::I32],
                    return_type: Ty::Bool,
                    ..FnSig::default()
                },
            ),
            (
                "normalized_param_fn".to_string(),
                FnSig {
                    params: vec![Ty::normalize_named(
                        "Sender".to_string(),
                        vec![Ty::Var(normalized_param_var)],
                    )],
                    return_type: Ty::Unit,
                    ..FnSig::default()
                },
            ),
            (
                "normalized_return_fn".to_string(),
                FnSig {
                    params: vec![Ty::I32],
                    return_type: Ty::normalize_named(
                        "Receiver".to_string(),
                        vec![Ty::Var(normalized_return_var)],
                    ),
                    ..FnSig::default()
                },
            ),
            (
                "leaked_param_fn".to_string(),
                FnSig {
                    params: vec![Ty::Tuple(vec![Ty::Var(leaked_param_var)])],
                    return_type: Ty::Unit,
                    ..FnSig::default()
                },
            ),
            (
                "leaked_return_fn".to_string(),
                FnSig {
                    params: vec![Ty::I32],
                    return_type: Ty::option(Ty::Var(leaked_return_var)),
                    ..FnSig::default()
                },
            ),
        ]);

        let mut expr_types = HashMap::new();
        let mut type_defs = HashMap::new();
        let mut call_type_args = HashMap::new();
        let mut record_init_type_args = HashMap::new();
        checker.validate_checker_output_contract(
            &mut expr_types,
            &mut type_defs,
            &mut fn_sigs,
            &mut call_type_args,
            &mut record_init_type_args,
        );

        assert!(
            fn_sigs.contains_key("good_fn"),
            "clean signature must survive the contract check"
        );
        assert!(!fn_sigs.contains_key("normalized_param_fn"));
        assert!(!fn_sigs.contains_key("normalized_return_fn"));
        assert!(
            !fn_sigs.contains_key("leaked_param_fn"),
            "signature with a real untracked Ty::Var in params must be pruned"
        );
        assert!(
            !fn_sigs.contains_key("leaked_return_fn"),
            "signature with a real untracked Ty::Var in return type must be pruned"
        );
    }

    #[test]
    #[expect(
        clippy::too_many_lines,
        reason = "exercise channel handle fields, variants, and methods in one focused regression"
    )]
    fn validate_checker_output_contract_prunes_unresolved_channel_member_elements() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let normalized_field_var = TypeVar::fresh();
        let normalized_variant_var = TypeVar::fresh();
        let normalized_method_var = TypeVar::fresh();
        let leaked_field_var = TypeVar::fresh();
        let leaked_variant_var = TypeVar::fresh();

        let mut type_defs = HashMap::from([
            (
                "Good".to_string(),
                TypeDef {
                    kind: TypeDefKind::Struct,
                    name: "Good".to_string(),
                    type_params: vec![],
                    bounds: HashMap::new(),
                    fields: HashMap::from([("value".to_string(), Ty::I32)]),
                    variants: HashMap::new(),
                    methods: HashMap::new(),
                    doc_comment: None,
                    field_order: vec![],
                    is_indirect: false,
                },
            ),
            (
                "NormalizedHandles".to_string(),
                TypeDef {
                    kind: TypeDefKind::Struct,
                    name: "NormalizedHandles".to_string(),
                    type_params: vec![],
                    bounds: HashMap::new(),
                    fields: HashMap::from([(
                        "tx".to_string(),
                        Ty::normalize_named(
                            "Sender".to_string(),
                            vec![Ty::Var(normalized_field_var)],
                        ),
                    )]),
                    variants: HashMap::from([(
                        "Recv".to_string(),
                        VariantDef::Tuple(vec![Ty::normalize_named(
                            "Receiver".to_string(),
                            vec![Ty::Var(normalized_variant_var)],
                        )]),
                    )]),
                    methods: HashMap::from([(
                        "close".to_string(),
                        FnSig {
                            params: vec![Ty::normalize_named(
                                "Sender".to_string(),
                                vec![Ty::Var(normalized_method_var)],
                            )],
                            return_type: Ty::Unit,
                            ..FnSig::default()
                        },
                    )]),
                    doc_comment: None,
                    field_order: vec!["tx".to_string()],
                    is_indirect: false,
                },
            ),
            (
                "LeakedField".to_string(),
                TypeDef {
                    kind: TypeDefKind::Struct,
                    name: "LeakedField".to_string(),
                    type_params: vec![],
                    bounds: HashMap::new(),
                    fields: HashMap::from([(
                        "value".to_string(),
                        Ty::Tuple(vec![Ty::Var(leaked_field_var)]),
                    )]),
                    variants: HashMap::new(),
                    methods: HashMap::new(),
                    doc_comment: None,
                    field_order: vec![],
                    is_indirect: false,
                },
            ),
            (
                "LeakedVariant".to_string(),
                TypeDef {
                    kind: TypeDefKind::Enum,
                    name: "LeakedVariant".to_string(),
                    type_params: vec![],
                    bounds: HashMap::new(),
                    fields: HashMap::new(),
                    variants: HashMap::from([(
                        "Recv".to_string(),
                        VariantDef::Tuple(vec![Ty::Var(leaked_variant_var)]),
                    )]),
                    methods: HashMap::new(),
                    doc_comment: None,
                    field_order: vec![],
                    is_indirect: false,
                },
            ),
        ]);

        let mut expr_types = HashMap::new();
        let mut fn_sigs = HashMap::new();
        let mut call_type_args = HashMap::new();
        let mut record_init_type_args = HashMap::new();
        checker.validate_checker_output_contract(
            &mut expr_types,
            &mut type_defs,
            &mut fn_sigs,
            &mut call_type_args,
            &mut record_init_type_args,
        );

        assert!(
            type_defs.contains_key("Good"),
            "concrete type definitions must survive the contract check"
        );
        assert!(
            !type_defs.contains_key("NormalizedHandles"),
            "a channel endpoint with an unresolved element must be pruned, not erased"
        );
        assert!(
            !type_defs.contains_key("LeakedField"),
            "type definitions with real untracked Ty::Var fields must be pruned"
        );
        assert!(
            !type_defs.contains_key("LeakedVariant"),
            "type definitions with real untracked Ty::Var variants must be pruned"
        );
    }

    #[test]
    fn validate_expr_output_contract_reports_and_prunes_ty_var_leak() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let leaked_span = SpanKey {
            start: 10,
            end: 20,
            module_idx: 0,
        };
        let leaked_var = TypeVar::fresh();
        let mut expr_types = HashMap::from([(leaked_span.clone(), Ty::Var(leaked_var))]);

        checker.validate_expr_output_contract(&mut expr_types, &HashSet::new());

        assert!(
            expr_types.is_empty(),
            "unresolved Ty::Var must be pruned from checker output: {expr_types:?}"
        );
        let inference_failed: Vec<_> = checker
            .errors
            .iter()
            .filter(|error| error.kind == TypeErrorKind::InferenceFailed)
            .collect();
        assert_eq!(
            inference_failed.len(),
            1,
            "expected a single InferenceFailed diagnostic for the leaked expr type: {:?}",
            checker.errors
        );
        assert_eq!(inference_failed[0].span.start, leaked_span.start);
        assert_eq!(inference_failed[0].span.end, leaked_span.end);
    }

    /// `validate_method_call_receiver_kinds_output_contract` retains entries
    /// for types that exist in the resolved `type_defs` map and prunes those
    /// that do not.
    #[test]
    fn validate_method_call_receiver_kinds_prunes_unknown_named_type_entries() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));

        let known_key = SpanKey {
            start: 10,
            end: 20,
            module_idx: 0,
        };
        let unknown_key = SpanKey {
            start: 30,
            end: 40,
            module_idx: 0,
        };

        checker.method_call_receiver_kinds.insert(
            known_key.clone(),
            MethodCallReceiverKind::NamedTypeInstance {
                type_name: "Widget".to_string(),
            },
        );
        checker.method_call_receiver_kinds.insert(
            unknown_key.clone(),
            MethodCallReceiverKind::NamedTypeInstance {
                type_name: "Phantom".to_string(),
            },
        );

        let mut type_defs = HashMap::from([(
            "Widget".to_string(),
            TypeDef {
                kind: TypeDefKind::Struct,
                name: "Widget".to_string(),
                type_params: vec![],
                bounds: HashMap::new(),
                fields: HashMap::new(),
                variants: HashMap::new(),
                methods: HashMap::new(),
                doc_comment: None,
                field_order: vec![],
                is_indirect: false,
            },
        )]);

        // Populate expr_types for both spans so that validate_method_call_output_contract
        // (span-based pruner) does not wipe entries before validate_method_call_receiver_kinds_output_contract runs.
        let mut expr_types =
            HashMap::from([(known_key.clone(), Ty::I64), (unknown_key.clone(), Ty::I64)]);
        let mut fn_sigs = HashMap::new();
        let mut call_type_args = HashMap::new();
        let mut record_init_type_args = HashMap::new();
        checker.validate_checker_output_contract(
            &mut expr_types,
            &mut type_defs,
            &mut fn_sigs,
            &mut call_type_args,
            &mut record_init_type_args,
        );

        assert!(
            checker.method_call_receiver_kinds.contains_key(&known_key),
            "NamedTypeInstance entry for a type in type_defs must survive"
        );
        assert!(
            !checker
                .method_call_receiver_kinds
                .contains_key(&unknown_key),
            "NamedTypeInstance entry for a type absent from type_defs must be pruned"
        );
    }

    /// Module-qualified type names (e.g. `json.Value`) are retained even though
    /// they are not present in `type_defs` — they live in the module registry.
    #[test]
    fn validate_method_call_receiver_kinds_retains_qualified_handle_type_entries() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));

        let handle_key = SpanKey {
            start: 50,
            end: 60,
            module_idx: 0,
        };
        checker.method_call_receiver_kinds.insert(
            handle_key.clone(),
            MethodCallReceiverKind::NamedTypeInstance {
                type_name: "json.Value".to_string(),
            },
        );

        let mut type_defs = HashMap::new(); // empty — json.Value is not a user type
                                            // Populate expr_types for the span so that validate_method_call_output_contract
                                            // (span-based pruner) does not wipe the entry before the name-based validator runs.
        let mut expr_types = HashMap::from([(handle_key.clone(), Ty::I64)]);
        let mut fn_sigs = HashMap::new();
        let mut call_type_args = HashMap::new();
        let mut record_init_type_args = HashMap::new();
        checker.validate_checker_output_contract(
            &mut expr_types,
            &mut type_defs,
            &mut fn_sigs,
            &mut call_type_args,
            &mut record_init_type_args,
        );

        assert!(
            checker.method_call_receiver_kinds.contains_key(&handle_key),
            "qualified handle-type entry (contains '.') must survive validation"
        );
    }

    /// `TraitObject` entries survive when the trait is present in `trait_defs`
    /// and are pruned when the trait is absent.
    #[test]
    fn validate_method_call_receiver_kinds_prunes_unknown_trait_object_entries() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));

        let known_trait_key = SpanKey {
            start: 70,
            end: 80,
            module_idx: 0,
        };
        let unknown_trait_key = SpanKey {
            start: 90,
            end: 100,
            module_idx: 0,
        };

        checker.method_call_receiver_kinds.insert(
            known_trait_key.clone(),
            MethodCallReceiverKind::TraitObject {
                trait_name: "Greeter".to_string(),
            },
        );
        checker.method_call_receiver_kinds.insert(
            unknown_trait_key.clone(),
            MethodCallReceiverKind::TraitObject {
                trait_name: "GhostTrait".to_string(),
            },
        );

        // Seed trait_defs with only the known trait.
        checker.trait_defs.insert(
            "Greeter".to_string(),
            TraitInfo {
                methods: vec![],
                associated_types: vec![],
                type_params: vec![],
            },
        );

        let mut type_defs = HashMap::new();
        // Populate expr_types for both trait spans so the span-based pruner does not
        // wipe entries before validate_method_call_receiver_kinds_output_contract runs.
        let mut expr_types = HashMap::from([
            (known_trait_key.clone(), Ty::I64),
            (unknown_trait_key.clone(), Ty::I64),
        ]);
        let mut fn_sigs = HashMap::new();
        let mut call_type_args = HashMap::new();
        let mut record_init_type_args = HashMap::new();
        checker.validate_checker_output_contract(
            &mut expr_types,
            &mut type_defs,
            &mut fn_sigs,
            &mut call_type_args,
            &mut record_init_type_args,
        );

        assert!(
            checker
                .method_call_receiver_kinds
                .contains_key(&known_trait_key),
            "TraitObject entry for a trait in trait_defs must survive"
        );
        assert!(
            !checker
                .method_call_receiver_kinds
                .contains_key(&unknown_trait_key),
            "TraitObject entry for a trait absent from trait_defs must be pruned"
        );
    }

    /// `NamedTypeInstance` entries whose `type_name` matches a generic type
    /// parameter from a function signature must survive validation — these are
    /// produced by trait-bounded type-parameter method dispatch.
    #[test]
    fn validate_method_call_receiver_kinds_retains_type_param_entries() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));

        let param_key = SpanKey {
            start: 110,
            end: 120,
            module_idx: 0,
        };
        checker.method_call_receiver_kinds.insert(
            param_key.clone(),
            MethodCallReceiverKind::NamedTypeInstance {
                type_name: "T".to_string(),
            },
        );

        // Populate the resolved fn_sigs map (mimicking the production path where
        // mod.rs drains self.fn_sigs via std::mem::take into resolved_fn_sigs before
        // calling validate_checker_output_contract).
        let mut fn_sigs = HashMap::from([(
            "display".to_string(),
            FnSig {
                impl_method: None,
                type_params: vec!["T".to_string()],
                type_param_bounds: HashMap::new(),
                param_names: vec!["item".to_string()],
                params: vec![Ty::Named {
                    builtin: None,
                    name: "T".to_string(),
                    args: vec![],
                }],
                return_type: Ty::Unit,
                accepts_kwargs: false,
                doc_comment: None,
                extern_symbol: None,
                requires_mutable_receiver: false,
                receiver_update: crate::ReceiverUpdate::Replace,
                param_ownership: vec![],
                consumes_receiver: false,
                returns_receiver_identity: false,
                is_builtin_variant: false,
            },
        )]);
        let mut type_defs = HashMap::new(); // "T" is not a user-defined type
                                            // Populate expr_types for the span so the span-based pruner does not wipe
                                            // the entry before validate_method_call_receiver_kinds_output_contract runs.
        let mut expr_types = HashMap::from([(param_key.clone(), Ty::I64)]);
        let mut call_type_args = HashMap::new();
        let mut record_init_type_args = HashMap::new();
        checker.validate_checker_output_contract(
            &mut expr_types,
            &mut type_defs,
            &mut fn_sigs,
            &mut call_type_args,
            &mut record_init_type_args,
        );

        assert!(
            checker.method_call_receiver_kinds.contains_key(&param_key),
            "NamedTypeInstance entry for a type-parameter name must survive validation"
        );
    }

    /// `validate_call_type_args_output_contract` retains entries whose span is
    /// present in `expr_types` and whose type arguments contain no inference vars.
    #[test]
    fn validate_call_type_args_output_contract_retains_valid_entries() {
        let valid_key = SpanKey {
            start: 10,
            end: 20,
            module_idx: 0,
        };
        let mut call_type_args = HashMap::from([(valid_key.clone(), vec![Ty::I32, Ty::Bool])]);
        let expr_types = HashMap::from([(valid_key.clone(), Ty::I32)]);

        Checker::validate_call_type_args_output_contract(&mut call_type_args, &expr_types);

        assert!(
            call_type_args.contains_key(&valid_key),
            "call_type_args entry with concrete types and a present span must survive"
        );
    }

    /// `validate_call_type_args_output_contract` prunes entries whose owning
    /// expression span is absent from the validated `expr_types` map.  An absent
    /// span means `validate_expr_output_contract` already pruned the expression
    /// (leaked inference state, cascading errors, etc.), so the side-table entry
    /// is orphaned and must not reach codegen.
    #[test]
    fn validate_call_type_args_output_contract_prunes_orphaned_entries() {
        let orphan_key = SpanKey {
            start: 30,
            end: 40,
            module_idx: 0,
        };
        let mut call_type_args = HashMap::from([(orphan_key.clone(), vec![Ty::I32])]);
        // expr_types is empty — the owning expression was pruned.
        let expr_types: HashMap<SpanKey, Ty> = HashMap::new();

        Checker::validate_call_type_args_output_contract(&mut call_type_args, &expr_types);

        assert!(
            call_type_args.is_empty(),
            "orphaned call_type_args entry (span absent from expr_types) must be pruned"
        );
    }

    /// `validate_call_type_args_output_contract` prunes entries that still contain
    /// unresolved `Ty::Var` inference holes even when the owning span is present in
    /// `expr_types`.  Leaked inference state must not cross the output boundary.
    #[test]
    fn validate_call_type_args_output_contract_prunes_leaked_inference_vars() {
        let present_key = SpanKey {
            start: 50,
            end: 60,
            module_idx: 0,
        };
        let inference_var = Ty::Var(crate::ty::TypeVar(42));
        let mut call_type_args =
            HashMap::from([(present_key.clone(), vec![Ty::I32, inference_var])]);
        // The span IS present in expr_types — only the inference var triggers pruning.
        let expr_types = HashMap::from([(present_key.clone(), Ty::I32)]);

        Checker::validate_call_type_args_output_contract(&mut call_type_args, &expr_types);

        assert!(
            call_type_args.is_empty(),
            "call_type_args entry containing Ty::Var must be pruned even if span is present"
        );
    }

    /// Mixed scenario: one valid entry, one orphaned entry, one entry with leaked
    /// inference state — only the valid entry must survive.
    #[test]
    fn validate_call_type_args_output_contract_mixed() {
        let valid_key = SpanKey {
            start: 10,
            end: 20,
            module_idx: 0,
        };
        let orphan_key = SpanKey {
            start: 30,
            end: 40,
            module_idx: 0,
        };
        let leaked_key = SpanKey {
            start: 50,
            end: 60,
            module_idx: 0,
        };
        let inference_var = Ty::Var(crate::ty::TypeVar(7));

        let mut call_type_args = HashMap::from([
            (valid_key.clone(), vec![Ty::I64]),
            (orphan_key.clone(), vec![Ty::Bool]),
            (leaked_key.clone(), vec![inference_var]),
        ]);
        // Only valid_key and leaked_key are present in expr_types.
        let expr_types =
            HashMap::from([(valid_key.clone(), Ty::I64), (leaked_key.clone(), Ty::I64)]);

        Checker::validate_call_type_args_output_contract(&mut call_type_args, &expr_types);

        assert!(
            call_type_args.contains_key(&valid_key),
            "valid call_type_args entry must survive"
        );
        assert!(
            !call_type_args.contains_key(&orphan_key),
            "orphaned call_type_args entry must be pruned"
        );
        assert!(
            !call_type_args.contains_key(&leaked_key),
            "call_type_args entry with leaked inference var must be pruned"
        );
    }

    // ── validate_handle_types_no_field_overlap ─────────────────────────────────

    /// A type whose fully-qualified name appears in `module_registry.handle_types`
    /// AND has non-empty `TypeDef.fields` must be rejected and pruned from
    /// `type_defs` so the incompatible representations (opaque-handle path
    /// vs struct-layout path) can never both reach codegen.
    ///
    /// Regression guard for hew-lang/hew#1252.
    #[test]
    fn validate_handle_types_no_field_overlap_rejects_overlap() {
        let mut module_registry = ModuleRegistry::new(vec![]);
        module_registry.insert_handle_type_for_test("fake.Handle".to_string());
        let mut checker = Checker::new(module_registry);

        // "fake.Handle" is the qualified key in type_defs — exactly as
        // register_qualified_type_alias would produce — and has non-empty fields.
        let mut type_defs = HashMap::from([(
            "fake.Handle".to_string(),
            TypeDef {
                kind: TypeDefKind::Struct,
                name: "fake.Handle".to_string(),
                type_params: vec![],
                bounds: HashMap::new(),
                fields: HashMap::from([("fd".to_string(), Ty::I32)]),
                variants: HashMap::new(),
                methods: HashMap::new(),
                doc_comment: None,
                field_order: vec![],
                is_indirect: false,
            },
        )]);

        checker.validate_handle_types_no_field_overlap(&mut type_defs);

        assert!(
            type_defs.is_empty(),
            "conflicting type must be pruned from type_defs"
        );
        let overlap_errors: Vec<_> = checker
            .errors
            .iter()
            .filter(|e| e.kind == TypeErrorKind::InvalidOperation)
            .collect();
        assert_eq!(
            overlap_errors.len(),
            1,
            "exactly one InvalidOperation error must be emitted for the overlap: {:?}",
            checker.errors
        );
        assert!(
            overlap_errors[0].message.contains("fake.Handle"),
            "error message must name the conflicting type"
        );
    }

    /// A user-defined type whose unqualified name coincidentally matches the
    /// short name of a stdlib handle type must NOT trigger the overlap check,
    /// because `module_registry.is_handle_type` requires a fully-qualified name
    /// (e.g. `"tls.TlsStream"`) and `type_defs` keys for user types are bare
    /// (e.g. `"TlsStream"`).
    ///
    /// A fieldless qualified entry for the same type must survive as well.
    ///
    /// Regression guard for hew-lang/hew#1252 false-positive risk.
    #[test]
    fn validate_handle_types_no_field_overlap_survives_unqualified_user_type() {
        let mut module_registry = ModuleRegistry::new(vec![]);
        module_registry.insert_handle_type_for_test("tls.TlsStream".to_string());
        let mut checker = Checker::new(module_registry);

        let mut type_defs = HashMap::from([
            // User type whose unqualified name matches the handle short name — must survive.
            (
                "TlsStream".to_string(),
                TypeDef {
                    kind: TypeDefKind::Struct,
                    name: "TlsStream".to_string(),
                    type_params: vec![],
                    bounds: HashMap::new(),
                    fields: HashMap::from([("fd".to_string(), Ty::I32)]),
                    variants: HashMap::new(),
                    methods: HashMap::new(),
                    doc_comment: None,
                    field_order: vec![],
                    is_indirect: false,
                },
            ),
            // Fieldless qualified alias for the actual handle type — also must survive.
            (
                "tls.TlsStream".to_string(),
                TypeDef {
                    kind: TypeDefKind::Struct,
                    name: "tls.TlsStream".to_string(),
                    type_params: vec![],
                    bounds: HashMap::new(),
                    fields: HashMap::new(), // fieldless — not a conflict
                    variants: HashMap::new(),
                    methods: HashMap::new(),
                    doc_comment: None,
                    field_order: vec![],
                    is_indirect: false,
                },
            ),
        ]);

        checker.validate_handle_types_no_field_overlap(&mut type_defs);

        assert_eq!(
            type_defs.len(),
            2,
            "both entries must survive: no overlap between unqualified user key and qualified handle name"
        );
        assert!(
            checker.errors.is_empty(),
            "no errors must be emitted: {:?}",
            checker.errors
        );
    }

    /// Diagnostic emitted for a qualified alias that has fields must carry the
    /// bare-name span, not the zero span that results when
    /// `register_qualified_type_alias` omits the span propagation.
    ///
    /// Regression guard for the `0..0` fallback in
    /// `validate_handle_types_no_field_overlap`.
    #[test]
    fn validate_handle_types_no_field_overlap_qualified_alias_span_is_propagated() {
        let mut module_registry = ModuleRegistry::new(vec![]);
        module_registry.insert_handle_type_for_test("fake.Handle".to_string());
        let mut checker = Checker::new(module_registry);

        // Seed the bare name entry in type_defs and type_def_spans, simulating
        // what register_type_namespace_name + register_type_decl would do.
        checker.type_def_spans.insert("Handle".to_string(), 10..25);
        checker.type_defs.insert(
            "Handle".to_string(),
            TypeDef {
                kind: TypeDefKind::Struct,
                name: "Handle".to_string(),
                type_params: vec![],
                bounds: HashMap::new(),
                fields: HashMap::new(),
                variants: HashMap::new(),
                methods: HashMap::new(),
                doc_comment: None,
                field_order: vec![],
                is_indirect: false,
            },
        );

        // register_qualified_type_alias must propagate the span to the qualified key.
        checker.register_qualified_type_alias("fake", "Handle");

        // Build the local type_defs map as the checker would pass to the validator:
        // the qualified alias has fields — triggering the overlap check.
        let mut type_defs = HashMap::from([(
            "fake.Handle".to_string(),
            TypeDef {
                kind: TypeDefKind::Struct,
                name: "fake.Handle".to_string(),
                type_params: vec![],
                bounds: HashMap::new(),
                fields: HashMap::from([("fd".to_string(), Ty::I32)]),
                variants: HashMap::new(),
                methods: HashMap::new(),
                doc_comment: None,
                field_order: vec![],
                is_indirect: false,
            },
        )]);

        checker.validate_handle_types_no_field_overlap(&mut type_defs);

        let overlap_errors: Vec<_> = checker
            .errors
            .iter()
            .filter(|e| e.kind == TypeErrorKind::InvalidOperation)
            .collect();
        assert_eq!(
            overlap_errors.len(),
            1,
            "expected exactly one overlap error"
        );
        assert_eq!(
            overlap_errors[0].span,
            10..25,
            "diagnostic span must be the bare-name declaration span, not 0..0"
        );
    }

    /// When a qualified key (e.g. `fake.Handle`) is identified as conflicting,
    /// the validator must defensively prune the bare-name twin (e.g. `Handle`) from
    /// `type_defs` so that `lookup_user_type_def`'s fallback cannot resolve to a
    /// field-bearing entry.
    #[test]
    fn validate_handle_types_no_field_overlap_prunes_bare_alias_twin() {
        let mut module_registry = ModuleRegistry::new(vec![]);
        module_registry.insert_handle_type_for_test("fake.Handle".to_string());
        let mut checker = Checker::new(module_registry);

        let mut type_defs = HashMap::from([(
            "Handle".to_string(),
            TypeDef {
                kind: TypeDefKind::Struct,
                name: "Handle".to_string(),
                type_params: vec![],
                bounds: HashMap::new(),
                fields: HashMap::from([("fd".to_string(), Ty::I32)]),
                variants: HashMap::new(),
                methods: HashMap::new(),
                doc_comment: None,
                field_order: vec![],
                is_indirect: false,
            },
        )]);

        // Also seed the qualified key that will trigger the conflict.
        type_defs.insert(
            "fake.Handle".to_string(),
            TypeDef {
                kind: TypeDefKind::Struct,
                name: "fake.Handle".to_string(),
                type_params: vec![],
                bounds: HashMap::new(),
                fields: HashMap::from([("fd".to_string(), Ty::I32)]),
                variants: HashMap::new(),
                methods: HashMap::new(),
                doc_comment: None,
                field_order: vec![],
                is_indirect: false,
            },
        );

        checker.validate_handle_types_no_field_overlap(&mut type_defs);

        // Both the qualified key and its bare-name twin must be removed.
        assert!(
            !type_defs.contains_key("fake.Handle"),
            "conflicting qualified key must be removed"
        );
        assert!(
            !type_defs.contains_key("Handle"),
            "bare-alias twin must also be pruned defensively"
        );

        // Verify an error was reported for the conflict.
        let overlap_errors: Vec<_> = checker
            .errors
            .iter()
            .filter(|e| e.kind == TypeErrorKind::InvalidOperation)
            .collect();
        assert_eq!(
            overlap_errors.len(),
            1,
            "expected exactly one overlap error for the qualified key"
        );
    }

    /// Integration test: handle-type field overlap is detected and pruned via the
    /// public entry point `validate_checker_output_contract`, not just the internal
    /// `validate_handle_types_no_field_overlap` validator.
    ///
    /// This test seeded an overlap via qualified-alias registration, verifies:
    /// 1. The overlap is rejected with an `InvalidOperation` diagnostic
    /// 2. Both the qualified key and bare-name twin are removed from `type_defs`
    /// 3. The error is reportable through the full contract validation pipeline
    #[test]
    fn validate_checker_output_contract_prunes_handle_type_field_overlap_via_public_entry() {
        let mut module_registry = ModuleRegistry::new(vec![]);
        module_registry.insert_handle_type_for_test("fake.Handle".to_string());
        let mut checker = Checker::new(module_registry);

        // Seed type_defs with both the bare name and qualified alias, simulating
        // what the checker's registration pipeline would produce:
        // - "Handle" from register_type_decl
        // - "fake.Handle" from register_qualified_type_alias with fields
        let mut type_defs = HashMap::from([
            (
                "Handle".to_string(),
                TypeDef {
                    kind: TypeDefKind::Struct,
                    name: "Handle".to_string(),
                    type_params: vec![],
                    bounds: HashMap::new(),
                    fields: HashMap::from([("fd".to_string(), Ty::I32)]),
                    variants: HashMap::new(),
                    methods: HashMap::new(),
                    doc_comment: None,
                    field_order: vec![],
                    is_indirect: false,
                },
            ),
            (
                "fake.Handle".to_string(),
                TypeDef {
                    kind: TypeDefKind::Struct,
                    name: "fake.Handle".to_string(),
                    type_params: vec![],
                    bounds: HashMap::new(),
                    fields: HashMap::from([("fd".to_string(), Ty::I32)]),
                    variants: HashMap::new(),
                    methods: HashMap::new(),
                    doc_comment: None,
                    field_order: vec![],
                    is_indirect: false,
                },
            ),
        ]);

        let mut expr_types = HashMap::new();
        let mut fn_sigs = HashMap::new();
        let mut call_type_args = HashMap::new();

        let mut record_init_type_args = HashMap::new();
        checker.validate_checker_output_contract(
            &mut expr_types,
            &mut type_defs,
            &mut fn_sigs,
            &mut call_type_args,
            &mut record_init_type_args,
        );

        // Both qualified key and bare-name twin must be pruned from type_defs
        // after validate_checker_output_contract processes the overlap.
        assert!(
            !type_defs.contains_key("fake.Handle"),
            "qualified handle-type key with fields must be pruned from type_defs"
        );
        assert!(
            !type_defs.contains_key("Handle"),
            "bare-alias twin must also be pruned defensively from type_defs"
        );

        // Exactly one InvalidOperation diagnostic must be reported.
        let overlap_errors: Vec<_> = checker
            .errors
            .iter()
            .filter(|e| e.kind == TypeErrorKind::InvalidOperation)
            .collect();
        assert_eq!(
            overlap_errors.len(),
            1,
            "exactly one overlap error must be emitted via the public contract entry: {:?}",
            checker.errors
        );
        assert!(
            overlap_errors[0].message.contains("fake.Handle"),
            "error message must name the conflicting type"
        );
    }

    // ── validate_lowering_facts_output_contract ────────────────────────────────

    /// Well-formed facts whose spans exist in `expr_types` must survive.
    #[test]
    fn lowering_facts_output_contract_retains_valid_facts() {
        use crate::lowering_facts::{
            DropKind, HashSetAbi, HashSetElementType, LoweringFact, LoweringKind,
        };
        let key = SpanKey {
            start: 1,
            end: 5,
            module_idx: 0,
        };
        let mut facts = HashMap::from([(
            key.clone(),
            LoweringFact {
                kind: LoweringKind::HashSet,
                element_type: HashSetElementType::I64,
                abi_variant: HashSetAbi::Int64,
                drop_kind: DropKind::HashSetFree,
            },
        )]);
        let expr_types = HashMap::from([(key.clone(), Ty::Bool)]);
        validate_lowering_facts_output_contract(&mut facts, &expr_types);
        assert!(
            facts.contains_key(&key),
            "a well-formed fact with a present span must survive the contract check"
        );
    }

    /// A fact whose span has been pruned from `expr_types` (orphaned) must be
    /// dropped so downstream codegen cannot observe a fact without a resolved
    /// expression type.
    #[test]
    fn lowering_facts_output_contract_prunes_orphaned_facts() {
        use crate::lowering_facts::{
            DropKind, HashSetAbi, HashSetElementType, LoweringFact, LoweringKind,
        };
        let key = SpanKey {
            start: 10,
            end: 20,
            module_idx: 0,
        };
        let mut facts = HashMap::from([(
            key.clone(),
            LoweringFact {
                kind: LoweringKind::HashSet,
                element_type: HashSetElementType::Str,
                abi_variant: HashSetAbi::String,
                drop_kind: DropKind::HashSetFree,
            },
        )]);
        // Span is absent from expr_types — simulates the expression being pruned
        // by validate_expr_output_contract due to a leaked inference variable.
        let expr_types: HashMap<SpanKey, Ty> = HashMap::new();
        validate_lowering_facts_output_contract(&mut facts, &expr_types);
        assert!(
            facts.is_empty(),
            "orphaned lowering fact (span absent from expr_types) must be pruned"
        );
    }

    /// An internally inconsistent fact (`element_type` / `abi_variant` mismatch)
    /// must be pruned even if its span exists in `expr_types`.
    #[test]
    fn lowering_facts_output_contract_prunes_inconsistent_facts() {
        use crate::lowering_facts::{
            DropKind, HashSetAbi, HashSetElementType, LoweringFact, LoweringKind,
        };
        let key = SpanKey {
            start: 30,
            end: 40,
            module_idx: 0,
        };
        let mut facts = HashMap::from([(
            key.clone(),
            // Intentionally wrong pairing: Str element with Int64 ABI.
            LoweringFact {
                kind: LoweringKind::HashSet,
                element_type: HashSetElementType::Str,
                abi_variant: HashSetAbi::Int64,
                drop_kind: DropKind::HashSetFree,
            },
        )]);
        let expr_types = HashMap::from([(key.clone(), Ty::Bool)]);
        validate_lowering_facts_output_contract(&mut facts, &expr_types);
        assert!(
            facts.is_empty(),
            "internally inconsistent fact (Str/Int64 mismatch) must be pruned"
        );
    }

    // ── Layout computation tests (C-2c) ────────────────────────────────────────

    /// Helper: build a minimal Copy record `TypeDef`.
    fn make_record(name: &str, fields: Vec<(&str, Ty)>) -> TypeDef {
        let field_order: Vec<String> = fields.iter().map(|(n, _)| n.to_string()).collect();
        TypeDef {
            kind: TypeDefKind::Record,
            name: name.to_string(),
            type_params: vec![],
            bounds: HashMap::new(),
            fields: fields
                .into_iter()
                .map(|(n, t)| (n.to_string(), t))
                .collect(),
            field_order,
            variants: HashMap::new(),
            methods: HashMap::new(),
            doc_comment: None,
            is_indirect: false,
        }
    }

    fn make_generic_record(name: &str, params: Vec<&str>, fields: Vec<(&str, Ty)>) -> TypeDef {
        let mut td = make_record(name, fields);
        td.type_params = params.into_iter().map(str::to_string).collect();
        td
    }

    #[test]
    fn primitive_copy_layout_bool_is_1_1() {
        assert_eq!(
            primitive_copy_layout(&Ty::Bool, &HashMap::new()),
            Some((1, 1))
        );
    }

    #[test]
    fn primitive_copy_layout_i8_u8_is_1_1() {
        assert_eq!(
            primitive_copy_layout(&Ty::I8, &HashMap::new()),
            Some((1, 1))
        );
        assert_eq!(
            primitive_copy_layout(&Ty::U8, &HashMap::new()),
            Some((1, 1))
        );
    }

    #[test]
    fn primitive_copy_layout_i16_u16_is_2_2() {
        assert_eq!(
            primitive_copy_layout(&Ty::I16, &HashMap::new()),
            Some((2, 2))
        );
        assert_eq!(
            primitive_copy_layout(&Ty::U16, &HashMap::new()),
            Some((2, 2))
        );
    }

    #[test]
    fn primitive_copy_layout_i32_u32_char_is_4_4() {
        assert_eq!(
            primitive_copy_layout(&Ty::I32, &HashMap::new()),
            Some((4, 4))
        );
        assert_eq!(
            primitive_copy_layout(&Ty::U32, &HashMap::new()),
            Some((4, 4))
        );
        assert_eq!(
            primitive_copy_layout(&Ty::Char, &HashMap::new()),
            Some((4, 4))
        );
    }

    #[test]
    fn primitive_copy_layout_i64_u64_duration_is_8_8() {
        assert_eq!(
            primitive_copy_layout(&Ty::I64, &HashMap::new()),
            Some((8, 8))
        );
        assert_eq!(
            primitive_copy_layout(&Ty::U64, &HashMap::new()),
            Some((8, 8))
        );
        assert_eq!(
            primitive_copy_layout(&Ty::Duration, &HashMap::new()),
            Some((8, 8))
        );
    }

    #[test]
    fn identity_aggregate_layouts_are_fixed() {
        let node_id = Ty::builtin_named(BuiltinType::NodeId, vec![]);
        let location = Ty::builtin_named(BuiltinType::Location, vec![]);
        let remote_pid = Ty::remote_pid(Ty::I64);
        let child_ref = Ty::child_ref(Ty::I64);
        let type_defs = HashMap::new();

        assert_eq!(primitive_copy_layout(&node_id, &type_defs), Some((16, 8)));
        assert_eq!(primitive_copy_layout(&location, &type_defs), Some((32, 8)));
        assert_eq!(
            primitive_copy_layout(&remote_pid, &type_defs),
            Some((32, 8))
        );
        assert_eq!(primitive_copy_layout(&child_ref, &type_defs), Some((16, 8)));
    }

    #[test]
    fn primitive_copy_layout_string_returns_none() {
        // String is heap-managed; not a fixed-layout Copy type. The hash-key
        // Its payload requires a clone rather than a bit copy.
        // blob — `primitive_copy_layout` stays the Copy authority.
        assert_eq!(primitive_copy_layout(&Ty::String, &HashMap::new()), None);
    }

    #[test]
    fn primitive_copy_layout_substitutes_generic_record_args() {
        let type_defs = HashMap::from([
            (
                "Wrap".to_string(),
                make_generic_record(
                    "Wrap",
                    vec!["T"],
                    vec![(
                        "v",
                        Ty::Named {
                            name: "T".to_string(),
                            args: vec![],
                            builtin: None,
                        },
                    )],
                ),
            ),
            (
                "Pair".to_string(),
                make_generic_record(
                    "Pair",
                    vec!["A", "B"],
                    vec![
                        (
                            "a",
                            Ty::Named {
                                name: "A".to_string(),
                                args: vec![],
                                builtin: None,
                            },
                        ),
                        (
                            "b",
                            Ty::Named {
                                name: "B".to_string(),
                                args: vec![],
                                builtin: None,
                            },
                        ),
                    ],
                ),
            ),
            (
                "Point".to_string(),
                make_record("Point", vec![("x", Ty::I64), ("y", Ty::I64)]),
            ),
            (
                "Holder".to_string(),
                make_generic_record(
                    "Holder",
                    vec!["T"],
                    vec![(
                        "value",
                        Ty::Named {
                            name: "T".to_string(),
                            args: vec![],
                            builtin: None,
                        },
                    )],
                ),
            ),
        ]);

        let wrap_i64 = Ty::Named {
            name: "Wrap".to_string(),
            args: vec![Ty::I64],
            builtin: None,
        };
        let pair_i64 = Ty::Named {
            name: "Pair".to_string(),
            args: vec![Ty::I64, Ty::I64],
            builtin: None,
        };
        let holder_point = Ty::Named {
            name: "Holder".to_string(),
            args: vec![Ty::Named {
                name: "Point".to_string(),
                args: vec![],
                builtin: None,
            }],
            builtin: None,
        };
        let nested_wrap = Ty::Named {
            name: "Wrap".to_string(),
            args: vec![wrap_i64.clone()],
            builtin: None,
        };

        assert_eq!(primitive_copy_layout(&wrap_i64, &type_defs), Some((8, 8)));
        assert_eq!(primitive_copy_layout(&pair_i64, &type_defs), Some((16, 8)));
        assert_eq!(
            primitive_copy_layout(&holder_point, &type_defs),
            Some((16, 8))
        );
        assert_eq!(
            primitive_copy_layout(&nested_wrap, &type_defs),
            Some((8, 8))
        );
    }

    #[test]
    fn tuple_record_constructor_signature_uses_exact_owner() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let pair = TypeDef {
            kind: TypeDefKind::Record,
            name: "Pair".to_string(),
            type_params: Vec::new(),
            bounds: HashMap::new(),
            fields: HashMap::new(),
            field_order: Vec::new(),
            variants: HashMap::new(),
            methods: HashMap::new(),
            doc_comment: None,
            is_indirect: false,
        };
        checker.fn_sigs.insert(
            "left.Pair".to_string(),
            FnSig {
                params: vec![Ty::I64],
                return_type: Ty::named("left.Pair", Vec::new()),
                ..FnSig::default()
            },
        );
        // A legacy bare constructor entry is present too. Before exact
        // matching, `right.Pair` could reach it through the short-name path.
        checker.fn_sigs.insert(
            "Pair".to_string(),
            FnSig {
                params: vec![Ty::Bool],
                return_type: Ty::named("left.Pair", Vec::new()),
                ..FnSig::default()
            },
        );
        assert_eq!(
            checker.tuple_record_constructor_fields("left.Pair", &pair),
            vec![Ty::I64],
            "the owning package's tuple constructor remains discoverable"
        );
        assert!(
            checker
                .tuple_record_constructor_fields("right.Pair", &pair)
                .is_empty(),
            "a same-leaf foreign constructor cannot supply left.Pair's layout"
        );
    }
}
