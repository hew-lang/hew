#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;
use crate::traits::RcFreeStatus;
use crate::BuiltinType;

pub(crate) fn signature_contains_error_type(params: &[Ty], ret: &Ty) -> bool {
    params.iter().any(ty_contains_error) || ty_contains_error(ret)
}

// ── Layout computation helpers (C-2c) ─────────────────────────────────────────

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
    match ty {
        Ty::Bool | Ty::I8 | Ty::U8 => Some((1, 1)),
        Ty::I16 | Ty::U16 => Some((2, 2)),
        // f32 is hash-ineligible (caught by ty_is_hash_eligible) but included
        // for completeness so this function can serve as a general primitive sizer.
        Ty::I32 | Ty::U32 | Ty::Char | Ty::F32 => Some((4, 4)),
        // f64 is hash-ineligible but included for the same reason.
        Ty::I64 | Ty::U64 | Ty::Duration | Ty::F64 => Some((8, 8)),
        Ty::Array(elem, count) => {
            let (elem_size, elem_align) = primitive_copy_layout(elem, type_defs)?;
            let count = usize::try_from(*count).ok()?;
            Some((elem_size.checked_mul(count)?, elem_align))
        }
        Ty::Named { name, args, .. } => {
            // Try direct lookup first, then strip module prefix (mirrors lookup_type_def).
            let type_def = type_defs.get(name.as_str()).or_else(|| {
                name.split_once('.')
                    .and_then(|(_, local)| type_defs.get(local))
            })?;
            if args.is_empty() {
                compute_copy_record_layout(type_def, type_defs)
            } else {
                if type_def.type_params.len() != args.len() {
                    return None;
                }
                let subst: HashMap<String, Ty> = type_def
                    .type_params
                    .iter()
                    .zip(args.iter())
                    .map(|(param, arg)| (param.clone(), arg.clone()))
                    .collect();
                let mut instantiated = type_def.clone();
                instantiated.fields = type_def
                    .fields
                    .iter()
                    .map(|(field, ty)| (field.clone(), ty.substitute_named_params_parallel(&subst)))
                    .collect();
                compute_copy_record_layout(&instantiated, type_defs)
            }
        }
        _ => None,
    }
}

/// Compute `(total_size, max_align)` for a Copy named-record type.
///
/// Fields are walked in **declaration order** — the order fields appear in the
/// source declaration.  This matches the order used by HIR `RecordLayout.fields`,
/// MIR `RecordLayout.field_tys`, and codegen's LLVM struct body emission, ensuring
/// that the checker-computed key size/alignment agrees with the binary ABI.
///
/// Uses `type_def.field_order` (populated by `register_record_decl` and friends in
/// `registration.rs`).  For synthetic/test `TypeDef`s where `field_order` is empty
/// but `fields` is non-empty, falls back to **alphabetical order** so that unit
/// tests that build `TypeDef`s by hand still produce a deterministic result.  This
/// fallback should not occur in production paths (the registration pass always
/// populates `field_order` for named-field records).
///
/// Returns `None` when:
/// - The record has no fields (zero-size keys are an ABI violation).
/// - A field's layout cannot be determined (type not in scope, generic param, etc.).
///
/// The computed size is the C-like struct size: fields are padded to their
/// natural alignment, and the total size is rounded up to the struct's max-field
/// alignment (i.e., `sizeof(struct { ... })` in C terms).
pub(crate) fn compute_copy_record_layout(
    type_def: &TypeDef,
    type_defs: &HashMap<String, TypeDef>,
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
        let (field_size, field_align) = primitive_copy_layout(field_ty, type_defs)?;

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

fn normalize_synthetic_channel_handle_type(ty: &Ty) -> Ty {
    match ty {
        Ty::Named { name, args, .. } => {
            let normalized_args: Vec<Ty> = args
                .iter()
                .map(normalize_synthetic_channel_handle_type)
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
                .map(normalize_synthetic_channel_handle_type)
                .collect(),
        ),
        Ty::Array(elem, size) => Ty::Array(
            Box::new(normalize_synthetic_channel_handle_type(elem)),
            *size,
        ),
        Ty::Slice(elem) => Ty::Slice(Box::new(normalize_synthetic_channel_handle_type(elem))),
        Ty::Function { params, ret } => Ty::Function {
            params: params
                .iter()
                .map(normalize_synthetic_channel_handle_type)
                .collect(),
            ret: Box::new(normalize_synthetic_channel_handle_type(ret)),
        },
        Ty::Closure {
            params,
            ret,
            captures,
        } => Ty::Closure {
            params: params
                .iter()
                .map(normalize_synthetic_channel_handle_type)
                .collect(),
            ret: Box::new(normalize_synthetic_channel_handle_type(ret)),
            captures: captures
                .iter()
                .map(normalize_synthetic_channel_handle_type)
                .collect(),
        },
        Ty::Pointer {
            is_mutable,
            pointee,
        } => Ty::Pointer {
            is_mutable: *is_mutable,
            pointee: Box::new(normalize_synthetic_channel_handle_type(pointee)),
        },
        _ => ty.clone(),
    }
}

fn normalized_variant_def_has_inference_var(variant: &VariantDef) -> bool {
    match variant {
        VariantDef::Unit => false,
        VariantDef::Tuple(fields) => fields
            .iter()
            .map(normalize_synthetic_channel_handle_type)
            .any(|field| field.has_inference_var()),
        VariantDef::Struct(fields) => fields
            .iter()
            .map(|(_, field)| normalize_synthetic_channel_handle_type(field))
            .any(|field| field.has_inference_var()),
    }
}

fn fn_sig_has_inference_var(sig: &FnSig) -> bool {
    sig.params
        .iter()
        .map(normalize_synthetic_channel_handle_type)
        .any(|param| param.has_inference_var())
        || normalize_synthetic_channel_handle_type(&sig.return_type).has_inference_var()
}

fn variant_def_has_inference_var(variant: &VariantDef) -> bool {
    normalized_variant_def_has_inference_var(variant)
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
    type_def
        .fields
        .values()
        .map(normalize_synthetic_channel_handle_type)
        .any(|field| field.has_inference_var())
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
            if !unresolved.is_subset(covered_inference_vars) {
                let normalized = normalize_synthetic_channel_handle_type(ty);
                if normalized != *ty {
                    let mut normalized_unresolved = HashSet::new();
                    collect_unresolved_inference_vars(&normalized, &mut normalized_unresolved);
                    *ty = normalized;
                    unresolved = normalized_unresolved;
                    if unresolved.is_empty() {
                        continue;
                    }
                }
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
                ActorMethodKind::Fire(_) => true,
                // Output-contract pruning ONLY: retain the dispatch entry when
                // the reply type is fully resolved and error-free. This is NOT
                // the reply-type admissibility gate — a non-Send reply (`Rc`,
                // etc.) is rejected at record time by `record_actor_method_dispatch`
                // (methods.rs, `E_DUPLEX_NON_SEND`), and any reply the codegen
                // reply-drop classifier cannot prove safe to drop fails closed
                // there (#1739). Do not add a Send/handle rejection here; this
                // pass runs after type-checking and only graduates the
                // side-table to a validated contract.
                ActorMethodKind::Ask(_, reply_ty) => {
                    !reply_ty.has_inference_var() && !reply_ty.contains_error()
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
        // pointers, dyn-Trait, LocalPid, and other non-serialisable types
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

    pub(super) fn reject_rc_collection_element(
        &mut self,
        container: &str,
        elem_ty: &Ty,
        span: &Span,
    ) -> bool {
        let resolved = self.subst.resolve(elem_ty);
        match self.registry.rc_free_status(&resolved) {
            RcFreeStatus::RcFree => true,
            RcFreeStatus::ContainsRc => {
                self.report_error(
                    TypeErrorKind::UnsafeCollectionElement,
                    span,
                    format!(
                        "`{container}` cannot hold `{}`; Rc<T> in collections is not yet \
                         supported (runtime does not track Rc ownership for collection elements)",
                        resolved.user_facing()
                    ),
                );
                false
            }
            RcFreeStatus::Recursive(type_name) => {
                self.report_error(
                    TypeErrorKind::UnsafeCollectionElement,
                    span,
                    format!(
                        "`{container}` cannot hold `{}`; RcFree could not be proven because `{type_name}` participates in a recursive type cycle",
                        resolved.user_facing()
                    ),
                );
                false
            }
        }
    }

    fn is_supported_hashmap_key_type(&self, ty: &Ty) -> bool {
        // W4.001 Stage C3: legacy per-K allowlist retired. Admit any K that
        // implements both `Hash` and `Eq` markers — the resolver's
        // `where K: Hash + Eq` bound is the sole admission contract.
        // Unsatisfied bounds (e.g. `f64: Hash` failing) surface as a
        // `BoundsNotSatisfied` diagnostic from `record_resolved_hashmap_call`.
        self.registry.implements_marker(ty, MarkerTrait::Hash)
            && self.registry.implements_marker(ty, MarkerTrait::Eq)
    }

    fn is_supported_hashmap_value_type(_ty: &Ty) -> bool {
        // W4.001 Stage C3: resolver imposes no V bound on
        // `impl<K, V> Map for HashMap<K, V> where K: Hash + Eq`.
        true
    }

    fn is_supported_hashmap_projection_element_type(&self, ty: &Ty) -> bool {
        match ty {
            Ty::Bool
            | Ty::Char
            | Ty::I32
            | Ty::U32
            | Ty::I64
            | Ty::U64
            | Ty::F32
            | Ty::F64
            | Ty::String => true,
            Ty::Named {
                name,
                builtin: None,
                ..
            } => {
                let Some(type_def) = self.type_defs.get(name.as_str()).or_else(|| {
                    name.split_once('.')
                        .and_then(|(_, local)| self.type_defs.get(local))
                }) else {
                    return false;
                };
                matches!(type_def.kind, TypeDefKind::Record | TypeDefKind::Enum)
                    && (primitive_copy_layout(ty, &self.type_defs).is_some()
                        || self.registry.implements_marker(ty, MarkerTrait::Copy))
            }
            _ => false,
        }
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

        // Ty::Var: inference is still in-flight at this call site.  Defer the
        // admission check until finalize_hashmap_admission() runs after all
        // inference has settled, mirroring the HashSet lowering-fact pattern.
        if matches!(resolved_key, Ty::Var(_)) || matches!(resolved_val, Ty::Var(_)) {
            self.deferred_hashmap_admission
                .entry(SpanKey::in_module(span, self.current_module_idx))
                .or_insert_with(|| DeferredHashMapAdmission {
                    span: span.clone(),
                    key_ty: key_ty.clone(),
                    val_ty: val_ty.clone(),
                    source_module: self.current_module.clone(),
                });
            return true; // optimistically admit; finalization will fail closed
        }

        // Named record key: defer to finalize_hashmap_admission for full hash-eligibility
        // check and HashMapLoweringFact production (C-2c).  Optimistically admit here;
        // finalize will fail closed with a diagnostic if the key is ineligible.
        if matches!(&resolved_key, Ty::Named { .. }) {
            self.deferred_hashmap_admission
                .entry(SpanKey::in_module(span, self.current_module_idx))
                .or_insert_with(|| DeferredHashMapAdmission {
                    span: span.clone(),
                    key_ty: resolved_key.clone(),
                    val_ty: resolved_val.clone(),
                    source_module: self.current_module.clone(),
                });
            return true;
        }

        if self.is_supported_hashmap_key_type(&resolved_key)
            && Self::is_supported_hashmap_value_type(&resolved_val)
        {
            return true;
        }

        // Key fails resolver bounds (e.g. `f64: Hash`). Emit the structured
        // `BoundsNotSatisfied(Hash/Eq, K)` diagnostic here and return false
        // so callers fail closed uniformly (bare-type annotation paths via
        // `validate_concrete_hashmap_type` plus all HashMap method arms,
        // including the resolver-bypass arms `is_empty` / `keys` /
        // `values` / `clone`). Method arms that also invoke
        // `record_resolved_hashmap_call` short-circuit on `Ty::Error`
        // before reaching the resolver, so no double-emit.
        if !self.has_bounds_not_satisfied_at(span) {
            let hash_ok = self
                .registry
                .implements_marker(&resolved_key, MarkerTrait::Hash);
            let eq_ok = self
                .registry
                .implements_marker(&resolved_key, MarkerTrait::Eq);
            let mut missing: Vec<&'static str> = Vec::new();
            if !hash_ok {
                missing.push("Hash");
            }
            if !eq_ok {
                missing.push("Eq");
            }
            let bound_summary = if missing.is_empty() {
                "Hash + Eq".to_string()
            } else {
                missing
                    .iter()
                    .map(|m| format!("K: {m}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            };
            self.report_error(
                TypeErrorKind::BoundsNotSatisfied,
                span,
                format!(
                    "`{}` does not satisfy the required bounds for `Map` \
                     ({bound_summary})",
                    resolved_key.user_facing()
                ),
            );
        }
        false
    }

    fn reject_unsafe_hashmap_element_types(
        &mut self,
        key_ty: &Ty,
        val_ty: &Ty,
        span: &Span,
    ) -> bool {
        let key_ok = self.reject_rc_collection_element("HashMap", key_ty, span);
        let val_ok = self.reject_rc_collection_element("HashMap", val_ty, span);
        key_ok && val_ok
    }

    pub(super) fn validate_hashmap_owned_element_types(
        &mut self,
        key_ty: &Ty,
        val_ty: &Ty,
        span: &Span,
    ) -> bool {
        self.reject_unsafe_hashmap_element_types(key_ty, val_ty, span)
            && self.validate_hashmap_key_value_types(key_ty, val_ty, span)
    }

    pub(super) fn validate_hashmap_projection_element_types(
        &mut self,
        key_ty: &Ty,
        val_ty: &Ty,
        method: &str,
        span: &Span,
    ) -> bool {
        if !self.validate_hashmap_owned_element_types(key_ty, val_ty, span) {
            return false;
        }

        let resolved_key = self.subst.resolve(key_ty).materialize_literal_defaults();
        let resolved_val = self.subst.resolve(val_ty).materialize_literal_defaults();

        if matches!(resolved_key, Ty::Error) || matches!(resolved_val, Ty::Error) {
            return false;
        }

        if matches!(resolved_key, Ty::Var(_)) || matches!(resolved_val, Ty::Var(_)) {
            return true;
        }

        if !self.is_supported_hashmap_projection_element_type(&resolved_val) {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!(
                    "`HashMap<{}, {}>.{method}()` is not yet supported: projecting from a map with value type `{}` into an owned `Vec` is not lowered; supported projection value types are scalar primitives, `string`, and Copy record/enum types",
                    resolved_key.user_facing(),
                    resolved_val.user_facing(),
                    resolved_val.user_facing()
                ),
            );
            return false;
        }

        if method == "keys" && !self.is_supported_hashmap_projection_element_type(&resolved_key) {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!(
                    "`HashMap<{}, {}>.keys()` is not yet supported: projecting key type `{}` into an owned `Vec` is not lowered; supported projection key types are scalar primitives, `string`, and Copy record/enum types",
                    resolved_key.user_facing(),
                    resolved_val.user_facing(),
                    resolved_key.user_facing()
                ),
            );
            return false;
        }

        true
    }

    pub(super) fn validate_hashset_element_type(&mut self, elem_ty: &Ty, span: &Span) -> bool {
        let resolved = self.subst.resolve(elem_ty);

        // Ty::Error: upstream already emitted a diagnostic; fail closed silently
        // to prevent cascading errors from admission logic.
        if matches!(resolved, Ty::Error) {
            return false;
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

        if matches!(resolved, Ty::String | Ty::I64 | Ty::U64 | Ty::IntLiteral) {
            return true;
        }

        // Named type: optimistically admit.  `record_hashset_lowering_fact` adds
        // the element type to `pending_lowering_facts`; `finalize_lowering_facts`
        // runs hash-eligibility and produces a `HashSetLoweringFact` or emits a
        // diagnostic (C-2c).
        if matches!(&resolved, Ty::Named { .. }) {
            return true;
        }

        // W4.001 Stage C3: legacy per-element allowlist retired. Admit any
        // T that implements `Hash + Eq`; otherwise emit a structured
        // `BoundsNotSatisfied` diagnostic and fail closed (see the
        // matching rationale in `validate_hashmap_key_value_types`).
        if self
            .registry
            .implements_marker(&resolved, MarkerTrait::Hash)
            && self.registry.implements_marker(&resolved, MarkerTrait::Eq)
        {
            return true;
        }

        if !self.has_bounds_not_satisfied_at(span) {
            let hash_ok = self
                .registry
                .implements_marker(&resolved, MarkerTrait::Hash);
            let eq_ok = self.registry.implements_marker(&resolved, MarkerTrait::Eq);
            let mut missing: Vec<&'static str> = Vec::new();
            if !hash_ok {
                missing.push("Hash");
            }
            if !eq_ok {
                missing.push("Eq");
            }
            let bound_summary = if missing.is_empty() {
                "Hash + Eq".to_string()
            } else {
                missing
                    .iter()
                    .map(|m| format!("T: {m}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            };
            self.report_error(
                TypeErrorKind::BoundsNotSatisfied,
                span,
                format!(
                    "`{}` does not satisfy the required bounds for `Set` \
                     ({bound_summary})",
                    resolved.user_facing()
                ),
            );
        }
        false
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
        if !self.reject_rc_collection_element("HashSet", elem_ty, span) {
            return false;
        }
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
        self.registry.implements_marker(elem_ty, MarkerTrait::Copy)
            || primitive_copy_layout(elem_ty, &self.type_defs).is_some()
    }

    pub(super) fn vec_element_contains_structural_array(
        &self,
        ty: &Ty,
        visiting: &mut HashSet<String>,
    ) -> bool {
        let resolved = self.subst.resolve(ty);
        match &resolved {
            Ty::Array(_, _) => true,
            Ty::Tuple(elems) => elems
                .iter()
                .any(|elem| self.vec_element_contains_structural_array(elem, visiting)),
            Ty::Named {
                builtin: Some(BuiltinType::Range | BuiltinType::Option | BuiltinType::Result),
                args,
                ..
            } => args
                .iter()
                .any(|arg| self.vec_element_contains_structural_array(arg, visiting)),
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
                    self.vec_element_contains_structural_array(&field_ty, visiting)
                }) || type_def.variants.values().any(|variant| match variant {
                    VariantDef::Unit => false,
                    VariantDef::Tuple(tys) => tys.iter().any(|ty| {
                        let ty = Self::instantiate_type_def_member(ty, &type_def.type_params, args);
                        self.vec_element_contains_structural_array(&ty, visiting)
                    }),
                    VariantDef::Struct(fields) => fields.iter().any(|(_, ty)| {
                        let ty = Self::instantiate_type_def_member(ty, &type_def.type_params, args);
                        self.vec_element_contains_structural_array(&ty, visiting)
                    }),
                });
                visiting.remove(type_def.name.as_str());
                result
            }
            _ => false,
        }
    }

    /// True when a Vec element type transitively carries a function/closure
    /// value INSIDE a composite (record field, enum variant payload, tuple
    /// member, Option/Result/Range argument). A direct `Vec<fn(...)>` element
    /// is the supported boxed-pair class and is NOT flagged here — the caller
    /// exempts the top-level Function/Closure shape. Composite elements ride
    /// the layout/owned byte-copy ABIs, which would shallow-copy the embedded
    /// pair and alias its sole-owner environment box, so they fail closed at
    /// admission. Recursion shape mirrors
    /// [`vec_element_contains_structural_array`](Self::vec_element_contains_structural_array).
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

        // Reject ANY Vec element that contains a structural array, regardless of
        // whether the array has a copy layout.  Codegen cannot lower array/composite
        // Vec elements yet (Cluster 2 deferred).  Admitting a copy-layout array
        // (e.g. [i64; 2]) here only defers the failure to an unspanned codegen
        // error — fail closed at the checker instead so the user sees a source span.
        let mut visiting = HashSet::new();
        if self.vec_element_contains_structural_array(resolved, &mut visiting) {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!(
                    "Vec<{}> is not supported; vec lowering does not support array element types yet",
                    resolved.user_facing()
                ),
            );
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
                        "Vec<{}> is not supported: the element type contains a function \
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
            Ty::Function { params, ret } => {
                params
                    .iter()
                    .all(|param| self.validate_concrete_collection_type(param, span, collection))
                    && self.validate_concrete_collection_type(ret, span, collection)
            }
            Ty::Closure {
                params,
                ret,
                captures,
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

    fn rc_payload_drop_supported(&self, ty: &Ty) -> bool {
        let resolved = self.subst.resolve(ty);
        if matches!(resolved, Ty::Error) {
            return true;
        }
        if matches!(resolved, Ty::Var(_)) {
            return false;
        }
        match &resolved {
            Ty::String | Ty::Bytes => true,
            Ty::Named {
                builtin: Some(BuiltinType::Rc),
                args,
                ..
            } if args.len() == 1 => self.rc_payload_drop_supported(&args[0]),
            Ty::Named { name, .. } if self.type_implements_trait(name, "Drop") => false,
            _ => self
                .registry
                .implements_marker(&resolved, MarkerTrait::Copy),
        }
    }

    pub(super) fn validate_rc_payload_type(&mut self, ty: &Ty, span: &Span) -> bool {
        let resolved = self.subst.resolve(ty);
        if self.rc_payload_drop_supported(&resolved) {
            return true;
        }

        self.report_error(
            TypeErrorKind::InvalidOperation,
            span,
            format!(
                "`Rc<{}>` is not currently supported; Rc only accepts Copy payloads, \
                `String`, `bytes`, and nested `Rc` values because the current Rc \
                drop path does not recursively drop owned contents or forward \
                arbitrary user-defined drop impls",
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
    fn validate_checker_output_contract_retains_channel_handles_and_prunes_other_ty_vars() {
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
        assert!(matches!(
            &fn_sigs["normalized_param_fn"].params[0],
            Ty::Named {
                builtin: Some(BuiltinType::Sender),
                args,
                ..
            } if args.len() == 1
        ));
        assert!(matches!(
            fn_sigs["normalized_return_fn"].return_type,
            Ty::Named {
                builtin: Some(BuiltinType::Receiver),
                ref args,
                ..
            } if args.len() == 1
        ));
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
    fn validate_checker_output_contract_retains_channel_handles_and_prunes_other_type_defs() {
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
            type_defs.contains_key("NormalizedHandles"),
            "synthetic bare channel handles must survive the output contract"
        );
        assert!(matches!(
            &type_defs["NormalizedHandles"].fields["tx"],
            Ty::Named {
                builtin: Some(BuiltinType::Sender),
                args,
                ..
            } if args.len() == 1
        ));
        assert!(matches!(
            &type_defs["NormalizedHandles"].variants["Recv"],
            VariantDef::Tuple(fields)
                if matches!(
                    fields.as_slice(),
                    [Ty::Named {
                        builtin: Some(BuiltinType::Receiver),
                        args,
                        ..
                    }] if args.len() == 1
                )
        ));
        assert!(matches!(
            &type_defs["NormalizedHandles"].methods["close"].params[0],
            Ty::Named {
                builtin: Some(BuiltinType::Sender),
                args,
                ..
            } if args.len() == 1
        ));
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
                type_params: vec!["T".to_string()],
                type_param_bounds: HashMap::new(),
                param_names: vec!["item".to_string()],
                params: vec![Ty::Named {
                    builtin: None,
                    name: "T".to_string(),
                    args: vec![],
                }],
                return_type: Ty::Unit,
                is_async: false,
                accepts_kwargs: false,
                doc_comment: None,
                extern_symbol: None,
                requires_mutable_receiver: false,
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

    fn make_enum(name: &str, variants: Vec<(&str, VariantDef)>) -> TypeDef {
        TypeDef {
            kind: TypeDefKind::Enum,
            name: name.to_string(),
            type_params: vec![],
            fields: HashMap::new(),
            field_order: vec![],
            variants: variants
                .into_iter()
                .map(|(name, variant)| (name.to_string(), variant))
                .collect(),
            methods: HashMap::new(),
            doc_comment: None,
            is_indirect: false,
        }
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
    fn primitive_copy_layout_string_returns_none() {
        // String is heap-managed; not a fixed-layout Copy type.
        assert_eq!(primitive_copy_layout(&Ty::String, &HashMap::new()), None);
    }

    #[test]
    fn hashmap_projection_element_gate_accepts_lowered_scalars_and_string() {
        let checker = Checker::new(ModuleRegistry::new(vec![]));
        for ty in [
            Ty::Bool,
            Ty::Char,
            Ty::I32,
            Ty::U32,
            Ty::I64,
            Ty::U64,
            Ty::F32,
            Ty::F64,
            Ty::String,
        ] {
            assert!(
                checker.is_supported_hashmap_projection_element_type(&ty),
                "expected `{}` to be admitted for HashMap projection",
                ty.user_facing()
            );
        }
    }

    #[test]
    fn hashmap_projection_element_gate_accepts_copy_record_and_enum() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        checker.type_defs.insert(
            "Point".to_string(),
            make_record("Point", vec![("x", Ty::I64), ("y", Ty::I64)]),
        );
        checker.type_defs.insert(
            "Direction".to_string(),
            make_enum(
                "Direction",
                vec![
                    ("North", VariantDef::Unit),
                    ("Delta", VariantDef::Tuple(vec![Ty::I64])),
                ],
            ),
        );
        checker
            .registry
            .register_type("Direction".to_string(), vec![Ty::I64]);

        for ty in [
            Ty::normalize_named("Point".to_string(), vec![]),
            Ty::normalize_named("Direction".to_string(), vec![]),
        ] {
            assert!(
                checker.is_supported_hashmap_projection_element_type(&ty),
                "expected `{}` to be admitted for HashMap projection",
                ty.user_facing()
            );
        }
    }

    #[test]
    fn hashmap_projection_element_gate_rejects_owned_aggregate_shapes() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        checker.type_defs.insert(
            "User".to_string(),
            make_record("User", vec![("name", Ty::String), ("id", Ty::I64)]),
        );
        checker.type_defs.insert(
            "Payload".to_string(),
            make_enum(
                "Payload",
                vec![(
                    "Chunk",
                    VariantDef::Tuple(vec![Ty::Named {
                        name: "Vec".to_string(),
                        args: vec![Ty::I64],
                        builtin: Some(BuiltinType::Vec),
                    }]),
                )],
            ),
        );
        checker.registry.register_type(
            "Payload".to_string(),
            vec![Ty::Named {
                name: "Vec".to_string(),
                args: vec![Ty::I64],
                builtin: Some(BuiltinType::Vec),
            }],
        );

        let record_ty = Ty::normalize_named("User".to_string(), vec![]);
        let enum_ty = Ty::normalize_named("Payload".to_string(), vec![]);
        let vec_ty = Ty::Named {
            name: "Vec".to_string(),
            args: vec![Ty::I64],
            builtin: Some(BuiltinType::Vec),
        };
        assert!(!checker.is_supported_hashmap_projection_element_type(&record_ty));
        assert!(!checker.is_supported_hashmap_projection_element_type(&enum_ty));
        assert!(!checker.is_supported_hashmap_projection_element_type(&vec_ty));
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
    fn compute_copy_record_layout_empty_record_returns_none() {
        let td = make_record("Empty", vec![]);
        assert_eq!(
            compute_copy_record_layout(&td, &HashMap::new()),
            None,
            "zero-field records must return None (zero-size ABI violation)"
        );
    }

    #[test]
    fn compute_copy_record_layout_single_i32_field() {
        // record Point { x: i32 }  →  size=4, align=4
        let td = make_record("Point", vec![("x", Ty::I32)]);
        assert_eq!(
            compute_copy_record_layout(&td, &HashMap::new()),
            Some((4, 4))
        );
    }

    #[test]
    fn compute_copy_record_layout_two_i32_fields() {
        // record Point { x: i32, y: i32 }  →  size=8, align=4
        let td = make_record("Point", vec![("x", Ty::I32), ("y", Ty::I32)]);
        assert_eq!(
            compute_copy_record_layout(&td, &HashMap::new()),
            Some((8, 4))
        );
    }

    #[test]
    fn compute_copy_record_layout_mixed_alignment_respects_padding() {
        // record Padded { a: bool, b: i32 }
        // Fields sorted alphabetically: a (bool,1,1) then b (i32,4,4)
        //   offset after a:  0+1 = 1
        //   offset after padding to align(4): 4
        //   offset after b:  4+4 = 8
        //   total size rounded to align(4): 8
        let td = make_record("Padded", vec![("a", Ty::Bool), ("b", Ty::I32)]);
        assert_eq!(
            compute_copy_record_layout(&td, &HashMap::new()),
            Some((8, 4))
        );
    }

    #[test]
    fn compute_copy_record_layout_declaration_order_is_respected() {
        // Both records have the same fields but in different declaration order.
        // Since `make_record` populates `field_order` from the Vec input,
        // these represent records declared with different field orderings.
        // This specific case (x:i8 then z:i64 vs z:i64 then x:i8) happens to
        // produce the same *size* — but the test confirms both variants are
        // handled correctly using their own field_order.
        let td1 = make_record("R", vec![("x", Ty::I8), ("z", Ty::I64)]);
        let td2 = make_record("R", vec![("z", Ty::I64), ("x", Ty::I8)]);
        // td1 declaration order [x, z]: x(1) at 0 → 1; z(8) align to 8 → 16; total=16
        // td2 declaration order [z, x]: z(8) at 0 → 8; x(1) at 8 → 9; total=align_up(9,8)=16
        // Both happen to produce size=16 (same struct size despite different order).
        assert_eq!(
            compute_copy_record_layout(&td1, &HashMap::new()),
            Some((16, 8))
        );
        assert_eq!(
            compute_copy_record_layout(&td2, &HashMap::new()),
            Some((16, 8))
        );
    }

    #[test]
    fn compute_copy_record_layout_declaration_order_diverges_from_alphabetical() {
        // record R { y: i32, z: i64, x: i32 }
        // Declaration order [y, z, x] vs alphabetical order [x, y, z] produce DIFFERENT sizes.
        //
        // Declaration order [y, z, x]:
        //   y(i32,4) at 0 → offset=4
        //   z(i64,8): align_up(4,8)=8 → offset=16  (4 bytes padding inserted)
        //   x(i32,4) at 16 → offset=20
        //   total = align_up(20,8) = 24; align=8
        //
        // Alphabetical order [x, y, z]:
        //   x(i32,4) at 0 → offset=4
        //   y(i32,4) at 4 → offset=8
        //   z(i64,8): align_up(8,8)=8 → offset=16  (no padding needed)
        //   total = align_up(16,8) = 16; align=8
        //
        // The divergence demonstrates why declaration order must be used to match codegen.
        let td_decl = make_record("R", vec![("y", Ty::I32), ("z", Ty::I64), ("x", Ty::I32)]);
        // Manually build a TypeDef with alphabetical field_order for comparison
        let td_alpha = {
            let fields = vec![("x", Ty::I32), ("y", Ty::I32), ("z", Ty::I64)];
            make_record("R", fields)
        };
        let decl_layout = compute_copy_record_layout(&td_decl, &HashMap::new());
        let alpha_layout = compute_copy_record_layout(&td_alpha, &HashMap::new());
        assert_eq!(
            decl_layout,
            Some((24, 8)),
            "declaration order [y,z,x] → size=24"
        );
        assert_eq!(
            alpha_layout,
            Some((16, 8)),
            "alphabetical order [x,y,z] → size=16"
        );
        assert_ne!(
            decl_layout, alpha_layout,
            "declaration order and alphabetical order must diverge for this field combination"
        );
    }

    #[test]
    fn finalize_hashmap_named_key_eligible_scalar_value_produces_layout_fact() {
        use crate::lowering_facts::{HashMapAbi, HashMapLoweringFactState};
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 10..20;
        // Register a simple Copy record: record Point { x: i32, y: i32 }
        checker.type_defs.insert(
            "Point".to_string(),
            make_record("Point", vec![("x", Ty::I32), ("y", Ty::I32)]),
        );
        // Insert a deferred entry: HashMap<Point, i64>
        checker.deferred_hashmap_admission.insert(
            SpanKey::in_module(&span, 0),
            DeferredHashMapAdmission {
                span: span.clone(),
                key_ty: Ty::normalize_named("Point".to_string(), vec![]),
                val_ty: Ty::I64,
                source_module: None,
            },
        );
        checker.finalize_hashmap_admission();
        assert!(
            checker.errors.is_empty(),
            "eligible Named key with i64 value must produce no errors; got: {:?}",
            checker.errors
        );
        let key = SpanKey::in_module(&span, 0);
        let fact = checker.hashmap_layout_facts.get(&key).expect(
            "finalize_hashmap_admission must produce a HashMapLoweringFact for eligible Named key",
        );
        assert_eq!(
            fact.state,
            HashMapLoweringFactState::Pending,
            "produced fact must be in Pending state"
        );
        assert!(
            matches!(&fact.abi, HashMapAbi::LayoutKey { key_record_name, .. } if key_record_name == "Point"),
            "abi must be LayoutKey with key_record_name == Point; got: {:?}",
            fact.abi
        );
        // Point has two i32 fields → size=8, align=4
        assert_eq!(
            fact.key_size,
            Some(8),
            "key_size must be Some(8) for two-i32 record"
        );
        assert_eq!(
            fact.key_align,
            Some(4),
            "key_align must be Some(4) for two-i32 record"
        );
    }

    #[test]
    fn hashmap_source_record_key_i64_fields_is_hash_eligible() {
        let source = r"
            record Point { x: i64, y: i64 }
            fn main() {
                let m: HashMap<Point, i64> = HashMap::new();
                m.insert(Point { x: 1, y: 2 }, 10);
            }
        ";
        let parsed = hew_parser::parse(source);
        assert!(
            parsed.errors.is_empty(),
            "parse errors: {:?}",
            parsed.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&parsed.program);
        let point_def = checker.type_defs.get("Point").cloned();
        assert!(
            matches!(
                point_def.as_ref().and_then(|td| td.fields.get("x")),
                Some(Ty::I64)
            ),
            "Point.x should register as i64; got: {point_def:?}"
        );
        assert!(
            output.errors.is_empty(),
            "HashMap<Point, i64> with i64 record fields must be admitted; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn finalize_hashmap_named_key_float_field_emits_error() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 50..60;
        // record Bad { x: f64 } — ineligible due to float
        checker
            .type_defs
            .insert("Bad".to_string(), make_record("Bad", vec![("x", Ty::F64)]));
        checker.deferred_hashmap_admission.insert(
            SpanKey::in_module(&span, 0),
            DeferredHashMapAdmission {
                span: span.clone(),
                key_ty: Ty::normalize_named("Bad".to_string(), vec![]),
                val_ty: Ty::I64,
                source_module: None,
            },
        );
        checker.finalize_hashmap_admission();
        assert!(
            !checker.errors.is_empty(),
            "float-field key must produce a diagnostic"
        );
        assert!(
            checker.hashmap_layout_facts.is_empty(),
            "no layout fact must be produced for ineligible key"
        );
    }

    #[test]
    fn finalize_hashset_named_elem_eligible_produces_layout_fact() {
        use crate::lowering_facts::{HashMapLoweringFactState, HashSetAbi};
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 70..80;
        // record Point { x: i32, y: i32 }
        checker.type_defs.insert(
            "Point".to_string(),
            make_record("Point", vec![("x", Ty::I32), ("y", Ty::I32)]),
        );
        // Simulate record_hashset_lowering_fact adding to pending_lowering_facts
        // (the Named path is admitted inline, then finalize_lowering_facts is called).
        // We bypass record_hashset_lowering_fact and inject directly into pending_lowering_facts.
        checker.pending_lowering_facts.insert(
            SpanKey::in_module(&span, 0),
            crate::check::types::PendingLoweringFact {
                hashset_element_ty: Ty::normalize_named("Point".to_string(), vec![]),
                source_module: None,
            },
        );
        let _result = checker.finalize_lowering_facts();
        assert!(
            checker.errors.is_empty(),
            "eligible Named element must produce no errors; got: {:?}",
            checker.errors
        );
        let key = SpanKey::in_module(&span, 0);
        let fact = checker.hashset_layout_facts.get(&key).expect(
            "finalize_lowering_facts must produce a HashSetLoweringFact for eligible Named element",
        );
        assert_eq!(
            fact.state,
            HashMapLoweringFactState::Pending,
            "produced fact must be in Pending state"
        );
        assert!(
            matches!(&fact.abi, HashSetAbi::Layout { elem_record_name } if elem_record_name == "Point"),
            "abi must be Layout with elem_record_name == Point; got: {:?}",
            fact.abi
        );
        assert_eq!(
            fact.elem_size,
            Some(8),
            "elem_size must be Some(8) for two-i32 record"
        );
        assert_eq!(
            fact.elem_align,
            Some(4),
            "elem_align must be Some(4) for two-i32 record"
        );
    }

    #[test]
    fn finalize_hashset_named_elem_float_field_emits_error() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 90..100;
        // record Bad { v: f32 }
        checker
            .type_defs
            .insert("Bad".to_string(), make_record("Bad", vec![("v", Ty::F32)]));
        checker.pending_lowering_facts.insert(
            SpanKey::in_module(&span, 0),
            crate::check::types::PendingLoweringFact {
                hashset_element_ty: Ty::normalize_named("Bad".to_string(), vec![]),
                source_module: None,
            },
        );
        let _result = checker.finalize_lowering_facts();
        assert!(
            !checker.errors.is_empty(),
            "float-field element must produce a diagnostic"
        );
        assert!(
            checker.hashset_layout_facts.is_empty(),
            "no layout fact must be produced for ineligible element"
        );
    }

    // ── Additional admissibility tests requested in cross-eco review ────────────

    #[test]
    fn hashmap_string_field_key_rejected() {
        // record K { s: string } — string field makes K ineligible (IneligibleManaged)
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 1..10;
        let mut td = make_record("K", vec![("s", Ty::String)]);
        td.field_order = vec!["s".to_string()];
        checker.type_defs.insert("K".to_string(), td);
        checker.deferred_hashmap_admission.insert(
            SpanKey::in_module(&span, 0),
            DeferredHashMapAdmission {
                span: span.clone(),
                key_ty: Ty::normalize_named("K".to_string(), vec![]),
                val_ty: Ty::I64,
                source_module: None,
            },
        );
        checker.finalize_hashmap_admission();
        assert!(
            !checker.errors.is_empty(),
            "string-field key must be rejected; errors: {:?}",
            checker.errors
        );
        assert!(
            checker.hashmap_layout_facts.is_empty(),
            "no layout fact must be produced for string-field key"
        );
        // Diagnostic must name the field or type clearly
        let msg = &checker.errors[0].message;
        assert!(
            msg.contains('K') || msg.contains("string"),
            "error must reference the type or field kind; got: {msg:?}"
        );
    }

    #[test]
    fn hashmap_managed_key_rejected() {
        // record Handle — is_indirect = true makes it IneligibleManaged
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 2..11;
        let mut td = make_record("Handle", vec![("fd", Ty::I32)]);
        td.is_indirect = true;
        checker.type_defs.insert("Handle".to_string(), td);
        checker.deferred_hashmap_admission.insert(
            SpanKey::in_module(&span, 0),
            DeferredHashMapAdmission {
                span: span.clone(),
                key_ty: Ty::normalize_named("Handle".to_string(), vec![]),
                val_ty: Ty::I64,
                source_module: None,
            },
        );
        checker.finalize_hashmap_admission();
        assert!(
            !checker.errors.is_empty(),
            "indirect/managed key must be rejected; errors: {:?}",
            checker.errors
        );
        assert!(
            checker.hashmap_layout_facts.is_empty(),
            "no layout fact for indirect key"
        );
    }

    #[test]
    fn hashmap_enum_key_rejected() {
        // type Color = Enum — Enum kind must be rejected (IneligibleNamedNonRecord)
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 3..12;
        let td = TypeDef {
            kind: TypeDefKind::Enum,
            name: "Color".to_string(),
            type_params: vec![],
            fields: HashMap::new(),
            field_order: vec![],
            variants: HashMap::new(),
            methods: HashMap::new(),
            doc_comment: None,
            is_indirect: false,
        };
        checker.type_defs.insert("Color".to_string(), td);
        checker.deferred_hashmap_admission.insert(
            SpanKey::in_module(&span, 0),
            DeferredHashMapAdmission {
                span: span.clone(),
                key_ty: Ty::normalize_named("Color".to_string(), vec![]),
                val_ty: Ty::I64,
                source_module: None,
            },
        );
        checker.finalize_hashmap_admission();
        assert!(
            !checker.errors.is_empty(),
            "Enum key must be rejected; errors: {:?}",
            checker.errors
        );
        assert!(
            checker.hashmap_layout_facts.is_empty(),
            "no layout fact for Enum key"
        );
    }

    #[test]
    fn hashmap_layout_key_with_layout_value_record_admitted() {
        // HashMap<Point, Pos> — both key and value are Copy named records
        use crate::lowering_facts::{HashMapAbi, HashMapLoweringFactState, HashMapValueType};
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 4..13;
        // record Point { x: i32, y: i32 }
        checker.type_defs.insert(
            "Point".to_string(),
            make_record("Point", vec![("x", Ty::I32), ("y", Ty::I32)]),
        );
        // record Pos { lat: i64, lon: i64 }
        checker.type_defs.insert(
            "Pos".to_string(),
            make_record("Pos", vec![("lat", Ty::I64), ("lon", Ty::I64)]),
        );
        checker.deferred_hashmap_admission.insert(
            SpanKey::in_module(&span, 0),
            DeferredHashMapAdmission {
                span: span.clone(),
                key_ty: Ty::normalize_named("Point".to_string(), vec![]),
                val_ty: Ty::normalize_named("Pos".to_string(), vec![]),
                source_module: None,
            },
        );
        checker.finalize_hashmap_admission();
        assert!(
            checker.errors.is_empty(),
            "eligible Named key + Named value must produce no errors; got: {:?}",
            checker.errors
        );
        let key = SpanKey::in_module(&span, 0);
        let fact = checker
            .hashmap_layout_facts
            .get(&key)
            .expect("layout key + layout value must produce a HashMapLoweringFact");
        assert_eq!(fact.state, HashMapLoweringFactState::Pending);
        assert!(
            matches!(
                &fact.abi,
                HashMapAbi::LayoutKey { key_record_name, val: HashMapValueType::Layout }
                    if key_record_name == "Point"
            ),
            "abi must be LayoutKey with val=Layout; got: {:?}",
            fact.abi
        );
        // Point has two i32 fields → size=8, align=4
        assert_eq!(fact.key_size, Some(8));
        assert_eq!(fact.key_align, Some(4));
        // Pos has two i64 fields → size=16, align=8
        assert_eq!(fact.val_size, Some(16));
        assert_eq!(fact.val_align, Some(8));
    }

    #[test]
    fn hashset_indirect_record_rejected() {
        // record Handle — is_indirect=true element must be rejected
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 5..14;
        let mut td = make_record("Handle", vec![("fd", Ty::I32)]);
        td.is_indirect = true;
        checker.type_defs.insert("Handle".to_string(), td);
        checker.pending_lowering_facts.insert(
            SpanKey::in_module(&span, 0),
            crate::check::types::PendingLoweringFact {
                hashset_element_ty: Ty::normalize_named("Handle".to_string(), vec![]),
                source_module: None,
            },
        );
        let _result = checker.finalize_lowering_facts();
        assert!(
            !checker.errors.is_empty(),
            "indirect/managed element must be rejected; errors: {:?}",
            checker.errors
        );
        assert!(
            checker.hashset_layout_facts.is_empty(),
            "no layout fact for indirect element"
        );
    }

    #[test]
    fn hashset_string_element_still_uses_string_abi() {
        // Regression: HashSet<String> must still produce LoweringFact with String ABI.
        use crate::lowering_facts::HashSetAbi;
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 6..15;
        checker.pending_lowering_facts.insert(
            SpanKey::in_module(&span, 0),
            crate::check::types::PendingLoweringFact {
                hashset_element_ty: Ty::String,
                source_module: None,
            },
        );
        let lowering_facts = checker.finalize_lowering_facts();
        assert!(
            checker.errors.is_empty(),
            "String element must produce no errors; got: {:?}",
            checker.errors
        );
        let key = SpanKey::in_module(&span, 0);
        let fact = lowering_facts
            .get(&key)
            .expect("finalize_lowering_facts must produce a LoweringFact for String element");
        assert_eq!(
            fact.abi_variant,
            HashSetAbi::String,
            "String element must produce String ABI; got: {:?}",
            fact.abi_variant
        );
        assert!(
            checker.hashset_layout_facts.is_empty(),
            "String element must NOT produce a HashSetLoweringFact (uses scalar path)"
        );
    }

    #[test]
    fn hashset_i64_element_still_uses_int64_abi() {
        // Regression: HashSet<i64> must still produce LoweringFact with Int64 ABI.
        use crate::lowering_facts::HashSetAbi;
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 7..16;
        checker.pending_lowering_facts.insert(
            SpanKey::in_module(&span, 0),
            crate::check::types::PendingLoweringFact {
                hashset_element_ty: Ty::I64,
                source_module: None,
            },
        );
        let lowering_facts = checker.finalize_lowering_facts();
        assert!(
            checker.errors.is_empty(),
            "I64 element must produce no errors; got: {:?}",
            checker.errors
        );
        let key = SpanKey::in_module(&span, 0);
        let fact = lowering_facts
            .get(&key)
            .expect("finalize_lowering_facts must produce a LoweringFact for I64 element");
        assert_eq!(
            fact.abi_variant,
            HashSetAbi::Int64,
            "I64 element must produce Int64 ABI; got: {:?}",
            fact.abi_variant
        );
        assert!(
            checker.hashset_layout_facts.is_empty(),
            "I64 element must NOT produce a HashSetLoweringFact (uses scalar path)"
        );
    }

    #[test]
    fn vec_array_element_rejected_at_checker_not_codegen() {
        // Regression guard: Vec<[i64; 2]> has a copy layout, but codegen cannot
        // lower array/composite Vec elements (Cluster 2 deferred).  Before this
        // fix the `&& !has_copy_layout` exception caused copy-layout arrays to
        // slip through the type checker and fail with an unspanned codegen error.
        // The checker must produce a spanned error regardless of copy layout.
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 10..24;
        let array_i64_2 = Ty::Array(Box::new(Ty::I64), 2);
        let result = checker.validate_vec_element_type(&array_i64_2, &span);
        assert!(
            !result,
            "Vec<[i64; 2]> element must be rejected at the checker"
        );
        assert!(
            !checker.errors.is_empty(),
            "Vec<[i64; 2]> element must emit a checker error; errors: {:?}",
            checker.errors
        );
        // The error must carry the source span.
        let err = &checker.errors[0];
        assert_eq!(
            err.span.start, 10,
            "checker error must carry the source span start; err: {err:?}"
        );
    }
}
