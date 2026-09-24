//! Split from `methods.rs`: checker methods, part 3 of 5.
#![allow(
    unused_imports,
    redundant_imports,
    clippy::wildcard_imports,
    reason = "chunk files share the parent module's import header"
)]
#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::super::*;
use super::*;
use crate::builtin_names::BuiltinNamedType;
use crate::check::calls::SignatureArgApplication;
use crate::check::dispatch::resolve_method_call;
use crate::check::types::GenericCallee;
use crate::check::types::{BareActorResolution, DeferredBuiltinCloneAdmission, DeferredWireCodec};
use crate::method_resolution::{
    collect_method_sigs_for_receiver, instantiate_stdlib_method_sig, lookup_builtin_method_sig,
    lookup_named_method_sig as shared_lookup_named_method_sig,
};
use crate::runtime_call::{FloatMethodOp, IntArithKind, IntBitOp, IntMethodWidth};
use crate::stdlib::{STD_NET_CONNECTION, STD_NET_LISTENER};
use crate::BuiltinType;

impl Checker {
    pub(in crate::check) fn check_rc_method(
        &mut self,
        type_args: &[Ty],
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        let inner_ty = type_args
            .first()
            .cloned()
            .unwrap_or(Ty::Var(TypeVar::fresh()));
        let record = |checker: &mut Self, op| {
            checker.record_rc_intrinsic(span, op, &inner_ty);
        };
        match method {
            // rc.clone() increments the reference count and returns a new Rc<T>
            "clone" => {
                self.check_arity(args, 0, "`Rc.clone`", span);
                record(self, RcIntrinsicOp::Clone);
                Ty::rc(inner_ty)
            }
            // rc.get() copies the inner value out of the Rc.
            // `LoadOp` performs a bitwise copy, which is only sound for `Copy`
            // types (no ownership to duplicate).  For non-Copy `T`, callers
            // share access via `rc.clone()` instead.
            "get" => {
                self.check_arity(args, 0, "`Rc.get`", span);
                if !self
                    .registry
                    .implements_marker(&inner_ty, MarkerTrait::Copy)
                {
                    self.report_error(
                        TypeErrorKind::BoundsNotSatisfied,
                        span,
                        format!(
                            "`Rc.get` requires `T: Copy`; `{}` is not `Copy` — \
                             use `rc.clone()` to share the reference instead",
                            inner_ty.user_facing()
                        ),
                    );
                    return Ty::Error;
                }
                record(self, RcIntrinsicOp::GetCopy);
                inner_ty
            }
            "set" => {
                self.check_arity(args, 1, "`Rc.set`", span);
                if let Some(arg) = args.first() {
                    let (expr, arg_span) = arg.expr();
                    let arg_ty = self.synthesize(expr, arg_span);
                    self.expect_type(&inner_ty, &arg_ty, arg_span);
                    self.reject_borrowed_parameter_consumption(expr, arg_span, "Rc.set");
                }
                record(self, RcIntrinsicOp::Set);
                Ty::Unit
            }
            "downgrade" => {
                self.check_arity(args, 0, "`Rc.downgrade`", span);
                record(self, RcIntrinsicOp::Downgrade);
                Ty::weak(inner_ty)
            }
            // rc.strong_count() returns the current reference count as i64
            "strong_count" => {
                self.check_arity(args, 0, "`Rc.strong_count`", span);
                record(self, RcIntrinsicOp::StrongCount);
                Ty::I64
            }
            "weak_count" => {
                self.check_arity(args, 0, "`Rc.weak_count`", span);
                record(self, RcIntrinsicOp::WeakCount);
                Ty::I64
            }
            "is_unique" => {
                self.check_arity(args, 0, "`Rc.is_unique`", span);
                record(self, RcIntrinsicOp::IsUnique);
                Ty::Bool
            }
            _ => {
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on `Rc<{}>`", inner_ty.user_facing()),
                );
                Ty::Error
            }
        }
    }

    pub(in crate::check) fn check_weak_method(
        &mut self,
        type_args: &[Ty],
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        let inner_ty = type_args
            .first()
            .cloned()
            .unwrap_or(Ty::Var(TypeVar::fresh()));
        match method {
            "clone" => {
                self.check_arity(args, 0, "`Weak.clone`", span);
                self.record_rc_intrinsic(span, RcIntrinsicOp::WeakClone, &inner_ty);
                Ty::weak(inner_ty)
            }
            "upgrade" => {
                self.check_arity(args, 0, "`Weak.upgrade`", span);
                self.record_rc_intrinsic(span, RcIntrinsicOp::WeakUpgrade, &inner_ty);
                Ty::option(Ty::rc(inner_ty))
            }
            _ => {
                for arg in args {
                    let (expr, arg_span) = arg.expr();
                    self.synthesize(expr, arg_span);
                }
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on `Weak<{}>`", inner_ty.user_facing()),
                );
                Ty::Error
            }
        }
    }

    pub(super) fn record_rc_intrinsic(&mut self, span: &Span, op: RcIntrinsicOp, payload_ty: &Ty) {
        let resolved = self
            .subst
            .resolve(payload_ty)
            .materialize_literal_defaults();
        match ResolvedTy::from_ty(&resolved) {
            Ok(payload_ty) => self.record_method_call_rewrite(
                span,
                MethodCallRewrite::RcIntrinsic { op, payload_ty },
            ),
            Err(_) => self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                "Rc/Weak operations require a concrete payload type".to_string(),
            ),
        }
    }

    pub(super) fn report_vec_contains_layout_equality_gate(
        &mut self,
        elem_ty: &Ty,
        eligibility: crate::eq_eligibility::EqEligibility,
        span: &Span,
    ) {
        use crate::eq_eligibility::EqEligibility;

        let reason = match eligibility {
            EqEligibility::Eligible => format!(
                "`Vec.contains` on layout-backed element type `{}` is equality-eligible, \
                 but layout contains is not yet supported for this element type",
                elem_ty.user_facing()
            ),
            EqEligibility::IneligibleManaged(managed_ty) => format!(
                "`Vec.contains` on layout-backed element type `{}` requires aggregate \
                 equality, but `{}` is layout-managed/non-Copy data",
                elem_ty.user_facing(),
                managed_ty.user_facing()
            ),
            EqEligibility::IneligibleOwned(owned_ty) => format!(
                "`Vec.contains` on layout-backed element type `{}` requires aggregate \
                 equality, but `{}` is owned or heap-backed data",
                elem_ty.user_facing(),
                owned_ty.user_facing()
            ),
            EqEligibility::IneligibleUnknown => format!(
                "`Vec.contains` on layout-backed element type `{}` requires aggregate \
                 equality, but equality eligibility is unknown",
                elem_ty.user_facing()
            ),
        };

        self.report_error(
            TypeErrorKind::InvalidOperation,
            span,
            format!(
                "{reason}; no runtime method rewrite was recorded, and layout Vec contains \
                 remains fail-closed"
            ),
        );
    }

    /// Channel/stream element admission for the layout-witness queue path
    /// (`Sender<T>`/`Receiver<T>`/`Stream<T>` recv/send). An element is
    /// admissible when the codegen element witness can describe it:
    ///
    /// - `string` / `bytes` — content-encoded queue envelopes;
    /// - Copy-eligible primitives and `BitCopy` records ([`primitive_copy_layout`]
    ///   resolves a fixed width) — Plain raw-representation envelopes;
    /// - heap-owning value types the §1.1 class rule gives an ownership
    ///   obligation ([`Checker::element_owns_heap`] — the same class the
    ///   backend reads for the element's clone and destroy actions, so the
    ///   checker and the witness cannot disagree about one element type);
    /// - monomorphic machine values — machines are tagged-union value types
    ///   whose state-variant layout is registered in `type_defs.variants`,
    ///   so the owned-element queue witness can describe them (with the same
    ///   no-unowned-container requirement as enum channel elements).
    ///   Generic machine instantiations are excluded (the substrate
    ///   canonicalizes to one bare-named layout; per-instantiation witnesses
    ///   do not exist).
    ///
    /// Everything else fails closed: builtin container/handle nominals
    /// (`Vec`/`HashMap`/streams/channels/pids), closures, and any type
    /// without a clone/drop thunk path. `BitCopy` enums ride the
    /// owned-element authority's record/enum admission and are lowered
    /// Plain by the witness (no heap leaf → no thunks), which is the
    /// correct Copy semantics.
    pub(in crate::check) fn queue_elem_admissible(&self, elem_ty: &Ty) -> bool {
        match elem_ty {
            // String/bytes are content-encoded envelopes; unconstrained
            // numeric literals default to i64/f64 (Plain 8-byte envelopes)
            // at literal-defaulting time, so a queue element constrained
            // only by a literal (`tx.send(42)`) must not be rejected
            // before defaulting runs.
            Ty::String | Ty::Bytes | Ty::IntLiteral | Ty::FloatLiteral => true,
            // Monomorphic machine values travel the owned-element queue
            // witness: machines are tagged-union value types registered in
            // `type_defs.variants`, satisfying the same thunk-path
            // requirements as enums. Generic machine instantiations are
            // refused (canonicalised to one bare-named decl layout; no
            // per-instantiation witness exists).
            Ty::Named {
                name,
                builtin,
                args,
            } if builtin.is_none() => {
                if let Some(type_def) = self.type_defs.get(name) {
                    if matches!(type_def.kind, TypeDefKind::Machine) {
                        // Generic instantiation: no per-instantiation layout.
                        if !args.is_empty() {
                            return false;
                        }
                        // Monomorphic: apply the same no-unowned-container
                        // requirement as for enum channel elements.
                        return !self.queue_element_holds_collection(
                            elem_ty,
                            &HashSet::new(),
                            &mut HashSet::new(),
                        );
                    }
                }
                self.queue_element_describable(elem_ty)
            }
            // Builtin container/handle nominals (`Vec`/`HashMap`/`HashSet`/
            // `Rc`/handles/...) can never ride the element-layout queue
            // witness: their ownership lives in a runtime context the queue
            // cannot clone or drop. This stays in lockstep with
            // `queue_elem_rejection_reason`, which rejects every `builtin:
            // Some(_)`. A nested-container Vec ELEMENT is admitted for
            // copy-in push, but that is a Vec-storage property, not a queue
            // property, and must not leak here. Primitives (`i64`/`bool`/`char`/...) are dedicated `Ty`
            // variants (not `Ty::Named`), so they remain queue-admissible via
            // the `_` arm's `primitive_copy_layout` check.
            //
            // A callable owns a heap environment the envelope has no ingress
            // for, which `queue_elem_rejection_reason` states in the same
            // words; every other shape is admitted on its value class.
            Ty::Named {
                builtin: Some(_), ..
            }
            | Ty::Function { .. }
            | Ty::Closure { .. } => false,
            _ => self.queue_element_describable(elem_ty),
        }
    }

    /// Can the element-layout queue witness describe this element?
    ///
    /// It can when the element has a value class at all — the class carries the
    /// clone and destroy actions the envelope needs — and holds no builtin
    /// collection. A type with no class (an abstract parameter among them) has
    /// no witness either and stays fail-closed here; the collection rule is the
    /// mailbox's own, stated on [`Self::queue_element_holds_collection`].
    pub(super) fn queue_element_describable(&self, elem_ty: &Ty) -> bool {
        self.element_value_facts(elem_ty).is_ok()
            && !self.queue_element_holds_collection(elem_ty, &HashSet::new(), &mut HashSet::new())
    }

    /// Explain why a channel/stream element type was rejected by
    /// [`Self::queue_elem_admissible`], for the fail-closed diagnostic.
    /// Completes "`{Container}<X>` is not supported: {clause}".
    pub(in crate::check) fn queue_elem_rejection_reason(&self, elem_ty: &Ty) -> String {
        if let Ty::Named {
            builtin: Some(_), ..
        } = elem_ty
        {
            return "builtin container and handle types cannot ride the \
                    element-layout queue witness; their ownership lives in a \
                    runtime context the queue cannot clone or drop"
                .to_string();
        }
        if matches!(elem_ty, Ty::Function { .. } | Ty::Closure { .. }) {
            return "function values cannot be queue elements".to_string();
        }
        if self.queue_element_holds_collection(elem_ty, &HashSet::new(), &mut HashSet::new()) {
            return "it holds a `Vec`/`HashMap`/`HashSet` field, and the mailbox envelope \
                    has no per-message release for one"
                .to_string();
        }
        self.element_admission_refusal(elem_ty).map_or_else(
            || "it has no value class the queue witness can describe".to_string(),
            |(_, refusal)| refusal,
        )
    }

    /// True when `ty` (or a transitive record/enum member) is — or holds a
    /// field of — a builtin collection (`Vec`/`HashMap`/`HashSet`).
    ///
    /// This is a MAILBOX rule, not a value-class one: the envelope deep-copies
    /// an element in but has no per-message release that recurses through a
    /// collection field, so a collection-bearing message would leak the field
    /// on every send. Vec storage admits the same shape (the collection's own
    /// destroy action releases it); the queue does not, until the mailbox
    /// release path recurses. The recursive enum's own self-edge through a
    /// `Vec` (`Array(Vec<RedisReply>)`) is the one admitted exception.
    pub(super) fn queue_element_holds_collection(
        &self,
        ty: &Ty,
        roots: &HashSet<String>,
        visiting: &mut HashSet<String>,
    ) -> bool {
        match ty {
            Ty::Named {
                name,
                builtin,
                args,
            } => {
                if matches!(
                    builtin,
                    Some(BuiltinType::Vec | BuiltinType::HashMap | BuiltinType::HashSet)
                ) {
                    // A collection whose every type argument is a `root` (the
                    // recursing element type) is the admitted self-recursion
                    // (`enum R { A(Vec<R>) }`): the enum's own owned thunk
                    // recurses through this field. Any other container element
                    // is unowned (no thunk path) — reject.
                    return !args.iter().all(|a| match a {
                        Ty::Named { name: an, .. } => roots.contains(an),
                        _ => false,
                    });
                }
                if builtin.is_some() {
                    // Other builtins (Option/Result/Rc/handles) carry their own
                    // ABI; recurse only through their type arguments.
                    return args
                        .iter()
                        .any(|a| self.queue_element_holds_collection(a, roots, visiting));
                }
                if !visiting.insert(name.clone()) {
                    // Self-recursive edge on a user type: the recursion through a
                    // user record/enum is finite by construction here (it only
                    // recurses once per name). It carries no bare container.
                    return false;
                }
                let result = self.type_defs.get(name).is_some_and(|td| {
                    td.fields
                        .values()
                        .any(|fty| self.queue_element_holds_collection(fty, roots, visiting))
                        || td.variants.values().any(|variant| match variant {
                            VariantDef::Unit => false,
                            VariantDef::Tuple(tys) => tys
                                .iter()
                                .any(|t| self.queue_element_holds_collection(t, roots, visiting)),
                            VariantDef::Struct(fields) => fields.iter().any(|(_, t)| {
                                self.queue_element_holds_collection(t, roots, visiting)
                            }),
                        })
                });
                visiting.remove(name);
                result
            }
            Ty::Tuple(elems) => elems
                .iter()
                .any(|e| self.queue_element_holds_collection(e, roots, visiting)),
            Ty::Array(inner, _) | Ty::Slice(inner) => {
                self.queue_element_holds_collection(inner, roots, visiting)
            }
            _ => false,
        }
    }

    pub(super) fn check_runtime_vec_method_from_source(
        &mut self,
        type_args: &[Ty],
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Option<Ty> {
        let sig = self.lookup_builtin_vec_method_sig(type_args, method)?;
        sig.extern_symbol.as_ref()?;

        if matches!(method, "push" | "pop" | "remove" | "clear" | "clone") {
            self.check_arity(args, sig.params.len(), &format!("`Vec.{method}`"), span);
        }

        for (index, expected) in sig.params.iter().enumerate() {
            let Some(arg) = args.get(index) else {
                continue;
            };
            let (expr, arg_span) = arg.expr();
            if matches!(method, "set" | "remove") && index == 0 {
                let actual = self.synthesize(expr, arg_span);
                let resolved = self.subst.resolve(&actual);
                if Self::is_narrower_signed_int(&resolved) {
                    self.numeric_operand_coercions.insert(
                        SpanKey::in_module(arg_span, self.current_module_idx),
                        Ty::I64,
                    );
                    continue;
                }
            }
            self.check_against(expr, arg_span, expected);
            // A value with no copy operation enters the collection by
            // transfer: the slot becomes its only owner, so a later use of the
            // source binding is a use after the move.
            self.record_value_transfer(expr, arg_span);
        }

        let elem_ty = type_args
            .first()
            .map_or_else(|| Ty::Var(TypeVar::fresh()), |ty| self.subst.resolve(ty));
        self.record_resolved_vec_call(method, &elem_ty, span);
        Some(sig.return_type)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "Vec keeps divergent contains/join/map/filter/fold arms inline; runtime-backed signatures delegate to the stdlib-source authority."
    )]
    pub(in crate::check) fn check_vec_method(
        &mut self,
        type_args: &[Ty],
        _receiver_ty: &Ty,
        resolved: &Ty,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        let elem_ty = type_args
            .first()
            .cloned()
            .unwrap_or(Ty::Var(TypeVar::fresh()));
        let _ = self.validate_vec_element_type(&elem_ty, span);
        let runtime_method_declared = self
            .lookup_builtin_vec_method_sig(type_args, method)
            .is_some();
        if method == "clone" && matches!(self.subst.resolve(&elem_ty), Ty::TraitObject { .. }) {
            self.check_arity(args, 0, "`Vec.clone`", span);
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                "`Vec<dyn Trait>.clone()` is not supported because trait objects have no \
                 semantic clone operation; use `into_iter()` to transfer the existing owners"
                    .to_string(),
            );
            return Ty::Error;
        }
        // A pipe half is cloned one handle at a time: `sink.clone()` retains
        // the pipe's handle count, and no Vec clone recipe duplicates one.
        if method == "clone"
            && matches!(
                self.subst.resolve(&elem_ty),
                Ty::Named {
                    builtin: Some(builtin),
                    ..
                } if builtin.is_pipe_half()
            )
        {
            self.check_arity(args, 0, "`Vec.clone`", span);
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!(
                    "`Vec<{}>.clone()` is not supported: a pipe half is cloned one handle at a \
                     time with `.clone()` on a `Sink`, and a `Stream` has one consumer",
                    elem_ty.user_facing()
                ),
            );
            return Ty::Error;
        }
        let result = match method {
            "into_iter" => {
                self.check_arity(args, 0, "`Vec.into_iter`", span);
                let resolved_elem = self.subst.resolve(&elem_ty);
                if !self.record_vec_iter_element_mode(&resolved_elem, span) {
                    return Ty::Error;
                }
                self.record_method_call_receiver_kind(
                    span,
                    MethodCallReceiverKind::PrimitiveTraitImpl {
                        trait_name: "IntoIterator".to_string(),
                        canonical_receiver: "Vec".to_string(),
                    },
                );
                self.record_method_call_rewrite(span, MethodCallRewrite::BuiltinVecIntoIter);
                Ty::builtin_named(BuiltinType::VecIter, vec![resolved_elem])
            }
            "iter" => {
                // An ordinary value transfer creates the independent snapshot.
                // Record the operation now and consume the finalized receiver
                // type at the HIR boundary, after inference has completed.
                self.check_arity(args, 0, "`Vec.iter`", span);
                let resolved_elem = self.subst.resolve(&elem_ty);
                if matches!(resolved_elem, Ty::TraitObject { .. }) {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        "`Vec<dyn Trait>.iter()` is not supported because a borrowed iterator \
                         needs an independent clone of each trait object; use `into_iter()` to \
                         transfer the existing owners"
                            .to_string(),
                    );
                    return Ty::Error;
                }
                if !self.record_vec_iter_element_mode(&resolved_elem, span) {
                    return Ty::Error;
                }
                self.record_method_call_rewrite(span, MethodCallRewrite::BuiltinVecIter);
                Ty::builtin_named(BuiltinType::VecIter, vec![resolved_elem])
            }
            "get" if runtime_method_declared => {
                // Trait-routed `Index<i64>` accessor: `<Vec<T> as Index>::get
                // -> Option<T>`. Dispatch is marked as the `Index` primitive
                // trait impl and lowered to the element-agnostic
                // `hew_vec_get_clone` intrinsic — the SINGLE fresh-owner choke
                // point that writes a retained/cloned owner into the `Option`
                // payload (drop-safe; `by-value-heap-params-are-borrows` P0).
                // `get` is intentionally NOT in `collection_method_desc`: this
                // explicit arm preserves the index-site widening ergonomic
                // (i8/i16/i32 → i64) that the strict trait signature would
                // otherwise reject.
                self.check_arity(args, 1, "`Vec.get`", span);
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    let actual = self.synthesize(expr, sp);
                    let resolved_idx = self.subst.resolve(&actual);
                    // Accept i64 or any narrower signed int (widened at the
                    // index site, identical to `[]`/`set`/`remove`); otherwise
                    // run the normal i64 coercion (literals, error wording).
                    if Self::is_narrower_signed_int(&resolved_idx) {
                        self.numeric_operand_coercions
                            .insert(SpanKey::in_module(sp, self.current_module_idx), Ty::I64);
                    } else {
                        self.check_against(expr, sp, &Ty::I64);
                    }
                }
                let resolved_elem = self.subst.resolve(&elem_ty);
                // A trait object stays refused: dispatching on a loaned `dyn`
                // payload has no working lowering yet, so the consuming
                // iterator remains the trait-object surface.
                if matches!(resolved_elem, Ty::TraitObject { .. }) {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        "`Vec<dyn Trait>.get()` is not supported because returning an owned \
                         element would require a semantic trait-object clone; use pop/remove or \
                         consuming iteration to move an owner out"
                            .to_string(),
                    );
                    return Ty::Error;
                }
                self.record_method_call_receiver_kind(
                    span,
                    MethodCallReceiverKind::PrimitiveTraitImpl {
                        trait_name: "Index".to_string(),
                        canonical_receiver: "Vec".to_string(),
                    },
                );
                // `get` copies the element out, so a concrete element with no
                // copy operation refuses here exactly as it refuses at `v[i]`
                // and at a range slice (#3395). An unbounded type parameter is
                // not concrete: a generic body keeps the borrowed read every
                // instantiation shares.
                if !self.validate_vec_get_element_clone_type(&resolved_elem, span) {
                    return Ty::Error;
                }
                // D432: an abstract element is read as a loan of the slot the
                // vector still owns, so `Some` carries the loan and the owning
                // removal stays the way to move an element out.
                let Some(mode) = self.vec_iteration_element_mode(&resolved_elem, span) else {
                    return Ty::Error;
                };
                // Records the resolved call through the shared Vec authority.
                self.record_resolved_vec_call("get", &resolved_elem, span);
                if mode == super::types::VecIterationMode::Borrow {
                    let key = SpanKey::in_module(span, self.current_module_idx);
                    self.borrowed_element_option_reads.insert(key);
                    if let Some(call) = self
                        .resolved_calls
                        .get_mut(&SpanKey::in_module(span, self.current_module_idx))
                    {
                        call.method_target.symbol_name =
                            crate::RuntimeCallFamily::Vector(crate::VecValueOp::GetBorrow)
                                .c_symbol()
                                .to_string();
                    }
                }
                // `<Vec<T> as Index>::Output` is `T`, so the projected return
                // is `Option<T>`.
                Ty::option(resolved_elem)
            }
            "contains" if runtime_method_declared => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &elem_ty);
                }
                let resolved_elem = self.subst.resolve(&elem_ty);
                if crate::vec_authority::classify_element(&resolved_elem, &self.type_defs)
                    == Some(crate::vec_authority::VecElementToken::Layout)
                {
                    // W3.032 Slice 3e: lift the layout gate for equality-
                    // eligible Copy records/tuples.  Authority chain: the
                    // checker is the sole arbiter; HIR/MIR/codegen treat the
                    // recorded `"hew_vec_contains_thunk"` symbol string as an
                    // opaque eligibility certificate and do NOT re-derive
                    // eligibility (see W3.032 plan §"Checker authority
                    // carry").
                    let eligibility =
                        crate::eq_eligibility::ty_is_eq_eligible(&resolved_elem, &self.type_defs);
                    let is_copy = self.vec_element_has_copy_layout(&resolved_elem);
                    let is_owned_admissible = self.element_owns_heap(&resolved_elem);
                    if matches!(eligibility, crate::eq_eligibility::EqEligibility::Eligible)
                        && (is_copy || is_owned_admissible)
                    {
                        self.record_resolved_vec_call("contains", &resolved_elem, span);
                    } else if matches!(eligibility, crate::eq_eligibility::EqEligibility::Eligible)
                    {
                        // Eligible but not Copy: layout-managed semantics
                        // (clone/drop) are not yet supported here.  The
                        // historical `_layout` fail-closed diagnostic is the
                        // closest substitute and names the would-be symbol.
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            span,
                            format!(
                                "`Vec.contains` on layout-backed element type `{}` requires \
                                 the element to be `Copy`; layout-managed records require \
                                 clone/drop semantics that are not implemented for \
                                 equality-based contains",
                                resolved_elem.user_facing()
                            ),
                        );
                    } else {
                        self.report_vec_contains_layout_equality_gate(
                            &resolved_elem,
                            eligibility,
                            span,
                        );
                    }
                } else {
                    self.record_resolved_vec_call("contains", &resolved_elem, span);
                }
                Ty::Bool
            }
            "join" if runtime_method_declared => {
                self.check_arity(args, 1, "`Vec.join`", span);
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &Ty::String);
                }
                if elem_ty == Ty::String {
                    // `Vec<string>::join` is the sole element-type cell;
                    // non-string element rejection remains the type gate
                    // below.
                    let resolved_elem = self.subst.resolve(&elem_ty);
                    self.record_resolved_vec_call("join", &resolved_elem, span);
                } else {
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!(
                            "`Vec.join` is only available on Vec<string>, not Vec<{}>",
                            elem_ty.user_facing()
                        ),
                    );
                }
                Ty::String
            }
            "map" => {
                self.check_arity(args, 1, "`Vec.map`", span);
                let ret_ty = Ty::Var(TypeVar::fresh());
                let expected_fn = Ty::Function {
                    capabilities: crate::CallableCapabilities {
                        suspends: true,
                        ..crate::CallableCapabilities::default()
                    },
                    params: vec![elem_ty.clone()],
                    ret: Box::new(ret_ty.clone()),
                };
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &expected_fn);
                }
                let resolved_ret = self.subst.resolve(&ret_ty);
                let resolved_elem = self.subst.resolve(&elem_ty);
                if !self.reject_vec_pipeline_fn_element("map", &resolved_elem, span) {
                    self.record_vec_higher_order_rewrite(
                        VecHigherOrderOp::Map,
                        &resolved_elem,
                        &resolved_ret,
                        span,
                    );
                }
                self.make_vec_type(resolved_ret, span)
            }
            "filter" => {
                self.check_arity(args, 1, "`Vec.filter`", span);
                let expected_fn = Ty::Function {
                    capabilities: crate::CallableCapabilities {
                        suspends: true,
                        ..crate::CallableCapabilities::default()
                    },
                    params: vec![elem_ty.clone()],
                    ret: Box::new(Ty::Bool),
                };
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &expected_fn);
                }
                let resolved_elem = self.subst.resolve(&elem_ty);
                if !self.reject_vec_pipeline_fn_element("filter", &resolved_elem, span) {
                    self.record_vec_higher_order_rewrite(
                        VecHigherOrderOp::Filter,
                        &resolved_elem,
                        &resolved_elem,
                        span,
                    );
                }
                resolved.clone()
            }
            "reduce" => {
                // Argument order: closure first, seed second
                // (`numbers.reduce(|a, b| a + b, 0)`) — `fold` with the
                // arguments flipped for chain readability (spec §3.8.6
                // documents this seeded form). A seedless 1-arg `reduce`
                // is deliberately not provided: it would need an
                // empty-vector answer, and we refuse to invent one.
                self.check_arity(args, 2, "`Vec.reduce`", span);
                let acc_ty = if let Some(arg) = args.get(1) {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp)
                } else {
                    Ty::Var(TypeVar::fresh())
                };
                let expected_fn = Ty::Function {
                    capabilities: crate::CallableCapabilities {
                        suspends: true,
                        ..crate::CallableCapabilities::default()
                    },
                    params: vec![acc_ty.clone(), elem_ty.clone()],
                    ret: Box::new(acc_ty.clone()),
                };
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &expected_fn);
                }
                let resolved_acc = self.subst.resolve(&acc_ty);
                let resolved_elem = self.subst.resolve(&elem_ty);
                if !self.reject_vec_pipeline_fn_element("reduce", &resolved_elem, span) {
                    self.record_vec_higher_order_rewrite(
                        VecHigherOrderOp::Reduce,
                        &resolved_elem,
                        &resolved_acc,
                        span,
                    );
                }
                resolved_acc
            }
            "fold" => {
                self.check_arity(args, 2, "`Vec.fold`", span);
                let acc_ty = if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp)
                } else {
                    Ty::Var(TypeVar::fresh())
                };
                let expected_fn = Ty::Function {
                    capabilities: crate::CallableCapabilities {
                        suspends: true,
                        ..crate::CallableCapabilities::default()
                    },
                    params: vec![acc_ty.clone(), elem_ty.clone()],
                    ret: Box::new(acc_ty.clone()),
                };
                if let Some(arg) = args.get(1) {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &expected_fn);
                }
                let resolved_elem = self.subst.resolve(&elem_ty);
                if self.reject_vec_pipeline_fn_element("fold", &resolved_elem, span) {
                    Ty::Error
                } else {
                    self.subst.resolve(&acc_ty)
                }
            }
            _ if runtime_method_declared => self
                .check_runtime_vec_method_from_source(type_args, method, args, span)
                .expect("Vec method signature was present immediately before dispatch"),
            _ => {
                // Unknown method fail-closed fallback.  Kept inline (NOT routed
                // through the shared `collection_method_fallback`) to preserve
                // the historical asymmetry: a successful primitive-trait
                // dispatch `return`s early and bypasses the structural-array
                // post-guard, whereas the "no method on Vec" path falls through
                // to it.
                let receiver = Ty::Named {
                    builtin: Some(BuiltinType::Vec),
                    name: "Vec".to_string(),
                    args: vec![self.subst.resolve(&elem_ty)],
                };
                if let Some(ret_ty) =
                    self.try_dispatch_primitive_trait_method(&receiver, method, args, span)
                {
                    return ret_ty;
                }
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on Vec"),
                );
                Ty::Error
            }
        };
        result
    }

    /// Stage A2: dispatch a method call on a primitive or compiler-builtin
    /// generic receiver to a user `impl Trait for <kind>` body via the
    /// `primitive_trait_impls` side table populated in Stage A1.
    ///
    /// Receiver-keyed (NOT trait-name-keyed): the lookup goes through
    /// `lookup_primitive_trait_method`, which keys on the canonical receiver
    /// kind first, so the surviving five magic `dyn Display` callers
    /// (`assert_eq` / `assert_ne` / `to_string` / `len` / `stop`) cannot be
    /// hijacked by trait-name string matching.
    ///
    /// Returns `Some(return_ty)` after applying argument checks and recording
    /// the dispatch metadata for codegen, or `None` when no impl matches and
    /// the caller should emit its own "no method on X" diagnostic so existing
    /// "no method on Vec" / "no method on string" wording survives.
    ///
    /// Limitation: if two distinct traits each define a method of the same
    /// name on the same receiver kind, this returns the first match the
    /// table iteration encounters.  Acceptable today (only `Display` is in
    /// scope per Phase 1 of #1565), but Phase 2 (`Debug`) must add a
    /// disambiguation rule before introducing same-name conflicts.
    pub(super) fn try_dispatch_primitive_trait_method(
        &mut self,
        resolved_receiver: &Ty,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Option<Ty> {
        // Default `IntLiteral` / `FloatLiteral` receivers to their canonical
        // numeric kind before the side-table lookup.  Without this,
        // `(42).fmt()` (literal-form receiver, never bound to a typed `let`)
        // would short-circuit on `canonical_primitive_or_builtin_key` returning
        // `None` for the still-polymorphic literal shape and the caller would
        // emit `no method `fmt` on int`, even though `(42_i64).fmt()` and
        // `let x: i64 = 42; x.fmt()` both succeed.  Mirrors the existing
        // defaulting sites at methods.rs:49 / 125 / 129 / 157 / 161 / 202 /
        // 254 / 317 — collapse the literal exactly at the boundary that would
        // otherwise diagnose, never eagerly upstream.
        let defaulted_receiver = resolved_receiver.materialize_literal_defaults();
        let canonical = Checker::canonical_primitive_or_builtin_key(&defaulted_receiver)?;
        let (trait_name, sig) = self.lookup_primitive_trait_method(&defaulted_receiver, method)?;
        // Bind the impl's type parameters from the concrete receiver's type
        // arguments BEFORE applying the signature, and PROVE the impl's `Self`
        // structurally matches the receiver. Without the binding a generic
        // builtin impl (`impl<E> Index for Vec<E>`) dispatched on `Vec<i64>`
        // leaves `E` (hence an `Option<E>` / `Self::Output` return) as an
        // unresolved inference var that escapes past the checker output
        // boundary — the builtin-vs-user asymmetry this fix closes. Without the
        // shape proof a constrained/concrete impl (`impl Acc for Vec<i64>`)
        // would be over-applied to a non-matching receiver (`Vec<string>`) and
        // project an authoritative-but-wrong return type (a fail-open). On a
        // non-match `instantiate_*` returns `None` and we fall through to the
        // "no method" path below, failing closed. User `Ty::Named` receivers
        // already do both via `lookup_named_method_sig`.
        let sig = self.instantiate_primitive_trait_method_sig(
            sig,
            &canonical,
            &trait_name,
            &defaulted_receiver,
        )?;
        let applied_sig = self.apply_instantiated_call_signature(
            &sig,
            None,
            args,
            span,
            SignatureArgApplication::PositionalOnly {
                arity_context: format!("method `{method}`"),
            },
            true,
            Some(GenericCallee::Method {
                type_name: &canonical,
                method,
                owner_type_args: &[],
            }),
        );
        let method_key = format!("{canonical}::{method}");
        // Concrete-specialised primitive impl (#2270): when the builtin receiver
        // has non-empty type args (e.g. `Vec<i64>`) and the impl is a concrete
        // specialisation (`impl Summable for Vec<i64>` — no generic impl type
        // params), the HIR fn_registry key is the mangled form
        // (`"Vec$$i64::total"`), not the bare `"Vec::total"`. The HIR first pass
        // mangled the key to prevent `impl Trait for Vec<i64>` and
        // `impl Trait for Vec<string>` from registering the same LLVM symbol.
        // Use the mangled c_symbol here so HIR can resolve it to a
        // `ResolvedRef::Item`; fall back to the bare key for generic impls and
        // for any type arg that cannot be mangled.
        let c_symbol = if let Ty::Named {
            args: receiver_type_args,
            ..
        } = &defaulted_receiver
        {
            if receiver_type_args.is_empty() {
                method_key.clone()
            } else {
                let resolved_args: Option<Vec<ResolvedTy>> = receiver_type_args
                    .iter()
                    .map(|ty| ResolvedTy::from_ty(ty).ok())
                    .collect();
                resolved_args
                    .as_ref()
                    .and_then(|args| crate::resolved_ty::mangle_impl_self_name(&canonical, args))
                    .filter(|m| self.fn_sigs.contains_key(&format!("{m}::{method}")))
                    .map_or_else(|| method_key.clone(), |m| format!("{m}::{method}"))
            }
        } else {
            method_key.clone()
        };
        let target = self
            .impl_method_declaration_ids
            .get(&c_symbol)
            .or_else(|| self.impl_method_declaration_ids.get(&method_key))
            .cloned()
            .map_or_else(
                || CallTarget::Unsupported {
                    reason: format!(
                        "primitive impl method `{c_symbol}` has no registered declaration identity"
                    ),
                },
                CallTarget::impl_method,
            );
        self.record_method_call_receiver_kind(
            span,
            MethodCallReceiverKind::PrimitiveTraitImpl {
                trait_name,
                canonical_receiver: canonical,
            },
        );
        if self.fn_sigs.contains_key(&method_key) || self.fn_sigs.contains_key(&c_symbol) {
            self.record_method_call_rewrite(
                span,
                MethodCallRewrite::RewriteToFunction {
                    target,
                    c_symbol,
                    // User-fn dispatch into a primitive trait impl
                    // (`i64::fmt` etc.) is open-set; the typed runtime-call
                    // catalog does not enumerate user-defined method keys.
                    descriptor: None,
                    extern_identity: None,
                    // Primitive trait-impl dispatch is a user-fn call; it never
                    // consumes the receiver as a handle release.
                    consumes_receiver: sig.consumes_receiver,
                    requires_mutable_receiver: sig.requires_mutable_receiver,
                    receiver_update: sig.receiver_update,
                    returns_receiver_identity: sig.returns_receiver_identity,
                },
            );
        }
        // Project any `Self::Output`-style associated-type carrier in the
        // return now that the impl's type params are bound to the concrete
        // receiver — `project_assoc_types` keys the concrete builtin base
        // (`<Vec<i64> as Index>::Output`) through `impl_assoc_type_bindings`.
        Some(self.project_assoc_types(&applied_sig.return_type))
    }

    /// Instantiate a primitive/builtin-generic trait-impl method signature
    /// against a concrete receiver — **fallibly**.
    ///
    /// Dispatch reaches here keyed only on the canonical builtin kind
    /// (`Vec`/`HashMap`/…), so the registered impl's `Self` type must still be
    /// *proven* to structurally match the concrete receiver before its return
    /// type is treated as authoritative. Returns `None` when the impl does not
    /// apply (constrained/concrete/nested `Self` that the receiver does not
    /// satisfy), so the caller falls through to the normal "no matching
    /// method/impl" path and fails **closed** — never projecting an authoritative
    /// return type for a receiver that does not implement the impl.
    ///
    /// On a match, binds the impl's type parameters by structurally unifying the
    /// impl's recorded `Self` type arguments
    /// ([`Checker::primitive_trait_impl_self_args`]) against the receiver's
    /// concrete type arguments, then substitutes both `Self` (→ the receiver)
    /// and the bound parameters through the signature's params and return type.
    /// Substituted parameters are dropped from `type_params` so
    /// `apply_instantiated_call_signature` does not re-instantiate them as fresh
    /// inference vars. Mirrors `instantiate_named_method_sig` (the user
    /// `Ty::Named` path) for builtin receivers, which have no `type_defs` entry.
    ///
    /// Hew rejects overlapping impls (the associated-type binding table is keyed
    /// by type *name*, so a second impl for the same `(kind, trait)` collides and
    /// is diagnosed at registration for builtins and user records alike). There
    /// is therefore at most one usable impl per `(canonical, trait)`, and the
    /// stored `Self` args identify exactly that impl — no per-impl table needed.
    ///
    /// Non-generic primitive impls (`impl Display for i64`) have empty stored
    /// `Self` args and an argument-less receiver, so the arity check passes
    /// trivially, no parameters bind, and only the `Self` → receiver substitution
    /// applies — leaving existing dispatch behaviour unchanged.
    pub(super) fn instantiate_primitive_trait_method_sig(
        &mut self,
        mut sig: FnSig,
        canonical: &str,
        trait_name: &str,
        receiver_ty: &Ty,
    ) -> Option<FnSig> {
        let mut subst: HashMap<String, Ty> = HashMap::new();
        // Clone out of the side table so the structural match can take `&mut
        // self` (it unifies unbound receiver vars against concrete `Self` args).
        if let Some(self_args) = self
            .primitive_trait_impl_self_args
            .get(&(canonical.to_string(), trait_name.to_string()))
            .cloned()
        {
            let receiver_args: Vec<Ty> = match receiver_ty {
                Ty::Named { args, .. } => args.clone(),
                _ => Vec::new(),
            };
            // Arity is part of the shape: an impl whose `Self` constructor takes
            // a different number of arguments than the receiver cannot apply.
            if self_args.len() != receiver_args.len() {
                return None;
            }
            let impl_params: HashSet<String> = sig.type_params.iter().cloned().collect();
            // Snapshot `self.subst` around the applicability probe: a multi-arg
            // `Self` (`HashMap<K, V>`) unifies each concrete arg into `self.subst`
            // as it matches, but a LATER arg may reject the impl. Without the
            // rollback an early concrete-arg unification would persist into the
            // checker's substitution past a `None` reject — binding an inference
            // var from an impl that does not actually apply. Restore on every
            // non-match so the probe is side-effect-free (hardening per the
            // A-general gate's non-blocking note).
            let subst_snapshot = self.subst.snapshot();
            for (self_arg, receiver_arg) in self_args.iter().zip(receiver_args.iter()) {
                if !self.match_self_arg_param(self_arg, receiver_arg, &impl_params, &mut subst) {
                    // `Self` does not structurally match the receiver — this impl
                    // does not apply. Roll back any partial unification, then fail
                    // closed.
                    self.subst.restore(subst_snapshot);
                    return None;
                }
            }
        }
        for param_ty in &mut sig.params {
            *param_ty = param_ty
                .substitute_named_param("Self", receiver_ty)
                .substitute_named_params_parallel(&subst);
        }
        sig.return_type = sig
            .return_type
            .substitute_named_param("Self", receiver_ty)
            .substitute_named_params_parallel(&subst);
        sig.type_params.retain(|tp| !subst.contains_key(tp));
        sig.type_param_bounds
            .retain(|tp, _| !subst.contains_key(tp));
        Some(sig)
    }

    /// Fallibly match a single impl `Self`-position type argument against the
    /// receiver's corresponding concrete argument, recording impl
    /// type-parameter bindings into `subst`. Returns `false` on any structural
    /// mismatch so the impl is rejected (fail closed) rather than over-applied.
    ///
    /// - A bare `Ty::Named { name, args: [] }` whose `name` is an impl type
    ///   parameter (`impl<E> … for Vec<E>` → `E`) binds to the resolved receiver
    ///   argument; a parameter appearing more than once (`HashMap<K, K>`) must
    ///   bind consistently.
    /// - A constructed nominal `Self` type (`Vec<T>` in `impl<T> Acc for
    ///   Vec<Vec<T>>`, or a concrete `Vec<i64>` / user `Point`) requires the
    ///   receiver to be the **same constructor** — identical `name`, `builtin`,
    ///   and arity — before recursing element-wise. This rejects
    ///   `Vec<Vec<T>>` ⊄ `Vec<Option<i64>>` (Vec ≠ Option) and `Vec<i64>` ⊄
    ///   `Vec<string>`.
    /// - A concrete non-`Named` leaf (`i64`, `string`, …) requires equality with
    ///   the resolved receiver argument.
    /// - In either concrete case, an unbound receiver `Ty::Var` is unified with
    ///   the fully-concrete `Self` argument, so inference can resolve the element
    ///   type when exactly one impl could apply.
    pub(super) fn match_self_arg_param(
        &mut self,
        self_arg: &Ty,
        receiver_arg: &Ty,
        impl_params: &HashSet<String>,
        subst: &mut HashMap<String, Ty>,
    ) -> bool {
        let receiver_resolved = self.subst.resolve(receiver_arg);
        match self_arg {
            // Bare impl type parameter: bind to the receiver arg (consistently).
            Ty::Named { name, args, .. } if args.is_empty() && impl_params.contains(name) => {
                if let Some(existing) = subst.get(name) {
                    return *existing == receiver_resolved;
                }
                subst.insert(name.clone(), receiver_resolved);
                true
            }
            // Constructed / concrete-nominal `Self` type: same constructor, then
            // recurse. An unbound receiver var unifies with a fully-concrete one.
            Ty::Named {
                name: s_name,
                builtin: s_builtin,
                args: s_args,
            } => {
                if matches!(receiver_resolved, Ty::Var(_)) {
                    return !Self::ty_contains_impl_param(self_arg, impl_params)
                        && self.try_unify_with_owner_identity(&receiver_resolved, self_arg);
                }
                let Ty::Named {
                    name: r_name,
                    builtin: r_builtin,
                    args: r_args,
                } = &receiver_resolved
                else {
                    return false;
                };
                if s_name != r_name || s_builtin != r_builtin || s_args.len() != r_args.len() {
                    return false;
                }
                s_args
                    .iter()
                    .zip(r_args.iter())
                    .all(|(s, r)| self.match_self_arg_param(s, r, impl_params, subst))
            }
            // Concrete non-`Named` leaf (`i64`, `string`, `bool`, …): require
            // equality, or unify an unbound receiver var with the concrete leaf.
            concrete => {
                if matches!(receiver_resolved, Ty::Var(_)) {
                    return self.try_unify_with_owner_identity(&receiver_resolved, concrete);
                }
                receiver_resolved == *concrete
            }
        }
    }

    /// Whether `ty` mentions any of the impl's type parameters anywhere in its
    /// structure. Used to gate unifying an unbound receiver var against a `Self`
    /// argument: only fully-concrete `Self` args (no impl params) may drive
    /// inference of the receiver's element type.
    pub(super) fn ty_contains_impl_param(ty: &Ty, impl_params: &HashSet<String>) -> bool {
        match ty {
            Ty::Named { name, args, .. } => {
                impl_params.contains(name)
                    || args
                        .iter()
                        .any(|a| Self::ty_contains_impl_param(a, impl_params))
            }
            _ => false,
        }
    }

    /// Stage A3: UFCS form of [`Self::try_dispatch_primitive_trait_method`].
    ///
    /// `Display::fmt(x)` registers `Display::fmt` in `fn_sigs` with the
    /// receiver param stripped (the `Trait::method` key triggers the
    /// is-method branch in `register_fn_sig_with_name`), so the call site
    /// would mis-arity (sig.params=[] vs args=[x]).  This helper detects
    /// the trait-qualified form, synthesizes the first arg as the
    /// receiver, and consults the same side table that powers
    /// receiver-form dispatch.  The first arg is type-checked against the
    /// canonical receiver kind itself; remaining args are type-checked
    /// against the registered sig's params (already receiver-stripped at
    /// registration time).
    ///
    /// Returns `None` when the receiver is not a primitive or builtin
    /// generic, or when no impl exists for the (kind, trait, method)
    /// triple.  The caller falls through to the existing trait-qualified
    /// dispatch in `calls.rs`, which keeps emitting today's diagnostics.
    pub(in crate::check) fn try_dispatch_ufcs_primitive_trait_method(
        &mut self,
        trait_name: &str,
        method_name: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Option<Ty> {
        let first_arg = args.first()?;
        let trait_key = self.trait_ref_lookup_key(trait_name);
        // Short-circuit before synthesising first_arg: if trait_name has no
        // primitive impls registered at all, return immediately.  This
        // prevents the fallback trait-qualified path from synthesising
        // first_arg a second time when the helper was never going to handle
        // the dispatch.  Synthesis of first_arg is deferred to after this
        // guard so it only happens when there is a real chance we will own
        // the call.
        let has_primitive_impl = self
            .primitive_trait_impls
            .keys()
            .any(|(_, tn)| tn == &trait_key);
        if !has_primitive_impl {
            return None;
        }
        let (first_expr, first_sp) = first_arg.expr();
        // Synthesize the receiver arg's type so we can route to the
        // canonical primitive/builtin-generic key.
        let receiver_ty = self.synthesize(first_expr, first_sp);
        // Default `IntLiteral` / `FloatLiteral` receivers in UFCS form
        // (e.g. `Display::fmt(42)`) for the same reason as the method-form
        // path at try_dispatch_primitive_trait_method: without defaulting,
        // the synthesized receiver_ty is still in literal shape and
        // `canonical_primitive_or_builtin_key` returns `None`, causing the
        // caller to fall through to the trait-qualified path which then
        // mis-arities (the receiver-stripped sig has 0 params vs the 1 arg
        // we just synthesized).
        let resolved_receiver = self
            .subst
            .resolve(&receiver_ty)
            .materialize_literal_defaults();
        let canonical = Checker::canonical_primitive_or_builtin_key(&resolved_receiver)?;
        // Lookup keyed on the canonical receiver kind + trait name +
        // method.  We call into the table directly (not the
        // walk-every-trait helper) because the trait name is known at
        // this call site and there is no ambiguity to resolve.
        let sig = self
            .primitive_trait_impls
            .get(&(canonical.clone(), trait_key.clone()))
            .and_then(|methods| methods.get(method_name))
            .cloned()?;
        // Bind the impl's type params from the concrete receiver before
        // applying the signature, and prove `Self` matches (mirroring the
        // method-form path) so a UFCS call (`Index::get(v, i)`) on a generic
        // builtin receiver projects its `Output`/return instead of leaking an
        // unresolved inference var — and a constrained/concrete impl is not
        // over-applied. On a non-match, fall through to the trait-qualified
        // path, which fails closed.
        let sig = self.instantiate_primitive_trait_method_sig(
            sig,
            &canonical,
            &trait_key,
            &resolved_receiver,
        )?;
        // Do not add an outer check_arity here.  apply_instantiated_call_signature
        // already calls check_arity on trailing_args via PositionalOnly, matching
        // the receiver-form path at try_dispatch_primitive_trait_method.  An
        // outer check on all args (receiver + trailing) would fire a second
        // arity diagnostic for the same call — e.g. Display::fmt(x, extra)
        // would emit both "expected 1 arg" and "expected 0 trailing args".
        // Type-check remaining args against the (receiver-stripped)
        // params using the same machinery as method-form dispatch.
        let trailing_args = &args[1.min(args.len())..];
        let applied = self.apply_instantiated_call_signature(
            &sig,
            None,
            trailing_args,
            span,
            SignatureArgApplication::PositionalOnly {
                arity_context: format!("method `{trait_name}.{method_name}`"),
            },
            true,
            Some(GenericCallee::Method {
                type_name: &canonical,
                method: method_name,
                owner_type_args: &[],
            }),
        );
        self.record_method_call_receiver_kind(
            span,
            MethodCallReceiverKind::PrimitiveTraitImpl {
                trait_name: trait_name.to_string(),
                canonical_receiver: canonical,
            },
        );
        Some(self.project_assoc_types(&applied.return_type))
    }

    /// Mutable methods write back to the receiver's place. Record projections
    /// share their root's mutability, just as they do for field assignment.
    pub(super) fn check_mutable_method_receiver(
        &mut self,
        receiver: &Spanned<Expr>,
        description: &str,
        span: &Span,
    ) {
        self.reject_indexed_writable_borrow(receiver);
        let place = self.expr_place(&receiver.0).or_else(|| {
            self.assignment_root_binding_name(&receiver.0)
                .map(|root| (root.to_string(), Vec::new()))
        });
        let root = place.as_ref().map(|(root, _)| root.as_str());
        if !root
            .and_then(|root| self.env.lookup_ref(root))
            .is_some_and(|binding| binding.is_mutable)
        {
            if let Some(error) =
                root.and_then(|root| self.private_capture_mutation_error(root, span))
            {
                self.errors.push(error);
                return;
            }
            let label =
                root.map_or_else(|| "this expression".to_string(), |root| format!("`{root}`"));
            let declaration = root
                .and_then(|root| self.env.lookup_ref(root))
                .and_then(|binding| binding.def_span.clone());
            let error_index = self.errors.len();
            self.report_error(
                TypeErrorKind::MutabilityError,
                span,
                format!("{description} requires a mutable binding receiver; {label} is not declared with `var`"),
            );
            if let (Some(declaration), Some(error)) =
                (declaration, self.errors.get_mut(error_index))
            {
                error.notes.push((
                    declaration,
                    "immutable binding declared here; use `var` to allow mutation".to_string(),
                    error.source_module.clone(),
                ));
            }
        } else if let Some((root, path)) = place {
            self.env.discount_mutation_receiver_read(&root);
            self.env.mark_written(&root);
            self.reject_borrowed_parameter_mutation(&root, &path, span);
        }
    }

    pub(in crate::check) fn check_method_call(
        &mut self,
        receiver: &Spanned<Expr>,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        let result = self.check_method_call_inner(receiver, method, args, span);
        let result = self.finish_actor_receive_call(receiver, span, result);
        let key = SpanKey::in_module(span, self.current_module_idx);
        self.check_method_callable_place(receiver, method, span);
        let runtime_rewrite_consumes_receiver = matches!(
            self.method_call_rewrites.get(&key),
            Some(MethodCallRewrite::RewriteToFunction {
                consumes_receiver: true,
                ..
            })
        );
        let runtime_rewrite_updates_receiver = matches!(
            self.method_call_rewrites.get(&key),
            Some(MethodCallRewrite::RewriteToFunction {
                descriptor: Some(descriptor),
                ..
            }) if matches!(
                descriptor.family().semantic_contract().map(|contract| contract.result),
                Some(
                    crate::runtime_call::RuntimeResultEffect::UpdatedReceiver(_)
                        | crate::runtime_call::RuntimeResultEffect::UpdatedReceiverAndValue(_)
                )
            )
        );
        let collection_updates_receiver = self
            .resolved_calls
            .get(&key)
            .and_then(|call| match call.target {
                CallTarget::RuntimeCollection(crate::MethodTargetFamily::Vec(method)) => {
                    crate::VecValueOp::from_method(method).map(crate::RuntimeCallFamily::Vector)
                }
                CallTarget::RuntimeCollection(crate::MethodTargetFamily::HashMap(method)) => {
                    crate::runtime_call::MapValueOp::from_method(method)
                        .map(crate::RuntimeCallFamily::Map)
                }
                CallTarget::RuntimeCollection(crate::MethodTargetFamily::HashSet(method)) => {
                    crate::runtime_call::SetValueOp::from_method(method)
                        .map(crate::RuntimeCallFamily::Set)
                }
                _ => None,
            })
            .is_some_and(|family| {
                matches!(
                    family.semantic_contract().map(|contract| contract.result),
                    Some(
                        crate::RuntimeResultEffect::UpdatedReceiver(_)
                            | crate::RuntimeResultEffect::UpdatedReceiverAndValue(_)
                    )
                )
            });
        if runtime_rewrite_updates_receiver || collection_updates_receiver {
            self.check_mutable_method_receiver(
                receiver,
                &format!("collection method `{method}`"),
                span,
            );
        }

        if runtime_rewrite_consumes_receiver {
            self.method_call_consumes_receiver.insert(key);
            if let Expr::Identifier(name) = &receiver.0 {
                // The typed consumption decision overrides a surface Copy
                // derivation. In particular, a lambda-actor handle is an
                // opaque wrapper, but release still consumes its sole runtime
                // handle and any later receiver use is invalid.
                self.env.mark_moved(name, receiver.1.clone());
            }
        }

        result
    }

    pub(in crate::check) fn check_dotted_type_member_call_against_expected(
        &mut self,
        receiver: &Spanned<Expr>,
        method: &str,
        args: &[CallArg],
        expected: &Ty,
        span: &Span,
    ) -> Option<Ty> {
        let head = self.resolve_dotted_type_head(receiver, method)?;
        let result = self.dispatch_dotted_type_member(
            &head,
            method,
            &DottedTypeMemberUse::Call {
                args,
                expected: Some(expected),
                span,
            },
        )?;
        self.mark_resolved_nominal_owner_used(&head.canonical_type);

        Some(result)
    }
}
