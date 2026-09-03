#![allow(
    deprecated,
    reason = "temporary named identity reconstruction migration seam"
)]

use super::{
    base_local, binder_read_is_borrow_safe_terminator, callee_is_resolved_item,
    callee_returns_fresh_owner, field_override_uses_record_field_drop, float_width,
    generator_yield_instr_escapes, generator_yield_terminator_escapes,
    hir_expr_contains_synthetic_vec_get_clone, literal_match_scrutinee_ty,
    place_is_interior_projection, string_binder_read_is_user_fn_borrow, ty_is_generator_handle,
    ty_is_indirect_enum, user_record_layout_key, BindingId, Builder, BuiltinType, CmpPred,
    Disposition, FailClosedReason, FieldOffset, FloatWidth, HashMap, HashSet, HirExpr, HirExprKind,
    HirLiteral, Instr, IntentKind, MirDiagnostic, MirDiagnosticKind, MirStatement, Place,
    ProducedValueOwnership, ProjectedPayloadOrigin, ProjectedPayloadProvenance,
    ProjectedPayloadRejectReason, ProjectedScrutinee, ReleaseSymbolVerdict, ResolvedRef,
    ResolvedTy, ScopeId, SiteId, Terminator, TrapKind, ValueClass, VecElementRelease,
    SYNTHETIC_PROJECTED_SCRUTINEE_NAME,
};

/// Release symbol for an `Rc` / `Weak` slot overwritten at a reassignment
/// generation boundary; see `Builder::emit_refcounted_overwrite_release`.
fn refcounted_overwrite_release_symbol(ty: &ResolvedTy) -> Option<&'static str> {
    match ty {
        ResolvedTy::Named {
            builtin: Some(BuiltinType::Rc),
            ..
        } => Some("hew_rc_drop"),
        ResolvedTy::Named {
            builtin: Some(BuiltinType::Weak),
            ..
        } => Some("hew_weak_drop_rc"),
        _ => None,
    }
}

fn is_builtin_option_carrier(ty: &ResolvedTy) -> bool {
    matches!(
        ty,
        ResolvedTy::Named {
            args,
            builtin: Some(BuiltinType::Option),
            ..
        } if args.len() == 1
    )
}

/// Chain-wide ownership mode for record/tuple project matches.
///
/// Project arms that introduce no field bindings, together with wildcard
/// fallbacks, only inspect the scrutinee. Any projected field binding or
/// whole-value binding transfers ownership on selection. Other predicate
/// families are not project chains and retain their existing classification.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) enum ProjectMatchOwnershipMode {
    Borrow,
    Consume,
    NotApplicable,
}

#[derive(Clone, Copy, Debug)]
enum ProjectAggregateKind {
    Record,
    Tuple,
}

#[derive(Clone, Debug)]
enum ProjectFieldDischarge {
    Leaf {
        kind: ProjectAggregateKind,
        field_idx: u32,
        field_ty: ResolvedTy,
        symbol: &'static str,
    },
    InPlace {
        kind: ProjectAggregateKind,
        field_idx: u32,
        field_ty: ResolvedTy,
    },
}

/// A whole-value project fallback has completed its owner handoff only when
/// both physical endpoints publish one unique generation for the exact
/// bindings involved. `current_owner_id_at_place` represents zero or multiple
/// candidates as `None`, so unknown and ambiguous places deliberately retain
/// the ordinary consume marker and remain validator-visible.
fn exact_whole_binding_owner_handoff(
    source: Option<crate::model::OwnerId>,
    source_binding: BindingId,
    destination: Option<crate::model::OwnerId>,
    destination_binding: BindingId,
) -> bool {
    matches!(
        (source, destination),
        (Some(source), Some(destination))
            if source.binding == source_binding
                && destination.binding == destination_binding
                && source.binding != destination.binding
    )
}

/// Classify a complete match chain before either parameter-consumption facts
/// or project lowering make an ownership decision.
pub(super) fn project_match_ownership_mode(
    arms: &[hew_hir::HirMatchArm],
) -> ProjectMatchOwnershipMode {
    if arms.is_empty()
        || arms.iter().any(|arm| {
            !matches!(
                arm.predicate,
                hew_hir::HirMatchArmPredicate::RecordProject { .. }
                    | hew_hir::HirMatchArmPredicate::TupleProject { .. }
                    | hew_hir::HirMatchArmPredicate::Wildcard
                    | hew_hir::HirMatchArmPredicate::Binding { .. }
            )
        })
    {
        return ProjectMatchOwnershipMode::NotApplicable;
    }

    if arms.iter().any(|arm| {
        !arm.bindings.is_empty()
            || matches!(arm.predicate, hew_hir::HirMatchArmPredicate::Binding { .. })
    }) {
        ProjectMatchOwnershipMode::Consume
    } else {
        ProjectMatchOwnershipMode::Borrow
    }
}

impl Builder {
    /// Lower an `HirExprKind::Match` expression to a tag-dispatch CFG.
    ///
    /// Emits the following block topology over `Place::EnumTag(scrutinee)`:
    ///
    /// ```text
    /// entry_bb (current):
    ///   scrutinee_local = lower(scrutinee)
    ///   tag_local = Move from Place::EnumTag(scrutinee_local)
    ///   Goto check_bb_0
    ///
    /// check_bb_i (one per non-wildcard arm i):
    ///   k = ConstI64(variant_idx_i)
    ///   cond_i = IntCmp(Eq, tag_local, k)
    ///   Branch { cond_i, then: body_bb_i, else: check_bb_{i+1} }
    ///
    /// body_bb_i:
    ///   result_local = lower(arm_i.body)
    ///   Goto join_bb
    ///
    /// (last check falls through to either the wildcard body or the
    /// fail-closed trap block)
    ///
    /// wildcard_bb (when a wildcard arm exists):
    ///   result_local = lower(wildcard.body)
    ///   Goto join_bb
    ///
    /// fallthrough_bb (when no wildcard arm — emitted as a runtime guard
    /// even though the checker pre-gates non-exhaustive matches per
    /// LESSONS `match-fail-closed`):
    ///   Trap { kind: ExhaustivenessFallthrough }
    ///
    /// join_bb:
    ///   (subsequent lowering continues here; result is result_local)
    /// ```
    ///
    /// Returns the result `Place::Local` that every arm body's value is
    /// moved into. For a Unit-valued match the result local is allocated
    /// but never read by codegen.
    #[allow(
        clippy::too_many_lines,
        reason = "single coherent CFG builder for the match dispatch chain; splitting would hide block-allocation ordering"
    )]
    pub(crate) fn lower_match(
        &mut self,
        result_site: SiteId,
        scrutinee: &HirExpr,
        arms: &[hew_hir::HirMatchArm],
        result_ty: &ResolvedTy,
    ) -> Option<Place> {
        if let Some(arm) = arms.iter().find(|arm| {
            arm.scope.is_none()
                && (!arm.bindings.is_empty()
                    || !arm.payload_variant_predicates.is_empty()
                    || matches!(arm.predicate, hew_hir::HirMatchArmPredicate::Binding { .. }))
        }) {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "match arm binding without lexical scope".to_string(),
                    site: arm.body.site,
                },
                note: "HIR must carry the synthetic arm scope that encloses pattern bindings, \
                       guards, and the body; lowering an unscoped payload alias would make its \
                       release lifetime ambiguous"
                    .to_string(),
            });
            return None;
        }
        // A `match` whose checker-assigned type is `Never` produces no usable
        // value: either every arm diverges (e.g. all arms `panic(...)`/`return`)
        // or the match sits in a discarded statement position alongside a
        // diverging arm. Each diverging arm's body lowers to `None` (no Move
        // into the result place), so the result alloca is never read — but it
        // must still be *allocated* because the sub-lowerings build their CFG
        // around a concrete `result_place`. Allocating it as `Never` trips the
        // codegen-front `primitive_to_llvm` fail-closed arm (Never has no value
        // representation). Substitute `Unit` — the canonical zero-sized
        // stand-in (an i8 alloca) — so the dead result place has a valid LLVM
        // type. Idiomatic Hew uses `panic(...)` directly as a match-arm body
        // (`.Some(v) => panic(...), .None => {}`); this keeps that form compiling.
        let result_ty = if matches!(result_ty, ResolvedTy::Never) {
            &ResolvedTy::Unit
        } else {
            result_ty
        };

        // Mixed-divergence recovery (#1911). When SOME arms diverge
        // (`return`/`panic`) and some yield a value, the checker types the whole
        // `match` `Unit`: a block ending in `return` is itself `Unit`, so the
        // arm-type join with the value arms collapses to `Unit` rather than the
        // value arms' real type. (This is distinct from the all-diverging case,
        // which the #1907 reachability guard handles by skipping the dead Move.)
        // With `result_ty == Unit` the result place is the i8 Unit stand-in, yet
        // the value-yielding arms move a non-scalar (`String`/`Vec`/record) into
        // it and the reachable join secures that i8 into the function's
        // non-scalar return slot — the `Move type mismatch: src=i8 dest=ptr`
        // abort. Recover the real result type from the arms: a genuinely-`Unit`
        // match has all-`Unit`/`Never` arm bodies, so the first arm whose body
        // type is neither `Unit` nor `Never` is the value the live arms produce,
        // and the result place must carry THAT type. The diverging arms still
        // lower to `None` (no Move into the result place; they secure their own
        // return slot and return), so widening the result place is safe for them.
        let result_ty = if matches!(result_ty, ResolvedTy::Unit) {
            arms.iter()
                .map(|arm| &arm.body.ty)
                .find(|ty| !matches!(ty, ResolvedTy::Unit | ResolvedTy::Never))
                .unwrap_or(result_ty)
        } else {
            result_ty
        };

        // Payload predicates (literal comparisons against constructor payload
        // fields) are now lowered inside `lower_match_enum_tag`. No early
        // exit here — the dispatcher routes to the correct sub-function which
        // handles them.

        // Dispatch: regex-predicate arms require ordered predicate dispatch
        // through the runtime ABI; enum-tag arms use the fast tag-compare chain.
        // A match expression may not mix Regex and EnumVariant arms (the checker
        // rejects heterogeneous scrutinee types before this point), so the
        // presence of any Regex arm signals the ordered-predicate path.
        let has_regex = arms
            .iter()
            .any(|a| matches!(a.predicate, hew_hir::HirMatchArmPredicate::Regex { .. }));
        let has_literal = arms
            .iter()
            .any(|a| matches!(a.predicate, hew_hir::HirMatchArmPredicate::Literal { .. }));
        let has_variant = arms.iter().any(|a| {
            matches!(
                a.predicate,
                hew_hir::HirMatchArmPredicate::EnumVariant { .. }
            )
        });
        let has_project = arms.iter().any(|a| {
            matches!(
                a.predicate,
                hew_hir::HirMatchArmPredicate::RecordProject { .. }
                    | hew_hir::HirMatchArmPredicate::TupleProject { .. }
            )
        });
        // A "pure binding/guard chain" is a match where every arm is either
        // `Binding` or `Wildcard` (all catch-all predicates). These must be
        // lowered as an ordered chain — not as a single wildcard — because
        // each arm may have a guard that falls through on failure.
        let is_pure_binding_chain = !has_regex
            && !has_literal
            && !has_variant
            && !has_project
            && arms.iter().all(|a| {
                matches!(
                    a.predicate,
                    hew_hir::HirMatchArmPredicate::Wildcard
                        | hew_hir::HirMatchArmPredicate::Binding { .. }
                )
            });
        let project_scrutinee = self.is_project_match_scrutinee_ty(&scrutinee.ty);

        assert!(
            !(has_literal && has_variant),
            "checker invariant violated: mixed Literal/Variant arms"
        );
        assert!(
            !(has_literal && has_regex),
            "checker invariant violated: mixed Literal/Regex arms"
        );
        assert!(
            !(has_project && (has_literal || has_regex || has_variant)),
            "checker invariant violated: mixed project/refutable match arms"
        );

        if has_regex {
            self.lower_match_regex(scrutinee, arms, result_ty)
        } else if is_pure_binding_chain {
            // Ordered chain of binding/wildcard arms, each with an optional
            // guard. Falls through on guard failure.
            self.lower_match_binding_chain(scrutinee, arms, result_ty)
        } else if has_literal || literal_match_scrutinee_ty(&scrutinee.ty) {
            self.lower_match_literal(scrutinee, arms, result_ty)
        } else if has_project || project_scrutinee {
            self.lower_match_project(scrutinee, arms, result_ty)
        } else {
            self.lower_match_enum_tag(result_site, scrutinee, arms, result_ty)
        }
    }

    fn retain_typed_join_branch(
        &mut self,
        result_site: SiteId,
        branch: &HirExpr,
        value: Place,
        result_ty: &ResolvedTy,
    ) -> Place {
        let retained_join = self
            .param_ownership
            .produced_value_facts
            .get(&result_site)
            .is_some_and(|fact| {
                matches!(
                    fact.ownership,
                    hew_types::ProducedValueOwnership::Owned {
                        acquisition: hew_types::ProducedValueAcquisition::Retained
                    }
                )
            });
        let borrowed_branch = self
            .param_ownership
            .produced_value_facts
            .get(&branch.site)
            .is_some_and(|fact| {
                matches!(
                    fact.ownership,
                    hew_types::ProducedValueOwnership::Borrowed
                        | hew_types::ProducedValueOwnership::ReceiverIdentity
                )
            });
        if !retained_join || !borrowed_branch {
            return value;
        }
        // The "borrowed branch" fact predates the uniform owned-carrier
        // protocol (D185). When lowering already emitted a
        // `NeutralizePayloadSlot { transferee: value }` for a variant-payload
        // slot, this branch value carries the payload's SOLE share — the
        // carrier slot is nulled, its guarded drop releases nothing — so a
        // join retain here would strand a `+1` per execution. Corroborated
        // against the recorded transferee set, never re-derived from the HIR
        // fact that misclassifies the transfer as a borrow.
        if base_local(value)
            .is_some_and(|local| self.variant_payload_transferee_locals.contains(&local))
        {
            return value;
        }
        match self.subst_ty(result_ty) {
            ResolvedTy::String => {
                let retained = self.alloc_local(ResolvedTy::String);
                self.push_instr(Instr::StringRetain {
                    value,
                    condition: crate::model::StringRetainCondition::Always,
                });
                self.yield_share_instr_exempt
                    .insert((self.current_block_id, self.instructions.len()));
                self.push_instr(Instr::Move {
                    dest: retained,
                    src: value,
                });
                self.restore_owner_after_retained_share(value, retained);
                retained
            }
            ResolvedTy::Bytes => {
                let retained = self.alloc_local(ResolvedTy::Bytes);
                self.push_instr(Instr::BytesRetain { value });
                self.yield_share_instr_exempt
                    .insert((self.current_block_id, self.instructions.len()));
                self.push_instr(Instr::Move {
                    dest: retained,
                    src: value,
                });
                self.restore_owner_after_retained_share(value, retained);
                retained
            }
            _ => value,
        }
    }

    fn is_project_match_scrutinee_ty(&self, ty: &ResolvedTy) -> bool {
        match ty {
            ResolvedTy::Tuple(_) => true,
            _ => user_record_layout_key(&self.subst_ty(ty))
                .is_some_and(|key| self.lookup_record_field_order(&key).is_some()),
        }
    }

    /// True when the match scrutinee is the `VecIter::next` desugar's
    /// synthetic `Option<T>` clone-out producer. Every `Some(x)` payload is a
    /// fresh, solely-owned per-frame value, so it follows the same body/edge
    /// release-or-transfer lifecycle as a generator or receiver yield.
    fn is_vec_iter_next_scrutinee(&self, scrutinee: &HirExpr) -> bool {
        is_builtin_option_carrier(&self.subst_ty(&scrutinee.ty))
            && hir_expr_contains_synthetic_vec_get_clone(scrutinee)
    }

    /// True when the match scrutinee is a generator `.next()` consumption node
    /// (`HirExprKind::GeneratorNext`) — either a source-level `g.next()` or the
    /// `for x in gen()` desugar's synthetic next-call. The yielded value bound by
    /// the `Some` arm is a FRESH, solely-owned heap value the runtime handed to
    /// the consumer (the coro `.next()` drive hands back an owned payload), so
    /// — like a `Vec<String>` iterator's retained string — it must be
    /// released at the end of the consuming body. Without that release every
    /// yielded heap value (a `Vec` yield, an `f"…"` string yield) leaks.
    fn is_generator_next_scrutinee(scrutinee: &HirExpr) -> bool {
        matches!(&scrutinee.kind, HirExprKind::GeneratorNext { .. })
    }

    /// True when the match scrutinee is a stream/channel recv call returning
    /// `Option<T>` whose `Some` payload owns heap. This covers every recv shape
    /// that surfaces through `lower_match_enum_tag`: the for-await desugar's
    /// synthetic `match channel.recv(__hew_for_iter_X) { .Some(item) => body,
    /// .None => break }` and source-level `match await rx.recv() { ... }` /
    /// `match rx.try_recv() { ... }` whose scrutinee lowers to the same direct
    /// `Call { callee: hew_channel_*_layout / hew_stream_*_layout }` shape.
    ///
    /// Each recv hands the consumer a FRESH, solely-owned heap value (the
    /// runtime allocates an `alloc_cstring_data` block per frame for `string`,
    /// a fresh bytes header for `Bytes`). The `Some(item)` arm payload binding
    /// owns exactly one reference whose only release path is the consuming
    /// body's per-iteration drop — identical ownership shape to a generator
    /// yield. Without that release every received frame leaks one heap block
    /// per iteration (every `Stream<string>` recv loop, every `Receiver<T>::recv`
    /// drain), which is the leak this fix closes.
    ///
    /// The detector matches structurally on the HIR `Call` callee `BindingRef`
    /// name — the same identity codegen uses to intercept the call and
    /// materialise `Option<T>` from the runtime's null-ptr-on-EOF return —
    /// rather than on the MIR terminator shape, so this fires before MIR
    /// lowering decides between `Terminator::Call` (blocking) and
    /// `Terminator::SuspendingChannelRecv` (suspending in execution-context
    /// callers). Both terminator shapes feed the same Option<T> wrapper into
    /// the match; the per-iteration drop discipline is identical.
    pub(crate) fn is_recv_next_scrutinee(scrutinee: &HirExpr) -> bool {
        let HirExprKind::Call { callee, .. } = &scrutinee.kind else {
            return false;
        };
        let HirExprKind::BindingRef { name, .. } = &callee.kind else {
            return false;
        };
        // Every recv-result-producing runtime symbol (returns `Option<T>` or
        // a result codegen wraps into `Option<T>`) that can appear as a match
        // scrutinee in a `match recv()` / `match next()` shape:
        //   * channel recv: the layout-witness `hew_channel_recv_layout` /
        //     `hew_channel_try_recv_layout` entries (one symbol per operation
        //     for every describable element type).
        //   * stream next: the layout-witness `hew_stream_next_layout` /
        //     `hew_stream_try_next_layout` entries.
        //   * duplex recv: `hew_duplex_recv` / `hew_duplex_try_recv` and the
        //     half-duplex `hew_duplex_recv_half` — all produce an `Option<T>`
        //     payload in the same shape.
        matches!(
            name.as_str(),
            "hew_channel_recv_layout"
                | "hew_channel_try_recv_layout"
                | "hew_stream_next_layout"
                | "hew_stream_try_next_layout"
                | "hew_duplex_recv"
                | "hew_duplex_recv_half"
                | "hew_duplex_try_recv"
        )
    }

    /// The classified release verdict for a generator-yielded (or
    /// channel-received) `Some(x)` payload of type `ty`:
    /// [`ReleaseSymbolVerdict::Wired`] carries the C-ABI symbol the
    /// consumer-body drop emits, restricted to the proven leak shapes — a
    /// heap-owning `string`, `bytes`, and any builtin `Vec<T>` whose element
    /// release is wired. [`ReleaseSymbolVerdict::WiredInPlace`] covers a
    /// registered heap-owning record/enum composite or structural tuple/array.
    /// Named composites use their synthesised in-place thunk; structural
    /// aggregates use the recursive field walker. A `BitCopy` composite owns
    /// no heap and never earns a release. Rc/Weak use their retain-balancing
    /// drops, and HashMap/HashSet use their layout-aware releases.
    /// [`ReleaseSymbolVerdict::Unwired`] is the fail-closed refusal: the
    /// value owns heap the buffer-only free cannot reach (a `Vec` of `bytes`
    /// or of an indirect-enum element), so the consulting site must reject
    /// the construct at compile time — never emit a wrong-ABI free.
    ///
    /// The `Wired` selection MUST mirror codegen's
    /// `resolved_ty_cow_heap_release` so the inline-drop validator
    /// (`lower_inline_drop` → congruence check) accepts the emitted symbol
    /// (`dedup-semantic-boundary`).
    ///
    /// `Bytes` does NOT appear in `resolved_ty_cow_heap_release` (a native `bytes`
    /// value is a stack-resident `BytesTriple { ptr, i32, i32 }`, not a single
    /// owned pointer, so the generic single-`ptr`-load release shape that
    /// `resolved_ty_cow_heap_release` describes does not apply). The inline-drop
    /// dispatcher (`lower_inline_drop`) intercepts the
    /// `(ty == Bytes, drop_fn == "hew_bytes_drop")` pair BEFORE the
    /// `resolved_ty_cow_heap_release` congruence check and routes it through the
    /// `BytesTriple`-aware emitter (`emit_bytes_inplace_drop`): GEP field 0,
    /// load the data ptr, call `hew_bytes_drop(data_ptr)`, null-store the
    /// field to make a structurally-reachable second drop a no-op against
    /// `hew_bytes_drop(null)`. This is the SAME triple-field-0 release shape
    /// the wirecodec decoder's bytes-drop emitter uses
    /// (`hew-codegen-rs/src/llvm.rs`, "Bytes: stored as a `{ ptr, i32, i32 }`
    /// triple"), kept in sync so the two cannot drift on which byte of the
    /// triple owns the heap allocation.
    pub(crate) fn generator_yield_drop_symbol(&self, ty: &ResolvedTy) -> ReleaseSymbolVerdict {
        match ty {
            ResolvedTy::TraitObject { .. } => {
                ReleaseSymbolVerdict::Wired("hew_dyn_trait_drop_boxed_in_place")
            }
            ResolvedTy::String => ReleaseSymbolVerdict::Wired("hew_string_drop"),
            // Per-iteration release for a `for await frame in <Stream<bytes>>`
            // binding (and any analogous Some-arm `bytes` payload on a recv-call
            // scrutinee). The layout-witness pop hands the consumer a fresh,
            // refcounted `BytesTriple` per frame: a body that does not move the
            // value out is the sole owner and must release exactly one reference
            // on every exit edge. Without this arm the per-frame triple's data
            // buffer is overwritten on the next iteration with no preceding
            // `hew_bytes_drop`, leaking one refcounted allocation per frame
            // (observed at 1.0 leak / frame on the `for await stream<bytes>`
            // oracle before this arm was added).
            ResolvedTy::Bytes => ReleaseSymbolVerdict::Wired("hew_bytes_drop"),
            ResolvedTy::Named {
                builtin: Some(hew_types::BuiltinType::Vec),
                args,
                ..
            } => {
                // The element's release bucket selects the Vec verdict, read
                // from the one typed classification. Its dispatch checks the
                // closure-pair bucket FIRST, mirroring codegen's
                // `resolved_ty_cow_heap_release` (a fn/closure element is neither
                // an owned composite nor a plain leaf; the inline-drop
                // congruence check rejects a mis-picked `hew_vec_free` for a
                // yielded `Vec<fn>` — `dedup-semantic-boundary`), and its
                // owned bucket routes through the SAME `is_owned_vec_element`
                // authority codegen's
                // `resolved_ty_element_owns_heap_for_owned_vec` agrees with.
                // The classification runs on the RAW element (a yield's type
                // is concrete at its producer; the field picker substitutes
                // first — the asymmetry is pinned by
                // `yield_and_field_pickers_match_legacy_symbol_table`). A
                // no-type-arg `Vec` falls through to the plain buffer free.
                args.first()
                    .map_or(ReleaseSymbolVerdict::Wired("hew_vec_free"), |elem| {
                        self.vec_release_symbol_verdict(elem)
                    })
            }
            ResolvedTy::Named {
                builtin: Some(hew_types::BuiltinType::HashMap),
                ..
            } => ReleaseSymbolVerdict::Wired("hew_hashmap_free_layout"),
            ResolvedTy::Named {
                builtin: Some(hew_types::BuiltinType::HashSet),
                ..
            } => ReleaseSymbolVerdict::Wired("hew_hashset_free_layout"),
            ResolvedTy::Named {
                builtin: Some(hew_types::BuiltinType::Rc),
                ..
            } => ReleaseSymbolVerdict::Wired("hew_rc_drop"),
            ResolvedTy::Named {
                builtin: Some(hew_types::BuiltinType::Weak),
                ..
            } => ReleaseSymbolVerdict::Wired("hew_weak_drop_rc"),
            ResolvedTy::Tuple(_) | ResolvedTy::Array(_, _) => ReleaseSymbolVerdict::WiredInPlace(
                crate::ownership::InPlaceReleaseKind::AggregateRecursive,
            ),
            // A field-bearing resource record has a drop-only in-place thunk:
            // it runs user close and then tears down its fields. It is affine
            // and therefore intentionally lacks the clone-total admission used
            // by ordinary composites, but a fresh arm-local owner needs only
            // the drop half.
            ResolvedTy::Named {
                name,
                args,
                is_opaque: false,
                ..
            } if args.is_empty()
                && self
                    .lifecycle_registry
                    .resource_record(&hew_types::DefId::legacy_reconstruct_from_full_path(name))
                    .is_some() =>
            {
                ReleaseSymbolVerdict::WiredInPlace(crate::ownership::InPlaceReleaseKind::Record)
            }
            // A registered heap-owning record/enum composite: release through
            // the synthesised in-place drop thunk. `owned_composite_release_kind`
            // (→ `elem_is_owned_abi_releasable`) is the SAME admission the
            // stream layout witness mirrors for its LayoutManaged deep-clone
            // (`owned_elem_thunk_key`), so this verdict Wires the release
            // exactly where the witness already clones — a shape it refuses
            // (BitCopy composite, indirect enum, closure-bearing, resource /
            // opaque leaves) keeps its existing NoDropPath / rejected posture.
            composite @ ResolvedTy::Named { .. } => {
                match self.owned_composite_release_kind(composite) {
                    Some(kind) => ReleaseSymbolVerdict::WiredInPlace(kind),
                    None => ReleaseSymbolVerdict::NoDropPath,
                }
            }
            _ => ReleaseSymbolVerdict::NoDropPath,
        }
    }

    /// The in-place thunk family (record vs enum) releasing an owned composite
    /// yield/recv payload, or `None` when the type is not an owned-ABI
    /// releasable composite. Admission is exactly
    /// [`Builder::elem_is_owned_abi_releasable`] — the documented MIR mirror
    /// of codegen's `owned_elem_thunk_key` witness authority — so the picker
    /// and the layout witness cannot drift on which composites carry live
    /// ownership through the queue (`dedup-semantic-boundary`). The
    /// record-vs-enum split re-reads the same registries that authority
    /// consulted (a name resolves to exactly one of them); a generic
    /// instantiation probes the short-name-mangled key (the registration
    /// form), with the short-name comparison covering module-qualified
    /// monomorphic names.
    fn owned_composite_release_kind(
        &self,
        ty: &ResolvedTy,
    ) -> Option<crate::ownership::InPlaceReleaseKind> {
        if !self.elem_is_owned_abi_releasable(ty) {
            return None;
        }
        let ResolvedTy::Named { name, args, .. } = ty else {
            return None;
        };
        let is_enum = crate::model::find_enum_layout(name, args, &self.enum_layouts).is_some();
        Some(if is_enum {
            crate::ownership::InPlaceReleaseKind::Enum
        } else {
            crate::ownership::InPlaceReleaseKind::Record
        })
    }

    /// The shared `Vec<E>` arm of both release-symbol pickers: map the
    /// element's typed release classification to the picker verdict. One
    /// body, consulted by `generator_yield_drop_symbol` (raw element) and
    /// `project_field_inline_drop_symbol` (substituted element), so the two
    /// pickers cannot drift on the fail-closed boundary
    /// (`dedup-semantic-boundary`).
    ///
    /// The `Unsupported` domain splits three ways, drawing the SAME boundary
    /// as the compile reject `unsupported_vec_element_walk`:
    ///   - `NoReleaseProtocol` with no owned-ABI release
    ///     (`!elem_is_owned_abi_releasable`) — a `bytes` fat triple or an
    ///     indirect-enum node — is [`ReleaseSymbolVerdict::Unwired`]: the
    ///     buffer-only free would leak every element node, so the consulting
    ///     site must refuse at compile time.
    ///   - `NoReleaseProtocol` where the element IS owned-ABI releasable: the
    ///     element's release is wired program-wide, but
    ///     `vec_owned_element_keys` is harvested per function, so a `Vec`
    ///     constructed in ANOTHER function (a generator body, a callee)
    ///     classifies unsupported HERE. The release picker must use the
    ///     harvest-independent `elem_is_owned_abi_releasable` authority and
    ///     emit `hew_vec_free_owned`; a buffer-only free would leak every
    ///     element payload.
    ///   - `UnknownValueClass` — a `Named` head no layout registry can see —
    ///     is [`ReleaseSymbolVerdict::Unwired`] too: a buffer-only free over an
    ///     element the authority never saw is the leak surface, so the
    ///     consulting site refuses at compile time.
    fn vec_release_symbol_verdict(&self, elem: &ResolvedTy) -> ReleaseSymbolVerdict {
        #[allow(
            clippy::match_same_arms,
            reason = "the repeated symbols are projections of distinct typed release \
                      decisions; keeping the arms separate makes the owned, plain, and \
                      fail-closed boundaries reviewable"
        )]
        match self.classify_vec_element_release(elem) {
            VecElementRelease::ClosurePair => ReleaseSymbolVerdict::Wired("hew_vec_free_owned"),
            VecElementRelease::OwnedElement => ReleaseSymbolVerdict::Wired("hew_vec_free_owned"),
            VecElementRelease::Plain => ReleaseSymbolVerdict::Wired("hew_vec_free"),
            VecElementRelease::Unsupported(FailClosedReason::NoReleaseProtocol)
                if self.elem_is_owned_abi_releasable(elem) =>
            {
                ReleaseSymbolVerdict::Wired("hew_vec_free_owned")
            }
            VecElementRelease::Unsupported(reason @ FailClosedReason::NoReleaseProtocol) => {
                ReleaseSymbolVerdict::Unwired(reason)
            }
            VecElementRelease::Unsupported(reason) => ReleaseSymbolVerdict::Unwired(reason),
        }
    }

    /// Emit a body-end release for a fresh `Some(x)` binding from a `VecIter`
    /// clone-out, generator drive, or receiver read. The consumer owns the
    /// value solely; releasing it here (per iteration for a `for` loop) frees
    /// the otherwise-overwritten frame. Gated on the shared body-shape
    /// drop-safety scan:
    /// if the binding's pointer escapes the consuming body (read as a
    /// non-print source operand, returned, re-yielded), MIR refuses to emit
    /// the drop and the value leaks rather than risking a use-after-free
    /// against the escaped alias.
    fn emit_generator_yield_binding_drop(
        &mut self,
        binding: BindingId,
        place: Place,
        ty: &ResolvedTy,
        body_start_block_id: u32,
        body_start_instr_len: usize,
        site: hew_hir::SiteId,
    ) {
        // Only a Wired / WiredInPlace verdict reaches this emitter: the
        // binding-registration gate schedules a body-end drop for those shapes
        // alone (Unwired is a fail-closed compile diagnostic there; NoDropPath
        // is never scheduled).
        let drop_fn = match self.generator_yield_drop_symbol(ty) {
            ReleaseSymbolVerdict::Wired(symbol) => crate::model::DropFnSpec::Release(symbol),
            ReleaseSymbolVerdict::WiredInPlace(kind) => crate::model::DropFnSpec::InPlace(kind),
            ReleaseSymbolVerdict::NoDropPath | ReleaseSymbolVerdict::Unwired(_) => return,
        };
        let Some(local) = base_local(place) else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "generator-yield binding drop".to_string(),
                    site,
                },
                note: format!(
                    "generator-yielded binding {binding} must lower to a Place::Local-backed \
                     owner so the yielded heap value can be balanced with {drop_fn:?}; got \
                     {place:?}"
                ),
            });
            return;
        };
        // A whole-local Move inside the consuming body is an ownership handoff,
        // not by itself an escape. Follow a unique linear handoff chain and
        // release its terminal owner. This is the OSSA rule: the source's
        // obligation moves with the value; it is not discarded merely because
        // the physical slot changed. Branching/multiple handoffs remain
        // fail-closed and are rejected by the balance verifier.
        let drop_place = self
            .generator_yield_linear_handoff_owner(body_start_block_id, body_start_instr_len, local)
            .unwrap_or(place);
        let drop_local = base_local(drop_place).unwrap_or(local);
        if self.generator_yield_binding_drop_safe(
            body_start_block_id,
            body_start_instr_len,
            drop_local,
        ) {
            self.push_instr(Instr::Drop {
                place: drop_place,
                ty: ty.clone(),
                drop_fn: Some(drop_fn),
            });
            self.record_body_end_release_event(binding, drop_place, ty, site);
        }
        // else: the value escapes the consuming body — leak-not-double-free.
        // No diagnostic: an escaping yield is a legitimate (if leaky) program,
        // not a lowering defect, unlike the Vec<String> getter which has no
        // other release path.
    }

    /// Record the logical lifetime end paired with a concrete body-end drop.
    /// The event lives in the release block, so predecessor unwind edges still
    /// see the generation as live while normal successors see it consumed.
    /// This is the block-level drop-flag transition used by drop elaboration;
    /// the physical `Instr::Drop` remains codegen's release operation.
    fn record_body_end_release_event(
        &mut self,
        binding: BindingId,
        place: Place,
        ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) {
        let name = self
            .owned_locals
            .iter()
            .find(|entry| entry.binding == binding)
            .map_or_else(
                || "__hew_body_end_owner".to_string(),
                |entry| entry.name.clone(),
            );
        self.statements.push(MirStatement::Use {
            binding,
            name,
            site,
            ty: ty.clone(),
            intent: hew_hir::IntentKind::Consume,
        });
        self.release_owned_local_from(
            binding,
            place,
            super::Disposition::ConsumedAt {
                transferee: None,
                site: super::DischargeSite::InlineRelease,
            },
        );
    }

    /// Follow the sole whole-local ownership handoff of a yielded/received
    /// frame owner. Returns the terminal local only when every intermediate
    /// owner has exactly one ownership-transferring use and that use is a
    /// `Move` into another local. This deliberately rejects forks and moves to
    /// aggregates/returns; those require path- or destination-owned cleanup.
    fn generator_yield_linear_handoff_owner(
        &self,
        start_block_id: u32,
        start_instr_len: usize,
        source: u32,
    ) -> Option<Place> {
        // The body-end block is still buffered while this query runs. Add a
        // read-only terminal snapshot so the ordinary CFG dominator proof can
        // distinguish a true linear handoff from a Move that occurs on only
        // one branch before the shared body-end cleanup.
        let mut body_cfg = self.pending_blocks.clone();
        body_cfg.push(super::BasicBlock {
            id: self.current_block_id,
            statements: self.statements.clone(),
            instructions: self.instructions.clone(),
            terminator: Terminator::Return,
        });
        let body_blocks = super::cfg_util::blocks_reachable_from(&body_cfg, start_block_id);
        let body_end_dominators = super::cfg_util::block_dominators(&body_cfg)
            .remove(&self.current_block_id)
            .unwrap_or_default();
        let mut current = source;
        let mut visited = HashSet::from([source]);
        let mut moved = false;

        loop {
            let mut handoffs = Vec::new();
            let mut other_escape = false;
            for block in body_cfg
                .iter()
                .filter(|block| body_blocks.contains(&block.id) || block.id == start_block_id)
            {
                let block_id = block.id;
                let instructions = block.instructions.as_slice();
                let start = if block_id == start_block_id {
                    start_instr_len.min(instructions.len())
                } else {
                    0
                };
                for (offset, instr) in instructions[start..].iter().enumerate() {
                    let instruction_index = start + offset;
                    // An adjacent retain makes this physical Move a fork, not
                    // the yielded owner's linear handoff. Lowering records the
                    // exact Move program point when it authors the retained
                    // share; the binder therefore stays at `current` for its
                    // per-iteration body-end release, while the destination
                    // owns the new count.
                    if self
                        .yield_share_instr_exempt
                        .contains(&(block_id, instruction_index))
                    {
                        continue;
                    }
                    if !generator_yield_instr_escapes(instr, current) {
                        continue;
                    }
                    match instr {
                        Instr::Move {
                            dest: Place::Local(dest),
                            src: Place::Local(src),
                        } if *src == current => handoffs.push((block_id, *dest)),
                        _ => other_escape = true,
                    }
                }
                if generator_yield_terminator_escapes(
                    &block.terminator,
                    self.suspend_kinds.get(&block_id),
                    current,
                ) {
                    other_escape = true;
                }
            }

            let [(handoff_block, next)] = handoffs.as_slice() else {
                break;
            };
            if other_escape || !body_end_dominators.contains(handoff_block) {
                // A branch-local handoff cannot select the shared cleanup
                // place: another predecessor still owns `current`. Leaving
                // the chain unresolved makes the ordinary escape scan omit
                // the unsafe body-end drop, while abandonment analysis emits
                // the explicit MaybeConsumed fail-closed diagnostic.
                break;
            }
            if !visited.insert(*next) {
                return None;
            }
            current = *next;
            moved = true;
        }

        moved.then_some(Place::Local(current))
    }

    /// Body-shape drop-safety scan for a generator-yielded binding. Unlike the
    /// `Vec<String>` retained-string scan (which rejects ANY source read because
    /// the string is a projection alias of a still-live Vec), the yielded value
    /// is a FRESH, solely-owned heap value: a borrowing read (a `.len()`-style
    /// getter call, an arithmetic operand, a `print`) does not transfer
    /// ownership, so it is safe. Only an OWNERSHIP-TRANSFERRING use makes the
    /// body-end drop wrong: a `Move` out of the binding's slot into another
    /// local, a store into a surviving aggregate, a spawn capture, or a
    /// consuming terminator (return / re-yield / actor send/ask). Those escape
    /// the body, so the body-end drop is skipped (leak-not-double-free; the
    /// move-checker / function-scope drop machinery owns the escaped value).
    pub(crate) fn generator_yield_binding_drop_safe(
        &self,
        start_block_id: u32,
        start_instr_len: usize,
        local: u32,
    ) -> bool {
        self.generator_yield_binding_drop_safe_until_scope(
            start_block_id,
            start_instr_len,
            local,
            None,
        )
    }

    /// Bound a minted call-result carrier's escape scan to its exact lexical
    /// lifetime. A loop reuses the same physical result local and static Mint
    /// on its next iteration; scanning past the non-carrying `ScopeExit`
    /// conflates that future value with the selected arm's current owner and
    /// can suppress the current generation's release.
    ///
    /// Only one binding/place/scope relation earns this boundary. Missing or
    /// aliased metadata retains the existing unbounded, fail-closed scan.
    fn call_carrier_body_end_drop_safe(
        &self,
        binding: BindingId,
        start_block_id: u32,
        start_instr_len: usize,
        local: u32,
    ) -> bool {
        let exact_bindings = self
            .binding_locals
            .iter()
            .filter_map(|(candidate, place)| {
                (base_local(*place) == Some(local) && self.binding_scope.contains_key(candidate))
                    .then_some(*candidate)
            })
            .collect::<Vec<_>>();
        let lifetime = (self.call_scrutinee_carrier_mint_locals.contains(&local)
            && matches!(exact_bindings.as_slice(), [candidate] if *candidate == binding))
        .then(|| {
            self.binding_scope
                .get(&binding)
                .copied()
                .map(|scope| (scope, binding))
        })
        .flatten()
        .filter(|lifetime| {
            self.owner_generations
                .get(&binding)
                .copied()
                .map(|generation| crate::model::OwnerId {
                    binding,
                    generation,
                })
                .is_some_and(|owner| {
                    self.call_carrier_release_precedes_scope(
                        start_block_id,
                        start_instr_len,
                        owner,
                        Place::Local(local),
                        *lifetime,
                    )
                })
        });
        self.generator_yield_binding_drop_safe_until_scope(
            start_block_id,
            start_instr_len,
            local,
            lifetime,
        )
    }

    fn generator_yield_binding_drop_safe_until_scope(
        &self,
        start_block_id: u32,
        start_instr_len: usize,
        local: u32,
        lifetime: Option<(ScopeId, BindingId)>,
    ) -> bool {
        // A minted enum carrier's projected payload binder is an interior
        // alias until an exact `NeutralizePayloadSlot` transfers that slot.
        // Scan each still-carrier-backed alias through the same escape proof:
        // an ownership-opaque extern may retain the binder even though it
        // never mentions the carrier local directly. Releasing the carrier at
        // arm end would then release host-owned storage (F1 domestic control).
        // A neutralized binder is deliberately omitted: its ownership moved
        // onward, while the carrier still owes its shell and sibling slots.
        let neutralized_slots: HashSet<Place> = self
            .pending_blocks
            .iter()
            .flat_map(|block| &block.instructions)
            .chain(self.instructions.iter())
            .filter_map(|instruction| match instruction {
                Instr::NeutralizePayloadSlot { place, .. } => Some(*place),
                _ => None,
            })
            .collect();
        let live_payload_aliases: HashSet<u32> = self
            .projected_payload_provenance
            .iter()
            .filter(|(_, provenance)| {
                base_local(provenance.source_place) == Some(local)
                    && !neutralized_slots.contains(&provenance.source_place)
            })
            .filter_map(|(binding, _)| {
                self.binding_locals
                    .get(binding)
                    .copied()
                    .and_then(base_local)
            })
            .collect();
        for alias in live_payload_aliases {
            // Retain/share bookkeeping can exempt the projected binder's call
            // from its own body-end drop scan. That does not prove the parent
            // carrier still owns the handed-off slot. Recheck call arguments
            // against the ownership-contract boundary itself: an unaudited
            // extern is consuming/opaque here even when another pass recorded
            // the binder use as a share for COW accounting.
            if self.pending_blocks.iter().any(|block| {
                matches!(
                    &block.terminator,
                    Terminator::Call { args, .. }
                        if args.iter().any(|arg| base_local(*arg) == Some(alias))
                            && !binder_read_is_borrow_safe_terminator(
                                &block.terminator,
                                self.suspend_kinds.get(&block.id),
                                alias,
                            )
                            && !string_binder_read_is_user_fn_borrow(
                                &block.terminator,
                                self.suspend_kinds.get(&block.id),
                                alias,
                                self.locals.get(alias as usize),
                                &self.module_fn_names,
                                &self.module_generic_fn_names,
                                &self.call_scrutinee_provenance.extern_table,
                            )
                )
            }) {
                return false;
            }
            let mut visiting = HashSet::new();
            let mut memo = HashMap::new();
            if !self.generator_yield_block_paths_drop_safe(
                start_block_id,
                start_block_id,
                start_instr_len,
                alias,
                lifetime,
                &mut visiting,
                &mut memo,
            ) {
                return false;
            }
        }
        let mut visiting = HashSet::new();
        let mut memo = HashMap::new();
        self.generator_yield_block_paths_drop_safe(
            start_block_id,
            start_block_id,
            start_instr_len,
            local,
            lifetime,
            &mut visiting,
            &mut memo,
        )
    }

    /// Return the first non-carrying lexical close of the exact synthetic
    /// carrier lifetime. Instructions after this marker belong to a later
    /// iteration when the loop reuses the result slot and must not influence
    /// the current owner's escape decision.
    fn call_carrier_scope_boundary(
        instructions: &[Instr],
        start: usize,
        local: u32,
        lifetime: Option<(ScopeId, BindingId)>,
    ) -> Option<usize> {
        let (scope, binding) = lifetime?;
        instructions[start..]
            .iter()
            .position(|instruction| {
                matches!(
                    instruction,
                    Instr::OwnershipEvent(crate::model::OwnershipEvent::ScopeExit {
                        scopes,
                        carry_places,
                        carried,
                        ..
                    }) if scopes.contains(&scope)
                        && !carry_places.iter().any(|place| base_local(*place) == Some(local))
                        && !carried.iter().any(|owner| owner.binding == binding)
                )
            })
            .map(|offset| start + offset)
    }

    /// Whether this selected arm already has an early-exit release before its
    /// carrier lifetime closes. Only that asymmetric shape needs a matching
    /// normal-fallthrough release before the scope join; an ordinary loop with
    /// all arms still live must retain its established single scope/backedge
    /// cleanup instead of being rewritten into per-arm drops.
    fn call_carrier_release_precedes_scope(
        &self,
        start_block_id: u32,
        start_instr_len: usize,
        owner: crate::model::OwnerId,
        place: Place,
        lifetime: (ScopeId, BindingId),
    ) -> bool {
        let mut pending = vec![start_block_id];
        let mut visited = HashSet::new();
        while let Some(block_id) = pending.pop() {
            if !visited.insert(block_id) {
                continue;
            }
            let (instructions, successors) = if block_id == self.current_block_id {
                (self.instructions.as_slice(), Vec::new())
            } else if let Some(block) = self
                .pending_blocks
                .iter()
                .find(|block| block.id == block_id)
            {
                (block.instructions.as_slice(), block.successors())
            } else {
                continue;
            };
            let start = if block_id == start_block_id {
                start_instr_len.min(instructions.len())
            } else {
                0
            };
            let boundary = Self::call_carrier_scope_boundary(
                instructions,
                start,
                base_local(place).expect("call carrier place is Local-backed"),
                Some(lifetime),
            );
            let end = boundary.unwrap_or(instructions.len());
            if instructions[start..end].iter().any(|instruction| {
                matches!(
                    instruction,
                    Instr::OwnershipEvent(crate::model::OwnershipEvent::Release {
                        owner: released,
                        place: released_place,
                    }) if *released == owner && *released_place == place
                )
            }) {
                return true;
            }
            if boundary.is_none() {
                pending.extend(successors);
            }
        }
        false
    }

    #[allow(
        clippy::too_many_arguments,
        clippy::too_many_lines,
        reason = "exhaustive Terminator match — adding new variants \
                  (most recently `MakeLambdaActor`) edges past the 100-line \
                  ceiling, while the exact optional lifetime boundary joins the \
                  existing traversal state without changing the function's \
                  structural responsibility (single yield-block walk)"
    )]
    #[allow(
        clippy::match_same_arms,
        reason = "`Trap` (diverging abort — no continuation to leak into) and \
                  `Return` (non-carrying function exit — the ReturnSlot Move \
                  is the escape, caught by the instruction scan) are both \
                  drop-safe for DIFFERENT reasons; folding them would let a \
                  future body-exiting terminator inherit the wrong \
                  justification"
    )]
    fn generator_yield_block_paths_drop_safe(
        &self,
        block_id: u32,
        start_block_id: u32,
        start_instr_len: usize,
        local: u32,
        lifetime: Option<(ScopeId, BindingId)>,
        visiting: &mut HashSet<u32>,
        memo: &mut HashMap<u32, bool>,
    ) -> bool {
        let allow_partial_carrier_neutralize =
            self.call_scrutinee_carrier_mint_locals.contains(&local)
                && self.locals.get(local as usize).is_some_and(|ty| {
                    !super::composite_own::direct_payload_has_registered_resource_record(
                        ty,
                        &self.enum_layouts,
                        &self.lifecycle_registry,
                    )
                });
        if let Some(ok) = memo.get(&block_id) {
            return *ok;
        }
        if block_id == self.current_block_id {
            let start = if block_id == start_block_id {
                start_instr_len
            } else {
                0
            };
            let end = Self::call_carrier_scope_boundary(&self.instructions, start, local, lifetime)
                .unwrap_or(self.instructions.len());
            return self.instructions[start..end]
                .iter()
                .enumerate()
                .all(|(offset, instr)| {
                    self.yield_share_instr_exempt
                        .contains(&(block_id, start + offset))
                        || (allow_partial_carrier_neutralize
                            && matches!(
                                instr,
                                Instr::NeutralizePayloadSlot {
                                    place: Place::MachineVariant { local: root, .. }
                                        | Place::EnumVariant { local: root, .. },
                                    ..
                                } if *root == local
                            ))
                        || !generator_yield_instr_escapes(instr, local)
                });
        }
        if !visiting.insert(block_id) {
            // A block already on the `visiting` stack is a loop back-edge: the
            // walk re-entered a block whose instructions + terminator were
            // already verified escape-free on the in-progress path before we
            // recursed into its successors. A back-edge is the loop's own
            // continuation, NOT a new escape site, so the path loops without
            // transferring ownership of the yielded value out of the body —
            // drop-safe. (See the not-yet-built note below for why fail-closed
            // here means LEAK, never double-free, so `true` is the safe answer
            // only because the first-visit escape scan already cleared the
            // body.)
            return true;
        }
        let Some(block) = self
            .pending_blocks
            .iter()
            .find(|block| block.id == block_id)
        else {
            // The walk reached a block that is neither the in-progress block
            // (`current_block_id`, handled above) nor yet in `pending_blocks`.
            // This is a FORWARD edge to a not-yet-lowered block — the consuming
            // body is still being built, and a `break`/`continue` restructures
            // the CFG so the body reaches loop-exit / loop-continuation targets
            // that the loop lowering will emit AFTER this drop scan runs.
            //
            // Such a forward target is never an escape site for the yielded
            // value: an ownership-transferring use (`Move` out, store into a
            // surviving aggregate, spawn capture, consuming terminator) is
            // emitted INLINE in the body, which is already built and was checked
            // by the first-visit escape scan on every block reached so far. A
            // not-yet-built continuation block carries only loop-structural
            // control flow. Treating it as drop-safe (`true`) preserves the
            // per-iteration body-end drop; the old conservative `false` here —
            // together with the back-edge `false` above — was the break/continue
            // leak: any `for v in gen()` body containing `break`/`continue`
            // suppressed the body-end drop for the WHOLE binding, leaking every
            // yielded heap value (verified 50 iters -> 100 leaks).
            //
            // Fail-closed direction check: the generator-yield drop's wrong
            // answer is a LEAK if over-suppressed, but a DOUBLE-FREE if
            // over-emitted. `true` here is safe ONLY because every real escape
            // is inline in an already-built block the scan visited first; the
            // forward block cannot introduce a new escape of a value bound in
            // this body.
            visiting.remove(&block_id);
            memo.insert(block_id, true);
            return true;
        };
        let ok = {
            let start = if block_id == start_block_id {
                start_instr_len
            } else {
                0
            };
            let boundary =
                Self::call_carrier_scope_boundary(&block.instructions, start, local, lifetime);
            let end = boundary.unwrap_or(block.instructions.len());
            // An escape in this block's instructions OR its terminator makes the
            // body-end drop unsound (the value left the body) — return false
            // immediately. Otherwise recurse into the successor(s).
            // Retain-backed share sites (`yield_share_instr_exempt` /
            // `yield_share_term_exempt`) are borrows of the binder's count —
            // the `+1` retain mints the destination's own owner — so they do
            // not suppress the body-end drop.
            let escapes_here =
                block.instructions[start..end]
                    .iter()
                    .enumerate()
                    .any(|(offset, instr)| {
                        !(self
                            .yield_share_instr_exempt
                            .contains(&(block_id, start + offset))
                            || (allow_partial_carrier_neutralize
                                && matches!(
                                    instr,
                                    Instr::NeutralizePayloadSlot {
                                        place: Place::MachineVariant { local: root, .. }
                                            | Place::EnumVariant { local: root, .. },
                                        ..
                                    } if *root == local
                                )))
                            && generator_yield_instr_escapes(instr, local)
                    })
                    || (boundary.is_none()
                        && !self.yield_share_term_exempt.contains(&(block_id, local))
                        && generator_yield_terminator_escapes(
                            &block.terminator,
                            self.suspend_kinds.get(&block.id),
                            local,
                        ));
            if escapes_here {
                false
            } else if boundary.is_some() {
                true
            } else {
                match &block.terminator {
                    Terminator::Goto { target } => self.generator_yield_block_paths_drop_safe(
                        *target,
                        start_block_id,
                        start_instr_len,
                        local,
                        lifetime,
                        visiting,
                        memo,
                    ),
                    Terminator::Call { next, .. }
                    | Terminator::MakeGenerator { next, .. }
                    | Terminator::MakeLambdaActor { next, .. } => self
                        .generator_yield_block_paths_drop_safe(
                            *next,
                            start_block_id,
                            start_instr_len,
                            local,
                            lifetime,
                            visiting,
                            memo,
                        ),
                    Terminator::Branch {
                        then_target,
                        else_target,
                        ..
                    } => {
                        self.generator_yield_block_paths_drop_safe(
                            *then_target,
                            start_block_id,
                            start_instr_len,
                            local,
                            lifetime,
                            visiting,
                            memo,
                        ) && self.generator_yield_block_paths_drop_safe(
                            *else_target,
                            start_block_id,
                            start_instr_len,
                            local,
                            lifetime,
                            visiting,
                            memo,
                        )
                    }
                    // Neither endpoint has a successor or transfers the
                    // generator's value. `Unreachable` is semantically
                    // impossible; `Trap` terminates the process. Both are
                    // therefore safe for the body-end drop analysis.
                    Terminator::Unreachable | Terminator::Trap { .. } => true,
                    // A `Return`-terminated path exits the function WITHOUT
                    // carrying the binding: `return v` moves the value through
                    // an `Instr::Move` into `Place::ReturnSlot`, and that Move
                    // is already classified as an escape by the instruction
                    // scan above. The return edge releases the current
                    // iteration's value itself (the return lowering fires the
                    // active yield-value ledger before sealing the block), and
                    // that edge is CFG-mutually-exclusive with the body-end /
                    // break-edge drops — so a non-carrying `Return` path is
                    // drop-safe. Answering false here poisoned the WHOLE
                    // binding: one early-return path suppressed the body-end
                    // drop and leaked every iteration's received value
                    // (#2412's early-return shape, one node per yield).
                    Terminator::Return => true,
                    // `Suspend` never appears in a generator body (gen bodies
                    // use `Yield`); a body-end drop across it is conservatively
                    // unsound here, like the other body-exiting terminators.
                    Terminator::Yield { .. }
                    | Terminator::Send { .. }
                    | Terminator::Ask { .. }
                    | Terminator::RemoteAsk { .. }
                    | Terminator::Suspend { .. }
                    | Terminator::SuspendingScopeDeadline { .. }
                    | Terminator::Select { .. }
                    | Terminator::SuspendingSelect { .. }
                    | Terminator::Join { .. } => false,
                }
            }
        };
        visiting.remove(&block_id);
        memo.insert(block_id, ok);
        ok
    }

    fn project_match_scrutinee_is_bitcopy(&self, ty: &ResolvedTy) -> bool {
        match self.subst_ty(ty) {
            ResolvedTy::Tuple(items) => items.iter().all(|item| {
                ValueClass::of_ty(&self.subst_ty(item), &self.type_classes) == ValueClass::BitCopy
            }),
            other => ValueClass::of_ty(&other, &self.type_classes) == ValueClass::BitCopy,
        }
    }

    /// True when `local`'s storage is an interior ALIAS of aggregate storage
    /// that another binding still owns: some defining write of it is a member
    /// field load (`RecordFieldLoad` / `TupleFieldLoad` byte-copy the member;
    /// non-string members are never retained) or a `Move` out of an
    /// interior-projection place (an enum/machine variant payload bind),
    /// possibly reached through whole-value `Move` copies of such a value.
    ///
    /// Consulted by `lower_match_project` to decide the skipped-field
    /// discharge for a partial destructure. Discharging THROUGH an alias
    /// frees heap the real owner's composite drop still walks: the in-place
    /// drop's null-store lands in the ALIAS slot, never the owner's, and the
    /// non-retaining leaf load+`Drop` path frees the original outright —
    /// both are double-frees once the owner's composite re-walks the field.
    /// An alias scrutinee therefore emits NO discharge and the owner's
    /// composite frees every original exactly once.
    ///
    /// The verdict is deliberately ANY-path: one interior defining write
    /// classifies the local as an alias. Misclassifying owned storage as an
    /// alias can only leak (the composite covers what the discharge would
    /// have freed, or — if the owner escaped — nothing frees it: the
    /// fail-closed direction), while misclassifying an alias as owned
    /// storage double-frees. Defining writes this walk does not model (call
    /// results, aggregate construction, constants) are owned storage and
    /// keep the discharge path.
    fn local_storage_is_interior_alias(&self, local: u32) -> bool {
        let mut visited: HashSet<u32> = HashSet::new();
        let mut work: Vec<u32> = vec![local];
        while let Some(l) = work.pop() {
            if !visited.insert(l) {
                continue;
            }
            let all_instrs = self
                .pending_blocks
                .iter()
                .flat_map(|b| b.instructions.iter())
                .chain(self.instructions.iter());
            for instr in all_instrs {
                match instr {
                    Instr::RecordFieldLoad { dest, .. } | Instr::TupleFieldLoad { dest, .. }
                        if matches!(dest, Place::Local(_)) && base_local(*dest) == Some(l) =>
                    {
                        return true;
                    }
                    Instr::Move { dest, src }
                        if matches!(dest, Place::Local(_)) && base_local(*dest) == Some(l) =>
                    {
                        if place_is_interior_projection(*src) {
                            return true;
                        }
                        if matches!(src, Place::Local(_)) {
                            if let Some(sl) = base_local(*src) {
                                work.push(sl);
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        false
    }

    /// Classified release verdict for an UNSELECTED owned field discarded in a
    /// record/tuple match destructure (the `_` arm on an owned-typed field),
    /// and for the functional-update override/carry pre-flights.
    ///
    /// Returns [`ReleaseSymbolVerdict::Wired`] only for field types whose drop
    /// is a single-`ptr` release the inline-drop dispatcher
    /// (`codegen-rs/llvm.rs :: lower_inline_drop`) is allowed to emit:
    ///   - `string` → `hew_string_drop`
    ///   - `bytes`  → `hew_bytes_drop` (triple-field-0 release)
    ///   - `Vec<T>` → `hew_vec_free` or descriptor-driven `hew_vec_free_owned`
    ///   - `HashMap<K,V>` → `hew_hashmap_free_layout`
    ///   - `HashSet<T>` → `hew_hashset_free_layout`
    ///   - `Generator<Y,R>` / `AsyncGenerator<Y>` → `hew_gen_coro_destroy`
    ///
    /// Returns [`ReleaseSymbolVerdict::Unwired`] for a `Vec` whose element
    /// release protocol is unwired (a `bytes` fat triple or an indirect-enum
    /// node — see `vec_release_symbol_verdict`): callers MUST refuse the
    /// construct at compile time; a `Wired`-gated pre-flight can no longer
    /// admit the buffer-only free over owned element nodes.
    ///
    /// Returns [`ReleaseSymbolVerdict::NoDropPath`] for owned-aggregate fields
    /// (records/tuples/enums) — their in-place drop is
    /// `DropKind::RecordInPlace` / `TupleInPlace` / `EnumInPlace`, NOT an
    /// inline `Instr::Drop`. The caller fails closed for these rather than
    /// emit a wrong-ABI free (leak-not-double-free posture).
    ///
    /// The symbol authority MUST agree with codegen's `resolved_ty_cow_heap_release`
    /// + Bytes-intercept in `lower_inline_drop` (`dedup-semantic-boundary`,
    ///   `lifecycle-symmetry`). A symbol absent from that authority would be
    ///   rejected at codegen-emit time as a wrong-ABI free.
    pub(crate) fn project_field_inline_drop_symbol(&self, ty: &ResolvedTy) -> ReleaseSymbolVerdict {
        match self.subst_ty(ty) {
            ResolvedTy::String => ReleaseSymbolVerdict::Wired("hew_string_drop"),
            ResolvedTy::Bytes => ReleaseSymbolVerdict::Wired("hew_bytes_drop"),
            ResolvedTy::Named {
                builtin: Some(hew_types::BuiltinType::Vec),
                ref args,
                ..
            } => {
                // The element's release bucket selects the Vec verdict through
                // `vec_release_symbol_verdict` — the same body the
                // generator-yield picker consults, so every drop-symbol
                // authority detects each bucket through one decision and
                // cannot drift. The dispatch checks the closure-pair bucket
                // FIRST, mirroring codegen's `resolved_ty_cow_heap_release` (see
                // `classify_vec_element_release`'s order doc; both
                // authorities are documented to agree. RecordFieldDrop
                // validates the closed symbol set plus the one distinct slot
                // ABI (`Bytes` iff `hew_bytes_drop` over the exact triple),
                // rather than rebuilding this same-pointer table in codegen
                // (`dedup-semantic-boundary`). The classification runs on
                // the SUBSTITUTED element (this match substitutes before
                // dispatching; the generator-yield picker classifies the raw
                // type — the asymmetry is pinned by
                // `yield_and_field_pickers_match_legacy_symbol_table`). A
                // no-type-arg `Vec` falls through to the plain buffer free.
                args.first()
                    .map_or(ReleaseSymbolVerdict::Wired("hew_vec_free"), |elem| {
                        self.vec_release_symbol_verdict(elem)
                    })
            }
            ResolvedTy::Named {
                builtin: Some(hew_types::BuiltinType::HashMap),
                ..
            } => ReleaseSymbolVerdict::Wired("hew_hashmap_free_layout"),
            ResolvedTy::Named {
                builtin: Some(hew_types::BuiltinType::HashSet),
                ..
            } => ReleaseSymbolVerdict::Wired("hew_hashset_free_layout"),
            ResolvedTy::Named {
                builtin:
                    Some(hew_types::BuiltinType::Generator | hew_types::BuiltinType::AsyncGenerator),
                ..
            } => ReleaseSymbolVerdict::Wired("hew_gen_coro_destroy"),
            _ => ReleaseSymbolVerdict::NoDropPath,
        }
    }

    /// THE shared fail-closed admissibility classifier for
    /// [`Instr::FieldDropInPlace`]: can codegen's address-based type-directed
    /// drop dispatcher (`emit_heap_slot_drop`, `hew-codegen-rs/src/llvm.rs`)
    /// discharge an owned field of this type at its field address, for every
    /// leaf the shape transitively reaches?
    ///
    /// ONE predicate answers both the MIR admission question ("may the
    /// safety-drop loop emit `FieldDropInPlace` for this skipped field?") and
    /// the drop-plan verifier's legality rule ("is this op's `ty` a shape the
    /// dispatcher can resolve?"), so MIR admission and codegen capability
    /// cannot drift (`dedup-semantic-boundary` — the same discipline
    /// `project_field_inline_drop_symbol` documents against codegen's
    /// `resolved_ty_cow_heap_release`).
    ///
    /// Admitted top-level shapes, mirroring `emit_heap_slot_drop`'s dispatch:
    ///   - user record with a registered layout — every field dischargeable
    ///     (the on-demand `__hew_record_drop_inplace_{name}` walk);
    ///   - tuple — every element dischargeable
    ///     (`emit_aggregate_recursive_drop`);
    ///   - fixed array — element dischargeable (per-element recursion);
    ///   - inline enum with a registered layout — every variant payload
    ///     dischargeable (`__hew_enum_drop_inplace_{name}` tag dispatch);
    ///   - indirect enum — every variant payload dischargeable (the recursive
    ///     `__hew_indirect_enum_free_{name}` node walk, dispatched FIRST in
    ///     codegen).
    ///
    /// Leaf COW types (`string`, `Vec`, …) are deliberately NOT admitted at
    /// top level — the admission OR (`leaf symbol || classifier`) keeps their
    /// discharge decision on `project_field_inline_drop_symbol`, and the
    /// `string` reroute onto `FieldDropInPlace` is its own decision (the
    /// retain-cancel), not a classifier verdict.
    ///
    /// Everything else — slices, `dyn Trait` fat fields, closure pairs,
    /// affine handles (`Channel` / `Task` / `CancellationToken`), opaque
    /// handles, free type params, unregistered layouts — is REFUSED
    /// (fail-closed: the caller keeps the NYI refusal; never a wrong-ABI
    /// free). Cycle-guarded on `Named` recursion exactly as
    /// `unsupported_vec_element_walk`.
    pub(crate) fn field_drop_in_place_admissible(&self, ty: &ResolvedTy) -> bool {
        let subst = self.subst_ty(ty);
        let mut visiting = HashSet::new();
        self.field_drop_aggregate_admissible(&subst, &mut visiting)
    }

    pub(crate) fn project_record_leaf_field_drop(
        &self,
        base: Place,
        field: u32,
        ty: &ResolvedTy,
    ) -> Option<Instr> {
        let ty = self.subst_ty(ty);
        let ReleaseSymbolVerdict::Wired(symbol) = self.project_field_inline_drop_symbol(&ty) else {
            return None;
        };
        Some(Instr::RecordFieldDrop {
            record: base,
            field_offset: FieldOffset(field),
            ty,
            drop_fn: crate::model::DropFnSpec::Release(symbol),
        })
    }

    /// The aggregate-shape half of [`Self::field_drop_in_place_admissible`]:
    /// true only for the five admitted aggregate shapes whose reachable
    /// leaves are all dischargeable. Operates on substituted types.
    fn field_drop_aggregate_admissible(
        &self,
        ty: &ResolvedTy,
        visiting: &mut HashSet<String>,
    ) -> bool {
        use crate::model::HeapOwnershipLayouts as _;
        match ty {
            ResolvedTy::Tuple(elems) => elems
                .iter()
                .all(|elem| self.field_drop_slot_dischargeable(elem, visiting)),
            ResolvedTy::Array(elem, _) => self.field_drop_slot_dischargeable(elem, visiting),
            ResolvedTy::Named {
                name,
                args,
                builtin: None,
                is_opaque: false,
            } => {
                // Cycle guard keyed on the mangled head (an `indirect enum`
                // references itself); a shape already on the walk stack is
                // admissible AT THIS EDGE — the outer frame still decides.
                let key = hew_hir::mangle_resolved_ty(ty);
                if !visiting.insert(key.clone()) {
                    return true;
                }
                let layouts = crate::model::MirHeapLayouts {
                    record_field_orders: &self.record_field_orders,
                    enum_layouts: &self.enum_layouts,
                };
                let verdict = if let Some(field_tys) = layouts.record_field_tys(name, args, None) {
                    field_tys
                        .iter()
                        .all(|field_ty| self.field_drop_slot_dischargeable(field_ty, visiting))
                } else if let Some(variants) = layouts.enum_variant_field_tys(name, args) {
                    // Inline and indirect enums both admit through their
                    // registered layout: the inline shape drops through the
                    // on-demand `__hew_enum_drop_inplace_{name}` tag walk, the
                    // indirect shape through the recursive
                    // `__hew_indirect_enum_free_{name}` node free. Either way
                    // every variant payload must be dischargeable.
                    variants
                        .iter()
                        .flatten()
                        .all(|payload_ty| self.field_drop_slot_dischargeable(payload_ty, visiting))
                } else {
                    // No registered layout to walk — refuse (fail-closed).
                    false
                };
                visiting.remove(&key);
                verdict
            }
            _ => false,
        }
    }

    /// Is one interior slot of an admitted aggregate dischargeable by the
    /// codegen dispatcher? A slot passes when it is an admitted aggregate
    /// shape, a leaf the dispatcher releases in place, or a shape owning no
    /// heap (nothing to discharge). Fail-closed on everything else.
    fn field_drop_slot_dischargeable(
        &self,
        ty: &ResolvedTy,
        visiting: &mut HashSet<String>,
    ) -> bool {
        let subst = self.subst_ty(ty);
        // Indirect enums FIRST: the structural heap-ownership authority is
        // blind to indirection (a scalar-payload `indirect enum` owns a heap
        // node the authority reports as non-owning), so the layout probe must
        // precede the ownership shortcut below.
        if ty_is_indirect_enum(&subst, &self.enum_layouts) {
            return self.field_drop_aggregate_admissible(&subst, visiting);
        }
        // Shapes the dispatcher fail-closes on are refused OUTRIGHT — even
        // where the ownership authority reports them non-owning (a closure
        // pair owns its env box behind a non-owning-classified fn surface; a
        // free type param has no layout to walk).
        if matches!(
            subst,
            ResolvedTy::Slice(_)
                | ResolvedTy::TraitObject { .. }
                | ResolvedTy::Function { .. }
                | ResolvedTy::Closure { .. }
                | ResolvedTy::Task(_)
                | ResolvedTy::TypeParam { .. }
        ) {
            return false;
        }
        // A slot owning no heap needs no discharge.
        if !crate::model::ty_owns_heap_mir(&subst, &self.record_field_orders, &self.enum_layouts) {
            return true;
        }
        match &subst {
            // Leaf shapes `emit_heap_slot_drop` releases in place: the
            // pointer/fat leaves (`string` / `bytes`, null-store
            // postcondition) and the handle leaves with a wired release
            // symbol (`HashMap` / `HashSet` / `Generator` /
            // `AsyncGenerator` / `Sink` / `Stream`).
            ResolvedTy::String
            | ResolvedTy::Bytes
            | ResolvedTy::Named {
                builtin:
                    Some(
                        hew_types::BuiltinType::HashMap
                        | hew_types::BuiltinType::HashSet
                        | hew_types::BuiltinType::Generator
                        | hew_types::BuiltinType::AsyncGenerator
                        | hew_types::BuiltinType::Sink
                        | hew_types::BuiltinType::Stream,
                    ),
                ..
            } => true,
            // The Vec element's release-bucket question routes through the
            // one typed classification (`classify_vec_element_release`) —
            // an element no bucket claims keeps the slot refused
            // (fail-closed), never a buffer-only free over leaking element
            // nodes.
            ResolvedTy::Named {
                builtin: Some(hew_types::BuiltinType::Vec),
                args,
                ..
            } => args.first().is_some_and(|elem| {
                !matches!(
                    self.classify_vec_element_release(elem),
                    VecElementRelease::Unsupported(_)
                )
            }),
            other => self.field_drop_aggregate_admissible(other, visiting),
        }
    }

    /// Field-precise heap-owning-field enumeration for a record/tuple match
    /// destructure scrutinee. For each field that transitively owns heap
    /// storage, returns `(field_idx, substituted_type)`. Used by
    /// `lower_match_project` to compute the fields needing explicit-drop
    /// emission for the partial-extraction case, and by record overwrite
    /// release to enumerate the leaves that must be discharged.
    ///
    /// The layout-aware heap-ownership authority excludes direct
    /// payload-free/scalar-payload enums: they are non-BitCopy values but own
    /// no heap, and treating one as an owner makes its absent release symbol
    /// veto the real owning siblings of a record. A non-BitCopy shape that the
    /// in-place dispatcher cannot discharge (notably a closure with a hidden
    /// environment box) remains in the list so its caller fails closed rather
    /// than silently skipping an unsupported owner.
    pub(crate) fn project_record_owned_field_list(
        &self,
        ty: &ResolvedTy,
    ) -> Vec<(u32, ResolvedTy)> {
        let subst = self.subst_ty(ty);
        let Some(key) = user_record_layout_key(&subst) else {
            return Vec::new();
        };
        let Some(field_order) = self.lookup_record_field_order(&key) else {
            return Vec::new();
        };
        field_order
            .iter()
            .enumerate()
            .filter_map(|(idx, (_name, field_ty))| {
                let substituted = self.subst_ty(field_ty);
                let owns_heap = crate::model::ty_owns_heap_mir(
                    &substituted,
                    &self.record_field_orders,
                    &self.enum_layouts,
                );
                let unsupported_non_bitcopy = self.binding_seeds_drop_elaboration(&substituted)
                    && !self.field_drop_slot_dischargeable(&substituted, &mut HashSet::new());
                if owns_heap || unsupported_non_bitcopy {
                    u32::try_from(idx).ok().map(|i| (i, substituted))
                } else {
                    None
                }
            })
            .collect()
    }

    /// #2420 -- may the value of `expr`, used as the RHS of `binding = expr`,
    /// embed an UN-RETAINED alias of `binding`'s old owned heap?
    ///
    /// The overwrite release (`emit_local_overwrite_release`) frees the old
    /// value's owned fields in place before the store. That is sound only when
    /// the incoming value cannot reference the same heap. Two shapes break it,
    /// both rooted in the RHS reading the reassigned binding:
    ///
    /// - `s = grow(s)` -- a by-value heap param is a BORROW (LESSONS
    ///   `by-value-heap-params-are-borrows`), and the callee's non-`string`
    ///   owned field load (`S { v: s.v }`) is a raw pointer copy with no
    ///   retain, so the returned value aliases the caller's old heap;
    /// - `s = S { n: s.n + 1, v: s.v }` -- the caller-side literal embeds the
    ///   projection directly.
    ///
    /// Fail-closed allowlist mirroring `return_value_may_alias_borrow`, with
    /// the leaf parameterised to "a read of `binding`" instead of "any
    /// parameter": wrappers recurse all reachable values, constructions
    /// recurse operands, a call may alias iff its callee is not
    /// summary-proven fresh (`fresh_owner_verdicts`) AND some argument
    /// may alias, projections recurse their object chain, and every unmodelled
    /// form answers `true`. Two deliberate refinements:
    ///
    /// - TYPE CUT: a value whose type has no un-retained owned leaf
    ///   (`ty_has_unretained_owned_leaf`) can never alias the released
    ///   storage -- `s.n + 1` (`BitCopy`) and string-only records (every
    ///   `string` aggregate load is retained `+1`) keep today's exact
    ///   release/free balance.
    /// - A bare read of a DIFFERENT binding answers `false` (release fires,
    ///   preserving the `s = s2` rebind free). An intra-function alias built
    ///   through another local (`let t = S { v: s.v }; s = t`) is a
    ///   PRE-EXISTING hole of the projection-alias machinery, not widened
    ///   here; closing it needs binding-level alias provenance.
    ///   WHEN-OBSOLETE: the COW retain-on-share spine retires this predicate
    ///   entirely (every share retained => the release is always sound).
    pub(crate) fn reassign_rhs_may_alias_binding(
        &self,
        expr: &HirExpr,
        binding: BindingId,
    ) -> bool {
        // A value that cannot carry an un-retained owned leaf cannot alias the
        // storage the overwrite release frees.
        if !self.ty_has_unretained_owned_leaf(&expr.ty) {
            return false;
        }
        match &expr.kind {
            // Value-passthrough wrappers: aliasing iff ANY reachable value
            // aliases. A missing tail/else cannot produce the owned value in
            // the first place, but stay fail-closed to mirror the summary walk.
            HirExprKind::Block(block) => block
                .tail
                .as_deref()
                .is_none_or(|t| self.reassign_rhs_may_alias_binding(t, binding)),
            HirExprKind::If {
                then_expr,
                else_expr,
                ..
            } => {
                self.reassign_rhs_may_alias_binding(then_expr, binding)
                    || else_expr
                        .as_deref()
                        .is_none_or(|e| self.reassign_rhs_may_alias_binding(e, binding))
            }
            HirExprKind::Match { arms, .. } => {
                arms.is_empty()
                    || arms
                        .iter()
                        .any(|arm| self.reassign_rhs_may_alias_binding(&arm.body, binding))
            }
            HirExprKind::Return { value } => value
                .as_deref()
                .is_none_or(|v| self.reassign_rhs_may_alias_binding(v, binding)),
            // Fresh leaves: a `.clone()` is a deep copy; a `Vec<T>` element
            // load / slice is an independent element (push-clone + refcount);
            // a literal owns nothing borrowed.
            HirExprKind::RecordCloneCall { .. }
            | HirExprKind::Index { .. }
            | HirExprKind::Slice { .. }
            | HirExprKind::Literal(_) => false,
            // Constructions alias iff an operand does.
            HirExprKind::StructInit { fields, base, .. } => {
                fields
                    .iter()
                    .any(|(_, v)| self.reassign_rhs_may_alias_binding(v, binding))
                    || base
                        .as_deref()
                        .is_some_and(|b| self.reassign_rhs_may_alias_binding(b, binding))
            }
            HirExprKind::TupleLiteral { elements } => elements
                .iter()
                .any(|e| self.reassign_rhs_may_alias_binding(e, binding)),
            HirExprKind::MachineVariantCtor { payload, .. } => {
                payload.as_ref().is_some_and(|fields| {
                    fields
                        .iter()
                        .any(|(_, v)| self.reassign_rhs_may_alias_binding(v, binding))
                })
            }
            // A call's result may alias `binding` iff the callee is not
            // statically resolvable (closure / fn-pointer -- a hidden capture
            // can smuggle the binding), or it is not summary-proven to return
            // a fresh owner AND some argument itself may alias (the callee can
            // forward that argument's heap into its return).
            HirExprKind::Call { callee, args, .. } => {
                !callee_is_resolved_item(callee)
                    || (!callee_returns_fresh_owner(
                        callee,
                        &self.call_scrutinee_provenance.fresh_owner_verdicts,
                    ) && args
                        .iter()
                        .any(|a| self.reassign_rhs_may_alias_binding(a, binding)))
            }
            // A projection aliases iff its object chain reaches the binding.
            HirExprKind::FieldAccess { object, .. } => {
                self.reassign_rhs_may_alias_binding(object, binding)
            }
            HirExprKind::TupleIndex { tuple, .. } => {
                self.reassign_rhs_may_alias_binding(tuple, binding)
            }
            // THE leaf: a read of the reassigned binding itself. A different
            // binding or an Item/Const ref is not the old value (see the doc
            // note on the pre-existing local-launder hole).
            HirExprKind::BindingRef { resolved, .. } => {
                matches!(resolved, ResolvedRef::Binding(id) if *id == binding)
            }
            // Compiler-known `Rc`/`Weak` operations. Without an arm these fell
            // to the conservative tail, which vetoed the overwrite release for
            // every `shared = Rc.new(..)` and leaked the outgoing generation.
            //
            // `New` allocates: `hew_rc_new` COPIES the payload into a fresh
            // block, so the handle itself can never be the old one. Its payload
            // can still embed an un-retained leaf read out of the old value
            // (`Rc.new(Node { v: old.get().v })`), so it takes the ordinary
            // construction rule — aliasing iff its operand aliases.
            //
            // `Clone` / `WeakClone` / `Downgrade` / `WeakUpgrade` are RETAINED
            // mints. They may well hand back the same allocation, but with the
            // count already incremented: the RHS is lowered before the release
            // is emitted, so `+1` strictly precedes the `-1` and the allocation
            // cannot reach zero across the overwrite. Releasing the outgoing
            // generation is exactly the balancing `-1` — vetoing it is what
            // leaks `shared = shared.clone()`.
            //
            // `GetCopy` returns a shallow copy of the PAYLOAD, which does share
            // the payload's un-retained leaves, and `Set` / the count and
            // uniqueness queries have no owned result. All keep the fail-closed
            // tail.
            HirExprKind::RcIntrinsic { op, value, .. } => match op {
                hew_types::RcIntrinsicOp::New => value
                    .as_deref()
                    .is_none_or(|v| self.reassign_rhs_may_alias_binding(v, binding)),
                hew_types::RcIntrinsicOp::Clone
                | hew_types::RcIntrinsicOp::WeakClone
                | hew_types::RcIntrinsicOp::Downgrade
                | hew_types::RcIntrinsicOp::WeakUpgrade => false,
                hew_types::RcIntrinsicOp::GetCopy
                | hew_types::RcIntrinsicOp::Set
                | hew_types::RcIntrinsicOp::StrongCount
                | hew_types::RcIntrinsicOp::WeakCount
                | hew_types::RcIntrinsicOp::IsUnique => true,
            },
            // Method calls (can return borrowed `self`), derefs, operators
            // over owned values, and every future form: fail closed.
            _ => true,
        }
    }

    /// True when `ty` transitively contains an owned heap leaf that aggregate
    /// loads share WITHOUT a retain -- the alias channel the overwrite release
    /// must respect (#2420).
    ///
    /// `string` is NOT such a leaf: every `string` aggregate field/element
    /// load is retained `+1` in codegen (`retain_string_field_load`), so a
    /// shared string never dangles when the old owner is released. `BitCopy`
    /// types own no heap. Everything else that owns heap -- Vec / `HashMap` /
    /// `HashSet` / Generator / bytes, enums with owned payloads, and any
    /// unmodelled owner -- is a raw-shared leaf; user records and tuples
    /// recurse their fields (value-recursive records are impossible by
    /// construction, so the recursion terminates).
    fn ty_has_unretained_owned_leaf(&self, ty: &ResolvedTy) -> bool {
        let ty = self.subst_ty(ty);
        if !crate::model::ty_owns_heap_mir(&ty, &self.record_field_orders, &self.enum_layouts) {
            return false;
        }
        match &ty {
            ResolvedTy::String | ResolvedTy::Bytes => false,
            ResolvedTy::Tuple(items) => items
                .iter()
                .any(|item| self.ty_has_unretained_owned_leaf(item)),
            _ => {
                if let Some(key) = user_record_layout_key(&ty) {
                    if let Some(field_order) = self.lookup_record_field_order(&key) {
                        // Clone the field types out so the `&self` borrow is
                        // released before the recursive calls.
                        let field_tys: Vec<ResolvedTy> =
                            field_order.iter().map(|(_, fty)| fty.clone()).collect();
                        return field_tys
                            .iter()
                            .any(|fty| self.ty_has_unretained_owned_leaf(fty));
                    }
                }
                // Vec / HashMap / HashSet / Generator / bytes, owned-payload
                // enums, unregistered records, opaque owners: fail closed.
                true
            }
        }
    }

    /// Release an `Rc` / `Weak` slot overwritten at a reassignment generation
    /// boundary. Returns `true` when it emitted the release, meaning the caller
    /// must not fall through to the aggregate arms.
    ///
    /// The outgoing handle's single count must be released at the overwrite or
    /// it is never released at all: the binding's scope-exit drop only ever
    /// discharges the LAST generation, so a skipped release here is a leak no
    /// later drop can recover.
    ///
    /// Selected here rather than in `project_field_inline_drop_symbol`, which
    /// deliberately has no `Rc`/`Weak` arm — that picker is shared with the
    /// Vec-element, record-field and generator-yield release paths, and widening
    /// it would change all of them. This is the overwrite seam only.
    ///
    /// The symbols are the ones `generator_yield_drop_symbol` already wires for
    /// these types, and codegen already admits both as inline `Instr::Drop`
    /// rituals (`is_known_cow_heap_drop_symbol`): load the slot, call the
    /// null-tolerant release, null the slot. The immediately following `Move`
    /// installs the new generation.
    ///
    /// Reached only through the flag-gated arm in `assign`, so a handle already
    /// transferred into an aggregate (flag == 1) never arrives here — that would
    /// be the double-free one step along.
    fn emit_refcounted_overwrite_release(&mut self, dest: Place, ty: &ResolvedTy) -> bool {
        let Some(symbol) = refcounted_overwrite_release_symbol(ty) else {
            return false;
        };
        self.push_instr(Instr::Drop {
            place: dest,
            ty: ty.clone(),
            drop_fn: Some(crate::model::DropFnSpec::Release(symbol)),
        });
        true
    }

    /// Release the heap-owning OLD value of a `var`-local slot before a
    /// reassignment store overwrites it, mirroring the actor state-field
    /// overwrite release. Without this, `r = make()` in a loop leaks the prior
    /// record every iteration: the bare `Instr::Move` blindly overwrites `dest`
    /// and only the final value is freed at scope exit (#53). State fields ride
    /// `__hew_record_overwrite_release` via `ActorStateFieldStore`; this is the
    /// var-local analogue, built from the same per-leaf release symbols the
    /// functional-update override-drop uses so codegen's congruence assert
    /// agrees.
    ///
    /// Caller-proven precondition: the binding is in `owned_locals` (the live
    /// sole owner) so a `..base`/move-out RHS that consumed it has already
    /// removed it and this never runs (no double-free). Fail-open: a shape with
    /// no congruent leaf symbol (enum, nested aggregate field) emits nothing and
    /// leaks as before -- never a partial or wrong-ABI free.
    #[allow(
        clippy::too_many_lines,
        reason = "typed overwrite release keeps every supported owner shape in one dispatch"
    )]
    fn publish_overwrite_owner_release(
        &mut self,
        binding: BindingId,
        place: Place,
        guard: Option<Place>,
    ) {
        let Some(generation) = self.owner_generations.get(&binding).copied() else {
            return;
        };
        let owner = crate::model::OwnerId {
            binding,
            generation,
        };
        let event = guard.map_or(
            crate::model::OwnershipEvent::Release { owner, place },
            |flag| crate::model::OwnershipEvent::GuardedRelease { owner, place, flag },
        );
        self.push_instr(Instr::OwnershipEvent(event));
    }

    #[allow(
        clippy::too_many_lines,
        reason = "overwrite lowering publishes the physical guard, release, generation reset, and recipe together"
    )]
    pub(crate) fn emit_local_overwrite_release(
        &mut self,
        binding: BindingId,
        dest: Place,
        target_ty: &ResolvedTy,
        guard: Option<Place>,
    ) {
        let ty = self.subst_ty(target_ty);
        // A `var` reassignment is a GENERATION BOUNDARY: the slot's previous
        // value is a distinct owner whose obligation must be discharged HERE,
        // before the store — not inferred from the binding's entry-time
        // classification (the value-context lattice's `AssignmentOverwrite`
        // rule, `drop_obligation.rs`). For a registered closeable `#[resource]`
        // (record or opaque lifecycle) the discharge is its exactly-once
        // `close(self)`: `var t = Tok{1}; t = Tok{2};` must close 1 at the
        // rebind and close 2 at scope exit. The inline drop's typed-zero
        // null-after-close plus the immediately following `Move` store keeps a
        // second release of generation 1 structurally unreachable.
        if matches!(
            super::named_type_marker(&ty, &self.type_classes),
            Some(hew_hir::ResourceMarker::Resource)
        ) {
            if let Some(spec) = super::resource_drop_fn(&ty, &self.type_classes) {
                self.push_instr(Instr::Drop {
                    place: dest,
                    ty,
                    drop_fn: Some(spec),
                });
                self.publish_overwrite_owner_release(binding, dest, guard);
            }
            // A resource with no resolvable close is rejected upstream
            // (E_RESOURCE_MISSING_CLOSE); nothing further to release either
            // way — `close(self)` consumes the whole value.
            return;
        }
        if self.emit_refcounted_overwrite_release(dest, &ty) {
            self.publish_overwrite_owner_release(binding, dest, guard);
            return;
        }
        // Single-pointer / fat-triple COW leaf (string / Vec / HashMap /
        // HashSet / Generator / bytes): drop the whole slot in place. Only a
        // Wired verdict emits; an Unwired `Vec` (element release unwired)
        // falls through and emits nothing — its binding stayed in
        // `owned_locals`, so `unsupported_vec_element_diagnostics` rejects
        // the function at compile time before this leak could run.
        if let ReleaseSymbolVerdict::Wired(symbol) = self.project_field_inline_drop_symbol(&ty) {
            self.push_instr(Instr::Drop {
                place: dest,
                ty,
                drop_fn: Some(crate::model::DropFnSpec::Release(symbol)),
            });
            self.publish_overwrite_owner_release(binding, dest, guard);
            return;
        }
        // A record or tuple whose OLD generation transitively carries a close
        // obligation (`Holder { tok: Tok }`, `(Tok, i64)`): release the WHOLE
        // old value through the same in-place walk its scope-exit drop uses
        // (`__hew_record_drop_inplace_<R>` is resource-aware close-then-
        // teardown; the aggregate-recursive walk routes a resource element
        // through it). The inline `InPlace` drop typed-zeroes the slot after
        // the walk, and the immediately following `Move` installs the fresh
        // generation -- so the binding's own scope-exit drop still releases
        // generation 2, and no per-field load/field-drop ever addresses the
        // root (which the sole-owner provers would read as a partial free and
        // answer by excluding the scope-exit drop: the leak this arm replaces).
        // The wrong-axis per-field filter previously skipped a scalar-field
        // resource entirely, leaking one resource per reassignment.
        let carries_close = crate::model::ty_drop_obligation(
            &ty,
            &crate::model::MirHeapLayouts {
                record_field_orders: &self.record_field_orders,
                enum_layouts: &self.enum_layouts,
            },
            self.type_classes.lifecycle_registry(),
        )
        .needs_close
            || matches!(
                ValueClass::of_ty(&ty, &self.type_classes),
                ValueClass::AffineResource | ValueClass::Linear
            );
        if carries_close && user_record_layout_key(&ty).is_some() {
            if self.field_drop_in_place_admissible(&ty) {
                self.push_instr(Instr::Drop {
                    place: dest,
                    ty,
                    drop_fn: Some(crate::model::DropFnSpec::InPlace(
                        crate::ownership::InPlaceReleaseKind::Record,
                    )),
                });
                self.publish_overwrite_owner_release(binding, dest, guard);
            }
            // Inadmissible shapes keep the fail-open skip (leak, never a
            // partial or wrong-ABI free).
            return;
        }
        let tuple_owns_heap = matches!(ty, ResolvedTy::Tuple(_))
            && crate::model::ty_owns_heap_mir(&ty, &self.record_field_orders, &self.enum_layouts);
        if matches!(ty, ResolvedTy::Tuple(_)) && (carries_close || tuple_owns_heap) {
            if self.field_drop_in_place_admissible(&ty) {
                self.push_instr(Instr::Drop {
                    place: dest,
                    ty,
                    drop_fn: Some(crate::model::DropFnSpec::InPlace(
                        crate::ownership::InPlaceReleaseKind::AggregateRecursive,
                    )),
                });
                self.publish_overwrite_owner_release(binding, dest, guard);
            }
            return;
        }
        // User record with heap-only obligations: release every owned field in
        // declaration order, the same per-field route the functional-update
        // override-drop takes. Skip the whole release unless EVERY owned field
        // has a known leaf symbol -- a nested record/enum field has none, and a
        // partial free would leak the rest while risking a wrong-ABI release.
        if user_record_layout_key(&ty).is_some() {
            let owned = self.project_record_owned_field_list(&ty);
            if owned.iter().any(|(_, fty)| {
                !matches!(
                    self.project_field_inline_drop_symbol(fty),
                    ReleaseSymbolVerdict::Wired(_)
                )
            }) {
                return;
            }
            let mut emitted = false;
            for (idx, fty) in owned {
                let ReleaseSymbolVerdict::Wired(symbol) =
                    self.project_field_inline_drop_symbol(&fty)
                else {
                    continue;
                };
                let offset = FieldOffset(idx);
                if field_override_uses_record_field_drop(&fty) {
                    self.push_instr(Instr::RecordFieldDrop {
                        record: dest,
                        field_offset: offset,
                        ty: fty,
                        drop_fn: crate::model::DropFnSpec::Release(symbol),
                    });
                    emitted = true;
                } else {
                    let old_val = self.alloc_local(fty.clone());
                    self.push_instr(Instr::RecordFieldLoad {
                        record: dest,
                        field_offset: offset,
                        dest: old_val,
                    });
                    self.push_instr(Instr::Drop {
                        place: old_val,
                        ty: fty,
                        drop_fn: Some(crate::model::DropFnSpec::Release(symbol)),
                    });
                    emitted = true;
                }
            }
            if emitted {
                self.publish_overwrite_owner_release(binding, dest, guard);
            }
        }
    }

    /// Preserve exactly one release authority for an inline enum's old active
    /// payload across a proven non-aliasing constructor reassignment.
    ///
    /// `emit_local_overwrite_release` intentionally handles leaves and records
    /// only. An inline enum needs its tag-aware in-place drop, and emitting that
    /// release while a match binder can still read the old payload would create
    /// a use-after-free. The caller has already proved
    /// `!reassign_rhs_may_alias_binding(value, binding)`, so a payload-bearing
    /// fresh constructor is just as independent as a unit variant. This helper
    /// handles the enum- and lexical-specific cases:
    ///
    /// * the target is a heap-owning, direct enum;
    /// * the incoming value is an enum constructor; and
    /// * with no live projected payload aliases, drop the old tagged value
    ///   immediately;
    /// * when direct live `string` binders cover every heap-owning field of the
    ///   selected variant, transfer the old generation's release to guarded
    ///   binder scope-exit drops; and
    /// * reject live payload shapes that cannot yet express that complete
    ///   delayed-release transfer.
    ///
    /// A move-out neutralizes the old payload slot before reassignment and does
    /// not need this cleanup. Missing scope facts conservatively classify an
    /// alias as live; an owned unsupported live alias produces a diagnostic
    /// rather than compiling a known leak or risking a double-free.
    #[allow(
        clippy::too_many_lines,
        reason = "the release-authority decision and its emitted CFG must stay adjacent so \
                  immediate, delayed, repeated-generation, and fail-closed cases cannot drift"
    )]
    pub(crate) fn emit_enum_overwrite_release(
        &mut self,
        binding: BindingId,
        dest: Place,
        target_ty: &ResolvedTy,
        value: &HirExpr,
        guard: Option<Place>,
    ) {
        let ty = self.subst_ty(target_ty);
        if !super::ty_is_heap_owning_enum_composite(
            &ty,
            &self.record_field_orders,
            &self.enum_layouts,
            self.type_classes.lifecycle_registry(),
        ) {
            return;
        }
        let ResolvedTy::Named { name, args, .. } = &ty else {
            return;
        };
        let Some(layout) = crate::model::find_enum_layout(name, args, &self.enum_layouts) else {
            return;
        };
        if layout.is_indirect {
            return;
        }
        let HirExprKind::MachineVariantCtor { state_idx, .. } = &value.kind else {
            return;
        };
        if layout.variants.get(*state_idx).is_none() {
            return;
        }
        // A reassigned `while let` scrutinee has already copied this old
        // generation into a dedicated iteration snapshot. Its back-edge/exit
        // drop is the sole release authority; dropping the parent slot here
        // would free bytes that the snapshot's live payload binders still
        // reference. This is the pre-existing while-let ownership protocol,
        // orthogonal to ordinary match-arm aliases over the parent slot.
        if self.active_while_let_snapshot_parents.contains(&binding) {
            return;
        }
        let matching_aliases = self
            .projected_payload_provenance
            .iter()
            .filter_map(|(payload_binding, provenance)| {
                let ProjectedPayloadOrigin::OwnedBinding(scrutinee) = &provenance.origin else {
                    return None;
                };
                (scrutinee.binding == binding)
                    .then_some((*payload_binding, provenance.source_place))
            })
            .collect::<Vec<_>>();
        if matching_aliases.iter().any(|(payload_binding, _)| {
            self.binding_scope
                .get(payload_binding)
                .is_none_or(|scope| self.active_scopes.contains(scope))
        }) {
            // Direct, still-live string binders can take over the old payload
            // generation's release after the store when together they cover
            // every heap-owning field in the selected variant. More complex
            // alias shapes reject until their delayed-release protocol is
            // represented.
            let mut active = matching_aliases
                .iter()
                .filter(|(payload_binding, _)| {
                    self.binding_scope
                        .get(payload_binding)
                        .is_some_and(|scope| self.active_scopes.contains(scope))
                })
                .copied()
                .collect::<Vec<_>>();
            active.sort_by_key(|(_, source)| match source {
                Place::MachineVariant {
                    variant_idx,
                    field_idx,
                    ..
                } => (*variant_idx, *field_idx),
                _ => (u32::MAX, u32::MAX),
            });
            let Some(Place::MachineVariant { variant_idx, .. }) =
                active.first().map(|(_, source)| *source)
            else {
                return;
            };
            let mut active_fields = active
                .iter()
                .filter_map(|(_, source)| match source {
                    Place::MachineVariant {
                        variant_idx: source_variant,
                        field_idx,
                        ..
                    } if *source_variant == variant_idx => Some(*field_idx as usize),
                    _ => None,
                })
                .collect::<Vec<_>>();
            active_fields.sort_unstable();
            let heap_fields = layout
                .variants
                .get(variant_idx as usize)
                .map(|variant| {
                    variant
                        .field_tys
                        .iter()
                        .enumerate()
                        .filter(|(_, field_ty)| {
                            crate::model::ty_owns_heap_mir(
                                &self.subst_ty(field_ty),
                                &self.record_field_orders,
                                &self.enum_layouts,
                            )
                        })
                        .map(|(index, _)| index)
                        .collect::<Vec<_>>()
                })
                .unwrap_or_default();
            let promoted = active
                .iter()
                .filter_map(|(payload_binding, _)| {
                    let binder_still_owns = self.owned_locals.iter().any(|entry| {
                        entry.binding == *payload_binding
                            && entry.disposition == Disposition::ScopeExit
                            && matches!(entry.ty, ResolvedTy::String)
                    });
                    (binder_still_owns
                        && self
                            .projected_payload_overwrite_flags
                            .contains_key(payload_binding))
                    .then(|| {
                        (
                            *payload_binding,
                            self.projected_payload_overwrite_flags[payload_binding],
                        )
                    })
                })
                .collect::<Vec<_>>();
            let active_scope_exit_owners = active
                .iter()
                .filter(|(payload_binding, _)| {
                    self.owned_locals.iter().any(|entry| {
                        entry.binding == *payload_binding
                            && entry.disposition == Disposition::ScopeExit
                    })
                })
                .count();
            if !heap_fields.is_empty()
                && active_fields == heap_fields
                && promoted.len() == active.len()
            {
                if self.in_fallthrough_match_guard {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::NotYetImplemented {
                            construct: "enum overwrite in a fallthrough match guard".to_string(),
                            site: value.site,
                        },
                        note: "overwriting an enum while its payload binder is live in a \
                                   guard needs edge-specific delayed release on the guard-false \
                                   path; bind the guard result without mutating the scrutinee"
                            .to_string(),
                    });
                    return;
                }
                let first_transfer = promoted.iter().all(|(payload_binding, _)| {
                    !self
                        .projected_payload_delayed_releases
                        .contains(payload_binding)
                });
                self.projected_payload_delayed_releases
                    .extend(promoted.iter().map(|(payload_binding, _)| *payload_binding));
                if first_transfer {
                    for (_, flag) in &promoted {
                        self.push_instr(Instr::ConstI64 {
                            dest: *flag,
                            value: 0,
                        });
                    }
                } else {
                    // Every field flag is updated in lockstep. Reading the
                    // first one is therefore a complete generation test for
                    // the parent variant.
                    let flag = promoted[0].1;
                    // The same arm may overwrite the parent more than
                    // once. `flag == 0` means an earlier executed overwrite
                    // already moved the original generation's release to
                    // the binders, so this overwrite must release the
                    // parent's current (newer) generation normally.
                    // `flag == 1` means the earlier syntactic overwrite was
                    // on an untaken path; this is the first runtime
                    // transfer and must preserve the binders' old payloads.
                    let zero = self.alloc_local(ResolvedTy::I64);
                    self.push_instr(Instr::ConstI64 {
                        dest: zero,
                        value: 0,
                    });
                    let parent_has_newer_generation = self.alloc_local(ResolvedTy::Bool);
                    self.push_instr(Instr::IntCmp {
                        dest: parent_has_newer_generation,
                        pred: CmpPred::Eq,
                        lhs: flag,
                        rhs: zero,
                    });
                    let release_bb = self.alloc_block();
                    let transfer_bb = self.alloc_block();
                    let cont_bb = self.alloc_block();
                    self.finish_current_block(Terminator::Branch {
                        cond: parent_has_newer_generation,
                        then_target: release_bb,
                        else_target: transfer_bb,
                    });
                    self.start_block(release_bb);
                    self.push_instr(Instr::Drop {
                        place: dest,
                        ty: ty.clone(),
                        drop_fn: Some(crate::model::DropFnSpec::InPlace(
                            crate::ownership::InPlaceReleaseKind::Enum,
                        )),
                    });
                    self.publish_overwrite_owner_release(binding, dest, guard);
                    self.finish_current_block(Terminator::Goto { target: cont_bb });
                    self.start_block(transfer_bb);
                    for (_, transfer_flag) in &promoted {
                        self.push_instr(Instr::ConstI64 {
                            dest: *transfer_flag,
                            value: 0,
                        });
                    }
                    self.finish_current_block(Terminator::Goto { target: cont_bb });
                    self.start_block(cont_bb);
                }
            } else if active_scope_exit_owners != 0 {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "enum overwrite with a live non-string payload alias"
                            .to_string(),
                        site: value.site,
                    },
                    note: "the old enum generation can be delayed through direct string \
                           binders only when those binders cover every heap-owning field of \
                           the selected variant; move the payload into a sole owner before \
                           overwriting the parent"
                        .to_string(),
                });
            }
            return;
        }
        self.push_instr(Instr::Drop {
            place: dest,
            ty,
            drop_fn: Some(crate::model::DropFnSpec::InPlace(
                crate::ownership::InPlaceReleaseKind::Enum,
            )),
        });
        self.publish_overwrite_owner_release(binding, dest, guard);
    }

    pub(crate) fn project_tuple_owned_field_list(&self, ty: &ResolvedTy) -> Vec<(u32, ResolvedTy)> {
        let subst = self.subst_ty(ty);
        let ResolvedTy::Tuple(items) = subst else {
            return Vec::new();
        };
        items
            .iter()
            .enumerate()
            .filter_map(|(idx, item)| {
                let substituted = self.subst_ty(item);
                if ValueClass::of_ty(&substituted, &self.type_classes) == ValueClass::BitCopy {
                    None
                } else {
                    u32::try_from(idx).ok().map(|i| (i, substituted))
                }
            })
            .collect()
    }

    /// Classify a non-BitCopy match-project scrutinee shape. A
    /// non-BitCopy record/tuple destructure touches the scrutinee storage in
    /// one of two regimes:
    /// - with bindings — per-field loads hand ownership of the bound owned
    ///   fields to the bindings, and the partial-extraction emitter drops
    ///   wildcarded owned fields IN PLACE on the scrutinee storage; or
    /// - all-wildcard — nothing is moved out, and the whole aggregate is
    ///   discarded after the (always-matching) arm runs.
    ///
    /// The ONLY scrutinee shape that lowers leak-free AND use-after-free-free
    /// for both regimes is a non-captured `BindingRef`:
    /// - it carries a composite drop, so an all-wildcard discard frees every
    ///   owned field (a temporary has no composite drop — the discard would
    ///   leak the whole aggregate); and
    /// - the dataflow checker can mark it `Consumed` at the destructure site,
    ///   so a post-match read of a binding whose fields were moved out fires
    ///   `UseAfterConsume` (a projection or capture re-exposes the consumed
    ///   storage with no place to anchor the consume).
    ///
    /// Returns `None` when the scrutinee is a non-captured `BindingRef` (safe
    /// to lower). Returns `Some((construct, note))` for every other shape so
    /// the caller can emit a fail-closed `NotYetImplemented` diagnostic:
    /// projections (`FieldAccess` / `TupleIndex` / `Index` / `Slice`), captured
    /// `BindingRef`s, and temporaries (`Call`, `StructInit`, `TupleLiteral`,
    /// blocks, …). The user binds the scrutinee to a local first.
    fn match_project_scrutinee_reject(
        &self,
        scrutinee: &HirExpr,
    ) -> Option<(&'static str, String)> {
        let ty = scrutinee.ty.user_facing();
        match &scrutinee.kind {
            HirExprKind::FieldAccess { .. }
            | HirExprKind::TupleIndex { .. }
            | HirExprKind::Index { .. }
            | HirExprKind::Slice { .. } => Some((
                "non-BitCopy match destructure on projection scrutinee",
                format!(
                    "scrutinee of `{ty}` is a projection; non-BitCopy match destructure \
                     drops owned fields IN PLACE on the scrutinee storage, and the \
                     projection lets later code re-read that freed storage. Bind the \
                     scrutinee to a local first: `let scrutinee = <expr>; match scrutinee \
                     {{ … }}` — the binding carries the consume mark that prevents \
                     post-match use",
                ),
            )),
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(id),
                ..
            } if self.capture_env_sources.contains_key(id) => Some((
                "non-BitCopy match destructure on projection scrutinee",
                format!(
                    "scrutinee of `{ty}` is a closure-env captured binding; the capture \
                     load bypasses the standard consume path, so a follow-up consume mark \
                     cannot anchor against the env source and a post-match read would \
                     re-use freed storage. Bind the scrutinee to a local inside the \
                     closure first: `let scrutinee = <expr>; match scrutinee {{ … }}`",
                ),
            )),
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(_),
                ..
            } => None,
            _ => Some((
                "non-BitCopy match destructure on temporary scrutinee",
                format!(
                    "scrutinee of `{ty}` is a temporary (fresh value); it has no composite \
                     drop, so destructuring it would leak every owned field the match does \
                     not move out — an all-wildcard arm leaks the whole aggregate, and a \
                     binding arm leaks the bound field that does not escape. Bind the \
                     scrutinee to a local first: `let scrutinee = <expr>; match scrutinee \
                     {{ … }}` — the local's composite drop frees the unmoved fields and \
                     carries the consume mark for the moved ones",
                ),
            )),
        }
    }

    /// Lower a match whose arms are all `Wildcard` or `Binding`, each with
    /// an optional guard. Arms are tried in source order; a guard failure
    /// falls through to the next arm. The last arm (which must succeed if
    /// the checker accepted the match as exhaustive) emits an
    /// `ExhaustivenessFallthrough` trap if its guard also fails.
    ///
    /// Block topology:
    /// ```text
    /// arm_0_bb:
    ///   bind x = scrutinee (Binding only)
    ///   guard_0 = lower(guard)  (if present)
    ///   Branch { guard_0, then: body_0_bb, else: arm_1_bb }
    /// body_0_bb: result = body; Goto join
    ///
    /// arm_1_bb: ... (same pattern)
    /// ...
    /// join_bb: (result)
    /// ```
    fn lower_match_binding_chain(
        &mut self,
        scrutinee: &HirExpr,
        arms: &[hew_hir::HirMatchArm],
        result_ty: &ResolvedTy,
    ) -> Option<Place> {
        let result_place = self.alloc_local(result_ty.clone());
        let join_bb = self.alloc_block();
        // Track whether any arm falls through to the join with a value; when
        // every arm diverges the join is unreachable (see `lower_match_enum_tag`
        // for the full rationale — #1907).
        let mut join_reachable = false;

        let scrutinee_place = self.lower_value(scrutinee)?;
        let scrutinee_local = match scrutinee_place {
            Place::Local(n) => n,
            other => {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "binding chain match scrutinee place shape".to_string(),
                        site: scrutinee.site,
                    },
                    note: format!(
                        "binding chain match scrutinee must lower to Place::Local; got {other:?}"
                    ),
                });
                return None;
            }
        };

        // Allocate arm blocks up front and link them together.
        let arm_bbs: Vec<u32> = (0..arms.len()).map(|_| self.alloc_block()).collect();
        // Jump from the entry block to the first arm.
        self.finish_current_block(Terminator::Goto { target: arm_bbs[0] });

        for (i, arm) in arms.iter().enumerate() {
            self.start_block(arm_bbs[i]);

            // The fallthrough target when this arm's guard fails: the next
            // arm's block, or the exhaustiveness trap.
            let fallthrough_bb = if i + 1 < arms.len() {
                arm_bbs[i + 1]
            } else {
                let trap_bb = self.alloc_block();
                // We'll emit this trap block after the loop.
                // Use a dedicated block to keep the CFG well-formed.
                trap_bb
            };

            // For Binding arms, bind the scrutinee to the pattern name.
            self.emit_match_arm_binding(arm, Place::Local(scrutinee_local), None);

            // Guard check: failure branches to the next arm.
            if let Some(guard) = &arm.guard {
                let guard_place = self.lower_match_arm_guard(guard);
                if let Some(guard_local) = guard_place {
                    let body_entry_bb = self.alloc_block();
                    self.finish_current_block(Terminator::Branch {
                        cond: guard_local,
                        then_target: body_entry_bb,
                        else_target: fallthrough_bb,
                    });
                    self.start_block(body_entry_bb);
                }
            }

            // Arm body.
            let value = self.lower_composite_result_value(&arm.body);
            if let Some(src) = value {
                self.push_composite_result_move(result_place, src, result_ty);
            }
            if !self.cursor_unreachable {
                join_reachable = true;
            }
            self.finish_current_block(Terminator::Goto { target: join_bb });

            // Emit the fallthrough trap for the last arm (if no next arm).
            if i + 1 == arms.len() {
                self.start_block(fallthrough_bb);
                self.finish_current_block(Terminator::Trap {
                    kind: crate::model::TrapKind::ExhaustivenessFallthrough,
                });
            }
        }

        self.start_block(join_bb);
        if !join_reachable {
            self.cursor_unreachable = true;
        }
        Some(result_place)
    }

    #[allow(
        clippy::too_many_lines,
        reason = "complete project-arm preflight keeps all fail-closed checks ahead of CFG emission"
    )]
    fn preflight_selected_project_arm(
        &mut self,
        scrutinee: &HirExpr,
        arm: &hew_hir::HirMatchArm,
        consume_owned: bool,
    ) -> Option<Vec<ProjectFieldDischarge>> {
        let project_kind = match &arm.predicate {
            hew_hir::HirMatchArmPredicate::RecordProject { .. } => {
                Some(ProjectAggregateKind::Record)
            }
            hew_hir::HirMatchArmPredicate::TupleProject { arity } => {
                for field_idx in arm.bindings.iter().map(|binding| binding.field_idx).chain(
                    arm.payload_predicates
                        .iter()
                        .map(|predicate| predicate.field_idx),
                ) {
                    if field_idx >= *arity {
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct: "tuple project field index out of range".to_string(),
                                site: arm.body.site,
                            },
                            note: format!(
                                "project field {field_idx} is outside arity {arity}; \
                                 the checker/HIR verifier should have rejected this"
                            ),
                        });
                        return None;
                    }
                }
                Some(ProjectAggregateKind::Tuple)
            }
            hew_hir::HirMatchArmPredicate::Wildcard
            | hew_hir::HirMatchArmPredicate::Binding { .. } => {
                if !arm.bindings.is_empty() {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::NotYetImplemented {
                            construct: "wildcard/binding match arm with project bindings"
                                .to_string(),
                            site: arm.body.site,
                        },
                        note: "wildcard/binding match arms must not carry project bindings; \
                               this is a checker/HIR bug"
                            .to_string(),
                    });
                    return None;
                }
                None
            }
            hew_hir::HirMatchArmPredicate::EnumVariant { .. }
            | hew_hir::HirMatchArmPredicate::Literal { .. }
            | hew_hir::HirMatchArmPredicate::Regex { .. } => {
                panic!("checker invariant violated: refutable arm in project match lowering");
            }
        };

        if !consume_owned || matches!(arm.predicate, hew_hir::HirMatchArmPredicate::Binding { .. })
        {
            return Some(Vec::new());
        }

        let aggregate_kind = project_kind.unwrap_or_else(|| {
            if matches!(self.subst_ty(&scrutinee.ty), ResolvedTy::Tuple(_)) {
                ProjectAggregateKind::Tuple
            } else {
                ProjectAggregateKind::Record
            }
        });
        let extracted: HashSet<u32> = arm
            .bindings
            .iter()
            .map(|binding| binding.field_idx)
            .collect();
        let owned_fields = match aggregate_kind {
            ProjectAggregateKind::Record => self.project_record_owned_field_list(&scrutinee.ty),
            ProjectAggregateKind::Tuple => self.project_tuple_owned_field_list(&scrutinee.ty),
        };
        let mut discharges = Vec::new();
        for (field_idx, field_ty) in owned_fields {
            if extracted.contains(&field_idx) {
                continue;
            }
            if matches!(field_ty, ResolvedTy::String)
                || self.field_drop_in_place_admissible(&field_ty)
            {
                discharges.push(ProjectFieldDischarge::InPlace {
                    kind: aggregate_kind,
                    field_idx,
                    field_ty,
                });
                continue;
            }
            if let ReleaseSymbolVerdict::Wired(symbol) =
                self.project_field_inline_drop_symbol(&field_ty)
            {
                discharges.push(ProjectFieldDischarge::Leaf {
                    kind: aggregate_kind,
                    field_idx,
                    field_ty,
                    symbol,
                });
                continue;
            }
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "match-destructure wildcard on owned aggregate field".to_string(),
                    site: scrutinee.site,
                },
                note: format!(
                    "field {field_idx} of `{}` has type `{}`, which neither the inline leaf \
                     release nor the field-addressed in-place drop can discharge \
                     (slices, `dyn Trait` fields, closures, affine handles, and \
                     unregistered layouts are refused). Bind the field explicitly so \
                     its drop is elaborated through `owned_locals`, or extract every \
                     sibling instead of using `_` on this field — refusing to lower \
                     fail-closed rather than emit a leak / wrong-ABI drop",
                    scrutinee.ty.user_facing(),
                    field_ty.user_facing(),
                ),
            });
            return None;
        }
        Some(discharges)
    }

    #[allow(
        clippy::too_many_arguments,
        clippy::too_many_lines,
        reason = "the ownership-complete selected-arm emitter keeps transfer and discharge ordering explicit"
    )]
    fn emit_selected_project_arm(
        &mut self,
        scrutinee: &HirExpr,
        scrutinee_local: u32,
        arm: &hew_hir::HirMatchArm,
        discharges: &[ProjectFieldDischarge],
        consume_owned: bool,
        result_place: Place,
    ) -> bool {
        let consume_scrutinee = if consume_owned {
            match &scrutinee.kind {
                HirExprKind::BindingRef {
                    resolved: ResolvedRef::Binding(id),
                    name,
                } if !self.capture_env_sources.contains_key(id) => Some((*id, name.clone())),
                _ => None,
            }
        } else {
            None
        };
        let scrutinee_is_interior_alias =
            consume_owned && self.local_storage_is_interior_alias(scrutinee_local);
        let mut overwritten_bindings = Vec::with_capacity(arm.bindings.len() + 1);
        let mut whole_binding_owner_handoff = false;

        for binding in &arm.bindings {
            let binding_ty = self.subst_ty(&binding.ty);
            self.push_bind_statement(
                binding.binding,
                binding.name.clone(),
                arm.body.site,
                binding_ty.clone(),
            );
            self.record_match_arm_binding_scope(binding.binding, arm);
            // U1 — the payload binder's owner is minted over a field of the
            // scrutinee, so the provenance question is the scrutinee's. The
            // warrant is what `register_owned_local` requires; the seed gate
            // alone can no longer reach it.
            let warrant =
                self.owner_warrant_for_scrutinee_payload(binding.binding, scrutinee, &binding_ty);
            let keep_for_drop_elab =
                self.binding_seeds_drop_elaboration(&binding_ty) && !warrant.withholds_mint();
            let binding_is_string = matches!(binding_ty, ResolvedTy::String);
            let dest = self.alloc_local(binding_ty.clone());
            let previous = self.binding_locals.insert(binding.binding, dest);
            if consume_scrutinee.is_some() && keep_for_drop_elab {
                if let Some(local_idx) = base_local(dest) {
                    self.match_project_consumed_binder_locals.insert(local_idx);
                }
            }
            match &arm.predicate {
                hew_hir::HirMatchArmPredicate::RecordProject { .. } => {
                    self.push_instr(Instr::RecordFieldLoad {
                        record: Place::Local(scrutinee_local),
                        field_offset: FieldOffset(binding.field_idx),
                        dest,
                    });
                }
                hew_hir::HirMatchArmPredicate::TupleProject { .. } => {
                    self.push_instr(Instr::TupleFieldLoad {
                        tuple: Place::Local(scrutinee_local),
                        field_index: binding.field_idx,
                        dest,
                    });
                }
                _ => unreachable!("project bindings passed selected-arm preflight"),
            }
            if keep_for_drop_elab {
                self.register_owned_local(
                    binding.binding,
                    binding.name.clone(),
                    binding_ty.clone(),
                    warrant,
                );
                if ty_is_generator_handle(&binding_ty) {
                    if let Some(scope) = self.active_scopes.last().copied() {
                        self.scope_generator_bindings.push((
                            scope,
                            binding.binding,
                            binding_ty.clone(),
                        ));
                    }
                }
            }
            if consume_scrutinee.is_some() && !scrutinee_is_interior_alias && binding_is_string {
                let field = match &arm.predicate {
                    hew_hir::HirMatchArmPredicate::RecordProject { .. } => {
                        crate::model::FieldAddr::Record(FieldOffset(binding.field_idx))
                    }
                    hew_hir::HirMatchArmPredicate::TupleProject { .. } => {
                        crate::model::FieldAddr::Tuple(binding.field_idx)
                    }
                    _ => unreachable!("string project binding passed selected-arm preflight"),
                };
                self.push_instr(Instr::FieldDropInPlace {
                    base: Place::Local(scrutinee_local),
                    field,
                    ty: ResolvedTy::String,
                });
            }
            overwritten_bindings.push((binding.binding, previous, keep_for_drop_elab));
        }

        if let hew_hir::HirMatchArmPredicate::Binding {
            binding_id,
            name,
            ty,
        } = &arm.predicate
        {
            let binding_ty = self.subst_ty(ty);
            self.push_bind_statement(*binding_id, name.clone(), arm.body.site, binding_ty.clone());
            self.record_match_arm_binding_scope(*binding_id, arm);
            let warrant =
                self.owner_warrant_for_scrutinee_payload(*binding_id, scrutinee, &binding_ty);
            let keep_for_drop_elab = consume_owned
                && self.binding_seeds_drop_elaboration(&binding_ty)
                && !warrant.withholds_mint();
            let source_owner = consume_scrutinee.as_ref().and_then(|(source, _)| {
                self.current_owner_id_at_place(Place::Local(scrutinee_local))
                    .filter(|owner| owner.binding == *source)
            });
            let dest = self.alloc_local(binding_ty.clone());
            let previous = self.binding_locals.insert(*binding_id, dest);
            self.push_instr(Instr::Move {
                dest,
                src: Place::Local(scrutinee_local),
            });
            if keep_for_drop_elab {
                self.register_owned_local(*binding_id, name.clone(), binding_ty.clone(), warrant);
                if let Some((source_binding, _)) = consume_scrutinee.as_ref() {
                    whole_binding_owner_handoff = exact_whole_binding_owner_handoff(
                        source_owner,
                        *source_binding,
                        self.current_owner_id_at_place(dest),
                        *binding_id,
                    );
                }
            }
            overwritten_bindings.push((*binding_id, previous, keep_for_drop_elab));
        }

        if consume_owned && !scrutinee_is_interior_alias {
            for discharge in discharges {
                match discharge {
                    ProjectFieldDischarge::Leaf {
                        kind,
                        field_idx,
                        field_ty,
                        symbol,
                    } => {
                        let temp = self.alloc_local(field_ty.clone());
                        match kind {
                            ProjectAggregateKind::Record => {
                                self.push_instr(Instr::RecordFieldLoad {
                                    record: Place::Local(scrutinee_local),
                                    field_offset: FieldOffset(*field_idx),
                                    dest: temp,
                                });
                            }
                            ProjectAggregateKind::Tuple => {
                                self.push_instr(Instr::TupleFieldLoad {
                                    tuple: Place::Local(scrutinee_local),
                                    field_index: *field_idx,
                                    dest: temp,
                                });
                            }
                        }
                        self.push_instr(Instr::Drop {
                            place: temp,
                            ty: field_ty.clone(),
                            drop_fn: Some(crate::model::DropFnSpec::Release(symbol)),
                        });
                    }
                    ProjectFieldDischarge::InPlace {
                        kind,
                        field_idx,
                        field_ty,
                    } => {
                        let field = match kind {
                            ProjectAggregateKind::Record => {
                                crate::model::FieldAddr::Record(FieldOffset(*field_idx))
                            }
                            ProjectAggregateKind::Tuple => {
                                crate::model::FieldAddr::Tuple(*field_idx)
                            }
                        };
                        self.push_instr(Instr::FieldDropInPlace {
                            base: Place::Local(scrutinee_local),
                            field,
                            ty: field_ty.clone(),
                        });
                    }
                }
            }
        }

        if let Some((scrutinee_id, scrutinee_name)) = consume_scrutinee {
            self.statements.push(MirStatement::Use {
                binding: scrutinee_id,
                name: scrutinee_name,
                site: scrutinee.site,
                ty: self.subst_ty(&scrutinee.ty),
                intent: IntentKind::Consume,
            });
            if !whole_binding_owner_handoff {
                self.mark_binding_moved(scrutinee_id);
            }
        }

        let value = self.lower_composite_result_value(&arm.body);
        for (binding, previous, keep_for_drop_elab) in overwritten_bindings.into_iter().rev() {
            if keep_for_drop_elab {
                continue;
            }
            if let Some(previous) = previous {
                self.binding_locals.insert(binding, previous);
            } else {
                self.binding_locals.remove(&binding);
            }
        }
        if let Some(src) = value {
            self.push_composite_result_move(result_place, src, &arm.body.ty);
        }
        !self.cursor_unreachable
    }

    /// Lower a project-match scrutinee to a local place. A CONSUMING project
    /// match is an ownership boundary: the selected arm's binder discharge and
    /// in-place field drops release every owned field of the scrutinee exactly
    /// once, so an owned call-carrier scrutinee must not ALSO receive the
    /// callee's terminal carrier snapshot drop (a resource double-close /
    /// string double-free).
    ///
    /// * A WHOLE carrier (the parameter slot itself) hands its release
    ///   authority to the match IN PLACE, and only on the paths that flow
    ///   through the match: the consumption block is recorded and
    ///   `append_owned_carrier_param_drops` skips the terminal snapshot drop
    ///   on exits the consumption dominates while keeping it on exits that
    ///   branch around the match (a guard / early `return` before the
    ///   destructure still releases the untouched carrier). The slot is not
    ///   neutralized, because a neutralized (zeroed) slot would still run a
    ///   `#[resource]` record's `close` over zeroed storage — an inline
    ///   resource has no null sentinel to skip on. An exit reachable both
    ///   through and around the consumption has no single release authority
    ///   and fails closed with a diagnostic.
    /// * A PROJECTION carrier transfers eagerly through the funnel
    ///   (`AggregateProjectionNeutralize`), so the terminal drop still
    ///   releases the untouched sibling fields exactly once.
    /// * A non-carrier scrutinee passes through untouched; a borrow-mode
    ///   match skips the boundary entirely and the carrier keeps its
    ///   terminal release.
    fn lower_project_match_scrutinee_local(
        &mut self,
        scrutinee: &HirExpr,
        consume_owned: bool,
        construct: &str,
        note: &str,
    ) -> Option<u32> {
        let raw_place = self.lower_value(scrutinee)?;
        let place = if consume_owned {
            match self.owned_carrier_authority(raw_place) {
                Some(super::OwnedCarrierNeutralizeTarget::Whole(root)) if root == raw_place => {
                    self.record_owned_carrier_transfer(raw_place);
                    self.owned_carrier_consumed
                        .entry(raw_place)
                        .or_default()
                        .push((self.current_block_id, scrutinee.site));
                    raw_place
                }
                Some(_) => {
                    let scrutinee_ty = self.subst_ty(&scrutinee.ty);
                    self.transfer_owned_carrier_place(raw_place, &scrutinee_ty)
                }
                None => {
                    // Either this is not a carrier or an earlier consume can
                    // reach the current block. A mutually-exclusive sibling
                    // remains available through the CFG reachability query
                    // above and records its own consume site.
                    raw_place
                }
            }
        } else {
            raw_place
        };
        let Place::Local(local) = place else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: construct.to_string(),
                    site: scrutinee.site,
                },
                note: note.to_string(),
            });
            return None;
        };
        Some(local)
    }

    fn lower_match_project(
        &mut self,
        scrutinee: &HirExpr,
        arms: &[hew_hir::HirMatchArm],
        result_ty: &ResolvedTy,
    ) -> Option<Place> {
        if arms.iter().any(|arm| !arm.payload_predicates.is_empty()) {
            return self.lower_match_project_predicate_chain(scrutinee, arms, result_ty);
        }
        let selected = arms.iter().find(|arm| {
            matches!(
                arm.predicate,
                hew_hir::HirMatchArmPredicate::RecordProject { .. }
                    | hew_hir::HirMatchArmPredicate::TupleProject { .. }
                    | hew_hir::HirMatchArmPredicate::Wildcard
                    | hew_hir::HirMatchArmPredicate::Binding { .. }
            )
        })?;

        if let Some(guard) = arms.iter().find_map(|arm| arm.guard.as_ref()) {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "guarded record/tuple match destructure".to_string(),
                    site: guard.site,
                },
                note: "a `match` on a record or tuple value lowers as a single irrefutable \
                       destructure, so an arm guard (`Pattern if <cond>`) cannot fall through to \
                       a later arm — the guard would be ignored and the first arm taken \
                       unconditionally. Move the condition into the arm body \
                       (`=> if <cond> { … } else { … }`), or match on an enum, whose guarded \
                       arms lower as an ordered fallthrough chain"
                    .to_string(),
            });
            return None;
        }

        let scrutinee_is_non_bitcopy = !self.project_match_scrutinee_is_bitcopy(&scrutinee.ty);
        if scrutinee_is_non_bitcopy {
            if let Some((construct, note)) = self.match_project_scrutinee_reject(scrutinee) {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: construct.to_string(),
                        site: scrutinee.site,
                    },
                    note,
                });
                return None;
            }
        }
        let consume_owned = scrutinee_is_non_bitcopy && !selected.bindings.is_empty();
        let discharges = self.preflight_selected_project_arm(scrutinee, selected, consume_owned)?;

        let result_place = self.alloc_local(result_ty.clone());
        let scrutinee_local = self.lower_project_match_scrutinee_local(
            scrutinee,
            consume_owned,
            "project match scrutinee place shape",
            "record/tuple match destructure requires a local scrutinee",
        )?;
        let join_bb = self.alloc_block();
        let body_bb = self.alloc_block();
        self.finish_current_block(Terminator::Goto { target: body_bb });
        self.start_block(body_bb);
        if let Some(scope) = selected.scope {
            self.active_scopes.push(scope);
        }
        let join_reachable = self.emit_selected_project_arm(
            scrutinee,
            scrutinee_local,
            selected,
            &discharges,
            consume_owned,
            result_place,
        );
        if let Some(scope) = selected.scope {
            self.emit_scope_exit_marker_with_carries([scope], [result_place]);
            self.active_scopes.pop();
        }
        self.finish_current_block(Terminator::Goto { target: join_bb });
        self.start_block(join_bb);
        if !join_reachable {
            self.cursor_unreachable = true;
        }
        Some(result_place)
    }

    /// Lower record/tuple match arms carrying literal element predicates as an
    /// ordered chain. Predicate checks happen before any field binding, so a
    /// mismatch can safely fall through without partially moving the scrutinee.
    #[allow(
        clippy::too_many_lines,
        reason = "ordered project-predicate lowering keeps comparison and fallthrough topology together"
    )]
    fn lower_match_project_predicate_chain(
        &mut self,
        scrutinee: &HirExpr,
        arms: &[hew_hir::HirMatchArm],
        result_ty: &ResolvedTy,
    ) -> Option<Place> {
        if let Some(guard) = arms.iter().find_map(|arm| arm.guard.as_ref()) {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "guarded record/tuple match destructure".to_string(),
                    site: guard.site,
                },
                note: "record/tuple project guards remain fail-closed; use literal \
                       element predicates for dispatch and move additional conditions \
                       into the selected arm body"
                    .to_string(),
            });
            return None;
        }

        let scrutinee_is_non_bitcopy = !self.project_match_scrutinee_is_bitcopy(&scrutinee.ty);
        let ownership_mode = project_match_ownership_mode(arms);
        let consume_owned = scrutinee_is_non_bitcopy
            && matches!(ownership_mode, ProjectMatchOwnershipMode::Consume);
        if scrutinee_is_non_bitcopy {
            if let Some((construct, note)) = self.match_project_scrutinee_reject(scrutinee) {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: construct.to_string(),
                        site: scrutinee.site,
                    },
                    note,
                });
                return None;
            }
            if matches!(ownership_mode, ProjectMatchOwnershipMode::NotApplicable) {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "non-project arm in owned literal-predicate match".to_string(),
                        site: scrutinee.site,
                    },
                    note: "owned literal-predicate lowering requires one uniform record/tuple \
                           project chain"
                        .to_string(),
                });
                return None;
            }
            if consume_owned {
                if let HirExprKind::BindingRef {
                    resolved: ResolvedRef::Binding(id),
                    ..
                } = &scrutinee.kind
                {
                    if self
                        .binding_locals
                        .get(id)
                        .and_then(|place| base_local(*place))
                        .is_some_and(|local| self.local_storage_is_interior_alias(local))
                    {
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct:
                                    "owned literal-predicate match on interior alias binding"
                                        .to_string(),
                                site: scrutinee.site,
                            },
                            note: "the direct binding aliases another aggregate's interior \
                                   storage, so selected-arm field discharge cannot transfer \
                                   ownership safely; bind or clone a fresh complete value first"
                                .to_string(),
                        });
                        return None;
                    }
                }
            }
        }

        let arm_discharges = arms
            .iter()
            .map(|arm| self.preflight_selected_project_arm(scrutinee, arm, consume_owned))
            .collect::<Option<Vec<_>>>()?;
        let result_place = self.alloc_local(result_ty.clone());
        let scrutinee_local = self.lower_project_match_scrutinee_local(
            scrutinee,
            consume_owned,
            "project predicate match scrutinee place shape",
            "record/tuple literal-predicate match requires a local scrutinee",
        )?;

        let join_bb = self.alloc_block();
        let arm_bbs: Vec<u32> = (0..arms.len()).map(|_| self.alloc_block()).collect();
        let trap_bb = self.alloc_block();
        let first_bb = arm_bbs.first().copied().unwrap_or(trap_bb);
        self.finish_current_block(Terminator::Goto { target: first_bb });
        let mut join_reachable = false;

        for (arm_idx, (arm, discharges)) in arms.iter().zip(arm_discharges.iter()).enumerate() {
            self.start_block(arm_bbs[arm_idx]);
            let fallthrough_bb = arm_bbs.get(arm_idx + 1).copied().unwrap_or(trap_bb);
            match &arm.predicate {
                hew_hir::HirMatchArmPredicate::RecordProject { .. }
                | hew_hir::HirMatchArmPredicate::TupleProject { .. } => {
                    for predicate in &arm.payload_predicates {
                        let field = self.alloc_local(predicate.ty.clone());
                        match &arm.predicate {
                            hew_hir::HirMatchArmPredicate::RecordProject { .. } => {
                                self.push_instr(Instr::RecordFieldLoad {
                                    record: Place::Local(scrutinee_local),
                                    field_offset: FieldOffset(predicate.field_idx),
                                    dest: field,
                                });
                            }
                            hew_hir::HirMatchArmPredicate::TupleProject { .. } => {
                                self.push_instr(Instr::TupleFieldLoad {
                                    tuple: Place::Local(scrutinee_local),
                                    field_index: predicate.field_idx,
                                    dest: field,
                                });
                            }
                            _ => unreachable!("project predicate passed arm preflight"),
                        }
                        let expected = self.lower_match_literal_constant(
                            &predicate.literal,
                            &predicate.ty,
                            arm.body.site,
                        )?;
                        let cond = self.alloc_local(ResolvedTy::Bool);
                        if let Some(width) = float_width(&predicate.ty) {
                            self.push_instr(Instr::FloatCmp {
                                pred: CmpPred::Eq,
                                lhs: field,
                                rhs: expected,
                                dest: cond,
                                width,
                            });
                        } else {
                            self.push_instr(Instr::IntCmp {
                                pred: CmpPred::Eq,
                                lhs: field,
                                rhs: expected,
                                dest: cond,
                            });
                        }
                        if matches!(predicate.ty, ResolvedTy::String) {
                            self.push_instr(Instr::Drop {
                                place: field,
                                ty: ResolvedTy::String,
                                drop_fn: Some(crate::model::DropFnSpec::Release("hew_string_drop")),
                            });
                        }
                        let pass_bb = self.alloc_block();
                        self.finish_current_block(Terminator::Branch {
                            cond,
                            then_target: pass_bb,
                            else_target: fallthrough_bb,
                        });
                        self.start_block(pass_bb);
                    }
                }
                hew_hir::HirMatchArmPredicate::Wildcard
                | hew_hir::HirMatchArmPredicate::Binding { .. } => {}
                hew_hir::HirMatchArmPredicate::EnumVariant { .. }
                | hew_hir::HirMatchArmPredicate::Literal { .. }
                | hew_hir::HirMatchArmPredicate::Regex { .. } => {
                    panic!("checker invariant violated: refutable non-project arm in project match")
                }
            }

            if let Some(scope) = arm.scope {
                self.active_scopes.push(scope);
            }
            join_reachable |= self.emit_selected_project_arm(
                scrutinee,
                scrutinee_local,
                arm,
                discharges,
                consume_owned,
                result_place,
            );
            if let Some(scope) = arm.scope {
                self.emit_scope_exit_marker_with_carries([scope], [result_place]);
                self.active_scopes.pop();
            }
            self.finish_current_block(Terminator::Goto { target: join_bb });
        }

        self.start_block(trap_bb);
        self.finish_current_block(Terminator::Trap {
            kind: TrapKind::ExhaustivenessFallthrough,
        });
        self.start_block(join_bb);
        if !join_reachable {
            self.cursor_unreachable = true;
        }
        Some(result_place)
    }

    /// Lower a match whose arms compare a scalar/string scrutinee against
    /// literals, interleaved with guarded `Binding`/`Wildcard` (catch-all)
    /// arms. Arms are tried **in source order** as one ordered chain — a
    /// literal mismatch *or* a guard failure on any arm falls through to the
    /// next arm, and the final fall-through emits an `ExhaustivenessFallthrough`
    /// trap.
    ///
    /// Treating every arm uniformly is load-bearing: a `Binding`/`Wildcard`
    /// arm is a catch-all only when it has no guard. A guarded binding arm
    /// (`x if x > 100 => ...`) must fall through on guard failure so a later
    /// arm — including a final `_` wildcard — still runs. The previous
    /// implementation collapsed all binding/wildcard arms into a single
    /// catch-all, silently dropping every arm after the first guarded binding
    /// arm (the real `_` default among them); on the fall-through path codegen
    /// then trapped, producing `check`-green / `run`-exit-1 with no output.
    /// See LESSONS `match-fail-closed`.
    ///
    /// Block topology (per arm `i`):
    /// ```text
    /// arm_i_bb:
    ///   (Literal)  cond = IntCmp(Eq, scrutinee, const_i)
    ///              Branch { cond, then: matched_i_bb, else: arm_{i+1}_bb }
    ///   matched_i_bb:
    ///              guard_i = lower(guard)             (if present)
    ///              Branch { guard_i, then: body_i_bb, else: arm_{i+1}_bb }
    ///   (Binding/Wildcard)
    ///              bind x = scrutinee                 (Binding only)
    ///              guard_i = lower(guard)             (if present)
    ///              Branch { guard_i, then: body_i_bb, else: arm_{i+1}_bb }
    /// body_i_bb:   result = lower(arm_i.body); Goto join_bb
    ///
    /// trap_bb:     Trap { ExhaustivenessFallthrough } (final fall-through)
    /// join_bb:     (result)
    /// ```
    #[allow(
        clippy::too_many_lines,
        reason = "literal match lowering is one ordered-chain CFG builder; \
                  splitting it would obscure block allocation and branch topology"
    )]
    fn lower_match_literal(
        &mut self,
        scrutinee: &HirExpr,
        arms: &[hew_hir::HirMatchArm],
        result_ty: &ResolvedTy,
    ) -> Option<Place> {
        let result_place = self.alloc_local(result_ty.clone());
        // Track whether any arm falls through to the join with a value; when
        // every arm diverges the join is unreachable (see `lower_match_enum_tag`
        // for the full rationale — #1907).
        let mut join_reachable = false;

        // Validate arm predicates up front; the checker guarantees a
        // homogeneous literal scrutinee, so the only legal arms are Literal
        // (refutable) and Binding/Wildcard (catch-all, optionally guarded).
        for arm in arms {
            match &arm.predicate {
                hew_hir::HirMatchArmPredicate::Literal { .. }
                | hew_hir::HirMatchArmPredicate::Wildcard
                | hew_hir::HirMatchArmPredicate::Binding { .. } => {}
                hew_hir::HirMatchArmPredicate::EnumVariant { .. } => {
                    panic!("checker invariant violated: mixed Literal/Variant arms");
                }
                hew_hir::HirMatchArmPredicate::Regex { .. } => {
                    panic!("checker invariant violated: mixed Literal/Regex arms");
                }
                hew_hir::HirMatchArmPredicate::RecordProject { .. }
                | hew_hir::HirMatchArmPredicate::TupleProject { .. } => {
                    panic!("checker invariant violated: mixed Literal/Project arms");
                }
            }
        }

        let scrutinee_place = self.lower_value(scrutinee)?;
        let scrutinee_local = match scrutinee_place {
            Place::Local(n) => n,
            other => {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "literal match scrutinee place shape".to_string(),
                        site: scrutinee.site,
                    },
                    note: format!(
                        "literal match scrutinee must lower to Place::Local; got {other:?}"
                    ),
                });
                return None;
            }
        };

        let join_bb = self.alloc_block();
        // One entry block per arm, linked in source order, plus a terminal
        // trap block reached only when no arm matches.
        let arm_bbs: Vec<u32> = (0..arms.len()).map(|_| self.alloc_block()).collect();
        let trap_bb = self.alloc_block();
        self.finish_current_block(Terminator::Goto { target: arm_bbs[0] });

        for (i, arm) in arms.iter().enumerate() {
            self.start_block(arm_bbs[i]);
            // The fall-through target when this arm does not match (literal
            // mismatch) or its guard fails: the next arm, or the trap.
            let fallthrough_bb = arm_bbs.get(i + 1).copied().unwrap_or(trap_bb);

            // Literal arms first test the scrutinee against the constant; a
            // mismatch falls through immediately to the next arm.
            if let hew_hir::HirMatchArmPredicate::Literal { lit, ty } = &arm.predicate {
                let expected = self.lower_match_literal_constant(lit, ty, arm.body.site)?;
                let cond_local = self.alloc_local(ResolvedTy::Bool);
                if let Some(width) = float_width(ty) {
                    self.push_instr(Instr::FloatCmp {
                        pred: CmpPred::Eq,
                        lhs: Place::Local(scrutinee_local),
                        rhs: expected,
                        dest: cond_local,
                        width,
                    });
                } else {
                    self.push_instr(Instr::IntCmp {
                        pred: CmpPred::Eq,
                        lhs: Place::Local(scrutinee_local),
                        rhs: expected,
                        dest: cond_local,
                    });
                }
                let matched_bb = self.alloc_block();
                self.finish_current_block(Terminator::Branch {
                    cond: cond_local,
                    then_target: matched_bb,
                    else_target: fallthrough_bb,
                });
                self.start_block(matched_bb);
                if !arm.bindings.is_empty() {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::NotYetImplemented {
                            construct: "bindings in literal match arm".to_string(),
                            site: arm.body.site,
                        },
                        note: "top-level literal match arms do not introduce payload bindings"
                            .to_string(),
                    });
                    return None;
                }
            } else {
                // Binding/Wildcard catch-all: bind the scrutinee name (if any).
                self.emit_match_arm_binding(arm, Place::Local(scrutinee_local), None);
            }

            // Guard check (applies to literal and catch-all arms alike):
            // failure falls through to the next arm.
            if let Some(guard) = &arm.guard {
                if let Some(guard_local) = self.lower_match_arm_guard(guard) {
                    let body_entry_bb = self.alloc_block();
                    self.finish_current_block(Terminator::Branch {
                        cond: guard_local,
                        then_target: body_entry_bb,
                        else_target: fallthrough_bb,
                    });
                    self.start_block(body_entry_bb);
                }
            }

            // Arm body: produce the result and jump to the join.
            if let Some(src) = self.lower_composite_result_value(&arm.body) {
                self.push_composite_result_move(result_place, src, result_ty);
            }
            if !self.cursor_unreachable {
                join_reachable = true;
            }
            self.finish_current_block(Terminator::Goto { target: join_bb });
        }

        // Terminal fall-through: no arm matched. The checker pre-gates
        // non-exhaustive matches, so this is a fail-closed backstop.
        self.start_block(trap_bb);
        self.finish_current_block(Terminator::Trap {
            kind: TrapKind::ExhaustivenessFallthrough,
        });

        self.start_block(join_bb);
        if !join_reachable {
            self.cursor_unreachable = true;
        }
        Some(result_place)
    }

    fn lower_match_literal_constant(
        &mut self,
        lit: &HirLiteral,
        ty: &ResolvedTy,
        site: SiteId,
    ) -> Option<Place> {
        match (lit, ty) {
            (HirLiteral::Integer(value), ty) if ty.is_integer_literal_match_scrutinee() => {
                let dest = self.alloc_local(ty.clone());
                self.push_instr(Instr::ConstI64 {
                    dest,
                    value: *value,
                });
                Some(dest)
            }
            (HirLiteral::Bool(value), ResolvedTy::Bool) => {
                let dest = self.alloc_local(ty.clone());
                self.push_instr(Instr::ConstI64 {
                    dest,
                    value: i64::from(*value),
                });
                Some(dest)
            }
            (HirLiteral::Char(value), ResolvedTy::Char) => {
                let dest = self.alloc_local(ty.clone());
                self.push_instr(Instr::CharLit {
                    value: *value as u32,
                    dest,
                });
                Some(dest)
            }
            (HirLiteral::Float(value), ResolvedTy::F32 | ResolvedTy::F64) => {
                let (value_bits, width) = match ty {
                    ResolvedTy::F32 => {
                        #[allow(
                            clippy::cast_possible_truncation,
                            reason = "checker admitted the f32 literal pattern"
                        )]
                        let narrowed = *value as f32;
                        (u64::from(narrowed.to_bits()), FloatWidth::F32)
                    }
                    ResolvedTy::F64 => (value.to_bits(), FloatWidth::F64),
                    _ => unreachable!("match arm pattern guards the float type"),
                };
                let dest = self.alloc_local(ty.clone());
                self.push_instr(Instr::FloatLit {
                    dest,
                    value_bits,
                    width,
                });
                Some(dest)
            }
            (HirLiteral::String(value), ResolvedTy::String) => {
                let bytes = value.as_bytes();
                if bytes.contains(&0) {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::NotYetImplemented {
                            construct: "string literal match pattern with embedded NUL".to_string(),
                            site,
                        },
                        note: "string match literals use the C-string runtime equality ABI; \
                               embedded NUL would truncate comparison, so it is rejected"
                            .to_string(),
                    });
                    return None;
                }
                let dest = self.alloc_local(ty.clone());
                self.push_instr(Instr::StringLit {
                    bytes: bytes.to_vec(),
                    dest,
                });
                Some(dest)
            }
            _ => {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!("unsupported literal match predicate {lit:?}: {ty:?}"),
                        site,
                    },
                    note:
                        "literal match lowering is wired only for integers, bool, char, and string"
                            .to_string(),
                });
                None
            }
        }
    }

    /// Lower a match expression whose non-wildcard arms are all
    /// `HirMatchArmPredicate::Regex` — an ordered predicate-dispatch CFG driven
    /// by `hew_regex_match` + `hew_regex_capture`.
    ///
    /// Block topology (per regex arm `i`, with `k` named captures):
    ///
    /// ```text
    /// entry_bb (current):
    ///   scrutinee_place = lower(scrutinee)
    ///   Goto check_bb_0
    ///
    /// check_bb_i:
    ///   lit_id_i = ConstI64(literal_id_i)
    ///   match_result_i = CallRuntimeAbi(hew_regex_match, [scrutinee, lit_id_i])
    ///   match_bool_i = IntCmp(NotEq, match_result_i, 0i64)
    ///   Branch { cond: match_bool_i, then: cap_bb_i_0 (or body_bb_i), else: check_bb_{i+1} }
    ///
    /// cap_bb_i_j  (one per named capture group j):
    ///   cap_idx_j = ConstI64(j)
    ///   cap_ptr_j = CallRuntimeAbi(hew_regex_capture, [scrutinee, lit_id_i, cap_idx_j])
    ///   null_k_j  = ConstI64(0)
    ///   null_cond_j = IntCmp(Eq, cap_ptr_j, null_k_j)
    ///   Branch { cond: null_cond_j, then: check_bb_{i+1}, else: cap_bb_i_{j+1} (or body_bb_i) }
    ///
    /// body_bb_i:
    ///   result = lower(arm_i.body)
    ///   Move { dest: result_place, src: result }
    ///   Goto join_bb
    ///
    /// (last check falls through to wildcard_bb or fail-closed trap)
    ///
    /// join_bb:
    ///   (subsequent lowering continues here)
    /// ```
    ///
    /// Capture bindings: each `hew_regex_capture` return value is placed into a
    /// fresh `Place::Local` typed as `ResolvedTy::I64` (opaque pointer — the
    /// runtime returns a NUL-terminated C string as `*mut u8`, which has no
    /// first-class MIR type today). The HIR arm body's capture references will
    /// resolve to these locals in the binding scope once the HIR producer threads
    /// `BindingIds` for captures (slice 5+ follow-on). For slice 4 the places are
    /// allocated and the null-check CFG is emitted correctly; the arm body simply
    /// does not yet reference them via `BindingRef`.
    ///
    /// WHY `hew_regex_match` returns i32 not bool: C ABI convention used by all
    /// predicate-returning runtime entries. The MIR branch uses an IntCmp(NotEq,
    /// _, 0) to produce the Bool cond local.
    ///
    /// WHY `literal_id` not handle: MIR has no `Place::RegexHandle` primitive yet.
    /// The runtime resolves `literal_id → compiled handle` via the module-init
    /// global array (wired in slice 5). WHEN-OBSOLETE: if a `Place::RegexGlobal`
    /// primitive lands, the id-to-handle indirection moves inside MIR.
    #[allow(
        clippy::too_many_lines,
        reason = "single coherent CFG builder for the regex predicate dispatch chain; splitting would hide block-allocation ordering"
    )]
    fn lower_match_regex(
        &mut self,
        scrutinee: &HirExpr,
        arms: &[hew_hir::HirMatchArm],
        result_ty: &ResolvedTy,
    ) -> Option<Place> {
        // Result local first so every arm's Move dominates it.
        let result_place = self.alloc_local(result_ty.clone());
        // Track whether any arm falls through to the join with a value; when
        // every arm diverges the join is unreachable (see `lower_match_enum_tag`
        // for the full rationale — #1907).
        let mut join_reachable = false;

        // Partition into ordered non-wildcard arms and the optional wildcard.
        // All non-wildcard arms must be Regex here; EnumVariant in a regex match
        // is a checker contract violation (heterogeneous scrutinee types).
        let mut regex_arms: Vec<&hew_hir::HirMatchArm> = Vec::new();
        let mut wildcard_arm: Option<&hew_hir::HirMatchArm> = None;
        for arm in arms {
            match &arm.predicate {
                hew_hir::HirMatchArmPredicate::Regex { .. } => {
                    regex_arms.push(arm);
                }
                hew_hir::HirMatchArmPredicate::Wildcard
                | hew_hir::HirMatchArmPredicate::Binding { .. } => {
                    if wildcard_arm.is_none() {
                        wildcard_arm = Some(arm);
                    }
                }
                hew_hir::HirMatchArmPredicate::EnumVariant { .. } => {
                    // EnumVariant arms cannot co-exist with Regex arms in a
                    // well-typed match (the checker rejects heterogeneous
                    // scrutinee types). Fail closed: emit a diagnostic and
                    // return without emitting a half-built CFG.
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::NotYetImplemented {
                            construct: "EnumVariant arm in regex match expression".to_string(),
                            site: scrutinee.site,
                        },
                        note: "a match expression cannot mix EnumVariant and Regex arms; \
                               this shape should have been rejected by the checker"
                            .to_string(),
                    });
                    return None;
                }
                hew_hir::HirMatchArmPredicate::Literal { .. } => {
                    panic!("checker invariant violated: mixed Literal/Regex arms");
                }
                hew_hir::HirMatchArmPredicate::RecordProject { .. }
                | hew_hir::HirMatchArmPredicate::TupleProject { .. } => {
                    panic!("checker invariant violated: mixed Regex/Project arms");
                }
            }
        }

        // Lower the scrutinee in the entry block.
        let scrutinee_place = self.lower_value(scrutinee)?;
        let scrutinee_local = match scrutinee_place {
            Place::Local(n) => n,
            other => {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "regex match scrutinee place shape".to_string(),
                        site: scrutinee.site,
                    },
                    note: format!(
                        "regex match scrutinee must lower to Place::Local; got {other:?}"
                    ),
                });
                return None;
            }
        };

        // Allocate the join block up front.
        let join_bb = self.alloc_block();

        // Allocate one body block per regex arm.
        let body_bbs: Vec<u32> = (0..regex_arms.len()).map(|_| self.alloc_block()).collect();

        // Tail block: wildcard or fail-closed trap.
        let tail_bb = self.alloc_block();

        // Build the check chain. For each regex arm:
        //   1. ConstI64(literal_id) → lit_local
        //   2. CallRuntimeAbi(hew_regex_match, [scrutinee, lit_local]) → i32 result
        //   3. IntCmp(NotEq, result, 0) → bool cond
        //   4. Branch(cond, then: cap_check_or_body, else: next_check)
        //
        // If the arm has named captures, insert a null-check chain between the
        // match call and the body block (one check per capture; any null → next arm).
        //
        // `arm_capture_places[i]` accumulates the capture pointer places for arm i so
        // the body block loop below can emit `hew_regex_free_capture` for each before
        // running the arm body (success-path ownership release) and the partial-failure
        // cleanup blocks can free the captures that were already allocated before the
        // null was discovered.
        let mut arm_capture_places: Vec<Vec<Place>> = Vec::with_capacity(regex_arms.len());

        for (i, arm) in regex_arms.iter().enumerate() {
            let (literal_id, captures) = match &arm.predicate {
                hew_hir::HirMatchArmPredicate::Regex {
                    literal_id,
                    captures,
                    ..
                } => (*literal_id, captures.as_slice()),
                // regex_arms only contains Regex arms (enforced above).
                other => {
                    unreachable!("regex_arms must only contain Regex predicates; got {other:?}")
                }
            };

            // ConstI64 for the literal id.
            let lit_local = self.alloc_local(ResolvedTy::I64);
            self.push_instr(Instr::ConstI64 {
                dest: lit_local,
                value: i64::from(literal_id),
            });

            // Call hew_regex_match — returns i32 (1 = match, 0 = no match).
            let match_result_local = self.alloc_local(ResolvedTy::I32);
            match crate::model::RuntimeCall::new(
                "hew_regex_match",
                vec![Place::Local(scrutinee_local), lit_local],
                Some(match_result_local),
            ) {
                Ok(call) => self.push_instr(Instr::CallRuntimeAbi(call)),
                Err(e) => {
                    // The symbol must be in the allowlist (we added it in slice 4);
                    // if we reach here it is a code invariant violation.
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::NotYetImplemented {
                            construct: format!("hew_regex_match runtime call: {e}"),
                            site: scrutinee.site,
                        },
                        note:
                            "hew_regex_match must be in the runtime allowlist (slice 4 invariant)"
                                .to_string(),
                    });
                    return None;
                }
            }

            // Widen the i32 match result to Bool for the branch condition.
            let zero_local = self.alloc_local(ResolvedTy::I32);
            self.push_instr(Instr::ConstI64 {
                dest: zero_local,
                value: 0,
            });
            let match_cond_local = self.alloc_local(ResolvedTy::Bool);
            self.push_instr(Instr::IntCmp {
                pred: crate::model::CmpPred::NotEq,
                lhs: match_result_local,
                rhs: zero_local,
                dest: match_cond_local,
            });

            // The else target for the match-check branch is the next arm's check
            // block (or the tail on the last arm). We emit the next check block
            // only if there is a next arm.
            let next_check_bb = if i + 1 < regex_arms.len() {
                self.alloc_block()
            } else {
                tail_bb
            };

            // If there are named captures, the "then" target is the first capture
            // null-check block; otherwise it is the body block directly.
            // We build the capture-null-check chain first (pre-allocating blocks),
            // then emit the branch that enters it.
            //
            // Capture places: typed as I64 (opaque pointer). The runtime returns
            // a NUL-terminated C string as *mut u8 cast to i64 (zero = null).
            // The i64 opaque representation is substrate-correct for null-check
            // dispatch until nullable pointer types are available.
            //
            // WHY I64 for a pointer: MIR has no nullable-pointer type today. I64
            // is the convention used by other handle places (lambda-actor handles
            // are also stored as i64-sized locals). WHEN-OBSOLETE: when a
            // `ResolvedTy::Pointer { nullable: true }` variant lands, capture
            // places should switch to it so codegen emits a real pointer icmp.
            let capture_places: Vec<Place> = captures
                .iter()
                .map(|_| self.alloc_local(ResolvedTy::I64))
                .collect();

            // Build the capture null-check chain from the end backward so we
            // have the "entry" target for the first check available when we emit
            // the match-branch.
            //
            // For each capture j (in order):
            //   cap_bb_j (allocated below for j >= 1; for j == 0 it is the first
            //   block after the match call):
            //     cap_idx_j = ConstI64(real_group_idx)
            //     cap_ptr_j = CallRuntimeAbi(hew_regex_capture, [scrutinee, lit_id, cap_idx_j])
            //     Move { dest: capture_places[j], src: cap_ptr_j }
            //     null_k = ConstI64(0)
            //     null_cond_j = IntCmp(Eq, capture_places[j], 0)
            //     Branch { then: cleanup_bb_j_or_next_check, else: cap_bb_{j+1} (or body_bb_i) }
            //
            //   cleanup_bb_j (only when j > 0; for j==0 null means no allocation yet):
            //     hew_regex_free_capture(capture_places[0]) ... hew_regex_free_capture(capture_places[j-1])
            //     Goto next_check_bb
            //
            // If captures is empty the first_cap_bb == body_bb_i (no intervening blocks).
            let cap_entry_bb = if captures.is_empty() {
                body_bbs[i]
            } else {
                // Pre-allocate interior capture check blocks (one per capture).
                let cap_check_bbs: Vec<u32> = std::iter::once(self.alloc_block())
                    .chain((1..captures.len()).map(|_| self.alloc_block()))
                    .collect();

                let first_cap_bb = cap_check_bbs[0];

                // Emit the match-check branch that enters the capture chain.
                self.finish_current_block(Terminator::Branch {
                    cond: match_cond_local,
                    then_target: first_cap_bb,
                    else_target: next_check_bb,
                });

                // Emit each capture null-check block.
                for (j, (_cap_name, group_idx)) in captures.iter().enumerate() {
                    self.start_block(cap_check_bbs[j]);

                    // ConstI64 for the real regex group index (1-based; group 0 is the
                    // whole match). Using the real group position rather than the
                    // named-capture-only ordinal `j` ensures correct lookup when unnamed
                    // positional groups precede named ones — e.g. `(foo)(?P<bar>bar)` has
                    // group 1=(foo) and group 2=bar; passing `j+1` would return group 1
                    // ("foo") instead of group 2 ("bar").
                    let cap_idx_local = self.alloc_local(ResolvedTy::I64);
                    self.push_instr(Instr::ConstI64 {
                        dest: cap_idx_local,
                        value: i64::from(*group_idx),
                    });

                    // hew_regex_capture returns the capture value (or null).
                    let cap_raw_local = self.alloc_local(ResolvedTy::I64);
                    match crate::model::RuntimeCall::new(
                        "hew_regex_capture",
                        vec![Place::Local(scrutinee_local), lit_local, cap_idx_local],
                        Some(cap_raw_local),
                    ) {
                        Ok(call) => self.push_instr(Instr::CallRuntimeAbi(call)),
                        Err(e) => {
                            self.diagnostics.push(MirDiagnostic {
                                kind: MirDiagnosticKind::NotYetImplemented {
                                    construct: format!("hew_regex_capture runtime call: {e}"),
                                    site: scrutinee.site,
                                },
                                note:
                                    "hew_regex_capture must be in the runtime allowlist (slice 4)"
                                        .to_string(),
                            });
                            return None;
                        }
                    }

                    // Store the raw capture pointer into the capture place for
                    // this named capture. The arm body will read from this local
                    // once the HIR producer threads BindingIds for captures.
                    self.push_instr(Instr::Move {
                        dest: capture_places[j],
                        src: cap_raw_local,
                    });

                    // Null check: if the capture pointer is zero the pattern did
                    // not capture this group → branch to cleanup/next arm (fail closed:
                    // missing capture ≠ empty string, LESSONS `match-fail-closed`).
                    let null_k_local = self.alloc_local(ResolvedTy::I64);
                    self.push_instr(Instr::ConstI64 {
                        dest: null_k_local,
                        value: 0,
                    });
                    let null_cond_local = self.alloc_local(ResolvedTy::Bool);
                    self.push_instr(Instr::IntCmp {
                        pred: crate::model::CmpPred::Eq,
                        lhs: capture_places[j],
                        rhs: null_k_local,
                        dest: null_cond_local,
                    });

                    // Then: null → go to cleanup (if prior captures allocated) or next arm.
                    // Else: non-null → next capture or body.
                    let else_target = if j + 1 < captures.len() {
                        cap_check_bbs[j + 1]
                    } else {
                        body_bbs[i]
                    };
                    // When j > 0, captures[0..j] were malloc'd and must be freed before
                    // we abandon this arm. Emit a cleanup block that calls
                    // hew_regex_free_capture for each allocated capture then Goto
                    // next_check_bb.
                    //
                    // When j == 0 the current capture is null so nothing was allocated
                    // yet — go directly to next_check_bb.
                    //
                    // SHIM: this free sequence only covers the straight-line null-fail
                    // path. If arm bodies contain early returns or trap paths the
                    // already-extracted captures would leak on those paths. A real fix
                    // requires scope-exit cleanup primitives in MIR (v0.6 substrate lane).
                    // WHY acceptable for v0.5: arm bodies in the current regex feature are
                    // value expressions (literals, arithmetic) with no early-return paths.
                    // WHEN-OBSOLETE: when MIR gains Instr::ScopeExit or Instr::CStringDrop.
                    let null_then_target = if j == 0 {
                        // No prior allocations; go directly to next arm check.
                        next_check_bb
                    } else {
                        // Prior captures[0..j] are malloc'd; emit a cleanup block.
                        let cleanup_bb = self.alloc_block();
                        self.finish_current_block(Terminator::Branch {
                            cond: null_cond_local,
                            then_target: cleanup_bb,
                            else_target,
                        });
                        self.start_block(cleanup_bb);
                        for &prior_place in capture_places.iter().take(j) {
                            match crate::model::RuntimeCall::new(
                                "hew_regex_free_capture",
                                vec![prior_place],
                                None,
                            ) {
                                Ok(call) => self.push_instr(Instr::CallRuntimeAbi(call)),
                                Err(e) => {
                                    self.diagnostics.push(MirDiagnostic {
                                        kind: MirDiagnosticKind::NotYetImplemented {
                                            construct: format!(
                                                "hew_regex_free_capture runtime call: {e}"
                                            ),
                                            site: scrutinee.site,
                                        },
                                        note: "hew_regex_free_capture must be in the allowlist"
                                            .to_string(),
                                    });
                                    return None;
                                }
                            }
                        }
                        self.finish_current_block(Terminator::Goto {
                            target: next_check_bb,
                        });
                        // Skip the branch below (already emitted with cleanup_bb as then).
                        continue;
                    };
                    self.finish_current_block(Terminator::Branch {
                        cond: null_cond_local,
                        then_target: null_then_target,
                        else_target,
                    });
                }

                // Return the entry block for the capture chain.
                // The match-check branch was already emitted above.
                first_cap_bb
            };

            // When there are no captures, emit the match-check branch here.
            // When there are captures, the branch was already emitted inside the
            // `else` block above; this branch would be a double-close. Guard with
            // the captures-empty check.
            if captures.is_empty() {
                self.finish_current_block(Terminator::Branch {
                    cond: match_cond_local,
                    then_target: cap_entry_bb,
                    else_target: next_check_bb,
                });
            }

            arm_capture_places.push(capture_places);

            // Open the next check block (or the tail, on the last arm) so the
            // next iteration has a current block to append to.
            self.start_block(next_check_bb);
        }

        // Tail block: wildcard body or fail-closed trap.
        // (We are already in next_check_bb / tail_bb at this point.)
        if let Some(wildcard) = wildcard_arm {
            let value = self.lower_value(&wildcard.body);
            if let Some(src) = value {
                self.push_instr(Instr::Move {
                    dest: result_place,
                    src,
                });
            }
            if !self.cursor_unreachable {
                join_reachable = true;
            }
            self.finish_current_block(Terminator::Goto { target: join_bb });
        } else {
            // Belt-and-braces runtime guard (LESSONS `match-fail-closed` P0).
            // The checker rejects non-exhaustive regex matches at compile time.
            self.finish_current_block(Terminator::Trap {
                kind: crate::model::TrapKind::ExhaustivenessFallthrough,
            });
        }

        // Arm body blocks.
        for (i, arm) in regex_arms.iter().enumerate() {
            self.start_block(body_bbs[i]);

            // Success path: all captures for this arm are non-null (malloc'd by
            // hew_regex_capture). Emit hew_regex_free_capture for each capture
            // AFTER the arm body runs (the body currently has no bindings to the
            // capture places, but ownership must still be released). When the HIR
            // producer wires capture BindingIds to these places (a follow-on lane),
            // the free must move to after the last use, not before — update at that time.
            //
            // SHIM: free is emitted before lower_value so the capture places are
            // released regardless of the body's type. This is correct for straight-line
            // bodies but would double-free if the body itself read the capture place and
            // then freed it. Since capture bindings are not yet threaded into the body
            // (no HIR BindingId → place mapping), this is safe for v0.5.
            // WHEN-OBSOLETE: when capture bindings are wired, move each free to after
            // the last use of that binding in the body.
            let cap_places = &arm_capture_places[i];
            for &cap_place in cap_places {
                match crate::model::RuntimeCall::new(
                    "hew_regex_free_capture",
                    vec![cap_place],
                    None,
                ) {
                    Ok(call) => self.push_instr(Instr::CallRuntimeAbi(call)),
                    Err(e) => {
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct: format!("hew_regex_free_capture body exit: {e}"),
                                site: scrutinee.site,
                            },
                            note: "hew_regex_free_capture must be in the allowlist".to_string(),
                        });
                        return None;
                    }
                }
            }

            let value = self.lower_composite_result_value(&arm.body);
            if let Some(src) = value {
                self.push_composite_result_move(result_place, src, result_ty);
            }
            if !self.cursor_unreachable {
                join_reachable = true;
            }
            self.finish_current_block(Terminator::Goto { target: join_bb });
        }

        // Join. Subsequent lowering continues here. When every arm diverged
        // (no arm fell through with a value) the join has no live predecessor;
        // flag the cursor unreachable so the caller skips the Move/Return that
        // would read the never-written `result_place` (#1907).
        self.start_block(join_bb);
        if !join_reachable {
            self.cursor_unreachable = true;
        }
        Some(result_place)
    }

    /// #2648 (S3, the #2523 twin gate) — the **Group C2** unconditional
    /// ephemeral-producer set: shapes whose produced value provably owns no
    /// re-readable heap alias, so the match temp is always a fresh sole owner
    /// regardless of any operand.
    ///
    /// This is the residue of the old blanket allowlist after the arms that CAN
    /// alias caller storage were split out into precise gates
    /// (`classify_producer_scrutinee_origin`): `Binary` (Group C1, string-concat
    /// is fresh but a heap non-string result is not), the aggregate constructors
    /// (Group B, fresh iff every owned operand is fresh), and the call/method
    /// arms (Group A, gated on the return-provenance authority). Everything left
    /// here is either scalar/bool-valued (the type short-circuit proves `∅`) or a
    /// MOVE out of a channel/mailbox/generator/machine (ownership transfers to
    /// the receiver), so it stays an unconditional `EphemeralTemp`.
    fn hir_scrutinee_is_unconditional_ephemeral_producer(kind: &HirExprKind) -> bool {
        matches!(
            kind,
            // Fresh scalar literals and pure-value operators (scalar/bool result).
            HirExprKind::Literal { .. }
                | HirExprKind::Unary { .. }
                | HirExprKind::NumericCast { .. }
                | HirExprKind::SaturatingWidthCast { .. }
                | HirExprKind::TryWidthCast { .. }
                | HirExprKind::IdentityCompare { .. }
                | HirExprKind::CancellationTokenIsCancelled { .. }
                | HirExprKind::RcIntrinsic { .. }
                // Spawns/closures/generators produce fresh handles or objects. A
                // `Closure` capturing a heap place is not destructured into
                // payload binders on this path, so it never reaches the
                // meaningful gate (fail-closed tightening is a future lane).
                | HirExprKind::Spawn { .. }
                | HirExprKind::SpawnedCall { .. }
                | HirExprKind::SpawnLambdaActor { .. }
                | HirExprKind::Closure { .. }
                | HirExprKind::GenBlock { .. }
                | HirExprKind::ActorGenStream { .. }
                // ask/recv/await and machine effects MOVE a value out of the
                // mailbox/channel/generator/machine — ownership transfers to the
                // receiver, so the result is a fresh sole owner.
                | HirExprKind::ActorAsk { .. }
                | HirExprKind::RemoteActorAsk { .. }
                | HirExprKind::AwaitTask { .. }
                | HirExprKind::AwaitRestart { .. }
                | HirExprKind::ConnAwaitRead { .. }
                | HirExprKind::ListenerAwaitAccept { .. }
                | HirExprKind::ChannelRecvAwait { .. }
                | HirExprKind::StreamRecvAwait { .. }
                | HirExprKind::GeneratorNext { .. }
                | HirExprKind::Select { .. }
                | HirExprKind::Join { .. }
                | HirExprKind::WireCodec { .. }
                | HirExprKind::MachineEmit { .. }
                | HirExprKind::MachineStep { .. }
                | HirExprKind::MachineTakeEmits { .. }
        )
    }

    /// #2648 (S3) — classify a non-`BindingRef` producer scrutinee for the #2523
    /// projected-payload move-out policy, routing every arm that CAN forward a
    /// caller-visible alias through the precise return-provenance authority
    /// (Fix-design-3, Groups A/B/C). No producer arm is an unconditional admit;
    /// a value that may alias caller storage is `Reject(AliasesCallerStorage)`.
    ///
    /// Interim [Rev-8]: the module-fn / builtin-getter tightenings that are
    /// precursor-INDEPENDENT are LIVE (the PARAM-forwarder reject and the F1
    /// borrowed-getter reject); the FULL precise `OPAQUE`-only module-fn rejects
    /// land at S4b, so a `∅`/`OPAQUE`-only module-fn or a user method call keeps
    /// today's `EphemeralTemp` (the legacy fail-open window, stated explicitly).
    fn classify_producer_scrutinee_origin(&self, scrutinee: &HirExpr) -> ProjectedPayloadOrigin {
        match &scrutinee.kind {
            // Group A — plain call: interim module-fn rule (mirrors the preflight
            // admission classifier). A resolved module-fn callee whose precise
            // summary carries PARAM forwards a by-value heap parameter → Reject;
            // `∅`/`OPAQUE`-only/unknown/extern/indirect → legacy `EphemeralTemp`.
            HirExprKind::Call { .. } => self.classify_call_arm_scrutinee_origin(scrutinee),
            // Group A — builtin-collection getter (`Vec`/`HashMap`/`HashSet`
            // dispatch; `ResolvedImplCall` is builtin-collection-only). The F1
            // emitted-symbol contract is precursor-INDEPENDENT and LIVE: Fresh
            // (`hew_vec_get_clone` / `hew_hashmap_get_clone_layout` / owned-return
            // string/bytes) → `EphemeralTemp`; a borrowed getter
            // (`hew_vec_get_owned`/`_ptr`, a `Vec<Vec<T>>` `.get`) → Reject.
            HirExprKind::ResolvedImplCall { .. } => {
                self.classify_builtin_getter_scrutinee_origin(scrutinee)
            }
            // Group A — a `.clone()` returns a fresh independent owner; and the
            // user method / trait / numeric method calls. Interim [Rev-8]: no
            // resolvable module-fn PARAM summary is reachable through the
            // receiver-keyed HIR variants, so they keep today's admission
            // (`EphemeralTemp`) — the legacy fail-open window for opaque-hidden
            // method forwarding, closed by the FULL precise method verdicts at
            // S4b. `NumericMethod` returns a scalar (the move-out never records a
            // heap provenance), so its classification is inert either way.
            HirExprKind::RecordCloneCall { .. }
            | HirExprKind::VarSelfMethodCall { .. }
            | HirExprKind::CallDynMethod { .. }
            | HirExprKind::CallTraitMethodStatic { .. }
            | HirExprKind::NumericMethod { .. } => ProjectedPayloadOrigin::EphemeralTemp,
            // Group B (aggregate constructors) + Group C1 (`Binary`) share ONE
            // precise-freshness gate (R6 — the SAME `return_alias_bits` operand
            // recursion the callee summary and the caller arg-scan use):
            // `EphemeralTemp` iff the scrutinee's precise bits are `∅`, else
            // Reject. An `Outer { inner: h.b }` / `(h.b, …)` over a live place is
            // rejected UNCONDITIONALLY; a string concat (`hew_string_concat`,
            // fresh-allocating) is `∅`, while a non-string heap `Binary` is not.
            HirExprKind::StructInit { .. }
            | HirExprKind::TupleLiteral { .. }
            | HirExprKind::MachineVariantCtor { .. } => {
                if self.scrutinee_precise_bits(scrutinee).is_fresh() {
                    ProjectedPayloadOrigin::EphemeralTemp
                } else {
                    ProjectedPayloadOrigin::Reject(
                        ProjectedPayloadRejectReason::AliasesCallerStorage,
                    )
                }
            }
            // U10 — Group C1 `Binary`, split out of the shared gate above and
            // given an explicit TYPE-AND-OPERATOR exclusion rather than relying
            // on `is_fresh()` to happen to answer `false` for everything else.
            //
            // Ephemeral admission here says "the payload move-out needs no
            // neutralize because the scrutinee's storage is not re-readable".
            // At heap-owning type that claim is only true of the fresh
            // `hew_string_concat` buffer: it is `malloc`ed at the site and its
            // bytes are copied OUT OF the borrowed operands, so no operand's
            // allocation — foreign or domestic — is the classified value. Any
            // other heap-owning `Binary` fails closed to `Reject`.
            //
            // Non-heap `Binary` (integer/float arithmetic, comparisons) keeps
            // the previous gate exactly: it records no heap provenance, so the
            // classification is inert.
            //
            // The `debug_assert` is the fire-if-one-ever-arrives pin: it
            // compares the explicit exclusion against the freshness gate that
            // used to carry this arm alone, and trips if the two ever disagree.
            HirExprKind::Binary { op, .. } => {
                let ty = self.subst_ty(&scrutinee.ty);
                let owns_heap = crate::model::ty_owns_heap_mir(
                    &ty,
                    &self.record_field_orders,
                    &self.enum_layouts,
                );
                let admissible = !owns_heap
                    || (matches!(ty, ResolvedTy::String) && matches!(op, super::BinaryOp::Add));
                let precise_fresh = self.scrutinee_precise_bits(scrutinee).is_fresh();
                debug_assert!(
                    admissible || !precise_fresh,
                    "a heap-owning `Binary` scrutinee outside the string-concat \
                     exclusion was classified fresh by the precise bits: the \
                     U10 exclusion no longer holds and a non-fresh allocation \
                     could be admitted as an ephemeral temp"
                );
                if admissible && precise_fresh {
                    ProjectedPayloadOrigin::EphemeralTemp
                } else {
                    ProjectedPayloadOrigin::Reject(
                        ProjectedPayloadRejectReason::AliasesCallerStorage,
                    )
                }
            }
            // Group C2 — provably no re-readable heap operand → unconditional
            // `EphemeralTemp`.
            other if Self::hir_scrutinee_is_unconditional_ephemeral_producer(other) => {
                ProjectedPayloadOrigin::EphemeralTemp
            }
            // Default-deny: a place projection / wrapper / any un-enumerated shape.
            _ => ProjectedPayloadOrigin::Reject(ProjectedPayloadRejectReason::ReadablePlace),
        }
    }

    /// Group A plain-`Call` arm: the completed HIR produced-value fact is the
    /// ownership authority. Only an owned call result has a fresh sole-owner
    /// temp that can be neutralized after moving out a payload. A borrowed result
    /// may forward caller-visible storage, so its payload move stays rejected.
    ///
    /// `NoOwner` and `Unknown` carry no proof that a caller-owned payload may be
    /// moved out, so both stay outside the payload-transfer rule and fail closed.
    fn classify_call_arm_scrutinee_origin(&self, scrutinee: &HirExpr) -> ProjectedPayloadOrigin {
        let ownership = self
            .param_ownership
            .produced_value_facts
            .get(&scrutinee.site)
            .map_or(ProducedValueOwnership::Unknown, |fact| fact.ownership);
        match ownership {
            ProducedValueOwnership::Owned { .. } => ProjectedPayloadOrigin::EphemeralTemp,
            ProducedValueOwnership::Borrowed
            | ProducedValueOwnership::ReceiverIdentity
            | ProducedValueOwnership::NoOwner
            | ProducedValueOwnership::Unknown => {
                ProjectedPayloadOrigin::Reject(ProjectedPayloadRejectReason::AliasesCallerStorage)
            }
        }
    }

    /// Group A builtin-collection-getter arm (F1, precursor-independent): resolve
    /// the EMITTED runtime symbol the site will lower to and consult the
    /// emitted-symbol return contract. Fresh (a proved-owner clone/retain/take
    /// getter or an owned-return string/bytes producer) → `EphemeralTemp`; a
    /// borrowed getter (`hew_vec_get_owned`/`_ptr`, `hew_vec_get_layout`), an
    /// interior getter, or an unresolvable element ABI → Reject (fail-closed).
    fn classify_builtin_getter_scrutinee_origin(
        &self,
        scrutinee: &HirExpr,
    ) -> ProjectedPayloadOrigin {
        match self.method_scrutinee_emitted_symbol(scrutinee) {
            Some(sym) if crate::return_provenance::method_return_provenance(&sym).is_fresh() => {
                ProjectedPayloadOrigin::EphemeralTemp
            }
            _ => ProjectedPayloadOrigin::Reject(ProjectedPayloadRejectReason::AliasesCallerStorage),
        }
    }

    /// Resolve the EMITTED runtime symbol a builtin-collection `ResolvedImplCall`
    /// scrutinee will lower to, reproducing lowering's placeholder decisions [F1]:
    /// a `HashMap` `get` always lowers to the fresh-owner clone choke regardless
    /// of the checker's `hew_hashmap_get_layout` placeholder; a generic
    /// `Vec<T>`-element method left a `_FAMILY` placeholder is re-resolved from the
    /// substituted element exactly as the call lowering does; a concrete call
    /// already carries its resolved linker-edge symbol. Returns `None` (→
    /// fail-closed Reject) for an unresolvable element ABI (closure/function
    /// elements the owned authority excludes) or a non-`ResolvedImplCall`.
    pub(crate) fn method_scrutinee_emitted_symbol(&self, scrutinee: &HirExpr) -> Option<String> {
        let HirExprKind::ResolvedImplCall {
            receiver,
            target_family,
            target_symbol,
            ..
        } = &scrutinee.kind
        else {
            return None;
        };
        // `HashMap::get -> Option<V>` always routes to the fresh-owner clone
        // choke (`hew_hashmap_get_clone_layout`), never the `hew_hashmap_get_layout`
        // placeholder the checker recorded (see `lower_hashmap_index_trap`).
        if matches!(
            target_family,
            hew_types::MethodTargetFamily::HashMap(hew_types::HashMapMethod::Get)
        ) {
            return Some("hew_hashmap_get_clone_layout".to_string());
        }
        // A generic-element `Vec<T>` method kept the `hew_vec_*_FAMILY`
        // placeholder — re-resolve it from the substituted element, the same
        // authority the call lowering consults.
        if target_symbol.ends_with("_FAMILY") {
            return self.resolve_polymorphic_vec_element_symbol(*target_family, &receiver.ty);
        }
        // Concrete dispatch: the checker already resolved the emitted symbol
        // (e.g. an owned-value `Vec::get` carries `hew_vec_get_clone` directly).
        Some(target_symbol.clone())
    }

    /// The precise three-state return-provenance bits of a scrutinee expression,
    /// evaluated through the shared `return_alias_bits` walk under the module's
    /// `PrecisePolicy`. Used by the Group B aggregate gate and the Group C1
    /// `Binary` gate so all four #2648 consumers (callee summary, caller
    /// arg-scan, aggregate scrutinee, `Binary` scrutinee) agree on operand
    /// freshness (R6). The current function's local binding-provenance is not
    /// threaded here — an aggregate scrutinee over a bare heap LOCAL operand
    /// therefore reads as `{PARAM}` (non-fresh) and over-rejects fail-closed; no
    /// reported case or fixture uses an aggregate/`Binary` scrutinee with a
    /// fresh-local operand, and the precise local threading is a future
    /// refinement.
    fn scrutinee_precise_bits(&self, scrutinee: &HirExpr) -> crate::return_provenance::AliasBits {
        use crate::return_provenance::{return_alias_bits, PrecisePolicy};
        let local_bits: HashMap<BindingId, crate::return_provenance::AliasBits> = HashMap::new();
        let policy = PrecisePolicy {
            provenance: &self.call_scrutinee_provenance.provenance,
            extern_table: &self.call_scrutinee_provenance.extern_table,
            local_bits: &local_bits,
        };
        return_alias_bits(scrutinee, &policy)
    }

    /// #2523 — classify a match/while-let/let-else/if-let scrutinee for the
    /// projected-payload move-out policy. FAIL-CLOSED: only a scrutinee *proven*
    /// a fresh sole owner takes the temp-neutralize consume path; everything
    /// else is rejected before codegen. Shared by every top-level payload
    /// binding loop so no destructuring construct bypasses default-deny.
    ///
    ///   * `OwnedBinding` — a bare owning `BindingRef` that is NOT captured by a
    ///     closure. The match MOVES it into the temp, so nulling the temp
    ///     transfers ownership and consume-marking the binding turns a re-read
    ///     into a compile-time use-after-move.
    ///   * `EphemeralTemp` — a proven fresh value producer (call / constructor /
    ///     literal / await): the temp is a fresh sole owner, neutralize only.
    ///   * `Reject(CapturedBinding)` — a closure-captured binding (F2): it is
    ///     read from the closure environment by BYTE-COPY (`ClosureEnvFieldLoad`,
    ///     see `capture_env_sources`), NOT moved into the temp, so the captured
    ///     copy survives the move and double-frees when the env drops.
    ///   * `Reject(ReadablePlace)` — a place projection or wrapper, or any
    ///     un-enumerated shape (default-deny).
    pub(crate) fn classify_scrutinee_origin(&self, scrutinee: &HirExpr) -> ProjectedPayloadOrigin {
        match &scrutinee.kind {
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(id),
                name,
            } => {
                if self.capture_env_sources.contains_key(id) {
                    // F2 — captured binding: env-copy origin the neutralize
                    // cannot reach. Reject rather than double-free.
                    ProjectedPayloadOrigin::Reject(ProjectedPayloadRejectReason::CapturedBinding)
                } else {
                    ProjectedPayloadOrigin::OwnedBinding(ProjectedScrutinee {
                        binding: *id,
                        name: name.clone(),
                        ty: self.subst_ty(&scrutinee.ty),
                    })
                }
            }
            _ => self.classify_producer_scrutinee_origin(scrutinee),
        }
    }

    /// Lower a match-arm GUARD expression with the
    /// fallthrough-guard flag set, so any projected heap-payload consumed inside
    /// it is rejected fail-closed (`GuardedConsume`) rather than emitting a
    /// `NeutralizePayloadSlot` that would run before the guard outcome is known.
    /// The flag is saved/restored so nested guards compose and the arm BODY
    /// (where a consume IS committed once the arm is taken) is unaffected.
    /// Borrow-only guards never reach the consume hook, so they stay valid.
    fn lower_match_arm_guard(&mut self, guard: &HirExpr) -> Option<Place> {
        let prev = self.in_fallthrough_match_guard;
        self.in_fallthrough_match_guard = true;
        let result = self.lower_value(guard);
        self.in_fallthrough_match_guard = prev;
        result
    }

    /// #2523 — record the interior-alias provenance for a heap-owning projected
    /// payload binder so its `Consume`-intent move-out routes through the
    /// default-deny consume hook (`lower_value`'s `Use { Consume }` arm). Gated
    /// on `keep_for_drop_elab`: a bit-copy payload (`i64`) owns no heap, so there
    /// is nothing to neutralize, double-free, or reject. Shared by every
    /// top-level AND nested payload binding loop so no destructure path bypasses
    /// the policy.
    pub(crate) fn record_projected_payload_provenance(
        &mut self,
        binding_id: BindingId,
        binder_name: &str,
        source_place: Place,
        origin: ProjectedPayloadOrigin,
        keep_for_drop_elab: bool,
    ) {
        if keep_for_drop_elab {
            // The overwrite flag guards the BINDER's own release. A binder
            // dispositioned `AliasOf` (a carrier-parameter payload) mints no
            // owner, so there is no generation for the guard to attach to.
            let direct_owned_string_alias = self.live_owner_generation(binding_id).is_some()
                && matches!(&origin, ProjectedPayloadOrigin::OwnedBinding(_))
                && self
                    .binding_locals
                    .get(&binding_id)
                    .and_then(|place| base_local(*place))
                    .and_then(|local| self.locals.get(local as usize))
                    .is_some_and(|ty| matches!(self.subst_ty(ty), ResolvedTy::String));
            self.projected_payload_provenance.insert(
                binding_id,
                ProjectedPayloadProvenance {
                    source_place,
                    binder_name: binder_name.to_string(),
                    origin,
                },
            );
            if direct_owned_string_alias
                && !self
                    .projected_payload_overwrite_flags
                    .contains_key(&binding_id)
            {
                let flag = self.alloc_local(ResolvedTy::I64);
                self.push_instr(Instr::ConstI64 {
                    dest: flag,
                    value: 1,
                });
                self.projected_payload_overwrite_flags
                    .insert(binding_id, flag);
                if let Some(generation) = self.owner_generations.get(&binding_id).copied() {
                    self.push_instr(Instr::OwnershipEvent(crate::model::OwnershipEvent::Guard {
                        owner: crate::model::OwnerId {
                            binding: binding_id,
                            generation,
                        },
                        flag,
                        kind: crate::model::OwnershipGuardKind::ProjectedPayload,
                    }));
                }
            }
        }
    }

    /// Record a match payload binder in the authoritative synthetic arm scope
    /// created by HIR lowering. That scope encloses the pattern bindings,
    /// optional guard, and body regardless of whether the body is itself a
    /// block expression.
    fn record_match_arm_binding_scope(&mut self, binding: BindingId, arm: &hew_hir::HirMatchArm) {
        if let Some(scope) = arm.scope {
            self.record_binding_scope_in(binding, scope);
        } else {
            self.record_binding_scope(binding);
        }
    }

    /// Returns the result `Place::Local` that every arm body's value is
    /// moved into. For a Unit-valued match the result local is allocated
    /// but never read by codegen.
    #[allow(
        clippy::too_many_lines,
        reason = "single coherent CFG builder for the match dispatch chain; splitting would hide block-allocation ordering"
    )]
    fn lower_match_enum_tag(
        &mut self,
        result_site: SiteId,
        scrutinee: &HirExpr,
        arms: &[hew_hir::HirMatchArm],
        result_ty: &ResolvedTy,
    ) -> Option<Place> {
        if !self.typed_produced_value_demand_is_resolved(
            scrutinee,
            "match scrutinee has unresolved ownership",
        ) {
            return None;
        }
        // Result local first so every arm's Move dominates it.
        let result_place = self.alloc_local(result_ty.clone());

        // Track whether ANY arm falls through to the join with a value. A
        // diverging body (`return`/`panic`) leaves the cursor unreachable (the
        // `return` statement lowering flags `cursor_unreachable`); a
        // non-diverging body leaves it reachable. When every arm diverges the
        // join block has no live predecessor and `result_place` is never
        // written, so the cursor is flagged unreachable below and the caller
        // (`function_body`) skips emitting a Move/Return that would read the
        // dead i8 `Unit` stand-in into a non-scalar slot (#1907). A
        // non-diverging body's `lower_value` may itself yield `None` (an empty
        // `Unit` block), so the reachability flag — not the value `Option` — is
        // the load-bearing signal.
        let mut join_reachable = false;

        // Partition arms: ordered non-wildcard checks followed by an
        // optional wildcard. The exhaustiveness checker prevents two
        // wildcards or a wildcard followed by a variant arm reaching
        // here, but we treat the first wildcard as the catch-all and
        // ignore any trailing arms (which would be dead per the
        // checker's reachability rule).
        let mut variant_arms: Vec<&hew_hir::HirMatchArm> = Vec::new();
        let mut wildcard_arm: Option<&hew_hir::HirMatchArm> = None;
        for arm in arms {
            match &arm.predicate {
                hew_hir::HirMatchArmPredicate::EnumVariant { .. } => {
                    variant_arms.push(arm);
                }
                hew_hir::HirMatchArmPredicate::Wildcard
                | hew_hir::HirMatchArmPredicate::Binding { .. } => {
                    // Binding arms act as catch-all, identical to Wildcard from
                    // the tag-dispatch perspective. The difference (binding the
                    // scrutinee to a name) is handled in the body block below.
                    if wildcard_arm.is_none() {
                        wildcard_arm = Some(arm);
                    }
                }
                // Regex arms are routed to `lower_match_regex` by the
                // `lower_match` dispatcher; reaching here is a contract violation.
                hew_hir::HirMatchArmPredicate::Regex { .. } => {
                    unreachable!(
                        "Regex arm in lower_match_enum_tag — lower_match dispatcher \
                         should have routed regex arms to lower_match_regex"
                    )
                }
                hew_hir::HirMatchArmPredicate::Literal { .. } => {
                    panic!("checker invariant violated: mixed Literal/Variant arms");
                }
                hew_hir::HirMatchArmPredicate::RecordProject { .. }
                | hew_hir::HirMatchArmPredicate::TupleProject { .. } => {
                    panic!("checker invariant violated: mixed Variant/Project arms");
                }
            }
        }
        let generator_next_scrutinee = Self::is_generator_next_scrutinee(scrutinee);
        let recv_next_scrutinee = Self::is_recv_next_scrutinee(scrutinee);
        // Iterator clone-out is element-type agnostic: the synthetic Vec/Get
        // call returns a fresh owner for scalar, retained, and descriptor-backed
        // element classes alike.
        let vec_iter_next_scrutinee = self.is_vec_iter_next_scrutinee(scrutinee);

        // An enum projected out of a tuple owner can transfer that field's
        // release authority into the match temp. The ordinary field load is a
        // byte-copy, so the transfer is completed below by zeroing the tuple
        // slot and registering the temp as the sole enum owner.
        let projected_tuple_owner =
            if let HirExprKind::TupleIndex { tuple, index } = &scrutinee.kind {
                if let HirExprKind::BindingRef {
                    resolved: ResolvedRef::Binding(binding),
                    name,
                } = &tuple.kind
                {
                    let owns_source = self.owned_locals.iter().any(|entry| {
                        entry.binding == *binding && entry.disposition == Disposition::ScopeExit
                    });
                    (owns_source
                        && arms.iter().all(|arm| arm.guard.is_none())
                        && super::ty_is_heap_owning_enum_composite(
                            &self.subst_ty(&scrutinee.ty),
                            &self.record_field_orders,
                            &self.enum_layouts,
                            self.type_classes.lifecycle_registry(),
                        ))
                    .then(|| {
                        u32::try_from(*index).ok().map(|field| {
                            (
                                ProjectedScrutinee {
                                    binding: *binding,
                                    name: name.clone(),
                                    ty: self.subst_ty(&tuple.ty),
                                },
                                field,
                            )
                        })
                    })
                    .flatten()
                } else {
                    None
                }
            } else {
                None
            };

        // Recv/generator/iterator-next matches have specialised per-arm
        // release authority for the ephemeral result shell/payload. Do not
        // also mint the generic typed-publication owner for that same
        // generation while lowering the scrutinee.
        let specialised_scrutinee_owner =
            generator_next_scrutinee || recv_next_scrutinee || vec_iter_next_scrutinee;
        if specialised_scrutinee_owner {
            self.suppress_typed_produced_owner_sites
                .insert(scrutinee.site);
        }
        // Lower the scrutinee in the entry block. A failure propagates via `?`;
        // the half-built match leaves no dangling block.
        let scrutinee_place = self.lower_value(scrutinee);
        self.suppress_typed_produced_owner_sites
            .remove(&scrutinee.site);
        let scrutinee_place = scrutinee_place?;
        // An unguarded match that binds a payload out of an inline field
        // projection takes ownership of that field: the arm binder is minted a
        // scope-exit owner, so the root aggregate must not also release it.
        if arms.iter().all(|arm| arm.guard.is_none())
            && arms.iter().any(|arm| !arm.bindings.is_empty())
        {
            if let Err(error) = self.publish_consuming_match_projection(scrutinee) {
                self.report_field_load_classification_failure(
                    scrutinee.site,
                    &scrutinee.ty,
                    &error,
                );
                return None;
            }
        }
        let scrutinee_local = match scrutinee_place {
            Place::Local(n) => n,
            other => {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "match scrutinee place shape".to_string(),
                        site: scrutinee.site,
                    },
                    note: format!(
                        "match scrutinee must lower to Place::Local; got {other:?}. The HIR \
                         producer should only emit Match for enum-typed scrutinees backed by \
                         a local slot"
                    ),
                });
                return None;
            }
        };

        // #2523 — classify the scrutinee so a projected-payload move-out is
        // made sound the right way (see `classify_scrutinee_origin`). FAIL-CLOSED:
        // only a bare owning (non-captured) binding or a proven-ephemeral
        // producer takes the temp-neutralize consume path; a place, a wrapper, a
        // closure-captured binding, or any un-enumerated shape is REJECTED.
        let mut scrutinee_origin = self.classify_scrutinee_origin(scrutinee);
        let mut projected_tuple_owner_active = false;

        if let Some((owner, field)) = projected_tuple_owner {
            let source_root = self
                .instructions
                .iter()
                .rev()
                .find_map(|instr| match instr {
                    Instr::TupleFieldLoad {
                        tuple,
                        field_index,
                        dest,
                    } if *dest == scrutinee_place && *field_index == field => Some(*tuple),
                    _ => None,
                });
            if let Some(source_root) = source_root {
                let warrant = self.owner_warrant_for_admitted_temp(scrutinee);
                if !warrant.withholds_mint() {
                    self.push_instr(Instr::AggregateProjectionNeutralize {
                        root: source_root,
                        fields: vec![field],
                        transferee: scrutinee_place,
                        scope_exit_owner: None,
                    });
                    self.statements.push(MirStatement::AggregateAlias {
                        binding: owner.binding,
                        name: owner.name,
                        site: scrutinee.site,
                        ty: owner.ty,
                        partial_projection: true,
                    });
                    self.adopt_synthetic_owned_local(
                        SYNTHETIC_PROJECTED_SCRUTINEE_NAME,
                        scrutinee.site,
                        scrutinee_local,
                        self.subst_ty(&scrutinee.ty),
                        warrant,
                    );
                    scrutinee_origin = ProjectedPayloadOrigin::EphemeralTemp;
                    projected_tuple_owner_active = true;
                }
            }
        }

        // synthetic owned binding over its temp so the arm-destructured
        // payload is released on every exit edge — most importantly the loop
        // back-edge, where each iteration previously leaked one payload
        // (#2429). No-op when lowering did not publish an owned result for
        // this exact place and site; recv/iter-next shapes keep their own
        // payload release discipline below.
        let call_scrutinee_owner =
            self.register_from_call_scrutinee_owner(scrutinee, scrutinee_local);
        let call_scrutinee_owner_needs_arm_release = call_scrutinee_owner.is_some()
            && self
                .call_scrutinee_carrier_mint_locals
                .contains(&scrutinee_local)
            && super::composite_own::enum_payloads_are_shell_drop_safe(
                &self.subst_ty(&scrutinee.ty),
                &self.enum_layouts,
                &self.record_field_orders,
                &self.type_classes,
                &super::outbound_record_layouts(self),
                &self.opaque_handle_names,
                &self.lifecycle_registry,
            )
            // A carrier whose variant payload is a declared-release `#[resource]`
            // RECORD is EXCLUDED from every enum drop class in the drop planner
            // (`direct_payload_has_registered_resource_record`): a record close is
            // not null-safe over a consumed/zeroed slot, so no `EnumInPlace` shell
            // drop is ever scheduled for it. The arm-release protocol below aliases
            // the payload binder onto that (absent) carrier drop and hands arm
            // results through the whole-carrier funnel, which leaks a
            // bound-but-unconsumed handle and strands an unbalanced retain on a
            // moved-out sibling `Err(string)`. Fall back to the same discipline the
            // `#[opaque]` handle carrier already uses (its shell is likewise not
            // drop-safe, so this predicate is already false there): the payload
            // binder owns its own scope-exit close, and a moved-out sibling routes
            // through the `call_carrier_has_resource_payload` match-result funnel.
            && !super::composite_own::direct_payload_has_registered_resource_record(
                &self.subst_ty(&scrutinee.ty),
                &self.enum_layouts,
                &self.lifecycle_registry,
            );
        if call_scrutinee_owner_needs_arm_release {
            let carrier = Place::Local(scrutinee_local);
            self.owned_carrier_neutralize
                .entry(carrier)
                .or_insert(super::OwnedCarrierNeutralizeTarget::Whole(carrier));
        }
        let weak_upgrade_owner_ty = matches!(
            &scrutinee.kind,
            HirExprKind::RcIntrinsic {
                op: hew_types::RcIntrinsicOp::WeakUpgrade,
                ..
            }
        )
        .then(|| call_scrutinee_owner.as_ref().map(|(_, ty)| ty.clone()))
        .flatten();

        // Load the tag into a fresh i64 local. `Place::EnumTag(local)`
        // is the substrate primitive; codegen GEPs to outer-struct
        // field 0 and the Move arm widens the iW tag to i64 as needed.
        let tag_local = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::Move {
            dest: tag_local,
            src: Place::EnumTag(scrutinee_local),
        });

        // Allocate join block up front so every arm body can target it.
        let join_bb = self.alloc_block();

        // Reserve one body block per variant arm and one block for the
        // wildcard/binding (or the fail-closed trap when neither exists).
        let body_bbs: Vec<u32> = (0..variant_arms.len())
            .map(|_| self.alloc_block())
            .collect();
        let tail_bb = self.alloc_block();

        // Chain: emit one Branch per variant arm. The first compare lives
        // in the entry block (current block immediately after the tag
        // load); subsequent compares are in their own blocks linked
        // through `else_target`.
        //
        // Also collect `fallthrough_bbs[i]` — the block arm i jumps to when
        // its payload predicates or guard fail. For arm i this is
        // check_bb_{i+1} (or tail_bb for the last arm).
        let mut fallthrough_bbs: Vec<u32> = Vec::with_capacity(variant_arms.len());
        for (i, arm) in variant_arms.iter().enumerate() {
            // Allocate a constant local for the variant index and an
            // i1 result local for the equality compare.
            let k_local = self.alloc_local(ResolvedTy::I64);
            let variant_idx = match &arm.predicate {
                hew_hir::HirMatchArmPredicate::EnumVariant { variant_idx, .. } => *variant_idx,
                // variant_arms only contains EnumVariant arms (enforced in the
                // partition loop above); any other predicate is a contract violation.
                other => unreachable!(
                    "variant_arms must only contain EnumVariant predicates; got {other:?}"
                ),
            };
            self.push_instr(Instr::ConstI64 {
                dest: k_local,
                value: i64::from(variant_idx),
            });
            let cond_local = self.alloc_local(ResolvedTy::Bool);
            self.push_instr(Instr::IntCmp {
                pred: crate::model::CmpPred::Eq,
                lhs: tag_local,
                rhs: k_local,
                dest: cond_local,
            });

            let next_target = if i + 1 < variant_arms.len() {
                self.alloc_block()
            } else {
                tail_bb
            };
            fallthrough_bbs.push(next_target);
            self.finish_current_block(Terminator::Branch {
                cond: cond_local,
                then_target: body_bbs[i],
                else_target: next_target,
            });
            // Open the next check block (or the tail). For the last
            // variant arm we leave the cursor in `tail_bb` so the
            // wildcard / trap emission below can append to it.
            self.start_block(next_target);
        }

        // Tail block: either the wildcard/binding body or the fail-closed trap.
        if let Some(wildcard) = wildcard_arm {
            // Wildcard and binding arms may also have guards. When a guard is
            // present, failing it falls through to the exhaustiveness trap (no
            // subsequent arm can match a wildcard's position).
            let guard_failed_bb = self.alloc_block();
            if let Some(scope) = wildcard.scope {
                self.active_scopes.push(scope);
            }

            self.emit_match_arm_binding(
                wildcard,
                Place::Local(scrutinee_local),
                None, // no variant_idx; entire scrutinee is bound
            );

            if let Some(guard) = &wildcard.guard {
                let guard_place = self.lower_match_arm_guard(guard);
                if let Some(guard_local) = guard_place {
                    let body_bb = self.alloc_block();
                    self.finish_current_block(Terminator::Branch {
                        cond: guard_local,
                        then_target: body_bb,
                        else_target: guard_failed_bb,
                    });
                    self.start_block(body_bb);
                }
            }

            let wildcard_carrier_needs_arm_release = call_scrutinee_owner_needs_arm_release
                && !matches!(
                    wildcard.predicate,
                    hew_hir::HirMatchArmPredicate::Binding { .. }
                );
            let carrier_body_start_block = self.current_block_id;
            let carrier_body_start_instr = self.instructions.len();
            let active_carrier_mark = self.active_generator_yield_values.len();
            if wildcard_carrier_needs_arm_release {
                if let Some((_, ty)) = &call_scrutinee_owner {
                    self.active_generator_yield_values.push((
                        self.active_scopes.len(),
                        Place::Local(scrutinee_local),
                        ty.clone(),
                        crate::model::DropFnSpec::InPlace(
                            crate::ownership::InPlaceReleaseKind::Enum,
                        ),
                        carrier_body_start_block,
                        carrier_body_start_instr,
                    ));
                }
            }
            let value = self.lower_value(&wildcard.body);
            if let Some(src) = value {
                let src =
                    self.retain_typed_join_branch(result_site, &wildcard.body, src, result_ty);
                self.push_instr(Instr::Move {
                    dest: result_place,
                    src,
                });
            }
            self.active_generator_yield_values
                .truncate(active_carrier_mark);
            if !self.cursor_unreachable && wildcard_carrier_needs_arm_release {
                if let Some((binding, ty)) = &call_scrutinee_owner {
                    self.emit_generator_yield_binding_drop(
                        *binding,
                        Place::Local(scrutinee_local),
                        ty,
                        carrier_body_start_block,
                        carrier_body_start_instr,
                        wildcard.body.site,
                    );
                }
            }
            // A body that does not diverge leaves the cursor reachable and
            // flows to the join with the arm's value (which may be a Unit
            // no-op for an empty block). A diverging body (`return`/`panic`)
            // leaves the cursor in a dead block, so this Goto seals dead code.
            if !self.cursor_unreachable {
                join_reachable = true;
            }
            if let Some(scope) = wildcard.scope {
                self.emit_scope_exit_marker_with_carries([scope], [result_place]);
                self.active_scopes.pop();
            }
            self.finish_current_block(Terminator::Goto { target: join_bb });

            // Guard-failed block: belt-and-braces trap (exhaustiveness still
            // holds but guard rejected the sole remaining catch-all).
            self.start_block(guard_failed_bb);
            self.finish_current_block(Terminator::Trap {
                kind: crate::model::TrapKind::ExhaustivenessFallthrough,
            });
        } else {
            // Belt-and-braces runtime guard per LESSONS `match-fail-closed`
            // (P0). The checker rejects non-exhaustive enum matches at
            // compile time so this block is dead in well-typed programs;
            // the trap proves the property at runtime.
            self.finish_current_block(Terminator::Trap {
                kind: crate::model::TrapKind::ExhaustivenessFallthrough,
            });
        }

        // Variant arm body blocks. Payload predicates are checked first (if
        // any), then payload bindings are initialised from the dominated variant
        // payload field, then guards are evaluated (guards may reference those
        // payload bindings), then the arm body is lowered so body `BindingRef`s
        // resolve through `binding_locals` like ordinary lets.
        // Order: predicates → bindings → guard → body.
        for (i, arm) in variant_arms.iter().enumerate() {
            self.start_block(body_bbs[i]);
            if let Some(scope) = arm.scope {
                self.active_scopes.push(scope);
            }
            let variant_idx = match &arm.predicate {
                hew_hir::HirMatchArmPredicate::EnumVariant { variant_idx, .. } => *variant_idx,
                other => unreachable!(
                    "variant_arms must only contain EnumVariant predicates; got {other:?}"
                ),
            };
            let fallthrough_bb = fallthrough_bbs[i];

            // Payload predicate checks: compare literal values against
            // constructor payload fields. Each failed comparison branches to
            // `fallthrough_bb` (the next arm's check block or the tail).
            for pred in &arm.payload_predicates {
                let field_place = Place::MachineVariant {
                    local: scrutinee_local,
                    variant_idx,
                    field_idx: pred.field_idx,
                };
                let expected =
                    self.lower_match_literal_constant(&pred.literal, &pred.ty, arm.body.site)?;
                let cond_local = self.alloc_local(ResolvedTy::Bool);
                self.push_instr(Instr::IntCmp {
                    pred: CmpPred::Eq,
                    lhs: field_place,
                    rhs: expected,
                    dest: cond_local,
                });
                let pass_bb = self.alloc_block();
                self.finish_current_block(Terminator::Branch {
                    cond: cond_local,
                    then_target: pass_bb,
                    else_target: fallthrough_bb,
                });
                self.start_block(pass_bb);
            }

            // Nested constructor predicate checks (`Err(IoError::NotFound)`,
            // `Ok(Ok(v))`): recursively load each nested payload slot into a
            // transient local, compare its enum tag, and branch to
            // `fallthrough_bb` on mismatch. Inner bindings are queued and
            // materialised alongside the arm's own bindings below so the
            // predicate phase stays side-effect-free on the binding maps.
            let mut nested_binding_jobs: Vec<(u32, u32, hew_hir::HirMatchArmBinding)> = Vec::new();
            for pvp in &arm.payload_variant_predicates {
                self.emit_payload_variant_predicate_checks(
                    pvp,
                    scrutinee_local,
                    variant_idx,
                    fallthrough_bb,
                    arm.body.site,
                    &mut nested_binding_jobs,
                )?;
            }

            let arm_is_some = matches!(
                &arm.predicate,
                hew_hir::HirMatchArmPredicate::EnumVariant {
                    variant_match,
                    variant_idx: 0,
                } if variant_match.type_name == "Option"
                    && variant_match.variant_name == "Some"
            );
            let arm_is_generator_some = generator_next_scrutinee && arm_is_some;
            let arm_is_fresh_owned_vec_iter_some = vec_iter_next_scrutinee && arm_is_some;
            // Recv-call scrutinee `Some` arm: the runtime hands the consumer a
            // FRESH, solely-owned heap value per frame (an `alloc_cstring_data`
            // block for `string`, a fresh `Bytes` header for `bytes`). The
            // payload binding's only release path is the consuming body's
            // per-iteration drop — identical ownership shape to a generator
            // yield (the coro `.next()` drive returns the same kind of fresh
            // owned value). Without this drop every received frame leaks a
            // heap block per iteration: the leak this fix closes for
            // `for await item in rx` / `match channel.recv(...) { ... }` /
            // `match stream.recv() { ... }`.
            let arm_is_recv_some = recv_next_scrutinee && arm_is_some;
            let call_carrier_needs_skipped_payload_owner = call_scrutinee_owner.is_some()
                && match &self.subst_ty(&scrutinee.ty) {
                    ResolvedTy::Named { name, args, .. } => crate::model::find_enum_layout(
                        name,
                        args,
                        &self.enum_layouts,
                    )
                    .is_some_and(|layout| {
                        layout.variants.iter().any(|variant| {
                            variant.field_tys.iter().any(|field_ty| {
                                self.binding_seeds_drop_elaboration(&self.subst_ty(field_ty))
                            })
                        })
                    }),
                    _ => false,
                };
            let call_carrier_has_resource_payload = call_scrutinee_owner.is_some()
                && match &self.subst_ty(&scrutinee.ty) {
                    ResolvedTy::Named { name, args, .. } => crate::model::find_enum_layout(
                        name,
                        args,
                        &self.enum_layouts,
                    )
                    .is_some_and(|layout| {
                        layout.variants.iter().any(|variant| {
                            variant.field_tys.iter().any(|field_ty| {
                                // A resource RECORD payload (`is_opaque: false`)
                                // or an `#[opaque]` resource handle payload both
                                // make the carrier's whole-shell drop unsafe (the
                                // handle is closed exactly once by its own arm),
                                // so a sibling string/bytes payload moved out as
                                // the match result cannot rely on a carrier drop
                                // to balance its retain — it must transfer
                                // cleanly. Detect BOTH resource shapes here; the
                                // pre-fix check saw only the record and leaked
                                // the sibling `Err(string)` of a
                                // `Result<OpaqueHandle, string>`.
                                matches!(
                                    field_ty,
                                    ResolvedTy::Named {
                                        name,
                                        args,
                                        is_opaque: false,
                                        ..
                                    } if args.is_empty()
                                        && self.lifecycle_registry.resource_record(
                                            &hew_types::DefId::legacy_reconstruct_from_full_path(name),
                                        ).is_some()
                                ) || self
                                    .lifecycle_registry
                                    .opaque_resource_for_ty(field_ty)
                                    .is_some()
                            })
                        })
                    }),
                    _ => false,
                };
            let mut overwritten_bindings = Vec::with_capacity(arm.bindings.len());
            let mut call_carrier_match_result_candidates = Vec::new();
            // Contextual `Sink` handoffs admitted while destructuring this
            // arm's payload binders. The destructure runs BEFORE the arm's
            // guard decides, so the slot-clearing instruction is held here and
            // emitted on the arm-selected edge below.
            let mut pending_sink_handoffs: Vec<Instr> = Vec::new();
            // Fresh `Some(x)` bindings whose payload owns heap. VecIter clone
            // reads, generator drives, and receiver reads all hand the body a
            // fresh sole owner, so one shared lifecycle releases it at
            // body/edge exit or records its ownership transfer.
            // Removed from `owned_locals` below so the function-scope drop
            // pass does not also fire (which would double-free).
            let mut generator_yield_drop_bindings = Vec::new();
            for binding in &arm.bindings {
                let binding_ty = self.subst_ty(&binding.ty);
                self.push_bind_statement(
                    binding.binding,
                    binding.name.clone(),
                    arm.body.site,
                    binding_ty.clone(),
                );
                self.record_match_arm_binding_scope(binding.binding, arm);
                // A mixed-return wrapper has no shell owner: the whole Result
                // may contain an opaque sibling. Its selected payload can still
                // be a measured transferred owner, proven by the active
                // `(variant, field)` summary. That authority is deliberately
                // used only when no whole call-scrutinee owner was minted, so a
                // fresh Result never gains a second payload release.
                let active_variant_payload_warrant = call_scrutinee_owner
                    .is_none()
                    .then(|| {
                        self.owner_warrant_for_fresh_variant_payload(
                            scrutinee,
                            variant_idx,
                            binding.field_idx,
                            &binding_ty,
                        )
                    })
                    .flatten();
                let fresh_active_payload = active_variant_payload_warrant.is_some();
                let warrant = active_variant_payload_warrant.unwrap_or_else(|| {
                    self.owner_warrant_for_scrutinee_payload(
                        binding.binding,
                        scrutinee,
                        &binding_ty,
                    )
                });
                let keep_for_drop_elab =
                    self.binding_seeds_drop_elaboration(&binding_ty) && !warrant.withholds_mint();
                let dest = self.alloc_local(binding.ty.clone());
                let previous = self.binding_locals.insert(binding.binding, dest);
                let payload_source = Place::MachineVariant {
                    local: scrutinee_local,
                    variant_idx,
                    field_idx: binding.field_idx,
                };
                self.push_instr(Instr::Move {
                    dest,
                    src: payload_source,
                });
                if keep_for_drop_elab {
                    let unguarded_payload_move =
                        call_scrutinee_owner.is_some() && arm.guard.is_none() && keep_for_drop_elab;
                    self.register_owned_payload_binder(
                        binding.binding,
                        binding.name.clone(),
                        binding_ty.clone(),
                        warrant,
                        scrutinee_local,
                        call_scrutinee_owner.as_ref(),
                    );
                    // A path-complete direct-call carrier release owns every
                    // untransferred payload slot on the selected arm. A
                    // `Weak.upgrade` payload follows that rule only while the
                    // carrier slot remains populated. An unguarded physical
                    // payload move is neutralized below, so its binder keeps
                    // the exact owner instead of becoming an alias whose
                    // emptied carrier cannot release it.
                    let binder_is_contextual_sink = matches!(
                        super::drop_plan::resource_drop_fn(&binding_ty, &self.type_classes),
                        Some(crate::model::DropFnSpec::Runtime(
                            hew_types::runtime_call::RuntimeDropDescriptor::SinkClose
                        ))
                    );
                    if (weak_upgrade_owner_ty.is_some() && !unguarded_payload_move)
                        || (call_scrutinee_owner_needs_arm_release
                            && arm.guard.is_some()
                            && !binder_is_contextual_sink)
                    {
                        self.set_owned_local_disposition(binding.binding, Disposition::AliasOf);
                    }
                }
                if fresh_active_payload && keep_for_drop_elab {
                    self.fresh_variant_payload_bindings.insert(binding.binding);
                    if let Some(local) = base_local(dest) {
                        self.fresh_variant_payload_binder_locals.insert(local);
                    }
                }
                // Whole call-carrier payloads normally remain aliases of the
                // shell and are released by its terminal composite drop.
                // Recursive record payloads moved out as the match result are
                // the narrow exception: record drop admission cannot infer
                // the transfer through the MachineVariant projection. CoW
                // strings/bytes already carry their own retain-aware transfer
                // authority; exempting them here would admit both the binder
                // and shell releases for one owner.
                if call_scrutinee_owner.is_some()
                    && arm.guard.is_some()
                    && keep_for_drop_elab
                    && (self.is_owned_aggregate_record_ty(&binding_ty)
                        || call_carrier_has_resource_payload)
                {
                    if let Some(local) = base_local(dest) {
                        call_carrier_match_result_candidates.push((binding.binding, local));
                    }
                }
                // Clearing the carrier slot here would neutralize it before a
                // guard on this arm has selected it: a false guard falls
                // through to a later arm that re-destructures the same slot and
                // binds a null handle, while this arm's dead binder keeps the
                // real close authority. Defer to the arm-selected edge — the
                // same neutralize-before-guard hazard `in_fallthrough_match_guard`
                // rejects for an in-guard consume.
                let sink_handoff = self.contextual_sink_payload_handoff(
                    call_scrutinee_owner.as_ref(),
                    binding.binding,
                    payload_source,
                    dest,
                    &binding_ty,
                );
                let transferred_sink = sink_handoff.is_some();
                pending_sink_handoffs.extend(sink_handoff);
                if transferred_sink && keep_for_drop_elab {
                    self.set_owned_local_disposition(binding.binding, Disposition::ScopeExit);
                }
                if projected_tuple_owner_active && keep_for_drop_elab {
                    self.push_move_out_neutralize(
                        payload_source,
                        crate::model::NeutralizeAuthority::EphemeralTempConsume,
                    );
                }
                // Once an unguarded arm is selected, moving its active payload
                // into the binder is an ownership transfer, not a long-lived
                // alias. Commit the structural move path immediately so every
                // later unwind edge cleans the binder generation. Guarded arms
                // defer this commit until their true edge; neutralizing before
                // a guard would corrupt fallthrough into sibling arms.
                let payload_binding_transfer_committed = call_scrutinee_owner.is_some()
                    && arm.guard.is_none()
                    && keep_for_drop_elab
                    && !transferred_sink;
                if payload_binding_transfer_committed {
                    self.push_instr(Instr::NeutralizePayloadSlot {
                        place: payload_source,
                        transferee: Some(dest),
                        authority: crate::model::NeutralizeAuthority::PayloadBindingTransfer,
                    });
                }
                // An owned call-carrier scrutinee gets a terminal snapshot
                // drop on every exit, so a payload binder that MOVES the
                // payload out must neutralize the variant slot on that arm —
                // the funnel authority registered here fires only when the
                // binder crosses an ownership boundary.
                if !transferred_sink && !payload_binding_transfer_committed {
                    self.note_carrier_payload_binder(
                        scrutinee_local,
                        payload_source,
                        dest,
                        &binding_ty,
                    );
                }
                overwritten_bindings.push((binding.binding, previous, keep_for_drop_elab));
                // #2523 — record provenance for a heap-owning TOP-LEVEL projected
                // payload binder so its `Consume`-intent move-out routes through
                // the default-deny consume hook.
                //
                // EXCEPTION — the VecIter / generator / recv `Some(x)`
                // arms carry a FRESH, solely-owned per-frame payload (the
                // synthetic `Option` shell holds a clone-out value, a coro
                // yield, or a received frame). Its release is already
                // owned by the arm's own `Disposition::BodyEndReleased` +
                // escape-suppression discipline (registered below). It is NOT a
                // projection of a re-readable aggregate that retains the bits,
                // so a move-out (`return line`, store to a longer-lived owner)
                // is a legitimate ownership transfer, not a dangling-source
                // hazard. Routing it through default-deny would falsely reject
                // that transfer as a re-readable-place move-out. Skip it.
                let is_fresh_owned_frame_payload =
                    arm_is_fresh_owned_vec_iter_some || arm_is_generator_some || arm_is_recv_some;
                if !is_fresh_owned_frame_payload && !payload_binding_transfer_committed {
                    // A binder that took the contextual `Sink` handoff is still a
                    // projected payload and must route its consumes through the
                    // same default-deny hook — a guard that consumes it (or moves
                    // it out) has to be refused fail-closed like any other. Its
                    // origin is not the generic scrutinee verdict: the handoff's
                    // admission already PROVED a fresh, solely-owned `Result`
                    // carrier, which is exactly `EphemeralTemp` — nulling the
                    // variant slot transfers ownership with no re-readable origin
                    // left behind.
                    let origin = if transferred_sink || fresh_active_payload {
                        ProjectedPayloadOrigin::EphemeralTemp
                    } else {
                        scrutinee_origin.clone()
                    };
                    self.record_projected_payload_provenance(
                        binding.binding,
                        &binding.name,
                        Place::MachineVariant {
                            local: scrutinee_local,
                            variant_idx,
                            field_idx: binding.field_idx,
                        },
                        origin,
                        keep_for_drop_elab,
                    );
                }
                if arm_is_fresh_owned_vec_iter_some || arm_is_generator_some || arm_is_recv_some {
                    // The picker verdict is consulted HERE, before this
                    // binding can be retracted from `owned_locals` — the
                    // fail-closed check therefore covers every binding that
                    // reaches the yield/recv release seam, including the ones
                    // the end-of-pass `unsupported_vec_element_diagnostics`
                    // scan never sees (that scan reads the FINAL
                    // `owned_locals`, and a retracted binding is gone from it
                    // by then).
                    match self.generator_yield_drop_symbol(&binding_ty) {
                        ReleaseSymbolVerdict::Wired(_) | ReleaseSymbolVerdict::WiredInPlace(_) => {
                            // The yielded/received payload owns heap (a
                            // `string`, a `Vec`, a `Bytes`, or a heap-owning
                            // record/enum composite) with a wired release.
                            // Schedule a body-end release and take it
                            // back out of `owned_locals` so the function-scope
                            // drop pass cannot also fire (double-free guard).
                            // The body-shape drop-safety scan in
                            // `emit_generator_yield_binding_drop` refuses to
                            // emit if the value escapes the body. The
                            // recv-call surface (`for await item in rx`,
                            // `match channel.recv(...)`,
                            // `match stream.recv()`) reuses this exact
                            // discipline because the recv runtime's ownership
                            // contract is identical: each `Some(item)` is a
                            // fresh heap allocation the consumer alone is
                            // responsible for releasing.
                            if keep_for_drop_elab {
                                self.set_owned_local_disposition(
                                    binding.binding,
                                    Disposition::BodyEndReleased,
                                );
                            }
                            // The binder class registry: frame-owned value,
                            // body-end release authority. Consulted by the
                            // projection-alias taint seed and the
                            // retained-share lowering of consuming uses.
                            if let Some(local) = base_local(dest) {
                                self.yield_binder_locals.insert(local);
                            }
                            generator_yield_drop_bindings.push((
                                binding.binding,
                                dest,
                                binding_ty,
                                arm.body.site,
                            ));
                        }
                        ReleaseSymbolVerdict::NoDropPath => {
                            // No validated consumer-drop path: the binding
                            // keeps its `owned_locals` entry and the
                            // function-scope machinery decides, leak-as-before
                            // rather than risking a double-free. Concrete
                            // VecIter elements reaching this class are rejected
                            // by the checker's clone-totality gate.
                        }
                        ReleaseSymbolVerdict::Unwired(_) => {
                            // Fail closed: the frame owns heap (a `Vec` of
                            // `bytes` or of an indirect-enum element) that no
                            // wired symbol can release — a buffer-only
                            // `hew_vec_free` would leak every element node,
                            // once per delivered frame. Reject at compile
                            // time. The binding is still retracted so the
                            // final-`owned_locals` scan does not stack a
                            // second diagnostic on the same construct.
                            if keep_for_drop_elab {
                                self.set_owned_local_disposition(
                                    binding.binding,
                                    Disposition::BodyEndReleased,
                                );
                            }
                            let elem = self
                                .unsupported_vec_element_in_ty(&binding_ty)
                                .unwrap_or_else(|| format!("`{}`", binding_ty.user_facing()));
                            self.diagnostics.push(MirDiagnostic {
                                kind: MirDiagnosticKind::NotYetImplemented {
                                    construct: format!(
                                        "`{}`: a `Vec` whose element is {elem} has no \
                                         per-element release protocol, so every yielded or \
                                         received frame would leak its heap nodes",
                                        binding.name
                                    ),
                                    site: arm.body.site,
                                },
                                note: "a generator yield or channel receive hands the \
                                       consuming body a fresh, solely-owned `Vec` per frame, \
                                       and a `Vec` of `bytes` or of an indirect-enum element \
                                       cannot yet be released element-by-element. This \
                                       construction is rejected at compile rather than \
                                       silently leaked once per iteration, and becomes \
                                       available once the per-element release is wired."
                                    .to_string(),
                            });
                        }
                    }
                }
            }

            // A wildcard payload (`Ok(_)` / `Err(_)`) has no HIR binding, so
            // the ordinary binder-owner path has nowhere to attach the active
            // variant's release. Materialise a synthetic, arm-local owner only
            // for a field that carries the same fresh-transfer proof. This is
            // the discard twin of the bound-payload path above: it still moves
            // the selected field once and gets one exit drop, while an opaque
            // sibling or an unproven field remains untouched (leak, never a
            // guessed release).
            if call_scrutinee_owner.is_none() || call_carrier_needs_skipped_payload_owner {
                use crate::model::HeapOwnershipLayouts as _;
                let bound_fields: HashSet<u32> = arm.bindings.iter().map(|b| b.field_idx).collect();
                let predicate_fields: HashSet<u32> = arm
                    .payload_variant_predicates
                    .iter()
                    .map(|p| p.field_idx)
                    .collect();
                let subst = self.subst_ty(&scrutinee.ty);
                let skipped_fields = match &subst {
                    ResolvedTy::Named { name, args, .. } => {
                        let layouts = crate::model::MirHeapLayouts {
                            record_field_orders: &self.record_field_orders,
                            enum_layouts: &self.enum_layouts,
                        };
                        layouts
                            .enum_variant_field_tys(name, args)
                            .and_then(|variants| variants.get(variant_idx as usize).cloned())
                            .into_iter()
                            .flatten()
                            .enumerate()
                            .filter_map(|(field_idx, ty)| {
                                let field_idx = u32::try_from(field_idx).expect(
                                    "checked enum field index must fit the MIR u32 carrier",
                                );
                                (!bound_fields.contains(&field_idx)
                                    && !predicate_fields.contains(&field_idx))
                                .then(|| (field_idx, self.subst_ty(&ty)))
                            })
                            .collect::<Vec<_>>()
                    }
                    _ => Vec::new(),
                };
                for (field_idx, field_ty) in skipped_fields {
                    if !self.binding_seeds_drop_elaboration(&field_ty) {
                        continue;
                    }
                    let warrant = if call_carrier_needs_skipped_payload_owner {
                        let admitted = self.owner_warrant_for_admitted_temp(scrutinee);
                        if admitted.withholds_mint() {
                            // The whole-carrier admission is opaque-tainted (a
                            // resource sibling in another variant), so it
                            // withholds. A DOMESTIC skipped leaf is still sound
                            // to release here when the callee measurably returns
                            // a fresh payload for THIS exact variant slot: fall
                            // back to the field-precise freshness authority so a
                            // `Result<OpaqueResource, string>`'s discarded
                            // `Err(_)` string is released instead of leaked. The
                            // field warrant only grants on proven freshness, so
                            // an opaque/foreign sibling stays withheld.
                            self.owner_warrant_for_fresh_variant_payload(
                                scrutinee,
                                variant_idx,
                                field_idx,
                                &field_ty,
                            )
                        } else {
                            Some(admitted)
                        }
                    } else {
                        self.owner_warrant_for_fresh_variant_payload(
                            scrutinee,
                            variant_idx,
                            field_idx,
                            &field_ty,
                        )
                    };
                    let Some(warrant) = warrant else {
                        continue;
                    };
                    let dest = self.alloc_local(field_ty.clone());
                    let local = base_local(dest).expect(
                        "alloc_local must produce a Local place for active-variant payload ownership",
                    );
                    let binding = self.adopt_synthetic_owned_local(
                        "__hew_active_variant_payload",
                        arm.body.site,
                        local,
                        field_ty.clone(),
                        warrant,
                    );
                    self.record_match_arm_binding_scope(binding, arm);
                    self.set_owned_local_disposition(binding, Disposition::BodyEndReleased);
                    self.fresh_variant_payload_bindings.insert(binding);
                    self.fresh_variant_payload_binder_locals.insert(local);
                    generator_yield_drop_bindings.push((binding, dest, field_ty, arm.body.site));
                    let source = Place::MachineVariant {
                        local: scrutinee_local,
                        variant_idx,
                        field_idx,
                    };
                    self.push_instr(Instr::Move { dest, src: source });
                    // A whole call carrier normally drops its active payload.
                    // The synthetic wildcard owner takes that payload instead,
                    // so clear the carrier slot on this arm. This is essential
                    // when a consuming field-bearing resource arm makes the
                    // carrier's recursive drop unsafe: the sibling wildcard
                    // still owns and releases its payload exactly once.
                    if call_carrier_needs_skipped_payload_owner {
                        self.push_instr(Instr::NeutralizePayloadSlot {
                            place: source,
                            transferee: Some(dest),
                            authority: crate::model::NeutralizeAuthority::PayloadBindingTransfer,
                        });
                    }
                }
            }

            // Nested constructor payload bindings (the `v` in `Ok(Ok(v))`):
            // same registration discipline as the arm's own bindings above —
            // `Bind` statement, `owned_locals` entry for non-BitCopy types so
            // the function-scope drop elaboration releases exactly once, and
            // a `binding_locals` slot so guard/body references resolve. The
            // source place projects from the transient nested-payload local
            // the predicate phase loaded, not from the scrutinee directly.
            // The vec-iter/generator/recv special drops above are top-level-
            // `Some`-payload concerns and cannot apply at nesting depth ≥ 1.
            for (src_local, src_variant_idx, binding) in nested_binding_jobs {
                let binding_ty = self.subst_ty(&binding.ty);
                self.push_bind_statement(
                    binding.binding,
                    binding.name.clone(),
                    arm.body.site,
                    binding_ty.clone(),
                );
                self.record_match_arm_binding_scope(binding.binding, arm);
                let warrant = self.owner_warrant_for_scrutinee_payload(
                    binding.binding,
                    scrutinee,
                    &binding_ty,
                );
                let keep_for_drop_elab =
                    self.binding_seeds_drop_elaboration(&binding_ty) && !warrant.withholds_mint();
                let dest = self.alloc_local(binding.ty.clone());
                let previous = self.binding_locals.insert(binding.binding, dest);
                self.push_instr(Instr::Move {
                    dest,
                    src: Place::MachineVariant {
                        local: src_local,
                        variant_idx: src_variant_idx,
                        field_idx: binding.field_idx,
                    },
                });
                if keep_for_drop_elab {
                    self.register_owned_local(
                        binding.binding,
                        binding.name.clone(),
                        binding_ty,
                        warrant,
                    );
                }
                overwritten_bindings.push((binding.binding, previous, keep_for_drop_elab));
                // #2523 F2 — a NESTED-pattern payload binder is bound from a
                // TRANSIENT copy the predicate phase loaded (`src_local`), NOT
                // from the outer value's real storage. Nulling that transient
                // cannot reach the outer value's nested slot, so a heap-owning
                // move-out would leave it dangling (double-free / leak). Record
                // provenance with the `NestedDestructure` reject reason so the
                // move-out is rejected fail-closed; a borrow-only nested binder
                // never hits the consume hook and is unaffected.
                self.record_projected_payload_provenance(
                    binding.binding,
                    &binding.name,
                    Place::MachineVariant {
                        local: src_local,
                        variant_idx: src_variant_idx,
                        field_idx: binding.field_idx,
                    },
                    ProjectedPayloadOrigin::Reject(ProjectedPayloadRejectReason::NestedDestructure),
                    keep_for_drop_elab,
                );
            }
            // Failure falls through to `fallthrough_bb` (re-try next arm).
            if let Some(guard) = &arm.guard {
                let guard_place = self.lower_match_arm_guard(guard);
                if let Some(guard_local) = guard_place {
                    let body_entry_bb = self.alloc_block();
                    self.finish_current_block(Terminator::Branch {
                        cond: guard_local,
                        then_target: body_entry_bb,
                        else_target: fallthrough_bb,
                    });
                    self.start_block(body_entry_bb);
                }
            }

            // The arm is now selected on every reaching path: clear the
            // carrier slots whose payloads this arm's binders own.
            for instr in pending_sink_handoffs.drain(..) {
                self.push_instr(instr);
            }

            let body_start_block_id = self.current_block_id;
            let body_start_instr_len = self.instructions.len();
            // A projected handoff discharges only the selected slot. Register
            // the whole carrier as well so its tag-aware drop releases the
            // neutralized shell and every unselected slot on the arm edge. A
            // whole-carrier transfer emitted while lowering the body remains
            // an escape and suppresses that edge drop in the shared scan.
            if call_scrutinee_owner_needs_arm_release {
                if let Some((binding, ty)) = &call_scrutinee_owner {
                    generator_yield_drop_bindings.push((
                        *binding,
                        Place::Local(scrutinee_local),
                        ty.clone(),
                        arm.body.site,
                    ));
                }
            }

            // Register the iteration's yielded heap values as active so a
            // `break`/`continue` inside the body frees them on its edge
            // (symmetric to the generator-handle release). The depth marker is
            // the current `active_scopes` length: a break/continue at
            // `loop_scope_depth <= marker` is leaving/looping a loop this value
            // is lexically inside, so it must free it. Drained after the body
            // lowers (the fall-through path uses the body-end drop instead).
            let active_yield_mark = self.active_generator_yield_values.len();
            for (_binding, place, ty, _site) in &generator_yield_drop_bindings {
                let is_minted_call_carrier = base_local(*place)
                    .is_some_and(|local| self.call_scrutinee_carrier_mint_locals.contains(&local));
                let drop_fn = if is_minted_call_carrier {
                    // The typed call-result publication minted the whole
                    // carrier owner. Its selected payload may transfer out,
                    // but the neutralized shell still needs its terminal
                    // recursive release on this edge. This is the same
                    // path-complete authority used by the wildcard arm above;
                    Some(crate::model::DropFnSpec::InPlace(
                        crate::ownership::InPlaceReleaseKind::Enum,
                    ))
                } else {
                    match self.generator_yield_drop_symbol(ty) {
                        ReleaseSymbolVerdict::Wired(symbol) => {
                            Some(crate::model::DropFnSpec::Release(symbol))
                        }
                        ReleaseSymbolVerdict::WiredInPlace(kind) => {
                            Some(crate::model::DropFnSpec::InPlace(kind))
                        }
                        ReleaseSymbolVerdict::NoDropPath | ReleaseSymbolVerdict::Unwired(_) => None,
                    }
                };
                if let Some(drop_fn) = drop_fn {
                    let depth = self.active_scopes.len();
                    self.active_generator_yield_values.push((
                        depth,
                        *place,
                        ty.clone(),
                        drop_fn,
                        body_start_block_id,
                        body_start_instr_len,
                    ));
                }
            }
            let mut value = self.lower_composite_result_value(&arm.body);
            // A direct whole-record match result (`.Some(g) => g`) transfers the
            // selected field out of a proved-fresh call carrier. The carrier's
            // active payload drop is suppressed on that arm, so the resulting
            // binder is the sole recursive owner. Exempt exactly that moved-out
            // record binder from projection taint. Merely reading a payload in
            // an otherwise statement-valued arm leaves the carrier as owner,
            // while CoW leaves use their separate retain-aware authority.
            if let Some(value_local) = value.and_then(base_local) {
                for (binding, local) in &call_carrier_match_result_candidates {
                    if *local == value_local {
                        self.fresh_variant_payload_bindings.insert(*binding);
                        self.fresh_variant_payload_binder_locals.insert(*local);
                        if let Some(source_place) = self
                            .projected_payload_provenance
                            .get(binding)
                            .map(|provenance| provenance.source_place)
                        {
                            // A resource payload moved out of the carrier is
                            // already neutralized on this arm by the projected
                            // move-out lowering (`ProjectedPayloadOrigin::
                            // EphemeralTemp`). Adding the match-result neutralize
                            // for the SAME slot would double-null it — harmless at
                            // runtime, but it violates the one-transfer-per-arm
                            // structural contract. Only mint the match-result
                            // neutralize when no earlier neutralize on this exact
                            // slot exists (the string/record match-result case,
                            // which has no independent resource move-out).
                            let already_neutralized = self.instructions.iter().any(|instr| {
                                matches!(
                                    instr,
                                    Instr::NeutralizePayloadSlot { place, .. }
                                        if *place == source_place
                                )
                            });
                            if !already_neutralized {
                                self.push_instr(Instr::NeutralizePayloadSlot {
                                    place: source_place,
                                    transferee: None,
                                    authority: crate::model::NeutralizeAuthority::MoveOutArmConsume,
                                });
                            }
                        }
                    }
                }
                if self.is_owned_aggregate_record_ty(result_ty) {
                    let source_place = self.projected_payload_provenance.iter().find_map(
                        |(binding, provenance)| {
                            (self.binding_locals.get(binding).copied()
                                == Some(Place::Local(value_local))
                                && base_local(provenance.source_place).is_some_and(|root| {
                                    self.call_scrutinee_carrier_mint_locals.contains(&root)
                                }))
                            .then_some(provenance.source_place)
                        },
                    );
                    if let Some(place) = source_place {
                        self.push_instr(Instr::NeutralizePayloadSlot {
                            place,
                            transferee: None,
                            authority: crate::model::NeutralizeAuthority::MoveOutArmConsume,
                        });
                    }
                }
            }
            if call_scrutinee_owner_needs_arm_release {
                if let Some(payload) = value {
                    if self.owned_carrier_authority(payload).is_some() {
                        value = Some(self.transfer_owned_carrier_place(payload, result_ty));
                    }
                }
            }

            // Drain the entries this arm registered; break/continue inside the
            // body has already cloned-freed them on its edges.
            self.active_generator_yield_values
                .truncate(active_yield_mark);

            for (binding, previous, keep_for_drop_elab) in overwritten_bindings.into_iter().rev() {
                // Owned arm payloads stay addressable for function-wide drop elaboration;
                // lexical liveness is still narrowed by the exit-state dataflow.
                if keep_for_drop_elab {
                    continue;
                }
                if let Some(previous) = previous {
                    self.binding_locals.insert(binding, previous);
                } else {
                    self.binding_locals.remove(&binding);
                }
            }

            if let Some(src) = value {
                let src = self.retain_typed_join_branch(result_site, &arm.body, src, result_ty);
                self.push_composite_result_move(result_place, src, result_ty);
            }
            for (binding, place, ty, site) in generator_yield_drop_bindings {
                let is_minted_call_carrier = base_local(place)
                    .is_some_and(|local| self.call_scrutinee_carrier_mint_locals.contains(&local));
                if is_minted_call_carrier {
                    if let Some(local) = base_local(place) {
                        if self.call_carrier_body_end_drop_safe(
                            binding,
                            body_start_block_id,
                            body_start_instr_len,
                            local,
                        ) {
                            self.push_instr(Instr::Drop {
                                place,
                                ty: ty.clone(),
                                drop_fn: Some(crate::model::DropFnSpec::InPlace(
                                    crate::ownership::InPlaceReleaseKind::Enum,
                                )),
                            });
                            self.record_body_end_release_event(binding, place, &ty, site);
                        }
                    }
                } else {
                    self.emit_generator_yield_binding_drop(
                        binding,
                        place,
                        &ty,
                        body_start_block_id,
                        body_start_instr_len,
                        site,
                    );
                }
            }
            // A non-diverging arm body leaves the cursor reachable: this Goto
            // links a live predecessor into the join. A diverging body
            // (`return`/`panic`) leaves the cursor in a dead block (the
            // statement lowering for `return` flagged `cursor_unreachable`),
            // so the Goto seals dead code and contributes no live edge.
            if !self.cursor_unreachable {
                join_reachable = true;
            }
            if let Some(scope) = arm.scope {
                self.emit_scope_exit_marker_with_carries([scope], [result_place]);
                self.active_scopes.pop();
            }
            self.finish_current_block(Terminator::Goto { target: join_bb });
        }

        // Join. Subsequent lowering continues here. When every arm diverged
        // (no arm fell through with a value), the join has no live predecessor:
        // flag the cursor unreachable so the caller does not emit a Move/Return
        // reading the never-written `result_place`. `start_block` resets the
        // flag, so set it AFTER opening the join.
        self.start_block(join_bb);
        if join_reachable {
            // Weak.upgrade produces a fresh Option<Rc<T>>. Release that owner
            // as soon as the match closes so a statement-valued match does not
            // retain an upgraded strong reference until function exit. The
            // in-place drop zeroes the Option slot; the existing exit plan is
            // still responsible for early return, panic, and cancellation and
            // becomes a no-op after this normal-path release.
            if let Some(owner_ty) = weak_upgrade_owner_ty {
                self.push_instr(Instr::Drop {
                    place: Place::Local(scrutinee_local),
                    ty: owner_ty,
                    drop_fn: Some(crate::model::DropFnSpec::InPlace(
                        crate::ownership::InPlaceReleaseKind::Enum,
                    )),
                });
            }
        } else {
            self.cursor_unreachable = true;
        }
        Some(result_place)
    }

    /// Emit the tag-check chain for one nested constructor payload predicate
    /// (recursive).
    ///
    /// Loads the payload slot `pred.field_idx` of the parent variant into a
    /// fresh `payload_ty`-typed local, compares that local's `EnumTag`
    /// against `pred.variant_idx`, and branches: match → a fresh pass block
    /// (left as the current block), mismatch → `fallthrough_bb` (the next
    /// arm's check block or the exhaustiveness tail).
    ///
    /// Ownership: the transient payload local is a non-owning alias of the
    /// parent variant's payload — it gets no `Bind` statement and no
    /// `owned_locals` entry, so drop elaboration never releases it directly.
    /// The parent composite (ultimately the match scrutinee) remains the
    /// registered owner and frees the loaded heap content through its
    /// recursive tag-aware `DropKind::EnumInPlace` scope-exit drop, which
    /// descends through this nesting depth. For that to hold, no ownership
    /// scan may misread the
    /// reads this method emits as payload escapes: the inner tag is read with
    /// `Place::EnumTag` (a bitcopy discriminant, exempted as a tag read), and
    /// the i64 tag destination is never tainted as a payload binder (the
    /// heap-owning propagation guard). Inner bindings extract ownership
    /// exactly like top-level arm bindings; they are queued into
    /// `binding_jobs` as `(parent_local, parent_variant_idx, binding)` and
    /// materialised by the caller in the binding phase, where they become
    /// same-scope payload binders the composite drop coordinates with (a bound
    /// inner string is read-but-not-independently-dropped, so the composite is
    /// its single owner — no double-free, no leak).
    pub(crate) fn emit_payload_variant_predicate_checks(
        &mut self,
        pred: &hew_hir::HirPayloadVariantPredicate,
        parent_local: u32,
        parent_variant_idx: u32,
        fallthrough_bb: u32,
        site: SiteId,
        binding_jobs: &mut Vec<(u32, u32, hew_hir::HirMatchArmBinding)>,
    ) -> Option<()> {
        let payload_ty = self.subst_ty(&pred.payload_ty);
        let payload_place = self.alloc_local(payload_ty);
        let Some(payload_local) = base_local(payload_place) else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "nested constructor payload local shape".to_string(),
                    site,
                },
                note: format!(
                    "nested payload predicate requires a Place::Local; got {payload_place:?}"
                ),
            });
            return None;
        };
        self.push_instr(Instr::Move {
            dest: payload_place,
            src: Place::MachineVariant {
                local: parent_local,
                variant_idx: parent_variant_idx,
                field_idx: pred.field_idx,
            },
        });
        let tag_local = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::Move {
            dest: tag_local,
            src: Place::EnumTag(payload_local),
        });
        let k_local = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::ConstI64 {
            dest: k_local,
            value: i64::from(pred.variant_idx),
        });
        let cond_local = self.alloc_local(ResolvedTy::Bool);
        self.push_instr(Instr::IntCmp {
            pred: CmpPred::Eq,
            lhs: tag_local,
            rhs: k_local,
            dest: cond_local,
        });
        let pass_bb = self.alloc_block();
        self.finish_current_block(Terminator::Branch {
            cond: cond_local,
            then_target: pass_bb,
            else_target: fallthrough_bb,
        });
        self.start_block(pass_bb);
        for binding in &pred.bindings {
            binding_jobs.push((payload_local, pred.variant_idx, binding.clone()));
        }
        for child in &pred.nested {
            self.emit_payload_variant_predicate_checks(
                child,
                payload_local,
                pred.variant_idx,
                fallthrough_bb,
                site,
                binding_jobs,
            )?;
        }
        Some(())
    }

    /// Emit the binding for a `Binding`-predicate arm and register it in
    /// `binding_locals`.
    ///
    /// A guarded arm is only tentatively selected. Its binder therefore cannot
    /// consume a `CoW` scrutinee before the guard succeeds: the false edge must
    /// retry the next arm with the original value. Retain the string/bytes
    /// share before copying it into the binder, giving that arm an independent
    /// owner which its guard/body exits can release. Unguarded binders keep the
    /// ordinary last-use move/transfer path.
    ///
    /// `variant_idx` is `Some` when the arm is inside an enum-tag dispatch
    /// (binding a variant payload rather than the whole scrutinee); `None`
    /// for top-level binding arms where the entire scrutinee is bound.
    fn emit_match_arm_binding(
        &mut self,
        arm: &hew_hir::HirMatchArm,
        scrutinee_local: Place,
        _variant_idx: Option<u32>,
    ) {
        let hew_hir::HirMatchArmPredicate::Binding {
            binding_id,
            name,
            ty,
        } = &arm.predicate
        else {
            // Not a binding arm — nothing to do.
            return;
        };
        let binding_ty = self.subst_ty(ty);
        self.push_bind_statement(*binding_id, name.clone(), arm.body.site, binding_ty.clone());
        self.record_match_arm_binding_scope(*binding_id, arm);
        let dest = self.alloc_local(binding_ty.clone());
        let retained_guard_binder =
            arm.guard.is_some() && matches!(binding_ty, ResolvedTy::String | ResolvedTy::Bytes);
        if retained_guard_binder {
            match binding_ty {
                ResolvedTy::String => self.push_instr(Instr::StringRetain {
                    value: scrutinee_local,
                    condition: crate::model::StringRetainCondition::Always,
                }),
                ResolvedTy::Bytes => {
                    self.push_instr(Instr::BytesRetain {
                        value: scrutinee_local,
                    });
                }
                _ => {}
            }
        }
        self.push_instr(Instr::Move {
            dest,
            src: scrutinee_local,
        });
        if retained_guard_binder {
            self.restore_owner_after_retained_share(scrutinee_local, dest);
        }
        self.binding_locals.insert(*binding_id, dest);
    }
}

#[cfg(test)]
mod builtin_carrier_tests {
    use super::*;
    use crate::model::{OwnerId, OwnershipEvent};

    #[test]
    fn dominating_yield_handoff_selects_the_terminal_owner() {
        let builder = Builder {
            current_block_id: 1,
            pending_blocks: vec![crate::model::BasicBlock {
                id: 0,
                statements: vec![],
                instructions: vec![Instr::Move {
                    dest: Place::Local(1),
                    src: Place::Local(0),
                }],
                terminator: Terminator::Goto { target: 1 },
            }],
            ..Builder::default()
        };

        assert_eq!(
            builder.generator_yield_linear_handoff_owner(0, 0, 0),
            Some(Place::Local(1)),
            "a Move on every path to body end transfers the shared cleanup place"
        );
    }

    #[test]
    fn branch_local_yield_handoff_does_not_select_a_shared_cleanup_place() {
        let builder = Builder {
            current_block_id: 3,
            pending_blocks: vec![
                crate::model::BasicBlock {
                    id: 0,
                    statements: vec![],
                    instructions: vec![],
                    terminator: Terminator::Branch {
                        cond: Place::Local(2),
                        then_target: 1,
                        else_target: 2,
                    },
                },
                crate::model::BasicBlock {
                    id: 1,
                    statements: vec![],
                    instructions: vec![Instr::Move {
                        dest: Place::Local(1),
                        src: Place::Local(0),
                    }],
                    terminator: Terminator::Goto { target: 3 },
                },
                crate::model::BasicBlock {
                    id: 2,
                    statements: vec![],
                    instructions: vec![],
                    terminator: Terminator::Goto { target: 3 },
                },
            ],
            ..Builder::default()
        };

        assert_eq!(
            builder.generator_yield_linear_handoff_owner(0, 0, 0),
            None,
            "a branch-only Move must leave shared cleanup authority unresolved"
        );
    }

    #[test]
    fn exact_whole_binding_handoff_requires_both_named_generations() {
        let source = BindingId(10);
        let destination = BindingId(12);
        assert!(exact_whole_binding_owner_handoff(
            Some(crate::model::OwnerId {
                binding: source,
                generation: 3,
            }),
            source,
            Some(crate::model::OwnerId {
                binding: destination,
                generation: 0,
            }),
            destination,
        ));
    }

    #[test]
    fn ambiguous_or_wrong_whole_binding_handoff_stays_unadmitted() {
        let source = BindingId(10);
        let destination = BindingId(12);
        let destination_owner = Some(crate::model::OwnerId {
            binding: destination,
            generation: 0,
        });
        assert!(!exact_whole_binding_owner_handoff(
            None,
            source,
            destination_owner,
            destination,
        ));
        assert!(!exact_whole_binding_owner_handoff(
            Some(crate::model::OwnerId {
                binding: BindingId(11),
                generation: 0,
            }),
            source,
            destination_owner,
            destination,
        ));
    }

    #[test]
    fn option_carrier_uses_builtin_identity_not_presentation() {
        let renamed = ResolvedTy::named_builtin(
            "presentation.RenamedOption",
            BuiltinType::Option,
            vec![ResolvedTy::String],
        );
        assert!(is_builtin_option_carrier(&renamed));

        let shadow = ResolvedTy::named_user("Option", vec![ResolvedTy::String]);
        assert!(!is_builtin_option_carrier(&shadow));
    }

    fn carrier_scope_exit(
        scope: ScopeId,
        carried: Vec<OwnerId>,
        carry_places: Vec<Place>,
    ) -> Instr {
        Instr::OwnershipEvent(OwnershipEvent::ScopeExit {
            scopes: vec![scope],
            owners: Vec::new(),
            carry_places,
            carried,
        })
    }

    fn carrier_scan_builder(binding: BindingId, scope: ScopeId) -> Builder {
        let mut builder = Builder::default();
        builder.call_scrutinee_carrier_mint_locals.insert(0);
        builder.binding_locals.insert(binding, Place::Local(0));
        builder.binding_scope.insert(binding, scope);
        builder.owner_generations.insert(binding, 0);
        builder
    }

    #[test]
    fn exact_noncarrying_scope_exit_bounds_call_carrier_scan() {
        let binding = BindingId(90);
        let scope = ScopeId(12);
        let mut builder = carrier_scan_builder(binding, scope);
        let owner = OwnerId {
            binding,
            generation: 0,
        };
        builder.instructions = vec![
            Instr::OwnershipEvent(OwnershipEvent::Release {
                owner,
                place: Place::Local(0),
            }),
            carrier_scope_exit(scope, Vec::new(), Vec::new()),
            // This write belongs to a later physical-slot lifetime. The
            // current generation's scan must stop at the lexical close.
            Instr::Move {
                dest: Place::Local(1),
                src: Place::Local(0),
            },
        ];

        assert!(builder.call_carrier_body_end_drop_safe(binding, 0, 0, 0));
    }

    #[test]
    fn carried_or_co_live_carrier_scope_does_not_hide_a_stale_use() {
        let binding = BindingId(91);
        let scope = ScopeId(13);
        let owner = OwnerId {
            binding,
            generation: 0,
        };
        let stale_use = Instr::Move {
            dest: Place::Local(1),
            src: Place::Local(0),
        };

        let mut carried = carrier_scan_builder(binding, scope);
        carried.instructions = vec![
            Instr::OwnershipEvent(OwnershipEvent::Release {
                owner,
                place: Place::Local(0),
            }),
            carrier_scope_exit(scope, vec![owner], Vec::new()),
            stale_use.clone(),
        ];
        assert!(
            !carried.call_carrier_body_end_drop_safe(binding, 0, 0, 0),
            "a carried OwnerId is not ended by the lexical marker"
        );

        let mut co_live = carrier_scan_builder(binding, scope);
        co_live
            .binding_locals
            .insert(BindingId(92), Place::Local(0));
        co_live.binding_scope.insert(BindingId(92), scope);
        co_live.instructions = vec![carrier_scope_exit(scope, Vec::new(), Vec::new()), stale_use];
        assert!(
            !co_live.call_carrier_body_end_drop_safe(binding, 0, 0, 0),
            "ambiguous same-place bindings must retain the fail-closed unbounded scan"
        );
    }
}
