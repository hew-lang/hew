use super::{
    base_local, callee_is_resolved_item, callee_returns_fresh_owner,
    field_override_uses_record_field_drop, float_width, generator_yield_instr_escapes,
    generator_yield_terminator_escapes, hir_expr_contains_synthetic_vec_index,
    hir_expr_contains_synthetic_vec_string_index, literal_match_scrutinee_ty, mangle_layout_key,
    place_is_interior_projection, short_name, ty_is_generator_handle, ty_is_indirect_enum,
    user_record_layout_key, BindingId, Builder, CmpPred, Disposition, FailClosedReason,
    FieldOffset, FloatWidth, HashMap, HashSet, HirExpr, HirExprKind, HirLiteral, Instr, IntentKind,
    MirDiagnostic, MirDiagnosticKind, MirStatement, Place, ProjectedPayloadOrigin,
    ProjectedPayloadProvenance, ProjectedPayloadRejectReason, ProjectedScrutinee,
    ReleaseSymbolVerdict, ResolvedRef, ResolvedTy, SiteId, Terminator, TrapKind, ValueClass,
    VecElementRelease,
};

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
        scrutinee: &HirExpr,
        arms: &[hew_hir::HirMatchArm],
        result_ty: &ResolvedTy,
    ) -> Option<Place> {
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
        // (`Some(v) => panic(...), None => {}`); this keeps that form compiling.
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
            self.lower_match_enum_tag(scrutinee, arms, result_ty)
        }
    }

    fn is_project_match_scrutinee_ty(&self, ty: &ResolvedTy) -> bool {
        match ty {
            ResolvedTy::Tuple(_) => true,
            _ => user_record_layout_key(&self.subst_ty(ty))
                .is_some_and(|key| self.lookup_record_field_order(&key).is_some()),
        }
    }

    pub(crate) fn is_vec_string_iter_next_scrutinee(&self, scrutinee: &HirExpr) -> bool {
        matches!(
            &self.subst_ty(&scrutinee.ty),
            ResolvedTy::Named {
                name,
                args,
                builtin: None,
                ..
            } if name == "Option" && matches!(args.as_slice(), [ResolvedTy::String])
        ) && hir_expr_contains_synthetic_vec_string_index(scrutinee)
    }

    /// #2523 provenance-skip predicate: true when the match scrutinee is the
    /// `for x in <vec>` desugar's synthetic `Option<T>` next-producer for ANY
    /// element type (`Vec<string>`, `Vec<(string, string)>`, `Vec<Person>`, …).
    /// Each `Some(x)` payload is a FRESH, solely-owned per-frame element the
    /// iteration handed the body — never a projection of a re-readable
    /// aggregate that retains the bits — so its move-out (`let (k, val) = pair`,
    /// `return pair`) is a legitimate ownership transfer that must NOT route
    /// through the default-deny consume hook. The narrower
    /// `is_vec_string_iter_next_scrutinee` still drives the `string`-specific
    /// per-iteration release disposition; this one only gates the provenance
    /// skip so no element type is falsely rejected as a re-readable-place move.
    fn is_vec_iter_next_scrutinee(&self, scrutinee: &HirExpr) -> bool {
        matches!(
            &self.subst_ty(&scrutinee.ty),
            ResolvedTy::Named {
                name,
                args,
                builtin: None,
                ..
            } if name == "Option" && args.len() == 1
        ) && hir_expr_contains_synthetic_vec_index(scrutinee)
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
    /// synthetic `match channel.recv(__hew_for_iter_X) { Some(item) => body,
    /// None => break }` and source-level `match await rx.recv() { ... }` /
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

    /// Emit the per-iteration release for a `for line in vec_of_strings` binding.
    ///
    /// `hew_vec_get_str` returns a FRESH, solely-owned retained owner of the
    /// element (`hew_string_clone` — a header-aware refcount bump that returns
    /// the caller its OWN reference; NOT a borrow of the Vec's live buffer slot,
    /// which is what the owned-element getter `hew_vec_get_owned` does). The Vec
    /// keeps its own reference and releases every element through its `destroy`
    /// descriptor independently. So the iteration binding owns exactly one
    /// reference that must be released with `hew_string_drop` on EVERY path out of
    /// the body — exactly the ownership shape of a generator-yielded `string`.
    ///
    /// The drop is placed per-path, not at a single post-body point:
    ///   - the fall-through (loop back-edge) path gets the body-end `Drop` emitted
    ///     here (this instruction lands at the end of the body's current block);
    ///   - `break`/`continue` edges free the current iteration's binding via
    ///     `emit_generator_yield_value_drops_for_break_continue` (the binding is
    ///     registered on `active_generator_yield_values` before the body lowers),
    ///     so an iteration that breaks/continues releases before jumping past the
    ///     body-end drop;
    ///   - an early `return` inside the body moves the element to the caller (an
    ///     ownership escape — see below), so no extra drop is owed on that path.
    ///
    /// Every structurally-reachable second free is a no-op: the inline drop
    /// null-stores the slot (codegen `emit_cow_heap_drop`) and the runtime
    /// `hew_string_drop` header-guards (`raii-null-after-move`).
    ///
    /// Ownership-escape handling: a body that genuinely transfers the binding's
    /// single retained reference out — a `Move`/`WitnessMove` into a surviving
    /// local, a store into a record/tuple/closure-env/actor-state aggregate, a
    /// spawn capture, or a consuming terminator (`return`/re-yield/send/ask) —
    /// hands ownership to a longer-lived owner that will release it. On those
    /// paths the body-end drop is SUPPRESSED (leak-not-double-free; the move
    /// checker / function-scope drop machinery owns the escaped reference). A
    /// borrowing read — string concat (`out + line`), `line.len()`,
    /// `print(line)`, any runtime-ABI/arithmetic operand — does NOT transfer the
    /// reference, so the per-iteration drop is still owed and is emitted. This is
    /// the SAME escape classification the generator-yield path uses
    /// (`generator_yield_binding_drop_safe`), reused here because the ownership
    /// shapes are identical.
    fn emit_vec_string_iter_binding_drop(
        &mut self,
        binding: BindingId,
        place: Place,
        ty: &ResolvedTy,
        body_start_block_id: u32,
        body_start_instr_len: usize,
        site: hew_hir::SiteId,
    ) {
        if !matches!(ty, ResolvedTy::String) {
            return;
        }
        let Some(local) = base_local(place) else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "Vec<String> for-in retained binding drop".to_string(),
                    site,
                },
                note: format!(
                    "for-in binding {binding:?} must lower to a Place::Local-backed string \
                     owner so the retained hew_vec_get_str result can be balanced with \
                     hew_string_drop; got {place:?}"
                ),
            });
            return;
        };
        // Per-path escape scan: emit the body-end drop unless the binding's single
        // retained reference escapes the body on every reachable path. Borrowing
        // reads (concat, getters, print) are non-escaping; only an
        // ownership-transferring use suppresses the drop. Shared with the
        // generator-yield path — identical fresh-solely-owned-reference shape.
        if self.generator_yield_binding_drop_safe(body_start_block_id, body_start_instr_len, local)
        {
            self.push_instr(Instr::Drop {
                place,
                ty: ty.clone(),
                drop_fn: Some(crate::model::DropFnSpec::Release("hew_string_drop")),
            });
        }
        // else: the retained element escapes the body (moved/stored/returned).
        // Leak-not-double-free — the consumer that received the reference owns it;
        // emitting another `hew_string_drop` here would over-release. No
        // diagnostic: an escaping element is a legitimate program shape, and the
        // function-scope drop machinery / move checker releases the escaped
        // reference where it lands.
    }

    /// The classified release verdict for a generator-yielded (or
    /// channel-received) `Some(x)` payload of type `ty`:
    /// [`ReleaseSymbolVerdict::Wired`] carries the C-ABI symbol the
    /// consumer-body drop emits, restricted to the proven leak shapes — a
    /// heap-owning `string`, `bytes`, and any builtin `Vec<T>` whose element
    /// release is wired. [`ReleaseSymbolVerdict::WiredInPlace`] covers a
    /// registered heap-owning record/enum composite (the `LayoutManaged`
    /// stream-element shapes): the release is the synthesised
    /// `__hew_record_drop_inplace_<R>` / `__hew_enum_drop_inplace_<E>` thunk,
    /// admitted by the same `elem_is_owned_abi_releasable` authority the
    /// layout-witness (send-side deep clone) mirrors, so the drop is wired
    /// exactly where the witness already clones. A `BitCopy` record/enum owns
    /// no heap and never earns it. [`ReleaseSymbolVerdict::NoDropPath`]
    /// covers shapes with no validated consumer-drop path (HashMap/HashSet
    /// yields — they leak as before rather than risk a double-free, matching
    /// the conservative posture of the function-scope `CoW` drop allow-set).
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
        let key = if args.is_empty() {
            name.clone()
        } else {
            mangle_layout_key(short_name(name), args)
        };
        let is_enum = self
            .enum_layouts
            .iter()
            .any(|el| el.name == key || short_name(&el.name) == short_name(name));
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
    ///   - `UnenumeratedShape` — the element owns NO heap as a flat element
    ///     (a free `TypeParam` in a generic skeleton, `Unit`, a bare runtime
    ///     view) — the buffer-only free IS the complete release; refusing
    ///     would reject un-monomorphised generic `Vec<T>` bodies that
    ///     instantiate to plain elements.
    fn vec_release_symbol_verdict(&self, elem: &ResolvedTy) -> ReleaseSymbolVerdict {
        #[allow(
            clippy::match_same_arms,
            reason = "the repeated symbols are projections of distinct typed release \
                      decisions; keeping the arms separate makes the owned, plain, \
                      fail-closed, and unenumerated boundaries reviewable"
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
            VecElementRelease::Unsupported(_) => ReleaseSymbolVerdict::Wired("hew_vec_free"),
        }
    }

    /// Emit a body-end release for a generator-yielded `Some(x)` binding. The
    /// yielded value is a fresh, solely-owned heap value the coro `.next()`
    /// drive handed the consumer; releasing it here (per-iteration for a
    /// `for`-in loop) is what frees the otherwise-leaked yield. Gated on the
    /// same body-shape drop-safety scan the `Vec<String>` iterator path uses:
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
                    "generator-yielded binding {binding:?} must lower to a Place::Local-backed \
                     owner so the yielded heap value can be balanced with {drop_fn:?}; got \
                     {place:?}"
                ),
            });
            return;
        };
        if self.generator_yield_binding_drop_safe(body_start_block_id, body_start_instr_len, local)
        {
            self.push_instr(Instr::Drop {
                place,
                ty: ty.clone(),
                drop_fn: Some(drop_fn),
            });
        }
        // else: the value escapes the consuming body — leak-not-double-free.
        // No diagnostic: an escaping yield is a legitimate (if leaky) program,
        // not a lowering defect, unlike the Vec<String> getter which has no
        // other release path.
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
        let mut visiting = HashSet::new();
        let mut memo = HashMap::new();
        self.generator_yield_block_paths_drop_safe(
            start_block_id,
            start_block_id,
            start_instr_len,
            local,
            &mut visiting,
            &mut memo,
        )
    }

    #[allow(
        clippy::too_many_lines,
        reason = "exhaustive Terminator match — adding new variants \
                  (most recently `MakeLambdaActor`) edges past the 100-line \
                  ceiling without changing the function's structural \
                  responsibility (single yield-block walk)"
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
        visiting: &mut HashSet<u32>,
        memo: &mut HashMap<u32, bool>,
    ) -> bool {
        if let Some(ok) = memo.get(&block_id) {
            return *ok;
        }
        if block_id == self.current_block_id {
            let start = if block_id == start_block_id {
                start_instr_len
            } else {
                0
            };
            return self.instructions[start..]
                .iter()
                .all(|instr| !generator_yield_instr_escapes(instr, local));
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
            // An escape in this block's instructions OR its terminator makes the
            // body-end drop unsound (the value left the body) — return false
            // immediately. Otherwise recurse into the successor(s).
            let escapes_here = block.instructions[start..]
                .iter()
                .any(|instr| generator_yield_instr_escapes(instr, local))
                || generator_yield_terminator_escapes(
                    &block.terminator,
                    self.suspend_kinds.get(&block.id),
                    local,
                );
            if escapes_here {
                false
            } else {
                match &block.terminator {
                    Terminator::Goto { target } => self.generator_yield_block_paths_drop_safe(
                        *target,
                        start_block_id,
                        start_instr_len,
                        local,
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
                            visiting,
                            memo,
                        ) && self.generator_yield_block_paths_drop_safe(
                            *else_target,
                            start_block_id,
                            start_instr_len,
                            local,
                            visiting,
                            memo,
                        )
                    }
                    Terminator::Trap { .. } => true,
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
                // authorities are documented to agree —
                // `dedup-semantic-boundary`, `is_known_cow_heap_drop_symbol`
                // / the RecordFieldDrop congruence assert reject a
                // mis-picked symbol fail-closed). The classification runs on
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
                let verdict = if let Some(field_tys) = layouts.record_field_tys(name, args) {
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
            // `AsyncGenerator`).
            ResolvedTy::String
            | ResolvedTy::Bytes
            | ResolvedTy::Named {
                builtin:
                    Some(
                        hew_types::BuiltinType::HashMap
                        | hew_types::BuiltinType::HashSet
                        | hew_types::BuiltinType::Generator
                        | hew_types::BuiltinType::AsyncGenerator,
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

    /// Field-precise owned-field enumeration for a record/tuple match
    /// destructure scrutinee. For each owned field (non-BitCopy by value-class)
    /// returns `(field_idx, substituted_type)`. Used by `lower_match_project`
    /// to compute the set of fields needing explicit-drop emission for the
    /// partial-extraction case.
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
                if ValueClass::of_ty(&substituted, &self.type_classes) == ValueClass::BitCopy {
                    None
                } else {
                    u32::try_from(idx).ok().map(|i| (i, substituted))
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
    /// summary-proven fresh (`funcupdate_fn_returns_fresh`) AND some argument
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
            HirExprKind::Call { callee, args } => {
                !callee_is_resolved_item(callee)
                    || (!callee_returns_fresh_owner(callee, &self.funcupdate_fn_returns_fresh)
                        && args
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
    pub(crate) fn emit_local_overwrite_release(&mut self, dest: Place, target_ty: &ResolvedTy) {
        let ty = self.subst_ty(target_ty);
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
            return;
        }
        // User record: release every owned field in declaration order, the same
        // per-field route the functional-update override-drop takes. Skip the
        // whole release unless EVERY owned field has a known leaf symbol -- a
        // nested record/enum field has none, and a partial free would leak the
        // rest while risking a wrong-ABI release.
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
                }
            }
        }
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
            if let hew_hir::HirMatchArmPredicate::Binding {
                binding_id,
                name,
                ty,
            } = &arm.predicate
            {
                let binding_ty = self.subst_ty(ty);
                self.statements.push(MirStatement::Bind {
                    binding: *binding_id,
                    name: name.clone(),
                    site: arm.body.site,
                    ty: binding_ty.clone(),
                });
                self.record_binding_scope(*binding_id);
                let dest = self.alloc_local(binding_ty);
                self.push_instr(Instr::Move {
                    dest,
                    src: Place::Local(scrutinee_local),
                });
                self.binding_locals.insert(*binding_id, dest);
            }

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
            let value = self.lower_value(&arm.body);
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
        reason = "record/tuple project lowering keeps binding setup, field loads, and arm body CFG in one fail-closed block"
    )]
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

        // Guard gate. A record/tuple project pattern is irrefutable, so the
        // first arm is taken unconditionally and this function lowers exactly
        // that one body — it does not build an ordered arm chain. An arm guard
        // (`Pattern if <cond>`) would therefore be silently dropped: a `false`
        // guard has no fallthrough target here, so the guarded arm would still
        // run (wrong result) and its destructure would consume the scrutinee
        // out from under the arm the user intended. Honoring guards needs the
        // deferred-bind / fallthrough lowering that `lower_match_binding_chain`
        // and `lower_match_enum_tag` provide for their irrefutable-or-tagged
        // arms; that is not wired for owned-field projection, where a failed
        // guard would have to roll back partial moves. Reject fail-closed for
        // every project match (BitCopy and non-BitCopy alike) rather than
        // miscompile. Unguarded arms are unaffected: with no guard the single
        // taken arm is exactly correct and any trailing arms are unreachable.
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

        let result_place = self.alloc_local(result_ty.clone());
        let scrutinee_place = self.lower_value(scrutinee)?;
        let scrutinee_local = match scrutinee_place {
            Place::Local(local) => local,
            other => {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "project match scrutinee place shape".to_string(),
                        site: scrutinee.site,
                    },
                    note: format!(
                        "record/tuple match destructure requires a local scrutinee; got {other:?}"
                    ),
                });
                return None;
            }
        };

        let scrutinee_is_non_bitcopy = !self.project_match_scrutinee_is_bitcopy(&scrutinee.ty);

        // Scrutinee-shape gate. A non-BitCopy record/tuple destructure
        // either moves owned fields out of the scrutinee storage (with bindings)
        // or discards the whole aggregate (all-wildcard). The only scrutinee
        // shape we can lower leak- AND use-after-free-free is a non-captured
        // `BindingRef`: it carries a composite drop (so an all-wildcard discard
        // frees every owned field) and the dataflow checker can mark it
        // `Consumed` (so a post-match read of a moved-out binding fires
        // `UseAfterConsume`). This gate runs for EVERY non-BitCopy match-project
        // — the earlier `!selected.bindings.is_empty()` guard let an all-wildcard
        // owned aggregate bypass the check, and a temporary all-wildcard
        // scrutinee (no composite drop) then leaked every owned field. Every
        // other shape — projections, captures, and temporaries — fails closed.
        // See `match_project_scrutinee_reject`.
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

        // Skipped-field drop pre-flight (partial extraction only). When at
        // least one owned field is bound, `derive_owned_record_drop_allowed` /
        // `derive_tuple_composite_drop_allowed` suppress the scrutinee's
        // composite drop (the field-binder release-owner rule and the direct
        // `FieldDropInPlace` exclusion rule), so every UNSELECTED owned field
        // must be discharged by the safety-drop loop below. Admission is the
        // OR of the two discharge paths that loop can emit:
        //   - a single-`ptr` leaf release symbol
        //     (`project_field_inline_drop_symbol` — the load+`Instr::Drop`
        //     path), or
        //   - the field-addressed in-place drop
        //     (`field_drop_in_place_admissible` — the `FieldDropInPlace`
        //     path for record / tuple / fixed-array / inline-enum /
        //     indirect-enum fields over registered layouts).
        // Anything neither path can place — slices, `dyn Trait` fat fields,
        // closures, affine handles (`Channel` / `Task` /
        // `CancellationToken`), unregistered layouts — keeps this fail-closed
        // refusal. Determine the unselected-owned set BEFORE emitting any code
        // so the diagnostic surfaces cleanly without a half-lowered block.
        // All-wildcard patterns skip this loop: with no binding to suppress
        // it, the scrutinee's own composite drop frees every field (the gate
        // above proved the scrutinee is a droppable `BindingRef`).
        if !selected.bindings.is_empty() && scrutinee_is_non_bitcopy {
            let extracted: HashSet<u32> = selected.bindings.iter().map(|b| b.field_idx).collect();
            let owned_fields: Vec<(u32, ResolvedTy)> = match &selected.predicate {
                hew_hir::HirMatchArmPredicate::RecordProject { .. } => {
                    self.project_record_owned_field_list(&scrutinee.ty)
                }
                hew_hir::HirMatchArmPredicate::TupleProject { .. } => {
                    self.project_tuple_owned_field_list(&scrutinee.ty)
                }
                _ => Vec::new(),
            };
            for (idx, field_ty) in &owned_fields {
                if extracted.contains(idx) {
                    continue;
                }
                // Admission requires a Wired leaf verdict or classifier
                // admission. An Unwired `Vec` field (element release
                // unwired) is refused here: it carries no emittable symbol
                // and the in-place classifier does not admit leaf `Vec`s.
                if !matches!(
                    self.project_field_inline_drop_symbol(field_ty),
                    ReleaseSymbolVerdict::Wired(_)
                ) && !self.field_drop_in_place_admissible(field_ty)
                {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::NotYetImplemented {
                            construct: "match-destructure wildcard on owned aggregate field"
                                .to_string(),
                            site: scrutinee.site,
                        },
                        note: format!(
                            "field {idx} of `{}` has type `{}`, which neither the inline leaf \
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
            }
        }

        let join_bb = self.alloc_block();
        let body_bb = self.alloc_block();
        self.finish_current_block(Terminator::Goto { target: body_bb });
        self.start_block(body_bb);

        // Decide once whether the scrutinee gets the follow-up `Use {
        // intent: Consume }` mark. This drives BOTH the post-destructure
        // consume emission AND each non-BitCopy binder's taint-suppression.
        // Hoisted out of the per-binding loop so the two paths cannot diverge
        // — a binder admitted to the sole-owner allow-set without a
        // corresponding scrutinee consume would double-free the shared buffer.
        //
        // Projections, captures, AND temporaries were already rejected by the
        // `match_project_scrutinee_reject` gate above, so the only scrutinee
        // that reaches here for a non-BitCopy match is a non-captured
        // `BindingRef`. It earns the consume mark + the binder untaint when the
        // arm binds at least one owned field; an all-wildcard arm moves nothing
        // out, so the binding stays live and its composite drop frees every
        // field (no consume mark — a post-match read of an all-wildcard
        // scrutinee is legitimate).
        let consume_scrutinee: Option<(BindingId, String)> = match &scrutinee.kind {
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(id),
                name,
            } if !selected.bindings.is_empty()
                && !self.project_match_scrutinee_is_bitcopy(&scrutinee.ty)
                && !self.capture_env_sources.contains_key(id) =>
            {
                Some((*id, name.clone()))
            }
            _ => None,
        };

        // Classify the scrutinee's storage once, above the bind loop, so both
        // the bound-string discharge inside it and the skipped-field discharge
        // below read the same verdict. `local_storage_is_interior_alias` is a
        // pure classifier over the already-emitted loads that define
        // `scrutinee_local` (an interior alias is minted by a `let inner =
        // outer.field` byte copy BEFORE this match lowers); the bind loop only
        // adds loads whose dest is a binder, never `scrutinee_local`, so the
        // verdict is invariant to where it is read.
        let scrutinee_is_interior_alias =
            scrutinee_is_non_bitcopy && self.local_storage_is_interior_alias(scrutinee_local);

        let mut overwritten_bindings = Vec::with_capacity(selected.bindings.len());
        for binding in &selected.bindings {
            let binding_ty = self.subst_ty(&binding.ty);
            self.statements.push(MirStatement::Bind {
                binding: binding.binding,
                name: binding.name.clone(),
                site: selected.body.site,
                ty: binding_ty.clone(),
            });
            self.record_binding_scope(binding.binding);
            // Non-BitCopy bindings (owned `string` / `bytes` / `Vec<T>` /
            // owned record/tuple/enum) MUST enter `owned_locals` so the
            // function-scope LIFO drop pass releases them exactly once, AND so
            // their presence in `release_owner_bases` excludes the source
            // aggregate's composite drop in `derive_owned_record_drop_allowed`
            // / `derive_tuple_composite_drop_allowed` (the field-binder rule).
            // Skipping this for an owned binding produces a leak (no drop
            // elaborated) OR a double-free (composite drop still fires plus
            // the binding's escape consume) — both fail-closed-unsafe.
            // BitCopy bindings stay out: their copy is free of heap ownership
            // and the surrounding scrutinee's composite drop covers them.
            let keep_for_drop_elab = self.binding_seeds_drop_elaboration(&binding_ty);
            let binding_is_string = matches!(binding_ty, ResolvedTy::String);
            if keep_for_drop_elab {
                self.register_owned_local(
                    binding.binding,
                    binding.name.clone(),
                    binding_ty.clone(),
                );
                // A generator/AsyncGenerator handle bound via match destructure
                // gets the same per-scope-exit `hew_gen_coro_destroy` registration the
                // `Let` path uses, so the per-iteration release fires for a
                // match inside a loop (see `scope_generator_bindings`).
                if ty_is_generator_handle(&binding_ty) {
                    if let Some(scope) = self.active_scopes.last().copied() {
                        self.scope_generator_bindings.push((
                            scope,
                            binding.binding,
                            binding_ty.clone(),
                        ));
                    }
                }
                // NB: unlike generators, `Stream<T>` / `Receiver<T>` handles are
                // NOT registered for a per-scope-exit close when bound via match
                // destructure. The deadlock fixed here is caused by the
                // `for await` desugar's synthetic `__hew_for_iter_*` CURSOR
                // (always a `Let` binding) staying open; a plain source binding
                // (`let (a, s) = …`) that is later consumed into that cursor
                // must NOT get its own scope-exit close, or it double-closes the
                // handle the cursor owns (the source binding's slot is not
                // null-stored on the consume in every destructure shape). The
                // `Let`-path registration + the function-exit LIFO (which
                // respects the consume) cover every real case; there is no valid
                // shape where a for-await cursor is bound via match destructure.
                // Mirrors the vec-iter cursor registration, which is likewise
                // `Let`-path-only.
            }
            let dest = self.alloc_local(binding_ty);
            // Pair the binder with the scrutinee's consume mark: register the
            // binder's base local so `derive_cow_sole_owner` skips the
            // projection-alias taint seed for it. Once the parent is
            // `Consumed` at this site, the binder owns its loaded payload
            // exclusively and must be admitted to the leaf CoW drop allow-set,
            // otherwise `build_lifo_drops`'s leaf-CoW arm silently emits no
            // drop and the payload leaks. The dataflow exit-state post-filter
            // at the `derive_cow_sole_owner` call site still removes any
            // binder consumed by the arm body (`=> y`), so this never
            // double-frees a moved-out payload.
            //
            // Gated: only when the scrutinee earns a consume mark (a non-
            // captured `BindingRef` non-BitCopy scrutinee with bindings)
            // AND the binder itself is non-BitCopy. BitCopy binders carry
            // no heap to drop; non-consume-marked scrutinees keep the
            // parent's composite drop alive, so binder taint must stand.
            if consume_scrutinee.is_some() && keep_for_drop_elab {
                if let Some(local_idx) = base_local(dest) {
                    self.match_project_consumed_binder_locals.insert(local_idx);
                }
            }
            match &selected.predicate {
                hew_hir::HirMatchArmPredicate::RecordProject { .. } => {
                    self.push_instr(Instr::RecordFieldLoad {
                        record: Place::Local(scrutinee_local),
                        field_offset: FieldOffset(binding.field_idx),
                        dest,
                    });
                }
                hew_hir::HirMatchArmPredicate::TupleProject { arity } => {
                    if binding.field_idx >= *arity {
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct: "tuple project binding index out of range".to_string(),
                                site: selected.body.site,
                            },
                            note: format!(
                                "binding `{}` projects field {} from arity {} tuple; \
                                 the checker/HIR verifier should have rejected this",
                                binding.name, binding.field_idx, arity
                            ),
                        });
                        return None;
                    }
                    self.push_instr(Instr::TupleFieldLoad {
                        tuple: Place::Local(scrutinee_local),
                        field_index: binding.field_idx,
                        dest,
                    });
                }
                hew_hir::HirMatchArmPredicate::Wildcard
                | hew_hir::HirMatchArmPredicate::Binding { .. } => {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::NotYetImplemented {
                            construct: "wildcard/binding match arm with project bindings"
                                .to_string(),
                            site: selected.body.site,
                        },
                        note: "wildcard/binding match arms must not carry project bindings; \
                               this is a checker/HIR bug"
                            .to_string(),
                    });
                    return None;
                }
                hew_hir::HirMatchArmPredicate::EnumVariant { .. }
                | hew_hir::HirMatchArmPredicate::Literal { .. }
                | hew_hir::HirMatchArmPredicate::Regex { .. } => {
                    panic!("checker invariant violated: refutable arm in project match lowering");
                }
            }
            // Bound-string original discharge. A `string`-typed field bound on
            // a consumed, non-alias root strands its original. Codegen retains
            // the string on the field load (`retain_string_field_load` →
            // `hew_string_clone`, rc 2): the binder owns the clone, but the
            // ORIGINAL handle still sits in the root slot with a `+1` nothing
            // releases — the root's composite drop is suppressed (the binder
            // seeds `release_owner_bases`) and the consume mark below retracts
            // the root from `owned_locals`. Discharge the original IN PLACE
            // right after the load: `FieldDropInPlace` raw-loads the root's
            // field handle, releases it (rc 1), and null-stores the slot; the
            // binder is then the sole owner — safe even when the arm returns
            // the binder (its own drop balances the retained clone).
            //
            // Gated exactly like the skipped-field discharge below:
            //   - consume-marked scrutinee: a non-consumed root keeps its
            //     composite drop, which frees the original — discharging here
            //     would double-free.
            //   - non-alias root: on an interior alias the original belongs to
            //     the OUTER composite, which frees it; `FieldDropInPlace`
            //     null-stores the alias slot, not the owner's (the alias gate
            //     below is the authority) — discharging would double-free.
            //   - `string`-typed field only: the retaining-load class. Non-
            //     string bound fields (`Vec` / `bytes` / handles / aggregates)
            //     load without a retain, so the binder takes the one handle and
            //     the root slot is dead — they do not strand and MUST NOT be
            //     discharged here.
            if consume_scrutinee.is_some() && !scrutinee_is_interior_alias && binding_is_string {
                let field = match &selected.predicate {
                    hew_hir::HirMatchArmPredicate::RecordProject { .. } => {
                        crate::model::FieldAddr::Record(FieldOffset(binding.field_idx))
                    }
                    hew_hir::HirMatchArmPredicate::TupleProject { .. } => {
                        crate::model::FieldAddr::Tuple(binding.field_idx)
                    }
                    _ => unreachable!("bound-string discharge only reached for project predicates"),
                };
                self.push_instr(Instr::FieldDropInPlace {
                    base: Place::Local(scrutinee_local),
                    field,
                    ty: ResolvedTy::String,
                });
            }
            let previous = self.binding_locals.insert(binding.binding, dest);
            overwritten_bindings.push((binding.binding, previous, keep_for_drop_elab));
        }

        // Partial-extraction safety drops. After the per-binding extraction
        // loop, every owned field of the scrutinee that is NOT in
        // `selected.bindings` is a wildcard discard. If the scrutinee carries
        // any owned content, the composite drop on the source aggregate is
        // already suppressed by `derive_owned_record_drop_allowed` /
        // `derive_tuple_composite_drop_allowed` once an extracted binding, an
        // inline drop emitted here, or a `FieldDropInPlace` emitted here seeds
        // the exclusion. Without explicit drops for the wildcarded owned
        // fields, those fields would leak (the composite is suppressed, the
        // extracted bindings do not cover them). Two discharge paths, decided
        // per field:
        //
        //   - `string` fields and classifier-admitted aggregates (record /
        //     tuple / fixed array / inline enum / indirect enum —
        //     `field_drop_in_place_admissible`) emit `Instr::FieldDropInPlace`:
        //     the release runs IN PLACE at the field address, type-directed by
        //     codegen's `emit_heap_slot_drop` dispatcher. Strings MUST take
        //     this path even though a leaf release symbol exists: codegen's
        //     `RecordFieldLoad` / `TupleFieldLoad` retain string-typed dests
        //     via `hew_string_clone` (`retain_string_field_load`), so a
        //     load+`Drop` pair retain-cancels — the temp's release frees the
        //     clone while the ORIGINAL slot value leaks. The in-place drop
        //     raw-loads the original handle, releases it, and null-stores the
        //     pointer word.
        //   - Every other leaf with a release symbol
        //     (`project_field_inline_drop_symbol`: `bytes`, `Vec`, `HashMap`,
        //     `HashSet`, generator handles — loads that do NOT retain) keeps
        //     the load-into-temp + inline `Instr::Drop` path.
        //
        // The pre-flight above guarantees every unselected owned field is
        // dischargeable by one of the two paths — if any was not, we have
        // already returned a fail-closed diagnostic and never enter this loop.
        //
        // Emit drops AFTER the extraction binds but BEFORE the body: the body
        // refers only to the bound names (not the wildcarded fields), so the
        // drops free the discarded heap content immediately and leave the body
        // to run with the bound owners live. The straight-line pre-body
        // position is load-bearing for `FieldDropInPlace`: the op executes
        // exactly once per arm entry (no join or back-edge can re-run it),
        // which — together with the provers' direct exclusion of the base
        // root — is the whole idempotence story for inline-composite fields
        // (they have no null-store; see the `Instr::FieldDropInPlace` model
        // contract).
        //
        // Alias-scrutinee gate. When the scrutinee's storage is an interior
        // ALIAS of aggregate storage another binding still owns (`let inner =
        // outer.field; match inner { … }` — the field load byte-copies the
        // member with no retain; or an enum payload binder `match opt {
        // Some(row) => match row { … } }`), the originals the wildcarded
        // fields point at belong to the OUTER aggregate, whose composite drop
        // frees every one of them exactly once at scope exit (the destructure
        // consume retracts the alias binding from `owned_locals`, so nothing
        // seeds the outer root's exclusion). Discharging a skipped field
        // through the alias frees heap that composite walk re-frees:
        // `FieldDropInPlace` null-stores the ALIAS slot, never the owner's,
        // and the non-retaining leaf load+`Drop` path frees the original
        // outright — both double-free. An alias scrutinee therefore emits NO
        // skipped-field discharge; bound `string` binders keep their retained
        // `+1` with its own balancing drop. If the owner's composite is
        // separately excluded (an unrelated escape), the skipped originals
        // leak — fail-closed — and the provers' direct `FieldDropInPlace`
        // binder rules remain the net for any emitter that does discharge
        // through an alias.
        if !selected.bindings.is_empty() && scrutinee_is_non_bitcopy && !scrutinee_is_interior_alias
        {
            let extracted: HashSet<u32> = selected.bindings.iter().map(|b| b.field_idx).collect();
            let owned_fields: Vec<(u32, ResolvedTy)> = match &selected.predicate {
                hew_hir::HirMatchArmPredicate::RecordProject { .. } => {
                    self.project_record_owned_field_list(&scrutinee.ty)
                }
                hew_hir::HirMatchArmPredicate::TupleProject { .. } => {
                    self.project_tuple_owned_field_list(&scrutinee.ty)
                }
                _ => Vec::new(),
            };
            for (idx, field_ty) in owned_fields {
                if extracted.contains(&idx) {
                    continue;
                }
                let field_is_string = matches!(field_ty, ResolvedTy::String);
                if !field_is_string {
                    if let ReleaseSymbolVerdict::Wired(drop_symbol) =
                        self.project_field_inline_drop_symbol(&field_ty)
                    {
                        let temp = self.alloc_local(field_ty.clone());
                        match &selected.predicate {
                            hew_hir::HirMatchArmPredicate::RecordProject { .. } => {
                                self.push_instr(Instr::RecordFieldLoad {
                                    record: Place::Local(scrutinee_local),
                                    field_offset: FieldOffset(idx),
                                    dest: temp,
                                });
                            }
                            hew_hir::HirMatchArmPredicate::TupleProject { .. } => {
                                self.push_instr(Instr::TupleFieldLoad {
                                    tuple: Place::Local(scrutinee_local),
                                    field_index: idx,
                                    dest: temp,
                                });
                            }
                            _ => unreachable!(
                                "owned-field enumeration only populated for project predicates"
                            ),
                        }
                        self.push_instr(Instr::Drop {
                            place: temp,
                            ty: field_ty,
                            drop_fn: Some(crate::model::DropFnSpec::Release(drop_symbol)),
                        });
                        continue;
                    }
                }
                if field_is_string || self.field_drop_in_place_admissible(&field_ty) {
                    let field = match &selected.predicate {
                        hew_hir::HirMatchArmPredicate::RecordProject { .. } => {
                            crate::model::FieldAddr::Record(FieldOffset(idx))
                        }
                        hew_hir::HirMatchArmPredicate::TupleProject { .. } => {
                            crate::model::FieldAddr::Tuple(idx)
                        }
                        _ => unreachable!(
                            "owned-field enumeration only populated for project predicates"
                        ),
                    };
                    self.push_instr(Instr::FieldDropInPlace {
                        base: Place::Local(scrutinee_local),
                        field,
                        ty: field_ty,
                    });
                    continue;
                }
                // Unreachable — the pre-flight at the head of the function
                // returned fail-closed on the same admission OR. Defense-in-
                // depth: surface the invariant rather than silently leak.
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "match-destructure wildcard on owned aggregate field"
                            .to_string(),
                        site: scrutinee.site,
                    },
                    note: format!(
                        "field {idx} of `{}` slipped past the pre-flight skipped-field \
                         admission check; refusing to emit a leak / wrong-ABI free",
                        scrutinee.ty.user_facing(),
                    ),
                });
                return None;
            }
        }

        // The destructure consumed the scrutinee's storage: bindings own the
        // loaded fields and any inline drops above released wildcarded owned
        // fields IN PLACE. If the scrutinee was a `BindingRef`, any post-match
        // read of that binding (`p.b` after `match p { Pair { a: x, b: _ } =>
        // x }`, or even a later full-field read once the binders drop their
        // payloads) would be a use-after-free. Emit a follow-up
        // `MirStatement::Use { intent: Consume }` for the scrutinee binding so
        // the dataflow checker transitions it to `Consumed(site)` — any later
        // `BindingRef` use then fires `UseAfterConsume`.
        //
        // The `consume_scrutinee` decision was hoisted above the per-binding
        // loop so this emission and the per-binder taint-suppression (at
        // `match_project_consumed_binder_locals`) stay in lock-step: a binder
        // admitted to the leaf CoW sole-owner allow-set without a
        // corresponding parent consume would double-free.
        if let Some((scrutinee_id, scrutinee_name)) = consume_scrutinee.as_ref() {
            let scrutinee_ty = self.subst_ty(&scrutinee.ty);
            self.statements.push(MirStatement::Use {
                binding: *scrutinee_id,
                name: scrutinee_name.clone(),
                site: scrutinee.site,
                ty: scrutinee_ty,
                intent: IntentKind::Consume,
            });
            self.mark_binding_moved(*scrutinee_id);
        }

        let value = self.lower_value(&selected.body);

        // For owned (non-BitCopy) pattern bindings, KEEP the `binding_locals`
        // entry alive past the arm body so `build_lifo_drops` can resolve the
        // binding's Place at function-exit (and loop back-edge) drop
        // elaboration time. Removing it here would leak the bound owned field:
        // for the leaf
        // `string`/`Vec`/etc. arm in `build_lifo_drops`, an `owned_locals`
        // entry without a `binding_locals` Place silently emits no drop
        // (the `if let Some(place) = binding_locals.get(...)` arm short-
        // circuits). The `keep_for_drop_elab` flag was set above iff the
        // binding's type is non-BitCopy — exactly the bindings that need
        // their drop elaborated. Lexical liveness is still narrowed by the
        // dataflow exit-state filter inside `drops_for_exit` (a binding
        // `Consumed` mid-body is excluded), mirroring the discipline the
        // enum-variant arm uses at `lower_match_enum_tag` (~L11860).
        // BitCopy bindings have no drop to elaborate, so restoring their
        // previous slot is correct and shadowing-safe.
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
            self.push_instr(Instr::Move {
                dest: result_place,
                src,
            });
        }
        // The single irrefutable arm: if its body diverges the join is
        // unreachable (see `lower_match_enum_tag` for the rationale — #1907).
        let join_reachable = !self.cursor_unreachable;
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
    ///
    /// The first slice is intentionally limited to `BitCopy` aggregate
    /// scrutinees. Owned aggregate projects use the path-sensitive extraction
    /// and skipped-field drop protocol in `lower_match_project`; duplicating
    /// those moves across runtime-selected arms needs a separate ownership
    /// proof and remains fail-closed here.
    #[allow(
        clippy::too_many_lines,
        reason = "ordered project-predicate CFG keeps field comparisons, bindings, and fallthrough topology together"
    )]
    fn lower_match_project_predicate_chain(
        &mut self,
        scrutinee: &HirExpr,
        arms: &[hew_hir::HirMatchArm],
        result_ty: &ResolvedTy,
    ) -> Option<Place> {
        if !self.project_match_scrutinee_is_bitcopy(&scrutinee.ty) {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "literal predicates on owned record/tuple match destructure"
                        .to_string(),
                    site: scrutinee.site,
                },
                note: "runtime-selected project arms over an owned aggregate require \
                       path-sensitive partial-move and skipped-field drop elaboration; \
                       refusing to duplicate ownership across fallthrough edges"
                    .to_string(),
            });
            return None;
        }
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

        let result_place = self.alloc_local(result_ty.clone());
        let scrutinee_place = self.lower_value(scrutinee)?;
        let scrutinee_local = match scrutinee_place {
            Place::Local(local) => local,
            other => {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "project predicate match scrutinee place shape".to_string(),
                        site: scrutinee.site,
                    },
                    note: format!(
                        "record/tuple literal-predicate match requires a local scrutinee; got {other:?}"
                    ),
                });
                return None;
            }
        };

        let join_bb = self.alloc_block();
        let arm_bbs: Vec<u32> = (0..arms.len()).map(|_| self.alloc_block()).collect();
        let trap_bb = self.alloc_block();
        let first_bb = arm_bbs.first().copied().unwrap_or(trap_bb);
        self.finish_current_block(Terminator::Goto { target: first_bb });
        let mut join_reachable = false;

        for (arm_idx, arm) in arms.iter().enumerate() {
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
                            hew_hir::HirMatchArmPredicate::TupleProject { arity } => {
                                if predicate.field_idx >= *arity {
                                    self.diagnostics.push(MirDiagnostic {
                                        kind: MirDiagnosticKind::NotYetImplemented {
                                            construct:
                                                "tuple project predicate index out of range"
                                                    .to_string(),
                                            site: arm.body.site,
                                        },
                                        note: format!(
                                            "literal predicate projects field {} from arity {} tuple",
                                            predicate.field_idx, arity
                                        ),
                                    });
                                    return None;
                                }
                                self.push_instr(Instr::TupleFieldLoad {
                                    tuple: Place::Local(scrutinee_local),
                                    field_index: predicate.field_idx,
                                    dest: field,
                                });
                            }
                            _ => unreachable!("project arm classification changed during lowering"),
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

            let mut overwritten_bindings = Vec::with_capacity(arm.bindings.len());
            for binding in &arm.bindings {
                let binding_ty = self.subst_ty(&binding.ty);
                self.statements.push(MirStatement::Bind {
                    binding: binding.binding,
                    name: binding.name.clone(),
                    site: arm.body.site,
                    ty: binding_ty.clone(),
                });
                self.record_binding_scope(binding.binding);
                let dest = self.alloc_local(binding_ty);
                match &arm.predicate {
                    hew_hir::HirMatchArmPredicate::RecordProject { .. } => {
                        self.push_instr(Instr::RecordFieldLoad {
                            record: Place::Local(scrutinee_local),
                            field_offset: FieldOffset(binding.field_idx),
                            dest,
                        });
                    }
                    hew_hir::HirMatchArmPredicate::TupleProject { arity } => {
                        if binding.field_idx >= *arity {
                            self.diagnostics.push(MirDiagnostic {
                                kind: MirDiagnosticKind::NotYetImplemented {
                                    construct: "tuple project binding index out of range"
                                        .to_string(),
                                    site: arm.body.site,
                                },
                                note: format!(
                                    "binding `{}` projects field {} from arity {} tuple",
                                    binding.name, binding.field_idx, arity
                                ),
                            });
                            return None;
                        }
                        self.push_instr(Instr::TupleFieldLoad {
                            tuple: Place::Local(scrutinee_local),
                            field_index: binding.field_idx,
                            dest,
                        });
                    }
                    _ => {
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct: "wildcard/binding project arm carrying field bindings"
                                    .to_string(),
                                site: arm.body.site,
                            },
                            note: "only record/tuple project arms may carry project bindings"
                                .to_string(),
                        });
                        return None;
                    }
                }
                let previous = self.binding_locals.insert(binding.binding, dest);
                overwritten_bindings.push((binding.binding, previous));
            }

            if matches!(arm.predicate, hew_hir::HirMatchArmPredicate::Binding { .. }) {
                self.emit_match_arm_binding(arm, Place::Local(scrutinee_local), None);
            }

            let value = self.lower_value(&arm.body);
            for (binding, previous) in overwritten_bindings.into_iter().rev() {
                if let Some(previous) = previous {
                    self.binding_locals.insert(binding, previous);
                } else {
                    self.binding_locals.remove(&binding);
                }
            }
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
            if let Some(src) = self.lower_value(&arm.body) {
                self.push_instr(Instr::Move {
                    dest: result_place,
                    src,
                });
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
            // Slice 5 will introduce proper CString/StrPtr semantics; for now the
            // i64 opaque representation is substrate-correct for null-check dispatch.
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

            let value = self.lower_value(&arm.body);
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
            | HirExprKind::MachineVariantCtor { .. }
            | HirExprKind::Binary { .. } => {
                if self.scrutinee_precise_bits(scrutinee).is_fresh() {
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

    /// Group A plain-`Call` arm: apply the interim module-fn PARAM-present reject
    /// (the same rule the preflight admission classifier applies upstream — this
    /// is the one-authority defence-in-depth for the #2523 twin), rescued by the
    /// SAME S2b caller arg-scan: a `{PARAM}`-only summary whose every argument is
    /// provably fresh is a fresh sole owner → `EphemeralTemp` (the twin agrees
    /// with the preflight's `Admit`). A PARAM-carrying summary that the arg-scan
    /// cannot rescue (a place/param/aliased-local argument, or a mixed
    /// `PARAM|OPAQUE` return) → Reject. Every other callee (`∅`/`OPAQUE`-only
    /// module fn, unknown/cross-module item, extern, builtin, indirect/closure)
    /// keeps today's `EphemeralTemp` — the interim legacy fail-open window; the
    /// FULL `OPAQUE`-only reject lands at S4b.
    fn classify_call_arm_scrutinee_origin(&self, scrutinee: &HirExpr) -> ProjectedPayloadOrigin {
        use crate::return_provenance::AliasBits;
        if let HirExprKind::Call { callee, args } = &scrutinee.kind {
            if let HirExprKind::BindingRef {
                name,
                resolved: ResolvedRef::Item(id),
            } = &callee.kind
            {
                // An extern callee carries the PLACEHOLDER `ItemId(0)` — never
                // consult the module summary for it (id collision); the
                // preflight already rejected any heap-extern scrutinee.
                if !self.call_scrutinee_provenance.extern_names.contains(name) {
                    if let Some(bits) = self.call_scrutinee_provenance.provenance.get(id) {
                        if bits.contains(AliasBits::PARAM) {
                            if bits.is_params_only() && self.params_only_args_provably_fresh(args) {
                                return ProjectedPayloadOrigin::EphemeralTemp;
                            }
                            return ProjectedPayloadOrigin::Reject(
                                ProjectedPayloadRejectReason::AliasesCallerStorage,
                            );
                        }
                    }
                }
            }
        }
        ProjectedPayloadOrigin::EphemeralTemp
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
            self.projected_payload_provenance.insert(
                binding_id,
                ProjectedPayloadProvenance {
                    source_place,
                    binder_name: binder_name.to_string(),
                    origin,
                },
            );
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
        scrutinee: &HirExpr,
        arms: &[hew_hir::HirMatchArm],
        result_ty: &ResolvedTy,
    ) -> Option<Place> {
        // #2648 preflight — run BEFORE any `lower_value`/CFG allocation. A reject
        // (forwarded borrowed parameter, un-audited heap extern) pushes exactly
        // one diagnostic and returns with NO partial MIR: no scrutinee call, no
        // owner mint, no `NeutralizePayloadSlot`.
        let scrutinee_admission = match self.classify_call_scrutinee_admission(scrutinee) {
            Ok(admission) => admission,
            Err(diag) => {
                self.diagnostics.push(*diag);
                return None;
            }
        };
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
        let vec_string_iter_next_scrutinee = self.is_vec_string_iter_next_scrutinee(scrutinee);
        let generator_next_scrutinee = Self::is_generator_next_scrutinee(scrutinee);
        let recv_next_scrutinee = Self::is_recv_next_scrutinee(scrutinee);
        // #2523 — element-type-agnostic fresh-owned vec-element iteration
        // (`for pair in v` over `Vec<(string, string)>`, `Vec<Person>`, …).
        // Gates ONLY the provenance skip below; disposition still keys off the
        // narrower string/generator/recv markers.
        let vec_iter_next_scrutinee = self.is_vec_iter_next_scrutinee(scrutinee);

        // Lower the scrutinee in the entry block. A failure propagates
        // via `?`; the half-built match leaves no dangling block.
        let scrutinee_place = self.lower_value(scrutinee)?;
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
        let scrutinee_origin = self.classify_scrutinee_origin(scrutinee);

        // synthetic owned binding over its temp so the arm-destructured
        // payload is released on every exit edge — most importantly the loop
        // back-edge, where each iteration previously leaked one payload
        // (#2429). No-op for binding-ref scrutinees, runtime-symbol
        // producers, and the recv/iter-next shapes that carry their own
        // release discipline.
        self.register_from_call_scrutinee_owner(scrutinee_admission, scrutinee, scrutinee_local);

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

            let value = self.lower_value(&wildcard.body);
            if let Some(src) = value {
                self.push_instr(Instr::Move {
                    dest: result_place,
                    src,
                });
            }
            // A body that does not diverge leaves the cursor reachable and
            // flows to the join with the arm's value (which may be a Unit
            // no-op for an empty block). A diverging body (`return`/`panic`)
            // leaves the cursor in a dead block, so this Goto seals dead code.
            if !self.cursor_unreachable {
                join_reachable = true;
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
            let arm_is_vec_iter_some = vec_string_iter_next_scrutinee && arm_is_some;
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
            let mut overwritten_bindings = Vec::with_capacity(arm.bindings.len());
            let mut retained_vec_string_iter_bindings = Vec::new();
            // Generator-yielded `Some(x)` bindings whose payload owns heap. The
            // yielded value is a fresh, solely-owned heap value the coro
            // `.next()` drive handed the consumer; it is released at the end
            // of the consuming body (per-iteration for a `for`-in loop).
            // Removed from `owned_locals` below so the function-scope drop
            // pass does not also fire (which would double-free).
            let mut generator_yield_drop_bindings = Vec::new();
            for binding in &arm.bindings {
                let binding_ty = self.subst_ty(&binding.ty);
                self.statements.push(MirStatement::Bind {
                    binding: binding.binding,
                    name: binding.name.clone(),
                    site: arm.body.site,
                    ty: binding_ty.clone(),
                });
                self.record_binding_scope(binding.binding);
                let keep_for_drop_elab = self.binding_seeds_drop_elaboration(&binding_ty);
                if keep_for_drop_elab {
                    self.register_owned_local(
                        binding.binding,
                        binding.name.clone(),
                        binding_ty.clone(),
                    );
                }
                let dest = self.alloc_local(binding.ty.clone());
                self.push_instr(Instr::Move {
                    dest,
                    src: Place::MachineVariant {
                        local: scrutinee_local,
                        variant_idx,
                        field_idx: binding.field_idx,
                    },
                });
                let previous = self.binding_locals.insert(binding.binding, dest);
                overwritten_bindings.push((binding.binding, previous, keep_for_drop_elab));
                // #2523 — record provenance for a heap-owning TOP-LEVEL projected
                // payload binder so its `Consume`-intent move-out routes through
                // the default-deny consume hook.
                //
                // EXCEPTION — the vec-string-iter / generator / recv `Some(x)`
                // arms carry a FRESH, solely-owned per-frame payload (the
                // synthetic `Option` shell holds a value the runtime just
                // handed the consumer: `hew_vec_get_str`'s refcount-bumped
                // owner, a coro yield, a received frame). Its release is already
                // owned by the arm's own `Disposition::BodyEndReleased` +
                // escape-suppression discipline (registered below). It is NOT a
                // projection of a re-readable aggregate that retains the bits,
                // so a move-out (`return line`, store to a longer-lived owner)
                // is a legitimate ownership transfer, not a dangling-source
                // hazard. Routing it through default-deny would falsely reject
                // that transfer as a re-readable-place move-out. Skip it.
                let is_fresh_owned_frame_payload =
                    arm_is_fresh_owned_vec_iter_some || arm_is_generator_some || arm_is_recv_some;
                if !is_fresh_owned_frame_payload {
                    self.record_projected_payload_provenance(
                        binding.binding,
                        &binding.name,
                        Place::MachineVariant {
                            local: scrutinee_local,
                            variant_idx,
                            field_idx: binding.field_idx,
                        },
                        scrutinee_origin.clone(),
                        keep_for_drop_elab,
                    );
                }
                if arm_is_vec_iter_some && matches!(binding_ty, ResolvedTy::String) {
                    // `hew_vec_get_str` returns a FRESH, solely-owned retained
                    // owner (refcount bump via `hew_string_clone` — NOT a borrow
                    // of the Vec's buffer slot, unlike the owned-element getter).
                    // Its ownership shape is therefore identical to a
                    // generator-yielded `string`: a per-iteration heap reference
                    // the body must release with `hew_string_drop` on every exit
                    // edge. Take it out of `owned_locals` so the function-scope
                    // LIFO drop pass cannot ALSO fire on the binding's final slot
                    // value (double-free guard) — the per-iteration body-end drop
                    // and the break/continue edge drops (registered below) own the
                    // release. The body-shape escape scan in
                    // `emit_vec_string_iter_binding_drop` suppresses the body-end
                    // drop only when the binding's single retained reference
                    // genuinely escapes the body (a `Move`/store/return that hands
                    // the reference to a longer-lived owner), matching the
                    // generator-yield posture.
                    if keep_for_drop_elab {
                        self.set_owned_local_disposition(
                            binding.binding,
                            Disposition::BodyEndReleased,
                        );
                    }
                    retained_vec_string_iter_bindings.push((
                        binding.binding,
                        dest,
                        binding_ty,
                        arm.body.site,
                    ));
                } else if arm_is_generator_some || arm_is_recv_some {
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
                            generator_yield_drop_bindings.push((
                                binding.binding,
                                dest,
                                binding_ty,
                                arm.body.site,
                            ));
                        }
                        ReleaseSymbolVerdict::NoDropPath => {
                            // No validated consumer-drop path (HashMap /
                            // HashSet yields): the binding keeps its
                            // `owned_locals` entry and the function-scope
                            // machinery decides, leak-as-before rather than
                            // risking a double-free.
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
                self.statements.push(MirStatement::Bind {
                    binding: binding.binding,
                    name: binding.name.clone(),
                    site: arm.body.site,
                    ty: binding_ty.clone(),
                });
                self.record_binding_scope(binding.binding);
                let keep_for_drop_elab = self.binding_seeds_drop_elaboration(&binding_ty);
                if keep_for_drop_elab {
                    self.register_owned_local(binding.binding, binding.name.clone(), binding_ty);
                }
                let dest = self.alloc_local(binding.ty.clone());
                self.push_instr(Instr::Move {
                    dest,
                    src: Place::MachineVariant {
                        local: src_local,
                        variant_idx: src_variant_idx,
                        field_idx: binding.field_idx,
                    },
                });
                let previous = self.binding_locals.insert(binding.binding, dest);
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

            let body_start_block_id = self.current_block_id;
            let body_start_instr_len = self.instructions.len();

            // Register the iteration's yielded heap values as active so a
            // `break`/`continue` inside the body frees them on its edge
            // (symmetric to the generator-handle release). The depth marker is
            // the current `active_scopes` length: a break/continue at
            // `loop_scope_depth <= marker` is leaving/looping a loop this value
            // is lexically inside, so it must free it. Drained after the body
            // lowers (the fall-through path uses the body-end drop instead).
            let active_yield_mark = self.active_generator_yield_values.len();
            for (_binding, place, ty, _site) in &generator_yield_drop_bindings {
                let drop_fn = match self.generator_yield_drop_symbol(ty) {
                    ReleaseSymbolVerdict::Wired(symbol) => {
                        Some(crate::model::DropFnSpec::Release(symbol))
                    }
                    ReleaseSymbolVerdict::WiredInPlace(kind) => {
                        Some(crate::model::DropFnSpec::InPlace(kind))
                    }
                    ReleaseSymbolVerdict::NoDropPath | ReleaseSymbolVerdict::Unwired(_) => None,
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
            // The retained `Vec<String>` iteration binding is a per-iteration
            // heap reference with the same lifecycle as a yielded value: register
            // it on the same active-value stack so a `break`/`continue` inside the
            // body frees THIS iteration's retained string on its edge (the
            // body-end drop is emitted after the body lowers, so a break/continue
            // jumps past it and would otherwise leak the breaking iteration's
            // element). `hew_string_drop` is the release symbol; the inline drop's
            // null-after-free (codegen `emit_cow_heap_drop` + runtime header
            // guard) makes the mutually-exclusive fall-through body-end drop a
            // no-op on the break/continue path.
            for (_binding, place, ty, _site) in &retained_vec_string_iter_bindings {
                if matches!(ty, ResolvedTy::String) {
                    let depth = self.active_scopes.len();
                    self.active_generator_yield_values.push((
                        depth,
                        *place,
                        ty.clone(),
                        crate::model::DropFnSpec::Release("hew_string_drop"),
                        body_start_block_id,
                        body_start_instr_len,
                    ));
                }
            }

            let value = self.lower_value(&arm.body);

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
                self.push_instr(Instr::Move {
                    dest: result_place,
                    src,
                });
            }
            for (binding, place, ty, site) in retained_vec_string_iter_bindings {
                self.emit_vec_string_iter_binding_drop(
                    binding,
                    place,
                    &ty,
                    body_start_block_id,
                    body_start_instr_len,
                    site,
                );
            }
            for (binding, place, ty, site) in generator_yield_drop_bindings {
                self.emit_generator_yield_binding_drop(
                    binding,
                    place,
                    &ty,
                    body_start_block_id,
                    body_start_instr_len,
                    site,
                );
            }
            // A non-diverging arm body leaves the cursor reachable: this Goto
            // links a live predecessor into the join. A diverging body
            // (`return`/`panic`) leaves the cursor in a dead block (the
            // statement lowering for `return` flagged `cursor_unreachable`),
            // so the Goto seals dead code and contributes no live edge.
            if !self.cursor_unreachable {
                join_reachable = true;
            }
            self.finish_current_block(Terminator::Goto { target: join_bb });
        }

        // Join. Subsequent lowering continues here. When every arm diverged
        // (no arm fell through with a value), the join has no live predecessor:
        // flag the cursor unreachable so the caller does not emit a Move/Return
        // reading the never-written `result_place`. `start_block` resets the
        // flag, so set it AFTER opening the join.
        self.start_block(join_bb);
        if !join_reachable {
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
    /// descends through this nesting depth. For that to hold, the
    /// `derive_enum_composite_drop_allowed` escape scan must NOT misread the
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

    /// Emit the binding for a `Binding`-predicate arm: move the entire
    /// scrutinee value into a fresh local and register it in `binding_locals`.
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
        self.statements.push(MirStatement::Bind {
            binding: *binding_id,
            name: name.clone(),
            site: arm.body.site,
            ty: binding_ty.clone(),
        });
        self.record_binding_scope(*binding_id);
        let dest = self.alloc_local(binding_ty);
        self.push_instr(Instr::Move {
            dest,
            src: scrutinee_local,
        });
        self.binding_locals.insert(*binding_id, dest);
    }
}
