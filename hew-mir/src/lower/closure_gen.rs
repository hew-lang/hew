#![allow(
    deprecated,
    reason = "temporary named identity reconstruction migration seam"
)]

use super::{
    base_local, check_function, check_to_diagnostic, collect_unknown_type_diagnostics, dataflow,
    elaborate, finalize_bytes_ownership, finalize_string_ownership,
    materialize_explicit_goto_edge_carries, seal_checked, terminator_is_suspend_carrier,
    ActorStateLoadMode, BindingId, Builder, BuiltinType, CaptureEnvSource, ClosureEnvAllocation,
    ClosureEnvFieldInit, ClosureEnvFieldOwnership, Disposition, FieldOffset, HashSet, HirBlock,
    HirExpr, HirExprKind, HirFn, Instr, IntentKind, LambdaCapture, LoweredFunction, MirDiagnostic,
    MirDiagnosticKind, MirStatement, Place, RawMirFunction, ReleaseSymbolVerdict, ResolvedRef,
    ResolvedTy, SiteId, SourceOrigin, StreamProducerPumpCtx, SuspendKind, Terminator,
    ValueClass, SENTINEL_STREAM_SEND_VALUE_BINDING,
};
use crate::model::{GeneratorEnvFieldPlan, GeneratorEnvPlan};

fn is_lambda_pid_ty(ty: &ResolvedTy) -> bool {
    ty.is_builtin(BuiltinType::LambdaPid)
}

/// Where a closure-environment field reads its initial value from, and
/// therefore whether this frame holds an owner it can transfer.
///
/// This is a different question from provenance. Provenance asks whether the
/// value is domestic or foreign and is answered by the proven-foreign ledger;
/// this asks whether an owner exists HERE to be consumed, which is what
/// `ClosureEnvFieldOwnership::OwnsMoved` promises to do.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(super) enum CaptureSource {
    /// The captured binding has a MIR local slot in this frame, so a consuming
    /// `Use` statement can transfer its scope-exit owner into the environment.
    ConsumableLocal,
    /// The value was loaded out of an ENCLOSING closure or generator
    /// environment (`capture_env_sources` / `ClosureEnvFieldLoad`). That
    /// environment still owns the field and this frame has no owner to give.
    AliasedFromEnclosingEnv,
}

impl CaptureSource {
    fn of(source_binding: Option<BindingId>) -> Self {
        if source_binding.is_some() {
            Self::ConsumableLocal
        } else {
            Self::AliasedFromEnclosingEnv
        }
    }
}

impl Builder {
    /// True when `ty` may be snapshotted into a generator environment with a
    /// total semantic clone and inverse drop.
    ///
    /// Named functions and empty-capture closures remain trivial because their
    /// environment word is proven null. Every other admitted shape must pass the
    /// shared layout-aware clone-total verdict; affine/resource/opaque handles,
    /// trait objects, and capturing closure pairs remain fail-closed.
    fn gen_env_capture_admissible(&self, ty: &ResolvedTy) -> bool {
        if matches!(ty, ResolvedTy::Function { .. }) {
            return true;
        }
        if let ResolvedTy::Closure { captures, .. } = ty {
            return captures.is_empty();
        }
        let record_layouts = self.record_layouts_for_classification();
        crate::state_clone::classify_value_snapshot_plan_with_lifecycle_registry(
            ty,
            &record_layouts,
            &self.enum_layouts,
            &self.opaque_handle_names,
            &self.lifecycle_registry,
        )
        .and_then(|plan| {
            plan.is_clone_total(
                &record_layouts,
                &self.enum_layouts,
                &self.opaque_handle_names,
                &self.lifecycle_registry,
            )
        })
        .unwrap_or(false)
    }

    fn gen_env_capture_field_plan(&self, ty: &ResolvedTy) -> Option<GeneratorEnvFieldPlan> {
        if matches!(ty, ResolvedTy::Function { .. })
            || matches!(ty, ResolvedTy::Closure { captures, .. } if captures.is_empty())
        {
            return Some(GeneratorEnvFieldPlan::TrivialCopy);
        }
        let record_layouts = self.record_layouts_for_classification();
        let classified = crate::state_clone::classify_value_snapshot_plan_with_lifecycle_registry(
            ty,
            &record_layouts,
            &self.enum_layouts,
            &self.opaque_handle_names,
            &self.lifecycle_registry,
        );
        // JUSTIFIED: `None` feeds the caller's fail-closed capture diagnostic; it admits nothing.
        let plan = classified.ok()?;
        if matches!(
            plan.root(),
            crate::state_clone::StateFieldCloneKind::BitCopy { .. }
        ) {
            Some(GeneratorEnvFieldPlan::TrivialCopy)
        } else {
            Some(GeneratorEnvFieldPlan::Owned(plan))
        }
    }

    pub(crate) fn capture_env_whole_escape_requires_clone(&self, ty: &ResolvedTy) -> bool {
        let class = ValueClass::of_ty(ty, &self.type_classes);
        !matches!(ty, ResolvedTy::String)
            && class != ValueClass::BitCopy
            && class != ValueClass::PersistentShare
    }

    pub(crate) fn reject_capture_env_whole_escape(
        &mut self,
        name: &str,
        ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) {
        self.diagnostics.push(MirDiagnostic {
            kind: MirDiagnosticKind::NotYetImplemented {
                construct: format!("whole-value move of captured generator/closure value `{name}`"),
                site,
            },
            note: format!(
                "captured owned value `{name}` cannot be moved out of the generator/closure \
                 environment; it has type `{}` and is borrowed into the environment, whose \
                 storage remains the owner. Clone it explicitly before moving it, or use it only \
                 through borrowing methods/projections.",
                ty.user_facing()
            ),
        });
    }

    pub(crate) fn reject_capture_env_whole_escape_expr(&mut self, expr: &HirExpr) -> bool {
        let HirExprKind::BindingRef {
            name,
            resolved: ResolvedRef::Binding(binding),
        } = &expr.kind
        else {
            return false;
        };
        let Some(source) = self.capture_env_sources.get(binding).cloned() else {
            return false;
        };
        if !self.capture_env_whole_escape_requires_clone(&source.ty) {
            return false;
        }
        self.reject_capture_env_whole_escape(name, &source.ty, expr.site);
        true
    }

    pub(crate) fn sanitize_symbol_component(input: &str) -> String {
        input
            .chars()
            .map(|ch| if ch.is_ascii_alphanumeric() { ch } else { '_' })
            .collect()
    }

    pub(crate) fn closure_env_pointer_ty(env_ty: &ResolvedTy) -> ResolvedTy {
        ResolvedTy::Pointer {
            is_mutable: false,
            pointee: Box::new(env_ty.clone()),
        }
    }

    pub(crate) fn lower_closure_literal(
        &mut self,
        expr: &HirExpr,
        params: &[hew_hir::HirBinding],
        ret_ty: &ResolvedTy,
        body: &HirExpr,
        captures: &[hew_hir::HirClosureCapture],
        escape_kind: hew_types::ClosureEscapeKind,
    ) -> Option<Place> {
        // Build the foundation-API layout (plan §15.1). Stored on the
        // builder so downstream consumers (generator codegen, auto-lock
        // injection) can query `lock_slot_for()` / `has_suspend_in_body()`
        // without re-deriving facts. Lock-slot construction is gated
        // off here — the follow-on consumer flips the gate; the layout
        // API is the single source of truth for the slot tail offset
        // (plan §15.3 risk 8 mitigation).
        let capture_fields: Vec<crate::closure_env::CaptureField> = captures
            .iter()
            .enumerate()
            .map(|(idx, cap)| crate::closure_env::CaptureField {
                id: crate::closure_env::CaptureId(
                    u32::try_from(idx).expect("closure capture count exceeds u32::MAX"),
                ),
                ty: self.subst_ty(&cap.ty),
                mode: cap.mode,
                is_sync: cap.is_sync,
            })
            .collect();
        let layout = crate::closure_env::ClosureEnvLayout::build(
            capture_fields,
            body,
            escape_kind,
            /* enable_lock_slots = */ false,
        );
        // The layout's allocation strategy commits the env storage the
        // MakeClosure emit uses. `Stack` (Local escape class) keeps the
        // frame alloca address — the closure provably never outlives the
        // introducing scope. `Heap` (Escapes) promotes: with captures the
        // env is copied into a `hew_dyn_box_alloc` box so the pair stays
        // valid after this frame unwinds; capture-free escaping closures
        // store a null env instead (zero loads in the shim, nothing owned,
        // and null is the pair-drop protocol's "skip" signal). `ScopeOwned`
        // (Forked) closures never reach this lowering arm (they ride
        // `lower_spawned_closure_task`); if one ever does, the stack emit
        // preserves the pre-promotion behaviour rather than fabricating an
        // unowned heap box.
        let strategy = layout.allocation_strategy();
        let (shim_name, _env_ty, env_place, suspends, manifest) =
            self.materialize_closure_env(expr, params, ret_ty, body, captures, strategy)?;
        // Record the body-suspends verdict so the enclosing `Let` handler can
        // attribute it to the bound binding (the suspendable-callee
        // discriminator the closure-call site reads).
        self.pending_closure_literal_suspends = Some(suspends);

        let env_mode = match strategy {
            crate::closure_env::AllocationStrategy::Heap => {
                if captures.is_empty() {
                    crate::model::ClosureEnvMode::Null
                } else {
                    crate::model::ClosureEnvMode::HeapBox
                }
            }
            crate::closure_env::AllocationStrategy::Stack
            | crate::closure_env::AllocationStrategy::ScopeOwned => {
                crate::model::ClosureEnvMode::Stack
            }
        };
        // Record the escape verdict so the enclosing `Let` handler can admit
        // the bound pair into the closure-pair drop set (the same
        // pending-flag pattern `pending_closure_literal_suspends` uses).
        self.pending_closure_literal_heap =
            Some(strategy == crate::closure_env::AllocationStrategy::Heap);

        // The env free thunk drops exactly what this manifest says the
        // environment owns. A `Null` env has no record and therefore no fields.
        let env_ownership = if env_mode == crate::model::ClosureEnvMode::Null {
            Vec::new()
        } else {
            manifest.iter().map(|field| field.ownership).collect()
        };
        let closure_place = self.alloc_local(expr.ty.clone());
        self.push_instr(Instr::MakeClosure {
            fn_symbol: shim_name,
            env: env_place,
            dest: closure_place,
            env_mode,
            env_ownership,
        });
        self.publish_closure_env_moved_handoffs(&manifest);

        Some(closure_place)
    }

    /// Retire each source owner only after its owning environment has crossed
    /// the physical construction or task-handoff boundary.
    ///
    /// Heap closures call this immediately after `MakeClosure`; scope-owned
    /// task closures call it immediately after `SpawnTaskClosure`. Runtime calls
    /// before either boundary retain the source owner for unwind cleanup, while
    /// later exits leave the environment destructor as the sole release
    /// authority. A missing source binding generation emits no fabricated
    /// transfer, and the exact-place ownership validator still rejects stale
    /// or conflicting authority.
    pub(super) fn publish_closure_env_moved_handoffs(&mut self, manifest: &[ClosureEnvFieldInit]) {
        let mut transferred = HashSet::new();
        for field in manifest {
            let Some(binding) = field
                .source_binding
                .filter(|_| field.ownership == ClosureEnvFieldOwnership::OwnsMoved)
            else {
                continue;
            };
            let Some(generation) = self.owner_generations.get(&binding).copied() else {
                continue;
            };
            let owner = crate::model::OwnerId {
                binding,
                generation,
            };
            if transferred.insert(owner) {
                self.push_instr(Instr::OwnershipEvent(
                    crate::model::OwnershipEvent::Transfer {
                        owner,
                        from: field.src,
                        to: None,
                        to_owner: None,
                        to_ty: None,
                    },
                ));
            }
        }
    }

    /// U2 — the per-function proven-foreign ledger CROSSES into the closure
    /// environment, and this is where it crosses.
    ///
    /// The decision the round-5 report asked for is "either the ledger crosses,
    /// or captures are proven ownerless by construction". It crosses, and it
    /// crosses here rather than being shipped into the closure body, because
    /// `materialize_closure_env` runs in the ENCLOSING builder — the frame whose
    /// ledger holds the fact — and `OwnsMoved` is the ONLY way a capture becomes
    /// owned. The closure body never mints an owner for a capture: it reads one
    /// through `capture_env_sources` / `ClosureEnvFieldLoad`, and whole-value
    /// escapes out of the env are already refused
    /// (`reject_capture_env_whole_escape`).
    ///
    /// `OwnsMoved` does two things at once: it consumes the source binding's
    /// owner (suppressing that binding's own drop) and it makes the heap env's
    /// destructor the release authority for the field. For a binding the `let`
    /// binder refused an owner for, the first half consumes an owner that never
    /// existed and the second half frees a handle the host still owns. Reading
    /// the ledger turns exactly that case into `BorrowsOnly`: the env aliases the
    /// value and releases nothing — the leak-not-double-free direction, and the
    /// same answer the binder itself gave.
    ///
    /// A checker-inferred `Borrow` capture (read-only body use, no `move`
    /// keyword) does NOT consume the source: the env owns a RETAINED SHARE
    /// (`OwnsClonedOrRetained`) and the source binding keeps its own owner.
    /// The string ownership prover mints the balancing `+1`
    /// (`string_share_sink_places` lists these env fields as share sinks;
    /// aggregate sources get `StringRetainCondition::AggregateBorrowedIngress`),
    /// and the env free thunk releases the env's share — two independent
    /// owners, each released exactly once. The share is admitted only for
    /// shapes the recursive string retain fully co-owns
    /// (`string_or_bitcopy_tree`): a shape with bytes/collection/resource
    /// leaves keeps the consuming `OwnsMoved` capture — the fail-closed
    /// direction (compile-time `UseAfterConsume` on a later source use), never
    /// a partially-retained env that would double-free the unretained leaves.
    /// That fallback retires when a whole-value retain/clone authority exists
    /// for those leaf classes (copy-on-write north star); extend the predicate, not this
    /// match.
    ///
    /// # `OwnsMoved` requires a source this frame can consume
    ///
    /// `OwnsMoved` is a TRANSFER, and a transfer has two halves: consume the
    /// source binding's owner, and install the env destructor as the release
    /// authority. `CaptureSource::AliasedFromEnclosingEnv` — a capture resolved
    /// through `capture_env_sources` and loaded out of an ENCLOSING closure or
    /// generator environment by `ClosureEnvFieldLoad` — has no local slot in
    /// this frame, so the first half cannot be emitted: there is no `Use`
    /// statement to write against a binding with no slot, and the enclosing
    /// environment goes on owning the field regardless. Taking the second half
    /// alone does not transfer anything; it mints a SECOND owner of one value.
    ///
    /// So the unavailability of the consume half is the proof that the own half
    /// is illegitimate, and `OwnsMoved` is gated on it below — once, after the
    /// per-strategy match, so no arm can grow its own exception.
    ///
    /// The gate does not simply drop to `BorrowsOnly`. An aliasing env field is
    /// only valid while the environment it read from is alive, and a closure
    /// carrying a transitive capture CAN outlive its generator (yield the
    /// closure out of the `gen` body and call it after the generator is
    /// destroyed — the checker admits this). So where the shape is one the
    /// recursive string retain fully co-owns (`string_or_bitcopy_tree`), the
    /// field takes `OwnsClonedOrRetained` instead: the env mints its OWN share,
    /// its free thunk releases that share, and the enclosing environment keeps
    /// its owner — two independent owners, each released exactly once, with
    /// independent lifetimes. `string_share_sink_places` keys off this verdict
    /// (not off the capture mode), so the balancing retain is minted for a
    /// `Move` capture on this path exactly as it is for a `Borrow` one.
    ///
    /// A shape with bytes/collection/resource leaves has no whole-value retain
    /// authority yet, so it falls to `BorrowsOnly`: an alias the enclosing
    /// environment still owns. That is the leak/dangle-if-escaped direction
    /// rather than the double-free one, and it retires with the copy-on-write
    /// north star — extend the predicate, not this gate.
    ///
    /// `OwnsClonedOrRetained` reached directly is deliberately NOT gated: it
    /// consumes nothing to begin with.
    ///
    /// # The proven-foreign ledger is keyed by the CAPTURED binding
    ///
    /// Not by the place the value is read from. `source_binding` answers "can
    /// this frame consume it"; the binding id answers "what is its provenance",
    /// and it is a valid ledger key regardless of slot residency. The parent's
    /// ledger is cloned into every child builder precisely so a nested body
    /// sees the fact.
    pub(super) fn closure_env_capture_ownership(
        &self,
        strategy: crate::closure_env::AllocationStrategy,
        ty: &ResolvedTy,
        binding: BindingId,
        source: CaptureSource,
        mode: hew_types::ClosureCaptureMode,
    ) -> ClosureEnvFieldOwnership {
        let ownership = self.closure_env_capture_ownership_by_strategy(strategy, ty, binding, mode);
        if ownership == ClosureEnvFieldOwnership::OwnsMoved
            && source == CaptureSource::AliasedFromEnclosingEnv
        {
            // Nothing here to consume, so no transfer to claim. Take an
            // independent share where the shape supports one; otherwise alias
            // and leave the enclosing environment as the sole release
            // authority.
            let ty = self.subst_ty(ty);
            return if super::composite_own::string_or_bitcopy_tree(
                &ty,
                &self.record_field_orders,
                &self.enum_layouts,
                &mut HashSet::new(),
            ) {
                ClosureEnvFieldOwnership::OwnsClonedOrRetained
            } else {
                ClosureEnvFieldOwnership::BorrowsOnly
            };
        }
        ownership
    }

    /// The per-strategy verdict, before the consumable-source gate above.
    fn closure_env_capture_ownership_by_strategy(
        &self,
        strategy: crate::closure_env::AllocationStrategy,
        ty: &ResolvedTy,
        binding: BindingId,
        mode: hew_types::ClosureCaptureMode,
    ) -> ClosureEnvFieldOwnership {
        match strategy {
            crate::closure_env::AllocationStrategy::Stack => ClosureEnvFieldOwnership::BorrowsOnly,
            crate::closure_env::AllocationStrategy::ScopeOwned => {
                let ty = self.subst_ty(ty);
                let nothing_to_own =
                    ValueClass::of_ty(&ty, &self.type_classes) == ValueClass::BitCopy;
                let proven_foreign = self.proven_foreign_bindings.contains(&binding);
                if mode == hew_types::ClosureCaptureMode::Move && !nothing_to_own && !proven_foreign
                {
                    ClosureEnvFieldOwnership::OwnsMoved
                } else {
                    ClosureEnvFieldOwnership::BorrowsOnly
                }
            }
            crate::closure_env::AllocationStrategy::Heap => {
                let ty = self.subst_ty(ty);
                // Nothing to release, so nothing to own.
                let nothing_to_own =
                    ValueClass::of_ty(&ty, &self.type_classes) == ValueClass::BitCopy;
                // U2 — THE LEDGER CROSSES. `own_moved` consumes the source
                // binding's scope-exit owner AND installs the heap env
                // destructor as the release authority for the captured value.
                // For a binding this frame proved foreign, the second half is a
                // release of a handle the program never owned. This decision
                // runs in the ENCLOSING builder, which is exactly the frame
                // whose ledger holds the fact, and the parent's ledger is
                // cloned into every child builder so a nested closure sees it
                // too.
                let proven_foreign = self.proven_foreign_bindings.contains(&binding);
                if nothing_to_own || proven_foreign {
                    ClosureEnvFieldOwnership::BorrowsOnly
                } else if mode == hew_types::ClosureCaptureMode::Borrow
                    && super::composite_own::string_or_bitcopy_tree(
                        &ty,
                        &self.record_field_orders,
                        &self.enum_layouts,
                        &mut HashSet::new(),
                    )
                {
                    ClosureEnvFieldOwnership::OwnsClonedOrRetained
                } else {
                    ClosureEnvFieldOwnership::OwnsMoved
                }
            }
        }
    }

    fn closure_env_allocation_manifest(
        strategy: crate::closure_env::AllocationStrategy,
    ) -> ClosureEnvAllocation {
        match strategy {
            crate::closure_env::AllocationStrategy::Stack => ClosureEnvAllocation::Stack,
            crate::closure_env::AllocationStrategy::Heap => ClosureEnvAllocation::Heap,
            crate::closure_env::AllocationStrategy::ScopeOwned => ClosureEnvAllocation::ScopeOwned,
        }
    }

    #[allow(
        clippy::too_many_lines,
        reason = "closure env materialization keeps env layout registration, \
                  ownership manifest construction, and shim generation aligned"
    )]
    pub(crate) fn materialize_closure_env(
        &mut self,
        expr: &HirExpr,
        params: &[hew_hir::HirBinding],
        ret_ty: &ResolvedTy,
        body: &HirExpr,
        captures: &[hew_hir::HirClosureCapture],
        strategy: crate::closure_env::AllocationStrategy,
    ) -> Option<(String, ResolvedTy, Place, bool, Vec<ClosureEnvFieldInit>)> {
        let closure_id = self.next_closure_id;
        self.next_closure_id = self
            .next_closure_id
            .checked_add(1)
            .expect("closure id overflow");
        // A NESTED closure literal is lowered while `current_function_symbol`
        // is the PARENT closure's invoke shim (`__hew_closure_invoke_<path>`).
        // Using it verbatim as the owner re-prefixes the symbol on every level
        // (`__hew_closure_invoke___hew_closure_invoke_main_0_0`) — redundant and
        // unbounded in nesting depth. Strip the shim prefix so the owner is the
        // stable nesting PATH (`main_0`): child builders reset `next_closure_id`
        // to 0, so the parent path is what keeps sibling/cross-level shim names
        // unique. The `MakeClosure` and the shim both derive their symbol from
        // this single site, so they stay in lockstep.
        let owner_src = self
            .current_function_symbol
            .strip_prefix("__hew_closure_invoke_")
            .unwrap_or(&self.current_function_symbol);
        let owner = Self::sanitize_symbol_component(owner_src);
        let env_name = format!("__hew_closure_env_{owner}_{closure_id}");
        let shim_name = format!("__hew_closure_invoke_{owner}_{closure_id}");
        let env_ty = ResolvedTy::Named {
            name: env_name.clone(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        };
        // Minted beside the shim symbol, from the SAME encounter order, so the
        // key of a nested closure records its parent chain instead of the
        // flattened owner path the symbol spells.
        let shim_key =
            self.mint_synthesized_child_key(crate::model::SynthesizedCallable::ClosureInvokeShim);

        // Each capture field's `ResolvedTy` is substituted through the
        // per-monomorphisation map: a closure inside `fn f<T>(x: T)` that
        // captures `x` records the env field as the CONCRETE argument (`i64`,
        // `string`, …), never the bare type-parameter `T`. The env record
        // layout is walked verbatim by the codegen-readiness diagnostic
        // (`collect_layout_field_diagnostics`) with no subst map of its own, so
        // an un-substituted `T` here surfaces as `E_MIR UnknownType T`; it is
        // also the struct codegen lays out, so the field MUST be concrete for a
        // correct ABI. A non-generic origin takes the identity-map fast path.
        let env_field_tys: Vec<ResolvedTy> = captures
            .iter()
            .map(|capture| self.subst_ty(&capture.ty))
            .collect();
        self.closure_record_layouts
            .push(crate::model::RecordLayout {
                name: env_name,
                field_tys: env_field_tys,
                // Compiler-internal closure-env record: positional `-g` names.
                field_names: Vec::new(),
            });

        let mut field_pairs = Vec::with_capacity(captures.len());
        let mut failed = false;
        let allocation = Self::closure_env_allocation_manifest(strategy);
        for (idx, capture) in captures.iter().enumerate() {
            let offset =
                FieldOffset(u32::try_from(idx).expect("closure capture count exceeds u32::MAX"));

            // Defence-in-depth gate: a `LambdaPid<M,R>` (lambda-actor handle) must
            // never appear as a fn-closure capture env field. The authoritative
            // rejection is `TypeErrorKind::ClosureCapturesDuplexHandle` in the
            // checker's `check_call`; if MIR sees one here, the checker gate
            // was bypassed by a new source form. Fail closed rather than
            // misrouting to `hew_duplex_send` (wrong runtime ABI).
            if is_lambda_pid_ty(&capture.ty) {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::ClosureCapturesDuplexHandle {
                        name: capture.name.clone(),
                        site: expr.site,
                    },
                    note: format!(
                        "closure capture `{}` has type LambdaPid<_,_>; no env-materialization \
                         protocol exists — checker gate in `check_call` must have been bypassed",
                        capture.name
                    ),
                });
                failed = true;
                continue;
            }

            let (src, source_binding) = if let Some(place) =
                self.binding_locals.get(&capture.binding).copied()
            {
                (place, Some(capture.binding))
            } else if let Some(source) = self.capture_env_sources.get(&capture.binding).cloned() {
                let temp = self.alloc_local(source.ty.clone());
                self.push_instr(Instr::ClosureEnvFieldLoad {
                    env: source.env,
                    env_ty: source.env_ty,
                    field_offset: source.field_offset,
                    dest: temp,
                });
                (temp, None)
            } else {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::CannotMaterializeClosureCapture {
                        binding: capture.binding,
                        name: capture.name.clone(),
                        site: expr.site,
                    },
                    note: format!(
                        "closure capture `{}` has no MIR backend slot or enclosing closure env field",
                        capture.name
                    ),
                });
                failed = true;
                continue;
            };
            let field_ty = self.subst_ty(&capture.ty);
            // Ledger query keys off the captured binding; `source_binding`
            // answers the separate question of whether this frame holds an
            // owner it can consume.
            let ownership = self.closure_env_capture_ownership(
                strategy,
                &field_ty,
                capture.binding,
                CaptureSource::of(source_binding),
                capture.mode,
            );
            // An ESCAPING environment that can neither own the field nor take a
            // share of it would be left aliasing storage the enclosing
            // environment releases at its own destruction — and this closure can
            // outlive that. Refuse instead of emitting the alias.
            if strategy == crate::closure_env::AllocationStrategy::Heap
                && source_binding.is_none()
                && ownership == ClosureEnvFieldOwnership::BorrowsOnly
                && crate::model::ty_owns_heap_mir(
                    &field_ty,
                    &self.record_field_orders,
                    &self.enum_layouts,
                )
                && !super::composite_own::string_or_bitcopy_tree(
                    &field_ty,
                    &self.record_field_orders,
                    &self.enum_layouts,
                    &mut HashSet::new(),
                )
            {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::EscapingCaptureAliasesEnclosingEnv {
                        name: capture.name.clone(),
                        ty: match &field_ty {
                            ResolvedTy::Named { name, .. } => name.clone(),
                            other => format!("{other:?}"),
                        },
                        site: expr.site,
                    },
                    note: format!(
                        "escaping closure capture `{}` is read out of an enclosing \
                         closure/generator environment that still owns it, and its type has no \
                         whole-value retain authority, so this environment can take neither the \
                         owner nor a share; the closure may outlive that environment and would \
                         read freed storage. Bind the value into a local of this frame first, or \
                         capture a field with a retainable type",
                        capture.name
                    ),
                });
                failed = true;
                continue;
            }
            if ownership == ClosureEnvFieldOwnership::OwnsMoved {
                if let Some(binding) = source_binding {
                    self.statements.push(MirStatement::Use {
                        binding,
                        name: capture.name.clone(),
                        site: expr.site,
                        ty: field_ty.clone(),
                        intent: IntentKind::Consume,
                    });
                }
            }
            // `ClosureEnvInit` copies the source bytes into the environment.
            // When those bytes came from an owned call-carrier parameter, the
            // environment becomes the sole owner and the original parameter
            // slot must be neutralized before the unconditional terminal
            // carrier drop. Ordinary bindings are not carrier-tracked, so the
            // transfer funnel leaves their source place unchanged.
            let src = if ownership == ClosureEnvFieldOwnership::OwnsMoved {
                self.transfer_owned_carrier_place(src, &field_ty)
            } else {
                src
            };
            field_pairs.push(ClosureEnvFieldInit {
                field_offset: offset,
                src,
                source_binding,
                capture_mode: capture.mode,
                allocation,
                ownership,
                source_is_parameter: self.funcupdate_param_ids.contains(&capture.binding),
            });
        }
        if failed {
            return None;
        }

        let env_place = self.alloc_local(env_ty.clone());
        self.push_instr(Instr::ClosureEnvInit {
            ty: env_ty.clone(),
            fields: field_pairs.clone(),
            dest: env_place,
        });

        let mut lowered = self.lower_closure_shim(
            &shim_name, &shim_key, &env_ty, params, ret_ty, body, captures,
        );
        // The suspendable-callee discriminator: the closure's invoke shim is a
        // coroutine iff its lowered MIR carries a suspend terminator — the
        // IDENTICAL structural fact codegen's `is_coroutine` reads off the same
        // shim (`hew-codegen-rs` `declare_function`/`lower_function`). Deriving
        // the call-site driver decision from the same carriers makes the two
        // sites agree by construction (container-abi-ctor-op-agreement) — there
        // is no second suspends-tracker to drift.
        let suspends = lowered
            .raw
            .blocks
            .iter()
            .any(|b| terminator_is_suspend_carrier(&b.terminator));
        if suspends {
            lowered.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "suspension inside a closure".to_string(),
                    site: body.site,
                },
                note: "a suspending closure lowers to a coroutine ramp whose native return is a \
                       continuation handle, but function types do not yet carry the suspension \
                       metadata needed for every direct, nested, and higher-order invocation to \
                       select the matching driver; refusing the closure prevents that handle from \
                       being interpreted as the closure's declared result"
                    .to_string(),
            });
        }
        self.generated_functions.push(lowered);

        Some((shim_name, env_ty, env_place, suspends, field_pairs))
    }

    /// The module-level lookup tables every child `Builder` (closure shim,
    /// lambda-actor body, task-entry adapter) inherits from its parent, plus
    /// `Builder::default()` for everything per-function. A nested body must
    /// resolve the same module facts the parent resolves — in particular
    /// `actor_layouts` drives `actor_method_info` (sends to a captured pid)
    /// and the `UnknownType` silencing in the codegen-readiness walk, so a
    /// site that misses it diagnoses `actor call on unknown actor` for a
    /// perfectly well-formed send. Construction sites override only the
    /// per-function identity fields (`current_function_symbol`,
    /// `current_function_call_conv`, body flags) via struct-update syntax;
    /// any future shared table belongs HERE so it cannot silently miss one
    /// of the construction sites.
    pub(crate) fn child_builder_tables(&self) -> Builder {
        Builder {
            type_classes: self.type_classes.clone(),
            record_field_orders: self.record_field_orders.clone(),
            actor_layouts: self.actor_layouts.clone(),
            supervisor_layout_map: self.supervisor_layout_map.clone(),
            enum_layouts: self.enum_layouts.clone(),
            opaque_handle_names: self.opaque_handle_names.clone(),
            // Inherit the resource registry so a resource-bearing record dropped
            // inside a closure shim / lambda-actor / gen body classifies the
            // handle as `Resource` (runs its close), not the empty-registry
            // `OpaqueHandle` no-op that would leak it.
            lifecycle_registry: self.lifecycle_registry.clone(),
            machine_layout_names: self.machine_layout_names.clone(),
            module_fn_names: self.module_fn_names.clone(),
            module_generic_fn_names: self.module_generic_fn_names.clone(),
            // Direct calls in nested user bodies keep the checker-selected
            // declaration, so their linker projection must cross the frame
            // boundary with the rest of the module facts.  In particular, an
            // imported generic free function may be first called from a
            // closure/lambda/task body; reconstructing a name from its DefId
            // here would let a same-leaf declaration win.  The parent already
            // received this exact declaration-to-emitted-symbol map from HIR.
            direct_call_symbols: self.direct_call_symbols.clone(),
            param_ownership: self.param_ownership.clone(),
            // U2 — the proven-foreign ledger CROSSES the frame boundary with the
            // captures. `BindingId`s are globally unique, so carrying the
            // parent's set verbatim is exact: a nested body that re-derives
            // ownership for a captured binding reads the same refusal the
            // enclosing `let` recorded, and a binding the child cannot see is
            // never consulted. Without this the child's ledger is empty and a
            // captured foreign handle re-enters every mint inside the body clean.
            proven_foreign_bindings: self.proven_foreign_bindings.clone(),
            subst: self.subst.clone(),
            call_site_type_args: self.call_site_type_args.clone(),
            supervisor_child_slots: self.supervisor_child_slots.clone(),
            task_entry_adapter_symbols: self.task_entry_adapter_symbols.clone(),
            // Child builders (closure shims, lambda-actor bodies, gen bodies)
            // inherit the parent's target pointer width so an isize/usize
            // div/shift lowered inside a closure emits the same per-target guard.
            pointer_width: self.pointer_width,
            // Destructive-funcupdate base provenance is computed once per
            // top-level function over the WHOLE body (the prescan recurses into
            // closure/gen bodies), and `BindingId`s are globally unique, so the
            // parent map already classifies this child's bindings. Inherit it so
            // a `{ ..base, f }` inside a closure is gated by the same proof
            // instead of failing closed for want of the map.
            funcupdate_base_proven: self.funcupdate_base_proven.clone(),
            // Same rationale: the enclosing function's by-value parameters are
            // globally-unique bindings a closure can capture and embed in a
            // funcupdate base, so the child must see them to reject the borrow.
            funcupdate_param_ids: self.funcupdate_param_ids.clone(),
            // #2648 — the module return-provenance context MUST reach every
            // child builder: without it the preflight classifies a resolved
            // module fn as an unknown item, which now declines rather than
            // mints — so a `match wrap(captured)` INSIDE a closure would lose a
            // legitimate release instead of gaining a double one. Never
            // `Builder::default()` for this field on a user-body child.
            call_scrutinee_provenance: self.call_scrutinee_provenance.clone(),
            // `call_scrutinee_local_freshness` is deliberately NOT inherited
            // (the spread leaves it the empty fail-closed default): the
            // parent's single-read/unaliased facts are computed for ONE
            // execution of the parent body, while a child body (closure /
            // lambda-actor / generator) can run any number of times per
            // parent execution — a captured local that looks single-read in
            // the parent may be re-read on every invocation. An empty map
            // admits NO local argument inside child bodies; literal /
            // fresh-ctor / Fresh-call arguments still admit.
            ..Builder::default()
        }
    }

    #[allow(
        clippy::too_many_lines,
        clippy::too_many_arguments,
        reason = "closure shim construction keeps raw/checked/elaborated MIR snapshots aligned \
                  and carries the shim's callable identity beside its emitted symbol"
    )]
    fn lower_closure_shim(
        &self,
        shim_name: &str,
        key: &crate::model::MirCallableKey,
        env_ty: &ResolvedTy,
        params: &[hew_hir::HirBinding],
        ret_ty: &ResolvedTy,
        body: &HirExpr,
        captures: &[hew_hir::HirClosureCapture],
    ) -> LoweredFunction {
        // Resolve the shim's return type through the per-monomorphisation subst
        // map: a closure in `fn f<T>() -> T` lowers its invoke shim with the
        // CONCRETE return ABI, never the bare `T`. The child builder inherits
        // `subst`, so the body's locals already substitute via `alloc_local`;
        // the shim's raw `return_ty`/`params` are built directly here and must
        // be substituted explicitly so codegen sees a concrete ABI.
        let ret_ty_subst = self.subst_ty(ret_ty);
        let ret_ty = &ret_ty_subst;
        let env_ptr_ty = Self::closure_env_pointer_ty(env_ty);
        // `child_builder_tables` carries the full shared-table list — notably
        // `actor_layouts`, so a closure body that sends to a captured pid
        // resolves `actor_method_info` exactly as the parent would, and
        // `supervisor_child_slots`, so the FieldAccess intercept arm fires for
        // a closure body that reads a child slot off a captured supervisor PID.
        let mut builder = Builder {
            current_function_symbol: shim_name.to_string(),
            // A closure nested inside this shim's body parents its own key
            // here, so nesting depth is carried by the key chain rather than
            // recovered from the owner baked into the emitted symbol.
            current_callable_key: Some(key.clone()),
            current_function_call_conv: crate::model::FunctionCallConv::ClosureInvoke,
            ..self.child_builder_tables()
        };
        if matches!(ret_ty, ResolvedTy::String)
            && !self.closure_body_returns_owned_string_carrier(body, params, captures)
        {
            builder.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "closure string return without an owned-return contract".to_string(),
                    site: body.site,
                },
                note: "a `fn(...) -> string` closure must return one independently \
                       releasable share; this body can reach an ownership-opaque \
                       string producer, so admitting its invoke result would \
                       manufacture a caller drop obligation"
                    .to_string(),
            });
        }

        let env_place = builder.alloc_local(env_ptr_ty.clone());
        for (idx, capture) in captures.iter().enumerate() {
            builder.capture_env_sources.insert(
                capture.binding,
                CaptureEnvSource {
                    env: env_place,
                    env_ty: env_ty.clone(),
                    field_offset: FieldOffset(
                        u32::try_from(idx).expect("closure capture count exceeds u32::MAX"),
                    ),
                    ty: self.subst_ty(&capture.ty),
                },
            );
            if self
                .closure_pair_env_may_be_nonnull
                .contains(&capture.binding)
            {
                builder
                    .closure_pair_env_may_be_nonnull
                    .insert(capture.binding);
            }
            if self.closure_pair_null_env.contains(&capture.binding) {
                builder.closure_pair_null_env.insert(capture.binding);
            }
        }
        for param in params {
            let place = builder.alloc_local(param.ty.clone());
            if let Place::Local(local) = place {
                // Closure-invoke arguments use the same borrowed by-value ABI
                // as ordinary Hew `string` parameters. Mirror `lower_params`
                // here so return ownership derivation can distinguish the
                // borrowed entry definition from a fresh producer: returning
                // this slot must retain one share for the caller, while a
                // fresh string result must not be retained again.
                builder.parameter_locals.insert(local);
                if matches!(builder.subst_ty(&param.ty), ResolvedTy::String) {
                    builder.borrowed_string_param_locals.insert(local);
                }
                // Closure-invoke `string`/`bytes` arguments are caller-borrowed
                // exactly like ordinary Hew by-value parameters (closures have
                // no consume/owned-carrier classification), so mirror the
                // structural registry the collection-ingress retain and the
                // outbound no-transfer guard consult.
                if matches!(
                    builder.subst_ty(&param.ty),
                    ResolvedTy::String | ResolvedTy::Bytes
                ) {
                    builder.borrowed_value_param_locals.insert(local);
                }
            }
            builder.binding_locals.insert(param.id, place);
            builder.seed_fn_param_provenance(param);
        }

        // #2301 -- run the same owned-Vec-key / consumed-and-reassigned-binding
        // pre-pass on the closure body that `function_body` runs for a
        // top-level function. `lower_closure_shim` builds a brand-new child
        // `Builder` and lowers `body` directly below; without this call,
        // `prepass_consumed_bindings`/`prepass_reassigned_bindings`
        // stay empty for every closure-local binding, so
        // `maybe_alloc_overwrite_guard_flag` never fires inside a closure body
        // and a `var` consumed on one control-flow arm and overwritten on a
        // sibling arm silently leaks its prior value (no guard flag, no
        // release before the overwrite) instead of getting the path-sensitive
        // release a byte-identical top-level function body would.
        builder.collect_expr_prepass_facts(body);

        if let Some(src) = builder.lower_value_for_move(body) {
            builder.push_instr(Instr::Move {
                dest: Place::ReturnSlot,
                src,
            });
        }
        builder.statements.push(MirStatement::Return {
            site: Some(body.site),
            ty: ret_ty.clone(),
        });

        // Closure bodies finish through the SAME seam as every other body kind
        // (`finalize_body`), so an ownership pass added there reaches a closure
        // without a second registration. This ramp used to hand-roll a partial
        // copy of `lower_function`'s pipeline, and a divergent-arm selection
        // inside a closure leaked a whole `Vec` per call because the copy
        // predated the pass that releases it.
        let super::FinalizedBody {
            blocks,
            body_statements,
        } = super::finalize_body(
            &mut builder,
            super::BodySeal::Cursor(Terminator::Return),
            super::BodyFinalizeSpec::nested_body(),
        );
        let mut raw_params = Vec::with_capacity(params.len() + 1);
        raw_params.push(env_ptr_ty);
        raw_params.extend(params.iter().map(|param| self.subst_ty(&param.ty)));
        let mut raw = RawMirFunction {
            name: shim_name.to_string(),
            key: key.clone(),
            return_ty: ret_ty.clone(),
            call_conv: crate::model::FunctionCallConv::ClosureInvoke,
            params: raw_params,
            locals: builder.locals.clone(),
            // Synthesised closure-invoke shim: no faithful user bindings.
            local_names: Vec::new(),
            local_scopes: Vec::new(),
            local_decl_bytes: Vec::new(),
            scope_table: Vec::new(),
            blocks,
            decisions: builder.decisions.clone(),
            intrinsic_id: None,
            await_deadline_ns: builder.await_deadline_ns.clone(),
            suspend_kinds: builder.suspend_kinds.clone(),
            lambda_actor_user_param_locals: Vec::new(),
            span: None,
            instr_spans: ::std::collections::BTreeMap::new(),
            source_origin: SourceOrigin::Unknown,
        };
        let synthetic_func = HirFn {
            id: hew_hir::ItemId(0),
            node: hew_hir::HirNodeId(0),
            declaration: hew_types::DefId::legacy_reconstruct_from_full_path(shim_name),
            name: shim_name.to_string(),
            type_params: Vec::new(),
            is_generator: false,
            intrinsic_id: None,
            params: params.to_vec(),
            return_ty: ret_ty.clone(),
            body: hew_hir::HirBlock {
                node: hew_hir::HirNodeId(0),
                scope: hew_hir::ScopeId(0),
                statements: Vec::new(),
                tail: None,
                ty: ret_ty.clone(),
                span: body.span.clone(),
            },
            span: body.span.clone(),
        };
        let dataflow_result = check_function(&builder, &raw.blocks, &synthetic_func);
        let mut diagnostics: Vec<MirDiagnostic> = dataflow_result
            .checks
            .iter()
            .filter_map(check_to_diagnostic)
            .collect();
        diagnostics.append(&mut builder.diagnostics);
        collect_unknown_type_diagnostics(&synthetic_func, &builder, &mut diagnostics);
        finalize_string_ownership(&mut raw, &mut builder, &dataflow_result);
        finalize_bytes_ownership(&mut raw, &mut builder, &dataflow_result);
        materialize_explicit_goto_edge_carries(&mut raw.blocks, &mut builder);
        raw.instr_spans.clone_from(&builder.instr_spans);
        let cooperate_sites = dataflow::compute_cooperate_sites(&raw.blocks);
        let (checked, elaboration_diagnostics) = seal_checked(
            shim_name.to_string(),
            key.clone(),
            ret_ty.clone(),
            raw.blocks.clone(),
            &raw,
            builder.decisions.clone(),
            dataflow_result.checks.clone(),
            cooperate_sites,
            &builder,
            &body_statements,
        );
        let elaborated = elaborate(&checked);
        diagnostics.extend(elaboration_diagnostics);

        LoweredFunction {
            raw,
            checked,
            elaborated,
            diagnostics,
            generated: builder.generated_functions,
            record_layouts: builder.closure_record_layouts,
        }
    }

    /// Synthesise a `ClosureInvoke`-ABI shim that forwards its user
    /// arguments to the named top-level function `fn_symbol` and stores
    /// the result into `ReturnSlot`. The shim is the bridge between the
    /// closure-pair calling convention (`ctx_ptr`, `env_ptr`, `...user_args`)
    /// and the plain calling convention of the target function.
    ///
    /// The `env_ptr` parameter (`locals[0]`) is **never loaded** — the named
    /// function has no captures. This is the core safety invariant: a
    /// null or garbage `env_ptr` is safe because the shim body contains
    /// zero `ClosureEnvFieldLoad` instructions.
    ///
    /// WHY: Named functions used as first-class values need a
    /// `FunctionCallConv::ClosureInvoke` wrapper so the uniform closure-call
    /// path (`lower_call_closure`) can invoke them through the closure pair.
    /// WHEN-OBSOLETE: if the runtime gains a separate fn-pointer ABI.
    /// WHAT-REAL: a native fn-pointer type that doesn't pretend to be a closure.
    #[allow(
        clippy::too_many_lines,
        reason = "exhaustive Terminator match arm in the shim's body \
                  (most recently the `MakeLambdaActor` arm) edges past \
                  the 100-line ceiling without changing the shim's \
                  single responsibility — synthesise a callable wrapper \
                  for the named fn"
    )]
    pub(crate) fn lower_named_fn_invoke_shim(
        &self,
        fn_symbol: &str,
        shim_name: &str,
        key: &crate::model::MirCallableKey,
        param_tys: &[ResolvedTy],
        ret_ty: &ResolvedTy,
    ) -> LoweredFunction {
        let env_ptr_ty = Self::closure_env_pointer_ty(&ResolvedTy::Unit);
        let mut builder = Builder {
            type_classes: self.type_classes.clone(),
            record_field_orders: self.record_field_orders.clone(),
            machine_layout_names: self.machine_layout_names.clone(),
            module_fn_names: self.module_fn_names.clone(),
            module_generic_fn_names: self.module_generic_fn_names.clone(),
            param_ownership: self.param_ownership.clone(),
            // U2 — the proven-foreign ledger CROSSES the frame boundary with the
            // captures. `BindingId`s are globally unique, so carrying the
            // parent's set verbatim is exact: a nested body that re-derives
            // ownership for a captured binding reads the same refusal the
            // enclosing `let` recorded, and a binding the child cannot see is
            // never consulted. Without this the child's ledger is empty and a
            // captured foreign handle re-enters every mint inside the body clean.
            proven_foreign_bindings: self.proven_foreign_bindings.clone(),
            subst: self.subst.clone(),
            call_site_type_args: self.call_site_type_args.clone(),
            supervisor_child_slots: self.supervisor_child_slots.clone(),
            pointer_width: self.pointer_width,
            current_function_symbol: shim_name.to_string(),
            current_callable_key: Some(key.clone()),
            current_function_call_conv: crate::model::FunctionCallConv::ClosureInvoke,
            task_entry_adapter_symbols: self.task_entry_adapter_symbols.clone(),
            // #2648 — synthetic call wrapper (no user match scrutinees), but the
            // provenance context is threaded uniformly: no child builder falls
            // back to the legacy fail-open default.
            call_scrutinee_provenance: self.call_scrutinee_provenance.clone(),
            ..Builder::default()
        };

        // Allocate locals for env_ptr (ignored) and each user argument.
        // locals[0] = env_ptr (ClosureInvoke ABI; never loaded)
        // locals[1..n] = user arguments forwarded to fn_symbol
        let _env_place = builder.alloc_local(env_ptr_ty.clone());
        let mut arg_places = Vec::with_capacity(param_tys.len());
        for ty in param_tys {
            arg_places.push(builder.alloc_local(ty.clone()));
        }

        // Block 0: call the original function, storing return value into ReturnSlot.
        let ret_block_id = builder.alloc_block();
        builder.finish_current_block(Terminator::Call {
            callee: fn_symbol.to_string(),
            authority: crate::model::CallAuthority::default(),
            args: arg_places.clone(),
            dest: Some(Place::ReturnSlot),
            next: ret_block_id,
        });

        // Block 1: return.
        builder.start_block(ret_block_id);
        // Synthesised trampoline: no user body, so the shared pipeline has
        // nothing to splice today. It still finishes through the one seam, so a
        // future pass cannot be silently absent here either.
        let super::FinalizedBody {
            blocks,
            body_statements,
        } = super::finalize_body(
            &mut builder,
            super::BodySeal::Cursor(Terminator::Return),
            super::BodyFinalizeSpec::nested_body(),
        );

        // Build params list: env_ptr_ty first (ClosureInvoke ABI), then user params.
        let mut raw_params = Vec::with_capacity(param_tys.len() + 1);
        raw_params.push(env_ptr_ty);
        raw_params.extend_from_slice(param_tys);
        let mut raw = RawMirFunction {
            name: shim_name.to_string(),
            key: key.clone(),
            return_ty: ret_ty.clone(),
            call_conv: crate::model::FunctionCallConv::ClosureInvoke,
            params: raw_params,
            locals: builder.locals.clone(),
            // Synthesised closure-invoke shim: no faithful user bindings.
            local_names: Vec::new(),
            local_scopes: Vec::new(),
            local_decl_bytes: Vec::new(),
            scope_table: Vec::new(),
            blocks: blocks.clone(),
            decisions: builder.decisions.clone(),
            intrinsic_id: None,
            await_deadline_ns: builder.await_deadline_ns.clone(),
            suspend_kinds: builder.suspend_kinds.clone(),
            lambda_actor_user_param_locals: Vec::new(),
            span: None,
            instr_spans: ::std::collections::BTreeMap::new(),
            source_origin: SourceOrigin::Unknown,
        };

        // Synthetic HirFn for dataflow checking — no HIR params (the shim
        // params are positional locals, not HIR bindings). An empty param list
        // is conservative: no param is pre-seeded as Live, so the checker
        // only sees the Bind/Move generated by the Terminator::Call dest.
        let synthetic_func = HirFn {
            id: hew_hir::ItemId(0),
            node: hew_hir::HirNodeId(0),
            declaration: hew_types::DefId::legacy_reconstruct_from_full_path(shim_name),
            name: shim_name.to_string(),
            type_params: Vec::new(),
            is_generator: false,
            intrinsic_id: None,
            params: Vec::new(),
            return_ty: ret_ty.clone(),
            body: hew_hir::HirBlock {
                node: hew_hir::HirNodeId(0),
                scope: hew_hir::ScopeId(0),
                statements: Vec::new(),
                tail: None,
                ty: ret_ty.clone(),
                span: 0..0,
            },
            span: 0..0,
        };
        let dataflow_result = check_function(&builder, &raw.blocks, &synthetic_func);
        let mut diagnostics: Vec<MirDiagnostic> = dataflow_result
            .checks
            .iter()
            .filter_map(check_to_diagnostic)
            .collect();
        diagnostics.append(&mut builder.diagnostics);
        collect_unknown_type_diagnostics(&synthetic_func, &builder, &mut diagnostics);
        finalize_string_ownership(&mut raw, &mut builder, &dataflow_result);
        finalize_bytes_ownership(&mut raw, &mut builder, &dataflow_result);
        materialize_explicit_goto_edge_carries(&mut raw.blocks, &mut builder);
        raw.instr_spans.clone_from(&builder.instr_spans);
        let cooperate_sites = dataflow::compute_cooperate_sites(&raw.blocks);
        let (checked, elaboration_diagnostics) = seal_checked(
            shim_name.to_string(),
            key.clone(),
            ret_ty.clone(),
            raw.blocks.clone(),
            &raw,
            builder.decisions.clone(),
            dataflow_result.checks.clone(),
            cooperate_sites,
            &builder,
            &body_statements,
        );
        let elaborated = elaborate(&checked);
        diagnostics.extend(elaboration_diagnostics);

        LoweredFunction {
            raw,
            checked,
            elaborated,
            diagnostics,
            generated: builder.generated_functions,
            record_layouts: builder.closure_record_layouts,
        }
    }

    /// Lower an `HirExprKind::SpawnLambdaActor` literal to a MIR
    /// `Place::LambdaActorHandle`. The literal allocates a fresh
    /// local (typed as the actor's `Duplex<Msg, Reply>`) and emits a
    /// `Place::LambdaActorHandle(local_id)` so drop elaboration
    /// selects `DropKind::LambdaActorRelease` — the
    /// stop-on-last-handle-drop protocol with weak-ref body capture
    /// (§5.9 ratification 2).
    ///
    /// Every HIR-resolved capture is forwarded into the function's
    /// `lambda_captures` ledger after proving that the source binding has a MIR
    /// `Place` in `binding_locals`. A capture whose source binding has no backend
    /// slot is a lowering error, never a silently smaller capture set.
    ///
    /// Body lowering (the actor's per-message dispatch) is a
    /// follow-up slice; the MIR shape only needs the handle Place plus
    /// the capture metadata. Codegen rejects `Place::LambdaActorHandle`
    /// today (fail-closed) so a runtime substrate is not required for
    /// the static checks to land.
    #[allow(
        clippy::too_many_lines,
        reason = "the function carries the full spawn-side wiring — \
                  handle alloc, capture validation, body Builder \
                  construction, body HIR lowering, return-shape \
                  rewriting, and `Terminator::MakeLambdaActor` emission. \
                  Splitting it would scatter the body-fn synthesis \
                  contract across helpers without reducing the total \
                  surface area; the structural responsibility is one \
                  spawn site → one MIR body → one make-lambda-actor \
                  terminator and the function's shape mirrors that \
                  responsibility 1:1"
    )]
    pub(crate) fn lower_spawn_lambda_actor(&mut self, expr: &HirExpr) -> Place {
        let HirExprKind::SpawnLambdaActor {
            params,
            reply_ty,
            body,
            captures,
        } = &expr.kind
        else {
            unreachable!("lower_spawn_lambda_actor called on non-SpawnLambdaActor kind");
        };
        // Two paths produce the handle:
        //   - `let <name> = actor |..| { .. }`: the `stmt` Let arm
        //     pre-allocates the binding's slot and stashes its
        //     `LambdaActorHandle` in `pending_lambda_actor_handle`
        //     so the body's Weak self-capture finds a backend slot
        //     for the let-binding. Reuse the pre-allocated handle.
        //   - any non-let position (return-position literal, an
        //     argument, etc.): allocate a fresh local on the fly.
        let handle = if let Some(handle) = self.pending_lambda_actor_handle {
            handle
        } else {
            let local = self.alloc_local(expr.ty.clone());
            let Place::Local(local_id) = local else {
                unreachable!("alloc_local returns Place::Local");
            };
            Place::LambdaActorHandle(local_id)
        };
        for capture in captures {
            // Each captured binding must already have a backend slot in the
            // enclosing function. The forward-bound recursive self capture is
            // the let-binding itself, whose `binding_locals` entry was populated
            // by the `stmt` Let arm before this producer ran.
            if !self.binding_locals.contains_key(&capture.binding) {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::CannotMaterializeClosureCapture {
                        binding: capture.binding,
                        name: capture.name.clone(),
                        site: expr.site,
                    },
                    note: format!(
                        "closure capture `{}` was resolved by HIR but has no MIR backend slot; \
                         refusing to drop the capture from the environment",
                        capture.name
                    ),
                });
                continue;
            }
            let capture_kind = match capture.kind {
                hew_hir::HirCaptureKind::Strong => crate::model::CaptureKind::Strong,
                hew_hir::HirCaptureKind::Weak => crate::model::CaptureKind::Weak,
            };
            self.lambda_captures.push(LambdaCapture {
                actor_handle: handle,
                captured: capture.binding,
                name: capture.name.clone(),
                capture_kind,
            });
        }

        // ── Body fn synthesis (M2 spawn-side) ──
        //
        // Mirrors `lower_gen_block`: mint a deterministic body-fn symbol,
        // synthesize a child `Builder` that lowers the lambda body under the
        // runtime ABI signature, push the LoweredFunction into
        // `self.generated_functions`, and emit `Terminator::MakeLambdaActor`
        // so codegen calls `hew_lambda_actor_new(...)` and stores the
        // returned handle into the enclosing function's
        // `LambdaActorHandle` slot.
        //
        // Scope:
        //   - Captures: materialised as a heap-boxed env record passed as
        //     the runtime state pointer (see the capture-env synthesis
        //     below). A capture-free lambda passes null state and the
        //     shared no-op state-drop stub.
        //   - Single user param (0 or 1): the codegen prologue copies
        //     `sizeof(user_ty)` bytes from `msg_ptr` into the param alloca.
        //     Multi-param bodies unpack each param at its packed-record
        //     field offset (see the LambdaActorBody prologue in codegen).
        //   - Tell shape (`reply_ty == Unit`): codegen returns i32 0
        //     unconditionally.
        //   - Ask shape: codegen serialises the body's ReturnSlot value into a
        //     fresh reply buffer via `hew_lambda_body_alloc_reply_buf`, stores
        //     the buf pointer + length into `*reply_out` / `*reply_len_out`,
        //     and returns i32 0.
        //
        // Drop safety (CLAUDE.md §1): the handle's release is scheduled at
        // scope exit by the enclosing function's replay-derived drop plan via
        // `place_aware_drop_fn` → `hew_lambda_actor_release`. The state-drop
        // no-op stub is invoked exactly once by the runtime at actor
        // shutdown, releasing nothing for the no-capture MVP.
        // Multi-param bodies ride the packed-args anonymous-record wire: the
        // call site (`lower_lambda_actor_call`) packs the N args into one
        // record and the body prologue unpacks each param at its natural
        // field offset from `msg_ptr` (the codegen LambdaActorBody prologue
        // rebuilds the identical struct from the user-param local types).

        let lambda_id = self.next_closure_id;
        self.next_closure_id = self
            .next_closure_id
            .checked_add(1)
            .expect("lambda id overflow — closure id counter exhausted");
        let owner = Self::sanitize_symbol_component(&self.current_function_symbol);
        let body_name = format!("__hew_lambda_body_{owner}_{lambda_id}");
        let body_key =
            self.mint_synthesized_child_key(crate::model::SynthesizedCallable::LambdaActorBody);

        // ── Capture-env synthesis ──
        //
        // Captures materialise as a synthetic env record in the enclosing
        // frame: one field per capture, in capture order. The supported
        // field classes are explicit and everything else fails closed:
        //
        //   - `Weak` self-handle (the forward-bound let-binding for
        //     recursion, §5.9 ratification 2): the field is OMITTED from
        //     the `RecordInit` — the handle does not exist until
        //     `hew_lambda_actor_new` returns. Codegen nulls the field at
        //     box time and back-fills it with the DOWNGRADED weak handle
        //     after construction; the env drop releases it via
        //     `hew_lambda_actor_weak_drop`. Holding a weak (not strong)
        //     self reference preserves stop-on-last-external-handle-drop.
        //   - Strong BitCopy scalar: the field store copies the value;
        //     the caller's binding stays live and untouched.
        //   - Strong pid (`LocalPid`): a BitCopy alias of an
        //     opaque identity reference with no drop glue. The field store
        //     copies the handle word; the caller's binding stays live and
        //     the env field gets no retain and no release — the actor's
        //     lifetime is runtime-owned.
        //   - Strong `string`: the field store copies the HANDLE bytes
        //     (an alias); codegen replaces the heap-env field with an
        //     independent `hew_string_clone` at box time, so the env owns
        //     its copy and the caller's binding remains the owner of the
        //     original. The env drop releases the clone via
        //     `hew_string_drop` exactly once at actor shutdown.
        //   - Strong `LambdaPid`: as with string, the frame store is only an
        //     alias. Codegen replaces it with `hew_lambda_actor_clone` in the
        //     boxed env, and the env drop releases that independent strong
        //     handle exactly once with `hew_lambda_actor_release`.
        //   - Anything else (Vec, HashMap, records, owned handles):
        //     `CannotMaterializeClosureCapture` — no silent shallow copy
        //     of an owned aggregate across the actor boundary.
        //
        // The env record outlives the spawning frame: codegen heap-boxes
        // it (`malloc(sizeof(env))` + `memcpy`) and passes the heap
        // pointer as `hew_lambda_actor_new`'s `state` arg. The body reads
        // captures back through the state pointer (`ClosureEnvFieldLoad`
        // on Local(0)); the synthesized `state_drop_fn` is the single
        // teardown owner (field drops, then `free`), called exactly once
        // by the runtime after the dispatch loop stops.
        let mut env_place: Option<Place> = None;
        let mut env_ty: Option<ResolvedTy> = None;
        let mut env_field_drops: Vec<crate::model::LambdaEnvFieldDrop> = Vec::new();
        let mut env_capture_field_tys: Vec<ResolvedTy> = Vec::new();
        let mut weak_capture_bindings: std::collections::HashSet<hew_hir::BindingId> =
            std::collections::HashSet::new();
        if !captures.is_empty() {
            let mut field_tys: Vec<ResolvedTy> = Vec::with_capacity(captures.len());
            let mut init_fields: Vec<(FieldOffset, Place)> = Vec::new();
            for (idx, capture) in captures.iter().enumerate() {
                let offset =
                    FieldOffset(u32::try_from(idx).expect("lambda capture count exceeds u32::MAX"));
                // `HirLambdaCapture` carries no type; the captured binding's
                // MIR slot type is the authority (for the weak self-capture
                // the slot is the handle local, typed `Duplex<Msg, Reply>`).
                let Some(capture_ty) = self
                    .binding_locals
                    .get(&capture.binding)
                    .and_then(|place| base_local(*place))
                    .and_then(|local| self.locals.get(local as usize))
                    .cloned()
                else {
                    // Already diagnosed by the backend-slot loop above.
                    return handle;
                };
                let drop_class = match (&capture.kind, &capture_ty) {
                    (hew_hir::HirCaptureKind::Weak, _) => {
                        weak_capture_bindings.insert(capture.binding);
                        crate::model::LambdaEnvFieldDrop::WeakSelfHandle
                    }
                    (hew_hir::HirCaptureKind::Strong, ResolvedTy::String) => {
                        crate::model::LambdaEnvFieldDrop::String
                    }
                    (
                        hew_hir::HirCaptureKind::Strong,
                        ResolvedTy::Named {
                            builtin: Some(BuiltinType::LambdaPid),
                            ..
                        },
                    ) => crate::model::LambdaEnvFieldDrop::StrongLambdaActorHandle,
                    // BitCopy scalars and pids share the no-drop class. A pid
                    // is an opaque identity reference with no drop glue (its
                    // drop is a codegen no-op — see the double-free origin
                    // analysis on `validate_lambda_captures`): capturing one
                    // is a BitCopy alias, ownership-identical to passing it
                    // to a fn by value. The actor's lifetime is runtime-owned,
                    // so the env field needs no retain and no release; a send
                    // after the target stops is the same pre-existing hazard
                    // class as any post-stop pid use — `hew_actor_send`'s
                    // liveness contract treats a dead target as a normal
                    // non-fault outcome.
                    (
                        hew_hir::HirCaptureKind::Strong,
                        ResolvedTy::I64
                        | ResolvedTy::I32
                        | ResolvedTy::I16
                        | ResolvedTy::I8
                        | ResolvedTy::U64
                        | ResolvedTy::U32
                        | ResolvedTy::U16
                        | ResolvedTy::U8
                        | ResolvedTy::F64
                        | ResolvedTy::F32
                        | ResolvedTy::Bool
                        | ResolvedTy::Char
                        | ResolvedTy::Named {
                            builtin: Some(BuiltinType::LocalPid),
                            ..
                        },
                    ) => crate::model::LambdaEnvFieldDrop::None,
                    (hew_hir::HirCaptureKind::Strong, other) => {
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::CannotMaterializeClosureCapture {
                                binding: capture.binding,
                                name: capture.name.clone(),
                                site: expr.site,
                            },
                            note: format!(
                                "lambda-actor capture `{}` has type `{}`, which the \
                                 capture env cannot carry yet: only BitCopy scalars, \
                                 `string`, actor pids, `LambdaPid`, and the weak self-handle have \
                                 an ownership protocol across the actor boundary. A \
                                 shallow byte copy of an owned aggregate would alias \
                                 its heap and double-free at shutdown — fail closed \
                                 instead.",
                                capture.name,
                                other.user_facing()
                            ),
                        });
                        return handle;
                    }
                };
                if !matches!(drop_class, crate::model::LambdaEnvFieldDrop::WeakSelfHandle) {
                    let Some(&src) = self.binding_locals.get(&capture.binding) else {
                        // Already diagnosed by the backend-slot loop above.
                        return handle;
                    };
                    init_fields.push((offset, src));
                }
                field_tys.push(capture_ty);
                env_field_drops.push(drop_class);
            }
            let env_name = format!("__hew_lambda_env_{owner}_{lambda_id}");
            let env_resolved_ty = ResolvedTy::Named {
                name: env_name.clone(),
                args: vec![],
                builtin: None,
                is_opaque: false,
            };
            env_capture_field_tys.clone_from(&field_tys);
            self.closure_record_layouts
                .push(crate::model::RecordLayout {
                    name: env_name,
                    field_tys,
                    // Compiler-internal lambda-env record: positional `-g` names.
                    field_names: Vec::new(),
                });
            let dest = self.alloc_local(env_resolved_ty.clone());
            self.push_instr(Instr::RecordInit {
                ty: env_resolved_ty.clone(),
                fields: init_fields,
                dest,
            });
            env_place = Some(dest);
            env_ty = Some(env_resolved_ty);
        }
        let state_drop_name = if env_place.is_some() {
            format!("__hew_lambda_env_drop_{owner}_{lambda_id}")
        } else {
            "__hew_lambda_state_drop_noop".to_string()
        };

        let shape = if matches!(reply_ty, ResolvedTy::Unit) {
            crate::model::LambdaActorShape::Tell
        } else {
            crate::model::LambdaActorShape::Ask
        };
        let shape_disc: i32 = match shape {
            crate::model::LambdaActorShape::Tell => 0,
            crate::model::LambdaActorShape::Ask => 1,
        };

        // ── Build child Builder for the body ──
        // Shares the parent's module tables (`child_builder_tables`) so a
        // lambda body that sends to a captured pid resolves the target
        // actor's `actor_method_info` exactly as the parent would.
        let mut body_builder = Builder {
            current_function_symbol: body_name.clone(),
            current_callable_key: Some(body_key.clone()),
            current_function_call_conv: crate::model::FunctionCallConv::LambdaActorBody(shape),
            ..self.child_builder_tables()
        };

        // Locals 0..=4: the five runtime ABI parameter slots. Their types
        // mirror `HewLambdaActorBody`'s C signature: (state ptr, msg ptr,
        // msg_len i64, reply_out ptr-of-ptr, reply_len_out ptr-of-usize).
        // The codegen parameter prologue stores LLVM args into these
        // allocas in order.
        let ptr_ty = ResolvedTy::Pointer {
            is_mutable: true,
            pointee: Box::new(ResolvedTy::Unit),
        };
        body_builder.locals.push(ptr_ty.clone()); // Local(0): state
        body_builder.locals.push(ptr_ty.clone()); // Local(1): msg ptr
        body_builder.locals.push(ResolvedTy::I64); // Local(2): msg_len
        body_builder.locals.push(ptr_ty.clone()); // Local(3): reply_out
        body_builder.locals.push(ptr_ty.clone()); // Local(4): reply_len_out

        // Allocate user-param locals AFTER the ABI slots and register them
        // in `binding_locals` so body HIR `BindingRef`s resolve. Track the
        // Local ids in the side-channel so codegen knows which slots need
        // the msg-deserialise prologue fragment.
        let mut user_param_local_ids: Vec<u32> = Vec::with_capacity(params.len());
        for param in params {
            let slot = body_builder.alloc_local(param.ty.clone());
            let Place::Local(slot_id) = slot else {
                unreachable!("alloc_local returns Place::Local");
            };
            body_builder.binding_locals.insert(param.id, slot);
            body_builder.seed_fn_param_provenance(param);
            user_param_local_ids.push(slot_id);
        }

        // Register each capture as a body-side env-field source: the body's
        // `BindingRef`s to a captured binding lower to `ClosureEnvFieldLoad`
        // through Local(0) — the runtime state pointer, which IS the boxed
        // env pointer when captures are present. Mirrors the closure-shim
        // env discipline at `lower_closure_shim`. Loads are read-only views
        // into the env; only the synthesized `state_drop_fn` frees env
        // fields (`ffi-ownership-contracts`).
        if let Some(env_resolved_ty) = &env_ty {
            for (idx, capture) in captures.iter().enumerate() {
                body_builder.capture_env_sources.insert(
                    capture.binding,
                    CaptureEnvSource {
                        env: Place::Local(0),
                        env_ty: env_resolved_ty.clone(),
                        field_offset: FieldOffset(
                            u32::try_from(idx).expect("lambda capture count exceeds u32::MAX"),
                        ),
                        ty: env_capture_field_tys[idx].clone(),
                    },
                );
                if self
                    .closure_pair_env_may_be_nonnull
                    .contains(&capture.binding)
                {
                    body_builder
                        .closure_pair_env_may_be_nonnull
                        .insert(capture.binding);
                }
                if self.closure_pair_null_env.contains(&capture.binding) {
                    body_builder.closure_pair_null_env.insert(capture.binding);
                }
            }
            body_builder.weak_lambda_capture_bindings = weak_capture_bindings;
        }

        // Lower the lambda body. The body is a single HirExpr (an arrow
        // body or block); reuse `lower_value` so all expression shapes —
        // BlockExpr, Call, BinaryOp, etc. — go through the standard path.
        //
        // #2301 -- same pre-pass gap as `lower_closure_shim`/`lower_gen_block`:
        // this lambda-actor body lowers via its own fresh `body_builder`
        // (built via `child_builder_tables`, which does not carry
        // `prepass_consumed_bindings`/`prepass_reassigned_bindings` either), so
        // without this call a `var` local to the lambda body that is consumed
        // on one control-flow arm and overwritten on a sibling arm silently
        // leaks its prior value instead of getting the path-sensitive release
        // a byte-identical top-level function body would.
        body_builder.collect_expr_prepass_facts(body);
        let tail_place = body_builder.lower_value(body);

        // Shape-driven return slot wiring:
        //   - Ask: move the body's tail value into ReturnSlot so codegen's
        //     LambdaActorBody Return epilogue can serialise it into the
        //     reply buffer.
        //   - Tell: discard the tail value; codegen returns i32 0
        //     unconditionally.
        if matches!(shape, crate::model::LambdaActorShape::Ask) {
            if let Some(src) = tail_place {
                body_builder.push_instr(Instr::Move {
                    dest: Place::ReturnSlot,
                    src,
                });
            }
        }

        // The lambda-actor handler body is a user body: it finishes through the
        // one seam, so a pass added there reaches it without a second
        // registration. Its spec withholds exactly one pair — the nested
        // fresh-temp releases, whose precondition this body does not meet
        // (measured on `BodyFinalizeSpec::nested_fresh_temp_releases`).
        let super::FinalizedBody {
            blocks: body_blocks,
            body_statements: nested_body_statements,
        } = super::finalize_body(
            &mut body_builder,
            super::BodySeal::Cursor(Terminator::Return),
            super::BodyFinalizeSpec::lambda_actor_body(),
        );
        let body_locals = body_builder.locals.clone();
        let body_user_return_ty = if matches!(shape, crate::model::LambdaActorShape::Ask) {
            reply_ty.clone()
        } else {
            ResolvedTy::Unit
        };

        // The body's MIR `return_ty` carries the USER reply type (Ask) or
        // `Unit` (Tell). Codegen consults this to pick the reply
        // serialisation width; the LLVM return type is always `i32`
        // (the status code) — see codegen's LambdaActorBody arm.
        let mut raw = RawMirFunction {
            name: body_name.clone(),
            key: body_key.clone(),
            return_ty: body_user_return_ty.clone(),
            call_conv: crate::model::FunctionCallConv::LambdaActorBody(shape),
            params: vec![
                ptr_ty.clone(),  // state
                ptr_ty.clone(),  // msg ptr
                ResolvedTy::I64, // msg_len
                ptr_ty.clone(),  // reply_out
                ptr_ty.clone(),  // reply_len_out
            ],
            locals: body_locals.clone(),
            // Synthesised lambda-actor body (runtime ABI shape): no `-g` DIEs.
            local_names: Vec::new(),
            local_scopes: Vec::new(),
            local_decl_bytes: Vec::new(),
            scope_table: Vec::new(),
            blocks: body_blocks.clone(),
            decisions: body_builder.decisions.clone(),
            intrinsic_id: None,
            await_deadline_ns: body_builder.await_deadline_ns.clone(),
            suspend_kinds: body_builder.suspend_kinds.clone(),
            lambda_actor_user_param_locals: user_param_local_ids.clone(),
            span: None,
            instr_spans: ::std::collections::BTreeMap::new(),
            source_origin: SourceOrigin::Unknown,
        };

        // Synthetic HirFn shell for `check_function` (mirrors `lower_gen_block`).
        let synthetic_fn = HirFn {
            id: hew_hir::ItemId(0),
            node: hew_hir::HirNodeId(0),
            declaration: hew_types::DefId::legacy_reconstruct_from_full_path(body_name.clone()),
            name: body_name.clone(),
            type_params: Vec::new(),
            is_generator: false,
            intrinsic_id: None,
            // Seed the user-visible params into the synthetic HirFn so the
            // dataflow checker treats them as Live at body entry — without
            // this seed every `BindingRef` to a user param reads as
            // "uninitialised before use" (the body has no `let` for the
            // param; it arrives via the runtime ABI msg buffer). The MIR
            // user-param locals were inserted into `binding_locals` above,
            // so this seed lines up the HIR-binding-keyed checker with the
            // MIR-local-keyed value sources.
            params: params.clone(),
            return_ty: body_user_return_ty.clone(),
            body: hew_hir::HirBlock {
                node: hew_hir::HirNodeId(0),
                scope: hew_hir::ScopeId(0),
                statements: Vec::new(),
                tail: None,
                ty: body_user_return_ty.clone(),
                span: expr.span.clone(),
            },
            span: expr.span.clone(),
        };

        let dataflow_result = check_function(&body_builder, &raw.blocks, &synthetic_fn);
        let mut body_diagnostics: Vec<MirDiagnostic> = dataflow_result
            .checks
            .iter()
            .filter_map(check_to_diagnostic)
            .collect();
        body_diagnostics.append(&mut body_builder.diagnostics);
        collect_unknown_type_diagnostics(&synthetic_fn, &body_builder, &mut body_diagnostics);
        finalize_string_ownership(&mut raw, &mut body_builder, &dataflow_result);
        finalize_bytes_ownership(&mut raw, &mut body_builder, &dataflow_result);
        materialize_explicit_goto_edge_carries(&mut raw.blocks, &mut body_builder);
        raw.instr_spans.clone_from(&body_builder.instr_spans);

        let cooperate_sites = dataflow::compute_cooperate_sites(&raw.blocks);
        let (checked, elaboration_diagnostics) = seal_checked(
            body_name.clone(),
            body_key.clone(),
            body_user_return_ty.clone(),
            raw.blocks.clone(),
            &raw,
            body_builder.decisions.clone(),
            dataflow_result.checks.clone(),
            cooperate_sites,
            &body_builder,
            &nested_body_statements,
        );
        let elaborated = elaborate(&checked);
        body_diagnostics.extend(elaboration_diagnostics);

        let body_lowered = LoweredFunction {
            raw,
            checked,
            elaborated,
            diagnostics: body_diagnostics,
            generated: body_builder.generated_functions,
            record_layouts: body_builder.closure_record_layouts,
        };
        self.generated_functions.push(body_lowered);

        // Emit `Terminator::MakeLambdaActor` so codegen wires
        // `hew_lambda_actor_new(64, shape, &body_fn, state, &state_drop_fn)`
        // and stores the resulting `*mut HewLambdaActorHandle` into
        // `handle`. `state` is null for a capture-free lambda; with
        // captures it is the heap-boxed env codegen builds from `env`.
        // The 64-slot default mailbox is a substrate constant
        // (mirrors the cooperate-site default in `hew-runtime`); a future
        // surface knob (`actor capacity 128 |..| { ... }`) can override
        // this without touching codegen.
        let next = self.alloc_block();
        self.finish_current_block(Terminator::MakeLambdaActor {
            dest: handle,
            body_fn: body_name,
            state_drop_fn: state_drop_name,
            shape: shape_disc,
            mailbox_capacity: 64,
            next,
            env: env_place,
            env_field_drops,
        });
        self.start_block(next);

        handle
    }

    /// Lower a call-syntax dispatch through a lambda-actor handle:
    /// `let h = actor |...| { ... }; h(msg)`.
    ///
    /// The intercept in `HirExprKind::Call` routes here when the callee
    /// is a `BindingRef` whose MIR Place is `Place::LambdaActorHandle(N)`.
    /// This is the M2 substrate seam that replaces both the
    /// `module_fn_names` collision miscompile (a user binding named `log`
    /// shadowing the `f64 -> f64` math builtin) and the indirect-call NYI
    /// arm (`dbl(5)`, `fib(10)`) — see the comment at the call site.
    ///
    /// Shape dispatch (tell vs ask) is driven by the callee's HIR type
    /// `Duplex<Msg, Reply>`: `Reply = Unit` → tell (`hew_lambda_actor_send`),
    /// otherwise → ask (`hew_lambda_actor_ask`). Mirrors the runtime
    /// `LambdaShape::{Tell,Ask}` enum (`hew-runtime/src/lambda_actor.rs`).
    ///
    /// Scope: a single argument rides the single-vertebra wire (8-byte
    /// zero-padded scalar spill, or the aggregate `(ptr, sizeof)` path
    /// codegen selects for struct-typed messages — mirrors the
    /// `hew_duplex_send` shape); two or more arguments pack into the
    /// same anonymous-record payload declared-actor multi-arg sends use
    /// (`lower_packed_args_payload`). Body-side self-sends through the
    /// captured weak handle dispatch via `hew_lambda_actor_weak_send`;
    /// a weak self-ask fails closed (no weak-ask runtime entry).
    /// LESSONS: substrate-over-surface, boundary-fail-closed.
    #[allow(
        clippy::too_many_lines,
        reason = "tell/ask × strong/weak-capture dispatch keeps the wire selection and \
                  its fail-closed arms in one body; splitting would scatter the single \
                  branch authority over the payload mechanism"
    )]
    pub(crate) fn lower_lambda_actor_call(
        &mut self,
        callee: &HirExpr,
        args: &[HirExpr],
        expr_ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        // Weak-capture dispatch: a body-side self-send through the captured
        // weak handle uses the weak ABI (`hew_lambda_actor_weak_send`). The
        // discriminator is the callee binding's membership in the body
        // builder's weak-capture set — the env field holds a
        // `*mut HewLambdaActorWeakHandle`, which the strong-send entry must
        // never receive (distinct repr; the runtime upgrade lives behind
        // the weak entry).
        let via_weak_capture = matches!(
            &callee.kind,
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(binding_id),
                ..
            } if self.weak_lambda_capture_bindings.contains(binding_id)
        );
        let handle = self.lower_value(callee)?;
        let handle_is_env_load = matches!(
            &callee.kind,
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(binding_id),
                ..
            } if self.capture_env_sources.contains_key(binding_id)
        );
        if !matches!(handle, Place::LambdaActorHandle(_)) && !handle_is_env_load {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::UnsupportedNode {
                    reason: format!(
                        "lambda-actor call routed to non-LambdaActorHandle place {handle:?}"
                    ),
                },
                note: "lower_lambda_actor_call expects the callee binding's Place to be \
                       Place::LambdaActorHandle or a captured-env Duplex field load; the \
                       gate in HirExprKind::Call must have drifted from the place_aware \
                       lookup"
                    .to_string(),
            });
            return None;
        }

        if args.is_empty() {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "lambda-actor call with arity 0".to_string(),
                    site,
                },
                note: "a zero-param lambda actor is dispatched with a unit message \
                       (`handle(())`); bare zero-argument dispatch is a follow-on slice"
                    .to_string(),
            });
            return None;
        }

        let reply_ty = match &callee.ty {
            ResolvedTy::Named { args: ty_args, .. } if ty_args.len() == 2 => ty_args[1].clone(),
            other => {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::UnsupportedNode {
                        reason: format!("lambda-actor callee has non-Duplex<S,R> type `{other:?}`"),
                    },
                    note: "expected `Duplex<Msg, Reply>` for lambda-actor dispatch".to_string(),
                });
                return None;
            }
        };
        let is_ask = !matches!(reply_ty, ResolvedTy::Unit);

        // Payload mechanism convergence: a single argument rides the existing
        // single-vertebra wire (8-byte zero-padded spill for scalars, or the
        // aggregate (ptr, sizeof) path codegen selects for struct-typed
        // messages); two or more arguments pack into the same anonymous-record
        // payload declared-actor multi-arg sends use.
        let msg_place = if args.len() == 1 {
            self.lower_value(&args[0])?
        } else {
            self.lower_packed_args_payload(args, site)?
        };

        // Wire byte-length operand for the SCALAR single-vertebra wire only:
        // the fixed 8-byte zero-padded slot (mirrors hew_duplex_send). MIR has
        // no sizeof expression, so for an aggregate payload (the packed-args
        // record, or a struct-typed single message) codegen is the single size
        // authority: it branches on the message Place's LLVM type and derives
        // (ptr, sizeof(aggregate)) itself, never consuming this operand on
        // that path. There is exactly one branch predicate (codegen's
        // struct-type check), so the two ends cannot drift.
        let len_local = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::ConstI64 {
            dest: len_local,
            value: 8,
        });

        // The call's HIR type is `Result<Reply, SendError>` (tell) or
        // `Result<Reply, AskError>` (ask). Allocate the dest so let-
        // binding consumers see a Place; the slot is left uninitialised
        // in this MVP (the LLVM verifier accepts uninit allocas; `hew
        // run` of a real result-bearing call needs a follow-on slice
        // that materialises Ok(_) from the runtime i32 status).
        let dest = self.alloc_local(expr_ty.clone());

        // A self-ask through the weak capture has no weak-ask runtime entry
        // (`hew_lambda_actor_weak_send` is tell-only); refuse rather than
        // upgrade-and-strong-ask behind the caller's back, which would keep
        // the actor alive past external refcount zero for the ask duration.
        if via_weak_capture && is_ask {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "recursive self-ask through a lambda-actor weak capture".to_string(),
                    site,
                },
                note: "the weak self-handle supports fire-and-forget self-sends \
                       (`hew_lambda_actor_weak_send`); an ask-shaped self-dispatch \
                       needs a weak-ask runtime entry with upgrade semantics"
                    .to_string(),
            });
            return None;
        }

        let call = if is_ask {
            // Out-params: reply_out is `*mut *mut u8` (pointer-sized
            // slot), reply_len_out is `*mut usize`. We allocate a Pointer
            // local for reply_out (alloca holds a ptr value, 8 bytes on
            // 64-bit) and an I64 local for reply_len_out (alloca holds an
            // i64). Codegen passes the alloca addresses as the first 5
            // args to the runtime call. The 6th arg is codegen-only:
            // an `AskError` local that the codegen branches into on the
            // call's non-zero (Err) status — the AskError tag is stored
            // here and `emit_result_err(dest, error_dest)` materialises
            // `Result::Err(askerror)` without the unconditional null
            // reply-pointer deref the previous codegen did. Mirrors the
            // `SuspendingAsk` precedent at `lower.rs:15340`.
            // After the ask completes:
            //   - On Ok (status 0): codegen loads the reply bytes into
            //     `dest`'s Result::Ok variant and frees the libc-
            //     allocated reply buffer with `hew_reply_payload_free`.
            //   - On Err (status != 0): codegen maps the SendError
            //     discriminant to an AskError variant, stores into
            //     `error_dest`'s MachineTag slot, and binds
            //     `Result::Err(error_dest)` into `dest`.
            // Marking the runtime call's `dest` as `Some(dest)` keeps
            // the dataflow ledger consistent (the dest binding becomes
            // Live at the call), so the replay-derived exit plan releases
            // it on scope exit if it carries owned-handle resources.
            let reply_ptr_slot = self.alloc_local(ResolvedTy::Pointer {
                is_mutable: true,
                pointee: Box::new(ResolvedTy::Unit),
            });
            let reply_len_slot = self.alloc_local(ResolvedTy::I64);
            let error_dest = self.alloc_local(
                hew_types::builtin_enums::resolved_monomorphic_builtin_enum_ty("AskError")
                    .expect("generated builtin enum catalog must contain AskError"),
            );
            crate::model::RuntimeCall::new(
                "hew_lambda_actor_ask",
                vec![
                    handle,
                    msg_place,
                    len_local,
                    reply_ptr_slot,
                    reply_len_slot,
                    error_dest,
                ],
                Some(dest),
            )
        } else {
            crate::model::RuntimeCall::new(
                if via_weak_capture {
                    "hew_lambda_actor_weak_send"
                } else {
                    "hew_lambda_actor_send"
                },
                vec![handle, msg_place, len_local],
                None,
            )
        }
        .expect("hew_lambda_actor_{send,weak_send,ask} are on the M2 runtime allowlist");

        self.push_instr(Instr::CallRuntimeAbi(call));
        Some(dest)
    }

    /// Lower `HirExprKind::GenBlock { body, yield_ty, return_ty, captures }`
    /// to a MIR generator shell.
    ///
    /// The enclosing function receives a `Place::Local` typed as
    /// `Generator<Y, R>` — this is a placeholder for S3b, which synthesises
    /// the state-record struct and fills in cross-yield live fields.
    ///
    /// The gen-block body is lowered into a separate synthetic function
    /// (name: `__hew_gen_body_{owner}_{id}`) and registered in
    /// `self.generated_functions` so `lower_hir_module` surfaces it in the
    /// `IrPipeline`. Inside the body, `HirExprKind::Yield { value }` lowers
    /// to `Terminator::Yield { value: <place>, next: <resume_block> }`.
    ///
    /// # S3b cross-yield liveness stub
    /// This function does NOT compute which locals are live across yield sites.
    /// S3b adds the cross-yield liveness pass that lifts live locals to
    /// state-record fields. The generated body's blocks contain the correct
    /// CFG shape (yield terminators, resume blocks) for S3b to consume without
    /// any re-lowering.
    ///
    /// # Fail-closed invariant
    /// If the HIR `yield_ty` or `return_ty` is unreachable in the current
    /// checker state, the lowering succeeds structurally (the placeholder
    /// place still has type `Generator<Y, R>`) and S3b/S4 detect the
    /// inconsistency when they interrogate the state-record.
    #[allow(
        clippy::too_many_lines,
        reason = "gen-block lowering keeps body-builder construction, S3b synthesis-pass \
                  invocation, raw/checked/elaborated triple assembly, and dataflow \
                  dispatch in one routine so the surface a regression touches is \
                  contiguous; helper-splitting would scatter the gen-body invariants \
                  across functions that each only see half of the contract"
    )]
    pub(crate) fn lower_gen_block(
        &mut self,
        expr: &HirExpr,
        body: &HirBlock,
        _yield_ty: &ResolvedTy,
        return_ty: &ResolvedTy,
        captures: &[hew_hir::HirGenCapture],
    ) -> Place {
        // Mint a unique generator-body function name via the shared closure
        // id counter so multiple gen blocks in one function do not collide.
        let gen_id = self.next_closure_id;
        self.next_closure_id = self
            .next_closure_id
            .checked_add(1)
            .expect("generator id overflow — closure id counter exhausted");
        let owner = Self::sanitize_symbol_component(&self.current_function_symbol);
        // Minted from the shared prefix const so the coroutine classifier
        // (`RawMirFunction::coroutine_facts`) and this minting site can never
        // drift onto different spellings.
        let body_name = format!("{}{owner}_{gen_id}", crate::model::GEN_BODY_PREFIX);
        let body_key =
            self.mint_synthesized_child_key(crate::model::SynthesizedCallable::GeneratorBody);

        // Allocate a place in the ENCLOSING function typed as
        // `Generator<yield_ty, return_ty>`.  S3b will replace this with the
        // real state-record type; for S3a it is purely a checker-authority
        // token so the binding in the enclosing scope has the right type.
        let gen_place = self.alloc_local(expr.ty.clone());

        // ── Capture-env synthesis (mirrors `lower_spawn_lambda_actor`) ──
        //
        // The generator body is a coro ramp (`__hew_gen_body_*`); its only
        // window onto the enclosing frame is the env record
        // `Terminator::MakeGenerator` heap-copies at construction (so it
        // outlives this constructing frame — the body reads it across
        // suspends). Each free variable — a `gen fn`'s formal parameters, a
        // `gen { }` block's captured outer locals (HIR computed this set, in
        // `captures`) — becomes one field of a synthetic env record built HERE
        // in the enclosing frame, in capture order. The body reads them back
        // through `Local(1)` (the env-pointer param; `Local(0)` is the
        // out-pointer) via `ClosureEnvFieldLoad` (registered below).
        //
        // SCOPE / FAIL-CLOSED: `gen_env_capture_admissible` governs what may be
        // snapshotted into the heap env. `Terminator::MakeGenerator` shallow-
        // seeds the record, replaces borrowed owned fields with semantic
        // clones, preserves a mailbox-delivered receive-handler parameter as
        // a transferred owner, and plants the reverse-order payload-drop
        // thunk. Admitted shapes:
        //   * clone-total structural values (String/Bytes/Rc/Weak, supported
        //     collections, tuples/arrays, records, and enums);
        //   * `BitCopy` scalars;
        //   * `ResolvedTy::Function` — a bare named-fn reference whose runtime
        //     env word is null;
        //   * `ResolvedTy::Closure` with no captures — same null-env guarantee.
        // Opaque/resource/IO handles, trait objects, and closure pairs with a
        // non-null environment remain rejected because no total clone exists.
        let mut env_place: Option<Place> = None;
        let mut env_ty: Option<ResolvedTy> = None;
        let mut env_capture_field_tys: Vec<ResolvedTy> = Vec::new();
        let mut env_field_plans: Vec<GeneratorEnvFieldPlan> = Vec::new();
        let mut env_moved_bindings: Vec<BindingId> = Vec::new();
        // Capture bindings rejected below as inadmissible to the owned env. Each
        // gets a root `NotYetImplemented`; the body sub-builder reads this set
        // to suppress the downstream `InitialisedBeforeUse`/`UnresolvedPlace`
        // cascade for the same bindings (only the root rejection is actionable).
        let mut poisoned_captures: HashSet<BindingId> = HashSet::new();
        if !captures.is_empty() {
            let mut field_tys: Vec<ResolvedTy> = Vec::with_capacity(captures.len());
            let mut init_fields: Vec<(FieldOffset, Place)> = Vec::new();
            let mut all_materialisable = true;
            for (idx, capture) in captures.iter().enumerate() {
                let offset =
                    FieldOffset(u32::try_from(idx).expect("gen capture count exceeds u32::MAX"));
                // The captured binding's MIR slot in the ENCLOSING frame is the
                // authority for both the value source and the field type. A
                // `Local` capture (a `gen fn`'s own params, a `gen {}`
                // block's outer locals, or a `receive gen fn` handler param)
                // already has a slot in `binding_locals`. An `ActorStateField`
                // capture has none — actor state fields resolve by NAME
                // through `current_actor_state_fields`, not by binding id —
                // so its value is snapshotted into a fresh shell local via
                // `ActorStateFieldLoad` first (state is snapshot-isolated: a
                // point-in-time copy taken now, in the enclosing actor-handler frame where state
                // is addressable, never a live reference); the resulting slot
                // then feeds `RecordInit` exactly like a `Local` capture's.
                let (slot, capture_ty) = match capture.source {
                    hew_hir::HirGenCaptureSource::Local => {
                        let slot = self.binding_locals.get(&capture.binding).copied();
                        let ty = slot
                            .and_then(base_local)
                            .and_then(|local| self.locals.get(local as usize))
                            .cloned();
                        (slot, ty)
                    }
                    hew_hir::HirGenCaptureSource::ActorStateField => {
                        match self.current_actor_state_fields.get(&capture.name).cloned() {
                            Some((field_offset, ty)) => {
                                let dest = self.alloc_local(ty.clone());
                                // P0 #2432 — fail-closed default; this snapshot is a
                                // RecordInit field a moment later (whole-value escape into
                                // the gen-body's captured env), so the classifier keeps it
                                // `Owned` regardless — recorded explicitly for consistency
                                // with the other two construct sites.
                                self.instructions.push(Instr::ActorStateFieldLoad {
                                    field_offset,
                                    dest,
                                    mode: ActorStateLoadMode::Owned,
                                });
                                (Some(dest), Some(ty))
                            }
                            None => (None, None),
                        }
                    }
                };
                let fn_env_provenance_unproven = matches!(capture_ty, Some(ResolvedTy::Function { .. }))
                    && self
                        .closure_pair_env_may_be_nonnull
                        .contains(&capture.binding)
                    // A source `gen fn` shell's own formal parameters are
                    // checked at every generator-constructor call. No other
                    // anonymous-gen capture has that external provenance gate.
                    && !(self.generator_shell_call_gate.is_some()
                        && self.closure_pair_param_owned.contains(&capture.binding));
                let capture_field_plan = capture_ty
                    .as_ref()
                    .and_then(|ty| self.gen_env_capture_field_plan(ty));
                match (slot, capture_ty) {
                    (Some(src), Some(ty))
                        if self.gen_env_capture_admissible(&ty)
                            && !fn_env_provenance_unproven
                            && capture_field_plan.is_some() =>
                    {
                        init_fields.push((offset, src));
                        field_tys.push(ty.clone());
                        let mut field_plan =
                            capture_field_plan.expect("generator env plan guard checked");
                        // A receive-generator shell owns each mailbox-delivered
                        // user parameter. Its body exists only in the generated
                        // coroutine, so construction may move that owner into
                        // the heap environment instead of cloning it and
                        // stranding the original in the shell. Ordinary `gen
                        // fn` parameters and anonymous-generator captures stay
                        // borrowed sources and retain the clone plan.
                        let transfers_mailbox_owner = self.stream_producer_pump.is_some()
                            && capture.source == hew_hir::HirGenCaptureSource::Local
                            && src
                                != self
                                    .stream_producer_pump
                                    .as_ref()
                                    .expect("receive-generator pump checked")
                                    .sink
                            && base_local(src)
                                .is_some_and(|local| self.parameter_locals.contains(&local));
                        if transfers_mailbox_owner {
                            if let GeneratorEnvFieldPlan::Owned(plan) = field_plan {
                                field_plan = GeneratorEnvFieldPlan::OwnedMove(plan);
                                env_moved_bindings.push(capture.binding);
                            }
                        }
                        // Generator environments store each capture in its value-ABI
                        // representation. An indirect enum is therefore a heap-node
                        // pointer, but the only enum snapshot helper clones an INLINE
                        // tagged union. `OwnedMove` needs no clone and is sound; an
                        // ordinary borrowed/source snapshot must fail closed until a
                        // pointer-backed indirect-enum deep-clone helper exists.
                        if matches!(field_plan, GeneratorEnvFieldPlan::Owned(_))
                            && crate::lower::drop_plan::ty_is_indirect_enum(&ty, &self.enum_layouts)
                        {
                            self.diagnostics.push(MirDiagnostic {
                                kind: MirDiagnosticKind::NotYetImplemented {
                                    construct: format!(
                                        "the cloned capture of indirect enum `{}` into a generator",
                                        capture.name
                                    ),
                                    site: expr.site,
                                },
                                note: format!(
                                    "cannot snapshot `{}` (type `{}`) into a generator: its \
                                     value-ABI slot is a heap-node pointer, and no total \
                                     pointer-backed indirect-enum clone helper exists. A \
                                     receive-generator mailbox owner may move the value into the \
                                     environment, but borrowed captures must remain fail-closed.",
                                    capture.name,
                                    ty.user_facing()
                                ),
                            });
                            poisoned_captures.insert(capture.binding);
                            all_materialisable = false;
                            continue;
                        }
                        env_field_plans.push(field_plan);
                    }
                    (Some(_), Some(ty)) => {
                        // Not admissible to the owned generator env. Name the
                        // fail-closed reason precisely:
                        //   * `Function` with unproven env provenance — an
                        //     anonymous gen block captured a parameter/call
                        //     result/aggregate read without a direct gen-fn call
                        //     boundary proving the pair's env word null; OR
                        //   * a closure pair with a non-null heap environment;
                        //   * an opaque/resource/IO handle with no dup primitive;
                        //   * a trait object or other unsupported structural leaf.
                        let reason = if fn_env_provenance_unproven {
                            "its fn value may carry a heap closure environment, and this \
                             anonymous generator construction has no call-boundary proof \
                             that the env word is null"
                        } else {
                            match &ty {
                                ResolvedTy::Closure { captures, .. } if !captures.is_empty() => {
                                    "a closure with a captured environment has no total clone; \
                                     shallow-copying its sole-owned env pointer would alias the \
                                     box and double-free it at teardown"
                                }
                                _ if crate::model::ty_contains_unclonable_opaque_with_names(
                                    &ty,
                                    &self.record_layouts_for_classification(),
                                    &self.enum_layouts,
                                    &self.opaque_handle_names,
                                ) =>
                                {
                                    "it transitively contains an `#[opaque]` runtime handle; an \
                                     opaque handle has no clone helper, so copying it would alias \
                                     the caller's handle"
                                }
                                _ => {
                                    "its structural shape does not provide both a total clone and \
                                     the matching inverse drop"
                                }
                            }
                        };
                        let construct = if fn_env_provenance_unproven {
                            format!(
                                "the capture of fn value `{}` with unproven env provenance \
                                 into a generator",
                                capture.name
                            )
                        } else {
                            format!(
                                "the capture of opaque/owned value `{}` into a generator",
                                capture.name
                            )
                        };
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct,
                                site: expr.site,
                            },
                            note: format!(
                                "cannot capture `{}` (type `{}`) into a generator: {reason}. \
                                 Only clone-total values and proven null-env fn references may be \
                                 admitted into a generator environment.",
                                capture.name,
                                ty.user_facing()
                            ),
                        });
                        // Poison this capture's binding id: the root
                        // `NotYetImplemented` above is the one actionable
                        // diagnostic. The body sub-builder's `BindingRef`
                        // resolution consults `poisoned_capture_ids` and stays
                        // silent, suppressing the `InitialisedBeforeUse` +
                        // `UnresolvedPlace` cascade for this binding.
                        poisoned_captures.insert(capture.binding);
                        all_materialisable = false;
                    }
                    _ => {
                        // No backend slot for the capture in the enclosing
                        // frame: the body-side read fail-closes at
                        // `UnresolvedPlace` with the canonical note. Do not
                        // fabricate an env field.
                        all_materialisable = false;
                    }
                }
            }
            // Env-record synthesis is all-or-nothing (see `all_materialisable`
            // below): if ANY capture is inadmissible, the whole generator
            // construction fails closed with a root `NotYetImplemented`, and
            // NO env record is built at all — not even for the otherwise-
            // admissible captures collected into `init_fields`/`field_tys`
            // above. Their would-be `capture_env_sources` entries are
            // therefore never registered either (that registration loop below
            // only runs when `env_ty.is_some()`).
            //
            // Without this poison-everything step, an admissible capture in
            // that same abandoned set would be neither in
            // `poisoned_captures` (only the explicitly-rejected binding was
            // added above) nor in `capture_env_sources` (no env exists to
            // source it from) — so its body-side `BindingRef` falls through
            // to the ordinary unresolved-binding path and cascades a spurious
            // `InitialisedBeforeUse` + `UnresolvedPlace` on top of the one
            // actionable root diagnostic. Poison every capture in the
            // abandoned set so the body stays silent for all of them: the
            // root rejection above is already the single actionable
            // diagnostic for the whole construction.
            if !all_materialisable {
                poisoned_captures.extend(captures.iter().map(|capture| capture.binding));
            }
            if all_materialisable {
                let env_name = format!("__hew_gen_env_{owner}_{gen_id}");
                let env_resolved_ty = ResolvedTy::Named {
                    name: env_name.clone(),
                    args: vec![],
                    builtin: None,
                    is_opaque: false,
                };
                env_capture_field_tys.clone_from(&field_tys);
                self.closure_record_layouts
                    .push(crate::model::RecordLayout {
                        name: env_name,
                        field_tys,
                        // Compiler-internal generator-env record: positional names.
                        field_names: Vec::new(),
                    });
                let dest = self.alloc_local(env_resolved_ty.clone());
                self.instructions.push(Instr::RecordInit {
                    ty: env_resolved_ty.clone(),
                    fields: init_fields,
                    dest,
                });
                env_place = Some(dest);
                env_ty = Some(env_resolved_ty);
            }
        }

        // Build a child Builder that lowers the gen-block body.
        // `in_gen_body: true` enables `HirExprKind::Yield` → `Terminator::Yield`
        // construction inside the body.
        let mut body_builder = Builder {
            current_function_symbol: body_name.clone(),
            current_callable_key: Some(body_key.clone()),
            current_function_call_conv: crate::model::FunctionCallConv::Default,
            in_gen_body: true,
            // A generator body is user code and needs the same complete shared
            // type/ownership registry as every other generated child body.
            // In particular, enum layouts drive both tagged overwrite release
            // and scope/abandonment drop admission. Building this child from a
            // hand-picked table subset left those layouts empty, so an enum
            // payload overwritten after a yield leaked even though the
            // byte-identical non-generator body released it. Keep the
            // fail-closed per-function defaults (including local freshness)
            // supplied by the shared constructor.
            ..self.child_builder_tables()
        };
        // Propagate the inadmissible-capture poison set into the body builder so
        // its `BindingRef` resolution stays silent for those bindings — the root
        // rejection already fired in the enclosing frame, and the un-materialised
        // capture would otherwise cascade into `InitialisedBeforeUse` +
        // `UnresolvedPlace`.
        body_builder.poisoned_capture_ids = poisoned_captures;

        // Prepend the coroutine-ramp parameters. The generator body is lowered
        // as an `llvm.coro.*` switched-resume coroutine ramp (the same substrate
        // the await-family uses, `hew-codegen-rs/src/coro.rs`); its leading
        // formal parameters are:
        //   Local(0) — `out_ptr: *mut Y`. The value channel. Before every
        //              `yield` the codegen `Terminator::Yield` arm stores the
        //              yielded value to `*out_ptr`; the consumer
        //              (`Instr::GeneratorNext`) reads it back from the same slot
        //              after each resume. This is the explicit out-pointer the
        //              `HewCont` substrate threads through the frame (cont.rs),
        //              NOT the forbidden non-null `coro.id` promise.
        //   Local(1) — `env_ptr: *const Env` (ONLY when the body has captures).
        //              The capture env record; the body reads each captured free
        //              variable through it via `ClosureEnvFieldLoad`. A
        //              capture-free generator has no env param — its single
        //              leading param is `out_ptr`.
        // Subsequent user-statement local allocations naturally start after the
        // leading params because `alloc_local` indexes from `locals.len()`.
        let gen_ptr_ty = ResolvedTy::Pointer {
            is_mutable: true,
            pointee: Box::new(ResolvedTy::Unit),
        };
        // Local(0): the out-pointer (always present).
        body_builder.locals.push(gen_ptr_ty.clone());
        let has_env = env_ty.is_some();
        if has_env {
            // Local(1): the capture-env pointer (only when captures materialised).
            body_builder.locals.push(gen_ptr_ty.clone());
        }

        // Register each materialised capture as a body-side env-field source:
        // the body's `BindingRef`s to a captured binding (a `gen fn` param or a
        // `gen { }` captured outer local) lower to `ClosureEnvFieldLoad` through
        // `Local(1)` — the env pointer (out_ptr occupies Local(0)). Mirrors the
        // lambda-actor-body env discipline: loads are read-only views into the
        // env. The coro frame is single-owner (no body thread), so the env copy
        // lifetime is bounded by the frame, not a runtime thread.
        if let Some(env_resolved_ty) = &env_ty {
            for (idx, capture) in captures.iter().enumerate() {
                body_builder.capture_env_sources.insert(
                    capture.binding,
                    CaptureEnvSource {
                        env: Place::Local(1),
                        env_ty: env_resolved_ty.clone(),
                        field_offset: FieldOffset(
                            u32::try_from(idx).expect("gen capture count exceeds u32::MAX"),
                        ),
                        ty: env_capture_field_tys[idx].clone(),
                    },
                );
            }
        }

        // #2301 -- same pre-pass gap as `lower_closure_shim`: this gen body
        // lowers via its own fresh `body_builder`; `child_builder_tables`
        // deliberately carries module registries but no per-body pre-pass
        // facts, so without this call
        // `prepass_consumed_bindings`/`prepass_reassigned_bindings` stay empty
        // for every binding local to the `gen fn`/`gen {}` body and a `var`
        // consumed on one control-flow arm and overwritten on a sibling arm
        // silently leaks its prior value instead of getting the path-sensitive
        // release a byte-identical top-level function body would.
        body_builder.collect_prepass_facts(body);

        // Lower all statements in the gen-block body. Yields inside the body
        // call `lower_yield_expr` which emits `Terminator::Yield` and advances
        // the cursor to a fresh resume block.
        body_builder.active_scopes.push(body.scope);
        for stmt in &body.statements {
            body_builder.stmt(stmt);
        }
        // Lower the tail expression (the implicit return value of the block).
        if let Some(tail) = &body.tail {
            if let Some(src) = body_builder.lower_value_for_move(tail) {
                body_builder.push_instr(Instr::Move {
                    dest: Place::ReturnSlot,
                    src,
                });
            }
        }
        body_builder.emit_pending_defers(body.scope);
        body_builder.active_scopes.pop();

        // Seal the last block with `Terminator::Return`. For a gen body
        // this represents the generator completing (returns `return_ty` which
        // S5 maps to `None` on the Iterator impl side).
        // Generator bodies finish through the one seam. This ramp used to
        // hand-roll a partial copy of `lower_function`'s pipeline — its own
        // comments said so — and each pass that copy predated was silently
        // absent inside a `gen fn`: the divergent-arm selection release among
        // them, leaking a whole `Vec` per iteration.
        let super::FinalizedBody {
            blocks,
            body_statements,
        } = super::finalize_body(
            &mut body_builder,
            super::BodySeal::Cursor(Terminator::Return),
            super::BodyFinalizeSpec::nested_body(),
        );

        // Cross-suspend state is owned by LLVM's CoroSplit. The generator body
        // lowers to an `llvm.coro.*` switched-resume coroutine; CoroSplit
        // automatically spills every local live across a `coro.suspend` into the
        // single heap frame and re-derives the drop manifest from the `cleanup`
        // outline. There is therefore NO separate cross-yield-liveness /
        // state-record synthesis pass: the prior `gen_state::synthesise` machinery
        // (the spike-era stand-in for the never-landed hand-rolled state machine)
        // is subsumed by the coro frame and removed (RC14).
        let body_locals_with_state = body_builder.locals.clone();

        // Build the raw/checked/elaborated triple for the body function.

        // The gen-body coroutine ramp's formal parameters mirror the leading
        // locals prepended above: `params[0]` is the out-pointer (`*mut Y`, the
        // value channel) and — only when the body has captures — `params[1]` is
        // the capture-env pointer. The `Terminator::Yield` codegen arm loads
        // `Local(0)` to store the yielded value before each `coro.suspend`; the
        // body reads env fields through `Local(1)` via `ClosureEnvFieldLoad`.
        //
        // WHY FunctionCallConv::Default: the body IS a coroutine ramp, detected
        // by codegen's `is_coroutine`/`has_suspend` carrier scan (it carries
        // `Terminator::Yield`), which overrides the LLVM return type to the
        // `coro.begin` handle (`ptr`) regardless of the call conv. No dedicated
        // call conv is needed; the carrier presence is the signal.
        let mut gen_body_params = vec![ResolvedTy::Pointer {
            is_mutable: true,
            pointee: Box::new(ResolvedTy::Unit),
        }];
        if has_env {
            gen_body_params.push(ResolvedTy::Pointer {
                is_mutable: true,
                pointee: Box::new(ResolvedTy::Unit),
            });
        }
        let mut raw = RawMirFunction {
            name: body_name.clone(),
            key: body_key.clone(),
            return_ty: return_ty.clone(),
            call_conv: crate::model::FunctionCallConv::Default,
            params: gen_body_params,
            locals: body_locals_with_state.clone(),
            // Synthesised generator body: no faithful user bindings, no DIEs.
            local_names: Vec::new(),
            local_scopes: Vec::new(),
            local_decl_bytes: Vec::new(),
            scope_table: Vec::new(),
            blocks: blocks.clone(),
            decisions: body_builder.decisions.clone(),
            intrinsic_id: None,
            await_deadline_ns: body_builder.await_deadline_ns.clone(),
            suspend_kinds: body_builder.suspend_kinds.clone(),
            lambda_actor_user_param_locals: Vec::new(),
            span: None,
            instr_spans: ::std::collections::BTreeMap::new(),
            source_origin: SourceOrigin::Unknown,
        };

        // A synthetic HirFn shell so `check_function` has a valid fn descriptor.
        // The body is empty — dataflow runs on the raw blocks directly.
        let synthetic_fn = HirFn {
            id: hew_hir::ItemId(0),
            node: hew_hir::HirNodeId(0),
            declaration: hew_types::DefId::legacy_reconstruct_from_full_path(body_name.clone()),
            name: body_name.clone(),
            type_params: Vec::new(),
            is_generator: false,
            intrinsic_id: None,
            params: Vec::new(),
            return_ty: return_ty.clone(),
            body: hew_hir::HirBlock {
                node: hew_hir::HirNodeId(0),
                scope: hew_hir::ScopeId(0),
                statements: Vec::new(),
                tail: None,
                ty: return_ty.clone(),
                span: expr.span.clone(),
            },
            span: expr.span.clone(),
        };

        let dataflow_result = check_function(&body_builder, &raw.blocks, &synthetic_fn);
        let mut body_diagnostics: Vec<MirDiagnostic> = dataflow_result
            .checks
            .iter()
            .filter_map(check_to_diagnostic)
            .collect();
        body_diagnostics.append(&mut body_builder.diagnostics);
        collect_unknown_type_diagnostics(&synthetic_fn, &body_builder, &mut body_diagnostics);
        finalize_string_ownership(&mut raw, &mut body_builder, &dataflow_result);
        finalize_bytes_ownership(&mut raw, &mut body_builder, &dataflow_result);
        materialize_explicit_goto_edge_carries(&mut raw.blocks, &mut body_builder);
        raw.instr_spans.clone_from(&body_builder.instr_spans);

        let cooperate_sites = dataflow::compute_cooperate_sites(&raw.blocks);
        let (checked, elaboration_diagnostics) = seal_checked(
            body_name.clone(),
            body_key.clone(),
            return_ty.clone(),
            raw.blocks.clone(),
            &raw,
            body_builder.decisions.clone(),
            dataflow_result.checks.clone(),
            cooperate_sites,
            &body_builder,
            &body_statements,
        );
        let elaborated = elaborate(&checked);
        body_diagnostics.extend(elaboration_diagnostics);

        let body_lowered = LoweredFunction {
            raw,
            checked,
            elaborated,
            diagnostics: body_diagnostics,
            generated: body_builder.generated_functions,
            record_layouts: body_builder.closure_record_layouts,
            // The state-record layout this body owns sits on the
        };
        self.generated_functions.push(body_lowered);

        // Materialize the generator value at the construction site: emit
        // `Terminator::MakeGenerator`. When the body captures BitCopy free
        // variables, `env` is the freshly-built env record place in this
        // (enclosing) frame: codegen heap-copies it (`hew_cont_frame_alloc` +
        // `memcpy`) and passes the copy's address to the `__hew_gen_body_*`
        // coro ramp. When there are no captures `env` is `None` and codegen
        // passes a null env to the ramp. The heap companion pointer (holding
        // the ramp's returned `llvm.coro.begin` handle) is stored into
        // `gen_place`; the gen-block expression evaluates to that place.
        let next = self.alloc_block();
        let env = match (env_place, env_ty) {
            (Some(place), Some(ty)) => Some(GeneratorEnvPlan {
                place,
                ty,
                fields: env_field_plans,
            }),
            (None, None) => None,
            _ => unreachable!("generator env place/type must be constructed together"),
        };
        let env_transfer_place = env.as_ref().map(|plan| plan.place);
        self.finish_current_block(Terminator::MakeGenerator {
            dest: gen_place,
            body_fn: body_name.clone(),
            next,
            env,
        });
        self.start_block(next);
        // `OwnedMove` commits only after `MakeGenerator` returns normally.
        // Publish the source neutralisation and exact OwnerId transfer in that
        // successor, where both the physical and semantic ownership changes
        // occur. The unwind edge retains the source owner in the caller.
        if let Some(env_place) = env_transfer_place {
            for binding in env_moved_bindings {
                let source = *self
                    .binding_locals
                    .get(&binding)
                    .expect("generator OwnedMove binding has no source place");
                self.push_instr(Instr::NeutralizePayloadSlot {
                    place: source,
                    transferee: Some(env_place),
                    authority: crate::model::NeutralizeAuthority::AggregateMemberConsume,
                });
                self.set_owned_local_consumed(
                    binding,
                    Some(env_place),
                    super::DischargeSite::BindingMoved,
                );
            }
        }
        gen_place
    }

    /// Reshape a `receive gen fn` handler shell into a stream-producer PUMP
    /// (decision 3). Called immediately after
    /// `lower_gen_block` constructs `gen_place` (the freshly-made generator
    /// handle) for a shell whose `Builder::stream_producer_pump` is `Some`.
    ///
    /// Emits, in the current (post-`MakeGenerator`) block:
    /// ```text
    /// loop_head:
    ///   opt = GeneratorNext(gen_place)          ; Option<Yield>
    ///   branch tag(opt) == 0 (Some) ? send : close
    /// send:
    ///   value = opt.0                            ; MachineVariant payload
    ///   suspend StreamSend { sink, value } -> after_send
    /// after_send:
    ///   goto loop_head
    /// close:
    ///   call hew_sink_close(sink) -> close_next
    /// close_next:
    ///   <cursor left open — the seam's `BodySeal::Cursor(Terminator::Return)`
    ///    seals it as the implicit unit return>
    /// ```
    ///
    /// The `Option<Yield>` tag convention (`Some` = 0, `None` = 1) mirrors
    /// `Instr::GeneratorNext`'s own documented codegen contract; the tag-test +
    /// payload-extract shape mirrors the general enum-match lowering's
    /// `arm_is_generator_some` arm (`lower_match_enum_tag`) — both read a
    /// `GeneratorNext` dest via `Place::EnumTag`/`Place::MachineVariant { local,
    /// variant_idx: 0, field_idx: 0 }`, the same substrate an ordinary `for v in
    /// gen()` loop already exercises.
    ///
    /// The pump's fault-close and cancellation wiring (decisions 6+7): a
    /// PROLOGUE registers the pump's sink with its own actor
    /// (`hew_actor_gen_sink_register`) so a terminal teardown reaching this
    /// actor while the pump is live can find and fault-close it; each loop
    /// iteration checks the consumer peer (`hew_sink_peer_closed`) BEFORE
    /// resuming the generator, breaking WITHOUT resuming further once the
    /// peer has closed (cancellation — an infinite generator plus a consumer
    /// `break` must not livelock the actor); the shared close path (generator
    /// exhausted OR peer closed) deregisters + frees the sink via
    /// `hew_actor_gen_sink_complete` (replacing the shell's earlier bare
    /// `hew_sink_close`).
    ///
    /// Emits the peer-closed check (decision 6): calls `hew_sink_peer_closed`,
    /// compares its C-ABI `i32` result against zero, and branches on it.
    /// Returns `(resume_bb, close_bb)` — the caller starts `resume_bb` to
    /// drive the generator, and starts (or shares) `close_bb` for the
    /// registered-complete close. Factored out of `build_stream_producer_pump`
    /// to keep that function under the `too_many_lines` threshold.
    fn emit_pump_peer_closed_check(&mut self, sink: Place) -> (u32, u32) {
        use hew_types::runtime_call::RuntimeCallFamily;

        let peer_closed = self.alloc_local(ResolvedTy::I32);
        let after_peer_check = self.alloc_block();
        self.finish_current_block(Terminator::Call {
            callee: "hew_sink_peer_closed".to_string(),
            authority: crate::CallAuthority::Runtime(RuntimeCallFamily::SinkPeerClosed),
            args: vec![sink],
            dest: Some(peer_closed),
            next: after_peer_check,
        });
        self.start_block(after_peer_check);

        let zero_i32 = self.alloc_local(ResolvedTy::I32);
        self.push_instr(Instr::ConstI64 {
            dest: zero_i32,
            value: 0,
        });
        let is_peer_closed = self.alloc_local(ResolvedTy::Bool);
        self.push_instr(Instr::IntCmp {
            pred: crate::model::CmpPred::NotEq,
            lhs: peer_closed,
            rhs: zero_i32,
            dest: is_peer_closed,
        });

        let resume_bb = self.alloc_block();
        let close_bb = self.alloc_block();
        self.finish_current_block(Terminator::Branch {
            cond: is_peer_closed,
            then_target: close_bb,
            else_target: resume_bb,
        });
        (resume_bb, close_bb)
    }

    /// Publish the pump's per-yield copy as an exact Checked-MIR owner.
    ///
    /// The stream send BORROWS the yielded value: `hew_stream_await_send` and
    /// its layout sibling copy the content out of the slot, so the pump stays
    /// the sole owner of its copy and must release it exactly once per yield.
    /// Minting the copy under a sentinel binding makes every exit that can
    /// observe it live — in particular the `Suspend` destroy-while-parked edge
    /// of the send — derive its cleanup from ownership replay through the
    /// owner's definition-site recipe. No abandon-only side table exists: the
    /// resume edge's inline `Drop` + `Release` ends the generation, so the
    /// two edges are mutually exclusive by replay, not by construction of a
    /// second plan source. `BitCopy` yields publish no owner (the registrar
    /// withholds a recipe-less mint).
    fn mint_pump_yield_value_owner(
        &mut self,
        value_local: Place,
        yield_ty: &ResolvedTy,
        site: SiteId,
    ) {
        let binding = SENTINEL_STREAM_SEND_VALUE_BINDING;
        let name = "__hew_stream_send_value".to_string();
        self.push_bind_statement(binding, name.clone(), site, yield_ty.clone());
        // Wire `binding_locals` BEFORE `register_owned_local`: the registrar
        // reads the slot to publish the Mint over the real destination.
        self.binding_locals.insert(binding, value_local);
        self.record_binding_scope(binding);
        // `GeneratorNext` moved the payload out of the generator's `Option`
        // slot into this frame: the copy is a domestic move-out with one
        // caller-side release obligation.
        self.register_owned_local(
            binding,
            name,
            yield_ty.clone(),
            Builder::owner_warrant_for_typed_produced_value(
                hew_types::ProducedValueOwnership::owned(
                    hew_types::ProducedValueAcquisition::MoveOut,
                ),
            ),
        );
    }

    /// Release the pump's per-yield value copy on the stream-send RESUME edge.
    ///
    /// This edge is reached ONLY when the send resumes (delivered/ready); the
    /// abandon-while-parked path runs the `Suspend` exit plan, which replay
    /// derives from the owner minted by [`Self::mint_pump_yield_value_owner`].
    /// Emit the inline release before the `Goto` loops back to overwrite
    /// `value_local`, end the owner generation beside it, and disposition the
    /// ledger entry off the scope-exit set so the function-scope release does
    /// not fire a second time. The release protocol comes from the same
    /// authority the consumer-side yield-binding drop uses
    /// (`generator_yield_drop_symbol`): `string`/`bytes`/`Vec` release through
    /// their wired symbol; a heap-owning record/enum composite releases through
    /// its synthesised in-place thunk (`DropFnSpec::InPlace`). `BitCopy`
    /// scalars carry no inline release (`NoDropPath`); an `Unwired` element is
    /// rejected upstream and never reaches the pump.
    fn emit_pump_yield_value_release(&mut self, value_local: Place, yield_ty: &ResolvedTy) {
        let drop_fn = match self.generator_yield_drop_symbol(yield_ty) {
            ReleaseSymbolVerdict::Wired(symbol) => crate::model::DropFnSpec::Release(symbol),
            ReleaseSymbolVerdict::WiredInPlace(kind) => crate::model::DropFnSpec::InPlace(kind),
            ReleaseSymbolVerdict::NoDropPath | ReleaseSymbolVerdict::Unwired(_) => return,
        };
        self.push_instr(Instr::Drop {
            place: value_local,
            ty: yield_ty.clone(),
            drop_fn: Some(drop_fn),
        });
        let binding = SENTINEL_STREAM_SEND_VALUE_BINDING;
        if let Some(generation) = self.owner_generations.get(&binding).copied() {
            self.push_instr(Instr::OwnershipEvent(
                crate::model::OwnershipEvent::Release {
                    owner: crate::model::OwnerId {
                        binding,
                        generation,
                    },
                    place: value_local,
                },
            ));
            self.set_owned_local_disposition(binding, Disposition::BodyEndReleased);
        }
    }

    pub(crate) fn build_stream_producer_pump(
        &mut self,
        gen_place: Place,
        pump: &StreamProducerPumpCtx,
        site: SiteId,
    ) {
        use hew_types::runtime_call::RuntimeCallFamily;

        // Prologue: register this pump's own sink with its own actor.
        let actor_self = self.emit_actor_self_handle();
        let after_register = self.alloc_block();
        self.finish_current_block(Terminator::Call {
            callee: "hew_actor_gen_sink_register".to_string(),
            authority: crate::CallAuthority::Runtime(RuntimeCallFamily::ActorGenSinkRegister),
            args: vec![actor_self, pump.sink],
            dest: None,
            next: after_register,
        });
        self.start_block(after_register);

        let loop_head = self.alloc_block();
        self.finish_current_block(Terminator::Goto { target: loop_head });
        self.start_block(loop_head);

        // Peer-closed check (decision 6): before every resume, not just the
        // first. Branches to `close_bb` without resuming when the consumer
        // has closed, else falls through into the caller's `resume_bb`.
        let (resume_bb, close_bb) = self.emit_pump_peer_closed_check(pump.sink);

        self.start_block(resume_bb);
        let option_ty =
            ResolvedTy::named_builtin("Option", BuiltinType::Option, vec![pump.yield_ty.clone()]);
        let opt_dest = self.alloc_local(option_ty);
        self.push_instr(Instr::GeneratorNext {
            dest: opt_dest,
            ctx: gen_place,
            ctx_owner: None,
            yield_ty: pump.yield_ty.clone(),
        });
        let Place::Local(opt_local) = opt_dest else {
            unreachable!("Builder::alloc_local always returns Place::Local")
        };

        let tag_local = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::Move {
            dest: tag_local,
            src: Place::EnumTag(opt_local),
        });
        let some_tag = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::ConstI64 {
            dest: some_tag,
            value: 0,
        });
        let is_some = self.alloc_local(ResolvedTy::Bool);
        self.push_instr(Instr::IntCmp {
            pred: crate::model::CmpPred::Eq,
            lhs: tag_local,
            rhs: some_tag,
            dest: is_some,
        });

        let send_bb = self.alloc_block();
        self.finish_current_block(Terminator::Branch {
            cond: is_some,
            then_target: send_bb,
            else_target: close_bb,
        });

        self.start_block(send_bb);
        let value_local = self.alloc_local(pump.yield_ty.clone());
        self.push_instr(Instr::Move {
            dest: value_local,
            src: Place::MachineVariant {
                local: opt_local,
                variant_idx: 0,
                field_idx: 0,
            },
        });
        self.mint_pump_yield_value_owner(value_local, &pump.yield_ty, site);
        let after_send = self.alloc_block();
        self.record_suspend_kind(SuspendKind::StreamSend {
            sink: pump.sink,
            value: value_local,
        });
        self.finish_current_block(Terminator::Suspend {
            resume: after_send,
            cleanup: after_send,
            is_final: false,
        });
        self.start_block(after_send);
        self.emit_pump_yield_value_release(value_local, &pump.yield_ty);
        self.finish_current_block(Terminator::Goto { target: loop_head });

        // Shared close path: reached with the generator exhausted (`None`)
        // OR the consumer peer already closed. Either way the sink is done;
        // deregister + free it exactly once.
        self.start_block(close_bb);
        let close_next = self.alloc_block();
        self.finish_current_block(Terminator::Call {
            callee: "hew_actor_gen_sink_complete".to_string(),
            authority: crate::CallAuthority::Runtime(RuntimeCallFamily::ActorGenSinkComplete),
            args: vec![actor_self, pump.sink],
            dest: None,
            next: close_next,
        });
        self.start_block(close_next);
    }

    /// Lower `HirExprKind::Yield { value, yield_ty }` inside a gen-block body.
    ///
    /// Must only be called from a `Builder` with `in_gen_body = true`. If
    /// `in_gen_body` is `false`, a `yield` expression appeared outside a
    /// generator body — this is a checker invariant violation. Fail-closed:
    /// emit `UnsupportedNode` and return `None` rather than fabricating a
    /// value.
    ///
    /// Emits `Terminator::Yield { value: <place>, next: <resume_block_id> }` on
    /// the CURRENT block, then advances the cursor to the fresh resume block.
    /// The yield expression evaluates to unit in the body (the caller ignores
    /// the `None` return).
    ///
    /// # S3b cross-yield liveness stub
    /// After yielding, locally-defined values that are used again after the
    /// resume point must be stored into (and reloaded from) the generator state
    /// record. S3b's liveness pass identifies those locals and emits
    /// store-before-yield / load-after-resume instructions. This function
    /// intentionally leaves that gap as a comment at the yield site so S3b
    /// has a clean insertion point.
    pub(crate) fn lower_yield_expr(
        &mut self,
        expr: &HirExpr,
        value: Option<&HirExpr>,
    ) -> Option<Place> {
        // Fail-closed: `yield` outside a gen-block body is a checker
        // invariant violation. Emit a clear diagnostic rather than
        // fabricating a value.
        if !self.in_gen_body {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "HirExprKind::Yield outside gen-block body".to_string(),
                    site: expr.site,
                },
                note: "yield is only valid inside a gen{} block; \
                       the HIR checker should have rejected this program before MIR"
                    .to_string(),
            });
            return None;
        }

        // Lower the yielded value to a Place.  If the value is absent (bare
        // `yield;` — unit-typed generator), allocate a unit constant.
        let value_place = if let Some(val_expr) = value {
            self.decide(val_expr);
            match self.lower_value_for_move(val_expr) {
                Some(p) => {
                    // `Yield` transfers the value into the generator companion's
                    // output slot. A direct BindingRef was already consumed by
                    // `lower_value_for_move`; close the anonymous typed-producer
                    // generation (for example `yield make_rows(i)`) here too so
                    // normal resume cannot drop the caller-owned output.
                    self.mark_yielded_binding_moved(val_expr);
                    p
                }
                None => {
                    // The value sub-expression failed to lower.  The child
                    // diagnostic has already been pushed; propagate failure
                    // by not emitting the Yield terminator.
                    return None;
                }
            }
        } else {
            // `yield;` — allocate a unit local as the value carrier.
            self.alloc_local(ResolvedTy::Unit)
        };

        // S3b insertion point: store cross-yield live locals to the state
        // record HERE, before the Terminator::Yield. S3b's liveness pass
        // identifies which locals are used after this resume point and emits
        // the store instructions at this site.
        // TODO(S3b): emit store-before-yield for cross-yield live locals.

        // Allocate the resume block id and seal the current block with the
        // Yield terminator.
        let resume_block = self.alloc_block();
        self.finish_current_block(Terminator::Yield {
            value: value_place,
            next: resume_block,
        });
        self.start_block(resume_block);

        // S3b insertion point: reload cross-yield live locals from the state
        // record HERE, at the top of the resume block.
        // TODO(S3b): emit load-after-resume for cross-yield live locals.

        // `yield` evaluates to unit in the gen body.
        None
    }
}

#[cfg(test)]
mod builtin_carrier_tests {
    use super::*;

    #[test]
    fn lambda_pid_gate_uses_builtin_identity_not_presentation() {
        let renamed = ResolvedTy::named_builtin(
            "presentation.RenamedLambdaPid",
            BuiltinType::LambdaPid,
            vec![ResolvedTy::String, ResolvedTy::I64],
        );
        assert!(is_lambda_pid_ty(&renamed));

        let shadow = ResolvedTy::named_user("LambdaPid", vec![ResolvedTy::String, ResolvedTy::I64]);
        assert!(!is_lambda_pid_ty(&shadow));
    }
}
