use super::{
    base_local, stream_handle_drop_descriptor, vec_iter_init_vec_source_expr, BindingId, Builder,
    BuiltinType, Disposition, HashSet, HirExpr, HirExprKind, Instr, LoopFrame, MirDiagnostic,
    MirDiagnosticKind, Place, ResolvedRef, ResolvedTy, ScopeId, ScopeInfoEntry,
};

impl Builder {
    /// Materialize all pending defers for `scope_id` in LIFO order.
    ///
    /// Called at every exit from a scope: the tail of a `Block` expression,
    /// the end of `function_body`, and (Stage 2) before early returns.
    /// Each deferred body expression is lowered inline — the result value
    /// is discarded (defer bodies are side-effect-only; their type is Unit
    /// per HIR lowering). LIFO ordering matches the Go/Swift/Zig defer
    /// discipline: last-registered runs first.
    pub(crate) fn emit_pending_defers(&mut self, scope_id: ScopeId) {
        let Some(defers) = self.pending_defers.remove(&scope_id) else {
            return;
        };
        // LIFO: iterate in reverse registration order.
        for body in defers.into_iter().rev() {
            // Lower the deferred body for its side effects. The result
            // Place is discarded — Q205-B mandates defer bodies are
            // effect-only (Unit-typed). Bindings resolve by lexical
            // reference at this point, observing their final value.
            let _ = self.lower_value(&body);
        }
    }
    /// Record the HIR scope a freshly-bound `BindingId` was declared in. Called
    /// at every `MirStatement::Bind` push site (let statements, match-arm
    /// payload bindings, function parameters, for-range counters,
    /// pattern-destructure binders) so the elaborator's back-edge drop pass can
    /// scope-filter which bindings get a per-iteration release. A `Bind`
    /// pushed outside any active scope (e.g. the synthetic param Binds before
    /// the body is entered) is recorded against the function's outer scope —
    /// `unwrap_or` to `ScopeId(0)` so the entry is non-empty even at the
    /// pre-scope edge; back-edge drops never reference scope 0 (no loop body
    /// closes there), so the fall-back is benign. The map is read by
    /// `enumerate_exits` when building each loop back-edge `Goto`'s `DropPlan`.
    pub(crate) fn record_binding_scope(&mut self, binding: BindingId) {
        let scope = self.active_scopes.last().copied().unwrap_or(ScopeId(0));
        self.binding_scope.insert(binding, scope);
        if let Some(span) = self.current_span {
            self.binding_decl_byte.insert(binding, span.0);
            self.note_scope_span(span);
        }
    }
    /// Widen the active scope's recorded byte-extent to cover `span`, and record
    /// its parent (the frame directly below the active scope) the first time the
    /// scope is observed. Drives the `-g` `DILexicalBlock` PC ranges. A no-op
    /// when no scope is active (only the pre-body synthetic param Binds, which
    /// carry no user-visible variable DIE anyway).
    pub(crate) fn note_scope_span(&mut self, span: (u32, u32)) {
        let depth = self.active_scopes.len();
        let Some(&scope) = self.active_scopes.last() else {
            return;
        };
        let parent = depth.checked_sub(2).map(|i| self.active_scopes[i]);
        let (start, end) = span;
        self.scope_info
            .entry(scope)
            .and_modify(|e| {
                e.min_start = e.min_start.min(start);
                e.max_end = e.max_end.max(end);
                // The first observation fixes the parent; a scope can be
                // re-entered (loop body), but its lexical parent is invariant.
                if e.parent.is_none() {
                    e.parent = parent;
                }
            })
            .or_insert(ScopeInfoEntry {
                parent,
                min_start: start,
                max_end: end,
            });
    }
    /// Emit a `hew_gen_coro_destroy` release for every generator/`AsyncGenerator`
    /// handle binding declared in `scope_id`, at the point this scope closes.
    /// This runs on the NORMAL scope-exit path of a block; because a block
    /// nested inside a loop re-executes (and its scope re-closes) once per
    /// outer iteration, the generator's coro frame + heap companion are
    /// released every iteration instead of accumulating until function exit.
    ///
    /// Each released binding is removed from `owned_locals` and
    /// `scope_generator_bindings` so the function-exit LIFO drop never fires a
    /// second `hew_gen_coro_destroy` on the same slot — and the inline
    /// `Instr::Drop` null-stores the slot afterwards, so even a
    /// structurally-reachable second drop observes null
    /// (`raii-null-after-move`; the runtime also null-guards). A binding whose
    /// slot was already consumed (moved out) earlier on the path is still
    /// safe: `hew_gen_coro_destroy(null)` is a no-op.
    pub(crate) fn emit_scope_generator_drops(&mut self, scope_id: ScopeId) {
        // Collect this scope's generator bindings (LIFO: reverse declaration
        // order) and drop them, leaving other scopes' entries in place.
        let mut to_drop: Vec<(hew_hir::BindingId, ResolvedTy)> = Vec::new();
        self.scope_generator_bindings.retain(|(s, binding, ty)| {
            if *s == scope_id {
                to_drop.push((*binding, ty.clone()));
                false
            } else {
                true
            }
        });
        for (binding, ty) in to_drop.into_iter().rev() {
            let Some(place) = self.binding_locals.get(&binding).copied() else {
                continue;
            };
            self.set_owned_local_disposition(binding, Disposition::ScopeReleased);
            self.push_instr(Instr::Drop {
                place,
                ty,
                drop_fn: Some(crate::model::DropFnSpec::Release("hew_gen_coro_destroy")),
            });
        }
    }
    /// Release the `vec` handle of every sole-owner `for x in …` cursor
    /// (`VecIter<T>`) declared in `scope_id` when that scope closes, so the
    /// handle is freed on every outer-loop iteration rather than leaking one per
    /// iteration. #1949.
    ///
    /// Mirrors [`Self::emit_scope_generator_drops`]: it emits the release inline
    /// as an `Instr::RecordFieldDrop` on the cursor's `vec` field (declaration-
    /// order field 0), removes the cursor from `owned_locals` so the function-exit
    /// LIFO pass cannot fire a second free, and the inline drop is null-safe
    /// (`hew_vec_free` no-ops on null). Only cursors the registration gate
    /// admitted are here — a sole-owner cursor with a `BitCopy`-element `vec`
    /// (`vec_iter_ty_drop_safe`) and an rvalue/consumed source
    /// (`vec_iter_let_cursor_owns_handle`). A `CowShare` place-source cursor and
    /// any owned/string-element cursor were never registered, so the handle is
    /// freed exactly once (by the source binding's drop for a place source, or
    /// left undropped for an owned-element vec — fail-closed leak, never a shared-
    /// element double-free).
    ///
    /// The element is `BitCopy` by the registration gate, so the `vec` handle's
    /// release is the plain `hew_vec_free` (buffer + handle; no per-element
    /// release path, hence no shared-element double-free hazard).
    pub(crate) fn emit_scope_vec_iter_drops(&mut self, scope_id: ScopeId) {
        let mut to_drop: Vec<(hew_hir::BindingId, ResolvedTy)> = Vec::new();
        self.scope_vec_iter_bindings.retain(|(s, binding, ty)| {
            if *s == scope_id {
                to_drop.push((*binding, ty.clone()));
                false
            } else {
                true
            }
        });
        for (binding, cursor_ty) in to_drop.into_iter().rev() {
            let Some(place) = self.binding_locals.get(&binding).copied() else {
                continue;
            };
            // The cursor's `vec` field is `Vec<T>` where `T` is the cursor's sole
            // type argument; the registration gate proved `T` is `BitCopy`, so the
            // release is the plain `hew_vec_free`.
            let ResolvedTy::Named { args, .. } = &cursor_ty else {
                continue;
            };
            let Some(elem_ty) = args.first().cloned() else {
                continue;
            };
            let vec_ty = ResolvedTy::Named {
                name: "Vec".to_string(),
                args: vec![elem_ty],
                builtin: Some(BuiltinType::Vec),
                is_opaque: false,
            };
            self.set_owned_local_disposition(binding, Disposition::ScopeReleased);
            self.push_instr(Instr::RecordFieldDrop {
                record: place,
                field_offset: crate::model::FieldOffset(0),
                ty: vec_ty,
                drop_fn: crate::model::DropFnSpec::Release("hew_vec_free"),
            });
        }
    }
    /// True when the `for x in …` cursor `value` (a HIR `VecIter { vec, idx }`
    /// struct-init) iterates a field/tuple PROJECTION rooted at a bare actor
    /// state-field reference — `for v in x.v` where `x` is a `var` state field.
    ///
    /// #2540 — the projected leaf (`x.v`) is owned by the actor STATE: the
    /// global `__hew_state_drop_*` destructor frees every state field
    /// unconditionally at actor teardown, and (unlike a local record's own drop)
    /// that state drop CANNOT be elided by the escaped-sibling analysis (#2212).
    /// So a projected-state-field cursor must NOT be registered for a scope-exit
    /// `RecordFieldDrop` of its `vec` handle: doing so frees the state-owned
    /// buffer a second time against the state drop — the use-after-free the
    /// supervisor normal-return cleanup epilogue exposes (latent on `main`,
    /// which never dropped the state, so the cursor's lone free went unnoticed
    /// as a leak-masking single free). The cursor instead BORROWS the handle,
    /// exactly as `vec_iter_let_cursor_owns_handle`'s bare-`BindingRef` arm
    /// already grants the direct-field `for v in x` (#2432/#2525): the state
    /// owns the handle and the state drop frees it exactly once.
    ///
    /// A LOCAL record projection (`for v in localBox.v`) is deliberately NOT
    /// matched. There the cursor is the sole owner and the local record's own
    /// drop of the escaped `v` field is elided (#2212), so the handle is freed
    /// exactly once — by the cursor; flipping it to a borrow would LEAK. Only a
    /// state-field root, whose drop is unconditional and un-elidable, needs the
    /// cursor's free suppressed. The root discriminator (`id` is not a
    /// function-local AND its `name` resolves in `current_actor_state_fields`)
    /// is the same one the `ActorStateFieldLoad` emitter uses to recognise a
    /// bare state field.
    pub(crate) fn vec_iter_source_projects_actor_state_field(&self, value: &HirExpr) -> bool {
        let Some(mut cur) = vec_iter_init_vec_source_expr(value) else {
            return false;
        };
        let mut projected = false;
        loop {
            match &cur.kind {
                HirExprKind::FieldAccess { object, .. } => {
                    projected = true;
                    cur = &**object;
                }
                HirExprKind::TupleIndex { tuple, .. } => {
                    projected = true;
                    cur = &**tuple;
                }
                HirExprKind::BindingRef {
                    name,
                    resolved: ResolvedRef::Binding(id),
                } => {
                    return projected
                        && !self.binding_locals.contains_key(id)
                        && self.current_actor_state_fields.contains_key(name);
                }
                _ => return false,
            }
        }
    }
    /// True when the `for x in …` cursor `value` iterates an INDEX projection
    /// `container[i]` whose `container` is an owned-element `Vec` — a
    /// `Vec<Vec<T>>` / `Vec<HashMap>` / `Vec<HashSet>` / `Vec<heap-owning
    /// record/tuple>` whose scope-exit (or actor-state) release is
    /// `hew_vec_free_owned`, which RECURSES and frees every element handle,
    /// including `container[i]`, via the per-element descriptor drop thunk.
    ///
    /// #2545 — the sibling of [`Self::vec_iter_source_projects_actor_state_field`]
    /// for the LOCAL Vec-element case. `for v in rows[i]` lowers to
    /// `VecIter { vec: rows[i], idx }`; the `vec` source is an `Index` expression,
    /// not a bare `BindingRef` + `Capture`, so
    /// [`vec_iter_let_cursor_owns_handle`] classifies the cursor as the SOLE
    /// owner and registers it for a scope-exit `RecordFieldDrop` of `rows[i]`.
    /// But the container `rows` (`Vec<Vec<T>>`) is itself released by
    /// `hew_vec_free_owned`, whose recursion ALSO frees `rows[i]` — the exact
    /// double-free the poisoned allocator turns into an abort. Unlike a local
    /// RECORD field escaping into a cursor (`for v in localBox.v`), where the
    /// record's drop of the escaped field is elided (#2212) so the cursor is the
    /// lone freer, a Vec ELEMENT projection has NO such elision: the container's
    /// recursive drop is the exactly-once freer, so the cursor must BORROW.
    ///
    /// Keyed on the container's resolved type being an owned-element Vec —
    /// [`Self::binding_ty_is_owned_element_vec`], the SAME authority the
    /// constructor and the container's own `hew_vec_free_owned` release consult —
    /// so the borrow fires for exactly the containers whose recursion frees the
    /// indexed element (`dedup-semantic-boundary`). Covers a local binding, an
    /// actor-state field (whose recursive teardown lands in the same lane), and
    /// an rvalue container equally: whichever owner recurses is the sole freer.
    /// A plain `Vec<i64>` container is NOT owned-element, so `for v in flat[i]`
    /// (element `i64`, no per-element drop) is untouched and keeps its existing
    /// disposition. Fail-closed direction is a leak (borrow when the container
    /// does not in fact recurse), never the double-free.
    pub(crate) fn vec_iter_source_indexes_owned_element_vec(&self, value: &HirExpr) -> bool {
        let Some(src) = vec_iter_init_vec_source_expr(value) else {
            return false;
        };
        let HirExprKind::Index { container, .. } = &src.kind else {
            return false;
        };
        self.binding_ty_is_owned_element_vec(&self.subst_ty(&container.ty))
    }
    /// Close every `Stream<T>` / `Receiver<T>` for-await cursor declared in
    /// `scope_id` when that scope closes — the `Generator`/`VecIter` analogue of
    /// #1949 for the general `for await` consumption path. A `for await v in
    /// <stream> { ...; break; }` (or natural exhaustion) followed by more code
    /// in the same function otherwise leaves the stream open until the ENCLOSING
    /// FUNCTION returns; the producer stays parked on backpressure and its peer
    /// is never observed closed, so the function deadlocks on any subsequent
    /// blocking work. Closing at block-scope exit (where the `break`/`None`
    /// loop-exit edges both land, inside the desugar block) wakes the parked
    /// producer promptly.
    ///
    /// Mirrors [`Self::emit_scope_generator_drops`]: removes the binding from
    /// `scope_stream_bindings`, dispositions it `ScopeReleased` so the
    /// function-exit LIFO cannot fire a second close, and the inline
    /// `Instr::Drop` (`RuntimeSymbol` close path) null-stores the slot after the
    /// close (`raii-null-after-move`; `hew_stream_close` /
    /// `hew_channel_receiver_close` also null-guard, so a moved-out or
    /// already-closed slot is a no-op). The `Runtime(StreamClose | ReceiverClose)`
    /// spec matches the exit-plan close exactly.
    pub(crate) fn emit_scope_stream_drops(&mut self, scope_id: ScopeId) {
        let mut to_drop: Vec<(hew_hir::BindingId, ResolvedTy)> = Vec::new();
        self.scope_stream_bindings.retain(|(s, binding, ty)| {
            if *s == scope_id {
                to_drop.push((*binding, ty.clone()));
                false
            } else {
                true
            }
        });
        for (binding, ty) in to_drop.into_iter().rev() {
            let Some(place) = self.binding_locals.get(&binding).copied() else {
                continue;
            };
            let Some(descriptor) = stream_handle_drop_descriptor(&ty) else {
                continue;
            };
            self.set_owned_local_disposition(binding, Disposition::ScopeReleased);
            self.push_instr(Instr::Drop {
                place,
                ty,
                drop_fn: Some(crate::model::DropFnSpec::Runtime(descriptor)),
            });
        }
    }
    /// Close a `Stream<T>` / `Receiver<T>` for-await cursor opened INSIDE a loop
    /// body on a `break`/`continue`/`return` edge — the analogue of
    /// [`Self::emit_generator_drops_for_break_continue`] for the stream handle.
    /// A stream abandoned per-iteration must close on that edge, because the
    /// block-scope close on the fall-through path is never reached on
    /// break/continue/return. Break and continue pass their loop frame's scope
    /// depth; return passes zero because it leaves every enclosing loop.
    ///
    /// CLONE discipline (mirrors the generator handle release): entries are NOT
    /// removed from `scope_stream_bindings` — the mutually-exclusive
    /// fall-through path still closes via the block-scope drain, and the inline
    /// close's null-after-close makes a structurally-reachable second close a
    /// no-op (`raii-null-after-move`; the runtime symbols also null-guard).
    pub(crate) fn emit_stream_drops_for_exit_edge(&mut self, min_scope_depth: usize) {
        let window: HashSet<ScopeId> = self.active_scopes[min_scope_depth..]
            .iter()
            .copied()
            .collect();
        let to_drop: Vec<(Place, ResolvedTy)> = self
            .scope_stream_bindings
            .iter()
            .rev()
            .filter(|(s, _, _)| window.contains(s))
            .filter_map(|(_, binding, ty)| {
                self.binding_locals
                    .get(binding)
                    .copied()
                    .map(|place| (place, ty.clone()))
            })
            .collect();
        for (place, ty) in to_drop {
            let Some(descriptor) = stream_handle_drop_descriptor(&ty) else {
                continue;
            };
            self.push_instr(Instr::Drop {
                place,
                ty,
                drop_fn: Some(crate::model::DropFnSpec::Runtime(descriptor)),
            });
        }
    }
    /// Emit defers for an early-return path. Walks `active_scopes` from
    /// innermost to outermost, cloning and lowering each scope's pending
    /// defers in LIFO order. Uses clone (not remove) because the normal
    /// scope-exit path or a sibling control-flow branch may still need the
    /// defers — only the canonical `emit_pending_defers` at normal scope
    /// exit drains them permanently.
    ///
    /// Q205-B: mutable vars observe their final value at this program point
    /// because the defer bodies are lowered inline here. Moved/consumed
    /// bindings are caught by the MIR move-checker at the materialization
    /// site — a use-after-move in the defer body produces a checker-stream
    /// violation exactly as it would at a normal scope exit.
    pub(crate) fn emit_defers_for_return(&mut self) {
        // Walk from innermost scope to outermost (reverse of push order).
        for i in (0..self.active_scopes.len()).rev() {
            let scope_id = self.active_scopes[i];
            let Some(defers) = self.pending_defers.get(&scope_id) else {
                continue;
            };
            // Clone — do not drain; sibling branches may still need these.
            let defers = defers.clone();
            // LIFO within this scope.
            for body in defers.into_iter().rev() {
                let _ = self.lower_value(&body);
            }
        }
    }
    /// Emit defers for a `break`/`continue` path. Identical discipline to
    /// `emit_defers_for_return` but bounded to the *in-loop window*: only
    /// `active_scopes[loop_scope_depth..]` — the scopes opened inside the
    /// loop body — flush here. The loop's enclosing scopes stay untouched
    /// because break/continue does not leave them; they flush via their own
    /// `emit_pending_defers` at their natural exit.
    ///
    /// `loop_scope_depth` is the `scopes_depth_at_entry` recorded in the
    /// `loop_stack` entry (captured before the loop body scope was pushed),
    /// so the window includes the loop body scope and every nested block
    /// scope between it and the break/continue site.
    ///
    /// Clone (not drain): a `break` mid-body leaves the body's normal
    /// scope-exit `emit_pending_defers(body.scope)` to drain the map for the
    /// (now dead) fall-through path. Both paths flush exactly once at runtime
    /// because they are mutually exclusive in the CFG (LESSONS
    /// `cleanup-all-exits`).
    pub(crate) fn emit_defers_for_break_continue(&mut self, loop_scope_depth: usize) {
        // Walk the in-loop window from innermost scope outward.
        for i in (loop_scope_depth..self.active_scopes.len()).rev() {
            let scope_id = self.active_scopes[i];
            let Some(defers) = self.pending_defers.get(&scope_id) else {
                continue;
            };
            // Clone — the natural scope-exit drain still needs these.
            let defers = defers.clone();
            // LIFO within this scope.
            for body in defers.into_iter().rev() {
                let _ = self.lower_value(&body);
            }
        }
    }
    /// Emit `hew_gen_coro_destroy` for every generator binding declared in the
    /// in-loop window (`active_scopes[loop_scope_depth..]`) on a
    /// `break`/`continue` edge. Without this, a generator declared in a loop
    /// body and then skipped past by `continue` (or escaped by `break`) leaks
    /// its coro frame + heap companion for that iteration, because the
    /// back-edge `emit_scope_generator_drops` on the normal fall-through path
    /// is never reached on the break/continue path.
    ///
    /// CLONE discipline (mirrors `emit_defers_for_break_continue`): the entries
    /// are NOT removed from `scope_generator_bindings` here, because the normal
    /// scope-exit close still drains them for the fall-through path. The two
    /// paths are mutually exclusive in the CFG, so each runtime path frees once;
    /// the inline drop's null-after-free makes any structurally-reachable second
    /// drop a no-op (`raii-null-after-move`; the runtime also null-guards).
    pub(crate) fn emit_generator_drops_for_break_continue(&mut self, loop_scope_depth: usize) {
        let window: HashSet<ScopeId> = self.active_scopes[loop_scope_depth..]
            .iter()
            .copied()
            .collect();
        let to_drop: Vec<(Place, ResolvedTy)> = self
            .scope_generator_bindings
            .iter()
            .rev()
            .filter(|(s, _, _)| window.contains(s))
            .filter_map(|(_, binding, ty)| {
                self.binding_locals
                    .get(binding)
                    .copied()
                    .map(|place| (place, ty.clone()))
            })
            .collect();
        for (place, ty) in to_drop {
            self.push_instr(Instr::Drop {
                place,
                ty,
                drop_fn: Some(crate::model::DropFnSpec::Release("hew_gen_coro_destroy")),
            });
        }
    }
    /// Emit a per-iteration drop for every active generator-yielded heap value
    /// that is lexically inside the loop a `break`/`continue` is leaving, so the
    /// break/continue ITERATION's yielded value (`Vec`/`string`/map) is freed on
    /// that edge instead of leaking. This is the value-side companion to
    /// `emit_generator_drops_for_break_continue` (which frees the generator
    /// HANDLE): the body-end drop (`emit_generator_yield_binding_drop`) is
    /// emitted AFTER the body lowers, so a `break`/`continue` jumps past it and
    /// the iteration that breaks/continues would otherwise leak its value.
    ///
    /// CLONE discipline (mirrors the handle release): entries are NOT removed —
    /// the mutually-exclusive fall-through path still frees via the body-end
    /// drop, and the inline drop's null-after-free makes a structurally-
    /// reachable second free a no-op (`raii-null-after-move`; the runtime also
    /// null-guards). An entry registered at `active_scopes` depth >=
    /// `loop_scope_depth` is inside the breaking loop's body window.
    /// Free the per-iteration yielded heap value(s) on a `break`/`continue`
    /// edge so the breaking/continuing iteration's payload is not leaked
    /// (the body-end drop is past the break — would never run on the break
    /// edge). Restricted to entries inside the breaking loop's scope window
    /// (`active_scopes` depth >= `loop_scope_depth`).
    ///
    /// Escape-aware: for each candidate entry, consults
    /// `generator_yield_binding_drop_safe` over the body window from the
    /// binding site to the current instructions buffer. When the value's
    /// slot has been MOVED into another local mid-body (e.g.
    /// `for await item in rx { carry = item; break; }`), the consumer (the
    /// move destination) owns the release; an unconditional break-edge drop
    /// would double-free the buffer at the move-out site (a use-after-free
    /// at every read of the move destination that happens after the break).
    /// The scan refuses such drops — leak-not-double-free, matching the
    /// body-end drop's discipline. (Before this fix the recv `Some(item)`
    /// path never registered an entry here so the conflict was masked;
    /// extending Phase F to recv-call scrutinees exposes the case so the
    /// escape filter is now load-bearing.)
    ///
    /// CLONE discipline (entries are NOT removed): the mutually-exclusive
    /// fall-through path still emits the body-end drop, which has its own
    /// escape scan; the inline drop's null-after-free makes a structurally-
    /// reachable second free a no-op (`raii-null-after-move`; the runtime
    /// also null-guards).
    ///
    /// EXIT-EDGE FAMILY (`cleanup-all-exits`): `break` and `continue` pass
    /// their loop frame's `scope_depth` (free only the entries inside the loop
    /// being left); an early `return` passes `min_scope_depth = 0` because a
    /// return leaves EVERY enclosing consuming body — each active entry is the
    /// current iteration's live value and must be freed on that edge. The same
    /// per-entry escape scan protects all three edges: `return v` has already
    /// moved the value into `Place::ReturnSlot`, so the scan sees the Move and
    /// refuses the drop (the caller owns the release).
    pub(crate) fn emit_generator_yield_value_drops_for_exit_edge(
        &mut self,
        min_scope_depth: usize,
    ) {
        let to_drop: Vec<(Place, ResolvedTy, crate::model::DropFnSpec, u32, usize)> = self
            .active_generator_yield_values
            .iter()
            .rev()
            .filter(|(depth, _, _, _, _, _)| *depth >= min_scope_depth)
            .map(|(_, place, ty, drop_fn, start_block_id, start_instr_len)| {
                (
                    *place,
                    ty.clone(),
                    drop_fn.clone(),
                    *start_block_id,
                    *start_instr_len,
                )
            })
            .collect();
        for (place, ty, drop_fn, start_block_id, start_instr_len) in to_drop {
            let Some(local) = base_local(place) else {
                continue;
            };
            if !self.generator_yield_binding_drop_safe(start_block_id, start_instr_len, local) {
                // Value escaped the body (Move-out, store-into-aggregate,
                // consuming terminator); the move-destination owns the
                // release. Skipping is leak-not-double-free posture, but
                // for an actually-escaped value it is correct: the consumer
                // releases at its own drop site, no leak.
                continue;
            }
            self.push_instr(Instr::Drop {
                place,
                ty,
                drop_fn: Some(drop_fn),
            });
        }
    }
    pub(crate) fn resolve_loop_frame(
        &mut self,
        label: Option<&str>,
        keyword: &'static str,
        site: hew_hir::SiteId,
    ) -> Option<LoopFrame> {
        let frame = match label {
            Some(name) => self
                .loop_stack
                .iter()
                .rev()
                .find(|frame| frame.label.as_deref() == Some(name)),
            None => self.loop_stack.last(),
        };
        if let Some(frame) = frame {
            return Some(frame.clone());
        }

        let reason = match label {
            Some(name) => format!("{keyword} targets unknown loop label `@{name}`"),
            None => format!("{keyword} used outside of a loop"),
        };
        self.diagnostics.push(MirDiagnostic {
            kind: MirDiagnosticKind::UnsupportedNode {
                reason: reason.clone(),
            },
            note: format!(
                "{reason} at site {site:?}; the type checker should reject this before MIR lowering"
            ),
        });
        None
    }
}
