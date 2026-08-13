//! Assignment-statement lowering (`x = <rhs>`, field/index/actor-state
//! targets) — carved from `expr.rs` as a sibling concern module (line-ceiling
//! ratchet). Pure move: the single `assign` entry point keeps every
//! assignment boundary rule in one exhaustive match.
use super::{
    runtime_symbol_for_call_expr, user_record_layout_key, Builder, BuiltinType, CmpPred,
    Disposition, FieldOffset, HirExpr, HirExprKind, Instr, MirDiagnostic, MirDiagnosticKind, Place,
    ResolvedRef, ResolvedTy, Terminator, TrapKind, ValueClass,
};
use crate::model::ActorStateStoreHandoff;

impl Builder {
    #[allow(
        clippy::too_many_lines,
        reason = "one exhaustive match keeps assignment boundary rules together"
    )]
    pub(crate) fn assign(&mut self, target: &HirExpr, value: &HirExpr) {
        if let HirExprKind::BindingRef {
            name,
            resolved: ResolvedRef::Binding(binding),
        } = &target.kind
        {
            if self
                .vec_iter_borrowed_sources
                .iter()
                .any(|(_, source)| source == binding)
            {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!(
                            "reassigning `{name}` while a VecIter cursor borrows it"
                        ),
                        site: target.site,
                    },
                    note: "the active for-loop cursor reads this Vec's handle directly; \
                           overwriting the source would release that handle while the cursor \
                           can still execute. Reassign the Vec before entering the loop, or \
                           wait until the loop has finished"
                        .to_string(),
                });
                return;
            }
        }
        // A record-field store whose target path is a borrowed field-projection
        // cursor's source — or ANY prefix of it — overwrites the slot the
        // borrowed handle lives in. The cursor's own handle copy (cursor
        // field 0) keeps dereferencing the replaced handle's storage on every
        // `next`, so a bypass here is a use-after-free the moment the old
        // handle is released (the poisoned-allocator abort). The whole-root
        // guard above already rejects `root = …`; this is the same boundary
        // one projection level down. Element-level mutation through the SAME
        // handle (`root.f[i] = …`, `.push`, `.clear`) stays allowed — `next`
        // re-loads the handle and re-probes the length, so in-place mutation
        // is a live view, never a stale pointer.
        if let Some((target_root, target_path, rendered)) = field_store_target_path(target) {
            if self
                .vec_iter_borrowed_projections
                .iter()
                .any(|(_, borrowed_root, borrowed_path)| {
                    *borrowed_root == target_root
                        && target_path.len() <= borrowed_path.len()
                        && borrowed_path[..target_path.len()] == target_path[..]
                })
            {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!(
                            "assigning `{rendered}` while a VecIter cursor borrows it"
                        ),
                        site: target.site,
                    },
                    note: "the active for-loop cursor reads this field's Vec handle directly; \
                           storing to the projected field (or any record containing it) would \
                           replace that handle while the cursor can still execute. Mutate \
                           elements through Vec methods or index assignment, or store after \
                           the loop has finished"
                        .to_string(),
                });
                return;
            }
        }
        // A `string`/`bytes` assignment whose RHS is an ACTIVE yield binder is
        // a retained SHARE, mirroring the `let x = <binding>` share-site
        // registration above in `stmt`. The binder's release authority is the
        // per-iteration body-end drop, so a `Consume` intent here suppresses
        // no scope-exit drop — it only poisons the dataflow (`MaybeConsumed`
        // joins → the vec-iter abandonment NYI wall) and strands the
        // post-pass share retain without a balancing destination release
        // (`ObligationUnderReleased`). Registering the share site downgrades
        // the use intent to `Read`; the sole-owner derivation then mints the
        // one retain and admits the destination's own drop.
        let mut yield_share_move = false;
        if let (
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(src_binding),
                ..
            },
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(target_binding),
                ..
            },
        ) = (&value.kind, &target.kind)
        {
            if matches!(self.subst_ty(&value.ty), ResolvedTy::String) {
                if let Some(place) = self.binding_locals.get(src_binding).copied() {
                    if self.active_yield_binder_place(place) {
                        self.string_local_share_sites
                            .insert(value.site, (*src_binding, *target_binding));
                        yield_share_move = true;
                    }
                }
            }
        }
        let copy_in = self.assign_target_stays_copy_in(target, value);
        let src = if copy_in {
            self.lower_value(value)
        } else {
            self.lower_value_for_move(value)
        };
        let Some(src) = src else {
            return;
        };
        // A direct assignment from an owned tuple projection transfers that
        // field into the assignment target. Tuple field loads byte-copy
        // non-string heap owners, so clear the source slot after the load; the
        // tuple keeps its remaining-field drop while the reassigned binding
        // becomes the sole owner of the extracted field.
        if !copy_in {
            if let HirExprKind::TupleIndex { tuple, index } = &value.kind {
                if let HirExprKind::BindingRef {
                    resolved: ResolvedRef::Binding(source_binding),
                    ..
                } = &tuple.kind
                {
                    let source_is_owned = self.owned_locals.iter().any(|entry| {
                        entry.binding == *source_binding
                            && entry.disposition == Disposition::ScopeExit
                    });
                    let field_ty = self.subst_ty(&value.ty);
                    let field_transfers = source_is_owned
                        && !matches!(field_ty, ResolvedTy::String)
                        && crate::model::ty_owns_heap_mir(
                            &field_ty,
                            &self.record_field_orders,
                            &self.enum_layouts,
                        );
                    if field_transfers {
                        if let Ok(field) = u32::try_from(*index) {
                            let source_root =
                                self.instructions
                                    .iter()
                                    .rev()
                                    .find_map(|instr| match instr {
                                        Instr::TupleFieldLoad {
                                            tuple,
                                            field_index,
                                            dest,
                                        } if *dest == src && *field_index == field => Some(*tuple),
                                        _ => None,
                                    });
                            if let Some(source_root) = source_root {
                                let already_neutralized = self.instructions.iter().any(|instr| {
                                    matches!(
                                        instr,
                                        Instr::AggregateProjectionNeutralize { root, fields, .. }
                                            if *root == source_root && fields.as_slice() == [field]
                                    )
                                });
                                if !already_neutralized {
                                    self.push_instr(Instr::AggregateProjectionNeutralize {
                                        root: source_root,
                                        fields: vec![field],
                                        transferee: src,
                                        scope_exit_owner: None,
                                    });
                                }
                            }
                        }
                    }
                }
            }
        }
        if let Some((field_offset, _)) = self.actor_state_field_for_target(target) {
            self.instructions.push(Instr::ActorStateFieldStore {
                field_offset,
                src,
                handoff: ActorStateStoreHandoff::ConsumeSource,
            });
            return;
        }
        match &target.kind {
            HirExprKind::ResolvedImplCall {
                receiver,
                method_name,
                target_symbol,
                target_family,
                type_args,
                args,
                ..
            } if matches!(
                target_family,
                hew_types::MethodTargetFamily::Vec(hew_types::VecMethod::Set)
            ) =>
            {
                if type_args.len() != 1 {
                    unreachable!(
                        "vec `.{method_name}` resolved to family {target_family:?} with {} \
                         type_args; Vec impls are registered with one element type",
                        type_args.len()
                    );
                }
                if self.vec_receiver_has_drop_only_receiver_element(&receiver.ty) {
                    let _ = self
                        .reject_drop_only_receiver_vec_operation("index set copy-in", target.site);
                    return;
                }
                let Some(receiver_place) = self.lower_value(receiver) else {
                    return;
                };
                let Some(index_arg) = args.first() else {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::UnsupportedNode {
                            reason: "checker-resolved Vec set target has no index argument"
                                .to_string(),
                        },
                        note: "Vec index assignment lowering requires exactly one index argument"
                            .to_string(),
                    });
                    return;
                };
                if args.len() != 1 {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::UnsupportedNode {
                            reason: format!(
                                "checker-resolved Vec set target has {} index arguments",
                                args.len()
                            ),
                        },
                        note: "Vec index assignment lowering requires exactly one index argument"
                            .to_string(),
                    });
                    return;
                }
                let Some(raw_index_place) = self.lower_value(index_arg) else {
                    return;
                };
                let index_place = if let Place::Local(raw_id) = raw_index_place {
                    let raw_ty = self.locals[raw_id as usize].clone();
                    match raw_ty {
                        ResolvedTy::I8 | ResolvedTy::I16 | ResolvedTy::I32 => {
                            let wide_place = self.alloc_local(ResolvedTy::I64);
                            self.push_instr(Instr::NumericCast {
                                dest: wide_place,
                                src: raw_index_place,
                                from_ty: raw_ty,
                                to_ty: ResolvedTy::I64,
                            });
                            wide_place
                        }
                        _ => raw_index_place,
                    }
                } else {
                    raw_index_place
                };
                let len_place = self.alloc_local(ResolvedTy::I64);
                self.push_instr(Instr::CallRuntimeAbi(
                    crate::model::RuntimeCall::new(
                        "hew_vec_len",
                        vec![receiver_place],
                        Some(len_place),
                    )
                    .expect("hew_vec_len is an allowlisted runtime symbol"),
                ));
                let oob_flag = self.alloc_local(ResolvedTy::Bool);
                self.push_instr(Instr::IntCmp {
                    dest: oob_flag,
                    pred: CmpPred::UnsignedGreaterEq,
                    lhs: index_place,
                    rhs: len_place,
                });
                let trap_bb = self.alloc_block();
                let cont_bb = self.alloc_block();
                self.finish_current_block(Terminator::Branch {
                    cond: oob_flag,
                    then_target: trap_bb,
                    else_target: cont_bb,
                });
                self.start_block(trap_bb);
                self.finish_current_block(Terminator::Trap {
                    kind: TrapKind::IndexOutOfBounds,
                });
                self.start_block(cont_bb);
                self.enforce_closure_pair_ingress(value);
                let arg_places = vec![receiver_place, index_place, src];
                let next = self.alloc_block();
                // A Vec index-assignment of a fresh materialised rvalue
                // (`v[i] = Name { .. }`, `v[i] = make()`) or a consumed bound
                // local (`v[i] = h`) has the SAME source-owner hole:
                // `hew_vec_set_owned` is COPY-IN (deep-clones the element into
                // the slot), but neither source has a later drop to balance that
                // clone. The constructor temp is unbound, while checked MIR
                // marks the local `Use { Consume }` and suppresses its scope-exit
                // drop. Route either sole-owner source to the MOVE-in sibling
                // `hew_vec_set_owned_move` (byte-transfers the element's heap
                // into the slot without a clone; the source is then dead).
                // `expr_is_materialized_owner` is the identical fresh-rvalue
                // predicate the `.set()`/push paths use. The second predicate is
                // deliberately narrower: only a non-parameter, non-capture
                // `BindingRef` whose effective MIR use intent is `Consume`
                // moves. A borrowed/read binding stays COPY-IN, as do
                // constructions embedding a whole by-value parameter.
                let effective_symbol = if self.vec_set_owned_assign_moves_rhs(target_symbol, value)
                {
                    "hew_vec_set_owned_move"
                } else {
                    target_symbol.as_str()
                };
                // `effective_symbol` is selected from the checker's resolved
                // Vec method family, including the owned-move refinement above;
                // carry its closed runtime identity rather than asking codegen
                // to recognise a linker spelling.
                let builtin =
                    hew_types::runtime_call::RuntimeCallFamily::from_c_symbol(effective_symbol);
                self.finish_current_block(Terminator::Call {
                    callee: effective_symbol.to_string(),
                    authority: builtin
                        .map(crate::CallAuthority::Runtime)
                        .unwrap_or_default(),
                    args: arg_places,
                    dest: None,
                    next,
                });
                self.start_block(next);
            }
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(binding),
                name,
                ..
            } => {
                if let Some(dest) = self.binding_locals.get(binding).copied() {
                    let cursor_flag = self.vec_iter_drop_flags.get(binding).copied();
                    let cursor_value_flag =
                        self.vec_iter_value_drop_flags.get(&value.site).copied();
                    // #2420 -- the overwrite release below is sound ONLY when
                    // the incoming value cannot alias the outgoing value's
                    // heap. An RHS that reads the reassigned binding (`s =
                    // grow(s)`, `s = S { n: s.n + 1, v: s.v }`) can hand back
                    // an UN-RETAINED alias of the old value's owned fields:
                    // by-value heap params are BORROWS and a non-`string`
                    // owned field load is a raw pointer copy, so releasing the
                    // old value here frees storage the incoming value still
                    // references -- use-after-free on the next field use and a
                    // double-free at the next release. When the RHS may alias,
                    // skip the release on BOTH the static and the flag-gated
                    // paths: fail-open (leak) is this seam's documented
                    // posture, matching the scope-exit exclusion
                    // (`derive_owned_record_drop_allowed`) for the identical
                    // aliasing channel. WHEN-OBSOLETE: the COW retain-on-share
                    // spine (every share retained => release always sound).
                    let rhs_may_alias_old = self.reassign_rhs_may_alias_binding(value, *binding);
                    // #53 / #2301: release the prior heap-owning value before
                    // the slot is overwritten.
                    if let Some(flag) = cursor_flag {
                        // RHS lowering snapshots its ownership bit before
                        // neutralizing a moved source. Consequently this guard
                        // also handles self-assignment: its source bit is
                        // already moved, so the old bytes are not released,
                        // then the saved bit is restored below.
                        self.emit_flag_gated_vec_iter_cursor_release(*binding, &target.ty);
                        self.push_instr(Instr::Move { dest, src });
                        if let Some(value_flag) = cursor_value_flag {
                            self.push_instr(Instr::Move {
                                dest: flag,
                                src: value_flag,
                            });
                        } else {
                            let owns_snapshot = self.vec_iter_value_is_owned(value);
                            self.push_instr(Instr::ConstI64 {
                                dest: flag,
                                value: i64::from(!owns_snapshot),
                            });
                        }
                    } else if let Some(flag) = self.overwrite_guard_flags.get(binding).copied() {
                        // #2301 -- `binding` is consumed on one control-flow path
                        // and overwritten on another. The consume removed it from
                        // `owned_locals` globally, so the static gate below would
                        // wrongly SKIP the release on the non-consuming path and
                        // leak the still-owned old value. Gate on the runtime
                        // flag instead: release iff `flag == 0`; the consume set
                        // `flag = 1` to hand the value to its new owner. Reset to
                        // 0 after the store so the fresh value is released on the
                        // next overwrite.
                        if !rhs_may_alias_old {
                            self.emit_flag_gated_overwrite_release(
                                *binding, dest, &target.ty, flag, value,
                            );
                        }
                        if yield_share_move {
                            self.push_instr(Instr::StringRetain {
                                value: src,
                                condition: crate::model::StringRetainCondition::FreshShare,
                            });
                            self.yield_share_instr_exempt
                                .insert((self.current_block_id, self.instructions.len()));
                        }
                        self.push_instr(Instr::Move { dest, src });
                        self.push_instr(Instr::ConstI64 {
                            dest: flag,
                            value: 0,
                        });
                        if super::ty_is_heap_owning_enum_composite(
                            &self.subst_ty(&target.ty),
                            &self.record_field_orders,
                            &self.enum_layouts,
                            self.type_classes.lifecycle_registry(),
                        ) {
                            // The reassignment constructs a fresh live enum
                            // generation. Restore its scope-exit obligation;
                            // per-exit dataflow excludes the consumed path and
                            // the same overwrite flag guards a shared
                            // MaybeConsumed join.
                            self.set_owned_local_disposition(*binding, Disposition::ScopeExit);
                        }
                    } else {
                        // #53: gated on the binding still owning live heap
                        // (scope-exit-live `owned_locals` membership) -- a
                        // self-reassign r = T{..r} or a move-out RHS already
                        // consumed it (dispositioned off the scope-exit set by
                        // `mark_binding_moved`, so absent from the live view),
                        // so this is skipped and never double-frees.
                        if !rhs_may_alias_old
                            && self.owned_locals.iter().any(|entry| {
                                &entry.binding == binding
                                    && entry.disposition == Disposition::ScopeExit
                            })
                        {
                            self.emit_local_overwrite_release(dest, &target.ty);
                            self.emit_enum_overwrite_release(*binding, dest, &target.ty, value);
                        }
                        if yield_share_move {
                            self.push_instr(Instr::StringRetain {
                                value: src,
                                condition: crate::model::StringRetainCondition::FreshShare,
                            });
                            self.yield_share_instr_exempt
                                .insert((self.current_block_id, self.instructions.len()));
                        }
                        self.push_instr(Instr::Move { dest, src });
                    }
                    // A simple-variable assignment RE-DEFINES its target: after
                    // `h = <rhs>` the binding `h` holds a fresh value and is
                    // unconditionally Live, regardless of any move/consume the
                    // RHS performed on `h` itself. Emit a checker-stream `Bind`
                    // so move-state tracking resets `h` to Live. Without this the
                    // self-consuming reassign idiom `h = T { ..h, f: new }`
                    // (the canonical functional-update loop body) would leave `h`
                    // flagged `Consumed` from the `..h` ingress and every
                    // subsequent read — including the next loop iteration — would
                    // spuriously trip `UseAfterConsume`. This re-`Bind` carries no
                    // drop semantics (it does not touch `owned_locals`, which is
                    // populated only at `let`/param sites), so scope-exit drop
                    // accounting for `h` is unchanged.
                    self.push_bind_statement(
                        *binding,
                        name.clone(),
                        target.site,
                        self.subst_ty(&target.ty),
                    );
                    // Assignment moves the RHS generation into an already-owned
                    // destination slot. If typed publication provisionally
                    // adopted the RHS temp, retire that exact source-place owner
                    // now; the destination binding remains the sole authority
                    // whose drop plan fans out across exits.
                    self.retire_provisional_owner_after_assignment_move(
                        *binding, dest, &target.ty, src, &value.ty,
                    );
                    // Generation boundary: a straight-line (top-level-scope)
                    // reassignment of a caller-borrowed parameter slot replaces
                    // the caller's value with a frame-owned one; the borrowed
                    // registries must stop answering for the slot so later
                    // ownership decisions (the collection-ingress retain, the
                    // outbound no-transfer guard, return-share derivation) see
                    // the frame-owned generation. Conditional reassignment
                    // keeps the registration — fail-closed toward
                    // retain-and-leak, never a double release.
                    if self.active_scopes.len() == 1 {
                        self.deregister_reassigned_borrowed_param(*binding);
                    }
                } else if let Some(source) = self.capture_env_sources.get(binding).cloned() {
                    // #1′ BorrowMut write-back: the assignment target is a
                    // captured `var` reassigned inside the closure body
                    // (`var total; |n| { total = total + n; total }`). The
                    // binding has no `binding_locals` slot — it lives in the
                    // closure env — so the write lands in the env field via the
                    // store twin of `ClosureEnvFieldLoad`. The env owns the
                    // mutable scalar (Option B): mutations accumulate across
                    // calls through the persistent env pointer, and the caller's
                    // original binding is independent.
                    //
                    // Restricted to `BitCopy` scalar fields. An owned captured
                    // field (string/Vec/record) would leak its prior value on
                    // overwrite without an env-field release — out of scope for
                    // the non-suspend scalar write-back path — so fail closed
                    // with a spanned diagnostic rather than emit a
                    // silently-leaking store.
                    let field_class = ValueClass::of_ty(&source.ty, &self.type_classes);
                    if field_class == ValueClass::BitCopy {
                        self.push_instr(Instr::ClosureEnvFieldStore {
                            env: source.env,
                            env_ty: source.env_ty,
                            field_offset: source.field_offset,
                            src,
                        });
                    } else {
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct: format!(
                                    "reassigning owned captured `{name}` inside a closure"
                                ),
                                site: target.site,
                            },
                            note: format!(
                                "captured `{name}` has a non-`BitCopy` type ({:?}); the closure-env \
                                 write-back supports scalar captures only — an owned field would \
                                 need an overwrite-release of its prior value",
                                source.ty
                            ),
                        });
                    }
                } else {
                    if self.poisoned_let_bindings.contains(binding) {
                        // The binding's `let`/`var` initializer already failed
                        // to lower and reported the root error; this write is
                        // pure cascade. Stay silent (the compile already
                        // fails) instead of stacking an `UnresolvedPlace`
                        // follow-on. Mirrors the read-arm guard above.
                        return;
                    }
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::UnresolvedPlace {
                            binding: *binding,
                            name: name.clone(),
                            site: target.site,
                        },
                        note: format!("assignment target binding {binding:?} has no MIR place"),
                    });
                }
            }
            // Record field-store: `r.x = src` lowers to a GEP+store on the
            // record's alloca (Q297 Stage 1, Q299=(a)). The aggregate `r`
            // stays `Live` after the store — only the named field's bytes
            // are overwritten.
            //
            // Note: the checker-side mutability gate is what restricts the
            // surface to `var`-bound records and `var self`-bound impl
            // methods; reaching MIR with a non-mutable target is impossible
            // (the checker would have already reported and produced
            // Ty::Error / a cascading skip). MIR's role here is purely
            // structural: resolve the field name → offset and emit the
            // store.
            HirExprKind::FieldAccess { object, field } => {
                let Some(record_place) = self.lower_value(object) else {
                    return;
                };
                let object_ty = self.subst_ty(&object.ty);
                // Resolve every record class through the typed authority.
                // User records preserve their canonical nominal owner;
                // compiler cursors select the reserved synthetic class from
                // their builtin discriminator and concrete arguments.
                let type_name = match &object_ty {
                    ResolvedTy::Named { name, .. } => {
                        user_record_layout_key(&object_ty).unwrap_or_else(|| name.clone())
                    }
                    other => {
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::UnsupportedNode {
                                reason: format!(
                                    "field-store on non-named type `{other:?}` (only \
                                     named record types are supported)"
                                ),
                            },
                            note: "field-store target object has an unsupported type".to_string(),
                        });
                        return;
                    }
                };
                let Some(field_order) = self.lookup_record_field_order(type_name.as_str()) else {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::UnsupportedNode {
                            reason: format!(
                                "field-store on unregistered record type `{type_name}`"
                            ),
                        },
                        note: "record type was not found in the field-order table; \
                               this is a checker bug"
                            .to_string(),
                    });
                    return;
                };
                let Some(idx) = field_order.iter().position(|(f, _)| f == field.as_str()) else {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::UnsupportedNode {
                            reason: format!(
                                "field-store on unknown field `{field}` of \
                                 record `{type_name}`"
                            ),
                        },
                        note: "field not found in declaration-order table; \
                               this is a checker bug"
                            .to_string(),
                    });
                    return;
                };
                let field_offset = FieldOffset(
                    u32::try_from(idx).expect("field index exceeds u32::MAX — impossible in Hew"),
                );
                self.push_instr(Instr::RecordFieldStore {
                    record: record_place,
                    field_offset,
                    src,
                });
                if !copy_in {
                    self.transfer_typed_produced_value_owner(value.site, src, record_place);
                }
            }
            // `xs[i] = v` over a `Vec<T>` lowers to the same runtime call that
            // `xs.set(i, v)` emits.
            HirExprKind::Index { container, index }
                if self.subst_ty(&container.ty).is_builtin(BuiltinType::Vec) =>
            {
                let Some(vec_place) = self.lower_value(container) else {
                    return;
                };
                let Some(index_place) = self.lower_value(index) else {
                    return;
                };
                let Some(symbol) =
                    runtime_symbol_for_call_expr(target).map(|(symbol, _, _)| symbol)
                else {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::UnsupportedNode {
                            reason: "Vec index assignment reached MIR without a resolved Vec set \
                                     runtime call"
                                .to_string(),
                        },
                        note: "checker must record the Vec set call at the index target span"
                            .to_string(),
                    });
                    return;
                };
                let next = self.alloc_block();
                // The index-assignment target came from the checker-owned
                // collection method family; preserve that closed runtime
                // identity on the call terminator.
                let builtin = hew_types::runtime_call::RuntimeCallFamily::from_c_symbol(&symbol);
                self.finish_current_block(Terminator::Call {
                    callee: symbol,
                    authority: builtin
                        .map(crate::CallAuthority::Runtime)
                        .unwrap_or_default(),
                    args: vec![vec_place, index_place, src],
                    dest: None,
                    next,
                });
                self.start_block(next);
            }
            // `m[k] = v` over a `HashMap<K, V>` lowers to the same
            // `hew_hashmap_insert_layout(map, key, val)` runtime call that
            // `m.insert(k, v)` emits, discarding the returned `bool` (the
            // index-assignment surface has no "was-new" result). The checker
            // accepted this target with value type `V`, so `src` already holds
            // a `V`-typed value. The container/index were NOT pre-lowered (the
            // outer `assign` only lowered `value`), so lower them here.
            HirExprKind::Index { container, index }
                if self
                    .subst_ty(&container.ty)
                    .is_builtin(BuiltinType::HashMap) =>
            {
                let Some(map_place) = self.lower_value(container) else {
                    return;
                };
                let Some(key_place) = self.lower_value(index) else {
                    return;
                };
                let next = self.alloc_block();
                // The callee identity is the typed family; the symbol string
                // is derived from the catalog bijection at construction so
                // the two can never drift.
                let family = hew_types::runtime_call::RuntimeCallFamily::HashMapInsertLayout;
                self.finish_current_block(Terminator::Call {
                    callee: family.c_symbol().to_string(),
                    authority: crate::CallAuthority::Runtime(family),
                    args: vec![map_place, key_place, src],
                    dest: None,
                    next,
                });
                self.start_block(next);
            }
            _ => self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::UnsupportedNode {
                    reason: "only local bindings, record fields, actor state fields, and \
                         Vec/HashMap index targets are assignable in MIR slice 4"
                        .to_string(),
                },
                note: "assignment target did not lower to a writable place".to_string(),
            }),
        }
    }
}

/// The `(root binding, field path, rendered spelling)` of a record-field
/// store target — a pure `FieldAccess` chain over a local `BindingRef` root
/// (`outer.inner.items = …` → `(outer, ["inner", "items"],
/// "outer.inner.items")`). Mirrors the source-side walk in
/// [`Builder::vec_iter_source_live_binding_record_field_path`] so the
/// borrowed-projection store guard compares like against like. `None` for any
/// other target shape (bare bindings, index targets, actor state) — those
/// have their own guards or lowering arms.
fn field_store_target_path(target: &HirExpr) -> Option<(hew_hir::BindingId, Vec<String>, String)> {
    let mut cur = target;
    let mut path: Vec<String> = Vec::new();
    loop {
        match &cur.kind {
            HirExprKind::SubsumedValue { source, .. } => cur = source,
            HirExprKind::FieldAccess { object, field } => {
                path.push(field.clone());
                cur = object;
            }
            HirExprKind::BindingRef {
                name,
                resolved: ResolvedRef::Binding(id),
            } => {
                if path.is_empty() {
                    return None;
                }
                path.reverse();
                let rendered = format!("{name}.{}", path.join("."));
                return Some((*id, path, rendered));
            }
            _ => return None,
        }
    }
}
