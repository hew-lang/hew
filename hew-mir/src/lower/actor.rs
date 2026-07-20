#[cfg(test)]
use super::collapse_actor_send_aliasing_to_idx0;
use super::{
    actor_name_from_handle_ty, actor_name_from_remote_pid_ty, is_self_expr,
    is_unit_close_error_result, is_unit_send_error_result, method_name_from_id,
    recv_result_payload_ty, ActorLayout, ActorMethodInfo, Builder, BuiltinType, CmpPred,
    FieldOffset, FloatWidth, FungibleChildRef, HashMap, HirExpr, HirExprKind, Instr, MirDiagnostic,
    MirDiagnosticKind, Place, ReleaseSymbolVerdict, ResolvedTy, RuntimeCallContext, SuspendKind,
    Terminator, CHILD_LOOKUP_RESULT_TY_NAME, RECEIVE_GEN_STREAM_CAPACITY,
};

impl Builder {
    pub(crate) fn actor_state_field_for_target(
        &self,
        expr: &HirExpr,
    ) -> Option<(FieldOffset, ResolvedTy)> {
        match &expr.kind {
            HirExprKind::BindingRef { name, .. } => {
                self.current_actor_state_fields.get(name).cloned()
            }
            HirExprKind::FieldAccess { object, field } if is_self_expr(object) => {
                self.current_actor_state_fields.get(field).cloned()
            }
            _ => None,
        }
    }

    /// Lower `link(target)` / `monitor(target)` to `Instr::CallRuntimeAbi`,
    /// constructing the composite return value (`Result<(), LinkError>` or
    /// `MonitorRef`) when the call is in value-needed context.
    ///
    /// **Statement-position** (`context == Discarded`): emit the ABI call with
    /// `dest = None`. The codegen handler calls `hew_actor_link` / discards the
    /// `hew_actor_monitor` u64 return — no composite needed.
    ///
    /// **Value-needed** (`context == ValueNeeded`): two shapes:
    ///
    /// - `link` (void runtime ABI): allocate a `Result<(), LinkError>` dest
    ///   local from the checker-authoritative `result_ty`, emit the ABI call
    ///   with `dest = Some(result_local)`. The codegen handler (`runtime_abi.rs`
    ///   `ActorLink` arm) detects the dest and calls `emit_result_ok(dest, None)`
    ///   to write `tag = 0` (Ok, no payload) — the only shape because
    ///   `hew_actor_link` is void/infallible at the runtime today.
    ///
    /// - `monitor` (i64 runtime ABI → `MonitorRef` struct): allocate a raw `i64`
    ///   dest for the runtime return, emit the ABI call storing the `ref_id` into
    ///   it, then emit `Instr::RecordInit` to assemble `MonitorRef { ref_id }`
    ///   into a freshly-allocated `MonitorRef` local from `result_ty`. Returns
    ///   the `MonitorRef` local.
    ///
    /// **Null-self window (INTERIM):** `hew_actor_self()` returns null outside an
    /// actor dispatch context. The "only valid in actor context" fail-closed gate
    /// belongs to the actor-messaging lane and is not installed here. Calls from
    /// `main` / free functions are an out-of-context window; the runtime handles
    /// null self gracefully (`hew_actor_monitor` returns `ref_id = 0`).
    pub(crate) fn lower_actor_link_or_monitor(
        &mut self,
        symbol: &str,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
        result_ty: Option<&ResolvedTy>,
    ) -> Option<Place> {
        // ARITY: the user-facing surface is 1-arg — `link(target)` /
        // `monitor(target)`. The linking/monitoring subject is the implicit
        // calling actor (`self`), matching Erlang/OTP `link(Pid)` /
        // `monitor(process, Pid)`. The 2-arg runtime ABI
        // (`hew_actor_link(parent, child)` / `hew_actor_monitor(watcher,
        // target)`) is satisfied by synthesizing `hew_actor_self()` as arg0
        // and the user target as arg1.
        if hir_args.len() != 1 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("runtime call `{symbol}` arity"),
                    site,
                },
                note: format!(
                    "`{symbol}` expects exactly 1 argument (target), got {}",
                    hir_args.len()
                ),
            });
            return None;
        }

        // Cross-node monitor: a `monitor(RemotePid<T>)` receiver routes
        // to the node-monitor ABI instead of the in-process actor-monitor ABI.
        // The remote receiver has no `HewActor*` in this address space, so the
        // 2-arg (watcher_ptr, target_ptr) shape does not apply:
        // `hew_node_monitor_location` takes the exact target Location by pointer
        // and registers a distributed-table entry keyed by that identity.
        // Detected from the target argument's
        // checker-authoritative resolved type. Only `hew_actor_monitor` reaches
        // the cross-node route; `hew_actor_link` of a remote target is deferred
        // (no cross-node link surface), so it keeps the local 2-arg shape.
        let target_ty = self.subst_ty(&hir_args[0].ty);
        if symbol == "hew_actor_monitor" && actor_name_from_remote_pid_ty(&target_ty).is_some() {
            return self.lower_node_monitor(hir_args, site, context, result_ty);
        }

        // arg1: the user-provided target handle. Lower it first so a failure
        // to lower the target is reported before we emit the self-handle call.
        let target = self.lower_value(&hir_args[0])?;

        // arg0: synthesize the implicit `self` subject via `hew_actor_self()`.
        let self_handle = self.emit_actor_self_handle();

        if context != RuntimeCallContext::ValueNeeded {
            // Statement-position (Discarded): emit with dest=None. The codegen
            // handler calls the C ABI and ignores the return.
            self.push_runtime_call(symbol, vec![self_handle, target], None);
            return None;
        }

        // Value-needed: construct the composite return.
        // `result_ty` carries the checker-authoritative return type:
        //   hew_actor_link   → Result<(), LinkError>
        //   hew_actor_monitor → MonitorRef
        // Fail closed if the checker did not record a return type — a
        // checker-boundary violation that the HIR→MIR handoff must not
        // propagate silently.
        let composite_ty = if let Some(ty) = result_ty {
            ty.clone()
        } else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("runtime call `{symbol}` value result"),
                    site,
                },
                note: format!(
                    "`{symbol}` value-needed path requires a checker-authoritative \
                     result type; result_ty was None (checker did not record a type \
                     for this call site — boundary violation)"
                ),
            });
            return None;
        };

        match symbol {
            "hew_actor_link" => {
                // hew_actor_link is void/infallible. Allocate the
                // Result<(), LinkError> dest and pass it to the ABI call.
                // The codegen ActorLink handler sees the dest and emits
                // `emit_result_ok(dest, None)` to write tag=0 (Ok, no payload).
                let result_local = self.alloc_local(composite_ty);
                self.push_runtime_call(symbol, vec![self_handle, target], Some(result_local));
                Some(result_local)
            }
            "hew_actor_monitor" => {
                // hew_actor_monitor returns a u64 ref_id. Store the raw i64
                // into a temp local, then assemble MonitorRef { ref_id } via
                // RecordInit. The codegen ActorMonitor handler stores the i64
                // call result into the raw dest; RecordInit copies it into the
                // struct field.
                let ref_id_local = self.alloc_local(ResolvedTy::I64);
                self.push_runtime_call(symbol, vec![self_handle, target], Some(ref_id_local));
                // Assemble MonitorRef { ref_id: i64 } from the raw ref_id.
                // FieldOffset(0) is the declaration-order index of `ref_id`
                // in `MonitorRef { ref_id: i64 }`.
                let monitor_ref_local = self.alloc_local(composite_ty.clone());
                self.push_instr(Instr::RecordInit {
                    ty: composite_ty,
                    fields: vec![(FieldOffset(0), ref_id_local)],
                    dest: monitor_ref_local,
                });
                Some(monitor_ref_local)
            }
            _ => unreachable!("only hew_actor_link / hew_actor_monitor reach this helper"),
        }
    }

    /// Lower a cross-node `monitor(RemotePid<T>)` to the node-monitor ABI.
    ///
    /// Unlike the local `hew_actor_monitor(watcher_ptr, target_ptr)` (which keys
    /// on `HewActor*` pointers), the remote target has no pointer in this
    /// address space. `hew_node_monitor_location(target: *const Location) -> i64`
    /// registers a distributed-table entry keyed by the exact target identity,
    /// sending a `CTRL_MONITOR_REQ` to the owning peer.
    /// Positive returns are the `ref_id` keying the registration; negative
    /// returns encode `MonitorError` as `-(variant + 1)`. Codegen assembles the
    /// checker-authoritative `Result<MonitorRef, MonitorError>` directly.
    fn lower_node_monitor(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
        result_ty: Option<&ResolvedTy>,
    ) -> Option<Place> {
        // The target is the remote pid; it lowers to the inline Location aggregate.
        let target = self.lower_value(&hir_args[0])?;

        if context != RuntimeCallContext::ValueNeeded {
            // Statement-position monitor: register but discard the ref. The
            // codegen handler calls the ABI and ignores the signed setup return.
            self.push_runtime_call("hew_node_monitor_location", vec![target], None);
            return None;
        }

        // Value-needed: the checker records
        // `Result<MonitorRef, MonitorError>` as the result type.
        // Fail closed if it did not — a HIR→MIR boundary violation.
        let composite_ty = if let Some(ty) = result_ty {
            ty.clone()
        } else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_node_monitor_location` value result".to_string(),
                    site,
                },
                note: "`hew_node_monitor_location` value-needed path requires a \
                       checker-authoritative result type; result_ty was None \
                       (checker did not record a type for this call site — \
                       boundary violation)"
                    .to_string(),
            });
            return None;
        };

        // Codegen decodes the signed setup return and writes either
        // Ok(MonitorRef { ref_id }) or Err(MonitorError::<variant>) in place.
        let result_local = self.alloc_local(composite_ty);
        self.push_runtime_call(
            "hew_node_monitor_location",
            vec![target],
            Some(result_local),
        );
        Some(result_local)
    }

    /// Lower a cross-node `link_remote(RemotePid<T>, PartitionPolicy)` to the
    /// node-link ABI.
    ///
    /// The user surface is 2-arg: the remote target pid and the `PartitionPolicy`
    /// governing what happens to the LOCAL linked actor when the remote dies. The
    /// linking subject (self) is resolved inside the runtime (like
    /// `hew_node_monitor_location` / `hew_actor_self`), so the ABI takes only
    /// `(target: *const Location, policy_tag: i64)`. The policy is a fieldless
    /// enum whose discriminant tag is extracted via `Place::EnumTag` and passed
    /// as the `policy_tag` i64.
    ///
    /// The return is `Result<(), LinkError>` (matching the local `link`). Positive
    /// ABI returns signal registration success; negative returns encode
    /// `LinkError` as `-(variant + 1)`. The EXIT arrives asynchronously when the
    /// remote dies.
    pub(crate) fn lower_node_link_remote(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
        result_ty: Option<&ResolvedTy>,
    ) -> Option<Place> {
        if hir_args.len() != 2 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_node_link_remote_location` arity".to_string(),
                    site,
                },
                note: format!(
                    "`link_remote` expects exactly 2 arguments (remote pid, policy), got {}",
                    hir_args.len()
                ),
            });
            return None;
        }

        // arg0: the remote pid (an inline Location aggregate).
        let target = self.lower_value(&hir_args[0])?;
        // arg1: the PartitionPolicy fieldless enum value; extract its discriminant
        // tag into an i64 the ABI consumes as the policy selector.
        let policy_value = self.lower_value(&hir_args[1])?;
        let Place::Local(policy_local) = policy_value else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_node_link_remote_location` policy operand"
                        .to_string(),
                    site,
                },
                note: "`link_remote` lowers the PartitionPolicy through the \
                       tagged-union discriminant; the policy operand must first \
                       materialise as an enum local"
                    .to_string(),
            });
            return None;
        };
        let policy_tag = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::Move {
            dest: policy_tag,
            src: Place::EnumTag(policy_local),
        });

        if context != RuntimeCallContext::ValueNeeded {
            // Statement-position: register the link, discard the ref_id.
            self.push_runtime_call(
                "hew_node_link_remote_location",
                vec![target, policy_tag],
                None,
            );
            return None;
        }

        // Value-needed: the checker records `Result<(), LinkError>` as the result.
        // Fail closed if it did not — a HIR→MIR boundary violation.
        let composite_ty = if let Some(ty) = result_ty {
            ty.clone()
        } else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_node_link_remote_location` value result"
                        .to_string(),
                    site,
                },
                note: "`hew_node_link_remote_location` value-needed path requires a \
                       checker-authoritative result type; result_ty was None \
                       (checker did not record a type for this call site — \
                       boundary violation)"
                    .to_string(),
            });
            return None;
        };
        // Allocate the Result<(), LinkError> dest and pass it to the ABI call.
        // Codegen decodes the signed setup return into Ok(()) or the precise Err.
        let result_local = self.alloc_local(composite_ty);
        self.push_runtime_call(
            "hew_node_link_remote_location",
            vec![target, policy_tag],
            Some(result_local),
        );
        Some(result_local)
    }

    /// Emit `Instr::CallRuntimeAbi` for discarded `unlink` calls.
    ///
    /// The user-facing surface is `unlink(target)` — 1 arg. The runtime ABI
    /// is `hew_actor_unlink(self_ptr, target_ptr)` — 2 args. The calling
    /// actor is the implicit first argument, synthesized via
    /// `hew_actor_self()`, mirroring `lower_actor_link_or_monitor`.
    ///
    /// `unlink` is a statement-position-only builtin in v0.5: it returns
    /// `Unit` and has no value-needed path, so value-needed context fails
    /// closed here rather than silently discarding the call.
    pub(crate) fn lower_actor_unlink(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
    ) -> Option<Place> {
        if hir_args.len() != 1 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_actor_unlink` arity".to_string(),
                    site,
                },
                note: format!(
                    "`hew_actor_unlink` expects exactly 1 argument (target), got {}",
                    hir_args.len()
                ),
            });
            return None;
        }

        if context == RuntimeCallContext::ValueNeeded {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_actor_unlink` value result".to_string(),
                    site,
                },
                note: "`hew_actor_unlink` returns unit; use it in statement position only"
                    .to_string(),
            });
            return None;
        }

        // arg1: the user-provided target handle. Lower it first so a failure
        // to lower the target is reported before emitting the self-handle call.
        let target = self.lower_value(&hir_args[0])?;

        // arg0: synthesize the implicit `self` subject via `hew_actor_self()`.
        let self_handle = self.emit_actor_self_handle();

        self.push_runtime_call("hew_actor_unlink", vec![self_handle, target], None);
        None
    }

    /// Pass-through handler for void-returning runtime symbols that carry no
    /// MIR-level composite-return semantics. The sole consumer today is
    /// `hew_actor_demonitor`, called directly from the body of the stdlib
    /// `impl MonitorRef { fn close(self) }` inherent method (lowered when a
    /// program imports `std::link_monitor`); the caller is responsible for
    /// lowering args correctly.
    ///
    /// Returns `None` (unit) in both statement and value-needed position — a
    /// void call appearing as the last expression in a block is valid Hew; the
    /// block yields unit. The `context` parameter is accepted but unused
    /// because the void→unit semantics are uniform across both positions.
    pub(crate) fn lower_simple_void_runtime_call(
        &mut self,
        symbol: &str,
        hir_args: &[hew_hir::HirExpr],
        _site: hew_hir::SiteId,
        _context: RuntimeCallContext,
    ) -> Option<Place> {
        let mut arg_places = Vec::with_capacity(hir_args.len());
        for arg in hir_args {
            let p = self.lower_value(arg)?;
            arg_places.push(p);
        }
        self.push_runtime_call(symbol, arg_places, None);
        None
    }

    /// Emit `Instr::CallRuntimeAbi` for a simple `(...) -> i64` runtime call.
    ///
    /// Lowers each argument to a Place and emits the call. In value position the
    /// i64 return is stored into a fresh i64 dest; in statement position the
    /// return is discarded. Used by the cross-node monitor extern surface
    /// (`hew_node_monitor_location` / `hew_node_monitor_recv`), whose returns are plain
    /// scalar reason / ref-id codes (no composite spine).
    pub(crate) fn lower_simple_int_runtime_call(
        &mut self,
        symbol: &str,
        hir_args: &[hew_hir::HirExpr],
        _site: hew_hir::SiteId,
        context: RuntimeCallContext,
        _result_ty: Option<&ResolvedTy>,
    ) -> Option<Place> {
        let mut arg_places = Vec::with_capacity(hir_args.len());
        for arg in hir_args {
            let p = self.lower_value(arg)?;
            arg_places.push(p);
        }
        if context != RuntimeCallContext::ValueNeeded {
            self.push_runtime_call(symbol, arg_places, None);
            return None;
        }
        let result_local = self.alloc_local(ResolvedTy::I64);
        self.push_runtime_call(symbol, arg_places, Some(result_local));
        Some(result_local)
    }

    /// Emit `Instr::CallRuntimeAbi` for `hew_duplex_pair`.
    ///
    /// HIR shape (from E1 bridge): `Call { callee: BindingRef("hew_duplex_pair"),
    /// args: [cap_expr] }` — one symmetric capacity arg.
    ///
    /// MIR emission:
    ///   1. Lower `cap_expr` → `cap_place`.
    ///   2. Allocate two fresh `DuplexHandle` locals (N0, N1).
    ///   3. Emit `CallRuntimeAbi { args: [cap, cap, DuplexHandle(N0), DuplexHandle(N1)], dest: None }`.
    ///   4. Allocate a "tuple proxy" `Place::Local(M)` to thread the two
    ///      output Places through the existing `BindingRef` lookup.
    ///   5. Register `tuple_decomp[M] = [DuplexHandle(N0), DuplexHandle(N1)]`.
    ///   6. Return `Some(Local(M))`.
    ///
    /// `TupleIndex` lowering recovers the individual `DuplexHandle` Places from
    /// `tuple_decomp`.  `owned_locals` registration for `a` and `b` happens
    /// naturally in `stmt()` when `let a = __tuple_N.0` stores
    /// `DuplexHandle(N0)` directly into `binding_locals` (see the handle-typed
    /// branch in the `stmt` Let arm).
    pub(crate) fn lower_duplex_pair(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        // E1 registers duplex_pair<S, R>(i64) — one symmetric capacity arg.
        // If E1 ever expands to two args (s_cap, r_cap), skip the duplication.
        let cap_place = if hir_args.len() == 1 {
            self.lower_value(&hir_args[0])
        } else if hir_args.len() >= 2 {
            // Future: two-arg form — just lower both and use the first two.
            self.lower_value(&hir_args[0])
        } else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "hew_duplex_pair with zero args".to_string(),
                    site,
                },
                note: "hew_duplex_pair requires at least one capacity argument".to_string(),
            });
            return None;
        };
        let Some(cap_place) = cap_place else {
            // Capacity expression failed to lower (e.g. nested Unsupported).
            // Diagnostic already recorded; propagate the failure.
            return None;
        };

        // If E1 emits two args, lower the second capacity independently.
        // For the one-arg case, duplicate the single capacity for both slots.
        let r_cap_place = if hir_args.len() >= 2 {
            self.lower_value(&hir_args[1]).unwrap_or(cap_place)
        } else {
            cap_place // symmetric capacity: s_cap == r_cap
        };

        // Allocate two DuplexHandle locals.  The local index is shared
        // between `Place::Local(N)` (for type bookkeeping in `self.locals`)
        // and `Place::DuplexHandle(N)` (for semantic kind tracking in the
        // instruction and drop streams).
        let local0 = self.alloc_local(ResolvedTy::Named {
            name: "Duplex".to_string(),
            args: vec![],
            builtin: Some(hew_types::BuiltinType::Duplex),
            is_opaque: false,
        });
        let Place::Local(n0) = local0 else {
            unreachable!("alloc_local returns Place::Local");
        };
        let dh0 = Place::DuplexHandle(n0);

        let local1 = self.alloc_local(ResolvedTy::Named {
            name: "Duplex".to_string(),
            args: vec![],
            builtin: Some(hew_types::BuiltinType::Duplex),
            is_opaque: false,
        });
        let Place::Local(n1) = local1 else {
            unreachable!("alloc_local returns Place::Local");
        };
        let dh1 = Place::DuplexHandle(n1);

        // Emit the runtime call.  The i32 return (error code) is discarded
        // (`dest: None`); the two DuplexHandle out-params are in args[2..=3].
        // Codegen (E4) interprets DuplexHandle places in args[2..=3] as
        // "pass the address of this local's alloca as *mut *mut DuplexHandle".
        self.push_instr(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new(
                "hew_duplex_pair",
                vec![cap_place, r_cap_place, dh0, dh1],
                None,
            )
            .expect("hew_duplex_pair is an allowlisted runtime symbol"),
        ));

        // Create a "tuple proxy" local so TupleIndex lowering can recover dh0/dh1.
        // The proxy carries no runtime value; its index is the key into tuple_decomp.
        // Using `ResolvedTy::Unit` for the proxy type so no spurious UnknownType
        // diagnostic fires for it.
        let proxy = self.alloc_local(ResolvedTy::Unit);
        let Place::Local(proxy_idx) = proxy else {
            unreachable!("alloc_local returns Place::Local");
        };
        self.tuple_decomp.insert(proxy_idx, vec![dh0, dh1]);

        Some(proxy)
    }

    /// Emit `Instr::CallRuntimeAbi` for `hew_supervisor_stop`.
    ///
    /// HIR shape: `Call { callee: BindingRef("supervisor_stop"), args: [sup_expr] }`.
    /// The checker registers `supervisor_stop(sup)` returning `Ty::Unit`, so
    /// this producer returns `None` — Unit is zero-sized and callers handle
    /// `None` as "no destination place" (see `CallClosure` and `CallTraitMethod`
    /// patterns in `lower_value`).
    ///
    /// MIR emission:
    ///   1. Lower `sup_expr` → `sup_place` (a `LocalPid<S>` — opaque ptr).
    ///   2. Emit `CallRuntimeAbi { "hew_supervisor_stop", args: [sup_place], dest: None }`.
    ///   3. Return `None` (Unit result).
    pub(crate) fn lower_supervisor_stop(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        if hir_args.len() != 1 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "supervisor_stop".to_string(),
                    site,
                },
                note: format!(
                    "supervisor_stop expects 1 argument (sup), got {}",
                    hir_args.len()
                ),
            });
            return None;
        }
        let sup_place = self.lower_value(&hir_args[0])?;
        self.push_instr(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new("hew_supervisor_stop", vec![sup_place], None)
                .expect("hew_supervisor_stop is an allowlisted runtime symbol"),
        ));
        // Unit return — no destination place.
        None
    }

    /// Emit the MIR sequence for a static supervisor child-slot access (F-04
    /// fungible reference).
    ///
    /// Called from the `HirExprKind::FieldAccess` intercept arm after the
    /// checker has confirmed the LHS is a supervisor with a static child at
    /// `slot_index`. The result is a FUNGIBLE child reference: it names the
    /// `(supervisor, slot)` role, not a specific actor instance, and the current
    /// occupant is RE-RESOLVED at each send/ask (see `emit_fungible_reresolve`).
    /// The accessor itself no longer traps on a not-live slot — liveness is the
    /// send's concern, surfaced there as a recoverable error.
    ///
    /// Produces:
    ///
    /// ```text
    /// entry_bb (current)
    ///   [lower object → sup_place]
    ///   ConstI64 { dest: idx_place, value: slot_index }
    ///   CallRuntimeAbi { "hew_supervisor_child_get",
    ///                    args: [sup_place, idx_place],
    ///                    dest: result_place }
    ///   RecordFieldLoad { record: result_place, field_offset: 1, dest: raw_handle }  -- i64 handle
    ///   Move { dest: handle_place (ActorHandle(N)), src: raw_handle }
    ///   [cursor stays here for subsequent lowering]
    /// ```
    ///
    /// The initial child-get seeds the alloca so an immediate same-expression use
    /// (`sup.w.tick()`) and the eventual binding (`let a = sup.w`) both have a
    /// well-formed handle. The crucial change vs. the snapshot model is that the
    /// handle local is recorded in `fungible_child_refs`, so a later send through
    /// it re-resolves rather than reusing this seed pointer. On a not-live slot at
    /// the accessor the handle is seeded with the (null) wire value; the send
    /// re-resolve is what decides liveness, so the seed value is never trusted as
    /// the send target.
    ///
    /// Returns `Some(handle_place)`. `handle_place` is `Place::ActorHandle(N)`
    /// where N is the backing local index of a freshly allocated
    /// `LocalPid<ChildActor>` local (typed as `result_ty` from the checker — the
    /// checker is the authority on the child actor type).
    ///
    /// S3 codegen interprets `CallRuntimeAbi` with a `__HewChildLookupResult`-typed
    /// dest as a struct-return call, emitting `{ i64, ptr }` in LLVM IR and storing
    /// the struct into the alloca slot. `RecordFieldLoad` at index 1 extracts the
    /// handle pointer (reinterpreted as i64 at the MIR layer; S3 emits `ptrtoint`
    /// when writing to the handle alloca).
    pub(crate) fn lower_supervisor_child_get(
        &mut self,
        object: &HirExpr,
        slot_index: u32,
        result_ty: &ResolvedTy,
        _site: hew_hir::SiteId,
    ) -> Option<Place> {
        // Lower the supervisor object expression to get the supervisor PID place.
        let sup_place = self.lower_value(object)?;

        // Allocate the final ActorHandle place typed as `result_ty`
        // (the checker-authority `LocalPid<ChildActor>` type for this site).
        let handle_local = self.alloc_local(result_ty.clone());
        let Place::Local(handle_id) = handle_local else {
            unreachable!("alloc_local always returns Place::Local");
        };
        let handle_place = Place::ActorHandle(handle_id);

        // Record the fungible reference BEFORE seeding so a re-resolve at any
        // send through `handle_place` knows the `(sup, slot)` to re-fetch.
        self.fungible_child_refs.insert(
            handle_id,
            FungibleChildRef {
                sup_place,
                slot_index,
            },
        );

        // Seed the alloca with one resolve (no trap, no liveness branch — the
        // send re-resolves and is the sole liveness authority).
        self.emit_child_get_into(sup_place, slot_index, handle_place);

        // The `instr_places` function in lower.rs surfaces `handle_place` to the
        // dataflow seed pass, maintaining the same bookkeeping invariant as
        // `lower_spawn_actor`.

        Some(handle_place)
    }

    /// Lower `await_restart sup.child` to `SuspendKind::RestartWait`.
    ///
    /// The supervisor analogue of `lower_await_task`: park the current actor on
    /// the supervisor restart observer until the static child slot is Live again,
    /// then resume re-fetching the now-Live `LocalPid<ChildType>`. Modelled on
    /// the `TaskAwait` suspend ramp; the resume edge re-resolves the slot through
    /// the SAME fungible-reference machinery a static child send uses
    /// (`emit_child_get_into` + `fungible_child_refs`), so the resumed handle is
    /// never a pointer cached across the suspend (LESSONS
    /// `replaceable-resource-handle-is-fungible-reference`, R6).
    ///
    /// `child` is the inner supervised-child accessor (a `FieldAccess`); its
    /// `site` keys `supervisor_child_slots` with the `(supervisor, slot)`
    /// discriminator. The checker already proved the operand is a static child,
    /// so a missing slot here is a lowering invariant break — fail closed.
    ///
    /// A `FunctionCallConv::Default` caller (`main`, free fn) has no parkable
    /// continuation, so it lowers the accessor seed only (the runtime observer's
    /// READY/immediate-Dead path resumes it through the same edge once codegen's
    /// blocking fallback lands); the suspend flip fires only for a caller that
    /// carries an execution context, exactly like `lower_await_task`.
    pub(crate) fn lower_await_restart(
        &mut self,
        child: &HirExpr,
        result_ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        // The inner accessor's site carries the (supervisor, slot) discriminator.
        let Some(slot) = self.supervisor_child_slots.get(&child.site).cloned() else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "await_restart on a non-supervised-child operand".to_string(),
                    site,
                },
                note: "await_restart lowering found no supervisor child slot for its operand; \
                       the checker should have rejected this"
                    .to_string(),
            });
            return None;
        };

        // The accessor's object is the supervisor PID. Extract + lower it (NOT the
        // whole FieldAccess, which would emit a child_get of its own).
        let HirExprKind::FieldAccess { object, .. } = &child.kind else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "await_restart operand is not a field access".to_string(),
                    site,
                },
                note:
                    "await_restart expects `sup.child`; its operand lowered to a non-FieldAccess \
                       HIR node"
                        .to_string(),
            });
            return None;
        };
        let sup_place = self.lower_value(object)?;

        // Translate the checker's combined-static index to the kind-partitioned
        // runtime slot index (the accessor reads the same partitioned space).
        let slot_index =
            self.partitioned_static_slot_index(&slot.supervisor, &slot.child_name, false);

        // Allocate the re-fetched handle local, typed as the child's
        // `LocalPid<ChildType>` (the checker authority on this expr's type).
        let handle_local = self.alloc_local(result_ty.clone());
        let Place::Local(handle_id) = handle_local else {
            unreachable!("alloc_local always returns Place::Local");
        };
        let handle_place = Place::ActorHandle(handle_id);

        // Record the fungible reference BEFORE the suspend so a send through the
        // resumed handle re-resolves the `(sup, slot)` at send time rather than
        // trusting any pointer captured across the suspension point.
        self.fungible_child_refs.insert(
            handle_id,
            FungibleChildRef {
                sup_place,
                slot_index,
            },
        );

        // Suspendable-caller flip: a caller that carries the execution context
        // (actor handler / closure / task entry) SUSPENDS on the restart observer
        // instead of spinning. The resume edge re-fetches the now-Live handle.
        if self.current_function_call_conv.carries_execution_context() {
            let next = self.alloc_block();
            self.record_suspend_kind(SuspendKind::RestartWait {
                sup_place,
                slot_index,
                result_dest: handle_place,
                deadline_result_dest: None,
            });
            self.finish_current_block(Terminator::Suspend {
                resume: next,
                cleanup: next,
                is_final: false,
            });
            self.start_block(next);
            // On resume the child slot is Live (notify_restart fires AFTER
            // store_child_slot); re-fetch it through the fungible re-resolve so
            // the handle reflects the new incarnation, never a stale snapshot.
            self.emit_child_get_into(sup_place, slot_index, handle_place);
            return Some(handle_place);
        }

        // Contextless caller (`main` / free fn): no parkable continuation, so
        // BLOCK the calling thread on the supervisor restart Condvar until the
        // child is Live or permanently Dead, then re-fetch. Safe to thread-block
        // here: `main` runs off the cooperative scheduler that fires the restart
        // (no self-deadlock), exactly as a contextless `await` blocks on its ask.
        // The R4 fail-closed contract holds: a permanent-Dead slot returns from
        // the blocking wait and the re-fetch surfaces the dead slot recoverably.
        let idx_place = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::ConstI64 {
            dest: idx_place,
            value: i64::from(slot_index),
        });
        self.push_instr(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new(
                "hew_supervisor_restart_await_blocking",
                vec![sup_place, idx_place],
                None,
            )
            .expect("hew_supervisor_restart_await_blocking is an allowlisted runtime symbol"),
        ));
        self.emit_child_get_into(sup_place, slot_index, handle_place);
        Some(handle_place)
    }

    /// Translate a supervisor child's combined-static checker index into its
    /// kind-partitioned runtime slot index.
    ///
    /// The runtime keeps actor children and nested supervisors in two separate
    /// 0-based tables. Codegen registers each kind into its own table while
    /// iterating `SupervisorLayout.children` (topological spawn order), so a
    /// child's runtime index is its position among same-kind children in that
    /// same iteration order. Count the same-kind children that precede this one
    /// in the layout to recover that index.
    ///
    /// Falls back to a 0 index if the supervisor or child is not found in the
    /// layout map (an upstream diagnostic already covers the unknown-supervisor
    /// case); the accessor's runtime null-guard then fail-closes the lookup.
    pub(crate) fn partitioned_static_slot_index(
        &self,
        supervisor: &str,
        child_name: &str,
        want_nested: bool,
    ) -> u32 {
        let Some(layout) = self.supervisor_layout_map.get(supervisor) else {
            return 0;
        };
        let mut index = 0u32;
        for child in &layout.children {
            // Pools live in a disjoint `pool_slots[]` space and occupy NEITHER
            // the static actor-child table nor the nested-supervisor table — skip
            // them on BOTH axes. This is the shared truth `occupies_static_child_slot`
            // encodes for the actor axis; the codegen bootstrap loop must skip
            // pools identically or a post-pool static accessor mis-routes.
            if child.is_pool {
                continue;
            }
            let child_is_nested = child.nested_bootstrap_symbol.is_some();
            if child_is_nested != want_nested {
                continue;
            }
            if child.name == child_name {
                // Invariant: a non-pool actor child (want_nested == false) is
                // exactly what `occupies_static_child_slot` admits; keep the two
                // iterations provably in lock-step.
                debug_assert!(
                    want_nested || child.occupies_static_child_slot(),
                    "partitioned_static_slot_index: actor-axis child `{child_name}` of \
                     supervisor `{supervisor}` must occupy a static child slot — the \
                     codegen bootstrap loop and this accessor lookup have diverged"
                );
                return index;
            }
            index += 1;
        }
        // Child not found among same-kind siblings — return the running count as
        // a best-effort index; an upstream diagnostic covers the genuine
        // unknown-child case.
        index
    }

    /// Lower a nested-supervisor child accessor (`app.api` where `api` is itself
    /// a supervisor) to a `hew_supervisor_nested_get(sup, slot)` call.
    ///
    /// Distinct from `lower_supervisor_child_get`: the parent resolves a nested
    /// child through its `child_supervisors` table (not its actor `children`
    /// table), so the runtime symbol is `hew_supervisor_nested_get`. The runtime
    /// returns the child supervisor pointer in field 1 of the same
    /// `__HewChildLookupResult` struct; the result type is
    /// `LocalPid<NestedSupervisor>`, so a subsequent dotted segment
    /// (`app.api.auth`) routes back through this intercept against the nested
    /// supervisor pointer.
    ///
    /// The handle is a plain `*mut HewSupervisor` reinterpreted as a PID — it is
    /// NOT registered as a fungible child ref. A fungible ref drives per-send
    /// re-resolution of an *actor* handle; a supervisor handle is only ever used
    /// as the receiver of a further child-get, which itself re-resolves the leaf
    /// actor at the moment of the send. Resolving the nested supervisor once per
    /// accessor expression is sufficient: the child supervisor's identity is
    /// stable across its own children's restarts, and a nested-supervisor
    /// escalation restart is re-resolved on the next `app.api` access.
    pub(crate) fn lower_supervisor_nested_get(
        &mut self,
        object: &HirExpr,
        slot_index: u32,
        result_ty: &ResolvedTy,
    ) -> Option<Place> {
        let sup_place = self.lower_value(object)?;

        // Allocate the handle typed as the checker-authority
        // `LocalPid<NestedSupervisor>` result type for this site.
        let handle_local = self.alloc_local(result_ty.clone());
        let Place::Local(handle_id) = handle_local else {
            unreachable!("alloc_local always returns Place::Local");
        };
        let handle_place = Place::ActorHandle(handle_id);

        // Emit a constant for the static nested slot index.
        let idx_place = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::ConstI64 {
            dest: idx_place,
            value: i64::from(slot_index),
        });

        // Allocate a local typed as the opaque `__HewChildLookupResult` record.
        // Codegen recognises this type name and emits a struct-return LLVM call.
        let result_place = self.alloc_local(ResolvedTy::Named {
            name: CHILD_LOOKUP_RESULT_TY_NAME.to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        });

        // Emit the nested-get runtime call. The dest carries the 16-byte struct.
        self.push_instr(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new(
                "hew_supervisor_nested_get",
                vec![sup_place, idx_place],
                Some(result_place),
            )
            .expect("hew_supervisor_nested_get is an allowlisted runtime symbol"),
        ));

        // Extract the child supervisor pointer (field 1, i64 at MIR level).
        let raw_handle = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::RecordFieldLoad {
            record: result_place,
            field_offset: FieldOffset(1),
            dest: raw_handle,
        });

        // Move the i64 wire value into the typed supervisor handle slot.
        self.push_instr(Instr::Move {
            dest: handle_place,
            src: raw_handle,
        });

        Some(handle_place)
    }

    /// Lower a static-pool accessor through a first-class
    /// `SupervisorPool<S, T>` receiver.
    ///
    /// - `Index` → `hew_supervisor_pool_child_get(sup, key, i)`; tag != 0 (not
    ///   Live) traps `SupervisorChildUnavailable` (`Vec[i]` OOB parity).
    /// - `Len`   → `hew_supervisor_pool_len(sup, key)` → `i64`.
    /// - `Get`   → same `child_get`; tag 0 → `Some(handle)`, tag != 0 → `None`
    ///   (a drop-safe `Option<LocalPid<T>>`, never a sentinel dressed as live).
    pub(crate) fn lower_pool_accessor(
        &mut self,
        expr: &HirExpr,
        accessor: &hew_types::PoolAccessor,
    ) -> Option<Place> {
        use hew_types::PoolAccessorKind;

        let (pool_expr, index_expr): (&HirExpr, Option<&HirExpr>) = match &expr.kind {
            HirExprKind::Index { container, index } => (container.as_ref(), Some(index.as_ref())),
            HirExprKind::Call { args, .. } => {
                let Some(receiver) = args.first() else {
                    self.pool_accessor_shape_error(expr.site, "pool method call has no receiver");
                    return None;
                };
                (receiver, args.get(1))
            }
            _ => {
                self.pool_accessor_shape_error(expr.site, "accessor is neither index nor call");
                return None;
            }
        };

        let pool_ty = self.subst_ty(&pool_expr.ty);
        let supervisor_ty = match &pool_ty {
            ResolvedTy::Named {
                builtin: Some(hew_types::BuiltinType::SupervisorPool),
                args,
                ..
            } if args.len() == 2 => args[0].clone(),
            _ => {
                self.pool_accessor_shape_error(expr.site, "receiver is not `SupervisorPool<S, T>`");
                return None;
            }
        };
        let pool_place = self.lower_value(pool_expr)?;
        let sup_place = self.alloc_local(ResolvedTy::Named {
            name: "LocalPid".to_string(),
            args: vec![supervisor_ty],
            builtin: Some(hew_types::BuiltinType::LocalPid),
            is_opaque: false,
        });
        self.push_instr(Instr::RecordFieldLoad {
            record: pool_place,
            field_offset: FieldOffset(0),
            dest: sup_place,
        });
        let key_place = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::RecordFieldLoad {
            record: pool_place,
            field_offset: FieldOffset(1),
            dest: key_place,
        });

        match accessor.kind {
            PoolAccessorKind::Len => {
                let dest = self.alloc_local(ResolvedTy::I64);
                self.push_instr(Instr::CallRuntimeAbi(
                    crate::model::RuntimeCall::new(
                        "hew_supervisor_pool_len",
                        vec![sup_place, key_place],
                        Some(dest),
                    )
                    .expect("hew_supervisor_pool_len is an allowlisted runtime symbol"),
                ));
                Some(dest)
            }
            PoolAccessorKind::Index => {
                let index_expr = index_expr?;
                let idx_place = self.lower_value(index_expr)?;
                Some(self.lower_pool_index(
                    sup_place,
                    key_place,
                    idx_place,
                    &self.subst_ty(&expr.ty),
                ))
            }
            PoolAccessorKind::Get => {
                let Some(index_expr) = index_expr else {
                    self.pool_accessor_shape_error(expr.site, "pool get call has no index");
                    return None;
                };
                let idx_place = self.lower_value(index_expr)?;
                let lookup = self.emit_pool_child_get(sup_place, key_place, idx_place);
                let result = self.alloc_local(self.subst_ty(&expr.ty));
                let next = self.alloc_block();
                self.finish_current_block(Terminator::Call {
                    callee: "hew_supervisor_pool_get_option".to_string(),
                    builtin: None,
                    args: vec![lookup],
                    dest: Some(result),
                    next,
                });
                self.start_block(next);
                Some(result)
            }
        }
    }

    /// Emit `hew_supervisor_pool_child_get(sup, pool_key, index)` and return the
    /// `__HewChildLookupResult` result place (tag in field 0, handle in field 1).
    fn emit_pool_child_get(
        &mut self,
        sup_place: Place,
        key_place: Place,
        idx_place: Place,
    ) -> Place {
        let result_place = self.alloc_local(ResolvedTy::Named {
            name: CHILD_LOOKUP_RESULT_TY_NAME.to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        });
        self.push_instr(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new(
                "hew_supervisor_pool_child_get",
                vec![sup_place, key_place, idx_place],
                Some(result_place),
            )
            .expect("hew_supervisor_pool_child_get is an allowlisted runtime symbol"),
        ));
        result_place
    }

    /// Lower `sup.pool[i]` (the trapping member accessor): call
    /// `pool_child_get`, then trap `SupervisorChildUnavailable` on a not-Live
    /// tag (OOB / mid-restart) and bind the Live handle into a `LocalPid<T>`
    /// (`Vec[i]` OOB parity — the index access is the sole liveness authority).
    fn lower_pool_index(
        &mut self,
        sup_place: Place,
        key_place: Place,
        idx_place: Place,
        result_ty: &ResolvedTy,
    ) -> Place {
        let result_place = self.emit_pool_child_get(sup_place, key_place, idx_place);
        let tag = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::RecordFieldLoad {
            record: result_place,
            field_offset: FieldOffset(0),
            dest: tag,
        });
        let zero = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::ConstI64 {
            dest: zero,
            value: 0,
        });
        let is_live = self.alloc_local(ResolvedTy::Bool);
        self.push_instr(Instr::IntCmp {
            dest: is_live,
            pred: CmpPred::Eq,
            lhs: tag,
            rhs: zero,
        });
        let live_bb = self.alloc_block();
        let trap_bb = self.alloc_block();
        self.finish_current_block(Terminator::Branch {
            cond: is_live,
            then_target: live_bb,
            else_target: trap_bb,
        });
        // OOB / not-live → trap (fail-closed, mirrors the static accessor).
        self.start_block(trap_bb);
        self.finish_current_block(Terminator::Trap {
            kind: crate::model::TrapKind::SupervisorChildUnavailable,
        });
        // Live → extract the handle into a typed LocalPid local.
        self.start_block(live_bb);
        let handle_local = self.alloc_local(result_ty.clone());
        let Place::Local(handle_id) = handle_local else {
            unreachable!("alloc_local always returns Place::Local");
        };
        let handle_place = Place::ActorHandle(handle_id);
        let raw_handle = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::RecordFieldLoad {
            record: result_place,
            field_offset: FieldOffset(1),
            dest: raw_handle,
        });
        self.push_instr(Instr::Move {
            dest: handle_place,
            src: raw_handle,
        });
        handle_place
    }

    /// Record a fail-closed shape error for a malformed pool accessor (the
    /// checker should have rejected these; this is the MIR backstop).
    fn pool_accessor_shape_error(&mut self, site: hew_hir::SiteId, why: &str) {
        self.diagnostics.push(MirDiagnostic {
            kind: MirDiagnosticKind::NotYetImplemented {
                construct: format!("malformed static-pool accessor: {why}"),
                site,
            },
            note: "a static-pool accessor must be `sup.pool[i]`, `sup.pool.get(i)`, \
                   or `sup.pool.len()`"
                .to_string(),
        });
    }

    /// Emit a `hew_supervisor_child_get(sup, slot)` call and store the resolved
    /// `*mut HewActor` (field 1) into `handle_place`, with NO liveness branch.
    ///
    /// On a not-live slot the runtime returns a null handle (field 1 == null);
    /// the caller (the accessor seed, or `emit_fungible_reresolve`) is responsible
    /// for whatever liveness handling is required. This is the shared "resolve the
    /// current child pointer into the handle slot" primitive.
    pub(crate) fn emit_child_get_into(
        &mut self,
        sup_place: Place,
        slot_index: u32,
        handle_place: Place,
    ) {
        // Emit a constant for the static slot index.
        let idx_place = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::ConstI64 {
            dest: idx_place,
            value: i64::from(slot_index),
        });

        // Allocate a local typed as the opaque `__HewChildLookupResult` record.
        // S3 codegen recognises this type name and emits a struct-return LLVM call.
        let result_place = self.alloc_local(ResolvedTy::Named {
            name: CHILD_LOOKUP_RESULT_TY_NAME.to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        });

        // Emit the runtime call. The dest carries the 16-byte struct return value.
        self.push_instr(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new(
                "hew_supervisor_child_get",
                vec![sup_place, idx_place],
                Some(result_place),
            )
            .expect("hew_supervisor_child_get is an allowlisted runtime symbol"),
        ));

        // Extract the handle pointer (field 1, i64 at MIR level).
        // S3 emits a `ptrtoint`/`inttoptr` as needed for the wire representation.
        let raw_handle = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::RecordFieldLoad {
            record: result_place,
            field_offset: FieldOffset(1),
            dest: raw_handle,
        });

        // Move the i64 wire value into the typed ActorHandle slot.
        // S3 emits the appropriate cast; at MIR level they are the same storage.
        self.push_instr(Instr::Move {
            dest: handle_place,
            src: raw_handle,
        });
    }

    /// Emit `Instr::CallRuntimeAbi` for a `.send` on an actor/duplex handle.
    ///
    /// HIR shape (from E1 bridge): `Call { callee: BindingRef("hew_duplex_send"),
    /// args: [receiver_expr, msg_expr] }` — receiver prepended by E1. The
    /// checker records the `hew_duplex_send` rewrite for `.send` on every
    /// `Duplex<S, R>` receiver, including lambda-actor handles (which type as
    /// `Duplex<Msg, Reply>`).
    ///
    /// MIR emission:
    ///   1. Lower `receiver_expr` → `recv_place`. A lambda-actor binding
    ///      lowers to `Place::LambdaActorHandle(N)`; a raw `Duplex::pair()`
    ///      handle lowers to `Place::DuplexHandle(N)`.
    ///   2. Lower `msg_expr` → `msg_place` (the integer value's `Local(K)`).
    ///   3. Emit `ConstI64 { dest: len_place, value: 8 }` — the byte-length.
    ///   4. Select the runtime symbol by the receiver `Place` variant
    ///      (`codegen-abi-authority`): `LambdaActorHandle` → the
    ///      lambda-actor ABI `hew_lambda_actor_send`; anything else → the
    ///      raw-duplex ABI `hew_duplex_send`. Routing each `Place` to exactly
    ///      one correct symbol is load-bearing: passing a `LambdaActorHandle`
    ///      to `hew_duplex_send` type-puns the handle and silently mis-delivers
    ///      (`no-fail-open-fallback-after-authority`).
    ///   5. Emit `CallRuntimeAbi { symbol, args: [recv, msg, len], dest }`.
    ///
    /// The receiver is NOT consumed (non-move send semantics); `owned_locals`
    /// for the receiver handle must persist across multiple sends and the
    /// scope-exit drop (LESSONS `raii-null-after-move`).
    pub(crate) fn lower_duplex_send(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
        result_ty: Option<&ResolvedTy>,
    ) -> Option<Place> {
        if hir_args.len() < 2 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "hew_duplex_send with fewer than 2 args".to_string(),
                    site,
                },
                note: "hew_duplex_send requires receiver + message arguments".to_string(),
            });
            return None;
        }
        // args[0] = receiver (DuplexHandle — non-consuming borrow; no Move emitted).
        let recv_place = self.lower_value(&hir_args[0]);
        // args[1] = message value.
        let msg_place = self.lower_value(&hir_args[1]);

        let (Some(recv_place), Some(msg_place)) = (recv_place, msg_place) else {
            // Argument lowering failed; diagnostic already recorded.
            return None;
        };

        // Route by receiver Place variant. A lambda-actor handle must use the
        // lambda-actor ABI; a raw `Duplex` handle uses the duplex ABI. The two
        // runtime symbols are distinct authorities for one `.send` surface, and
        // a `LambdaActorHandle` routed through `hew_duplex_send` type-puns the
        // handle and silently drops the message (Evidence #2). The `Place`
        // variant is the canonical "which handle is this" signal — selecting on
        // it (not on the receiver's type, which is `Duplex<Msg, Reply>` for
        // both) keeps ABI selection on an explicit authority
        // (`codegen-abi-authority`, `no-fail-open-fallback-after-authority`).
        let symbol = match recv_place {
            Place::LambdaActorHandle(_) => "hew_lambda_actor_send",
            _ => "hew_duplex_send",
        };

        // Materialize the runtime's i32 send status into a user-visible
        // `Result<(), SendError>`, or fail closed. The decision is on the result
        // SHAPE, not the call context, so an ask-shaped `.send` fails closed in
        // BOTH statement and value position — it must never lower as a
        // fire-and-forget tell that silently drops the reply the user's type
        // says they receive.
        //
        // Shape is the checker-recorded result type (`checker-authority`: the
        // producer consumes the recorded type, it does not re-infer it):
        //   - tell-shaped `Result<(), SendError>` -> supported. Value context
        //     binds a dest local that codegen fills from the rc (the D1
        //     discriminant mapping lives in codegen, the single authority);
        //     statement context discards the status (`dest: None`,
        //     fire-and-forget delivery).
        //   - ask-shaped `Result<R, AskError>` (a non-unit `Duplex` reply) or
        //     any other type -> a separate lowering this change does not own. Fail
        //     closed with a stable NYI diagnostic BEFORE emitting any send
        //     instruction (`no-fail-open-fallback-after-authority`), rather than
        //     bind nothing (the misleading `UnresolvedPlace`, Evidence #1),
        //     mis-size the slot, or — in statement context — drop the reply
        //     silently. The check runs before the length const below so the
        //     rejected path emits neither the const nor the call.
        let resolved_result = result_ty.map(|ty| self.subst_ty(ty));
        if !resolved_result
            .as_ref()
            .is_some_and(is_unit_send_error_result)
        {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "`.send` whose result is not Result<(), SendError>".to_string(),
                    site,
                },
                note: format!(
                    "`.send` materializes only the tell-shaped `Result<(), \
                     SendError>`; got {resolved_result:?}. Ask-shaped `.send` (a \
                     non-unit `Duplex` reply yielding `Result<R, AskError>`) is a \
                     separate lowering and fails closed in both statement and \
                     value context rather than lower as a fire-and-forget tell \
                     that drops the reply."
                ),
            });
            return None;
        }
        // Tell-shaped: bind the Result in value context; discard the status in
        // statement context (fire-and-forget delivery).
        let dest = match context {
            RuntimeCallContext::ValueNeeded => Some(self.alloc_local(
                resolved_result.expect("tell-shaped result is Some after the guard above"),
            )),
            RuntimeCallContext::Discarded => None,
        };

        // Emit the byte-length constant.  The runtime ABI takes `*const u8 + usize`;
        // E4 codegen stores `msg_place`'s value to a stack alloca and passes its
        // address.  The length constant here encodes the fixed 8-byte integer size.
        //
        // SHIM(E4): the ConstI64(8) encodes the integer payload byte-length.
        // WHY: MIR has no "sizeof" expression; the integer spine always uses 8-byte
        //   i64 values, so the length is a compile-time constant for this skeleton.
        // WHEN obsolete: when the type system can express the payload size directly,
        //   or when hew_duplex_send uses a typed message rather than a byte slice.
        // WHAT: replace with a proper sizeof/alignof expression or a typed ABI.
        let len_place = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::ConstI64 {
            dest: len_place,
            value: 8,
        });

        // Emit the runtime call.  `recv_place` is used as a borrow (not consumed);
        // the receiver's `owned_locals` entry survives for subsequent sends and the
        // scope-exit drop (LESSONS `raii-null-after-move`, `cleanup-all-exits`).
        // `dest` is `Some` only in value context; codegen materializes the Result
        // there and leaves the rc unobserved when it is `None`.
        self.push_instr(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new(symbol, vec![recv_place, msg_place, len_place], dest)
                .expect("send symbol is an allowlisted runtime symbol"),
        ));

        dest
    }

    /// Lower an explicit `.close()` call rewritten to `hew_duplex_close`.
    ///
    /// The checker records `"hew_duplex_close"` for every `.close()` call on a
    /// `Duplex<S,R>`-typed receiver, including `LambdaPid<M,R>` handles (which
    /// surface as `Duplex<M,R>`).  The `Place` variant on the receiver is the
    /// authority for which runtime symbol to invoke:
    ///
    ///   - `Place::LambdaActorHandle(N)` → `hew_lambda_actor_release`.
    ///     The release ABI takes only the handle pointer; codegen rejects
    ///     `dest: Some(...)` for this symbol, so the call is always `dest: None`.
    ///     The checker records `Unit` as the return type (the release never
    ///     fails), so in value context a unit literal is produced.
    ///
    ///   - Any other `Place` → raw `Duplex` explicit close; not yet lowered.
    ///     Fails closed so the pipeline rejects the program before codegen runs.
    ///
    /// The checker marks the receiver as consumed (`method_call_consumes_receiver`
    /// + `mark_expr_moved_if_non_copy`), so drop elaboration at scope exit will
    ///   not re-drop the released handle.
    pub(crate) fn lower_duplex_close(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
        _result_ty: Option<&ResolvedTy>,
    ) -> Option<Place> {
        let Some(receiver_expr) = hir_args.first() else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "hew_duplex_close with no receiver arg".to_string(),
                    site,
                },
                note: "hew_duplex_close requires at least one argument (the handle)".to_string(),
            });
            return None;
        };
        let recv_place = self.lower_value(receiver_expr)?;

        if let Place::LambdaActorHandle(_) = recv_place {
            // Explicit LambdaPid::close() → hew_lambda_actor_release.
            //
            // The release ABI is: hew_lambda_actor_release(handle: *mut HewLambdaActorHandle)
            // Codegen rejects dest: Some(_) for this symbol (the i32 rc is always
            // discarded), so we never pass a dest — the handle is released and the
            // result, if needed in value context, is a unit literal (the checker
            // records `()` for LambdaPid::close() since the release is
            // unconditionally successful).
            self.push_instr(Instr::CallRuntimeAbi(
                crate::model::RuntimeCall::new("hew_lambda_actor_release", vec![recv_place], None)
                    .expect("hew_lambda_actor_release is an allowlisted runtime symbol"),
            ));

            // The checker records `Unit` as the return type for LambdaPid::close()
            // (the release never fails). In value context, bind a unit literal so
            // any `let _ = handle.close()` binding has a well-typed Place.
            if context == RuntimeCallContext::ValueNeeded {
                let unit_place = self.alloc_local(ResolvedTy::Unit);
                self.push_instr(Instr::UnitLit { dest: unit_place });
                Some(unit_place)
            } else {
                None
            }
        } else {
            // Explicit `handle.close()` on a raw `Duplex<S,R>` binding routes
            // through drop elaboration, not a value-position call: codegen's
            // close arm is the drop ritual's responsibility (it pairs the
            // `hew_duplex_close` call with the alloca-zero so a later drop
            // cannot re-close). The unified handle's scope-exit drop already
            // emits exactly that ritual, so an explicit raw close is redundant
            // with the RAII close and fails closed here rather than emit a
            // value-position call codegen refuses. The half-handle close
            // (`hew_duplex_close_half`) has a dedicated value-position arm
            // because its direction discriminant is not a drop-ritual concern.
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("explicit Duplex::close() on a raw handle ({recv_place:?})"),
                    site,
                },
                note: "explicit `Duplex::close()` on a raw `Duplex<S,R>` binding is closed \
                       implicitly at scope exit by drop elaboration; remove the explicit \
                       `.close()` call. Split the handle and close a half \
                       (`SendHalf`/`RecvHalf`) for an explicit per-direction close."
                    .to_string(),
            });
            None
        }
    }

    /// Lower `.send_half()` / `.recv_half()` on a unified `Duplex<S, R>`.
    ///
    /// HIR shape (E1 bridge): `Call { callee: BindingRef(symbol), args:
    /// [receiver_expr] }`. The checker marks the call consuming, so the
    /// `receiver_expr` carries `IntentKind::Consume`; `lower_value` emits the
    /// `Use { Consume }` that transitions the unified `DuplexHandle` binding to
    /// `Consumed` and drops it out of `owned_locals`, so its scope-exit close is
    /// suppressed and the extracted half is the only live handle on that
    /// direction (`raii-null-after-move`, `cleanup-all-exits`).
    ///
    /// MIR emission:
    ///   1. Lower `receiver_expr` → the unified `DuplexHandle` Place (consuming).
    ///   2. Allocate a fresh backing local typed `SendHalf<S>` / `RecvHalf<R>`
    ///      (the checker-recorded result type) and wrap it as the matching
    ///      half `Place`. The `Place` variant is the authority codegen reads to
    ///      pick the per-direction close ABI at drop, so it must match the
    ///      extracted direction (`codegen-abi-authority`).
    ///   3. Emit `CallRuntimeAbi { symbol, args: [duplex], dest: Some(half) }` —
    ///      the half ABIs RETURN the new handle pointer (unlike `hew_duplex_pair`,
    ///      which writes out-params), so the half Place is the call dest.
    ///   4. Return the half Place; the enclosing `Let` registers it in
    ///      `binding_locals` + `owned_locals` for its own scope-exit close.
    pub(crate) fn lower_duplex_half_extract(
        &mut self,
        symbol: &str,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        result_ty: Option<&ResolvedTy>,
    ) -> Option<Place> {
        let Some(receiver_expr) = hir_args.first() else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("{symbol} with no receiver arg"),
                    site,
                },
                note: format!("`{symbol}` requires the Duplex receiver argument"),
            });
            return None;
        };
        // Consuming receiver: the recorded `IntentKind::Consume` drives the
        // move-mark inside `lower_value`.
        let recv_place = self.lower_value(receiver_expr)?;

        // The result type is the half handle type (`SendHalf<S>` / `RecvHalf<R>`);
        // consume it as the backing local's type so the handle's element layout is
        // recorded for codegen (`checker-authority`).
        let half_ty = result_ty.map_or(
            ResolvedTy::Named {
                name: if symbol == "hew_duplex_send_half" {
                    "SendHalf".to_string()
                } else {
                    "RecvHalf".to_string()
                },
                args: vec![],
                builtin: None,
                is_opaque: true,
            },
            |ty| self.subst_ty(ty),
        );
        let backing = self.alloc_local(half_ty);
        let Place::Local(n) = backing else {
            unreachable!("alloc_local returns Place::Local");
        };
        let half_place = if symbol == "hew_duplex_send_half" {
            Place::SendHalf(n)
        } else {
            Place::RecvHalf(n)
        };

        self.push_instr(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new(symbol, vec![recv_place], Some(half_place))
                .expect("half-extract symbol is an allowlisted runtime symbol"),
        ));

        Some(half_place)
    }

    /// Lower `.send(msg)` / `.try_send(msg)` on a `SendHalf<S>`.
    ///
    /// Mirrors `lower_duplex_send`'s `(handle, msg_ptr, len)` ABI: the receiver
    /// is a borrowing `SendHalf` Place (NON-consuming — a half is sent on many
    /// times before its own close), the message spills into the fixed 8-byte
    /// integer slot, and the runtime's i32 status materialises into the
    /// checker-recorded `Result<(), SendError>` in value context (discarded in
    /// statement context). Any other recorded shape fails closed
    /// (`checker-authority`, `boundary-fail-closed`).
    pub(crate) fn lower_half_send(
        &mut self,
        symbol: &str,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
        result_ty: Option<&ResolvedTy>,
    ) -> Option<Place> {
        if hir_args.len() < 2 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("{symbol} with fewer than 2 args"),
                    site,
                },
                note: format!("`{symbol}` requires receiver + message arguments"),
            });
            return None;
        }
        // args[0] = SendHalf receiver (borrow; no Move emitted).
        let recv_place = self.lower_value(&hir_args[0]);
        // args[1] = message value.
        let msg_place = self.lower_value(&hir_args[1]);
        let (Some(recv_place), Some(msg_place)) = (recv_place, msg_place) else {
            return None;
        };

        let resolved_result = result_ty.map(|ty| self.subst_ty(ty));
        if !resolved_result
            .as_ref()
            .is_some_and(is_unit_send_error_result)
        {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("`{symbol}` whose result is not Result<(), SendError>"),
                    site,
                },
                note: format!(
                    "`{symbol}` materializes only `Result<(), SendError>`; got \
                     {resolved_result:?}"
                ),
            });
            return None;
        }
        let dest = match context {
            RuntimeCallContext::ValueNeeded => Some(self.alloc_local(
                resolved_result.expect("tell-shaped result is Some after the guard above"),
            )),
            RuntimeCallContext::Discarded => None,
        };

        // The fixed 8-byte integer payload length (mirrors `lower_duplex_send`).
        // SHIM: the integer spine always uses 8-byte i64 values, so the length
        // is a compile-time constant for the scalar message shape.
        // WHEN obsolete: when the typed-message ABI lands a per-type payload size.
        // WHAT: replace with a sizeof/alignof expression or a typed ABI.
        let len_place = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::ConstI64 {
            dest: len_place,
            value: 8,
        });

        self.push_instr(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new(symbol, vec![recv_place, msg_place, len_place], dest)
                .expect("half-send symbol is an allowlisted runtime symbol"),
        ));

        dest
    }

    /// Lower the recv family: `.recv()` / `.try_recv()` on a `RecvHalf<R>` and
    /// `.recv()` / `.try_recv()` on a unified `Duplex<S, R>` (channel mode).
    ///
    /// Every symbol shares the `(handle, out_ptr, out_len) -> i32` runtime ABI:
    /// the runtime writes the received payload into caller out-params and returns
    /// a `RecvError` status. The receiver is a borrowing handle Place
    /// (NON-consuming — recv is called repeatedly until close). The checker
    /// records `Result<R, RecvError>`; this producer allocates the dest typed
    /// from that recorded shape and lets codegen materialise the tagged union
    /// from the out-params + status (the recv codegen arm owns the payload copy
    /// and `hew_duplex_payload_free`). Fails closed on any non-recv-Result shape
    /// (`checker-authority`, `boundary-fail-closed`).
    pub(crate) fn lower_duplex_recv(
        &mut self,
        symbol: &str,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
        result_ty: Option<&ResolvedTy>,
    ) -> Option<Place> {
        let Some(receiver_expr) = hir_args.first() else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("{symbol} with no receiver arg"),
                    site,
                },
                note: format!("`{symbol}` requires the receiver handle argument"),
            });
            return None;
        };
        // Borrowing receiver (recv is non-consuming).
        let recv_place = self.lower_value(receiver_expr)?;

        let resolved_result = result_ty.map(|ty| self.subst_ty(ty));
        let Some(resolved_result) = resolved_result else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("{symbol} with no recorded result type"),
                    site,
                },
                note: format!("`{symbol}` requires a checker-recorded `Result<R, RecvError>`"),
            });
            return None;
        };
        if recv_result_payload_ty(&resolved_result).is_none() {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("`{symbol}` whose result is not Result<R, RecvError>"),
                    site,
                },
                note: format!(
                    "`{symbol}` materializes only `Result<R, RecvError>`; got \
                     {resolved_result:?}"
                ),
            });
            return None;
        }

        // The recv runtime ABI always writes its payload out-params and returns a
        // status; codegen materialises the Result from those, so a dest is
        // required even in statement context (the status carries the closed/empty
        // signal the user discards but the codegen arm still consumes). Allocate
        // the dest from the checker-recorded shape unconditionally.
        let dest = self.alloc_local(resolved_result);
        self.push_instr(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new(symbol, vec![recv_place], Some(dest))
                .expect("recv symbol is an allowlisted runtime symbol"),
        ));

        match context {
            RuntimeCallContext::ValueNeeded => Some(dest),
            RuntimeCallContext::Discarded => None,
        }
    }

    /// Lower `.close()` on a `SendHalf<S>` / `RecvHalf<R>`.
    ///
    /// The checker rewrites half-close to `hew_duplex_close_half` and marks the
    /// receiver consuming, so `lower_value` transitions the half binding to
    /// `Consumed` and suppresses its scope-exit close — this call is the single
    /// close on the consuming path (the runtime's `AtomicBool` guard keeps a
    /// re-entrant close safe). The runtime ABI is `(half_ptr, direction) -> i32`
    /// with `direction` SendHalf=0 / RecvHalf=1; the direction is carried by the
    /// receiver's `Place` variant (`codegen-abi-authority`), and codegen
    /// materialises the discriminant at the call from that variant exactly as
    /// the drop-elaboration close already does. The i32 status materialises into
    /// the checker-recorded `Result<(), CloseError>`; any other shape fails
    /// closed.
    pub(crate) fn lower_half_close(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
        result_ty: Option<&ResolvedTy>,
    ) -> Option<Place> {
        let Some(receiver_expr) = hir_args.first() else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "hew_duplex_close_half with no receiver arg".to_string(),
                    site,
                },
                note: "`hew_duplex_close_half` requires the half-handle argument".to_string(),
            });
            return None;
        };
        // Consuming receiver: recorded `IntentKind::Consume` drives the move-mark.
        let recv_place = self.lower_value(receiver_expr)?;

        // The receiver must be a half Place — that variant is the direction
        // authority codegen reads. A non-half Place is a producer-contract
        // violation; fail closed rather than emit a mis-directed close.
        if !matches!(recv_place, Place::SendHalf(_) | Place::RecvHalf(_)) {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("hew_duplex_close_half on non-half handle ({recv_place:?})"),
                    site,
                },
                note: "`hew_duplex_close_half` requires a SendHalf/RecvHalf receiver; the \
                       direction discriminant is selected from the Place variant"
                    .to_string(),
            });
            return None;
        }

        let resolved_result = result_ty.map(|ty| self.subst_ty(ty));
        if !resolved_result
            .as_ref()
            .is_some_and(is_unit_close_error_result)
        {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "`half.close` whose result is not Result<(), CloseError>"
                        .to_string(),
                    site,
                },
                note: format!(
                    "half-handle `.close()` materializes only `Result<(), CloseError>`; \
                     got {resolved_result:?}"
                ),
            });
            return None;
        }
        let dest =
            match context {
                RuntimeCallContext::ValueNeeded => Some(self.alloc_local(
                    resolved_result.expect("close result is Some after the guard above"),
                )),
                RuntimeCallContext::Discarded => None,
            };

        // The direction discriminant is a codegen-side projection of the
        // receiver `Place` variant (SendHalf=0 / RecvHalf=1), mirroring the
        // drop-elaboration half-close. The producer passes only the handle; the
        // codegen arm appends the discriminant from the Place.
        self.push_instr(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new("hew_duplex_close_half", vec![recv_place], dest)
                .expect("hew_duplex_close_half is an allowlisted runtime symbol"),
        ));

        dest
    }

    pub(crate) fn lower_actor_payload(
        &mut self,
        args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        match args {
            [] => Some(self.alloc_local(ResolvedTy::Unit)),
            [arg] => self.lower_value(arg),
            _ => self.lower_packed_args_payload(args, site),
        }
    }

    /// Pack a multi-argument actor payload into a synthetic anonymous-record
    /// Place — the single shared payload mechanism for every multi-arg actor
    /// send shape (tell, ask, select arm, join branch, and the lambda-actor
    /// multi-param message).
    ///
    /// Wire contract: the packed record is a stack local in the caller's
    /// frame, filled field-by-field via `RecordInit` (struct GEP + store,
    /// each field written at exactly `sizeof(field_ty)`). Codegen derives
    /// `(ptr, sizeof(packed_record))` from the Place through the existing
    /// `actor_payload_ptr_size` path and the mailbox deep-copies that many
    /// bytes — the same bounded-copy invariant the single-arg wire carries.
    /// The receive side (`emit_actor_dispatch_trampoline`) reconstructs the
    /// identical non-packed LLVM struct from the handler's `param_tys` and
    /// unpacks each param at its natural field offset, so both ends agree on
    /// the layout by construction (same field types, same order, same
    /// `packed = false` struct rules).
    ///
    /// Ownership: the checker (`enforce_actor_method_send_args`) marks every
    /// arg moved at the boundary, so a heap-owning field's caller binding is
    /// dead after the send; the field store into the packed record IS the
    /// move. The packed temp itself is a non-binding local — no scope-exit
    /// drop is scheduled for it, so the bytes (including any heap pointers)
    /// have exactly one consumer: the mailbox node the runtime deep-copies
    /// them into.
    ///
    /// The minted type name is module-unique (`owner` symbol + per-function
    /// monotonic id), so two handlers with identical field types can never
    /// collide in `record_layouts` / the codegen struct map.
    pub(crate) fn lower_packed_args_payload(
        &mut self,
        args: &[hew_hir::HirExpr],
        _site: hew_hir::SiteId,
    ) -> Option<Place> {
        let mut field_places: Vec<Place> = Vec::with_capacity(args.len());
        let mut field_tys: Vec<ResolvedTy> = Vec::with_capacity(args.len());
        for arg in args {
            let place = self.lower_value(arg)?;
            field_places.push(place);
            field_tys.push(self.subst_ty(&arg.ty));
        }
        Some(self.pack_actor_payload_from_places(field_places, field_tys))
    }

    /// The pack half of [`Self::lower_packed_args_payload`]: mint the
    /// module-unique packed-record layout and emit the `RecordInit` over
    /// ALREADY-LOWERED argument places. Split out so the fungible-child tell
    /// path can evaluate arguments before its liveness branch (argument
    /// effects are user-visible and must not depend on child liveness) while
    /// building the packed temp — whose bytes the `Send` alone consumes — on
    /// the delivery edge only. Pure MIR construction: no user effect runs
    /// here, so the emission point is free to move across control flow.
    fn pack_actor_payload_from_places(
        &mut self,
        field_places: Vec<Place>,
        field_tys: Vec<ResolvedTy>,
    ) -> Place {
        let packed_id = self.next_closure_id;
        self.next_closure_id = self
            .next_closure_id
            .checked_add(1)
            .expect("packed-args id overflow — closure id counter exhausted");
        let owner = Self::sanitize_symbol_component(&self.current_function_symbol);
        let packed_name = format!("__hew_packed_args_{owner}_{packed_id}");
        let packed_ty = ResolvedTy::Named {
            name: packed_name.clone(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        };
        self.closure_record_layouts
            .push(crate::model::RecordLayout {
                name: packed_name,
                field_tys,
                // Compiler-internal packed-args record: positional `-g` names.
                field_names: Vec::new(),
            });

        let fields: Vec<(FieldOffset, Place)> = field_places
            .into_iter()
            .enumerate()
            .map(|(idx, place)| {
                (
                    FieldOffset(
                        u32::try_from(idx)
                            .expect("packed-args field count exceeds u32::MAX — impossible in Hew"),
                    ),
                    place,
                )
            })
            .collect();
        let dest = self.alloc_local(packed_ty.clone());
        self.push_instr(Instr::RecordInit {
            ty: packed_ty,
            fields,
            dest,
        });
        dest
    }

    pub(crate) fn actor_method_info(
        &mut self,
        receiver_ty: &ResolvedTy,
        method_id: &str,
        site: hew_hir::SiteId,
    ) -> Option<ActorMethodInfo> {
        let actor_name = actor_name_from_handle_ty(receiver_ty)?;
        let method_name = method_name_from_id(method_id);
        let Some(layout) = self.actor_layouts.get(actor_name) else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("actor call on unknown actor `{actor_name}`"),
                    site,
                },
                note: "receiver type named an actor with no MIR actor layout".to_string(),
            });
            return None;
        };
        let Some(handler) = layout
            .handlers
            .iter()
            .find(|handler| handler.name == method_name)
        else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("unknown actor handler `{method_id}` on `{actor_name}`"),
                    site,
                },
                note:
                    "actor method dispatch side table named a handler absent from the actor layout"
                        .to_string(),
            });
            return None;
        };
        Some(ActorMethodInfo {
            msg_type: handler.msg_type,
            param_tys: handler.param_tys.clone(),
            return_ty: handler.return_ty.clone(),
        })
    }

    /// Returns the `(sup, slot)` fungible reference for `place` if it is a
    /// supervisor-child handle, else `None`.
    pub(crate) fn fungible_child_ref_of(&self, place: Place) -> Option<FungibleChildRef> {
        match place {
            Place::ActorHandle(id) => self.fungible_child_refs.get(&id).copied(),
            _ => None,
        }
    }

    /// F-04: re-resolve a fungible child reference into its handle alloca at the
    /// send site, branching on the current liveness of the slot.
    ///
    /// Emits `hew_supervisor_child_get(sup, slot)`, stores the fresh `*mut
    /// HewActor` into `handle_place`, and branches:
    /// - Live (`tag == 0`)  → continues in the returned `live_bb` (cursor parked
    ///   there) with `handle_place` holding the CURRENT live child pointer.
    /// - not-Live (`tag != 0`) → branches to `recover_bb` (the caller wires the
    ///   recoverable fail-closed path there — the tell releases its undelivered
    ///   payload values and skips the Send).
    ///
    /// Because the pointer is fetched fresh under the slot lock at the instant of
    /// the send and used immediately in the same turn, the "valid only within the
    /// current scheduler turn" borrow contract is honoured structurally rather
    /// than by user discipline — the stale-handle UAF cannot arise.
    ///
    /// Returns `(live_bb, recover_bb)`. The current block is finished with the
    /// liveness branch; NEITHER block is started — the caller fills `recover_bb`
    /// first, then `start_block(live_bb)` to continue with the send (this mirrors
    /// the trap-block-then-success-block ordering the old accessor used, keeping
    /// the `start_block` empty-buffer invariant satisfied).
    fn emit_fungible_reresolve(
        &mut self,
        child_ref: FungibleChildRef,
        handle_place: Place,
    ) -> (u32, u32) {
        let FungibleChildRef {
            sup_place,
            slot_index,
        } = child_ref;

        // Re-fetch into the same opaque struct so we can read BOTH the tag and
        // the handle. We reuse the result place for both extracts.
        let idx_place = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::ConstI64 {
            dest: idx_place,
            value: i64::from(slot_index),
        });
        let result_place = self.alloc_local(ResolvedTy::Named {
            name: CHILD_LOOKUP_RESULT_TY_NAME.to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        });
        self.push_instr(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new(
                "hew_supervisor_child_get",
                vec![sup_place, idx_place],
                Some(result_place),
            )
            .expect("hew_supervisor_child_get is an allowlisted runtime symbol"),
        ));

        // Store the fresh handle pointer (field 1) into the handle alloca so the
        // Send terminator delivers to the CURRENT child.
        let raw_handle = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::RecordFieldLoad {
            record: result_place,
            field_offset: FieldOffset(1),
            dest: raw_handle,
        });
        self.push_instr(Instr::Move {
            dest: handle_place,
            src: raw_handle,
        });

        // Extract the tag (field 0) and branch on liveness.
        let tag_place = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::RecordFieldLoad {
            record: result_place,
            field_offset: FieldOffset(0),
            dest: tag_place,
        });
        let zero_place = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::ConstI64 {
            dest: zero_place,
            value: 0,
        });
        let is_live_flag = self.alloc_local(ResolvedTy::Bool);
        self.push_instr(Instr::IntCmp {
            pred: CmpPred::Eq,
            lhs: tag_place,
            rhs: zero_place,
            dest: is_live_flag,
        });

        let live_bb = self.alloc_block();
        let recover_bb = self.alloc_block();
        self.finish_current_block(Terminator::Branch {
            cond: is_live_flag,
            then_target: live_bb,
            else_target: recover_bb,
        });
        (live_bb, recover_bb)
    }

    /// Release one undelivered actor-tell payload value on the not-live
    /// recover edge of a fungible supervisor-child send (#2126).
    ///
    /// The delivered edge's `Terminator::Send` is the payload's one consumer:
    /// the mailbox takes ownership of the value's bytes (heap pointers
    /// included), and the checker marks every send argument moved at the
    /// boundary, so no scope-exit drop covers the value. When the liveness
    /// branch takes the recover edge instead, the Send never runs — this
    /// helper stands in for it, releasing exactly the ownership the delivered
    /// edge would have consumed. The two edges are exclusive per send
    /// execution, so the release runs exactly once.
    ///
    /// Shape dispatch, routed through the drop authorities:
    /// - a type that does not seed drop elaboration
    ///   (`binding_seeds_drop_elaboration` — the `BitCopy` spine) owns nothing;
    ///   no instruction is emitted.
    /// - a Wired leaf (`project_field_inline_drop_symbol`: `string`, `bytes`,
    ///   `Vec` with a wired element release, `HashMap`, `HashSet`, generator
    ///   handles) releases through one whole-value `Instr::Drop` — the same
    ///   inline release the overwrite path emits for these shapes. A static
    ///   string literal is safe here: `hew_string_drop` skips read-only
    ///   segment pointers via its `is_static_string` guard.
    /// - an Unwired `Vec` (element release protocol unwired) is refused fail
    ///   closed, per the picker contract ("a `Wired`-gated pre-flight can no
    ///   longer admit the buffer-only free"). Unreachable today: a receive
    ///   handler parameter of such a type is already rejected by the
    ///   scope-exit element scan in the handler's own body, so no tell can
    ///   target one — defence in depth, not a live diagnostic.
    /// - everything else returns with no instruction. The seed gate already
    ///   excluded `BitCopy`, so this arm is a View/handle shape with no release
    ///   obligation at this seam (e.g. an actor pid) — or an owned aggregate:
    ///   SHIM(F-04 recover-path aggregate payload): an owned-aggregate
    ///   payload (user record / tuple / enum with heap fields) has no inline
    ///   whole-value release — its drop kinds (`RecordInPlace` /
    ///   `TupleInPlace` / `EnumInPlace`) are function-scope drop-plan
    ///   entries, not inline `Instr::Drop`s — so an undelivered aggregate
    ///   payload still leaks its heap fields on the not-live edge. WHY: the
    ///   leak is bounded to restart/shutdown windows and refusing the shape
    ///   would reject every record tell through a supervisor child. WHEN
    ///   obsolete: when the type-directed drop-table consolidation gives
    ///   aggregates a whole-value release emittable at an arbitrary
    ///   instruction position. WHAT: route this arm through that whole-value
    ///   drop instead of returning empty-handed.
    fn emit_undelivered_send_payload_release(
        &mut self,
        place: Place,
        ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<()> {
        let ty = self.subst_ty(ty);
        if !self.binding_seeds_drop_elaboration(&ty) {
            return Some(());
        }
        match self.project_field_inline_drop_symbol(&ty) {
            ReleaseSymbolVerdict::Wired(symbol) => {
                self.push_instr(Instr::Drop {
                    place,
                    ty,
                    drop_fn: Some(crate::model::DropFnSpec::Release(symbol)),
                });
                Some(())
            }
            ReleaseSymbolVerdict::Unwired(_) => {
                let elem = self
                    .unsupported_vec_element_in_ty(&ty)
                    .unwrap_or_else(|| format!("`{}`", ty.user_facing()));
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!(
                            "actor tell payload: a `Vec` whose element is {elem} has no \
                             per-element release protocol, so a send skipped at a \
                             not-live supervisor child would leak its heap nodes"
                        ),
                        site,
                    },
                    note: "a `Vec` of `bytes` or of an indirect-enum element cannot yet \
                           be released element-by-element, and a fungible-child tell \
                           must free an undelivered payload on the not-live recover \
                           edge. This construction is rejected at compile rather than \
                           silently leaked, and becomes available once the per-element \
                           release is wired."
                        .to_string(),
                });
                None
            }
            // `WiredInPlace` is the yield/recv picker's composite verdict;
            // `project_field_inline_drop_symbol` never returns it (owned
            // aggregates stay `NoDropPath` here — the F-04 SHIM above). Kept
            // as an explicit arm so admitting composites at THIS seam is a
            // deliberate decision, not an accidental fall-through.
            ReleaseSymbolVerdict::WiredInPlace(_) | ReleaseSymbolVerdict::NoDropPath => Some(()),
        }
    }

    pub(crate) fn lower_actor_send(
        &mut self,
        receiver: &HirExpr,
        method_id: &str,
        args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        let info = self.actor_method_info(&receiver.ty, method_id, site)?;
        // `ActorMethodKind::Fire` is only produced by `record_actor_method_dispatch`
        // when `reply_ty == Ty::Unit`, so every `ActorSend` HIR node refers to a
        // unit-returning handler by construction.  The arity check below is the
        // remaining structural guard.
        if info.param_tys.len() != args.len() {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("actor send arity mismatch for `{method_id}`"),
                    site,
                },
                note: format!(
                    "handler expects {} argument(s), call supplied {}",
                    info.param_tys.len(),
                    args.len()
                ),
            });
            return None;
        }
        let actor = self.lower_value(receiver)?;
        let child_ref = self.fungible_child_ref_of(actor);
        // Argument evaluation stays HERE, in the pre-branch block: an argument
        // expression's effects are user-visible and must run whether or not a
        // fungible child is live — the liveness branch below decides DELIVERY,
        // never evaluation.
        let mut lowered: Vec<(Place, ResolvedTy)> = Vec::with_capacity(args.len());
        for arg in args {
            let place = self.lower_value(arg)?;
            let ty = self.subst_ty(&arg.ty);
            lowered.push((place, ty));
        }
        // The payload Place. Zero args: a unit local (owns nothing). One arg:
        // the argument's own place — no pack exists. Multi-arg: the packed
        // anonymous record; through a FUNGIBLE child reference the pack is
        // deferred into `live_bb` below, so the packed temp — whose bytes the
        // `Send` alone consumes — is never built on the discard path.
        let mut value = match &lowered[..] {
            [] => Some(self.alloc_local(ResolvedTy::Unit)),
            [(place, _)] => Some(*place),
            _ => None,
        };
        if value.is_none() && child_ref.is_none() {
            let (field_places, field_tys): (Vec<Place>, Vec<ResolvedTy>) =
                lowered.iter().cloned().unzip();
            value = Some(self.pack_actor_payload_from_places(field_places, field_tys));
        }
        let next = self.alloc_block();
        // F-04: a fire-and-forget send through a FUNGIBLE supervisor-child
        // reference re-resolves the current live child at the send site. On a
        // not-live slot (mid-restart or permanently down) the send fail-closes as
        // a recoverable no-op (the message is dropped, NOT a program-killing
        // trap) — the tell's contract is best-effort delivery, so dropping into a
        // restart window is the correct recoverable behaviour. The `recover_bb`
        // releases the undelivered payload values (#2126) and joins straight to
        // `next` so control flow continues normally; `live_bb` becomes the
        // current cursor where the multi-arg pack (if any) is built and the Send
        // terminator is emitted with the freshly-resolved child pointer.
        if let Some(child_ref) = child_ref {
            let (live_bb, recover_bb) = self.emit_fungible_reresolve(child_ref, actor);
            // recover_bb: not-live → the Send is skipped, so nothing consumes
            // the already-evaluated argument values. Release each one exactly
            // as the delivered edge would have consumed it (the two edges are
            // exclusive, so the release runs exactly once), then continue.
            self.start_block(recover_bb);
            for (place, ty) in &lowered {
                self.emit_undelivered_send_payload_release(*place, ty, site)?;
            }
            self.finish_current_block(Terminator::Goto { target: next });
            // live_bb: the freshly-resolved current child; the Send below
            // targets it. The multi-arg pack is built here, on the delivery
            // edge only (pure MIR construction over the pre-branch argument
            // places — no user effect moves across the branch).
            self.start_block(live_bb);
            if value.is_none() {
                let (field_places, field_tys): (Vec<Place>, Vec<ResolvedTy>) =
                    lowered.into_iter().unzip();
                value = Some(self.pack_actor_payload_from_places(field_places, field_tys));
            }
        }
        let value = value.expect("payload place is populated for every arity above");
        // Determine alias mode: look up the first argument's span in the
        // checker's `actor_send_aliasing` map.  Only an explicit `Alias`
        // classification promotes the mode; every `Copy(reason)` variant and
        // every absent entry defaults to `Copy` (fail-closed).
        let alias_mode = if args.len() == 1 {
            let key = hew_types::SpanKey::from(&args[0].span);
            match self.actor_send_aliasing.get(&key).copied() {
                Some(hew_types::ActorSendAliasing::Alias) => crate::model::SendAliasMode::Alias,
                // All Copy(reason) variants and missing entries → Copy (fail-closed).
                _ => crate::model::SendAliasMode::Copy,
            }
        } else {
            // Zero-arg send and the multi-arg packed-record payload: the
            // payload Place is unit / a fresh packed temp, never the first
            // arg's binding, so the per-arg alias classification does not
            // transfer — fail-closed Copy.
            crate::model::SendAliasMode::Copy
        };
        self.finish_current_block(Terminator::Send {
            actor,
            msg_type: info.msg_type,
            value,
            next,
            alias_mode,
        });
        self.start_block(next);
        None
    }

    /// Lower a `receive gen fn` call (`e.ticks()`, `t.stream(3)`) — decision 4
    /// for receive-gen-fn. Constructs a per-call bounded channel, then
    /// tell-sends a "start" message (the call's args plus the sink half) to
    /// the actor's stream-producer pump; the expression value is the stream
    /// half. No `await` here — the checker's ask-without-await guard already
    /// exempts generator methods.
    ///
    /// `ActorHandlerLayout`'s producer row (`lower_actor_handler_layouts`)
    /// carries `param_tys` = the handler's own params PLUS one trailing
    /// sink — the single pack/unpack authority shared with the dispatch
    /// trampoline's generic per-handler arg-unpack loop. Packing here mirrors
    /// `lower_actor_send`'s convention exactly: a single total field rides
    /// bare (no pack); two or more use `pack_actor_payload_from_places`.
    pub(crate) fn lower_actor_gen_stream(
        &mut self,
        receiver: &HirExpr,
        method: &str,
        args: &[hew_hir::HirExpr],
        expr: &HirExpr,
    ) -> Option<Place> {
        let site = expr.site;
        let info = self.actor_method_info(&receiver.ty, method, site)?;
        let Some((sink_ty, real_param_tys)) = info
            .param_tys
            .split_last()
            .map(|(sink, rest)| (sink.clone(), rest.to_vec()))
        else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("actor stream-producer `{method}` has no sink param"),
                    site,
                },
                note: "the producer row must carry at least the synthetic trailing sink \
                       (lower_actor_handler_layouts)"
                    .to_string(),
            });
            return None;
        };
        if real_param_tys.len() != args.len() {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("actor stream-producer arity mismatch for `{method}`"),
                    site,
                },
                note: format!(
                    "handler expects {} argument(s), call supplied {}",
                    real_param_tys.len(),
                    args.len()
                ),
            });
            return None;
        }

        // `receive gen fn` calls do not route through `lower_direct_call`, so
        // enforce the same closure-env provenance boundary here before packing
        // the start message.
        self.reject_unproven_generator_fn_args(args);

        let actor = self.lower_value(receiver)?;
        let mut lowered: Vec<(Place, ResolvedTy)> = Vec::with_capacity(args.len() + 1);
        for arg in args {
            let place = self.lower_value(arg)?;
            let ty = self.subst_ty(&arg.ty);
            lowered.push((place, ty));
        }

        let stream_ty = self.subst_ty(&expr.ty);
        let (sink, stream) = self.build_receive_gen_channel(&sink_ty, stream_ty);

        lowered.push((sink, sink_ty));
        let value = if let [(place, _)] = &lowered[..] {
            *place
        } else {
            let (field_places, field_tys): (Vec<Place>, Vec<ResolvedTy>) =
                lowered.into_iter().unzip();
            self.pack_actor_payload_from_places(field_places, field_tys)
        };

        let next = self.alloc_block();
        self.finish_current_block(Terminator::Send {
            actor,
            msg_type: info.msg_type,
            value,
            next,
            // Args crossing into a stream-producer start message use the same
            // fail-closed default as the zero/multi-arg `ActorSend` cases;
            // per-arg alias classification for stream calls is a follow-on.
            alias_mode: crate::model::SendAliasMode::Copy,
        });
        self.start_block(next);
        Some(stream)
    }

    /// Construct the per-call bounded channel for a `receive gen fn` dispatch
    /// and extract both halves, returning `(sink, stream)`.
    ///
    /// The pair box (`hew_stream_channel`) is a transient two-pointer carrier
    /// whose lifetime is exactly this sequence: `hew_stream_pair_sink` and
    /// `hew_stream_pair_stream` each null-store their field on extraction
    /// (ownership transfer), so once both halves are out the box owns neither
    /// half. `hew_stream_pair_free` then releases ONLY the empty carrier —
    /// never the sink or stream — and is null-guarded, so a fail-closed null
    /// `pair` is a no-op rather than a fault. Skipping the free leaks the
    /// carrier box once per call.
    ///
    /// The capacity is a laziness/throughput tunable (delivery is eager
    /// producer drive with bounded-channel backpressure; a small buffer keeps
    /// it pull-equivalent).
    fn build_receive_gen_channel(
        &mut self,
        sink_ty: &ResolvedTy,
        stream_ty: ResolvedTy,
    ) -> (Place, Place) {
        let capacity = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::ConstI64 {
            dest: capacity,
            value: RECEIVE_GEN_STREAM_CAPACITY,
        });
        let opaque_ptr_ty = ResolvedTy::Pointer {
            is_mutable: true,
            pointee: Box::new(ResolvedTy::Unit),
        };
        let pair = self.alloc_local(opaque_ptr_ty);
        let after_channel = self.alloc_block();
        self.finish_current_block(Terminator::Call {
            callee: "hew_stream_channel".to_string(),
            builtin: None,
            args: vec![capacity],
            dest: Some(pair),
            next: after_channel,
        });
        self.start_block(after_channel);

        let sink = self.alloc_local(sink_ty.clone());
        let after_sink = self.alloc_block();
        self.finish_current_block(Terminator::Call {
            callee: "hew_stream_pair_sink".to_string(),
            builtin: None,
            args: vec![pair],
            dest: Some(sink),
            next: after_sink,
        });
        self.start_block(after_sink);

        let stream = self.alloc_local(stream_ty);
        let after_stream = self.alloc_block();
        self.finish_current_block(Terminator::Call {
            callee: "hew_stream_pair_stream".to_string(),
            builtin: None,
            args: vec![pair],
            dest: Some(stream),
            next: after_stream,
        });
        self.start_block(after_stream);

        // Free the now-empty carrier (see the doc comment above): both halves
        // are extracted, so this releases only the two-pointer box.
        let after_free = self.alloc_block();
        self.finish_current_block(Terminator::Call {
            callee: "hew_stream_pair_free".to_string(),
            builtin: None,
            args: vec![pair],
            dest: None,
            next: after_free,
        });
        self.start_block(after_free);

        (sink, stream)
    }

    pub(crate) fn lower_actor_ask(
        &mut self,
        receiver: &HirExpr,
        method_id: &str,
        args: &[hew_hir::HirExpr],
        reply_ty: &ResolvedTy,
        deadline_ns: Option<i64>,
        expr: &HirExpr,
    ) -> Option<Place> {
        let site = expr.site;
        let info = self.actor_method_info(&receiver.ty, method_id, site)?;
        if info.return_ty != *reply_ty {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("actor ask reply type mismatch for `{method_id}`"),
                    site,
                },
                note: format!(
                    "handler returns {}, ask expression expects {}",
                    info.return_ty.user_facing(),
                    reply_ty.user_facing()
                ),
            });
            return None;
        }
        if info.param_tys.len() != args.len() {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("actor ask arity mismatch for `{method_id}`"),
                    site,
                },
                note: format!(
                    "handler expects {} argument(s), call supplied {}",
                    info.param_tys.len(),
                    args.len()
                ),
            });
            return None;
        }
        let actor = self.lower_value(receiver)?;
        // F-04: an ask through a FUNGIBLE supervisor-child reference re-resolves
        // the current child at the ask site. Unlike the tell path, the ask needs
        // NO liveness branch: a not-live slot resolves to a null handle, and the
        // existing ask err path fail-closes a null/stale actor to
        // `Err(AskError::ActorStopped)` (`actor_send_result_internal_reply`
        // null-guards at actor.rs:4078 → `ErrActorStopped` →
        // `hew_actor_ask_take_last_error` → `Err`). So storing the freshly
        // resolved pointer (null when not-live) into the handle alloca is
        // sufficient: a live child is asked, a not-live one yields a recoverable
        // `Err` rather than a UAF or trap.
        if let Some(child_ref) = self.fungible_child_ref_of(actor) {
            self.emit_child_get_into(child_ref.sup_place, child_ref.slot_index, actor);
        }
        let value = self.lower_actor_payload(args, site)?;
        // `result_dest` holds `Result<R, AskError>` — the R-ASK unified return type.
        // Its type comes from the HIR expression's checker-assigned type, which is
        // `Result<reply_ty, AskError>` after the unification fix in the type checker.
        let result_dest = self.alloc_local(self.subst_ty(&expr.ty));
        let reply_dest = self.alloc_local(reply_ty.clone());
        let error_dest = self.alloc_local(ResolvedTy::Named {
            name: "AskError".to_string(),
            args: Vec::new(),
            builtin: Some(BuiltinType::AskError),
            is_opaque: false,
        });
        let next = self.alloc_block();
        // Suspendable-caller flip (W6.010, E2/D-W2). A caller that carries the
        // execution context (an actor handler / closure / task entry) runs on
        // the scheduler as a coroutine and can PARK its continuation: emit the
        // non-blocking `SuspendingAsk` so the ask suspends (freeing the worker)
        // and resumes on the reply, binding the value on the resume edge. A
        // `FunctionCallConv::Default` caller (`main`, a free function) runs on a
        // foreign/main thread with no parkable continuation, so it keeps the
        // blocking `Terminator::Ask` (the condvar path, E6). The resume edge IS
        // `next` — lowering continues in the same block where the ask result is
        // already bound; `cleanup` routes to the codegen single-teardown
        // epilogue (`SuspendingAsk` carries no separate MIR cleanup block, so it
        // reuses `next` as the drop-elaboration cleanup seam, exactly as the
        // codegen `Terminator::Suspend` case-1 edge routes to the shared
        // epilogue).
        if self.current_function_call_conv.carries_execution_context() {
            // NEW-6b: record an `await … | after d` deadline for THIS block (the
            // one finished with `SuspendingAsk`). Only a suspendable caller carries
            // the deadline — a blocking `Terminator::Ask` (foreign/main thread) has
            // no parkable continuation to time out, so a deadline there fails
            // closed below. Literal-only ns (constant side-table, no Place
            // threaded into the IR).
            if let Some(ns) = deadline_ns {
                self.await_deadline_ns.insert(self.current_block_id, ns);
            }
            self.record_suspend_kind(SuspendKind::Ask {
                actor,
                msg_type: info.msg_type,
                value,
                result_dest,
                reply_dest,
                error_dest,
            });
            self.finish_current_block(Terminator::Suspend {
                resume: next,
                cleanup: next,
                is_final: false,
            });
        } else {
            // A blocking caller (`main`, a free function) has no parkable
            // continuation to time out: the deadline cannot be armed on the
            // condvar path, and silently dropping it would turn `| after d`
            // into an unbounded wait. Refuse instead of miscompiling.
            if deadline_ns.is_some() {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!(
                            "`await {method_id}(...) | after d` from a blocking caller \
                             (`main` or a free function)"
                        ),
                        site,
                    },
                    note: "the deadline timer arms against a suspended continuation; \
                           only actor handlers, closures, and task entries suspend — \
                           move the deadline ask into an actor handler"
                        .to_string(),
                });
                return None;
            }
            self.finish_current_block(Terminator::Ask {
                actor,
                msg_type: info.msg_type,
                value,
                result_dest,
                reply_dest,
                error_dest,
                next,
            });
        }
        self.start_block(next);
        Some(result_dest)
    }

    fn remote_actor_method_info(
        &mut self,
        receiver_ty: &ResolvedTy,
        msg_ty: &ResolvedTy,
        reply_ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<ActorMethodInfo> {
        let actor_name = actor_name_from_remote_pid_ty(receiver_ty)?;
        let Some(layout) = self.actor_layouts.get(actor_name) else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("remote actor ask on unknown actor `{actor_name}`"),
                    site,
                },
                note: "RemotePid<T> named an actor with no MIR actor layout".to_string(),
            });
            return None;
        };
        // The remote wire carries exactly ONE message value, so only
        // single-parameter handlers are remote-dispatchable. A multi-arg
        // handler whose first param matches the sent message must NOT be
        // selected: the local wire for that handler is the packed-args
        // anonymous record, and a single-value payload delivered to a
        // body that unpacks the full record reads out of bounds on the
        // receiving node. Fail closed with a distinct diagnostic
        // (`serializer-fail-closed`); the cross-node payload
        // serialization lane lands the positive path.
        let Some(handler) = layout.handlers.iter().find(|handler| {
            handler.param_tys.len() == 1
                && handler
                    .param_tys
                    .first()
                    .is_some_and(|param| param == msg_ty)
                && handler.return_ty == *reply_ty
        }) else {
            if let Some(multi) = layout.handlers.iter().find(|handler| {
                handler.param_tys.len() > 1
                    && handler
                        .param_tys
                        .first()
                        .is_some_and(|param| param == msg_ty)
                    && handler.return_ty == *reply_ty
            }) {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::RemotePayloadUnsupported {
                        actor: actor_name.to_string(),
                        handler: multi.name.clone(),
                        site,
                    },
                    note: format!(
                        "receive fn `{}` takes {} parameters; a remote ask carries one \
                         message value and the cross-node codec is not seeded for \
                         packed multi-arg payloads. Declare a single-parameter handler \
                         (e.g. wrapping the fields in a record) for remote dispatch.",
                        multi.name,
                        multi.param_tys.len()
                    ),
                });
                return None;
            }
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("remote actor ask handler lookup for `{actor_name}`"),
                    site,
                },
                note: format!(
                    "no receive handler accepts {} and returns {}",
                    msg_ty.user_facing(),
                    reply_ty.user_facing()
                ),
            });
            return None;
        };
        Some(ActorMethodInfo {
            msg_type: handler.msg_type,
            param_tys: handler.param_tys.clone(),
            return_ty: handler.return_ty.clone(),
        })
    }

    pub(crate) fn lower_remote_actor_ask(
        &mut self,
        receiver: &HirExpr,
        msg: &HirExpr,
        timeout_ms: &HirExpr,
        reply_ty: &ResolvedTy,
        expr: &HirExpr,
    ) -> Option<Place> {
        let msg_ty = self.subst_ty(&msg.ty);
        let info = self.remote_actor_method_info(&receiver.ty, &msg_ty, reply_ty, expr.site)?;
        let actor = self.lower_value(receiver)?;
        let value = self.lower_value(msg)?;
        let timeout_ms = self.lower_value(timeout_ms)?;
        let result_dest = self.alloc_local(self.subst_ty(&expr.ty));
        let reply_dest = self.alloc_local(reply_ty.clone());
        let error_dest = self.alloc_local(ResolvedTy::Named {
            name: "AskError".to_string(),
            args: Vec::new(),
            builtin: Some(BuiltinType::AskError),
            is_opaque: false,
        });
        let next = self.alloc_block();
        // Suspendable-caller flip (NEW-5). A caller that carries the execution
        // context (an actor handler / closure / task entry) runs on the
        // scheduler as a coroutine and can PARK its continuation across the
        // cross-node wire round-trip: emit the non-blocking
        // `SuspendingRemoteAsk` so the ask suspends (freeing the worker) and
        // resumes when the wire reply / peer-drop / timeout lands, binding the
        // `Result<Reply, AskError>` on the resume edge. A
        // `FunctionCallConv::Default` caller (`main`, a free function) runs on a
        // foreign/main thread with no parkable continuation, so it keeps the
        // blocking `Terminator::RemoteAsk` (`hew_node_api_ask`). The resume edge
        // IS `next` (lowering continues in the block where the result is bound);
        // `cleanup` reuses `next` as the multi-suspend drop-elaboration seam,
        // exactly as `SuspendingAsk` does.
        if self.current_function_call_conv.carries_execution_context() {
            self.record_suspend_kind(SuspendKind::RemoteAsk {
                actor,
                msg_type: info.msg_type,
                value,
                timeout_ms,
                result_dest,
                reply_dest,
                error_dest,
                reply_ty: reply_ty.clone(),
            });
            self.finish_current_block(Terminator::Suspend {
                resume: next,
                cleanup: next,
                is_final: false,
            });
        } else {
            self.finish_current_block(Terminator::RemoteAsk {
                actor,
                msg_type: info.msg_type,
                value,
                timeout_ms,
                result_dest,
                reply_dest,
                error_dest,
                reply_ty: reply_ty.clone(),
                next,
            });
        }
        self.start_block(next);
        Some(result_dest)
    }

    pub(crate) fn invalid_spawn_arg_note(
        actor_name: &str,
        name: &str,
        explicit_init: bool,
        init_param_names: &[String],
        state_field_names: &[String],
    ) -> String {
        if explicit_init {
            // Both init params and state fields are valid spawn args when an
            // init() block is present (COEXIST model). Report the full valid set.
            let params = format!(
                "[{}]",
                init_param_names
                    .iter()
                    .map(|n| format!("`{n}`"))
                    .collect::<Vec<_>>()
                    .join(", ")
            );
            let fields = format!(
                "[{}]",
                state_field_names
                    .iter()
                    .map(|n| format!("`{n}`"))
                    .collect::<Vec<_>>()
                    .join(", ")
            );
            format!(
                "actor `{actor_name}` has no init() parameter or state field named `{name}`; \
                 init() parameters are: {params}; state fields are: {fields}"
            )
        } else {
            let fields = format!(
                "[{}]",
                state_field_names
                    .iter()
                    .map(|n| format!("`{n}`"))
                    .collect::<Vec<_>>()
                    .join(", ")
            );
            format!(
                "actor `{actor_name}` has no state field named `{name}`; \
                 fields are: {fields}"
            )
        }
    }

    #[allow(
        clippy::too_many_lines,
        reason = "one coherent dispatcher: supervisor-spawn routing (config-arg \
                  threading + no-config gate) followed by the actor-spawn arg \
                  validation + state lowering; splitting would scatter the spawn \
                  contract across helpers"
    )]
    pub(crate) fn lower_spawn_actor(
        &mut self,
        actor_name: &str,
        args: &[(String, HirExpr)],
        expr: &HirExpr,
    ) -> Option<Place> {
        // ── Supervisor dispatch ───────────────────────────────────────────
        // Check if `actor_name` names a supervisor before falling through to
        // the actor-layout path. Supervisors are not in `actor_layouts`; their
        // spawn is routed to the synthesised bootstrap function via a
        // `Terminator::Call` rather than `Instr::SpawnActor`.
        if let Some(sup_layout) = self.supervisor_layout_map.get(actor_name).cloned() {
            // A config supervisor (`supervisor App(config: T)`) is spawned as
            // `spawn App(config: cfg)`: the single config value is threaded to
            // the bootstrap's config parameter, which codegen reads to build the
            // supervisor-owned config buffer for the init thunks. A no-config
            // supervisor takes no args. The arg count must match whether the
            // layout carries a config param (the HIR gate is the user-facing
            // diagnostic; this is the MIR backstop).
            let expected_args = usize::from(sup_layout.config_param.is_some());
            if args.len() != expected_args {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!(
                            "supervisor `{actor_name}` spawn expected {expected_args} \
                             argument(s) (config param {}), got {}",
                            if expected_args == 0 {
                                "absent"
                            } else {
                                "present"
                            },
                            args.len()
                        ),
                        site: expr.site,
                    },
                    note: "a config supervisor is spawned `spawn App(config: value)`; a \
                           no-config supervisor `spawn App`"
                        .to_string(),
                });
                return None;
            }
            let bootstrap_args: Vec<hew_hir::HirExpr> =
                args.iter().map(|(_, expr)| expr.clone()).collect();
            // Route `spawn Sup` → `Terminator::Call { bootstrap_symbol }`.
            // `lower_direct_call` allocates the destination local (typed
            // `LocalPid<Sup>` from `expr.ty`) and emits the call terminator,
            // passing the config value (if any) as the bootstrap's config arg.
            return self.lower_direct_call(
                &sup_layout.bootstrap_symbol,
                None,
                &bootstrap_args,
                &expr.ty,
                expr.site,
            );
        }
        // ── Actor dispatch (existing path) ───────────────────────────────
        let Some(layout) = self.actor_layouts.get(actor_name).cloned() else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("spawn of unknown actor `{actor_name}`"),
                    site: expr.site,
                },
                note: "named actor spawn requires a MIR actor layout".to_string(),
            });
            return None;
        };
        let explicit_init = layout.init_symbol.is_some();
        let mut explicit: HashMap<&str, &HirExpr> = HashMap::new();
        for (name, arg) in args {
            // COEXIST model (A155): when an init() block is present, spawn args
            // may name EITHER init() parameters (routed to the init call) OR
            // state fields (routed to the initial state record). Without init(),
            // only state fields are valid.
            let is_valid = if explicit_init {
                layout.init_param_names.iter().any(|n| n == name)
                    || layout.state_field_names.iter().any(|n| n == name)
            } else {
                layout.state_field_names.iter().any(|n| n == name)
            };
            if !is_valid {
                let note = Self::invalid_spawn_arg_note(
                    actor_name,
                    name,
                    explicit_init,
                    &layout.init_param_names,
                    &layout.state_field_names,
                );
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::InvalidActorSpawnArgument {
                        actor: actor_name.to_string(),
                        argument: name.clone(),
                        site: expr.site,
                    },
                    note,
                });
                return None;
            }
            if explicit.contains_key(name.as_str()) {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!("spawn `{actor_name}` duplicate argument `{name}`"),
                        site: expr.site,
                    },
                    note: "actor spawn arguments are named; each name may be supplied at most once"
                        .to_string(),
                });
                return None;
            }
            explicit.insert(name.as_str(), arg);
        }
        let init_args =
            self.lower_spawn_actor_init_args(actor_name, &layout, explicit_init, &explicit, expr)?;
        let Ok(state) = self.lower_spawn_actor_state_or_diag(
            actor_name,
            &layout,
            explicit_init,
            &explicit,
            expr,
        ) else {
            return None;
        };
        let slot = self.alloc_local(expr.ty.clone());
        let Place::Local(local_id) = slot else {
            unreachable!("alloc_local returns Place::Local");
        };
        let dest = Place::ActorHandle(local_id);
        self.push_instr(Instr::SpawnActor {
            actor_name: actor_name.to_string(),
            state,
            init_args,
            dest,
            max_heap_bytes: layout.max_heap_bytes,
            cycle_capable: layout.cycle_capable,
            mailbox_capacity: layout.mailbox_capacity,
            overflow_policy: layout.overflow_policy.clone(),
        });
        Some(dest)
    }

    fn lower_spawn_actor_state_or_diag(
        &mut self,
        actor_name: &str,
        layout: &ActorLayout,
        explicit_init: bool,
        explicit: &HashMap<&str, &HirExpr>,
        expr: &HirExpr,
    ) -> Result<Option<Place>, ()> {
        let diagnostics_before_state = self.diagnostics.len();
        let Ok(state) =
            self.lower_spawn_actor_state(actor_name, layout, explicit_init, explicit, expr)
        else {
            if self.diagnostics.len() == diagnostics_before_state {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!("spawn `{actor_name}` state lowering failed"),
                        site: expr.site,
                    },
                    note: "actor spawn state lowering failed before a field/value diagnostic was recorded"
                        .to_string(),
                });
            }
            return Err(());
        };
        Ok(state)
    }

    fn lower_spawn_actor_init_args(
        &mut self,
        actor_name: &str,
        layout: &ActorLayout,
        explicit_init: bool,
        explicit: &HashMap<&str, &HirExpr>,
        expr: &HirExpr,
    ) -> Option<Vec<Place>> {
        let mut init_args = Vec::new();
        if !explicit_init {
            return Some(init_args);
        }
        for param_name in &layout.init_param_names {
            let Some(arg) = explicit.get(param_name.as_str()) else {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!(
                            "spawn `{actor_name}` missing init parameter `{param_name}`"
                        ),
                        site: expr.site,
                    },
                    note: "actor spawn with an explicit init block requires every init parameter by name"
                        .to_string(),
                });
                return None;
            };
            init_args.push(self.lower_value(arg)?);
        }
        Some(init_args)
    }

    fn lower_spawn_actor_state(
        &mut self,
        actor_name: &str,
        layout: &ActorLayout,
        explicit_init: bool,
        explicit: &HashMap<&str, &HirExpr>,
        expr: &HirExpr,
    ) -> Result<Option<Place>, ()> {
        if layout.state_field_names.is_empty() {
            return Ok(None);
        }
        let state_ty = ResolvedTy::Named {
            name: actor_name.to_string(),
            args: Vec::new(),
            builtin: None,
            is_opaque: false,
        };
        let dest = self.alloc_local(state_ty.clone());
        let mut fields = Vec::new();
        for (idx, field_name) in layout.state_field_names.iter().enumerate() {
            let src = if let Some(arg) = explicit.get(field_name.as_str()) {
                self.lower_value(arg).ok_or(())?
            } else if let Some(default) = layout
                .state_field_defaults
                .get(idx)
                .and_then(std::option::Option::as_ref)
            {
                self.lower_value(default).ok_or(())?
            } else if explicit_init {
                self.default_actor_state_field_value(
                    actor_name,
                    field_name,
                    &layout.state_field_tys[idx],
                    expr.site,
                )
                .ok_or(())?
            } else {
                self.lower_spawn_actor_state_arg(actor_name, field_name, explicit, expr)
                    .ok_or(())?
            };
            fields.push((
                FieldOffset(
                    u32::try_from(idx)
                        .expect("actor state field count exceeds u32::MAX — impossible"),
                ),
                src,
            ));
        }
        self.push_instr(Instr::RecordInit {
            ty: state_ty,
            fields,
            dest,
        });
        Ok(Some(dest))
    }

    fn lower_spawn_actor_state_arg(
        &mut self,
        actor_name: &str,
        field_name: &str,
        explicit: &HashMap<&str, &HirExpr>,
        expr: &HirExpr,
    ) -> Option<Place> {
        let Some(arg) = explicit.get(field_name) else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::MissingActorSpawnArgument {
                    actor: actor_name.to_string(),
                    field: field_name.to_string(),
                    site: expr.site,
                },
                note: "actor spawn without an init block requires every state field by declaration name"
                    .to_string(),
            });
            return None;
        };
        self.lower_value(arg)
    }

    fn default_actor_state_field_value(
        &mut self,
        actor_name: &str,
        field_name: &str,
        ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        let dest = self.alloc_local(ty.clone());
        match ty {
            ResolvedTy::I8
            | ResolvedTy::I16
            | ResolvedTy::I32
            | ResolvedTy::I64
            | ResolvedTy::U8
            | ResolvedTy::U16
            | ResolvedTy::U32
            | ResolvedTy::U64
            | ResolvedTy::Isize
            | ResolvedTy::Usize
            | ResolvedTy::Bool
            | ResolvedTy::Char
            | ResolvedTy::Duration => {
                self.push_instr(Instr::ConstI64 { dest, value: 0 });
                Some(dest)
            }
            ResolvedTy::F32 => {
                self.push_instr(Instr::FloatLit {
                    dest,
                    value_bits: 0.0f32.to_bits().into(),
                    width: FloatWidth::F32,
                });
                Some(dest)
            }
            ResolvedTy::F64 => {
                self.push_instr(Instr::FloatLit {
                    dest,
                    value_bits: 0.0f64.to_bits(),
                    width: FloatWidth::F64,
                });
                Some(dest)
            }
            ResolvedTy::Unit => {
                self.push_instr(Instr::UnitLit { dest });
                Some(dest)
            }
            other => {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!(
                            "actor init default state value for field `{actor_name}.{field_name}`"
                        ),
                        site,
                    },
                    note: format!(
                        "state field `{field_name}` has type `{other:?}`; spawn-time init currently only zero-initializes scalar state before calling `__init`"
                    ),
                });
                None
            }
        }
    }
}

#[cfg(test)]
mod actor_send_aliasing_collapse {
    //! Revision-2 regression: `actor_send_aliasing` is the one audited
    //! checker-owned `SpanKey` fact consumed in MIR rather than during HIR body
    //! lowering. MIR keys every send lookup at `SpanKey::from` (`module_idx` = 0),
    //! but the checker stamps the map with per-module indices, so a send inside
    //! a non-root (imported / file-import) actor or fn body would miss its fact
    //! and silently fall back to the `Copy` deep-copy path. `collapse_*_to_idx0`
    //! re-keys to idx 0, collision-safe.
    use super::*;
    use hew_types::{ActorSendAliasing, ActorSendCopyReason, SpanKey};

    #[test]
    fn non_root_alias_entry_is_found_at_idx0() {
        let mut map = HashMap::new();
        // Checker recorded an Alias send at byte 10..14 under non-root module 2.
        map.insert(
            SpanKey {
                start: 10,
                end: 14,
                module_idx: 2,
            },
            ActorSendAliasing::Alias,
        );
        let collapsed = collapse_actor_send_aliasing_to_idx0(&map);
        // MIR looks up at module_idx = 0 (SpanKey::from): the entry must resolve.
        let key = SpanKey {
            start: 10,
            end: 14,
            module_idx: 0,
        };
        assert_eq!(
            collapsed.get(&key).copied(),
            Some(ActorSendAliasing::Alias),
            "non-root Alias send must be visible to MIR's idx-0 lookup"
        );
    }

    #[test]
    fn cross_module_byte_range_conflict_falls_back_to_copy() {
        let mut map = HashMap::new();
        // Two different files collide at byte range 4..8 with conflicting modes.
        map.insert(
            SpanKey {
                start: 4,
                end: 8,
                module_idx: 1,
            },
            ActorSendAliasing::Alias,
        );
        map.insert(
            SpanKey {
                start: 4,
                end: 8,
                module_idx: 2,
            },
            ActorSendAliasing::Copy(ActorSendCopyReason::CopyType),
        );
        let collapsed = collapse_actor_send_aliasing_to_idx0(&map);
        // Conflict → omit → idx-0 lookup misses → MIR defaults to safe Copy.
        let key = SpanKey {
            start: 4,
            end: 8,
            module_idx: 0,
        };
        assert!(
            !collapsed.contains_key(&key),
            "a cross-file conflict at one byte range must collapse to the Copy default"
        );
    }

    #[test]
    fn root_alias_entry_is_preserved() {
        let mut map = HashMap::new();
        map.insert(
            SpanKey {
                start: 2,
                end: 5,
                module_idx: 0,
            },
            ActorSendAliasing::Alias,
        );
        let collapsed = collapse_actor_send_aliasing_to_idx0(&map);
        assert_eq!(
            collapsed
                .get(&SpanKey {
                    start: 2,
                    end: 5,
                    module_idx: 0,
                })
                .copied(),
            Some(ActorSendAliasing::Alias),
            "root-level Alias sends must be unaffected by the collapse"
        );
    }
}
