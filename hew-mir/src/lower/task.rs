use super::{
    bracket_actor_handler_blocks, check_function, check_to_diagnostic,
    collect_unknown_type_diagnostics, dataflow, elaborate, finalize_bytes_ownership,
    finalize_string_ownership, BasicBlock, BindingId, Builder, CheckedMirFunction, FieldOffset,
    HirBlock, HirExpr, HirExprKind, HirFn, HirJoin, HirSelect, HirSelectArmKind, HirStmtKind,
    Instr, IntentKind, JoinBranch, LoweredFunction, MirDiagnostic, MirDiagnosticKind, MirStatement,
    Place, RawMirFunction, ResolvedTy, SelectArm, SelectArmKind, SourceOrigin,
    SpawnEnvFieldOwnership, SuspendKind, Terminator, ThirFunction, ValueClass,
};

impl Builder {
    /// Lower a recognised `hew_*` runtime-ABI call to
    /// `Instr::CallRuntimeAbi`.
    ///
    /// Called from `lower_value`'s `HirExprKind::Call` arm when the
    /// callee is a `BindingRef` whose name passes
    /// `runtime_symbols::is_known_runtime_symbol`.  The HIR args have
    /// already been validated by the HIR pipeline; this method lower
    /// each arg via `lower_value`, then emits the appropriate
    /// instruction sequence.
    ///
    /// # `hew_duplex_pair` encoding
    ///
    /// The runtime C-ABI takes `(s_cap, r_cap, *mut *mut HewDuplexHandle,
    /// *mut *mut HewDuplexHandle)`.  The user surface is `duplex_pair(N)`
    /// with one symmetric capacity arg.  E2 duplicates `args[0]` into
    /// both cap slots and passes two fresh `Place::DuplexHandle(N0/N1)`
    /// in the out-param positions (`args[2..=3]`).  Codegen (E4) takes
    /// the address of each `DuplexHandle` local and passes it as the
    /// actual pointer.  A "tuple proxy" `Place::Local(M)` is returned
    /// so that subsequent `TupleIndex` projections can recover the
    /// individual `DuplexHandle` Places via `self.tuple_decomp`.
    ///
    /// # `hew_duplex_send` encoding
    ///
    /// The runtime C-ABI takes `(*mut HewDuplexHandle, *const u8, usize)`.
    /// For an integer payload `42`, this method emits a prefatory
    /// `Instr::ConstI64 { value: 8 }` as the byte-length constant
    /// before the `CallRuntimeAbi`.  The message value Place is passed
    /// as-is; codegen (E4) stores it to a stack alloca and passes its
    /// address as the `*const u8`.
    ///
    /// # SHIM(E4) convention
    ///
    // SHIM(E4): codegen interprets Place::DuplexHandle(N) per-symbol convention:
    //   hew_duplex_send/recv/close: load the raw ptr from local-N's alloca, pass as *mut HewDuplexHandle.
    //   hew_duplex_pair out-params (args[2], args[3]): take address of local-N's alloca, pass as *mut *mut HewDuplexHandle.
    // The message-value Place::Local(N) in args[1] of hew_duplex_send is store-to-alloca + address-cast by E4.
    // The length Place::Local(N) in args[2] of hew_duplex_send carries the ConstI64(8) emitted above.
    // WHY: MIR names semantics; address materialisation is a codegen-target concern.
    // WHEN obsolete: when E4's lower_instr arm is wired and tested for each of these conventions.
    // WHAT: replace with direct LLVMBuildCall emission for each symbol group.
    fn task_scope_ty() -> ResolvedTy {
        ResolvedTy::Named {
            name: "HewTaskScope".to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        }
    }

    pub(crate) fn lower_task_scope(&mut self, body: &HirBlock) -> Place {
        let scope_place = self.alloc_local(Self::task_scope_ty());
        self.push_runtime_call("hew_task_scope_new", vec![], Some(scope_place));

        let previous_scope_place = self.alloc_local(Self::task_scope_ty());
        self.push_runtime_call(
            "hew_task_scope_set_current",
            vec![scope_place],
            Some(previous_scope_place),
        );

        let saved_scope = self.current_task_scope.replace(scope_place);
        self.active_scopes.push(body.scope);
        for stmt in &body.statements {
            self.stmt(stmt);
        }
        if let Some(tail) = &body.tail {
            let _ = self.lower_value(tail);
        }
        self.emit_pending_defers(body.scope);
        self.active_scopes.pop();
        self.current_task_scope = saved_scope;

        self.push_runtime_call("hew_task_scope_join_all", vec![scope_place], None);
        self.push_runtime_call("hew_task_scope_destroy", vec![scope_place], None);
        let restored_scope_place = self.alloc_local(Self::task_scope_ty());
        self.push_runtime_call(
            "hew_task_scope_set_current",
            vec![previous_scope_place],
            Some(restored_scope_place),
        );

        let unit_place = self.alloc_local(ResolvedTy::Unit);
        self.push_instr(Instr::UnitLit { dest: unit_place });
        unit_place
    }

    fn direct_no_arg_unit_callee(
        &mut self,
        callee: &HirExpr,
        args: &[HirExpr],
        ret_ty: &ResolvedTy,
        site: hew_hir::SiteId,
        construct: &str,
    ) -> Option<String> {
        // For a generic call site, resolve the monomorphized callee symbol by
        // mangling the origin name with the per-site concrete type arguments
        // that HIR recorded. This mirrors the direct-call resolution in the
        // lower_value Call arm (which dispatches to the mangled symbol for
        // generic top-level user functions).
        if let Some(type_args) = self.call_site_type_args.get(&site).cloned() {
            if !type_args.is_empty() {
                let HirExprKind::BindingRef { name, .. } = &callee.kind else {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::NotYetImplemented {
                            construct: construct.to_string(),
                            site,
                        },
                        note: "generic task spawn requires a direct function binding \
                               as callee"
                            .to_string(),
                    });
                    return None;
                };
                let substituted: Vec<ResolvedTy> =
                    type_args.iter().map(|t| self.subst_ty(t)).collect();
                let mangled = hew_hir::monomorph::mangle(name, &substituted);
                if !self.module_fn_names.contains(&mangled) {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::NotYetImplemented {
                            construct: construct.to_string(),
                            site,
                        },
                        note: format!(
                            "generic spawn of `{name}` has no registered \
                             monomorphization `{mangled}`; HIR must emit this \
                             instantiation before task spawning can proceed"
                        ),
                    });
                    return None;
                }
                // Apply the same no-arg/unit-return gate as the concrete path.
                if !args.is_empty() || !matches!(ret_ty, ResolvedTy::Unit) {
                    for arg in args {
                        let _ = self.lower_value(arg);
                    }
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::NotYetImplemented {
                            construct: construct.to_string(),
                            site,
                        },
                        note: "cancellation-token task lowering currently supports only \
                               no-argument functions returning unit; value/result task \
                               propagation remains fail-closed"
                            .to_string(),
                    });
                    return None;
                }
                return Some(mangled);
            }
        }
        // Safety guard: a generic function without recorded type arguments
        // has no resolvable monomorphization and cannot be spawned.
        if matches!(
            &callee.kind,
            HirExprKind::BindingRef { name, .. } if self.module_generic_fn_names.contains(name)
        ) {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: construct.to_string(),
                    site,
                },
                note: "generic function spawned without a resolved concrete \
                       instantiation; provide explicit type arguments"
                    .to_string(),
            });
            return None;
        }
        // A no-argument callee returning UNIT is supported via this function.
        // Value-returning (`T != ()`) no-arg spawns are allowed only through
        // the `fork t = callee()` bound form — the task handle is then awaited
        // to retrieve `T`. An UNBOUND implicit spawn of a non-unit callee
        // (bare `callee()` inside `scope { }` with `T != ()`) is rejected here:
        // the result would be silently discarded and the value channel unused,
        // which is a miscompile not a runtime leak.
        //
        // Arg-bearing spawns route through `lower_spawned_args_call_task` before
        // this point; value+args spawns remain fail-closed there.
        if !args.is_empty() || !matches!(ret_ty, ResolvedTy::Unit) {
            for arg in args {
                let _ = self.lower_value(arg);
            }
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: construct.to_string(),
                    site,
                },
                note: "cancellation-token task lowering currently supports only \
                       no-argument functions returning unit; value/result task \
                       propagation remains fail-closed"
                    .to_string(),
            });
            return None;
        }
        match &callee.kind {
            HirExprKind::BindingRef { name, .. } if self.module_fn_names.contains(name) => {
                Some(name.clone())
            }
            _ => {
                let _ = self.lower_value(callee);
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: construct.to_string(),
                        site,
                    },
                    note: "fork cancellation lowering requires a direct module function callee"
                        .to_string(),
                });
                None
            }
        }
    }

    fn mir_sanitize_symbol(symbol: &str) -> String {
        symbol
            .chars()
            .map(|ch| if ch.is_ascii_alphanumeric() { ch } else { '_' })
            .collect()
    }

    fn task_entry_adapter_symbol(callee_symbol: &str) -> String {
        format!(
            "__hew_task_entry_{}",
            Self::mir_sanitize_symbol(callee_symbol)
        )
    }

    /// Resolve (and, on first use, allocate) the task-entry adapter symbol
    /// for `callee_symbol`. Stable across repeated calls with the same
    /// `callee_symbol` (returns the previously allocated adapter symbol
    /// without regenerating the adapter body). If the naive sanitized name
    /// is already owned by a *different* original callee symbol, appends a
    /// numeric disambiguation suffix so every distinct callee still gets its
    /// own, uniquely named adapter.
    fn ensure_task_entry_adapter(&mut self, callee_symbol: &str, result_ty: &ResolvedTy) -> String {
        if let Some(existing) = self.task_entry_adapter_symbols.borrow().get(callee_symbol) {
            return existing.clone();
        }
        let base_symbol = Self::task_entry_adapter_symbol(callee_symbol);
        let already_used = self
            .task_entry_adapter_symbols
            .borrow()
            .values()
            .any(|used| used == &base_symbol);
        let adapter_symbol = if already_used {
            let mut candidate;
            let mut suffix = 2usize;
            loop {
                candidate = format!("{base_symbol}__dup{suffix}");
                let taken = self
                    .task_entry_adapter_symbols
                    .borrow()
                    .values()
                    .any(|used| used == &candidate);
                if !taken {
                    break candidate;
                }
                suffix += 1;
            }
        } else {
            base_symbol
        };
        self.task_entry_adapter_symbols
            .borrow_mut()
            .insert(callee_symbol.to_string(), adapter_symbol.clone());
        let lowered = self.synthesize_task_entry_adapter(callee_symbol, &adapter_symbol, result_ty);
        self.generated_functions.push(lowered);
        adapter_symbol
    }

    /// Synthesize the per-callee task-entry adapter (`__hew_task_entry_<fn>`,
    /// `TaskEntry` call-conv). The adapter is the body the codegen task wrapper
    /// invokes on the spawned worker; the wrapper publishes the adapter's return
    /// value through `hew_task_set_result` and then `hew_task_complete_threaded`.
    ///
    /// For a value-returning task (`result_ty != ()`) the adapter calls the body
    /// into a typed `dest` local and RETURNS it, so the wrapper captures the
    /// child's `T` and writes it into the task result buffer. For a unit task
    /// the adapter calls the body with `dest: None` and returns unit — the
    /// wrapper publishes nothing.
    fn synthesize_task_entry_adapter(
        &self,
        callee_symbol: &str,
        adapter_symbol: &str,
        result_ty: &ResolvedTy,
    ) -> LoweredFunction {
        let is_value_task = !matches!(result_ty, ResolvedTy::Unit);
        // Value task: the body call writes its `T` directly into the function
        // return slot so `Terminator::Return` hands it back; the codegen wrapper
        // then publishes that return value through `hew_task_set_result`. Unit
        // task: discard the body result (`dest: None`).
        let call_dest = if is_value_task {
            Some(Place::ReturnSlot)
        } else {
            None
        };
        let mut blocks = vec![
            BasicBlock {
                id: 0,
                statements: vec![],
                instructions: vec![],
                terminator: Terminator::Call {
                    callee: callee_symbol.to_string(),
                    builtin: None,
                    args: vec![],
                    dest: call_dest,
                    next: 1,
                },
            },
            BasicBlock {
                id: 1,
                statements: vec![],
                instructions: vec![],
                terminator: Terminator::Return,
            },
        ];
        bracket_actor_handler_blocks(&mut blocks);

        let adapter_return_ty = result_ty.clone();
        let thir = ThirFunction {
            name: adapter_symbol.to_string(),
            return_ty: adapter_return_ty.clone(),
            statements: vec![],
        };
        let mut raw = RawMirFunction {
            name: adapter_symbol.to_string(),
            return_ty: adapter_return_ty.clone(),
            call_conv: crate::model::FunctionCallConv::TaskEntry,
            params: vec![],
            locals: vec![],
            // Synthesised task-entry adapter: no user bindings, no `-g` DIEs.
            local_names: Vec::new(),
            local_scopes: Vec::new(),
            local_decl_bytes: Vec::new(),
            scope_table: Vec::new(),
            blocks,
            decisions: vec![],
            intrinsic_id: None,
            await_deadline_ns: std::collections::HashMap::new(),
            suspend_kinds: std::collections::HashMap::new(),
            lambda_actor_user_param_locals: Vec::new(),
            span: None,
            instr_spans: ::std::collections::BTreeMap::new(),
            source_origin: SourceOrigin::Unknown,
        };
        let builder = Builder {
            current_function_symbol: adapter_symbol.to_string(),
            current_function_call_conv: crate::model::FunctionCallConv::TaskEntry,
            ..self.child_builder_tables()
        };
        let mut dataflow_result = dataflow::analyze(&raw.blocks, &builder.type_classes, &[]);
        dataflow_result
            .checks
            .extend(crate::model::validate_context_markers(&raw));
        let diagnostics: Vec<MirDiagnostic> = dataflow_result
            .checks
            .iter()
            .filter_map(check_to_diagnostic)
            .collect();
        let string_derivation = finalize_string_ownership(&mut raw, &builder, &dataflow_result);
        let bytes_derivation = finalize_bytes_ownership(&mut raw, &builder, &dataflow_result);
        let cooperate_sites = dataflow::compute_cooperate_sites(&raw.blocks);
        let checked = CheckedMirFunction {
            name: adapter_symbol.to_string(),
            return_ty: adapter_return_ty,
            blocks: raw.blocks.clone(),
            decisions: vec![],
            checks: dataflow_result.checks.clone(),
            cooperate_sites,
        };
        let elaborated = elaborate(
            &checked,
            &builder,
            &[],
            &dataflow_result,
            Some(&string_derivation.allowed),
            Some(&bytes_derivation.allowed),
        );
        LoweredFunction {
            thir,
            raw,
            checked,
            elaborated,
            diagnostics,
            generated: vec![],
            record_layouts: vec![],
        }
    }

    pub(crate) fn lower_spawned_call_task(
        &mut self,
        callee: &HirExpr,
        args: &[HirExpr],
        task_ty: &ResolvedTy,
        // `true` for a bound `fork t = callee()` spawn (value-task allowed);
        // `false` for an implicit/unbound spawn (non-unit callee must reject).
        bound: bool,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        let Some(scope_place) = self.current_task_scope else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "spawned call without current task scope".to_string(),
                    site,
                },
                note: "task spawn reached MIR without a scope-owned cancellation token; \
                       refusing to emit an unobservable cancellation edge"
                    .to_string(),
            });
            return None;
        };
        let ResolvedTy::Task(inner) = task_ty else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "spawned call with non-Task type".to_string(),
                    site,
                },
                note: "HIR SpawnedCall must carry Task<T>".to_string(),
            });
            return None;
        };
        if !self.current_function_call_conv.carries_execution_context() {
            let callee_name = match &callee.kind {
                HirExprKind::BindingRef { name, .. } => name.as_str(),
                HirExprKind::Closure { .. } => "<closure>",
                _ => "<callee>",
            };
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!(
                        "cannot spawn `{callee_name}` from `{}`",
                        self.current_function_symbol
                    ),
                    site,
                },
                note: format!(
                    "`scope {{ fork ... }}` requires an enclosing ctx-bearing execution context, \
                     but `{}` has Default call-conv (no execution-context parameter). Move this \
                     spawn into an actor handler body or an actor-internal helper. Caller-side \
                     ctx-routing for top-level `fn main` is tracked under \
                     W4.010-followup-caller-ctx-routing.",
                    self.current_function_symbol
                ),
            });
            return None;
        }
        if matches!(callee.kind, HirExprKind::Closure { .. }) {
            return self.lower_spawned_closure_task(
                callee,
                args,
                task_ty,
                inner,
                scope_place,
                site,
            );
        }
        if !args.is_empty() {
            return self.lower_spawned_args_call_task(
                callee,
                args,
                task_ty,
                inner,
                scope_place,
                site,
            );
        }
        // Bound value-returning task (`fork t = callee()` where callee returns T ≠ ()):
        // route through the value-callee path — no unit-return restriction, the task handle
        // is awaited to retrieve `T`. Unbound implicit spawns (`callee()` as a bare scope
        // statement, `bound = false`) fall through to `direct_no_arg_unit_callee` where the
        // unit-return gate rejects the non-unit callee fail-closed.
        if bound && !matches!(&**inner, ResolvedTy::Unit) {
            return self.lower_no_arg_value_callee_task(callee, inner, task_ty, scope_place, site);
        }
        let user_callee_symbol =
            self.direct_no_arg_unit_callee(callee, args, inner, site, "spawned call")?;
        let callee_symbol = self.ensure_task_entry_adapter(&user_callee_symbol, inner);

        let task_place = self.alloc_local(task_ty.clone());
        self.push_runtime_call("hew_task_new", vec![], Some(task_place));
        self.push_runtime_call("hew_task_scope_spawn", vec![scope_place, task_place], None);
        self.push_instr(Instr::SpawnTaskDirect {
            task: task_place,
            callee_symbol,
        });
        Some(task_place)
    }

    /// Lower a no-argument value-returning task spawn: `fork t = callee()` where
    /// `callee` returns `T ≠ ()`. The task-entry adapter is synthesized with
    /// `result_ty = T` so the child's `T` is published via `hew_task_set_result`
    /// and the awaiting handler reads it on the resume edge.
    ///
    /// This path does NOT enforce a unit-return restriction (unlike
    /// `direct_no_arg_unit_callee`). All other gates (non-module-fn callee,
    /// arg-bearing callee) remain fail-closed. Generic callees are resolved to
    /// their monomorphized symbol via HIR's per-site type arguments.
    fn lower_no_arg_value_callee_task(
        &mut self,
        callee: &HirExpr,
        inner: &ResolvedTy,
        task_ty: &ResolvedTy,
        scope_place: Place,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        // Require a direct BindingRef callee.
        let HirExprKind::BindingRef { name, .. } = &callee.kind else {
            let _ = self.lower_value(callee);
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "value-task spawn".to_string(),
                    site,
                },
                note: "value-task spawn requires a direct module function callee".to_string(),
            });
            return None;
        };
        // For a generic call site, resolve the monomorphized callee symbol by
        // mangling the origin name with the per-site concrete type arguments.
        let user_callee_symbol =
            if let Some(type_args) = self.call_site_type_args.get(&site).cloned() {
                if type_args.is_empty() {
                    name.clone()
                } else {
                    let substituted: Vec<ResolvedTy> =
                        type_args.iter().map(|t| self.subst_ty(t)).collect();
                    let mangled = hew_hir::monomorph::mangle(name, &substituted);
                    if !self.module_fn_names.contains(&mangled) {
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct: "value-task spawn".to_string(),
                                site,
                            },
                            note: format!(
                                "generic spawn of `{name}` has no registered \
                                 monomorphization `{mangled}`; HIR must emit this \
                                 instantiation before task spawning can proceed"
                            ),
                        });
                        return None;
                    }
                    mangled
                }
            } else {
                name.clone()
            };
        // Safety guard: a generic function without recorded type arguments
        // has no resolvable monomorphization and cannot be spawned.
        if user_callee_symbol == *name && self.module_generic_fn_names.contains(name) {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "value-task spawn".to_string(),
                    site,
                },
                note: "generic function spawned without a resolved concrete \
                       instantiation; provide explicit type arguments"
                    .to_string(),
            });
            return None;
        }
        if !self.module_fn_names.contains(&user_callee_symbol) {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "value-task spawn".to_string(),
                    site,
                },
                note: format!(
                    "value-task spawn callee `{user_callee_symbol}` is not a registered \
                     module function"
                ),
            });
            return None;
        }
        let callee_symbol = self.ensure_task_entry_adapter(&user_callee_symbol, inner);
        let task_place = self.alloc_local(task_ty.clone());
        self.push_runtime_call("hew_task_new", vec![], Some(task_place));
        self.push_runtime_call("hew_task_scope_spawn", vec![scope_place, task_place], None);
        self.push_instr(Instr::SpawnTaskDirect {
            task: task_place,
            callee_symbol,
        });
        Some(task_place)
    }

    /// Lower a spawned call with arguments: `fork t = worker(a, b);` or the
    /// fork-block / implicit-spawn forms carrying args. The args are lowered
    /// in the parent (consuming owned bindings exactly as a direct call
    /// would), packed into a fork-env record, and the spawn dispatches via
    /// `SpawnTaskClosure` to a synthesized fork-entry shim that loads the
    /// env fields back out and calls the target.
    ///
    /// Ownership contract (drop-allowset rules, verified against the emitted
    /// drop plans): the parent emits NO drop for moved-in args — the consume
    /// facts from arg lowering remove them from the parent's plan; the env
    /// rc-box frees bytes only (codegen passes a null drop fn); the shim
    /// emits no drops for the env-loaded temps. This is byte-for-byte the
    /// same plan shape as the direct-call baseline (`shout(greeting)`):
    /// under the current M-COW move-only spine the by-value string param is
    /// read `CowShare` by the callee and released by no one, so the fork form
    /// leaks exactly where the direct call already leaks — never a
    /// double-free. WHEN-OBSOLETE: M-COW retain-on-share; the env transfer
    /// then needs a real retain/release pair in lockstep with call args.
    ///
    /// First slice restricts arg types to `BitCopy` scalars + `string`;
    /// anything else refuses with a diagnostic (never a miscompile). The
    /// callee must return unit — value-bearing forks are gated at the
    /// await site and remain fail-closed until result propagation lands.
    #[expect(
        clippy::too_many_lines,
        reason = "single fail-closed boundary sequence — generic/callee/return/arg-class \
                  gates, then env pack + shim + spawn; splitting would scatter the refusal \
                  conditions away from the spawn they guard"
    )]
    fn lower_spawned_args_call_task(
        &mut self,
        callee: &HirExpr,
        args: &[HirExpr],
        task_ty: &ResolvedTy,
        inner: &ResolvedTy,
        scope_place: Place,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        let HirExprKind::BindingRef { name, .. } = &callee.kind else {
            let _ = self.lower_value(callee);
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "spawned call".to_string(),
                    site,
                },
                note: "arg-bearing task spawn requires a direct module function callee".to_string(),
            });
            return None;
        };
        // For a generic call site, resolve the monomorphized callee symbol by
        // mangling the origin name with the per-site concrete type arguments.
        let callee_sym: String =
            if let Some(type_args) = self.call_site_type_args.get(&site).cloned() {
                if type_args.is_empty() {
                    name.clone()
                } else {
                    let substituted: Vec<ResolvedTy> =
                        type_args.iter().map(|t| self.subst_ty(t)).collect();
                    let mangled = hew_hir::monomorph::mangle(name, &substituted);
                    if !self.module_fn_names.contains(&mangled) {
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct: "spawned call".to_string(),
                                site,
                            },
                            note: format!(
                                "generic spawn of `{name}` has no registered \
                                 monomorphization `{mangled}`; HIR must emit this \
                                 instantiation before task spawning can proceed"
                            ),
                        });
                        return None;
                    }
                    mangled
                }
            } else {
                name.clone()
            };
        // Safety guard: a generic function without recorded type arguments
        // has no resolvable monomorphization and cannot be spawned.
        if callee_sym == *name && self.module_generic_fn_names.contains(name) {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "spawned call".to_string(),
                    site,
                },
                note: "generic function spawned without a resolved concrete \
                       instantiation; provide explicit type arguments"
                    .to_string(),
            });
            return None;
        }
        if !self.module_fn_names.contains(&callee_sym) {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "spawned call".to_string(),
                    site,
                },
                note: format!(
                    "arg-bearing task spawn callee `{callee_sym}` is not a registered module function"
                ),
            });
            return None;
        }
        if !matches!(inner, ResolvedTy::Unit) {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "spawned call".to_string(),
                    site,
                },
                note: "arg-bearing task spawn currently requires a unit-returning callee; \
                       value/result task propagation remains fail-closed"
                    .to_string(),
            });
            return None;
        }
        // Per-arg type restriction (this slice): BitCopy scalars + string.
        // Anything with non-trivial drop glue or interior ownership refuses
        // here — transferring it through the byte-copied env without a
        // retain/drop story would miscompile, and we fail closed instead.
        let mut arg_tys = Vec::with_capacity(args.len());
        for arg in args {
            let arg_ty = self.subst_ty(&arg.ty);
            let class = ValueClass::of_ty(&arg_ty, &self.type_classes);
            let allowed = class == ValueClass::BitCopy || matches!(arg_ty, ResolvedTy::String);
            if !allowed {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "spawned call argument".to_string(),
                        site,
                    },
                    note: format!(
                        "task spawn argument of type `{}` is not yet supported; \
                         this slice transfers BitCopy scalars and `string` only — \
                         richer owned types need an env retain/drop plan first",
                        arg_ty.user_facing()
                    ),
                });
                return None;
            }
            arg_tys.push(arg_ty);
        }
        // Lower args left-to-right. Owned bindings (e.g. a string) record
        // their consume fact here, which removes them from the parent's
        // drop plan — ownership rides the env bytes into the child.
        let mut arg_places = Vec::with_capacity(args.len());
        for arg in args {
            arg_places.push(self.lower_value(arg)?);
        }

        // Pack the lowered args into a fork-env record (RecordInit memcpys
        // the fields; the layout registers alongside closure env records).
        let fork_id = self.next_closure_id;
        self.next_closure_id = self
            .next_closure_id
            .checked_add(1)
            .expect("closure id overflow");
        let owner = Self::sanitize_symbol_component(&self.current_function_symbol);
        let env_name = format!("__hew_fork_env_{owner}_{fork_id}");
        let shim_name = format!("__hew_fork_entry_{owner}_{fork_id}");
        let env_ty = ResolvedTy::Named {
            name: env_name.clone(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        };
        self.closure_record_layouts
            .push(crate::model::RecordLayout {
                name: env_name,
                field_tys: arg_tys.clone(),
                // Compiler-internal fork-env record: positional `-g` names.
                field_names: Vec::new(),
            });
        let field_pairs: Vec<(FieldOffset, Place)> = arg_places
            .iter()
            .enumerate()
            .map(|(idx, place)| {
                (
                    FieldOffset(u32::try_from(idx).expect("fork arg count exceeds u32::MAX")),
                    *place,
                )
            })
            .collect();
        let env_place = self.alloc_local(env_ty.clone());
        self.push_instr(Instr::RecordInit {
            ty: env_ty.clone(),
            fields: field_pairs,
            dest: env_place,
        });

        let lowered = self.synthesize_fork_entry_shim(&callee_sym, &shim_name, &arg_tys, &env_ty);
        self.generated_functions.push(lowered);

        let task_place = self.alloc_local(task_ty.clone());
        self.push_runtime_call("hew_task_new", vec![], Some(task_place));
        self.push_runtime_call("hew_task_scope_spawn", vec![scope_place, task_place], None);
        self.push_instr(Instr::SpawnTaskClosure {
            task: task_place,
            fn_symbol: shim_name,
            env: env_place,
            env_ty,
            env_ownership: arg_tys
                .iter()
                .map(|ty| {
                    if ValueClass::of_ty(ty, &self.type_classes) == ValueClass::BitCopy {
                        SpawnEnvFieldOwnership::BorrowsOnly
                    } else {
                        SpawnEnvFieldOwnership::OwnsMoved
                    }
                })
                .collect(),
        });
        Some(task_place)
    }

    /// Synthesize the fork-entry shim for an arg-bearing task spawn: a
    /// `ClosureInvoke`-ABI function `(ctx, env_ptr)` that loads each arg
    /// back out of the env record and calls the target function with them.
    ///
    /// Loading an owned string capture retains it so the environment remains
    /// valid while the call runs. The shim balances that temporary retain after
    /// the callee returns; this does not release the moved environment owner.
    #[expect(
        clippy::too_many_lines,
        reason = "shim construction keeps raw/checked/elaborated MIR snapshots aligned, \
                  mirroring lower_closure_shim / lower_named_fn_invoke_shim"
    )]
    fn synthesize_fork_entry_shim(
        &self,
        callee_symbol: &str,
        shim_name: &str,
        arg_tys: &[ResolvedTy],
        env_ty: &ResolvedTy,
    ) -> LoweredFunction {
        let env_ptr_ty = Self::closure_env_pointer_ty(env_ty);
        let mut builder = Builder {
            type_classes: self.type_classes.clone(),
            record_field_orders: self.record_field_orders.clone(),
            actor_layouts: self.actor_layouts.clone(),
            supervisor_layout_map: self.supervisor_layout_map.clone(),
            machine_layout_names: self.machine_layout_names.clone(),
            module_fn_names: self.module_fn_names.clone(),
            module_generic_fn_names: self.module_generic_fn_names.clone(),
            funcupdate_fn_returns_fresh: self.funcupdate_fn_returns_fresh.clone(),
            param_ownership: self.param_ownership.clone(),
            subst: self.subst.clone(),
            call_site_type_args: self.call_site_type_args.clone(),
            supervisor_child_slots: self.supervisor_child_slots.clone(),
            pointer_width: self.pointer_width,
            current_function_symbol: shim_name.to_string(),
            current_function_call_conv: crate::model::FunctionCallConv::ClosureInvoke,
            task_entry_adapter_symbols: self.task_entry_adapter_symbols.clone(),
            // #2648 — synthetic call wrapper (no user match scrutinees), but the
            // provenance context is threaded uniformly: no child builder falls
            // back to the legacy fail-open default.
            call_scrutinee_provenance: self.call_scrutinee_provenance.clone(),
            ..Builder::default()
        };

        // locals[0] = env_ptr (ClosureInvoke ABI). Each arg is loaded out of
        // the env record into a fresh temp and forwarded to the callee.
        let env_place = builder.alloc_local(env_ptr_ty.clone());
        let mut arg_places = Vec::with_capacity(arg_tys.len());
        for (idx, ty) in arg_tys.iter().enumerate() {
            let dest = builder.alloc_local(ty.clone());
            builder.instructions.push(Instr::ClosureEnvFieldLoad {
                env: env_place,
                env_ty: env_ty.clone(),
                field_offset: FieldOffset(
                    u32::try_from(idx).expect("fork arg count exceeds u32::MAX"),
                ),
                dest,
            });
            arg_places.push(dest);
        }

        let ret_block_id = builder.alloc_block();
        builder.finish_current_block(Terminator::Call {
            callee: callee_symbol.to_string(),
            builtin: None,
            args: arg_places.clone(),
            dest: None,
            next: ret_block_id,
        });
        builder.start_block(ret_block_id);
        for (place, ty) in arg_places.iter().zip(arg_tys) {
            if matches!(ty, ResolvedTy::String) {
                builder.instructions.push(Instr::Drop {
                    place: *place,
                    ty: ty.clone(),
                    drop_fn: Some(crate::model::DropFnSpec::Release("hew_string_drop")),
                });
            }
        }
        let blocks = builder.finalize_blocks(Terminator::Return);

        let thir_statements: Vec<MirStatement> = blocks
            .iter()
            .flat_map(|b| b.statements.iter().cloned())
            .collect();
        let thir = ThirFunction {
            name: shim_name.to_string(),
            return_ty: ResolvedTy::Unit,
            statements: thir_statements,
        };
        let mut raw = RawMirFunction {
            name: shim_name.to_string(),
            return_ty: ResolvedTy::Unit,
            call_conv: crate::model::FunctionCallConv::ClosureInvoke,
            params: vec![env_ptr_ty],
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
        // locals are positional, not HIR bindings).
        let synthetic_func = HirFn {
            id: hew_hir::ItemId(0),
            node: hew_hir::HirNodeId(0),
            name: shim_name.to_string(),
            type_params: Vec::new(),
            is_generator: false,
            intrinsic_id: None,
            params: Vec::new(),
            return_ty: ResolvedTy::Unit,
            body: hew_hir::HirBlock {
                node: hew_hir::HirNodeId(0),
                scope: hew_hir::ScopeId(0),
                statements: Vec::new(),
                tail: None,
                ty: ResolvedTy::Unit,
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
        let string_derivation = finalize_string_ownership(&mut raw, &builder, &dataflow_result);
        let bytes_derivation = finalize_bytes_ownership(&mut raw, &builder, &dataflow_result);
        let cooperate_sites = dataflow::compute_cooperate_sites(&raw.blocks);
        let checked = CheckedMirFunction {
            name: shim_name.to_string(),
            return_ty: ResolvedTy::Unit,
            blocks: raw.blocks.clone(),
            decisions: builder.decisions.clone(),
            checks: dataflow_result.checks.clone(),
            cooperate_sites,
        };
        let elaborated = elaborate(
            &checked,
            &builder,
            &thir.statements,
            &dataflow_result,
            Some(&string_derivation.allowed),
            Some(&bytes_derivation.allowed),
        );

        LoweredFunction {
            thir,
            raw,
            checked,
            elaborated,
            diagnostics,
            generated: builder.generated_functions,
            record_layouts: builder.closure_record_layouts,
        }
    }

    fn lower_spawned_closure_task(
        &mut self,
        callee: &HirExpr,
        args: &[HirExpr],
        task_ty: &ResolvedTy,
        inner: &ResolvedTy,
        scope_place: Place,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        let HirExprKind::Closure {
            params,
            ret_ty,
            body,
            captures,
            escape_kind: _,
        } = &callee.kind
        else {
            unreachable!("caller checked closure callee");
        };
        if !args.is_empty()
            || !params.is_empty()
            || !matches!(inner, ResolvedTy::Unit)
            || !matches!(ret_ty, ResolvedTy::Unit)
        {
            for arg in args {
                let _ = self.lower_value(arg);
            }
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "spawned closure".to_string(),
                    site,
                },
                note: "spawned-closure task lowering supports only zero-argument closures \
                       returning unit; value/result task propagation remains fail-closed"
                    .to_string(),
            });
            return None;
        }
        if let Some(capture) = captures.iter().find(|capture| !capture.is_send) {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "spawned closure non-Send capture".to_string(),
                    site,
                },
                note: format!(
                    "closure capture `{}` is not Send; refusing to transfer its environment \
                     across the spawned task boundary",
                    capture.name
                ),
            });
            return None;
        }
        let (fn_symbol, env_ty, env_place, _suspends) = self.materialize_closure_env(
            callee,
            params,
            ret_ty,
            body,
            captures,
            crate::closure_env::AllocationStrategy::ScopeOwned,
        )?;
        let task_place = self.alloc_local(task_ty.clone());
        self.push_runtime_call("hew_task_new", vec![], Some(task_place));
        self.push_runtime_call("hew_task_scope_spawn", vec![scope_place, task_place], None);
        self.push_instr(Instr::SpawnTaskClosure {
            task: task_place,
            fn_symbol,
            env: env_place,
            env_ty,
            env_ownership: vec![SpawnEnvFieldOwnership::BorrowsOnly; captures.len()],
        });
        Some(task_place)
    }

    pub(crate) fn lower_fork_block_task(
        &mut self,
        body: &HirBlock,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        let expr = if body.statements.len() == 1 && body.tail.is_none() {
            let HirStmtKind::Expr(expr) = &body.statements[0].kind else {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "fork block cancellation child".to_string(),
                        site,
                    },
                    note: "fork block task lowering currently supports expression-call statements only"
                        .to_string(),
                });
                return None;
            };
            expr
        } else if body.statements.is_empty() {
            let Some(tail) = &body.tail else {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "fork block cancellation child".to_string(),
                        site,
                    },
                    note: "fork block task lowering requires a no-argument unit function call"
                        .to_string(),
                });
                return None;
            };
            tail
        } else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "fork block cancellation child".to_string(),
                    site,
                },
                note: "fork block task lowering currently supports exactly one \
                       statement: a no-argument unit function call"
                    .to_string(),
            });
            return None;
        };
        let HirExprKind::Call { callee, args } = &expr.kind else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "fork block cancellation child".to_string(),
                    site,
                },
                note: "fork block task lowering currently supports a direct function call body"
                    .to_string(),
            });
            return None;
        };
        let task_ty = ResolvedTy::Task(Box::new(ResolvedTy::Unit));
        // Use the inner call's site (expr.site) so that generic call-site type
        // arguments recorded by HIR for the inner call are visible to the
        // spawn lowering path. The outer ForkBlock site differs from the inner
        // Call site, and call_site_type_args is keyed on the inner site.
        self.lower_spawned_call_task(callee, args, &task_ty, false, expr.site)
    }

    pub(crate) fn lower_scope_deadline(
        &mut self,
        duration: &HirExpr,
        body: &HirBlock,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        let Some(scope_place) = self.current_task_scope else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "scope deadline cancellation edge".to_string(),
                    site,
                },
                note: "deadline reached MIR without an active task scope token".to_string(),
            });
            return None;
        };
        // Deadline bodies lower later on this same Builder, after the outer
        // function pre-pass has already completed. Refresh the fixed-point
        // facts here so body-local fn producers cannot bypass the generator
        // provenance gate.
        self.collect_prepass_facts(body);
        let has_body = !body.statements.is_empty() || body.tail.is_some();

        // Empty `after(d) {}` keeps the legacy per-deadline-thread cancel on BOTH
        // call-convs — there is no body to route to, so nothing suspends. The
        // deadline simply cancels the scope's children when it fires.
        if !has_body {
            let duration_place = self.lower_value(duration)?;
            self.push_runtime_call(
                "hew_task_scope_cancel_after_ns",
                vec![scope_place, duration_place],
                None,
            );
            let unit_place = self.alloc_local(ResolvedTy::Unit);
            self.push_instr(Instr::UnitLit { dest: unit_place });
            return Some(unit_place);
        }

        // A NON-EMPTY `after(d) { body }` from a SUSPENDABLE caller (actor handler
        // / closure / task entry) lowers onto the `SuspendingScopeDeadline`
        // carrier: codegen arms a deadline on the global timer wheel carrying the
        // parked continuation, the scope's children run, and the FIRST of {all
        // children joined, deadline fired} wins the shared one-shot arbiter. The
        // deadline-wins edge routes to `timeout_body_block` (the lowered `after`
        // body); the join-wins edge skips it. A `FunctionCallConv::Default` caller
        // has no parkable continuation, so the non-empty timeout body stays
        // fail-closed there (mirrors the suspending await / sleep / select flips).
        if !self.current_function_call_conv.carries_execution_context() {
            // Lower the duration + body operands for diagnostics coherence, then
            // fail closed: a contextless caller cannot park to run the body.
            let _ = self.lower_value(duration);
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "scope deadline body".to_string(),
                    site,
                },
                note: "a non-empty after(...) timeout body is only lowered from an \
                       execution-context caller (actor handler / closure / task \
                       entry); a contextless caller has no parkable continuation to \
                       run the body on the deadline edge"
                    .to_string(),
            });
            return None;
        }

        let duration_place = self.lower_value(duration)?;

        // Allocate the timer-fired body block + the convergence (resume) block.
        // The carrier's `resume` is the scope-complete path AND the point the
        // timeout body falls through to; `cleanup` reuses `resume` exactly as the
        // await / sleep / select carriers do (the coro `cleanup` outline is the
        // abandon teardown owner, not this MIR block).
        let timeout_body_block = self.alloc_block();
        let resume = self.alloc_block();

        self.finish_current_block(Terminator::SuspendingScopeDeadline {
            scope: scope_place,
            duration_ms: duration_place,
            timeout_body_block,
            resume,
            cleanup: resume,
        });

        // The deadline-fired edge: lower the `after(...)` body, then converge on
        // `resume`. The body runs ONLY on the timeout edge; the join-wins resume
        // edge branches straight into `resume` from the coro switch.
        self.start_block(timeout_body_block);
        for stmt in &body.statements {
            self.stmt(stmt);
        }
        if let Some(tail) = &body.tail {
            let _ = self.lower_value(tail);
        }
        self.finish_current_block(Terminator::Goto { target: resume });

        self.start_block(resume);
        let unit_place = self.alloc_local(ResolvedTy::Unit);
        self.push_instr(Instr::UnitLit { dest: unit_place });
        Some(unit_place)
    }

    pub(crate) fn lower_await_task(
        &mut self,
        binding_name: &str,
        binding_id: BindingId,
        output_ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        let is_value_task = !matches!(output_ty, ResolvedTy::Unit);
        let Some(task_place) = self.binding_locals.get(&binding_id).copied() else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::UnresolvedPlace {
                    binding: binding_id,
                    name: "<await-task>".to_string(),
                    site,
                },
                note: "await task binding has no backend task handle slot".to_string(),
            });
            return None;
        };
        // `await t` consumes the linear task handle. Record the consume fact
        // so the dataflow MustConsume exit check sees the handle as used and
        // a second `await t` reports UseAfterConsume. Mirrors the
        // `BindingRef { intent: Consume }` path in `lower_value`.
        self.statements.push(MirStatement::Use {
            binding: binding_id,
            name: binding_name.to_string(),
            site,
            ty: ResolvedTy::Task(Box::new(output_ty.clone())),
            intent: IntentKind::Consume,
        });
        self.mark_binding_moved(binding_id);

        // Suspendable-caller flip: in a caller that carries the execution
        // context (actor handler / closure / task entry) `await t` SUSPENDS on
        // the child task's completion instead of blocking the worker in
        // `hew_task_await_blocking` (a condvar). The completion observer
        // (`hew_task_await_suspend`) re-enqueues the parked continuation on the
        // child's `Done`. A `FunctionCallConv::Default` caller (`main`, free fn)
        // has no parkable continuation and keeps the blocking call. Reuses the
        // same `carries_execution_context` discriminator as the recv/ask flips.
        if self.current_function_call_conv.carries_execution_context() {
            if let Some(scope_place) = self.current_task_scope {
                // Value task: allocate the slot the child's `T` is read into on
                // the resume edge (codegen reads `hew_task_get_result` into it,
                // copying the result-buffer bytes at the `T` element width). A
                // unit task binds nothing.
                let result_dest = if is_value_task {
                    Some(self.alloc_local(output_ty.clone()))
                } else {
                    None
                };
                let next = self.alloc_block();
                // The carrier rides the multi-suspend epilogue, so `cleanup`
                // reuses `next` exactly as the recv/ask carriers do.
                self.record_suspend_kind(SuspendKind::TaskAwait {
                    scope: scope_place,
                    task: task_place,
                    result_dest,
                });
                self.finish_current_block(Terminator::Suspend {
                    resume: next,
                    cleanup: next,
                    is_final: false,
                });
                self.start_block(next);
                if let Some(result_dest) = result_dest {
                    // The resume edge bound the child's `T` into `result_dest`.
                    return Some(result_dest);
                }
                let unit_place = self.alloc_local(ResolvedTy::Unit);
                self.push_instr(Instr::UnitLit { dest: unit_place });
                return Some(unit_place);
            }
        }

        // Contextless keep path: a `FunctionCallConv::Default` caller has no
        // parkable continuation and blocks the foreign thread on the condvar.
        // Value tasks cannot reach this path: `lower_spawned_call_task` refuses
        // to spawn from a Default-callconv caller, so a value `Task<T>` handle
        // never exists in a contextless awaiter. Fail closed rather than emit a
        // blocking read whose result-width copy was never proven on this path.
        if is_value_task {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "await task result".to_string(),
                    site,
                },
                note: "awaiting a value-returning task is only lowered from an \
                       execution-context caller (actor handler / closure / task \
                       entry); a `Task<T>` handle cannot reach a Default-callconv \
                       awaiter because value tasks cannot be spawned there"
                    .to_string(),
            });
            return None;
        }
        self.push_runtime_call("hew_task_await_blocking", vec![task_place], None);
        let unit_place = self.alloc_local(ResolvedTy::Unit);
        self.push_instr(Instr::UnitLit { dest: unit_place });
        Some(unit_place)
    }

    /// Lower a sealed `select{}` expression to MIR.
    ///
    /// ## Shape produced
    ///
    /// ```text
    /// originating_bb:
    ///   <lower each arm's actor receiver + args / duration into Places>
    ///   Terminator::Select { arms, next: join_bb }
    ///
    /// arm_body_bb[i]:                    // entered when arm i wins
    ///   <body lowers; binding (if any) resolves through binding_locals
    ///    to the per-arm reply slot codegen writes via hew_reply_wait>
    ///   Move { dest: result_place, src: <arm body value> }
    ///   Terminator::Goto { target: join_bb }
    ///
    /// join_bb:                           // single convergence point
    ///   <subsequent function lowering continues here; the select's
    ///    value is result_place, written by exactly one arm body>
    /// ```
    ///
    /// ## Producer-bridge contract (consumed by codegen / slice 3)
    ///
    /// Codegen reads `Terminator::Select { arms, next }` and, for each
    /// currently-supported arm:
    ///   * `SelectArmKind::ActorAsk { actor, method, args }` — emits
    ///     `hew_reply_channel_new` + `hew_actor_ask_with_channel` per
    ///     arm in the originating block; calls `hew_select_first` to
    ///     pick a winner; on win, calls `hew_reply_wait` and writes the
    ///     reply into `arm.binding` (the reply slot MIR allocated),
    ///     then jumps to `arm.body_block`; on loss, calls
    ///     `hew_reply_channel_cancel` + `hew_reply_channel_free`.
    ///   * `SelectArmKind::AfterTimer { duration }` — wins when the
    ///     deadline elapses; jumps to `arm.body_block` with no binding.
    ///
    /// ## Out-of-scope consumer arm kinds (fail-closed)
    ///
    /// `StreamNext` and `TaskAwait` are produced in MIR so the producer
    /// boundary carries the sealed HIR shape forward. Backend target-specific
    /// fail-closed checks live in codegen, where the requested target is known.
    ///
    /// ## Cleanup-CFG composition (D24-2 / `ExitPath::Select`)
    ///
    /// The select terminator emits a `Terminator::Select`; the
    /// elaboration pass (`enumerate_exits` at lower.rs:6450) wires
    /// `ExitPath::Select { block: originating_bb, next: join_bb }`
    /// into `drop_plans` automatically — the function-wide LIFO drop
    /// plan is empty for this exit (per-arm loser cleanup happens at
    /// the codegen dispatch site, not at function exit).
    #[allow(
        clippy::too_many_lines,
        reason = "lower_select threads four phases — arm-kind rejection, \
                  block allocation, per-arm Place lowering + binding \
                  registration, and per-arm body emit — that don't \
                  factor cleanly into helpers without re-threading \
                  Builder state (binding_locals, statements buffer, \
                  current block cursor)"
    )]
    pub(crate) fn lower_select(
        &mut self,
        select: &HirSelect,
        expected_ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        // Result local first so it dominates every arm-body's Move.
        // For Unit-typed selects the placeholder write is benign — no
        // load occurs in the join block. Mirrors the `lower_if` pattern.
        let result_place = self.alloc_local(expected_ty.clone());

        // Allocate body blocks for every arm and the single join block
        // up front so each `SelectArm.body_block` is known before the
        // originating block seals with `Terminator::Select`.
        let body_bbs: Vec<u32> = (0..select.arms.len()).map(|_| self.alloc_block()).collect();
        let join_bb = self.alloc_block();

        // Lower per-arm operands and allocate per-arm value slots in the
        // ORIGINATING block.
        // Codegen consumes the SelectArm payload to emit the per-arm
        // setup (channel alloc + ask issue) in the same originating
        // block before the `hew_select_first` dispatch.
        let mut mir_arms: Vec<SelectArm> = Vec::with_capacity(select.arms.len());
        for (arm_index, arm) in select.arms.iter().enumerate() {
            let (kind, binding_place) = match &arm.kind {
                HirSelectArmKind::ActorAsk {
                    actor,
                    method,
                    args,
                } => {
                    // Resolve the actor handler's reply type so the
                    // reply slot is typed correctly. Mirrors the
                    // single-arm `lower_actor_ask` path; differs only
                    // in that the wait + bind happen across the
                    // Terminator::Select boundary, not Terminator::Ask.
                    let info = self.actor_method_info(&actor.ty, method, site)?;
                    if info.param_tys.len() != args.len() {
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct: format!(
                                    "select actor-ask arm arity mismatch for `{method}`"
                                ),
                                site,
                            },
                            note: format!(
                                "handler expects {} argument(s), arm supplied {}",
                                info.param_tys.len(),
                                args.len()
                            ),
                        });
                        return None;
                    }
                    let actor_place = self.lower_value(actor)?;
                    // F-04: a select arm asking a FUNGIBLE supervisor-child
                    // reference re-resolves the current child into the handle
                    // alloca before the select dispatch. Like the single-shot
                    // ask, no liveness branch is needed here: a not-live slot
                    // resolves to a null handle, and the runtime ask path
                    // fail-closes a null/stale actor to an ask error
                    // (`actor_send_result_internal_reply` null-guard) rather than
                    // a UAF or trap.
                    if let Some(child_ref) = self.fungible_child_ref_of(actor_place) {
                        self.emit_child_get_into(
                            child_ref.sup_place,
                            child_ref.slot_index,
                            actor_place,
                        );
                    }
                    // Lower each argument exactly once with move semantics. The
                    // resulting places are both the arm's source authority and
                    // the inputs to its packed payload; re-lowering here would
                    // leave the original handler-owned binding live after the
                    // mailbox handoff and make exit cleanup double-drop it.
                    let mut lowered = Vec::with_capacity(args.len());
                    for arg in args {
                        lowered.push((self.lower_value_for_move(arg)?, self.subst_ty(&arg.ty)));
                    }
                    let arg_places = lowered.iter().map(|(place, _)| *place).collect();
                    self.record_pending_actor_request_args(
                        self.current_block_id,
                        super::PendingOutboundTarget::SelectArm(arm_index),
                        lowered
                            .iter()
                            .zip(args)
                            .map(|((place, ty), arg)| (*place, ty.clone(), arg.site)),
                    );
                    // Pack args into the same payload shape as a single-shot ask:
                    // one payload ptr + size through `hew_actor_ask_with_channel`.
                    let payload_place = match &lowered[..] {
                        [] => self.alloc_local(ResolvedTy::Unit),
                        [(place, _)] => *place,
                        _ => {
                            let (field_places, field_tys): (Vec<Place>, Vec<ResolvedTy>) =
                                lowered.into_iter().unzip();
                            self.pack_actor_payload_from_places(field_places, field_tys)
                        }
                    };
                    // Per-arm reply slot. Codegen writes
                    // `hew_reply_wait`'s result here on win before
                    // jumping into the arm body. Register against the
                    // HIR binding so the body's BindingRef resolves to
                    // this slot.
                    let reply_dest = self.alloc_local(info.return_ty.clone());
                    if let Some(binding_id) = arm.binding_id {
                        self.binding_locals.insert(binding_id, reply_dest);
                    }
                    (
                        SelectArmKind::ActorAsk {
                            actor: actor_place,
                            method: method.clone(),
                            args: arg_places,
                            msg_type: info.msg_type,
                            value: payload_place,
                            cleanup_plan: None,
                        },
                        Some(reply_dest),
                    )
                }
                HirSelectArmKind::StreamNext { stream } => {
                    let stream_place = self.lower_value(stream)?;
                    let stream_ty = self.subst_ty(&stream.ty);
                    let item_ty = match stream_ty {
                        ResolvedTy::Named { name, mut args, .. }
                            if name == "Stream" && args.len() == 1 =>
                        {
                            args.remove(0)
                        }
                        _ => ResolvedTy::Unit,
                    };
                    let item_dest = self.alloc_local(item_ty);
                    if let Some(binding_id) = arm.binding_id {
                        self.binding_locals.insert(binding_id, item_dest);
                    }
                    (
                        SelectArmKind::StreamNext {
                            stream: stream_place,
                        },
                        Some(item_dest),
                    )
                }
                HirSelectArmKind::TaskAwait { task } => {
                    let task_place = self.lower_value(task)?;
                    let task_ty = self.subst_ty(&task.ty);
                    let await_ty = match task_ty {
                        ResolvedTy::Task(inner) => *inner,
                        _ => ResolvedTy::Unit,
                    };
                    let await_dest = self.alloc_local(await_ty);
                    if let Some(binding_id) = arm.binding_id {
                        self.binding_locals.insert(binding_id, await_dest);
                    }
                    (
                        SelectArmKind::TaskAwait { task: task_place },
                        Some(await_dest),
                    )
                }
                HirSelectArmKind::ChannelRecv { receiver } => {
                    // The arm binds `Option<T>` — the same shape an awaited
                    // `rx.recv()` produces; the winner edge pops the queued item
                    // via the non-blocking layout-witness try_recv and
                    // materialises it. The element type comes from the
                    // checker-resolved `Receiver<T>` handle type (mirror of the
                    // `Stream<T>` extraction on the StreamNext arm above), never
                    // from a runtime symbol name.
                    let recv_place = self.lower_value(receiver)?;
                    let receiver_ty = self.subst_ty(&receiver.ty);
                    // Dispatch on the typed builtin discriminator (with the
                    // short-name fallback): the name string is `"Receiver"`
                    // for a locally constructed handle but module-qualified
                    // (`"channel.Receiver"`) for an annotated parameter, and
                    // the bare-name compare silently fell through to the Unit
                    // witness. Mirrors `select_arm_binding_ty` in
                    // `hew-hir/src/lower.rs`.
                    let elem = match receiver_ty {
                        ResolvedTy::Named {
                            name,
                            mut args,
                            builtin,
                            ..
                        } if args.len() == 1
                            && (matches!(builtin, Some(hew_types::BuiltinType::Receiver))
                                || hew_types::short_name(&name) == "Receiver") =>
                        {
                            args.remove(0)
                        }
                        // A malformed receiver type cannot supply a witness;
                        // Unit produces a zero-size witness the runtime
                        // aborts on fail-closed (mirrors the StreamNext arm's
                        // Unit fallback).
                        _ => ResolvedTy::Unit,
                    };
                    let option_ty = ResolvedTy::Named {
                        name: "Option".to_string(),
                        args: vec![elem.clone()],
                        builtin: None,
                        is_opaque: false,
                    };
                    let item_dest = self.alloc_local(option_ty);
                    if let Some(binding_id) = arm.binding_id {
                        self.binding_locals.insert(binding_id, item_dest);
                    }
                    (
                        SelectArmKind::ChannelRecv {
                            receiver: recv_place,
                            elem_ty: elem,
                        },
                        Some(item_dest),
                    )
                }
                HirSelectArmKind::AfterTimer { duration } => {
                    let duration_place = self.lower_value(duration)?;
                    // AfterTimer arms bind no value — `binding_id` is
                    // None by construction (HIR forbids `<name> from
                    // after ...` patterns). Defensive: even if a
                    // future HIR shape attached a binding, we'd skip
                    // registration since codegen has no value to write.
                    debug_assert!(
                        arm.binding_id.is_none(),
                        "AfterTimer arms must not carry a binding_id"
                    );
                    (
                        SelectArmKind::AfterTimer {
                            duration: duration_place,
                        },
                        None,
                    )
                }
            };
            mir_arms.push(SelectArm {
                kind,
                body_block: body_bbs[arm_index],
                binding: binding_place,
            });
        }

        // Seal the originating block with the select terminator.
        //
        // Suspendable-caller flip: in a caller that carries the execution
        // context (actor handler / closure / task entry) the `select{}`
        // SUSPENDS on the first-ready of its arms instead of busy-polling the
        // worker in `hew_select_first`. The `SuspendingSelect` carrier rides the
        // SAME `arms` payload (identical per-arm body blocks + bindings) but
        // codegen builds the readiness waitset + arms the deadline on the global
        // timer wheel + `coro.suspend`s, resuming on the first-ready wake and
        // cancelling the losers. `cleanup` reuses `join_bb` (the resume edge)
        // exactly as the recv / ask / sleep carriers do — the coro `cleanup`
        // outline is the abandon teardown owner, not this MIR block. A
        // `FunctionCallConv::Default` caller (`main`, free fn) has no parkable
        // continuation and keeps the blocking `Terminator::Select` /
        // `hew_select_first` path. Reuses the same `carries_execution_context`
        // discriminator as the recv / ask / await / sleep flips.
        if self.current_function_call_conv.carries_execution_context() {
            self.finish_current_block(Terminator::SuspendingSelect {
                arms: mir_arms,
                resume: join_bb,
                cleanup: join_bb,
            });
        } else {
            self.finish_current_block(Terminator::Select {
                arms: mir_arms,
                next: join_bb,
            });
        }

        // Per-arm body blocks. Each lowers the arm body; the body's
        // BindingRef (for ActorAsk arms with a binding) resolves
        // through `binding_locals` to the per-arm reply slot codegen
        // populated. AfterTimer arms have no binding by construction.
        // Every body block terminates with Goto join_bb so the join
        // converges (single-predecessor-per-arm CFG; the converging
        // result_place plays the role of an SSA phi for the join).
        for (arm_index, arm) in select.arms.iter().enumerate() {
            self.start_block(body_bbs[arm_index]);
            // ActorAsk arms with a value-bearing binding: emit a
            // `MirStatement::Bind` at the body-block entry so the
            // dataflow pass sees the binding initialised before the
            // body's `BindingRef` reads. Codegen writes
            // `hew_reply_wait`'s result into `SelectArm.binding` on
            // win, then jumps into this body block — the Bind here
            // mirrors that runtime initialisation in the MIR
            // statement stream.
            let mut arm_binding: Option<(BindingId, Place)> = None;
            if let (Some(binding_id), Some(binding_name)) =
                (arm.binding_id, arm.binding_name.as_ref())
            {
                let binding_ty = self.subst_ty(&arm.body.ty);
                // The arm body's HIR type matches the bound reply
                // type (HIR's `select_arm_binding_ty` for ActorAsk
                // arms reads the same `actor_method_dispatch` reply
                // type the body's `BindingRef.ty` carries).
                // Use the arm-binding's resolved type (not the body
                // expression's) by consulting binding_locals' Place
                // which we populated earlier.
                let _ = binding_ty;
                let binding_place = self
                    .binding_locals
                    .get(&binding_id)
                    .copied()
                    .expect("ActorAsk arm registered its reply slot before body lowering");
                let ty_of_place: ResolvedTy = match binding_place {
                    Place::Local(n) => self.locals[n as usize].clone(),
                    _ => arm.body.ty.clone(),
                };
                self.statements.push(MirStatement::Bind {
                    binding: binding_id,
                    name: binding_name.clone(),
                    site: arm.body.site,
                    ty: ty_of_place.clone(),
                });
                self.record_binding_scope(binding_id);
                // The select-arm binding owns the value the runtime
                // materialises into its slot on the win edge (the reply
                // channel / channel reaps only NON-consumed legs), so it
                // enters `owned_locals` exactly like a `let`-bound owned
                // local. Win-edge-only release is the DATAFLOW's
                // invariant, not this Bind's scope placement: the binding
                // is `Live` only in its own body block, the join-entry
                // meet demotes it to absent (`Uninit ⊔ X = Uninit`,
                // dataflow.rs), and the scope-close forward-`Goto` pass
                // releases it on the winning body-block→join edge — the
                // last point it is provably the live sole owner. Loser
                // arms' slots are never `Live`, so no drop can fire on
                // them. `ValueOwnership::classify` filters BitCopy types
                // to no-op drops, and the move-out mark below suppresses
                // the drop when the arm body moves the value out.
                // Registering here — the one site shared by every
                // value-bearing arm kind — covers ActorAsk, StreamNext,
                // TaskAwait, and ChannelRecv uniformly; AfterTimer arms
                // bind nothing and fall outside this block.
                self.register_owned_local(binding_id, binding_name.clone(), ty_of_place);
                arm_binding = Some((binding_id, binding_place));
            }
            let body_value = self.lower_value(&arm.body);
            if let Some(src) = body_value {
                // Direct escape (`=> r`): the arm body's value IS the arm
                // binding's own slot, and the Move below transfers it into
                // the select result. Mark the binding consumed so its
                // scope-exit drop is suppressed and the destination
                // (`let x = select { ... }`) is the sole owner — otherwise
                // two owned locals alias one heap pointer and the drop
                // provers fail closed to a leak on both.
                if let Some((binding_id, binding_place)) = arm_binding {
                    if src == binding_place {
                        self.mark_binding_moved(binding_id);
                    }
                }
                self.push_instr(Instr::Move {
                    dest: result_place,
                    src,
                });
            }
            self.finish_current_block(Terminator::Goto { target: join_bb });
        }

        // Join block — subsequent lowering continues here.
        self.start_block(join_bb);
        Some(result_place)
    }

    /// Lower a `join { ... }` expression — STAGE 1 fail-closed placeholder.
    ///
    /// Stage 2 replaces this with the real `Terminator::Join` producer (the
    /// wait-ALL sibling of `lower_select`). Until then, fail closed with a
    /// `NotYetImplemented` diagnostic so `hew check` reports a precise
    /// limitation rather than silently mis-lowering the construct.
    /// Lower a `join { ... }` expression — the wait-ALL sibling of
    /// `lower_select`. Allocates the result-tuple local, issues every
    /// branch ask (resolving the handler reply type + packing the
    /// payload exactly as the `select` `ActorAsk` arm does), and seals the
    /// originating block with `Terminator::Join`. Codegen waits for ALL
    /// replies and materialises the tuple; per HEW-SPEC-2026 §4.11.2 a
    /// branch trap cancels the remaining branches and propagates.
    pub(crate) fn lower_join(
        &mut self,
        join: &HirJoin,
        expected_ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        // Result tuple local first so it dominates the next block.
        let result_place = self.alloc_local(expected_ty.clone());
        let next_bb = self.alloc_block();

        let mut mir_branches: Vec<JoinBranch> = Vec::with_capacity(join.branches.len());
        for (branch_index, branch) in join.branches.iter().enumerate() {
            // Resolve the actor handler's reply type so the per-branch
            // reply slot is typed correctly — identical to the single-arm
            // `lower_actor_ask` / select ActorAsk path.
            let info = self.actor_method_info(&branch.actor.ty, &branch.method, site)?;
            if info.param_tys.len() != branch.args.len() {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!(
                            "join actor-ask branch arity mismatch for `{}`",
                            branch.method
                        ),
                        site,
                    },
                    note: format!(
                        "handler expects {} argument(s), branch supplied {}",
                        info.param_tys.len(),
                        branch.args.len()
                    ),
                });
                return None;
            }
            let actor_place = self.lower_value(&branch.actor)?;
            // Lower each argument once and use those exact moved places for the
            // branch metadata and payload. This keeps handler-param ownership
            // singular when a join forwards an owning value.
            let mut lowered = Vec::with_capacity(branch.args.len());
            for arg in &branch.args {
                lowered.push((self.lower_value_for_move(arg)?, self.subst_ty(&arg.ty)));
            }
            let arg_places = lowered.iter().map(|(place, _)| *place).collect();
            self.record_pending_actor_request_args(
                self.current_block_id,
                super::PendingOutboundTarget::JoinBranch(branch_index),
                lowered
                    .iter()
                    .zip(&branch.args)
                    .map(|((place, ty), arg)| (*place, ty.clone(), arg.site)),
            );
            // Pack args into the same payload shape as single-shot ask/select.
            let payload_place = match &lowered[..] {
                [] => self.alloc_local(ResolvedTy::Unit),
                [(place, _)] => *place,
                _ => {
                    let (field_places, field_tys): (Vec<Place>, Vec<ResolvedTy>) =
                        lowered.into_iter().unzip();
                    self.pack_actor_payload_from_places(field_places, field_tys)
                }
            };
            // Per-branch reply slot. Codegen writes `hew_reply_wait`'s
            // result here, then composes it into `result_place`'s tuple
            // element at this branch's index.
            let reply_dest = self.alloc_local(info.return_ty.clone());
            mir_branches.push(JoinBranch {
                actor: actor_place,
                method: branch.method.clone(),
                args: arg_places,
                msg_type: info.msg_type,
                value: payload_place,
                cleanup_plan: None,
                reply_dest,
                reply_ty: info.return_ty.clone(),
            });
        }

        // Seal the originating block with the join terminator.
        self.finish_current_block(Terminator::Join {
            branches: mir_branches,
            result: result_place,
            next: next_bb,
        });

        // Continuation — subsequent lowering resumes after all replies
        // landed and the result tuple is bound.
        self.start_block(next_bb);
        Some(result_place)
    }

    /// Lower `await conn.read()` / `await conn.read_string()` (NEW-1). Mirrors
    /// `lower_actor_ask`'s suspendable-caller flip:
    ///
    /// - A caller that carries the execution context (actor handler / closure /
    ///   task entry) lowers to `Terminator::SuspendingRead`: the read suspends
    ///   (freeing the worker) and resumes when the reactor reports the fd ready,
    ///   binding the bytes on the resume edge.
    /// - A `FunctionCallConv::Default` caller (`main`, free fn) runs on a
    ///   foreign/main thread with no parkable continuation, so it keeps the
    ///   blocking `hew_tcp_read` FFI call (E8).
    ///
    /// `read_string` reads `bytes` then converts via `hew_bytes_to_string`; the
    /// suspend carrier always binds `bytes` (the value-routing destination), and
    /// the string conversion wraps the bound bytes on the resume edge.
    pub(crate) fn lower_conn_await_read(
        &mut self,
        conn: &HirExpr,
        to_string: bool,
        deadline_ns: Option<i64>,
        expr: &HirExpr,
    ) -> Option<Place> {
        let conn_place = self.lower_value(conn)?;
        let bytes_ty = ResolvedTy::Bytes;

        // The bytes slot the read binds (the SuspendingRead `result_dest` / the
        // blocking `hew_tcp_read` return).
        let bytes_dest = self.alloc_local(bytes_ty.clone());

        if self.current_function_call_conv.carries_execution_context() {
            let deadline_result_dest =
                deadline_ns.map(|_| self.alloc_local(self.subst_ty(&expr.ty)));
            let error_dest = deadline_ns.map(|_| {
                self.alloc_local(ResolvedTy::Named {
                    name: "NetError".to_string(),
                    args: Vec::new(),
                    builtin: None,
                    is_opaque: false,
                })
            });
            let next = self.alloc_block();
            if let Some(ns) = deadline_ns {
                self.await_deadline_ns.insert(self.current_block_id, ns);
            }
            // `SuspendingRead` carries no separate MIR cleanup block — it rides
            // the multi-suspend epilogue, so `cleanup` reuses `next` (exactly as
            // `SuspendingAsk` does).
            self.record_suspend_kind(SuspendKind::Read {
                conn: conn_place,
                result_dest: bytes_dest,
                deadline_result_dest,
                error_dest,
                to_string: to_string && deadline_result_dest.is_some(),
            });
            self.finish_current_block(Terminator::Suspend {
                resume: next,
                cleanup: next,
                is_final: false,
            });
            self.start_block(next);
            if let Some(result_dest) = deadline_result_dest {
                if to_string {
                    // `read_string | after d` success path: codegen already
                    // converted bytes → string and packed Ok(string) into
                    // `result_dest` (via the `to_string` flag on the terminator).
                    // MIR skips the bytes-to-string call here — the conversion
                    // is codegen-side on the resume edge.
                    return Some(result_dest);
                }
                return Some(result_dest);
            }
        } else {
            if deadline_ns.is_some() {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "`await conn.read() | after d` in a non-suspendable context"
                            .to_string(),
                        site: expr.site,
                    },
                    note: "read deadlines require a suspendable actor/closure/task context; \
                           default-call-convention functions have no parkable continuation to \
                           resume on timeout"
                        .to_string(),
                });
                return None;
            }
            // Default callers use the blocking read FFI: they run on a foreign/main
            // thread with no parkable continuation. Closure shims above fail closed
            // for captured Connection awaits until closure invocations can suspend.
            let next = self.alloc_block();
            self.finish_current_block(Terminator::Call {
                callee: "hew_tcp_read".to_string(),
                builtin: None,
                args: vec![conn_place],
                dest: Some(bytes_dest),
                next,
            });
            self.start_block(next);
        }

        if !to_string {
            return Some(bytes_dest);
        }
        // `read_string`: convert the bound bytes to a string.
        let string_dest = self.alloc_local(self.subst_ty(&expr.ty));
        let next = self.alloc_block();
        self.finish_current_block(Terminator::Call {
            callee: "hew_bytes_to_string".to_string(),
            builtin: None,
            args: vec![bytes_dest],
            dest: Some(string_dest),
            next,
        });
        self.start_block(next);
        Some(string_dest)
    }

    /// Lower `await rx.recv() | after d` (L4 phase 2, channel form). Suspending
    /// channel-recv with a deadline attached. When `deadline_ns` is present:
    ///
    /// - `expr.ty` is `Result<Option<T>, TimeoutError>` (set by HIR).
    /// - `result_dest` is allocated for the raw `Option<T>` slot; codegen wraps
    ///   it into `Ok(_)` or emits `Err(TimeoutError::Timeout)` via the
    ///   `deadline_result_dest` slot.
    ///
    /// In a non-suspendable context with a deadline, fail closed with a
    /// diagnostic (no parkable continuation → no deadline semantics possible).
    pub(crate) fn lower_channel_recv_await(
        &mut self,
        receiver: &HirExpr,
        deadline_ns: Option<i64>,
        expr: &HirExpr,
    ) -> Option<Place> {
        let receiver_place = self.lower_value(receiver)?;

        // When deadline is active, `expr.ty` is `Result<Option<T>, TimeoutError>`.
        // Extract `Option<T>` for result_dest; allocate outer slot for deadline_result_dest.
        let option_ty = if deadline_ns.is_some() {
            match &expr.ty {
                hew_types::ResolvedTy::Named { args, .. } if !args.is_empty() => {
                    self.subst_ty(&args[0])
                }
                other => self.subst_ty(other),
            }
        } else {
            self.subst_ty(&expr.ty)
        };

        // Extract the element type from `Option<T>`.
        let elem_ty = match &option_ty {
            hew_types::ResolvedTy::Named { args, .. } if !args.is_empty() => {
                self.subst_ty(&args[0])
            }
            other => other.clone(),
        };

        let result_dest = self.alloc_local(option_ty);

        if self.current_function_call_conv.carries_execution_context() {
            let deadline_result_dest =
                deadline_ns.map(|_| self.alloc_local(self.subst_ty(&expr.ty)));
            let error_dest = deadline_ns.map(|_| {
                self.alloc_local(hew_types::ResolvedTy::Named {
                    name: "TimeoutError".to_string(),
                    args: Vec::new(),
                    builtin: Some(hew_types::BuiltinType::TimeoutError),
                    is_opaque: false,
                })
            });
            let next = self.alloc_block();
            if let Some(ns) = deadline_ns {
                self.await_deadline_ns.insert(self.current_block_id, ns);
            }
            self.record_suspend_kind(SuspendKind::ChannelRecv {
                receiver: receiver_place,
                result_dest,
                elem_ty: elem_ty.clone(),
                deadline_result_dest,
                error_dest,
            });
            self.finish_current_block(Terminator::Suspend {
                resume: next,
                cleanup: next,
                is_final: false,
            });
            self.start_block(next);
            if let Some(outer_dest) = deadline_result_dest {
                return Some(outer_dest);
            }
        } else {
            if deadline_ns.is_some() {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "`await rx.recv() | after d` in a non-suspendable context"
                            .to_string(),
                        site: expr.site,
                    },
                    note: "channel recv deadlines require a suspendable actor/closure/task \
                           context; default-call-convention functions have no parkable \
                           continuation to resume on timeout"
                        .to_string(),
                });
                return None;
            }
            // Default callers keep the blocking hew_channel_recv_layout FFI call.
            let next = self.alloc_block();
            self.finish_current_block(Terminator::Call {
                callee: "hew_channel_recv_layout".to_string(),
                builtin: hew_types::runtime_call::RuntimeCallFamily::from_c_symbol(
                    "hew_channel_recv_layout",
                ),
                args: vec![receiver_place],
                dest: Some(result_dest),
                next,
            });
            self.start_block(next);
        }

        Some(result_dest)
    }

    /// Lower `await stream.recv() | after d` (L4 phase 2, stream form). Suspending
    /// stream-recv with a deadline attached. When `deadline_ns` is present:
    ///
    /// - `expr.ty` is `Result<Option<T>, TimeoutError>` (set by HIR).
    /// - `result_dest` is allocated for the raw `Option<T>` slot; codegen wraps
    ///   it into `Ok(_)` or emits `Err(TimeoutError::Timeout)` via the
    ///   `deadline_result_dest` slot.
    ///
    /// In a non-suspendable context with a deadline, fail closed with a
    /// diagnostic (no parkable continuation → no deadline semantics possible).
    pub(crate) fn lower_stream_recv_await(
        &mut self,
        stream: &HirExpr,
        deadline_ns: Option<i64>,
        expr: &HirExpr,
    ) -> Option<Place> {
        let stream_place = self.lower_value(stream)?;

        // When deadline is active, `expr.ty` is `Result<Option<T>, TimeoutError>`.
        // Extract `Option<T>` for result_dest; allocate outer slot for deadline_result_dest.
        let option_ty = if deadline_ns.is_some() {
            match &expr.ty {
                hew_types::ResolvedTy::Named { args, .. } if !args.is_empty() => {
                    self.subst_ty(&args[0])
                }
                other => self.subst_ty(other),
            }
        } else {
            self.subst_ty(&expr.ty)
        };

        // Extract the element type from `Option<T>`.
        let elem_ty = match &option_ty {
            hew_types::ResolvedTy::Named { args, .. } if !args.is_empty() => {
                self.subst_ty(&args[0])
            }
            other => other.clone(),
        };

        let result_dest = self.alloc_local(option_ty);

        if self.current_function_call_conv.carries_execution_context() {
            let deadline_result_dest =
                deadline_ns.map(|_| self.alloc_local(self.subst_ty(&expr.ty)));
            let error_dest = deadline_ns.map(|_| {
                self.alloc_local(hew_types::ResolvedTy::Named {
                    name: "TimeoutError".to_string(),
                    args: Vec::new(),
                    builtin: Some(hew_types::BuiltinType::TimeoutError),
                    is_opaque: false,
                })
            });
            let next = self.alloc_block();
            if let Some(ns) = deadline_ns {
                self.await_deadline_ns.insert(self.current_block_id, ns);
            }
            self.record_suspend_kind(SuspendKind::StreamNext {
                stream: stream_place,
                result_dest,
                elem_ty: elem_ty.clone(),
                deadline_result_dest,
                error_dest,
            });
            self.finish_current_block(Terminator::Suspend {
                resume: next,
                cleanup: next,
                is_final: false,
            });
            self.start_block(next);
            if let Some(outer_dest) = deadline_result_dest {
                return Some(outer_dest);
            }
        } else {
            if deadline_ns.is_some() {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "`await stream.recv() | after d` in a non-suspendable context"
                            .to_string(),
                        site: expr.site,
                    },
                    note: "stream recv deadlines require a suspendable actor/closure/task \
                           context; default-call-convention functions have no parkable \
                           continuation to resume on timeout"
                        .to_string(),
                });
                return None;
            }
            // Default callers keep the blocking hew_stream_next_layout FFI call.
            let next = self.alloc_block();
            self.finish_current_block(Terminator::Call {
                callee: "hew_stream_next_layout".to_string(),
                builtin: hew_types::runtime_call::RuntimeCallFamily::from_c_symbol(
                    "hew_stream_next_layout",
                ),
                args: vec![stream_place],
                dest: Some(result_dest),
                next,
            });
            self.start_block(next);
        }

        Some(result_dest)
    }

    /// Lower `await listener.accept()` (NEW-2). The listener-readiness sibling of
    /// [`Self::lower_conn_await_read`]:
    ///
    /// - A caller that carries the execution context (actor handler / closure /
    ///   task entry) lowers to `Terminator::SuspendingAccept`: the accept
    ///   suspends (freeing the worker) and resumes when the reactor reports the
    ///   listener ready, binding the accepted `Connection` on the resume edge.
    /// - A `FunctionCallConv::Default` caller (`main`, free fn) runs on a
    ///   foreign/main thread with no parkable continuation, so it keeps the
    ///   blocking `hew_tcp_accept` FFI call (the caller-conv flip mirrors
    ///   `lower_conn_await_read`).
    pub(crate) fn lower_listener_await_accept(
        &mut self,
        listener: &HirExpr,
        deadline_ns: Option<i64>,
        expr: &HirExpr,
    ) -> Option<Place> {
        let listener_place = self.lower_value(listener)?;

        // When a deadline is active, `expr.ty` is `Result<Connection, NetError>` (set
        // by HIR). The raw `Connection` slot is the `result_dest`; codegen wraps it
        // into `Ok(_)` or binds `Err(NetError::TimedOut)` into `deadline_result_dest`.
        // Without a deadline, `expr.ty` is `Connection` directly.
        let conn_ty = if deadline_ns.is_some() {
            // Extract the Ok arm type (Connection) from Result<Connection, NetError>.
            match &expr.ty {
                hew_types::ResolvedTy::Named { args, .. } if !args.is_empty() => {
                    self.subst_ty(&args[0])
                }
                other => self.subst_ty(other),
            }
        } else {
            self.subst_ty(&expr.ty)
        };

        // The `Connection` slot the accept binds (the SuspendingAccept
        // `result_dest` / the blocking `hew_tcp_accept` return).
        let conn_dest = self.alloc_local(conn_ty);

        if self.current_function_call_conv.carries_execution_context() {
            let deadline_result_dest =
                deadline_ns.map(|_| self.alloc_local(self.subst_ty(&expr.ty)));
            let error_dest = deadline_ns.map(|_| {
                self.alloc_local(hew_types::ResolvedTy::Named {
                    name: "NetError".to_string(),
                    args: Vec::new(),
                    builtin: None,
                    is_opaque: false,
                })
            });
            let next = self.alloc_block();
            if let Some(ns) = deadline_ns {
                self.await_deadline_ns.insert(self.current_block_id, ns);
            }
            // `SuspendingAccept` carries no separate MIR cleanup block — it rides
            // the multi-suspend epilogue, so `cleanup` reuses `next` (exactly as
            // `SuspendingRead` does).
            self.record_suspend_kind(SuspendKind::Accept {
                listener: listener_place,
                result_dest: conn_dest,
                deadline_result_dest,
                error_dest,
            });
            self.finish_current_block(Terminator::Suspend {
                resume: next,
                cleanup: next,
                is_final: false,
            });
            self.start_block(next);
            if let Some(result_dest) = deadline_result_dest {
                return Some(result_dest);
            }
        } else {
            if deadline_ns.is_some() {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "`await ln.accept() | after d` in a non-suspendable context"
                            .to_string(),
                        site: expr.site,
                    },
                    note: "accept deadlines require a suspendable actor/closure/task context; \
                           default-call-convention functions have no parkable continuation to \
                           resume on timeout"
                        .to_string(),
                });
                return None;
            }
            // Default callers use the blocking accept FFI: they run on a
            // foreign/main thread with no parkable continuation.
            let next = self.alloc_block();
            self.finish_current_block(Terminator::Call {
                callee: "hew_tcp_accept".to_string(),
                builtin: None,
                args: vec![listener_place],
                dest: Some(conn_dest),
                next,
            });
            self.start_block(next);
        }

        Some(conn_dest)
    }
}
