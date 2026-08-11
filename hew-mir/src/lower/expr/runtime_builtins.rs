use super::{
    Builder, Instr, MirDiagnostic, MirDiagnosticKind, Place, ResolvedTy, RuntimeCallContext,
    Terminator,
};

impl Builder {
    #[expect(
        clippy::too_many_lines,
        reason = "runtime-call lowering is the single typed authority dispatch boundary"
    )]
    pub(super) fn lower_runtime_call(
        &mut self,
        symbol: &str,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
        result_ty: Option<&ResolvedTy>,
    ) -> Option<Place> {
        // Construction-time contract: the symbol must be in the allowlist.
        // This is the HIR-string-boundary gate: the caller dispatched this
        // symbol from a `BindingRef` name, so we assert in all build profiles
        // that it is known before we dispatch to a symbol-specific arm.
        // `RuntimeCall::new` enforces the same invariant at the MIR data level;
        // this assert defends the dispatch table (LESSONS `boundary-fail-closed`).
        assert!(
            crate::runtime_symbols::is_known_runtime_symbol(symbol),
            "lower_runtime_call called with unrecognised symbol `{symbol}`; \
             the call site must gate on is_known_runtime_symbol first"
        );

        match symbol {
            "hew_duplex_pair" => self.lower_duplex_pair(hir_args, site),
            "hew_duplex_send" => self.lower_duplex_send(hir_args, site, context, result_ty),
            "hew_duplex_close" => self.lower_duplex_close(hir_args, site, context, result_ty),
            "hew_duplex_send_half" | "hew_duplex_recv_half" => {
                self.lower_duplex_half_extract(symbol, hir_args, site, result_ty)
            }
            "hew_send_half_send" | "hew_send_half_try_send" => {
                self.lower_half_send(symbol, hir_args, site, context, result_ty)
            }
            "hew_recv_half_recv"
            | "hew_recv_half_try_recv"
            | "hew_duplex_recv"
            | "hew_duplex_try_recv" => {
                self.lower_duplex_recv(symbol, hir_args, site, context, result_ty)
            }
            "hew_duplex_close_half" => self.lower_half_close(hir_args, site, context, result_ty),
            "hew_supervisor_stop" => self.lower_supervisor_stop(hir_args, site),
            "hew_actor_link" | "hew_actor_monitor" => {
                self.lower_actor_link_or_monitor(symbol, hir_args, site, context, result_ty)
            }
            // `hew_actor_demonitor(ref_id: i64) -> void`: cancels a monitor.
            // The auto-drop path for a MonitorRef value (scope-exit, the common
            // case) goes through RuntimeDropDescriptor::MonitorRefClose →
            // lower_drop_runtime (struct-field extraction in llvm.rs), NOT this
            // arm. This arm lowers the DIRECT call in the body of
            // `impl MonitorRef { fn close(self) { hew_actor_demonitor(self.ref_id) } }`
            // (std/link_monitor.hew): a program that `import std::link_monitor`s
            // lowers that inherent `close` body, whose `unsafe` block calls the
            // symbol directly with a plain i64 `ref_id`. Returns void; a
            // value-needed context is fail-closed in the helper.
            "hew_actor_demonitor" => {
                self.lower_simple_void_runtime_call(symbol, hir_args, site, context)
            }
            "hew_actor_unlink" => self.lower_actor_unlink(hir_args, site, context),
            "hew_bytes_push" => self.lower_bytes_push(hir_args, site, context),
            "hew_vec_len" => self.lower_bytes_len(hir_args, site, context),
            "hew_bytes_pop" => self.lower_bytes_pop(hir_args, site, context),
            "hew_bytes_set" => self.lower_bytes_set(hir_args, site, context),
            "hew_bytes_is_empty" => self.lower_bytes_is_empty(hir_args, site, context),
            "hew_bytes_contains" => self.lower_bytes_contains(hir_args, site, context),
            "hew_bytes_clear" => self.lower_bytes_clear(hir_args, site, context),
            "hew_bytes_append" => self.lower_bytes_append(hir_args, site, context),
            "hew_bytes_get" => self.lower_bytes_get_option(hir_args, site, context, result_ty),
            "hew_string_get" => self.lower_string_get_option(hir_args, site, context, result_ty),
            // Sentinel-wrapping string inspectors: the runtime returns `-1`
            // for miss/OOB; codegen intercepts the callee and materialises
            // `None` / `Some(...)` (D46 sentinel -> Option sweep).
            "hew_string_find" => self.lower_string_sentinel_option(
                hew_types::runtime_call::RuntimeCallFamily::StringFind,
                hir_args,
                site,
                context,
                result_ty,
            ),
            "hew_string_char_at" => self.lower_string_sentinel_option(
                hew_types::runtime_call::RuntimeCallFamily::StringCharAt,
                hir_args,
                site,
                context,
                result_ty,
            ),
            "hew_string_char_at_utf8" => self.lower_string_sentinel_option(
                hew_types::runtime_call::RuntimeCallFamily::StringCharAtUtf8,
                hir_args,
                site,
                context,
                result_ty,
            ),
            "hew_string_char_count" => self.lower_string_char_count(hir_args, site, context),
            // Cross-node monitor extern surface. Value-position
            // `monitor(RemotePid)` routes through `lower_node_monitor`.
            "hew_node_monitor_location" => {
                self.lower_simple_int_runtime_call(symbol, hir_args, site, context, result_ty)
            }
            // Cross-node link: `link_remote(RemotePid<T>, PartitionPolicy)`
            // establishes a cross-node link and returns `Result<(), LinkError>`.
            // The remote target has no `HewActor*` in this address space, so it
            // routes to the node-link ABI keyed by the exact Location + the policy
            // discriminant; the linking subject (self) is resolved inside
            // the runtime. Unlike `monitor(RemotePid)` (which is dispatched out of
            // `hew_actor_monitor` by the RemotePid receiver type), `link_remote`
            // is its own builtin that always reaches the cross-node form.
            "hew_node_link_remote_location" => {
                self.lower_node_link_remote(hir_args, site, context, result_ty)
            }
            "hew_observe_read_u64"
            | "hew_observe_scrape"
            | "hew_observe_series"
            | "hew_observe_barrier" => {
                self.lower_observe_runtime_call(symbol, hir_args, site, context)
            }
            "hew_metric_counter_register"
            | "hew_metric_counter_inc"
            | "hew_metric_counter_add"
            | "hew_metric_gauge_register"
            | "hew_metric_gauge_set"
            | "hew_metric_gauge_inc"
            | "hew_metric_gauge_dec"
            | "hew_metric_gauge_add"
            | "hew_metric_histogram_register_simple"
            | "hew_metric_histogram_record" => {
                self.lower_metric_runtime_call(symbol, hir_args, site, context)
            }
            "hew_duration_nanos"
            | "hew_duration_micros"
            | "hew_duration_millis"
            | "hew_duration_secs"
            | "hew_duration_mins"
            | "hew_duration_hours"
            | "hew_duration_abs"
            | "hew_duration_is_zero" => {
                self.lower_duration_runtime_call(symbol, hir_args, site, context)
            }
            "hew_instant_now" | "hew_instant_elapsed" | "hew_instant_duration_since" => {
                self.lower_instant_runtime_call(symbol, hir_args, site, context)
            }
            _ => {
                // Known-allowlisted symbol but no producer arm yet.  Fail closed
                // so the pipeline rejects the program before codegen runs.
                // Individual symbol producers land in follow-up slices (recv,
                // half-handle split, close, lambda-actor lifecycle).
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!("runtime call `{symbol}`"),
                        site,
                    },
                    note: format!(
                        "`{symbol}` is a recognised runtime symbol but has no \
                         MIR producer arm yet; wired per-symbol in follow-up slices"
                    ),
                });
                None
            }
        }
    }

    /// Materialise the `()` result for a bytes mutator (`push`/`set`/`clear`/
    /// `append`) used in value/match-arm position. The op already emitted its
    /// side-effecting `push_runtime_call`; the write-back and drop accounting
    /// are identical to statement position. In value position the caller binds
    /// a unit, so allocate a fresh zero-sized Unit and define it; in statement
    /// position the result is discarded and we return `None`.
    fn lower_bytes_unit_result(&mut self, context: RuntimeCallContext) -> Option<Place> {
        if context != RuntimeCallContext::ValueNeeded {
            return None;
        }
        let dest = self.alloc_local(ResolvedTy::Unit);
        self.push_instr(Instr::UnitLit { dest });
        Some(dest)
    }

    fn lower_bytes_push(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
    ) -> Option<Place> {
        if hir_args.len() != 2 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_bytes_push` arity".to_string(),
                    site,
                },
                note: format!(
                    "`hew_bytes_push` expects a bytes receiver and one byte argument, got {}",
                    hir_args.len()
                ),
            });
            return None;
        }
        let bytes = self.lower_value(&hir_args[0])?;
        let byte = self.lower_value(&hir_args[1])?;
        self.push_runtime_call("hew_bytes_push", vec![bytes, byte], None);
        self.lower_bytes_unit_result(context)
    }

    /// Emit `hew_bytes_pop(&mut BytesTriple) -> i64` for `bytes.pop()`, then
    /// narrow the ABI result to Hew's checker-authored `u8` result type.
    ///
    /// Returns the popped byte as a u8 dest when a value is needed; codegen
    /// passes the receiver alloca address so the runtime writes back the
    /// shrunken triple. An empty buffer fails closed in the runtime (the spec
    /// `pop` signature has no Option). The receiver is BORROWED — listed in
    /// bytes-receiver contract, so it keeps its scope-exit drop.
    fn lower_bytes_pop(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
    ) -> Option<Place> {
        if hir_args.len() != 1 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_bytes_pop` arity".to_string(),
                    site,
                },
                note: format!(
                    "`hew_bytes_pop` (bytes.pop) expects 1 argument (receiver), got {}",
                    hir_args.len()
                ),
            });
            return None;
        }
        let buf = self.lower_value(&hir_args[0])?;
        if context != RuntimeCallContext::ValueNeeded {
            self.push_runtime_call("hew_bytes_pop", vec![buf], None);
            return None;
        }
        let abi_result = self.alloc_local(ResolvedTy::I64);
        self.push_runtime_call("hew_bytes_pop", vec![buf], Some(abi_result));
        let result = self.alloc_local(ResolvedTy::U8);
        self.push_instr(Instr::NumericCast {
            dest: result,
            src: abi_result,
            from_ty: ResolvedTy::I64,
            to_ty: ResolvedTy::U8,
        });
        Some(result)
    }

    /// Emit `hew_bytes_set(&mut BytesTriple, index, byte)` for `bytes.set(i, b)`.
    ///
    /// Statement-position mutation: codegen passes the receiver alloca address
    /// (write-back after `CoW`) plus the i64 index and the byte. An
    /// out-of-range index fails closed in the runtime. The receiver is
    /// BORROWED.
    fn lower_bytes_set(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
    ) -> Option<Place> {
        if hir_args.len() != 3 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_bytes_set` arity".to_string(),
                    site,
                },
                note: format!(
                    "`hew_bytes_set` (bytes.set) expects 3 arguments (receiver, index, byte), \
                     got {}",
                    hir_args.len()
                ),
            });
            return None;
        }
        let buf = self.lower_value(&hir_args[0])?;
        let idx = self.lower_value(&hir_args[1])?;
        let byte = self.lower_value(&hir_args[2])?;
        self.push_runtime_call("hew_bytes_set", vec![buf, idx, byte], None);
        self.lower_bytes_unit_result(context)
    }

    /// Emit `hew_bytes_is_empty(*const BytesTriple) -> bool` for
    /// `bytes.is_empty()`. Pure read; the receiver is BORROWED.
    fn lower_bytes_is_empty(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
    ) -> Option<Place> {
        if hir_args.len() != 1 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_bytes_is_empty` arity".to_string(),
                    site,
                },
                note: format!(
                    "`hew_bytes_is_empty` (bytes.is_empty) expects 1 argument (receiver), got {}",
                    hir_args.len()
                ),
            });
            return None;
        }
        let buf = self.lower_value(&hir_args[0])?;
        let dest = (context == RuntimeCallContext::ValueNeeded)
            .then(|| self.alloc_local(ResolvedTy::Bool));
        self.push_runtime_call("hew_bytes_is_empty", vec![buf], dest);
        dest
    }

    /// Emit `hew_bytes_contains(*const BytesTriple, byte) -> bool` for
    /// `bytes.contains(b)`. Pure read; the receiver is BORROWED.
    fn lower_bytes_contains(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
    ) -> Option<Place> {
        if hir_args.len() != 2 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_bytes_contains` arity".to_string(),
                    site,
                },
                note: format!(
                    "`hew_bytes_contains` (bytes.contains) expects 2 arguments (receiver, byte), \
                     got {}",
                    hir_args.len()
                ),
            });
            return None;
        }
        let buf = self.lower_value(&hir_args[0])?;
        let byte = self.lower_value(&hir_args[1])?;
        let dest = (context == RuntimeCallContext::ValueNeeded)
            .then(|| self.alloc_local(ResolvedTy::Bool));
        self.push_runtime_call("hew_bytes_contains", vec![buf, byte], dest);
        dest
    }

    /// Emit `hew_bytes_clear(&mut BytesTriple)` for `bytes.clear()`.
    ///
    /// Statement-position in-place reset; codegen passes the receiver alloca
    /// address so the runtime releases the buffer ref and writes back the empty
    /// triple. The receiver is BORROWED (clear releases its OWN reference and
    /// leaves the binding owning a null triple whose scope-exit drop is inert).
    fn lower_bytes_clear(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
    ) -> Option<Place> {
        if hir_args.len() != 1 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_bytes_clear` arity".to_string(),
                    site,
                },
                note: format!(
                    "`hew_bytes_clear` (bytes.clear) expects 1 argument (receiver), got {}",
                    hir_args.len()
                ),
            });
            return None;
        }
        let buf = self.lower_value(&hir_args[0])?;
        self.push_runtime_call("hew_bytes_clear", vec![buf], None);
        self.lower_bytes_unit_result(context)
    }

    /// Emit `hew_bytes_append(&mut dst, ...)` for `bytes.append(other)`.
    ///
    /// Statement-position mutation. MIR carries the two `bytes` places
    /// `[dst, other]`; codegen passes the dst alloca address (write-back) and
    /// unpacks `other` into the scalar `(src_ptr, src_offset, src_len)` runtime
    /// args. Both operands are BORROWED — `hew_bytes_append` copies the source
    /// region and never takes its reference (see
    /// the bytes-all-args contract, so `other` keeps its scope-exit drop.
    fn lower_bytes_append(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
    ) -> Option<Place> {
        if hir_args.len() != 2 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_bytes_append` arity".to_string(),
                    site,
                },
                note: format!(
                    "`hew_bytes_append` (bytes.append) expects 2 arguments (receiver, other), \
                     got {}",
                    hir_args.len()
                ),
            });
            return None;
        }
        let dst = self.lower_value(&hir_args[0])?;
        let other = self.lower_value(&hir_args[1])?;
        self.push_runtime_call("hew_bytes_append", vec![dst, other], None);
        self.lower_bytes_unit_result(context)
    }

    /// Emit `hew_vec_len(buf) -> i64` for `bytes.len()` calls.
    ///
    /// The `impl bytes` extern block in `std/io.hew` declares `len` with
    /// `#[extern_symbol(hew_vec_len)]`. At MIR time the callee name is
    /// already `hew_vec_len` (allowlisted), so it routes here rather than
    /// through the for-in-loop path that uses `hew_vec_len` directly.
    /// ABI: 1 arg (bytes receiver, passed as a `*mut HewVec`), returns `i64`.
    fn lower_bytes_len(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
    ) -> Option<Place> {
        if hir_args.len() != 1 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_vec_len` arity".to_string(),
                    site,
                },
                note: format!(
                    "`hew_vec_len` (bytes.len) expects 1 argument (receiver), got {}",
                    hir_args.len()
                ),
            });
            return None;
        }
        let buf = self.lower_value(&hir_args[0])?;
        let dest =
            (context == RuntimeCallContext::ValueNeeded).then(|| self.alloc_local(ResolvedTy::I64));
        self.push_runtime_call("hew_vec_len", vec![buf], dest);
        dest
    }

    /// Emit `bytes.get(index) -> Option<u8>`, the non-trapping byte accessor.
    ///
    /// De-aliased from the trapping `b[i]` sugar (`hew_bytes_index`, which
    /// aborts on OOB): `.get` returns `None` out of bounds instead of trapping.
    /// Mirrors the Vec/HashMap `.get` shape — a single `Terminator::Call` to a
    /// codegen-intercepted symbol (`hew_bytes_get`) that owns the bounds-check
    /// CFG and the `Some`/`None` materialisation. The symbol carries no runtime
    /// export (`builtin: None`, like `hew_vec_get_clone`): codegen does the
    /// check over the stack-resident `BytesTriple` and an in-bounds typed load.
    ///
    /// The receiver is BORROWED, not consumed — `hew_bytes_get` carries the
    /// collection-receiver contract, so `buf` keeps its scope-exit drop. The
    /// `u8` element is a scalar (Copy): the `Some` payload is a
    /// by-value load with no owned clone, so drop-safety is trivial.
    fn lower_bytes_get_option(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
        result_ty: Option<&ResolvedTy>,
    ) -> Option<Place> {
        if hir_args.len() != 2 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_bytes_get` arity".to_string(),
                    site,
                },
                note: format!(
                    "`hew_bytes_get` (bytes.get) expects 2 arguments (receiver, index), got {}",
                    hir_args.len()
                ),
            });
            return None;
        }
        // The checker types `b.get(i)` as `Option<u8>`; size the dest enum slot
        // with that exact type so codegen resolves the registered Option layout
        // (`checker-authority`: consume the recorded type, never re-infer it).
        let Some(opt_ty) = result_ty else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_bytes_get` result type".to_string(),
                    site,
                },
                note: "`hew_bytes_get` (bytes.get) needs the checker-recorded \
                       `Option<u8>` result type to size its dest slot"
                    .to_string(),
            });
            return None;
        };
        let buf = self.lower_value(&hir_args[0])?;
        let idx = self.lower_value(&hir_args[1])?;
        // Always materialise the Option; the bounds-check CFG lives in codegen.
        // A discarded result is a dead local the optimiser elides, but the Call
        // terminator still needs a dest + a `next` block to continue into.
        let result = self.alloc_local(opt_ty.clone());
        let next = self.alloc_block();
        self.finish_current_block(Terminator::Call {
            callee: "hew_bytes_get".to_string(),
            authority: crate::CallAuthority::Runtime(
                hew_types::runtime_call::RuntimeCallFamily::BytesGet,
            ),
            args: vec![buf, idx],
            dest: Some(result),
            next,
        });
        self.start_block(next);
        let _ = context;
        Some(result)
    }

    /// Lower `string.get(index) -> Option<char>` to a single `Terminator::Call`
    /// to the codegen-intercepted `hew_string_get` symbol.
    ///
    /// Mirrors the bytes `.get` shape — the symbol carries no runtime export
    /// (`builtin: None`): codegen bounds-checks the index against
    /// `hew_string_char_count` and materialises `Some(char)` / `None` over the
    /// in-bounds `hew_string_index` codepoint load.
    ///
    /// The receiver is BORROWED, not consumed — `hew_string_get` carries the
    /// collection-receiver contract, so `s` keeps its scope-exit drop. The
    /// `char` element is a scalar (Copy): the `Some` payload is a by-value
    /// codepoint with no owned clone, so drop-safety is trivial.
    fn lower_string_get_option(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
        result_ty: Option<&ResolvedTy>,
    ) -> Option<Place> {
        if hir_args.len() != 2 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_string_get` arity".to_string(),
                    site,
                },
                note: format!(
                    "`hew_string_get` (string.get) expects 2 arguments (receiver, index), got {}",
                    hir_args.len()
                ),
            });
            return None;
        }
        // The checker types `s.get(i)` as `Option<char>`; size the dest enum slot
        // with that exact type so codegen resolves the registered Option layout
        // (`checker-authority`: consume the recorded type, never re-infer it).
        let Some(opt_ty) = result_ty else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_string_get` result type".to_string(),
                    site,
                },
                note: "`hew_string_get` (string.get) needs the checker-recorded \
                       `Option<char>` result type to size its dest slot"
                    .to_string(),
            });
            return None;
        };
        let s = self.lower_value(&hir_args[0])?;
        let idx = self.lower_value(&hir_args[1])?;
        // Always materialise the Option; the bounds-check CFG lives in codegen.
        // A discarded result is a dead local the optimiser elides, but the Call
        // terminator still needs a dest + a `next` block to continue into.
        let result = self.alloc_local(opt_ty.clone());
        let next = self.alloc_block();
        self.finish_current_block(Terminator::Call {
            callee: "hew_string_get".to_string(),
            authority: crate::CallAuthority::Runtime(
                hew_types::runtime_call::RuntimeCallFamily::StringGet,
            ),
            args: vec![s, idx],
            dest: Some(result),
            next,
        });
        self.start_block(next);
        let _ = context;
        Some(result)
    }

    /// Lower a sentinel-wrapping string inspector (`string.find(needle)`,
    /// `string.char_at(i)`, `string.codepoint_at_utf8(i)`) to a single
    /// `Terminator::Call` to the codegen-intercepted runtime symbol.
    ///
    /// Mirrors the `string.get` shape: the checker records the `Option<...>`
    /// result type (`Option<i64>` for find/codepoint, `Option<char>` for
    /// `char_at`); codegen calls the real runtime entry (which keeps its `-1`
    /// miss/OOB sentinel at the C ABI) and materialises `Some(value)` /
    /// `None` from the sign of the result (D46 sentinel -> Option sweep).
    ///
    /// The receiver and needle are BORROWED (string-inspector contract); the
    /// `Some` payload is a scalar (Copy), so drop-safety is trivial.
    fn lower_string_sentinel_option(
        &mut self,
        family: hew_types::runtime_call::RuntimeCallFamily,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
        result_ty: Option<&ResolvedTy>,
    ) -> Option<Place> {
        let symbol = family.c_symbol();
        if hir_args.len() != 2 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("runtime call `{symbol}` arity"),
                    site,
                },
                note: format!(
                    "`{symbol}` expects 2 arguments (receiver, needle/index), got {}",
                    hir_args.len()
                ),
            });
            return None;
        }
        // The checker types the call as `Option<...>`; size the dest enum slot
        // with that exact type so codegen resolves the registered Option layout
        // (`checker-authority`: consume the recorded type, never re-infer it).
        let Some(opt_ty) = result_ty else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("runtime call `{symbol}` result type"),
                    site,
                },
                note: format!(
                    "`{symbol}` needs the checker-recorded `Option<...>` result \
                     type to size its dest slot"
                ),
            });
            return None;
        };
        let s = self.lower_value(&hir_args[0])?;
        let arg = self.lower_value(&hir_args[1])?;
        // Always materialise the Option; the sentinel-branch CFG lives in
        // codegen. A discarded result is a dead local the optimiser elides,
        // but the Call terminator still needs a dest + a `next` block.
        let result = self.alloc_local(opt_ty.clone());
        let next = self.alloc_block();
        self.finish_current_block(Terminator::Call {
            callee: symbol.to_string(),
            authority: crate::CallAuthority::Runtime(family),
            args: vec![s, arg],
            dest: Some(result),
            next,
        });
        self.start_block(next);
        let _ = context;
        Some(result)
    }

    /// Emit `hew_string_char_count(s) -> i32`, widened to the Hew-facing `i64`.
    /// The runtime ABI returns i32, while the stdlib-facing
    /// `string.char_count_utf8()` declaration returns i64. Keep the call ABI
    /// honest by storing the runtime result in an i32 temporary and inserting
    /// the same explicit `NumericCast` used by open-end string slicing.
    fn lower_string_char_count(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
    ) -> Option<Place> {
        if hir_args.len() != 1 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_string_char_count` arity".to_string(),
                    site,
                },
                note: format!(
                    "`hew_string_char_count` expects 1 argument (receiver), got {}",
                    hir_args.len()
                ),
            });
            return None;
        }

        let s = self.lower_value(&hir_args[0])?;
        if context != RuntimeCallContext::ValueNeeded {
            self.push_runtime_call("hew_string_char_count", vec![s], None);
            return None;
        }

        let count_i32 = self.alloc_local(ResolvedTy::I32);
        self.push_runtime_call("hew_string_char_count", vec![s], Some(count_i32));
        let count_i64 = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::NumericCast {
            dest: count_i64,
            src: count_i32,
            from_ty: ResolvedTy::I32,
            to_ty: ResolvedTy::I64,
        });
        Some(count_i64)
    }

    /// Emit `hew_string_concat(lhs, rhs) -> string` for the typed runtime
    /// call route used by f-string interpolation. Binary string `+` reaches
    /// the same runtime through `lower_binary`; this arm closes the separate
    /// `CallTarget::Runtime(StringConcat)` producer path without recovering a
    /// callee from its source spelling.
    pub(super) fn lower_string_concat_runtime_call(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
        result_ty: Option<&ResolvedTy>,
    ) -> Option<Place> {
        if hir_args.len() != 2 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_string_concat` arity".to_string(),
                    site,
                },
                note: format!(
                    "`hew_string_concat` expects 2 string arguments, got {}",
                    hir_args.len()
                ),
            });
            return None;
        }
        if !matches!(result_ty, Some(ResolvedTy::String)) {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_string_concat` result type".to_string(),
                    site,
                },
                note: "`hew_string_concat` requires the checker-recorded string result type"
                    .to_string(),
            });
            return None;
        }
        let lhs = self.lower_value(&hir_args[0])?;
        let rhs = self.lower_value(&hir_args[1])?;
        let dest = self.alloc_local(ResolvedTy::String);
        self.push_runtime_call("hew_string_concat", vec![lhs, rhs], Some(dest));
        let _ = context;
        Some(dest)
    }

    /// Lower the `impl duration` receiver methods declared in
    /// `std/builtins.hew` (`#[extern_symbol(hew_duration_*)]`).
    ///
    /// Every symbol takes a single i64-backed `duration` receiver. The
    /// conversion/predicate symbols return `i64`; `hew_duration_is_zero`
    /// returns the C `i32` boolean (`1`/`0`) that codegen narrows to `i1` at
    /// the call boundary, with no explicit cast.
    fn lower_duration_runtime_call(
        &mut self,
        symbol: &str,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
    ) -> Option<Place> {
        if hir_args.len() != 1 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("runtime call `{symbol}` arity"),
                    site,
                },
                note: format!(
                    "`{symbol}` expects 1 argument (the duration receiver), got {}",
                    hir_args.len()
                ),
            });
            return None;
        }

        let return_ty = if symbol == "hew_duration_is_zero" {
            ResolvedTy::Bool
        } else {
            ResolvedTy::I64
        };

        let receiver = self.lower_value(&hir_args[0])?;
        let dest =
            (context == RuntimeCallContext::ValueNeeded).then(|| self.alloc_local(return_ty));
        self.push_runtime_call(symbol, vec![receiver], dest);
        dest
    }
    /// Synthesize the current actor's own handle via the `hew_actor_self()`
    /// runtime primitive and return the `Place` holding it.
    ///
    /// The implicit ABI-only callers do not dispatch through the pid's actor
    /// protocol, so their result uses the erased `LocalPid<Unit>` tag. A
    /// value-position `this` must instead call [`Self::emit_actor_self_handle_typed`]
    /// with the checker/HIR-provided `LocalPid<ConcreteActor>` type: downstream
    /// consumers such as `conn.attach(this)` use that tag to resolve the actor's
    /// protocol descriptor and message ids.
    pub(crate) fn emit_actor_self_handle(&mut self) -> Place {
        self.emit_actor_self_handle_typed(&ResolvedTy::Named {
            name: hew_types::BuiltinType::LocalPid
                .canonical_name()
                .to_string(),
            args: vec![ResolvedTy::Unit],
            builtin: Some(hew_types::BuiltinType::LocalPid),
            is_opaque: false,
        })
    }

    /// Synthesize a value-position actor self handle while preserving its
    /// checker-authoritative concrete `LocalPid<Actor>` tag.
    pub(super) fn emit_actor_self_handle_typed(&mut self, self_ty: &ResolvedTy) -> Place {
        let self_handle = self.alloc_local(self_ty.clone());
        self.push_runtime_call("hew_actor_self", vec![], Some(self_handle));
        self_handle
    }

    /// Lower the `impl instant` methods declared in `std/builtins.hew`
    /// (`#[extern_symbol(hew_instant_*)]`).
    ///
    /// `instant` is i64-backed (a monotonic nanosecond timestamp), so every
    /// argument and result is a bare `i64`:
    /// - `hew_instant_now()` -> `i64` (no receiver; reads the monotonic clock).
    /// - `hew_instant_elapsed(now: i64)` -> `i64` (a `duration` in ns).
    /// - `hew_instant_duration_since(now: i64, earlier: i64)` -> `i64`.
    ///
    /// The arity is derived from the symbol so a malformed call fails closed
    /// before codegen rather than silently mis-marshalling the ABI.
    fn lower_instant_runtime_call(
        &mut self,
        symbol: &str,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
    ) -> Option<Place> {
        let expected_arity = match symbol {
            "hew_instant_now" => 0,
            "hew_instant_elapsed" => 1,
            "hew_instant_duration_since" => 2,
            _ => unreachable!("instant lowering called for non-instant symbol `{symbol}`"),
        };
        if hir_args.len() != expected_arity {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("runtime call `{symbol}` arity"),
                    site,
                },
                note: format!(
                    "`{symbol}` expects {expected_arity} argument(s), got {}",
                    hir_args.len()
                ),
            });
            return None;
        }

        let mut arg_places = Vec::with_capacity(hir_args.len());
        for arg in hir_args {
            arg_places.push(self.lower_value(arg)?);
        }
        let dest =
            (context == RuntimeCallContext::ValueNeeded).then(|| self.alloc_local(ResolvedTy::I64));
        self.push_runtime_call(symbol, arg_places, dest);
        dest
    }
}
