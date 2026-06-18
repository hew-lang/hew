//! Runtime-ABI symbol allowlist for `Instr::CallRuntimeAbi`.
//!
//! `Instr::CallRuntimeAbi` carries a `String` symbol naming a
//! `hew_*` C-ABI entry in `hew-runtime/`. Accepting any string at
//! construction time would invite typos that survive to link-time
//! (or worse, silently route to a wrong runtime entry). Per
//! HEW-SPEC §3.7 boundary-fail-closed and LESSONS row P0
//! `boundary-fail-closed` (49), the producer validates every
//! symbol against this allowlist BEFORE the `Instr` enters the
//! `BasicBlock::instructions` stream. A symbol absent from the
//! allowlist surfaces as a hard `MirDiagnostic` so the failure
//! lands at MIR construction, not at codegen link-time.
//!
//! Source of truth: `scripts/jit-symbol-classification.toml`'s
//! `stable` and `codegen-stable` lists. The full toml is parsed by
//! `hew-runtime`'s build script — `hew-mir` carries the M2-substrate
//! subset inline so the allowlist check does not require a build-time
//! fixture or parsing step. When a new runtime-ABI symbol becomes
//! producer-emittable from MIR, add it to both lists in the same change
//! (`stable` if user-callable via `extern "rt"`, `codegen-stable` if
//! emitted only by the compiler).
//! The drift-test in `tests/runtime_symbols_classification.rs` —
//! TODO when a producer for a non-substrate symbol lands — would
//! cross-verify the two lists; for now the substrate list is
//! short enough to maintain by hand.
//!
//! WHY (M2 slice 4.5c shim): the typecheck→MIR bridge that maps
//! `Duplex<S, R>::send(msg)` (a `MethodCallRewrite` side-table entry
//! in `hew-types`) to a free-function call on a runtime ABI symbol
//! does not yet reach the Rust MIR pipeline (`hew compile`).
//! Producers in `hew-mir` therefore have no callsite today; the
//! allowlist + the `Instr::CallRuntimeAbi` variant land first so
//! slice 5 codegen (LLVM IR emission) has a target to wire and
//! the producer-side bridge work that lights up callers can land
//! in a follow-up slice without retrofitting the `Instr` enum.
//! WHEN-OBSOLETE: when the typecheck→HIR/MIR bridge lands (a
//! 4.5b-follow-up slice owning the bridge decision), the unit
//! tests in this module become the discovery surface for "did the
//! bridge wire every symbol on the allowlist". WHAT: a real
//! consumer of `is_known_runtime_symbol` from the HIR-to-MIR
//! Call-lowering arm in `hew-mir/src/lower.rs`.

/// M2-substrate runtime-ABI symbols that an `Instr::CallRuntimeAbi`
/// may name. Sorted lexicographically for stable diffs and
/// binary-searchable membership.
///
/// Each entry is a `#[no_mangle] extern "C" fn` exported by
/// `hew-runtime/src/duplex.rs` (or the lambda-actor sibling
/// module). The full set in `scripts/jit-symbol-classification.toml`
/// `stable` is broader (per-actor / per-mailbox / per-IO entries);
/// the substrate subset that MIR producers can emit today is the
/// list below.
// Lexicographically sorted: `hew_actor_*` < `hew_bytes_*` < `hew_duplex_*`
// < `hew_lambda_actor_*` < `hew_recv_half_*` < `hew_regex_*`
// < `hew_reply_channel_*` < `hew_send_half_*`.
// Section comments mark the substrate-grouping for readability; the binary-search
// invariant is over the flat ordering.
const MIR_EMITTER_RUNTIME_SYMBOLS: &[&str] = &[
    // --- Actor cooperate/link/monitor surface -------------------------------
    "hew_actor_ask",
    // `hew_actor_ask_with_channel(actor, msg_type, data, size, ch) -> i32`
    // (`hew-runtime/src/actor.rs:3259`). Sends a request with a caller-
    // provided reply channel. Returns 0 (HewError::Ok) on success;
    // non-zero indicates failure. Used by `Terminator::Select`
    // codegen to issue per-arm asks before `hew_select_first` decides
    // the winner.
    "hew_actor_ask_with_channel",
    // `hew_actor_cooperate() -> c_int` — reduction-budget safepoint injected
    // by codegen at Checked MIR cooperate sites. Implemented by both native
    // and WASM schedulers.
    "hew_actor_cooperate",
    // `hew_actor_link(parent, child)` — bidirectional link; void return. The
    // Hew `link()` builtin wraps the call in `Ok(())` unconditionally because
    // the current runtime does not surface AlreadyLinked as a return code.
    // Codegen composite-return synthesis (Result<(), LinkError>) requires the
    // Cluster 2 spine; the codegen arm is a tracked-gap shim until that lands.
    "hew_actor_link",
    // `hew_actor_monitor(watcher, target) -> u64` — returns a ref_id. Dead
    // targets return immediately with a DOWN signal; ref_id is still non-zero.
    // Codegen struct-wrapping (MonitorRef { ref_id }) requires Cluster 2 spine.
    "hew_actor_monitor",
    // `hew_actor_self() -> *mut HewActor` (`hew-runtime/src/actor.rs:3862`).
    // Returns the actor installed in the current dispatch context, or null
    // when called outside one. The MIR producer synthesizes this as ABI
    // arg0 (the implicit `self` subject) for the 1-arg `link`/`monitor`
    // builtins; the borrowed handle carries no drop obligation.
    "hew_actor_self",
    "hew_actor_send_by_id",
    "hew_actor_spawn",
    // `hew_actor_unlink(a, b)` — removes the bidirectional process link
    // between `a` and `b`. The Hew user-facing name is `unlink(target)`;
    // the linking subject is the implicit calling actor (`self`), mirroring
    // the `link` ABI. The 2-arg runtime ABI is satisfied by synthesizing
    // `hew_actor_self()` as arg0 and the user target as arg1. Implemented
    // at `hew-runtime/src/link.rs:148`.
    "hew_actor_unlink",
    // --- Auto-injected mutex substrate  -----------------
    // `hew_auto_mutex_alloc() -> *mut HewAutoMutex`
    // (`hew-runtime/src/auto_mutex.rs`). Allocates one opaque mutex
    // handle; the compiler emits one call per populated
    // `ClosureEnvLayout::lock_slot_for` slot at closure-env /
    // generator-state materialisation. Compiler-only emission per
    // Per substrate decision ( no user-visible `Mutex<T>`).
    "hew_auto_mutex_alloc",
    // `hew_auto_mutex_free(mtx: *mut HewAutoMutex)` — frees one
    // handle. Idempotent on null. The compiler emits this once per
    // `_alloc` at env destructor time; the LIFO drop stream guarantees
    // every `unlock` precedes the matching `free` for the same handle
    // (release-before-free invariant).
    "hew_auto_mutex_free",
    // `hew_auto_mutex_lock(mtx: *mut HewAutoMutex)` — acquire. The
    // compiler emits this immediately BEFORE each cross-suspend access
    // of the shared capture; suspend points themselves sit OUTSIDE
    // the bracket (avoids classic async-mutex deadlock).
    "hew_auto_mutex_lock",
    // `hew_auto_mutex_unlock(mtx: *mut HewAutoMutex)` — release. The
    // compiler emits this immediately AFTER the access completes.
    "hew_auto_mutex_unlock",
    // --- Bytes value index/slice substrate (W3 collections-sugar S2) --------
    // `hew_bytes_index(ptr, offset, len, index) -> u8`
    //   (`hew-runtime/src/bytes.rs`). O(1) byte load over a (ptr,offset,len)
    //   `BytesTriple`. Aborts on negative index or index >= len. Emitted by
    //   the MIR producer arm for `b[i]` over `Ty::Bytes` receivers.
    "hew_bytes_index",
    // `hew_bytes_len(triple: *const BytesTriple) -> i64`
    //   (`hew-runtime/src/bytes.rs`). Reads the logical byte length from the
    //   stack-resident triple. Emitted for open-end bytes ranges `b[a..]` /
    //   `b[..]` so MIR materialises the end bound before calling
    //   `hew_bytes_slice`.
    "hew_bytes_len",
    // `hew_bytes_push(&mut BytesTriple, byte: u8)` appends one byte, updating
    // the caller's stack-resident triple after CoW/growth. Emitted for
    // `bytes.push(i32)` receiver methods; codegen truncates the element to u8.
    "hew_bytes_push",
    // `hew_bytes_slice(ptr, offset, len, start, end) -> BytesTriple`
    //   (`hew-runtime/src/bytes.rs`). O(1) byte-range slice that bumps the
    //   underlying refcount for non-empty results (empty slice returns a
    //   null/0/0 triple). Aborts on invalid bounds. Emitted for `b[a..b]`.
    "hew_bytes_slice",
    // --- Cancellation-token retain/release (ABI pin) -------------
    // `hew_cancel_token_is_requested(token: *mut HewCancellationToken) -> bool`
    // (`hew-runtime/src/task_scope.rs:272`). Non-blocking poll: returns
    // true if the token's cancel flag has been set. Generator cancel-poll
    // and `CancellationToken.is_cancelled()` observation both emit this
    // symbol; the observation call borrows the token and does not release it.
    "hew_cancel_token_is_requested",
    // `hew_cancel_token_release(token: *mut HewCancellationToken) -> void`
    // (`hew-runtime/src/task_scope.rs`). Decrements the token's refcount;
    // frees when it reaches zero, releasing the parent reference recursively.
    "hew_cancel_token_release",
    // `hew_cancel_token_retain(token: *mut HewCancellationToken) -> void`
    // (`hew-runtime/src/task_scope.rs`). Increments the token's refcount.
    // Null-safe.
    "hew_cancel_token_retain",
    // --- Duplex<S, R> dual-queue substrate ----------------------
    "hew_duplex_clone",
    "hew_duplex_close",
    "hew_duplex_close_half",
    "hew_duplex_pair",
    "hew_duplex_payload_free",
    "hew_duplex_recv",
    "hew_duplex_recv_half",
    "hew_duplex_send",
    "hew_duplex_send_half",
    "hew_duplex_try_recv",
    "hew_duplex_try_send",
    // --- Monomorphic time canaries ------------------------------------------
    "hew_duration_abs",
    "hew_duration_hours",
    "hew_duration_is_zero",
    "hew_duration_micros",
    "hew_duration_millis",
    "hew_duration_mins",
    "hew_duration_nanos",
    "hew_duration_secs",
    // --- Trait-object heap-box storage ABI (W3.031 Stage 0) -----
    // `hew_dyn_box_alloc(size: usize, align: usize) -> *mut u8`
    // `hew_dyn_box_free(ptr: *mut u8, size: usize, align: usize)`
    // (`hew-runtime/src/trait_object.rs`). Heap storage for
    // return-by-value `dyn Trait` values: the callee allocates,
    // memcpy's the concrete value in, and returns the fat pointer;
    // the receiving `DropKind::TraitObject { storage: HeapBoxed }`
    // ritual runs vtable slot 0 then frees the buffer (size/align
    // sourced from vtable prefix slots 1 and 2). Both entries fail
    // closed on align==0 / null ptr / invalid Layout — see the
    // module doc in trait_object.rs for the convention. Companion
    // dispatch diagnostic `hew_vtable_dispatch_panic_on_oob` lives
    // further down this list under its own `hew_v*` block; the
    // surfaces share a lane plan (W3.031 §1.7.3) but the allowlist
    // is sorted lex for binary-search correctness.
    "hew_dyn_box_alloc",
    "hew_dyn_box_free",
    // --- Layout-backed HashMap surface (W3.003 C-1b) ------------
    // Variable-stride open-addressing map keyed by opaque blobs whose
    // identity is delegated to caller-supplied hash/eq thunks. Sibling
    // of the string-keyed `hew_hashmap_*_impl` entries in
    // `scripts/jit-symbol-classification.toml`; codegen for the layout
    // path lands in C-3 and will reach `Instr::CallRuntimeAbi` then.
    "hew_hashmap_contains_key_layout",
    "hew_hashmap_free_layout",
    "hew_hashmap_get_layout",
    "hew_hashmap_insert_layout",
    "hew_hashmap_len_layout",
    "hew_hashmap_new_with_layout",
    "hew_hashmap_remove_layout",
    // --- Layout-backed HashSet surface (W3.003 C-1c) -------------
    // Thin wrapper over the layout HashMap that fixes val_layout to the
    // ZST (size=0, align=1) marker. All probe/hash/eq work is delegated
    // to the C-1b ABI above; codegen for the layout path lands in C-3.
    "hew_hashset_contains_layout",
    "hew_hashset_free_layout",
    "hew_hashset_insert_layout",
    "hew_hashset_is_empty_layout",
    "hew_hashset_len_layout",
    "hew_hashset_new_with_layout",
    "hew_hashset_remove_layout",
    "hew_instant_duration_since",
    "hew_instant_elapsed",
    "hew_instant_now",
    // --- Lambda-actor surface (overlays Duplex<Msg, Reply>) -----
    "hew_lambda_actor_ask",
    "hew_lambda_actor_clone",
    "hew_lambda_actor_downgrade",
    "hew_lambda_actor_new",
    "hew_lambda_actor_release",
    "hew_lambda_actor_send",
    "hew_lambda_actor_weak_clone",
    "hew_lambda_actor_weak_drop",
    "hew_lambda_actor_weak_send",
    // hew_lambda_body_alloc_reply_buf: ALLOC counterpart to the
    // internal `free_body_reply_buf`; called by compiler-emitted body
    // fns when materialising an ask-shape reply payload. Single
    // `usize` arg → `*mut u8`. Allocator-paired with the runtime's
    // `Box::from_raw` free; never libc-tracked (the tracker guards
    // against accidental libc::malloc reaching `free_body_reply_buf`).
    "hew_lambda_body_alloc_reply_buf",
    // hew_lambda_drain_all: codegen-emitted at `main` exit so a
    // non-actor-using program that spawned lambda actors still drains
    // their dispatch threads before the process exits (the existing
    // `hew_shutdown_wait` waits only for scheduler workers; lambda
    // actors run on dedicated OS threads outside that pool). Single
    // `i64` arg (timeout_ms, 0 = 5 s default) → `i32` (0 = clean
    // drain, 1 = timed out). Always safe to call: returns immediately
    // when no lambda actors have ever been spawned.
    "hew_lambda_drain_all",
    // --- user metrics (#1862) ------------------------------------------------
    // std::metrics emit path: register-or-get a counter/gauge/histogram (and
    // their labelled `*Vec` forms), then mutate by integer handle. Symbols are
    // declared via `extern "C"` in `std/metrics.hew` (the semaphore/observe
    // model) and are also producer-emittable through this allowlist so codegen
    // may lower the calls directly. Bodies live in `hew-runtime/src/metrics.rs`.
    "hew_metric_counter_add",
    "hew_metric_counter_inc",
    "hew_metric_counter_register",
    "hew_metric_gauge_add",
    "hew_metric_gauge_dec",
    "hew_metric_gauge_inc",
    "hew_metric_gauge_register",
    "hew_metric_gauge_set",
    "hew_metric_histogram_record",
    "hew_metric_histogram_register",
    "hew_metric_histogram_register_simple",
    "hew_metric_vec_register",
    "hew_metric_vec_with",
    // --- end user metrics (#1862) --------------------------------------------
    // --- Observe read surface ------------------------------------------------
    "hew_observe_barrier",
    "hew_observe_read_u64",
    "hew_observe_scrape",
    "hew_observe_series",
    // --- Rc allocation for task-owned closure environments --------------------
    "hew_rc_new",
    // --- RecvHalf<T> ---------------------------------------------
    "hew_recv_half_recv",
    "hew_recv_half_try_recv",
    // --- Regex runtime ABI (slice 4 allowlist; codegen/FFI wired in slice 5) -
    // `hew_regex_capture(scrutinee: *const u8, literal_id: i64, capture_idx: i64)
    //   -> *mut u8` — returns a heap-allocated (strdup-style) NUL-terminated
    //   C string for the capture group at `capture_idx`, or null if the group
    //   did not participate in the match. MIR lowering emits this for each named
    //   capture in a regex arm; the null check drives a branch to the next arm
    //   (fail-closed: missing capture ≠ empty string).
    //   WHY not `hew_regex_match` returns captures: keeping match and extraction
    //   separate lets codegen materialise only the captures the arm body actually
    //   reads. WHEN-OBSOLETE: if the runtime gains a single-call
    //   match-and-capture-all API the producer would switch; the MIR shape
    //   (one CallRuntimeAbi per capture + null check) would still be correct.
    //   WHAT: `hew-runtime/src/regex.rs` `extern "C" fn hew_regex_capture` (slice 5).
    "hew_regex_capture",
    // `hew_regex_compile(pattern: *const u8, len: i64) -> *mut HewRegex` — called
    //   from module-init (slice 5) to compile each pattern and store the handle
    //   in the corresponding global slot. Allowlisted here (slice 4) so the MIR
    //   module-init emission in slice 5 can validate the symbol at construction.
    //   MIR lowering in `lower_match` does NOT emit this call; the lit-id i64
    //   constant is the indirection that slice 5 resolves to the global handle.
    //   WHY separate from `hew_regex_match`: compile once at module init, match
    //   many times at call sites. WHEN-OBSOLETE: never for the compile/match
    //   split — the split is substrate-correct.
    "hew_regex_compile",
    // `hew_regex_free_capture(ptr: *mut u8) -> void` — frees a capture string
    //   returned by `hew_regex_capture`. MIR lowering emits this call at arm-body
    //   exit (for each non-null capture that was extracted on the success path) and
    //   on the null-fail paths when earlier captures were already allocated before a
    //   later capture returned null. Mirrors `libc::free` but isolates the alloc ABI.
    //   WHY a wrapper: if the runtime's string allocator changes, only this wrapper
    //   needs updating, not codegen. WHEN-OBSOLETE: if MIR gains a typed
    //   `Instr::CStringDrop` the producer switches to that and this symbol is retired.
    //   SHIM: body-exit free only covers straight-line arm bodies; bodies with early
    //   returns or trap paths would leak. Real fix requires scope-exit cleanup
    //   primitives in MIR (v0.6 substrate lane).
    "hew_regex_free_capture",
    // `hew_regex_match(scrutinee: *const u8, literal_id: i64) -> i32` — returns
    //   1 if the pattern for `literal_id` matches, 0 otherwise. `literal_id`
    //   is the 0-based index into the module's regex-literal global array; the
    //   runtime resolves it to the compiled `*HewRegex` handle and calls
    //   `regex_is_match`. Returning i32 (not bool/i1) to match the C ABI
    //   convention used by other predicate-returning runtime entries.
    //   WHY id-keyed not handle-keyed: MIR does not yet have a `Place::RegexHandle`
    //   primitive; the id-to-handle resolution inside the runtime is the slice-5
    //   concern (module-init global array). WHEN-OBSOLETE: if MIR gains a global
    //   slot place the producer would pass the handle directly and the id indirection
    //   would be removed. WHAT: `hew-runtime/src/regex.rs` (slice 5).
    "hew_regex_match",
    // --- Reply channel surface (select{} actor-ask arm) ----------
    // `hew_reply_channel_cancel(ch) -> void`
    // (`hew-runtime/src/reply_channel.rs:440`). Marks a reply channel
    // cancelled so a late replier observes the flag and releases its
    // sender-side ref without UAF. Codegen invokes this on every
    // loser arm of a `Terminator::Select` BEFORE freeing the channel
    // (cancel-then-free is the Risk R4 ordering invariant).
    "hew_reply_channel_cancel",
    // `hew_reply_channel_free(ch) -> void`
    // (`hew-runtime/src/reply_channel.rs:409`). Releases one reference;
    // frees the channel when refcount reaches zero. Symmetric with
    // `hew_reply_channel_new`.
    "hew_reply_channel_free",
    // `hew_reply_channel_new() -> *mut HewReplyChannel`
    // (`hew-runtime/src/reply_channel.rs:78`). Allocates a fresh
    // single-shot reply channel with one caller-side reference.
    "hew_reply_channel_new",
    // hew_reply_payload_free: libc free for ask reply payloads
    // delivered through `hew_lambda_actor_ask` / `hew_reply_wait`.
    // Codegen calls this on the ask call-site reply_out slot after
    // decoding the payload into the user's dest. Allocator-paired
    // with `alloc_reply_buffer` (libc::malloc) inside the reply
    // channel.
    "hew_reply_payload_free",
    // `hew_reply_wait(ch) -> *mut c_void`
    // (`hew-runtime/src/reply_channel.rs:296`). Blocks until the reply
    // arrives; returns the reply pointer (caller frees with libc::free)
    // or null on orphaned-ask. Does NOT consume the channel ref.
    "hew_reply_wait",
    // --- Select winner-picker -----------------------------------
    // `hew_select_first(channels, count, timeout_ms) -> i32`
    // (`hew-runtime/src/reply_channel.rs:484`). Polls multiple reply
    // channels; returns the index of the first ready channel, or -1
    // on timeout. `timeout_ms < 0` waits indefinitely. Codegen
    // marshals the per-arm channel array + AfterTimer duration into
    // this call.
    "hew_select_first",
    // --- SendHalf<T> ---------------------------------------------
    "hew_send_half_send",
    "hew_send_half_try_send",
    // --- String char-count/concat/codepoint index/slice substrate ------------
    // `hew_string_char_count(s) -> i32` (`hew-runtime/src/string.rs`).
    //   Counts UTF-8 codepoints. Emitted for open-end string ranges `s[a..]` /
    //   `s[..]`; MIR widens the i32 result to i64 before passing it to
    //   `hew_string_slice_codepoints`.
    "hew_string_char_count",
    // `hew_string_concat(a, b) -> *mut c_char` (`hew-runtime/src/string.rs`).
    //   Fresh owned concatenation result. Emitted for `string + string`;
    //   drop-safety follows the existing `String` value-class discipline.
    "hew_string_concat",
    // `hew_string_index(s, i) -> i32` (`hew-runtime/src/string.rs`).
    //   Codepoint at codepoint offset i; O(n). Aborts on null / invalid
    //   UTF-8 / negative / OOB. NO -1 sentinel. Emitted by the MIR
    //   producer arm for `s[i]` over `Ty::String` receivers.
    "hew_string_index",
    // `hew_string_slice_codepoints(s, start, end) -> *mut c_char`
    //   (`hew-runtime/src/string.rs`). Fresh malloc'd codepoint slice
    //   [start, end). Aborts on null / invalid UTF-8 / negative /
    //   start>end / end>char_count. Emitted for `s[a..b]`. Disjoint
    //   from the input pointer (drop-safety: fresh allocation).
    "hew_string_slice_codepoints",
    // --- Supervisor static-child slot lookup ---------------------------------
    // `hew_supervisor_child_get(sup: *mut HewSupervisor, key: u32) -> ChildLookupResult`
    // (`hew-runtime/src/supervisor.rs`). Non-blocking typed slot lookup for
    // static (non-pool) supervisor children. Returns a 16-byte discriminated
    // result: tag 0=Live (handle non-null), 1=Transient, 2=Dead. The MIR
    // producer arm for dotted-access lowering is deferred until the
    // `Instr::CallRuntimeAbi` emitter shape is established.
    "hew_supervisor_child_get",
    // `hew_supervisor_nested_get(sup: *mut HewSupervisor, key: u32) -> ChildLookupResult`
    // (`hew-runtime/src/supervisor.rs`). Same as hew_supervisor_child_get but
    // over child_supervisors; the handle field carries a *mut HewSupervisor
    // bit-pattern for multi-segment dotted access (`app.api.auth`).
    "hew_supervisor_nested_get",
    // `hew_supervisor_stop(sup: *mut HewSupervisor) -> void`
    // (`hew-runtime/src/supervisor.rs:1944`). Graceful shutdown: requests
    // shutdown of all children and initiates teardown. Void return — the
    // Hew `supervisor_stop(sup)` builtin discards the result. Requires
    // the native preemptive scheduler; WASM-excluded (same family rule as
    // `hew_supervisor_child_get`).
    "hew_supervisor_stop",
    // --- Task ABI (scope{}/spawn/await) — Phase 2, rows 2/3/4 ----------------
    // `hew_task_await_blocking(task: *mut HewTask) -> *mut c_void`
    // (`hew-runtime/src/task_scope.rs:411`). Blocks the calling thread until
    // the task completes, then returns the result pointer (or null if no
    // result). Needed for `await task` (row 4).
    "hew_task_await_blocking",
    "hew_task_complete_threaded",
    "hew_task_completion_observe",
    "hew_task_completion_unobserve",
    // `hew_task_free(task: *mut HewTask) -> void`
    // (`hew-runtime/src/task_scope.rs:237`). Frees a Box-allocated HewTask
    // and its result buffer. Called by the scope teardown path and by the
    // await-sequence after consuming the result. Part of row 4.
    "hew_task_free",
    "hew_task_get_env",
    "hew_task_get_error",
    // `hew_task_get_result(task: *mut HewTask) -> *mut c_void`
    // (`hew-runtime/src/task_scope.rs:283`). Returns the task's result
    // pointer if done, null otherwise. Must be called after
    // `hew_task_await_blocking` which guarantees the task is done.
    // Part of row 4.
    "hew_task_get_result",
    // `hew_task_new() -> *mut HewTask`
    // (`hew-runtime/src/task_scope.rs:214`). Box-allocates and returns a
    // new HewTask in the Ready state with all fields zeroed/null. Needed
    // for spawned calls (row 3) — producer calls this before
    // `hew_task_spawn_thread`.
    "hew_task_new",
    "hew_task_scope_cancel_after_ns",
    "hew_task_scope_destroy",
    "hew_task_scope_join_all",
    "hew_task_scope_new",
    "hew_task_scope_set_current",
    "hew_task_scope_spawn",
    "hew_task_set_env",
    // `hew_task_set_result(task: *mut HewTask, result: *mut c_void, size: usize)
    //  -> void` (`hew-runtime/src/task_scope.rs`). Deep-copies the value
    // representation into a task-owned malloc buffer. Emitted by the codegen
    // task wrapper to publish a value-returning task's body result before
    // `hew_task_complete_threaded`; the awaiter reads it via
    // `hew_task_get_result` on the resume edge. Part of value-task await.
    "hew_task_set_result",
    // `hew_task_spawn_thread(task: *mut HewTask, task_fn: TaskFn) -> void`
    // (`hew-runtime/src/task_scope.rs:368`). Spawns `task_fn(task)` on a
    // new OS thread. `TaskFn = unsafe extern "C" fn(*mut HewTask)`. The
    // function pointer arg has no precedent in the current Place/arg-load
    // pattern; the codegen arm is fail-closed with a SHIM comment naming
    // the producer-contract decision. Needed for spawned calls (row 3).
    "hew_task_spawn_thread",
    // --- Vec<T> indexing (C-2) ----------------------------------
    // hew_vec_get_T(v: *mut HewVec, index: i64) -> T — one per element type.
    // Element types supported: bool, i32, i64, f64, ptr (handle/opaque), str (returns
    // a retained/header-aware owner; caller balances with hew_string_drop).
    // Vec<String> for-in lowering emits hew_vec_get_str only when the retained
    // per-iteration owner is paired with an explicit hew_string_drop.
    // hew_vec_get_layout(v, index, layout) -> *const c_void — layout-descriptor
    // path for BitCopy Named records and tuples; codegen reads the element back
    // through the dest-place type. Emitted by subscript (xs[i]) and for-in
    // lowering for value-record element types.
    // Lexicographic note: hew_vec_get_* < hew_vec_len (g < l).
    "hew_vec_get_bool",
    "hew_vec_get_f32",
    "hew_vec_get_f64",
    "hew_vec_get_i16",
    "hew_vec_get_i32",
    "hew_vec_get_i64",
    "hew_vec_get_i8",
    "hew_vec_get_layout",
    // W5.016 owned-element borrow getter: returns a borrowed pointer into the
    // live buffer for an owned (non-Copy) record/enum/tuple element. The for-in
    // / index getter routes owned elements here instead of `hew_vec_get_layout`
    // (which aborts on an owned descriptor) or `hew_vec_get_ptr` (8-byte stride).
    "hew_vec_get_owned",
    "hew_vec_get_ptr",
    "hew_vec_get_str",
    "hew_vec_get_u16",
    "hew_vec_get_u8",
    // hew_vec_len(v: *mut HewVec) -> i64
    "hew_vec_len",
    // --- Vec<T> range-slice (C-3) -------------------------------
    // hew_vec_slice_range_T(v, start, end) -> *mut HewVec<T> — allocates
    // a fresh Vec<T> populated from [start, end) on `v`. The MIR emitter
    // bounds-checks `start <= end` and `end <= len(v)` with trap-on-OOB
    // before calling; runtime defends-in-depth with the same checks.
    // Element types covered: i32, i64, f64, ptr (handle/opaque), str
    // (header-aware copy per element; result vec owns and releases via
    // existing ElemKind::String free-on-drop path in hew_vec_free).
    // Lexicographic note: hew_vec_len < hew_vec_slice_range_* (l < s).
    "hew_vec_slice_range_bytesize",
    "hew_vec_slice_range_f64",
    "hew_vec_slice_range_i32",
    "hew_vec_slice_range_i64",
    "hew_vec_slice_range_ptr",
    "hew_vec_slice_range_str",
    // --- Trait-object dispatch diagnostics (TO-1) ---------------
    // `hew_vtable_dispatch_panic_on_oob(slot: u32, max: u32) -> !`
    // (`hew-runtime/src/trait_object.rs`). Diagnostic trap codegen
    // wires on the unreachable arm of a vtable-slot match (LESSONS
    // P0 `exhaustive-coverage`: no wildcard fallthrough in dispatch).
    // Also the wire target for a null-vtable fail-closed. Lane plan
    // `runtime-trait-object-abi.md` design D-4 rejects routing the
    // *actual* dispatch through a runtime helper — codegen emits
    // GEP+load+call inline — so this is the only trait-object symbol
    // the runtime exposes today; per-trait dispatch sites name it
    // only on the OOB / null-vtable arms.
    "hew_vtable_dispatch_panic_on_oob",
];

/// Error returned when a `RuntimeCall` is constructed with a symbol that
/// is not in the M2 runtime-ABI allowlist.
///
/// Carrying the rejected symbol string lets callers emit diagnostics that
/// name the exact offender (`MirDiagnosticKind::NotYetImplemented`,
/// codegen assertions, unit-test assertions).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnknownRuntimeSymbol(pub String);

impl std::fmt::Display for UnknownRuntimeSymbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "symbol `{}` is not in the M2 runtime-ABI allowlist \
             (see `runtime_symbols::MIR_EMITTER_RUNTIME_SYMBOLS`)",
            self.0
        )
    }
}

/// Return `true` if `symbol` is a recognised runtime-ABI entry
/// `Instr::CallRuntimeAbi` may name. Binary search; the static
/// list is sorted in `MIR_EMITTER_RUNTIME_SYMBOLS`.
#[must_use]
pub fn is_known_runtime_symbol(symbol: &str) -> bool {
    MIR_EMITTER_RUNTIME_SYMBOLS.binary_search(&symbol).is_ok()
}

/// Borrow the full static allowlist. Useful for tests and dump
/// surfaces that want to enumerate the recognised symbols.
#[must_use]
pub fn known_runtime_symbols() -> &'static [&'static str] {
    MIR_EMITTER_RUNTIME_SYMBOLS
}

/// W5.011 P3 — `true` when a runtime-ABI string call hands the caller a
/// **fresh, solely-owned `string`** that must be balanced by exactly one
/// `hew_string_drop`.
///
/// Two ownership shapes qualify, and both place an identical single-drop
/// obligation on the caller (verified against `hew-runtime/src/string.rs` and
/// `vec.rs`):
///
/// - **Fresh allocation** (`hew_string_concat`, `…_to_uppercase`,
///   `hew_int_to_string`, …): the result is a brand-new `malloc_cstring` /
///   `alloc_cstring_data` buffer at refcount 1, documented "released via
///   `hew_string_drop`".
/// - **Refcounted retain** (`hew_string_clone`, `hew_vec_get_str` →
///   `retain_string_element` → `hew_string_clone`): the result aliases an
///   existing buffer with its refcount bumped by one. `hew_string_drop`
///   decrements; the buffer frees only at refcount zero. The caller still owes
///   exactly one drop to balance the `+1`.
///
/// This is the **producer** half of the borrowing-read-aware owned-string
/// cleanup. A binding whose backing local traces (through `Move`) to one of
/// these dests is a proven sole owner that may earn a scope-exit
/// `DropKind::CowHeap`, provided every use of it is a verified borrow
/// ([`is_borrowing_string_use`]). Symbols absent here are treated as
/// non-producers — the binding is not admitted, so the result leaks rather
/// than risk a double-free (fail-closed).
///
/// The match is exhaustive-by-listing with a `false` default: a new string
/// producer must be classified here explicitly (and verified to allocate or
/// retain, never to alias an input without a refcount bump) before its result
/// can be admitted to drop.
#[must_use]
pub fn is_fresh_owned_string_producer(symbol: &str) -> bool {
    matches!(
        symbol,
        // --- Fresh-allocation producers (rc == 1, released via hew_string_drop) ---
        "hew_string_concat"
            | "hew_string_to_lowercase"
            | "hew_string_to_uppercase"
            | "hew_string_trim"
            | "hew_string_replace"
            | "hew_string_slice"
            | "hew_string_slice_codepoints"
            | "hew_string_repeat"
            | "hew_string_from_char"
            | "hew_char_to_string"
            | "hew_int_to_string"
            | "hew_i64_to_string"
            | "hew_uint_to_string"
            | "hew_u64_to_string"
            | "hew_float_to_string"
            | "hew_bool_to_string"
            // --- Refcounted-retain producers (+1 owner aliasing a live buffer) ---
            | "hew_string_clone"
            | "hew_vec_get_str"
    )
}

/// W5.011 P3 — `true` when passing an owned `string` as an argument to this
/// runtime-ABI symbol is a **borrowing read**: the callee inspects or copies
/// the bytes but does NOT take ownership, so the caller retains its single
/// `hew_string_drop` obligation.
///
/// This is the **use** half of the borrowing-read-aware cleanup. A fresh-owned
/// string binding ([`is_fresh_owned_string_producer`]) is admitted to a
/// scope-exit drop only when EVERY use of it is a verified borrow; any use that
/// is not on this list (a container-insert move such as
/// `hew_hashmap_insert_layout`, a user/closure/trait call, an unrecognised
/// runtime symbol) is treated as an ownership-transferring escape and the
/// binding is excluded (leak, never double-free).
///
/// Every symbol here was verified read-only / copy-in against
/// `hew-runtime/src/string.rs` and `vec.rs`:
/// - the scalar/`Vec`/`bytes`-returning inspectors (`hew_string_length`,
///   `…_contains`, `…_find`, `…_to_bytes`, `…_split`, …) only read the input;
/// - the `string`-returning transforms (`hew_string_concat`, `…_to_uppercase`,
///   `hew_string_clone`, …) copy / refcount-bump the input and leave the
///   argument's ownership with the caller;
/// - `hew_vec_push_str` stores an independent `copy_string_element_in` copy, so
///   the pushed argument is borrowed (the caller still owns and drops it).
///
/// Container-move sinks (`hew_hashmap_insert_layout`, `hew_hashset_insert*`)
/// are deliberately ABSENT: they transfer ownership of the string into the
/// container, so the caller must NOT also drop it.
#[must_use]
pub fn is_borrowing_string_use(symbol: &str) -> bool {
    matches!(
        symbol,
        // --- Scalar / bytes / Vec inspectors (read-only, scalar or fresh-container
        //     return; the string argument is never retained) ---
        "hew_string_length"
            | "hew_string_char_count"
            | "hew_string_char_at"
            | "hew_string_char_at_utf8"
            | "hew_string_index"
            | "hew_string_starts_with"
            | "hew_string_ends_with"
            | "hew_string_contains"
            | "hew_string_is_empty"
            | "hew_string_is_digit"
            | "hew_string_is_alpha"
            | "hew_string_is_alphanumeric"
            | "hew_string_find"
            | "hew_string_index_of_start"
            | "hew_string_to_bytes"
            | "hew_string_split"
            | "hew_string_lines"
            | "hew_string_chars"
            // --- string-returning transforms: copy / refcount-bump the input,
            //     argument ownership stays with the caller ---
            | "hew_string_concat"
            | "hew_string_to_lowercase"
            | "hew_string_to_uppercase"
            | "hew_string_trim"
            | "hew_string_replace"
            | "hew_string_slice"
            | "hew_string_slice_codepoints"
            | "hew_string_repeat"
            | "hew_string_clone"
            // --- container copy-in (stores an independent copy; arg is borrowed) ---
            | "hew_vec_push_str"
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn allowlist_is_sorted_for_binary_search() {
        // Binary-search correctness depends on the list being sorted.
        // A future contributor adding an entry out of order would
        // silently break membership for some symbols; pin the
        // invariant.
        for window in MIR_EMITTER_RUNTIME_SYMBOLS.windows(2) {
            assert!(
                window[0] < window[1],
                "MIR_EMITTER_RUNTIME_SYMBOLS is not lexicographically sorted: \
                 {} >= {}",
                window[0],
                window[1],
            );
        }
    }

    #[test]
    fn known_substrate_symbols_recognised() {
        // Every entry in the allowlist must round-trip.
        for sym in MIR_EMITTER_RUNTIME_SYMBOLS {
            assert!(
                is_known_runtime_symbol(sym),
                "allowlist entry {sym} should be recognised",
            );
        }
    }

    #[test]
    fn unknown_symbol_rejected() {
        // A symbol the substrate does not emit must NOT be
        // recognised — typo-class bugs need to fail closed.
        assert!(!is_known_runtime_symbol("hew_duplex_sned"));
        assert!(!is_known_runtime_symbol("hew_duplex_send_t"));
        assert!(!is_known_runtime_symbol(""));
        assert!(!is_known_runtime_symbol("printf"));
    }

    #[test]
    fn substrate_quartet_present() {
        // Spot-check the four most load-bearing entries.
        // (These are the symbols a slice-5 lowering of Duplex
        // send/recv/close/pair lands first.)
        assert!(is_known_runtime_symbol("hew_duplex_pair"));
        assert!(is_known_runtime_symbol("hew_duplex_send"));
        assert!(is_known_runtime_symbol("hew_duplex_recv"));
        assert!(is_known_runtime_symbol("hew_duplex_close"));
    }

    #[test]
    fn task_abi_symbols_present() {
        // Phase 2 substrate for scope{}/spawn/await (inventory rows 2/3/4).
        // Each of these must be recognised before the MIR producer arms can
        // be wired in lower.rs. The canonical structured-concurrency surface
        // is `hew_task_scope_*` (W2.006); legacy `hew_scope_*` has been
        // removed.
        assert!(is_known_runtime_symbol("hew_task_scope_spawn"));
        assert!(is_known_runtime_symbol("hew_task_new"));
        assert!(is_known_runtime_symbol("hew_task_spawn_thread"));
        assert!(is_known_runtime_symbol("hew_task_await_blocking"));
        assert!(is_known_runtime_symbol("hew_task_get_result"));
        assert!(is_known_runtime_symbol("hew_task_free"));
    }
}
