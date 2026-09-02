# Runtime ownership table

Status: generated inventory over main `54e8dde2c` (2026-09-01), revision 6 (revision 5 with the reconciliation pass applied: sections 3 and 8 below, plus the `fork` spelling in section 2a. No row, count or generator output moves).

## When these documents disagree

Quoting `hew-orchestration/plans/final-ladder-program.md` §5.1 in full, which is the authority for this paragraph and for the identical paragraph in `ir-ladder.md` and `sir-domain-matrix.md`:

> `docs/internal/ir-ladder.md` decides SIR ops, ownership kinds, MIR forms, and
> runtime symbol names; `docs/internal/sir-domain-matrix.md` decides which
> phase owns a construct; `docs/internal/runtime-ownership-table.md` decides a
> runtime symbol's parameter and result ownership. This plan decides sequencing
> and gates. A disagreement inside a document's own domain is a defect in the
> other document, fixed in the same PR that finds it; none of the four is a
> fallback for another.

No other precedence rule is in force. This document's domain is a symbol's **parameter and result ownership**; the symbol's *name* and the ops that call it are `ir-ladder.md`'s, and the phase that needs a row is `sir-domain-matrix.md`'s. Section 6's bug list records disagreements found in either direction; a bug whose fix belongs to another document says so and that document lands it in the same PR.

**What this document is, and what the compiler can read.** It is the ownership inventory of every `hew_*` C-ABI symbol, and the place where a row is *decided*. It is NOT a compiler input: nothing projects section 9 into a crate. The only FFI ownership fact the compiler can read today is `scripts/jit-symbol-classification.toml`, which `hew-types/build.rs:52` -> `generate_ffi_ownership_table` (`:60-118`) projects into `hew_types::ffi_contracts::FFI_OWNERSHIP_CONTRACTS`, and which `ir-ladder.md` §6.4 names as the row HIR→SIR reads. **That rule is not enforced by any code on main**, and `ir-ladder.md` §6.4 (revision 7) schedules the enforcement as "a missing row is a build error **from P1**" for every symbol P1 lowers, with the `std/` `extern` declarations joining at P3 — so read the admission rule below as "ready to be written", never as "the build stops without it today"; section 2's admission paragraph carries the consequence. The ladder keeps ONE compiler-readable authority — the TOML, generated into `hew-mir/src/runtime_symbols.rs`, the one symbol table (§5.1, section 3's A4 note) — and this table is the mint queue that feeds it: **an admitted row here is a row that is ready to be written into `scripts/jit-symbol-classification.toml`, not a row the lowering can already read.** A `br` row therefore costs one TOML edit before P1 can emit its symbol, and section 8's backlog is ordered by the phase that needs the edit. Three things the TOML schema cannot yet spell, so P1 must land them in `scripts/jit-symbol-classification.toml` + `hew-types/build.rs` alongside the rows: the `copy_value` target per carrier (`k`, section 2a), the mutate-in-place receiver (`m`, B4) and the argument binding a `rel`/`k` symbol is called with when the carrier is not a single pointer (section 2a's binding column). A fourth P1 item is not a schema change but a lowering one, and it is now decided rather than open: the fresh lowering REFUSES an `ExternOwnershipFact::Absent` for every symbol P1 lowers, and the `std/` `extern` declarations join at P3 (`ir-ladder.md` §6.4 revision 7; section 2, admission rule; section 8's P1 lowering row). Nothing on main refuses one, so the refusal is P1 lane work, not a fact to cite.

This copy is the **compact profile**: the 1083 rows that are compiler-referenced, `.hew`-declared, or carry an audited TOML contract. Section 9 prints only the 518 of those whose audited contract is absent or contradicted; the 565 `au` rows of the compact profile are carried verbatim from `scripts/jit-symbol-classification.toml` and are not repeated in the fence (section 9's preamble states that exclusion). The remaining 452 rows of the 1535-row universe are in the generated full profile named in section 8. 452 is 9 fewer than section 1's "461 definitions no compiler path or `.hew` extern references", because 9 of those definitions carry an audited TOML contract and nothing else — `v = "T"`: `hew_bytes_from_str`, `hew_dist_monitor_remote_watcher_registered_total`, `hew_last_error`, `hew_mailbox_has_user_messages`, `hew_reply_channel_failure_kind`, `hew_supervisor_notify_child_supervisor_escalation`, `hew_tcp_attach`, `hew_vec_from_u8_data`, `hew_vec_new_generic` — so the TOML limb pulls them into the compact profile although no compiler path names them, and the fence excludes them because they are `au`.

Every count in this document is printed by the generator run named in section 8 and is never hand-edited; a count that disagrees with the generator is a stale document, not a fact.

**Two things the table does not cover, and what P1 does instead.** The universe below is `hew_*` names only, so the C-ABI symbols codegen emits that are NOT `hew_*` (libc `malloc`/`free`/`memset`, B18) and the symbols a user's own `extern "C" { }` block declares (B19) can never have a row. Read literally, the admission rule would ban both. It does not: the fresh emitter calls no libc symbol except the single `free` the user-extern string-adoption protocol needs (B19); every one of the 23 libc sites in today's emitter is drift (B18), and a user-declared extern is admitted on its `.hew` declaration alone under the user-extern rule in section 2. Both are decisions, not omissions; B18 and B19 carry them.

## 1. Scope and universe

The universe is the union of four sets, each produced by one command:

| set | command | count |
| --- | --- | --- |
| definitions | `#[no_mangle]` / `#[cfg_attr(.., no_mangle)]` `extern "C"`/`"C-unwind"` fns and statics under `hew-runtime/src` and `hew-std/src`, direct and macro-generated (`ffi_inventory.py scan_defs`) | 1625 definitions, 1512 unique symbols (1237 runtime + 388 std; 24 statics; 77 macro-generated; 99 cfg twins native/wasm/test) |
| compiler-referenced | every `"hew_*"` string literal in `hew-codegen-rs/src/**`, `hew-mir/src/**`, `hew-types/src/**` and `hew-hir/src/**`, outside `/tests/` paths, `*_tests.rs` files, line comments, brace-matched `#[cfg(test)] mod` blocks, and the files named by an external `#[cfg(test)] mod <name>;` declaration (`gen_doc.py scan_refs_stripped` + `external_test_mod_files`), intersected with the known-symbol universe (definitions, `.hew` externs, TOML contracts, the synthetic list) plus the hard-coded release sets `D` (`RuntimeDropDescriptor::c_symbol`) and `L` (`HeapLeaf`/`CowHeapRelease::release_symbol`) | 840 distinct literals; 644 name a known symbol (codegen 509, MIR 265, catalog 224, hew-types 301, D 9, L 8); 196 residue literals are LLVM value names (`hew_reply_call`, `hew_vec_get_layout_load`, `hew_duplex_`, ...), none of which is a definition |
| std externs | every `fn` inside `extern "C" { }` in `std/**/*.hew` with its `consume` marks (`ffi_inventory.py scan_hew`) | 491 declarations, 477 unique symbols |
| std externs, second source | `#[extern_symbol(...)]` method bindings in `std/**/*.hew` — NOT read by `scan_hew`, so these declarations carry no `H` provenance and no `consume` marks in the rows (B20) | 77 attribute sites, 76 parse, 75 distinct targets (69 monomorphic + 6 `{T}` templates) |
| audited contracts | `[[ownership.contracts]]` in `scripts/jit-symbol-classification.toml`, projected by `hew-types/build.rs` into `hew_types::ffi_contracts::FFI_OWNERSHIP_CONTRACTS` | 568 rows; tiers stable 841, stable-stdlib 312, codegen-stable 145, internal 101 (union 1399) |

Universe: **1535 rows**. Set algebra (all from the same run of `gen_doc.py`):

- compiler-referenced with an audited TOML contract: 133; without: **511**. The audited table covers the `.hew`-visible externs; the codegen-emitted runtime protocol (collections, strings, actors, reply channels, coroutines) is governed by the other authorities in section 3.
- **"Compiler-referenced" is a superset of "compiler-emitted".** A literal that is only compared against (`is_known_cow_heap_drop_symbol` at `llvm.rs:26336`, the `RuntimeCallFamily` spelling tables at `hew-types/src/runtime_call.rs:1099-1600`) or rewritten before emission (`hew_hashmap_get_layout`, row flag `rw`, finding: `lower_hashmap_get_layout_call` emits `hew_hashmap_get_clone_layout`, `hew-codegen-rs/src/llvm.rs:31614-31619`, `layout.rs:5632-5641`) is referenced and therefore has a row. The section 8 gate runs over this superset, which is the fail-closed direction: it cannot miss an emitted symbol that the scanner sees, and a referenced-but-never-emitted row costs one unused entry.
- **The scanner is a shortcut** (`gen_doc.py` comment above `scan_refs_stripped`). WHY: the compiler has no single dump of the C symbols it emits, so the set is recovered from source text. WHEN obsolete: when a compiler-side dump (`hew tool ffi-symbols` or equivalent) prints the `BuiltinLinkage` symbols (`RuntimeFfiShim`/`ToStringShim`/`StringCloneShim`/`PrintIntercept.runtime_symbol`), the `intern_runtime_decl` arms, `RuntimeCallFamily::c_symbol` + `vec_scalar_c_symbol`, `RuntimeDropDescriptor::c_symbol` and the `HeapLeaf`/`CowHeapRelease` release symbols from the compiler's own tables. WHAT the real fix is: derive the set from that dump and delete the scanner; the gate then runs over the emitted set exactly. Revision 1 used four per-file regexes (one literal per line in `intern_runtime_decl`, `symbol:` key patterns in the catalog, a preceding-context test plus a name-suffix blacklist in codegen) that dropped 34 defined symbols the compiler references: `hew_print_value` (`const PRINT_RUNTIME`, `stdlib_catalog.rs:365/472`), `hew_u8_to_string` (`tostring_entry!`, `:707`), the 26 `hew_assert_{eq,ne}_*` rows (`assert_entry!`, `:735-795`), `hew_bytes_from_static` (literal follows the helper name, `wire.rs:1172`, `llvm.rs:15049,18640`), `hew_cbor_ser_i64` (`wire.rs:595`), `hew_cbor_de_failed` (`_failed$` blacklist, `wire.rs:4391`), `hew_cont_crash_cleanup_retire` (second alternative of one arm, `runtime_abi.rs:4494`), `hew_trap_with_code` (`_code$` blacklist, `llvm.rs:8263-8267`) and `hew_dispatch_state_cleanup_begin_replace` (second alternative, `runtime_abi.rs:4501`, emitted at `llvm.rs:18411`; it has a TOML row so it is `au`). `gen_doc.py` asserts that the revision 1 union minus `hew_totally_unknown_symbol` (a test literal the old scanner admitted) is a subset of the new set and prints the difference. **Revision 3a named three `hew-types/src` files and then carried an ad-hoc probe paragraph for the rest; revision 4 makes the probe the rule.** The scan now walks all of `hew-types/src/**` and `hew-hir/src/**` (same test/comment stripping), which is what the probe was hand-simulating. That is not cosmetic: `hew-types/src` carries `hew_*` string literals naming real runtime symbols in eleven non-test files the old three-file list omitted (`check/methods.rs` 51 literals, `vec_authority.rs` 36, `extern_symbol.rs` 25, `stdlib.rs` 20, `builtin_names.rs` 19, `stdlib_loader.rs` 15, `module_registry.rs` 8, `check/registration.rs` 7, `stdlib_authority.rs` 6, `jit_symbols.rs` 4, `check/mod.rs` 3, plus `check/{expressions,dispatch}.rs` and `stdlib_authority/codegen.rs` at 2 each), and the rule that admits a symbol must cover the file its literal actually lives in, not rely on the same name appearing somewhere else. Widening moves compiler-referenced 639 -> 644, with-contract 130 -> 133, without 509 -> 511, distinct literals 835 -> 840, and the hew-types provenance letter 292 -> 301; the residue stays 196, the universe stays 1535, and **section 9 is byte-identical** (`diff` of the two `emit_compact.py` runs is empty), because the five newly admitted names are `hew_channel_new` and `hew_log_emit` (both `au`) and `hew_stream_chunks`/`hew_stream_take`/`hew_stream_collect_string` (already in the table on their `.hew` declaration). Nine rows gain the `F` provenance letter: those five plus `hew_bytes_drop`, `hew_string_drop`, `hew_channel_sender_clone`, `hew_hashmap_remove_take_layout`. `hew-hir/src/lower.rs:37892` is now inside the walked tree and contributes nothing, because the stripper removes its `#[cfg(test)]` block; **revision 5 closes the other half of that stripper.** `strip_test_mods` only cut brace-matched INLINE blocks, so a `#[cfg(test)] mod <name>;` that names a SEPARATE file left the whole file scanned as production source: it is not under `/tests/`, it is not `*_tests.rs`, and it has no inline block. `hew-codegen-rs/src/lib.rs:39-40` is the exemplar (`#[cfg(test)]` / `mod runtime_family_parity;`), and `hew-codegen-rs/src/runtime_family_parity.rs` carries 46 distinct `hew_*` literals of its own (`hew_duration_abs`, `hew_regex_handle`, `hew_metric_vec_with`, `hew_actor_unlink`, ...). `external_test_mod_files` resolves every such declaration to its file and drops it: **15 targets resolve, 5 were already excluded by the `_tests.rs` / `/test` rules, 10 are newly excluded** (`runtime_family_parity.rs`, four under `hew-mir/src/lower/composite_own/`, three under `hew-mir/src/lower/drop_plan/`, two under `hew-mir/src/lower/expr/`). **No count moves**: every literal those 10 files carry is independently named by a production file, so `refs_all` stays 840, `called` stays 644 and section 9 is byte-identical. What was wrong is the rule, not the arithmetic — a future test file added this way that named a `hew_*` symbol with no other production reference would have inflated `called` (and the without-contract count) with the regeneration gate still green, because the gate compares the generator against itself; `hew-cli/src/link.rs` stays outside the scan and is a link-time symbol probe, not an emission site. `hew-cli`, `hew-compile` and `hew-sir` remain out of scope: none emits a C call.
- definitions outside every JIT tier: 113: 24 statics (21 `hew_layout_{key,val}_*` descriptors + 3 `HEW_CIRCUIT_BREAKER_*`), 13 runtime fns that exist only under `cfg_attr(.., no_mangle)`, and 76 hew-std fns the verifier does not require in `stable-stdlib` (e.g. `hew_json_string_free`, `hew_http_request`, `hew_jwt_free`).
- std externs without a TOML contract: 11: `hew_actor_demonitor`, `hew_cron_next`, `hew_observe_barrier`, `hew_quic_stream_last_recv_timed_out`, `hew_sink_close`, `hew_sink_flush`, `hew_sink_write_bytes`, `hew_sink_write_string`, `hew_stream_chunks`, `hew_stream_lines`, `hew_stream_take`.
- definitions no compiler path or `.hew` extern references: 461 (JIT/host/test surface; rows kept in the full profile so the table stays total). `hew_actor_send` and `hew_actor_send_wire` are in this bucket: no compiler crate file contains the literal `"hew_actor_send"` (codegen sends through `hew_actor_send_aliased`/envelopes and `hew_actor_ask`); their section 4 contracts describe the runtime for the JIT/host callers.
- referenced names with no definition: 23, all compiler-synthetic or intercepted (`e = "sy"`): catalog `CalleeNameDispatchOnly` rows (`hew_bytes_get`, `hew_string_get`, `hew_regex_handle`, `hew_remote_pid_send`, `hew_tls_attach_local`, `hew_ws_attach_local`), identity-display names rewritten by `compiler_synthetic_runtime_ownership_symbol` (`hew_location_*`, `hew_remote_pid_*`, `hew_node_id_display`), MIR-only callees intercepted in codegen (`hew_supervisor_pool_get_option`, `hew_vec_append_layout`, `hew_vec_clear_layout`, `hew_vec_push`, `hew_vec_set`), and codegen-private module initialisers (`hew_module_init_*`). Under the SIR ladder these names must not reach the FFI table: they are HIR/MIR intrinsics, not C symbols. `hew_totally_unknown_symbol` (revision 1's 24th) is a `#[cfg(test)]` literal and is no longer counted. **Correction (B20):** 12 of the 23 (`hew_location_{display,incarnation,node_id,slot}`, `hew_remote_pid_{display,incarnation,location,node_id,slot}`, `hew_node_id_display`, `hew_bytes_get`, `hew_string_get`) are NOT compiler-invented names — `std/builtins.hew:240-270` declares them as `#[extern_symbol(...)]` bindings, i.e. std surface. The interception claim still holds (`hew-hir/src/stdlib_catalog.rs:143-147` `compiler_synthetic_runtime_ownership_symbol` rewrites the display names; `hew-mir/src/lower/expr.rs:100-108` maps the rest to `Kind::*` intrinsics), so `sy` is the right grade and no C symbol is emitted; what was wrong is calling their provenance synthetic. The `v` column carries no `H` for them because `scan_hew` never read the attribute form.
- **The `{T}` extern-symbol templates are an emission source with no Rust literal.** `std/builtins.hew:396-408` declares `hew_vec_{push,pop,get,set,remove_at,contains}_{T}`; `hew-types/src/extern_symbol.rs:355-393` `ExternSymbolTemplate::expand` builds the concrete name at check time from `RuntimeCallingConvention::canonical_token()`, and `hew-types/src/vec_authority.rs:422-428` `available()` admits it only if `runtime_symbols()` contains it. No `"hew_*"` literal exists for these expansions, so the section 1 claim that the scanner "cannot miss an emitted symbol that the scanner sees" holds only because every expansion coincidentally also appears as a literal in `runtime_call.rs`/`stdlib_catalog.rs`. WHY this is a shortcut: the gate reads source text, not the expander. WHEN obsolete: with the compiler-side symbol dump named in the next bullet, which must enumerate template expansions. WHAT the real fix is: have the dump call `expand` over the admitted token set so a new template or a new `canonical_token` cannot leak past the gate unnoticed.
- 61 symbols have a `#[cfg_attr(.., no_mangle)]` definition (wasm/test twins); 13 exist ONLY that way: `hew_sched_run`, `hew_wasm_emit`, `hew_wasm_free_meta_json`, `hew_wasm_outbound_len`, `hew_wasm_query_meta`, `hew_wasm_recv`, `hew_wasm_runtime_exit`, `hew_wasm_sched_enqueue`, `hew_wasm_sched_tick`, `hew_wasm_send`, `hew_wasm_sleeping_count`, `hew_wasm_tick`, `hew_wasm_timer_tick`. `scripts/verify-ffi-symbols.py:190-199` requires the literal `#[no_mangle]`, so those 13 are outside every tier; `hew_sched_run` and `hew_wasm_runtime_exit` among them are `intern_runtime_decl` arms (`hew-codegen-rs/src/runtime_abi.rs:4966,4971`).

## 2. Vocabulary

Parameter modes (one letter per parameter, in declaration order):

| code | mode | meaning at the call edge |
| --- | --- | --- |
| `b` | borrow | callee reads/copies; caller keeps its obligation |
| `c` | consume | callee takes the owner; caller's obligation is discharged on every return path. A byte copy that the callee then owns (registers a drop thunk for, or releases through the owning type's teardown) IS a consume: the heap embedded in the copied bytes lives once, in the callee (`hew_task_set_result`, `hew_rc_new`; and `hew_msg_envelope_new`, whose row is in the full profile because no compiler crate names it yet -- section 2a) |
| `r` | retain | callee keeps an additional reference for ITSELF (stores it in its own structure); caller keeps its own obligation. NOT used for refcount-bump primitives whose +1 is handed back to the caller: those are `k` rows (section 2a). **A body signal can never prove this**, so `r` is written only by an audited TOML `retain` param or by a hand-read. Revision 3a's heuristic did mint it, exactly once and wrongly: `hew_bytes_slice` (`bytes.rs:1071-1114`) bumps the buffer refcount and hands the +1 back inside the returned `BytesTriple`, and the `hew_*_clone` body-signal class printed `p="rssss"`. A lowering reading that row emits a retain the runtime does not consume, leaking one buffer reference per slice. `heur_param` now falls through to `b` on a retain signal and the row reads `p="bssss" r="R"`. Live uses of `r`: three `au` rows whose TOML says `retain` (`hew_actor_await_send_by_id` index 5, `hew_supervisor_role_ask_with_channel` index 5, `hew_supervisor_role_await_send` index 6 — each retains the caller's reply slot into the runtime's table) and the exemplar `hew_cancel_token_new_child` (`task_scope.rs:199`, `p="r"`, `br-`), whose row is in the full profile because no compiler path names it |
| `o` | owned-out | out-parameter the callee fills with a value the caller now owns (moved out of the container, no callee-side drop). Written ONLY on the success return (`hew_vec_pop_owned` returns 0 without writing, `hew_hashmap_remove_take_layout` returns `false` without writing, `hew_cron_next` writes only on status 0): the lowering branches on the result before minting the `Owned` value. On a BitCopy-typed slot (`hew_cron_next` `*mut i64`, `hew_actor_monitor` ref id, `hew_duplex_pair` handles that are `H`) the minted value is `None`-kinded: no obligation, but the success-only initialization rule still applies |
| `t` | retain-out | out-parameter filled with a retained/cloned share (caller owes one release); same success-only initialization as `o` |
| `s` | bitcopy | scalar, no obligation |
| `g` | glue | thunk / descriptor pointer (`drop_fn`, `clone_fn`, `HewVecElemLayout`, `HewMapKeyLayout`, ...) |
| `m` | mutate-in-place | caller's slot is updated in place; the callee may release the old representation (CoW fork, clear). **A `&mut T` Rust parameter is always `m`, never `b`** (revision 5 rule, `heur_param`): `b` promises the caller's value is unchanged across the call (section 7's `begin_borrow`/`end_borrow`), which a `&mut` receiver contradicts by construction. Five parameters in the 1535-row universe are `&mut`, all `&mut BytesTriple`: `hew_bytes_{append,clear,pop,push,set}` index 0 |
| `x` | conditional | consumed on one return path and not on another (bug list B3). Convention: `cabi_guard!` / null-pointer early returns are precondition violations and do NOT count as return paths (otherwise every `c` row would be `x`); a genuine failure leg does count (OOM in `deep_copy_state`, a terminal/foreign actor, a cancelled channel, a null glue pointer). **One more leg is declared a precondition violation, not a failure leg:** `hew_rc_new`/`hew_arc_new` return null without copying or dropping `data` when `alloc_layout(size, align)` returns `None` (`rc.rs:109-112`, `arc.rs:108-111`; the OOM leg is separate and traps through `handle_alloc_error` at `rc.rs:115-117`). `alloc_layout` (`rc.rs:67-74`) returns `None` only when `Layout::from_size_align`/`extend` overflows: either argument can be the cause. `normalise_align` (`rc.rs:57-63`) is `if align <= 1 { 1 } else { align.next_power_of_two() }`, and `usize::next_power_of_two` has no defined result past `2^63`: it panics in a debug build and WRAPS TO 0 in a release build, after which `Layout::from_size_align(size, 0)` fails its power-of-two check and `alloc_layout`'s `.ok()?` returns `None` (`rc.rs:67-74`). Revision 4 said the rounding made align safe; it does not. The leg is still reachable only from a payload size past `isize::MAX` or an alignment past `2^63`, and `size`/`align` are compiler-computed from the monomorphic payload layout (`abi_size_align`, `llvm.rs:16050`), which produces neither. DECISION: header+payload layout overflow is a precondition violation, so the `data` slot stays `c` and the row stays admitted. The alternative is `x`, which would un-admit `hew_rc_new` and with it every `Rc::new` in P1. Rows with `x` are not admitted |
| `?` | unproven | heuristic could not decide; not admitted |

Result: `N` none, `S` bitcopy, `F` fresh owned-out (caller owes `rel`), `R` retained share (same pointer, caller owes `rel`), `B` borrowed alias (invalidated by the next mutation of arg 0), `H` runtime-owned handle (no caller obligation), `D` static data, `O` owned-out box, `X` conditional, `?` unproven.

`O` is the ask/await result protocol: the pointer is a box holding the REPRESENTATION of a value of the call's result type. The lowering performs `load.take` of the value out of the box (the value becomes `Owned` with its own type-directed release) and then releases the box with `rel`; when `rel` is absent the box belongs to arg 0's owner and is freed by that owner's release (`hew_task_take_result`: `hew_task_free` frees the buffer as raw bytes, `task_scope.rs:679`). Rows: `hew_actor_ask`, `hew_reply_wait` (`rel = hew_reply_payload_free`), `hew_task_take_result` (no `rel`).

**Every `O` row has a null leg, and `load.take` must be dominated by a null test.** `hew_task_take_result` returns null when the task is not `Done`, when the result was never written, or when it was already consumed (`task_scope.rs:781-784`); `hew_reply_wait` returns null for a null channel (`reply_channel.rs:783-785`) and for a delivered-but-unallocated reply — `hew_reply`'s `alloc_reply_buffer` failure publishes a null value and sets `allocation_failed` (`reply_channel.rs:666-680`), which `take_ready_reply` hands back as null (`:443-455`). Rather than invent a rule, the lowering copies what codegen already does at the one site that gets this right: `suspend.rs:3519-3540` calls `hew_task_take_result`, `build_is_null`s the result and branches to the failure edge, with the comment "A successful value task must have a written, unconsumed result. A null take is therefore an invalid completion fact and routes to the same payload-free failure edge instead of exposing an uninitialized destination". So: an `O` result defines its `Owned` value on the non-null edge only; the null edge is a fail-closed edge that mints nothing and destroys nothing. For `hew_reply_wait` the null edge is reachable on OOM in production, not only on a broken precondition, so it must be a real diagnostic path until the section 5 ask-ABI change removes it. `hew_reply_payload_free` is `libc::free` plus a debug allocator-pairing assert (`reply_channel.rs:749-769`); codegen today frees the box with a direct libc `free` at seven sites (`suspend.rs:5970,6481,6903,7022,9767,11862`, `llvm.rs:33217`; B18 counts the same seven) and with the symbol at one (`runtime_abi.rs:1032`, lambda ask). The symbol is canonical; the direct frees are drift the fresh emitter does not repeat. `F` + `rel` cannot express this protocol (it would free the box and leak the value's heap), which is why the three rows carry `O`.

Trap (`tr`), written in the order `U A P T G`: `A` an UNCONDITIONAL process abort (`libc::abort`/`process::abort` in the body, or one of the runtime's always-aborting helpers: `abort_owned_descriptor_missing`, `abort_owned_thunk_missing` and their callers `owned_descriptor`/`owned_clone_fn`/`owned_drop_fn`, `vec.rs:2530-2585`; and, new in revision 4, `abort_layout_aware_operation` (`vec.rs:161-168`, `libc::abort`) with its two callers `abort_if_layout_aware` (`vec.rs:213-220`) and `ensure_cap` (`vec.rs:62-68`)); `P` `assert!`/`panic!`/`unreachable!`/`expect`/`handle_alloc_error`; `U` `extern "C-unwind"`; `T` the actor-aware bounds-trap bridge; `G` `cabi_guard!` early return; absent = none found. `tr` includes traps reached through runtime-internal helpers the body calls, not only the top-level body. **The helper list is a hand-maintained name set, and that is a shortcut.** WHY: the scanner reads one function body's text, so a trap two calls away is invisible unless the intermediate helper is named. WHEN obsolete: when the generator computes a call-graph closure over the runtime crate's private helpers instead of matching names. WHAT the real fix is: that closure, seeded from the `libc::abort` / `runtime_bounds_trap` / `panic!` sites. The set today: `abort_oob`, `abort_pop_empty`, `abort_owned_descriptor_missing`, `abort_owned_thunk_missing`, `owned_descriptor`, `owned_clone_fn`, `owned_drop_fn`, `abort_stride_mismatch`, `abort_if_crash_cleanup_finalizer_trap`, `abort_layout_aware_operation`, `abort_if_layout_aware`, `ensure_cap`, `elem_layout_witness`, the Vec constructors, and — new in revision 5 — the bytes family: `alloc_buf` / `realloc_buf` / `ensure_unique` (each ends in `libc::abort` on allocation failure, `bytes.rs:148-155`, `:227-236`, `:181-208`) for `A`, and `bytes_bounds_trap` / `bytes_index_oob_trap` / `bytes_slice_oob_trap` / `bytes_offset_overflow_trap` / `hew_bytes_abort_empty_pop` (`bytes.rs:57-62`, `:935`, `:951`, `:985`, `:410`) for `T`.

**`T` is new in revision 3a and it is not a synonym for `A`.** Revision 3 attributed `abort_oob`/`abort_pop_empty` to `libc::abort`. They do not abort: both end in `runtime_bounds_trap(HEW_TRAP_INDEX_OUT_OF_BOUNDS)` (`vec.rs:119`, `:145`), which calls `crate::supervisor::hew_trap_with_code` (`trap_code.rs:132-138`), which stamps the actor error code and `std::panic::panic_any(HewPanic)` when an actor is stamped and `current_context_can_unwind()` (`supervisor.rs:429-433`), falling back to `std::process::abort()` only outside an actor. A `T` row therefore UNWINDS through the caller's cleanup pads on the path that matters, exactly like a `U` row, and section 7's cleanup-edge rule covers `T` as well as `U`. This letter is why `hew_vec_pop_str`, `hew_vec_get_str`, `hew_vec_remove_at_str` (revision 3: `tr="U"`) and the `mt` pop rows (revision 3: `tr="UP"`, from a `abort_` prefix match against a macro body that contains no `panic!`/`assert!` — `vec_pop_primitive!` at `vec.rs:1389-1427`) now read `UT`: their only trap is the bridge. Revision 5 adds the bytes half of the same bridge: `hew_bytes_index` (`:1018` -> `bytes_index_oob_trap` `:935`) and `hew_bytes_slice` (`:1071` -> `bytes_slice_oob_trap` `:951`, `bytes_offset_overflow_trap` `:985`) and `hew_bytes_pop` (`:427` -> `hew_bytes_abort_empty_pop` `:410`) all reach `bytes_bounds_trap` (`:57-62`) -> `runtime_bounds_trap`, and printed `tr="U"` with no `T`. `hew_bytes_set` (`:462`) already read `UT` in revision 4 only because it names `runtime_bounds_trap` inline, and now reads `UAT` because `ensure_unique` is in the helper list. `tr` is not in the build-rule predicate, so no emitted edge changed when these letters were corrected; what changed is whether the promotion evidence is true.

**An empty `tr` never means "cannot trap", on ANY grade.** Revision 4 scoped this caveat to `au` rows; two other grades under-report for two different reasons, and both were live.

- On an `au` row the column is heuristic-only: an audited row takes `p`/`r`/`rel`/`tr` from the TOML and the body-signal scan, never from `verified.py` (that is the B15 precedence), and the scan cannot see a trap inside a helper it does not name — `hew_string_clone` prints `tr="-"` although `cstring_retain` aborts past `CSTRING_RC_MAX`.
- On a `bh`/`bh?` row the same helper blindness applies with no TOML to fall back on. That is what hid `A` on `hew_bytes_append` (`:581`, `alloc_buf` in its null-dst arm) and `T` on `hew_bytes_index`/`hew_bytes_slice`/`hew_bytes_pop`; the revision 5 helper list fixes these four.
- On a `br`/`br-` row the column is whatever the `verified.py` entry spelled, and an entry that omits `trap=` prints nothing. That is the `hew_bytes_push` case and it is a DIFFERENT defect from the scanner one: the scan already classified the body as `abort` (its first arm calls `alloc_buf` literally, `bytes.rs:347-352`), and the row printed no `tr` because the revision-3b hand-read entry left `trap` at its `"-"` default. Section 2's reading rule item (4) is the rule that was broken, not the scanner.

The hand-read trap survives in the full profile's `pf` and in section 4.

`g` lists glue parameter indexes as a **concatenated decimal-digit string, one digit per index, no separator**, ascending: `g="3"` is index 3 (`hew_rc_new`, `p="cssg"`), `g="13"` is indexes 1 and 3 (`hew_channel_poll`, `p="bg?g"`), `g="24"` is indexes 2 and 4 (`hew_lambda_actor_new`, `p="ssgxg"`), `g="01"` is indexes 0 and 1 (`hew_hashmap_new_with_layout`, `p="gg"`). This is a shortcut. WHY: every arity in the universe is single-digit, so no reader can confuse `"13"` for index 13 — the longest `p` over the 1535 rows is 7. WHEN obsolete: the first symbol with more than ten parameters, which the generator must then reject or the column must gain a separator. WHAT the real fix is: emit `g` as a TOML array of integers. `p` already carries `g` at each index, so `g` is a convenience view and a reader that distrusts it should read `p`. `rel` abbreviations: S=`hew_string_drop`, B=`hew_bytes_drop`, V=`hew_vec_free`, VO=`hew_vec_free_owned` (constructor provenance only; B4 makes it the same function as V), M=`hew_hashmap_free_layout`, HS=`hew_hashset_free_layout`; anything else is the release symbol spelled in full.

**`rel` describes the RESULT, never an out slot.** An `o`/`t` slot mints an SIR `Owned` value of the slot's static type, and that value's `destroy_value` is type-directed — the element glue for a container element, `hew_string_drop` for a String-kind vec whose `layout` is null, nothing for a BitCopy slot — decided by the lowering from the type, never by the row (section 7). So **no `o`/`t` row carries `rel`, and the generator asserts it**. Revision 3a instead spelled a sentinel `rel="E"` on three of the 45 `o`/`t` rows (`hew_vec_get_clone`, `hew_vec_take_owned`, `hew_vec_remove_at_owned`) and left the other 42 empty, by hand: `hew_vec_pop_owned` (`vec.rs:2926-2942`) and `hew_vec_remove_at_owned` (`vec.rs:2130-2157`) are the same move-out — memcpy the element to `out`, no drop, remove the slot — and disagreed on the column, and the admission predicate could not see it because it never checked `rel` on an out slot. The sentinel is deleted; the three rows lose it and nothing else changes, because section 7 already carried the rule. The same non-symbol sentinel is what makes the audited rows in B15 contradict their bodies (`release-symbol = "HewTypeLayout.drop_fn"`), so the TOML fix there is `result = "none"` with no release symbol, not a sentinel of its own.

Other columns: `d` first definition anchor as `file:line` under `hew-runtime/src/` (`std/` prefix = `hew-std/src/`; the full profile lists every cfg twin and `!macro` origin; for cfg twins the first definition the scanner meets is anchored); `f` flags as bug-list codes (`B2`..`B21`), `O` (the row's result is the box protocol above) or `tw` (cfg twins differ in body or signals); `k` marks a `copy_value` lowering target (section 2a): `k="retain"` is a refcount share (the +1 is the caller's new obligation), `k="clone"` a deep copy (the result is a fresh container, section 2a's SEAM row). Every `k` row must name its `rel`, because a `copy_value` that mints an obligation the row cannot spell is unusable (B21). `rw="<symbol>"` marks a MIR callee codegen rewrites to another symbol before emission (the row describes the runtime function the compiler never emits; the emitted edge is the named row).

Mapping to `scripts/jit-symbol-classification.toml` `[[ownership.contracts]]` (so the two can be merged mechanically): `b`,`s`,`g`,`o`,`t`,`m` -> `borrow`; `c` -> `consume`; `r` -> `retain`; `x` and `?` have no TOML spelling (absence is the TOML's fail-closed state). Result `F` -> `fresh` + `release-symbol` + `result-retention = transferred`, `R` -> `retained` + `shared-refcount`, `B` -> `borrowed`, `N`/`S`/`H` -> `none`. **Four axes have no TOML spelling at all** and are the schema work the status line names: `O` (the box protocol), `k` (the `copy_value` target per carrier, section 2a), `m` (a receiver whose representation the callee replaced, B4) and the **argument binding** — which projection of the Owned value a `rel` or `k` symbol is actually called with (section 2a's binding column). The fourth was unnamed until revision 5 and it is not cosmetic: `hew_bytes_from_static` returns a `BytesTriple` by value while its `rel` (`hew_bytes_drop`) takes a bare `*mut u8`, so "`destroy_value` lowers to `rel`" is uncomputable for the P1 carrier without the projection. A row that depends on any of the four cannot be minted into `scripts/jit-symbol-classification.toml` as it stands, however well it is read.

Evidence grade (`e`): `au` audited TOML row carried verbatim; `br` Rust body read under the reading rule below (proof anchor in the full profile's `pf` and in section 4); `br-` Rust body hand-read in revision 1 BEFORE the reading rule existed (proof anchor kept; NOT admitted until re-read); `hd` `.hew` declaration + body signals; `bh` body-signal heuristic (`ffi_inventory.py body_signals`: `free_cstring`/`Box::from_raw`/`libc::free` -> consume, `cstring_retain`/`hew_*_clone`/`clone_alias` -> retain, `CStr::from_ptr`/`&*p`/`from_raw_parts`/`.cast`/`.add` -> borrow, `str_to_malloc`/`alloc_cstring*`/`Box::into_raw`/`hew_*_new` -> fresh); `mt` macro template (signature shared by every invocation); `st` static; `sy` synthetic name (no C symbol). A trailing `?` marks a row with at least one unproven slot **on the `bh` grade only** — `bh` vs `bh?` is the one place the marker carries information, because both are heuristic rows and the marker says which of them the heuristic could not finish. It is not a universal reading of the grade column: the 23 `sy` rows all carry an unproven slot (`r="?"`, and `p="?"` where they have parameters) with no trailing marker, because a synthetic name has no C symbol to read and "unproven" is its permanent state, not a gap a re-read closes. Admission clause 1 blocks `sy` outright, so nothing depends on the marker there. Counts: au 565, br 63, br- 104, bh 439, bh? 269, mt 48, st 24, sy 23 (1535). `hd` has none: see admission clause 1.

**Precedence when a symbol has both an audited row and a hand-read body.** 41 symbols have a `[[ownership.contracts]]` row in `scripts/jit-symbol-classification.toml` AND an entry in `verified.py` (40 in revision 4; `hew_weak_clone_rc` is the revision 5 addition, section 2a). Revision 3's generator let the hand-read entry overwrite the grade unconditionally, which silently demoted 40 audited rows out of `au` and out of admission — including `hew_string_drop` (so every string `destroy_value` was a build error) and every `hew_vec_new_*` constructor P2 needs. DECISION: **the audited TOML row is the authority for its symbol**; a `verified.py` entry on an audited symbol is corroboration, not a replacement row. When the two AGREE through the mapping above the row keeps `au` and is admitted (38 symbols, 34 of which revision 3 had blocked). When the hand-read body CONTRADICTS the audited row the fail-closed answer is to admit neither: the row prints the runtime facts that were read, drops to `br-`, and carries `[B15]` naming the audited spelling it contradicts. Exactly three symbols land there — `hew_vec_get_clone`, `hew_vec_take_owned` (both TOML `result = "fresh"` with `release-symbol = "HewTypeLayout.drop_fn"`, which is not a symbol; the runtime returns a `bool` status and the value goes to the `out` slot) and `hew_vec_get_str` (B5). Fixing those three TOML rows is what admits them; that is a `scripts/jit-symbol-classification.toml` edit, not a table edit.

**Reading rule for `br`.** A row is `br` only when the reader (1) followed every callee that receives the pointer argument or the result (`hew_reply` -> `release_sender_ref_if_cancelled` / `publish_reply_from_sender_ref`, both ending in `hew_reply_channel_free(ch)`), (2) read the owning type's teardown for the argument's `drop_fn`/release (`hew_task_free` runs `result_drop_fn` on the bytes `hew_task_set_result` copied; `hew_rc_drop` runs `drop_fn` on the bytes `hew_rc_new` copied), (3) read every cfg twin or flagged `tw`, and (4) recorded every trap reached through helpers. Revision 1 stopped at the top-level body, and three of its sampled `br` rows were wrong in the double-free direction (`hew_reply` `b` for a consumed sender ref, `hew_task_set_result` `b` for an adopted copy, `hew_rc_new` `b` for an adopted payload; B14). Every revision 1 hand-read row therefore carries `br-` and is the re-read backlog listed in section 8; 63 rows were re-read under the rule (58 in revision 3, plus `hew_task_set_env`, `hew_task_get_env` and `hew_weak_drop_rc` in 3a, plus `hew_bytes_drop` and `hew_rc_drop` in revision 4). Revision 3 itself broke the rule once in the same direction: `hew_rc_new` was promoted to `br` with the modes of the WRONG parameter index (section 4), which is why 3a re-reads the signature as part of the rule, not only the body.

**Admission rule.** A row is **admitted** when it is complete enough to be written into `scripts/jit-symbol-classification.toml` and emitted from there. Admission is a property of the row, not a check this table performs. **It predicts no build error on main.** Revision 4 cited `ExternOwnershipFact::Absent` (`hew-types/src/ffi_contracts.rs:130`) as the error; it is not one. `extern_ownership_contract` returns `Absent` as a two-variant enum value with a `contract() -> Option<&_>` accessor and no diagnostic anywhere, and its consumers DEFAULT rather than fail: `hew-mir/src/lower/facts.rs:1295-1302` reads the contract if present and otherwise falls through to `crate::runtime_symbols::callee_ownership_contract(callee).borrows_string_call_args()` — A4, the string-keyed table section 9 replaces — and `hew-types/src/runtime_call.rs:2025-2040` states the policy in its own words: "the safe default for unrecognised cases is `false` (borrowing), preserved here by the explicit closed-set listing". `hew-types/build.rs:60-118` `generate_ffi_ownership_table` panics only on an unmapped param ownership / result ownership / discharge depth / result retention STRING; it never checks that a referenced symbol has a row, and never checks that a `release-symbol` names a symbol with a row of its own — which is how four rows can carry `release-symbol = "hew_sink_close"` (`scripts/jit-symbol-classification.toml:3037,3104,3113,4625`) while `grep -c '^symbol = "hew_sink_close"'` prints 0 (admission clause 3, B21).

**So through P1 and P2 the edges are unguarded, and that is an architect decision to place, not a table fact.** The ladder schedules the rejection at P3 (`docs/ir-ladder.md:2186-2191`, "**A missing row is a build error** [P3]") and `final-ladder-program.md` §4 puts "FFI ownership table" in the P3 row — but that P3 item is about `std/` extern DECLARATION rows (plan §1.5), while the symbols P1 emits are the runtime protocol: `hew_string_drop`, `hew_bytes_drop`, `hew_rc_drop`, the bytes and Rc mints. Nothing rejects an absent row for those in P1 or P2, so the string/bytes/Rc/collection edges of P1 and P2 lower through A3/A4 defaults exactly as they do today. **Recommendation:** the fresh lowering's own refusal on `Absent` for the symbols IT emits is a P1 deliverable, separate from the P3 std-extern sweep; if the architect leaves it at P3, this document should say plainly that P1 and P2 ship unguarded rather than imply a gate. Until one of the two lands, an admitted row here means "ready to be written into the TOML", and a missing row means "the lowering silently defaults". Three clauses, in order:

1. **Grade and slots.** `e` in {`au`, `br`}, no `?`/`x` in `p`, no `?`/`X` in `r`. Every other grade — `br-`, `bh`, `bh?`, `mt`, `st`, `sy` — is blocked. `hd` is in the vocabulary but has **zero rows in the universe**, and the reason is contingent, not structural: `gen_doc.py` does emit `hd` for a `.hew`-declared symbol with neither a TOML row nor a hand-read entry, and all 11 such symbols happen to be hand-read today (`hew_actor_demonitor`, `hew_cron_next`, `hew_observe_barrier`, `hew_quic_stream_last_recv_timed_out`, the four `hew_sink_*`, `hew_stream_{chunks,lines,take}` — section 1's "std externs without a TOML contract" list, all present in `verified.py`, ten `br-` and `hew_cron_next` `br`). A twelfth such declaration would print `hd` and would be blocked. The OTHER `hd` — a user-declared extern — is synthesized at the call site and never printed here (the carve-out below). `hd` is not admitted **for std externs** because B6 shows the `.hew` `consume` mark is wrong 17 times. Over the 1535 rows this clause admits **623**.
2. **A `k` row names its release.** A `copy_value` target mints an obligation; if the row cannot spell the release that discharges it, the lowering can emit the retain and nothing else. Revision 3a left three of its retain rows in exactly that state (`hew_bytes_clone_ref`, `hew_cancel_token_retain`, `hew_reply_channel_retain`: the release lived only in section 2a's prose column) and the old predicate could not see it, because it was written as "an `F` or `R` row with an empty `rel`" and all three are `r="N"`. All 14 `k` rows carry `rel` (13 in revision 4 plus `hew_weak_clone_rc`), so this clause removes nothing today; it is in the predicate so it cannot recur. The clause is why the lambda-actor weak carrier in section 2a's table has NO `k`: its release `hew_lambda_actor_weak_drop` is `bh` and unread, so a `k` there would mint an obligation the row cannot discharge.
3. **The release is itself admitted, transitively.** `rel` must name a row in the universe that clauses 1 and 2 admit, computed to a fixpoint. Without this clause an admitted mint could point at a blocked release: revision 3a admitted `hew_bytes_from_static` (every bytes literal, P1) with `rel="B"` while `hew_bytes_drop` was `br-`, and `hew_rc_new`/`hew_rc_clone` with `rel="hew_rc_drop"` while that row was `br-`. This is B15's `hew_string_drop` failure one level of indirection out, and it is B21. Seven rows fail it today: `hew_arc_new` and `hew_lambda_actor_clone` (releases `br-`), `hew_reply_channel_retain` (`hew_reply_channel_free` `br-`), and four `au` rows whose audited `release-symbol` is `hew_sink_close`, which has no audited row of its own (`hew_http_respond_stream`, `hew_stream_from_file_write`, `hew_stream_pair_sink`, `hew_stream_pair_sink_bytes`) — an audited-table defect in the same family as B1, fixed in `scripts/jit-symbol-classification.toml`. Also still checked on every run, and empty today: an admitted `F`/`R` row with no `rel` at all (`r in (F, R) and not rel` returns 0).

**The predicate's domain is the `hew_*` universe of section 1.** A symbol declared in a non-`std/` `extern "C" { }` block has no row in this table and never will; the lowering SYNTHESIZES one from the declaration (`borrow` per parameter, `consume` where the declaration marks it, result per the string-adoption protocol below) and that synthesized row is admitted. Nothing else may be synthesized: a `hew_*` name with no admitted row is still a build error. Promotion means reading the body under the reading rule and moving the row to `br`, then landing the TOML contract that the compiler actually reads.

The count of admitted rows is the ratchet; today it is **616 of 1535** (561 `au` + 55 `br`; clause 1 gives 623, and clauses 2-3 withdraw the seven above). Five `br` rows are blocked by clause 1 on `x`/`X`: `hew_actor_spawn_opts`, `hew_lambda_actor_new`, `hew_lambda_actor_send`, `hew_reply`, `hew_vec_pop_ptr`. For comparison the revision 1 rule (which admitted `br-` and had no release clause) would give 719 over the same rows; revision 1 printed 687 over its smaller universe. The ratchet is recomputed by the generator on every run, never carried, and **it is not a goal**: revision 4 moves it 621 -> 616 by adding a clause that withdraws rows, which is the fail-closed direction.

**Two carve-outs the admission rule needs to be honest** (revision 3 had neither; B18 and B19 carry the evidence):

- **Non-`hew_*` C symbols.** The rule the ladder ends at is "a call to a C-ABI symbol without a contract row is a build error" (`docs/ir-ladder.md` §6.4), and this table's universe is `hew_*` names only, so `malloc`/`free`/`memset` can never be minted into one. Resolution: **the fresh emitter calls no libc symbol.** Allocation and release go through runtime rows; where the P1 paths need one that is only `bh` today (`hew_alloc` `p="ss"` `r="F"` with no `rel`, `hew_dealloc` `p="?ss"`, `hew_realloc` `p="?sss"`), promoting those three is a P1 prerequisite, not an exemption. All 23 sites in today's emitter (B18) are drift. The one exception that is a real protocol, not drift, is `emit_extern_malloc_string_adoption` (`llvm.rs:22720-22724`, caller `:22811`), which releases a user extern's malloc'd C string with libc `free`; it is covered by the user-extern rule below, not by a `free` row.
- **User-declared `extern "C" { }`.** A Hew program declares its own externs (`docs/specs/HEW-SPEC-2026.md:2124` §3.9.1; 61 `.hew` files under `tests/` and `hew-cli/tests/`; `examples/borrow_marker.hew:7-13`). No row can exist for those symbols and no evidence grade applies — there is no Rust body to read. Resolution: **a user-declared extern is admitted on its declaration alone**, which is the `hd` grade, with plan §1.5's defaults (`borrow` per parameter, `consuming` where the declaration says so) and the user as the authority for their own C. This is not the std-extern `hd` that B6 blocks: B6 is about `std/` declarations that disagree with a Rust body we own and can fix, and a `std/` row is never admitted on `hd`. The result rule for a user extern is the existing codegen protocol: a `string` result is adopted into the header-aware domain and the foreign allocation is released with libc `free` (`llvm.rs:22720-22724`), i.e. `r="F"` with a release the table cannot spell — until it can, that adoption stays a codegen-owned protocol and the row says so.

## 2a. Retain targets (`copy_value` lowering)

Plan §6 makes SIR `copy_value` the only retain authority and deletes `retain_string_field_load` (B11). `copy_value %v : T` lowers to the row marked `k` for `T`'s runtime carrier; the new `Owned` value is the result pointer when `r = "R"` (same pointer, +1) or `r = "F"` (a fresh handle), and — when `r = "N"` and the primitive returns unit — a by-value copy of `%v` ITSELF, not of arg 0. In every case arg 0 is `b`: the callee keeps nothing, the +1 is the caller's new obligation.

**What arg 0 is, is the binding column, and for one carrier it is not `%v`.** Revision 4 said the `r="N"` result is "the SAME handle as arg 0", which is false for `bytes`: arg 0 of `hew_bytes_clone_ref` is `data_ptr: *mut u8` (`bytes.rs:294`), while the carrier is the three-field `BytesTriple {ptr, offset, len}` (`bytes.rs:74-81`). The retain is `hew_bytes_clone_ref(%v.ptr)` and the new `Owned` is a copy of the whole triple — same `ptr`, same `offset`, same `len`, one more buffer reference. The release is the mirror: `hew_bytes_drop(%v.ptr)` (`bytes.rs:318`), which is why `hew_bytes_from_static` can return a `BytesTriple` by value and still name `rel="B"`. Every other carrier binds the identity, so the column reads `%v` and only `bytes` reads `%v.ptr`. The `O` box protocol is the third shape: its release takes two arguments — `hew_reply_payload_free(ptr, _len)` (`reply_channel.rs:749`) — and codegen passes the box pointer and the published byte length (`runtime_abi.rs:1029-1039`, `&[reply_ptr_val, reply_len_val]`). **This axis has no TOML spelling** (section 2), so P1 lands it beside `k` and `m`. Revision 1 spelled these four ways (`p="r" r="R"` double-counted one increment as two obligations; `p="r" r="N"` had no `Owned` value for the +1 to live in).

**Every carrier below whose row is `au`, `br` or `br-` carries `k` on its row**, so the target is read from the row and not from this table: `k="retain"` for the refcount carriers, `k="clone"` for the deep-copy ones. Revision 3a marked only five, and left `string` — the P1 carrier — marked nowhere the fence could show. One carrier is still a heuristic row and therefore cannot carry `k` at all: the lambda-actor WEAK handle, last row of the table; a `k` on an unread row would claim a target the reading rule has not proved, and admission clause 1 blocks it regardless. Naming it here is the point — revision 4's table was silently missing both weak carriers while codegen clones weak handles today (`llvm.rs:16077` `Op::WeakClone`, `:19520` `StateFieldCloneKind::Weak => "hew_weak_clone_rc"`, `:21381`), so `copy_value %w : Weak<T>` had no target to lower to and the omission was invisible because `hew_weak_clone_rc` is `au` and `au` rows are excluded from the fence by rule. `where` says which profile prints the row: *fence* = section 9 of this copy, *full* = the generated full profile named in section 8 (an `au` row is never repeated in the fence, and an unreferenced row is not in the compact profile at all). *rel adm* is admission clause 3: whether the release is itself admitted.

| carrier | `copy_value` target | binding | `k` | row | where | release | rel adm |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `string` (`*mut c_char`, header-aware) | `hew_string_clone` | `%v` | retain | `b` -> `R`, `au` corroborated by a hand-read (`string.rs:1287-1298`, unmanaged pointers pass through untouched; the abort past `CSTRING_RC_MAX` is inside `cstring_retain`, `hew-cabi/src/cabi.rs:478-490`, so the printed row is `tr="-"` and the abort lives in `pf`) | full (`au`) | `hew_string_drop` (`au`) | yes |
| `bytes` (`BytesTriple`) | `hew_bytes_clone_ref` | **`%v.ptr`** | retain | `b` -> `N`, `br` (`bytes.rs:294-309`, aborts past `BYTES_RC_MAX`); the share is the same triple | fence | `hew_bytes_drop` (`br`, re-read in revision 4) | yes |
| `Rc<T>` | `hew_rc_clone` | `%v` | retain | `b` -> `R`, `br` (`rc.rs:156-165`) | fence | `hew_rc_drop` (`br`, re-read in revision 4) | yes |
| `Arc<T>` | `hew_arc_clone` | `%v` | retain | `b` -> `R`, `br-` (`arc.rs:156`) | full (unreferenced) | `hew_arc_drop` (`br-`) | no — P3 |
| reply channel | `hew_reply_channel_retain` | `%v` | retain | `b` -> `N`, `br` (`reply_channel.rs:316-325`) | fence | `hew_reply_channel_free` (`br-`) | no — B21, P4 |
| cancel token | `hew_cancel_token_retain` | `%v` | retain | `b` -> `N`, `br-` (`task_scope.rs:166`) | fence | `hew_cancel_token_release` (`br-`) | no — P4 |
| message envelope | `hew_msg_envelope_clone_alias` | `%v` | retain | `b` -> `R`, `br-` (`mailbox.rs:557`) | full (unreferenced) | `hew_msg_envelope_release` (`br-`) | no — P4 |
| lambda actor handle | `hew_lambda_actor_clone` | `%v` | retain | `b` -> `F`, `br` (`lambda_actor.rs:1173-1174`: `Box::into_raw(Box::new(HewLambdaActorHandle::new(cloned)))`, a FRESH wrapper, null when the handle was released) | fence | `hew_lambda_actor_release` (`br-`) | no — B21, P4 |
| channel sender | `hew_channel_sender_clone` | `%v` | retain | `b` -> `F`, `au` corroborated (`channel.rs:409-415`, `senders.fetch_add` then a fresh `Box::into_raw` wrapper) | full (`au`) | `hew_channel_sender_close` (`au`) | yes |
| `Vec` | `hew_vec_clone` / `hew_vec_clone_owned` (deep) | `%v` | clone | `b` -> `F`, both `au` corroborated | full (`au`) | V / VO (both `br`) | yes |
| `HashMap`, `HashSet` | `hew_hashmap_clone_layout`, `hew_hashset_clone_layout` (deep) | `%v` | clone | `b` -> `F`, `br-` | fence | M / HS (both `br-`) | no — P2 |
| `Weak<T>` (Rc) | `hew_weak_clone_rc` | `%v` (header ptr) | retain | `b` -> `R`, `au` corroborated by a revision 5 hand-read (`rc.rs:399-409`: `assert!(inner.weak > 0)` then `inner.weak += 1`, returns the SAME header pointer) | full (`au`) | `hew_weak_drop_rc` (`br`; `rc.rs:458-483` asserts `weak > 0` and deallocs at `weak == 0 && strong == 0`) | yes |
| lambda actor weak handle | `hew_lambda_actor_weak_clone` | `%v` | — (row is `bh`) | `b` -> `F`, `bh` (`lambda_actor.rs:1572-1593`: released-flag guard, `clone_handle()` then `Box::into_raw` of a FRESH wrapper; null on a null or already-released handle) | fence | `hew_lambda_actor_weak_drop` (`bh`, `lambda_actor.rs:1611`) | no — P4, and the row carries neither `k` nor `rel` until both are read |

`copy_value` of a record, enum or tuple is the per-type clone glue (plan §1.3), which calls these rows per heap leaf; there is no runtime row for the aggregate.

**Five carriers have no row in the FENCE, and the reason differs per carrier.** `string`, `Vec`, `channel sender` and `Weak<T>` are `au`: their rows are real and admitted, and section 9's preamble excludes `au` rows by rule — read them in `scripts/jit-symbol-classification.toml`, in the full profile, or in the reprint below. `Arc<T>` and the message envelope are different: no compiler path names them, so they are outside the compact profile entirely. Every row this paragraph and the next two send a reader to the full profile for is reprinted below, so no redirect in this section depends on an uncommitted file. `grep -n '^symbol = "hew_arc_new"\|^symbol = "hew_arc_clone"\|^symbol = "hew_arc_drop"' scripts/jit-symbol-classification.toml` prints nothing and `grep -rn '"hew_arc_new"\|"hew_arc_clone"\|"hew_arc_drop"' hew-codegen-rs/src hew-mir/src hew-types/src hew-hir/src --include=*.rs | grep -v _tests` prints nothing; the same two greps over `hew_msg_envelope_clone_alias`, `hew_msg_envelope_release`, `hew_msg_envelope_new` and `hew_mailbox_send_aliased` also print nothing, although the runtime defines all four (`mailbox.rs:542,557,571,2652`) and `docs/ir-ladder.md` §5.6 makes the envelope pair the message-payload protocol P4 emits. Revision 3a called the `Arc<T>` line "the one row in this table with no row in section 9"; it was not, and the envelope pair is the counter-example. Both become wrong the moment their phase lands: the generator pulls a symbol into the compact set as soon as a compiler crate contains its literal, and until then P3's `Arc` and P4's envelope rows must be read in the full profile. The `hew_arc_new` row is `cssg` for the same reason `hew_rc_new` is (section 4).

**Two more §5.6 symbols, and one that correctly has no row anywhere.** `hew_mailbox_send_aliased` (`mailbox.rs:2652`, `br-`) and `hew_msg_node_free` (`mailbox.rs:1093`, `bh`) are in the same unreferenced position as the envelope pair: defined, rowed in the full profile, named by no compiler crate, and needed by P4. `hew_layout_string_drop` is NOT: `hew-runtime/src/layout_intrinsics.rs:263` declares it `extern "C" fn` with no `#[no_mangle]`, so it is a private descriptor thunk whose address is stored in `HewVecElemLayout`/`HewMapValueLayout` (`:416`, `:483`), never a linked C symbol. It has no row and must not get one; plan §1.3 replaces exactly this hand-written thunk family with per-type drop glue.

**Rows the phases need that neither the fence nor the backlog shows, reprinted.** Revision 4 sent the reader to "the full profile named in section 8" five times; section 8 named it only as a command (`emit.py out.json`) with no committed path and no gate, so a P3/P4 implementer reading this file could not see the rows plan §5.2 item 5 and §5.6 depend on. Section 8 now names the path the profile lands at and puts it under the regeneration gate; until it lands, these are the rows, printed in the fence's field order (`au` rows included here, which the fence rule excludes; their informational `[hr]` corroboration flag is elided because section 9 never prints one):

```toml
# full-profile rows reprinted for the phases that need them; NOT part of section 9's 518.
{n="hew_arc_new",d="arc:102",p="cssg",r="F",rel="hew_arc_drop",tr="P",g="3",e="br",f=["B16","B21"]},
{n="hew_arc_clone",d="arc:156",p="b",r="R",rel="hew_arc_drop",k="retain",e="br-"},
{n="hew_arc_drop",d="arc:183",p="c",r="N",tr="P",e="br-"},
{n="hew_msg_envelope_new",d="mailbox:542",p="csg",r="F",rel="hew_msg_envelope_release",g="2",e="br-"},
{n="hew_msg_envelope_clone_alias",d="mailbox:557",p="b",r="R",rel="hew_msg_envelope_release",tr="G",k="retain",e="br-"},
{n="hew_msg_envelope_release",d="mailbox:571",p="c",r="N",tr="G",e="br-"},
{n="hew_mailbox_send_aliased",d="mailbox:2652",p="bsc",r="S",e="br-"},
{n="hew_msg_node_free",d="mailbox:1093",p="c",r="N",tr="G",e="bh"},
{n="hew_actor_send_wire",d="actor:3926",p="bsc",r="S",e="br",f=["tw"]},
{n="hew_string_clone",d="string:1287",p="b",r="R",rel="S",k="retain",e="au"},
{n="hew_weak_clone_rc",d="rc:399",p="b",r="R",rel="hew_weak_drop_rc",tr="P",k="retain",e="au"},
{n="hew_channel_sender_clone",d="channel:409",p="b",r="F",rel="hew_channel_sender_close",tr="G",k="retain",e="au"},
{n="hew_vec_clone",d="vec:1779",p="b",r="F",rel="V",k="clone",e="au"},
{n="hew_vec_clone_owned",d="vec:2966",p="b",r="F",rel="VO",k="clone",e="au"},
{n="hew_cancel_token_new_child",d="task_scope:199",p="r",r="F",rel="hew_cancel_token_release",e="br-"},
```

`hew_actor_send_wire` is here for a different reason from the rest: it is the one ADMITTED `br` row of the 55 that section 9 cannot show, because no compiler crate names it (section 1's `never_referenced` bucket), which is why 54 of the 55 are in the fence.

**One row IS in the fence and still hides from the backlog: `hew_cont_frame_free`.** `docs/ir-ladder.md:1578-1581` §5.2 item 6 makes a generator "a companion block {coro handle, env, env-drop thunk, out-drop thunk, started, pending} freed with `hew_cont_frame_free`, not an rc env", so P3's generator drop glue calls it — and its row is `{n="hew_cont_frame_free",d="cont:574",p="?",r="N",e="bh?"}`, an unproven parameter mode blocked by admission clause 1. Revision 4's P3 backlog row named `hew_gen_coro_destroy` and not this, so it fell to "unscheduled" although §5.2 item 6 puts it in the same glue body. Section 8's P3 row now carries it.

The collection row is a SEAM, not a retain: `HewVec` (`hew-cabi/src/vec.rs:12-34`: `data`, `len`, `cap`, `elem_size`, `elem_kind`, `layout`, `layout_storage`) and `HewLayoutHashMap` carry no refcount field, so `copy_value` on a `Vec`/`HashMap`/`HashSet` is an O(n) deep clone today. WHY: the runtime containers were built as uniquely-owned buffers and the COW story (plan §0 "refcounted, copy-on-write") is implemented by the compiler's `fork` decision on top of them. WHEN obsolete: when the containers gain a header refcount and a `hew_vec_clone_ref`/`hew_vec_fork` pair like `bytes` (`bytes.rs:76`). WHAT the real fix is: a refcounted container header with retain/fork rows in this table; until then the SIR canonical optimizations (copy propagation, borrow instead of copy) are what keeps collection reads from cloning.

## 3. The authorities this table replaces

Eight places decide FFI ownership on main; none is total and they disagree (section 6). **A1 is not on the deletion list**: it is the projection the compiler reads (status line), and this table is where its rows are decided before they are written there. A2-A8 are what section 9 replaces.

**A4 is half of a file, and the other half stays. Both halves are now written down in both documents.** This table listed A4 = `callee_ownership_contract` at `hew-mir/src/runtime_symbols.rs:411` among the authorities section 9 replaces, while `ir-ladder.md` kept the file without qualification. **Decision (plan §6, landed in `ir-ladder.md` §5.1 and §9):** `hew-mir/src/runtime_symbols.rs` is **the only symbol table**, and it carries both halves of a symbol's row — the spelling **and** its ownership. The FFI ownership TOML (`scripts/jit-symbol-classification.toml`) stays the source text an author edits and is **generated into** `runtime_symbols.rs` [P1], the way `hew-types/build.rs:52` already generates `ffi_contracts::FFI_OWNERSHIP_CONTRACTS` from it — never a second table beside it. What A4 loses is the **string-keyed** verdict inside that same file: `callee_ownership_contract` and its `CalleeOwnershipContract {receiver, string_args, result}` join by callee spelling, which is the name-keyed join plan §6 forbids, and they are deleted with the legacy lowerer (B9). Concretely: **`GlueDecl::Leaf.release` is the `rel` of the symbol's generated row, called with that row's binding (section 2a); the row is in `runtime_symbols.rs` and its text is in the TOML.** The status line above is unchanged in substance — the TOML is still the one place a row is written and the one thing the build reads — and gains the generated destination.

| # | authority | anchor | what it decides | coverage |
| --- | --- | --- | --- | --- |
| A1 | `[[ownership.contracts]]` | `scripts/jit-symbol-classification.toml:1970`, projected by `hew-types/build.rs:56-118` into `ffi_contracts::FFI_OWNERSHIP_CONTRACTS`; ratchet `scripts/ffi-ownership-ratchet.toml` (`unclassified = 831`) | per-symbol `result`/`params`/`release-symbol`/`discharge-depth`/`result-retention`; read by `hew-mir/src/return_provenance.rs:1931,2001,2256`, `lower/facts.rs:1297`, `runtime_symbols.rs:379`, `RuntimeCallFamily::consumes_receiver` (only for `TcpAttachLocal`, `runtime_call.rs:2037`), and A8 | 568 symbols; 133 of the 644 compiler-referenced |
| A2 | `.hew` `consume` marks | `hew-types/src/check/registration.rs:10223-10250` -> `ExternContract.consuming_params`, `fn_param_ownership` (`ProducedArgumentBoundary::Transfer/Borrow`); HIR `HirExternFn.param_consume` (`hew-hir/src/node.rs:370`); MIR `return_provenance.rs:1867` | which extern params consume | 477 symbols, 31 with any `consume` |
| A3 | `RuntimeCallFamily` | `hew-types/src/runtime_call.rs:366` (variants), `:1099` `c_symbol`, `:2031` `consumes_receiver` (9 hard-coded close families + TOML for TcpAttachLocal), `:2094` `arg_consume_verdict`, `:2144` `result_ownership`, `:2160` `result_authority` | receiver consume, per-arg verdict, result ownership for codegen-emitted calls | 280 symbols |
| A4 | `callee_ownership_contract` | `hew-mir/src/runtime_symbols.rs:411` string-keyed `CalleeOwnershipContract {receiver, string_args, result}` | receiver/string-arg/result ownership by callee spelling. Deleted with the legacy lowerer; the **file** stays as the one symbol table, with A1's rows generated into it (note above) | 155 spellings |
| A5 | `RuntimeDropDescriptor` + `HeapLeaf`/`CowHeapRelease` | `hew-types/src/runtime_call.rs:2717,2834`; `hew-mir/src/ownership.rs:237,1036,1157,1190` | the release symbol per resource/heap leaf | 16 symbols |
| A6 | codegen | `hew-codegen-rs/src/runtime_abi.rs:4101` `intern_runtime_decl` (signature arms, no ownership); `layout.rs:4822` `emit_insert_overwrite_key_release` and `wire.rs:3200-3224` (conditional key release, two shapes; B3); `llvm.rs:16698` `retain_string_field_load`; `llvm.rs:26336` `is_known_cow_heap_drop_symbol`; `layout.rs:5632` `lower_hashmap_get_layout_call` (rewrites `hew_hashmap_get_layout` to the owned clone); `suspend.rs:3521` vs `:9765` (`take` vs `get` of a task result, B12) | ABI shapes, and the ownership decisions the ground map lists | 509 literals |
| A7 | `stdlib_catalog` | `hew-hir/src/stdlib_catalog.rs:356` `BuiltinEntry {params, return_ty, linkage}` | types and linkage; the linkage variant is the input to A8 | 224 symbols |
| A8 | `stdlib_catalog::result_ownership` | `hew-hir/src/stdlib_catalog.rs:2815-2830`: `ToStringShim => owned(Fresh)`, `StringCloneShim => owned(Clone)`, `RuntimeFfiShim { symbol } => extern_ownership_contract(symbol)` then `runtime_ffi_result_ownership` (`:2833-2855`, requires non-empty `release_symbol`, `discharge_depth != None`, `result_retention == Transferred`); live caller `hew-hir/src/verify.rs:313` (`builtin_call_targets`, falling back to `RuntimeCallFamily::from_checker_signature`) | result ownership of every catalog call from the linkage variant; exactly the `ToStringShim`/`StringCloneShim`/`RuntimeFfiShim` symbols revision 1 had no rows for (`hew_u8_to_string`, the `hew_assert_*` family) | catalog linkage symbols |

## 4. Runtime protocol contracts (hand-read)

The `br`/`br-` rows carry these anchors; the shape of each family:

- **string** (`*mut c_char`, header-aware refcount in `hew-cabi/src/cabi.rs:222` `CStringHeader`): `hew_string_drop` = release (`string.rs:1264` `is_managed_cstring` gate then `free_cstring`; no-op for literals/foreign pointers); `hew_string_clone` = the retain target (`string.rs:1287` `is_managed_cstring` gate then `cstring_retain`, same pointer, abort on refcount overflow; section 2a); `hew_string_concat` allocates via `alloc_cstring_data` (`string.rs:270`, `CSTRING-ALLOC` marker) and aborts on length overflow; `hew_u8_to_string` and its siblings return a fresh `malloc_cstring` (`string.rs:467-478`); `hew_string_builder_new` is `Box::into_raw` (`string.rs:60`) balanced only by `hew_string_builder_finish`, which aborts on a null builder or length overflow, `Box::from_raw`s the builder, and returns a fresh string or null on OOM (`string.rs:204-221`, `cabi_guard!` at `:211`, so `tr="AG"`); `hew_vec_join_str` (`string.rs:1038`) fetches elements via `hew_vec_get_str` and releases them itself.
- **bytes** (`BytesTriple {ptr, offset, len}` by value; `bytes.rs:74-81`; buffer refcount read by `refcount()` at `:96` over the `HEADER_SIZE = 8` header at `:32`): `hew_bytes_new` and `hew_bytes_from_static` (`:717`, every bytes literal; copies the static source into a fresh `alloc_buf`, which aborts on OOM `:148-155`) mint rc=1; `hew_bytes_clone_ref` is the retain target (`:294`, `fetch_add`, abort past `BYTES_RC_MAX`, unit result); `hew_bytes_drop` `fetch_sub == 1` then `libc::free(base)`, no underflow assert (`:318`); **five entry points take `&mut BytesTriple` and all five are mode `m`** — `hew_bytes_push` (`:347`), `hew_bytes_clear` (`:560`), `hew_bytes_pop` (`:427`), `hew_bytes_set` (`:462`), `hew_bytes_append` (`:581`): `clear` releases this receiver's reference and resets the triple to the canonical empty value, the other four go through `ensure_unique` (`:181-208`), whose fork path allocates a new buffer, copies the active region and calls `hew_bytes_drop(ptr)` on the old one — its own doc says "The decrement on the fork path CONSUMES one owner of `ptr`: the caller is understood to be moving its own reference forward onto the returned buffer" (`:169-175`) — after which the callee writes `triple.ptr` and `triple.offset` back. Revision 4 printed `b` for `pop`, `set` and `append` because the heuristic mapped a borrow signal on a `&mut` receiver to `b`; `b` is the one mode that cannot be true of them, because section 7 gives `b` a `begin_borrow`/`end_borrow` around the call and the caller's value is not the same afterwards. `alloc_buf`/`realloc_buf` abort on allocation failure (`:148-155`, `:227-236`), which is the `A` in those rows' `tr`; `hew_bytes_index`/`hew_bytes_slice`/`hew_bytes_pop` reach `runtime_bounds_trap` through `bytes_bounds_trap` (`:57-62`), which is the `T`.
- **Vec** (`HewVec` with inline `HewVecElemLayout` descriptor, `hew-cabi/src/vec.rs:71,207`): constructors `libc::malloc` (`vec.rs:315`); `hew_vec_new_with_layout` copies the descriptor and stamps `vec_string_{clone,drop}_inplace` for String kind, `LayoutManaged => unreachable!()` (`:532-570`); `hew_vec_new_with_elem_layout` runs `validate_elem_layout` (`:249-272`, non-Plain without `drop_fn` aborts); `hew_vec_new_str` leaves `layout` null and sets `elem_kind = String` (`:416-427`); free/clear walk `drop_fn` per live slot (`drop_element_range` `:1530`, `free_vec_descriptor` `:1557-1569`); string elements copy in on push/set (`copy_string_element_in` `:625`, `:763`, `:1348` releases the old), retain on get/clone/slice/append (`retain_string_element` `:647` = `hew_string_clone`, `:850`, `clone_vec_descriptor` `:1706`, `:1131`, `:1822`), transfer out on pop/remove (`:1444`, `:2066` `read()` without retain or release; `hew_vec_remove_at_str` opens with `abort_if_layout_aware(v)` (`:2069` -> `:213` -> `abort_layout_aware_operation` `:161` `libc::abort`), which is why its row reads `tr="UAT"` — a layout-aware vec reaching a string accessor is a P1 emitter invariant, not a user error); owned elements deep-clone on push/set (`clone_fn`, `:2634`, `:2840`), move on `*_move` (`:2681`, `:2892`); **the three move-out paths are NOT the same shape** and revision 3's "move-out on pop/take/remove ... memcpy to `out`, no drop" flattened them: `hew_vec_pop_owned` (`:2926-2942`) decrements `len` BEFORE the memcpy, so the source slot leaves the live range; `hew_vec_remove_at_owned` (`:2130-2157`) memcpies then shifts the tail down and decrements `len`, same result; but `hew_vec_take_owned` (`:2812-2830`) memcpies to `out`, `write_bytes(src, 0, layout.size)` and **leaves `len` unchanged**, so the zeroed slot stays live and `drop_element_range` (`:1530-1544`) will later run the element `drop_fn` over all-zero bytes at `free`/`clear`/`set`. That is a hidden requirement on the P1 per-type drop glue — see B17. `hew_vec_remove_at_owned` guards null and reaches the bounds bridge on OOB (`:2135`, `:2143`); `hew_vec_get_owned`/`get_layout`/`get_ptr` return pointers into the live buffer (`:2711`, `:2379`, `:897`); `hew_vec_get_clone` retains a String-kind element or memcpy+`clone_fn`s an owned one into `out` (`:2752-2800`), so the out value's release is element-type-directed and the row spells no `rel` (section 2's out-slot rule); on the owned arm it reaches `libc::abort` three ways — a non-zero clone-thunk status (`:2789-2794`), `owned_descriptor` -> `abort_owned_descriptor_missing` and `owned_clone_fn` -> `abort_owned_thunk_missing` (`:2530-2547`) — so its row is `tr="AG"`, not the `tr="G"` revision 3 printed; `hew_vec_pop_ptr` removes the element (`len -= 1` then `read()`, `:1408-1426`) and nothing in the vec releases the pointee afterwards, so its result is a transfer whose ownership depends on the descriptor (`X`, B3; closure-pair vecs own their elements, `hew-mir/src/lower/expr.rs:9832-9834`); `hew_vec_take_all` moves the contents into a fresh vec built by the aborting constructors and leaves the source empty (`:2990-3015`, `tr="GA"`); `hew_vec_free` and `hew_vec_free_owned` are the same function (`:1593`, `:2953`; B4).
- **HashMap/HashSet** (`HewLayoutHashMap` with by-value `HewMapKeyLayout`/`HewMapValueLayout` snapshots, `hashmap.rs:851`): `validate_descriptor_ownership` (`:461`) panics when a String/LayoutManaged descriptor has `drop_fn = None`; insert moves K and V in on the vacant path and only V on the present path (`:1240-1310`); get borrows a slot pointer (`:1327`; the compiler never emits it, `rw` -> `hew_hashmap_get_clone_layout`); get_clone clones via `clone_fn` into `out` (`:1379`); remove drops stored K and V via `drop_fn` (`:1455`); remove_take drops K and moves V out (`:1552`); free/clear drop every occupied slot (`:1672`, `:1746`); clone aborts when a non-Plain value layout has `clone_fn = None` (`:1008`); iterators are `Box::into_raw`/`Box::from_raw` (`:133`, `:198`). HashSet is `HashMap<T, ()>` (`hashset.rs:245`) and delegates insert/remove/clear/free (`:314`, `:362`, `:385`, `:523`).
- **Arc/Rc** (`arc.rs:24`, `rc.rs:24`): `*_new` copies the payload BYTES into the cell and stores `Option<drop_fn>` (`arc.rs:102`, `rc.rs:103-148`); `*_drop` runs `drop_fn(ptr)` at zero (`arc.rs:183`, `rc.rs:181`), so the heap embedded in the payload is owned by the cell and the `data` argument is consumed. The signature is `(data: *const u8, size: usize, align: usize, drop_fn: Option<fn>)` (`rc.rs:103-108`), so the mode string is **`cssg`** — index 0 is the consume, 1 and 2 are scalars, 3 is the glue. Revision 3 wrote `bcsg`, one index off: it marked `size: usize` as the consume and the payload as a borrow, so a lowering reading it would `move` a usize and `begin_borrow` the payload, the caller would release the payload heap at scope exit, and `hew_rc_drop` would run `drop_fn` over the same bytes at zero — a double free on every `Rc::new` of a heap-owning payload. (MIR keeps the `Rc::new` value operand in the consume scan, `hew-mir/src/lower/facts.rs:2494-2495`; the `Rc::new` codegen site passes `rc_payload_drop_thunk` as arg 3, `llvm.rs:16053-16064`.) The consume of index 0 is defined by the glue at index 3, and the second call site does not pass total glue — see B16. `*_clone` are retain targets returning the same pointer (`arc.rs:156`, `rc.rs:156-165`; section 2a); `*_drop` assert `old > 0`; `hew_arc_downgrade`/`hew_rc_downgrade` bump `weak` and return the header pointer (`arc.rs:280`, `rc.rs:372`); `hew_weak_upgrade_arc` CAS `strong + 1` or null (`arc.rs:304`); `hew_weak_drop_arc` asserts `old > 0` (`arc.rs:347`); `hew_rc_set` swaps the payload and destroys the replacement (`rc.rs:303`).
- **Messages** (`mailbox.rs`, `cow_envelope.rs`): `hew_actor_send` deep-copies `data` (doc comment `actor.rs:3757`; chain `actor_send_internal` `:6077` -> `actor_send_result_internal_reply` `:5805` -> `mailbox::hew_mailbox_send_fire_and_forget`); the copy is released through `message_drop_fn` on eviction, coalesce and drain (`mailbox.rs:870`, `:1390`, `:1834`), so the heap embedded in `data` is ADOPTED on the enqueued path; the `ErrForeignRuntime`/`ErrActorStopped` legs (`:5820`, `:5828`) return before any copy, so `data` is `x` (B3); MIR moves every send/ask argument (`hew-mir/src/lower/actor.rs:1871`, `:2213` `lower_value_for_move`). `hew_actor_send_aliased` / `hew_mailbox_send_aliased` consume exactly one envelope refcount (`actor.rs:3812`, `mailbox.rs:2652`); `hew_msg_envelope_new` adopts the payload pointer and frees it with `drop_glue` + `libc::free` at zero (`mailbox.rs:542`, `cow_envelope.rs:117-121`); `hew_msg_envelope_clone_alias`/`release` are the refcount pair (`mailbox.rs:557`, `:571`); the mailbox's `message_drop_fn` (`mailbox.rs:1466`, registered by `hew_mailbox_set_message_drop_fn` `:1896` / `hew_actor_set_message_drop` `actor.rs:5489`) releases queued payloads on eviction and drain.
- **Ask/reply** (`reply_channel.rs`): channels are refcounted (`hew_reply_channel_new` `:158` `Box::into_raw refs=1`, `retain` `:316`, `free` `:894` runs `reply_drop_fn` on an undelivered value then `libc::free`); `hew_reply` CONSUMES one sender reference of `ch` on every path past the null guard (`release_sender_ref_if_cancelled` `:358-372` and `publish_reply_from_sender_ref` `:440` both end in `hew_reply_channel_free(ch)`; codegen retains that reference for exactly this release, `suspend.rs:6219-6225`), deep-copies `value` on delivery, and on the cancel/alloc-fail legs the REGISTERED `reply_drop_fn` reclaims `value` (`:642-689`, conditional; B3, B13); `hew_reply_channel_signal_ready` consumes one retained reference (`:707`); `hew_actor_ask` (`actor.rs:6198`, data adopted on the enqueued path as for `hew_actor_send`) and `hew_reply_wait` (`:781`) return the reply BOX (`O`): codegen loads the value out into `reply_dest` and frees only the box (`llvm.rs:33207-33218`), so `destroy_value` of the result must be `load.take` + box release, never `rel` alone.
- **Actors** (`actor.rs`): `hew_actor_spawn_opts` deep-copies `init_state` (`:3502`, `deep_copy_state` `:2986-3001`) and the actor adopts the copy (teardown runs `state_drop_fn`, `:2600`, `:2740`; codegen registers the state drop/clone thunks after spawn, `suspend.rs:7248-7249`; MIR moves every state field, `hew-mir/src/lower/actor.rs:2737,2769-2771`), but the OOM leg (`:3503-3505`) returns null without adopting, so the opts parameter is `x` (B3); `hew_actor_spawn_opts_adopt` adopts `cloned_state` (`:3592`); the `HewActor` pointer is runtime-owned (`H`); `hew_actor_set_state_drop` / `set_message_drop` / `set_state_clone` store non-`Option` fn pointers as `Some` (`:5466`, `:5489`, `:5531`); `hew_actor_send_wire` (native `:3926-3939`) consumes `bytes` (`hew_vec_free` at `:3939`); its `bytes.is_null() || actor.is_null()` early return is a precondition violation under the section 2 `x` convention (the doc requires a valid spawned `actor`), so the row is `bsc`, while the wasm32 twin (`:3960-3965`) frees the vec on that leg too (`tw`); `hew_actor_demonitor` takes the scalar `ref_id` (`monitor.rs:1069`). Lambda actors (`lambda_actor.rs:1091-1129`): `hew_lambda_actor_new` adopts `state` only on the non-null return (five null-returning legs, none calls `state_drop`; `HewLambdaActor::new` doc `:601-602` leaves release to the caller), so `state` is `x`; `hew_lambda_actor_send` copies the payload into an opaque `Vec<u8>` (`:1227` `to_vec`) only past the released/null/stopped legs and the runtime has no message drop thunk (only `state_drop`), so the message is `x` and an undispatched copy cannot release an embedded string; `hew_lambda_actor_clone` heap-allocates a FRESH wrapper (`:1173-1174`), not a refcount on the argument pointer.
- **Closeable handles**: `hew_stream_close` / `hew_sink_close` `drop(Box::from_raw(..))` (`stream.rs:1848`, `:1906`); `hew_sink_flush`/`write_string`/`write_bytes` borrow (`:1892`, `:2170` `CStr::from_ptr`, `:2396` `from_raw_parts`); `hew_stream_lines`/`chunks`/`take`/`map_string` call `consume_stream_inner`, which `ptr::read`s the backing out and `dealloc`s the `HewStream` shell, then return a fresh stream (`:2049`, `:2071`, `:2357`, `stream_transform_entry`); `hew_duplex_close`/`close_half` flip `released` and release (`duplex.rs:1131`, `:1496`); `hew_duplex_send_half`/`recv_half` `Box::from_raw` the unified handle and return a fresh half (`:1189`, `:1235`); `hew_channel_sender_close`/`receiver_close` `drop(Box::from_raw(..))` (`channel.rs:435`, `:455`); `hew_channel_sender_clone` `senders.fetch_add` + `Box::into_raw` (`:409`); `hew_channel_send_layout` (`:316-338`) and `hew_stream_send_layout` (`stream.rs:2486-2530`, declared by the same codegen arm `runtime_abi.rs:4697-4703`) copy the element bytes into an envelope and, for `LayoutManaged` descriptors, stamp the core so discard exits release the envelope's heap: the caller's heap is adopted for `LayoutManaged` and copied otherwise (`x`, B3; `elem_layout_witness` aborts on a malformed witness, `channel_common.rs:80-82`); `hew_channel_recv_layout` writes the element to `out` (`:351`).
- **Coroutines / cancellation / dyn / tasks**: `hew_cont_destroy` `coro_destroy(handle)` unless crash-owned (`cont.rs:1969`); `hew_gen_coro_destroy` destroys the inner handle then runs the env thunk (`:2058`); `hew_cont_crash_cleanup_retire` takes a scalar token (`:1162-1188`); `hew_cancel_token_release` `refs.fetch_sub == 1` -> `Box::from_raw`, parent released recursively (`task_scope.rs:132`); `hew_cancel_token_retain` (`:166`, retain target); `hew_cancel_token_new_child` retains the parent into the child (`:189`, genuine `r`); `hew_dyn_box_alloc`/`free` (`trait_object.rs:318`, `:380`, panic on invalid layout, assert non-null); `hew_task_new`/`free` `Box::into_raw`/`from_raw` (`task_scope.rs:638`, `:666-687`; free runs `result_drop_fn(t.result)` when `result_written && !result_consumed`, then `libc::free(t.result)`, then releases the adopted env with `hew_rc_drop(t.env_ptr)` at `:681-683` — which `assert!`s and `.expect()`s, so `hew_task_free` is `tr="PG"`, not the empty trap column revision 3 printed); **the task ADOPTS its environment**: `hew_task_set_env` (`:696-702`) stores the pointer and `hew_task_free` releases it, so index 1 is a consume (`p="bc"`, not revision 3's `b?`), and the only codegen caller hands it the freshly minted `hew_rc_new` handle and never releases it (`thunks.rs:729-734`, no `hew_rc_drop` of `rc_env` anywhere in `emit_spawn_task_closure`). A second `hew_task_set_env` on the same task overwrites `env_ptr` without releasing the previous one, so the lowering calls it at most once per task. `hew_task_get_env` (`:744-748`) returns the pointer with no refcount touch: a borrowed alias of the task-owned env, invalidated by `hew_task_free` (`r="B"`, not `?`); `hew_task_set_result` `libc::malloc` + copy (`:1294-1308`) and the task then OWNS the heap embedded in the copied bytes, so `result` is consumed (`bcs`; codegen agrees, `thunks.rs:114-121`); `hew_task_take_result` sets `result_consumed` and returns the task-owned box (`:776-784`, `O` without `rel`); `hew_task_get_result` returns the same buffer WITHOUT marking it consumed (`:756-764`, `B`; B12); `hew_task_set_result_drop_fn` stores `Option<fn>` (`:1322`); `hew_supervisor_set_config_drop_fn` takes a non-`Option` fn (`supervisor.rs:10084`). `hew_cron_next` writes `*out_ts` (an `i64`) only on status 0 (`hew-std/src/time/cron.rs:124-150`).
- **Traps and prints**: `hew_trap_with_code` is `extern "C-unwind"` on both targets (native `supervisor.rs:429` stamps the actor error code and `panic_any(HewPanic)` when the context can unwind; wasm32 twin `trap_code.rs:170` panics or `process::exit(1)`), so every call site needs a cleanup edge (section 7); `hew_print_value` (`print.rs:185-215`) aborts on an unknown kind tag and its `Str` arm `printf`s the borrowed pointer bits without freeing (`print_str` `:131-151`); `hew_assert*` (`assert.rs`) compare scalars or `strcmp` two borrowed C strings and `libc::abort` on failure.

## 5. Containers whose element glue is `Option`

Every runtime container that can own elements carries its element glue as `Option<fn>`. The plan (§1.3) makes glue mandatory for owning element types; the table below is the change list, with what fails closed today.

| container | field | today | required change |
| --- | --- | --- | --- |
| Vec | `HewVecElemLayout.{clone_fn,drop_fn}` (`hew-cabi/src/vec.rs:217,220`) | constructor aborts when `ownership_kind != Plain && drop_fn.is_none()` (`vec.rs:268`); `clone_fn` checked only at first clone use (`vec.rs:2571`) | keep the abort; make `clone_fn` mandatory at construction for non-Plain (`hew_vec_new_with_elem_layout`), since a release-only descriptor makes `hew_vec_clone_owned` a runtime abort the compiler cannot see |
| HashMap key | `HewMapKeyLayout.drop_fn` (`hew-cabi/src/map.rs:192`) | `validate_descriptor_ownership` (`hashmap.rs:461`) panics when `ownership_kind` is String/LayoutManaged and `drop_fn` is `None`; `hash_fn`/`eq_fn` `None` panics too (`:377`) | none beyond dropping the `Option` in the C struct once every descriptor is compiler-emitted |
| HashMap value | `HewMapValueLayout.{drop_fn,clone_fn}` (`map.rs:229,234`) | `drop_fn` validated by `validate_descriptor_ownership` (`hashmap.rs:461`); `clone_fn` is never validated (`validate_val_layout` `:420` checks only align/size), so a `None` clone thunk fails at first `get_clone`/`clone_layout` use | require `clone_fn` for non-Plain at construction |
| HashSet | inherits the key layout (`hashset.rs:245`) | as HashMap key | as HashMap key |
| Arc / Rc | `HewArcInner.drop_fn` (`arc.rs:24`), `HewRcInner.drop_fn` (`rc.rs:24`), parameter of `hew_arc_new`/`hew_rc_new` | `None` accepted for any payload; a heap-owning payload then leaks at zero. Revision 3 said "codegen always passes `rc_payload_drop_thunk`, `llvm.rs:16053`" — **that is false**: only the `Rc::new` site does (`llvm.rs:16053-16064`). The `SpawnTaskClosure` environment site passes a null pointer when no field is `OwnsMoved` and otherwise a thunk that releases ONLY the `OwnsMoved` fields (`thunks.rs:685-715`), so at that site the same symbol adopts some of the bytes and borrows the rest (B16) | `hew_arc_new`/`hew_rc_new` take a non-`Option` `drop_fn` (a no-op thunk for BitCopy payloads emitted by codegen); the `Option` field is deleted, which also makes the partial-glue site above a type error rather than a silent split |
| mailbox | `HewMailbox.message_drop_fn` (`mailbox.rs:1466`), set by `hew_mailbox_set_message_drop_fn` (`:1896`) and `HewActorSpawnOpts.message_drop_fn` (`actor.rs:2949`) | `None` means evicted/drained payloads are freed without element release (`mailbox.rs:1834`) | spawn requires a message drop thunk for every actor whose message set carries heap; the `Option` on `spawn_opts` becomes mandatory |
| actor state | `HewActor.state_drop_fn` (`actor.rs:1264`), `state_clone_fn` (`:1291`) | `None` skips state release at teardown (`actor.rs:2600`) | `hew_actor_spawn_opts` takes the state drop thunk in the opts struct (non-`Option`) |
| reply channel | `HewReplyChannel.reply_drop_fn: AtomicPtr` null = none (`reply_channel.rs:100`), set by `hew_reply_channel_set_reply_drop_fn(ch, Option<fn>)` (`:258`) | undelivered replies with embedded heap leak when unset (`:339`); the runtime-internal channel of the blocking `hew_actor_ask` never sets it (B13) | the ask lowering passes the thunk at `hew_reply_channel_new`; `hew_actor_ask` takes it as a parameter |
| envelope | `HewMsgEnvelope.drop_glue` (`cow_envelope.rs:30`) via `hew_msg_envelope_new(.., Option<fn>)` | payload freed with `libc::free` only when `None` | non-`Option` parameter |
| task / supervisor | `HewTask.result_drop_fn` (`task_scope.rs:317`), `hew_task_set_result_drop_fn(Option)` (`:1322`); `config_drop_fn` (`supervisor.rs:700`) | unset = leak of embedded heap on unread result | thunk passed at `hew_task_new` / config install |
| lambda actor | `hew_lambda_actor_new(.., state_drop: Option<fn>)` (`lambda_actor.rs:1091`) | `None` -> null return + last_error (fail-closed for the glue), but no null-returning leg releases `state` (B3, row `x`) | make the parameter non-`Option` so the failure is a link/type error; the remaining null legs (capacity 0, invalid shape, thread spawn) must release `state` or the ABI must return the rejected state |

## 6. Bug list

Each item names the command that shows it and the decisive lines.

**B1. The typed release symbols are outside the audited table**  
`for s in hew_sink_close hew_duplex_close hew_duplex_close_half hew_lambda_actor_release hew_cancel_token_release hew_actor_demonitor hew_gen_coro_destroy hew_bytes_drop hew_vec_free hew_vec_free_owned hew_hashmap_free_layout hew_hashset_free_layout; do grep -c "^symbol = \"$s\"" scripts/jit-symbol-classification.toml; done` prints `0` twelve times; only `hew_string_drop`, `hew_stream_close`, `hew_channel_sender_close`, `hew_channel_receiver_close` have rows. `RuntimeDropDescriptor::c_symbol` (`hew-types/src/runtime_call.rs:2834`) and `HeapLeaf::release_symbol` (`hew-mir/src/ownership.rs:1036`) name these 16; `ffi_contracts::extern_ownership_contract` answers `Absent` for 12 of the compiler's own release protocol. Of the 644 compiler-referenced symbols, 511 have no contract (`gen_doc.py` `called_without_contract`). The four that DO have rows are admitted from 3a onward under the B15 precedence, so `hew_string_drop` is no longer a build error at every string `destroy_value`; the other 12 stay blocked until they gain a TOML row or are re-read.

**B2. `RuntimeCallFamily::arg_consume_verdict` reports `ProvenBorrow` for receivers the body frees**  
`hew-types/src/runtime_call.rs:2094-2100`: index 0 is `ProvenConsume` iff `consumes_receiver()` (`:2031`, nine hard-coded families), else `ProvenBorrow`. Bodies that consume their receiver and are NOT in that list: `hew_auto_mutex_free` (auto_mutex.rs:216), `hew_cancel_token_release` (task_scope.rs:183), `hew_dyn_box_free` (trait_object.rs:380), `hew_hashmap_free_layout` (hashmap.rs:1672), `hew_hashset_free_layout` (hashset.rs:523), `hew_lambda_actor_weak_drop` (lambda_actor.rs:1611), `hew_rc_drop` (rc.rs:181), `hew_regex_free_capture` (hew-std regex.rs:507), `hew_reply_channel_free` (reply_channel.rs:894), `hew_reply_payload_free` (reply_channel.rs:749), `hew_task_free` (task_scope.rs:666), `hew_task_scope_destroy` (task_scope.rs:2341). Mutate-in-place receivers are likewise `ProvenBorrow` although they release or move the old representation: `hew_vec_take_all` and all five `&mut BytesTriple` entry points — `hew_bytes_clear`, `hew_bytes_push`, and, new in revision 5, `hew_bytes_pop`, `hew_bytes_set`, `hew_bytes_append` (the generator now flags 23 of these, up from 20). Non-receiver indexes default to `ConservativeConsume` (`:2158`), so `hew_actor_ask`'s data slot has no proven verdict either. The comment at `:2069` admits the default is chosen because a missed consume leaks rather than double-frees; the verdict name says proven. SIR rule 5 cannot consume this axis.

**B3. Conditional consume at container ingress has no mode**  
`hew-runtime/src/hashmap.rs:1240-1310`: on the vacant path K and V are memcpy'd in (both consumed); on the present path the old V is dropped and the new V memcpy'd in (V consumed) while the stored K is reused and the caller's duplicate K is NOT consumed. Codegen compensates at two sites with two shapes: `emit_insert_overwrite_key_release` (`hew-codegen-rs/src/layout.rs:4822-4860`) keyed on the `i1` return, which fails closed for any heap key other than `string` or a string-bearing record (`:4838-4855` `FailClosed("... only string and string-bearing record keys are layout-managed")`), and the CBOR decode path (`wire.rs:3197-3232`), which branches on `was_inserted` and releases the duplicate key through the `emit_de_drop_owned` type walk. The two agree only because the checker refuses `bytes` and non-record enum keys; plan §6 deletes `emit_de_drop_owned` as "decode failure cleanup", but the wire.rs duplicate-key release is a SUCCESS-path release that the replacement runtime ABI must absorb. `RuntimeCallFamily::arg_consume_verdict` says `ProvenConsume` for indexes 1 and 2 (`runtime_call.rs:2106`) and `HashSetInsertLayout` index 1 (`:2117`). Same shape, "adopt the copied representation on the success path only": `hew_reply` (`reply_channel.rs:642`, value reclaimed by the registered `reply_drop_fn` on the cancel/alloc-fail legs), `hew_actor_spawn_opts` (`actor.rs:3503-3505`, OOM leg), `hew_actor_ask`/`hew_actor_send` data (`actor.rs:5820,5828`, terminal/foreign legs), `hew_lambda_actor_new` state and `hew_lambda_actor_send` message (`lambda_actor.rs:1098-1129`, `:1194-1213`), `hew_channel_send_layout`/`hew_stream_send_layout` (`channel.rs:316`, `stream.rs:2486`, adoption depends on `ownership_kind`), `hew_vec_push_ptr`/`hew_vec_set_ptr`/`hew_vec_pop_ptr` (`vec.rs:791,2166,1457`, pointee ownership depends on the vec descriptor). Out-parameters are the mirror image: `hew_vec_pop_owned` (`vec.rs:2926`), `hew_vec_take_owned` (`:2812`), `hew_vec_get_clone` (`:2752`), `hew_vec_remove_at_owned` (`:2130`), `hew_hashmap_remove_take_layout` (`hashmap.rs:1552`), `hew_hashmap_get_clone_layout` (`:1379`), `hew_cron_next` (`cron.rs:124`) write `out` only on the success return, so an `o`/`t` slot is initialized on one edge only. **The drift site the fresh emitter must not repeat** (the out-parameter counterpart of the direct-`free` list in section 2): the `hew_vec_pop_owned` codegen site at `hew-codegen-rs/src/layout.rs:2243-2262` calls the runtime with `dest_ptr`, **discards the returned `i32` and emits no branch**, so `dest` is treated as initialized on both edges while the runtime writes it only when `len != 0` (`vec.rs:2932-2934`). Nothing in `hew-mir` guards it either: `grep -rn 'VecPopOwned\|hew_vec_pop_owned' hew-mir/src --include=*.rs | grep -v /tests/` finds `runtime_symbols.rs:619,956`, `lower/facts.rs:4010,4120` and the family match at `lower/expr.rs:9752` — no emptiness test, so whether MIR honours the `o` mode today is unproven, and P1 must not inherit the assumption. Rows carry `x`/`X` and are not admitted: P1 cannot emit spawn, send, ask or lambda-actor calls until the SIR lowering has a `consume-if(ret)` / `init-if(ret)` mode with a branch on the result, or the runtime ABI changes (insert returns the rejected key through an out-parameter, or always consumes and re-retains; spawn/new release the rejected state; send/ask always adopt).

**B4. `hew_vec_free_owned` is `hew_vec_free`**  
`hew-runtime/src/vec.rs:1593` and `:2953` both call `free_vec_descriptor` (`:1557-1569`), which walks `drop_element_range` for every live slot regardless of which constructor built the vec. `CowHeapRelease::release_symbol` (`hew-mir/src/ownership.rs:1190`) distinguishes `VecPlain` -> `hew_vec_free` from `VecOwnedElement`/`VecClosurePairs` -> `hew_vec_free_owned`, codegen validates the string set at `hew-codegen-rs/src/llvm.rs:26336`, and the `hew_vec_take_all` doc comment (`vec.rs:2985-2988`) asks for `hew_vec_free_owned` on descriptor-backed results. Resolution: `hew_vec_free` is the one release symbol (one `CowHeapRelease::Vec` arm; the 7 `hew_vec_free_owned` TOML `release-symbol` references collapse onto it; `hew_vec_free_owned` is deleted with the legacy lowerer). Rows keep `VO` only as constructor provenance. `hew_vec_take_all` is an `au` row from 3a onward, so it prints the TOML's `release-symbol = "hew_vec_free_owned"`; that spelling is correct on the runtime today for the same reason (`vec.rs:1593` and `:2953` are one function) and it is the spelling that changes when this bug lands. A second thing the `au` grade costs on this symbol: the hand-read mode is `m` (the source vec is emptied in place) and the TOML says `borrow`. Section 2's mapping sends `m` -> `borrow`, so the two do not contradict and the row is admitted — but **the TOML has no spelling for `m`**, so an audited row can never tell the lowering that the receiver's representation was replaced. `hew_bytes_clear` and `hew_bytes_push` are the same shape (B2). Until the TOML grows the axis, an `m` receiver is only visible on a `br` row.

**Closed: the ladder now names the same symbol.** `ir-ladder.md` §5.2 item 6 spelled the Vec leaf release as "`Vec<T>` -> `hew_vec_free_owned` (`hew_vec_free` for BitCopy elements, vec.rs:1593/2953)" through revision 6; revision 7 spells it `hew_vec_free` for every element class, which is what this bug resolves to. Both symbols are live admitted `br` rows in the fence today (`{n="hew_vec_free",d="vec:1593",p="c",r="N",e="br"}` and `{n="hew_vec_free_owned",d="vec:2953",p="c",r="N",e="br",f=["B4"]}`) and both bodies are `unsafe { free_vec_descriptor(v) }`, so either spelling links and runs; the disagreement is invisible at run time and only shows up at P5 when one of them is deleted. A P2 lane building `GlueBody::Leaf` from the ladder emits a symbol P5 removes; a lane building it from this resolution emits a symbol the ladder does not name. Ladder §5.2 item 6 now reads `hew_vec_free` for every element class, with `hew_vec_free_owned` kept only as the constructor-provenance alias `VO`; this table's resolution did not move.

**B5. `hew_vec_get_str` contract contradicts the TOML's own retention axis**  
`scripts/jit-symbol-classification.toml:2049-2056`: `result = "retained"`, `result-retention = "transferred"`. `hew-runtime/src/vec.rs:850-862`: `retain_string_element(raw)` = `hew_string_clone` refcount bump returning the SAME pointer the vec still holds. The header (`:38-50`) defines `transferred` as sole owner and `shared-refcount` as an aliased refcount share; the body is the latter. `hew_mir::return_provenance::build_extern_contract_table` mints from `transferred`.

**B6. Seventeen std externs are declared without `consume` while the runtime consumes the argument**  
`python3 gen_doc.py inv.json out.json` bug class `hew-vs-toml` (14, audited contract says consume): `hew_channel_receiver_close` (std/channel/channel.hew:226), `hew_channel_sender_close` (std/channel/channel.hew:225), `hew_cron_free` (std/time/cron/cron.hew:158), `hew_deque_free` (std/deque.hew:109), `hew_glob_free` (std/path.hew:396), `hew_http_response_free` (std/net/http/http_client.hew:298), `hew_process_drop` (std/process.hew:325), `hew_proto_msg_free` (std/encoding/protobuf/protobuf.hew:138), `hew_quic_event_free` (std/net/quic/quic.hew:752), `hew_regex_free` (std/text/regex/regex.hew:284), `hew_semaphore_free` (std/semaphore.hew:122), `hew_url_free` (std/net/url/url.hew:441), `hew_ws_message_free` (std/net/websocket/websocket.hew:377), `hew_xml_free` (std/encoding/xml/xml.hew:182). Plus three stream adapters with no contract at all: `hew_stream_lines`, `hew_stream_chunks`, `hew_stream_take` (`std/stream.hew:274-277` declare `stream: Stream<string>` without `consume`; `hew-runtime/src/stream.rs:2049,2071,2357` call `consume_stream_inner`, which `ptr::read`s the backing out and `dealloc`s the `HewStream` shell). The stream case is the severe one: `Stream` carries `RuntimeDropDescriptor::StreamClose`, so a scope-exit `hew_stream_close` on the consumed source is a double free unless the Hew wrapper moves the binding. For the 14, the `.hew` parameter is an opaque handle record (e.g. `hew_regex_free(re: PatternHandle)`, `std/text/regex/regex.hew:284`) and the consuming disposition rides the enclosing `close(consuming self)` method instead of the extern edge; the checker's `release_signature_mismatch` (`hew-types/src/check/registration.rs:415`) runs only inside the opaque-resource lifecycle join (`:637`), reached from a producer contract with `resource-result-type`. With plan §1.5 ("every `extern` declaration in `std/` carries a total ownership row") these declarations must gain `consume` or the SIR edge borrows a handle the callee frees.

**B7. `hew_bytes_drop` has no double-release guard**  
`hew-runtime/src/bytes.rs:318-333`: `rc.fetch_sub(1, Release) == 1` frees; a second drop on a freed or zero-count buffer wraps the `u32` silently. `hew_arc_drop` (`arc.rs:196`) and `hew_rc_drop` (`rc.rs:229`) assert `old > 0`. Under the OSSA verifier a double `destroy_value` is a compile error, but the runtime oracle for the parity harness should abort, not wrap.

**B8. `hew_vec_new_with_layout` treats `LayoutManaged` as unreachable while a shipped descriptor is `LayoutManaged`**  
`hew-runtime/src/vec.rs:558` `HewTypeOwnershipKind::LayoutManaged => unreachable!()`; `hew-runtime/src/layout_intrinsics.rs:421-427` `hew_layout_key_bytes` has `ownership_kind: LayoutManaged`. Only the HashMap constructor accepts that descriptor today; the Vec constructor panics if codegen ever routes a bytes-element layout through `_layout`. Mark WHEN: obsolete once Vec element glue is always the thunk-bearing `HewVecElemLayout` (plan §1.3).

**B9. Four parallel ownership vocabularies for the same call**  
`hew-mir/src/runtime_symbols.rs:411` `callee_ownership_contract` (string-keyed, 155 spellings, `ReceiverOwnership`/`StringArgsOwnership`/`ResultOwnership`), `hew-types/src/runtime_call.rs:2094` `arg_consume_verdict` (family-keyed, `ConsumeVerdict`), `ffi_contracts` (TOML), and `hew_hir::stdlib_catalog::result_ownership` (linkage-variant-keyed `ProducedValueOwnership`, `stdlib_catalog.rs:2815`, live at `verify.rs:313`). One example of drift: `hew_bytes_clear` is `BorrowsReceiver` in A4 (`runtime_symbols.rs`), `ProvenBorrow` in A3, and the body releases the buffer and resets the caller's triple (`bytes.rs:560-580`, row anchor `bytes:560`, mode `m`). A8 decides `owned(Fresh)` for every `ToStringShim` from the variant alone, with no row for the symbol it names. Section 9 is the single decision point and A1 (`scripts/jit-symbol-classification.toml` -> `ffi_contracts`) is the single projection the lowering reads; A3/A4/A8 become derived views or are deleted with the legacy lowerer (plan §2); if A8 is not on the deletion list it survives P5 as a shadow authority for exactly the catalog symbols revision 1 had no rows for.

**B10. `scripts/verify-ffi-symbols.py` misses conditional exports and does not require hew-std classification**  
`scripts/verify-ffi-symbols.py:190-199` regex needs literal `#[no_mangle]`; `grep -rc 'cfg_attr([^)]*no_mangle' hew-runtime/src` finds 34 attribute sites, 61 symbols, 13 with no unconditional twin (section 1). `:666` excludes hew-std from the completeness check, so 76 hew-std exports (e.g. `hew_json_string_free`, `hew_http_request`, `hew_msgpack_free`) sit in no tier and can never get a contract.

**B11. `retain_string_field_load` is a codegen-side retain with no runtime row**  
`hew-codegen-rs/src/llvm.rs:16698` emits `hew_string_clone` for interior string loads decided from `is_string_const_ty`; the runtime contract of `hew_string_clone` (`R`, same pointer) is correct but the decision to retain is not carried by MIR. Plan §6 deletes it in favour of SIR `copy_value`; section 2a gives `copy_value` its symbol per carrier.

**B12. Codegen reads a task result through two symbols with opposite ownership**  
`grep -rn '"hew_task_get_result"\|"hew_task_take_result"' hew-codegen-rs/src --include=*.rs | grep -v llvm_tests`: `suspend.rs:3521 "hew_task_take_result"` (SuspendingTaskAwait bind: "the load+store MOVES the value representation out of the buffer into the awaiter's binding slot", `:3506-3516`) and `suspend.rs:9765 "hew_task_get_result"` (select `TaskAwait` winner arm: `build_call(task_get_result, ..)` `:9905-9925` then `build_load(dest_ty, reply_ptr)` / `build_store(dest_ptr, reply_val)` into the arm binding `:9976-9990`). `hew-runtime/src/task_scope.rs:756-764` `hew_task_get_result` returns `t.result` without touching `result_consumed`; `:776-784` `hew_task_take_result` sets `result_consumed = true`; `:670-676` `hew_task_free` runs `result_drop_fn(t.result)` when `result_written && !result_consumed`. For an owned `T` the select path double-releases (teardown drops bytes whose heap the binding now owns) or, if MIR treats the binding as a borrow, dangles after `hew_task_free`. The path is latent on main: HIR lowers an `await <expr>` select arm to `HirSelectArmKind::TaskAwait` (`hew-hir/src/lower.rs:21822-21827`) but the checker admits only `Expr::MethodCall` / `Expr::Await(MethodCall)` arm sources (`hew-types/src/check/calls.rs:2702-2723`). P4 re-lowers select and must use the `hew_task_take_result` row (`b` -> `O`, admitted) there; the `hew_task_get_result` row stays `B` and carries this flag.

**B13. The blocking `hew_actor_ask` channel registers no reply destructor**  
`hew-runtime/src/actor.rs:6198-6211`: `let ch = reply_channel::hew_reply_channel_new();` followed directly by `submit_ask_with_reply_channel(..)` with no `hew_reply_channel_set_reply_drop_fn`; the handler thunk discards `hew_reply`'s bool (`hew-codegen-rs/src/thunks.rs:4504-4509`); `reply_channel.rs:620-632`: "When no destructor is registered ... the `false` return signals the caller still owns `value` and must free it". Codegen-created channels all wire the destructor (`wire_reply_drop_fn` at `suspend.rs:5334,6218,10540,11925`, one per `hew_reply_channel_new` site `:5309,6202,10316,11824`); the gap is the runtime-internal channel, so an owned reply to a blocking `hew_actor_ask` leaks (never double-frees) on the cancel/OOM legs. Reachable only on OOM or orphan; fix is the section 5 change (the ask ABI takes the thunk).

**B14. Revision 1's hand-read grade had no reading rule**  
Three sampled `br` rows were wrong in the double-free direction because the reader stopped at the top-level body: `hew_reply` marked `ch` borrowed while `release_sender_ref_if_cancelled` (`reply_channel.rs:368`) and `publish_reply_from_sender_ref` (`:440`) each end in `hew_reply_channel_free(ch)`; `hew_task_set_result` marked `result` borrowed while `hew_task_free` (`task_scope.rs:670-676`) runs `result_drop_fn` on the copied bytes; `hew_rc_new` marked the payload borrowed while `hew_rc_drop` (`rc.rs:181`) runs the stored `drop_fn` on the copied bytes. The same class: `hew_actor_spawn_opts` (`deep_copy_state` + `state_drop_fn` at teardown) and `hew_actor_ask` data (mailbox copy + `message_drop_fn`), both `b` in revision 1 while MIR moves the operand (`lower_value_for_move`) and codegen registers the drop thunk on the runtime side. The reading rule is now written in section 2; every revision 1 hand-read row is `br-` and not admitted until re-read. The review's arity pass (`.tmp/rev2/arity.py`, not re-run here) reported 0 parameter-arity mismatches over the revision 1 rows, so the misreads are mode errors, not shape errors. Revision 3's own fix for `hew_rc_new` then landed the consume on the wrong INDEX (`bcsg`: `size: usize` consumed, payload borrowed) with the arity still right, which the arity pass cannot see — the reading rule now covers the signature, not only the body, and the generator asserts the mode string against the Rust parameter list per index rather than per length.

**B15. Two authorities for the same symbol, with the precedence unstated**  
`awk '/^\[\[ownership.contracts\]\]/{c=1;next} c && /^symbol = /{print; c=0}' scripts/jit-symbol-classification.toml | sed 's/symbol = "//;s/"//' | LC_ALL=C sort -u > /tmp/toml.txt` (568) and `grep -oE '\{n="hew_[a-z0-9_]+"' <this file> | sed 's/{n="//;s/"//' | LC_ALL=C sort -u > /tmp/doc.txt`, then `LC_ALL=C comm -12 /tmp/doc.txt /tmp/toml.txt` printed **40** names on revision 3 — symbols with BOTH an audited contract and a `verified.py` entry, although section 9's header says it lists symbols "without an audited contract". Revision 3's generator let the hand-read entry overwrite the grade unconditionally (`gen_doc.py`, the `if sym in V:` branch), which is exactly the arithmetic 568 - 40 = the printed 528 `au`. 36 of the 40 were demoted to `br-` and therefore NOT admitted, while their TOML rows would have been: `hew_string_drop`, `hew_stream_close`, `hew_channel_{sender,receiver}_close`, `hew_channel_sender_clone`, `hew_string_{builder_new,compare}`, `hew_char_to_string`, `hew_vec_new` and the 13 `hew_vec_new_*` constructors, `hew_vec_clone{,_layout,_owned}`, `hew_vec_get_{owned,str}`, `hew_vec_{push,set}_owned_move`, `hew_vec_take_owned`, `hew_vec_slice_range_str`, `hew_hashmap_iter_{new,free}_layout`, `hew_hashset_iter_{new,free}_layout`. The P1 consequence was concrete: `hew_string_drop` was `br-` (a build error at the call site) while admitted `F` rows carried `rel = hew_string_drop`, so every string `destroy_value` was unbuildable, and every Vec constructor the P2 lane needs was unadmitted although audited. **Resolution (section 2, "Precedence"):** the TOML row is the authority; a `verified.py` entry corroborates it. 37 of the 40 agree through the section 2 mapping and return to `au`; three contradict it and stay blocked with a `[B15]` flag. The three TOML rows to fix, in `scripts/jit-symbol-classification.toml`:

| symbol | TOML says | the body says | fix |
| --- | --- | --- | --- |
| `hew_vec_get_clone` (`:2044` region) | `result = "fresh"`, `release-symbol = "HewTypeLayout.drop_fn"` | returns `bool`; the value lands in `out` and its release is the ELEMENT type's glue (`vec.rs:2752-2800`) | `result = "none"` with an EMPTY `release-symbol`. The out slot's release is not a symbol at all, so neither the TOML nor this table may invent a sentinel for it — the lowering derives it from the slot's static type (section 2's out-slot rule). Revision 3a's `rel="E"` was the same sentinel one layer up; it is deleted from the emitted row and survives only inside `verified.py`, where it is the hand-read fact that proves this divergence |
| `hew_vec_take_owned` | same pair | same shape (`vec.rs:2812-2830`) | same |
| `hew_vec_get_str` (`:2051-2056`) | `result = "retained"` with `result-retention = "transferred"` | `retain_string_element` is a refcount bump returning the SAME pointer (`vec.rs:851-862`) — a share, which the TOML header's own definition calls `shared-refcount` | `result-retention = "shared-refcount"` (this is B5) |

**B16. `hew_rc_new`'s adoption of arg 0 is defined by the glue at arg 3, and one call site passes partial glue**  
`grep -rn '"hew_rc_new"' hew-codegen-rs/src --include=*.rs --exclude=llvm_tests.rs --exclude-dir=tests` names two emission sites with different assumptions about `data`. `llvm.rs:16053-16064` (`Rc::new`) passes `rc_payload_drop_thunk(fn_ctx, payload_ty)`, so the cell adopts the whole payload: `c`, as the row says. `thunks.rs:685-715` (the `SpawnTaskClosure` environment) filters the env fields to `SpawnEnvFieldOwnership::OwnsMoved` and then `let drop_fn = if owned_field_kinds.is_empty() { null_drop } else { get_or_emit_spawn_env_rc_drop_thunk(..) }` — a null glue pointer when nothing is moved, and otherwise a thunk that releases ONLY the moved fields. `BorrowsOnly` is not a BitCopy-only verdict: `hew-mir/src/lower/task.rs:45-56` maps both `ClosureEnvFieldOwnership::BorrowsOnly` and `OwnsClonedOrRetained` to `SpawnEnvFieldOwnership::BorrowsOnly`, and `lower/closure_gen.rs:424-445` mints `BorrowsOnly` for a `Stack` strategy, for a `ScopeOwned` capture that is not a `Move`, and for proven-foreign heap bindings. `hew-mir/src/model.rs:7717-7727` states the intent: "`SpawnTaskClosure` carries both moved fork-call arguments and borrowed scope-owned closure captures. The runtime Rc payload destructor must release only the former." So at that site the same symbol consumes some of `data`'s bytes and borrows others, and `hew_rc_drop` releases nothing at all when the glue is null (`rc.rs:229-233`). A lowering that reads `c` and emits `move %env` double-frees every `BorrowsOnly` heap capture (the cell's thunk skips it, the outer owner destroys it, but the move discharged the outer obligation). **Resolution:** the row's `c` on index 0 means "adopts exactly what the glue at index 3 releases", the section 5 change makes `drop_fn` non-`Option`, and the fresh emitter builds the env from `copy_value`s with total glue. The partial-glue site is drift, in the same sense as the direct-`free` sites in section 2, and must not be reproduced.

**B17. `hew_vec_take_owned` leaves a zeroed slot live, so the drop glue must be a no-op on zeros**  
`hew-runtime/src/vec.rs:2812-2830`: `copy_nonoverlapping(src, out, layout.size)` then `write_bytes(src, 0, layout.size)` and `true`, with **no `len` change**. The slot remains inside the live range, so `drop_element_range` (`:1530-1544`) runs the element `drop_fn(slot)` over the all-zero bytes at the next `hew_vec_free`, `clear` or `set`. Contrast `hew_vec_pop_owned` (`:2937`, `len -= 1` first) and `hew_vec_remove_at_owned` (`:2154`, shift then `len -= 1`), which remove the slot. This is an unstated contract on the P1 per-type glue, and it is not only the drop half: `hew_vec_get_clone` (`:2752-2800`) and `hew_vec_clone_owned` will run the descriptor's `clone_fn` over the same zeroed bytes. Both thunks must tolerate a zeroed representation (enum tag 0, null pointers, zero lengths) — `drop_fn` as a no-op, `clone_fn` as a zero-to-zero copy that reports success. Either the glue spec states it — with a negative test that runs the glue over a zeroed buffer — or the runtime grows a tombstone (mark the slot dead, or swap-remove and decrement `len`) so the glue never sees a zeroed slot. The second is the real fix; the first is what P1 must assume until it lands.

**B18. The libc symbols codegen emits are outside the table's universe**  
`grep -rn --include=*.rs -E 'get_or_declare_libc_malloc\(|get_or_declare_free\(|get_or_declare_libc_free\(' hew-codegen-rs/src | grep -v -E '/tests/|_tests\.rs|fn get_or_declare'` prints 19 production sites (`llvm.rs:6443,6576,18789,19154,22811,32595,33217`; `suspend.rs:5970,6481,6903,7022,7677,8555,8664,9063,9767,11862`; `thunks.rs:348`; `wire.rs:4286`); `suspend.rs:9400-9403` adds `free` directly through `llvm_mod.add_function("free", ..)` and `wire.rs:4440-4446` through `declare_codec_prim(.., "free", ..)`; `grep -rn '"memset"' hew-codegen-rs/src` adds `wire.rs:4039` and `:4344`. Section 1 defines the universe as `hew_*` names, so none of the 23 can have a row, and section 2's defence ("the direct frees are drift") named only 7 of them and said nothing about the 6 `malloc` sites (clone glue `llvm.rs:6443`, lambda env heap `:32595`, supervisor-owned buffers `suspend.rs:7677`/`:8555`, init-thunk state `:9063`, CBOR decode `wire.rs:4286`) or `memset`. The runtime's own allocation rows are not an alternative today: `hew_alloc` is `p="ss"` `r="F"` with no `rel`, `hew_dealloc` is `p="?ss"`, `hew_realloc` is `p="?sss"` `r="?"` — all `bh`/`bh?`, none admitted. **Resolution (section 2 carve-out):** the fresh emitter calls no libc symbol; promoting `hew_alloc`/`hew_dealloc`/`hew_realloc` to `br` is a P1 prerequisite for the lambda-env, supervisor and CBOR paths. The one site that is a protocol rather than drift is `llvm.rs:22811`, the user-extern malloc-string adoption (`llvm.rs:22720-22724`), which B19 owns.

**B19. User-declared `extern "C"` symbols are absent by construction and have no evidence grade**  
`grep -rl --include=*.hew 'extern "C"' tests hew-cli/tests | wc -l` prints 61; `examples/borrow_marker.hew:7-13` declares `hew_log_write_borrow`, `hew_util_strlen`, `hew_util_streq`; `docs/specs/HEW-SPEC-2026.md:2124` §3.9.1 documents user `extern` blocks as language surface. Section 1 admits only `std/**/*.hew` externs and runtime/hew-std definitions, so no row can exist for a user's symbol, and no grade fits a callee with no body we can read (`hd` needs ".hew declaration + body signals", `bh` reads Rust bodies). Read literally the admission rule bans every user FFI call, and the codegen protocol that already exists for them — a `string` result adopted into the header-aware domain and released with libc `free` (`llvm.rs:22720-22724`, caller `:31834`) — has no expressible row. Plan §1.5's default (`borrow` per parameter, `consuming` where declared) is exactly the `hd` grade section 2 refuses for `std/`. **Resolution (section 2 carve-out):** `hd` on a NON-std extern is admitted — the user is the authority for their own C, and there is nothing to re-read; `hd` on a `std/` extern stays blocked, because B6 shows the mark is wrong 17 times against a Rust body we own. The two rules are about different things and the doc now says so.

**B20. `#[extern_symbol]` is a second std-extern source the inventory does not read**  
`grep -rc --include=*.hew 'extern_symbol' std` prints 10/34/24/9 for `std/io.hew`, `std/builtins.hew`, `std/string.hew`, `std/concurrency/lambda_actor.hew` (77 attribute sites, 76 parse, 75 distinct targets: 69 monomorphic plus the 6 `{T}` templates), while `ffi_inventory.py scan_hew` reads only `fn` inside `extern "C" { }` (491 declarations). No symbol is MISSING — all 69 monomorphic targets have rows — but 67 of them carry no `H` provenance in the `v` column, so plan §1.5's "every `extern` declaration in `std/` carries a total ownership row" cannot be checked from this table for those declarations, and their `consume` marks (if any) are never read. Two knock-ons, both recorded in section 1: 12 targets are graded `sy` and were described as compiler-invented names although `std/builtins.hew:240-270` declares them; and the `{T}` templates expand at check time (`hew-types/src/extern_symbol.rs:355-393`, gated by `hew-types/src/vec_authority.rs:422-428`) with no Rust literal anywhere, so the section 1 gate covers them only by coincidence. Fix: `scan_hew` reads the attribute form as well, and the eventual compiler-side symbol dump enumerates template expansions.

**B21. Admission was not transitive: a mint could be admitted while its release was blocked**  
Revision 3a's predicate read `e`/`p`/`r` on one row at a time, so an admitted row could name a release with no admitted row of its own. Over `rev3/out.json` that set was not empty and it hit P1 first: `hew_bytes_from_static` (`{n="hew_bytes_from_static",d="bytes:717",p="bs",r="F",rel="B",tr="A",e="br"}` — section 4's "every bytes literal") was admitted while `{n="hew_bytes_drop",d="bytes:318",p="c",r="N",e="br-",f=["B7"]}` was not, so `destroy_value` of a bytes literal was a build error against an admitted mint; same shape for `hew_rc_new` and `hew_rc_clone` against `hew_rc_drop`, and for `hew_lambda_actor_clone` against `hew_lambda_actor_release`. This is B15's `hew_string_drop` failure one level of indirection out. The single check the doc DID run every time — "an `F` or `R` row with an empty `rel`" — is vacuous against it, and it also cannot see a `k` row with no `rel`, because `hew_bytes_clone_ref`, `hew_cancel_token_retain` and `hew_reply_channel_retain` are all `r="N"`. **Resolution:** admission clauses 2 and 3 in section 2, computed to a fixpoint by the generator, with the failing rows flagged `[B21]`. Seven rows fail today; four of them are `au` rows whose audited `release-symbol` is `hew_sink_close`, a symbol with no audited row of its own (B1's list) — that is a `scripts/jit-symbol-classification.toml` defect, not a table one. `hew_bytes_drop` and `hew_rc_drop` were re-read under the section 2 reading rule in revision 4 rather than left blocked, because P1 needs both (section 8's backlog says which phase needs each of the rest).

Informational: 99 symbols have cfg twins (native / wasm32 / test); 20 of them differ in body signals between twins (rows flagged `tw`). The wasm twins of `hew_actor_*` (other than `hew_actor_send_wire`), `hew_reply_*`, `hew_stream_*` were not hand-read; their native contract is what the `br`/`br-`/`au` rows describe.

## 7. What the SIR lowering reads

- Call edge: for each argument index i, `p[i]` gives the mode; `b`/`s`/`g`/`o`/`t`/`m` leave the caller's `Owned` value alive (`begin_borrow`/`end_borrow` around the call); `o`/`t` define a new `Owned` value from the out slot on the success edge only (the lowering branches on the result first; B3), or a `None`-kinded value when the slot type is BitCopy; `c` is a `move` into the call; `r` is a `copy_value` the callee keeps for itself. `x` has no lowering: the row is not admitted (B3).
- Result: `F`/`R` define an `Owned` value whose `destroy_value` lowers to `rel` **called with the carrier's binding, which is not always the value itself** (section 2a): for `bytes` the value is a `BytesTriple` and the call is `hew_bytes_drop(%v.ptr)`, for every other carrier it is `rel(%v)`, and for the `O` box it is the two-argument `hew_reply_payload_free(box, len)`. A lowering that reads `rel` as "pass the Owned value" cannot emit the P1 bytes release at all, because the runtime symbol takes a `*mut u8` and the value is a three-field struct; `O` defines an `Owned` value of the call's result TYPE obtained by `load.take` from the returned box, after which the box is released with `rel` (or left to arg 0's owner when `rel` is absent). **`O` branches on null first**: every `O` row can return a null box (section 2), so `load.take` is emitted on the non-null edge and the null edge is a fail-closed edge that mints nothing — the shape `suspend.rs:3519-3540` already emits for `hew_task_take_result`; `B` is `Guaranteed` under a borrow of argument 0 that ends at the next mutating call on the same receiver (rows with `RuntimeCallFamily::invalidates_collection_element_aliases`); `H` is `None`; `X` has no lowering (B3). **An `o`/`t` row never spells a release**: the out value is an `Owned` of the slot's static type and its `destroy_value` is type-directed — the element glue for a container element, `hew_string_drop` for a String-kind vec whose `layout` is null, nothing for a BitCopy slot. `rel` on a row always describes the RESULT.
- Retain: `copy_value` of a value whose carrier has a `k="retain"` row lowers to that symbol called with the carrier's binding (section 2a); the new `Owned` value is the result pointer (`R`/`F`) or, on an `N` row, a by-value copy of the operand — for `bytes` that is `hew_bytes_clone_ref(%v.ptr)` and a copy of the whole triple, NOT of arg 0. A carrier with no `k` row (the lambda-actor weak handle today) has no `copy_value` lowering and the call is a build error, not a default.
- Rewrites: a row with `rw` is never emitted by name; the lowering emits the named row's symbol and reads that row's modes.
- Glue: `g` indexes receive the drop-glue function pointer emitted per monomorphic type (plan §1.3), never `null`, for every owning element type.
- Traps: `tr` present means the call can abort/unwind. A `U` row (`hew_trap_with_code`, `hew_cont_destroy`, `hew_cont_resume`, the `C-unwind` Vec accessors) **and a `T` row** need a cleanup edge in the SIR `Suspend`-style terminator so `Owned` values live across it are destroyed on the unwind path (rule 1 counts it as a path). `T` is load-bearing here for a different reason than `U`: `U` is the declared ABI, `T` is the actor-aware bounds bridge, which `panic_any`s into the caller's cleanup pads whenever an actor is stamped (`supervisor.rs:429-433`). A row with `T` and no `U` is still an unwinding call inside an actor. `A` and `P` do not need a cleanup edge — they terminate the process — but `A` on a row means a runtime invariant the emitter must not be able to violate (a missing descriptor or thunk, `vec.rs:2530-2547`), which the plan §1.3 glue makes unreachable rather than caught.

## 8. Maintenance

Generated by the scratchpad scripts (revision 5 copies under `scratchpad/rev6/`; revision 4's under `scratchpad/rev5/`) to be landed as `scripts/ffi-ownership-table/{inventory,verified,gen,emit}.py` with a `make ffi-ownership-table-check` gate: `ffi_inventory.py` (definitions, references, `.hew` externs, TOML), `verified.py` (the `br`/`br-` overrides, one entry per hand-read body with its proof anchor; `rr=True` marks a body read under the section 2 reading rule, `k` a `copy_value` target), `gen_doc.py` (join, `scan_refs_stripped`, cross-checks, the subset assertion against the revision 1 set, the admission fixpoint, counts) + `emit_compact.py` (section 9 rows; the full profile with every cfg-twin anchor, `pf` proof text, `fam` verdicts, tier, `rc`, and all 1535 rows is `emit.py out.json`, which lands COMMITTED beside this file as `docs/internal/runtime-ownership-table-full.md` — revision 4 named it only as a command, so the five redirects to it pointed at nothing a reader could open). Commands: `python3 ffi_inventory.py inv.json`; `python3 rev6/gen_doc.py inv.json rev6/out.json` (prints every count in sections 1 and 2, the new-minus-old set, the residue and the admission bug classes); `python3 rev6/emit_compact.py rev6/out.json` (prints the 518 rows); `python3 emit.py rev6/out.json` (prints the full profile).

**How a row is added, and the rule for a symbol a phase INTRODUCES.** An existing symbol gets a row by having a definition: the inventory scan finds it and mints a `bh`/`bh?` heuristic row. That path cannot serve a symbol that does not exist yet, and the ladder introduces seven (`docs/ir-ladder.md:2469-2476`): `hew_hashmap_insert_layout_move` / `hew_hashset_insert_layout_move` [P2]; `hew_task_take_env`, `hew_arc_release_storage` [P3/P4]; `hew_msg_envelope_take_payload`, `hew_msg_payload_free`, `hew_mailbox_send_aliased_with_reply` [P4, §5.6]. `grep -rl "<sym>" --include=*.rs --include=*.toml --include=*.hew .` from the repo root prints nothing for all seven. A heuristic row on a symbol whose body the lane just wrote is worthless anyway, and admission clause 1 blocks it. **Rule: a phase-introduced symbol lands its definition, its `verified.py` entry with `rr=True` (the body is new, so it is read under the section 2 reading rule by its author and again by the lane's validator), and its `[[ownership.contracts]]` row, in the SAME change** — it is admitted on arrival or the phase does not ship it. The backlog below carries the seven so a phase brief cannot miss them.

**What the gate checks, and why it is not "every referenced symbol is admitted".** That wording was in revision 3a and it cannot be landed: 462 of the 644 compiler-referenced symbols are not admitted today, so the gate would be red on the day it lands and would stay red until the whole backlog cleared. Three checks instead, and the first two are green now:

| check | blocking | today |
| --- | --- | --- |
| membership: every compiler-referenced symbol has a ROW (any grade) | yes | green — the fail-closed direction; a new emitted symbol with no row fails the build of the table, not of a user program |
| regeneration: `emit_compact.py` reproduces section 9 byte for byte, `emit.py` reproduces `docs/internal/runtime-ownership-table-full.md` byte for byte, and `gen_doc.py` reproduces every count in sections 1, 2 and 9 | yes | green — a hand-edited row, a stale count, or a full profile that drifted from the fence fails |
| ratchet: the admitted count does not fall | yes, with an escape | 616; a fall is allowed only when the change table below records the withdrawn rows and why (revision 4 withdraws seven under B21) |

The membership check is the one that protects the compiler: an admitted row is a row READY for `scripts/jit-symbol-classification.toml`, and the phase that needs the symbol lands the TOML edit. The promotion backlog is therefore ordered by phase, not by grade:

| phase | rows it needs promoted | state |
| --- | --- | --- |
| P1 (scalars, `string`, `bytes`, `Rc::new`, drop glue) | `hew_bytes_drop`, `hew_rc_drop` | **done in revision 4** (re-read under the reading rule; `hew_bytes_from_static`, `hew_rc_new`, `hew_rc_clone` are admitted again as a result). Still open for P1: `hew_alloc`/`hew_dealloc`/`hew_realloc` (`bh`/`bh?`, B18) if any P1 path needs a raw allocation |
| P1 (TOML schema) | the `k` axis (`copy_value` target per carrier), the `m` axis (mutate-in-place receiver, B4) and the **binding** axis (which projection a `rel`/`k` symbol is called with — `%v.ptr` for `bytes`, `(box, len)` for `O`; section 2a) have no `[[ownership.contracts]]` spelling | open — a `scripts/jit-symbol-classification.toml` + `hew-types/build.rs` change, not a table change |
| P1 (lowering) | the fresh lowering REFUSES `ExternOwnershipFact::Absent` for every symbol P1 lowers — its own runtime-protocol callees, the leaf releases and retains — and the `std/` `extern` declarations join the same rule at P3 with the `consume` sweep. On main `Absent` falls through to A3/A4 defaults (`lower/facts.rs:1295-1302`, `runtime_call.rs:2025-2040`) | **decided** — `ir-ladder.md` §6.4 (revision 7) carries it as "a missing row is a build error from P1", scoped by what the phase lowers. P1 and P2 do not ship the emitter's own edges unguarded |
| P2 (collections) | `hew_hashmap_free_layout`, `hew_hashset_free_layout`, `hew_hashmap_clone_layout`, `hew_hashset_clone_layout` (`br-`); the three TOML rows in B15 (`hew_vec_get_clone`, `hew_vec_take_owned`, `hew_vec_get_str`); **introduced**: `hew_hashmap_insert_layout_move`, `hew_hashset_insert_layout_move` (definition + `rr=True` entry + TOML row in the same change); **cross-doc**: closed — ladder §5.2 item 6 (revision 7) names `hew_vec_free` for every element class and `hew_vec_free_owned` only as constructor provenance (B4) | open |
| P3 (closures, `dyn`, FFI table, generators) | `hew_arc_new` / `hew_arc_clone` / `hew_arc_drop` (`br`/`br-`, and outside the compact profile until a compiler crate names them); `hew_gen_coro_destroy` AND `hew_cont_frame_free` (`bh?`, `p="?"` — ladder §5.2 item 6 puts both in one generator glue body); `hew_sink_close` (four `au` rows name it as their release and it has none of its own); **introduced**: `hew_task_take_env`, `hew_arc_release_storage` | open |
| P4 (actors, ask/reply, async) | `hew_reply_channel_free`, `hew_cancel_token_release`, `hew_lambda_actor_release`, `hew_msg_envelope_{new,clone_alias,release}`, `hew_mailbox_send_aliased`, `hew_msg_node_free`, `hew_lambda_actor_weak_clone` / `hew_lambda_actor_weak_drop` (the section 2a carrier with no `k`) (`br-`/`bh`); the `x`/`X` rows blocked on B3 (`hew_actor_spawn_opts`, `hew_lambda_actor_new`, `hew_lambda_actor_send`, `hew_reply`, `hew_vec_pop_ptr`), which need a `consume-if(ret)` mode or a runtime ABI change, not a re-read; **introduced**: `hew_msg_envelope_take_payload`, `hew_msg_payload_free`, `hew_mailbox_send_aliased_with_reply` | open |
| unscheduled | the remaining `br-` rows, then the `bh`/`bh?`/`mt` rows | open |

**The `au` rows are owned by `scripts/jit-symbol-classification.toml` and must be edited there** — that sentence is load-bearing, because it IS the B15 precedence: a `verified.py` entry can corroborate an audited row or contradict it, but it can never replace it, and the three contradictions in B15's table are fixed in the TOML, not here.

**What revision 3a changed, and where.** Round-2 review of revision 3 returned 17 findings; all 17 reproduced against main `54e8dde2c`, none was refuted. The generator diff:

| file | change | why |
| --- | --- | --- |
| `ffi_inventory.py` `body_signals` | trap classes: `abort` narrowed to unconditional aborts plus the always-aborting Vec descriptor helpers; new `bounds-trap` class for `abort_oob`/`abort_pop_empty`/`runtime_bounds_trap`/`hew_trap_with_code`; `abort_` prefix removed from the `assert/panic` regex | section 2's `T` letter; the `abort_` prefix was matching bridge helpers and macro bodies with no panic in them |
| `rev3/gen_doc.py` `TRAP_CODE` | `+ "bounds-trap": "T"` | same |
| `rev3/gen_doc.py` row build | audited-row precedence: `toml_handread_divergence()` decides `au` (corroborated) vs `br-` + `[B15]` (contradicted) instead of the unconditional hand-read override | B15 |
| `rev3/gen_doc.py` row build | per-index mode check: an ownership mode on a scalar parameter, or a non-glue mode on a glue parameter, is a flagged bug (`bugs["mode-index"]`) | B14/B16; a negative control re-adding `hew_rc_new` with `p="bcsg"` prints "verified.py p1 size: mode c on a scalar (usize)", and the current table prints nothing |
| `rev3/verified.py` | 14 corrected or new entries in a `revision 3a` block at the end (last write wins): `hew_rc_new`/`hew_arc_new` `cssg`; `hew_task_set_env` `bc`; `hew_task_get_env` `r="B"`; `hew_weak_drop_rc` `c`; trap letters on `hew_task_free`, `hew_vec_{get_clone,take_owned,pop_owned,pop_ptr,pop_str,get_str,remove_at_str,remove_at_owned}` | sections 2 and 4 |

Row-level blast radius, `diff` of the revision 3 section 9 against this one: 37 rows leave section 9 for `au` (B15), 50 rows change only the `tr` column (the `T` letter and the narrowed `A`), and 11 rows change substantively — `hew_rc_new`, `hew_task_set_env`, `hew_task_get_env`, `hew_weak_drop_rc`, `hew_vec_pop_owned`, `hew_vec_pop_str`, `hew_vec_remove_at_str`, `hew_vec_remove_at_owned` (all now admitted or re-graded on read evidence) and `hew_vec_get_clone`, `hew_vec_get_str`, `hew_vec_take_owned` (blocked on the three TOML rows in B15). No row was added and none was deleted. The ratchet moves 581 -> 621, which is 41 rows gained and one lost, computed by re-running the revision 3 generator from the backups in `scratchpad/verify/oldgen/` (it reproduces `au 528, br 58, admitted 581` exactly) and diffing the admission predicate over the two `out.json` files. The 41: 34 audited rows that stopped being demoted (B15) and 7 newly re-read bodies (`hew_arc_new`, `hew_task_set_env`, `hew_task_get_env`, `hew_weak_drop_rc`, `hew_vec_pop_owned`, `hew_vec_pop_str`, `hew_vec_remove_at_str`). The one lost is `hew_vec_get_clone`, which revision 3 admitted on a hand-read that contradicts its audited row. **The ratchet is not a goal**: this movement is a correction to how it was counted, plus one row correctly withdrawn, and a fix to the three TOML rows in B15 moves it again.

**What revision 4 changed, and where.** Round-4 review of revision 3a returned 16 findings. Fifteen reproduced against main `54e8dde2c`; one was refuted — that section 9 omits the nine TOML-only symbols named in the status line is section 9's own `au` exclusion rule, not a contradiction, and the status line is rewritten to say which profile prints what rather than to change the arithmetic. Four of the fifteen carried a sub-claim that did not survive checking: `hew_rc_clone` and `hew_lambda_actor_clone` do carry `rel` (only `hew_bytes_clone_ref`, `hew_cancel_token_retain` and `hew_reply_channel_retain` did not); `hew_cancel_token_new_child` and `hew_string_clone` do have rows, in the full profile, and `hew_string_clone` already carried `k="retain"` there; `hew_msg_envelope_{new,clone_alias,release}`, `hew_mailbox_send_aliased` and `hew_msg_node_free` have full-profile rows and only `hew_layout_string_drop` has none, correctly. The generator diff:

| file | change | why |
| --- | --- | --- |
| `ffi_inventory.py` `body_signals` | the always-aborting helper list gains `abort_layout_aware_operation` / `abort_if_layout_aware` / `ensure_cap` (`vec.rs:161,213,62`) | `hew_vec_remove_at_str` is an admitted `br` row whose first statement reaches `libc::abort`, and section 2's reading rule item (4) requires every trap reached through helpers |
| `rev5/gen_doc.py` `heur_param` | a `retain` body signal no longer mints param mode `r`; it falls through to `b` | `hew_bytes_slice` hands its +1 back to the caller inside the returned triple, which is the `k` shape, not `r`; only a TOML `retain` or a hand-read may write `r` |
| `rev5/gen_doc.py` `scan_refs_stripped` | the scan walks all non-test `hew-types/src/**` and `hew-hir/src/**` instead of three named files plus a prose probe | eleven `hew-types/src` files carry `hew_*` literals naming real runtime symbols and were outside the declared rule |
| `rev5/gen_doc.py` row build | `rel` is stripped from every `o`/`t` row and the generator asserts none carries it | the `rel="E"` sentinel was on 3 of 45 out-slot rows by hand; the out value's release is type-directed (section 7) |
| `rev5/gen_doc.py` admission | clause 2 (a `k` row names its `rel`) and clause 3 (the `rel` target is itself admitted, to a fixpoint), flagged `[B21]`, with `admitted_base` printed beside `admitted` | B21 |
| `rev5/verified.py` | a `revision 3b` block (last write wins): `rel` on the three retain rows; `hew_bytes_drop` and `hew_rc_drop` re-read with `rr=True`; `hew_vec_remove_at_str` `tr="UAT"`; `hew_task_set_result` `tr="PG"`; `k="retain"` on `hew_channel_sender_clone`, `k="clone"` on `hew_vec_clone`, `hew_vec_clone_owned`, `hew_hashmap_clone_layout`, `hew_hashset_clone_layout` | sections 2, 2a, 4 |

Row-level blast radius, `diff` of the revision 3a section 9 against this one: 28 rows change and none is added or deleted. 25 change only `tr` (the layout-aware abort: `hew_vec_remove_at_*`, `hew_vec_slice_range_*`, `hew_vec_{contains,equals}_thunk`, `hew_vec_{get,set,pop}_generic`, `hew_vec_from_u8_data`, `hew_vec_contains_layout`, plus `hew_task_set_result`'s missing `G`); `hew_bytes_slice` changes `p="rssss"` to `p="bssss"`; `hew_bytes_drop` and `hew_rc_drop` move `br-` -> `br`; three rows gain `rel`, three lose the `E` sentinel, five gain `k`. Counts: compiler-referenced 639 -> 644, with-contract 130 -> 133, without 509 -> 511, distinct literals 835 -> 840, grades `br` 61 -> 63 and `br-` 106 -> 104. The universe stays 1535, the compact profile stays 1083, and section 9 stays at 518 rows (`br` 61, `br-` 85, `bh` 153, `bh?` 131, `mt` 44, `st` 21, `sy` 23).

**The ratchet moves 621 -> 616, and it is recomputable from the two profiles.** Clause 1 alone gives 623 (621 plus `hew_bytes_drop` and `hew_rc_drop`); clauses 2-3 withdraw seven (B21). The 616 splits 561 `au` + 55 `br`. Of the 55 admitted `br` rows, **54 are in section 9's fence and one is not**: `hew_actor_send_wire`, which no compiler crate names (section 1's `never_referenced` bucket) and which therefore has no row in the compact profile. Revision 3a's 621 could not be reconciled against its own fence for the same reason plus `hew_arc_new`, which was admitted then and is withdrawn now. The four `au` rows that lose admission are `hew_http_respond_stream`, `hew_stream_from_file_write`, `hew_stream_pair_sink` and `hew_stream_pair_sink_bytes`; the three `br` rows are `hew_arc_new`, `hew_lambda_actor_clone` and `hew_reply_channel_retain`.

**What revision 5 changed, and where.** Round-5 review of revision 4 returned 15 findings. All 15 reproduced against main `54e8dde2c`; none was refuted. Two carried a sub-claim that did not survive checking, and both are corrections to the FINDING, not to the fix:

- The external-test-mod gap resolves **15** `#[cfg(test)] mod <name>;` targets, not the 4 the finding listed; **5 of the 15 were already excluded** by the `_tests.rs` / `/test` path rules (`stale_owner_canonicalization_tests.rs`, `synthesized_identity_tests.rs`, `replay_plan_tests.rs`, `handle_borrowing_call_abi_tests.rs`, `hew-types/src/check/tests/mod.rs`), so **10 files are newly excluded**. The finding's own conclusion holds: re-running the scanner with and without them gives 840 literals both ways, so no count and no row moves.
- `hew_bytes_push` printed no `tr` for a different reason from `hew_bytes_append`. The finding read both as scanner blindness; the inventory dump shows `hew_bytes_push` already classified as `abort` (`ffi_inventory.py` sees the literal `alloc_buf(` in its first arm), and the empty column came from the revision-3b `verified.py` entry leaving `trap` at its `"-"` default. The scanner fix covers `append`/`index`/`slice`/`pop`; the `verified.py` fix covers `push`. Section 2's trap caveat now names all three grades instead of only `au`.

One anchor was wrong in passing and is corrected: `hew_cancel_token_new_child` is `task_scope.rs:199`, not `:189`.

The generator diff:

| file | change | why |
| --- | --- | --- |
| `ffi_inventory.py` `body_signals` | the always-aborting helper list gains `alloc_buf` / `realloc_buf` / `ensure_unique`; the bounds-bridge list gains `bytes_bounds_trap` / `bytes_index_oob_trap` / `bytes_slice_oob_trap` / `bytes_offset_overflow_trap` / `hew_bytes_abort_empty_pop` | four bytes rows under-reported `A` or `T`; section 2's reading rule item (4) requires every trap reached through a helper, and the helper list is the shortcut that implements it |
| `rev6/gen_doc.py` `heur_param` | a `&mut T` Rust parameter returns `m` before any signal class is consulted | `b` promises the caller's value is unchanged across the call; three `&mut BytesTriple` receivers printed `b` while their `ensure_unique` fork releases one owner of the old buffer and rewrites the caller's slot |
| `rev6/gen_doc.py` `external_test_mod_files` | files named by a `#[cfg(test)] mod <name>;` declaration are dropped from the reference scan | `strip_test_mods` cut only inline brace-matched blocks, so a test module in its own file was scanned as production source |
| `rev6/verified.py` | a `revision 5` block (last write wins): `hew_weak_clone_rc` corroborated with `k="retain"`; `hew_bytes_push` gains `trap="A"` | section 2a had no weak carrier although codegen emits `hew_weak_clone_rc` for `Op::WeakClone` and actor-state weak field clones; the push entry's empty trap column |

Row-level blast radius, `diff` of the revision 4 section 9 against this one: **6 rows change and none is added or deleted.** `hew_bytes_append` `p="bbss"` -> `p="mbss"` and gains `tr="A"` and `[B2]`; `hew_bytes_pop` `p="b"` -> `p="m"`, `tr="U"` -> `tr="UAT"`, gains `[B2]`; `hew_bytes_set` `p="bss"` -> `p="mss"`, `tr="UT"` -> `tr="UAT"`, gains `[B2]`; `hew_bytes_index` and `hew_bytes_slice` `tr="U"` -> `tr="UT"`; `hew_bytes_push` gains `tr="A"`. `hew_bytes_pop`'s `A` and `T` are a consequence of the helper rule rather than of a finding — the finding named `append`/`push` for `A` and `index`/`slice` for `T` — and both are read: `pop` reaches `libc::abort` through `ensure_unique` -> `alloc_buf` and `runtime_bounds_trap` through `hew_bytes_abort_empty_pop` -> `bytes_bounds_trap`.

**The ratchet does not move: 616 -> 616.** All six changed rows are `bh` or `br-` and blocked by admission clause 1 either way, and `hew_weak_clone_rc` was already an admitted `au` row. Every count in sections 1 and 2 reproduces unchanged (universe 1535, compiler-referenced 644, with-contract 133, without 511, distinct literals 840, residue 196, compact 1083, fence 518, grades `au` 565 / `br` 63 / `br-` 104 / `bh` 439 / `bh?` 269 / `mt` 48 / `st` 24 / `sy` 23, `admitted_base` 623, admitted 616 = 561 `au` + 55 `br`, 54 of them in the fence). Two generator bug-class counts move as a consequence: `family-vs-table` 20 -> 23 (the three new `m` receivers join `hew_bytes_clear`/`hew_bytes_push` as A3 `ProvenBorrow` disagreements, B2) and `toml-corroborated` 37 -> 38 (`hew_weak_clone_rc`).

Nothing in revision 5 is a re-read that promotes a row; it raised four architect decisions instead, and revision 6 records their answers. Three are landed in `ir-ladder.md` revision 7 and this document follows them: the `Absent` refusal lands at **P1** for every symbol P1 lowers, with `std/` declarations joining at P3 (§6.4; section 8's P1-lowering row above); the Vec leaf release is **`hew_vec_free`** for every element class (§5.2 item 6; B4); and `runtime_symbols.rs` is the **one symbol table**, carrying spelling and ownership together with the TOML generated into it, while `callee_ownership_contract` goes with the legacy lowerer (§5.1, §9; section 3's A4 note above). The fourth is this document's own and is still open: where the full profile is committed — `docs/internal/runtime-ownership-table-full.md` is named by section 8 and by five redirects, and the generator that emits it is still in a scratchpad rather than at `scripts/ffi-ownership-table/`.

## 9. Table

Rows with `e = "au"` (565 compiler-referenced or `.hew`-declared symbols) are the `[[ownership.contracts]]` rows of `scripts/jit-symbol-classification.toml` mapped through section 2 and are not repeated here; the full profile lists them. Below: every compiler-referenced or `.hew`-declared symbol **whose audited contract is absent or contradicted** (518 rows: br 61, br- 85, bh 153, bh? 131, mt 44, st 21, sy 23). Of these, 54 are admitted; the 55th admitted `br` row of the universe (`hew_actor_send_wire`) is unreferenced and lives in the full profile. Revision 3 said "without an audited contract", which was not what its generator produced — 40 audited symbols appeared here anyway (B15). Legend in section 2; `k` and `rw` were new in revision 3, the `T` trap letter and the `[B15]`/`[B16]`/`[B17]` flags in 3a, the `[B21]` flag and `k="clone"` in revision 4; the `rel="E"` sentinel that revision 3 introduced is deleted (section 2's out-slot rule). Revision 5 changes six bytes rows and no others (section 8): the `&mut` receiver rule writes `m` where the heuristic wrote `b`, and the bytes helper list restores the `A` and `T` trap letters.

```toml
# runtime-ownership-table v5 (compact: compiler-referenced or .hew-declared symbols whose audited contract is absent or contradicted). d = file:line under hew-runtime/src/ (std/ prefix = hew-std/src/), e = evidence grade, p omitted when there are no parameters. Generated by rev6/emit_compact.py; do not hand-edit.
sym = [
{n="hew_actor_ask",d="actor:6198",p="bsxs",r="O",rel="hew_reply_payload_free",e="br-",f=["B3","O","B13"]},
{n="hew_actor_ask_take_last_error",d="actor:546",r="S",e="bh"},
{n="hew_actor_ask_with_channel",d="actor:6351",p="?s?sb",r="S",e="bh?",f=["tw"]},
{n="hew_actor_cooperate",d="scheduler:4423",r="S",e="bh"},
{n="hew_actor_demonitor",d="monitor:1069",p="s",r="N",e="br-"},
{n="hew_actor_exit_unhandled",d="actor:7034",p="s",r="N",tr="UT",e="bh"},
{n="hew_actor_gen_sink_complete",d="actor:6615",p="bb",r="N",e="bh"},
{n="hew_actor_gen_sink_register",d="actor:6582",p="bb",r="N",e="bh"},
{n="hew_actor_link",d="link:78",p="bb",r="N",e="bh"},
{n="hew_actor_monitor",d="monitor:862",p="bbo",r="S",e="bh"},
{n="hew_actor_park_lifecycle_cont",d="scheduler:2155",p="b?",r="S",e="bh?"},
{n="hew_actor_pid",d="actor:7251",p="b",r="S",tr="G",e="bh"},
{n="hew_actor_register_type",d="actor:5204",p="?b",r="N",e="bh?",f=["tw"]},
{n="hew_actor_schedule_periodic",d="timer_periodic:571",p="?ss",r="?",e="bh?"},
{n="hew_actor_self",d="actor:6992",r="?",e="bh?"},
{n="hew_actor_send_by_id",d="actor:4026",p="s?s?s",r="S",e="bh?"},
{n="hew_actor_set_message_drop",d="actor:5489",p="bg",r="N",tr="G",g="1",e="br-"},
{n="hew_actor_set_state_clone",d="actor:5531",p="bg",r="N",tr="G",g="1",e="br-"},
{n="hew_actor_set_state_drop",d="actor:5466",p="bg",r="N",tr="G",g="1",e="br-"},
{n="hew_actor_set_terminate",d="actor:5409",p="bg",r="N",tr="G",g="1",e="bh"},
{n="hew_actor_spawn",d="actor:3449",p="?s?",r="?",e="bh?"},
{n="hew_actor_spawn_opts",d="actor:3494",p="x",r="H",e="br",f=["B3"]},
{n="hew_actor_state_lock_acquire",d="actor:745",p="?",r="S",tr="G",e="bh?"},
{n="hew_actor_state_lock_release",d="actor:762",p="?",r="S",tr="G",e="bh?"},
{n="hew_actor_unlink",d="link:146",p="bb",r="N",e="bh"},
{n="hew_alloc",d="mem:114",p="ss",r="F",tr="P",e="bh"},
{n="hew_assert",d="assert:71",p="s",r="N",tr="A",e="br"},
{n="hew_assert_eq_bool",d="assert:217",p="ss",r="N",tr="A",e="br"},
{n="hew_assert_eq_f64",d="assert:157",p="ss",r="N",tr="A",e="br"},
{n="hew_assert_eq_i16",d="assert:49",p="ss",r="N",tr="A",e="br"},
{n="hew_assert_eq_i32",d="assert:50",p="ss",r="N",tr="A",e="br"},
{n="hew_assert_eq_i64",d="assert:85",p="ss",r="N",tr="A",e="br"},
{n="hew_assert_eq_i8",d="assert:48",p="ss",r="N",tr="A",e="br"},
{n="hew_assert_eq_isize",d="assert:51",p="ss",r="N",tr="A",e="br"},
{n="hew_assert_eq_str",d="assert:114",p="bb",r="N",tr="A",e="br"},
{n="hew_assert_eq_u16",d="assert:52",p="ss",r="N",tr="A",e="br"},
{n="hew_assert_eq_u32",d="assert:53",p="ss",r="N",tr="A",e="br"},
{n="hew_assert_eq_u64",d="assert:54",p="ss",r="N",tr="A",e="br"},
{n="hew_assert_eq_u8",d="assert:189",p="ss",r="N",tr="A",e="br"},
{n="hew_assert_eq_usize",d="assert:55",p="ss",r="N",tr="A",e="br"},
{n="hew_assert_ne_bool",d="assert:233",p="ss",r="N",tr="A",e="br"},
{n="hew_assert_ne_f64",d="assert:175",p="ss",r="N",tr="A",e="br"},
{n="hew_assert_ne_i16",d="assert:57",p="ss",r="N",tr="A",e="br"},
{n="hew_assert_ne_i32",d="assert:58",p="ss",r="N",tr="A",e="br"},
{n="hew_assert_ne_i64",d="assert:99",p="ss",r="N",tr="A",e="br"},
{n="hew_assert_ne_i8",d="assert:56",p="ss",r="N",tr="A",e="br"},
{n="hew_assert_ne_isize",d="assert:59",p="ss",r="N",tr="A",e="br"},
{n="hew_assert_ne_str",d="assert:135",p="bb",r="N",tr="A",e="br"},
{n="hew_assert_ne_u16",d="assert:60",p="ss",r="N",tr="A",e="br"},
{n="hew_assert_ne_u32",d="assert:61",p="ss",r="N",tr="A",e="br"},
{n="hew_assert_ne_u64",d="assert:62",p="ss",r="N",tr="A",e="br"},
{n="hew_assert_ne_u8",d="assert:203",p="ss",r="N",tr="A",e="br"},
{n="hew_assert_ne_usize",d="assert:63",p="ss",r="N",tr="A",e="br"},
{n="hew_auto_mutex_alloc",d="auto_mutex:106",r="F",e="bh"},
{n="hew_auto_mutex_free",d="auto_mutex:216",p="c",r="N",e="bh",f=["B2"]},
{n="hew_auto_mutex_lock",d="auto_mutex:135",p="b",r="N",tr="P",e="bh"},
{n="hew_auto_mutex_unlock",d="auto_mutex:183",p="b",r="N",tr="P",e="bh"},
{n="hew_await_cancel_cancel",d="await_cancel:360",p="?ss",r="S",e="bh?"},
{n="hew_await_cancel_complete",d="await_cancel:346",p="?",r="S",e="bh?"},
{n="hew_await_cancel_free",d="await_cancel:300",p="c",r="N",e="bh"},
{n="hew_await_cancel_new",d="await_cancel:246",p="?g?",r="F",g="1",e="bh?"},
{n="hew_await_cancel_schedule_deadline_ms",d="await_cancel:384",p="b?s",r="S",e="bh?"},
{n="hew_await_cancel_status",d="await_cancel:332",p="b",r="S",e="bh"},
{n="hew_bool_to_string",d="string:592",p="s",r="F",e="bh"},
{n="hew_bytes_append",d="bytes:581",p="mbss",r="N",tr="A",e="bh",f=["B2"]},
{n="hew_bytes_clear",d="bytes:560",p="m",r="N",e="br-",f=["B2"]},
{n="hew_bytes_clone_ref",d="bytes:294",p="b",r="N",rel="B",tr="A",k="retain",e="br"},
{n="hew_bytes_contains",d="bytes:530",p="bs",r="S",e="bh"},
{n="hew_bytes_drop",d="bytes:318",p="c",r="N",e="br",f=["B7"]},
{n="hew_bytes_from_static",d="bytes:717",p="bs",r="F",rel="B",tr="A",e="br"},
{n="hew_bytes_get",r="?",e="sy"},
{n="hew_bytes_index",d="bytes:1018",p="bsss",r="S",tr="UT",e="bh"},
{n="hew_bytes_is_empty",d="bytes:515",p="b",r="S",e="bh"},
{n="hew_bytes_len",d="bytes:895",p="b",r="S",e="bh"},
{n="hew_bytes_pop",d="bytes:427",p="m",r="S",tr="UAT",e="bh",f=["B2"]},
{n="hew_bytes_push",d="bytes:347",p="ms",r="N",tr="A",e="br-",f=["B2"]},
{n="hew_bytes_set",d="bytes:462",p="mss",r="N",tr="UAT",e="bh",f=["B2"]},
{n="hew_bytes_slice",d="bytes:1071",p="bssss",r="R",tr="UT",e="bh"},
{n="hew_cancel_token_is_requested",d="task_scope:273",p="?",r="S",tr="G",e="bh?"},
{n="hew_cancel_token_release",d="task_scope:183",p="c",r="N",e="br-",f=["B2"]},
{n="hew_cancel_token_retain",d="task_scope:166",p="b",r="N",rel="hew_cancel_token_release",k="retain",e="br-"},
{n="hew_cbor_de_array_next",d="cbor_serial:867",p="?",r="S",e="bh?"},
{n="hew_cbor_de_bool",d="cbor_serial:1198",p="?",r="S",e="bh?"},
{n="hew_cbor_de_bytes",d="cbor_serial:1251",p="?o",r="F",e="bh?"},
{n="hew_cbor_de_char",d="cbor_serial:1152",p="?",r="S",e="bh?"},
{n="hew_cbor_de_enter_array",d="cbor_serial:843",p="?",r="N",e="bh?"},
{n="hew_cbor_de_enter_map",d="cbor_serial:634",p="?",r="N",e="bh?"},
{n="hew_cbor_de_enum_begin",d="cbor_serial:924",p="?",r="S",e="bh?"},
{n="hew_cbor_de_enum_end",d="cbor_serial:990",p="?",r="N",e="bh?"},
{n="hew_cbor_de_exit_array",d="cbor_serial:894",p="?",r="N",e="bh?"},
{n="hew_cbor_de_exit_map",d="cbor_serial:664",p="?",r="N",e="bh?"},
{n="hew_cbor_de_f64",d="cbor_serial:1180",p="?",r="S",e="bh?"},
{n="hew_cbor_de_fail",d="cbor_serial:608",p="?",r="N",e="bh?"},
{n="hew_cbor_de_failed",d="cbor_serial:587",p="b",r="S",e="br"},
{n="hew_cbor_de_free",d="cbor_serial:620",p="c",r="N",e="bh"},
{n="hew_cbor_de_int_checked",d="cbor_serial:1111",p="?ss",r="S",e="bh?"},
{n="hew_cbor_de_is_null",d="cbor_serial:1018",p="?",r="S",e="bh?"},
{n="hew_cbor_de_new",d="cbor_serial:555",p="bs",r="F",e="bh"},
{n="hew_cbor_de_select_key",d="cbor_serial:682",p="?s",r="N",e="bh?"},
{n="hew_cbor_de_skip",d="cbor_serial:1039",p="?",r="N",e="bh?"},
{n="hew_cbor_de_string",d="cbor_serial:1217",p="?",r="F",e="bh?"},
{n="hew_cbor_ser_begin_array",d="cbor_serial:224",p="?",r="N",e="bh?"},
{n="hew_cbor_ser_begin_map",d="cbor_serial:160",p="?",r="N",e="bh?"},
{n="hew_cbor_ser_bool",d="cbor_serial:371",p="?s",r="N",e="bh?"},
{n="hew_cbor_ser_bytes",d="cbor_serial:418",p="?bss",r="N",e="bh?"},
{n="hew_cbor_ser_end_array",d="cbor_serial:255",p="?",r="N",e="bh?"},
{n="hew_cbor_ser_end_map",d="cbor_serial:183",p="?",r="N",e="bh?"},
{n="hew_cbor_ser_f64",d="cbor_serial:359",p="?s",r="N",e="bh?"},
{n="hew_cbor_ser_finish",d="cbor_serial:448",p="co",r="F",e="bh"},
{n="hew_cbor_ser_i64",d="cbor_serial:335",p="bs",r="N",e="br"},
{n="hew_cbor_ser_key_u64",d="cbor_serial:293",p="?s",r="N",e="bh?"},
{n="hew_cbor_ser_new",d="cbor_serial:151",r="F",e="bh"},
{n="hew_cbor_ser_null",d="cbor_serial:383",p="?",r="N",e="bh?"},
{n="hew_cbor_ser_string",d="cbor_serial:397",p="?b",r="N",e="bh?"},
{n="hew_cbor_ser_u64",d="cbor_serial:347",p="?s",r="N",e="bh?"},
{n="hew_channel_await_recv",d="channel:492",p="b??",r="S",e="bh?"},
{n="hew_channel_cancel_pending_read",d="channel:665",p="bs",r="N",e="bh"},
{n="hew_channel_detach_recv",d="channel:513",p="b?",r="N",e="bh?"},
{n="hew_channel_poll",d="channel:572",p="bg?g",r="S",tr="A",g="13",e="bh?"},
{n="hew_channel_recv_cancel_cleanup",d="channel:726",p="bs",r="N",e="bh"},
{n="hew_channel_recv_layout",d="channel:351",p="bog",r="S",tr="G",g="2",e="br-",f=["tw"]},
{n="hew_channel_send_layout",d="channel:316",p="bxg",r="N",tr="AG",g="2",e="br-",f=["B3"]},
{n="hew_channel_try_recv_layout",d="channel:378",p="b?g",r="S",tr="G",g="2",e="bh?"},
{n="hew_conn_await_read",d="transport:3453",p="s??",r="S",e="bh?"},
{n="hew_cont_crash_cleanup_arm",d="cont:997",p="s?ssgss",r="S",g="4",e="bh?"},
{n="hew_cont_crash_cleanup_deactivate",d="cont:1124",p="s",r="S",e="bh"},
{n="hew_cont_crash_cleanup_retire",d="cont:1162",p="s",r="S",e="br"},
{n="hew_cont_destroy",d="cont:1969",p="c",r="N",tr="U",e="br-"},
{n="hew_cont_done",d="cont:1901",p="?",r="S",e="bh?"},
{n="hew_cont_frame_alloc",d="cont:473",p="s",r="?",e="bh?"},
{n="hew_cont_frame_alloc_tracked",d="cont:493",p="s",r="?",e="bh?"},
{n="hew_cont_frame_free",d="cont:574",p="?",r="N",e="bh?"},
{n="hew_cont_frame_handoff",d="cont:1751",p="?",r="N",e="bh?"},
{n="hew_cont_poll",d="cont:1934",p="??",r="S",e="bh?"},
{n="hew_cont_resume",d="cont:1872",p="?",r="N",tr="U",e="bh?"},
{n="hew_context_install",d="execution_context:403",p="b",r="?",e="bh?"},
{n="hew_context_reply_channel_swap_pop",d="execution_context:535",r="N",e="bh"},
{n="hew_context_reply_channel_swap_push",d="execution_context:505",p="?",r="N",e="bh?"},
{n="hew_context_restore",d="execution_context:410",p="b",r="N",e="bh"},
{n="hew_cron_next",d="std/time/cron:124",p="bso",r="S",e="br"},
{n="hew_dealloc",d="mem:210",p="?ss",r="N",tr="P",e="bh?"},
{n="hew_duplex_clone",d="duplex:1571",p="b",r="F",e="bh"},
{n="hew_duplex_close",d="duplex:1131",p="c",r="S",e="br-"},
{n="hew_duplex_close_half",d="duplex:1496",p="cs",r="S",e="br-"},
{n="hew_duplex_pair",d="duplex:783",p="ssoo",r="S",e="bh"},
{n="hew_duplex_payload_free",d="duplex:1092",p="bs",r="N",e="bh"},
{n="hew_duplex_recv",d="duplex:953",p="boo",r="S",e="bh"},
{n="hew_duplex_recv_half",d="duplex:1235",p="c",r="F",rel="hew_duplex_close_half",e="br-"},
{n="hew_duplex_send",d="duplex:834",p="bbs",r="S",e="bh"},
{n="hew_duplex_send_half",d="duplex:1189",p="c",r="F",rel="hew_duplex_close_half",e="br-"},
{n="hew_duplex_try_recv",d="duplex:1031",p="boo",r="S",e="bh"},
{n="hew_duplex_try_send",d="duplex:904",p="bbs",r="S",e="bh"},
{n="hew_duration_abs",d="io_time:222",p="s",r="S",e="bh"},
{n="hew_duration_hours",d="io_time:212",p="s",r="S",e="bh"},
{n="hew_duration_is_zero",d="io_time:232",p="s",r="S",e="bh"},
{n="hew_duration_micros",d="io_time:172",p="s",r="S",e="bh"},
{n="hew_duration_millis",d="io_time:182",p="s",r="S",e="bh"},
{n="hew_duration_mins",d="io_time:202",p="s",r="S",e="bh"},
{n="hew_duration_nanos",d="io_time:162",p="s",r="S",e="bh"},
{n="hew_duration_secs",d="io_time:192",p="s",r="S",e="bh"},
{n="hew_dyn_box_alloc",d="trait_object:318",p="ss",r="F",rel="hew_dyn_box_free",tr="P",e="br-"},
{n="hew_dyn_box_free",d="trait_object:380",p="css",r="N",tr="P",e="br-",f=["B2"]},
{n="hew_exit",d="lib:182",p="s",r="N",e="bh"},
{n="hew_float_to_string",d="string:559",p="s",r="F",e="bh"},
{n="hew_gen_coro_destroy",d="cont:2058",p="c",r="N",tr="U",e="br-"},
{n="hew_get_reply_channel",d="execution_context:431",r="?",e="bh?"},
{n="hew_global_timer_wheel",d="timer_periodic:124",r="?",e="bh?"},
{n="hew_hashmap_clear_layout",d="hashmap:1746",p="b",r="N",e="br-"},
{n="hew_hashmap_clone_layout",d="hashmap:1008",p="b",r="F",rel="M",tr="A",k="clone",e="br-"},
{n="hew_hashmap_contains_key_layout",d="hashmap:1440",p="bb",r="S",tr="A",e="br-"},
{n="hew_hashmap_free_layout",d="hashmap:1672",p="c",r="N",e="br-",f=["B2"]},
{n="hew_hashmap_get_clone_layout",d="hashmap:1379",p="bbt",r="S",tr="A",e="br-"},
{n="hew_hashmap_get_layout",d="hashmap:1327",p="bb",r="B",tr="A",rw="hew_hashmap_get_clone_layout",e="br-"},
{n="hew_hashmap_insert_layout",d="hashmap:1174",p="bxc",r="S",tr="A",e="br-",f=["B3","B2"]},
{n="hew_hashmap_keys_layout",d="hashmap:1838",p="b",r="F",tr="A",e="bh"},
{n="hew_hashmap_len_layout",d="hashmap:1633",p="b",r="S",e="br-"},
{n="hew_hashmap_new_with_layout",d="hashmap:851",p="gg",r="F",rel="M",tr="A",g="01",e="br-"},
{n="hew_hashmap_remove_layout",d="hashmap:1455",p="bb",r="S",tr="A",e="br-"},
{n="hew_hashmap_remove_take_layout",d="hashmap:1552",p="bbo",r="S",tr="A",e="br-"},
{n="hew_hashmap_values_layout",d="hashmap:2024",p="b",r="F",tr="A",e="bh"},
{n="hew_hashset_clear_layout",d="hashset:385",p="b",r="N",tr="A",e="br-"},
{n="hew_hashset_clone_layout",d="hashset:477",p="b",r="F",rel="HS",tr="A",k="clone",e="br-"},
{n="hew_hashset_contains_layout",d="hashset:338",p="b?",r="S",e="bh?"},
{n="hew_hashset_free_layout",d="hashset:523",p="c",r="N",e="br-",f=["B2"]},
{n="hew_hashset_insert_layout",d="hashset:314",p="bx",r="S",tr="A",e="br-",f=["B3","B2"]},
{n="hew_hashset_is_empty_layout",d="hashset:419",p="?",r="S",e="bh?"},
{n="hew_hashset_len_layout",d="hashset:404",p="b",r="S",e="bh"},
{n="hew_hashset_new_with_layout",d="hashset:245",p="g",r="F",rel="HS",tr="A",g="0",e="br-"},
{n="hew_hashset_remove_layout",d="hashset:362",p="bb",r="S",tr="A",e="br-"},
{n="hew_hashset_to_vec_layout",d="hashset:442",p="b",r="?",e="bh?"},
{n="hew_i64_to_string",d="string:503",p="s",r="F",e="bh"},
{n="hew_instant_duration_since",d="io_time:152",p="ss",r="S",e="bh"},
{n="hew_instant_elapsed",d="io_time:140",p="s",r="S",e="bh"},
{n="hew_instant_now",d="io_time:127",r="S",e="bh"},
{n="hew_int_to_string",d="string:449",p="s",r="F",e="bh"},
{n="hew_lambda_actor_ask",d="lambda_actor:1270",p="bbsoo",r="S",e="bh"},
{n="hew_lambda_actor_clone",d="lambda_actor:1140",p="b",r="F",rel="hew_lambda_actor_release",k="retain",e="br",f=["B21"]},
{n="hew_lambda_actor_downgrade",d="lambda_actor:1489",p="b",r="F",e="bh"},
{n="hew_lambda_actor_new",d="lambda_actor:1091",p="ssgxg",r="F",rel="hew_lambda_actor_release",g="24",e="br",f=["B3"]},
{n="hew_lambda_actor_release",d="lambda_actor:1423",p="c",r="S",e="br-"},
{n="hew_lambda_actor_send",d="lambda_actor:1189",p="bxs",r="S",e="br",f=["B3"]},
{n="hew_lambda_actor_weak_clone",d="lambda_actor:1572",p="b",r="F",e="bh"},
{n="hew_lambda_actor_weak_drop",d="lambda_actor:1611",p="c",r="S",e="bh",f=["B2"]},
{n="hew_lambda_actor_weak_send",d="lambda_actor:1523",p="bbs",r="S",e="bh"},
{n="hew_lambda_body_alloc_reply_buf",d="lambda_actor:243",p="s",r="F",e="bh"},
{n="hew_lambda_drain_all",d="lambda_actor:322",p="s",r="S",e="bh"},
{n="hew_layout_key_bool",d="layout_intrinsics:387",r="D",e="st"},
{n="hew_layout_key_bytes",d="layout_intrinsics:420",r="D",e="st"},
{n="hew_layout_key_char",d="layout_intrinsics:398",r="D",e="st"},
{n="hew_layout_key_f32",d="layout_intrinsics:369",r="D",e="st"},
{n="hew_layout_key_f64",d="layout_intrinsics:377",r="D",e="st"},
{n="hew_layout_key_i32",d="layout_intrinsics:335",r="D",e="st"},
{n="hew_layout_key_i64",d="layout_intrinsics:343",r="D",e="st"},
{n="hew_layout_key_string",d="layout_intrinsics:409",r="D",e="st"},
{n="hew_layout_key_u32",d="layout_intrinsics:351",r="D",e="st"},
{n="hew_layout_key_u64",d="layout_intrinsics:359",r="D",e="st"},
{n="hew_layout_val_bool",d="layout_intrinsics:460",r="D",e="st"},
{n="hew_layout_val_bytes",d="layout_intrinsics:487",r="D",e="st"},
{n="hew_layout_val_char",d="layout_intrinsics:469",r="D",e="st"},
{n="hew_layout_val_f32",d="layout_intrinsics:457",r="D",e="st"},
{n="hew_layout_val_f64",d="layout_intrinsics:458",r="D",e="st"},
{n="hew_layout_val_i32",d="layout_intrinsics:453",r="D",e="st"},
{n="hew_layout_val_i64",d="layout_intrinsics:454",r="D",e="st"},
{n="hew_layout_val_string",d="layout_intrinsics:478",r="D",e="st"},
{n="hew_layout_val_u32",d="layout_intrinsics:455",r="D",e="st"},
{n="hew_layout_val_u64",d="layout_intrinsics:456",r="D",e="st"},
{n="hew_layout_val_unit",d="layout_intrinsics:499",r="D",e="st"},
{n="hew_listener_await_accept",d="transport:3491",p="s??",r="S",e="bh?"},
{n="hew_local_pid_supervisor_child_get",d="supervisor:8803",p="ss",r="S",e="bh"},
{n="hew_location_display",r="?",e="sy"},
{n="hew_location_incarnation",r="?",e="sy"},
{n="hew_location_node_id",r="?",e="sy"},
{n="hew_location_slot",r="?",e="sy"},
{n="hew_machine_emit_push",d="machine_emit:369",p="bss?",r="S",e="bh?"},
{n="hew_machine_emit_step_enter",d="machine_emit:393",p="b",r="S",tr="U",e="bh"},
{n="hew_machine_emit_step_exit_keep",d="machine_emit:442",p="b",r="S",tr="U",e="bh"},
{n="hew_machine_emit_take",d="machine_emit:468",p="bss",r="S",tr="U",e="bh"},
{n="hew_metric_histogram_register",d="metrics:1050",p="?bs",r="S",e="bh?"},
{n="hew_metric_vec_register",d="metrics:1112",p="?s?s",r="S",e="bh?"},
{n="hew_metric_vec_with",d="metrics:1151",p="s?s",r="S",e="bh?"},
{n="hew_module_init_actor_codecs",r="?",e="sy"},
{n="hew_module_init_regex",r="?",e="sy"},
{n="hew_msg_envelope_payload_ptr",d="mailbox:584",p="b",r="B",e="br-"},
{n="hew_node_api_allow_peer",d="hew_node:5565",p="s?",r="S",e="bh?"},
{n="hew_node_api_ask_async_location",d="hew_node:6190",p="??s?ss?",r="?",e="bh?"},
{n="hew_node_api_ask_cancel",d="hew_node:6279",p="c",r="N",e="bh"},
{n="hew_node_api_ask_finish",d="hew_node:6239",p="c?ss",r="?",e="bh?"},
{n="hew_node_api_ask_location",d="hew_node:6084",p="??s?sss",r="?",e="bh?"},
{n="hew_node_api_connect",d="hew_node:5135",p="?",r="S",e="bh?"},
{n="hew_node_api_identity_key",d="hew_node:5678",r="F",e="bh"},
{n="hew_node_api_load_keys",d="hew_node:5415",p="?",r="S",e="bh?"},
{n="hew_node_api_lookup_location",d="hew_node:5266",p="??",r="S",e="bh?"},
{n="hew_node_api_register_by_pid",d="hew_node:5198",p="?s",r="S",e="bh?"},
{n="hew_node_api_set_transport",d="hew_node:5299",p="?",r="S",e="bh?"},
{n="hew_node_api_shutdown",d="hew_node:5088",r="S",e="bh"},
{n="hew_node_api_start",d="hew_node:4942",p="?",r="S",e="bh?"},
{n="hew_node_ask_take_last_error",d="hew_node:617",r="S",e="bh"},
{n="hew_node_id_display",r="?",e="sy"},
{n="hew_node_link_remote_location",d="hew_node:3603",p="?s",r="S",e="bh?"},
{n="hew_node_monitor_location",d="hew_node:3408",p="?o",r="S",e="bh?"},
{n="hew_observe_barrier",d="observe:472",r="S",e="br-"},
{n="hew_panic",d="actor:7142",r="N",tr="UAP",e="bh"},
{n="hew_panic_msg",d="actor:7232",p="b",r="N",tr="U",e="bh"},
{n="hew_print_value",d="print:185",p="sss",r="N",tr="AP",e="br"},
{n="hew_quic_stream_last_recv_timed_out",d="std/quic:1501",r="S",e="br-"},
{n="hew_rc_clone",d="rc:156",p="b",r="R",rel="hew_rc_drop",k="retain",e="br"},
{n="hew_rc_downgrade",d="rc:372",p="b",r="F",rel="hew_weak_drop_rc",e="br-"},
{n="hew_rc_drop",d="rc:181",p="c",r="N",tr="P",e="br",f=["B2"]},
{n="hew_rc_get",d="rc:279",p="b",r="B",e="br-"},
{n="hew_rc_is_unique",d="rc:359",p="b",r="S",e="br-"},
{n="hew_rc_new",d="rc:103",p="cssg",r="F",rel="hew_rc_drop",tr="P",g="3",e="br",f=["B16","B2"]},
{n="hew_rc_set",d="rc:303",p="bc",r="N",tr="P",e="br-"},
{n="hew_rc_strong_count",d="rc:245",p="b",r="S",e="br-"},
{n="hew_rc_weak_count",d="rc:261",p="b",r="S",e="br-"},
{n="hew_read_slot_cancel",d="read_slot:298",p="b",r="N",e="bh"},
{n="hew_read_slot_cancel_cleanup",d="read_slot:401",p="bs",r="N",e="bh"},
{n="hew_read_slot_free",d="read_slot:252",p="c",r="N",e="bh"},
{n="hew_read_slot_new",d="read_slot:197",r="F",e="bh"},
{n="hew_read_slot_set_await_cancel",d="read_slot:338",p="b?",r="N",e="bh?"},
{n="hew_read_slot_status",d="read_slot:425",p="b",r="S",e="bh"},
{n="hew_read_slot_take",d="read_slot:444",p="b",r="?",e="bh?"},
{n="hew_read_slot_take_handle",d="read_slot:493",p="b",r="S",e="bh"},
{n="hew_realloc",d="mem:150",p="?sss",r="?",tr="P",e="bh?"},
{n="hew_recv_half_recv",d="duplex:1362",p="boo",r="S",e="bh"},
{n="hew_recv_half_try_recv",d="duplex:1421",p="boo",r="S",e="bh"},
{n="hew_regex_capture",d="std/regex:453",p="b?s",r="F",e="bh?"},
{n="hew_regex_compile",d="std/regex:386",p="?",r="F",e="bh?"},
{n="hew_regex_free_capture",d="std/regex:507",p="c",r="N",e="bh",f=["B2"]},
{n="hew_regex_handle",r="?",e="sy"},
{n="hew_regex_match",d="std/regex:414",p="b?",r="S",e="bh?"},
{n="hew_register_handler_name",d="actor:5283",p="?sb",r="N",e="bh?",f=["tw"]},
{n="hew_remote_pid_display",r="?",e="sy"},
{n="hew_remote_pid_incarnation",r="?",e="sy"},
{n="hew_remote_pid_location",r="?",e="sy"},
{n="hew_remote_pid_node_id",r="?",e="sy"},
{n="hew_remote_pid_send",r="?",e="sy"},
{n="hew_remote_pid_slot",r="?",e="sy"},
{n="hew_reply",d="reply_channel:642",p="cxs",r="S",e="br",f=["B3","B13"]},
{n="hew_reply_channel_cancel",d="reply_channel:948",p="b",r="N",e="bh"},
{n="hew_reply_channel_cancel_cleanup",d="reply_channel:282",p="bs",r="N",e="bh"},
{n="hew_reply_channel_free",d="reply_channel:894",p="c",r="N",e="br-",f=["B2"]},
{n="hew_reply_channel_is_orphaned",d="reply_channel:1065",p="b",r="S",e="bh"},
{n="hew_reply_channel_new",d="reply_channel:158",r="F",rel="hew_reply_channel_free",e="br-"},
{n="hew_reply_channel_publish_cancelled",d="reply_channel:969",p="b",r="N",e="bh"},
{n="hew_reply_channel_publish_task_failed",d="reply_channel:987",p="b",r="N",e="bh"},
{n="hew_reply_channel_retain",d="reply_channel:316",p="b",r="N",rel="hew_reply_channel_free",k="retain",e="br",f=["B21"]},
{n="hew_reply_channel_set_await_cancel",d="reply_channel:224",p="b?",r="N",e="bh?"},
{n="hew_reply_channel_set_parked_waiter",d="reply_channel:196",p="b?",r="N",e="bh?"},
{n="hew_reply_channel_set_reply_drop_fn",d="reply_channel:258",p="bg",r="N",g="1",e="br-"},
{n="hew_reply_channel_signal_ready",d="reply_channel:708",p="c",r="N",e="br-"},
{n="hew_reply_payload_free",d="reply_channel:749",p="cs",r="N",e="br",f=["B2"]},
{n="hew_reply_wait",d="reply_channel:781",p="b",r="O",rel="hew_reply_payload_free",e="br-",f=["O","tw"]},
{n="hew_require_execution_context",d="execution_context:641",r="?",e="bh?"},
{n="hew_runtime_cleanup_after_main",d="scheduler:1301",r="N",e="bh"},
{n="hew_sched_init",d="scheduler:749",r="S",e="bh"},
{n="hew_sched_run",d="scheduler_wasm:1030",r="N",e="bh"},
{n="hew_sched_shutdown",d="scheduler:1091",r="N",e="bh"},
{n="hew_select_first",d="reply_channel:1087",p="bss",r="S",e="bh"},
{n="hew_select_ready_index",d="reply_channel:1145",p="bs",r="S",e="bh"},
{n="hew_send_half_send",d="duplex:1279",p="bbs",r="S",e="bh"},
{n="hew_send_half_try_send",d="duplex:1320",p="bbs",r="S",e="bh"},
{n="hew_ser_free_bytes",d="xnode_serial:51",p="c",r="N",e="bh"},
{n="hew_shutdown_initiate",d="shutdown:229",p="s",r="N",e="bh"},
{n="hew_shutdown_initiate_implicit",d="shutdown:240",p="s",r="N",e="bh"},
{n="hew_shutdown_wait",d="shutdown:308",r="S",e="bh"},
{n="hew_sink_close",d="stream:1906",p="c",r="N",e="br-"},
{n="hew_sink_detach_await",d="stream:2779",p="b?",r="N",e="bh?"},
{n="hew_sink_flush",d="stream:1892",p="b",r="N",tr="G",e="br-"},
{n="hew_sink_peer_closed",d="stream:1949",p="b",r="S",e="bh"},
{n="hew_sink_try_write_bytes",d="stream:2915",p="bb",r="S",e="bh"},
{n="hew_sink_try_write_string",d="stream:2830",p="bb",r="S",e="bh"},
{n="hew_sink_write_bytes",d="stream:2396",p="bb",r="N",tr="G",e="br-"},
{n="hew_sink_write_string",d="stream:2170",p="bb",r="N",tr="G",e="br-"},
{n="hew_sleep_ns",d="io_time:66",p="s",r="N",e="bh"},
{n="hew_sleep_until_ns",d="io_time:85",p="s",r="N",e="bh"},
{n="hew_stream_await_next",d="stream:2447",p="b??",r="S",e="bh?"},
{n="hew_stream_await_send",d="stream:2655",p="b??b",r="S",e="bh?"},
{n="hew_stream_await_send_layout",d="stream:2717",p="b???g",r="S",g="4",e="bh?"},
{n="hew_stream_cancel_pending_read",d="stream:1728",p="bs",r="N",e="bh",f=["tw"]},
{n="hew_stream_chunks",d="stream:2071",p="cs",r="F",rel="hew_stream_close",tr="G",e="br-",f=["B6"]},
{n="hew_stream_detach_await",d="stream:2626",p="b?",r="N",e="bh?"},
{n="hew_stream_lines",d="stream:2049",p="c",r="F",rel="hew_stream_close",tr="G",e="br-",f=["B6"]},
{n="hew_stream_next_layout",d="stream:2540",p="b?g",r="S",tr="G",g="2",e="bh?"},
{n="hew_stream_poll",d="stream:1506",p="bg?",r="S",tr="A",g="1",e="bh?",f=["tw"]},
{n="hew_stream_pop_layout",d="stream:2568",p="b?g",r="S",tr="G",g="2",e="bh?"},
{n="hew_stream_recv_cancel_cleanup",d="stream:1819",p="bs",r="N",e="bh"},
{n="hew_stream_send_layout",d="stream:2486",p="bxg",r="N",tr="AG",g="2",e="br-",f=["B3"]},
{n="hew_stream_take",d="stream:2357",p="cs",r="F",rel="hew_stream_close",tr="G",e="br-",f=["B6"]},
{n="hew_stream_try_next_layout",d="stream:2602",p="b?g",r="S",tr="G",g="2",e="bh?"},
{n="hew_string_char_at",d="string:1171",p="bs",r="S",tr="G",e="bh"},
{n="hew_string_char_at_utf8",d="string:1360",p="?s",r="S",e="bh?"},
{n="hew_string_char_count",d="string:1315",p="?",r="S",e="bh?"},
{n="hew_string_concat",d="string:270",p="bb",r="F",rel="S",tr="A",e="br-"},
{n="hew_string_contains",d="string:386",p="??",r="S",tr="G",e="bh?"},
{n="hew_string_ends_with",d="string:367",p="bb",r="S",tr="G",e="bh"},
{n="hew_string_equals",d="string:815",p="bb",r="S",e="br-"},
{n="hew_string_find",d="string:338",p="??",r="S",tr="G",e="bh?"},
{n="hew_string_from_char",d="string:1213",p="s",r="?",e="bh?"},
{n="hew_string_get",r="?",e="sy"},
{n="hew_string_hash_fnv1a",d="string:837",p="b",r="S",e="br-"},
{n="hew_string_index",d="string:1563",p="?s",r="S",tr="U",e="bh?"},
{n="hew_string_is_alpha",d="string:411",p="b",r="S",tr="G",e="bh"},
{n="hew_string_is_alphanumeric",d="string:424",p="b",r="S",tr="G",e="bh"},
{n="hew_string_is_digit",d="string:398",p="b",r="S",tr="G",e="bh"},
{n="hew_string_is_empty",d="string:437",p="?",r="S",tr="G",e="bh?"},
{n="hew_string_length",d="string:777",p="b",r="S",e="bh"},
{n="hew_string_slice_codepoints",d="string:1607",p="?ss",r="F",tr="U",e="bh?"},
{n="hew_string_starts_with",d="string:353",p="?b",r="S",tr="G",e="bh?"},
{n="hew_supervisor_add_child_spec",d="supervisor:3887",p="bb",r="S",tr="G",e="bh"},
{n="hew_supervisor_add_child_supervisor_with_init",d="supervisor:8470",p="bbs",r="S",e="bh"},
{n="hew_supervisor_child_get",d="supervisor:8673",p="?s",r="S",e="bh?"},
{n="hew_supervisor_direct_id",d="supervisor:4277",p="b",r="S",e="bh"},
{n="hew_supervisor_nested_get",d="supervisor:9300",p="bs",r="S",e="bh"},
{n="hew_supervisor_new",d="supervisor:3803",p="sss",r="F",e="bh"},
{n="hew_supervisor_pool_add_slot",d="supervisor:10427",p="b?ss",r="S",tr="G",e="bh?"},
{n="hew_supervisor_pool_child_get",d="supervisor:10646",p="bss",r="S",e="bh"},
{n="hew_supervisor_pool_get_option",r="?",e="sy"},
{n="hew_supervisor_pool_len",d="supervisor:10798",p="bs",r="S",e="bh"},
{n="hew_supervisor_pool_member_add_static",d="supervisor:10550",p="bss",r="S",tr="G",e="bh"},
{n="hew_supervisor_restart_await_blocking",d="supervisor:10278",p="?s",r="N",e="bh?"},
{n="hew_supervisor_restart_await_detach",d="supervisor:10237",p="b?",r="N",e="bh?"},
{n="hew_supervisor_restart_await_suspend",d="supervisor:10150",p="bs??",r="S",e="bh?"},
{n="hew_supervisor_set_child_init_fn",d="supervisor:10001",p="bsscs",r="N",e="bh"},
{n="hew_supervisor_set_child_lifecycle",d="supervisor:9786",p="bss",r="N",e="bh"},
{n="hew_supervisor_set_child_state_clone",d="supervisor:9865",p="bsg",r="N",g="2",e="bh"},
{n="hew_supervisor_set_child_state_drop",d="supervisor:9718",p="bsg",r="N",g="2",e="bh"},
{n="hew_supervisor_set_config_drop_fn",d="supervisor:10084",p="bg",r="N",g="1",e="br-"},
{n="hew_supervisor_start",d="supervisor:4024",p="b",r="S",tr="G",e="bh"},
{n="hew_supervisor_stop",d="supervisor:4258",p="?",r="N",tr="G",e="bh?"},
{n="hew_task_await_blocking",d="task_scope:1593",p="b",r="?",tr="G",e="bh?"},
{n="hew_task_await_suspend",d="task_scope:956",p="?b??",r="S",e="bh?"},
{n="hew_task_complete_threaded",d="task_scope:1632",p="b",r="N",tr="G",e="bh"},
{n="hew_task_completion_observe",d="task_scope:798",p="?b??",r="S",e="bh?"},
{n="hew_task_completion_unobserve",d="task_scope:837",p="?b??",r="S",e="bh?"},
{n="hew_task_detach_await",d="task_scope:1009",p="???",r="N",e="bh?"},
{n="hew_task_free",d="task_scope:666",p="c",r="N",tr="PG",e="br",f=["B2"]},
{n="hew_task_get_env",d="task_scope:744",p="b",r="B",tr="G",e="br"},
{n="hew_task_get_error",d="task_scope:1337",p="b",r="S",tr="G",e="bh"},
{n="hew_task_get_result",d="task_scope:756",p="b",r="B",tr="G",e="br",f=["B12"]},
{n="hew_task_new",d="task_scope:638",r="F",rel="hew_task_free",e="br-"},
{n="hew_task_result_publication_checkpoint",d="task_scope:2046",p="b",r="N",tr="G",e="bh"},
{n="hew_task_scope_cancel",d="task_scope:2067",p="b",r="N",tr="G",e="bh"},
{n="hew_task_scope_cancel_after_ns",d="task_scope:2105",p="bs",r="N",tr="G",e="bh"},
{n="hew_task_scope_completion_observe",d="task_scope:1146",p="b??",r="S",e="bh?"},
{n="hew_task_scope_destroy",d="task_scope:2341",p="c",r="N",tr="G",e="bh",f=["B2"]},
{n="hew_task_scope_detach_completion",d="task_scope:1273",p="?",r="N",e="bh?"},
{n="hew_task_scope_join_all",d="task_scope:1650",p="b",r="N",tr="G",e="bh"},
{n="hew_task_scope_new",d="task_scope:1760",r="F",e="bh"},
{n="hew_task_scope_set_current",d="task_scope:43",p="b",r="?",e="bh?"},
{n="hew_task_scope_spawn",d="task_scope:1809",p="bb",r="N",tr="G",e="bh"},
{n="hew_task_set_env",d="task_scope:696",p="bc",r="N",tr="G",e="br"},
{n="hew_task_set_result",d="task_scope:1294",p="bcs",r="N",tr="PG",e="br"},
{n="hew_task_set_result_drop_fn",d="task_scope:1322",p="bg",r="N",g="1",e="br-"},
{n="hew_task_spawn_thread",d="task_scope:1408",p="bs",r="N",tr="G",e="bh"},
{n="hew_task_spawn_thread_with_inherited_context",d="task_scope:1479",p="bbs",r="S",tr="G",e="bh"},
{n="hew_task_take_result",d="task_scope:776",p="b",r="O",tr="G",e="br",f=["O"]},
{n="hew_tls_attach_local",r="?",e="sy"},
{n="hew_trap_with_code",d="supervisor:429",p="s",r="N",tr="UP",e="br",f=["tw"]},
{n="hew_u64_to_string",d="string:521",p="s",r="F",e="bh"},
{n="hew_u8_to_string",d="string:467",p="s",r="F",rel="S",e="br"},
{n="hew_uint_to_string",d="string:485",p="s",r="F",e="bh"},
{n="hew_vec_append",d="vec:1822",p="bb",r="N",tr="A",e="br-"},
{n="hew_vec_append_layout",r="?",e="sy"},
{n="hew_vec_clear",d="vec:1578",p="b",r="N",e="br-"},
{n="hew_vec_clear_layout",r="?",e="sy"},
{n="hew_vec_closure_pair_drop_inplace",d="vec:1612",p="c",r="N",e="br-"},
{n="hew_vec_contains_f64",d="vec:1913",p="bs",r="S",tr="G",e="mt"},
{n="hew_vec_contains_i32",d="vec:1911",p="bs",r="S",tr="G",e="mt"},
{n="hew_vec_contains_i64",d="vec:1912",p="bs",r="S",tr="G",e="mt"},
{n="hew_vec_contains_owned",d="vec:3167",p="b?g",r="S",tr="AG",g="2",e="bh?"},
{n="hew_vec_contains_str",d="vec:2191",p="bb",r="S",tr="G",e="br-"},
{n="hew_vec_contains_thunk",d="vec:3053",p="b?g",r="S",tr="AG",g="2",e="bh?"},
{n="hew_vec_equals_thunk",d="vec:3112",p="bbg",r="S",tr="AG",g="2",e="bh"},
{n="hew_vec_free",d="vec:1593",p="c",r="N",e="br"},
{n="hew_vec_free_owned",d="vec:2953",p="c",r="N",e="br",f=["B4"]},
{n="hew_vec_get_bool",d="vec:833",p="bs",r="S",tr="UT",e="mt"},
{n="hew_vec_get_clone",d="vec:2752",p="bst",r="S",tr="AG",e="br-",f=["B15","B3"]},
{n="hew_vec_get_f32",d="vec:865",p="bs",r="S",tr="UT",e="mt"},
{n="hew_vec_get_f64",d="vec:864",p="bs",r="S",tr="UT",e="mt"},
{n="hew_vec_get_generic",d="vec:2356",p="bs",r="?",tr="UAT",e="bh?"},
{n="hew_vec_get_i16",d="vec:836",p="bs",r="S",tr="UT",e="mt"},
{n="hew_vec_get_i32",d="vec:832",p="bs",r="S",tr="UT",e="mt"},
{n="hew_vec_get_i64",d="vec:838",p="bs",r="S",tr="UT",e="mt"},
{n="hew_vec_get_i8",d="vec:834",p="bs",r="S",tr="UT",e="mt"},
{n="hew_vec_get_layout",d="vec:2379",p="bsg",r="B",tr="U",g="2",e="br-"},
{n="hew_vec_get_ptr",d="vec:897",p="bs",r="B",tr="U",e="br-"},
{n="hew_vec_get_str",d="vec:851",p="bs",r="R",rel="S",tr="UT",e="br-",f=["B15","B5"]},
{n="hew_vec_get_u16",d="vec:837",p="bs",r="S",tr="UT",e="mt"},
{n="hew_vec_get_u8",d="vec:835",p="bs",r="S",tr="UT",e="mt"},
{n="hew_vec_is_empty",d="vec:1519",p="b",r="S",e="bh"},
{n="hew_vec_join_str",d="string:1038",p="bb",r="F",rel="S",tr="A",e="br-"},
{n="hew_vec_len",d="vec:1469",p="b",r="S",e="br-"},
{n="hew_vec_pop_bool",d="vec:1430",p="b",r="S",tr="UT",e="mt"},
{n="hew_vec_pop_f32",d="vec:1456",p="b",r="S",tr="UT",e="mt"},
{n="hew_vec_pop_f64",d="vec:1455",p="b",r="S",tr="UT",e="mt"},
{n="hew_vec_pop_i16",d="vec:1433",p="b",r="S",tr="UT",e="mt"},
{n="hew_vec_pop_i32",d="vec:1429",p="b",r="S",tr="UT",e="mt"},
{n="hew_vec_pop_i64",d="vec:1435",p="b",r="S",tr="UT",e="mt"},
{n="hew_vec_pop_i8",d="vec:1431",p="b",r="S",tr="UT",e="mt"},
{n="hew_vec_pop_layout",d="vec:2482",p="bog",r="S",tr="G",g="2",e="br-"},
{n="hew_vec_pop_owned",d="vec:2926",p="bo",r="S",tr="AG",e="br",f=["B3"]},
{n="hew_vec_pop_ptr",d="vec:1457",p="b",r="X",tr="UTG",e="br",f=["B3"]},
{n="hew_vec_pop_str",d="vec:1444",p="b",r="F",rel="S",tr="UT",e="br"},
{n="hew_vec_pop_u16",d="vec:1434",p="b",r="S",tr="UT",e="mt"},
{n="hew_vec_pop_u8",d="vec:1432",p="b",r="S",tr="UT",e="mt"},
{n="hew_vec_push",r="?",e="sy"},
{n="hew_vec_push_bool",d="vec:744",p="bs",r="N",tr="A",e="mt"},
{n="hew_vec_push_f32",d="vec:781",p="bs",r="N",tr="A",e="mt"},
{n="hew_vec_push_f64",d="vec:780",p="bs",r="N",tr="A",e="mt"},
{n="hew_vec_push_generic",d="vec:2304",p="bb",r="N",tr="A",e="bh"},
{n="hew_vec_push_i16",d="vec:747",p="bs",r="N",tr="A",e="mt"},
{n="hew_vec_push_i32",d="vec:743",p="bs",r="N",tr="A",e="mt"},
{n="hew_vec_push_i64",d="vec:749",p="bs",r="N",tr="A",e="mt"},
{n="hew_vec_push_i8",d="vec:745",p="bs",r="N",tr="A",e="mt"},
{n="hew_vec_push_layout",d="vec:2328",p="bbg",r="N",tr="A",g="2",e="br-"},
{n="hew_vec_push_owned",d="vec:2634",p="bb",r="N",tr="A",e="br-"},
{n="hew_vec_push_ptr",d="vec:791",p="bx",r="N",tr="U",e="br-",f=["B3","B2"]},
{n="hew_vec_push_str",d="vec:763",p="bb",r="N",tr="A",e="br-"},
{n="hew_vec_push_u16",d="vec:748",p="bs",r="N",tr="A",e="mt"},
{n="hew_vec_push_u8",d="vec:746",p="bs",r="N",tr="A",e="mt"},
{n="hew_vec_remove_at_bool",d="vec:2048",p="bs",r="S",tr="UAT",e="mt"},
{n="hew_vec_remove_at_f32",d="vec:2055",p="bs",r="S",tr="UAT",e="mt"},
{n="hew_vec_remove_at_f64",d="vec:2056",p="bs",r="S",tr="UAT",e="mt"},
{n="hew_vec_remove_at_i16",d="vec:2051",p="bs",r="S",tr="UAT",e="mt"},
{n="hew_vec_remove_at_i32",d="vec:2053",p="bs",r="S",tr="UAT",e="mt"},
{n="hew_vec_remove_at_i64",d="vec:2054",p="bs",r="S",tr="UAT",e="mt"},
{n="hew_vec_remove_at_i8",d="vec:2049",p="bs",r="S",tr="UAT",e="mt"},
{n="hew_vec_remove_at_layout",d="vec:1963",p="bsog",r="S",tr="UATG",g="3",e="bh"},
{n="hew_vec_remove_at_owned",d="vec:2130",p="bso",r="S",tr="UATG",e="br",f=["B3"]},
{n="hew_vec_remove_at_ptr",d="vec:2091",p="bs",r="?",tr="UATG",e="bh?"},
{n="hew_vec_remove_at_str",d="vec:2066",p="bs",r="F",rel="S",tr="UAT",e="br"},
{n="hew_vec_remove_at_u16",d="vec:2052",p="bs",r="S",tr="UAT",e="mt"},
{n="hew_vec_remove_at_u8",d="vec:2050",p="bs",r="S",tr="UAT",e="mt"},
{n="hew_vec_set",r="?",e="sy"},
{n="hew_vec_set_bool",d="vec:1285",p="bss",r="N",tr="UT",e="bh"},
{n="hew_vec_set_f32",d="vec:1383",p="bss",r="N",tr="UT",e="mt"},
{n="hew_vec_set_f64",d="vec:1372",p="bss",r="N",tr="UT",e="bh"},
{n="hew_vec_set_i16",d="vec:1319",p="bss",r="N",tr="UT",e="mt"},
{n="hew_vec_set_i32",d="vec:1268",p="bss",r="N",tr="UT",e="bh"},
{n="hew_vec_set_i64",d="vec:1328",p="bss",r="N",tr="UT",e="bh"},
{n="hew_vec_set_i8",d="vec:1317",p="bss",r="N",tr="UT",e="mt"},
{n="hew_vec_set_layout",d="vec:2430",p="bsbg",r="N",tr="U",g="3",e="br-"},
{n="hew_vec_set_owned",d="vec:2840",p="bsb",r="N",tr="U",e="br-"},
{n="hew_vec_set_ptr",d="vec:2166",p="bsx",r="N",tr="U",e="br-",f=["B3","B2"]},
{n="hew_vec_set_str",d="vec:1348",p="bsb",r="N",tr="U",e="br-"},
{n="hew_vec_set_u16",d="vec:1320",p="bss",r="N",tr="UT",e="mt"},
{n="hew_vec_set_u8",d="vec:1318",p="bss",r="N",tr="UT",e="mt"},
{n="hew_vec_take_owned",d="vec:2812",p="bso",r="S",tr="AG",e="br-",f=["B15","B3","B17"]},
{n="hew_vtable_dispatch_panic_on_oob",d="trait_object:204",p="ss",r="N",tr="P",e="bh"},
{n="hew_wasm_register_actor_meta",d="bridge:598",p="b",r="N",e="bh",f=["tw"]},
{n="hew_wasm_runtime_exit",d="scheduler_wasm:1072",r="N",e="bh"},
{n="hew_weak_drop_rc",d="rc:458",p="c",r="N",tr="P",e="br",f=["B2"]},
{n="hew_weak_upgrade_rc",d="rc:423",p="b",r="?",e="bh?"},
{n="hew_wire_cbor_to_text",d="wire_text:846",p="bs?s",r="F",e="bh?"},
{n="hew_wire_text_to_cbor",d="wire_text:903",p="b?soo",r="F",e="bh?"},
{n="hew_ws_attach_local",r="?",e="sy"},
{n="hew_xnode_register_codec",d="xnode_serial:150",p="?sgg",r="N",g="23",e="bh?"},
{n="hew_xnode_register_reply_codec",d="xnode_serial:185",p="?sgg",r="N",g="23",e="bh?"},
]
```
