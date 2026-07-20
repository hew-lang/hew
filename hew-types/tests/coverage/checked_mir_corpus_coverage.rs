//! Family-coverage accounting for the golden MIR corpus
//! (`examples/v05/checked-mir/`).
//!
//! The corpus is the byte-identical behavioural oracle for internal
//! retyping work on the runtime-call seams. This test pins WHICH
//! [`RuntimeCallFamily`] variants the corpus actually exercises: a
//! family whose `c_symbol()` appears (quoted) in at least one golden
//! dump is covered — the oracle will catch a mis-migration of that
//! family. The remainder is pinned in `EXPECTED_UNCOVERED`, fail-closed
//! in both directions:
//!
//! - a family LEAVING the uncovered set (a new fixture now exercises
//!   it) must be removed from the list — the pin ratchets forward only;
//! - a family ENTERING the uncovered set (a fixture or golden was
//!   dropped) fails loudly — the oracle silently shrinking is exactly
//!   the drift this test exists to refuse.
//!
//! [`RuntimeCallFamily`]: hew_types::runtime_call::RuntimeCallFamily

use std::collections::BTreeSet;
use std::path::PathBuf;

use hew_types::runtime_call::all_runtime_call_families;

/// Families with no corpus fixture today, keyed by `c_symbol()`.
///
/// Every entry carries a reason group. Probed against the live binary
/// while building the corpus — each group names WHY the symbol cannot
/// appear in a `--dump-mir` golden today and WHEN that changes.
fn expected_uncovered() -> BTreeSet<&'static str> {
    EXPECTED_UNCOVERED.iter().copied().collect()
}

const EXPECTED_UNCOVERED: &[&str] = &[
    // -- Structured-terminator lowering: the MIR carries a typed
    //    terminator (Spawn / Ask / Select / Suspend), not a callee
    //    symbol; the C symbol materialises at the codegen edge only.
    "hew_actor_ask",
    "hew_actor_ask_with_channel",
    "hew_actor_send_by_id",
    "hew_actor_spawn",
    "hew_reply_channel_cancel",
    "hew_reply_channel_free",
    "hew_reply_channel_new",
    "hew_reply_payload_free",
    "hew_reply_wait",
    "hew_select_first",
    "hew_sink_write_bytes",
    "hew_sink_try_write_bytes",
    "hew_sink_try_write_string",
    "hew_hashmap_new_with_layout",
    // `hew_hashmap_remove_layout` (bool remove, drops both K and V) is no
    // longer produced by any Hew surface: `HashMap.remove(k)` lowers to the
    // Option-producing `hew_hashmap_remove_take_layout` (A233). The bool
    // kernel remains a runtime export for HashSet internals and hew-cabi
    // callers, which never appear in a MIR dump.
    "hew_hashmap_remove_layout",
    "hew_hashset_new_with_layout",
    // -- Codegen-internal emission: injected by codegen passes (drop
    //    elaboration backends, deadline timers, closure-env mutexes,
    //    task spill machinery), never present in a MIR dump.
    // `hew_actor_demonitor` is emitted exclusively from lower_drop_runtime
    // (struct-field extraction path for MonitorRef::close); it never appears
    // as a RuntimeCallFamily in an Instr::CallRuntimeAbi MIR dump of any
    // corpus fixture (it does appear via the std::link_monitor import path,
    // but no corpus fixture exercises that path).
    "hew_actor_demonitor",
    "hew_auto_mutex_alloc",
    "hew_auto_mutex_free",
    "hew_auto_mutex_lock",
    "hew_auto_mutex_unlock",
    "hew_cancel_token_is_requested",
    "hew_cancel_token_release",
    "hew_cancel_token_retain",
    "hew_dyn_box_alloc",
    "hew_dyn_box_free",
    "hew_lambda_actor_release",
    "hew_lambda_actor_clone",
    "hew_lambda_actor_downgrade",
    "hew_lambda_actor_weak_clone",
    "hew_lambda_actor_weak_drop",
    "hew_lambda_actor_weak_send",
    "hew_lambda_body_alloc_reply_buf",
    "hew_lambda_drain_all",
    "hew_rc_new",
    "hew_task_await_blocking",
    "hew_task_complete_threaded",
    "hew_task_completion_observe",
    "hew_task_completion_unobserve",
    "hew_task_free",
    "hew_task_get_env",
    "hew_task_get_error",
    "hew_task_get_result",
    "hew_task_scope_cancel_after_ns",
    "hew_task_set_env",
    "hew_task_set_result",
    "hew_task_spawn_thread",
    "hew_vtable_dispatch_panic_on_oob",
    // -- Compiler-internal name: explicit user spelling rejected
    //    ("'cooperate' is compiler-internal").
    "hew_actor_cooperate",
    // -- The channel-Duplex split user surface is lowered end-to-end and
    //    covered by the `duplex_split_half_ops` / `duplex_unified_recv` corpus
    //    fixtures: half extraction (`hew_duplex_send_half` /
    //    `hew_duplex_recv_half`), half send/recv/try (`hew_send_half_send` /
    //    `hew_send_half_try_send` / `hew_recv_half_recv` /
    //    `hew_recv_half_try_recv`), half close (`hew_duplex_close_half`), and
    //    unified channel-mode recv (`hew_duplex_recv` / `hew_duplex_try_recv`).
    //    Those families are no longer pinned.
    //
    //    The `duplex_split_half_ops` fixture also exercises `hew_duplex_pair`,
    //    so that family is now covered and no longer pinned.
    //
    //    Still uncovered:
    //    - `hew_duplex_send`: the tell-shaped send renders as a D1 send carrier
    //      whose symbol materialises at the codegen edge, not as a `family:`
    //      row the corpus needle matches.
    //    - `hew_duplex_clone`: no user clone surface.
    //    - `hew_duplex_close`: explicit raw close routes through drop
    //      elaboration, not a value-position call, so no fixture emits it.
    //    - `hew_duplex_payload_free`: codegen-internal — emitted inside the
    //      recv materialisation, never its own CallRuntimeAbi family in a dump.
    //    - `hew_duplex_try_send`: no user surface lowers to it today.
    //    - `hew_supervisor_nested_get`: v0.6 surface.
    "hew_duplex_clone",
    "hew_duplex_close",
    "hew_duplex_payload_free",
    "hew_duplex_send",
    "hew_duplex_try_send",
    "hew_supervisor_nested_get",
    // -- Static-pool accessor symbols. `sup.pool[i]` / `.len()` lower to these
    //    (`sup.pool.get(i)` fail-closes at MIR for now); the static-pool surface
    //    is exercised by the vertical-slice fixtures (supervisor_static_pool*),
    //    not the golden checked-mir corpus, so they pin as uncovered here.
    "hew_supervisor_pool_child_get",
    "hew_supervisor_pool_len",
    // -- `bytes.get` lowers to a `Terminator::Call` whose callee codegen
    //    intercepts to build `Option<u8>`; the call carries the callee as a
    //    string (`builtin: None`), so the `BytesGet` family is a descriptor
    //    row only and never renders as `family: BytesGet` in the corpus.
    "hew_bytes_get",
    // -- Constructor callee identity intercepted by codegen. The checked-MIR
    //    corpus records the lowered runtime call, not the `bytes::new` catalog
    //    identity itself.
    "bytes::new",
    // -- No user-facing surface lowers to the catalogued symbol today
    //    (probed: `bytes.len()` routes through `hew_vec_len`; no
    //    `char_count` string method; no Instant/Duration value surface;
    //    `std::text::regex` rides module externs, the match-arm regex
    //    path emits only `hew_regex_is_match`/`hew_regex_capture_width`).
    "hew_bytes_len",
    "hew_duration_abs",
    "hew_duration_hours",
    "hew_duration_is_zero",
    "hew_duration_micros",
    "hew_duration_millis",
    "hew_duration_mins",
    // `hew_duration_nanos` is now covered: the `impl Display for duration` fmt
    // body (`to_string(val.nanos()) + "ns"`) calls `.nanos()`, so the symbol
    // appears in every checked-mir golden dump.
    "hew_duration_secs",
    "hew_instant_duration_since",
    "hew_instant_elapsed",
    "hew_instant_now",
    "hew_observe_barrier",
    "hew_observe_read_u64",
    "hew_observe_scrape",
    "hew_observe_series",
    "hew_regex_capture",
    "hew_regex_compile",
    "hew_regex_free_capture",
    // -- Synthetic codegen-only family: `hew_regex_handle` names the
    //    value-position regex literal materialisation (`let pat = re"..."`).
    //    It carries no runtime symbol — codegen GEP-loads the compiled handle
    //    from `@hew_regex_handles[literal_id]` — and is exercised by the
    //    vertical-slice `regex_literal_value` fixture, not the golden
    //    checked-mir corpus, so it pins as uncovered here.
    "hew_regex_handle",
    "hew_string_char_count",
    // -- `string.get` lowers to a `Terminator::Call` whose callee codegen
    //    intercepts to build `Option<char>`; the call carries the callee as a
    //    string (`builtin: None`), so the `StringGet` family is a descriptor
    //    row only and never renders as `family: StringGet` in the corpus.
    "hew_string_get",
    // -- Layout-witness entry with no probed source producer: bytes
    //    sends ride hew_sink_write_bytes and string pipe sends ride
    //    hew_sink_write_string, so no surface emits the stream send
    //    layout entry today.
    "hew_stream_send_layout",
    // -- Pre-staged codegen intercept with no in-corpus producer
    //    (network actor attach requires a live net surface fixture).
    "hew_tcp_attach_local",
    // -- Cross-node monitor surface. `monitor(RemotePid<T>)` lowers to
    //    `hew_node_monitor_location`, but requires a two-process distributed
    //    program to exercise; it is
    //    proven by the compiled two-process e2e fixture
    //    (hew-cli/tests/distributed_two_process_e2e.rs), not the single-program
    //    golden checked-mir corpus, so they pin as uncovered here.
    "hew_node_monitor_location",
    // -- Native-only `Node::*` catalog identities not exercised by the
    //    single-program golden corpus. Node start/register/shutdown are covered
    //    by existing fixtures; peer setup and connection lifecycle are proven
    //    by the distributed integration suite.
    "Node::allow_peer",
    "Node::connect",
    "Node::id",
    "Node::identity_key",
    "Node::load_keys",
    // -- Cross-node link surface. An explicit `link(RemotePid<T>)` lowers to
    //    `hew_node_link_remote_location`, but exercising it requires a two-process
    //    distributed program (the link only materializes across a node
    //    transport); it is proven by the compiled two-process e2e fixture
    //    (hew-cli/tests/distributed_two_process_e2e.rs `link_remote_crash_cascade_*`),
    //    not the single-program golden checked-mir corpus, so it pins as
    //    uncovered here alongside the cross-node monitor families.
    "hew_node_link_remote_location",
    // -- Ptr element-type variants with no source-reachable producer
    //    (no user surface yields a ptr-element Vec).
    // -- Owned Vec.get is lowered through the clone-returning ABI for
    //    ownership-safe extraction; the legacy move-out symbol is no longer
    //    emitted by checked MIR.
    "hew_vec_get_owned",
    "hew_vec_get_ptr",
    "hew_vec_slice_range_ptr",
    // -- Direct-call collection kernels catalogued for typed codegen
    //    partitioning. These are runtime/codegen helper identities rather than
    //    families rendered by the current checked-MIR corpus.
    "hew_hashmap_clear_layout",
    "hew_hashmap_clone_layout",
    "hew_hashset_clear_layout",
    "hew_hashset_clone_layout",
    "hew_hashset_to_vec_layout",
    "hew_vec_clone_layout",
    "hew_vec_clone_owned",
    "hew_vec_contains_owned",
    "hew_vec_contains_thunk",
    "hew_vec_pop_bool",
    "hew_vec_pop_layout",
    "hew_vec_pop_owned",
    "hew_vec_push_owned",
    "hew_vec_remove_at_bool",
    "hew_vec_remove_at_layout",
    "hew_vec_remove_at_owned",
    "hew_vec_set_bool",
    "hew_vec_set_i32",
    "hew_vec_set_layout",
    "hew_vec_set_owned",
    // -- Per-type slice-range symbols replaced by the unified bytesize path.
    //    I32/I64/F64 scalar slice-range now routes through
    //    `hew_vec_slice_range_bytesize` (layout-aware, overflow-checked);
    //    the per-type symbols are no longer emitted by the MIR lowering pass.
    //    `hew_vec_slice_range_bytesize` itself IS covered by the corpus
    //    (vec_element_widths golden). Remove these pins when per-type symbols
    //    are reinstated or the bytesize path is split again.
    "hew_vec_slice_range_f64",
    "hew_vec_slice_range_i32",
    "hew_vec_slice_range_i64",
    // -- Metric families with no Phase-A user surface. `std::metrics`
    //    exposes only the scalar register/mutate path (counter, gauge,
    //    and the simple histogram), all covered by the
    //    `metric_register_record` fixture. The bucketed histogram
    //    registration and the labelled (vec) families take a raw
    //    `(ptr, len)` C-array ABI that a Hew `extern "C"` `Vec<T>` cannot
    //    marshal, so no Phase-A surface lowers to these symbols. They
    //    leave this list when labelled metrics land in v0.5.3 with a
    //    `HewVec`-shaped ABI.
    "hew_metric_histogram_register",
    "hew_metric_vec_register",
    "hew_metric_vec_with",
    // -- receive-gen-fn stream-producer pump: emitted only
    //    by `build_stream_producer_pump` (hew-mir/src/lower.rs), never by a
    //    checker-registered user-facing surface. The golden checked-mir
    //    corpus has no `receive gen fn` fixture yet (a later pass promotes the
    //    probes to fixtures under `tests/vertical-slice`, a different
    //    corpus); pin as uncovered here until a golden dump exercises one.
    "hew_actor_gen_sink_complete",
    "hew_actor_gen_sink_register",
    "hew_sink_peer_closed",
];

fn golden_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../examples/v05/checked-mir/golden")
}

#[test]
fn corpus_covers_every_family_or_pins_the_gap() {
    let dir = golden_dir();
    assert!(
        dir.is_dir(),
        "golden corpus missing at {} — run `make checked-mir-golden`",
        dir.display()
    );

    let mut haystack = String::new();
    let mut dump_count = 0usize;
    for entry in std::fs::read_dir(&dir).expect("read golden dir") {
        let path = entry.expect("dir entry").path();
        if path.extension().is_some_and(|e| e == "mir") {
            haystack.push_str(&std::fs::read_to_string(&path).expect("read golden dump"));
            dump_count += 1;
        }
    }
    assert!(
        dump_count > 0,
        "no .mir dumps under {} — run `make checked-mir-golden`",
        dir.display()
    );

    // The `RuntimeCall` payload renders its TYPED family
    // (`family: VecSliceRange(\n    I64,\n),`) rather than a symbol
    // string; normalize the pretty Debug output (strip whitespace, fold
    // the trailing comma inside parens) so a single-line family needle
    // matches it.
    let normalized: String = haystack
        .split_whitespace()
        .collect::<String>()
        .replace(",)", ")");

    let mut uncovered = BTreeSet::new();
    for family in all_runtime_call_families() {
        let sym = family.c_symbol();
        // Three renderings count as coverage:
        //
        //  1. Quoted symbol (legacy Debug format):
        //     `callee: "hew_vec_len"`, `drop_fn: "hew_string_drop"`
        //     Quoting avoids substring false-positives for short names
        //     like `abs` / `min` in the old format.
        //
        //  2. Typed family (Instr::CallRuntimeAbi route, both formats):
        //     `family: VecLen,` (Debug) or `family:VecLen,` (normalized).
        //     Matched against the whitespace-normalised text so that
        //     multi-line pretty-printed variants are recognised.
        //
        //  3. Word-boundary patterns in the structured text format:
        //     - `<sym>(…)` call syntax: ` abs(`, ` hew_vec_len(`
        //     - elab call-terminator: ` abs ->`, ` hew_vec_len ->`
        //     - drop annotations: `release({sym})`, `cow_heap({sym})`
        //     - raw call_rt prefix: `call_rt {sym}(`
        //     Using ` {sym}(` or ` {sym} ` avoids substring false-positives
        //     even for short names — `abs` only appears preceded by a space
        //     and followed by `(` or ` ->` in the structured dump.
        let quoted = format!("\"{sym}\"");
        let family_needle = format!("family:{family:?},")
            .split_whitespace()
            .collect::<String>();
        let call_args = format!(" {sym}(");
        let call_term = format!(" {sym} ->");
        let release_ann = format!("release({sym})");
        let cow_heap_ann = format!("cow_heap({sym})");
        let call_rt_unquoted = format!("call_rt {sym}(");
        if !haystack.contains(&quoted)
            && !normalized.contains(&family_needle)
            && !haystack.contains(&call_args)
            && !haystack.contains(&call_term)
            && !haystack.contains(&release_ann)
            && !haystack.contains(&cow_heap_ann)
            && !haystack.contains(&call_rt_unquoted)
        {
            uncovered.insert(sym);
        }
    }

    let expected = expected_uncovered();
    let newly_covered: Vec<_> = expected.difference(&uncovered).collect();
    let newly_uncovered: Vec<_> = uncovered.difference(&expected).collect();
    assert!(
        newly_covered.is_empty() && newly_uncovered.is_empty(),
        "corpus family coverage drifted.\n\
         Now covered (remove from EXPECTED_UNCOVERED): {newly_covered:?}\n\
         No longer covered (add a fixture or justify the pin): {newly_uncovered:?}\n\
         Covered {covered}/{total} families.",
        covered = all_runtime_call_families().len() - uncovered.len(),
        total = all_runtime_call_families().len(),
    );
}
