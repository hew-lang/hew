//! Regression oracle for a `for` loop iterating a field of a BY-VALUE record
//! parameter.
//!
//! `for claim in ledger.claims` inside `fn f(ledger: ClaimsLedger)` used to
//! hand the field's Vec handle to a sole-owner cursor that freed it at
//! exhaustion, while the parameter's guarded carrier drop
//! (`append_owned_carrier_param_drops` → `__hew_record_drop_inplace_<R>`)
//! still released the whole record at normal return — the guard tracks only
//! whole-value transfer, never a field-level move, so every call double-freed
//! the iterated field's buffer (a silent SIGABRT under the macOS poisoned
//! allocator; `hew check` was clean). A LOCAL record root had the sibling
//! defect: the composite-drop prover classified the cursor ingress as a field
//! escape, excluded the whole root, and leaked every non-iterated field.
//!
//! The fix makes such a cursor BORROW the field handle
//! (`vec_iter_source_live_binding_record_field_root`, the third member of the
//! projection-borrow family after #2540/#2545): the root's single drop
//! authority — carrier drop for parameters, `RecordInPlace` for locals —
//! frees every field exactly once, and the borrow-ingress exemption
//! (`vec_iter_projection_borrow_inits`) keeps the local root admitted.
//!
//! Covered behaviours: full iteration at both call boundaries, an early
//! `break` partial iteration with post-loop reads of BOTH fields, repeated
//! iteration of the same parameter field, the local two-field partial-loop
//! sibling leak, and the rvalue-rooted projection control that must KEEP its
//! sole-owner cursor free. A panic-mid-loop edge is pinned structurally by the
//! elaborated drop plans (the guarded `vec_iter_cursor` entries on panic /
//! cancel edges), not by this process-level oracle — an aborting process
//! cannot host the `leaks(1)` atExit probe.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance_exact_lines, compile_to_native, measure_leaks_exact,
    run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

/// The original failing shape: a two-field record consumed by value, one field
/// iterated, the record rebuilt and returned. One helper frame per iteration so
/// leaked storage is unreachable from `main`'s final frame.
const PARAM_FIELD_LOOP_TEMPLATE: &str = r#"
type Claim {
    run_id: string,
    amount: i64,
}

type ClaimsLedger {
    claims: Vec<Claim>,
    terminal_runs: Vec<string>,
}

fn without_terminal_claims(ledger: ClaimsLedger) -> ClaimsLedger {
    let terminal_runs = ledger.terminal_runs;
    var claims: Vec<Claim> = Vec.new();
    for claim in ledger.claims {
        if !terminal_runs.contains(claim.run_id) {
            claims.push(claim);
        }
    }
    ClaimsLedger { claims: claims, terminal_runs: terminal_runs }
}

fn run_case(seed: i64) -> i64 {
    var claims: Vec<Claim> = Vec.new();
    claims.push(Claim { run_id: "r1", amount: 10 + seed });
    claims.push(Claim { run_id: "r2", amount: 20 + seed });
    claims.push(Claim { run_id: "r3", amount: 30 + seed });
    var terminal: Vec<string> = Vec.new();
    terminal.push("r2");
    let ledger = ClaimsLedger { claims: claims, terminal_runs: terminal };
    let pruned = without_terminal_claims(ledger);
    var total: i64 = 0;
    for claim in pruned.claims {
        total = total + claim.amount;
    }
    total + pruned.claims.len() + pruned.terminal_runs.len()
}

fn main() -> i64 {
    var checksum = 0;
    for frame in 0..__FRAMES__ {
        checksum = checksum + run_case(frame);
        println("frame");
    }
    if checksum >= 0 { 0 } else { 91 }
}
"#;

/// Minimal double-free repro: by-value single-field record, field iterated with
/// a no-op body, record never touched again. Crashed at `count_claims` return.
const PARAM_MINIMAL_SOURCE: &str = r#"
type Claim {
    run_id: string,
    amount: i64,
}

type ClaimsLedger {
    claims: Vec<Claim>,
}

fn count_claims(ledger: ClaimsLedger) -> i64 {
    var n: i64 = 0;
    for claim in ledger.claims {
        n = n + 1;
    }
    n
}

fn main() {
    var claims: Vec<Claim> = Vec.new();
    claims.push(Claim { run_id: "r1", amount: 10 });
    claims.push(Claim { run_id: "r2", amount: 20 });
    claims.push(Claim { run_id: "r3", amount: 30 });
    let ledger = ClaimsLedger { claims: claims };
    let n = count_claims(ledger);
    println(f"kept {n} claims");
}
"#;

/// Early `break` abandons the cursor mid-iteration; the borrow contract means
/// BOTH fields — including the partially iterated one — stay readable after
/// the loop and release exactly once at the carrier drop.
const PARAM_EARLY_BREAK_SOURCE: &str = r#"
type Claim {
    run_id: string,
    amount: i64,
}

type ClaimsLedger {
    claims: Vec<Claim>,
    terminal_runs: Vec<string>,
}

fn first_amount(ledger: ClaimsLedger) -> i64 {
    var found: i64 = 0;
    for claim in ledger.claims {
        found = claim.amount;
        break;
    }
    found + ledger.claims.len() + ledger.terminal_runs.len()
}

fn main() {
    var claims: Vec<Claim> = Vec.new();
    claims.push(Claim { run_id: "r1", amount: 10 });
    claims.push(Claim { run_id: "r2", amount: 20 });
    var terminal: Vec<string> = Vec.new();
    terminal.push("r2");
    let ledger = ClaimsLedger { claims: claims, terminal_runs: terminal };
    println(f"v={first_amount(ledger)}");
}
"#;

/// Iterating the same parameter field twice proves the cursor left the handle
/// live (a sole-owner cursor freed it at first exhaustion — use-after-free).
const PARAM_DOUBLE_ITERATION_SOURCE: &str = r#"
type Claim {
    run_id: string,
    amount: i64,
}

type ClaimsLedger {
    claims: Vec<Claim>,
}

fn sum_twice(ledger: ClaimsLedger) -> i64 {
    var total: i64 = 0;
    for claim in ledger.claims {
        total = total + claim.amount;
    }
    for claim in ledger.claims {
        total = total + claim.amount;
    }
    total
}

fn main() {
    var claims: Vec<Claim> = Vec.new();
    claims.push(Claim { run_id: "r1", amount: 10 });
    claims.push(Claim { run_id: "r2", amount: 20 });
    let ledger = ClaimsLedger { claims: claims };
    println(f"total={sum_twice(ledger)}");
}
"#;

/// The LOCAL-root sibling defect: iterating one field of a two-field local
/// record excluded the root's composite drop wholesale and leaked the
/// non-iterated `terminal_runs` Vec (3 nodes / 176 bytes per frame).
const LOCAL_TWO_FIELD_PARTIAL_SOURCE: &str = r#"
type Claim {
    run_id: string,
    amount: i64,
}

type ClaimsLedger {
    claims: Vec<Claim>,
    terminal_runs: Vec<string>,
}

fn main() {
    var claims: Vec<Claim> = Vec.new();
    claims.push(Claim { run_id: "r1", amount: 10 });
    claims.push(Claim { run_id: "r2", amount: 20 });
    var terminal: Vec<string> = Vec.new();
    terminal.push("r2");
    let ledger = ClaimsLedger { claims: claims, terminal_runs: terminal };
    var n: i64 = 0;
    for claim in ledger.claims {
        n = n + 1;
    }
    println(f"kept {n} claims, {ledger.terminal_runs.len()} terminal");
}
"#;

/// Control: an RVALUE-rooted projection has no surviving owner, so its cursor
/// must KEEP the sole-owner free — the borrow verdict is scoped to live
/// binding roots and must not flip this shape into a leak.
const RVALUE_ROOT_PROJECTION_CONTROL_SOURCE: &str = r#"
type Claim {
    run_id: string,
    amount: i64,
}

type ClaimsLedger {
    claims: Vec<Claim>,
}

fn make_ledger() -> ClaimsLedger {
    var claims: Vec<Claim> = Vec.new();
    claims.push(Claim { run_id: "r1", amount: 10 });
    claims.push(Claim { run_id: "r2", amount: 20 });
    ClaimsLedger { claims: claims }
}

fn main() {
    var total: i64 = 0;
    for claim in make_ledger().claims {
        total = total + claim.amount;
    }
    println(f"total={total}");
}
"#;

/// The projection-store bypass: assigning the ITERATED field mid-loop
/// replaced the slot holding the borrowed handle while the cursor's own handle
/// copy kept dereferencing the old storage — an abort under the poisoned
/// allocator and reused-handle reads (`40,40,40,40` from `[40, 2]`) without
/// it. The store now rejects at check-time (`vec_iter_borrowed_projections`
/// prefix guard), so the runtime scenario is unreachable by construction.
const PROJECTION_STORE_MID_LOOP_SOURCE: &str = r#"
type Holder {
    items: Vec<i64>,
}

fn main() {
    var items: Vec<i64> = Vec.new();
    items.push(40);
    items.push(2);
    var holder = Holder { items: items };
    for value in holder.items {
        println(f"{value}");
        holder.items = Vec.new();
    }
}
"#;

/// Element-level mutation of the iterated field through the SHARED handle:
/// `push` mid-loop extends the live view (the appended element is visited),
/// `clear` ends it (the next length probe reads 0). Every `next` re-loads the
/// handle from the cursor and clones the element out, so neither operation
/// can leave the cursor holding a stale buffer pointer.
const ITERATED_FIELD_PUSH_CLEAR_SOURCE: &str = r#"
type Holder {
    items: Vec<i64>,
}

fn main() {
    var items: Vec<i64> = Vec.new();
    items.push(40);
    items.push(2);
    var holder = Holder { items: items };
    var seen: i64 = 0;
    var total: i64 = 0;
    for value in holder.items {
        seen = seen + 1;
        total = total + value;
        if seen == 1 {
            holder.items.push(7);
        }
        if seen == 3 {
            holder.items.clear();
        }
    }
    println(f"seen={seen} total={total} len={holder.items.len()}");
}
"#;

/// Index assignment INTO the iterated field (`holder.items[1] = 9`) is an
/// in-place element store through the shared handle — bounds-checked, no
/// handle replacement — so the cursor observes the new element on the next
/// clone-out: a live view, not a stale read.
const ITERATED_FIELD_INDEX_SET_SOURCE: &str = r#"
type Holder {
    items: Vec<i64>,
}

fn main() {
    var items: Vec<i64> = Vec.new();
    items.push(40);
    items.push(2);
    var holder = Holder { items: items };
    var seen: i64 = 0;
    var total: i64 = 0;
    for value in holder.items {
        if seen == 0 {
            holder.items[1] = 9;
        }
        seen = seen + 1;
        total = total + value;
    }
    println(f"seen={seen} total={total}");
}
"#;

fn param_field_loop_source(frames: usize) -> String {
    PARAM_FIELD_LOOP_TEMPLATE.replace("__FRAMES__", &frames.to_string())
}

fn expected_lines(frames: usize) -> usize {
    frames
}

fn compile_and_run(source: &str, dir: &std::path::Path, name: &str) -> std::path::PathBuf {
    let bin = compile_to_native(source, dir, name);
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "{name} must survive the poisoned allocator — an abort here is the \
         field-loop double-free / use-after-free this oracle pins:\n{}",
        describe_output(&output)
    );
    bin
}

#[test]
fn projection_store_mid_loop_rejects_at_check_time() {
    let dir = tempfile::Builder::new()
        .prefix("projection-store-mid-loop-")
        .tempdir()
        .expect("tempdir");
    let hew_src = dir.path().join("projection_store_mid_loop.hew");
    std::fs::write(&hew_src, PROJECTION_STORE_MID_LOOP_SOURCE).expect("write hew source");

    let output = std::process::Command::new(support::hew_binary())
        .args(["check", hew_src.to_str().expect("hew src utf-8")])
        .current_dir(support::repo_root())
        .output()
        .expect("invoke hew check");
    assert!(
        !output.status.success(),
        "storing to the iterated field projection mid-loop must reject at \
         check-time — accepting it re-opens the mid-loop handle replacement \
         (poisoned-allocator abort / reused-handle reads):\n{}",
        describe_output(&output)
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("assigning `holder.items` while a VecIter cursor borrows it"),
        "the rejection must name the projected path and the borrowing cursor:\n{}",
        describe_output(&output)
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the poisoned allocator contract is macOS-only; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn iterated_field_push_and_clear_run_as_live_view() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("iterated-field-push-clear-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_and_run(
        ITERATED_FIELD_PUSH_CLEAR_SOURCE,
        dir.path(),
        "iterated_field_push_clear",
    );
    let output = run_under_malloc_scribble(&bin);
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "seen=3 total=49 len=0\n",
        "a mid-loop push must extend the live view (the appended 7 is visited) \
         and a mid-loop clear must end it at the next length probe"
    );
    assert_eq!(
        measure_leaks_exact(&bin),
        (0, 0),
        "element mutation through the shared handle must not strand cleared or \
         reallocated element storage"
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the poisoned allocator contract is macOS-only; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn iterated_field_index_set_runs_in_place() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("iterated-field-index-set-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_and_run(
        ITERATED_FIELD_INDEX_SET_SOURCE,
        dir.path(),
        "iterated_field_index_set",
    );
    let output = run_under_malloc_scribble(&bin);
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "seen=2 total=49\n",
        "an in-place element store into the iterated field must be observed by \
         the cursor's next clone-out (40 then the stored 9)"
    );
    assert_eq!(measure_leaks_exact(&bin), (0, 0));
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn param_record_field_loop_has_flat_leak_slope() {
    assert_frame_slope_below_tolerance_exact_lines(
        "param_record_field_loop",
        param_field_loop_source,
        expected_lines,
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the poisoned allocator contract is macOS-only; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn param_field_loop_minimal_runs_clean_and_leak_free() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("param-field-loop-minimal-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_and_run(PARAM_MINIMAL_SOURCE, dir.path(), "param_field_loop_minimal");
    let output = run_under_malloc_scribble(&bin);
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "kept 3 claims\n",
        "full iteration of the parameter field must visit every element"
    );
    assert_eq!(
        measure_leaks_exact(&bin),
        (0, 0),
        "the carrier drop is the single release authority for the iterated field"
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the poisoned allocator contract is macOS-only; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn param_field_loop_early_break_keeps_both_fields_live_and_leak_free() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("param-field-loop-early-break-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_and_run(
        PARAM_EARLY_BREAK_SOURCE,
        dir.path(),
        "param_field_loop_early_break",
    );
    let output = run_under_malloc_scribble(&bin);
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "v=13\n",
        "a partial iteration must leave the iterated field readable (len=2) \
         alongside its sibling (len=1)"
    );
    assert_eq!(
        measure_leaks_exact(&bin),
        (0, 0),
        "an abandoned borrow-cursor must not strand the un-iterated remainder"
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the poisoned allocator contract is macOS-only; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn param_field_double_iteration_reads_live_handle_twice() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("param-field-double-iteration-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_and_run(
        PARAM_DOUBLE_ITERATION_SOURCE,
        dir.path(),
        "param_field_double_iteration",
    );
    let output = run_under_malloc_scribble(&bin);
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "total=60\n",
        "the second loop must observe the same live elements as the first"
    );
    assert_eq!(measure_leaks_exact(&bin), (0, 0));
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the poisoned allocator contract is macOS-only; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn local_two_field_partial_loop_releases_the_non_iterated_sibling() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("local-two-field-partial-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_and_run(
        LOCAL_TWO_FIELD_PARTIAL_SOURCE,
        dir.path(),
        "local_two_field_partial",
    );
    let output = run_under_malloc_scribble(&bin);
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "kept 2 claims, 1 terminal\n",
        "the sibling field must stay readable after the field loop"
    );
    assert_eq!(
        measure_leaks_exact(&bin),
        (0, 0),
        "the local root keeps its RecordInPlace composite: the iterated field is \
         borrowed by the cursor and the sibling must not leak"
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the poisoned allocator contract is macOS-only; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn rvalue_rooted_projection_cursor_still_frees_its_snapshot() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("rvalue-root-projection-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_and_run(
        RVALUE_ROOT_PROJECTION_CONTROL_SOURCE,
        dir.path(),
        "rvalue_root_projection",
    );
    let output = run_under_malloc_scribble(&bin);
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "total=30\n",
        "an rvalue-rooted projection loop must still visit every element"
    );
    assert_eq!(
        measure_leaks_exact(&bin),
        (0, 0),
        "with no surviving root binding the cursor remains the sole owner and \
         must free the projected handle"
    );
}
