//! Bytes ownership leak oracles: sender-local scope-exit drop and
//! actor-state-field overwrite drop.
//!
//! Empirical slope oracles for the two verified bytes leak classes:
//!
//!   * **Sender-local scope-exit leak**: a `bytes` local that is still the
//!     sole owner of its refcounted buffer at scope exit received no drop
//!     (`cow_value_leaf_drop_symbol` deliberately excluded `Bytes`), so a
//!     loop creating one fresh buffer per iteration leaked one allocation
//!     node per frame. The fix is the `derive_local_bytes_drop_allowed`
//!     fail-closed admission authority in `hew-mir/src/lower.rs` plus the
//!     `DropKind::CowHeap { "hew_bytes_drop" }` arm routed through
//!     codegen's BytesTriple-aware `emit_bytes_inplace_drop`. The loop
//!     shape also exercises the back-edge `DropPlan` coverage (the
//!     per-iteration local must be released on the loop back-edge, not
//!     just at function exit).
//!
//!   * **State-field overwrite leak**: `lower_actor_state_field_store`
//!     stored the incoming value over the previous field value with no
//!     preceding release, so every re-store of a `bytes` (or `string`)
//!     state field leaked the prior buffer permanently. The fix emits a
//!     pointer-inequality-guarded old-value release keyed on the
//!     MIR-level `StateFieldCloneKind` before the store.
//!
//! ## Slope methodology
//!
//! Mirrors `recv_loop_leak_oracle.rs`: compile the same shape at a LOW
//! frame count and a HIGH frame count, measure leak NODE counts under
//! `leaks --atExit` with the poisoned-allocator triple, and assert the
//! delta stays within a small constant independent of frames. The
//! pre-fix bug class is PER-FRAME GROWTH (slope 1.0 leak/frame), which
//! over a `50 - 3 = 47`-frame delta lands an order of magnitude above
//! the +5 tolerance. Absolute counts are deliberately not asserted —
//! runtime/scheduler one-off allocations jitter by ±1 node.
//!
//! The long-running overwrite anchor uses the same fixture at
//! `LONG_FRAMES = 100`: pre-fix that probe leaks ~100 nodes; post-fix it
//! holds at the LOW-probe count (each overwrite releases the previous
//! buffer; `state_drop` releases the final one exactly once).
//!
//! ## Skip behaviour
//!
//! macOS-only (`leaks(1)` is Darwin's allocator inspector). On other
//! platforms the tests log `skip:` and return without failing; inside
//! the macOS path the assertion only runs when both measurements
//! succeed.

#![cfg(unix)]

mod support;

use std::path::PathBuf;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

/// Low frame count: exercises the loop back-edge / re-store path at
/// least twice while staying close to the constant-overhead floor.
const LOW_FRAMES: usize = 3;

/// High frame count for the slope check. A slope of 1.0 leak/frame
/// (the pre-fix measurement for both bytes leak classes) produces
/// `HIGH_FRAMES - LOW_FRAMES = 47` excess nodes against the tolerance
/// of 5.
const HIGH_FRAMES: usize = 50;

/// Long-running overwrite anchor (the regression pin for the
/// state-field overwrite drop): 100 re-stores of the same bytes field.
const LONG_FRAMES: usize = 100;

/// Maximum permitted leak-node delta between the HIGH and LOW probes.
/// Same headroom rationale as `recv_loop_leak_oracle.rs`: absorbs
/// one-off scheduler/runtime allocations that appear only in the HIGH
/// run while still catching a slope of ~0.1 leaks/frame.
const SLOPE_TOLERANCE: usize = 5;

// ── fixtures ──────────────────────────────────────────────────────────────

/// Fixture A — sender-local scope-exit drop. A `while` loop creates one
/// fresh `bytes` buffer per iteration and reads it (`.len()`, a
/// receiver-borrowing op that must NOT suppress the drop), then lets it
/// go out of scope on the back-edge. Pre-fix: one leaked allocation
/// node per iteration (no scope-exit drop for bytes). Post-fix: the
/// per-iteration local is proven sole-owner and released on every
/// back-edge plus the final fall-through.
fn local_loop_source(frames: usize) -> String {
    format!(
        "fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let p = \"sender-local-bytes-payload\".to_bytes();\n\
         \x20       total = total + p.len();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

/// Fixture B/C — actor-state-field overwrite drop. The sender creates
/// one fresh `bytes` buffer per message and sends it to an actor that
/// stores it into `buf`, overwriting the previous value each time.
/// Each sent buffer is consumed by the send (mailbox hand-off): the
/// sender must NOT drop it (that would be a use-after-free against the
/// actor's copy), and the actor's re-store must release the previous
/// buffer (pre-fix: leaked permanently, slope 1.0/frame). The final
/// buffer is released exactly once by the synthesised `state_drop`.
fn overwrite_source(frames: usize) -> String {
    format!(
        "actor ByteStore {{\n\
         \x20   var buf: bytes;\n\
         \n\
         \x20   receive fn store(packet: bytes) {{\n\
         \x20       buf = packet;\n\
         \x20   }}\n\
         \n\
         \x20   receive fn get_len() -> i64 {{\n\
         \x20       buf.len()\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let store = spawn ByteStore(buf: bytes::new());\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let packet = \"overwrite-bytes-payload\".to_bytes();\n\
         \x20       store.store(packet);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   sleep(2000ms);\n\
         \x20   match await store.get_len() {{\n\
         \x20       Ok(v) => v,\n\
         \x20       Err(_) => -1,\n\
         \x20   }}\n\
         }}\n"
    )
}

fn field_load_share_source(frames: usize) -> String {
    format!(
        "type Holder {{ payload: bytes, }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let holder = Holder {{ payload: \"field-load\".to_bytes() }};\n\
         \x20       let extracted = holder.payload;\n\
         \x20       total = total + extracted.len() + holder.payload.len();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

fn container_element_read_source(frames: usize) -> String {
    format!(
        "fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let values: Vec<((bytes, i64), bool)> = Vec::new();\n\
         \x20       let source = \"container-read\".to_bytes();\n\
         \x20       values.push(((source, 7), true));\n\
         \x20       let item = values[0];\n\
         \x20       let (inner, flag) = item;\n\
         \x20       let (payload, number) = inner;\n\
         \x20       if flag {{ total = total + payload.len() + number; }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

fn live_local_coown_source(frames: usize) -> String {
    format!(
        "type Holder {{ payload: bytes, }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let source = \"aggregate-share\".to_bytes();\n\
         \x20       let holder = Holder {{ payload: source }};\n\
         \x20       total = total + holder.payload.len();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

fn duplicating_return_source(frames: usize) -> String {
    format!(
        "fn duplicate(value: bytes) -> (bytes, bytes) {{ (value, value) }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let source = \"return-share\".to_bytes();\n\
         \x20       let returned = duplicate(source);\n\
         \x20       let (left, right) = returned;\n\
         \x20       total = total + source.len() + left.len() + right.len();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

/// Fixture G — A240 S1 hole #1: `let alias = value` where `value` is a by-value
/// `bytes` PARAMETER. A by-value heap param is a borrow (the caller retains
/// ownership and drops the original), so the co-owning `alias` shares the
/// caller's live buffer. Pre-fix `alias`'s scope-exit drop double-frees the
/// caller's buffer (a UAF/abort under the poisoned allocator, not a leak);
/// post-fix the move-share retain balances it and the per-frame buffer — one
/// per iteration in `main` — is freed exactly once. The slope check pins the
/// post-fix balance: an OVER-retain (leaking instead of double-freeing) would
/// grow the count per frame and fail here, so this also gates against the
/// leak-direction over-correction.
fn param_coown_source(frames: usize) -> String {
    format!(
        "fn share_param(value: bytes) -> i64 {{\n\
         \x20   let alias = value;\n\
         \x20   value.len() + alias.len()\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let payload = \"param-coown-payload\".to_bytes();\n\
         \x20       total = total + share_param(payload);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

/// Fixture H — A240 S1 hole #2: `let alias = source; ...; source` where the
/// co-owner `source` ESCAPES by return while `alias` drops locally. Pre-fix
/// `alias`'s drop frees the buffer the caller now owns (UAF on the returned
/// handle, then a double free at the caller's drop); post-fix the move-share
/// retain balances `alias`'s drop against the caller's. Same slope-as-balance
/// gate as fixture G.
fn owned_partner_escape_source(frames: usize) -> String {
    format!(
        "fn make_escape() -> bytes {{\n\
         \x20   let source = \"owned-escape-payload\".to_bytes();\n\
         \x20   let alias = source;\n\
         \x20   let _len = alias.len();\n\
         \x20   source\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let escaped = make_escape();\n\
         \x20       total = total + escaped.len();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

/// Fixture K — #2559: an allowlisted fresh-owned-bytes runtime result used as a
/// transient method receiver. `b[1..4]` lowers to `hew_bytes_slice`, an
/// ALLOWLISTED runtime symbol that nonetheless hands back a fresh rc==1 handle
/// (it bumps the shared buffer's refcount). The chained `.len()` borrows that
/// transient (`hew_vec_len`) and it then dies with no binding. Pre-fix the
/// allowlist gate excluded every known runtime symbol, so the slice temp had no
/// drop on any edge (slope 1.0 leak/frame — the leak #2559 reports); post-fix
/// the `FreshOwnedBytes` contract admits it and the nested-fresh-bytes-temp
/// splice releases it exactly once right after the borrow.
fn slice_transient_len_source(frames: usize) -> String {
    format!(
        "fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let b = \"slice-transient-source-payload\".to_bytes();\n\
         \x20       total = total + b[1..4].len();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

/// Fixture I — #2542: an UNNAMED user-call `bytes` result used as a transient
/// method receiver. `mk()` returns a fresh owned bytes; `.len()` borrows it
/// (`hew_vec_len`); with no `let` binding the result flows straight into that
/// borrow and then dies. Pre-fix the temp had no drop on ANY edge (slope 1.0
/// leak/frame — the leak this issue reports); post-fix the
/// nested-fresh-bytes-temp splice releases it exactly once right after the
/// borrow. The sibling `let b = mk(); b.len()` already held flat (it routes
/// through the binding-scoped `derive_local_bytes_drop_allowed`), so this
/// fixture pins the transient gap specifically.
fn usercall_transient_len_source(frames: usize) -> String {
    format!(
        "fn mk() -> bytes {{ \"unnamed-usercall-transient-payload\".to_bytes() }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       total = total + mk().len();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

/// Fixture I2 — #2542 terminator-use path: an unnamed user-call `bytes` result
/// borrowed by a method that lowers to a `Terminator::Call` (`.get(i)` →
/// `hew_bytes_get`). Every `Terminator::Call` bytes arg is a borrow (Hew's
/// by-value heap params), so the transient is released at the call's
/// continuation. Pre-fix slope 1.0 leak/frame; post-fix flat.
fn usercall_transient_get_source(frames: usize) -> String {
    format!(
        "fn mk() -> bytes {{ \"usercall-get-transient-payload\".to_bytes() }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       match mk().get(0) {{\n\
         \x20           Some(_b) => {{ total = total + 1; }}\n\
         \x20           None => {{}}\n\
         \x20       }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

/// Fixture J — #2542 discard: a user-call `bytes` result in statement position
/// (`mk();`). Zero uses; pre-fix the discarded temp leaked every frame, post-fix
/// the splice drops it at the front of the producer's single-predecessor
/// continuation. The slope doubles as the over-retain gate: an inline drop that
/// mis-fired more than once would still fail the balance.
fn usercall_discarded_source(frames: usize) -> String {
    format!(
        "fn mk() -> bytes {{ \"usercall-discarded-transient-payload\".to_bytes() }}\n\
         fn main() -> i64 {{\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       mk();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   i\n\
         }}\n"
    )
}

/// No-double-free pin (#2542): the shapes the nested-fresh-bytes-temp pass must
/// NOT drop. `let b = mk()` is released by the binding-scoped derivation; a
/// user-call result RETURNED by `forward()` is owned by the caller; a user-call
/// result moved into a record FIELD is owned by the aggregate. If the pass
/// mis-admitted any (a `Move`/return/record-ingress is NOT a borrowing use, so
/// each must stay excluded), the producer-side drop would free a buffer the real
/// owner also frees — a crash under the poisoned-allocator triple before the
/// `42 OK` checksum prints. Each payload is 14 bytes, so the three `.len()`
/// reads sum to 42.
const BYTES_USERCALL_NO_DOUBLE_FREE_SOURCE: &str = "\
fn mk() -> bytes {\n\
\x20   \"escape-payload\".to_bytes()\n\
}\n\
\n\
fn forward() -> bytes {\n\
\x20   mk()\n\
}\n\
\n\
type Boxed {\n\
\x20   payload: bytes,\n\
}\n\
\n\
fn main() {\n\
\x20   let b = mk();\n\
\x20   let named_len = b.len();\n\
\x20   let fwd = forward();\n\
\x20   let fwd_len = fwd.len();\n\
\x20   let boxed = Boxed { payload: mk() };\n\
\x20   let box_len = boxed.payload.len();\n\
\x20   print(named_len + fwd_len + box_len);\n\
\x20   print(\"OK\");\n\
}\n";

// ── leak measurement plumbing (same shape as recv_loop_leak_oracle) ──────

/// Compile `source` to a native binary via `hew compile --emit-dir` and
/// return the binary path.
fn compile_to_native(source: &str, dir: &std::path::Path, name: &str) -> PathBuf {
    let hew_src = dir.join(format!("{name}.hew"));
    std::fs::write(&hew_src, source).expect("write hew source");

    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--emit-dir",
            dir.to_str().expect("emit-dir utf-8"),
            hew_src.to_str().expect("hew src utf-8"),
        ])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile");

    assert!(
        output.status.success(),
        "hew compile failed for {name}:\n{}",
        describe_output(&output)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    let bin = stdout
        .lines()
        .find_map(|l| l.strip_prefix("native: "))
        .unwrap_or_else(|| panic!("no `native:` line for {name}:\n{stdout}"))
        .to_string();
    PathBuf::from(bin)
}

/// Run `bin` under the poisoned-allocator triple + `leaks --atExit` and
/// return `Some(leak_count)` when `leaks` produced a usable report.
/// Parses the canonical `Process <pid>: N leak(s) for B total leaked
/// bytes.` summary (both singular and plural forms).
fn measure_leaks(bin: &std::path::Path) -> Option<usize> {
    let output = Command::new("leaks")
        .arg("--atExit")
        .arg("--")
        .arg(bin)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .output()
        .ok()?;
    if !output.status.success() && output.stdout.is_empty() {
        eprintln!(
            "skip: leaks declined to attach to {}: {}",
            bin.display(),
            String::from_utf8_lossy(&output.stderr)
        );
        return None;
    }
    let report = String::from_utf8_lossy(&output.stdout);
    let mut parsed: Option<usize> = None;
    for line in report.lines() {
        if !line.contains(" leaks for ") && !line.contains(" leak for ") {
            continue;
        }
        if let Some(rest) = line.strip_prefix("Process ") {
            if !rest.chars().next().is_some_and(|c| c.is_ascii_digit()) {
                continue;
            }
            if let Some(after_colon) = rest.split_once(": ").map(|(_, s)| s) {
                if let Some(n) = after_colon.split_whitespace().next() {
                    if let Ok(n) = n.parse::<usize>() {
                        eprintln!("  parsed leak count from line: {line}");
                        parsed = Some(n);
                        break;
                    }
                }
            }
        }
    }
    if parsed.is_none() {
        eprintln!(
            "skip: leaks did not emit a `Process <pid>: N leak(s) for B total leaked bytes.` \
             summary for {}: stderr=\n{}",
            bin.display(),
            String::from_utf8_lossy(&output.stderr)
        );
    }
    parsed
}

/// Build the shape at `low_frames` and `high_frames`, measure leak NODE
/// counts, and assert the delta stays within `SLOPE_TOLERANCE`.
fn assert_frame_slope_below_tolerance(
    shape_name: &str,
    source_fn: fn(usize) -> String,
    low_frames: usize,
    high_frames: usize,
) {
    if !cfg!(target_os = "macos") {
        eprintln!("skip: {shape_name}: leaks(1) is macOS-only");
        return;
    }
    let leaks_avail = Command::new("which")
        .arg("leaks")
        .output()
        .is_ok_and(|o| o.status.success());
    if !leaks_avail {
        eprintln!("skip: {shape_name}: `leaks` binary not on PATH");
        return;
    }

    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("bytes-leak-{shape_name}-"))
        .tempdir()
        .expect("tempdir");

    let bin_low = compile_to_native(
        &source_fn(low_frames),
        dir.path(),
        &format!("{shape_name}_low"),
    );
    let bin_high = compile_to_native(
        &source_fn(high_frames),
        dir.path(),
        &format!("{shape_name}_high"),
    );

    let Some(low_leaks) = measure_leaks(&bin_low) else {
        return;
    };
    let Some(high_leaks) = measure_leaks(&bin_high) else {
        return;
    };

    eprintln!(
        "{shape_name}: low_frames={low_frames} low_leaks={low_leaks} \
         high_frames={high_frames} high_leaks={high_leaks} \
         tolerance={SLOPE_TOLERANCE}"
    );
    assert!(
        high_leaks <= low_leaks + SLOPE_TOLERANCE,
        "{shape_name}: per-frame leak SLOPE — low_frames={low_frames} low_leaks={low_leaks}, \
         high_frames={high_frames} high_leaks={high_leaks}. Excess of {} NODES over the \
         tolerance of {SLOPE_TOLERANCE} indicates a per-iteration bytes allocation is not \
         being released (pre-fix slope is 1.0 leak/frame for both bytes leak classes). \
         Re-run with `MallocStackLogging=1 leaks --atExit -- {}` to see which stack the \
         leaked block came from.",
        high_leaks.saturating_sub(low_leaks + SLOPE_TOLERANCE),
        bin_high.display()
    );
    assert!(
        high_leaks + SLOPE_TOLERANCE >= low_leaks,
        "{shape_name}: HIGH leak count is more than {SLOPE_TOLERANCE} below LOW \
         (low={low_leaks}, high={high_leaks}) — the actor likely did not finish \
         draining {high_frames} messages before `leaks --atExit` snapshotted. Increase \
         the `sleep_ms(...)` budget in the shape source."
    );
}

// ── oracles ───────────────────────────────────────────────────────────────

/// Fixture A: sender-local scope-exit drop for a loop-held `bytes`
/// local. Pre-fix slope 1.0 leak/frame (no scope-exit drop for bytes);
/// post-fix the per-iteration buffer is released on every back-edge.
/// Reverting either the `derive_local_bytes_drop_allowed` admission or
/// the codegen `CowHeap`-Bytes intercept fails this by ~47 nodes.
#[test]
fn bytes_local_loop_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "bytes_local_loop",
        local_loop_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}

/// Fixture B: actor-state-field overwrite drop. Each re-store of
/// `buf` must release the previous buffer (pre-fix: leaked
/// permanently, slope 1.0 leak/frame). Also pins the sender side: the
/// consumed-by-send locals must NOT be dropped by the sender (a
/// sender-side drop would be a use-after-free against the actor's
/// mailbox copy, surfacing as a crash under the poisoned-allocator
/// triple before any leak count is read).
#[test]
fn bytes_state_overwrite_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "bytes_state_overwrite",
        overwrite_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}

/// Fixture C: long-running overwrite regression anchor — the same
/// overwrite shape at 100 re-stores. Pre-fix this leaks ~100 nodes;
/// post-fix it holds at the LOW-probe count. This is the durable pin
/// for the old-value release in `lower_actor_state_field_store`.
#[test]
fn bytes_state_overwrite_long_run_holds_flat() {
    assert_frame_slope_below_tolerance(
        "bytes_state_overwrite_long",
        overwrite_source,
        LOW_FRAMES,
        LONG_FRAMES,
    );
}

#[test]
fn bytes_field_load_share_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "bytes_field_load_share",
        field_load_share_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}

#[test]
fn bytes_container_element_read_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "bytes_container_element_read",
        container_element_read_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}

#[test]
fn bytes_live_local_coown_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "bytes_live_local_coown",
        live_local_coown_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}

#[test]
fn bytes_duplicating_return_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "bytes_duplicating_return",
        duplicating_return_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}

/// Fixture G: move-share of a by-value bytes PARAMETER (A240 S1 hole #1). The
/// co-owning local must retain so its scope-exit drop does not double-free the
/// caller's borrowed buffer. Post-fix holds flat (each per-frame buffer freed
/// exactly once by the caller); an OVER-retain would leak per frame and fail
/// the slope. Pre-fix the double-free is a UAF/abort (macOS `leaks` is blind to
/// the underflow; the Linux `ASan` fixture is the authoritative double-free gate).
#[test]
fn bytes_param_coown_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "bytes_param_coown",
        param_coown_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}

/// Fixture H: move-share whose co-owner escapes by return (A240 S1 hole #2).
/// The surviving local co-owner must retain so its scope-exit drop does not
/// double-free the returned buffer. Post-fix holds flat; same balance-vs-leak
/// gate as fixture G.
#[test]
fn bytes_owned_partner_escape_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "bytes_owned_partner_escape",
        owned_partner_escape_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}

/// Fixture I: an unnamed user-call `bytes` result used as a transient method
/// receiver (`mk().len()`) — the #2542 leak. Pre-fix slope 1.0 leak/frame (the
/// temp had no drop on any edge); post-fix the nested-fresh-bytes-temp splice
/// releases it once after the borrow. Reverting either the collector's
/// admission or the apply splice fails this by ~47 nodes.
#[test]
fn bytes_usercall_transient_receiver_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "bytes_usercall_transient_len",
        usercall_transient_len_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}

/// Fixture I2: an unnamed user-call `bytes` result borrowed by a
/// `Terminator::Call` method (`mk().get(0)`). Pre-fix slope 1.0 leak/frame;
/// post-fix the terminator-use splice releases it at the continuation.
#[test]
fn bytes_usercall_transient_terminator_use_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "bytes_usercall_transient_get",
        usercall_transient_get_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}

/// Fixture J: a discarded user-call `bytes` result (`mk();`). Pre-fix slope 1.0
/// leak/frame; post-fix the discard splice drops it at the producer's
/// continuation. Holds flat.
#[test]
fn bytes_usercall_discarded_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "bytes_usercall_discarded",
        usercall_discarded_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}

/// Fixture K: an allowlisted fresh-owned-bytes runtime result used as a
/// transient method receiver (`b[1..4].len()`) — the #2559 leak. Pre-fix
/// slope 1.0 leak/frame (the allowlist gate excluded `hew_bytes_slice`);
/// post-fix the `FreshOwnedBytes` contract admits the transient and the splice
/// releases it once after the borrow. Reverting the contract or the collector
/// gate relaxation fails this by ~47 nodes.
#[test]
fn bytes_slice_transient_receiver_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "bytes_slice_transient_len",
        slice_transient_len_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}

/// No-double-free pin (#2542): the named-binding / returned / record-ingress
/// shapes the nested-fresh-bytes-temp pass must leave alone run to completion
/// under the poisoned-allocator triple and print the `42 OK` checksum. A
/// producer-side drop of any of those (mis-admitting a `Move`/return/record
/// ingress as a borrowing use) would free a buffer the real owner also frees —
/// a crash or scribbled value before the checksum prints.
#[test]
fn bytes_usercall_escape_shapes_run_clean_under_malloc_scribble() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("bytes-leak-usercall-escape-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        BYTES_USERCALL_NO_DOUBLE_FREE_SOURCE,
        dir.path(),
        "usercall_escape_shapes",
    );

    let output = Command::new(&bin)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .output()
        .expect("run usercall-escape-shapes binary");

    assert!(
        output.status.success(),
        "usercall escape shapes must run clean under the poisoned allocator — a \
         crash here indicates the nested-fresh-bytes-temp pass mis-dropped a \
         named-binding / returned / record-ingress bytes value (double free);\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        "42OK",
        "usercall escape shapes must print the 3x14 checksum untouched — a \
         scribbled value indicates the producer freed a buffer the real owner \
         still reads;\n{}",
        describe_output(&output)
    );
}
