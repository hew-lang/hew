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
         \x20   let buf: bytes;\n\
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
         \x20   sleep_ms(2000);\n\
         \x20   match await store.get_len() {{\n\
         \x20       Ok(v) => v,\n\
         \x20       Err(_) => -1,\n\
         \x20   }}\n\
         }}\n"
    )
}

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
