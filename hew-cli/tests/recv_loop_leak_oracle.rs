//! Per-iteration recv-result drop on the loop back-edge.
//!
//! Empirical leak oracle for the heap-owning `Option<T>` let-binding that
//! lives across a loop back-edge in the recv shapes:
//!
//!   * `for await item in rx` over `std::channel::Receiver<string>`,
//!     where the Some-arm binding `item` is the per-iteration heap
//!     holder.
//!   * Source-level `let opt = await rx.recv()` over the same channel,
//!     where `opt: Option<string>` is the per-iteration heap holder and
//!     the Some-arm uses the payload read-only.
//!   * Non-suspending `let opt = rx.try_recv()` in a `while` loop, same
//!     `Option<string>` shape as the above but without the suspend ramp.
//!
//! Before the fix the consuming binding (item / opt) was overwritten on
//! the loop back-edge with no preceding drop — a `+1` `alloc_cstring_
//! data` (32-byte) leak per frame. The fix wires per-iteration drops via
//! two complementary mechanisms (per-arm inline drop on a recv scrutinee
//! for the for-await case; back-edge `Goto` `DropPlan` populated from a
//! scope-filtered subset of `drops_for_exit` for the source-level case).
//!
//! ## Why slope, not constant-delta
//!
//! The earlier oracle compared a zero-frame baseline against a 3-frame
//! run and asserted `three_leaks <= zero_leaks + 1`. That check was
//! brittle: the channel close-state's small allocations (cstring close
//! marker, mutex / `cond_t`) are subject to ±1 baseline jitter, and the
//! +1 tolerance was crossing it nondeterministically (`zero=3 three=5`
//! → false positive). The real bug class is PER-FRAME GROWTH: the
//! original leak added one allocation NODE per iteration, so 30 frames
//! produced ~30 extra leak nodes (trunk pre-fix measurement: `frames=30
//! → 35 leaks`, `frames=100 → 105 leaks` — slope of 1.0 leak / frame).
//!
//! A slope oracle directly measures that bug: compile the same shape at
//! a LOW frame count (small, e.g. 3) and a HIGH frame count (large,
//! e.g. 50) with the SAME channel capacity in both, measure leak NODE
//! counts under `leaks --atExit`, and assert the delta is bounded by a
//! small constant independent of frames. With the fix the leak NODE
//! count is invariant: the only growing allocation is the channel's
//! internal ring buffer (ONE node whose size grows with peak-enqueued
//! items because the channel itself is never dropped at actor exit —
//! a separate pre-existing channel-state issue, out of scope). The
//! ring buffer adds zero new NODES even as it resizes; per-frame
//! cstring leaks would each appear as a separate node.
//!
//! The slope tolerance is `5` leaks for a `50 - 3 = 47`-frame delta:
//! plenty of headroom for one-off scheduler / runtime allocations that
//! sometimes appear in the high-frame run but did not in the low-frame
//! run, while still catching a slope of even ~0.1 leaks/frame (a 4.7-
//! leak excess at delta=47). Trunk pre-fix would fail this oracle by a
//! factor of ~5× (slope 1.0 = 47 excess leaks vs the tolerance of 5).
//!
//! ## Channel capacity
//!
//! Channel capacity is held at a fixed large value (`1024`) so the
//! channel's internal ring buffer's final size is dominated by peak
//! items enqueued, not the constructor argument. The buffer at peak
//! enqueue is one allocation node; its size grows monotonically with
//! frames sent until the actor starts consuming. Both LOW and HIGH
//! probes use the same capacity → the same buffer allocation pattern
//! → no spurious node-count delta from the channel itself.
//!
//! ## Continue back-edge
//!
//! The `continue` back-edge is registered the same way as the natural
//! fall-through (the `Continue` lowering inserts the current block into
//! `loop_back_edge_blocks` before emitting the `Goto`), so
//! `match opt { Some(_) => { ...; continue; } }` releases `opt`'s
//! `EnumInPlace` before re-entering the loop header. Both shapes appear
//! in the slope-tested set.
//!
//! ## Payload-escape UAF probes
//!
//! Separate `carry_continue_payload_escape_no_uaf` /
//! `carry_fallthrough_payload_escape_no_uaf` tests assert STDOUT
//! content (not leak counts) on a `var carry; while { Some(item) =>
//! carry = item; ... } println(carry)` shape. Pre-fix the back-edge
//! `EnumInPlace` freed the payload while `carry` still aliased it and
//! `MallocScribble` poisoned the freed cstring in place, so the
//! post-loop print read an empty string. Post-fix prints the captured
//! value intact. These probes do not need slope sweeping — they assert
//! a single concrete stdout shape.
//!
//! ## Skip behaviour
//!
//! macOS-only oracle (`leaks(1)` is Darwin's Mach-port allocator
//! inspector; Linux has no equivalent). On any other platform the test
//! logs `skip:` and returns without failing. Inside the macOS path
//! `leaks` itself may decline to attach when the binary is not
//! debuggable (rare in CI; logs `skip:` and returns); the assertion
//! only runs when both the LOW and HIGH measurements succeed.

#![cfg(unix)]

mod support;

use std::path::PathBuf;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

// ── per-shape Hew fixtures ────────────────────────────────────────────────
//
// All four recv shapes share the same channel-capacity constant (1024)
// and the same actor-side `sleep_ms(3000)` budget. The capacity is
// large enough that the channel's internal ring buffer reaches the
// same peak allocation for every frame count we probe, so the channel
// state's leaked bytes contribute the SAME node count to both the LOW
// and HIGH measurements. The sleep budget is sized for the actor to
// drain `HIGH_FRAMES` items and exit cleanly before `leaks --atExit`
// snapshots the heap; an actor that has not exited yet would have the
// live channel state counted as "still in use" and hide the bug.

/// Fixed channel capacity used by every probe source. Sized well past
/// any frame count this oracle tests so the internal ring buffer
/// reaches the same peak allocation across LOW and HIGH measurements,
/// contributing one stable node (a single growing allocation) to both
/// leak counts.
const CHANNEL_CAPACITY: usize = 1024;

/// Low frame count: minimum value that exercises the back-edge at
/// least once. With `frames = 3` the loop body runs four times (three
/// drains + the closing `None`), so the back-edge fires three times.
const LOW_FRAMES: usize = 3;

/// High frame count for the slope check. Picked to be large enough
/// that a per-frame leak would dominate any constant-overhead noise
/// (a slope of 1 leak / frame would produce `HIGH_FRAMES - LOW_FRAMES
/// = 47` extra leak nodes against the `SLOPE_TOLERANCE` of 5), yet
/// small enough to keep the test runtime under a second per shape on
/// macOS even when the binary is built in `--release`.
const HIGH_FRAMES: usize = 50;

/// Maximum permitted leak-node delta between the HIGH and LOW probes.
/// Counts excess leak nodes only; the channel's internal ring buffer
/// resizing is ONE node growing in bytes, not multiple new nodes, so
/// it does not contribute. A per-frame allocation leak would each
/// produce its own node — e.g. trunk pre-fix's 32-byte cstring per
/// iteration showed 1.0 leak / frame slope, which over `delta = 47`
/// frames would land at 47 excess leak nodes (9× the tolerance).
const SLOPE_TOLERANCE: usize = 5;

/// `for await item in rx` over `std::channel::Receiver<string>`.
/// `frames` drives the number of `send` calls before the channel
/// closes. Channel capacity is `CHANNEL_CAPACITY` (constant across
/// probes); main's `sleep_ms(3000)` lets the actor drain the queue
/// before the process exits.
fn for_await_source(frames: usize) -> String {
    use std::fmt::Write as _;
    let sends = (0..frames).fold(String::new(), |mut acc, i| {
        let _ = writeln!(acc, "        tx.send(\"f{i}\");");
        acc
    });
    format!(
        "import std::channel::channel;\n\
         \n\
         actor ForAwaitRecv {{\n\
         \x20   receive fn run(unused: i64) {{\n\
         \x20       let (tx, rx): (channel.Sender<string>, channel.Receiver<string>) = channel.new({CHANNEL_CAPACITY});\n\
         {sends}\
         \x20       tx.close();\n\
         \x20       for await item in rx {{\n\
         \x20           println(\"got\");\n\
         \x20       }}\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn main() {{\n\
         \x20   let w = spawn ForAwaitRecv;\n\
         \x20   w.run(0);\n\
         \x20   sleep_ms(3000);\n\
         }}\n"
    )
}

/// Source-level `let opt = await rx.recv(); match opt {{ Some(item) =>
/// println(item), None => stop }}` over the same channel. The leak
/// pre-fix is opt's payload (Local 14 in the elab MIR), not the Some-
/// arm item binding (which is non-escaping); the fix populates opt's
/// back-edge drop plan with the `EnumInPlace` drop on `Option<String>`.
fn await_recv_source(frames: usize) -> String {
    use std::fmt::Write as _;
    let sends = (0..frames).fold(String::new(), |mut acc, i| {
        let _ = writeln!(acc, "        tx.send(\"f{i}\");");
        acc
    });
    format!(
        "import std::channel::channel;\n\
         \n\
         actor AwaitRecv {{\n\
         \x20   receive fn run(unused: i64) {{\n\
         \x20       let (tx, rx): (channel.Sender<string>, channel.Receiver<string>) = channel.new({CHANNEL_CAPACITY});\n\
         {sends}\
         \x20       tx.close();\n\
         \x20       var keep_going = true;\n\
         \x20       while keep_going {{\n\
         \x20           let opt = await rx.recv();\n\
         \x20           match opt {{\n\
         \x20               Some(item) => println(item),\n\
         \x20               None => {{ keep_going = false; }},\n\
         \x20           }}\n\
         \x20       }}\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn main() {{\n\
         \x20   let w = spawn AwaitRecv;\n\
         \x20   w.run(0);\n\
         \x20   sleep_ms(3000);\n\
         }}\n"
    )
}

/// Non-suspending `let opt = rx.try_recv()` loop. Same per-iter
/// `Option<string>` binding shape; the back-edge fix applies
/// identically to the non-suspending recv-result.
fn try_recv_source(frames: usize) -> String {
    use std::fmt::Write as _;
    let sends = (0..frames).fold(String::new(), |mut acc, i| {
        let _ = writeln!(acc, "        tx.send(\"f{i}\");");
        acc
    });
    format!(
        "import std::channel::channel;\n\
         \n\
         actor TryRecv {{\n\
         \x20   receive fn run(unused: i64) {{\n\
         \x20       let (tx, rx): (channel.Sender<string>, channel.Receiver<string>) = channel.new({CHANNEL_CAPACITY});\n\
         {sends}\
         \x20       tx.close();\n\
         \x20       var keep_going = true;\n\
         \x20       while keep_going {{\n\
         \x20           let opt = rx.try_recv();\n\
         \x20           match opt {{\n\
         \x20               Some(item) => println(item),\n\
         \x20               None => {{ keep_going = false; }},\n\
         \x20           }}\n\
         \x20       }}\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn main() {{\n\
         \x20   let w = spawn TryRecv;\n\
         \x20   w.run(0);\n\
         \x20   sleep_ms(3000);\n\
         }}\n"
    )
}

/// `let opt = rx.try_recv(); match opt { Some(_) => { ...; continue; }
/// None => break-out }`. Drives the `continue` back-edge path: the
/// Some arm runs `continue` BEFORE the loop body's natural fall-
/// through, so the body-end Drop is past the terminator. `continue`
/// must register its own back-edge `DropPlan` (mirroring the fall-
/// through Goto) or the per-iteration `Option<string>` leaks. Pre-fix
/// trunk slope: 1.0 leak / frame.
fn try_recv_continue_source(frames: usize) -> String {
    use std::fmt::Write as _;
    let sends = (0..frames).fold(String::new(), |mut acc, i| {
        let _ = writeln!(acc, "        tx.send(\"f{i}\");");
        acc
    });
    format!(
        "import std::channel::channel;\n\
         \n\
         actor TryRecvContinue {{\n\
         \x20   receive fn run(unused: i64) {{\n\
         \x20       let (tx, rx): (channel.Sender<string>, channel.Receiver<string>) = channel.new({CHANNEL_CAPACITY});\n\
         {sends}\
         \x20       tx.close();\n\
         \x20       var keep_going = true;\n\
         \x20       while keep_going {{\n\
         \x20           let opt = rx.try_recv();\n\
         \x20           match opt {{\n\
         \x20               Some(item) => {{\n\
         \x20                   println(\"got\");\n\
         \x20                   continue;\n\
         \x20               }},\n\
         \x20               None => {{ keep_going = false; }},\n\
         \x20           }}\n\
         \x20       }}\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn main() {{\n\
         \x20   let w = spawn TryRecvContinue;\n\
         \x20   w.run(0);\n\
         \x20   sleep_ms(3000);\n\
         }}\n"
    )
}

/// Source-level `let opt = await rx.recv(); match opt { Some(_) =>
/// { ...; continue; } None => break-out }`. Same continue back-edge
/// path as `try_recv_continue_source`, but through the suspending
/// recv ramp — the MIR loop terminator differs
/// (`SuspendingChannelRecv` vs `Call`) but the back-edge `Goto` is
/// the same shape and the same back-edge `DropPlan` registration
/// applies.
fn await_recv_continue_source(frames: usize) -> String {
    use std::fmt::Write as _;
    let sends = (0..frames).fold(String::new(), |mut acc, i| {
        let _ = writeln!(acc, "        tx.send(\"f{i}\");");
        acc
    });
    format!(
        "import std::channel::channel;\n\
         \n\
         actor AwaitRecvContinue {{\n\
         \x20   receive fn run(unused: i64) {{\n\
         \x20       let (tx, rx): (channel.Sender<string>, channel.Receiver<string>) = channel.new({CHANNEL_CAPACITY});\n\
         {sends}\
         \x20       tx.close();\n\
         \x20       var keep_going = true;\n\
         \x20       while keep_going {{\n\
         \x20           let opt = await rx.recv();\n\
         \x20           match opt {{\n\
         \x20               Some(item) => {{\n\
         \x20                   println(\"got\");\n\
         \x20                   continue;\n\
         \x20               }},\n\
         \x20               None => {{ keep_going = false; }},\n\
         \x20           }}\n\
         \x20       }}\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn main() {{\n\
         \x20   let w = spawn AwaitRecvContinue;\n\
         \x20   w.run(0);\n\
         \x20   sleep_ms(3000);\n\
         }}\n"
    )
}

// ── leak measurement plumbing ─────────────────────────────────────────────

/// Compile `source` to a native binary via `hew compile --emit-dir` and
/// return the binary path. The temp dir backs the binary; keep it alive
/// for the duration of the measurement.
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

/// Run `bin` under `MallocScribble` + `MallocGuardEdges` + `leaks
/// --atExit` and return `Some(leak_count)` if `leaks` produced a usable
/// report, `None` if it declined to attach (the binary was not debuggable
/// or `leaks` was unavailable). The leak-count is parsed from the
/// canonical line `Process <pid>: N leaks for B total leaked bytes.` —
/// the only stable field across macOS releases.
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
        // "Process <pid>: N leaks for B total leaked bytes." — the
        // canonical summary line. Reject the "Process <pid>: N nodes
        // malloced for B KB" line that appears AHEAD of the leak summary
        // (it counts total allocations, not leaks).
        if !line.contains(" leaks for ") {
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
            "skip: leaks did not emit a `Process <pid>: N leaks for B total leaked bytes.` \
             summary for {}: stderr=\n{}",
            bin.display(),
            String::from_utf8_lossy(&output.stderr)
        );
    }
    parsed
}

/// Per-shape probe: build a LOW-frame binary and a HIGH-frame binary
/// for the SAME shape, measure leak NODE counts under `leaks --atExit`
/// with the `MallocScribble` / `MallocPreScribble` / `MallocGuardEdges`
/// poisoned-allocator triple, and assert the per-frame slope is below
/// `SLOPE_TOLERANCE`, i.e. `high_leaks` fits within
/// `low_leaks + SLOPE_TOLERANCE`.
///
/// The check ignores BYTES (the channel's internal ring buffer is one
/// allocation that resizes with peak items enqueued — its size grows
/// but the NODE COUNT stays at one). A per-frame allocation leak (the
/// original bug class — one 32-byte `alloc_cstring_data` block per
/// iteration that the back-edge drop should have released) shows up
/// as `HIGH_FRAMES - LOW_FRAMES = 47` extra NODES, far above the
/// tolerance.
///
/// Pre-fix trunk measurement on `for_await_source`:
/// `LOW (3 frames) = 8 leaks, HIGH (30 frames) = 35 leaks` →
/// `slope = (35-8)/(30-3) = 1.0 leaks/frame`. Extrapolated to this
/// oracle's `HIGH_FRAMES=50` the pre-fix delta would be ~47, an order
/// of magnitude above the tolerance. Post-fix both probes hold at the
/// same NODE count (currently 5 across the whole range; tolerance 5
/// absorbs any one-off scheduler / runtime / channel-warmup
/// allocation that sometimes appears only in the HIGH probe).
fn assert_per_frame_slope_below_tolerance(shape_name: &str, source_fn: fn(usize) -> String) {
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
        .prefix(&format!("recv-leak-{shape_name}-"))
        .tempdir()
        .expect("tempdir");

    let bin_low = compile_to_native(
        &source_fn(LOW_FRAMES),
        dir.path(),
        &format!("{shape_name}_low"),
    );
    let bin_high = compile_to_native(
        &source_fn(HIGH_FRAMES),
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
        "{shape_name}: low_frames={LOW_FRAMES} low_leaks={low_leaks} \
         high_frames={HIGH_FRAMES} high_leaks={high_leaks} \
         tolerance={SLOPE_TOLERANCE}"
    );
    assert!(
        high_leaks <= low_leaks + SLOPE_TOLERANCE,
        "{shape_name}: per-frame leak SLOPE — low_frames={LOW_FRAMES} low_leaks={low_leaks}, \
         high_frames={HIGH_FRAMES} high_leaks={high_leaks}. Excess of {} NODES over the \
         tolerance of {SLOPE_TOLERANCE} indicates the back-edge is producing a per-iteration \
         allocation. Trunk pre-fix slope is ~1.0 leak/frame for the original bug \
         (`alloc_cstring_data` per iteration). Re-run with `MallocStackLogging=1 leaks \
         --atExit -- {}` to see which stack the leaked block came from.",
        high_leaks.saturating_sub(low_leaks + SLOPE_TOLERANCE),
        bin_high.display()
    );
    // Also assert non-negative growth — a HIGH probe with FEWER leaks
    // than LOW would mean the higher-frame run did not actually drain
    // the channel before `leaks --atExit` snapshotted, so the test is
    // not measuring what we think. The sleep budget in main is sized
    // for `HIGH_FRAMES` to complete; if this fires, increase it.
    assert!(
        high_leaks + SLOPE_TOLERANCE >= low_leaks,
        "{shape_name}: HIGH leak count is more than {SLOPE_TOLERANCE} below LOW \
         (low={low_leaks}, high={high_leaks}) — the actor likely did not finish \
         draining {HIGH_FRAMES} frames before `leaks --atExit` snapshotted. Increase \
         the `sleep_ms(...)` budget in the shape source."
    );
}

// ── per-shape slope tests ─────────────────────────────────────────────────

/// `for await item in rx` over `Receiver<string>`: no per-frame leak
/// node growth. Drives the per-arm inline-drop discipline keyed on
/// the recv-call scrutinee + Some-arm payload binding. The original
/// bug class produced one `alloc_cstring_data` allocation per
/// iteration (trunk slope: 1.0 leak / frame); the slope check fails
/// at +47 NODES against the +5 tolerance.
#[test]
fn for_await_recv_string_loop_no_per_frame_leak_slope() {
    assert_per_frame_slope_below_tolerance("for_await", for_await_source);
}

/// Source-level `let opt = await rx.recv()` + `match opt`: no
/// per-frame leak node growth. Drives the back-edge `DropPlan`
/// discipline (`binding_scope` + `loop_back_edge_blocks` populating
/// the back-edge `Goto` plan with the scope-filtered `EnumInPlace`
/// drop).
#[test]
fn source_await_recv_string_loop_no_per_frame_leak_slope() {
    assert_per_frame_slope_below_tolerance("await_recv", await_recv_source);
}

/// Non-suspending `let opt = rx.try_recv()` loop: no per-frame leak
/// node growth. Drives the same back-edge `DropPlan` path; `try_recv`
/// differs from `await rx.recv()` only at the call seam (no suspend
/// ramp), so the MIR loop shape is identical and the same back-edge
/// drop applies.
#[test]
fn try_recv_string_loop_no_per_frame_leak_slope() {
    assert_per_frame_slope_below_tolerance("try_recv", try_recv_source);
}

/// `let opt = rx.try_recv(); match opt { Some(_) => { ...; continue; }
/// None => stop }`: no per-frame leak node growth. Drives the
/// `continue` back-edge `DropPlan` registration; the Some arm's
/// `continue` jumps to the loop header BEFORE the loop body's natural
/// fall-through Goto, so the fall-through registration alone would
/// miss this exit. Trunk pre-fix slope on this shape mirrors
/// `for_await` (1.0 leak / frame).
#[test]
fn try_recv_continue_loop_no_per_frame_leak_slope() {
    assert_per_frame_slope_below_tolerance("try_recv_continue", try_recv_continue_source);
}

/// Same continue back-edge probe but through the suspending
/// `await rx.recv()` ramp. Confirms the back-edge `DropPlan`
/// registration fires regardless of whether the recv terminator is
/// `Call` (`try_recv`) or `SuspendingChannelRecv` (await recv) —
/// both shapes feed the same `Goto` back to the loop header, and the
/// scope-filtered drop discipline is terminator-agnostic.
#[test]
fn await_recv_continue_loop_no_per_frame_leak_slope() {
    assert_per_frame_slope_below_tolerance("await_recv_continue", await_recv_continue_source);
}
//
// The leak oracle above only asserts "no per-iteration cstring leak".
// A SECOND class of bug lives on the same back-edge: an outer/surviving
// local that aliases the payload via `carry = item` would, if the
// EnumInPlace back-edge drop fires anyway, observe a USE-AFTER-FREE on
// the next read — the buffer was freed, the alias still points to it,
// `MallocScribble` poisons the freed bytes, the print reads empty
// poisoned memory instead of the captured payload.
//
// The fix is in `derive_enum_composite_drop_allowed`: the payload-binder
// forward-propagation now refuses to mark a Move destination as a
// "benign onward hand-off" unless the destination binding shares the
// same declaring scope as the source binder. An outer-scope `carry`
// fails the scope match, the source `item` is therefore seen as
// escaping into general storage, the EnumInPlace is excluded, and the
// back-edge no longer frees the payload while `carry` aliases it.
//
// These probes assert STDOUT content (not just leak counts) because the
// UAF signature is "captured payload reads as empty after the back-edge
// drop, under `MallocScribble + MallocPreScribble`" — only a stdout
// equality check catches that. Each probe sends ONE frame so there is
// exactly one back-edge to exercise, and reads the carried payload
// after the loop closes.

/// `var carry; while { match opt { Some(item) => { carry = item;
/// continue; } None => break-out } } println(carry)`. Pre-fix, the
/// payload-binder forward-propagation marked `carry` as a benign
/// onward hand-off (any `Move dest=carry src=item` propagated), so the
/// `EnumInPlace` was admitted on the back-edge, the payload was freed,
/// and the post-loop `println(carry)` read poisoned memory. Post-fix,
/// `carry`'s outer scope ≠ `item`'s arm scope → no propagation → escape
/// scan sees `item → carry` as an unbound-destination escape → root
/// excluded → no back-edge drop → no UAF.
fn carry_continue_escape_source() -> String {
    "import std::channel::channel;\n\
     \n\
     actor CarryContinueEscape {\n\
     \x20   receive fn run(unused: i64) {\n\
     \x20       let (tx, rx): (channel.Sender<string>, channel.Receiver<string>) = channel.new(4);\n\
     \x20       tx.send(\"escaped\");\n\
     \x20       tx.close();\n\
     \x20       var carry = \"init\";\n\
     \x20       var keep_going = true;\n\
     \x20       println(\"before\");\n\
     \x20       while keep_going {\n\
     \x20           let opt = rx.try_recv();\n\
     \x20           match opt {\n\
     \x20               Some(item) => {\n\
     \x20                   carry = item;\n\
     \x20                   continue;\n\
     \x20               },\n\
     \x20               None => { keep_going = false; },\n\
     \x20           }\n\
     \x20       }\n\
     \x20       println(carry);\n\
     \x20       println(\"after\");\n\
     \x20   }\n\
     }\n\
     \n\
     fn main() {\n\
     \x20   let w = spawn CarryContinueEscape;\n\
     \x20   w.run(0);\n\
     \x20   sleep_ms(100);\n\
     }\n"
        .to_string()
}

/// Same payload-escape shape but the Some arm falls through naturally
/// (sets `keep_going = false` instead of `continue`-ing). The
/// back-edge here is the loop body's natural Goto, not the
/// `Continue` lowering, so this exercises the OTHER back-edge entry
/// point in `loop_back_edge_blocks`. The escape-scan fix must reject
/// both — the propagation step runs once and feeds both back-edge
/// registrations identically.
fn carry_fallthrough_escape_source() -> String {
    "import std::channel::channel;\n\
     \n\
     actor CarryFallEscape {\n\
     \x20   receive fn run(unused: i64) {\n\
     \x20       let (tx, rx): (channel.Sender<string>, channel.Receiver<string>) = channel.new(4);\n\
     \x20       tx.send(\"escaped\");\n\
     \x20       tx.close();\n\
     \x20       var carry = \"init\";\n\
     \x20       var keep_going = true;\n\
     \x20       println(\"before\");\n\
     \x20       while keep_going {\n\
     \x20           let opt = rx.try_recv();\n\
     \x20           match opt {\n\
     \x20               Some(item) => {\n\
     \x20                   carry = item;\n\
     \x20                   keep_going = false;\n\
     \x20               },\n\
     \x20               None => { keep_going = false; },\n\
     \x20           }\n\
     \x20       }\n\
     \x20       println(carry);\n\
     \x20       println(\"after\");\n\
     \x20   }\n\
     }\n\
     \n\
     fn main() {\n\
     \x20   let w = spawn CarryFallEscape;\n\
     \x20   w.run(0);\n\
     \x20   sleep_ms(100);\n\
     }\n"
        .to_string()
}

/// Build the binary, run it under `MallocScribble + MallocPreScribble +
/// MallocGuardEdges` (the macOS poisoned-allocator triple that turns a
/// use-after-free into an observable "freed memory reads as poisoned"
/// signal), and assert each `expected` line appears in stdout in order.
/// A back-edge that wrongly frees an outer-aliased payload would either
/// print an empty middle line (the cstring buffer was freed-and-
/// poisoned in place; print reads the poison-init bytes as a
/// zero-length string) or crash (when the guard pages catch the read).
fn assert_payload_escape_prints(shape_name: &str, source: &str, expected: &[&str]) {
    if !cfg!(target_os = "macos") {
        eprintln!("skip: {shape_name}: payload-escape probe is macOS-only (MallocScribble)");
        return;
    }
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("recv-escape-{shape_name}-"))
        .tempdir()
        .expect("tempdir");

    let bin = compile_to_native(source, dir.path(), shape_name);

    let output = Command::new(&bin)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .output()
        .expect("run payload-escape probe binary");

    assert!(
        output.status.success(),
        "{shape_name}: binary exited non-zero (likely a UAF caught by MallocGuardEdges):\n{}",
        describe_output(&output)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    let lines: Vec<&str> = stdout.lines().collect();
    assert_eq!(
        lines.len(),
        expected.len(),
        "{shape_name}: stdout line count mismatch — expected {} lines {expected:?}, got {} lines {lines:?}.\n\
         A middle-line mismatch with an EMPTY line in the freed slot is the canonical UAF signature: \
         the back-edge `EnumInPlace` freed the payload while `carry` still aliased it, and `MallocScribble` \
         poisoned the freed buffer so the print reads a zero-length string.",
        expected.len(),
        lines.len(),
    );
    for (i, (got, want)) in lines.iter().zip(expected.iter()).enumerate() {
        assert_eq!(
            got, want,
            "{shape_name}: stdout line {i} mismatch — expected {want:?}, got {got:?}. \
             If the middle line is empty, the back-edge drop freed an outer-aliased payload (UAF).\n\
             Full stdout:\n{stdout}"
        );
    }
}

/// Outer-scope payload alias via `carry = item` followed by `continue`,
/// then `println(carry)` after the loop. Must print `before / escaped /
/// after`. Pre-fix: `before / "" / after` under `MallocScribble +
/// MallocPreScribble` — the buffer was freed by the back-edge
/// `EnumInPlace` and `MallocScribble` poisoned the freed cstring so
/// the post-loop print read a zero-length string.
#[test]
fn carry_continue_payload_escape_no_uaf() {
    assert_payload_escape_prints(
        "carry_continue_escape",
        &carry_continue_escape_source(),
        &["before", "escaped", "after"],
    );
}

/// Same shape with natural fall-through instead of `continue`. The
/// fallthrough back-edge and the `continue` back-edge are registered
/// at different code paths (`loop_back_edge_blocks` is populated at
/// the body bottom for fallthrough and at the `Continue` lowering for
/// the explicit form), but the escape-scan check that gates the
/// `EnumInPlace` admission runs once per function and protects both.
#[test]
fn carry_fallthrough_payload_escape_no_uaf() {
    assert_payload_escape_prints(
        "carry_fallthrough_escape",
        &carry_fallthrough_escape_source(),
        &["before", "escaped", "after"],
    );
}
