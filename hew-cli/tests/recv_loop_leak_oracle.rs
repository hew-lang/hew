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
//! `match opt { .Some(_) => { ...; continue; } }` releases `opt`'s
//! `EnumInPlace` before re-entering the loop header. Both shapes appear
//! in the slope-tested set.
//!
//! ## Payload-escape UAF probes
//!
//! Separate `carry_continue_payload_escape_no_uaf` /
//! `carry_fallthrough_payload_escape_no_uaf` tests assert STDOUT
//! content (not leak counts) on a `var carry; while { .Some(item) =>
//! carry = item; ... } println(carry)` shape. Pre-fix the back-edge
//! `EnumInPlace` freed the payload while `carry` still aliased it and
//! `MallocScribble` poisoned the freed cstring in place, so the
//! post-loop print read an empty string. Post-fix prints the captured
//! value intact. These probes do not need slope sweeping — they assert
//! a single concrete stdout shape.
//!
//! ## Skip behaviour
//!
//! macOS-only oracle: `leaks(1)` is Darwin's Mach-port allocator
//! inspector and Linux has no equivalent, and the payload-escape UAF
//! probes need the `MallocScribble` / `MallocPreScribble` /
//! `MallocGuardEdges` poisoned allocator to turn a use-after-free into
//! an observable signal. Every test here is therefore annotated
//! `#[cfg_attr(not(target_os = "macos"), ignore = "…")]`, which the
//! runner RECORDS as a skip with its reason.
//!
//! Nothing in this file skips at run time any more. It used to: the
//! slope harness returned early on a non-macOS host, again when `leaks`
//! was missing from `PATH`, and again whenever `leaks` declined to
//! attach — each logging `skip:` to stderr and returning, so the
//! `#[test]` reported PASS having asserted nothing. That made the whole
//! file green on Linux and Windows always, and on macOS whenever the
//! tool was absent. An absent capability is not a green result (the
//! shape deleted from `try_require_wasi_runner` in `#2826`): the
//! macOS-only condition is now a compile-time `ignore` the runner
//! counts, and every other way of failing to measure — no `leaks` on a
//! macOS host, `leaks` declining to attach, a probe that does not run to
//! completion — is a FAILURE. See `support::leak_slope`.
//!
//! ## Work witness
//!
//! A slope oracle whose HIGH probe never ran the loop under test
//! measures nothing while reporting a flat slope. Every slope test here
//! goes through `assert_frame_slope_below_tolerance_exact_lines`, which
//! runs each probe plainly, requires a successful exit, and pins the
//! stdout line count to the shape's declared per-frame output. That is
//! not a hypothetical guard: the `for_await_stream_bytes` fixture
//! shipped emitting `frames²` sends into a `CHANNEL_CAPACITY`-bounded
//! pipe, so its HIGH probe (`50² = 2500 > 1024`) parked forever on
//! backpressure and drained ZERO frames from the day it landed.

#![cfg(unix)]

mod support;

use std::path::Path;
use std::process::Command;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, assert_frame_slope_below_tolerance_exact_lines,
    compile_to_native, measure_leaks, measure_leaks_exact, require_leaks_tool,
    require_macos_poisoned_allocator, run_under_malloc_scribble, HIGH_FRAMES, LOW_FRAMES,
    SLOPE_TOLERANCE,
};
use support::{describe_output, require_codegen, run_bounded_command};

/// Every slope shape in this file prints exactly one line per frame from
/// its consuming loop body, so the drained-frame count is the frame
/// count. Passed to
/// [`assert_frame_slope_below_tolerance_exact_lines`] as the work
/// witness: a probe that prints a different number of lines did not
/// drain what it was asked to and its leak count is not a slope sample.
fn one_line_per_frame(frames: usize) -> usize {
    frames
}

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
///
/// It is also a HARD CEILING on how many items a probe may enqueue
/// before its consumer starts draining. Every shape here makes one
/// actor both producer and consumer — the sends all run before the
/// drain loop — and these are BOUNDED, blocking, backpressured
/// channels and pipes. A shape that enqueues more than this parks
/// forever inside its send with nothing left to drain it, never
/// reaches `close()`, and measures a stalled process instead of a
/// per-frame slope. Keep every probe's send count at exactly one per
/// frame so `HIGH_FRAMES` sends stay far below this.
const CHANNEL_CAPACITY: usize = 1024;

// Frame counts and tolerance come from `support::leak_slope`, the single
// authority for the slope methodology; they are the same values this
// file used to define privately. `LOW_FRAMES = 3` is the minimum that
// exercises the back-edge (the body runs four times: three drains plus
// the closing `None`). `HIGH_FRAMES = 50` makes a 1-leak/frame slope
// worth 47 excess NODES against `SLOPE_TOLERANCE = 5`. The tolerance
// counts excess NODES only: a channel's internal ring buffer resizing
// is ONE node growing in bytes, while a per-frame allocation leak
// produces one node each.

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
        "import std.channel.channel;\n\
         \n\
         actor ForAwaitRecv {{\n\
         \x20   receive fn run(unused: i64) {{\n\
         \x20       let (tx, rx): (channel.Sender<string>, channel.Receiver<string>) = match channel.new({CHANNEL_CAPACITY}) {{ .Ok(pair) => pair, .Err(error) => panic(error), }};\n\
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
         \x20   sleep(3000ms);\n\
         }}\n"
    )
}

// ── Direct Receiver handoff: park, then destroy ───────────────────────────
//
// The full-drain `for_await_source` above proves the ordinary source-to-cursor
// handoff and the per-item drop path, but closes the sender before the loop.
// Every receive therefore completes immediately. This fixture makes the
// complementary ownership seam observable: each child transfers its direct
// `Receiver<string>` into `for await`, consumes exactly one owned payload, and
// then parks on the next receive while its Sender remains live in the same
// coroutine frame. `supervisor_stop` destroys those parked children through the
// production actor teardown path. A stale source authority would double-drop
// the receiver handle; a missing cursor authority would leak it (and its live
// Sender/channel state) per child.
//
// `frames` is the number of children deliberately parked at the same seam.
// The high/low leak comparison catches a per-child abandon leak, while the
// poisoned allocator run makes the competing source/cursor drop authority an
// immediate failure. Each child prints exactly once after consuming its owned
// value, so the stdout count is a work witness that every handoff reached the
// parked second receive before teardown.
fn parked_for_await_receiver_handoff_source(frames: usize) -> String {
    use std::fmt::Write as _;

    let children = (0..frames).fold(String::new(), |mut acc, index| {
        let _ = writeln!(acc, "    child receiver{index}: ParkedReceiver;");
        acc
    });
    let starts = (0..frames).fold(String::new(), |mut acc, index| {
        let _ = writeln!(
            acc,
            "    let receiver{index} = app.receiver{index};\n    receiver{index}.run({index});"
        );
        acc
    });

    format!(
        "import std.channel.channel;\n\
         \n\
         actor ParkedReceiver {{\n\
         \x20   receive fn run(index: i64) {{\n\
         \x20       let (tx, rx): (channel.Sender<string>, channel.Receiver<string>) = match channel.new(1) {{ .Ok(pair) => pair, .Err(error) => panic(error), }};\n\
         \x20       let payload = f\"parked-direct-receiver-{{index}}\";\n\
         \x20       tx.send(payload);\n\
         \x20       for await item in rx {{\n\
         \x20           if item.len() <= 0 {{ panic(\"receiver payload\"); }}\n\
         \x20           println(\"parked\");\n\
         \x20       }}\n\
         \x20   }}\n\
         }}\n\
         \n\
         supervisor App {{\n\
         \x20   strategy: one_for_one;\n\
         \x20   intensity: 3 within 60s;\n\
         {children}\
         }}\n\
         \n\
         fn main() {{\n\
         \x20   let app = spawn App;\n\
         {starts}\
         \x20   sleep(500ms);\n\
         \x20   supervisor_stop(app);\n\
         }}\n"
    )
}

/// The ordinary run is a work witness for the slope samples and the poisoned
/// run. A successful `leaks --atExit` invocation alone cannot prove the probe
/// reached its second, parked receive: the inspector reports its own status
/// rather than the inspected program's. Require every requested child to have
/// consumed its first frame and printed the exact marker before teardown.
fn assert_parked_for_await_receiver_work(bin: &Path, expected_frames: usize, context: &str) {
    let output = run_bounded_command(
        Command::new(bin),
        format!("run parked direct Receiver handoff ({context})"),
    );
    assert!(
        output.status.success(),
        "{context}: parked direct Receiver handoff failed:\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    let lines: Vec<_> = stdout.lines().collect();
    assert_eq!(
        lines.len(),
        expected_frames,
        "{context}: work witness expected {expected_frames} parked Receiver handoffs, got {} lines {lines:?}. A missing line means the child did not consume its owned first payload and park on the next `for await` receive before supervisor teardown.",
        lines.len(),
    );
    assert!(
        lines.iter().all(|line| *line == "parked"),
        "{context}: work witness had an unexpected marker sequence {lines:?}; every child must consume exactly one direct Receiver payload before being destroyed while parked",
    );
}

// ── Drop-only Vec<Receiver<T>> ownership ──────────────────────────────────
//
// A Receiver cannot be cloned, so `Vec<Receiver<T>>` uses the owned move-in
// element ABI with a descriptor whose clone thunk is null and whose drop thunk
// closes exactly one slot. Each helper invocation below creates a fresh channel,
// explicitly closes its Sender, moves the sole Receiver into `[rx]`, witnesses
// the Vec length, and returns so the Vec owns the only remaining endpoint drop.
//
// Repeating the helper makes both failure directions authoritative:
//
//   * a missing/incorrect descriptor drop grows one live channel allocation
//     family per call and fails the exact-zero LOW/HIGH endpoint checks;
//   * retaining source authority after `hew_vec_push_owned_move` closes the same
//     Receiver twice when the source binding and Vec unwind, which the HIGH
//     MallocScribble run turns into an abort.
fn receiver_vec_drop_only_source(frames: usize) -> String {
    use std::fmt::Write as _;

    let calls = (0..frames).fold(String::new(), |mut acc, _| {
        let _ = writeln!(acc, "    drop_receiver_vec();");
        acc
    });
    format!(
        "import std.channel.channel;\n\
         \n\
         fn drop_receiver_vec() {{\n\
         \x20   let (tx, rx): (channel.Sender<i64>, channel.Receiver<i64>) = match channel.new(1) {{ .Ok(pair) => pair, .Err(error) => panic(error), }};\n\
         \x20   tx.close();\n\
         \x20   let receivers: Vec<channel.Receiver<i64>> = [rx];\n\
         \x20   if receivers.len() != 1 {{ panic(\"receiver Vec move\"); }}\n\
         \x20   println(\"receiver-vec-dropped\");\n\
         }}\n\
         \n\
         fn main() {{\n\
         {calls}\
         }}\n"
    )
}

fn assert_receiver_vec_drop_only_work(bin: &Path, expected_frames: usize, context: &str) {
    let output = run_bounded_command(
        Command::new(bin),
        format!("run drop-only Receiver Vec fixture ({context})"),
    );
    assert!(
        output.status.success(),
        "{context}: drop-only Receiver Vec fixture failed:\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    let lines: Vec<_> = stdout.lines().collect();
    assert_eq!(
        lines.len(),
        expected_frames,
        "{context}: work witness expected {expected_frames} dropped Receiver Vecs, got {} lines {lines:?}",
        lines.len(),
    );
    assert!(
        lines.iter().all(|line| *line == "receiver-vec-dropped"),
        "{context}: drop-only Receiver Vec fixture had an unexpected marker sequence {lines:?}",
    );
}

// ── Cloneable Vec<Sender<T>> ownership ───────────────────────────────────
//
// Sender is the complementary endpoint contract to the drop-only Receiver
// above: moving `tx` into the first Vec transfers its endpoint authority, and
// cloning that Vec must call the descriptor's sender-clone thunk once for its
// sole slot.  Both Vec values then release their separate sender references at
// scope exit, while the paired Receiver is closed explicitly.  Repeating this
// complete lifecycle catches every refcount imbalance directly:
//
//   * a missing Vec clone retain makes the second Vec drop an unowned sender
//     reference (MallocScribble turns that use-after-free into a failure);
//   * a missing Vec-slot close retains one sender/channel family per helper
//     invocation and fails the exact-zero endpoint checks;
//   * retaining source authority after move-in closes one sender twice.
//
// The `sender-vec-cloned` line is an exact work witness.  It proves the clone
// and both length reads completed before either Vec reaches scope teardown;
// a binary that failed before taking the clone would otherwise look leak-free
// merely because it did no relevant work.
fn sender_vec_clone_drop_source(frames: usize) -> String {
    use std::fmt::Write as _;

    let calls = (0..frames).fold(String::new(), |mut acc, _| {
        let _ = writeln!(acc, "    clone_and_drop_sender_vec();");
        acc
    });
    format!(
        "import std.channel.channel;\n\
         \n\
         fn clone_and_drop_sender_vec() {{\n\
         \x20   let (tx, rx): (channel.Sender<i64>, channel.Receiver<i64>) = match channel.new(1) {{ .Ok(pair) => pair, .Err(error) => panic(error), }};\n\
         \x20   let senders: Vec<channel.Sender<i64>> = [tx];\n\
         \x20   let senders_copy = senders.clone();\n\
         \x20   if senders.len() != 1 {{ panic(\"sender Vec source clone\"); }}\n\
         \x20   if senders_copy.len() != 1 {{ panic(\"sender Vec cloned clone\"); }}\n\
         \x20   rx.close();\n\
         \x20   println(\"sender-vec-cloned\");\n\
         }}\n\
         \n\
         fn main() {{\n\
         {calls}\
         }}\n"
    )
}

fn assert_sender_vec_clone_drop_work(bin: &Path, expected_frames: usize, context: &str) {
    let output = run_bounded_command(
        Command::new(bin),
        format!("run cloneable Sender Vec fixture ({context})"),
    );
    assert!(
        output.status.success(),
        "{context}: cloneable Sender Vec fixture failed:\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    let lines: Vec<_> = stdout.lines().collect();
    assert_eq!(
        lines.len(),
        expected_frames,
        "{context}: work witness expected {expected_frames} cloned Sender Vec lifecycles, got {} lines {lines:?}",
        lines.len(),
    );
    assert!(
        lines.iter().all(|line| *line == "sender-vec-cloned"),
        "{context}: cloneable Sender Vec fixture had an unexpected marker sequence {lines:?}",
    );
}

/// Source-level `let opt = await rx.recv(); match opt {{ .Some(item) =>
/// println(item), .None => stop }}` over the same channel. The leak
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
        "import std.channel.channel;\n\
         \n\
         actor AwaitRecv {{\n\
         \x20   receive fn run(unused: i64) {{\n\
         \x20       let (tx, rx): (channel.Sender<string>, channel.Receiver<string>) = match channel.new({CHANNEL_CAPACITY}) {{ .Ok(pair) => pair, .Err(error) => panic(error), }};\n\
         {sends}\
         \x20       tx.close();\n\
         \x20       var keep_going = true;\n\
         \x20       while keep_going {{\n\
         \x20           let opt = await rx.recv();\n\
         \x20           match opt {{\n\
         \x20               Some(item) => println(item),\n\
         \x20               .None => {{ keep_going = false; }},\n\
         \x20           }}\n\
         \x20       }}\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn main() {{\n\
         \x20   let w = spawn AwaitRecv;\n\
         \x20   w.run(0);\n\
         \x20   sleep(3000ms);\n\
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
        "import std.channel.channel;\n\
         \n\
         actor TryRecv {{\n\
         \x20   receive fn run(unused: i64) {{\n\
         \x20       let (tx, rx): (channel.Sender<string>, channel.Receiver<string>) = match channel.new({CHANNEL_CAPACITY}) {{ .Ok(pair) => pair, .Err(error) => panic(error), }};\n\
         {sends}\
         \x20       tx.close();\n\
         \x20       var keep_going = true;\n\
         \x20       while keep_going {{\n\
         \x20           let opt = rx.try_recv();\n\
         \x20           match opt {{\n\
         \x20               Some(item) => println(item),\n\
         \x20               .None => {{ keep_going = false; }},\n\
         \x20           }}\n\
         \x20       }}\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn main() {{\n\
         \x20   let w = spawn TryRecv;\n\
         \x20   w.run(0);\n\
         \x20   sleep(3000ms);\n\
         }}\n"
    )
}

/// `let opt = rx.try_recv(); match opt { .Some(_) => { ...; continue; }
/// .None => break-out }`. Drives the `continue` back-edge path: the
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
        "import std.channel.channel;\n\
         \n\
         actor TryRecvContinue {{\n\
         \x20   receive fn run(unused: i64) {{\n\
         \x20       let (tx, rx): (channel.Sender<string>, channel.Receiver<string>) = match channel.new({CHANNEL_CAPACITY}) {{ .Ok(pair) => pair, .Err(error) => panic(error), }};\n\
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
         \x20               .None => {{ keep_going = false; }},\n\
         \x20           }}\n\
         \x20       }}\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn main() {{\n\
         \x20   let w = spawn TryRecvContinue;\n\
         \x20   w.run(0);\n\
         \x20   sleep(3000ms);\n\
         }}\n"
    )
}

/// Source-level `let opt = await rx.recv(); match opt { .Some(_) =>
/// { ...; continue; } .None => break-out }`. Same continue back-edge
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
        "import std.channel.channel;\n\
         \n\
         actor AwaitRecvContinue {{\n\
         \x20   receive fn run(unused: i64) {{\n\
         \x20       let (tx, rx): (channel.Sender<string>, channel.Receiver<string>) = match channel.new({CHANNEL_CAPACITY}) {{ .Ok(pair) => pair, .Err(error) => panic(error), }};\n\
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
         \x20               .None => {{ keep_going = false; }},\n\
         \x20           }}\n\
         \x20       }}\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn main() {{\n\
         \x20   let w = spawn AwaitRecvContinue;\n\
         \x20   w.run(0);\n\
         \x20   sleep(3000ms);\n\
         }}\n"
    )
}

/// Owned-payload send loop: every frame sends a FRESH heap string
/// (`f"item-{i}"` — an owned f-string allocation per iteration), then
/// drains via `for await`. The other shapes in this suite send string
/// LITERALS, which are immortal rodata (no header, no drop obligation)
/// — they exercise only the recv side. This shape covers the SEND seam
/// for owned payloads: the per-iteration owned string flows through the
/// `tx.send(s)` wrapper param into the intercepted borrow-contract call,
/// and the sender-side binding `s` must still be released exactly once
/// per iteration. An unreleased sender-side owner (or a retained copy
/// the borrow contract failed to balance) shows up as 1.0 leak / frame.
fn owned_send_source(frames: usize) -> String {
    format!(
        "import std.channel.channel;\n\
         \n\
         actor OwnedSend {{\n\
         \x20   receive fn run(unused: i64) {{\n\
         \x20       let (tx, rx): (channel.Sender<string>, channel.Receiver<string>) = match channel.new({CHANNEL_CAPACITY}) {{ .Ok(pair) => pair, .Err(error) => panic(error), }};\n\
         \x20       var i: i64 = 0;\n\
         \x20       while i < {frames} {{\n\
         \x20           let s = f\"item-{{i}}\";\n\
         \x20           tx.send(s);\n\
         \x20           i = i + 1;\n\
         \x20       }}\n\
         \x20       tx.close();\n\
         \x20       for await item in rx {{\n\
         \x20           println(\"got\");\n\
         \x20       }}\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn main() {{\n\
         \x20   let w = spawn OwnedSend;\n\
         \x20   w.run(0);\n\
         \x20   sleep(3000ms);\n\
         }}\n"
    )
}

// ── per-shape slope tests ─────────────────────────────────────────────────

/// `for await item in rx` over `Receiver<string>`: no per-frame leak
/// node growth. Drives the per-arm inline-drop discipline keyed on
/// the recv-call scrutinee + Some-arm payload binding. The original
/// bug class produced one `alloc_cstring_data` allocation per
/// iteration (trunk slope: 1.0 leak / frame); the slope check fails
/// at +47 NODES against the +5 tolerance.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn for_await_recv_string_loop_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance_exact_lines(
        "for_await",
        for_await_source,
        one_line_per_frame,
    );
}

/// Direct `Receiver<string>` ownership handoff under the hard lifecycle edge:
/// every child has transferred `rx` into `for await`, consumed one owned value,
/// then is destroyed while parked on the next receive. The low/high samples
/// must have a flat leak slope, and the high sample must complete cleanly under
/// Darwin's poisoned allocator — together pin no leaked cursor and no duplicate
/// source/cursor drop authority on abandonment.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn parked_for_await_receiver_handoff_has_flat_leak_slope_and_no_double_free() {
    require_leaks_tool();
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("parked-for-await-receiver-")
        .tempdir()
        .expect("tempdir");
    let low = compile_to_native(
        &parked_for_await_receiver_handoff_source(LOW_FRAMES),
        dir.path(),
        "parked_for_await_receiver_low",
    );
    let high = compile_to_native(
        &parked_for_await_receiver_handoff_source(HIGH_FRAMES),
        dir.path(),
        "parked_for_await_receiver_high",
    );

    assert_parked_for_await_receiver_work(&low, LOW_FRAMES, "low plain run");
    assert_parked_for_await_receiver_work(&high, HIGH_FRAMES, "high plain run");

    let scribble = run_under_malloc_scribble(&high);
    assert!(
        scribble.status.success(),
        "parked direct Receiver handoff aborted under the poisoned allocator — the source and cursor may both have released the handoff handle on the abandon edge:\n{}",
        describe_output(&scribble)
    );
    let scribble_stdout = String::from_utf8_lossy(&scribble.stdout);
    let scribble_lines: Vec<_> = scribble_stdout.lines().collect();
    assert_eq!(
        scribble_lines.len(),
        HIGH_FRAMES,
        "poisoned allocator run did not witness all {HIGH_FRAMES} parked Receiver handoffs: {scribble_lines:?}"
    );
    assert!(
        scribble_lines.iter().all(|line| *line == "parked"),
        "poisoned allocator run had an unexpected work witness {scribble_lines:?}"
    );

    let low_leaks = measure_leaks(&low);
    let high_leaks = measure_leaks(&high);
    eprintln!(
        "parked_for_await_receiver: low_children={LOW_FRAMES} low_leaks={low_leaks} high_children={HIGH_FRAMES} high_leaks={high_leaks} tolerance={SLOPE_TOLERANCE}"
    );
    assert!(
        high_leaks <= low_leaks + SLOPE_TOLERANCE,
        "parked direct Receiver handoff leaked on the destroy-while-parked edge: low_children={LOW_FRAMES} low_leaks={low_leaks}, high_children={HIGH_FRAMES} high_leaks={high_leaks}, tolerance={SLOPE_TOLERANCE}. A positive slope means the transferred source or cursor was not reclaimed per stopped child. Re-run with `MallocStackLogging=1 leaks --atExit -- {}`",
        high.display()
    );
}

/// `Vec<Receiver<i64>>` owns its moved-in endpoint through a clone-null,
/// drop-present element descriptor. Both LOW and HIGH samples must be exact
/// zero-leak endpoints, and the HIGH sample must unwind cleanly under Darwin's
/// poisoned allocator. This pins the runtime behavior of the descriptor path,
/// not merely its emitted IR shape.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn receiver_vec_drop_only_has_zero_leak_endpoints_and_no_double_close() {
    require_leaks_tool();
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("receiver-vec-drop-only-")
        .tempdir()
        .expect("tempdir");
    let low = compile_to_native(
        &receiver_vec_drop_only_source(LOW_FRAMES),
        dir.path(),
        "receiver_vec_drop_only_low",
    );
    let high = compile_to_native(
        &receiver_vec_drop_only_source(HIGH_FRAMES),
        dir.path(),
        "receiver_vec_drop_only_high",
    );

    assert_receiver_vec_drop_only_work(&low, LOW_FRAMES, "low plain run");
    assert_receiver_vec_drop_only_work(&high, HIGH_FRAMES, "high plain run");

    let scribble = run_under_malloc_scribble(&high);
    assert!(
        scribble.status.success(),
        "drop-only Receiver Vec aborted under the poisoned allocator — source and slot may both have closed the moved endpoint:\n{}",
        describe_output(&scribble)
    );
    let scribble_stdout = String::from_utf8_lossy(&scribble.stdout);
    let scribble_lines: Vec<_> = scribble_stdout.lines().collect();
    assert_eq!(
        scribble_lines.len(),
        HIGH_FRAMES,
        "poisoned allocator run did not witness all {HIGH_FRAMES} Receiver Vec drops: {scribble_lines:?}"
    );
    assert!(
        scribble_lines
            .iter()
            .all(|line| *line == "receiver-vec-dropped"),
        "poisoned allocator run had an unexpected Receiver Vec work witness {scribble_lines:?}"
    );

    let low_leaks = measure_leaks_exact(&low);
    let high_leaks = measure_leaks_exact(&high);
    eprintln!(
        "receiver_vec_drop_only: low_frames={LOW_FRAMES} low={low_leaks:?} high_frames={HIGH_FRAMES} high={high_leaks:?}"
    );
    assert_eq!(
        low_leaks,
        (0, 0),
        "drop-only Receiver Vec LOW endpoint leaked: {low_leaks:?}"
    );
    assert_eq!(
        high_leaks,
        (0, 0),
        "drop-only Receiver Vec HIGH endpoint leaked: {high_leaks:?}; a per-Vec endpoint leak must not be hidden behind a slope tolerance"
    );
}

/// `Vec<Sender<i64>>` moves the original endpoint into an owned descriptor,
/// clones it through that descriptor's sender retain thunk, then releases both
/// Vecs while an explicitly closed paired Receiver completes the channel
/// lifecycle.  LOW and HIGH endpoints must both be exactly zero leaks; the
/// HIGH `MallocScribble` execution catches duplicate/missing sender ownership
/// before a flat slope could hide it.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn sender_vec_clone_drop_has_zero_leak_endpoints_and_no_double_free() {
    require_leaks_tool();
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("sender-vec-clone-drop-")
        .tempdir()
        .expect("tempdir");
    let low = compile_to_native(
        &sender_vec_clone_drop_source(LOW_FRAMES),
        dir.path(),
        "sender_vec_clone_drop_low",
    );
    let high = compile_to_native(
        &sender_vec_clone_drop_source(HIGH_FRAMES),
        dir.path(),
        "sender_vec_clone_drop_high",
    );

    assert_sender_vec_clone_drop_work(&low, LOW_FRAMES, "low plain run");
    assert_sender_vec_clone_drop_work(&high, HIGH_FRAMES, "high plain run");

    let scribble = run_under_malloc_scribble(&high);
    assert!(
        scribble.status.success(),
        "cloneable Sender Vec aborted under the poisoned allocator — source, clone, or one Vec slot may have released an endpoint twice:\n{}",
        describe_output(&scribble)
    );
    let scribble_stdout = String::from_utf8_lossy(&scribble.stdout);
    let scribble_lines: Vec<_> = scribble_stdout.lines().collect();
    assert_eq!(
        scribble_lines.len(),
        HIGH_FRAMES,
        "poisoned allocator run did not witness all {HIGH_FRAMES} cloned Sender Vec lifecycles: {scribble_lines:?}"
    );
    assert!(
        scribble_lines
            .iter()
            .all(|line| *line == "sender-vec-cloned"),
        "poisoned allocator run had an unexpected Sender Vec work witness {scribble_lines:?}"
    );

    let low_leaks = measure_leaks_exact(&low);
    let high_leaks = measure_leaks_exact(&high);
    eprintln!(
        "sender_vec_clone_drop: low_frames={LOW_FRAMES} low={low_leaks:?} high_frames={HIGH_FRAMES} high={high_leaks:?}"
    );
    assert_eq!(
        low_leaks,
        (0, 0),
        "cloneable Sender Vec LOW endpoint leaked: {low_leaks:?}"
    );
    assert_eq!(
        high_leaks,
        (0, 0),
        "cloneable Sender Vec HIGH endpoint leaked: {high_leaks:?}; a per-Vec clone/drop imbalance must not be hidden behind a slope tolerance"
    );
}

/// Source-level `let opt = await rx.recv()` + `match opt`: no
/// per-frame leak node growth. Drives the back-edge `DropPlan`
/// discipline (`binding_scope` + `loop_back_edge_blocks` populating
/// the back-edge `Goto` plan with the scope-filtered `EnumInPlace`
/// drop).
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn source_await_recv_string_loop_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance_exact_lines(
        "await_recv",
        await_recv_source,
        one_line_per_frame,
    );
}

/// Non-suspending `let opt = rx.try_recv()` loop: no per-frame leak
/// node growth. Drives the same back-edge `DropPlan` path; `try_recv`
/// differs from `await rx.recv()` only at the call seam (no suspend
/// ramp), so the MIR loop shape is identical and the same back-edge
/// drop applies.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn try_recv_string_loop_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance_exact_lines("try_recv", try_recv_source, one_line_per_frame);
}

/// `let opt = rx.try_recv(); match opt { .Some(_) => { ...; continue; }
/// .None => stop }`: no per-frame leak node growth. Drives the
/// `continue` back-edge `DropPlan` registration; the Some arm's
/// `continue` jumps to the loop header BEFORE the loop body's natural
/// fall-through Goto, so the fall-through registration alone would
/// miss this exit. Trunk pre-fix slope on this shape mirrors
/// `for_await` (1.0 leak / frame).
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn try_recv_continue_loop_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance_exact_lines(
        "try_recv_continue",
        try_recv_continue_source,
        one_line_per_frame,
    );
}

/// Same continue back-edge probe but through the suspending
/// `await rx.recv()` ramp. Confirms the back-edge `DropPlan`
/// registration fires regardless of whether the recv terminator is
/// `Call` (`try_recv`) or `SuspendingChannelRecv` (await recv) —
/// both shapes feed the same `Goto` back to the loop header, and the
/// scope-filtered drop discipline is terminator-agnostic.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn await_recv_continue_loop_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance_exact_lines(
        "await_recv_continue",
        await_recv_continue_source,
        one_line_per_frame,
    );
}

/// Owned f-string payload sent per frame (`let s = f"item-{i}";
/// tx.send(s);`): no per-frame leak node growth on EITHER side of the
/// channel. The literal-sending shapes above prove only the recv side
/// (a literal has no send-side drop obligation); this shape proves the
/// send seam — the owned string's shared ownership across the
/// `Sender::send` wrapper param and the intercepted borrow-contract
/// call must balance to exactly one release per iteration. Measured on
/// the string retain-on-share branch: LOW(3)=5, HIGH(50)=5 leak nodes
/// (slope 0; the channel ring buffer grows in bytes, not nodes). An
/// unbalanced send-side owner would show slope 1.0 = +47 nodes.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn owned_payload_send_loop_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance_exact_lines(
        "owned_send",
        owned_send_source,
        one_line_per_frame,
    );
}

// ── Stream<bytes> per-frame leak oracle ───────────────────────────────────
//
// The string oracle covers `Option<string>` (single-`ptr` slot). `bytes`
// is the WIDER ABI variant: a native `bytes` value is a stack-resident
// `BytesTriple { ptr, i32, i32 }`, not a single owned pointer. The
// matching recv surface today is `for await frame in <Stream<bytes>>`
// (the layout-witness pop hands the consumer a fresh refcounted triple
// per frame). Pre-fix, the per-iteration Some-arm payload was overwritten
// on the back-edge with no `hew_bytes_drop` call (the codegen-side cow_
// heap_release table only handles single-ptr slot shapes, so the inline-
// drop path skipped Bytes entirely); the consumer leaked one refcounted
// allocation per frame (`hew_bytes_from_str` stack — 1.0 leak/frame
// trunk slope, same shape as the original string bug class).
//
// The fix is two-ended:
//
//   * MIR `generator_yield_drop_symbol` adds a `ResolvedTy::Bytes =>
//     Some("hew_bytes_drop")` arm so the recv-scrutinee Some-arm payload
//     binding receives an inline `Instr::Drop` (the same registration
//     mechanism the String arm uses).
//   * Codegen `lower_inline_drop` intercepts `(ty=Bytes,
//     drop_fn="hew_bytes_drop")` BEFORE the cow_heap_release_symbol
//     congruence check (Bytes is deliberately absent from that table —
//     its slot shape is not single-ptr) and routes to
//     `emit_bytes_inplace_drop`: GEP triple-field-0 → load data ptr →
//     `hew_bytes_drop(data_ptr)` → null-store field 0 for idempotency.
//     A defensive explicit `StateFieldCloneKind::Bytes` arm in
//     `emit_field_drop_step` mirrors the same triple-aware path for
//     embedded Bytes fields (enum/record payload drop helpers) so the
//     pre-existing generic single-`ptr`-load fall-through is replaced by
//     a layout-explicit emitter: a future BytesTriple field reshuffle
//     breaks loudly instead of silently freeing the wrong eightbyte.
//
// Probe shape rationale: the producer pre-allocates ONE `bytes` value
// (`let b = "...".to_bytes()`) outside the loop and reuses it every
// frame via `await sink.send(b)`. `hew_sink_write_bytes` BORROWS the
// buffer (caller retains ownership — `hew-runtime/src/stream.rs` doc
// on `hew_sink_write_bytes`), and the layout-witness pop returns a
// FRESH refcounted triple on the consumer side, so per-iteration
// allocation happens at pop time (the consumer side — what this oracle
// covers). The producer holds a single function-scope `b` whose
// function-scope drop is a separate concern: bytes locals now have a
// scope-exit drop authority (`derive_local_bytes_drop_allowed` in
// `hew-mir/src/lower.rs`), but `b` is read by `await sink.send(b)` —
// a terminator the fail-closed escape scan treats as an ownership
// escape — so the prover excludes it (leak, never double-free) until
// the suspending-send borrow contract is encoded in the allow-list.
// That single producer leak is a CONSTANT
// (one node, independent of frame count), so the slope check sees
// the same 1-node baseline at LOW and HIGH probes; the slope tolerance
// of 5 absorbs it with headroom.
//
// Send count MUST stay at exactly one per loop iteration. `bytes_pipe`
// is a BOUNDED, blocking, backpressured pipe (`std/stream.hew`), and
// this shape makes ONE actor both producer and consumer: the sends all
// run before the drain loop starts. Emit more than `CHANNEL_CAPACITY`
// sends and the actor parks forever inside `await sink.send(b)` with
// nothing left to drain it — it never reaches `sink.close()`, never
// enters the `for await`, and the probe measures a stalled process's
// orphaned pipe buffer instead of a per-frame drop slope. That is the
// defect this fixture shipped with: it spliced `frames` copies of the
// send inside `while i < frames`, so the send count was `frames²` and
// the HIGH probe at `50² = 2500 > 1024` drained ZERO frames.
// `assert_per_frame_slope_below_tolerance`'s drained-frame witness now
// makes that stall a failure instead of a measurement.

/// `for await frame in <Stream<bytes>>`: the canonical recv-scrutinee
/// payload-binding shape for the Bytes ABI variant. `frames` controls
/// the number of `sink.send(b)` calls before `sink.close()`; the
/// consumer drains via `for await`. Pre-fix the per-iteration triple
/// from the layout-witness pop is overwritten on the back-edge with
/// no `hew_bytes_drop` — 1.0 leak/frame (consumer-side). Post-fix the
/// Some-arm payload binding's `Instr::Drop { ty: Bytes, drop_fn:
/// Some("hew_bytes_drop") }` registration releases the triple's data
/// buffer on every body-end edge.
///
/// One send per iteration, so the total send count is `frames` and
/// stays under the bounded pipe's `CHANNEL_CAPACITY` — see the
/// send-count note above the shape rationale.
fn for_await_stream_bytes_source(frames: usize) -> String {
    format!(
        "import std.stream;\n\
         \n\
         actor ForAwaitStreamBytes {{\n\
         \x20   receive fn run(unused: i64) {{\n\
         \x20       let (sink, input) = match stream.bytes_pipe({CHANNEL_CAPACITY}) {{ .Ok(pair) => pair, .Err(error) => panic(error), }};\n\
         \x20       let b = \"frame-some-long-data\".to_bytes();\n\
         \x20       var i: i64 = 0;\n\
         \x20       while i < {frames} {{\n\
         \x20           await sink.send(b);\n\
         \x20           i = i + 1;\n\
         \x20       }}\n\
         \x20       sink.close();\n\
         \x20       for await frame in input {{\n\
         \x20           println(\"got\");\n\
         \x20       }}\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn main() {{\n\
         \x20   let w = spawn ForAwaitStreamBytes;\n\
         \x20   w.run(0);\n\
         \x20   sleep(3000ms);\n\
         }}\n"
    )
}

/// `for await frame in <Stream<bytes>>` per-frame leak-slope oracle.
///
/// Trunk PRE-FIX leak counts on this exact probe (reuse-one-`bytes`
/// producer, ONE send per iteration):
///   * LOW (3 frames):  ~4   leaks (consumer-side per-frame + 1 constant)
///   * HIGH (50 frames): ~51 leaks (consumer-side per-frame + 1 constant)
///   * slope ≈ 1.0 leak/frame → +47 over the +5 tolerance → loud
///     failure.
///
/// POST-FIX leak counts on the same probe:
///   * LOW: 1, HIGH: 1 — slope = 0 (the producer's single pre-
///     allocated `b` is the only leak; consumer-side per-frame leak
///     is now zero). Re-measured out to 500 frames: still 1.
///
/// The MIR `generator_yield_drop_symbol` Bytes arm + codegen
/// `lower_inline_drop` Bytes interceptor are what put the
/// `hew_bytes_drop` call in the for-await body: without them the body
/// block emits none (IR-verified — `grep -c hew_bytes_drop <ir>`
/// returns 0 pre-fix; post-fix the body-end block carries exactly one
/// `call void @hew_bytes_drop(ptr %bytes_drop_ptr)` preceded by the
/// triple-field-0 GEP + load and followed by the null-store). This
/// test fails trunk-style by a factor of ~10× if either end is
/// reverted.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn for_await_stream_bytes_loop_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance_exact_lines(
        "for_await_stream_bytes",
        for_await_stream_bytes_source,
        one_line_per_frame,
    );
}

// ── Stream<bytes> payload-escape UAF probe ────────────────────────────────
//
// Mirrors the carry_*_payload_escape_no_uaf string probes but with the
// Bytes ABI variant. An outer-scope `var carry` aliasing the per-frame
// payload via `carry = frame` would, if the back-edge inline-drop fires
// anyway, free the carried triple's data buffer while `carry` still
// aliases it; `MallocScribble + MallocPreScribble` poisons the freed
// buffer in place; the post-loop `println(carry.to_string())` would
// read either poisoned bytes (printed as garbage or empty) or trip a
// `MallocGuardEdges` page guard (process abort, non-zero exit).
//
// The string-side fix lives in `derive_enum_composite_drop_allowed`'s
// payload-binder forward-propagation (refusing to mark Move destinations
// in OUTER scopes as benign onward hand-offs); the same gate fires for
// Bytes because it is keyed on the local and binding scope, not the
// type. This probe asserts the gate does fire for Bytes (no UAF on
// `carry` after the back-edge).
//
// Stream<bytes> sends ONE frame so there is exactly one back-edge to
// exercise. The carried frame is round-tripped through `.to_string()`
// at print time (Bytes → String for the println call); a poisoned
// or freed underlying buffer would surface as empty/garbage output
// or a crash.

/// `var carry; for await frame in <Stream<bytes>>(1 frame) { carry =
/// frame; }; println(carry.to_string())`. With the back-edge inline-
/// drop for Bytes wired AND the escape-scan gate honouring scope
/// boundaries (the same type-agnostic local-based gate that protects
/// String payload escapes), the carried frame's buffer must survive
/// past the loop body for the post-loop print to read it intact. A
/// regression in the escape-scan that admitted the back-edge drop for
/// Bytes would print empty / poisoned bytes here.
fn carry_for_await_bytes_escape_source() -> String {
    "import std.stream;\n\
     \n\
     actor CarryStreamBytesEscape {\n\
     \x20   receive fn run(unused: i64) {\n\
     \x20       let (sink, input) = match stream.bytes_pipe(4) { .Ok(pair) => pair, .Err(error) => panic(error), };\n\
     \x20       let b = \"escaped-bytes-payload\".to_bytes();\n\
     \x20       await sink.send(b);\n\
     \x20       sink.close();\n\
     \x20       var carry = \"init\".to_bytes();\n\
     \x20       println(\"before\");\n\
     \x20       for await frame in input {\n\
     \x20           carry = frame;\n\
     \x20       }\n\
     \x20       println(carry.to_string());\n\
     \x20       println(\"after\");\n\
     \x20   }\n\
     }\n\
     \n\
     fn main() {\n\
     \x20   let w = spawn CarryStreamBytesEscape;\n\
     \x20   w.run(0);\n\
     \x20   sleep(200ms);\n\
     }\n"
    .to_string()
}

/// `for await frame in <Stream<bytes>>` with outer-scope `carry =
/// frame`: must print `before / escaped-bytes-payload / after`. If
/// the Bytes back-edge inline-drop ever became scope-blind and freed
/// `frame` while `carry` aliased it, the middle line would be empty
/// or garbage under `MallocScribble + MallocPreScribble`, or the
/// process would abort under `MallocGuardEdges`. Either is a UAF
/// and this assertion catches it. The escape gate is local-based
/// (not type-keyed) so the same gate that protects String payload
/// escapes protects Bytes payload escapes; this probe asserts the
/// cross-type invariant directly.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn carry_for_await_bytes_payload_escape_no_uaf() {
    assert_payload_escape_prints(
        "carry_for_await_bytes_escape",
        &carry_for_await_bytes_escape_source(),
        &["before", "escaped-bytes-payload", "after"],
    );
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

/// `var carry; while { match opt { .Some(item) => { carry = item;
/// continue; } .None => break-out } } println(carry)`. Pre-fix, the
/// payload-binder forward-propagation marked `carry` as a benign
/// onward hand-off (any `Move dest=carry src=item` propagated), so the
/// `EnumInPlace` was admitted on the back-edge, the payload was freed,
/// and the post-loop `println(carry)` read poisoned memory. Post-fix,
/// `carry`'s outer scope ≠ `item`'s arm scope → no propagation → escape
/// scan sees `item → carry` as an unbound-destination escape → root
/// excluded → no back-edge drop → no UAF.
fn carry_continue_escape_source() -> String {
    "import std.channel.channel;\n\
     \n\
     actor CarryContinueEscape {\n\
     \x20   receive fn run(unused: i64) {\n\
     \x20       let (tx, rx): (channel.Sender<string>, channel.Receiver<string>) = match channel.new(4) { .Ok(pair) => pair, .Err(error) => panic(error), };\n\
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
     \x20               .None => { keep_going = false; },\n\
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
     \x20   sleep(100ms);\n\
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
    "import std.channel.channel;\n\
     \n\
     actor CarryFallEscape {\n\
     \x20   receive fn run(unused: i64) {\n\
     \x20       let (tx, rx): (channel.Sender<string>, channel.Receiver<string>) = match channel.new(4) { .Ok(pair) => pair, .Err(error) => panic(error), };\n\
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
     \x20               .None => { keep_going = false; },\n\
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
     \x20   sleep(100ms);\n\
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
    // No runtime skip: the poisoned allocator this probe depends on is a
    // macOS facility, and that is a compile-time fact gated by the
    // `#[cfg_attr(not(target_os = "macos"), ignore = "…")]` on each caller
    // so the runner records a SKIP. Reaching here off macOS means the
    // attribute is missing, and the guard says so loudly rather than
    // returning green having asserted nothing.
    require_macos_poisoned_allocator();
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
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
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
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn carry_fallthrough_payload_escape_no_uaf() {
    assert_payload_escape_prints(
        "carry_fallthrough_escape",
        &carry_fallthrough_escape_source(),
        &["before", "escaped", "after"],
    );
}

// ── Actor-generator stream: loop-var release across the early-return edge ──
//
// `for await v in m.items()` over a `receive gen fn` stream. The consuming
// body releases its received value on EVERY path out of the body: the
// fall-through body-end drop, the break/continue edge drops, and the early-
// `return` edge. Pre-fix, a `return` on any body path was treated as an
// ownership escape of the loop variable by the body walk, which suppressed
// the body-end release for the WHOLE binding — every received string leaked
// one `alloc_cstring_data` node per yield (trunk measurement on the 50-frame
// probe: 38 leaked nodes, all rooted in `hew_stream_next_layout`'s decode
// copy), not just the returning iteration's.
//
// The `return v` shape (the loop variable itself moved to the caller) is the
// exactly-once wall: the ReturnSlot move is a real escape, so BOTH the
// body-end release and the return-edge release must stay suppressed — the
// scribble pin below catches an over-emitted release as a poisoned read.

/// Full drain of a `receive gen fn` string stream — the headline shape of
/// the consumer-side loop-var discipline (one received string per yield,
/// released at body end each iteration). Slope 0 pins it.
fn gen_stream_string_drain_source(frames: usize) -> String {
    format!(
        "actor Maker {{\n\
         \x20   receive gen fn items() -> string {{\n\
         \x20       for i in 0..{frames} {{\n\
         \x20           yield f\"item-{{i}}\";\n\
         \x20       }}\n\
         \x20   }}\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   var seen: i64 = 0;\n\
         \x20   let m = spawn Maker;\n\
         \x20   for await v in m.items() {{\n\
         \x20       if v.len() < 6 {{ return 91; }}\n\
         \x20       seen = seen + 1;\n\
         \x20   }}\n\
         \x20   if seen != {frames} {{ return 92; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Mid-drain early `return`: the iterations before the hit release at body
/// end, the returning iteration releases on the return edge. Pre-fix this
/// leaked EVERY iteration's received string (the return path poisoned the
/// body walk), slope 1.0/frame.
fn gen_stream_string_early_return_source(frames: usize) -> String {
    let stop_at = frames / 2 + 1;
    format!(
        "actor Maker {{\n\
         \x20   receive gen fn items() -> string {{\n\
         \x20       for i in 0..{frames} {{\n\
         \x20           yield f\"item-{{i}}\";\n\
         \x20       }}\n\
         \x20   }}\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   var seen: i64 = 0;\n\
         \x20   let m = spawn Maker;\n\
         \x20   for await v in m.items() {{\n\
         \x20       if v.len() < 6 {{ return 91; }}\n\
         \x20       seen = seen + 1;\n\
         \x20       if seen >= {stop_at} {{\n\
         \x20           return 0;\n\
         \x20       }}\n\
         \x20   }}\n\
         \x20   92\n\
         }}\n"
    )
}

/// Bytes variant of the early-return shape: the received `bytes` triple
/// releases through `hew_bytes_drop` on the same edges as the string case.
fn gen_stream_bytes_early_return_source(frames: usize) -> String {
    let stop_at = frames / 2 + 1;
    format!(
        "actor Maker {{\n\
         \x20   receive gen fn frames() -> bytes {{\n\
         \x20       for i in 0..{frames} {{\n\
         \x20           yield \"frame-data\".to_bytes();\n\
         \x20       }}\n\
         \x20   }}\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   var seen: i64 = 0;\n\
         \x20   let m = spawn Maker;\n\
         \x20   for await b in m.frames() {{\n\
         \x20       if b.len() != 10 {{ return 91; }}\n\
         \x20       seen = seen + 1;\n\
         \x20       if seen >= {stop_at} {{\n\
         \x20           return 0;\n\
         \x20       }}\n\
         \x20   }}\n\
         \x20   92\n\
         }}\n"
    )
}

/// `return v` — the loop variable moves to the caller, who reads it after
/// the stream loop is gone. Neither the body-end release nor the return-edge
/// release may fire (over-emitting frees the caller's string; under the
/// scribble triple the post-return `println` reads poison or trips a guard
/// page).
fn gen_stream_return_carry_source() -> String {
    "actor Maker {\n\
     \x20   receive gen fn items() -> string {\n\
     \x20       yield \"escaped\";\n\
     \x20       yield \"rest\";\n\
     \x20   }\n\
     }\n\
     fn first(m: LocalPid<Maker>) -> string {\n\
     \x20   for await v in m.items() {\n\
     \x20       return v;\n\
     \x20   }\n\
     \x20   \"none\"\n\
     }\n\
     fn main() {\n\
     \x20   println(\"before\");\n\
     \x20   let m = spawn Maker;\n\
     \x20   let s = first(m);\n\
     \x20   println(s);\n\
     \x20   println(\"after\");\n\
     }\n"
    .to_string()
}

/// Full-drain slope oracle for the actor-generator string stream (the
/// consumer decode copy is released at body end, one per yield).
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn gen_stream_string_drain_no_per_frame_leak_slope() {
    // This shape prints nothing: it signals a short drain through its EXIT
    // CODE (`92` when `seen != frames`), which the harness's work witness
    // checks by requiring a successful plain run.
    assert_frame_slope_below_tolerance("gen_stream_string_drain", gen_stream_string_drain_source);
}

/// Early-return slope oracle: a `return`-carrying body path must not
/// suppress the per-iteration release (pre-fix slope 1.0 leak/frame).
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn gen_stream_string_early_return_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "gen_stream_string_early_return",
        gen_stream_string_early_return_source,
    );
}

/// Bytes variant of the early-return slope oracle.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn gen_stream_bytes_early_return_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "gen_stream_bytes_early_return",
        gen_stream_bytes_early_return_source,
    );
}

/// Exactly-once wall for the return edge: a loop variable RETURNED to the
/// caller is caller-owned; the return-edge release must stay suppressed.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn gen_stream_returned_loop_var_no_uaf() {
    assert_payload_escape_prints(
        "gen_stream_return_carry",
        &gen_stream_return_carry_source(),
        &["before", "escaped", "after"],
    );
}

// ── Identity-callee forwarding: `wrap(v)` on the return / break edges ──────
//
// The generator/recv-yield exit-edge ledger's escape scan
// (`generator_yield_terminator_escapes`) used to classify EVERY
// `Terminator::Call` argument as a non-escaping borrow (a structural "any
// call is a borrow" rule). That is wrong when the callee forwards its
// argument back out as its own return value: `wrap` below is an identity
// pass-through (a validator / `.trim()`-style wrapper / decorator shape),
// so `v` and `wrap(v)`'s result alias the SAME underlying buffer. The
// escape scan said "no escape", so the exit-edge ledger still fired a
// `Drop` on `v`'s place AFTER `wrap` had already threaded that buffer into
// its own return slot — a silent use-after-free: the caller reads the
// freed-and-poisoned buffer as an empty string instead of the real value
// (GitHub issue #2412). The fix makes the scan consult the SAME closed
// ownership-contract list the body-end scan's `CallRuntimeAbi` arm uses
// (`call_args_borrow_safe` in `hew-mir/src/lower.rs`); a
// directly-resolved Hew function like `wrap`, which is not on that list,
// now counts as an escape and the exit-edge drop is retracted.
//
// The identical corruption reproduces via `break` (`carry = wrap(v);
// break;`) because both edges share the same ledger emitter
// (`emit_generator_yield_value_drops_for_exit_edge`) and the same escape
// scan — a pre-existing defect on `break`/`continue`, which wired the
// ledger before the `return` edge existed.

/// `fn wrap(v: string) -> string { return v; }` — an identity pass-through
/// callee. `return wrap(v)` forwards the loop variable through the call;
/// neither the body-end release nor the return-edge release may fire on
/// `v` (the call's result IS `v`'s buffer, now owned by the caller through
/// `wrap`'s own return). Pre-fix: the exit-edge ledger dropped `v` after
/// `wrap` returned, freeing the buffer the caller is about to read —
/// `println(s)` printed an empty string instead of `escaped` under the
/// scribble triple.
fn gen_stream_return_forwarded_via_call_source() -> String {
    "actor Maker {\n\
     \x20   receive gen fn items() -> string {\n\
     \x20       yield \"escaped\";\n\
     \x20       yield \"rest\";\n\
     \x20   }\n\
     }\n\
     fn wrap(v: string) -> string {\n\
     \x20   return v;\n\
     }\n\
     fn first(m: LocalPid<Maker>) -> string {\n\
     \x20   for await v in m.items() {\n\
     \x20       return wrap(v);\n\
     \x20   }\n\
     \x20   \"none\"\n\
     }\n\
     fn main() {\n\
     \x20   println(\"before\");\n\
     \x20   let m = spawn Maker;\n\
     \x20   let s = first(m);\n\
     \x20   println(s);\n\
     \x20   println(\"after\");\n\
     }\n"
    .to_string()
}

/// `break`-analog of the identity-forwarding shape: `carry = wrap(v);
/// break;`. Same ledger emitter, same escape scan, same identity callee —
/// proves the fix closes both exit edges, not just `return`.
fn gen_stream_break_forwarded_via_call_source() -> String {
    "actor Maker {\n\
     \x20   receive gen fn items() -> string {\n\
     \x20       yield \"escaped\";\n\
     \x20       yield \"rest\";\n\
     \x20   }\n\
     }\n\
     fn wrap(v: string) -> string {\n\
     \x20   return v;\n\
     }\n\
     fn main() {\n\
     \x20   println(\"before\");\n\
     \x20   let m = spawn Maker;\n\
     \x20   var carry = \"init\";\n\
     \x20   for await v in m.items() {\n\
     \x20       carry = wrap(v);\n\
     \x20       break;\n\
     \x20   }\n\
     \x20   println(carry);\n\
     \x20   println(\"after\");\n\
     }\n"
    .to_string()
}

/// Identity-callee forwarding on the RETURN edge must not free the buffer
/// the caller is about to read. `GuardMalloc` ×3 in the flake gate.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn gen_stream_return_forwarded_via_call_no_uaf() {
    assert_payload_escape_prints(
        "gen_stream_return_forwarded_via_call",
        &gen_stream_return_forwarded_via_call_source(),
        &["before", "escaped", "after"],
    );
}

/// Identity-callee forwarding on the BREAK edge — same ledger emitter and
/// escape scan as the return edge, so the fix must close both. `GuardMalloc`
/// ×3 in the flake gate.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn gen_stream_break_forwarded_via_call_no_uaf() {
    assert_payload_escape_prints(
        "gen_stream_break_forwarded_via_call",
        &gen_stream_break_forwarded_via_call_source(),
        &["before", "escaped", "after"],
    );
}
