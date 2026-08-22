//! Dead-recipient fire-and-forget send: undelivered-payload ownership oracle.
//!
//! Delivery is a fire-and-forget send's ONE consumer. On enqueue the mailbox
//! takes ownership of the payload's bytes (heap pointers included) and the
//! checker marks the send argument moved, so no scope-exit drop covers it. Every
//! UNDELIVERED status therefore leaves the prepared carrier owned at the send
//! site, and the send terminator's cleanup plan is its only release point.
//!
//! `hew-codegen-rs/src/llvm.rs` used to route the `ErrActorStopped` status —
//! the documented no-op edge for a send to an already-terminal actor — straight
//! to the successor block, running the cleanup plan ONLY on the trap edge. So
//! every send of an owned value to a dead actor leaked its payload, silently and
//! per send: exactly the shape a supervisor-style broadcast to a churning peer
//! set produces in a loop.
//!
//! ## What this oracle pins
//!
//! 1. **Flat slope over a dead recipient.** A `Sink` actor is crashed once, then
//!    sent owned `string`, owned-record (`{ string, Vec<string> }`), and
//!    `Vec<string>` payloads in a loop. The actor is spawned ONCE, so the only
//!    per-iteration heap is the undelivered payload itself. Measured: pre-fix
//!    240 leaks at 25 iterations and 9990 at 1000 (~10 nodes per iteration);
//!    post-fix 0 at both.
//!
//! 2. **No double release when the recipient dies MID-send.** The release now
//!    runs on the shared undelivered edge that BOTH the stopped-recipient no-op
//!    and the fail-closed trap flow through, so a send racing the recipient's
//!    terminal transition must release exactly once. The race driver interleaves
//!    a crashing fire-and-forget with owned-payload sends under the
//!    `MallocScribble` / `MallocPreScribble` / `MallocGuardEdges` triple: a
//!    double release (or a release of bytes the mailbox already took) aborts
//!    there. The process still exits 1 — the crash it stages is unsupervised —
//!    so the pin is "terminated under its own control with the crash status",
//!    never "was killed".
//!
//! macOS-only (`leaks(1)` and the Darwin poisoned allocator); elsewhere the
//! tests record a counted SKIP. That is why this file carries no
//! `#![cfg(unix)]`: a module-level cfg deletes the tests from the binary on
//! Windows, which is an INVISIBLE skip — the runner reports neither a pass nor
//! a skip for a test it never saw. The per-test
//! `#[cfg_attr(not(target_os = "macos"), ignore = ...)]` is the one gate, and
//! it makes every non-macOS host — Linux and Windows alike — a skip with a
//! reason in the run summary.

mod support;

use support::leak_slope::{
    compile_to_native, measure_leaks, run_probe_witness, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen, run_bounded_command};

use std::process::{Command, Output};

/// Run a compiled probe once and capture its output, bounded.
fn probe_output(bin: &std::path::Path) -> Output {
    run_bounded_command(Command::new(bin), format!("probe {}", bin.display()))
}

/// Low iteration count: exercises the dead-recipient edge enough to clear the
/// constant runtime baseline without approaching the slope signal.
const LOW_ITERS: usize = 25;

/// High iteration count. Pre-fix this drove ~10 leaked heap nodes per iteration
/// across the three payload shapes (owned string; the record's label plus its
/// two-element `Vec<string>` and that vector's own elements; the standalone
/// vector and its elements); post-fix the count holds flat.
const HIGH_ITERS: usize = 1000;

/// Maximum permitted leak-node delta between the HIGH and LOW probes. Same
/// headroom rationale as the sibling oracles: absorbs one-off runtime
/// allocations while still catching a slope orders of magnitude below the
/// pre-fix ~10 nodes/iter.
const SLOPE_TOLERANCE: usize = 8;

/// Iterations for the die-mid-send race driver.
const RACE_ITERS: usize = 400;

/// Dead-recipient driver: crash `Sink` once with an `ask` that panics, then
/// send it owned payloads of three shapes in a loop. Every one of those sends
/// returns `ErrActorStopped` — the documented no-op edge — so the payload is
/// never enqueued and the send site owns its release.
///
/// `println` per iteration is the work witness: a probe that stopped sending
/// (or never reached the loop) shrinks its line count and cannot be read as a
/// flat slope.
fn dead_recipient_source(iters: usize) -> String {
    format!(
        r#"
record Parcel {{
    label: string,
    tags: Vec<string>,
}}

actor Sink {{
    receive fn take_string(payload: string) {{
        println(f"STRING:{{payload}}");
    }}

    receive fn take_record(parcel: Parcel) {{
        println(f"RECORD:{{parcel.label}}");
    }}

    receive fn take_vec(items: Vec<string>) {{
        println(f"VEC:{{items.len()}}");
    }}

    receive fn die() -> i64 {{
        panic("sink crashed before any payload was sent")
    }}
}}

fn parcel(n: i64) -> Parcel {{
    let tags: Vec<string> = [f"tag-{{n}}", "shared-heap-tag".to_upper()];
    Parcel {{ label: f"parcel-{{n}}", tags: tags }}
}}

fn main() {{
    let sink = spawn Sink;
    match await sink.die() {{
        .Ok(_) => println("UNEXPECTED_REPLY"),
        .Err(_) => println("SINK_DEAD"),
    }}
    for i in 0..{iters} {{
        sink.take_string(f"owned-payload-{{i}}".to_upper());
        sink.take_record(parcel(i));
        let items: Vec<string> = [f"item-{{i}}", "vector-heap-element".to_upper()];
        sink.take_vec(items);
        println(f"SENT:{{i}}");
    }}
    println("DONE");
}}
"#
    )
}

/// Race driver, in two phases.
///
/// Phase 1 queues the crashing fire-and-forget in the MIDDLE of a run of
/// owned-payload sends, so the sends around it straddle the recipient's
/// terminal transition — some deliver, some take the stopped-recipient no-op,
/// and at least one issues while the transition is in flight. That is the
/// double-release hazard: both outcomes now flow through the same undelivered
/// release edge.
///
/// Phase 2 settles first, then sends the same payloads again. Those sends
/// cannot deliver — the recipient is terminal and its mailbox is closed — so
/// the stopped-recipient edge is exercised by construction rather than by
/// winning a race. The delivered-line count is the witness: `2 * iters` sends
/// are issued and at most `iters` can ever print, so a run where every send
/// delivered (the edge never taken, the oracle asserting nothing) is visible.
fn die_mid_send_source(iters: usize) -> String {
    format!(
        r#"
actor Sink {{
    receive fn take_string(payload: string) {{
        println(f"STRING:{{payload}}");
    }}

    receive fn die() {{
        panic("sink crashed mid-send")
    }}
}}

fn main() {{
    let sink = spawn Sink;
    for i in 0..{iters} {{
        if i == {iters} / 2 {{
            sink.die();
        }}
        sink.take_string(f"racing-payload-{{i}}".to_upper());
    }}
    sleep(200ms);
    for i in 0..{iters} {{
        sink.take_string(f"post-crash-payload-{{i}}".to_upper());
    }}
    println("RACE_DONE");
}}
"#
    )
}

/// A dead recipient must not accumulate undelivered payloads: the leak-node
/// count is flat across a 40x iteration delta.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn dead_recipient_send_has_no_per_iteration_payload_leak() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("actor-dead-send-leak-")
        .tempdir()
        .expect("tempdir");

    let bin_low = compile_to_native(
        &dead_recipient_source(LOW_ITERS),
        dir.path(),
        "dead_recipient_low",
    );
    let bin_high = compile_to_native(
        &dead_recipient_source(HIGH_ITERS),
        dir.path(),
        "dead_recipient_high",
    );

    // Work witness: both probes must actually reach and complete their send
    // loops. A probe that parked or exited early reports a small leak count
    // that reads as a flat slope while having measured nothing.
    let low_lines = run_probe_witness(&bin_low, &[]);
    let high_lines = run_probe_witness(&bin_high, &[]);
    assert!(
        high_lines > low_lines,
        "dead-recipient probe printed {high_lines} lines at {HIGH_ITERS} iterations and \
         {low_lines} at {LOW_ITERS}: the send loop did not scale with the iteration count, so \
         the leak numbers below are not slope samples"
    );

    // Edge witness: the `Sink` is crashed by an `ask` BEFORE the first payload
    // is sent, so every one of these sends must take the stopped-recipient
    // edge. A single delivered payload would mean the probe was measuring
    // ordinary delivery, not the edge under test.
    let probe_run = probe_output(&bin_low);
    let probe_stdout = String::from_utf8_lossy(&probe_run.stdout);
    assert!(
        probe_stdout.contains("SINK_DEAD"),
        "dead-recipient probe did not observe the recipient crash, so its sends were not to a \
         terminal actor:\nstdout:\n{probe_stdout}"
    );
    assert!(
        !probe_stdout.contains("STRING:")
            && !probe_stdout.contains("RECORD:")
            && !probe_stdout.contains("VEC:"),
        "dead-recipient probe delivered a payload: the recipient was live for at least one \
         send, so the stopped-recipient release edge is not what this slope measures:\n\
         stdout:\n{probe_stdout}"
    );

    let low_leaks = measure_leaks(&bin_low);
    let high_leaks = measure_leaks(&bin_high);

    eprintln!(
        "dead_recipient_send: low_iters={LOW_ITERS} low_leaks={low_leaks} \
         high_iters={HIGH_ITERS} high_leaks={high_leaks} tolerance={SLOPE_TOLERANCE}"
    );
    assert!(
        high_leaks <= low_leaks + SLOPE_TOLERANCE,
        "dead_recipient_send: per-iteration leak SLOPE — low_iters={LOW_ITERS} \
         low_leaks={low_leaks}, high_iters={HIGH_ITERS} high_leaks={high_leaks}. Excess of {} \
         nodes over the tolerance of {SLOPE_TOLERANCE} means a fire-and-forget send to an \
         already-terminal actor is not releasing its undelivered payload: the stopped-recipient \
         edge in `Terminator::Send` lowering is skipping the cleanup plan again. Re-run with \
         `MallocStackLogging=1 leaks --atExit -- {}` for the leaked block's stack.",
        high_leaks.saturating_sub(low_leaks + SLOPE_TOLERANCE),
        bin_high.display()
    );
}

/// A recipient dying MID-send must not double-release the payload: the delivered
/// edge (mailbox owns the bytes) and the undelivered edge (send site owns them)
/// are exclusive, and the release now sits on the shared undelivered edge.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "poisoned-allocator probe is macOS-only; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn recipient_dying_mid_send_does_not_double_release_payload() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("actor-dead-send-race-")
        .tempdir()
        .expect("tempdir");

    let bin = compile_to_native(
        &die_mid_send_source(RACE_ITERS),
        dir.path(),
        "die_mid_send_race",
    );
    let output = run_under_malloc_scribble(&bin);

    // The staged crash is unsupervised, so exit status 1 IS the expected
    // outcome. What must never happen is termination by a signal (SIGABRT from
    // a double free, SIGSEGV from a use-after-free of scribbled bytes).
    assert_eq!(
        output.status.code(),
        Some(1),
        "die-mid-send race did not terminate under its own control with the unsupervised-crash \
         status. A signal here means the undelivered-payload release ran twice, or ran on bytes \
         the mailbox had already taken:\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("RACE_DONE"),
        "die-mid-send race did not complete its send loops, so it never reached the sends that \
         straddle the recipient's terminal transition:\n{}",
        describe_output(&output)
    );

    // Witness that the stopped-recipient edge was actually taken. The driver
    // issues 2 * RACE_ITERS sends; every one of the second loop's is to an
    // actor that is already terminal, so it cannot be enqueued and cannot
    // print. A run where the delivered count reached the issued count would
    // mean no send ever reached the edge under test, and the no-abort result
    // above would be vacuous.
    let issued = RACE_ITERS * 2;
    let delivered = stdout.matches("STRING:").count();
    assert!(
        delivered <= RACE_ITERS,
        "die-mid-send race delivered {delivered} of {issued} sends: the post-crash sends reached \
         a live mailbox, so the stopped-recipient release edge was never exercised and this \
         probe proves nothing:\n{}",
        describe_output(&output)
    );
    assert!(
        stdout.contains("STRING:RACING-PAYLOAD-0"),
        "die-mid-send race delivered nothing at all, so the pre-crash sends never ran and the \
         crash did not interleave with live delivery:\n{}",
        describe_output(&output)
    );
}
