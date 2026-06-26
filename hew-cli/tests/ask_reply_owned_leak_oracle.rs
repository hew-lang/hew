//! Owned ask-reply ownership leak oracle (#1739 / #1735).
//!
//! An ask reply whose type embeds heap (`string`/`Bytes`/`Vec`/`HashMap`/
//! `HashSet`/`Closure`/owned-field struct) is byte-copied into the reply
//! channel buffer, aliasing that heap into the channel. Every *non-consume*
//! exit edge of the channel must release it exactly once:
//!
//!   * delivered-but-never-consumed (a select loser whose reply already
//!     landed, or a timed-out/shut-down waiter) — released on the channel
//!     teardown leg;
//!   * cancelled-before-delivery / per-reply OOM (the reply lands on an
//!     already-abandoned channel) — released on the `hew_reply` false leg.
//!
//! The runtime makes the channel's registered reply destructor the single
//! canonical reaper for all of those edges (the consumed edge is the waiter's
//! responsibility). Codegen registers that destructor at each ask-caller site.
//! Before the fix the byte-copied buffer was freed with `libc::free` and the
//! embedded heap leaked on every one of these edges.
//!
//! ## What this oracle pins (deterministically)
//!
//! 1. **Cancel-leg reclaim, flat slope.** A slow owned-`string` replier that
//!    always loses a 1 ms timeout: every reply lands on the cancelled channel
//!    (the `hew_reply` false leg). The producer is `spawn`ed ONCE so the only
//!    per-iteration heap is the abandoned owned reply. Post-fix the leak count
//!    is FLAT (≈0) across a 50× iteration delta; pre-fix it grows ~1 leak/iter
//!    (measured: 20 → 1168 leaks over 20 → 2000 iters). This is the headline
//!    `#1739` regression at the integration level.
//!
//! 2. **No over-eager free on either teardown leg.** Both the cancel-leg
//!    driver and a never-consumed select-loser driver run under the
//!    `MallocScribble` / `MallocGuardEdges` / `MallocCheckHeapEach` poisoned
//!    allocator: a destructor that double-freed the embedded heap (e.g. ran on
//!    a buffer the waiter also drops, or on both the producer value and the
//!    published buffer) aborts here before any leak count is read. This is the
//!    central hazard of making the channel the single reaper.
//!
//! ## Why the never-consumed driver is a no-abort probe only (not a slope)
//!
//! A select winner binds its owned reply, and a *bound-but-unused* match/select
//! arm binding of an owned payload is not yet scope-dropped — a PRE-EXISTING,
//! reply-channel-INDEPENDENT consume-side drop gap (a plain `let s = e; s.len()`
//! drops correctly; `select { reply from a.m() => 0, ... }` leaks the winner's
//! `reply`). A per-`spawn` actor allocation leaks similarly. Both contaminate a
//! consume/loop leak slope but neither aborts, so the never-consumed leg is
//! pinned by the deterministic no-double-free guard here, and by the
//! `hew-runtime` reply-destructor counter unit tests (the authoritative,
//! contamination-free leak proof). The cancel-leg slope driver sidesteps both
//! by spawning once and never binding an owned reply.
//!
//! macOS-only (`leaks(1)` is Darwin's allocator inspector); skips elsewhere.

#![cfg(unix)]

mod support;

use std::path::PathBuf;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

/// Low iteration count: exercises the cancel leg many times while staying near
/// the constant-overhead floor.
const LOW_ITERS: usize = 20;

/// High iteration count for the slope check. Pre-fix this drove ~1 leak/iter of
/// abandoned owned replies; post-fix the count holds flat.
const HIGH_ITERS: usize = 1000;

/// Maximum permitted leak-node delta between the HIGH and LOW cancel-leg probes.
/// Same headroom rationale as the sibling oracles (`nested_payload_leak_oracle`
/// etc.): absorbs one-off runtime allocations while still catching a slope an
/// order of magnitude below the pre-fix ~1 leak/iter.
const SLOPE_TOLERANCE: usize = 8;

/// Iteration count for the never-consumed no-abort probe.
const NO_ABORT_ITERS: usize = 400;

// ── fixtures ──────────────────────────────────────────────────────────────

/// Cancel-leg driver: a slow owned-`string` replier `spawn`ed ONCE, asked with
/// a 1 ms timeout it can never beat (it sleeps 5 ms). Every reply therefore
/// lands on the already-cancelled channel — the `hew_reply` false leg the fix
/// reaps via the registered destructor. The owned arm never wins, so its
/// `reply` is never bound: the only per-iteration heap is the abandoned reply
/// itself, making the post-fix leak slope a clean ≈0.
fn cancelled_owned_reply_source(iters: usize) -> String {
    format!(
        "actor SlowReplier {{\n\
         \x20   receive fn fetch() -> string {{\n\
         \x20       sleep(5ms);\n\
         \x20       \"owned-reply-heap-payload\".to_uppercase()\n\
         \x20   }}\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   let slow = spawn SlowReplier;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {iters} {{\n\
         \x20       let r = select {{\n\
         \x20           reply from slow.fetch() => 1,\n\
         \x20           after 1ms => 0,\n\
         \x20       }};\n\
         \x20       let _ = r;\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Never-consumed driver: two fast owned-`string` repliers raced in a select.
/// The loser's reply is delivered into its channel and then abandoned (the
/// delivered-but-never-consumed teardown leg). Used as a no-double-free probe
/// only — the winner binds its owned `reply`, which the pre-existing consume
/// -side drop gap leaks, so this drives a leak but must never ABORT.
fn never_consumed_owned_reply_source(iters: usize) -> String {
    format!(
        "actor Replier {{\n\
         \x20   let tag: string;\n\
         \x20   receive fn fetch() -> string {{ tag.to_uppercase() }}\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {iters} {{\n\
         \x20       let a = spawn Replier(tag: \"alpha-owned-reply\");\n\
         \x20       let b = spawn Replier(tag: \"beta-owned-reply\");\n\
         \x20       let r = select {{\n\
         \x20           reply from a.fetch() => 10,\n\
         \x20           reply from b.fetch() => 20,\n\
         \x20       }};\n\
         \x20       let _ = r;\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   0\n\
         }}\n"
    )
}

// ── leak measurement plumbing (same shape as nested_payload_leak_oracle) ───

/// Compile `source` to a native binary via `hew compile --emit-dir` and return
/// the binary path.
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

/// Run `bin` under the poisoned-allocator triple + `leaks --atExit` and return
/// `Some(leak_count)` when `leaks` produced a usable report.
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
    let report = String::from_utf8_lossy(&output.stdout);
    if !report.contains(" leaks for ") && !report.contains(" leak for ") {
        eprintln!(
            "skip: leaks did not emit a usable summary for {}: stderr=\n{}",
            bin.display(),
            String::from_utf8_lossy(&output.stderr)
        );
        return None;
    }
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
                        return Some(n);
                    }
                }
            }
        }
    }
    None
}

/// Run `bin` under the full poisoned-allocator triple WITHOUT `leaks`, with
/// periodic heap verification, and assert it exits cleanly. This is the
/// over-eager-free / double-free / use-after-free guard: a reply destructor
/// that released heap the waiter still owns (or ran twice on the same buffer)
/// aborts here. `main` returns a FIXED 0 so a non-zero status is unambiguously
/// a crash, not a payload value escaping as the exit code.
fn assert_no_poisoned_allocator_abort(bin: &std::path::Path, label: &str) {
    let output = Command::new(bin)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .env("MallocCheckHeapEach", "256")
        .output()
        .unwrap_or_else(|e| panic!("run {label} binary under poisoned allocator: {e}"));
    assert!(
        output.status.success(),
        "{label}: aborted under the poisoned-allocator triple — a reply \
         destructor double-freed / used-after-free the owned reply heap on a \
         channel teardown leg (#1739): {}\nstderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
}

fn leaks_available() -> bool {
    cfg!(target_os = "macos")
        && Command::new("which")
            .arg("leaks")
            .output()
            .is_ok_and(|o| o.status.success())
}

// ── oracles ────────────────────────────────────────────────────────────────

/// Cancel-leg reclaim: every owned reply landing on an already-cancelled
/// channel is reaped by the registered destructor, so the leak count is flat
/// across the iteration delta. Pre-fix slope is ~1 leak/iter (20 → 1168 over
/// 20 → 2000 iters); post-fix it holds at ≈0. Also asserts no over-eager free.
#[test]
fn cancelled_owned_reply_no_per_iter_leak_slope() {
    if !leaks_available() {
        eprintln!("skip: ask_reply_owned_leak oracle: leaks(1) is macOS-only / not on PATH");
        return;
    }
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("ask-reply-owned-leak-")
        .tempdir()
        .expect("tempdir");

    let bin_low = compile_to_native(
        &cancelled_owned_reply_source(LOW_ITERS),
        dir.path(),
        "cancelled_owned_low",
    );
    let bin_high = compile_to_native(
        &cancelled_owned_reply_source(HIGH_ITERS),
        dir.path(),
        "cancelled_owned_high",
    );

    // Over-eager-free / double-free / UAF guard first: a crash here would also
    // break the leak probe, so surface the clearer failure mode.
    assert_no_poisoned_allocator_abort(&bin_high, "cancel-leg owned reply");

    let Some(low_leaks) = measure_leaks(&bin_low) else {
        return;
    };
    let Some(high_leaks) = measure_leaks(&bin_high) else {
        return;
    };

    eprintln!(
        "cancelled_owned_reply: low_iters={LOW_ITERS} low_leaks={low_leaks} \
         high_iters={HIGH_ITERS} high_leaks={high_leaks} tolerance={SLOPE_TOLERANCE}"
    );
    assert!(
        high_leaks <= low_leaks + SLOPE_TOLERANCE,
        "cancelled_owned_reply: per-iteration leak SLOPE — low_iters={LOW_ITERS} \
         low_leaks={low_leaks}, high_iters={HIGH_ITERS} high_leaks={high_leaks}. Excess of {} \
         NODES over the tolerance of {SLOPE_TOLERANCE} means an owned ask reply landing on a \
         cancelled channel is not reclaimed by the registered destructor (pre-fix slope ~1 \
         leak/iter). Re-run with `MallocStackLogging=1 leaks --atExit -- {}` for the leaked \
         block's stack.",
        high_leaks.saturating_sub(low_leaks + SLOPE_TOLERANCE),
        bin_high.display()
    );
}

/// Never-consumed (select-loser) teardown leg: a delivered-but-abandoned owned
/// reply must be released without double-freeing what the winner/waiter owns.
/// Pinned as a no-over-eager-free probe (see the module docs for why this leg
/// is not a leak-slope assertion here — the authoritative reclaim proof is the
/// `hew-runtime` reply-destructor counter unit tests).
#[test]
fn never_consumed_owned_reply_no_double_free() {
    if !leaks_available() {
        eprintln!("skip: ask_reply_owned_leak oracle: leaks(1) is macOS-only / not on PATH");
        return;
    }
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("ask-reply-never-consumed-")
        .tempdir()
        .expect("tempdir");

    let bin = compile_to_native(
        &never_consumed_owned_reply_source(NO_ABORT_ITERS),
        dir.path(),
        "never_consumed_owned",
    );

    assert_no_poisoned_allocator_abort(&bin, "never-consumed select-loser owned reply");
}
