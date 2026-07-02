//! Fungible-tell recover-edge payload oracle (#2126) — the empirical
//! (compiled-binary) half of the undelivered-payload release contract in
//! `lower_actor_send` / `emit_undelivered_send_payload_release`.
//!
//! ## The shape
//!
//! A fire-and-forget tell through a FUNGIBLE supervisor-child reference
//! re-resolves the current child at the send site and branches on slot
//! liveness. On a not-live slot (mid-restart, or the supervisor stopped) the
//! send fail-closes as a recoverable no-op — the message is dropped. The
//! payload value, however, was already evaluated in the pre-branch block
//! (argument effects are user-visible and must not depend on liveness), and
//! the skipped `Send` was its one consumer: without the recover-edge release
//! every not-live heap-payload tell leaked its payload at exactly one
//! allocation per send (#2126: slope 1, 64 B `to_upper` buffers here).
//!
//! The fix releases each undelivered payload value on the recover edge —
//! `drop <payload> fn=release(hew_string_drop)` before the recover `Goto` —
//! standing in for the `Send` that never ran. The delivered edge is
//! untouched: the release and the `Send` live on exclusive branch arms, so
//! the payload's ownership is discharged exactly once either way.
//!
//! ## The safety boundary
//!
//! The recover-edge release must NEVER fire on the delivered path — the
//! mailbox owns the payload bytes after the `Send`, and the receiving
//! handler's scope-exit drop releases them. The scribble pin runs the same
//! tell shape against a LIVE child under the poisoned allocator and asserts
//! the exact sentinel: a release that also ran on the delivery edge is a
//! double-free abort / poisoned read.
//!
//! ## De-flake: slope, not single-shot exact-zero
//!
//! The slope leg compiles the not-live loop at LOW and HIGH iteration counts
//! and the leak-NODE delta must stay within tolerance
//! (`support::leak_slope`); a regressed release leaks one node per send — an
//! order of magnitude above the tolerance. macOS-only for the slope leg
//! (`leaks(1)`); the scribble pin runs on any unix host.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

// ── looped slope fixture ────────────────────────────────────────────────

/// The #2126 repro: the supervisor is stopped, so every tell through the
/// fungible child reference takes the not-live recover edge with a fresh
/// heap payload in hand. Pre-fix slope: exactly 1 leak/send (the payload
/// buffer).
fn fungible_tell_recover_loop_source(frames: usize) -> String {
    format!(
        "actor Worker {{\n\
         \x20   var seen: i64;\n\
         \x20   receive fn take(s: string) {{\n\
         \x20       if s.is_empty() {{ seen = seen; }} else {{ seen = seen + 1; }}\n\
         \x20   }}\n\
         }}\n\
         \n\
         supervisor App {{\n\
         \x20   strategy: one_for_one;\n\
         \x20   intensity: 3 within 60s;\n\
         \n\
         \x20   child w: Worker(seen: 0);\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let sup = spawn App;\n\
         \x20   let w = sup.w;\n\
         \x20   supervisor_stop(sup);\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       w.take(\"undelivered-heap-payload\".to_upper());\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   0\n\
         }}\n"
    )
}

// ── scribble fixture (single-cycle, exact stdout) ───────────────────────

/// The SAME tell shape against a LIVE child: the payloads are delivered and
/// the child counts them. The recover-edge release must not run here — a
/// release on the delivery edge frees the bytes the mailbox (and then the
/// handler) still owns.
const DELIVERED_SCRIBBLE_SOURCE: &str = "\
actor Worker {\n\
\x20   var seen: i64;\n\
\x20   receive fn take(s: string) {\n\
\x20       if s.is_empty() { seen = seen; } else { seen = seen + 1; }\n\
\x20   }\n\
\x20   receive fn count() -> i64 {\n\
\x20       seen\n\
\x20   }\n\
}\n\
\n\
supervisor App {\n\
\x20   strategy: one_for_one;\n\
\x20   intensity: 3 within 60s;\n\
\n\
\x20   child w: Worker(seen: 0);\n\
}\n\
\n\
fn main() -> i64 {\n\
\x20   let sup = spawn App;\n\
\x20   let w = sup.w;\n\
\x20   var i: i64 = 0;\n\
\x20   while i < 5 {\n\
\x20       w.take(\"delivered-heap-payload\".to_upper());\n\
\x20       i = i + 1;\n\
\x20   }\n\
\x20   let n = match await w.count() {\n\
\x20       Ok(v) => v,\n\
\x20       Err(_) => -1,\n\
\x20   };\n\
\x20   if n == 5 {\n\
\x20       print(\"k\");\n\
\x20   }\n\
\x20   0\n\
}\n";

// ── slope oracle ────────────────────────────────────────────────────────

/// The not-live tell loop holds a flat leak slope: each undelivered payload
/// is released on the recover edge. A regressed release leaks one payload
/// buffer per send and trips the tolerance.
#[test]
fn fungible_tell_recover_payload_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("fungible_tell_recover", fungible_tell_recover_loop_source);
}

// ── scribble (double-free / use-after-free) pin ─────────────────────────

/// Delivered-path control: the live-child tell delivers every payload (the
/// child's count is the sentinel) and runs clean under the poisoned
/// allocator — the recover-edge release must not touch the delivery edge.
#[test]
fn fungible_tell_delivered_payload_clean_under_malloc_scribble() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("fungible-tell-delivered-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(DELIVERED_SCRIBBLE_SOURCE, dir.path(), "delivered_control");
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "the delivered-path tell must run clean under the poisoned allocator — a crash \
         here means the recover-edge release fired on the delivery edge (double-free \
         against the mailbox-owned payload);\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        "k",
        "sentinel mismatch — the live child did not receive all five payloads;\n{}",
        describe_output(&output)
    );
}
