//! Durable leak oracle for `std/text/semver/semver.hew::matches_single`.
//!
//! `matches_single` parses a constraint's operator prefix, parses the
//! requirement into `req_ver: Version`, and — for the `^` and `~` arms —
//! compares `v.maj == req_ver.maj` / `v.min == req_ver.min` before returning.
//! Those two comparisons are exactly the site a scanner token-ownership leak
//! once misread as a full transfer of `req_ver`, discarding its trailing
//! release on every call through those arms. In the broken base, the
//! `25/24/24/0` release-count shape means each match call leaked the `req_ver`
//! `Version` record, represented by multiple allocator nodes per record. A
//! pre-fix build of this probe's shape would therefore grow its leak count
//! LINEARLY with the frame count rather than staying flat; that is the failure
//! mode this file exists to catch, and it catches it two ways rather than by
//! rebuilding history:
//!
//!   * the exact-zero assertion ([`zero_leaks_and_deterministic_checksum_at_low_and_high`])
//!     rejects any retained allocation at either frame count;
//!   * the canonical slope assertion ([`flat_leak_slope_across_all_operators`])
//!     compares [`LOW_FRAMES`] and [`HIGH_FRAMES`]. A per-iteration leak scales
//!     with the frame delta, while constant allocator baseline noise cancels in
//!     the difference.
//!
//! ## What the probe exercises
//!
//! `run_case` calls `Version::matches` once per recognized operator — `>=`,
//! `<=`, `!=`, `>`, `<`, `^`, `~`, and the explicit `=` — plus once more with a
//! bare, no-prefix constraint that exercises the UNMATCHED-OPERATOR fallthrough
//! (none of `matches_single`'s `c.starts_with(...)` arms fire, so the operator
//! stays defaulted to `=`). Each of the eight recognized operators gets both a
//! true and a false case. The `^`/`~` cases with `cmp >= 0` exercise their
//! `req_ver.maj` / `req_ver.min` projections on both true and false RHS
//! results; the additional `^2.0.0` and `~2.0.0` cases make the left side of
//! `cmp >= 0 && ...` false and verify the short-circuit before those
//! projections. Every call result is weighted by a distinct power of two and
//! summed, giving a WORK CHECKSUM that is wrong the moment any operator
//! resolves the wrong boolean or any parse path is silently skipped — not just
//! a line count. The checksum is a fixed multiple of the frame count
//! (`87381` per iteration: only the even-numbered weight exponents contribute),
//! both frame counts pin an exact expected value. The runtime probe exercises
//! the normal-return plan.
//!
//! Fails closed exactly as the shared harness does: a non-macOS host records a
//! compile-time `#[ignore]` skip (never a silent pass), and a macOS host with
//! no working `leaks(1)` / poisoned allocator panics rather than reporting
//! success — see `support::leak_slope`'s module docs.

#![cfg(unix)]

mod support;

use std::fmt::Write as _;

use support::leak_slope::{
    assert_frame_slope_below_tolerance_exact_lines, compile_to_native, measure_leaks_exact,
    require_leaks_tool, HIGH_FRAMES, LOW_FRAMES,
};
use support::{describe_output, require_codegen};

const PROBE_TEMPLATE: &str = r#"
import std.text.semver;

fn weighted(matched: bool, weight: i64) -> i64 {
    if matched { weight } else { 0 }
}

/// One call per recognized operator (true AND false), plus the no-prefix
/// fallthrough, weighted by a distinct power of two so the sum is a checksum
/// rather than a count: `87381` per call when every true/false case resolves
/// as expected.
fn run_case() -> i64 {
    let v = semver.parse("1.2.3");
    var sum = 0;
    sum = sum + weighted(v.matches(">=1.2.3"), 1);
    sum = sum + weighted(v.matches(">=1.3.0"), 2);
    sum = sum + weighted(v.matches("<=1.2.3"), 4);
    sum = sum + weighted(v.matches("<=1.2.0"), 8);
    sum = sum + weighted(v.matches("!=1.2.4"), 16);
    sum = sum + weighted(v.matches("!=1.2.3"), 32);
    sum = sum + weighted(v.matches(">1.2.2"), 64);
    sum = sum + weighted(v.matches(">1.2.3"), 128);
    sum = sum + weighted(v.matches("<1.2.4"), 256);
    sum = sum + weighted(v.matches("<1.2.3"), 512);
    sum = sum + weighted(v.matches("^1.2.0"), 1024);
    sum = sum + weighted(v.matches("^0.9.0"), 2048);
    sum = sum + weighted(v.matches("^2.0.0"), 262144);
    sum = sum + weighted(v.matches("~1.2.0"), 4096);
    sum = sum + weighted(v.matches("~1.1.5"), 8192);
    sum = sum + weighted(v.matches("~2.0.0"), 524288);
    sum = sum + weighted(v.matches("=1.2.3"), 16384);
    sum = sum + weighted(v.matches("=1.2.4"), 32768);
    sum = sum + weighted(v.matches("1.2.3"), 65536);
    sum = sum + weighted(v.matches("1.2.4"), 131072);
    sum
}

fn main() -> i64 {
    var total = 0;
    for frame in 0..__FRAMES__ {
        total = total + run_case();
        println("frame");
    }
    println(f"checksum={total}");
    0
}
"#;

/// Fixed per-iteration checksum: only the nine `true`-resolving calls
/// contribute (weights `1, 4, 16, 64, 256, 1024, 4096, 16384, 65536`), so the
/// total at `frames` iterations is `CHECKSUM_PER_FRAME * frames` exactly.
const CHECKSUM_PER_FRAME: i64 = 87_381;

fn probe_source(frames: usize) -> String {
    PROBE_TEMPLATE.replace("__FRAMES__", &frames.to_string())
}

fn expected_lines(frames: usize) -> usize {
    // One "frame" line per iteration plus the trailing checksum line.
    frames + 1
}

fn expected_stdout(frames: usize) -> String {
    let mut out = "frame\n".repeat(frames);
    let checksum = CHECKSUM_PER_FRAME * i64::try_from(frames).expect("frame count fits in i64");
    let _ = writeln!(out, "checksum={checksum}");
    out
}

/// Canonical flat-slope pin: [`LOW_FRAMES`] vs [`HIGH_FRAMES`], exact work
/// witness (the printed line count is a deterministic function of `frames`),
/// leak-node delta within [`support::leak_slope::SLOPE_TOLERANCE`]. A
/// per-iteration `req_ver` leak in the `^`/`~` arms would scale with the frame
/// delta here, while constant allocator baseline noise cancels in the
/// difference.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn flat_leak_slope_across_all_operators() {
    assert_frame_slope_below_tolerance_exact_lines(
        "semver_matches_single_all_operators",
        probe_source,
        expected_lines,
    );
}

/// Exact-zero pin at both [`LOW_FRAMES`] and [`HIGH_FRAMES`], each gated on the
/// probe's own exact stdout (the deterministic work checksum) so a leak count
/// of zero is never trusted from a probe that ran fewer cases than it should
/// have. This assertion rejects any retained allocation directly; the paired
/// slope assertion is what isolates frame-dependent leakage because baseline
/// allocator noise cancels in its difference.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn zero_leaks_and_deterministic_checksum_at_low_and_high() {
    require_leaks_tool();
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("semver-matches-leak-oracle-")
        .tempdir()
        .expect("tempdir");

    for &frames in &[LOW_FRAMES, HIGH_FRAMES] {
        let bin = compile_to_native(&probe_source(frames), dir.path(), &format!("p{frames}"));

        let output = std::process::Command::new(&bin)
            .output()
            .unwrap_or_else(|e| panic!("run probe at {frames} frames: {e}"));
        assert!(
            output.status.success(),
            "probe at {frames} frames must run cleanly:\n{}",
            describe_output(&output)
        );
        assert_eq!(
            String::from_utf8_lossy(&output.stdout),
            expected_stdout(frames),
            "probe at {frames} frames must print exactly one `frame` line per iteration \
             plus the deterministic work checksum — a mismatch means the loop under test \
             did not run every case, so a zero-leak reading below would not be trustworthy"
        );

        let (count, bytes) = measure_leaks_exact(&bin);
        assert_eq!(
            count,
            0,
            "leaks(1) reported {count} leak(s) at {frames} frames — `matches_single`'s \
             `req_ver` (or another co-resident local) is not being released on every exit \
             path. Re-run with `MallocStackLogging=1 leaks --atExit -- {}`.",
            bin.display()
        );
        assert_eq!(
            bytes, 0,
            "expected 0 leaked bytes at {frames} frames, got {bytes}"
        );
    }
    eprintln!(
        "matches_single all-operator oracle: 0 leaks for 0 total leaked bytes at both \
         {LOW_FRAMES} and {HIGH_FRAMES} frames, checksum verified at both — PASS"
    );
}
