//! Branch-around returned-member leak — slope oracles plus poisoned-allocator
//! exactly-once pins.
//!
//! Empirical oracle for the branch-around under-release class the S1
//! obligation-balance census surfaced in `semver::try_parse` (44 MB / 500k
//! error-path calls) and `base64::decode` (30 MB / 500k reject-path calls).
//!
//! ## The defect
//!
//! A value handed to the caller through the return flow — either an aggregate
//! member folded into a returned record (`return Version { pre, build }`) or a
//! whole-value return (`return out`) — is removed from the scope-exit drop plan
//! **path-insensitively**: the aggregate-member handoff derivation strips it
//! from every drop class, and a whole-value return retracts its binding to
//! `ConsumedAt`. That is correct on the return path (the caller owns it now),
//! but a GUARD EARLY-RETURN that exits BEFORE the hand-off still owns the value
//! locally and leaks it — the value reaches a `Return` exit live and
//! undischarged.
//!
//! ## The fix
//!
//! `elaborate` re-admits the member's scope-exit drop on exactly the `Return`
//! exits its transfer site provably cannot reach (CFG reachability from the
//! hand-off block), gated on the dataflow proving it definitely `Live` at that
//! exit. Purely additive: it never removes an existing drop and emits one only
//! where the value is the still-live sole owner, so it leak-fixes without any
//! double-free. Scoped to leaf `CoW` values (`string` / `bytes`).
//!
//! ## Slope methodology
//!
//! Shared harness (`support::leak_slope`): compile the same shape at LOW and
//! HIGH iteration counts, measure leak NODE counts under `leaks --atExit` with
//! the poisoned-allocator triple, assert the delta stays within tolerance. The
//! pre-fix defect is PER-CALL GROWTH on the guard-return path.
//!
//! ## Exactly-once pins
//!
//! The return path must NOT double-free: on the tail return the caller owns the
//! value and the re-admission must skip that exit. The pin binary runs both the
//! guard-return and the tail-return paths (whole-value string/bytes and a
//! returned-record member) to completion under
//! `MallocScribble`/`MallocPreScribble`/`MallocGuardEdges` and must print the
//! exact checksum — a double-free aborts (or scribbles) before the output
//! completes.
//!
//! ## Skip behaviour
//!
//! Slope oracles are macOS-only (`leaks(1)`); elsewhere they log `skip:` and
//! return. The scribble pin runs on any unix host.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

// ── slope fixtures — the guard-return path leaks per call pre-fix ────────────

/// Whole-value `string` return (the `base64::decode` / `out` shape at the leaf
/// level). `s` is a fresh owned string returned on the tail; on the `bail`
/// guard-return it is live and undischarged. Pre-fix: one leaked string per
/// bail call.
fn whole_value_string_guard_return_source(frames: usize) -> String {
    format!(
        "fn build_or_bail(bail: bool, seed: string) -> string {{\n\
         \x20   let payload = seed + \"-owned-heap-payload\";\n\
         \x20   if bail {{\n\
         \x20       return \"bailed\";\n\
         \x20   }}\n\
         \x20   payload\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let r = build_or_bail(true, \"per-call-seed\");\n\
         \x20       total = total + r.len();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

/// Whole-value `bytes` return (the exact `base64::decode` shape). `out`
/// accumulates content, is returned on the tail, and on the reject guard-return
/// is live and undischarged. Pre-fix: one leaked bytes buffer per reject call.
fn whole_value_bytes_guard_return_source(frames: usize) -> String {
    format!(
        "fn decode_or_reject(reject: bool) -> bytes {{\n\
         \x20   let out: bytes = bytes::new();\n\
         \x20   out.push(1);\n\
         \x20   out.push(2);\n\
         \x20   out.push(3);\n\
         \x20   if reject {{\n\
         \x20       return bytes::new();\n\
         \x20   }}\n\
         \x20   out\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let r = decode_or_reject(true);\n\
         \x20       total = total + r.len();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

/// Returned-record member (the `semver::try_parse` / `Version { pre, build }`
/// shape). `pre` and `build` are owned string fields moved into a record
/// returned on the tail; on the guard-return they are live and undischarged.
/// Pre-fix: two leaked strings per bail call.
fn returned_record_member_guard_return_source(frames: usize) -> String {
    format!(
        "type Parsed {{ pre: string; build: string; n: i64; }}\n\
         \n\
         fn parse_or_bail(bail: bool, seed: string) -> Parsed {{\n\
         \x20   let pre = seed + \"-pre\";\n\
         \x20   let build = seed + \"-build\";\n\
         \x20   if bail {{\n\
         \x20       return Parsed {{ pre: \"\", build: \"\", n: 0 }};\n\
         \x20   }}\n\
         \x20   Parsed {{ pre: pre, build: build, n: 1 }}\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let p = parse_or_bail(true, \"per-call-seed\");\n\
         \x20       total = total + p.n;\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

// ── exactly-once pin — both paths, poisoned allocator ────────────────────────

/// Both the guard-return AND the tail-return paths of every shape run to
/// completion under the poisoned allocator. The tail return hands the value to
/// the caller (the re-admission must SKIP that exit); the guard return keeps it
/// local (the re-admission fires). A double-free on the tail path — the
/// re-admitted drop firing where the value escaped — aborts or scribbles before
/// `OK`.
const EXACTLY_ONCE_PIN_SOURCE: &str = "\
fn build_or_bail(bail: bool, seed: string) -> string {\n\
\x20   let payload = seed + \"-owned-heap-payload\";\n\
\x20   if bail {\n\
\x20       return \"bailed\";\n\
\x20   }\n\
\x20   payload\n\
}\n\
\n\
fn decode_or_reject(reject: bool) -> bytes {\n\
\x20   let out: bytes = bytes::new();\n\
\x20   out.push(1);\n\
\x20   out.push(2);\n\
\x20   out.push(3);\n\
\x20   if reject {\n\
\x20       return bytes::new();\n\
\x20   }\n\
\x20   out\n\
}\n\
\n\
type Parsed { pre: string; build: string; n: i64; }\n\
\n\
fn parse_or_bail(bail: bool, seed: string) -> Parsed {\n\
\x20   let pre = seed + \"-pre\";\n\
\x20   let build = seed + \"-build\";\n\
\x20   if bail {\n\
\x20       return Parsed { pre: \"\", build: \"\", n: 0 };\n\
\x20   }\n\
\x20   Parsed { pre: pre, build: build, n: 1 }\n\
}\n\
\n\
fn main() {\n\
\x20   print(build_or_bail(true, \"seed\").len());\n\
\x20   print(build_or_bail(false, \"seed\").len());\n\
\x20   print(decode_or_reject(true).len());\n\
\x20   print(decode_or_reject(false).len());\n\
\x20   print(parse_or_bail(true, \"seed\").n);\n\
\x20   print(parse_or_bail(false, \"seed\").n);\n\
\x20   print(\"OK\");\n\
}\n";

/// The concatenated `print` output of [`EXACTLY_ONCE_PIN_SOURCE`]:
/// `build_or_bail` bail=`"bailed"`.len()=6, tail=`"seed-owned-heap-payload"`.len()=23 ·
/// `decode_or_reject` reject=0, tail=3 · `parse_or_bail` bail n=0, tail n=1 · OK.
const EXACTLY_ONCE_PIN_EXPECTED: &str = "6230301OK";

// ── oracles ──────────────────────────────────────────────────────────────────

/// Whole-value `string` return, guard-return path: no per-call leak.
#[test]
fn whole_value_string_guard_return_no_per_call_leak_slope() {
    assert_frame_slope_below_tolerance(
        "whole_value_string_guard_return",
        whole_value_string_guard_return_source,
    );
}

/// Whole-value `bytes` return (the `base64::decode` shape), reject path: no
/// per-call leak.
#[test]
fn whole_value_bytes_guard_return_no_per_call_leak_slope() {
    assert_frame_slope_below_tolerance(
        "whole_value_bytes_guard_return",
        whole_value_bytes_guard_return_source,
    );
}

/// Returned-record member (the `semver::try_parse` shape), bail path: no
/// per-call leak of the owned string fields.
#[test]
fn returned_record_member_guard_return_no_per_call_leak_slope() {
    assert_frame_slope_below_tolerance(
        "returned_record_member_guard_return",
        returned_record_member_guard_return_source,
    );
}

/// Exactly-once pin: both the guard-return and tail-return paths of every shape
/// run to completion under the poisoned-allocator triple and print the exact
/// checksums. A double-free — the re-admitted drop firing on the tail path
/// where the value escaped to the caller — aborts or scribbles before `OK`.
#[test]
fn branch_around_return_shapes_run_clean_under_malloc_scribble() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("branch-around-return-pin-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(EXACTLY_ONCE_PIN_SOURCE, dir.path(), "exactly_once_pin");

    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "branch-around-return shapes must run clean under the poisoned allocator \
         — a crash indicates the re-admitted scope-exit drop fired on the tail \
         return where the value escaped to the caller (double free);\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        EXACTLY_ONCE_PIN_EXPECTED,
        "branch-around-return shapes must print the exact checksums — a \
         scribbled value indicates a freed buffer was still read;\n{}",
        describe_output(&output)
    );
}
