//! Wire CBOR codec ownership leak oracles: decode-owned field drop and the
//! decode-failure partial free-on-error path.
//!
//! The CBOR body codec emits two halves per `#[wire]` type:
//!
//!   * `__hew_cbor_serialize_<key>` borrows the value and writes a CBOR map
//!     into a fresh buffer (encode side — no ownership transfer).
//!   * `__hew_cbor_deserialize_<key>` mallocs a zero-initialised struct shell,
//!     decodes field-by-field allocating owned fields (string / `Vec<T>` /
//!     nested struct), and returns the owned shell. On a malformed/short/
//!     type-mismatched stream the ciborium reader LATCHES failure and the
//!     thunk's `fail_bb` drops every owned field already written — before
//!     `free(dst)`, in place, so the shell is freed exactly once — then
//!     returns null, which traps the `.decode()` call site (fail-closed).
//!
//! These oracles pin both halves, for both record and enum `#[wire]` shapes:
//!
//!   * **Decode-owned field drop** (`owned_field_round_trip_no_per_frame_leak_slope`):
//!     a loop that round-trips a struct carrying an owned `string`, an owned
//!     `Vec<string>` (owned elements), and an owned nested `#[wire]` struct
//!     (owned nested string) must hold a flat leak count across frames. A
//!     missing drop in the decoded-value glue (the Vec element drop, the
//!     nested record drop, or the top-level string drop) shows up as a slope
//!     of >= 1 leak/frame — an order of magnitude above the +5 tolerance over
//!     the `50 - 3 = 47`-frame delta.
//!
//!   * **Owned-payload ENUM round-trip drop**
//!     (`enum_owned_payload_round_trip_no_per_frame_leak_slope`): the same flat
//!     slope for a `#[wire]` enum whose payload variant carries an owned
//!     `string` + owned `Vec<string>` + owned nested `#[wire]` struct. Enum
//!     cleanup delegates to `get_or_declare_enum_drop_inplace` (a different
//!     helper from the record drop), so it is pinned separately.
//!
//!   * **Decode-failure partial free-on-error**
//!     (`decode_failure_frees_partials_no_double_free` and its enum sibling
//!     `enum_owned_payload_decode_failure_frees_partials_no_double_free`): a
//!     cross-decode where a value is decoded against a layout that allocates an
//!     owned field and then latches failure on a later field, driving the
//!     `fail_bb` partial-drop path. Run under the poisoned-allocator triple:
//!     the program must trap (fail-closed) WITHOUT the `hew-cabi: free_cstring:
//!     ... double-free` abort — proving the partial owned field is freed exactly
//!     once (no double-free) and that the shell free does not race the field
//!     drop.
//!
//!   * **Out-of-range enum-tag decode free (actor context)**
//!     (`actor_oob_enum_tag_decode_frees_reader_and_shell_no_excess_slope`): an
//!     unknown enum wire tag traps fail-closed; inside an actor `receive`
//!     handler the supervisor's crash-recovery `siglongjmp` keeps the process
//!     alive, so a leak there ACCUMULATES (the actor remote-send dispatch
//!     hazard). A DIFFERENTIAL slope (OOB vs an in-range baseline that shares
//!     the spawn-per-frame structure) cancels the codec-independent per-spawn
//!     actor-cell leak; the remainder is the codec's own per-message leak, which
//!     must be ~0. Pre-fix the unknown-tag path took an inline trap that unwound
//!     past the thunk's reader + shell free (~5 nodes/frame).
//!
//! ## Isolating the codec from two pre-existing, out-of-scope drop confounds
//!
//! Both fixtures are written to measure ONLY the codec's own drop glue. Two
//! unrelated, pre-existing leak/suppression behaviours would otherwise mask or
//! forge a codec result:
//!
//!   1. **Unnamed `bytes` temporary scope-exit drop.** A `bytes` value produced
//!      by a call and consumed as an UNNAMED temporary (e.g.
//!      `T.decode(v.encode())` or the unrelated `"x".to_bytes().len()`) is not
//!      released at the end of its statement — one leaked buffer per frame.
//!      This reproduces on a clean base build with `.to_bytes()` (no codec on
//!      the path), so it is a general MIR temporary-drop derivation gap, NOT a
//!      codec defect: the existing `derive_local_bytes_drop_allowed` admission
//!      in `hew-mir/src/lower.rs` covers NAMED bytes locals (see
//!      `bytes_drop_leak_oracle::bytes_local_loop_no_per_frame_leak_slope`) but
//!      not anonymous temporaries. The round-trip fixture therefore BINDS the
//!      encoded bytes to a named local (`let raw = p.encode();`) so the buffer
//!      drops on the loop back-edge and the slope reflects only the decoded
//!      value's drop.
//!
//!   2. **Managed-aggregate field-read drop suppression** (the `string_field_load`
//!      confound). Reading an owned field OUT of an owned aggregate suppresses
//!      that aggregate's own in-place field drop (documented out-of-scope in
//!      `string_field_load_leak_oracle`). The fixture therefore reads ONLY a
//!      scalar keep-alive field (`back.seq`, an `i64`) to hold the decoded
//!      value live to scope exit without tripping the suppression — the owned
//!      string / Vec / nested fields are never read, so their drop is driven
//!      purely by the synthesised value drop at scope exit.
//!
//! ## Error-path under-free disposition
//!
//! `.decode()` traps (SIGTRAP/SIGILL) on a malformed stream — it does not
//! return a `Result` — so `leaks --atExit` (which needs a normal exit to run
//! its hook) cannot snapshot the failing decode for an UNDER-free slope. The
//! error-path under-free guarantee is instead established structurally: the
//! `fail_bb` calls `emit_de_drop_owned(dst)` over the SAME null-safe drop
//! helpers (`hew_string_drop`, `hew_vec_free`, `__hew_record_drop_inplace_*`)
//! that the success-path slope proves leak-free, walking every field of the
//! zero-initialised shell (unwritten fields are null and short-circuit) before
//! `free(dst)`. The no-DOUBLE-free half is proven dynamically here under the
//! poisoned-allocator triple.
//!
//! ## Slope methodology + skip behaviour
//!
//! Mirrors `bytes_drop_leak_oracle.rs`: compile the same shape at LOW and HIGH
//! frame counts, measure leak NODE counts under `leaks --atExit` with the
//! poisoned-allocator triple, and assert the delta stays within a small
//! constant. macOS-only (`leaks(1)` is Darwin's allocator inspector); other
//! platforms log `skip:` and return.

#![cfg(unix)]

mod support;

use std::os::unix::process::ExitStatusExt;
use std::path::PathBuf;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

/// Low frame count: exercises the loop back-edge drop at least twice while
/// staying near the constant-overhead floor.
const LOW_FRAMES: usize = 3;

/// High frame count for the slope check. A slope of 1.0 leak/frame (a dropped
/// owned field) produces `HIGH_FRAMES - LOW_FRAMES = 47` excess nodes against
/// the tolerance of 5.
const HIGH_FRAMES: usize = 50;

/// Maximum permitted leak-node delta between the HIGH and LOW probes. Same
/// headroom rationale as `bytes_drop_leak_oracle.rs`: absorbs one-off
/// runtime allocations that appear only in the HIGH run while still catching a
/// slope of ~0.1 leaks/frame.
const SLOPE_TOLERANCE: usize = 5;

// ── fixtures ──────────────────────────────────────────────────────────────

/// Round-trip fixture: build a struct carrying an owned `string`, an owned
/// `Vec<string>` (with owned elements), an owned nested `#[wire]` struct (with
/// an owned nested string), plus a scalar `seq` field. Encode it to a NAMED
/// bytes local (so the encoded buffer drops on the back-edge — see confound 1),
/// decode it back, and read ONLY the scalar `back.seq` keep-alive (so the
/// decoded value is held live to scope exit without the field-read drop
/// suppression — see confound 2). The decoded value's owned string / Vec /
/// nested fields must be released by the synthesised value drop on every frame;
/// a missing drop is a >= 1 leak/frame slope.
fn round_trip_source(frames: usize) -> String {
    format!(
        "#[wire]\n\
         type Inner {{ name: string @1; }}\n\
         #[wire]\n\
         type Packet {{ label: string @1; tags: Vec<string> @2; inner: Inner @3; seq: i64 @4; }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let tags: Vec<string> = Vec::new();\n\
         \x20       tags.push(\"alpha-element\");\n\
         \x20       tags.push(\"beta-element\");\n\
         \x20       let p = Packet {{ label: \"payload-label-value\", tags: tags, inner: Inner {{ name: \"inner-owned-name\" }}, seq: i }};\n\
         \x20       let raw = p.encode();\n\
         \x20       let back = Packet.decode(raw);\n\
         \x20       total = total + back.seq;\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

/// Round-trip fixture for a `Vec<#[wire] struct>` field whose ELEMENT is a
/// heap-free record — a list of wire messages, the core distributed batch
/// pattern (events / peers / log entries). `Vec::new` builds this as a
/// layout-aware `BitCopy` vec (`hew_vec_new_with_layout`, `ownership_kind=Plain`),
/// so the codec round-trips each slot through `hew_vec_get_layout` /
/// `hew_vec_push_layout`. The decoded vec is freed by `hew_vec_free` (a Plain
/// layout-aware vec owns no per-element heap), so the leak slope must stay flat.
/// A vec freed under the wrong ABI, or an element double-decoded into an
/// over-grown buffer, shows up as a per-frame slope. (Measured post-fix: 0 leaks
/// at both LOW and HIGH.)
///
/// The decoded value is held live by reading the SCALAR sibling field `seq`
/// (not the `items` Vec) — reading the Vec field itself (`.len()` / `[i]`)
/// triggers the pre-existing field-read drop-suppression confound that the
/// existing `round_trip` oracle dodges the same way (it reads `back.seq`, never
/// `back.tags`). The `raw` encode buffer is a NAMED local so the anonymous-bytes
/// temporary confound (confound 1) does not add its own per-frame node.
fn vec_struct_round_trip_source(frames: usize) -> String {
    format!(
        "#[wire]\n\
         type Inner {{ v: i64 @1; }}\n\
         #[wire]\n\
         type Container {{ items: Vec<Inner> @1; seq: i64 @2; }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let xs: Vec<Inner> = Vec::new();\n\
         \x20       xs.push(Inner {{ v: 10 }});\n\
         \x20       xs.push(Inner {{ v: 20 }});\n\
         \x20       xs.push(Inner {{ v: 12 }});\n\
         \x20       let c = Container {{ items: xs, seq: i }};\n\
         \x20       let raw = c.encode();\n\
         \x20       let back = Container.decode(raw);\n\
         \x20       total = total + back.seq;\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

/// Decode-failure fixture: encode a two-`string` value, then decode the SAME
/// bytes against a `{string @1, i64 @2}` layout. Field 1 (`string`) decodes
/// and allocates a C string; field 2 reads an `i64` where the stream holds a
/// CBOR text head, latching reader failure and driving the `fail_bb` partial
/// free-on-error path (drop the field-1 string, then free the shell, then
/// return null → trap). The trailing `p.b` is unreachable (the decode traps
/// first) but keeps the decoded binding live so the codec path is fully
/// emitted.
const DECODE_FAILURE_SOURCE: &str = "#[wire]\n\
     type TwoStr { a: string @1; b: string @2; }\n\
     #[wire]\n\
     type Pair { a: string @1; b: i64 @2; }\n\
     \n\
     fn main() -> i64 {\n\
     \x20   let t = TwoStr { a: \"owned-field-a-value\", b: \"owned-field-b-value\" };\n\
     \x20   let raw = t.encode();\n\
     \x20   let p = Pair.decode(raw);\n\
     \x20   p.b\n\
     }\n";

/// Round-trip fixture for a `#[wire]` enum whose payload variant carries an
/// owned `string`, an owned `Vec<string>` (owned elements), and an owned nested
/// `#[wire]` struct (owned nested string). Enum payload cleanup delegates to
/// `get_or_declare_enum_drop_inplace` — a DIFFERENT helper from the record drop
/// the struct round-trip oracle pins — so this shape is pinned separately.
///
/// The decoded value is held live to scope exit by a wildcard `match` (which
/// neither moves an owned field out — avoiding the field-read drop suppression
/// confound — nor reads a scalar that does not exist on an enum). The owned
/// payload must be released by the enum value drop on every frame; a missing
/// variant-field drop is a >= 1 leak/frame slope. The encoded buffer is bound
/// to a NAMED local (`raw`) so the anonymous-bytes-temporary confound does not
/// add its own per-frame node (see confound 1).
fn enum_owned_payload_round_trip_source(frames: usize) -> String {
    format!(
        "#[wire]\n\
         type Inner {{ name: string @1; }}\n\
         #[wire]\n\
         enum Payload {{ Empty; Full(string, Vec<string>, Inner); }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let tags: Vec<string> = Vec::new();\n\
         \x20       tags.push(\"alpha-element\");\n\
         \x20       tags.push(\"beta-element\");\n\
         \x20       let p = Payload::Full(\"payload-label-value\", tags, Inner {{ name: \"inner-owned-name\" }});\n\
         \x20       let raw = p.encode();\n\
         \x20       let back = Payload.decode(raw);\n\
         \x20       let n = match back {{ Payload::Empty => 0, Payload::Full(_, _, _) => 1, }};\n\
         \x20       total = total + n;\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

/// Decode-failure fixture for an owned-payload enum: encode `Full(string,
/// Vec<string>, Inner)` then decode the SAME bytes against a variant layout
/// whose second field is an `i64`. Field 0 (`string`) decodes and ALLOCATES;
/// field 1 reads an `i64` where the stream holds a CBOR array, latching reader
/// failure and driving the enum-variant `fail_bb` partial free-on-error path
/// (drop the already-decoded owned string, then free the shell, then return
/// null → trap). Proves the partial owned field is freed exactly once.
const ENUM_DECODE_FAILURE_SOURCE: &str = "#[wire]\n\
     type Inner { name: string @1; }\n\
     #[wire]\n\
     enum PayloadGood { Empty; Full(string, Vec<string>, Inner); }\n\
     #[wire]\n\
     enum PayloadBad { Empty; Full(string, i64, Inner); }\n\
     \n\
     fn main() -> i64 {\n\
     \x20   let tags: Vec<string> = Vec::new();\n\
     \x20   tags.push(\"alpha-element\");\n\
     \x20   tags.push(\"beta-element\");\n\
     \x20   let p = PayloadGood::Full(\"owned-field-a-value\", tags, Inner { name: \"inner-owned-name\" });\n\
     \x20   let raw = p.encode();\n\
     \x20   let bad = PayloadBad.decode(raw);\n\
     \x20   match bad { PayloadBad::Empty => 0, PayloadBad::Full(_, n, _) => n, }\n\
     }\n";

/// Actor-context out-of-range enum-tag fixture, parameterised by the message
/// the spawned actor decodes. A `Decoder` actor decodes `raw` bytes against the
/// two-variant `Narrow` enum inside its `receive` handler; `main` spawns one
/// actor per frame, sends the bytes via an ask, and recovers the per-actor
/// result (`Ok`/`Err`).
///
/// With `message_expr = "Wide::C(...)"` the bytes carry wire tag 2 — out of
/// range for `Narrow` (tags 0, 1) — so the decode hits the unknown-tag path,
/// traps fail-closed, the actor crashes, and the supervisor's crash-recovery
/// `siglongjmp` keeps the PROCESS alive (the ask resolves `Err`). With
/// `message_expr = "Narrow::B(1)"` the in-range decode succeeds and the actor
/// returns normally — the codec-independent per-spawn baseline.
///
/// Both shapes spawn one `Decoder` per frame, so both carry the SAME
/// pre-existing per-spawn actor-cell leak (~1 node/frame; see
/// `ask_reply_owned_leak_oracle`). The OOB decode must add NO leak beyond that
/// baseline — that is the property the differential test below pins.
fn actor_enum_decode_source(frames: usize, message_expr: &str) -> String {
    format!(
        "#[wire]\n\
         enum Wide {{ A; B(i64); C(string); }}\n\
         #[wire]\n\
         enum Narrow {{ A; B(i64); }}\n\
         \n\
         actor Decoder {{\n\
         \x20   let seq: i64;\n\
         \x20   receive fn decode_it(raw: bytes) -> i64 {{\n\
         \x20       let back = Narrow.decode(raw);\n\
         \x20       match back {{ Narrow::A => 0, Narrow::B(n) => n, }}\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let w = {message_expr};\n\
         \x20       let raw = w.encode();\n\
         \x20       let a = spawn Decoder(seq: i);\n\
         \x20       let r = await a.decode_it(raw);\n\
         \x20       match r {{ Ok(v) => {{ total = total + v; }}, Err(_) => {{ total = total + 1; }}, }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

/// Out-of-range branch: the actor decodes a `Wide::C` payload (wire tag 2) as a
/// `Narrow` (no tag 2) — the unknown-tag path under test.
fn actor_oob_enum_decode_source(frames: usize) -> String {
    actor_enum_decode_source(frames, "Wide::C(\"oob-enum-owned-payload-string\")")
}

/// In-range baseline: the same actor/spawn shape decoding a well-formed
/// `Narrow::B(1)` — isolates the codec-independent per-spawn actor-cell leak.
fn actor_in_range_enum_decode_source(frames: usize) -> String {
    actor_enum_decode_source(frames, "Narrow::B(1)")
}

// ── leak measurement plumbing (same shape as bytes_drop_leak_oracle) ────────

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
/// `Some(leak_count)` when `leaks` produced a usable report. Parses the
/// canonical `Process <pid>: N leak(s) for B total leaked bytes.` summary.
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

/// True when this host can run the `leaks(1)` oracle.
fn leaks_available(shape_name: &str) -> bool {
    if !cfg!(target_os = "macos") {
        eprintln!("skip: {shape_name}: leaks(1) is macOS-only");
        return false;
    }
    let on_path = Command::new("which")
        .arg("leaks")
        .output()
        .is_ok_and(|o| o.status.success());
    if !on_path {
        eprintln!("skip: {shape_name}: `leaks` binary not on PATH");
    }
    on_path
}

/// Compile `source_fn` at `low_frames` and `high_frames` and return the leak
/// NODE counts `(low_leaks, high_leaks)` measured under `leaks --atExit` + the
/// poisoned-allocator triple. Returns `None` when the host cannot run the
/// `leaks(1)` oracle (logs `skip:` and the caller returns without failing).
fn frame_leak_counts(
    shape_name: &str,
    source_fn: fn(usize) -> String,
    low_frames: usize,
    high_frames: usize,
) -> Option<(usize, usize)> {
    if !leaks_available(shape_name) {
        return None;
    }

    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("wire-cbor-leak-{shape_name}-"))
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

    let low_leaks = measure_leaks(&bin_low)?;
    let high_leaks = measure_leaks(&bin_high)?;

    eprintln!(
        "{shape_name}: low_frames={low_frames} low_leaks={low_leaks} \
         high_frames={high_frames} high_leaks={high_leaks} tolerance={SLOPE_TOLERANCE}"
    );
    Some((low_leaks, high_leaks))
}

/// Build the shape at `low_frames` and `high_frames`, measure leak NODE counts,
/// and assert the delta stays within `SLOPE_TOLERANCE`.
fn assert_frame_slope_below_tolerance(
    shape_name: &str,
    source_fn: fn(usize) -> String,
    low_frames: usize,
    high_frames: usize,
) {
    let Some((low_leaks, high_leaks)) =
        frame_leak_counts(shape_name, source_fn, low_frames, high_frames)
    else {
        return;
    };
    assert!(
        high_leaks <= low_leaks + SLOPE_TOLERANCE,
        "{shape_name}: per-frame leak SLOPE — low_frames={low_frames} low_leaks={low_leaks}, \
         high_frames={high_frames} high_leaks={high_leaks}. Excess of {} NODES over the \
         tolerance of {SLOPE_TOLERANCE} indicates a decoded owned field (string / Vec element / \
         nested record) is not released by the value drop. Re-run a standalone build of this \
         shape under `MallocStackLogging=1 leaks --atExit -- <bin>` to see the leaked block's \
         allocation stack.",
        high_leaks.saturating_sub(low_leaks + SLOPE_TOLERANCE),
    );
}

// ── oracles ───────────────────────────────────────────────────────────────

/// Decode-owned field drop: round-tripping a struct with an owned `string`,
/// owned `Vec<string>`, and owned nested `#[wire]` struct holds a flat leak
/// count across frames. Dropping the Vec element drop, the nested record drop,
/// or the top-level string drop from the decoded-value glue fails this by ~47
/// nodes.
#[test]
fn owned_field_round_trip_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "wire_cbor_round_trip",
        round_trip_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}

/// Layout-aware `Vec<#[wire] struct>` round-trip drop: a field holding a list of
/// heap-free wire records round-trips with a flat leak slope. The decoded vec is
/// a layout-aware `BitCopy` vec (matching `Vec::new`'s construction), freed by the
/// record value drop via `hew_vec_free`. A wrong-ABI free or a per-element decode
/// over-allocation would show up as a per-frame slope.
#[test]
fn vec_struct_round_trip_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "wire_cbor_vec_struct_round_trip",
        vec_struct_round_trip_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}

/// Compile `source` (a decode-failure shape that traps fail-closed) and run it
/// under the poisoned-allocator triple, asserting it (a) traps (SIGILL=4 /
/// SIGTRAP=5) rather than exiting 0 with a silent partial, and (b) does NOT
/// fire the cabi `free_cstring` / `double-free` guard — proving every partial
/// owned field decoded before the failure is freed exactly once on the error
/// path. Skips on non-macOS (the poisoned-allocator triple is a Darwin tool).
fn assert_decode_failure_traps_no_double_free(shape_name: &str, source: &str) {
    if !cfg!(target_os = "macos") {
        eprintln!(
            "skip: {shape_name}: poisoned-allocator triple is exercised via macOS leaks tooling"
        );
        return;
    }
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("wire-cbor-{shape_name}-"))
        .tempdir()
        .expect("tempdir");

    let bin = compile_to_native(source, dir.path(), shape_name);

    let output = Command::new(&bin)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .output()
        .expect("run decode-failure binary");

    let stderr = String::from_utf8_lossy(&output.stderr);
    eprintln!(
        "{shape_name}: status={:?} signal={:?} stderr={}",
        output.status.code(),
        output.status.signal(),
        stderr.trim()
    );

    // Fail-closed: the malformed decode must trap (SIGILL=4 / SIGTRAP=5), never
    // exit 0 with a silent partial.
    let trapped = matches!(output.status.signal(), Some(4 | 5));
    assert!(
        trapped,
        "{shape_name}: expected a fail-closed trap (SIGILL/SIGTRAP) on the malformed decode, \
         got status={:?} signal={:?}\nstderr:\n{stderr}",
        output.status.code(),
        output.status.signal()
    );

    // No double-free: the cabi `free_cstring` guard aborts with this exact
    // sentinel on a double-free / corrupted header. Its ABSENCE proves the
    // partial owned field was freed exactly once on the error path.
    assert!(
        !stderr.contains("free_cstring") && !stderr.contains("double-free"),
        "{shape_name}: the cabi double-free guard fired on the decode-failure path — the partial \
         owned field was freed more than once (or the shell free raced the field drop):\n{stderr}"
    );
}

/// Decode-failure partial free-on-error: a cross-decode that allocates field 1
/// (string) then latches failure on field 2 (int-where-text) must trap
/// (fail-closed) WITHOUT the cabi `free_cstring` double-free abort under the
/// poisoned-allocator triple. Proves the `fail_bb` drops the partial owned
/// field exactly once before freeing the shell — no double-free, no shell/field
/// free race. (The under-free half is structural — the `fail_bb` walks every
/// field via the same null-safe drop helpers the slope oracle proves leak-free
/// — because the trap precludes a `leaks --atExit` snapshot.)
#[test]
fn decode_failure_frees_partials_no_double_free() {
    assert_decode_failure_traps_no_double_free("wire_cbor_decode_failure", DECODE_FAILURE_SOURCE);
}

/// Item 2 — owned-payload enum round-trip drop. A `#[wire]` enum variant
/// carrying an owned `string`, an owned `Vec<string>`, and an owned nested
/// `#[wire]` struct round-trips with a FLAT leak slope: the enum value drop
/// (via `get_or_declare_enum_drop_inplace`) releases every owned variant field
/// on each frame. A missing variant-field drop is a ~47-node excess at the
/// `50 - 3` frame delta. (Measured post-fix: 0 leaks at both LOW and HIGH.)
#[test]
fn enum_owned_payload_round_trip_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "wire_cbor_enum_owned_round_trip",
        enum_owned_payload_round_trip_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}

/// Item 2 — owned-payload enum decode-failure free-on-error. Decoding a
/// `Full(string, Vec<string>, Inner)` payload against a `Full(string, i64,
/// Inner)` layout allocates the first owned string, then latches failure on the
/// `i64`-where-array field, driving the enum-variant `fail_bb`. Must trap
/// fail-closed with no double-free of the partial owned string.
#[test]
fn enum_owned_payload_decode_failure_frees_partials_no_double_free() {
    assert_decode_failure_traps_no_double_free(
        "wire_cbor_enum_decode_failure",
        ENUM_DECODE_FAILURE_SOURCE,
    );
}

/// Item 1 — out-of-range enum-tag decode frees the reader + shell (no leak)
/// when the decode runs in an actor handler and the process survives the trap.
///
/// An unknown enum wire tag traps fail-closed. In an actor `receive` handler
/// that trap is caught by the supervisor's crash-recovery `siglongjmp`, so the
/// PROCESS keeps running and accumulates one leak per malformed message — which
/// is exactly the actor remote-send dispatch hazard. Pre-fix the unknown-tag
/// path took an INLINE native trap that `siglongjmp`ed PAST the deserialize
/// thunk's `hew_cbor_de_free(reader)` + `free(dst)` (measured: +5 leak
/// nodes/frame — the parsed CBOR tree + the partial shell). Post-fix it routes
/// through the thunk's `fail_bb` (free-before-trap), the same discipline every
/// other malformed shape already follows.
///
/// The proof is a DIFFERENTIAL slope. Both the OOB shape and an in-range
/// baseline spawn one `Decoder` actor per frame, so both carry the same
/// pre-existing per-spawn actor-cell leak (~1 node/frame — a known,
/// codec-independent leak; see `ask_reply_owned_leak_oracle`). Subtracting the
/// baseline slope from the OOB slope cancels that floor; the remainder is the
/// codec's own per-message leak, which must be within tolerance (post-fix: ~0;
/// pre-fix: ~5/frame → ~235 excess nodes over the 47-frame delta).
///
/// NOTE: the actor REMOTE-send decode path (`xnode_serial::decode_payload`,
/// which drops a failed decode instead of trapping) is not yet reachable from
/// compiled Hew (Node cross-node dispatch is codegen-pending). The same
/// deserialize thunk backs both the local `.decode()` call site exercised here
/// and that remote path, so this fixture pins the thunk's free discipline that
/// both callers depend on.
#[test]
fn actor_oob_enum_tag_decode_frees_reader_and_shell_no_excess_slope() {
    let Some((oob_low, oob_high)) = frame_leak_counts(
        "wire_cbor_actor_oob_enum",
        actor_oob_enum_decode_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    ) else {
        return;
    };
    let Some((base_low, base_high)) = frame_leak_counts(
        "wire_cbor_actor_in_range_enum",
        actor_in_range_enum_decode_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    ) else {
        return;
    };

    let oob_slope = oob_high.saturating_sub(oob_low);
    let base_slope = base_high.saturating_sub(base_low);
    eprintln!(
        "actor OOB enum differential: oob_slope={oob_slope} (oob_low={oob_low} oob_high={oob_high}) \
         base_slope={base_slope} (base_low={base_low} base_high={base_high}) tolerance={SLOPE_TOLERANCE}"
    );
    assert!(
        oob_slope <= base_slope + SLOPE_TOLERANCE,
        "out-of-range enum-tag decode leaks per malformed message: OOB slope {oob_slope} exceeds \
         the per-spawn baseline slope {base_slope} by {} nodes (tolerance {SLOPE_TOLERANCE}). The \
         unknown-tag path is not freeing the reader (parsed CBOR tree) + shell before failing — \
         it likely took an inline trap that unwound past the thunk's free-closed cleanup.",
        oob_slope.saturating_sub(base_slope + SLOPE_TOLERANCE),
    );
}
