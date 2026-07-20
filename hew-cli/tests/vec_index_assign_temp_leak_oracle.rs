//! Owned-Vec INDEX-ASSIGNMENT (`v[i] = ..`) leak oracles: neither a fresh
//! aggregate RHS nor a consumed bound-local RHS may leak its heap.
//!
//! ## The bug this pins
//!
//! `v[i] = Holder { .. }` lowers through `assign()`'s Vec-set target arm, which
//! emitted `hew_vec_set_owned` (COPY-IN — deep-clone the element into the slot)
//! UNCONDITIONALLY. The `.set(i, ..)` METHOD path already routed a fresh
//! materialised owner to the MOVE-in sibling `hew_vec_set_owned_move`, but the
//! index-assignment SUGAR did not: it never applied the
//! `expr_is_materialized_owner` gate, so an unbound `Holder { .. }` RHS was
//! deep-cloned COPY-IN with no binding and no scope-exit drop to balance the
//! clone — the source temp's owned heap leaked once per store. Measured on the
//! fix base: the 50-frame positive shape leaks 200 nodes (~4 per store); the fix
//! routes the fresh materialised RHS to `hew_vec_set_owned_move` and it leaks 0.
//!
//! ## Why a DEEP-OWNED element (not a string field)
//!
//! Same rationale as the sibling `vec_push_temp_leak_oracle`: a record with only
//! a `string` field does NOT leak under copy-in (strings are refcount-shared and
//! reclaimed via the vec free). Only a DEEP-OWNED element (`Holder { items:
//! Vec<string> }`, whose `clone_fn` deep-copies into a DISTINCT buffer) exposes
//! the leak. A `Vec<record{string}>` fixture would pass even with the bug.
//!
//! ## The consumed bound-local hole
//!
//! A bare-binding RHS (`v[i] = h`) is `Use { intent=Consume }` in checked MIR:
//! `v[i] = h; use h` is rejected. COPY-IN cloned `h` into the slot while the
//! consume suppressed `h`'s scope-exit drop, orphaning the original deep-owned
//! heap (~4 nodes/store). The fix routes exactly a consumed non-parameter,
//! non-capture `BindingRef` to `hew_vec_set_owned_move`. A borrowed/read binding,
//! such as a local borrowed through a closure capture, remains COPY-IN because
//! the capture environment still owns it.
//!
//! ## Skip behaviour
//!
//! The slope oracle is macOS-only (`leaks(1)` is Darwin's allocator inspector);
//! elsewhere it logs `skip:` and returns. The symbol pin runs on any unix host.

#![cfg(unix)]

mod support;

use std::path::PathBuf;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

/// Low frame count: exercises the loop back-edge at least twice while staying
/// near the constant-overhead floor.
const LOW_FRAMES: usize = 3;

/// High frame count for the slope check. A pre-fix slope of ~4 leaks/frame for
/// the deep-owned element leak class produces `HIGH_FRAMES - LOW_FRAMES = 47`
/// frames × 4 ≈ 188 excess nodes against the tolerance of 5.
const HIGH_FRAMES: usize = 50;

/// Maximum permitted leak-node delta between the HIGH and LOW probes. Same
/// headroom rationale as the sibling oracles: absorbs one-off runtime/scheduler
/// allocations that appear only in the HIGH run while still catching a slope of
/// ~0.1 leaks/frame.
const SLOPE_TOLERANCE: usize = 5;

/// Shared prelude: a deep-owned element type and a fresh-Vec producer. `mkItems`
/// returns a FRESH `Vec<string>` (rc=1, its own buffer) so each element the
/// store ingests owns distinct heap the leak oracle can count.
const PRELUDE: &str = "\
record Holder { items: Vec<string> }\n\
fn mkItems(i: i64) -> Vec<string> {\n\
\x20   var xs: Vec<string> = Vec::new();\n\
\x20   xs.push(\"deep-elem-a\");\n\
\x20   xs.push(\"deep-elem-b\");\n\
\x20   return xs;\n\
}\n";

// ── fixtures ──────────────────────────────────────────────────────────────

/// An unbound `Holder { .. }` INDEX-ASSIGN per iteration over a one-element seed.
/// The RHS is a fresh materialised owner routed to `hew_vec_set_owned_move`;
/// index-assignment's overwrite-drop frees the replaced element and the move
/// transfers the new element's heap, so the source temp leaks nothing. Pre-fix
/// (`hew_vec_set_owned`, COPY-IN) this leaks ~4 nodes/frame.
fn index_assign_temp_source(frames: usize) -> String {
    format!(
        "{PRELUDE}\
         fn main() -> i64 {{\n\
         \x20   var v: Vec<Holder> = Vec::new();\n\
         \x20   v.push(Holder {{ items: mkItems(0) }});\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       v[0] = Holder {{ items: mkItems(i) }};\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   v.len()\n\
         }}\n"
    )
}

/// A consumed bound local per iteration. Checked MIR suppresses the local's
/// scope-exit drop, so MOVE-IN must transfer its heap into the Vec slot. Pre-fix,
/// COPY-IN leaks the local's original nested Vec and strings (~4 nodes/frame).
fn index_assign_consumed_bound_source(frames: usize) -> String {
    format!(
        "{PRELUDE}\
         fn main() -> i64 {{\n\
         \x20   var v: Vec<Holder> = Vec::new();\n\
         \x20   v.push(Holder {{ items: mkItems(0) }});\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let h = Holder {{ items: mkItems(i) }};\n\
         \x20       v[0] = h;\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   v.len()\n\
         }}\n"
    )
}

/// Symbol/run pin source for the consumed bound-local route. The stored element
/// is read back through the Vec (`v[0].items.len()` == 2). Expected stdout:
/// `2OK`.
const CONSUMED_BOUND_SOURCE: &str = "\
record Holder { items: Vec<string> }\n\
fn mkItems(i: i64) -> Vec<string> {\n\
\x20   var xs: Vec<string> = Vec::new();\n\
\x20   xs.push(\"deep-elem-a\");\n\
\x20   xs.push(\"deep-elem-b\");\n\
\x20   return xs;\n\
}\n\
fn indexAssignBound() -> i64 {\n\
\x20   var v: Vec<Holder> = Vec::new();\n\
\x20   v.push(Holder { items: mkItems(0) });\n\
\x20   let h = Holder { items: mkItems(1) };\n\
\x20   v[0] = h;\n\
\x20   return v[0].items.len();\n\
}\n\
fn main() {\n\
\x20   print(indexAssignBound());\n\
\x20   print(\"OK\");\n\
}\n";

/// Borrowed negative control: `h` is a bound local owned by the closure capture
/// environment. Each invocation may read it into the Vec, so index-assignment
/// must remain COPY-IN. Expected stdout: `22OK`.
const BORROWED_CAPTURE_SOURCE: &str = "\
record Holder { items: Vec<string> }\n\
fn mkItems() -> Vec<string> {\n\
\x20   let xs: Vec<string> = Vec::new();\n\
\x20   xs.push(\"deep-elem-a\");\n\
\x20   xs.push(\"deep-elem-b\");\n\
\x20   return xs;\n\
}\n\
fn main() {\n\
\x20   let h = Holder { items: mkItems() };\n\
\x20   let assign = || {\n\
\x20       var v: Vec<Holder> = Vec::new();\n\
\x20       v.push(Holder { items: mkItems() });\n\
\x20       v[0] = h;\n\
\x20       v[0].items.len()\n\
\x20   };\n\
\x20   print(assign());\n\
\x20   print(assign());\n\
\x20   print(\"OK\");\n\
}\n";

// ── leak measurement plumbing (same shape as vec_push_temp_leak_oracle) ────

/// Compile `source` to a native binary via `hew compile --emit-dir` and return
/// the emit directory plus the binary path. The relocatable `<name>.o` sits
/// alongside the binary in the emit dir for symbol inspection.
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

/// Build the shape at `low_frames` and `high_frames`, measure leak NODE counts,
/// and assert the delta stays within `SLOPE_TOLERANCE`.
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
        .prefix(&format!("vec-index-assign-leak-{shape_name}-"))
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
         high_frames={high_frames} high_leaks={high_leaks} tolerance={SLOPE_TOLERANCE}"
    );
    assert!(
        high_leaks <= low_leaks + SLOPE_TOLERANCE,
        "{shape_name}: per-frame leak SLOPE — low_frames={low_frames} low_leaks={low_leaks}, \
         high_frames={high_frames} high_leaks={high_leaks}. Excess of {} NODES over the \
         tolerance of {SLOPE_TOLERANCE} indicates a per-iteration owned-element temp is not \
         being released — the unbound aggregate index-assign RHS is deep-cloned COPY-IN and its \
         heap leaks. Route the fresh materialised RHS to the MOVE-in sibling \
         (`hew_vec_set_owned_move`) in `assign()`'s Vec-set target arm. Re-run with \
         `MallocStackLogging=1 leaks --atExit -- {}` to see the leaked block's stack.",
        high_leaks.saturating_sub(low_leaks + SLOPE_TOLERANCE),
        bin_high.display()
    );
}

// ── oracles ───────────────────────────────────────────────────────────────

/// An unbound `Holder { .. }` INDEX-ASSIGN per iteration must not leak — the
/// fresh materialised RHS is routed to `hew_vec_set_owned_move`, not deep-cloned.
/// Reverting the `expr_is_materialized_owner` gate in `assign()`'s Vec-set arm
/// fails this by ~188 nodes: this is the hole the fix closes.
#[test]
fn vec_index_assign_owned_temp_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "index_assign_temp",
        index_assign_temp_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}

/// A consumed bound-local index-assignment must have zero per-frame leak slope.
/// Reverting the consumed-`BindingRef` route leaks ~4 nodes/frame.
#[test]
fn vec_index_assign_consumed_bound_local_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "index_assign_consumed_bound",
        index_assign_consumed_bound_source,
        LOW_FRAMES,
        HIGH_FRAMES,
    );
}

/// The consumed bound-local route must emit MOVE-IN rather than COPY-IN. The
/// binary also runs clean under the poisoned allocator and prints `2OK`.
#[test]
fn vec_index_assign_consumed_bound_local_moves_in() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("vec-index-assign-consumed-bound-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(CONSUMED_BOUND_SOURCE, dir.path(), "consumed_bound");

    // The relocatable object sits alongside the binary in the emit dir.
    let obj = dir.path().join("consumed_bound.o");
    let nm = Command::new("nm")
        .arg(&obj)
        .output()
        .expect("invoke nm on emitted object");
    assert!(
        nm.status.success(),
        "nm failed on {}:\n{}",
        obj.display(),
        describe_output(&nm)
    );
    let symbols = String::from_utf8_lossy(&nm.stdout);
    // Symbol names are `_`-prefixed on Mach-O and bare on ELF; match the stem.
    assert!(
        symbols
            .lines()
            .any(|line| line.contains("hew_vec_set_owned_move")),
        "consumed bound-local index-assign (`v[i] = h`) must emit the MOVE-IN symbol \
         `hew_vec_set_owned_move`; checked MIR consumes `h`, so COPY-IN would orphan \
         `h`'s original heap. Emitted symbols were:\n{symbols}"
    );

    let output = Command::new(&bin)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .output()
        .expect("run consumed-bound binary");
    assert!(
        output.status.success(),
        "consumed bound-local index-assign must run clean under the poisoned allocator;\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        "2OK",
        "consumed bound-local index-assign must print the stored element's len (2) then OK;\n{}",
        describe_output(&output)
    );
}

/// Borrowed bound-local negative control: a closure capture remains COPY-IN and
/// can be used by two invocations without transferring the capture's heap.
#[test]
fn vec_index_assign_borrowed_capture_stays_copy_in() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("vec-index-assign-borrowed-capture-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(BORROWED_CAPTURE_SOURCE, dir.path(), "borrowed_capture");

    let obj = dir.path().join("borrowed_capture.o");
    let nm = Command::new("nm")
        .arg(&obj)
        .output()
        .expect("invoke nm on emitted object");
    assert!(
        nm.status.success(),
        "nm failed on {}:\n{}",
        obj.display(),
        describe_output(&nm)
    );
    let symbols = String::from_utf8_lossy(&nm.stdout);
    assert!(
        symbols.contains("hew_vec_set_owned")
            && !symbols
                .lines()
                .any(|line| line.contains("hew_vec_set_owned_move")),
        "borrowed closure-captured bound local must emit COPY-IN \
         `hew_vec_set_owned`, never `hew_vec_set_owned_move`. Emitted symbols were:\n{symbols}"
    );

    let output = Command::new(&bin)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .output()
        .expect("run borrowed-capture binary");
    assert!(
        output.status.success(),
        "borrowed-capture index-assign must run clean under the poisoned allocator;\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "22OK",
        "borrowed capture must survive both index assignments;\n{}",
        describe_output(&output)
    );
}
