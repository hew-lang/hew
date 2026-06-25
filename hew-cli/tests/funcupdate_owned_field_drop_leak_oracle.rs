//! Functional-update owned-field drop leak oracle.
//!
//! Exercises `{ ..base, field: new_value }` where `field` is an owned
//! heap type (`string`, `bytes`, `Vec<T>`, `HashMap`, `HashSet`).
//!
//! Before the fix (the functional-update overridden-owned-field leak in
//! `hew-mir/src/lower.rs`), every iteration of a
//! `{ ..base, label: new_string }` loop that overrides a heap-owning field
//! leaked exactly ONE allocation node (the old value of `label` from
//! `base` was never released — the new record owned the replacement, the
//! base's composite drop was suppressed by `derive_owned_record_drop_allowed`,
//! but the overridden field's old allocation was abandoned).
//!
//! After the fix, the functional-update arm emits, before the `RecordInit`:
//!
//! 1. `RecordFieldDrop { record: base, field_offset: N, … }` for every
//!    single-pointer COW field (`string`, `Vec<T>`, `HashMap`, `HashSet`):
//!    raw load → release → null-store.
//! 2. `RecordFieldLoad + Instr::Drop` for the fat `bytes` `{ptr,len,cap}`
//!    triple, whose destructor takes the whole by-value value.
//!
//! `Generator` is also a single-pointer COW field handled by
//! `RecordFieldDrop`, but a *functional update* can never reach one: a record
//! carrying a `Generator` field is front-stopped before this lowering by
//! record-clone-thunk synthesis (the coroutine handle has no per-field `dup`
//! symbol). The `hew_gen_coro_destroy` override-drop arm is therefore
//! exercised out of band (via tuple/enum match-destructure), not by this
//! oracle, and is deliberately NOT claimed here.
//!
//! …so every replaced owned value is released at the construction site on
//! every execution path. The release is sound because `..base` consumes the
//! base (the move-checker rejects any later read), so the freed old value has
//! no surviving reader.
//!
//! ## Shape coverage
//!
//! * **string field** — the most common override case; pre-fix slope ~1.0
//!   node/frame.
//! * **bytes field** — override a `bytes` field in a local loop; verifies
//!   the `hew_bytes_drop` inline-drop arm is correctly selected.
//! * **Vec<i64> field** — override a plain `Vec<i64>` field (no owned
//!   elements); exercises `hew_vec_free`.
//! * **`HashMap` field** — override a `HashMap<string,i64>` field; exercises
//!   the `hew_hashmap_free_layout` single-pointer COW release.
//! * **`HashSet` field** — override a `HashSet<i64>` field; exercises the
//!   `hew_hashset_free_layout` single-pointer COW release.
//! * **multi-field** — override a `string` AND a `Vec<i64>` in the same
//!   update expression; each must be independently released.
//!
//! ## Slope methodology
//!
//! Mirrors `bytes_drop_leak_oracle.rs`: compile the same shape at LOW
//! and HIGH frame counts, measure leak NODE counts under `leaks --atExit`
//! with the poisoned-allocator triple (`MallocScribble` et al.), and assert
//! the delta stays within `SLOPE_TOLERANCE` nodes regardless of frame count.
//!
//! macOS-only: the `leaks(1)` allocator inspector is a Darwin tool.  The
//! oracle skips (with an explanatory message) when `leaks` is unavailable.

#![cfg(unix)]

mod support;

use std::path::PathBuf;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

/// Low frame count: stays close to the constant-overhead floor.
const LOW_FRAMES: usize = 3;

/// High frame count for the slope check.  A slope of 1.0 leak/frame
/// (pre-fix measurement) produces `50 - 3 = 47` excess nodes against
/// the tolerance of `5`.
const HIGH_FRAMES: usize = 50;

/// Maximum permitted leak-node delta between the HIGH and LOW probes.
const SLOPE_TOLERANCE: usize = 5;

// ── fixture sources ──────────────────────────────────────────────────────

/// Functional update overriding a `string` field in a plain local loop.
///
/// `label` is replaced each iteration with a fresh `hew_string_repeat`
/// allocation.  Pre-fix: one leaked `cstring` node per iteration.
/// Post-fix: the old `label` is released via `hew_string_drop` before
/// the new record is built.
fn string_field_source(frames: usize) -> String {
    format!(
        "import std::string;\n\
         \n\
         record Cfg {{\n\
         \x20   label: string,\n\
         \x20   count: i64,\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var c = Cfg {{ label: string.repeat(\"a\", 32), count: 0 }};\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       c = Cfg {{ label: string.repeat(\"b\", 32), ..c }};\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   c.count\n\
         }}\n"
    )
}

/// Functional update overriding a `bytes` field in a local loop.
///
/// Each iteration replaces `buf` with a fresh `to_bytes()` allocation.
/// Pre-fix: one leaked bytes-buffer node per iteration.
/// Post-fix: the old `buf` is released via `hew_bytes_drop`.
fn bytes_field_source(frames: usize) -> String {
    format!(
        "record ByteHolder {{\n\
         \x20   buf: bytes,\n\
         \x20   count: i64,\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var h = ByteHolder {{ buf: \"initial\".to_bytes(), count: 0 }};\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       h = ByteHolder {{ buf: \"loop-payload\".to_bytes(), ..h }};\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   h.count\n\
         }}\n"
    )
}

/// Functional update overriding a `Vec<i64>` field in a local loop.
///
/// Each iteration replaces `items` with a fresh Vec allocation.
/// Pre-fix: one leaked Vec-header node per iteration.
/// Post-fix: the old `items` is released via `hew_vec_free`.
fn vec_field_source(frames: usize) -> String {
    format!(
        "record VecHolder {{\n\
         \x20   items: Vec<i64>,\n\
         \x20   tag: i64,\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let init: Vec<i64> = Vec::new();\n\
         \x20   init.push(99);\n\
         \x20   var h = VecHolder {{ items: init, tag: 0 }};\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let next: Vec<i64> = Vec::new();\n\
         \x20       next.push(i);\n\
         \x20       h = VecHolder {{ items: next, ..h }};\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   h.tag\n\
         }}\n"
    )
}

/// Functional update overriding a `HashMap<string,i64>` field in a loop.
///
/// Each iteration replaces `m` with a fresh map allocation.
/// Pre-fix: one leaked HashMap-control node per iteration.
/// Post-fix: the old `m` is released via `hew_hashmap_free_layout`.
fn hashmap_field_source(frames: usize) -> String {
    format!(
        "record MapHolder {{\n\
         \x20   m: HashMap<string, i64>,\n\
         \x20   tag: i64,\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let init: HashMap<string, i64> = HashMap::new();\n\
         \x20   init.insert(\"seed\", 1);\n\
         \x20   var h = MapHolder {{ m: init, tag: 0 }};\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let next: HashMap<string, i64> = HashMap::new();\n\
         \x20       next.insert(\"k\", i);\n\
         \x20       h = MapHolder {{ m: next, ..h }};\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   h.tag\n\
         }}\n"
    )
}

/// Functional update overriding a `HashSet<i64>` field in a loop.
///
/// Each iteration replaces `s` with a fresh set allocation.
/// Pre-fix: one leaked HashSet-control node per iteration.
/// Post-fix: the old `s` is released via `hew_hashset_free_layout`.
fn hashset_field_source(frames: usize) -> String {
    format!(
        "record SetHolder {{\n\
         \x20   s: HashSet<i64>,\n\
         \x20   tag: i64,\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let init: HashSet<i64> = HashSet::<i64>::new();\n\
         \x20   init.insert(99);\n\
         \x20   var h = SetHolder {{ s: init, tag: 0 }};\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let next: HashSet<i64> = HashSet::<i64>::new();\n\
         \x20       next.insert(i);\n\
         \x20       h = SetHolder {{ s: next, ..h }};\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   h.tag\n\
         }}\n"
    )
}

/// Functional update overriding both a `string` AND a `Vec<i64>` field.
///
/// Pre-fix: two leaked nodes per iteration (one per overridden field).
/// Post-fix: both fields independently released before `RecordInit`.
fn multi_field_source(frames: usize) -> String {
    format!(
        "import std::string;\n\
         \n\
         record Multi {{\n\
         \x20   label: string,\n\
         \x20   items: Vec<i64>,\n\
         \x20   id: i64,\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let init: Vec<i64> = Vec::new();\n\
         \x20   init.push(0);\n\
         \x20   var m = Multi {{ label: string.repeat(\"z\", 16), items: init, id: 0 }};\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let next: Vec<i64> = Vec::new();\n\
         \x20       next.push(i);\n\
         \x20       m = Multi {{ label: string.repeat(\"y\", 16), items: next, ..m }};\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   m.id\n\
         }}\n"
    )
}

// ── carry-axis sources ─────────────────────────────────────────────────────
//
// Every override source above CARRIES only a BitCopy (`count` / `tag` / `id`)
// field — the coverage gap that let the carried-record SIGSEGV and the
// carried-closure double-free ship while the prior suites passed. These
// sources invert the axis: each OVERRIDES a churn `string` and CARRIES the
// owned field under test every frame, so a per-frame leak in the carry path
// (the consumed base's field not transferred, or transferred without the base
// excluded from its composite drop) shows as slope. Slope-0 confirms the
// carried field is moved exactly once per frame.

/// Carry a nested owned-RECORD field while churning a `string`.
fn carry_record_field_source(frames: usize) -> String {
    format!(
        "import std::string;\n\
         \n\
         record Inner {{\n\
         \x20   label: string,\n\
         \x20   n: i64,\n\
         }}\n\
         record Pair {{\n\
         \x20   keep: Inner,\n\
         \x20   churn: string,\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var p = Pair {{ keep: Inner {{ label: string.repeat(\"k\", 32), n: 1 }}, churn: string.repeat(\"a\", 32) }};\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       p = Pair {{ churn: string.repeat(\"b\", 32), ..p }};\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   p.keep.n\n\
         }}\n"
    )
}

/// Carry a `string` field while churning a different `string` field.
fn carry_string_field_source(frames: usize) -> String {
    format!(
        "import std::string;\n\
         \n\
         record Pair {{\n\
         \x20   keep: string,\n\
         \x20   churn: string,\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var p = Pair {{ keep: string.repeat(\"k\", 32), churn: string.repeat(\"a\", 32) }};\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       p = Pair {{ churn: string.repeat(\"b\", 32), ..p }};\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   p.keep.len()\n\
         }}\n"
    )
}

/// Carry a `Vec<string>` (owned-element) field while churning a `string`.
fn carry_vec_field_source(frames: usize) -> String {
    format!(
        "import std::string;\n\
         \n\
         record Pair {{\n\
         \x20   keep: Vec<string>,\n\
         \x20   churn: string,\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let v: Vec<string> = Vec::new();\n\
         \x20   v.push(string.repeat(\"k\", 32));\n\
         \x20   var p = Pair {{ keep: v, churn: string.repeat(\"a\", 32) }};\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       p = Pair {{ churn: string.repeat(\"b\", 32), ..p }};\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   p.keep.len()\n\
         }}\n"
    )
}

/// Carry a `HashMap<string,string>` field while churning a `string`.
fn carry_hashmap_field_source(frames: usize) -> String {
    format!(
        "import std::string;\n\
         \n\
         record Pair {{\n\
         \x20   keep: HashMap<string, string>,\n\
         \x20   churn: string,\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let m: HashMap<string, string> = HashMap::new();\n\
         \x20   m.insert(string.repeat(\"k\", 32), string.repeat(\"v\", 32));\n\
         \x20   var p = Pair {{ keep: m, churn: string.repeat(\"a\", 32) }};\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       p = Pair {{ churn: string.repeat(\"b\", 32), ..p }};\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   p.keep.len()\n\
         }}\n"
    )
}

/// Carry a `HashSet<string>` field while churning a `string`.
fn carry_hashset_field_source(frames: usize) -> String {
    format!(
        "import std::string;\n\
         \n\
         record Pair {{\n\
         \x20   keep: HashSet<string>,\n\
         \x20   churn: string,\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let s: HashSet<string> = HashSet::new();\n\
         \x20   s.insert(string.repeat(\"k\", 32));\n\
         \x20   var p = Pair {{ keep: s, churn: string.repeat(\"a\", 32) }};\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       p = Pair {{ churn: string.repeat(\"b\", 32), ..p }};\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   p.keep.len()\n\
         }}\n"
    )
}

/// Carry a `bytes` field while churning a `string`.
fn carry_bytes_field_source(frames: usize) -> String {
    format!(
        "import std::string;\n\
         \n\
         record Pair {{\n\
         \x20   keep: bytes,\n\
         \x20   churn: string,\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var p = Pair {{ keep: string.repeat(\"k\", 32).to_bytes(), churn: string.repeat(\"a\", 32) }};\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       p = Pair {{ churn: string.repeat(\"b\", 32), ..p }};\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   p.keep.len()\n\
         }}\n"
    )
}

// ── plumbing (same shape as bytes_drop_leak_oracle) ───────────────────────

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
            "skip: leaks did not emit a `Process <pid>: N leak(s) ...` summary for {}: \
             stderr=\n{}",
            bin.display(),
            String::from_utf8_lossy(&output.stderr)
        );
    }
    parsed
}

/// Build the shape at LOW and HIGH frame counts, measure leak NODE
/// counts, and assert the delta stays within `SLOPE_TOLERANCE`.
fn assert_frame_slope_below_tolerance(shape_name: &str, source_fn: fn(usize) -> String) {
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
        .prefix(&format!("funcupdate-leak-{shape_name}-"))
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
         tolerance of {SLOPE_TOLERANCE} indicates the old field value is not being released \
         at the functional-update site (pre-fix slope: ~1 node/frame per overridden owned \
         field). Re-run with `MallocStackLogging=1 leaks --atExit -- {}` to identify the \
         leaked stack.",
        high_leaks.saturating_sub(low_leaks + SLOPE_TOLERANCE),
        bin_high.display()
    );
    assert!(
        high_leaks + SLOPE_TOLERANCE >= low_leaks,
        "{shape_name}: HIGH leak count is more than {SLOPE_TOLERANCE} below LOW \
         (low={low_leaks}, high={high_leaks}) — the binary did not finish before \
         `leaks --atExit` snapshotted. Increase the iteration count."
    );
}

// ── oracles ───────────────────────────────────────────────────────────────

/// `string` field override: pre-fix slope ~1.0 node/frame (one leaked
/// `cstring` buffer per iteration); post-fix slope 0.
#[test]
fn funcupdate_string_field_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance("funcupdate_string_field", string_field_source);
}

/// `bytes` field override: pre-fix slope ~1.0 node/frame (one leaked
/// `BytesTriple` data-ptr per iteration); post-fix slope 0.
#[test]
fn funcupdate_bytes_field_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance("funcupdate_bytes_field", bytes_field_source);
}

/// `Vec<i64>` field override: pre-fix slope ~1.0 node/frame (one leaked
/// Vec header per iteration); post-fix slope 0.
#[test]
fn funcupdate_vec_field_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance("funcupdate_vec_field", vec_field_source);
}

/// `HashMap<string,i64>` field override: pre-fix slope ~1.0 node/frame
/// (one leaked map-control node per iteration); post-fix slope 0.
#[test]
fn funcupdate_hashmap_field_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance("funcupdate_hashmap_field", hashmap_field_source);
}

/// `HashSet<i64>` field override: pre-fix slope ~1.0 node/frame (one
/// leaked set-control node per iteration); post-fix slope 0.
#[test]
fn funcupdate_hashset_field_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance("funcupdate_hashset_field", hashset_field_source);
}

/// Multi-field override (`string` + `Vec<i64>`): pre-fix slope ~2.0
/// nodes/frame (two leaked allocations per iteration); post-fix slope 0.
#[test]
fn funcupdate_multi_field_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance("funcupdate_multi_field", multi_field_source);
}

// ── carry-axis oracles (coverage-gap closure) ──────────────────────────────

/// Carried nested owned-RECORD field: pre-fix this SIGSEGV'd on the
/// double-freed carried record; post-fix the carry transfers it once per
/// frame — slope 0.
#[test]
fn funcupdate_carry_record_field_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance("funcupdate_carry_record_field", carry_record_field_source);
}

/// Carried `string` field (the retain-vs-exclude question): the
/// carried string is moved once per frame, not retained-without-release —
/// slope 0.
#[test]
fn funcupdate_carry_string_field_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance("funcupdate_carry_string_field", carry_string_field_source);
}

/// Carried `Vec<string>` (owned-element) field: moved once per frame, slope 0.
#[test]
fn funcupdate_carry_vec_field_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance("funcupdate_carry_vec_field", carry_vec_field_source);
}

/// Carried `HashMap<string,string>` field: moved once per frame, slope 0.
#[test]
fn funcupdate_carry_hashmap_field_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "funcupdate_carry_hashmap_field",
        carry_hashmap_field_source,
    );
}

/// Carried `HashSet<string>` field: moved once per frame, slope 0.
#[test]
fn funcupdate_carry_hashset_field_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance(
        "funcupdate_carry_hashset_field",
        carry_hashset_field_source,
    );
}

/// Carried `bytes` field: the fat `{ptr,len,cap}` triple is moved once per
/// frame, slope 0.
#[test]
fn funcupdate_carry_bytes_field_no_per_frame_leak_slope() {
    assert_frame_slope_below_tolerance("funcupdate_carry_bytes_field", carry_bytes_field_source);
}
