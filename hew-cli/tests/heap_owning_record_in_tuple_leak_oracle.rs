//! Constant-leak oracle for a heap-owning record reached as a TUPLE MEMBER.
//!
//! ## The leak this pins (DIV-1)
//!
//! A `(Boxed, i64)` tuple local where `type Boxed { payload: Vec<i64> }`. The
//! tuple member-drop is governed by the structural heap-ownership authority that
//! decides whether the tuple earns a scope-exit `DropKind::TupleInPlace` drop.
//! Before the heap-ownership walkers were unified, the MIR-side walker that
//! gated the tuple member-drop (`ty_is_heap_owning_tuple` → the record-blind
//! `ty_contains_heap_owning`) did NOT consult record field types: a bare nested
//! user record is a `Named` that is neither a builtin nor an enum layout, so the
//! walker answered `false`. The tuple therefore earned NO drop, and the inner
//! `Vec<i64>` backing buffer LEAKED on every scope exit — one buffer per element
//! per call.
//!
//! The fix routes every heap-ownership query through the single record-aware
//! `ty_owns_heap` authority, which consults `Boxed`'s `payload: Vec<i64>` field
//! and classifies the tuple heap-owning. The MIR then emits the `TupleInPlace`
//! drop whose per-member step calls `__hew_record_drop_inplace_Boxed`, and the
//! codegen tuple-member seed pass synthesises that thunk body so the inner Vec
//! is released exactly once.
//!
//! ## Methodology: absolute count against a scalar-Vec control
//!
//! macOS `leaks --atExit` treats a pointer still in a function-scope `alloca` at
//! process exit as REACHABLE. To expose the leak the way Valgrind does, each
//! fixture allocates the tuple in a HELPER fn that returns, run in a loop: the
//! stack slot is reused every iteration, so each leaked backing buffer becomes
//! genuinely unreachable and `leaks` counts it. Pre-fix the record-in-tuple
//! fixture stood `ITERATIONS` nodes above the control; post-fix it sits at the
//! control's floor. The CONTROL holds the `Vec<i64>` DIRECTLY in the tuple
//! (`(Vec<i64>, i64)`) — a shape the walker classified correctly pre-fix
//! (its collection leaf arm fires), establishing the runtime's allocation floor.
//!
//! ## Skip behaviour
//!
//! `leaks(1)` is Darwin's allocator inspector; on non-macOS hosts the leak
//! probes log `skip:` and return. The `MallocScribble` no-double-free pin runs
//! on any unix host.

#![cfg(unix)]

mod support;

use std::path::PathBuf;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

/// Loop iterations each fixture's helper-fn is called. Pre-fix every iteration
/// leaked its helper's inner Vec backing buffer; the helper return reuses the
/// stack slot so the leaked buffers are unreachable at exit. Picked well above
/// the jitter tolerance so a regression is unambiguous.
const ITERATIONS: usize = 64;

/// Permitted leak-node delta between a fixture and the control. Pre-fix the
/// fixture stood >= `ITERATIONS` nodes above; post-fix it matches.
const FLOOR_TOLERANCE: usize = 2;

// -- fixtures ----------------------------------------------------------------

/// Control: a `(Vec<i64>, i64)` tuple holding the Vec DIRECTLY. The collection
/// leaf arm of the heap-ownership authority classifies it correctly even before
/// the record-aware fix, so this never leaked — it establishes the floor.
fn control_vec_in_tuple_source() -> String {
    format!(
        "\
fn build(n: i64) -> i64 {{
    let inner: Vec<i64> = Vec::new();
    inner.push(n);
    inner.push(n + 1);
    let pair = (inner, n);
    pair.1
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for i in 0..{ITERATIONS} {{
        total = total + build(i);
    }}
    if total != {expected} {{ return 70; }}
    0
}}
",
        expected = (0..ITERATIONS).sum::<usize>()
    )
}

/// DIV-1: a `(Boxed, i64)` tuple where `Boxed { payload: Vec<i64> }`. Pre-fix
/// the record-blind walker mis-classified the tuple non-heap-owning, so its
/// member-drop never fired and the inner `Vec<i64>` buffer leaked once per call.
fn record_in_tuple_source() -> String {
    format!(
        "\
type Boxed {{ payload: Vec<i64> }}

fn build(n: i64) -> i64 {{
    let inner: Vec<i64> = Vec::new();
    inner.push(n);
    inner.push(n + 1);
    let pair = (Boxed {{ payload: inner }}, n);
    pair.1
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for i in 0..{ITERATIONS} {{
        total = total + build(i);
    }}
    if total != {expected} {{ return 71; }}
    0
}}
",
        expected = (0..ITERATIONS).sum::<usize>()
    )
}

/// Nested-record DIV-1: `(Outer, i64)` where `Outer { inner: Inner }` and
/// `Inner { payload: Vec<i64> }`. The heap leaf is two record layers deep — the
/// authority must recurse through `Outer`'s field into `Inner`'s field.
fn nested_record_in_tuple_source() -> String {
    format!(
        "\
type Inner {{ payload: Vec<i64> }}
type Outer {{ inner: Inner }}

fn build(n: i64) -> i64 {{
    let v: Vec<i64> = Vec::new();
    v.push(n);
    v.push(n + 1);
    let pair = (Outer {{ inner: Inner {{ payload: v }} }}, n);
    pair.1
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for i in 0..{ITERATIONS} {{
        total = total + build(i);
    }}
    if total != {expected} {{ return 72; }}
    0
}}
",
        expected = (0..ITERATIONS).sum::<usize>()
    )
}

/// No-double-free pin: a `(Boxed, i64)` plus a co-resident scalar Vec in one
/// frame. The Boxed-in-tuple member-drop must run EXACTLY once and free the
/// inner Vec; a double-free or wrong-ABI free crashes under the poisoned
/// allocator triple before the sentinel prints. Runs on any unix.
const NO_DOUBLE_FREE_SOURCE: &str = "\
type Boxed { payload: Vec<i64> }

fn main() {
    let inner: Vec<i64> = Vec::new();
    inner.push(10);
    inner.push(20);
    let pair = (Boxed { payload: inner }, 5);
    let ns: Vec<i64> = Vec::new();
    ns.push(7);
    let sum = pair.1 + ns[0];
    print(sum);
    print(\"OK\");
}
";

// -- leak measurement plumbing (shape shared with the sibling oracles) -------

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
                        parsed = Some(n);
                        break;
                    }
                }
            }
        }
    }
    parsed
}

/// macOS + `leaks(1)` availability guard.
fn leaks_available(shape_name: &str) -> bool {
    if !cfg!(target_os = "macos") {
        eprintln!("skip: {shape_name}: leaks(1) is macOS-only");
        return false;
    }
    let avail = Command::new("which")
        .arg("leaks")
        .output()
        .is_ok_and(|o| o.status.success());
    if !avail {
        eprintln!("skip: {shape_name}: `leaks` binary not on PATH");
    }
    avail
}

/// Compile + measure `fixture` against the Vec-in-tuple control and assert the
/// fixture sits at the control's floor. A pre-fix record-in-tuple leak puts the
/// fixture >= `ITERATIONS` nodes above.
fn assert_no_leak_over_control(shape_name: &str, fixture_source: &str) {
    if !leaks_available(shape_name) {
        return;
    }
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("record-in-tuple-leak-{shape_name}-"))
        .tempdir()
        .expect("tempdir");

    let control_bin = compile_to_native(&control_vec_in_tuple_source(), dir.path(), "control");
    let fixture_bin = compile_to_native(fixture_source, dir.path(), shape_name);

    let Some(control_leaks) = measure_leaks(&control_bin) else {
        return;
    };
    let Some(fixture_leaks) = measure_leaks(&fixture_bin) else {
        return;
    };

    eprintln!(
        "{shape_name}: control_leaks={control_leaks} fixture_leaks={fixture_leaks} \
         tolerance={FLOOR_TOLERANCE} iterations={ITERATIONS}"
    );
    assert!(
        fixture_leaks <= control_leaks + FLOOR_TOLERANCE,
        "{shape_name}: a heap-owning record reached as a TUPLE MEMBER leaked -- the fixture \
         leaked {fixture_leaks} nodes against the Vec-in-tuple control's floor of \
         {control_leaks} (tolerance {FLOOR_TOLERANCE}). An excess of {} nodes means the \
         record-blind heap-ownership walker re-appeared: the tuple member-drop did not fire \
         the nested record's Vec release. The fix routes every heap-ownership query through \
         the record-aware `ty_owns_heap` authority (hew-mir) so `ty_is_heap_owning_tuple` \
         sees the record's heap field. Re-run with \
         `MallocStackLogging=1 leaks --atExit -- {}` to see the leaked stack.",
        fixture_leaks.saturating_sub(control_leaks + FLOOR_TOLERANCE),
        fixture_bin.display()
    );
}

// -- oracles -----------------------------------------------------------------

/// `(Boxed, i64)` where `Boxed { payload: Vec<i64> }` -- the headline DIV-1
/// shape. Pre-fix the inner Vec leaked once per helper call; post-fix it sits
/// at the Vec-in-tuple control floor. Reverting the record-aware authority
/// fails this by >= `ITERATIONS` nodes.
#[test]
fn record_in_tuple_no_leak() {
    assert_no_leak_over_control("record_in_tuple", &record_in_tuple_source());
}

/// `(Outer, i64)` where `Outer { inner: Inner { payload: Vec<i64> } }` -- the
/// heap leaf is two record layers deep; the authority must recurse through both.
#[test]
fn nested_record_in_tuple_no_leak() {
    assert_no_leak_over_control("nested_record_in_tuple", &nested_record_in_tuple_source());
}

/// No-double-free pin: the `(Boxed, i64)` member-drop must run EXACTLY once and
/// free the inner Vec; the co-resident scalar Vec stays clean. A double-free or
/// wrong-ABI free crashes under `MallocScribble` before the sentinel. Any unix.
#[test]
fn record_in_tuple_release_is_exactly_once_under_malloc_scribble() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("record-in-tuple-no-double-free-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(NO_DOUBLE_FREE_SOURCE, dir.path(), "no_double_free");

    let output = Command::new(&bin)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .output()
        .expect("run no-double-free binary");

    assert!(
        output.status.success(),
        "the record-in-tuple member-drop must run exactly once -- a crash here indicates a \
         double-free of the inner Vec buffer or a wrong-ABI free;\n{}",
        describe_output(&output)
    );
    // pair.1 (5) + ns[0] (7) = 12, followed by the `OK` sentinel.
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        "12OK",
        "output must be untouched -- a scribbled value indicates an over-drop corrupted a \
         live value;\n{}",
        describe_output(&output)
    );
}
