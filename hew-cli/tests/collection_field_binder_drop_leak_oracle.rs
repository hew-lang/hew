//! Leak oracle for a heap-owning collection reached as a record FIELD or tuple
//! MEMBER and then read through a BORROWING method (`.len()`, `.get(i)`, …).
//!
//! ## The leak this pins
//!
//! A `Pair { a: i64, b: Vec<i64> }` (or `(Vec<i64>, i64)`) local earns a
//! tag-aware `DropKind::RecordInPlace` / `DropKind::TupleInPlace` scope-exit drop
//! ONLY when the fail-closed sole-owner derivation
//! (`derive_owned_record_drop_allowed` / `derive_tuple_composite_drop_allowed`)
//! proves the composite still owns its heap field/member at scope exit. That
//! derivation's escape scan classifies an owned field-binder — the `Vec`/`HashMap`
//! handle loaded out of the composite — as ESCAPING when it is read by ANY
//! instruction other than an exempt one, and over-excludes the whole composite
//! (fail-closed: leak, never double-free).
//!
//! The escape scan exempted a `string` field-binder read by a borrowing string
//! call (`hew_string_length`, `print`, …) so `r.label.len()` kept its record
//! drop. It did NOT exempt a COLLECTION field-binder read by a borrowing
//! collection op (`hew_vec_len`, `hew_vec_get_*`, `hew_hashmap_get_layout`, …):
//! `p.b.len()` therefore excluded the composite, and the inner `Vec<i64>` backing
//! buffer + handle leaked on every scope exit — one chain per call.
//!
//! The fix routes the field-binder escape scan through the SAME receiver-borrow
//! authority the collection-LOCAL scan already uses
//! (`is_vec_receiver_borrow_symbol` / `is_collection_receiver_borrow_callee`):
//! a field-binder read ONLY as the borrowed receiver (arg[0]) of such a call is
//! an interior borrow, not an escape, so the composite keeps its in-place drop
//! and the inner collection is released exactly once.
//!
//! ## Methodology: absolute count against a no-field-read control
//!
//! macOS `leaks --atExit` treats a pointer still in a function-scope `alloca` at
//! process exit as REACHABLE. To expose the leak the way Valgrind does, each
//! fixture allocates the composite in a HELPER fn that returns, run in a loop:
//! the stack slot is reused every iteration, so each leaked backing buffer
//! becomes genuinely unreachable and `leaks` counts it. Pre-fix the
//! field-read fixtures stood `ITERATIONS` chains above the control; post-fix they
//! sit at the control's floor. The CONTROL builds the SAME composite but reads
//! only its `BitCopy` scalar member (no collection field-binder), so its drop
//! always fired correctly — establishing the runtime's allocation floor.
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
/// leaked its composite's inner collection backing buffer + handle; the helper
/// return reuses the stack slot so the leaked nodes are unreachable at exit.
/// Picked well above the jitter tolerance so a regression is unambiguous.
const ITERATIONS: usize = 64;

/// Permitted leak-node delta between a fixture and the control. Pre-fix the
/// field-read fixtures stood >= `ITERATIONS` nodes above; post-fix they match.
const FLOOR_TOLERANCE: usize = 2;

// -- fixtures ----------------------------------------------------------------

/// Control: a `Pair { a: i64, b: Vec<i64> }` whose helper reads ONLY the
/// `BitCopy` scalar field `a` (never the collection field). The escape scan
/// classifies this composite correctly even before the borrow-aware fix (no
/// field-binder is ever produced), so it never leaked — it establishes the
/// floor.
fn control_record_scalar_read_source() -> String {
    format!(
        "\
type Pair {{ a: i64, b: Vec<i64> }}

fn make(n: i64) -> Pair {{
    let inner: Vec<i64> = Vec::new();
    inner.push(n);
    inner.push(n + 1);
    Pair {{ a: n, b: inner }}
}}

fn build(n: i64) -> i64 {{
    let p = make(n);
    p.a
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for i in 0..{ITERATIONS} {{
        total = total + build(i);
    }}
    if total != {expected} {{ return 60; }}
    0
}}
",
        expected = (0..ITERATIONS).sum::<usize>()
    )
}

/// `Pair { a: i64, b: Vec<i64> }` whose helper reads the COLLECTION field via the
/// borrowing `.len()` (`hew_vec_len`). Pre-fix the field-binder escape scan
/// treated the loaded `Vec` handle as escaping and excluded the record, leaking
/// the inner buffer once per call.
fn record_collection_field_read_source() -> String {
    format!(
        "\
type Pair {{ a: i64, b: Vec<i64> }}

fn make(n: i64) -> Pair {{
    let inner: Vec<i64> = Vec::new();
    inner.push(n);
    inner.push(n + 1);
    Pair {{ a: n, b: inner }}
}}

fn build(n: i64) -> i64 {{
    let p = make(n);
    p.b.len()
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for i in 0..{ITERATIONS} {{
        total = total + build(i);
    }}
    if total != {expected} {{ return 61; }}
    0
}}
",
        expected = ITERATIONS * 2
    )
}

/// A bare `Boxed { payload: Vec<i64> }` (the only field owns heap) read via
/// `.payload.len()`. Same leak shape as `Pair`, no `BitCopy` sibling.
fn bare_record_collection_field_read_source() -> String {
    format!(
        "\
type Boxed {{ payload: Vec<i64> }}

fn make(n: i64) -> Boxed {{
    let inner: Vec<i64> = Vec::new();
    inner.push(n);
    inner.push(n + 1);
    Boxed {{ payload: inner }}
}}

fn build(n: i64) -> i64 {{
    let b = make(n);
    b.payload.len()
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for i in 0..{ITERATIONS} {{
        total = total + build(i);
    }}
    if total != {expected} {{ return 62; }}
    0
}}
",
        expected = ITERATIONS * 2
    )
}

/// A `(Vec<i64>, i64)` tuple whose helper reads the collection member via
/// `p.0.len()`. Pins the TUPLE escape scan (`derive_tuple_composite_drop_allowed`)
/// — the tuple analogue of the record gap.
fn tuple_collection_member_read_source() -> String {
    format!(
        "\
fn make(n: i64) -> (Vec<i64>, i64) {{
    let inner: Vec<i64> = Vec::new();
    inner.push(n);
    inner.push(n + 1);
    (inner, n)
}}

fn build(n: i64) -> i64 {{
    let p = make(n);
    p.0.len()
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for i in 0..{ITERATIONS} {{
        total = total + build(i);
    }}
    if total != {expected} {{ return 63; }}
    0
}}
",
        expected = ITERATIONS * 2
    )
}

/// A nested `Outer { inner: Inner { payload: Vec<i64> } }` read through both
/// record layers via `o.inner.payload.len()`. The borrow-aware exemption must
/// recurse: the inner `Vec` handle is loaded two field layers deep and still
/// read only as the borrowed receiver of `hew_vec_len`.
fn nested_record_collection_field_read_source() -> String {
    format!(
        "\
type Inner {{ payload: Vec<i64> }}
type Outer {{ inner: Inner }}

fn make(n: i64) -> Outer {{
    let v: Vec<i64> = Vec::new();
    v.push(n);
    v.push(n + 1);
    Outer {{ inner: Inner {{ payload: v }} }}
}}

fn build(n: i64) -> i64 {{
    let o = make(n);
    o.inner.payload.len()
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for i in 0..{ITERATIONS} {{
        total = total + build(i);
    }}
    if total != {expected} {{ return 64; }}
    0
}}
",
        expected = ITERATIONS * 2
    )
}

/// A `Boxed { payload: Vec<i64> }` read via the borrowing element getter
/// `.payload[0]` (`hew_vec_get_*`) rather than `.len()`. Confirms the exemption
/// covers the getter family, not just the length inspector.
fn record_collection_index_read_source() -> String {
    format!(
        "\
type Boxed {{ payload: Vec<i64> }}

fn make(n: i64) -> Boxed {{
    let inner: Vec<i64> = Vec::new();
    inner.push(n);
    inner.push(n + 1);
    Boxed {{ payload: inner }}
}}

fn build(n: i64) -> i64 {{
    let b = make(n);
    b.payload[0]
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for i in 0..{ITERATIONS} {{
        total = total + build(i);
    }}
    if total != {expected} {{ return 65; }}
    0
}}
",
        expected = (0..ITERATIONS).sum::<usize>()
    )
}

/// String-field control: `Named { label: string }` read via `.label.len()`. This
/// path was ALREADY exempt (the borrowing-string rule), so it never leaked. Pins
/// the string side as an unchanged baseline so a regression that broke the
/// existing string exemption while adding the collection one is caught here.
fn record_string_field_read_source() -> String {
    format!(
        "\
type Named {{ label: string }}

fn make(n: i64) -> Named {{
    Named {{ label: \"a heap allocated label long enough to allocate\" }}
}}

fn build(n: i64) -> i64 {{
    let r = make(n);
    r.label.len()
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for i in 0..{ITERATIONS} {{
        total = total + build(i);
    }}
    0
}}
"
    )
}

/// No-double-free pin: a `Pair { a: i64, b: Vec<i64> }` whose collection field is
/// read via `.len()`, co-resident with a scalar Vec in one frame. The record
/// member-drop must run EXACTLY once and free the inner Vec; a double-free or
/// wrong-ABI free crashes under the poisoned allocator triple before the sentinel
/// prints. Runs on any unix.
const NO_DOUBLE_FREE_SOURCE: &str = "\
type Pair { a: i64, b: Vec<i64> }

fn main() {
    let inner: Vec<i64> = Vec::new();
    inner.push(10);
    inner.push(20);
    let p = Pair { a: 5, b: inner };
    let n = p.b.len();
    let ns: Vec<i64> = Vec::new();
    ns.push(7);
    let sum = p.a + n + ns[0];
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

/// Compile + measure `fixture` against the scalar-read control and assert the
/// fixture sits at the control's floor. A pre-fix collection-field-binder leak
/// puts the fixture >= `ITERATIONS` nodes above.
fn assert_no_leak_over_control(shape_name: &str, fixture_source: &str) {
    if !leaks_available(shape_name) {
        return;
    }
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("collection-field-binder-leak-{shape_name}-"))
        .tempdir()
        .expect("tempdir");

    let control_bin =
        compile_to_native(&control_record_scalar_read_source(), dir.path(), "control");
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
        "{shape_name}: a heap-owning collection reached as a record field / tuple member and \
         read via a BORROWING method leaked -- the fixture leaked {fixture_leaks} nodes against \
         the scalar-read control's floor of {control_leaks} (tolerance {FLOOR_TOLERANCE}). An \
         excess of {} nodes means the field-binder escape scan again treated the borrowed \
         collection receiver as an escape and excluded the composite from its in-place drop. The \
         fix routes the field-binder scan through the receiver-borrow authority \
         (`is_vec_receiver_borrow_symbol` / `is_collection_receiver_borrow_callee`) so a field \
         read only as the borrowed receiver keeps the composite's drop. Re-run with \
         `MallocStackLogging=1 leaks --atExit -- {}` to see the leaked stack.",
        fixture_leaks.saturating_sub(control_leaks + FLOOR_TOLERANCE),
        fixture_bin.display()
    );
}

// -- oracles -----------------------------------------------------------------

/// `Pair { a: i64, b: Vec<i64> }` read via `p.b.len()` -- the headline shape.
/// Pre-fix the inner Vec leaked once per helper call; post-fix it sits at the
/// scalar-read control floor.
#[test]
fn record_collection_field_read_no_leak() {
    assert_no_leak_over_control(
        "record_collection_field_read",
        &record_collection_field_read_source(),
    );
}

/// `Boxed { payload: Vec<i64> }` (single owned field) read via `.payload.len()`.
#[test]
fn bare_record_collection_field_read_no_leak() {
    assert_no_leak_over_control(
        "bare_record_collection_field_read",
        &bare_record_collection_field_read_source(),
    );
}

/// `(Vec<i64>, i64)` tuple read via `p.0.len()` -- the tuple escape-scan analogue.
#[test]
fn tuple_collection_member_read_no_leak() {
    assert_no_leak_over_control(
        "tuple_collection_member_read",
        &tuple_collection_member_read_source(),
    );
}

/// `Outer { inner: Inner { payload: Vec<i64> } }` read via
/// `o.inner.payload.len()` -- the exemption must recurse through both layers.
#[test]
fn nested_record_collection_field_read_no_leak() {
    assert_no_leak_over_control(
        "nested_record_collection_field_read",
        &nested_record_collection_field_read_source(),
    );
}

/// `Boxed { payload: Vec<i64> }` read via the borrowing element getter
/// `.payload[0]` -- confirms the getter family is exempt, not only `.len()`.
#[test]
fn record_collection_index_read_no_leak() {
    assert_no_leak_over_control(
        "record_collection_index_read",
        &record_collection_index_read_source(),
    );
}

/// `Named { label: string }` read via `.label.len()` -- the ALREADY-exempt string
/// side. Stays at the floor; a regression that broke the string exemption while
/// adding the collection one is caught here.
#[test]
fn record_string_field_read_no_leak() {
    assert_no_leak_over_control(
        "record_string_field_read",
        &record_string_field_read_source(),
    );
}

/// No-double-free pin: the `Pair { b: Vec }` member-drop must run EXACTLY once
/// and free the inner Vec; the co-resident scalar Vec stays clean. A double-free
/// or wrong-ABI free crashes under `MallocScribble` before the sentinel. Any unix.
#[test]
fn record_collection_field_read_release_is_exactly_once_under_malloc_scribble() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("collection-field-binder-no-double-free-")
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
        "the record collection-field member-drop must run exactly once -- a crash here indicates \
         a double-free of the inner Vec buffer or a wrong-ABI free;\n{}",
        describe_output(&output)
    );
    // p.a (5) + p.b.len() (2) + ns[0] (7) = 14, followed by the `OK` sentinel.
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        "14OK",
        "output must be untouched -- a scribbled value indicates an over-drop corrupted a \
         live value;\n{}",
        describe_output(&output)
    );
}
