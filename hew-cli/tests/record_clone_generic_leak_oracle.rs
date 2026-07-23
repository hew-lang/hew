//! Drop-safety oracle for GENERIC record clone (`clone Pair<i64, i64>` and
//! owned-field instantiations). A per-mono clone thunk
//! synthesised WITHOUT its matching per-mono drop thunk is a leak (and, with a
//! shallow byte-copy, a double-free). The codegen synthesis loop emits the
//! clone AND drop body together per monomorphised key
//! (`emit_state_clone_drop_synthesis`), and the clone site is seeded from the
//! raw-MIR `RecordCloneInplace` walk (`collect_record_clone_inplace_seeds`);
//! this oracle proves that pair is balanced at runtime.
//!
//! ## Methodology: differential against a NO-CLONE construction control
//!
//! macOS `leaks --atExit` treats a pointer still sitting in a function-scope
//! `alloca` at process exit as REACHABLE and does not flag it. Each fixture
//! therefore allocates in a HELPER fn that returns an `i64`, run in a loop: the
//! stack slot is reused every iteration, so a leaked clone buffer becomes
//! genuinely unreachable and `leaks` counts it (the shape Valgrind flags).
//!
//! The clone fixture is measured against a control that performs the IDENTICAL
//! record CONSTRUCTION but performs NO clone. Constructing an owned record from
//! a fresh string producer (`seed + "-a"` into a field) has a pre-existing,
//! clone-independent leak (it reproduces with zero `clone` in the program —
//! the `construct-only` control fixtures in this file confirm), so the control absorbs that
//! floor. The DIFFERENCE between fixture and control is exactly the clone's
//! deep-copied buffers and their matching drop. A balanced clone/drop pair
//! leaves the fixture at the control's floor; a missing per-mono drop puts the
//! fixture `ITERATIONS * fields-per-clone` nodes above it.
//! This is the floor-equality assertion the `assert-distinguishes-garbage`
//! rule calls for, not a happy-path `> 0`.
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

/// Loop iterations each fixture's helper-fn is called. Each iteration's clone
/// buffers become unreachable when the helper returns (stack-slot reuse), the
/// shape `leaks` detects. Picked well above the jitter tolerance so a regression
/// of even a few un-dropped clone buffers is unambiguous.
const ITERATIONS: usize = 64;

/// Permitted leak-node delta between a clone fixture and its no-clone control.
/// A missing per-mono drop puts the fixture `>= ITERATIONS` nodes above the
/// control; the tolerance of 2 absorbs the +/-1 runtime one-off jitter the
/// sibling oracles document while staying far below `ITERATIONS`.
const FLOOR_TOLERANCE: usize = 2;

// -- fixtures ----------------------------------------------------------------

/// Control: construct an owned `Pair<string, string>` in a helper, NO clone.
/// Establishes the construction floor (the clone-independent fresh-producer
/// leak the fixture also incurs).
fn control_flat_source() -> String {
    format!(
        "\
type Pair<A, B> {{ a: A; b: B; }}

fn build(seed: string) -> i64 {{
    let p = Pair {{ a: seed + \"-a\", b: seed + \"-b\" }};
    p.a.len() + p.b.len()
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for i in 0..{ITERATIONS} {{
        total = total + build(\"seed\");
    }}
    if total != {expected} {{ return 70; }}
    0
}}
",
        expected = ITERATIONS * 12
    )
}

/// Fixture: construct + `clone` an owned `Pair<string, string>`, using BOTH the
/// original and the clone (so neither is elided) and dropping both at helper
/// exit. The clone deep-copies two heap strings; its matching per-mono drop must
/// free them. Against `control_flat_source` the clone must add no leak.
fn fixture_flat_source() -> String {
    format!(
        "\
type Pair<A, B> {{ a: A; b: B; }}

fn build(seed: string) -> i64 {{
    let p = Pair {{ a: seed + \"-a\", b: seed + \"-b\" }};
    let q = clone p;
    q.a.len() + q.b.len() + p.a.len() + p.b.len()
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for i in 0..{ITERATIONS} {{
        total = total + build(\"seed\");
    }}
    if total != {expected} {{ return 71; }}
    0
}}
",
        expected = ITERATIONS * 24
    )
}

/// Control: construct a NESTED owned `Pair<Pair<string, string>, string>`, no clone.
fn control_nested_source() -> String {
    format!(
        "\
type Pair<A, B> {{ a: A; b: B; }}

fn build(seed: string) -> i64 {{
    let inner = Pair {{ a: seed + \"-a\", b: seed + \"-b\" }};
    let p = Pair {{ a: inner, b: seed + \"-c\" }};
    p.a.a.len() + p.a.b.len() + p.b.len()
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for i in 0..{ITERATIONS} {{
        total = total + build(\"seed\");
    }}
    if total != {expected} {{ return 72; }}
    0
}}
",
        expected = ITERATIONS * 18
    )
}

/// Fixture: construct + `clone` the nested record. The clone must deep-copy the
/// inner record AND all three heap strings, and the per-mono drop of the nested
/// instantiation must free every cloned buffer transitively.
fn fixture_nested_source() -> String {
    format!(
        "\
type Pair<A, B> {{ a: A; b: B; }}

fn build(seed: string) -> i64 {{
    let inner = Pair {{ a: seed + \"-a\", b: seed + \"-b\" }};
    let p = Pair {{ a: inner, b: seed + \"-c\" }};
    let q = clone p;
    q.a.a.len() + q.a.b.len() + q.b.len() + p.a.a.len() + p.a.b.len() + p.b.len()
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for i in 0..{ITERATIONS} {{
        total = total + build(\"seed\");
    }}
    if total != {expected} {{ return 73; }}
    0
}}
",
        expected = ITERATIONS * 36
    )
}

/// Control: construct BOTH a `Pair<i64, i64>` and a `Pair<string, string>` in
/// one helper, no clone. Pins that the per-mono keying does not cross-wire two
/// instantiations of the same generic record.
fn control_multi_source() -> String {
    format!(
        "\
type Pair<A, B> {{ a: A; b: B; }}

fn build(seed: string) -> i64 {{
    let nums = Pair {{ a: 1, b: 2 }};
    let strs = Pair {{ a: seed + \"-a\", b: seed + \"-b\" }};
    nums.a + nums.b + strs.a.len() + strs.b.len()
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for i in 0..{ITERATIONS} {{
        total = total + build(\"seed\");
    }}
    if total != {expected} {{ return 74; }}
    0
}}
",
        expected = ITERATIONS * 15
    )
}

/// Fixture: clone BOTH instantiations. The scalar `Pair$$i64$i64` clone is a
/// trivial (no-heap) copy; the `Pair$$string$string` clone deep-copies two heap
/// strings. Each instantiation must resolve its OWN per-mono clone/drop pair.
fn fixture_multi_source() -> String {
    format!(
        "\
type Pair<A, B> {{ a: A; b: B; }}

fn build(seed: string) -> i64 {{
    let nums = Pair {{ a: 1, b: 2 }};
    let strs = Pair {{ a: seed + \"-a\", b: seed + \"-b\" }};
    let nums2 = clone nums;
    let strs2 = clone strs;
    nums2.a + nums2.b + strs2.a.len() + strs2.b.len()
        + nums.a + nums.b + strs.a.len() + strs.b.len()
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for i in 0..{ITERATIONS} {{
        total = total + build(\"seed\");
    }}
    if total != {expected} {{ return 75; }}
    0
}}
",
        expected = ITERATIONS * 30
    )
}

/// Control: an actor holds a generic `Pair<i64, string>` in a state field by
/// MOVE (`held = p`), no clone. Each loop iteration spawns the actor, stores,
/// and lets it go out of scope so the runtime shuts it down and drops the state
/// field. Establishes the actor-shutdown floor: the runtime-concat string
/// (`tag + "-held"`) carries the same clone-independent fresh-producer leak the
/// flat fixtures document, present here with ZERO clone.
fn control_actor_source() -> String {
    format!(
        "\
type Pair<A, B> {{ a: A; b: B; }}

actor Keeper {{
    var held: Pair<i64, string> = Pair {{ a: 0, b: \"none\" }};
    receive fn store(n: i64, tag: string) -> i64 {{
        let s2 = tag + \"-held\";
        let p = Pair {{ a: n, b: s2 }};
        held = p;
        return held.a;
    }}
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for i in 0..{ITERATIONS} {{
        let k = spawn Keeper();
        let base = \"row\";
        let tag = base + \"-x\";
        match await k.store(i, tag) {{
            Ok(n) => {{ total = total + n; }}
            Err(_) => {{ return 80; }}
        }}
    }}
    if total != {expected} {{ return 81; }}
    0
}}
",
        expected = ITERATIONS * (ITERATIONS - 1) / 2
    )
}

/// Fixture: the actor stores its state field by `clone` (`held = clone p`). The
/// per-mono clone of `Pair$$i64$string` deep-copies the heap string; its matching
/// per-mono DROP must fire when the actor shuts down and the state field is
/// released. A clone synthesised without its paired drop puts the fixture
/// `>= ITERATIONS` nodes above the move control. Against `control_actor_source`
/// the clone must add no leak beyond the construction floor.
fn fixture_actor_source() -> String {
    format!(
        "\
type Pair<A, B> {{ a: A; b: B; }}

actor Keeper {{
    var held: Pair<i64, string> = Pair {{ a: 0, b: \"none\" }};
    receive fn store(n: i64, tag: string) -> i64 {{
        let s2 = tag + \"-held\";
        let p = Pair {{ a: n, b: s2 }};
        held = clone p;
        return held.a;
    }}
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for i in 0..{ITERATIONS} {{
        let k = spawn Keeper();
        let base = \"row\";
        let tag = base + \"-x\";
        match await k.store(i, tag) {{
            Ok(n) => {{ total = total + n; }}
            Err(_) => {{ return 80; }}
        }}
    }}
    if total != {expected} {{ return 81; }}
    0
}}
",
        expected = ITERATIONS * (ITERATIONS - 1) / 2
    )
}

/// No-double-free pin: clone an owned `Pair<string, string>`, read fields from
/// BOTH the original and the clone, and let both drop in one frame. If the clone
/// shallow-copied (aliased) the string buffers instead of deep-copying, dropping
/// both originals and clone would double-free the shared buffers and crash under
/// the poisoned-allocator triple before the sentinel prints. Runs on any unix.
const NO_DOUBLE_FREE_SOURCE: &str = "\
type Pair<A, B> { a: A; b: B; }

fn main() {
    let p = Pair { a: \"alpha\", b: \"beta\" };
    let q = clone p;
    let sum = p.a.len() + p.b.len() + q.a.len() + q.b.len();
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

/// macOS + `leaks(1)` availability guard shared by every leak probe.
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

/// Compile + measure `fixture` (construct + clone) against `control` (the same
/// construction with no clone) and assert the clone added no leak beyond the
/// construction floor (within `FLOOR_TOLERANCE`). A missing per-mono drop puts
/// the fixture `>= ITERATIONS` nodes above the control.
fn assert_clone_adds_no_leak(shape_name: &str, control_source: &str, fixture_source: &str) {
    if !leaks_available(shape_name) {
        return;
    }
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("record-clone-leak-{shape_name}-"))
        .tempdir()
        .expect("tempdir");

    let control_bin = compile_to_native(control_source, dir.path(), "control");
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
        "{shape_name}: generic record clone DROP LEAK -- the clone fixture leaked \
         {fixture_leaks} nodes against the no-clone construction control's floor of \
         {control_leaks} (tolerance {FLOOR_TOLERANCE}). An excess of {} nodes means a per-mono \
         clone thunk was synthesised WITHOUT its matching per-mono drop (R1 asymmetry): the \
         clone's deep-copied buffers are never freed. The fix is the clone/drop thunk PAIR \
         emitted together per key in `emit_state_clone_drop_synthesis`, seeded by \
         `collect_record_clone_inplace_seeds` (hew-mir/src/thunk_requirements.rs). Re-run with \
         `MallocStackLogging=1 leaks --atExit -- {}` to see the leaked stack.",
        fixture_leaks.saturating_sub(control_leaks + FLOOR_TOLERANCE),
        fixture_bin.display()
    );
}

// -- oracles -----------------------------------------------------------------

/// Flat `clone Pair<string, string>`: the per-mono clone deep-copies two heap
/// strings and the matching drop frees them. Reverting the clone-seed collector
/// makes this fail to COMPILE; omitting the per-mono drop fails it by
/// `>= ITERATIONS` leaked nodes.
#[test]
fn generic_record_clone_flat_no_drop_leak() {
    assert_clone_adds_no_leak("clone_flat", &control_flat_source(), &fixture_flat_source());
}

/// Nested `clone Pair<Pair<string, string>, string>`: the clone must recurse
/// into the inner record and free every transitively cloned buffer.
#[test]
fn generic_record_clone_nested_no_drop_leak() {
    assert_clone_adds_no_leak(
        "clone_nested",
        &control_nested_source(),
        &fixture_nested_source(),
    );
}

/// Two instantiations of the same generic record cloned in one frame. Pins that
/// per-mono keying gives each its own balanced clone/drop pair (the scalar
/// `Pair$$i64$i64` and the owned `Pair$$string$string`).
#[test]
fn generic_record_clone_multi_instantiation_no_drop_leak() {
    assert_clone_adds_no_leak(
        "clone_multi",
        &control_multi_source(),
        &fixture_multi_source(),
    );
}

/// Actor-shutdown path for clone/drop symmetry: a generic aggregate cloned into
/// an actor STATE field must have its per-mono drop fire when the actor shuts
/// down. Each iteration spawns + drives + drops a `Keeper` holding a cloned
/// `Pair$$i64$string`; against the MOVE control (same construction, no clone)
/// the clone must add no leak. A per-mono clone synthesised without its paired
/// drop on the shutdown path leaks `>= ITERATIONS` heap strings here — the
/// silent unwind UAF/leak the sync oracles cannot reach.
#[test]
fn generic_record_clone_actor_shutdown_no_drop_leak() {
    assert_clone_adds_no_leak(
        "clone_actor",
        &control_actor_source(),
        &fixture_actor_source(),
    );
}

/// No-double-free pin: the clone must DEEP-COPY, not alias, the string buffers,
/// and the clone + original must each drop exactly once. A shallow-copy alias
/// would double-free under the poisoned-allocator triple before the sentinel
/// prints. Runs on any unix.
#[test]
fn generic_record_clone_release_is_exactly_once_under_malloc_scribble() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("record-clone-no-double-free-")
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
        "generic record clone release must run exactly once -- a crash here indicates the clone \
         aliased the string buffers (shallow copy) and dropping both the original and the clone \
         double-freed them;\n{}",
        describe_output(&output)
    );
    // p.a("alpha"=5) + p.b("beta"=4) + q.a(5) + q.b(4) = 18, then the `OK` sentinel.
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        "18OK",
        "output must be untouched -- a scribbled value indicates an over-drop corrupted a live \
         string;\n{}",
        describe_output(&output)
    );
}
