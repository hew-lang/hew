//! Ownership oracle for a DIVERGENT-ARM VALUE SELECTION — `let out = match c
//! { true => a, false => b };` and its family.
//!
//! One branch-join slot is written by two mutually-exclusive arms. It ends up
//! holding exactly ONE of the sources; every other source still owns a live
//! allocation. Before the fix the join slot was reachable from two distinct
//! whole-value alias roots, so it was evicted as conflicted and each arm's
//! `Move` read as an ownership escape — which excluded EVERY arm source from
//! the scope-exit drop set. The join slot's owner freed the selected value and
//! the losing arm's value leaked: 96 leaks / 6912 bytes over 48 calls for
//! `Vec<i64>` (one whole Vec per call — 96 B handle + 48 B buffer), with no
//! `clone` anywhere.
//!
//! Both directions are pinned for every shape, and both must hold:
//!
//! * NO UNDER-RELEASE — the per-iteration leak slope must stay flat. This is
//!   the direction the defect failed. A single-shot measurement would not
//!   separate a per-call leak from baseline noise; the slope does.
//! * NO OVER-RELEASE — the fix keeps the arm sources in the scope-exit drop set
//!   and relies on the transferred slot being NULLED, so the release walks a
//!   null on the path that transferred. If the null were missing, a second free
//!   of the join slot's allocation reaches the runtime's own double-free guard,
//!   which aborts the process. That guard is portable, so the over-release
//!   assertion is a plain bounded run on EVERY platform. A macOS-only sibling
//!   repeats it under the poisoned allocator, which additionally turns a
//!   use-after-free READ of freed storage into an abort rather than a silent
//!   read of stale bytes.
//!
//! The shapes cover the CLASS, not one syntax:
//!
//! * `match` and `if`/`else` value arms (the `if` form copies through a per-arm
//!   temp, so its join slot's direct sources are temps);
//! * a NESTED selection, where the outer join slot's sources are one owned
//!   local and the inner join slot;
//! * the selection in RETURN position, where the join slot is handed to the
//!   caller and only the losing arm is this frame's to release;
//! * a selection whose sibling arm DIVERGES (`return` from one arm), where the
//!   early exit still owns the un-transferred source;
//! * an arm producing a FRESH value rather than naming a local;
//! * the leaf classes that reach different drop provers: plain `Vec<i64>`,
//!   owned-element `Vec<string>`, a `HashMap` handle, and a record whose field
//!   owns a Vec (the composite `RecordInPlace` release).
//!
//! `string` and `Rc<T>` selections were measured over the same shapes and were
//! already flat: their provers do not take the whole-value alias route.
//!
//! The platform-independent half of this coverage — which instruction is
//! emitted where, and which drops each return exit plans — is in
//! `hew-mir/tests/lowering_expr/divergent_selection_transfer.rs` and runs on
//! every host.

#![cfg(unix)]

mod support;

use std::path::PathBuf;
use std::process::{Command, Stdio};

use support::leak_slope::{
    assert_frame_slope_below_tolerance_exact_lines, compile_to_native, require_leaks_tool,
    run_under_malloc_scribble,
};
use support::{describe_output, require_codegen, run_bounded_command};

/// One frame prints exactly one line, so the slope harness pins the drained
/// iteration count instead of settling for monotonicity.
fn expected_lines(frames: usize) -> usize {
    frames
}

const MAKE_VEC: &str = r"
fn make() -> Vec<i64> {
    let v: Vec<i64> = Vec.new();
    v.push(40);
    v.push(2);
    return v;
}
";

const MAKE_STRINGS: &str = r#"
fn make() -> Vec<string> {
    let v: Vec<string> = Vec.new();
    v.push("per-call-element-one");
    v.push("per-call-element-two");
    return v;
}
"#;

const MAKE_MAP: &str = r#"
fn make() -> HashMap<string, i64> {
    let m: HashMap<string, i64> = HashMap.new();
    m.insert("alpha-key-one", 1);
    m.insert("beta-key-two", 2);
    return m;
}
"#;

/// `main` alternates the selector so BOTH arms are exercised within one probe,
/// and prints one line per frame for the work witness.
fn driver(frames: usize) -> String {
    format!(
        r#"
fn main() -> i64 {{
    var total: i64 = 0;
    for seed in 0..{frames} {{
        total = total + probe(seed % 2 == 0);
        println("frame");
    }}
    match total >= 0 {{
        true => 0,
        false => 1,
    }}
}}
"#
    )
}

fn match_arms_source(frames: usize) -> String {
    format!(
        "{MAKE_VEC}
fn probe(c: bool) -> i64 {{
    let a = make();
    let b = make();
    let out = match c {{ true => a, false => b }};
    out.len()
}}
{}",
        driver(frames)
    )
}

fn if_else_arms_source(frames: usize) -> String {
    format!(
        "{MAKE_VEC}
fn probe(c: bool) -> i64 {{
    let a = make();
    let b = make();
    let out = if c {{ a }} else {{ b }};
    out.len()
}}
{}",
        driver(frames)
    )
}

fn nested_arms_source(frames: usize) -> String {
    format!(
        "{MAKE_VEC}
fn probe(c: bool) -> i64 {{
    let a = make();
    let b = make();
    let d = make();
    let out = match c {{
        true => a,
        false => match d.len() > 1 {{ true => b, false => d }},
    }};
    out.len()
}}
{}",
        driver(frames)
    )
}

/// The join slot leaves the frame. The caller owns what it receives, so only
/// the LOSING arm is this frame's to release.
fn return_position_source(frames: usize) -> String {
    format!(
        "{MAKE_VEC}
fn pick(c: bool) -> Vec<i64> {{
    let a = make();
    let b = make();
    match c {{ true => a, false => b }}
}}

fn probe(c: bool) -> i64 {{
    let out = pick(c);
    out.len()
}}
{}",
        driver(frames)
    )
}

/// One arm diverges, so the join slot has a single source — but the early exit
/// still owns the un-transferred local and must release it there.
fn diverging_arm_source(frames: usize) -> String {
    format!(
        "{MAKE_VEC}
fn probe(c: bool) -> i64 {{
    let a = make();
    let b = make();
    let out = match c {{
        true => a,
        false => {{ return b.len(); }}
    }};
    out.len()
}}
{}",
        driver(frames)
    )
}

/// One arm names a local, the other produces a fresh value: the join slot still
/// has two distinct sources and the named local still owes a release on the arm
/// it did not win.
fn fresh_arm_source(frames: usize) -> String {
    format!(
        "{MAKE_VEC}
fn probe(c: bool) -> i64 {{
    let a = make();
    let out = match c {{ true => a, false => make() }};
    out.len()
}}
{}",
        driver(frames)
    )
}

fn hashmap_source(frames: usize) -> String {
    format!(
        "{MAKE_MAP}
fn probe(c: bool) -> i64 {{
    let a = make();
    let b = make();
    let out = match c {{ true => a, false => b }};
    out.len()
}}
{}",
        driver(frames)
    )
}

/// Owned-element `Vec<string>`: the losing arm's release must walk the
/// runtime's per-element string free as well as the buffer.
fn owned_element_vec_source(frames: usize) -> String {
    format!(
        "{MAKE_STRINGS}
fn probe(c: bool) -> i64 {{
    let a = make();
    let b = make();
    let out = match c {{ true => a, false => b }};
    out.len()
}}
{}",
        driver(frames)
    )
}

/// A record VALUE whose field owns a Vec. Its release is the composite
/// `RecordInPlace` thunk, a different drop prover from the leaf handle classes.
fn record_field_source(frames: usize) -> String {
    format!(
        "{MAKE_VEC}
type Holder {{ items: Vec<i64>; }}

fn probe(c: bool) -> i64 {{
    let a = Holder {{ items: make() }};
    let b = Holder {{ items: make() }};
    let out = match c {{ true => a, false => b }};
    out.items.len()
}}
{}",
        driver(frames)
    )
}

/// NEGATIVE CONTROL for the over-release direction: the arm source is READ
/// after the selection, so it must keep its bits. `probe` returns
/// `out.len() + a.len()` = 4 on the arm that selected `a`; a nulled slot reads
/// a zero-length (or faults) and the total no longer matches.
fn source_read_after_selection_source(frames: usize) -> String {
    format!(
        "{MAKE_VEC}
fn probe(c: bool) -> i64 {{
    let a = make();
    let b = make();
    let out = match c {{ true => a, false => b }};
    out.len() + a.len()
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for seed in 0..{frames} {{
        total = total + probe(true);
        println(\"frame\");
    }}
    total - {frames} * 4
}}
"
    )
}

/// Compile the shape at a fixed frame count and return the probe binary. The
/// tempdir is returned alongside so it outlives the caller's run.
fn compile_probe(name: &str, source_fn: fn(usize) -> String) -> (tempfile::TempDir, PathBuf) {
    let dir = tempfile::Builder::new()
        .prefix(&format!("divergent-selection-{name}-"))
        .tempdir()
        .expect("tempdir");
    let binary = compile_to_native(&source_fn(16), dir.path(), name);
    (dir, binary)
}

const OVER_RELEASE_EXPLANATION: &str =
    "a divergent-arm selection transfers ONE allocation into the join slot; the source's retained \
     scope-exit release must walk a nulled slot on that path, not free the value a second time";

/// The PORTABLE over-release half: a plain bounded run, on every platform.
///
/// A second release of the join slot's allocation reaches the runtime's own
/// double-free guard, which aborts the process, so a probe that exits under its
/// own control with status 0 has proved the release count is not too high. This
/// is the assertion that runs on the Linux and FreeBSD lanes, not only on a
/// macOS box.
fn assert_shape_does_not_over_release(name: &str, source_fn: fn(usize) -> String) {
    require_codegen();
    let (_dir, binary) = compile_probe(name, source_fn);
    let mut command = Command::new(&binary);
    command.stdout(Stdio::piped()).stderr(Stdio::piped());
    let run = run_bounded_command(command, name.to_string());
    assert!(
        run.status.success(),
        "{name}: {OVER_RELEASE_EXPLANATION}:\n{}",
        describe_output(&run)
    );
}

/// The macOS-only sharpening of the same assertion.
///
/// `MallocScribble` / `MallocPreScribble` / `MallocGuardEdges` are Darwin
/// libmalloc facilities: they turn the WEAKER failure — a release that frees
/// storage the join slot's owner then reads — into an abort as well, where the
/// plain run above would read intact bytes and pass.
fn assert_shape_does_not_over_release_under_poisoned_allocator(
    name: &str,
    source_fn: fn(usize) -> String,
) {
    require_codegen();
    let (_dir, binary) = compile_probe(name, source_fn);
    let run = run_under_malloc_scribble(&binary);
    assert!(
        run.status.success(),
        "{name}: {OVER_RELEASE_EXPLANATION}, and the poisoned allocator additionally rejects a \
         read of storage a premature release already freed:\n{}",
        describe_output(&run)
    );
}

/// The UNDER-release half: the per-iteration leak slope must stay flat.
fn assert_shape_does_not_under_release(name: &str, source_fn: fn(usize) -> String) {
    require_codegen();
    require_leaks_tool();
    assert_frame_slope_below_tolerance_exact_lines(name, source_fn, expected_lines);
}

/// Three assertions per shape. The over-release assertion runs EVERYWHERE (the
/// runtime's double-free guard is the portable detector); its poisoned-allocator
/// sibling and the leak-slope half need Darwin facilities and record a counted
/// SKIP elsewhere.
macro_rules! selection_shape {
    ($over:ident, $poisoned:ident, $under:ident, $name:literal, $source:ident) => {
        #[test]
        fn $over() {
            assert_shape_does_not_over_release($name, $source);
        }

        #[cfg_attr(
            not(target_os = "macos"),
            ignore = "poisoned allocator is macOS-only; absence must be a counted skip"
        )]
        #[test]
        fn $poisoned() {
            assert_shape_does_not_over_release_under_poisoned_allocator($name, $source);
        }

        #[cfg_attr(
            not(target_os = "macos"),
            ignore = "leak oracle needs macOS `leaks(1)`; absence must be a counted skip"
        )]
        #[test]
        fn $under() {
            assert_shape_does_not_under_release($name, $source);
        }
    };
}

selection_shape!(
    match_arms_does_not_over_release,
    match_arms_does_not_over_release_under_poisoned_allocator,
    match_arms_does_not_under_release,
    "match_arms",
    match_arms_source
);

selection_shape!(
    if_else_arms_does_not_over_release,
    if_else_arms_does_not_over_release_under_poisoned_allocator,
    if_else_arms_does_not_under_release,
    "if_else_arms",
    if_else_arms_source
);

selection_shape!(
    nested_arms_does_not_over_release,
    nested_arms_does_not_over_release_under_poisoned_allocator,
    nested_arms_does_not_under_release,
    "nested_arms",
    nested_arms_source
);

selection_shape!(
    return_position_does_not_over_release,
    return_position_does_not_over_release_under_poisoned_allocator,
    return_position_does_not_under_release,
    "return_position",
    return_position_source
);

selection_shape!(
    diverging_arm_does_not_over_release,
    diverging_arm_does_not_over_release_under_poisoned_allocator,
    diverging_arm_does_not_under_release,
    "diverging_arm",
    diverging_arm_source
);

selection_shape!(
    fresh_arm_does_not_over_release,
    fresh_arm_does_not_over_release_under_poisoned_allocator,
    fresh_arm_does_not_under_release,
    "fresh_arm",
    fresh_arm_source
);

selection_shape!(
    hashmap_handle_does_not_over_release,
    hashmap_handle_does_not_over_release_under_poisoned_allocator,
    hashmap_handle_does_not_under_release,
    "hashmap_handle",
    hashmap_source
);

selection_shape!(
    owned_element_vec_does_not_over_release,
    owned_element_vec_does_not_over_release_under_poisoned_allocator,
    owned_element_vec_does_not_under_release,
    "owned_element_vec",
    owned_element_vec_source
);

selection_shape!(
    record_field_does_not_over_release,
    record_field_does_not_over_release_under_poisoned_allocator,
    record_field_does_not_under_release,
    "record_field",
    record_field_source
);

selection_shape!(
    source_read_after_selection_does_not_over_release,
    source_read_after_selection_does_not_over_release_under_poisoned_allocator,
    source_read_after_selection_does_not_under_release,
    "read_after_selection",
    source_read_after_selection_source
);
