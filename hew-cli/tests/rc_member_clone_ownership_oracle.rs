//! Ownership oracle for `Rc` members under structural clone.
//!
//! Two halves, and both must hold:
//!
//! 1. An `Rc` sitting in a VALUE aggregate (tuple element, `Option`/`Result`
//!    payload, record field) is refused at check time. Aggregate ingress of an
//!    `Rc` emits no retain, while both the source binder and the aggregate's
//!    composite drop release the handle, so the inverse drop plan over-releases
//!    and the program aborts with `Rc double-free`. Until the ingress retain
//!    exists, the checker fails closed — and it must do so before any native
//!    artifact is written.
//! 2. The shape that IS admitted — a heap container holding `Rc` elements,
//!    which clones through the owned-element thunk — does not over-release in
//!    either drop order: it runs to completion, and a double-free aborts the
//!    process, so a clean exit is itself the over-release oracle. The
//!    under-release (leak) half of that same shape is a measured, tracked
//!    defect shared with `origin/main`; see the ignored pins at the bottom.

#![cfg(unix)]

mod support;

use std::path::PathBuf;
use std::process::Command;

use support::leak_slope::{measure_leaks_exact, require_leaks_tool};
use support::{describe_output, hew_binary, repo_root, require_codegen, strip_ansi};

const ITERATIONS: usize = 48;

/// A `Vec<Rc<Node>>` round trip whose `body` decides which of the two owners —
/// the clone or the original — survives the frame.
fn vec_of_rc_source(body: &str) -> String {
    format!(
        "\
type Node {{ value: i64, }}

fn make(seed: i64) -> Vec<Rc<Node>> {{
    let holders: Vec<Rc<Node>> = Vec.new();
    holders.push(Rc.new(Node {{ value: seed }}));
{body}
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for seed in 0..{ITERATIONS} {{
        let kept = make(seed);
        total = total + kept.len();
    }}
    match total == {ITERATIONS} {{
        true => 0,
        false => 71,
    }}
}}
"
    )
}

fn keep_clone_source() -> String {
    vec_of_rc_source(
        "\
    let copy = clone holders;
    match copy.len() == 1 {
        true => copy,
        false => holders,
    }",
    )
}

fn keep_original_source() -> String {
    vec_of_rc_source(
        "\
    let copy = clone holders;
    match copy.len() == 1 {
        true => holders,
        false => copy,
    }",
    )
}

fn compile_to_native(source: &str, dir: &std::path::Path, name: &str) -> PathBuf {
    let hew_src = dir.join(format!("{name}.hew"));
    std::fs::write(&hew_src, source).expect("write Hew source");
    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--emit-dir",
            dir.to_str().expect("emit-dir utf-8"),
            hew_src.to_str().expect("Hew source utf-8"),
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
    let binary = stdout
        .lines()
        .find_map(|line| line.strip_prefix("native: "))
        .unwrap_or_else(|| panic!("no native output for {name}:\n{stdout}"));
    PathBuf::from(binary)
}

/// Compile and run the admitted shape.
///
/// This is the over-release oracle, and it is not cosmetic: releasing a shared
/// handle once too often trips the runtime's `Rc double-free` guard and kills
/// the process, so a program that exits 0 has proved the release count is not
/// too high. The under-release half is measured separately below.
fn assert_vec_of_rc_drop_order_does_not_over_release(
    name: &str,
    source: &str,
) -> (tempfile::TempDir, PathBuf) {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix(&format!("rc-member-clone-{name}-"))
        .tempdir()
        .expect("tempdir");
    let binary = compile_to_native(source, dir.path(), name);

    let run = Command::new(&binary)
        .current_dir(repo_root())
        .output()
        .expect("invoke compiled Rc-element program");
    assert!(
        run.status.success(),
        "{name}: cloning a `Vec<Rc<Node>>` must not over-release the shared handle \
         (an `Rc double-free` aborts the process):\n{}",
        describe_output(&run)
    );
    // The emit directory is returned so a caller that measures the binary keeps
    // it alive; dropping it here would delete the artifact under `leaks(1)`.
    (dir, binary)
}

#[test]
fn vec_of_rc_dropping_original_before_returned_clone_does_not_over_release() {
    let _kept =
        assert_vec_of_rc_drop_order_does_not_over_release("keep_clone", &keep_clone_source());
}

#[test]
fn vec_of_rc_dropping_clone_before_returned_original_does_not_over_release() {
    let _kept =
        assert_vec_of_rc_drop_order_does_not_over_release("keep_original", &keep_original_source());
}

/// The under-release half of the same shape.
///
/// TRACKED DEFECT, measured not assumed: on this branch AND on `origin/main`
/// this program reports `144 leaks for 10752 total leaked bytes` over its 48
/// iterations, while the identical program without the `clone` reports
/// `0 leaks for 0 total leaked bytes`. It is the mirror of the value-aggregate
/// over-release these tests otherwise pin: `Rc` has no aggregate-ingress retain
/// derivation, so its clone/drop accounting is wrong in BOTH directions
/// depending on which side of the ingress the extra owner lands on.
///
/// WHY ignored rather than asserted at the observed count: pinning 144 would
/// ratchet the defect into the suite. The exact-zero assertion is the target,
/// and it flips green the moment `Rc`/`Weak` gain the ingress retain that
/// `StringRetain` already has — at which point these two tests lose the
/// `#[ignore]` and the checker's value-aggregate refusal is deleted with them.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)`; absence must be a counted skip"
)]
#[cfg_attr(
    target_os = "macos",
    ignore = "tracked: `Rc` has no aggregate-ingress retain, so `clone Vec<Rc<T>>` \
              under-releases (144 leaks / 10752 bytes here and on origin/main)"
)]
#[test]
fn vec_of_rc_dropping_original_before_returned_clone_leaks_nothing() {
    let (_dir, binary) =
        assert_vec_of_rc_drop_order_does_not_over_release("keep_clone_leaks", &keep_clone_source());
    require_leaks_tool();
    assert_eq!(
        measure_leaks_exact(&binary),
        (0, 0),
        "cloning a `Vec<Rc<Node>>` must release every handle exactly once"
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)`; absence must be a counted skip"
)]
#[cfg_attr(
    target_os = "macos",
    ignore = "tracked: `Rc` has no aggregate-ingress retain, so `clone Vec<Rc<T>>` \
              under-releases (144 leaks / 10752 bytes here and on origin/main)"
)]
#[test]
fn vec_of_rc_dropping_clone_before_returned_original_leaks_nothing() {
    let (_dir, binary) = assert_vec_of_rc_drop_order_does_not_over_release(
        "keep_original_leaks",
        &keep_original_source(),
    );
    require_leaks_tool();
    assert_eq!(
        measure_leaks_exact(&binary),
        (0, 0),
        "cloning a `Vec<Rc<Node>>` must release every handle exactly once"
    );
}

const TUPLE_RC_MEMBER: &str = r#"
type Node { value: i64, }

fn main() -> i64 {
    let shared: Rc<Node> = Rc.new(Node { value: 7 });
    let pair: (Rc<Node>, string) = (shared, "tag");
    let _copied = clone pair;
    0
}
"#;

const OPTION_RC_PAYLOAD: &str = r"
type Node { value: i64, }

fn main() -> i64 {
    let shared: Rc<Node> = Rc.new(Node { value: 7 });
    let held: Option<Rc<Node>> = Some(shared);
    let _copied = clone held;
    0
}
";

const RECORD_RC_FIELD: &str = r#"
type Node { value: i64, }
type Holder { r: Rc<Node>, tag: string, }

fn main() -> i64 {
    let shared: Rc<Node> = Rc.new(Node { value: 7 });
    let holder = Holder { r: shared, tag: "tag" };
    let _copied = clone holder;
    0
}
"#;

fn assert_value_aggregate_rc_member_is_refused(name: &str, source: &str, member: &str) {
    let dir = tempfile::Builder::new()
        .prefix(&format!("rc-member-refusal-{name}-"))
        .tempdir()
        .expect("tempdir");
    let hew_src = dir.path().join(format!("{name}.hew"));
    std::fs::write(&hew_src, source).expect("write Hew source");
    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--emit-dir",
            dir.path().to_str().expect("emit-dir utf-8"),
            hew_src.to_str().expect("Hew source utf-8"),
        ])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile");
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        !output.status.success(),
        "{name}: an `Rc` in a value aggregate has no balanced clone/drop plan and must be \
         refused:\n{stderr}"
    );
    assert!(
        stderr.contains(&format!("member `{member}` of type `Rc<Node>`"))
            && stderr.contains("no aggregate-ingress retain"),
        "{name}: the refusal must name the offending member and the mechanism:\n{stderr}"
    );
    assert!(
        !dir.path().join(name).exists(),
        "{name}: refusal must happen before a native artifact is emitted"
    );
}

#[test]
fn tuple_with_rc_member_clone_is_refused_before_codegen() {
    assert_value_aggregate_rc_member_is_refused("tuple_rc", TUPLE_RC_MEMBER, "0");
}

#[test]
fn option_with_rc_payload_clone_is_refused_before_codegen() {
    assert_value_aggregate_rc_member_is_refused("option_rc", OPTION_RC_PAYLOAD, "Some");
}

#[test]
fn record_with_rc_field_clone_is_refused_before_codegen() {
    assert_value_aggregate_rc_member_is_refused("record_rc", RECORD_RC_FIELD, "r");
}
