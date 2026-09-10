//! Ownership oracle for `Rc` members under structural clone.
//!
//! An `Rc` is an ordinary owned member: a heap container holding `Rc` elements
//! and a value aggregate holding one (tuple element, `Option` payload, record
//! field) both retain on ingress and release with their owner. Releasing a
//! shared handle once too often trips the runtime's `Rc double-free` guard and
//! kills the process, so a clean exit is the over-release oracle; the
//! `leaks(1)` measurements below are the under-release half.

#![cfg(unix)]

mod support;

use std::path::PathBuf;
use std::process::Command;

use support::leak_slope::{measure_leaks_exact, require_leaks_tool};
use support::{describe_output, hew_binary, repo_root, require_codegen};

const ITERATIONS: usize = 48;

/// A `Vec<Rc<Node>>` round trip whose `body` decides which of the two owners —
/// the clone or the original — survives the frame.
fn vec_of_rc_source(body: &str) -> String {
    format!(
        "\
type Node {{ value: i64, }}

fn make(seed: i64) -> Vec<Rc<Node>> {{
    var holders: Vec<Rc<Node>> = Vec.new();
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
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)`; absence must be a counted skip"
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

/// A value aggregate holding an `Rc` clones and releases in balance: the
/// ingress retain gives the aggregate its own strong reference, and the
/// composite drop gives it back. A double-free aborts, so exit 0 is the proof.
fn assert_value_aggregate_rc_member_balances(name: &str, source: &str) {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix(&format!("rc-member-balance-{name}-"))
        .tempdir()
        .expect("tempdir");
    let binary = compile_to_native(source, dir.path(), name);
    let run = Command::new(&binary)
        .current_dir(repo_root())
        .output()
        .expect("invoke compiled Rc-member program");
    assert!(
        run.status.success(),
        "{name}: cloning a value aggregate holding an `Rc` must not over-release it \
         (an `Rc double-free` aborts the process):\n{}",
        describe_output(&run)
    );
}

#[test]
fn tuple_with_rc_member_clone_balances() {
    assert_value_aggregate_rc_member_balances("tuple_rc", TUPLE_RC_MEMBER);
}

#[test]
fn option_with_rc_payload_clone_balances() {
    assert_value_aggregate_rc_member_balances("option_rc", OPTION_RC_PAYLOAD);
}

#[test]
fn record_with_rc_field_clone_balances() {
    assert_value_aggregate_rc_member_balances("record_rc", RECORD_RC_FIELD);
}
