//! End-to-end truth check for the `needless_range_loop` suggestion.
//!
//! `needless_range_loop` tells a user to replace `for i in 0..xs.len()` with
//! `for x in xs`. Direct Vec iteration goes through `VecIter::next`, which
//! clones each element into an independent owner, so the rewrite only compiles
//! when the element type has a semantic clone. Inside a generic template that
//! is a question about the *bound*: `count_all<T>(values: Vec<T>)` compiles in
//! its indexed form for a `#[resource]` instantiation, while the suggested
//! direct form fails that same instantiation at MIR with
//! `MIR lowering for VecIter<Handle> clone-out is not implemented yet`. An
//! unbounded parameter therefore gets no suggestion; `T: Clone` proves the
//! element clones and does.

mod support;

use std::path::Path;
use std::process::Command;

use support::{
    describe_output, hew_binary, repo_root, require_codegen, run_bounded_command, strip_ansi,
};

const DIRECT_ITERATION_HELP: &str = "iterate the collection directly";

fn fixture(name: &str) -> std::path::PathBuf {
    repo_root()
        .join("hew-cli/tests/fixtures/diagnostic_suggestions")
        .join(name)
}

fn check_fixture(name: &str) -> std::process::Output {
    Command::new(hew_binary())
        .args([
            "check",
            fixture(name).to_str().expect("fixture path must be UTF-8"),
        ])
        .current_dir(repo_root())
        .output()
        .expect("hew check must run")
}

fn check_stderr(name: &str) -> String {
    let output = check_fixture(name);
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        output.status.success(),
        "`{name}` must type-check:\n{stderr}"
    );
    stderr
}

/// Build `name` into `dir` and run it, asserting both succeed. This is the
/// executable half of suggestion truth: a suggested rewrite is only truthful if
/// the compiler accepts it all the way through codegen, not just through the
/// checker.
fn build_and_run(name: &str, dir: &Path, stem: &str) {
    let build = Command::new(hew_binary())
        .args([
            "build",
            fixture(name).to_str().expect("fixture path must be UTF-8"),
            "-o",
            dir.join(stem).to_str().expect("output path must be UTF-8"),
        ])
        .current_dir(dir)
        .output()
        .expect("hew build must run");
    assert!(
        build.status.success(),
        "`{name}` must compile:\n{}",
        describe_output(&build)
    );

    let binary = dir.join(stem);
    let run = run_bounded_command(Command::new(&binary), format!("run {}", binary.display()));
    assert!(
        run.status.success(),
        "`{name}` must run:\n{}",
        describe_output(&run)
    );
}

#[test]
fn local_pid_range_loop_has_no_uncompilable_direct_iteration_suggestion() {
    let stderr = check_stderr("local_pid_indexed_broadcast.hew");
    assert!(
        !stderr.contains(DIRECT_ITERATION_HELP),
        "the lint must not suggest unsupported VecIter ownership semantics:\n{stderr}"
    );
}

#[test]
fn unbounded_generic_range_loop_has_no_direct_iteration_suggestion() {
    let stderr = check_stderr("unbounded_generic_indexed.hew");
    assert!(
        !stderr.contains(DIRECT_ITERATION_HELP),
        "an unbounded `T` does not prove the element clones, so the rewrite is not \
         guaranteed to compile at every monomorphisation:\n{stderr}"
    );
}

#[test]
fn clone_bounded_generic_range_loop_suggests_direct_iteration() {
    let stderr = check_stderr("clone_bounded_generic_indexed.hew");
    assert!(
        stderr.contains(DIRECT_ITERATION_HELP),
        "`T: Clone` proves the element clones, so the lint must still fire:\n{stderr}"
    );
}

/// The suggestion emitted for the `T: Clone` template above, written out, is a
/// program the compiler accepts and runs.
#[test]
fn clone_bounded_generic_direct_iteration_compiles_and_runs() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("suggestion-truth-")
        .tempdir_in(repo_root())
        .expect("temp dir");
    build_and_run(
        "clone_bounded_generic_direct.hew",
        dir.path(),
        "clone_bounded_generic_direct",
    );
}
