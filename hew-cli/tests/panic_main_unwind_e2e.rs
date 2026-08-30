//! `panic()` in main context unwinds with cleanup (hew-lang/hew#3074).
//!
//! The vertical-slice fixture carries the same contract, but that harness is
//! fail-fast and stops at the first unrelated regression, so the drop proof
//! lives here too where it runs on its own.
//!
//! What the observation rests on: `Conn` is a `#[resource]` type whose `close`
//! prints, and it is live when `main` panics. Its output can only appear if the
//! panic unwound through the MIR-authored cleanup pad. Before the unification
//! the runtime exited from `hew_panic` and the line was missing.
//!
//! The boundary exists only where cleanup is structured LLVM unwinding. On a
//! crash-owner-registry target - wasm32 and windows-msvc, which Inkwell cannot
//! give valid MSVC EH funclets - `hew_panic` still exits the process directly
//! and no scope exit runs, exactly as every other `invoke` cleanup edge is
//! absent there. Rather than skip, the test pins that behaviour too, so the day
//! the boundary reaches those targets this test fails and gets updated.

mod support;

use std::path::Path;

use hew_codegen_rs::{
    cleanup_capabilities_for_target, native_emission_triple, CleanupUnwindStrategy,
};

use support::{describe_output, repo_root, require_codegen, run_bounded_hew_run, tempdir};

/// The cleanup mechanism codegen will pick for the binary this test runs.
///
/// Asks the production authority about the triple codegen actually emits for,
/// rather than reading `cfg!(windows)` as a stand-in for it.
fn native_unwind_strategy() -> CleanupUnwindStrategy {
    cleanup_capabilities_for_target(&native_emission_triple()).unwind_strategy
}

/// `main` panics with a live `#[resource]` binding.
const PANIC_WITH_LIVE_RESOURCE: &str = r#"
#[resource]
type Conn {
    fd: i64
}

impl Conn {
    fn close(c: Conn) {
        print(c.fd);
        println("");
    }
}

fn main() -> i64 {
    let c: Conn = Conn { fd: 7 };
    println("work");
    panic("boom");
    0
}
"#;

/// The same program without the panic. Counterfactual for the close output:
/// it proves `7` marks a scope exit that ran, not a line the fixture prints
/// unconditionally, and it pins the status difference to the panic alone.
const RETURNS_WITH_LIVE_RESOURCE: &str = r#"
#[resource]
type Conn {
    fd: i64
}

impl Conn {
    fn close(c: Conn) {
        print(c.fd);
        println("");
    }
}

fn main() -> i64 {
    let c: Conn = Conn { fd: 7 };
    println("work");
    0
}
"#;

fn write_source(dir: &Path, name: &str, source: &str) -> std::path::PathBuf {
    let path = dir.join(name);
    std::fs::write(&path, source).expect("write fixture source");
    path
}

#[test]
fn main_context_panic_runs_the_live_resource_close_and_keeps_the_panic_status() {
    require_codegen();
    let dir = tempdir();
    let source = write_source(
        dir.path(),
        "panic_live_resource.hew",
        PANIC_WITH_LIVE_RESOURCE,
    );

    let output = run_bounded_hew_run(&source, repo_root());

    assert_eq!(
        output.status.code(),
        Some(101),
        "a main-context panic keeps its documented status; {}",
        describe_output(&output)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    match native_unwind_strategy() {
        CleanupUnwindStrategy::StructuredLlvm => assert_eq!(
            stdout.replace("\r\n", "\n"),
            "work\n7\n",
            "the live resource's close must run during the unwind, after the body's own \
             output; {}",
            describe_output(&output)
        ),
        // WHY: a crash-owner-registry target emits no landing pads at all, so
        // `hew_panic` exits from where it stands and the live `Conn` is
        // reclaimed by the OS rather than by its `close`.
        // WHEN OBSOLETE: when that target gains executable unwind cleanup -
        // MSVC EH funclets through Inkwell, or a registry sweep that runs
        // scope exits on the panic path.
        // WHAT THE REAL FIX IS: extend the boundary to that target, at which
        // point this arm starts failing and both arms collapse into the
        // structured one above.
        CleanupUnwindStrategy::CrashOwnerRegistry => assert_eq!(
            stdout.replace("\r\n", "\n"),
            "work\n",
            "without structured unwind cleanup the close cannot run, and nothing else may \
             print in its place; {}",
            describe_output(&output)
        ),
    }

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("boom"),
        "the panic message belongs on stderr; {}",
        describe_output(&output)
    );
    assert!(
        !stderr.contains("panicked at"),
        "the unwind is Hew's, so Rust's default panic hook must stay silent; {}",
        describe_output(&output)
    );
}

#[test]
fn the_same_program_without_a_panic_exits_clean_with_the_same_close_output() {
    require_codegen();
    let dir = tempdir();
    let source = write_source(
        dir.path(),
        "return_live_resource.hew",
        RETURNS_WITH_LIVE_RESOURCE,
    );

    let output = run_bounded_hew_run(&source, repo_root());

    assert_eq!(
        output.status.code(),
        Some(0),
        "the counterfactual returns normally; {}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout.replace("\r\n", "\n"),
        "work\n7\n",
        "the normal-return close output is what the panic path must match; {}",
        describe_output(&output)
    );
}
