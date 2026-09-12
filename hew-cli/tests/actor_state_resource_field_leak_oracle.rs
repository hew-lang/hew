//! An opaque resource in actor state closes exactly once at teardown.
//!
//! Native output and the Darwin allocator oracles exercise a real deque
//! allocation. Actor-state replacement is covered by the native acceptance
//! case `actor-resource-replacement`, including ASan/LSan execution.

#![cfg(unix)]

mod support;

use support::leak_slope::{measure_leaks_exact, require_leaks_tool};

use std::path::{Path, PathBuf};
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

// ── fixtures ────────────────────────────────────────────────────────────────

/// Shared prelude: a `#[resource] #[opaque]` handle backed by the runtime deque
/// (a real global-heap box), whose `close(self)` frees it and prints `closed`.
const PRELUDE: &str = "\
#[resource]\n\
#[opaque]\n\
type Dq {}\n\
impl Dq {\n\
    fn close(consume self) { unsafe { hew_deque_free(self) }; println(\"closed\"); }\n\
}\n\
extern \"C\" {\n\
    fn hew_deque_new() -> Dq;\n\
    fn hew_deque_free(consume dq: Dq);\n\
}\n";

fn src(body: &str) -> String {
    format!("{PRELUDE}{body}")
}

/// Positive control: a `var dq: Dq` state field that is NEVER reassigned. The
/// actor closes it exactly once at teardown. Deterministic stdout: `done` then
/// `closed`.
const POSITIVE_BODY: &str = "\
actor Keeper {\n\
    var dq: Dq,\n\
    receive fn ping() -> i64 { 1 }\n\
}\n\
fn main() -> i64 {\n\
    let k = spawn Keeper(dq: unsafe { hew_deque_new() });\n\
    let _ = select { reply from k.ping() => 1, after 50ms => 0 };\n\
    println(\"done\");\n\
    0\n\
}\n";
const POSITIVE_EXPECTED: &str = "done\nclosed\n";

// ── plumbing ──────────────────────────────────────────────────────────────────

/// Compile `source` to a native binary; assert the compile succeeds and return
/// the binary path.
fn compile_to_native(source: &str, dir: &Path, name: &str) -> PathBuf {
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

// ── positive control (the working path the gate must not break) ───────────────

/// A never-reassigned `var dq: Dq` state field closes exactly once at teardown.
#[test]
fn unreassigned_resource_field_closes_once() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("actor-state-pos-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(&src(POSITIVE_BODY), dir.path(), "pos");
    let output = Command::new(&bin).output().expect("run positive binary");
    assert!(
        output.status.success(),
        "never-reassigned resource-field actor must run clean;\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        POSITIVE_EXPECTED,
        "an actor whose `#[resource]` state field is never reassigned must still \
         run `close(self)` EXACTLY once at teardown (a `var` field that is merely \
         mutable must not trip the overwrite gate);\n{}",
        describe_output(&output)
    );
}

/// The positive control runs clean under the poisoned-allocator triple — a
/// double-free (close + shutdown-drop both freeing) would abort here.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn unreassigned_resource_field_no_double_free_under_malloc_scribble() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("actor-state-df-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(&src(POSITIVE_BODY), dir.path(), "pos");
    let output = Command::new(&bin)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .output()
        .expect("run positive binary under poisoned allocator");
    assert!(
        output.status.success(),
        "never-reassigned resource-field actor must run clean under the poisoned \
         allocator — an abort here is a double-free of the deque handle;\n{}",
        describe_output(&output)
    );
}

/// The positive control leaks nothing: `0 leaks for 0 total leaked bytes` over a
/// genuine `hew_deque_new` heap box (non-vacuous — an un-run close would leak it).
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn unreassigned_resource_field_zero_leaks_exact() {
    require_leaks_tool();
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("actor-state-leak-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(&src(POSITIVE_BODY), dir.path(), "pos");
    let (count, bytes) = measure_leaks_exact(&bin);
    assert_eq!(
        count,
        0,
        "leaks(1) reported {count} leak(s) — the actor shutdown drop must free the \
         deque field exactly once (after running `close`). Re-run with \
         `MallocStackLogging=1 leaks --atExit -- {}`.",
        bin.display()
    );
    assert_eq!(bytes, 0, "expected 0 leaked bytes, got {bytes}");
    eprintln!("#2654 zero-leak: 0 leaks for 0 total leaked bytes — PASS");
}
