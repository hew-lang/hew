//! RAII-1 oracle: an authored `#[resource] #[opaque]` handle is released by its
//! own `close` exactly once on every exit path, alone and inside a record
//! (spec §3.7.3 / §10(d)).
//!
//! The fixture resource is the runtime deque (`hew_deque_new` → `Box::into_raw`,
//! a real global-heap allocation); `close(self)` calls `hew_deque_free` and
//! prints `closed`, so a skipped release leaks a box and a repeated one
//! double-frees it.
//!
//! ## What each oracle pins
//!
//! - **close fires exactly once on each exit path.** Sync scope exit,
//!   forked-task completion and actor shutdown each print `closed` once.
//!
//! - **moving the field out transfers the release.** `let d = h.dq; d.close()`
//!   and `h.dq.close()` release through the extracted handle while the record's
//!   own cleanup releases nothing, because SIR certifies the field absent
//!   there. These replace the pre-final-path refusals, which existed only
//!   because the source slot had no null-after-move.
//!
//! - **overwriting the field releases the displaced handle first.** `h.dq = new`
//!   lowers to an assignment carrying a destroy of the old contents, so the old
//!   handle closes before the store and the new one at scope exit.
//!
//! - **a close that does not consume its receiver is refused.** An opaque
//!   handle's own `close` is its only release, so a receiver still owned where
//!   that close ends has none: the compiler refuses rather than re-enter
//!   `close` or drop a foreign handle silently.
//!
//! - **no double-free under the poisoned-allocator triple** and **exact
//!   zero-leak (macOS `leaks(1)`)** over a 200× construct-then-drop loop.
//!
//! Release at a cancelled suspension is a runtime oracle in the
//! `opaque-resource-in-aggregates` core-acceptance case, which holds the same
//! record live across a cancelled `sleep`.

#![cfg(unix)]

mod support;

use support::leak_slope::{measure_leaks_exact, require_leaks_tool};

use std::path::{Path, PathBuf};
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

// ── fixtures ────────────────────────────────────────────────────────────────

/// Shared prelude: a `#[resource] #[opaque]` handle backed by the runtime deque
/// (real global heap, links via `libhew.a` with no custom C shim) and a plain
/// record that owns one. `close(self)` frees the deque and prints `closed`.
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
}\n\
type Holder { dq: Dq }\n";

/// Build a fixture by appending `body` to the shared prelude.
fn src(body: &str) -> String {
    format!("{PRELUDE}{body}")
}

/// Sync scope-exit: the record drops at end of `main`, the thunk runs `close`.
/// Deterministic stdout: `made` then `closed`.
const SCOPE_EXIT_BODY: &str = "\
fn main() {\n\
    let h = Holder { dq: unsafe { hew_deque_new() } };\n\
    println(\"made\");\n\
}\n";
const SCOPE_EXIT_EXPECTED: &str = "made\nclosed\n";

/// Forked task: the worker constructs the record and runs to completion; the task
/// frame's drop glue runs `close` exactly once. Ordering is timing-dependent
/// (the count, not the order, is asserted).
const FORKED_TASK_BODY: &str = "\
fn worker() {\n\
    let _h = Holder { dq: unsafe { hew_deque_new() } };\n\
    sleep(10ms);\n\
    println(\"worker-done\");\n\
}\n\
actor Driver {\n\
    receive fn drive() {\n\
        scope { let t = fork worker(); };\n\
    }\n\
}\n\
fn main() -> i64 {\n\
    let d = spawn Driver;\n\
    let _ = d.drive();\n\
    sleep(100ms);\n\
    println(\"main-done\");\n\
    0\n\
}\n";

/// Looped construct-then-drop: 200 deques, each freed exactly once by the thunk.
/// A double-free aborts under the poisoned triple; an un-run close leaks one box
/// per cycle.
const LOOP_BODY: &str = "\
fn cycle() { let h = Holder { dq: unsafe { hew_deque_new() } }; let _ = h; }\n\
fn main() -> i64 {\n\
    var n: i64 = 0;\n\
    for i in 0..200 { cycle(); n = n + 1; }\n\
    if n != 200 { return 70; }\n\
    0\n\
}\n";

/// Moving the field out of the record: the extraction owns the release and the
/// record's own cleanup releases nothing, so `close` runs exactly once.
const EXTRACT_VIA_BIND_BODY: &str = "\
fn main() {\n\
    let h = Holder { dq: unsafe { hew_deque_new() } };\n\
    let d = h.dq;\n\
    d.close();\n\
}\n";

/// Method-receiver shape of the same move: `h.dq.close()` projects the field out
/// and consumes it in one expression.
const EXTRACT_VIA_METHOD_BODY: &str = "\
fn main() {\n\
    let h = Holder { dq: unsafe { hew_deque_new() } };\n\
    h.dq.close();\n\
}\n";

/// Overwriting the field in place: the displaced handle closes before the store,
/// the replacement at scope exit. A store, not a load, so it is a distinct path
/// from extraction.
const OVERWRITE_BODY: &str = "\
fn main() {\n\
    var h = Holder { dq: unsafe { hew_deque_new() } };\n\
    h.dq = unsafe { hew_deque_new() };\n\
    println(\"reassigned\");\n\
}\n";

/// Negative control for the release rule: a `close` that does not consume its
/// receiver leaves the handle with no admissible release, so the program is
/// refused rather than silently leaked or closed twice.
const CLOSE_KEEPS_RECEIVER: &str = "\
#[resource]\n\
#[opaque]\n\
type Kept {}\n\
impl Kept {\n\
    fn close(consume self) { println(\"bye\"); }\n\
}\n\
extern \"C\" {\n\
    fn hew_deque_new() -> Kept;\n\
}\n\
fn main() {\n\
    let _k = unsafe { hew_deque_new() };\n\
    println(\"made\");\n\
}\n";

/// Acceptance target: the bare resource-field record must ADMIT (no W3.029).
const ADMIT_BODY: &str = "\
fn main() {\n\
    let h = Holder { dq: unsafe { hew_deque_new() } };\n\
    let _ = h;\n\
}\n";

/// Actor shutdown: the resource-field record is an actor state field, released
/// when the actor stops.
const ACTOR_BODY: &str = "\
actor Keeper {\n\
    let h: Holder,\n\
    receive fn ping() -> i64 { 1 }\n\
}\n\
fn main() -> i64 {\n\
    let k = spawn Keeper(h: Holder { dq: unsafe { hew_deque_new() } });\n\
    let _ = select { reply from k.ping() => 1, after 50ms => 0 };\n\
    println(\"done\");\n\
    0\n\
}\n";

// ── plumbing ──────────────────────────────────────────────────────────────────

/// Compile `source` to a native binary via `hew compile --emit-dir`; return the
/// binary path. Asserts the compile succeeds.
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

/// Compile and run `source`, asserting a clean exit and exact stdout.
fn assert_stdout(name: &str, source: &str, expected: &str) {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix(&format!("raii1-{name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), name);
    let output = Command::new(&bin).output().expect("run fixture binary");
    assert!(
        output.status.success(),
        "{name} must run clean;\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        expected,
        "{name} released its handle on a different schedule than the one the \
         drop recipe pins;\n{}",
        describe_output(&output)
    );
}

// ── oracles ───────────────────────────────────────────────────────────────────

/// Sync scope-exit: `close` fires exactly once on drop. Deterministic stdout.
#[test]
fn raii1_record_resource_field_closes_once_on_scope_exit() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("raii1-scope-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(&src(SCOPE_EXIT_BODY), dir.path(), "scope");
    let output = Command::new(&bin).output().expect("run scope binary");
    assert!(
        output.status.success(),
        "scope-exit fixture must run clean;\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        SCOPE_EXIT_EXPECTED,
        "a record embedding a `#[resource] #[opaque]` field must run the field's \
         `close(self)` exactly once on scope-exit drop;\n{}",
        describe_output(&output)
    );
}

/// Forked task: `close` fires exactly once on task-frame teardown.
#[test]
fn raii1_record_resource_field_closes_once_in_forked_task() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("raii1-task-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(&src(FORKED_TASK_BODY), dir.path(), "task");
    let output = Command::new(&bin).output().expect("run task binary");
    assert!(
        output.status.success(),
        "forked-task fixture must run clean;\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    let closed = stdout.lines().filter(|l| *l == "closed").count();
    assert_eq!(
        closed,
        1,
        "a record dropped on forked-task completion must run `close` EXACTLY once \
         (got {closed});\n{}",
        describe_output(&output)
    );
}

/// Looped construct-then-drop, 200×: no double-free under the poisoned triple.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn raii1_record_resource_field_no_double_free_under_malloc_scribble() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("raii1-df-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(&src(LOOP_BODY), dir.path(), "loop");
    let output = Command::new(&bin)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .output()
        .expect("run loop binary");
    assert!(
        output.status.success(),
        "looped resource-field record must run clean under the poisoned allocator — \
         an abort here is a double-free of the deque handle (the thunk close and a \
         second path both freed it);\n{}",
        describe_output(&output)
    );
}

/// Looped construct-then-drop: exactly `0 leaks for 0 total leaked bytes`.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn raii1_record_resource_field_zero_leaks_exact() {
    require_leaks_tool();
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("raii1-leak-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(&src(LOOP_BODY), dir.path(), "loop");
    let (count, bytes) = measure_leaks_exact(&bin);
    assert_eq!(
        count,
        0,
        "leaks(1) reported {count} leak(s) — the record drop thunk must free its \
         deque field exactly once per cycle (after running `close`). Re-run with \
         `MallocStackLogging=1 leaks --atExit -- {}`.",
        bin.display()
    );
    assert_eq!(bytes, 0, "expected 0 leaked bytes, got {bytes}");
    eprintln!("raii1 zero-leak: 0 leaks for 0 total leaked bytes — PASS");
}

/// Moving the field out (bind shape): the extraction closes it exactly once and
/// the record's own cleanup adds nothing.
#[test]
fn raii1_record_resource_field_extraction_via_bind_closes_once() {
    assert_stdout("extract_bind", &src(EXTRACT_VIA_BIND_BODY), "closed\n");
}

/// Moving the field out (method-receiver shape): `h.dq.close()` closes once.
#[test]
fn raii1_record_resource_field_extraction_via_method_closes_once() {
    assert_stdout("extract_method", &src(EXTRACT_VIA_METHOD_BODY), "closed\n");
}

/// Overwriting the field releases the displaced handle before the store and the
/// replacement at scope exit: two closes, in that order, neither one repeated.
#[test]
fn raii1_record_resource_field_overwrite_releases_both() {
    assert_stdout(
        "overwrite_assign",
        &src(OVERWRITE_BODY),
        "closed\nreassigned\nclosed\n",
    );
}

/// Negative control: an authored opaque `close` that does not consume its
/// receiver is refused, because nothing else can release the handle. The
/// refusal is a lowering fact, so it needs a compile rather than a check.
#[test]
fn raii1_opaque_close_keeping_its_receiver_rejected() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("raii1-keeps-receiver-")
        .tempdir()
        .expect("tempdir");
    let hew_src = dir.path().join("keeps.hew");
    std::fs::write(&hew_src, CLOSE_KEEPS_RECEIVER).expect("write hew source");
    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--emit-dir",
            dir.path().to_str().expect("emit-dir utf-8"),
            hew_src.to_str().expect("hew src utf-8"),
        ])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile");
    assert!(
        !output.status.success(),
        "a close that leaves its receiver owned must be REFUSED;\n{}",
        describe_output(&output)
    );
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        combined.contains("must consume its receiver"),
        "expected the receiver-consumption refusal, got:\n{combined}"
    );
}

/// Acceptance target: the bare resource-field record ADMITS (no W3.029).
#[test]
fn raii1_record_resource_field_admitted_no_w3_029() {
    let dir = tempfile::Builder::new()
        .prefix("raii1-admit-")
        .tempdir()
        .expect("tempdir");
    let hew_src = dir.path().join("admit.hew");
    std::fs::write(&hew_src, src(ADMIT_BODY)).expect("write hew source");
    let output = Command::new(hew_binary())
        .args(["check", hew_src.to_str().expect("hew src utf-8")])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew check");
    assert!(
        output.status.success(),
        "a plain record embedding a `#[resource] #[opaque]` field must ADMIT \
         (RAII-1 add-around W3.029);\n{}",
        describe_output(&output)
    );
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        !combined.contains("W3.029"),
        "expected no W3.029 admission refusal, got:\n{combined}"
    );
}

/// Actor shutdown: stopping the actor releases the state record's handle once.
#[test]
fn raii1_actor_shutdown_closes_state_field_once() {
    assert_stdout("actor", &src(ACTOR_BODY), "done\nclosed\n");
}
