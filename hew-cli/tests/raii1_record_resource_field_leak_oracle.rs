//! RAII-1 oracle: a plain record embedding a `#[resource] #[opaque]` handle
//! field drops the field exactly once on every exit path (spec §3.7.3 / §10(d)).
//!
//! Before RAII-1, MIR refused such a record at admission (W3.029: "value class
//! that MIR cannot lower yet … field has value class `AffineResource`") because a
//! `#[resource] #[opaque]` handle classified as a no-op-drop `OpaqueHandle`. The
//! lane builds a `resource_opaque_close` registry — `(opaque_type, "<T>::close")`
//! for single-slot opaque resources whose `close` is a USER method — and routes
//! the field through `StateFieldCloneKind::Resource`, so the owning aggregate's
//! recursive `__hew_record_drop_inplace_<R>` thunk runs the field's `close(self)`
//! exactly once. All exit paths converge on that one thunk:
//!
//!   - **sync scope-exit** — `RecordInPlace` drop calls the thunk;
//!   - **forked-task completion / cancel** — the task frame's drop glue calls it
//!     (cooperative cancel at a resume point drops the frame-spilled record, the
//!     SAME way an owned `Vec` local across a suspend is dropped);
//!   - **actor shutdown** — `hew_actor_set_state_drop(@__hew_state_drop_<A>)` →
//!     `__hew_state_drop_<A>` → the record thunk.
//!
//! ## What each oracle pins
//!
//! - **close fires exactly once (non-vacuous).** The test resource is the runtime
//!   deque (`hew_deque_new` → `Box::into_raw`, a real global-heap allocation);
//!   `close(self)` calls `hew_deque_free` and prints `closed`. Pre-fix the record
//!   did not compile at all (W3.029); a regression that skipped the thunk close
//!   would either leak the deque (caught by the zero-leak oracle) or never print
//!   `closed` (caught by the exact-stdout oracle).
//!
//! - **no double-free under the poisoned-allocator triple.** Constructing and
//!   dropping the record in a 200× loop frees the deque exactly once per cycle;
//!   if the thunk close and any other path both freed it, this aborts under
//!   `MallocScribble`/`MallocPreScribble`/`MallocGuardEdges`.
//!
//! - **exact zero-leak (macOS `leaks(1)`).** The looped construct-then-drop drops
//!   to `0 leaks for 0 total leaked bytes` over a genuine heap deque — non-vacuous
//!   because an un-run close would leak one `HewDeque` box per cycle.
//!
//! - **manual field extraction is REFUSED (fail-closed, no double-free).**
//!   Projecting the resource field OUT of the record (`let d = h.dq`,
//!   `h.dq.close()`) would byte-copy the pointer-width handle with no
//!   null-after-move on the source slot, so the record's thunk AND the extracted
//!   handle's consumer would both free the one context — a double-free. Until the
//!   source-slot null-after-move lands (RAII-2), the compiler refuses the
//!   projection rather than emit it (LESSONS boundary-fail-closed). This pins the
//!   reject so a future loosening is a deliberate, reviewed change.
//!
//! - **the record is ADMITTED (the acceptance target).** The bare resource-field
//!   record compiles with no W3.029 — the W3.029-add-around control.
//!
//! - **IR-structural: the actor-shutdown and async-cancel exit paths route to the
//!   close.** Their runtime triggers are entangled with actor/task lifecycle
//!   (an idle actor is not stopped at program exit; deterministic mid-suspend
//!   cancellation is not reproducible in a unit test), so these paths are pinned
//!   structurally: the emitted IR wires each exit through
//!   `__hew_record_drop_inplace_Holder` → `Dq::close`.

#![cfg(unix)]

mod support;

use std::path::{Path, PathBuf};
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

// ── fixtures ────────────────────────────────────────────────────────────────

/// Shared prelude: a `#[resource] #[opaque]` handle backed by the runtime deque
/// (real global heap, links via `libhew_std.a` with no custom C shim) and a plain
/// record that owns one. `close(self)` frees the deque and prints `closed`.
const PRELUDE: &str = "\
#[resource]\n\
#[opaque]\n\
type Dq {}\n\
impl Dq {\n\
    fn close(self) { unsafe { hew_deque_free(self) }; println(\"closed\"); }\n\
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
        scope { fork { worker(); }; };\n\
    }\n\
}\n\
fn main() -> i64 {\n\
    let d = spawn Driver;\n\
    d.drive();\n\
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

/// R7 reject control: projecting the resource field OUT of the record then closing
/// the extraction would double-free. Must be REFUSED at compile time.
const EXTRACT_VIA_BIND_BODY: &str = "\
fn main() {\n\
    let h = Holder { dq: unsafe { hew_deque_new() } };\n\
    let d = h.dq;\n\
    d.close();\n\
}\n";

/// R7 reject control, method-receiver shape: `h.dq.close()` also projects the
/// field out before consuming it. Same double-free, same refusal.
const EXTRACT_VIA_METHOD_BODY: &str = "\
fn main() {\n\
    let h = Holder { dq: unsafe { hew_deque_new() } };\n\
    h.dq.close();\n\
}\n";

/// R8 reject control: OVERWRITING the resource field in place (`h.dq = src`)
/// raw-stores over the slot — the OLD handle leaks (its `close` never runs) and
/// `src` becomes a second owner of a context the record's drop thunk will also
/// free. A distinct escape vector from extraction (a store, not a load); must be
/// REFUSED at compile time until overwrite-release lands (RAII-2).
const OVERWRITE_BODY: &str = "\
fn main() {\n\
    var h = Holder { dq: unsafe { hew_deque_new() } };\n\
    h.dq = unsafe { hew_deque_new() };\n\
    println(\"reassigned\");\n\
}\n";

/// Acceptance target: the bare resource-field record must ADMIT (no W3.029).
const ADMIT_BODY: &str = "\
fn main() {\n\
    let h = Holder { dq: unsafe { hew_deque_new() } };\n\
    let _ = h;\n\
}\n";

/// Actor-shutdown IR fixture: the resource-field record is an actor state field.
const ACTOR_BODY: &str = "\
actor Keeper {\n\
    let h: Holder;\n\
    receive fn ping() -> i64 { 1 }\n\
}\n\
fn main() -> i64 {\n\
    let k = spawn Keeper(h: Holder { dq: unsafe { hew_deque_new() } });\n\
    let _ = select { reply from k.ping() => 1, after 50ms => 0 };\n\
    0\n\
}\n";

/// Async-cancel IR fixture: the record is held LIVE across a real suspend point
/// (an actor `ask`), so the handler lowers to a coroutine whose cooperative-cancel
/// resume blocks must drop the frame-spilled record.
const ASYNC_CANCEL_BODY: &str = "\
actor Echo {\n\
    receive fn ping() -> i64 { 42 }\n\
}\n\
actor Worker {\n\
    receive fn run() -> i64 {\n\
        let h = Holder { dq: unsafe { hew_deque_new() } };\n\
        let _ = h;\n\
        let e = spawn Echo;\n\
        let v = match await e.ping() { Ok(x) => x, Err(_) => 0 };\n\
        v\n\
    }\n\
}\n\
fn main() -> i64 {\n\
    let w = spawn Worker;\n\
    let _ = select { reply from w.run() => 1, after 200ms => 0 };\n\
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

/// Compile `source` and read the emitted `<name>.ll` LLVM IR text.
fn compile_and_read_ll(source: &str, dir: &Path, name: &str) -> String {
    let _bin = compile_to_native(source, dir, name);
    let ll = dir.join(format!("{name}.ll"));
    std::fs::read_to_string(&ll).unwrap_or_else(|e| panic!("read emitted IR {}: {e}", ll.display()))
}

/// Extract a function body from `ll`: the lines from the `define` containing
/// `needle` up to and including the closing `}` line.
fn fn_body(ll: &str, needle: &str) -> Option<String> {
    let mut lines = ll.lines();
    let mut body = String::new();
    let mut in_fn = false;
    for line in lines.by_ref() {
        if !in_fn {
            if line.starts_with("define") && line.contains(needle) {
                in_fn = true;
                body.push_str(line);
                body.push('\n');
            }
            continue;
        }
        body.push_str(line);
        body.push('\n');
        if line == "}" {
            return Some(body);
        }
    }
    None
}

/// Compile `source` and assert it is REFUSED with the owned-handle aggregate
/// extraction diagnostic.
fn assert_extraction_rejected(name: &str, source: &str) {
    let dir = tempfile::Builder::new()
        .prefix(&format!("raii1-reject-{name}-"))
        .tempdir()
        .expect("tempdir");
    let hew_src = dir.path().join(format!("{name}.hew"));
    std::fs::write(&hew_src, source).expect("write hew source");

    let output = Command::new(hew_binary())
        .args(["check", hew_src.to_str().expect("hew src utf-8")])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew check");

    assert!(
        !output.status.success(),
        "{name}: projecting a `#[resource]` field out of its owning record must be \
         REFUSED (it would double-free: the record's drop thunk and the extracted \
         handle's consumer both free the one context), but compile succeeded:\n{}",
        describe_output(&output)
    );
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        combined.contains("extracting an owned handle") || combined.contains("out of an aggregate"),
        "{name}: expected the owned-handle aggregate-extraction refusal, got:\n{combined}"
    );
}

/// Assert that overwriting a `#[resource]` field in place (`h.dq = src`,
/// lowering to `Instr::RecordFieldStore`) is REFUSED at compile time with the
/// overwrite-specific wording. The store vector is distinct from extraction: the
/// store gate keys on the stored value's opaque-resource type, not a field load.
fn assert_overwrite_rejected(name: &str, source: &str) {
    let dir = tempfile::Builder::new()
        .prefix(&format!("raii1-overwrite-{name}-"))
        .tempdir()
        .expect("tempdir");
    let hew_src = dir.path().join(format!("{name}.hew"));
    std::fs::write(&hew_src, source).expect("write hew source");

    let output = Command::new(hew_binary())
        .args(["check", hew_src.to_str().expect("hew src utf-8")])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew check");

    assert!(
        !output.status.success(),
        "{name}: overwriting a `#[resource]` field in place must be REFUSED (the old \
         field handle leaks — its `close` never runs — and the stored value is \
         double-owned with the record's drop thunk), but compile succeeded:\n{}",
        describe_output(&output)
    );
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        combined.contains("overwriting an owned handle field"),
        "{name}: expected the owned-handle overwrite refusal (distinct from the \
         extraction wording), got:\n{combined}"
    );
}

/// Run `bin` under the poisoned-allocator triple; return `(success, leak_count,
/// leaked_bytes)` from `leaks --atExit` when a summary is produced.
fn measure_leaks_exact(bin: &Path) -> Option<(usize, usize)> {
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
    for line in report.lines() {
        if !line.contains(" leaks for ") && !line.contains(" leak for ") {
            continue;
        }
        let Some(rest) = line.strip_prefix("Process ") else {
            continue;
        };
        if !rest.chars().next().is_some_and(|c| c.is_ascii_digit()) {
            continue;
        }
        let Some(after_colon) = rest.split_once(": ").map(|(_, s)| s) else {
            continue;
        };
        let mut words = after_colon.split_whitespace();
        let count = words.next()?.parse::<usize>().ok()?;
        let _ = words.next(); // "leaks" / "leak"
        let _ = words.next(); // "for"
        let bytes = words.next()?.parse::<usize>().ok()?;
        eprintln!("  leaks(1) summary: count={count} bytes={bytes} (from: {line})");
        return Some((count, bytes));
    }
    None
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
#[test]
fn raii1_record_resource_field_zero_leaks_exact() {
    if !cfg!(target_os = "macos") {
        eprintln!("skip: leaks(1) is macOS-only");
        return;
    }
    let leaks_avail = Command::new("which")
        .arg("leaks")
        .output()
        .is_ok_and(|o| o.status.success());
    if !leaks_avail {
        eprintln!("skip: `leaks` binary not on PATH");
        return;
    }
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("raii1-leak-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(&src(LOOP_BODY), dir.path(), "loop");
    let Some((count, bytes)) = measure_leaks_exact(&bin) else {
        return;
    };
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

/// R7 reject control (bind shape): `let d = h.dq; d.close()` is REFUSED.
#[test]
fn raii1_record_resource_field_extraction_via_bind_rejected() {
    assert_extraction_rejected("extract_bind", &src(EXTRACT_VIA_BIND_BODY));
}

/// R7 reject control (method-receiver shape): `h.dq.close()` is REFUSED.
#[test]
fn raii1_record_resource_field_extraction_via_method_rejected() {
    assert_extraction_rejected("extract_method", &src(EXTRACT_VIA_METHOD_BODY));
}

/// R8 reject control: overwriting the field in place (`h.dq = src`) is REFUSED.
/// Closes the drop-safety hole where `Instr::RecordFieldStore` raw-stored over an
/// opaque-resource slot — leaking the old handle and double-owning the new one.
#[test]
fn raii1_record_resource_field_overwrite_rejected() {
    assert_overwrite_rejected("overwrite_assign", &src(OVERWRITE_BODY));
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

/// IR-structural: the actor-shutdown exit path routes the state drop through the
/// record thunk to `Dq::close`.
#[test]
fn raii1_actor_shutdown_drop_spine_reaches_close() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("raii1-actor-ir-")
        .tempdir()
        .expect("tempdir");
    let ll = compile_and_read_ll(&src(ACTOR_BODY), dir.path(), "actor");

    assert!(
        ll.contains("hew_actor_set_state_drop") && ll.contains("@__hew_state_drop_Keeper"),
        "actor must register its state-drop fn `@__hew_state_drop_Keeper`"
    );
    let state_drop = fn_body(&ll, "@__hew_state_drop_Keeper")
        .expect("missing `__hew_state_drop_Keeper` definition");
    assert!(
        state_drop.contains("__hew_record_drop_inplace_Holder"),
        "actor state drop must call the record thunk;\n{state_drop}"
    );
    let thunk = fn_body(&ll, "@__hew_record_drop_inplace_Holder")
        .expect("missing `__hew_record_drop_inplace_Holder` definition");
    assert!(
        thunk.contains("@\"Dq::close\""),
        "the record drop thunk must call the field's `Dq::close`;\n{thunk}"
    );
}

/// IR-structural: with the record held LIVE across a suspend point, the handler is
/// a coroutine whose cooperative-cancel resume path drops the frame-spilled record
/// through the thunk to `Dq::close` (the async-cancel exit path).
#[test]
fn raii1_async_cancel_drop_spine_reaches_close() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("raii1-cancel-ir-")
        .tempdir()
        .expect("tempdir");
    let ll = compile_and_read_ll(&src(ASYNC_CANCEL_BODY), dir.path(), "cancel");

    let run = fn_body(&ll, "@Worker__recv__run").expect("missing `Worker__recv__run` definition");
    assert!(
        run.contains("llvm.coro.suspend"),
        "a record held across an `ask` must lower the handler to a coroutine \
         (suspend point present);\n{run}"
    );
    assert!(
        run.contains("cancel_exit") && run.contains("__hew_record_drop_inplace_Holder"),
        "the coroutine's cooperative-cancel resume path must drop the frame-spilled \
         record through the thunk;\n{run}"
    );
    let thunk = fn_body(&ll, "@__hew_record_drop_inplace_Holder")
        .expect("missing `__hew_record_drop_inplace_Holder` definition");
    assert!(
        thunk.contains("@\"Dq::close\""),
        "the record drop thunk must call the field's `Dq::close`;\n{thunk}"
    );
}
