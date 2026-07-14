//! #2654 oracle: an actor-state field holding a `#[resource] #[opaque]` handle
//! (or a pointer-backed `IoHandle`) may NOT be overwritten in place — the store
//! has no release-before-store, so the previous handle would leak (its `close`
//! never runs) while the actor's shutdown drop double-owns the freshly-stored
//! one. This is the actor-state sibling of the record `RecordFieldStore`
//! overwrite gate (`raii1_record_resource_field_leak_oracle.rs`); the two now
//! obey the same exactly-once-close invariant.
//!
//! ## Background (the asymmetry this closes)
//!
//! Before this lane, `h.dq = src` on a plain record was fail-closed rejected
//! ("overwriting an owned handle field … the previous handle would leak …
//! rebuild the whole record"), but the structurally identical `self.dq = src`
//! on actor state compiled silently and leaked: the emitted `Keeper__recv__…`
//! IR was a bare `store ptr … , ptr …` with no old-value load and no `Dq::close`
//! (the single close lived only in `__hew_state_drop_Keeper`), so every overwrite
//! dropped one `hew_deque_new` box on the floor (`leaks --atExit` reported ~500
//! ROOT LEAKs on the original repro). This oracle pins the refusal so the leak
//! cannot reappear, and pins the working (never-reassigned) path so the refusal
//! did not break exactly-once close.
//!
//! ## What each oracle pins
//!
//! - **Resource reassignment is REFUSED (fail-closed, no leak).** `self.dq = src`
//!   over a `#[resource] #[opaque]` state field fails `hew check` with the
//!   actor-state overwrite diagnostic (naming the field + handle type; remediation
//!   = re-`spawn`, NOT the record's "rebuild the whole record").
//! - **`IoHandle` reassignment is ALSO refused (FD-exhaustion vector closed by
//!   construction).** A `Stream<T>` state field reassignment is refused with the
//!   same actor-state wording, so no compiled binary can leak a descriptor by
//!   overwriting a live stream handle. (Such a field is doubly-unconstructible
//!   today — pointer-backed IO handles are not `Send` and fail the supervisor-
//!   restart clone helper — so this is a fail-closed boundary ahead of the
//!   capability rather than a currently-reachable user program; the regression
//!   scan confirmed no accepted program reassigns an actor-state IO handle.)
//! - **The never-reassigned field still closes EXACTLY once (non-vacuous).** A
//!   `var dq: Dq` state field that is read but never overwritten runs `close(self)`
//!   exactly once at actor teardown — proven by exact stdout, by `0 leaks for 0
//!   total leaked bytes` over a genuine `hew_deque_new` heap box, and by a clean
//!   run under the poisoned-allocator triple. `var` (mutable) alone must not trip
//!   the gate — only an actual overwrite does.
//! - **IR-structural: the single close lives in the shutdown drop.** The emitted
//!   IR wires exactly one `Dq::close` into `__hew_state_drop_Keeper`, followed by
//!   a `store ptr null` (null-after-close) — the exactly-once teardown the gate
//!   protects.

#![cfg(unix)]

mod support;

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
    fn close(self) { unsafe { hew_deque_free(self) }; println(\"closed\"); }\n\
}\n\
extern \"C\" {\n\
    fn hew_deque_new() -> Dq;\n\
    fn hew_deque_free(consume dq: Dq);\n\
}\n";

fn src(body: &str) -> String {
    format!("{PRELUDE}{body}")
}

/// #2654 core: overwriting a `#[resource]` actor-state field in place. Must be
/// REFUSED — the old handle would leak, the new one would be double-owned.
const RESOURCE_REASSIGN_BODY: &str = "\
actor Keeper {\n\
    var dq: Dq;\n\
    receive fn replace() -> i64 {\n\
        dq = unsafe { hew_deque_new() };\n\
        1\n\
    }\n\
}\n\
fn main() -> i64 {\n\
    let k = spawn Keeper(dq: unsafe { hew_deque_new() });\n\
    let _ = select { reply from k.replace() => 1, after 50ms => 0 };\n\
    0\n\
}\n";

/// `IoHandle` vector: reassigning a `Stream<T>` actor-state field (the handle
/// arrives by-value through a handler parameter). Must be REFUSED with the same
/// actor-state wording — closes the descriptor-leak vector by construction.
const IOHANDLE_REASSIGN_BODY: &str = "\
actor Piper {\n\
    var s: Stream<i64>;\n\
    receive fn reset(ns: Stream<i64>) -> i64 {\n\
        s = ns;\n\
        1\n\
    }\n\
}\n\
fn main() -> i64 { 0 }\n";

/// Positive control: a `var dq: Dq` state field that is NEVER reassigned. The
/// actor closes it exactly once at teardown. Deterministic stdout: `done` then
/// `closed`.
const POSITIVE_BODY: &str = "\
actor Keeper {\n\
    var dq: Dq;\n\
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

/// Assert that overwriting an owned-handle actor-state field is REFUSED at
/// compile time with the actor-state remediation wording (distinct from the
/// record's "rebuild the whole record"). `handle_needle` is a substring of the
/// rendered handle type expected in the message.
fn assert_reassign_rejected(name: &str, source: &str, handle_needle: &str) {
    let dir = tempfile::Builder::new()
        .prefix(&format!("actor-state-reassign-{name}-"))
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
        "{name}: overwriting an owned handle in actor state must be REFUSED (the old \
         field handle leaks — its `close` never runs — and the stored value is \
         double-owned with the actor's shutdown drop), but compile succeeded:\n{}",
        describe_output(&output)
    );
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        combined.contains("overwriting an owned handle in actor state"),
        "{name}: expected the actor-state overwrite refusal, got:\n{combined}"
    );
    assert!(
        combined.contains("re-`spawn`"),
        "{name}: actor-state remediation must say re-`spawn` (not the record's \
         'rebuild the whole record'), got:\n{combined}"
    );
    assert!(
        combined.contains(handle_needle),
        "{name}: refusal must name the handle type `{handle_needle}`, got:\n{combined}"
    );
}

/// Run `bin`; return `(leak_count, leaked_bytes)` from `leaks --atExit` when a
/// summary is produced (mirrors the record oracle's harness).
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

/// Extract the `define`d function body containing `needle` (up to the closing
/// `}` line) from LLVM IR text.
fn fn_body(ll: &str, needle: &str) -> Option<String> {
    let mut in_fn = false;
    let mut body = String::new();
    for line in ll.lines() {
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

// ── negative oracles (the #2654 core) ─────────────────────────────────────────

/// Resource reassignment (`self.dq = new`) is REFUSED with actor-state wording.
#[test]
fn actor_state_resource_reassign_refused() {
    assert_reassign_rejected("resource", &src(RESOURCE_REASSIGN_BODY), "Dq");
}

/// `IoHandle` (`Stream<T>`) reassignment is ALSO refused — descriptor-leak vector
/// closed by construction.
#[test]
fn actor_state_iohandle_reassign_refused() {
    assert_reassign_rejected("iohandle", IOHANDLE_REASSIGN_BODY, "Stream<i64>");
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
#[test]
fn unreassigned_resource_field_zero_leaks_exact() {
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
        .prefix("actor-state-leak-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(&src(POSITIVE_BODY), dir.path(), "pos");
    let Some((count, bytes)) = measure_leaks_exact(&bin) else {
        return;
    };
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

/// IR-structural: exactly one `Dq::close` in `__hew_state_drop_Keeper`, followed
/// by a null-after-close store — the exactly-once teardown the gate protects.
#[test]
fn positive_control_ir_single_close_in_state_drop() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("actor-state-ir-")
        .tempdir()
        .expect("tempdir");
    let _bin = compile_to_native(&src(POSITIVE_BODY), dir.path(), "pos");
    let ll = std::fs::read_to_string(dir.path().join("pos.ll")).expect("read emitted IR");
    let drop_body = fn_body(&ll, "@__hew_state_drop_Keeper(")
        .expect("emitted IR must define __hew_state_drop_Keeper");
    assert_eq!(
        drop_body.matches("@\"Dq::close\"").count(),
        1,
        "actor state drop must call the resource close EXACTLY once:\n{drop_body}"
    );
    assert!(
        drop_body.contains("store ptr null"),
        "actor state drop must null the field after close:\n{drop_body}"
    );
}
