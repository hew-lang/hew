//! Field-bearing `#[resource]` record `close()` oracle (spec §3.7.3 / §10(d)).
//!
//! A `#[resource]` type that owns a heap/aggregate field (a nested record, a
//! `string`, a `Vec`, …) is admitted to the owned-aggregate-record drop set and
//! routes to the recursive `__hew_record_drop_inplace_<R>` thunk. That thunk
//! freed the record's heap leaves but silently SKIPPED the user `close(self)` —
//! losing the RAII contract spec §3.7.3 guarantees ("`close()` runs exactly once
//! per value"). A nested `#[resource]` field's `close()` was skipped the same
//! way, two record layers deep.
//!
//! The fix runs the user `close(self)` as the FIRST step of the drop thunk —
//! before the reverse-order field teardown (spec §10(d): the resource's own
//! close fires, then fields drop in reverse declaration order). Because the
//! thunk is recursive, a nested `#[resource]` field's `close()` fires through
//! its OWN thunk, reached by the field-drop step. close operates on the external
//! resource only; the field-wise teardown still frees each heap leaf exactly
//! once, so the change adds the RAII close without touching heap ownership (no
//! double-free).
//!
//! ## What each oracle pins
//!
//! - **Semantic — close fires (the non-vacuous pre-fix signal).** The primary
//!   defect of this gap is a SILENTLY SKIPPED `close()`, not a byte leak (the
//!   record teardown already freed the heap leaves). So the pre-fix baseline is
//!   the ABSENCE of the close side-effects: pre-fix a field-bearing
//!   `#[resource]` (and its nested `#[resource]` field) print neither
//!   `outer-closed` nor `inner-closed` on scope-exit drop; post-fix BOTH print,
//!   once each, outer before inner (§10(d) order). A regression that drops the
//!   thunk close-call fails the exact-stdout assertion.
//!
//! - **No double-close on an explicit `close()`.** An explicit `o.close()`
//!   consumes the record (the move-checker removes it from the scope-exit drop
//!   set), so `close` fires EXACTLY once — the explicit call — and the thunk
//!   close does NOT also run. A regression that ran the thunk close on a
//!   consumed value would print the close side-effect twice.
//!
//! - **No double-free under the poisoned-allocator triple (any unix).** A
//!   field-bearing `#[resource]` whose heap field is solely record-owned: the
//!   thunk runs `close(self)` then frees the field exactly once. If close and
//!   the teardown each freed the field this aborts under
//!   `MallocScribble`/`MallocPreScribble`/`MallocGuardEdges`. The loop body
//!   amplifies any per-iteration double-free.
//!
//! - **Exact zero-leak (macOS via `leaks(1)`).** A field-bearing `#[resource]`
//!   owning a real `Vec<i64>` field freed via `Vec::new()` + `push` (the same
//!   heap source the heap-owning-record oracles use — provably a heap buffer,
//!   not a folded literal) drops to exactly `0 leaks for 0 total leaked bytes`.
//!   Asserting `0` over a genuine heap field, with the `box-closed` side-effect
//!   proving the drop path executed, is non-vacuous.

#![cfg(unix)]

mod support;

use std::path::PathBuf;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

// ── fixtures ──────────────────────────────────────────────────────────────

/// Nested field-bearing resource: `Outer { inner: Inner; tag: i64 }` where both
/// `Outer` and `Inner` are `#[resource]`. On scope-exit drop the thunk runs
/// `Outer::close` then field-teardown recurses into `Inner`'s thunk, which runs
/// `Inner::close`. Pre-fix: only `before-scope-exit` printed. Post-fix: outer
/// then inner close fire (§10(d) order).
const NESTED_CLOSE_SOURCE: &str = "\
#[resource] type Inner { fd: i64; }\n\
impl Inner { fn close(self) { println(\"inner-closed\"); } }\n\
#[resource] type Outer { inner: Inner; tag: i64; }\n\
impl Outer { fn close(self) { println(\"outer-closed\"); } }\n\
fn make_inner() -> Inner { Inner { fd: 3 } }\n\
fn main() {\n\
\x20   let o = Outer { inner: make_inner(), tag: 7 };\n\
\x20   println(\"before-scope-exit\");\n\
}\n";

/// Expected post-fix output: the marker line, then outer close, then nested
/// inner close — close runs before field teardown, and the nested field's close
/// runs as part of that teardown.
const NESTED_CLOSE_EXPECTED: &str = "before-scope-exit\nouter-closed\ninner-closed\n";

/// Explicit-consume control: `o.close()` consumes `o`, so the scope-exit thunk
/// drop is suppressed (no double-close). `outer-closed` prints EXACTLY once.
/// `inner` is owned by the consumed value; the user `close(self)` chose not to
/// close it, so `inner-closed` does NOT print (no thunk teardown on a consumed
/// value) — leaking `inner`'s (here scalar) storage is acceptable; a
/// double-close / double-free is not.
const EXPLICIT_CONSUME_SOURCE: &str = "\
#[resource] type Inner { fd: i64; }\n\
impl Inner { fn close(self) { println(\"inner-closed\"); } }\n\
#[resource] type Outer { inner: Inner; tag: i64; }\n\
impl Outer { fn close(self) { println(\"outer-closed\"); } }\n\
fn make_inner() -> Inner { Inner { fd: 3 } }\n\
fn main() {\n\
\x20   let o = Outer { inner: make_inner(), tag: 7 };\n\
\x20   o.close();\n\
\x20   println(\"after-explicit\");\n\
}\n";

/// Expected: exactly one `outer-closed` (the explicit call), then the marker.
/// A second `outer-closed` would mean the thunk close ran on a consumed value.
const EXPLICIT_CONSUME_EXPECTED: &str = "outer-closed\nafter-explicit\n";

/// Heap-field resource, double-free landmine + leak source: `Box { payload:
/// Vec<i64>; fd: i64 }` is `#[resource]`. The `Vec` is built with `Vec::new()` +
/// `push` and moved into the record (solely record-owned). The thunk runs
/// `Box::close` then frees the `Vec` exactly once. Looped to amplify any
/// per-iteration double-free / leak. `close` has an empty body (it touches no
/// field), so the only field release is the teardown — if close also freed the
/// Vec, this aborts under the poisoned allocator.
const HEAP_FIELD_SOURCE: &str = "\
#[resource] type Box { payload: Vec<i64>; fd: i64; }\n\
impl Box { fn close(self) { } }\n\
fn build(n: i64) -> i64 {\n\
\x20   let v: Vec<i64> = Vec::new();\n\
\x20   v.push(n);\n\
\x20   v.push(n + 1);\n\
\x20   let b = Box { payload: v, fd: n };\n\
\x20   b.fd\n\
}\n\
fn main() -> i64 {\n\
\x20   var total: i64 = 0;\n\
\x20   for i in 0..200 { total = total + build(i); }\n\
\x20   if total != 19900 { return 70; }\n\
\x20   0\n\
}\n";

/// Heap-field resource, side-effect + zero-leak fixture: the same `Box` shape
/// run once with a `close` that prints, so the zero-leak assertion is paired
/// with proof the drop path executed (`box-closed`) over a genuine heap `Vec`.
const HEAP_FIELD_CLOSE_SOURCE: &str = "\
#[resource] type Box { payload: Vec<i64>; fd: i64; }\n\
impl Box { fn close(self) { println(\"box-closed\"); } }\n\
fn build() -> i64 {\n\
\x20   let v: Vec<i64> = Vec::new();\n\
\x20   v.push(10);\n\
\x20   v.push(20);\n\
\x20   let b = Box { payload: v, fd: 7 };\n\
\x20   b.fd\n\
}\n\
fn run_one_cycle() { let x = build(); print(\"ok\"); }\n\
fn main() { run_one_cycle(); }\n";

// ── plumbing ──────────────────────────────────────────────────────────────

/// Compile `source` to a native binary via `hew compile --emit-dir` and return
/// the binary path.
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

/// Run `bin` under the poisoned-allocator triple + `leaks --atExit`; return
/// `Some((leak_count, leaked_bytes))` when `leaks` produced a usable summary.
fn measure_leaks_exact(bin: &std::path::Path) -> Option<(usize, usize)> {
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
        let Some(count_str) = words.next() else {
            continue;
        };
        let Ok(count) = count_str.parse::<usize>() else {
            continue;
        };
        let _ = words.next(); // "leaks" / "leak"
        let _ = words.next(); // "for"
        let bytes_str = words.next()?;
        let Ok(bytes) = bytes_str.parse::<usize>() else {
            return None;
        };
        eprintln!("  leaks(1) summary: count={count} bytes={bytes} (from: {line})");
        return Some((count, bytes));
    }
    None
}

/// Compile + run `source`, assert clean exit and exact stdout.
fn assert_exact_stdout(name: &str, source: &str, expected: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("resource-record-close-{name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), name);

    let output = Command::new(&bin)
        .output()
        .unwrap_or_else(|error| panic!("run {name} binary: {error}"));

    assert!(
        output.status.success(),
        "{name} must run clean;\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        expected,
        "{name}: a field-bearing `#[resource]` record's user `close(self)` must fire \
         on scope-exit drop, before field teardown (spec §3.7.3 / §10(d)), and a nested \
         `#[resource]` field's `close()` must fire through the recursive drop thunk. \
         Wrong output means the thunk skipped a close (pre-fix gap) OR ran one twice \
         (double-close);\n{}",
        describe_output(&output)
    );
}

/// Compile + run `source` under the poisoned-allocator triple, assert clean exit
/// (the double-free pin) and the i64 exit status the fixture computes.
fn assert_no_double_free_under_malloc_scribble(name: &str, source: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("resource-record-close-df-{name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), name);

    let output = Command::new(&bin)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .output()
        .unwrap_or_else(|error| panic!("run {name} binary: {error}"));

    assert!(
        output.status.success(),
        "{name} must run clean under the poisoned allocator — an abort here is a \
         double-free of a `#[resource]` record's heap field (the user `close(self)` and \
         the field-wise teardown both released the same buffer), or a non-zero exit is \
         the fixture's own sum/teardown check failing;\n{}",
        describe_output(&output)
    );
}

/// Compile + run under `leaks --atExit`, assert exactly `(0, 0)`.
fn assert_zero_leaks_exact(name: &str, source: &str) {
    if !cfg!(target_os = "macos") {
        eprintln!("skip: {name}: leaks(1) is macOS-only");
        return;
    }
    let leaks_avail = Command::new("which")
        .arg("leaks")
        .output()
        .is_ok_and(|o| o.status.success());
    if !leaks_avail {
        eprintln!("skip: {name}: `leaks` binary not on PATH");
        return;
    }

    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("resource-record-close-zero-leak-{name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), name);

    let Some((leak_count, leaked_bytes)) = measure_leaks_exact(&bin) else {
        return;
    };

    assert_eq!(
        leak_count,
        0,
        "{name}: leaks(1) reported {leak_count} leak(s) — expected exactly 0. The \
         `#[resource]` record's drop thunk must free its `Vec<i64>` field exactly once \
         (after running the user `close`). Re-run with \
         `MallocStackLogging=1 leaks --atExit -- {}`.",
        bin.display()
    );
    assert_eq!(
        leaked_bytes, 0,
        "{name}: leaks(1) reported 0 leak nodes but {leaked_bytes} total leaked bytes — \
         expected exactly 0.",
    );

    eprintln!("{name}: leaks(1) confirmed 0 leaks for 0 total leaked bytes — PASS");
}

// ── oracles ─────────────────────────────────────────────────────────────────

/// Nested field-bearing resource: outer close fires, then nested inner close,
/// on scope-exit drop. Pre-fix neither printed (silently-skipped close).
#[test]
fn resource_record_nested_close_fires_on_drop() {
    assert_exact_stdout("nested_close", NESTED_CLOSE_SOURCE, NESTED_CLOSE_EXPECTED);
}

/// Explicit `o.close()` consumes the record: close fires EXACTLY once (the
/// explicit call), the scope-exit thunk close is suppressed (no double-close).
#[test]
fn resource_record_explicit_close_single_no_double() {
    assert_exact_stdout(
        "explicit_consume",
        EXPLICIT_CONSUME_SOURCE,
        EXPLICIT_CONSUME_EXPECTED,
    );
}

/// Heap-field resource looped 200×: close-then-free runs each iteration with no
/// double-free of the solely-record-owned `Vec<i64>` under the poisoned triple.
#[test]
fn resource_record_heap_field_no_double_free_under_malloc_scribble() {
    assert_no_double_free_under_malloc_scribble("heap_field", HEAP_FIELD_SOURCE);
}

/// Heap-field resource: the drop thunk runs `close` then frees the genuine
/// `Vec<i64>` field exactly once — `0 leaks for 0 total leaked bytes`.
#[test]
fn resource_record_heap_field_zero_leaks_exact() {
    assert_zero_leaks_exact("heap_field_close", HEAP_FIELD_CLOSE_SOURCE);
}
