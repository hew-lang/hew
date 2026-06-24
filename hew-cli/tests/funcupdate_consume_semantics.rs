//! Functional-update CONSUME-semantics fixtures.
//!
//! `T { field: new, ..base }` over an owned (heap-carrying) record CONSUMES
//! `base`: its carried fields move into the new record and its overridden
//! owned fields are destructively released at the construction site. That
//! destructive release is sound ONLY because the move-checker forbids any
//! later read of `base` — so the freed old value has no surviving reader.
//!
//! These fixtures pin both halves of that contract:
//!
//! * **reject** — programs that read `base` after it is consumed (a second
//!   `..base` from the same source; a `base.field` read) must FAIL `hew check`
//!   with `UseAfterConsume`. Before the consume-mark landed they compiled and
//!   corrupted memory (use-after-free / double-free of `base`'s overridden
//!   owned field). The compile error IS the safe outcome.
//!
//! * **reject** — a self-referential override whose value bare-aliases the
//!   consumed base (`{ items: s.items, ..s }`) must FAIL `hew check`
//!   (`NotYetImplemented`): the override reads the very field the destructive
//!   release would free.
//!
//! * **accept** — the canonical reassign-loop idiom (`c = T { ..c, f: new }`)
//!   and the explicit-clone override (`{ items: s.items.clone(), ..s }`) must
//!   `check` and `run` cleanly. The reassign re-defines `c`, so the consume of
//!   the old `c` is immediately superseded by the fresh binding — the loop
//!   stays legal. (Leak-freedom of the reassign loop is pinned separately by
//!   `funcupdate_owned_field_drop_leak_oracle`.)
//!
//! The two reject fixtures are the empirical P0 repros that originally
//! miscompiled; turning them into compile errors is the fix's headline
//! observable.

#![cfg(unix)]

mod support;

use std::process::Command;

use support::{hew_binary, repo_root, require_codegen};

/// Run `hew check <source>` and return `(success, combined_output)`.
fn hew_check(source: &str) -> (bool, String) {
    let dir = tempfile::tempdir().expect("tempdir");
    let src = dir.path().join("fixture.hew");
    std::fs::write(&src, source).expect("write source");
    let output = Command::new(hew_binary())
        .arg("check")
        .arg(&src)
        .current_dir(repo_root())
        .output()
        .expect("invoke hew check");
    let mut combined = String::from_utf8_lossy(&output.stdout).into_owned();
    combined.push_str(&String::from_utf8_lossy(&output.stderr));
    (output.status.success(), combined)
}

/// Run `hew run <source>` and return `(success, combined_output)`.
fn hew_run(source: &str) -> (bool, String) {
    let dir = tempfile::tempdir().expect("tempdir");
    let src = dir.path().join("fixture.hew");
    std::fs::write(&src, source).expect("write source");
    let output = Command::new(hew_binary())
        .arg("run")
        .arg(&src)
        .current_dir(repo_root())
        .output()
        .expect("invoke hew run");
    let mut combined = String::from_utf8_lossy(&output.stdout).into_owned();
    combined.push_str(&String::from_utf8_lossy(&output.stderr));
    (output.status.success(), combined)
}

// ── reject: base reused after consume ─────────────────────────────────────

/// Repro A: the same `base` feeds two functional updates. The first `..base`
/// consumes it; the second reads consumed storage whose overridden owned
/// field has already been destructively freed → double-free at run time.
/// `hew check` must reject the SECOND `..base`.
#[test]
fn reject_base_reused_in_two_updates_is_use_after_consume() {
    let source = r#"
record VHolder { items: Vec<i64>, tag: string }
fn main() {
    let init: Vec<i64> = Vec::new(); init.push(1);
    let base = VHolder { items: init, tag: "base" };
    let next1: Vec<i64> = Vec::new(); next1.push(2);
    let updated1 = VHolder { items: next1, ..base };
    let next2: Vec<i64> = Vec::new(); next2.push(3);
    let updated2 = VHolder { items: next2, ..base };
    println(updated1.items.len());
    println(updated2.items.len());
}
"#;
    let (ok, out) = hew_check(source);
    assert!(!ok, "base-reuse must fail check; got success:\n{out}");
    assert!(
        out.contains("used after it was consumed") || out.contains("UseAfterConsume"),
        "expected UseAfterConsume on the second `..base`; got:\n{out}"
    );
}

/// A `base.field` READ after the base was consumed by `..base` must also be
/// rejected (the single-override-then-reuse shape, repro T3).
#[test]
fn reject_base_field_read_after_consume_is_use_after_consume() {
    let source = r#"
record VHolder { items: Vec<i64>, tag: string }
fn main() {
    let init: Vec<i64> = Vec::new(); init.push(7);
    let base = VHolder { items: init, tag: "base" };
    let next: Vec<i64> = Vec::new(); next.push(8);
    let updated = VHolder { items: next, ..base };
    println(updated.items.len());
    println(base.items.len());
}
"#;
    let (ok, out) = hew_check(source);
    assert!(!ok, "base.field read after consume must fail check:\n{out}");
    assert!(
        out.contains("used after it was consumed") || out.contains("UseAfterConsume"),
        "expected UseAfterConsume on the post-update `base.items` read; got:\n{out}"
    );
}

// ── reject: self-override aliasing the consumed base ───────────────────────

/// Repro B: the override value `s.items` bare-aliases the same field the
/// destructive release would free. Until the COW value model lands this is a
/// fail-closed `NotYetImplemented`, not a silent miscompile.
#[test]
fn reject_self_override_aliasing_consumed_base() {
    let source = r#"
record VHolder { items: Vec<i64>, tag: string }
fn main() {
    let init: Vec<i64> = Vec::new(); init.push(7);
    let s = VHolder { items: init, tag: "base" };
    let s2 = VHolder { items: s.items, ..s };
    println(s2.items.len());
}
"#;
    let (ok, out) = hew_check(source);
    assert!(!ok, "self-override aliasing base must fail check:\n{out}");
    assert!(
        out.contains("aliasing the consumed base") || out.contains("E_NOT_YET_IMPLEMENTED"),
        "expected NotYetImplemented for the self-aliasing override; got:\n{out}"
    );
}

// ── accept: the canonical reassign-loop idiom ──────────────────────────────

/// `h = T { ..h, f: new }` in a loop re-defines `h` each iteration, so the
/// consume of the old `h` is superseded by the fresh binding. Must `check`
/// and `run` cleanly — this is the load-bearing functional-update idiom.
#[test]
fn accept_reassign_loop_idiom_runs_clean() {
    require_codegen();
    let source = r"
record VecHolder { items: Vec<i64>, tag: i64 }
fn main() {
    let init: Vec<i64> = Vec::new(); init.push(99);
    var h = VecHolder { items: init, tag: 0 };
    var i: i64 = 0;
    while i < 8 {
        let next: Vec<i64> = Vec::new(); next.push(i);
        h = VecHolder { items: next, ..h };
        i = i + 1;
    }
    println(h.items.len());
}
";
    let (ok, out) = hew_run(source);
    assert!(ok, "reassign-loop idiom must run cleanly; got:\n{out}");
    assert!(
        out.contains('1'),
        "reassign loop should leave a 1-element vec; got:\n{out}"
    );
}

/// The explicit-clone override `{ items: s.items.clone(), ..s }` is the safe
/// way to thread a base field through an update: the clone is a fresh +1
/// owner, so consuming `s` and releasing its old `items` cannot dangle. Must
/// `check` and `run` cleanly.
#[test]
fn accept_clone_override_runs_clean() {
    require_codegen();
    let source = r"
record VecHolder { items: Vec<i64>, tag: i64 }
fn main() {
    let init: Vec<i64> = Vec::new(); init.push(5);
    let s = VecHolder { items: init, tag: 1 };
    let s2 = VecHolder { items: s.items.clone(), ..s };
    println(s2.items.len());
}
";
    let (ok, out) = hew_run(source);
    assert!(ok, "clone-override must run cleanly; got:\n{out}");
    assert!(
        out.contains('1'),
        "clone-override should yield a 1-element vec; got:\n{out}"
    );
}
