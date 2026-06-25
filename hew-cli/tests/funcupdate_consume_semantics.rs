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
//!
//! ## Base-shape fixtures — fail-closed ALLOWLIST (projection / index / rvalue)
//!
//! The destructive base is gated by a fail-closed ALLOWLIST
//! (`base_is_safe_for_destructive_funcupdate`): the override-drop and the
//! shallow carry proceed ONLY when the base is PROVABLY safe — a bare
//! consume-marked binding, or a materialised owner with no live alias (a
//! call / `.clone()` result, a `Vec` element `v[i]`, or a projection rooted at
//! one). EVERY other base is rejected `NotYetImplemented`. This replaced an
//! earlier denylist that enumerated unsafe projection shapes and repeatedly
//! missed cases: `FieldAccess` (`..o.inner`), then `Index`, then `TupleIndex`
//! (`..t.0`, which lowers to an aliasing `TupleFieldLoad`) — each a fresh
//! use-after-free of the projected field plus a double-free at the live
//! binding's scope-exit drop. The allowlist is complete by construction: no
//! projection shape — field, tuple-index, nested, machine-state (`self.field`),
//! or any future expr form — can slip.
//!
//! Rejected (interior-alias a live binding): `..o.inner`, `..t.0`,
//! `..o.pair.0`, `..t.0.inner`, and a machine-state `..self.payload`. Accepted
//! (consumed binding or materialised owner): a bare base (`..base`), an owned
//! rvalue (`..makeInner()`), a projection of a temporary (`..makeOuter().inner`
//! — rooted at a call), an indexed base (`..v[i]`), a projection through an
//! index (`..o.items[0].inner`), and the `.clone()` escape hatch
//! (`..o.inner.clone()`). The authoritative memory-safety oracle is the
//! external Guard-Malloc + `MallocScribble` repro matrix (the in-suite `run`
//! fixtures use plain malloc).
//!
//! ## Wrapper completeness — value-passthrough forms (block / if / match)
//!
//! The allowlist is COMPLETE THROUGH value-passthrough wrappers: a block tail,
//! both `if` branches, and every `match` arm body are looked THROUGH, and the
//! base is admitted only when EVERY reachable value is itself a materialised
//! owner. A wrapper that can yield a live-binding projection is rejected
//! fail-closed — `..if c { o.inner } else { makeInner() }`,
//! `..{ let _z = 0; o.inner }`, `..match c { _ => o.inner }` — and so is a
//! block-wrapped bare binding (`..{ base }`): the consume cannot peel the
//! block, so the base must prove materialised, which a bare-binding tail fails
//! (admitting it while the consume silently no-ops on the block is itself a
//! UAF). An all-rvalue wrapper whose every branch / arm / tail is a fresh owned
//! value is accepted and runs clean (`..if c { makeInner() } else { makeOther() }`).
//!
//! ## Owned-aggregate override + closure-pair / Generator inline-drop
//!
//! Overriding an owned-AGGREGATE field (record / tuple / enum) is a
//! fail-closed `NotYetImplemented` (the in-place aggregate drop kinds are
//! function-scope, not inline `Instr::Drop` targets). Separately, the
//! single-pointer COW leaf-release authority `project_field_inline_drop_symbol`
//! — shared by the functional-update override-drop AND the match-destructure
//! wildcard inline-drop — must agree with codegen's `cow_heap_release_symbol`
//! for EVERY type, including a closure-pair `Vec<fn>` (release symbol
//! `hew_vec_free_closure_pairs`, not `hew_vec_free`) and a `Generator`
//! (`hew_gen_coro_destroy`). A tuple wildcard-destructure over those element
//! types exercises the shared authority end-to-end and must run clean.

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

// ── reject: projection of a LIVE binding base (the residual P0 UAF) ─────────

/// `{ ..o.inner, label: new }` projects a field of the live binding `o`. The
/// projection is NOT consume-marked (`o` stays live), but the overridden owned
/// `label` is released in place — so a later `o.inner.label` read is a
/// use-after-free and `o`'s scope-exit drop double-frees it. Empirically this
/// compiled clean and corrupted memory (a `free_cstring` double-free abort;
/// a crash on the freed read under Guard Malloc). It must now FAIL `hew check`
/// fail-closed (`NotYetImplemented`); the compile error is the safe outcome.
#[test]
fn reject_projection_of_live_binding_base() {
    let source = r#"
import std::string;
record Inner { label: string, n: i64 }
record Outer { inner: Inner, tag: i64 }
fn main() {
    let o = Outer { inner: Inner { label: string.repeat("a", 32), n: 1 }, tag: 9 };
    let u = Inner { label: string.repeat("b", 32), ..o.inner };
    println(u.label);
    println(o.inner.label);
}
"#;
    let (ok, out) = hew_check(source);
    assert!(
        !ok,
        "projection-of-live-binding base must fail check; got success:\n{out}"
    );
    assert!(
        out.contains("not a binding or owned value")
            && out.contains("field projection of a live binding"),
        "expected the fail-closed allowlist NotYetImplemented; got:\n{out}"
    );
}

/// Carry variant: `{ ..o.inner, n: 5 }` overrides only the `BitCopy` `n` and
/// CARRIES the owned `label` from the live projection. No override-drop fires,
/// but the carried owned field is raw-loaded (aliased) into the new record
/// while `o` stays live — so `u` and `o.inner` share one `label` buffer and
/// `o`'s scope-exit drop double-frees it. The reject is therefore gated on the
/// base TYPE being an owned aggregate (not on the overridden field being
/// owned): a bare-binding carry is safe only because `..base` consumes the
/// base, which a live projection never does.
#[test]
fn reject_projection_of_live_binding_base_carry_only() {
    let source = r#"
import std::string;
record Inner { label: string, n: i64 }
record Outer { inner: Inner, tag: i64 }
fn main() {
    let o = Outer { inner: Inner { label: string.repeat("a", 32), n: 1 }, tag: 9 };
    let u = Inner { n: 5, ..o.inner };
    println(u.label);
    println(o.inner.label);
}
"#;
    let (ok, out) = hew_check(source);
    assert!(
        !ok,
        "projection-of-live-binding carry-only base must fail check; got success:\n{out}"
    );
    assert!(
        out.contains("not a binding or owned value")
            && out.contains("field projection of a live binding"),
        "expected the fail-closed allowlist NotYetImplemented; got:\n{out}"
    );
}

// ── reject: TupleIndex / nested / machine-state projection bases ────────────
//
// These pin the STRUCTURAL fix: the destructive base allowlist
// (`base_is_safe_for_destructive_funcupdate`). The prior denylist enumerated
// only `FieldAccess` projections, so a base that projects an owned record out
// of a live binding through a `TupleIndex` (`..t.0`) — which lowers to an
// aliasing `TupleFieldLoad`, not a materialising clone — slipped through and
// reopened the use-after-free. The allowlist admits a base ONLY when it is a
// bare consume-marked binding or a materialised owner, so EVERY projection of
// a live binding (field, tuple-index, nested, machine-state) is rejected by
// construction.

/// `..t.0` override: a `TupleIndex` projection of a live tuple binding. The
/// overridden owned `label` would be released in place on `t.0` while `t`
/// stays live — a use-after-free of `t.0.label` and a double-free at `t`'s
/// scope-exit drop. (The `TupleIndex` use-after-free this fix closes.)
#[test]
fn reject_tuple_index_of_live_binding_base() {
    let source = r#"
import std::string;
record Inner { label: string, n: i64 }
fn main() {
    let t = (Inner { label: string.repeat("a", 32), n: 1 }, 7);
    let u = Inner { label: string.repeat("b", 32), ..t.0 };
    println(u.label);
    println(t.0.label);
}
"#;
    let (ok, out) = hew_check(source);
    assert!(
        !ok,
        "tuple-index-of-live-binding base must fail check; got success:\n{out}"
    );
    assert!(
        out.contains("not a binding or owned value"),
        "expected the fail-closed allowlist NotYetImplemented; got:\n{out}"
    );
}

/// `..t.0` carry-only: overrides only the `BitCopy` `n`, CARRIES the owned
/// `label`. The carry is a shallow `RecordFieldLoad` aliasing `t.0.label`
/// while `t` stays live — `u` and `t.0` share one buffer, double-freed at
/// scope exit. Rejected on the base TYPE (owned aggregate), not the overridden
/// field.
#[test]
fn reject_tuple_index_of_live_binding_base_carry_only() {
    let source = r#"
import std::string;
record Inner { label: string, n: i64 }
fn main() {
    let t = (Inner { label: string.repeat("a", 32), n: 1 }, 7);
    let u = Inner { n: 5, ..t.0 };
    println(u.label);
    println(t.0.label);
}
"#;
    let (ok, out) = hew_check(source);
    assert!(
        !ok,
        "tuple-index carry-only base must fail check; got success:\n{out}"
    );
    assert!(
        out.contains("not a binding or owned value"),
        "expected the fail-closed allowlist NotYetImplemented; got:\n{out}"
    );
}

/// `..o.pair.0`: a `TupleIndex` reached THROUGH a `FieldAccess` of a live
/// binding. The projection chain bottoms out at the live `o`, so it aliases
/// `o.pair.0`'s storage. Rejected fail-closed.
#[test]
fn reject_tuple_index_through_field_of_live_binding_base() {
    let source = r#"
import std::string;
record Inner { label: string, n: i64 }
record Outer { pair: (Inner, i64), tag: i64 }
fn main() {
    let o = Outer { pair: (Inner { label: string.repeat("a", 32), n: 1 }, 7), tag: 9 };
    let u = Inner { label: string.repeat("b", 32), ..o.pair.0 };
    println(u.label);
    println(o.pair.0.label);
}
"#;
    let (ok, out) = hew_check(source);
    assert!(
        !ok,
        "tuple-index-through-field base must fail check; got success:\n{out}"
    );
    assert!(
        out.contains("not a binding or owned value"),
        "expected the fail-closed allowlist NotYetImplemented; got:\n{out}"
    );
}

/// `..t.0.inner`: a `FieldAccess` reached THROUGH a `TupleIndex` of a live
/// binding. The chain bottoms out at the live `t`, so it aliases
/// `t.0.inner`'s storage. Rejected fail-closed.
#[test]
fn reject_field_through_tuple_index_of_live_binding_base() {
    let source = r#"
import std::string;
record Inner { label: string, n: i64 }
record Mid { inner: Inner, k: i64 }
fn main() {
    let t = (Mid { inner: Inner { label: string.repeat("a", 32), n: 1 }, k: 3 }, 7);
    let u = Inner { label: string.repeat("b", 32), ..t.0.inner };
    println(u.label);
    println(t.0.inner.label);
}
"#;
    let (ok, out) = hew_check(source);
    assert!(
        !ok,
        "field-through-tuple-index base must fail check; got success:\n{out}"
    );
    assert!(
        out.contains("not a binding or owned value"),
        "expected the fail-closed allowlist NotYetImplemented; got:\n{out}"
    );
}

/// `..self.payload`: a machine-state field projection (`MachineFieldAccess`)
/// inside a reenter transition, where `payload` is an owned record. This is a
/// SECOND shape the old `FieldAccess`-only denylist missed (`MachineFieldAccess`
/// is a distinct HIR variant). The projection aliases the live machine state's
/// payload storage; the override-drop would free it in place. Rejected
/// fail-closed by the allowlist (which admits only bindings / materialised
/// owners). A `BitCopy` payload base is exempt (no override-drop) and is NOT
/// rejected — the destructive base allowlist is type-fenced on the base being
/// an owned aggregate.
#[test]
fn reject_machine_state_field_projection_base() {
    let source = r#"
import std::string;
record Inner { label: string, n: i64 }
machine Holder {
    events { Bump; }
    state Empty;
    state Full { payload: Inner; }
    on Bump: Empty => Full {
        Full { payload: Inner { label: string.repeat("a", 32), n: 1 } }
    }
    on Bump: Full => Full reenter {
        Full { payload: Inner { label: string.repeat("b", 32), ..self.payload } }
    }
}
fn main() {
    var m = Empty;
    m.step(Bump);
    m.step(Bump);
    println(m.state_name());
}
"#;
    let (ok, out) = hew_check(source);
    assert!(
        !ok,
        "machine-state field projection base must fail check; got success:\n{out}"
    );
    assert!(
        out.contains("not a binding or owned value") && out.contains("self.field"),
        "expected the fail-closed allowlist NotYetImplemented naming self.field; got:\n{out}"
    );
}

// ── reject: value-passthrough WRAPPER bases (block / if / match) ────────────
//
// A wrapper — a block (`{ ..; tail }`), an `if`, or a `match` — is a value-
// passthrough form: its value is the tail / the selected branch / the winning
// arm. The allowlist looks THROUGH each wrapper (`expr_is_materialized_owner`)
// and admits the base only when EVERY reachable value is a materialised owner.
// A live-binding projection reachable through ANY wrapper is rejected fail-
// closed: the consume does not peel the wrapper, and a conditionally-selected
// binding cannot be soundly consumed, so the override-drop would free storage
// the live owner still references. The `if`/`block` wrapper-escape was a
// residual use-after-free an earlier denylist shipped (a live projection
// hidden behind `if true { … }` / `{ … }`); these pin it closed.

/// `..if true { o.inner } else { makeInner() }`: the then-branch yields a
/// projection of the live binding `o`. One reachable value aliases `o`'s
/// interior, so the whole `if` base is rejected — even though the else-branch
/// is a materialised owner. (The `if`-wrapper escape this completes.)
#[test]
fn reject_wrapper_if_live_projection_base() {
    let source = r#"
import std::string;
record Inner { label: string, n: i64 }
record Outer { inner: Inner, tag: i64 }
fn makeInner() -> Inner { Inner { label: string.repeat("a", 32), n: 1 } }
fn main() {
    let o = Outer { inner: Inner { label: string.repeat("a", 32), n: 1 }, tag: 9 };
    let u = Inner { label: string.repeat("b", 32), ..if true { o.inner } else { makeInner() } };
    println(u.label);
    println(o.inner.label);
}
"#;
    let (ok, out) = hew_check(source);
    assert!(
        !ok,
        "if-wrapper over a live-binding projection must fail check; got success:\n{out}"
    );
    assert!(
        out.contains("not a binding or owned value"),
        "expected the fail-closed allowlist NotYetImplemented; got:\n{out}"
    );
}

/// `..{ let _z = 0; o.inner }`: a non-empty block whose tail projects the live
/// binding `o`. The block is looked through to its tail (statements are side-
/// effecting only); the tail aliases `o`, so the base is rejected. (The
/// block-wrapper escape this completes.)
#[test]
fn reject_block_wrapper_live_projection_base() {
    let source = r#"
import std::string;
record Inner { label: string, n: i64 }
record Outer { inner: Inner, tag: i64 }
fn main() {
    let o = Outer { inner: Inner { label: string.repeat("a", 32), n: 1 }, tag: 9 };
    let u = Inner { label: string.repeat("b", 32), ..{ let _z: i64 = 0; o.inner } };
    println(u.label);
    println(o.inner.label);
}
"#;
    let (ok, out) = hew_check(source);
    assert!(
        !ok,
        "block-wrapper over a live-binding projection must fail check; got success:\n{out}"
    );
    assert!(
        out.contains("not a binding or owned value"),
        "expected the fail-closed allowlist NotYetImplemented; got:\n{out}"
    );
}

/// `..match flag { true => o.inner, false => makeInner() }`: a `match` arm body
/// projects the live binding `o`. Every arm body must be a materialised owner;
/// the `true` arm aliases `o`, so the whole `match` base is rejected.
#[test]
fn reject_match_wrapper_live_projection_base() {
    let source = r#"
import std::string;
record Inner { label: string, n: i64 }
record Outer { inner: Inner, tag: i64 }
fn makeInner() -> Inner { Inner { label: string.repeat("a", 32), n: 1 } }
fn main() {
    let o = Outer { inner: Inner { label: string.repeat("a", 32), n: 1 }, tag: 9 };
    let flag = true;
    let u = Inner { label: string.repeat("b", 32), ..match flag { true => o.inner, false => makeInner() } };
    println(u.label);
    println(o.inner.label);
}
"#;
    let (ok, out) = hew_check(source);
    assert!(
        !ok,
        "match-wrapper over a live-binding projection must fail check; got success:\n{out}"
    );
    assert!(
        out.contains("not a binding or owned value"),
        "expected the fail-closed allowlist NotYetImplemented; got:\n{out}"
    );
}

/// `..{ base }`: a block-wrapped bare binding. The consume marker
/// (`alias_moved_owned_operand`) fires only on a SYNTACTICALLY bare `BindingRef`
/// — it does NOT peel the block, so `base` is never consume-marked. Admitting
/// this as the bare-binding case while the consume silently no-ops would leave
/// `base` live AND destructively release its overridden field — a use-after-
/// free plus a double-free at `base`'s scope-exit drop. The base must instead
/// prove materialised (it cannot: the tail is a bare binding), so it is
/// rejected fail-closed.
#[test]
fn reject_block_wrapped_binding_base() {
    let source = r#"
import std::string;
record Inner { label: string, n: i64 }
fn main() {
    let base = Inner { label: string.repeat("a", 32), n: 1 };
    let u = Inner { label: string.repeat("b", 32), ..{ base } };
    println(u.label);
    println(base.label);
}
"#;
    let (ok, out) = hew_check(source);
    assert!(
        !ok,
        "block-wrapped bare-binding base must fail check; got success:\n{out}"
    );
    assert!(
        out.contains("not a binding or owned value"),
        "expected the fail-closed allowlist NotYetImplemented; got:\n{out}"
    );
}

// ── reject: overriding an owned-AGGREGATE field ────────────────────────────

/// Overriding a record/tuple/enum field (`inner: Inner` here) in a
/// functional-update has no single-pointer leaf release symbol — the in-place
/// aggregate drop kinds (`RecordInPlace` / `TupleInPlace` / `EnumInPlace`) are
/// function-scope drops, not inline `Instr::Drop` targets. Fail-closed
/// `NotYetImplemented` rather than emit a leak / wrong-ABI free.
#[test]
fn reject_owned_aggregate_field_override() {
    let source = r#"
record Inner { label: string, n: i64 }
record Outer { inner: Inner, tag: i64 }
fn main() {
    let init = Outer { inner: Inner { label: "x", n: 1 }, tag: 0 };
    let updated = Outer { inner: Inner { label: "y", n: 2 }, ..init };
    println(updated.tag);
}
"#;
    let (ok, out) = hew_check(source);
    assert!(
        !ok,
        "owned-aggregate field override must fail check; got success:\n{out}"
    );
    assert!(
        out.contains("override of owned-aggregate field") || out.contains("owned-aggregate type"),
        "expected the owned-aggregate override NotYetImplemented; got:\n{out}"
    );
}

// ── accept: base shapes that are NOT interior aliases of a live binding ─────

/// An owned rvalue base (`{ ..makeInner(), f: new }`): the call result is a
/// consumed temporary with no surviving named alias. The overridden owned
/// `label` of the temporary is released, the temporary's other fields move
/// into the new record. Must `check` and `run` cleanly (no false reject).
#[test]
fn accept_owned_rvalue_base_runs_clean() {
    require_codegen();
    let source = r#"
import std::string;
record Inner { label: string, n: i64 }
fn makeInner() -> Inner { Inner { label: string.repeat("a", 32), n: 1 } }
fn main() {
    let u = Inner { label: string.repeat("b", 32), ..makeInner() };
    println(u.n);
}
"#;
    let (ok, out) = hew_run(source);
    assert!(ok, "owned-rvalue base must run cleanly; got:\n{out}");
    assert!(
        out.contains('1'),
        "owned-rvalue base should carry n=1 from the temporary; got:\n{out}"
    );
}

/// A projection of a TEMPORARY (`{ ..makeOuter().inner, f: new }`): the base
/// is rooted at a call, not a live binding, so there is no surviving alias of
/// the released field. Must `check` and `run` cleanly (not caught by the
/// projection-of-live-binding reject).
#[test]
fn accept_projection_of_temporary_base_runs_clean() {
    require_codegen();
    let source = r#"
import std::string;
record Inner { label: string, n: i64 }
record Outer { inner: Inner, tag: i64 }
fn makeOuter() -> Outer { Outer { inner: Inner { label: string.repeat("a", 32), n: 1 }, tag: 9 } }
fn main() {
    let u = Inner { label: string.repeat("b", 32), ..makeOuter().inner };
    println(u.n);
}
"#;
    let (ok, out) = hew_run(source);
    assert!(
        ok,
        "projection-of-temporary base must run cleanly; got:\n{out}"
    );
    assert!(
        out.contains('1'),
        "projection-of-temporary base should carry n=1; got:\n{out}"
    );
}

/// An INDEXED base (`{ ..v[0], f: new }`): a `Vec<T>` element load. The
/// element is independent of any live binding — `hew_vec_push_owned`
/// deep-clones on insert and the buffer carries its own refcount, so the
/// override-drop's in-place release decrements a shared count rather than
/// freeing storage `v[0]` still references. Must `check` and `run` cleanly
/// with the source element's OWNED `label` left intact (not just the `BitCopy`
/// `n`). (The external Guard-Malloc matrix is the authoritative oracle; this
/// reads the owned field under plain malloc as a coarse in-suite check.)
#[test]
fn accept_index_of_live_binding_base_runs_clean() {
    require_codegen();
    let source = r#"
import std::string;
record Inner { label: string, n: i64 }
fn main() {
    let v: Vec<Inner> = Vec::new();
    v.push(Inner { label: string.repeat("a", 32), n: 7 });
    let u = Inner { label: string.repeat("b", 32), ..v[0] };
    println(u.label);
    println(v[0].label);
    println(v[0].n);
}
"#;
    let (ok, out) = hew_run(source);
    assert!(ok, "indexed base must run cleanly; got:\n{out}");
    assert!(
        out.contains(&"b".repeat(32)),
        "indexed base should apply the override label; got:\n{out}"
    );
    assert!(
        out.contains(&"a".repeat(32)),
        "indexed base should leave v[0]'s owned label intact; got:\n{out}"
    );
    assert!(
        out.contains('7'),
        "indexed base should carry n=7; got:\n{out}"
    );
}

/// A projection THROUGH an index (`{ ..o.items[0].inner, f: new }`): the chain
/// bottoms out at a `Vec` element load, which materialises an independent
/// owner — NOT an alias of the live binding `o`. Must `check` and `run`
/// cleanly (the allowlist recurses the projection to the `Index` root).
#[test]
fn accept_field_through_index_base_runs_clean() {
    require_codegen();
    let source = r#"
import std::string;
record Inner { label: string, n: i64 }
record Mid { inner: Inner, k: i64 }
record Outer { items: Vec<Mid>, tag: i64 }
fn main() {
    let v: Vec<Mid> = Vec::new();
    v.push(Mid { inner: Inner { label: string.repeat("a", 32), n: 7 }, k: 3 });
    let o = Outer { items: v, tag: 9 };
    let u = Inner { label: string.repeat("b", 32), ..o.items[0].inner };
    println(u.label);
    println(o.items[0].inner.label);
}
"#;
    let (ok, out) = hew_run(source);
    assert!(ok, "field-through-index base must run cleanly; got:\n{out}");
    assert!(
        out.contains(&"a".repeat(32)) && out.contains(&"b".repeat(32)),
        "field-through-index base should apply override and leave source intact; got:\n{out}"
    );
}

/// The `.clone()` escape hatch the reject diagnostic recommends: cloning a
/// projection of a live binding (`..o.inner.clone()`) materialises a fresh
/// owned value (`RecordCloneCall`), which the allowlist admits. Must `check`
/// and `run` cleanly with the source binding left intact — this is the
/// sanctioned rewrite for the rejected `..o.inner` shape.
#[test]
fn accept_clone_of_projection_base_runs_clean() {
    require_codegen();
    let source = r#"
import std::string;
record Inner { label: string, n: i64 }
record Outer { inner: Inner, tag: i64 }
fn main() {
    let o = Outer { inner: Inner { label: string.repeat("a", 32), n: 1 }, tag: 9 };
    let u = Inner { label: string.repeat("b", 32), ..o.inner.clone() };
    println(u.label);
    println(o.inner.label);
}
"#;
    let (ok, out) = hew_run(source);
    assert!(ok, "clone-of-projection base must run cleanly; got:\n{out}");
    assert!(
        out.contains(&"a".repeat(32)) && out.contains(&"b".repeat(32)),
        "clone-of-projection base should apply override and leave source intact; got:\n{out}"
    );
}

// ── accept: all-rvalue WRAPPER bases (block / if / match) ──────────────────

/// `..if c { makeInner() } else { makeOther() }`: every branch is a fresh owned
/// call result, so whichever branch is selected the base is a materialised
/// owner with no live alias. The allowlist looks through the `if` and admits
/// it. Must `check` and `run` cleanly (the sound sibling of the rejected
/// `if`-wrapper-over-live-projection).
#[test]
fn accept_all_rvalue_if_wrapper_base_runs_clean() {
    require_codegen();
    let source = r#"
import std::string;
record Inner { label: string, n: i64 }
fn makeInner() -> Inner { Inner { label: string.repeat("a", 32), n: 1 } }
fn makeOther() -> Inner { Inner { label: string.repeat("c", 32), n: 2 } }
fn main() {
    let c = true;
    let u = Inner { label: string.repeat("b", 32), ..if c { makeInner() } else { makeOther() } };
    println(u.label);
    println(u.n);
}
"#;
    let (ok, out) = hew_run(source);
    assert!(
        ok,
        "all-rvalue if-wrapper base must run cleanly; got:\n{out}"
    );
    assert!(
        out.contains(&"b".repeat(32)) && out.contains('1'),
        "all-rvalue if-wrapper should apply the override and carry n=1 from makeInner(); got:\n{out}"
    );
}

/// `..match flag { true => makeInner(), false => makeOther() }`: every arm body
/// is a fresh owned call result. The allowlist looks through the `match` and
/// admits it. Must `check` and `run` cleanly.
#[test]
fn accept_all_rvalue_match_wrapper_base_runs_clean() {
    require_codegen();
    let source = r#"
import std::string;
record Inner { label: string, n: i64 }
fn makeInner() -> Inner { Inner { label: string.repeat("a", 32), n: 1 } }
fn makeOther() -> Inner { Inner { label: string.repeat("c", 32), n: 2 } }
fn main() {
    let flag = true;
    let u = Inner { label: string.repeat("b", 32), ..match flag { true => makeInner(), false => makeOther() } };
    println(u.n);
}
"#;
    let (ok, out) = hew_run(source);
    assert!(
        ok,
        "all-rvalue match-wrapper base must run cleanly; got:\n{out}"
    );
    assert!(
        out.contains('1'),
        "all-rvalue match-wrapper should carry n=1 from the true arm; got:\n{out}"
    );
}

/// `..{ println("seed"); makeInner() }`: a non-empty block whose tail is a
/// materialised owner. The block's statement is side-effecting only; its VALUE
/// is the tail call result, which has no live alias. The allowlist peels to the
/// tail regardless of statement count and admits it. Must `check` and `run`
/// cleanly (the sound sibling of the rejected block-wrapper-over-live-projection).
#[test]
fn accept_block_statements_rvalue_tail_base_runs_clean() {
    require_codegen();
    let source = r#"
import std::string;
record Inner { label: string, n: i64 }
fn makeInner() -> Inner { Inner { label: string.repeat("a", 32), n: 1 } }
fn main() {
    let u = Inner { label: string.repeat("b", 32), ..{ println("seed"); makeInner() } };
    println(u.n);
}
"#;
    let (ok, out) = hew_run(source);
    assert!(
        ok,
        "block-with-statements rvalue-tail base must run cleanly; got:\n{out}"
    );
    assert!(
        out.contains("seed") && out.contains('1'),
        "block tail should run the side-effecting statement and carry n=1; got:\n{out}"
    );
}

// ── accept: closure-pair Vec / Generator inline-drop release-symbol parity ──

/// A tuple `(Vec<fn(...)>, i64)` wildcard-destructure releases the discarded
/// `Vec<fn>` field via the shared `project_field_inline_drop_symbol` authority.
/// Its release symbol is `hew_vec_free_closure_pairs` (the per-element env
/// thunk + pair-box walk), NOT `hew_vec_free`. Before the parity fix the MIR
/// authority emitted `hew_vec_free`, which codegen rejected as incongruent
/// with the field type (a hard fail-closed). It must now `check` and `run`
/// cleanly.
#[test]
fn accept_closure_pair_vec_tuple_wildcard_runs_clean() {
    require_codegen();
    let source = r"
fn double(x: i64) -> i64 { x * 2 }
fn main() {
    let t: (Vec<fn(i64) -> i64>, i64) = ([double], 5);
    match t {
        (_, n) => println(n),
    }
}
";
    let (ok, out) = hew_run(source);
    assert!(
        ok,
        "closure-pair Vec tuple wildcard-destructure must run cleanly; got:\n{out}"
    );
    assert!(
        out.contains('5'),
        "wildcard destructure should bind n=5; got:\n{out}"
    );
}

/// A tuple `(Generator<Y, R>, i64)` wildcard-destructure releases the
/// discarded `Generator` field via `hew_gen_coro_destroy` (the coro-frame
/// teardown). Exercises the `Generator` arm of the shared inline-drop
/// authority end-to-end (the funcupdate override path cannot reach it — a
/// record carrying a `Generator` field is blocked earlier at the record-clone
/// front-stop, since the pointer-backed handle has no dup symbol).
#[test]
fn accept_generator_tuple_wildcard_runs_clean() {
    require_codegen();
    let source = r"
fn main() {
    let t: (Generator<i64, ()>, i64) = (gen { yield 1; yield 2; }, 5);
    match t {
        (_, n) => println(n),
    }
}
";
    let (ok, out) = hew_run(source);
    assert!(
        ok,
        "Generator tuple wildcard-destructure must run cleanly; got:\n{out}"
    );
    assert!(
        out.contains('5'),
        "wildcard destructure should bind n=5; got:\n{out}"
    );
}
