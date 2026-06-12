//! Type-checker signature tests for `link(handle)` and `monitor(handle)`.
//!
//! B2 slice: pins that
//!   - `link(actor_ref)` → `Result<(), LinkError>`
//!   - `monitor(actor_ref)` → `MonitorRef`
//!   - `link(non_actor)` → `TYPE_MISMATCH`
//!   - `monitor(non_actor)` → `TYPE_MISMATCH`

mod common;

use common::typecheck_isolated as typecheck;
use hew_types::error::TypeErrorKind;

// ── actor fixture ─────────────────────────────────────────────────────────────

const ACTOR_DECL: &str = r"
actor Worker {
    receive fn ping() {}
}
";

fn with_actor(body: &str) -> String {
    format!("{ACTOR_DECL}\nfn main() {{\n{body}\n}}")
}

// ── link return type ──────────────────────────────────────────────────────────

#[test]
fn link_actor_ref_returns_result_link_error() {
    // `link(handle)` must produce `Result<(), LinkError>`.
    // Binding to that type must typecheck without errors.
    let src = with_actor(
        r"
        let w = spawn Worker;
        let r: Result<(), LinkError> = link(w);
    ",
    );
    let out = typecheck(&src);
    assert!(
        out.errors.is_empty(),
        "link(actor_ref) should produce Result<(), LinkError> with no errors; got: {:?}",
        out.errors
    );
}

#[test]
fn link_result_can_be_ignored_as_statement() {
    // Using `link(worker)` as a bare statement (result discarded) must still
    // typecheck — callers are not required to handle the error.
    let src = with_actor(
        r"
        let w = spawn Worker;
        link(w);
    ",
    );
    let out = typecheck(&src);
    assert!(
        out.errors.is_empty(),
        "link(actor_ref) as statement should typecheck without errors; got: {:?}",
        out.errors
    );
}

#[test]
fn link_result_bound_to_let_wildcard_typechecks() {
    // `let _ = link(w)` must typecheck without errors.
    let src = with_actor(
        r"
        let w = spawn Worker;
        let _ = link(w);
    ",
    );
    let out = typecheck(&src);
    assert!(
        out.errors.is_empty(),
        "let _ = link(actor_ref) should typecheck without errors; got: {:?}",
        out.errors
    );
}

// ── monitor return type ───────────────────────────────────────────────────────

#[test]
fn monitor_actor_ref_returns_monitor_ref() {
    // `monitor(handle) -> MonitorRef` — binding to that type must produce no errors.
    let src = with_actor(
        r"
        let w = spawn Worker;
        let m: MonitorRef = monitor(w);
    ",
    );
    let out = typecheck(&src);
    assert!(
        out.errors.is_empty(),
        "monitor(actor_ref) should produce MonitorRef with no errors; got: {:?}",
        out.errors
    );
}

#[test]
fn monitor_ref_not_assignable_to_int() {
    // `MonitorRef` must not unify with `i64` — the type system must reject this.
    let src = with_actor(
        r"
        let w = spawn Worker;
        let _x: i64 = monitor(w);
    ",
    );
    let out = typecheck(&src);
    assert!(
        out.errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::Mismatch { .. })),
        "monitor(actor_ref) assigned to i64 should produce a Mismatch error; got: {:?}",
        out.errors
    );
}

// ── non-actor rejection ───────────────────────────────────────────────────────

#[test]
fn link_non_actor_produces_mismatch() {
    // `link` requires `ActorRef<_>`; passing a plain `i64` must be rejected.
    let src = r"
fn main() {
    let x: i64 = 42;
    let _ = link(x);
}
";
    let out = typecheck(src);
    assert!(
        out.errors.iter().any(|e| {
            matches!(
                &e.kind,
                TypeErrorKind::Mismatch { .. } | TypeErrorKind::InvalidOperation
            )
        }),
        "link(non_actor) should produce a type error; got: {:?}",
        out.errors
    );
}

#[test]
fn monitor_non_actor_produces_mismatch() {
    // `monitor` requires `ActorRef<_>`; passing a `string` must be rejected.
    let src = r#"
fn main() {
    let s: string = "hello";
    let _ = monitor(s);
}
"#;
    let out = typecheck(src);
    assert!(
        out.errors.iter().any(|e| {
            matches!(
                &e.kind,
                TypeErrorKind::Mismatch { .. } | TypeErrorKind::InvalidOperation
            )
        }),
        "monitor(non_actor) should produce a type error; got: {:?}",
        out.errors
    );
}
