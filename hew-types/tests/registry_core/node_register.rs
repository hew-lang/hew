use crate::common;

use hew_types::error::TypeErrorKind;

// ── Deliverable 1: Node::register tightens 2nd arg to T ────────────

/// Passing a freshly-spawned actor handle must typecheck without error.
#[test]
fn node_register_accepts_an_actor_handle() {
    let output = common::typecheck(
        r#"
        actor Worker {
            let n: i32,
            init() {}
        }
        fn main() {
            let pid = spawn Worker(n: 0);
            Node.register("worker", pid);
        }
    "#,
    );
    assert!(
        output.errors.is_empty(),
        "Node::register with an actor handle should typecheck cleanly; got: {:#?}",
        output.errors
    );
}

/// An actor is the type of its handle, so no wrapper type names "any actor"
/// and the registered signature carries a free variable for the operand. The
/// call site proves the operand instead: an integer literal is refused there.
#[test]
fn node_register_rejects_integer_literal() {
    let output = common::typecheck(
        r#"
        fn main() {
            Node.register("worker", 42);
        }
    "#,
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InvalidOperation
                && e.message.contains("expects an actor handle")),
        "Node::register with an integer should be refused as a non-handle operand; got: {:#?}",
        output.errors
    );
}

/// Passing a `RemotePid<T>` (via wrong-type annotation) must be rejected.
#[test]
fn node_register_rejects_remote_pid() {
    let output = common::typecheck(
        r#"
        actor Worker {
            let n: i32,
            init() {}
        }
        fn main() {
            let pid = spawn Worker(n: 0);
            let remote: RemotePid<Worker> = pid;
            Node.register("worker", remote);
        }
    "#,
    );
    assert!(
        !output.errors.is_empty(),
        "Node::register with RemotePid must produce at least one type error"
    );
}

// ── Deliverable 2: IntCmp accepts i32 operands ────────────────────────────────

/// `Node::register` returns `i32`; comparing it with the literal `0` must
/// typecheck without error (regression guard for the i32-vs-i64 width bug).
#[test]
fn node_register_result_eq_zero_typechecks() {
    let output = common::typecheck(
        r#"
        actor Worker {
            let n: i32,
            init() {}
        }
        fn main() {
            let pid = spawn Worker(n: 0);
            let ok: bool = Node.register("worker", pid) == 0;
        }
    "#,
    );
    assert!(
        output.errors.is_empty(),
        "comparing Node::register result (i32) with 0 should typecheck cleanly; got: {:#?}",
        output.errors
    );
}

/// An explicit i32 variable compared with an i32 literal must also be accepted.
#[test]
fn i32_compared_with_literal_typechecks() {
    let output = common::typecheck(
        r"
        fn main() {
            let x: i32 = 7;
            let ok: bool = x == 0;
        }
    ",
    );
    assert!(
        output.errors.is_empty(),
        "i32 == integer literal should typecheck cleanly; got: {:#?}",
        output.errors
    );
}
