mod common;

use common::typecheck;
use hew_types::error::TypeErrorKind;

// ── LocalPid.tell method compiles ────────────────────────────────────────────

#[test]
fn local_pid_tell_method_compiles() {
    // LocalPid<T> has `fn tell(pid: LocalPid<T>, msg: T)` in its impl block.
    // When called as `pid.tell(msg)`, the checker dispatches via the user-type
    // method table populated from the `impl LocalPid<T>` block in std/builtins.hew.
    //
    // Note: `T` in `LocalPid<T>` is the actor type, so `tell` accepts a value
    // of the actor type as the message parameter. This is the S1 scaffold;
    // per-actor message typing is a follow-on concern when associated types land.
    //
    // This test validates that: (a) LocalPid method dispatch works, and
    // (b) the method lookup doesn't produce an "unknown method" error.
    let output = typecheck(
        r"
        actor Worker {
            let id: i32;
            init() {}
        }
        fn main() {
            let w = spawn Worker(id: 0);
            let _ww = spawn Worker(id: 1);
            // `w.tell(_ww)` sends a Worker (actor type T) to another Worker —
            // this is the type the impl produces for S1.
            w.tell(_ww);
        }
    ",
    );
    // The S1 encoding fixes `T` in `LocalPid<T>` to the actor type, so
    // `tell` has signature `(LocalPid<T>, T)` with T=Worker — i.e. the second
    // arg expects a `Worker`, not a `LocalPid<Worker>`. Passing _ww (itself a
    // `LocalPid<Worker>`) produces a Mismatch on the message arg. This is the
    // documented Pid<T> trait conflict motivating the associated-types lane.
    //
    // What this test pins: method dispatch found `tell` on `LocalPid` (no
    // UndefinedMethod error), and the only diagnostic is the expected Mismatch.
    let undefined = output
        .errors
        .iter()
        .any(|e| matches!(e.kind, TypeErrorKind::UndefinedMethod));
    assert!(
        !undefined,
        "tell method should be dispatched on LocalPid (no UndefinedMethod): {:#?}",
        output.errors
    );
    assert!(
        output
            .errors
            .iter()
            .all(|e| matches!(e.kind, TypeErrorKind::Mismatch { .. })),
        "LocalPid.tell: only Mismatch errors are tolerated in S1 (T vs actor-type drift): {:#?}",
        output.errors
    );
}

// ── Pid<T> trait: declared in builtins ──────────────────────────────────────

#[test]
fn pid_trait_parseable_in_builtins() {
    // Verify that `Pid<T>` is declared in std/builtins.hew and the checker
    // can load builtins without parse errors. The presence of LocalPid and
    // RemotePid registrations is tested in m3_surface_pid_types.rs.
    //
    // This test uses a minimal program that doesn't exercise Pid<T> bounds
    // but does trigger builtins.hew loading (which happens on every typecheck).
    let output = typecheck(
        r"
        actor Ping {
            let x: i32;
            init() {}
        }
        fn main() {
            let _p = spawn Ping(x: 0);
        }
    ",
    );
    assert!(
        output.errors.is_empty(),
        "builtins.hew with Pid<T> should parse and load cleanly: {:#?}",
        output.errors
    );
}

// ── LocalPid.to_remote_via stub compiles ─────────────────────────────────────

#[test]
fn local_pid_to_remote_via_stub_compiles() {
    // The `to_remote_via` method is a SHIM stub. Verify the surface compiles —
    // that is, the method is found in LocalPid's type def and the call doesn't
    // produce an "unknown method" error.
    //
    // The stub accepts `dyn Display` for node_handle so any printable value works.
    let output = typecheck(
        r#"
        actor Bot {
            let n: i32;
            init() {}
        }
        fn main() {
            let local = spawn Bot(n: 0);
            let _remote = local.to_remote_via("node-1");
        }
    "#,
    );
    // to_remote_via has signature `(LocalPid<T>, dyn Display) -> RemotePid<T>`.
    // The current S1 checker does not unify a concrete `string` against the
    // `dyn Display` slot (a separate trait-object-coercion gap), so a Mismatch
    // fires on the second arg. This test pins: method dispatch found
    // `to_remote_via` on `LocalPid` (no UndefinedMethod), and only Mismatch
    // errors are tolerated. The dyn-coercion gap is out of scope for M3-S1.
    let undefined = output
        .errors
        .iter()
        .any(|e| matches!(e.kind, TypeErrorKind::UndefinedMethod));
    assert!(
        !undefined,
        "to_remote_via should be dispatched on LocalPid (no UndefinedMethod): {:#?}",
        output.errors
    );
    assert!(
        output
            .errors
            .iter()
            .all(|e| matches!(e.kind, TypeErrorKind::Mismatch { .. })),
        "to_remote_via: only Mismatch errors are tolerated in S1: {:#?}",
        output.errors
    );
}

// Note: a `structural_pid_satisfaction_probe` test was removed here. It captured
// the checker's verdict on `LocalPid<Worker>` satisfying `Pid<Worker>` without
// asserting a direction (probe only). Per CLAUDE.md test discipline, a test
// that cannot fail for a plausible bug is not a test. The Pid<T> trait
// type-parameter conflict is the motivation for the associated-types lane that
// follows; that lane will introduce a real assertion for the satisfaction
// question.

// ── RemotePid.tell method found ───────────────────────────────────────────────

#[test]
fn remote_pid_tell_method_found() {
    // RemotePid<T> has `fn tell(pid: RemotePid<T>, msg: T)` declared as a
    // fail-closed stub. This test verifies that a RemotePid value (obtained
    // via to_remote_via) finds the `tell` method.
    let output = typecheck(
        r#"
        actor Bot {
            let n: i32;
            init() {}
        }
        fn main() {
            let local = spawn Bot(n: 0);
            let remote = local.to_remote_via("peer");
            let _b2 = spawn Bot(n: 1);
            remote.tell(_b2);
        }
    "#,
    );
    // remote.tell signature is `(RemotePid<T>, T)`. With the S1 encoding
    // T=Bot (actor type), passing a `LocalPid<Bot>` as the second arg
    // produces a Mismatch (the documented Pid<T> trait conflict). The
    // earlier `to_remote_via` call also produces the dyn Display gap.
    // What this test pins: method dispatch found `tell` on `RemotePid`
    // (no UndefinedMethod), and only Mismatch errors are tolerated.
    let undefined = output
        .errors
        .iter()
        .any(|e| matches!(e.kind, TypeErrorKind::UndefinedMethod));
    assert!(
        !undefined,
        "tell should be dispatched on RemotePid (no UndefinedMethod): {:#?}",
        output.errors
    );
    assert!(
        output
            .errors
            .iter()
            .all(|e| matches!(e.kind, TypeErrorKind::Mismatch { .. })),
        "RemotePid.tell: only Mismatch errors are tolerated in S1: {:#?}",
        output.errors
    );
}
