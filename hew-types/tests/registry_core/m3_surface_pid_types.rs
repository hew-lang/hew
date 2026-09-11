use crate::common;

use hew_types::Ty;

// ── An actor is the type of its handle (D489): spawn returns the actor's own
// type ──────────────────────────────────────────────────────────────────────

#[test]
fn spawn_returns_the_actor_type() {
    let source = r"
        actor Counter {
            let n: i32,
            init() {}
        }
        fn main() {
            let c = spawn Counter(n: 0);
        }
    ";
    let (prog, output) = common::parse_and_typecheck_inline(source);
    assert!(
        output.errors.is_empty(),
        "should type-check cleanly: {:#?}",
        output.errors
    );
    let main = prog
        .items
        .iter()
        .find_map(|(item, _)| match item {
            hew_parser::ast::Item::Function(fd) if fd.name == "main" => Some(fd),
            _ => None,
        })
        .expect("no main");
    // Find the span of the `spawn Counter(n: 0)` expression (not the let stmt).
    let spawn_span = main
        .body
        .stmts
        .iter()
        .find_map(|(s, _)| match s {
            hew_parser::ast::Stmt::Let {
                value: Some((hew_parser::ast::Expr::Spawn { .. }, span)),
                ..
            } => Some(span.clone()),
            _ => None,
        })
        .expect("no spawn expression in let");
    let key = hew_types::check::SpanKey::from(&spawn_span);
    // spawn should produce Counter itself, the actor handle. Scan all
    // expr_types for any actor-handle entry if the exact span is off.
    let ty = output.expr_types.get(&key).cloned().unwrap_or_else(|| {
        output
            .expr_types
            .values()
            .find(|t| t.actor_handle_identity().is_some())
            .cloned()
            .unwrap_or(Ty::Unit)
    });
    assert_eq!(
        ty.actor_handle_identity(),
        Some(("Counter", &[][..])),
        "expected the Counter actor handle, got {ty:?}"
    );
}

// ── An actor handle: marker traits ────────────────────────────────────────────

#[test]
fn actor_handle_is_send_sync_copy() {
    use hew_types::traits::{MarkerTrait, TraitRegistry};
    let reg = TraitRegistry::new();
    let ty = Ty::actor_handle("Counter", vec![]);
    for marker in [
        MarkerTrait::Send,
        MarkerTrait::Sync,
        MarkerTrait::Copy,
        MarkerTrait::Clone,
        MarkerTrait::Frozen,
        MarkerTrait::Debug,
    ] {
        assert!(
            reg.implements_marker(&ty, marker),
            "an actor handle should implement {marker:?}"
        );
    }
}

// ── RemotePid: marker traits ─────────────────────────────────────────────────

#[test]
fn remote_pid_is_send_sync_copy() {
    use hew_types::traits::{MarkerTrait, TraitRegistry};
    let reg = TraitRegistry::new();
    let ty = Ty::remote_pid(Ty::Named {
        builtin: None,
        name: "Counter".into(),
        args: vec![],
    });
    for marker in [
        MarkerTrait::Send,
        MarkerTrait::Sync,
        MarkerTrait::Copy,
        MarkerTrait::Clone,
        MarkerTrait::Frozen,
        MarkerTrait::Debug,
    ] {
        assert!(
            reg.implements_marker(&ty, marker),
            "RemotePid should implement {marker:?}"
        );
    }
}

// ── Unification: an actor handle and RemotePid are distinct nominal types ────

#[test]
fn actor_handle_and_remote_pid_do_not_unify() {
    // The local and remote handle families are distinct nominal types over the
    // same actor: `Worker` (the actor handle) must never be accepted where a
    // `RemotePid<Worker>` is expected, and vice versa. Their discriminators
    // carry different ABI shapes (`*mut HewActor` vs a packed `i64`), so a
    // silent unify would be a miscompile.
    use hew_types::ty::Substitution;
    use hew_types::unify::unify;
    let actor_nominal = Ty::Named {
        builtin: None,
        name: "Worker".into(),
        args: vec![],
    };
    let actor_handle = Ty::actor_handle("Worker", vec![]);
    let remote_pid = Ty::remote_pid(actor_nominal);

    let mut subst = Substitution::new();
    assert!(
        unify(&mut subst, &actor_handle, &remote_pid).is_err(),
        "the actor handle must not unify with RemotePid<Worker>"
    );
    let mut subst = Substitution::new();
    assert!(
        unify(&mut subst, &remote_pid, &actor_handle).is_err(),
        "RemotePid<Worker> must not unify with the actor handle"
    );
}

// ── builtin_names registration ────────────────────────────────────────────────

// Negative controls: `LocalPid`, `Pid` and `LambdaPid` are retired surface
// spellings (D489) and must not resolve through the builtin-name registry —
// an actor's handle is named by the actor itself, so there is no longer one
// canonical name to register.
#[test]
fn local_pid_no_longer_registered_in_builtin_names() {
    use hew_types::builtin_names::builtin_named_type;
    assert!(
        builtin_named_type("LocalPid").is_none(),
        "LocalPid must not resolve in the builtin_named_type registry"
    );
}

#[test]
fn lambda_pid_no_longer_registered_in_builtin_names() {
    use hew_types::builtin_names::builtin_named_type;
    assert!(
        builtin_named_type("LambdaPid").is_none(),
        "LambdaPid must not resolve in the builtin_named_type registry"
    );
}

#[test]
fn remote_pid_registered_in_builtin_names() {
    use hew_types::builtin_names::builtin_named_type;
    assert!(
        builtin_named_type("RemotePid").is_some(),
        "RemotePid should be in builtin_named_type registry"
    );
}

// ── Ty helpers ────────────────────────────────────────────────────────────────

#[test]
fn ty_actor_handle_helper() {
    let ty = Ty::actor_handle("Msg", vec![]);
    assert_eq!(ty.actor_handle_identity(), Some(("Msg", &[][..])));
    assert!(ty.as_actor_handle().is_some());
    assert_eq!(ty.as_remote_pid(), None);
}

#[test]
fn ty_remote_pid_helper() {
    let inner = Ty::Named {
        builtin: None,
        name: "Msg".into(),
        args: vec![],
    };
    let ty = Ty::remote_pid(inner.clone());
    assert_eq!(ty.as_remote_pid(), Some(&inner));
    assert_eq!(ty.actor_handle_identity(), None);
}
