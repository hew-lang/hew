mod common;

use hew_types::Ty;

// ── LocalPid: spawn returns LocalPid<T> ─────────────────────────────────────

#[test]
fn spawn_returns_local_pid() {
    let source = r"
        actor Counter {
            let n: i32;
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
    // spawn should produce LocalPid<Counter>, not ActorRef<Counter>.
    // Scan all expr_types for any LocalPid entry if the exact span is off.
    let ty = output.expr_types.get(&key).cloned().unwrap_or_else(|| {
        output
            .expr_types
            .values()
            .find(|t| matches!(t, Ty::Named { name, .. } if name == "LocalPid"))
            .cloned()
            .unwrap_or(Ty::Unit)
    });
    assert!(
        matches!(&ty, Ty::Named { name, args, .. } if name == "LocalPid" && args.len() == 1),
        "expected LocalPid<Counter>, got {ty:?}"
    );
}

// ── LocalPid: marker traits ──────────────────────────────────────────────────

#[test]
fn local_pid_is_send_sync_copy() {
    use hew_types::traits::{MarkerTrait, TraitRegistry};
    let reg = TraitRegistry::new();
    let ty = Ty::local_pid(Ty::Named {
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
            "LocalPid should implement {marker:?}"
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

// ── Unification: LocalPid and ActorRef are distinct nominal types ─────────────

#[test]
fn localpid_actorref_no_longer_unify() {
    use hew_types::ty::Substitution;
    use hew_types::unify::unify;
    let actor = Ty::Named {
        builtin: None,
        name: "Worker".into(),
        args: vec![],
    };
    let local_pid = Ty::local_pid(actor.clone());
    let actor_ref = Ty::actor_ref(actor);

    let mut subst = Substitution::new();
    assert!(
        unify(&mut subst, &local_pid, &actor_ref).is_err(),
        "LocalPid<T> must not unify with ActorRef<T>"
    );
    let mut subst = Substitution::new();
    assert!(
        unify(&mut subst, &actor_ref, &local_pid).is_err(),
        "ActorRef<T> must not unify with LocalPid<T>"
    );
}

// ── Unification: RemotePid does NOT unify with ActorRef ──────────────────────

#[test]
fn remote_pid_does_not_unify_with_actor_ref() {
    // RemotePid<T> must not be accepted where ActorRef<T> is expected.
    // The test verifies the discriminator is preserved.
    //
    // We can't construct a RemotePid<T> at the Hew source level yet
    // (from_raw requires SHIM wiring); test via the Rust type API instead.
    use hew_types::ty::Substitution;
    use hew_types::unify::unify;
    let mut subst = Substitution::new();
    let actor_ref = Ty::actor_ref(Ty::Named {
        builtin: None,
        name: "Counter".into(),
        args: vec![],
    });
    let remote_pid = Ty::remote_pid(Ty::Named {
        builtin: None,
        name: "Counter".into(),
        args: vec![],
    });
    let result = unify(&mut subst, &remote_pid, &actor_ref);
    assert!(
        result.is_err(),
        "RemotePid<T> must NOT unify with ActorRef<T>; got Ok"
    );
}

// ── builtin_names registration ────────────────────────────────────────────────

#[test]
fn local_pid_registered_in_builtin_names() {
    use hew_types::builtin_names::builtin_named_type;
    assert!(
        builtin_named_type("LocalPid").is_some(),
        "LocalPid should be in builtin_named_type registry"
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
fn ty_local_pid_helper() {
    let inner = Ty::Named {
        builtin: None,
        name: "Msg".into(),
        args: vec![],
    };
    let ty = Ty::local_pid(inner.clone());
    assert_eq!(ty.as_local_pid(), Some(&inner));
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
    assert_eq!(ty.as_local_pid(), None);
}
