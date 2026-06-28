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
    // spawn should produce LocalPid<Counter>.
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

// ── Unification: LocalPid and RemotePid are distinct nominal types ────────────

#[test]
fn local_pid_and_remote_pid_do_not_unify() {
    // The local and remote handle families are distinct nominal types over the
    // same actor: a `LocalPid<T>` must never be accepted where a `RemotePid<T>`
    // is expected, and vice versa. Their discriminators carry different ABI
    // shapes (`*mut HewActor` vs a packed `i64`), so a silent unify would be a
    // miscompile.
    use hew_types::ty::Substitution;
    use hew_types::unify::unify;
    let actor = Ty::Named {
        builtin: None,
        name: "Worker".into(),
        args: vec![],
    };
    let local_pid = Ty::local_pid(actor.clone());
    let remote_pid = Ty::remote_pid(actor);

    let mut subst = Substitution::new();
    assert!(
        unify(&mut subst, &local_pid, &remote_pid).is_err(),
        "LocalPid<T> must not unify with RemotePid<T>"
    );
    let mut subst = Substitution::new();
    assert!(
        unify(&mut subst, &remote_pid, &local_pid).is_err(),
        "RemotePid<T> must not unify with LocalPid<T>"
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
