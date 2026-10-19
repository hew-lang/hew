use crate::common;
use hew_parser::ast::Ident;

use hew_types::Ty;

// ── An actor is the type of its handle (D489): spawn returns the actor's own
// type ──────────────────────────────────────────────────────────────────────

#[test]
fn spawn_returns_the_actor_type() {
    let source = r"actor Counter {
    let n: i32;
    init() {}
}

fn main() {
    let c = spawn Counter { n: 0 };
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
            hew_parser::ast::Item::Function(fd) if fd.name == Ident::new("main") => Some(fd),
            _ => None,
        })
        .expect("no main");
    // Find the span of the `spawn Counter { n: 0 }` expression (not the let stmt).
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
        ty.actor_handle_identity()
            .map(|(head, args)| (head.spelling.as_str(), args)),
        Some(("Counter", &[][..])),
        "expected the Counter actor handle, got {ty:?}"
    );
}

// ── An actor handle: marker traits ────────────────────────────────────────────

#[test]
fn actor_handle_is_send_sync_copy() {
    use hew_types::traits::{MarkerTrait, TraitRegistry};
    let reg = TraitRegistry::new();
    let ty = Ty::actor_for_test("Counter", vec![]);
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
fn remote_pid_no_longer_registered_in_builtin_names() {
    use hew_types::builtin_names::builtin_named_type;
    assert!(
        builtin_named_type("RemotePid").is_none(),
        "RemotePid must not be in builtin_named_type registry"
    );
}

// ── Ty helpers ────────────────────────────────────────────────────────────────

#[test]
fn ty_actor_handle_helper() {
    let ty = Ty::actor_for_test("Msg", vec![]);
    assert_eq!(
        ty.actor_handle_identity()
            .map(|(head, args)| (head.spelling.as_str(), args)),
        Some(("Msg", &[][..]))
    );
    assert!(ty.as_actor_handle().is_some());
}
