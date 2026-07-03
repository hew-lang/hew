/// Checker tests for generic-actor spawn type-arg handling.
///
/// Covers:
///   • `checker_generic_actor_spawn_substitutes_type_args` — PID type carries resolved args
///   • `checker_missing_turbofish_on_generic_actor_diagnostic` — `MissingActorTypeArgs` fires
///   • `checker_actor_bound_enforcement_reuses_machine_helper` — `BoundsNotSatisfied` fires
///   • `checker_actor_type_arg_arity_mismatch` — wrong count → `ActorTypeArgArityMismatch`
///   • `checker_non_generic_actor_spawn_no_diagnostic` — regression: no error for bare `spawn Foo()`
use crate::common;

use hew_types::error::TypeErrorKind;
use hew_types::Ty;

// ── checker_generic_actor_spawn_substitutes_type_args ────────────────────────

/// Spawning a generic actor with explicit type args produces a PID whose inner
/// `Named.args` vector carries the resolved type argument.
#[test]
fn checker_generic_actor_spawn_substitutes_type_args() {
    let source = r"
actor Buffer<T> {
    receive fn push(item: T) {}
}

fn main() {
    let pid = spawn Buffer<i64>();
}
";
    let (_prog, output) = common::parse_and_typecheck_isolated(source);
    assert!(
        output.errors.is_empty(),
        "expected no errors, got: {:#?}",
        output.errors
    );

    // The expr_types map should contain a LocalPid<Buffer<i64>> entry.
    let pid_ty = output
        .expr_types
        .values()
        .find(|t| matches!(t, Ty::Named { name, .. } if name == "LocalPid"))
        .cloned()
        .unwrap_or_else(|| {
            panic!(
                "no LocalPid entry found in expr_types; keys: {:#?}",
                output.expr_types
            )
        });

    // Destructure LocalPid<Buffer<i64>>
    let Ty::Named {
        name: pid_name,
        args: pid_args,
        ..
    } = &pid_ty
    else {
        panic!("expected Named LocalPid, got {pid_ty:?}");
    };
    assert_eq!(pid_name, "LocalPid");
    assert_eq!(
        pid_args.len(),
        1,
        "LocalPid should have 1 arg (the actor type)"
    );

    let inner = &pid_args[0];
    let Ty::Named {
        name: actor_name,
        args: actor_args,
        ..
    } = inner
    else {
        panic!("expected Named inner type in LocalPid, got {inner:?}");
    };
    assert_eq!(actor_name, "Buffer");
    assert_eq!(actor_args.len(), 1, "Buffer<i64> should have 1 type arg");
    assert!(
        matches!(&actor_args[0], Ty::I64),
        "expected i64 type arg, got {:?}",
        actor_args[0]
    );
}

// ── checker_missing_turbofish_on_generic_actor_diagnostic ───────────────────

/// Spawning a generic actor without type args emits `MissingActorTypeArgs`.
#[test]
fn checker_missing_turbofish_on_generic_actor_diagnostic() {
    let source = r"
actor Buffer<T> {
    receive fn push(item: T) {}
}

fn main() {
    let _pid = spawn Buffer();
}
";
    let (_prog, output) = common::parse_and_typecheck_isolated(source);

    let missing_args_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| {
            matches!(
                &e.kind,
                TypeErrorKind::MissingActorTypeArgs { actor_name, .. }
                if actor_name == "Buffer"
            )
        })
        .collect();

    assert!(
        !missing_args_errors.is_empty(),
        "expected MissingActorTypeArgs diagnostic for `spawn Buffer()` on generic actor; \
         errors present: {:#?}",
        output.errors
    );

    let err = &missing_args_errors[0];
    // Message should include the actor name and a hint about the required form.
    assert!(
        err.message.contains("Buffer"),
        "diagnostic message should mention 'Buffer', got: {:?}",
        err.message
    );
}

// ── checker_actor_bound_enforcement_reuses_machine_helper ───────────────────

/// Generic actor with `<T: Send>` bound: spawning with a non-Send type
/// fires `BoundsNotSatisfied` via `enforce_actor_instantiation_bounds`.
///
/// This test exercises the clone-pattern: `enforce_actor_instantiation_bounds`
/// is a copy of `enforce_machine_instantiation_bounds` scoped to the actor
/// bound table — both helpers route through `enforce_named_type_param_bounds`.
#[test]
fn checker_actor_bound_enforcement_reuses_machine_helper() {
    // `Closure` is not a Send type in the hew type system, but for an
    // isolated checker test we just need any type that is NOT in the
    // `Send` impl set.  Using `fn(i32) -> i32` as a function-pointer
    // type is the safest choice: it resolves cleanly but does not carry
    // a `Send` impl.
    //
    // However, since function-pointer syntax is complex in Hew, we instead
    // declare a local record type that has no `Send` impl and verify that
    // the checker correctly rejects it.
    //
    // Note: For the isolated checker, only built-in types have `Send`.
    // A user-declared record without `impl Send` is non-Send by default.
    let source = r"
type Plain { value: i32 }

actor Holder<T: Send> {
    receive fn put(item: T) {}
}

fn main() {
    let _pid = spawn Holder<Plain>();
}
";
    let (_prog, output) = common::parse_and_typecheck_isolated(source);

    let bounds_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| matches!(&e.kind, TypeErrorKind::BoundsNotSatisfied))
        .collect();

    assert!(
        !bounds_errors.is_empty(),
        "expected BoundsNotSatisfied for Holder<Plain> where T: Send; \
         errors present: {:#?}",
        output.errors
    );
}

// ── checker_actor_type_arg_arity_mismatch ───────────────────────────────────

/// Supplying the wrong number of type arguments to a generic actor spawn
/// emits `ActorTypeArgArityMismatch`.
#[test]
fn checker_actor_type_arg_arity_mismatch() {
    let source = r"
actor Pair<A, B> {
    receive fn handle(a: A, b: B) {}
}

fn main() {
    // Pair needs 2 type args; supply only 1.
    let _pid = spawn Pair<i64>();
}
";
    let (_prog, output) = common::parse_and_typecheck_isolated(source);

    let arity_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| {
            matches!(
                &e.kind,
                TypeErrorKind::ActorTypeArgArityMismatch {
                    actor_name,
                    expected: 2,
                    got: 1,
                }
                if actor_name == "Pair"
            )
        })
        .collect();

    assert!(
        !arity_errors.is_empty(),
        "expected ActorTypeArgArityMismatch(expected=2, got=1); \
         errors present: {:#?}",
        output.errors
    );
}

// ── checker_non_generic_actor_spawn_no_diagnostic ───────────────────────────

/// Non-generic actor spawn produces no errors — regression guard for the
/// pre-existing `spawn Foo()` path. The empty-type-args fast path must
/// remain silent for actors with no declared type params.
#[test]
fn checker_non_generic_actor_spawn_no_diagnostic() {
    let source = r"
actor Counter {
    receive fn tick() {}
}

fn main() {
    let _pid = spawn Counter();
}
";
    let (_prog, output) = common::parse_and_typecheck_isolated(source);

    let spawn_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| {
            matches!(
                &e.kind,
                TypeErrorKind::MissingActorTypeArgs { .. }
                    | TypeErrorKind::ActorTypeArgArityMismatch { .. }
            )
        })
        .collect();

    assert!(
        spawn_errors.is_empty(),
        "non-generic actor should produce no type-arg errors; got: {spawn_errors:#?}"
    );
}

// ── checker_actor_spawn_type_args_recorded_in_output ────────────────────────

/// Explicit type args on a generic spawn are recorded in
/// `output.actor_spawn_type_args` for actor-mono discovery consumption.
#[test]
fn checker_actor_spawn_type_args_recorded_in_output() {
    let source = r"
actor Worker<T> {
    receive fn run(task: T) {}
}

fn main() {
    let _pid = spawn Worker<i32>();
}
";
    let (_prog, output) = common::parse_and_typecheck_isolated(source);
    assert!(
        output.errors.is_empty(),
        "should type-check cleanly: {:#?}",
        output.errors
    );

    // At least one entry in actor_spawn_type_args for Worker<i32>.
    let worker_entry = output
        .actor_spawn_type_args
        .values()
        .find(|(name, args)| name == "Worker" && !args.is_empty());

    assert!(
        worker_entry.is_some(),
        "expected actor_spawn_type_args entry for Worker<i32>; table: {:#?}",
        output.actor_spawn_type_args
    );

    let (_, args) = worker_entry.unwrap();
    assert_eq!(args.len(), 1);
    assert!(
        matches!(args[0], Ty::I32),
        "expected i32, got {:?}",
        args[0]
    );
}

// ── checker_spawn_hashmap_new_infers_from_field_type ─────────────────────────

/// `spawn Cache(store: HashMap::new())` must check clean: the field's declared
/// type (`HashMap<string, i64>`) is pushed down so `HashMap::new()` infers
/// its key/value type variables rather than leaving them unbound and triggering
/// a spurious "not Send" error.
///
/// Regression for the bug where `check_spawn` called `synthesize` without an
/// expected type, leaving `HashMap<?T, ?U>` unbound and producing:
///   "cannot send `HashMap<?T22, ?T23>` to actor: type is not Send"
///   "cannot infer type for expression type at checker output boundary"
#[test]
fn checker_spawn_hashmap_new_infers_from_field_type() {
    let source = r"
actor Cache {
    let store: HashMap<string, i64>;
    receive fn size() -> i64 {
        store.len()
    }
}

fn main() {
    let c = spawn Cache(store: HashMap::new());
}
";
    let (_prog, output) = common::parse_and_typecheck_inline(source);
    assert!(
        output.errors.is_empty(),
        "spawn Cache(store: HashMap::new()) should check clean; errors: {:#?}",
        output.errors
    );
}

// ── checker_spawn_vec_new_infers_from_field_type ─────────────────────────────

/// `spawn Log(entries: Vec::new())` must check clean: the field's declared
/// type (`Vec<string>`) is pushed down so `Vec::new()` infers its element
/// type.  Same root cause as the `HashMap` case.
#[test]
fn checker_spawn_vec_new_infers_from_field_type() {
    let source = r"
actor Log {
    let entries: Vec<string>;
    receive fn count() -> i64 {
        entries.len()
    }
}

fn main() {
    let _log = spawn Log(entries: Vec::new());
}
";
    let (_prog, output) = common::parse_and_typecheck_inline(source);
    assert!(
        output.errors.is_empty(),
        "spawn Log(entries: Vec::new()) should check clean; errors: {:#?}",
        output.errors
    );
}
