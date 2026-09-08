//! Generic actor constructor, bound and handler substitution contracts.

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

#[test]
fn checker_actor_rejects_non_send_type_argument_at_spawn() {
    let source = "actor Holder<T> { receive fn put(item: T) {} } fn main() { let _pid = spawn Holder<Rc<i64>>(); }";
    let (_, output) = common::parse_and_typecheck_isolated(source);
    assert!(
        output.errors.iter().any(
            |error| matches!(error.kind, TypeErrorKind::BoundsNotSatisfied)
                && error.message.contains("Send")
                && error.message.contains("Rc")
        ),
        "{:#?}",
        output.errors
    );
}

#[test]
fn checker_actor_accepts_structurally_send_record_argument() {
    let source = "type Packet { value: i64 } actor Holder<T> { receive fn put(item: T) {} } fn main() { let _pid = spawn Holder<Packet>(); }";
    let (_, output) = common::parse_and_typecheck_isolated(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
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

#[test]
fn checker_actor_infers_owner_from_init_and_substitutes_handler_reply() {
    let source = "actor Holder<T> { var value: Option<T> = .None, init(seed: T) { value = .Some(seed); } receive fn get() -> Option<T> { value } } fn main() { let holder = spawn Holder(seed: 41); let result: i64 = holder.get().expect(\"reply\").expect(\"set\"); }";
    let (_, output) = common::parse_and_typecheck_inline(source);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
}

#[test]
fn checker_actor_handler_rejects_another_instances_payload() {
    let source = "actor Holder<T> { receive fn put(value: T) {} } fn main() { let holder = spawn Holder<i64>(); holder.put(\"wrong instance\"); }";
    let (_, output) = common::parse_and_typecheck_inline(source);
    assert!(output.errors.iter().any(|error| matches!(&error.kind, TypeErrorKind::Mismatch { expected, actual } if expected == "i64" && actual == "string")), "{:#?}", output.errors);
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
    let store: HashMap<string, i64>,
    receive fn size() -> i64 {
        store.len()
    }
}

fn main() {
    let c = spawn Cache(store: HashMap.new());
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
    let entries: Vec<string>,
    receive fn count() -> i64 {
        entries.len()
    }
}

fn main() {
    let _log = spawn Log(entries: Vec.new());
}
";
    let (_prog, output) = common::parse_and_typecheck_inline(source);
    assert!(
        output.errors.is_empty(),
        "spawn Log(entries: Vec::new()) should check clean; errors: {:#?}",
        output.errors
    );
}

// ── checker_spawn_substitutes_type_args_into_generic_field_arg ───────────────

/// `spawn Box<i64>(value: 5)` must check the `value` constructor arg against
/// the SUBSTITUTED field type (`i64`), not the unsubstituted declared generic
/// `T`.  Regression for #2447: `check_spawn_constructor_args` compared the arg
/// against the raw declared `T`, yielding a spurious
///   "type mismatch: expected `T`, found `i64`".
#[test]
fn checker_spawn_substitutes_type_args_into_generic_field_arg() {
    let source = r"
actor Box<T> {
    let value: T,
    receive fn touch() {}
}

fn main() {
    let _b = spawn Box<i64>(value: 5);
}
";
    let (_prog, output) = common::parse_and_typecheck_inline(source);
    assert!(
        output.errors.is_empty(),
        "spawn Box<i64>(value: 5) should check clean after type-arg substitution; errors: {:#?}",
        output.errors
    );
}

// ── checker_spawn_generic_field_arg_type_mismatch_still_reported ─────────────

/// The #2447 substitution must not mask a genuine mismatch: `spawn Box<i64>(
/// value: "x")` supplies a `string` where the instantiated field type is
/// `i64`, so a type-mismatch error must still fire (against the substituted
/// `i64`, not the generic `T`).
#[test]
fn checker_spawn_generic_field_arg_type_mismatch_still_reported() {
    let source = r#"
actor Box<T> {
    let value: T,
    receive fn touch() {}
}

fn main() {
    let _b = spawn Box<i64>(value: "x");
}
"#;
    let (_prog, output) = common::parse_and_typecheck_inline(source);
    assert!(
        !output.errors.is_empty(),
        "spawn Box<i64>(value: \"x\") should still report a type mismatch against the substituted i64"
    );
}
