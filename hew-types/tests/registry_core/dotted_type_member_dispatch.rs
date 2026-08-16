use crate::common::typecheck_isolated;
use hew_types::error::TypeErrorKind;

#[test]
fn dotted_type_members_share_canonical_dispatch() {
    let output = typecheck_isolated(
        r#"
machine Lifecycle {
    events { Reset; }
    state Start;
    state Running { value: i64; }
    on Reset: Running => Start { Start }
    default { state }
}

fn main() {
    let start: Lifecycle = Lifecycle.Start;
    let running: Lifecycle = Lifecycle.Running { value: 42 };
    let some: Option<i64> = Option.Some(5);
    let ok: Result<i64, string> = Result.Ok(6);
    let set: HashSet<i64> = HashSet<i64>.new();
    let explicit: Option<i64> = Option<i64>.Some(7);
    set.insert(8);
    println(f"{start.state_name()} {running.state_name()} {some.unwrap()} {ok.unwrap()} {explicit.unwrap()} {set.len()}");
}
"#,
    );

    assert!(
        output.errors.is_empty(),
        "all dotted type-member shapes should typecheck: {:#?}",
        output.errors
    );
}

#[test]
fn bare_type_without_member_remains_an_error() {
    let output = typecheck_isolated(
        r"
machine Lifecycle {
    events { Reset; }
    state Start;
    state Running;
    on Reset: Running => Start { Start }
    default { state }
}

fn main() {
    let bad = Lifecycle;
}
",
    );

    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::TypeUsedAsValue),
        "a type head with no selected member must remain a value error: {:#?}",
        output.errors
    );
}

#[test]
fn dotted_builtin_head_arity_is_checked() {
    let output = typecheck_isolated(
        r"
fn main() {
    let bad = Option<i64, string>.Some(1);
}
",
    );

    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::ArityMismatch),
        "an invalid explicit builtin head must fail at the type boundary: {:#?}",
        output.errors
    );
}

#[test]
fn lexical_value_head_does_not_fall_back_to_builtin_leaf() {
    let output = typecheck_isolated(
        r"
fn main() {
    let Option = 1;
    let bad = Option.Some(2);
}
",
    );

    assert!(
        output
            .errors
            .iter()
            .any(|error| error.message.contains("no method `Some` on `i64`")),
        "a lexical value must win over the same-spelled builtin head: {:#?}",
        output.errors
    );
}
