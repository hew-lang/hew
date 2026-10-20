//! Nominal wall-clock boundaries remain explicit through aliases and storage.

use crate::common::typecheck_isolated as typecheck;

#[test]
fn timestamp_rejects_implicit_integer_and_monotonic_domains() {
    for source in [
        "fn f(value: timestamp) -> i64 { value }",
        "fn f(value: i64) -> timestamp { value }",
        "fn f(value: instant) -> timestamp { value }",
        "fn f(value: timestamp) -> instant { value }",
        "fn f(value: timestamp) { let _ = value + 1; }",
        "fn f(a: timestamp, b: timestamp) { let _ = a + b; }",
        "fn f(a: timestamp, b: instant) { let _ = a - b; }",
        "fn f(value: timestamp) { let _ = value as i64; }",
    ] {
        let checked = typecheck(source);
        assert!(
            checked
                .errors
                .iter()
                .any(|error| error.source_module.is_none()),
            "must reject the source {source}: {:?}",
            checked.errors
        );
    }
}

#[test]
fn timestamp_pair_cannot_be_read_or_forged() {
    for source in [
        "fn f(value: timestamp) { let _ = value.seconds; }",
        "fn f(value: timestamp) { let _ = value.nanoseconds; }",
        "fn f() { let _ = timestamp { seconds: 0, nanoseconds: 1000000000 }; }",
        "type Wall = timestamp; fn f(value: Wall) { let _ = value.seconds; }",
        "type Wall = timestamp; fn f() { let _ = Wall { seconds: 0, nanoseconds: 1000000000 }; }",
    ] {
        let checked = typecheck(source);
        assert!(
            checked
                .errors
                .iter()
                .any(|error| error.source_module.is_none()),
            "must reject the source {source}: {:?}",
            checked.errors
        );
    }
}

#[test]
fn timestamp_admits_only_duration_updates() {
    let checked = typecheck(
        "fn f(value: timestamp) -> timestamp { var next = value; next += 1ns; next -= 2ns; next }",
    );
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
    for source in [
        "fn f(value: timestamp) { var next = value; next += 1; }",
        "fn f(value: timestamp) { var next = value; next *= 2; }",
    ] {
        let checked = typecheck(source);
        assert!(
            checked
                .errors
                .iter()
                .any(|error| error.source_module.is_none()),
            "must reject the source {source}: {:?}",
            checked.errors
        );
    }
}
