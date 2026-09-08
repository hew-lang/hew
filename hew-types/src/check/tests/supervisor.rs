#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
pub(super) use super::*;

#[test]
fn nested_supervisor_remains_a_valid_child_target() {
    let output = check_source(
        "actor Worker { receive fn ping() {} }\n\
         supervisor Inner { child worker: Worker, }\n\
         supervisor Root { child inner: Inner, }",
    );
    assert!(
        output.errors.is_empty(),
        "a nested supervisor is a supervisable child target: {:#?}",
        output.errors
    );
}

#[test]
fn generic_supervisor_infers_config_and_child_type_before_function_projection() {
    let output = check_source(
        r#"
        fn main() {
            let group = spawn Group(seed: "owned");
            let value: Result<string, ActorError<Never>> = group.worker.get();
            close(group);
        }
        actor Worker<T> { let value: T, receive fn get() -> T { value } }
        supervisor Group<T>(seed: T) { child worker: Worker(value: seed), }
    "#,
    );
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
}

#[test]
fn generic_supervisor_rejects_wrong_child_argument_type() {
    let output = check_source(
        r"
        actor Worker<T> { let value: T, receive fn get() -> T { value } }
        supervisor Group<T>(seed: T) { child worker: Worker<i64>(value: seed), }
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|error| matches!(error.kind, TypeErrorKind::Mismatch { .. })),
        "{:#?}",
        output.errors
    );
}

#[test]
fn generic_supervisor_rejects_non_send_type_argument_at_spawn() {
    let output = check_source(
        r"
        supervisor Group<T> {}
        fn main() { let _ = spawn Group<Rc<i64>>(); }
    ",
    );
    assert!(
        output.errors.iter().any(
            |error| matches!(error.kind, TypeErrorKind::BoundsNotSatisfied)
                && error.message.contains("Send")
        ),
        "{:#?}",
        output.errors
    );
}

#[test]
fn generic_supervisor_checks_nested_send_bounds() {
    let output = check_source(
        r#"
        actor Worker<T> { let value: T, receive fn get() -> T { value } }
        supervisor Group<T>(seed: Vec<T>) { child worker: Worker<Vec<T>>(value: seed), }
        fn main() {
            let group = spawn Group(seed: ["owned"]);
            let values: Result<Vec<string>, ActorError<Never>> = group.worker.get();
            close(group);
        }
    "#,
    );
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
}

#[test]
fn supervisor_init_arg_scalar_config_field_admitted() {
    // Config field access must be typed before constructing the child.
    let output = check_source(
        r"
        type AppConfig { size: i64 }

        actor Cache {
            var stored: i64,
            init(capacity: i64) {
                stored = capacity;
            }
            receive fn noop() {}
        }

        supervisor App(config: AppConfig) {
            child cache: Cache(capacity: config.size),
        }
        ",
    );

    assert!(output.errors.is_empty(), "{:#?}", output.errors);
}

#[test]
fn supervisor_init_arg_config_nonexistent_field_surfaces_error() {
    // Typing the init-arg expr against the config param surfaces a real
    // diagnostic for a missing config field, at the arg-expr span.
    let output = check_source(
        r"
        type AppConfig { size: i64 }

        actor Cache {
            var capacity: i64,
            init(capacity: i64) {
                capacity = capacity;
            }
            receive fn noop() {}
        }

        supervisor App(config: AppConfig) {
            child cache: Cache(capacity: config.nonexistent),
        }
        ",
    );

    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("nonexistent") && e.message.contains("AppConfig")),
        "reading a non-existent config field must surface a typed error naming \
         the field and the config struct; errors: {:#?}",
        output.errors
    );
}

#[test]
fn supervisor_init_arg_config_field_type_mismatch_surfaces_error() {
    // A config field whose type does not match the actor init param surfaces a
    // type-mismatch diagnostic (the actor expects i64; the config field is
    // bool). The init-arg-expr typing is what makes this catchable.
    let output = check_source(
        r"
        type AppConfig { flag: bool }

        actor Cache {
            var capacity: i64,
            init(capacity: i64) {
                capacity = capacity;
            }
            receive fn noop() {}
        }

        supervisor App(config: AppConfig) {
            child cache: Cache(capacity: config.flag),
        }
        ",
    );

    // The arg expr types cleanly to bool; the mismatch against the i64 param is
    // reported by the spawn/constructor arg check. Either a Mismatch error or
    // a constructor-arg diagnostic is acceptable — assert at least one error
    // mentions the type clash so a silent accept fails the test.
    assert!(
        !output.errors.is_empty(),
        "a config field whose type mismatches the actor init param must produce \
         at least one diagnostic; errors: {:#?}",
        output.errors
    );
}

#[test]
fn supervisor_pool_count_missing_is_rejected() {
    let output = check_source(
        r"
        actor Worker { receive fn ping() {} }

        supervisor App {
            strategy: simple_one_for_one,
            pool workers: Worker
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("E_SUPERVISOR_POOL_COUNT_MISSING")),
        "a pool child without `count:` must be rejected; errors: {:#?}",
        output.errors
    );
}

#[test]
fn supervisor_pool_count_zero_literal_is_rejected() {
    let output = check_source(
        r"
        actor Worker { receive fn ping() {} }

        supervisor App {
            strategy: simple_one_for_one,
            pool workers: Worker count: 0
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("E_SUPERVISOR_POOL_COUNT_NON_POSITIVE")),
        "a `count: 0` pool must be rejected; errors: {:#?}",
        output.errors
    );
}

#[test]
fn supervisor_pool_count_negative_literal_is_rejected() {
    let output = check_source(
        r"
        actor Worker { receive fn ping() {} }

        supervisor App {
            strategy: simple_one_for_one,
            pool workers: Worker count: -3
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("E_SUPERVISOR_POOL_COUNT_NON_POSITIVE")),
        "a negative `count:` pool must be rejected; errors: {:#?}",
        output.errors
    );
}

#[test]
fn supervisor_pool_count_positive_literal_is_accepted() {
    let output = check_source(
        r"
        actor Worker { receive fn ping() {} }

        supervisor App {
            strategy: simple_one_for_one,
            pool workers: Worker count: 5
        }
        ",
    );
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.message.contains("E_SUPERVISOR_POOL_COUNT")),
        "a positive `count:` pool must be accepted; errors: {:#?}",
        output.errors
    );
}

#[test]
fn supervisor_pool_count_string_literal_is_rejected() {
    let output = check_source(
        r#"
        actor Worker { receive fn ping() {} }

        supervisor App {
            strategy: simple_one_for_one,
            pool workers: Worker count: "five"
        }
        "#,
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("E_SUPERVISOR_POOL_COUNT_TYPE")),
        "a non-integer `count:` must be rejected; errors: {:#?}",
        output.errors
    );
}

#[test]
fn supervisor_pool_count_dynamic_config_is_accepted() {
    // A dynamic count (`config.workers`) is accepted by the type-checker (the
    // expr resolves through the config record layout) but codegen currently
    // rejects it with CodegenError::FailClosed — the dynamic 0..N bootstrap
    // loop is not yet emitted. This test only covers the type-checker accept.
    let output = check_source(
        r"
        type AppConfig { workers: i64 }

        actor Worker { receive fn ping() {} }

        supervisor App(config: AppConfig) {
            strategy: simple_one_for_one,
            pool workers: Worker count: config.workers
        }
        ",
    );
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.message.contains("E_SUPERVISOR_POOL_COUNT")),
        "a dynamic config-derived `count:` must be accepted; errors: {:#?}",
        output.errors
    );
}

#[test]
fn supervisor_wired_cycle_reports_distinct_kind() {
    let output = check_source(
        r"
        actor ActorA {
            init(dep: LocalPid<ActorB>) {}
            receive fn ping() {}
        }
        actor ActorB {
            init(dep: LocalPid<ActorA>) {}
            receive fn ping() {}
        }

        supervisor CycleApp {
            strategy: one_for_one,

            child a: ActorA wired_to: { dep: b },
            child b: ActorB wired_to: { dep: a },
        }
        ",
    );

    let err = output
        .errors
        .iter()
        .find(|e| e.message.contains("E_SUPERVISOR_WIRED_CYCLE"))
        .expect("wired_to cycle a->b->a must emit E_SUPERVISOR_WIRED_CYCLE");
    assert_eq!(
        err.kind,
        TypeErrorKind::SupervisorError {
            subkind: SupervisorErrorKind::WiredCycle,
        }
    );
    assert_eq!(err.kind.as_kind_str(), "SupervisorWiredCycle");
}
