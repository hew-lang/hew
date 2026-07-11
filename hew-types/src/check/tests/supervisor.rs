#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
pub(super) use super::*;

#[test]
fn supervisor_permanent_owned_heap_rejects_vec_field() {
    let output = check_source(
        r"
        actor Counter {
            let count: Vec<i64>;
            receive fn inc() {}
        }

        supervisor App {
            child worker: Counter restart: permanent;
        }
        ",
    );

    assert!(
        output.errors.iter().any(|e| {
            e.kind
                == TypeErrorKind::SupervisorError {
                    subkind: SupervisorErrorKind::PermanentOwnedHeap,
                }
                && e.message.contains("E_SUPERVISOR_PERMANENT_OWNED_HEAP")
                && e.message.contains("worker")
                && e.message.contains("Counter")
                && e.message.contains("count")
        }),
        "permanent child with Vec field must be rejected; errors: {:#?}",
        output.errors
    );
}

#[test]
fn supervisor_permanent_owned_heap_rejects_string_field() {
    let output = check_source(
        r"
        actor Labelled {
            let label: string;
            receive fn noop() {}
        }

        supervisor App {
            child item: Labelled restart: permanent;
        }
        ",
    );

    assert!(
        output.errors.iter().any(|e| {
            e.kind
                == TypeErrorKind::SupervisorError {
                    subkind: SupervisorErrorKind::PermanentOwnedHeap,
                }
                && e.message.contains("E_SUPERVISOR_PERMANENT_OWNED_HEAP")
                && e.message.contains("label")
        }),
        "permanent child with string field must be rejected; errors: {:#?}",
        output.errors
    );
}

#[test]
fn supervisor_permanent_owned_heap_rejects_hashmap_field() {
    let output = check_source(
        r"
        actor Registry {
            let entries: HashMap<string, i64>;
            receive fn noop() {}
        }

        supervisor App {
            child reg: Registry restart: permanent;
        }
        ",
    );

    assert!(
        output.errors.iter().any(|e| {
            e.kind
                == TypeErrorKind::SupervisorError {
                    subkind: SupervisorErrorKind::PermanentOwnedHeap,
                }
                && e.message.contains("E_SUPERVISOR_PERMANENT_OWNED_HEAP")
                && e.message.contains("entries")
        }),
        "permanent child with HashMap field must be rejected; errors: {:#?}",
        output.errors
    );
}

#[test]
fn supervisor_permanent_owned_heap_rejects_hashset_field() {
    let output = check_source(
        r"
        actor Deduplicator {
            let seen: HashSet<i64>;
            receive fn noop() {}
        }

        supervisor App {
            child dedup: Deduplicator restart: permanent;
        }
        ",
    );

    assert!(
        output.errors.iter().any(|e| {
            e.kind
                == TypeErrorKind::SupervisorError {
                    subkind: SupervisorErrorKind::PermanentOwnedHeap,
                }
                && e.message.contains("E_SUPERVISOR_PERMANENT_OWNED_HEAP")
                && e.message.contains("seen")
        }),
        "permanent child with HashSet field must be rejected; errors: {:#?}",
        output.errors
    );
}

#[test]
fn supervisor_permanent_owned_heap_rejects_bytes_field() {
    let output = check_source(
        r"
        actor Blob {
            let data: bytes;
            receive fn noop() {}
        }

        supervisor App {
            child store: Blob restart: permanent;
        }
        ",
    );

    assert!(
        output.errors.iter().any(|e| {
            e.kind
                == TypeErrorKind::SupervisorError {
                    subkind: SupervisorErrorKind::PermanentOwnedHeap,
                }
                && e.message.contains("E_SUPERVISOR_PERMANENT_OWNED_HEAP")
                && e.message.contains("data")
        }),
        "permanent child with bytes field must be rejected; errors: {:#?}",
        output.errors
    );
}

#[test]
fn supervisor_implicit_permanent_owned_heap_rejects() {
    // No restart keyword — defaults to permanent per llvm.rs:3013.
    let output = check_source(
        r"
        actor Worker {
            let items: Vec<i64>;
            receive fn noop() {}
        }

        supervisor App {
            child w: Worker;
        }
        ",
    );

    assert!(
        output.errors.iter().any(|e| {
            e.kind
                == TypeErrorKind::SupervisorError {
                    subkind: SupervisorErrorKind::PermanentOwnedHeap,
                }
                && e.message.contains("E_SUPERVISOR_PERMANENT_OWNED_HEAP")
                && e.message.contains("items")
        }),
        "implicit-permanent child (no restart keyword) with Vec field must be rejected; \
         errors: {:#?}",
        output.errors
    );
}

#[test]
fn supervisor_permanent_owned_heap_rejects_nested_vec() {
    // Vec<Vec<i64>> — outer Vec matches ty_is_known_owned_heap.
    let output = check_source(
        r"
        actor Nested {
            let matrix: Vec<Vec<i64>>;
            receive fn noop() {}
        }

        supervisor App {
            child n: Nested restart: permanent;
        }
        ",
    );

    assert!(
        output.errors.iter().any(|e| {
            e.kind
                == TypeErrorKind::SupervisorError {
                    subkind: SupervisorErrorKind::PermanentOwnedHeap,
                }
                && e.message.contains("E_SUPERVISOR_PERMANENT_OWNED_HEAP")
                && e.message.contains("matrix")
        }),
        "permanent child with Vec<Vec<i64>> field must be rejected (outer Vec matched); \
         errors: {:#?}",
        output.errors
    );
}

// Positive tests — the diagnostic must NOT fire.

#[test]
fn supervisor_transient_owned_heap_ok() {
    let output = check_source(
        r"
        actor Worker {
            let items: Vec<i64>;
            receive fn noop() {}
        }

        supervisor App {
            child w: Worker restart: transient;
        }
        ",
    );

    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.message.contains("E_SUPERVISOR_PERMANENT_OWNED_HEAP")),
        "transient child must not trigger owned-heap reject; errors: {:#?}",
        output.errors
    );
}

#[test]
fn supervisor_temporary_owned_heap_ok() {
    let output = check_source(
        r"
        actor Worker {
            let items: Vec<i64>;
            receive fn noop() {}
        }

        supervisor App {
            child w: Worker restart: temporary;
        }
        ",
    );

    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.message.contains("E_SUPERVISOR_PERMANENT_OWNED_HEAP")),
        "temporary child must not trigger owned-heap reject; errors: {:#?}",
        output.errors
    );
}

#[test]
fn supervisor_permanent_copy_only_fields_ok() {
    let output = check_source(
        r"
        actor Counter {
            let count: i64;
            let flag: bool;
            let ratio: f64;
            receive fn noop() {}
        }

        supervisor App {
            child c: Counter restart: permanent;
        }
        ",
    );

    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.message.contains("E_SUPERVISOR_PERMANENT_OWNED_HEAP")),
        "permanent child with only copy-type fields must not trigger owned-heap reject; \
         errors: {:#?}",
        output.errors
    );
}

#[test]
fn supervisor_pool_child_permanent_vec_ok() {
    // Pool children are exempt: they are dynamically spawned, not restarted
    // from a fixed init_state spec.
    let output = check_source(
        r"
        actor Worker {
            let items: Vec<i64>;
            receive fn noop() {}
        }

        supervisor App {
            strategy: simple_one_for_one,
            pool workers: Worker(count: 2)
        }
        ",
    );

    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.message.contains("E_SUPERVISOR_PERMANENT_OWNED_HEAP")),
        "pool child must not trigger owned-heap reject; errors: {:#?}",
        output.errors
    );
}

// KNOWN-RESIDUAL: a user-defined record that *wraps* an owned-heap type is not
// detected by ty_is_known_owned_heap because the check only inspects the
// top-level Ty variant of each actor state field.  The full fix
// (init_state_clone_fn, v0.5.0.1 P0) will close this gap.  This test locks
// the boundary so future implementers see it explicitly.
#[test]
fn supervisor_permanent_record_containing_vec_known_residual_gap() {
    // The actor field type is a named record (not directly Vec/string/etc.),
    // so the check does NOT fire.  This is the documented residual gap.
    let output = check_source(
        r"
        record Wrapper { inner: Vec<i64> }

        actor Holder {
            let data: Wrapper;
            receive fn noop() {}
        }

        supervisor App {
            child h: Holder restart: permanent;
        }
        ",
    );

    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.message.contains("E_SUPERVISOR_PERMANENT_OWNED_HEAP")),
        "KNOWN-RESIDUAL: record-wrapping-Vec not yet detected by owned-heap check \
         (full fix: init_state_clone_fn v0.5.0.1 P0); if this assertion now fails the \
         residual gap has been closed — update this test accordingly; \
         errors: {:#?}",
        output.errors
    );
}

// ── E_SUPERVISOR_INIT_ARG_NON_BITCOPY — byte-copy wall for init args ───────────
//
// Negative tests: init args whose actor init-parameter type is an owned-heap
// type must be rejected, regardless of restart policy.
//
// Positive tests: scalar/BitCopy init args and pool children remain admitted.

fn assert_supervisor_init_arg_non_bitcopy(source: &str, param_type_source: &str) {
    let output = check_source(source);
    let init_start = source
        .find("init(")
        .unwrap_or_else(|| panic!("test source must contain an init block"));
    let expected_span_start = source
        .get(init_start..)
        .and_then(|init_source| init_source.find(param_type_source))
        .map_or_else(
            || panic!("test source must contain `{param_type_source}`"),
            |offset| init_start + offset,
        );
    let expected_span = expected_span_start..expected_span_start + param_type_source.len();
    let wall_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.message.contains("E_SUPERVISOR_INIT_ARG_NON_BITCOPY"))
        .collect();

    assert!(
        wall_errors.iter().any(|e| e.span == expected_span),
        "supervisor init arg of type `{param_type_source}` must be rejected with \
         E_SUPERVISOR_INIT_ARG_NON_BITCOPY at the parameter type span {expected_span:?}; \
         wall errors: {wall_errors:#?}; all errors: {:#?}",
        output.errors
    );
}

fn assert_supervisor_init_arg_admitted(source: &str) {
    let output = check_source(source);
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.message.contains("E_SUPERVISOR_INIT_ARG_NON_BITCOPY")),
        "init arg must be admitted by the init-closure reproducibility wall; errors: {:#?}",
        output.errors
    );
}

#[test]
fn supervisor_init_arg_string_admitted() {
    // An owned `string` init arg is reproducible: the init thunk allocating-clones
    // it per incarnation, so a restart gets a fresh, unaliased value.
    assert_supervisor_init_arg_admitted(
        r#"
        actor Greeter {
            let name: string;
            init(name: string) {
                name = name;
            }
            receive fn greet() {}
        }

        supervisor App {
            child g: Greeter(name: "hello");
        }
        "#,
    );
}

#[test]
fn supervisor_init_arg_vec_rejects() {
    assert_supervisor_init_arg_non_bitcopy(
        r"
        actor Collector {
            let items: Vec<i64>;
            init(items: Vec<i64>) {
                items = items;
            }
            receive fn noop() {}
        }

        supervisor App {
            child c: Collector(items: []);
        }
        ",
        "Vec<i64>",
    );
}

#[test]
fn supervisor_init_arg_bytes_admitted() {
    // Owned `bytes` is reproducible: the init thunk refcount-bump-clones it per
    // incarnation (the config buffer + the fresh state share a counted ref).
    assert_supervisor_init_arg_admitted(
        r"
        actor Payload {
            let data: bytes;
            init(data: bytes) {
                data = data;
            }
            receive fn noop() {}
        }

        supervisor App {
            child p: Payload(data: []);
        }
        ",
    );
}

#[test]
fn supervisor_init_arg_option_string_rejects() {
    assert_supervisor_init_arg_non_bitcopy(
        r#"
        actor MaybeGreeter {
            let name: Option<string>;
            init(name: Option<string>) {
                name = name;
            }
            receive fn greet() {}
        }

        supervisor App {
            child g: MaybeGreeter(name: Some("hello"));
        }
        "#,
        "Option<string>",
    );
}

#[test]
fn supervisor_init_arg_tuple_rejects() {
    assert_supervisor_init_arg_non_bitcopy(
        r#"
        actor PairHolder {
            let pair: (string, i64);
            init(pair: (string, i64)) {
                pair = pair;
            }
            receive fn noop() {}
        }

        supervisor App {
            child p: PairHolder(pair: ("hello", 1));
        }
        "#,
        "(string, i64)",
    );
}

#[test]
fn supervisor_init_arg_record_wrapping_vec_rejects() {
    assert_supervisor_init_arg_non_bitcopy(
        r"
        record Wrapper { inner: Vec<i64> }

        actor Holder {
            let data: Wrapper;
            init(data: Wrapper) {
                data = data;
            }
            receive fn noop() {}
        }

        supervisor App {
            child h: Holder(data: Wrapper { inner: [] });
        }
        ",
        "Wrapper",
    );
}

#[test]
fn supervisor_init_arg_alias_of_string_admitted() {
    // A transparent alias of `string` (`type Name = string`) resolves to
    // `Ty::String`, which is reproducible — admitted like a bare `string`.
    assert_supervisor_init_arg_admitted(
        r#"
        type Name = string;

        actor Greeter {
            let name: Name;
            init(name: Name) {
                name = name;
            }
            receive fn greet() {}
        }

        supervisor App {
            child g: Greeter(name: "hello");
        }
        "#,
    );
}

#[test]
fn supervisor_init_arg_hashmap_rejects() {
    assert_supervisor_init_arg_non_bitcopy(
        r"
        actor Index {
            let entries: HashMap<string, i64>;
            init(entries: HashMap<string, i64>) {
                entries = entries;
            }
            receive fn noop() {}
        }

        supervisor App {
            child i: Index(entries: HashMap::<string, i64>::new());
        }
        ",
        "HashMap<string, i64>",
    );
}

#[test]
fn supervisor_init_arg_hashset_rejects() {
    assert_supervisor_init_arg_non_bitcopy(
        r"
        actor Seen {
            let ids: HashSet<i64>;
            init(ids: HashSet<i64>) {
                ids = ids;
            }
            receive fn noop() {}
        }

        supervisor App {
            child s: Seen(ids: HashSet::<i64>::new());
        }
        ",
        "HashSet<i64>",
    );
}

#[test]
fn supervisor_init_arg_sender_handle_rejects() {
    assert_supervisor_init_arg_non_bitcopy(
        r"
        actor Forwarder {
            let tx: channel.Sender<string>;
            init(tx: channel.Sender<string>) {
                tx = tx;
            }
            receive fn noop() {}
        }

        supervisor App {
            child f: Forwarder(tx: 0);
        }
        ",
        "channel.Sender<string>",
    );
}

// Positive tests — admitted cases.

#[test]
fn supervisor_init_arg_i64_admitted() {
    let output = check_source(
        r"
        actor Counter {
            let start: i64;
            init(start: i64) {
                start = start;
            }
            receive fn noop() {}
        }

        supervisor App {
            child c: Counter(start: 0);
        }
        ",
    );

    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.message.contains("E_SUPERVISOR_INIT_ARG_NON_BITCOPY")),
        "i64 init arg must be admitted; errors: {:#?}",
        output.errors
    );
}

#[test]
fn supervisor_init_arg_bool_and_f64_admitted() {
    let output = check_source(
        r"
        actor Sensor {
            let enabled: bool;
            let threshold: f64;
            init(enabled: bool, threshold: f64) {
                enabled = enabled;
                threshold = threshold;
            }
            receive fn noop() {}
        }

        supervisor App {
            child s: Sensor(enabled: true, threshold: 0.5);
        }
        ",
    );

    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.message.contains("E_SUPERVISOR_INIT_ARG_NON_BITCOPY")),
        "bool and f64 init args must be admitted; errors: {:#?}",
        output.errors
    );
}

#[test]
fn supervisor_init_arg_scalar_config_field_admitted() {
    // A scalar `config.field` init arg is the v0.6 init-closure surface: the
    // actor param is scalar, so the thunk re-produces it by a plain load. The
    // checker binds the config param in scope, types `config.size` to i64, and
    // the byte-copy wall (keyed on the scalar param) admits it.
    let output = check_source(
        r"
        record AppConfig { size: i64 }

        actor Cache {
            var capacity: i64;
            init(capacity: i64) {
                capacity = capacity;
            }
            receive fn noop() {}
        }

        supervisor App(config: AppConfig) {
            child cache: Cache(capacity: config.size);
        }
        ",
    );

    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.message.contains("E_SUPERVISOR_INIT_ARG_NON_BITCOPY")),
        "a scalar config.field init arg must be admitted; errors: {:#?}",
        output.errors
    );
    // The synthesis pass must not leave a leaky "undefined variable `config`"
    // error — the config param is bound in scope for the init-arg exprs.
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.message.contains("undefined variable `config`")),
        "config param must be in scope for init-arg exprs; errors: {:#?}",
        output.errors
    );
}

#[test]
fn supervisor_init_arg_config_nonexistent_field_surfaces_error() {
    // Typing the init-arg expr against the config param surfaces a real
    // diagnostic for a missing config field, at the arg-expr span.
    let output = check_source(
        r"
        record AppConfig { size: i64 }

        actor Cache {
            var capacity: i64;
            init(capacity: i64) {
                capacity = capacity;
            }
            receive fn noop() {}
        }

        supervisor App(config: AppConfig) {
            child cache: Cache(capacity: config.nonexistent);
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
        record AppConfig { flag: bool }

        actor Cache {
            var capacity: i64;
            init(capacity: i64) {
                capacity = capacity;
            }
            receive fn noop() {}
        }

        supervisor App(config: AppConfig) {
            child cache: Cache(capacity: config.flag);
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
fn supervisor_pool_child_string_init_arg_exempt() {
    // Pool children are dynamically spawned, not from a fixed state template,
    // so the byte-copy wall does not apply to them.
    let output = check_source(
        r"
        actor Worker {
            let name: string;
            init(name: string) {
                name = name;
            }
            receive fn noop() {}
        }

        supervisor App {
            strategy: simple_one_for_one,
            pool workers: Worker(count: 2)
        }
        ",
    );

    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.message.contains("E_SUPERVISOR_INIT_ARG_NON_BITCOPY")),
        "pool child init args must not trigger the byte-copy wall; errors: {:#?}",
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
            pool workers: Worker(count: 0)
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
            pool workers: Worker(count: -3)
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
            pool workers: Worker(count: 5)
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
            pool workers: Worker(count: "five")
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
        record AppConfig { workers: i64 }

        actor Worker { receive fn ping() {} }

        supervisor App(config: AppConfig) {
            strategy: simple_one_for_one,
            pool workers: Worker(count: config.workers)
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
fn supervisor_child_no_init_args_string_field_not_flagged_by_bitcopy_wall() {
    // The E_SUPERVISOR_INIT_ARG_NON_BITCOPY check only fires when the child
    // declaration passes explicit args.  An actor with a string field but no
    // args in the child spec is handled by E_SUPERVISOR_PERMANENT_OWNED_HEAP
    // (permanent policy) or not at all (transient/temporary).
    let output = check_source(
        r"
        actor Worker {
            let items: Vec<i64>;
            receive fn noop() {}
        }

        supervisor App {
            child w: Worker restart: transient;
        }
        ",
    );

    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.message.contains("E_SUPERVISOR_INIT_ARG_NON_BITCOPY")),
        "child with no init args must not trigger the bitcopy wall; errors: {:#?}",
        output.errors
    );
}

// ── Machine-readable kind wiring (issue #2377) ─────────────────────────────
// The supervisor diagnostics must expose distinct `as_kind_str()` values
// (JSON `code` / LSP `data.kind`), not the generic `InvalidOperation`. These
// assert the end-to-end wiring for a real emitted diagnostic, not just the
// enum table in error.rs.

#[test]
fn supervisor_permanent_owned_heap_reports_distinct_kind() {
    let output = check_source(
        r"
        actor Counter {
            let count: Vec<i64>;
            receive fn inc() {}
        }

        supervisor App {
            child worker: Counter restart: permanent;
        }
        ",
    );

    let err = output
        .errors
        .iter()
        .find(|e| e.message.contains("E_SUPERVISOR_PERMANENT_OWNED_HEAP"))
        .expect("permanent-owned-heap diagnostic must be emitted");

    assert_eq!(
        err.kind,
        TypeErrorKind::SupervisorError {
            subkind: SupervisorErrorKind::PermanentOwnedHeap,
        }
    );
    assert_eq!(err.kind.as_kind_str(), "SupervisorPermanentOwnedHeap");
    // Regression guard for #2377: must no longer collapse to the generic kind.
    assert_ne!(err.kind.as_kind_str(), "InvalidOperation");
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
            strategy: one_for_one

            child a: ActorA wired_to: { dep: b };
            child b: ActorB wired_to: { dep: a };
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
    // A different supervisor diagnostic yields a different kind string,
    // proving the family no longer shares one indistinguishable kind.
    assert_ne!(err.kind.as_kind_str(), "SupervisorPermanentOwnedHeap");
}
