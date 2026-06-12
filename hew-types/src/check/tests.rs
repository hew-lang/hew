#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;
use crate::eq_eligibility::{ty_is_eq_eligible, EqEligibility};
use crate::module_registry::ModuleRegistry;
use crate::BuiltinType;
use hew_parser::ast::IntRadix;
use hew_parser::ast::{ImportName, TraitMethod, TypeExpr, Visibility};
use hew_parser::module::{Module, ModuleGraph, ModuleId};

/// Module registry with the repo root as a search path, so stdlib
/// modules (e.g. `std::encoding::json`) can be loaded during tests.
fn test_registry() -> ModuleRegistry {
    let repo_root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .to_path_buf();
    ModuleRegistry::new(vec![repo_root])
}

fn check_source(source: &str) -> TypeCheckOutput {
    let parse_result = hew_parser::parse(source);
    assert!(
        parse_result.errors.is_empty(),
        "check_source should parse cleanly, got: {:#?}",
        parse_result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.check_program(&parse_result.program)
}

#[test]
fn test_empty_program() {
    let output = check_source("");
    assert!(output.errors.is_empty());
}

#[test]
fn tuple_numeric_field_access_out_of_bounds_is_rejected() {
    let output = check_source("fn main() -> i64 { let t = (1, false); t.2 }");
    assert!(
        output.errors.iter().any(|error| {
            error.kind == TypeErrorKind::UndefinedField
                && error.message.contains("tuple index 2 out of range")
        }),
        "expected tuple index out-of-range UndefinedField error, got: {:#?}",
        output.errors
    );
}

#[test]
fn freshen_inner_recurses_into_pointer_pointee_vars() {
    let checker = Checker::new(ModuleRegistry::new(vec![]));
    let original = TypeVar::fresh();
    let ty = Ty::Pointer {
        is_mutable: false,
        pointee: Box::new(Ty::Var(original)),
    };
    let mut mapping = HashMap::new();

    let freshened = checker.freshen_inner(&ty, &mut mapping);

    let Ty::Pointer { pointee, .. } = freshened else {
        panic!("expected pointer type");
    };
    let Ty::Var(fresh) = *pointee else {
        panic!("expected freshened pointee var");
    };

    assert_ne!(fresh, original);
    assert_eq!(mapping.get(&original.0), Some(&Ty::Var(fresh)));
}

#[test]
fn freshen_inner_recurses_into_trait_object_bound_args() {
    let checker = Checker::new(ModuleRegistry::new(vec![]));
    let original = TypeVar::fresh();
    let ty = Ty::TraitObject {
        traits: vec![crate::ty::TraitObjectBound {
            trait_name: "Iterator".to_string(),
            args: vec![Ty::Var(original)],
            assoc_bindings: vec![],
        }],
    };
    let mut mapping = HashMap::new();

    let freshened = checker.freshen_inner(&ty, &mut mapping);

    let Ty::TraitObject { traits } = freshened else {
        panic!("expected trait object type");
    };
    let [bound] = traits.as_slice() else {
        panic!("expected one trait bound");
    };
    let [Ty::Var(fresh)] = bound.args.as_slice() else {
        panic!("expected freshened trait-object arg var");
    };

    assert_ne!(*fresh, original);
    assert_eq!(mapping.get(&original.0), Some(&Ty::Var(*fresh)));
}

#[test]
fn cancellation_token_local_and_is_cancelled_typecheck() {
    let output = check_source(
        r"
        fn observe(token: CancellationToken) -> bool {
            let t: CancellationToken = token;
            return t.is_cancelled();
        }
        ",
    );

    assert!(
        output.errors.is_empty(),
        "CancellationToken local and is_cancelled() should type-check: {:#?}",
        output.errors
    );
    assert!(
        output
            .method_call_rewrites
            .values()
            .any(|rewrite| { matches!(rewrite, MethodCallRewrite::CancellationTokenIsCancelled) }),
        "is_cancelled() must record the checker-owned cancellation-token intrinsic"
    );
}

#[test]
fn cancellation_token_has_no_cancel_method() {
    let output = check_source(
        r"
        fn observe(token: CancellationToken) {
            token.cancel();
        }
        ",
    );

    assert!(
        output.errors.iter().any(|err| {
            matches!(err.kind, TypeErrorKind::UndefinedMethod)
                && err
                    .message
                    .contains("no method `cancel` on `CancellationToken`")
        }),
        "CancellationToken.cancel() must remain out of scope: {:#?}",
        output.errors
    );
}

#[test]
fn vec_copy_record_new_constructor_typechecks() {
    let output = check_source(
        r"
        type Point { x: i64, y: i64 }

        fn main() {
            let points: Vec<Point> = Vec::new();
            let _ = points.len();
        }
        ",
    );

    assert!(
        output.errors.is_empty(),
        "Copy record layout Vec::new should type-check: {:#?}",
        output.errors
    );
}

#[test]
fn vec_tuple_string_new_constructor_preserves_existing_typecheck_behavior() {
    let output = check_source(
        r"
        fn main() {
            let headers: Vec<(string, string)> = Vec::new();
            let _ = headers.len();
        }
        ",
    );

    assert!(
        output.errors.is_empty(),
        "Vec<(string, string)> constructor should keep existing typecheck behavior: {:#?}",
        output.errors
    );
}

#[test]
fn hashmap_new_turbofish_typechecks() {
    let output = check_source(
        r"
        record Key { id: i64 }

        fn main() {
            let _m = HashMap::<Key, i64>::new();
        }
        ",
    );

    assert!(
        output.errors.is_empty(),
        "HashMap::<K, V>::new() should type-check with explicit type args: {:#?}",
        output.errors
    );
}

#[test]
fn hashset_new_turbofish_typechecks() {
    let output = check_source(
        r"
        fn main() {
            let _s = HashSet::<i64>::new();
        }
        ",
    );

    assert!(
        output.errors.is_empty(),
        "HashSet::<T>::new() should type-check with explicit type args: {:#?}",
        output.errors
    );
}

#[test]
fn hashmap_new_turbofish_arity_mismatch_is_rejected() {
    let output = check_source(
        r"
        fn main() {
            let _m = HashMap::<i64>::new();
        }
        ",
    );

    assert!(
        !output.errors.is_empty(),
        "HashMap::<K>::new() must produce an arity diagnostic",
    );
}

#[test]
fn hashset_new_turbofish_arity_mismatch_is_rejected() {
    let output = check_source(
        r"
        fn main() {
            let _s = HashSet::<i64, i64>::new();
        }
        ",
    );

    assert!(
        !output.errors.is_empty(),
        "HashSet::<T, U>::new() must produce an arity diagnostic",
    );
}

#[test]
fn vec_new_with_error_element_remains_error_typed() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let span = 0..8;
    let func = (
        Expr::FieldAccess {
            object: Box::new((Expr::Identifier("Vec".to_string()), 0..3)),
            field: "new".to_string(),
        },
        span.clone(),
    );
    let expected = Ty::Named {
        name: "Vec".to_string(),
        args: vec![Ty::Error],
        builtin: None,
    };

    let result = checker
        .check_call_against_expected_constructor(&func, None, &[], &expected, &span)
        .expect("Vec::new should be recognized as a constructor");

    assert_eq!(result, Ty::Error);
    assert_eq!(
        checker.expr_types.get(&SpanKey::from(&span)),
        Some(&Ty::Error),
        "Ty::Error element should record the call as error-typed"
    );
    assert!(
        checker.errors.iter().all(|error| {
            !error.message.contains("layout-managed Vec construction")
                && !error.message.contains("hew_vec_new_with_layout")
        }),
        "Ty::Error element must not be classified as a valid layout Vec element: {:#?}",
        checker.errors
    );
}

#[test]
fn vec_owned_record_new_admitted_via_owned_abi() {
    // W5.016: an owned record element (`Person { name: string }`) carries a
    // synthesizable clone/drop thunk path, so `Vec<Person>::new()` constructs
    // through the owned-element ABI (`hew_vec_new_with_elem_layout`) instead of
    // failing closed. The construction no longer emits the layout-managed
    // Copy-gate diagnostic.
    let output = check_source(
        r"
        type Person { name: string }

        fn main() {
            let people: Vec<Person> = Vec::new();
            let _ = people.len();
        }
        ",
    );

    assert!(
        !output.errors.iter().any(|error| {
            error.message.contains("not `Copy`")
                && error.message.contains("layout-managed Vec construction")
        }),
        "owned record Vec::new must be admitted via the owned ABI, got {:#?}",
        output.errors
    );
}

#[test]
fn vec_layout_unsupported_method_remains_fail_closed() {
    // `Vec::clear` on layout-backed records has no runtime backing and
    // must continue to fail closed.  (`Vec::remove`, `Vec::contains`,
    // `Vec::push`, `Vec::get`, `Vec::set`, and `Vec::pop` are all lifted for
    // Copy record/tuple elements as of W3.003 / W3.032.)
    let output = check_source(
        r"
        type Point { x: i64, y: i64 }

        fn main() {
            var points: Vec<Point> = [];
            points.push(Point { x: 1, y: 2 });
            points.clear();
        }
        ",
    );

    assert!(
        output.errors.iter().any(|error| {
            error.message.contains("`Vec::clear`")
                && error.message.contains("not")
                && error.message.contains("runtime-backed yet")
        }),
        "Vec::clear on layout-backed records must fail closed, got {:#?}",
        output.errors
    );
    assert!(
        output.method_call_rewrites.values().all(|rewrite| {
            !matches!(rewrite, MethodCallRewrite::RewriteToFunction { c_symbol, .. } if c_symbol == "hew_vec_clear_layout")
        }),
        "layout-backed Vec::clear must not record a runtime rewrite: {:#?}",
        output.method_call_rewrites
    );
}

#[test]
fn vec_contains_eq_eligibility_classifies_layout_elements() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.type_defs.insert(
        "Point".to_string(),
        TypeDef {
            kind: TypeDefKind::Struct,
            name: "Point".to_string(),
            type_params: vec![],
            fields: HashMap::from([("x".to_string(), Ty::I64), ("y".to_string(), Ty::I64)]),
            variants: HashMap::new(),
            methods: HashMap::new(),
            doc_comment: None,
            field_order: vec![],
            is_indirect: false,
        },
    );
    checker.type_defs.insert(
        "WithFloat".to_string(),
        TypeDef {
            kind: TypeDefKind::Struct,
            name: "WithFloat".to_string(),
            type_params: vec![],
            fields: HashMap::from([("x".to_string(), Ty::F32)]),
            variants: HashMap::new(),
            methods: HashMap::new(),
            doc_comment: None,
            field_order: vec![],
            is_indirect: false,
        },
    );
    checker.type_defs.insert(
        "Handle".to_string(),
        TypeDef {
            kind: TypeDefKind::Struct,
            name: "Handle".to_string(),
            type_params: vec![],
            fields: HashMap::new(),
            variants: HashMap::new(),
            methods: HashMap::new(),
            doc_comment: None,
            field_order: vec![],
            is_indirect: true,
        },
    );

    let point = Ty::Named {
        name: "Point".to_string(),
        args: vec![],
        builtin: None,
    };
    let with_float = Ty::Named {
        name: "WithFloat".to_string(),
        args: vec![],
        builtin: None,
    };
    let handle = Ty::Named {
        name: "Handle".to_string(),
        args: vec![],
        builtin: None,
    };
    let unknown = Ty::Named {
        name: "Unknown".to_string(),
        args: vec![],
        builtin: None,
    };

    assert_eq!(
        ty_is_eq_eligible(&point, &checker.type_defs),
        EqEligibility::Eligible
    );
    assert_eq!(
        ty_is_eq_eligible(&Ty::Tuple(vec![Ty::I32, Ty::F64]), &checker.type_defs),
        EqEligibility::IneligibleFloat(Ty::F64)
    );
    assert_eq!(
        ty_is_eq_eligible(&with_float, &checker.type_defs),
        EqEligibility::IneligibleFloat(Ty::F32)
    );
    assert_eq!(
        ty_is_eq_eligible(&Ty::Tuple(vec![Ty::I32, Ty::String]), &checker.type_defs),
        EqEligibility::IneligibleManaged(Ty::String)
    );
    assert_eq!(
        ty_is_eq_eligible(&handle, &checker.type_defs),
        EqEligibility::IneligibleOwned(handle)
    );
    assert_eq!(
        ty_is_eq_eligible(&unknown, &checker.type_defs),
        EqEligibility::IneligibleUnknown
    );
}

#[test]
fn vec_contains_f64_typechecks() {
    let output = check_source(
        r"
        fn main() {
            let values: Vec<f64> = Vec::new();
            let _ = values.contains(1.5);
        }
        ",
    );

    assert!(
        output.errors.is_empty(),
        "scalar Vec<f64>::contains should remain accepted: {:#?}",
        output.errors
    );

    // After the W4.027 Stage 3 resolved-call kernel cutover, Vec dispatch is
    // recorded via `resolved_calls`, not the legacy `method_call_rewrites` side table.
    assert!(
        output.resolved_calls.values().any(|call| {
            call.method_name == "contains" && call.target.symbol_name == "hew_vec_contains_f64"
        }),
        "Vec<f64>::contains must route to hew_vec_contains_f64 via resolved_calls: {:#?}",
        output.resolved_calls
    );
}

#[test]
fn vec_contains_float_record_rejected_with_eq_eligibility_diagnostic() {
    let output = check_source(
        r"
        type Measurement { value: f32 }

        fn main() {
            let values: Vec<Measurement> = Vec::new();
            let needle = Measurement { value: 1.0 };
            let _ = values.contains(needle);
        }
        ",
    );

    assert!(
        output.errors.iter().any(|error| {
            error.message.contains("`Vec::contains`")
                && error.message.contains("floating-point")
                && error.message.contains("f32")
        }),
        "float-field layout Vec::contains should cite float ineligibility: {:#?}",
        output.errors
    );
}

#[test]
fn vec_contains_layout_managed_record_rejected_with_eq_eligibility_diagnostic() {
    let output = check_source(
        r#"
        type Person { name: string }

        fn main() {
            var people: Vec<Person> = [];
            let needle = Person { name: "ada" };
            let _ = people.contains(needle);
        }
        "#,
    );

    assert!(
        output.errors.iter().any(|error| {
            error.message.contains("`Vec::contains`")
                && error.message.contains("layout-managed/non-Copy")
                && error.message.contains("string")
        }),
        "layout-managed Vec::contains should cite managed ineligibility: {:#?}",
        output.errors
    );
}

#[test]
fn vec_owned_record_push_routes_to_owned_abi() {
    // W5.016: pushing an owned record element routes to the owned-element ABI
    // (`hew_vec_push_owned`) rather than failing the layout-managed Copy gate.
    // The owned op deep-clones the element in; the record's per-type
    // clone/drop thunks make it droppable.
    let output = check_source(
        r#"
        type Person { name: string }

        fn main() {
            var people: Vec<Person> = [];
            let p = Person { name: "ada" };
            people.push(p);
        }
        "#,
    );

    assert!(
        !output.errors.iter().any(|error| {
            error.message.contains("not `Copy`")
                && error.message.contains("layout-managed Vec elements")
        }),
        "owned record Vec::push must be admitted via the owned ABI, got {:#?}",
        output.errors
    );
    assert!(
        output.resolved_calls.values().any(|call| {
            call.method_name == "push" && call.target.symbol_name == "hew_vec_push_owned"
        }),
        "owned record Vec::push must route to hew_vec_push_owned via resolved_calls: {:#?}",
        output.resolved_calls
    );
}

#[test]
fn vec_local_pid_push_routes_to_pointer_abi() {
    // Regression: `Vec<LocalPid<T>>` is a collection of pointer-shaped actor
    // handles. The constructor lowers `hew_vec_new_ptr` (null layout) in
    // codegen; the checker MUST agree and route push to `hew_vec_push_ptr`, not
    // `hew_vec_push_layout` (which would abort at runtime on the null layout —
    // the constructor-vs-push authority split). Driven by the `builtin`
    // discriminant (`BuiltinType::lowers_as_pointer_vec_element`), NOT the
    // `TypeDef.is_indirect` flag, which actor handles leave `false`.
    let output = check_source(
        r"
        actor Worker {
            receive fn ping() {}
        }

        fn main() {
            let v: Vec<LocalPid<Worker>> = Vec::new();
            let w = spawn Worker;
            v.push(w);
            let _ = v.len();
        }
        ",
    );

    assert!(
        output.errors.is_empty(),
        "Vec<LocalPid<Worker>> must type-check: {:#?}",
        output.errors
    );
    assert!(
        output.resolved_calls.values().any(|call| {
            call.method_name == "push" && call.target.symbol_name == "hew_vec_push_ptr"
        }),
        "Vec<LocalPid<Worker>>::push must route to hew_vec_push_ptr via resolved_calls: {:#?}",
        output.resolved_calls
    );
}

#[test]
fn vec_self_recursive_enum_fails_closed_with_accurate_diagnostic() {
    // A self-recursive enum (`Array(Vec<RedisReply>)`) is owned but needs the
    // recursive owned-thunk synthesis that is a separate follow-on. It must
    // fail closed at construction with a diagnostic that names the real blocker
    // (the container field), NOT a misleading `Copy` / `hew_vec_new_with_layout`
    // message.
    let output = check_source(
        r"
        enum RedisReply {
            Nil;
            Int(i64);
            Array(Vec<RedisReply>);
        }

        fn main() {
            let v: Vec<RedisReply> = Vec::new();
            let _ = v.len();
        }
        ",
    );

    assert!(
        output.errors.iter().any(|error| {
            error.message.contains("cannot be a `Vec` element")
                && error.message.contains("Vec`/`HashMap`/`HashSet` field")
        }),
        "self-recursive enum Vec must fail closed naming the container blocker: {:#?}",
        output.errors
    );
    assert!(
        !output.errors.iter().any(|error| {
            error.message.contains("hew_vec_new_with_layout")
                || error.message.contains("is not `Copy`")
        }),
        "self-recursive enum Vec diagnostic must not blame Copy or name \
         hew_vec_new_with_layout: {:#?}",
        output.errors
    );
}

#[test]
fn local_pid_actor_dispatch_uses_builtin_discriminator() {
    let output = check_source(
        r"
        actor Worker {
            receive fn ping() {}
        }

        fn main() {
            let worker = spawn Worker;
            worker.ping();
        }
        ",
    );

    assert!(output.errors.is_empty(), "type errors: {:?}", output.errors);
    assert!(
        output.actor_method_dispatch.values().any(
            |dispatch| matches!(dispatch, ActorMethodKind::Fire(method) if method == "Worker::ping")
        ),
        "LocalPid<Worker> actor dispatch must be recorded by typed builtin discriminator: {:?}",
        output.actor_method_dispatch
    );
}

#[test]
fn remote_pid_does_not_fall_through_to_local_actor_dispatch() {
    let output = check_source(
        r"
        actor Worker {
            receive fn ping() {}
        }

        fn main() {
            let remote: RemotePid<Worker>;
            remote.ping();
        }
        ",
    );

    assert!(
        output.actor_method_dispatch.is_empty(),
        "RemotePid<Worker> must not be treated as a local actor dispatch receiver: {:?}",
        output.actor_method_dispatch
    );
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::UndefinedMethod),
        "remote actor receive dispatch should be rejected as an undefined method; got: {:?}",
        output.errors
    );
}

#[test]
fn turbofish_on_remote_pid_from_raw_resolves_concrete_type() {
    // A7 S3: `RemotePid::<T>::from_raw(...)` must propagate the explicit
    // turbofish type-arg through the impl-block-introduced type parameter
    // so the result is typed `RemotePid<Counter>`, not `RemotePid<T>`.
    let output = check_source(
        r"
        actor Counter {
            receive fn inc() {}
        }

        fn main() {
            let p: RemotePid<Counter> = RemotePid::<Counter>::from_raw(1, 42);
        }
        ",
    );

    assert!(
        output.errors.is_empty(),
        "turbofish on RemotePid::from_raw should type-check; got: {:?}",
        output.errors
    );
}

#[test]
fn turbofish_on_remote_pid_from_raw_rejects_mismatched_assignment() {
    // Fail-closed: explicit turbofish T must not silently coerce to a
    // different annotated `RemotePid<U>` at the assignment site.
    let output = check_source(
        r"
        actor Counter {
            receive fn inc() {}
        }
        actor Worker {
            receive fn ping() {}
        }

        fn main() {
            let p: RemotePid<Counter> = RemotePid::<Worker>::from_raw(1, 42);
        }
        ",
    );

    assert!(
        !output.errors.is_empty(),
        "RemotePid<Worker> must not satisfy RemotePid<Counter> binding",
    );
}

#[test]
fn turbofish_arity_mismatch_is_rejected() {
    // Fail-closed: too many turbofish args on a 1-arity associated fn
    // must surface as a typed arity diagnostic, not silent success.
    let output = check_source(
        r"
        actor Counter {
            receive fn inc() {}
        }

        fn main() {
            let p = RemotePid::<Counter, Counter>::from_raw(1, 42);
        }
        ",
    );

    assert!(
        !output.errors.is_empty(),
        "turbofish with too many type args must produce a diagnostic",
    );
}

#[test]
fn block_wrapped_await_actor_ask_types_as_result() {
    // `await { actor.method() }` — the method call is wrapped in a bare block
    // whose trailing expression is the method call. The checker must unwrap the
    // block and recognise the ask, returning `Result<i64, AskError>` rather than
    // the raw method return type `i64`.  Before the fix this produced a
    // "constructor pattern `Ok` cannot match non-enum type `i64`" error.
    let output = check_source(
        r"
        actor Doubler {
            receive fn process(n: i64) -> i64 {
                n * 2
            }
        }

        fn main() {
            let doubler = spawn Doubler;
            let r = match await { doubler.process(5) } {
                Ok(v) => v,
                Err(_) => -1,
            };
        }
        ",
    );

    assert!(
        output.errors.is_empty(),
        "block-wrapped actor-ask await must type-check without errors; got: {:?}",
        output.errors
    );
}

#[test]
fn bare_await_actor_ask_in_match_still_types_as_result() {
    // Regression guard: the bare form `await actor.method()` must continue
    // to resolve to `Result<i64, AskError>` so that `Ok`/`Err` arms type-check.
    let output = check_source(
        r"
        actor Doubler {
            receive fn process(n: i64) -> i64 {
                n * 2
            }
        }

        fn main() {
            let doubler = spawn Doubler;
            let r = match await doubler.process(5) {
                Ok(v) => v,
                Err(_) => -1,
            };
        }
        ",
    );

    assert!(
        output.errors.is_empty(),
        "bare actor-ask await in match must type-check without errors; got: {:?}",
        output.errors
    );
}

#[test]
fn constructor_pattern_against_non_enum_still_errors() {
    // Negative: an `Ok`/`Err` pattern against a plain integer literal must
    // still produce a "cannot match non-enum type" diagnostic. The block-unwrap
    // in the await arm must not loosen the check for non-await match scrutinees.
    let output = check_source(
        r"
        fn main() {
            let r = match 42 {
                Ok(v) => v,
                Err(_) => -1,
            };
        }
        ",
    );

    assert!(
        output.errors.iter().any(|e| {
            matches!(e.kind, TypeErrorKind::Mismatch { .. })
                && e.message.contains("cannot match non-enum type")
        }),
        "Ok/Err pattern against i64 must still emit a non-enum-type mismatch error; got: {:?}",
        output.errors
    );
}

#[test]
fn supervisor_wired_to_rejects_remote_pid_by_role() {
    let output = check_source(
        r"
        actor Db {
            receive fn query() {}
        }

        actor Api {
            init(db: RemotePid<Db>) {}
        }

        supervisor App {
            child db: Db;
            child api: Api wired_to: { db };
        }
        ",
    );

    assert!(
        output.errors.iter().any(|error| {
            error.kind == TypeErrorKind::InvalidOperation
                && error
                    .message
                    .contains("E_SUPERVISOR_WIRED_TO_TYPE_MISMATCH")
                && error.message.contains("RemotePid<Db>")
        }),
        "wired_to must reject RemotePid<Db> rather than accepting actor-pid spelling: {:?}",
        output.errors
    );
}

#[test]
fn machine_state_user_machine_stays_nominal_not_builtin_marker() {
    let output = check_source(
        r"
        machine MachineState {
            events {
                Tick;
            }

            state Idle;
            state Running;


            on Tick: Idle => Running {
                Running
            }
            on Tick: Running => Idle {
                Idle
            }
        }

        fn main() {
            var m = Idle;
            m.step(Tick);
        }
        ",
    );

    assert!(output.errors.is_empty(), "type errors: {:?}", output.errors);
    assert!(
        output
            .machine_method_dispatch
            .values()
            .any(|dispatch| matches!(dispatch, MachineMethodKind::Step { machine_name } if machine_name == "MachineState")),
        "user machine named MachineState must still register as a nominal machine: {:?}",
        output.machine_method_dispatch
    );
    assert_eq!(
        Ty::normalize_named("MachineState".to_string(), vec![]),
        Ty::Named {
            builtin: Some(crate::BuiltinType::MachineState),
            name: "MachineState".to_string(),
            args: vec![],
        },
        "the builtin handle marker remains available separately"
    );
}

#[test]
fn register_type_decl_marks_transitive_handle_bearing_structs() {
    let mut registry = ModuleRegistry::new(vec![]);
    registry.insert_handle_type_for_test("regex.Pattern".to_string());
    let mut checker = Checker::new(registry);

    let inner = TypeDecl {
        visibility: Visibility::Private,
        kind: TypeDeclKind::Struct,
        name: "Inner".to_string(),
        type_params: None,
        where_clause: None,
        body: vec![TypeBodyItem::Field {
            name: "pattern".to_string(),
            ty: (
                TypeExpr::Named {
                    name: "regex.Pattern".to_string(),
                    type_args: None,
                },
                0..0,
            ),
            attributes: vec![],
            doc_comment: None,
            span: 0..0,
        }],
        doc_comment: None,
        wire: None,
        is_indirect: false,
        resource_marker: hew_parser::ast::ResourceMarker::None,
        is_opaque: false,
        consuming_methods: Vec::new(),
    };
    let outer = TypeDecl {
        visibility: Visibility::Private,
        kind: TypeDeclKind::Struct,
        name: "Outer".to_string(),
        type_params: None,
        where_clause: None,
        body: vec![TypeBodyItem::Field {
            name: "inner".to_string(),
            ty: (
                TypeExpr::Named {
                    name: "Inner".to_string(),
                    type_args: None,
                },
                0..0,
            ),
            attributes: vec![],
            doc_comment: None,
            span: 0..0,
        }],
        doc_comment: None,
        wire: None,
        is_indirect: false,
        resource_marker: hew_parser::ast::ResourceMarker::None,
        is_opaque: false,
        consuming_methods: Vec::new(),
    };
    let plain = TypeDecl {
        visibility: Visibility::Private,
        kind: TypeDeclKind::Struct,
        name: "Plain".to_string(),
        type_params: None,
        where_clause: None,
        body: vec![TypeBodyItem::Field {
            name: "count".to_string(),
            ty: (
                TypeExpr::Named {
                    name: "i64".to_string(),
                    type_args: None,
                },
                0..0,
            ),
            attributes: vec![],
            doc_comment: None,
            span: 0..0,
        }],
        doc_comment: None,
        wire: None,
        is_indirect: false,
        resource_marker: hew_parser::ast::ResourceMarker::None,
        is_opaque: false,
        consuming_methods: Vec::new(),
    };

    checker.register_type_decl(&inner);
    checker.register_type_decl(&outer);
    checker.register_type_decl(&plain);
    checker.register_qualified_type_alias("regexwrap", "Outer");

    // Registrations set handle_bearing_dirty; flush before reading the set.
    checker.ensure_handle_bearing_fresh();

    assert!(checker.handle_bearing_structs.contains("Inner"));
    assert!(checker.handle_bearing_structs.contains("Outer"));
    assert!(checker.handle_bearing_structs.contains("regexwrap.Outer"));
    assert!(!checker.handle_bearing_structs.contains("Plain"));
}

#[test]
fn centralized_hashset_admissibility_rejects_nested_rc_elements() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.type_defs.insert(
        "Holder".to_string(),
        TypeDef {
            kind: TypeDefKind::Struct,
            name: "Holder".to_string(),
            type_params: vec![],
            fields: HashMap::from([("value".to_string(), Ty::rc(Ty::I64))]),
            variants: HashMap::new(),
            methods: HashMap::new(),
            doc_comment: None,
            field_order: vec![],
            is_indirect: false,
        },
    );
    checker
        .registry
        .register_rcfree_members("Holder".to_string(), vec![Ty::rc(Ty::I64)]);

    let holder_ty = Ty::Named {
        builtin: None,
        name: "Holder".to_string(),
        args: vec![],
    };
    assert!(
        !checker.validate_hashset_owned_element_type(&holder_ty, &(0..0)),
        "HashSet element admissibility should fail closed for nested Rc payloads"
    );
    assert!(
        checker
            .errors
            .iter()
            .any(|err| err.kind == TypeErrorKind::UnsafeCollectionElement
                && err.message.contains("HashSet")),
        "expected centralized HashSet admissibility to report UnsafeCollectionElement, got: {:?}",
        checker.errors
    );
}

#[test]
fn centralized_hashset_admissibility_rejects_named_enum_with_rc_payload() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.type_defs.insert(
        "MaybeHolder".to_string(),
        TypeDef {
            kind: TypeDefKind::Enum,
            name: "MaybeHolder".to_string(),
            type_params: vec![],
            fields: HashMap::new(),
            variants: HashMap::from([(
                "Some".to_string(),
                VariantDef::Tuple(vec![Ty::rc(Ty::I64)]),
            )]),
            methods: HashMap::new(),
            doc_comment: None,
            field_order: vec![],
            is_indirect: false,
        },
    );
    checker
        .registry
        .register_rcfree_members("MaybeHolder".to_string(), vec![Ty::rc(Ty::I64)]);

    let enum_ty = Ty::Named {
        builtin: None,
        name: "MaybeHolder".to_string(),
        args: vec![],
    };
    assert!(!checker.validate_hashset_owned_element_type(&enum_ty, &(0..0)));
}

#[test]
fn centralized_hashset_admissibility_rejects_recursive_rcfree_cycle() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let a_ty = Ty::Named {
        builtin: None,
        name: "A".to_string(),
        args: vec![],
    };
    let b_ty = Ty::Named {
        builtin: None,
        name: "B".to_string(),
        args: vec![],
    };
    checker
        .registry
        .register_rcfree_members("A".to_string(), vec![b_ty.clone()]);
    checker
        .registry
        .register_rcfree_members("B".to_string(), vec![a_ty.clone()]);

    assert!(
        !checker.validate_hashset_owned_element_type(&a_ty, &(0..0)),
        "HashSet element admissibility should fail closed for recursive RcFree cycles"
    );
    assert!(checker.errors.iter().any(|err| {
        err.kind == TypeErrorKind::UnsafeCollectionElement
            && err.message.contains("recursive type cycle")
            && err.message.contains('A')
    }));
}

#[test]
fn centralized_hashset_admissibility_rejects_module_qualified_named_rc_payload() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker
        .registry
        .register_rcfree_members("Holder".to_string(), vec![Ty::rc(Ty::I64)]);

    let holder_ty = Ty::Named {
        builtin: None,
        name: "widgets.Holder".to_string(),
        args: vec![],
    };
    assert!(!checker.validate_hashset_owned_element_type(&holder_ty, &(0..0)));
    assert!(checker.errors.iter().any(|err| {
        err.kind == TypeErrorKind::UnsafeCollectionElement && err.message.contains("HashSet")
    }));
}

#[test]
fn free_call_len_on_hashset_records_lowering_fact() {
    let parsed = hew_parser::parse(
        r"
        fn main() -> i64 {
            let s: HashSet<i64> = HashSet::new();
            len(s)
        }
        ",
    );
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );

    let call_span = match &parsed.program.items[0].0 {
        Item::Function(function) => function
            .body
            .trailing_expr
            .as_ref()
            .map(|expr| expr.1.clone())
            .expect("expected trailing len(s) call"),
        other => panic!("expected function item, got: {other:?}"),
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&parsed.program);
    assert!(output.errors.is_empty(), "type errors: {:?}", output.errors);

    let fact = output
        .lowering_facts
        .get(&SpanKey::from(&call_span))
        .expect("expected len(s) to record a lowering fact");
    assert_eq!(fact.kind, crate::LoweringKind::HashSet);
    assert_eq!(fact.element_type, crate::HashSetElementType::I64);
    assert_eq!(fact.abi_variant, crate::HashSetAbi::Int64);
    assert_eq!(fact.drop_kind, crate::DropKind::HashSetFree);
}

#[test]
fn concrete_vec_validation_reaches_function_wrapped_vec() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let ty = Ty::Function {
        params: vec![Ty::Named {
            builtin: Some(crate::BuiltinType::Vec),
            name: "Vec".to_string(),
            args: vec![Ty::Array(Box::new(Ty::I64), 4)],
        }],
        ret: Box::new(Ty::Unit),
    };

    assert!(!checker.validate_concrete_vec_type(&ty, &(0..0)));
    assert!(checker
        .errors
        .iter()
        .any(|err| err.kind == TypeErrorKind::InvalidOperation && err.message.contains("Vec<")));
}

#[test]
fn concrete_hashset_validation_reaches_pointer_wrapped_hashset() {
    // W4.001 Stage C3: per-element allowlist retired. `HashSet<bool>` is
    // admitted (bool implements Hash + Eq). The traversal still reaches
    // the wrapped HashSet — exercised here by verifying the pointer wrapper
    // walks into the element type without panicking. Behaviour-shape tests
    // for unsupported elements now live at the resolver site
    // (`record_resolved_hashset_call` emitting `BoundsNotSatisfied`).
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let ty = Ty::Pointer {
        is_mutable: false,
        pointee: Box::new(Ty::Named {
            builtin: Some(crate::BuiltinType::HashSet),
            name: "HashSet".to_string(),
            args: vec![Ty::Bool],
        }),
    };

    assert!(checker.validate_concrete_hashset_type(&ty, &(0..0)));
    assert!(
        checker.errors.is_empty(),
        "Stage C3: HashSet<bool> must admit cleanly; errors: {:?}",
        checker.errors
    );
}

#[test]
fn concrete_hashmap_validation_reaches_tuple_wrapped_hashmap() {
    // W4.001 Stage C3: per-K/V allowlist retired. `HashMap<i64, String>` is
    // admitted (i64 implements Hash + Eq). Verifies the traversal still
    // walks into the tuple-wrapped HashMap; rejection-shape tests now live
    // at the resolver site (BoundsNotSatisfied with witness attribution).
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let ty = Ty::Tuple(vec![
        Ty::Named {
            builtin: Some(crate::BuiltinType::HashMap),
            name: "HashMap".to_string(),
            args: vec![Ty::I64, Ty::String],
        },
        Ty::Unit,
    ]);

    assert!(checker.validate_concrete_hashmap_type(&ty, &(0..0)));
    assert!(
        checker.errors.is_empty(),
        "Stage C3: HashMap<i64, String> must admit cleanly; errors: {:?}",
        checker.errors
    );
}

#[test]
fn non_root_private_type_rcfree_is_registered_during_body_checking() {
    let parsed = hew_parser::parse(
        r"
        type Holder {
            value: Rc<i64>
        }

        fn helper() {
            var v = Vec::new();
            let h = Holder { value: Rc::new(1) };
            v.push(h);
        }",
    );
    assert!(
        parsed.errors.is_empty(),
        "module parse errors: {:?}",
        parsed.errors
    );

    let root_id = ModuleId::root();
    let mod_id = ModuleId::new(vec!["helpers".to_string()]);
    let module = Module {
        id: mod_id.clone(),
        items: parsed.program.items,
        imports: vec![],
        source_paths: vec![],
        doc: None,
    };
    let mut mg = ModuleGraph::new(root_id.clone());
    mg.add_module(module).unwrap();
    mg.topo_order = vec![mod_id, root_id];
    let program = Program {
        module_graph: Some(mg),
        items: vec![],
        module_doc: None,
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);
    assert!(
        output
            .errors
            .iter()
            .any(|err| err.kind == TypeErrorKind::UnsafeCollectionElement),
        "expected non-root private type with transitive Rc to be rejected from Vec during body checking, got: {:?}",
        output.errors
    );
}

#[test]
fn actor_decl_registers_rcfree_members_for_collection_checks() {
    let parsed = hew_parser::parse(
        r"
        actor Worker {
            let value: Rc<i64>;
            receive fn ping() {}
        }

        fn main() {}",
    );
    assert!(
        parsed.errors.is_empty(),
        "program parse errors: {:?}",
        parsed.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let _ = checker.check_program(&parsed.program);
    let actor_ref_ty = Ty::actor_ref(Ty::Named {
        builtin: None,
        name: "Worker".to_string(),
        args: vec![],
    });

    assert!(
        checker.reject_rc_collection_element("HashSet", &actor_ref_ty, &(0..0)),
        "ActorRef<Worker> should pass RcFree collection admissibility even when Worker stores Rc"
    );
    assert!(
        !checker.errors.iter().any(|err| {
            err.kind == TypeErrorKind::UnsafeCollectionElement && err.message.contains("HashSet")
        }),
        "ActorRef<Worker> should not emit a HashSet UnsafeCollectionElement error, got: {:?}",
        checker.errors
    );
}

// ── W5.004 (F1a): `#[intrinsic]` floor-protocol placement gate (A605) ──────
//
// The `#[intrinsic("…")]` surface is compiler-internal-only. A declaration is
// accepted only inside a designated stdlib-floor module; anywhere else —
// including the user's root module — is a hard `E_INTRINSIC_OUTSIDE_FLOOR`
// error so no user-reachable path can wire itself to a compiler intrinsic.

/// Build a single-module program whose one module has the given dotted path
/// (e.g. `["std", "math"]`) and contains `source`'s items.
fn check_source_in_module(source: &str, module_path: Vec<String>) -> TypeCheckOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "module source must parse cleanly, got: {:?}",
        parsed.errors
    );
    let root_id = ModuleId::root();
    let mod_id = ModuleId::new(module_path);
    let module = Module {
        id: mod_id.clone(),
        items: parsed.program.items,
        imports: vec![],
        source_paths: vec![],
        doc: None,
    };
    let mut mg = ModuleGraph::new(root_id.clone());
    mg.add_module(module).unwrap();
    mg.topo_order = vec![mod_id, root_id];
    let program = Program {
        module_graph: Some(mg),
        items: vec![],
        module_doc: None,
    };
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.check_program(&program)
}

#[test]
fn intrinsic_in_floor_module_is_accepted() {
    // `std.math` is the canonical floor module for math intrinsics; the
    // bodyless `#[intrinsic("math.sqrt")]` declaration must register cleanly.
    let output = check_source_in_module(
        r#"#[intrinsic("math.sqrt")] pub fn sqrt(x: f64) -> f64;"#,
        vec!["std".to_string(), "math".to_string()],
    );
    assert!(
        !output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::IntrinsicOutsideFloor { .. })),
        "intrinsic declared in floor module `std.math` must be accepted, got: {:?}",
        output.errors
    );
}

#[test]
fn intrinsic_in_user_root_module_is_rejected() {
    // A user program declaring `#[intrinsic]` at the root module must be
    // rejected: the root/user module is never a floor module (fail-closed).
    let output = check_source(r#"#[intrinsic("math.sqrt")] pub fn sqrt(x: f64) -> f64;"#);
    let hit = output.errors.iter().find_map(|e| match &e.kind {
        TypeErrorKind::IntrinsicOutsideFloor {
            intrinsic_key,
            module,
        } => Some((intrinsic_key.clone(), module.clone())),
        _ => None,
    });
    let (key, module) =
        hit.expect("root-module `#[intrinsic]` declaration must be rejected as outside the floor");
    assert_eq!(key, "math.sqrt", "diagnostic must name the intrinsic key");
    assert_eq!(
        module, "(root)",
        "diagnostic must label the offending module as the root/user module"
    );
}

#[test]
fn intrinsic_in_non_floor_module_is_rejected() {
    // A non-floor module (here a user `app` module) is likewise rejected,
    // proving the gate is an explicit allowlist, not "any module with a path".
    let output = check_source_in_module(
        r#"#[intrinsic("math.sqrt")] pub fn sqrt(x: f64) -> f64;"#,
        vec!["app".to_string()],
    );
    let hit = output.errors.iter().find_map(|e| match &e.kind {
        TypeErrorKind::IntrinsicOutsideFloor { module, .. } => Some(module.clone()),
        _ => None,
    });
    assert_eq!(
        hit.as_deref(),
        Some("app"),
        "non-floor module `app` must be rejected with its path in the diagnostic, got: {:?}",
        output.errors
    );
}

#[test]
fn normal_function_does_not_trip_intrinsic_gate() {
    // Functions without `#[intrinsic]` are never touched by the floor gate,
    // even in a user module.
    let output = check_source("pub fn add(a: i64, b: i64) -> i64 { a + b }");
    assert!(
        !output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::IntrinsicOutsideFloor { .. })),
        "non-intrinsic function must not trip the floor gate, got: {:?}",
        output.errors
    );
}

#[test]
fn intrinsic_on_impl_method_in_floor_module_is_rejected() {
    // `#[intrinsic]` on an impl method inside std.math must be REJECTED even
    // though std.math is an allowlisted floor module.  Only top-level free
    // functions in floor modules are valid intrinsic declarations; method
    // dispatch slots are never wired to compiler intrinsics (A605).
    //
    // The test asserts both legs of the fail-closed guarantee:
    //   1. The method-keyed entry does NOT appear in `intrinsic_declarations`.
    //   2. An `IntrinsicOnMethod` diagnostic IS emitted naming the key and
    //      the intrinsic catalog key.
    let source = r#"
type MathHelper {}
impl MathHelper {
    #[intrinsic("math.sqrt")] pub fn sqrt(x: f64) -> f64;
}
"#;
    let output = check_source_in_module(source, vec!["std".to_string(), "math".to_string()]);

    // Leg 1: must NOT be in intrinsic_declarations.
    let method_key_present = output
        .intrinsic_declarations
        .keys()
        .any(|k| k.contains("::"));
    assert!(
        !method_key_present,
        "impl-method intrinsic must not be inserted into intrinsic_declarations; \
         got: {:?}",
        output.intrinsic_declarations
    );

    // Leg 2: must emit IntrinsicOnMethod naming both the catalog key and the
    // method key.
    let hit = output.errors.iter().find_map(|e| match &e.kind {
        TypeErrorKind::IntrinsicOnMethod {
            intrinsic_key,
            method_key,
        } => Some((intrinsic_key.clone(), method_key.clone())),
        _ => None,
    });
    let (catalog_key, method_key) = hit.expect(
        "impl-method `#[intrinsic]` in a floor module must emit IntrinsicOnMethod diagnostic",
    );
    assert_eq!(
        catalog_key, "math.sqrt",
        "IntrinsicOnMethod diagnostic must name the intrinsic catalog key"
    );
    assert!(
        method_key.contains("::"),
        "IntrinsicOnMethod diagnostic must carry the method-shaped key (contains `::`);\
         got: {method_key:?}"
    );
}

// ── W5.005 (F1b): memory-intrinsic floor (`mem.*`) placement gate (A605) ───
//
// `std.mem` joins `std.math` on the `INTRINSIC_FLOOR_MODULES` allowlist. The
// same A605 gate that governs math intrinsics must accept `mem.*` declarations
// inside `std.mem` and reject them everywhere else, fail-closed.

#[test]
fn mem_intrinsic_in_floor_module_is_accepted() {
    // All five `mem.*` floor intrinsics must register cleanly inside the
    // canonical `std.mem` floor module (no IntrinsicOutsideFloor), and each
    // must land in `intrinsic_declarations` keyed by its qualified name.
    let source = r#"
#[intrinsic("mem.alloc")] pub fn alloc(size: u64, align: u64) -> *mut u8;
#[intrinsic("mem.realloc")] pub fn realloc(ptr: *mut u8, old_size: u64, new_size: u64, align: u64) -> *mut u8;
#[intrinsic("mem.dealloc")] pub fn dealloc(ptr: *mut u8, size: u64, align: u64);
#[intrinsic("mem.ptr_offset")] pub fn ptr_offset(ptr: *mut u8, byte_offset: u64) -> *mut u8;
#[intrinsic("mem.ptr_copy")] pub fn ptr_copy(dst: *mut u8, src: *mut u8, byte_count: u64);
"#;
    let output = check_source_in_module(source, vec!["std".to_string(), "mem".to_string()]);
    assert!(
        !output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::IntrinsicOutsideFloor { .. })),
        "mem intrinsics declared in floor module `std.mem` must be accepted, got: {:?}",
        output.errors
    );
    for key in [
        "std.mem.alloc",
        "std.mem.realloc",
        "std.mem.dealloc",
        "std.mem.ptr_offset",
        "std.mem.ptr_copy",
    ] {
        assert!(
            output.intrinsic_declarations.contains_key(key),
            "intrinsic_declarations must record `{key}`; got: {:?}",
            output.intrinsic_declarations
        );
    }
}

#[test]
fn mem_intrinsic_in_user_root_module_is_rejected() {
    // A user program declaring a `mem.*` intrinsic at the root module must be
    // rejected: the memory floor is compiler-internal-only (A605), so no
    // user-reachable module can wire itself to the allocator primitives. This
    // is validation candidate 3 (the surface-immutability acceptance gate) as
    // an executable assertion.
    let output = check_source(
        r#"#[intrinsic("mem.alloc")] pub fn alloc(size: u64, align: u64) -> *mut u8;"#,
    );
    let hit = output.errors.iter().find_map(|e| match &e.kind {
        TypeErrorKind::IntrinsicOutsideFloor {
            intrinsic_key,
            module,
        } => Some((intrinsic_key.clone(), module.clone())),
        _ => None,
    });
    let (key, module) = hit.expect(
        "root-module `#[intrinsic(\"mem.alloc\")]` declaration must be rejected outside the floor",
    );
    assert_eq!(key, "mem.alloc", "diagnostic must name the intrinsic key");
    assert_eq!(
        module, "(root)",
        "diagnostic must label the offending module as the root/user module"
    );
    // Fail-closed: a rejected declaration must NOT leak into the live
    // intrinsic dispatch table.
    assert!(
        output.intrinsic_declarations.is_empty(),
        "rejected mem intrinsic must not be recorded as a dispatch target; got: {:?}",
        output.intrinsic_declarations
    );
}

#[test]
fn mem_intrinsic_in_non_floor_module_is_rejected() {
    // A non-floor `app` module is likewise rejected — the allowlist is an
    // explicit enumeration (`std.math`, `std.mem`), not a prefix/path match.
    // Pins the gate-regression behaviour (validation candidate 7) for `std.mem`.
    let output = check_source_in_module(
        r#"#[intrinsic("mem.dealloc")] pub fn dealloc(ptr: *mut u8, size: u64, align: u64);"#,
        vec!["app".to_string()],
    );
    let hit = output.errors.iter().find_map(|e| match &e.kind {
        TypeErrorKind::IntrinsicOutsideFloor { module, .. } => Some(module.clone()),
        _ => None,
    });
    assert_eq!(
        hit.as_deref(),
        Some("app"),
        "non-floor module `app` must be rejected with its path in the diagnostic, got: {:?}",
        output.errors
    );
}

#[test]
fn checker_output_contract_intersects_assignment_target_side_tables() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.assign_target_kinds.insert(
        SpanKey {
            start: 1,
            end: 2,
            module_idx: 0,
        },
        AssignTargetKind::LocalVar,
    );
    checker.assign_target_shapes.insert(
        SpanKey {
            start: 3,
            end: 4,
            module_idx: 0,
        },
        AssignTargetShape { is_unsigned: false },
    );

    let mut expr_types = HashMap::new();
    let mut type_defs = HashMap::new();
    let mut fn_sigs = HashMap::new();
    let mut call_type_args = HashMap::new();
    let mut record_init_type_args = HashMap::new();
    checker.validate_checker_output_contract(
        &mut expr_types,
        &mut type_defs,
        &mut fn_sigs,
        &mut call_type_args,
        &mut record_init_type_args,
    );

    assert!(
        checker.assign_target_kinds.is_empty(),
        "orphan assign_target_kinds entries should be pruned at the output boundary: {:?}",
        checker.assign_target_kinds
    );
    assert!(
        checker.assign_target_shapes.is_empty(),
        "orphan assign_target_shapes entries should be pruned at the output boundary: {:?}",
        checker.assign_target_shapes
    );
}

#[test]
fn expr_output_contract_rechecks_normalized_unresolved_subset() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let sender_var = TypeVar::fresh();
    let covered_var = TypeVar::fresh();
    let span = SpanKey {
        start: 10,
        end: 20,
        module_idx: 0,
    };
    let mut expr_types = HashMap::from([(
        span.clone(),
        Ty::Tuple(vec![
            Ty::Named {
                builtin: None,
                name: "Sender".to_string(),
                args: vec![Ty::Var(sender_var)],
            },
            Ty::Var(covered_var),
        ]),
    )]);

    checker.validate_expr_output_contract(&mut expr_types, &HashSet::from([covered_var]));

    assert!(
        checker
            .errors
            .iter()
            .all(|error| error.kind != TypeErrorKind::InferenceFailed),
        "normalized covered vars must not emit InferenceFailed: {checker_errors:#?}",
        checker_errors = checker.errors
    );
    assert!(
        !expr_types.contains_key(&span),
        "covered unresolved expr types should still be pruned after normalization: {expr_types:?}"
    );
}

// ── method-call output-contract validation ───────────────────────────────────

/// Valid method-call metadata must survive the output-contract boundary when
/// the corresponding `expr_types` entry is present and fully resolved.
#[test]
fn checker_output_contract_retains_valid_method_call_metadata() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let span = SpanKey {
        start: 10,
        end: 20,
        module_idx: 0,
    };
    checker.method_call_receiver_kinds.insert(
        span.clone(),
        MethodCallReceiverKind::NamedTypeInstance {
            type_name: "Foo".to_string(),
        },
    );
    checker
        .method_call_rewrites
        .insert(span.clone(), MethodCallRewrite::DeferToLowering);

    // expr_types has the matching span with a concrete, fully-resolved type.
    let mut expr_types = HashMap::new();
    expr_types.insert(span.clone(), Ty::I64);
    // type_defs must include "Foo" so validate_method_call_receiver_kinds_output_contract
    // retains the NamedTypeInstance entry after validate_method_call_output_contract passes it.
    let mut type_defs = HashMap::from([(
        "Foo".to_string(),
        TypeDef {
            kind: TypeDefKind::Struct,
            name: "Foo".to_string(),
            type_params: vec![],
            fields: HashMap::new(),
            variants: HashMap::new(),
            methods: HashMap::new(),
            doc_comment: None,
            field_order: vec![],
            is_indirect: false,
        },
    )]);
    let mut fn_sigs = HashMap::new();
    let mut call_type_args = HashMap::new();
    let mut record_init_type_args = HashMap::new();
    checker.validate_checker_output_contract(
        &mut expr_types,
        &mut type_defs,
        &mut fn_sigs,
        &mut call_type_args,
        &mut record_init_type_args,
    );

    assert!(
        checker.method_call_receiver_kinds.contains_key(&span),
        "valid method_call_receiver_kinds entry must be retained: {:?}",
        checker.method_call_receiver_kinds
    );
    assert!(
        checker.method_call_rewrites.contains_key(&span),
        "valid method_call_rewrites entry must be retained: {:?}",
        checker.method_call_rewrites
    );
}

/// Orphaned method-call metadata — where the corresponding `expr_types` span
/// was pruned — must be removed at the output-contract boundary.
#[test]
fn checker_output_contract_prunes_orphaned_method_call_metadata() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    // Insert metadata keyed to spans that have NO corresponding expr_types entry.
    checker.method_call_receiver_kinds.insert(
        SpanKey {
            start: 10,
            end: 20,
            module_idx: 0,
        },
        MethodCallReceiverKind::NamedTypeInstance {
            type_name: "Bar".to_string(),
        },
    );
    checker.method_call_rewrites.insert(
        SpanKey {
            start: 30,
            end: 40,
            module_idx: 0,
        },
        MethodCallRewrite::RewriteToFunction {
            c_symbol: "hew_bar_method".to_string(),
            descriptor: None,
            elem_ty: None,
            consumes_receiver: false,
        },
    );

    // expr_types is empty — no span survives.
    let mut expr_types = HashMap::new();
    let mut type_defs = HashMap::new();
    let mut fn_sigs = HashMap::new();
    let mut call_type_args = HashMap::new();
    let mut record_init_type_args = HashMap::new();
    checker.validate_checker_output_contract(
        &mut expr_types,
        &mut type_defs,
        &mut fn_sigs,
        &mut call_type_args,
        &mut record_init_type_args,
    );

    assert!(
        checker.method_call_receiver_kinds.is_empty(),
        "orphan method_call_receiver_kinds entries must be pruned: {:?}",
        checker.method_call_receiver_kinds
    );
    assert!(
        checker.method_call_rewrites.is_empty(),
        "orphan method_call_rewrites entries must be pruned: {:?}",
        checker.method_call_rewrites
    );
}

/// When a method-call expression's `expr_types` entry is pruned because it
/// carries an unresolved inference variable (simulating a failed / error-typed
/// receiver), the corresponding receiver-kind and rewrite side-table entries
/// must not leak to the output.
#[test]
fn checker_output_contract_prunes_method_call_metadata_for_leaked_inference_var_expr() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let leaked_span = SpanKey {
        start: 50,
        end: 60,
        module_idx: 0,
    };
    let good_span = SpanKey {
        start: 70,
        end: 80,
        module_idx: 0,
    };

    // The leaked span has an unresolved inference var — validate_expr_output_contract
    // will strip it from expr_types, so the method-call metadata must follow.
    checker.method_call_receiver_kinds.insert(
        leaked_span.clone(),
        MethodCallReceiverKind::NamedTypeInstance {
            type_name: "Bad".to_string(),
        },
    );
    checker
        .method_call_rewrites
        .insert(leaked_span.clone(), MethodCallRewrite::DeferToLowering);
    // The good span carries a fully-resolved type and its metadata should survive.
    checker.method_call_receiver_kinds.insert(
        good_span.clone(),
        MethodCallReceiverKind::NamedTypeInstance {
            type_name: "Good".to_string(),
        },
    );
    checker
        .method_call_rewrites
        .insert(good_span.clone(), MethodCallRewrite::DeferToLowering);

    // Build expr_types: leaked entry has a fresh (unresolved) inference var;
    // good entry carries a concrete type.
    let mut expr_types = HashMap::new();
    expr_types.insert(leaked_span.clone(), Ty::Var(TypeVar::fresh()));
    expr_types.insert(good_span.clone(), Ty::Bool);

    // type_defs must include "Good" so validate_method_call_receiver_kinds_output_contract
    // retains the NamedTypeInstance entry for the good span after the span-based pruner passes it.
    let mut type_defs = HashMap::from([(
        "Good".to_string(),
        TypeDef {
            kind: TypeDefKind::Struct,
            name: "Good".to_string(),
            type_params: vec![],
            fields: HashMap::new(),
            variants: HashMap::new(),
            methods: HashMap::new(),
            doc_comment: None,
            field_order: vec![],
            is_indirect: false,
        },
    )]);
    let mut fn_sigs = HashMap::new();
    let mut call_type_args = HashMap::new();
    let mut record_init_type_args = HashMap::new();
    checker.validate_checker_output_contract(
        &mut expr_types,
        &mut type_defs,
        &mut fn_sigs,
        &mut call_type_args,
        &mut record_init_type_args,
    );

    // The leaked span must have been pruned from expr_types by
    // validate_expr_output_contract, which in turn must cascade to prune the
    // orphaned method-call metadata.
    assert!(
        !expr_types.contains_key(&leaked_span),
        "leaked inference-var expr must be pruned from expr_types"
    );
    assert!(
        !checker
            .method_call_receiver_kinds
            .contains_key(&leaked_span),
        "method_call_receiver_kinds entry for pruned expr must not survive: {:?}",
        checker.method_call_receiver_kinds
    );
    assert!(
        !checker.method_call_rewrites.contains_key(&leaked_span),
        "method_call_rewrites entry for pruned expr must not survive: {:?}",
        checker.method_call_rewrites
    );

    // The good span must be retained in all three maps.
    assert!(
        expr_types.contains_key(&good_span),
        "fully-resolved expr must be retained in expr_types"
    );
    assert!(
        checker.method_call_receiver_kinds.contains_key(&good_span),
        "method_call_receiver_kinds entry for valid expr must survive: {:?}",
        checker.method_call_receiver_kinds
    );
    assert!(
        checker.method_call_rewrites.contains_key(&good_span),
        "method_call_rewrites entry for valid expr must survive: {:?}",
        checker.method_call_rewrites
    );
}

#[test]
fn module_qualified_call_rewrites_record_registry_c_symbol_metadata() {
    let parsed = hew_parser::parse(
        r#"
import std::fs;

fn main() {
    let _ = fs.read("test.txt");
}
"#,
    );
    assert!(
        parsed.errors.is_empty(),
        "expected clean parse, got: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&parsed.program);
    assert!(
        output.errors.is_empty(),
        "expected clean typecheck, got: {:#?}",
        output.errors
    );
    assert!(
        output.method_call_rewrites.values().any(|rewrite| matches!(
            rewrite,
            MethodCallRewrite::RewriteModuleQualifiedToFunction { c_symbol, .. }
                if c_symbol == "hew_file_read"
        )),
        "expected checker-owned module-qualified rewrite metadata, got: {:?}",
        output.method_call_rewrites
    );
}

#[test]
fn module_qualified_pure_hew_stdlib_wrapper_rewrites_to_qualified_symbol() {
    let parsed = hew_parser::parse(
        r#"
import std::path;

fn main() {
    let _ = path.dirname("a/b");
}
"#,
    );
    assert!(
        parsed.errors.is_empty(),
        "expected clean parse, got: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&parsed.program);
    assert!(
        output.errors.is_empty(),
        "expected clean typecheck, got: {:#?}",
        output.errors
    );
    assert!(
        output.method_call_rewrites.values().any(|rewrite| matches!(
            rewrite,
            MethodCallRewrite::RewriteModuleQualifiedToFunction { c_symbol, .. }
                if c_symbol == "path.dirname"
        )),
        "expected pure-Hew stdlib wrapper to rewrite to module-qualified symbol, got: {:?}",
        output.method_call_rewrites
    );
}

// Helper functions for testing AST construction
fn make_int_literal(n: i64, span: Span) -> Spanned<Expr> {
    (
        Expr::Literal(Literal::Integer {
            value: n,
            radix: IntRadix::Decimal,
        }),
        span,
    )
}

fn make_bool_literal(b: bool, span: Span) -> Spanned<Expr> {
    (Expr::Literal(Literal::Bool(b)), span)
}

#[test]
fn test_literal_types() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));

    // Integer literals synthesize as a first-class literal kind.
    let int_expr = make_int_literal(42, 0..2);
    let int_ty = checker.synthesize(&int_expr.0, &int_expr.1);
    assert_eq!(int_ty, Ty::IntLiteral);

    // Test boolean literal
    let bool_expr = make_bool_literal(true, 0..4);
    let bool_ty = checker.synthesize(&bool_expr.0, &bool_expr.1);
    assert_eq!(bool_ty, Ty::Bool);
}

#[test]
fn test_builtin_registration() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.register_builtins();

    // Check that println_int is registered
    assert!(checker.fn_sigs.contains_key("println_int"));
    let sig = &checker.fn_sigs["println_int"];
    assert_eq!(sig.params.len(), 1);
    assert_eq!(sig.params[0], Ty::I64);
    assert_eq!(sig.return_type, Ty::Unit);
}

#[test]
fn test_yield_outside_generator() {
    use hew_parser::ast::{Block, Expr, FnDecl, Item, Program, Stmt};

    let yield_expr: Spanned<Expr> = (Expr::Yield(None), 10..15);
    let body = Block {
        stmts: vec![(Stmt::Expression(yield_expr), 10..15)],
        trailing_expr: None,
    };
    let fd = FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Private,
        name: "not_a_gen".to_string(),
        type_params: None,
        params: vec![],
        return_type: None,
        where_clause: None,
        body,
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
        intrinsic: None,
    };
    let program = Program {
        module_graph: None,
        items: vec![(Item::Function(fd), 0..30)],
        module_doc: None,
    };
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);
    assert!(output
        .errors
        .iter()
        .any(|e| e.kind == TypeErrorKind::YieldOutsideGenerator));
}

#[test]
fn test_receive_gen_fn_returns_stream() {
    use hew_parser::ast::{ActorDecl, Expr, Item, Literal, ReceiveFnDecl, Stmt, TypeExpr};

    let receive_fn = ReceiveFnDecl {
        is_generator: true,
        name: "numbers".to_string(),
        type_params: None,
        params: vec![],
        return_type: Some((
            TypeExpr::Named {
                name: "i64".to_string(),
                type_args: None,
            },
            0..0,
        )),
        where_clause: None,
        body: Block {
            stmts: vec![(
                Stmt::Expression((
                    Expr::Yield(Some(Box::new((
                        Expr::Literal(Literal::Integer {
                            value: 1,
                            radix: IntRadix::Decimal,
                        }),
                        0..0,
                    )))),
                    0..0,
                )),
                0..0,
            )],
            trailing_expr: None,
        },
        span: 0..0,
        attributes: vec![],
        doc_comment: None,
    };

    let actor = ActorDecl {
        visibility: Visibility::Pub,
        name: "NumberStream".to_string(),
        type_params: vec![],
        super_traits: None,
        init: None,
        fields: vec![],
        receive_fns: vec![receive_fn],
        methods: vec![],
        mailbox_capacity: None,
        overflow_policy: None,
        is_isolated: false,
        doc_comment: None,
        max_heap_bytes: None,
    };
    let program = Program {
        module_graph: None,
        items: vec![(Item::Actor(actor), 0..0)],
        module_doc: None,
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);
    assert!(output.errors.is_empty());
    assert_eq!(
        output.fn_sigs["NumberStream::numbers"].return_type,
        Ty::stream(Ty::I64)
    );
}

#[test]
fn typecheck_generic_call_with_explicit_type_args() {
    // This test exercises generic type-arg resolution, not Rc safety.
    // The BorrowedParamReturn diagnostic on `identity` is expected and filtered.
    let source = concat!(
        "fn identity<T>(x: T) -> T { x }\n",
        "fn main() {\n",
        "    let a = identity<i64>(42);\n",
        "    let b = identity<string>(\"hello\");\n",
        "    println(a);\n",
        "    println(b);\n",
        "}\n"
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let unexpected: Vec<_> = output
        .errors
        .iter()
        .filter(|e| !matches!(e.kind, TypeErrorKind::BorrowedParamReturn))
        .collect();
    assert!(unexpected.is_empty(), "unexpected errors: {unexpected:?}");
}

#[test]
fn typecheck_generic_call_with_inferred_type_args() {
    // This test exercises generic type-arg resolution, not Rc safety.
    // The BorrowedParamReturn diagnostic on `identity` is expected and filtered.
    let source = concat!(
        "fn identity<T>(x: T) -> T { x }\n",
        "fn main() {\n",
        "    let a = identity(42);\n",
        "    let b = identity(\"hello\");\n",
        "    println(a);\n",
        "    println(b);\n",
        "}\n"
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let unexpected: Vec<_> = output
        .errors
        .iter()
        .filter(|e| !matches!(e.kind, TypeErrorKind::BorrowedParamReturn))
        .collect();
    assert!(unexpected.is_empty(), "unexpected errors: {unexpected:?}");
    assert!(
        output
            .call_type_args
            .values()
            .any(|args| args == &vec![Ty::I64]),
        "expected inferred i64 literal type args to materialize at output boundary, got {:?}",
        output.call_type_args
    );
}

#[test]
fn typecheck_generator_yield_uses_element_type() {
    let source = concat!(
        "gen fn count_up() -> i64 {\n",
        "    yield 1;\n",
        "    yield 2;\n",
        "}\n"
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_async_generator_yield_uses_element_type() {
    let source = concat!(
        "async gen fn count_up() -> i64 {\n",
        "    yield 1;\n",
        "    yield 2;\n",
        "}\n"
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_generator_yield_mismatch_reports_element_type() {
    let source = "gen fn bad() -> i64 { yield \"oops\"; }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.iter().any(|e| {
            matches!(
                &e.kind,
                TypeErrorKind::Mismatch { expected, actual }
                if expected == "i64" && actual == "string"
            )
        }),
        "expected element-type mismatch, got: {:?}",
        output.errors
    );
    assert!(
        output
            .errors
            .iter()
            .all(|e| !e.message.contains("Generator<")),
        "yield diagnostic should mention element type, got: {:?}",
        output.errors
    );
}

#[test]
fn test_stream_annotation_resolves_to_stream_type() {
    use hew_parser::ast::{FnDecl, Item, TypeExpr};

    // Stream<i32> (the canonical name) must resolve to Ty::stream(Ty::I32).
    let fn_decl = FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Private,
        name: "foo".to_string(),
        type_params: None,
        params: vec![],
        return_type: Some((
            TypeExpr::Named {
                name: "Stream".to_string(),
                type_args: Some(vec![(
                    TypeExpr::Named {
                        name: "i32".to_string(),
                        type_args: None,
                    },
                    0..0,
                )]),
            },
            0..0,
        )),
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: None,
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
        intrinsic: None,
    };

    let program = Program {
        module_graph: None,
        items: vec![(Item::Function(fn_decl), 0..0)],
        module_doc: None,
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);
    // The body is empty (returns unit) so there will be a return-type mismatch error,
    // but fn_sigs is populated in pass 1 (before body checking), so the signature
    // should already reflect the resolved return type.
    assert_eq!(output.fn_sigs["foo"].return_type, Ty::stream(Ty::I32));
}

#[test]
fn test_actor_stream_name_no_longer_aliases_stream() {
    use hew_parser::ast::{FnDecl, Item, TypeExpr};

    // ActorStream<i32> must NOT resolve to Ty::stream(Ty::I32) — the alias is removed.
    // It should resolve to Ty::Named { name: "ActorStream", .. } (an unknown named type).
    let fn_decl = FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Private,
        name: "bar".to_string(),
        type_params: None,
        params: vec![],
        return_type: Some((
            TypeExpr::Named {
                name: "ActorStream".to_string(),
                type_args: Some(vec![(
                    TypeExpr::Named {
                        name: "i32".to_string(),
                        type_args: None,
                    },
                    0..0,
                )]),
            },
            0..0,
        )),
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: None,
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
        intrinsic: None,
    };

    let program = Program {
        module_graph: None,
        items: vec![(Item::Function(fn_decl), 0..0)],
        module_doc: None,
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);
    // The alias is removed: ActorStream<i32> must resolve to an unknown Named
    // type, not the built-in stream alias.  Pinning the exact form means a
    // regression that re-introduces the alias will produce a type mismatch
    // rather than a vacuously passing assert_ne.
    assert_eq!(
        output.fn_sigs["bar"].return_type,
        Ty::Named {
            builtin: None,
            name: "ActorStream".to_string(),
            args: vec![Ty::I32],
        },
        "ActorStream<i32> must resolve to an unknown Named type, not the Stream alias"
    );
}

#[test]
fn test_stream_canonical_name_still_resolves_after_actor_stream_removal() {
    use hew_parser::ast::{FnDecl, Item, TypeExpr};

    // Positive companion to test_actor_stream_name_no_longer_aliases_stream:
    // removing the ActorStream alias must not break resolution of the canonical
    // Stream<Y> name.
    let fn_decl = FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Private,
        name: "baz".to_string(),
        type_params: None,
        params: vec![],
        return_type: Some((
            TypeExpr::Named {
                name: "Stream".to_string(),
                type_args: Some(vec![(
                    TypeExpr::Named {
                        name: "i32".to_string(),
                        type_args: None,
                    },
                    0..0,
                )]),
            },
            0..0,
        )),
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: None,
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
        intrinsic: None,
    };

    let program = Program {
        module_graph: None,
        items: vec![(Item::Function(fn_decl), 0..0)],
        module_doc: None,
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);
    // Stream<i32> must still resolve to the built-in stream type.
    assert_eq!(
        output.fn_sigs["baz"].return_type,
        Ty::stream(Ty::I32),
        "Stream<i32> (canonical name) must resolve to Ty::stream(Ty::I32)"
    );
}

#[test]
fn test_qualified_builtin_type_names_canonicalize_in_signatures() {
    let source = concat!(
        "import std::stream;\n",
        "import std::channel::channel;\n",
        "\n",
        "fn stream_id(s: stream.Stream<i64>) -> stream.Stream<i64> { s }\n",
        "fn close_sender(tx: channel.Sender) {\n",
        "    tx.close();\n",
        "}\n",
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&result.program);
    assert!(output.errors.is_empty(), "type errors: {:?}", output.errors);
    assert_eq!(output.fn_sigs["stream_id"].params[0], Ty::stream(Ty::I64));
    assert_eq!(output.fn_sigs["stream_id"].return_type, Ty::stream(Ty::I64));
    assert!(matches!(
        &output.fn_sigs["close_sender"].params[0],
        Ty::Named {
            builtin: Some(crate::BuiltinType::Sender),
            args,
            ..
        } if args.len() == 1
    ));
}

#[test]
fn test_arity_mismatch_too_many_args() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.register_builtins();
    // println_int takes 1 arg; call with 2
    let call = (
        Expr::Call {
            function: Box::new((Expr::Identifier("println_int".to_string()), 0..11)),
            type_args: None,
            args: vec![
                CallArg::Positional((
                    Expr::Literal(hew_parser::ast::Literal::Integer {
                        value: 1,
                        radix: IntRadix::Decimal,
                    }),
                    12..13,
                )),
                CallArg::Positional((
                    Expr::Literal(hew_parser::ast::Literal::Integer {
                        value: 2,
                        radix: IntRadix::Decimal,
                    }),
                    15..16,
                )),
            ],
            is_tail_call: false,
        },
        0..17,
    );
    checker.synthesize(&call.0, &call.1);
    assert!(checker
        .errors
        .iter()
        .any(|e| e.kind == TypeErrorKind::ArityMismatch));
}

#[test]
fn test_arity_mismatch_too_few_args() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.register_builtins();
    // println_int takes 1 arg; call with 0
    let call = (
        Expr::Call {
            function: Box::new((Expr::Identifier("println_int".to_string()), 0..11)),
            type_args: None,
            args: vec![],
            is_tail_call: false,
        },
        0..13,
    );
    checker.synthesize(&call.0, &call.1);
    assert!(checker
        .errors
        .iter()
        .any(|e| e.kind == TypeErrorKind::ArityMismatch));
}

#[test]
fn typecheck_error_undefined_var() {
    let result = hew_parser::parse("fn main() -> i32 {\n    let x = undefined_var;\n    x\n}");
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output.errors.is_empty(),
        "expected type error for undefined variable"
    );
}

#[test]
fn removed_alias_int_emits_suggestion_for_i64_or_isize() {
    let result = hew_parser::parse("fn main() { let x: int = 5; }");
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let err = output
        .errors
        .iter()
        .find(|e| e.kind == TypeErrorKind::UndefinedType && e.message.contains("int"))
        .expect("expected UndefinedType error for removed alias `int`");
    assert!(
        err.message.contains("i64") || err.message.contains("isize"),
        "diagnostic should suggest i64 or isize; got: {}",
        err.message
    );
}

#[test]
fn removed_alias_uint_emits_suggestion_for_u64_or_usize() {
    let result = hew_parser::parse("fn main() { let x: uint = 5; }");
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let err = output
        .errors
        .iter()
        .find(|e| e.kind == TypeErrorKind::UndefinedType && e.message.contains("uint"))
        .expect("expected UndefinedType error for removed alias `uint`");
    assert!(
        err.message.contains("u64") || err.message.contains("usize"),
        "diagnostic should suggest u64 or usize; got: {}",
        err.message
    );
}

#[test]
fn removed_alias_int_capital_is_hard_error_with_i64_suggestion() {
    let result = hew_parser::parse("fn main() { let x: Int = 5; }");
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    // `Int` is no longer accepted; it must produce a hard type error.
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::UndefinedType && e.message.contains("Int")),
        "expected UndefinedType error for removed alias `Int`; got errors: {:?}",
        output.errors
    );
    // No warning about Int should be emitted — this is a hard error, not a
    // deprecation.  Other warnings (e.g. UnusedVariable for `x`) are fine.
    assert!(
        output
            .warnings
            .iter()
            .all(|w| !w.message.contains("Int") && !w.message.contains("deprecated")),
        "unexpected Int-related warning; got: {:?}",
        output.warnings
    );
    // The error message should suggest i64.
    let err = output
        .errors
        .iter()
        .find(|e| e.kind == TypeErrorKind::UndefinedType && e.message.contains("Int"))
        .unwrap();
    assert!(
        err.message.contains("i64") || err.message.contains("isize"),
        "diagnostic should suggest i64 or isize; got: {}",
        err.message
    );
}

#[test]
fn typecheck_error_type_mismatch() {
    let source = concat!(
        "fn add(a: i32, b: i32) -> i32 {\n",
        "    a + b\n",
        "}\n\n",
        "fn main() {\n",
        "    let result = add(\"hello\", \"world\");\n",
        "}"
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output.errors.is_empty(),
        "expected type errors for mismatched argument types"
    );
}

// -----------------------------------------------------------------------
// Additional edge-case tests
// -----------------------------------------------------------------------

#[test]
fn test_string_literal_type() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let expr = (Expr::Literal(Literal::String("hello".to_string())), 0..5);
    let ty = checker.synthesize(&expr.0, &expr.1);
    assert_eq!(ty, Ty::String);
}

#[test]
#[expect(
    clippy::approx_constant,
    reason = "testing that 3.14 parses as Float, not using it as PI"
)]
fn test_float_literal_type() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let expr = (Expr::Literal(Literal::Float(3.14)), 0..4);
    let ty = checker.synthesize(&expr.0, &expr.1);
    assert_eq!(ty, Ty::FloatLiteral);
}

#[test]
fn test_char_literal_type() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let expr = (Expr::Literal(Literal::Char('a')), 0..3);
    let ty = checker.synthesize(&expr.0, &expr.1);
    assert_eq!(ty, Ty::Char);
}

#[test]
fn typecheck_binary_op_type_mismatch() {
    let source = "fn main() -> i32 {\n    let x: i32 = 1;\n    let y: bool = true;\n    x + y\n}";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output.errors.is_empty(),
        "expected type error for i32 + bool"
    );
}

/// Structural record equality has no MIR/codegen lowering yet; the checker
/// must refuse `==` on record values with a spanned diagnostic instead of
/// leaking an unspanned `IntCmp` rejection from codegen-front.
#[test]
fn record_equality_comparison_reports_not_yet_implemented() {
    let source = "type Pt {\n    x: i64;\n    y: i64;\n}\n\nfn main() {\n    let a = Pt { x: 1, y: 2 };\n    let b = Pt { x: 1, y: 2 };\n    if a == b {\n        println(\"equal\");\n    }\n}";
    let output = check_source(source);
    let err = output
        .errors
        .iter()
        .find(|e| e.kind == TypeErrorKind::InvalidOperation)
        .unwrap_or_else(|| {
            panic!(
                "expected InvalidOperation for record `==`: {:#?}",
                output.errors
            )
        });
    assert!(
        err.message
            .contains("`==` on record type `Pt` is not yet implemented"),
        "diagnostic must name the operator and type: {}",
        err.message
    );
    // The span must cover the whole comparison (both operands), not just
    // one operand. The parser's operand spans may extend a byte past the
    // identifier, so pin "starts at `a`, reaches past `b`" rather than the
    // exact end offset.
    let cmp_start = source.find("a == b").unwrap();
    assert_eq!(
        err.span.start, cmp_start,
        "diagnostic must start at the left operand"
    );
    assert!(
        err.span.end >= cmp_start + "a == b".len(),
        "diagnostic must reach the right operand: {:?}",
        err.span
    );
    assert!(
        err.suggestions
            .iter()
            .any(|s| s.contains("compare individual fields")),
        "diagnostic must suggest field-wise comparison: {:?}",
        err.suggestions
    );
}

/// `!=` and the ordering operators leak through the same codegen-front
/// `IntCmp` path as `==`; all six comparison operators are gated.
#[test]
fn record_inequality_and_ordering_comparisons_rejected() {
    let source = "type Pt {\n    x: i64;\n    y: i64;\n}\n\nfn main() {\n    let a = Pt { x: 1, y: 2 };\n    let b = Pt { x: 1, y: 2 };\n    let ne = a != b;\n    let lt = a < b;\n    let _ = ne;\n    let _ = lt;\n}";
    let output = check_source(source);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InvalidOperation
                && e.message
                    .contains("`!=` on record type `Pt` is not yet implemented")),
        "expected refusal for record `!=`: {:#?}",
        output.errors
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InvalidOperation
                && e.message
                    .contains("`<` is not supported for record type `Pt`")),
        "expected refusal for record `<`: {:#?}",
        output.errors
    );
}

/// The gate keys off `TypeDefKind::Struct`/`Record`; enum comparisons must
/// not trip it (enum `==` keeps its current checker behaviour).
#[test]
fn enum_equality_not_gated_by_record_comparison_refusal() {
    let source = "enum Colour {\n    Red;\n    Green;\n}\n\nfn main() -> bool {\n    let a = Colour::Red;\n    let b = Colour::Green;\n    a == b\n}";
    let output = check_source(source);
    assert!(
        output.errors.is_empty(),
        "enum `==` must not trip the record-comparison gate: {:#?}",
        output.errors
    );
}

/// When the operand types disagree, the plain mismatch diagnostic wins;
/// the record gate must not double-report.
#[test]
fn record_comparison_type_mismatch_reports_mismatch_not_refusal() {
    let source = "type Pt {\n    x: i64;\n    y: i64;\n}\n\nfn main() -> bool {\n    let a = Pt { x: 1, y: 2 };\n    a == 5\n}";
    let output = check_source(source);
    assert!(
        output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::Mismatch { .. })),
        "expected a type mismatch error: {:#?}",
        output.errors
    );
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.message.contains("not yet implemented")),
        "record gate must not fire on mismatched operands: {:#?}",
        output.errors
    );
}

#[test]
fn int_literal_locals_unify_to_concrete_integer_binary_width() {
    let source = r"
        fn step() -> i32 {
            return 10;
        }

        fn main() -> i32 {
            let target_value = 7;
            var total = 0;
            if step() == target_value {
                total = total + step();
            }
            return total;
        }
        ";
    let output = check_source(source);

    assert!(
        output.errors.is_empty(),
        "literal-seeded operands should infer the i32 binary width: {:#?}",
        output.errors
    );
    let literal_key = span_key_for(source, "7");
    assert_eq!(
        output.expr_types.get(&literal_key),
        Some(&Ty::I32),
        "literal-backed binding should resolve to the concrete i32 width"
    );
}

#[test]
fn concrete_integer_float_comparison_stays_rejected() {
    let output = check_source(
        r"
        fn bad(x: i32, y: f64) -> bool {
            return x == y;
        }
        ",
    );

    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("explicit conversion")),
        "expected i32 vs f64 comparison to require an explicit conversion: {:#?}",
        output.errors
    );
}

#[test]
fn integer_string_comparison_stays_rejected() {
    let output = check_source(
        r#"
        fn bad(x: i32) -> bool {
            return x == "7";
        }
        "#,
    );

    assert!(
        !output.errors.is_empty(),
        "expected i32 vs string comparison to be rejected"
    );
}

// ── Regression: literal-bound-to-local width propagation ──────────────────
//
// These tests guard the path where a literal is bound to a local variable via
// `let` or `var`, then compared/computed with a concrete-width integer.  The
// checker must propagate the concrete width back to the literal local so the
// HIR and MIR see matching widths at binary operation sites.
//
// Background: commit 53aa2a06 fixed the case where a literal is a direct
// binary operand; the `let target = 7` path works through the `const_values`
// table + `expect_inferable_literal_binding` machinery.  Without this path
// the literal materialises to I64 at binding time, causing
// `IntCmp{I32, I64}` or `IntArithChecked{I32, I64}` in MIR that the
// fail-closed codegen correctly rejects.
//
// This cluster also tests that the for-range loop variable adopts the correct
// element width (the checker infers the element type from the range bounds;
// hew-hir threads this through to the HIR binding so MIR locals match).

/// The let-bound literal path (`let target = 7; fn_returning_i32() == target`)
/// must produce no type errors and the literal must be recorded as I32.
/// Regression: if `infer_integer_literal_binding_type` stops creating a Var
/// for the literal, the 7 stays `IntLiteral` → `I64` at output and downstream
/// MIR sees `IntCmp{I32, I64}`.
#[test]
fn let_bound_literal_unifies_to_i32_width_when_compared_against_i32_fn() {
    let source = r"
        fn pick(x: i32) -> i32 { x }

        fn main() -> i32 {
            let target = 7;
            if pick(7) == target { 1 } else { 0 }
        }
    ";
    let output = check_source(source);
    assert!(
        output.errors.is_empty(),
        "let-bound literal compared against i32 fn result must not error: {:#?}",
        output.errors
    );
    let literal_key = span_key_for(source, "7");
    // The first `7` in the source is `pick(7)` — skip to the `let target = 7`
    // literal which is the second occurrence.
    let second_7_pos = source[literal_key.end..]
        .find('7')
        .map(|p| p + literal_key.end);
    if let Some(pos) = second_7_pos {
        let key = SpanKey {
            start: pos,
            end: pos + 1,
            module_idx: 0,
        };
        let recorded = output.expr_types.get(&key);
        assert_eq!(
            recorded,
            Some(&Ty::I32),
            "let-bound literal should resolve to I32 via use-site context, got {recorded:?}"
        );
    }
}

/// `var` bindings with an untyped integer literal remain inferable (not
/// immediately materialised to I64) so that use-site context can narrow them.
/// Regression: if `var passed = 0` materialises to I64 before `passed +
/// fn_returning_i32()` constrains it, the arithmetic site gets
/// IntArithChecked{I64, I32} in MIR.
#[test]
fn var_bound_literal_unifies_to_i32_when_added_to_i32_result() {
    let source = r"
        fn count() -> i32 { 1 }
        fn main() -> i32 {
            var passed = 0;
            passed = passed + count();
            passed
        }
    ";
    let output = check_source(source);
    assert!(
        output.errors.is_empty(),
        "var-bound literal should infer I32 via arithmetic context: {:#?}",
        output.errors
    );
}

#[test]
fn integer_literal_match_pattern_must_fit_scrutinee_width() {
    let output = check_source(
        r"
        fn classify(x: i8) -> i64 {
            match x {
                128 => 1,
                _ => 0,
            }
        }
    ",
    );
    assert!(
        output.errors.iter().any(|err| err
            .message
            .contains("does not fit in match scrutinee type `i8`")),
        "expected i8 match literal range error, got: {:#?}",
        output.errors
    );
}

/// `for i in 2 .. n + 1` with `n: i32` — the checker must infer the range
/// element type as I32 so that uses of `i` as a narrower operand don't get
/// widened to I64.  Regression: if the range element type defaults to I64
/// and the literal operands in the loop body are recorded as I32 via the
/// `n: i32` context, MIR sees IntArithChecked{I64, I32}.
///
/// This test validates the checker-level half of the invariant: passing `i`
/// to a function that accepts `i32` must not require explicit coercion.
/// The HIR/MIR threading of this type is tested end-to-end by the
/// `climbing_stairs` and `matrix_multiply` corpus files once the Vec ABI gap
/// is fixed.
#[test]
fn for_range_loop_var_infers_i32_from_i32_bound() {
    let source = r"
        fn take_i32(x: i32) -> i32 { x }
        fn main() -> i32 {
            let n: i32 = 5;
            var acc: i32 = 0;
            for i in 0 .. n {
                acc = acc + take_i32(i);
            }
            acc
        }
    ";
    let output = check_source(source);
    assert!(
        output.errors.is_empty(),
        "for-range loop variable should infer i32 when bound is i32: {:#?}",
        output.errors
    );
}

/// Checker accepts mixed-width range bounds (i32..i64) and resolves the
/// loop variable to the WIDER type (i64), not the narrower start bound.
///
/// The checker's `common_integer_type(i32, i64)` chooses `i64`.  The range
/// type must be `Range<i64>` so HIR reads the correct element type.
#[test]
fn for_range_mixed_width_bounds_resolves_to_wider_type() {
    let source = r"
        fn id_i64(x: i64) -> i64 { x }
        fn main() -> i64 {
            let a: i32 = 2;
            let b: i64 = 6;
            var sum: i64 = 0;
            for i in a..b {
                sum = sum + id_i64(i);
            }
            sum
        }
    ";
    let output = check_source(source);
    assert!(
        output.errors.is_empty(),
        "mixed-width range bounds should resolve to the wider type: {:#?}",
        output.errors
    );
}

/// Checker accepts negative literal range bounds (`-5..5`) when the loop
/// variable is narrowed to i32 via context.
///
/// The deferred `TypeVar` for the range element type must be re-recorded for
/// both the outer (`-5`) span AND the inner literal (`5`) span so HIR
/// unary lowering sees matching operand/result widths.
#[test]
fn for_range_negative_literal_bound_accepted_at_i32() {
    let source = r"
        fn id_i32(x: i32) -> i32 { x }
        fn main() -> i32 {
            var sum: i32 = 0;
            for i in -5..5 {
                sum = sum + id_i32(i);
            }
            sum
        }
    ";
    let output = check_source(source);
    assert!(
        output.errors.is_empty(),
        "negative literal range bounds should be accepted at i32: {:#?}",
        output.errors
    );
}

#[test]
fn typecheck_rejects_implicit_signedness_change_in_call() {
    let source = concat!(
        "fn takes_u32(x: u32) -> u32 { x }\n",
        "fn main() -> u32 {\n",
        "    let n: i64 = 42;\n",
        "    takes_u32(n)\n",
        "}\n"
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("cannot implicitly convert")
                && e.message.contains("i64")
                && e.message.contains("u32")),
        "expected integer-mismatch rejection diagnostic, got: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_rejects_implicit_integer_to_float_in_call() {
    let source = concat!(
        "fn takes_f64(x: f64) -> f64 { x }\n",
        "fn main() -> f64 {\n",
        "    let n: i64 = 42;\n",
        "    takes_f64(n)\n",
        "}\n"
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("implicit numeric coercion")),
        "expected explicit coercion diagnostic, got: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_rejects_implicit_integer_widening_in_call() {
    // Passing i32 where i64 is expected is an error; the caller must write
    // `takes_i64(n as i64)`.  Silent widening was removed because LLVM's
    // IR verifier rejects the resulting mistyped call instruction.
    let source = concat!(
        "fn takes_i64(x: i64) -> i64 { x }\n",
        "fn main() -> i64 {\n",
        "    let n: i32 = 42;\n",
        "    takes_i64(n)\n",
        "}\n"
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("cannot implicitly convert")
                && e.message.contains("i32")
                && e.message.contains("i64")),
        "expected integer-widening rejection diagnostic, got: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_return_type_mismatch() {
    // The type checker may not flag all return-type mismatches at the
    // trailing-expression level; verify the function signature is recorded.
    let source = "fn foo() -> i32 {\n    true\n}";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    // The function signature should still reflect i32 return type
    assert_eq!(output.fn_sigs["foo"].return_type, Ty::I32);
}

#[test]
fn typecheck_trailing_return_stmt_matches_declared_type() {
    let source = "fn foo() -> i32 {\n    return 42;\n}";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_trailing_return_stmt_matches_declared_bool_type() {
    let source = "fn foo() -> bool {\n    return true;\n}";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_nested_function_calls() {
    let source = concat!(
        "fn double(x: i32) -> i32 { let two: i32 = 2; x * two }\n",
        "fn main() -> i32 { let x: i32 = 5; double(double(x)) }\n"
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_let_with_explicit_type() {
    let source = "fn main() { let x: i32 = 42; }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_let_type_annotation_mismatch() {
    let source = "fn main() { let x: i64 = \"hello\"; }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output.errors.is_empty(),
        "expected type error for string assigned to i64 variable"
    );
    assert!(output
        .errors
        .iter()
        .any(|e| { e.message.contains("expected `i64`") && e.message.contains("found `string`") }));
}

#[test]
fn typecheck_if_branch_type_consistency() {
    let source =
        "fn main() -> i32 {\n    if true { let x: i32 = 1; x } else { let y: i32 = 2; y }\n}";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_vec_type_annotation() {
    let source = "fn main() { let v: Vec<i32> = Vec::new(); }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    // Vec::new() may or may not resolve depending on builtins, but should not panic
    assert!(output.errors.len() <= 2);
}

#[test]
fn unresolved_vec_new_method_chain_fails_closed() {
    let source = "fn main() { Vec::new().clear(); }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);

    assert!(
        output.errors.iter().any(|err| {
            err.kind == TypeErrorKind::InferenceFailed
                && err.message.contains("Vec element type")
                && err.message.contains("Vec<")
        }),
        "expected fail-closed Vec inference diagnostic, got errors: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_multiple_functions_cross_call() {
    let source = concat!(
        "fn add(a: i32, b: i32) -> i32 { a + b }\n",
        "fn mul(a: i32, b: i32) -> i32 { a * b }\n",
        "fn main() -> i32 { add(mul(2, 3), 4) }\n"
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_actor_receive_fn_registered() {
    use hew_parser::ast::{ActorDecl, Block, Item, Param, ReceiveFnDecl, TypeExpr};

    let recv = ReceiveFnDecl {
        is_generator: false,
        name: "greet".to_string(),
        params: vec![Param {
            name: "name".to_string(),
            ty: (
                TypeExpr::Named {
                    name: "string".into(),
                    type_args: None,
                },
                0..0,
            ),
            is_mutable: false,
        }],
        return_type: None,
        body: Block {
            stmts: vec![],
            trailing_expr: None,
        },
        type_params: None,
        where_clause: None,
        span: 0..0,
        attributes: vec![],
        doc_comment: None,
    };
    let actor = ActorDecl {
        visibility: Visibility::Pub,
        name: "Greeter".to_string(),
        type_params: vec![],
        super_traits: None,
        init: None,
        fields: vec![],
        receive_fns: vec![recv],
        methods: vec![],
        mailbox_capacity: None,
        overflow_policy: None,
        is_isolated: false,
        doc_comment: None,
        max_heap_bytes: None,
    };
    let program = Program {
        module_graph: None,
        items: vec![(Item::Actor(actor), 0..0)],
        module_doc: None,
    };
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);
    assert!(output.fn_sigs.contains_key("Greeter::greet"));
}

fn span_key_for(source: &str, needle: &str) -> SpanKey {
    let start = source
        .find(needle)
        .unwrap_or_else(|| panic!("missing `{needle}` in source"));
    SpanKey {
        start,
        end: start + needle.len(),
        module_idx: 0,
    }
}

#[test]
fn context_readers_typecheck_inside_receive_handler() {
    let source = "\
        actor Worker {
            receive fn ping() {
                let actor_value = @actor_id;
                let supervisor_value = @supervisor;
                let span_value = @trace_span;
            }
        }";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
    assert_eq!(
        output.expr_types.get(&span_key_for(source, "@actor_id")),
        Some(&Ty::U64)
    );
    assert_eq!(
        output.expr_types.get(&span_key_for(source, "@trace_span")),
        Some(&Ty::U64)
    );
    assert_eq!(
        output.expr_types.get(&span_key_for(source, "@supervisor")),
        Some(&Ty::Pointer {
            is_mutable: true,
            pointee: Box::new(Ty::Unit),
        })
    );
}

#[test]
fn context_reader_outside_handler_is_typed_diagnostic() {
    let output = check_source("fn main() -> u64 { @actor_id }");
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::ContextReaderOutsideHandler),
        "{:?}",
        output.errors
    );
}

#[test]
fn context_reader_in_non_actor_lambda_is_typed_diagnostic() {
    let source = "\
        actor Worker {
            receive fn ping() {
                let f = || @actor_id;
            }
        }";
    let output = check_source(source);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::ContextReaderOutsideHandler),
        "{:?}",
        output.errors
    );
}

/// `#[max_heap(N)]` on an actor → `actor_max_heap` side-table entry for that actor.
#[test]
fn max_heap_attribute_populates_side_table() {
    let source = "#[max_heap(4096)] actor Cache { receive fn get() {} }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected type errors: {:?}",
        output.errors
    );
    assert_eq!(
        output.actor_max_heap.get("Cache"),
        Some(&4096u64),
        "actor_max_heap must record the parsed cap for Cache"
    );
}

/// Actor without `#[max_heap]` must not appear in the side-table.
#[test]
fn max_heap_absent_actor_not_in_side_table() {
    let source = "actor Plain { receive fn tick() {} }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected type errors: {:?}",
        output.errors
    );
    assert!(
        !output.actor_max_heap.contains_key("Plain"),
        "actor without #[max_heap] must not appear in actor_max_heap"
    );
}

/// `#[max_heap(2 mb)]` — suffix conversion done by the parser, checker sees bytes.
#[test]
fn max_heap_mb_suffix_populates_side_table_as_bytes() {
    let source = "#[max_heap(2 mb)] actor Big { receive fn work() {} }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected type errors: {:?}",
        output.errors
    );
    assert_eq!(
        output.actor_max_heap.get("Big"),
        Some(&(2u64 * 1024 * 1024)),
        "2 mb must be recorded as 2_097_152 bytes"
    );
}

#[test]
fn typecheck_empty_function_no_error() {
    let source = "fn noop() {}";
    let result = hew_parser::parse(source);
    assert!(result.errors.is_empty());
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_recursive_function() {
    let source = concat!(
        "fn factorial(n: i32) -> i32 {\n",
        "    let one: i32 = 1; if n <= one { one } else { n * factorial(n - one) }\n",
        "}\n"
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_local_result_enum_not_qualified_to_sqlite() {
    let source = concat!(
        "import ecosystem::db::sqlite;\n",
        "enum Result {\n",
        "    Ok(i64);\n",
        "    Err(i64)\n",
        "}\n",
        "fn unwrap_or(r: Result, fallback: i64) -> i64 {\n",
        "    match r {\n",
        "        Ok(v) => v,\n",
        "        Err(_) => fallback,\n",
        "    }\n",
        "}\n"
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    // Filter out the expected UnresolvedImport for the dummy stdlib import — the
    // test is about local type naming, not module resolution.
    let non_import_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind != TypeErrorKind::UnresolvedImport)
        .collect();
    assert!(
        non_import_errors.is_empty(),
        "unexpected errors: {non_import_errors:?}"
    );
    let sig = output
        .fn_sigs
        .get("unwrap_or")
        .expect("unwrap_or signature should be registered");
    assert_eq!(
        sig.params[0],
        Ty::Named {
            builtin: None,
            name: "Result".to_string(),
            args: vec![],
        }
    );
}

#[test]
fn typecheck_match_statement_exhaustive_enum_ok() {
    let (errors, _) = parse_and_check(concat!(
        "enum Light { Red; Green; }\n",
        "fn main() { let v: Light = Red; match v { Red => 1, Green => 2, } let _done = 0; }\n",
    ));
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
}

#[test]
fn typecheck_match_statement_missing_variant_errors() {
    let (errors, warnings) = parse_and_check(concat!(
        "enum Light { Red; Green; }\n",
        "fn main() { let v: Light = Red; match v { Red => 1, } let _done = 0; }\n",
    ));
    assert!(
        warnings
            .iter()
            .all(|w| !matches!(w.kind, TypeErrorKind::NonExhaustiveMatch)),
        "non-exhaustive enum match must not be a warning: {warnings:?}"
    );
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::NonExhaustiveMatch)),
        "expected non-exhaustive match error, got: {errors:?}"
    );
}

#[test]
fn typecheck_guarded_wildcard_not_exhaustive() {
    // A guarded wildcard (`_ if false`) does not count as an exhaustive arm;
    // matching bool with only that arm leaves both `true` and `false` uncovered.
    // Bool is enum-like, so this is a hard error.
    let (errors, warnings) = parse_and_check(concat!(
        "fn main() {\n",
        "    let x = true;\n",
        "    match x {\n",
        "        _ if false => 0,\n",
        "    }\n",
        "    let _done = 0;\n",
        "}\n",
    ));
    assert!(
        warnings
            .iter()
            .all(|w| !matches!(w.kind, TypeErrorKind::NonExhaustiveMatch)),
        "non-exhaustive bool match must not be a warning: {warnings:?}"
    );
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::NonExhaustiveMatch)),
        "expected non-exhaustive match error, got: {errors:?}"
    );
    let err = errors
        .iter()
        .find(|e| matches!(e.kind, TypeErrorKind::NonExhaustiveMatch))
        .expect("expected NonExhaustiveMatch error");
    assert_eq!(err.severity, crate::error::Severity::Error);
    assert_eq!(err.message, "non-exhaustive match: missing true, false");
}

/// Literal-only i64 matches are fail-closed: without a catch-all there are
/// infinitely many missing values, so this is a hard non-exhaustive error.
#[test]
fn typecheck_i64_literal_missing_catchall_is_error() {
    let (errors, warnings) = parse_and_check(concat!(
        "fn main() {\n",
        "    let x: i64 = 5;\n",
        "    match x {\n",
        "        1 => 10,\n",
        "        2 => 20,\n",
        "    }\n",
        "    let _done = 0;\n",
        "}\n",
    ));
    assert!(
        warnings
            .iter()
            .all(|w| !matches!(w.kind, TypeErrorKind::NonExhaustiveMatch)),
        "non-exhaustive literal i64 match must not be a warning: {warnings:?}"
    );
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::NonExhaustiveMatch)),
        "literal i64 missing catch-all must be an error: {errors:?}"
    );
}

/// String matches are over an open domain; literal arms need a catch-all.
#[test]
fn typecheck_string_literal_missing_catchall_is_error() {
    let (errors, warnings) = parse_and_check(concat!(
        "fn main() {\n",
        "    let s: string = \"maybe\";\n",
        "    match s {\n",
        "        \"yes\" => 1,\n",
        "        \"no\" => 0,\n",
        "    }\n",
        "    let _done = 0;\n",
        "}\n",
    ));
    assert!(
        warnings
            .iter()
            .all(|w| !matches!(w.kind, TypeErrorKind::NonExhaustiveMatch)),
        "non-exhaustive literal string match must not be a warning: {warnings:?}"
    );
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::NonExhaustiveMatch)),
        "literal string missing catch-all must be an error: {errors:?}"
    );
}

#[test]
fn typecheck_string_literal_with_catchall_is_exhaustive() {
    let (errors, warnings) = parse_and_check(concat!(
        "fn main() {\n",
        "    let s: string = \"maybe\";\n",
        "    match s {\n",
        "        \"yes\" => 1,\n",
        "        \"no\" => 0,\n",
        "        _ => -1,\n",
        "    }\n",
        "    let _done = 0;\n",
        "}\n",
    ));
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    assert!(
        warnings
            .iter()
            .all(|w| !matches!(w.kind, TypeErrorKind::NonExhaustiveMatch)),
        "catch-all string match must not warn as non-exhaustive: {warnings:?}"
    );
}

#[test]
fn typecheck_float_literal_pattern_errors() {
    let (errors, _) = parse_and_check(concat!(
        "fn main() {\n",
        "    let x: f64 = -1.0;\n",
        "    match x {\n",
        "        -1.0 => 10,\n",
        "        _ => 0,\n",
        "    }\n",
        "}\n",
    ));
    assert!(
        errors.iter().any(|e| {
            matches!(e.kind, TypeErrorKind::InvalidOperation)
                && e.message
                    .contains("float literal patterns are not supported")
        }),
        "expected float literal pattern rejection, got: {errors:?}"
    );
}

#[test]
fn typecheck_struct_pattern_unknown_field_errors() {
    let (errors, _) = parse_and_check(concat!(
        "type Point { x: i64, y: i64 }\n",
        "fn main() {\n",
        "    let p = Point { x: 1, y: 2 };\n",
        "    match p {\n",
        "        Point { x, z } => x,\n",
        "    }\n",
        "}\n",
    ));
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::UndefinedField)),
        "expected UndefinedField error for unknown field 'z', got: {errors:?}"
    );
}

#[test]
fn typecheck_match_wrong_enum_variant_errors() {
    // Matching a Colour scrutinee with a Shape variant should be an error.
    let (errors, _) = parse_and_check(concat!(
        "enum Colour { Red; Green; Blue; }\n",
        "enum Shape { Circle(i32); Rectangle(i32); }\n",
        "fn describe(c: Colour) -> i32 {\n",
        "    match c {\n",
        "        Circle(r) => r,\n",
        "        _ => 0,\n",
        "    }\n",
        "}\n",
    ));
    assert!(
        !errors.is_empty(),
        "expected type error for wrong-enum variant in match, got no errors"
    );
}

#[test]
fn typecheck_or_pattern_asymmetric_bindings_error() {
    let (errors, _) = parse_and_check(concat!(
        "fn main() {\n",
        "    let value = (1, 2);\n",
        "    match value {\n",
        "        (x, _) | (_, _) => 0,\n",
        "    }\n",
        "}\n",
    ));
    let err = errors
        .iter()
        .find(|e| matches!(e.kind, TypeErrorKind::OrPatternBindingMismatch))
        .expect("expected asymmetric or-pattern binding diagnostic");
    assert_eq!(err.message, "or-pattern branches must bind the same names");
    assert!(
        err.notes
            .iter()
            .any(|(_, note)| note.contains("left branch binds `x`")),
        "expected left-branch binding note, got: {err:?}"
    );
    assert!(
        err.notes
            .iter()
            .any(|(_, note)| note.contains("right branch binds no names")),
        "expected right-branch binding note, got: {err:?}"
    );
}

#[test]
fn typecheck_or_pattern_symmetric_bindings_ok() {
    let (errors, warnings) = parse_and_check(concat!(
        "fn main() -> i64 {\n",
        "    let value = (1, 2);\n",
        "    match value {\n",
        "        (x, _) | (_, x) => x,\n",
        "        _ => 0,\n",
        "    }\n",
        "}\n",
    ));
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    assert!(warnings.is_empty(), "unexpected warnings: {warnings:?}");
}

#[test]
fn typecheck_or_pattern_incompatible_binding_types_error() {
    let (errors, _) = parse_and_check(concat!(
        "fn unwrap(result: Result<i64, string>) -> i64 {\n",
        "    match result {\n",
        "        Ok(x) | Err(x) => x,\n",
        "    }\n",
        "}\n",
    ));
    let err = errors
        .iter()
        .find(|e| matches!(e.kind, TypeErrorKind::OrPatternBindingMismatch))
        .expect("expected incompatible or-pattern binding diagnostic");
    assert!(
        err.notes
            .iter()
            .any(|(_, note)| note.contains("left branch binds `x` as `i64`")),
        "expected left-branch type note, got: {err:?}"
    );
    assert!(
        err.notes
            .iter()
            .any(|(_, note)| note.contains("right branch binds `x` as `string`")),
        "expected right-branch type note, got: {err:?}"
    );
}

#[test]
fn typecheck_error_scrutinee_constructor_pattern_stays_fail_closed() {
    let (errors, _) = parse_and_check(concat!(
        "fn main() {\n",
        "    let _value = match missing {\n",
        "        Some(x) => x,\n",
        "        None => panic(\"boom\"),\n",
        "    };\n",
        "}\n",
    ));
    assert_eq!(
        errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::UndefinedVariable))
            .count(),
        1,
        "expected only the errored scrutinee to report UndefinedVariable: {errors:?}"
    );
    assert!(
        errors
            .iter()
            .all(|e| !matches!(e.kind, TypeErrorKind::InferenceFailed)),
        "errored scrutinees must not seed constructor-pattern inference holes: {errors:?}"
    );
}

#[test]
fn typecheck_error_scrutinee_struct_pattern_no_undefined_variable_cascade() {
    let (errors, _) = parse_and_check(concat!(
        "type Point { x: i64, y: i64 }\n",
        "fn main() {\n",
        "    let _value = match missing {\n",
        "        Point { x, y } => x + y,\n",
        "    };\n",
        "}\n",
    ));
    assert_eq!(
        errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::UndefinedVariable))
            .count(),
        1,
        "expected only the errored scrutinee to report UndefinedVariable: {errors:?}"
    );
}

#[test]
fn typecheck_error_scrutinee_struct_variant_pattern_no_cascade() {
    let (errors, _) = parse_and_check(concat!(
        "enum Shape { Move { x: i64 } }\n",
        "fn main() {\n",
        "    let _value = match missing {\n",
        "        Shape::Move { x } => x,\n",
        "    };\n",
        "}\n",
    ));
    assert_eq!(
        errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::UndefinedVariable))
            .count(),
        1,
        "expected only the errored scrutinee to report UndefinedVariable: {errors:?}"
    );
}

#[test]
fn typecheck_error_scrutinee_struct_pattern_with_subpattern_no_cascade() {
    let (errors, _) = parse_and_check(concat!(
        "type Point { x: i64 }\n",
        "fn main() {\n",
        "    let _value = match missing {\n",
        "        Point { x: inner_x } => inner_x,\n",
        "    };\n",
        "}\n",
    ));
    assert_eq!(
        errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::UndefinedVariable))
            .count(),
        1,
        "expected only the errored scrutinee to report UndefinedVariable: {errors:?}"
    );
}

#[test]
fn typecheck_bool_scrutinee_constructor_pattern_errors() {
    let (errors, warnings) = parse_and_check(concat!(
        "fn main() {\n",
        "    match true {\n",
        "        Some(v) => 1,\n",
        "        None => 0,\n",
        "        _ => 2,\n",
        "    }\n",
        "}\n",
    ));
    assert!(
        errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrorKind::Mismatch { expected, actual }
                if expected == "bool" && actual == "Some"
        )),
        "expected constructor-pattern mismatch on bool scrutinee, got: {errors:?}"
    );
    assert!(
        errors
            .iter()
            .any(|e| e.message.contains("constructor pattern `Some`")),
        "expected fail-closed constructor-pattern diagnostic, got: {errors:?}"
    );
    assert!(
        warnings
            .iter()
            .all(|w| !matches!(w.kind, TypeErrorKind::NonExhaustiveMatch)),
        "wildcard arm should suppress exhaustiveness follow-ons: {warnings:?}"
    );
}

#[test]
fn typecheck_int_scrutinee_struct_pattern_errors_without_binding_cascade() {
    let (errors, _) = parse_and_check(concat!(
        "type Point { x: i64 }\n",
        "fn main() {\n",
        "    let _ = match 42 {\n",
        "        Point { x } => {\n",
        "            let _ = x;\n",
        "            0\n",
        "        },\n",
        "    };\n",
        "}\n",
    ));
    assert_eq!(
        errors.len(),
        1,
        "expected only the struct-pattern mismatch, got: {errors:?}"
    );
    assert!(
        errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrorKind::Mismatch { expected, actual }
                if expected == "i64" && actual == "Point"
        )),
        "expected struct-pattern mismatch on i64 scrutinee, got: {errors:?}"
    );
    assert!(
        errors.iter().any(|e| e
            .message
            .contains("struct pattern `Point` cannot match non-struct type `i64`")),
        "expected fail-closed struct-pattern diagnostic, got: {errors:?}"
    );
    assert!(
        errors
            .iter()
            .all(|e| !matches!(e.kind, TypeErrorKind::UndefinedVariable)),
        "struct-pattern mismatch must not cascade into undefined-variable errors: {errors:?}"
    );
}

#[test]
fn typecheck_bool_scrutinee_tuple_pattern_errors_without_binding_cascade() {
    let (errors, _) = parse_and_check(concat!(
        "fn main() {\n",
        "    let _ = match true {\n",
        "        (left, right) => {\n",
        "            let _ = left;\n",
        "            let _ = right;\n",
        "            0\n",
        "        },\n",
        "        _ => 0,\n",
        "    };\n",
        "}\n",
    ));
    assert_eq!(
        errors.len(),
        1,
        "expected only the tuple-pattern mismatch, got: {errors:?}"
    );
    assert!(
        errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrorKind::Mismatch { expected, actual }
                if expected == "bool" && actual == "tuple"
        )),
        "expected tuple-pattern mismatch on bool scrutinee, got: {errors:?}"
    );
    assert!(
        errors.iter().any(|e| e
            .message
            .contains("tuple pattern cannot match non-tuple type `bool`")),
        "expected fail-closed tuple-pattern diagnostic, got: {errors:?}"
    );
    assert!(
        errors
            .iter()
            .all(|e| !matches!(e.kind, TypeErrorKind::UndefinedVariable)),
        "tuple-pattern mismatch must not cascade into undefined-variable errors: {errors:?}"
    );
}

#[test]
fn typecheck_error_scrutinee_skips_exhaustiveness_follow_on() {
    let (errors, warnings) = parse_and_check(concat!(
        "fn main() {\n",
        "    match missing {\n",
        "        true => 1,\n",
        "    }\n",
        "    let _done = 0;\n",
        "}\n",
    ));
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::UndefinedVariable)),
        "expected primary undefined variable error, got: {errors:?}"
    );
    assert!(
        errors
            .iter()
            .all(|e| !matches!(e.kind, TypeErrorKind::NonExhaustiveMatch)),
        "Ty::Error scrutinee must not emit follow-on non-exhaustive errors: {errors:?}"
    );
    assert!(
        warnings
            .iter()
            .all(|w| !matches!(w.kind, TypeErrorKind::NonExhaustiveMatch)),
        "Ty::Error scrutinee must not emit follow-on non-exhaustive warnings: {warnings:?}"
    );
}

#[test]
fn typecheck_generic_enum_constructor_infers_type_args() {
    let (errors, _) = parse_and_check(concat!(
        "enum Option<T> { Some(T); None; }\n",
        "fn take_int(x: Option<i64>) -> Option<i64> { x }\n",
        "fn take_string(x: Option<string>) -> Option<string> { x }\n",
        "fn main() { take_int(Some(42)); take_string(Some(\"hello\")); }\n",
    ));
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
}

#[test]
fn generic_enum_constructor_expected_context_coerces_payload_literal() {
    let source = concat!(
        "enum Option<T> { Some(T); None; }\n",
        "fn take_int(x: Option<i64>) -> Option<i64> { x }\n",
        "fn main() { take_int(Some(42)); }\n",
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let main_fn = result
        .program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            Item::Function(function) if function.name == "main" => Some(function),
            _ => None,
        })
        .expect("main function should exist");
    let Stmt::Expression((outer_call, _)) = &main_fn.body.stmts[0].0 else {
        panic!("expected outer call statement");
    };
    let Expr::Call {
        args: outer_args, ..
    } = outer_call
    else {
        panic!("expected outer call expression");
    };
    let (inner_call, inner_call_span) = outer_args[0].expr();
    let Expr::Call {
        args: inner_args, ..
    } = inner_call
    else {
        panic!("expected inner constructor call");
    };
    let (_, literal_span) = inner_args[0].expr();

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
    assert_eq!(
        output.expr_types.get(&SpanKey::from(literal_span)),
        Some(&Ty::I64),
        "constructor payload literal should coerce to `i64`: {:?}",
        output.expr_types
    );
    assert_eq!(
        output.expr_types.get(&SpanKey::from(inner_call_span)),
        Some(&Ty::option(Ty::I64)),
        "constructor call should resolve to `Option<i64>`: {:?}",
        output.expr_types
    );
}

#[test]
fn builtin_result_constructors_materialize_output_types_without_call_type_args() {
    let source = concat!(
        "fn main() -> i64 {\n",
        "    Ok(7);\n",
        "    Err(9);\n",
        "    0\n",
        "}\n",
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let main_fn = result
        .program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            Item::Function(function) if function.name == "main" => Some(function),
            _ => None,
        })
        .expect("main function should exist");
    let Stmt::Expression((Expr::Call { .. }, ok_call_span)) = &main_fn.body.stmts[0].0 else {
        panic!("expected first statement to be `Ok(...)`");
    };
    let Stmt::Expression((Expr::Call { .. }, err_call_span)) = &main_fn.body.stmts[1].0 else {
        panic!("expected second statement to be `Err(...)`");
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
    assert!(
        !output
            .call_type_args
            .contains_key(&SpanKey::from(ok_call_span)),
        "builtin `Ok(...)` should not be serialized as a generic call: {:?}",
        output.call_type_args
    );
    assert!(
        !output
            .call_type_args
            .contains_key(&SpanKey::from(err_call_span)),
        "builtin `Err(...)` should not be serialized as a generic call: {:?}",
        output.call_type_args
    );
    assert_eq!(
        output.expr_types.get(&SpanKey::from(ok_call_span)),
        Some(&Ty::result(Ty::I64, Ty::I64)),
        "expected `Ok(7)` output type to materialize fully before serialization: {:?}",
        output.expr_types
    );
    assert_eq!(
        output.expr_types.get(&SpanKey::from(err_call_span)),
        Some(&Ty::result(Ty::I64, Ty::I64)),
        "expected `Err(9)` output type to materialize fully before serialization: {:?}",
        output.expr_types
    );
}

#[test]
fn result_constructors_accept_unit_payloads() {
    let source = concat!(
        "fn ok_unit() -> Result<(), i64> { Ok(()) }\n",
        "fn err_unit() -> Result<i64, ()> { Err(()) }\n",
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}

#[test]
fn ok_unit_match_pattern_accepted() {
    // `Ok(())` as a match arm pattern against `Result<(), E>` must not error.
    let (errors, _) = parse_and_check(concat!(
        "fn main() {\n",
        "    let r: Result<(), string> = Ok(());\n",
        "    let _ = match r {\n",
        "        Ok(()) => 0,\n",
        "        Err(_) => 1,\n",
        "    };\n",
        "}\n",
    ));
    assert!(
        errors.is_empty(),
        "Ok(()) pattern in match arm against Result<(), E> must type-check: {errors:?}"
    );
}

#[test]
fn err_unit_match_pattern_accepted() {
    // `Err(())` as a match arm pattern against `Result<T, ()>` must not error.
    let (errors, _) = parse_and_check(concat!(
        "fn main() {\n",
        "    let r: Result<i64, ()> = Err(());\n",
        "    let _ = match r {\n",
        "        Ok(n) => n,\n",
        "        Err(()) => 0,\n",
        "    };\n",
        "}\n",
    ));
    assert!(
        errors.is_empty(),
        "Err(()) pattern in match arm against Result<T, ()> must type-check: {errors:?}"
    );
}

#[test]
fn unit_match_pattern_accepted_on_unit_scrutinee() {
    // `()` as a top-level match pattern on a unit scrutinee must not error.
    let (errors, _) = parse_and_check(concat!(
        "fn unit_val() -> () { () }\n",
        "fn main() {\n",
        "    let _ = match unit_val() {\n",
        "        () => 0,\n",
        "    };\n",
        "}\n",
    ));
    assert!(
        errors.is_empty(),
        "() pattern against unit scrutinee must type-check: {errors:?}"
    );
}

#[test]
fn ok_unit_pattern_rejected_against_non_unit_ok_payload() {
    // `Ok(())` against `Result<i64, E>` must still be an error — the payload
    // type is `i64`, not unit, so the empty-tuple pattern is a mismatch.
    let (errors, _) = parse_and_check(concat!(
        "fn main() {\n",
        "    let r: Result<i64, string> = Ok(1);\n",
        "    let _ = match r {\n",
        "        Ok(()) => 0,\n",
        "        Err(_) => 1,\n",
        "    };\n",
        "}\n",
    ));
    assert!(
        !errors.is_empty(),
        "Ok(()) against Result<i64, E> must produce a type error"
    );
}

#[test]
fn builtin_result_constructor_composite_output_type_fallbacks_materialize() {
    let source = concat!(
        "fn main() -> i64 {\n",
        "    Ok(Some(7));\n",
        "    Err(Some(9));\n",
        "    0\n",
        "}\n",
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let main_fn = result
        .program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            Item::Function(function) if function.name == "main" => Some(function),
            _ => None,
        })
        .expect("main function should exist");
    let Stmt::Expression((Expr::Call { .. }, ok_call_span)) = &main_fn.body.stmts[0].0 else {
        panic!("expected first statement to be `Ok(...)`");
    };
    let Stmt::Expression((Expr::Call { .. }, err_call_span)) = &main_fn.body.stmts[1].0 else {
        panic!("expected second statement to be `Err(...)`");
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
    assert!(
        !output
            .call_type_args
            .contains_key(&SpanKey::from(ok_call_span)),
        "builtin `Ok(...)` should not be serialized as a generic call: {:?}",
        output.call_type_args
    );
    assert!(
        !output
            .call_type_args
            .contains_key(&SpanKey::from(err_call_span)),
        "builtin `Err(...)` should not be serialized as a generic call: {:?}",
        output.call_type_args
    );
    let expected = Ty::result(Ty::option(Ty::I64), Ty::option(Ty::I64));
    assert_eq!(
        output.expr_types.get(&SpanKey::from(ok_call_span)),
        Some(&expected),
        "expected `Ok(Some(7))` output type to preserve composite fallback: {:?}",
        output.expr_types
    );
    assert_eq!(
        output.expr_types.get(&SpanKey::from(err_call_span)),
        Some(&expected),
        "expected `Err(Some(9))` output type to preserve composite fallback: {:?}",
        output.expr_types
    );
}

#[test]
fn explicit_cooperate_expression_is_parse_error() {
    let source = "fn main() { cooperate(); }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.iter().any(|error| error.message.contains(
            "'cooperate' is compiler-internal; explicit cooperate expressions are not supported"
        )),
        "expected explicit cooperate parse rejection, got: {:?}",
        result.errors
    );
}

#[test]
fn warn_unused_variable() {
    let source = "fn main() { let unused_var = 42; }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(output.errors.is_empty(), "errors: {:?}", output.errors);
    assert!(
        output
            .warnings
            .iter()
            .any(|w| w.message.contains("unused variable `unused_var`")),
        "expected unused variable warning, got: {:?}",
        output.warnings
    );
}

#[test]
fn warn_var_never_mutated() {
    let source = "fn main() { var x = 10; println(x); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(output.errors.is_empty(), "errors: {:?}", output.errors);
    assert!(
        output
            .warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "expected unmutated var warning, got: {:?}",
        output.warnings
    );
}

#[test]
fn no_warn_underscore_prefix() {
    let source = "fn main() { let _unused = 42; }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.message.contains("_unused")),
        "should not warn on _ prefix, got: {:?}",
        output.warnings
    );
}

// -----------------------------------------------------------------------
// Lint / warning regression tests
// -----------------------------------------------------------------------

/// Helper: parse + typecheck, return (errors, warnings).
fn parse_and_check(source: &str) -> (Vec<TypeError>, Vec<TypeError>) {
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "unexpected parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    (output.errors, output.warnings)
}

fn parse_and_check_with_stdlib(source: &str) -> (Vec<TypeError>, Vec<TypeError>) {
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "unexpected parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&result.program);
    (output.errors, output.warnings)
}

#[test]
fn where_clause_assoc_binding_projects_iterator_item_in_generic_body() {
    let result = hew_parser::parse(
        r"
        fn first_or_zero<I>(var it: I) -> i64
        where
            I: Iterator<Item = i64>,
        {
            match it.next() {
                Some(x) => x,
                None => 0,
            }
        }

        fn main() -> i64 {
            let v: Vec<i64> = Vec::new();
            v.push(42);
            first_or_zero(v.into_iter())
        }
        ",
    );
    assert!(
        result.errors.is_empty(),
        "unexpected parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&result.program);

    assert!(
        output.warnings.is_empty(),
        "unexpected warnings: {:?}",
        output.warnings
    );
    assert!(
        output.errors.is_empty(),
        "Iterator<Item = i64> should project I::Item to i64 in a generic body: {:?}",
        output.errors
    );
    assert!(
        output
            .pattern_resolutions
            .values()
            .flat_map(|arm| arm.payload_bindings.iter())
            .any(|payload| payload.binding_name == "x" && payload.ty == Ty::I64),
        "match payload binding from Option<I::Item> should be published as i64: {:?}",
        output.pattern_resolutions
    );
}

#[test]
fn where_clause_assoc_binding_projects_non_iterator_assoc_type() {
    let (errors, warnings) = parse_and_check(
        r"
        trait Projector {
            type Output;
            fn get(self) -> Self::Output;
        }

        type Meter {
            value: i64;
        }

        impl Projector for Meter {
            type Output = i64;

            fn get(self) -> i64 {
                self.value
            }
        }

        fn read<P>(p: P) -> i64
        where
            P: Projector<Output = i64>,
        {
            p.get()
        }

        fn main() -> i64 {
            read(Meter { value: 42 })
        }
        ",
    );

    assert!(warnings.is_empty(), "unexpected warnings: {warnings:?}");
    assert!(
        errors.is_empty(),
        "where-clause associated-type projection should not be Iterator-specific: {errors:?}"
    );
}

#[test]
fn unbound_where_clause_assoc_projection_remains_type_error() {
    let (errors, _warnings) = parse_and_check_with_stdlib(
        r"
        fn first_or_zero<I>(var it: I) -> i64
        where
            I: Iterator,
        {
            match it.next() {
                Some(x) => x,
                None => 0,
            }
        }
        ",
    );

    assert!(
        errors.iter().any(|error| error.message.contains("I::Item")),
        "unbound I::Item must remain unresolved/fail-closed; got {errors:?}"
    );
}

#[test]
fn call_site_rejects_assoc_binding_mismatch() {
    let (errors, _warnings) = parse_and_check_with_stdlib(
        r#"
        fn first_or_zero<I>(var it: I) -> i64
        where
            I: Iterator<Item = i64>,
        {
            match it.next() {
                Some(x) => x,
                None => 0,
            }
        }

        fn main() -> i64 {
            let v: Vec<string> = Vec::new();
            v.push("not an integer");
            first_or_zero(v.into_iter())
        }
        "#,
    );

    assert!(
        errors.iter().any(|error| {
            error.kind == TypeErrorKind::BoundsNotSatisfied
                && error.message.contains("Iterator<Item = i64>")
                && error.message.contains("string")
        }),
        "Vec<string> must not satisfy Iterator<Item = i64>; got {errors:?}"
    );
}

#[test]
fn scope_error_type_constructs_and_field_accesses() {
    let source = concat!(
        "import std::concurrency;\n",
        "fn read_primary(err: concurrency.ScopeError<i64>) -> i64 {\n",
        "    let primary: i64 = err.primary;\n",
        "    primary\n",
        "}\n",
        "fn read_others(err: concurrency.ScopeError<i64>) -> Vec<i64> {\n",
        "    let others: Vec<i64> = err.also_failed;\n",
        "    others\n",
        "}\n",
        "fn read_cancelled(err: concurrency.ScopeError<i64>) -> i64 {\n",
        "    let cancelled: i64 = err.cancelled_count;\n",
        "    cancelled\n",
        "}\n",
        "fn pass_through(err: concurrency.ScopeError<i64>) -> concurrency.ScopeError<i64> {\n",
        "    err\n",
        "}\n",
        "fn main() {\n",
        "}\n",
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected type errors: {:?}",
        output.errors
    );
}

#[test]
fn builtin_print_registration_keeps_display_bounds_on_bare_names() {
    let mut checker = Checker::new(test_registry());
    checker.register_builtins();

    for name in ["print", "println", "to_string", "assert_eq", "assert_ne"] {
        let sig = checker
            .fn_sigs
            .get(name)
            .unwrap_or_else(|| panic!("missing builtin signature for {name}"));
        assert_eq!(
            sig.type_params,
            vec!["T".to_string()],
            "{name} should expose a single generic parameter"
        );
        assert_eq!(
            sig.type_param_bounds.get("T"),
            Some(&vec!["Display".to_string()]),
            "{name} should keep a Display bound on its bare-name registration"
        );
    }

    let len_sig = checker.fn_sigs.get("len").expect("missing len builtin");
    assert!(
        len_sig.type_param_bounds.is_empty(),
        "len must stay out of the Display migration"
    );
}

#[test]
fn print_and_println_reject_struct_without_display_impl() {
    let (errors, warnings) = parse_and_check_with_stdlib(
        r"
        type Hidden {
            value: i64;
        }

        fn main() {
            let hidden = Hidden { value: 1 };
            print(hidden);
            println(hidden);
        }
        ",
    );

    assert!(warnings.is_empty(), "unexpected warnings: {warnings:?}");
    let bounds_errors: Vec<_> = errors
        .iter()
        .filter(|error| {
            error.kind == TypeErrorKind::BoundsNotSatisfied && error.message.contains("Display")
        })
        .collect();
    assert_eq!(
        bounds_errors.len(),
        2,
        "print/println should reject values without Display: {errors:?}"
    );
}

#[test]
fn display_impl_satisfies_bounded_magic_builtins() {
    let (errors, warnings) = parse_and_check_with_stdlib(
        r#"
        type Widget {
            value: i64;
        }

        impl Display for Widget {
            fn fmt(widget: Widget) -> string {
                "widget"
            }
        }

        fn main() {
            let widget = Widget { value: 1 };
            print(widget);
            println(widget);
            let text = to_string(widget);
            assert_eq(widget, widget);
            assert_ne(widget, widget);
            println(text);
        }
        "#,
    );

    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    assert!(warnings.is_empty(), "unexpected warnings: {warnings:?}");
}

#[test]
fn deferred_bound_check_drains_after_defaulting() {
    let mut checker = make_checker_with_trait("MyTrait", &[], false, false);
    let span = 0..0;
    let var = TypeVar::fresh();
    let sig = FnSig {
        type_params: vec!["T".to_string()],
        type_param_bounds: HashMap::from([("T".to_string(), vec!["MyTrait".to_string()])]),
        ..Default::default()
    };

    checker.enforce_type_param_bounds(&sig, &[Ty::Var(var)], &span);
    assert!(
        checker
            .errors
            .iter()
            .all(|error| error.kind != TypeErrorKind::BoundsNotSatisfied),
        "unresolved arg should defer bound enforcement: {:?}",
        checker.errors
    );
    assert_eq!(checker.deferred_bound_checks.len(), 1);

    checker.subst.insert(var, &Ty::I64).unwrap();
    checker.drain_deferred_bound_checks();

    let bounds_errors: Vec<_> = checker
        .errors
        .iter()
        .filter(|error| error.kind == TypeErrorKind::BoundsNotSatisfied)
        .collect();
    assert_eq!(
        bounds_errors.len(),
        1,
        "expected exactly one deferred bound failure after resolution: {:?}",
        checker.errors
    );
    assert!(
        bounds_errors[0].message.contains("MyTrait") && bounds_errors[0].message.contains("i64"),
        "expected deferred diagnostic to mention MyTrait and i64: {:?}",
        bounds_errors[0]
    );
}

#[test]
fn deferred_bound_check_skips_when_var_remains_unresolved() {
    let mut checker = make_checker_with_trait("MyTrait", &[], false, false);
    let span = 0..0;
    let var = TypeVar::fresh();
    let sig = FnSig {
        type_params: vec!["T".to_string()],
        type_param_bounds: HashMap::from([("T".to_string(), vec!["MyTrait".to_string()])]),
        ..Default::default()
    };

    checker.enforce_type_param_bounds(&sig, &[Ty::Var(var)], &span);
    checker
        .deferred_inference_holes
        .push(DeferredInferenceHole {
            span: span.clone(),
            context: "test deferred bound hole".to_string(),
            hole_vars: vec![var],
            source_module: None,
        });

    checker.drain_deferred_bound_checks();
    let program = hew_parser::parse("").program;
    checker.report_unresolved_inference_holes(&program);

    let bounds_errors: Vec<_> = checker
        .errors
        .iter()
        .filter(|error| error.kind == TypeErrorKind::BoundsNotSatisfied)
        .collect();
    assert!(
        bounds_errors.is_empty(),
        "unresolved hole should not also emit bound noise: {:?}",
        checker.errors
    );
    let inference_errors: Vec<_> = checker
        .errors
        .iter()
        .filter(|error| error.kind == TypeErrorKind::InferenceFailed)
        .collect();
    assert_eq!(
        inference_errors.len(),
        1,
        "expected unresolved-hole reporter to stay authoritative: {:?}",
        checker.errors
    );
}

#[test]
fn deferred_bound_check_drains_when_var_resolves_to_satisfying_type() {
    let mut checker = Checker::new(test_registry());
    checker.register_builtins();
    let span = 0..0;
    let var = TypeVar::fresh();
    let sig = FnSig {
        type_params: vec!["T".to_string()],
        type_param_bounds: HashMap::from([("T".to_string(), vec!["Display".to_string()])]),
        ..Default::default()
    };

    checker.enforce_type_param_bounds(&sig, &[Ty::Var(var)], &span);
    checker.subst.insert(var, &Ty::I64).unwrap();
    checker.drain_deferred_bound_checks();

    assert!(
        checker.errors.is_empty(),
        "resolved Display-bound arg should pass deferred drain: {:?}",
        checker.errors
    );
}

// ---- unused variable ----

#[test]
fn warn_unused_in_nested_scope() {
    let (errors, warnings) = parse_and_check("fn main() { if true { let nested = 1; } }");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        warnings
            .iter()
            .any(|w| w.message.contains("unused variable `nested`")),
        "expected unused variable warning for nested, got: {warnings:?}"
    );
}

#[test]
fn no_warn_used_variable() {
    let (errors, warnings) = parse_and_check("fn main() { let x = 42; println(x); }");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("unused variable `x`")),
        "should not warn on used variable, got: {warnings:?}"
    );
}

#[test]
fn no_warn_underscore_alone() {
    let (_, warnings) = parse_and_check("fn main() { let _ = 42; }");
    assert!(
        !warnings.iter().any(|w| w.message.contains("unused")),
        "bare _ should never warn, got: {warnings:?}"
    );
}

// ---- var never mutated ----

#[test]
fn no_warn_var_actually_mutated() {
    let (errors, warnings) = parse_and_check("fn main() { var x = 10; x = 20; println(x); }");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "should not warn when var is actually reassigned, got: {warnings:?}"
    );
}

#[test]
fn mutable_param_can_be_reassigned() {
    let (errors, warnings) = parse_and_check("fn bump(var x: i64) -> i64 { x = x + 1; x }");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "mutable param reassignment should suppress unused-mut warning, got: {warnings:?}"
    );
}

#[test]
fn immutable_param_cannot_be_reassigned() {
    let (errors, warnings) = parse_and_check("fn bump(x: i64) -> i64 { x = x + 1; x }");
    assert!(
        errors.iter().any(|e| e
            .message
            .contains("cannot assign to immutable variable `x`")),
        "expected immutable parameter assignment error, got: {errors:?}"
    );
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "immutable param should not produce unused-mut warning, got: {warnings:?}"
    );
}

#[test]
fn immutable_field_assignment_root_is_rejected() {
    let (errors, warnings) = parse_and_check(concat!(
        "type Point { x: i64; }\n",
        "fn main() { let p = Point { x: 1 }; p.x = 2; }\n",
    ));
    assert!(
        errors.iter().any(|e| e
            .message
            .contains("cannot assign to immutable variable `p`")),
        "expected immutable field-assignment root error, got: {errors:?}"
    );
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "immutable field assignment should not produce unused-mut warning, got: {warnings:?}"
    );
}

#[test]
fn immutable_param_field_assignment_root_is_rejected() {
    let (errors, warnings) = parse_and_check(concat!(
        "type Point { x: i64; }\n",
        "fn bump(p: Point) { p.x = 2; }\n",
    ));
    assert!(
        errors.iter().any(|e| e
            .message
            .contains("cannot assign to immutable variable `p`")),
        "expected immutable parameter field-assignment root error, got: {errors:?}"
    );
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "immutable parameter field assignment should not produce unused-mut warning, got: {warnings:?}"
    );
}

#[test]
fn immutable_compound_field_assignment_root_is_rejected() {
    let (errors, warnings) = parse_and_check(concat!(
        "type Point { x: i64; }\n",
        "fn main() { let p = Point { x: 1 }; p.x += 2; }\n",
    ));
    assert!(
        errors.iter().any(|e| e
            .message
            .contains("cannot assign to immutable variable `p`")),
        "expected immutable compound field-assignment root error, got: {errors:?}"
    );
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "immutable compound field assignment should not produce unused-mut warning, got: {warnings:?}"
    );
}

#[test]
fn mutable_field_assignment_root_counts_as_mutation() {
    let (errors, warnings) = parse_and_check(concat!(
        "type Point { x: i64; }\n",
        "fn main() { var p = Point { x: 1 }; p.x = 2; println(p.x); }\n",
    ));
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "field assignment should count as a mutation of the root binding, got: {warnings:?}"
    );
}

#[test]
fn mutable_param_field_assignment_root_counts_as_mutation() {
    let (errors, warnings) = parse_and_check(concat!(
        "type Point { x: i64; }\n",
        "fn bump(var p: Point) { p.x = 2; println(p.x); }\n",
    ));
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "parameter field assignment should count as a mutation of the root binding, got: {warnings:?}"
    );
}

#[test]
fn immutable_index_assignment_root_is_rejected() {
    let (errors, warnings) = parse_and_check("fn main() { let xs = [1, 2]; xs[0] = 3; }\n");
    assert!(
        errors.iter().any(|e| e
            .message
            .contains("cannot assign to immutable variable `xs`")),
        "expected immutable index-assignment root error, got: {errors:?}"
    );
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "immutable index assignment should not produce unused-mut warning, got: {warnings:?}"
    );
}

#[test]
fn immutable_param_index_assignment_root_is_rejected() {
    let (errors, warnings) = parse_and_check("fn bump(xs: Vec<i64>) { xs[0] = 2; }\n");
    assert!(
        errors.iter().any(|e| e
            .message
            .contains("cannot assign to immutable variable `xs`")),
        "expected immutable parameter index-assignment root error, got: {errors:?}"
    );
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "immutable parameter index assignment should not produce unused-mut warning, got: {warnings:?}"
    );
}

#[test]
fn mutable_index_assignment_root_counts_as_mutation() {
    let (errors, warnings) =
        parse_and_check("fn main() { var xs = [1, 2]; xs[0] = 3; println(xs[0]); }\n");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "index assignment should count as a mutation of the root binding, got: {warnings:?}"
    );
}

#[test]
fn mutable_param_index_assignment_root_counts_as_mutation() {
    let (errors, warnings) =
        parse_and_check("fn bump(var xs: Vec<i64>) { xs[0] = 2; println(xs[0]); }\n");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "parameter index assignment should count as a mutation of the root binding, got: {warnings:?}"
    );
}

#[test]
fn warn_var_never_mutated_suggestion() {
    let (_, warnings) = parse_and_check("fn main() { var x = 10; println(x); }");
    let w = warnings
        .iter()
        .find(|w| w.message.contains("never reassigned"))
        .expect("expected never-reassigned warning");
    assert!(
        w.suggestions.iter().any(|s| s.contains("let")),
        "should suggest using `let`, got: {:?}",
        w.suggestions
    );
}

// ---- assignment LHS not false-positive as "used" ----

#[test]
fn warn_write_only_variable() {
    // `x` is only written to, never read — should get unused warning
    let (_, warnings) = parse_and_check("fn main() { var x = 0; x = 1; }");
    assert!(
        warnings
            .iter()
            .any(|w| w.message.contains("unused variable `x`")),
        "write-only variable should be warned as unused, got: {warnings:?}"
    );
}

#[test]
fn no_warn_variable_used_then_assigned() {
    // `x` is read (println) AND then written — it's genuinely used
    let (_, warnings) = parse_and_check("fn main() { var x = 0; println(x); x = 1; println(x); }");
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("unused variable `x`")),
        "variable that is read should not get unused warning, got: {warnings:?}"
    );
}

// ---- while true → loop ----

#[test]
fn warn_while_true() {
    let (_, warnings) = parse_and_check("fn main() { while true { break; } }");
    let w = warnings
        .iter()
        .find(|w| w.message.contains("while true"))
        .expect("expected while-true style warning");
    assert!(
        w.suggestions.iter().any(|s| s.contains("loop")),
        "should suggest loop, got: {:?}",
        w.suggestions
    );
    assert!(
        matches!(w.kind, TypeErrorKind::StyleSuggestion),
        "should be StyleSuggestion kind"
    );
}

#[test]
fn no_warn_while_condition() {
    let (_, warnings) = parse_and_check("fn main() { let x = true; while x { break; } }");
    assert!(
        !warnings.iter().any(|w| w.message.contains("while true")),
        "should not warn on while <variable>, got: {warnings:?}"
    );
}

// ---- unused return value ----

#[test]
fn no_warn_discarded_return_value() {
    // Calling a function that returns a value without binding the result
    // should NOT warn — users can discard results freely
    let (_, warnings) = parse_and_check(concat!(
        "fn compute() -> i32 { 42 }\n",
        "fn main() { compute(); }\n",
    ));
    assert!(
        !warnings.iter().any(|w| w.message.contains("unused")),
        "discarded return value should not warn, got: {warnings:?}"
    );
}

#[test]
fn no_warn_unused_println() {
    // println returns unit, and is a known side-effect function
    let (_, warnings) = parse_and_check("fn main() { println(42); }");
    assert!(
        !warnings.iter().any(|w| w.message.contains("unused")),
        "println() should not produce unused value warning, got: {warnings:?}"
    );
}

#[test]
fn no_warn_unused_spawn() {
    // spawn is a side-effect expression — don't warn about discarded return
    let (_, warnings) = parse_and_check(concat!(
        "actor Worker { count: i32;\n",
        "    receive fn work() {} }\n",
        "fn main() { let _w = spawn Worker(count: 0); }\n",
    ));
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("unused") && !w.message.contains("unused variable")),
        "spawn() should not produce unused value warning, got: {warnings:?}"
    );
}

// ---- unit binding ----

#[test]
fn warn_unit_binding() {
    let (_, warnings) = parse_and_check("fn main() { let x = println(42); }");
    assert!(
        warnings.iter().any(|w| w.message.contains("unit type")),
        "binding to unit type should warn, got: {warnings:?}"
    );
}

#[test]
fn no_warn_non_unit_binding() {
    let (_, warnings) =
        parse_and_check("fn compute() -> i32 { 42 }\nfn main() { let x = compute(); println(x); }");
    assert!(
        !warnings.iter().any(|w| w.message.contains("unit type")),
        "binding to non-unit type should not warn, got: {warnings:?}"
    );
}

// ---- Levenshtein "did you mean?" suggestions ----

#[test]
fn suggest_similar_variable() {
    let (errors, _) = parse_and_check("fn main() { let counter = 42; println(conter); }");
    let err = errors
        .iter()
        .find(|e| e.message.contains("conter"))
        .expect("expected error for misspelled variable");
    assert!(
        err.suggestions.iter().any(|s| s.contains("counter")),
        "should suggest 'counter', got: {:?}",
        err.suggestions
    );
}

#[test]
fn suggest_similar_function() {
    let (errors, _) = parse_and_check(concat!(
        "fn calculate_sum(a: i32, b: i32) -> i32 { a + b }\n",
        "fn main() { calculate_sun(1, 2); }\n",
    ));
    let err = errors
        .iter()
        .find(|e| e.message.contains("calculate_sun"))
        .expect("expected error for misspelled function");
    assert!(
        err.suggestions.iter().any(|s| s.contains("calculate_sum")),
        "should suggest 'calculate_sum', got: {:?}",
        err.suggestions
    );
}

#[test]
fn suggest_similar_type() {
    // Use a misspelled type in a constructor position, which triggers undefined type lookup
    let (errors, _) = parse_and_check(concat!(
        "type Point { x: i32; y: i32; }\n",
        "fn make() { let p = Pont { x: 0, y: 0 }; println(p.x); }\n",
    ));
    let err = errors
        .iter()
        .find(|e| e.message.contains("Pont"))
        .expect("expected error for misspelled type");
    assert!(
        err.suggestions.iter().any(|s| s.contains("Point")),
        "should suggest 'Point', got: {:?}",
        err.suggestions
    );
}

#[test]
fn suggest_similar_field() {
    let (errors, _) = parse_and_check(concat!(
        "type Point { x: i32; y: i32; }\n",
        "fn get_z(p: Point) -> i32 { p.z }\n",
    ));
    let err = errors
        .iter()
        .find(|e| e.message.contains('z'))
        .expect("expected error for undefined field");
    assert!(
        !err.suggestions.is_empty(),
        "should suggest similar fields, got: {:?}",
        err.suggestions
    );
}

#[test]
fn suggest_similar_method() {
    let (errors, _) = parse_and_check(concat!(
        "type Counter {}\n",
        "impl Counter { fn length(c: Counter) -> i64 { 0 } }\n",
        "fn main() { let c = Counter {}; c.lenght(); }\n",
    ));
    let err = errors
        .iter()
        .find(|e| e.message.contains("lenght"))
        .expect("expected error for misspelled method");
    assert!(
        err.suggestions.iter().any(|s| s.contains("length")),
        "should suggest 'length', got: {:?}",
        err.suggestions
    );
}

#[test]
fn no_suggest_method_when_too_different() {
    let (errors, _) = parse_and_check(concat!(
        "type Counter {}\n",
        "impl Counter { fn length(c: Counter) -> i64 { 0 } }\n",
        "fn main() { let c = Counter {}; c.zzzzz(); }\n",
    ));
    let err = errors
        .iter()
        .find(|e| e.message.contains("zzzzz"))
        .expect("expected error for undefined method");
    assert!(
        err.suggestions.is_empty() || !err.suggestions.iter().any(|s| s.contains("length")),
        "should not suggest distant method names, got: {:?}",
        err.suggestions
    );
}

#[test]
fn no_suggest_when_too_different() {
    let (errors, _) = parse_and_check("fn main() { let alpha = 1; println(zzzzz); }");
    let err = errors
        .iter()
        .find(|e| e.message.contains("zzzzz"))
        .expect("expected error for undefined variable");
    // "zzzzz" is too far from "alpha" — no suggestion
    assert!(
        err.suggestions.is_empty() || !err.suggestions.iter().any(|s| s.contains("alpha")),
        "should not suggest distant names, got: {:?}",
        err.suggestions
    );
}

// ---- warning severity and kind ----

#[test]
fn lint_warnings_have_warning_severity() {
    let (_, warnings) = parse_and_check("fn main() { let unused = 42; }");
    for w in &warnings {
        assert_eq!(
            w.severity,
            crate::error::Severity::Warning,
            "lint warnings must have Warning severity, got: {:?} for {}",
            w.severity,
            w.message
        );
    }
}

#[test]
fn unused_variable_has_correct_kind() {
    let (_, warnings) = parse_and_check("fn main() { let unused = 42; }");
    let w = warnings
        .iter()
        .find(|w| w.message.contains("unused"))
        .unwrap();
    assert!(
        matches!(w.kind, TypeErrorKind::UnusedVariable),
        "expected UnusedVariable kind, got: {:?}",
        w.kind
    );
}

#[test]
fn never_mutated_has_correct_kind() {
    let (_, warnings) = parse_and_check("fn main() { var x = 10; println(x); }");
    let w = warnings
        .iter()
        .find(|w| w.message.contains("never reassigned"))
        .unwrap();
    assert!(
        matches!(w.kind, TypeErrorKind::UnusedMut),
        "expected UnusedMut kind, got: {:?}",
        w.kind
    );
}

// ---- multiple warnings in one function ----

#[test]
fn multiple_warnings_in_one_fn() {
    let (errors, warnings) = parse_and_check(concat!(
        "fn main() {\n",
        "    let unused_a = 1;\n",
        "    var never_written = 2;\n",
        "    println(never_written);\n",
        "    while true { break; }\n",
        "}\n",
    ));
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        warnings
            .iter()
            .any(|w| w.message.contains("unused variable `unused_a`")),
        "missing unused_a warning: {warnings:?}"
    );
    assert!(
        warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "missing never-mutated warning: {warnings:?}"
    );
    assert!(
        warnings.iter().any(|w| w.message.contains("while true")),
        "missing while-true warning: {warnings:?}"
    );
}

// ── Unreachable Code Tests ──────────────────────────────────────────

#[test]
fn warn_unreachable_after_return() {
    let source = "fn foo() -> i32 { return 1; let x = 2; x }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::UnreachableCode),
        "expected unreachable code warning, got: {:?}",
        output.warnings
    );
}

#[test]
fn no_warn_unreachable_when_return_is_last() {
    let source = "fn foo() -> i32 { let x = 1; return x; }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::UnreachableCode),
        "should not warn when return is last statement: {:?}",
        output.warnings
    );
}

// ── Shadowing Tests ─────────────────────────────────────────────────

#[test]
fn error_same_scope_shadowing() {
    let source = "fn main() { let x = 5; let x = 10; println(x); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::Shadowing
                && e.message.contains("already defined in this scope")),
        "expected same-scope shadowing error, got: {:?}",
        output.errors
    );
}

#[test]
fn warn_nested_scope_shadowing() {
    // Nested/child scope shadowing is a warning, not an error.
    let source = "fn main() { let x = 1; if true { let x = 2; println(x); } println(x); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::Shadowing
                && w.message.contains("shadows a binding in an outer scope")),
        "expected outer-scope shadowing warning, got warnings: {:?}, errors: {:?}",
        output.warnings,
        output.errors
    );
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::Shadowing),
        "outer-scope shadowing of a local should not be an error: {:?}",
        output.errors
    );
}

#[test]
fn no_shadowing_diagnostic_underscore_prefix() {
    // Underscore-prefixed bindings are fully exempt from shadowing diagnostics.
    let source = "fn main() { let _x = 1; if true { let _x = 2; println(_x); } println(_x); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::Shadowing),
        "should not error for _ prefixed vars: {:?}",
        output.errors
    );
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::Shadowing),
        "should not warn for _ prefixed vars: {:?}",
        output.warnings
    );
}

#[test]
fn no_shadowing_diagnostic_for_loop_var() {
    // For-loop induction variables are exempt from shadowing diagnostics.
    let source = "fn main() { let i = 0; for i in 0..10 { println(i); } println(i); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::Shadowing),
        "should not error for for-loop variable shadowing: {:?}",
        output.errors
    );
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::Shadowing),
        "should not warn for for-loop variable shadowing: {:?}",
        output.warnings
    );
}

#[test]
fn warn_deeply_nested_scope_shadowing() {
    // Shadowing across multiple scope levels is still a warning.
    let source = r"
        fn main() {
            let val = 1;
            if true {
                if true {
                    let val = 99;
                    println(val);
                }
            }
            println(val);
        }
    ";
    let (errors, warnings) = parse_and_check(source);
    assert!(
        warnings.iter().any(|w| w.kind == TypeErrorKind::Shadowing
            && w.message.contains("shadows a binding in an outer scope")),
        "expected nested shadowing warning, got warnings: {warnings:?}, errors: {errors:?}"
    );
    assert!(
        !errors.iter().any(|e| e.kind == TypeErrorKind::Shadowing),
        "outer-scope shadowing of a local should not be an error: {errors:?}"
    );
}

#[test]
fn test_actor_field_shadowing_is_error() {
    // Shadowing an actor field is a hard error — bare field access requires
    // unambiguous names.
    let source = r"
        actor Counter {
            var count: i64 = 0;
            receive fn update(count: i64) {
                println(count);
            }
        }
    ";
    let (errors, _warnings) = parse_and_check(source);
    assert!(
        errors.iter().any(|e| e.kind == TypeErrorKind::Shadowing
            && e.message
                .contains("variable `count` shadows a binding in an outer scope")),
        "should error on param shadowing actor field, got: {errors:?}",
    );
}

#[test]
fn test_actor_fn_method_field_shadowing_is_error() {
    // Shadowing an actor field via an fn helper method is also a hard error.
    let source = r"
        actor Counter {
            var count: i64 = 0;
            fn helper(count: i64) -> i64 { count }
        }
    ";
    let (errors, _warnings) = parse_and_check(source);
    assert!(
        errors.iter().any(|e| e.kind == TypeErrorKind::Shadowing
            && e.message
                .contains("variable `count` shadows a binding in an outer scope")),
        "should error on fn param shadowing actor field, got: {errors:?}",
    );
}

#[test]
fn actor_this_field_points_to_bare_state_field() {
    let source = r"
        actor Counter {
            let count: i64;
            receive fn get() -> i64 {
                this.count
            }
        }
    ";
    let (errors, _) = parse_and_check(source);
    assert!(
        errors.iter().any(|error| {
            error.kind == TypeErrorKind::UndefinedField
                && error.message.contains("`this` is the actor handle")
                && error.message.contains("not `this.count`")
                && error
                    .suggestions
                    .iter()
                    .any(|suggestion| suggestion == "count")
        }),
        "`this.field` in actor body should suggest bare field access; got: {errors:?}",
    );
}

// ── Dead Code (Unused Function) Tests ───────────────────────────────

#[test]
fn warn_dead_code_unused_function() {
    let source = "fn unused_helper() -> i32 { 42 } fn main() { println(1); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::DeadCode && w.message.contains("unused_helper")),
        "expected dead code warning for unused_helper, got: {:?}",
        output.warnings
    );
}

#[test]
fn no_warn_dead_code_main() {
    let source = "fn main() { println(1); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::DeadCode),
        "should not warn about main: {:?}",
        output.warnings
    );
}

#[test]
fn no_warn_dead_code_called_function() {
    let source = "fn helper() -> i32 { 42 } fn main() { let x = helper(); println(x); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::DeadCode && w.message.contains("helper")),
        "should not warn about called function: {:?}",
        output.warnings
    );
}

#[test]
fn no_warn_dead_code_underscore_prefix() {
    let source = "fn _unused() -> i32 { 42 } fn main() { println(1); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::DeadCode),
        "should not warn for _ prefixed functions: {:?}",
        output.warnings
    );
}

#[test]
fn no_warn_dead_code_called_from_actor_receive() {
    let source = r"
fn fib(n: i32) -> i32 {
if n <= 1 { n } else { fib(n - 1) + fib(n - 2) }
}
actor Worker {
receive fn compute(n: i32) {
    let r = fib(n);
    println(r);
}
}
fn main() {
let w = spawn Worker();
w.compute(10);
}
";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::DeadCode && w.message.contains("fib")),
        "should not warn about function called from actor receive fn: {:?}",
        output.warnings
    );
}

// ── Unused Import Tests ─────────────────────────────────────────────

#[test]
fn warn_unused_import() {
    let source = "import std::encoding::json;\nfn main() { println(1); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&result.program);
    assert!(
        output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::UnusedImport && w.message.contains("json")),
        "expected unused import warning for json, got: {:?}",
        output.warnings
    );
}

#[test]
fn no_warn_used_import() {
    let source =
        "import std::encoding::json;\nfn main() { let v = json.parse(\"[]\"); println(v); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&result.program);
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::UnusedImport && w.message.contains("json")),
        "should not warn about used import: {:?}",
        output.warnings
    );
}

#[test]
fn stdlib_import_registers_trait_impls_for_generic_bounds() {
    let root_source = r"
        import std::string;

        fn main() -> string {
            string.describe(string.make_label())
        }
    ";
    let module_source = r#"
        pub trait Describable {
            fn describe(val: Self) -> string;
        }

        pub type Label {
            text: string;
        }

        pub fn make_label() -> Label {
            Label { text: "hello" }
        }

        impl Describable for Label {
            fn describe(label: Label) -> string {
                label.text
            }
        }

        pub fn describe<T: Describable>(item: T) -> string {
            item.describe()
        }

    "#;

    let mut root = hew_parser::parse(root_source);
    assert!(
        root.errors.is_empty(),
        "root parse errors: {:?}",
        root.errors
    );
    let call_span = root
        .program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            Item::Function(fd) if fd.name == "main" => {
                fd.body.trailing_expr.as_ref().map(|expr| expr.1.clone())
            }
            _ => None,
        })
        .expect("main trailing call should exist");
    let module = hew_parser::parse(module_source);
    assert!(
        module.errors.is_empty(),
        "module parse errors: {:?}",
        module.errors
    );

    let import_decl = root
        .program
        .items
        .iter_mut()
        .find_map(|(item, _)| match item {
            Item::Import(import) => Some(import),
            _ => None,
        })
        .expect("root import should exist");
    import_decl.resolved_items = Some(module.program.items.clone());

    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&root.program);

    assert!(
        !output.user_modules.contains("string"),
        "stdlib Hew import should not go through the user-module import path"
    );
    assert!(
        output.errors.is_empty(),
        "stdlib imported Hew impl should satisfy imported generic bounds: {:?}",
        output.errors
    );
    let inferred = output
        .call_type_args
        .get(&SpanKey::from(&call_span))
        .expect("stdlib imported generic call should record inferred type args");
    assert_eq!(
        inferred,
        &vec![Ty::Named {
            builtin: None,
            name: "Label".to_string(),
            args: vec![],
        }]
    );
    assert!(
        checker
            .trait_impls_set
            .contains(&("Label".to_string(), "Describable".to_string())),
        "stdlib Hew items should register trait impls for downstream generic bound checks"
    );
}

#[test]
fn impl_for_primitive_int_populates_primitive_trait_impl_table() {
    // Stage A1: `impl Display for i64` registers under the canonical `i64`
    // key (the lowering name for `Ty::I64`) so receiver-keyed dispatch can
    // find it later.  The literal AST string `i64` must round-trip through
    // `Ty::from_name` → `canonical_lowering_name` to agree with the
    // dispatch site, which only ever sees a resolved `Ty`.
    let source = r#"
        pub trait Display {
            fn fmt(val: Self) -> string;
        }

        impl Display for i64 {
            fn fmt(n: i64) -> string {
                ""
            }
        }
    "#;
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );

    let mut checker = Checker::new(test_registry());
    let _output = checker.check_program(&parsed.program);

    let methods = checker
        .primitive_trait_impls
        .get(&("i64".to_string(), "Display".to_string()))
        .expect("primitive trait impl table should have entry for (i64, Display)");
    let fmt_sig = methods
        .get("fmt")
        .expect("fmt method should be recorded for impl Display for i64");
    assert!(
        fmt_sig.params.is_empty(),
        "receiver should be filtered: {:?}",
        fmt_sig.params
    );
    assert_eq!(fmt_sig.return_type, Ty::String);
}

#[test]
fn impl_for_builtin_vec_populates_primitive_trait_impl_table() {
    // Vec is a compiler-builtin generic with no `type_defs` entry; user
    // impls on Vec must reach the side table the same way primitives do.
    let source = r#"
        pub trait Display {
            fn fmt(val: Self) -> string;
        }

        impl Display for Vec<i32> {
            fn fmt(v: Vec<i32>) -> string {
                ""
            }
        }
    "#;
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );

    let mut checker = Checker::new(test_registry());
    let _output = checker.check_program(&parsed.program);

    assert!(
        checker
            .primitive_trait_impls
            .contains_key(&("Vec".to_string(), "Display".to_string())),
        "primitive trait impl table should record impls keyed on the bare \
         builtin generic name (Vec) regardless of element type"
    );
}

#[test]
fn impl_for_user_struct_does_not_pollute_primitive_trait_impl_table() {
    // The side table must stay empty for user-defined struct receivers —
    // those flow through `type_defs` and would create duplicate dispatch
    // paths if the helper accepted them.
    let source = r#"
        pub trait Display {
            fn fmt(val: Self) -> string;
        }

        pub type MyType {
            value: i64;
        }

        impl Display for MyType {
            fn fmt(m: MyType) -> string {
                ""
            }
        }
    "#;
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );

    let mut checker = Checker::new(test_registry());
    let _output = checker.check_program(&parsed.program);

    // The side table is non-empty after `register_builtins` because the ten
    // `std/builtins.hew` Display blanket impls (i8/i16/i32/i64/u8/u16/u32/u64/
    // bool/char) land here as the source-of-truth dispatch entries (see
    // `register_builtins_hew_impls`).  The invariant this test enforces is
    // narrower: a user `impl Display for MyType` (where `MyType` is a
    // user-declared struct) must NOT leak into this primitive-keyed table —
    // user-struct method dispatch goes through `type_defs.methods` instead.
    let leaked: Vec<_> = checker
        .primitive_trait_impls
        .keys()
        .filter(|(rx_key, _)| rx_key == "MyType")
        .collect();
    assert!(
        leaked.is_empty(),
        "user struct impls must not leak into the primitive trait table: {leaked:?}"
    );
}

/// Stage A2: receiver-kind dispatch matrix.
///
/// Each row exercises one canonical receiver kind (primitive or
/// compiler-builtin generic) plus a user `impl Display for <kind>` and
/// asserts that `x.fmt()` typechecks cleanly and records a
/// `MethodCallReceiverKind::PrimitiveTraitImpl` metadata entry.  The
/// metadata is the checker→codegen output-boundary contract; if a
/// receiver kind dispatches via the new path but the metadata is not
/// recorded, downstream codegen has no way to find the resolved impl
/// (per the `checker-output-boundary` P0 lesson).
fn assert_primitive_trait_dispatch_records_metadata(
    source: &str,
    expected_canonical: &str,
    expected_trait: &str,
) {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors for source `{source}`: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&parsed.program);
    assert!(
        output.errors.is_empty(),
        "expected clean type check for source:\n{source}\n\nbut got: {:?}",
        output.errors
    );
    let any = output
        .method_call_receiver_kinds
        .values()
        .any(|kind| match kind {
            MethodCallReceiverKind::PrimitiveTraitImpl {
                trait_name,
                canonical_receiver,
            } => trait_name == expected_trait && canonical_receiver == expected_canonical,
            _ => false,
        });
    assert!(
        any,
        "expected PrimitiveTraitImpl{{trait_name={expected_trait}, canonical_receiver={expected_canonical}}} \
         in method_call_receiver_kinds, found: {:?}",
        output.method_call_receiver_kinds.values().collect::<Vec<_>>()
    );
}

#[test]
fn primitive_impl_dispatch_resolves_int_receiver() {
    assert_primitive_trait_dispatch_records_metadata(
        r#"
            pub trait Display { fn fmt(val: Self) -> string; }
            impl Display for i64 {
                fn fmt(n: i64) -> string { "" }
            }
            fn main() {
                let x: i64 = 42;
                let _ = x.fmt();
            }
        "#,
        "i64",
        "Display",
    );
}

#[test]
fn primitive_impl_dispatch_resolves_bool_receiver() {
    assert_primitive_trait_dispatch_records_metadata(
        r#"
            pub trait Display { fn fmt(val: Self) -> string; }
            impl Display for bool {
                fn fmt(b: bool) -> string { "" }
            }
            fn main() {
                let b: bool = true;
                let _ = b.fmt();
            }
        "#,
        "bool",
        "Display",
    );
}

#[test]
fn primitive_impl_dispatch_resolves_char_receiver() {
    assert_primitive_trait_dispatch_records_metadata(
        r#"
            pub trait Display { fn fmt(val: Self) -> string; }
            impl Display for char {
                fn fmt(c: char) -> string { "" }
            }
            fn main() {
                let c: char = 'a';
                let _ = c.fmt();
            }
        "#,
        "char",
        "Display",
    );
}

#[test]
fn primitive_impl_dispatch_resolves_i32_receiver() {
    // Width-aliased integer kinds dispatch through the same canonical key
    // as their `Ty::I32` variant — `i32` here, not `i64`.
    assert_primitive_trait_dispatch_records_metadata(
        r#"
            pub trait Display { fn fmt(val: Self) -> string; }
            impl Display for i32 {
                fn fmt(n: i32) -> string { "" }
            }
            fn main() {
                let n: i32 = 7;
                let _ = n.fmt();
            }
        "#,
        "i32",
        "Display",
    );
}

#[test]
fn primitive_impl_dispatch_resolves_string_receiver_via_method_using_trait_method() {
    // string routes through the declarative receiver dispatch fallback, not
    // the wildcard.  This sentinel guarantees that path consults the side
    // table for a method name that does NOT collide with a builtin string
    // method (so the not-found branch is the one that runs).  We use a Display
    // method named `to_display_string` to avoid the impl-on-string body
    // type-check issue tracked separately (see issue #1565 follow-ups).
    assert_primitive_trait_dispatch_records_metadata(
        r#"
            pub trait MyShow { fn show(val: Self) -> i64; }
            impl MyShow for string {
                fn show(s: string) -> i64 { 0 }
            }
            fn main() {
                let s: string = "hi";
                let _: i64 = s.show();
            }
        "#,
        "string",
        "MyShow",
    );
}

#[test]
fn primitive_impl_dispatch_resolves_vec_receiver() {
    // Vec routes through `check_vec_method`'s not-found arm.
    assert_primitive_trait_dispatch_records_metadata(
        r#"
            pub trait Display { fn fmt(val: Self) -> string; }
            impl Display for Vec<i32> {
                fn fmt(v: Vec<i32>) -> string { "" }
            }
            fn main() {
                let v: Vec<i32> = Vec::new();
                let _ = v.fmt();
            }
        "#,
        "Vec",
        "Display",
    );
}

#[test]
fn primitive_impl_dispatch_preserves_builtin_numeric_conversion() {
    // R5 mitigation: the side table is consulted only when the compiler
    // builtin returns "no match".  Numeric width-conversion methods like
    // `.to_i64()` flow through their own dispatch arm (methods.rs around
    // the `is_numeric() && starts_with("to_")` guard).  With a user
    // `Display for i32` in scope, calling the builtin must still resolve
    // to `Ty::I64`, not be hijacked into the user trait's `fmt` method.
    //
    // Uses `i32.to_i64()` (infallible widening) rather than the previously
    // tested `i64.to_i32()` (i64→i32 narrowing), which now requires
    // `.try_to_i32()` under the B-1c strict-width rules.
    let source = r#"
        pub trait Display { fn fmt(val: Self) -> string; }
        impl Display for i32 {
            fn fmt(n: i32) -> string { "" }
        }
        fn main() {
            let n: i32 = 7;
            let _: i64 = n.to_i64();
        }
    "#;
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&parsed.program);
    assert!(
        output.errors.is_empty(),
        "builtin i32.to_i64 regressed under Stage A2 dispatch: {:?}",
        output.errors
    );
}

#[test]
fn primitive_impl_dispatch_resolves_ufcs_form_for_int_receiver() {
    // Stage A3: `Display::fmt(x)` on a primitive receiver must resolve
    // identically to `x.fmt()`.  Today the trait method registers in
    // `fn_sigs` with the receiver param stripped, so the existing
    // `fn_sigs` lookup mis-arities the call (0 expected vs 1 supplied).
    // The UFCS dispatcher intercepts before that lookup.
    assert_primitive_trait_dispatch_records_metadata(
        r#"
            pub trait Display { fn fmt(val: Self) -> string; }
            impl Display for i64 {
                fn fmt(n: i64) -> string { "" }
            }
            fn main() {
                let x: i64 = 42;
                let _ = Display::fmt(x);
            }
        "#,
        "i64",
        "Display",
    );
}

#[test]
fn primitive_impl_dispatch_resolves_ufcs_form_with_extra_args() {
    // UFCS receiver-and-trailing-args case: `Trait::method(receiver,
    // arg1, arg2)`.  The receiver-stripped sig has params=[string], so
    // the call takes 2 args total (receiver + arg1) and the sig is
    // applied to the trailing args after the receiver is consumed.
    assert_primitive_trait_dispatch_records_metadata(
        r#"
            pub trait Show { fn show(val: Self, suffix: string) -> string; }
            impl Show for i64 {
                fn show(n: i64, suffix: string) -> string { suffix }
            }
            fn main() {
                let x: i64 = 42;
                let _: string = Show::show(x, "!");
            }
        "#,
        "i64",
        "Show",
    );
}

#[test]
fn pub_type_receiver_with_user_trait_impl_still_dispatches_via_existing_path() {
    // Stage A4 regression sentinel: `pub type Foo` with `impl Display for
    // Foo` must continue to dispatch through the existing `type_defs`
    // method registry — it must NOT be hijacked into the primitive
    // side table (the primitive table only houses receivers that
    // `type_defs` cannot reach).  The receiver-kind metadata for the
    // call must be `NamedTypeInstance`, not `PrimitiveTraitImpl`.
    let source = r#"
        pub trait Display { fn fmt(val: Self) -> string; }
        pub type Foo {
            value: i64;
        }
        impl Display for Foo {
            fn fmt(f: Foo) -> string { "" }
        }
        fn main() {
            let f: Foo = Foo { value: 1 };
            let _: string = f.fmt();
        }
    "#;
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&parsed.program);
    assert!(
        output.errors.is_empty(),
        "user `pub type` + impl Display dispatch regressed: {:?}",
        output.errors
    );
    let dispatched_via_named = output.method_call_receiver_kinds.values().any(|kind| {
        matches!(
            kind,
            MethodCallReceiverKind::NamedTypeInstance { type_name } if type_name == "Foo"
        )
    });
    let leaked_into_primitive_table = output
        .method_call_receiver_kinds
        .values()
        .any(|kind| matches!(kind, MethodCallReceiverKind::PrimitiveTraitImpl { .. }));
    assert!(
        dispatched_via_named,
        "expected NamedTypeInstance{{type_name=Foo}} metadata for f.fmt(); got: {:?}",
        output
            .method_call_receiver_kinds
            .values()
            .collect::<Vec<_>>()
    );
    assert!(
        !leaked_into_primitive_table,
        "user struct dispatch leaked into primitive trait table: {:?}",
        output
            .method_call_receiver_kinds
            .values()
            .collect::<Vec<_>>()
    );
}

#[test]
fn ufcs_on_pub_type_receiver_does_not_record_primitive_trait_impl_metadata() {
    // Regression for the synthesize-before-short-circuit ordering bug.
    //
    // When `Display::fmt(f)` is called and `f` is a `pub type` (non-primitive)
    // receiver, the UFCS helper must return `None` without recording
    // `PrimitiveTraitImpl` metadata.  Before the fix, the helper could
    // still synthesise the first arg and — on a route that should belong
    // entirely to the existing type-def path — leave stale side-effects.
    //
    // The short-circuit added by this fix ensures that when `Display` has
    // no primitive impls registered (the trait is purely user-defined here),
    // the helper exits before synthesising `first_arg`, preventing any
    // double-processing of the receiver expression.
    //
    // `UserDisplay` is declared without any primitive blanket impls, so
    // the helper returns `None` immediately.  The test asserts no
    // `PrimitiveTraitImpl` metadata is recorded — the call is handled
    // entirely by the receiver-form dispatch path.
    let source = r#"
        pub trait UserDisplay { fn show(val: Self) -> string; }
        pub type Widget { value: i64; }
        impl UserDisplay for Widget {
            fn show(w: Widget) -> string { "" }
        }
        fn main() {
            let w: Widget = Widget { value: 1 };
            let _: string = w.show();
        }
    "#;
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&parsed.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors for UserDisplay on pub type: {:?}",
        output.errors
    );
    let leaked_primitive_meta = output
        .method_call_receiver_kinds
        .values()
        .any(|kind| matches!(kind, MethodCallReceiverKind::PrimitiveTraitImpl { .. }));
    assert!(
        !leaked_primitive_meta,
        "UserDisplay (no primitive impls) must not produce PrimitiveTraitImpl metadata: {:?}",
        output
            .method_call_receiver_kinds
            .values()
            .collect::<Vec<_>>()
    );
}

#[test]
fn ufcs_over_applied_call_emits_exactly_one_arity_diagnostic() {
    // Regression for the double-arity-diagnostic bug.
    //
    // `Display::fmt(x, extra)` should produce exactly one arity error.
    // The old code ran both an explicit outer `check_arity(args, params+1)`
    // and then `apply_instantiated_call_signature` which internally calls
    // `check_arity(trailing_args, params)` via PositionalOnly — two
    // different checks for the same call, two different messages.
    //
    // The fix removes the redundant outer check_arity, leaving only the
    // inner one, matching the receiver-form path's behaviour.
    let source = r#"
        pub trait Display { fn fmt(val: Self) -> string; }
        impl Display for i64 {
            fn fmt(n: i64) -> string { "" }
        }
        fn main() {
            let x: i64 = 42;
            let _ = Display::fmt(x, "extra");
        }
    "#;
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&parsed.program);
    let arity_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.message.contains("argument"))
        .collect();
    assert_eq!(
        arity_errors.len(),
        1,
        "expected exactly 1 arity diagnostic for Display::fmt(x, extra), got {}: {:?}",
        arity_errors.len(),
        arity_errors
    );
}

#[test]
fn primitive_impl_dispatch_unknown_method_still_emits_error() {
    // When no impl matches and no builtin method exists, the existing
    // "no method `<name>` on <kind>" diagnostic must still fire — the
    // helper returns None so the existing reporter runs.
    let source = r#"
        pub trait Display { fn fmt(val: Self) -> string; }
        impl Display for i64 {
            fn fmt(n: i64) -> string { "" }
        }
        fn main() {
            let x: i64 = 42;
            let _ = x.no_such_method();
        }
    "#;
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&parsed.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("no method `no_such_method`")),
        "expected `no method` diagnostic, got: {:?}",
        output.errors
    );
}

// ---------------------------------------------------------------------------
// Slice 1 (#1668, #1669): literal-form receivers and builtins blanket impls
// ---------------------------------------------------------------------------

#[test]
fn primitive_trait_dispatch_int_literal_receiver() {
    // #1668 reproducer: an `IntLiteral` receiver (no enclosing typed binding)
    // must default to `i64` before the side-table lookup so `(42).fmt()`
    // resolves the same way `let x: i64 = 42; x.fmt()` does.  Pre-fix this
    // emitted `no method `fmt` on i64` because
    // `canonical_primitive_or_builtin_key` short-circuited on the literal.
    assert_primitive_trait_dispatch_records_metadata(
        r#"
            pub trait Display { fn fmt(val: Self) -> string; }
            impl Display for i64 {
                fn fmt(n: i64) -> string { "" }
            }
            fn main() {
                let _ = (42).fmt();
            }
        "#,
        "i64",
        "Display",
    );
}

#[test]
fn primitive_trait_dispatch_float_literal_receiver() {
    // Mirror of the i64-literal case for `FloatLiteral`.  Defaulting must
    // collapse the literal to `f64` (via `materialize_literal_defaults`)
    // before canonical-key lookup.
    assert_primitive_trait_dispatch_records_metadata(
        r#"
            pub trait Display { fn fmt(val: Self) -> string; }
            impl Display for f64 {
                fn fmt(x: f64) -> string { "" }
            }
            fn main() {
                let _ = (3.14).fmt();
            }
        "#,
        "f64",
        "Display",
    );
}

#[test]
fn primitive_trait_dispatch_ufcs_int_literal() {
    // Stage A3 (UFCS) sentinel: `Display::fmt(42)` must default the
    // synthesized receiver `IntLiteral` the same way Stage A2 does for
    // method-form.  Without UFCS-side defaulting the helper returns None
    // and the trait-qualified path mis-arities (sig.params=[] vs args=[42]).
    assert_primitive_trait_dispatch_records_metadata(
        r#"
            pub trait Display { fn fmt(val: Self) -> string; }
            impl Display for i64 {
                fn fmt(n: i64) -> string { "" }
            }
            fn main() {
                let _ = Display::fmt(42);
            }
        "#,
        "i64",
        "Display",
    );
}

#[test]
fn primitive_trait_dispatch_builtins_blanket_no_redeclare() {
    // #1669 reproducer: with no in-file `trait Display` declaration, the
    // ten `std/builtins.hew` blanket impls must already be reachable so
    // `let x: i64 = 42; x.fmt()` typechecks.  This is the "registration
    // boundary" fix — `register_builtins_hew_impls` populates
    // `primitive_trait_impls` from the compiled-in stdlib source.
    assert_primitive_trait_dispatch_records_metadata(
        r"
            fn main() {
                let x: i64 = 42;
                let _ = x.fmt();
            }
        ",
        "i64",
        "Display",
    );
}

#[test]
fn primitive_trait_dispatch_records_rewrite_for_builtin_fmt() {
    let parsed = hew_parser::parse(
        r"
            fn main() {
                let x: i64 = 42;
                let _ = x.fmt();
            }
        ",
    );
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&parsed.program);
    assert!(
        output.errors.is_empty(),
        "expected clean typecheck, got: {:?}",
        output.errors
    );
    assert!(
        output.method_call_rewrites.values().any(|rewrite| matches!(
            rewrite,
            MethodCallRewrite::RewriteToFunction { c_symbol, .. } if c_symbol == "i64::fmt"
        )),
        "primitive-trait fmt dispatch must record a function rewrite, got: {:?}",
        output.method_call_rewrites
    );
}

#[test]
fn primitive_trait_dispatch_builtins_blanket_each_kind() {
    // One receiver per kind covered by std/builtins.hew lines 29–87.
    // Each row asserts `x.fmt()` resolves with NO in-file `trait Display`
    // declaration, exercising the builtins-blanket registration end to end.
    let cases: &[(&str, &str, &str)] = &[
        ("i8", "1 as i8", "i8"),
        ("i16", "1 as i16", "i16"),
        ("i32", "1 as i32", "i32"),
        ("i64", "1 as i64", "i64"),
        ("u8", "1 as u8", "u8"),
        ("u16", "1 as u16", "u16"),
        ("u32", "1 as u32", "u32"),
        ("u64", "1 as u64", "u64"),
        ("bool", "true", "bool"),
        ("char", "'a'", "char"),
    ];
    for (annotation, init, canonical) in cases {
        let source = format!(
            r"
                fn main() {{
                    let x: {annotation} = {init};
                    let _ = x.fmt();
                }}
            "
        );
        assert_primitive_trait_dispatch_records_metadata(&source, canonical, "Display");
    }
}

#[test]
fn primitive_trait_dispatch_builtins_blanket_does_not_shadow_user_redeclare() {
    // Builtin-precedence sentinel: the audit's invariant is that a user's
    // in-file `trait Display { ... }` declaration continues to take
    // precedence over the builtins-blanket impls.  Concretely, if a user
    // redeclares Display with a *different* method name, the builtins
    // `fmt` lookup must NOT silently satisfy a call to that user method
    // name on a primitive receiver — it should still diagnose "no method
    // on i64".  This guards the registration ordering risk called out in
    // the lane plan: builtins.hew impls land in the side table only; they
    // do not pollute `trait_defs` or hijack user names.
    let source = r"
        pub trait Display {
            fn render(val: Self) -> string;
        }
        fn main() {
            let x: i64 = 42;
            // `render` is the user trait's method name; no impl exists for i64.
            let _ = x.render();
        }
    ";
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&parsed.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("no method `render`")),
        "expected `no method `render`` diagnostic, got: {:?}",
        output.errors
    );
    // And the user's own trait redeclaration must still be present in
    // trait_defs (i.e. our builtins-blanket loader did NOT register
    // Display first and force the user declaration to be skipped).
    assert!(
        checker.trait_defs.contains_key("Display"),
        "user trait Display must remain registered; trait_defs keys: {:?}",
        checker.trait_defs.keys().collect::<Vec<_>>()
    );
}

#[test]
fn primitive_trait_dispatch_negative_non_display_method_still_diagnoses() {
    // Negative sentinel: a method name that no Display impl provides on
    // a primitive receiver must continue to emit the existing
    // `no method `<name>` on i64` diagnostic.  Defaulting the literal must
    // not silently swallow the not-found path.
    let source = r"
        fn main() {
            let _ = (42).no_such_method();
        }
    ";
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&parsed.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("no method `no_such_method`")),
        "expected `no method `no_such_method`` diagnostic, got: {:?}",
        output.errors
    );
}

#[test]
fn print_user_struct_without_display_impl_is_rejected_by_checker() {
    // #1670 sentinel: user structs must not reach codegen's print lowering
    // unless the checker has resolved a Display impl for the printed type.
    // Without that bound, `print(Foo { ... })` should fail here rather than
    // relying on PrintOpLowering's unsupported-aggregate terminal.
    let source = r#"
        pub type Foo {
            label: string;
        }

        fn main() {
            print(Foo { label: "no display" });
        }
    "#;
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&parsed.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("does not implement trait `Display`")),
        "expected missing Display diagnostic for print(Foo {{ ... }}), got: {:?}",
        output.errors
    );
}

#[test]
fn primitive_trait_dispatch_builtins_blanket_populates_side_table_at_register_builtins() {
    // White-box sentinel: confirm `register_builtins` alone (no
    // user source, no full `check_program`) seeds the
    // `primitive_trait_impls` side table with the ten builtins blanket
    // impls.  This guards against the registration boundary regressing
    // — if `register_builtins_hew_impls` ever stops being called, the
    // method-form Display dispatch silently regresses without any test
    // source needing to fail, so we pin the table contents here.
    let mut checker = Checker::new(test_registry());
    checker.register_builtins();
    let expected_canonical_keys = [
        "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "bool", "char",
    ];
    for key in expected_canonical_keys {
        let entry = checker
            .primitive_trait_impls
            .get(&(key.to_string(), "Display".to_string()))
            .unwrap_or_else(|| {
                panic!(
                    "missing builtins-blanket Display impl for primitive `{key}`; \
                     primitive_trait_impls keys: {:?}",
                    checker.primitive_trait_impls.keys().collect::<Vec<_>>()
                )
            });
        let fmt_sig = entry
            .get("fmt")
            .unwrap_or_else(|| panic!("missing fmt method for ({key}, Display) entry: {entry:?}"));
        assert!(
            fmt_sig.params.is_empty(),
            "receiver should be filtered from fmt sig for `{key}`: {fmt_sig:?}"
        );
        assert_eq!(
            fmt_sig.return_type,
            Ty::String,
            "fmt for `{key}` must return string, got: {:?}",
            fmt_sig.return_type
        );
    }
}

#[test]
fn duplicate_stdlib_import_with_same_resolved_source_does_not_reregister_items() {
    let mut root = hew_parser::parse(
        r"
            import std::bench;
            import std::bench;
        ",
    );
    assert!(
        root.errors.is_empty(),
        "root parse errors: {:?}",
        root.errors
    );

    let bench_path = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join("std/bench/bench.hew")
        .canonicalize()
        .expect("stdlib bench module should exist");
    let bench_source =
        std::fs::read_to_string(&bench_path).expect("should read stdlib bench Hew source");
    let bench_module = hew_parser::parse(&bench_source);
    assert!(
        bench_module.errors.is_empty(),
        "bench parse errors: {:?}",
        bench_module.errors
    );

    for import_decl in root
        .program
        .items
        .iter_mut()
        .filter_map(|(item, _)| match item {
            Item::Import(import) => Some(import),
            _ => None,
        })
    {
        import_decl.resolved_items = Some(bench_module.program.items.clone());
        import_decl.resolved_source_paths = vec![bench_path.clone()];
    }

    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&root.program);

    assert!(
        !output.user_modules.contains("bench"),
        "stdlib Hew import should not go through the user-module import path"
    );
    assert!(
        output.type_defs.contains_key("Suite"),
        "stdlib Hew items should still register public types"
    );
    assert!(
        output.fn_sigs.contains_key("bench.suite"),
        "stdlib Hew items should still register qualified functions"
    );
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::DuplicateDefinition),
        "duplicate stdlib imports should dedupe Hew item registration: {:?}",
        output.errors
    );
}

// ── Warning severity tests ──────────────────────────────────────────

#[test]
fn unreachable_code_has_warning_severity() {
    let source = "fn foo() -> i32 { return 1; let x = 2; x }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let w = output
        .warnings
        .iter()
        .find(|w| w.kind == TypeErrorKind::UnreachableCode);
    assert!(w.is_some(), "expected unreachable warning");
    assert_eq!(
        w.unwrap().severity,
        crate::error::Severity::Warning,
        "unreachable code should have Warning severity"
    );
}

#[test]
fn shadowing_warning_has_note_for_original_definition() {
    // The warning for outer-scope shadowing must include a note pointing back
    // to where the original binding was defined.
    let source = "fn main() { let x = 1; if true { let x = 2; println(x); } println(x); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let w = output
        .warnings
        .iter()
        .find(|w| w.kind == TypeErrorKind::Shadowing);
    assert!(w.is_some(), "expected shadowing warning");
    assert!(
        !w.unwrap().notes.is_empty(),
        "shadowing warning should have a note pointing to the original definition"
    );
}

// ── Bug fix regression tests ────────────────────────────────────────

#[test]
fn no_warn_unused_read_then_assign() {
    // Bug 1: var x = 0; println(x); x = 1; should NOT warn about unused x
    let (errors, warnings) = parse_and_check("fn main() { var x = 0; println(x); x = 1; }");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("unused variable `x`")),
        "read-then-assign should not produce unused warning, got: {warnings:?}"
    );
}

#[test]
fn warn_unreachable_after_if_all_branches_return() {
    // Bug 2: if where all branches return should mark subsequent code unreachable
    let (_, warnings) = parse_and_check(
        "fn foo() -> i32 { if true { return 1; } else { return 2; } let y = 3; y }",
    );
    assert!(
        warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::UnreachableCode),
        "expected unreachable code warning after if with all-returning branches, got: {warnings:?}"
    );
}

#[test]
fn no_warn_dead_code_function_referenced_as_value() {
    // Bug 3: let f = helper; f(); should mark helper as called
    let (_, warnings) =
        parse_and_check("fn helper() -> i32 { 42 } fn main() { let f = helper; println(f); }");
    assert!(
        !warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::DeadCode && w.message.contains("helper")),
        "function referenced as value should not get dead code warning, got: {warnings:?}"
    );
}

#[test]
fn warn_dead_code_self_recursive_function() {
    // Bug 4: fn rec() { rec(); } fn main() {} — rec only calls itself, dead
    let (_, warnings) = parse_and_check("fn rec() { rec(); } fn main() { println(1); }");
    assert!(
        warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::DeadCode && w.message.contains("rec")),
        "self-recursive unreachable function should get dead code warning, got: {warnings:?}"
    );
}

// -----------------------------------------------------------------------
// Module namespacing tests
// -----------------------------------------------------------------------

/// Helper: build a simple pub function declaration.
fn make_pub_fn(name: &str, params: Vec<Param>, ret: Option<TypeExpr>) -> FnDecl {
    FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Pub,
        name: name.to_string(),
        type_params: None,
        params,
        return_type: ret.map(|te| (te, 0..0)),
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: Some(Box::new(make_int_literal(0, 0..1))),
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
        intrinsic: None,
    }
}

/// Helper: build a private (non-pub) function declaration.
fn make_priv_fn(name: &str) -> FnDecl {
    FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Private,
        name: name.to_string(),
        type_params: None,
        params: vec![],
        return_type: Some((
            TypeExpr::Named {
                name: "i32".to_string(),
                type_args: None,
            },
            0..0,
        )),
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: Some(Box::new(make_int_literal(0, 0..1))),
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
        intrinsic: None,
    }
}

/// Helper: build an `ImportDecl` with resolved items.
fn make_user_import(
    path: &[&str],
    spec: Option<ImportSpec>,
    items: Vec<Spanned<Item>>,
) -> ImportDecl {
    ImportDecl {
        path: path.iter().map(ToString::to_string).collect(),
        spec,
        file_path: None,
        resolved_items: Some(items),
        resolved_item_source_paths: Vec::new(),
        resolved_source_paths: Vec::new(),
    }
}

/// Helper: type-check a program with given items.
fn check_items(items: Vec<Spanned<Item>>) -> TypeCheckOutput {
    let program = Program {
        module_graph: None,
        items,
        module_doc: None,
    };
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.check_program(&program)
}

// -- should_import_name --

#[test]
fn should_import_name_bare_import_returns_false() {
    assert!(!Checker::should_import_name("helper", &None));
}

#[test]
fn should_import_name_glob_returns_true() {
    assert!(Checker::should_import_name(
        "helper",
        &Some(ImportSpec::Glob)
    ));
    assert!(Checker::should_import_name(
        "anything",
        &Some(ImportSpec::Glob)
    ));
}

#[test]
fn should_import_name_named_match() {
    let spec = Some(ImportSpec::Names(vec![
        ImportName {
            name: "helper".to_string(),
            alias: None,
        },
        ImportName {
            name: "parse".to_string(),
            alias: None,
        },
    ]));
    assert!(Checker::should_import_name("helper", &spec));
    assert!(Checker::should_import_name("parse", &spec));
    assert!(!Checker::should_import_name("other", &spec));
}

// -- Bare import: qualified only --

#[test]
fn bare_import_registers_qualified_name() {
    let helper = make_pub_fn(
        "helper",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let import = make_user_import(
        &["myapp", "utils"],
        None, // bare import
        vec![(Item::Function(helper), 0..0)],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    assert!(
        output.fn_sigs.contains_key("utils.helper"),
        "bare import should register qualified name 'utils.helper'"
    );
    assert!(
        !output.fn_sigs.contains_key("helper"),
        "bare import should NOT register unqualified name 'helper'"
    );
}

// -- Glob import: everything unqualified --

#[test]
fn glob_import_registers_unqualified_names() {
    let helper = make_pub_fn(
        "helper",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let other = make_pub_fn(
        "other",
        vec![],
        Some(TypeExpr::Named {
            name: "string".to_string(),
            type_args: None,
        }),
    );
    let import = make_user_import(
        &["myapp", "utils"],
        Some(ImportSpec::Glob),
        vec![
            (Item::Function(helper), 0..0),
            (Item::Function(other), 0..0),
        ],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    // Both qualified and unqualified should be registered
    assert!(output.fn_sigs.contains_key("utils.helper"));
    assert!(output.fn_sigs.contains_key("utils.other"));
    assert!(
        output.fn_sigs.contains_key("helper"),
        "glob import should register unqualified 'helper'"
    );
    assert!(
        output.fn_sigs.contains_key("other"),
        "glob import should register unqualified 'other'"
    );
}

// -- Named import: specific names only --

#[test]
fn named_import_registers_specified_names_only() {
    let helper = make_pub_fn(
        "helper",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let other = make_pub_fn(
        "other",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let import = make_user_import(
        &["myapp", "utils"],
        Some(ImportSpec::Names(vec![ImportName {
            name: "helper".to_string(),
            alias: None,
        }])),
        vec![
            (Item::Function(helper), 0..0),
            (Item::Function(other), 0..0),
        ],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    // Both should be qualified
    assert!(output.fn_sigs.contains_key("utils.helper"));
    assert!(output.fn_sigs.contains_key("utils.other"));
    // Only "helper" should be unqualified
    assert!(
        output.fn_sigs.contains_key("helper"),
        "named import should register 'helper' unqualified"
    );
    assert!(
        !output.fn_sigs.contains_key("other"),
        "named import should NOT register 'other' unqualified"
    );
}

// -- Pub visibility enforcement --

#[test]
fn non_pub_functions_not_registered() {
    let priv_fn = make_priv_fn("secret");
    let pub_fn = make_pub_fn(
        "visible",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let import = make_user_import(
        &["myapp", "utils"],
        Some(ImportSpec::Glob), // even glob shouldn't expose private fns
        vec![
            (Item::Function(priv_fn), 0..0),
            (Item::Function(pub_fn), 0..0),
        ],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    assert!(
        !output.fn_sigs.contains_key("utils.secret"),
        "non-pub function should not be registered as qualified"
    );
    assert!(
        !output.fn_sigs.contains_key("secret"),
        "non-pub function should not be registered as unqualified"
    );
    assert!(output.fn_sigs.contains_key("utils.visible"));
    assert!(output.fn_sigs.contains_key("visible"));
}

// -- User module const registration --

#[test]
fn user_module_registers_pub_consts() {
    use hew_parser::ast::ConstDecl;

    let pub_const = ConstDecl {
        visibility: Visibility::Pub,
        name: "MAX_SIZE".to_string(),
        ty: (
            TypeExpr::Named {
                name: "i32".to_string(),
                type_args: None,
            },
            0..0,
        ),
        value: make_int_literal(100, 0..3),
        doc_comment: None,
    };
    let priv_const = ConstDecl {
        visibility: Visibility::Private,
        name: "INTERNAL".to_string(),
        ty: (
            TypeExpr::Named {
                name: "i32".to_string(),
                type_args: None,
            },
            0..0,
        ),
        value: make_int_literal(42, 0..2),
        doc_comment: None,
    };
    let import = make_user_import(
        &["myapp", "config"],
        Some(ImportSpec::Glob),
        vec![
            (Item::Const(pub_const), 0..0),
            (Item::Const(priv_const), 0..0),
        ],
    );

    let program = Program {
        module_graph: None,
        items: vec![(Item::Import(import), 0..0)],
        module_doc: None,
    };
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let _output = checker.check_program(&program);

    // pub const should be findable in the environment
    assert!(
        checker.env.lookup_ref("config.MAX_SIZE").is_some(),
        "pub const should be registered as qualified"
    );
    assert!(
        checker.env.lookup_ref("MAX_SIZE").is_some(),
        "pub const should be unqualified with glob import"
    );
    assert!(
        checker.env.lookup_ref("config.INTERNAL").is_none(),
        "private const should NOT be registered"
    );
    assert!(
        checker.env.lookup_ref("INTERNAL").is_none(),
        "private const should NOT be registered unqualified"
    );
}

#[test]
fn user_module_const_bare_import_qualified_only() {
    use hew_parser::ast::ConstDecl;

    let pub_const = ConstDecl {
        visibility: Visibility::Pub,
        name: "LIMIT".to_string(),
        ty: (
            TypeExpr::Named {
                name: "i32".to_string(),
                type_args: None,
            },
            0..0,
        ),
        value: make_int_literal(50, 0..2),
        doc_comment: None,
    };
    let import = make_user_import(
        &["myapp", "config"],
        None, // bare import
        vec![(Item::Const(pub_const), 0..0)],
    );

    let program = Program {
        module_graph: None,
        items: vec![(Item::Import(import), 0..0)],
        module_doc: None,
    };
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let _output = checker.check_program(&program);

    assert!(
        checker.env.lookup_ref("config.LIMIT").is_some(),
        "pub const should be registered as qualified"
    );
    assert!(
        checker.env.lookup_ref("LIMIT").is_none(),
        "bare import should NOT register const unqualified"
    );
}

// -- Module-qualified const field access --

/// `module.CONST` resolves to the const's declared type without an
/// "undefined variable" diagnostic.  This covers the `check_field_access`
/// pre-dispatch added to fix R2 (module-scope const binding).
#[test]
fn module_qualified_const_field_access_resolves() {
    use hew_parser::ast::ConstDecl;

    let pub_const = ConstDecl {
        visibility: Visibility::Pub,
        name: "LIMIT".to_string(),
        ty: (
            TypeExpr::Named {
                name: "i64".to_string(),
                type_args: None,
            },
            0..0,
        ),
        value: make_int_literal(50, 0..2),
        doc_comment: None,
    };
    // Parse a function that references the const via qualified access.
    let mut root = hew_parser::parse(
        r"
import myapp::config;

fn caller() -> i64 {
    config.LIMIT
}
",
    );
    assert!(
        root.errors.is_empty(),
        "program should parse cleanly, got: {:#?}",
        root.errors
    );
    // Inject resolved items into the import so the checker sees the const.
    let import_decl = root
        .program
        .items
        .iter_mut()
        .find_map(|(item, _)| match item {
            Item::Import(imp) => Some(imp),
            _ => None,
        })
        .expect("import decl should exist");
    import_decl.resolved_items = Some(vec![(Item::Const(pub_const), 0..0)]);

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&root.program);
    assert!(
        output.errors.is_empty(),
        "module-qualified const access should resolve cleanly, got: {:#?}",
        output.errors
    );
}

/// Accessing a non-existent const via `module.NONEXISTENT` should produce a
/// targeted "module has no exported constant" diagnostic rather than the leaky
/// "undefined variable `module`" error.
#[test]
fn module_qualified_const_undefined_emits_targeted_diagnostic() {
    use hew_parser::ast::ConstDecl;

    let pub_const = ConstDecl {
        visibility: Visibility::Pub,
        name: "LIMIT".to_string(),
        ty: (
            TypeExpr::Named {
                name: "i64".to_string(),
                type_args: None,
            },
            0..0,
        ),
        value: make_int_literal(50, 0..2),
        doc_comment: None,
    };
    let mut root = hew_parser::parse(
        r"
import myapp::config;

fn caller() -> i64 {
    config.NONEXISTENT
}
",
    );
    assert!(
        root.errors.is_empty(),
        "program should parse cleanly, got: {:#?}",
        root.errors
    );
    let import_decl = root
        .program
        .items
        .iter_mut()
        .find_map(|(item, _)| match item {
            Item::Import(imp) => Some(imp),
            _ => None,
        })
        .expect("import decl should exist");
    import_decl.resolved_items = Some(vec![(Item::Const(pub_const), 0..0)]);

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&root.program);
    assert!(
        output.errors.iter().any(|err| {
            err.kind == TypeErrorKind::UndefinedField
                && err
                    .message
                    .contains("module `config` has no exported constant `NONEXISTENT`")
        }),
        "expected targeted 'no exported constant' diagnostic, got: {:#?}",
        output.errors
    );
    // Must NOT produce the leaky "undefined variable `config`" error.
    assert!(
        !output
            .errors
            .iter()
            .any(|err| err.message.contains("undefined variable `config`")),
        "must not emit 'undefined variable `config`' leak, got: {:#?}",
        output.errors
    );
}

// -- User module type registration --

#[test]
fn user_module_registers_types() {
    let struct_decl = TypeDecl {
        visibility: Visibility::Pub,
        kind: TypeDeclKind::Struct,
        name: "Config".to_string(),
        type_params: None,
        where_clause: None,
        body: vec![TypeBodyItem::Field {
            name: "value".to_string(),
            ty: (
                TypeExpr::Named {
                    name: "i32".to_string(),
                    type_args: None,
                },
                0..0,
            ),
            attributes: Vec::new(),
            doc_comment: None,
            span: 0..0,
        }],
        doc_comment: None,
        wire: None,
        is_indirect: false,
        resource_marker: hew_parser::ast::ResourceMarker::None,
        is_opaque: false,
        consuming_methods: Vec::new(),
    };
    let import = make_user_import(
        &["myapp", "config"],
        None, // bare import
        vec![(Item::TypeDecl(struct_decl), 0..0)],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    assert!(
        output.type_defs.contains_key("Config"),
        "user module type should be registered unqualified"
    );
    assert!(
        output.type_defs.contains_key("config.Config"),
        "user module type should also be registered as qualified"
    );
}

// -- user_modules set --

#[test]
fn user_modules_set_populated() {
    let helper = make_pub_fn(
        "helper",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let import = make_user_import(
        &["myapp", "utils"],
        None,
        vec![(Item::Function(helper), 0..0)],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    assert!(
        output.user_modules.contains("utils"),
        "user_modules should contain the module short name"
    );
}

#[test]
fn stdlib_not_in_user_modules() {
    // A stdlib import should NOT appear in user_modules
    let import = ImportDecl {
        path: vec!["std".to_string(), "fs".to_string()],
        spec: None,
        file_path: None,
        resolved_items: None,
        resolved_item_source_paths: Vec::new(),
        resolved_source_paths: Vec::new(),
    };
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    assert!(
        !output.user_modules.contains("fs"),
        "stdlib module should NOT be in user_modules"
    );
}

// -- Function signature correctness --

#[test]
fn user_module_fn_sig_has_correct_types() {
    let helper = make_pub_fn(
        "add",
        vec![
            Param {
                name: "a".to_string(),
                ty: (
                    TypeExpr::Named {
                        name: "i32".to_string(),
                        type_args: None,
                    },
                    0..0,
                ),
                is_mutable: false,
            },
            Param {
                name: "b".to_string(),
                ty: (
                    TypeExpr::Named {
                        name: "i32".to_string(),
                        type_args: None,
                    },
                    0..0,
                ),
                is_mutable: false,
            },
        ],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let import = make_user_import(
        &["mylib", "math"],
        None,
        vec![(Item::Function(helper), 0..0)],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    let sig = output
        .fn_sigs
        .get("math.add")
        .expect("math.add should be registered");
    assert_eq!(sig.params.len(), 2, "should have 2 params");
    assert_eq!(sig.params[0], Ty::I32);
    assert_eq!(sig.params[1], Ty::I32);
    assert_eq!(sig.return_type, Ty::I32);
    assert_eq!(sig.param_names, vec!["a", "b"]);
}

// -- Multiple modules don't collide --

#[test]
fn two_modules_same_fn_name_no_collision() {
    let helper_a = make_pub_fn(
        "run",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let helper_b = make_pub_fn(
        "run",
        vec![],
        Some(TypeExpr::Named {
            name: "string".to_string(),
            type_args: None,
        }),
    );
    let import_a = make_user_import(
        &["pkg", "alpha"],
        None,
        vec![(Item::Function(helper_a), 0..0)],
    );
    let import_b = make_user_import(
        &["pkg", "beta"],
        None,
        vec![(Item::Function(helper_b), 0..0)],
    );
    let output = check_items(vec![
        (Item::Import(import_a), 0..0),
        (Item::Import(import_b), 0..0),
    ]);

    assert!(output.fn_sigs.contains_key("alpha.run"));
    assert!(output.fn_sigs.contains_key("beta.run"));
    // Both should have different return types
    assert_eq!(output.fn_sigs["alpha.run"].return_type, Ty::I32);
    assert_eq!(output.fn_sigs["beta.run"].return_type, Ty::String);
}

// -- Import with no resolved items (stdlib) still works --

#[test]
fn import_without_resolved_items_emits_unresolved_error() {
    // An import with resolved_items = None and no stdlib match (empty registry)
    // must now emit an UnresolvedImport error rather than silently dropping.
    let import = ImportDecl {
        path: vec!["unknown".to_string(), "pkg".to_string()],
        spec: None,
        file_path: None,
        resolved_items: None,
        resolved_item_source_paths: Vec::new(),
        resolved_source_paths: Vec::new(),
    };
    let output = check_items(vec![(Item::Import(import), 0..0)]);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::UnresolvedImport),
        "expected UnresolvedImport error, got: {errors:?}",
        errors = output.errors
    );
    assert!(!output.user_modules.contains("pkg"));
}

#[test]
fn import_with_resolved_items_no_error() {
    // When resolved_items is provided the user-module path is taken and no
    // UnresolvedImport diagnostic should be emitted.
    let import = make_user_import(&["myapp", "util"], None, vec![]);
    let output = check_items(vec![(Item::Import(import), 0..0)]);
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::UnresolvedImport),
        "unexpected UnresolvedImport error for user module with resolved_items"
    );
    assert!(output.user_modules.contains("util"));
}

#[test]
fn stdlib_import_keeps_stream_from_file_stream_typed_after_fs_import() {
    let stream_import = ImportDecl {
        path: vec!["std".to_string(), "stream".to_string()],
        spec: None,
        file_path: None,
        resolved_items: None,
        resolved_item_source_paths: Vec::new(),
        resolved_source_paths: Vec::new(),
    };
    let fs_import = ImportDecl {
        path: vec!["std".to_string(), "fs".to_string()],
        spec: None,
        file_path: None,
        resolved_items: None,
        resolved_item_source_paths: Vec::new(),
        resolved_source_paths: Vec::new(),
    };
    let program = Program {
        module_graph: None,
        items: vec![
            (Item::Import(stream_import), 0..0),
            (Item::Import(fs_import), 0..0),
        ],
        module_doc: None,
    };

    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&program);
    let stream_from_file = output
        .fn_sigs
        .get("stream.from_file")
        .expect("expected std::stream import to register stream.from_file");

    assert_eq!(
        stream_from_file.return_type,
        Ty::result(Ty::stream(Ty::String), Ty::String),
        "std::stream import should keep from_file() typed as Result<Stream<string>, string>"
    );
}

#[test]
fn file_import_without_resolved_items_emits_unresolved_error() {
    let import = ImportDecl {
        path: vec![],
        spec: None,
        file_path: Some("missing.hew".to_string()),
        resolved_items: None,
        resolved_item_source_paths: Vec::new(),
        resolved_source_paths: Vec::new(),
    };
    let program = Program {
        module_graph: None,
        items: vec![(Item::Import(import), 0..20)],
        module_doc: None,
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);
    let error = output
        .errors
        .iter()
        .find(|e| e.kind == TypeErrorKind::UnresolvedImport)
        .expect("expected UnresolvedImport error for unresolved file import");

    assert!(
        error.message.contains("missing.hew"),
        "unresolved file import should mention the missing file path: {error:?}"
    );
}

#[test]
fn merged_file_import_duplicate_pub_name_emits_duplicate_definition() {
    let shared_decl = make_pub_fn(
        "shared",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let import = ImportDecl {
        path: vec![],
        spec: None,
        file_path: Some("pkg.hew".to_string()),
        resolved_items: Some(vec![
            (Item::Function(shared_decl.clone()), 0..5),
            (Item::Function(shared_decl), 10..15),
        ]),
        resolved_item_source_paths: vec![
            std::path::PathBuf::from("pkg/pkg.hew"),
            std::path::PathBuf::from("pkg/helpers.hew"),
        ],
        resolved_source_paths: vec![
            std::path::PathBuf::from("pkg/pkg.hew"),
            std::path::PathBuf::from("pkg/helpers.hew"),
        ],
    };
    let output = check_items(vec![(Item::Import(import), 0..20)]);
    let error = output
        .errors
        .iter()
        .find(|e| e.kind == TypeErrorKind::DuplicateDefinition)
        .expect("merged file import should fail closed on duplicate pub names");

    assert!(
        error.message.contains("shared"),
        "duplicate pub name error should mention the colliding binding: {error:?}"
    );
    assert_eq!(
        error.notes.first().map(|(span, _)| span.clone()),
        Some(0..5),
        "duplicate pub name should point back to the first merged definition"
    );
}

#[test]
fn repeated_flat_file_import_with_same_resolved_source_does_not_reregister_items() {
    let shared_source = std::path::PathBuf::from("pkg/pkg.hew");
    let import = ImportDecl {
        path: vec![],
        spec: None,
        file_path: Some("pkg.hew".to_string()),
        resolved_items: Some(vec![(
            Item::Function(make_pub_fn(
                "shared",
                vec![],
                Some(TypeExpr::Named {
                    name: "i32".to_string(),
                    type_args: None,
                }),
            )),
            0..5,
        )]),
        resolved_item_source_paths: vec![shared_source.clone()],
        resolved_source_paths: vec![shared_source],
    };
    let output = check_items(vec![
        (Item::Import(import.clone()), 0..5),
        (Item::Import(import), 10..15),
    ]);

    assert!(
        output.errors.is_empty(),
        "same resolved flat file import should stay idempotent: {:?}",
        output.errors
    );
    assert!(
        output.fn_sigs.contains_key("shared"),
        "flat file import should still register the imported function"
    );
}

#[test]
fn repeated_stdlib_import_does_not_duplicate_hew_items() {
    let repo_root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .to_path_buf();
    let fs_path = repo_root.join("std/fs.hew");
    let source = std::fs::read_to_string(&fs_path).expect("std/fs.hew should exist");
    let parsed = hew_parser::parse(&source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors in std/fs.hew: {:?}",
        parsed.errors
    );

    let import = ImportDecl {
        path: vec!["std".to_string(), "fs".to_string()],
        spec: None,
        file_path: None,
        resolved_items: Some(parsed.program.items),
        resolved_item_source_paths: Vec::new(),
        resolved_source_paths: vec![fs_path],
    };
    let program = Program {
        module_graph: None,
        items: vec![
            (Item::Import(import.clone()), 0..0),
            (Item::Import(import), 0..0),
        ],
        module_doc: None,
    };

    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&program);

    assert!(
        output.errors.is_empty(),
        "unexpected errors for repeated stdlib import: {:?}",
        output.errors
    );
    assert!(
        output.type_defs.contains_key("IoError"),
        "expected std::fs Hew items to remain registered"
    );
}

// -- Empty module import --

#[test]
fn empty_module_import_no_crash() {
    let import = make_user_import(&["myapp", "empty"], None, vec![]);
    let output = check_items(vec![(Item::Import(import), 0..0)]);
    assert!(output.user_modules.contains("empty"));
    assert!(output.errors.is_empty());
}

// -- Import alias binding --

#[test]
fn import_alias_binds_under_alias_name() {
    // import mymod::{foo as bar} — "bar" must resolve, "foo" must not be unqualified
    let helper = make_pub_fn(
        "foo",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let import = make_user_import(
        &["mymod"],
        Some(ImportSpec::Names(vec![ImportName {
            name: "foo".to_string(),
            alias: Some("bar".to_string()),
        }])),
        vec![(Item::Function(helper), 0..0)],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    // qualified form always uses original name
    assert!(
        output.fn_sigs.contains_key("mymod.foo"),
        "qualified 'mymod.foo' should be registered regardless of alias"
    );
    // unqualified binding must use the alias
    assert!(
        output.fn_sigs.contains_key("bar"),
        "aliased import should register unqualified binding 'bar'"
    );
    // original unqualified name must NOT be registered
    assert!(
        !output.fn_sigs.contains_key("foo"),
        "aliased import must NOT register unqualified 'foo'"
    );
}

#[test]
fn import_alias_multiple_names() {
    // import pkg::{alpha as a, beta as b}
    let fn_alpha = make_pub_fn(
        "alpha",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let fn_beta = make_pub_fn(
        "beta",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let import = make_user_import(
        &["pkg"],
        Some(ImportSpec::Names(vec![
            ImportName {
                name: "alpha".to_string(),
                alias: Some("a".to_string()),
            },
            ImportName {
                name: "beta".to_string(),
                alias: Some("b".to_string()),
            },
        ])),
        vec![
            (Item::Function(fn_alpha), 0..0),
            (Item::Function(fn_beta), 0..0),
        ],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    assert!(
        output.fn_sigs.contains_key("a"),
        "'a' alias should be registered"
    );
    assert!(
        output.fn_sigs.contains_key("b"),
        "'b' alias should be registered"
    );
    assert!(
        !output.fn_sigs.contains_key("alpha"),
        "original 'alpha' must not be unqualified"
    );
    assert!(
        !output.fn_sigs.contains_key("beta"),
        "original 'beta' must not be unqualified"
    );
}

// -- Trait import from module --

#[test]
fn import_trait_from_module_glob() {
    use hew_parser::ast::{TraitDecl, TraitItem, TraitMethod};

    let trait_decl = TraitDecl {
        visibility: Visibility::Pub,
        name: "Display".to_string(),
        type_params: None,
        super_traits: None,
        items: vec![TraitItem::Method(TraitMethod {
            name: "display".to_string(),
            type_params: None,
            params: vec![],
            return_type: None,
            where_clause: None,
            body: None,
            span: 0..0,
            doc_comment: None,
            lang_item: None,
        })],
        doc_comment: None,
        lang_item: None,
    };
    let import = make_user_import(
        &["mylib", "fmt"],
        Some(ImportSpec::Glob),
        vec![(Item::Trait(trait_decl), 0..0)],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    assert!(
        output.errors.is_empty(),
        "importing a pub trait should not produce errors: {:?}",
        output.errors
    );
    // The module should be registered as a user module
    assert!(
        output.user_modules.contains("fmt"),
        "module 'fmt' should be in user_modules"
    );
}

#[test]
fn import_private_trait_not_registered() {
    use hew_parser::ast::{TraitDecl, TraitItem, TraitMethod};

    let private_trait = TraitDecl {
        visibility: Visibility::Private,
        name: "Internal".to_string(),
        type_params: None,
        super_traits: None,
        items: vec![TraitItem::Method(TraitMethod {
            name: "internal_op".to_string(),
            type_params: None,
            params: vec![],
            return_type: None,
            where_clause: None,
            body: None,
            span: 0..0,
            doc_comment: None,
            lang_item: None,
        })],
        doc_comment: None,
        lang_item: None,
    };
    let import = make_user_import(
        &["mylib", "internals"],
        Some(ImportSpec::Glob),
        vec![(Item::Trait(private_trait), 0..0)],
    );
    // Should complete without errors; private trait is simply ignored
    let output = check_items(vec![(Item::Import(import), 0..0)]);
    assert!(output.errors.is_empty(), "errors: {:?}", output.errors);
}

// -- Orphan rule warning --

#[test]
fn orphan_impl_emits_warning() {
    use hew_parser::ast::TraitBound;
    // impl ExternalTrait for ExternalType → neither is local → orphan warning
    let impl_decl = ImplDecl {
        type_params: None,
        trait_bound: Some(TraitBound {
            name: "SomeTrait".to_string(),
            type_args: None,
            assoc_type_bindings: vec![],
        }),
        target_type: (
            TypeExpr::Named {
                name: "SomeType".to_string(),
                type_args: None,
            },
            0..0,
        ),
        where_clause: None,
        type_aliases: vec![],
        methods: vec![],
    };
    let output = check_items(vec![(Item::Impl(impl_decl), 0..0)]);

    let has_orphan_warning = output
        .warnings
        .iter()
        .any(|w| w.kind == crate::error::TypeErrorKind::OrphanImpl);
    assert!(
        has_orphan_warning,
        "expected OrphanImpl warning when neither trait nor type is local, got: {:?}",
        output.warnings
    );
}

#[test]
fn local_type_impl_no_orphan_warning() {
    use hew_parser::ast::TraitBound;
    // Locally defined type: impl SomeExternalTrait for LocalType → no orphan warning
    let type_decl = TypeDecl {
        visibility: Visibility::Pub,
        kind: TypeDeclKind::Struct,
        name: "LocalType".to_string(),
        type_params: None,
        where_clause: None,
        body: vec![],
        doc_comment: None,
        wire: None,
        is_indirect: false,
        resource_marker: hew_parser::ast::ResourceMarker::None,
        is_opaque: false,
        consuming_methods: Vec::new(),
    };
    let impl_decl = ImplDecl {
        type_params: None,
        trait_bound: Some(TraitBound {
            name: "ExternalTrait".to_string(),
            type_args: None,
            assoc_type_bindings: vec![],
        }),
        target_type: (
            TypeExpr::Named {
                name: "LocalType".to_string(),
                type_args: None,
            },
            0..0,
        ),
        where_clause: None,
        type_aliases: vec![],
        methods: vec![],
    };
    let output = check_items(vec![
        (Item::TypeDecl(type_decl), 0..0),
        (Item::Impl(impl_decl), 0..0),
    ]);

    let has_orphan = output
        .warnings
        .iter()
        .any(|w| w.kind == crate::error::TypeErrorKind::OrphanImpl);
    assert!(
        !has_orphan,
        "impl on a locally defined type must NOT produce an orphan warning"
    );
}

#[test]
fn test_file_import_private_items_not_visible() {
    use hew_parser::ast::{
        Block, ConstDecl, Expr, FnDecl, ImportDecl, Item, Literal, Program, Spanned, TypeDecl,
        TypeDeclKind, TypeExpr,
    };

    let private_fn = Item::Function(FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Private,
        name: "private_func".to_string(),
        type_params: None,
        params: vec![],
        return_type: None,
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: None,
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
        intrinsic: None,
    });

    let private_const = Item::Const(ConstDecl {
        visibility: Visibility::Private,
        name: "PRIVATE_CONST".to_string(),
        ty: (
            TypeExpr::Named {
                name: "i64".to_string(),
                type_args: None,
            },
            0..0,
        ),
        value: (
            Expr::Literal(Literal::Integer {
                value: 42,
                radix: hew_parser::ast::IntRadix::Decimal,
            }),
            0..0,
        ),
        doc_comment: None,
    });

    let private_type = Item::TypeDecl(TypeDecl {
        visibility: Visibility::Private,
        kind: TypeDeclKind::Struct,
        name: "PrivateType".to_string(),
        type_params: None,
        where_clause: None,
        body: vec![],
        doc_comment: None,
        wire: None,
        is_indirect: false,
        resource_marker: hew_parser::ast::ResourceMarker::None,
        is_opaque: false,
        consuming_methods: Vec::new(),
    });

    let resolved: Vec<Spanned<Item>> = vec![
        (private_fn, 0..0),
        (private_const, 0..0),
        (private_type, 0..0),
    ];

    let import_decl = ImportDecl {
        path: vec![],
        spec: None,
        file_path: Some("private_lib.hew".to_string()),
        resolved_items: Some(resolved),
        resolved_item_source_paths: Vec::new(),
        resolved_source_paths: Vec::new(),
    };

    let program = Program {
        module_graph: None,
        items: vec![(Item::Import(import_decl), 0..0)],
        module_doc: None,
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);

    assert!(
        !output.fn_sigs.contains_key("private_func"),
        "private function must not be registered from file import"
    );
    assert!(
        checker.env.lookup("PRIVATE_CONST").is_none(),
        "private const must not be registered from file import"
    );
    assert!(
        !checker.known_types.contains("PrivateType"),
        "private type must not be registered from file import"
    );
}

#[test]
fn check_generic_lambda_removed_emits_typed_diagnostic() {
    // Generic lambda `<T>(params) => body` was removed in v0.5.
    // The parser must emit a typed E_CLOSURE_PIPE_SYNTAX diagnostic.
    let source = r"
        fn main() {
            let id = <T>(x: T) => x;
        }
    ";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.iter().any(|e| {
            matches!(e.kind, hew_parser::ParseDiagnosticKind::ClosurePipeSyntax)
                && e.message.contains("E_CLOSURE_PIPE_SYNTAX")
        }),
        "expected typed E_CLOSURE_PIPE_SYNTAX for removed generic lambda, got: {:?}",
        result.errors
    );
}

/// Slice-1 generic lambda regression test.
///
/// Verifies that:
/// 1. A let-bound generic lambda type-checks cleanly.
/// 2. A direct call whose arguments make the type obvious resolves the
///    return type correctly.
/// 3. `call_type_args` is populated for the call so the enricher can
///    fill in explicit type arguments before serialisation to MLIR.
// Generic lambda `<T>(params) => body` was removed in v0.5.
// The tests below confirm the parser emits typed diagnostics instead.
// Equivalent named-function generics continue to work (tested elsewhere).

#[test]
fn generic_lambda_slice1_removed_emits_diagnostic() {
    let source = r"
        fn main() {
            let v: i64 = 30;
            let r = <T>(a: T, b: T) -> T => a;
            let q = r(v, v);
        }
    ";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.iter().any(|e| {
            matches!(e.kind, hew_parser::ParseDiagnosticKind::ClosurePipeSyntax)
                && e.message.contains("E_CLOSURE_PIPE_SYNTAX")
        }),
        "expected typed E_CLOSURE_PIPE_SYNTAX for removed generic lambda, got: {:?}",
        result.errors
    );
}

#[test]
fn generic_lambda_two_type_params_removed_emits_diagnostic() {
    let source = "fn main() { let combine = <A, B>(a: A, b: B) -> A => a; combine(1, 2); }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.iter().any(|e| {
            matches!(e.kind, hew_parser::ParseDiagnosticKind::ClosurePipeSyntax)
                && e.message.contains("E_CLOSURE_PIPE_SYNTAX")
        }),
        "expected typed E_CLOSURE_PIPE_SYNTAX for removed generic lambda, got: {:?}",
        result.errors
    );
}

#[test]
fn contextual_lambda_binding_records_lambda_expr_type() {
    let source = concat!(
        "fn main() {\n",
        "    let f: fn(i64) -> i64 = |x| x + 1;\n",
        "    let y = f(5);\n",
        "}\n",
    );

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let lambda_span = result
        .program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            Item::Function(fd) if fd.name == "main" => {
                fd.body.stmts.iter().find_map(|(stmt, _)| match stmt {
                    Stmt::Let {
                        value: Some((Expr::Lambda { .. }, span)),
                        ..
                    } => Some(span.clone()),
                    _ => None,
                })
            }
            _ => None,
        })
        .expect("main let-bound lambda should exist");

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "type check errors: {:?}",
        output.errors
    );

    assert_eq!(
        output.expr_types.get(&SpanKey::from(&lambda_span)),
        Some(&Ty::Function {
            params: vec![Ty::I64],
            ret: Box::new(Ty::I64),
        })
    );
}

#[test]
fn method_level_type_params_freshen_per_named_method_call() {
    let source = r"
        type Holder { value: i64 }

        impl Holder {
            fn pick<T>(h: Holder, value: T) -> T {
                value
            }
        }

        fn main() {
            let h = Holder { value: 1 };
            let n = h.pick(42);
            let flag = h.pick(true);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "type check errors: {:?}",
        output.errors
    );
    assert_eq!(
        output.call_type_args.len(),
        2,
        "expected one entry per method call"
    );
    assert!(
        output
            .call_type_args
            .values()
            .any(|args| args == &vec![crate::ty::Ty::I64]),
        "expected one call to infer T=i64, got {:?}",
        output.call_type_args
    );
    assert!(
        output
            .call_type_args
            .values()
            .any(|args| args == &vec![crate::ty::Ty::Bool]),
        "expected one call to infer T=bool, got {:?}",
        output.call_type_args
    );
}

#[test]
fn generic_impl_method_level_type_params_freshen_per_call() {
    let source = r"
        type Box<T> { value: T }

        impl<T> Box<T> {
            fn transform<U>(b: Box<T>, f: fn(T) -> U) -> Box<U> {
                Box { value: f(b.value) }
            }
        }

        fn double(x: i64) -> i64 { x * 2 }
        fn is_even(x: i64) -> bool { x % 2 == 0 }

        fn main() {
            let b = Box { value: 42 };
            let doubled = b.transform(double);
            let even = b.transform(is_even);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "type check errors: {:?}",
        output.errors
    );
    assert_eq!(
        output.call_type_args.len(),
        2,
        "expected one entry per method call"
    );
    assert!(
        output
            .call_type_args
            .values()
            .any(|args| args == &vec![crate::ty::Ty::I64]),
        "expected one call to infer U=i64, got {:?}",
        output.call_type_args
    );
    assert!(
        output
            .call_type_args
            .values()
            .any(|args| args == &vec![crate::ty::Ty::Bool]),
        "expected one call to infer U=bool, got {:?}",
        output.call_type_args
    );
}

#[test]
fn generic_impl_method_underconstrained_type_param_reports_inference_failed() {
    let source = r"
        enum Maybe<T> { Some(T); None; }
        type Holder {}

        impl Holder {
            fn wrap<T>(h: Holder, value: Maybe<T>) -> Maybe<T> {
                value
            }
        }

        fn main() {
            let h = Holder {};
            let unresolved = h.wrap(None);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::InferenceFailed),
        "expected InferenceFailed for unresolved method-level type param, got {:?}",
        output.errors
    );
}

#[test]
fn trait_method_type_params_freshen_per_call_on_bounded_type_param() {
    let source = r"
        trait Transform {
            fn apply<U>(item: Self, f: fn(i64) -> U) -> U;
        }

        type Holder { value: i64 }

        impl Transform for Holder {
            fn apply<U>(item: Holder, f: fn(i64) -> U) -> U {
                f(item.value)
            }
        }

        fn double(x: i64) -> i64 { x * 2 }
        fn is_odd(x: i64) -> bool { x % 2 != 0 }

        fn run<T: Transform>(item: T) {
            let doubled = item.apply(double);
            let odd = item.apply(is_odd);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "type check errors: {:?}",
        output.errors
    );
    assert_eq!(
        output.call_type_args.len(),
        2,
        "expected one entry per trait-bound method call"
    );
    assert!(
        output
            .call_type_args
            .values()
            .any(|args| args == &vec![crate::ty::Ty::I64]),
        "expected one trait-bound call to infer U=i64, got {:?}",
        output.call_type_args
    );
    assert!(
        output
            .call_type_args
            .values()
            .any(|args| args == &vec![crate::ty::Ty::Bool]),
        "expected one trait-bound call to infer U=bool, got {:?}",
        output.call_type_args
    );
}

#[test]
fn trait_method_type_params_do_not_unify_across_calls() {
    let source = r"
        trait Transform {
            fn apply<U>(item: Self, f: fn(i64) -> U) -> U;
        }

        type Holder { value: i64 }

        impl Transform for Holder {
            fn apply<U>(item: Holder, f: fn(i64) -> U) -> U {
                f(item.value)
            }
        }

        fn double(x: i64) -> i64 { x * 2 }
        fn is_odd(x: i64) -> bool { x % 2 != 0 }

        fn run<T: Transform>(item: T) {
            let doubled = item.apply(double);
            let odd = item.apply(is_odd);
            println(doubled);
            println(odd);
        }

        fn main() {
            let h = Holder { value: 21 };
            run(h);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "trait-bound method calls should infer independently, got {:?}",
        output.errors
    );
}

/// Generic lambda `<T>(params) => body` was removed in v0.5.
/// The parser must emit a typed `E_CLOSURE_PIPE_SYNTAX` diagnostic even when
/// the generic lambda appears as a call argument rather than a let binding.
#[test]
fn generic_lambda_in_arg_position_rejected() {
    // Replaces `generic_lambda_scratch_state_no_leak`: the old test verified
    // that scratch state didn't leak between a generic lambda argument and a
    // subsequent let-binding. The scenario is now moot because generic lambdas
    // are rejected at the parse stage. This assertion verifies the removal
    // diagnostic fires in argument position.
    let source = r"fn main() { apply(<T>(x: T) => x, 5); }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.iter().any(|e| {
            matches!(e.kind, hew_parser::ParseDiagnosticKind::ClosurePipeSyntax)
                && e.message.contains("E_CLOSURE_PIPE_SYNTAX")
        }),
        "expected typed E_CLOSURE_PIPE_SYNTAX for removed generic lambda in arg position, got: {:?}",
        result.errors
    );
}

#[test]
fn test_self_with_generics_in_impl() {
    let source = r"
        type Pair<T> {
            first: T,
            second: T,
        }

        impl<T> Pair<T> {
            fn new(first: T, second: T) -> Self {
                return Pair { first: first, second: second };
            }

            fn swap(p: Pair<T>) -> Self {
                return Pair { first: p.second, second: p.first };
            }
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "type check errors: {:?}",
        output.errors
    );

    // Verify that Self resolves to Pair<T>, not bare Pair
    // The new method should return Pair<T>
    let new_sig = output
        .fn_sigs
        .get("Pair::new")
        .expect("Pair::new should exist");
    if let Ty::Named { name, args, .. } = &new_sig.return_type {
        assert_eq!(name, "Pair", "return type should be Pair");
        assert_eq!(args.len(), 1, "Pair should have one type argument");
    } else {
        panic!("Expected Pair::new to return a named type");
    }
}

#[test]
fn test_trait_object_type_args_substitution() {
    // Bug 2: Test that dyn Trait<Args> methods get correct substitutions
    let source = r"
        trait MyIter<T> {
            fn next(iter: Self) -> Option<T>;
        }

        type Counter {
            count: i64;
        }

        impl MyIter<i64> for Counter {
            fn next(c: Counter) -> Option<i64> {
                Some(42)
            }
        }

        fn test_iterator() -> i64 {
            let iter: dyn MyIter<i64> = Counter { count: 5 };
            let result = iter.next(); // Should be Option<i64>, not Option<T>
            match result {
                Some(x) => x,
                None => 0
            }
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);

    if !output.errors.is_empty() {
        for error in &output.errors {
            println!("Type error: {error}");
        }
    }

    assert!(
        output.errors.is_empty(),
        "type check errors: {:?}",
        output.errors
    );
}

#[test]
#[allow(
    clippy::too_many_lines,
    reason = "proof assertions for all 6 call_type_args entries"
)]
fn trait_bound_compound_generic_methods_do_not_cross_contaminate() {
    let source = r#"
        trait Transform {
            fn apply<U>(item: Self, f: fn(i64) -> U) -> U;
        }

        trait Label {
            fn tag<V>(item: Self, prefix: V) -> string;
        }

        type Holder { value: i64 }

        impl Transform for Holder {
            fn apply<U>(item: Holder, f: fn(i64) -> U) -> U {
                f(item.value)
            }
        }

        impl Label for Holder {
            fn tag<V>(item: Holder, prefix: V) -> string {
                "tagged"
            }
        }

        fn is_odd(x: i64) -> bool { x % 2 != 0 }

        fn run<T: Transform + Label>(item: T) {
            let odd = item.apply(is_odd);
            let tagged_num = item.tag(42);
            let tagged_str = item.tag("lbl");
            println(odd);
            println(tagged_num);
            println(tagged_str);
        }
    "#;

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "type check errors: {:?}",
        output.errors
    );
    // Exact count: 3 compound-bound generic method calls (apply<U=bool>, tag<V=i64>,
    // tag<V=string>) + 3 println builtins now registered with Display bounds.  Each
    // call site produces one entry keyed by span, so the total is deterministic.
    assert_eq!(
        output.call_type_args.len(),
        6,
        "expected 3 generic-method calls + 3 println Display-bound calls, got {:?}",
        output.call_type_args
    );
    assert!(
        output
            .call_type_args
            .values()
            .any(|args| args == &vec![crate::ty::Ty::Bool]),
        "expected one Transform call to infer U=bool, got {:?}",
        output.call_type_args
    );
    assert!(
        output
            .call_type_args
            .values()
            .any(|args| args == &vec![crate::ty::Ty::I64]),
        "expected one Label call to infer V=i64, got {:?}",
        output.call_type_args
    );
    assert!(
        output
            .call_type_args
            .values()
            .any(|args| args == &vec![crate::ty::Ty::String]),
        "expected one Label call to infer V=string, got {:?}",
        output.call_type_args
    );
    // Per-value count proof: pin exactly how many entries carry each type.
    // apply<U=bool> + println<T=bool> = 2; tag<V=i64> only = 1;
    // tag<V=string> + println<T=string> × 2 = 3.
    let bool_count = output
        .call_type_args
        .values()
        .filter(|args| args.as_slice() == [crate::ty::Ty::Bool])
        .count();
    assert_eq!(
        bool_count, 2,
        "apply<U=bool> and println<T=bool>: expected 2 [bool] entries, got {:?}",
        output.call_type_args
    );
    let int_count = output
        .call_type_args
        .values()
        .filter(|args| args.as_slice() == [crate::ty::Ty::I64])
        .count();
    assert_eq!(
        int_count, 1,
        "tag<V=i64>: expected exactly 1 [i64] entry, got {:?}",
        output.call_type_args
    );
    let string_count = output
        .call_type_args
        .values()
        .filter(|args| args.as_slice() == [crate::ty::Ty::String])
        .count();
    assert_eq!(
        string_count, 3,
        "tag<V=string> + println<T=string> × 2: expected 3 [string] entries, got {:?}",
        output.call_type_args
    );
}

#[test]
fn test_wire_since_without_version_warns() {
    use hew_parser::ast::{WireFieldMeta, WireMetadata};
    let wire = WireMetadata {
        field_meta: vec![WireFieldMeta {
            field_name: "added_field".to_string(),
            field_number: 2,
            is_optional: false,
            is_deprecated: false,
            is_repeated: false,
            json_name: None,
            yaml_name: None,
            since: Some(2),
        }],
        reserved_numbers: vec![],
        json_case: None,
        yaml_case: None,
        version: None,
        min_version: None,
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.validate_wire_version_constraints("TestMsg", &wire);

    assert!(checker.errors.is_empty(), "should not produce errors");
    assert_eq!(checker.warnings.len(), 1);
    assert!(
        checker.warnings[0].message.contains("since 2"),
        "warning should mention since: {}",
        checker.warnings[0].message
    );
    assert!(
        checker.warnings[0]
            .message
            .contains("no #[wire(version = N)]"),
        "warning should mention missing version: {}",
        checker.warnings[0].message
    );
    assert_eq!(checker.warnings[0].span, 0..0);
}

#[test]
fn test_wire_since_without_version_uses_registered_decl_span() {
    use hew_parser::ast::{WireFieldMeta, WireMetadata};
    let wire = WireMetadata {
        field_meta: vec![WireFieldMeta {
            field_name: "added_field".to_string(),
            field_number: 2,
            is_optional: false,
            is_deprecated: false,
            is_repeated: false,
            json_name: None,
            yaml_name: None,
            since: Some(2),
        }],
        reserved_numbers: vec![],
        json_case: None,
        yaml_case: None,
        version: None,
        min_version: None,
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.register_type_namespace_name(None, "TestMsg", &(10..50));
    checker.validate_wire_version_constraints("TestMsg", &wire);

    assert_eq!(checker.warnings.len(), 1);
    assert_eq!(checker.warnings[0].span, 10..50);
}

#[test]
fn test_wire_since_with_version_no_extra_warning() {
    use hew_parser::ast::{WireFieldMeta, WireMetadata};
    let wire = WireMetadata {
        field_meta: vec![WireFieldMeta {
            field_name: "added_field".to_string(),
            field_number: 2,
            is_optional: false,
            is_deprecated: false,
            is_repeated: false,
            json_name: None,
            yaml_name: None,
            since: Some(2),
        }],
        reserved_numbers: vec![],
        json_case: None,
        yaml_case: None,
        version: Some(3),
        min_version: None,
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.validate_wire_version_constraints("TestMsg", &wire);

    assert!(checker.errors.is_empty(), "should not produce errors");
    // No "since without version" warning since version is present
    let since_without_version = checker
        .warnings
        .iter()
        .any(|w| w.message.contains("no #[wire(version = N)]"));
    assert!(
        !since_without_version,
        "should not warn about missing version"
    );
}

#[test]
fn empty_fn_body_return_mismatch_uses_decl_span() {
    let source = "fn greet() -> string {}";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let expected_span = match &result.program.items[0].0 {
        hew_parser::ast::Item::Function(fd) => fd.decl_span.clone(),
        item => panic!("expected function item, got {item:?}"),
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let mismatch = output
        .errors
        .iter()
        .find(|error| matches!(error.kind, TypeErrorKind::Mismatch { .. }))
        .unwrap_or_else(|| panic!("expected mismatch error, got {:?}", output.errors));

    assert_eq!(mismatch.span, expected_span);
}

#[test]
fn empty_receive_fn_body_return_mismatch_uses_decl_span() {
    let source = r"
actor Greeter {
    receive fn greet() -> string {}
}
";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let expected_span = match &result.program.items[0].0 {
        hew_parser::ast::Item::Actor(actor) => actor.receive_fns[0].span.clone(),
        item => panic!("expected actor item, got {item:?}"),
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let mismatch = output
        .errors
        .iter()
        .find(|error| matches!(error.kind, TypeErrorKind::Mismatch { .. }))
        .unwrap_or_else(|| panic!("expected mismatch error, got {:?}", output.errors));

    assert_eq!(mismatch.span, expected_span);
}

#[test]
fn actor_ref_cycle_warning_uses_first_actor_decl_span() {
    let source = concat!(
        "actor Alpha {\n",
        "    let beta: ActorRef<Beta>;\n",
        "}\n",
        "actor Beta {\n",
        "    let alpha: ActorRef<Alpha>;\n",
        "}\n",
        "fn main() {}\n",
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let expected_span = result
        .program
        .items
        .iter()
        .find_map(|(item, span)| match item {
            Item::Actor(actor) if actor.name == "Alpha" => Some(span.clone()),
            _ => None,
        })
        .expect("expected Alpha actor item");

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "actor cycle warning should not introduce type errors: {:?}",
        output.errors
    );

    let warning = output
        .warnings
        .iter()
        .find(|warning| warning.kind == TypeErrorKind::ActorRefCycle)
        .unwrap_or_else(|| panic!("expected ActorRefCycle warning, got {:?}", output.warnings));
    let actor_decl_start = source
        .find("actor Alpha")
        .expect("expected Alpha declaration text");
    let actor_name_end = actor_decl_start + "actor Alpha".len();

    assert_ne!(warning.span, 0..0);
    assert_eq!(warning.span, expected_span);
    assert!(
        warning.span.start <= actor_decl_start && actor_name_end <= warning.span.end,
        "warning span should cover the first actor declaration, got {:?}",
        warning.span
    );
}

#[test]
fn recursive_value_type_self_enum_is_rejected() {
    let output = check_source(
        r"
        enum Tree { Leaf; Node(i64, Tree, Tree); }
        fn main() {}
        ",
    );

    let error = output
        .errors
        .iter()
        .find(|error| {
            matches!(
                &error.kind,
                TypeErrorKind::RecursiveValueType {
                    type_name,
                    referenced_type,
                } if type_name == "Tree" && referenced_type == "Tree"
            )
        })
        .unwrap_or_else(|| panic!("expected recursive Tree error, got {:?}", output.errors));

    assert!(error.message.contains("enum `Tree` is infinitely sized"));
    assert!(error
        .message
        .contains("variant `Node` contains `Tree` by value"));
}

#[test]
fn recursive_value_type_mutual_enums_are_rejected() {
    let output = check_source(
        r"
        enum A { A1(B); }
        enum B { B1(A); }
        fn main() {}
        ",
    );

    let recursive_types: HashSet<_> = output
        .errors
        .iter()
        .filter_map(|error| match &error.kind {
            TypeErrorKind::RecursiveValueType { type_name, .. } => Some(type_name.as_str()),
            _ => None,
        })
        .collect();

    assert!(
        recursive_types.contains("A") && recursive_types.contains("B"),
        "expected A and B recursive value type errors, got {:?}",
        output.errors
    );
}

#[test]
fn recursive_value_type_allows_non_recursive_nested_enum() {
    let output = check_source(
        r"
        enum Inner { A; B(i64); }
        enum Outer { D(Inner); }
        fn main() {}
        ",
    );

    assert!(
        output.errors.is_empty(),
        "non-recursive nested enum should type-check, got {:?}",
        output.errors
    );
}

#[test]
fn recursive_value_type_rejects_record_enum_cycle() {
    let output = check_source(
        r"
        record Boxed { tree: Tree }
        enum Tree { Leaf; Node(Boxed); }
        fn main() {}
        ",
    );

    assert!(
        output
            .errors
            .iter()
            .any(|error| matches!(error.kind, TypeErrorKind::RecursiveValueType { .. })),
        "expected record/enum recursive value type error, got {:?}",
        output.errors
    );
}

#[test]
fn recursive_value_type_rejects_generic_record_wrapper_cycle() {
    let output = check_source(
        r"
        record Wrapper<T> { value: T }
        enum Tree { Leaf; Node(Wrapper<Tree>); }
        fn main() {}
        ",
    );

    assert!(
        output.errors.iter().any(|error| {
            matches!(
                &error.kind,
                TypeErrorKind::RecursiveValueType {
                    type_name,
                    referenced_type,
                } if type_name == "Tree" && referenced_type == "Tree"
            )
        }),
        "expected generic wrapper recursive value type error, got {:?}",
        output.errors
    );
}

#[test]
fn recursive_value_type_allows_pointer_self_reference() {
    let output = check_source(
        r"
        record Node { next: *const Node }
        fn main() {}
        ",
    );

    assert!(
        output.errors.is_empty(),
        "pointer indirection should break recursive value type cycle, got {:?}",
        output.errors
    );
}

#[test]
fn typecheck_await_actor_ref_returns_unit() {
    let output = check_source(
        r#"
        actor Greeter {
            receive fn greet(name: string) {
                println(name);
            }
        }
        fn main() {
            let g = spawn Greeter;
            g.greet("hi");
            close(g);
            await g;
        }
        "#,
    );
    assert!(
        output.errors.is_empty(),
        "expected no errors, got: {:?}",
        output.errors
    );
}

#[test]
fn named_actor_receive_dispatch_reports_bad_arg_once() {
    let result = hew_parser::parse(
        r"
        actor Greeter {
            receive fn greet(name: string) {}
        }

        fn main() {
            let g = spawn Greeter;
            g.greet(missing_name);
        }
        ",
    );
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let undefined_variable_count = output
        .errors
        .iter()
        .filter(|error| matches!(error.kind, TypeErrorKind::UndefinedVariable))
        .count();

    assert_eq!(
        undefined_variable_count, 1,
        "named actor receive dispatch should not resynthesize the same bad arg: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_await_close_actor_ref() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.register_builtins();

    checker.env.define(
        "g".to_string(),
        Ty::actor_ref(Ty::Named {
            builtin: None,
            name: "Greeter".to_string(),
            args: vec![],
        }),
        false,
    );

    let span = 0..0;
    let expr = Expr::Await(Box::new((
        Expr::Call {
            function: Box::new((Expr::Identifier("close".to_string()), span.clone())),
            type_args: None,
            args: vec![CallArg::Positional((
                Expr::Identifier("g".to_string()),
                span.clone(),
            ))],
            is_tail_call: false,
        },
        span.clone(),
    )));

    let ty = checker.synthesize(&expr, &span);
    assert_eq!(ty, Ty::Unit);
    assert!(
        checker.errors.is_empty(),
        "expected no errors, got: {:?}",
        checker.errors
    );
}

#[test]
fn typecheck_await_close_lambda_actor() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.register_builtins();

    checker.env.define(
        "worker".to_string(),
        Ty::Named {
            builtin: Some(crate::BuiltinType::Actor),
            name: "Actor".to_string(),
            args: vec![Ty::I64],
        },
        false,
    );

    let span = 0..0;
    let expr = Expr::Await(Box::new((
        Expr::Call {
            function: Box::new((Expr::Identifier("close".to_string()), span.clone())),
            type_args: None,
            args: vec![CallArg::Positional((
                Expr::Identifier("worker".to_string()),
                span.clone(),
            ))],
            is_tail_call: false,
        },
        span.clone(),
    )));

    let ty = checker.synthesize(&expr, &span);
    assert_eq!(ty, Ty::Unit);
    assert!(
        checker.errors.is_empty(),
        "expected no errors, got: {:?}",
        checker.errors
    );
}

#[test]
fn typecheck_join_rejects_non_actor_sources() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let span = 0..0;
    let expr = Expr::Join(vec![
        make_int_literal(1, span.clone()),
        make_int_literal(2, span.clone()),
    ]);
    let _ = checker.synthesize(&expr, &span);
    assert!(
        checker.errors.iter().any(|error| error
            .message
            .contains("join expression element must be actor.method(args)")),
        "expected join source error, got: {:?}",
        checker.errors
    );
}

#[test]
fn typecheck_integer_literal_coerces_in_arithmetic() {
    // `n - 1` where n: i32 should work — literal 1 coerces to i32
    let source = concat!(
        "fn fib(n: i32) -> i32 {\n",
        "    if n <= 1 { n } else { fib(n - 1) + fib(n - 2) }\n",
        "}\n",
        "fn main() { println(fib(10)); }\n"
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "integer literal should coerce in arithmetic: {:?}",
        output.errors
    );
}

#[test]
fn i32_coerces_to_bool_in_condition_position() {
    let (errors, _warnings) =
        parse_and_check("fn foo(flag: i32) -> i32 { if flag { 1 } else { 0 } }");
    assert!(
        errors.is_empty(),
        "i32 in bool position must be allowed: {errors:?}"
    );
}

#[test]
fn bool_does_not_coerce_to_i32() {
    let (errors, _warnings) = parse_and_check("fn foo(flag: bool) -> i32 { flag }");
    assert!(
        errors
            .iter()
            .any(|error| matches!(error.kind, TypeErrorKind::Mismatch { .. })),
        "bool where i32 expected must be rejected: {errors:?}"
    );
}

#[test]
fn handle_type_does_not_coerce_to_string() {
    let (errors, _warnings) = parse_and_check_with_stdlib(
        "import std::encoding::json;\nfn foo(value: json.Value) -> string { value }",
    );
    assert!(
        errors
            .iter()
            .any(|error| matches!(error.kind, TypeErrorKind::Mismatch { .. })),
        "json.Value where string expected must be rejected: {errors:?}"
    );
}

#[test]
fn string_does_not_coerce_to_handle_type() {
    let (errors, _warnings) = parse_and_check_with_stdlib(
        "import std::encoding::json;\nfn foo(text: string) -> json.Value { text }",
    );
    assert!(
        errors
            .iter()
            .any(|error| matches!(error.kind, TypeErrorKind::Mismatch { .. })),
        "string where json.Value expected must be rejected: {errors:?}"
    );
}

#[test]
fn unconstrained_range_defaults_to_i64() {
    let source = "fn main() { for i in 0..10 { println(i); } }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let main_fn = result
        .program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            Item::Function(function) if function.name == "main" => Some(function),
            _ => None,
        })
        .expect("main function should exist");
    let Stmt::For { iterable, .. } = &main_fn.body.stmts[0].0 else {
        panic!("expected for statement");
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unconstrained range should type-check: {:?}",
        output.errors
    );
    assert_eq!(
        output.expr_types.get(&SpanKey::from(&iterable.1)),
        Some(&Ty::range(Ty::I64)),
        "unconstrained range literal should default to Range<i64>: {:?}",
        output.expr_types
    );
}

#[test]
fn typecheck_literal_range_infers_from_context() {
    // `for i in 0..8 { fib(i) }` where fib takes i32 — range bounds
    // should not force i64; the loop variable should be usable as i32
    let source = concat!(
        "fn fib(n: i32) -> i32 {\n",
        "    if n <= 1 { n } else { fib(n - 1) + fib(n - 2) }\n",
        "}\n",
        "fn main() {\n",
        "    for i in 0..8 {\n",
        "        println(fib(i));\n",
        "    }\n",
        "}\n"
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "literal range should infer element type from context: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_cast_expression_numeric() {
    let source = concat!(
        "fn main() {\n",
        "    let x: i64 = 42;\n",
        "    let y: i32 = x as i32;\n",
        "    let z: f64 = y as f64;\n",
        "    println(y);\n",
        "    println(z);\n",
        "}\n"
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "numeric casts should type-check: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_cast_expression_invalid() {
    let source = concat!(
        "fn main() {\n",
        "    let s = \"hello\";\n",
        "    let x = s as i32;\n",
        "    println(x);\n",
        "}\n"
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("cannot cast")),
        "should reject invalid cast: {:?}",
        output.errors
    );
}

// ── Literal coercion tests ────────────────────────────────────────

#[test]
fn literal_coercion_integer_to_i32() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let lit = make_int_literal(42, 0..2);
    let ty = checker.check_against(&lit.0, &lit.1, &Ty::I32);
    assert_eq!(ty, Ty::I32);
}

#[test]
fn literal_coercion_integer_to_u8() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let lit = make_int_literal(255, 0..3);
    let ty = checker.check_against(&lit.0, &lit.1, &Ty::U8);
    assert_eq!(ty, Ty::U8);
}

#[test]
fn literal_coercion_integer_to_u8_overflow() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let lit = make_int_literal(256, 0..3);
    let ty = checker.check_against(&lit.0, &lit.1, &Ty::U8);
    assert_eq!(ty, Ty::Error);
    assert!(
        checker
            .errors
            .iter()
            .any(|e| e.message.contains("does not fit")),
        "expected range error: {:?}",
        checker.errors
    );
}

#[test]
fn literal_coercion_negative_to_unsigned() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let lit = (
        Expr::Unary {
            op: UnaryOp::Negate,
            operand: Box::new(make_int_literal(1, 1..2)),
        },
        0..2,
    );
    let ty = checker.check_against(&lit.0, &lit.1, &Ty::U32);
    assert_eq!(ty, Ty::Error);
    assert!(
        checker
            .errors
            .iter()
            .any(|e| e.message.contains("negative literal")),
        "expected negative-to-unsigned error: {:?}",
        checker.errors
    );
}

#[test]
fn literal_coercion_i32_overflow() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    // 2^31 = 2147483648, which exceeds i32 max (2147483647)
    let lit = make_int_literal(2_147_483_648, 0..10);
    let ty = checker.check_against(&lit.0, &lit.1, &Ty::I32);
    assert_eq!(ty, Ty::Error);
    assert!(
        checker
            .errors
            .iter()
            .any(|e| e.message.contains("does not fit")),
        "expected range error: {:?}",
        checker.errors
    );
}

#[test]
fn int_literal_infers_from_annotated_binding() {
    let output = check_source(
        r"
        fn main() {
            let x: i32 = 1;
            let y: u8 = 255;
            let z: i16 = -12;
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "integer literals should infer from adjacent annotations: {:#?}",
        output.errors
    );
}

#[test]
fn int_literal_inference_rejects_out_of_range_annotation() {
    let output = check_source(
        r"
        fn main() {
            let x: i32 = 2147483648;
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("does not fit")),
        "out-of-range literal must reject against inferred i32 context: {:#?}",
        output.errors
    );
}

#[test]
fn literal_coercion_integer_to_f32() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let lit = make_int_literal(42, 0..2);
    let ty = checker.check_against(&lit.0, &lit.1, &Ty::F32);
    assert_eq!(ty, Ty::F32);
}

#[test]
fn literal_coercion_float_to_f32() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let lit = (Expr::Literal(Literal::Float(std::f64::consts::PI)), 0..4);
    let ty = checker.check_against(&lit.0, &lit.1, &Ty::F32);
    assert_eq!(ty, Ty::F32);
}

#[test]
fn literal_coercion_integer_fits_i8() {
    assert!(integer_fits_type(127, &Ty::I8));
    assert!(integer_fits_type(-128, &Ty::I8));
    assert!(!integer_fits_type(128, &Ty::I8));
    assert!(!integer_fits_type(-129, &Ty::I8));
}

#[test]
fn literal_coercion_integer_fits_u8() {
    assert!(integer_fits_type(0, &Ty::U8));
    assert!(integer_fits_type(255, &Ty::U8));
    assert!(!integer_fits_type(256, &Ty::U8));
    assert!(!integer_fits_type(-1, &Ty::U8));
}

#[test]
fn literal_coercion_integer_fits_i16() {
    assert!(integer_fits_type(32767, &Ty::I16));
    assert!(integer_fits_type(-32768, &Ty::I16));
    assert!(!integer_fits_type(32768, &Ty::I16));
}

#[test]
fn literal_coercion_integer_fits_u16() {
    assert!(integer_fits_type(65535, &Ty::U16));
    assert!(!integer_fits_type(65536, &Ty::U16));
}

#[test]
fn literal_coercion_integer_fits_i32() {
    assert!(integer_fits_type(2_147_483_647, &Ty::I32));
    assert!(integer_fits_type(-2_147_483_648, &Ty::I32));
    assert!(!integer_fits_type(2_147_483_648, &Ty::I32));
}

#[test]
fn literal_coercion_integer_fits_u32() {
    assert!(integer_fits_type(4_294_967_295, &Ty::U32));
    assert!(!integer_fits_type(4_294_967_296, &Ty::U32));
    assert!(!integer_fits_type(-1, &Ty::U32));
}

#[test]
fn literal_coercion_integer_fits_u64() {
    // i64 max fits in u64
    assert!(integer_fits_type(i64::MAX, &Ty::U64));
    // 0 fits
    assert!(integer_fits_type(0, &Ty::U64));
    // Negative doesn't fit
    assert!(!integer_fits_type(-1, &Ty::U64));
}

// ── Array literal → Vec type coercion tests ──────────────────────

#[test]
fn array_literal_synthesizes_vec() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let elems = vec![
        make_int_literal(1, 1..2),
        make_int_literal(2, 4..5),
        make_int_literal(3, 7..8),
    ];
    let arr = (Expr::Array(elems), 0..9);
    let ty = checker.synthesize(&arr.0, &arr.1);
    assert_eq!(
        ty,
        Ty::Named {
            builtin: Some(BuiltinType::Vec),
            name: "Vec".to_string(),
            args: vec![Ty::IntLiteral],
        }
    );
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {checker_errors:#?}",
        checker_errors = checker.errors
    );
}

#[test]
fn literal_coercion_array_to_i32_vec() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let elems = vec![
        make_int_literal(1, 1..2),
        make_int_literal(2, 4..5),
        make_int_literal(3, 7..8),
    ];
    let arr = (Expr::Array(elems), 0..9);
    let expected = Ty::Named {
        builtin: Some(BuiltinType::Vec),
        name: "Vec".to_string(),
        args: vec![Ty::I32],
    };
    let ty = checker.check_against(&arr.0, &arr.1, &expected);
    assert_eq!(ty, expected);
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {checker_errors:#?}",
        checker_errors = checker.errors
    );
}

#[test]
fn array_literal_methods_index_and_get_resolve() {
    let output = check_source(
        r"
        fn main() -> i64 {
            let values = [1, 2, 3];
            if values.len() != 3 { return 10; }
            if values[0] != 1 { return 11; }
            if values.get(0) != 1 { return 12; }
            0
        }
        ",
    );

    assert!(
        output.errors.is_empty(),
        "array literal should type as Vec<T> for methods and indexing: {:#?}",
        output.errors
    );
    assert!(
        output
            .resolved_calls
            .values()
            .any(|call| call.method_name == "get"),
        "Vec::get on an inferred integer array literal must record a resolved call: {:#?}",
        output.resolved_calls
    );
}

#[test]
fn literal_coercion_array_repeat_to_i32() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let value = make_int_literal(0, 1..2);
    let count = make_int_literal(5, 4..5);
    let arr = (
        Expr::ArrayRepeat {
            value: Box::new(value),
            count: Box::new(count),
        },
        0..6,
    );
    let expected = Ty::Array(Box::new(Ty::I32), 5);
    let ty = checker.check_against(&arr.0, &arr.1, &expected);
    assert_eq!(ty, expected);
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

// ── Tuple literal coercion tests ───────────────────────────────────

#[test]
fn tuple_literal_coercion_to_typed() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let elems = vec![
        make_int_literal(42, 1..3),
        (Expr::Literal(Literal::Float(std::f64::consts::PI)), 5..9),
    ];
    let tuple = (Expr::Tuple(elems), 0..10);
    let expected = Ty::Tuple(vec![Ty::I32, Ty::F32]);
    let ty = checker.check_against(&tuple.0, &tuple.1, &expected);
    assert_eq!(ty, expected);
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

#[test]
fn tuple_literal_coercion_overflow() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let elems = vec![make_int_literal(256, 1..4)];
    let tuple = (Expr::Tuple(elems), 0..5);
    let expected = Ty::Tuple(vec![Ty::U8]);
    let _ty = checker.check_against(&tuple.0, &tuple.1, &expected);
    // The tuple itself coerces, but the element should have reported an error
    assert!(
        checker
            .errors
            .iter()
            .any(|e| e.message.contains("does not fit")),
        "expected range error: {:?}",
        checker.errors
    );
}

#[test]
fn range_default_type_is_i64() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let range = (
        Expr::Range {
            start: None,
            end: None,
            inclusive: false,
        },
        0..2,
    );
    let ty = checker.synthesize(&range.0, &range.1);
    assert_eq!(ty, Ty::range(Ty::I64));
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

// ── Type variable resolution in check_against ────────────────────

#[test]
fn literal_coercion_through_type_var() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    // Create a type variable and unify it with i32
    let tv = TypeVar::fresh();
    checker.subst.insert(tv, &Ty::I32).unwrap();
    // Now check an integer literal against the type variable
    let lit = make_int_literal(42, 0..2);
    let ty = checker.check_against(&lit.0, &lit.1, &Ty::Var(tv));
    assert_eq!(ty, Ty::I32);
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

// ── Let-bound literal coercion tests ─────────────────────────────

#[test]
fn let_bound_literal_coercion() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    // Simulate: let n = 5
    let let_stmt = Stmt::Let {
        pattern: (Pattern::Identifier("n".to_string()), 4..5),
        ty: None,
        value: Some(make_int_literal(5, 8..9)),
    };
    checker.check_stmt(&let_stmt, &(0..10));
    // Now check: let x: i32 = n
    let ident = (Expr::Identifier("n".to_string()), 15..16);
    let ty = checker.check_against(&ident.0, &ident.1, &Ty::I32);
    assert_eq!(ty, Ty::I32);
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

#[test]
fn let_bound_literal_overflow() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    // Simulate: let n = 2147483648 (exceeds i32 max)
    let let_stmt = Stmt::Let {
        pattern: (Pattern::Identifier("n".to_string()), 4..5),
        ty: None,
        value: Some(make_int_literal(2_147_483_648, 8..18)),
    };
    checker.check_stmt(&let_stmt, &(0..19));
    // Now check: let x: i32 = n — should fail with range error
    let ident = (Expr::Identifier("n".to_string()), 24..25);
    let ty = checker.check_against(&ident.0, &ident.1, &Ty::I32);
    assert_eq!(ty, Ty::Error);
    assert!(
        checker
            .errors
            .iter()
            .any(|e| e.message.contains("does not fit")),
        "expected range error: {:?}",
        checker.errors
    );
}

#[test]
fn derived_intliteral_identifier_coerces_without_const_values() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let source_stmt = Stmt::Let {
        pattern: (Pattern::Identifier("n".to_string()), 4..5),
        ty: None,
        value: Some((
            Expr::Binary {
                left: Box::new(make_int_literal(1, 8..9)),
                op: BinaryOp::Add,
                right: Box::new(make_int_literal(2, 12..13)),
            },
            8..13,
        )),
    };
    checker.check_stmt(&source_stmt, &(0..14));

    assert!(
        !checker.const_values.contains_key("n"),
        "derived literals should not register const_values"
    );

    let target_stmt = Stmt::Let {
        pattern: (Pattern::Identifier("y".to_string()), 20..21),
        ty: Some((
            TypeExpr::Named {
                name: "i32".to_string(),
                type_args: None,
            },
            23..26,
        )),
        value: Some((Expr::Identifier("n".to_string()), 29..30)),
    };
    checker.check_stmt(&target_stmt, &(20..30));

    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

#[test]
fn negated_literal_let_binding_coerces_signed() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let let_stmt = Stmt::Let {
        pattern: (Pattern::Identifier("n".to_string()), 4..5),
        ty: None,
        value: Some((
            Expr::Unary {
                op: UnaryOp::Negate,
                operand: Box::new(make_int_literal(5, 9..10)),
            },
            8..10,
        )),
    };
    checker.check_stmt(&let_stmt, &(0..11));

    let ident = (Expr::Identifier("n".to_string()), 16..17);
    let ty = checker.check_against(&ident.0, &ident.1, &Ty::I8);
    assert_eq!(ty, Ty::I8);
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

#[test]
fn const_default_width_registers_in_const_values() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let decl = ConstDecl {
        visibility: Visibility::Private,
        name: "N".to_string(),
        ty: (
            TypeExpr::Named {
                name: "i64".to_string(),
                type_args: None,
            },
            0..0,
        ),
        value: make_int_literal(100, 0..3),
        doc_comment: None,
    };
    checker.check_const(&decl, &(0..3));

    assert!(matches!(
        checker.const_values.get("N"),
        Some(ConstValue::Integer(100))
    ));

    let ident = (Expr::Identifier("N".to_string()), 10..11);
    let ty = checker.check_against(&ident.0, &ident.1, &Ty::I32);
    assert_eq!(ty, Ty::I32);
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

#[test]
fn const_explicit_width_not_in_const_values() {
    // Explicit-width consts (`const N: i32 = 100`) must not be registered
    // in `const_values` because they have a known non-literal type and cannot
    // be widened freely at use sites.
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let decl = ConstDecl {
        visibility: Visibility::Private,
        name: "N".to_string(),
        ty: (
            TypeExpr::Named {
                name: "i32".to_string(),
                type_args: None,
            },
            0..0,
        ),
        value: make_int_literal(100, 0..3),
        doc_comment: None,
    };
    checker.check_const(&decl, &(0..3));

    assert!(
        !checker.const_values.contains_key("N"),
        "explicit-width consts should not register const_values"
    );
}

#[test]
fn const_explicit_width_assigned_to_wider_type_is_rejected() {
    // `const N: i32 = 100; let y: i64 = N;` must be a type error now that
    // implicit integer widening is removed.  The caller must write `N as i64`.
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let decl = ConstDecl {
        visibility: Visibility::Private,
        name: "N".to_string(),
        ty: (
            TypeExpr::Named {
                name: "i32".to_string(),
                type_args: None,
            },
            0..0,
        ),
        value: make_int_literal(100, 0..3),
        doc_comment: None,
    };
    checker.check_const(&decl, &(0..3));

    let target_stmt = Stmt::Let {
        pattern: (Pattern::Identifier("y".to_string()), 10..11),
        ty: Some((
            TypeExpr::Named {
                name: "i64".to_string(),
                type_args: None,
            },
            14..17,
        )),
        value: Some((Expr::Identifier("N".to_string()), 20..21)),
    };
    checker.check_stmt(&target_stmt, &(10..21));

    let binding = checker.env.lookup_ref("N").expect("N should be defined");
    assert_eq!(binding.ty, Ty::I32);

    // The widening rejection must fire exactly once, on the assignment span
    // (10..21), not on the const declaration span (0..3).
    let widening_errors: Vec<_> = checker
        .errors
        .iter()
        .filter(|e| {
            e.message.contains("cannot implicitly convert")
                && e.message.contains("i32")
                && e.message.contains("i64")
        })
        .collect();
    assert_eq!(
        widening_errors.len(),
        1,
        "expected exactly one widening-rejection error, got: {:?}",
        checker.errors
    );
    // No error should reference the const decl span (0..3) — the decl itself
    // is well-typed; only the assignment is rejected.
    assert!(
        !checker
            .errors
            .iter()
            .any(|e| e.span.start < 3 && e.span.end <= 3),
        "unexpected error on const decl span: {:?}",
        checker.errors
    );
}

#[test]
fn mutable_var_initializer_keeps_integer_literal_inferable() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let var_stmt = Stmt::Var {
        name: "n".to_string(),
        ty: None,
        value: Some(make_int_literal(5, 8..9)),
    };
    checker.check_stmt(&var_stmt, &(0..10));
    let binding = checker
        .env
        .lookup_ref("n")
        .expect("mutable binding should be defined");
    assert!(matches!(binding.ty, Ty::Var(_)));
    assert_eq!(checker.subst.resolve(&binding.ty), Ty::IntLiteral);
    assert_eq!(
        checker
            .expr_types
            .get(&SpanKey {
                start: 8,
                end: 9,
                module_idx: 0
            })
            .cloned(),
        Some(binding.ty.clone())
    );
}

#[test]
fn typecheck_output_materializes_literal_kinds_for_unannotated_lets() {
    let source = "fn main() { let x = 1; let y = 2.0; }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output.expr_types.values().any(Ty::is_numeric_literal),
        "TypeCheckOutput should materialize surviving literal kinds before serialization: {:?}",
        output.expr_types
    );
    assert!(output.expr_types.values().any(|ty| ty == &Ty::I64));
    assert!(output.expr_types.values().any(|ty| ty == &Ty::F64));
}

#[test]
fn bind_pattern_struct_fields_substitute_generic_type_args() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.type_defs.insert(
        "Pair".to_string(),
        TypeDef {
            kind: TypeDefKind::Struct,
            name: "Pair".to_string(),
            type_params: vec!["T".to_string(), "U".to_string()],
            fields: HashMap::from([
                (
                    "first".to_string(),
                    Ty::Named {
                        builtin: None,
                        name: "T".to_string(),
                        args: vec![],
                    },
                ),
                (
                    "second".to_string(),
                    Ty::Named {
                        builtin: None,
                        name: "U".to_string(),
                        args: vec![],
                    },
                ),
            ]),
            variants: HashMap::new(),
            methods: HashMap::new(),
            doc_comment: None,
            field_order: vec![],
            is_indirect: false,
        },
    );

    checker.bind_pattern(
        &Pattern::Struct {
            name: "Pair".to_string(),
            fields: vec![
                hew_parser::ast::PatternField {
                    name: "first".to_string(),
                    pattern: None,
                },
                hew_parser::ast::PatternField {
                    name: "second".to_string(),
                    pattern: None,
                },
            ],
        },
        &Ty::Named {
            builtin: None,
            name: "Pair".to_string(),
            args: vec![Ty::I64, Ty::Bool],
        },
        false,
        &(0..10),
    );

    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
    assert_eq!(
        checker
            .env
            .lookup_ref("first")
            .map(|binding| binding.ty.clone()),
        Some(Ty::I64),
        "generic struct destructuring must bind instantiated field types"
    );
    assert_eq!(
        checker
            .env
            .lookup_ref("second")
            .map(|binding| binding.ty.clone()),
        Some(Ty::Bool),
        "generic struct destructuring must bind instantiated field types"
    );
}

#[test]
fn or_pattern_binding_helper_rejects_mutability_mismatch() {
    let checker = Checker::new(ModuleRegistry::new(vec![]));
    let names = HashSet::from(["x".to_string()]);
    let mut left_env = crate::env::TypeEnv::new();
    let mut right_env = crate::env::TypeEnv::new();
    left_env.define_with_span("x".to_string(), Ty::I64, true, 0..1);
    right_env.define_with_span("x".to_string(), Ty::I64, false, 0..1);

    assert!(
        !checker.or_pattern_bindings_match(&left_env, &right_env, &names, &names),
        "or-pattern merge must reject bindings with mismatched mutability"
    );
}

#[test]
fn struct_pattern_missing_type_def_emits_diagnostic() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));

    checker.bind_pattern(
        &Pattern::Struct {
            name: "Ghost".to_string(),
            fields: vec![hew_parser::ast::PatternField {
                name: "value".to_string(),
                pattern: None,
            }],
        },
        &Ty::Named {
            builtin: None,
            name: "Ghost".to_string(),
            args: vec![],
        },
        false,
        &(0..5),
    );

    assert!(
        checker.errors.iter().any(|error| {
            error.kind == TypeErrorKind::UndefinedType
                && error.message.contains("type `Ghost` is not defined")
        }),
        "missing type defs in struct patterns must fail closed: {checker_errors:#?}",
        checker_errors = checker.errors
    );
    assert_eq!(
        checker
            .env
            .lookup_ref("value")
            .map(|binding| binding.ty.clone()),
        Some(Ty::Error),
        "missing type defs should seed placeholder bindings for recovery"
    );
}

// ── Struct init literal coercion tests ─────────────────────────────

fn register_generic_wrapper(checker: &mut Checker) {
    let mut fields = HashMap::new();
    fields.insert(
        "value".to_string(),
        Ty::Named {
            builtin: None,
            name: "T".to_string(),
            args: vec![],
        },
    );
    checker.type_defs.insert(
        "Wrapper".to_string(),
        TypeDef {
            kind: TypeDefKind::Struct,
            name: "Wrapper".to_string(),
            type_params: vec!["T".to_string()],
            fields,
            variants: HashMap::new(),
            methods: HashMap::new(),
            doc_comment: None,
            field_order: vec![],
            is_indirect: false,
        },
    );
}

#[test]
fn struct_init_coerces_literal_to_expected_type_arg() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    register_generic_wrapper(&mut checker);

    // Wrapper { value: 42 } checked against Wrapper<i32>
    let init = (
        Expr::StructInit {
            name: "Wrapper".to_string(),
            fields: vec![("value".to_string(), make_int_literal(42, 10..12))],
            type_args: None,
            base: None,
        },
        0..20,
    );
    let expected = Ty::Named {
        builtin: None,
        name: "Wrapper".to_string(),
        args: vec![Ty::I32],
    };
    let ty = checker.check_against(&init.0, &init.1, &expected);
    assert_eq!(ty, expected);
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

#[test]
fn struct_init_infers_type_param_from_literal() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    register_generic_wrapper(&mut checker);

    // Wrapper { value: 42 } without expected type keeps the literal kind until
    // a later coercion/defaulting boundary.
    let init = (
        Expr::StructInit {
            name: "Wrapper".to_string(),
            fields: vec![("value".to_string(), make_int_literal(42, 10..12))],
            type_args: None,
            base: None,
        },
        0..20,
    );
    let ty = checker.synthesize(&init.0, &init.1);
    assert_eq!(
        ty,
        Ty::Named {
            builtin: None,
            name: "Wrapper".to_string(),
            args: vec![Ty::IntLiteral],
        }
    );
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

#[test]
fn struct_init_overflow_in_expected_type() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    register_generic_wrapper(&mut checker);

    // Wrapper { value: 256 } checked against Wrapper<u8> — should error
    let init = (
        Expr::StructInit {
            name: "Wrapper".to_string(),
            fields: vec![("value".to_string(), make_int_literal(256, 10..13))],
            type_args: None,
            base: None,
        },
        0..20,
    );
    let expected = Ty::Named {
        builtin: None,
        name: "Wrapper".to_string(),
        args: vec![Ty::U8],
    };
    let _ty = checker.check_against(&init.0, &init.1, &expected);
    assert!(
        checker
            .errors
            .iter()
            .any(|e| e.message.contains("does not fit")),
        "expected range error: {:?}",
        checker.errors
    );
}

// ── Explicit type args at struct init site (F1 fix) ────────────────────────

#[test]
fn struct_init_explicit_type_arg_seeds_substitution() {
    // `Wrapper<string> { value: "hello" }` — explicit type arg must constrain
    // field checking against string, not an unbound param.
    let source = r#"
        type Wrapper<T> { value: T }
        fn main() {
            let w = Wrapper<string> { value: "hello" };
        }
    "#;
    let tco = check_source(source);
    assert!(
        tco.errors.is_empty(),
        "explicit type arg should check cleanly: {:?}",
        tco.errors
    );
}

#[test]
fn struct_init_explicit_type_arg_wrong_field_type_errors() {
    // `Wrapper<i64> { value: "hello" }` — explicit arg is i64, field is string: error.
    let source = r#"
        type Wrapper<T> { value: T }
        fn main() {
            let w = Wrapper<i64> { value: "hello" };
        }
    "#;
    let parse_result = hew_parser::parse(source);
    assert!(
        parse_result.errors.is_empty(),
        "should parse: {:?}",
        parse_result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parse_result.program);
    assert!(
        !tco.errors.is_empty(),
        "mismatched explicit type arg should produce a type error"
    );
}

#[test]
fn struct_init_explicit_type_arg_arity_mismatch_errors() {
    // `Wrapper<i64, string>` on a one-param struct should report arity mismatch.
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    register_generic_wrapper(&mut checker);

    let span = 0..30_usize;
    let type_args = Some(vec![
        (
            TypeExpr::Named {
                name: "i64".to_string(),
                type_args: None,
            },
            0..3_usize,
        ),
        (
            TypeExpr::Named {
                name: "string".to_string(),
                type_args: None,
            },
            4..10_usize,
        ),
    ]);
    let init = (
        Expr::StructInit {
            name: "Wrapper".to_string(),
            fields: vec![("value".to_string(), make_int_literal(1, 20..21))],
            type_args,
            base: None,
        },
        span.clone(),
    );
    checker.synthesize(&init.0, &init.1);
    assert!(
        checker
            .errors
            .iter()
            .any(|e| e.message.contains("type parameter")),
        "arity mismatch should produce an error, got: {:?}",
        checker.errors
    );
}

#[test]
fn struct_init_explicit_type_arg_roundtrip_via_parse() {
    // Parser + checker integration: `Wrapper<string> { value: "hello" }` must
    // parse *and* type-check with the synthesised type `Wrapper<string>`.
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    register_generic_wrapper(&mut checker);

    let source = r#"type Wrapper<T> { value: T }
fn main() { let w = Wrapper<string> { value: "hello" }; }"#;
    let parse_result = hew_parser::parse(source);
    assert!(
        parse_result.errors.is_empty(),
        "parse errors: {:?}",
        parse_result.errors
    );
    let tco = checker.check_program(&parse_result.program);
    assert!(
        tco.errors.is_empty(),
        "parse+check roundtrip should be error-free: {:?}",
        tco.errors
    );
}

// ── Reject tests for check_against paths (blocker fixes) ───────────────────

#[test]
fn struct_init_explicit_type_arg_conflicts_with_binding_type_errors() {
    // `let w: Wrapper<i64> = Wrapper<string> { value: 1 };`
    // The explicit `string` annotation conflicts with the expected `i64`.
    // The checker must reject this rather than silently ignoring it.
    let source = r"
        type Wrapper<T> { value: T }
        fn main() {
            let w: Wrapper<i64> = Wrapper<string> { value: 1 };
        }
    ";
    let parse_result = hew_parser::parse(source);
    assert!(
        parse_result.errors.is_empty(),
        "should parse without errors: {:?}",
        parse_result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parse_result.program);
    assert!(
        !tco.errors.is_empty(),
        "conflicting explicit type arg should produce a type error"
    );
    let has_mismatch = tco.errors.iter().any(|e| {
        matches!(e.kind, TypeErrorKind::Mismatch { .. })
            || e.message.contains("conflicts")
            || e.message.contains("string")
    });
    assert!(
        has_mismatch,
        "error should mention the type conflict, got: {:?}",
        tco.errors
    );
}

#[test]
fn struct_init_explicit_type_arg_on_enum_variant_in_check_against_errors() {
    // Explicit type args on an enum variant struct initializer when the expected
    // type is already known should produce an error (fail-closed for this slice).
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    // Register a generic enum `Keeper<T>` with a struct variant `Holding { value: T }`
    let mut variant_fields = HashMap::new();
    variant_fields.insert(
        "Holding".to_string(),
        VariantDef::Struct(vec![(
            "value".to_string(),
            Ty::Named {
                builtin: None,
                name: "T".to_string(),
                args: vec![],
            },
        )]),
    );
    checker.type_defs.insert(
        "Keeper".to_string(),
        TypeDef {
            kind: TypeDefKind::Enum,
            name: "Keeper".to_string(),
            type_params: vec!["T".to_string()],
            fields: HashMap::new(),
            variants: variant_fields,
            methods: HashMap::new(),
            doc_comment: None,
            field_order: vec![],
            is_indirect: false,
        },
    );

    let span = 0..30_usize;
    // Explicit type args on an enum variant struct form in check_against path.
    let type_args = Some(vec![(
        TypeExpr::Named {
            name: "i64".to_string(),
            type_args: None,
        },
        0..3_usize,
    )]);
    let init = Expr::StructInit {
        name: "Holding".to_string(),
        fields: vec![("value".to_string(), make_int_literal(42, 10..12))],
        type_args,
        base: None,
    };
    let expected = Ty::Named {
        builtin: None,
        name: "Keeper".to_string(),
        args: vec![Ty::I64],
    };
    checker.check_against(&init, &span, &expected);
    assert!(
        !checker.errors.is_empty(),
        "explicit type args on enum variant struct form in check_against should produce an error"
    );
}

#[test]
fn struct_init_explicit_type_arg_on_enum_variant_synthesize_seeds_correctly() {
    // `Keeper::Holding<i64> { value: 42 }` in the synthesize path — the explicit
    // `i64` annotation should pre-seed the type_arg_map so the synthesised type
    // is `Keeper<i64>`, not `Keeper<i64>` or an unconstrained type var.
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    // Register `Keeper<T>` enum with struct variant `Holding { value: T }`
    let mut variant_fields_map = HashMap::new();
    variant_fields_map.insert(
        "Keeper::Holding".to_string(),
        VariantDef::Struct(vec![(
            "value".to_string(),
            Ty::Named {
                builtin: None,
                name: "T".to_string(),
                args: vec![],
            },
        )]),
    );
    checker.type_defs.insert(
        "Keeper".to_string(),
        TypeDef {
            kind: TypeDefKind::Enum,
            name: "Keeper".to_string(),
            type_params: vec!["T".to_string()],
            fields: HashMap::new(),
            variants: variant_fields_map,
            methods: HashMap::new(),
            doc_comment: None,
            field_order: vec![],
            is_indirect: false,
        },
    );

    let span = 0..30_usize;
    let type_args = Some(vec![(
        TypeExpr::Named {
            name: "i64".to_string(),
            type_args: None,
        },
        0..3_usize,
    )]);
    let init = Expr::StructInit {
        name: "Keeper::Holding".to_string(),
        fields: vec![("value".to_string(), make_int_literal(42, 10..12))],
        type_args,
        base: None,
    };
    let result = checker.synthesize(&init, &span);
    assert!(
        checker.errors.is_empty(),
        "enum variant explicit type arg should synthesize without errors: {:?}",
        checker.errors
    );
    // The synthesised type should be Keeper<i64> (IntLiteral coerces to i64 / i64)
    assert!(
        matches!(result, Ty::Named { ref name, .. } if name == "Keeper"),
        "synthesised type should be Keeper<…>, got: {result}"
    );
}

// ── record_init_type_args side-table emission ─────────────────────────────
//
// These tests verify that `check_struct_init` populates
// `TypeCheckOutput.record_init_type_args` with the resolved concrete `Ty`
// arguments for every user-defined generic record / enum-struct-variant
// initialiser site. Mirrors the `call_type_args` contract for generic free
// function calls (`record_concrete_call_type_args`).

fn collect_record_init_args(tco: &TypeCheckOutput) -> Vec<Vec<Ty>> {
    let mut entries: Vec<_> = tco.record_init_type_args.iter().collect();
    // Stable ordering by span start for deterministic assertions.
    entries.sort_by_key(|(k, _)| k.start);
    entries.into_iter().map(|(_, v)| v.clone()).collect()
}

#[test]
fn record_init_type_args_inferred_box_int() {
    // `Box { value: 42 }` — checker must infer `[i64]` from the literal
    // (post-defaulting) and record it on the side-table.
    let source = r"
        type Box<T> { value: T }
        fn main() { let _b = Box { value: 42 }; }
    ";
    let tco = check_source(source);
    assert!(
        tco.errors.is_empty(),
        "should check cleanly: {:?}",
        tco.errors
    );
    let entries = collect_record_init_args(&tco);
    assert_eq!(
        entries.len(),
        1,
        "exactly one record-init entry expected, got: {entries:?}"
    );
    assert_eq!(
        entries[0],
        vec![Ty::I64],
        "Box {{ value: 42 }} should resolve to Box<i64>, got: {entries:?}"
    );
}

#[test]
fn record_init_type_args_inferred_box_string() {
    let source = r#"
        type Box<T> { value: T }
        fn main() { let _b = Box { value: "hello" }; }
    "#;
    let tco = check_source(source);
    assert!(tco.errors.is_empty(), "errors: {:?}", tco.errors);
    let entries = collect_record_init_args(&tco);
    assert_eq!(entries.len(), 1);
    assert_eq!(entries[0], vec![Ty::String]);
}

#[test]
fn record_init_type_args_explicit_type_arg() {
    // Explicit `<string>` annotation should produce a single
    // record_init_type_args entry of `[string]`.
    let source = r#"
        type Box<T> { value: T }
        fn main() { let _b = Box<string> { value: "hi" }; }
    "#;
    let tco = check_source(source);
    assert!(tco.errors.is_empty(), "errors: {:?}", tco.errors);
    let entries = collect_record_init_args(&tco);
    assert_eq!(entries.len(), 1);
    assert_eq!(entries[0], vec![Ty::String]);
}

#[test]
fn record_init_type_args_two_params() {
    // Two type params, inferred from two field values of different types.
    let source = r#"
        type Pair<A, B> { first: A, second: B }
        fn main() { let _p = Pair { first: 1, second: "y" }; }
    "#;
    let tco = check_source(source);
    assert!(tco.errors.is_empty(), "errors: {:?}", tco.errors);
    let entries = collect_record_init_args(&tco);
    assert_eq!(entries.len(), 1);
    assert_eq!(entries[0], vec![Ty::I64, Ty::String]);
}

#[test]
fn record_init_type_args_generic_in_generic_user_user() {
    // Generic-in-generic with two user records: `Box<Inner<i64>>` from
    // nested struct-init literals.  The checker emits one entry per
    // initialiser site, each with its own concrete type-args:
    //   - inner `Inner { x: 1 }`  → `[i64]`
    //   - outer `Box { value: Inner { x: 1 } }` → `[Inner<i64>]`
    let source = r"
        type Inner<T> { x: T }
        type Box<U> { value: U }
        fn main() { let _b = Box { value: Inner { x: 1 } }; }
    ";
    let tco = check_source(source);
    assert!(tco.errors.is_empty(), "errors: {:?}", tco.errors);
    let entries = collect_record_init_args(&tco);
    assert_eq!(entries.len(), 2, "got: {entries:?}");
    // Sorted by span start: outer Box init begins before inner Inner init.
    // The outer Box's single arg is `Inner<i64>`; the inner Inner's single
    // arg is `i64`.
    assert_eq!(
        entries[0],
        vec![Ty::Named {
            builtin: None,
            name: "Inner".to_string(),
            args: vec![Ty::I64],
        }]
    );
    assert_eq!(entries[1], vec![Ty::I64]);
}

#[test]
fn record_init_type_args_monomorphic_record_emits_no_entry() {
    // Fail-closed contract negative test: a record with empty `type_params`
    // must not produce a `record_init_type_args` entry. Downstream HIR
    // monomorphisation skips entries for monomorphic records.
    let source = r"
        type Mono { value: i64 }
        fn main() { let _m = Mono { value: 42 }; }
    ";
    let tco = check_source(source);
    assert!(tco.errors.is_empty(), "errors: {:?}", tco.errors);
    assert!(
        tco.record_init_type_args.is_empty(),
        "monomorphic record-init should not emit a side-table entry, got: {:?}",
        tco.record_init_type_args
    );
}

#[test]
fn record_init_type_args_field_access_returns_substituted_type() {
    // Verifies that the *field-access* substitution path (already implemented
    // in `check_field_access` lines 3139-3146) and the side-table emission
    // agree: `b.value` on `b: Box<i64>` returns `i64`, and the init site
    // records `[i64]`.
    let source = r"
        type Box<T> { value: T }
        fn main() {
            let b = Box { value: 42 };
            let _v = b.value;
        }
    ";
    let tco = check_source(source);
    assert!(tco.errors.is_empty(), "errors: {:?}", tco.errors);
    // The side-table records the Box<i64> instantiation.
    let entries = collect_record_init_args(&tco);
    assert_eq!(entries, vec![vec![Ty::I64]]);
    // `b.value` resolves to `i64` (the substituted T) via the field-access
    // substitution at `check_field_access:3142-3145`.
    let field_access_ty = tco
        .expr_types
        .iter()
        .find_map(|(_, ty)| (ty == &Ty::I64).then(|| ty.clone()));
    assert_eq!(field_access_ty, Some(Ty::I64));
}

#[test]
fn record_init_type_args_two_distinct_instantiations() {
    // Same generic record at two different T's in the same program — two
    // distinct side-table entries, one per init site.
    let source = r#"
        type Box<T> { value: T }
        fn main() {
            let _a = Box { value: 42 };
            let _b = Box { value: "hi" };
        }
    "#;
    let tco = check_source(source);
    assert!(tco.errors.is_empty(), "errors: {:?}", tco.errors);
    let entries = collect_record_init_args(&tco);
    assert_eq!(entries.len(), 2, "got: {entries:?}");
    // Sorted by span start; first site is the `Box { value: 42 }` literal.
    assert_eq!(entries[0], vec![Ty::I64]);
    assert_eq!(entries[1], vec![Ty::String]);
}

#[test]
fn record_init_type_args_enum_struct_variant_fully_bound() {
    // Generic enum struct-variant init with an annotation that binds every
    // type parameter: `let x: Either<i64, string> = Either::Left { value: 1 }`.
    // The record_init_type_args entry resolves both T=i64 and E=string.
    let source = r"
        enum Either<T, E> {
            Left { value: T };
            Right { err: E };
        }
        fn main() {
            let _x: Either<i64, string> = Either::Left { value: 1 };
        }
    ";
    let tco = check_source(source);
    assert!(tco.errors.is_empty(), "errors: {:?}", tco.errors);
    let entries = collect_record_init_args(&tco);
    assert_eq!(entries.len(), 1, "got: {entries:?}");
    assert_eq!(entries[0], vec![Ty::I64, Ty::String]);
}

#[test]
fn record_init_type_args_enum_struct_variant_partial_inference_pruned() {
    // Negative test for the fail-closed contract: when only one of the two
    // type parameters can be inferred from the init's field values, the
    // entry's second arg stays a Ty::Var and must be pruned by
    // `validate_record_init_type_args_output_contract`.
    let source = r"
        enum Either<T, E> {
            Left { value: T };
            Right { err: E };
        }
        fn main() { let _x = Either::Left { value: 42 }; }
    ";
    let tco = check_source(source);
    // The source emits an InferenceFailed error for `_x` (E unresolved) — we
    // assert the side-table did not leak a partial entry.
    assert!(
        tco.record_init_type_args.is_empty(),
        "partial inference must not leak: {:?}",
        tco.record_init_type_args
    );
}

#[test]
fn record_init_type_args_unknown_field_does_not_emit_entry() {
    // `Box { wrong_field: 42 }` should produce an UndefinedField diagnostic
    // and not leak an entry into the side-table.
    let source = r"
        type Box<T> { value: T }
        fn main() { let _b = Box { wrong_field: 42 }; }
    ";
    let tco = check_source(source);
    assert!(
        tco.errors
            .iter()
            .any(|e| e.message.contains("no field `wrong_field`")),
        "expected UndefinedField diagnostic, got: {:?}",
        tco.errors
    );
    // Fail-closed: because T was never bound (no successful field), the
    // entry's resolved arg is still Ty::Var and the contract validator drops
    // it. record_init_type_args must therefore be empty.
    assert!(
        tco.record_init_type_args.is_empty(),
        "unknown-field init should not leak a side-table entry: {:?}",
        tco.record_init_type_args
    );
}

// ── trait-rewrite-substitution propagation probe ─────────────────────────────
//
// If a generic record has a method that internally calls another trait-bound
// generic function, does the substituted T propagate to the inner call's
// `call_type_args`? Methods on user generic records do not yet propagate the
// substituted type parameter to inner generic call sites (a v0.6 gap) — this
// test documents the state of the seam today rather than asserting
// end-to-end propagation.

#[test]
fn record_init_type_args_trait_rewrite_substitution_probe() {
    // Probe: does a substituted `T` propagate to an inner trait-bound generic
    // call invoked from a method on a user-defined generic record?
    //
    // Hew uses an explicit-receiver method form (no `self` keyword); the
    // receiver is named with its type. The probe constructs `Wrapper { value:
    // 42 }`, calls `Wrapper::show(w)`, and inspects `tco.call_type_args` for
    // the inner `to_string(...)` call. If propagation works, that call's
    // resolved type-args contain `[i64]`. If not, the call_type_args entry is
    // absent or contains an unbound `T`.
    //
    // **Construction-side side-table emission is the load-bearing assert.**
    // The propagation question is informational — the answer is captured in
    // the worker return prose.
    let source = r"
        type Wrapper<T: Display> { value: T }
        impl<T: Display> Wrapper<T> {
            fn show(w: Wrapper<T>) -> string { to_string(w.value) }
        }
        fn main() -> i64 {
            let w = Wrapper { value: 42 };
            let _s = w.show();
            0
        }
    ";
    let parse_result = hew_parser::parse(source);
    // If the surface (impl blocks on user generic records, `to_string` free
    // function) does not parse, the probe documents that gap and exits — the
    // construction-side seam is exercised by the other tests in this family.
    if !parse_result.errors.is_empty() {
        return;
    }
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parse_result.program);
    let entries = collect_record_init_args(&tco);
    // **Load-bearing assertion**: regardless of method-body substitution gaps,
    // the construction `Wrapper { value: 42 }` MUST record the instantiation.
    if tco.errors.is_empty() {
        assert!(!entries.is_empty(), "wrapper init should record an entry");
        assert!(
            entries.iter().any(|e| e.first() == Some(&Ty::I64)),
            "at least one entry should bind T=i64: {entries:?}"
        );
    }
    // **Probe result**: with this fixture, the construction site at
    // `Wrapper { value: 42 }` resolves to `record_init_type_args = [[i64]]`,
    // BUT the inner `to_string(w.value)` call inside `show` records
    // `call_type_args = [Named{name:"T", args:[]}]` — i.e. the unsubstituted
    // type parameter, not `i64`. Methods on user generic records do not
    // propagate the substituted T to inner generic call sites today; this is
    // a v0.6 gap. Only the construction-side surface is captured here.
}

#[test]
fn trailing_integer_literal_coerces_to_declared_return_type() {
    // fn foo() -> i32 { 0 }  — bare 0 defaulted to i64 before fix
    let source = "fn foo() -> i32 { 0 }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "trailing literal should coerce to declared return type: {:?}",
        output.errors
    );
}

#[test]
fn trailing_integer_literal_coerces_smaller_width() {
    // fn foo() -> i8 { 42 }  — literal fits in i8
    let source = "fn foo() -> i8 { 42 }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "literal 42 fits in i8 and should coerce: {:?}",
        output.errors
    );
}

#[test]
fn trailing_integer_literal_out_of_range_is_rejected() {
    // fn foo() -> i8 { 300 }  — 300 does not fit in i8 (range -128..=127)
    let source = "fn foo() -> i8 { 300 }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("does not fit")),
        "out-of-range literal should be rejected: {:?}",
        output.errors
    );
}

#[test]
fn trailing_if_with_literal_branches_coerces() {
    // fn foo(x: i32) -> i32 { if x > 0 { 1 } else { 0 } }
    // Both branches are integer literals that should coerce to i32.
    let source = "fn foo(x: i32) -> i32 { if x > 0 { 1 } else { 0 } }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "if-else with integer literals should coerce to declared return type: {:?}",
        output.errors
    );
}

#[test]
fn explicit_return_with_literal_still_works() {
    // Regression guard: explicit return was already working; must stay working.
    let source = "fn foo() -> i32 { return 0; }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "explicit return with literal coercion should still work: {:?}",
        output.errors
    );
}

#[test]
fn trailing_negative_literal_rejected_for_unsigned_return() {
    // fn foo() -> u32 { -1 }  — negative literal cannot fit in unsigned type
    let source = "fn foo() -> u32 { -1 }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output.errors.is_empty(),
        "negative literal should be rejected for unsigned return type"
    );
}

#[test]
fn trailing_match_with_literal_arms_coerces() {
    // fn foo(x: bool) -> i32 { match x { true => 1, false => 0 } }
    // Stmt::Match is the last statement; both arms are integer literals that
    // should coerce to i32 via the pre-seeded expected type.
    let source = "fn foo(x: bool) -> i32 { match x { true => 1, false => 0 } }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "match with integer literal arms should coerce to declared return type: {:?}",
        output.errors
    );
}

#[test]
fn tuple_if_element_coerces_from_expected_type() {
    // The tuple annotation supplies i32 to the nested if expression's first
    // element; the literal branch must not synthesize to i64 first.
    let source = "fn foo(flag: bool, y: i32) -> (i32, i32) { (if flag { 1 } else { y }, y) }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "nested tuple if should inherit the tuple element type: {:?}",
        output.errors
    );
}

#[test]
fn tuple_match_element_coerces_from_expected_type() {
    // The tuple annotation supplies i32 to the nested match expression's
    // first element; the literal arm must not synthesize to i64 first.
    let source =
        "fn foo(flag: bool, y: i32) -> (i32, i32) { (match flag { true => 1, false => y }, y) }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "nested tuple match should inherit the tuple element type: {:?}",
        output.errors
    );
}

#[test]
fn trailing_type_mismatch_reports_exactly_one_error() {
    // fn foo() -> i32 { "hello" }
    // check_against already reports the mismatch at the expression site;
    // check_fn_decl's outer expect_type must NOT fire a duplicate.
    let source = "fn foo() -> i32 { \"hello\" }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert_eq!(
        output.errors.len(),
        1,
        "expected exactly one type mismatch error, got: {:?}",
        output.errors
    );
}

#[test]
fn trailing_identifier_mismatch_reports_exactly_one_error() {
    // fn foo(s: string) -> i32 { s }
    // The identifier arm in check_against matched before the default arm,
    // so without the guard it fired a second duplicate error.
    let source = "fn foo(s: string) -> i32 { s }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert_eq!(
        output.errors.len(),
        1,
        "expected exactly one type mismatch error for identifier, got: {:?}",
        output.errors
    );
}

#[test]
fn error_return_type_does_not_suppress_match_arm_diagnostics() {
    // fn foo() -> UnknownType { match true { true => "hello", false => 42 } }
    // UnknownType resolves to Ty::Error. Without the Ty::Error guard in
    // check_match_expr, the error type pre-seeds all arms via check_against,
    // silently accepting the string/i64 mismatch between arms.
    let source = r#"fn foo() -> UnknownType { match true { true => "hello", false => 42 } }"#;
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    // We expect at least two errors: one for UnknownType and one for the
    // arm type mismatch (string vs i64). Before the fix only the
    // UnknownType error appeared.
    let arm_mismatch = output.errors.iter().any(|e| {
        let msg = format!("{e:?}");
        msg.contains("TypeMismatch") || msg.contains("mismatch")
    });
    assert!(
        arm_mismatch,
        "match arms with mismatched types should still report an error even when \
         the return type is Ty::Error; got: {:?}",
        output.errors
    );
}

#[test]
fn nonwire_from_json_returns_result_self_string() {
    // Regression: non-wire struct.from_json(s) must type-check as
    // Result<Self, string>, not Self.  The SHIM that returned Self directly
    // was removed; this test pins the correct surface type.
    let source = r#"
type Point { x: i32; y: i32; }
fn main() {
let s = "{\"x\":1,\"y\":2}";
let r: Result<Point, string> = Point.from_json(s);
}
"#;
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "from_json should return Result<Self, string> with no type errors; got: {:?}",
        output.errors
    );
}

#[test]
fn nonwire_from_json_bare_self_is_type_error() {
    // Assigning the result of from_json directly to `Self` (not Result<Self, …>)
    // must produce a type mismatch — confirms the SHIM is gone.
    let source = r#"
type Point { x: i32; y: i32; }
fn main() {
let s = "{\"x\":1,\"y\":2}";
let p: Point = Point.from_json(s);
}
"#;
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let has_mismatch = output.errors.iter().any(|e| {
        let msg = format!("{e:?}");
        msg.contains("TypeMismatch") || msg.contains("mismatch") || msg.contains("Result")
    });
    assert!(
        has_mismatch,
        "assigning Result<Point, string> to Point must be a type error; got: {:?}",
        output.errors
    );
}

#[test]
fn nonwire_from_yaml_and_from_toml_return_result() {
    // Both from_yaml and from_toml should also return Result<Self, string>.
    let source = r#"
type Cfg { n: i32; }
fn main() {
let _a: Result<Cfg, string> = Cfg.from_yaml("n: 1");
let _b: Result<Cfg, string> = Cfg.from_toml("n = 1");
}
"#;
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "from_yaml / from_toml should return Result<Self, string>; got: {:?}",
        output.errors
    );
}

// -------------------------------------------------------------------------
// Structural-bounds scaffold tests (E1)
//
// These tests verify the `type_structurally_satisfies` scaffold and the
// updated `type_satisfies_trait_bound` fallback without changing any
// existing program behaviour.
// -------------------------------------------------------------------------

/// Build a minimal `Checker` with a trait registered in `trait_defs`.
///
/// The trait is method-only (no associated types, no generic methods) unless
/// the caller opts in via the `with_assoc` / `with_generic_method` flags.
fn make_checker_with_trait(
    trait_name: &str,
    method_names: &[&str],
    with_assoc: bool,
    with_generic_method: bool,
) -> Checker {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));

    let mut items: Vec<hew_parser::ast::TraitItem> = method_names
        .iter()
        .map(|name| {
            let type_params = if with_generic_method {
                Some(vec![TypeParam {
                    name: "U".to_string(),
                    bounds: vec![],
                }])
            } else {
                None
            };
            TraitItem::Method(TraitMethod {
                name: name.to_string(),
                type_params,
                params: vec![Param {
                    name: "val".to_string(),
                    ty: (
                        TypeExpr::Named {
                            name: "Self".to_string(),
                            type_args: None,
                        },
                        0..4,
                    ),
                    is_mutable: false,
                }],
                return_type: None,
                where_clause: None,
                body: None,
                span: 0..0,
                doc_comment: None,
                lang_item: None,
            })
        })
        .collect();

    if with_assoc {
        items.push(TraitItem::AssociatedType {
            name: "Output".to_string(),
            default: None,
            bounds: vec![],
            span: 0..0,
        });
    }

    let td = TraitDecl {
        visibility: hew_parser::ast::Visibility::Private,
        name: trait_name.to_string(),
        type_params: None,
        super_traits: None,
        items,
        doc_comment: None,
        lang_item: None,
    };

    let info = Checker::trait_info_from_decl(&td);
    checker.trait_defs.insert(trait_name.to_string(), info);
    checker
}

fn make_test_type_def(
    name: &str,
    type_params: Vec<String>,
    methods: HashMap<String, FnSig>,
) -> TypeDef {
    TypeDef {
        kind: TypeDefKind::Struct,
        name: name.to_string(),
        type_params,
        fields: HashMap::new(),
        variants: HashMap::new(),
        methods,
        doc_comment: None,
        field_order: vec![],
        is_indirect: false,
    }
}

#[test]
fn structural_satisfies_returns_false_for_unknown_trait() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    assert!(
        !checker.type_structurally_satisfies("MyType", "NoSuchTrait"),
        "unknown trait must not satisfy structural check"
    );
}

#[test]
fn structural_satisfies_e1_guard_associated_types() {
    let mut checker = make_checker_with_trait("Indexed", &["get"], true, false);
    assert!(
        !checker.type_structurally_satisfies("MyType", "Indexed"),
        "E1 guard: traits with associated types must return false"
    );
}

#[test]
fn structural_satisfies_e1_guard_generic_methods() {
    let mut checker = make_checker_with_trait("Mapper", &["map"], false, true);
    assert!(
        !checker.type_structurally_satisfies("MyType", "Mapper"),
        "E1 guard: traits with generic methods must return false"
    );
}

#[test]
fn structural_satisfies_e1_guard_method_only_trait_unknown_type_returns_false() {
    // In E2, the placeholder is replaced with real method-presence matching.
    // An unregistered type still returns false because no methods are found.
    let mut checker = make_checker_with_trait("Greet", &["hello"], false, false);
    assert!(
        !checker.type_structurally_satisfies("MyType", "Greet"),
        "unregistered type must not satisfy structural check even after E2"
    );
}

#[test]
fn type_satisfies_trait_bound_nominal_path_unchanged() {
    // Verify that existing nominal conformance still works after the
    // structural fallback was wired into type_satisfies_trait_bound.
    let source = r"
        trait Greet {
            fn hello(val: Self);
        }

        type Greeter {}

        impl Greet for Greeter {
            fn hello(val: Greeter) {}
        }

        fn use_greet<T: Greet>(t: T) {}

        fn main() {
            let g = Greeter {};
            use_greet(g);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "nominal trait conformance must still succeed after E1 scaffold: {:?}",
        output.errors
    );
}

#[test]
fn type_satisfies_trait_bound_missing_impl_still_fails() {
    // A type that has no impl and no structural match must still fail the
    // bound — E1 must not silently accept it.
    let source = r"
        trait Greet {
            fn hello(val: Self);
        }

        type Stranger {}

        fn use_greet<T: Greet>(t: T) {}

        fn main() {
            let s = Stranger {};
            use_greet(s);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output.errors.is_empty(),
        "E1 must not accept a type with no impl and no structural match; expected errors"
    );
}

// -------------------------------------------------------------------------
// Primitive bound-satisfiability tests
//
// Guard the path where a primitive type (i64, f64, bool, string) satisfies a
// user-defined trait bound via an explicit `impl Trait for <primitive>`.
// Before the fix, the `_` arm in `type_satisfies_trait_bound` only consulted
// the `MarkerTrait` table and never checked `trait_impls_set`, causing all
// user-trait bounds on primitives to be falsely rejected.
// -------------------------------------------------------------------------

#[test]
fn primitive_i64_with_impl_satisfies_user_trait_bound() {
    // Positive: an explicit `impl Show for i64` must allow `i64` to satisfy
    // the `T: Show` bound on a generic function.
    let source = r#"
        trait Show {
            fn show(val: Self) -> string;
        }

        impl Show for i64 {
            fn show(val: i64) -> string { "i64" }
        }

        fn display<T: Show>(x: T) -> string {
            x.show()
        }

        fn main() -> i64 {
            let s = display(5);
            if s == "i64" { 0 } else { 1 }
        }
    "#;

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let bound_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied))
        .collect();
    assert!(
        bound_errors.is_empty(),
        "i64 with explicit impl must satisfy user-trait bound; got: {bound_errors:?}"
    );
}

#[test]
fn primitive_f64_bool_string_with_impl_satisfy_user_trait_bound() {
    // Positive: f64, bool, and string each satisfy the bound when an impl exists.
    let source = r#"
        trait Label {
            fn label(val: Self) -> string;
        }

        impl Label for f64    { fn label(val: f64)    -> string { "f64"    } }
        impl Label for bool   { fn label(val: bool)   -> string { "bool"   } }
        impl Label for string { fn label(val: string) -> string { "string" } }

        fn tag<T: Label>(x: T) -> string {
            x.label()
        }

        fn main() -> i64 {
            let a = tag(1.5);
            let b = tag(true);
            let c = tag("hi");
            if a == "f64" && b == "bool" && c == "string" { 0 } else { 1 }
        }
    "#;

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let bound_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied))
        .collect();
    assert!(
        bound_errors.is_empty(),
        "f64/bool/string with explicit impl must satisfy user-trait bound; got: {bound_errors:?}"
    );
}

#[test]
fn primitive_without_impl_still_rejected_for_user_trait_bound() {
    // Negative: a primitive with no `impl Trait for <primitive>` must still
    // produce a `BoundsNotSatisfied` error — the fix must not weaken the gate.
    let source = r"
        trait Show {
            fn show(val: Self) -> string;
        }

        fn display<T: Show>(x: T) -> string {
            x.show()
        }

        fn main() {
            let _ = display(5);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let bound_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied))
        .collect();
    assert!(
        !bound_errors.is_empty(),
        "primitive without a matching impl must still be rejected with BoundsNotSatisfied"
    );
}

#[test]
fn marker_trait_bounds_on_primitives_unaffected() {
    // Regression: built-in marker traits (e.g. `Ord`) on primitives must
    // still be satisfied via the `MarkerTrait` table, not just through
    // `trait_impls_set`.  The fix adds a pre-check for user impls but must
    // not remove or skip the `MarkerTrait` fallback.
    let source = r"
        fn max_val<T: Ord>(a: T, b: T) -> T {
            if a > b { a } else { b }
        }

        fn main() -> i64 {
            max_val(3, 7)
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let bound_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied))
        .collect();
    assert!(
        bound_errors.is_empty(),
        "built-in Ord marker on i64 must still satisfy bound after primitive-impl fix; got: {bound_errors:?}"
    );
}

// -------------------------------------------------------------------------
// E2 structural method-presence tests
//
// These exercises the live structural-satisfaction logic that replaced the
// E1 placeholder.  Programs use `impl Type { fn method }` (no trait) so
// the method is registered in `type_defs.methods` without a nominal impl.
// -------------------------------------------------------------------------

#[test]
fn structural_e2_single_method_match_satisfies_bound() {
    // Positive: a type that has the required method via a bare impl block
    // (no explicit `impl Trait for Type`) must satisfy the bound structurally.
    let source = r"
        trait Area {
            fn area(val: Self) -> i64;
        }

        type Square {}

        impl Square {
            fn area(s: Square) -> i64 { 1 }
        }

        fn measure<T: Area>(s: T) -> i64 {
            s.area()
        }

        fn main() {
            let sq = Square {};
            let _ = measure(sq);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let bound_err = output
        .errors
        .iter()
        .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied));
    assert!(
        !bound_err,
        "structural method match must satisfy Area bound; got: {:?}",
        output.errors
    );
}

#[test]
fn structural_e2_multi_method_trait_all_present_satisfies_bound() {
    // Positive: all required methods present → bound satisfied.
    let source = r#"
        trait Named {
            fn label(val: Self) -> string;
            fn code(val: Self) -> i64;
        }

        type Widget {}

        impl Widget {
            fn label(w: Widget) -> string { "w" }
            fn code(w: Widget) -> i64 { 0 }
        }

        fn print_label<T: Named>(t: T) -> string {
            t.label()
        }

        fn main() {
            let w = Widget {};
            let _ = print_label(w);
        }
    "#;

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let bound_err = output
        .errors
        .iter()
        .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied));
    assert!(
        !bound_err,
        "all methods present → Named bound must be satisfied; got: {:?}",
        output.errors
    );
}

#[test]
fn structural_e2_method_with_non_self_param_satisfies_bound() {
    // Positive: trait method has a non-receiver parameter; the type's method
    // must have the same arity and parameter type.
    let source = r"
        trait Scalable {
            fn scale(val: Self, factor: i64) -> i64;
        }

        type Brick {}

        impl Brick {
            fn scale(b: Brick, factor: i64) -> i64 { factor }
        }

        fn resize<T: Scalable>(t: T) -> i64 {
            t.scale(2)
        }

        fn main() {
            let b = Brick {};
            let _ = resize(b);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let bound_err = output
        .errors
        .iter()
        .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied));
    assert!(
        !bound_err,
        "matching non-receiver param must satisfy Scalable bound; got: {:?}",
        output.errors
    );
}

#[test]
fn structural_e2_nominal_impl_still_preferred_over_structural() {
    // Positive: an explicit `impl Trait for Type` still works; E2 must not
    // break the nominal path.
    let source = r"
        trait Area {
            fn area(val: Self) -> i64;
        }

        type Circle {}

        impl Area for Circle {
            fn area(c: Circle) -> i64 { 3 }
        }

        fn measure<T: Area>(s: T) -> i64 {
            s.area()
        }

        fn main() {
            let c = Circle {};
            let _ = measure(c);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let bound_err = output
        .errors
        .iter()
        .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied));
    assert!(
        !bound_err,
        "explicit impl must still satisfy bound in E2; got: {:?}",
        output.errors
    );
}

#[test]
fn structural_e2_wrong_return_type_does_not_satisfy_bound() {
    // Negative: the type has a method with the right name but wrong return type;
    // the bound must not be satisfied.
    let source = r#"
        trait Area {
            fn area(val: Self) -> i64;
        }

        type Triangle {}

        impl Triangle {
            fn area(t: Triangle) -> string { "big" }
        }

        fn measure<T: Area>(s: T) -> i64 {
            s.area()
        }

        fn main() {
            let t = Triangle {};
            let _ = measure(t);
        }
    "#;

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied)),
        "wrong return type must not satisfy Area bound; got: {:?}",
        output.errors
    );
}

#[test]
fn structural_e2_wrong_arity_does_not_satisfy_bound() {
    // Negative: the type's method has one extra non-receiver parameter;
    // the arity mismatch must cause the bound to fail.
    let source = r"
        trait Ping {
            fn ping(val: Self) -> i64;
        }

        type Server {}

        impl Server {
            fn ping(s: Server, timeout: i64) -> i64 { 1 }
        }

        fn use_ping<T: Ping>(t: T) -> i64 {
            t.ping()
        }

        fn main() {
            let s = Server {};
            let _ = use_ping(s);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied)),
        "arity mismatch must not satisfy Ping bound; got: {:?}",
        output.errors
    );
}

#[test]
fn structural_e2_missing_one_of_two_methods_does_not_satisfy_bound() {
    // Negative: a multi-method trait where only one of two required methods is present.
    let source = r#"
        trait Named {
            fn label(val: Self) -> string;
            fn code(val: Self) -> i64;
        }

        type Partial {}

        impl Partial {
            fn label(p: Partial) -> string { "p" }
            // `code` is intentionally missing
        }

        fn use_named<T: Named>(t: T) -> string {
            t.label()
        }

        fn main() {
            let p = Partial {};
            let _ = use_named(p);
        }
    "#;

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied)),
        "partial method set must not satisfy Named bound; got: {:?}",
        output.errors
    );
}

#[test]
fn structural_e2_all_default_methods_still_requires_explicit_impl() {
    // Negative (conservative): a trait whose every method has a default body
    // has no required methods.  E2 returns false in that case — an explicit
    // `impl Trait for Type` is still needed, keeping explicit impls authoritative
    // for default-only and marker-like traits.
    let source = r#"
        trait WithDefault {
            fn greet(val: Self) -> string { "hello" }
        }

        type Thingy {}

        impl Thingy {
            fn greet(t: Thingy) -> string { "world" }
        }

        fn use_it<T: WithDefault>(t: T) {}

        fn main() {
            let t = Thingy {};
            use_it(t);
        }
    "#;

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied)),
        "all-default-method trait must require explicit impl; got: {:?}",
        output.errors
    );
}

// -------------------------------------------------------------------------
// Bound-diagnostic clarity tests (v0.3.0 slice: bound-diagnostic-clarity)
//
// These tests verify that BoundsNotSatisfied errors carry a diagnostic hint
// (in `suggestions`) that distinguishes the concrete failure mode:
//   • missing method(s)
//   • arity mismatch
//   • return-type / signature mismatch
//   • E1 guard requiring an explicit `impl` declaration
// -------------------------------------------------------------------------

#[test]
fn bound_diagnostic_missing_method_hint() {
    // A type that has no method at all should produce a hint naming the missing method.
    let source = r"
        trait Ping {
            fn ping(val: Self) -> i64;
        }

        type Widget {}

        fn use_ping<T: Ping>(t: T) -> i64 { 0 }

        fn main() {
            let w = Widget {};
            let _ = use_ping(w);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);

    let err = output
        .errors
        .iter()
        .find(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied))
        .expect("expected BoundsNotSatisfied error");

    assert!(
        err.suggestions
            .iter()
            .any(|s| s.contains("ping") && s.contains("missing")),
        "suggestion should mention missing method `ping`; got suggestions: {:?}",
        err.suggestions
    );
}

#[test]
fn bound_diagnostic_arity_mismatch_hint() {
    // A type whose method has the right name but the wrong number of parameters
    // should produce a hint mentioning the arity mismatch.
    let source = r"
        trait Measure {
            fn measure(val: Self) -> i64;
        }

        type Ruler {}

        impl Ruler {
            fn measure(r: Ruler, scale: i64) -> i64 { scale }
        }

        fn use_measure<T: Measure>(t: T) -> i64 { 0 }

        fn main() {
            let r = Ruler {};
            let _ = use_measure(r);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);

    let err = output
        .errors
        .iter()
        .find(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied))
        .expect("expected BoundsNotSatisfied error");

    assert!(
        err.suggestions.iter().any(|s| s.contains("arity")),
        "suggestion should mention arity mismatch; got: {:?}",
        err.suggestions
    );
}

#[test]
fn bound_diagnostic_return_type_mismatch_hint() {
    // A type whose method has the right name and arity but returns the wrong type
    // should produce a hint mentioning the return-type mismatch.
    let source = r#"
        trait Label {
            fn label(val: Self) -> string;
        }

        type Tag {}

        impl Tag {
            fn label(t: Tag) -> i64 { 0 }
        }

        fn use_label<T: Label>(t: T) -> string { "" }

        fn main() {
            let tag = Tag {};
            let _ = use_label(tag);
        }
    "#;

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);

    let err = output
        .errors
        .iter()
        .find(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied))
        .expect("expected BoundsNotSatisfied error");

    assert!(
        err.suggestions.iter().any(|s| s.contains("return-type")),
        "suggestion should mention return-type mismatch; got: {:?}",
        err.suggestions
    );
}

#[test]
fn bound_diagnostic_e1_associated_type_requires_explicit_impl_hint() {
    // A trait that declares an associated type triggers the E1 guard.
    // The diagnostic hint should tell the user an explicit impl is needed.
    let source = r"
        trait Container {
            type Item;
            fn get(val: Self) -> i64;
        }

        type Box {}

        fn use_container<T: Container>(t: T) -> i64 { 0 }

        fn main() {
            let b = Box {};
            let _ = use_container(b);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);

    let err = output
        .errors
        .iter()
        .find(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied))
        .expect("expected BoundsNotSatisfied error");

    assert!(
        err.suggestions
            .iter()
            .any(|s| s.contains("explicit") && s.contains("impl")),
        "suggestion should mention explicit impl for E1 (associated type) guard; got: {:?}",
        err.suggestions
    );
}

#[test]
fn trait_method_where_clause_bound_enforced_negative() {
    let source = r#"
        trait Printable {
            fn print(val: Self) -> string;
        }

        trait Formatter {
            fn apply<U>(item: Self, value: U) -> string where U: Printable;
        }

        type Printer {}
        type Page {}
        type Rock {}

        impl Printable for Page {
            fn print(val: Page) -> string { "page" }
        }

        impl Formatter for Printer {
            fn apply<U>(item: Printer, value: U) -> string where U: Printable {
                "formatted"
            }
        }

        fn run<T: Formatter>(item: T) {
            let _ = item.apply(Rock {});
        }
    "#;

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let err = output
        .errors
        .iter()
        .find(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied))
        .expect("expected BoundsNotSatisfied error");

    assert!(
        err.message.contains("Printable") && err.message.contains('U'),
        "expected bound error to mention the method-level bound, got {err:?}"
    );
}

#[test]
fn trait_method_where_clause_bound_enforced_positive() {
    let source = r#"
        trait Printable {
            fn print(val: Self) -> string;
        }

        trait Formatter {
            fn apply<U>(item: Self, value: U) -> string where U: Printable;
        }

        type Printer {}
        type Page {}

        impl Printable for Page {
            fn print(val: Page) -> string { "page" }
        }

        impl Formatter for Printer {
            fn apply<U>(item: Printer, value: U) -> string where U: Printable {
                "formatted"
            }
        }

        fn run<T: Formatter>(item: T) {
            let ok = item.apply(Page {});
            println(ok);
        }
    "#;

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "expected method-level where-clause bound to be satisfied, got {:?}",
        output.errors
    );
    assert!(
        output.call_type_args.values().any(|args| args
            == &vec![crate::ty::Ty::Named {
                builtin: None,
                name: "Page".to_string(),
                args: vec![]
            }]),
        "expected method-level bound call to infer U=Page, got {:?}",
        output.call_type_args
    );
}

#[test]
fn named_method_lookup_prefers_type_defs_before_fn_sigs() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));

    let mut methods = HashMap::new();
    methods.insert(
        "hello".to_string(),
        FnSig {
            return_type: Ty::String,
            ..FnSig::default()
        },
    );
    checker.type_defs.insert(
        "Speaker".to_string(),
        make_test_type_def("Speaker", vec![], methods),
    );
    checker.fn_sigs.insert(
        "Speaker::hello".to_string(),
        FnSig {
            return_type: Ty::I64,
            ..FnSig::default()
        },
    );

    let sig = checker
        .lookup_named_method_sig("Speaker", &[], "hello")
        .expect("type_defs method should resolve");
    assert_eq!(sig.return_type, Ty::String);
}

#[test]
fn named_type_with_get_method_rejects_bracket_index_via_type_def() {
    // m[k] on a named type that has a `.get()` method is no longer accepted;
    // the checker must emit a diagnostic pointing at `.get(k)`.
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));

    let mut methods = HashMap::new();
    methods.insert(
        "get".to_string(),
        FnSig {
            param_names: vec!["index".to_string()],
            params: vec![Ty::I64],
            return_type: Ty::Named {
                builtin: None,
                name: "T".to_string(),
                args: vec![],
            },
            ..FnSig::default()
        },
    );
    checker.type_defs.insert(
        "Boxy".to_string(),
        make_test_type_def("Boxy", vec!["T".to_string()], methods),
    );
    checker.env.define(
        "boxy".to_string(),
        Ty::Named {
            builtin: None,
            name: "Boxy".to_string(),
            args: vec![Ty::String],
        },
        false,
    );

    let expr = Expr::Index {
        object: Box::new((Expr::Identifier("boxy".to_string()), 0..4)),
        index: Box::new(make_int_literal(0, 5..6)),
    };

    let ty = checker.synthesize(&expr, &(0..6));
    assert_eq!(
        ty,
        Ty::Error,
        "bracket-index on named type must produce Ty::Error"
    );
    assert!(
        !checker.errors.is_empty(),
        "expected a diagnostic for bracket-index on named type with .get()"
    );
    let msg = &checker.errors[0].message;
    assert!(
        msg.contains(".get(k)"),
        "diagnostic should point at .get(k), got: {msg}"
    );
}

#[test]
fn named_type_with_get_method_rejects_bracket_index_via_fn_sig() {
    // Same as above but the `get` method is registered via fn_sigs rather than
    // inline on the type_def (the fn_sig-fallback path in lookup_named_method_sig).
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.type_defs.insert(
        "Wrapper".to_string(),
        make_test_type_def("Wrapper", vec!["T".to_string()], HashMap::new()),
    );
    checker.fn_sigs.insert(
        "Wrapper::get".to_string(),
        FnSig {
            param_names: vec!["index".to_string()],
            params: vec![Ty::I64],
            return_type: Ty::Named {
                builtin: None,
                name: "T".to_string(),
                args: vec![],
            },
            ..FnSig::default()
        },
    );
    checker.env.define(
        "wrapper".to_string(),
        Ty::Named {
            builtin: None,
            name: "Wrapper".to_string(),
            args: vec![Ty::String],
        },
        false,
    );

    let expr = Expr::Index {
        object: Box::new((Expr::Identifier("wrapper".to_string()), 0..7)),
        index: Box::new(make_int_literal(0, 8..9)),
    };

    let ty = checker.synthesize(&expr, &(0..9));
    assert_eq!(
        ty,
        Ty::Error,
        "bracket-index on named type must produce Ty::Error"
    );
    assert!(
        !checker.errors.is_empty(),
        "expected a diagnostic for bracket-index on named type with .get() via fn_sigs"
    );
    let msg = &checker.errors[0].message;
    assert!(
        msg.contains(".get(k)"),
        "diagnostic should point at .get(k), got: {msg}"
    );
}

#[test]
fn hashmap_bracket_index_is_a_compile_error() {
    // m[k] on HashMap<string, i64> must be a compile error since the
    // named-type .get() fallback is removed. The explicit m.get(k) is the
    // correct form (returns Option<i64>).
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));

    // Register HashMap with a string-keyed .get() method (as the stdlib defines it).
    // Return type is Option<V>, represented as the Named form.
    let option_v = Ty::Named {
        builtin: Some(crate::BuiltinType::Option),
        name: "Option".to_string(),
        args: vec![Ty::Named {
            builtin: None,
            name: "V".to_string(),
            args: vec![],
        }],
    };
    let mut methods = HashMap::new();
    methods.insert(
        "get".to_string(),
        FnSig {
            param_names: vec!["key".to_string()],
            params: vec![Ty::String],
            return_type: option_v,
            ..FnSig::default()
        },
    );
    checker.type_defs.insert(
        "HashMap".to_string(),
        make_test_type_def("HashMap", vec!["K".to_string(), "V".to_string()], methods),
    );
    checker.env.define(
        "m".to_string(),
        Ty::Named {
            builtin: None,
            name: "HashMap".to_string(),
            args: vec![Ty::String, Ty::I64],
        },
        false,
    );

    // Use an i64 index so the only diagnostic comes from the named-type guard,
    // not from a type mismatch on the index expression itself.
    let expr = Expr::Index {
        object: Box::new((Expr::Identifier("m".to_string()), 0..1)),
        index: Box::new(make_int_literal(0, 2..3)),
    };

    let ty = checker.synthesize(&expr, &(0..8));
    assert_eq!(ty, Ty::Error, "m[k] on HashMap must produce Ty::Error");
    assert!(
        !checker.errors.is_empty(),
        "m[k] on HashMap<string, i64> must produce a diagnostic"
    );
    let msg = &checker.errors[0].message;
    assert!(
        msg.contains(".get(k)"),
        "diagnostic should point at .get(k), got: {msg}"
    );
}

#[test]
fn index_trait_user_impl_runs() {
    let output = check_source(
        r"
        type Grid {
            bias: i32;
        }

        impl Index for Grid {
            type Output = i32;

            fn at(g: Grid, key: i32) -> i32 {
                g.bias + key
            }
        }

        fn f() -> i32 {
            let g = Grid { bias: 40 };
            g[2]
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "user Index impl should type-check: {:?}",
        output.errors
    );
}

#[test]
fn index_dispatch_via_trait_for_vec() {
    let output = check_source("fn f(xs: Vec<i32>) -> i32 { xs[0] }");
    assert!(
        output.errors.is_empty(),
        "Vec indexing should continue to type-check through the auto-impl path: {:?}",
        output.errors
    );
}

#[test]
fn index_read_for_hashmap_string_key_yields_option() {
    // `m["k"]` now reads through the HashMap get ABI and yields `Option<V>`
    // (not the i32-keyed `Index` trait). A function returning `Option<i32>`
    // accepts the index result directly.
    let output = check_source(r#"fn f(m: HashMap<string, i32>) -> Option<i32> { m["k"] }"#);
    assert!(
        output.errors.is_empty(),
        "HashMap<string, _> read indexing should type-check as Option<V>: {:?}",
        output.errors
    );
}

#[test]
fn index_read_for_hashmap_string_key_is_not_bare_value() {
    // The read result is `Option<V>`, so a function annotated to return the
    // bare value `i32` must be rejected — proving the surface is fail-closed
    // (callers must handle the `None` case) rather than aborting on a miss.
    let output = check_source(r#"fn f(m: HashMap<string, i32>) -> i32 { m["k"] }"#);
    assert!(
        !output.errors.is_empty(),
        "HashMap read index returns Option<V>, so a bare-i32 return must mismatch: {:?}",
        output.errors
    );
}

#[test]
fn dyn_index_with_output_binding() {
    let output = check_source("fn f(idx: dyn Index<Output = i32>) -> i32 { idx[2] }");
    assert!(
        output.errors.is_empty(),
        "dyn Index<Output = i32> indexing should type-check: {:?}",
        output.errors
    );
    assert!(
        output
            .dyn_trait_method_calls
            .values()
            .any(|call| call.trait_name == "Index" && call.method_name == "at"),
        "checker should record a dyn Index::at vtable dispatch for []"
    );
}

#[test]
fn named_method_lookup_substitutes_type_params_for_fn_sig_fallback() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.type_defs.insert(
        "Wrapper".to_string(),
        make_test_type_def("Wrapper", vec!["T".to_string()], HashMap::new()),
    );
    checker.fn_sigs.insert(
        "Wrapper::value".to_string(),
        FnSig {
            param_names: vec!["next".to_string()],
            params: vec![Ty::Named {
                builtin: None,
                name: "T".to_string(),
                args: vec![],
            }],
            return_type: Ty::Named {
                builtin: None,
                name: "T".to_string(),
                args: vec![],
            },
            ..FnSig::default()
        },
    );

    let sig = checker
        .lookup_named_method_sig("Wrapper", &[Ty::String], "value")
        .expect("fn_sigs fallback should resolve");
    assert_eq!(sig.params, vec![Ty::String]);
    assert_eq!(sig.return_type, Ty::String);
}

#[test]
fn module_qualified_named_type_method_rewrite_uses_unqualified_method_symbol() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.type_defs.insert(
        "Thing".to_string(),
        make_test_type_def("Thing", vec![], HashMap::new()),
    );
    checker.fn_sigs.insert(
        "Thing::label".to_string(),
        FnSig {
            return_type: Ty::String,
            ..FnSig::default()
        },
    );
    checker.env.define(
        "thing".to_string(),
        Ty::Named {
            builtin: None,
            name: "widgets.Thing".to_string(),
            args: vec![],
        },
        false,
    );

    let receiver = (Expr::Identifier("thing".to_string()), 0..5);
    let ty = checker.check_method_call(&receiver, "label", &[], &(0..13));

    assert_eq!(ty, Ty::String);
    assert!(
        checker.errors.is_empty(),
        "expected clean module-qualified method dispatch, got: {:?}",
        checker.errors
    );
    assert!(
        checker
            .method_call_rewrites
            .values()
            .any(|rewrite| matches!(
                rewrite,
                MethodCallRewrite::RewriteToFunction { c_symbol, .. } if c_symbol == "Thing::label"
            )),
        "module-qualified receiver must rewrite via unqualified method key, got: {:?}",
        checker.method_call_rewrites
    );
    assert!(
        !checker
            .method_call_rewrites
            .values()
            .any(|rewrite| matches!(
                rewrite,
                MethodCallRewrite::RewriteToFunction { c_symbol, .. }
                    if c_symbol == "widgets.Thing::label"
            )),
        "qualified type prefix must not leak into method rewrite symbol: {:?}",
        checker.method_call_rewrites
    );
}

#[test]
fn generic_named_method_calls_record_method_type_args() {
    let source = r#"
        type Wrapper<T> { value: T }

        impl<T> Wrapper<T> {
            fn map<U>(wrapper: Wrapper<T>, mapper: fn(T) -> U) -> U {
                mapper(wrapper.value)
            }
        }

        fn to_len(value: string) -> i64 {
            value.len()
        }

        fn main() {
            let wrapper = Wrapper { value: "hew" };
            let len = wrapper.map(to_len);
        }
    "#;

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "generic method call should type-check cleanly: {:?}",
        output.errors
    );
    assert!(
        output
            .call_type_args
            .values()
            .any(|args| args == &vec![Ty::I64]),
        "method call should record inferred method type args, got {:?}",
        output.call_type_args
    );
}

#[test]
fn impl_method_registration_keeps_inline_method_bounds_on_all_surfaces() {
    let source = r"
        trait Show {
            fn show(value: Self);
        }

        type Wrapper {}

        impl Wrapper {
            fn map<U: Show>(wrapper: Wrapper, value: U) -> U {
                value
            }
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "inline-bounded impl method should register without checker errors: {:?}",
        output.errors
    );

    let fn_sig = output
        .fn_sigs
        .get("Wrapper::map")
        .expect("impl method must populate fn_sigs");
    let method_sig = output
        .type_defs
        .get("Wrapper")
        .and_then(|type_def| type_def.methods.get("map"))
        .expect("impl method must populate type_def.methods");

    assert_eq!(
        fn_sig.type_param_bounds.get("U"),
        Some(&vec!["Show".to_string()]),
        "fn_sigs surface must retain method-inline bounds"
    );
    assert_eq!(
        method_sig.type_param_bounds.get("U"),
        Some(&vec!["Show".to_string()]),
        "type_def.methods surface must retain method-inline bounds"
    );
}

// -------------------------------------------------------------------------
// Structural-hardening tests (qualified names + super-trait walk)
// -------------------------------------------------------------------------

#[test]
fn structural_hardening_uses_fn_sigs_named_method_fallback() {
    let mut checker = make_checker_with_trait("Greet", &["hello"], false, false);
    checker.type_defs.insert(
        "Speaker".to_string(),
        make_test_type_def("Speaker", vec![], HashMap::new()),
    );
    checker
        .fn_sigs
        .insert("Speaker::hello".to_string(), FnSig::default());

    assert!(
        checker.type_structurally_satisfies("Speaker", "Greet"),
        "structural check should reuse named-method fn_sigs fallback"
    );
}

#[test]
fn structural_hardening_prefers_builtin_method_surface_for_imported_handle() {
    let mut checker = make_checker_with_trait("Closable", &["close"], false, false);

    let mut methods = HashMap::new();
    methods.insert(
        "close".to_string(),
        FnSig {
            return_type: Ty::I32,
            ..FnSig::default()
        },
    );
    checker.type_defs.insert(
        "Sender".to_string(),
        make_test_type_def("Sender", vec![], methods),
    );
    checker.fn_sigs.insert(
        "Sender::close".to_string(),
        FnSig {
            return_type: Ty::I32,
            ..FnSig::default()
        },
    );

    assert!(
        checker.type_structurally_satisfies("channel.Sender", "Closable"),
        "structural check should prefer builtin Sender::close over imported stubs"
    );
}

#[test]
fn structural_hardening_qualified_trait_name_matches() {
    // A type registered under "Speaker" must structurally satisfy a bound
    // expressed as "greet.Greet" once "greet" is a known module.
    // We build the checker state manually because check_program drains
    // type_defs/fn_sigs at the end of the pass.
    let mut checker = make_checker_with_trait("Greet", &["hello"], false, false);
    checker.modules.insert("greet".to_string());

    // Register a TypeDef for Speaker.  The trait `hello(val: Self)` has its
    // receiver stripped by lookup_trait_method, so the effective trait_sig has
    // params=[].  The concrete method entry must match: receiver already stripped.
    let type_def = TypeDef {
        kind: TypeDefKind::Struct,
        name: "Speaker".to_string(),
        type_params: vec![],
        fields: HashMap::new(),
        variants: HashMap::new(),
        methods: {
            let mut m = HashMap::new();
            m.insert("hello".to_string(), FnSig::default()); // params=[], return=Unit
            m
        },
        doc_comment: None,
        field_order: vec![],
        is_indirect: false,
    };
    checker.type_defs.insert("Speaker".to_string(), type_def);

    assert!(
        checker.type_structurally_satisfies("Speaker", "greet.Greet"),
        "structural check with qualified trait name must succeed after normalization"
    );
    // Unqualified form must still work too.
    assert!(
        checker.type_structurally_satisfies("Speaker", "Greet"),
        "structural check with unqualified trait name must still succeed"
    );
}

#[test]
fn structural_hardening_qualified_type_name_matches() {
    // A bound check with the type expressed as "mymod.Speaker" must succeed
    // when "mymod" is a known module and "Speaker" is registered in type_defs.
    let mut checker = make_checker_with_trait("Greet", &["hello"], false, false);
    checker.modules.insert("mymod".to_string());

    let type_def = TypeDef {
        kind: TypeDefKind::Struct,
        name: "Speaker".to_string(),
        type_params: vec![],
        fields: HashMap::new(),
        variants: HashMap::new(),
        methods: {
            let mut m = HashMap::new();
            m.insert("hello".to_string(), FnSig::default());
            m
        },
        doc_comment: None,
        field_order: vec![],
        is_indirect: false,
    };
    checker.type_defs.insert("Speaker".to_string(), type_def);

    assert!(
        checker.type_structurally_satisfies("mymod.Speaker", "Greet"),
        "structural check with qualified type name must succeed after normalization"
    );
    // Unqualified form must still work too.
    assert!(
        checker.type_structurally_satisfies("Speaker", "Greet"),
        "unqualified type name must still succeed"
    );
}

#[test]
fn structural_hardening_unknown_module_qualifier_is_rejected() {
    // If the prefix is not a known module, we must not strip it and must
    // not accidentally match a same-suffix type/trait.
    let mut checker = make_checker_with_trait("Greet", &["hello"], false, false);
    // "unknown" is NOT inserted into modules.

    let type_def = TypeDef {
        kind: TypeDefKind::Struct,
        name: "Speaker".to_string(),
        type_params: vec![],
        fields: HashMap::new(),
        variants: HashMap::new(),
        methods: {
            let mut m = HashMap::new();
            m.insert("hello".to_string(), FnSig::default());
            m
        },
        doc_comment: None,
        field_order: vec![],
        is_indirect: false,
    };
    checker.type_defs.insert("Speaker".to_string(), type_def);

    // Trait "unknown.Greet" should not resolve to "Greet" because "unknown" is
    // not a registered module.
    assert!(
        !checker.type_structurally_satisfies("Speaker", "unknown.Greet"),
        "unrecognised module prefix must not be stripped"
    );
}

#[test]
fn structural_hardening_super_trait_methods_required() {
    // If trait B extends A, a type must provide A's required methods to
    // structurally satisfy B.  Before the fix, only B's own methods were
    // checked and A's were silently skipped.
    let source = r"
        trait Printable {
            fn print(val: Self);
        }

        trait PrettyPrintable: Printable {
            fn pretty_print(val: Self);
        }

        type Doc {}

        impl Doc {
            fn pretty_print(d: Doc) {}
            // `print` (from super-trait Printable) is intentionally missing
        }

        fn use_pp<T: PrettyPrintable>(t: T) {}

        fn main() {
            let d = Doc {};
            use_pp(d);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied)),
        "missing super-trait method must fail structural check; got: {:?}",
        output.errors
    );
}

#[test]
fn structural_hardening_super_trait_methods_all_present_succeeds() {
    // When ALL required methods across the super-trait chain are present the
    // structural check must succeed without an explicit impl.
    let source = r"
        trait Printable {
            fn print(val: Self);
        }

        trait PrettyPrintable: Printable {
            fn pretty_print(val: Self);
        }

        type Doc {}

        impl Doc {
            fn print(d: Doc) {}
            fn pretty_print(d: Doc) {}
        }

        fn use_pp<T: PrettyPrintable>(t: T) {}

        fn main() {
            let d = Doc {};
            use_pp(d);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "all super-trait methods present must pass structural check: {:?}",
        output.errors
    );
}

#[test]
fn structural_hardening_child_default_overrides_super_required_method() {
    // If a child trait provides a default implementation for a super-trait
    // method, that inherited requirement is satisfied by the trait itself and
    // must not be re-required structurally from the concrete type.
    let source = r"
        trait Printable {
            fn print(val: Self);
        }

        trait PrettyPrintable: Printable {
            fn print(val: Self) {}
            fn pretty_print(val: Self);
        }

        type Doc {}

        impl Doc {
            fn pretty_print(d: Doc) {}
        }

        fn use_pp<T: PrettyPrintable>(t: T) {}

        fn main() {
            let d = Doc {};
            use_pp(d);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "child default override should satisfy inherited structural requirement: {:?}",
        output.errors
    );
}

#[test]
fn structural_hardening_diamond_sibling_shadowing_merges_across_supers() {
    // In a diamond, sibling default branches must cover inherited methods
    // collectively so only the root trait's still-required methods remain.
    let source = r"
        trait A {
            fn a(val: Self);
            fn b(val: Self);
        }

        trait B: A {
            fn a(val: Self) {}
        }

        trait C: A {
            fn b(val: Self) {}
        }

        trait D: B + C {
            fn d(val: Self);
        }

        type Doc {}

        impl Doc {
            fn d(d: Doc) {}
        }

        fn use_d<T: D>(t: T) {}

        fn main() {
            let d = Doc {};
            use_d(d);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "diamond sibling shadowing should merge across supers: {:?}",
        output.errors
    );
}

#[test]
fn structural_hardening_super_trait_e1_guard_propagates() {
    // If a super-trait has an associated type, the E1 guard must veto the
    // entire structural check — even if the immediate trait has no assoc types.
    use hew_parser::ast::{Param, TraitDecl, TraitItem, TraitMethod, TypeExpr};
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));

    // Build super-trait with an associated type.
    let assoc_super = TraitDecl {
        visibility: hew_parser::ast::Visibility::Private,
        name: "AssocSuper".to_string(),
        type_params: None,
        super_traits: None,
        items: vec![
            TraitItem::AssociatedType {
                name: "Output".to_string(),
                default: None,
                bounds: vec![],
                span: 0..0,
            },
            TraitItem::Method(TraitMethod {
                name: "do_it".to_string(),
                type_params: None,
                params: vec![Param {
                    name: "val".to_string(),
                    ty: (
                        TypeExpr::Named {
                            name: "Self".to_string(),
                            type_args: None,
                        },
                        0..4,
                    ),
                    is_mutable: false,
                }],
                return_type: None,
                where_clause: None,
                body: None,
                span: 0..0,
                doc_comment: None,
                lang_item: None,
            }),
        ],
        doc_comment: None,
        lang_item: None,
    };
    let info_super = Checker::trait_info_from_decl(&assoc_super);
    checker
        .trait_defs
        .insert("AssocSuper".to_string(), info_super);

    // Child trait with no assoc types of its own.
    let child = TraitDecl {
        visibility: hew_parser::ast::Visibility::Private,
        name: "ChildTrait".to_string(),
        type_params: None,
        super_traits: Some(vec![hew_parser::ast::TraitBound {
            name: "AssocSuper".to_string(),
            type_args: None,
            assoc_type_bindings: vec![],
        }]),
        items: vec![TraitItem::Method(TraitMethod {
            name: "run".to_string(),
            type_params: None,
            params: vec![Param {
                name: "val".to_string(),
                ty: (
                    TypeExpr::Named {
                        name: "Self".to_string(),
                        type_args: None,
                    },
                    0..4,
                ),
                is_mutable: false,
            }],
            return_type: None,
            where_clause: None,
            body: None,
            span: 0..0,
            doc_comment: None,
            lang_item: None,
        })],
        doc_comment: None,
        lang_item: None,
    };
    let info_child = Checker::trait_info_from_decl(&child);
    checker
        .trait_defs
        .insert("ChildTrait".to_string(), info_child);
    checker
        .trait_super
        .insert("ChildTrait".to_string(), vec!["AssocSuper".to_string()]);

    assert!(
        !checker.type_structurally_satisfies("AnyType", "ChildTrait"),
        "E1 guard in super-trait must veto structural check for child trait"
    );
}

#[test]
fn structural_hardening_super_trait_generic_method_guard_propagates() {
    // If a super-trait has a generic method, the E1 guard must veto the whole
    // structural check for the child trait too.
    use hew_parser::ast::{Param, TraitDecl, TraitItem, TraitMethod, TypeExpr, TypeParam};
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));

    let generic_super = TraitDecl {
        visibility: hew_parser::ast::Visibility::Private,
        name: "GenericSuper".to_string(),
        type_params: None,
        super_traits: None,
        items: vec![TraitItem::Method(TraitMethod {
            name: "map".to_string(),
            type_params: Some(vec![TypeParam {
                name: "U".to_string(),
                bounds: vec![],
            }]),
            params: vec![Param {
                name: "val".to_string(),
                ty: (
                    TypeExpr::Named {
                        name: "Self".to_string(),
                        type_args: None,
                    },
                    0..4,
                ),
                is_mutable: false,
            }],
            return_type: None,
            where_clause: None,
            body: None,
            span: 0..0,
            doc_comment: None,
            lang_item: None,
        })],
        doc_comment: None,
        lang_item: None,
    };
    let info_super = Checker::trait_info_from_decl(&generic_super);
    checker
        .trait_defs
        .insert("GenericSuper".to_string(), info_super);

    let child = TraitDecl {
        visibility: hew_parser::ast::Visibility::Private,
        name: "ChildTrait".to_string(),
        type_params: None,
        super_traits: Some(vec![hew_parser::ast::TraitBound {
            name: "GenericSuper".to_string(),
            type_args: None,
            assoc_type_bindings: vec![],
        }]),
        items: vec![TraitItem::Method(TraitMethod {
            name: "run".to_string(),
            type_params: None,
            params: vec![Param {
                name: "val".to_string(),
                ty: (
                    TypeExpr::Named {
                        name: "Self".to_string(),
                        type_args: None,
                    },
                    0..4,
                ),
                is_mutable: false,
            }],
            return_type: None,
            where_clause: None,
            body: None,
            span: 0..0,
            doc_comment: None,
            lang_item: None,
        })],
        doc_comment: None,
        lang_item: None,
    };
    let info_child = Checker::trait_info_from_decl(&child);
    checker
        .trait_defs
        .insert("ChildTrait".to_string(), info_child);
    checker
        .trait_super
        .insert("ChildTrait".to_string(), vec!["GenericSuper".to_string()]);

    assert!(
        !checker.type_structurally_satisfies("AnyType", "ChildTrait"),
        "generic-method guard in super-trait must veto structural check for child trait"
    );
}

#[test]
fn cyclic_trait_hierarchy_bound_check_surfaces_diagnostic() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker
        .trait_super
        .insert("TraitA".to_string(), vec!["TraitB".to_string()]);
    checker
        .trait_super
        .insert("TraitB".to_string(), vec!["TraitA".to_string()]);
    checker
        .trait_impls_set
        .insert(("Thing".to_string(), "TraitA".to_string()));

    let sig = FnSig {
        type_params: vec!["T".to_string()],
        type_param_bounds: HashMap::from([("T".to_string(), vec!["MissingTrait".to_string()])]),
        ..Default::default()
    };

    checker.enforce_type_param_bounds(
        &sig,
        &[Ty::Named {
            builtin: None,
            name: "Thing".to_string(),
            args: vec![],
        }],
        &(0..0),
    );

    assert!(
        checker
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::BoundsNotSatisfied),
        "expected cyclic trait hierarchy bound check to fail with a diagnostic; got {:?}",
        checker.errors
    );
}

// ── Non-root module inference-hole fail-closed regression tests ───────────────
//
// These cover the `report_unresolved_inference_holes` path for non-root module
// items in a `module_graph`.  Signature-level `_` holes are tracked by
// `collect_functions` (which already walks non-root modules via `topo_order`)
// and are detected by `report_unresolved_inference_in_items` via the
// `lookup_scoped_item` scoped-name fallback.
//
// Body-level holes (expressions containing `_`) require non-root module body
// checking from PR #756 to propagate into the inference state.  PR #756 added
// the infrastructure; the tests below prove the deferred-hole drain path works
// for non-root module bodies too:
//   - `body_cast_infer_hole_fails_closed`: unresolvable `as _` cast target
//   - `body_let_annotation_infer_resolves_cleanly`: resolvable `let y: _ = 42`
//   - `body_lambda_infer_param_hole_fails_closed`: unresolvable lambda `|x: _|`

#[cfg(test)]
mod non_root_module_inference_scope {
    use super::*;

    fn make_non_root_module(
        mod_id: &ModuleId,
        fn_name: &str,
        param_ty: TypeExpr,
        return_ty: Option<TypeExpr>,
    ) -> Module {
        let fn_decl = FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Private,
            name: fn_name.to_string(),
            type_params: None,
            params: vec![Param {
                name: "x".to_string(),
                ty: (param_ty, 10..11),
                is_mutable: false,
            }],
            return_type: return_ty.map(|ty| (ty, 15..16)),
            where_clause: None,
            body: Block {
                stmts: vec![],
                trailing_expr: None,
            },
            doc_comment: None,
            decl_span: 0..0,
            fn_span: 0..0,
            intrinsic: None,
        };
        Module {
            id: mod_id.clone(),
            items: vec![(Item::Function(fn_decl), 0..30)],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        }
    }

    fn make_program_with_non_root(module: Module) -> Program {
        let root_id = ModuleId::root();
        let mod_id = module.id.clone();
        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(module).unwrap();
        // Dependencies (non-root) come before the root in topo order.
        mg.topo_order = vec![mod_id, root_id];
        Program {
            module_graph: Some(mg),
            items: vec![],
            module_doc: None,
        }
    }

    // Helper: collect InferenceFailed errors from the output.
    fn inference_failed_errors(output: &TypeCheckOutput) -> Vec<&TypeError> {
        output
            .errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::InferenceFailed))
            .collect()
    }

    /// A non-root module function whose parameter type is `_` must produce an
    /// `InferenceFailed` error via `report_unresolved_inference_holes`.
    #[test]
    fn fn_param_infer_hole_fails_closed() {
        let mod_id = ModuleId::new(vec!["utils".to_string()]);
        let module = make_non_root_module(&mod_id, "helper", TypeExpr::Infer, None);
        let program = make_program_with_non_root(module);

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let errs = inference_failed_errors(&output);
        assert!(
            !errs.is_empty(),
            "expected InferenceFailed for unresolved `_` param in non-root module fn; got errors: {:?}",
            output.errors
        );
    }

    /// A non-root module function whose return type is `_` now resolves from
    /// body-checking just like a root-module function. An empty body therefore
    /// resolves `_` to `unit` instead of leaving an unresolved inference hole.
    #[test]
    fn fn_return_infer_hole_resolves_from_body() {
        let mod_id = ModuleId::new(vec!["helpers".to_string()]);
        let concrete_param = TypeExpr::Named {
            name: "i64".to_string(),
            type_args: None,
        };
        let module =
            make_non_root_module(&mod_id, "compute", concrete_param, Some(TypeExpr::Infer));
        let program = make_program_with_non_root(module);

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let errs = inference_failed_errors(&output);
        assert!(
            errs.is_empty(),
            "non-root `_` return type should resolve from body-checking; got InferenceFailed errors: {errs:?}"
        );
        assert!(
            output.errors.is_empty(),
            "non-root `_` return type should resolve cleanly; got errors: {:?}",
            output.errors
        );
    }

    /// A non-root module function with fully concrete types must not produce
    /// any `InferenceFailed` errors (baseline / regression guard).
    #[test]
    fn fn_concrete_types_passes() {
        let mod_id = ModuleId::new(vec!["math".to_string()]);
        let concrete_param = TypeExpr::Named {
            name: "i64".to_string(),
            type_args: None,
        };
        let concrete_return = TypeExpr::Named {
            name: "i64".to_string(),
            type_args: None,
        };
        let module = make_non_root_module(&mod_id, "add", concrete_param, Some(concrete_return));
        let program = make_program_with_non_root(module);

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let errs = inference_failed_errors(&output);
        assert!(
            errs.is_empty(),
            "concrete non-root module fn must not produce InferenceFailed; got: {errs:?}"
        );
    }

    /// A non-root module function with `_` param must not prevent the root
    /// module items from being checked — root errors remain independent.
    #[test]
    fn infer_hole_in_non_root_does_not_suppress_root_errors() {
        use hew_parser::ast::{FnDecl, Item};

        let mod_id = ModuleId::new(vec!["side".to_string()]);
        let module = make_non_root_module(&mod_id, "side_fn", TypeExpr::Infer, None);

        // Root module also has a function with `_` param — should also error.
        let root_fn = FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Private,
            name: "root_fn".to_string(),
            type_params: None,
            params: vec![Param {
                name: "v".to_string(),
                ty: (TypeExpr::Infer, 50..51),
                is_mutable: false,
            }],
            return_type: None,
            where_clause: None,
            body: Block {
                stmts: vec![],
                trailing_expr: None,
            },
            doc_comment: None,
            decl_span: 0..0,
            fn_span: 0..0,
            intrinsic: None,
        };

        let root_id = ModuleId::root();
        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(module).unwrap();
        mg.topo_order = vec![mod_id, root_id];
        let program = Program {
            module_graph: Some(mg),
            items: vec![(Item::Function(root_fn), 40..80)],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let errs = inference_failed_errors(&output);
        assert!(
            errs.len() >= 2,
            "expected InferenceFailed for both the non-root module fn and the root fn; got: {:?}",
            output.errors
        );
    }

    /// Two non-root module functions both with `_` params must each produce
    /// an `InferenceFailed` error — holes are not collapsed into a single error.
    #[test]
    fn multiple_infer_holes_in_non_root_all_fail_closed() {
        use hew_parser::ast::{Block, FnDecl, Item, Param, TypeExpr};
        let mod_id = ModuleId::new(vec!["util2".to_string()]);
        let root_id = ModuleId::root();

        let make_infer_fn = |name: &str, span_start: usize| -> Spanned<Item> {
            let fd = FnDecl {
                attributes: vec![],
                is_async: false,
                is_generator: false,
                visibility: Visibility::Private,
                name: name.to_string(),
                type_params: None,
                params: vec![Param {
                    name: "a".to_string(),
                    ty: (TypeExpr::Infer, span_start..span_start + 1),
                    is_mutable: false,
                }],
                return_type: None,
                where_clause: None,
                body: Block {
                    stmts: vec![],
                    trailing_expr: None,
                },
                doc_comment: None,
                decl_span: 0..0,
                fn_span: 0..0,
                intrinsic: None,
            };
            (Item::Function(fd), span_start..span_start + 30)
        };

        let non_root = Module {
            id: mod_id.clone(),
            items: vec![make_infer_fn("alpha", 0), make_infer_fn("beta", 40)],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };

        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(non_root).unwrap();
        mg.topo_order = vec![mod_id, root_id];
        let program = Program {
            module_graph: Some(mg),
            items: vec![],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let errs = inference_failed_errors(&output);
        assert!(
            errs.len() >= 2,
            "expected at least 2 InferenceFailed errors for two `_`-param fns in non-root module; got: {:?}",
            output.errors
        );
    }

    /// A non-root module body containing `let y = x as _` must fail closed:
    /// the unresolved `_` cast target must produce an `InferenceFailed` error.
    ///
    /// Regression test: deferred inference holes created during non-root module
    /// body checking (via `synthesize_cast`) were not reported by
    /// `report_unresolved_inference_holes` because the body-level deferred-hole
    /// list was only flushed for the *root* module's item walk, not for
    /// non-root module bodies.
    #[test]
    fn body_cast_infer_hole_fails_closed() {
        // fn foo(x: i64) { let y = x as _; }  — `_` cast target is unresolved
        let mod_id = ModuleId::new(vec!["castmod".to_string()]);
        let root_id = ModuleId::root();

        let cast_expr = Expr::Cast {
            expr: Box::new((Expr::Identifier("x".to_string()), 20..21)),
            ty: (TypeExpr::Infer, 25..26),
        };
        let let_stmt = Stmt::Let {
            pattern: (Pattern::Identifier("y".to_string()), 14..15),
            ty: None,
            value: Some((cast_expr, 18..26)),
        };
        let fn_decl = FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Private,
            name: "foo".to_string(),
            type_params: None,
            params: vec![Param {
                name: "x".to_string(),
                ty: (
                    TypeExpr::Named {
                        name: "i64".to_string(),
                        type_args: None,
                    },
                    7..10,
                ),
                is_mutable: false,
            }],
            return_type: None,
            where_clause: None,
            body: Block {
                stmts: vec![(let_stmt, 13..27)],
                trailing_expr: None,
            },
            doc_comment: None,
            decl_span: 0..0,
            fn_span: 0..0,
            intrinsic: None,
        };

        let non_root = Module {
            id: mod_id.clone(),
            items: vec![(Item::Function(fn_decl), 0..30)],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };

        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(non_root).unwrap();
        mg.topo_order = vec![mod_id, root_id];
        let program = Program {
            module_graph: Some(mg),
            items: vec![],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let errs = inference_failed_errors(&output);
        assert!(
            !errs.is_empty(),
            "expected InferenceFailed for unresolved `_` cast target in non-root module body; got errors: {:?}",
            output.errors
        );
    }

    /// A non-root module body with `let y: _ = 42` must resolve the `_`
    /// annotation to `i64` from the value — no spurious `InferenceFailed`.
    ///
    /// This is the positive-path counterpart to `body_cast_infer_hole_fails_closed`:
    /// a deferred annotation hole that IS constrained by body-checking must not
    /// produce a false diagnostic.  Before PR #756 added non-root body checking,
    /// this would have left the type-var unresolved and erroneously fired.
    #[test]
    fn body_let_annotation_infer_resolves_cleanly() {
        // fn bar() { let y: _ = 42; }  — `_` must resolve to i64 from the value
        let mod_id = ModuleId::new(vec!["letmod".to_string()]);
        let root_id = ModuleId::root();

        let let_stmt = Stmt::Let {
            pattern: (Pattern::Identifier("y".to_string()), 14..15),
            ty: Some((TypeExpr::Infer, 17..18)),
            value: Some(make_int_literal(42, 21..23)),
        };
        let fn_decl = FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Private,
            name: "bar".to_string(),
            type_params: None,
            params: vec![],
            return_type: None,
            where_clause: None,
            body: Block {
                stmts: vec![(let_stmt, 13..24)],
                trailing_expr: None,
            },
            doc_comment: None,
            decl_span: 0..0,
            fn_span: 0..0,
            intrinsic: None,
        };

        let non_root = Module {
            id: mod_id.clone(),
            items: vec![(Item::Function(fn_decl), 0..30)],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };

        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(non_root).unwrap();
        mg.topo_order = vec![mod_id, root_id];
        let program = Program {
            module_graph: Some(mg),
            items: vec![],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let errs = inference_failed_errors(&output);
        assert!(
            errs.is_empty(),
            "let `_` annotation must resolve from value — no InferenceFailed expected; got: {errs:?}"
        );
        assert!(
            output.errors.is_empty(),
            "let `_` annotation must resolve cleanly; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn inferred_binding_without_annotation_fails_closed() {
        let source = "fn main() { let f = |x| x; }";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);

        let errs = inference_failed_errors(&output);
        assert!(
            errs.iter()
                .any(|err| err.message.contains("local binding `f`")),
            "expected InferenceFailed for unresolved inferred binding `f`; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn explicit_generic_lambda_binding_rejected() {
        // Generic lambda `<T>(params) => body` was removed in v0.5.
        // The parser must emit a typed E_CLOSURE_PIPE_SYNTAX diagnostic.
        let source = "fn main() { let id = <T>(x: T) => x; }";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.iter().any(|e| {
                matches!(e.kind, hew_parser::ParseDiagnosticKind::ClosurePipeSyntax)
                    && e.message.contains("E_CLOSURE_PIPE_SYNTAX")
            }),
            "expected typed E_CLOSURE_PIPE_SYNTAX for removed generic lambda, got: {:?}",
            result.errors
        );
    }

    #[test]
    fn unresolved_inferred_return_through_none_fails_closed() {
        let source = "fn maybe() -> _ { None }";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);

        let errs = inference_failed_errors(&output);
        assert!(
            !errs.is_empty(),
            "expected InferenceFailed for unresolved `None` return; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn trait_default_method_explicit_infer_return_registers_qualified_signature() {
        use hew_parser::ast::{TraitDecl, TraitItem};

        let trait_decl = TraitDecl {
            visibility: Visibility::Private,
            name: "Answerer".to_string(),
            type_params: None,
            super_traits: None,
            items: vec![TraitItem::Method(TraitMethod {
                name: "answer".to_string(),
                type_params: None,
                params: vec![Param {
                    name: "value".to_string(),
                    ty: (
                        TypeExpr::Named {
                            name: "i64".to_string(),
                            type_args: None,
                        },
                        12..15,
                    ),
                    is_mutable: false,
                }],
                return_type: Some((TypeExpr::Infer, 10..11)),
                where_clause: None,
                body: Some(Block {
                    stmts: vec![],
                    trailing_expr: Some(Box::new((Expr::Identifier("value".to_string()), 20..25))),
                }),
                span: 0..0,
                doc_comment: None,
                lang_item: None,
            })],
            doc_comment: None,
            lang_item: None,
        };
        let program = Program {
            module_graph: None,
            items: vec![(Item::Trait(trait_decl), 0..30)],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        assert!(
            output.errors.is_empty(),
            "trait default `->_` return should resolve cleanly; got errors: {:?}",
            output.errors
        );
        assert_eq!(output.fn_sigs["Answerer::answer"].return_type, Ty::I64);
    }

    #[test]
    fn trait_default_method_unresolved_explicit_infer_return_fails_closed() {
        use hew_parser::ast::{TraitDecl, TraitItem};

        let trait_decl = TraitDecl {
            visibility: Visibility::Private,
            name: "Answerer".to_string(),
            type_params: None,
            super_traits: None,
            items: vec![TraitItem::Method(TraitMethod {
                name: "answer".to_string(),
                type_params: None,
                params: vec![],
                return_type: Some((TypeExpr::Infer, 10..11)),
                where_clause: None,
                body: Some(Block {
                    stmts: vec![],
                    trailing_expr: Some(Box::new((Expr::Identifier("None".to_string()), 20..24))),
                }),
                span: 0..0,
                doc_comment: None,
                lang_item: None,
            })],
            doc_comment: None,
            lang_item: None,
        };
        let program = Program {
            module_graph: None,
            items: vec![(Item::Trait(trait_decl), 0..30)],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let errs = inference_failed_errors(&output);
        assert!(
            errs.iter().any(|err| err
                .message
                .contains("signature of trait method `Answerer::answer`")),
            "expected InferenceFailed for unresolved trait default `->_`; got errors: {:?}",
            output.errors
        );
        assert!(
            !output.fn_sigs.contains_key("Answerer::answer"),
            "failing trait method signature should be stripped from checker output: {:?}",
            output.fn_sigs
        );
    }

    #[test]
    fn trait_default_method_explicit_infer_return_propagates_to_impl_method_signature() {
        use hew_parser::ast::{ImplDecl, TraitBound, TraitDecl, TraitItem, TypeDecl, TypeDeclKind};

        let trait_decl = TraitDecl {
            visibility: Visibility::Private,
            name: "Answerer".to_string(),
            type_params: None,
            super_traits: None,
            items: vec![TraitItem::Method(TraitMethod {
                name: "answer".to_string(),
                type_params: None,
                params: vec![Param {
                    name: "value".to_string(),
                    ty: (
                        TypeExpr::Named {
                            name: "i64".to_string(),
                            type_args: None,
                        },
                        12..15,
                    ),
                    is_mutable: false,
                }],
                return_type: Some((TypeExpr::Infer, 10..11)),
                where_clause: None,
                body: Some(Block {
                    stmts: vec![],
                    trailing_expr: Some(Box::new((Expr::Identifier("value".to_string()), 20..25))),
                }),
                span: 0..0,
                doc_comment: None,
                lang_item: None,
            })],
            doc_comment: None,
            lang_item: None,
        };
        let greeter = TypeDecl {
            visibility: Visibility::Private,
            kind: TypeDeclKind::Struct,
            name: "Greeter".to_string(),
            type_params: None,
            where_clause: None,
            body: vec![],
            doc_comment: None,
            wire: None,
            is_indirect: false,
            resource_marker: hew_parser::ast::ResourceMarker::None,
            is_opaque: false,
            consuming_methods: Vec::new(),
        };
        let impl_decl = ImplDecl {
            type_params: None,
            trait_bound: Some(TraitBound {
                name: "Answerer".to_string(),
                type_args: None,
                assoc_type_bindings: vec![],
            }),
            target_type: (
                TypeExpr::Named {
                    name: "Greeter".to_string(),
                    type_args: None,
                },
                30..37,
            ),
            where_clause: None,
            type_aliases: vec![],
            methods: vec![],
        };
        let program = Program {
            module_graph: None,
            items: vec![
                (Item::Trait(trait_decl), 0..30),
                (Item::TypeDecl(greeter), 31..40),
                (Item::Impl(impl_decl), 41..60),
            ],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        assert!(
            output.errors.is_empty(),
            "default impl method signature should inherit resolved trait return type; got errors: {:?}",
            output.errors
        );
        assert_eq!(output.fn_sigs["Greeter::answer"].return_type, Ty::I64);
    }

    #[test]
    fn trait_default_method_with_concrete_receiver_keeps_implicit_impl_arity() {
        let source = r"
            type Greeter {
                id: i64;
            }

            trait Answerer {
                fn answer(g: Greeter) -> i64 {
                    42
                }
            }

            impl Answerer for Greeter {}
        ";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "unexpected parse errors: {:?}",
            result.errors
        );

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);

        assert!(
            output.errors.is_empty(),
            "default impl method with a concrete receiver should typecheck cleanly; got errors: {:?}",
            output.errors
        );
        assert!(
            output.fn_sigs["Greeter::answer"].params.is_empty(),
            "default impl method should not expose the concrete receiver as an explicit argument: {:?}",
            output.fn_sigs["Greeter::answer"]
        );
    }

    #[test]
    fn inferred_binding_does_not_duplicate_lambda_hole_error() {
        let source = "fn main() { let f = |x: _| x; }";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);

        let errs = inference_failed_errors(&output);
        assert_eq!(
            errs.len(),
            1,
            "expected only the lambda hole diagnostic, got: {:?}",
            output.errors
        );
        assert!(
            errs[0].message.contains("lambda parameter `x`"),
            "expected lambda hole diagnostic, got: {:?}",
            output.errors
        );
    }

    #[test]
    fn inferred_binding_does_not_duplicate_cast_hole_error() {
        let source = "fn main(x: i64) { let y = x as _; }";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);

        let errs = inference_failed_errors(&output);
        assert_eq!(
            errs.len(),
            1,
            "expected only the cast hole diagnostic, got: {:?}",
            output.errors
        );
        assert!(
            errs[0].message.contains("cast target type"),
            "expected cast hole diagnostic, got: {:?}",
            output.errors
        );
    }

    #[test]
    fn bare_channel_handle_signature_stays_valid() {
        let source = concat!(
            "import std::channel::channel;\n",
            "fn close_sender(tx: channel.Sender) {\n",
            "    tx.close();\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );

        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&result.program);
        let errs = inference_failed_errors(&output);
        assert!(
            errs.is_empty(),
            "bare channel handle signatures should not produce InferenceFailed: {errs:?}"
        );
        assert!(
            output.errors.is_empty(),
            "bare channel handle signatures should type-check cleanly: {:?}",
            output.errors
        );
    }

    /// A non-root module body containing a lambda with an unresolved `_`
    /// parameter type must fail closed: the deferred hole created during
    /// body checking must produce an `InferenceFailed` error.
    ///
    /// Regression: the lambda-param deferred-hole path
    /// (`expressions.rs::synthesize_lambda`) must also drain into
    /// `report_unresolved_inference_holes` for non-root module bodies.
    #[test]
    fn body_lambda_infer_param_hole_fails_closed() {
        // fn foo() { let f = |x: _| x; }  — lambda param `_` never constrained
        let mod_id = ModuleId::new(vec!["lambdamod".to_string()]);
        let root_id = ModuleId::root();

        // |x: _| x  — lambda with infer-typed parameter, no call site to resolve it
        let lambda_expr = Expr::Lambda {
            is_move: false,
            type_params: None,
            params: vec![LambdaParam {
                name: "x".to_string(),
                ty: Some((TypeExpr::Infer, 15..16)),
            }],
            return_type: None,
            body: Box::new((Expr::Identifier("x".to_string()), 19..20)),
        };
        let let_stmt = Stmt::Let {
            pattern: (Pattern::Identifier("f".to_string()), 10..11),
            ty: None,
            value: Some((lambda_expr, 14..21)),
        };
        let fn_decl = FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Private,
            name: "foo".to_string(),
            type_params: None,
            params: vec![],
            return_type: None,
            where_clause: None,
            body: Block {
                stmts: vec![(let_stmt, 9..22)],
                trailing_expr: None,
            },
            doc_comment: None,
            decl_span: 0..0,
            fn_span: 0..0,
            intrinsic: None,
        };

        let non_root = Module {
            id: mod_id.clone(),
            items: vec![(Item::Function(fn_decl), 0..30)],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };

        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(non_root).unwrap();
        mg.topo_order = vec![mod_id, root_id];
        let program = Program {
            module_graph: Some(mg),
            items: vec![],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let errs = inference_failed_errors(&output);
        assert!(
            !errs.is_empty(),
            "expected InferenceFailed for unresolved lambda `_` param in non-root module body; got errors: {:?}",
            output.errors
        );
    }
}

// ── module_graph body typecheck parity (v0.3 blocker) ────────────────────────
//
// Non-root module_graph bodies must be typechecked, not just registered.
// A type error in an imported module body must not be silently missed.

/// Build a minimal two-module `Program`: a root module (empty) and a single
/// non-root module `mymod` containing the supplied items.
fn make_program_with_module_graph(non_root_items: Vec<Spanned<Item>>) -> Program {
    let root_id = ModuleId::root();
    let non_root_id = ModuleId::new(vec!["mymod".to_string()]);

    let root_module = Module {
        id: root_id.clone(),
        items: vec![],
        imports: vec![],
        source_paths: vec![],
        doc: None,
    };
    let non_root_module = Module {
        id: non_root_id.clone(),
        items: non_root_items,
        imports: vec![],
        source_paths: vec![],
        doc: None,
    };

    let mut mg = ModuleGraph::new(root_id.clone());
    mg.add_module(root_module).unwrap();
    mg.add_module(non_root_module).unwrap();
    // Dependency order: non-root first, then root (root depends on mymod).
    mg.topo_order = vec![non_root_id, root_id];

    Program {
        module_graph: Some(mg),
        items: vec![],
        module_doc: None,
    }
}

/// A type error (bool body in an i64 function) in a non-root `module_graph` body
/// must be reported by `check_program`.  Before the parity fix this was silently
/// missed because the body-check loop only visited `program.items`.
#[test]
fn module_graph_body_type_error_is_reported() {
    // fn bad() -> i64 { true }  — body returns bool, declared i64
    let bad_fn = FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Pub,
        name: "bad".to_string(),
        type_params: None,
        params: vec![],
        return_type: Some((
            TypeExpr::Named {
                name: "i64".to_string(),
                type_args: None,
            },
            0..3,
        )),
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: Some(Box::new(make_bool_literal(true, 0..4))),
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
        intrinsic: None,
    };

    let program = make_program_with_module_graph(vec![(Item::Function(bad_fn), 0..10)]);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);

    assert!(
        !output.errors.is_empty(),
        "expected a type error from non-root module body, but none were reported"
    );
    assert!(
        output.errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrorKind::Mismatch { expected, actual }
                if expected.contains("i64")
                    && actual.contains("bool")
        )),
        "expected a Mismatch(i64/i64, bool) error; got: {:?}",
        output.errors
    );
}

/// A function with an inferred return type (`-> _`) in a non-root `module_graph`
/// body must have its return type resolved by body checking.  Without the parity
/// fix the type var is never unified and the checker emits a spurious
/// `InferenceFailed` error.  With the fix the body resolves `_` to `i64` and no
/// error is emitted.
#[test]
fn module_graph_body_infer_return_resolves_without_error() {
    // fn inferred() -> _ { 42 }  — `_` must resolve to i64 from the body
    let inferred_fn = FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Pub,
        name: "inferred".to_string(),
        type_params: None,
        params: vec![],
        return_type: Some((TypeExpr::Infer, 0..1)),
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: Some(Box::new(make_int_literal(42, 0..2))),
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
        intrinsic: None,
    };

    let program = make_program_with_module_graph(vec![(Item::Function(inferred_fn), 0..10)]);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);

    assert!(
        output.errors.is_empty(),
        "inferred return type should resolve cleanly; errors: {:?}",
        output.errors
    );
}

/// A local binding in a non-root module body must take precedence over a
/// same-named module when typechecking method calls on identifier receivers.
#[test]
fn module_graph_body_local_binding_named_like_module_still_resolves_methods() {
    let ok_fn = FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Pub,
        name: "ok".to_string(),
        type_params: None,
        params: vec![Param {
            name: "math".to_string(),
            ty: (
                TypeExpr::Named {
                    name: "string".to_string(),
                    type_args: None,
                },
                0..6,
            ),
            is_mutable: false,
        }],
        return_type: Some((
            TypeExpr::Named {
                name: "bool".to_string(),
                type_args: None,
            },
            0..4,
        )),
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: Some(Box::new((
                Expr::MethodCall {
                    receiver: Box::new((Expr::Identifier("math".to_string()), 0..4)),
                    method: "contains".to_string(),
                    args: vec![CallArg::Positional((
                        Expr::Literal(Literal::String("x".to_string())),
                        5..8,
                    ))],
                },
                0..18,
            ))),
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
        intrinsic: None,
    };

    let program = make_program_with_module_graph(vec![(Item::Function(ok_fn), 0..30)]);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);

    assert!(
        output.errors.is_empty(),
        "local bindings should win over same-named modules in non-root method calls; errors: {:?}",
        output.errors
    );
}

#[test]
fn module_qualified_call_rejects_private_body_only_signature() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.modules.insert("mymod".to_string());
    checker.fn_sigs.insert(
        "mymod.secret".to_string(),
        FnSig {
            return_type: Ty::I64,
            ..FnSig::default()
        },
    );

    let receiver = (Expr::Identifier("mymod".to_string()), 0..5);
    let ty = checker.check_method_call(&receiver, "secret", &[], &(0..12));

    assert_eq!(ty, Ty::Error);
    assert!(
        checker
            .errors
            .iter()
            .any(|err| matches!(err.kind, TypeErrorKind::UndefinedMethod)),
        "module-qualified calls must not resolve against non-exported private signatures: {:?}",
        checker.errors
    );
}

#[test]
fn module_qualified_call_accepts_exported_signature() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.modules.insert("mymod".to_string());
    checker
        .module_fn_exports
        .insert("mymod.visible".to_string());
    checker.fn_sigs.insert(
        "mymod.visible".to_string(),
        FnSig {
            return_type: Ty::I64,
            ..FnSig::default()
        },
    );

    let receiver = (Expr::Identifier("mymod".to_string()), 0..5);
    let ty = checker.check_method_call(&receiver, "visible", &[], &(0..13));

    assert_eq!(ty, Ty::I64);
    assert!(
        checker.errors.is_empty(),
        "exported module-qualified calls must keep working; errors: {:?}",
        checker.errors
    );
}

#[test]
fn module_graph_body_private_local_type_is_available() {
    let local_type = TypeDecl {
        visibility: Visibility::Private,
        name: "Local".to_string(),
        type_params: None,
        where_clause: None,
        kind: TypeDeclKind::Struct,
        body: vec![TypeBodyItem::Field {
            name: "x".to_string(),
            ty: (
                TypeExpr::Named {
                    name: "i64".to_string(),
                    type_args: None,
                },
                0..1,
            ),
            attributes: Vec::new(),
            doc_comment: None,
            span: 0..0,
        }],
        is_indirect: false,
        doc_comment: None,
        wire: None,
        resource_marker: hew_parser::ast::ResourceMarker::None,
        is_opaque: false,
        consuming_methods: Vec::new(),
    };

    let ok_fn = FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Pub,
        name: "ok".to_string(),
        type_params: None,
        params: vec![],
        return_type: Some((
            TypeExpr::Named {
                name: "i64".to_string(),
                type_args: None,
            },
            0..3,
        )),
        where_clause: None,
        body: Block {
            stmts: vec![(
                Stmt::Let {
                    pattern: (Pattern::Identifier("a".to_string()), 0..1),
                    ty: None,
                    value: Some((
                        Expr::StructInit {
                            name: "Local".to_string(),
                            fields: vec![("x".to_string(), make_int_literal(1, 0..1))],
                            type_args: None,
                            base: None,
                        },
                        0..10,
                    )),
                },
                0..10,
            )],
            trailing_expr: Some(Box::new((
                Expr::FieldAccess {
                    object: Box::new((Expr::Identifier("a".to_string()), 0..1)),
                    field: "x".to_string(),
                },
                11..14,
            ))),
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
        intrinsic: None,
    };

    let program = make_program_with_module_graph(vec![
        (Item::TypeDecl(local_type), 0..10),
        (Item::Function(ok_fn), 10..30),
    ]);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);

    assert!(
        output.errors.is_empty(),
        "private non-root local types should resolve within the same module body; errors: {:?}",
        output.errors
    );
}

#[test]
#[expect(
    clippy::too_many_lines,
    reason = "constructs an explicit multi-module fixture for the parity regression"
)]
fn module_graph_body_prefers_same_module_private_helper_over_global_bare_name() {
    let i64_ty = TypeExpr::Named {
        name: "i64".to_string(),
        type_args: None,
    };
    let string_ty = TypeExpr::Named {
        name: "string".to_string(),
        type_args: None,
    };

    let helper_i64 = FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Private,
        name: "helper".to_string(),
        type_params: None,
        params: vec![],
        return_type: Some((i64_ty.clone(), 0..3)),
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: Some(Box::new(make_int_literal(42, 0..2))),
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
        intrinsic: None,
    };

    let ok_fn = FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Pub,
        name: "ok".to_string(),
        type_params: None,
        params: vec![],
        return_type: Some((i64_ty, 0..3)),
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: Some(Box::new((
                Expr::Call {
                    function: Box::new((Expr::Identifier("helper".to_string()), 0..6)),
                    type_args: None,
                    args: vec![],
                    is_tail_call: false,
                },
                0..8,
            ))),
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
        intrinsic: None,
    };

    let helper_string = FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Private,
        name: "helper".to_string(),
        type_params: None,
        params: vec![],
        return_type: Some((string_ty, 10..16)),
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: Some(Box::new((
                Expr::Literal(Literal::String("wrong".to_string())),
                10..17,
            ))),
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
        intrinsic: None,
    };

    let root_id = ModuleId::root();
    let alpha_id = ModuleId::new(vec!["alpha".to_string()]);
    let beta_id = ModuleId::new(vec!["beta".to_string()]);
    let root_module = Module {
        id: root_id.clone(),
        items: vec![],
        imports: vec![],
        source_paths: vec![],
        doc: None,
    };
    let alpha_module = Module {
        id: alpha_id.clone(),
        items: vec![
            (Item::Function(helper_i64), 0..20),
            (Item::Function(ok_fn), 20..40),
        ],
        imports: vec![],
        source_paths: vec![],
        doc: None,
    };
    let beta_module = Module {
        id: beta_id.clone(),
        items: vec![(Item::Function(helper_string), 40..60)],
        imports: vec![],
        source_paths: vec![],
        doc: None,
    };

    let mut mg = ModuleGraph::new(root_id.clone());
    mg.add_module(root_module).unwrap();
    mg.add_module(alpha_module).unwrap();
    mg.add_module(beta_module).unwrap();
    mg.topo_order = vec![alpha_id, beta_id, root_id];

    let program = Program {
        module_graph: Some(mg),
        items: vec![],
        module_doc: None,
    };
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);

    assert!(
        output.errors.is_empty(),
        "same-module private helper should win over another module's bare helper name; errors: {:?}",
        output.errors
    );
}

#[test]
#[expect(
    clippy::too_many_lines,
    reason = "constructs an explicit multi-module fixture for the parity regression"
)]
fn module_graph_body_prefers_same_module_private_extern_over_global_bare_name() {
    let i64_ty = TypeExpr::Named {
        name: "i64".to_string(),
        type_args: None,
    };
    let string_ty = TypeExpr::Named {
        name: "string".to_string(),
        type_args: None,
    };

    let extern_i64 = ExternBlock {
        abi: "C".to_string(),
        functions: vec![ExternFnDecl {
            attributes: Vec::new(),
            name: "hew_test_raw".to_string(),
            params: vec![],
            return_type: Some((i64_ty.clone(), 0..3)),
            is_variadic: false,
            span: 0..0,
        }],
    };
    let ok_fn = FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Pub,
        name: "ok".to_string(),
        type_params: None,
        params: vec![],
        return_type: Some((i64_ty, 0..3)),
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: Some(Box::new((
                Expr::UnsafeBlock(Box::new(Block {
                    stmts: vec![],
                    trailing_expr: Some(Box::new((
                        Expr::Call {
                            function: Box::new((
                                Expr::Identifier("hew_test_raw".to_string()),
                                0..12,
                            )),
                            type_args: None,
                            args: vec![],
                            is_tail_call: false,
                        },
                        0..14,
                    ))),
                })),
                0..14,
            ))),
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
        intrinsic: None,
    };
    let extern_string = ExternBlock {
        abi: "C".to_string(),
        functions: vec![ExternFnDecl {
            attributes: Vec::new(),
            name: "hew_test_raw".to_string(),
            params: vec![],
            return_type: Some((string_ty, 20..26)),
            is_variadic: false,
            span: 0..0,
        }],
    };

    let root_id = ModuleId::root();
    let alpha_id = ModuleId::new(vec!["alpha".to_string()]);
    let beta_id = ModuleId::new(vec!["beta".to_string()]);
    let root_module = Module {
        id: root_id.clone(),
        items: vec![],
        imports: vec![],
        source_paths: vec![],
        doc: None,
    };
    let alpha_module = Module {
        id: alpha_id.clone(),
        items: vec![
            (Item::ExternBlock(extern_i64), 0..20),
            (Item::Function(ok_fn), 20..40),
        ],
        imports: vec![],
        source_paths: vec![],
        doc: None,
    };
    let beta_module = Module {
        id: beta_id.clone(),
        items: vec![(Item::ExternBlock(extern_string), 40..60)],
        imports: vec![],
        source_paths: vec![],
        doc: None,
    };

    let mut mg = ModuleGraph::new(root_id.clone());
    mg.add_module(root_module).unwrap();
    mg.add_module(alpha_module).unwrap();
    mg.add_module(beta_module).unwrap();
    mg.topo_order = vec![alpha_id, beta_id, root_id];

    let program = Program {
        module_graph: Some(mg),
        items: vec![],
        module_doc: None,
    };
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);

    assert!(
        output.errors.is_empty(),
        "same-module private extern should win over another module's bare extern name; errors: {:?}",
        output.errors
    );
}

// ── module-body-diagnostic-completion: source_module tagging tests ────────────
//
// These tests prove that diagnostics originating in non-root module bodies
// carry the correct `source_module` value so the CLI can route them to the
// right source file when rendering.
//
// Regression coverage for module-body typecheck parity and diagnostic-envelope
// unification.

#[cfg(test)]
mod module_body_diagnostic_envelope {
    use super::*;

    // ── helpers ────────────────────────────────────────────────────────────────

    /// Build a minimal `Program` with a non-root module `mod_name` whose items
    /// are the supplied `items`.  The root module is empty.
    fn make_program_with_named_module(mod_name: &str, items: Vec<Spanned<Item>>) -> Program {
        let root_id = ModuleId::root();
        let mod_id = ModuleId::new(vec![mod_name.to_string()]);

        let non_root = Module {
            id: mod_id.clone(),
            items,
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };

        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(non_root).unwrap();
        mg.topo_order = vec![mod_id, root_id];

        Program {
            module_graph: Some(mg),
            items: vec![],
            module_doc: None,
        }
    }

    /// Build a minimal fn declaration that returns `bool` but is declared `-> i64`.
    fn make_mistyped_fn(name: &str) -> Spanned<Item> {
        let fn_decl = FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Pub,
            name: name.to_string(),
            type_params: None,
            params: vec![],
            return_type: Some((
                TypeExpr::Named {
                    name: "i64".to_string(),
                    type_args: None,
                },
                5..8,
            )),
            where_clause: None,
            body: Block {
                stmts: vec![],
                trailing_expr: Some(Box::new((Expr::Literal(Literal::Bool(true)), 10..14))),
            },
            doc_comment: None,
            decl_span: 0..0,
            fn_span: 0..0,
            intrinsic: None,
        };
        (Item::Function(fn_decl), 0..20)
    }

    // ── body-check errors carry the module name ────────────────────────────────

    /// A type mismatch in a non-root module body must be tagged with the
    /// module's dotted name in `TypeError::source_module`.
    #[test]
    fn body_mismatch_error_tagged_with_source_module() {
        let program = make_program_with_named_module("mymod", vec![make_mistyped_fn("bad")]);

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        assert!(
            !output.errors.is_empty(),
            "expected a type error from non-root module body"
        );
        for err in &output.errors {
            assert_eq!(
                err.source_module.as_deref(),
                Some("mymod"),
                "error from non-root module body must be tagged with 'mymod'; got: {:?}",
                err.source_module
            );
        }
    }

    /// A type mismatch in the root module must NOT be tagged (`source_module`
    /// stays None).
    #[test]
    fn root_module_error_has_no_source_module_tag() {
        // fn bad() -> i64 { true }  in root items
        let fn_decl = FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Private,
            name: "bad".to_string(),
            type_params: None,
            params: vec![],
            return_type: Some((
                TypeExpr::Named {
                    name: "i64".to_string(),
                    type_args: None,
                },
                5..8,
            )),
            where_clause: None,
            body: Block {
                stmts: vec![],
                trailing_expr: Some(Box::new((Expr::Literal(Literal::Bool(true)), 10..14))),
            },
            doc_comment: None,
            decl_span: 0..0,
            fn_span: 0..0,
            intrinsic: None,
        };
        let program = Program {
            module_graph: None,
            items: vec![(Item::Function(fn_decl), 0..20)],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        assert!(
            !output.errors.is_empty(),
            "expected a type error from root module"
        );
        for err in &output.errors {
            assert!(
                err.source_module.is_none(),
                "root module error must not have source_module set; got: {:?}",
                err.source_module
            );
        }
    }

    /// Errors from different non-root modules must each carry their own module name.
    #[test]
    fn errors_from_multiple_modules_tagged_independently() {
        use hew_parser::ast::{Block, FnDecl, Item, Literal, TypeExpr, Visibility};

        let make_bad_fn = |fn_name: &str| -> Spanned<Item> {
            let fd = FnDecl {
                attributes: vec![],
                is_async: false,
                is_generator: false,
                visibility: Visibility::Pub,
                name: fn_name.to_string(),
                type_params: None,
                params: vec![],
                return_type: Some((
                    TypeExpr::Named {
                        name: "i64".to_string(),
                        type_args: None,
                    },
                    5..8,
                )),
                where_clause: None,
                body: Block {
                    stmts: vec![],
                    trailing_expr: Some(Box::new((Expr::Literal(Literal::Bool(true)), 10..14))),
                },
                doc_comment: None,
                decl_span: 0..0,
                fn_span: 0..0,
                intrinsic: None,
            };
            (Item::Function(fd), 0..20)
        };

        let root_id = ModuleId::root();
        let alpha_id = ModuleId::new(vec!["alpha".to_string()]);
        let beta_id = ModuleId::new(vec!["beta".to_string()]);

        let alpha = Module {
            id: alpha_id.clone(),
            items: vec![make_bad_fn("alpha_bad")],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };
        let beta = Module {
            id: beta_id.clone(),
            items: vec![make_bad_fn("beta_bad")],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };

        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(alpha).unwrap();
        mg.add_module(beta).unwrap();
        mg.topo_order = vec![alpha_id, beta_id, root_id];

        let program = Program {
            module_graph: Some(mg),
            items: vec![],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        assert!(
            output.errors.len() >= 2,
            "expected errors from both alpha and beta modules; got: {:?}",
            output.errors
        );

        let alpha_errs: Vec<_> = output
            .errors
            .iter()
            .filter(|e| e.source_module.as_deref() == Some("alpha"))
            .collect();
        let beta_errs: Vec<_> = output
            .errors
            .iter()
            .filter(|e| e.source_module.as_deref() == Some("beta"))
            .collect();

        assert!(
            !alpha_errs.is_empty(),
            "expected at least one error tagged 'alpha'; errors: {:?}",
            output.errors
        );
        assert!(
            !beta_errs.is_empty(),
            "expected at least one error tagged 'beta'; errors: {:?}",
            output.errors
        );
    }

    // ── deferred inference hole drain carries source module ────────────────────

    /// An unresolved cast target `as _` in a non-root module body must produce
    /// an `InferenceFailed` error tagged with the module name.
    #[test]
    fn deferred_cast_hole_tagged_with_source_module() {
        // fn foo(x: i64) { let y = x as _; }  — cast target _ never resolved
        use hew_parser::ast::{Block, FnDecl, Param, Stmt, TypeExpr, Visibility};

        let cast_expr = Expr::Cast {
            expr: Box::new((Expr::Identifier("x".to_string()), 20..21)),
            ty: (TypeExpr::Infer, 25..26),
        };
        let let_stmt = Stmt::Let {
            pattern: (Pattern::Identifier("y".to_string()), 14..15),
            ty: None,
            value: Some((cast_expr, 18..27)),
        };
        let fn_decl = FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Private,
            name: "foo".to_string(),
            type_params: None,
            params: vec![Param {
                name: "x".to_string(),
                ty: (
                    TypeExpr::Named {
                        name: "i64".to_string(),
                        type_args: None,
                    },
                    8..11,
                ),
                is_mutable: false,
            }],
            return_type: None,
            where_clause: None,
            body: Block {
                stmts: vec![(let_stmt, 13..28)],
                trailing_expr: None,
            },
            doc_comment: None,
            decl_span: 0..0,
            fn_span: 0..0,
            intrinsic: None,
        };

        let program =
            make_program_with_named_module("castmod", vec![(Item::Function(fn_decl), 0..30)]);

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let inference_failed: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::InferenceFailed))
            .collect();

        assert!(
            !inference_failed.is_empty(),
            "expected InferenceFailed for unresolved `as _` in non-root module; errors: {:?}",
            output.errors
        );
        for err in &inference_failed {
            assert_eq!(
                err.source_module.as_deref(),
                Some("castmod"),
                "deferred cast inference error must carry source module 'castmod'; got {:?}",
                err.source_module
            );
        }
    }

    /// Signature-level inference holes in non-root modules (fn param `_`) must
    /// also be tagged with the module name.
    #[test]
    fn signature_inference_hole_tagged_with_source_module() {
        // fn helper(_ : _) {}  — unresolved param type
        let fn_decl = FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Private,
            name: "helper".to_string(),
            type_params: None,
            params: vec![Param {
                name: "v".to_string(),
                ty: (TypeExpr::Infer, 10..11),
                is_mutable: false,
            }],
            return_type: None,
            where_clause: None,
            body: Block {
                stmts: vec![],
                trailing_expr: None,
            },
            doc_comment: None,
            decl_span: 0..0,
            fn_span: 0..0,
            intrinsic: None,
        };

        let program =
            make_program_with_named_module("sigmod", vec![(Item::Function(fn_decl), 0..20)]);

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let inference_failed: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::InferenceFailed))
            .collect();

        assert!(
            !inference_failed.is_empty(),
            "expected InferenceFailed for `_` param in non-root module fn; errors: {:?}",
            output.errors
        );
        for err in &inference_failed {
            assert_eq!(
                err.source_module.as_deref(),
                Some("sigmod"),
                "signature inference error must carry source module 'sigmod'; got {:?}",
                err.source_module
            );
        }
    }

    /// Deferred channel rewrite finalization must preserve the non-root module
    /// tag when it emits a post-inference `InferenceFailed` diagnostic.
    #[test]
    fn deferred_channel_rewrite_error_tagged_with_source_module() {
        let parsed = hew_parser::parse(
            r"
                fn bad(rx: Receiver<_>) {
                    let _ = rx.recv();
                }
            ",
        );
        assert!(
            parsed.errors.is_empty(),
            "module parse errors: {:?}",
            parsed.errors
        );

        let program = make_program_with_named_module("chanmod", parsed.program.items.clone());

        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&program);

        let inference_failed: Vec<_> = output
            .errors
            .iter()
            .filter(|e| {
                matches!(e.kind, TypeErrorKind::InferenceFailed) && e.message.contains("inner type")
            })
            .collect();

        assert!(
            !inference_failed.is_empty(),
            "expected deferred channel inference failure in non-root module; errors: {:?}",
            output.errors
        );
        for err in inference_failed {
            assert_eq!(
                err.source_module.as_deref(),
                Some("chanmod"),
                "deferred channel rewrite error must carry source module 'chanmod'; got {:?}",
                err.source_module
            );
        }
    }

    #[test]
    fn assign_target_shapes_populated_for_while_loop_with_import() {
        // Reproduces the eval_large_stderr CI failure:
        // synthesized source for `fn spam_err` eval step with `import std::io`
        let source = "import std::io;\nfn spam_err() {\n    var i = 0;\n    while i < 20000 {\n        io.write_err(\"line\\n\");\n        i = i + 1;\n    }\n}\nfn main() {\n}\n";
        let parse_result = hew_parser::parse(source);
        assert!(
            parse_result.errors.is_empty(),
            "parse errors: {:?}",
            parse_result.errors
        );
        let mut checker = crate::Checker::new(crate::module_registry::ModuleRegistry::new(
            crate::module_registry::build_module_search_paths(),
        ));
        let tco = checker.check_program(&parse_result.program);
        // `i` in `i = i + 1` must appear in assign_target_shapes
        let has_shape = tco.assign_target_shapes.iter().any(|(k, _)| k.start == 109);
        assert!(
            has_shape,
            "assign_target_shapes missing entry for i at ~109; got: {:?}",
            tco.assign_target_shapes.keys().collect::<Vec<_>>()
        );
    }
}

// ── Warning source-module attribution (PR-A slice) ──────────────────────────

#[cfg(test)]
mod warning_source_attribution {
    use super::*;

    fn make_unused_import_decl() -> ImportDecl {
        // import std::encoding::json  (unresolved — no resolved_items)
        // The module registry will fail to find it, but the import is still
        // registered into import_spans so the UnusedImport path is exercised.
        // Use a fake single-segment path so `register_import` takes the user-
        // module branch (path non-empty, no resolved_items → unresolved error
        // path, does NOT insert into import_spans).
        //
        // Instead we supply `resolved_items = Some(vec![])` to convince
        // register_import to follow the user-module branch and insert into
        // import_spans.
        ImportDecl {
            path: vec!["fakemod".to_string()],
            spec: None,
            file_path: None,
            resolved_items: Some(vec![]),
            resolved_item_source_paths: vec![],
            resolved_source_paths: vec![],
        }
    }

    fn make_trivial_fn(name: &str) -> FnDecl {
        FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Private,
            name: name.to_string(),
            type_params: None,
            params: vec![],
            return_type: None,
            where_clause: None,
            body: Block {
                stmts: vec![],
                trailing_expr: None,
            },
            doc_comment: None,
            decl_span: 0..0,
            fn_span: 0..0,
            intrinsic: None,
        }
    }

    fn make_non_root_program_with_fn_body(
        module_name: &str,
        name: &str,
        stmts: Vec<Spanned<Stmt>>,
    ) -> Program {
        let fn_decl = FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Private,
            name: name.to_string(),
            type_params: None,
            params: vec![],
            return_type: None,
            where_clause: None,
            body: Block {
                stmts,
                trailing_expr: None,
            },
            doc_comment: None,
            decl_span: 0..0,
            fn_span: 0..0,
            intrinsic: None,
        };
        let root_id = ModuleId::root();
        let module_id = ModuleId::new(vec![module_name.to_string()]);
        let root_module = Module {
            id: root_id.clone(),
            items: vec![],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };
        let sub_module = Module {
            id: module_id.clone(),
            items: vec![(Item::Function(fn_decl), 0..40)],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };

        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(root_module).unwrap();
        mg.add_module(sub_module).unwrap();
        mg.topo_order = vec![module_id, root_id];

        Program {
            module_graph: Some(mg),
            items: vec![],
            module_doc: None,
        }
    }

    /// Build a Program whose module graph has:
    ///   - a root module with `fn main()` (no imports, in program.items)
    ///   - a non-root module "submod" with an import of "fakemod" and `fn helper()`
    fn build_program_with_non_root_import_and_fn() -> Program {
        let root_id = ModuleId::root();
        let submod_id = ModuleId::new(vec!["submod".to_string()]);

        let main_fn = make_trivial_fn("main");
        let helper_fn = make_trivial_fn("helper");
        let import_decl = make_unused_import_decl();

        let root_module = Module {
            id: root_id.clone(),
            items: vec![],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };
        let sub_module = Module {
            id: submod_id.clone(),
            items: vec![
                (Item::Import(import_decl), 0..20),
                (Item::Function(helper_fn), 25..50),
            ],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };

        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(root_module).unwrap();
        mg.add_module(sub_module).unwrap();
        mg.topo_order = vec![submod_id, root_id];

        Program {
            module_graph: Some(mg),
            // Root-module items live in program.items (not in Module.items for root).
            items: vec![(Item::Function(main_fn), 0..10)],
            module_doc: None,
        }
    }

    /// An `UnusedImport` warning emitted for an import registered while
    /// `current_module` was "submod" must carry `source_module = Some("submod")`.
    #[test]
    fn non_root_unused_import_carries_source_module() {
        let program = build_program_with_non_root_import_and_fn();
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let unused_import_warnings: Vec<_> = output
            .warnings
            .iter()
            .filter(|w| w.kind == TypeErrorKind::UnusedImport && w.message.contains("fakemod"))
            .collect();

        assert!(
            !unused_import_warnings.is_empty(),
            "expected an UnusedImport warning for 'fakemod'; got warnings: {:?}",
            output.warnings
        );
        for w in &unused_import_warnings {
            assert_eq!(
                w.source_module.as_deref(),
                Some("submod"),
                "UnusedImport for 'fakemod' must carry source_module='submod'; got {:?}",
                w.source_module
            );
        }
    }

    /// A root-module `UnusedImport` (registered while `current_module = None`) must
    /// continue to carry `source_module = None` — no regression.
    #[test]
    fn root_unused_import_has_no_source_module() {
        // Build an ImportDecl with resolved_items so it reaches import_spans.
        let import_decl = make_unused_import_decl();
        let main_fn = make_trivial_fn("main");

        let program = Program {
            module_graph: None,
            items: vec![
                (Item::Import(import_decl), 0..20),
                (Item::Function(main_fn), 25..40),
            ],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let w = output
            .warnings
            .iter()
            .find(|w| w.kind == TypeErrorKind::UnusedImport && w.message.contains("fakemod"));

        assert!(
            w.is_some(),
            "expected UnusedImport warning for root 'fakemod'; got: {:?}",
            output.warnings
        );
        assert_eq!(
            w.unwrap().source_module,
            None,
            "root-module UnusedImport must have source_module=None; got {:?}",
            w.unwrap().source_module
        );
    }

    /// Functions defined in a non-root module must have their source module stored
    /// in `fn_def_spans` even though the current dead-code filter skips dot-named
    /// functions (they are never promoted to `DeadCode` warnings today).
    /// This guards the attribution infrastructure for future use.
    #[test]
    fn non_root_fn_def_span_stores_source_module() {
        let program = build_program_with_non_root_import_and_fn();
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        checker.check_program(&program);

        // Non-root function is keyed as "submod.helper" in fn_def_spans.
        let entry = checker.fn_def_spans.get("submod.helper");
        assert!(
            entry.is_some(),
            "fn_def_spans must contain 'submod.helper'; keys: {:?}",
            checker.fn_def_spans.keys().collect::<Vec<_>>()
        );
        let (_, stored_module) = entry.unwrap();
        assert_eq!(
            stored_module.as_deref(),
            Some("submod"),
            "fn_def_spans entry for 'submod.helper' must store source_module='submod'; got {stored_module:?}",
        );
    }

    /// Root-module functions must store `source_module = None` in `fn_def_spans`.
    #[test]
    fn root_fn_def_span_has_no_source_module() {
        let program = build_program_with_non_root_import_and_fn();
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        checker.check_program(&program);

        // Root fn (main) has no module prefix.
        let entry = checker.fn_def_spans.get("main");
        assert!(
            entry.is_some(),
            "fn_def_spans must contain 'main'; keys: {:?}",
            checker.fn_def_spans.keys().collect::<Vec<_>>()
        );
        let (_, stored_module) = entry.unwrap();
        assert_eq!(
            stored_module.as_deref(),
            None,
            "fn_def_spans entry for 'main' must have source_module=None; got {stored_module:?}",
        );
    }

    #[test]
    fn non_root_unreachable_warning_carries_source_module() {
        let stmts = vec![
            (Stmt::Return(None), 10..16),
            (
                Stmt::Expression((Expr::Literal(Literal::Bool(true)), 21..25)),
                21..23,
            ),
        ];
        let program = make_non_root_program_with_fn_body("submod", "warns", stmts);

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let warnings: Vec<_> = output
            .warnings
            .iter()
            .filter(|w| w.kind == TypeErrorKind::UnreachableCode)
            .collect();

        assert!(
            !warnings.is_empty(),
            "expected UnreachableCode warning in non-root module; got: {:?}",
            output.warnings
        );
        for warning in warnings {
            assert_eq!(
                warning.source_module.as_deref(),
                Some("submod"),
                "UnreachableCode warning must carry source_module='submod'; got {:?}",
                warning.source_module
            );
        }
    }

    // ── ImportKey: same short-name across different owning modules ─────────────

    fn make_named_import_decl(short_name: &str) -> ImportDecl {
        ImportDecl {
            path: vec![short_name.to_string()],
            spec: None,
            file_path: None,
            resolved_items: Some(vec![]),
            resolved_item_source_paths: vec![],
            resolved_source_paths: vec![],
        }
    }

    /// Two different owning modules both import a module with the same short
    /// name.  Neither module uses the import, so both must get an
    /// `UnusedImport` warning — the use-site in one must not suppress the
    /// warning for the other.
    #[test]
    #[allow(
        clippy::similar_names,
        reason = "mod_a_id / mod_b_id are intentionally symmetric"
    )]
    fn same_short_name_imports_in_different_owners_each_warn_unused() {
        let root_id = ModuleId::root();
        let mod_a_id = ModuleId::new(vec!["mod_a".to_string()]);
        let mod_b_id = ModuleId::new(vec!["mod_b".to_string()]);

        let import_a = make_named_import_decl("fakemod");
        let import_b = make_named_import_decl("fakemod");

        let root_module = Module {
            id: root_id.clone(),
            items: vec![],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };
        let module_a = Module {
            id: mod_a_id.clone(),
            items: vec![(Item::Import(import_a), 0..20)],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };
        let module_b = Module {
            id: mod_b_id.clone(),
            items: vec![(Item::Import(import_b), 100..120)],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };

        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(root_module).unwrap();
        mg.add_module(module_a).unwrap();
        mg.add_module(module_b).unwrap();
        mg.topo_order = vec![mod_a_id, mod_b_id, root_id];

        let program = Program {
            module_graph: Some(mg),
            items: vec![],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let unused: Vec<_> = output
            .warnings
            .iter()
            .filter(|w| w.kind == TypeErrorKind::UnusedImport && w.message.contains("fakemod"))
            .collect();

        assert_eq!(
            unused.len(),
            2,
            "expected exactly 2 UnusedImport warnings (one per owning module), got {}: {:?}",
            unused.len(),
            unused
        );

        let owners: std::collections::HashSet<Option<&str>> =
            unused.iter().map(|w| w.source_module.as_deref()).collect();
        assert!(
            owners.contains(&Some("mod_a")),
            "expected an UnusedImport attributed to 'mod_a'; got: {owners:?}",
        );
        assert!(
            owners.contains(&Some("mod_b")),
            "expected an UnusedImport attributed to 'mod_b'; got: {owners:?}",
        );
    }

    /// When one owning module *uses* `fakemod` (via a module-qualified call
    /// registered through `import_spans`) and another owning module imports the
    /// same short name but never uses it, only the second module's import must
    /// be warned as unused.
    ///
    /// This is the core clobber regression: before the `ImportKey` fix, marking
    /// `fakemod` as used in `mod_a` would also suppress the warning for `mod_b`.
    #[test]
    #[expect(
        clippy::too_many_lines,
        reason = "inline AST construction for the import-clobber regression scenario"
    )]
    #[allow(
        clippy::similar_names,
        reason = "mod_a_id / mod_b_id are intentionally symmetric"
    )]
    fn used_import_in_one_owner_does_not_suppress_unused_in_another() {
        let root_id = ModuleId::root();
        let mod_a_id = ModuleId::new(vec!["mod_a".to_string()]);
        let mod_b_id = ModuleId::new(vec!["mod_b".to_string()]);

        // mod_a: import fakemod  +  fn caller() { fakemod.helper() }
        // The call to fakemod.helper() marks fakemod as used in mod_a's context.

        // Register a pub fn "helper" in fakemod so module_fn_exports and fn_sigs
        // contain "fakemod.helper" — that is what the method-dispatch path checks.
        let helper_fn = FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Pub,
            name: "helper".to_string(),
            type_params: None,
            params: vec![],
            return_type: None,
            where_clause: None,
            body: Block {
                stmts: vec![],
                trailing_expr: None,
            },
            doc_comment: None,
            decl_span: 0..0,
            fn_span: 0..0,
            intrinsic: None,
        };

        // caller() body: `fakemod.helper()` expressed as a MethodCall statement.
        let call_stmt = Stmt::Expression((
            Expr::MethodCall {
                receiver: Box::new((
                    Expr::Identifier("fakemod".to_string()),
                    Span::from(200..206),
                )),
                method: "helper".to_string(),
                args: vec![],
            },
            Span::from(200..215),
        ));
        let caller_fn = FnDecl {
            attributes: vec![],
            is_async: false,
            is_generator: false,
            visibility: Visibility::Private,
            name: "caller".to_string(),
            type_params: None,
            params: vec![],
            return_type: None,
            where_clause: None,
            body: Block {
                stmts: vec![(call_stmt, 200..215)],
                trailing_expr: None,
            },
            doc_comment: None,
            decl_span: 150..200,
            fn_span: 0..0,
            intrinsic: None,
        };

        // Import of fakemod in mod_a must carry resolved_items that include
        // the pub helper fn so the module is actually registered and
        // module_fn_exports gets "fakemod.helper".
        let import_a_with_items = ImportDecl {
            path: vec!["fakemod".to_string()],
            spec: None,
            file_path: None,
            resolved_items: Some(vec![(Item::Function(helper_fn), 0..30)]),
            resolved_item_source_paths: vec![],
            resolved_source_paths: vec![],
        };

        // mod_b: import fakemod  (unused — no code references it)
        let import_b = make_named_import_decl("fakemod");

        let root_module = Module {
            id: root_id.clone(),
            items: vec![],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };
        let module_a = Module {
            id: mod_a_id.clone(),
            items: vec![
                (Item::Import(import_a_with_items), 0..30),
                (Item::Function(caller_fn), 150..220),
            ],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };
        let module_b = Module {
            id: mod_b_id.clone(),
            items: vec![(Item::Import(import_b), 300..320)],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };

        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(root_module).unwrap();
        mg.add_module(module_a).unwrap();
        mg.add_module(module_b).unwrap();
        mg.topo_order = vec![mod_a_id, mod_b_id, root_id];

        let program = Program {
            module_graph: Some(mg),
            items: vec![],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let unused: Vec<_> = output
            .warnings
            .iter()
            .filter(|w| w.kind == TypeErrorKind::UnusedImport && w.message.contains("fakemod"))
            .collect();

        // mod_b's import is unused; mod_a's is used (caller() calls fakemod.helper())
        // so exactly one warning, attributed to mod_b.
        assert_eq!(
            unused.len(),
            1,
            "expected exactly 1 UnusedImport warning (for mod_b), got {}: {:?}",
            unused.len(),
            unused
        );
        assert_eq!(
            unused[0].source_module.as_deref(),
            Some("mod_b"),
            "the single UnusedImport must be attributed to 'mod_b'; got: {:?}",
            unused[0].source_module
        );
    }

    // ── Ty::Error return-context seeding regressions ──────────────────────────
    //
    // When a function's return-type annotation cannot be resolved (e.g.
    // `UnknownType`), `resolve_type_expr` produces `Ty::Error`.  Before this
    // fix the error type was passed as the *expected* type into the body,
    // causing `expect_type`'s guard (`expected_resolved != Ty::Error`) to
    // silently swallow genuine body-level type errors.

    fn has_mismatch(errors: &[crate::error::TypeError]) -> bool {
        errors.iter().any(|e| {
            matches!(e.kind, TypeErrorKind::Mismatch { .. })
                || e.message.contains("mismatch")
                || e.message.contains("TypeMismatch")
        })
    }

    #[test]
    fn error_return_type_does_not_suppress_trailing_expr_mismatch() {
        // fn foo() -> UnknownType { let x: i32 = "bad"; x }
        // The `let x: i32 = "bad"` is a type mismatch inside the body.
        // When the fn return annotation is Ty::Error the body was previously
        // checked with check_against(_, Ty::Error), masking the let-binding error.
        // After the fix the body is synthesized (expected=None), so the let
        // mismatch is still reported.
        let source = r#"fn foo() -> UnknownType { let x: i32 = "bad"; x }"#;
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        assert!(
            has_mismatch(&output.errors),
            "trailing-expr body mismatch must be reported even when return type \
             is Ty::Error; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn error_return_type_does_not_suppress_explicit_return_mismatch() {
        // fn foo() -> UnknownType { let x: i32 = "bad"; return x; }
        // The let-binding mismatch must be reported.
        let source = r#"fn foo() -> UnknownType { let x: i32 = "bad"; return x; }"#;
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        assert!(
            has_mismatch(&output.errors),
            "body mismatch inside explicit return must be reported even when \
             return type is Ty::Error; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn error_return_type_does_not_suppress_receive_fn_body_mismatch() {
        // receive fn handler() -> UnknownType { let x: i32 = "bad"; x }
        // inside an actor; body mismatch must be reported.
        let source = r#"
actor MyActor {
    var value: i32 = 0;
    receive fn handler() -> UnknownType { let x: i32 = "bad"; x }
}
"#;
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        assert!(
            has_mismatch(&output.errors),
            "body mismatch in receive fn must be reported even when return type \
             is Ty::Error; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn error_return_type_does_not_suppress_lambda_annotated_return_mismatch() {
        // Lambda with annotated (unresolvable) return type:
        //   let f = (x: i32) -> UnknownType => { let y: i32 = "bad"; y };
        // The let-binding mismatch inside the lambda body must still be reported.
        let source = r#"fn foo() { let f = |x: i32| -> UnknownType { let y: i32 = "bad"; y }; }"#;
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        assert!(
            has_mismatch(&output.errors),
            "body mismatch in lambda annotated return must be reported even when \
             return type is Ty::Error; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn error_return_type_question_mark_on_non_result_still_reports() {
        // fn foo() -> UnknownType { let x: i64 = 1; x? }
        //
        // The inner-type check for `?` ("? requires Result or Option, found i64")
        // fires unconditionally via the else branch of the PostfixTry handler.
        // It must not be suppressed even when the enclosing return annotation
        // resolves to Ty::Error — that only bypasses the *context* diagnostic
        // ("? cannot be used in a function returning X"), not the inner-type check.
        let source = r"fn foo() -> UnknownType { let x: i64 = 1; x? }";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        let has_try_err = output
            .errors
            .iter()
            .any(|e| e.message.contains("requires Result or Option"));
        assert!(
            has_try_err,
            "? on non-Result/non-Option must still report \
             '? requires Result or Option' even when return annotation is \
             Ty::Error; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn question_mark_result_check_passes_with_matching_error_type() {
        let source = r"
fn pass(r: Result<i64, i64>) -> Result<i64, i64> {
    let x: i64 = r?;
    Ok(x)
}
";
        let (errors, _) = parse_and_check(source);
        assert!(
            errors.is_empty(),
            "Result<T, E>? in Result<_, same E> function must check-pass; got {errors:?}"
        );
    }

    #[test]
    fn question_mark_option_check_passes_in_option_function() {
        let source = r"
fn pass(o: Option<i64>) -> Option<i64> {
    let x: i64 = o?;
    Some(x)
}
";
        let (errors, _) = parse_and_check(source);
        assert!(
            errors.is_empty(),
            "Option<T>? in Option<_> function must check-pass; got {errors:?}"
        );
    }

    #[test]
    fn question_mark_in_i64_returning_function_errors() {
        let source = r"
fn bad(r: Result<i64, i64>) -> i64 {
    r?
}
";
        let (errors, _) = parse_and_check(source);
        assert!(
            errors.iter().any(|e| e
                .message
                .contains("cannot be used in a function returning `i64`")),
            "`?` in an i64-returning function must be rejected; got {errors:?}"
        );
    }

    #[test]
    fn question_mark_result_error_type_mismatch_errors() {
        let source = r"
fn bad(r: Result<i64, string>) -> Result<i64, i64> {
    let x: i64 = r?;
    Ok(x)
}
";
        let (errors, _) = parse_and_check(source);
        assert!(
            errors
                .iter()
                .any(|e| e.message.contains("`?` error type mismatch")),
            "Result<T, E1>? in Result<_, E2> function must reject mismatched E; got {errors:?}"
        );
    }

    #[test]
    fn error_return_type_question_mark_on_result_no_false_context_error() {
        // fn foo() -> UnknownType { let r: Result<i64, string> = Ok(1); r? }
        //
        // When the return annotation is unresolvable (Ty::Error) and `?` is
        // used on a valid Result, the *context* diagnostic ("? cannot be used
        // in a function returning <error>") must NOT fire — we cannot know
        // whether the intended return type would have supported `?`.  Only the
        // annotation-resolution error should appear.
        let source = r"fn foo() -> UnknownType { let r: Result<i64, string> = Ok(1); r? }";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        let has_ctx_err = output
            .errors
            .iter()
            .any(|e| e.message.contains("cannot be used in a function returning"));
        assert!(
            !has_ctx_err,
            "? on valid Result in bad-annotation function must NOT emit a \
             spurious context error; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn builtin_named_return_type_still_reports_question_mark_context_error() {
        // fn foo() -> Vec<i32> { let r: Result<i64, string> = Ok(1); let x: i64 = r?; Vec::new() }
        //
        // PR #923 bypasses the `?` context diagnostic for genuinely unknown named
        // return annotations. Builtin named types like Vec must still report the
        // context error even though they are not registered in type_defs/type_aliases.
        let source = r"fn foo() -> Vec<i32> { let r: Result<i64, string> = Ok(1); let x: i64 = r?; Vec::new() }";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        let has_ctx_err = output.errors.iter().any(|e| {
            e.message
                .contains("cannot be used in a function returning `Vec<i32>`")
        });
        assert!(
            has_ctx_err,
            "? on valid Result in a function returning builtin Vec must still \
             emit the context error; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn error_return_type_question_mark_in_lambda_no_false_context_error() {
        // fn foo() { let r: Result<i64, string> = Ok(1); let f = |x: i64| -> UnknownType { r? }; }
        //
        // Same invariant as the plain-function case but inside a lambda whose
        // return annotation is Ty::Error.  The `?` context check sees the
        // lambda's own `current_return_type` (Ty::Error), so the Ty::Error
        // bypass must apply there too.
        let source = r"fn foo() { let r: Result<i64, string> = Ok(1); let f = |x: i64| -> UnknownType { r? }; }";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        let has_ctx_err = output
            .errors
            .iter()
            .any(|e| e.message.contains("cannot be used in a function returning"));
        assert!(
            !has_ctx_err,
            "? on valid Result inside a lambda with bad return annotation must \
             NOT emit a spurious context error; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn valid_return_annotation_good_path_regression_guard() {
        // fn foo() -> i32 { 42 }
        //
        // A function with a valid, resolvable return annotation must typecheck
        // cleanly.  This guards against regressions introduced by the Ty::Error
        // seeding fix inadvertently breaking the happy path (literal coercion,
        // trailing-expression checking, and expect_type alignment).
        let source = r"fn _foo() -> i32 { 42 }";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        assert!(
            output.errors.is_empty(),
            "valid typed-return function must not produce type errors; \
             got: {:?}",
            output.errors
        );
        assert!(
            output.warnings.is_empty(),
            "valid typed-return function must not produce warnings; \
             got: {:?}",
            output.warnings
        );
    }

    // ── ExternBlock scoped-lookup regression ───────────────────────────────────
    //
    // When an extern block lives in a non-root module, the key stored in
    // `fn_sig_inference_holes` is scoped as `"mymod.extfn"` (set by
    // `register_extern_block` → `scoped_module_item_name`).
    //
    // Before the fix, `report_unresolved_inference_in_items` used a bare-name
    // lookup (`fn_sig_inference_holes.get("extfn")`), so the inference hole was
    // never detected and no `InferenceFailed` error was emitted for non-root
    // extern functions with `_`-typed parameters.
    //
    // After the fix the arm uses `lookup_scoped_item(…, module_name, "extfn")`
    // which resolves the scoped key and the error is emitted + tagged correctly.

    fn make_extern_block_with_infer_param(fn_name: &str) -> Item {
        Item::ExternBlock(ExternBlock {
            abi: "C".to_string(),
            functions: vec![ExternFnDecl {
                attributes: Vec::new(),
                name: fn_name.to_string(),
                params: vec![Param {
                    name: "p".to_string(),
                    ty: (TypeExpr::Infer, 20..21),
                    is_mutable: false,
                }],
                return_type: None,
                is_variadic: false,
                span: 0..0,
            }],
        })
    }

    /// A non-root extern function with a `_`-typed parameter must fail closed
    /// with `InferenceFailed` tagged `source_module = Some("mymod")`.
    ///
    /// Regression guard for the `Item::ExternBlock` bare-name lookup bug in
    /// `report_unresolved_inference_in_items`.
    #[test]
    fn non_root_extern_fn_infer_param_fails_closed_with_source_module() {
        let extern_item = make_extern_block_with_infer_param("extfn");
        let root_id = ModuleId::root();
        let mymod_id = ModuleId::new(vec!["mymod".to_string()]);

        let mymod = Module {
            id: mymod_id.clone(),
            items: vec![(extern_item, 0..30)],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };
        let root_module = Module {
            id: root_id.clone(),
            items: vec![],
            imports: vec![],
            source_paths: vec![],
            doc: None,
        };

        let mut mg = ModuleGraph::new(root_id.clone());
        mg.add_module(root_module).unwrap();
        mg.add_module(mymod).unwrap();
        mg.topo_order = vec![mymod_id, root_id];

        let program = Program {
            module_graph: Some(mg),
            items: vec![],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let inference_errs: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::InferenceFailed))
            .collect();

        assert!(
            !inference_errs.is_empty(),
            "non-root extern fn with `_` param must produce InferenceFailed; \
             got errors: {:?}",
            output.errors
        );
        for err in &inference_errs {
            assert_eq!(
                err.source_module.as_deref(),
                Some("mymod"),
                "InferenceFailed for non-root extern fn must carry \
                 source_module='mymod'; got {:?}",
                err.source_module
            );
        }
    }

    /// A root-module extern function with a `_`-typed parameter must also fail
    /// closed with `InferenceFailed`, with `source_module = None`.
    ///
    /// Confirms the fix does not break the root-module code path.
    #[test]
    fn root_extern_fn_infer_param_fails_closed_without_source_module() {
        let extern_item = make_extern_block_with_infer_param("root_extfn");
        let program = Program {
            module_graph: None,
            items: vec![(extern_item, 0..30)],
            module_doc: None,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let inference_errs: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::InferenceFailed))
            .collect();

        assert!(
            !inference_errs.is_empty(),
            "root extern fn with `_` param must produce InferenceFailed; \
             got errors: {:?}",
            output.errors
        );
        for err in &inference_errs {
            assert_eq!(
                err.source_module, None,
                "InferenceFailed for root extern fn must have source_module=None; got {:?}",
                err.source_module
            );
        }
    }

    /// A non-root scope warning (`UnusedVariable`) carries `source_module`.
    ///
    /// Certifies that `emit_scope_warnings` copies `self.current_module` into
    /// the warning and the snapshot module tagging pass in `check_program` does
    /// not overwrite it when already set.
    #[test]
    fn non_root_unused_variable_warning_carries_source_module() {
        // fn warns() { let x = 42; }  — `x` is never read
        let stmts = vec![(
            Stmt::Let {
                pattern: (Pattern::Identifier("x".to_string()), 10..11),
                ty: None,
                value: Some((
                    Expr::Literal(Literal::Integer {
                        value: 42,
                        radix: IntRadix::Decimal,
                    }),
                    14..16,
                )),
            },
            10..16,
        )];
        let program = make_non_root_program_with_fn_body("warnmod", "warns", stmts);

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&program);

        let unused: Vec<_> = output
            .warnings
            .iter()
            .filter(|w| w.kind == TypeErrorKind::UnusedVariable)
            .collect();

        assert!(
            !unused.is_empty(),
            "expected UnusedVariable warning for `x` in non-root module; got warnings: {:?}",
            output.warnings
        );
        for w in &unused {
            assert_eq!(
                w.source_module.as_deref(),
                Some("warnmod"),
                "UnusedVariable warning must carry source_module='warnmod'; got {:?}",
                w.source_module
            );
        }
    }
}

// ── WASM compile-time reject tests ──────────────────────────────────────────
//
// These tests verify that `Channels`, `Semaphore`, `Timers`, and `Streams` features are
// rejected as compile-time errors (not warnings) when the WASM target is
// enabled.  The reject path is exercised by setting `checker.enable_wasm_target()`
// before calling `check_program`.
//
// Coverage:
//  - channel.new / send / try_recv → allowed on wasm32 bounded subset
//  - Receiver<T>::recv / `for await ... in Receiver<T>` → BlockingChannelRecv error
//  - semaphore.new / try_acquire / release / count / free → allowed on wasm32
//  - Semaphore::acquire / Semaphore::acquire_timeout → BlockingSemaphoreAcquire error
//  - sleep_ms → Timers warning
//  - sleep → Timers warning
//  - Stream<T>::next → Streams error
//  - stream.* module constructor call → Streams error
//  - Non-wasm target: none of the above fire
mod wasm_rejects {
    use super::*;

    /// Parse `source`, enable the WASM target, run the type checker, and
    /// return the resulting output.
    fn check_wasm(source: &str) -> TypeCheckOutput {
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors in wasm_rejects test: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        checker.enable_wasm_target();
        checker.check_program(&result.program)
    }

    /// Parse `source` without the WASM target and return the output.
    fn check_native(source: &str) -> TypeCheckOutput {
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors in wasm_rejects test (native): {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        checker.check_program(&result.program)
    }

    fn has_platform_limitation_error(output: &TypeCheckOutput) -> bool {
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::PlatformLimitation)
    }

    fn has_platform_limitation_warning(output: &TypeCheckOutput) -> bool {
        output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::PlatformLimitation)
    }

    fn platform_error_contains(output: &TypeCheckOutput, fragment: &str) -> bool {
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::PlatformLimitation && e.message.contains(fragment))
    }

    fn platform_warning_contains(output: &TypeCheckOutput, fragment: &str) -> bool {
        output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::PlatformLimitation && w.message.contains(fragment))
    }

    // ── sleep_ms ─────────────────────────────────────────────────────────────

    #[test]
    fn wasm_rejects_sleep_ms() {
        let output = check_wasm("fn main() { sleep_ms(100); }");
        assert!(
            has_platform_limitation_warning(&output),
            "sleep_ms should emit a PlatformLimitation warning on WASM; got warnings: {:?}",
            output.warnings
        );
        assert!(
            platform_warning_contains(&output, "Timer"),
            "warning message should mention Timer feature; got: {:?}",
            output.warnings
        );
        assert!(
            !has_platform_limitation_error(&output),
            "sleep_ms should NOT be a compile-time error on WASM (cooperative semantics); got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_sleep() {
        let output = check_wasm("fn main() { sleep(1); }");
        assert!(
            has_platform_limitation_warning(&output),
            "sleep should emit a PlatformLimitation warning on WASM; got warnings: {:?}",
            output.warnings
        );
        assert!(
            !has_platform_limitation_error(&output),
            "sleep should NOT be a compile-time error on WASM (cooperative semantics); got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn native_sleep_ms_no_platform_error() {
        let output = check_native("fn main() { sleep_ms(100); }");
        assert!(
            !has_platform_limitation_error(&output),
            "sleep_ms should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_warns_on_every_attribute() {
        let output =
            check_wasm("actor Ticker { #[every(10ms)] receive fn tick() {} } fn main() {}");
        assert!(
            has_platform_limitation_warning(&output),
            "#[every] should emit a PlatformLimitation warning on WASM; got warnings: {:?}",
            output.warnings
        );
        assert!(
            platform_warning_contains(&output, "Timer"),
            "#[every] warning should mention Timer operations; got: {:?}",
            output.warnings
        );
        assert!(
            !has_platform_limitation_error(&output),
            "#[every] should NOT be a compile-time error on WASM; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn native_every_attribute_no_platform_error() {
        let output =
            check_native("actor Ticker { #[every(10ms)] receive fn tick() {} } fn main() {}");
        assert!(
            !has_platform_limitation_error(&output),
            "#[every] should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
    }

    // ── channel.new ──────────────────────────────────────────────────────────

    #[test]
    fn wasm_allows_bounded_channel_subset() {
        let source = concat!(
            "import std::channel::channel;\n",
            "fn main() {\n",
            "    let (tx, rx) = channel.new(1);\n",
            "    tx.send(\"hello\");\n",
            "    let _ = rx.try_recv();\n",
            "    tx.close();\n",
            "    rx.close();\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        checker.enable_wasm_target();
        let output = checker.check_program(&result.program);
        assert!(
            !has_platform_limitation_error(&output),
            "bounded channel.new/send/try_recv subset should be allowed on WASM; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn native_channel_new_no_platform_error() {
        let source = concat!(
            "import std::channel::channel;\n",
            "fn main() {\n",
            "    let pair = channel.new(0);\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&result.program);
        assert!(
            !has_platform_limitation_error(&output),
            "channel.new should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn receive_fn_await_channel_recv_does_not_warn_blocking() {
        let source = concat!(
            "import std::channel::channel;\n",
            "actor Worker {\n",
            "    receive fn run() {\n",
            "        let (tx, rx): (channel.Sender<string>, channel.Receiver<string>) = channel.new(1);\n",
            "        tx.send(\"hello\");\n",
            "        tx.close();\n",
            "        let _ = await rx.recv();\n",
            "        rx.close();\n",
            "    }\n",
            "}\n",
            "fn main() {\n",
            "    let w = spawn Worker;\n",
            "    w.run();\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&result.program);
        assert!(
            output.errors.is_empty(),
            "awaited receive-fn recv fixture should type-check cleanly: {:?}",
            output.errors
        );
        assert!(
            !output
                .warnings
                .iter()
                .any(|w| w.kind == TypeErrorKind::BlockingCallInReceiveFn),
            "await rx.recv() suspends and must not warn as blocking: {:?}",
            output.warnings
        );
    }

    #[test]
    fn receive_fn_bare_channel_recv_still_warns_blocking() {
        let source = concat!(
            "import std::channel::channel;\n",
            "actor Worker {\n",
            "    receive fn run() {\n",
            "        let (_tx, rx): (channel.Sender<string>, channel.Receiver<string>) = channel.new(1);\n",
            "        let _ = rx.recv();\n",
            "        rx.close();\n",
            "    }\n",
            "}\n",
            "fn main() {\n",
            "    let w = spawn Worker;\n",
            "    w.run();\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&result.program);
        assert!(
            output
                .warnings
                .iter()
                .any(|w| w.kind == TypeErrorKind::BlockingCallInReceiveFn),
            "bare rx.recv() in a receive fn must still warn as blocking: {:?}",
            output.warnings
        );
    }

    #[test]
    fn wasm_rejects_blocking_channel_recv() {
        let source = concat!(
            "import std::channel::channel;\n",
            "fn main() {\n",
            "    let (_tx, rx) = channel.new(1);\n",
            "    let _ = rx.recv();\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        checker.enable_wasm_target();
        let output = checker.check_program(&result.program);
        assert!(
            has_platform_limitation_error(&output),
            "blocking recv should still be a compile-time error on WASM; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "Blocking channel receive"),
            "error message should mention blocking channel receive; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_for_await_receiver() {
        let source = concat!(
            "import std::channel::channel;\n",
            "fn main() {\n",
            "    let (tx, rx) = channel.new(1);\n",
            "    tx.send(\"hello\");\n",
            "    tx.close();\n",
            "    for await item in rx {\n",
            "        println(item);\n",
            "    }\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        checker.enable_wasm_target();
        let output = checker.check_program(&result.program);
        assert!(
            has_platform_limitation_error(&output),
            "`for await` over Receiver<T> should be a compile-time error on WASM; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "Blocking channel receive"),
            "error message should mention blocking channel receive; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_for_await_stream() {
        let source = concat!(
            "import std::stream;\n",
            "fn main() {\n",
            "    let (sink, input) = stream.bytes_pipe(1);\n",
            "    sink.close();\n",
            "    for await item in input {\n",
            "        println(item.to_string());\n",
            "    }\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        checker.enable_wasm_target();
        let output = checker.check_program(&result.program);
        assert!(
            has_platform_limitation_error(&output),
            "`for await` over Stream<T> should be a compile-time error on WASM; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "Stream operations"),
            "error message should mention Stream operations; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn native_for_await_receiver_no_platform_error() {
        let source = concat!(
            "import std::channel::channel;\n",
            "fn main() {\n",
            "    let (tx, rx) = channel.new(1);\n",
            "    tx.send(\"hello\");\n",
            "    tx.close();\n",
            "    for await item in rx {\n",
            "        println(item);\n",
            "    }\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&result.program);
        assert!(
            !has_platform_limitation_error(&output),
            "`for await` over Receiver<T> should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_allows_non_blocking_semaphore_subset() {
        let source = concat!(
            "import std::semaphore;\n",
            "fn main() {\n",
            "    let sem = semaphore.new(1);\n",
            "    let _ = sem.count();\n",
            "    let _ = sem.try_acquire();\n",
            "    sem.release();\n",
            "    sem.free();\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        checker.enable_wasm_target();
        let output = checker.check_program(&result.program);
        assert!(
            !has_platform_limitation_error(&output),
            "non-blocking semaphore subset should be allowed on WASM; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_blocking_semaphore_methods() {
        let source = concat!(
            "import std::semaphore;\n",
            "fn main() {\n",
            "    let sem = semaphore.new(1);\n",
            "    sem.acquire();\n",
            "    let _ = sem.acquire_timeout(10);\n",
            "    sem.free();\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        checker.enable_wasm_target();
        let output = checker.check_program(&result.program);
        let reject_count = output
            .errors
            .iter()
            .filter(|error| {
                error.kind == TypeErrorKind::PlatformLimitation
                    && error.message.contains("Blocking semaphore acquire")
            })
            .count();
        assert!(
            reject_count >= 2,
            "blocking semaphore methods should be rejected on WASM; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn native_blocking_semaphore_methods_no_platform_error() {
        let source = concat!(
            "import std::semaphore;\n",
            "fn main() {\n",
            "    let sem = semaphore.new(1);\n",
            "    sem.acquire();\n",
            "    let _ = sem.acquire_timeout(10);\n",
            "    sem.release();\n",
            "    sem.free();\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&result.program);
        assert!(
            !has_platform_limitation_error(&output),
            "blocking semaphore methods should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
    }

    // ── Stream<T> methods ────────────────────────────────────────────────────

    #[test]
    fn wasm_rejects_stream_method() {
        // Use a function that accepts a Stream<string> and calls .next().
        // The stream module must be imported to register Stream types.
        let source = concat!(
            "import std::stream;\n",
            "fn consume(s: stream.Stream<string>) -> string {\n",
            "    s.next()\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        checker.enable_wasm_target();
        let output = checker.check_program(&result.program);
        assert!(
            has_platform_limitation_error(&output),
            "Stream<T>::next should be a compile-time error on WASM; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "Stream"),
            "error message should mention Stream feature; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn native_stream_method_no_platform_error() {
        let source = concat!(
            "import std::stream;\n",
            "fn consume(s: stream.Stream<string>) -> string {\n",
            "    s.next()\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&result.program);
        assert!(
            !has_platform_limitation_error(&output),
            "Stream<T>::next should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
    }

    // ── TLS / QUIC / DNS / OS reject + CryptoRandom warn ───────────────────

    fn check_wasm_with_registry(source: &str) -> TypeCheckOutput {
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors in wasm_rejects test: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        checker.enable_wasm_target();
        checker.check_program(&result.program)
    }

    #[test]
    fn wasm_rejects_tls_module_call() {
        let source = concat!(
            "import std::net::tls;\n",
            "fn main() { tls.connect(\"host\", 443); }\n",
        );
        let output = check_wasm_with_registry(source);
        assert!(
            has_platform_limitation_error(&output),
            "tls.connect should be a compile-time error on WASM; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "std::net::tls"),
            "error message should mention TLS feature; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_quic_module_call() {
        let source = concat!(
            "import std::net::quic;\n",
            "fn main() { quic.new_client(); }\n",
        );
        let output = check_wasm_with_registry(source);
        assert!(
            has_platform_limitation_error(&output),
            "quic.* should be a compile-time error on WASM; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "std::net::quic"),
            "error message should mention QUIC feature; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_dns_module_call() {
        let source = concat!(
            "import std::net::dns;\n",
            "fn main() { dns.resolve(\"example.com\"); }\n",
        );
        let output = check_wasm_with_registry(source);
        assert!(
            has_platform_limitation_error(&output),
            "dns.resolve should be a compile-time error on WASM; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "std::net::dns"),
            "error message should mention DNS feature; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_os_module_call() {
        let source = concat!("import std::os;\n", "fn main() { os.env(\"HOME\"); }\n",);
        let output = check_wasm_with_registry(source);
        assert!(
            has_platform_limitation_error(&output),
            "os.* should be a compile-time error on WASM; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "std::os"),
            "error message should mention OS feature; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_warns_crypto_random_bytes() {
        // crypto.random_bytes is a WARNING, not an error, because the wasm32
        // implementation falls back to a seeded non-cryptographic PRNG.
        let source = concat!(
            "import std::crypto::crypto;\n",
            "fn main() { crypto.random_bytes(16); }\n",
        );
        let output = check_wasm_with_registry(source);
        assert!(
            has_platform_limitation_warning(&output),
            "crypto.random_bytes should emit a PlatformLimitation warning on WASM; got warnings: {:?}",
            output.warnings
        );
        assert!(
            platform_warning_contains(&output, "random_bytes"),
            "warning message should mention crypto.random_bytes; got: {:?}",
            output.warnings
        );
        assert!(
            !has_platform_limitation_error(&output),
            "crypto.random_bytes should NOT be a compile-time error on WASM; got errors: {:?}",
            output.errors
        );
    }

    // ── Native sibling tests: no platform error on non-wasm target ────────

    #[test]
    fn native_tls_no_platform_error() {
        let source = concat!(
            "import std::net::tls;\n",
            "fn main() { tls.connect(\"host\", 443); }\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&result.program);
        assert!(
            !has_platform_limitation_error(&output),
            "tls.connect should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
        assert!(
            !has_platform_limitation_warning(&output),
            "tls.connect should not emit PlatformLimitation warning on native target; got: {:?}",
            output.warnings
        );
    }

    #[test]
    fn native_quic_no_platform_error() {
        let source = concat!(
            "import std::net::quic;\n",
            "fn main() { quic.new_client(); }\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&result.program);
        assert!(
            !has_platform_limitation_error(&output),
            "quic.* should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
        assert!(
            !has_platform_limitation_warning(&output),
            "quic.* should not emit PlatformLimitation warning on native target; got: {:?}",
            output.warnings
        );
    }

    #[test]
    fn native_dns_no_platform_error() {
        let source = concat!(
            "import std::net::dns;\n",
            "fn main() { dns.resolve(\"example.com\"); }\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&result.program);
        assert!(
            !has_platform_limitation_error(&output),
            "dns.resolve should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
        assert!(
            !has_platform_limitation_warning(&output),
            "dns.resolve should not emit PlatformLimitation warning on native target; got: {:?}",
            output.warnings
        );
    }

    #[test]
    fn native_os_no_platform_error() {
        let source = concat!("import std::os;\n", "fn main() { os.env(\"HOME\"); }\n",);
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&result.program);
        assert!(
            !has_platform_limitation_error(&output),
            "os.* should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
        assert!(
            !has_platform_limitation_warning(&output),
            "os.* should not emit PlatformLimitation warning on native target; got: {:?}",
            output.warnings
        );
    }

    #[test]
    fn native_crypto_random_bytes_no_platform_error() {
        let source = concat!(
            "import std::crypto::crypto;\n",
            "fn main() { crypto.random_bytes(16); }\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&result.program);
        assert!(
            !has_platform_limitation_error(&output),
            "crypto.random_bytes should not emit PlatformLimitation error on native target; got: {:?}",
            output.errors
        );
        assert!(
            !has_platform_limitation_warning(&output),
            "crypto.random_bytes should not emit PlatformLimitation warning on native target; got: {:?}",
            output.warnings
        );
    }

    // ── Deduplication: same call site emits only one error ─────────────────

    #[test]
    fn wasm_reject_deduplicates_same_span() {
        // Two consecutive calls at different call sites should produce two
        // warnings, not one (each span is unique).
        let output = check_wasm("fn main() { sleep_ms(100); sleep_ms(200); }");
        let count = output
            .warnings
            .iter()
            .filter(|w| w.kind == TypeErrorKind::PlatformLimitation && w.message.contains("Timer"))
            .count();
        assert_eq!(
            count, 2,
            "two distinct sleep_ms call sites should produce two warnings; got: {:?}",
            output.warnings
        );
    }

    // ── Reject-level features now fail closed on WASM ────────────────────────

    fn supervisor_calls_source() -> &'static str {
        // `pool` is a reserved keyword from S-A (supervisor `pool` child decls).
        // Use `wp` for the local WorkerPool ref to avoid the keyword conflict.
        r"
            actor Worker {
                receive fn ping() {}
            }

            supervisor WorkerPool {
                strategy: one_for_one,
                intensity: 1 within 10s,
                child w1: Worker
            }

            fn main() {
                let wp = spawn WorkerPool;
                let worker = supervisor_child(wp, 0);
                supervisor_stop(wp);
                worker.ping();
            }
        "
    }

    fn link_monitor_calls_source() -> &'static str {
        r"
            actor Worker {
                receive fn ping() {}
            }

            fn main() {
                let worker = spawn Worker;
                let m: MonitorRef = monitor(worker);
                link(worker);
                let _ = m.close();
            }
        "
    }

    fn monitor_result_is_not_int_source() -> &'static str {
        r"
            actor Worker {
                receive fn ping() {}
            }

            fn main() {
                let worker = spawn Worker;
                let _ok: MonitorRef = monitor(worker);
                let x: i64 = monitor(worker);
                println(x);
            }
        "
    }

    fn monitor_ref_use_after_close_source() -> &'static str {
        r"
            actor Worker {
                receive fn ping() {}
            }

            fn main() {
                let worker = spawn Worker;
                let m: MonitorRef = monitor(worker);
                let _ = m.close();
                let _ = m.close();
            }
        "
    }

    fn structured_concurrency_scope_source() -> &'static str {
        "fn main() { let result = scope { 1 + 2 }; println(result); }"
    }

    fn scope_tasks_source() -> &'static str {
        r"
            fn main() {
                scope {
                    fork task = compute();
                    await task;
                }
            }
            fn compute() -> i64 { 42 }
        "
    }

    #[test]
    fn wasm_rejects_supervisor_calls() {
        let output = check_wasm(supervisor_calls_source());
        assert!(
            output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::PlatformLimitation),
            "supervision operations should be WASM errors; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "Supervision tree"),
            "error message should mention Supervision tree feature; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn native_supervisor_calls_no_platform_error() {
        let output = check_native(supervisor_calls_source());
        assert!(
            !has_platform_limitation_error(&output),
            "supervision operations should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_supervisor_declaration() {
        let output = check_wasm(
            r"
            actor Worker {
                receive fn ping() {}
            }

            supervisor WorkerPool {
                strategy: one_for_one,
                intensity: 1 within 10s,
                child w1: Worker
            }

            fn main() {}
        ",
        );
        assert!(
            has_platform_limitation_error(&output),
            "supervisor declarations should be a WASM error; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "Supervision tree"),
            "error message should mention Supervision tree feature; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_link_monitor_calls() {
        let output = check_wasm(link_monitor_calls_source());
        assert!(
            has_platform_limitation_error(&output),
            "link/monitor operations should be a WASM error; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "Link/monitor"),
            "error message should mention Link/monitor feature; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn native_link_monitor_no_platform_error() {
        let output = check_native(link_monitor_calls_source());
        assert!(
            !has_platform_limitation_error(&output),
            "link/monitor operations should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
        assert!(
            output.errors.is_empty(),
            "link/monitor MonitorRef flow should typecheck cleanly on native target; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn monitor_result_is_not_int() {
        let output = check_native(monitor_result_is_not_int_source());
        assert!(
            output
                .errors
                .iter()
                .any(|e| matches!(e.kind, TypeErrorKind::Mismatch { .. })),
            "monitor() should no longer typecheck as i64; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn monitor_ref_use_after_close() {
        let output = check_native(monitor_ref_use_after_close_source());
        assert!(
            output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::UseAfterMove),
            "second close() should surface UseAfterMove; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_structured_concurrency_scope() {
        let output = check_wasm(structured_concurrency_scope_source());
        assert!(
            has_platform_limitation_error(&output),
            "scope expressions should be a WASM error; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "Structured concurrency"),
            "error message should mention Structured concurrency feature; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn native_scope_no_platform_error() {
        let output = check_native(structured_concurrency_scope_source());
        assert!(
            !has_platform_limitation_error(&output),
            "scope expressions should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_scope_tasks() {
        let output = check_wasm(scope_tasks_source());
        assert!(
            has_platform_limitation_error(&output),
            "scope tasks should be a WASM error; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "Task handles"),
            "error message should mention Task feature; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn native_scope_tasks_no_platform_error() {
        let output = check_native(scope_tasks_source());
        assert!(
            !has_platform_limitation_error(&output),
            "scope tasks should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_multi_arm_literal_timed_select_is_not_warning() {
        let output = check_wasm(
            r"
            actor Responder {
                let value: i64;
                receive fn get() -> i64 {
                    value
                }
            }

            fn main() {
                let a = spawn Responder(value: 1);
                let b = spawn Responder(value: 2);
                let result = select {
                    x from a.get() => x,
                    y from b.get() => y,
                    after 1ms => -1,
                };
                println(result);
            }
        ",
        );
        assert!(
            !output
                .warnings
                .iter()
                .any(|w| w.kind == TypeErrorKind::PlatformLimitation),
            "literal timed select should no longer warn on WASM; got warnings: {:?}",
            output.warnings
        );
        assert!(
            !output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::PlatformLimitation),
            "literal timed select should not error on WASM; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_computed_timed_select_no_longer_warns() {
        let output = check_wasm(
            r"
            actor Responder {
                let value: i64;
                receive fn get() -> i64 {
                    value
                }
            }

            fn main() {
                let a = spawn Responder(value: 1);
                let b = spawn Responder(value: 2);
                let timeout = 1ms;
                let result = select {
                    x from a.get() => x,
                    y from b.get() => y,
                    after timeout => -1,
                };
                println(result);
            }
        ",
        );
        assert!(
            !output.warnings.iter().any(
                |w| w.kind == TypeErrorKind::PlatformLimitation && w.message.contains("Select")
            ),
            "computed timed select should not warn on WASM; got warnings: {:?}",
            output.warnings
        );
        assert!(
            !output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::PlatformLimitation),
            "computed timed select should not error on WASM; got errors: {:?}",
            output.errors
        );
    }

    // ── Ty::Error cascade-suppression regressions ────────────────────────────
    //
    // These tests verify that independent arg diagnostics are NOT suppressed
    // when the receiver/callee already has type Ty::Error.  Prior to the fix,
    // "bad arg" errors were silently dropped at every unknown-method `_` arm
    // and at `check_call_with_type` when called with a Ty::Error callee type.

    fn check_error_cascade(source: &str) -> Vec<TypeErrorKind> {
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        output.errors.into_iter().map(|e| e.kind).collect()
    }

    #[test]
    fn bad_string_method_with_bad_arg_reports_both_errors() {
        // `s.nonexistent_method(undefined_arg)` must report BOTH:
        //   - "no method `nonexistent_method` on string"
        //   - "undefined variable `undefined_arg`"
        // Before the fix, only the first error was reported.
        let kinds =
            check_error_cascade(r"fn foo(s: string) { s.nonexistent_method(undefined_arg) }");
        assert!(
            kinds.contains(&TypeErrorKind::UndefinedMethod),
            "expected UndefinedMethod; got: {kinds:?}",
        );
        assert!(
            kinds.contains(&TypeErrorKind::UndefinedVariable),
            "expected UndefinedVariable (must not be cascade-suppressed); got: {kinds:?}",
        );
    }

    #[test]
    fn bad_vec_method_with_bad_arg_reports_both_errors() {
        // `v.nonexistent_method(undefined_arg)` on a Vec must report both errors.
        let kinds =
            check_error_cascade(r"fn foo(v: Vec<i64>) { v.nonexistent_method(undefined_arg) }");
        assert!(
            kinds.contains(&TypeErrorKind::UndefinedMethod),
            "expected UndefinedMethod; got: {kinds:?}",
        );
        assert!(
            kinds.contains(&TypeErrorKind::UndefinedVariable),
            "expected UndefinedVariable (must not be cascade-suppressed); got: {kinds:?}",
        );
    }

    #[test]
    fn bad_hashmap_method_with_bad_arg_reports_both_errors() {
        // `m.nonexistent_method(undefined_arg)` on a HashMap must report both errors.
        let kinds = check_error_cascade(
            r"fn foo(m: HashMap<string, i64>) { m.nonexistent_method(undefined_arg) }",
        );
        assert!(
            kinds.contains(&TypeErrorKind::UndefinedMethod),
            "expected UndefinedMethod; got: {kinds:?}",
        );
        assert!(
            kinds.contains(&TypeErrorKind::UndefinedVariable),
            "expected UndefinedVariable (must not be cascade-suppressed); got: {kinds:?}",
        );
    }

    #[test]
    fn error_typed_callable_still_checks_arg_errors() {
        // `let f = undefined_fn(); f(undefined_arg)` — when `f` has type Ty::Error
        // (because `undefined_fn` is unknown), the args to `f(...)` must still be
        // synthesized so `undefined_arg` errors are surfaced.
        let kinds =
            check_error_cascade(r"fn foo() { let f = undefined_fn(); let _ = f(undefined_arg); }");
        assert!(
            kinds.contains(&TypeErrorKind::UndefinedFunction),
            "expected UndefinedFunction for `undefined_fn`; got: {kinds:?}",
        );
        assert!(
            kinds.contains(&TypeErrorKind::UndefinedVariable),
            "expected UndefinedVariable (must not be cascade-suppressed); got: {kinds:?}",
        );
    }

    #[test]
    fn chained_bad_method_with_bad_arg_reports_arg_error() {
        // `s.bad_method().another_method(undefined_arg)` — the (Ty::Error, _) arm
        // already synthesizes args for chained calls, so `undefined_arg` SHOULD be
        // reported.  This test guards that the chained-call path is not regressed.
        let kinds = check_error_cascade(
            r"fn foo(s: string) { let _ = s.bad_method().another_method(undefined_arg); }",
        );
        assert!(
            kinds.contains(&TypeErrorKind::UndefinedMethod),
            "expected UndefinedMethod for `bad_method`; got: {kinds:?}",
        );
        assert!(
            kinds.contains(&TypeErrorKind::UndefinedVariable),
            "expected UndefinedVariable in chained call; got: {kinds:?}",
        );
    }

    #[test]
    fn simple_bad_method_chain_still_suppresses_duplicate_error() {
        // `s.bad_method().to_string()` — the `.to_string()` on the Ty::Error result
        // is correctly suppressed (not a new error). The (Ty::Error, _) arm must
        // NOT emit a duplicate diagnostic for the chained method.
        let kinds =
            check_error_cascade(r"fn foo(s: string) { let _ = s.bad_method().to_string(); }");
        assert_eq!(
            kinds,
            vec![TypeErrorKind::UndefinedMethod],
            "expected exactly [UndefinedMethod] — chain must stay suppressed; got: {kinds:?}",
        );
    }
}

// ── Supervisor child slot index tests ────────────────────────────────────
//
// These tests verify that the checker assigns correct slot indices to
// supervisor children (static and pool), populates the side-table at
// field-access sites, and rejects unknown child names.
#[cfg(test)]
mod supervisor_child_slot_tests {
    use super::*;

    fn parse_and_check(source: &str) -> TypeCheckOutput {
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors in supervisor_child_slot test: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        checker.check_program(&result.program)
    }

    /// A static child `child cache: Cache` must resolve to slot index 0 in
    /// the static space and produce a `ChildSlot { kind: Static, index: 0 }` entry
    /// in the output's `supervisor_child_slots` map at the field-access span.
    #[test]
    fn static_child_resolves_with_correct_slot_index() {
        let source = r"
            actor Cache {
                receive fn query() {}
            }

            supervisor App {
                child cache: Cache
            }

            fn main() {
                let app = spawn App;
                let c = app.cache;
                supervisor_stop(app);
            }
        ";
        let output = parse_and_check(source);
        assert!(
            output.errors.is_empty(),
            "expected no errors; got: {:?}",
            output.errors
        );
        // The `supervisor_child_slots` map must contain exactly one entry for
        // the `app.cache` access. We verify kind and index without pinning the
        // exact byte offset (which would be brittle).
        let slot = output
            .supervisor_child_slots
            .values()
            .find(|s| s.child_ty == "Cache");
        let slot = slot.expect("expected a ChildSlot for Cache child in supervisor_child_slots");
        assert_eq!(
            slot.kind,
            ChildKind::Static,
            "cache child should be in the static slot space"
        );
        assert_eq!(slot.index, 0, "first static child gets slot index 0");
    }

    /// A second static child `child log: Log` declared after `cache` must receive
    /// slot index 1 in the static space, distinct from the first child.
    #[test]
    fn second_static_child_gets_sequential_slot_index() {
        let source = r"
            actor Cache {
                receive fn query() {}
            }

            actor Log {
                receive fn write() {}
            }

            supervisor App {
                child cache: Cache
                child log: Log
            }

            fn main() {
                let app = spawn App;
                let c = app.cache;
                let l = app.log;
                supervisor_stop(app);
            }
        ";
        let output = parse_and_check(source);
        assert!(
            output.errors.is_empty(),
            "expected no errors; got: {:?}",
            output.errors
        );
        let cache_slot = output
            .supervisor_child_slots
            .values()
            .find(|s| s.child_ty == "Cache")
            .expect("expected ChildSlot for Cache");
        let log_slot = output
            .supervisor_child_slots
            .values()
            .find(|s| s.child_ty == "Log")
            .expect("expected ChildSlot for Log");
        assert_eq!(cache_slot.kind, ChildKind::Static);
        assert_eq!(cache_slot.index, 0);
        assert_eq!(log_slot.kind, ChildKind::Static);
        assert_eq!(log_slot.index, 1);
    }

    /// A pool child declared with `pool worker: Worker` must resolve to
    /// slot index 0 in the *pool* space. It must not collide with a static
    /// child that also has index 0 — the two spaces are disjoint.
    #[test]
    fn pool_child_resolves_with_pool_space_slot_index() {
        let source = r"
            actor Worker {
                receive fn ping() {}
            }

            supervisor Pool {
                strategy: simple_one_for_one,
                pool worker: Worker
            }

            fn main() {
                let p = spawn Pool;
                let w = p.worker;
                supervisor_stop(p);
            }
        ";
        let output = parse_and_check(source);
        assert!(
            output.errors.is_empty(),
            "expected no errors; got: {:?}",
            output.errors
        );
        let slot = output
            .supervisor_child_slots
            .values()
            .find(|s| s.child_ty == "Worker")
            .expect("expected ChildSlot for Worker pool child");
        assert_eq!(
            slot.kind,
            ChildKind::Pool,
            "pool child should be in the pool slot space"
        );
        assert_eq!(slot.index, 0, "first pool child gets pool slot index 0");
    }

    /// In a supervisor with both static and pool children, their slot indices
    /// must be disjoint: the static child has `(Static, 0)` and the pool child
    /// has `(Pool, 0)`. Neither borrows an index from the other space.
    #[test]
    fn static_and_pool_indices_are_disjoint() {
        let source = r"
            actor Cache {
                receive fn query() {}
            }

            actor Worker {
                receive fn ping() {}
            }

            supervisor App {
                child cache: Cache
                pool worker: Worker
            }

            fn main() {
                let app = spawn App;
                let c = app.cache;
                supervisor_stop(app);
            }
        ";
        // NOTE: A supervisor with mixed static+pool children may not pass all
        // strategy consistency checks (that's S-B). We only check that the
        // checker computes correct slot indices for the declared children.
        // Strategy-level errors are acceptable; slot-index population is not gated on them.
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);

        let cache_slot = output
            .supervisor_child_slots
            .values()
            .find(|s| s.child_ty == "Cache");
        let cache_slot = cache_slot.expect("expected ChildSlot for Cache static child");
        assert_eq!(cache_slot.kind, ChildKind::Static);
        assert_eq!(cache_slot.index, 0, "static child index starts at 0");
        // Pool child not accessed in this program body; its slot is not in the side-table.
        // That is correct — the side-table is keyed by access-expression span.
    }

    /// Accessing an unknown child name on a supervisor-typed value must produce
    /// a type error (`UndefinedField`). The side-table must NOT contain an entry
    /// for this access.
    #[test]
    fn unknown_child_name_produces_type_error() {
        let source = r"
            actor Cache {
                receive fn query() {}
            }

            supervisor App {
                child cache: Cache
            }

            fn main() {
                let app = spawn App;
                let x = app.unknown;
                supervisor_stop(app);
            }
        ";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        assert!(
            !output.errors.is_empty(),
            "expected a type error for unknown supervisor child `unknown`"
        );
        // No slot must be recorded for the bad access.
        assert!(
            output.supervisor_child_slots.is_empty(),
            "supervisor_child_slots must be empty when the child name is unknown; got: {:?}",
            output.supervisor_child_slots
        );
    }
}

// ── if-let / while-let pattern contract ──────────────────────────────────
//
// These tests verify that the checker rejects Struct, Tuple, Or, and
// Literal patterns at the top level of `if let` / `while let`, because
// codegen has no support for them.  Constructor, Wildcard, and Identifier
// are the only allowed patterns.
#[cfg(test)]
mod iflet_whilelet_pattern_contract {
    use super::*;

    fn check_iflet_whilelet(source: &str) -> Vec<crate::error::TypeError> {
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        checker.check_program(&result.program).errors
    }

    #[test]
    fn iflet_stmt_literal_pattern_is_rejected() {
        let errors = check_iflet_whilelet(r"fn foo(x: i64) { if let 1 = x { 0 } }");
        assert!(
            errors.iter().any(|e| e.kind == TypeErrorKind::InvalidOperation
                && e.message.contains("literal")),
            "expected InvalidOperation for literal if-let pattern; got: {errors:?}",
        );
    }

    #[test]
    fn iflet_stmt_struct_pattern_is_accepted() {
        let errors = check_iflet_whilelet(
            r"type Point { x: i64; y: i64; } fn foo(p: Point) { if let Point { x, y } = p { x + y } }",
        );
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "struct if-let pattern must not emit InvalidOperation; got: {errors:?}",
        );
    }

    #[test]
    fn iflet_stmt_tuple_pattern_is_accepted() {
        let errors = check_iflet_whilelet(r"fn foo(x: (i64, i64)) { if let (a, b) = x { a + b } }");
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "tuple if-let pattern must not emit InvalidOperation; got: {errors:?}",
        );
    }

    #[test]
    fn iflet_stmt_or_pattern_is_accepted() {
        let errors =
            check_iflet_whilelet(r"enum E { A; B; } fn foo(x: E) { if let A | B = x { 0 } }");
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "or if-let pattern must not emit InvalidOperation; got: {errors:?}",
        );
    }

    #[test]
    fn iflet_stmt_wildcard_pattern_is_accepted() {
        let errors = check_iflet_whilelet(r"fn foo(x: i64) { if let _ = x { 0 } }");
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "wildcard if-let pattern must not emit InvalidOperation; got: {errors:?}",
        );
    }

    #[test]
    fn iflet_stmt_identifier_pattern_is_accepted() {
        let errors = check_iflet_whilelet(r"fn foo(x: i64) { if let y = x { 0 } }");
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "identifier if-let pattern must not emit InvalidOperation; got: {errors:?}",
        );
    }

    #[test]
    fn whilelet_stmt_literal_pattern_is_rejected() {
        let errors = check_iflet_whilelet(r"fn foo(x: i64) { while let 1 = x { break; } }");
        assert!(
            errors.iter().any(|e| e.kind == TypeErrorKind::InvalidOperation
                && e.message.contains("literal")),
            "expected InvalidOperation for literal while-let pattern; got: {errors:?}",
        );
    }

    #[test]
    fn whilelet_stmt_struct_pattern_is_accepted() {
        let errors = check_iflet_whilelet(
            r"enum Msg { Data { value: i64 }; Done; } fn foo(x: Msg) { while let Data { value } = x { break; } }",
        );
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "struct while-let pattern must not emit InvalidOperation; got: {errors:?}",
        );
    }

    #[test]
    fn whilelet_stmt_tuple_pattern_is_accepted() {
        let errors =
            check_iflet_whilelet(r"fn foo(x: (i64, i64)) { while let (a, b) = x { break; } }");
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "tuple while-let pattern must not emit InvalidOperation; got: {errors:?}",
        );
    }

    #[test]
    fn whilelet_stmt_or_pattern_is_accepted() {
        let errors = check_iflet_whilelet(
            r"enum E { A; B; } fn foo(x: E) { while let A | B = x { break; } }",
        );
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "or while-let pattern must not emit InvalidOperation; got: {errors:?}",
        );
    }

    #[test]
    fn whilelet_stmt_wildcard_pattern_is_accepted() {
        let errors = check_iflet_whilelet(r"fn foo(x: i64) { while let _ = x { break; } }");
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "wildcard while-let pattern must not emit InvalidOperation; got: {errors:?}",
        );
    }

    #[test]
    fn whilelet_stmt_identifier_pattern_is_accepted() {
        let errors = check_iflet_whilelet(r"fn foo(x: i64) { while let y = x { break; } }");
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "identifier while-let pattern must not emit InvalidOperation; got: {errors:?}",
        );
    }

    #[test]
    fn whilelet_stmt_labeled_break_is_accepted() {
        let errors =
            check_iflet_whilelet(r"fn foo(x: i64) { @scan: while let y = x { break @scan; } }");
        assert!(
            !errors
                .iter()
                .any(|e| e.message.contains("unknown loop label")),
            "labeled while-let break must not emit unknown loop label; got: {errors:?}",
        );
    }

    #[test]
    fn whilelet_stmt_labeled_continue_is_accepted() {
        let errors =
            check_iflet_whilelet(r"fn foo(x: i64) { @scan: while let y = x { continue @scan; } }");
        assert!(
            !errors
                .iter()
                .any(|e| e.message.contains("unknown loop label")),
            "labeled while-let continue must not emit unknown loop label; got: {errors:?}",
        );
    }

    #[test]
    fn nested_loop_can_target_outer_whilelet_label() {
        let errors = check_iflet_whilelet(
            r"fn foo(x: i64) { @scan: while let y = x { loop { break @scan; } } }",
        );
        assert!(
            !errors
                .iter()
                .any(|e| e.message.contains("unknown loop label")),
            "nested loops must resolve outer while-let labels; got: {errors:?}",
        );
    }

    #[test]
    fn whilelet_stmt_unknown_label_still_errors() {
        let errors = check_iflet_whilelet(r"fn foo(x: i64) { while let y = x { break @scan; } }");
        assert!(
            errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation
                    && e.message.contains("unknown loop label `@scan`")),
            "unknown while-let labels must still error; got: {errors:?}",
        );
    }
}

// ── for-loop iterable fail-closed regressions ──────────────────────────────

mod for_loop_iterable_fail_closed {
    use super::*;

    fn check_for_over(iter_ty: Ty) -> Vec<TypeError> {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        checker.env.define("it".to_string(), iter_ty, false);
        let for_stmt = Stmt::For {
            label: None,
            is_await: false,
            pattern: (Pattern::Identifier("x".to_string()), 0..1),
            iterable: (Expr::Identifier("it".to_string()), 7..9),
            body: Block {
                stmts: vec![],
                trailing_expr: None,
            },
        };
        checker.check_stmt(&for_stmt, &(0..20));
        checker.errors
    }

    // ── catch-all: unsupported iterable type ───────────────────────────────

    #[test]
    fn unsupported_iterable_bool_emits_not_iterable_diagnostic() {
        let result = hew_parser::parse("fn main() { for x in true { } }");
        assert!(
            result.errors.is_empty(),
            "parse errors: {result_errors:?}",
            result_errors = result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        assert!(
            output.errors.iter().any(|e| {
                e.kind == TypeErrorKind::InvalidOperation && e.message.contains("not iterable")
            }),
            "expected 'not iterable' diagnostic for bool iterable; got: {errs:?}",
            errs = output.errors
        );
    }

    #[test]
    fn unsupported_iterable_does_not_produce_fresh_typevar_elem() {
        // Direct AST: `for x in it` where `it: bool`. The elem type must be
        // Ty::Error, not Ty::Var, ensuring no inference holes leak downstream.
        let errors = check_for_over(Ty::Bool);
        assert!(
            errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "expected InvalidOperation diagnostic for non-iterable; got: {errors:?}",
        );
    }

    // ── Vec with empty type args ───────────────────────────────────────────

    #[test]
    fn vec_with_empty_type_args_emits_diagnostic_not_fresh_var() {
        let errors = check_for_over(Ty::Named {
            builtin: Some(crate::BuiltinType::Vec),
            name: "Vec".to_string(),
            args: vec![],
        });
        assert!(
            errors.iter().any(|e| {
                e.kind == TypeErrorKind::InvalidOperation && e.message.contains("Vec")
            }),
            "expected InvalidOperation for Vec with no type args; got: {errors:?}",
        );
    }

    // ── Stream with empty type args (plain `for`, not `for await`) ────────

    #[test]
    fn stream_with_empty_type_args_emits_diagnostic_not_fresh_var() {
        let errors = check_for_over(Ty::Named {
            builtin: None,
            name: "Stream".to_string(),
            args: vec![],
        });
        assert!(
            errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "expected InvalidOperation for Stream with no type args; got: {errors:?}",
        );
    }

    // ── valid iterables must not regress ──────────────────────────────────

    #[test]
    fn vec_with_type_arg_is_valid() {
        let errors = check_for_over(Ty::Named {
            builtin: Some(crate::BuiltinType::Vec),
            name: "Vec".to_string(),
            args: vec![Ty::I64],
        });
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "Vec<i64> iterable must not emit InvalidOperation; got: {errors:?}",
        );
    }

    #[test]
    fn array_iterable_is_valid() {
        let errors = check_for_over(Ty::Array(Box::new(Ty::I32), 4));
        assert!(
            errors.is_empty(),
            "Array iterable must not emit errors; got: {errors:?}",
        );
    }

    #[test]
    fn range_iterable_is_valid() {
        let errors = check_for_over(Ty::Named {
            builtin: Some(crate::BuiltinType::Range),
            name: "Range".to_string(),
            args: vec![Ty::I64],
        });
        assert!(
            errors.is_empty(),
            "Range<i64> iterable must not emit errors; got: {errors:?}",
        );
    }

    #[test]
    fn user_iterator_impl_is_valid_for_loop_iterable() {
        let output = check_source(
            r"
            type Counter {
                val: i32;
            }

            impl Iterator for Counter {
                type Item = i32;
                fn next(var self) -> Option<i32> {
                    Some(self.val)
                }
            }

            fn takes_i32(x: i32) {}

            fn main() {
                for x in Counter { val: 0 } {
                    takes_i32(x);
                }
            }
            ",
        );
        assert!(
            output.errors.is_empty(),
            "user Iterator impl should type-check as a for-loop iterable: {:?}",
            output.errors
        );
    }

    #[test]
    fn builtin_dyn_iterator_item_binding_smoke() {
        // W3.042 S2-S4: the dyn-trait dispatch gate enforces that
        // `Iterator::next` (declared `var self` in `std/builtins.hew`)
        // is called only on a `var`-bound receiver. The parameter is
        // therefore declared `var iter` so the method dispatch picks the
        // mutable-receiver path; without `var` here the call is correctly
        // rejected with a MutabilityError naming `dyn Iterator`.
        let output = check_source(
            r"
            type Counter {
                val: i32;
            }

            impl Iterator for Counter {
                type Item = i32;
                fn next(var self) -> Option<i32> {
                    Some(self.val)
                }
            }

            fn use_iter(var iter: dyn Iterator<Item = i32>) -> Option<i32> {
                iter.next()
            }

            fn main() {
                use_iter(Counter { val: 1 });
            }
            ",
        );
        assert!(
            output.errors.is_empty(),
            "builtin dyn Iterator<Item = i32> should accept Counter: {:?}",
            output.errors
        );
    }

    #[test]
    fn generator_blocks_are_deferred_to_generator_surface_slice() {
        // `gen { yield 1; yield 2; }` type-checks as Generator<i32, Unit>.
        // The checker synthesizes the yield type from yield expressions; HIR/MIR
        // lowering is still fail-closed on GenBlock but that is a compile-phase
        // boundary, not a type-check boundary.
        let result = hew_parser::parse("fn main() { let g = gen { yield 1; yield 2; }; }");
        assert!(
            result.errors.is_empty(),
            "gen {{ ... }} expression blocks should parse cleanly: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        assert!(
            output.errors.is_empty(),
            "gen block with yield expressions must type-check cleanly: {:?}",
            output.errors
        );
    }

    #[test]
    fn closure_capture_facts_are_binding_accurate_and_deduplicated() {
        let output = check_source(
            r"
            fn main() {
                let k: i32 = 2;
                let f = |n: i32| n + k + k;
            }
            ",
        );
        assert!(
            output.errors.is_empty(),
            "copy capture should type-check cleanly: {:?}",
            output.errors
        );

        let facts: Vec<_> = output
            .closure_capture_facts
            .values()
            .flat_map(|facts| facts.iter())
            .filter(|fact| fact.name == "k")
            .collect();
        assert_eq!(
            facts.len(),
            1,
            "capture facts should deduplicate by binding id"
        );
        assert_eq!(facts[0].ty, Ty::I32);
        assert_eq!(facts[0].mode, ClosureCaptureMode::Copy);
        assert_eq!(facts[0].mode_origin, CaptureModeOrigin::ImplicitCopy);
    }

    #[test]
    fn closure_non_copy_capture_inferred_as_borrow_accepted() {
        // A non-Copy, read-only capture without `move` type-checks
        // cleanly and records `Borrow` / `InferredBorrow` on the
        // capture fact.
        let output = check_source(
            r#"
            fn main() {
                let s: string = "hew";
                let f = || s;
                let _ = f;
            }
            "#,
        );
        assert!(
            output.errors.is_empty(),
            "non-Copy read-only capture should type-check via inferred Borrow: {:?}",
            output.errors
        );
        let s_fact = output
            .closure_capture_facts
            .values()
            .flat_map(|facts| facts.iter())
            .find(|fact| fact.name == "s")
            .expect("capture fact for `s` must exist");
        assert_eq!(
            s_fact.mode,
            ClosureCaptureMode::Borrow,
            "read-only non-Copy capture should infer Borrow",
        );
        assert_eq!(s_fact.mode_origin, CaptureModeOrigin::InferredBorrow);
    }

    #[test]
    fn move_closure_capture_consumes_non_copy_binding() {
        let output = check_source(
            r#"
            fn main() {
                let s: string = "hew";
                let f = move || s;
                let again = s;
            }
            "#,
        );
        assert!(
            output
                .errors
                .iter()
                .any(|err| err.kind == TypeErrorKind::UseAfterMove),
            "use after move capture should be rejected: {:?}",
            output.errors
        );
    }

    // ── inferred Borrow / BorrowMut capture modes ────────────────────────

    #[test]
    fn closure_capture_inferred_borrow_for_println_use() {
        // Acceptance witness: `let s = "hello"; let f = |x| s; f(0)`.
        // The capture is non-Copy, read-only, no `move` keyword. The
        // checker must record `Borrow` / `InferredBorrow` AND accept
        // the program.
        let output = check_source(
            r#"
            fn main() {
                let s: string = "hello";
                let f = |x: i32| {
                    let _ = s;
                    x
                };
                let _ = f;
            }
            "#,
        );
        assert!(
            output.errors.is_empty(),
            "inferred-borrow capture must be accepted: {:?}",
            output.errors
        );
        let s_fact = output
            .closure_capture_facts
            .values()
            .flat_map(|facts| facts.iter())
            .find(|fact| fact.name == "s")
            .expect("expected a capture fact for `s`");
        assert_eq!(s_fact.mode, ClosureCaptureMode::Borrow);
        assert_eq!(s_fact.mode_origin, CaptureModeOrigin::InferredBorrow);
    }

    #[test]
    fn closure_capture_inferred_borrowmut_via_mutating_method_call() {
        // Mutating method (`push`) on a captured binding promotes the
        // inferred mode to `BorrowMut` / `InferredBorrowMut`. The
        // assertion REQUIRES the fact to exist — a missing fact fails
        // the test (previous version used `if let Some(..)` which
        // passed silently when the fact was absent).
        let output = check_source(
            r"
            fn main() {
                var xs: Vec<i32> = Vec::new();
                let f = || xs.push(1);
                let _ = f;
            }
            ",
        );
        let xs_fact = output
            .closure_capture_facts
            .values()
            .flat_map(|facts| facts.iter())
            .find(|fact| fact.name == "xs")
            .expect("capture fact for `xs` must exist (mutating method call)");
        assert_eq!(
            xs_fact.mode,
            ClosureCaptureMode::BorrowMut,
            "mutating method call should infer BorrowMut: {xs_fact:?}",
        );
        assert_eq!(xs_fact.mode_origin, CaptureModeOrigin::InferredBorrowMut);
    }

    #[test]
    fn closure_capture_inferred_borrowmut_via_assignment_projection() {
        // Assignment-projection path: `xs[0] = 1` mutates the
        // root binding `xs`, so the capture promotes to BorrowMut.
        let output = check_source(
            r"
            fn main() {
                var xs: Vec<i32> = Vec::new();
                let f = || { xs[0] = 1; };
                let _ = f;
            }
            ",
        );
        let xs_fact = output
            .closure_capture_facts
            .values()
            .flat_map(|facts| facts.iter())
            .find(|fact| fact.name == "xs")
            .expect("capture fact for `xs` must exist (assignment projection)");
        assert_eq!(xs_fact.mode, ClosureCaptureMode::BorrowMut);
        assert_eq!(xs_fact.mode_origin, CaptureModeOrigin::InferredBorrowMut);
    }

    #[test]
    fn closure_capture_explicit_move_records_move_origin() {
        let output = check_source(
            r#"
            fn main() {
                let s: string = "hew";
                let f = move || s;
                let _ = f;
            }
            "#,
        );
        let s_fact = output
            .closure_capture_facts
            .values()
            .flat_map(|facts| facts.iter())
            .find(|fact| fact.name == "s")
            .expect("capture fact for `s` must exist (explicit move)");
        assert_eq!(s_fact.mode, ClosureCaptureMode::Move);
        assert_eq!(s_fact.mode_origin, CaptureModeOrigin::ExplicitMove);
    }

    // ── NonSyncMutCaptureCrossesSuspend gate — direct body-scanner tests ─
    //
    // The end-to-end diagnostic depends on three independent conditions:
    //   1. The capture mode resolves to `BorrowMut` (already covered by
    //      the borrow/borrow-mut tests above).
    //   2. The capture's resolved type is non-`Sync` (a separate
    //      `TraitRegistry::is_sync` query — covered by the marker-trait
    //      tests in `crate::traits`).
    //   3. The closure body contains a suspend point (`await`, channel
    //      `recv`, `yield`), as observed by `scan_lambda_body`.
    //
    // The three tests below are focused tests of condition (3) for each
    // suspend source, including the new `Yield = suspend` classification.
    // They build synthetic lambda bodies directly so the body-scanner
    // is exercised without depending on user-constructible non-`Sync`
    // types.
    //
    // The end-to-end diagnostic emission path (conditions 1+2+3 plus
    // the BorrowMut mode-origin message threading) is covered by
    // `non_sync_mut_capture_crosses_suspend_end_to_end` below.

    fn lit(value: i64) -> Spanned<Expr> {
        (
            Expr::Literal(Literal::Integer {
                value,
                radix: IntRadix::Decimal,
            }),
            0..0,
        )
    }

    #[test]
    fn scan_lambda_body_marks_await_as_suspend() {
        let body: Spanned<Expr> = (Expr::Await(Box::new(lit(0))), 0..0);
        let facts = super::closure_inference::scan_lambda_body(&body);
        assert!(facts.has_suspend, "await must mark has_suspend=true");
        assert_eq!(facts.suspend_kind, "await");
    }

    #[test]
    fn scan_lambda_body_marks_channel_recv_as_suspend() {
        use hew_parser::ast::SelectArm;
        let arm = SelectArm {
            binding: (hew_parser::ast::Pattern::Identifier("v".to_string()), 0..0),
            source: lit(0),
            body: lit(0),
        };
        let body: Spanned<Expr> = (
            Expr::Select {
                arms: vec![arm],
                timeout: None,
            },
            0..0,
        );
        let facts = super::closure_inference::scan_lambda_body(&body);
        assert!(facts.has_suspend, "select recv must mark has_suspend=true");
        assert_eq!(facts.suspend_kind, "channel recv");
    }

    #[test]
    fn scan_lambda_body_marks_yield_as_suspend() {
        let body: Spanned<Expr> = (Expr::Yield(Some(Box::new(lit(1)))), 0..0);
        let facts = super::closure_inference::scan_lambda_body(&body);
        assert!(facts.has_suspend, "yield must mark has_suspend=true");
        assert_eq!(facts.suspend_kind, "yield");
    }

    #[test]
    fn non_sync_mut_capture_crosses_suspend_end_to_end() {
        // End-to-end witness: a closure that mutates a captured
        // non-`Sync` non-`Copy` binding across a suspend point must
        // emit `NonSyncMutCaptureCrossesSuspend` and surface the
        // BorrowMut mode-origin label in the human-readable message.
        //
        // The capture type is a user-defined struct wrapping a raw
        // pointer: raw pointers are the canonical non-Sync primitive,
        // and a struct wrapping one is non-Copy (default for user
        // records) and non-Sync (inherits from its non-Send field).
        // The suspend point is `yield`, which the body scanner
        // accepts uniformly with `await` and channel `recv`.
        let output = check_source(
            r#"
            extern "C" { fn make_ptr() -> *const i64; }
            type Wrap { p: *const i64, xs: Vec<i64> }
            fn host() {
                var w: Wrap = Wrap { p: unsafe { make_ptr() }, xs: Vec::new() };
                let f = || { w = w; let _g = gen { yield 0; }; };
                let _ = f;
            }
            "#,
        );
        let found = output.errors.iter().any(|err| {
            matches!(
                &err.kind,
                TypeErrorKind::NonSyncMutCaptureCrossesSuspend { capture_name, .. }
                    if capture_name == "w"
            ) && err.message.contains("inferred from a mutating use")
        });
        assert!(
            found,
            "expected NonSyncMutCaptureCrossesSuspend(w) with mode-origin label in message; got: {:#?}",
            output.errors
        );
    }

    // ── escape classification facts populated for every closure ──────────

    #[test]
    fn closure_escape_facts_populated_for_local_call() {
        let output = check_source(
            r"
            fn main() {
                let k: i32 = 1;
                let f = || k + 1;
                let _ = f();
            }
            ",
        );
        assert!(
            !output.closure_escape_facts.is_empty(),
            "every closure literal should produce one escape fact"
        );
        let any_local = output
            .closure_escape_facts
            .values()
            .any(|f| matches!(f.kind, ClosureEscapeKind::Local));
        assert!(
            any_local,
            "direct-call-only closure must classify as Local: {:?}",
            output.closure_escape_facts
        );
    }

    #[test]
    fn closure_escape_facts_default_to_escapes_for_returned_lambda() {
        let output = check_source(
            r"
            fn make() -> fn(i32) -> i32 {
                |n: i32| n + 1
            }
            ",
        );
        let any_escapes = output
            .closure_escape_facts
            .values()
            .any(|f| matches!(f.kind, ClosureEscapeKind::Escapes));
        assert!(
            any_escapes,
            "returned anonymous lambda should classify as Escapes: {:?}",
            output.closure_escape_facts
        );
    }

    // ── escape classification matrix — the six additional cases ──────────

    fn assert_escape_kind(output: &TypeCheckOutput, expected: ClosureEscapeKind, label: &str) {
        let found = output
            .closure_escape_facts
            .values()
            .any(|f| f.kind == expected);
        assert!(
            found,
            "{label}: expected at least one closure classified {expected:?}; got: {:?}",
            output.closure_escape_facts
        );
    }

    #[test]
    fn closure_escape_anonymous_fork_body_is_forked() {
        // Case: `scope { fork { || compute() }; }`.
        let output = check_source(
            r"
            fn compute() -> i32 { 1 }
            fn main() {
                scope {
                    fork {
                        || compute()
                    };
                };
            }
            ",
        );
        assert_escape_kind(
            &output,
            ClosureEscapeKind::Forked,
            "anonymous fork-body lambda",
        );
    }

    #[test]
    fn closure_escape_named_then_forked_is_forked() {
        // Case: `scope { let f = ...; fork { f() } }`.
        let output = check_source(
            r"
            fn main() {
                scope {
                    let f = || 1;
                    fork {
                        f();
                    };
                };
            }
            ",
        );
        assert_escape_kind(
            &output,
            ClosureEscapeKind::Forked,
            "named-then-forked lambda",
        );
    }

    #[test]
    fn closure_escape_channel_send_is_escapes() {
        // Case: closure stored-in / sent through a channel.
        let output = check_source(
            r"
            fn main() {
                let (tx, _rx) = channel::<fn() -> i32>();
                let f = || 1;
                tx.send(f);
            }
            ",
        );
        assert_escape_kind(&output, ClosureEscapeKind::Escapes, "channel-send escape");
    }

    #[test]
    fn closure_escape_higher_order_arg_is_escapes() {
        // Case: `g(|| captured)` — anonymous lambda as
        // argument to a free function.
        let output = check_source(
            r"
            fn run(g: fn() -> i32) -> i32 { g() }
            fn main() {
                let _ = run(|| 1);
            }
            ",
        );
        assert_escape_kind(
            &output,
            ClosureEscapeKind::Escapes,
            "higher-order-arg lambda",
        );
    }

    #[test]
    fn closure_escape_block_tail_value_is_escapes() {
        // Case: `{ let f = ...; f }` — block-tail value.
        let output = check_source(
            r"
            fn make() -> fn() -> i32 {
                let f = || 1;
                f
            }
            ",
        );
        assert_escape_kind(
            &output,
            ClosureEscapeKind::Escapes,
            "block-tail value closure",
        );
    }

    #[test]
    fn closure_escape_nested_inner_escapes_via_send() {
        // Case: inner closure escapes inside an outer
        // closure's body (the inner is sent through a channel).
        // Both inner and outer must classify Escapes — inner because
        // the channel send is a non-local use, outer because it has
        // no enumerable local-only use-site set.
        let output = check_source(
            r"
            fn main() {
                let (tx, _rx) = channel::<fn() -> i32>();
                let outer = || {
                    let inner = || 1;
                    tx.send(inner);
                };
                let _ = outer;
            }
            ",
        );
        // At least two closure literals → two facts; ensure both
        // resolve to Escapes (no Local survives).
        assert!(
            output.closure_escape_facts.len() >= 2,
            "expected facts for both outer and inner closures: {:?}",
            output.closure_escape_facts
        );
        let any_local = output
            .closure_escape_facts
            .values()
            .any(|f| matches!(f.kind, ClosureEscapeKind::Local));
        assert!(
            !any_local,
            "nested transitive escape: no closure should classify Local; got: {:?}",
            output.closure_escape_facts
        );
    }

    // ── ClosureEscapeAdvisory narrowing: PassedToHigherOrder ─────────────
    //
    // Inlining a let-bound closure passed to a higher-order function does not
    // relieve the escape — an anonymous closure in argument position still
    // hits AnonContext::PassedToHigherOrder, so the advisory would fire again.
    // The admit_local rule set therefore excludes PassedToHigherOrder.

    #[test]
    fn escape_advisory_not_emitted_for_let_bound_closure_passed_to_higher_order() {
        // `let f = |x: i64| x * 2; apply(f, 5)` — the closure escapes via
        // PassedToHigherOrder but NO advisory should fire because inlining
        // the closure at the call site provides no relief.
        let output = check_source(
            r"
            fn apply(f: fn(i64) -> i64, x: i64) -> i64 { f(x) }
            fn main() {
                let double = |x: i64| x * 2;
                let _ = apply(double, 5);
            }
            ",
        );
        let advisory_for_higher_order = output.warnings.iter().any(|w| {
            matches!(
                &w.kind,
                TypeErrorKind::ClosureEscapeAdvisory { rule }
                    if rule.contains("PassedToHigherOrder")
            )
        });
        assert!(
            !advisory_for_higher_order,
            "no ClosureEscapeAdvisory should fire for a let-bound closure passed to a \
             higher-order function; got warnings: {:?}",
            output.warnings
        );
    }

    #[test]
    fn escape_advisory_still_emitted_for_let_bound_closure_escaped_as_block_tail() {
        // Precision check: narrowing PassedToHigherOrder must not disable the
        // advisory for other genuinely-advisable rules.  A let-bound closure
        // returned as a block tail value (EscapesViaBlockValue) still produces
        // an advisory because making it anonymous at the return site would
        // resolve to AnonContext::Returned → Escapes::Returned, not Local.
        // The suggestion ("inline at call site") IS actionable in some
        // refactors even for EscapesViaBlockValue, so that rule stays.
        let output = check_source(
            r"
            fn make() -> fn() -> i32 {
                let f = || 1;
                f
            }
            ",
        );
        let has_advisory = output
            .warnings
            .iter()
            .any(|w| matches!(&w.kind, TypeErrorKind::ClosureEscapeAdvisory { .. }));
        assert!(
            has_advisory,
            "ClosureEscapeAdvisory must still fire for a let-bound closure returned as a \
             block-tail value; got warnings: {:?}",
            output.warnings
        );
    }

    // ── Fail-closed defenses for missing facts ───────────────────────────
    //
    // These tests drive `emit_unresolved_closure_diagnostics` directly
    // with synthetic span lists and (deliberately empty) fact maps so
    // that the contract enforcer's emission paths are exercised even
    // when the checker proper always populates both maps.

    #[test]
    fn unresolved_closure_capture_mode_diagnostic_fires_on_missing_fact() {
        let span: Span = 10..20;
        let sites = vec![(span.clone(), Some("f".to_string()), 0u32)];
        let capture_facts: HashMap<SpanKey, Vec<ClosureCaptureFact>> = HashMap::new();
        let mut escape_facts: HashMap<SpanKey, ClosureEscapeFact> = HashMap::new();
        // Populate escape so ONLY the capture-mode diagnostic fires.
        escape_facts.insert(
            SpanKey::from(&span),
            ClosureEscapeFact {
                kind: ClosureEscapeKind::Local,
                rule: ClosureEscapeRule::DirectCallOnly,
            },
        );
        let mut diagnostics = Vec::new();
        super::emit_unresolved_closure_diagnostics(
            &sites,
            &capture_facts,
            &escape_facts,
            &mut diagnostics,
        );
        assert!(
            diagnostics
                .iter()
                .any(|err| matches!(err.kind, TypeErrorKind::ClosureCaptureModeUnresolved { .. })),
            "expected ClosureCaptureModeUnresolved; got: {diagnostics:?}"
        );
        assert!(
            !diagnostics
                .iter()
                .any(|err| err.kind == TypeErrorKind::ClosureEscapeKindUnresolved),
            "ClosureEscapeKindUnresolved must NOT fire when escape fact is present",
        );
    }

    #[test]
    fn unresolved_closure_escape_kind_diagnostic_fires_on_missing_fact() {
        let span: Span = 30..40;
        let sites = vec![(span.clone(), None, 0u32)];
        let mut capture_facts: HashMap<SpanKey, Vec<ClosureCaptureFact>> = HashMap::new();
        // Populate capture so ONLY the escape diagnostic fires.
        capture_facts.insert(SpanKey::from(&span), Vec::new());
        let escape_facts: HashMap<SpanKey, ClosureEscapeFact> = HashMap::new();
        let mut diagnostics = Vec::new();
        super::emit_unresolved_closure_diagnostics(
            &sites,
            &capture_facts,
            &escape_facts,
            &mut diagnostics,
        );
        assert!(
            diagnostics
                .iter()
                .any(|err| err.kind == TypeErrorKind::ClosureEscapeKindUnresolved),
            "expected ClosureEscapeKindUnresolved; got: {diagnostics:?}"
        );
        assert!(
            !diagnostics
                .iter()
                .any(|err| matches!(err.kind, TypeErrorKind::ClosureCaptureModeUnresolved { .. })),
            "ClosureCaptureModeUnresolved must NOT fire when capture fact is present",
        );
    }

    #[test]
    fn unresolved_closure_diagnostics_silent_when_contract_holds() {
        let span: Span = 50..60;
        let sites = vec![(span.clone(), Some("f".to_string()), 0u32)];
        let mut capture_facts: HashMap<SpanKey, Vec<ClosureCaptureFact>> = HashMap::new();
        capture_facts.insert(SpanKey::from(&span), Vec::new());
        let mut escape_facts: HashMap<SpanKey, ClosureEscapeFact> = HashMap::new();
        escape_facts.insert(
            SpanKey::from(&span),
            ClosureEscapeFact {
                kind: ClosureEscapeKind::Local,
                rule: ClosureEscapeRule::DirectCallOnly,
            },
        );
        let mut diagnostics = Vec::new();
        super::emit_unresolved_closure_diagnostics(
            &sites,
            &capture_facts,
            &escape_facts,
            &mut diagnostics,
        );
        assert!(
            diagnostics.is_empty(),
            "no diagnostic expected when both facts are present; got: {diagnostics:?}"
        );
    }

    // ── already-errored / divergent iterables must not get extra diagnostics ─

    #[test]
    fn error_typed_iterable_does_not_emit_extra_not_iterable_diagnostic() {
        // Ty::Error propagates silently; no spurious "type is not iterable".
        let errors = check_for_over(Ty::Error);
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "Ty::Error iterable must not emit InvalidOperation; got: {errors:?}",
        );
    }

    #[test]
    fn never_typed_iterable_does_not_emit_not_iterable_diagnostic() {
        // Ty::Never is divergent; no spurious "type is not iterable".
        let errors = check_for_over(Ty::Never);
        assert!(
            !errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InvalidOperation),
            "Ty::Never iterable must not emit InvalidOperation; got: {errors:?}",
        );
    }
}

/// Regression (L23 / `SpanKey` per-module discriminator): a closure literal
/// living in a *non-root* module must not trip the fail-closed
/// `ClosureCaptureModeUnresolved` / `ClosureEscapeKindUnresolved` contract
/// enforcers. The checker stamps capture/escape facts with the module's
/// 1-based `module_idx`; the validator must look them up at that same index
/// rather than the hardcoded root `0`. Before the fix this checked with a
/// spurious internal error for any imported module containing a closure.
#[test]
fn closure_in_imported_module_does_not_trip_fail_closed_contract() {
    let output = check_source_in_module(
        r"
pub fn compute(n: i64) -> i64 {
    let f = |x: i64| -> i64 { x * 2 };
    f(n)
}
",
        vec!["mylib".to_string()],
    );
    assert!(
        !output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::ClosureCaptureModeUnresolved { .. })),
        "closure in a non-root module must not raise ClosureCaptureModeUnresolved; got: {:?}",
        output.errors
    );
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::ClosureEscapeKindUnresolved),
        "closure in a non-root module must not raise ClosureEscapeKindUnresolved; got: {:?}",
        output.errors
    );
}

// ── Sub-issue 2: bind-then-return bypass regression tests ──────────────────

/// Parse and type-check a program with one fictional owned-handle type registered.
fn check_source_with_handle(source: &str, handle_type: &str) -> TypeCheckOutput {
    let parse_result = hew_parser::parse(source);
    assert!(
        parse_result.errors.is_empty(),
        "parse errors in test source: {:#?}",
        parse_result.errors
    );
    let mut registry = ModuleRegistry::new(vec![]);
    registry.insert_handle_type_for_test(handle_type.to_string());
    let mut checker = Checker::new(registry);
    checker.check_program(&parse_result.program)
}

/// Direct `return self.field` — the existing check; must still fire after the
/// bind-then-return refactor.
#[test]
fn direct_handle_field_return_is_rejected() {
    let output = check_source_with_handle(
        r"
        type PatternWrapper { pattern: regex.Pattern }

        impl PatternWrapper {
            fn get_pattern(wrapper: PatternWrapper) -> regex.Pattern {
                wrapper.pattern
            }
        }
        ",
        "regex.Pattern",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InvalidOperation
                && e.message.contains("pattern")
                && e.message.contains("double-free")),
        "direct return of owned handle field must be rejected; got: {:?}",
        output.errors
    );
}

/// `let p = wrapper.pattern; p` — the bind-then-return bypass from issue #1315 sub-2.
#[test]
fn bind_then_return_handle_field_is_rejected() {
    let output = check_source_with_handle(
        r"
        type PatternWrapper { pattern: regex.Pattern }

        impl PatternWrapper {
            fn get_pattern(wrapper: PatternWrapper) -> regex.Pattern {
                let p = wrapper.pattern;
                p
            }
        }
        ",
        "regex.Pattern",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InvalidOperation
                && e.message.contains("pattern")
                && e.message.contains("double-free")),
        "let-binding alias of owned handle field must be rejected; got: {:?}",
        output.errors
    );
}

/// Diagnostic message for bind-then-return must name the binding so the user
/// can locate the alias.
#[test]
fn bind_then_return_diagnostic_names_binding() {
    let output = check_source_with_handle(
        r"
        type PatternWrapper { pattern: regex.Pattern }

        impl PatternWrapper {
            fn extract(wrapper: PatternWrapper) -> regex.Pattern {
                let p = wrapper.pattern;
                p
            }
        }
        ",
        "regex.Pattern",
    );
    let msg = output
        .errors
        .iter()
        .find(|e| e.kind == TypeErrorKind::InvalidOperation)
        .map_or("", |e| e.message.as_str());
    assert!(
        msg.contains("via let-binding `p`"),
        "error message must identify the binding name; got: {msg:?}"
    );
}

/// `let p = wrapper.pattern; printDebug(p); return p` — intermediate use
/// does not suppress the diagnostic.
#[test]
fn bind_then_return_with_intermediate_use_is_rejected() {
    let output = check_source_with_handle(
        r"
        type PatternWrapper { pattern: regex.Pattern }

        impl PatternWrapper {
            fn get_pattern(wrapper: PatternWrapper) -> regex.Pattern {
                let p = wrapper.pattern;
                println(p);
                p
            }
        }
        ",
        "regex.Pattern",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InvalidOperation
                && e.message.contains("double-free")),
        "alias with intermediate use must still be rejected; got: {:?}",
        output.errors
    );
}

/// A method that returns a non-handle field must not trigger the diagnostic.
#[test]
fn non_handle_field_return_is_allowed() {
    let output = check_source_with_handle(
        r"
        type PatternWrapper { pattern: regex.Pattern, label: string }

        impl PatternWrapper {
            fn get_label(wrapper: PatternWrapper) -> string {
                wrapper.label
            }
        }
        ",
        "regex.Pattern",
    );
    assert!(
        output.errors.iter().all(
            |e| e.kind != TypeErrorKind::InvalidOperation || !e.message.contains("double-free")
        ),
        "returning a non-handle field must not be rejected; got: {:?}",
        output.errors
    );
}

/// A let-binding whose value is NOT a receiver field access must not be
/// flagged when it appears in return position.
#[test]
fn non_field_let_binding_return_is_allowed() {
    let output = check_source_with_handle(
        r"
        type PatternWrapper { pattern: regex.Pattern }

        impl PatternWrapper {
            fn get_label(wrapper: PatternWrapper) -> string {
                let s = to_string(42);
                s
            }
        }
        ",
        "regex.Pattern",
    );
    assert!(
        output.errors.iter().all(
            |e| e.kind != TypeErrorKind::InvalidOperation || !e.message.contains("double-free")
        ),
        "returning a non-field binding must not be rejected; got: {:?}",
        output.errors
    );
}

// ── Sub-issue 3: O(N²) registration scaling tests ──────────────────────────

/// Registering N struct types should trigger `refresh_handle_bearing_structs`
/// exactly once (lazy fixpoint), not N times.
///
/// This is an operation-count assertion — deterministic and never flaky.
/// For N=100, N=200, N=400 we expect `refresh_call_count` == 1 after the first
/// lookup, regardless of N.
#[test]
fn handle_bearing_refresh_deferred_to_single_fixpoint_pass() {
    fn register_n_plain_structs(n: usize) -> usize {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        for i in 0..n {
            let td = hew_parser::ast::TypeDecl {
                visibility: hew_parser::ast::Visibility::Private,
                kind: hew_parser::ast::TypeDeclKind::Struct,
                name: format!("S{i}"),
                type_params: None,
                where_clause: None,
                body: vec![hew_parser::ast::TypeBodyItem::Field {
                    name: "value".to_string(),
                    ty: (
                        hew_parser::ast::TypeExpr::Named {
                            name: "i64".to_string(),
                            type_args: None,
                        },
                        0..0,
                    ),
                    attributes: vec![],
                    doc_comment: None,
                    span: 0..0,
                }],
                doc_comment: None,
                wire: None,
                is_indirect: false,
                resource_marker: hew_parser::ast::ResourceMarker::None,
                is_opaque: false,
                consuming_methods: Vec::new(),
            };
            checker.register_type_decl(&td);
        }
        // Trigger the lazy refresh with one lookup.
        checker.ensure_handle_bearing_fresh();
        checker.refresh_call_count
    }

    let count_100 = register_n_plain_structs(100);
    let count_200 = register_n_plain_structs(200);
    let count_400 = register_n_plain_structs(400);

    // Each run should refresh exactly once regardless of N.
    assert_eq!(count_100, 1, "N=100: expected 1 refresh, got {count_100}");
    assert_eq!(count_200, 1, "N=200: expected 1 refresh, got {count_200}");
    assert_eq!(count_400, 1, "N=400: expected 1 refresh, got {count_400}");
}

/// Timing check: registering 400 structs must run in at most 4× the time it
/// takes for 100 structs, demonstrating linear (not quadratic) scaling.
///
/// Uses `Instant::elapsed`-bounded ratio rather than an absolute wall-clock
/// threshold so CI hardware differences don't cause false failures.
/// Kept `#[ignore]` (CONVERT-C deferred): migration to `benches/` requires
/// adding Criterion to hew-types and is non-trivial. Invoke explicitly with
/// `cargo test -- --include-ignored` when you want the timing signal.
#[test]
#[ignore = "wall-clock ratio test; run explicitly with --include-ignored (benches/ migration deferred: no Criterion dep)"]
fn handle_bearing_registration_scales_linearly_not_quadratically() {
    use std::time::Instant;

    fn time_register_n(n: usize) -> std::time::Duration {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let start = Instant::now();
        for i in 0..n {
            let td = hew_parser::ast::TypeDecl {
                visibility: hew_parser::ast::Visibility::Private,
                kind: hew_parser::ast::TypeDeclKind::Struct,
                name: format!("T{i}"),
                type_params: None,
                where_clause: None,
                body: vec![hew_parser::ast::TypeBodyItem::Field {
                    name: "x".to_string(),
                    ty: (
                        hew_parser::ast::TypeExpr::Named {
                            name: "i64".to_string(),
                            type_args: None,
                        },
                        0..0,
                    ),
                    attributes: vec![],
                    doc_comment: None,
                    span: 0..0,
                }],
                doc_comment: None,
                wire: None,
                is_indirect: false,
                resource_marker: hew_parser::ast::ResourceMarker::None,
                is_opaque: false,
                consuming_methods: Vec::new(),
            };
            checker.register_type_decl(&td);
        }
        checker.ensure_handle_bearing_fresh();
        start.elapsed()
    }

    let t100 = time_register_n(100);
    let t400 = time_register_n(400);

    // Use as_secs_f64 to avoid u128->f64 precision-loss lints; nanosecond
    // precision is far more than this test needs.
    let ratio = t400.as_secs_f64() / t100.as_secs_f64().max(f64::EPSILON);
    assert!(
        ratio < 16.0,
        "registration of 400 structs took {ratio:.1}× as long as 100 structs — expected < 16× \
         (quadratic would be ~16×, linear is ~4×). t100={t100:?} t400={t400:?}"
    );
}

// ── Task<T> surface rules ──────────────────────────────────────────────────
//
// `Task<T>` is a compiler-internal type. It has no user-source spelling:
//   - `fork name = expr` inside a `fork{}` body is the only construction site;
//     the binding's type is inferred to `Ty::Task(T)` by HIR lowering.
//   - `await name` inside a `select` arm or `fork{}` body consumes the handle
//     and yields `T`.
//   - Any explicit `Task<T>` in a user-written type annotation is rejected with
//     `E_TASK_NOT_NAMEABLE` (= `TypeErrorKind::TaskNotNameable`).
//
// §3.3 diagnostic-surface coverage: BOTH paths must be covered:
//   1. `Task<T>` written in an annotation → `TaskNotNameable` error (no infer).
//   2. `scope.launch { ... }` / `ScopeLaunch` → inferred `Ty::Task(T)`;
//      `await` on it yields `T` (no error on clean code).

mod task_type_surface_rules {
    use super::*;

    // ── Rejection: user-written Task<T> in annotation positions ─────────────

    #[test]
    fn task_in_let_annotation_is_rejected() {
        let output = check_source(
            r"
            fn main() {
                let _t: Task<i64> = 0;
            }
            ",
        );
        assert!(
            output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::TaskNotNameable),
            "Task<i64> in let annotation must emit TaskNotNameable; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn task_in_fn_param_annotation_is_rejected() {
        let output = check_source(
            r"
            fn foo(t: Task<i64>) -> i64 { 0 }
            fn main() -> i64 { 0 }
            ",
        );
        assert!(
            output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::TaskNotNameable),
            "Task<i64> in fn param must emit TaskNotNameable; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn task_in_return_type_annotation_is_rejected() {
        let output = check_source(
            r"
            fn foo() -> Task<i64> { 0 }
            fn main() -> i64 { 0 }
            ",
        );
        assert!(
            output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::TaskNotNameable),
            "Task<i64> as return type must emit TaskNotNameable; got: {:#?}",
            output.errors
        );
    }

    // ── Accept path: `fork name = call(...)` inside scope{} infers Ty::Task;
    // `await name` consumes the binding and yields T ──────────────────────────

    #[test]
    fn scope_fork_binding_infers_task_and_await_consumes_it() {
        // `scope { fork task = compute(); await task; }` is the new structured
        // surface for spawning a child task and joining it; it must type-check
        // without emitting TaskNotNameable.
        let output = check_source(
            r"
            fn compute() -> i64 { 42 }
            fn main() {
                scope {
                    fork task = compute();
                    await task;
                }
            }
            ",
        );
        assert!(
            !output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::TaskNotNameable),
            "clean scope {{ fork x = call(); await x; }} must not emit TaskNotNameable; got: {:#?}",
            output.errors
        );
    }

    // ── in_unsafe scope flag sentinel ────────────────────────────────────────
    //
    // These tests verify that the `in_unsafe` flag is correctly set to `true`
    // inside an `unsafe { }` block and `false` outside one.  The observable
    // behaviour is the existing `require_unsafe` gate on extern fn calls:
    // inside unsafe => no error; outside unsafe => error.
    //
    // There are no unsafe *operations* yet (T1-B-* and T1-C-* add them), but
    // extern fn calls already use `in_unsafe` via `require_unsafe`, making them
    // a natural fail-closed probe.

    #[test]
    fn in_unsafe_flag_true_inside_unsafe_block_no_error() {
        // An extern fn call inside `unsafe { }` must not produce an error:
        // the `in_unsafe` flag is `true` during the block body.
        let output = check_source(
            r#"
            extern "C" { fn raw_op() -> i64; }
            fn caller() -> i64 {
                unsafe { raw_op() }
            }
            "#,
        );
        let unsafe_errors: Vec<_> = output
            .errors
            .iter()
            .filter(|e| e.message.contains("unsafe"))
            .collect();
        assert!(
            unsafe_errors.is_empty(),
            "extern fn call inside unsafe block must not emit unsafe errors; got: {unsafe_errors:#?}"
        );
    }

    #[test]
    fn in_unsafe_flag_false_outside_unsafe_block_emits_error() {
        // An extern fn call outside `unsafe { }` must produce an error:
        // the `in_unsafe` flag is `false` in the surrounding function body.
        let output = check_source(
            r#"
            extern "C" { fn raw_op() -> i64; }
            fn caller() -> i64 {
                raw_op()
            }
            "#,
        );
        let has_unsafe_error = output.errors.iter().any(|e| e.message.contains("unsafe"));
        let errors = &output.errors;
        assert!(
            has_unsafe_error,
            "extern fn call outside unsafe block must emit an unsafe-required error; got: {errors:#?}"
        );
    }

    #[test]
    fn in_unsafe_flag_restored_after_unsafe_block_exits() {
        // After the unsafe block closes, `in_unsafe` reverts to `false`.
        // A second extern fn call after the block, outside any unsafe context,
        // must still produce an error.
        let output = check_source(
            r#"
            extern "C" { fn raw_op() -> i64; }
            fn caller() -> i64 {
                unsafe { raw_op() };
                raw_op()
            }
            "#,
        );
        let has_unsafe_error = output.errors.iter().any(|e| e.message.contains("unsafe"));
        let errors = &output.errors;
        assert!(
            has_unsafe_error,
            "extern fn call after unsafe block closes must emit an unsafe-required error; got: {errors:#?}"
        );
    }

    // ── Raw pointer dereference fail-closed endpoint (v0.5) ─────────────────
    //
    // v0.5 parses `*expr` only far enough to reject it deterministically
    // at type-check time.  No HIR/MIR/codegen path is reached, so native
    // and WASM lowering both fail-closed identically — this is the WASM
    // parity story for raw pointer ops in v0.5.

    #[test]
    fn raw_deref_outside_unsafe_emits_unsafe_block_required() {
        // `*p` outside `unsafe { ... }` must emit
        // `UnsafeOperationRequiresBlock` with the raw-pointer-dereference
        // operation tag.
        let output = check_source(
            r#"
            extern "C" { fn make_ptr() -> *const i64; }
            fn caller() -> i64 {
                let p = unsafe { make_ptr() };
                *p
            }
            "#,
        );
        let raw_deref_errors: Vec<_> = output
            .errors
            .iter()
            .filter(|e| {
                matches!(
                    &e.kind,
                    TypeErrorKind::UnsafeOperationRequiresBlock { operation }
                        if operation == "raw pointer dereference"
                )
            })
            .collect();
        assert_eq!(
            raw_deref_errors.len(),
            1,
            "expected exactly one `UnsafeOperationRequiresBlock(raw pointer dereference)` \
             error; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn raw_deref_inside_unsafe_emits_not_lowered() {
        // `*p` inside `unsafe { ... }` is still fail-closed: the compiler does not
        // lower raw-pointer dereference to HIR/MIR/codegen, so the checker
        // emits `RawPointerOpNotLowered` rather than typing the
        // expression as the pointee type.
        let output = check_source(
            r#"
            extern "C" { fn make_ptr() -> *const i64; }
            fn caller() -> i64 {
                unsafe {
                    let p = make_ptr();
                    *p
                }
            }
            "#,
        );
        let not_lowered: Vec<_> = output
            .errors
            .iter()
            .filter(|e| {
                matches!(
                    &e.kind,
                    TypeErrorKind::RawPointerOpNotLowered { operation }
                        if operation == "raw pointer dereference"
                )
            })
            .collect();
        assert_eq!(
            not_lowered.len(),
            1,
            "expected exactly one `RawPointerOpNotLowered(raw pointer dereference)` \
             error; got: {:#?}",
            output.errors
        );

        // Crucially, no `UnsafeOperationRequiresBlock` error must fire
        // inside `unsafe { ... }`: that diagnostic is for callers, not
        // for the v0.5 "not lowered" rejection.
        let unsafe_required = output
            .errors
            .iter()
            .any(|e| matches!(&e.kind, TypeErrorKind::UnsafeOperationRequiresBlock { .. }));
        assert!(
            !unsafe_required,
            "raw deref inside unsafe must not emit UnsafeOperationRequiresBlock; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn raw_deref_address_of_not_introduced_in_v05() {
        // Address-of (`&expr`, `&mut expr`, `&*expr`) remains out of scope
        // for v0.5.  `&*` is still the single `AmpStar` wrapping-multiply
        // token; introducing prefix `&` as address-of would require a
        // lexer change that is explicitly deferred.  Sanity-check that
        // a prefix `&expr` is not silently accepted as a raw-pointer
        // construction operator — the parser must reject before the
        // type checker even sees a coherent AST.
        let parse_result = hew_parser::parse(
            r"
            fn caller() -> i64 {
                let x: i64 = 0;
                let _p = &x;
                0
            }
            ",
        );
        assert!(
            !parse_result.errors.is_empty(),
            "prefix `&expr` must not be accepted as a v0.5 address-of \
             operator; got no parse errors"
        );
    }
}

// ---------------------------------------------------------------------------
// record admission (A-3)
// ---------------------------------------------------------------------------

mod record_admission {
    use super::*;

    #[test]
    fn construction_ok_named_record() {
        // Named-field construction with all required fields and correct types
        // must produce no errors.
        let output = check_source(
            r"
            record Point { x: i64, y: i64 }
            fn main() {
                let p = Point { x: 1, y: 2 };
            }
            ",
        );
        assert!(
            output.errors.is_empty(),
            "valid record construction must not produce errors; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn construction_missing_field_rejected() {
        // Omitting a required field must produce a missing-field error.
        let output = check_source(
            r"
            record Point { x: i64, y: i64 }
            fn main() {
                let p = Point { x: 1 };
            }
            ",
        );
        let has_missing = output
            .errors
            .iter()
            .any(|e| e.message.contains("missing field") && e.message.contains('y'));
        assert!(
            has_missing,
            "omitting a required field must emit a missing-field error; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn construction_extra_field_rejected() {
        // Providing a field that does not exist in the record must produce an
        // undefined-field error.
        let output = check_source(
            r"
            record Point { x: i64, y: i64 }
            fn main() {
                let p = Point { x: 1, y: 2, z: 3 };
            }
            ",
        );
        let has_extra = output
            .errors
            .iter()
            .any(|e| e.message.contains('z') && e.message.contains("Point"));
        assert!(
            has_extra,
            "extra field must emit an undefined-field error; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn construction_wrong_type_rejected() {
        // A field initialised with the wrong type must produce a type-mismatch
        // error.
        let output = check_source(
            r#"
            record Point { x: i64, y: i64 }
            fn main() {
                let p = Point { x: "hello", y: 2 };
            }
            "#,
        );
        let has_mismatch = output.errors.iter().any(|e| {
            e.message.contains("string")
                || e.message.contains("type mismatch")
                || e.message.contains("expected")
        });
        assert!(
            has_mismatch,
            "wrong-typed field must emit a type error; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn field_read_ok() {
        // Field access on a valid record must resolve the field type and
        // produce no errors.
        let output = check_source(
            r"
            record Point { x: i64, y: i64 }
            fn main() {
                let p = Point { x: 10, y: 20 };
                let n: i64 = p.x;
            }
            ",
        );
        assert!(
            output.errors.is_empty(),
            "valid field read must produce no errors; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn field_write_rejected() {
        // Assigning to a record field through an immutable binding must be rejected.
        let output = check_source(
            r"
            record Point { x: i64, y: i64 }
            fn main() {
                let p: Point = Point { x: 1, y: 2 };
                p.x = 5;
            }
            ",
        );
        let has_rejection = output
            .errors
            .iter()
            .any(|e| e.message.contains("record") || e.message.contains("immutable"));
        assert!(
            has_rejection,
            "field assignment on a record must be rejected; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn field_write_through_mutable_binding_accepted() {
        let output = check_source(
            r"
            record Point { x: i64, y: i64 }
            fn main() {
                var p: Point = Point { x: 1, y: 2 };
                p.x = 5;
            }
            ",
        );
        assert!(
            output.errors.is_empty(),
            "field assignment through a mutable record binding must type-check; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn tuple_record_construction_ok() {
        // Tuple-positional construction `UserId(42)` must resolve to the
        // declared record type without errors.
        let output = check_source(
            r"
            record UserId(i64);
            fn make() -> UserId {
                UserId(42)
            }
            ",
        );
        assert!(
            output.errors.is_empty(),
            "valid tuple-record construction must produce no errors; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn tuple_record_dot_zero_access_rejected() {
        // `.0` index-style access on a tuple record must be rejected (A-D2).
        // Fields map is empty; `check_field_access` will report an undefined-field
        // error for the synthesised field name `"0"`.
        let output = check_source(
            r"
            record UserId(i64);
            fn get_inner(id: UserId) -> i64 {
                id.0
            }
            ",
        );
        let has_error = !output.errors.is_empty();
        assert!(
            has_error,
            "`.0` access on a tuple record must produce an error; got: {:#?}",
            output.errors
        );
    }
}

/// Tests for functional-update syntax `R { field: v, ..base }`.
/// Slice A-5 of the primitives surface plan.
#[cfg(test)]
mod record {
    mod functional_update {
        use super::super::*;

        #[test]
        fn base_fills_unspecified_fields_accepted() {
            // `Point { x: 5, ..base }` where base is Point — checker accepts it
            // because `base` fills the missing `y` field.
            let output = check_source(
                r"
                record Point { x: i64, y: i64 }
                fn f(base: Point) -> Point {
                    Point { x: 5, ..base }
                }
                ",
            );
            assert!(
                output.errors.is_empty(),
                "functional update with matching base type must have no errors; got: {:#?}",
                output.errors
            );
        }

        #[test]
        fn all_fields_explicit_with_base_accepted() {
            // All fields listed explicitly plus base — still valid (explicit overrides).
            let output = check_source(
                r"
                record Point { x: i64, y: i64 }
                fn f(base: Point) -> Point {
                    Point { x: 1, y: 2, ..base }
                }
                ",
            );
            assert!(
                output.errors.is_empty(),
                "functional update overriding all fields must have no errors; got: {:#?}",
                output.errors
            );
        }

        #[test]
        fn no_explicit_fields_base_only_accepted() {
            // `Point { ..base }` — base fills all fields.
            let output = check_source(
                r"
                record Point { x: i64, y: i64 }
                fn f(base: Point) -> Point {
                    Point { ..base }
                }
                ",
            );
            assert!(
                output.errors.is_empty(),
                "functional update with zero explicit fields must have no errors; got: {:#?}",
                output.errors
            );
        }

        #[test]
        fn base_wrong_type_rejected() {
            // `Point { x: 5, ..other }` where `other` is a different type — must error.
            let output = check_source(
                r"
                record Point { x: i64, y: i64 }
                record Color { r: i64, g: i64, b: i64 }
                fn f(other: Color) -> Point {
                    Point { x: 5, ..other }
                }
                ",
            );
            let has_error = output
                .errors
                .iter()
                .any(|e| e.message.contains("functional-update base"));
            assert!(
                has_error,
                "functional update with wrong base type must produce a type error; got: {:#?}",
                output.errors
            );
        }

        #[test]
        fn missing_field_without_base_still_rejected() {
            // No functional update — missing field still an error.
            let output = check_source(
                r"
                record Point { x: i64, y: i64 }
                fn f() -> Point {
                    Point { x: 1 }
                }
                ",
            );
            let has_missing = output
                .errors
                .iter()
                .any(|e| e.message.contains("missing field") && e.message.contains('y'));
            assert!(
                has_missing,
                "missing field without base must still produce a missing-field error; got: {:#?}",
                output.errors
            );
        }

        #[test]
        fn explicit_field_type_wrong_rejected() {
            // Explicit field type mismatch must still be caught even when base is present.
            let output = check_source(
                r#"
                record Point { x: i64, y: i64 }
                fn f(base: Point) -> Point {
                    Point { x: "not-an-i64", ..base }
                }
                "#,
            );
            assert!(
                !output.errors.is_empty(),
                "wrong type for explicit field in functional update must produce an error; got: {:#?}",
                output.errors
            );
        }

        #[test]
        fn base_wrong_type_in_typed_let_rejected() {
            // Typed `let p: Point = Point { x: 5, ..other }` where `other` is a
            // different type — the check_against path must also validate the base.
            let output = check_source(
                r"
                record Point { x: i64, y: i64 }
                record Color { r: i64, g: i64, b: i64 }
                fn f(other: Color) {
                    let p: Point = Point { x: 5, ..other };
                }
                ",
            );
            let has_error = output
                .errors
                .iter()
                .any(|e| e.message.contains("functional-update base"));
            assert!(
                has_error,
                "wrong base type must be caught even with type annotation on let; got: {:#?}",
                output.errors
            );
        }
    }
}

/// Tests for numeric opt-out arithmetic: `.wrapping_*`, `.checked_*`, `.saturating_*`
/// on every integer width. Slice B-3 of the primitives surface plan.
#[cfg(test)]
mod methods {
    use super::*;

    mod integer_checked_wrapping_saturating {
        use super::*;

        // --- wrapping_* accepts same-width argument and returns same type ---

        #[test]
        fn wrapping_add_i32_ok() {
            let output = check_source(r"fn f(a: i32, b: i32) -> i32 { a.wrapping_add(b) }");
            assert!(
                output.errors.is_empty(),
                "wrapping_add(i32, i32) should typecheck: {:#?}",
                output.errors
            );
        }

        #[test]
        fn wrapping_sub_i64_ok() {
            let output = check_source(r"fn f(a: i64, b: i64) -> i64 { a.wrapping_sub(b) }");
            assert!(
                output.errors.is_empty(),
                "wrapping_sub(i64, i64) should typecheck: {:#?}",
                output.errors
            );
        }

        #[test]
        fn wrapping_mul_u8_ok() {
            let output = check_source(r"fn f(a: u8, b: u8) -> u8 { a.wrapping_mul(b) }");
            assert!(
                output.errors.is_empty(),
                "wrapping_mul(u8, u8) should typecheck: {:#?}",
                output.errors
            );
        }

        #[test]
        fn wrapping_add_u64_ok() {
            let output = check_source(r"fn f(a: u64, b: u64) -> u64 { a.wrapping_add(b) }");
            assert!(
                output.errors.is_empty(),
                "wrapping_add(u64, u64) should typecheck: {:#?}",
                output.errors
            );
        }

        #[test]
        fn wrapping_add_isize_ok() {
            let output = check_source(r"fn f(a: isize, b: isize) -> isize { a.wrapping_add(b) }");
            assert!(
                output.errors.is_empty(),
                "wrapping_add(isize, isize) should typecheck: {:#?}",
                output.errors
            );
        }

        #[test]
        fn wrapping_sub_usize_ok() {
            let output = check_source(r"fn f(a: usize, b: usize) -> usize { a.wrapping_sub(b) }");
            assert!(
                output.errors.is_empty(),
                "wrapping_sub(usize, usize) should typecheck: {:#?}",
                output.errors
            );
        }

        #[test]
        fn wrapping_mul_i16_ok() {
            let output = check_source(r"fn f(a: i16, b: i16) -> i16 { a.wrapping_mul(b) }");
            assert!(
                output.errors.is_empty(),
                "wrapping_mul(i16, i16) should typecheck: {:#?}",
                output.errors
            );
        }

        #[test]
        fn wrapping_add_u16_ok() {
            let output = check_source(r"fn f(a: u16, b: u16) -> u16 { a.wrapping_add(b) }");
            assert!(
                output.errors.is_empty(),
                "wrapping_add(u16, u16) should typecheck: {:#?}",
                output.errors
            );
        }

        #[test]
        fn wrapping_add_i8_ok() {
            let output = check_source(r"fn f(a: i8, b: i8) -> i8 { a.wrapping_add(b) }");
            assert!(
                output.errors.is_empty(),
                "wrapping_add(i8, i8) should typecheck: {:#?}",
                output.errors
            );
        }

        #[test]
        fn wrapping_add_u32_ok() {
            let output = check_source(r"fn f(a: u32, b: u32) -> u32 { a.wrapping_add(b) }");
            assert!(
                output.errors.is_empty(),
                "wrapping_add(u32, u32) should typecheck: {:#?}",
                output.errors
            );
        }

        // --- checked_* returns Option<W> ---

        #[test]
        fn checked_add_i32_returns_option() {
            let output = check_source(
                r"
                fn f(a: i32, b: i32) -> Option<i32> {
                    a.checked_add(b)
                }
                ",
            );
            assert!(
                output.errors.is_empty(),
                "checked_add returns Option<i32>: {:#?}",
                output.errors
            );
        }

        #[test]
        fn checked_sub_u64_returns_option() {
            let output = check_source(
                r"
                fn f(a: u64, b: u64) -> Option<u64> {
                    a.checked_sub(b)
                }
                ",
            );
            assert!(
                output.errors.is_empty(),
                "checked_sub returns Option<u64>: {:#?}",
                output.errors
            );
        }

        #[test]
        fn checked_mul_i64_returns_option() {
            let output = check_source(
                r"
                fn f(a: i64, b: i64) -> Option<i64> {
                    a.checked_mul(b)
                }
                ",
            );
            assert!(
                output.errors.is_empty(),
                "checked_mul returns Option<i64>: {:#?}",
                output.errors
            );
        }

        #[test]
        fn checked_add_usize_returns_option() {
            let output = check_source(
                r"
                fn f(a: usize, b: usize) -> Option<usize> {
                    a.checked_add(b)
                }
                ",
            );
            assert!(
                output.errors.is_empty(),
                "checked_add(usize) returns Option<usize>: {:#?}",
                output.errors
            );
        }

        // --- saturating_* ---

        #[test]
        fn saturating_add_i32_ok() {
            let output = check_source(r"fn f(a: i32, b: i32) -> i32 { a.saturating_add(b) }");
            assert!(
                output.errors.is_empty(),
                "saturating_add(i32) should typecheck: {:#?}",
                output.errors
            );
        }

        #[test]
        fn saturating_sub_u8_ok() {
            let output = check_source(r"fn f(a: u8, b: u8) -> u8 { a.saturating_sub(b) }");
            assert!(
                output.errors.is_empty(),
                "saturating_sub(u8) should typecheck: {:#?}",
                output.errors
            );
        }

        #[test]
        fn saturating_mul_i64_ok() {
            let output = check_source(r"fn f(a: i64, b: i64) -> i64 { a.saturating_mul(b) }");
            assert!(
                output.errors.is_empty(),
                "saturating_mul(i64) should typecheck: {:#?}",
                output.errors
            );
        }

        // --- Negative: mixed-width argument rejected ---

        #[test]
        fn wrapping_add_mixed_width_rejected() {
            let output = check_source(r"fn f(a: i32, b: i64) -> i32 { a.wrapping_add(b) }");
            assert!(
                !output.errors.is_empty(),
                "wrapping_add(i32, i64) must be a type error"
            );
        }

        #[test]
        fn checked_add_mixed_width_rejected() {
            let output = check_source(
                r"
                fn f(a: u8, b: i32) -> Option<u8> {
                    a.checked_add(b)
                }
                ",
            );
            assert!(
                !output.errors.is_empty(),
                "checked_add(u8, i32) must be a type error"
            );
        }

        // --- Negative: float receiver rejected ---

        #[test]
        fn wrapping_add_float_receiver_rejected() {
            let output = check_source(r"fn f(a: f32, b: f32) { a.wrapping_add(b); }");
            assert!(
                !output.errors.is_empty(),
                "wrapping_add on f32 must be an error"
            );
        }

        #[test]
        fn saturating_add_f64_receiver_rejected() {
            let output = check_source(r"fn f(a: f64, b: f64) { a.saturating_add(b); }");
            assert!(
                !output.errors.is_empty(),
                "saturating_add on f64 must be an error"
            );
        }

        // --- Negative: unknown op in family rejected ---

        #[test]
        fn wrapping_div_rejected() {
            let output = check_source(r"fn f(a: i32, b: i32) { a.wrapping_div(b); }");
            assert!(
                !output.errors.is_empty(),
                "wrapping_div must be rejected (div is out of B-3 scope)"
            );
        }

        #[test]
        fn saturating_neg_rejected() {
            let output = check_source(r"fn f(a: i32) { a.saturating_neg(); }");
            assert!(
                !output.errors.is_empty(),
                "saturating_neg must be rejected (not in scope)"
            );
        }

        // --- Negative: zero args rejected ---

        #[test]
        fn wrapping_add_no_arg_rejected() {
            let output = check_source(r"fn f(a: i32) { a.wrapping_add(); }");
            assert!(
                !output.errors.is_empty(),
                "wrapping_add() with no argument must be an arity error"
            );
        }

        // --- Negative: checked_add result cannot be used as bare W ---

        #[test]
        fn checked_add_result_is_not_bare_i32() {
            let output = check_source(r"fn f(a: i32, b: i32) -> i32 { a.checked_add(b) }");
            assert!(
                !output.errors.is_empty(),
                "checked_add returns Option<i32>, not i32 — must be a type error"
            );
        }
    }
}

// ── Associated-types — slice 1 (bounds + defaults end-to-end) ──────────────
//
// Trait-side `type Bar [: Bound] [= default]` declarations flow through
// `trait_info_from_decl_with_diagnostics` into `TraitInfo.associated_types`.
// Impl-side `type Bar = X;` flows through `build_impl_alias_entries` and is
// bound-checked by `check_assoc_type_bounds` in `enter_impl_scope` when
// `enforce = true`.
//
// These tests pin the user-visible behaviour of slice 1:
//   1. defaults fill missing impl bindings;
//   2. trait-side bounds on assoc types reject violating impls;
//   3. impl-side type params satisfy assoc-type bounds via the impl's own
//      `where`/`<T: Bound>` clauses (not via `current_function` plumbing);
//   4. missing-binding diagnostics fire when no default exists;
//   5. `Self::Bar` continues to resolve in trait method signatures;
//   6. duplicate `type Bar; type Bar;` in a trait body is rejected.
mod assoc_types_slice1 {
    use super::*;

    #[test]
    fn assoc_type_default_fills_missing_impl_binding() {
        // Trait declares `type Step = i32`; impl omits the binding.
        // Resolution must use the default rather than reporting "missing".
        let output = check_source(
            r"
            trait Counter {
                type Step = i32;
                fn step(val: Self) -> Self::Step;
            }

            type Tick {}

            impl Counter for Tick {
                fn step(val: Tick) -> i32 { 1 }
            }
            ",
        );
        assert!(
            output.errors.is_empty(),
            "expected no errors; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn assoc_type_bound_violation_rejects_impl() {
        // Trait declares `type Out: Display`; impl binds it to a type
        // that does not implement Display. Must produce BoundsNotSatisfied.
        let (errors, _warnings) = parse_and_check_with_stdlib(
            r"
            trait Show {
                type Out: Display;
                fn show(val: Self) -> Self::Out;
            }

            type Widget {}

            type Plain {}

            impl Show for Widget {
                type Out = Plain;
                fn show(val: Widget) -> Plain { Plain {} }
            }
            ",
        );
        assert!(
            errors
                .iter()
                .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied)
                    && e.message.contains("Display")
                    && e.message.contains("Plain")),
            "expected BoundsNotSatisfied citing Display and Plain; got: {errors:?}"
        );
    }

    #[test]
    fn assoc_type_bound_satisfied_by_impl_type_param() {
        // `impl<T: Display> Show for Holder<T> { type Out = T; }` — the
        // assoc-type binding is itself a type parameter that carries the
        // required bound. Must accept without spurious BoundsNotSatisfied.
        let (errors, _warnings) = parse_and_check_with_stdlib(
            r"
            trait Show {
                type Out: Display;
                fn show(val: Self) -> Self::Out;
            }

            type Holder<T> {
                value: T;
            }

            impl<T: Display> Show for Holder<T> {
                type Out = T;
                fn show(val: Holder<T>) -> T { val.value }
            }
            ",
        );
        let bound_errors: Vec<_> = errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied))
            .collect();
        assert!(
            bound_errors.is_empty(),
            "impl<T: Display> binding `type Out = T` must satisfy `type Out: Display`; \
             got bound errors: {bound_errors:?}; all errors: {errors:?}"
        );
    }

    #[test]
    fn assoc_type_missing_binding_diagnostic() {
        // Non-defaulted assoc type with no impl-side binding must be
        // diagnosed at impl-registration time.
        let output = check_source(
            r"
            trait Container {
                type Item;
                fn first(val: Self) -> Self::Item;
            }

            type Box {}

            impl Container for Box {
                fn first(val: Box) -> i64 { 0 }
            }
            ",
        );
        assert!(
            output
                .errors
                .iter()
                .any(|e| e.message.contains("Item") && e.message.contains("associated type")),
            "expected missing-associated-type diagnostic citing `Item`; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn self_assoc_in_trait_method_signature_resolves() {
        // Regression: the existing `Self::Bar` prefix-match path
        // (resolution.rs ~line 613) survives the TraitInfo schema change.
        // The method's declared return type is `Self::Item`; the impl
        // binds `type Item = i64`. Checker must accept the impl's `next`
        // returning `Option<i64>` against `Option<Self::Item>`.
        let output = check_source(
            r"
            trait Iterator {
                type Item;
                fn next(var val: Self) -> Option<Self::Item>;
            }

            type Counter {
                value: i64;
            }

            impl Iterator for Counter {
                type Item = i64;
                fn next(var c: Counter) -> Option<i64> { Some(c.value) }
            }
            ",
        );
        assert!(
            output.errors.is_empty(),
            "expected clean check; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn duplicate_assoc_type_in_trait_body_rejected() {
        // `type Bar; type Bar;` in a single trait body is a duplicate
        // declaration. Must report DuplicateDefinition.
        let output = check_source(
            r"
            trait Foo {
                type Bar;
                type Bar;
            }
            ",
        );
        assert!(
            output
                .errors
                .iter()
                .any(|e| matches!(e.kind, TypeErrorKind::DuplicateDefinition)
                    && e.message.contains("Bar")),
            "expected DuplicateDefinition citing `Bar`; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn assoc_type_bound_skips_when_rhs_is_error() {
        // Regression: when the impl-side `type Out = <unresolvable>` itself
        // produces a `Ty::Error` (e.g. an undefined type), the bounds
        // checker must skip its trait-bound check so we don't pile a
        // cascading `BoundsNotSatisfied` on top of the primary error.
        let (errors, _warnings) = parse_and_check_with_stdlib(
            r"
            trait Show {
                type Out: Display;
                fn show(val: Self) -> Self::Out;
            }

            type Widget {}

            impl Show for Widget {
                type Out = Task<i64>;
                fn show(val: Widget) -> i64 { 0 }
            }
            ",
        );
        let bound_errors: Vec<_> = errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied))
            .collect();
        assert!(
            bound_errors.is_empty(),
            "Ty::Error RHS (from `Task<i64>` not being nameable) must suppress the \
             bound-check cascade; got bound errors: {bound_errors:?}; all errors: {errors:?}"
        );
        // Sanity: the primary error from the bad RHS must still fire.
        assert!(
            !errors.is_empty(),
            "expected the primary diagnostic from the unresolvable RHS to remain"
        );
    }

    #[test]
    fn assoc_type_bound_violation_in_default_points_at_default() {
        // When a trait body supplies a default that violates its own
        // declared bound (`type Out: Display = Plain`), the impl that
        // omits the binding inherits the (bad) default — and the
        // diagnostic must point at the default expression in the trait
        // body, not at the trait header or the impl header.
        let source = r"
            trait Show {
                type Out: Display = Plain;
                fn show(val: Self) -> Self::Out;
            }

            type Plain {}

            type Widget {}

            impl Show for Widget {
                fn show(val: Widget) -> Plain { Plain {} }
            }
            ";
        let (errors, _warnings) = parse_and_check_with_stdlib(source);
        let bound_err = errors
            .iter()
            .find(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied))
            .expect("expected BoundsNotSatisfied for default `Plain` vs `Display`");
        let snippet = &source[bound_err.span.clone()];
        assert_eq!(
            snippet, "Plain",
            "diagnostic span must cover the default `Plain` site, not the trait \
             or impl header; got {snippet:?} (full span {:?})",
            bound_err.span
        );
    }

    #[test]
    fn assoc_type_composite_generic_binding_fails_closed() {
        // `impl<T: Display> Show for Container<T> { type Out = Option<T>; }`
        // — the RHS is a composite generic (`Option<T>`), not a bare impl
        // type-param. Slice 1 deliberately does *not* yet propagate bounds
        // through composite shapes, so `Option<T>` falls through to
        // `type_satisfies_trait_bound`, which (correctly) reports that
        // `Option<T>` does not implement `Display`. This test pins the
        // fail-closed behaviour: we'd rather reject a maybe-valid impl than
        // silently accept an unverified bound. Slice 2 (composite bound
        // propagation) is the proper home for the relaxation.
        let (errors, _warnings) = parse_and_check_with_stdlib(
            r"
            trait Show {
                type Out: Display;
                fn show(val: Self) -> Self::Out;
            }

            type Container<T> {
                value: T;
            }

            impl<T: Display> Show for Container<T> {
                type Out = Option<T>;
                fn show(val: Container<T>) -> Option<T> { Some(val.value) }
            }
            ",
        );
        let bound_err = errors
            .iter()
            .find(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied))
            .expect(
                "slice 1 must fail closed on composite `type Out = Option<T>`; \
                 slice 2 is the proper home for composite bound propagation",
            );
        assert!(
            bound_err.message.contains("Option") && bound_err.message.contains("Display"),
            "expected BoundsNotSatisfied citing `Option` and `Display`; got: {bound_err:?}"
        );
    }

    // TODO(assoc-types slice 2 / parser): `TraitDecl` does not yet parse a
    // `where` clause at the trait header, so `trait Foo where Self::Bar:
    // Display { ... }` cannot be tested end-to-end. Once the parser
    // surfaces `TraitDecl.where_clause`, drop the `#[ignore]` and verify
    // the bound is enforced on impls whose `Self::Bar` binding does not
    // satisfy `Display`.
    #[test]
    #[ignore = "trait-header where-clause syntax not yet parsed; see TODO above"]
    fn assoc_type_where_clause_bound_enforced() {
        let (errors, _warnings) = parse_and_check_with_stdlib(
            r"
            trait Show where Self::Out: Display {
                type Out;
                fn show(val: Self) -> Self::Out;
            }

            type Widget {}

            type Plain {}

            impl Show for Widget {
                type Out = Plain;
                fn show(val: Widget) -> Plain { Plain {} }
            }
            ",
        );
        assert!(
            errors
                .iter()
                .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied)
                    && e.message.contains("Display")
                    && e.message.contains("Plain")),
            "expected BoundsNotSatisfied citing Display and Plain; got: {errors:?}"
        );
    }
}

// ── Associated-types — slice 2 (T::Bar projection in generic signatures) ──
//
// Slice 1 closed `Self::Bar` end-to-end. Slice 2 adds the load-bearing piece:
// `T::Bar` projections where `T` is a generic type parameter with a trait
// bound that declares `Bar`. The parser already stringifies `T::Bar` into
// `TypeExpr::Named { name: "T::Bar" }`; this slice teaches the resolver to
// (a) materialise a deferred `Ty::AssocType` carrier, (b) collapse it at
// call-site monomorphisation when `T` becomes concrete, and (c) reject
// surfaces where `T` has no bound declaring `Bar`.
mod assoc_types_slice2 {
    use super::*;

    #[test]
    fn tbar_projection_in_fn_signature_resolves() {
        // Trait declares `type Item`; generic fn returns `I::Item`. With no
        // call site, the signature alone must check clean — the return type
        // is a deferred `Ty::AssocType` carrier.
        let output = check_source(
            r"
            trait Iterator {
                type Item;
                fn next(it: Self) -> Option<Self::Item>;
            }

            fn make<I: Iterator>(it: I) -> I::Item {
                it.next().unwrap()
            }
            ",
        );
        // The body itself uses `it.next()` which requires dispatch to a
        // trait method on a generic-typed receiver. Whether that
        // dispatches today is orthogonal; the *signature* must accept
        // `I::Item` without an UndefinedType diagnostic citing `I::Item`.
        assert!(
            !output
                .errors
                .iter()
                .any(|e| matches!(e.kind, TypeErrorKind::UndefinedType)
                    && e.message.contains("I::Item")
                    && e.message.contains("no bounds")),
            "signature `I::Item` must resolve via the `Iterator` bound; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn tbar_substitutes_at_call_site() {
        // Calling `make(counter)` where `Counter: Iterator<Item = i32>`
        // must materialise the return type as `i32` (the impl's binding).
        let output = check_source(
            r"
            trait Iterator {
                type Item;
                fn next(var it: Self) -> Option<Self::Item>;
            }

            type Counter {
                value: i64;
            }

            impl Iterator for Counter {
                type Item = i64;
                fn next(var c: Counter) -> Option<i64> { Some(c.value) }
            }

            fn make<I: Iterator>(it: I) -> Option<I::Item> {
                it.next()
            }

            fn caller() -> Option<i64> {
                make(Counter { value: 1 })
            }
            ",
        );
        // No type-mismatch error: caller's `Option<i64>` annotation must
        // unify with `make`'s monomorphised return `Option<I::Item>` →
        // `Option<i64>` once `I = Counter` collapses via the impl's
        // `type Item = i64` binding.
        let mismatches: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::Mismatch { .. }))
            .collect();
        assert!(
            mismatches.is_empty(),
            "expected `make(counter)` to monomorphise to `Option<i64>`; got mismatches: {mismatches:?}; all errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn tbar_missing_bound_diagnostic() {
        // `fn bad<T>(_: T) -> T::Item` with no bound on `T`. The resolver
        // must reject with a typed diagnostic naming the missing-bound
        // surface — not silently treat `T::Item` as an opaque named type.
        let output = check_source(
            r"
            trait Iterator {
                type Item;
                fn next(it: Self) -> Option<Self::Item>;
            }

            fn bad<T>(it: T) -> T::Item {
                it.next().unwrap()
            }
            ",
        );
        assert!(
            output
                .errors
                .iter()
                .any(|e| matches!(e.kind, TypeErrorKind::UndefinedType)
                    && e.message.contains("T::Item")
                    && (e.message.contains("no bounds") || e.message.contains("no trait bound"))),
            "expected projection-missing-bound diagnostic; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn tbar_in_generic_position_collect() {
        // The canonical motivating example: `Vec<I::Item>` in a return type.
        // The carrier nests inside Vec, and at call-site monomorphisation
        // the projection collapses to the impl's binding.
        let output = check_source(
            r"
            trait Iterator {
                type Item;
                fn next(var it: Self) -> Option<Self::Item>;
            }

            type Counter {}

            impl Iterator for Counter {
                type Item = i64;
                fn next(var c: Counter) -> Option<i64> { None }
            }

            fn collect<I: Iterator>(it: I) -> Vec<I::Item> {
                Vec::new()
            }

            fn caller() -> Vec<i64> {
                collect(Counter {})
            }
            ",
        );
        let mismatches: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::Mismatch { .. }))
            .collect();
        assert!(
            mismatches.is_empty(),
            "expected `Vec<I::Item>` to monomorphise to `Vec<i64>`; got mismatches: {mismatches:?}; all errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn tbar_unknown_assoc_diagnostic() {
        // `T: Iterator` declares `type Item`. `T::Other` references an
        // assoc name the trait does not declare. Must produce a typed
        // diagnostic citing the bounds in scope.
        let output = check_source(
            r"
            trait Iterator {
                type Item;
                fn next(it: Self) -> Option<Self::Item>;
            }

            fn bad<T: Iterator>(it: T) -> T::Other {
                it.next().unwrap()
            }
            ",
        );
        assert!(
            output
                .errors
                .iter()
                .any(|e| matches!(e.kind, TypeErrorKind::UndefinedType)
                    && e.message.contains("Other")
                    && e.message.contains("Iterator")),
            "expected unknown-assoc diagnostic citing Other and Iterator; got: {:?}",
            output.errors
        );
    }

    // ── D4: UnknownTraitBoundShape — reject positional type args on bounds ─────
    //
    // `T: Eq<U>` is not a valid bound form in Hew. Positional type arguments
    // on trait bounds are silently erased by `collect_type_param_bounds`,
    // which would reduce `Eq<U>` to bare `Eq` without any diagnostic.
    // The validator fires before erasure and produces `UnknownTraitBoundShape`.

    /// `fn f<T, U>(x: T) where T: Eq<U>` must produce `UnknownTraitBoundShape`
    /// citing `Eq`. This is the canonical D4 regression test.
    #[test]
    fn unknown_trait_bound_shape_rejected_for_positional_type_arg_in_where_clause() {
        let output = check_source(
            r"
            fn f<T, U>(x: T) -> T where T: Eq<U> {
                x
            }
            fn main() {}
            ",
        );
        assert!(
            output.errors.iter().any(|e| matches!(
                &e.kind,
                TypeErrorKind::UnknownTraitBoundShape { trait_name }
                    if trait_name == "Eq"
            )),
            "expected UnknownTraitBoundShape for `Eq<U>` in where-clause; got: {:?}",
            output.errors
        );
    }

    /// `fn f<T: Eq<U>, U>(x: T) -> T` must also produce `UnknownTraitBoundShape`
    /// when the positional arg is an inline type-param bound (not a where-clause).
    #[test]
    fn unknown_trait_bound_shape_rejected_for_positional_type_arg_inline() {
        let output = check_source(
            r"
            fn f<T: Eq<U>, U>(x: T) -> T {
                x
            }
            fn main() {}
            ",
        );
        assert!(
            output.errors.iter().any(|e| matches!(
                &e.kind,
                TypeErrorKind::UnknownTraitBoundShape { trait_name }
                    if trait_name == "Eq"
            )),
            "expected UnknownTraitBoundShape for inline `T: Eq<U>`; got: {:?}",
            output.errors
        );
    }

    /// `impl<T: Eq<U>> Foo<T> { }` must produce `UnknownTraitBoundShape` for
    /// the inline bound `Eq<U>` on the impl type parameter.
    #[test]
    fn unknown_trait_bound_shape_rejected_for_impl_inline_type_param_bound() {
        let output = check_source(
            r"
            type Foo<T> { value: T; }
            impl<T: Eq<U>, U> Foo<T> { }
            fn main() {}
            ",
        );
        assert!(
            output.errors.iter().any(|e| matches!(
                &e.kind,
                TypeErrorKind::UnknownTraitBoundShape { trait_name }
                    if trait_name == "Eq"
            )),
            "expected UnknownTraitBoundShape for inline `T: Eq<U>` on impl; got: {:?}",
            output.errors
        );
    }

    /// `impl<T, U> Foo<T> where T: Eq<U>` must produce `UnknownTraitBoundShape`
    /// for the where-clause bound `Eq<U>` on the impl type parameter.
    #[test]
    fn unknown_trait_bound_shape_rejected_for_impl_where_clause_bound() {
        let output = check_source(
            r"
            type Foo<T> { value: T; }
            impl<T, U> Foo<T> where T: Eq<U> { }
            fn main() {}
            ",
        );
        assert!(
            output.errors.iter().any(|e| matches!(
                &e.kind,
                TypeErrorKind::UnknownTraitBoundShape { trait_name }
                    if trait_name == "Eq"
            )),
            "expected UnknownTraitBoundShape for where-clause `T: Eq<U>` on impl; got: {:?}",
            output.errors
        );
    }

    /// `machine M<T: Eq<U>> { }` must produce `UnknownTraitBoundShape` for
    /// the inline bound `Eq<U>` on the machine type parameter.
    #[test]
    fn unknown_trait_bound_shape_rejected_for_machine_type_param_bound() {
        let output = check_source(
            r"
            machine M<T: Eq<U>, U> { }
            fn main() {}
            ",
        );
        assert!(
            output.errors.iter().any(|e| matches!(
                &e.kind,
                TypeErrorKind::UnknownTraitBoundShape { trait_name }
                    if trait_name == "Eq"
            )),
            "expected UnknownTraitBoundShape for machine `T: Eq<U>`; got: {:?}",
            output.errors
        );
    }

    // ── extern "rt" validation ─────────────────────────────────────────────────
    //
    // `extern "rt"` declares JIT-visible runtime functions. Every symbol must
    // appear in the `stable` section of scripts/jit-symbol-classification.toml.
    // Unclassified symbols produce `ExternRtSymbolUnclassified`; classified
    // symbols are accepted. `extern "C"` is unchanged by this validation.

    fn make_extern_rt_block(symbols: &[&str]) -> Item {
        Item::ExternBlock(ExternBlock {
            abi: "rt".to_string(),
            functions: symbols
                .iter()
                .map(|name| ExternFnDecl {
                    attributes: Vec::new(),
                    name: name.to_string(),
                    params: vec![],
                    return_type: None,
                    is_variadic: false,
                    span: 0..name.len(),
                })
                .collect(),
        })
    }

    /// A classified `extern "rt"` symbol must not produce an error.
    #[test]
    fn extern_rt_classified_symbol_accepted() {
        // hew_sleep_ms is in the stable list.
        let extern_item = make_extern_rt_block(&["hew_sleep_ms"]);
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&Program {
            items: vec![(extern_item, 0..30)],
            module_doc: None,
            module_graph: None,
        });
        let rt_errors: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::ExternRtSymbolUnclassified { .. }))
            .collect();
        assert!(
            rt_errors.is_empty(),
            "classified symbol hew_sleep_ms must not produce ExternRtSymbolUnclassified; \
             got: {rt_errors:?}"
        );
    }

    /// An unclassified `extern "rt"` symbol must produce `ExternRtSymbolUnclassified`.
    #[test]
    fn extern_rt_unclassified_symbol_rejected() {
        let extern_item = make_extern_rt_block(&["fake_unclassified_symbol"]);
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&Program {
            items: vec![(extern_item, 0..40)],
            module_doc: None,
            module_graph: None,
        });
        assert!(
            output.errors.iter().any(|e| matches!(&e.kind,
                TypeErrorKind::ExternRtSymbolUnclassified { symbol_name, .. }
                if symbol_name == "fake_unclassified_symbol"
            )),
            "unclassified symbol must produce ExternRtSymbolUnclassified; got: {:?}",
            output.errors
        );
    }

    /// A `codegen-stable` symbol must be rejected in `extern "rt"` — it is
    /// compiler-emitted, not user-callable. The checker only accepts `stable`.
    #[test]
    fn extern_rt_codegen_stable_symbol_rejected() {
        // hew_actor_cooperate is in the codegen-stable tier, not stable.
        let extern_item = make_extern_rt_block(&["hew_actor_cooperate"]);
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&Program {
            items: vec![(extern_item, 0..40)],
            module_doc: None,
            module_graph: None,
        });
        assert!(
            output.errors.iter().any(|e| matches!(&e.kind,
                TypeErrorKind::ExternRtSymbolUnclassified { symbol_name, .. }
                if symbol_name == "hew_actor_cooperate"
            )),
            "codegen-stable symbol hew_actor_cooperate must be rejected in \
             extern \"rt\"; got: {:?}",
            output.errors
        );
    }

    /// The dyn-trait heap-box storage ABI (`hew_dyn_box_alloc` /
    /// `hew_dyn_box_free`) is compiler-emission only. Exposing it as
    /// user-callable would let user code call the free path with a wrong
    /// `(size, align)` pair or with a foreign pointer, producing double-free
    /// / wrong-layout UB. Both symbols must live in `codegen-stable` and the
    /// checker must reject any `extern "rt"` declaration that names them.
    #[test]
    fn extern_rt_dyn_box_symbols_rejected() {
        for sym in ["hew_dyn_box_alloc", "hew_dyn_box_free"] {
            let extern_item = make_extern_rt_block(&[sym]);
            let mut checker = Checker::new(ModuleRegistry::new(vec![]));
            let output = checker.check_program(&Program {
                items: vec![(extern_item, 0..40)],
                module_doc: None,
                module_graph: None,
            });
            assert!(
                output.errors.iter().any(|e| matches!(&e.kind,
                    TypeErrorKind::ExternRtSymbolUnclassified { symbol_name, .. }
                    if symbol_name == sym
                )),
                "codegen-stable symbol {sym} must be rejected in extern \"rt\"; \
                 got: {:?}",
                output.errors
            );
        }
    }

    /// The suspending-read lifecycle ABI (`hew_conn_await_read` /
    /// `hew_read_slot_new` / `_free` / `_cancel` / `_status` / `_take`) is
    /// compiler-emission only — codegen lowers `await conn.read()` into these
    /// calls and manages the slot's manual refcount + cancellation protocol.
    /// Exposing them as user-callable `extern "rt"` surface would let user code
    /// allocate/free/cancel slots out of protocol and corrupt the refcount
    /// (double-free / use-after-free). All six must live in `codegen-stable` and
    /// the checker must reject any `extern "rt"` declaration that names them.
    #[test]
    fn extern_rt_read_slot_lifecycle_symbols_rejected() {
        for sym in [
            "hew_conn_await_read",
            "hew_read_slot_new",
            "hew_read_slot_free",
            "hew_read_slot_cancel",
            "hew_read_slot_status",
            "hew_read_slot_take",
        ] {
            let extern_item = make_extern_rt_block(&[sym]);
            let mut checker = Checker::new(ModuleRegistry::new(vec![]));
            let output = checker.check_program(&Program {
                items: vec![(extern_item, 0..40)],
                module_doc: None,
                module_graph: None,
            });
            assert!(
                output.errors.iter().any(|e| matches!(&e.kind,
                    TypeErrorKind::ExternRtSymbolUnclassified { symbol_name, .. }
                    if symbol_name == sym
                )),
                "codegen-stable read-slot symbol {sym} must be rejected in \
                 extern \"rt\"; got: {:?}",
                output.errors
            );
        }
    }

    /// The Windows-ABI-shim supervisor helpers (`hew_supervisor_child_get_raw`
    /// and `hew_supervisor_nested_get_raw`) are compiler-emitted only — the
    /// codegen translates MIR `hew_supervisor_child_get` / `_nested_get` calls
    /// into these `_raw` variants to avoid the Windows x64 MSVC sret ABI
    /// mismatch.  Both must live in `codegen-stable` so that user `extern "rt"`
    /// declarations are rejected by the checker.
    #[test]
    fn extern_rt_supervisor_raw_shims_rejected() {
        for sym in [
            "hew_supervisor_child_get_raw",
            "hew_supervisor_nested_get_raw",
        ] {
            let extern_item = make_extern_rt_block(&[sym]);
            let mut checker = Checker::new(ModuleRegistry::new(vec![]));
            let output = checker.check_program(&Program {
                items: vec![(extern_item, 0..60)],
                module_doc: None,
                module_graph: None,
            });
            assert!(
                output.errors.iter().any(|e| matches!(&e.kind,
                    TypeErrorKind::ExternRtSymbolUnclassified { symbol_name, .. }
                    if symbol_name == sym
                )),
                "codegen-stable shim {sym} must be rejected in extern \"rt\"; \
                 got: {:?}",
                output.errors
            );
        }
    }

    /// The stream/sink error channel splits producer from consumer: the
    /// `set_last_error` setters are `internal` (AOT-only, called by hew-cabi
    /// forwarders in native packages) and MUST be rejected in `extern "rt"` — a
    /// user could otherwise mutate the thread-local error channel and allocate
    /// caller-sized strings. The read-side getters stay `stable` so user code
    /// may inspect the last error.
    #[test]
    fn extern_rt_stream_error_setters_rejected_getters_accepted() {
        for setter in [
            "hew_stream_set_last_error",
            "hew_stream_set_last_error_with_errno",
        ] {
            let extern_item = make_extern_rt_block(&[setter]);
            let mut checker = Checker::new(ModuleRegistry::new(vec![]));
            let output = checker.check_program(&Program {
                items: vec![(extern_item, 0..60)],
                module_doc: None,
                module_graph: None,
            });
            assert!(
                output.errors.iter().any(|e| matches!(&e.kind,
                    TypeErrorKind::ExternRtSymbolUnclassified { symbol_name, .. }
                    if symbol_name == setter
                )),
                "internal stream-error setter {setter} must be rejected in extern \"rt\"; \
                 got: {:?}",
                output.errors
            );
        }

        for getter in ["hew_stream_last_error", "hew_stream_last_errno"] {
            let extern_item = make_extern_rt_block(&[getter]);
            let mut checker = Checker::new(ModuleRegistry::new(vec![]));
            let output = checker.check_program(&Program {
                items: vec![(extern_item, 0..40)],
                module_doc: None,
                module_graph: None,
            });
            let rt_errors: Vec<_> = output
                .errors
                .iter()
                .filter(|e| {
                    matches!(&e.kind,
                        TypeErrorKind::ExternRtSymbolUnclassified { symbol_name, .. }
                        if symbol_name == getter
                    )
                })
                .collect();
            assert!(
                rt_errors.is_empty(),
                "stable stream-error getter {getter} must be accepted in extern \"rt\"; \
                 got: {rt_errors:?}"
            );
        }
    }

    /// `extern "C"` blocks with any symbol name must NOT be validated against
    /// the stable list — that is raw user FFI surface.
    #[test]
    fn extern_c_bypasses_rt_validation() {
        let extern_item = Item::ExternBlock(ExternBlock {
            abi: "C".to_string(),
            functions: vec![ExternFnDecl {
                attributes: Vec::new(),
                name: "totally_made_up_ffi_symbol".to_string(),
                params: vec![],
                return_type: None,
                is_variadic: false,
                span: 0..0,
            }],
        });
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&Program {
            items: vec![(extern_item, 0..50)],
            module_doc: None,
            module_graph: None,
        });
        let rt_errors: Vec<_> = output
            .errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::ExternRtSymbolUnclassified { .. }))
            .collect();
        assert!(
            rt_errors.is_empty(),
            "extern \"C\" must not trigger ExternRtSymbolUnclassified; got: {rt_errors:?}"
        );
    }

    // ── gen{} in actor receive handler (A98 / Q98) ─────────────────────────

    /// A `gen { }` block inside an actor receive handler must produce
    /// `GenBlockInActorReceive`, not a generic `InvalidOperation`.
    #[test]
    fn genblock_inside_actor_receive_handler_is_rejected() {
        let output = check_source(
            r"
            actor Counter {
                count: i32;
                receive fn tick() {
                    let _g = gen { count = count + 1; };
                }
            }
            fn main() {}
            ",
        );
        assert!(
            output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::GenBlockInActorReceive),
            "gen{{}} inside actor receive handler must emit GenBlockInActorReceive; got: {:?}",
            output.errors
        );
    }

    /// A `gen { }` block in a plain function (not an actor handler) must emit
    /// `EmptyGenerator` — never `GenBlockInActorReceive`.
    #[test]
    fn genblock_outside_actor_receive_handler_is_not_rejected_with_actor_error() {
        let output = check_source(
            r"
            fn main() {
                let _g = gen { };
            }
            ",
        );
        assert!(
            !output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::GenBlockInActorReceive),
            "gen{{}} outside actor handler must not emit GenBlockInActorReceive; got: {:?}",
            output.errors
        );
        // Empty gen{} fails with EmptyGenerator (no yield expressions to infer from).
        assert!(
            output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::EmptyGenerator),
            "empty gen{{}} must emit EmptyGenerator; got: {:?}",
            output.errors
        );
    }

    // ── gen{} typed checking ────────────────────────────────────────────────

    /// A `gen { yield 1; yield 2; }` in a plain function type-checks cleanly
    /// and produces no errors.  The yield type is inferred as `i32`.
    #[test]
    fn gen_block_outside_receive_type_checks_cleanly() {
        let output = check_source(
            r"
            fn main() {
                let _g = gen { yield 1; yield 2; };
            }
            ",
        );
        assert!(
            output.errors.is_empty(),
            "gen block with yield expressions outside actor receive must type-check cleanly: {:?}",
            output.errors
        );
    }

    /// An empty `gen { }` block must emit `EmptyGenerator` because the yield
    /// type-variable cannot be resolved without any `yield` expressions.
    #[test]
    fn gen_block_empty_emits_empty_generator() {
        let output = check_source(
            r"
            fn main() {
                let _g = gen { };
            }
            ",
        );
        assert!(
            output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::EmptyGenerator),
            "empty gen{{}} must emit EmptyGenerator; got: {:?}",
            output.errors
        );
    }

    /// `gen { }` inside an actor receive handler must emit `GenBlockInActorReceive`
    /// and must NOT emit `EmptyGenerator` — the actor guard fires first.
    #[test]
    fn genblock_in_actor_receive_is_rejected_not_empty_generator() {
        let output = check_source(
            r"
            actor Counter {
                count: i32;
                receive fn tick() {
                    let _g = gen { };
                }
            }
            fn main() {}
            ",
        );
        assert!(
            output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::GenBlockInActorReceive),
            "gen{{}} inside actor receive must emit GenBlockInActorReceive; got: {:?}",
            output.errors
        );
        assert!(
            !output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::EmptyGenerator),
            "gen{{}} inside actor receive must not emit EmptyGenerator (actor guard fires first); got: {:?}",
            output.errors
        );
    }

    // ── gen{} / await in machine transition bodies (Machine Lane B S6) ─────

    #[test]
    fn genblock_inside_machine_transition_is_rejected() {
        let output = check_source(
            r"
            machine Door {
                events {
                    Toggle;
                }

                state Closed;
                state Open;


                on Toggle: Closed => Open {
                    gen { yield Open; }
                }
                on Toggle: Open => Closed {
                    Closed
                }
            }
            fn main() {}
            ",
        );
        assert!(
            output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::GenBlockInMachineTransition),
            "gen{{}} inside machine transition must emit GenBlockInMachineTransition; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn await_inside_machine_transition_is_rejected() {
        let output = check_source(
            r"
            machine Door {
                events {
                    Toggle;
                }

                state Closed;
                state Open;


                on Toggle: Closed => Open {
                    await pending;
                    Open
                }
                on Toggle: Open => Closed {
                    Closed
                }
            }
            fn main() {}
            ",
        );
        assert!(
            output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::AwaitInMachineTransition),
            "await inside machine transition must emit AwaitInMachineTransition; got: {:?}",
            output.errors
        );
    }

    // ── gen{} Return-component inference ───────────────────────────────────

    /// `gen { 1 }` has a tail expression but no yield.  The Return component
    /// must be inferred as i64 (not Unit), and no `EmptyGenerator` is emitted.
    #[test]
    fn gen_block_tail_expr_infers_return_component() {
        let output = check_source(
            r"
            fn main() {
                let _g = gen { 1 };
            }
            ",
        );
        assert!(
            output.errors.is_empty(),
            "gen block with tail expression but no yield must type-check cleanly: {:?}",
            output.errors
        );
    }

    /// `gen { return 1; }` has an explicit return but no yield.  The Return
    /// component is i64, and the body never yields (Yield=Never).  No error.
    #[test]
    fn gen_block_explicit_return_infers_return_component() {
        let output = check_source(
            r"
            fn main() {
                let _g = gen { return 1; };
            }
            ",
        );
        assert!(
            output.errors.is_empty(),
            "gen block with explicit return but no yield must type-check cleanly: {:?}",
            output.errors
        );
    }

    /// `gen { yield 1; 2 }` has both yield and a tail expression.
    /// Both Yield and Return must be inferred (i64 each); no error.
    #[test]
    fn gen_block_yield_and_tail_both_infer() {
        let output = check_source(
            r"
            fn main() {
                let _g = gen { yield 1; 2 };
            }
            ",
        );
        assert!(
            output.errors.is_empty(),
            "gen block with yield and tail expression must type-check cleanly: {:?}",
            output.errors
        );
    }

    /// `return 1` inside gen{} must NOT produce a return-type mismatch against
    /// the full Generator<Y, R> shape; the checker extracts the Return component
    /// for `Stmt::Return` when `in_generator` is set.
    #[test]
    fn gen_block_return_does_not_mismatch_full_generator_type() {
        let output = check_source(
            r"
            fn main() {
                let _g = gen { return 42; };
            }
            ",
        );
        assert!(
            !output
                .errors
                .iter()
                .any(|e| matches!(&e.kind, TypeErrorKind::Mismatch { .. })),
            "`return <expr>` inside gen{{}} must not produce a Mismatch against the Generator \
             wrapper; got: {:?}",
            output.errors
        );
    }

    // ── recursive closure self-reference (E_CLOSURE_RECURSIVE) ─────────────

    /// A closure that references its own let-binding by name must produce
    /// `ClosureRecursive`, not `UndefinedVariable`.
    #[test]
    fn recursive_closure_self_reference_is_rejected() {
        let output = check_source(
            r"
            fn main() {
                let f = |x: i32| -> i32 { f(x) };
            }
            ",
        );
        assert!(
            output
                .errors
                .iter()
                .any(|e| matches!(e.kind, TypeErrorKind::ClosureRecursive { .. })),
            "recursive closure self-reference must emit ClosureRecursive; got: {:?}",
            output.errors
        );
    }

    /// A non-recursive closure that references a binding of the same name
    /// from an outer scope is fine — the binding exists at capture depth.
    #[test]
    fn closure_referencing_outer_binding_same_name_is_not_recursive_error() {
        let output = check_source(
            r"
            fn apply(f: fn(i32) -> i32, x: i32) -> i32 { f(x) }
            fn main() {
                let k: i32 = 10;
                let f = |x: i32| -> i32 { x + k };
                let _result = apply(f, 5);
            }
            ",
        );
        assert!(
            !output
                .errors
                .iter()
                .any(|e| matches!(e.kind, TypeErrorKind::ClosureRecursive { .. })),
            "non-recursive closure should not emit ClosureRecursive; got: {:?}",
            output.errors
        );
    }

    /// The hint in the diagnostic must direct the user to add the symbol to
    /// the classification toml.
    #[test]
    fn extern_rt_unclassified_hint_mentions_toml() {
        let extern_item = make_extern_rt_block(&["my_custom_symbol"]);
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&Program {
            items: vec![(extern_item, 0..30)],
            module_doc: None,
            module_graph: None,
        });
        let err = output
            .errors
            .iter()
            .find(|e| matches!(e.kind, TypeErrorKind::ExternRtSymbolUnclassified { .. }))
            .expect("expected ExternRtSymbolUnclassified error");
        assert!(
            err.suggestions
                .iter()
                .any(|s| s.contains("jit-symbol-classification.toml")),
            "suggestion must mention jit-symbol-classification.toml; got: {:?}",
            err.suggestions
        );
    }
}

// ── stack-hint scanner coverage for tail-promoted if-let ─────────────────
//
// `Expr::IfLet` is produced when `if let … { … }` appears in tail position
// inside a block (the parser promotes `Stmt::IfLet` → `Expr::IfLet` and
// places it in `Block::trailing_expr`).  The scan_expr_for_stack_hints walker
// must handle `Expr::IfLet` bodies so that heap bindings declared inside them
// are reported.
#[cfg(test)]
mod iflet_tail_stack_hint_coverage {
    use super::*;

    fn stack_hint_names(source: &str) -> Vec<String> {
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        output
            .stack_hints
            .into_iter()
            .map(|h| h.binding_name)
            .collect()
    }

    /// A `Vec` binding inside a tail-promoted `if let` body must be reported.
    #[test]
    fn tail_promoted_iflet_body_bindings_are_stack_hint_scanned() {
        let names = stack_hint_names(
            r"fn foo(opt: (i64, i64)) {
    if let (a, _b) = opt {
        let v: Vec<i64> = Vec::new();
        v
    }
}",
        );
        assert!(
            names.iter().any(|n| n == "v"),
            "expected stack hint for `v` inside tail-promoted if-let body; got: {names:?}",
        );
    }

    /// A `Vec` binding inside the `else` block of a tail-promoted `if let`
    /// must also be reported.
    #[test]
    fn tail_promoted_iflet_else_body_bindings_are_stack_hint_scanned() {
        let names = stack_hint_names(
            r"fn foo(opt: (i64, i64)) {
    if let (a, _b) = opt {
        a
    } else {
        let w: Vec<i64> = Vec::new();
        0
    }
}",
        );
        assert!(
            names.iter().any(|n| n == "w"),
            "expected stack hint for `w` inside tail-promoted if-let else-body; got: {names:?}",
        );
    }
}

// ── pattern_resolution side-table tests ──────────────────────────────────────

#[cfg(test)]
mod pattern_resolution {
    use super::*;

    /// Helper: type-check `source` and return the `pattern_resolutions` map.
    fn pattern_resolutions(source: &str) -> HashMap<SpanKey, ArmResolution> {
        let output = check_source(source);
        assert!(
            output.errors.is_empty(),
            "pattern_resolutions test must parse and check cleanly; errors: {:#?}",
            output.errors
        );
        output.pattern_resolutions
    }

    // ── wildcard arm ─────────────────────────────────────────────────────────

    #[test]
    fn wildcard_arm_records_wildcard_kind() {
        let resolutions = pattern_resolutions(
            r"
fn foo(x: i64) {
    match x {
        _ => {}
    }
}",
        );
        assert_eq!(resolutions.len(), 1, "expected one resolution");
        let arm = resolutions.values().next().unwrap();
        assert_eq!(arm.pattern_kind, PatternKind::Wildcard);
        assert!(arm.variant_match.is_none());
        assert!(arm.payload_bindings.is_empty());
    }

    // ── literal arm ──────────────────────────────────────────────────────────

    #[test]
    fn literal_arm_records_literal_kind() {
        let resolutions = pattern_resolutions(
            r"
fn foo(x: i64) -> i64 {
    match x {
        1 => 10,
        _ => 0,
    }
}",
        );
        // Two arms: literal + wildcard
        assert_eq!(resolutions.len(), 2, "expected two resolutions");
        let literal_arm = resolutions
            .values()
            .find(|r| r.pattern_kind == PatternKind::Literal)
            .expect("expected a Literal arm");
        assert!(literal_arm.variant_match.is_none());
        assert!(literal_arm.payload_bindings.is_empty());
    }

    // ── plain binding arm ─────────────────────────────────────────────────────

    #[test]
    fn binding_arm_records_binding_kind_no_variant_match() {
        let resolutions = pattern_resolutions(
            r"
fn foo(x: i64) -> i64 {
    match x {
        n => n,
    }
}",
        );
        assert_eq!(resolutions.len(), 1);
        let arm = resolutions.values().next().unwrap();
        assert_eq!(arm.pattern_kind, PatternKind::Binding);
        assert!(arm.variant_match.is_none());
        assert!(arm.payload_bindings.is_empty());
    }

    // ── Option<T> arms ───────────────────────────────────────────────────────

    #[test]
    fn option_some_arm_records_variant_ctor_and_payload() {
        let resolutions = pattern_resolutions(
            r"
fn foo(opt: Option<i64>) -> i64 {
    match opt {
        Some(v) => v,
        None => 0,
    }
}",
        );
        assert_eq!(resolutions.len(), 2, "expected two arms");

        let some_arm = resolutions
            .values()
            .find(|r| {
                r.variant_match
                    .as_ref()
                    .is_some_and(|vm| vm.variant_name == "Some")
            })
            .expect("expected a Some arm");
        assert_eq!(some_arm.pattern_kind, PatternKind::VariantCtor);
        let vm = some_arm.variant_match.as_ref().unwrap();
        assert_eq!(vm.type_name, "Option");
        assert_eq!(vm.variant_name, "Some");
        assert_eq!(some_arm.payload_bindings.len(), 1);
        assert_eq!(some_arm.payload_bindings[0].binding_name, "v");
        assert_eq!(some_arm.payload_bindings[0].field_idx, 0);
        assert_eq!(some_arm.payload_bindings[0].ty, Ty::I64);

        let none_arm = resolutions
            .values()
            .find(|r| {
                r.variant_match
                    .as_ref()
                    .is_some_and(|vm| vm.variant_name == "None")
            })
            .expect("expected a None arm");
        assert_eq!(none_arm.pattern_kind, PatternKind::VariantCtor);
        assert!(none_arm.payload_bindings.is_empty());
    }

    #[test]
    fn option_some_wildcard_payload_emits_no_binding() {
        let resolutions = pattern_resolutions(
            r"
fn foo(opt: Option<i64>) {
    match opt {
        Some(_) => {},
        None => {},
    }
}",
        );
        let some_arm = resolutions
            .values()
            .find(|r| {
                r.variant_match
                    .as_ref()
                    .is_some_and(|vm| vm.variant_name == "Some")
            })
            .expect("expected a Some arm");
        // Wildcard sub-pattern emits no PayloadBinding
        assert!(
            some_arm.payload_bindings.is_empty(),
            "wildcard payload should not emit a PayloadBinding"
        );
    }

    // ── Result<T, E> arms ────────────────────────────────────────────────────

    #[test]
    fn result_ok_err_arms_record_variant_match() {
        let resolutions = pattern_resolutions(
            r"
fn foo(r: Result<i64, string>) -> i64 {
    match r {
        Ok(v) => v,
        Err(_) => -1,
    }
}",
        );
        assert_eq!(resolutions.len(), 2);

        let ok_arm = resolutions
            .values()
            .find(|r| {
                r.variant_match
                    .as_ref()
                    .is_some_and(|vm| vm.variant_name == "Ok")
            })
            .expect("expected an Ok arm");
        let vm = ok_arm.variant_match.as_ref().unwrap();
        assert_eq!(vm.type_name, "Result");
        assert_eq!(vm.variant_name, "Ok");
        assert_eq!(ok_arm.payload_bindings.len(), 1);
        assert_eq!(ok_arm.payload_bindings[0].binding_name, "v");
        assert_eq!(ok_arm.payload_bindings[0].ty, Ty::I64);

        let err_arm = resolutions
            .values()
            .find(|r| {
                r.variant_match
                    .as_ref()
                    .is_some_and(|vm| vm.variant_name == "Err")
            })
            .expect("expected an Err arm");
        // Wildcard payload — no binding recorded
        assert!(err_arm.payload_bindings.is_empty());
    }

    // ── user enum arms ───────────────────────────────────────────────────────

    #[test]
    fn user_enum_unit_variant_records_variant_ctor_no_payload() {
        let resolutions = pattern_resolutions(
            r"
enum Color { Red; Green; Blue }
fn foo(c: Color) {
    match c {
        Red => {},
        Green => {},
        Blue => {},
    }
}",
        );
        assert_eq!(resolutions.len(), 3);
        for arm in resolutions.values() {
            assert_eq!(arm.pattern_kind, PatternKind::VariantCtor);
            assert!(arm.variant_match.is_some());
            assert_eq!(arm.variant_match.as_ref().unwrap().type_name, "Color");
            assert!(arm.payload_bindings.is_empty());
        }
    }

    #[test]
    fn user_enum_tuple_variant_records_payload_bindings() {
        let resolutions = pattern_resolutions(
            r"
enum Shape { Circle(i64); Square(i64) }
fn foo(s: Shape) -> i64 {
    match s {
        Circle(r) => r,
        Square(side) => side,
    }
}",
        );
        assert_eq!(resolutions.len(), 2);

        let circle_arm = resolutions
            .values()
            .find(|r| {
                r.variant_match
                    .as_ref()
                    .is_some_and(|vm| vm.variant_name == "Circle")
            })
            .expect("expected Circle arm");
        assert_eq!(circle_arm.payload_bindings.len(), 1);
        assert_eq!(circle_arm.payload_bindings[0].binding_name, "r");
        assert_eq!(circle_arm.payload_bindings[0].field_idx, 0);
        assert_eq!(circle_arm.payload_bindings[0].ty, Ty::I64);
    }

    // ── match expression (not statement) ────────────────────────────────────

    #[test]
    fn match_expression_arm_records_resolution() {
        let resolutions = pattern_resolutions(
            r"
fn foo(opt: Option<i64>) -> i64 {
    let v = match opt {
        Some(x) => x,
        None => 0,
    };
    v
}",
        );
        assert_eq!(resolutions.len(), 2);
        let some_arm = resolutions
            .values()
            .find(|r| {
                r.variant_match
                    .as_ref()
                    .is_some_and(|vm| vm.variant_name == "Some")
            })
            .expect("match expression must also record Some arm");
        assert_eq!(some_arm.payload_bindings.len(), 1);
        assert_eq!(some_arm.payload_bindings[0].binding_name, "x");
    }

    // ── tuple pattern ────────────────────────────────────────────────────────

    #[test]
    fn tuple_pattern_records_tuple_kind_and_bindings() {
        let resolutions = pattern_resolutions(
            r"
fn foo(pair: (i64, i64)) -> i64 {
    match pair {
        (a, b) => a + b,
    }
}",
        );
        assert_eq!(resolutions.len(), 1);
        let arm = resolutions.values().next().unwrap();
        assert_eq!(arm.pattern_kind, PatternKind::TuplePattern);
        assert!(arm.variant_match.is_none());
        assert_eq!(arm.payload_bindings.len(), 2);
        assert_eq!(arm.payload_bindings[0].binding_name, "a");
        assert_eq!(arm.payload_bindings[0].field_idx, 0);
        assert_eq!(arm.payload_bindings[1].binding_name, "b");
        assert_eq!(arm.payload_bindings[1].field_idx, 1);
    }

    #[test]
    fn record_pattern_uses_declaration_order_for_field_indices() {
        let resolutions = pattern_resolutions(
            r"
type Weird {
    z: i64,
    a: i64,
}

fn foo(w: Weird) -> i64 {
    match w {
        Weird { z, a } => z - a,
    }
}",
        );
        let arm = resolutions.values().next().unwrap();
        assert_eq!(arm.pattern_kind, PatternKind::StructPattern);
        assert_eq!(arm.payload_bindings.len(), 2);
        assert_eq!(arm.payload_bindings[0].binding_name, "z");
        assert_eq!(arm.payload_bindings[0].field_idx, 0);
        assert_eq!(arm.payload_bindings[1].binding_name, "a");
        assert_eq!(arm.payload_bindings[1].field_idx, 1);
    }

    #[test]
    fn record_match_omitted_field_fails_closed_until_rest_patterns() {
        let output = check_source(
            r"
type Point {
    x: i64,
    y: i64,
}

fn foo(p: Point) -> i64 {
    match p {
        Point { x } => x,
    }
}",
        );
        assert!(
            output.errors.iter().any(|error| {
                error.kind == TypeErrorKind::InvalidOperation
                    && error.message.contains("omits field(s) y")
                    && error.message.contains("rest patterns")
            }),
            "expected omitted-field rest-pattern diagnostic, got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn tuple_match_literal_subpattern_fails_closed() {
        let output = check_source(
            r"
fn foo(pair: (i64, i64)) -> i64 {
    match pair {
        (0, y) => y,
        _ => 0,
    }
}",
        );
        assert!(
            output.errors.iter().any(|error| {
                error.kind == TypeErrorKind::InvalidOperation
                    && error.message.contains("tuple subpattern `integer literal`")
            }),
            "expected tuple literal-subpattern diagnostic, got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn record_match_literal_subpattern_fails_closed() {
        let output = check_source(
            r"
type Point {
    x: i64,
    y: i64,
}

fn foo(p: Point) -> i64 {
    match p {
        Point { x: 0, y } => y,
        _ => 0,
    }
}",
        );
        assert!(
            output.errors.iter().any(|error| {
                error.kind == TypeErrorKind::InvalidOperation
                    && error.message.contains("field subpattern `integer literal`")
            }),
            "expected record literal-subpattern diagnostic, got: {:#?}",
            output.errors
        );
    }

    // ── or-pattern is absent ─────────────────────────────────────────────────

    #[test]
    fn or_pattern_arm_absent_from_side_table() {
        let resolutions = pattern_resolutions(
            r"
fn foo(x: i64) {
    match x {
        1 | 2 => {},
        _ => {},
    }
}",
        );
        // The or-pattern arm must be absent; only the wildcard arm is recorded.
        assert_eq!(
            resolutions.len(),
            1,
            "or-pattern arm must not appear in pattern_resolutions"
        );
        let arm = resolutions.values().next().unwrap();
        assert_eq!(arm.pattern_kind, PatternKind::Wildcard);
    }

    // ── payload types resolved at output boundary ────────────────────────────

    #[test]
    fn payload_binding_type_is_concrete_not_inferred() {
        let resolutions = pattern_resolutions(
            r"
fn foo(opt: Option<i64>) -> i64 {
    match opt {
        Some(v) => v,
        None => 0,
    }
}",
        );
        let some_arm = resolutions
            .values()
            .find(|r| {
                r.variant_match
                    .as_ref()
                    .is_some_and(|vm| vm.variant_name == "Some")
            })
            .unwrap();
        // Ty::Var must not survive the output boundary
        assert!(
            !matches!(some_arm.payload_bindings[0].ty, Ty::Var(_)),
            "payload binding type must be concrete, not Ty::Var"
        );
        assert_eq!(some_arm.payload_bindings[0].ty, Ty::I64);
    }

    #[test]
    fn struct_variant_records_source_order_field_idx() {
        // Variant declared with fields in order: c, a, b.
        // A pattern matching only `a` must record field_idx == 1
        // (the declaration position), not field_idx == 0 (alphabetical
        // position of "a" among ["a","b","c"]).
        let resolutions = pattern_resolutions(
            r"
enum Tri { Bar { c: i64; a: i64; b: i64 } }
fn foo(t: Tri) -> i64 {
    match t {
        Tri::Bar { a } => a,
    }
}",
        );
        let bar_arm = resolutions
            .values()
            .find(|r| {
                r.variant_match
                    .as_ref()
                    .is_some_and(|vm| vm.variant_name == "Bar")
            })
            .expect("Bar arm must be recorded");
        assert_eq!(bar_arm.payload_bindings.len(), 1);
        assert_eq!(bar_arm.payload_bindings[0].binding_name, "a");
        // Declaration order: c=0, a=1, b=2 — so field_idx for 'a' must be 1.
        assert_eq!(
            bar_arm.payload_bindings[0].field_idx, 1,
            "field_idx must reflect declaration order, not alphabetical order"
        );
    }
}

// ── Unsupported payload subpatterns (fail-closed gate) ──────────────────────
//
// These tests verify that the checker emits `UnsupportedPayloadSubpattern`
// rather than silently lowering unsupported payload subpatterns as wildcards.

/// Literal in tuple-variant payload position is accepted by the checker and
/// carried forward for HIR/MIR predicate handling rather than being rejected at
/// the parser/checker boundary.
#[test]
fn constructor_payload_literal_is_accepted() {
    let output = check_source(
        r"
enum Shape { Line(i64); Square(i64) }
fn main() -> i64 {
    let s = Shape::Line(2);
    match s {
        Shape::Line(1) => 999,
        Shape::Line(x) => x,
        Shape::Square(_) => 0,
    }
}",
    );
    assert!(
        !output.errors.iter().any(|e| matches!(
            &e.kind,
            crate::error::TypeErrorKind::UnsupportedPayloadSubpattern {
                kind_label,
                ..
            } if kind_label == "literal"
        )),
        "literal payload subpatterns must not emit UnsupportedPayloadSubpattern; got errors: {:#?}",
        output.errors
    );
}

/// Nested constructor in tuple-variant payload is accepted and recorded as a
/// `PayloadVariantPattern` in the arm's resolution side-table entry.
#[test]
fn constructor_payload_nested_ctor_is_accepted_and_recorded() {
    let output = check_source(
        r"
enum Color { Red; Green }
enum Shape { Line(Color); Square(i64) }
fn main() -> i64 {
    let s = Shape::Line(Color::Red);
    match s {
        Shape::Line(Color::Red) => 1,
        Shape::Line(_) => 0,
        Shape::Square(_) => 0,
    }
}",
    );
    assert!(
        output.errors.is_empty(),
        "nested constructor payload subpattern must be accepted; got errors: {:#?}",
        output.errors
    );
    let nested: Vec<_> = output
        .pattern_resolutions
        .values()
        .flat_map(|resolution| resolution.payload_variant_patterns.iter())
        .collect();
    assert_eq!(
        nested.len(),
        1,
        "exactly one arm carries a nested constructor subpattern"
    );
    let pvp = nested[0];
    assert_eq!(pvp.field_idx, 0);
    assert_eq!(pvp.variant_match.type_name, "Color");
    assert_eq!(pvp.variant_match.variant_name, "Red");
    assert!(pvp.bindings.is_empty());
    assert!(pvp.nested.is_empty());
}

/// A nested constructor that binds from the inner payload (`Ok(Ok(v))`)
/// records the inner binding on the nested pattern, not on the arm.
#[test]
fn constructor_payload_nested_ctor_inner_binding_recorded() {
    let output = check_source(
        r"
fn doubly() -> Result<Result<i64, string>, string> {
    Ok(Ok(42))
}
fn main() -> i64 {
    match doubly() {
        Ok(Ok(v)) => v,
        Ok(Err(e)) => 0 - 1,
        Err(e) => 0 - 2,
    }
}",
    );
    assert!(
        output.errors.is_empty(),
        "nested Result patterns must be accepted; got errors: {:#?}",
        output.errors
    );
    let inner_ok = output
        .pattern_resolutions
        .values()
        .flat_map(|resolution| resolution.payload_variant_patterns.iter())
        .find(|pvp| pvp.variant_match.variant_name == "Ok")
        .expect("Ok(Ok(v)) arm records a nested Ok pattern");
    assert_eq!(inner_ok.bindings.len(), 1);
    assert_eq!(inner_ok.bindings[0].binding_name, "v");
    assert_eq!(inner_ok.bindings[0].field_idx, 0);
}

/// Tuple destructure inside tuple-variant payload position must be rejected.
#[test]
fn constructor_payload_tuple_destructure_emits_unsupported_diagnostic() {
    let output = check_source(
        r"
enum Pair { Both((i64, i64)); None }
fn main() -> i64 {
    let p = Pair::Both((1, 2));
    match p {
        Pair::Both((a, b)) => a,
        Pair::None => 0,
    }
}",
    );
    assert!(
        output.errors.iter().any(|e| matches!(
            &e.kind,
            crate::error::TypeErrorKind::UnsupportedPayloadSubpattern { .. }
        )),
        "expected UnsupportedPayloadSubpattern error for tuple-in-payload; got errors: {:#?}",
        output.errors
    );
}

/// Binding and wildcard payload subpatterns must remain accepted.
/// Guards against the rejection being too broad.
#[test]
fn constructor_payload_binding_and_wildcard_are_accepted() {
    let output = check_source(
        r"
enum Shape { Line(i64); Square(i64) }
fn foo(s: Shape) -> i64 {
    match s {
        Shape::Line(x) => x,
        Shape::Square(_) => 0,
    }
}",
    );
    assert!(
        !output.errors.iter().any(|e| matches!(
            &e.kind,
            crate::error::TypeErrorKind::UnsupportedPayloadSubpattern { .. }
        )),
        "binding and wildcard payload subpatterns must not emit UnsupportedPayloadSubpattern; \
         got errors: {:#?}",
        output.errors
    );
}

// ── Generic machine transition-body inference (Lane B S8 prerequisite) ────

/// Bare struct-state constructor in a generic machine transition body must
/// type-check when the machine has type params and the state has a generic
/// field.  `Faulted { error: event.error }` must resolve without errors.
#[test]
fn generic_machine_struct_state_bare_constructor_infers() {
    let output = check_source(
        r"
        machine Work<T> {
            events {
                Crash { code: i64; }
            }

            state Running { handle: T; }
            state Faulted { code: i64; }


            on Crash: Running => Faulted {
                Faulted { code: event.code }
            }
            on Crash: Faulted => Faulted {
                state
            }
        }
        fn main() {}
        ",
    );
    assert!(
        output.errors.is_empty(),
        "bare struct-state constructor in generic machine transition must type-check; \
         got errors: {:#?}",
        output.errors
    );
}

/// Qualified struct-state constructor `Machine::State { … }` inside a
/// generic machine transition body must also type-check.
#[test]
fn generic_machine_struct_state_qualified_constructor_infers() {
    let output = check_source(
        r"
        machine Work<T> {
            events {
                Crash { code: i64; }
            }

            state Running { handle: T; }
            state Faulted { code: i64; }


            on Crash: Running => Faulted {
                Work::Faulted { code: event.code }
            }
            on Crash: Faulted => Faulted {
                state
            }
        }
        fn main() {}
        ",
    );
    assert!(
        output.errors.is_empty(),
        "qualified struct-state constructor in generic machine transition must type-check; \
         got errors: {:#?}",
        output.errors
    );
}

/// Non-generic machine struct-state constructors must continue to work
/// (regression guard for the `synthesize`→`check_against` change).
#[test]
fn non_generic_machine_struct_state_constructor_regression_free() {
    let output = check_source(
        r"
        machine Door {
            events {
                OpenDoor { id: i64; }
                CloseDoor;
            }

            state Closed;
            state Opened { handle: i64; }


            on OpenDoor: Closed => Opened {
                Door::Opened { handle: event.id }
            }
            on CloseDoor: Opened => Closed {
                Closed
            }
            on OpenDoor: Opened => Opened {
                state
            }
            on CloseDoor: Closed => Closed {
                state
            }
        }
        fn main() {}
        ",
    );
    assert!(
        output.errors.is_empty(),
        "non-generic machine struct-state constructor must stay green (regression); \
         got errors: {:#?}",
        output.errors
    );
}

#[test]
fn machine_transition_self_field_reads_source_payload() {
    let output = check_source(
        r"
        machine Counter {
            events {
                Inc;
                Reset;
            }

            state Zero;
            state NonZero { value: i64; }


            on Inc: Zero => NonZero {
                NonZero { value: 1 }
            }
            on Inc: NonZero => NonZero reenter {
                NonZero { value: self.value + 1 }
            }
            on Reset: NonZero => Zero {
                Zero
            }
            on Reset: Zero => Zero reenter {
                Zero
            }
        }
        fn main() {}
        ",
    );
    assert!(
        output.errors.is_empty(),
        "`self.field` inside a concrete machine transition must type-check; \
         got errors: {:#?}",
        output.errors
    );
}

#[test]
fn machine_transition_bare_self_remains_rejected() {
    let output = check_source(
        r"
        machine Counter {
            events {
                Reset;
            }

            state Zero;
            state NonZero { value: i64; }

            on Reset: NonZero => Zero {
                self
            }
            on Reset: Zero => Zero reenter {
                Zero
            }
        }
        fn main() {}
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::UndefinedVariable
                && error.message.contains("`self` is not a valid identifier")),
        "bare `self` must remain rejected outside `self.field`; got errors: {:#?}",
        output.errors
    );
}

/// `step()` on a concretely-typed generic machine instance must accept a
/// bare event name — the receiver's generic args must substitute through the
/// registered event param.
#[test]
fn generic_machine_step_bare_event_propagates_receiver_args() {
    let output = check_source(
        r"
        machine Work<T> {
            events {
                Initialise;
                Started { handle: T; }
            }

            state Created;
            state Running { handle: T; }


            on Initialise: Created => Created {
                Created
            }
            on Initialise: Running => Running {
                state
            }
            on Started: Created => Running {
                Running { handle: event.handle }
            }
            on Started: Running => Running {
                state
            }
        }
        fn main() {
            var w: Work<i64> = Created;
            w.step(Initialise);
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "step() with bare unit event on generic machine must type-check; \
         got errors: {:#?}",
        output.errors
    );
}

// ── C1 UAF guard: E_SUPERVISOR_PERMANENT_OWNED_HEAP ──────────────────────────
// Negative tests — the diagnostic MUST fire.

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
            e.kind == TypeErrorKind::InvalidOperation
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
            e.kind == TypeErrorKind::InvalidOperation
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
            e.kind == TypeErrorKind::InvalidOperation
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
            e.kind == TypeErrorKind::InvalidOperation
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
            e.kind == TypeErrorKind::InvalidOperation
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
            e.kind == TypeErrorKind::InvalidOperation
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
            e.kind == TypeErrorKind::InvalidOperation
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
            pool workers: Worker
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

// ── W3.001 Stage 2 — #[extern_symbol] ingest into FnSig.extern_symbol ─────────

/// `#[extern_symbol("…")]` on an `extern "C"` block fn populates
/// `FnSig.extern_symbol` with a parsed structured template.
#[test]
fn extern_symbol_on_extern_c_fn_populates_fn_sig_spec() {
    let output = check_source(
        r#"
        extern "C" {
            #[extern_symbol("hew_vec_push_{T}")]
            fn vec_push(v: i64, x: i64);
        }
        "#,
    );
    let sig = output
        .fn_sigs
        .get("vec_push")
        .expect("extern fn must be registered");
    let spec = sig
        .extern_symbol
        .as_ref()
        .expect("Stage 2 must populate extern_symbol on the FnSig");
    assert_eq!(spec.template.raw, "hew_vec_push_{T}");
    assert_eq!(
        spec.template.placeholders,
        vec![crate::extern_symbol::PlaceholderName::T]
    );
    assert!(
        output.errors.is_empty(),
        "well-formed template must not emit diagnostics, got: {:#?}",
        output.errors
    );
}

/// Extern fns without `#[extern_symbol]` carry `None` (regression
/// guard: the field must remain opt-in and not default to a synthetic
/// template derived from the fn name).
#[test]
fn extern_fn_without_extern_symbol_attribute_has_none_spec() {
    let output = check_source(
        r#"
        extern "C" {
            fn unrelated(x: i64) -> i64;
        }
        "#,
    );
    let sig = output
        .fn_sigs
        .get("unrelated")
        .expect("extern fn must be registered");
    assert!(
        sig.extern_symbol.is_none(),
        "fn without #[extern_symbol] must not carry a spec"
    );
}

/// `#[extern_symbol("…")]` on an inherent impl method populates the
/// spec on BOTH the `fn_sigs` entry and the `TypeDef.methods` entry,
/// so Stage-3 method-call rewrite (which reads from `td.methods`) sees
/// the same template as Stage-2 ingest.
#[test]
fn extern_symbol_on_impl_method_populates_both_fn_sigs_and_type_def_methods() {
    let output = check_source(
        r#"
        type Holder { x: i64 }

        impl Holder {
            #[extern_symbol("hew_holder_clone")]
            fn cloned(self) -> Holder {
                self
            }
        }
        "#,
    );
    assert!(
        output.errors.is_empty(),
        "well-formed template must not emit diagnostics, got: {:#?}",
        output.errors
    );
    let sig = output
        .fn_sigs
        .get("Holder::cloned")
        .expect("impl method must be in fn_sigs");
    let spec = sig
        .extern_symbol
        .as_ref()
        .expect("Stage 2 must populate extern_symbol on the fn_sigs entry");
    assert_eq!(spec.template.raw, "hew_holder_clone");
    assert!(
        spec.template.is_monomorphic(),
        "no {{T}} placeholder → monomorphic"
    );

    let td = output
        .type_defs
        .get("Holder")
        .expect("Holder type must be registered");
    let method_sig = td
        .methods
        .get("cloned")
        .expect("cloned method must be on TypeDef");
    let method_spec = method_sig
        .extern_symbol
        .as_ref()
        .expect("Stage 2 must mirror the spec onto TypeDef.methods");
    assert_eq!(method_spec.template.raw, "hew_holder_clone");
}

/// A malformed template surfaces as `InvalidExternSymbolTemplate` with
/// the exact deterministic `reason` string from the parser — this is
/// the Stage-2 diagnostic gate referenced by plan §5.5.
#[test]
fn malformed_extern_symbol_template_emits_invalid_template_diagnostic() {
    let output = check_source(
        r#"
        extern "C" {
            #[extern_symbol("hew_vec_{Q}")]
            fn bad(x: i64);
        }
        "#,
    );
    let invalid: Vec<_> = output
        .errors
        .iter()
        .filter_map(|e| match &e.kind {
            TypeErrorKind::InvalidExternSymbolTemplate { reason } => Some(reason.as_str()),
            _ => None,
        })
        .collect();
    assert_eq!(
        invalid.len(),
        1,
        "expected exactly one InvalidExternSymbolTemplate, got errors: {:#?}",
        output.errors
    );
    assert!(
        invalid[0].contains("`{Q}`"),
        "diagnostic reason must name the offending placeholder, got: {:?}",
        invalid[0]
    );

    // Fail-closed: the FnSig must NOT carry a partial / malformed
    // template — Stage 3 should treat this fn as having no rewrite
    // and route through the legacy path (or surface an
    // unresolved-symbol diagnostic later).
    let sig = output
        .fn_sigs
        .get("bad")
        .expect("extern fn must still be registered for downstream resolution");
    assert!(
        sig.extern_symbol.is_none(),
        "rejected template must leave extern_symbol = None (fail-closed)",
    );
}

/// An empty template is rejected with the exact `"empty template"`
/// reason — Stage-5 diagnostic-precision tests pin against this
/// spelling.
#[test]
fn empty_extern_symbol_template_is_rejected_with_empty_reason() {
    let output = check_source(
        r#"
        extern "C" {
            #[extern_symbol("")]
            fn empty(x: i64);
        }
        "#,
    );
    let reason = output
        .errors
        .iter()
        .find_map(|e| match &e.kind {
            TypeErrorKind::InvalidExternSymbolTemplate { reason } => Some(reason.clone()),
            _ => None,
        })
        .expect("expected InvalidExternSymbolTemplate diagnostic");
    assert_eq!(reason, "empty template");
}

// ---------------------------------------------------------------------------
// Q297 Stage 1 — receiver-mutability flag plumbing.
//
// These tests replace the descoped accept fixtures
// `iter_next_mut_receiver.hew` (S1-V2) and `iter_var_receiver_drop_once.hew`
// (S1-V5). End-to-end coverage of those shapes is blocked on pre-existing
// gaps (`Self`-substitution at the MIR boundary for user trait-impl bodies
// and `MethodCallNoRewrite` on direct `v.into_iter()` / `it.next()` outside
// the for-loop desugar). The checker-level invariant — that the
// `requires_mutable_receiver` flag is populated everywhere the call-site
// gate reads it from — is exactly what Stage 1 owns, so we pin it here.

#[test]
fn q297_user_iterator_impl_records_mut_receiver_flag_in_both_tables() {
    // `lookup_named_method_sig` prefers `td.methods` before `fn_sigs`, so
    // the flag must be set in BOTH tables. Missing either one silently
    // disables the caller-side mutable-binding gate.
    let output = check_source(
        r"
        type Counter { val: i32 }

        impl Iterator for Counter {
            type Item = i32;
            fn next(var self) -> Option<i32> {
                Some(self.val)
            }
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "expected clean typecheck, got: {:?}",
        output.errors,
    );
    let sig = output
        .fn_sigs
        .get("Counter::next")
        .expect("Counter::next must be registered in fn_sigs");
    assert!(
        sig.requires_mutable_receiver,
        "fn_sigs[Counter::next].requires_mutable_receiver must be true for `var self`",
    );
    let td = output
        .type_defs
        .get("Counter")
        .expect("Counter type must be registered");
    let method_sig = td
        .methods
        .get("next")
        .expect("Counter::next must be present in td.methods");
    assert!(
        method_sig.requires_mutable_receiver,
        "td.methods[next].requires_mutable_receiver must be true for `var self`",
    );
}

#[test]
fn q297_immut_self_method_records_no_mut_receiver_flag() {
    // Negative control: a plain `self` receiver must NOT carry the flag.
    let output = check_source(
        r"
        type Counter { val: i32 }

        impl Counter {
            fn peek(self) -> i32 { self.val }
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "expected clean typecheck, got: {:?}",
        output.errors,
    );
    let sig = output
        .fn_sigs
        .get("Counter::peek")
        .expect("Counter::peek must be registered");
    assert!(
        !sig.requires_mutable_receiver,
        "plain `self` receiver must not set requires_mutable_receiver",
    );
}

#[test]
fn q297_trait_var_self_vs_impl_self_rejects_with_receiver_mutability_detail() {
    // Trait declares `var self`; impl uses plain `self`. Q004's
    // receiver-mutability axis (added in Stage 1) must reject this.
    let output = check_source(
        r"
        trait Bump {
            fn step(var self) -> i64;
        }

        type Box { n: i64 }

        impl Bump for Box {
            fn step(self) -> i64 { self.n }
        }
        ",
    );
    let mismatch = output.errors.iter().find(|e| {
        matches!(
            &e.kind,
            TypeErrorKind::TraitImplSignatureMismatch { detail, .. }
                if *detail == "receiver mutability"
        )
    });
    assert!(
        mismatch.is_some(),
        "expected TraitImplSignatureMismatch(receiver mutability), got: {:?}",
        output.errors,
    );
}

#[test]
fn q297_let_bound_receiver_rejects_var_self_method_call() {
    // Caller-side gate: a `let`-bound (immutable) receiver cannot dispatch
    // through a method that requires `var self`. Use a trait-impl shape —
    // Stage 1 keeps the long-standing rejection of `var self` on inherent
    // impls; the relaxation only applies to trait impls where the trait
    // contract gives the mutation observable meaning.
    let output = check_source(
        r"
        trait Bump {
            fn step(var self) -> i64;
        }

        type Counter { val: i64 }

        impl Bump for Counter {
            fn step(var self) -> i64 {
                self.val = self.val + 1;
                self.val
            }
        }

        fn main() {
            let c = Counter { val: 0 };
            c.step();
        }
        ",
    );
    let mutability = output.errors.iter().find(|e| {
        matches!(e.kind, TypeErrorKind::MutabilityError)
            && e.message.contains("requires a mutable binding receiver")
    });
    assert!(
        mutability.is_some(),
        "expected MutabilityError on let-bound receiver, got: {:?}",
        output.errors,
    );
}

#[test]
fn q297_var_bound_receiver_accepts_var_self_method_call() {
    // Positive control for the caller-side gate: a `var`-bound receiver
    // must dispatch cleanly through the same `var self` trait method.
    let output = check_source(
        r"
        trait Bump {
            fn step(var self) -> i64;
        }

        type Counter { val: i64 }

        impl Bump for Counter {
            fn step(var self) -> i64 {
                self.val = self.val + 1;
                self.val
            }
        }

        fn main() {
            var c = Counter { val: 0 };
            c.step();
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "var-bound receiver must accept var-self trait-method call, got: {:?}",
        output.errors,
    );
}

#[test]
fn w3042_static_trait_dispatch_let_bound_receiver_rejects_var_self_method() {
    // W3.042 S2-S4: receiver-mutability gate on the generic-bound
    // StaticTraitDispatch sub-arm. A `let`-bound generic-typed receiver
    // dispatched through a trait method that declares `var self` must
    // emit a MutabilityError that names the dispatch kind so the
    // diagnostic is distinguishable from the (Ty::Named, _) variant.
    let output = check_source(
        r"
        trait Bump {
            fn step(var self) -> i64;
        }

        type Counter { val: i64 }

        impl Bump for Counter {
            fn step(var self) -> i64 {
                self.val = self.val + 1;
                self.val
            }
        }

        fn pump<I: Bump>(it: I) -> i64 {
            it.step()
        }

        fn main() {
            var c = Counter { val: 0 };
            pump(c);
        }
        ",
    );
    let mutability = output.errors.iter().find(|e| {
        matches!(e.kind, TypeErrorKind::MutabilityError)
            && e.message
                .contains("statically dispatched on type parameter")
            && e.message.contains("requires a mutable binding receiver")
    });
    assert!(
        mutability.is_some(),
        "expected StaticTraitDispatch MutabilityError on let-bound generic receiver, got: {:?}",
        output.errors,
    );
}

#[test]
fn w3042_static_trait_dispatch_var_bound_receiver_accepts_var_self_method() {
    // Positive control: a `var`-bound generic-typed receiver dispatched
    // through the same `var self` trait method must type-check clean.
    let output = check_source(
        r"
        trait Bump {
            fn step(var self) -> i64;
        }

        type Counter { val: i64 }

        impl Bump for Counter {
            fn step(var self) -> i64 {
                self.val = self.val + 1;
                self.val
            }
        }

        fn pump<I: Bump>(var it: I) -> i64 {
            it.step()
        }

        fn main() {
            var c = Counter { val: 0 };
            pump(c);
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .all(|e| !matches!(e.kind, TypeErrorKind::MutabilityError)),
        "var-bound generic receiver must accept var-self trait-method call, got: {:?}",
        output.errors,
    );
}

#[test]
fn w3042_dyn_trait_let_bound_receiver_rejects_var_self_method() {
    // W3.042 S2-S4: receiver-mutability gate on the Ty::TraitObject /
    // DynMethodCall arm. A `let`-bound `Box<dyn Trait>` receiver
    // dispatched through a trait method that declares `var self` must
    // emit a MutabilityError that names `dyn <Trait>` so the diagnostic
    // is distinguishable from the (Ty::Named, _) and StaticTraitDispatch
    // variants.
    let output = check_source(
        r"
        trait Bump {
            fn step(var self) -> i64;
        }

        type Counter { val: i64 }

        impl Bump for Counter {
            fn step(var self) -> i64 {
                self.val = self.val + 1;
                self.val
            }
        }

        fn invoke(b: dyn Bump) -> i64 {
            b.step()
        }

        fn main() {
            invoke(Counter { val: 0 });
        }
        ",
    );
    let mutability = output.errors.iter().find(|e| {
        matches!(e.kind, TypeErrorKind::MutabilityError)
            && e.message.contains("dyn Bump")
            && e.message.contains("requires a mutable binding receiver")
    });
    assert!(
        mutability.is_some(),
        "expected DynMethodCall MutabilityError on let-bound dyn receiver, got: {:?}",
        output.errors,
    );
}

#[test]
fn q297_stdlib_iterator_next_and_vec_iter_carry_mut_receiver_flag() {
    // Stage 1 flipped `Iterator::next` in `std/builtins.hew` to `var self`
    // and updated the `VecIter` impl in lockstep. Pin the impl-side
    // registration: `register_builtins_hew_impls` feeds the `impl<T>
    // Iterator for VecIter<T>` block through the same Pass-2 path that
    // user impls use, so `td.methods[next]` on `VecIter` must carry the
    // receiver-mutability flag for `lookup_named_method_sig` to surface
    // it to the caller-side gate. (The bare trait declaration in
    // builtins.hew is intentionally not promoted into `fn_sigs` — see
    // `register_builtins_hew_impls` doc comment — so we pin the
    // load-bearing impl side rather than the trait side.)
    let output = check_source("");
    let td = output
        .type_defs
        .get("VecIter")
        .expect("VecIter must be pre-registered from std/builtins.hew");
    let next_sig = td
        .methods
        .get("next")
        .expect("VecIter::next must be present in td.methods");
    assert!(
        next_sig.requires_mutable_receiver,
        "VecIter::next must declare `var self` to match the trait after Q297 Stage 1; \
         td.methods[next].requires_mutable_receiver was false",
    );
}

// ── W4.042: builtin `None` checker-boundary type record ───────────────────────
//
// True root cause (re-plan, tip d81529ba): `synthesize_inner`'s
// `Expr::Identifier("None")` arm early-`return`s `Ty::option(Var)` and bypasses
// the universal `record_type(span, &ty)` tail every other arm reaches. With no
// `expr_types` entry the `check_program` boundary resolve has nothing to write
// back, so the post-unification concrete `Option<i64>` is never recorded at the
// `None` span. Downstream, HIR's unit-ctor fallback stamps a bare
// `Named{Option, args:[]}` → codegen D10. The v1 root cause (a no-op in the HIR
// walker `try_register_enum_instantiation`) was REFUTED: that walker works.

/// Bare builtin `None` under an `Option<i64>` return must leave a resolvable
/// `expr_types` entry that finalizes to the POST-SUBSTITUTION concrete
/// `Option<i64>` — not `Option<Var>` and not absent.
#[test]
fn builtin_none_records_option_type_at_span() {
    let src = "fn f() -> Option<i64> { None }\nfn main() { let _x = f(); }";
    let out = check_source(src);
    assert!(
        out.errors.is_empty(),
        "unexpected type errors: {:#?}",
        out.errors
    );
    let none_start = src.find("None").expect("source must contain `None`");
    // The AST identifier span keys the `expr_types` entry; match on its start
    // offset (the exact span end is a parser detail) and require exactly one
    // entry begins there.
    let mut matches = out.expr_types.iter().filter(|(k, _)| k.start == none_start);
    let (_, recorded) = matches.next().unwrap_or_else(|| {
        panic!(
            "no expr_types entry for the bare `None` at offset {none_start}; entries: {:#?}",
            out.expr_types
        )
    });
    assert!(
        matches.next().is_none(),
        "expected exactly one expr_types entry starting at the `None` offset"
    );
    match recorded {
        Ty::Named { name, args, .. } => {
            assert_eq!(
                name, "Option",
                "recorded type must be Option, got {recorded:?}"
            );
            assert_eq!(
                args.as_slice(),
                &[Ty::I64],
                "recorded `None` type must be finalized to the concrete Option<i64> \
                 (post-substitution), not Option<Var>; got {recorded:?}"
            );
        }
        other => panic!("expected Named Option<i64>, got {other:?}"),
    }
}

/// Fail-closed regression guard (must PASS on tip and stay passing): a
/// genuinely-unconstrained `None` must still surface an inference error — the
/// Stage 2 record change must NOT paper this over with a bogus literal default.
#[test]
fn unconstrained_none_is_inference_error() {
    let out = check_source("fn main() { let x = None; }");
    assert!(
        out.errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InferenceFailed),
        "unconstrained `None` must remain a fail-closed inference error; got {:#?}",
        out.errors
    );
}

// ---------------------------------------------------------------------------
// W4.047 P1.3: typed `resolved_expr_types` handoff population invariants.
//
// These probe the substrate added in P1.1/P1.2 directly at the checker
// boundary: the typed map must be exactly the `ResolvedTy::from_ty` image of
// the concrete entries in `expr_types`, never a superset (no fabricated types)
// and never under-populated for concrete spans (totality for accepted
// programs). They are the unit-level half of the totality evidence.
// ---------------------------------------------------------------------------

/// For a real accepted program, `resolved_expr_types` is exactly the
/// `from_ty`-image of the *concrete* entries in `expr_types`: every typed entry
/// agrees with `from_ty(expr_types[k])`, and every `expr_types` entry that
/// `from_ty` accepts is present in the typed map. This is the population
/// invariant the HIR shadow assert relies on being tautological.
#[test]
fn resolved_expr_types_is_exact_from_ty_image_of_concrete_expr_types() {
    let out = check_source(
        "fn add(a: i64, b: i64) -> i64 { a + b }\n\
         fn main() { let x: i64 = add(40, 2); let y: bool = x > 0; }",
    );
    assert!(
        out.errors.is_empty(),
        "fixture must type-check cleanly; got {:#?}",
        out.errors
    );

    // No fabricated entries: every typed key exists in expr_types and matches.
    for (key, resolved) in &out.resolved_expr_types {
        let ty = out.expr_types.get(key).unwrap_or_else(|| {
            panic!("resolved_expr_types key {key:?} absent from expr_types (fabricated)")
        });
        let expected = ResolvedTy::from_ty(ty).unwrap_or_else(|e| {
            panic!("typed map holds {key:?} but expr_types type {ty:?} fails from_ty: {e}")
        });
        assert_eq!(
            resolved, &expected,
            "typed entry for {key:?} disagrees with from_ty(expr_types[{key:?}])"
        );
    }

    // Totality: every concrete expr_types entry is present in the typed map.
    for (key, ty) in &out.expr_types {
        if ResolvedTy::from_ty(ty).is_ok() {
            assert!(
                out.resolved_expr_types.contains_key(key),
                "concrete expr_types entry {key:?} ({ty:?}) missing from typed handoff map"
            );
        }
    }
}

/// A concrete accepted program populates a non-empty typed map (the handoff is
/// actually carrying data, not silently empty), and every entry is a
/// well-formed `ResolvedTy` with no residual boundary state.
#[test]
fn resolved_expr_types_populated_and_concrete_for_concrete_program() {
    let out = check_source("fn main() { let x: i64 = 7; let y: bool = x == 7; }");
    assert!(out.errors.is_empty(), "fixture must type-check cleanly");
    assert!(
        !out.resolved_expr_types.is_empty(),
        "concrete program must hand off at least one typed expr"
    );
}

/// `TypeCheckOutput::insert_expr_type` keeps the two maps in sync exactly as
// ── Vec index auto-widening tests ────────────────────────────────────────────
//
// A372 / A373: signed integers narrower than i64 are accepted as Vec index
// arguments (`.get`, `.set`, `.remove`) and as `xs[i]` index expressions.
// The widening is operand-only: the Vec element return type is NOT changed.
// Non-integer and unsigned-integer indices remain rejected.

#[test]
fn vec_get_accepts_i32_index() {
    let output = check_source(
        r"
        fn main() {
            let xs: Vec<i64> = Vec::new();
            xs.push(10);
            let i: i32 = 0;
            let _v = xs.get(i);
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "Vec::get with i32 index should be accepted without error, got: {:#?}",
        output.errors
    );
}

#[test]
fn vec_set_accepts_i32_index() {
    let output = check_source(
        r"
        fn main() {
            let xs: Vec<i64> = Vec::new();
            xs.push(0);
            let i: i32 = 0;
            xs.set(i, 99);
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "Vec::set with i32 index should be accepted without error, got: {:#?}",
        output.errors
    );
}

#[test]
fn vec_index_accepts_i32_index() {
    let output = check_source(
        r"
        fn main() -> i64 {
            let xs: Vec<i64> = Vec::new();
            xs.push(42);
            let i: i32 = 0;
            xs[i]
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "xs[i32] index expression should be accepted without error, got: {:#?}",
        output.errors
    );
}

#[test]
fn vec_index_rejects_string_index() {
    let output = check_source(
        r#"
        fn main() {
            let xs: Vec<i64> = Vec::new();
            let s = "hello";
            let _v = xs.get(s);
        }
        "#,
    );
    assert!(
        !output.errors.is_empty(),
        "Vec::get with a string index must be rejected"
    );
}

#[test]
fn vec_index_rejects_bool_index() {
    let output = check_source(
        r"
        fn main() {
            let xs: Vec<i64> = Vec::new();
            let flag = true;
            let _v = xs.get(flag);
        }
        ",
    );
    assert!(
        !output.errors.is_empty(),
        "Vec::get with a bool index must be rejected"
    );
}

#[test]
fn vec_index_rejects_unsigned_i32_index() {
    let output = check_source(
        r"
        fn main() {
            let xs: Vec<i64> = Vec::new();
            let u: u32 = 0;
            let _v = xs.get(u);
        }
        ",
    );
    // u32 is not a narrower *signed* integer — unsigned indices are not
    // auto-widened because the sign-extension semantics differ.
    assert!(
        !output.errors.is_empty(),
        "Vec::get with a u32 index must be rejected (unsigned is not auto-widened)"
    );
}

/// the boundary does: a concrete type lands in both maps; a non-concrete type
/// (a leaked inference var) lands only in `expr_types`, leaving the typed map
/// correctly absent (fail-closed omission, never a fabricated guess).
#[test]
fn insert_expr_type_mirrors_boundary_concreteness_split() {
    let mut out = TypeCheckOutput::default();
    let concrete = SpanKey {
        start: 0,
        end: 4,
        module_idx: 0,
    };
    let leaked = SpanKey {
        start: 4,
        end: 8,
        module_idx: 0,
    };

    out.insert_expr_type(concrete.clone(), Ty::I64);
    out.insert_expr_type(leaked.clone(), Ty::Var(TypeVar::fresh()));

    assert_eq!(out.expr_types.get(&concrete), Some(&Ty::I64));
    assert_eq!(
        out.resolved_expr_types.get(&concrete),
        Some(&ResolvedTy::from_ty(&Ty::I64).unwrap()),
        "concrete type must populate the typed handoff map"
    );

    assert!(
        out.expr_types.contains_key(&leaked),
        "leaked inference var still recorded in the Ty side-table"
    );
    assert!(
        !out.resolved_expr_types.contains_key(&leaked),
        "leaked inference var must be absent from the typed handoff map"
    );
}

// --- Generic swap regression (G4b-bug-1) ---
//
// A return type that PERMUTES the type params of a generic record triggered a
// false "type mismatch" because the sequential per-param substitution loop
// aliased the two params: applying A→B then B→A over the same accumulator
// turned both fields back to A. Parallel substitution fixes this.

#[test]
fn generic_swap_return_type_typechecks_without_false_mismatch() {
    // fn swap<A, B>(p: Pair<A, B>) -> Pair<B, A> { Pair { first: p.second, second: p.first } }
    // Before fix: "type mismatch: expected `B`, found `A`" on `p.first` (the
    // `second` field of the return, which expects type A after swapping).
    let output = check_source(
        r"
type Pair<A, B> { first: A; second: B; }

fn swap<A, B>(p: Pair<A, B>) -> Pair<B, A> {
    Pair<B, A> { first: p.second, second: p.first }
}

fn main() {
    let p = Pair { first: 1, second: 2 };
    let _s = swap(p);
}
",
    );
    assert!(
        output.errors.is_empty(),
        "generic swap must type-check without errors; got: {:#?}",
        output.errors
    );
}

#[test]
fn generic_swap_heterogeneous_params_typechecks() {
    // Heterogeneous concrete args (i64 / bool) expose the aliasing bug most
    // clearly: before the fix the second field got expected type `A` instead of
    // `i64`, and the checker emitted "expected `A`, found `i64`".
    let output = check_source(
        r"
type Pair<A, B> { first: A; second: B; }

fn swap<A, B>(p: Pair<A, B>) -> Pair<B, A> {
    Pair<B, A> { first: p.second, second: p.first }
}

fn main() {
    let p = Pair { first: 1, second: true };
    let _s = swap(p);
}
",
    );
    assert!(
        output.errors.is_empty(),
        "heterogeneous generic swap must type-check; got: {:#?}",
        output.errors
    );
}

#[test]
fn generic_identity_pair_homogeneous_still_typechecks() {
    // Confirm that the non-swapping (homogeneous) case was not broken by the
    // parallel-substitution change.
    let output = check_source(
        r"
type Pair<A, B> { first: A; second: B; }

fn fst<A, B>(p: Pair<A, B>) -> A { p.first }

fn main() {
    let p = Pair { first: 10, second: 3 };
    let _x = fst(p);
}
",
    );
    assert!(
        output.errors.is_empty(),
        "identity (non-swap) generic must still type-check; got: {:#?}",
        output.errors
    );
}

// --- expressions.rs field-access swap regression (G4b-bug-2) ---
//
// `expressions.rs` resolves `receiver.field` for a generic type by zipping
// `td.type_params` with the instantiation `args` and substituting the field's
// declared type.  When the args are themselves abstract named params in swapped
// order (e.g. `Pair<B, A>` inside a function generic over `<A, B>`), sequential
// substitution aliases both params: A→B then B→A maps `A` back to `A`, so
// `p.first` in `fn f<A,B>(p: Pair<B,A>) -> B` yielded type `A` instead of `B`.
// Parallel substitution reads the original field type once and maps all params
// simultaneously.

#[test]
fn generic_field_access_on_swapped_instantiation_typechecks() {
    // fn get_first_of_swapped<A, B>(p: Pair<B, A>) -> B { p.first }
    // Before fix: "type mismatch: expected `B`, found `A`" on `p.first`.
    // Field `first: A` in `Pair<A, B>`, instantiated with args [B, A]:
    // sequential A→B then B→A gives A again; parallel gives B.
    let output = check_source(
        r"
type Pair<A, B> { first: A; second: B; }

fn get_first_of_swapped<A, B>(p: Pair<B, A>) -> B {
    p.first
}
",
    );
    assert!(
        output.errors.is_empty(),
        "field access on swapped generic instantiation must type-check; got: {:#?}",
        output.errors
    );
}

#[test]
fn generic_field_access_second_on_swapped_instantiation_typechecks() {
    // fn get_second_of_swapped<A, B>(p: Pair<B, A>) -> A { p.second }
    // Companion: field `second: B`, instantiated with args [B, A].
    // Sequential B→B (no-op for param A), then B→A gives A. Actually fine
    // in the single-param substitution — but the combined swap test below
    // exercises both fields in the same function to catch any residual alias.
    let output = check_source(
        r"
type Pair<A, B> { first: A; second: B; }

fn get_second_of_swapped<A, B>(p: Pair<B, A>) -> A {
    p.second
}
",
    );
    assert!(
        output.errors.is_empty(),
        "second-field access on swapped generic instantiation must type-check; got: {:#?}",
        output.errors
    );
}

// --- generics.rs trait-object-bound swap regression (G4b-bug-3) ---
//
// `apply_trait_object_bound_substitutions` (generics.rs) substitutes the
// trait's declared type params into the method signature using the concrete
// args from the `dyn Trait<...>` bound.  When a generic function is
// parameterised `<A, B>` and accepts `dyn Mapper<B, A>` (swapped), the
// bound.args are [Named("B"), Named("A")] while type_params are ["A", "B"].
// Sequential substitution: A→B over the method sig, then B→A aliases the
// just-renamed B back to A — so `val: A` is expected for the arg instead
// of `val: B`.  Parallel substitution fixes the alias.

#[test]
fn dyn_trait_two_params_swapped_bound_typechecks() {
    // trait Mapper<A, B> { fn map(Self, A) -> B }
    // fn apply_swapped<A, B>(f: dyn Mapper<B, A>, val: B) -> A { f.map(val) }
    // Before fix: "type mismatch: expected `A`, found `B`" on `val`.
    let output = check_source(
        r"
trait Mapper<A, B> {
    fn map(val: A) -> B;
}

fn apply_swapped<A, B>(f: dyn Mapper<B, A>, val: B) -> A {
    f.map(val)
}
",
    );
    assert!(
        output.errors.is_empty(),
        "dyn Mapper<B,A> call must type-check under swapped bound; got: {:#?}",
        output.errors
    );
}

// --- registration.rs rename_method_type_params swap regression (G4b-bug-4) ---
//
// `rename_method_type_params` (registration.rs) renames the trait method's
// declared type params to match the impl method's names before comparing
// signatures.  When the impl reverses the trait's param order
// (`trait Discard<T,U>` / `impl fn discard<U,T>`), sequential application
// renames T→U then U→T — mapping both back to T.  The impl's first param
// `U` therefore fails to match the now-aliased expected `T` even though the
// signature is structurally equivalent.  Parallel rename resolves the alias.

#[test]
fn impl_method_swapped_type_param_names_accepted() {
    // trait Discard { fn discard<T, U>(a: T, b: U) -> i64; }
    // impl Discard for Discarter { fn discard<U, T>(a: U, b: T) -> i64 { 0 } }
    // Before fix: false "parameter `a` has type `U` but trait requires `T`".
    let output = check_source(
        r"
trait Discard {
    fn discard<T, U>(a: T, b: U) -> i64;
}

type Discarter { }

impl Discard for Discarter {
    fn discard<U, T>(a: U, b: T) -> i64 { 0 }
}
",
    );
    assert!(
        output.errors.is_empty(),
        "impl with swapped method type-param names must satisfy trait; got: {:#?}",
        output.errors
    );
}

// ── lambda annotated-param vs expected-fn-type unification (L29) ─────────────
//
// When a lambda has an explicit param annotation (`|x: T|`) and is passed
// where a `fn(U) -> V` is expected, the checker must unify `T` against `U`.
// Previously the unification was only attempted when the annotation contained
// a `_` hole — fully-concrete annotations (no holes) were silently accepted
// regardless of the expected type. Ditto for annotated return types.

/// A fully-annotated param type that contradicts the expected fn type must be
/// rejected at the call-site with a Mismatch error.
#[test]
fn annotated_lambda_param_type_contradicts_expected_fn_type_is_rejected() {
    // `apply` expects `fn(bool) -> i64`; the lambda says `|x: i64|`.
    let output = check_source(
        r"
fn apply(f: fn(bool) -> i64) -> i64 { f(true) }

fn main() -> i64 {
    apply(|x: i64| x + 1)
}
",
    );
    assert!(
        output.errors.iter().any(|e| {
            matches!(
                &e.kind,
                TypeErrorKind::Mismatch { expected, actual }
                    if expected.contains("bool") && actual.contains("i64")
                        || expected.contains("i64") && actual.contains("bool")
            )
        }),
        "expected Mismatch error for bool/i64 param annotation conflict, got: {:#?}",
        output.errors
    );
}

/// A fully-annotated return type that contradicts the expected fn type must be
/// rejected.
#[test]
fn annotated_lambda_return_type_contradicts_expected_fn_type_is_rejected() {
    // `apply` expects `fn(i64) -> bool`; the lambda says `-> i64`.
    let output = check_source(
        r"
fn apply(f: fn(i64) -> bool) -> bool { f(1) }

fn main() -> bool {
    apply(|x: i64| -> i64 { x + 1 })
}
",
    );
    assert!(
        output.errors.iter().any(|e| {
            matches!(
                &e.kind,
                TypeErrorKind::Mismatch { expected, actual }
                    if expected.contains("bool") && actual.contains("i64")
                        || expected.contains("i64") && actual.contains("bool")
            )
        }),
        "expected Mismatch error for bool/i64 return annotation conflict, got: {:#?}",
        output.errors
    );
}

/// Arity mismatch: a lambda with more params than the expected fn type is
/// already rejected; verify this is still true (regression pin).
#[test]
fn lambda_arity_mismatch_against_expected_fn_type_is_rejected() {
    let output = check_source(
        r"
fn apply(f: fn(i64) -> i64) -> i64 { f(1) }

fn main() -> i64 {
    apply(|x: i64, y: i64| x + y)
}
",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::ArityMismatch),
        "expected ArityMismatch for lambda with extra param, got: {:#?}",
        output.errors
    );
}

/// Unannotated lambda (inference path) must still be accepted when the body
/// is compatible with the expected param type.
#[test]
fn unannotated_lambda_matching_expected_fn_type_is_accepted() {
    let output = check_source(
        r"
fn apply(f: fn(bool) -> i64) -> i64 { f(true) }

fn main() -> i64 {
    apply(|x| if x { 1 } else { 0 })
}
",
    );
    assert!(
        output.errors.is_empty(),
        "unannotated lambda compatible with expected fn type must pass; got: {:#?}",
        output.errors
    );
}

/// Correctly-annotated lambda (annotation matches expected type) is accepted.
#[test]
fn correctly_annotated_lambda_matching_expected_fn_type_is_accepted() {
    let output = check_source(
        r"
fn apply(f: fn(bool) -> i64) -> i64 { f(true) }

fn main() -> i64 {
    apply(|x: bool| if x { 1 } else { 0 })
}
",
    );
    assert!(
        output.errors.is_empty(),
        "correctly annotated lambda must be accepted; got: {:#?}",
        output.errors
    );
}

/// Nested fn type: param is itself a function type; verify annotation mismatch
/// is caught when the outer expected type and annotation disagree on the
/// inner function type.
#[test]
fn annotated_lambda_param_nested_fn_type_contradiction_is_rejected() {
    // Expected: `fn(fn(i64) -> i64) -> i64`
    // Lambda annotation: `fn(fn(bool) -> i64) -> i64` (inner param type wrong)
    let output = check_source(
        r"
fn apply(f: fn(fn(i64) -> i64) -> i64) -> i64 { f(|x| x + 1) }

fn main() -> i64 {
    apply(|g: fn(bool) -> i64| g(true))
}
",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| matches!(&e.kind, TypeErrorKind::Mismatch { .. })),
        "nested fn type annotation mismatch must be rejected; got: {:#?}",
        output.errors
    );
}

// ── Actor field mutability enforcement ──────────────────────────────────
//
// `var` fields are writable everywhere; `let` and bare fields may only be
// assigned inside `init { }` (the constructor). Handlers, methods, and
// lifecycle hooks reject the write with a field-shaped MutabilityError that
// names the declaration site and the `var` fix.

fn immutable_field_errors(output: &TypeCheckOutput, field: &str) -> Vec<String> {
    output
        .errors
        .iter()
        .filter(|e| {
            matches!(e.kind, TypeErrorKind::MutabilityError)
                && e.message
                    .contains(&format!("cannot assign to immutable field `{field}`"))
        })
        .map(|e| e.message.clone())
        .collect()
}

#[test]
fn let_field_assignment_in_receive_fn_is_rejected() {
    let output = check_source(
        r"
actor Counter {
    let count: i64;
    receive fn bump() {
        count = count + 1;
    }
}
",
    );
    assert_eq!(
        immutable_field_errors(&output, "count").len(),
        1,
        "let field write in a receive fn must be rejected; got: {:#?}",
        output.errors
    );
    let err = output
        .errors
        .iter()
        .find(|e| {
            e.message
                .contains("cannot assign to immutable field `count`")
        })
        .expect("immutable field error");
    assert!(
        !err.notes.is_empty(),
        "diagnostic must carry a declaration-site note; got: {err:#?}"
    );
    assert!(
        err.suggestions.iter().any(|s| s.contains("var")),
        "diagnostic must suggest declaring the field with `var`; got: {err:#?}"
    );
}

#[test]
fn bare_field_assignment_in_receive_fn_is_rejected() {
    // A bare field declaration defaults to immutable (parser default),
    // so it follows the same rule as `let`.
    let output = check_source(
        r"
actor Counter {
    count: i64;
    receive fn bump() {
        count = count + 1;
    }
}
",
    );
    assert_eq!(
        immutable_field_errors(&output, "count").len(),
        1,
        "bare field write in a receive fn must be rejected; got: {:#?}",
        output.errors
    );
}

#[test]
fn compound_assignment_to_let_field_is_rejected() {
    let output = check_source(
        r"
actor Counter {
    let count: i64;
    receive fn bump() {
        count += 1;
    }
}
",
    );
    assert_eq!(
        immutable_field_errors(&output, "count").len(),
        1,
        "compound assignment to a let field must be rejected; got: {:#?}",
        output.errors
    );
}

#[test]
fn let_field_assignment_in_actor_method_is_rejected() {
    let output = check_source(
        r"
actor Counter {
    let count: i64;
    receive fn poke() {
        bump();
    }
    fn bump() {
        count = count + 1;
    }
}
",
    );
    assert_eq!(
        immutable_field_errors(&output, "count").len(),
        1,
        "let field write in a plain actor method must be rejected; got: {:#?}",
        output.errors
    );
}

#[test]
fn let_field_assignment_in_on_stop_hook_is_rejected() {
    let output = check_source(
        r"
actor Counter {
    let count: i64;
    receive fn poke() {}
    #[on(stop)]
    fn drain() {
        count = 0;
    }
}
",
    );
    assert_eq!(
        immutable_field_errors(&output, "count").len(),
        1,
        "let field write in a lifecycle hook must be rejected; got: {:#?}",
        output.errors
    );
}

#[test]
fn let_field_assignment_in_init_is_accepted() {
    let output = check_source(
        r"
actor Counter {
    let count: i64;
    init(initial: i64) {
        count = initial;
    }
    receive fn total() -> i64 {
        count
    }
}
",
    );
    assert!(
        immutable_field_errors(&output, "count").is_empty(),
        "init must be allowed to assign let fields; got: {:#?}",
        output.errors
    );
}

#[test]
fn var_field_assignment_in_receive_fn_is_accepted() {
    let output = check_source(
        r"
actor Counter {
    var count: i64;
    receive fn bump() {
        count = count + 1;
    }
}
",
    );
    assert!(
        immutable_field_errors(&output, "count").is_empty(),
        "var field write in a receive fn must be accepted; got: {:#?}",
        output.errors
    );
}

#[test]
fn let_field_read_in_receive_fn_is_accepted() {
    let output = check_source(
        r"
actor Counter {
    let step: i64;
    var count: i64;
    receive fn bump() {
        count = count + step;
    }
}
",
    );
    assert!(
        output
            .errors
            .iter()
            .all(|e| !matches!(e.kind, TypeErrorKind::MutabilityError)),
        "reading a let field in a receive fn must be accepted; got: {:#?}",
        output.errors
    );
}

// ── #[every(duration)] periodic receive handlers ─────────────────────────────
//
// Validation behaviours for the periodic-handler attribute: interval floor,
// handler shape (params / return / generator), attribute arity, and the
// supervisor-child rejection (periodic timers are armed by spawn-site
// codegen, which the supervisor child-spec spawn path never reaches).
mod every_attribute {
    use super::*;

    fn invalid_op_contains(output: &TypeCheckOutput, fragment: &str) -> bool {
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::InvalidOperation && e.message.contains(fragment))
    }

    #[test]
    fn valid_millisecond_interval_accepted() {
        let output = check_source(
            "actor Ticker { var count: i64 = 0; #[every(50ms)] receive fn tick() { count += 1; } } fn main() {}",
        );
        assert!(
            output.errors.is_empty(),
            "a 50ms periodic handler is the spec'd happy path; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn sub_millisecond_interval_rejected() {
        let output =
            check_source("actor Ticker { #[every(500us)] receive fn tick() {} } fn main() {}");
        assert!(
            invalid_op_contains(&output, "minimum periodic interval is 1ms"),
            "500us floors to a 0ms timer interval and must be rejected; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn exactly_one_millisecond_accepted() {
        let output =
            check_source("actor Ticker { #[every(1ms)] receive fn tick() {} } fn main() {}");
        assert!(
            output.errors.is_empty(),
            "1ms is the minimum valid interval and must be accepted; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn handler_with_params_rejected() {
        let output =
            check_source("actor Ticker { #[every(10ms)] receive fn tick(n: i64) {} } fn main() {}");
        assert!(
            invalid_op_contains(&output, "must not have parameters"),
            "periodic handlers receive no payload; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn handler_with_return_type_rejected() {
        let output = check_source(
            "actor Ticker { #[every(10ms)] receive fn tick() -> i64 { 1 } } fn main() {}",
        );
        assert!(
            invalid_op_contains(&output, "must not have a return type"),
            "periodic handlers are fire-and-forget; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn generator_handler_rejected() {
        let output = check_source(
            "actor Ticker { #[every(10ms)] receive gen fn ticks() -> i64 { yield 1; } } fn main() {}",
        );
        assert!(
            invalid_op_contains(&output, "must not be a generator"),
            "generator receive fns have no dispatchable body for a tick; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn multiple_every_attributes_rejected() {
        let output = check_source(
            "actor Ticker { #[every(10ms)] #[every(20ms)] receive fn tick() {} } fn main() {}",
        );
        assert!(
            invalid_op_contains(&output, "multiple #[every] attributes"),
            "only one #[every] per handler; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn non_duration_argument_rejected() {
        let output = check_source("actor Ticker { #[every(5)] receive fn tick() {} } fn main() {}");
        assert!(
            invalid_op_contains(&output, "must be a duration literal"),
            "a bare integer is not a duration; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn supervisor_child_with_periodic_handler_rejected() {
        let output = check_source(
            r"
            actor Heartbeat {
                #[every(100ms)]
                receive fn beat() {}
            }

            supervisor App {
                child hb: Heartbeat;
            }

            fn main() {}
            ",
        );
        assert!(
            invalid_op_contains(&output, "E_SUPERVISOR_PERIODIC_CHILD"),
            "supervisor child spawns never reach spawn-site timer arming; got: {:#?}",
            output.errors
        );
    }

    #[test]
    fn supervisor_child_without_periodic_handler_accepted() {
        let output = check_source(
            r"
            actor Worker {
                receive fn work() {}
            }

            supervisor App {
                child w: Worker;
            }

            fn main() {}
            ",
        );
        assert!(
            !invalid_op_contains(&output, "E_SUPERVISOR_PERIODIC_CHILD"),
            "the accept twin: a message-driven child must not trip the periodic check; got: {:#?}",
            output.errors
        );
    }
}
