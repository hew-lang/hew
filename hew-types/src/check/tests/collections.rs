#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
pub(super) use super::*;

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
            bounds: HashMap::new(),
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
            bounds: HashMap::new(),
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
            bounds: HashMap::new(),
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
    // Floats are equality-eligible: structural equality bit-casts the float
    // and compares the bit pattern (bitwise/total semantics).
    assert_eq!(
        ty_is_eq_eligible(&Ty::Tuple(vec![Ty::I32, Ty::F64]), &checker.type_defs),
        EqEligibility::Eligible
    );
    assert_eq!(
        ty_is_eq_eligible(&with_float, &checker.type_defs),
        EqEligibility::Eligible
    );
    assert_eq!(
        ty_is_eq_eligible(&Ty::Tuple(vec![Ty::I32, Ty::String]), &checker.type_defs),
        EqEligibility::Eligible
    );
    assert_eq!(
        ty_is_eq_eligible(&Ty::Tuple(vec![Ty::I32, Ty::Bytes]), &checker.type_defs),
        EqEligibility::IneligibleManaged(Ty::Bytes)
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
fn generic_record_clone_concrete_instantiation_is_admissible() {
    // A generic record whose type params resolve to concrete,
    // clonable types is admissible — the per-mono clone thunk is synthesised
    // per instantiation.
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.type_defs.insert(
        "Pair".to_string(),
        TypeDef {
            kind: TypeDefKind::Record,
            name: "Pair".to_string(),
            type_params: vec!["A".to_string(), "B".to_string()],
            bounds: HashMap::new(),
            fields: HashMap::from([
                (
                    "a".to_string(),
                    Ty::Named {
                        name: "A".to_string(),
                        args: vec![],
                        builtin: None,
                    },
                ),
                (
                    "b".to_string(),
                    Ty::Named {
                        name: "B".to_string(),
                        args: vec![],
                        builtin: None,
                    },
                ),
            ]),
            variants: HashMap::new(),
            methods: HashMap::new(),
            doc_comment: None,
            field_order: vec!["a".to_string(), "b".to_string()],
            is_indirect: false,
        },
    );
    let span = Span::from(0..0);
    assert!(matches!(
        checker.record_clone_admissibility("Pair", &[Ty::I64, Ty::I64], &span),
        RecordCloneAdmissibility::Admissible
    ));
}

#[test]
fn generic_record_clone_opaque_instantiation_fails_closed() {
    // The opaque-leaf walk is substitution-aware: `Box<Handle>` instantiates
    // the declared field `item: T` to `item: Handle` before checking, so the
    // transitive opaque leaf is detected even though the declared field type
    // is the abstract param `T`.
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.user_opaque_type_names.insert("Handle".to_string());
    checker.type_defs.insert(
        "Handle".to_string(),
        TypeDef {
            kind: TypeDefKind::Struct,
            name: "Handle".to_string(),
            type_params: vec![],
            bounds: HashMap::new(),
            fields: HashMap::new(),
            variants: HashMap::new(),
            methods: HashMap::new(),
            doc_comment: None,
            field_order: vec![],
            is_indirect: true,
        },
    );
    checker.type_defs.insert(
        "Box".to_string(),
        TypeDef {
            kind: TypeDefKind::Record,
            name: "Box".to_string(),
            type_params: vec!["T".to_string()],
            bounds: HashMap::new(),
            fields: HashMap::from([(
                "item".to_string(),
                Ty::Named {
                    name: "T".to_string(),
                    args: vec![],
                    builtin: None,
                },
            )]),
            variants: HashMap::new(),
            methods: HashMap::new(),
            doc_comment: None,
            field_order: vec!["item".to_string()],
            is_indirect: false,
        },
    );
    let span = Span::from(0..0);
    let handle = Ty::Named {
        name: "Handle".to_string(),
        args: vec![],
        builtin: None,
    };
    assert!(matches!(
        checker.record_clone_admissibility("Box", std::slice::from_ref(&handle), &span),
        RecordCloneAdmissibility::OpaqueField { .. }
    ));
}

#[test]
fn generic_record_clone_unresolved_var_is_nyi() {
    // An unresolved receiver (a generic record whose type args are still
    // inference vars) keeps the `GenericRecord` NYI diagnostic; the
    // substitution-aware opaque walk must not regress this clean reject.
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.type_defs.insert(
        "Pair".to_string(),
        TypeDef {
            kind: TypeDefKind::Record,
            name: "Pair".to_string(),
            type_params: vec!["A".to_string(), "B".to_string()],
            bounds: HashMap::new(),
            fields: HashMap::from([
                (
                    "a".to_string(),
                    Ty::Named {
                        name: "A".to_string(),
                        args: vec![],
                        builtin: None,
                    },
                ),
                (
                    "b".to_string(),
                    Ty::Named {
                        name: "B".to_string(),
                        args: vec![],
                        builtin: None,
                    },
                ),
            ]),
            variants: HashMap::new(),
            methods: HashMap::new(),
            doc_comment: None,
            field_order: vec!["a".to_string(), "b".to_string()],
            is_indirect: false,
        },
    );
    let span = Span::from(0..0);
    let args = [Ty::Var(crate::ty::TypeVar(0)), Ty::I64];
    assert!(matches!(
        checker.record_clone_admissibility("Pair", &args, &span),
        RecordCloneAdmissibility::GenericRecord
    ));
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
fn vec_contains_float_record_now_typechecks() {
    // A record with an `f32` field is equality-eligible (floats compare on
    // their bit pattern) and Copy, so `Vec::contains` is admitted.
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
        output.errors.is_empty(),
        "float-record Vec::contains should compile under bitwise float equality: {:#?}",
        output.errors
    );
}

#[test]
fn vec_contains_layout_managed_record_rejected_with_eq_eligibility_diagnostic() {
    let output = check_source(
        r"
        type Packet { data: bytes }

        fn has_packet(values: Vec<Packet>, needle: Packet) -> bool {
            values.contains(needle)
        }
        ",
    );

    assert!(
        output.errors.iter().any(|error| {
            error.message.contains("`Vec::contains`")
                && error.message.contains("layout-managed/non-Copy")
                && error.message.contains("bytes")
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
fn vec_generic_bitcopy_record_methods_route_to_plain_layout_abi() {
    let output = check_source(
        r"
        type Wrap<T> { v: T }
        type Pair<A, B> { a: A, b: B }
        type Point { x: i64, y: i64 }
        type Holder<T> { value: T }

        fn main() {
            var wraps: Vec<Wrap<i64>> = [];
            wraps.push(Wrap { v: 1 });
            wraps.set(0, Wrap { v: 2 });
            let _wg = wraps.get(0);
            let _wi = wraps[0];
            let _wp = wraps.pop();

            var pairs: Vec<Pair<i64, i64>> = [];
            pairs.push(Pair { a: 3, b: 4 });

            var holders: Vec<Holder<Point>> = [];
            holders.push(Holder { value: Point { x: 5, y: 6 } });

            var nested: Vec<Wrap<Wrap<i64>>> = [];
            nested.push(Wrap { v: Wrap { v: 7 } });
        }
        ",
    );

    assert!(
        output.errors.is_empty(),
        "concrete generic BitCopy record Vec operations must type-check: {:#?}",
        output.errors
    );

    for method in ["push", "set", "pop"] {
        assert!(
            output.resolved_calls.values().any(|call| {
                call.method_name == method
                    && call.target.symbol_name == format!("hew_vec_{method}_layout")
            }),
            "Vec<Wrap<i64>> {method} must route to the Plain layout ABI, got: {:#?}",
            output.resolved_calls
        );
    }
    // `get` is trait-routed (`<Vec<T> as Index>::get`) to the element-agnostic
    // fresh-owner choke point, NOT the per-element `_layout` getter.
    assert!(
        output.resolved_calls.values().any(|call| {
            call.method_name == "get" && call.target.symbol_name == "hew_vec_get_clone"
        }),
        "Vec<Wrap<i64>>::get must route to the hew_vec_get_clone intrinsic, got: {:#?}",
        output.resolved_calls
    );
    assert!(
        output
            .resolved_calls
            .values()
            .all(|call| !call.target.symbol_name.ends_with("_owned")),
        "BitCopy generic records must not route to owned Vec ABI: {:#?}",
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
fn user_method_on_builtin_result_wrapper_is_rejected() {
    // A user package may declare its own `type Result` and `impl` methods on
    // it (the sqlite ecosystem package does exactly this). An actor ask
    // (`await db.query(...)`) is typed as the builtin wrapper
    // `Result<UserResult, AskError>`. Calling the user method `free` on that
    // wrapper must be rejected: `free` is not part of the builtin `Result`
    // surface, and admitting it (via the bare-name `Result::free` key the user
    // impl registers in `fn_sigs`) produces an ill-typed call that passes the
    // wrapper aggregate where the inner success value is required — codegen
    // then rejects the LLVM module. The checker must reject this with an
    // `UndefinedMethod` diagnostic naming the builtin `Result<...>` receiver.
    let output = check_source(
        r#"
        type Result { handle: i64; }
        impl Result {
            fn free(self) {}
        }
        actor Db {
            receive fn query(sql: string) -> Result {
                Result { handle: 0 }
            }
        }
        fn main() {
            let db = spawn Db;
            let r = await db.query("SELECT 1");
            r.free();
        }
        "#,
    );

    assert!(
        output.errors.iter().any(|error| {
            error.kind == TypeErrorKind::UndefinedMethod
                && error.message.contains("no method `free`")
                && error.message.contains("Result<")
        }),
        "user method `free` on a builtin `Result<...>` ask-wrapper must be \
         rejected as UndefinedMethod; got: {:#?}",
        output.errors
    );
    // The colliding user method must NOT be recorded as a dispatch rewrite —
    // that is the ill-typed call codegen-front would reject.
    assert!(
        !output.method_call_rewrites.values().any(|rewrite| matches!(
            rewrite,
            MethodCallRewrite::RewriteToFunction { c_symbol, .. } if c_symbol == "Result::free"
        )),
        "no `Result::free` rewrite must be recorded for a builtin Result \
         receiver; got: {:#?}",
        output.method_call_rewrites
    );
}

#[test]
fn builtin_result_methods_resolve_on_actor_ask_wrapper() {
    // A user package may declare its own `type Result` with a method whose name
    // COLLIDES with the builtin surface but whose signature differs — here
    // `fn is_ok(self) -> i64`. Both land in `fn_sigs` under the bare key
    // `Result::is_ok`. An actor ask (`await d.process(5)`) is typed as the
    // builtin wrapper `Result<i64, AskError>`. Calling `r.is_ok()` on that
    // builtin receiver must resolve to the BUILTIN `bool`-returning method, not
    // the user `i64`-returning one.
    //
    // Resolution must be origin-based, not a name allowlist: `is_ok` IS a
    // builtin method name, so a name-only gate would still consult `fn_sigs`
    // and select the colliding user `is_ok` — the `let ok: bool` annotation
    // then fails with `found i64`. Confining the lookup to the stdlib snapshot
    // for builtin `Result`/`Option` receivers selects the builtin method for
    // ALL method names.
    let output = check_source(
        r"
        type Result { handle: i64; }
        impl Result {
            fn is_ok(self) -> i64 { self.handle }
        }
        actor Doubler {
            receive fn process(n: i64) -> i64 { n * 2 }
        }
        fn main() {
            let d = spawn Doubler;
            let r = await d.process(5);
            let ok: bool = r.is_ok();
            let v = r.unwrap();
        }
        ",
    );

    assert!(
        output.errors.is_empty(),
        "builtin Result methods on an actor-ask wrapper must resolve to the \
         builtin surface even when a user `type Result` declares a colliding \
         `is_ok` with a different return type; got: {:#?}",
        output.errors
    );
    // The `is_ok` call must lower to the builtin structured marker, never the
    // user `Result::is_ok` method key. A user-method rewrite here is the
    // ill-typed call codegen-front would reject.
    assert!(
        output.method_call_rewrites.values().any(|rewrite| matches!(
            rewrite,
            MethodCallRewrite::BuiltinOptionResult {
                method: OptionResultMethod::ResultIsOk
            }
        )),
        "`r.is_ok()` on a builtin Result receiver must lower to \
         the structured ResultIsOk marker; got: {:#?}",
        output.method_call_rewrites
    );
    assert!(
        !output.method_call_rewrites.values().any(|rewrite| matches!(
            rewrite,
            MethodCallRewrite::RewriteToFunction { c_symbol, .. } if c_symbol == "Result::is_ok"
        )),
        "no user `Result::is_ok` rewrite must be recorded for a builtin Result \
         receiver; got: {:#?}",
        output.method_call_rewrites
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
            bounds: HashMap::new(),
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
            bounds: HashMap::new(),
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
            args: vec![Ty::Array(Box::new(Ty::String), 4)],
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
fn imported_module_record_seeds_send_marker_for_actor_ask_reply() {
    // An imported package actor whose `ask` replies with an imported `pub type`
    // record is gated on the reply being `Send` at the dispatch site
    // (`record_actor_method_dispatch`, `E_DUPLEX_NON_SEND`). Send derives from
    // the named type's structural member set; imported module types are
    // registered through `pre_register_type_decl`, which must seed the
    // trait-registry member set so the derivation resolves through the
    // imported record's fields instead of conservatively failing on an unknown
    // name. The record is named `Result` to mirror the prelude-shadowing
    // fixture (`tests/pkg-import/imported_actor_ask_record.hew`) that surfaced
    // the over-rejection: a plainly-Send `i64`-field record was rejected
    // because the importer's registry had no entry under the bare name.
    let parsed = hew_parser::parse(
        r"
        pub type Result {
            handle: i64
        }

        fn helper() {}",
    );
    assert!(
        parsed.errors.is_empty(),
        "module parse errors: {:?}",
        parsed.errors
    );

    let root_id = ModuleId::root();
    let mod_id = ModuleId::new(vec!["testffi".to_string()]);
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
    let _ = checker.check_program(&program);

    let result_ty = Ty::Named {
        builtin: None,
        name: "Result".to_string(),
        args: vec![],
    };
    assert!(
        checker
            .registry
            .implements_marker(&result_ty, crate::traits::MarkerTrait::Send),
        "imported record `Result {{ handle: i64 }}` must derive Send so an \
         imported actor ask-reply is not spuriously rejected with \
         E_DUPLEX_NON_SEND"
    );
}

#[test]
fn same_bare_name_imported_replies_derive_send_per_module() {
    // Two imported packages each export a `pub type Reply`: one non-Send
    // (`Rc<i64>`), one Send (`i64`). The ask-reply Send gate
    // (`record_actor_method_dispatch`, `E_DUPLEX_NON_SEND`) derives Send from a
    // named type's structural member set. Keying that derivation on the bare
    // name `Reply` collides across the two modules — the trait registry's
    // `type_fields` is last-write-wins — so a Send lookup on the bare name reads
    // whichever module won the race: a non-Send reply could derive `Send` from
    // the other module's fields and slip the gate, reaching codegen where it
    // trips the D10 named-`Rc` fail-closed. The fix seeds the marker tables
    // under each type's module-qualified key (`badpkg.Reply`, `goodpkg.Reply`)
    // so the derivation is collision-free. Assert the registry derives the
    // qualified identities correctly: non-Send for the `Rc` module, Send for the
    // `i64` module — the load-bearing negative case the bare key cannot express.
    let bad = hew_parser::parse(
        r"
        pub type Reply {
            field: Rc<i64>
        }",
    );
    assert!(
        bad.errors.is_empty(),
        "badpkg parse errors: {:?}",
        bad.errors
    );
    let good = hew_parser::parse(
        r"
        pub type Reply {
            handle: i64
        }",
    );
    assert!(
        good.errors.is_empty(),
        "goodpkg parse errors: {:?}",
        good.errors
    );

    let root_id = ModuleId::root();
    let bad_id = ModuleId::new(vec!["badpkg".to_string()]);
    let good_id = ModuleId::new(vec!["goodpkg".to_string()]);
    let mut mg = ModuleGraph::new(root_id.clone());
    mg.add_module(Module {
        id: bad_id.clone(),
        items: bad.program.items,
        imports: vec![],
        source_paths: vec![],
        doc: None,
    })
    .unwrap();
    mg.add_module(Module {
        id: good_id.clone(),
        items: good.program.items,
        imports: vec![],
        source_paths: vec![],
        doc: None,
    })
    .unwrap();
    mg.topo_order = vec![bad_id, good_id, root_id];
    let program = Program {
        module_graph: Some(mg),
        items: vec![],
        module_doc: None,
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let _ = checker.check_program(&program);

    let qualified = |name: &str| Ty::Named {
        builtin: None,
        name: name.to_string(),
        args: vec![],
    };
    assert!(
        !checker
            .registry
            .implements_marker(&qualified("badpkg.Reply"), crate::traits::MarkerTrait::Send),
        "`badpkg.Reply {{ field: Rc<i64> }}` must NOT derive Send — its qualified \
         marker set must hold its OWN Rc field, not the sibling module's i64"
    );
    assert!(
        checker.registry.implements_marker(
            &qualified("goodpkg.Reply"),
            crate::traits::MarkerTrait::Send
        ),
        "`goodpkg.Reply {{ handle: i64 }}` must derive Send — its qualified marker \
         set must hold its OWN i64 field, immune to the bare-name collision"
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
    let local_pid_ty = Ty::local_pid(Ty::Named {
        builtin: None,
        name: "Worker".to_string(),
        args: vec![],
    });

    assert!(
        checker.reject_rc_collection_element("HashSet", &local_pid_ty, &(0..0)),
        "LocalPid<Worker> should pass RcFree collection admissibility even when Worker stores Rc"
    );
    assert!(
        !checker.errors.iter().any(|err| {
            err.kind == TypeErrorKind::UnsafeCollectionElement && err.message.contains("HashSet")
        }),
        "LocalPid<Worker> should not emit a HashSet UnsafeCollectionElement error, got: {:?}",
        checker.errors
    );
}

// ── W5.004 (F1a): `#[intrinsic]` floor-protocol placement gate (A605) ──────
//
// The `#[intrinsic("…")]` surface is compiler-internal-only. A declaration is
// accepted only inside a designated stdlib-floor module; anywhere else —
// including the user's root module — is a hard `E_INTRINSIC_OUTSIDE_FLOOR`
// error so no user-reachable path can wire itself to a compiler intrinsic.
