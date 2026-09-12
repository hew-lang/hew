type HewString = c_void;
use std::cell::RefCell;
use std::collections::BTreeSet;

#[derive(Default, Debug)]
struct Trace {
    live: BTreeSet<usize>,
    clones: usize,
    drops: Vec<usize>,
    children: Vec<usize>,
    invalid_release: bool,
}
thread_local! { static TRACE: RefCell<Trace> = RefCell::default(); }

fn register(value: *mut Value) -> *mut Value {
    if !value.is_null() {
        TRACE.with_borrow_mut(|trace| assert!(trace.live.insert(value as usize)));
    }
    value
}
fn remove(value: *mut Value) -> bool {
    TRACE.with_borrow_mut(|trace| {
        let owned = value.is_null() || trace.live.remove(&(value as usize));
        trace.invalid_release |= !owned;
        owned
    })
}
unsafe extern "C" fn trace_clone(value: *const Value) -> *mut Value {
    TRACE.with_borrow_mut(|trace| trace.clones += 1);
    // SAFETY: the physical copy borrows a live runtime value or invalid null.
    register(unsafe { raw_clone(value) })
}
unsafe extern "C" fn trace_free(value: *mut Value) {
    TRACE.with_borrow_mut(|trace| trace.drops.push(value as usize));
    if remove(value) {
        // SAFETY: remove establishes the one remaining owner, or a safe null.
        unsafe { raw_free(value) };
    }
}
unsafe extern "C" fn trace_object_set(value: *mut Value, key: *const HewString, child: *mut Value) {
    TRACE.with_borrow_mut(|trace| trace.children.push(child as usize));
    if remove(child) {
        // SAFETY: the mutation receives a live receiver/key and consumes child.
        unsafe { raw_object_set(value, key.cast(), child) };
    }
}
unsafe extern "C" fn trace_array_push(value: *mut Value, child: *mut Value) {
    TRACE.with_borrow_mut(|trace| trace.children.push(child as usize));
    if remove(child) {
        // SAFETY: the mutation receives a live receiver and consumes child.
        unsafe { raw_array_push(value, child) };
    }
}
fn finish() {
    let trace = TRACE.take();
    for pointer in &trace.live {
        // SAFETY: any missing release leaves an allocation owned by this test.
        unsafe { raw_free(*pointer as *mut Value) };
    }
    assert!(
        !trace.invalid_release,
        "duplicate or foreign release: {trace:?}"
    );
    assert!(trace.live.is_empty(), "leaked encoding owners: {trace:?}");
}
fn string_from_str(text: &str) -> *mut HewString {
    let mut output = std::ptr::null_mut();
    // SAFETY: text is valid UTF-8 and the output slot is uniquely writable.
    unsafe {
        hew_runtime::string::hew_string_literal_new(
            text.as_ptr(),
            u32::try_from(text.len()).unwrap(),
            &raw mut output,
        )
    };
    output.cast()
}
unsafe fn string_release(value: *mut HewString) {
    // SAFETY: the caller transfers one owned managed string reference.
    unsafe { hew_runtime::string::hew_string_drop(value.cast()) };
}
fn parse(text: &str) -> *mut Value {
    let string = string_from_str(text);
    // SAFETY: parsing borrows the live managed string; the returned value is owned.
    let value = unsafe { raw_parse(string.cast()) };
    // SAFETY: parsing has finished borrowing this owned string.
    unsafe { string_release(string) };
    register(value)
}
fn call_body(
    semantic: &hew_sir::SemModule,
    optimized: bool,
    call: impl FnOnce(&ExecutionEngine<'_>, &str),
) {
    let physical = physical_fixture(semantic, &native_emission_triple());
    let name = emitted_symbol(&physical, &physical.callables[0]);
    let ctx = Context::create();
    let llvm = llvm(&ctx, &physical);
    llvm.get_function(&name)
        .unwrap()
        .set_linkage(Linkage::External);
    let engine = engine(&llvm, optimized);
    bind_runtime(&llvm, &engine);
    call(&engine, &name);
}
fn call_op(op: EncodingOp, optimized: bool, call: impl FnOnce(&ExecutionEngine<'_>, &str)) {
    call_body(
        &fixture::operation(RuntimeCallFamily::Encoding { format: FORMAT, op }),
        optimized,
        call,
    );
}
fn success(status: i32, fault: *mut c_void) {
    assert_eq!(status, 0);
    assert!(fault.is_null());
}

fn unit_member_module() -> hew_sir::SemModule {
    use hew_sir::{
        AggregateShapeRef, BoundaryDecision, BoundaryOperand, OwnKind, SemBlock, SemOpKind,
        SemTerminator,
    };
    let tuple = ResolvedTy::Tuple(vec![ResolvedTy::Unit, fixture::value(FORMAT)]);
    let mut semantic = fixture::skeleton(vec![fixture::value(FORMAT)], tuple.clone());
    semantic.functions[0].blocks = vec![SemBlock {
        id: hew_sir::BlockId(0),
        args: vec![],
        ops: vec![
            fixture::op(
                0,
                SemOpKind::ConstUnit,
                vec![fixture::result(1, ResolvedTy::Unit, OwnKind::None)],
            ),
            fixture::op(
                1,
                SemOpKind::AggregateMake {
                    shape: AggregateShapeRef::Tuple,
                    fields: vec![fixture::operand(1), fixture::operand(0)],
                },
                vec![fixture::result(2, tuple.clone(), OwnKind::Owned)],
            ),
            fixture::op(
                2,
                SemOpKind::CopyValue {
                    source: fixture::operand(2),
                },
                vec![fixture::result(3, tuple.clone(), OwnKind::Owned)],
            ),
            fixture::op(
                3,
                SemOpKind::DestroyValue {
                    value: fixture::operand(2),
                },
                vec![],
            ),
            fixture::op(
                4,
                SemOpKind::Destructure {
                    shape: AggregateShapeRef::Tuple,
                    aggregate: fixture::operand(3),
                },
                vec![
                    fixture::result(4, ResolvedTy::Unit, OwnKind::None),
                    fixture::result(5, fixture::value(FORMAT), OwnKind::Owned),
                ],
            ),
            fixture::op(
                5,
                SemOpKind::AggregateMake {
                    shape: AggregateShapeRef::Tuple,
                    fields: vec![fixture::operand(4), fixture::operand(5)],
                },
                vec![fixture::result(6, tuple, OwnKind::Owned)],
            ),
        ],
        terminator: SemTerminator::Return {
            value: Some(BoundaryOperand {
                operand: fixture::operand(6),
                decision: BoundaryDecision::Move,
            }),
        },
    }];
    hew_sir::check_module(&semantic).unwrap();
    semantic
}

#[test]
fn unit_member_preserves_recursive_encoding_glue_and_aggregate_return_at_o0_o2() {
    let semantic = unit_member_module();
    for optimized in [false, true] {
        for invalid in [false, true] {
            let original = if invalid {
                std::ptr::null_mut()
            } else {
                register(raw_from_u64(u64::MAX))
            };
            let mut copy = std::ptr::null_mut();
            let mut fault = std::ptr::null_mut();
            type Pair = unsafe extern "C" fn(*mut Value, *mut *mut Value, *mut *mut c_void) -> i32;
            call_body(&semantic, optimized, |engine, name| {
                // SAFETY: native (Unit, Value) is an empty field followed by one
                // pointer, returned indirectly through the private result slot.
                success(
                    unsafe {
                        engine.get_function::<Pair>(name).unwrap().call(
                            original,
                            &raw mut copy,
                            &raw mut fault,
                        )
                    },
                    fault,
                );
            });
            assert_eq!(copy.is_null(), invalid);
            if !invalid {
                assert_ne!(copy, original);
                // SAFETY: the tuple result owns this independent copied value.
                assert_eq!(unsafe { raw_get_u64(copy) }, u64::MAX);
            }
            TRACE.with_borrow(|trace| {
                assert_eq!(trace.clones, 1);
                assert_eq!(trace.drops, [original as usize]);
            });
            // SAFETY: Unit needs no release; the test owns the returned value field.
            unsafe { trace_free(copy) };
            finish();
        }
    }
}

#[test]
fn copies_mutate_independently_and_transfer_children_once_at_o0_o2() {
    for optimized in [false, true] {
        let original = parse(r#"{"items":[1,2],"label":"original"}"#);
        let mut copy = std::ptr::null_mut();
        let mut fault = std::ptr::null_mut();
        type Copy = unsafe extern "C" fn(*const Value, *mut *mut Value, *mut *mut c_void) -> i32;
        call_body(&fixture::copy(FORMAT), optimized, |engine, name| {
            // SAFETY: checked pointer parameter and private owned-result ABI.
            success(
                unsafe {
                    engine.get_function::<Copy>(name).unwrap().call(
                        original,
                        &raw mut copy,
                        &raw mut fault,
                    )
                },
                fault,
            );
        });
        assert_ne!(copy, original);
        // SAFETY: both owners are live and distinct.
        assert_eq!(unsafe { raw_eq(original, copy) }, 1);
        let child = parse(r#"{"nested":[true,"changed"]}"#);
        let key = string_from_str("items");
        let mut updated = std::ptr::null_mut();
        type Set = unsafe extern "C" fn(
            *mut Value,
            *mut HewString,
            *mut Value,
            *mut *mut Value,
            *mut *mut c_void,
        ) -> i32;
        call_op(EncodingOp::ObjectSet, optimized, |engine, name| {
            // SAFETY: the body consumes both registered values and the key string.
            success(
                unsafe {
                    engine.get_function::<Set>(name).unwrap().call(
                        copy,
                        key,
                        child,
                        &raw mut updated,
                        &raw mut fault,
                    )
                },
                fault,
            );
        });
        assert_eq!(updated, copy);
        // SAFETY: mutation retained both top-level owners, altering only copy.
        assert_eq!(unsafe { raw_eq(original, updated) }, 0);
        TRACE.with_borrow(|trace| {
            assert_eq!(trace.clones, 1);
            assert_eq!(trace.children, [child as usize]);
            assert!(trace.drops.is_empty());
        });
        // SAFETY: both distinct owners remain with the test after the private return.
        unsafe {
            trace_free(original);
            trace_free(updated);
        }
        finish();
    }
}

#[test]
fn array_mutation_consumes_children_even_for_invalid_receivers_at_o0_o2() {
    for optimized in [false, true] {
        for input in ["[]", "1", "["] {
            let receiver = parse(input);
            let child = register(raw_from_u64(u64::MAX));
            let mut updated = std::ptr::null_mut();
            let mut fault = std::ptr::null_mut();
            type Push = unsafe extern "C" fn(
                *mut Value,
                *mut Value,
                *mut *mut Value,
                *mut *mut c_void,
            ) -> i32;
            call_op(EncodingOp::ArrayPush, optimized, |engine, name| {
                // SAFETY: checked consuming pointer parameters and private return slots.
                success(
                    unsafe {
                        engine.get_function::<Push>(name).unwrap().call(
                            receiver,
                            child,
                            &raw mut updated,
                            &raw mut fault,
                        )
                    },
                    fault,
                );
            });
            assert_eq!(updated, receiver);
            if input == "[]" {
                // SAFETY: updated owns the mutated array; selection returns a fresh owner.
                let element = register(unsafe { raw_array_get(updated, 0) });
                // SAFETY: element owns the inserted unsigned integer.
                assert_eq!(unsafe { raw_get_u64(element) }, u64::MAX);
                // SAFETY: the element is a separate owned selection.
                unsafe { trace_free(element) };
            }
            TRACE.with_borrow(|trace| {
                assert_eq!(trace.clones, 0);
                assert_eq!(trace.children, [child as usize]);
            });
            // SAFETY: the receiver is returned as an owner even when invalid/null.
            unsafe { trace_free(updated) };
            finish();
        }
    }
}

#[test]
fn local_cleanup_tracks_null_owners_separately_from_pointer_bits_at_o0_o2() {
    for optimized in [false, true] {
        for input in ["{\"nested\":[1,2]}", "["] {
            let original = parse(input);
            let mut copy = std::ptr::null_mut();
            let mut fault = std::ptr::null_mut();
            type Copy = unsafe extern "C" fn(*mut Value, *mut *mut Value, *mut *mut c_void) -> i32;
            call_body(&fixture::local_copy(FORMAT), optimized, |engine, name| {
                // SAFETY: the body consumes original into its Local and returns a deep copy.
                success(
                    unsafe {
                        engine.get_function::<Copy>(name).unwrap().call(
                            original,
                            &raw mut copy,
                            &raw mut fault,
                        )
                    },
                    fault,
                );
            });
            TRACE.with_borrow(|trace| {
                assert_eq!(trace.clones, 1);
                assert_eq!(trace.drops, [original as usize]);
            });
            assert_eq!(copy.is_null(), input == "[");
            // SAFETY: even an invalid null represents the returned logical owner.
            unsafe { trace_free(copy) };
            TRACE.with_borrow(|trace| assert_eq!(trace.drops.len(), 2));
            finish();
        }
    }
}

#[test]
fn scalar_and_managed_string_carriers_execute_at_o0_o2() {
    for optimized in [false, true] {
        macro_rules! scalar {
            ($ty:ty, $from:ident, $get:ident, $expected:expr) => {{
                let mut value = std::ptr::null_mut();
                let mut fault = std::ptr::null_mut();
                type From = unsafe extern "C" fn($ty, *mut *mut Value, *mut *mut c_void) -> i32;
                call_op(EncodingOp::$from, optimized, |engine, name| {
                    // SAFETY: checked scalar parameter and private owned pointer result.
                    success(
                        unsafe {
                            engine.get_function::<From>(name).unwrap().call(
                                $expected,
                                &raw mut value,
                                &raw mut fault,
                            )
                        },
                        fault,
                    );
                });
                register(value);
                let mut output: $ty = Default::default();
                type Get = unsafe extern "C" fn(*mut Value, *mut $ty, *mut *mut c_void) -> i32;
                call_op(EncodingOp::$get, optimized, |engine, name| {
                    // SAFETY: checked scalar result and consumed input owner.
                    success(
                        unsafe {
                            engine.get_function::<Get>(name).unwrap().call(
                                value,
                                &raw mut output,
                                &raw mut fault,
                            )
                        },
                        fault,
                    );
                });
                assert_eq!(output, $expected);
            }};
        }
        scalar!(i64, FromInt, GetInt, i64::MIN);
        scalar!(f64, FromFloat, GetFloat, -12345.25);
        scalar!(i32, FromBool, GetBool, 1);
        let mut value = std::ptr::null_mut();
        let mut fault = std::ptr::null_mut();
        type FromU64 = unsafe extern "C" fn(u64, *mut *mut Value, *mut *mut c_void) -> i32;
        call_op(EncodingOp::FromU64, optimized, |engine, name| {
            // SAFETY: verified native u64 carrier and private pointer result slot.
            success(
                unsafe {
                    engine.get_function::<FromU64>(name).unwrap().call(
                        u64::MAX,
                        &raw mut value,
                        &raw mut fault,
                    )
                },
                fault,
            );
        });
        register(value);
        let mut integer = 0;
        type GetU64 = unsafe extern "C" fn(*mut Value, *mut u64, *mut *mut c_void) -> i32;
        call_op(EncodingOp::GetU64, optimized, |engine, name| {
            // SAFETY: this body borrows then releases its consumed input owner.
            success(
                unsafe {
                    engine.get_function::<GetU64>(name).unwrap().call(
                        value,
                        &raw mut integer,
                        &raw mut fault,
                    )
                },
                fault,
            );
        });
        assert_eq!(integer, u64::MAX);
        let text = "héllo 🦀";
        let input = string_from_str(text);
        type FromString =
            unsafe extern "C" fn(*mut HewString, *mut *mut Value, *mut *mut c_void) -> i32;
        call_op(EncodingOp::FromString, optimized, |engine, name| {
            // SAFETY: this checked body consumes and releases the managed input string.
            success(
                unsafe {
                    engine.get_function::<FromString>(name).unwrap().call(
                        input,
                        &raw mut value,
                        &raw mut fault,
                    )
                },
                fault,
            );
        });
        register(value);
        let mut output = std::ptr::null_mut();
        type GetString =
            unsafe extern "C" fn(*mut Value, *mut *mut HewString, *mut *mut c_void) -> i32;
        call_op(EncodingOp::GetString, optimized, |engine, name| {
            // SAFETY: the checked body consumes value and returns an owned managed string.
            success(
                unsafe {
                    engine.get_function::<GetString>(name).unwrap().call(
                        value,
                        &raw mut output,
                        &raw mut fault,
                    )
                },
                fault,
            );
        });
        // SAFETY: output is the live managed string returned by the runtime.
        let expected = string_from_str(text);
        assert_eq!(
            unsafe { hew_runtime::string::hew_string_equals(output.cast(), expected.cast()) },
            1
        );
        // SAFETY: the test owns the expected string.
        unsafe { string_release(expected) };
        // SAFETY: the test owns the returned string.
        unsafe { string_release(output) };
        finish();
    }
}
