//! Execution of verified semantic callable construction, receivers and cleanup.

use super::*;
use hew_sir as sir;
use hew_types::{CallableCallMode, CallableCapabilities, TypeFactContext, TypeFactService};

fn op(id: u32, kind: sir::SemOpKind, result: Option<(u32, ResolvedTy, OwnKind)>) -> sir::SemOp {
    sir::SemOp {
        id: sir::OpId(id),
        kind,
        results: result
            .into_iter()
            .map(|(id, ty, own)| sir::ValueDef {
                id: sir::ValueId(id),
                ty,
                own,
            })
            .collect(),
        provenance: sir::Provenance::Synthesized,
    }
}

fn operand(id: u32) -> sir::Operand {
    sir::Operand {
        value: sir::ValueId(id),
    }
}
fn boundary(id: u32, decision: sir::BoundaryDecision) -> sir::BoundaryOperand {
    sir::BoundaryOperand {
        operand: operand(id),
        decision,
    }
}
fn result(id: u32) -> sir::ValueDef {
    sir::ValueDef {
        id: sir::ValueId(id),
        ty: ResolvedTy::I64,
        own: OwnKind::None,
    }
}
fn drop_receiver(id: u32, receiver: u32) -> sir::SemOp {
    op(
        id,
        sir::SemOpKind::DestroyValue {
            value: operand(receiver),
        },
        None,
    )
}

fn function(callable: &sir::SemCallable, blocks: Vec<sir::SemBlock>) -> sir::SemFunction {
    sir::SemFunction {
        id: callable.function,
        callable: callable.id,
        declaration: callable.declaration.clone(),
        name: callable.symbol.clone(),
        source_origin: callable.source_origin.clone(),
        span: 0..0,
        params: vec![],
        return_ty: callable.signature.return_ty.clone(),
        entry: BlockId(0),
        bindings: vec![],
        places: vec![],
        blocks,
    }
}

fn counter_body(module: &sir::SemModule, mode: CallableCallMode, fault: bool) -> sir::SemFunction {
    let mut ops = vec![];
    if mode == CallableCallMode::Var {
        ops.push(op(
            0,
            sir::SemOpKind::StoreAssign {
                place: sir::PlaceId(0),
                value: operand(1),
            },
            None,
        ));
    }
    let load = if mode == CallableCallMode::Once {
        sir::SemOpKind::LoadTake {
            place: sir::PlaceId(0),
        }
    } else {
        sir::SemOpKind::LoadCopy {
            place: sir::PlaceId(0),
        }
    };
    ops.push(op(1, load, Some((2, ResolvedTy::I64, OwnKind::None))));
    if mode == CallableCallMode::Once {
        ops.push(drop_receiver(2, 0));
    }
    let terminator = if fault {
        sir::SemTerminator::Trap {
            kind: TrapKind::DivideByZero,
        }
    } else {
        sir::SemTerminator::Return {
            value: Some(boundary(2, sir::BoundaryDecision::Copy)),
        }
    };
    let mut body = function(
        &module.callables[1],
        vec![sir::SemBlock {
            id: BlockId(0),
            args: vec![],
            ops,
            terminator,
        }],
    );
    body.params = vec![
        sir::BlockArg {
            value: sir::ValueId(0),
            ty: module.closures[0].ty.clone(),
            own: if mode == CallableCallMode::Once {
                OwnKind::Owned
            } else {
                OwnKind::Guaranteed
            },
        },
        sir::BlockArg {
            value: sir::ValueId(1),
            ty: ResolvedTy::I64,
            own: OwnKind::None,
        },
    ];
    body.places.push(sir::PlaceDecl {
        id: sir::PlaceId(0),
        ty: ResolvedTy::I64,
        origin: sir::PlaceOrigin::Capture {
            environment: sir::ValueId(0),
            field: 0,
        },
    });
    body
}

fn factory_body(module: &sir::SemModule, weak_once: bool, invoke: bool) -> sir::SemFunction {
    let original = &module.closures[0].ty;
    let exposed = if weak_once {
        ResolvedTy::Function {
            params: vec![ResolvedTy::I64],
            ret: Box::new(ResolvedTy::I64),
            capabilities: CallableCapabilities {
                call: CallableCallMode::Once,
                clone: false,
            },
        }
    } else {
        original.clone()
    };
    let mut ops = vec![
        op(
            0,
            sir::SemOpKind::ConstI64(10),
            Some((0, ResolvedTy::I64, OwnKind::None)),
        ),
        op(
            1,
            sir::SemOpKind::ClosureMake {
                closure: sir::ClosureId(0),
                fields: vec![operand(0)],
            },
            Some((1, original.clone(), OwnKind::Owned)),
        ),
    ];
    let receiver = if weak_once {
        ops.push(op(
            2,
            sir::SemOpKind::CallableCoerce { source: operand(1) },
            Some((2, exposed.clone(), OwnKind::Owned)),
        ));
        2
    } else {
        1
    };
    if !invoke {
        return function(
            &module.callables[0],
            vec![sir::SemBlock {
                id: BlockId(0),
                args: vec![],
                ops,
                terminator: sir::SemTerminator::Return {
                    value: Some(boundary(receiver, sir::BoundaryDecision::Move)),
                },
            }],
        );
    }
    let (_, _, capabilities) = sir::callable_parts(&exposed).unwrap();
    let decision = match capabilities.call {
        CallableCallMode::Read => sir::BoundaryDecision::Borrow,
        CallableCallMode::Var => sir::BoundaryDecision::BorrowMut,
        CallableCallMode::Once => sir::BoundaryDecision::Move,
    };
    ops.push(op(
        3,
        sir::SemOpKind::ConstI64(42),
        Some((3, ResolvedTy::I64, OwnKind::None)),
    ));
    let cleanup = |id| {
        if capabilities.call == CallableCallMode::Once {
            vec![]
        } else {
            vec![drop_receiver(id, receiver)]
        }
    };
    function(
        &module.callables[0],
        vec![
            sir::SemBlock {
                id: BlockId(0),
                args: vec![],
                ops,
                terminator: sir::SemTerminator::IndirectCall {
                    id: sir::OpId(4),
                    callee: boundary(receiver, decision),
                    signature: sir::callable_value_signature(&exposed, &module.type_facts).unwrap(),
                    args: vec![boundary(3, sir::BoundaryDecision::Copy)],
                    result: sir::CallResult::Value(result(4)),
                    normal: Some(sir::Edge {
                        target: BlockId(1),
                        args: vec![operand(4)],
                    }),
                    unwind: sir::CallUnwind::Cleanup(sir::Edge {
                        target: BlockId(2),
                        args: vec![],
                    }),
                },
            },
            sir::SemBlock {
                id: BlockId(1),
                args: vec![sir::BlockArg {
                    value: sir::ValueId(5),
                    ty: ResolvedTy::I64,
                    own: OwnKind::None,
                }],
                ops: cleanup(5),
                terminator: sir::SemTerminator::Return {
                    value: Some(boundary(5, sir::BoundaryDecision::Copy)),
                },
            },
            sir::SemBlock {
                id: BlockId(2),
                args: vec![],
                ops: cleanup(6),
                terminator: sir::SemTerminator::ResumeUnwind,
            },
        ],
    )
}

fn counter_module(
    mode: CallableCallMode,
    weak_once: bool,
    fault: bool,
    invoke: bool,
) -> sir::SemModule {
    let ty = ResolvedTy::Closure {
        params: vec![ResolvedTy::I64],
        ret: Box::new(ResolvedTy::I64),
        captures: vec![ResolvedTy::I64],
        capabilities: CallableCapabilities {
            call: mode,
            clone: true,
        },
    };
    let exposed = if weak_once {
        ResolvedTy::Function {
            params: vec![ResolvedTy::I64],
            ret: Box::new(ResolvedTy::I64),
            capabilities: CallableCapabilities {
                call: CallableCallMode::Once,
                clone: false,
            },
        }
    } else {
        ty.clone()
    };
    let parent = sir::SemCallable {
        id: CallableId(0),
        function: hew_hir::ItemId(0),
        declaration: hew_types::DefId::for_test("make_counter"),
        instance: sir::CallableInstance::Monomorphic,
        symbol: "make_counter".into(),
        source_origin: sir::FunctionSourceOrigin::Unknown,
        signature: sir::SemSignature {
            params: vec![],
            return_ty: if invoke {
                ResolvedTy::I64
            } else {
                exposed.clone()
            },
        },
        call_conv: sir::SemCallConv::Default,
        kind: sir::SemCallableKind::HewDirect,
    };
    let body = sir::SemCallable {
        id: CallableId(1),
        instance: sir::CallableInstance::Closure(sir::ClosureId(0)),
        symbol: "counter_body".into(),
        kind: sir::SemCallableKind::HewClosure,
        signature: sir::SemSignature {
            params: vec![
                sir::SemAbiParam {
                    ty: ty.clone(),
                    passing: match mode {
                        CallableCallMode::Read => SemParamPassing::Borrow,
                        CallableCallMode::Var => SemParamPassing::BorrowMut,
                        CallableCallMode::Once => SemParamPassing::Consume,
                    },
                    caller_visible_projection: mode != CallableCallMode::Once,
                },
                sir::SemAbiParam {
                    ty: ResolvedTy::I64,
                    passing: SemParamPassing::ReadOnly,
                    caller_visible_projection: false,
                },
            ],
            return_ty: ResolvedTy::I64,
        },
        ..parent.clone()
    };
    let mut facts = TypeFactService::new(TypeFactContext::default(), BTreeMap::new());
    for required in [&ResolvedTy::I64, &ResolvedTy::Unit, &ty, &exposed] {
        facts.require(required).unwrap();
    }
    let mut module = sir::SemModule {
        actors: Vec::new(),
        callables: vec![parent, body],
        type_facts: facts.rows().clone(),
        closures: vec![sir::SemClosure {
            id: sir::ClosureId(0),
            instance: sir::ClosureInstanceKey {
                enclosing: CallableId(0),
                literal: hew_hir::HirNodeId(7),
            },
            body: CallableId(1),
            ty,
            fields: vec![sir::SemCaptureField {
                binding: hew_hir::BindingId(0),
                ty: ResolvedTy::I64,
                access: if mode == CallableCallMode::Var {
                    hew_types::ClosureCaptureAccess::Var
                } else {
                    hew_types::ClosureCaptureAccess::Read
                },
                consumption: if mode == CallableCallMode::Once {
                    hew_types::ClosureCaptureConsumption::Consumed
                } else {
                    hew_types::ClosureCaptureConsumption::Retained
                },
            }],
        }],
        ..sir::SemModule::default()
    };
    module.functions = vec![
        factory_body(&module, weak_once, invoke),
        counter_body(&module, mode, fault),
    ];
    assert!(
        sir::verify_module(&module).is_empty(),
        "{:?}",
        sir::verify_module(&module)
    );
    module
}

fn lower_counter(
    mode: CallableCallMode,
    weak_once: bool,
    fault: bool,
    invoke: bool,
) -> PhysicalModule {
    let semantic = counter_module(mode, weak_once, fault, invoke);
    let target = physical_target_for_inventory(
        &native_emission_triple(),
        &hew_mir::physical::physical_type_inventory(&semantic),
    )
    .unwrap();
    hew_mir::lower_physical_module(&semantic, target)
        .unwrap()
        .into_unverified()
}

thread_local! { static CALLABLE_DROPS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) }; }
unsafe extern "C" fn counted_callable_drop(value: *mut hew_runtime::callable::HewCallableValue) {
    CALLABLE_DROPS.set(CALLABLE_DROPS.get() + 1);
    // SAFETY: invoked by compiler-generated destruction with a live owning slot.
    unsafe { hew_runtime::callable::hew_callable_drop(value) };
}

fn counter_engine<'ctx>(llvm: &Module<'ctx>, optimized: bool) -> ExecutionEngine<'ctx> {
    let engine = engine(llvm, optimized);
    if let Some(drop) = llvm.get_function("hew_callable_drop") {
        engine.add_global_mapping(&drop, counted_callable_drop as *const () as usize);
    }
    engine
}

#[test]
fn verified_mutable_closure_escapes_and_copies_private_state_at_o0_o2() {
    use hew_runtime::callable::{hew_callable_clone, hew_callable_drop, HewCallableValue};
    let physical = lower_counter(CallableCallMode::Var, false, false, false);
    for optimized in [false, true] {
        let ctx = Context::create();
        let llvm = llvm(&ctx, &physical);
        let engine = counter_engine(&llvm, optimized);
        let symbol = emitted_symbol(&physical, &physical.callables[0]);
        // SAFETY: the verified factory writes an owning carrier using the private
        // out/fault ABI; its descriptor provides this exact erased scalar ABI.
        unsafe {
            let factory = engine.get_function::<unsafe extern "C" fn(*mut HewCallableValue, *mut *mut c_void) -> i32>(&symbol).unwrap();
            let mut owner = std::mem::MaybeUninit::uninit();
            let mut fault = std::ptr::null_mut();
            assert_eq!(factory.call(owner.as_mut_ptr(), &raw mut fault), 0);
            assert!(fault.is_null());
            let mut owner = owner.assume_init();
            let mut copy = std::mem::MaybeUninit::uninit();
            assert_eq!(hew_callable_clone(&raw const owner, copy.as_mut_ptr()), 0);
            let mut copy = copy.assume_init();
            assert_ne!(owner.environment, copy.environment);
            for (value, replacement) in [(&owner, 41_i64), (&copy, 17)] {
                let mut replacement = replacement;
                let args = [(&raw mut replacement).cast::<c_void>()];
                let mut output = -1_i64;
                let invoke = (*value.descriptor).invoke_borrow.unwrap();
                assert_eq!(
                    hew_runtime::coro_root::hew_coro_run_callable(
                        invoke,
                        value.environment,
                        args.as_ptr(),
                        (&raw mut output).cast(),
                        &raw mut fault
                    ),
                    0
                );
                assert_eq!(output, replacement);
                assert!(fault.is_null());
            }
            #[repr(C)]
            struct Counter {
                mask: u8,
                value: i64,
            }
            assert_eq!((*owner.environment.cast::<Counter>()).value, 41);
            assert_eq!((*copy.environment.cast::<Counter>()).value, 17);
            assert_eq!((*owner.environment.cast::<Counter>()).mask, 1);
            hew_callable_drop(&raw mut copy);
            hew_callable_drop(&raw mut owner);
        }
    }
}

#[test]
fn verified_indirect_once_calls_drop_once_and_preserve_fault_results_at_o0_o2() {
    for (mode, weakened) in [
        (CallableCallMode::Once, false),
        (CallableCallMode::Var, true),
    ] {
        for faulting in [false, true] {
            let physical = lower_counter(mode, weakened, faulting, true);
            for optimized in [false, true] {
                CALLABLE_DROPS.set(0);
                let ctx = Context::create();
                let llvm = llvm(&ctx, &physical);
                let engine = counter_engine(&llvm, optimized);
                let symbol = emitted_symbol(&physical, &physical.callables[0]);
                // SAFETY: the verified runner has no source parameters and uses
                // the exact scalar result/fault private ABI.
                unsafe {
                    let run = engine
                        .get_function::<unsafe extern "C" fn(*mut i64, *mut *mut c_void) -> i32>(
                            &symbol,
                        )
                        .unwrap();
                    let mut result = -77;
                    let mut fault = std::ptr::null_mut();
                    let status = run.call(&raw mut result, &raw mut fault);
                    if faulting {
                        assert_ne!(status, 0);
                        assert_eq!(result, -77);
                        assert!(!fault.is_null());
                        hew_runtime::fault::hew_fault_drop(fault.cast());
                    } else {
                        assert_eq!(status, 0);
                        assert_eq!(result, if weakened { 42 } else { 10 });
                        assert!(fault.is_null());
                    }
                    assert_eq!(
                        CALLABLE_DROPS.get(),
                        1,
                        "receiver must be disposed on exactly one ownership path"
                    );
                }
            }
        }
    }
}

#[test]
fn source_callable_values_escape_copy_and_invoke_at_o0_o2() {
    let cases = [
        ("function value", "fn add(value: i64) -> i64 { value + 1 } fn main() -> i64 { let invoke = add; invoke(41) }", 42),
        ("escaped snapshot", "fn snapshot() -> fn() -> i64 { var count: i64 = 10; let read = || count; count = 99; read } fn main() -> i64 { let read = snapshot(); read() }", 10),
        ("copied counter", "fn counter() -> fn[var, clone]() -> i64 { let count: i64 = 0; capture(var count) || { count = count + 1; count } } fn main() -> i64 { var first = counter(); var second = first; first(); first() * 10 + second() }", 21),
        ("weakened once", "fn main() -> i64 { let base: i64 = 41; let invoke: fn[once]() -> i64 = || base + 1; invoke() }", 42),
    ];
    for (name, source, expected) in cases {
        assert_source_outcome(name, source, Some(expected));
    }
}

#[test]
fn source_nested_owners_and_consumed_captures_survive_at_o0_o2() {
    let cases = [
        ("nested owned escape", r#"fn factory() -> fn() -> fn() -> string { let word = "Ready"; || { let local = word; || local } } fn detached() -> fn() -> string { let outer = factory(); outer() } fn main() -> i64 { let inner = detached(); if inner() == "Ready" { 42 } else { 0 } }"#, 42),
        ("consumed captured callable", "fn answer() -> i64 { 42 } fn main() -> i64 { let callback: fn[once]() -> i64 = answer; let outer = move || callback(); outer() }", 42),
        ("remaining field after take", "fn answer() -> i64 { 41 } fn increment(value: i64) -> i64 { value + 1 } fn main() -> i64 { let once: fn[once]() -> i64 = answer; let read = increment; let outer = move || { let value = once(); read(value) }; outer() }", 42),
    ];
    for (name, source, expected) in cases {
        assert_source_outcome(name, source, Some(expected));
    }
}

#[test]
fn source_argument_fault_releases_capture_loan_at_o0_o2() {
    assert_source_outcome("argument fault", "fn increment(value: i64) -> i64 { value + 1 } fn main() -> i64 { let callback = increment; let outer = move |divisor: i64| callback(100 / divisor); outer(0) }", None);
}

fn assert_source_outcome(name: &str, source: &str, expected: Option<i64>) {
    assert_source_cleanup(name, source, expected, None);
}

fn assert_source_cleanup(name: &str, source: &str, expected: Option<i64>, drops: Option<usize>) {
    let physical = physical(source);
    let main = physical
        .callables
        .iter()
        .find(|row| row.declaration.full_path() == "main")
        .unwrap();
    let symbol = emitted_symbol(&physical, main);
    for optimized in [false, true] {
        CALLABLE_DROPS.set(0);
        let ctx = Context::create();
        let llvm = llvm(&ctx, &physical);
        let engine = counter_engine(&llvm, optimized);
        // SAFETY: source main uses the verified scalar result/fault ABI.
        unsafe {
            let main = engine
                .get_function::<unsafe extern "C" fn(*mut i64, *mut *mut c_void) -> i32>(&symbol)
                .unwrap();
            let mut result = -77;
            let mut fault = std::ptr::null_mut();
            let status = main.call(&raw mut result, &raw mut fault);
            if let Some(expected) = expected {
                assert_eq!(status, 0, "{name}");
                assert!(fault.is_null(), "{name}");
                assert_eq!(result, expected, "{name}");
            } else {
                assert_ne!(status, 0, "{name}");
                assert_eq!(result, -77, "{name}");
                assert!(!fault.is_null(), "{name}");
                hew_runtime::fault::hew_fault_drop(fault.cast());
            }
        }
        if let Some(drops) = drops {
            assert_eq!(CALLABLE_DROPS.get(), drops, "{name}, optimized={optimized}");
        }
    }
}

#[test]
fn source_destructured_once_fields_preserve_siblings_and_drop_owners_at_o0_o2() {
    for capabilities in ["once", "once, clone"] {
        let source = format!("type Two {{ a: fn[{capabilities}]() -> i64, b: fn() -> i64 }} fn main() -> i64 {{ let first: i64 = 41; let second: i64 = 1; let value = Two {{ a: || first, b: || second }}; let Two {{ a, b }} = value; let result = a(); result + b() }}");
        // The sibling's erased type has no Clone guarantee, so the whole
        // record transfers even when the once field is cloneable.
        assert_source_cleanup(
            "destructured once field and sibling",
            &source,
            Some(42),
            Some(2),
        );
    }
}

#[test]
fn source_destructured_once_tuple_fields_preserve_siblings_at_o0_o2() {
    for capabilities in ["once", "once, clone"] {
        let source = format!("fn main() -> i64 {{ let first: i64 = 41; let second: i64 = 1; let callback: fn[{capabilities}]() -> i64 = || first; let value = (callback, || second); let (a, b) = value; let result = a(); result + b() }}");
        assert_source_outcome(
            "destructured once tuple field and sibling",
            &source,
            Some(42),
        );
    }
}

#[test]
fn source_destructured_once_fields_clean_up_argument_and_body_faults_at_o0_o2() {
    for capabilities in ["once", "once, clone"] {
        for (body, argument) in [
            ("first + divisor", "100 / zero()"),
            ("first / divisor", "0"),
        ] {
            let source = format!("type Two {{ a: fn[{capabilities}](i64) -> i64, b: fn() -> i64 }} fn zero() -> i64 {{ 0 }} fn main() -> i64 {{ let first: i64 = 41; let second: i64 = 1; let value = Two {{ a: |divisor: i64| {body}, b: || second }}; let Two {{ a, b }} = value; let result = a({argument}); result + b() }}");
            assert_source_cleanup(
                "destructured once field fault cleanup",
                &source,
                None,
                Some(2),
            );
        }
    }
}
