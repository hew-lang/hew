//! Real allocated captures survive zero iterations until lexical Local cleanup.

use super::*;
use hew_runtime::callable::HewCallableValue;
use std::cell::RefCell;

#[path = "../../hew-mir/src/physical_local_fixture.rs"]
mod fixture;

#[derive(Debug, PartialEq, Eq)]
enum Event {
    Observe,
    Release(usize),
}
#[derive(Default)]
struct Trace {
    live: BTreeMap<usize, usize>,
    events: Vec<Event>,
    marker: usize,
}
thread_local! { static TRACE: RefCell<Trace> = RefCell::default(); }

unsafe extern "C" fn release(pointer: *mut c_void) {
    let live = TRACE.with_borrow_mut(|trace| {
        trace.events.push(Event::Release(pointer as usize));
        let Some(count) = trace.live.get_mut(&(pointer as usize)) else {
            return false;
        };
        *count -= 1;
        if *count == 0 {
            trace.live.remove(&(pointer as usize));
        }
        true
    });
    if live {
        // SAFETY: the first release owns this registered runtime allocation.
        unsafe { hew_runtime::string::hew_string_drop(pointer.cast()) };
    }
}
unsafe extern "C" fn clone_string(pointer: *const c_void) -> *mut c_void {
    TRACE.with_borrow_mut(|trace| {
        *trace.live.get_mut(&(pointer as usize)).unwrap() += 1;
    });
    // SAFETY: the JIT borrows the registered string to retain one additional owner.
    unsafe { hew_runtime::string::hew_string_clone(pointer.cast()).cast() }
}
unsafe extern "C" fn length(pointer: *const c_void) -> i64 {
    TRACE.with_borrow_mut(|trace| {
        if pointer as usize == trace.marker {
            trace.events.push(Event::Observe);
        }
    });
    // SAFETY: the JIT supplies a live borrowed runtime string.
    unsafe { hew_runtime::string::hew_string_length(pointer.cast()) }
}
fn string(text: &str) -> *mut c_void {
    let mut pointer = std::ptr::null_mut();
    // SAFETY: text is UTF-8 and the output slot is uniquely writable.
    unsafe {
        hew_runtime::string::hew_string_literal_new(
            text.as_ptr(),
            u32::try_from(text.len()).unwrap(),
            &raw mut pointer,
        )
    };
    TRACE.with_borrow_mut(|trace| {
        assert!(trace.live.insert(pointer as usize, 1).is_none());
    });
    pointer.cast()
}
fn physical_fixture(semantic: &hew_sir::SemModule) -> PhysicalModule {
    let target = physical_target_for_inventory(
        &native_emission_triple(),
        &hew_mir::physical::physical_type_inventory(semantic),
    )
    .unwrap();
    hew_mir::lower_physical_module(semantic, target)
        .unwrap()
        .into_unverified()
}
fn symbol(module: &PhysicalModule, name: &str) -> String {
    emitted_symbol(
        module,
        module
            .callables
            .iter()
            .find(|callable| callable.declaration.full_path() == name)
            .unwrap(),
    )
}
fn bind_trace(llvm: &Module<'_>, engine: &ExecutionEngine<'_>) {
    engine.add_global_mapping(
        &llvm.get_function("hew_string_drop").unwrap(),
        release as *const () as usize,
    );
    if let Some(function) = llvm.get_function("hew_string_length") {
        engine.add_global_mapping(&function, length as *const () as usize);
    }
    if let Some(function) = llvm.get_function("hew_string_clone") {
        engine.add_global_mapping(&function, clone_string as *const () as usize);
    }
}
fn finish(expected: &[Event]) {
    let (events, live) = TRACE.with_borrow_mut(|trace| {
        (
            std::mem::take(&mut trace.events),
            std::mem::take(&mut trace.live),
        )
    });
    for (pointer, count) in &live {
        for _ in 0..*count {
            // SAFETY: each leaked registered reference remains owned by the test.
            unsafe { hew_runtime::string::hew_string_drop((*pointer as *mut c_void).cast()) };
        }
    }
    assert!(live.is_empty(), "leaked runtime strings: {live:?}");
    assert_eq!(events, expected);
}

#[test]
fn local_join_assignment_loans_and_reentrant_empty_lifetimes_execute_at_o0_o2() {
    for case in [
        fixture::Case::JoinEnd,
        fixture::Case::JoinAssign,
        fixture::Case::Borrow,
        fixture::Case::EmptyLoop,
        fixture::Case::ZeroSized,
    ] {
        let physical = physical_fixture(&fixture::module(case));
        let name = symbol(&physical, "probe");
        for optimized in [false, true] {
            let ctx = Context::create();
            let llvm = llvm(&ctx, &physical);
            llvm.get_function(&name)
                .unwrap()
                .set_linkage(Linkage::External);
            let engine = engine(&llvm, optimized);
            if !matches!(case, fixture::Case::ZeroSized) {
                bind_trace(&llvm, &engine);
            }
            for taken in [false, true] {
                let mut fault = std::ptr::null_mut();
                let mut empty = 0_u8;
                let owner = if matches!(case, fixture::Case::ZeroSized) {
                    (&raw mut empty).cast()
                } else {
                    string("local owner")
                };
                type Probe = unsafe extern "C" fn(*mut c_void, u8, *mut *mut c_void) -> i32;
                // SAFETY: strings use the direct pointer carrier; the empty record
                // uses an indirect zero-byte slot and the emitter never reads bytes.
                let status = unsafe {
                    engine.get_function::<Probe>(&name).unwrap().call(
                        owner,
                        u8::from(taken),
                        &raw mut fault,
                    )
                };
                assert_eq!(status, 0, "{case:?}, O2={optimized}, taken={taken}");
                assert!(fault.is_null());
                let count = if matches!(case, fixture::Case::ZeroSized) {
                    0
                } else if matches!(case, fixture::Case::JoinAssign) {
                    2
                } else {
                    1
                };
                finish(
                    &(0..count)
                        .map(|_| Event::Release(owner as usize))
                        .collect::<Vec<_>>(),
                );
            }
        }
    }
}

#[test]
fn allocated_capture_survives_zero_iterations_until_after_loop_work_at_o0_o2() {
    let physical = physical_fixture(&allocated_closure());
    let factory_symbol = symbol(&physical, "make");
    let probe_symbol = symbol(&physical, "probe");
    for optimized in [false, true] {
        let ctx = Context::create();
        let llvm = llvm(&ctx, &physical);
        for name in [&factory_symbol, &probe_symbol] {
            llvm.get_function(name)
                .unwrap()
                .set_linkage(Linkage::External);
        }
        let engine = engine(&llvm, optimized);
        bind_trace(&llvm, &engine);
        type Factory =
            unsafe extern "C" fn(*mut c_void, *mut HewCallableValue, *mut *mut c_void) -> i32;
        type Probe = unsafe extern "C" fn(
            *mut HewCallableValue,
            *mut c_void,
            u8,
            *mut i64,
            *mut *mut c_void,
        ) -> i32;
        for run in [false, true] {
            let capture = string("captured allocation");
            let marker = string("after loop");
            TRACE.with_borrow_mut(|trace| trace.marker = marker as usize);
            let mut callable = HewCallableValue {
                environment: std::ptr::null_mut(),
                descriptor: std::ptr::null(),
            };
            let mut fault = std::ptr::null_mut();
            // SAFETY: source signature and native carriers match these C slots.
            let status = unsafe {
                engine
                    .get_function::<Factory>(&factory_symbol)
                    .unwrap()
                    .call(capture, &raw mut callable, &raw mut fault)
            };
            assert_eq!(status, 0);
            assert!(fault.is_null());
            assert!(
                !callable.environment.is_null(),
                "the oracle requires an allocated capture"
            );
            TRACE.with_borrow(|trace| {
                assert!(trace.events.is_empty(), "factory released the capture")
            });
            let mut result = -1;
            // SAFETY: this consumes the allocated callable and independent marker.
            let status = unsafe {
                engine.get_function::<Probe>(&probe_symbol).unwrap().call(
                    &raw mut callable,
                    marker,
                    u8::from(run),
                    &raw mut result,
                    &raw mut fault,
                )
            };
            assert_eq!(status, 0, "O2={optimized}, run={run}");
            assert!(fault.is_null());
            assert_eq!(result, 10);
            finish(&if run {
                vec![
                    Event::Release(capture as usize),
                    Event::Observe,
                    Event::Release(marker as usize),
                ]
            } else {
                vec![
                    Event::Observe,
                    Event::Release(capture as usize),
                    Event::Release(marker as usize),
                ]
            });
        }
    }
}

#[test]
fn local_linear_trap_reclaims_representation_without_a_consume_call_at_o0_o2() {
    let physical = physical_fixture(&fixture::module(fixture::Case::LinearTrap));
    let name = symbol(&physical, "probe");
    for optimized in [false, true] {
        let ctx = Context::create();
        let llvm = llvm(&ctx, &physical);
        llvm.get_function(&name)
            .unwrap()
            .set_linkage(Linkage::External);
        let engine = engine(&llvm, optimized);
        bind_trace(&llvm, &engine);
        let payload = string("linear representation");
        let mut token = payload;
        let mut fault = std::ptr::null_mut();
        type Probe = unsafe extern "C" fn(*mut *mut c_void, u8, *mut *mut c_void) -> i32;
        // SAFETY: Token is the checked one-pointer record, consumed indirectly;
        // unit returns have no result slot in the private ABI.
        let status = unsafe {
            engine
                .get_function::<Probe>(&name)
                .unwrap()
                .call(&raw mut token, 0, &raw mut fault)
        };
        assert_ne!(status, 0);
        assert!(!fault.is_null());
        // SAFETY: the trap transfers one owned runtime fault to the caller.
        unsafe { hew_runtime::fault::hew_fault_drop(fault.cast()) };
        finish(&[Event::Release(payload as usize)]);
    }
}

#[allow(
    clippy::too_many_lines,
    reason = "keep the complete loop, observable work and unwind fixture together"
)]
fn allocated_closure() -> hew_sir::SemModule {
    use fixture::*;
    use hew_sir::*;
    let mut module = source("fn make(consume text: string) -> fn[once]() -> i64 { move || text.len() } fn probe(consume callback: fn[once]() -> i64, consume marker: string, run: bool) -> i64 { callback(); marker.len() } fn main() {}");
    let function = probe(&mut module);
    let ty = function.params[0].ty.clone();
    let mut invoke = function
        .blocks
        .iter()
        .find_map(|block| {
            matches!(block.terminator, SemTerminator::IndirectCall { .. })
                .then(|| block.terminator.clone())
        })
        .unwrap();
    let mut observe = function
        .blocks
        .iter()
        .find_map(|block| {
            matches!(block.terminator, SemTerminator::RtCall { .. })
                .then(|| block.terminator.clone())
        })
        .unwrap();
    let result = |id| ValueDef {
        id: ValueId(id),
        ty: ResolvedTy::I64,
        own: OwnKind::None,
    };
    let arg = |value, ty| BlockArg {
        value: ValueId(value),
        ty,
        own: OwnKind::None,
    };
    let SemTerminator::IndirectCall {
        callee,
        result: output,
        normal,
        unwind,
        ..
    } = &mut invoke
    else {
        unreachable!()
    };
    *callee = BoundaryOperand {
        operand: operand(3),
        decision: BoundaryDecision::Move,
    };
    *output = CallResult::Value(result(4));
    *normal = edge(3, &[4]);
    *unwind = CallUnwind::Cleanup(edge(6, &[]));
    let SemTerminator::RtCall {
        args,
        result: output,
        normal,
        unwind,
        ..
    } = &mut observe
    else {
        unreachable!()
    };
    assert_eq!(args.len(), 1);
    args[0].operand = operand(1);
    *output = CallResult::Value(result(7));
    *normal = edge(5, &[7]);
    assert!(matches!(unwind, CallUnwind::NotApplicable));
    function.bindings.clear();
    function.places = vec![PlaceDecl {
        id: PlaceId(0),
        ty: ty.clone(),
        origin: PlaceOrigin::Local,
    }];
    function.blocks = vec![
        block(0, vec![alloc(), init(0)], SemTerminator::Goto(edge(1, &[]))),
        block(1, vec![], branch(2, 2, 4)),
        block(
            2,
            vec![load(
                SemOpKind::LoadTake { place: PlaceId(0) },
                3,
                ty,
                OwnKind::Owned,
            )],
            invoke,
        ),
        // The consuming iteration breaks: a once callable cannot be called on
        // a second iteration. The untaken edge reaches the same after-loop work.
        SemBlock {
            args: vec![arg(5, ResolvedTy::I64)],
            ..block(3, vec![], SemTerminator::Goto(edge(4, &[])))
        },
        block(4, vec![], observe),
        SemBlock {
            args: vec![arg(8, ResolvedTy::I64)],
            ..block(
                5,
                vec![end(), destroy(1)],
                SemTerminator::Return {
                    value: Some(BoundaryOperand {
                        operand: operand(8),
                        decision: BoundaryDecision::Copy,
                    }),
                },
            )
        },
        block(6, vec![end(), destroy(1)], SemTerminator::ResumeUnwind),
    ];
    normalize(&mut module);
    check_module(&module).unwrap();
    module
}
