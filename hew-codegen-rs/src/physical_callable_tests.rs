//! Callable carrier and receiver execution below source closure production.

#[path = "physical_callable_execution_tests.rs"]
mod execution;

use super::*;
use hew_mir::physical::SemParamPassing;

fn exclusive_receiver() -> (PhysicalModule, CallableId) {
    let mut physical = physical(
        r"
        fn inspect(values: Vec<i64>) -> i64 { values.len() }
        fn main() -> i64 {
            var values: Vec<i64> = Vec.new();
            values.push(3);
            inspect(values)
        }
        ",
    );
    let callee = physical
        .callables
        .iter_mut()
        .find(|callee| callee.declaration.full_path() == "inspect")
        .unwrap();
    callee.params[0].passing = SemParamPassing::BorrowMut;
    callee.params[0].carrier = ParamCarrier::Indirect;
    let id = callee.id;
    for function in &mut physical.functions {
        for block in &mut function.blocks {
            if let PhysicalTerminator::Call { callee, args, .. } = &mut block.terminator {
                if *callee == id {
                    let ArgumentTransfer::Borrow(source) = args[0] else {
                        panic!("source fixture must borrow its vector");
                    };
                    args[0] = ArgumentTransfer::BorrowMut(source);
                }
            }
        }
    }
    (physical, id)
}

#[test]
fn exclusive_receiver_uses_caller_slot_at_o0_o2() {
    let (physical, id) = exclusive_receiver();
    for optimized in [false, true] {
        let ctx = Context::create();
        let llvm = llvm(&ctx, &physical);
        let symbol = emitted_symbol(&physical, &physical.callables[id.0 as usize]);
        let engine = engine(&llvm, optimized);
        type Inspect = unsafe extern "C" fn(*mut *mut c_void, *mut i64, *mut *mut c_void) -> i32;
        // SAFETY: the exclusive parameter receives the address of the caller's
        // live vector handle. The callee borrows it and returns a scalar only.
        unsafe {
            let original = hew_runtime::vec::hew_vec_new_i64();
            let mut value = original.cast::<c_void>();
            let mut result = -1;
            let mut fault = std::ptr::null_mut();
            let inspect = engine.get_function::<Inspect>(&symbol).unwrap();
            assert_eq!(
                inspect.call(&raw mut value, &raw mut result, &raw mut fault),
                0
            );
            assert_eq!(result, 0);
            assert!(fault.is_null());
            assert_eq!(value, original.cast());
            assert_eq!(hew_runtime::vec::hew_vec_len(original), 0);
            hew_runtime::vec::hew_vec_free(original);
        }
    }
}

#[test]
fn callable_carrier_and_masked_environments_follow_target_layout() {
    use hew_runtime::callable::HewCallableValue;
    use hew_types::CallableCapabilities;

    let function = ResolvedTy::Function {
        capabilities: CallableCapabilities::default(),
        params: vec![],
        ret: Box::new(ResolvedTy::Unit),
    };
    let closure = |captures| ResolvedTy::Closure {
        capabilities: CallableCapabilities::default(),
        params: vec![],
        ret: Box::new(ResolvedTy::Unit),
        captures,
    };
    let zero_sized = closure(vec![ResolvedTy::Unit]);
    let mut fields = vec![ResolvedTy::U8; 8];
    fields.push(ResolvedTy::I64);
    let aligned = closure(fields);
    for (triple, pointer_bytes) in [
        ("x86_64-unknown-linux-gnu", 8),
        ("x86_64-pc-windows-msvc", 8),
        ("aarch64-apple-darwin", 8),
        ("wasm32-wasip1", 4),
    ] {
        let target = physical_target_for_types(triple, [&function, &zero_sized, &aligned])
            .expect("callable layouts");
        for ty in [&function, &zero_sized, &aligned] {
            let carrier = target.layout(ty).unwrap();
            assert_eq!(carrier.size, pointer_bytes * 2, "{triple}");
            assert_eq!(u64::from(carrier.align), pointer_bytes, "{triple}");
        }
        let empty = target.environment_layout(&function).unwrap();
        assert_eq!((empty.size, empty.align), (0, 1));
        let zero_sized = target.environment_layout(&zero_sized).unwrap();
        assert_eq!((zero_sized.size, zero_sized.align), (1, 1));
        let aligned = target.environment_layout(&aligned).unwrap();
        assert_eq!((aligned.size, aligned.align), (24, 8), "{triple}");
        let PhysicalRepr::Struct(fields) = &aligned.repr else {
            panic!("environment struct");
        };
        assert_eq!(fields[0].size, 2, "ninth capture needs a second mask byte");
        let ctx = Context::create();
        let data = TargetData::create(&target.data_layout);
        let ty = llvm_type(&ctx, &aligned.repr).unwrap().into_struct_type();
        assert_eq!(data.offset_of_element(&ty, 9), Some(16));
    }
    let host = physical_target_for_types(&native_emission_triple(), [&function]).unwrap();
    let carrier = host.layout(&function).unwrap();
    assert_eq!(carrier.size, size_of::<HewCallableValue>() as u64);
    assert_eq!(carrier.align as usize, align_of::<HewCallableValue>());
}

fn vector_environment() -> PhysicalModule {
    use hew_mir::physical::PhysicalEnvironmentGlue;
    let mut physical = physical(
        r"fn main() -> i64 {
            var values: Vec<Vec<i64>> = Vec.new();
            values.len()
        }",
    );
    let field = physical
        .vector_glue
        .iter()
        .find_map(|glue| {
            matches!(glue.element.clone, Some(CloneAction::Vector(_))).then(|| glue.element.clone())
        })
        .unwrap();
    let ty = ResolvedTy::Closure {
        capabilities: hew_types::CallableCapabilities {
            call: hew_types::CallableCallMode::Read,
            clone: true,
        },
        params: vec![],
        ret: Box::new(ResolvedTy::Unit),
        captures: vec![field.ty.clone(), field.ty.clone()],
    };
    let ctx = Context::create();
    let data = TargetData::create(&physical.target.data_layout);
    let field_layout = physical.target.layout(&field.ty).unwrap().clone();
    let layout =
        callable_environment_layout(&ctx, &data, vec![field_layout.clone(), field_layout]).unwrap();
    physical
        .target
        .insert_environment_layout(ty.clone(), layout);
    physical.environment_glue.push(PhysicalEnvironmentGlue {
        ty,
        fields: vec![field.clone(), field],
        cloneable: true,
    });
    physical
}

#[repr(C)]
struct VectorEnvironment {
    mask: u8,
    first: *mut hew_runtime::vec::HewVec,
    second: *mut hew_runtime::vec::HewVec,
}

unsafe extern "C" fn unused_invoke(
    _: *mut c_void,
    _: *const *mut c_void,
    _: *mut c_void,
    _: *mut *mut c_void,
) -> i32 {
    0
}

#[test]
fn generated_environment_clone_separates_vectors_and_honours_partial_masks_at_o0_o2() {
    use hew_runtime::callable::{
        hew_callable_clone, hew_callable_drop, hew_callable_env_alloc, HewCallableDescriptor,
        HewCallableValue,
    };
    use hew_runtime::vec::{
        hew_vec_free, hew_vec_len, hew_vec_new_i64, hew_vec_push_i64, HewValueLayout,
    };

    let physical = vector_environment();
    for optimized in [false, true] {
        let ctx = Context::create();
        let llvm = environment_llvm(&ctx, &physical);
        let engine = engine(&llvm, optimized);
        // SAFETY: the getter returns a compiler-emitted immutable layout. The
        // repr(C) test object matches that target layout and outlives its owners.
        unsafe {
            let getter = engine
                .get_function::<unsafe extern "C" fn() -> *const HewValueLayout>(
                    "environment_layout",
                )
                .unwrap();
            let layout = &*getter.call();
            assert_eq!(layout.size, size_of::<VectorEnvironment>());
            assert_eq!(layout.align, align_of::<VectorEnvironment>());
            let descriptor = HewCallableDescriptor {
                environment: layout,
                invoke_borrow: Some(unused_invoke),
                invoke_once: unused_invoke,
            };
            for mask in [1, 3] {
                let mut source = HewCallableValue {
                    environment: hew_callable_env_alloc(&raw const descriptor),
                    descriptor: &raw const descriptor,
                };
                let fields = &mut *source.environment.cast::<VectorEnvironment>();
                fields.first = hew_vec_new_i64();
                hew_vec_push_i64(fields.first, 7);
                fields.second = if mask == 3 {
                    hew_vec_new_i64()
                } else {
                    std::ptr::dangling_mut()
                };
                fields.mask = mask;
                let mut output = std::mem::MaybeUninit::uninit();
                assert_eq!(
                    hew_callable_clone(&raw const source, output.as_mut_ptr()),
                    0
                );
                let mut copy = output.assume_init();
                assert_ne!(source.environment, copy.environment);
                let copied = &mut *copy.environment.cast::<VectorEnvironment>();
                assert_eq!(copied.mask, mask);
                assert_ne!(copied.first, fields.first);
                hew_vec_push_i64(copied.first, 9);
                assert_eq!(hew_vec_len(copied.first), 2);
                assert_eq!(hew_vec_len(fields.first), 1);
                if mask == 3 {
                    assert_ne!(copied.second, fields.second);
                    // A take clears only this logical owner. The detached vector
                    // remains usable after the rest of the environment is dropped.
                    let detached = copied.second;
                    copied.mask &= !2;
                    hew_callable_drop(&raw mut copy);
                    hew_vec_push_i64(detached, 11);
                    assert_eq!(hew_vec_len(detached), 1);
                    hew_vec_free(detached);
                } else {
                    hew_callable_drop(&raw mut copy);
                }
                assert!(copy.environment.is_null() && copy.descriptor.is_null());
                hew_callable_drop(&raw mut copy);
                hew_callable_drop(&raw mut source);
            }
        }
    }
}

fn environment_llvm<'ctx>(ctx: &'ctx Context, physical: &PhysicalModule) -> Module<'ctx> {
    let llvm = llvm(ctx, physical);
    let getter = llvm.add_function(
        "environment_layout",
        ctx.ptr_type(AddressSpace::default()).fn_type(&[], false),
        None,
    );
    let builder = ctx.create_builder();
    builder.position_at_end(ctx.append_basic_block(getter, "entry"));
    let descriptor = llvm
        .get_global(&super::super::callable::environment_descriptor_symbol(0))
        .unwrap();
    builder
        .build_return(Some(&descriptor.as_pointer_value()))
        .unwrap();
    llvm
}

thread_local! {
    static VECTOR_DROPS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    static NESTED_DROPS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

unsafe extern "C" fn counted_vector_drop(value: *mut hew_runtime::vec::HewVec) {
    VECTOR_DROPS.set(VECTOR_DROPS.get() + 1);
    // SAFETY: the generated environment transfers this live vector exactly once.
    unsafe { hew_runtime::vec::hew_vec_free_owned(value) };
}

unsafe extern "C" fn fail_nested_clone(_: *const c_void, _: *mut c_void) -> i32 {
    17
}
unsafe extern "C" fn count_nested_drop(_: *mut c_void) {
    NESTED_DROPS.set(NESTED_DROPS.get() + 1);
}

#[repr(C)]
struct NestedEnvironment {
    mask: u8,
    vector: *mut hew_runtime::vec::HewVec,
    callable: hew_runtime::callable::HewCallableValue,
}

#[test]
fn failed_nested_callable_clone_rolls_back_completed_fields_and_preserves_output_at_o0_o2() {
    use hew_runtime::callable::{
        hew_callable_clone, hew_callable_drop, hew_callable_env_alloc, HewCallableDescriptor,
        HewCallableValue,
    };
    use hew_runtime::vec::{hew_vec_len, hew_vec_new_i64, hew_vec_push_i64, HewValueLayout};

    let mut physical = vector_environment();
    let nested = ResolvedTy::Function {
        capabilities: hew_types::CallableCapabilities {
            call: hew_types::CallableCallMode::Read,
            clone: true,
        },
        params: vec![],
        ret: Box::new(ResolvedTy::Unit),
    };
    let target = physical_target_for_types(&physical.target.triple, [&nested]).unwrap();
    physical
        .target
        .insert_layout(nested.clone(), target.layout(&nested).unwrap().clone());
    let env = &mut physical.environment_glue[0];
    env.fields[1] = PhysicalValueRecipe {
        ty: nested.clone(),
        own: OwnKind::Owned,
        clone: Some(CloneAction::Callable),
        destroy: Some(DestroyAction::Callable),
    };
    let ResolvedTy::Closure { captures, .. } = &mut env.ty else {
        unreachable!()
    };
    captures[1] = nested;
    let ctx = Context::create();
    let data = TargetData::create(&physical.target.data_layout);
    let layout = callable_environment_layout(
        &ctx,
        &data,
        env.fields
            .iter()
            .map(|field| physical.target.layout(&field.ty).unwrap().clone())
            .collect(),
    )
    .unwrap();
    physical
        .target
        .insert_environment_layout(env.ty.clone(), layout);
    let nested_layout = HewValueLayout {
        size: 8,
        align: 8,
        ownership_kind: HewTypeOwnershipKind::LayoutManaged,
        clone_fn: Some(fail_nested_clone),
        drop_fn: Some(count_nested_drop),
    };
    let nested_descriptor = HewCallableDescriptor {
        environment: &raw const nested_layout,
        invoke_borrow: Some(unused_invoke),
        invoke_once: unused_invoke,
    };
    for optimized in [false, true] {
        VECTOR_DROPS.set(0);
        NESTED_DROPS.set(0);
        let ctx = Context::create();
        let llvm = environment_llvm(&ctx, &physical);
        let engine = engine(&llvm, optimized);
        let drop = llvm.get_function("hew_vec_free_owned").unwrap();
        engine.add_global_mapping(&drop, counted_vector_drop as *const () as usize);
        // SAFETY: the test installs one deliberately failing nested copy callback.
        // The generated outer callback owns rollback and must preserve output.
        unsafe {
            let getter = engine
                .get_function::<unsafe extern "C" fn() -> *const HewValueLayout>(
                    "environment_layout",
                )
                .unwrap();
            let layout = &*getter.call();
            assert_eq!(layout.size, size_of::<NestedEnvironment>());
            assert_eq!(layout.align, align_of::<NestedEnvironment>());
            let descriptor = HewCallableDescriptor {
                environment: layout,
                invoke_borrow: Some(unused_invoke),
                invoke_once: unused_invoke,
            };
            let mut source = HewCallableValue {
                environment: hew_callable_env_alloc(&raw const descriptor),
                descriptor: &raw const descriptor,
            };
            let fields = &mut *source.environment.cast::<NestedEnvironment>();
            fields.vector = hew_vec_new_i64();
            hew_vec_push_i64(fields.vector, 7);
            fields.callable = HewCallableValue {
                environment: hew_callable_env_alloc(&raw const nested_descriptor),
                descriptor: &raw const nested_descriptor,
            };
            fields.mask = 3;
            let sentinel = std::ptr::dangling_mut::<c_void>();
            let mut output = HewCallableValue {
                environment: sentinel,
                descriptor: std::ptr::null(),
            };
            assert_eq!(hew_callable_clone(&raw const source, &raw mut output), 17);
            assert_eq!(output.environment, sentinel);
            assert!(output.descriptor.is_null());
            assert_eq!(
                VECTOR_DROPS.get(),
                1,
                "completed vector copy must be rolled back"
            );
            assert_eq!(
                NESTED_DROPS.get(),
                0,
                "failed nested copy did not create an owner"
            );
            assert_eq!(fields.mask, 3);
            assert_eq!(hew_vec_len(fields.vector), 1);
            hew_callable_drop(&raw mut source);
            assert_eq!(VECTOR_DROPS.get(), 2);
            assert_eq!(NESTED_DROPS.get(), 1);
        }
    }
}
