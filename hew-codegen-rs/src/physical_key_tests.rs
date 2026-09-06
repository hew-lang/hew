use std::ffi::c_void;

use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_types::{module_registry::ModuleRegistry, Checker};
use inkwell::execution_engine::{ExecutionEngine, JitFunction};

use super::*;

type HashCallback = unsafe extern "C" fn(*const c_void, *mut u64, *mut *mut c_void) -> i32;
type EqCallback =
    unsafe extern "C" fn(*const c_void, *const c_void, *mut bool, *mut *mut c_void) -> i32;

fn physical(source: &str) -> PhysicalModule {
    physical_for_triple(source, &native_emission_triple())
}

fn physical_for_triple(source: &str, triple: &str) -> PhysicalModule {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let checked = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
    let hir = lower_program_host_target(&parsed.program, &checked, &ResolutionCtx);
    assert!(hir.diagnostics.is_empty(), "{:?}", hir.diagnostics);
    let lowered = hew_sir::lower_module(&hir.module, &checked);
    assert!(
        lowered.statuses.iter().any(|status| status.name == "main"
            && matches!(status.status, hew_sir::SirLoweringStatus::Lowered)),
        "{:?}",
        lowered.statuses
    );
    assert!(hew_sir::verify_module(&lowered.module).is_empty());
    let target = physical_target_for_inventory(
        triple,
        &hew_mir::physical::physical_type_inventory(&lowered.module),
    )
    .unwrap();
    hew_mir::lower_physical_module(&lowered.module, target)
        .unwrap()
        .module()
        .clone()
}

fn selected_symbol(
    module: &PhysicalModule,
    ty: &ResolvedTy,
    capability: ValueCapability,
) -> String {
    let index = module
        .value_capabilities
        .keys()
        .position(|key| key == &(ty.clone(), capability))
        .unwrap();
    format!("__hew_value_callback_{index}")
}

fn llvm<'ctx>(ctx: &'ctx Context, physical: &PhysicalModule) -> Module<'ctx> {
    let machine = crate::llvm::target_machine_for_triple_with_opt_level(
        &physical.target.triple,
        OptLevel::O0,
    )
    .unwrap();
    build_module(ctx, physical, "key_callbacks", &machine).unwrap()
}

// Exercise a physical recipe below source admission while retaining the exact
// component callback table that came from the checked source fixture.
fn add_recipe_callback<'ctx>(
    ctx: &'ctx Context,
    llvm: Module<'ctx>,
    physical: &PhysicalModule,
    name: &str,
    capability: ValueCapability,
    emit: impl FnOnce(
        &mut SelectedValueEmitter<'_, 'ctx, '_>,
        PointerValue<'ctx>,
        Option<PointerValue<'ctx>>,
    ) -> CodegenResult<()>,
) -> Module<'ctx> {
    let callbacks = physical
        .value_capabilities
        .keys()
        .map(|(ty, cap)| {
            (
                (ty.clone(), *cap),
                llvm.get_function(&selected_symbol(physical, ty, *cap))
                    .unwrap(),
            )
        })
        .collect();
    let ptr = ctx.ptr_type(AddressSpace::default());
    let count = if capability == ValueCapability::Hash {
        3
    } else {
        4
    };
    let function = llvm.add_function(
        name,
        ctx.i32_type().fn_type(&vec![ptr.into(); count], false),
        None,
    );
    let parent = ModuleEmitter {
        ctx,
        module: physical,
        llvm,
        functions: BTreeMap::new(),
        value_callbacks: BTreeMap::new(),
    };
    let mut emitter = SelectedValueEmitter::new(&parent, &callbacks, function, capability).unwrap();
    let lhs = emitter.parameter(0).unwrap();
    let rhs = if capability == ValueCapability::Eq {
        Some(emitter.parameter(1).unwrap())
    } else {
        None
    };
    emit(&mut emitter, lhs, rhs).unwrap();
    parent.llvm
}

fn engine<'ctx>(llvm: &Module<'ctx>, optimized: bool) -> ExecutionEngine<'ctx> {
    if optimized {
        // The test calls these otherwise-private callbacks by address, so retain
        // their entry points while applying the production optimization pipeline.
        for function in llvm.get_functions() {
            if function
                .get_name()
                .to_bytes()
                .starts_with(b"__hew_value_callback_")
            {
                function.set_linkage(Linkage::External);
            }
        }
        let triple = native_emission_triple();
        let machine =
            crate::llvm::target_machine_for_triple_with_opt_level(&triple, OptLevel::O2).unwrap();
        llvm.set_triple(&machine.get_triple());
        llvm.run_passes(
            "default<O2>",
            &machine,
            inkwell::passes::PassBuilderOptions::create(),
        )
        .unwrap();
        llvm.verify().unwrap();
    }
    let engine = llvm
        .create_jit_execution_engine(inkwell::OptimizationLevel::None)
        .unwrap();
    // Bind the actual runtime ABI; callbacks and private user bodies remain JIT code.
    macro_rules! bind {
        ($($module:ident :: $name:ident),* $(,)?) => {$({
            if let Some(function) = llvm.get_function(stringify!($name)) {
                engine.add_global_mapping(&function, hew_runtime::$module::$name as *const () as usize);
            }
        })*};
    }
    bind!(
        string::hew_string_hash_fnv1a,
        string::hew_string_equals,
        string::hew_string_literal_new,
        string::hew_string_clone,
        string::hew_string_drop,
        string::hew_string_length,
        string::hew_string_byte_length,
        string::hew_string_to_uppercase,
        string::hew_string_to_bytes_owned,
        bytes::hew_bytes_eq,
        bytes::hew_bytes_clone_ref,
        bytes::hew_bytes_drop,
        bytes::hew_bytes_len,
        bytes::hew_bytes_literal_new,
        bytes::hew_bytes_push_owned,
        hashmap::hew_hashmap_new_with_layout,
        hashmap::hew_hashmap_free_layout,
        hashmap::hew_hashmap_clone_layout,
        hashmap::hew_hashmap_insert_clone_layout,
        hashmap::hew_hashmap_get_clone_layout,
        hashmap::hew_hashmap_len_layout,
        hashmap::hew_hashmap_iter_new_layout,
        hashmap::hew_hashmap_iter_next_layout,
        hashmap::hew_hashmap_iter_free_layout,
        hashset::hew_hashset_new_with_layout,
        hashset::hew_hashset_free_layout,
        hashset::hew_hashset_clone_layout,
        hashset::hew_hashset_len_layout,
        hashset::hew_hashset_iter_new_layout,
        hashset::hew_hashset_iter_next_layout,
        hashset::hew_hashset_iter_free_layout,
        vec::hew_vec_new_with_elem_layout,
        vec::hew_vec_free_owned,
        vec::hew_vec_clone_owned,
        vec::hew_vec_push_owned,
        vec::hew_vec_push_owned_move,
        vec::hew_vec_get_clone,
        vec::hew_vec_get_owned,
        vec::hew_vec_len,
        fault::hew_fault_new,
        fault::hew_fault_drop,
        fault::hew_fault_report,
    );
    engine
}

fn callbacks<'ctx>(
    engine: &ExecutionEngine<'ctx>,
    module: &PhysicalModule,
    ty: &ResolvedTy,
) -> (
    JitFunction<'ctx, HashCallback>,
    JitFunction<'ctx, EqCallback>,
) {
    // SAFETY: the emitter declares precisely these slot/result/fault ABIs.
    unsafe {
        (
            engine
                .get_function(&selected_symbol(module, ty, ValueCapability::Hash))
                .unwrap(),
            engine
                .get_function(&selected_symbol(module, ty, ValueCapability::Eq))
                .unwrap(),
        )
    }
}

fn hash<T>(callback: &JitFunction<'_, HashCallback>, value: &T) -> u64 {
    let mut output = 0xfeedface;
    let mut fault = std::ptr::dangling_mut::<c_void>();
    // SAFETY: callers select a callback matching T's physical representation.
    let status = unsafe {
        callback.call(
            std::ptr::from_ref(value).cast(),
            &raw mut output,
            &raw mut fault,
        )
    };
    assert_eq!(status, 0);
    assert!(fault.is_null());
    output
}

fn equal<T>(callback: &JitFunction<'_, EqCallback>, lhs: &T, rhs: &T) -> bool {
    let mut output = false;
    let mut fault = std::ptr::dangling_mut::<c_void>();
    // SAFETY: callers provide two live slots matching the selected callback type.
    let status = unsafe {
        callback.call(
            std::ptr::from_ref(lhs).cast(),
            std::ptr::from_ref(rhs).cast(),
            &raw mut output,
            &raw mut fault,
        )
    };
    assert_eq!(status, 0);
    assert!(fault.is_null());
    output
}

#[test]
fn scalar_float_keys_have_total_bitwise_equality_and_coherent_hashes() {
    for optimized in [false, true] {
        let physical = physical("fn main() { let values: HashMap<f64, i64> = HashMap.new(); }");
        let ctx = Context::create();
        let llvm = llvm(&ctx, &physical);
        let engine = engine(&llvm, optimized);
        let (hash_fn, eq_fn) = callbacks(&engine, &physical, &ResolvedTy::F64);
        let nan = f64::from_bits(0x7ff8_0000_0000_0042);
        let other_nan = f64::from_bits(0x7ff8_0000_0000_0043);
        for value in [0.0, -0.0, 1.5, f64::INFINITY, nan] {
            assert!(equal(&eq_fn, &value, &value));
            assert_eq!(hash(&hash_fn, &value), value.to_bits());
        }
        assert!(!equal(&eq_fn, &0.0, &-0.0));
        assert!(!equal(&eq_fn, &nan, &other_nan));
    }
}

#[test]
fn aggregate_hash_ignores_padding_and_composes_exact_user_field_methods() {
    for optimized in [false, true] {
        let physical = physical(
            r"
        type Inner { id: i64 }
        impl Hash for Inner { fn hash(self) -> i64 { self.id % 10 } }
        impl Eq for Inner { fn eq(self, other: Inner) -> bool { self.id % 10 == other.id % 10 } }
        type Outer { tag: u8, inner: Inner }
        fn main() { let values: HashMap<Outer, i64> = HashMap.new(); }
    ",
        );
        let outer = ResolvedTy::named_user("Outer", vec![]);
        let ctx = Context::create();
        let llvm = llvm(&ctx, &physical);
        let engine = engine(&llvm, optimized);
        let (hash_fn, eq_fn) = callbacks(&engine, &physical, &outer);
        // Aligned storage with intentionally distinct padding bytes. The real field
        // offsets are read from the target, not assumed from a Rust field layout.
        let structure = llvm_type(&ctx, &physical.target.layout(&outer).unwrap().repr)
            .unwrap()
            .into_struct_type();
        let target = TargetData::create(&physical.target.data_layout);
        let tag_offset = usize::try_from(target.offset_of_element(&structure, 0).unwrap()).unwrap();
        let inner_offset =
            usize::try_from(target.offset_of_element(&structure, 1).unwrap()).unwrap();
        let mut left = [0xaaaa_aaaa_aaaa_aaaau64; 2];
        let mut right = [0x5555_5555_5555_5555u64; 2];
        // SAFETY: both arrays cover the verified 16-byte record and are i64 aligned.
        unsafe {
            let a = left.as_mut_ptr().cast::<u8>();
            let b = right.as_mut_ptr().cast::<u8>();
            a.add(tag_offset).write(7);
            b.add(tag_offset).write(7);
            a.add(inner_offset).cast::<i64>().write(1);
            b.add(inner_offset).cast::<i64>().write(11);
        }
        assert!(equal(&eq_fn, &left, &right));
        assert_eq!(hash(&hash_fn, &left), hash(&hash_fn, &right));
    }
}

#[test]
fn counted_strings_and_byte_regions_ignore_owner_identity() {
    for optimized in [false, true] {
        let physical = physical(
            r"
        fn main() { let strings: HashMap<string, i64> = HashMap.new(); }
    ",
        );
        let ctx = Context::create();
        let llvm = llvm(&ctx, &physical);
        // The base's legacy collection gate refuses bytes keys. Exercise the byte
        // recipe directly without weakening that gate or fabricating a selection.
        let llvm = add_recipe_callback(
            &ctx,
            llvm,
            &physical,
            "bytes_hash",
            ValueCapability::Hash,
            |emitter, lhs, rhs| emitter.bytes(lhs, rhs),
        );
        let llvm = add_recipe_callback(
            &ctx,
            llvm,
            &physical,
            "bytes_eq",
            ValueCapability::Eq,
            |emitter, lhs, rhs| emitter.bytes(lhs, rhs),
        );
        llvm.verify().unwrap();
        let engine = engine(&llvm, optimized);
        let (string_hash, string_eq) = callbacks(&engine, &physical, &ResolvedTy::String);
        // SAFETY: these wrappers use the same declared callback ABI as key descriptors.
        let bytes_hash = unsafe { engine.get_function::<HashCallback>("bytes_hash").unwrap() };
        // SAFETY: the equality wrapper takes two byte slots and result/fault outputs.
        let bytes_eq = unsafe { engine.get_function::<EqCallback>("bytes_eq").unwrap() };
        // SAFETY: literal constructors receive readable counted data and unique
        // outputs; all handles stay live until the last JIT callback returns.
        unsafe {
            let mut a = std::ptr::null_mut();
            let mut b = std::ptr::null_mut();
            let mut c = std::ptr::null_mut();
            hew_runtime::string::hew_string_literal_new(b"a\0b".as_ptr(), 3, &raw mut a);
            hew_runtime::string::hew_string_literal_new(b"a\0b".as_ptr(), 3, &raw mut b);
            hew_runtime::string::hew_string_literal_new(b"a\0c".as_ptr(), 3, &raw mut c);
            assert!(equal(&string_eq, &a, &b));
            assert!(!equal(&string_eq, &a, &c));
            assert_eq!(hash(&string_hash, &a), hash(&string_hash, &b));
            hew_runtime::string::hew_string_drop(a);
            hew_runtime::string::hew_string_drop(b);
            hew_runtime::string::hew_string_drop(c);
        }
        let left_data = b"xxa\0byy";
        let right_data = b"a\0b";
        let left = hew_runtime::bytes::BytesTriple {
            ptr: left_data.as_ptr().cast_mut(),
            offset: 2,
            len: 3,
        };
        let right = hew_runtime::bytes::BytesTriple {
            ptr: right_data.as_ptr().cast_mut(),
            offset: 0,
            len: 3,
        };
        assert!(equal(&bytes_eq, &left, &right));
        assert_eq!(hash(&bytes_hash, &left), hash(&bytes_hash, &right));
        let empty = hew_runtime::bytes::BytesTriple {
            ptr: std::ptr::null_mut(),
            offset: 0,
            len: 0,
        };
        assert!(equal(&bytes_eq, &empty, &empty));
        assert_eq!(hash(&bytes_hash, &empty), FNV_OFFSET);
    }
}

#[test]
fn user_fault_preserves_status_pointer_and_unwritten_callback_result() {
    for optimized in [false, true] {
        let physical = physical(
            r"
        type Inner { id: i64 }
        impl Hash for Inner { fn hash(self) -> i64 { 1 } }
        impl Eq for Inner { fn eq(self, other: Inner) -> bool { true } }
        type Outer { inner: Inner }
        fn main() { let values: HashMap<Outer, i64> = HashMap.new(); }
    ",
        );
        let ctx = Context::create();
        let llvm = ctx.create_module("key_fault");
        llvm.set_data_layout(&TargetData::create(&physical.target.data_layout).get_data_layout());
        let mut emitter = ModuleEmitter {
            ctx: &ctx,
            module: &physical,
            llvm,
            functions: BTreeMap::new(),
            value_callbacks: BTreeMap::new(),
        };
        emitter.declare_functions().unwrap();
        emitter.emit_selected_value_callbacks().unwrap();
        // Model the private callable's failure ABI with a distinct nonzero status
        // and an opaque pointer. It deliberately dirties its own scratch result.
        for plan in physical.value_capabilities.values() {
            if let PhysicalValueMethod::User(id) = plan.method {
                let function = emitter.functions[&id];
                let builder = ctx.create_builder();
                builder.position_at_end(ctx.append_basic_block(function, "fault"));
                let count = function.count_params();
                let output = function
                    .get_nth_param(count - 2)
                    .unwrap()
                    .into_pointer_value();
                let fault = function
                    .get_nth_param(count - 1)
                    .unwrap()
                    .into_pointer_value();
                let layout = physical.callables[id.0 as usize]
                    .return_layout
                    .as_ref()
                    .unwrap();
                builder
                    .build_store(output, llvm_type(&ctx, &layout.repr).unwrap().const_zero())
                    .unwrap();
                builder
                    .build_store(
                        fault,
                        ctx.i64_type()
                            .const_int(0x1234, false)
                            .const_to_pointer(ctx.ptr_type(AddressSpace::default())),
                    )
                    .unwrap();
                builder
                    .build_return(Some(&ctx.i32_type().const_int(77, false)))
                    .unwrap();
            }
        }
        emitter.llvm.verify().unwrap();
        let engine = engine(&emitter.llvm, optimized);
        let outer = ResolvedTy::named_user("Outer", vec![]);
        let (hash_fn, eq_fn) = callbacks(&engine, &physical, &outer);
        let input = 1i64;
        let mut hash_out = 0xfeed_face_cafe_beefu64;
        let mut eq_out = true;
        let mut fault = std::ptr::null_mut();
        // SAFETY: Outer contains one i64 field. Both callback signatures and the
        // writable result/fault slots match the generated ABI; fault is never read.
        unsafe {
            assert_eq!(
                hash_fn.call(
                    std::ptr::from_ref(&input).cast(),
                    &raw mut hash_out,
                    &raw mut fault
                ),
                77
            );
            assert_eq!(hash_out, 0xfeed_face_cafe_beef);
            assert_eq!(fault as usize, 0x1234);
            fault = std::ptr::null_mut();
            assert_eq!(
                eq_fn.call(
                    std::ptr::from_ref(&input).cast(),
                    std::ptr::from_ref(&input).cast(),
                    &raw mut eq_out,
                    &raw mut fault
                ),
                77
            );
            assert!(eq_out);
            assert_eq!(fault as usize, 0x1234);
        }
    }
}

#[test]
fn owned_user_key_methods_receive_borrowed_slots_and_override_structure() {
    for optimized in [false, true] {
        let physical = physical(
            r"
        type Key { name: string }
        impl Hash for Key { fn hash(self) -> i64 { 55 } }
        impl Eq for Key { fn eq(self, other: Key) -> bool { true } }
        fn main() { let values: HashMap<Key, i64> = HashMap.new(); }
    ",
        );
        for plan in physical.value_capabilities.values() {
            if let PhysicalValueMethod::User(id) = plan.method {
                assert!(physical.callables[id.0 as usize]
                    .params
                    .iter()
                    .all(|param| param.carrier == ParamCarrier::Indirect));
            }
        }
        let ctx = Context::create();
        let llvm = llvm(&ctx, &physical);
        let engine = engine(&llvm, optimized);
        let (hash_fn, eq_fn) =
            callbacks(&engine, &physical, &ResolvedTy::named_user("Key", vec![]));
        // SAFETY: Key has exactly one managed-string pointer field; both strings
        // outlive the borrowed key callbacks and are released once afterwards.
        unsafe {
            let mut left = std::ptr::null_mut();
            let mut right = std::ptr::null_mut();
            hew_runtime::string::hew_string_literal_new(b"left".as_ptr(), 4, &raw mut left);
            hew_runtime::string::hew_string_literal_new(b"right".as_ptr(), 5, &raw mut right);
            assert_eq!(hash(&hash_fn, &left), 55);
            assert!(equal(&eq_fn, &left, &right));
            hew_runtime::string::hew_string_drop(left);
            hew_runtime::string::hew_string_drop(right);
        }
    }
}

#[test]
fn key_descriptors_require_construction_and_complete_selected_plans() {
    let mut physical = physical(
        r"
        fn inspect(values: HashMap<i64, bool>) -> i64 { values.len() }
        fn main() { let values: HashMap<i64, i64> = HashMap.new(); }
    ",
    );
    let ctx = Context::create();
    let module = llvm(&ctx, &physical);
    let borrowed = physical
        .map_glue
        .iter()
        .find(|glue| glue.value.ty == ResolvedTy::Bool)
        .unwrap();
    let constructed = physical
        .map_glue
        .iter()
        .find(|glue| glue.value.ty == ResolvedTy::I64)
        .unwrap();
    assert!(module
        .get_global(&map_key_descriptor_symbol(borrowed.id))
        .is_none());
    assert!(module
        .get_global(&map_key_descriptor_symbol(constructed.id))
        .is_some());
    physical
        .value_capabilities
        .remove(&(ResolvedTy::I64, ValueCapability::Hash));
    let emitter = ModuleEmitter {
        ctx: &ctx,
        module: &physical,
        llvm: ctx.create_module("missing_key"),
        functions: BTreeMap::new(),
        value_callbacks: BTreeMap::new(),
    };
    assert!(emitter
        .emit_selected_value_callbacks()
        .unwrap_err()
        .to_string()
        .contains("lacks selected Hash"));
}

#[test]
fn selected_vector_and_variant_equality_walks_live_elements_and_active_fields() {
    for optimized in [false, true] {
        let physical = physical(
            r"
        fn main() {
            let values: HashMap<i64, i64> = HashMap.new();
            let vector = [1];
            let optional = vector.get(0);
        }
    ",
        );
        let ctx = Context::create();
        let llvm = llvm(&ctx, &physical);
        let variant = physical.variant_glue.first().unwrap().id;
        let llvm = add_recipe_callback(
            &ctx,
            llvm,
            &physical,
            "vector_eq",
            ValueCapability::Eq,
            |emitter, lhs, rhs| emitter.vector(&ResolvedTy::I64, lhs, rhs.unwrap()),
        );
        let llvm = add_recipe_callback(
            &ctx,
            llvm,
            &physical,
            "variant_eq",
            ValueCapability::Eq,
            |emitter, lhs, rhs| emitter.variant(variant, lhs, rhs.unwrap()),
        );
        llvm.verify().unwrap();
        let engine = engine(&llvm, optimized);
        // SAFETY: both wrappers have the slot/slot/bool-out/fault-out ABI.
        let vec_eq = unsafe { engine.get_function::<EqCallback>("vector_eq").unwrap() };
        // SAFETY: the wrapper consumes the exact Option slot representation.
        let variant_eq = unsafe { engine.get_function::<EqCallback>("variant_eq").unwrap() };
        let layout = hew_runtime::vec::HewValueLayout {
            size: std::mem::size_of::<i64>(),
            align: std::mem::align_of::<i64>(),
            ownership_kind: HewTypeOwnershipKind::Plain,
            clone_fn: None,
            drop_fn: None,
        };
        // SAFETY: each vector owns copied i64 slots described by layout and remains
        // live and unmodified during equality. Every vector is freed once.
        unsafe {
            let a = hew_runtime::vec::hew_vec_new_with_elem_layout(&raw const layout);
            let b = hew_runtime::vec::hew_vec_new_with_elem_layout(&raw const layout);
            let c = hew_runtime::vec::hew_vec_new_with_elem_layout(&raw const layout);
            assert!(equal(&vec_eq, &a, &b));
            for value in [1i64, 2, 3] {
                hew_runtime::vec::hew_vec_push_owned(a, std::ptr::from_ref(&value).cast());
                hew_runtime::vec::hew_vec_push_owned(b, std::ptr::from_ref(&value).cast());
            }
            for value in [1i64, 2, 4] {
                hew_runtime::vec::hew_vec_push_owned(c, std::ptr::from_ref(&value).cast());
            }
            assert!(equal(&vec_eq, &a, &b));
            assert!(!equal(&vec_eq, &a, &c));
            let last = 4i64;
            hew_runtime::vec::hew_vec_push_owned(b, std::ptr::from_ref(&last).cast());
            assert!(!equal(&vec_eq, &a, &b));
            hew_runtime::vec::hew_vec_free_owned(a);
            hew_runtime::vec::hew_vec_free_owned(b);
            hew_runtime::vec::hew_vec_free_owned(c);
        }
        // Option<i64>'s active arm is target-laid out as tag plus aligned i64.
        let option = physical.variant_glue.first().unwrap();
        let layout = physical.target.variant_layout(&option.ty).unwrap();
        let object = llvm_type(&ctx, &layout.object.repr)
            .unwrap()
            .into_struct_type();
        let target = TargetData::create(&physical.target.data_layout);
        assert_eq!(target.get_abi_size(&object), 16);
        assert_eq!(target.offset_of_element(&object, 1), Some(8));
        assert!(equal(&variant_eq, &[0u64, 3], &[0u64, 3]));
        assert!(!equal(&variant_eq, &[0u64, 3], &[0u64, 4]));
        assert!(!equal(&variant_eq, &[0u64, 3], &[1u64, 3]));
        assert!(equal(&variant_eq, &[1u64, 3], &[1u64, 99]));
    }
}

#[test]
fn absent_components_and_unadmitted_collection_recipes_fail_closed() {
    let mut physical = physical(
        r"
        type Key { id: i64 }
        fn main() { let values: HashMap<Key, i64> = HashMap.new(); }
    ",
    );
    let ctx = Context::create();
    physical
        .value_capabilities
        .remove(&(ResolvedTy::I64, ValueCapability::Hash));
    let emitter = ModuleEmitter {
        ctx: &ctx,
        module: &physical,
        llvm: ctx.create_module("missing_component"),
        functions: BTreeMap::new(),
        value_callbacks: BTreeMap::new(),
    };
    assert!(emitter
        .emit_selected_value_callbacks()
        .unwrap_err()
        .to_string()
        .contains("has no callback"));
    // Malformed physical recipes must refuse even if a table entry exists.
    let key = (ResolvedTy::named_user("Key", vec![]), ValueCapability::Eq);
    for method in [
        PhysicalValueMethod::Map(PhysicalMapId(0)),
        PhysicalValueMethod::Set(PhysicalSetId(0)),
    ] {
        let mut malformed = physical.clone();
        malformed
            .value_capabilities
            .retain(|candidate, _| candidate == &key);
        malformed.value_capabilities.get_mut(&key).unwrap().method = method;
        let emitter = ModuleEmitter {
            ctx: &ctx,
            module: &malformed,
            llvm: ctx.create_module("unadmitted_recipe"),
            functions: BTreeMap::new(),
            value_callbacks: BTreeMap::new(),
        };
        assert!(emitter
            .emit_selected_value_callbacks()
            .unwrap_err()
            .to_string()
            .contains("outside checker admission"));
    }
}

#[test]
fn key_callback_layouts_and_private_calls_verify_on_windows_and_macos() {
    let source = r"
        type Key { name: string }
        impl Hash for Key { fn hash(self) -> i64 { 55 } }
        impl Eq for Key { fn eq(self, other: Key) -> bool { true } }
        fn main() {
            let values: HashMap<Key, i64> = HashMap.new();
            let floats: HashMap<f64, i64> = HashMap.new();
            let set: HashSet<i64> = HashSet.new();
        }
    ";
    for triple in ["x86_64-pc-windows-msvc", "aarch64-apple-darwin"] {
        let physical = physical_for_triple(source, triple);
        let ctx = Context::create();
        let llvm = llvm(&ctx, &physical);
        let llvm = add_recipe_callback(
            &ctx,
            llvm,
            &physical,
            "bytes_hash",
            ValueCapability::Hash,
            |emitter, lhs, rhs| emitter.bytes(lhs, rhs),
        );
        llvm.verify().unwrap();
        for glue in &physical.map_glue {
            assert!(llvm
                .get_global(&map_key_descriptor_symbol(glue.id))
                .is_some());
        }
        for glue in &physical.set_glue {
            assert!(llvm
                .get_global(&set_key_descriptor_symbol(glue.id))
                .is_some());
        }
    }
}

#[path = "physical_value_call_tests.rs"]
mod value_calls;
