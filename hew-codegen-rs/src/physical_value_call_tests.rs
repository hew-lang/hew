//! Execute selected calls through verified SIR, physical MIR and ordinary bodies.

use super::*;
use hew_sir::{BoundaryDecision, SemTerminator, SemValueMethodPlan};
use hew_types::{TypeFactService, ValueMethodPlan};

fn selected_calls(source: &str, triple: &str) -> PhysicalModule {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let checked = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
    let hir = lower_program_host_target(&parsed.program, &checked, &ResolutionCtx);
    assert!(hir.diagnostics.is_empty(), "{:?}", hir.diagnostics);
    let mut module = hew_sir::lower_module(&hir.module, &checked).module;
    assert!(hew_sir::verify_module(&module).is_empty());
    // The source producer is a separate layer. Ask the real checker for exact
    // selections over this fixture's inventory, then replace ordinary calls
    // while retaining their verified operand loans and both cleanup paths.
    let mut service = TypeFactService::new(checked.type_fact_context, checked.type_facts);
    let inventory = hew_mir::physical::physical_type_inventory(&module);
    for ty in inventory.types() {
        for capability in [ValueCapability::Eq, ValueCapability::Hash] {
            if module
                .value_capabilities
                .contains_key(&(ty.clone(), capability))
            {
                continue;
            }
            if let Some(selection) = service.capability_plan(ty, capability).unwrap() {
                if *selection.plan() == ValueMethodPlan::Derived {
                    service.require(ty).unwrap();
                    module.value_capabilities.insert(
                        (ty.clone(), capability),
                        SemValueMethodPlan {
                            selection,
                            callable: None,
                        },
                    );
                }
            }
        }
    }
    module.type_facts.extend(service.into_rows());
    let targets: BTreeMap<_, _> = module
        .functions
        .iter()
        .filter_map(|function| {
            let capability = match function.name.as_str() {
                "selected_eq" => ValueCapability::Eq,
                "selected_hash" => ValueCapability::Hash,
                _ => return None,
            };
            let ty = module.callable(function.callable).unwrap().signature.params[0]
                .ty
                .clone();
            Some((function.callable, (ty, capability)))
        })
        .collect();
    let mut converted = 0;
    for function in &mut module.functions {
        for block in &mut function.blocks {
            if let SemTerminator::Call {
                id,
                callee,
                args,
                result,
                normal,
                unwind,
            } = &block.terminator
            {
                let Some((ty, capability)) = targets.get(callee) else {
                    continue;
                };
                let mut args = args.clone();
                for arg in &mut args {
                    arg.decision = BoundaryDecision::Borrow;
                }
                block.terminator = SemTerminator::ValueCall {
                    id: *id,
                    ty: ty.clone(),
                    capability: *capability,
                    args,
                    result: result.clone(),
                    normal: normal.clone(),
                    unwind: unwind.clone(),
                };
                converted += 1;
            }
        }
    }
    assert!(
        converted > 0,
        "fixture must contain an executed selected call"
    );
    assert!(
        hew_sir::verify_module(&module).is_empty(),
        "{:?}",
        hew_sir::verify_module(&module)
    );
    let target =
        physical_target_for_inventory(triple, &hew_mir::physical::physical_type_inventory(&module))
            .unwrap();
    hew_mir::lower_physical_module(&module, target)
        .unwrap()
        .module()
        .clone()
}

// Expose the ordinary private function through uniform slot arguments for the
// Rust oracle. This adapts only its declared ABI, not the ValueCall inside it.
fn expose_probe<'ctx>(
    ctx: &'ctx Context,
    llvm: &Module<'ctx>,
    physical: &PhysicalModule,
    name: &str,
) -> String {
    let callable = physical
        .callables
        .iter()
        .find(|callable| callable.symbol == name)
        .unwrap();
    let callee = llvm
        .get_function(&emitted_symbol(physical, callable))
        .unwrap();
    let symbol = format!("test_{name}");
    let pointer = ctx.ptr_type(AddressSpace::default());
    let wrapper = llvm.add_function(
        &symbol,
        ctx.i32_type()
            .fn_type(&vec![pointer.into(); callable.params.len() + 2], false),
        None,
    );
    let builder = ctx.create_builder();
    builder.position_at_end(ctx.append_basic_block(wrapper, "entry"));
    let mut args = Vec::<BasicMetadataValueEnum<'_>>::new();
    for (index, parameter) in callable.params.iter().enumerate() {
        let slot = wrapper
            .get_nth_param(u32::try_from(index).unwrap())
            .unwrap()
            .into_pointer_value();
        args.push(match parameter.carrier {
            ParamCarrier::Indirect => slot.into(),
            ParamCarrier::Direct => builder
                .build_load(
                    llvm_type(ctx, &parameter.layout.repr).unwrap(),
                    slot,
                    "argument",
                )
                .unwrap()
                .into(),
        });
    }
    args.extend(
        wrapper
            .get_params()
            .into_iter()
            .skip(callable.params.len())
            .map(BasicMetadataValueEnum::from),
    );
    let status = builder
        .build_call(callee, &args, "status")
        .unwrap()
        .try_as_basic_value()
        .basic()
        .unwrap();
    builder.build_return(Some(&status)).unwrap();
    llvm.verify().unwrap();
    symbol
}

#[test]
fn ordinary_string_eq_and_scalar_hash_use_selected_callbacks_at_o0_o2() {
    let physical = selected_calls(
        r#"
        fn selected_eq(a: string, b: string) -> bool { false }
        fn selected_hash(value: i64) -> i64 { 0 }
        fn probe(a: string, b: string) -> bool { selected_eq(a, b) }
        fn hash_probe(value: i64) -> i64 { selected_hash(value) }
        fn main() -> i64 { if probe("a", "b") { hash_probe(7) } else { 0 } }
    "#,
        &native_emission_triple(),
    );
    for optimized in [false, true] {
        let ctx = Context::create();
        let llvm = llvm(&ctx, &physical);
        assert!(llvm
            .get_globals()
            .all(|global| !global.get_name().to_bytes().starts_with(b"__hew_map_key_")));
        let eq_symbol = expose_probe(&ctx, &llvm, &physical, "probe");
        let hash_symbol = expose_probe(&ctx, &llvm, &physical, "hash_probe");
        let engine = engine(&llvm, optimized);
        // SAFETY: wrappers expose the recorded slot/result/fault signatures.
        unsafe {
            let eq_fn = engine.get_function::<EqCallback>(&eq_symbol).unwrap();
            let hash_fn = engine.get_function::<HashCallback>(&hash_symbol).unwrap();
            let mut a = std::ptr::null_mut();
            let mut b = std::ptr::null_mut();
            let mut c = std::ptr::null_mut();
            hew_runtime::string::hew_string_literal_new(b"a\0b".as_ptr(), 3, &raw mut a);
            hew_runtime::string::hew_string_literal_new(b"a\0b".as_ptr(), 3, &raw mut b);
            hew_runtime::string::hew_string_literal_new(b"a\0c".as_ptr(), 3, &raw mut c);
            assert!(equal(&eq_fn, &a, &b));
            assert!(!equal(&eq_fn, &a, &c));
            assert_eq!(hash(&hash_fn, &-7_i64), (-7_i64).cast_unsigned());
            assert_eq!(hew_runtime::string::hew_string_byte_length(a), 3);
            for value in [a, b, c] {
                hew_runtime::string::hew_string_drop(value);
            }
        }
    }
}

thread_local! {
    static PRINTED_BOOLS: std::cell::RefCell<Vec<bool>> = const { std::cell::RefCell::new(Vec::new()) };
    static CREATED_FAULT: std::cell::Cell<*mut hew_runtime::fault::HewFault> = const { std::cell::Cell::new(std::ptr::null_mut()) };
}

extern "C" fn capture_bool(value: u8) {
    PRINTED_BOOLS.with_borrow_mut(|values| values.push(value != 0));
}

extern "C" fn capture_fault(code: i32) -> *mut hew_runtime::fault::HewFault {
    let fault = hew_runtime::fault::hew_fault_new(code);
    assert!(CREATED_FAULT.replace(fault).is_null());
    fault
}

const COMPOSITE_SOURCE: &str =
    include_str!("../../tests/core-acceptance/cases/selected-composite-equality.hew");
const FAULT_SOURCE: &str =
    include_str!("../../tests/core-acceptance/cases/selected-equality-callback-fault.hew");

type MainBody = unsafe extern "C" fn(*mut i64, *mut *mut c_void) -> i32;

#[test]
fn source_composite_eq_executes_exact_generic_methods_and_float_rules_at_o0_o2() {
    let physical = physical(COMPOSITE_SOURCE);
    assert!(physical
        .functions
        .iter()
        .flat_map(|function| &function.blocks)
        .any(|block| matches!(block.terminator, PhysicalTerminator::ValueCall { .. })));
    assert!(physical
        .value_capabilities
        .values()
        .any(|plan| matches!(plan.method, PhysicalValueMethod::User(_))));
    for optimized in [false, true] {
        PRINTED_BOOLS.with_borrow_mut(Vec::clear);
        let ctx = Context::create();
        let llvm = llvm(&ctx, &physical);
        let symbol = expose_probe(&ctx, &llvm, &physical, "main");
        let engine = engine(&llvm, optimized);
        engine.add_global_mapping(
            &llvm.get_function("hew_println_bool").unwrap(),
            capture_bool as *const () as usize,
        );
        let mut result = -1;
        let mut fault = std::ptr::null_mut();
        // SAFETY: the wrapper exposes main's recorded i64 result/fault ABI.
        unsafe {
            let main = engine.get_function::<MainBody>(&symbol).unwrap();
            assert_eq!(main.call(&raw mut result, &raw mut fault), 0);
        }
        assert_eq!(result, 0);
        assert!(fault.is_null());
        PRINTED_BOOLS.with_borrow(|values| {
            assert_eq!(
                values,
                &[true, true, true, true, true, true, true, true, true, false, true, true, true,]
            )
        });
    }
}

#[test]
fn source_selected_eq_fault_preserves_status_owner_and_caller_result_at_o0_o2() {
    let physical = physical(FAULT_SOURCE);
    for optimized in [false, true] {
        assert!(CREATED_FAULT.get().is_null());
        PRINTED_BOOLS.with_borrow_mut(Vec::clear);
        let ctx = Context::create();
        let llvm = llvm(&ctx, &physical);
        let symbol = expose_probe(&ctx, &llvm, &physical, "main");
        let engine = engine(&llvm, optimized);
        engine.add_global_mapping(
            &llvm.get_function("hew_fault_new").unwrap(),
            capture_fault as *const () as usize,
        );
        engine.add_global_mapping(
            &llvm.get_function("hew_println_bool").unwrap(),
            capture_bool as *const () as usize,
        );
        let mut result = 0x1234_5678_i64;
        let mut fault = std::ptr::null_mut();
        // SAFETY: main has an i64 output and transfers one fault on failure.
        unsafe {
            let main = engine.get_function::<MainBody>(&symbol).unwrap();
            assert_eq!(
                main.call(&raw mut result, &raw mut fault),
                HEW_TRAP_DIVIDE_BY_ZERO
            );
            assert_eq!(result, 0x1234_5678);
            assert!(!fault.is_null());
            assert_eq!(fault, CREATED_FAULT.replace(std::ptr::null_mut()).cast());
            hew_runtime::fault::hew_fault_drop(fault.cast());
        }
        PRINTED_BOOLS.with_borrow(|values| assert!(values.is_empty()));
    }
}

#[test]
fn source_selected_value_calls_verify_at_o0_o2_for_windows_and_macos() {
    for triple in ["x86_64-pc-windows-msvc", "aarch64-apple-darwin"] {
        for source in [COMPOSITE_SOURCE, FAULT_SOURCE, BYTES_SOURCE] {
            let physical = physical_for_triple(source, triple);
            let ctx = Context::create();
            let llvm = llvm(&ctx, &physical);
            llvm.verify().unwrap();
            let machine =
                crate::llvm::target_machine_for_triple_with_opt_level(triple, OptLevel::O2)
                    .unwrap();
            llvm.run_passes(
                "default<O2>",
                &machine,
                inkwell::passes::PassBuilderOptions::create(),
            )
            .unwrap();
            llvm.verify().unwrap();
        }
    }
}

#[test]
fn bare_float_arithmetic_and_nan_inequality_keep_ieee_semantics_at_o0_o2() {
    let physical = physical(
        r"
        fn main() -> i64 {
            let nan = 0.0 / 0.0;
            println(nan != nan);
            println(nan != 1.0);
            println((7.0 - 1.0) * 2.0 / 3.0 + 1.0 == 5.0);
            println(7.0 % 3.0 == 1.0);
            0
        }
    ",
    );
    for optimized in [false, true] {
        PRINTED_BOOLS.with_borrow_mut(Vec::clear);
        let ctx = Context::create();
        let llvm = llvm(&ctx, &physical);
        let symbol = expose_probe(&ctx, &llvm, &physical, "main");
        let engine = engine(&llvm, optimized);
        engine.add_global_mapping(
            &llvm.get_function("hew_println_bool").unwrap(),
            capture_bool as *const () as usize,
        );
        let mut result = -1;
        let mut fault = std::ptr::null_mut();
        // SAFETY: main's wrapper initializes its i64 result and fault output.
        unsafe {
            assert_eq!(
                engine
                    .get_function::<MainBody>(&symbol)
                    .unwrap()
                    .call(&raw mut result, &raw mut fault),
                0
            );
        }
        assert_eq!(result, 0);
        assert!(fault.is_null());
        PRINTED_BOOLS.with_borrow(|values| assert_eq!(values, &[true; 4]));
    }
}

thread_local! {
    static STRING_CLONES: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    static STRING_DROPS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

unsafe extern "C" fn count_string_clone(value: *const c_void) -> *mut c_void {
    STRING_CLONES.set(STRING_CLONES.get() + 1);
    // SAFETY: the generated call borrows a live managed string handle.
    unsafe { hew_runtime::string::hew_string_clone(value.cast()).cast() }
}

unsafe extern "C" fn count_string_drop(value: *mut c_void) {
    STRING_DROPS.set(STRING_DROPS.get() + 1);
    // SAFETY: the generated call transfers one managed string owner.
    unsafe { hew_runtime::string::hew_string_drop(value.cast()) };
}

#[repr(C)]
struct Label {
    value: *mut c_void,
    divisor: i64,
}

#[repr(C)]
struct Envelope {
    label: Label,
}

#[test]
fn generic_user_eq_keeps_borrowed_owners_on_success_and_fault_at_o0_o2() {
    let physical = physical(
        r#"
        type Label<T> { value: T, divisor: i64 }
        impl<T> Eq for Label<T> {
            fn eq(self, other: Label<T>) -> bool { 10 / self.divisor == other.divisor }
        }
        type Envelope { label: Label<string> }
        fn probe(a: Envelope, b: Envelope) -> bool { a == b }
        fn main() -> i64 {
            let a = Envelope { label: Label { value: "left", divisor: 2 } };
            let b = Envelope { label: Label { value: "right", divisor: 5 } };
            if probe(a, b) { 0 } else { 1 }
        }
    "#,
    );
    assert!(physical
        .value_capabilities
        .values()
        .any(|plan| match plan.method {
            PhysicalValueMethod::User(id) => matches!(
                physical.callables[id.0 as usize].instance,
                hew_sir::CallableInstance::Generic(_)
            ),
            _ => false,
        }));
    for optimized in [false, true] {
        let ctx = Context::create();
        let llvm = llvm(&ctx, &physical);
        let symbol = expose_probe(&ctx, &llvm, &physical, "probe");
        let engine = engine(&llvm, optimized);
        for (name, address) in [
            ("hew_string_clone", count_string_clone as *const () as usize),
            ("hew_string_drop", count_string_drop as *const () as usize),
            ("hew_fault_new", capture_fault as *const () as usize),
        ] {
            if let Some(function) = llvm.get_function(name) {
                engine.add_global_mapping(&function, address);
            }
        }
        STRING_CLONES.set(0);
        STRING_DROPS.set(0);
        // SAFETY: Envelope wraps the verified pointer/i64 Label<string> layout. The probe
        // borrows both complete records; Rust retains and releases their owners.
        unsafe {
            let mut left = std::ptr::null_mut();
            let mut right = std::ptr::null_mut();
            hew_runtime::string::hew_string_literal_new(b"left".as_ptr(), 4, &raw mut left);
            hew_runtime::string::hew_string_literal_new(b"right".as_ptr(), 5, &raw mut right);
            let mut a = Envelope {
                label: Label {
                    value: left.cast(),
                    divisor: 2,
                },
            };
            let b = Envelope {
                label: Label {
                    value: right.cast(),
                    divisor: 5,
                },
            };
            let probe = engine.get_function::<EqCallback>(&symbol).unwrap();
            assert!(equal(&probe, &a, &b));
            a.label.divisor = 0;
            let mut output = true;
            let mut fault = std::ptr::null_mut();
            assert!(CREATED_FAULT.get().is_null());
            assert_eq!(
                probe.call(
                    (&raw const a).cast(),
                    (&raw const b).cast(),
                    &raw mut output,
                    &raw mut fault
                ),
                HEW_TRAP_DIVIDE_BY_ZERO
            );
            assert!(output);
            assert!(!fault.is_null());
            assert_eq!(fault, CREATED_FAULT.replace(std::ptr::null_mut()).cast());
            hew_runtime::fault::hew_fault_drop(fault.cast());
            assert_eq!(a.label.value, left.cast());
            assert_eq!(b.label.value, right.cast());
            assert_eq!(hew_runtime::string::hew_string_byte_length(left), 4);
            assert_eq!(hew_runtime::string::hew_string_byte_length(right), 5);
            assert_eq!(
                STRING_CLONES.get(),
                0,
                "borrowed Eq must not clone caller owners"
            );
            assert_eq!(
                STRING_DROPS.get(),
                0,
                "borrowed Eq must not drop caller owners"
            );
            hew_runtime::string::hew_string_drop(left);
            hew_runtime::string::hew_string_drop(right);
        }
    }
}

const BYTES_SOURCE: &str =
    include_str!("../../tests/core-acceptance/cases/selected-bytes-equality.hew");

thread_local! {
    static PRINTED_STRINGS: std::cell::RefCell<Vec<String>> = const { std::cell::RefCell::new(Vec::new()) };
}

unsafe extern "C" fn capture_string(value: *const c_void) {
    let mut output = std::mem::MaybeUninit::uninit();
    // SAFETY: the print ABI borrows a managed string. Convert it through the
    // public runtime API and release the temporary bytes owner after recording.
    unsafe {
        hew_runtime::string::hew_string_to_bytes_owned(value.cast(), output.as_mut_ptr());
        let bytes = output.assume_init();
        let text = if bytes.len == 0 {
            String::new()
        } else {
            String::from_utf8(
                std::slice::from_raw_parts(
                    bytes.ptr.add(bytes.offset as usize),
                    bytes.len as usize,
                )
                .to_vec(),
            )
            .unwrap()
        };
        hew_runtime::bytes::hew_bytes_drop(bytes.ptr);
        PRINTED_STRINGS.with_borrow_mut(|values| values.push(text));
    }
}

#[test]
fn source_nested_bytes_eq_and_user_hash_key_execute_at_o0_o2() {
    let physical = physical(BYTES_SOURCE);
    assert!(physical
        .value_capabilities
        .contains_key(&(ResolvedTy::Bytes, ValueCapability::Eq)));
    assert!(!physical
        .value_capabilities
        .contains_key(&(ResolvedTy::Bytes, ValueCapability::Hash)));
    for optimized in [false, true] {
        PRINTED_BOOLS.with_borrow_mut(Vec::clear);
        PRINTED_STRINGS.with_borrow_mut(Vec::clear);
        let ctx = Context::create();
        let llvm = llvm(&ctx, &physical);
        let symbol = expose_probe(&ctx, &llvm, &physical, "main");
        let engine = engine(&llvm, optimized);
        engine.add_global_mapping(
            &llvm.get_function("hew_println_bool").unwrap(),
            capture_bool as *const () as usize,
        );
        engine.add_global_mapping(
            &llvm.get_function("hew_println_str").unwrap(),
            capture_string as *const () as usize,
        );
        let mut result = -1;
        let mut fault = std::ptr::null_mut();
        // SAFETY: the wrapper exposes main's verified result and fault outputs.
        unsafe {
            assert_eq!(
                engine
                    .get_function::<MainBody>(&symbol)
                    .unwrap()
                    .call(&raw mut result, &raw mut fault),
                0
            );
        }
        assert_eq!(result, 0);
        assert!(fault.is_null());
        PRINTED_BOOLS.with_borrow(|values| assert_eq!(values, &[true; 6]));
        PRINTED_STRINGS.with_borrow(|values| assert_eq!(values, &["STORED"]));
    }
}

#[test]
fn source_bytes_eq_borrows_only_the_active_region_at_o0_o2() {
    let physical = physical(
        r#"
        fn probe(a: bytes, b: bytes) -> bool { a == b }
        fn main() -> i64 { if probe("A\0B".to_bytes(), "A\0B".to_bytes()) { 0 } else { 1 } }
    "#,
    );
    for optimized in [false, true] {
        let ctx = Context::create();
        let llvm = llvm(&ctx, &physical);
        let symbol = expose_probe(&ctx, &llvm, &physical, "probe");
        let engine = engine(&llvm, optimized);
        // SAFETY: the probe borrows valid Bytes triples; each underlying buffer
        // stays live until its single Rust-side release after all comparisons.
        unsafe {
            let probe = engine.get_function::<EqCallback>(&symbol).unwrap();
            let mut a = hew_runtime::bytes::hew_bytes_from_static(b"xA\0By".as_ptr(), 5);
            a.offset = 1;
            a.len = 3;
            let b = hew_runtime::bytes::hew_bytes_from_static(b"A\0B".as_ptr(), 3);
            let mut short = hew_runtime::bytes::hew_bytes_from_static(b"A\0B".as_ptr(), 3);
            short.len = 2;
            let empty = hew_runtime::bytes::BytesTriple {
                ptr: std::ptr::null_mut(),
                offset: 0,
                len: 0,
            };
            assert!(equal(&probe, &a, &b));
            assert!(!equal(&probe, &a, &short));
            assert!(equal(&probe, &empty, &empty));
            assert!(!equal(&probe, &a, &empty));
            assert_eq!(std::slice::from_raw_parts(a.ptr, 5), b"xA\0By");
            for value in [a, b, short] {
                hew_runtime::bytes::hew_bytes_drop(value.ptr);
            }
        }
    }
}
