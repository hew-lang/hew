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
