use std::collections::HashMap;

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{Instr, IrPipeline, MirStatement, OwnershipEvent};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn pipeline(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:#?}", parsed.errors);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(tc_output.errors.is_empty(), "{:#?}", tc_output.errors);
    let hir = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(hir.diagnostics.is_empty(), "{:#?}", hir.diagnostics);
    hew_mir::lower_hir_module(&hir.module)
}

#[test]
fn nested_tuple_alias_rebind_inherits_alias_instead_of_minting_an_owner() {
    let pipeline = pipeline(
        r"
fn nested_alias(values: Vec<((bytes, i64), bool)>) -> i64 {
    let item = values[0];
    let (inner, flag) = item;
    let (payload, number) = inner;
    if flag { payload.len() + number } else { -1 }
}
",
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "{:#?}",
        pipeline.diagnostics
    );
    let function = pipeline
        .raw_mir
        .iter()
        .find(|function| function.name == "nested_alias")
        .expect("nested_alias raw MIR");

    let synthetic_tuples: HashMap<_, _> = function
        .blocks
        .iter()
        .flat_map(|block| &block.statements)
        .filter_map(|statement| match statement {
            MirStatement::Bind { binding, name, .. } if name.starts_with("__tuple_") => {
                Some((*binding, name.as_str()))
            }
            _ => None,
        })
        .collect();
    assert_eq!(synthetic_tuples.len(), 2, "{:#?}", function.blocks);

    let owner_definitions: Vec<_> = function
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| match instruction {
            Instr::OwnershipEvent(OwnershipEvent::Mint { owner, place, .. })
                if synthetic_tuples.contains_key(&owner.binding) =>
            {
                Some((owner.binding, *place))
            }
            Instr::OwnershipEvent(OwnershipEvent::Transfer {
                to: Some(place),
                to_owner: Some(owner),
                ..
            }) if synthetic_tuples.contains_key(&owner.binding) => Some((owner.binding, *place)),
            _ => None,
        })
        .collect();
    assert_eq!(
        owner_definitions.len(),
        1,
        "the first synthetic tuple takes the fresh clone-result owner, but the second is a byte-copy alias of its interior and must not mint another owner: {:#?}",
        function.blocks
    );
    let outer_binding = synthetic_tuples
        .keys()
        .min_by_key(|binding| binding.0)
        .copied()
        .expect("outer synthetic tuple binding");
    assert_eq!(
        owner_definitions[0].0, outer_binding,
        "only the outer synthetic tuple may own the cloned aggregate: {synthetic_tuples:?}"
    );
}
