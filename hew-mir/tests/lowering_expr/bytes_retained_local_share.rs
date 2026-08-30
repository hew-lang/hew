use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{Instr, IrPipeline, OwnershipEvent, Place};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn pipeline(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "type errors: {:#?}",
        tc_output.errors
    );
    let hir = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        hir.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}",
        hir.diagnostics
    );
    hew_mir::lower_hir_module(&hir.module)
}

#[test]
fn returned_source_and_local_alias_get_independent_bytes_owners() {
    let pipeline = pipeline(
        r#"
fn owned_partner_escape() -> bytes {
    let source = "owned-partner-escape".to_bytes();
    let alias = source;
    let _len = alias.len();
    source
}
"#,
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "{:#?}",
        pipeline.diagnostics
    );
    let function = pipeline
        .checked_mir
        .iter()
        .find(|function| function.name == "owned_partner_escape")
        .expect("owned_partner_escape Checked MIR");

    // The retain must be spliced immediately before the share's Move so no
    // later pass reads the pair as an adoption. The alias's own Mint is a
    // separate claim: it may be published before or after the physical move
    // depending on which pass authored it, so it is asserted by presence, not
    // by adjacency.
    let retained_copy = function.blocks.iter().find_map(|block| {
        block
            .instructions
            .windows(2)
            .find_map(|window| match window {
                [Instr::BytesRetain { value: source }, Instr::Move { dest, src }]
                    if source == src && dest != src =>
                {
                    Some((*source, *dest))
                }
                _ => None,
            })
    });
    let (source, alias) = retained_copy.expect("typed Bytes share must retain before the move");
    assert!(
        function.blocks.iter().any(
            |block| block.instructions.iter().any(|instruction| matches!(
                instruction,
                Instr::OwnershipEvent(OwnershipEvent::Mint { place, .. }) if *place == alias
            ))
        ),
        "the retained alias must own its own minted generation"
    );
    assert!(!function
        .blocks
        .iter()
        .any(
            |block| block.instructions.iter().any(|instruction| matches!(
                instruction,
                Instr::NeutralizePayloadSlot {
                    place,
                    transferee: Some(destination),
                    ..
                } if *place == source && *destination == alias
            ))
        ));
    assert!(function
        .blocks
        .iter()
        .any(
            |block| block.instructions.iter().any(|instruction| matches!(
                instruction,
                Instr::OwnershipEvent(OwnershipEvent::Relocate {
                    from,
                    to: Place::ReturnSlot,
                    ..
                }) if *from == source
            ))
        ));
}
