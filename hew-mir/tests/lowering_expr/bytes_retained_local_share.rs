use hew_hir::{lower_program, BindingId, ResolutionCtx};
use hew_mir::{
    CheckedMirFunction, CowHeapRelease, DropKind, ExitPath, Instr, IrPipeline, MirStatement,
    OwnerId, OwnershipEvent, Place,
};
use hew_types::{module_registry::ModuleRegistry, Checker, ResolvedTy};

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

fn named_binding(function: &CheckedMirFunction, name: &str) -> BindingId {
    function
        .blocks
        .iter()
        .flat_map(|block| block.statements.iter())
        .find_map(|statement| match statement {
            MirStatement::Bind {
                binding,
                name: candidate,
                ..
            } if candidate == name => Some(*binding),
            _ => None,
        })
        .unwrap_or_else(|| panic!("{name} binding"))
}

fn owner_publication(
    function: &CheckedMirFunction,
    binding: BindingId,
) -> (OwnerId, Place, ResolvedTy) {
    function
        .blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .find_map(|instruction| match instruction {
            Instr::OwnershipEvent(OwnershipEvent::Mint { owner, place, ty })
                if owner.binding == binding =>
            {
                Some((*owner, *place, ty.clone()))
            }
            Instr::OwnershipEvent(OwnershipEvent::Transfer {
                to: Some(place),
                to_owner: Some(owner),
                to_ty: Some(ty),
                ..
            }) if owner.binding == binding => Some((*owner, *place, ty.clone())),
            _ => None,
        })
        .unwrap_or_else(|| panic!("owner publication for {binding:?}"))
}

fn assert_exact_return_drops(
    pipeline: &IrPipeline,
    holder: (Place, &ResolvedTy),
    extracted: (Place, &ResolvedTy),
) {
    let elaborated = pipeline
        .elaborated_mir
        .iter()
        .find(|function| function.name == "field_load_share")
        .expect("field_load_share Elaborated MIR");
    let return_plan = elaborated
        .drop_plans
        .iter()
        .find_map(|(exit, plan)| matches!(exit, ExitPath::Return { .. }).then_some(plan))
        .expect("field_load_share return drop plan");
    let holder_drops = return_plan
        .drops
        .iter()
        .filter(|drop| {
            drop.place == holder.0
                && &drop.ty == holder.1
                && matches!(drop.kind, DropKind::RecordInPlace)
        })
        .count();
    let extracted_drops = return_plan
        .drops
        .iter()
        .filter(|drop| {
            drop.place == extracted.0
                && &drop.ty == extracted.1
                && matches!(
                    drop.kind,
                    DropKind::CowHeap {
                        release: CowHeapRelease::Bytes
                    }
                )
        })
        .count();
    assert_eq!(holder_drops, 1, "the holder root drops exactly once");
    assert_eq!(
        extracted_drops, 1,
        "the retained extracted bytes owner drops exactly once"
    );
    assert_eq!(
        return_plan.drops.len(),
        2,
        "the return plan contains no stale or duplicate cleanup"
    );
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

#[test]
fn retained_bytes_field_keeps_the_record_root_and_drops_both_owners_once() {
    let pipeline = pipeline(
        r#"
type Holder { payload: bytes }

fn field_load_share() -> i64 {
    let holder = Holder { payload: "field-load".to_bytes() };
    let extracted = holder.payload;
    extracted.len() + holder.payload.len()
}
"#,
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "{:#?}",
        pipeline.diagnostics
    );
    let checked = pipeline
        .checked_mir
        .iter()
        .find(|function| function.name == "field_load_share")
        .expect("field_load_share Checked MIR");

    let holder_binding = named_binding(checked, "holder");
    let extracted_binding = named_binding(checked, "extracted");
    let (holder_owner, holder_place, holder_ty) = owner_publication(checked, holder_binding);
    let (extracted_owner, extracted_place, extracted_ty) =
        owner_publication(checked, extracted_binding);
    assert_eq!(extracted_ty, ResolvedTy::Bytes);

    let projected = checked.blocks.iter().find_map(|block| {
        block
            .instructions
            .iter()
            .find_map(|instruction| match instruction {
                Instr::RecordFieldLoad { record, dest, .. } if *record == holder_place => {
                    Some(*dest)
                }
                _ => None,
            })
    });
    let projected = projected.expect("bytes field load from the holder owner");
    assert!(checked.blocks.iter().any(|block| {
        block.instructions.iter().any(
            |instruction| matches!(instruction, Instr::BytesRetain { value } if *value == projected),
        )
    }));
    assert!(
        checked.blocks.iter().all(|block| {
            block.instructions.iter().all(|instruction| {
                !matches!(
                    instruction,
                    Instr::OwnershipEvent(OwnershipEvent::Transfer {
                        owner,
                        from,
                        to: None,
                        ..
                    }) if *owner == holder_owner && *from == holder_place
                )
            })
        }),
        "a retained bytes projection must not discharge the live record root",
    );
    assert_ne!(holder_owner, extracted_owner);

    assert_exact_return_drops(
        &pipeline,
        (holder_place, &holder_ty),
        (extracted_place, &extracted_ty),
    );
}
