use hew_mir::{
    BlockKind, CheckedMirFunction, ElabBlock, ElaboratedMirFunction, IrPipeline, RawMirFunction,
};

fn stages_for(raw: &RawMirFunction) -> (CheckedMirFunction, ElaboratedMirFunction) {
    let elaborated = ElaboratedMirFunction {
        name: raw.name.clone(),
        key: raw.key.clone(),
        return_ty: raw.return_ty.clone(),
        statements: raw
            .blocks
            .iter()
            .flat_map(|block| block.statements.iter().cloned())
            .collect(),
        decisions: raw.decisions.clone(),
        blocks: raw
            .blocks
            .iter()
            .map(|block| ElabBlock {
                id: block.id,
                kind: BlockKind::Normal,
                drops: Vec::new(),
                successor: None,
            })
            .collect(),
        drop_plans: Vec::new(),
        coroutine: None,
        lambda_captures: Vec::new(),
    };
    let checked = CheckedMirFunction {
        name: raw.name.clone(),
        key: raw.key.clone(),
        return_ty: raw.return_ty.clone(),
        blocks: raw.blocks.clone(),
        decisions: raw.decisions.clone(),
        checks: Vec::new(),
        cooperate_sites: Vec::new(),
        ownership_elaboration: Some(Box::new(elaborated.clone())),
    };
    (checked, elaborated)
}

/// Complete a hand-built test pipeline with exact-key Checked and Elaborated
/// artifacts. Existing explicit stage fixtures retain authority.
pub(crate) fn complete_stages(mut pipeline: IrPipeline) -> IrPipeline {
    for raw in &pipeline.raw_mir {
        let (mut checked, derived_elaborated) = stages_for(raw);
        let elaborated = pipeline
            .elaborated_mir
            .iter()
            .find(|candidate| candidate.key == raw.key)
            .cloned()
            .unwrap_or(derived_elaborated);
        if !pipeline
            .elaborated_mir
            .iter()
            .any(|candidate| candidate.key == raw.key)
        {
            pipeline.elaborated_mir.push(elaborated.clone());
        }
        if !pipeline
            .checked_mir
            .iter()
            .any(|candidate| candidate.key == raw.key)
        {
            checked.ownership_elaboration = Some(Box::new(elaborated));
            pipeline.checked_mir.push(checked);
        }
    }
    pipeline
}

/// Type-check `program` so HIR lowering sees the checker's declaration
/// identities.
///
/// These harnesses assert emitted shape below the checker, but HIR resolves
/// every item through `TypeCheckOutput::identity` and fails closed when that
/// view is empty, so the checker still has to run to mint the identities.
#[allow(
    dead_code,
    reason = "used by the harnesses that lower source, not by every test root"
)]
pub fn checker_output(program: &hew_parser::ast::Program) -> hew_types::TypeCheckOutput {
    hew_types::Checker::new(hew_types::module_registry::ModuleRegistry::new(Vec::new()))
        .check_program(program)
}
