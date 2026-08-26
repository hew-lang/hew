//! SIR → raw-MIR lowering for the initial value/CFG execution slice.
//!
//! This module is intentionally the *lowering boundary*, rather than a
//! dependency of `hew-sir`: SIR owns semantic values and block arguments;
//! raw MIR owns addressable storage, trap CFG, and the backend ABI.  The first
//! executable subset is deliberately narrow and rejects a function as a unit
//! when it would need ownership, aggregate layout, call ABI, or suspension
//! facts that SIR does not yet carry.

use std::collections::{BTreeMap, BTreeSet};

use hew_parser::ast::{BinaryOp, UnaryOp};
use hew_sir::{
    BlockArg, BlockId, Edge, FunctionSourceOrigin, Operand, SemBlock, SemFunction, SemModule,
    SemOp, SemOpKind, SemTerminator, UseMode, ValueDef, ValueId,
};
#[cfg(test)]
use hew_types::DefId;
use hew_types::ResolvedTy;

use crate::{
    dataflow, BasicBlock, CheckedMirFunction, FunctionCallConv, Instr, IntArithOp, IntSignedness,
    IrPipeline, ModuleCapabilities, Place, RawMirFunction, Strategy, Terminator, TrapKind,
};

/// The result of lowering one SIR function into the existing raw/checked MIR
/// boundary.  No elaborated MIR is produced for this slice: it accepts only
/// scalar, non-owning values, and the code generator deliberately accepts a
/// missing elaborated entry as an empty drop-plan set.
#[derive(Debug, Clone, PartialEq)]
pub struct SirMirLowered {
    pub raw: RawMirFunction,
    pub checked: CheckedMirFunction,
}

/// A conservative refusal to lower a SIR function through the first
/// executable SIR boundary.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SirMirLoweringError {
    pub reason: String,
}

impl SirMirLoweringError {
    fn unsupported(reason: impl Into<String>) -> Self {
        Self {
            reason: reason.into(),
        }
    }
}

impl std::fmt::Display for SirMirLoweringError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(&self.reason)
    }
}

impl std::error::Error for SirMirLoweringError {}

/// Per-function outcome when a SIR module is applied to an existing MIR
/// pipeline.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SirMirLoweringStatus {
    Lowered,
    Unsupported { reason: String },
}

/// Deterministic summary of a SIR → MIR application attempt.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct SirMirLoweringReport {
    pub statuses: Vec<(String, SirMirLoweringStatus)>,
}

impl SirMirLoweringReport {
    #[must_use]
    pub fn lowered_count(&self) -> usize {
        self.statuses
            .iter()
            .filter(|(_, status)| matches!(status, SirMirLoweringStatus::Lowered))
            .count()
    }
}

/// Lower every executable SIR function possible into `pipeline`.
///
/// The caller normally begins with the established HIR → MIR pipeline. This
/// adapter replaces only independently lowerable scalar/CFG functions,
/// preserves every module-level layout/runtime fact, and keeps the established
/// raw/checked MIR for all other functions while the cutover is in progress.
/// Replaced functions deliberately lose their prior elaborated entry:
/// retaining a drop plan authored for a different CFG would be unsound, while
/// this value-only slice requires no drops.
#[allow(
    clippy::too_many_lines,
    reason = "the temporary bridge keeps module verification, per-function eligibility, and atomic pipeline replacement together so a partial candidate cannot obscure the transition boundary"
)]
#[must_use]
pub fn apply_sir_to_pipeline(
    module: &SemModule,
    pipeline: &mut IrPipeline,
) -> SirMirLoweringReport {
    let mut report = SirMirLoweringReport::default();
    if let Some(diagnostic) = hew_sir::verify_module(module).into_iter().next() {
        let reason = format!(
            "SIR module verifier rejected the candidate before MIR realization: {:?}",
            diagnostic.kind
        );
        report
            .statuses
            .extend(module.functions.iter().map(|function| {
                (
                    function.name.clone(),
                    SirMirLoweringStatus::Unsupported {
                        reason: reason.clone(),
                    },
                )
            }));
        return report;
    }
    let mut changed = false;

    for function in &module.functions {
        let matching_raw: Vec<_> = pipeline
            .raw_mir
            .iter()
            .enumerate()
            .filter_map(|(index, raw)| (raw.name == function.name).then_some(index))
            .collect();
        let [raw_index] = matching_raw.as_slice() else {
            let reason = if matching_raw.is_empty() {
                "no unique established raw-MIR function matches this SIR function"
            } else {
                "multiple established raw-MIR functions match this SIR function"
            };
            report.statuses.push((
                function.name.clone(),
                SirMirLoweringStatus::Unsupported {
                    reason: reason.to_string(),
                },
            ));
            continue;
        };

        let matching_checked: Vec<_> = pipeline
            .checked_mir
            .iter()
            .enumerate()
            .filter_map(|(index, checked)| (checked.name == function.name).then_some(index))
            .collect();
        if !pipeline.checked_mir.is_empty() && matching_checked.len() != 1 {
            report.statuses.push((
                function.name.clone(),
                SirMirLoweringStatus::Unsupported {
                    reason: "established checked MIR does not have one matching function"
                        .to_string(),
                },
            ));
            continue;
        }
        let template = pipeline.raw_mir[*raw_index].clone();
        match lower_sir_function(function, &template) {
            Ok(mut lowered) => {
                // The initial scalar SIR subset is acyclic and has no calls,
                // so it creates no scheduler sites of its own.  Preserve the
                // established site plan exactly during this transition rather
                // than deriving one from raw block numbering: SIR edge
                // forwarders are deliberately inserted after their targets,
                // which used to look like false back-edges to the legacy
                // numeric heuristic.  A later SIR effect/CFG scheduling pass
                // replaces this bridge; it is not a second long-term policy.
                if let Some(&checked_index) = matching_checked.first() {
                    let established_sites = &pipeline.checked_mir[checked_index].cooperate_sites;
                    if !cooperate_sites_apply_to(&lowered.raw, established_sites) {
                        report.statuses.push((
                            function.name.clone(),
                            SirMirLoweringStatus::Unsupported {
                                reason: "established scheduler cooperate sites do not map to the SIR-realized CFG"
                                    .to_string(),
                            },
                        ));
                        continue;
                    }
                    lowered
                        .checked
                        .cooperate_sites
                        .clone_from(established_sites);
                } else if !lowered.checked.cooperate_sites.is_empty() {
                    report.statuses.push((
                        function.name.clone(),
                        SirMirLoweringStatus::Unsupported {
                            reason: "SIR realization would introduce scheduler cooperate sites without a scheduling-fact bridge"
                                .to_string(),
                        },
                    ));
                    continue;
                }
                pipeline.raw_mir[*raw_index] = lowered.raw;
                if let Some(&checked_index) = matching_checked.first() {
                    pipeline.checked_mir[checked_index] = lowered.checked;
                }
                pipeline
                    .elaborated_mir
                    .retain(|elaborated| elaborated.name != function.name);
                report
                    .statuses
                    .push((function.name.clone(), SirMirLoweringStatus::Lowered));
                changed = true;
            }
            Err(error) => report.statuses.push((
                function.name.clone(),
                SirMirLoweringStatus::Unsupported {
                    reason: error.reason,
                },
            )),
        }
    }

    if changed {
        pipeline.capabilities =
            ModuleCapabilities::from_raw_mir(&pipeline.raw_mir, &pipeline.extern_decls);
        pipeline.lint_warnings = crate::liveness::run_mir_lints(&pipeline.raw_mir);
        pipeline.debug_assert_capabilities_current();
    }
    report
}

fn cooperate_sites_apply_to(raw: &RawMirFunction, sites: &[crate::CooperateSite]) -> bool {
    sites.iter().all(|site| {
        raw.blocks.iter().any(|block| block.id == site.bb_id)
            && match site.kind {
                crate::CooperateKind::FunctionEntry => site.bb_id == 0,
                // The first executable SIR slice rejects cyclic CFGs, so a
                // legacy loop site cannot truthfully survive its realization.
                crate::CooperateKind::LoopBackEdge => false,
            }
    })
}

/// Lower a verified semantic SSA function to scalar raw MIR using an
/// established raw function as ABI/parameter-boundary template.
///
/// The template is not an instruction source: its body, locals, statements,
/// decisions, debug-local names, and drop plans are discarded. It supplies
/// only ABI identity that this first SIR slice does not yet preserve (the
/// default-call ABI classification and finalized parameter boundary facts).
/// SIR itself carries the source span and source origin. Those parameter facts
/// are copied exactly into both replacement raw/checked functions; no
/// source-operation ownership decisions are copied onto SIR-generated storage.
///
/// # Errors
///
/// Returns an explicit unsupported reason whenever the function needs a
/// semantic fact not carried by the initial SIR value/CFG subset.
pub fn lower_sir_function(
    function: &SemFunction,
    template: &RawMirFunction,
) -> Result<SirMirLowered, SirMirLoweringError> {
    if let Some(diagnostic) = hew_sir::verify_function(function).into_iter().next() {
        return Err(SirMirLoweringError::unsupported(format!(
            "SIR verifier rejected function `{}`: {:?}",
            function.name, diagnostic.kind
        )));
    }
    let parameter_decisions = validate_template(function, template)?;
    let collected = CollectedValues::from_function(function)?;
    let mut lowerer = RawLowerer::new(function, collected)?;
    for block in &function.blocks {
        lowerer.lower_block(block)?;
    }
    let (locals, blocks) = lowerer.finish()?;

    let raw = RawMirFunction {
        name: template.name.clone(),
        return_ty: function.return_ty.clone(),
        call_conv: template.call_conv,
        params: function
            .params
            .iter()
            .map(|param| param.ty.clone())
            .collect(),
        locals,
        // SIR records operation provenance, but it intentionally does not yet
        // carry source binding identity/lexical scopes. Leaving these empty is
        // truthful; copying HIR-authored debug metadata onto different storage
        // would create a false debugger view.
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks,
        decisions: parameter_decisions.clone(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),
        lambda_actor_user_param_locals: Vec::new(),
        span: Some((
            u32::try_from(function.span.start).unwrap_or(u32::MAX),
            u32::try_from(function.span.end).unwrap_or(u32::MAX),
        )),
        instr_spans: std::collections::BTreeMap::new(),
        source_origin: raw_source_origin(&function.source_origin),
    };
    let checked = CheckedMirFunction {
        name: raw.name.clone(),
        return_ty: raw.return_ty.clone(),
        blocks: raw.blocks.clone(),
        decisions: parameter_decisions,
        checks: crate::validate_context_markers(&raw),
        cooperate_sites: dataflow::compute_cooperate_sites(&raw.blocks),
    };
    Ok(SirMirLowered { raw, checked })
}

fn raw_source_origin(origin: &FunctionSourceOrigin) -> crate::SourceOrigin {
    match origin {
        FunctionSourceOrigin::RootUnit => crate::SourceOrigin::RootUnit,
        FunctionSourceOrigin::Foreign(module) => crate::SourceOrigin::Foreign(module.clone()),
        FunctionSourceOrigin::Unknown => crate::SourceOrigin::Unknown,
    }
}

fn validate_template(
    function: &SemFunction,
    template: &RawMirFunction,
) -> Result<Vec<crate::DecisionFact>, SirMirLoweringError> {
    if template.name != function.name {
        return Err(SirMirLoweringError::unsupported(
            "SIR function name does not match the established raw-MIR ABI template",
        ));
    }
    if template.call_conv != FunctionCallConv::Default {
        return Err(SirMirLoweringError::unsupported(
            "only ordinary default-call-convention functions are executable through initial SIR lowering",
        ));
    }
    if template.intrinsic_id.is_some()
        || !template.await_deadline_ns.is_empty()
        || !template.suspend_kinds.is_empty()
        || !template.lambda_actor_user_param_locals.is_empty()
    {
        return Err(SirMirLoweringError::unsupported(
            "functions carrying intrinsic, suspension, or actor ABI facts remain on the established MIR path",
        ));
    }
    if template.return_ty != function.return_ty {
        return Err(SirMirLoweringError::unsupported(
            "SIR return type does not match the established raw-MIR ABI template",
        ));
    }
    let sir_params: Vec<_> = function.params.iter().map(|param| &param.ty).collect();
    let template_params: Vec<_> = template.params.iter().collect();
    if sir_params != template_params {
        return Err(SirMirLoweringError::unsupported(
            "SIR parameter types do not match the established raw-MIR ABI template",
        ));
    }
    if !matches!(function.entry, BlockId(0)) {
        return Err(SirMirLoweringError::unsupported(
            "the initial raw-MIR bridge requires SIR entry block bb0",
        ));
    }
    if !is_supported_return_type(&function.return_ty) {
        return Err(SirMirLoweringError::unsupported(format!(
            "return type `{}` is not yet a scalar SIR-to-MIR value",
            function.return_ty.user_facing()
        )));
    }
    if sir_cfg_has_cycle(function) {
        return Err(SirMirLoweringError::unsupported(
            "cyclic SIR CFGs remain deferred until scheduler sites derive from SIR effects and CFG rather than legacy statement counts",
        ));
    }
    template_parameter_decisions(function, template)
}

fn template_parameter_decisions(
    function: &SemFunction,
    template: &RawMirFunction,
) -> Result<Vec<crate::DecisionFact>, SirMirLoweringError> {
    let mut facts = template
        .decisions
        .iter()
        .filter(|decision| matches!(decision.strategy, Strategy::ParamBoundary(_)))
        .cloned()
        .collect::<Vec<_>>();
    facts.sort_unstable_by_key(|decision| match decision.strategy {
        Strategy::ParamBoundary(fact) => fact.param_index,
        _ => unreachable!("filtered to parameter-boundary decisions"),
    });
    if facts.len() != function.params.len() {
        return Err(SirMirLoweringError::unsupported(
            "established raw-MIR ABI template lacks one finalized parameter-boundary fact per SIR parameter",
        ));
    }
    let parameter_count = u32::try_from(function.params.len()).map_err(|_| {
        SirMirLoweringError::unsupported("SIR parameter count exceeds raw-MIR ABI limits")
    })?;
    for (expected_index, (decision, parameter)) in facts.iter().zip(&function.params).enumerate() {
        let Strategy::ParamBoundary(fact) = decision.strategy else {
            unreachable!("filtered to parameter-boundary decisions");
        };
        if fact.param_index != u32::try_from(expected_index).expect("index fits after count check")
            || fact.param_count != parameter_count
            || decision.ty != parameter.ty
        {
            return Err(SirMirLoweringError::unsupported(
                "established parameter-boundary facts do not match the SIR ABI signature",
            ));
        }
    }
    Ok(facts)
}

fn sir_cfg_has_cycle(function: &SemFunction) -> bool {
    let mut states = BTreeMap::<BlockId, VisitState>::new();
    let by_id = function
        .blocks
        .iter()
        .map(|block| (block.id, block))
        .collect::<BTreeMap<_, _>>();
    has_cycle_from(function.entry, &by_id, &mut states)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VisitState {
    Visiting,
    Visited,
}

fn has_cycle_from(
    block_id: BlockId,
    by_id: &BTreeMap<BlockId, &SemBlock>,
    states: &mut BTreeMap<BlockId, VisitState>,
) -> bool {
    match states.get(&block_id) {
        Some(VisitState::Visiting) => return true,
        Some(VisitState::Visited) => return false,
        None => {}
    }
    states.insert(block_id, VisitState::Visiting);
    let Some(block) = by_id.get(&block_id) else {
        // The SIR verifier diagnoses an unknown successor.  Keep this adapter
        // failure separate and let its normal edge validation report it.
        states.insert(block_id, VisitState::Visited);
        return false;
    };
    for edge in block.terminator.successors() {
        if has_cycle_from(edge.target, by_id, states) {
            return true;
        }
    }
    states.insert(block_id, VisitState::Visited);
    false
}

fn is_supported_return_type(ty: &ResolvedTy) -> bool {
    matches!(ty, ResolvedTy::Unit) || is_supported_value_type(ty)
}

fn is_supported_value_type(ty: &ResolvedTy) -> bool {
    ty.is_integer() || matches!(ty, ResolvedTy::Bool)
}

#[derive(Debug)]
struct CollectedValues {
    types: BTreeMap<ValueId, ResolvedTy>,
    block_args: BTreeMap<BlockId, Vec<BlockArg>>,
}

impl CollectedValues {
    fn from_function(function: &SemFunction) -> Result<Self, SirMirLoweringError> {
        let mut types = BTreeMap::new();
        let mut block_args = BTreeMap::new();
        for parameter in &function.params {
            insert_value(&mut types, parameter.value, &parameter.ty)?;
        }
        if function.blocks.is_empty() {
            return Err(SirMirLoweringError::unsupported(
                "SIR function has no basic blocks",
            ));
        }
        for (index, block) in function.blocks.iter().enumerate() {
            if block.id.0
                != u32::try_from(index).map_err(|_| {
                    SirMirLoweringError::unsupported("SIR block count exceeds raw-MIR limits")
                })?
            {
                return Err(SirMirLoweringError::unsupported(
                    "the initial raw-MIR bridge requires SIR blocks ordered by contiguous id",
                ));
            }
            if block_args.insert(block.id, block.args.clone()).is_some() {
                return Err(SirMirLoweringError::unsupported(
                    "SIR function contains duplicate basic-block ids",
                ));
            }
            if block.id == function.entry && !block.args.is_empty() {
                return Err(SirMirLoweringError::unsupported(
                    "SIR entry block must not carry block arguments; SemFunction.params define entry values",
                ));
            }
            for argument in &block.args {
                insert_value(&mut types, argument.value, &argument.ty)?;
            }
            for operation in &block.ops {
                for result in &operation.results {
                    insert_value(&mut types, result.id, &result.ty)?;
                }
            }
        }
        for (value, ty) in &types {
            if !is_supported_value_type(ty) {
                return Err(SirMirLoweringError::unsupported(format!(
                    "SSA value %{} of type `{}` needs aggregate, reference, or ownership lowering",
                    value.0,
                    ty.user_facing()
                )));
            }
        }
        Ok(Self { types, block_args })
    }
}

fn insert_value(
    values: &mut BTreeMap<ValueId, ResolvedTy>,
    value: ValueId,
    ty: &ResolvedTy,
) -> Result<(), SirMirLoweringError> {
    if values.insert(value, ty.clone()).is_some() {
        return Err(SirMirLoweringError::unsupported(format!(
            "SIR value %{} is defined more than once",
            value.0
        )));
    }
    Ok(())
}

#[derive(Debug)]
struct PendingBlock {
    id: u32,
    instructions: Vec<Instr>,
    terminator: Option<Terminator>,
}

struct RawLowerer<'a> {
    function: &'a SemFunction,
    value_types: BTreeMap<ValueId, ResolvedTy>,
    block_args: BTreeMap<BlockId, Vec<BlockArg>>,
    value_places: BTreeMap<ValueId, Place>,
    locals: Vec<ResolvedTy>,
    blocks: Vec<PendingBlock>,
    current: u32,
}

impl<'a> RawLowerer<'a> {
    fn new(
        function: &'a SemFunction,
        collected: CollectedValues,
    ) -> Result<Self, SirMirLoweringError> {
        let mut locals = Vec::new();
        let mut value_places = BTreeMap::new();
        for parameter in &function.params {
            let local = u32::try_from(locals.len())
                .map_err(|_| SirMirLoweringError::unsupported("raw-MIR local count exceeds u32"))?;
            locals.push(parameter.ty.clone());
            value_places.insert(parameter.value, Place::Local(local));
        }
        for (value, ty) in &collected.types {
            if value_places.contains_key(value) {
                continue;
            }
            let local = u32::try_from(locals.len())
                .map_err(|_| SirMirLoweringError::unsupported("raw-MIR local count exceeds u32"))?;
            locals.push(ty.clone());
            value_places.insert(*value, Place::Local(local));
        }
        let blocks = function
            .blocks
            .iter()
            .map(|block| PendingBlock {
                id: block.id.0,
                instructions: Vec::new(),
                terminator: None,
            })
            .collect();
        Ok(Self {
            function,
            value_types: collected.types,
            block_args: collected.block_args,
            value_places,
            locals,
            blocks,
            current: function.entry.0,
        })
    }

    fn lower_block(&mut self, block: &SemBlock) -> Result<(), SirMirLoweringError> {
        self.current = block.id.0;
        for operation in &block.ops {
            self.lower_op(operation)?;
        }
        self.lower_terminator(&block.terminator)
    }

    fn lower_op(&mut self, operation: &SemOp) -> Result<(), SirMirLoweringError> {
        let (result, result_ty) = Self::single_result(operation)?;
        let dest = self.value_place(result)?;
        match &operation.kind {
            SemOpKind::ConstI64(value) => {
                if !result_ty.is_integer() {
                    return Err(SirMirLoweringError::unsupported(format!(
                        "integer constant result %{} has non-integer type `{}`",
                        result.0,
                        result_ty.user_facing()
                    )));
                }
                self.push(Instr::ConstI64 {
                    dest,
                    value: *value,
                });
            }
            SemOpKind::ConstBool(value) => {
                if result_ty != ResolvedTy::Bool {
                    return Err(SirMirLoweringError::unsupported(format!(
                        "boolean constant result %{} has type `{}` rather than bool",
                        result.0,
                        result_ty.user_facing()
                    )));
                }
                self.push(Instr::ConstI64 {
                    dest,
                    value: i64::from(*value),
                });
            }
            SemOpKind::Unary { op, value } => self.lower_unary(*op, value, dest, &result_ty)?,
            SemOpKind::Binary { op, lhs, rhs } => {
                self.lower_binary(*op, lhs, rhs, dest, &result_ty)?;
            }
            SemOpKind::Cast { value, to } => {
                Self::require_read(value, "cast")?;
                let from_ty = self.value_type(value.value)?.clone();
                if to != &result_ty || !from_ty.can_explicitly_numeric_cast_to(to) {
                    return Err(SirMirLoweringError::unsupported(
                        "SIR cast does not carry a checker-admitted scalar numeric conversion",
                    ));
                }
                self.push(Instr::NumericCast {
                    dest,
                    src: self.value_place(value.value)?,
                    from_ty,
                    to_ty: to.clone(),
                });
            }
            SemOpKind::Call { .. } => {
                return Err(SirMirLoweringError::unsupported(
                    "direct calls remain on the established MIR path until SIR carries resolved emitted-symbol and ABI authority",
                ));
            }
        }
        Ok(())
    }

    fn lower_unary(
        &mut self,
        op: UnaryOp,
        operand: &Operand,
        dest: Place,
        result_ty: &ResolvedTy,
    ) -> Result<(), SirMirLoweringError> {
        Self::require_read(operand, "unary operation")?;
        let operand_ty = self.value_type(operand.value)?.clone();
        let operand_place = self.value_place(operand.value)?;
        match op {
            UnaryOp::Not if operand_ty == ResolvedTy::Bool && result_ty == &ResolvedTy::Bool => {
                self.push(Instr::BoolNot {
                    dest,
                    operand: operand_place,
                });
            }
            UnaryOp::Negate if operand_ty == *result_ty && operand_ty.is_integer() => {
                let flag = self.fresh_local(ResolvedTy::Bool)?;
                self.push(Instr::IntNegChecked {
                    signed: signedness(&operand_ty)?,
                    dest,
                    operand: operand_place,
                    overflow_flag: flag,
                });
                self.split_overflow(flag)?;
            }
            UnaryOp::BitNot if operand_ty == *result_ty && operand_ty.is_integer() => {
                self.push(Instr::IntBitNot {
                    dest,
                    operand: operand_place,
                });
            }
            UnaryOp::RawDeref | UnaryOp::Not | UnaryOp::Negate | UnaryOp::BitNot => {
                return Err(SirMirLoweringError::unsupported(format!(
                    "unary operator `{op:?}` is not in the scalar SIR-to-MIR subset"
                )));
            }
        }
        Ok(())
    }

    #[allow(
        clippy::too_many_lines,
        reason = "the initial adapter intentionally uses one exhaustive operator map so its supported semantics are auditable at the SIR→MIR boundary"
    )]
    fn lower_binary(
        &mut self,
        op: BinaryOp,
        lhs: &Operand,
        rhs: &Operand,
        dest: Place,
        result_ty: &ResolvedTy,
    ) -> Result<(), SirMirLoweringError> {
        Self::require_read(lhs, "binary operation")?;
        Self::require_read(rhs, "binary operation")?;
        let lhs_ty = self.value_type(lhs.value)?.clone();
        let rhs_ty = self.value_type(rhs.value)?.clone();
        let lhs_place = self.value_place(lhs.value)?;
        let rhs_place = self.value_place(rhs.value)?;

        let comparison = match op {
            BinaryOp::Equal => Some(crate::CmpPred::Eq),
            BinaryOp::NotEqual => Some(crate::CmpPred::NotEq),
            BinaryOp::Less => Some(ordering_predicate(&lhs_ty, crate::CmpPred::SignedLess)?),
            BinaryOp::LessEqual => Some(ordering_predicate(&lhs_ty, crate::CmpPred::SignedLessEq)?),
            BinaryOp::Greater => Some(ordering_predicate(&lhs_ty, crate::CmpPred::SignedGreater)?),
            BinaryOp::GreaterEqual => Some(ordering_predicate(
                &lhs_ty,
                crate::CmpPred::SignedGreaterEq,
            )?),
            _ => None,
        };
        if let Some(pred) = comparison {
            if lhs_ty != rhs_ty || result_ty != &ResolvedTy::Bool {
                return Err(SirMirLoweringError::unsupported(
                    "comparison operands/result do not have the scalar SIR-to-MIR shape",
                ));
            }
            self.push(Instr::IntCmp {
                dest,
                pred,
                lhs: lhs_place,
                rhs: rhs_place,
            });
            return Ok(());
        }

        if lhs_ty != rhs_ty || lhs_ty != *result_ty || !lhs_ty.is_integer() {
            return Err(SirMirLoweringError::unsupported(
                "binary arithmetic requires same-typed integer SIR operands and result",
            ));
        }
        match op {
            BinaryOp::WrappingAdd => self.push(Instr::IntAdd {
                dest,
                lhs: lhs_place,
                rhs: rhs_place,
            }),
            BinaryOp::WrappingSub => self.push(Instr::IntSub {
                dest,
                lhs: lhs_place,
                rhs: rhs_place,
            }),
            BinaryOp::WrappingMul => self.push(Instr::IntMul {
                dest,
                lhs: lhs_place,
                rhs: rhs_place,
            }),
            BinaryOp::BitAnd => self.push(Instr::IntBitAnd {
                dest,
                lhs: lhs_place,
                rhs: rhs_place,
            }),
            BinaryOp::BitOr => self.push(Instr::IntBitOr {
                dest,
                lhs: lhs_place,
                rhs: rhs_place,
            }),
            BinaryOp::BitXor => self.push(Instr::IntBitXor {
                dest,
                lhs: lhs_place,
                rhs: rhs_place,
            }),
            BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply => {
                let operation = match op {
                    BinaryOp::Add => IntArithOp::Add,
                    BinaryOp::Subtract => IntArithOp::Sub,
                    BinaryOp::Multiply => IntArithOp::Mul,
                    _ => unreachable!("matched above"),
                };
                let flag = self.fresh_local(ResolvedTy::Bool)?;
                self.push(Instr::IntArithChecked {
                    op: operation,
                    signed: signedness(&lhs_ty)?,
                    dest,
                    lhs: lhs_place,
                    rhs: rhs_place,
                    overflow_flag: flag,
                });
                self.split_overflow(flag)?;
            }
            BinaryOp::And | BinaryOp::Or => {
                return Err(SirMirLoweringError::unsupported(
                    "logical operators must be represented as SIR control flow, not Binary operations",
                ));
            }
            BinaryOp::Divide
            | BinaryOp::Modulo
            | BinaryOp::Shl
            | BinaryOp::Shr
            | BinaryOp::Range
            | BinaryOp::RangeInclusive => {
                return Err(SirMirLoweringError::unsupported(format!(
                    "binary operator `{op}` needs a later SIR-to-MIR legalization slice"
                )));
            }
            BinaryOp::Equal
            | BinaryOp::NotEqual
            | BinaryOp::Less
            | BinaryOp::LessEqual
            | BinaryOp::Greater
            | BinaryOp::GreaterEqual => unreachable!("handled by comparison branch"),
        }
        Ok(())
    }

    fn split_overflow(&mut self, flag: Place) -> Result<(), SirMirLoweringError> {
        let trap = self.fresh_block()?;
        let continuation = self.fresh_block()?;
        self.terminate(Terminator::Branch {
            cond: flag,
            then_target: trap,
            else_target: continuation,
        })?;
        self.current = trap;
        self.terminate(Terminator::Trap {
            kind: TrapKind::IntegerOverflow,
        })?;
        self.current = continuation;
        Ok(())
    }

    fn lower_terminator(&mut self, terminator: &SemTerminator) -> Result<(), SirMirLoweringError> {
        match terminator {
            SemTerminator::Return { value: Some(value) } => {
                if self.value_type(*value)? != &self.function.return_ty {
                    return Err(SirMirLoweringError::unsupported(
                        "SIR return value type does not match function return type",
                    ));
                }
                self.push(Instr::Move {
                    dest: Place::ReturnSlot,
                    src: self.value_place(*value)?,
                });
                self.terminate(Terminator::Return)
            }
            SemTerminator::Return { value: None } => {
                if self.function.return_ty != ResolvedTy::Unit {
                    return Err(SirMirLoweringError::unsupported(
                        "value-less SIR return is valid only for a unit-returning function",
                    ));
                }
                self.terminate(Terminator::Return)
            }
            SemTerminator::Goto(edge) => {
                let source = self.current;
                let target = self.materialize_edge(edge)?;
                self.current = source;
                self.terminate(Terminator::Goto { target })
            }
            SemTerminator::Branch {
                condition,
                then_target,
                else_target,
            } => {
                if self.value_type(*condition)? != &ResolvedTy::Bool {
                    return Err(SirMirLoweringError::unsupported(
                        "SIR branch condition must have bool type",
                    ));
                }
                let source = self.current;
                let then_target = self.materialize_edge(then_target)?;
                let else_target = self.materialize_edge(else_target)?;
                self.current = source;
                self.terminate(Terminator::Branch {
                    cond: self.value_place(*condition)?,
                    then_target,
                    else_target,
                })
            }
            SemTerminator::Unreachable => Err(SirMirLoweringError::unsupported(
                "SIR unreachable terminators remain deferred until raw MIR has a semantic unreachable terminator",
            )),
        }
    }

    /// Materialise SIR block arguments into raw-MIR locals.
    ///
    /// Raw MIR has no block arguments. A forwarding block ensures a branch
    /// writes only the selected edge, and the two-phase source→scratch then
    /// scratch→argument sequence preserves *parallel-copy* semantics for loop
    /// back edges such as `goto bb1(%b, %a)`.
    fn materialize_edge(&mut self, edge: &Edge) -> Result<u32, SirMirLoweringError> {
        let target_args = self
            .block_args
            .get(&edge.target)
            .ok_or_else(|| SirMirLoweringError::unsupported("SIR edge targets an unknown block"))?
            .clone();
        if target_args.len() != edge.args.len() {
            return Err(SirMirLoweringError::unsupported(
                "SIR edge argument count does not match target block arguments",
            ));
        }
        if edge.args.is_empty() {
            return Ok(edge.target.0);
        }
        let forwarding = self.fresh_block()?;
        self.current = forwarding;
        let mut copies = Vec::with_capacity(edge.args.len());
        for (source, target) in edge.args.iter().zip(&target_args) {
            let source_ty = self.value_type(*source)?;
            if source_ty != &target.ty {
                return Err(SirMirLoweringError::unsupported(
                    "SIR edge argument type does not match target block argument",
                ));
            }
            let scratch = self.fresh_local(source_ty.clone())?;
            self.push(Instr::Move {
                dest: scratch,
                src: self.value_place(*source)?,
            });
            copies.push((scratch, self.value_place(target.value)?));
        }
        for (scratch, destination) in copies {
            self.push(Instr::Move {
                dest: destination,
                src: scratch,
            });
        }
        self.terminate(Terminator::Goto {
            target: edge.target.0,
        })?;
        Ok(forwarding)
    }

    fn single_result(operation: &SemOp) -> Result<(ValueId, ResolvedTy), SirMirLoweringError> {
        let [ValueDef { id, ty }] = operation.results.as_slice() else {
            return Err(SirMirLoweringError::unsupported(
                "the initial SIR-to-MIR bridge requires exactly one result per operation",
            ));
        };
        Ok((*id, ty.clone()))
    }

    fn require_read(operand: &Operand, context: &str) -> Result<(), SirMirLoweringError> {
        if operand.mode != UseMode::Read {
            return Err(SirMirLoweringError::unsupported(format!(
                "{context} uses {:?}; ownership-aware SIR operand lowering is not enabled yet",
                operand.mode
            )));
        }
        Ok(())
    }

    fn value_type(&self, value: ValueId) -> Result<&ResolvedTy, SirMirLoweringError> {
        self.value_types.get(&value).ok_or_else(|| {
            SirMirLoweringError::unsupported(format!("SIR uses undefined value %{}", value.0))
        })
    }

    fn value_place(&self, value: ValueId) -> Result<Place, SirMirLoweringError> {
        self.value_places.get(&value).copied().ok_or_else(|| {
            SirMirLoweringError::unsupported(format!(
                "SIR value %{} has no materialized raw-MIR local",
                value.0
            ))
        })
    }

    fn fresh_local(&mut self, ty: ResolvedTy) -> Result<Place, SirMirLoweringError> {
        let local = u32::try_from(self.locals.len())
            .map_err(|_| SirMirLoweringError::unsupported("raw-MIR local count exceeds u32"))?;
        self.locals.push(ty);
        Ok(Place::Local(local))
    }

    fn fresh_block(&mut self) -> Result<u32, SirMirLoweringError> {
        let id = u32::try_from(self.blocks.len())
            .map_err(|_| SirMirLoweringError::unsupported("raw-MIR block count exceeds u32"))?;
        self.blocks.push(PendingBlock {
            id,
            instructions: Vec::new(),
            terminator: None,
        });
        Ok(id)
    }

    fn push(&mut self, instruction: Instr) {
        self.current_block_mut().instructions.push(instruction);
    }

    fn terminate(&mut self, terminator: Terminator) -> Result<(), SirMirLoweringError> {
        let block = self.current_block_mut();
        if block.terminator.replace(terminator).is_some() {
            return Err(SirMirLoweringError::unsupported(format!(
                "raw-MIR block bb{} received more than one terminator",
                block.id
            )));
        }
        Ok(())
    }

    fn current_block_mut(&mut self) -> &mut PendingBlock {
        &mut self.blocks[self.current as usize]
    }

    fn finish(self) -> Result<(Vec<ResolvedTy>, Vec<BasicBlock>), SirMirLoweringError> {
        let mut seen = BTreeSet::new();
        let mut blocks = Vec::with_capacity(self.blocks.len());
        for block in self.blocks {
            if !seen.insert(block.id) {
                return Err(SirMirLoweringError::unsupported(
                    "raw-MIR bridge constructed duplicate block ids",
                ));
            }
            let terminator = block.terminator.ok_or_else(|| {
                SirMirLoweringError::unsupported(format!(
                    "raw-MIR bridge left bb{} without a terminator",
                    block.id
                ))
            })?;
            blocks.push(BasicBlock {
                id: block.id,
                statements: Vec::new(),
                instructions: block.instructions,
                terminator,
            });
        }
        Ok((self.locals, blocks))
    }
}

fn signedness(ty: &ResolvedTy) -> Result<IntSignedness, SirMirLoweringError> {
    if ty.is_signed_integer() {
        Ok(IntSignedness::Signed)
    } else if ty.is_unsigned_integer() {
        Ok(IntSignedness::Unsigned)
    } else {
        Err(SirMirLoweringError::unsupported(format!(
            "type `{}` is not an integer with a MIR signedness",
            ty.user_facing()
        )))
    }
}

fn ordering_predicate(
    ty: &ResolvedTy,
    signed: crate::CmpPred,
) -> Result<crate::CmpPred, SirMirLoweringError> {
    if ty.is_signed_integer() {
        return Ok(signed);
    }
    if !ty.is_unsigned_integer() {
        return Err(SirMirLoweringError::unsupported(
            "ordered SIR comparisons require integer operands",
        ));
    }
    match signed {
        crate::CmpPred::SignedLess => Ok(crate::CmpPred::UnsignedLess),
        crate::CmpPred::SignedLessEq => Ok(crate::CmpPred::UnsignedLessEq),
        crate::CmpPred::SignedGreater => Ok(crate::CmpPred::UnsignedGreater),
        crate::CmpPred::SignedGreaterEq => Ok(crate::CmpPred::UnsignedGreaterEq),
        _ => unreachable!("ordering_predicate accepts a signed ordering predicate"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hew_hir::{IntentKind, ItemId, SiteId, ValueClass};
    use hew_sir::{OpId, Provenance};

    fn template(name: &str, params: Vec<ResolvedTy>, return_ty: ResolvedTy) -> RawMirFunction {
        let parameter_count = u32::try_from(params.len()).expect("test parameter count fits u32");
        let decisions = params
            .iter()
            .enumerate()
            .map(|(index, ty)| crate::DecisionFact {
                site: SiteId(u32::try_from(index).expect("test parameter index fits u32")),
                ty: ty.clone(),
                value_class: ValueClass::BitCopy,
                intent: IntentKind::Unknown,
                strategy: Strategy::ParamBoundary(crate::ParamBoundaryFact {
                    param_index: u32::try_from(index).expect("test parameter index fits u32"),
                    param_count: parameter_count,
                    caller_visible_projection: false,
                    mode: crate::ParamBoundaryMode::BorrowReadOnly,
                }),
                why: "test parameter boundary".to_string(),
            })
            .collect();
        RawMirFunction {
            name: name.to_string(),
            return_ty,
            call_conv: FunctionCallConv::Default,
            params,
            locals: Vec::new(),
            local_names: Vec::new(),
            local_scopes: Vec::new(),
            local_decl_bytes: Vec::new(),
            scope_table: Vec::new(),
            blocks: Vec::new(),
            decisions,
            intrinsic_id: None,
            await_deadline_ns: std::collections::HashMap::new(),
            suspend_kinds: std::collections::HashMap::new(),
            lambda_actor_user_param_locals: Vec::new(),
            span: None,
            instr_spans: std::collections::BTreeMap::new(),
            source_origin: crate::SourceOrigin::Unknown,
        }
    }

    fn definition(id: u32, ty: ResolvedTy) -> ValueDef {
        ValueDef {
            id: ValueId(id),
            ty,
        }
    }

    fn operand(id: u32) -> Operand {
        Operand {
            value: ValueId(id),
            mode: UseMode::Read,
        }
    }

    fn op(id: u32, result: ValueDef, kind: SemOpKind) -> SemOp {
        SemOp {
            id: OpId(id),
            results: vec![result],
            kind,
            provenance: Provenance::Synthesized,
        }
    }

    #[test]
    #[allow(
        clippy::too_many_lines,
        reason = "the complete diamond pins SIR block arguments, checked arithmetic legalization, and raw CFG construction together"
    )]
    fn realizes_ssa_diamond_into_raw_cfg_and_overflow_paths() {
        let function = SemFunction {
            id: ItemId(0),
            declaration: DefId::for_test("f"),
            name: "f".to_string(),
            span: 12..96,
            source_origin: FunctionSourceOrigin::RootUnit,
            params: vec![
                BlockArg {
                    value: ValueId(0),
                    ty: ResolvedTy::I64,
                },
                BlockArg {
                    value: ValueId(1),
                    ty: ResolvedTy::I64,
                },
            ],
            return_ty: ResolvedTy::I64,
            entry: BlockId(0),
            blocks: vec![
                SemBlock {
                    id: BlockId(0),
                    args: Vec::new(),
                    ops: vec![
                        op(0, definition(2, ResolvedTy::I64), SemOpKind::ConstI64(0)),
                        op(
                            1,
                            definition(3, ResolvedTy::Bool),
                            SemOpKind::Binary {
                                op: BinaryOp::Greater,
                                lhs: operand(0),
                                rhs: operand(2),
                            },
                        ),
                    ],
                    terminator: SemTerminator::Branch {
                        condition: ValueId(3),
                        then_target: Edge {
                            target: BlockId(1),
                            args: vec![ValueId(1)],
                        },
                        else_target: Edge {
                            target: BlockId(2),
                            args: vec![ValueId(1)],
                        },
                    },
                },
                SemBlock {
                    id: BlockId(1),
                    args: vec![BlockArg {
                        value: ValueId(4),
                        ty: ResolvedTy::I64,
                    }],
                    ops: vec![
                        op(2, definition(5, ResolvedTy::I64), SemOpKind::ConstI64(1)),
                        op(
                            3,
                            definition(6, ResolvedTy::I64),
                            SemOpKind::Binary {
                                op: BinaryOp::Add,
                                lhs: operand(4),
                                rhs: operand(5),
                            },
                        ),
                    ],
                    terminator: SemTerminator::Goto(Edge {
                        target: BlockId(3),
                        args: vec![ValueId(6)],
                    }),
                },
                SemBlock {
                    id: BlockId(2),
                    args: vec![BlockArg {
                        value: ValueId(7),
                        ty: ResolvedTy::I64,
                    }],
                    ops: vec![
                        op(4, definition(8, ResolvedTy::I64), SemOpKind::ConstI64(2)),
                        op(
                            5,
                            definition(9, ResolvedTy::I64),
                            SemOpKind::Binary {
                                op: BinaryOp::Add,
                                lhs: operand(7),
                                rhs: operand(8),
                            },
                        ),
                    ],
                    terminator: SemTerminator::Goto(Edge {
                        target: BlockId(3),
                        args: vec![ValueId(9)],
                    }),
                },
                SemBlock {
                    id: BlockId(3),
                    args: vec![BlockArg {
                        value: ValueId(10),
                        ty: ResolvedTy::I64,
                    }],
                    ops: vec![
                        op(6, definition(11, ResolvedTy::I64), SemOpKind::ConstI64(3)),
                        op(
                            7,
                            definition(12, ResolvedTy::I64),
                            SemOpKind::Binary {
                                op: BinaryOp::Multiply,
                                lhs: operand(10),
                                rhs: operand(11),
                            },
                        ),
                    ],
                    terminator: SemTerminator::Return {
                        value: Some(ValueId(12)),
                    },
                },
            ],
        };

        let lowered = lower_sir_function(
            &function,
            &template("f", vec![ResolvedTy::I64, ResolvedTy::I64], ResolvedTy::I64),
        )
        .expect("the scalar SSA diamond should lower to raw MIR");

        assert!(lowered.raw.blocks.len() > function.blocks.len());
        assert_eq!(lowered.raw.span, Some((12, 96)));
        assert_eq!(lowered.raw.source_origin, crate::SourceOrigin::RootUnit);
        assert!(lowered.raw.blocks.iter().any(|block| {
            matches!(
                block.terminator,
                Terminator::Trap {
                    kind: TrapKind::IntegerOverflow
                }
            )
        }));
        assert!(lowered.raw.blocks.iter().any(|block| {
            block.instructions.iter().any(|instruction| {
                matches!(
                    instruction,
                    Instr::IntArithChecked {
                        op: IntArithOp::Add,
                        ..
                    }
                )
            })
        }));
        assert!(lowered.raw.blocks.iter().any(|block| {
            matches!(block.terminator, Terminator::Return)
                && block.instructions.iter().any(|instruction| {
                    matches!(
                        instruction,
                        Instr::Move {
                            dest: Place::ReturnSlot,
                            ..
                        }
                    )
                })
        }));
        assert!(lowered.checked.checks.is_empty());
    }

    #[test]
    fn rejects_unverified_sir_before_raw_realization() {
        let function = SemFunction {
            id: ItemId(0),
            declaration: DefId::for_test("bad_entry"),
            name: "bad_entry".to_string(),
            span: 0..0,
            source_origin: FunctionSourceOrigin::Unknown,
            params: Vec::new(),
            return_ty: ResolvedTy::I64,
            entry: BlockId(0),
            blocks: vec![SemBlock {
                id: BlockId(0),
                args: vec![BlockArg {
                    value: ValueId(0),
                    ty: ResolvedTy::I64,
                }],
                ops: Vec::new(),
                terminator: SemTerminator::Return {
                    value: Some(ValueId(0)),
                },
            }],
        };

        let error = lower_sir_function(
            &function,
            &template("bad_entry", Vec::new(), ResolvedTy::I64),
        )
        .expect_err("a malformed SIR entry must fail before raw MIR exists");
        assert!(error.reason.contains("SIR verifier rejected function"));
        assert!(error.reason.contains("EntryBlockArgs"));
    }

    #[test]
    fn edge_argument_materialization_uses_parallel_copies() {
        let function = SemFunction {
            id: ItemId(0),
            declaration: DefId::for_test("swap"),
            name: "swap".to_string(),
            span: 0..0,
            source_origin: FunctionSourceOrigin::Unknown,
            params: vec![
                BlockArg {
                    value: ValueId(0),
                    ty: ResolvedTy::I64,
                },
                BlockArg {
                    value: ValueId(1),
                    ty: ResolvedTy::I64,
                },
                BlockArg {
                    value: ValueId(2),
                    ty: ResolvedTy::Bool,
                },
            ],
            return_ty: ResolvedTy::I64,
            entry: BlockId(0),
            blocks: vec![
                SemBlock {
                    id: BlockId(0),
                    args: Vec::new(),
                    ops: Vec::new(),
                    terminator: SemTerminator::Branch {
                        condition: ValueId(2),
                        then_target: Edge {
                            target: BlockId(1),
                            args: vec![ValueId(1), ValueId(0)],
                        },
                        else_target: Edge {
                            target: BlockId(1),
                            args: vec![ValueId(0), ValueId(1)],
                        },
                    },
                },
                SemBlock {
                    id: BlockId(1),
                    args: vec![
                        BlockArg {
                            value: ValueId(3),
                            ty: ResolvedTy::I64,
                        },
                        BlockArg {
                            value: ValueId(4),
                            ty: ResolvedTy::I64,
                        },
                    ],
                    ops: Vec::new(),
                    terminator: SemTerminator::Return {
                        value: Some(ValueId(3)),
                    },
                },
            ],
        };
        let lowered = lower_sir_function(
            &function,
            &template(
                "swap",
                vec![ResolvedTy::I64, ResolvedTy::I64, ResolvedTy::Bool],
                ResolvedTy::I64,
            ),
        )
        .expect("a scalar branch with block arguments should lower");

        let forwarders: Vec<_> = lowered
            .raw
            .blocks
            .iter()
            .filter(|block| {
                matches!(block.terminator, Terminator::Goto { target: 1 })
                    && block.instructions.len() == 4
            })
            .collect();
        assert_eq!(forwarders.len(), 2, "one forwarding block per branch edge");
        for block in forwarders {
            let first_phase = &block.instructions[..2];
            let second_phase = &block.instructions[2..];
            assert!(first_phase.iter().all(|instruction| matches!(
                instruction,
                Instr::Move {
                    dest: Place::Local(destination),
                    ..
                } if *destination >= 5
            )));
            assert!(second_phase.iter().all(|instruction| matches!(
                instruction,
                Instr::Move {
                    dest: Place::Local(destination),
                    ..
                } if *destination == 3 || *destination == 4
            )));
        }
    }

    #[test]
    fn replacing_a_scalar_body_removes_stale_elaboration() {
        let function = SemFunction {
            id: ItemId(0),
            declaration: DefId::for_test("constant"),
            name: "constant".to_string(),
            span: 0..0,
            source_origin: FunctionSourceOrigin::Unknown,
            params: Vec::new(),
            return_ty: ResolvedTy::I64,
            entry: BlockId(0),
            blocks: vec![SemBlock {
                id: BlockId(0),
                args: Vec::new(),
                ops: vec![op(
                    0,
                    definition(0, ResolvedTy::I64),
                    SemOpKind::ConstI64(42),
                )],
                terminator: SemTerminator::Return {
                    value: Some(ValueId(0)),
                },
            }],
        };
        let raw = template("constant", Vec::new(), ResolvedTy::I64);
        let checked = CheckedMirFunction {
            name: "constant".to_string(),
            return_ty: ResolvedTy::I64,
            blocks: Vec::new(),
            decisions: Vec::new(),
            checks: Vec::new(),
            cooperate_sites: Vec::new(),
        };
        let mut pipeline = IrPipeline {
            raw_mir: vec![raw],
            checked_mir: vec![checked],
            elaborated_mir: vec![crate::ElaboratedMirFunction {
                name: "constant".to_string(),
                return_ty: ResolvedTy::I64,
                statements: Vec::new(),
                decisions: Vec::new(),
                blocks: Vec::new(),
                drop_plans: Vec::new(),
                coroutine: None,
                lambda_captures: Vec::new(),
            }],
            ..IrPipeline::default()
        };

        let report = apply_sir_to_pipeline(
            &SemModule {
                functions: vec![function],
            },
            &mut pipeline,
        );
        assert_eq!(report.lowered_count(), 1, "{report:#?}");
        assert!(pipeline.elaborated_mir.is_empty());
        assert!(matches!(
            pipeline.raw_mir[0].blocks[0].instructions.as_slice(),
            [
                Instr::ConstI64 { value: 42, .. },
                Instr::Move {
                    dest: Place::ReturnSlot,
                    ..
                }
            ]
        ));
    }

    #[test]
    fn module_verification_prevents_duplicate_functions_from_mutating_mir() {
        let function = SemFunction {
            id: ItemId(0),
            declaration: DefId::for_test("duplicate"),
            name: "duplicate".to_string(),
            span: 0..0,
            source_origin: FunctionSourceOrigin::Unknown,
            params: Vec::new(),
            return_ty: ResolvedTy::I64,
            entry: BlockId(0),
            blocks: vec![SemBlock {
                id: BlockId(0),
                args: Vec::new(),
                ops: vec![op(
                    0,
                    definition(0, ResolvedTy::I64),
                    SemOpKind::ConstI64(42),
                )],
                terminator: SemTerminator::Return {
                    value: Some(ValueId(0)),
                },
            }],
        };
        let mut duplicate = function.clone();
        duplicate.id = ItemId(1);
        let raw = template("duplicate", Vec::new(), ResolvedTy::I64);
        let mut pipeline = IrPipeline {
            raw_mir: vec![raw.clone()],
            ..IrPipeline::default()
        };

        let report = apply_sir_to_pipeline(
            &SemModule {
                functions: vec![function, duplicate],
            },
            &mut pipeline,
        );

        assert_eq!(report.lowered_count(), 0, "{report:#?}");
        assert_eq!(pipeline.raw_mir, vec![raw]);
        assert!(report.statuses.iter().all(|(_, status)| matches!(
            status,
            SirMirLoweringStatus::Unsupported { reason }
                if reason.contains("SIR module verifier rejected")
        )));
    }
}
