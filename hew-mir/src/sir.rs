//! SIR → raw-MIR lowering for the initial value/CFG execution slice.
//!
//! This module is intentionally the *lowering boundary*, rather than a
//! dependency of `hew-sir`: SIR owns semantic values and block arguments;
//! raw MIR owns addressable storage, trap CFG, and the backend ABI.  The first
//! executable subset is deliberately narrow and rejects a function as a unit
//! when it would need ownership, aggregate layout, call ABI, or suspension
//! facts that SIR does not yet carry.

use std::collections::{BTreeMap, BTreeSet};

use hew_hir::{IntentKind, SiteId, ValueClass};
use hew_parser::ast::{BinaryOp, UnaryOp};
#[cfg(test)]
use hew_sir::CallableInstance;
use hew_sir::{
    BlockArg, BlockId, CallableId, Edge, FunctionSourceOrigin, Operand, SemBlock, SemCallConv,
    SemCallable, SemCallableKind, SemFunction, SemModule, SemOp, SemOpKind, SemParamPassing,
    SemTerminator, UseMode, ValueDef, ValueId,
};
#[cfg(test)]
use hew_types::DefId;
use hew_types::ResolvedTy;

use crate::{
    dataflow, raw_uses_virtual_values, raw_virtual_class, verify_raw_virtual_value_checked,
    verify_raw_virtual_value_elaborated, verify_raw_virtual_value_function, BasicBlock, BlockKind,
    CheckedMirFunction, DropPlan, ElabBlock, ElaboratedMirFunction, ExitPath, FunctionCallConv,
    Instr, IntArithOp, IntSignedness, IrPipeline, ModuleCapabilities, ParamBoundaryFact,
    ParamBoundaryMode, Place, RawMirFunction, RawValueDef, RawValueId, RawValueOp, Strategy,
    Terminator, TrapKind, ValueMaterializationReason,
};

/// The result of lowering one SIR function through the complete existing MIR
/// ladder.
///
/// The first executable SIR slice admits only scalar, non-owning values, so
/// its elaborated body has zero drop plans. It is nevertheless explicit: SIR
/// must never rely on codegen's legacy "missing elaboration means no drops"
/// compatibility behavior. In particular, a semantic `Unreachable` block is
/// represented by a normal zero-drop elaborated block with no `ExitPath`.
#[derive(Debug, Clone, PartialEq)]
struct SirMirLowered {
    raw: RawMirFunction,
    checked: CheckedMirFunction,
    elaborated: ElaboratedMirFunction,
}

/// A closed, self-contained scalar SIR call-graph realization.
///
/// This component takes no raw-MIR template input. SIR owns each callable's
/// symbol, signature, and parameter ABI facts; this lowering independently
/// creates both raw and checked MIR.
/// A driver either installs the entire selected call graph or reports its
/// unsupported boundary — it never silently mixes a SIR caller with a legacy
/// body inside the selected closure.
#[derive(Debug, Clone, PartialEq)]
pub struct SirMirComponent {
    /// Callables included in deterministic SIR table order.
    callables: Vec<CallableId>,
    raw_mir: Vec<RawMirFunction>,
    checked_mir: Vec<CheckedMirFunction>,
    elaborated_mir: Vec<ElaboratedMirFunction>,
}

impl SirMirComponent {
    /// The deterministic closed callable set realized by this component.
    #[must_use]
    pub fn callables(&self) -> &[CallableId] {
        &self.callables
    }

    /// Build the body portion of a fresh MIR pipeline from SIR-owned facts.
    ///
    /// This deliberately starts with `IrPipeline::default()` rather than a
    /// legacy HIR→MIR pipeline. The admitted scalar direct-call domain has no
    /// declaration layouts, runtime authorities, or ownership facts; later
    /// SIR domains will receive a bodyless declaration-header input instead.
    #[must_use]
    pub fn into_pipeline(self) -> IrPipeline {
        let mut pipeline = IrPipeline {
            raw_mir: self.raw_mir,
            checked_mir: self.checked_mir,
            elaborated_mir: self.elaborated_mir,
            ..IrPipeline::default()
        };
        pipeline.capabilities =
            ModuleCapabilities::from_raw_mir(&pipeline.raw_mir, &pipeline.extern_decls);
        pipeline.lint_warnings = crate::liveness::run_mir_lints(&pipeline.raw_mir);
        pipeline.debug_assert_capabilities_current();
        pipeline
    }
}

/// A conservative refusal to lower a SIR function through the first
/// executable SIR boundary.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SirMirLoweringError {
    pub reason: String,
    /// Callable whose missing SIR body caused a strict closed-component
    /// refusal, when the failure is specifically a HIR→SIR surface gap.
    /// Driver diagnostics use this resolved identity to report the originating
    /// SIR lowering reason without parsing display strings.
    pub missing_body: Option<CallableId>,
}

impl SirMirLoweringError {
    fn unsupported(reason: impl Into<String>) -> Self {
        Self {
            reason: reason.into(),
            missing_body: None,
        }
    }

    fn missing_body(callable: CallableId, reason: impl Into<String>) -> Self {
        Self {
            reason: reason.into(),
            missing_body: Some(callable),
        }
    }
}

impl std::fmt::Display for SirMirLoweringError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(&self.reason)
    }
}

impl std::error::Error for SirMirLoweringError {}

/// Lower the closed scalar component reachable from the module's resolved
/// entry callable.
///
/// This is the only program-selection entry point strict SIR lowering has.
/// The entry is
/// the identity HIR published and SIR joined on; a module that carries no
/// entry fact is not an executable program and is refused here with a typed
/// error rather than being rescued by a name lookup.
///
/// # Errors
///
/// Returns a deterministic refusal when the module carries no entry callable,
/// or when [`lower_closed_scalar_component`] refuses the entry's component.
pub fn lower_entry_component(module: &SemModule) -> Result<SirMirComponent, SirMirLoweringError> {
    let entry = module.entry_callable.ok_or_else(|| {
        SirMirLoweringError::unsupported(
            "strict SIR lowering requires a resolved entry callable; this module carries no HIR entry declaration",
        )
    })?;
    lower_closed_scalar_component(module, &[entry])
}

/// Lower a closed scalar direct-call component without consulting legacy MIR.
///
/// `roots` are resolved SIR callable identities, normally the HIR-established
/// root-unit entry callable. Every direct call reachable from a root must have
/// exactly one SIR body in the admitted scalar domain. This is intentionally
/// all-or-nothing: the strict SIR lane must not hide a legacy body behind a
/// SIR call edge.
///
/// # Errors
///
/// Returns a deterministic refusal when module verification fails or a
/// reachable callable has no SIR body / falls outside the initial direct-call
/// domain.
pub fn lower_closed_scalar_component(
    module: &SemModule,
    roots: &[CallableId],
) -> Result<SirMirComponent, SirMirLoweringError> {
    if let Some(diagnostic) = hew_sir::verify_module(module).into_iter().next() {
        return Err(SirMirLoweringError::unsupported(format!(
            "SIR module verifier rejected the direct-call component: {:?}",
            diagnostic.kind
        )));
    }
    if roots.is_empty() {
        return Err(SirMirLoweringError::unsupported(
            "strict SIR lowering requires at least one resolved callable root",
        ));
    }

    // One association for the whole closure: the walk below asks for a body
    // once per callable it reaches, and the realization loop asks again.
    let bodies = module.function_index();
    let mut selected = BTreeSet::new();
    let mut pending = roots
        .iter()
        .copied()
        .map(|root| (root, vec![root]))
        .collect::<Vec<_>>();

    while let Some((callable_id, path)) = pending.pop() {
        if !selected.insert(callable_id) {
            continue;
        }
        let callable = module.callable(callable_id).ok_or_else(|| {
            SirMirLoweringError::unsupported(format!(
                "strict SIR direct-call closure reaches unknown callable {} via {}",
                callable_id.0,
                format_callable_path(module, &path)
            ))
        })?;
        validate_direct_callable(callable)?;
        let function = bodies.function(callable_id).ok_or_else(|| {
            SirMirLoweringError::missing_body(
                callable_id,
                format!(
                    "strict SIR direct-call closure requires one lowered body for `{}` via {}",
                    callable.symbol,
                    format_callable_path(module, &path)
                ),
            )
        })?;
        for callee in direct_callees(function) {
            let mut next_path = path.clone();
            next_path.push(callee);
            pending.push((callee, next_path));
        }
    }

    let mut raw_mir = Vec::with_capacity(selected.len());
    let mut checked_mir = Vec::with_capacity(selected.len());
    let mut elaborated_mir = Vec::with_capacity(selected.len());
    for callable_id in &selected {
        let function = bodies.function(*callable_id).ok_or_else(|| {
            SirMirLoweringError::unsupported(format!(
                "selected SIR callable {} lost its body during component realization",
                callable_id.0
            ))
        })?;
        let lowered = lower_verified_sir_function(module, function)?;
        raw_mir.push(lowered.raw);
        checked_mir.push(lowered.checked);
        elaborated_mir.push(lowered.elaborated);
    }

    Ok(SirMirComponent {
        callables: selected.into_iter().collect(),
        raw_mir,
        checked_mir,
        elaborated_mir,
    })
}

/// Test helper for lowering one verifier-approved SIR function.
///
/// Production callers must use [`lower_closed_scalar_component`] so every
/// direct call resolves within a closed realized component.
#[cfg(test)]
fn lower_sir_function(
    module: &SemModule,
    function: &SemFunction,
) -> Result<SirMirLowered, SirMirLoweringError> {
    if let Some(diagnostic) = hew_sir::verify_module(module).into_iter().next() {
        return Err(SirMirLoweringError::unsupported(format!(
            "SIR module verifier rejected function `{}`: {:?}",
            function.name, diagnostic.kind
        )));
    }
    lower_verified_sir_function(module, function)
}

fn lower_verified_sir_function(
    module: &SemModule,
    function: &SemFunction,
) -> Result<SirMirLowered, SirMirLoweringError> {
    let callable = module.callable(function.callable).ok_or_else(|| {
        SirMirLoweringError::unsupported(format!(
            "SIR function `{}` has no resolved callable {}",
            function.name, function.callable.0
        ))
    })?;
    validate_direct_callable(callable)?;
    validate_sir_function_signature(function, callable)?;
    if function.entry != BlockId(0) {
        return Err(SirMirLoweringError::unsupported(
            "the initial raw-MIR realization requires SIR entry block bb0",
        ));
    }

    let (locals, blocks) = if function_uses_virtual_tuple_values(function) {
        VirtualRawLowerer::new(function)?.finish()
    } else {
        let collected = CollectedValues::from_function(function)?;
        let mut lowerer = RawLowerer::new(function, collected, module)?;
        for block in &function.blocks {
            lowerer.lower_block(block)?;
        }
        lowerer.finish()?
    };
    let parameter_decisions = sir_parameter_decisions(callable)?;
    let raw = RawMirFunction {
        name: callable.symbol.clone(),
        return_ty: callable.signature.return_ty.clone(),
        call_conv: FunctionCallConv::Default,
        params: callable
            .signature
            .params
            .iter()
            .map(|parameter| parameter.ty.clone())
            .collect(),
        locals,
        // SIR values do not yet carry source binding/scope debug identities.
        // An empty debug projection is truthful and preferable to copying
        // storage metadata authored for a legacy HIR→MIR body.
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
        source_origin: raw_source_origin(&callable.source_origin),
    };
    let mut checked = CheckedMirFunction {
        name: raw.name.clone(),
        return_ty: raw.return_ty.clone(),
        blocks: raw.blocks.clone(),
        decisions: parameter_decisions,
        checks: crate::validate_context_markers(&raw),
        // Strict SIR may introduce edge-forwarding blocks after the CFG nodes
        // they target. Its scheduler therefore uses structural latches rather
        // than the legacy raw-MIR numeric block-order convention.
        cooperate_sites: dataflow::compute_structural_cooperate_sites(&raw.blocks),
        ownership_elaboration: None,
    };
    verify_strict_sir_raw_checked(module, callable, &raw, &checked)?;
    let elaborated = zero_drop_elaboration(&raw, &checked)?;
    checked.ownership_elaboration = Some(Box::new(elaborated.clone()));
    if raw_uses_virtual_values(&raw) {
        verify_strict_sir_virtual_elaboration(&raw, &checked, &elaborated)?;
    }
    Ok(SirMirLowered {
        raw,
        checked,
        elaborated,
    })
}

/// Verify the small, deliberately storage-free SIR → raw/checked-MIR contract.
///
/// This is intentionally narrower than the legacy raw-MIR validators.  The
/// strict SIR lane currently admits only scalar values, direct calls, and the
/// CFG forms authored by [`RawLowerer`].  Checking that exact contract here
/// means a future SIR lowering edit cannot accidentally smuggle an unmodelled
/// ownership, place, or ABI convention through a raw-MIR body merely because
/// a later backend happens to accept it.
fn verify_strict_sir_raw_checked(
    module: &SemModule,
    callable: &SemCallable,
    raw: &RawMirFunction,
    checked: &CheckedMirFunction,
) -> Result<(), SirMirLoweringError> {
    if raw.name != callable.symbol
        || raw.return_ty != callable.signature.return_ty
        || raw.call_conv != FunctionCallConv::Default
    {
        return Err(SirMirLoweringError::unsupported(format!(
            "strict SIR raw/checked verifier: raw function does not match callable `{}` ABI",
            callable.symbol
        )));
    }
    if checked.name != raw.name || checked.return_ty != raw.return_ty {
        return Err(SirMirLoweringError::unsupported(format!(
            "strict SIR raw/checked verifier: checked function identity does not match raw `{}`",
            raw.name
        )));
    }

    let expected_params = callable
        .signature
        .params
        .iter()
        .map(|parameter| parameter.ty.clone())
        .collect::<Vec<_>>();
    if raw.params != expected_params {
        return Err(SirMirLoweringError::unsupported(format!(
            "strict SIR raw/checked verifier: raw parameters do not match callable `{}` ABI",
            callable.symbol
        )));
    }
    if raw_uses_virtual_values(raw) {
        return verify_strict_sir_virtual_raw_checked(callable, raw, checked);
    }
    if raw.locals.len() < raw.params.len() {
        return Err(SirMirLoweringError::unsupported(format!(
            "strict SIR raw/checked verifier: raw `{}` has fewer locals than ABI parameters",
            raw.name
        )));
    }
    for (index, parameter_ty) in raw.params.iter().enumerate() {
        if raw.locals[index] != *parameter_ty {
            return Err(SirMirLoweringError::unsupported(format!(
                "strict SIR raw/checked verifier: raw `{}` parameter local {index} does not match its ABI type",
                raw.name
            )));
        }
    }
    for (index, local_ty) in raw.locals.iter().enumerate() {
        if !is_supported_value_type(local_ty) {
            return Err(SirMirLoweringError::unsupported(format!(
                "strict SIR raw/checked verifier: raw `{}` local {index} has unsupported type `{}`",
                raw.name,
                local_ty.user_facing()
            )));
        }
    }

    verify_strict_sir_block_layout("raw", &raw.blocks)?;
    verify_strict_sir_block_layout("checked", &checked.blocks)?;
    if raw.blocks != checked.blocks {
        return Err(SirMirLoweringError::unsupported(format!(
            "strict SIR raw/checked verifier: raw and checked blocks diverge for `{}`",
            raw.name
        )));
    }
    if raw.decisions != checked.decisions {
        return Err(SirMirLoweringError::unsupported(format!(
            "strict SIR raw/checked verifier: raw and checked decisions diverge for `{}`",
            raw.name
        )));
    }
    if checked.checks != crate::validate_context_markers(raw) {
        return Err(SirMirLoweringError::unsupported(format!(
            "strict SIR raw/checked verifier: checked context findings are stale for `{}`",
            raw.name
        )));
    }
    if checked.cooperate_sites != dataflow::compute_structural_cooperate_sites(&raw.blocks) {
        return Err(SirMirLoweringError::unsupported(format!(
            "strict SIR raw/checked verifier: checked scheduler facts are stale for `{}`",
            raw.name
        )));
    }
    verify_strict_sir_parameter_boundary_facts(callable, raw)?;

    for block in &raw.blocks {
        if !block.statements.is_empty() {
            return Err(SirMirLoweringError::unsupported(format!(
                "strict SIR raw/checked verifier: raw bb{} carries legacy MIR statements",
                block.id
            )));
        }
        for (instruction_index, instruction) in block.instructions.iter().enumerate() {
            verify_strict_sir_instruction(raw, block.id, instruction_index, instruction)?;
        }
        verify_strict_sir_terminator(module, raw, block)?;
    }
    Ok(())
}

/// Verify the one-block virtual-value Raw → Checked boundary.
///
/// Raw-MIR owns the semantic virtual-value contract, including its `RawValueId`
/// type map and no-storage ABI. SIR retains only its callable-specific
/// parameter-boundary facts here; LLVM consumes the same Raw verifier facts.
fn verify_strict_sir_virtual_raw_checked(
    callable: &SemCallable,
    raw: &RawMirFunction,
    checked: &CheckedMirFunction,
) -> Result<(), SirMirLoweringError> {
    let facts = verify_raw_virtual_value_function(raw)
        .map_err(strict_sir_virtual_value_error)?
        .ok_or_else(|| {
            SirMirLoweringError::unsupported(format!(
                "strict SIR virtual verifier: raw `{}` has no virtual values",
                raw.name
            ))
        })?;
    verify_raw_virtual_value_checked(raw, checked, &facts)
        .map_err(strict_sir_virtual_value_error)?;
    verify_strict_sir_parameter_boundary_facts(callable, raw)
}

fn strict_sir_virtual_value_error(error: crate::RawVirtualValueError) -> SirMirLoweringError {
    let crate::RawVirtualValueError { reason } = error;
    SirMirLoweringError::unsupported(format!("strict SIR virtual verifier: {reason}"))
}

fn verify_strict_sir_block_layout(
    lane: &str,
    blocks: &[BasicBlock],
) -> Result<(), SirMirLoweringError> {
    if blocks.is_empty() {
        return Err(SirMirLoweringError::unsupported(format!(
            "strict SIR raw/checked verifier: {lane} MIR has no basic blocks"
        )));
    }
    for (index, block) in blocks.iter().enumerate() {
        let expected_id = u32::try_from(index).map_err(|_| {
            SirMirLoweringError::unsupported(format!(
                "strict SIR raw/checked verifier: {lane} MIR block count exceeds u32"
            ))
        })?;
        if block.id != expected_id {
            return Err(SirMirLoweringError::unsupported(format!(
                "strict SIR raw/checked verifier: {lane} MIR has noncanonical block id bb{} at index {index}",
                block.id
            )));
        }
        for target in block.successors() {
            let target_index = usize::try_from(target).map_err(|_| {
                SirMirLoweringError::unsupported(format!(
                    "strict SIR raw/checked verifier: {lane} bb{} successor bb{target} cannot index the CFG",
                    block.id
                ))
            })?;
            if target_index >= blocks.len() {
                return Err(SirMirLoweringError::unsupported(format!(
                    "strict SIR raw/checked verifier: {lane} bb{} targets missing bb{target}",
                    block.id
                )));
            }
        }
    }
    Ok(())
}

fn verify_strict_sir_parameter_boundary_facts(
    callable: &SemCallable,
    raw: &RawMirFunction,
) -> Result<(), SirMirLoweringError> {
    if raw.decisions.len() != callable.signature.params.len() {
        return Err(SirMirLoweringError::unsupported(format!(
            "strict SIR raw/checked verifier: raw `{}` has {} parameter-boundary facts for {} ABI parameters",
            raw.name,
            raw.decisions.len(),
            callable.signature.params.len()
        )));
    }
    let parameter_count = u32::try_from(callable.signature.params.len()).map_err(|_| {
        SirMirLoweringError::unsupported(
            "strict SIR raw/checked verifier: callable parameter count exceeds u32",
        )
    })?;
    for (index, (decision, parameter)) in raw
        .decisions
        .iter()
        .zip(&callable.signature.params)
        .enumerate()
    {
        let parameter_index = u32::try_from(index).map_err(|_| {
            SirMirLoweringError::unsupported(
                "strict SIR raw/checked verifier: callable parameter index exceeds u32",
            )
        })?;
        let Strategy::ParamBoundary(fact) = &decision.strategy else {
            return Err(SirMirLoweringError::unsupported(format!(
                "strict SIR raw/checked verifier: raw `{}` decision {index} is not a parameter-boundary fact",
                raw.name
            )));
        };
        let expected = ParamBoundaryFact {
            param_index: parameter_index,
            param_count: parameter_count,
            caller_visible_projection: parameter.caller_visible_projection,
            mode: match parameter.passing {
                SemParamPassing::ReadOnly => ParamBoundaryMode::BorrowReadOnly,
            },
        };
        if decision.site != SiteId(parameter_index)
            || decision.ty != parameter.ty
            || decision.value_class != ValueClass::BitCopy
            || decision.intent != IntentKind::Unknown
            || *fact != expected
        {
            return Err(SirMirLoweringError::unsupported(format!(
                "strict SIR raw/checked verifier: raw `{}` parameter-boundary fact {index} does not match callable ABI",
                raw.name
            )));
        }
    }
    Ok(())
}

#[allow(
    clippy::too_many_lines,
    reason = "one exhaustive scalar-instruction allowlist makes the strict SIR-to-MIR boundary auditable in one place"
)]
fn verify_strict_sir_instruction(
    raw: &RawMirFunction,
    block_id: u32,
    instruction_index: usize,
    instruction: &Instr,
) -> Result<(), SirMirLoweringError> {
    let context = format!("raw bb{block_id} instruction {instruction_index}");
    match instruction {
        Instr::ConstI64 { dest, .. } => {
            let destination_ty = strict_sir_local_ty(raw, *dest, &context)?;
            if !destination_ty.is_integer() && *destination_ty != ResolvedTy::Bool {
                return Err(SirMirLoweringError::unsupported(format!(
                    "strict SIR raw/checked verifier: {context} writes a constant to non-scalar `{}`",
                    destination_ty.user_facing()
                )));
            }
        }
        Instr::BoolNot { dest, operand } => {
            verify_strict_sir_same_type(raw, *dest, *operand, &context, &ResolvedTy::Bool)?;
        }
        Instr::IntNegChecked {
            signed,
            dest,
            operand,
            overflow_flag,
        } => {
            let destination_ty =
                verify_strict_sir_same_integer_type(raw, *dest, *operand, &context)?;
            if signedness(destination_ty)? != *signed
                || strict_sir_local_ty(raw, *overflow_flag, &context)? != &ResolvedTy::Bool
            {
                return Err(SirMirLoweringError::unsupported(format!(
                    "strict SIR raw/checked verifier: {context} has an invalid checked-negation shape"
                )));
            }
        }
        Instr::IntBitNot { dest, operand } => {
            let _ = verify_strict_sir_same_integer_type(raw, *dest, *operand, &context)?;
        }
        Instr::IntAdd { dest, lhs, rhs }
        | Instr::IntSub { dest, lhs, rhs }
        | Instr::IntMul { dest, lhs, rhs }
        | Instr::IntBitAnd { dest, lhs, rhs }
        | Instr::IntBitOr { dest, lhs, rhs }
        | Instr::IntBitXor { dest, lhs, rhs } => {
            let destination_ty = verify_strict_sir_same_integer_type(raw, *dest, *lhs, &context)?;
            if strict_sir_local_ty(raw, *rhs, &context)? != destination_ty {
                return Err(SirMirLoweringError::unsupported(format!(
                    "strict SIR raw/checked verifier: {context} mixes scalar arithmetic local types"
                )));
            }
        }
        Instr::IntArithChecked {
            signed,
            dest,
            lhs,
            rhs,
            overflow_flag,
            ..
        } => {
            let destination_ty = verify_strict_sir_same_integer_type(raw, *dest, *lhs, &context)?;
            if strict_sir_local_ty(raw, *rhs, &context)? != destination_ty
                || signedness(destination_ty)? != *signed
                || strict_sir_local_ty(raw, *overflow_flag, &context)? != &ResolvedTy::Bool
            {
                return Err(SirMirLoweringError::unsupported(format!(
                    "strict SIR raw/checked verifier: {context} has an invalid checked-arithmetic shape"
                )));
            }
        }
        Instr::IntCmp {
            dest,
            pred,
            lhs,
            rhs,
        } => {
            if strict_sir_local_ty(raw, *dest, &context)? != &ResolvedTy::Bool {
                return Err(SirMirLoweringError::unsupported(format!(
                    "strict SIR raw/checked verifier: {context} comparison result is not bool"
                )));
            }
            let lhs_ty = strict_sir_local_ty(raw, *lhs, &context)?;
            let is_bool_equality = lhs_ty == &ResolvedTy::Bool
                && matches!(pred, crate::CmpPred::Eq | crate::CmpPred::NotEq);
            if (!lhs_ty.is_integer() && !is_bool_equality)
                || strict_sir_local_ty(raw, *rhs, &context)? != lhs_ty
            {
                return Err(SirMirLoweringError::unsupported(format!(
                    "strict SIR raw/checked verifier: {context} comparison operands are not same-typed integers or boolean equality operands"
                )));
            }
        }
        Instr::NumericCast {
            dest,
            src,
            from_ty,
            to_ty,
        } => {
            if strict_sir_local_ty(raw, *src, &context)? != from_ty
                || strict_sir_local_ty(raw, *dest, &context)? != to_ty
                || !from_ty.can_explicitly_numeric_cast_to(to_ty)
            {
                return Err(SirMirLoweringError::unsupported(format!(
                    "strict SIR raw/checked verifier: {context} has an invalid numeric-cast shape"
                )));
            }
        }
        Instr::Move { dest, src } => verify_strict_sir_move(raw, *dest, *src, &context)?,
        _ => {
            return Err(SirMirLoweringError::unsupported(format!(
                "strict SIR raw/checked verifier: {context} uses an instruction outside the scalar SIR subset"
            )));
        }
    }
    Ok(())
}

fn verify_strict_sir_terminator(
    module: &SemModule,
    raw: &RawMirFunction,
    block: &BasicBlock,
) -> Result<(), SirMirLoweringError> {
    match &block.terminator {
        Terminator::Return => {
            let return_stores = block
                .instructions
                .iter()
                .filter(|instruction| {
                    matches!(
                        instruction,
                        Instr::Move {
                            dest: Place::ReturnSlot,
                            ..
                        }
                    )
                })
                .count();
            if raw.return_ty == ResolvedTy::Unit {
                if return_stores != 0 {
                    return Err(SirMirLoweringError::unsupported(format!(
                        "strict SIR raw/checked verifier: raw bb{} writes a value for a unit return",
                        block.id
                    )));
                }
            } else if return_stores != 1
                || !matches!(
                    block.instructions.last(),
                    Some(Instr::Move {
                        dest: Place::ReturnSlot,
                        ..
                    })
                )
            {
                return Err(SirMirLoweringError::unsupported(format!(
                    "strict SIR raw/checked verifier: raw bb{} must finish its scalar return with one return-slot move",
                    block.id
                )));
            }
        }
        Terminator::Goto { .. }
        | Terminator::Unreachable
        | Terminator::Trap {
            kind: TrapKind::IntegerOverflow,
        } => {}
        Terminator::Branch { cond, .. } => {
            if strict_sir_local_ty(raw, *cond, &format!("raw bb{} branch", block.id))?
                != &ResolvedTy::Bool
            {
                return Err(SirMirLoweringError::unsupported(format!(
                    "strict SIR raw/checked verifier: raw bb{} branches on a non-bool local",
                    block.id
                )));
            }
        }
        Terminator::Call {
            callee,
            authority,
            args,
            dest,
            ..
        } => verify_strict_sir_call(module, raw, block.id, callee, *authority, args, *dest)?,
        _ => {
            return Err(SirMirLoweringError::unsupported(format!(
                "strict SIR raw/checked verifier: raw bb{} uses a terminator outside the scalar SIR subset",
                block.id
            )));
        }
    }
    Ok(())
}

fn verify_strict_sir_call(
    module: &SemModule,
    raw: &RawMirFunction,
    block_id: u32,
    callee_symbol: &str,
    authority: crate::CallAuthority,
    args: &[Place],
    dest: Option<Place>,
) -> Result<(), SirMirLoweringError> {
    if authority != crate::CallAuthority::Direct {
        return Err(SirMirLoweringError::unsupported(format!(
            "strict SIR raw/checked verifier: raw bb{block_id} direct call `{callee_symbol}` has non-direct authority"
        )));
    }
    let mut matching = module
        .callables
        .iter()
        .filter(|callable| callable.symbol == callee_symbol);
    let callee = matching.next().ok_or_else(|| {
        SirMirLoweringError::unsupported(format!(
            "strict SIR raw/checked verifier: raw bb{block_id} calls unknown SIR callable `{callee_symbol}`"
        ))
    })?;
    if matching.next().is_some() {
        return Err(SirMirLoweringError::unsupported(format!(
            "strict SIR raw/checked verifier: raw bb{block_id} call `{callee_symbol}` has ambiguous SIR callable identity"
        )));
    }
    validate_direct_callable(callee)?;
    if args.len() != callee.signature.params.len() {
        return Err(SirMirLoweringError::unsupported(format!(
            "strict SIR raw/checked verifier: raw bb{block_id} call `{callee_symbol}` has {} arguments for {} ABI parameters",
            args.len(),
            callee.signature.params.len()
        )));
    }
    for (index, (argument, parameter)) in args.iter().zip(&callee.signature.params).enumerate() {
        if strict_sir_local_ty(
            raw,
            *argument,
            &format!("raw bb{block_id} call argument {index}"),
        )? != &parameter.ty
        {
            return Err(SirMirLoweringError::unsupported(format!(
                "strict SIR raw/checked verifier: raw bb{block_id} call `{callee_symbol}` argument {index} does not match its ABI type"
            )));
        }
    }
    match (&callee.signature.return_ty, dest) {
        (ResolvedTy::Unit, None) => {}
        (ResolvedTy::Unit, Some(_)) => {
            return Err(SirMirLoweringError::unsupported(format!(
                "strict SIR raw/checked verifier: raw bb{block_id} unit call `{callee_symbol}` has a destination"
            )));
        }
        (_, None) => {
            return Err(SirMirLoweringError::unsupported(format!(
                "strict SIR raw/checked verifier: raw bb{block_id} value call `{callee_symbol}` lacks a destination"
            )));
        }
        (return_ty, Some(destination)) => {
            if strict_sir_local_ty(
                raw,
                destination,
                &format!("raw bb{block_id} call destination"),
            )? != return_ty
            {
                return Err(SirMirLoweringError::unsupported(format!(
                    "strict SIR raw/checked verifier: raw bb{block_id} call `{callee_symbol}` destination does not match its ABI return"
                )));
            }
        }
    }
    Ok(())
}

fn verify_strict_sir_move(
    raw: &RawMirFunction,
    dest: Place,
    src: Place,
    context: &str,
) -> Result<(), SirMirLoweringError> {
    let source_ty = strict_sir_local_ty(raw, src, context)?;
    match dest {
        Place::Local(_) => {
            if strict_sir_local_ty(raw, dest, context)? != source_ty {
                return Err(SirMirLoweringError::unsupported(format!(
                    "strict SIR raw/checked verifier: {context} moves between different local types"
                )));
            }
        }
        Place::ReturnSlot => {
            if raw.return_ty == ResolvedTy::Unit || &raw.return_ty != source_ty {
                return Err(SirMirLoweringError::unsupported(format!(
                    "strict SIR raw/checked verifier: {context} writes an incompatible value to the return slot"
                )));
            }
        }
        _ => {
            return Err(SirMirLoweringError::unsupported(format!(
                "strict SIR raw/checked verifier: {context} moves into a non-scalar place"
            )));
        }
    }
    Ok(())
}

fn verify_strict_sir_same_type(
    raw: &RawMirFunction,
    dest: Place,
    operand: Place,
    context: &str,
    expected: &ResolvedTy,
) -> Result<(), SirMirLoweringError> {
    if strict_sir_local_ty(raw, dest, context)? != expected
        || strict_sir_local_ty(raw, operand, context)? != expected
    {
        return Err(SirMirLoweringError::unsupported(format!(
            "strict SIR raw/checked verifier: {context} does not use `{}` locals",
            expected.user_facing()
        )));
    }
    Ok(())
}

fn verify_strict_sir_same_integer_type<'a>(
    raw: &'a RawMirFunction,
    dest: Place,
    operand: Place,
    context: &str,
) -> Result<&'a ResolvedTy, SirMirLoweringError> {
    let destination_ty = strict_sir_local_ty(raw, dest, context)?;
    if !destination_ty.is_integer() || strict_sir_local_ty(raw, operand, context)? != destination_ty
    {
        return Err(SirMirLoweringError::unsupported(format!(
            "strict SIR raw/checked verifier: {context} does not use same-typed integer locals"
        )));
    }
    Ok(destination_ty)
}

fn strict_sir_local_ty<'a>(
    raw: &'a RawMirFunction,
    place: Place,
    context: &str,
) -> Result<&'a ResolvedTy, SirMirLoweringError> {
    let Place::Local(local) = place else {
        return Err(SirMirLoweringError::unsupported(format!(
            "strict SIR raw/checked verifier: {context} uses non-local place {place:?}"
        )));
    };
    let local_index = usize::try_from(local).map_err(|_| {
        SirMirLoweringError::unsupported(format!(
            "strict SIR raw/checked verifier: {context} local {local} cannot index raw locals"
        ))
    })?;
    let local_ty = raw.locals.get(local_index).ok_or_else(|| {
        SirMirLoweringError::unsupported(format!(
            "strict SIR raw/checked verifier: {context} references out-of-bounds local {local}"
        ))
    })?;
    if !is_supported_value_type(local_ty) {
        return Err(SirMirLoweringError::unsupported(format!(
            "strict SIR raw/checked verifier: {context} references non-scalar local {local}"
        )));
    }
    Ok(local_ty)
}

fn validate_direct_callable(callable: &SemCallable) -> Result<(), SirMirLoweringError> {
    if callable.call_conv != SemCallConv::Default || callable.kind != SemCallableKind::HewDirect {
        return Err(SirMirLoweringError::unsupported(format!(
            "SIR callable `{}` is outside the initial default HewDirect ABI domain",
            callable.symbol
        )));
    }
    for (index, parameter) in callable.signature.params.iter().enumerate() {
        if parameter.passing != SemParamPassing::ReadOnly
            || parameter.caller_visible_projection
            || !is_supported_value_type(&parameter.ty)
        {
            return Err(SirMirLoweringError::unsupported(format!(
                "SIR callable `{}` parameter {index} needs ownership or aggregate ABI lowering",
                callable.symbol
            )));
        }
    }
    if !is_supported_return_type(&callable.signature.return_ty) {
        return Err(SirMirLoweringError::unsupported(format!(
            "SIR callable `{}` return type `{}` needs later ABI lowering",
            callable.symbol,
            callable.signature.return_ty.user_facing()
        )));
    }
    Ok(())
}

fn validate_sir_function_signature(
    function: &SemFunction,
    callable: &SemCallable,
) -> Result<(), SirMirLoweringError> {
    let function_params = function
        .params
        .iter()
        .map(|parameter| &parameter.ty)
        .collect::<Vec<_>>();
    let callable_params = callable
        .signature
        .params
        .iter()
        .map(|parameter| &parameter.ty)
        .collect::<Vec<_>>();
    if function.callable != callable.id
        || function.id != callable.function
        || function.declaration != callable.declaration
        || function.name != callable.symbol
        || function.source_origin != callable.source_origin
        || function.return_ty != callable.signature.return_ty
        || function_params != callable_params
    {
        return Err(SirMirLoweringError::unsupported(format!(
            "SIR function `{}` does not match its resolved callable authority",
            function.name
        )));
    }
    Ok(())
}

fn sir_parameter_decisions(
    callable: &SemCallable,
) -> Result<Vec<crate::DecisionFact>, SirMirLoweringError> {
    let parameter_count = u32::try_from(callable.signature.params.len()).map_err(|_| {
        SirMirLoweringError::unsupported("SIR parameter count exceeds raw-MIR ABI limits")
    })?;
    callable
        .signature
        .params
        .iter()
        .enumerate()
        .map(|(index, parameter)| {
            let index = u32::try_from(index).map_err(|_| {
                SirMirLoweringError::unsupported("SIR parameter index exceeds raw-MIR ABI limits")
            })?;
            Ok(crate::DecisionFact {
                // ABI boundary facts have no source expression site. The
                // stable parameter ordinal is sufficient for the raw/checked
                // decision stream and deliberately does not borrow a legacy
                // HIR-to-MIR site identity.
                site: SiteId(index),
                ty: parameter.ty.clone(),
                value_class: ValueClass::BitCopy,
                intent: IntentKind::Unknown,
                strategy: Strategy::ParamBoundary(ParamBoundaryFact {
                    param_index: index,
                    param_count: parameter_count,
                    caller_visible_projection: false,
                    mode: ParamBoundaryMode::BorrowReadOnly,
                }),
                why: "SIR scalar direct-call parameter boundary".to_string(),
            })
        })
        .collect()
}

fn direct_callees(function: &SemFunction) -> impl Iterator<Item = CallableId> + '_ {
    function.blocks.iter().flat_map(|block| {
        block
            .ops
            .iter()
            .filter_map(|operation| match &operation.kind {
                SemOpKind::Call { callee, .. } => Some(*callee),
                _ => None,
            })
    })
}

fn format_callable_path(module: &SemModule, path: &[CallableId]) -> String {
    path.iter()
        .map(|id| {
            module.callable(*id).map_or_else(
                || format!("callable#{}", id.0),
                |callable| callable.symbol.clone(),
            )
        })
        .collect::<Vec<_>>()
        .join(" → ")
}

/// Build the explicit elaborated artifact for the initial SIR value-only
/// subset.
///
/// This is intentionally not a second drop-elaboration algorithm. The subset
/// rejects ownership-bearing values before Raw MIR, so it has no possible drop
/// obligations. It nevertheless retains every Raw block as a normal `ElabBlock`
/// and records an empty `DropPlan` for every runtime-reachable exit. Only a
/// semantic `Unreachable` carries no `ExitPath` plan; that makes the Raw →
/// Checked → Elaborated ladder total for SIR bodies without treating an
/// impossible path as a trap or cleanup edge.
fn zero_drop_elaboration(
    raw: &RawMirFunction,
    checked: &CheckedMirFunction,
) -> Result<ElaboratedMirFunction, SirMirLoweringError> {
    debug_assert_eq!(raw.name, checked.name);
    debug_assert_eq!(raw.return_ty, checked.return_ty);
    debug_assert_eq!(raw.blocks, checked.blocks);
    let mut blocks = raw
        .blocks
        .iter()
        .map(|block| ElabBlock {
            id: block.id,
            kind: BlockKind::Normal,
            drops: Vec::new(),
            successor: None,
        })
        .collect::<Vec<_>>();
    let mut next_cleanup_id = raw
        .blocks
        .iter()
        .map(|block| block.id)
        .max()
        .map_or(0, |id| id.saturating_add(1));
    // Codegen injects a cancellation branch at every cooperation site. That
    // branch is an executable exit just as much as a terminator edge, so it
    // must retain an explicit (empty in this subset) plan. Canonicalize by
    // block id because distinct site kinds could otherwise name the same
    // injected cancellation edge.
    let cancellation_blocks = checked
        .cooperate_sites
        .iter()
        .map(|site| site.bb_id)
        .collect::<BTreeSet<_>>();
    for block_id in &cancellation_blocks {
        let Some(block) = raw.blocks.iter().find(|block| block.id == *block_id) else {
            return Err(SirMirLoweringError::unsupported(format!(
                "strict SIR zero-drop elaboration found a cooperate site for missing raw block bb{block_id}"
            )));
        };
        // Codegen injects the cooperation/cancellation branch before the
        // block's terminator. A semantic Unreachable must remain a bare,
        // plan-free endpoint, so a stale scheduler fact naming it is invalid
        // rather than something elaboration can silently skip.
        if matches!(block.terminator, Terminator::Unreachable) {
            return Err(SirMirLoweringError::unsupported(format!(
                "strict SIR zero-drop elaboration found a cooperate site for semantic unreachable bb{block_id}"
            )));
        }
    }
    let mut drop_plans = Vec::new();
    for block in &raw.blocks {
        let exit = match &block.terminator {
            // A semantic unreachable is not a language-visible exit. It has no
            // cleanup edge or drop plan even in the explicit Elaborated body.
            Terminator::Unreachable => None,
            Terminator::Return => Some(ExitPath::Return { block: block.id }),
            Terminator::Goto { target } => Some(ExitPath::Goto {
                block: block.id,
                target: *target,
            }),
            Terminator::Branch {
                then_target,
                else_target,
                ..
            } => Some(ExitPath::Branch {
                block: block.id,
                then_target: *then_target,
                else_target: *else_target,
            }),
            Terminator::Call { callee, next, .. } => Some(ExitPath::Call {
                block: block.id,
                callee: callee.clone(),
                next: *next,
            }),
            Terminator::Trap { .. } => {
                // A trap is an executable panic path, unlike semantic
                // `Unreachable`. Even the no-drop SIR subset must preserve its
                // exit identity and cleanup shape for codegen and future
                // ownership elaboration.
                blocks.push(ElabBlock {
                    id: next_cleanup_id,
                    kind: BlockKind::Cleanup,
                    drops: Vec::new(),
                    successor: None,
                });
                next_cleanup_id = next_cleanup_id.saturating_add(1);
                Some(ExitPath::Panic { block: block.id })
            }
            other => {
                return Err(SirMirLoweringError::unsupported(format!(
                    "strict SIR zero-drop elaboration encountered unsupported raw terminator in bb{}: {other:?}",
                    block.id
                )));
            }
        };
        if let Some(exit) = exit {
            drop_plans.push((exit, DropPlan::default()));
        }
        if cancellation_blocks.contains(&block.id) {
            drop_plans.push((ExitPath::Cancel { block: block.id }, DropPlan::default()));
        }
    }
    Ok(ElaboratedMirFunction {
        name: raw.name.clone(),
        return_ty: raw.return_ty.clone(),
        statements: Vec::new(),
        decisions: checked.decisions.clone(),
        blocks,
        drop_plans,
        coroutine: None,
        lambda_captures: Vec::new(),
    })
}

/// Verify the explicit Elaborated-MIR artifact for a virtual-value Raw body.
///
/// Raw-MIR owns the zero-drop mirror invariant so direct Raw -> LLVM lowering
/// cannot accept a different elaboration shape than SIR lowering does.
fn verify_strict_sir_virtual_elaboration(
    raw: &RawMirFunction,
    checked: &CheckedMirFunction,
    elaborated: &ElaboratedMirFunction,
) -> Result<(), SirMirLoweringError> {
    let facts = verify_raw_virtual_value_function(raw)
        .map_err(strict_sir_virtual_value_error)?
        .ok_or_else(|| {
            SirMirLoweringError::unsupported(format!(
                "strict SIR virtual elaboration: raw `{}` has no virtual values",
                raw.name
            ))
        })?;
    verify_raw_virtual_value_elaborated(raw, checked, elaborated, &facts)
        .map_err(strict_sir_virtual_value_error)
}

fn raw_source_origin(origin: &FunctionSourceOrigin) -> crate::SourceOrigin {
    match origin {
        FunctionSourceOrigin::RootUnit => crate::SourceOrigin::RootUnit,
        FunctionSourceOrigin::Foreign(module) => crate::SourceOrigin::Foreign(module.clone()),
        FunctionSourceOrigin::Unknown => crate::SourceOrigin::Unknown,
    }
}

fn is_supported_return_type(ty: &ResolvedTy) -> bool {
    matches!(ty, ResolvedTy::Unit) || is_supported_value_type(ty)
}

fn is_supported_value_type(ty: &ResolvedTy) -> bool {
    ty.is_integer() || matches!(ty, ResolvedTy::Bool)
}

/// Whether a SIR function needs the narrowly admitted raw-MIR virtual-value
/// realization instead of the established local/Place lowerer.
///
/// The presence of a semantic tuple operation is intentional evidence that
/// lowering through `Place::Local` would destroy the value-only boundary this
/// slice is proving. Do not silently fall back to the legacy tuple lowering:
/// an unsupported shape must remain an explicit SIR implementation gap.
fn function_uses_virtual_tuple_values(function: &SemFunction) -> bool {
    function.blocks.iter().any(|block| {
        block.ops.iter().any(|operation| {
            matches!(
                &operation.kind,
                SemOpKind::TupleMake { .. } | SemOpKind::TupleGet { .. }
            )
        })
    })
}

/// Typed SIR values used by the one-block raw virtual-value realization.
///
/// This deliberately does not reuse [`CollectedValues`]: the established
/// `RawLowerer` remains scalar/Place based, while this collector admits the
/// separately verified recursive `BitCopy` tuple family.
fn collect_virtual_value_types(
    function: &SemFunction,
) -> Result<BTreeMap<ValueId, ResolvedTy>, SirMirLoweringError> {
    let mut types = BTreeMap::new();
    for parameter in &function.params {
        insert_value(&mut types, parameter.value, &parameter.ty)?;
    }
    for block in &function.blocks {
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
        if raw_virtual_class(ty).is_none() {
            return Err(SirMirLoweringError::unsupported(format!(
                "virtual raw value %{} of type `{}` needs ownership or representation lowering",
                value.0,
                ty.user_facing()
            )));
        }
    }
    Ok(types)
}

/// Lower SIR's first aggregate family directly to Raw MIR virtual values.
///
/// This is intentionally not a second general IR. It is a short, strict Raw
/// MIR construction path whose only addressable operation is the final ABI
/// store to `ReturnSlot`. Control-flow value transport, calls, ownership, and
/// aggregate ABI lowering remain rejected until their dedicated Raw-MIR work.
struct VirtualRawLowerer<'a> {
    function: &'a SemFunction,
    value_types: BTreeMap<ValueId, ResolvedTy>,
    instructions: Vec<Instr>,
}

impl<'a> VirtualRawLowerer<'a> {
    fn new(function: &'a SemFunction) -> Result<Self, SirMirLoweringError> {
        if function.entry != BlockId(0) {
            return Err(SirMirLoweringError::unsupported(
                "the raw virtual-value slice requires SIR entry block bb0",
            ));
        }
        let [block] = function.blocks.as_slice() else {
            return Err(SirMirLoweringError::unsupported(
                "the raw virtual-value slice admits exactly one SIR basic block",
            ));
        };
        if block.id != BlockId(0) || !block.args.is_empty() {
            return Err(SirMirLoweringError::unsupported(
                "the raw virtual-value slice requires an argument-free entry bb0",
            ));
        }
        let value_types = collect_virtual_value_types(function)?;
        let mut lowerer = Self {
            function,
            value_types,
            instructions: Vec::new(),
        };
        lowerer.lower_params()?;
        for operation in &block.ops {
            lowerer.lower_op(operation)?;
        }
        lowerer.lower_terminator(&block.terminator)?;
        Ok(lowerer)
    }

    fn lower_params(&mut self) -> Result<(), SirMirLoweringError> {
        for (index, parameter) in self.function.params.iter().enumerate() {
            if !is_supported_value_type(&parameter.ty) {
                return Err(SirMirLoweringError::unsupported(format!(
                    "raw virtual-value parameter {index} has non-scalar type `{}`; tuple ABI lowering is deferred",
                    parameter.ty.user_facing()
                )));
            }
            let index = u32::try_from(index).map_err(|_| {
                SirMirLoweringError::unsupported(
                    "raw virtual-value parameter count exceeds the u32 ABI range",
                )
            })?;
            self.instructions.push(Instr::Value(RawValueOp::Param {
                dest: self.value_def(parameter.value)?,
                index,
            }));
        }
        Ok(())
    }

    #[expect(
        clippy::too_many_lines,
        reason = "the bounded virtual-value operation vocabulary stays in one match so every admitted SIR operation is visible at the Raw boundary"
    )]
    fn lower_op(&mut self, operation: &SemOp) -> Result<(), SirMirLoweringError> {
        let (result, result_ty) = RawLowerer::single_result(operation)?;
        let dest = self.value_def(result)?;
        let operation = match &operation.kind {
            SemOpKind::ConstI64(value) => {
                if !result_ty.is_integer() {
                    return Err(SirMirLoweringError::unsupported(format!(
                        "raw virtual integer constant result %{} has non-integer type `{}`",
                        result.0,
                        result_ty.user_facing()
                    )));
                }
                RawValueOp::ConstI64 {
                    dest,
                    value: *value,
                }
            }
            SemOpKind::ConstBool(value) => {
                if result_ty != ResolvedTy::Bool {
                    return Err(SirMirLoweringError::unsupported(format!(
                        "raw virtual boolean constant result %{} has type `{}` rather than bool",
                        result.0,
                        result_ty.user_facing()
                    )));
                }
                RawValueOp::ConstBool {
                    dest,
                    value: *value,
                }
            }
            SemOpKind::TupleMake { elements } => {
                let ResolvedTy::Tuple(element_tys) = &result_ty else {
                    return Err(SirMirLoweringError::unsupported(format!(
                        "SIR tuple.make result %{} has non-tuple type `{}`",
                        result.0,
                        result_ty.user_facing()
                    )));
                };
                if elements.len() != element_tys.len() {
                    return Err(SirMirLoweringError::unsupported(
                        "SIR tuple.make operand count does not match its semantic tuple type",
                    ));
                }
                let mut fields = Vec::with_capacity(elements.len());
                for (index, (element, expected_ty)) in elements.iter().zip(element_tys).enumerate()
                {
                    Self::require_read(element, "SIR tuple.make element")?;
                    let actual_ty = self.value_type(element.value)?;
                    if actual_ty != expected_ty {
                        return Err(SirMirLoweringError::unsupported(format!(
                            "SIR tuple.make element {index} type `{}` does not match semantic tuple element `{}`",
                            actual_ty.user_facing(),
                            expected_ty.user_facing()
                        )));
                    }
                    fields.push(Self::raw_value_id(element.value));
                }
                RawValueOp::TupleMake { dest, fields }
            }
            SemOpKind::TupleGet { tuple, index } => {
                Self::require_read(tuple, "SIR tuple.get operand")?;
                let tuple_ty = self.value_type(tuple.value)?;
                let ResolvedTy::Tuple(element_tys) = tuple_ty else {
                    return Err(SirMirLoweringError::unsupported(format!(
                        "SIR tuple.get operand %{} has non-tuple type `{}`",
                        tuple.value.0,
                        tuple_ty.user_facing()
                    )));
                };
                let index_usize = usize::try_from(*index).map_err(|_| {
                    SirMirLoweringError::unsupported(
                        "SIR tuple.get index cannot index the semantic tuple",
                    )
                })?;
                let expected_ty = element_tys.get(index_usize).ok_or_else(|| {
                    SirMirLoweringError::unsupported(format!(
                        "SIR tuple.get index {index} is outside `{}`",
                        tuple_ty.user_facing()
                    ))
                })?;
                if &result_ty != expected_ty {
                    return Err(SirMirLoweringError::unsupported(format!(
                        "SIR tuple.get result type `{}` does not match selected element `{}`",
                        result_ty.user_facing(),
                        expected_ty.user_facing()
                    )));
                }
                RawValueOp::TupleGet {
                    dest,
                    tuple: Self::raw_value_id(tuple.value),
                    index: *index,
                }
            }
            SemOpKind::Unary { .. }
            | SemOpKind::Binary { .. }
            | SemOpKind::Cast { .. }
            | SemOpKind::Call { .. } => {
                return Err(SirMirLoweringError::unsupported(
                    "the raw virtual-value slice admits only constants and semantic tuple make/get operations",
                ));
            }
        };
        self.instructions.push(Instr::Value(operation));
        Ok(())
    }

    fn lower_terminator(&mut self, terminator: &SemTerminator) -> Result<(), SirMirLoweringError> {
        match terminator {
            SemTerminator::Return { value: Some(value) } => {
                Self::require_read(value, "SIR virtual return value")?;
                if self.value_type(value.value)? != &self.function.return_ty {
                    return Err(SirMirLoweringError::unsupported(
                        "SIR virtual return value type does not match the function return type",
                    ));
                }
                if self.function.return_ty == ResolvedTy::Unit {
                    return Err(SirMirLoweringError::unsupported(
                        "unit SIR function must not materialize a virtual return value",
                    ));
                }
                self.instructions.push(Instr::MaterializeValue {
                    dest: Place::ReturnSlot,
                    value: Self::raw_value_id(value.value),
                    reason: ValueMaterializationReason::ReturnAbi,
                });
                Ok(())
            }
            SemTerminator::Return { value: None }
                if self.function.return_ty == ResolvedTy::Unit =>
            {
                Ok(())
            }
            SemTerminator::Return { value: None } => Err(SirMirLoweringError::unsupported(
                "non-unit SIR function has a value-less virtual return",
            )),
            SemTerminator::Goto(_) | SemTerminator::Branch { .. } | SemTerminator::Unreachable => {
                Err(SirMirLoweringError::unsupported(
                    "the raw virtual-value slice admits only an ordinary Return terminator",
                ))
            }
        }
    }

    fn raw_value_id(value: ValueId) -> RawValueId {
        RawValueId(value.0)
    }

    fn value_def(&self, value: ValueId) -> Result<RawValueDef, SirMirLoweringError> {
        Ok(RawValueDef {
            id: Self::raw_value_id(value),
            ty: self.value_type(value)?.clone(),
        })
    }

    fn value_type(&self, value: ValueId) -> Result<&ResolvedTy, SirMirLoweringError> {
        self.value_types.get(&value).ok_or_else(|| {
            SirMirLoweringError::unsupported(format!(
                "SIR virtual raw lowering uses undefined value %{}",
                value.0
            ))
        })
    }

    fn require_read(operand: &Operand, context: &str) -> Result<(), SirMirLoweringError> {
        RawLowerer::require_read(operand, context)
    }

    fn finish(self) -> (Vec<ResolvedTy>, Vec<BasicBlock>) {
        (
            Vec::new(),
            vec![BasicBlock {
                id: 0,
                statements: Vec::new(),
                instructions: self.instructions,
                terminator: Terminator::Return,
            }],
        )
    }
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
    module: &'a SemModule,
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
        module: &'a SemModule,
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
            module,
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
        if let SemOpKind::Call { callee, args } = &operation.kind {
            return self.lower_call(*callee, args, &operation.results);
        }
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
            SemOpKind::TupleMake { .. } | SemOpKind::TupleGet { .. } => {
                return Err(SirMirLoweringError::unsupported(
                    "SIR tuple values require the raw virtual-value lowering path",
                ));
            }
            SemOpKind::Call { .. } => unreachable!("calls return before value-result lowering"),
        }
        Ok(())
    }

    /// Legalize a semantic direct call into raw MIR's CFG-splitting call
    /// terminator. The callable signature determines whether the operation
    /// has one destination value or no destination at all. The current SIR
    /// block continues at the newly-created raw continuation, so subsequent
    /// operations and its source terminator stay in program order without
    /// manufacturing a second SIR CFG form.
    fn lower_call(
        &mut self,
        callee: CallableId,
        args: &[Operand],
        results: &[ValueDef],
    ) -> Result<(), SirMirLoweringError> {
        let module = self.module;
        let callable = module.callable(callee).ok_or_else(|| {
            SirMirLoweringError::unsupported(format!(
                "SIR call targets unknown callable {}",
                callee.0
            ))
        })?;
        validate_direct_callable(callable)?;
        let destination = match (callable.signature.return_ty == ResolvedTy::Unit, results) {
            (true, []) => None,
            (true, _) => {
                return Err(SirMirLoweringError::unsupported(format!(
                    "unit-returning SIR call to `{}` must have zero results",
                    callable.symbol
                )));
            }
            (false, [ValueDef { id, ty }]) if ty == &callable.signature.return_ty => {
                Some(self.value_place(*id)?)
            }
            (false, [ValueDef { ty, .. }]) => {
                return Err(SirMirLoweringError::unsupported(format!(
                    "SIR call result `{}` does not match callable `{}` return `{}`",
                    ty.user_facing(),
                    callable.symbol,
                    callable.signature.return_ty.user_facing()
                )));
            }
            (false, _) => {
                return Err(SirMirLoweringError::unsupported(format!(
                    "non-unit SIR call to `{}` requires exactly one result",
                    callable.symbol
                )));
            }
        };
        if args.len() != callable.signature.params.len() {
            return Err(SirMirLoweringError::unsupported(format!(
                "SIR call to `{}` has {} argument(s), expected {}",
                callable.symbol,
                args.len(),
                callable.signature.params.len()
            )));
        }
        let mut raw_args = Vec::with_capacity(args.len());
        for (index, (argument, parameter)) in
            args.iter().zip(&callable.signature.params).enumerate()
        {
            Self::require_read(argument, "direct call")?;
            let actual = self.value_type(argument.value)?;
            if actual != &parameter.ty || parameter.passing != SemParamPassing::ReadOnly {
                return Err(SirMirLoweringError::unsupported(format!(
                    "SIR call argument {index} does not satisfy `{}` scalar ReadOnly ABI",
                    callable.symbol
                )));
            }
            raw_args.push(self.value_place(argument.value)?);
        }

        let continuation = self.fresh_block()?;
        self.terminate(Terminator::Call {
            callee: callable.symbol.clone(),
            authority: crate::CallAuthority::Direct,
            args: raw_args,
            dest: destination,
            next: continuation,
        })?;
        self.current = continuation;
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
                Self::require_read(value, "SIR return value")?;
                if self.value_type(value.value)? != &self.function.return_ty {
                    return Err(SirMirLoweringError::unsupported(
                        "SIR return value type does not match function return type",
                    ));
                }
                self.push(Instr::Move {
                    dest: Place::ReturnSlot,
                    src: self.value_place(value.value)?,
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
                Self::require_read(condition, "SIR branch condition")?;
                if self.value_type(condition.value)? != &ResolvedTy::Bool {
                    return Err(SirMirLoweringError::unsupported(
                        "SIR branch condition must have bool type",
                    ));
                }
                let source = self.current;
                let then_target = self.materialize_edge(then_target)?;
                let else_target = self.materialize_edge(else_target)?;
                self.current = source;
                self.terminate(Terminator::Branch {
                    cond: self.value_place(condition.value)?,
                    then_target,
                    else_target,
                })
            }
            SemTerminator::Unreachable => self.terminate(Terminator::Unreachable),
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
            Self::require_read(source, "SIR edge argument")?;
            let source_ty = self.value_type(source.value)?;
            if source_ty != &target.ty {
                return Err(SirMirLoweringError::unsupported(
                    "SIR edge argument type does not match target block argument",
                ));
            }
            let scratch = self.fresh_local(source_ty.clone())?;
            self.push(Instr::Move {
                dest: scratch,
                src: self.value_place(source.value)?,
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
    use hew_hir::ItemId;
    use hew_sir::{OpId, Provenance, SemAbiParam, SemSignature};

    fn test_callable(function: &SemFunction) -> SemCallable {
        SemCallable {
            id: function.callable,
            function: function.id,
            declaration: function.declaration.clone(),
            instance: CallableInstance::Monomorphic,
            symbol: function.name.clone(),
            source_origin: function.source_origin.clone(),
            signature: SemSignature {
                params: function
                    .params
                    .iter()
                    .map(|parameter| SemAbiParam {
                        ty: parameter.ty.clone(),
                        passing: SemParamPassing::ReadOnly,
                        caller_visible_projection: false,
                    })
                    .collect(),
                return_ty: function.return_ty.clone(),
            },
            call_conv: SemCallConv::Default,
            kind: SemCallableKind::HewDirect,
            effect_summary: hew_sir::EffectSummary::Unknown,
        }
    }

    fn test_module(functions: Vec<SemFunction>) -> SemModule {
        let mut callables = functions.iter().map(test_callable).collect::<Vec<_>>();
        callables.sort_unstable_by_key(|callable| callable.id);
        let root_unit_callables = callables
            .iter()
            .filter(|callable| callable.source_origin == FunctionSourceOrigin::RootUnit)
            .map(|callable| callable.id)
            .collect::<Vec<_>>();
        let entry_callable = callables
            .iter()
            .find(|callable| {
                callable.source_origin == FunctionSourceOrigin::RootUnit
                    && callable.symbol == "main"
            })
            .map(|callable| callable.id);
        SemModule {
            callables,
            generic_templates: Vec::new(),
            root_unit_callables,
            entry_callable,
            functions,
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

    fn zero_result_op(id: u32, kind: SemOpKind) -> SemOp {
        SemOp {
            id: OpId(id),
            results: Vec::new(),
            kind,
            provenance: Provenance::Synthesized,
        }
    }

    fn strict_i64_identity_function() -> SemFunction {
        SemFunction {
            id: ItemId(0),
            callable: CallableId(0),
            declaration: DefId::for_test("identity"),
            name: "identity".to_string(),
            span: 0..0,
            source_origin: FunctionSourceOrigin::RootUnit,
            params: vec![BlockArg {
                value: ValueId(0),
                ty: ResolvedTy::I64,
            }],
            return_ty: ResolvedTy::I64,
            entry: BlockId(0),
            blocks: vec![SemBlock {
                id: BlockId(0),
                args: Vec::new(),
                ops: Vec::new(),
                terminator: SemTerminator::Return {
                    value: Some(operand(0)),
                },
            }],
        }
    }

    fn strict_unreachable_function() -> SemFunction {
        SemFunction {
            id: ItemId(0),
            callable: CallableId(0),
            declaration: DefId::for_test("semantic_unreachable"),
            name: "semantic_unreachable".to_string(),
            span: 0..0,
            source_origin: FunctionSourceOrigin::RootUnit,
            params: Vec::new(),
            return_ty: ResolvedTy::Unit,
            entry: BlockId(0),
            blocks: vec![SemBlock {
                id: BlockId(0),
                args: Vec::new(),
                ops: Vec::new(),
                terminator: SemTerminator::Unreachable,
            }],
        }
    }

    fn strict_boolean_equality_function() -> SemFunction {
        SemFunction {
            id: ItemId(0),
            callable: CallableId(0),
            declaration: DefId::for_test("same"),
            name: "same".to_string(),
            span: 0..0,
            source_origin: FunctionSourceOrigin::RootUnit,
            params: vec![
                BlockArg {
                    value: ValueId(0),
                    ty: ResolvedTy::Bool,
                },
                BlockArg {
                    value: ValueId(1),
                    ty: ResolvedTy::Bool,
                },
            ],
            return_ty: ResolvedTy::Bool,
            entry: BlockId(0),
            blocks: vec![SemBlock {
                id: BlockId(0),
                args: Vec::new(),
                ops: vec![op(
                    0,
                    definition(2, ResolvedTy::Bool),
                    SemOpKind::Binary {
                        op: BinaryOp::Equal,
                        lhs: operand(0),
                        rhs: operand(1),
                    },
                )],
                terminator: SemTerminator::Return {
                    value: Some(operand(2)),
                },
            }],
        }
    }

    /// The bounded virtual-value proof body: values are constructed and
    /// projected as a semantic tuple, but only the scalar result crosses the
    /// existing ABI return slot. This is deliberately parameter-free so it
    /// exercises no aggregate ABI policy.
    fn strict_virtual_tuple_projection_function() -> SemFunction {
        let pair_ty = ResolvedTy::Tuple(vec![ResolvedTy::I64, ResolvedTy::I64]);
        SemFunction {
            id: ItemId(0),
            callable: CallableId(0),
            declaration: DefId::for_test("main"),
            name: "main".to_string(),
            span: 0..0,
            source_origin: FunctionSourceOrigin::RootUnit,
            params: Vec::new(),
            return_ty: ResolvedTy::I64,
            entry: BlockId(0),
            blocks: vec![SemBlock {
                id: BlockId(0),
                args: Vec::new(),
                ops: vec![
                    op(0, definition(0, ResolvedTy::I64), SemOpKind::ConstI64(0)),
                    op(1, definition(1, ResolvedTy::I64), SemOpKind::ConstI64(42)),
                    op(
                        2,
                        definition(2, pair_ty),
                        SemOpKind::TupleMake {
                            elements: vec![operand(0), operand(1)],
                        },
                    ),
                    op(
                        3,
                        definition(3, ResolvedTy::I64),
                        SemOpKind::TupleGet {
                            tuple: operand(2),
                            index: 0,
                        },
                    ),
                ],
                terminator: SemTerminator::Return {
                    value: Some(operand(3)),
                },
            }],
        }
    }

    /// This only exists as a Raw/LLVM ABI regression fixture. Tuple
    /// parameters and results intentionally remain outside the first slice;
    /// individual scalar parameters can still be bound directly as virtual
    /// values and feed an internal tuple.
    fn strict_virtual_scalar_param_tuple_projection_function() -> SemFunction {
        let pair_ty = ResolvedTy::Tuple(vec![ResolvedTy::I64, ResolvedTy::I64]);
        SemFunction {
            id: ItemId(0),
            callable: CallableId(0),
            declaration: DefId::for_test("pair_second"),
            name: "pair_second".to_string(),
            span: 0..0,
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
            blocks: vec![SemBlock {
                id: BlockId(0),
                args: Vec::new(),
                ops: vec![
                    op(
                        0,
                        definition(2, pair_ty),
                        SemOpKind::TupleMake {
                            elements: vec![operand(0), operand(1)],
                        },
                    ),
                    op(
                        1,
                        definition(3, ResolvedTy::I64),
                        SemOpKind::TupleGet {
                            tuple: operand(2),
                            index: 1,
                        },
                    ),
                ],
                terminator: SemTerminator::Return {
                    value: Some(operand(3)),
                },
            }],
        }
    }

    #[test]
    fn strict_post_lowering_verifier_accepts_boolean_equality_and_rejects_bad_local() {
        let function = strict_boolean_equality_function();
        let module = test_module(vec![function.clone()]);
        let mut lowered = lower_sir_function(&module, &function)
            .expect("boolean equality is in the strict scalar SIR subset");
        let callable = module
            .callable(function.callable)
            .expect("test callable must exist");
        verify_strict_sir_raw_checked(&module, callable, &lowered.raw, &lowered.checked)
            .expect("the strict lowerer must produce a self-consistent raw/checked pair");
        assert!(matches!(
            lowered.raw.blocks[0].instructions[0],
            Instr::IntCmp {
                pred: crate::CmpPred::Eq,
                ..
            }
        ));

        match &mut lowered.raw.blocks[0].instructions[0] {
            Instr::IntCmp { dest, .. } => *dest = Place::Local(99),
            instruction => {
                panic!("expected boolean equality to lower as IntCmp, got {instruction:?}")
            }
        }
        lowered.checked.blocks = lowered.raw.blocks.clone();
        let error =
            verify_strict_sir_raw_checked(&module, callable, &lowered.raw, &lowered.checked)
                .expect_err("an out-of-bounds scalar local must fail at the SIR raw boundary");
        assert!(error.reason.contains("out-of-bounds local 99"));
    }

    #[test]
    fn realizes_semantic_tuple_values_through_the_no_drop_mir_ladder() {
        let function = strict_virtual_tuple_projection_function();
        let module = test_module(vec![function.clone()]);
        let lowered = lower_sir_function(&module, &function)
            .expect("the internal tuple projection must use virtual Raw MIR values");
        let callable = module
            .callable(function.callable)
            .expect("test callable must exist");

        assert!(lowered.raw.locals.is_empty());
        assert_eq!(lowered.raw.blocks.len(), 1);
        assert_eq!(lowered.checked.blocks, lowered.raw.blocks);
        assert_eq!(
            lowered.raw.blocks[0].instructions,
            vec![
                Instr::Value(RawValueOp::ConstI64 {
                    dest: RawValueDef {
                        id: RawValueId(0),
                        ty: ResolvedTy::I64,
                    },
                    value: 0,
                }),
                Instr::Value(RawValueOp::ConstI64 {
                    dest: RawValueDef {
                        id: RawValueId(1),
                        ty: ResolvedTy::I64,
                    },
                    value: 42,
                }),
                Instr::Value(RawValueOp::TupleMake {
                    dest: RawValueDef {
                        id: RawValueId(2),
                        ty: ResolvedTy::Tuple(vec![ResolvedTy::I64, ResolvedTy::I64]),
                    },
                    fields: vec![RawValueId(0), RawValueId(1)],
                }),
                Instr::Value(RawValueOp::TupleGet {
                    dest: RawValueDef {
                        id: RawValueId(3),
                        ty: ResolvedTy::I64,
                    },
                    tuple: RawValueId(2),
                    index: 0,
                }),
                Instr::MaterializeValue {
                    dest: Place::ReturnSlot,
                    value: RawValueId(3),
                    reason: ValueMaterializationReason::ReturnAbi,
                },
            ]
        );
        verify_strict_sir_raw_checked(&module, callable, &lowered.raw, &lowered.checked)
            .expect("the virtual Raw and Checked bodies must agree");
        verify_strict_sir_virtual_elaboration(&lowered.raw, &lowered.checked, &lowered.elaborated)
            .expect("the virtual body must elaborate to one explicit empty return plan");
    }

    #[test]
    fn virtual_raw_verifier_requires_exact_scalar_param_binding_and_return_boundary() {
        let function = strict_virtual_scalar_param_tuple_projection_function();
        let module = test_module(vec![function.clone()]);
        let mut lowered = lower_sir_function(&module, &function)
            .expect("scalar params may feed an internal virtual tuple");
        let callable = module
            .callable(function.callable)
            .expect("test callable must exist");

        assert!(lowered.raw.locals.is_empty());
        assert!(matches!(
            lowered.raw.blocks[0].instructions.as_slice(),
            [
                Instr::Value(RawValueOp::Param { index: 0, .. }),
                Instr::Value(RawValueOp::Param { index: 1, .. }),
                ..,
                Instr::MaterializeValue {
                    dest: Place::ReturnSlot,
                    reason: ValueMaterializationReason::ReturnAbi,
                    ..
                },
            ]
        ));

        let Instr::Value(RawValueOp::Param { index, .. }) =
            &mut lowered.raw.blocks[0].instructions[1]
        else {
            panic!("the second instruction must bind ABI parameter 1");
        };
        *index = 0;
        lowered.checked.blocks = lowered.raw.blocks.clone();
        let error =
            verify_strict_sir_raw_checked(&module, callable, &lowered.raw, &lowered.checked)
                .expect_err("one ABI parameter may map to exactly one virtual definition");
        assert!(error.reason.contains("expected ordered parameter 1"));

        let mut lowered = lower_sir_function(&module, &function)
            .expect("fresh virtual lowering must remain valid");
        let Instr::MaterializeValue { dest, .. } = lowered.raw.blocks[0]
            .instructions
            .last_mut()
            .expect("virtual return materialization")
        else {
            panic!("the final virtual instruction must materialize the ABI return");
        };
        *dest = Place::Local(0);
        lowered.checked.blocks = lowered.raw.blocks.clone();
        let error =
            verify_strict_sir_raw_checked(&module, callable, &lowered.raw, &lowered.checked)
                .expect_err("only ReturnSlot may materialize a virtual value");
        assert!(error.reason.contains("ReturnAbi -> ReturnSlot"));
    }

    #[test]
    fn strict_post_lowering_verifier_rejects_bad_cfg_target() {
        let function = strict_i64_identity_function();
        let module = test_module(vec![function.clone()]);
        let mut lowered =
            lower_sir_function(&module, &function).expect("the identity function should lower");
        let callable = module
            .callable(function.callable)
            .expect("test callable must exist");
        lowered.raw.blocks[0].terminator = Terminator::Goto { target: 1 };
        lowered.checked.blocks = lowered.raw.blocks.clone();

        let error =
            verify_strict_sir_raw_checked(&module, callable, &lowered.raw, &lowered.checked)
                .expect_err("a raw target outside the canonical CFG must fail at the SIR boundary");
        assert!(error.reason.contains("targets missing bb1"));
    }

    #[test]
    fn realizes_semantic_unreachable_through_the_explicit_zero_drop_ladder() {
        let function = strict_unreachable_function();
        let pipeline =
            lower_closed_scalar_component(&test_module(vec![function]), &[CallableId(0)])
                .expect("a semantic unreachable is a legal strict SIR endpoint")
                .into_pipeline();

        let raw = pipeline
            .raw_mir
            .first()
            .expect("the component must contain one raw-MIR body");
        assert!(matches!(raw.blocks[0].terminator, Terminator::Unreachable));
        assert!(raw.blocks[0].successors().is_empty());

        let checked = pipeline
            .checked_mir
            .first()
            .expect("the component must contain one checked-MIR body");
        assert_eq!(checked.blocks, raw.blocks);
        assert!(checked.checks.is_empty());
        assert!(checked.cooperate_sites.is_empty());

        let elaborated = pipeline
            .elaborated_mir
            .first()
            .expect("SIR bodies must carry an explicit elaborated artifact");
        assert_eq!(elaborated.name, raw.name);
        assert!(elaborated.drop_plans.is_empty());
        assert_eq!(elaborated.blocks.len(), 1);
        let block = &elaborated.blocks[0];
        assert_eq!(block.id, 0);
        assert_eq!(block.kind, BlockKind::Normal);
        assert!(block.drops.is_empty());
        assert_eq!(block.successor, None);
    }

    /// The zero-drop elaboration is total for strict SIR bodies, but a
    /// scheduler fact naming a semantic `Unreachable` block is stale rather
    /// than something elaboration may quietly skip: codegen injects the
    /// cancellation branch before the terminator, which would leave a
    /// plan-free endpoint carrying an executable exit.
    #[test]
    fn zero_drop_elaboration_rejects_a_cooperate_site_on_semantic_unreachable() {
        let function = strict_unreachable_function();
        let module = test_module(vec![function.clone()]);
        let lowered = lower_sir_function(&module, &function)
            .expect("a semantic unreachable is a legal strict SIR endpoint");
        assert!(matches!(
            lowered.raw.blocks[0].terminator,
            Terminator::Unreachable
        ));

        let mut stale = lowered.checked.clone();
        stale.cooperate_sites = vec![crate::CooperateSite {
            bb_id: lowered.raw.blocks[0].id,
            kind: crate::CooperateKind::FunctionEntry,
        }];
        let error = zero_drop_elaboration(&lowered.raw, &stale)
            .expect_err("a cooperate site on a semantic unreachable block must fail closed");
        assert!(
            error
                .reason
                .contains("cooperate site for semantic unreachable bb0"),
            "{}",
            error.reason
        );

        // Negative control: without the stale scheduler fact the same body
        // elaborates, so the refusal above is the site rule and not a
        // body-shape rejection.
        zero_drop_elaboration(&lowered.raw, &lowered.checked)
            .expect("the unmodified strict body must still elaborate");
    }

    #[test]
    fn strict_post_lowering_verifier_rejects_parameter_boundary_fact_drift() {
        let function = strict_i64_identity_function();
        let module = test_module(vec![function.clone()]);
        let mut lowered =
            lower_sir_function(&module, &function).expect("the identity function should lower");
        let callable = module
            .callable(function.callable)
            .expect("test callable must exist");
        lowered.raw.decisions[0].strategy = Strategy::ParamBoundary(ParamBoundaryFact {
            param_index: 0,
            param_count: 1,
            caller_visible_projection: false,
            mode: ParamBoundaryMode::TransferResource,
        });
        lowered.checked.decisions = lowered.raw.decisions.clone();

        let error =
            verify_strict_sir_raw_checked(&module, callable, &lowered.raw, &lowered.checked)
                .expect_err("a stale ownership boundary fact must fail before later MIR stages");
        assert!(error.reason.contains("parameter-boundary fact 0"));
    }

    #[test]
    #[allow(
        clippy::too_many_lines,
        reason = "the complete diamond pins SIR block arguments, checked arithmetic legalization, and raw CFG construction together"
    )]
    fn realizes_ssa_diamond_into_raw_cfg_and_overflow_paths() {
        let function = SemFunction {
            id: ItemId(0),
            callable: CallableId(0),
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
                        condition: operand(3),
                        then_target: Edge {
                            target: BlockId(1),
                            args: vec![operand(1)],
                        },
                        else_target: Edge {
                            target: BlockId(2),
                            args: vec![operand(1)],
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
                        args: vec![operand(6)],
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
                        args: vec![operand(9)],
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
                        value: Some(operand(12)),
                    },
                },
            ],
        };

        let module = test_module(vec![function.clone()]);
        let lowered = lower_sir_function(&module, &function)
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
        let cancellation_blocks = lowered
            .checked
            .cooperate_sites
            .iter()
            .filter_map(|site| {
                lowered
                    .raw
                    .blocks
                    .iter()
                    .find(|block| block.id == site.bb_id)
                    .filter(|block| !matches!(block.terminator, Terminator::Unreachable))
                    .map(|_| site.bb_id)
            })
            .collect::<BTreeSet<_>>();
        assert_eq!(
            lowered.elaborated.drop_plans.len(),
            lowered
                .raw
                .blocks
                .iter()
                .filter(|block| !matches!(block.terminator, Terminator::Unreachable))
                .count()
                + cancellation_blocks.len(),
            "every strict SIR runtime or injected cancellation exit needs an explicit zero-drop plan"
        );
        assert!(lowered
            .elaborated
            .drop_plans
            .iter()
            .all(|(_, plan)| plan.drops.is_empty()));
        assert!(lowered
            .elaborated
            .drop_plans
            .iter()
            .any(|(exit, _)| { matches!(exit, ExitPath::Branch { block: 0, .. }) }));
        let trap_block = lowered
            .raw
            .blocks
            .iter()
            .find(|block| {
                matches!(
                    block.terminator,
                    Terminator::Trap {
                        kind: TrapKind::IntegerOverflow
                    }
                )
            })
            .expect("checked arithmetic must preserve an overflow trap block")
            .id;
        assert!(lowered.elaborated.drop_plans.iter().any(|(exit, plan)| {
            matches!(exit, ExitPath::Panic { block } if *block == trap_block)
                && plan.drops.is_empty()
        }));
        assert!(lowered
            .elaborated
            .blocks
            .iter()
            .any(|block| block.kind == BlockKind::Cleanup && block.drops.is_empty()));
        for block in cancellation_blocks {
            assert!(lowered.elaborated.drop_plans.iter().any(|(exit, plan)| {
                matches!(exit, ExitPath::Cancel { block: cancel_block } if *cancel_block == block)
                    && plan.drops.is_empty()
            }));
        }
    }

    #[test]
    fn rejects_unverified_sir_before_raw_realization() {
        let function = SemFunction {
            id: ItemId(0),
            callable: CallableId(0),
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
                    value: Some(operand(0)),
                },
            }],
        };

        let module = test_module(vec![function.clone()]);
        let error = lower_sir_function(&module, &function)
            .expect_err("a malformed SIR entry must fail before raw MIR exists");
        assert!(error
            .reason
            .contains("SIR module verifier rejected function"));
        assert!(error.reason.contains("EntryBlockArgs"));
    }

    #[test]
    fn rejects_noncanonical_sir_blocks_before_raw_realization() {
        let function = SemFunction {
            id: ItemId(0),
            callable: CallableId(0),
            declaration: DefId::for_test("noncanonical"),
            name: "noncanonical".to_string(),
            span: 0..0,
            source_origin: FunctionSourceOrigin::Unknown,
            params: Vec::new(),
            return_ty: ResolvedTy::I64,
            entry: BlockId(0),
            blocks: vec![
                SemBlock {
                    id: BlockId(0),
                    args: Vec::new(),
                    ops: Vec::new(),
                    terminator: SemTerminator::Goto(Edge {
                        target: BlockId(2),
                        args: Vec::new(),
                    }),
                },
                SemBlock {
                    id: BlockId(2),
                    args: Vec::new(),
                    ops: vec![op(
                        0,
                        definition(0, ResolvedTy::I64),
                        SemOpKind::ConstI64(1),
                    )],
                    terminator: SemTerminator::Return {
                        value: Some(operand(0)),
                    },
                },
            ],
        };
        let module = test_module(vec![function.clone()]);

        let error = lower_sir_function(&module, &function)
            .expect_err("noncanonical SIR must fail verification before raw-MIR realization");
        assert!(error
            .reason
            .contains("SIR module verifier rejected function"));
        assert!(error.reason.contains("NonCanonicalBlockOrder"));
    }

    #[test]
    #[allow(
        clippy::too_many_lines,
        reason = "the parallel-copy edge materialization contract is clearest as one complete CFG fixture"
    )]
    fn edge_argument_materialization_uses_parallel_copies() {
        let function = SemFunction {
            id: ItemId(0),
            callable: CallableId(0),
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
                        condition: operand(2),
                        then_target: Edge {
                            target: BlockId(1),
                            args: vec![operand(1), operand(0)],
                        },
                        else_target: Edge {
                            target: BlockId(1),
                            args: vec![operand(0), operand(1)],
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
                        value: Some(operand(3)),
                    },
                },
            ],
        };
        let module = test_module(vec![function.clone()]);
        let lowered = lower_sir_function(&module, &function)
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
        assert!(
            forwarders.iter().all(|block| block.id > 1),
            "SIR allocates edge-forwarding blocks after their bb1 target"
        );
        assert!(
            lowered.checked.cooperate_sites.is_empty(),
            "an acyclic SIR CFG with high-id forwarders must not acquire legacy numeric scheduler sites: {:?}",
            lowered.checked.cooperate_sites
        );
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
    #[allow(
        clippy::similar_names,
        clippy::too_many_lines,
        reason = "the complete direct-call component fixture keeps caller/callee ABI and continuation assertions together"
    )]
    fn realizes_a_closed_direct_call_component_without_a_raw_template() {
        let callee = SemFunction {
            id: ItemId(1),
            callable: CallableId(0),
            declaration: DefId::for_test("add_one"),
            name: "add_one".to_string(),
            span: 20..60,
            source_origin: FunctionSourceOrigin::RootUnit,
            params: vec![BlockArg {
                value: ValueId(0),
                ty: ResolvedTy::I64,
            }],
            return_ty: ResolvedTy::I64,
            entry: BlockId(0),
            blocks: vec![SemBlock {
                id: BlockId(0),
                args: Vec::new(),
                ops: vec![
                    op(0, definition(1, ResolvedTy::I64), SemOpKind::ConstI64(1)),
                    op(
                        1,
                        definition(2, ResolvedTy::I64),
                        SemOpKind::Binary {
                            op: BinaryOp::Add,
                            lhs: operand(0),
                            rhs: operand(1),
                        },
                    ),
                ],
                terminator: SemTerminator::Return {
                    value: Some(operand(2)),
                },
            }],
        };
        let caller = SemFunction {
            id: ItemId(0),
            callable: CallableId(1),
            declaration: DefId::for_test("main"),
            name: "main".to_string(),
            span: 0..80,
            source_origin: FunctionSourceOrigin::RootUnit,
            params: Vec::new(),
            return_ty: ResolvedTy::I64,
            entry: BlockId(0),
            blocks: vec![SemBlock {
                id: BlockId(0),
                args: Vec::new(),
                ops: vec![
                    op(0, definition(0, ResolvedTy::I64), SemOpKind::ConstI64(40)),
                    op(
                        1,
                        definition(1, ResolvedTy::I64),
                        SemOpKind::Call {
                            callee: CallableId(0),
                            args: vec![operand(0)],
                        },
                    ),
                    // This operation must land in the Call continuation; it
                    // proves raw-MIR's terminator call did not truncate the
                    // semantic source block.
                    op(2, definition(2, ResolvedTy::I64), SemOpKind::ConstI64(1)),
                    op(
                        3,
                        definition(3, ResolvedTy::I64),
                        SemOpKind::Binary {
                            op: BinaryOp::Add,
                            lhs: operand(1),
                            rhs: operand(2),
                        },
                    ),
                ],
                terminator: SemTerminator::Return {
                    value: Some(operand(3)),
                },
            }],
        };
        let module = test_module(vec![caller, callee]);
        let component = lower_closed_scalar_component(&module, &[CallableId(1)])
            .expect("a closed scalar SIR direct-call graph should lower independently");

        assert_eq!(component.callables(), &[CallableId(0), CallableId(1)]);
        let caller_raw = component
            .raw_mir
            .iter()
            .find(|raw| raw.name == "main")
            .expect("selected caller must have a fresh raw body");
        let (destination, continuation) = match &caller_raw.blocks[0].terminator {
            Terminator::Call {
                callee,
                authority,
                dest: Some(destination),
                next,
                ..
            } => {
                assert_eq!(callee, "add_one");
                assert_eq!(*authority, crate::CallAuthority::Direct);
                (*destination, *next)
            }
            other => panic!("expected SIR call to lower as raw terminator, got {other:?}"),
        };
        assert!(matches!(destination, Place::Local(_)));
        let call_next = continuation;
        let continuation = caller_raw
            .blocks
            .iter()
            .find(|block| block.id == call_next)
            .expect("raw call continuation must exist");
        assert!(continuation
            .instructions
            .iter()
            .any(|instruction| { matches!(instruction, Instr::ConstI64 { value: 1, .. }) }));
        assert!(continuation.instructions.iter().any(|instruction| {
            matches!(
                instruction,
                Instr::IntArithChecked {
                    op: IntArithOp::Add,
                    ..
                }
            )
        }));
        let callee_checked = component
            .checked_mir
            .iter()
            .find(|checked| checked.name == "add_one")
            .expect("callee must receive freshly-derived checked MIR");
        assert!(matches!(
            callee_checked.decisions.as_slice(),
            [crate::DecisionFact {
                strategy: Strategy::ParamBoundary(ParamBoundaryFact {
                    param_index: 0,
                    param_count: 1,
                    caller_visible_projection: false,
                    mode: ParamBoundaryMode::BorrowReadOnly,
                }),
                ..
            }]
        ));
        let main_cooperate_blocks = component
            .checked_mir
            .iter()
            .find(|checked| checked.name == "main")
            .expect("caller must receive freshly-derived checked MIR")
            .cooperate_sites
            .iter()
            .map(|site| site.bb_id)
            .collect::<BTreeSet<_>>();
        assert!(
            !main_cooperate_blocks.is_empty(),
            "the direct caller must retain a scheduler cooperation site"
        );
        let pipeline = component.into_pipeline();
        assert_eq!(pipeline.raw_mir.len(), 2);
        assert_eq!(pipeline.checked_mir.len(), 2);
        assert_eq!(pipeline.elaborated_mir.len(), 2);
        let main_elaborated = pipeline
            .elaborated_mir
            .iter()
            .find(|elaborated| elaborated.name == "main")
            .expect("selected caller must retain explicit elaboration");
        assert!(main_elaborated.drop_plans.iter().any(|(exit, plan)| {
            matches!(
                exit,
                ExitPath::Call {
                    block: 0,
                    callee,
                    next,
                } if callee == "add_one" && *next == call_next
            ) && plan.drops.is_empty()
        }));
        for block in main_cooperate_blocks {
            assert!(main_elaborated.drop_plans.iter().any(|(exit, plan)| {
                matches!(exit, ExitPath::Cancel { block: cancel_block } if *cancel_block == block)
                    && plan.drops.is_empty()
            }));
        }
        for ((raw, checked), elaborated) in pipeline
            .raw_mir
            .iter()
            .zip(&pipeline.checked_mir)
            .zip(&pipeline.elaborated_mir)
        {
            let cancellation_blocks = checked
                .cooperate_sites
                .iter()
                .filter_map(|site| {
                    raw.blocks
                        .iter()
                        .find(|block| block.id == site.bb_id)
                        .filter(|block| !matches!(block.terminator, Terminator::Unreachable))
                        .map(|_| site.bb_id)
                })
                .collect::<BTreeSet<_>>();
            assert_eq!(
                elaborated.drop_plans.len(),
                raw.blocks
                    .iter()
                    .filter(|block| !matches!(block.terminator, Terminator::Unreachable))
                    .count()
                    + cancellation_blocks.len(),
                "every strict SIR runtime or injected cancellation exit must retain its zero-drop elaboration plan"
            );
            assert!(elaborated
                .drop_plans
                .iter()
                .all(|(_, plan)| plan.drops.is_empty()));
            for block in cancellation_blocks {
                assert!(elaborated.drop_plans.iter().any(|(exit, plan)| {
                    matches!(exit, ExitPath::Cancel { block: cancel_block } if *cancel_block == block)
                        && plan.drops.is_empty()
                }));
            }
        }
    }

    #[test]
    #[allow(
        clippy::similar_names,
        reason = "the focused unit-call component fixture intentionally contrasts callee and caller raw-MIR realization"
    )]
    fn realizes_a_zero_result_unit_direct_call_without_a_raw_template() {
        let callee = SemFunction {
            id: ItemId(1),
            callable: CallableId(0),
            declaration: DefId::for_test("record"),
            name: "record".to_string(),
            span: 20..40,
            source_origin: FunctionSourceOrigin::RootUnit,
            params: vec![BlockArg {
                value: ValueId(0),
                ty: ResolvedTy::I64,
            }],
            return_ty: ResolvedTy::Unit,
            entry: BlockId(0),
            blocks: vec![SemBlock {
                id: BlockId(0),
                args: Vec::new(),
                ops: Vec::new(),
                terminator: SemTerminator::Return { value: None },
            }],
        };
        let caller = SemFunction {
            id: ItemId(0),
            callable: CallableId(1),
            declaration: DefId::for_test("main"),
            name: "main".to_string(),
            span: 0..60,
            source_origin: FunctionSourceOrigin::RootUnit,
            params: Vec::new(),
            return_ty: ResolvedTy::Unit,
            entry: BlockId(0),
            blocks: vec![SemBlock {
                id: BlockId(0),
                args: Vec::new(),
                ops: vec![
                    op(0, definition(0, ResolvedTy::I64), SemOpKind::ConstI64(7)),
                    zero_result_op(
                        1,
                        SemOpKind::Call {
                            callee: CallableId(0),
                            args: vec![operand(0)],
                        },
                    ),
                ],
                terminator: SemTerminator::Return { value: None },
            }],
        };
        let component =
            lower_closed_scalar_component(&test_module(vec![caller, callee]), &[CallableId(1)])
                .expect("a closed unit direct-call graph should lower independently");

        let caller_raw = component
            .raw_mir
            .iter()
            .find(|raw| raw.name == "main")
            .expect("selected unit caller must have a fresh raw body");
        let continuation = match &caller_raw.blocks[0].terminator {
            Terminator::Call {
                callee,
                authority,
                args,
                dest: None,
                next,
            } => {
                assert_eq!(callee, "record");
                assert_eq!(*authority, crate::CallAuthority::Direct);
                assert_eq!(args.len(), 1);
                *next
            }
            other => panic!("expected zero-result SIR call to lower as raw call, got {other:?}"),
        };
        assert_eq!(
            caller_raw.locals.len(),
            1,
            "the unit call must not allocate a raw-MIR destination local"
        );
        assert!(matches!(
            caller_raw
                .blocks
                .iter()
                .find(|block| block.id == continuation)
                .expect("raw unit-call continuation must exist")
                .terminator,
            Terminator::Return
        ));
    }

    #[test]
    fn rejects_a_reachable_callable_without_a_sir_body_atomically() {
        let caller = SemFunction {
            id: ItemId(0),
            callable: CallableId(0),
            declaration: DefId::for_test("main"),
            name: "main".to_string(),
            span: 0..20,
            source_origin: FunctionSourceOrigin::RootUnit,
            params: Vec::new(),
            return_ty: ResolvedTy::I64,
            entry: BlockId(0),
            blocks: vec![SemBlock {
                id: BlockId(0),
                args: Vec::new(),
                ops: vec![op(
                    0,
                    definition(0, ResolvedTy::I64),
                    SemOpKind::Call {
                        callee: CallableId(1),
                        args: Vec::new(),
                    },
                )],
                terminator: SemTerminator::Return {
                    value: Some(operand(0)),
                },
            }],
        };
        let mut module = test_module(vec![caller]);
        module.callables.push(SemCallable {
            id: CallableId(1),
            function: ItemId(1),
            declaration: DefId::for_test("missing"),
            instance: CallableInstance::Monomorphic,
            symbol: "missing".to_string(),
            source_origin: FunctionSourceOrigin::Unknown,
            signature: SemSignature {
                params: Vec::new(),
                return_ty: ResolvedTy::I64,
            },
            call_conv: SemCallConv::Default,
            kind: SemCallableKind::HewDirect,
            effect_summary: hew_sir::EffectSummary::Unknown,
        });

        let error = lower_closed_scalar_component(&module, &[CallableId(0)])
            .expect_err("a reachable callable without a SIR body must not fall back");
        assert!(error
            .reason
            .contains("requires one lowered body for `missing`"));
        assert!(error.reason.contains("main → missing"));
    }

    #[test]
    fn module_verification_rejects_two_bodies_for_one_callable() {
        let function = SemFunction {
            id: ItemId(0),
            callable: CallableId(0),
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
                    value: Some(operand(0)),
                },
            }],
        };
        let mut duplicate = function.clone();
        duplicate.id = ItemId(1);

        // Negative control: one body for the callable is a legal component.
        lower_closed_scalar_component(&test_module(vec![function.clone()]), &[CallableId(0)])
            .expect("a single body for the callable must realize");

        let error = lower_closed_scalar_component(
            &test_module(vec![function, duplicate]),
            &[CallableId(0)],
        )
        .expect_err("two bodies for one callable must fail module verification");
        assert!(
            error.reason.contains("SIR module verifier rejected"),
            "{}",
            error.reason
        );
    }

    /// The strict lane selects a program from the entry fact alone. Removing
    /// the fact must refuse with a typed error, not fall through to some other
    /// root body, and not go looking for a callable named `main`.
    #[test]
    fn entry_component_lowering_fails_closed_without_an_entry_fact() {
        let function = SemFunction {
            id: ItemId(0),
            callable: CallableId(0),
            declaration: DefId::for_test("main"),
            name: "main".to_string(),
            span: 0..0,
            source_origin: FunctionSourceOrigin::RootUnit,
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
                    value: Some(operand(0)),
                },
            }],
        };
        let mut module = test_module(vec![function]);

        // Positive control: with the entry fact present this exact module is a
        // program, so the refusal below is about the fact and nothing else.
        let component = lower_entry_component(&module)
            .expect("an entry fact must select the component it names");
        assert_eq!(component.callables(), &[CallableId(0)]);

        module.entry_callable = None;
        let error = lower_entry_component(&module)
            .expect_err("a module with no entry fact is not an executable program");
        assert!(
            error.reason.contains("no HIR entry declaration"),
            "{}",
            error.reason
        );
        assert_eq!(
            error.missing_body, None,
            "a missing entry fact is not a missing SIR body"
        );
    }
}
