//! Verification facts for Raw MIR's first value-only execution subset.
//!
//! Raw MIR normally owns addressable [`crate::Place`]s and their lifetime
//! realization.  This deliberately narrow lane instead carries a small,
//! typed stream of virtual values until the one permitted representation
//! boundary: `ReturnAbi -> Place::ReturnSlot`.  Keeping its semantic verifier
//! in `hew-mir` makes the Raw -> Checked -> Elaborated contract authoritative
//! for both SIR lowering and direct Raw -> LLVM consumers.

use std::collections::BTreeMap;
use std::fmt;

use hew_hir::{IntentKind, SiteId, ValueClass};
use hew_types::ResolvedTy;

use crate::{
    dataflow, validate_context_markers, BlockKind, CheckedMirFunction, DropPlan,
    ElaboratedMirFunction, ExitPath, FunctionCallConv, Instr, ParamBoundaryMode, Place,
    RawMirFunction, RawValueDef, RawValueId, RawValueOp, SourceOrigin, Strategy, Terminator,
    ValueMaterializationReason,
};

/// Semantic facts proven for an admitted Raw virtual-value function.
///
/// `parameter_values[index]` is the one virtual definition of ABI parameter
/// `index`; keeping it ordered makes the ABI-prefix invariant explicit instead
/// of leaving codegen to reconstruct it from a hash map.  `value_tys` is the
/// canonical semantic type map used to reject LLVM-type aliases such as
/// `i64`/`u64` before LLVM lowering.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RawVirtualValueFacts {
    pub value_tys: BTreeMap<RawValueId, ResolvedTy>,
    pub parameter_values: Vec<RawValueId>,
    pub return_value: Option<RawValueId>,
}

/// A malformed first-slice virtual Raw-MIR body or its required ladder stage.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RawVirtualValueError {
    pub reason: String,
}

impl RawVirtualValueError {
    fn new(reason: impl Into<String>) -> Self {
        Self {
            reason: reason.into(),
        }
    }
}

impl fmt::Display for RawVirtualValueError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(&self.reason)
    }
}

impl std::error::Error for RawVirtualValueError {}

/// Whether a Raw body enters the value-only lane at all.
#[must_use]
pub fn raw_uses_virtual_values(raw: &RawMirFunction) -> bool {
    raw.blocks.iter().any(|block| {
        block.instructions.iter().any(|instruction| {
            matches!(
                instruction,
                Instr::Value(_) | Instr::MaterializeValue { .. }
            )
        })
    })
}

/// Whether a type can cross the initial virtual-value function ABI.
///
/// Tuples deliberately do not qualify: they are internal value expressions in
/// this slice, not an ABI decision.
#[must_use]
pub fn is_supported_raw_virtual_scalar_type(ty: &ResolvedTy) -> bool {
    ty.is_integer() || *ty == ResolvedTy::Bool
}

/// Closed semantic classes admitted by Raw MIR's first virtual-value lane.
///
/// Every production consumer of virtual values matches this enum exhaustively.
/// Adding an ownership-bearing or representation-bearing class therefore
/// breaks those consumers at compile time until each one makes an explicit
/// storage, suspension, and drop decision.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RawVirtualClass {
    Integer,
    Bool,
    Tuple,
}

/// Classify a type admitted for an internal Raw virtual value.
///
/// The wildcard is deliberately fail-closed over [`ResolvedTy`]: a new type is
/// rejected until it is assigned an explicit [`RawVirtualClass`].
#[must_use]
pub fn raw_virtual_class(ty: &ResolvedTy) -> Option<RawVirtualClass> {
    match ty {
        ResolvedTy::I8
        | ResolvedTy::I16
        | ResolvedTy::I32
        | ResolvedTy::I64
        | ResolvedTy::U8
        | ResolvedTy::U16
        | ResolvedTy::U32
        | ResolvedTy::U64
        | ResolvedTy::Isize
        | ResolvedTy::Usize => Some(RawVirtualClass::Integer),
        ResolvedTy::Bool => Some(RawVirtualClass::Bool),
        ResolvedTy::Tuple(elements)
            if !elements.is_empty()
                && elements.iter().all(|element| {
                    matches!(
                        raw_virtual_class(element),
                        Some(
                            RawVirtualClass::Integer
                                | RawVirtualClass::Bool
                                | RawVirtualClass::Tuple
                        )
                    )
                }) =>
        {
            Some(RawVirtualClass::Tuple)
        }
        _ => None,
    }
}

/// Return the admitted class of a Raw virtual-value operation's definition.
#[must_use]
pub fn raw_virtual_operation_class(operation: &RawValueOp) -> Option<RawVirtualClass> {
    let dest = match operation {
        RawValueOp::Param { dest, .. }
        | RawValueOp::ConstI64 { dest, .. }
        | RawValueOp::ConstBool { dest, .. }
        | RawValueOp::TupleMake { dest, .. }
        | RawValueOp::TupleGet { dest, .. } => dest,
    };
    raw_virtual_class(&dest.ty)
}

/// Verify a Raw body in the first, one-block virtual-value subset.
///
/// Returns `Ok(None)` for ordinary storage-oriented Raw MIR.  A value body
/// must use the exact no-storage subset; callers can use the returned facts to
/// bind parameters and values without re-deriving semantic types.
///
/// # Errors
///
/// Returns [`RawVirtualValueError`] when a body enters the virtual lane but
/// violates its scalar ABI, semantic def-use, or no-storage invariants.
#[expect(
    clippy::too_many_lines,
    reason = "the initial Raw virtual-value contract is intentionally exhaustive and centralized"
)]
pub fn verify_raw_virtual_value_function(
    raw: &RawMirFunction,
) -> Result<Option<RawVirtualValueFacts>, RawVirtualValueError> {
    if !raw_uses_virtual_values(raw) {
        return Ok(None);
    }

    if raw.call_conv != FunctionCallConv::Default {
        return Err(raw_virtual_error(format!(
            "raw virtual-value function `{}` requires the default call convention",
            raw.name
        )));
    }
    if raw.coroutine_facts().is_coroutine {
        return Err(raw_virtual_error(format!(
            "raw virtual-value function `{}` carries coroutine realization facts outside the first value-only slice",
            raw.name
        )));
    }
    if matches!(
        &raw.source_origin,
        SourceOrigin::SynthesizedActorHandler { .. } | SourceOrigin::SynthesizedMachineStep { .. }
    ) {
        return Err(raw_virtual_error(format!(
            "raw virtual-value function `{}` carries synthesized actor or machine provenance outside the first value-only slice",
            raw.name
        )));
    }
    if !raw.locals.is_empty()
        || !raw.local_names.is_empty()
        || !raw.local_scopes.is_empty()
        || !raw.local_decl_bytes.is_empty()
        || !raw.scope_table.is_empty()
    {
        return Err(raw_virtual_error(format!(
            "raw virtual-value function `{}` declares local storage; virtual values have no Place::Local representation",
            raw.name
        )));
    }
    if raw.intrinsic_id.is_some()
        || !raw.await_deadline_ns.is_empty()
        || !raw.suspend_kinds.is_empty()
        || !raw.lambda_actor_user_param_locals.is_empty()
    {
        return Err(raw_virtual_error(format!(
            "raw virtual-value function `{}` carries intrinsic, suspension, or actor ABI facts outside the first value-only slice",
            raw.name
        )));
    }
    if raw.return_ty != ResolvedTy::Unit && !is_supported_raw_virtual_scalar_type(&raw.return_ty) {
        return Err(raw_virtual_error(format!(
            "raw virtual-value function `{}` return type `{}` is outside the scalar-or-unit ABI subset; tuples remain internal values",
            raw.name,
            raw.return_ty.user_facing()
        )));
    }
    verify_virtual_parameter_decisions(raw)?;

    let [block] = raw.blocks.as_slice() else {
        return Err(raw_virtual_error(format!(
            "raw virtual-value function `{}` must contain exactly entry bb0",
            raw.name
        )));
    };
    if block.id != 0
        || !block.statements.is_empty()
        || !matches!(block.terminator, Terminator::Return)
    {
        return Err(raw_virtual_error(format!(
            "raw virtual-value function `{}` must have a statement-free bb0 Return body",
            raw.name
        )));
    }

    let mut value_tys = BTreeMap::new();
    let mut parameter_values = Vec::with_capacity(raw.params.len());
    let mut next_instruction = 0_usize;
    for (parameter_index, parameter_ty) in raw.params.iter().enumerate() {
        if !is_supported_raw_virtual_scalar_type(parameter_ty) {
            return Err(raw_virtual_error(format!(
                "raw virtual-value function `{}` ABI parameter {parameter_index} `{}` is outside the scalar ABI subset",
                raw.name,
                parameter_ty.user_facing()
            )));
        }
        let expected_index = u32::try_from(parameter_index).map_err(|_| {
            raw_virtual_error(format!(
                "raw virtual-value function `{}` exceeds the u32 parameter ABI range",
                raw.name
            ))
        })?;
        let context = format!(
            "raw virtual-value function `{}` bb0 instruction {next_instruction}",
            raw.name
        );
        let Some(Instr::Value(RawValueOp::Param { dest, index })) =
            block.instructions.get(next_instruction)
        else {
            return Err(raw_virtual_error(format!(
                "{context} must define ABI parameter {expected_index} in the ordered RawValueOp::Param prefix"
            )));
        };
        if *index != expected_index {
            return Err(raw_virtual_error(format!(
                "{context} defines ABI parameter {index}, expected ordered parameter {expected_index}"
            )));
        }
        if dest.ty != *parameter_ty {
            return Err(raw_virtual_error(format!(
                "{context} virtual parameter %{} type `{}` does not match ABI parameter {parameter_index} type `{}`",
                dest.id.0,
                dest.ty.user_facing(),
                parameter_ty.user_facing()
            )));
        }
        define_virtual_value(&mut value_tys, dest, &context)?;
        parameter_values.push(dest.id);
        next_instruction = next_instruction.saturating_add(1);
    }

    let mut return_value = None;
    for (instruction_index, instruction) in
        block.instructions.iter().enumerate().skip(next_instruction)
    {
        let context = format!(
            "raw virtual-value function `{}` bb0 instruction {instruction_index}",
            raw.name
        );
        if return_value.is_some() {
            return Err(raw_virtual_error(format!(
                "{context} follows ReturnAbi materialization"
            )));
        }
        match instruction {
            Instr::Value(RawValueOp::Param { .. }) => {
                return Err(raw_virtual_error(format!(
                    "{context} defines a parameter outside the ordered ABI prefix"
                )));
            }
            Instr::Value(operation) => {
                verify_virtual_value_operation(&mut value_tys, operation, &context)?;
            }
            Instr::MaterializeValue {
                dest,
                value,
                reason,
            } => {
                if *dest != Place::ReturnSlot || *reason != ValueMaterializationReason::ReturnAbi {
                    return Err(raw_virtual_error(format!(
                        "{context} materializes outside the ReturnAbi -> ReturnSlot boundary"
                    )));
                }
                let value_ty = value_tys.get(value).ok_or_else(|| {
                    raw_virtual_error(format!(
                        "{context} materializes undefined virtual value %{}",
                        value.0
                    ))
                })?;
                if raw.return_ty == ResolvedTy::Unit || value_ty != &raw.return_ty {
                    return Err(raw_virtual_error(format!(
                        "{context} materializes `{}` for scalar return `{}`",
                        value_ty.user_facing(),
                        raw.return_ty.user_facing()
                    )));
                }
                return_value = Some(*value);
            }
            other => {
                return Err(raw_virtual_error(format!(
                    "{context} uses storage-oriented instruction {other:?}; virtual-value functions cannot mix RawValueOp with Place-based lowering"
                )));
            }
        }
    }

    match (raw.return_ty == ResolvedTy::Unit, return_value) {
        (true, None) | (false, Some(_)) => {}
        (true, Some(_)) => {
            return Err(raw_virtual_error(format!(
                "unit raw virtual-value function `{}` must not materialize a return value",
                raw.name
            )));
        }
        (false, None) => {
            return Err(raw_virtual_error(format!(
                "non-unit raw virtual-value function `{}` lacks ReturnAbi materialization",
                raw.name
            )));
        }
    }

    Ok(Some(RawVirtualValueFacts {
        value_tys,
        parameter_values,
        return_value,
    }))
}

/// Verify that Checked MIR is the scheduler-free, context-free mirror of an
/// admitted virtual Raw body.
///
/// # Errors
///
/// Returns [`RawVirtualValueError`] when the facts no longer match Raw MIR or
/// Checked MIR introduces divergent CFG, context, or scheduler state.
pub fn verify_raw_virtual_value_checked(
    raw: &RawMirFunction,
    checked: &CheckedMirFunction,
    facts: &RawVirtualValueFacts,
) -> Result<(), RawVirtualValueError> {
    verify_facts_match_raw(raw, facts)?;
    if checked.name != raw.name || checked.return_ty != raw.return_ty {
        return Err(raw_virtual_error(format!(
            "raw virtual-value checked MIR does not match Raw identity for `{}`",
            raw.name
        )));
    }
    if checked.blocks != raw.blocks || checked.decisions != raw.decisions {
        return Err(raw_virtual_error(format!(
            "raw virtual-value checked MIR does not mirror Raw blocks or decisions for `{}`",
            raw.name
        )));
    }

    let expected_checks = validate_context_markers(raw);
    if !expected_checks.is_empty() {
        return Err(raw_virtual_error(format!(
            "raw virtual-value function `{}` unexpectedly requires execution-context checks",
            raw.name
        )));
    }
    if !checked.checks.is_empty() {
        return Err(raw_virtual_error(format!(
            "raw virtual-value checked MIR for `{}` carries context findings outside the value-only subset",
            raw.name
        )));
    }

    let expected_cooperate_sites = dataflow::compute_structural_cooperate_sites(&raw.blocks);
    if !expected_cooperate_sites.is_empty() {
        return Err(raw_virtual_error(format!(
            "raw virtual-value function `{}` requires scheduler cooperation outside the first value-only subset",
            raw.name
        )));
    }
    if !checked.cooperate_sites.is_empty() {
        return Err(raw_virtual_error(format!(
            "raw virtual-value checked MIR for `{}` carries scheduler cooperate sites outside the value-only subset",
            raw.name
        )));
    }
    Ok(())
}

/// Verify the zero-drop Elaborated-MIR mirror required by a virtual Raw body.
///
/// # Errors
///
/// Returns [`RawVirtualValueError`] when Raw/Checked verification fails or the
/// Elaborated body carries any ownership, drop, coroutine, or cleanup work.
pub fn verify_raw_virtual_value_elaborated(
    raw: &RawMirFunction,
    checked: &CheckedMirFunction,
    elaborated: &ElaboratedMirFunction,
    facts: &RawVirtualValueFacts,
) -> Result<(), RawVirtualValueError> {
    verify_raw_virtual_value_checked(raw, checked, facts)?;
    if elaborated.name != raw.name
        || elaborated.return_ty != raw.return_ty
        || elaborated.decisions != checked.decisions
        || !elaborated.statements.is_empty()
        || elaborated.coroutine.is_some()
        || !elaborated.lambda_captures.is_empty()
    {
        return Err(raw_virtual_error(format!(
            "raw virtual-value Elaborated MIR does not preserve the zero-drop identity of `{}`",
            raw.name
        )));
    }
    let [block] = elaborated.blocks.as_slice() else {
        return Err(raw_virtual_error(format!(
            "raw virtual-value Elaborated MIR for `{}` must retain exactly one normal block",
            raw.name
        )));
    };
    if block.id != 0
        || block.kind != BlockKind::Normal
        || !block.drops.is_empty()
        || block.successor.is_some()
    {
        return Err(raw_virtual_error(format!(
            "raw virtual-value Elaborated MIR for `{}` carries storage or drop work in bb0",
            raw.name
        )));
    }
    let [(exit, plan)] = elaborated.drop_plans.as_slice() else {
        return Err(raw_virtual_error(format!(
            "raw virtual-value Elaborated MIR for `{}` must contain one return drop plan",
            raw.name
        )));
    };
    if *exit != (ExitPath::Return { block: 0 }) || *plan != DropPlan::default() {
        return Err(raw_virtual_error(format!(
            "raw virtual-value Elaborated MIR for `{}` has a non-empty or non-return exit plan",
            raw.name
        )));
    }
    Ok(())
}

/// Verify the mandatory Raw -> Checked -> Elaborated ladder for a virtual Raw
/// body, returning the canonical facts codegen may use for LLVM realization.
///
/// Legacy storage-oriented Raw MIR deliberately retains its existing optional
/// stage behavior.  Once a body contains any virtual value, however, both
/// mirrors are mandatory so a hand-built pipeline cannot bypass ownership or
/// scheduler admission at codegen.
///
/// # Errors
///
/// Returns [`RawVirtualValueError`] when a virtual Raw body is malformed or
/// either mandatory ladder mirror is missing or not the zero-effect mirror.
pub fn verify_raw_virtual_value_ladder(
    raw: &RawMirFunction,
    checked: Option<&CheckedMirFunction>,
    elaborated: Option<&ElaboratedMirFunction>,
) -> Result<Option<RawVirtualValueFacts>, RawVirtualValueError> {
    let Some(facts) = verify_raw_virtual_value_function(raw)? else {
        return Ok(None);
    };
    let checked = checked.ok_or_else(|| {
        raw_virtual_error(format!(
            "raw virtual-value function `{}` requires matching Checked MIR before codegen",
            raw.name
        ))
    })?;
    let elaborated = elaborated.ok_or_else(|| {
        raw_virtual_error(format!(
            "raw virtual-value function `{}` requires matching Elaborated MIR before codegen",
            raw.name
        ))
    })?;
    verify_raw_virtual_value_elaborated(raw, checked, elaborated, &facts)?;
    Ok(Some(facts))
}

fn raw_virtual_error(reason: impl Into<String>) -> RawVirtualValueError {
    RawVirtualValueError::new(reason)
}

fn verify_facts_match_raw(
    raw: &RawMirFunction,
    facts: &RawVirtualValueFacts,
) -> Result<(), RawVirtualValueError> {
    let Some(expected) = verify_raw_virtual_value_function(raw)? else {
        return Err(raw_virtual_error(format!(
            "ordinary Raw function `{}` cannot carry virtual-value facts",
            raw.name
        )));
    };
    if expected != *facts {
        return Err(raw_virtual_error(format!(
            "raw virtual-value facts do not match the canonical Raw verification for `{}`",
            raw.name
        )));
    }
    Ok(())
}

/// Validate the Raw-owned ABI decision stream without consulting SIR.
///
/// The first virtual-value lane has only immutable `BitCopy` parameters. The
/// decision facts are still codegen-visible authority, so accepting a missing
/// or forged stream merely because Checked mirrors it would create a second
/// unverified ABI path.
fn verify_virtual_parameter_decisions(raw: &RawMirFunction) -> Result<(), RawVirtualValueError> {
    if raw.decisions.len() != raw.params.len() {
        return Err(raw_virtual_error(format!(
            "raw virtual-value function `{}` has {} parameter-boundary facts for {} ABI parameters",
            raw.name,
            raw.decisions.len(),
            raw.params.len()
        )));
    }
    let parameter_count = u32::try_from(raw.params.len()).map_err(|_| {
        raw_virtual_error(format!(
            "raw virtual-value function `{}` exceeds the u32 parameter ABI range",
            raw.name
        ))
    })?;
    for (index, (decision, parameter_ty)) in raw.decisions.iter().zip(&raw.params).enumerate() {
        let parameter_index = u32::try_from(index).map_err(|_| {
            raw_virtual_error(format!(
                "raw virtual-value function `{}` exceeds the u32 parameter ABI range",
                raw.name
            ))
        })?;
        let Strategy::ParamBoundary(fact) = &decision.strategy else {
            return Err(raw_virtual_error(format!(
                "raw virtual-value function `{}` decision {index} is not an ordered ParamBoundary fact",
                raw.name
            )));
        };
        if decision.site != SiteId(parameter_index)
            || decision.ty != *parameter_ty
            || decision.value_class != ValueClass::BitCopy
            || decision.intent != IntentKind::Unknown
            || fact.param_index != parameter_index
            || fact.param_count != parameter_count
            || fact.caller_visible_projection
            || fact.mode != ParamBoundaryMode::BorrowReadOnly
        {
            return Err(raw_virtual_error(format!(
                "raw virtual-value function `{}` parameter-boundary fact {index} is not the immutable BitCopy ABI fact for parameter {parameter_index}",
                raw.name
            )));
        }
    }
    Ok(())
}

fn define_virtual_value(
    value_tys: &mut BTreeMap<RawValueId, ResolvedTy>,
    dest: &RawValueDef,
    context: &str,
) -> Result<(), RawVirtualValueError> {
    if raw_virtual_class(&dest.ty).is_none() {
        return Err(raw_virtual_error(format!(
            "{context} defines unsupported virtual value %{} type `{}`",
            dest.id.0,
            dest.ty.user_facing()
        )));
    }
    if value_tys.insert(dest.id, dest.ty.clone()).is_some() {
        return Err(raw_virtual_error(format!(
            "{context} defines virtual value %{} more than once",
            dest.id.0
        )));
    }
    Ok(())
}

fn verify_virtual_value_operation(
    value_tys: &mut BTreeMap<RawValueId, ResolvedTy>,
    operation: &RawValueOp,
    context: &str,
) -> Result<(), RawVirtualValueError> {
    match operation {
        RawValueOp::Param { .. } => Err(raw_virtual_error(format!(
            "{context} parameter definition escaped the ABI prefix"
        ))),
        RawValueOp::ConstI64 { dest, .. } => {
            if !dest.ty.is_integer() {
                return Err(raw_virtual_error(format!(
                    "{context} integer constant has non-integer type `{}`",
                    dest.ty.user_facing()
                )));
            }
            define_virtual_value(value_tys, dest, context)
        }
        RawValueOp::ConstBool { dest, .. } => {
            if dest.ty != ResolvedTy::Bool {
                return Err(raw_virtual_error(format!(
                    "{context} boolean constant has type `{}`",
                    dest.ty.user_facing()
                )));
            }
            define_virtual_value(value_tys, dest, context)
        }
        RawValueOp::TupleMake { dest, fields } => {
            let ResolvedTy::Tuple(element_tys) = &dest.ty else {
                return Err(raw_virtual_error(format!(
                    "{context} tuple.make result %{} is not a tuple",
                    dest.id.0
                )));
            };
            if fields.len() != element_tys.len() {
                return Err(raw_virtual_error(format!(
                    "{context} tuple.make has {} fields for {} semantic elements",
                    fields.len(),
                    element_tys.len()
                )));
            }
            for (field_index, (field, expected_ty)) in fields.iter().zip(element_tys).enumerate() {
                let actual_ty = value_tys.get(field).ok_or_else(|| {
                    raw_virtual_error(format!(
                        "{context} tuple.make field {field_index} reads undefined virtual value %{}",
                        field.0
                    ))
                })?;
                if actual_ty != expected_ty {
                    return Err(raw_virtual_error(format!(
                        "{context} tuple.make field {field_index} has `{}`, expected `{}`",
                        actual_ty.user_facing(),
                        expected_ty.user_facing()
                    )));
                }
            }
            define_virtual_value(value_tys, dest, context)
        }
        RawValueOp::TupleGet { dest, tuple, index } => {
            let element_tys = match value_tys.get(tuple) {
                Some(ResolvedTy::Tuple(element_tys)) => element_tys,
                Some(tuple_ty) => {
                    return Err(raw_virtual_error(format!(
                        "{context} tuple.get reads non-tuple `{}`",
                        tuple_ty.user_facing()
                    )));
                }
                None => {
                    return Err(raw_virtual_error(format!(
                        "{context} tuple.get reads undefined virtual value %{}",
                        tuple.0
                    )));
                }
            };
            let index = usize::try_from(*index).map_err(|_| {
                raw_virtual_error(format!("{context} tuple.get index cannot index a tuple"))
            })?;
            let expected_ty = element_tys.get(index).ok_or_else(|| {
                raw_virtual_error(format!(
                    "{context} tuple.get index {index} is out of bounds"
                ))
            })?;
            if &dest.ty != expected_ty {
                return Err(raw_virtual_error(format!(
                    "{context} tuple.get result `{}` does not match `{}`",
                    dest.ty.user_facing(),
                    expected_ty.user_facing()
                )));
            }
            define_virtual_value(value_tys, dest, context)
        }
    }
}
