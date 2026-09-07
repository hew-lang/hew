use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

use crate::ownership::TypeFactTable;
use crate::{
    AggregateShapeId, AggregateShapeRef, BindingTarget, BlockId, CallableId, CallableInstance,
    GenericTemplateId, SemAggregateShape, SemCallConv, SemCallable, SemCallableKind, SemFunction,
    SemGenericTemplate, SemModule, SemOp, SemOpKind, SemParamPassing, SemSignature, SemTerminator,
    SemVariantShape, SirInstanceKey, UseSite, ValueId, VariantShapeId,
};
use crate::{OpId, OwnKind};
use hew_hir::{monomorph::function_monomorph_symbol, substitute_type_params};
use hew_types::ResolvedTy;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SirDiagnosticKind {
    DuplicateFunctionName(String),
    DuplicateFunctionDeclaration(String),
    DuplicateCallableId(CallableId),
    DuplicateCallableDeclaration(String),
    DuplicateCallableInstance(String),
    DuplicateCallableSymbol(String),
    DuplicateGenericTemplate(String),
    InvalidGenericTemplate {
        template: String,
        reason: String,
    },
    InvalidCallable {
        callable: CallableId,
        reason: String,
    },
    InvalidAggregateShape {
        shape: AggregateShapeId,
        reason: String,
    },
    InvalidResourceType {
        ty: ResolvedTy,
        reason: String,
    },
    InvalidCollectionType {
        ty: ResolvedTy,
        reason: String,
    },
    InvalidValueCapability {
        ty: ResolvedTy,
        capability: hew_types::ValueCapability,
        reason: String,
    },
    InvalidVariantShape {
        shape: VariantShapeId,
        reason: String,
    },
    InvalidRootCallable {
        callable: CallableId,
        reason: String,
    },
    /// `entry_callable` is an executable-program boundary, not merely one of
    /// the source roots.  Keep its source identity and ABI rule separate from
    /// the general root-unit table invariant so a malformed entry cannot
    /// silently become an arbitrary callable during SIR → MIR lowering.
    InvalidEntryCallable {
        callable: CallableId,
        reason: String,
    },
    MissingFunctionCallable {
        declaration: String,
    },
    FunctionCallableMismatch {
        callable: CallableId,
        reason: String,
    },
    UnknownCallable {
        op: OpId,
        callee: CallableId,
    },
    MissingEntry(BlockId),
    EntryBlockArgs {
        entry: BlockId,
        actual: usize,
    },
    /// SIR block IDs are vector positions as well as CFG identities at the
    /// raw-MIR realization boundary.  Keep that representation invariant
    /// explicit so consumers may safely index a verified function by ID.
    NonCanonicalBlockOrder {
        expected: BlockId,
        actual: BlockId,
    },
    DuplicateBlock(BlockId),
    UnknownBlock(BlockId),
    EdgeArity {
        from: BlockId,
        to: BlockId,
        expected: usize,
        actual: usize,
    },
    EdgeType {
        from: BlockId,
        to: BlockId,
        argument: usize,
        expected: String,
        actual: String,
    },
    DuplicateValue(ValueId),
    DuplicateOp(OpId),
    InvalidResultArity {
        op: OpId,
        actual: usize,
    },
    InvalidCallResultArity {
        op: OpId,
        callee: CallableId,
        expected: usize,
        actual: usize,
    },
    InvalidConstType {
        op: OpId,
        expected: &'static str,
        actual: String,
    },
    InvalidCast {
        op: OpId,
        reason: String,
    },
    InvalidOperation {
        op: OpId,
        reason: String,
    },
    /// A value definition whose §1.2 ownership kind is not the one the class
    /// table gives its type, or whose type §1.1 cannot class at all. The kind
    /// is a pure function of the type, so a definition that says otherwise is
    /// a fact no later phase can trust.
    OwnershipKind {
        value: ValueId,
        reason: String,
    },
    /// An owned SSA obligation is unavailable at a use, overwritten by a new
    /// dynamic definition, or left live at an exit.
    OwnershipLifetime {
        block: BlockId,
        value: ValueId,
        reason: &'static str,
    },
    /// Local storage activity, content availability or cleanup is invalid.
    PlaceLifetime {
        block: BlockId,
        place: crate::PlaceId,
        reason: &'static str,
    },
    /// The function-owned fault must be present at propagation and cannot be lost.
    FaultLifetime {
        block: BlockId,
        reason: &'static str,
    },
    /// Call results must be forwarded through that call's normal edge; the
    /// continuation uses its block argument, never the edge-local definition.
    InvalidCallResultUse {
        value: ValueId,
        definition: BlockId,
        use_block: BlockId,
    },
    /// A source binding naming a value or place this body never defines. §1.6
    /// reads the table to tell a user-facing wall from an internal error, so a
    /// row it cannot resolve would silently drop the user's name.
    UnknownBinding {
        name: String,
        target: BindingTarget,
    },
    /// A terminator kind this relation table states no rule for. The
    /// counterpart of [`SirDiagnosticKind::InvalidOperation`]'s
    /// outside-the-table arm: a terminator nothing checks is refused, not
    /// admitted.
    InvalidTerminator {
        reason: String,
    },
    BranchConditionType {
        value: ValueId,
        actual: String,
    },
    ReturnType {
        expected: String,
        actual: Option<String>,
    },
    /// Unit-returning SIR functions use a zero-value `Return`; the initial
    /// value domain intentionally has no unit SSA carrier.
    UnitReturnValue {
        value: ValueId,
    },
    UndefinedValue(ValueId),
    NonDominatingUse {
        value: ValueId,
        definition: BlockId,
        use_block: BlockId,
    },
    UseBeforeDefinition {
        value: ValueId,
        block: BlockId,
    },
    /// A CFG rewrite made a formerly executable block unreachable even though
    /// discarding that region is not yet semantically safe.
    UnsafeCfgDiscard {
        block: BlockId,
        reason: CfgDiscardSafetyReason,
    },
}

/// The fail-closed reasons a CFG rewrite may not discard a reachable region.
///
/// The initial SIR domain is no-drop, but the ownership cases remain explicit
/// here so widening that domain cannot silently make an existing CFG rewrite
/// unsound. Each violation is a concrete verifier-ledger row rather than a
/// prose-only precondition on the optimizer.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CfgDiscardSafetyReason {
    /// An operation in the discarded region may transfer control to a
    /// language-visible trap.
    MayTrap { op: OpId },
    /// A discarded terminator can directly trap or select a checked-arithmetic
    /// failure edge.
    MayTrapTerminator,
    /// A block argument or operation result is not proven to be a no-drop
    /// value in the currently admitted SIR value domain.
    DropObligationValue { value: ValueId },
    /// A move or consume in the discarded region transfers or discharges an
    /// ownership obligation.
    DropObligationUse { site: UseSite },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SirDiagnostic {
    /// Display name of the function the finding is about, or `<module>` for
    /// a module-level finding.
    pub function: String,
    /// The callable the finding is about; the identity consumers join on.
    /// `None` for a module-level finding.
    pub callable: Option<CallableId>,
    pub kind: SirDiagnosticKind,
}

#[derive(Debug)]
pub(crate) struct CallableContext<'a> {
    by_id: BTreeMap<CallableId, &'a SemCallable>,
    closures: &'a [crate::SemClosure],
    actors: &'a [crate::SemActor],
}

/// Index an already-verified module's callable table.
///
/// [`verify_callable_table`] both validates and indexes; a pass that has
/// already run [`verify_module`] over the same callables needs only the index,
/// and building it here lets that pass hold the table while it mutates the
/// module's bodies.
pub(crate) fn callable_context<'a>(
    callables: &'a [SemCallable],
    closures: &'a [crate::SemClosure],
    actors: &'a [crate::SemActor],
) -> CallableContext<'a> {
    CallableContext {
        closures,
        actors,
        by_id: callables
            .iter()
            .map(|callable| (callable.id, callable))
            .collect(),
    }
}

impl<'a> CallableContext<'a> {
    fn callable(&self, id: CallableId) -> Option<&'a SemCallable> {
        self.by_id.get(&id).copied()
    }

    /// The ABI slot of one parameter of `id`, when the table names it.
    fn param_passing(&self, id: CallableId, index: usize) -> Option<SemParamPassing> {
        self.callable(id)
            .and_then(|callable| callable.signature.params.get(index))
            .map(|param| param.passing)
    }
}

fn verify_aggregate_shapes(module: &SemModule, diagnostics: &mut Vec<SirDiagnostic>) {
    let mut types = HashSet::new();
    let mut instances = HashSet::new();
    for (index, shape) in module.aggregate_shapes.iter().enumerate() {
        let expected =
            AggregateShapeId(u32::try_from(index).expect("SIR aggregate shape count exceeds u32"));
        let mut refuse = |reason| {
            diagnostics.push(module_diag(SirDiagnosticKind::InvalidAggregateShape {
                shape: shape.id,
                reason,
            }));
        };
        if shape.id != expected {
            refuse(format!(
                "non-canonical table position: expected {}, found {}",
                expected.0, shape.id.0
            ));
        }
        if shape.aggregate_ty.nominal_instance().as_ref() != Some(&shape.instance) {
            refuse(format!(
                "concrete type `{}` does not carry the descriptor's nominal instance",
                shape.aggregate_ty.user_facing()
            ));
        }
        if !types.insert(shape.aggregate_ty.clone()) {
            refuse(format!(
                "concrete type `{}` has more than one descriptor",
                shape.aggregate_ty.user_facing()
            ));
        }
        if !instances.insert(shape.instance.clone()) {
            refuse("nominal instance has more than one descriptor".to_string());
        }
        let mut names = HashSet::new();
        if shape.fields.iter().any(|field| !names.insert(&field.name)) {
            refuse("descriptor repeats a field name".to_string());
        }
        if crate::OwnKind::of_ty(&shape.aggregate_ty, &module.type_facts).is_err() {
            refuse(format!(
                "concrete type `{}` has no exact ownership facts",
                shape.aggregate_ty.user_facing()
            ));
        }
        if let Err(reason) = crate::aggregate_field_recipes(
            AggregateShapeRef::Record(shape.id),
            &shape.aggregate_ty,
            &module.aggregate_shapes,
            &module.type_facts,
        ) {
            refuse(reason);
        }
    }
}

fn verify_variant_shapes(module: &SemModule, diagnostics: &mut Vec<SirDiagnostic>) {
    let mut types = HashSet::new();
    for (index, shape) in module.variant_shapes.iter().enumerate() {
        let expected =
            VariantShapeId(u32::try_from(index).expect("SIR variant shape count exceeds u32"));
        let mut refuse = |reason| {
            diagnostics.push(module_diag(SirDiagnosticKind::InvalidVariantShape {
                shape: shape.id,
                reason,
            }));
        };
        if shape.id != expected {
            refuse(format!(
                "non-canonical table position: expected {}, found {}",
                expected.0, shape.id.0
            ));
        }
        if !matches!(shape.enum_ty, ResolvedTy::Named { .. }) {
            refuse(format!(
                "variant descriptor type `{}` is not an exact enum instance",
                shape.enum_ty.user_facing()
            ));
        }
        if !types.insert(shape.enum_ty.clone()) {
            refuse(format!(
                "concrete type `{}` has more than one variant descriptor",
                shape.enum_ty.user_facing()
            ));
        }
        let mut variant_names = HashSet::new();
        for variant in &shape.variants {
            if !variant_names.insert(&variant.name) {
                refuse("descriptor repeats a variant name".to_string());
            }
            let mut field_names = HashSet::new();
            if variant
                .fields
                .iter()
                .any(|field| !field_names.insert(&field.name))
            {
                refuse(format!("variant `{}` repeats a field name", variant.name));
            }
        }
        if crate::OwnKind::of_ty(&shape.enum_ty, &module.type_facts).is_err() {
            refuse(format!(
                "concrete type `{}` has no exact ownership facts",
                shape.enum_ty.user_facing()
            ));
        }
        for variant in 0..shape.variants.len() {
            let Ok(variant) = u32::try_from(variant) else {
                refuse("variant count exceeds the module-local ID range".to_string());
                break;
            };
            if let Err(reason) = crate::variant_field_recipes(
                shape.id,
                variant,
                &shape.enum_ty,
                &module.variant_shapes,
                &module.type_facts,
            ) {
                refuse(reason);
            }
        }
    }
}

/// Per-function place analysis retained by successful semantic verification.
/// Its plan and cleanup dispositions come from the same checked body.
#[derive(Debug)]
pub struct CheckedFunction {
    places: crate::PlacePlan,
    lifetimes: crate::PlaceLifetimes,
}

impl CheckedFunction {
    #[must_use]
    pub fn place_plan(&self) -> &crate::PlacePlan {
        &self.places
    }

    #[must_use]
    pub fn place_lifetimes(&self) -> &crate::PlaceLifetimes {
        &self.lifetimes
    }
}

/// A module accepted by every semantic context and function check.
/// The immutable input borrow keeps its analyses tied to the checked revision.
#[derive(Debug)]
pub struct CheckedModule<'a> {
    module: &'a SemModule,
    functions: BTreeMap<CallableId, CheckedFunction>,
}

impl<'a> CheckedModule<'a> {
    #[must_use]
    pub fn module(&self) -> &'a SemModule {
        self.module
    }

    /// Analysis for a concrete body, identified by its semantic callable.
    #[must_use]
    pub fn function(&self, callable: CallableId) -> Option<&CheckedFunction> {
        self.functions.get(&callable)
    }
}

#[must_use]
pub fn verify_module(module: &SemModule) -> Vec<SirDiagnostic> {
    check_module(module).err().unwrap_or_default()
}

fn verify_resources(module: &SemModule, diagnostics: &mut Vec<SirDiagnostic>) {
    for key in module.type_facts.keys().filter(|key| {
        matches!(key.0, ResolvedTy::Task(_))
            || crate::generator_parts(&key.0).is_some()
            || key.0.is_builtin(hew_types::BuiltinType::Stream)
            || key.0.is_builtin(hew_types::BuiltinType::Sink)
            || (hew_types::runtime_call::FileReadHandleKind::of_ty(&key.0).is_some()
                || hew_types::runtime_call::IoHandleKind::of_ty(&key.0).is_some())
    }) {
        if !module.resources.contains_key(&key.0) {
            diagnostics.push(module_diag(SirDiagnosticKind::InvalidResourceType {
                ty: key.0.clone(),
                reason: "resource type has no checked release contract".into(),
            }));
        }
    }
    for (ty, release) in &module.resources {
        let result = module
            .type_facts
            .get(&hew_types::TypeInstanceKey(ty.clone()))
            .ok_or_else(|| "resource release has no exact type facts".to_string())
            .and_then(|facts| crate::verify_resource_release(ty, release, facts));
        if let Err(reason) = result {
            diagnostics.push(module_diag(SirDiagnosticKind::InvalidResourceType {
                ty: ty.clone(),
                reason,
            }));
        }
    }
}

/// Verify all module contracts and every body, retaining their checked places
/// and lifetime dispositions for the next compiler stage. Module context is
/// checked once; each body's place plan and lifetime flow are computed once.
///
/// # Errors
/// Returns all diagnostics from the same checks used by [`verify_module`].
pub fn check_module(module: &SemModule) -> Result<CheckedModule<'_>, Vec<SirDiagnostic>> {
    let mut functions = BTreeMap::new();
    let mut diagnostics = Vec::new();
    let callables = verify_callable_table(module, &mut diagnostics);
    verify_aggregate_shapes(module, &mut diagnostics);
    verify_variant_shapes(module, &mut diagnostics);
    verify_resources(module, &mut diagnostics);

    for ((ty, capability), plan) in &module.value_capabilities {
        if let Err(reason) =
            crate::capability::verify_value_capability(module, ty, *capability, plan)
        {
            diagnostics.push(module_diag(SirDiagnosticKind::InvalidValueCapability {
                ty: ty.clone(),
                capability: *capability,
                reason,
            }));
        }
    }
    for key in module.type_facts.keys() {
        if hew_types::runtime_call::collection_type_arguments(&key.0).is_some() {
            if let Err(reason) = crate::model::collection_value_dependencies(
                &key.0,
                &module.type_facts,
                &module.aggregate_shapes,
                &module.variant_shapes,
                &module.resources,
            ) {
                diagnostics.push(module_diag(SirDiagnosticKind::InvalidCollectionType {
                    ty: key.0.clone(),
                    reason,
                }));
            }
        }
    }
    let mut names = HashSet::new();
    let mut declarations = HashSet::new();
    for function in &module.functions {
        verify_required_value_capabilities(module, function, &mut diagnostics);
        if let Err(reason) = crate::defer::verify_calls(module, function) {
            diagnostics.push(diag(
                function,
                SirDiagnosticKind::InvalidTerminator {
                    reason: reason.into(),
                },
            ));
        }
        if !names.insert(function.name.clone()) {
            diagnostics.push(diag(
                function,
                SirDiagnosticKind::DuplicateFunctionName(function.name.clone()),
            ));
        }
        let monomorphic_body = callables
            .callable(function.callable)
            .is_none_or(|callable| matches!(callable.instance, CallableInstance::Monomorphic));
        if monomorphic_body && !declarations.insert(function.declaration.clone()) {
            diagnostics.push(diag(
                function,
                SirDiagnosticKind::DuplicateFunctionDeclaration(format!(
                    "{:?}",
                    function.declaration
                )),
            ));
        }
        for operation in function.blocks.iter().flat_map(|block| &block.ops) {
            let missing = match operation.kind {
                SemOpKind::ConstStr(id) => !module.string_literals.contains_key(&id),
                SemOpKind::ConstBytes(id) => !module.bytes_literals.contains_key(&id),
                _ => false,
            };
            if missing {
                diagnostics.push(diag(
                    function,
                    SirDiagnosticKind::InvalidOperation {
                        op: operation.id,
                        reason: "literal operation references a missing module pool entry"
                            .to_string(),
                    },
                ));
            }
        }
        let (function_diagnostics, analysis) = check_function_with_context(
            function,
            Some(&callables),
            &module.type_facts,
            &module.aggregate_shapes,
            &module.variant_shapes,
        );
        diagnostics.extend(function_diagnostics);
        if let Some(analysis) = analysis {
            functions.insert(function.callable, analysis);
        }
    }
    if diagnostics.is_empty() {
        Ok(CheckedModule { module, functions })
    } else {
        Err(diagnostics)
    }
}

fn verify_required_value_capabilities(
    module: &SemModule,
    function: &SemFunction,
    diagnostics: &mut Vec<SirDiagnostic>,
) {
    use hew_types::runtime_call::{MapValueOp, SetValueOp};
    use hew_types::{RuntimeCallFamily, ValueCapability};

    for block in &function.blocks {
        if let SemTerminator::ValueCall { ty, capability, .. } = &block.terminator {
            let checked = module
                .value_capabilities
                .get(&(ty.clone(), *capability))
                .ok_or_else(|| "value call requires its exact selected method".to_string())
                .and_then(|plan| {
                    crate::capability::verify_value_capability(module, ty, *capability, plan)
                });
            if let Err(reason) = checked {
                diagnostics.push(diag(
                    function,
                    SirDiagnosticKind::InvalidValueCapability {
                        ty: ty.clone(),
                        capability: *capability,
                        reason,
                    },
                ));
            }
        }
        let SemTerminator::RtCall {
            family:
                RuntimeCallFamily::Map(MapValueOp::New) | RuntimeCallFamily::Set(SetValueOp::New),
            result: crate::CallResult::Value(result),
            ..
        } = &block.terminator
        else {
            continue;
        };
        let Some((_, arguments)) = hew_types::runtime_call::collection_type_arguments(&result.ty)
        else {
            continue;
        };
        let Some(key) = arguments.first() else {
            continue;
        };
        for capability in [ValueCapability::Hash, ValueCapability::Eq] {
            if !module
                .value_capabilities
                .contains_key(&(key.clone(), capability))
            {
                diagnostics.push(diag(
                    function,
                    SirDiagnosticKind::InvalidValueCapability {
                        ty: key.clone(),
                        capability,
                        reason: "collection construction requires a selected key method".into(),
                    },
                ));
            }
        }
    }
}

/// Verify one function against the resolved callable table in `module`.
///
/// Use this at an inter-IR boundary that can receive direct calls.  The
/// context-free [`verify_function`] remains useful for local CFG construction,
/// but cannot prove a `CallableId`'s signature or ABI facts in isolation.
#[must_use]
pub fn verify_function_in_module(module: &SemModule, function: &SemFunction) -> Vec<SirDiagnostic> {
    let mut diagnostics = Vec::new();
    let callables = verify_callable_table(module, &mut diagnostics);
    verify_required_value_capabilities(module, function, &mut diagnostics);
    if let Err(reason) = crate::defer::verify_calls(module, function) {
        diagnostics.push(diag(
            function,
            SirDiagnosticKind::InvalidTerminator {
                reason: reason.into(),
            },
        ));
    }
    diagnostics.extend(verify_function_with_context(
        function,
        Some(&callables),
        &module.type_facts,
        &module.aggregate_shapes,
        &module.variant_shapes,
    ));
    diagnostics
}

/// Check one function and return cleanup dispositions from its lifetime flow.
/// No producer flag or physical fault-carrier inference may replace this query.
/// This focused query does not validate the entire module context. Compiler
/// stage boundaries should use [`check_module`] to retain both the checked
/// place plan and lifetime result after all module-level checks.
///
/// # Errors
/// Returns the same semantic diagnostics as function verification, including
/// invalid callable contracts, storage activity, loans and linear cleanup.
///
/// # Panics
/// Panics only if an internal verifier inconsistency accepts a function without
/// producing its checked lifetime result.
pub fn place_lifetimes(
    module: &SemModule,
    function: &SemFunction,
) -> Result<crate::PlaceLifetimes, Vec<SirDiagnostic>> {
    let mut diagnostics = Vec::new();
    let callables = verify_callable_table(module, &mut diagnostics);
    verify_required_value_capabilities(module, function, &mut diagnostics);
    if let Err(reason) = crate::defer::verify_calls(module, function) {
        diagnostics.push(diag(
            function,
            SirDiagnosticKind::InvalidTerminator {
                reason: reason.into(),
            },
        ));
    }
    let (function_diagnostics, lifetimes) = check_function_with_context(
        function,
        Some(&callables),
        &module.type_facts,
        &module.aggregate_shapes,
        &module.variant_shapes,
    );
    diagnostics.extend(function_diagnostics);
    if diagnostics.is_empty() {
        Ok(lifetimes
            .expect("valid function has checked place lifetimes")
            .lifetimes)
    } else {
        Err(diagnostics)
    }
}

/// Verify one semantic SSA function before it crosses into another SIR pass
/// or the ownership/layout MIR boundary.
///
/// Keeping this public lets every consumer fail closed rather than relying on
/// a particular CLI lane to have run whole-module verification first. Without a
/// module it carries neither the callable table nor the §6.2 fact table, so it
/// refuses a parameter, whose §1.2 kind is its ABI slot, and a value whose type
/// §1.1 cannot class without declaration facts. Use
/// [`verify_function_in_module`] wherever those facts exist.
#[allow(
    clippy::too_many_lines,
    reason = "the verifier keeps SSA collection, CFG shape, and dominance checks together so the stage boundary is auditable"
)]
#[must_use]
pub fn verify_function(function: &SemFunction) -> Vec<SirDiagnostic> {
    verify_function_with_facts(function, &TypeFactTable::new())
}

/// Verify one function against the §6.2 fact table its module carries.
///
/// A function verified away from its module has no fact table to read, so a
/// value whose class needs declaration facts is refused rather than admitted.
/// A pass that holds the module passes its table here so the kind it audits is
/// the one the lowering wrote.
#[must_use]
pub(crate) fn verify_function_with_facts(
    function: &SemFunction,
    facts: &TypeFactTable,
) -> Vec<SirDiagnostic> {
    verify_function_with_context(function, None, facts, &[], &[])
}

/// Verify the semantic precondition for discarding blocks during a CFG rewrite.
///
/// `rewritten` is the post-edge-rewrite, pre-compaction candidate, so both
/// functions still use the same block identities. Only blocks reachable in
/// `original` and newly unreachable in `rewritten` are examined. The rewrite
/// fails closed if such a block contains a potentially trapping operation, a
/// value outside the proven no-drop domain, or an ownership transfer/discharge.
#[must_use]
pub(crate) fn verify_cfg_discard_safety(
    original: &SemFunction,
    rewritten: &SemFunction,
) -> Vec<SirDiagnostic> {
    let original_cfg = crate::build_cfg_index(original);
    let rewritten_cfg = crate::build_cfg_index(rewritten);
    let discarded = original_cfg
        .reachable()
        .difference(rewritten_cfg.reachable())
        .copied()
        .collect::<BTreeSet<_>>();
    let mut diagnostics = Vec::new();

    for block in &original.blocks {
        if !discarded.contains(&block.id) {
            continue;
        }
        for argument in &block.args {
            if !is_initial_value_type(&argument.ty) {
                diagnostics.push(cfg_discard_diag(
                    original,
                    block.id,
                    CfgDiscardSafetyReason::DropObligationValue {
                        value: argument.value,
                    },
                ));
            }
        }
        for operation in &block.ops {
            if operation.kind.effects().may_trap()
                || matches!(operation.kind, SemOpKind::RegisterDefer { .. })
            {
                diagnostics.push(cfg_discard_diag(
                    original,
                    block.id,
                    CfgDiscardSafetyReason::MayTrap { op: operation.id },
                ));
            }
            for result in &operation.results {
                if !is_initial_value_type(&result.ty) {
                    diagnostics.push(cfg_discard_diag(
                        original,
                        block.id,
                        CfgDiscardSafetyReason::DropObligationValue { value: result.id },
                    ));
                }
            }
            // An operand carries no mode: what a use does to its value is
            // the op it feeds, so the obligation question is asked of the
            // operation kind. A terminator in the admitted domain transfers no
            // obligation of its own - a `Suspend`'s `Move` inputs do, and they
            // arrive with the phase that emits one.
            if operation.kind.transfers_obligation() {
                operation.kind.visit_operands(|operand, use_| {
                    diagnostics.push(cfg_discard_diag(
                        original,
                        block.id,
                        CfgDiscardSafetyReason::DropObligationUse {
                            site: UseSite::Operation {
                                op: operation.id,
                                operand,
                                value: use_.value,
                            },
                        },
                    ));
                });
            }
        }
        if matches!(
            block.terminator,
            SemTerminator::EnterDefer { .. }
                | SemTerminator::FinishDefer { .. }
                | SemTerminator::CleanupDispatch { .. }
                | SemTerminator::RecoverFault { .. }
                | SemTerminator::CheckedRaiseFault { .. }
                | SemTerminator::CheckedBinary { .. }
                | SemTerminator::SwitchVariant { .. }
                | SemTerminator::Trap { .. }
                | SemTerminator::Panic { .. }
        ) {
            diagnostics.push(cfg_discard_diag(
                original,
                block.id,
                CfgDiscardSafetyReason::MayTrapTerminator,
            ));
        }
    }

    diagnostics
}

fn cfg_discard_diag(
    function: &SemFunction,
    block: BlockId,
    reason: CfgDiscardSafetyReason,
) -> SirDiagnostic {
    diag(
        function,
        SirDiagnosticKind::UnsafeCfgDiscard { block, reason },
    )
}

pub(crate) fn verify_function_with_context(
    function: &SemFunction,
    callable_context: Option<&CallableContext<'_>>,
    facts: &TypeFactTable,
    aggregate_shapes: &[SemAggregateShape],
    variant_shapes: &[SemVariantShape],
) -> Vec<SirDiagnostic> {
    check_function_with_context(
        function,
        callable_context,
        facts,
        aggregate_shapes,
        variant_shapes,
    )
    .0
}

#[allow(
    clippy::too_many_lines,
    reason = "the verifier keeps SSA collection, CFG shape, and dominance checks together so the stage boundary is auditable"
)]
fn check_function_with_context(
    function: &SemFunction,
    callable_context: Option<&CallableContext<'_>>,
    facts: &TypeFactTable,
    aggregate_shapes: &[SemAggregateShape],
    variant_shapes: &[SemVariantShape],
) -> (Vec<SirDiagnostic>, Option<CheckedFunction>) {
    let mut diagnostics = Vec::new();
    if let Err(reason) = crate::defer::plan(function)
        .map_err(str::to_string)
        .and_then(|_| crate::task_scope::verify(function))
    {
        diagnostics.push(diag(
            function,
            SirDiagnosticKind::InvalidTerminator { reason },
        ));
    }
    verify_function_callable_identity(function, callable_context, &mut diagnostics);
    if let Err(reason) = verify_capture_places(function, callable_context)
        .and_then(|()| crate::actor::verify_places(function, callable_context.map(|c| c.actors)))
    {
        diagnostics.push(diag(
            function,
            SirDiagnosticKind::InvalidCallable {
                callable: function.callable,
                reason,
            },
        ));
    }
    let mut blocks = BTreeMap::new();
    for (index, block) in function.blocks.iter().enumerate() {
        let expected = BlockId(
            u32::try_from(index).expect("SIR block count exceeds the module-local ID range"),
        );
        if block.id != expected {
            diagnostics.push(diag(
                function,
                SirDiagnosticKind::NonCanonicalBlockOrder {
                    expected,
                    actual: block.id,
                },
            ));
        }
        if blocks.insert(block.id, block).is_some() {
            diagnostics.push(diag(function, SirDiagnosticKind::DuplicateBlock(block.id)));
        }
    }
    if !blocks.contains_key(&function.entry) {
        diagnostics.push(diag(
            function,
            SirDiagnosticKind::MissingEntry(function.entry),
        ));
    } else if let Some(entry) = blocks.get(&function.entry) {
        if !entry.args.is_empty() {
            diagnostics.push(diag(
                function,
                SirDiagnosticKind::EntryBlockArgs {
                    entry: function.entry,
                    actual: entry.args.len(),
                },
            ));
        }
    }
    let mut values = HashSet::new();
    let mut types = HashMap::new();
    let mut definitions = HashMap::new();
    let mut operations = HashSet::new();
    for (index, param) in function.params.iter().enumerate() {
        record_value(function, param.value, &mut values, &mut diagnostics);
        // §1.2 rule 3: a parameter's kind is its header slot before it is its
        // type's class, so the audit reads the slot the lowering read. Without
        // the callable table there is no slot to read, and the rule has no
        // authority to audit against: that is a finding, not a `ReadOnly`
        // default, which would admit a `Guaranteed` borrow slot as `Owned` and
        // refuse the borrow slot the lowering actually wrote.
        let expected = match callable_context
            .and_then(|context| context.param_passing(function.callable, index))
        {
            Some(passing) => crate::OwnKind::of_param(&param.ty, passing, facts),
            None => Err(format!(
                "parameter {index} has no header slot in the callable table, so §1.2 rule 3 has no ABI fact to audit its ownership kind against"
            )),
        };
        verify_own_kind(
            function,
            param.value,
            &param.ty,
            param.own,
            expected,
            &mut diagnostics,
        );
        types.insert(param.value, param.ty.clone());
        definitions.insert(param.value, (function.entry, DefinitionPoint::BlockEntry));
    }
    for block in &function.blocks {
        for arg in &block.args {
            record_value(function, arg.value, &mut values, &mut diagnostics);
            verify_own_kind(
                function,
                arg.value,
                &arg.ty,
                arg.own,
                crate::OwnKind::of_ty(&arg.ty, facts),
                &mut diagnostics,
            );
            types.insert(arg.value, arg.ty.clone());
            definitions.insert(arg.value, (block.id, DefinitionPoint::BlockEntry));
        }
        for (op_index, op) in block.ops.iter().enumerate() {
            if !operations.insert(op.id) {
                diagnostics.push(diag(function, SirDiagnosticKind::DuplicateOp(op.id)));
            }
            for result in &op.results {
                record_value(function, result.id, &mut values, &mut diagnostics);
                verify_own_kind(
                    function,
                    result.id,
                    &result.ty,
                    result.own,
                    crate::OwnKind::of_ty(&result.ty, facts).map(|own| {
                        if op.kind.borrow_parent().is_some() {
                            crate::OwnKind::Guaranteed
                        } else {
                            own
                        }
                    }),
                    &mut diagnostics,
                );
                types.insert(result.id, result.ty.clone());
                definitions.insert(result.id, (block.id, DefinitionPoint::Operation(op_index)));
            }
        }
        if let SemTerminator::Call { id, .. }
        | SemTerminator::RtCall { id, .. }
        | SemTerminator::ActorCall { id, .. }
        | SemTerminator::ValueCall { id, .. }
        | SemTerminator::IndirectCall { id, .. }
        | SemTerminator::CheckedBinary { id, .. }
        | SemTerminator::SwitchVariant { id, .. } = &block.terminator
        {
            if !operations.insert(*id) {
                diagnostics.push(diag(function, SirDiagnosticKind::DuplicateOp(*id)));
            }
        }
        block.terminator.visit_results(|result| {
            record_value(function, result.id, &mut values, &mut diagnostics);
            verify_own_kind(
                function,
                result.id,
                &result.ty,
                result.own,
                crate::OwnKind::of_ty(&result.ty, facts),
                &mut diagnostics,
            );
            types.insert(result.id, result.ty.clone());
            definitions.insert(result.id, (block.id, DefinitionPoint::NormalEdge));
        });
    }
    // §1.6's binding table is read by every user-facing wall, so a row naming
    // a target this body never defines is refused rather than silently dropped
    // when the wall goes looking for the user's name.
    for binding in &function.bindings {
        let known = match binding.target {
            BindingTarget::Value(value) => values.contains(&value),
            BindingTarget::Place(place) => function.places.iter().any(|decl| decl.id == place),
        };
        if !known {
            diagnostics.push(diag(
                function,
                SirDiagnosticKind::UnknownBinding {
                    name: binding.name.clone(),
                    target: binding.target,
                },
            ));
        }
    }
    let projections = crate::place_plan(function, aggregate_shapes, facts);
    if let Err(reason) = &projections {
        diagnostics.push(diag(
            function,
            SirDiagnosticKind::InvalidCallable {
                callable: function.callable,
                reason: reason.clone(),
            },
        ));
    }
    // Every value type is known before checking operations, edges, and
    // terminators. In particular this catches a malformed use whose value is
    // defined in a later block rather than silently skipping its type check.
    let variants = VariantVerifyContext {
        facts,
        aggregate_shapes,
        shapes: variant_shapes,
    };
    for block in &function.blocks {
        for op in &block.ops {
            verify_callable_operation(
                function,
                op,
                &types,
                facts,
                callable_context,
                &mut diagnostics,
            );
            if let Some(result) = crate::projection::verify_operation(function, op, &types, facts)
                .or_else(|| {
                    crate::actor::verify_operation(
                        function,
                        op,
                        &types,
                        facts,
                        callable_context.map(|c| c.actors),
                    )
                })
                .or_else(|| verify_capture_operation(function, op, &types, facts, callable_context))
            {
                if let Err(reason) = result {
                    invalid_operation(function, op.id, reason, &mut diagnostics);
                }
                continue;
            }
            verify_operation_shape(
                function,
                op,
                &types,
                facts,
                aggregate_shapes,
                variant_shapes,
                &mut diagnostics,
            );
        }
        block.terminator.visit_successors(|edge| {
            let Some(target) = blocks.get(&edge.target) else {
                diagnostics.push(diag(function, SirDiagnosticKind::UnknownBlock(edge.target)));
                return;
            };
            if target.args.len() != edge.args.len() {
                diagnostics.push(diag(
                    function,
                    SirDiagnosticKind::EdgeArity {
                        from: block.id,
                        to: edge.target,
                        expected: target.args.len(),
                        actual: edge.args.len(),
                    },
                ));
            }
            for (argument, (value, target_arg)) in edge.args.iter().zip(&target.args).enumerate() {
                let Some(actual) = types.get(&value.value) else {
                    continue;
                };
                if actual != &target_arg.ty {
                    diagnostics.push(diag(
                        function,
                        SirDiagnosticKind::EdgeType {
                            from: block.id,
                            to: edge.target,
                            argument,
                            expected: target_arg.ty.user_facing().to_string(),
                            actual: actual.user_facing().to_string(),
                        },
                    ));
                }
            }
        });
        verify_terminator_shape(
            function,
            block,
            &types,
            &blocks,
            callable_context,
            &variants,
            &mut diagnostics,
        );
    }
    if blocks.contains_key(&function.entry) {
        let dominators = crate::compute_dominators(function);
        for block in &function.blocks {
            for (op_index, op) in block.ops.iter().enumerate() {
                verify_uses(
                    function,
                    &dominators,
                    &definitions,
                    block.id,
                    Some(op_index),
                    uses_in_op(function, op),
                    &mut diagnostics,
                );
            }
            verify_uses(
                function,
                &dominators,
                &definitions,
                block.id,
                None,
                uses_in_terminator(&block.terminator),
                &mut diagnostics,
            );
        }
    }
    let mut lifetimes = None;
    if let Ok(projections) = projections {
        let analysis = crate::lifetime::verify(function, &projections, facts);
        diagnostics.extend(analysis.violations.into_iter().map(|violation| {
            diag(
                function,
                match (violation.value, violation.place) {
                    (_, Some(place)) => SirDiagnosticKind::PlaceLifetime {
                        block: violation.block,
                        place,
                        reason: violation.reason,
                    },
                    (Some(value), None) => SirDiagnosticKind::OwnershipLifetime {
                        block: violation.block,
                        value,
                        reason: violation.reason,
                    },
                    (None, None) => SirDiagnosticKind::FaultLifetime {
                        block: violation.block,
                        reason: violation.reason,
                    },
                },
            )
        }));
        lifetimes = Some(CheckedFunction {
            places: projections,
            lifetimes: analysis.lifetimes,
        });
    }
    (diagnostics, lifetimes)
}

#[allow(
    clippy::too_many_lines,
    reason = "callable identity, signature, and root-entry invariants share one auditable module boundary"
)]
fn verify_callable_table<'a>(
    module: &'a SemModule,
    diagnostics: &mut Vec<SirDiagnostic>,
) -> CallableContext<'a> {
    let mut actor_declarations = HashSet::new();
    for actor in &module.actors {
        let checked = actor.validate(module).and_then(|()| {
            if actor_declarations.insert(&actor.declaration) {
                Ok(())
            } else {
                Err("actor declaration is repeated".into())
            }
        });
        if let Err(reason) = checked {
            diagnostics.push(module_diag(SirDiagnosticKind::InvalidTerminator { reason }));
        }
    }
    let mut instances = HashSet::new();
    for closure in &module.closures {
        let result = closure.validate(module).and_then(|()| {
            if instances.insert(closure.instance) {
                Ok(())
            } else {
                Err("closure literal is repeated within its enclosing instance".to_string())
            }
        });
        if let Err(reason) = result {
            diagnostics.push(module_diag(SirDiagnosticKind::InvalidCallable {
                callable: closure.body,
                reason,
            }));
        }
    }
    let generic_templates = verify_generic_template_headers(module, diagnostics);
    let mut by_id = BTreeMap::new();
    let mut ids = HashSet::new();
    let mut monomorphic_declarations = HashSet::new();
    let mut generic_declarations = HashSet::new();
    let mut generic_instances = HashSet::new();
    let mut symbols = HashSet::new();
    for (index, callable) in module.callables.iter().enumerate() {
        let expected = CallableId(
            u32::try_from(index).expect("SIR callable count exceeds the module-local ID range"),
        );
        if callable.id != expected {
            diagnostics.push(module_diag(SirDiagnosticKind::InvalidCallable {
                callable: callable.id,
                reason: format!(
                    "table position {index} requires id {:?}, found {:?}",
                    expected, callable.id
                ),
            }));
        }
        if !ids.insert(callable.id) {
            diagnostics.push(module_diag(SirDiagnosticKind::DuplicateCallableId(
                callable.id,
            )));
        }
        match &callable.instance {
            CallableInstance::Closure(id) => {
                if module
                    .closure(*id)
                    .is_none_or(|closure| closure.body != callable.id)
                {
                    diagnostics.push(module_diag(SirDiagnosticKind::InvalidCallable {
                        callable: callable.id,
                        reason: "closure body has no matching canonical environment descriptor"
                            .to_string(),
                    }));
                }
            }
            CallableInstance::Monomorphic => {
                if !monomorphic_declarations.insert(callable.declaration.clone()) {
                    diagnostics.push(module_diag(
                        SirDiagnosticKind::DuplicateCallableDeclaration(
                            callable.declaration.full_path().to_string(),
                        ),
                    ));
                }
                if generic_declarations.contains(&callable.declaration) {
                    diagnostics.push(module_diag(SirDiagnosticKind::InvalidCallable {
                        callable: callable.id,
                        reason: "a declaration cannot have both a monomorphic SIR body and concrete generic SIR instances"
                            .to_string(),
                    }));
                }
                if generic_templates.contains_key(&GenericTemplateId {
                    declaration: callable.declaration.clone(),
                }) {
                    diagnostics.push(module_diag(SirDiagnosticKind::InvalidCallable {
                        callable: callable.id,
                        reason: "a declaration with a generic semantic template header cannot also be a monomorphic SIR body"
                            .to_string(),
                    }));
                }
            }
            CallableInstance::EntryAdapter => {
                if module.entry_callable != Some(callable.id) {
                    diagnostics.push(module_diag(SirDiagnosticKind::InvalidCallable {
                        callable: callable.id,
                        reason: "entry adapter is not the module's entry callable".to_string(),
                    }));
                }
            }
            CallableInstance::Generic(key) => {
                generic_declarations.insert(callable.declaration.clone());
                if monomorphic_declarations.contains(&callable.declaration) {
                    diagnostics.push(module_diag(SirDiagnosticKind::InvalidCallable {
                        callable: callable.id,
                        reason: "a declaration cannot have both a monomorphic SIR body and concrete generic SIR instances"
                            .to_string(),
                    }));
                }
                verify_generic_callable_instance(
                    module,
                    callable,
                    key,
                    &mut generic_instances,
                    &generic_templates,
                    diagnostics,
                );
            }
        }
        if !symbols.insert(callable.symbol.clone()) {
            diagnostics.push(module_diag(SirDiagnosticKind::DuplicateCallableSymbol(
                callable.symbol.clone(),
            )));
        }
        if callable.call_conv != SemCallConv::Default {
            diagnostics.push(module_diag(SirDiagnosticKind::InvalidCallable {
                callable: callable.id,
                reason: "initial SIR direct-call domain requires Default call convention"
                    .to_string(),
            }));
        }
        let expected_kind = if matches!(callable.instance, CallableInstance::Closure(_)) {
            SemCallableKind::HewClosure
        } else if let SemCallableKind::HewActor(id) = callable.kind {
            match module.actor(id) {
                Some(actor) if actor.bodies().any(|body| body == callable.id) => {
                    SemCallableKind::HewActor(id)
                }
                _ => SemCallableKind::HewDirect,
            }
        } else {
            SemCallableKind::HewDirect
        };
        if callable.kind != expected_kind {
            diagnostics.push(module_diag(SirDiagnosticKind::InvalidCallable {
                callable: callable.id,
                reason: "callable kind differs from its source instance contract".to_string(),
            }));
        }
        for (parameter, abi) in callable.signature.params.iter().enumerate() {
            if !is_supported_call_value(module, &abi.ty) {
                diagnostics.push(module_diag(SirDiagnosticKind::InvalidCallable {
                    callable: callable.id,
                    reason: format!(
                        "parameter {parameter} has type `{}` outside the owned-call SIR surface",
                        abi.ty.user_facing()
                    ),
                }));
            }
            if parameter == 0
                && (matches!(callable.instance, CallableInstance::Closure(_))
                    || matches!(callable.kind, SemCallableKind::HewActor(_)))
            {
                // The closure descriptor validates its exact receiver type,
                // access permission and ownership together.
                continue;
            }
            let expected_passing = match crate::OwnKind::of_ty(&abi.ty, &module.type_facts) {
                Ok(crate::OwnKind::Owned) if abi.passing == SemParamPassing::Consume => {
                    SemParamPassing::Consume
                }
                Ok(crate::OwnKind::Owned) => SemParamPassing::Borrow,
                Ok(crate::OwnKind::None) => SemParamPassing::ReadOnly,
                Ok(crate::OwnKind::Guaranteed) => {
                    diagnostics.push(module_diag(SirDiagnosticKind::InvalidCallable {
                        callable: callable.id,
                        reason: format!(
                            "parameter {parameter} concrete type facts produced the borrow-only Guaranteed kind"
                        ),
                    }));
                    continue;
                }
                Err(reason) => {
                    diagnostics.push(module_diag(SirDiagnosticKind::InvalidCallable {
                        callable: callable.id,
                        reason: format!(
                            "parameter {parameter} has no exact ownership facts: {reason}"
                        ),
                    }));
                    continue;
                }
            };
            if abi.passing != expected_passing {
                diagnostics.push(module_diag(SirDiagnosticKind::InvalidCallable {
                    callable: callable.id,
                    reason: format!(
                        "parameter {parameter} has {:?} passing, expected {expected_passing:?}",
                        abi.passing
                    ),
                }));
            }
            if abi.caller_visible_projection {
                diagnostics.push(module_diag(SirDiagnosticKind::InvalidCallable {
                    callable: callable.id,
                    reason: format!(
                        "parameter {parameter} has a caller-visible projection before SIR owns that ABI feature"
                    ),
                }));
            }
        }
        if !is_supported_call_return(module, &callable.signature.return_ty) {
            diagnostics.push(module_diag(SirDiagnosticKind::InvalidCallable {
                callable: callable.id,
                reason: format!(
                    "return type `{}` is outside the initial scalar SIR callable domain",
                    callable.signature.return_ty.user_facing()
                ),
            }));
        }
        by_id.entry(callable.id).or_insert(callable);
    }

    let mut previous_root = None;
    for root in &module.root_unit_callables {
        if previous_root.is_some_and(|previous| previous >= *root) {
            diagnostics.push(module_diag(SirDiagnosticKind::InvalidRootCallable {
                callable: *root,
                reason: "root-unit callable IDs must be unique and table-ordered".to_string(),
            }));
        }
        previous_root = Some(*root);
        match by_id.get(root) {
            None => diagnostics.push(module_diag(SirDiagnosticKind::InvalidRootCallable {
                callable: *root,
                reason: "root-unit callable does not exist in the table".to_string(),
            })),
            Some(callable) if callable.source_origin != crate::FunctionSourceOrigin::RootUnit => {
                diagnostics.push(module_diag(SirDiagnosticKind::InvalidRootCallable {
                    callable: *root,
                    reason: "root-unit callable has non-root source provenance".to_string(),
                }));
            }
            Some(_) => {}
        }
    }
    if let Some(entry) = module.entry_callable {
        match by_id.get(&entry) {
            None => diagnostics.push(module_diag(SirDiagnosticKind::InvalidEntryCallable {
                callable: entry,
                reason: "entry callable does not exist in the table".to_string(),
            })),
            Some(callable)
                if callable.source_origin != crate::FunctionSourceOrigin::RootUnit
                    || !module.root_unit_callables.contains(&entry) =>
            {
                diagnostics.push(module_diag(SirDiagnosticKind::InvalidEntryCallable {
                    callable: entry,
                    reason: "entry callable must be a listed root-unit callable".to_string(),
                }));
            }
            Some(callable)
                if !matches!(
                    callable.instance,
                    CallableInstance::Monomorphic | CallableInstance::EntryAdapter
                ) =>
            {
                diagnostics.push(module_diag(SirDiagnosticKind::InvalidEntryCallable {
                    callable: entry,
                    reason:
                        "entry callable must be a monomorphic source body, not a generic instance"
                            .to_string(),
                }));
            }
            Some(_)
                if module.entry_exit_plan.as_ref().is_some_and(|plan| {
                    matches!(plan.action, hew_types::EntryExitAction::Result { .. })
                }) =>
            {
                diagnostics.push(module_diag(SirDiagnosticKind::InvalidEntryCallable {
                    callable: entry,
                    reason: "Result entry actions are realized by the SIR entry adapter before verification"
                        .to_string(),
                }));
            }
            Some(callable) if !callable.signature.params.is_empty() => {
                diagnostics.push(module_diag(SirDiagnosticKind::InvalidEntryCallable {
                    callable: entry,
                    reason: "entry callable must be parameterless for the native and WASI entry adapters"
                        .to_string(),
                }));
            }
            Some(callable)
                if callable.signature.return_ty != ResolvedTy::Unit
                    && !callable.signature.return_ty.is_integer() =>
            {
                diagnostics.push(module_diag(SirDiagnosticKind::InvalidEntryCallable {
                    callable: entry,
                    reason: "entry callable must return unit or an integer exit status".to_string(),
                }));
            }
            Some(_) => {}
        }
    }
    CallableContext {
        by_id,
        closures: &module.closures,
        actors: &module.actors,
    }
}

/// Collect and verify body-free semantic template headers before checking
/// concrete generic callable bodies.
fn verify_generic_template_headers<'a>(
    module: &'a SemModule,
    diagnostics: &mut Vec<SirDiagnostic>,
) -> BTreeMap<GenericTemplateId, &'a SemGenericTemplate> {
    let mut templates = BTreeMap::new();
    for template in &module.generic_templates {
        let name = template.id.declaration.full_path().to_string();
        if template.type_params.is_empty() {
            diagnostics.push(module_diag(SirDiagnosticKind::InvalidGenericTemplate {
                template: name.clone(),
                reason: "a generic template header must retain at least one type parameter"
                    .to_string(),
            }));
        }
        let mut type_params = HashSet::new();
        for (index, parameter) in template.type_params.iter().enumerate() {
            if parameter.is_empty() {
                diagnostics.push(module_diag(SirDiagnosticKind::InvalidGenericTemplate {
                    template: name.clone(),
                    reason: format!("type parameter {index} has an empty semantic name"),
                }));
            }
            if !type_params.insert(parameter) {
                diagnostics.push(module_diag(SirDiagnosticKind::InvalidGenericTemplate {
                    template: name.clone(),
                    reason: format!("type parameter `{parameter}` occurs more than once"),
                }));
            }
        }
        for (index, parameter) in template.signature.params.iter().enumerate() {
            if !matches!(
                parameter.passing,
                SemParamPassing::ReadOnly | SemParamPassing::Consume
            ) || parameter.caller_visible_projection
            {
                diagnostics.push(module_diag(SirDiagnosticKind::InvalidGenericTemplate {
                    template: name.clone(),
                    reason: format!(
                        "template parameter {index} must retain a declared read or consume contract without a caller-visible projection"
                    ),
                }));
            }
        }
        if templates.insert(template.id.clone(), template).is_some() {
            diagnostics.push(module_diag(SirDiagnosticKind::DuplicateGenericTemplate(
                name,
            )));
        }
    }
    templates
}

/// Verify the semantic identity of one concrete SIR generic body.
///
/// Verify concrete semantic type arguments and substituted borrow contracts
/// before physical MIR chooses storage or ABI details.
fn verify_generic_callable_instance(
    module: &SemModule,
    callable: &SemCallable,
    key: &SirInstanceKey,
    seen: &mut HashSet<SirInstanceKey>,
    templates: &BTreeMap<GenericTemplateId, &SemGenericTemplate>,
    diagnostics: &mut Vec<SirDiagnostic>,
) {
    if key.template.declaration != callable.declaration {
        diagnostics.push(module_diag(SirDiagnosticKind::InvalidCallable {
            callable: callable.id,
            reason: "generic instance template declaration does not match callable provenance"
                .to_string(),
        }));
    }
    if key.type_args.is_empty() {
        diagnostics.push(module_diag(SirDiagnosticKind::InvalidCallable {
            callable: callable.id,
            reason: "generic instance has no semantic type arguments".to_string(),
        }));
    }
    if !seen.insert(key.clone()) {
        diagnostics.push(module_diag(SirDiagnosticKind::DuplicateCallableInstance(
            format!(
                "{}<{}>",
                key.template.declaration.full_path(),
                key.type_args
                    .iter()
                    .map(|ty| ty.user_facing().to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        )));
    }
    for (index, argument) in key.type_args.iter().enumerate() {
        if *argument != ResolvedTy::Never && !is_supported_call_value(module, argument) {
            diagnostics.push(module_diag(SirDiagnosticKind::InvalidCallable {
                callable: callable.id,
                reason: format!(
                    "generic semantic type argument {index} `{}` has no concrete SIR value contract",
                    argument.user_facing()
                ),
            }));
        }
    }
    let Some(template) = templates.get(&key.template).copied() else {
        diagnostics.push(module_diag(SirDiagnosticKind::InvalidCallable {
            callable: callable.id,
            reason: "generic instance has no body-free semantic template header".to_string(),
        }));
        return;
    };
    if callable.function != template.function {
        diagnostics.push(module_diag(SirDiagnosticKind::InvalidCallable {
            callable: callable.id,
            reason: "generic instance source item provenance does not match its semantic template header"
                .to_string(),
        }));
    }
    if callable.source_origin != template.source_origin {
        diagnostics.push(module_diag(SirDiagnosticKind::InvalidCallable {
            callable: callable.id,
            reason: "generic instance source origin does not match its semantic template header"
                .to_string(),
        }));
    }
    if key.type_args.len() != template.type_params.len() {
        diagnostics.push(module_diag(SirDiagnosticKind::InvalidCallable {
            callable: callable.id,
            reason: format!(
                "generic instance carries {} type argument(s), but template `{}` requires {}",
                key.type_args.len(),
                template.id.declaration.full_path(),
                template.type_params.len()
            ),
        }));
        return;
    }
    let expected_signature =
        match substitute_template_signature(template, &key.type_args, &module.type_facts) {
            Ok(signature) => signature,
            Err(reason) => {
                diagnostics.push(module_diag(SirDiagnosticKind::InvalidCallable {
                    callable: callable.id,
                    reason,
                }));
                return;
            }
        };
    if callable.signature != expected_signature {
        diagnostics.push(module_diag(SirDiagnosticKind::InvalidCallable {
            callable: callable.id,
            reason: "generic instance signature does not equal its semantic template signature after substitution"
                .to_string(),
        }));
    }
    let expected_symbol = function_monomorph_symbol(&template.symbol, &key.type_args);
    if callable.symbol != expected_symbol {
        diagnostics.push(module_diag(SirDiagnosticKind::InvalidCallable {
            callable: callable.id,
            reason:
                "generic instance emitted symbol is not the derived projection of its semantic key"
                    .to_string(),
        }));
    }
}

fn substitute_template_signature(
    template: &SemGenericTemplate,
    type_args: &[ResolvedTy],
    facts: &TypeFactTable,
) -> Result<SemSignature, String> {
    Ok(SemSignature {
        params: template
            .signature
            .params
            .iter()
            .map(|parameter| {
                let ty = substitute_type_params(&parameter.ty, &template.type_params, type_args);
                let own = crate::OwnKind::of_ty(&ty, facts)?;
                Ok(crate::SemAbiParam {
                    ty,
                    passing: if own == crate::OwnKind::Owned {
                        if parameter.passing == SemParamPassing::Consume {
                            SemParamPassing::Consume
                        } else {
                            SemParamPassing::Borrow
                        }
                    } else {
                        SemParamPassing::ReadOnly
                    },
                    caller_visible_projection: parameter.caller_visible_projection,
                })
            })
            .collect::<Result<Vec<_>, String>>()?,
        return_ty: substitute_type_params(
            &template.signature.return_ty,
            &template.type_params,
            type_args,
        ),
    })
}

fn verify_function_callable_identity(
    function: &SemFunction,
    callable_context: Option<&CallableContext<'_>>,
    diagnostics: &mut Vec<SirDiagnostic>,
) {
    let Some(callable_context) = callable_context else {
        return;
    };
    let Some(callable) = callable_context.callable(function.callable) else {
        diagnostics.push(diag(
            function,
            SirDiagnosticKind::MissingFunctionCallable {
                declaration: function.declaration.full_path().to_string(),
            },
        ));
        return;
    };
    let function_params = function
        .params
        .iter()
        .map(|parameter| parameter.ty.clone())
        .collect::<Vec<_>>();
    let callable_params = callable
        .signature
        .params
        .iter()
        .map(|parameter| parameter.ty.clone())
        .collect::<Vec<_>>();
    let identity_matches = callable.function == function.id
        && callable.declaration == function.declaration
        && callable.symbol == function.name
        && callable.source_origin == function.source_origin
        && callable_params == function_params
        && callable.signature.return_ty == function.return_ty;
    if !identity_matches {
        diagnostics.push(diag(
            function,
            SirDiagnosticKind::FunctionCallableMismatch {
                callable: function.callable,
                reason: "function identity, provenance, or SSA signature differs from its resolved callable"
                    .to_string(),
            },
        ));
    }
}

fn closure_for_body<'a>(
    function: &SemFunction,
    context: Option<&CallableContext<'a>>,
) -> Result<&'a crate::SemClosure, String> {
    context
        .and_then(|context| {
            context
                .closures
                .iter()
                .find(|closure| closure.body == function.callable)
        })
        .ok_or_else(|| "capture access has no concrete closure body descriptor".to_string())
}

fn verify_capture_places(
    function: &SemFunction,
    context: Option<&CallableContext<'_>>,
) -> Result<(), String> {
    let places = function
        .places
        .iter()
        .filter(|place| matches!(place.origin, crate::PlaceOrigin::Capture { .. }))
        .collect::<Vec<_>>();
    let closure = closure_for_body(function, context);
    if places.is_empty()
        && closure
            .as_ref()
            .map_or(true, |closure| closure.fields.is_empty())
    {
        return Ok(());
    }
    let closure = closure?;
    let receiver = function
        .params
        .first()
        .ok_or_else(|| "capture places have no environment receiver".to_string())?;
    if receiver.ty != closure.ty || places.len() != closure.fields.len() {
        return Err("capture places differ from the complete environment descriptor".to_string());
    }
    for (index, (place, field)) in places.into_iter().zip(&closure.fields).enumerate() {
        let index = u32::try_from(index).map_err(|_| "capture index exceeds u32".to_string())?;
        if place.id != crate::PlaceId(index)
            || place.ty != field.ty
            || place.origin
                != (crate::PlaceOrigin::Capture {
                    environment: receiver.value,
                    field: index,
                })
        {
            return Err(
                "capture place identity, type or receiver differs from its descriptor".to_string(),
            );
        }
    }
    Ok(())
}

fn verify_capture_operation(
    function: &SemFunction,
    operation: &SemOp,
    types: &HashMap<ValueId, ResolvedTy>,
    facts: &TypeFactTable,
    context: Option<&CallableContext<'_>>,
) -> Option<Result<(), String>> {
    let (place, stored, borrowed, takes) = match &operation.kind {
        SemOpKind::LoadCopy { place } => (*place, None, false, false),
        SemOpKind::LoadTake { place } => (*place, None, false, true),
        SemOpKind::LoadBorrow { place } => (*place, None, true, false),
        SemOpKind::StoreAssign { place, value } => (*place, Some(value), false, false),
        _ => return None,
    };
    Some((|| {
        let closure = closure_for_body(function, context)?;
        let decl = function
            .places
            .iter()
            .find(|decl| decl.id == place)
            .ok_or_else(|| "capture operation names an unknown place".to_string())?;
        let crate::PlaceOrigin::Capture { field, .. } = decl.origin else {
            return Err("capture operation requires an environment-owned place".to_string());
        };
        let field = closure
            .fields
            .get(field as usize)
            .ok_or_else(|| "capture operation names an unknown environment field".to_string())?;
        let (_, _, capabilities) = crate::callable_parts(&closure.ty)?;
        if let Some(value) = stored {
            if field.access != hew_types::ClosureCaptureAccess::Var
                || capabilities.call == hew_types::CallableCallMode::Read
            {
                return Err("capture assignment requires private mutable access".to_string());
            }
            if !operation.results.is_empty() || types.get(&value.value) != Some(&decl.ty) {
                return Err(
                    "capture assignment must consume one exact field value with no result"
                        .to_string(),
                );
            }
            return Ok(());
        }
        let [result] = operation.results.as_slice() else {
            return Err("capture load must produce exactly one field value".to_string());
        };
        if result.ty != decl.ty {
            return Err("capture load changes its field type".to_string());
        }
        if borrowed {
            if OwnKind::of_ty(&decl.ty, facts) != Ok(OwnKind::Owned) {
                return Err(
                    "capture loan requires its exact environment and an owning field".to_string(),
                );
            }
        } else if takes {
            if capabilities.call != hew_types::CallableCallMode::Once
                || field.consumption != hew_types::ClosureCaptureConsumption::Consumed
            {
                return Err(
                    "taking a capture requires a consuming field in a call-once body".to_string(),
                );
            }
        } else if facts
            .get(&hew_types::TypeInstanceKey(decl.ty.clone()))
            .is_none_or(|row| row.clone == hew_types::CloneKind::None)
        {
            return Err("capture field has no copy operation".to_string());
        }
        Ok(())
    })())
}

fn callable_mutation_permitted(
    function: &SemFunction,
    value: ValueId,
    context: Option<&CallableContext<'_>>,
) -> bool {
    let mut value = value;
    let mut visited = HashSet::new();
    loop {
        if !visited.insert(value) {
            return false;
        }
        if let Some((index, _)) = function
            .params
            .iter()
            .enumerate()
            .find(|(_, param)| param.value == value)
        {
            return context
                .and_then(|context| context.param_passing(function.callable, index))
                .is_some_and(|passing| {
                    matches!(
                        passing,
                        SemParamPassing::BorrowMut | SemParamPassing::Consume
                    )
                });
        }
        if function
            .blocks
            .iter()
            .flat_map(|block| &block.args)
            .any(|arg| arg.value == value)
        {
            return true;
        }
        let Some(operation) = function
            .blocks
            .iter()
            .flat_map(|block| &block.ops)
            .find(|operation| operation.results.iter().any(|result| result.id == value))
        else {
            // Call results are fresh owned values on their normal continuation.
            return function.blocks.iter().any(|block| {
                let mut found = false;
                block.terminator.visit_results(|result| {
                    found |= result.id == value && result.own == OwnKind::Owned;
                });
                found
            });
        };
        if let SemOpKind::LoadBorrow { place, .. } = operation.kind {
            if let Ok((root, _)) = crate::projection::place_path(&function.places, place) {
                match root {
                    crate::OwnerRoot::Value(root) => {
                        value = root;
                        continue;
                    }
                    crate::OwnerRoot::Local(_) => return true,
                }
            }
            return closure_for_body(function, context)
                .ok()
                .is_some_and(|closure| {
                    function
                        .places
                        .iter()
                        .find(|decl| decl.id == place)
                        .is_some_and(|decl| {
                            let crate::PlaceOrigin::Capture { field, .. } = decl.origin else {
                                return false;
                            };
                            closure.fields.get(field as usize).is_some_and(|field| {
                                field.access == hew_types::ClosureCaptureAccess::Var
                            }) && crate::callable_parts(&closure.ty).is_ok_and(|(_, _, caps)| {
                                caps.call != hew_types::CallableCallMode::Read
                            })
                        })
                });
        }
        if let Some(crate::PlaceBase::Value(parent)) = operation.kind.borrow_parent() {
            value = parent;
            continue;
        }
        return operation
            .results
            .iter()
            .any(|result| result.id == value && result.own == OwnKind::Owned);
    }
}

fn verify_indirect_call(
    function: &SemFunction,
    terminator: &SemTerminator,
    types: &HashMap<ValueId, ResolvedTy>,
    facts: &TypeFactTable,
    blocks: &BTreeMap<BlockId, &crate::SemBlock>,
    context: Option<&CallableContext<'_>>,
) -> Result<(), String> {
    let SemTerminator::IndirectCall {
        callee,
        signature,
        args,
        result,
        normal,
        unwind,
        ..
    } = terminator
    else {
        unreachable!()
    };
    if !matches!(unwind, crate::CallUnwind::Cleanup(edge) if failure_cfg_matches_exit(edge, None, blocks))
    {
        return Err(
            "indirect call requires cleanup that propagates the original fault".to_string(),
        );
    }
    let ty = types
        .get(&callee.operand.value)
        .ok_or_else(|| "indirect call has no typed receiver".to_string())?;
    let (_, _, capabilities) = crate::callable_parts(ty)?;
    if OwnKind::of_ty(ty, facts) != Ok(OwnKind::Owned)
        || signature != &crate::callable_value_signature(ty, facts)?
    {
        return Err("indirect call differs from its exact callable type and signature".to_string());
    }
    let decision = match capabilities.call {
        hew_types::CallableCallMode::Read => crate::BoundaryDecision::Borrow,
        hew_types::CallableCallMode::Var => crate::BoundaryDecision::BorrowMut,
        hew_types::CallableCallMode::Once => crate::BoundaryDecision::Move,
    };
    if callee.decision != decision {
        return Err(
            "indirect receiver transfer differs from its invocation capability".to_string(),
        );
    }
    if capabilities.call == hew_types::CallableCallMode::Var
        && !callable_mutation_permitted(function, callee.operand.value, context)
    {
        return Err(
            "mutable invocation requires an owned receiver or a proved private mutable loan"
                .to_string(),
        );
    }
    if args.len() != signature.params.len() {
        return Err("indirect call argument count differs from its signature".to_string());
    }
    for (arg, param) in args.iter().zip(&signature.params) {
        let expected = match param.passing {
            SemParamPassing::ReadOnly => crate::BoundaryDecision::Copy,
            SemParamPassing::Borrow => crate::BoundaryDecision::Borrow,
            SemParamPassing::BorrowMut => crate::BoundaryDecision::BorrowMut,
            SemParamPassing::Consume => crate::BoundaryDecision::Move,
        };
        if arg.decision != expected || types.get(&arg.operand.value) != Some(&param.ty) {
            return Err(
                "indirect call argument type or transfer differs from its signature".to_string(),
            );
        }
    }
    if normal.is_none() != (signature.return_ty == ResolvedTy::Never) {
        return Err("indirect call normal edge differs from its return type".to_string());
    }
    match result {
        crate::CallResult::Value(value)
            if value.ty == signature.return_ty && value.ty != ResolvedTy::Unit =>
        {
            Ok(())
        }
        crate::CallResult::Unit if signature.return_ty == ResolvedTy::Unit => Ok(()),
        crate::CallResult::Never if signature.return_ty == ResolvedTy::Never => Ok(()),
        _ => Err("indirect call result differs from its signature".to_string()),
    }
}

fn verify_callable_operation(
    function: &SemFunction,
    operation: &SemOp,
    types: &HashMap<ValueId, ResolvedTy>,
    facts: &TypeFactTable,
    context: Option<&CallableContext<'_>>,
    diagnostics: &mut Vec<SirDiagnostic>,
) {
    if !matches!(
        operation.kind,
        SemOpKind::FunctionMake { .. }
            | SemOpKind::ClosureMake { .. }
            | SemOpKind::GeneratorMake { .. }
            | SemOpKind::CallableCoerce { .. }
    ) {
        return;
    }
    let [result] = operation.results.as_slice() else {
        return;
    };
    let checked = (|| {
        let context = context.ok_or_else(|| {
            "callable construction requires its module's semantic contract".to_string()
        })?;
        match &operation.kind {
            SemOpKind::FunctionMake { callable } => {
                let target = context
                    .callable(*callable)
                    .ok_or_else(|| "function value has no exact callable target".to_string())?;
                if target.kind != SemCallableKind::HewDirect {
                    return Err(
                        "a closure body cannot be exposed without its environment".to_string()
                    );
                }
                let ResolvedTy::Function { capabilities, .. } = &result.ty else {
                    return Err(
                        "a function value requires a capture-free function type".to_string()
                    );
                };
                if *capabilities != hew_types::CallableCapabilities::FUNCTION_ITEM
                    || crate::callable_value_signature(&result.ty, facts)? != target.signature
                {
                    return Err(
                        "function value signature or capabilities differ from its exact target"
                            .to_string(),
                    );
                }
            }
            SemOpKind::ClosureMake { closure, fields } => {
                let descriptor = context
                    .closures
                    .get(closure.0 as usize)
                    .filter(|descriptor| descriptor.id == *closure)
                    .ok_or_else(|| {
                        "closure construction has no canonical environment descriptor".to_string()
                    })?;
                if descriptor.instance.enclosing != function.callable
                    || descriptor.ty != result.ty
                    || fields.len() != descriptor.fields.len()
                {
                    return Err("closure construction disagrees with its enclosing instance, type or field count".to_string());
                }
                for (value, field) in fields.iter().zip(&descriptor.fields) {
                    if types.get(&value.value) != Some(&field.ty) {
                        return Err(
                            "closure construction changes a captured field type".to_string()
                        );
                    }
                }
            }
            SemOpKind::GeneratorMake { closure, callable } => {
                let descriptor = context
                    .closures
                    .get(closure.0 as usize)
                    .filter(|descriptor| descriptor.id == *closure)
                    .ok_or("generator has no exact producer environment")?;
                let (yielded, returned) = crate::generator_parts(&result.ty)
                    .ok_or("generator construction has no checked generator type")?;
                let (params, output, capabilities) = crate::callable_parts(&descriptor.ty)?;
                if types.get(&callable.value) != Some(&descriptor.ty)
                    || descriptor.generator_yield.as_ref() != Some(yielded)
                    || output != returned
                    || !params.is_empty()
                    || capabilities.call != hew_types::CallableCallMode::Once
                {
                    return Err("generator construction changes its producer contract".into());
                }
            }
            SemOpKind::CallableCoerce { source } => {
                let source_ty = types
                    .get(&source.value)
                    .ok_or_else(|| "callable coercion has no input definition".to_string())?;
                crate::verify_callable_coercion(source_ty, &result.ty, facts)?;
            }
            _ => unreachable!("selected callable construction operation"),
        }
        if result.own != crate::OwnKind::Owned {
            return Err(
                "a callable value must carry its environment ownership obligation".to_string(),
            );
        }
        Ok(())
    })();
    if let Err(reason) = checked {
        invalid_operation(function, operation.id, reason, diagnostics);
    }
}

fn is_initial_scalar(ty: &ResolvedTy) -> bool {
    ty.is_integer()
        || matches!(
            ty,
            ResolvedTy::Bool | ResolvedTy::F64 | ResolvedTy::Char | ResolvedTy::Duration
        )
}

fn is_initial_call_value(ty: &ResolvedTy) -> bool {
    if hew_types::runtime_call::FileReadHandleKind::of_ty(ty).is_some()
        || hew_types::runtime_call::IoHandleKind::of_ty(ty).is_some()
    {
        return true;
    }
    crate::generator_parts(ty).is_some()
        || is_initial_scalar(ty)
        || matches!(
            ty,
            ResolvedTy::String | ResolvedTy::Bytes | ResolvedTy::Task(_)
        )
}

/// Value types physical MIR can realize without borrowing, drops, allocation,
/// or layout-dependent semantics.
///
/// SIR retains tuples as abstract values; this predicate merely bounds the
/// semantic domain to recursively `BitCopy` scalar elements until the
/// ownership/layout layer owns aggregate resource realization.
fn is_initial_value_type(ty: &ResolvedTy) -> bool {
    is_initial_scalar(ty)
        || matches!(ty, ResolvedTy::Tuple(elements)
            if elements.iter().all(is_initial_value_type))
}

fn is_supported_call_value(module: &SemModule, ty: &ResolvedTy) -> bool {
    is_initial_call_value(ty)
        || crate::stream_element(ty).is_some()
        || crate::sink_element(ty).is_some()
        || module.actors.iter().any(|actor| actor.handle_ty == *ty)
        || hew_types::runtime_call::collection_type_arguments(ty).is_some()
        || ty.is_builtin(hew_types::BuiltinType::JsonValue)
        || ty.is_builtin(hew_types::BuiltinType::YamlValue)
        || matches!(ty, ResolvedTy::Function { .. } | ResolvedTy::Closure { .. })
        || matches!(ty, ResolvedTy::Tuple(_))
        || module.aggregate_shape_for_type(ty).is_some()
        || module.variant_shape_for_type(ty).is_some()
}

fn is_supported_call_return(module: &SemModule, ty: &ResolvedTy) -> bool {
    matches!(ty, ResolvedTy::Unit | ResolvedTy::Never) || is_supported_call_value(module, ty)
}

/// Verify one consuming destructure: exact per-field recipes and result arity.
fn verify_destructure_shape(
    function: &SemFunction,
    operation: &SemOp,
    types: &HashMap<ValueId, ResolvedTy>,
    facts: &TypeFactTable,
    aggregate_shapes: &[SemAggregateShape],
    variant_shapes: &[SemVariantShape],
    diagnostics: &mut Vec<SirDiagnostic>,
) {
    let destructure = match &operation.kind {
        SemOpKind::Destructure { shape, aggregate } => types.get(&aggregate.value).map(|ty| {
            (
                "aggregate.destructure",
                ty,
                crate::aggregate_field_recipes(*shape, ty, aggregate_shapes, facts),
            )
        }),
        SemOpKind::VariantDestructure {
            shape,
            variant,
            source,
        } => types.get(&source.value).map(|ty| {
            (
                "variant.destructure",
                ty,
                crate::variant_field_recipes(*shape, *variant, ty, variant_shapes, facts),
            )
        }),
        _ => None,
    };
    let Some((operation_name, source_ty, recipes)) = destructure else {
        return;
    };
    let recipes = match recipes {
        Ok(recipes) => recipes,
        Err(reason) => {
            invalid_operation(function, operation.id, reason, diagnostics);
            return;
        }
    };
    if crate::OwnKind::of_ty(source_ty, facts).is_err() {
        invalid_operation(
            function,
            operation.id,
            format!(
                "{operation_name} operand `{}` has no exact ownership facts",
                source_ty.user_facing()
            ),
            diagnostics,
        );
    }
    if operation.results.len() != recipes.len() {
        diagnostics.push(diag(
            function,
            SirDiagnosticKind::InvalidResultArity {
                op: operation.id,
                actual: operation.results.len(),
            },
        ));
        return;
    }
    for (index, (result, recipe)) in operation.results.iter().zip(recipes).enumerate() {
        if result.ty != recipe.ty {
            invalid_operation(
                function,
                operation.id,
                format!(
                    "{operation_name} result {index} has `{}`, expected `{}`",
                    result.ty.user_facing(),
                    recipe.ty.user_facing()
                ),
                diagnostics,
            );
        }
    }
}

#[allow(
    clippy::too_many_lines,
    reason = "the closed first-slice operation relation table is deliberately central so additions must make their verifier rule explicit"
)]
fn verify_operation_shape(
    function: &SemFunction,
    operation: &SemOp,
    types: &HashMap<ValueId, ResolvedTy>,
    facts: &TypeFactTable,
    aggregate_shapes: &[SemAggregateShape],
    variant_shapes: &[SemVariantShape],
    diagnostics: &mut Vec<SirDiagnostic>,
) {
    if matches!(
        operation.kind,
        SemOpKind::Destructure { .. } | SemOpKind::VariantDestructure { .. }
    ) {
        verify_destructure_shape(
            function,
            operation,
            types,
            facts,
            aggregate_shapes,
            variant_shapes,
            diagnostics,
        );
        return;
    }
    if let SemOpKind::StreamPipe { capacity } = operation.kind {
        let element = match operation.results.as_slice() {
            [stream, sink] => crate::pipe_parts(&stream.ty, &sink.ty).filter(|_| {
                stream.own == crate::OwnKind::Owned && sink.own == crate::OwnKind::Owned
            }),
            _ => None,
        };
        if element.is_none() || capacity == 0 {
            invalid_operation(
                function,
                operation.id,
                "stream.pipe produces one owned Stream<T> and one owned Sink<T> of the same element"
                    .to_string(),
                diagnostics,
            );
        }
        return;
    }
    let expected_results = usize::from(!matches!(
        operation.kind,
        SemOpKind::TaskScopeEnter { .. }
            | SemOpKind::TaskScopeClose { .. }
            | SemOpKind::RegisterDefer { .. }
            | SemOpKind::DestroyValue { .. }
            | SemOpKind::AllocPlace { .. }
            | SemOpKind::EndBorrow { .. }
            | SemOpKind::StoreInit { .. }
            | SemOpKind::StoreAssign { .. }
            | SemOpKind::EndLifetime { .. }
    ));
    if operation.results.len() != expected_results {
        diagnostics.push(diag(
            function,
            SirDiagnosticKind::InvalidResultArity {
                op: operation.id,
                actual: operation.results.len(),
            },
        ));
        return;
    }
    if let SemOpKind::DestroyValue { value } = &operation.kind {
        if let Some(ty) = types.get(&value.value) {
            if crate::OwnKind::of_ty(ty, facts) != Ok(crate::OwnKind::Owned) {
                invalid_operation(
                    function,
                    operation.id,
                    format!("destroy_value operand `{}` is not owned", ty.user_facing()),
                    diagnostics,
                );
            }
        }
        return;
    }
    if let SemOpKind::TaskScopeEnter {
        duration: Some(duration),
        ..
    } = &operation.kind
    {
        if types.get(&duration.value) != Some(&ResolvedTy::Duration) {
            invalid_operation(
                function,
                operation.id,
                "scope deadline requires Duration".into(),
                diagnostics,
            );
        }
    }
    if matches!(
        operation.kind,
        SemOpKind::TaskScopeEnter { .. }
            | SemOpKind::TaskScopeClose { .. }
            | SemOpKind::RegisterDefer { .. }
            | SemOpKind::EndBorrow { .. }
    ) {
        // The lifetime relation requires an active local loan and proves
        // that every projection depending on it has already ended.
        return;
    }
    if expected_results == 0 {
        invalid_operation(
            function,
            operation.id,
            "operation is outside the verified SIR relation table".to_string(),
            diagnostics,
        );
        return;
    }
    let result = &operation.results[0];
    match &operation.kind {
        SemOpKind::TaskSpawn { callable, .. } => {
            let valid = types.get(&callable.value).is_some_and(|ty| {
                crate::callable_parts(ty).is_ok_and(|(params, output, caps)| {
                    params.is_empty()
                        && caps.call == hew_types::CallableCallMode::Once
                        && result.ty == ResolvedTy::Task(Box::new(output.clone()))
                })
            });
            if !valid {
                invalid_operation(
                    function,
                    operation.id,
                    "task spawn requires a nullary once callable and its exact Task result".into(),
                    diagnostics,
                );
            }
        }
        SemOpKind::TaskScopeEnter { .. }
        | SemOpKind::TaskScopeClose { .. }
        | SemOpKind::RegisterDefer { .. } => unreachable!("result-free marker handled above"),
        SemOpKind::ConstUnit => {
            if result.ty != ResolvedTy::Unit || result.own != crate::OwnKind::None {
                invalid_operation(
                    function,
                    operation.id,
                    "const unit must produce exactly Unit without ownership".to_string(),
                    diagnostics,
                );
            }
        }
        SemOpKind::ConstI64(_) if !result.ty.is_integer() => diagnostics.push(diag(
            function,
            SirDiagnosticKind::InvalidConstType {
                op: operation.id,
                expected: "integer",
                actual: result.ty.user_facing().to_string(),
            },
        )),
        SemOpKind::ConstBool(_) if result.ty != ResolvedTy::Bool => diagnostics.push(diag(
            function,
            SirDiagnosticKind::InvalidConstType {
                op: operation.id,
                expected: "bool",
                actual: result.ty.user_facing().to_string(),
            },
        )),
        SemOpKind::ConstF64(_) if result.ty != ResolvedTy::F64 => diagnostics.push(diag(
            function,
            SirDiagnosticKind::InvalidConstType {
                op: operation.id,
                expected: "f64",
                actual: result.ty.user_facing().to_string(),
            },
        )),
        SemOpKind::ConstDuration(_) if result.ty != ResolvedTy::Duration => diagnostics.push(diag(
            function,
            SirDiagnosticKind::InvalidConstType {
                op: operation.id,
                expected: "duration",
                actual: result.ty.user_facing().to_string(),
            },
        )),
        SemOpKind::ConstChar(_) if result.ty != ResolvedTy::Char => diagnostics.push(diag(
            function,
            SirDiagnosticKind::InvalidConstType {
                op: operation.id,
                expected: "char",
                actual: result.ty.user_facing().to_string(),
            },
        )),
        SemOpKind::TupleMake { elements } => {
            let ResolvedTy::Tuple(element_tys) = &result.ty else {
                invalid_operation(
                    function,
                    operation.id,
                    format!(
                        "tuple.make result must have a semantic tuple type, found `{}`",
                        result.ty.user_facing()
                    ),
                    diagnostics,
                );
                return;
            };
            if !is_initial_value_type(&result.ty) {
                invalid_operation(
                    function,
                    operation.id,
                    format!(
                        "tuple.make result `{}` is outside SIR's initial no-drop scalar/tuple value domain",
                        result.ty.user_facing()
                    ),
                    diagnostics,
                );
            }
            if element_tys.len() != elements.len() {
                invalid_operation(
                    function,
                    operation.id,
                    format!(
                        "tuple.make for `{}` has {} operand(s), expected {}",
                        result.ty.user_facing(),
                        elements.len(),
                        element_tys.len()
                    ),
                    diagnostics,
                );
            }
            for (index, (element, expected_ty)) in elements.iter().zip(element_tys).enumerate() {
                if let Some(actual_ty) = types.get(&element.value) {
                    if actual_ty != expected_ty {
                        invalid_operation(
                            function,
                            operation.id,
                            format!(
                                "tuple.make operand {index} has `{}`, expected `{}`",
                                actual_ty.user_facing(),
                                expected_ty.user_facing()
                            ),
                            diagnostics,
                        );
                    }
                }
            }
        }
        SemOpKind::TupleGet { tuple, index } => {
            let Some(tuple_ty) = types.get(&tuple.value) else {
                return;
            };
            let ResolvedTy::Tuple(element_tys) = tuple_ty else {
                invalid_operation(
                    function,
                    operation.id,
                    format!(
                        "tuple.get operand has non-tuple semantic type `{}`",
                        tuple_ty.user_facing()
                    ),
                    diagnostics,
                );
                return;
            };
            if !is_initial_value_type(tuple_ty) {
                invalid_operation(
                    function,
                    operation.id,
                    format!(
                        "tuple.get operand `{}` is outside SIR's initial no-drop scalar/tuple value domain",
                        tuple_ty.user_facing()
                    ),
                    diagnostics,
                );
            }
            let Some(expected_ty) = usize::try_from(*index)
                .ok()
                .and_then(|index| element_tys.get(index))
            else {
                invalid_operation(
                    function,
                    operation.id,
                    format!(
                        "tuple.get index {index} is out of bounds for `{}` with {} element(s)",
                        tuple_ty.user_facing(),
                        element_tys.len()
                    ),
                    diagnostics,
                );
                return;
            };
            if &result.ty != expected_ty {
                invalid_operation(
                    function,
                    operation.id,
                    format!(
                        "tuple.get index {index} from `{}` produces `{}`, expected `{}`",
                        tuple_ty.user_facing(),
                        result.ty.user_facing(),
                        expected_ty.user_facing()
                    ),
                    diagnostics,
                );
            }
        }
        SemOpKind::AggregateMake { shape, fields } => {
            let recipes =
                match crate::aggregate_field_recipes(*shape, &result.ty, aggregate_shapes, facts) {
                    Ok(recipes) => recipes,
                    Err(reason) => {
                        invalid_operation(function, operation.id, reason, diagnostics);
                        return;
                    }
                };
            if crate::OwnKind::of_ty(&result.ty, facts).is_err() {
                invalid_operation(
                    function,
                    operation.id,
                    format!(
                        "aggregate.make result `{}` has no exact ownership facts",
                        result.ty.user_facing()
                    ),
                    diagnostics,
                );
            }
            if fields.len() != recipes.len() {
                invalid_operation(
                    function,
                    operation.id,
                    format!(
                        "aggregate.make for `{}` has {} field(s), expected {}",
                        result.ty.user_facing(),
                        fields.len(),
                        recipes.len()
                    ),
                    diagnostics,
                );
            }
            for (index, (field, recipe)) in fields.iter().zip(recipes).enumerate() {
                if types.get(&field.value) != Some(&recipe.ty) {
                    let actual = types
                        .get(&field.value)
                        .map_or("<undefined>".to_string(), |ty| ty.user_facing().to_string());
                    invalid_operation(
                        function,
                        operation.id,
                        format!(
                            "aggregate.make field {index} has `{actual}`, expected `{}`",
                            recipe.ty.user_facing()
                        ),
                        diagnostics,
                    );
                }
            }
        }
        SemOpKind::VariantMake {
            shape,
            variant,
            fields,
        } => {
            let recipes = match crate::variant_field_recipes(
                *shape,
                *variant,
                &result.ty,
                variant_shapes,
                facts,
            ) {
                Ok(recipes) => recipes,
                Err(reason) => {
                    invalid_operation(function, operation.id, reason, diagnostics);
                    return;
                }
            };
            if crate::OwnKind::of_ty(&result.ty, facts).is_err() {
                invalid_operation(
                    function,
                    operation.id,
                    format!(
                        "variant.make result `{}` has no exact ownership facts",
                        result.ty.user_facing()
                    ),
                    diagnostics,
                );
            }
            if fields.len() != recipes.len() {
                invalid_operation(
                    function,
                    operation.id,
                    format!(
                        "variant.make for `{}` variant {variant} has {} field(s), expected {}",
                        result.ty.user_facing(),
                        fields.len(),
                        recipes.len()
                    ),
                    diagnostics,
                );
            }
            for (index, (field, recipe)) in fields.iter().zip(recipes).enumerate() {
                if types.get(&field.value) != Some(&recipe.ty) {
                    let actual = types
                        .get(&field.value)
                        .map_or("<undefined>".to_string(), |ty| ty.user_facing().to_string());
                    invalid_operation(
                        function,
                        operation.id,
                        format!(
                            "variant.make field {index} has `{actual}`, expected `{}`",
                            recipe.ty.user_facing()
                        ),
                        diagnostics,
                    );
                }
            }
        }
        SemOpKind::AggregateProjectCopy {
            shape,
            aggregate,
            field,
        }
        | SemOpKind::AggregateProjectBorrow {
            shape,
            aggregate,
            field,
        } => {
            let borrowing = operation.kind.borrow_parent().is_some();
            let operation_name = if borrowing {
                "aggregate.project_borrow"
            } else {
                "aggregate.project_copy"
            };
            let Some(aggregate_ty) = types.get(&aggregate.value) else {
                return;
            };
            let recipes =
                match crate::aggregate_field_recipes(*shape, aggregate_ty, aggregate_shapes, facts)
                {
                    Ok(recipes) => recipes,
                    Err(reason) => {
                        invalid_operation(function, operation.id, reason, diagnostics);
                        return;
                    }
                };
            if crate::OwnKind::of_ty(aggregate_ty, facts).is_err() {
                invalid_operation(
                    function,
                    operation.id,
                    format!(
                        "{operation_name} operand `{}` has no exact ownership facts",
                        aggregate_ty.user_facing()
                    ),
                    diagnostics,
                );
            }
            let Some(recipe) = usize::try_from(*field)
                .ok()
                .and_then(|index| recipes.get(index))
            else {
                invalid_operation(
                    function,
                    operation.id,
                    format!(
                        "{operation_name} field {field} is out of bounds for `{}` with {} field(s)",
                        aggregate_ty.user_facing(),
                        recipes.len()
                    ),
                    diagnostics,
                );
                return;
            };
            if result.ty != recipe.ty {
                invalid_operation(
                    function,
                    operation.id,
                    format!(
                        "{operation_name} field {field} produces `{}`, expected `{}`",
                        result.ty.user_facing(),
                        recipe.ty.user_facing()
                    ),
                    diagnostics,
                );
            }
            if borrowing && recipe.own != crate::OwnKind::Owned {
                invalid_operation(
                    function,
                    operation.id,
                    "aggregate.project_borrow requires an owning field; no-drop fields use an ordinary copy"
                        .to_string(),
                    diagnostics,
                );
            }
            if !borrowing && recipe.clone == hew_types::CloneKind::None {
                invalid_operation(
                    function,
                    operation.id,
                    format!(
                        "aggregate.project_copy field {field} of `{}` has no copy operation",
                        aggregate_ty.user_facing()
                    ),
                    diagnostics,
                );
            }
        }
        SemOpKind::VariantIs {
            shape,
            variant,
            source,
        } => {
            let Some(source_ty) = types.get(&source.value) else {
                return;
            };
            if let Err(reason) =
                crate::variant_field_types(*shape, *variant, source_ty, variant_shapes)
            {
                invalid_operation(function, operation.id, reason, diagnostics);
            }
            if result.ty != ResolvedTy::Bool {
                invalid_operation(
                    function,
                    operation.id,
                    format!(
                        "variant.is produces `{}`, expected `bool`",
                        result.ty.user_facing()
                    ),
                    diagnostics,
                );
            }
        }
        SemOpKind::VariantProjectCopy {
            shape,
            variant,
            source,
            field,
        }
        | SemOpKind::VariantProjectBorrow {
            shape,
            variant,
            source,
            field,
        } => {
            let borrowing = operation.kind.borrow_parent().is_some();
            let operation_name = if borrowing {
                "variant.project_borrow"
            } else {
                "variant.project_copy"
            };
            let Some(source_ty) = types.get(&source.value) else {
                return;
            };
            let recipes = match crate::variant_field_recipes(
                *shape,
                *variant,
                source_ty,
                variant_shapes,
                facts,
            ) {
                Ok(recipes) => recipes,
                Err(reason) => {
                    invalid_operation(function, operation.id, reason, diagnostics);
                    return;
                }
            };
            let Some(recipe) = usize::try_from(*field)
                .ok()
                .and_then(|index| recipes.get(index))
            else {
                invalid_operation(
                    function,
                    operation.id,
                    format!(
                        "{operation_name} field {field} is out of bounds for `{}` variant {variant} with {} field(s)",
                        source_ty.user_facing(),
                        recipes.len()
                    ),
                    diagnostics,
                );
                return;
            };
            if result.ty != recipe.ty {
                invalid_operation(
                    function,
                    operation.id,
                    format!(
                        "{operation_name} field {field} produces `{}`, expected `{}`",
                        result.ty.user_facing(),
                        recipe.ty.user_facing()
                    ),
                    diagnostics,
                );
            }
            if borrowing && recipe.own != crate::OwnKind::Owned {
                invalid_operation(
                    function,
                    operation.id,
                    "variant.project_borrow requires an owning field; no-drop fields use an ordinary copy"
                        .to_string(),
                    diagnostics,
                );
            }
            if !borrowing && recipe.clone == hew_types::CloneKind::None {
                invalid_operation(
                    function,
                    operation.id,
                    format!(
                        "variant.project_copy field {field} of `{}` has no copy operation",
                        source_ty.user_facing()
                    ),
                    diagnostics,
                );
            }
        }
        SemOpKind::Cast { value, to } => {
            if &result.ty != to {
                diagnostics.push(diag(
                    function,
                    SirDiagnosticKind::InvalidCast {
                        op: operation.id,
                        reason: "operation result type differs from cast target type".to_string(),
                    },
                ));
            }
            if let Some(from) = types.get(&value.value) {
                if !from.can_explicitly_numeric_cast_to(to) {
                    diagnostics.push(diag(
                        function,
                        SirDiagnosticKind::InvalidCast {
                            op: operation.id,
                            reason: format!(
                                "checker does not admit `{}` as `{}`",
                                from.user_facing(),
                                to.user_facing()
                            ),
                        },
                    ));
                }
            }
        }
        SemOpKind::Unary { op, value } => {
            let Some(operand_ty) = types.get(&value.value) else {
                return;
            };
            let valid = match op {
                hew_parser::ast::UnaryOp::Not => {
                    operand_ty == &ResolvedTy::Bool && result.ty == ResolvedTy::Bool
                }
                hew_parser::ast::UnaryOp::Negate => {
                    operand_ty == &result.ty && operand_ty.is_float()
                }
                hew_parser::ast::UnaryOp::BitNot => {
                    operand_ty == &result.ty && operand_ty.is_integer()
                }
                // Raw dereference is rejected before HIR. A future safe load
                // operation will carry explicit memory semantics instead.
                hew_parser::ast::UnaryOp::RawDeref => false,
            };
            if !valid {
                invalid_operation(
                    function,
                    operation.id,
                    format!(
                        "unary `{op:?}` has invalid `{}` -> `{}` types",
                        operand_ty.user_facing(),
                        result.ty.user_facing()
                    ),
                    diagnostics,
                );
            }
        }
        SemOpKind::Binary { op, lhs, rhs } => {
            let (Some(lhs_ty), Some(rhs_ty)) = (types.get(&lhs.value), types.get(&rhs.value))
            else {
                return;
            };
            let valid = match op {
                hew_parser::ast::BinaryOp::And | hew_parser::ast::BinaryOp::Or => false,
                hew_parser::ast::BinaryOp::Equal
                | hew_parser::ast::BinaryOp::NotEqual
                | hew_parser::ast::BinaryOp::Less
                | hew_parser::ast::BinaryOp::LessEqual
                | hew_parser::ast::BinaryOp::Greater
                | hew_parser::ast::BinaryOp::GreaterEqual => {
                    lhs_ty == rhs_ty && result.ty == ResolvedTy::Bool
                }
                hew_parser::ast::BinaryOp::Range | hew_parser::ast::BinaryOp::RangeInclusive => {
                    lhs_ty == rhs_ty
                }
                hew_parser::ast::BinaryOp::Add
                | hew_parser::ast::BinaryOp::Subtract
                | hew_parser::ast::BinaryOp::Multiply
                | hew_parser::ast::BinaryOp::Divide
                | hew_parser::ast::BinaryOp::Modulo
                | hew_parser::ast::BinaryOp::Shl
                | hew_parser::ast::BinaryOp::Shr => {
                    lhs_ty == rhs_ty
                        && lhs_ty == &result.ty
                        && crate::checked_binary_failure_kinds(*op, lhs_ty).is_none()
                }
                hew_parser::ast::BinaryOp::BitAnd
                | hew_parser::ast::BinaryOp::BitOr
                | hew_parser::ast::BinaryOp::BitXor
                | hew_parser::ast::BinaryOp::WrappingAdd
                | hew_parser::ast::BinaryOp::WrappingSub
                | hew_parser::ast::BinaryOp::WrappingMul => {
                    lhs_ty == rhs_ty && lhs_ty == &result.ty
                }
            };
            if !valid {
                let reason = match op {
                    hew_parser::ast::BinaryOp::And | hew_parser::ast::BinaryOp::Or => {
                        "logical `&&` / `||` must be represented as SIR branch CFG, not Binary"
                            .to_string()
                    }
                    hew_parser::ast::BinaryOp::Add
                    | hew_parser::ast::BinaryOp::Subtract
                    | hew_parser::ast::BinaryOp::Multiply
                    | hew_parser::ast::BinaryOp::Divide
                    | hew_parser::ast::BinaryOp::Modulo
                    | hew_parser::ast::BinaryOp::Shl
                    | hew_parser::ast::BinaryOp::Shr
                        if lhs_ty.is_integer() =>
                    {
                        format!("checked integer `{op}` must use a CheckedBinary terminator")
                    }
                    _ => format!(
                        "binary `{op}` has incompatible `{}`, `{}` -> `{}` types",
                        lhs_ty.user_facing(),
                        rhs_ty.user_facing(),
                        result.ty.user_facing()
                    ),
                };
                invalid_operation(function, operation.id, reason, diagnostics);
            }
        }
        SemOpKind::ConstStr(_) if result.ty != ResolvedTy::String => diagnostics.push(diag(
            function,
            SirDiagnosticKind::InvalidConstType {
                op: operation.id,
                expected: "string",
                actual: result.ty.user_facing().to_string(),
            },
        )),
        SemOpKind::ConstBytes(_) if result.ty != ResolvedTy::Bytes => diagnostics.push(diag(
            function,
            SirDiagnosticKind::InvalidConstType {
                op: operation.id,
                expected: "bytes",
                actual: result.ty.user_facing().to_string(),
            },
        )),
        SemOpKind::CopyValue { source } | SemOpKind::Move { source } => {
            if let Some(source_ty) = types.get(&source.value) {
                if source_ty != &result.ty {
                    invalid_operation(
                        function,
                        operation.id,
                        format!(
                            "ownership operation has `{}` input and `{}` result",
                            source_ty.user_facing(),
                            result.ty.user_facing()
                        ),
                        diagnostics,
                    );
                }
            }
            if crate::OwnKind::of_ty(&result.ty, facts) != Ok(crate::OwnKind::Owned) {
                invalid_operation(
                    function,
                    operation.id,
                    format!(
                        "ownership operation result `{}` is not an owned value",
                        result.ty.user_facing()
                    ),
                    diagnostics,
                );
            }
            if matches!(operation.kind, SemOpKind::CopyValue { .. })
                && facts
                    .get(&hew_types::TypeInstanceKey(result.ty.clone()))
                    .is_none_or(|row| row.clone == hew_types::CloneKind::None)
            {
                invalid_operation(
                    function,
                    operation.id,
                    format!("`{}` has no copy operation", result.ty.user_facing()),
                    diagnostics,
                );
            }
        }
        SemOpKind::BeginBorrow { owner } => {
            if types.get(&owner.value) != Some(&result.ty)
                || crate::OwnKind::of_ty(&result.ty, facts) != Ok(crate::OwnKind::Owned)
            {
                invalid_operation(
                    function,
                    operation.id,
                    "begin_borrow must preserve the exact type of an owning value".to_string(),
                    diagnostics,
                );
            }
        }
        SemOpKind::ConstI64(_)
        | SemOpKind::ConstBool(_)
        | SemOpKind::ConstF64(_)
        | SemOpKind::ConstChar(_)
        | SemOpKind::ConstDuration(_)
        | SemOpKind::ConstStr(_)
        | SemOpKind::ConstBytes(_)
        | SemOpKind::FunctionMake { .. }
        | SemOpKind::ClosureMake { .. }
        | SemOpKind::GeneratorMake { .. }
        | SemOpKind::StreamPipe { .. }
        | SemOpKind::CallableCoerce { .. } => {}
        // Dormant operations remain fail-closed until their producer and
        // complete semantic validation land together.
        SemOpKind::LoadBorrow { .. }
        | SemOpKind::StrEq { .. }
        | SemOpKind::BytesEq { .. }
        | SemOpKind::EndBorrow { .. }
        | SemOpKind::DestroyValue { .. }
        | SemOpKind::Fork { .. }
        | SemOpKind::Destructure { .. }
        | SemOpKind::VariantDestructure { .. }
        | SemOpKind::AllocPlace { .. }
        | SemOpKind::LoadCopy { .. }
        | SemOpKind::LoadTake { .. }
        | SemOpKind::StoreInit { .. }
        | SemOpKind::StoreAssign { .. }
        | SemOpKind::EndLifetime { .. } => invalid_operation(
            function,
            operation.id,
            "operation is outside the verified SIR relation table".to_string(),
            diagnostics,
        ),
    }
}

#[allow(
    clippy::too_many_arguments,
    clippy::too_many_lines,
    reason = "direct-call verification keeps callable ABI, result arity, and operand rules together at the SIR boundary"
)]
fn verify_direct_call_terminator(
    function: &SemFunction,
    id: OpId,
    callee: CallableId,
    args: &[crate::BoundaryOperand],
    result: &crate::CallResult,
    normal: Option<&crate::Edge>,
    types: &HashMap<ValueId, ResolvedTy>,
    callable_context: Option<&CallableContext<'_>>,
    diagnostics: &mut Vec<SirDiagnostic>,
) {
    let Some(callable_context) = callable_context else {
        // A context-free verifier cannot know whether a legal direct call is
        // unit-returning, but it can still enforce the initial 0-or-1 result
        // representation and the operand-use discipline above.
        return;
    };
    let Some(target) = callable_context.callable(callee) else {
        diagnostics.push(diag(
            function,
            SirDiagnosticKind::UnknownCallable { op: id, callee },
        ));
        return;
    };
    // An actor method is a direct callee only from a body of the same actor,
    // which lends its own state seat exclusively as the first argument.
    let admitted = match target.kind {
        SemCallableKind::HewDirect => true,
        SemCallableKind::HewClosure => false,
        SemCallableKind::HewActor(actor) => {
            callable_context
                .actors
                .get(actor.0 as usize)
                .is_some_and(|descriptor| {
                    descriptor.methods.contains(&callee)
                        && descriptor.bodies().any(|body| body == function.callable)
                })
                && args.first().is_some_and(|seat| {
                    seat.decision == crate::BoundaryDecision::BorrowMut
                        && function.params.first().map(|param| param.value)
                            == Some(seat.operand.value)
                })
        }
    };
    if target.call_conv != SemCallConv::Default || !admitted {
        invalid_operation(
            function,
            id,
            "direct call targets a callable outside SIR's default HewDirect ABI domain".to_string(),
            diagnostics,
        );
    }
    let never = target.signature.return_ty == ResolvedTy::Never;
    if normal.is_none() != never || matches!(result, crate::CallResult::Never) != never {
        invalid_operation(
            function,
            id,
            "direct call result and normal edge differ from its return type".to_string(),
            diagnostics,
        );
    }
    let expected_results = usize::from(!matches!(
        target.signature.return_ty,
        ResolvedTy::Unit | ResolvedTy::Never
    ));
    let actual_results = usize::from(matches!(result, crate::CallResult::Value(_)));
    if actual_results != expected_results {
        diagnostics.push(diag(
            function,
            SirDiagnosticKind::InvalidCallResultArity {
                op: id,
                callee,
                expected: expected_results,
                actual: actual_results,
            },
        ));
    } else if let crate::CallResult::Value(result) = result {
        if result.ty != target.signature.return_ty {
            invalid_operation(
                function,
                id,
                format!(
                    "direct call result has `{}`, callee `{}` returns `{}`",
                    result.ty.user_facing(),
                    target.declaration.full_path(),
                    target.signature.return_ty.user_facing()
                ),
                diagnostics,
            );
        }
    }
    if args.len() != target.signature.params.len() {
        invalid_operation(
            function,
            id,
            format!(
                "direct call to `{}` has {} argument(s), expected {}",
                target.declaration.full_path(),
                args.len(),
                target.signature.params.len()
            ),
            diagnostics,
        );
    }
    for (index, (argument, parameter)) in args.iter().zip(&target.signature.params).enumerate() {
        let expected_decision = match parameter.passing {
            SemParamPassing::ReadOnly => crate::BoundaryDecision::Copy,
            SemParamPassing::Borrow => crate::BoundaryDecision::Borrow,
            SemParamPassing::BorrowMut => crate::BoundaryDecision::BorrowMut,
            SemParamPassing::Consume => crate::BoundaryDecision::Move,
        };
        if argument.decision != expected_decision {
            invalid_operation(
                function,
                id,
                format!(
                    "direct call argument {index} to `{}` is {:?}, expected {:?} for {:?} parameter passing",
                    target.declaration.full_path(),
                    argument.decision,
                    expected_decision,
                    parameter.passing
                ),
                diagnostics,
            );
        }
        if let Some(actual) = types.get(&argument.operand.value) {
            if actual != &parameter.ty {
                invalid_operation(
                    function,
                    id,
                    format!(
                        "direct call argument {index} to `{}` has `{}`, expected `{}`",
                        target.declaration.full_path(),
                        actual.user_facing(),
                        parameter.ty.user_facing()
                    ),
                    diagnostics,
                );
            }
        }
    }
}

#[allow(
    clippy::too_many_arguments,
    clippy::too_many_lines,
    reason = "one closed runtime relation boundary checks arguments, result and failure CFG"
)]
fn verify_runtime_call_terminator(
    function: &SemFunction,
    id: OpId,
    family: hew_types::RuntimeCallFamily,
    args: &[crate::BoundaryOperand],
    result: &crate::CallResult,
    normal: &crate::Edge,
    unwind: &crate::CallUnwind,
    types: &HashMap<ValueId, ResolvedTy>,
    blocks: &BTreeMap<BlockId, &crate::SemBlock>,
    shapes: &VariantVerifyContext<'_>,
    diagnostics: &mut Vec<SirDiagnostic>,
) {
    use hew_types::{RuntimeArgumentEffect, RuntimeResultEffect};

    let Some(contract) = family.semantic_contract() else {
        invalid_operation(
            function,
            id,
            format!("runtime family `{family:?}` has no admitted semantic contract"),
            diagnostics,
        );
        return;
    };
    let parameter_types = args
        .iter()
        .map(|argument| types.get(&argument.operand.value).cloned())
        .collect::<Option<Vec<_>>>();
    let Some(parameter_types) = parameter_types else {
        return;
    };
    let result_ty = match result {
        crate::CallResult::Unit => ResolvedTy::Unit,
        crate::CallResult::Never => ResolvedTy::Never,
        crate::CallResult::Value(value) => value.ty.clone(),
    };
    let instantiated = match contract.instantiate(&parameter_types, &result_ty) {
        Ok(contract) => contract,
        Err(reason) => {
            invalid_operation(function, id, reason, diagnostics);
            return;
        }
    };
    for (index, (argument, expected)) in args.iter().zip(contract.arguments).enumerate() {
        let Some(facts) = shapes
            .facts
            .get(&hew_types::TypeInstanceKey(parameter_types[index].clone()))
        else {
            continue;
        };
        let expected_decision = match expected.effect.resolve(facts.clone) {
            RuntimeArgumentEffect::Value => unreachable!("value ingress was resolved"),
            RuntimeArgumentEffect::Borrow => crate::BoundaryDecision::Borrow,
            RuntimeArgumentEffect::Copy => crate::BoundaryDecision::Copy,
            RuntimeArgumentEffect::Move => crate::BoundaryDecision::Move,
        };
        if argument.decision != expected_decision {
            invalid_operation(
                function,
                id,
                format!(
                    "runtime family `{family:?}` argument {index} has {:?} boundary, expected {expected_decision:?}",
                    argument.decision
                ),
                diagnostics,
            );
        }
    }

    let expected_own = match contract.result {
        RuntimeResultEffect::Unit | RuntimeResultEffect::Never => None,
        RuntimeResultEffect::BitCopy(_) => Some(crate::OwnKind::None),
        RuntimeResultEffect::FreshOwned(_)
        | RuntimeResultEffect::UpdatedReceiver(_)
        | RuntimeResultEffect::FreshOwnedVariant(_)
        | RuntimeResultEffect::UpdatedReceiverAndValue(_) => Some(crate::OwnKind::Owned),
        RuntimeResultEffect::IndependentValue(_) => {
            if shapes
                .facts
                .get(&hew_types::TypeInstanceKey(instantiated.result_ty.clone()))
                .is_some_and(|facts| facts.clone == hew_types::CloneKind::None)
            {
                invalid_operation(
                    function,
                    id,
                    "runtime read has no semantic copy for its result".into(),
                    diagnostics,
                );
                return;
            }
            match crate::OwnKind::of_ty(&instantiated.result_ty, shapes.facts) {
                Ok(own) => Some(own),
                Err(reason) => {
                    invalid_operation(function, id, reason, diagnostics);
                    return;
                }
            }
        }
    };
    match (expected_own, result) {
        (None, crate::CallResult::Never)
            if matches!(contract.result, RuntimeResultEffect::Never) =>
        {
            // The call never returns, so its mandatory normal edge is only
            // a structural continuation: the lifetime pass does not follow it.
            let unreachable_target = normal.args.is_empty()
                && blocks.get(&normal.target).is_some_and(|block| {
                    matches!(block.terminator, crate::SemTerminator::Unreachable)
                });
            if !unreachable_target {
                invalid_operation(
                    function,
                    id,
                    format!(
                        "runtime family `{family:?}` never returns; its normal edge must be an argument-free unreachable block"
                    ),
                    diagnostics,
                );
            }
        }
        (None, crate::CallResult::Unit) if matches!(contract.result, RuntimeResultEffect::Unit) => {
            if !normal.args.is_empty() {
                invalid_operation(
                    function,
                    id,
                    "unit runtime call forwards a normal-edge value".to_string(),
                    diagnostics,
                );
            }
        }
        (Some(own), crate::CallResult::Value(value)) => {
            if value.ty != instantiated.result_ty || value.own != own {
                invalid_operation(
                    function,
                    id,
                    format!(
                        "runtime family `{family:?}` result is `{}`/{:?}, expected `{}`/{own:?}",
                        value.ty.user_facing(),
                        value.own,
                        instantiated.result_ty.user_facing()
                    ),
                    diagnostics,
                );
            }
            if let RuntimeResultEffect::FreshOwnedVariant(kind) = contract.result {
                if let Err(reason) = crate::runtime_variant_shape_refs(
                    kind,
                    &value.ty,
                    shapes.aggregate_shapes,
                    shapes.shapes,
                ) {
                    invalid_operation(function, id, reason, diagnostics);
                }
            }
            let forwarded = normal
                .args
                .iter()
                .filter(|operand| operand.value == value.id)
                .count();
            if forwarded != 1 {
                invalid_operation(
                    function,
                    id,
                    format!(
                        "runtime family `{family:?}` result must be forwarded exactly once on its normal edge, found {forwarded}"
                    ),
                    diagnostics,
                );
            }
        }
        _ => invalid_operation(
            function,
            id,
            format!("runtime family `{family:?}` result shape disagrees with its contract"),
            diagnostics,
        ),
    }

    match (contract.failures, unwind) {
        ([], crate::CallUnwind::NotApplicable) => {}
        (failures, crate::CallUnwind::Cleanup(edge))
            if !failures.is_empty() && (failures.len() == 1 || contract.propagates_fault()) =>
        {
            let expected = if contract.propagates_fault() {
                None
            } else {
                crate::runtime_failure_trap_kind(failures[0])
            };
            if !failure_cfg_matches_exit(edge, expected, blocks) {
                diagnostics.push(diag(
                    function,
                    SirDiagnosticKind::InvalidTerminator {
                        reason: format!(
                            "runtime family `{family:?}` failure edge does not end only in {expected:?}"
                        ),
                    },
                ));
            }
        }
        _ => diagnostics.push(diag(
            function,
            SirDiagnosticKind::InvalidTerminator {
                reason: format!(
                    "runtime family `{family:?}` unwind shape disagrees with its exact failure set"
                ),
            },
        )),
    }
}

fn verify_value_call_terminator(
    function: &SemFunction,
    terminator: &SemTerminator,
    types: &HashMap<ValueId, ResolvedTy>,
    blocks: &BTreeMap<BlockId, &crate::SemBlock>,
    diagnostics: &mut Vec<SirDiagnostic>,
) {
    let SemTerminator::ValueCall {
        id,
        ty,
        capability,
        args,
        result,
        unwind,
        ..
    } = terminator
    else {
        unreachable!("selected value call verifier requires a value call");
    };
    let (arity, result_ty) = match capability {
        hew_types::ValueCapability::Hash => (1, ResolvedTy::I64),
        hew_types::ValueCapability::Eq => (2, ResolvedTy::Bool),
    };
    if args.len() != arity
        || args.iter().any(|arg| {
            arg.decision != crate::BoundaryDecision::Borrow
                || types.get(&arg.operand.value) != Some(ty)
        })
    {
        invalid_operation(
            function,
            *id,
            format!("selected {capability:?} requires {arity} borrowed operands of its exact type"),
            diagnostics,
        );
    }
    if !matches!(result, crate::CallResult::Value(value) if value.ty == result_ty && value.own == crate::OwnKind::None)
    {
        invalid_operation(
            function,
            *id,
            format!("selected {capability:?} requires its scalar {result_ty:?} result"),
            diagnostics,
        );
    }
    if !matches!(unwind, crate::CallUnwind::Cleanup(edge) if failure_cfg_matches_exit(edge, None, blocks))
    {
        invalid_operation(
            function,
            *id,
            "selected value call requires cleanup that propagates the original fault".into(),
            diagnostics,
        );
    }
}

fn invalid_operation(
    function: &SemFunction,
    op: OpId,
    reason: String,
    diagnostics: &mut Vec<SirDiagnostic>,
) {
    diagnostics.push(diag(
        function,
        SirDiagnosticKind::InvalidOperation { op, reason },
    ));
}

#[allow(
    clippy::too_many_arguments,
    reason = "checked arithmetic keeps its type, result visibility and exact failure CFG contract together"
)]
fn verify_checked_binary_terminator(
    function: &SemFunction,
    id: OpId,
    op: hew_parser::ast::BinaryOp,
    lhs: &crate::Operand,
    rhs: &crate::Operand,
    result: &crate::ValueDef,
    normal: &crate::Edge,
    failures: &[crate::CheckedFailure],
    types: &HashMap<ValueId, ResolvedTy>,
    blocks: &BTreeMap<BlockId, &crate::SemBlock>,
    diagnostics: &mut Vec<SirDiagnostic>,
) {
    let (Some(lhs_ty), Some(rhs_ty)) = (types.get(&lhs.value), types.get(&rhs.value)) else {
        return;
    };
    if !crate::checked_binary_types_match(op, lhs_ty, rhs_ty, &result.ty) {
        diagnostics.push(diag(
            function,
            SirDiagnosticKind::InvalidTerminator {
                reason: format!(
                    "checked binary `{op}` has incompatible `{}`, `{}` -> `{}` types",
                    lhs_ty.user_facing(),
                    rhs_ty.user_facing(),
                    result.ty.user_facing()
                ),
            },
        ));
        return;
    }
    let Some(required) = crate::checked_binary_failure_kinds(op, &result.ty) else {
        diagnostics.push(diag(
            function,
            SirDiagnosticKind::InvalidTerminator {
                reason: format!(
                    "binary `{op}` over `{}` is not checked integer arithmetic",
                    lhs_ty.user_facing()
                ),
            },
        ));
        return;
    };
    let actual: Vec<_> = failures.iter().map(|failure| failure.kind).collect();
    if actual != required {
        diagnostics.push(diag(
            function,
            SirDiagnosticKind::InvalidTerminator {
                reason: format!(
                    "checked binary `{op}` has failure kinds {actual:?}, expected {required:?}"
                ),
            },
        ));
    }
    let forwarded = normal
        .args
        .iter()
        .filter(|operand| operand.value == result.id)
        .count();
    if forwarded != 1 {
        invalid_operation(
            function,
            id,
            format!(
                "checked binary result must be forwarded exactly once on its normal edge, found {forwarded}"
            ),
            diagnostics,
        );
    }
    for failure in failures {
        if !failure_cfg_matches_exit(&failure.edge, Some(failure.kind), blocks) {
            diagnostics.push(diag(
                function,
                SirDiagnosticKind::InvalidTerminator {
                    reason: format!(
                        "checked binary `{op}` failure {:?} does not end only in a matching trap",
                        failure.kind
                    ),
                },
            ));
        }
    }
}

pub(crate) fn defer_drain_suffix(
    block: BlockId,
    blocks: &BTreeMap<BlockId, &crate::SemBlock>,
    visiting: &mut BTreeSet<BlockId>,
) -> bool {
    if !visiting.insert(block) {
        return false;
    }
    let Some(body) = blocks.get(&block) else {
        return false;
    };
    if body.ops.iter().any(|op| {
        !matches!(
            op.kind,
            SemOpKind::EndBorrow { .. }
                | SemOpKind::DestroyValue { .. }
                | SemOpKind::EndLifetime { .. }
        )
    }) {
        return false;
    }
    let valid = match &body.terminator {
        SemTerminator::EnterDefer { .. }
        | SemTerminator::FinishDefer { .. }
        | SemTerminator::CleanupDispatch { .. }
        | SemTerminator::RecoverFault { .. } => true,
        SemTerminator::Suspend {
            kind: crate::SuspendKind::ValueClose { .. },
            resumes,
            cancel,
            unwind,
            ..
        } => resumes
            .iter()
            .chain([cancel, unwind])
            .all(|edge| defer_drain_suffix(edge.target, blocks, visiting)),
        SemTerminator::Goto(edge) => defer_drain_suffix(edge.target, blocks, visiting),
        SemTerminator::Branch {
            then_target,
            else_target,
            ..
        } => {
            defer_drain_suffix(then_target.target, blocks, visiting)
                && defer_drain_suffix(else_target.target, blocks, visiting)
        }
        _ => false,
    };
    visiting.remove(&block);
    valid
}

fn checked_raise_origin(
    target: BlockId,
    kind: crate::TrapKind,
    function: &SemFunction,
    visiting: &mut BTreeSet<BlockId>,
) -> bool {
    if target == function.entry || !visiting.insert(target) {
        return false;
    }
    let mut found = false;
    for block in &function.blocks {
        let mut incoming = Vec::new();
        block.terminator.visit_successors_with_slots(|slot, edge| {
            if edge.target == target {
                incoming.push(slot);
            }
        });
        for slot in incoming {
            found = true;
            let valid = match &block.terminator {
                SemTerminator::CheckedBinary { failures, .. } => {
                    slot.0 > 0
                        && failures
                            .get((slot.0 - 1) as usize)
                            .is_some_and(|failure| failure.kind == kind)
                }
                SemTerminator::RtCall {
                    family,
                    unwind: crate::CallUnwind::Cleanup(_),
                    ..
                } => {
                    slot.0 == 1
                        && family.semantic_contract().is_some_and(|contract| {
                            contract.failures.iter().all(|failure| {
                                crate::runtime_failure_trap_kind(*failure) == Some(kind)
                            }) && !contract.failures.is_empty()
                        })
                }
                SemTerminator::Goto(_) | SemTerminator::Branch { .. } => {
                    block.ops.iter().all(|op| {
                        matches!(
                            op.kind,
                            SemOpKind::EndBorrow { .. }
                                | SemOpKind::DestroyValue { .. }
                                | SemOpKind::EndLifetime { .. }
                        )
                    }) && checked_raise_origin(block.id, kind, function, visiting)
                }
                _ => false,
            };
            if !valid {
                return false;
            }
        }
    }
    visiting.remove(&target);
    found
}

#[expect(
    clippy::too_many_lines,
    reason = "recursive fault-cleanup proof covers each permitted terminator"
)]
fn failure_cfg_matches_exit(
    edge: &crate::Edge,
    expected: Option<crate::TrapKind>,
    blocks: &BTreeMap<BlockId, &crate::SemBlock>,
) -> bool {
    fn reaches_only_matching_exits(
        block_id: BlockId,
        expected: Option<crate::TrapKind>,
        blocks: &BTreeMap<BlockId, &crate::SemBlock>,
        visiting: &mut std::collections::HashSet<BlockId>,
        complete: &mut std::collections::HashSet<BlockId>,
    ) -> bool {
        if complete.contains(&block_id) {
            return true;
        }
        if !visiting.insert(block_id) {
            return false;
        }
        let Some(block) = blocks.get(&block_id) else {
            return false;
        };
        if block.ops.iter().any(|op| {
            !matches!(
                op.kind,
                SemOpKind::EndBorrow { .. }
                    | SemOpKind::TaskScopeClose { .. }
                    | SemOpKind::DestroyValue { .. }
                    | SemOpKind::EndLifetime { .. }
            )
        }) {
            return false;
        }
        let valid = match &block.terminator {
            SemTerminator::Suspend {
                kind:
                    crate::SuspendKind::Join {
                        mode:
                            crate::TaskScopeJoinMode::PropagateFault
                            | crate::TaskScopeJoinMode::CancelLosersAfterFault,
                        ..
                    }
                    | crate::SuspendKind::ValueClose { .. },
                resumes,
                cancel,
                unwind,
                ..
            } => {
                expected.is_none()
                    && resumes.iter().chain([cancel, unwind]).all(|edge| {
                        reaches_only_matching_exits(edge.target, None, blocks, visiting, complete)
                    })
            }
            SemTerminator::EnterDefer { .. }
            | SemTerminator::FinishDefer { .. }
            | SemTerminator::RecoverFault { .. }
            | SemTerminator::ResumeUnwind => expected.is_none(),
            SemTerminator::CheckedRaiseFault { kind, .. } => expected == Some(*kind),
            SemTerminator::CleanupDispatch { fault, .. } => {
                expected.is_none()
                    && reaches_only_matching_exits(fault.target, None, blocks, visiting, complete)
            }
            SemTerminator::Trap { kind } => Some(*kind) == expected,
            SemTerminator::Goto(next) => {
                reaches_only_matching_exits(next.target, expected, blocks, visiting, complete)
            }
            SemTerminator::Branch {
                then_target,
                else_target,
                ..
            } => {
                reaches_only_matching_exits(
                    then_target.target,
                    expected,
                    blocks,
                    visiting,
                    complete,
                ) && reaches_only_matching_exits(
                    else_target.target,
                    expected,
                    blocks,
                    visiting,
                    complete,
                )
            }
            SemTerminator::Return { .. }
            | SemTerminator::Panic { .. }
            | SemTerminator::CheckedBinary { .. }
            | SemTerminator::SwitchVariant { .. }
            | SemTerminator::Call { .. }
            | SemTerminator::RtCall { .. }
            | SemTerminator::ActorCall { .. }
            | SemTerminator::ValueCall { .. }
            | SemTerminator::IndirectCall { .. }
            | SemTerminator::Suspend { .. }
            | SemTerminator::Unreachable => false,
        };
        visiting.remove(&block_id);
        if valid {
            complete.insert(block_id);
        }
        valid
    }

    reaches_only_matching_exits(
        edge.target,
        expected,
        blocks,
        &mut std::collections::HashSet::new(),
        &mut std::collections::HashSet::new(),
    )
}

struct VariantVerifyContext<'a> {
    facts: &'a TypeFactTable,
    aggregate_shapes: &'a [SemAggregateShape],
    shapes: &'a [SemVariantShape],
}

fn verify_variant_switch_terminator(
    function: &SemFunction,
    terminator: &SemTerminator,
    types: &HashMap<ValueId, ResolvedTy>,
    context: &VariantVerifyContext<'_>,
    diagnostics: &mut Vec<SirDiagnostic>,
) {
    let SemTerminator::SwitchVariant {
        id,
        shape,
        scrutinee,
        arms,
    } = terminator
    else {
        unreachable!("variant-switch verifier requires a variant-switch terminator");
    };
    let Some(enum_ty) = types.get(&scrutinee.value) else {
        return;
    };
    let Some(descriptor) = usize::try_from(shape.0)
        .ok()
        .and_then(|index| context.shapes.get(index))
        .filter(|descriptor| descriptor.id == *shape)
    else {
        diagnostics.push(diag(
            function,
            SirDiagnosticKind::InvalidTerminator {
                reason: format!("variant shape {} is missing or non-canonical", shape.0),
            },
        ));
        return;
    };
    if &descriptor.enum_ty != enum_ty {
        diagnostics.push(diag(
            function,
            SirDiagnosticKind::InvalidTerminator {
                reason: format!(
                    "variant shape {} describes `{}`, not scrutinee `{}`",
                    shape.0,
                    descriptor.enum_ty.user_facing(),
                    enum_ty.user_facing()
                ),
            },
        ));
    }
    if crate::OwnKind::of_ty(enum_ty, context.facts).is_err() {
        diagnostics.push(diag(
            function,
            SirDiagnosticKind::InvalidTerminator {
                reason: format!(
                    "variant switch scrutinee `{}` has no exact ownership facts",
                    enum_ty.user_facing()
                ),
            },
        ));
    }
    if arms.len() != descriptor.variants.len() {
        diagnostics.push(diag(
            function,
            SirDiagnosticKind::InvalidTerminator {
                reason: format!(
                    "variant switch has {} arm(s), descriptor requires {}",
                    arms.len(),
                    descriptor.variants.len()
                ),
            },
        ));
    }
    let mut seen = HashSet::new();
    for arm in arms {
        if !seen.insert(arm.variant) {
            diagnostics.push(diag(
                function,
                SirDiagnosticKind::InvalidTerminator {
                    reason: format!("variant switch repeats arm {}", arm.variant),
                },
            ));
        }
        verify_variant_switch_arm(function, *id, *shape, enum_ty, arm, context, diagnostics);
    }
    for variant in 0..descriptor.variants.len() {
        let variant = u32::try_from(variant).expect("verified variant count exceeds u32");
        if !seen.contains(&variant) {
            diagnostics.push(diag(
                function,
                SirDiagnosticKind::InvalidTerminator {
                    reason: format!("variant switch is missing arm {variant}"),
                },
            ));
        }
    }
}

fn verify_variant_switch_arm(
    function: &SemFunction,
    id: OpId,
    shape: VariantShapeId,
    enum_ty: &ResolvedTy,
    arm: &crate::SemVariantArm,
    context: &VariantVerifyContext<'_>,
    diagnostics: &mut Vec<SirDiagnostic>,
) {
    let recipes = match crate::variant_field_recipes(
        shape,
        arm.variant,
        enum_ty,
        context.shapes,
        context.facts,
    ) {
        Ok(recipes) => recipes,
        Err(reason) => {
            diagnostics.push(diag(
                function,
                SirDiagnosticKind::InvalidTerminator { reason },
            ));
            return;
        }
    };
    if arm.fields.len() != recipes.len() {
        diagnostics.push(diag(
            function,
            SirDiagnosticKind::InvalidTerminator {
                reason: format!(
                    "variant arm {} defines {} field(s), descriptor requires {}",
                    arm.variant,
                    arm.fields.len(),
                    recipes.len()
                ),
            },
        ));
    }
    for (index, (field, recipe)) in arm.fields.iter().zip(&recipes).enumerate() {
        if field.ty != recipe.ty {
            diagnostics.push(diag(
                function,
                SirDiagnosticKind::InvalidTerminator {
                    reason: format!(
                        "variant arm {} field {index} has `{}`, expected `{}`",
                        arm.variant,
                        field.ty.user_facing(),
                        recipe.ty.user_facing()
                    ),
                },
            ));
        }
    }
    if arm.target.args.len() != arm.fields.len()
        || arm
            .target
            .args
            .iter()
            .zip(&arm.fields)
            .any(|(argument, field)| argument.value != field.id)
    {
        invalid_operation(
            function,
            id,
            format!(
                "variant arm {} must forward every defined field exactly once and in declaration order",
                arm.variant
            ),
            diagnostics,
        );
    }
}

#[allow(
    clippy::too_many_lines,
    reason = "the verifier keeps the closed terminator dispatch visibly exhaustive"
)]
fn verify_terminator_shape(
    function: &SemFunction,
    block: &crate::SemBlock,
    types: &HashMap<ValueId, ResolvedTy>,
    blocks: &BTreeMap<BlockId, &crate::SemBlock>,
    callable_context: Option<&CallableContext<'_>>,
    variants: &VariantVerifyContext<'_>,
    diagnostics: &mut Vec<SirDiagnostic>,
) {
    let terminator = &block.terminator;
    match terminator {
        SemTerminator::RecoverFault {
            result,
            deadline_variant,
            fault_variant,
            unwind,
            ..
        } => {
            let valid = result.own == OwnKind::Owned
                && variants.shapes.iter().any(|shape| {
                    shape.enum_ty == result.ty
                        && !shape.is_indirect
                        && shape.variants.len() == 2
                        && [(*deadline_variant, "Deadline"), (*fault_variant, "Fault")]
                            .iter()
                            .all(|(tag, name)| {
                                shape.variants.get(*tag as usize).is_some_and(|variant| {
                                    variant.name == *name
                                        && variant.fields.len() == 1
                                        && variant.fields[0].ty == ResolvedTy::String
                                })
                            })
                });
            if !valid || !failure_cfg_matches_exit(unwind, None, blocks) {
                diagnostics.push(diag(function, SirDiagnosticKind::InvalidTerminator { reason: "scope recovery requires owned Deadline/Fault string variants and a propagating cancellation edge".into() }));
            }
        }

        SemTerminator::ActorCall {
            id,
            operation,
            args,
            result,
            unwind,
            ..
        } => {
            let check = (|| {
                let context =
                    callable_context.ok_or("actor boundary requires its module contracts")?;
                let signature = operation.signature(context.actors, |id| {
                    context
                        .callable(id)
                        .map(|callable| callable.signature.clone())
                })?;
                if args.len() != signature.params.len()
                    || args.iter().zip(&signature.params).any(|(arg, param)| {
                        arg.decision != crate::BoundaryDecision::Move
                            || types.get(&arg.operand.value) != Some(&param.ty)
                    })
                {
                    return Err(
                        "actor boundary must transfer its complete typed payload".to_string()
                    );
                }
                match result {
                    crate::CallResult::Unit if signature.return_ty == ResolvedTy::Unit => {}
                    crate::CallResult::Value(value)
                        if value.ty == signature.return_ty
                            && value.own == OwnKind::of_ty(&value.ty, variants.facts)? => {}
                    _ => return Err("actor boundary result differs from its protocol".into()),
                }
                if !matches!(unwind, crate::CallUnwind::Cleanup(edge) if failure_cfg_matches_exit(edge, None, blocks))
                {
                    return Err("actor boundary requires cleanup that propagates its fault".into());
                }
                Ok(())
            })();
            if let Err(reason) = check {
                invalid_operation(function, *id, reason, diagnostics);
            }
        }
        SemTerminator::FinishDefer { next, .. } => {
            if !defer_drain_suffix(next.target, blocks, &mut BTreeSet::new()) {
                diagnostics.push(diag(
                    function,
                    SirDiagnosticKind::InvalidTerminator {
                        reason:
                            "defer finish must advance through finite cleanup to the next boundary"
                                .into(),
                    },
                ));
            }
        }
        SemTerminator::CleanupDispatch { fault, .. } => {
            if !failure_cfg_matches_exit(fault, None, blocks) {
                diagnostics.push(diag(
                    function,
                    SirDiagnosticKind::InvalidTerminator {
                        reason: "cleanup dispatch fault edge must continue bounded fault cleanup"
                            .into(),
                    },
                ));
            }
        }
        SemTerminator::CheckedRaiseFault { kind, cleanup } => {
            if !checked_raise_origin(block.id, *kind, function, &mut BTreeSet::new()) {
                diagnostics.push(diag(
                    function,
                    SirDiagnosticKind::InvalidTerminator {
                        reason: "checked raise does not match its producing failure edge".into(),
                    },
                ));
            }
            if !failure_cfg_matches_exit(cleanup, None, blocks) {
                diagnostics.push(diag(function, SirDiagnosticKind::InvalidTerminator { reason: "checked raise requires bounded cleanup preserving its materialized fault".into() }));
            }
        }
        SemTerminator::Return { value: Some(value) } if function.return_ty == ResolvedTy::Unit => {
            diagnostics.push(diag(
                function,
                SirDiagnosticKind::UnitReturnValue {
                    value: value.operand.value,
                },
            ));
        }
        SemTerminator::Return { value: Some(value) } => {
            if let Some(actual) = types.get(&value.operand.value) {
                if actual != &function.return_ty {
                    diagnostics.push(diag(
                        function,
                        SirDiagnosticKind::ReturnType {
                            expected: function.return_ty.user_facing().to_string(),
                            actual: Some(actual.user_facing().to_string()),
                        },
                    ));
                }
            }
        }
        SemTerminator::Return { value: None } if function.return_ty != ResolvedTy::Unit => {
            diagnostics.push(diag(
                function,
                SirDiagnosticKind::ReturnType {
                    expected: function.return_ty.user_facing().to_string(),
                    actual: None,
                },
            ));
        }
        SemTerminator::Branch { condition, .. } => {
            if let Some(actual) = types.get(&condition.value) {
                if actual != &ResolvedTy::Bool {
                    diagnostics.push(diag(
                        function,
                        SirDiagnosticKind::BranchConditionType {
                            value: condition.value,
                            actual: actual.user_facing().to_string(),
                        },
                    ));
                }
            }
        }
        switch @ SemTerminator::SwitchVariant { .. } => {
            verify_variant_switch_terminator(function, switch, types, variants, diagnostics);
        }
        SemTerminator::Call {
            id,
            callee,
            args,
            result,
            normal,
            ..
        } => verify_direct_call_terminator(
            function,
            *id,
            *callee,
            args,
            result,
            normal.as_ref(),
            types,
            callable_context,
            diagnostics,
        ),
        SemTerminator::CheckedBinary {
            id,
            op,
            lhs,
            rhs,
            result,
            normal,
            failures,
        } => verify_checked_binary_terminator(
            function,
            *id,
            *op,
            lhs,
            rhs,
            result,
            normal,
            failures,
            types,
            blocks,
            diagnostics,
        ),
        call @ SemTerminator::IndirectCall { id, .. } => {
            if let Err(reason) = verify_indirect_call(
                function,
                call,
                types,
                variants.facts,
                blocks,
                callable_context,
            ) {
                invalid_operation(function, *id, reason, diagnostics);
            }
        }
        call @ SemTerminator::ValueCall { .. } => {
            verify_value_call_terminator(function, call, types, blocks, diagnostics);
        }
        SemTerminator::Panic { message, cleanup } => {
            if types.get(&message.operand.value) != Some(&ResolvedTy::String)
                || message.decision != crate::BoundaryDecision::Borrow
            {
                diagnostics.push(diag(
                    function,
                    SirDiagnosticKind::InvalidTerminator {
                        reason: "panic requires one borrowed String message".into(),
                    },
                ));
            }
            if !failure_cfg_matches_exit(cleanup, None, blocks)
                || crate::lifetime::cleanup_suffixes(function).get(&cleanup.target) != Some(&0)
            {
                diagnostics.push(diag(
                    function,
                    SirDiagnosticKind::InvalidTerminator {
                        reason: "panic requires finite cleanup that propagates its original fault"
                            .into(),
                    },
                ));
            }
        }
        SemTerminator::RtCall {
            id,
            family,
            args,
            result,
            normal,
            unwind,
        } => verify_runtime_call_terminator(
            function,
            *id,
            *family,
            args,
            result,
            normal,
            unwind,
            types,
            blocks,
            variants,
            diagnostics,
        ),
        SemTerminator::EnterDefer { .. }
        | SemTerminator::Return { .. }
        | SemTerminator::Goto(_)
        | SemTerminator::Trap { .. }
        | SemTerminator::ResumeUnwind
        | SemTerminator::Unreachable => {}
        // `Trap`'s kind table and `Suspend`'s shape rules - §1.5's kind/arity/
        // mode agreement and the cancel-edge and resume-edge orderings -
        // belong to the phase that emits one, and neither has a producer on
        // this route. An unverified terminator is refused for the same reason
        // an unverified operation is: admitting it would let a shape nothing
        // checks reach MIR. This is the operation arm's refusal, not a new
        // ownership rule.
        SemTerminator::Suspend {
            kind,
            inputs,
            result,
            resumes,
            cancel,
            unwind,
        } => {
            let valid = match kind {
                crate::SuspendKind::Ask { actor, message, .. } => {
                    callable_context.and_then(|context| context.actors.get(actor.0 as usize))
                        .filter(|descriptor| descriptor.id == *actor)
                        .and_then(|descriptor| descriptor.ask_signature(*message).ok())
                        .is_some_and(|signature| {
                            resumes.len() == 1
                                && inputs.len() == signature.params.len()
                                && inputs.iter().zip(&signature.params).all(|(input, parameter)| {
                                    input.decision == crate::BoundaryDecision::Move
                                        && types.get(&input.operand.value) == Some(&parameter.ty)
                                })
                                && matches!(result, crate::CallResult::Value(value) if value.ty == signature.return_ty)
                        })
                }
                crate::SuspendKind::NativeIo { operation } => {
                    let argument_types = inputs
                        .iter()
                        .map(|input| types.get(&input.operand.value).cloned())
                        .collect::<Option<Vec<_>>>();
                    resumes.len() == 1
                        && inputs
                            .iter()
                            .all(|input| input.decision == crate::BoundaryDecision::Borrow)
                        && matches!(result, crate::CallResult::Value(value)
                            if argument_types.is_some_and(|arguments| operation.contract().matches_signature(&arguments, &value.ty))
                                && OwnKind::of_ty(&value.ty, variants.facts) == Ok(value.own))
                }
                crate::SuspendKind::Sleep => {
                    resumes.len() == 1
                        && matches!(result, crate::CallResult::Unit)
                        && matches!(inputs.as_slice(), [input]
                        if input.decision == crate::BoundaryDecision::Copy
                        && types.get(&input.operand.value) == Some(&ResolvedTy::Duration))
                }
                crate::SuspendKind::Await => {
                    resumes.len() == usize::from(!matches!(result, crate::CallResult::Never))
                        && matches!(inputs.as_slice(), [input]
                        if input.decision == crate::BoundaryDecision::Move
                        && matches!(types.get(&input.operand.value), Some(ResolvedTy::Task(output))
                            if match result {
                                crate::CallResult::Unit => **output == ResolvedTy::Unit,
                                crate::CallResult::Never => **output == ResolvedTy::Never,
                                crate::CallResult::Value(value) => value.ty == **output
                                    && value.ty != ResolvedTy::Never,
                            }))
                }
                crate::SuspendKind::Yield => {
                    let yielded = callable_context
                        .and_then(|context| {
                            context
                                .closures
                                .iter()
                                .find(|closure| closure.body == function.callable)
                        })
                        .and_then(|closure| closure.generator_yield.as_ref());
                    resumes.len() == 1
                        && matches!(result, crate::CallResult::Unit)
                        && matches!(inputs.as_slice(), [input]
                            if input.decision == crate::BoundaryDecision::Move
                            && types.get(&input.operand.value) == yielded)
                        && yielded.is_some()
                }
                crate::SuspendKind::GeneratorNext => {
                    resumes.len() == 1
                        && matches!(inputs.as_slice(), [input]
                        if input.decision == crate::BoundaryDecision::BorrowMut
                        && types.get(&input.operand.value).and_then(crate::generator_parts)
                            .is_some_and(|(yielded, _)| matches!(result, crate::CallResult::Value(value)
                                if value.ty == ResolvedTy::named_builtin("Option", hew_types::BuiltinType::Option, vec![yielded.clone()]))))
                }
                crate::SuspendKind::StreamNext => {
                    resumes.len() == 1
                        && matches!(inputs.as_slice(), [input]
                        if input.decision == crate::BoundaryDecision::BorrowMut
                        && types.get(&input.operand.value).and_then(crate::stream_element)
                            .is_some_and(|element| matches!(result, crate::CallResult::Value(value)
                                if value.ty == ResolvedTy::named_builtin("Option", hew_types::BuiltinType::Option, vec![element.clone()])
                                    && OwnKind::of_ty(&value.ty, variants.facts) == Ok(value.own))))
                }
                crate::SuspendKind::StreamSend => {
                    // Only a stream producer body sends: its owned sink is
                    // lent and the element transfers to the consumer.
                    let element = callable_context.and_then(|context| {
                        context.actors.iter().flat_map(|actor| &actor.handlers)
                            .find(|handler| handler.callable == function.callable)
                            .and_then(|handler| handler.stream.as_ref())
                    });
                    resumes.len() == 2
                        && matches!(result, crate::CallResult::Unit)
                        && matches!(inputs.as_slice(), [sink, value]
                            if sink.decision == crate::BoundaryDecision::Borrow
                            && value.decision == crate::BoundaryDecision::Move
                            && element.is_some()
                            && types.get(&sink.operand.value).and_then(crate::sink_element) == element
                            && types.get(&value.operand.value) == element)
                }
                crate::SuspendKind::ValueClose { place, selection } => {
                    resumes.len() == 1
                        && resumes.first() == Some(cancel)
                        && resumes.first() == Some(unwind)
                        && matches!(result, crate::CallResult::Unit)
                        && if let Some(place) = place {
                            *selection == crate::ValueCloseSelection::Whole
                                && inputs.is_empty()
                                && function.places.iter().any(|declaration| {
                                    declaration.id == *place
                                        && matches!(declaration.origin, crate::PlaceOrigin::Local | crate::PlaceOrigin::Aggregate { .. })
                                        && crate::OwnKind::of_ty(&declaration.ty, variants.facts)
                                            .ok()
                                            == Some(crate::OwnKind::Owned)
                                })
                        } else {
                            let expected_len = if *selection == crate::ValueCloseSelection::Whole {
                                1
                            } else {
                                2
                            };
                            inputs.len() == expected_len
                                    && inputs[0].decision == crate::BoundaryDecision::Borrow
                                    && types.get(&inputs[0].operand.value).is_some_and(|ty| {
                                        crate::OwnKind::of_ty(ty, variants.facts).ok() == Some(crate::OwnKind::Owned)
                                            && (*selection == crate::ValueCloseSelection::Whole
                                                || matches!(ty, ResolvedTy::Named { builtin: Some(hew_types::BuiltinType::Vec), args, .. } if args.len() == 1))
                                    })
                                    && (*selection == crate::ValueCloseSelection::Whole
                                        || (inputs[1].decision == crate::BoundaryDecision::Copy
                                            && types.get(&inputs[1].operand.value) == Some(&ResolvedTy::I64)))
                        }
                }
                crate::SuspendKind::Join { .. } => {
                    inputs.is_empty()
                        && resumes.len() == 1
                        && matches!(result, crate::CallResult::Unit)
                }
                crate::SuspendKind::Select { has_timeout, .. } => {
                    let tasks = if *has_timeout {
                        inputs.split_last().and_then(|(duration, tasks)| {
                            (duration.decision == crate::BoundaryDecision::Copy
                                && types.get(&duration.operand.value)
                                    == Some(&ResolvedTy::Duration))
                            .then_some(tasks)
                        })
                    } else {
                        Some(inputs.as_slice())
                    };
                    resumes.len() == 1
                        && matches!(result, crate::CallResult::Value(value)
                            if value.ty == ResolvedTy::I64 && value.own == crate::OwnKind::None)
                        && tasks.is_some_and(|tasks| {
                            (*has_timeout || !tasks.is_empty())
                                && tasks.iter().all(|input| {
                                    input.decision == crate::BoundaryDecision::Borrow
                                        && matches!(
                                            types.get(&input.operand.value),
                                            Some(ResolvedTy::Task(_))
                                        )
                                })
                        })
                }
                _ => false,
            };
            if !valid {
                diagnostics.push(diag(
                    function,
                    SirDiagnosticKind::InvalidTerminator {
                        reason: format!(
                            "{kind:?} suspension has no matching input/result/resume contract"
                        ),
                    },
                ));
            }
        }
    }
}

#[derive(Clone, Copy)]
enum DefinitionPoint {
    BlockEntry,
    Operation(usize),
    NormalEdge,
}

#[allow(clippy::too_many_arguments, reason = "small verifier transfer helper")]
fn verify_uses(
    function: &SemFunction,
    dominators: &crate::Dominators,
    definitions: &HashMap<ValueId, (BlockId, DefinitionPoint)>,
    use_block: BlockId,
    use_index: Option<usize>,
    uses: Vec<(ValueId, bool)>,
    diagnostics: &mut Vec<SirDiagnostic>,
) {
    for (value, on_call_normal_edge) in uses {
        let Some((definition, definition_index)) = definitions.get(&value) else {
            diagnostics.push(diag(function, SirDiagnosticKind::UndefinedValue(value)));
            continue;
        };
        if matches!(definition_index, DefinitionPoint::NormalEdge) {
            if definition != &use_block || !on_call_normal_edge {
                diagnostics.push(diag(
                    function,
                    SirDiagnosticKind::InvalidCallResultUse {
                        value,
                        definition: *definition,
                        use_block,
                    },
                ));
            }
            continue;
        }
        if definition == &use_block {
            if let (DefinitionPoint::Operation(definition_index), Some(use_index)) =
                (definition_index, use_index)
            {
                if definition_index >= &use_index {
                    diagnostics.push(diag(
                        function,
                        SirDiagnosticKind::UseBeforeDefinition {
                            value,
                            block: use_block,
                        },
                    ));
                }
            }
            continue;
        }
        if !dominators
            .sets
            .get(&use_block)
            .is_some_and(|set| set.contains(definition))
        {
            diagnostics.push(diag(
                function,
                SirDiagnosticKind::NonDominatingUse {
                    value,
                    definition: *definition,
                    use_block,
                },
            ));
        }
    }
}

fn record_value(
    function: &SemFunction,
    value: ValueId,
    values: &mut HashSet<ValueId>,
    diagnostics: &mut Vec<SirDiagnostic>,
) {
    if !values.insert(value) {
        diagnostics.push(diag(function, SirDiagnosticKind::DuplicateValue(value)));
    }
}

/// §1.2: a value's ownership kind is decided by one rule, and this reads that
/// rule's answer back off the definition.
///
/// `expected` comes from the same derivation the lowering used —
/// [`OwnKind::of_param`] for a parameter, whose header slot decides before its
/// type's class does, `Guaranteed` for a local borrow producer, and
/// [`OwnKind::of_ty`] for every other definition.
/// Without the audit `own` is a free field the lowering writes and nothing
/// reads, so an `i64` could present as `Owned`, and a `Guaranteed` could ride
/// on a value no borrow produced. A type neither the fact table nor §1.1 can
/// decide is refused for the same reason the lowering refuses it: there is no
/// default kind.
///
fn verify_own_kind(
    function: &SemFunction,
    value: ValueId,
    ty: &ResolvedTy,
    own: crate::OwnKind,
    expected: Result<crate::OwnKind, String>,
    diagnostics: &mut Vec<SirDiagnostic>,
) {
    match expected {
        Ok(expected) if expected == own => {}
        Ok(expected) => diagnostics.push(diag(
            function,
            SirDiagnosticKind::OwnershipKind {
                value,
                reason: format!(
                    "value is declared {own:?} but the class of `{}` gives it {expected:?}",
                    ty.user_facing()
                ),
            },
        )),
        Err(error) => diagnostics.push(diag(
            function,
            SirDiagnosticKind::OwnershipKind {
                value,
                reason: error,
            },
        )),
    }
}

fn diag(function: &SemFunction, kind: SirDiagnosticKind) -> SirDiagnostic {
    SirDiagnostic {
        function: function.name.clone(),
        callable: Some(function.callable),
        kind,
    }
}

fn module_diag(kind: SirDiagnosticKind) -> SirDiagnostic {
    SirDiagnostic {
        function: "<module>".to_string(),
        callable: None,
        kind,
    }
}

fn uses_in_op(function: &SemFunction, op: &crate::SemOp) -> Vec<(ValueId, bool)> {
    let mut uses = Vec::new();
    op.visit_operands(|_, operand| uses.push((operand.value, false)));
    op.kind.visit_places(|id| {
        if let Some(crate::PlaceDecl {
            origin:
                crate::PlaceOrigin::Capture { environment, .. }
                | crate::PlaceOrigin::ActorState {
                    state: environment, ..
                },
            ..
        }) = function.places.iter().find(|place| place.id == id)
        {
            uses.push((*environment, false));
        } else if let Ok((crate::OwnerRoot::Value(root), _)) =
            crate::projection::place_path(&function.places, id)
        {
            uses.push((root, false));
        }
    });
    uses
}

fn uses_in_terminator(term: &SemTerminator) -> Vec<(ValueId, bool)> {
    let mut uses = Vec::new();
    // The canonical visitor orders control inputs before normal-edge
    // arguments. Only that interval can see the terminator result.
    let normal_slots = match term {
        SemTerminator::Call { args, normal, .. } => {
            args.len()..args.len() + normal.as_ref().map_or(0, |edge| edge.args.len())
        }
        SemTerminator::RtCall { args, normal, .. }
        | SemTerminator::ActorCall { args, normal, .. }
        | SemTerminator::ValueCall { args, normal, .. } => {
            args.len()..args.len() + normal.args.len()
        }
        SemTerminator::IndirectCall { args, normal, .. } => {
            let start = 1 + args.len();
            start..start + normal.as_ref().map_or(0, |edge| edge.args.len())
        }
        SemTerminator::CheckedBinary { normal, .. } => 2..2 + normal.args.len(),
        SemTerminator::RecoverFault { normal, .. } => 0..normal.args.len(),
        SemTerminator::Suspend {
            inputs, resumes, ..
        } => inputs.len()..inputs.len() + resumes.first().map_or(0, |edge| edge.args.len()),
        SemTerminator::SwitchVariant { arms, .. } => {
            let end = 1 + arms.iter().map(|arm| arm.target.args.len()).sum::<usize>();
            1..end
        }
        _ => 0..0,
    };
    term.visit_operands(|slot, operand| {
        uses.push((operand.value, normal_slots.contains(&(slot.0 as usize))));
    });
    uses
}

#[cfg(test)]
mod cfg_discard_safety_tests {
    use super::{verify_cfg_discard_safety, CfgDiscardSafetyReason, SirDiagnosticKind};
    use crate::ownership::OwnKind;
    use crate::{
        BlockArg, BlockId, CallableId, Edge, FunctionSourceOrigin, OpId, Operand, Provenance,
        SemBlock, SemFunction, SemOp, SemOpKind, SemTerminator, UseSite, ValueId,
    };
    use hew_hir::ItemId;
    use hew_types::{DefId, ResolvedTy};

    fn operand(value: u32) -> Operand {
        Operand {
            value: ValueId(value),
        }
    }

    fn returned(value: u32) -> crate::BoundaryOperand {
        crate::BoundaryOperand {
            operand: operand(value),
            decision: crate::BoundaryDecision::Move,
        }
    }

    fn param(value: u32, ty: ResolvedTy) -> BlockArg {
        BlockArg {
            value: ValueId(value),
            ty,
            own: OwnKind::None,
        }
    }

    /// A discarded block whose ops discharge an ownership obligation is unsafe
    /// to discard: the obligation would never be consumed on the surviving
    /// path. The obligation is named by the operation now, not by a mode on the
    /// operand it reads.
    #[test]
    fn records_a_discarded_ownership_discharge_as_a_drop_obligation() {
        let original = SemFunction {
            id: ItemId(0),
            callable: CallableId(0),
            declaration: DefId::for_test("discarded_drop_obligation"),
            name: "discarded_drop_obligation".to_string(),
            span: 0..0,
            source_origin: FunctionSourceOrigin::Unknown,
            params: vec![param(0, ResolvedTy::Bool), param(1, ResolvedTy::I64)],
            return_ty: ResolvedTy::I64,
            entry: BlockId(0),
            places: Vec::new(),
            bindings: Vec::new(),
            blocks: vec![
                SemBlock {
                    id: BlockId(0),
                    args: Vec::new(),
                    ops: Vec::new(),
                    terminator: SemTerminator::Branch {
                        condition: operand(0),
                        then_target: Edge {
                            target: BlockId(1),
                            args: Vec::new(),
                        },
                        else_target: Edge {
                            target: BlockId(2),
                            args: Vec::new(),
                        },
                    },
                },
                SemBlock {
                    id: BlockId(1),
                    args: Vec::new(),
                    ops: Vec::new(),
                    terminator: SemTerminator::Return {
                        value: Some(returned(1)),
                    },
                },
                SemBlock {
                    id: BlockId(2),
                    args: Vec::new(),
                    ops: vec![SemOp {
                        id: OpId(0),
                        results: Vec::new(),
                        kind: SemOpKind::DestroyValue { value: operand(1) },
                        provenance: Provenance::Synthesized,
                    }],
                    terminator: SemTerminator::Return {
                        value: Some(returned(1)),
                    },
                },
            ],
        };
        let mut rewritten = original.clone();
        rewritten.blocks[0].terminator = SemTerminator::Goto(Edge {
            target: BlockId(1),
            args: Vec::new(),
        });

        let diagnostics = verify_cfg_discard_safety(&original, &rewritten);
        assert!(diagnostics.iter().any(|diagnostic| matches!(
            &diagnostic.kind,
            SirDiagnosticKind::UnsafeCfgDiscard {
                block: BlockId(2),
                reason: CfgDiscardSafetyReason::DropObligationUse {
                    site: UseSite::Operation { op: OpId(0), .. }
                }
            }
        )));
    }

    /// The counterfactual: a discarded block whose ops read their operands
    /// without transferring an obligation is not reported for one.
    #[test]
    fn a_discarded_pure_block_is_not_a_drop_obligation() {
        let original = SemFunction {
            id: ItemId(0),
            callable: CallableId(0),
            declaration: DefId::for_test("discarded_pure_block"),
            name: "discarded_pure_block".to_string(),
            span: 0..0,
            source_origin: FunctionSourceOrigin::Unknown,
            params: vec![param(0, ResolvedTy::Bool), param(1, ResolvedTy::I64)],
            return_ty: ResolvedTy::I64,
            entry: BlockId(0),
            places: Vec::new(),
            bindings: Vec::new(),
            blocks: vec![
                SemBlock {
                    id: BlockId(0),
                    args: Vec::new(),
                    ops: Vec::new(),
                    terminator: SemTerminator::Branch {
                        condition: operand(0),
                        then_target: Edge {
                            target: BlockId(1),
                            args: Vec::new(),
                        },
                        else_target: Edge {
                            target: BlockId(2),
                            args: Vec::new(),
                        },
                    },
                },
                SemBlock {
                    id: BlockId(1),
                    args: Vec::new(),
                    ops: Vec::new(),
                    terminator: SemTerminator::Return {
                        value: Some(returned(1)),
                    },
                },
                SemBlock {
                    id: BlockId(2),
                    args: Vec::new(),
                    ops: vec![SemOp {
                        id: OpId(0),
                        results: vec![crate::ValueDef {
                            id: ValueId(2),
                            ty: ResolvedTy::I64,
                            own: OwnKind::None,
                        }],
                        kind: SemOpKind::ConstI64(7),
                        provenance: Provenance::Synthesized,
                    }],
                    terminator: SemTerminator::Return {
                        value: Some(returned(2)),
                    },
                },
            ],
        };
        let mut rewritten = original.clone();
        rewritten.blocks[0].terminator = SemTerminator::Goto(Edge {
            target: BlockId(1),
            args: Vec::new(),
        });

        let diagnostics = verify_cfg_discard_safety(&original, &rewritten);
        assert!(!diagnostics.iter().any(|diagnostic| matches!(
            &diagnostic.kind,
            SirDiagnosticKind::UnsafeCfgDiscard {
                reason: CfgDiscardSafetyReason::DropObligationUse { .. },
                ..
            }
        )));
    }
}

#[cfg(test)]
mod parameter_own_kind_tests {
    use super::{callable_context, verify_function_with_context, SirDiagnosticKind};
    use crate::ownership::{OwnKind, TypeFactTable};
    use crate::{
        BlockArg, BlockId, CallableId, CallableInstance, FunctionSourceOrigin, SemAbiParam,
        SemBlock, SemCallConv, SemCallable, SemCallableKind, SemFunction, SemParamPassing,
        SemSignature, SemTerminator, ValueId,
    };
    use hew_hir::ItemId;
    use hew_types::{DefId, ResolvedTy, TypeFactContext, TypeFactService};

    fn function(ty: ResolvedTy, own: OwnKind) -> SemFunction {
        SemFunction {
            id: ItemId(0),
            callable: CallableId(0),
            declaration: DefId::for_test("takes_one_parameter"),
            name: "takes_one_parameter".to_string(),
            span: 0..0,
            source_origin: FunctionSourceOrigin::Unknown,
            params: vec![BlockArg {
                value: ValueId(0),
                ty,
                own,
            }],
            return_ty: ResolvedTy::Unit,
            entry: BlockId(0),
            places: Vec::new(),
            bindings: Vec::new(),
            blocks: vec![SemBlock {
                id: BlockId(0),
                args: Vec::new(),
                ops: Vec::new(),
                terminator: SemTerminator::Return { value: None },
            }],
        }
    }

    fn callable(function: &SemFunction, passing: SemParamPassing) -> SemCallable {
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
                        passing,
                        caller_visible_projection: false,
                    })
                    .collect(),
                return_ty: function.return_ty.clone(),
            },
            call_conv: SemCallConv::Default,
            kind: SemCallableKind::HewDirect,
        }
    }

    /// Is there a §1.2 ownership-kind finding about the one parameter?
    fn kind_finding(diagnostics: &[crate::SirDiagnostic]) -> Option<&str> {
        diagnostics
            .iter()
            .find_map(|diagnostic| match &diagnostic.kind {
                SirDiagnosticKind::OwnershipKind { value, reason } if *value == ValueId(0) => {
                    Some(reason.as_str())
                }
                _ => None,
            })
    }

    /// §1.2 rule 3: a `Borrow` header slot makes the parameter `Guaranteed` for
    /// the whole body whatever its type's class says. A `string` in that slot
    /// carrying the class's `Owned` contradicts the slot, and the audit says so.
    #[test]
    fn verifier_refuses_a_borrow_slot_parameter_the_class_kind_contradicts() {
        let function = function(ResolvedTy::String, OwnKind::Owned);
        let callables = vec![callable(&function, SemParamPassing::Borrow)];
        let context = callable_context(&callables, &[], &[]);
        let diagnostics = verify_function_with_context(
            &function,
            Some(&context),
            &TypeFactTable::new(),
            &[],
            &[],
        );
        let reason = kind_finding(&diagnostics).expect("a Borrow slot refuses an Owned parameter");
        assert!(reason.contains("Guaranteed"), "{reason}");
    }

    /// The counterfactual: the same parameter in the same slot, carrying the
    /// kind rule 3 gives it, is admitted. The finding above is about the slot
    /// disagreement and not about `Borrow` being unrepresentable.
    #[test]
    fn verifier_admits_a_borrow_slot_parameter_that_is_guaranteed() {
        let function = function(ResolvedTy::String, OwnKind::Guaranteed);
        let callables = vec![callable(&function, SemParamPassing::Borrow)];
        let context = callable_context(&callables, &[], &[]);
        let diagnostics = verify_function_with_context(
            &function,
            Some(&context),
            &TypeFactTable::new(),
            &[],
            &[],
        );
        assert_eq!(None, kind_finding(&diagnostics));
    }

    /// The same body in a `ReadOnly` slot takes the class table's answer, so
    /// `Guaranteed` is the wrong kind there. Rule 3 reads the slot rather than
    /// making `Guaranteed` always acceptable on a parameter.
    #[test]
    fn verifier_refuses_a_read_only_slot_parameter_that_claims_guaranteed() {
        let function = function(ResolvedTy::String, OwnKind::Guaranteed);
        let callables = vec![callable(&function, SemParamPassing::ReadOnly)];
        let context = callable_context(&callables, &[], &[]);
        let mut facts = TypeFactService::new(TypeFactContext::default(), TypeFactTable::new());
        facts.require(&ResolvedTy::String).unwrap();
        let diagnostics =
            verify_function_with_context(&function, Some(&context), facts.rows(), &[], &[]);
        let reason =
            kind_finding(&diagnostics).expect("a ReadOnly slot refuses a Guaranteed parameter");
        assert!(reason.contains("Owned"), "{reason}");
    }

    /// The negative control for the deleted `ReadOnly` default: with no callable
    /// table there is no header slot, so rule 3 has no ABI fact to audit
    /// against and the parameter is refused. Defaulting to `ReadOnly` admitted
    /// this `i64` silently, and would have admitted a `Borrow` slot's
    /// `Guaranteed` parameter as `Owned`.
    #[test]
    fn verifier_refuses_a_parameter_whose_header_slot_it_cannot_read() {
        let function = function(ResolvedTy::I64, OwnKind::None);
        let diagnostics =
            verify_function_with_context(&function, None, &TypeFactTable::new(), &[], &[]);
        let reason =
            kind_finding(&diagnostics).expect("no callable table means no slot to audit against");
        assert!(reason.contains("no header slot"), "{reason}");
    }
}

#[cfg(test)]
mod binding_table_tests {
    use super::{verify_function, SirDiagnosticKind};
    use crate::ownership::{Binding, BindingId, BindingTarget};
    use crate::{
        BlockId, CallableId, FunctionSourceOrigin, SemBlock, SemFunction, SemTerminator, ValueId,
    };
    use hew_hir::ItemId;
    use hew_types::{DefId, ResolvedTy};

    fn function(bindings: Vec<Binding>) -> SemFunction {
        SemFunction {
            id: ItemId(0),
            callable: CallableId(0),
            declaration: DefId::for_test("named"),
            name: "named".to_string(),
            span: 0..0,
            source_origin: FunctionSourceOrigin::Unknown,
            params: Vec::new(),
            return_ty: ResolvedTy::Unit,
            entry: BlockId(0),
            places: Vec::new(),
            bindings,
            blocks: vec![SemBlock {
                id: BlockId(0),
                args: Vec::new(),
                ops: Vec::new(),
                terminator: SemTerminator::Return { value: None },
            }],
        }
    }

    fn binding(name: &str, value: u32) -> Binding {
        Binding {
            id: BindingId(0),
            name: name.to_string(),
            span: 0..0,
            mutable: false,
            target: BindingTarget::Value(ValueId(value)),
        }
    }

    /// §1.6 reads the binding table to give a wall the user's own name for the
    /// value it refuses. A row naming a value this body never defines cannot be
    /// resolved, so the wall would silently lose the name; the verifier refuses
    /// the row instead.
    #[test]
    fn verifier_refuses_a_binding_naming_a_value_the_body_never_defines() {
        let diagnostics = verify_function(&function(vec![binding("ghost", 7)]));
        assert!(
            diagnostics.iter().any(|diagnostic| matches!(
                &diagnostic.kind,
                SirDiagnosticKind::UnknownBinding { name, target }
                    if name == "ghost" && *target == BindingTarget::Value(ValueId(7))
            )),
            "{diagnostics:#?}"
        );
    }

    /// The counterfactual: an empty table raises nothing, so the rule is about
    /// the unresolvable row and not about carrying bindings at all.
    #[test]
    fn verifier_admits_a_body_whose_binding_table_is_empty() {
        let diagnostics = verify_function(&function(Vec::new()));
        assert!(
            !diagnostics.iter().any(|diagnostic| matches!(
                diagnostic.kind,
                SirDiagnosticKind::UnknownBinding { .. }
            )),
            "{diagnostics:#?}"
        );
    }
}

#[cfg(test)]
mod defer_close_suffix_tests {
    use super::{defer_drain_suffix, BTreeSet, BlockId, SemTerminator};
    use crate::Edge;

    #[test]
    fn value_close_requires_finite_cleanup_on_every_successor() {
        let edge = |id| Edge {
            target: BlockId(id),
            args: Vec::new(),
        };
        let close = |resume, cancel, unwind| SemTerminator::Suspend {
            kind: crate::SuspendKind::ValueClose {
                place: Some(crate::PlaceId(0)),
                selection: crate::ValueCloseSelection::Whole,
            },
            inputs: Vec::new(),
            result: crate::CallResult::Unit,
            resumes: vec![edge(resume)],
            cancel: edge(cancel),
            unwind: edge(unwind),
        };
        let check = |terminator| {
            let blocks = [
                crate::SemBlock {
                    id: BlockId(0),
                    args: Vec::new(),
                    ops: Vec::new(),
                    terminator,
                },
                crate::SemBlock {
                    id: BlockId(1),
                    args: Vec::new(),
                    ops: Vec::new(),
                    terminator: SemTerminator::CleanupDispatch {
                        normal: edge(2),
                        fault: edge(2),
                    },
                },
                crate::SemBlock {
                    id: BlockId(2),
                    args: Vec::new(),
                    ops: Vec::new(),
                    terminator: SemTerminator::Unreachable,
                },
            ];
            let map = blocks.iter().map(|block| (block.id, block)).collect();
            defer_drain_suffix(BlockId(0), &map, &mut BTreeSet::new())
        };
        assert!(check(close(1, 1, 1)));
        for exits in [
            (0, 1, 1),
            (1, 0, 1),
            (1, 1, 0),
            (2, 1, 1),
            (1, 2, 1),
            (1, 1, 2),
        ] {
            assert!(
                !check(close(exits.0, exits.1, exits.2)),
                "invalid cleanup exits {exits:?}"
            );
        }
        let mut ordinary = close(1, 1, 1);
        if let SemTerminator::Suspend { kind, .. } = &mut ordinary {
            *kind = crate::SuspendKind::Yield;
        }
        assert!(
            !check(ordinary),
            "ordinary suspension cannot become cleanup"
        );
    }
}
