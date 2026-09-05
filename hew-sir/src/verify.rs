use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

use crate::ownership::TypeFactTable;
use crate::OpId;
use crate::{
    AggregateShapeId, AggregateShapeRef, BindingTarget, BlockId, CallableId, CallableInstance,
    GenericTemplateId, SemAggregateShape, SemCallConv, SemCallable, SemCallableKind, SemFunction,
    SemGenericTemplate, SemModule, SemOp, SemOpKind, SemParamPassing, SemSignature, SemTerminator,
    SemVariantShape, SirInstanceKey, UseSite, ValueId, VariantShapeId,
};
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
}

/// Index an already-verified module's callable table.
///
/// [`verify_callable_table`] both validates and indexes; a pass that has
/// already run [`verify_module`] over the same callables needs only the index,
/// and building it here lets that pass hold the table while it mutates the
/// module's bodies.
pub(crate) fn callable_context(callables: &[SemCallable]) -> CallableContext<'_> {
    CallableContext {
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
        if shape.variants.is_empty() {
            refuse("variant descriptor has no variants".to_string());
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

#[must_use]
pub fn verify_module(module: &SemModule) -> Vec<SirDiagnostic> {
    let mut diagnostics = Vec::new();
    let callables = verify_callable_table(module, &mut diagnostics);
    verify_aggregate_shapes(module, &mut diagnostics);
    verify_variant_shapes(module, &mut diagnostics);
    let mut names = HashSet::new();
    let mut declarations = HashSet::new();
    for function in &module.functions {
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
        diagnostics.extend(verify_function_with_context(
            function,
            Some(&callables),
            &module.type_facts,
            &module.aggregate_shapes,
            &module.variant_shapes,
        ));
    }
    diagnostics
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
    diagnostics.extend(verify_function_with_context(
        function,
        Some(&callables),
        &module.type_facts,
        &module.aggregate_shapes,
        &module.variant_shapes,
    ));
    diagnostics
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
            if operation.kind.effects().may_trap() {
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
            SemTerminator::CheckedBinary { .. }
                | SemTerminator::SwitchVariant { .. }
                | SemTerminator::Trap { .. }
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

#[allow(
    clippy::too_many_lines,
    reason = "the verifier keeps SSA collection, CFG shape, and dominance checks together so the stage boundary is auditable"
)]
pub(crate) fn verify_function_with_context(
    function: &SemFunction,
    callable_context: Option<&CallableContext<'_>>,
    facts: &TypeFactTable,
    aggregate_shapes: &[SemAggregateShape],
    variant_shapes: &[SemVariantShape],
) -> Vec<SirDiagnostic> {
    let mut diagnostics = Vec::new();
    verify_function_callable_identity(function, callable_context, &mut diagnostics);
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
                    crate::OwnKind::of_ty(&result.ty, facts),
                    &mut diagnostics,
                );
                types.insert(result.id, result.ty.clone());
                definitions.insert(result.id, (block.id, DefinitionPoint::Operation(op_index)));
            }
        }
        if let SemTerminator::Call { id, .. }
        | SemTerminator::RtCall { id, .. }
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
    // Every value type is known before checking operations, edges, and
    // terminators. In particular this catches a malformed use whose value is
    // defined in a later block rather than silently skipping its type check.
    let variants = VariantVerifyContext {
        facts,
        shapes: variant_shapes,
    };
    for block in &function.blocks {
        for op in &block.ops {
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
            &block.terminator,
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
                    uses_in_op(op),
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
    diagnostics.extend(
        crate::lifetime::verify(function)
            .into_iter()
            .map(|violation| {
                diag(
                    function,
                    SirDiagnosticKind::OwnershipLifetime {
                        block: violation.block,
                        value: violation.value,
                        reason: violation.reason,
                    },
                )
            }),
    );
    diagnostics
}

#[allow(
    clippy::too_many_lines,
    reason = "callable identity, signature, and root-entry invariants share one auditable module boundary"
)]
fn verify_callable_table<'a>(
    module: &'a SemModule,
    diagnostics: &mut Vec<SirDiagnostic>,
) -> CallableContext<'a> {
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
        if callable.kind != SemCallableKind::HewDirect {
            diagnostics.push(module_diag(SirDiagnosticKind::InvalidCallable {
                callable: callable.id,
                reason: "initial SIR callable table admits only ordinary HewDirect bodies"
                    .to_string(),
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
            let expected_passing = match crate::OwnKind::of_ty(&abi.ty, &module.type_facts) {
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
            Some(callable) if !matches!(callable.instance, CallableInstance::Monomorphic) => {
                diagnostics.push(module_diag(SirDiagnosticKind::InvalidEntryCallable {
                    callable: entry,
                    reason:
                        "entry callable must be a monomorphic source body, not a generic instance"
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
    CallableContext { by_id }
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
            if parameter.passing != SemParamPassing::ReadOnly || parameter.caller_visible_projection
            {
                diagnostics.push(module_diag(SirDiagnosticKind::InvalidGenericTemplate {
                    template: name.clone(),
                    reason: format!(
                        "template parameter {index} carries ownership or caller-visible ABI policy before SIR owns it"
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
/// The initial generic slice intentionally accepts only scalar concrete type
/// arguments, but the key still records them as `ResolvedTy` rather than any
/// representation property.  This makes malformed or residual generic SIR
/// fail here, before raw MIR is allowed to choose storage or ABI details.
fn verify_generic_callable_instance(
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
        if !is_initial_scalar(argument) {
            diagnostics.push(module_diag(SirDiagnosticKind::InvalidCallable {
                callable: callable.id,
                reason: format!(
                    "generic semantic type argument {index} `{}` is outside the initial scalar SIR instance surface",
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
    let expected_signature = substitute_template_signature(template, &key.type_args);
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
) -> SemSignature {
    SemSignature {
        params: template
            .signature
            .params
            .iter()
            .map(|parameter| crate::SemAbiParam {
                ty: substitute_type_params(&parameter.ty, &template.type_params, type_args),
                passing: parameter.passing,
                caller_visible_projection: parameter.caller_visible_projection,
            })
            .collect(),
        return_ty: substitute_type_params(
            &template.signature.return_ty,
            &template.type_params,
            type_args,
        ),
    }
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

fn is_initial_scalar(ty: &ResolvedTy) -> bool {
    ty.is_integer() || matches!(ty, ResolvedTy::Bool | ResolvedTy::F64 | ResolvedTy::Char)
}

fn is_initial_call_value(ty: &ResolvedTy) -> bool {
    is_initial_scalar(ty) || matches!(ty, ResolvedTy::String | ResolvedTy::Bytes)
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
            if !elements.is_empty() && elements.iter().all(is_initial_value_type))
}

fn is_supported_call_value(module: &SemModule, ty: &ResolvedTy) -> bool {
    is_initial_call_value(ty)
        || matches!(ty, ResolvedTy::Tuple(fields) if !fields.is_empty())
        || module.aggregate_shape_for_type(ty).is_some()
        || module.variant_shape_for_type(ty).is_some()
}

fn is_supported_call_return(module: &SemModule, ty: &ResolvedTy) -> bool {
    matches!(ty, ResolvedTy::Unit) || is_supported_call_value(module, ty)
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
    if let SemOpKind::Destructure { shape, aggregate } = &operation.kind {
        let Some(aggregate_ty) = types.get(&aggregate.value) else {
            return;
        };
        let recipes =
            match crate::aggregate_field_recipes(*shape, aggregate_ty, aggregate_shapes, facts) {
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
                    "aggregate.destructure operand `{}` has no exact ownership facts",
                    aggregate_ty.user_facing()
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
                        "aggregate.destructure result {index} has `{}`, expected `{}`",
                        result.ty.user_facing(),
                        recipe.ty.user_facing()
                    ),
                    diagnostics,
                );
            }
        }
        return;
    }
    let expected_results = usize::from(!matches!(
        operation.kind,
        SemOpKind::DestroyValue { .. }
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
        } => {
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
                        "aggregate.project_copy operand `{}` has no exact ownership facts",
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
                        "aggregate.project_copy field {field} is out of bounds for `{}` with {} field(s)",
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
                        "aggregate.project_copy field {field} produces `{}`, expected `{}`",
                        result.ty.user_facing(),
                        recipe.ty.user_facing()
                    ),
                    diagnostics,
                );
            }
            if recipe.clone == hew_types::CloneKind::None {
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
                    lhs_ty == rhs_ty && lhs_ty == &result.ty && !lhs_ty.is_integer()
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
        SemOpKind::ConstI64(_)
        | SemOpKind::ConstBool(_)
        | SemOpKind::ConstF64(_)
        | SemOpKind::ConstChar(_)
        | SemOpKind::ConstStr(_)
        | SemOpKind::ConstBytes(_) => {}
        // Dormant operations remain fail-closed until their producer and
        // complete semantic validation land together.
        SemOpKind::ConstUnit
        | SemOpKind::ConstDuration(_)
        | SemOpKind::StrEq { .. }
        | SemOpKind::BytesEq { .. }
        | SemOpKind::BeginBorrow { .. }
        | SemOpKind::EndBorrow { .. }
        | SemOpKind::DestroyValue { .. }
        | SemOpKind::Fork { .. }
        | SemOpKind::Destructure { .. }
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
    if target.call_conv != SemCallConv::Default || target.kind != SemCallableKind::HewDirect {
        invalid_operation(
            function,
            id,
            "direct call targets a callable outside SIR's default HewDirect ABI domain".to_string(),
            diagnostics,
        );
    }
    let expected_results = usize::from(target.signature.return_ty != ResolvedTy::Unit);
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
    if args.len() != contract.arguments.len() {
        invalid_operation(
            function,
            id,
            format!(
                "runtime family `{family:?}` has {} argument(s), expected {}",
                args.len(),
                contract.arguments.len()
            ),
            diagnostics,
        );
    }
    for (index, (argument, expected)) in args.iter().zip(contract.arguments).enumerate() {
        if let Some(actual) = types.get(&argument.operand.value) {
            if !expected.ty.matches(actual) {
                invalid_operation(
                    function,
                    id,
                    format!(
                        "runtime family `{family:?}` argument {index} has `{}`, expected {:?}",
                        actual.user_facing(),
                        expected.ty
                    ),
                    diagnostics,
                );
            }
        }
        let expected_decision = match expected.effect {
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

    let expected_result = match contract.result {
        RuntimeResultEffect::Unit => None,
        RuntimeResultEffect::BitCopy(kind) => Some((kind, crate::OwnKind::None)),
        RuntimeResultEffect::FreshOwned(kind) | RuntimeResultEffect::UpdatedReceiver(kind) => {
            Some((kind, crate::OwnKind::Owned))
        }
    };
    match (expected_result, result) {
        (None, crate::CallResult::Unit) => {
            if !normal.args.is_empty() {
                invalid_operation(
                    function,
                    id,
                    "unit runtime call forwards a normal-edge value".to_string(),
                    diagnostics,
                );
            }
        }
        (Some((kind, own)), crate::CallResult::Value(value)) => {
            let expected_ty = kind.resolved_ty();
            if value.ty != expected_ty || value.own != own {
                invalid_operation(
                    function,
                    id,
                    format!(
                        "runtime family `{family:?}` result is `{}`/{:?}, expected `{}`/{own:?}",
                        value.ty.user_facing(),
                        value.own,
                        expected_ty.user_facing()
                    ),
                    diagnostics,
                );
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
        ([failure], crate::CallUnwind::Cleanup(edge)) => {
            let expected = crate::runtime_failure_trap_kind(*failure);
            if !failure_cfg_matches_trap(edge, expected, blocks) {
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
    if lhs_ty != rhs_ty || lhs_ty != &result.ty {
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
    let Some(required) = crate::checked_binary_failure_kinds(op, lhs_ty) else {
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
        if !failure_cfg_matches_trap(&failure.edge, failure.kind, blocks) {
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

fn failure_cfg_matches_trap(
    edge: &crate::Edge,
    expected: crate::TrapKind,
    blocks: &BTreeMap<BlockId, &crate::SemBlock>,
) -> bool {
    fn reaches_only_matching_traps(
        block_id: BlockId,
        expected: crate::TrapKind,
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
        let valid = match &block.terminator {
            SemTerminator::Trap { kind } => *kind == expected,
            SemTerminator::Goto(next) => {
                reaches_only_matching_traps(next.target, expected, blocks, visiting, complete)
            }
            SemTerminator::Branch {
                then_target,
                else_target,
                ..
            } => {
                reaches_only_matching_traps(
                    then_target.target,
                    expected,
                    blocks,
                    visiting,
                    complete,
                ) && reaches_only_matching_traps(
                    else_target.target,
                    expected,
                    blocks,
                    visiting,
                    complete,
                )
            }
            SemTerminator::Return { .. }
            | SemTerminator::CheckedBinary { .. }
            | SemTerminator::SwitchVariant { .. }
            | SemTerminator::Call { .. }
            | SemTerminator::RtCall { .. }
            | SemTerminator::Suspend { .. }
            | SemTerminator::ResumeUnwind
            | SemTerminator::Unreachable => false,
        };
        visiting.remove(&block_id);
        if valid {
            complete.insert(block_id);
        }
        valid
    }

    reaches_only_matching_traps(
        edge.target,
        expected,
        blocks,
        &mut std::collections::HashSet::new(),
        &mut std::collections::HashSet::new(),
    )
}

struct VariantVerifyContext<'a> {
    facts: &'a TypeFactTable,
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
    terminator: &SemTerminator,
    types: &HashMap<ValueId, ResolvedTy>,
    blocks: &BTreeMap<BlockId, &crate::SemBlock>,
    callable_context: Option<&CallableContext<'_>>,
    variants: &VariantVerifyContext<'_>,
    diagnostics: &mut Vec<SirDiagnostic>,
) {
    match terminator {
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
            ..
        } => verify_direct_call_terminator(
            function,
            *id,
            *callee,
            args,
            result,
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
            diagnostics,
        ),
        SemTerminator::Return { .. }
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
        SemTerminator::Suspend { .. } => {
            diagnostics.push(diag(
                function,
                SirDiagnosticKind::InvalidTerminator {
                    reason: "terminator is outside the verified SIR relation table".to_string(),
                },
            ));
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
/// type's class does, and [`OwnKind::of_ty`] for every other definition.
/// Without the audit `own` is a free field the lowering writes and nothing
/// reads, so an `i64` could present as `Owned`, and a `Guaranteed` could ride
/// on a value no borrow produced. A type neither the fact table nor §1.1 can
/// decide is refused for the same reason the lowering refuses it: there is no
/// default kind.
///
/// MARKED SHORTCUT - a `begin_borrow` result is not yet a case here.
/// WHY: `Guaranteed` has two producers under §1.2 - a borrow-slot parameter,
/// which this reads, and a `begin_borrow` result, which no phase emits on this
/// route.
/// WHEN: `begin_borrow` lands (L2).
/// WHAT: a `begin_borrow` result is `Guaranteed` whatever its type's class
/// says, so the derivation branches on the defining operation as well as on
/// the header slot.
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

fn uses_in_op(op: &crate::SemOp) -> Vec<(ValueId, bool)> {
    let mut uses = Vec::new();
    op.visit_operands(|_, operand| uses.push((operand.value, false)));
    uses
}

fn uses_in_terminator(term: &SemTerminator) -> Vec<(ValueId, bool)> {
    let mut uses = Vec::new();
    // The canonical visitor orders control inputs before normal-edge
    // arguments. Only that interval can see the terminator result.
    let normal_slots = match term {
        SemTerminator::Call { args, normal, .. } | SemTerminator::RtCall { args, normal, .. } => {
            args.len()..args.len() + normal.args.len()
        }
        SemTerminator::CheckedBinary { normal, .. } => 2..2 + normal.args.len(),
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
        let context = callable_context(&callables);
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
        let context = callable_context(&callables);
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
        let context = callable_context(&callables);
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
