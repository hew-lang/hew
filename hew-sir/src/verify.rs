use std::collections::{BTreeMap, HashMap, HashSet};

use crate::OpId;
use crate::{
    BlockId, CallableId, CallableInstance, GenericTemplateId, SemCallConv, SemCallable,
    SemCallableKind, SemFunction, SemGenericTemplate, SemModule, SemOp, SemOpKind, SemParamPassing,
    SemSignature, SemTerminator, SirInstanceKey, UseMode, UseSite, ValueId,
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
    /// The initial scalar SIR surface is intentionally read-only even though
    /// operands retain the broader ownership vocabulary for later slices.
    /// Keeping the exact use site makes malformed edge/return/condition uses
    /// fail at the SIR stage rather than reaching ownership MIR silently.
    InvalidUseMode {
        site: UseSite,
        expected: UseMode,
        actual: UseMode,
        context: &'static str,
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SirDiagnostic {
    pub function: String,
    pub kind: SirDiagnosticKind,
}

#[derive(Debug)]
struct CallableContext<'a> {
    by_id: BTreeMap<CallableId, &'a SemCallable>,
}

impl<'a> CallableContext<'a> {
    fn callable(&self, id: CallableId) -> Option<&'a SemCallable> {
        self.by_id.get(&id).copied()
    }
}

#[must_use]
pub fn verify_module(module: &SemModule) -> Vec<SirDiagnostic> {
    let mut diagnostics = Vec::new();
    let callables = verify_callable_table(module, &mut diagnostics);
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
        diagnostics.extend(verify_function_with_context(function, Some(&callables)));
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
    diagnostics.extend(verify_function_with_context(function, Some(&callables)));
    diagnostics
}

/// Verify one semantic SSA function before it crosses into another SIR pass
/// or the ownership/layout MIR boundary.
///
/// Keeping this public lets every consumer fail closed rather than relying on
/// a particular CLI lane to have run whole-module verification first.
#[allow(
    clippy::too_many_lines,
    reason = "the verifier keeps SSA collection, CFG shape, and dominance checks together so the stage boundary is auditable"
)]
#[must_use]
pub fn verify_function(function: &SemFunction) -> Vec<SirDiagnostic> {
    verify_function_with_context(function, None)
}

#[allow(
    clippy::too_many_lines,
    reason = "the verifier keeps SSA collection, CFG shape, and dominance checks together so the stage boundary is auditable"
)]
fn verify_function_with_context(
    function: &SemFunction,
    callable_context: Option<&CallableContext<'_>>,
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
    for param in &function.params {
        record_value(function, param.value, &mut values, &mut diagnostics);
        types.insert(param.value, param.ty.clone());
        definitions.insert(param.value, (function.entry, None));
    }
    for block in &function.blocks {
        for arg in &block.args {
            record_value(function, arg.value, &mut values, &mut diagnostics);
            types.insert(arg.value, arg.ty.clone());
            definitions.insert(arg.value, (block.id, None));
        }
        for (op_index, op) in block.ops.iter().enumerate() {
            if !operations.insert(op.id) {
                diagnostics.push(diag(function, SirDiagnosticKind::DuplicateOp(op.id)));
            }
            for result in &op.results {
                record_value(function, result.id, &mut values, &mut diagnostics);
                types.insert(result.id, result.ty.clone());
                definitions.insert(result.id, (block.id, Some(op_index)));
            }
        }
    }
    // Every value type is known before checking operations, edges, and
    // terminators. In particular this catches a malformed use whose value is
    // defined in a later block rather than silently skipping its type check.
    for block in &function.blocks {
        for op in &block.ops {
            verify_operation_shape(function, op, &types, callable_context, &mut diagnostics);
        }
        verify_terminator_operand_modes(function, block.id, &block.terminator, &mut diagnostics);
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
        verify_terminator_shape(function, &block.terminator, &types, &mut diagnostics);
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
            if !is_initial_scalar(&abi.ty) {
                diagnostics.push(module_diag(SirDiagnosticKind::InvalidCallable {
                    callable: callable.id,
                    reason: format!(
                        "parameter {parameter} has non-scalar type `{}`",
                        abi.ty.user_facing()
                    ),
                }));
            }
            if abi.passing != SemParamPassing::ReadOnly {
                diagnostics.push(module_diag(SirDiagnosticKind::InvalidCallable {
                    callable: callable.id,
                    reason: format!("parameter {parameter} has non-ReadOnly ABI passing"),
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
        if !is_initial_scalar_return(&callable.signature.return_ty) {
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
            Some(callable) if callable.declaration.full_path() != "main" => {
                diagnostics.push(module_diag(SirDiagnosticKind::InvalidEntryCallable {
                    callable: entry,
                    reason: "entry callable must resolve to the canonical root-unit source `main` declaration"
                        .to_string(),
                }));
            }
            Some(callable) if callable.symbol != "main" => {
                diagnostics.push(module_diag(SirDiagnosticKind::InvalidEntryCallable {
                    callable: entry,
                    reason: "entry callable must retain the canonical emitted `main` symbol"
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
    ty.is_integer() || matches!(ty, ResolvedTy::Bool)
}

fn is_initial_scalar_return(ty: &ResolvedTy) -> bool {
    matches!(ty, ResolvedTy::Unit) || is_initial_scalar(ty)
}

#[allow(
    clippy::too_many_lines,
    reason = "the closed first-slice operation relation table is deliberately central so additions must make their verifier rule explicit"
)]
fn verify_operation_shape(
    function: &SemFunction,
    operation: &SemOp,
    types: &HashMap<ValueId, ResolvedTy>,
    callable_context: Option<&CallableContext<'_>>,
    diagnostics: &mut Vec<SirDiagnostic>,
) {
    operation.visit_operands(|operand, use_| {
        require_read_use(
            function,
            UseSite::Operation {
                op: operation.id,
                operand,
                value: use_.value,
                mode: use_.mode,
            },
            operation_operand_context(&operation.kind, operand),
            diagnostics,
        );
    });
    if let SemOpKind::Call { callee, args } = &operation.kind {
        verify_direct_call_operation(
            function,
            operation,
            *callee,
            args,
            types,
            callable_context,
            diagnostics,
        );
        return;
    }
    if operation.results.len() != 1 {
        diagnostics.push(diag(
            function,
            SirDiagnosticKind::InvalidResultArity {
                op: operation.id,
                actual: operation.results.len(),
            },
        ));
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
                    operand_ty == &result.ty && (operand_ty.is_integer() || operand_ty.is_float())
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
                | hew_parser::ast::BinaryOp::BitAnd
                | hew_parser::ast::BinaryOp::BitOr
                | hew_parser::ast::BinaryOp::BitXor
                | hew_parser::ast::BinaryOp::Shl
                | hew_parser::ast::BinaryOp::Shr
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
        SemOpKind::Call { .. } => unreachable!("calls return before value-result validation"),
        SemOpKind::ConstI64(_) | SemOpKind::ConstBool(_) => {}
    }
}

#[allow(
    clippy::too_many_arguments,
    clippy::too_many_lines,
    reason = "direct-call verification keeps callable ABI, result arity, and operand rules together at the SIR boundary"
)]
fn verify_direct_call_operation(
    function: &SemFunction,
    operation: &SemOp,
    callee: CallableId,
    args: &[crate::Operand],
    types: &HashMap<ValueId, ResolvedTy>,
    callable_context: Option<&CallableContext<'_>>,
    diagnostics: &mut Vec<SirDiagnostic>,
) {
    if operation.results.len() > 1 {
        diagnostics.push(diag(
            function,
            SirDiagnosticKind::InvalidResultArity {
                op: operation.id,
                actual: operation.results.len(),
            },
        ));
        return;
    }
    let Some(callable_context) = callable_context else {
        // A context-free verifier cannot know whether a legal direct call is
        // unit-returning, but it can still enforce the initial 0-or-1 result
        // representation and the operand-use discipline above.
        return;
    };
    let Some(target) = callable_context.callable(callee) else {
        diagnostics.push(diag(
            function,
            SirDiagnosticKind::UnknownCallable {
                op: operation.id,
                callee,
            },
        ));
        return;
    };
    if target.call_conv != SemCallConv::Default || target.kind != SemCallableKind::HewDirect {
        invalid_operation(
            function,
            operation.id,
            "direct call targets a callable outside SIR's default HewDirect ABI domain".to_string(),
            diagnostics,
        );
    }
    let expected_results = usize::from(target.signature.return_ty != ResolvedTy::Unit);
    if operation.results.len() != expected_results {
        diagnostics.push(diag(
            function,
            SirDiagnosticKind::InvalidCallResultArity {
                op: operation.id,
                callee,
                expected: expected_results,
                actual: operation.results.len(),
            },
        ));
    } else if let [result] = operation.results.as_slice() {
        if result.ty != target.signature.return_ty {
            invalid_operation(
                function,
                operation.id,
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
            operation.id,
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
        if parameter.passing != SemParamPassing::ReadOnly {
            invalid_operation(
                function,
                operation.id,
                format!(
                    "direct call target `{}` parameter {index} has non-ReadOnly ABI passing",
                    target.declaration.full_path()
                ),
                diagnostics,
            );
        }
        if let Some(actual) = types.get(&argument.value) {
            if actual != &parameter.ty {
                invalid_operation(
                    function,
                    operation.id,
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

fn require_read_use(
    function: &SemFunction,
    site: UseSite,
    context: &'static str,
    diagnostics: &mut Vec<SirDiagnostic>,
) {
    let mode = match site {
        UseSite::Operation { mode, .. } | UseSite::Terminator { mode, .. } => mode,
    };
    if mode != UseMode::Read {
        diagnostics.push(diag(
            function,
            SirDiagnosticKind::InvalidUseMode {
                site,
                expected: UseMode::Read,
                actual: mode,
                context,
            },
        ));
    }
}

fn operation_operand_context(kind: &SemOpKind, operand: crate::OperandSlot) -> &'static str {
    match kind {
        SemOpKind::ConstI64(_) | SemOpKind::ConstBool(_) => "operation operand",
        SemOpKind::Unary { .. } => "unary operand",
        SemOpKind::Binary { .. } if operand.0 == 0 => "binary left operand",
        SemOpKind::Binary { .. } => "binary right operand",
        SemOpKind::Cast { .. } => "cast operand",
        SemOpKind::Call { .. } => "direct call argument",
    }
}

fn verify_terminator_operand_modes(
    function: &SemFunction,
    block: BlockId,
    terminator: &SemTerminator,
    diagnostics: &mut Vec<SirDiagnostic>,
) {
    terminator.visit_operands(|operand, use_| {
        require_read_use(
            function,
            UseSite::Terminator {
                block,
                operand,
                value: use_.value,
                mode: use_.mode,
            },
            terminator.operand_context(operand),
            diagnostics,
        );
    });
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

fn verify_terminator_shape(
    function: &SemFunction,
    terminator: &SemTerminator,
    types: &HashMap<ValueId, ResolvedTy>,
    diagnostics: &mut Vec<SirDiagnostic>,
) {
    match terminator {
        SemTerminator::Return { value: Some(value) } if function.return_ty == ResolvedTy::Unit => {
            diagnostics.push(diag(
                function,
                SirDiagnosticKind::UnitReturnValue { value: value.value },
            ));
        }
        SemTerminator::Return { value: Some(value) } => {
            if let Some(actual) = types.get(&value.value) {
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
        SemTerminator::Return { .. } | SemTerminator::Goto(_) | SemTerminator::Unreachable => {}
    }
}

#[allow(clippy::too_many_arguments, reason = "small verifier transfer helper")]
fn verify_uses(
    function: &SemFunction,
    dominators: &crate::Dominators,
    definitions: &HashMap<ValueId, (BlockId, Option<usize>)>,
    use_block: BlockId,
    use_index: Option<usize>,
    uses: Vec<ValueId>,
    diagnostics: &mut Vec<SirDiagnostic>,
) {
    for value in uses {
        let Some((definition, definition_index)) = definitions.get(&value) else {
            diagnostics.push(diag(function, SirDiagnosticKind::UndefinedValue(value)));
            continue;
        };
        if definition == &use_block {
            if let (Some(definition_index), Some(use_index)) = (definition_index, use_index) {
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

fn diag(function: &SemFunction, kind: SirDiagnosticKind) -> SirDiagnostic {
    SirDiagnostic {
        function: function.name.clone(),
        kind,
    }
}

fn module_diag(kind: SirDiagnosticKind) -> SirDiagnostic {
    SirDiagnostic {
        function: "<module>".to_string(),
        kind,
    }
}

fn uses_in_op(op: &crate::SemOp) -> Vec<ValueId> {
    let mut uses = Vec::new();
    op.visit_operands(|_, operand| uses.push(operand.value));
    uses
}

fn uses_in_terminator(term: &SemTerminator) -> Vec<ValueId> {
    let mut uses = Vec::new();
    term.visit_operands(|_, operand| uses.push(operand.value));
    uses
}
