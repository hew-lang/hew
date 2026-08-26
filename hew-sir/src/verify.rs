use std::collections::{BTreeMap, HashMap, HashSet};

use crate::{
    BlockId, OpId, SemFunction, SemModule, SemOp, SemOpKind, SemTerminator, UseMode, ValueId,
};
use hew_types::ResolvedTy;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SirDiagnosticKind {
    DuplicateFunctionName(String),
    DuplicateFunctionDeclaration(String),
    MissingEntry(BlockId),
    EntryBlockArgs {
        entry: BlockId,
        actual: usize,
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
    BranchConditionType {
        value: ValueId,
        actual: String,
    },
    ReturnType {
        expected: String,
        actual: Option<String>,
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

#[must_use]
pub fn verify_module(module: &SemModule) -> Vec<SirDiagnostic> {
    let mut diagnostics = Vec::new();
    let mut names = HashSet::new();
    let mut declarations = HashSet::new();
    for function in &module.functions {
        if !names.insert(function.name.clone()) {
            diagnostics.push(diag(
                function,
                SirDiagnosticKind::DuplicateFunctionName(function.name.clone()),
            ));
        }
        if !declarations.insert(function.declaration.clone()) {
            diagnostics.push(diag(
                function,
                SirDiagnosticKind::DuplicateFunctionDeclaration(format!(
                    "{:?}",
                    function.declaration
                )),
            ));
        }
        diagnostics.extend(verify_function(function));
    }
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
    let mut diagnostics = Vec::new();
    let mut blocks = BTreeMap::new();
    for block in &function.blocks {
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
            verify_operation_shape(function, op, &types, &mut diagnostics);
        }
        for edge in block.terminator.successors() {
            let Some(target) = blocks.get(&edge.target) else {
                diagnostics.push(diag(function, SirDiagnosticKind::UnknownBlock(edge.target)));
                continue;
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
                let Some(actual) = types.get(value) else {
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
        }
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
    reason = "the closed first-slice operation relation table is deliberately central so additions must make their verifier rule explicit"
)]
fn verify_operation_shape(
    function: &SemFunction,
    operation: &SemOp,
    types: &HashMap<ValueId, ResolvedTy>,
    diagnostics: &mut Vec<SirDiagnostic>,
) {
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
            require_read_operand(function, operation.id, value.mode, "cast", diagnostics);
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
            require_read_operand(
                function,
                operation.id,
                value.mode,
                "unary operation",
                diagnostics,
            );
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
            require_read_operand(
                function,
                operation.id,
                lhs.mode,
                "binary left operand",
                diagnostics,
            );
            require_read_operand(
                function,
                operation.id,
                rhs.mode,
                "binary right operand",
                diagnostics,
            );
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
        SemOpKind::ConstI64(_) | SemOpKind::ConstBool(_) | SemOpKind::Call { .. } => {}
    }
}

fn require_read_operand(
    function: &SemFunction,
    op: OpId,
    mode: UseMode,
    context: &str,
    diagnostics: &mut Vec<SirDiagnostic>,
) {
    if mode != UseMode::Read {
        invalid_operation(
            function,
            op,
            format!(
                "{context} uses {mode:?}; only Read is legal until ownership-aware SIR operations exist"
            ),
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

fn verify_terminator_shape(
    function: &SemFunction,
    terminator: &SemTerminator,
    types: &HashMap<ValueId, ResolvedTy>,
    diagnostics: &mut Vec<SirDiagnostic>,
) {
    match terminator {
        SemTerminator::Return { value: Some(value) } => {
            if let Some(actual) = types.get(value) {
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
            if let Some(actual) = types.get(condition) {
                if actual != &ResolvedTy::Bool {
                    diagnostics.push(diag(
                        function,
                        SirDiagnosticKind::BranchConditionType {
                            value: *condition,
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

fn uses_in_op(op: &crate::SemOp) -> Vec<ValueId> {
    match &op.kind {
        SemOpKind::ConstI64(_) | SemOpKind::ConstBool(_) => Vec::new(),
        SemOpKind::Unary { value, .. } | SemOpKind::Cast { value, .. } => vec![value.value],
        SemOpKind::Binary { lhs, rhs, .. } => vec![lhs.value, rhs.value],
        SemOpKind::Call { args, .. } => args.iter().map(|arg| arg.value).collect(),
    }
}

fn uses_in_terminator(term: &SemTerminator) -> Vec<ValueId> {
    match term {
        SemTerminator::Return { value } => value.iter().copied().collect(),
        SemTerminator::Goto(edge) => edge.args.clone(),
        SemTerminator::Branch {
            condition,
            then_target,
            else_target,
        } => {
            let mut values = vec![*condition];
            values.extend(then_target.args.iter().copied());
            values.extend(else_target.args.iter().copied());
            values
        }
        SemTerminator::Unreachable => Vec::new(),
    }
}
