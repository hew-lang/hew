use std::collections::{BTreeMap, HashMap, HashSet};

use crate::{BlockId, SemFunction, SemModule, SemTerminator, ValueId};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SirDiagnosticKind {
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
    module.functions.iter().flat_map(verify_function).collect()
}

fn verify_function(function: &SemFunction) -> Vec<SirDiagnostic> {
    let mut diagnostics = Vec::new();
    let mut blocks = BTreeMap::new();
    for block in &function.blocks {
        if blocks.insert(block.id, block).is_some() {
            diagnostics.push(diag(function, SirDiagnosticKind::DuplicateBlock(block.id)));
        }
    }
    let mut values = HashSet::new();
    let mut types = HashMap::new();
    let mut definitions = HashMap::new();
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
            for result in &op.results {
                record_value(function, result.id, &mut values, &mut diagnostics);
                types.insert(result.id, result.ty.clone());
                definitions.insert(result.id, (block.id, Some(op_index)));
            }
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
    }
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
    diagnostics
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
    use crate::SemOpKind;
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
