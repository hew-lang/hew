//! Authored jumps must close over authored loops before fragments acquire the
//! evaluator's internal reaction loop. Check both source and rewritten fragments.
#![expect(
    clippy::result_large_err,
    reason = "machine control uses the shared checker diagnostic value"
)]
use super::{walk_block, walk_expr, NodeVisitor};
use crate::error::{TypeError, TypeErrorKind};
use hew_parser::ast::{Block, Expr, Ident, MachineDecl, Span, Spanned, Stmt};
use std::collections::HashMap;

#[derive(Default)]
struct Scopes {
    bodies: HashMap<*const Block, Option<Ident>>,
    active: Vec<Option<Ident>>,
    error: Option<TypeError>,
}
impl NodeVisitor for Scopes {
    fn enters_nested_callables(&self) -> bool {
        false
    }
    fn visit_block(&mut self, block: &Block) {
        if let Some(label) = self.bodies.get(&std::ptr::from_ref(block)) {
            self.active.push(*label);
        }
    }
    fn leave_block(&mut self, block: &Block) {
        if self.bodies.contains_key(&std::ptr::from_ref(block)) {
            self.active.pop();
        }
    }
    fn visit_stmt(&mut self, stmt: &Stmt, span: &Span) {
        match stmt {
            Stmt::Loop { label, body }
            | Stmt::While { label, body, .. }
            | Stmt::WhileLet { label, body, .. }
            | Stmt::For { label, body, .. } => {
                self.bodies.insert(std::ptr::from_ref(body), *label);
            }
            Stmt::Break { label, .. } | Stmt::Continue { label } => {
                let local = label.map_or(!self.active.is_empty(), |label| {
                    self.active
                        .iter()
                        .flatten()
                        .any(|active| active.name == label.name)
                });
                if !local && self.error.is_none() {
                    self.error = Some(TypeError::new(TypeErrorKind::MachineExhaustivenessError,span.clone(),
                        "a machine jump must target a loop declared inside the same guard, body, hook or initializer"));
                }
            }
            _ => {}
        }
    }
}
pub(super) fn validate_block(value: &Block) -> Result<(), TypeError> {
    let mut scopes = Scopes::default();
    walk_block(value, &mut scopes);
    scopes.error.map_or(Ok(()), Err)
}
pub(super) fn validate_expr(value: &Spanned<Expr>) -> Result<(), TypeError> {
    let mut scopes = Scopes::default();
    walk_expr(&value.0, &value.1, &mut scopes);
    scopes.error.map_or(Ok(()), Err)
}
pub(super) fn validate(machine: &MachineDecl) -> Result<(), TypeError> {
    for rule in machine.authored_transitions() {
        if let Some(guard) = &rule.guard {
            validate_expr(guard)?;
        }
        validate_expr(&rule.body)?;
    }
    for (_, hook) in machine.state_hooks() {
        validate_block(hook)?;
    }
    for group in &machine.composite_groups {
        if let Some(initial) = &group.initial_value {
            validate_expr(initial)?;
        }
        for region in &group.regions {
            if let Some(initial) = &region.initial_value {
                validate_expr(initial)?;
            }
        }
    }
    Ok(())
}
