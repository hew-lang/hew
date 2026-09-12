//! What a machine body binds at a cursor offset.
//!
//! A transition body, its guard, and a state's `entry`/`exit` hooks bind
//! exactly two implicit names: `state` is the source state's payload and
//! `event` is the incoming input (HEW-SPEC-2026 §3.11.3). `self` is an actor
//! or method receiver and is never bound here, so the editor never offers it.
//! Completions and hover read this one description rather than each deriving
//! the scope from the AST again.

use hew_parser::ast::{Block, MachineDecl, Span};

/// The name a machine body binds and the type it renders as in the editor.
#[derive(Debug)]
pub struct MachineBinding {
    pub name: &'static str,
    pub ty: String,
}

/// The implicit bindings in scope inside `machine` at `offset`, plus any
/// event-head binding names (`on Open(by): …`) the rule declares.
#[derive(Debug)]
pub struct MachineScope {
    pub bindings: Vec<MachineBinding>,
    pub head_bindings: Vec<String>,
}

/// Describe the machine body containing `offset`, or `None` when the offset is
/// outside every transition body, guard and hook.
#[must_use]
pub fn scope_at(machine: &MachineDecl, offset: usize) -> Option<MachineScope> {
    for transition in &machine.transitions {
        let in_body = contains(&transition.body.1, offset);
        let in_guard = transition
            .guard
            .as_ref()
            .is_some_and(|guard| contains(&guard.1, offset));
        if !in_body && !in_guard {
            continue;
        }
        let source = if transition.source_state == "_" {
            machine.name.clone()
        } else {
            format!("{}.{}", machine.name, transition.source_state)
        };
        let bindings = vec![
            MachineBinding {
                name: "state",
                ty: source,
            },
            MachineBinding {
                name: "event",
                ty: format!("{}Event.{}", machine.name, transition.event_name),
            },
        ];
        return Some(MachineScope {
            bindings,
            head_bindings: transition.event_bindings.clone(),
        });
    }

    for state in &machine.states {
        let hooks = [state.entry.as_ref(), state.exit.as_ref()];
        if !hooks
            .into_iter()
            .flatten()
            .any(|hook| block_span(hook).is_some_and(|span| contains(&span, offset)))
        {
            continue;
        }
        return Some(MachineScope {
            bindings: vec![MachineBinding {
                name: "state",
                ty: format!("{}.{}", machine.name, state.name),
            }],
            head_bindings: Vec::new(),
        });
    }

    None
}

fn contains(span: &Span, offset: usize) -> bool {
    span.start <= offset && offset <= span.end
}

/// The source range a block covers, from its first statement to its last.
///
/// `Block` carries no span of its own; a hook's extent is the extent of what
/// it holds, which is enough to decide whether the cursor is inside it.
fn block_span(block: &Block) -> Option<Span> {
    let start = block
        .stmts
        .first()
        .map(|(_, span)| span.start)
        .or_else(|| block.trailing_expr.as_ref().map(|value| value.1.start))?;
    let end = block
        .trailing_expr
        .as_ref()
        .map(|value| value.1.end)
        .or_else(|| block.stmts.last().map(|(_, span)| span.end))?;
    Some(start..end)
}
