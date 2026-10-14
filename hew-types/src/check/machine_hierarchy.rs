//! Checked declaration identity, initial descent and transition boundaries.

#![expect(
    clippy::result_large_err,
    reason = "hierarchy checking uses the shared checker diagnostic value"
)]

use std::collections::HashMap;

use hew_parser::ast::{sym, Block, Expr, Ident, MachineDecl, Span, Spanned, TypeExpr};

use crate::error::{TypeError, TypeErrorKind};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct NodeId(usize);

#[derive(Debug, Clone, Copy)]
enum Declaration {
    Leaf(usize),
    Compound(usize),
}

#[derive(Debug, Clone)]
struct Node {
    declaration: Declaration,
    parent: Option<NodeId>,
    initial: Option<NodeId>,
}

#[derive(Debug, Clone)]
pub struct Hierarchy {
    nodes: Vec<Node>,
    names: HashMap<Ident, NodeId>,
    rules: Vec<ResolvedRule>,
}

#[derive(Debug, Clone)]
struct ResolvedRule {
    source: NodeId,
    target: Option<NodeId>,
    domain: Option<NodeId>,
    priority: usize,
    body_span: Span,
}

impl Hierarchy {
    pub(super) fn check(machine: &MachineDecl) -> Result<Self, TypeError> {
        let mut result = Self {
            nodes: Vec::new(),
            names: HashMap::new(),
            rules: Vec::new(),
        };
        for (index, state) in machine.states.iter().enumerate() {
            result.insert(state.name, state.span.clone(), Declaration::Leaf(index))?;
        }
        for (index, group) in machine.composite_groups.iter().enumerate() {
            result.insert(group.name, group.span.clone(), Declaration::Compound(index))?;
        }
        for group in &machine.composite_groups {
            let parent = result.names[&group.name];
            for member in &group.members {
                let child = result.resolve(*member, group.span.clone())?;
                if result.nodes[child.0].parent.replace(parent).is_some() {
                    return Err(refusal(
                        group.span.clone(),
                        format!("state `{member}` has more than one parent"),
                    ));
                }
            }
            let initial = result.resolve(group.initial, group.span.clone())?;
            if result.nodes[initial.0].parent != Some(parent) {
                return Err(refusal(
                    group.span.clone(),
                    format!(
                        "initial state `{}` must be a direct child of `{}`",
                        group.initial, group.name
                    ),
                ));
            }
            result.nodes[parent.0].initial = Some(initial);
        }
        for (name, node) in &result.names {
            let mut visited = Vec::new();
            let mut current = Some(*node);
            while let Some(id) = current {
                if visited.contains(&id) {
                    return Err(refusal(
                        result.span(machine, *node),
                        format!("state hierarchy contains a cycle at `{name}`"),
                    ));
                }
                visited.push(id);
                current = result.nodes[id.0].parent;
            }
        }
        Ok(result)
    }

    /// Resolve inherited fields and scoped rules once, retaining their node
    /// identities alongside the ordinary declarations they will generate.
    pub(super) fn resolve_relation(
        &mut self,
        machine: &MachineDecl,
    ) -> Result<MachineDecl, TypeError> {
        let mut resolved = machine.clone();
        for state in &mut resolved.states {
            let node = self.resolve(state.name, state.span.clone())?;
            state.fields = self.inherited_fields(machine, node)?;
        }
        resolved.transitions.clear();
        let authored = machine
            .transitions
            .iter()
            .map(|rule| (rule, None))
            .chain(machine.composite_groups.iter().flat_map(|group| {
                let scope = self.names[&group.name];
                group
                    .parent_transitions
                    .iter()
                    .map(move |rule| (rule, Some(scope)))
            }))
            .collect::<Vec<_>>();
        for (rule, scope) in authored {
            let source = if rule.source_state.name == sym::UNDERSCORE {
                None
            } else {
                Some(self.resolve_path(
                    machine,
                    rule.source_state,
                    &rule.source_path,
                    rule.span.clone(),
                )?)
            };
            if let (Some(scope), Some(source)) = (scope, source) {
                if !self.ancestors(source).contains(&scope) {
                    return Err(refusal(
                        rule.span.clone(),
                        format!(
                            "source `{}` is outside compound `{}`",
                            rule.source_state,
                            self.name(machine, scope)
                        ),
                    ));
                }
            }
            let target = if rule.target_state.name == sym::UNDERSCORE {
                None
            } else {
                Some(self.resolve_path(
                    machine,
                    rule.target_state,
                    &rule.target_path,
                    rule.span.clone(),
                )?)
            };
            for state in &machine.states {
                let leaf = self.names[&state.name];
                let path = self.ancestors(leaf);
                let selected_scope = source.or(scope);
                if selected_scope.is_some_and(|node| !path.contains(&node)) {
                    continue;
                }
                let priority = selected_scope.map_or(path.len(), |node| {
                    path.iter()
                        .position(|known| *known == node)
                        .expect("scope is on branch")
                });
                let mut expanded = rule.clone();
                expanded.source_state = state.name;
                if let Some(target) = target {
                    let initial = self.leaf(target);
                    expanded.target_state = self.name(machine, initial);
                    if target != initial {
                        rewrite_target(
                            &mut expanded.body.0,
                            rule.target_state,
                            expanded.target_state,
                            machine.name,
                        );
                    }
                }
                resolved.transitions.push(expanded);
                self.rules.push(ResolvedRule {
                    source: leaf,
                    target,
                    domain: target.and_then(|target| self.domain(leaf, target, rule.reenter)),
                    priority,
                    body_span: rule.body.1.clone(),
                });
            }
        }
        Ok(resolved)
    }

    pub(super) fn transitions(&self, machine: &MachineDecl) -> Vec<(String, Span)> {
        self.rules
            .iter()
            .map(|rule| {
                (
                    self.name(machine, rule.source).to_string(),
                    rule.body_span.clone(),
                )
            })
            .collect()
    }

    pub(super) fn rules_for(
        &self,
        machine: &MachineDecl,
        state: Ident,
        event: Ident,
    ) -> Vec<usize> {
        let mut rules: Vec<_> = machine
            .transitions
            .iter()
            .enumerate()
            .filter(|(_, rule)| rule.source_state == state && rule.event_name == event)
            .map(|(index, _)| index)
            .collect();
        rules.sort_by_key(|index| {
            (
                self.rules[*index].priority,
                machine.transitions[*index].span.start,
            )
        });
        if let Some(priority) = rules.iter().find_map(|index| {
            machine.transitions[*index]
                .guard
                .is_none()
                .then_some(self.rules[*index].priority)
        }) {
            rules.retain(|index| self.rules[*index].priority <= priority);
        }
        rules
    }

    pub(super) fn boundary(&self, rule: usize) -> (NodeId, Option<NodeId>) {
        let checked = &self.rules[rule];
        (checked.source, checked.domain)
    }

    pub(super) fn target(&self, rule: usize) -> Option<NodeId> {
        self.rules[rule].target
    }

    fn insert(
        &mut self,
        name: Ident,
        span: Span,
        declaration: Declaration,
    ) -> Result<(), TypeError> {
        let id = NodeId(self.nodes.len());
        if self.names.insert(name, id).is_some() {
            return Err(refusal(
                span,
                format!("machine state `{name}` is declared more than once; names must be unique across compound paths"),
            ));
        }
        self.nodes.push(Node {
            declaration,
            parent: None,
            initial: None,
        });
        Ok(())
    }

    pub(super) fn resolve(&self, name: Ident, span: Span) -> Result<NodeId, TypeError> {
        self.names
            .get(&name)
            .copied()
            .ok_or_else(|| refusal(span, format!("machine transition references an undeclared state or input event: state `{name}`")))
    }

    pub(super) fn resolve_path(
        &self,
        machine: &MachineDecl,
        name: Ident,
        path: &[Ident],
        span: Span,
    ) -> Result<NodeId, TypeError> {
        let node = self.resolve(name, span.clone())?;
        if !path.is_empty() {
            let mut actual: Vec<_> = self
                .ancestors(node)
                .into_iter()
                .rev()
                .map(|id| self.name(machine, id))
                .collect();
            if path.first() == Some(&machine.name) {
                actual.insert(0, machine.name);
            }
            if !actual.ends_with(path) {
                return Err(refusal(
                    span,
                    format!(
                        "state path `{}` does not name `{}`",
                        path.iter()
                            .map(ToString::to_string)
                            .collect::<Vec<_>>()
                            .join("."),
                        actual
                            .iter()
                            .map(ToString::to_string)
                            .collect::<Vec<_>>()
                            .join(".")
                    ),
                ));
            }
        }
        Ok(node)
    }

    pub(super) fn inherited_fields(
        &self,
        machine: &MachineDecl,
        node: NodeId,
    ) -> Result<Vec<(Ident, Spanned<TypeExpr>)>, TypeError> {
        let mut fields = Vec::new();
        let mut owners = HashMap::new();
        for owner in self.ancestors(node).into_iter().rev() {
            let declared = match self.nodes[owner.0].declaration {
                Declaration::Leaf(index) => &machine.states[index].fields,
                Declaration::Compound(index) => &machine.composite_groups[index].fields,
            };
            for (field, ty) in declared {
                if let Some(prior) = owners.insert(*field, owner) {
                    return Err(refusal(
                        ty.1.clone(),
                        format!(
                            "field `{field}` in `{}` conflicts with inherited field from `{}`",
                            self.name(machine, owner),
                            self.name(machine, prior)
                        ),
                    ));
                }
                fields.push((*field, ty.clone()));
            }
        }
        Ok(fields)
    }

    pub(super) fn leaf(&self, mut node: NodeId) -> NodeId {
        while let Some(child) = self.nodes[node.0].initial {
            node = child;
        }
        node
    }

    pub(super) fn name(&self, machine: &MachineDecl, node: NodeId) -> Ident {
        match self.nodes[node.0].declaration {
            Declaration::Leaf(index) => machine.states[index].name,
            Declaration::Compound(index) => machine.composite_groups[index].name,
        }
    }

    fn span(&self, machine: &MachineDecl, node: NodeId) -> Span {
        match self.nodes[node.0].declaration {
            Declaration::Leaf(index) => machine.states[index].span.clone(),
            Declaration::Compound(index) => machine.composite_groups[index].span.clone(),
        }
    }

    /// Active branch from its leaf towards the machine root.
    pub(super) fn ancestors(&self, node: NodeId) -> Vec<NodeId> {
        let mut path = vec![node];
        while let Some(parent) = self.nodes[path.last().expect("path contains its leaf").0].parent {
            path.push(parent);
        }
        path
    }

    /// Named sibling changes retain their nearest common ancestor. Explicit
    /// reentry of an active target instead retains that target's parent.
    pub(super) fn domain(&self, source: NodeId, target: NodeId, reenter: bool) -> Option<NodeId> {
        let source_path = self.ancestors(source);
        if reenter && source_path.contains(&target) {
            return self.nodes[target.0].parent;
        }
        self.ancestors(self.leaf(target))
            .into_iter()
            .find(|node| source_path.contains(node))
    }

    pub(super) fn hooks<'a>(
        &self,
        machine: &'a MachineDecl,
        node: NodeId,
        entry: bool,
    ) -> Option<&'a Block> {
        let (enter, exit) = match self.nodes[node.0].declaration {
            Declaration::Leaf(index) => (&machine.states[index].entry, &machine.states[index].exit),
            Declaration::Compound(index) => (
                &machine.composite_groups[index].entry,
                &machine.composite_groups[index].exit,
            ),
        };
        if entry {
            enter.as_ref()
        } else {
            exit.as_ref()
        }
    }
}

fn rewrite_target(expr: &mut Expr, target: Ident, initial: Ident, machine: Ident) {
    match expr {
        Expr::Ident(name) if *name == target => *name = initial,
        Expr::ContextVariant(variant) if variant.name == target => variant.name = initial,
        Expr::StructInit { path, .. } => {
            if path.as_single() == Some(target)
                || matches!(path.segments.as_slice(), [(owner, _), (name, _)] if *owner == machine && *name == target)
            {
                path.segments
                    .last_mut()
                    .expect("named target has a segment")
                    .0 = initial;
            }
        }
        Expr::FieldAccess { object, field }
            if field.0 == target && matches!(&object.0, Expr::Ident(name) if *name == machine) =>
        {
            field.0 = initial;
        }
        Expr::Block(block) => {
            if let Some(tail) = &mut block.trailing_expr {
                rewrite_target(&mut tail.0, target, initial, machine);
            }
        }
        Expr::If {
            then_block,
            else_block,
            ..
        } => {
            rewrite_target(&mut then_block.0, target, initial, machine);
            if let Some(other) = else_block {
                rewrite_target(&mut other.0, target, initial, machine);
            }
        }
        Expr::Match { arms, .. } => {
            for arm in arms {
                rewrite_target(&mut arm.body.0, target, initial, machine);
            }
        }
        _ => {}
    }
}

fn refusal(span: Span, message: String) -> TypeError {
    TypeError::new(TypeErrorKind::MachineExhaustivenessError, span, message)
}
