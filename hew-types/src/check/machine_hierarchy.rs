//! Checked declaration identity, initial descent and transition boundaries.

#![expect(
    clippy::result_large_err,
    reason = "hierarchy checking uses the shared checker diagnostic value"
)]

use std::collections::HashMap;

use hew_parser::ast::{
    sym, Block, Expr, Ident, MachineDecl, MachineTrigger, Span, Spanned, TypeExpr,
};

use crate::error::{TypeError, TypeErrorKind};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) struct NodeId(usize);

#[derive(Debug, Clone, Copy)]
enum Declaration {
    Leaf(usize),
    Compound(usize),
    Region { group: usize, region: usize },
}

#[derive(Debug, Clone)]
struct Node {
    declaration: Declaration,
    parent: Option<NodeId>,
    initial: Option<NodeId>,
    fields: Vec<(Ident, Spanned<TypeExpr>)>,
}

#[derive(Debug, Clone)]
pub struct Hierarchy {
    nodes: Vec<Node>,
    names: HashMap<Ident, NodeId>,
    regions: HashMap<(Ident, Ident), NodeId>,
    rules: Vec<ResolvedRule>,
}

#[derive(Debug, Clone)]
struct ResolvedRule {
    source: NodeId,
    target: Option<NodeId>,
    domain: Option<NodeId>,
    body_span: Span,
    owner: Option<NodeId>,
    ordinal: usize,
}

impl Hierarchy {
    pub(super) fn check(machine: &MachineDecl) -> Result<Self, TypeError> {
        let mut result = Self {
            nodes: Vec::new(),
            names: HashMap::new(),
            regions: HashMap::new(),
            rules: Vec::new(),
        };
        for (index, state) in machine.states.iter().enumerate() {
            result.insert(
                state.name,
                state.span.clone(),
                Declaration::Leaf(index),
                state.fields.clone(),
            )?;
        }
        for (index, group) in machine.composite_groups.iter().enumerate() {
            result.insert(
                group.name,
                group.span.clone(),
                Declaration::Compound(index),
                group.fields.clone(),
            )?;
            for (region_index, region) in group.regions.iter().enumerate() {
                let id = NodeId(result.nodes.len());
                if result
                    .regions
                    .insert((group.name, region.name), id)
                    .is_some()
                {
                    return Err(refusal(
                        region.span.clone(),
                        format!(
                            "parallel state `{}` declares region `{}` more than once",
                            group.name, region.name
                        ),
                    ));
                }
                result.nodes.push(Node {
                    declaration: Declaration::Region {
                        group: index,
                        region: region_index,
                    },
                    parent: None,
                    initial: None,
                    fields: Vec::new(),
                });
            }
        }
        result.link_children(machine)?;
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

    fn link_children(&mut self, machine: &MachineDecl) -> Result<(), TypeError> {
        for group in &machine.composite_groups {
            let parent = self.names[&group.name];
            if group.regions.is_empty() {
                for member in &group.members {
                    let child = self.resolve(*member, group.span.clone())?;
                    if self.nodes[child.0].parent.replace(parent).is_some() {
                        return Err(refusal(
                            group.span.clone(),
                            format!("state `{member}` has more than one parent"),
                        ));
                    }
                }
                let initial = group.initial.ok_or_else(|| {
                    refusal(
                        group.span.clone(),
                        format!(
                            "composite state `{}` must declare an initial state",
                            group.name
                        ),
                    )
                })?;
                let initial = self.resolve(initial, group.span.clone())?;
                if self.nodes[initial.0].parent != Some(parent) {
                    return Err(refusal(
                        group.span.clone(),
                        format!(
                            "initial state `{}` must be a direct child of `{}`",
                            group.initial.expect("checked above"),
                            group.name
                        ),
                    ));
                }
                self.nodes[parent.0].initial = Some(initial);
            } else {
                if group.initial.is_some() || !group.members.is_empty() {
                    return Err(refusal(
                        group.span.clone(),
                        format!(
                            "parallel state `{}` declares exclusive children",
                            group.name
                        ),
                    ));
                }
                for region in &group.regions {
                    let region_node = self.regions[&(group.name, region.name)];
                    self.nodes[region_node.0].parent = Some(parent);
                    for member in &region.members {
                        let child = self.resolve(*member, region.span.clone())?;
                        if self.nodes[child.0].parent.replace(region_node).is_some() {
                            return Err(refusal(
                                region.span.clone(),
                                format!("state `{member}` has more than one parent"),
                            ));
                        }
                    }
                    let initial = self.resolve(region.initial, region.span.clone())?;
                    if self.nodes[initial.0].parent != Some(region_node) {
                        return Err(refusal(
                            region.span.clone(),
                            format!(
                                "initial state `{}` must be a direct child of region `{}`",
                                region.initial, region.name
                            ),
                        ));
                    }
                    self.nodes[region_node.0].initial = Some(initial);
                }
            }
        }
        Ok(())
    }

    /// Resolve inherited fields and scoped rules once, retaining their node
    /// identities alongside the ordinary declarations they will generate.
    #[expect(
        clippy::too_many_lines,
        reason = "resolve each authored rule and its configuration write domain together"
    )]
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
        let mut authored = machine
            .transitions
            .iter()
            .map(|rule| (rule, None))
            .chain(machine.composite_groups.iter().flat_map(|group| {
                let scope = self.names[&group.name];
                group
                    .parent_transitions
                    .iter()
                    .map(move |rule| (rule, Some(scope)))
                    .chain(group.regions.iter().flat_map(|region| {
                        let scope = self.regions[&(group.name, region.name)];
                        region
                            .parent_transitions
                            .iter()
                            .map(move |rule| (rule, Some(scope)))
                    }))
            }))
            .collect::<Vec<_>>();
        authored.sort_by_key(|(rule, _)| rule.span.start);
        for (ordinal, (rule, scope)) in authored.into_iter().enumerate() {
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
            if rule.trigger == MachineTrigger::Completion
                && source.is_none_or(|node| {
                    !matches!(self.nodes[node.0].declaration, Declaration::Compound(_))
                })
            {
                return Err(refusal(
                    rule.span.clone(),
                    "a completion transition must name a compound source state".to_string(),
                ));
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
            if rule.history
                && target.is_none_or(|node| {
                    !matches!(self.nodes[node.0].declaration, Declaration::Compound(_))
                })
            {
                return Err(refusal(
                    rule.span.clone(),
                    "a history destination must name a compound state".to_string(),
                ));
            }
            let selected_scope = source.or(scope);
            let container = selected_scope.and_then(|node| {
                if matches!(self.nodes[node.0].declaration, Declaration::Region { .. }) {
                    Some(node)
                } else {
                    self.container(node)
                }
            });
            for leaf in self.configurations(container) {
                let path = self.ancestors(leaf);
                if selected_scope.is_some_and(|node| !path.contains(&node)) {
                    continue;
                }
                let mut expanded = rule.clone();
                expanded.source_state = self.name(machine, leaf);
                if let Some(target) = target {
                    let initial = self.leaf(target);
                    if target != initial
                        && !rule.history
                        && !self.has_initial_value(machine, target)
                    {
                        expanded.target_state = self.name(machine, initial);
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
                    body_span: rule.body.1.clone(),
                    owner: selected_scope,
                    ordinal,
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
        let path = self.ancestors(self.names[&state]);
        let mut rules: Vec<_> = machine
            .transitions
            .iter()
            .enumerate()
            .filter(|(index, rule)| {
                rule.trigger == MachineTrigger::Event(event)
                    && path.contains(&self.rules[*index].source)
            })
            .map(|(index, _)| index)
            .collect();
        let depth = |index: usize| {
            self.rules[index]
                .owner
                .map_or(0, |owner| self.ancestors(owner).len())
        };
        rules.sort_by_key(|index| {
            (
                std::cmp::Reverse(depth(*index)),
                machine.transitions[*index].span.start,
            )
        });
        if let Some(priority) = rules.iter().find_map(|index| {
            machine.transitions[*index]
                .guard
                .is_none()
                .then_some(depth(*index))
        }) {
            rules.retain(|index| depth(*index) >= priority);
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

    pub(super) fn is_parallel(&self, node: NodeId) -> bool {
        self.nodes.iter().any(|child| {
            child.parent == Some(node) && matches!(child.declaration, Declaration::Region { .. })
        })
    }

    pub(super) fn container(&self, node: NodeId) -> Option<NodeId> {
        self.ancestors(node)
            .into_iter()
            .skip(1)
            .find(|id| matches!(self.nodes[id.0].declaration, Declaration::Region { .. }))
    }

    pub(super) fn configurations(&self, container: Option<NodeId>) -> Vec<NodeId> {
        self.nodes
            .iter()
            .enumerate()
            .filter_map(|(index, node)| {
                let id = NodeId(index);
                ((matches!(node.declaration, Declaration::Leaf(_)) || self.is_parallel(id))
                    && self.container(id) == container)
                    .then_some(id)
            })
            .collect()
    }

    pub(super) fn configuration_path(&self, node: NodeId) -> Vec<NodeId> {
        let mut path = self
            .ancestors(node)
            .into_iter()
            .filter(|id| *id == node || self.is_parallel(*id))
            .collect::<Vec<_>>();
        path.reverse();
        path
    }

    pub(super) fn completion_owner(&self, rule: usize) -> NodeId {
        self.rules[rule].owner.expect("checked completion source")
    }

    pub(super) fn is_final(&self, machine: &MachineDecl, node: NodeId) -> bool {
        matches!(self.nodes[node.0].declaration, Declaration::Leaf(index) if machine.states[index].is_final)
    }

    pub(super) fn parent(&self, node: NodeId) -> Option<NodeId> {
        self.nodes[node.0].parent
    }

    pub(super) fn regional_owner(&self, rule: usize) -> bool {
        self.rules[rule].owner.is_some_and(|owner| {
            matches!(self.nodes[owner.0].declaration, Declaration::Region { .. })
                || self.container(owner).is_some()
        })
    }

    pub(super) fn ordinal(&self, rule: usize) -> usize {
        self.rules[rule].ordinal
    }

    pub(super) fn candidate_order(
        &self,
        machine: &MachineDecl,
        trigger: MachineTrigger,
    ) -> Vec<usize> {
        let mut rules = machine
            .transitions
            .iter()
            .enumerate()
            .filter_map(|(index, rule)| (rule.trigger == trigger).then_some(index))
            .collect::<Vec<_>>();
        rules.sort_by_key(|index| {
            (
                std::cmp::Reverse(
                    self.rules[*index]
                        .owner
                        .map_or(0, |owner| self.ancestors(owner).len()),
                ),
                machine.transitions[*index].span.start,
            )
        });
        rules
    }

    pub(super) fn write_region(&self, rule: usize) -> Option<NodeId> {
        let rule = &self.rules[rule];
        rule.target?;
        let domain = rule.domain?;
        self.ancestors(domain)
            .into_iter()
            .find(|node| matches!(self.nodes[node.0].declaration, Declaration::Region { .. }))
    }

    pub(super) fn conflicts(&self, first: usize, second: usize) -> bool {
        if self.ordinal(first) == self.ordinal(second) {
            return true;
        }
        match (self.write_region(first), self.write_region(second)) {
            (Some(a), Some(b)) => {
                a == b || self.ancestors(a).contains(&b) || self.ancestors(b).contains(&a)
            }
            _ => true,
        }
    }

    pub(super) fn hook_nodes(&self) -> Vec<NodeId> {
        self.nodes
            .iter()
            .enumerate()
            .filter_map(|(index, node)| {
                (!matches!(node.declaration, Declaration::Region { .. })).then_some(NodeId(index))
            })
            .collect()
    }

    /// Canonical order for hooks and observed departures at a transition boundary.
    pub(super) fn boundary_order_key(
        &self,
        machine: &MachineDecl,
        node: NodeId,
        entry: bool,
    ) -> (usize, usize) {
        let depth = self.ancestors(node).len();
        (
            if entry { depth } else { usize::MAX - depth },
            self.span(machine, node).start,
        )
    }

    pub(super) fn configurations_compatible(&self, first: NodeId, second: NodeId) -> bool {
        let choices = |node| {
            self.configurations(self.container(node))
                .into_iter()
                .filter(|config| self.ancestors(*config).contains(&node))
                .collect::<Vec<_>>()
        };
        choices(first).into_iter().any(|first| {
            choices(second).into_iter().any(|second| {
                let first = self.configuration_path(first);
                let second = self.configuration_path(second);
                !first.iter().any(|a| {
                    second
                        .iter()
                        .any(|b| a != b && self.container(*a) == self.container(*b))
                })
            })
        })
    }

    pub(super) fn crosses(&self, rule: usize, node: NodeId) -> bool {
        let rule = &self.rules[rule];
        if rule.target == Some(rule.source)
            && rule.domain == Some(rule.source)
            && !self.is_parallel(rule.source)
        {
            return false;
        }
        match rule.domain {
            None => true,
            Some(domain) => node != domain && self.ancestors(node).contains(&domain),
        }
    }

    fn insert(
        &mut self,
        name: Ident,
        span: Span,
        declaration: Declaration,
        fields: Vec<(Ident, Spanned<TypeExpr>)>,
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
            fields,
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
            let declared = &self.nodes[owner.0].fields;
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

    pub(super) fn parallel_groups(&self, machine: &MachineDecl) -> Vec<NodeId> {
        machine
            .composite_groups
            .iter()
            .filter(|group| !group.regions.is_empty())
            .filter_map(|group| self.names.get(&group.name).copied())
            .collect()
    }

    pub(super) fn group_regions(&self, machine: &MachineDecl, group: NodeId) -> Vec<NodeId> {
        let Declaration::Compound(index) = self.nodes[group.0].declaration else {
            return Vec::new();
        };
        machine.composite_groups[index]
            .regions
            .iter()
            .filter_map(|region| {
                self.regions
                    .get(&(machine.composite_groups[index].name, region.name))
                    .copied()
            })
            .collect()
    }

    /// Fields owned inside the region, excluding shared parent payload.
    pub(super) fn region_state_fields(
        &self,
        state: Ident,
        region: NodeId,
    ) -> Vec<(Ident, Spanned<TypeExpr>)> {
        let mut fields = self
            .ancestors(*self.names.get(&state).expect("checked state declaration"))
            .into_iter()
            .take_while(|node| *node != region)
            .collect::<Vec<_>>();
        fields.reverse();
        fields
            .into_iter()
            .flat_map(|node| self.nodes[node.0].fields.clone())
            .collect()
    }

    pub(super) fn initial_value<'a>(
        &self,
        machine: &'a MachineDecl,
        node: NodeId,
    ) -> Option<&'a Spanned<Expr>> {
        match self.nodes[node.0].declaration {
            Declaration::Region { group, region } => machine.composite_groups[group].regions
                [region]
                .initial_value
                .as_ref(),
            Declaration::Compound(group) => machine.composite_groups[group].initial_value.as_ref(),
            Declaration::Leaf(_) => None,
        }
    }

    pub(super) fn has_initial_value(&self, machine: &MachineDecl, mut node: NodeId) -> bool {
        loop {
            if self.initial_value(machine, node).is_some() {
                return true;
            }
            let Some(child) = self.nodes[node.0].initial else {
                return false;
            };
            node = child;
        }
    }

    pub(super) fn initial_child(&self, node: NodeId) -> Option<NodeId> {
        self.nodes[node.0].initial
    }

    pub(super) fn children(&self, parent: NodeId) -> Vec<NodeId> {
        self.nodes
            .iter()
            .enumerate()
            .filter_map(|(index, node)| (node.parent == Some(parent)).then_some(NodeId(index)))
            .collect()
    }

    pub(super) fn own_fields(&self, node: NodeId) -> &[(Ident, Spanned<TypeExpr>)] {
        &self.nodes[node.0].fields
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
            Declaration::Region { group, region } => {
                machine.composite_groups[group].regions[region].name
            }
        }
    }

    pub(super) fn span(&self, machine: &MachineDecl, node: NodeId) -> Span {
        match self.nodes[node.0].declaration {
            Declaration::Leaf(index) => machine.states[index].span.clone(),
            Declaration::Compound(index) => machine.composite_groups[index].span.clone(),
            Declaration::Region { group, region } => {
                machine.composite_groups[group].regions[region].span.clone()
            }
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
        let common = self
            .ancestors(target)
            .into_iter()
            .find(|node| source_path.contains(node));
        // Crossing regions retires the complete parallel configuration.
        if let Some(common) = common {
            if self.is_parallel(common) && source != common && target != common {
                return self.nodes[common.0].parent;
            }
        }
        common
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
            Declaration::Region { .. } => return None,
        };
        if entry {
            enter.as_ref()
        } else {
            exit.as_ref()
        }
    }
}

pub(super) fn rewrite_target(expr: &mut Expr, target: Ident, initial: Ident, machine: Ident) {
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
