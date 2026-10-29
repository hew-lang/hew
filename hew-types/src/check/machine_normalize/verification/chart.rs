//! A second derivation from authored declarations checks the hierarchy used by
//! SMT selection. These are exhaustive finite relation checks, not sampled
//! executions and not an assumption that the compiler's helper rules are right.
use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicUsize, Ordering};

use super::{Hierarchy, Ident, MachineDecl, NodeId};
use hew_parser::ast::{sym, MachineTransition, Spanned, TypeExpr};

pub(super) static CHARTS: AtomicUsize = AtomicUsize::new(0);
pub(super) static REJECTED: AtomicUsize = AtomicUsize::new(0);

#[derive(Clone, Copy, PartialEq, Eq)]
enum Kind {
    Leaf,
    Exclusive,
    Parallel,
    Region,
}
struct Node {
    id: NodeId,
    name: Ident,
    parent: Option<usize>,
    initial: Option<usize>,
    kind: Kind,
    fields: Vec<(Ident, Spanned<TypeExpr>)>,
    initial_value: bool,
}
struct Chart {
    nodes: Vec<Node>,
    names: HashMap<Ident, usize>,
    regions: HashMap<(Ident, Ident), usize>,
}

fn require(condition: bool, message: impl FnOnce() -> String) -> Result<(), String> {
    if !condition {
        return Err(message());
    }
    Ok(())
}

impl Chart {
    fn new(machine: &MachineDecl, hierarchy: &Hierarchy) -> Result<Self, String> {
        let mut chart = Self {
            nodes: Vec::new(),
            names: HashMap::new(),
            regions: HashMap::new(),
        };
        for (name, fields, kind, initial_value) in machine
            .states
            .iter()
            .map(|state| (state.name, &state.fields, Kind::Leaf, false))
            .chain(machine.composite_groups.iter().map(|group| {
                (
                    group.name,
                    &group.fields,
                    if group.regions.is_empty() {
                        Kind::Exclusive
                    } else {
                        Kind::Parallel
                    },
                    group.initial_value.is_some(),
                )
            }))
        {
            let id = hierarchy
                .resolve(name, 0..0)
                .map_err(|e| format!("missing source state {name}: {e:?}"))?;
            chart.names.insert(name, chart.nodes.len());
            chart.nodes.push(Node {
                id,
                name,
                parent: None,
                initial: None,
                kind,
                fields: fields.clone(),
                initial_value,
            });
        }
        for group in &machine.composite_groups {
            let parent = chart.names[&group.name];
            if group.regions.is_empty() {
                for member in &group.members {
                    chart.nodes[chart.names[member]].parent = Some(parent);
                }
                chart.nodes[parent].initial = group.initial.map(|name| chart.names[&name]);
            } else {
                let ids = hierarchy.group_regions(machine, chart.nodes[parent].id);
                for region in &group.regions {
                    let id = ids
                        .iter()
                        .find(|id| hierarchy.name(machine, **id) == region.name)
                        .copied()
                        .ok_or_else(|| format!("missing region {}.{}", group.name, region.name))?;
                    let index = chart.nodes.len();
                    chart.regions.insert((group.name, region.name), index);
                    chart.nodes.push(Node {
                        id,
                        name: region.name,
                        parent: Some(parent),
                        initial: Some(chart.names[&region.initial]),
                        kind: Kind::Region,
                        fields: Vec::new(),
                        initial_value: region.initial_value.is_some(),
                    });
                    for member in &region.members {
                        chart.nodes[chart.names[member]].parent = Some(index);
                    }
                }
            }
        }
        let actual: HashSet<_> = hierarchy.verification_nodes().into_iter().collect();
        let expected: HashSet<_> = chart.nodes.iter().map(|node| node.id).collect();
        require(
            expected.len() == chart.nodes.len() && expected == actual,
            || "state and region identities are not a bijection with the authored chart".into(),
        )?;
        Ok(chart)
    }

    fn path(&self, mut node: usize) -> Vec<usize> {
        let mut path = vec![node];
        while let Some(parent) = self.nodes[node].parent {
            assert!(!path.contains(&parent), "source hierarchy must be acyclic");
            path.push(parent);
            node = parent;
        }
        path.reverse();
        path
    }
    fn contains(&self, ancestor: usize, node: usize) -> bool {
        self.path(node).contains(&ancestor)
    }
    fn region(&self, node: usize) -> Option<usize> {
        self.path(node)
            .into_iter()
            .rev()
            .skip(1)
            .find(|id| self.nodes[*id].kind == Kind::Region)
    }
    fn configurations(&self, container: Option<usize>) -> Vec<usize> {
        (0..self.nodes.len())
            .filter(|node| {
                matches!(self.nodes[*node].kind, Kind::Leaf | Kind::Parallel)
                    && self.region(*node) == container
            })
            .collect()
    }
    fn focus(&self, node: usize) -> Vec<usize> {
        self.path(node)
            .into_iter()
            .filter(|id| *id == node || self.nodes[*id].kind == Kind::Parallel)
            .collect()
    }
    fn leaf(&self, mut node: usize) -> usize {
        while let Some(child) = self.nodes[node].initial {
            node = child;
        }
        node
    }
    fn domain(&self, source: usize, target: usize, reenter: bool) -> Option<usize> {
        let common = self
            .path(source)
            .into_iter()
            .zip(self.path(target))
            .take_while(|(a, b)| a == b)
            .last()
            .map(|(id, _)| id);
        if reenter && self.contains(target, source) {
            return self.nodes[target].parent;
        }
        match common {
            Some(id) if self.nodes[id].kind == Kind::Parallel && id != source && id != target => {
                self.nodes[id].parent
            }
            other => other,
        }
    }
    fn compatible(&self, a: usize, b: usize) -> bool {
        self.configurations(self.region(a))
            .into_iter()
            .filter(|leaf| self.contains(a, *leaf))
            .any(|left| {
                self.configurations(self.region(b))
                    .into_iter()
                    .filter(|leaf| self.contains(b, *leaf))
                    .any(|right| {
                        let choices: HashMap<_, _> = self
                            .focus(left)
                            .into_iter()
                            .map(|node| (self.region(node), node))
                            .collect();
                        self.focus(right).into_iter().all(|node| {
                            choices
                                .get(&self.region(node))
                                .is_none_or(|other| *other == node)
                        })
                    })
            })
    }
    fn index(&self, id: NodeId) -> usize {
        self.nodes
            .iter()
            .position(|node| node.id == id)
            .expect("checked node bijection")
    }
    fn ids(&self, nodes: impl IntoIterator<Item = usize>) -> HashSet<NodeId> {
        nodes.into_iter().map(|node| self.nodes[node].id).collect()
    }
    fn resolve(&self, machine: &MachineDecl, name: Ident, path: &[Ident]) -> Result<usize, String> {
        let node = *self
            .names
            .get(&name)
            .ok_or_else(|| format!("unknown state {name}"))?;
        let mut qualified: Vec<_> = self
            .path(node)
            .into_iter()
            .map(|id| self.nodes[id].name)
            .collect();
        if path.first() == Some(&machine.name) {
            qualified.insert(0, machine.name);
        }
        require(qualified.ends_with(path), || {
            format!("source path {path:?} does not name {qualified:?}")
        })?;
        Ok(node)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "pointwise hierarchy refinement keeps each finite relation visible"
    )]
    fn check_nodes(&self, machine: &MachineDecl, h: &Hierarchy) -> Result<(), String> {
        require(
            h.hook_nodes().into_iter().collect::<HashSet<_>>()
                == self.ids(
                    (0..self.nodes.len()).filter(|node| self.nodes[*node].kind != Kind::Region),
                ),
            || "hook owners differ from authored state declarations".into(),
        )?;
        for (index, node) in self.nodes.iter().enumerate() {
            let label = || format!("hierarchy projection disagrees at {} ({index})", node.name);
            for entry in [false, true] {
                let expected = if node.kind == Kind::Region {
                    None
                } else {
                    machine
                        .states
                        .iter()
                        .find(|state| state.name == node.name)
                        .and_then(|state| {
                            if entry {
                                state.entry.as_ref()
                            } else {
                                state.exit.as_ref()
                            }
                        })
                        .or_else(|| {
                            machine
                                .composite_groups
                                .iter()
                                .find(|group| group.name == node.name)
                                .and_then(|group| {
                                    if entry {
                                        group.entry.as_ref()
                                    } else {
                                        group.exit.as_ref()
                                    }
                                })
                        })
                };
                require(h.hooks(machine, node.id, entry) == expected, label)?;
            }
            require(
                h.parent(node.id) == node.parent.map(|p| self.nodes[p].id),
                label,
            )?;
            require(
                h.initial_child(node.id) == node.initial.map(|p| self.nodes[p].id),
                label,
            )?;
            require(h.leaf(node.id) == self.nodes[self.leaf(index)].id, label)?;
            require(
                h.container(node.id) == self.region(index).map(|p| self.nodes[p].id),
                label,
            )?;
            require(
                h.is_parallel(node.id) == (node.kind == Kind::Parallel),
                label,
            )?;
            require(
                h.name(machine, node.id) == node.name && h.own_fields(node.id) == node.fields,
                label,
            )?;
            let depth = self.path(index).len();
            for entry in [false, true] {
                require(
                    h.boundary_order_key(machine, node.id, entry)
                        == (
                            if entry { depth } else { usize::MAX - depth },
                            h.span(machine, node.id).start,
                        ),
                    label,
                )?;
            }
            require(
                h.configuration_path(node.id)
                    == self
                        .focus(index)
                        .into_iter()
                        .map(|p| self.nodes[p].id)
                        .collect::<Vec<_>>(),
                label,
            )?;
            require(
                h.ancestors(node.id)
                    .into_iter()
                    .rev()
                    .eq(self.path(index).into_iter().map(|p| self.nodes[p].id)),
                label,
            )?;
            require(
                h.children(node.id).into_iter().collect::<HashSet<_>>()
                    == self.ids(
                        (0..self.nodes.len())
                            .filter(|child| self.nodes[*child].parent == Some(index)),
                    ),
                label,
            )?;
            require(
                h.configurations(Some(node.id))
                    .into_iter()
                    .collect::<HashSet<_>>()
                    == self.ids(self.configurations(Some(index))),
                label,
            )?;
            let inherited: Vec<_> = self
                .path(index)
                .into_iter()
                .flat_map(|p| self.nodes[p].fields.clone())
                .collect();
            require(
                h.inherited_fields(machine, node.id)
                    .is_ok_and(|fields| fields == inherited),
                label,
            )?;
            let final_state = machine
                .states
                .iter()
                .find(|state| state.name == node.name)
                .is_some_and(|state| state.is_final);
            require(h.is_final(machine, node.id) == final_state, label)?;
            let mut initial = Some(index);
            let mut has_value = false;
            while let Some(current) = initial {
                has_value |= self.nodes[current].initial_value;
                initial = self.nodes[current].initial;
            }
            require(h.has_initial_value(machine, node.id) == has_value, label)?;
            if node.kind == Kind::Leaf {
                let actual = &machine
                    .states
                    .iter()
                    .find(|state| state.name == node.name)
                    .expect("source leaf")
                    .fields;
                // Source fields remain unexpanded; resolved fields are checked below.
                require(actual == &node.fields, label)?;
            }
            for other in 0..self.nodes.len() {
                require(
                    h.configurations_compatible(node.id, self.nodes[other].id)
                        == self.compatible(index, other),
                    label,
                )?;
                for reenter in [false, true] {
                    require(
                        h.domain(node.id, self.nodes[other].id, reenter)
                            == self.domain(index, other, reenter).map(|p| self.nodes[p].id),
                        label,
                    )?;
                }
            }
        }
        require(
            h.configurations(None).into_iter().collect::<HashSet<_>>()
                == self.ids(self.configurations(None)),
            || "root state representation is incomplete".into(),
        )?;
        Ok(())
    }

    #[expect(
        clippy::too_many_lines,
        reason = "check authored coverage, domains and conflict relations together"
    )]
    fn check_rules(
        &self,
        source: &MachineDecl,
        resolved: &MachineDecl,
        h: &Hierarchy,
    ) -> Result<(), String> {
        let mut authored: Vec<(&MachineTransition, Option<usize>)> =
            source.transitions.iter().map(|rule| (rule, None)).collect();
        for group in &source.composite_groups {
            authored.extend(
                group
                    .parent_transitions
                    .iter()
                    .map(|rule| (rule, Some(self.names[&group.name]))),
            );
            for region in &group.regions {
                authored.extend(
                    region
                        .parent_transitions
                        .iter()
                        .map(|rule| (rule, Some(self.regions[&(group.name, region.name)]))),
                );
            }
        }
        authored.sort_by_key(|(rule, _)| rule.span.start);
        require(
            h.verification_rule_count() == resolved.transitions.len(),
            || "resolved rule metadata has a different cardinality".into(),
        )?;
        let mut covered = HashSet::new();
        for (ordinal, (rule, scope)) in authored.into_iter().enumerate() {
            let owner = if rule.source_state.name == sym::UNDERSCORE {
                scope
            } else {
                Some(self.resolve(source, rule.source_state, &rule.source_path)?)
            };
            let target = if rule.target_state.name == sym::UNDERSCORE {
                None
            } else {
                Some(self.resolve(source, rule.target_state, &rule.target_path)?)
            };
            let container = owner.and_then(|id| {
                if self.nodes[id].kind == Kind::Region {
                    Some(id)
                } else {
                    self.region(id)
                }
            });
            let expected: HashSet<_> = self
                .configurations(container)
                .into_iter()
                .filter(|leaf| owner.is_none_or(|id| self.contains(id, *leaf)))
                .collect();
            let mut actual = HashSet::new();
            for (index, expanded) in resolved
                .transitions
                .iter()
                .enumerate()
                .filter(|(index, _)| h.ordinal(*index) == ordinal)
            {
                let label =
                    || format!("authored rule {ordinal} expansion {index} changed semantics");
                covered.insert(index);
                let (state, domain) = h.boundary(index);
                let leaf = self.index(state);
                require(actual.insert(leaf), || {
                    format!("authored rule {ordinal} repeats a source configuration")
                })?;
                require(
                    expected.contains(&leaf) && expanded.source_state == self.nodes[leaf].name,
                    label,
                )?;
                let expected_target = if let Some(target) = target {
                    let mut current = Some(target);
                    let mut custom = false;
                    while let Some(node) = current {
                        custom |= self.nodes[node].initial_value;
                        current = self.nodes[node].initial;
                    }
                    self.nodes[if rule.history || custom {
                        target
                    } else {
                        self.leaf(target)
                    }]
                    .name
                } else {
                    rule.target_state
                };
                require(
                    expanded.target_state == expected_target
                        && expanded.target_path == rule.target_path
                        && expanded.source_path == rule.source_path
                        && expanded.body_form == rule.body_form,
                    label,
                )?;
                require(
                    h.target(index) == target.map(|p| self.nodes[p].id)
                        && h.selection_owner(index) == owner.map(|p| self.nodes[p].id),
                    label,
                )?;
                require(
                    domain
                        == target
                            .and_then(|target| self.domain(leaf, target, rule.reenter))
                            .map(|p| self.nodes[p].id),
                    label,
                )?;
                require(
                    expanded.trigger == rule.trigger
                        && expanded.guard == rule.guard
                        && expanded.event_bindings == rule.event_bindings
                        && expanded.span == rule.span
                        && expanded.body.1 == rule.body.1
                        && expanded.history == rule.history
                        && expanded.reenter == rule.reenter,
                    label,
                )?;
                for (node_index, node) in self.nodes.iter().enumerate() {
                    let crossed = domain.is_none_or(|domain| {
                        self.index(domain) != node_index
                            && self.contains(self.index(domain), node_index)
                    });
                    require(h.crosses(index, node.id) == crossed, label)?;
                }
            }
            require(actual == expected, || {
                format!("authored rule {ordinal} is missing or has extra source configurations")
            })?;
        }
        require(covered.len() == resolved.transitions.len(), || {
            "resolved relation contains an unauthored transition".into()
        })?;
        for state in &resolved.states {
            let node = self.names[&state.name];
            let fields: Vec<_> = self
                .path(node)
                .into_iter()
                .flat_map(|p| self.nodes[p].fields.clone())
                .collect();
            require(state.fields == fields, || {
                format!("inherited state fields changed at {}", state.name)
            })?;
        }
        for i in 0..resolved.transitions.len() {
            let (_, domain) = h.boundary(i);
            let expected_region = h
                .target(i)
                .and(domain)
                .and_then(|id| {
                    self.path(self.index(id))
                        .into_iter()
                        .rev()
                        .find(|p| self.nodes[*p].kind == Kind::Region)
                })
                .map(|p| self.nodes[p].id);
            require(h.write_region(i) == expected_region, || {
                format!("wrong write domain for rule {i}")
            })?;
            for j in 0..resolved.transitions.len() {
                let expected = h.ordinal(i) == h.ordinal(j)
                    || match (h.write_region(i), h.write_region(j)) {
                        (Some(a), Some(b)) => {
                            self.contains(self.index(a), self.index(b))
                                || self.contains(self.index(b), self.index(a))
                        }
                        _ => true,
                    };
                require(h.conflicts(i, j) == expected, || {
                    format!("wrong conflict relation for rules {i}/{j}")
                })?;
            }
        }
        Ok(())
    }
}

fn check(
    source: &MachineDecl,
    resolved: &MachineDecl,
    hierarchy: &Hierarchy,
) -> Result<(), String> {
    let chart = Chart::new(source, hierarchy)?;
    chart.check_nodes(source, hierarchy)?;
    chart.check_rules(source, resolved, hierarchy)
}

pub(in crate::check::machine_normalize) fn verify_chart(
    source: &MachineDecl,
    resolved: &MachineDecl,
    hierarchy: &Hierarchy,
) {
    check(source, resolved, hierarchy).unwrap_or_else(|reason| {
        panic!(
            "machine {}: authored chart refinement failed: {reason}",
            source.name
        )
    });
    CHARTS.fetch_add(1, Ordering::Relaxed);
    if std::env::var_os("HEW_MACHINE_MUTATIONS").is_some() {
        for kind in 0..5 {
            if REJECTED.load(Ordering::Relaxed) & (1 << kind) != 0 {
                continue;
            }
            if let Some(changed) = hierarchy.verification_corruption(kind) {
                let reason = check(source, resolved, &changed)
                    .expect_err("corrupted source hierarchy escaped refinement checking");
                super::save(
                    &format!("mutant-hierarchy-{kind}"),
                    "counterexample",
                    &reason,
                );
                REJECTED.fetch_or(1 << kind, Ordering::Relaxed);
            }
        }
    }
}
