//! Prove fork-transfer markers once per concrete callable origin.

use std::collections::{HashMap, VecDeque};

use super::{CallableOrigin, Checker, EffectBody, EffectGraph};
use crate::env::TypeBindingId;
use crate::traits::MarkerTrait;
use crate::Ty;

#[derive(Default)]
struct Node {
    users: Vec<usize>,
    remaining: usize,
    blocked: bool,
    proven: bool,
}

/// Marker proofs form an AND graph. A cycle cannot establish its own proof:
/// only primitive marker facts and independently proven captures seed it.
pub(super) struct MarkerSolver<'a> {
    checker: &'a Checker,
    graph: &'a EffectGraph,
    identities: HashMap<(CallableOrigin, MarkerTrait), usize>,
    origins: Vec<(CallableOrigin, MarkerTrait)>,
    nodes: Vec<Node>,
    expanded: usize,
    proven: VecDeque<usize>,
}

impl<'a> MarkerSolver<'a> {
    pub(super) fn new(checker: &'a Checker, graph: &'a EffectGraph) -> Self {
        Self {
            checker,
            graph,
            identities: HashMap::new(),
            origins: Vec::new(),
            nodes: Vec::new(),
            expanded: 0,
            proven: VecDeque::new(),
        }
    }

    fn intern(&mut self, origin: CallableOrigin, marker: MarkerTrait) -> usize {
        let key = (origin, marker);
        if let Some(id) = self.identities.get(&key) {
            return *id;
        }
        let id = self.nodes.len();
        self.identities.insert(key.clone(), id);
        self.origins.push(key);
        self.nodes.push(Node::default());
        id
    }

    fn mark(&mut self, id: usize) {
        if !self.nodes[id].proven {
            self.nodes[id].proven = true;
            self.proven.push_back(id);
        }
    }

    fn projected_origin(&self, binding: TypeBindingId, path: &[String]) -> Option<CallableOrigin> {
        let mut origin = self.graph.bindings.get(&binding)?;
        if path.is_empty() {
            return Some(origin.clone());
        }
        let mut remaining: VecDeque<_> = path.iter().cloned().collect();
        let mut aliases: HashMap<&CallableOrigin, Vec<Vec<String>>> = HashMap::new();
        loop {
            match origin {
                CallableOrigin::Binding { binding, path } => {
                    let previous = aliases.entry(origin).or_default();
                    // Re-entering this exact alias before consuming its old
                    // continuation cannot reach an independently proven leaf.
                    if previous.iter().any(|previous| {
                        remaining.len() >= previous.len()
                            && remaining
                                .iter()
                                .rev()
                                .zip(previous.iter().rev())
                                .all(|(a, b)| a == b)
                    }) {
                        return None;
                    }
                    previous.push(remaining.iter().cloned().collect());
                    for field in path.iter().rev() {
                        remaining.push_front(field.clone());
                    }
                    origin = self.graph.bindings.get(binding)?;
                }
                CallableOrigin::Aggregate(fields) => {
                    let Some(field) = remaining.pop_front() else {
                        return Some(origin.clone());
                    };
                    origin = fields.get(&field)?;
                }
                _ => return remaining.is_empty().then(|| origin.clone()),
            }
        }
    }

    fn dependencies(
        &self,
        origin: &CallableOrigin,
        marker: MarkerTrait,
    ) -> Option<Vec<CallableOrigin>> {
        match origin {
            CallableOrigin::Unknown => None,
            CallableOrigin::Typed(ty) => {
                match self.checker.subst.resolve(ty).callable_body().clone() {
                    Ty::Closure {
                        identity: EffectBody::Closure(key),
                        ..
                    } => {
                        let captures = self.checker.closure_capture_facts.get(&key)?;
                        Some(
                            captures
                                .iter()
                                .filter_map(|capture| {
                                    let proven = if marker == MarkerTrait::Sync {
                                        capture.is_sync
                                    } else {
                                        capture.is_send
                                    };
                                    (!proven).then(|| CallableOrigin::Binding {
                                        binding: capture.binding_id,
                                        path: Vec::new(),
                                    })
                                })
                                .collect(),
                        )
                    }
                    ty => self
                        .checker
                        .registry
                        .implements_marker(&ty, marker)
                        .then(Vec::new),
                }
            }
            CallableOrigin::Aggregate(fields) => Some(fields.values().cloned().collect()),
            CallableOrigin::Binding { binding, path } => {
                Some(vec![self.projected_origin(*binding, path)?])
            }
        }
    }

    pub(super) fn proves(&mut self, origin: &CallableOrigin, marker: MarkerTrait) -> bool {
        let root = self.intern(origin.clone(), marker);
        while self.expanded < self.nodes.len() {
            let id = self.expanded;
            self.expanded += 1;
            let (origin, marker) = self.origins[id].clone();
            let Some(dependencies) = self.dependencies(&origin, marker) else {
                self.nodes[id].blocked = true;
                continue;
            };
            for dependency in dependencies {
                let dependency = self.intern(dependency, marker);
                if !self.nodes[dependency].proven {
                    self.nodes[id].remaining += 1;
                    self.nodes[dependency].users.push(id);
                }
            }
            if self.nodes[id].remaining == 0 {
                self.mark(id);
            }
        }
        while let Some(id) = self.proven.pop_front() {
            let users = std::mem::take(&mut self.nodes[id].users);
            for user in users {
                self.nodes[user].remaining -= 1;
                if self.nodes[user].remaining == 0 && !self.nodes[user].blocked {
                    self.mark(user);
                }
            }
        }
        self.nodes[root].proven
    }
}
