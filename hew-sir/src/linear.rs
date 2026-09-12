//! Receiver identity accompanying the existing lifetime flow.
//!
//! This is provenance, not a second availability analysis. Availability still
//! decides whether a value or field is live. The paths distinguish an incoming
//! terminal receiver from unrelated linear owners packed beside it.

use std::collections::{BTreeMap, BTreeSet};

use crate::{
    AggregateShapeId, AggregateShapeRef, OwnerRoot, PlacePlan, SemFunction, SemOp, SemOpKind,
    ValueId,
};

const RECEIVER: u8 = 1;
const OTHER: u8 = 2;
type Tree = BTreeMap<Vec<u32>, u8>;

#[derive(Clone, Default)]
pub(super) struct Origins(BTreeMap<OwnerRoot, Tree>);

fn select(tree: &Tree, path: &[u32]) -> Tree {
    let mut selected = Tree::new();
    for (key, origin) in tree {
        let tail = if let Some(tail) = key.strip_prefix(path) {
            tail.to_vec()
        } else if path.starts_with(key) {
            // An opaque incoming aggregate's origin applies to its children.
            // A join with a more precise sibling path cannot erase that fact.
            Vec::new()
        } else {
            continue;
        };
        *selected.entry(tail).or_default() |= origin;
    }
    selected
}

fn nest(target: &mut Tree, field: u32, tree: Tree) {
    for (mut path, origin) in tree {
        path.insert(0, field);
        target.insert(path, origin);
    }
}

impl Origins {
    pub(super) fn parameters(function: &SemFunction, linear: &BTreeSet<ValueId>) -> Self {
        Self(
            function
                .params
                .iter()
                .filter(|param| linear.contains(&param.value))
                .map(|param| {
                    let origin = if function.terminal_receiver == Some(param.value) {
                        RECEIVER
                    } else {
                        OTHER
                    };
                    (
                        OwnerRoot::Value(param.value),
                        BTreeMap::from([(Vec::new(), origin)]),
                    )
                })
                .collect(),
        )
    }

    pub(super) fn join(&mut self, other: &Self) -> bool {
        let mut changed = false;
        for (root, tree) in &other.0 {
            let target = self.0.entry(*root).or_default();
            for (path, origin) in tree {
                let entry = target.entry(path.clone()).or_default();
                changed |= *entry | origin != *entry;
                *entry |= origin;
            }
        }
        changed
    }

    pub(super) fn remove_value(&mut self, value: ValueId) {
        self.0.remove(&OwnerRoot::Value(value));
    }

    pub(super) fn define(&mut self, value: ValueId, linear: bool) {
        self.set(
            value,
            if linear {
                BTreeMap::from([(Vec::new(), OTHER)])
            } else {
                Tree::new()
            },
        );
    }

    fn value(&self, value: ValueId) -> Tree {
        self.0
            .get(&OwnerRoot::Value(value))
            .cloned()
            .unwrap_or_default()
    }

    fn set(&mut self, value: ValueId, tree: Tree) {
        if tree.is_empty() {
            self.remove_value(value);
        } else {
            self.0.insert(OwnerRoot::Value(value), tree);
        }
    }

    pub(super) fn transfer_value(&mut self, before: &Self, source: ValueId, destination: ValueId) {
        self.set(destination, before.value(source));
    }

    fn path(plan: &PlacePlan, place: crate::PlaceId) -> Option<(OwnerRoot, Vec<u32>)> {
        plan.projection(place).map(|projection| {
            (
                projection.root,
                projection.path.iter().map(|step| step.field).collect(),
            )
        })
    }

    fn place(&self, plan: &PlacePlan, place: crate::PlaceId) -> Tree {
        let Some((root, path)) = Self::path(plan, place) else {
            return Tree::new();
        };
        self.0
            .get(&root)
            .map(|tree| select(tree, &path))
            .unwrap_or_default()
    }

    fn replace_place(
        &mut self,
        plan: &PlacePlan,
        place: crate::PlaceId,
        replacement: Tree,
        linear_places: &BTreeSet<crate::PlaceId>,
    ) {
        let Some((root, prefix)) = Self::path(plan, place) else {
            return;
        };
        // Refine an opaque boundary origin using the already-checked content
        // partition before removing one field. Otherwise its ancestor taint
        // would survive even after its last linear child was consumed.
        if let Some(leaves) = plan.leaves(root) {
            let before = self.0.get(&root).cloned().unwrap_or_default();
            let mut refined = Tree::new();
            for leaf in leaves.iter().filter(|leaf| linear_places.contains(leaf)) {
                let (_, path) = Self::path(plan, *leaf).expect("checked projected leaf");
                for (tail, origin) in select(&before, &path) {
                    let mut full = path.clone();
                    full.extend(tail);
                    *refined.entry(full).or_default() |= origin;
                }
            }
            self.0.insert(root, refined);
        }
        let tree = self.0.entry(root).or_default();
        tree.retain(|path, _| !path.starts_with(&prefix));
        for (tail, origin) in replacement {
            let mut path = prefix.clone();
            path.extend(tail);
            tree.insert(path, origin);
        }
    }

    pub(super) fn finishes_receiver(&self, kind: &SemOpKind, plan: &PlacePlan) -> bool {
        let tree = match kind {
            SemOpKind::DestroyValue { value } => self.value(value.value),
            SemOpKind::EndLifetime { place } => self.place(plan, *place),
            // Assignment is replacement, never a terminal method exit.
            _ => return false,
        };
        !tree.is_empty() && tree.values().all(|origin| *origin == RECEIVER)
    }

    pub(super) fn operation(
        &mut self,
        before: &Self,
        op: &SemOp,
        plan: &PlacePlan,
        marked: &BTreeSet<AggregateShapeId>,
        linear: &BTreeSet<ValueId>,
        linear_places: &BTreeSet<crate::PlaceId>,
    ) {
        let result = op.results.first().map(|result| result.id);
        match &op.kind {
            SemOpKind::Move { source } => {
                if let Some(result) = result {
                    self.transfer_value(before, source.value, result);
                }
            }
            SemOpKind::TupleMake { elements }
            | SemOpKind::AggregateMake {
                fields: elements, ..
            }
            | SemOpKind::ArrayMake { fields: elements }
            | SemOpKind::VariantMake {
                fields: elements, ..
            } => {
                let Some(result) = result else {
                    return;
                };
                let mut tree = Tree::new();
                for (index, operand) in elements.iter().enumerate() {
                    nest(
                        &mut tree,
                        u32::try_from(index).expect("verified field count"),
                        before.value(operand.value),
                    );
                }
                let nominal = matches!(op.kind, SemOpKind::AggregateMake { shape: AggregateShapeRef::Record(shape), .. } if marked.contains(&shape));
                if linear.contains(&result) && (nominal || tree.is_empty()) {
                    tree.insert(Vec::new(), OTHER);
                }
                self.set(result, tree);
            }
            SemOpKind::Destructure { aggregate, .. }
            | SemOpKind::VariantDestructure {
                source: aggregate, ..
            } => {
                let tree = before.value(aggregate.value);
                for (field, result) in op.results.iter().enumerate() {
                    let selected = select(
                        &tree,
                        &[u32::try_from(field).expect("verified field count")],
                    );
                    // An unexpanded incoming aggregate remains an unrelated owner.
                    if !linear.contains(&result.id) {
                        self.set(result.id, Tree::new());
                    } else if !selected.is_empty() {
                        self.set(result.id, selected);
                    }
                }
            }
            SemOpKind::LoadTake { place } => {
                if let Some(result) = result {
                    let selected = before.place(plan, *place);
                    if !linear.contains(&result) {
                        self.set(result, Tree::new());
                    } else if !selected.is_empty() {
                        self.set(result, selected);
                    }
                }
                self.replace_place(plan, *place, Tree::new(), linear_places);
            }
            SemOpKind::StoreInit { place, value } | SemOpKind::StoreAssign { place, value } => {
                self.replace_place(plan, *place, before.value(value.value), linear_places);
            }
            SemOpKind::EndLifetime { place } => {
                self.replace_place(plan, *place, Tree::new(), linear_places);
            }
            _ => {}
        }
    }
}
