//! Indexed writable paths materialize semantic values and publish them from
//! the leaf outwards. Runtime calls retain ownership of COW and failure rules.
use super::{AggregateSelection, BindingPlace, BindingTarget, Builder};
use crate::{Operand, OwnKind, PlaceId, Provenance, SemOpKind, ValueId};
use hew_hir::{HirExpr, HirExprKind};
use hew_types::{ResolvedTy, RuntimeCallFamily};

pub(super) struct IndexedStep {
    expression: HirExpr,
    index: ValueId,
    read: RuntimeCallFamily,
    update: RuntimeCallFamily,
    projections: Vec<AggregateSelection>,
}

pub(super) struct WritablePath {
    pub base: BindingPlace,
    steps: Vec<IndexedStep>,
}

pub(super) struct Writeback {
    root: WritableRoot,
    frames: Vec<(IndexedStep, ValueId, ValueId)>,
}

/// Where a writable path publishes its updated leaf.
pub(super) enum WritableRoot {
    /// A place of this body, or a field place of a staged seat copy that is
    /// published back whole. `taken` marks a state seat the mutation took.
    Place {
        leaf: PlaceId,
        staged_root: Option<(PlaceId, ValueId)>,
        taken: bool,
    },
    /// A field beneath a whole owner. The mutation ran on a copy of the
    /// field, which is assigned back through the owner.
    WholeOwner { root: PlaceId, base: BindingPlace },
}

impl Builder<'_, '_> {
    pub(super) fn resolve_writable_path(
        &mut self,
        target: &HirExpr,
    ) -> Result<WritablePath, String> {
        let (root, projections) = self.projection_chain(target)?;
        if matches!(root.kind, HirExprKind::BorrowedIndex { .. }) {
            return Err("E_OWN_CONSUME_BORROWED: an affine collection element is borrowed; a writable path requires a semantic copy".into());
        }
        let HirExprKind::Index { container, index } = &root.kind else {
            return Ok(WritablePath {
                base: self.resolve_mutable_place(target)?,
                steps: Vec::new(),
            });
        };
        let mut path = self.resolve_writable_path(container)?;
        let (read, update) = self
            .service
            .module
            .indexed_place_operations
            .get(&root.site)
            .copied()
            .ok_or("indexed writable path has no checker-selected replacement contract")?;
        let index = self.lower_expr(index)?;
        path.steps.push(IndexedStep {
            expression: root.clone(),
            index,
            read,
            update,
            projections,
        });
        Ok(path)
    }

    pub(super) fn path_is_indexed(path: &WritablePath) -> bool {
        !path.steps.is_empty()
    }
    pub(super) fn path_indices(path: &WritablePath) -> Vec<ValueId> {
        path.steps.iter().map(|step| step.index).collect()
    }

    /// State and capture fields are published through their whole owning seat.
    /// Only the staged copy exposes field places to the indexed transaction.
    /// A path beneath a whole owner has neither; callers that can mutate a
    /// copy of its field check [`Self::whole_owner_root`] first.
    pub(super) fn stage_writable_root(
        &mut self,
        base: &BindingPlace,
        provenance: &Provenance,
    ) -> Result<(PlaceId, Option<(PlaceId, ValueId)>), String> {
        if let Some(root) = self.owned_projection(base)? {
            return Ok((root, None));
        }
        if self.whole_owner_root(base)?.is_some() {
            return Err(
                "a `var self` call on a field beneath an owner that has no copy or \
                 keeps one whole owner is not implemented"
                    .into(),
            );
        }
        let BindingTarget::Place(root) = self.binding_target(base.binding)? else {
            return Err("indexed writable root has no owning seat".into());
        };
        let staged = self.emit_typed(
            provenance.clone(),
            &base.root_ty,
            SemOpKind::LoadCopy { place: root },
        )?;
        let leaf = self.value_projection_place(staged, &base.root_ty, &base.projections)?;
        Ok((leaf, Some((root, staged))))
    }

    /// The container an indexed path starts from, and where the updated
    /// container is published.
    pub(super) fn stage_indexed_base(
        &mut self,
        base: &BindingPlace,
        provenance: &Provenance,
    ) -> Result<(ValueId, WritableRoot), String> {
        if let Some(root) = self.whole_owner_root(base)? {
            let container = self.copy_through_whole_owner(root, base, provenance)?;
            return Ok((
                container,
                WritableRoot::WholeOwner {
                    root,
                    base: base.clone(),
                },
            ));
        }
        let (leaf, staged_root) = self.stage_writable_root(base, provenance)?;
        let container = self.emit_typed(
            provenance.clone(),
            &base.leaf_ty,
            SemOpKind::LoadCopy { place: leaf },
        )?;
        Ok((
            container,
            WritableRoot::Place {
                leaf,
                staged_root,
                taken: false,
            },
        ))
    }

    pub(super) fn publish_writable_root(
        &mut self,
        root: PlaceId,
        replacement: ValueId,
        staged_root: Option<(PlaceId, ValueId)>,
        taken: bool,
        provenance: &Provenance,
    ) -> Result<(), String> {
        if taken {
            self.restore_taken_place(root, replacement, provenance.clone())?;
        } else {
            self.store_projected(root, replacement, provenance.clone())?;
        }
        if let Some((root, staged)) = staged_root {
            self.store_projected(root, staged, provenance.clone())?;
        }
        Ok(())
    }

    pub(super) fn publish_writable(
        &mut self,
        root: WritableRoot,
        replacement: ValueId,
        provenance: &Provenance,
    ) -> Result<(), String> {
        match root {
            WritableRoot::Place {
                leaf,
                staged_root,
                taken,
            } => self.publish_writable_root(leaf, replacement, staged_root, taken, provenance),
            WritableRoot::WholeOwner { root, base } => {
                self.assign_through_whole_owner(root, &base, replacement, provenance.clone())
            }
        }
    }

    pub(super) fn acquire_indexed_path(
        &mut self,
        path: WritablePath,
        mut container: ValueId,
        root: WritableRoot,
        take: bool,
        provenance: &Provenance,
    ) -> Result<(ValueId, Writeback), String> {
        let count = path.steps.len();
        let mut frames = Vec::with_capacity(count);
        for (at, step) in path.steps.into_iter().enumerate() {
            let HirExprKind::Index {
                container: source,
                index,
            } = &step.expression.kind
            else {
                unreachable!()
            };
            let element = self
                .lower_runtime_operation_with(
                    &step.expression,
                    step.read,
                    &[source, index],
                    true,
                    &[(0, container), (1, step.index)],
                )?
                .ok_or("indexed writable path read has no value")?;
            let leaf =
                self.read_writable_element(element, &step, take && at + 1 == count, provenance)?;
            frames.push((step, container, element));
            container = leaf;
        }
        Ok((container, Writeback { root, frames }))
    }

    fn read_writable_element(
        &mut self,
        element: ValueId,
        step: &IndexedStep,
        take: bool,
        provenance: &Provenance,
    ) -> Result<ValueId, String> {
        if step.projections.is_empty() {
            return Ok(element);
        }
        let ty = self.ty(&step.expression.ty);
        if self.value_own_kind(element) == Some(OwnKind::None) {
            let mut value = element;
            for (ty, shape, index) in &step.projections {
                value =
                    self.emit_destructure_value(value, ty, *shape, provenance.clone())?[*index].id;
            }
            return Ok(value);
        }
        let place = self.value_projection_place(element, &ty, &step.projections)?;
        let leaf_ty = self.places[place.0 as usize].ty.clone();
        self.emit_typed(
            provenance.clone(),
            &leaf_ty,
            if take {
                SemOpKind::LoadTake { place }
            } else {
                SemOpKind::LoadCopy { place }
            },
        )
    }

    pub(super) fn publish_indexed_path(
        &mut self,
        writeback: Writeback,
        mut replacement: ValueId,
        provenance: &Provenance,
    ) -> Result<(), String> {
        for (step, container, element) in writeback.frames.into_iter().rev() {
            let ty = self.ty(&step.expression.ty);
            let updated = if step.projections.is_empty() {
                if element != replacement && self.owned_live.contains_key(&element) {
                    self.emit_destroy(element)?;
                }
                replacement
            } else if self.value_own_kind(element) == Some(OwnKind::Owned) {
                self.assign_through_owned_value(
                    element,
                    &ty,
                    &step.projections,
                    replacement,
                    provenance.clone(),
                )?;
                element
            } else {
                self.replace_scalar_value_leaf(element, &step.projections, replacement, provenance)?
            };
            let HirExprKind::Index {
                container: source,
                index,
            } = &step.expression.kind
            else {
                unreachable!()
            };
            let mut operation = step.expression.clone();
            operation.ty = ResolvedTy::Unit;
            replacement = self
                .lower_runtime_operation_with(
                    &operation,
                    step.update,
                    &[source, index, &step.expression],
                    false,
                    &[(0, container), (1, step.index), (2, updated)],
                )?
                .ok_or("indexed replacement did not return its updated receiver")?;
        }
        self.publish_writable(writeback.root, replacement, provenance)
    }

    fn replace_scalar_value_leaf(
        &mut self,
        mut value: ValueId,
        projections: &[AggregateSelection],
        mut replacement: ValueId,
        provenance: &Provenance,
    ) -> Result<ValueId, String> {
        let mut parents = Vec::new();
        for (ty, shape, index) in projections {
            let fields = self.emit_destructure_value(value, ty, *shape, provenance.clone())?;
            value = fields[*index].id;
            parents.push((ty, shape, index, fields));
        }
        for (ty, shape, index, fields) in parents.into_iter().rev() {
            let fields = fields
                .into_iter()
                .enumerate()
                .map(|(at, field)| Operand {
                    value: if at == *index { replacement } else { field.id },
                })
                .collect();
            replacement = self.emit_typed(
                provenance.clone(),
                ty,
                SemOpKind::AggregateMake {
                    shape: *shape,
                    fields,
                },
            )?;
        }
        Ok(replacement)
    }
}
