//! Indexed writable paths materialize semantic values and publish them from
//! the leaf outwards. Runtime calls retain ownership of COW and failure rules.
use super::{AggregateSelection, BindingPlace, Builder};
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
    root: PlaceId,
    frames: Vec<(IndexedStep, ValueId, ValueId)>,
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

    pub(super) fn acquire_indexed_path(
        &mut self,
        path: WritablePath,
        root: PlaceId,
        take: bool,
        provenance: &Provenance,
    ) -> Result<(ValueId, Writeback), String> {
        let mut container = self.emit_typed(
            provenance.clone(),
            &path.base.leaf_ty,
            SemOpKind::LoadCopy { place: root },
        )?;
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
        self.store_projected(writeback.root, replacement, provenance.clone())
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
