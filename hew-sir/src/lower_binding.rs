//! Lexical binding acquisition and scope exits. The declaration stack records
//! source lifetime boundaries, never payload initialization or drop decisions.

use super::{BindingId, BindingTarget, Builder, OwnKind, PlaceId, PlaceOrigin, Provenance};
use crate::{Operand, PlaceDecl, SemOp, SemOpKind, ValueId};
use hew_types::ResolvedTy;

impl Builder<'_, '_> {
    pub(super) fn binding_target(&self, binding: BindingId) -> Result<BindingTarget, String> {
        self.bindings
            .get(&binding)
            .copied()
            .ok_or_else(|| format!("binding `{binding}` is not available in the SIR environment"))
    }

    pub(super) fn target_ty(&self, target: BindingTarget) -> Result<ResolvedTy, String> {
        match target {
            BindingTarget::Value(value) => self.value_ty(value),
            BindingTarget::Place(place) => self
                .places
                .get(place.0 as usize)
                .map(|place| place.ty.clone()),
        }
        .ok_or_else(|| "binding target has no concrete type".to_string())
    }

    pub(super) fn scalar_binding(&self, binding: BindingId) -> Result<ValueId, String> {
        match self.binding_target(binding)? {
            BindingTarget::Value(value) if self.value_own_kind(value) != Some(OwnKind::Owned) => {
                Ok(value)
            }
            _ => Err(format!("binding `{binding}` is not a non-owning SSA value")),
        }
    }

    pub(super) fn emit_place_operation(
        &mut self,
        kind: SemOpKind,
        provenance: Provenance,
    ) -> Result<(), String> {
        let operation = SemOp {
            id: crate::OpId(self.ops),
            results: Vec::new(),
            kind,
            provenance,
        };
        self.current_block_mut().append_op(operation)?;
        self.ops += 1;
        Ok(())
    }

    pub(super) fn acquire_binding_target(
        &mut self,
        value: ValueId,
    ) -> Result<BindingTarget, String> {
        if self.value_own_kind(value) != Some(OwnKind::Owned) {
            return Ok(BindingTarget::Value(value));
        }
        let ty = self
            .value_ty(value)
            .ok_or_else(|| "binding initializer has no type".to_string())?;
        let place =
            PlaceId(u32::try_from(self.places.len()).map_err(|_| "place count exceeds u32")?);
        self.places.push(PlaceDecl {
            id: place,
            ty,
            origin: PlaceOrigin::Local,
        });
        self.emit_place_operation(SemOpKind::AllocPlace { place }, Provenance::Synthesized)?;
        self.emit_place_operation(
            SemOpKind::StoreInit {
                place,
                value: Operand { value },
            },
            Provenance::Synthesized,
        )?;
        self.owned_live.remove(&value);
        Ok(BindingTarget::Place(place))
    }

    pub(super) fn declare_in_scope(&mut self, binding: BindingId) {
        self.scopes
            .last_mut()
            .expect("function declaration scope")
            .push(binding);
    }

    /// Emit an exit without changing the declaration context used to generate
    /// another successor. `EndLifetime` itself determines initialized contents.
    pub(super) fn end_scopes(&mut self, floor: usize) -> Result<(), String> {
        let bindings = self.scopes[floor..]
            .iter()
            .rev()
            .flat_map(|scope| scope.iter().rev())
            .copied()
            .collect::<Vec<_>>();
        for binding in bindings {
            if let Some(BindingTarget::Place(place)) = self.bindings.get(&binding).copied() {
                if self.places[place.0 as usize].origin == PlaceOrigin::Local {
                    self.emit_place_operation(
                        SemOpKind::EndLifetime { place },
                        Provenance::Synthesized,
                    )?;
                }
            }
        }
        Ok(())
    }

    pub(super) fn leave_scope(&mut self) {
        for binding in self.scopes.pop().expect("active declaration scope") {
            self.bindings.remove(&binding);
            self.binding_declarations.remove(&binding);
        }
    }

    pub(super) fn value_borrow_root(&self, value: ValueId) -> Result<crate::OwnerRoot, String> {
        let parent = self
            .blocks
            .iter()
            .flat_map(|block| &block.ops)
            .find(|operation| operation.results.iter().any(|result| result.id == value))
            .and_then(|operation| operation.kind.borrow_parent());
        match parent {
            Some(crate::PlaceBase::Value(parent)) => self.value_borrow_root(parent),
            Some(crate::PlaceBase::Place(place)) => {
                crate::projection::place_path(&self.places, place).map(|(root, _)| root)
            }
            None => Ok(crate::OwnerRoot::Value(value)),
        }
    }
}
