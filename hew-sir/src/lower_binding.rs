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

    pub(super) fn allocate_local(&mut self, ty: ResolvedTy) -> Result<PlaceId, String> {
        let place =
            PlaceId(u32::try_from(self.places.len()).map_err(|_| "place count exceeds u32")?);
        self.places.push(PlaceDecl {
            id: place,
            ty,
            origin: PlaceOrigin::Local,
        });
        self.emit_place_operation(SemOpKind::AllocPlace { place }, Provenance::Synthesized)?;
        Ok(place)
    }

    pub(super) fn acquire_binding_target(
        &mut self,
        value: ValueId,
    ) -> Result<BindingTarget, String> {
        if self.value_own_kind(value) != Some(OwnKind::Owned) {
            return Ok(BindingTarget::Value(value));
        }
        self.acquire_local_target(value)
    }

    /// Mutable bindings have stable storage across calls, cleanup and suspension.
    pub(super) fn acquire_local_target(&mut self, value: ValueId) -> Result<BindingTarget, String> {
        let ty = self
            .value_ty(value)
            .ok_or_else(|| "binding initializer has no type".to_string())?;
        let place = self.allocate_local(ty)?;
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

    pub(super) fn end_binding_scope(&mut self, binding: BindingId) -> Result<(), String> {
        let declaration = self.binding_declarations[&binding];
        if let BindingTarget::Place(place) = self.source_bindings[declaration].target {
            if self.places[place.0 as usize].origin == PlaceOrigin::Local {
                if self.value_needs_close(&self.places[place.0 as usize].ty) {
                    self.close_value(Some(place), None)?;
                }
                self.emit_place_operation(
                    SemOpKind::EndLifetime { place },
                    Provenance::Synthesized,
                )?;
            }
        }
        Ok(())
    }

    /// Emit an exit without changing the declaration context used to generate
    /// another successor. `EndLifetime` itself determines initialized contents.
    pub(super) fn end_scopes(&mut self, floor: usize) -> Result<(), String> {
        self.drain_scopes(floor, true)
    }

    pub(super) fn leave_scope(&mut self) {
        for binding in self.scopes.pop().expect("active declaration scope") {
            self.bindings.remove(&binding);
            self.binding_declarations.remove(&binding);
        }
    }

    pub(super) fn value_borrow_root(&self, mut value: ValueId) -> Result<crate::OwnerRoot, String> {
        loop {
            match self.borrow_parents.get(&value).copied() {
                Some(crate::PlaceBase::Value(parent)) => value = parent,
                Some(crate::PlaceBase::Place(place)) => {
                    return match self.places[place.0 as usize].origin {
                        PlaceOrigin::Capture { environment, .. }
                        | PlaceOrigin::ActorState {
                            state: environment, ..
                        } => Ok(crate::OwnerRoot::Value(environment)),
                        _ => {
                            crate::projection::place_path(&self.places, place).map(|(root, _)| root)
                        }
                    };
                }
                None => return Ok(crate::OwnerRoot::Value(value)),
            }
        }
    }
}
