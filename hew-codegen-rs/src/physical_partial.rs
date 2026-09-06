//! Root aliases and compiler-local initialization bits from physical MIR.

use std::collections::{btree_map::Entry, BTreeMap};

use hew_mir::physical::{SemParamPassing, StorageOrigin};
use inkwell::builder::Builder;
use inkwell::values::{FunctionValue, IntValue, PointerValue};

use super::{
    callable, llvm_type, CodegenError, CodegenResult, FunctionEmitter, LlvmResultExt,
    ModuleEmitter, PhysicalCallable, PhysicalFunction, StorageId,
};

pub(super) fn allocate_storage<'ctx>(
    module: &ModuleEmitter<'ctx, '_>,
    function: &PhysicalFunction,
    signature: &PhysicalCallable,
    value: FunctionValue<'ctx>,
    builder: &Builder<'ctx>,
) -> CodegenResult<Vec<PointerValue<'ctx>>> {
    let mut slots = function
        .storage
        .iter()
        .map(|storage| {
            if matches!(storage.origin, StorageOrigin::Aggregate(_)) {
                return Ok(None);
            }
            if let StorageOrigin::ActorState { state, field } = storage.origin {
                let index = function
                    .parameters
                    .iter()
                    .position(|id| *id == state)
                    .ok_or_else(|| {
                        CodegenError::FailClosed("actor state is not a receiver parameter".into())
                    })?;
                let incoming = value
                    .get_nth_param(u32::try_from(index).map_err(|_| {
                        CodegenError::FailClosed("receiver index exceeds u32".into())
                    })?)
                    .ok_or_else(|| CodegenError::FailClosed("missing actor state receiver".into()))?
                    .into_pointer_value();
                return builder
                    .build_struct_gep(
                        llvm_type(module.ctx, &signature.params[index].layout.repr)?
                            .into_struct_type(),
                        incoming,
                        field,
                        "actor.state.field",
                    )
                    .llvm_ctx("address exclusive actor state field")
                    .map(Some);
            }
            if matches!(storage.origin, StorageOrigin::Capture { .. }) {
                return callable::capture_parameter_slot(
                    module, function, signature, value, builder, storage,
                )
                .map(Some);
            }
            if let Some((index, _)) = function
                .parameters
                .iter()
                .zip(&signature.params)
                .enumerate()
                .find(|(_, (id, param))| {
                    **id == storage.id && param.passing == SemParamPassing::BorrowMut
                })
            {
                return value
                    .get_nth_param(u32::try_from(index).map_err(|_| {
                        CodegenError::FailClosed("parameter index exceeds u32".into())
                    })?)
                    .map(|parameter| Some(parameter.into_pointer_value()))
                    .ok_or_else(|| {
                        CodegenError::FailClosed("missing exclusive parameter address".into())
                    });
            }
            let slot = builder
                .build_alloca(
                    llvm_type(module.ctx, &storage.layout.repr)?,
                    &format!("s{}", storage.id.0),
                )
                .llvm_ctx("allocate physical storage")?;
            slot.as_instruction()
                .ok_or_else(|| {
                    CodegenError::FailClosed(
                        "physical storage allocation is not an instruction".into(),
                    )
                })?
                .set_alignment(storage.layout.align)
                .map_err(|error| CodegenError::FailClosed(error.to_string()))?;
            Ok(Some(slot))
        })
        .collect::<CodegenResult<Vec<_>>>()?;
    for (&id, projection) in &function.place_storage {
        if projection.path.is_empty() {
            continue;
        }
        let mut address = slots
            .get(projection.root.0 as usize)
            .copied()
            .flatten()
            .ok_or_else(|| {
                CodegenError::FailClosed("aggregate projection lacks root storage".into())
            })?;
        for step in &projection.path {
            let glue = module
                .module
                .aggregate_glue
                .get(step.glue.0 as usize)
                .filter(|glue| glue.id == step.glue)
                .ok_or_else(|| {
                    CodegenError::FailClosed("aggregate path lacks its target glue".into())
                })?;
            let layout = module.module.target.layout(&glue.ty).ok_or_else(|| {
                CodegenError::FailClosed("aggregate path lacks its target layout".into())
            })?;
            address = builder
                .build_struct_gep(
                    llvm_type(module.ctx, &layout.repr)?.into_struct_type(),
                    address,
                    step.field,
                    "aggregate.field",
                )
                .llvm_ctx("address verified aggregate projection")?;
        }
        slots[id.0 as usize] = Some(address);
    }
    slots
        .into_iter()
        .enumerate()
        .map(|(id, slot)| {
            slot.ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "physical storage {id} has no allocation or alias"
                ))
            })
        })
        .collect()
}

pub(super) fn allocate_flags<'ctx>(
    module: &ModuleEmitter<'ctx, '_>,
    function: &PhysicalFunction,
    builder: &Builder<'ctx>,
) -> CodegenResult<BTreeMap<StorageId, PointerValue<'ctx>>> {
    let mut flags = BTreeMap::new();
    for projection in function.place_storage.values() {
        for leaf in &projection.leaves {
            if let Entry::Vacant(entry) = flags.entry(leaf.storage) {
                let flag = builder
                    .build_alloca(
                        module.ctx.bool_type(),
                        &format!("s{}.initialized", leaf.storage.0),
                    )
                    .llvm_ctx("allocate aggregate leaf initialization")?;
                let initialized = function.parameters.contains(&projection.root);
                builder
                    .build_store(
                        flag,
                        module
                            .ctx
                            .bool_type()
                            .const_int(u64::from(initialized), false),
                    )
                    .llvm_ctx("initialize aggregate leaf state")?;
                entry.insert(flag);
            }
        }
    }
    Ok(flags)
}

impl<'ctx> FunctionEmitter<'_, 'ctx> {
    pub(super) fn place_flag(&self, leaf: StorageId) -> CodegenResult<PointerValue<'ctx>> {
        self.place_flags.get(&leaf).copied().ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "aggregate leaf {} lacks initialization storage",
                leaf.0
            ))
        })
    }

    pub(super) fn place_initialized(&self, leaf: StorageId) -> CodegenResult<IntValue<'ctx>> {
        self.builder
            .build_load(
                self.ctx.bool_type(),
                self.place_flag(leaf)?,
                "aggregate.initialized",
            )
            .llvm_ctx("load aggregate leaf initialization")
            .map(|value| value.into_int_value())
    }

    pub(super) fn set_place_initialized(
        &self,
        id: StorageId,
        initialized: bool,
    ) -> CodegenResult<()> {
        if let Some(projection) = self.function.place_storage.get(&id) {
            for leaf in &projection.leaves {
                self.builder
                    .build_store(
                        self.place_flag(leaf.storage)?,
                        self.ctx
                            .bool_type()
                            .const_int(u64::from(initialized), false),
                    )
                    .llvm_ctx("publish aggregate leaf initialization")?;
            }
        }
        Ok(())
    }

    pub(super) fn destroy_place_contents(&self, source: StorageId) -> CodegenResult<bool> {
        let Some(projection) = self.function.place_storage.get(&source) else {
            return Ok(false);
        };
        for leaf in projection.leaves.iter().rev() {
            if let Some(action) = leaf.destroy {
                let initialized = self.place_initialized(leaf.storage)?;
                let drop = self.ctx.append_basic_block(self.value, "aggregate.drop");
                let next = self.ctx.append_basic_block(self.value, "aggregate.next");
                self.builder
                    .build_conditional_branch(initialized, drop, next)
                    .llvm_ctx("test aggregate leaf initialization before destruction")?;
                self.builder.position_at_end(drop);
                self.set_place_initialized(leaf.storage, false)?;
                let value = self.load(leaf.storage, "aggregate.drop.value")?;
                self.value_emitter().destroy_loaded_value(
                    value,
                    &self.storage(leaf.storage)?.layout,
                    action,
                )?;
                self.builder
                    .build_unconditional_branch(next)
                    .llvm_ctx("finish aggregate leaf destruction")?;
                self.builder.position_at_end(next);
            } else {
                self.set_place_initialized(leaf.storage, false)?;
            }
        }
        Ok(true)
    }
}
