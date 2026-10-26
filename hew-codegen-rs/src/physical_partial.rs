//! Root aliases and compiler-local initialization bits from physical MIR.

use std::collections::{btree_map::Entry, BTreeMap};

use hew_mir::physical::{LeafContents, OwnKind, PhysicalCleanup, SemParamPassing, StorageOrigin};
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
    let addressing = builder;
    let entry = value
        .get_first_basic_block()
        .ok_or_else(|| CodegenError::FailClosed("physical function has no entry block".into()))?;
    let mut slots = function
        .storage
        .iter()
        .map(|storage| {
            if matches!(storage.origin, StorageOrigin::Aggregate(_)) {
                return Ok(None);
            }
            if matches!(storage.origin, StorageOrigin::Session { .. }) {
                return super::session::allocate_slot(module, function, value, builder, storage)
                    .map(Some);
            }
            if let StorageOrigin::ActorState { state, field, .. } = storage.origin {
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
                return addressing
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
            if let StorageOrigin::Capture { .. } = storage.origin {
                let slot = callable::capture_parameter_slot(
                    module, function, signature, value, builder, storage,
                )?;
                return Ok(Some(slot));
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
            let ty = llvm_type(module.ctx, &storage.layout.repr)?;
            let name = format!("s{}", storage.id.0);
            let slot = super::coro::entry_storage(module.ctx, entry, ty, &name)?;
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
            address = addressing
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
            let slot = slot.ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "physical storage {id} has no allocation or alias"
                ))
            })?;
            Ok(slot)
        })
        .collect()
}

pub(super) fn allocate_flags<'ctx>(
    module: &ModuleEmitter<'ctx, '_>,
    function: &PhysicalFunction,
    builder: &Builder<'ctx>,
    slots: &[PointerValue<'ctx>],
) -> CodegenResult<BTreeMap<StorageId, PointerValue<'ctx>>> {
    let addressing = builder;
    let entry_block = builder
        .get_insert_block()
        .and_then(|block| block.get_parent())
        .and_then(|function| function.get_first_basic_block())
        .ok_or_else(|| CodegenError::FailClosed("physical function has no entry block".into()))?;
    let mut flags = BTreeMap::new();
    for storage in &function.storage {
        if matches!(storage.origin, StorageOrigin::Session { .. }) {
            let value = entry_block
                .get_parent()
                .ok_or_else(|| CodegenError::FailClosed("session entry has no function".into()))?;
            flags.extend(super::session::register_leaves(
                module, function, value, builder, slots, storage,
            )?);
        }
    }
    for projection in function.place_storage.values() {
        for leaf in &projection.leaves {
            if let Entry::Vacant(entry) = flags.entry(leaf.storage) {
                if let StorageOrigin::ActorState { state, field, .. } =
                    function.storage[leaf.storage.0 as usize].origin
                {
                    if !module.module.managed_types[&function.storage[leaf.storage.0 as usize].ty]
                        .tracks_occupancy()
                    {
                        continue;
                    }
                    let layout = &function.storage[state.0 as usize].layout;
                    entry.insert(super::actor::state_field_initialized(
                        module.ctx,
                        addressing,
                        slots[state.0 as usize],
                        layout,
                        field,
                    )?);
                    continue;
                }
                let flag = super::coro::entry_storage(
                    module.ctx,
                    entry_block,
                    module.ctx.bool_type().into(),
                    &format!("s{}.initialized", leaf.storage.0),
                )?;
                let initialized = module.ctx.bool_type().const_int(
                    u64::from(function.parameters.contains(&projection.root)),
                    false,
                );
                builder
                    .build_store(flag, initialized)
                    .llvm_ctx("initialize aggregate leaf state")?;
                entry.insert(flag);
            }
        }
    }
    for storage in &function.storage {
        let StorageOrigin::ActorState { state, field, .. } = storage.origin else {
            continue;
        };
        if !module.module.managed_types[&storage.ty].tracks_occupancy() {
            continue;
        }
        if let Entry::Vacant(entry) = flags.entry(storage.id) {
            entry.insert(super::actor::state_field_initialized(
                module.ctx,
                addressing,
                slots[state.0 as usize],
                &function.storage[state.0 as usize].layout,
                field,
            )?);
        }
    }
    if module.module.callables[function.callable.0 as usize].is_resumable {
        for storage in &function.storage {
            if storage.own != OwnKind::Owned
                || !function.frame_roots.slots.contains(&storage.id)
                || matches!(
                    storage.origin,
                    StorageOrigin::Capture { .. } | StorageOrigin::ActorState { .. }
                )
                || function.place_storage.contains_key(&storage.id)
            {
                continue;
            }
            if let Entry::Vacant(entry) = flags.entry(storage.id) {
                let flag = super::coro::entry_storage(
                    module.ctx,
                    entry_block,
                    module.ctx.bool_type().into(),
                    &format!("s{}.initialized", storage.id.0),
                )?;
                builder
                    .build_store(
                        flag,
                        module
                            .ctx
                            .bool_type()
                            .const_int(u64::from(function.parameters.contains(&storage.id)), false),
                    )
                    .llvm_ctx("initialize owned invocation slot")?;
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
        if matches!(self.storage(id)?.origin, StorageOrigin::ActorState { .. }) {
            if !self.module.managed_types[&self.storage(id)?.ty].tracks_occupancy() {
                return Ok(());
            }
            self.builder
                .build_store(
                    self.place_flag(id)?,
                    self.ctx
                        .bool_type()
                        .const_int(u64::from(initialized), false),
                )
                .llvm_ctx("publish actor state field initialization")?;
            return Ok(());
        }
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
            self.publish_session_change(projection.root)?;
        } else if let Some(flag) = self.place_flags.get(&id) {
            self.builder
                .build_store(
                    *flag,
                    self.ctx
                        .bool_type()
                        .const_int(u64::from(initialized), false),
                )
                .llvm_ctx("publish owned invocation slot initialization")?;
        }
        Ok(())
    }

    /// Release exactly the contents SIR certified for this cleanup site.
    /// The initialization bits are a run-time cache of the same fact, so an
    /// absent leaf costs nothing and a present leaf needs no test: only a site
    /// whose incoming paths disagree reaches the bits.
    pub(super) fn destroy_certified_contents(
        &self,
        source: StorageId,
        cleanup: &PhysicalCleanup,
    ) -> CodegenResult<bool> {
        self.destroy_partition(source, &|leaf| {
            cleanup.leaf(leaf).ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "aggregate leaf {} is outside its certified cleanup partition",
                    leaf.0
                ))
            })
        })
    }

    /// Release every leaf that holds contents at run time. This is for an
    /// emitter-local failure path inside one operation's expansion, which is
    /// not a SIR cleanup site and carries no certificate.
    pub(super) fn destroy_initialized_contents(&self, source: StorageId) -> CodegenResult<bool> {
        self.destroy_partition(source, &|_| Ok(LeafContents::Conditional))
    }

    fn destroy_partition(
        &self,
        source: StorageId,
        contents: &dyn Fn(StorageId) -> CodegenResult<LeafContents>,
    ) -> CodegenResult<bool> {
        let Some(projection) = self.function.place_storage.get(&source) else {
            return Ok(false);
        };
        self.debug_unavailable(source);
        for leaf in projection.leaves.iter().rev() {
            self.debug_unavailable(leaf.storage);
            let held = contents(leaf.storage)?;
            // An absent leaf holds nothing and its bit already reads false on
            // every path that arrives here.
            if held == LeafContents::Absent {
                continue;
            }
            let Some(action) = leaf.recipe.destroy else {
                self.set_place_initialized(leaf.storage, false)?;
                continue;
            };
            let next = if held == LeafContents::Present {
                None
            } else {
                let initialized = self.place_initialized(leaf.storage)?;
                let drop = self.ctx.append_basic_block(self.value, "aggregate.drop");
                let next = self.ctx.append_basic_block(self.value, "aggregate.next");
                self.builder
                    .build_conditional_branch(initialized, drop, next)
                    .llvm_ctx("test aggregate leaf initialization before destruction")?;
                self.builder.position_at_end(drop);
                Some(next)
            };
            self.set_place_initialized(leaf.storage, false)?;
            let value = self.load(leaf.storage, "aggregate.drop.value")?;
            self.debug_unavailable(leaf.storage);
            self.release_loaded(value, &self.storage(leaf.storage)?.layout, action)?;
            if let Some(next) = next {
                self.builder
                    .build_unconditional_branch(next)
                    .llvm_ctx("finish aggregate leaf destruction")?;
                self.builder.position_at_end(next);
            }
        }
        Ok(true)
    }
}
