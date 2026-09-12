//! Trait-object tables, erasure and vtable dispatch.
//!
//! The runtime owns the layout: a `dyn Trait` value is the two-word
//! `HewTraitObject { data, vtable }`, and every table is the three-word prefix
//! (`drop_in_place`, `size_of`, `align_of`) followed by one function pointer
//! per method slot. See `hew-runtime/src/trait_object.rs`.
//!
//! Each slot holds a thunk rather than the implementation itself, so the
//! dispatch site's function type depends only on the trait method, never on
//! the concrete receiver's carrier.

use super::*;
use hew_mir::physical::{PhysicalVtable, PhysicalVtableId, PhysicalVtableSlot};

pub(super) fn vtable_symbol(id: PhysicalVtableId) -> String {
    format!("__hew_vtable_{}", id.0)
}

fn vtable_drop_symbol(id: PhysicalVtableId) -> String {
    format!("__hew_vtable_{}_drop", id.0)
}

fn vtable_slot_symbol(id: PhysicalVtableId, slot: u32) -> String {
    format!("__hew_vtable_{}_slot_{slot}", id.0)
}

/// The dispatch function type for one slot: the erased receiver rides a
/// pointer, the rest of the ABI is the private call convention.
fn dispatch_type<'ctx>(
    ctx: &'ctx Context,
    signature: &hew_mir::physical::PhysicalCallSignature,
) -> CodegenResult<inkwell::types::FunctionType<'ctx>> {
    let pointer = ctx.ptr_type(AddressSpace::default());
    let mut params: Vec<BasicMetadataTypeEnum<'ctx>> = vec![pointer.into()];
    for param in &signature.params {
        params.push(match param.carrier {
            ParamCarrier::Direct => llvm_type(ctx, &param.layout.repr)?.into(),
            ParamCarrier::Indirect => pointer.into(),
        });
    }
    if signature.return_layout.is_some() {
        params.push(pointer.into());
    }
    params.push(pointer.into());
    Ok(ctx.i32_type().fn_type(&params, false))
}

impl<'ctx> ModuleEmitter<'ctx, '_> {
    pub(super) fn emit_vtables(&self) -> CodegenResult<()> {
        for table in &self.module.vtables {
            self.emit_vtable(table)?;
        }
        Ok(())
    }

    fn emit_vtable(&self, table: &PhysicalVtable) -> CodegenResult<()> {
        let target = TargetData::create(&self.module.target.data_layout);
        let word = self.ctx.ptr_sized_int_type(&target, None);
        let drop = self.emit_vtable_drop(table)?;
        let mut fields: Vec<BasicValueEnum<'ctx>> = vec![
            drop.into(),
            word.const_int(table.concrete_layout.size, false).into(),
            word.const_int(u64::from(table.concrete_layout.align), false)
                .into(),
        ];
        for slot in &table.slots {
            fields.push(self.emit_vtable_slot(table, slot)?.into());
        }
        let field_types: Vec<BasicTypeEnum<'ctx>> = fields
            .iter()
            .map(inkwell::values::BasicValueEnum::get_type)
            .collect();
        let struct_ty = self.ctx.struct_type(&field_types, false);
        let global = self
            .llvm
            .add_global(struct_ty, None, &vtable_symbol(table.id));
        global.set_linkage(Linkage::Internal);
        global.set_constant(true);
        global.set_initializer(&struct_ty.const_named_struct(&fields));
        Ok(())
    }

    /// Slot 0: run the boxed value's destructor without freeing its storage.
    fn emit_vtable_drop(&self, table: &PhysicalVtable) -> CodegenResult<PointerValue<'ctx>> {
        let name = vtable_drop_symbol(table.id);
        let Some(action) = table.concrete.destroy else {
            // A concrete type with no obligation still needs a callable slot:
            // an empty body keeps every table's shape identical.
            let function = self.llvm.add_function(
                &name,
                self.ctx
                    .void_type()
                    .fn_type(&[self.ctx.ptr_type(AddressSpace::default()).into()], false),
                Some(Linkage::Internal),
            );
            let builder = self.ctx.create_builder();
            let entry = self.ctx.append_basic_block(function, "entry");
            builder.position_at_end(entry);
            builder
                .build_return(None)
                .llvm_ctx("finish empty trait-object drop")?;
            return Ok(function.as_global_value().as_pointer_value());
        };
        self.emit_value_drop_callback(&name, &table.concrete_layout, action)
    }

    /// One method slot: adapt the erased data pointer to the implementation's
    /// own receiver carrier, then forward every remaining argument unchanged.
    fn emit_vtable_slot(
        &self,
        table: &PhysicalVtable,
        slot: &PhysicalVtableSlot,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let callee = callable(self.module, slot.callee)?;
        if callee.is_resumable {
            return Err(CodegenError::FailClosed(format!(
                "slot {} of `{}` names a resumable body; a suspending trait method has no vtable ABI",
                slot.slot,
                table.concrete_ty.user_facing()
            )));
        }
        let target = *self.functions.get(&slot.callee).ok_or_else(|| {
            CodegenError::FailClosed("trait-object slot has no emitted implementation".into())
        })?;
        let receiver = callee.params.first().ok_or_else(|| {
            CodegenError::FailClosed("trait-object slot implementation takes no receiver".into())
        })?;
        let function = self.llvm.add_function(
            &vtable_slot_symbol(table.id, slot.slot),
            dispatch_type(self.ctx, &slot.signature)?,
            Some(Linkage::Internal),
        );
        let builder = self.ctx.create_builder();
        let entry = self.ctx.append_basic_block(function, "entry");
        builder.position_at_end(entry);
        let data = function
            .get_nth_param(0)
            .ok_or_else(|| CodegenError::FailClosed("trait-object slot lacks its receiver".into()))?
            .into_pointer_value();
        let mut arguments: Vec<BasicMetadataValueEnum<'ctx>> = vec![match receiver.carrier {
            ParamCarrier::Indirect => data.into(),
            ParamCarrier::Direct => builder
                .build_load(
                    llvm_type(self.ctx, &receiver.layout.repr)?,
                    data,
                    "dyn.receiver",
                )
                .llvm_ctx("load erased receiver")?
                .into(),
        }];
        for index in 1..function.count_params() {
            arguments.push(
                function
                    .get_nth_param(index)
                    .ok_or_else(|| {
                        CodegenError::FailClosed("trait-object slot lost a parameter".into())
                    })?
                    .into(),
            );
        }
        let status = builder
            .build_call(target, &arguments, "dyn.slot.status")
            .llvm_ctx("forward vtable slot to its implementation")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("vtable slot returned no status".into()))?;
        builder
            .build_return(Some(&status))
            .llvm_ctx("return vtable slot status")?;
        Ok(function.as_global_value().as_pointer_value())
    }
}

impl<'ctx> FunctionEmitter<'_, 'ctx> {
    /// Box the concrete value and pair it with its table's constant.
    pub(super) fn emit_dyn_make(
        &self,
        dest: StorageId,
        vtable: PhysicalVtableId,
        source: StorageId,
    ) -> CodegenResult<()> {
        let table = self
            .module
            .vtables
            .get(vtable.0 as usize)
            .filter(|table| table.id == vtable)
            .ok_or_else(|| {
                CodegenError::FailClosed("erasure names no realized dispatch table".into())
            })?;
        let target = TargetData::create(&self.module.target.data_layout);
        let word = self.ctx.ptr_sized_int_type(&target, None);
        let alloc = get_or_declare_external(
            self.llvm,
            hew_types::RuntimeCallFamily::DynBoxAlloc.c_symbol(),
            self.ctx
                .ptr_type(AddressSpace::default())
                .fn_type(&[word.into(), word.into()], false),
        )?;
        let data = self
            .builder
            .build_call(
                alloc,
                &[
                    word.const_int(table.concrete_layout.size, false).into(),
                    word.const_int(u64::from(table.concrete_layout.align), false)
                        .into(),
                ],
                "dyn.box",
            )
            .llvm_ctx("allocate erased value storage")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| {
                CodegenError::FailClosed("dyn box allocation returned no pointer".into())
            })?
            .into_pointer_value();
        let value = self.load(source, "dyn.concrete")?;
        self.builder
            .build_store(data, value)
            .llvm_ctx("move the concrete value into its box")?;
        self.clear_owned(source)?;
        let vtable_global = self
            .llvm
            .get_global(&vtable_symbol(vtable))
            .ok_or_else(|| CodegenError::FailClosed("dispatch table was never emitted".into()))?
            .as_pointer_value();
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let fat = self
            .ctx
            .struct_type(&[pointer.into(), pointer.into()], false);
        let object = self
            .builder
            .build_insert_value(fat.get_undef(), data, 0, "dyn.data")
            .llvm_ctx("place the erased data pointer")?;
        let object = self
            .builder
            .build_insert_value(object, vtable_global, 1, "dyn.vtable")
            .llvm_ctx("place the dispatch table pointer")?;
        self.builder
            .build_store(self.slots[dest.0 as usize], object)
            .llvm_ctx("store the trait object")?;
        Ok(())
    }

    /// Load one slot from the receiver's table and call through it.
    #[expect(
        clippy::too_many_arguments,
        reason = "one dispatch boundary: receiver, slot, ABI and both continuations"
    )]
    pub(super) fn emit_dyn_call(
        &self,
        receiver: ArgumentTransfer,
        slot: u32,
        signature: &hew_mir::physical::PhysicalCallSignature,
        transfers: &[ArgumentTransfer],
        result: Option<StorageId>,
        normal: Option<&PhysicalEdge>,
        unwind: Option<&PhysicalEdge>,
    ) -> CodegenResult<()> {
        self.builder
            .build_store(
                self.active_fault,
                self.ctx.ptr_type(AddressSpace::default()).const_null(),
            )
            .llvm_ctx("clear active fault before dispatch")?;
        let (source, consumed) = match receiver {
            ArgumentTransfer::Borrow(source) | ArgumentTransfer::BorrowMut(source) => {
                (source, false)
            }
            ArgumentTransfer::Move(source) => (source, true),
            ArgumentTransfer::Clone { .. } => {
                return Err(CodegenError::FailClosed(
                    "a trait object has no clone; dispatch cannot copy its receiver".into(),
                ))
            }
        };
        let object = self.load(source, "dyn.receiver")?.into_struct_value();
        let data = self
            .builder
            .build_extract_value(object, 0, "dyn.data")
            .llvm_ctx("read the erased data pointer")?
            .into_pointer_value();
        let table = self
            .builder
            .build_extract_value(object, 1, "dyn.vtable")
            .llvm_ctx("read the dispatch table pointer")?
            .into_pointer_value();
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let slots = pointer.array_type(slot + 1);
        let entry = unsafe {
            self.builder
                .build_in_bounds_gep(
                    slots,
                    table,
                    &[
                        self.ctx.i32_type().const_zero(),
                        self.ctx.i32_type().const_int(u64::from(slot), false),
                    ],
                    "dyn.slot",
                )
                .llvm_ctx("address the dispatch slot")?
        };
        let callee = self
            .builder
            .build_load(pointer, entry, "dyn.callee")
            .llvm_ctx("load the dispatch slot")?
            .into_pointer_value();
        let mut arguments: Vec<BasicMetadataValueEnum<'ctx>> = vec![data.into()];
        let mut moved = Vec::new();
        for (transfer, parameter) in transfers.iter().zip(&signature.params) {
            let (argument, value) = match transfer {
                ArgumentTransfer::Borrow(argument) | ArgumentTransfer::BorrowMut(argument) => {
                    (*argument, None)
                }
                ArgumentTransfer::Move(argument) => {
                    moved.push(*argument);
                    (*argument, None)
                }
                ArgumentTransfer::Clone { source, action } => {
                    (*source, Some(self.clone_value(*source, *action)?))
                }
            };
            match parameter.carrier {
                ParamCarrier::Direct => arguments.push(
                    value
                        .map_or_else(|| self.load(argument, "dyn.argument"), Ok)?
                        .into(),
                ),
                ParamCarrier::Indirect => {
                    if let Some(value) = value {
                        let temp = self.value_emitter().entry_scratch(
                            llvm_type(self.ctx, &parameter.layout.repr)?,
                            "dyn.clone.argument",
                        )?;
                        self.builder
                            .build_store(temp, value)
                            .llvm_ctx("store cloned indirect argument")?;
                        arguments.push(temp.into());
                    } else {
                        arguments.push(self.slots[argument.0 as usize].into());
                    }
                }
            }
        }
        if let Some(result) = result {
            arguments.push(self.slots[result.0 as usize].into());
        }
        arguments.push(self.active_fault.into());
        let status = self
            .builder
            .build_indirect_call(
                dispatch_type(self.ctx, signature)?,
                callee,
                &arguments,
                "dyn.status",
            )
            .llvm_ctx("dispatch through the vtable slot")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("dynamic dispatch returned no status".into()))?
            .into_int_value();
        self.builder
            .build_store(self.active_status, status)
            .llvm_ctx("store active dispatch status")?;
        for argument in moved {
            self.clear_owned(argument)?;
        }
        if consumed {
            // The concrete value moved into the callee, so only the box is
            // left; releasing it here keeps both continuations clean.
            self.value_emitter().free_dyn_box(data, table)?;
            self.clear_owned(source)?;
        }
        self.emit_call_outcome(status, result, normal, unwind)
    }
}

impl<'ctx> ValueEmitter<'_, 'ctx> {
    /// Release an erased value: run its own destructor through slot 0, then
    /// free the box with the size and alignment the same table carries.
    pub(super) fn destroy_trait_object(&self, value: BasicValueEnum<'ctx>) -> CodegenResult<()> {
        let object = value.into_struct_value();
        let data = self
            .builder
            .build_extract_value(object, 0, "dyn.drop.data")
            .llvm_ctx("read the erased data pointer")?
            .into_pointer_value();
        let table = self
            .builder
            .build_extract_value(object, 1, "dyn.drop.vtable")
            .llvm_ctx("read the dispatch table pointer")?
            .into_pointer_value();
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let drop = self
            .builder
            .build_load(pointer, table, "dyn.drop.slot")
            .llvm_ctx("load the drop slot")?
            .into_pointer_value();
        self.builder
            .build_indirect_call(
                self.ctx.void_type().fn_type(&[pointer.into()], false),
                drop,
                &[data.into()],
                "",
            )
            .llvm_ctx("run the erased value's destructor")?;
        self.free_dyn_box(data, table)
    }

    /// Free one erased box using its table's `size_of`/`align_of` prefix.
    fn free_dyn_box(
        &self,
        data: PointerValue<'ctx>,
        table: PointerValue<'ctx>,
    ) -> CodegenResult<()> {
        let target = TargetData::create(&self.module.target.data_layout);
        let word = self.ctx.ptr_sized_int_type(&target, None);
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let prefix = self
            .ctx
            .struct_type(&[pointer.into(), word.into(), word.into()], false);
        let mut layout = [word.const_zero(); 2];
        for (index, name) in [(1u32, "dyn.size"), (2, "dyn.align")] {
            let field = self
                .builder
                .build_struct_gep(prefix, table, index, name)
                .llvm_ctx("address the table layout prefix")?;
            layout[index as usize - 1] = self
                .builder
                .build_load(word, field, name)
                .llvm_ctx("load the table layout prefix")?
                .into_int_value();
        }
        let free = get_or_declare_external(
            self.llvm,
            hew_types::RuntimeCallFamily::DynBoxFree.c_symbol(),
            self.ctx
                .void_type()
                .fn_type(&[pointer.into(), word.into(), word.into()], false),
        )?;
        self.builder
            .build_call(free, &[data.into(), layout[0].into(), layout[1].into()], "")
            .llvm_ctx("free the erased value's box")?;
        Ok(())
    }
}
