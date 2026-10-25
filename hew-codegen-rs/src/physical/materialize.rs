//! Execute the verified acquisition plan before publishing a managed value.

use super::*;
use hew_mir::physical::{PhysicalManagedSource, PhysicalMaterialization};

impl<'ctx> FunctionEmitter<'_, 'ctx> {
    pub(super) fn publish_managed_result(
        &self,
        result: StorageId,
        failure: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let layout = self.managed_value_layout(&self.storage(result)?.ty)?;
        let cleanup = self
            .value_emitter()
            .entry_scratch(pointer.into(), "publication.cleanup")?;
        self.set_capture_initialized(result, false)?;
        self.set_place_initialized(result, false)?;
        let publish = get_or_declare_external(
            self.llvm,
            "hew_budget_publish_value",
            self.ctx.i32_type().fn_type(&[pointer.into(); 3], false),
        )?;
        let status = self
            .runtime_call_value(
                publish,
                &[
                    layout.into(),
                    self.slots[result.0 as usize].into(),
                    cleanup.into(),
                ],
                "publication.status",
            )?
            .into_int_value();
        let accepted = self
            .ctx
            .append_basic_block(self.value, "publication.accepted");
        let refused = self
            .ctx
            .append_basic_block(self.value, "publication.refused");
        let ok = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                self.ctx.i32_type().const_zero(),
                "publication.ok",
            )
            .llvm_ctx("inspect private result acquisition")?;
        self.builder
            .build_conditional_branch(ok, accepted, refused)
            .llvm_ctx("publish only an acquired managed result")?;
        self.builder.position_at_end(refused);
        self.clear_owned(result)?;
        self.initialize_active_fault(hew_runtime::internal::types::HEW_TRAP_HEAP_EXCEEDED)?;
        let cursor = self
            .builder
            .build_load(pointer, cleanup, "publication.cleanup")
            .llvm_ctx("take unpublished result cleanup")?
            .into_pointer_value();
        release::drain(&self.value_emitter(), self.frame.as_ref(), cursor)?;
        self.emit_edge(failure)?;
        self.builder.position_at_end(accepted);
        Ok(())
    }

    fn managed_input_slot(
        &self,
        input: &hew_mir::physical::PhysicalManagedInput,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let value = match input.source {
            PhysicalManagedSource::Value(source) => return Ok(self.slots[source.0 as usize]),
            PhysicalManagedSource::AggregateField { source, field } => {
                let aggregate = self.load(source, "budget.aggregate")?.into_struct_value();
                self.builder
                    .build_extract_value(aggregate, field, "budget.field")
                    .llvm_ctx("read the selected managed field")?
            }
            PhysicalManagedSource::VariantField {
                source,
                variant,
                field,
                glue,
            } => {
                let payload = self.variant_field_payload(source, variant, glue)?;
                self.builder
                    .build_extract_value(payload, field, "budget.variant.field")
                    .llvm_ctx("read the selected managed variant field")?
            }
        };
        let slot = self
            .value_emitter()
            .entry_scratch(value.get_type(), "budget.input")?;
        self.builder
            .build_store(slot, value)
            .llvm_ctx("snapshot the managed field carrier")?;
        Ok(slot)
    }

    pub(super) fn emit_materialization(
        &self,
        operations: &[PhysicalOp],
        plan: &PhysicalMaterialization,
        normal: &PhysicalEdge,
        failure: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let i64_ty = self.ctx.i64_type();
        let size_ty = self
            .ctx
            .ptr_sized_int_type(&TargetData::create(&self.module.target.data_layout), None);
        let state = self
            .frame
            .as_ref()
            .map_or(pointer.const_null(), |frame| frame.state);
        let permit = self
            .value_emitter()
            .entry_scratch(pointer.into(), "budget.permit")?;
        self.builder
            .build_store(permit, pointer.const_null())
            .llvm_ctx("clear managed permit")?;
        let input_ty = self.ctx.struct_type(
            &[
                pointer.into(),
                pointer.into(),
                i64_ty.into(),
                self.ctx.i8_type().into(),
            ],
            false,
        );
        let count = u32::try_from(plan.inputs.len())
            .map_err(|_| CodegenError::FailClosed("too many managed inputs".into()))?;
        let array_ty = input_ty.array_type(count);
        let table = self
            .value_emitter()
            .entry_scratch(array_ty.into(), "budget.inputs")?;
        for (index, input) in plan.inputs.iter().enumerate() {
            let index = u32::try_from(index).expect("checked input count");
            // SAFETY: this index selects a declared element in the input table.
            let row = unsafe {
                self.builder.build_in_bounds_gep(
                    array_ty,
                    table,
                    &[
                        self.ctx.i32_type().const_zero(),
                        self.ctx.i32_type().const_int(u64::from(index), false),
                    ],
                    "budget.input.row",
                )
            }
            .llvm_ctx("address the checked input table")?;
            let layout = self.managed_value_layout(&input.ty)?;
            let slot = self.managed_input_slot(input)?;
            let values: [BasicValueEnum<'ctx>; 4] = [
                layout.into(),
                slot.into(),
                i64_ty.const_int(input.copies, false).into(),
                self.ctx
                    .i8_type()
                    .const_int(u64::from(input.consume), false)
                    .into(),
            ];
            for (field, value) in values.into_iter().enumerate() {
                let field = self
                    .builder
                    .build_struct_gep(
                        input_ty,
                        row,
                        u32::try_from(field).expect("four input fields"),
                        "budget.input.field",
                    )
                    .llvm_ctx("address managed input metadata")?;
                self.builder
                    .build_store(field, value)
                    .llvm_ctx("write managed input metadata")?;
            }
        }
        let status = if let Some(bytes) = plan.fresh_bytes {
            let fresh = if let Some(length) = plan.bytes_literal_length {
                let capacity = get_or_declare_external(
                    self.llvm,
                    "hew_bytes_literal_capacity",
                    i64_ty.fn_type(&[self.ctx.i32_type().into()], false),
                )?;
                self.runtime_call_value(
                    capacity,
                    &[self
                        .ctx
                        .i32_type()
                        .const_int(u64::from(length), false)
                        .into()],
                    "budget.bytes.capacity",
                )?
                .into_int_value()
            } else {
                i64_ty.const_int(bytes, false)
            };
            let begin = get_or_declare_external(
                self.llvm,
                "hew_budget_materialize_begin",
                self.ctx.i32_type().fn_type(
                    &[
                        pointer.into(),
                        i64_ty.into(),
                        pointer.into(),
                        size_ty.into(),
                        pointer.into(),
                    ],
                    false,
                ),
            )?;
            self.runtime_call_value(
                begin,
                &[
                    state.into(),
                    fresh.into(),
                    table.into(),
                    size_ty.const_int(u64::from(count), false).into(),
                    permit.into(),
                ],
                "budget.acquire",
            )?
            .into_int_value()
        } else {
            let overflow = get_or_declare_external(
                self.llvm,
                "hew_budget_materialize_overflow",
                self.ctx.i32_type().fn_type(&[pointer.into()], false),
            )?;
            self.runtime_call_value(overflow, &[state.into()], "budget.overflow")?
                .into_int_value()
        };
        let construct = self.ctx.append_basic_block(self.value, "budget.construct");
        let refused = self.ctx.append_basic_block(self.value, "budget.refused");
        let success = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                self.ctx.i32_type().const_zero(),
                "budget.accepted",
            )
            .llvm_ctx("classify managed acquisition")?;
        self.builder
            .build_conditional_branch(success, construct, refused)
            .llvm_ctx("route checked acquisition")?;
        self.builder.position_at_end(refused);
        self.emit_edge(failure)?;
        self.builder.position_at_end(construct);
        for operation in operations {
            self.emit_op(operation)?;
        }
        let permit = self
            .builder
            .build_load(pointer, permit, "budget.prepared")
            .llvm_ctx("take managed permit")?
            .into_pointer_value();
        let commit = get_or_declare_external(
            self.llvm,
            "hew_budget_materialize_commit",
            self.ctx.i32_type().fn_type(&[pointer.into(); 3], false),
        )?;
        let layout = self.managed_value_layout(&self.storage(plan.result)?.ty)?;
        self.runtime_call_value(
            commit,
            &[
                permit.into(),
                layout.into(),
                self.slots[plan.result.0 as usize].into(),
            ],
            "budget.commit",
        )?;
        if plan.start_task {
            let start = get_or_declare_external(
                self.llvm,
                "hew_checked_task_start",
                self.ctx.void_type().fn_type(&[pointer.into()], false),
            )?;
            let task = self.load(plan.result, "budget.task")?;
            self.runtime_call_void(start, &[task.into()], "")?;
        }
        self.emit_edge(normal)
    }
}
