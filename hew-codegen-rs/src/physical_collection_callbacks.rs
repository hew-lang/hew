//! Collection traversal that invokes selected user capabilities without parking
//! an actor worker. Borrowed operands stay in the caller's verified storage.

use super::*;
use hew_cabi::map::HewMapProbeStatus;

pub(super) struct CollectionProbe<'a, 'ctx> {
    pub key: &'a ResolvedTy,
    pub begin: &'static str,
    pub receiver: BasicValueEnum<'ctx>,
    pub input: PointerValue<'ctx>,
    pub inserting: bool,
    pub commit: &'static str,
    pub outputs: &'a [BasicMetadataValueEnum<'ctx>],
}

pub(super) struct CollectionCallbacks<'a, 'ctx> {
    pub values: &'a ValueEmitter<'a, 'ctx>,
    pub frame: Option<&'a coro::Frame<'ctx>>,
    pub callbacks: &'a key::CallbackTable<'ctx>,
    pub fault: PointerValue<'ctx>,
    pub status: PointerValue<'ctx>,
    pub failure: BasicBlock<'ctx>,
    pub allocations: Option<BasicBlock<'ctx>>,
}

impl<'ctx> CollectionCallbacks<'_, 'ctx> {
    fn scratch(&self, ty: BasicTypeEnum<'ctx>, name: &str) -> CodegenResult<PointerValue<'ctx>> {
        let Some(block) = self.allocations else {
            return self.values.entry_scratch(ty, name);
        };
        let builder = self.values.ctx.create_builder();
        if let Some(end) = block.get_terminator() {
            builder.position_before(&end);
        } else {
            builder.position_at_end(block);
        }
        builder
            .build_alloca(ty, name)
            .llvm_ctx("allocate collection callback scratch")
    }
    fn call_value(
        &self,
        function: FunctionValue<'ctx>,
        args: &[BasicMetadataValueEnum<'ctx>],
        name: &str,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        self.values
            .builder
            .build_call(function, args, name)
            .llvm_ctx("call collection runtime")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("collection runtime returned no value".into()))
    }
    fn call_void(
        &self,
        function: FunctionValue<'ctx>,
        args: &[BasicMetadataValueEnum<'ctx>],
        _name: &str,
    ) -> CodegenResult<()> {
        self.values
            .builder
            .build_call(function, args, "")
            .llvm_ctx("call collection runtime")?;
        Ok(())
    }
    /// Probe borrows the caller's owners until every callback has succeeded.
    /// Cancellation frees only the cursor before the ordinary move cleanup.
    pub(super) fn probe(
        &self,
        operation: CollectionProbe<'_, 'ctx>,
        release_commit: bool,
    ) -> CodegenResult<IntValue<'ctx>> {
        let pointer = self.values.ctx.ptr_type(AddressSpace::default());
        let i32_ty = self.values.ctx.i32_type();
        let begin = get_or_declare_external(
            self.values.llvm,
            operation.begin,
            pointer.fn_type(&[pointer.into(), pointer.into(), i32_ty.into()], false),
        )?;
        let probe = self.call_value(
            begin,
            &[
                operation.receiver.into(),
                operation.input.into(),
                i32_ty
                    .const_int(u64::from(operation.inserting), false)
                    .into(),
            ],
            "collection.probe",
        )?;
        let step = self
            .values
            .ctx
            .append_basic_block(self.values.value, "collection.probe.step");
        let hash = self
            .values
            .ctx
            .append_basic_block(self.values.value, "collection.probe.hash");
        let eq = self
            .values
            .ctx
            .append_basic_block(self.values.value, "collection.probe.eq");
        let ready = self
            .values
            .ctx
            .append_basic_block(self.values.value, "collection.probe.ready");
        let failed = self
            .values
            .ctx
            .append_basic_block(self.values.value, "collection.probe.failed");
        self.values
            .builder
            .build_unconditional_branch(step)
            .llvm_ctx("start collection probe")?;
        self.values.builder.position_at_end(step);
        let step_fn = get_or_declare_external(
            self.values.llvm,
            "hew_hashmap_probe_step",
            i32_ty.fn_type(&[pointer.into()], false),
        )?;
        let status = self
            .call_value(step_fn, &[probe.into()], "collection.probe.request")?
            .into_int_value();
        self.values
            .builder
            .build_switch(
                status,
                ready,
                &[
                    (
                        i32_ty.const_int(HewMapProbeStatus::NeedHash as u64, false),
                        hash,
                    ),
                    (
                        i32_ty.const_int(HewMapProbeStatus::NeedEq as u64, false),
                        eq,
                    ),
                ],
            )
            .llvm_ctx("select requested collection capability")?;
        for (block, capability, output_ty, submit) in [
            (
                hash,
                ValueCapability::Hash,
                self.values.ctx.i64_type(),
                "hew_hashmap_probe_submit_hash",
            ),
            (
                eq,
                ValueCapability::Eq,
                self.values.ctx.i8_type(),
                "hew_hashmap_probe_submit_eq",
            ),
        ] {
            self.values.builder.position_at_end(block);
            let left_fn =
                external_unary_ptr(self.values.ctx, self.values.llvm, "hew_hashmap_probe_left")?;
            let left = self.call_value(left_fn, &[probe.into()], "collection.probe.left")?;
            let mut arguments = vec![left.into()];
            if capability == ValueCapability::Eq {
                let right_fn = external_unary_ptr(
                    self.values.ctx,
                    self.values.llvm,
                    "hew_hashmap_probe_right",
                )?;
                arguments.push(
                    self.call_value(right_fn, &[probe.into()], "collection.probe.right")?
                        .into(),
                );
            }
            let output = self.scratch(output_ty.into(), "collection.probe.output")?;
            arguments.push(output.into());
            arguments.push(self.fault.into());
            self.values
                .builder
                .build_store(self.fault, pointer.const_null())
                .llvm_ctx("clear collection callback fault")?;
            let callback = *self
                .callbacks
                .get(&(operation.key.clone(), capability))
                .ok_or_else(|| {
                    CodegenError::FailClosed(
                        "collection probe lacks selected key capability".into(),
                    )
                })?;
            let status = self.values.invoke_value_callback(
                self.frame,
                operation.key,
                capability,
                callback,
                &arguments,
            )?;
            self.values
                .builder
                .build_store(self.status, status)
                .llvm_ctx("retain collection callback status")?;
            let succeeded = self
                .values
                .builder
                .build_int_compare(
                    IntPredicate::EQ,
                    status,
                    i32_ty.const_zero(),
                    "collection.probe.succeeded",
                )
                .llvm_ctx("test collection callback outcome")?;
            let complete = self
                .values
                .ctx
                .append_basic_block(self.values.value, "collection.probe.submit");
            self.values
                .builder
                .build_conditional_branch(succeeded, complete, failed)
                .llvm_ctx("select collection callback outcome")?;
            self.values.builder.position_at_end(complete);
            let value = self
                .values
                .builder
                .build_load(output_ty, output, "collection.probe.value")
                .llvm_ctx("read successful collection callback output")?;
            let submit_fn = get_or_declare_external(
                self.values.llvm,
                submit,
                self.values
                    .ctx
                    .void_type()
                    .fn_type(&[pointer.into(), output_ty.into()], false),
            )?;
            self.call_void(
                submit_fn,
                &[probe.into(), value.into()],
                "collection.probe.submit",
            )?;
            self.values
                .builder
                .build_unconditional_branch(step)
                .llvm_ctx("continue collection probe")?;
        }
        self.values.builder.position_at_end(failed);
        let free = external_drop(self.values.ctx, self.values.llvm, "hew_hashmap_probe_free")?;
        self.call_void(free, &[probe.into()], "collection.probe.free")?;
        self.values
            .builder
            .build_unconditional_branch(self.failure)
            .llvm_ctx("enter collection callback cleanup")?;
        self.values.builder.position_at_end(ready);
        let mut arguments = vec![probe.into()];
        arguments.extend_from_slice(operation.outputs);
        let parameters = vec![pointer.into(); arguments.len()];
        let commit = get_or_declare_external(
            self.values.llvm,
            operation.commit,
            self.values.ctx.i8_type().fn_type(&parameters, false),
        )?;
        let invoke = || {
            Ok(self
                .call_value(commit, &arguments, "collection.probe.commit")?
                .into_int_value())
        };
        // The result becomes owned in the normal successor before a displaced
        // value's close fault enters cleanup. End the sink before that edge.
        if release_commit {
            self.values.emit_release_in_sink(invoke)
        } else {
            invoke()
        }
    }
}

impl<'ctx> FunctionEmitter<'_, 'ctx> {
    pub(super) fn emit_collection_probe(
        &self,
        operation: CollectionProbe<'_, 'ctx>,
        failure: Option<&PhysicalEdge>,
        consumed: &[(StorageId, DestroyAction)],
    ) -> CodegenResult<IntValue<'ctx>> {
        let failure = failure.ok_or_else(|| {
            CodegenError::FailClosed("collection probe lacks callback cleanup".into())
        })?;
        let failed = self
            .ctx
            .append_basic_block(self.value, "collection.callback.cleanup");
        let values = self.value_emitter();
        let emitter = CollectionCallbacks {
            values: &values,
            frame: self.frame.as_ref(),
            callbacks: self.value_callbacks,
            fault: self.active_fault,
            status: self.active_status,
            failure: failed,
            allocations: None,
        };
        let result = emitter.probe(
            operation,
            consumed
                .iter()
                .any(|(_, action)| self.module.releases.raises_fault(*action)),
        )?;
        let ready = self
            .builder
            .get_insert_block()
            .expect("probe commit has a block");
        self.builder.position_at_end(failed);
        for &(source, destroy) in consumed {
            self.destroy_owned_operand(source, destroy)?;
        }
        self.emit_edge(failure)?;
        self.builder.position_at_end(ready);
        Ok(result)
    }

    pub(super) fn emit_vector_contains(
        &self,
        element: &ResolvedTy,
        vector: PointerValue<'ctx>,
        needle: PointerValue<'ctx>,
        result: StorageId,
        normal: &PhysicalEdge,
        failure: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let i64_ty = self.ctx.i64_type();
        let callback = *self
            .value_callbacks
            .get(&(element.clone(), ValueCapability::Eq))
            .ok_or_else(|| {
                CodegenError::FailClosed("vector membership lacks selected equality".into())
            })?;
        let len_fn = get_or_declare_external(
            self.llvm,
            "hew_vec_len",
            i64_ty.fn_type(&[pointer.into()], false),
        )?;
        let len = self
            .runtime_call_value(len_fn, &[vector.into()], "contains.len")?
            .into_int_value();
        let index_slot = self
            .value_emitter()
            .entry_scratch(i64_ty.into(), "contains.index")?;
        self.builder
            .build_store(index_slot, i64_ty.const_zero())
            .llvm_ctx("initialize membership cursor")?;
        let check = self.ctx.append_basic_block(self.value, "contains.check");
        let item = self.ctx.append_basic_block(self.value, "contains.item");
        let compare = self.ctx.append_basic_block(self.value, "contains.compare");
        let failed = self.ctx.append_basic_block(self.value, "contains.failed");
        let next = self.ctx.append_basic_block(self.value, "contains.next");
        let found = self.ctx.append_basic_block(self.value, "contains.found");
        let absent = self.ctx.append_basic_block(self.value, "contains.absent");
        self.builder
            .build_unconditional_branch(check)
            .llvm_ctx("start membership traversal")?;
        self.builder.position_at_end(check);
        let index = self
            .builder
            .build_load(i64_ty, index_slot, "contains.index")
            .llvm_ctx("load membership cursor")?
            .into_int_value();
        let more = self
            .builder
            .build_int_compare(IntPredicate::ULT, index, len, "contains.more")
            .llvm_ctx("test membership cursor")?;
        self.builder
            .build_conditional_branch(more, item, absent)
            .llvm_ctx("select membership element")?;
        self.builder.position_at_end(item);
        let get = get_or_declare_external(
            self.llvm,
            "hew_vec_get_owned",
            pointer.fn_type(&[pointer.into(), i64_ty.into()], false),
        )?;
        let slot =
            self.runtime_call_value(get, &[vector.into(), index.into()], "contains.element")?;
        self.builder
            .build_store(self.active_fault, pointer.const_null())
            .llvm_ctx("clear equality fault")?;
        let status = self.value_emitter().invoke_value_callback(
            self.frame.as_ref(),
            element,
            ValueCapability::Eq,
            callback,
            &[
                slot.into(),
                needle.into(),
                self.slots[result.0 as usize].into(),
                self.active_fault.into(),
            ],
        )?;
        self.builder
            .build_store(self.active_status, status)
            .llvm_ctx("retain equality status")?;
        let success = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                self.ctx.i32_type().const_zero(),
                "contains.success",
            )
            .llvm_ctx("test equality outcome")?;
        self.builder
            .build_conditional_branch(success, compare, failed)
            .llvm_ctx("select equality outcome")?;
        self.builder.position_at_end(failed);
        self.emit_edge(failure)?;
        self.builder.position_at_end(compare);
        let equal = self.load(result, "contains.equal")?.into_int_value();
        let equal = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                equal,
                equal.get_type().const_zero(),
                "contains.equal.test",
            )
            .llvm_ctx("test selected equality")?;
        self.builder
            .build_conditional_branch(equal, found, next)
            .llvm_ctx("select membership result")?;
        self.builder.position_at_end(next);
        let next_index = self
            .builder
            .build_int_add(index, i64_ty.const_int(1, false), "contains.next.index")
            .llvm_ctx("advance membership cursor")?;
        self.builder
            .build_store(index_slot, next_index)
            .llvm_ctx("retain membership cursor")?;
        self.builder
            .build_unconditional_branch(check)
            .llvm_ctx("continue membership traversal")?;
        self.builder.position_at_end(found);
        self.store(result, self.ctx.i8_type().const_int(1, false).into())?;
        self.emit_result_edge(Some(result), normal)?;
        self.builder.position_at_end(absent);
        self.store(result, self.ctx.i8_type().const_zero().into())?;
        self.emit_result_edge(Some(result), normal)
    }
}
