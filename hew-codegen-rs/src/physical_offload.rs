//! `#[offload]` extern calls: the job's environment and its two thunks.
//!
//! The environment is one struct holding every argument, then the result.
//! `run` calls the extern with those arguments and stores its result; it runs
//! on a pool thread. `release` destroys the arguments and, when asked, the
//! result; it runs on whichever thread drops the environment last.

use super::encoding::ForeignResult;
use super::*;
use hew_mir::physical::{PhysicalOffload, PhysicalOffloadSlot};
use hew_types::ResolvedTy;

/// One offloaded extern's environment layout and thunks.
pub(super) struct OffloadThunks<'ctx> {
    env: inkwell::types::StructType<'ctx>,
    run: FunctionValue<'ctx>,
    release: FunctionValue<'ctx>,
}

fn env_type<'ctx>(
    ctx: &'ctx Context,
    offload: &PhysicalOffload,
) -> CodegenResult<inkwell::types::StructType<'ctx>> {
    let fields = offload
        .params
        .iter()
        .chain(&offload.result)
        .map(|slot| llvm_type(ctx, &slot.layout.repr))
        .collect::<CodegenResult<Vec<_>>>()?;
    Ok(ctx.struct_type(&fields, false))
}

impl<'ctx> FunctionEmitter<'_, 'ctx> {
    fn offload_thunks(
        &self,
        id: hew_mir::physical::OffloadId,
    ) -> CodegenResult<OffloadThunks<'ctx>> {
        let offload =
            self.module.offloads.get(id.0 as usize).ok_or_else(|| {
                CodegenError::FailClosed("offload names no published extern".into())
            })?;
        let env = env_type(self.ctx, offload)?;
        let run_name = format!("__hew_offload_run_{}", id.0);
        let release_name = format!("__hew_offload_release_{}", id.0);
        if let (Some(run), Some(release)) = (
            self.llvm.get_function(&run_name),
            self.llvm.get_function(&release_name),
        ) {
            return Ok(OffloadThunks { env, run, release });
        }
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let run = self.llvm.add_function(
            &run_name,
            self.ctx.void_type().fn_type(&[pointer.into()], false),
            Some(Linkage::Internal),
        );
        let release = self.llvm.add_function(
            &release_name,
            self.ctx
                .void_type()
                .fn_type(&[pointer.into(), self.ctx.i32_type().into()], false),
            Some(Linkage::Internal),
        );
        self.emit_offload_run(offload, env, run)?;
        self.emit_offload_release(offload, env, release)?;
        Ok(OffloadThunks { env, run, release })
    }

    fn thunk_emitter<'b>(
        &'b self,
        builder: &'b Builder<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> ValueEmitter<'b, 'ctx> {
        ValueEmitter {
            module: self.module,
            ctx: self.ctx,
            llvm: self.llvm,
            builder,
            value: function,
            fault_sink: None,
        }
    }

    /// `run(env)`: call the extern exactly as a direct call would.
    fn emit_offload_run(
        &self,
        offload: &PhysicalOffload,
        env_ty: inkwell::types::StructType<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> CodegenResult<()> {
        let builder = self.ctx.create_builder();
        let entry = self.ctx.append_basic_block(function, "entry");
        let body = self.ctx.append_basic_block(function, "body");
        builder.position_at_end(entry);
        builder
            .build_unconditional_branch(body)
            .llvm_ctx("enter offload run")?;
        builder.position_at_end(body);
        let env = function
            .get_nth_param(0)
            .ok_or_else(|| CodegenError::FailClosed("offload run lacks its environment".into()))?
            .into_pointer_value();
        let mut values = Vec::with_capacity(offload.params.len());
        let widen = (0_u32..)
            .zip(&offload.params)
            .filter_map(|(index, slot)| Widen::of(&slot.recipe.ty).map(|kind| (index, kind)))
            .collect::<Vec<_>>();
        for (index, slot) in offload.params.iter().enumerate() {
            let field = builder
                .build_struct_gep(env_ty, env, index as u32, "offload.argument")
                .llvm_ctx("address offload argument")?;
            // `bytes` crosses every C boundary by pointer to its triple.
            if slot.recipe.ty == ResolvedTy::Bytes {
                values.push(field.into());
            } else {
                values.push(
                    builder
                        .build_load(
                            llvm_type(self.ctx, &slot.layout.repr)?,
                            field,
                            "offload.value",
                        )
                        .llvm_ctx("load offload argument")?,
                );
            }
        }
        let result = offload
            .result
            .as_ref()
            .map(|slot| {
                Ok::<_, CodegenError>(ForeignResult {
                    destination: builder
                        .build_struct_gep(
                            env_ty,
                            env,
                            offload.params.len() as u32,
                            "offload.result",
                        )
                        .llvm_ctx("address offload result")?,
                    storage: llvm_type(self.ctx, &slot.layout.repr)?,
                    align: slot.layout.align,
                })
            })
            .transpose()?;
        self.thunk_emitter(&builder, function).call_foreign(
            &offload.symbol,
            &values,
            &widen,
            result,
            &offload.result_abi,
        )?;
        builder.build_return(None).llvm_ctx("finish offload run")?;
        Ok(())
    }

    /// `release(env, result_live)`: destroy the arguments, then the result
    /// when the job produced one nobody took.
    fn emit_offload_release(
        &self,
        offload: &PhysicalOffload,
        env_ty: inkwell::types::StructType<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> CodegenResult<()> {
        let builder = self.ctx.create_builder();
        let entry = self.ctx.append_basic_block(function, "entry");
        let body = self.ctx.append_basic_block(function, "body");
        builder.position_at_end(entry);
        builder
            .build_unconditional_branch(body)
            .llvm_ctx("enter offload release")?;
        builder.position_at_end(body);
        let env = function
            .get_nth_param(0)
            .ok_or_else(|| {
                CodegenError::FailClosed("offload release lacks its environment".into())
            })?
            .into_pointer_value();
        let result_live = function
            .get_nth_param(1)
            .ok_or_else(|| {
                CodegenError::FailClosed("offload release lacks its result flag".into())
            })?
            .into_int_value();
        let values = self.thunk_emitter(&builder, function);
        let destroy = |index: usize, slot: &PhysicalOffloadSlot| -> CodegenResult<()> {
            let Some(action) = slot.recipe.destroy else {
                return Ok(());
            };
            let field = builder
                .build_struct_gep(env_ty, env, index as u32, "offload.owned")
                .llvm_ctx("address offload owner")?;
            let value = builder
                .build_load(
                    llvm_type(self.ctx, &slot.layout.repr)?,
                    field,
                    "offload.owner",
                )
                .llvm_ctx("load offload owner")?;
            values.destroy_loaded_value(value, &slot.layout, action)
        };
        for (index, slot) in offload.params.iter().enumerate() {
            destroy(index, slot)?;
        }
        if let Some(slot) = offload
            .result
            .as_ref()
            .filter(|slot| slot.recipe.destroy.is_some())
        {
            let live = self.ctx.append_basic_block(function, "offload.result.live");
            let done = self.ctx.append_basic_block(function, "offload.released");
            let owned = builder
                .build_int_compare(
                    IntPredicate::NE,
                    result_live,
                    self.ctx.i32_type().const_zero(),
                    "offload.result.owned",
                )
                .llvm_ctx("test offload result ownership")?;
            builder
                .build_conditional_branch(owned, live, done)
                .llvm_ctx("select offload result release")?;
            builder.position_at_end(live);
            destroy(offload.params.len(), slot)?;
            builder
                .build_unconditional_branch(done)
                .llvm_ctx("finish offload result release")?;
            builder.position_at_end(done);
        }
        builder
            .build_return(None)
            .llvm_ctx("finish offload release")?;
        Ok(())
    }

    /// Move the inputs into a fresh environment, hand it to the pool, and
    /// park until the job finishes or the task is cancelled. Cancellation
    /// does not wait: the operation keeps the environment until the job ends.
    pub(super) fn emit_offload(
        &self,
        id: hew_mir::physical::OffloadId,
        args: &[ArgumentTransfer],
        result: Option<StorageId>,
        normal: &PhysicalEdge,
        cancel: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let frame = self.frame.as_ref().ok_or_else(|| {
            CodegenError::FailClosed("an offloaded call requires a resumable body".into())
        })?;
        let thunks = self.offload_thunks(id)?;
        let data = TargetData::create(&self.module.target.data_layout);
        let size = data.get_abi_size(&thunks.env);
        let align = u64::from(data.get_abi_alignment(&thunks.env));
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let i64_ty = self.ctx.i64_type();
        let usize_ty = self.ctx.ptr_sized_int_type(&data, None);
        let alloc = get_or_declare_external(
            self.llvm,
            "hew_alloc",
            pointer.fn_type(&[i64_ty.into(), i64_ty.into()], false),
        )?;
        let env = suspend::call_value(
            &self.builder,
            alloc,
            &[
                i64_ty.const_int(size.max(1), false).into(),
                i64_ty.const_int(align, false).into(),
            ],
            "offload.env",
        )?
        .into_pointer_value();
        for (index, transfer) in args.iter().enumerate() {
            let value = match transfer {
                ArgumentTransfer::Move(source) => {
                    let value = self.load(*source, "offload.input")?;
                    self.clear_owned(*source)?;
                    value
                }
                ArgumentTransfer::Clone { source, action } => self.clone_value(*source, *action)?,
                ArgumentTransfer::Borrow(_) | ArgumentTransfer::BorrowMut(_) => {
                    return Err(CodegenError::FailClosed(
                        "an offloaded call owns its inputs and borrows none".into(),
                    ));
                }
            };
            let field = self
                .builder
                .build_struct_gep(thunks.env, env, index as u32, "offload.input.slot")
                .llvm_ctx("address offload input")?;
            self.builder
                .build_store(field, value)
                .llvm_ctx("move input into offload environment")?;
        }
        let waker = self.task_pointer_call("hew_coro_state_waker", &[frame.state.into()])?;
        let submit = get_or_declare_external(
            self.llvm,
            "hew_async_offload",
            pointer.fn_type(
                &[
                    pointer.into(),
                    pointer.into(),
                    pointer.into(),
                    usize_ty.into(),
                    usize_ty.into(),
                    pointer.into(),
                ],
                false,
            ),
        )?;
        let request = suspend::call_value(
            &self.builder,
            submit,
            &[
                thunks.run.as_global_value().as_pointer_value().into(),
                thunks.release.as_global_value().as_pointer_value().into(),
                env.into(),
                usize_ty.const_int(size.max(1), false).into(),
                usize_ty.const_int(align, false).into(),
                waker.into(),
            ],
            "offload.request",
        )?
        .into_pointer_value();
        let (completed, cancelled) = self.wait_for_request(request)?;
        self.builder.position_at_end(cancelled);
        self.state_value("hew_async_io_cancel", request)?;
        self.free_handle("hew_async_io_free", request)?;
        self.initialize_cancellation_fault()?;
        self.emit_edge(cancel)?;

        self.builder.position_at_end(completed);
        let taken = self.ctx.append_basic_block(self.value, "offload.taken");
        let invalid = self.ctx.append_basic_block(self.value, "offload.invalid");
        // Restore the carried error slot before the take consumes the result.
        self.state_value("hew_async_io_restore_error", request)?;
        let (output, offset, length) = match result {
            Some(result) => {
                let index = u32::try_from(args.len())
                    .map_err(|_| CodegenError::FailClosed("offload arity exceeds u32".into()))?;
                let offset = data.offset_of_element(&thunks.env, index).ok_or_else(|| {
                    CodegenError::FailClosed("offload result has no offset".into())
                })?;
                let length =
                    data.get_abi_size(&llvm_type(self.ctx, &self.storage(result)?.layout.repr)?);
                (self.slots[result.0 as usize], offset, length)
            }
            None => (pointer.const_null(), 0, 0),
        };
        let take = get_or_declare_external(
            self.llvm,
            "hew_async_io_take_offload",
            self.ctx.i32_type().fn_type(
                &[
                    pointer.into(),
                    pointer.into(),
                    usize_ty.into(),
                    usize_ty.into(),
                ],
                false,
            ),
        )?;
        let status = suspend::call_value(
            &self.builder,
            take,
            &[
                request.into(),
                output.into(),
                usize_ty.const_int(offset, false).into(),
                usize_ty.const_int(length, false).into(),
            ],
            "offload.take",
        )?
        .into_int_value();
        let ok = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                self.ctx.i32_type().const_int(1, false),
                "offload.took",
            )
            .llvm_ctx("check offload result transfer")?;
        self.builder
            .build_conditional_branch(ok, taken, invalid)
            .llvm_ctx("validate offload take")?;
        self.builder.position_at_end(invalid);
        self.reject_invalid_task_state()?;
        self.builder.position_at_end(taken);
        self.free_handle("hew_async_io_free", request)?;
        self.emit_result_edge(result, normal)
    }
}
