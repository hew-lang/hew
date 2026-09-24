//! Terminators, checked arithmetic, calls, edges, defer and fault propagation.

use super::*;

impl<'a, 'ctx> FunctionEmitter<'a, 'ctx> {
    pub(super) fn emit_terminator(&self, block: &PhysicalBlock) -> CodegenResult<()> {
        match &block.terminator {
            PhysicalTerminator::RemoteAsk {
                actor,
                message,
                target,
                payload,
                timeout,
                result,
                normal,
                cancel,
                unwind,
            } => self.emit_remote_ask(
                *actor, *message, *target, *payload, *timeout, *result, normal, cancel, unwind,
            ),
            PhysicalTerminator::ActorAsk {
                actor,
                message,
                policy,
                deadline_ns,
                sealed,
                args,
                result,
                normal,
                cancel,
                unwind,
            } => self.emit_actor_ask(
                *actor,
                *message,
                *policy,
                *deadline_ns,
                *sealed,
                args,
                *result,
                normal,
                cancel,
                unwind,
            ),
            PhysicalTerminator::TaskSelect {
                order,
                sources,
                timeout,
                result,
                normal,
                cancel,
                unwind,
            } => self.emit_task_select(*order, sources, *timeout, *result, normal, cancel, unwind),
            PhysicalTerminator::GeneratorYield {
                value,
                normal,
                cancel,
                ..
            } => self.emit_generator_yield(value, normal, cancel),
            PhysicalTerminator::GeneratorNext {
                generator,
                result,
                normal,
                cancel,
                unwind,
            } => self.emit_generator_next(generator, *result, normal, cancel, unwind),
            PhysicalTerminator::StreamNext { .. } => self.emit_stream_next(block),
            PhysicalTerminator::StreamSend { .. } => self.emit_stream_send(block),
            PhysicalTerminator::TaskAwait {
                task,
                result,
                normal,
                cancel,
                unwind,
            } => self.emit_task_await(task, *result, normal, cancel, unwind),
            PhysicalTerminator::TaskScopeJoin {
                scope,
                mode,
                normal,
                unwind,
            } => self.emit_task_scope_join(*scope, *mode, normal, unwind),
            PhysicalTerminator::NativeIo {
                operation,
                args,
                result,
                normal,
                cancel,
                ..
            } => self.emit_native_io(*operation, args, *result, normal, cancel),
            PhysicalTerminator::Sleep {
                duration,
                normal,
                cancel,
                unwind,
            } => self.emit_sleep(*duration, normal, cancel, unwind),
            PhysicalTerminator::SleepUntil {
                deadline,
                normal,
                cancel,
                unwind,
            } => self.emit_sleep_until(*deadline, normal, cancel, unwind),
            PhysicalTerminator::EnterDefer { park, body, .. } => self.emit_enter_defer(*park, body),
            PhysicalTerminator::FinishDefer { park, next, .. } => {
                self.emit_finish_defer(*park, next)
            }
            PhysicalTerminator::CleanupDispatch { normal, fault } => {
                self.emit_cleanup_dispatch(normal, fault)
            }
            PhysicalTerminator::RecoverFault {
                result,
                glue,
                deadline_variant,
                fault_variant,
                normal,
                unwind,
            } => self.emit_scope_recovery(
                *result,
                *glue,
                *deadline_variant,
                *fault_variant,
                normal,
                unwind,
            ),
            PhysicalTerminator::CheckedRaiseFault { kind, cleanup } => {
                self.initialize_active_fault(trap_code(*kind))?;
                self.emit_edge(cleanup)
            }
            PhysicalTerminator::DynCall {
                receiver,
                slot,
                signature,
                args,
                result,
                normal,
                unwind,
            } => self.emit_dyn_call(
                *receiver,
                *slot,
                signature,
                args,
                *result,
                normal.as_ref(),
                unwind.as_ref(),
            ),
            PhysicalTerminator::IndirectCall {
                callee,
                signature,
                args,
                result,
                normal,
                unwind,
            } => self.emit_indirect_call(
                *callee,
                signature,
                args,
                *result,
                normal.as_ref(),
                unwind.as_ref(),
            ),

            PhysicalTerminator::Return { value } => self.emit_return(*value),
            PhysicalTerminator::Goto(edge) => self.emit_edge(edge),
            PhysicalTerminator::Branch {
                condition,
                then_target,
                else_target,
            } => {
                let condition = self.load(*condition, "branch.condition")?.into_int_value();
                let condition = self
                    .builder
                    .build_int_compare(
                        IntPredicate::NE,
                        condition,
                        condition.get_type().const_zero(),
                        "branch.truth",
                    )
                    .llvm_ctx("normalize physical branch condition")?;
                let then_block = self.ctx.append_basic_block(self.value, "branch.then.edge");
                let else_block = self.ctx.append_basic_block(self.value, "branch.else.edge");
                self.builder
                    .build_conditional_branch(condition, then_block, else_block)
                    .llvm_ctx("emit physical branch")?;
                self.builder.position_at_end(then_block);
                self.emit_edge(then_target)?;
                self.builder.position_at_end(else_block);
                self.emit_edge(else_target)
            }
            PhysicalTerminator::SwitchVariant {
                scrutinee,
                glue,
                arms,
            } => self.emit_variant_switch(*scrutinee, *glue, arms),
            PhysicalTerminator::CheckedBinary {
                op,
                lhs,
                rhs,
                result,
                normal,
                failures,
            } => self.emit_checked_binary(*op, *lhs, *rhs, *result, normal, failures),
            PhysicalTerminator::ActorCall {
                operation,
                args,
                result,
                normal,
                unwind,
            } => self.emit_actor_call(operation.clone(), args, *result, normal, unwind.as_ref()),
            PhysicalTerminator::Call {
                callee,
                args,
                result,
                normal,
                unwind,
                handback,
            } => self.emit_call(
                *callee,
                args,
                *result,
                *handback,
                normal.as_ref(),
                unwind.as_ref(),
            ),
            PhysicalTerminator::WireCodec {
                direction,
                plan,
                recipes,
                text_result,
                input,
                result,
                normal,
                unwind,
            } => self.emit_wire_codec(
                *direction,
                plan,
                recipes,
                *text_result,
                *input,
                *result,
                normal,
                unwind,
            ),
            PhysicalTerminator::ValueCall {
                ty,
                capability,
                args,
                result,
                normal,
                unwind,
            } => self.emit_value_call(ty, *capability, args, *result, normal, unwind),
            PhysicalTerminator::RuntimeCall {
                action,
                args,
                result,
                normal,
                failure,
            } => self.emit_runtime_call(*action, args, *result, normal, failure.as_ref()),
            PhysicalTerminator::ExternCall {
                symbol,
                args,
                result,
                result_abi,
                normal,
                ..
            } => self.emit_extern_call(symbol, args, *result, result_abi, normal),
            PhysicalTerminator::Panic { message, cleanup } => self.emit_panic(*message, cleanup),
            PhysicalTerminator::Trap(kind) => {
                let code = trap_code(*kind);
                self.emit_new_fault(code)
            }
            PhysicalTerminator::PropagateFault { handback } => {
                if let Some(handback) = handback {
                    self.emit_receiver_handback(*handback)?;
                }
                self.emit_propagate_fault()
            }
            PhysicalTerminator::Unreachable => self
                .builder
                .build_unreachable()
                .llvm_ctx("emit physical unreachable")
                .map(|_| ()),
        }
    }

    fn emit_return(&self, transfer: Option<ReturnTransfer>) -> CodegenResult<()> {
        if let Some(transfer) = transfer {
            let value = match transfer {
                ReturnTransfer::Borrow(source) | ReturnTransfer::Move(source) => {
                    self.load(source, "return.value")?
                }
                ReturnTransfer::Clone { source, action } => self.clone_value(source, action)?,
            };
            let result_out = self.result_out.ok_or_else(|| {
                CodegenError::FailClosed("physical value return has no result-out parameter".into())
            })?;
            self.builder
                .build_store(result_out, value)
                .llvm_ctx("store physical result-out")?;
        }
        self.builder
            .build_store(
                self.fault_out,
                self.ctx.ptr_type(AddressSpace::default()).const_null(),
            )
            .llvm_ctx("clear physical fault-out on success")?;
        self.emit_finish(self.ctx.i32_type().const_zero())
    }

    fn emit_checked_binary(
        &self,
        op: BinaryOp,
        lhs: StorageId,
        rhs: StorageId,
        result: StorageId,
        normal: &PhysicalEdge,
        failures: &[PhysicalCheckedFailure],
    ) -> CodegenResult<()> {
        let left = self.load(lhs, "checked.left")?.into_int_value();
        let right = self.load(rhs, "checked.right")?.into_int_value();
        let signed = is_signed(&self.storage(lhs)?.ty);
        match op {
            BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply => {
                let intrinsic_name = match (op, signed) {
                    (BinaryOp::Add, true) => "llvm.sadd.with.overflow",
                    (BinaryOp::Add, false) => "llvm.uadd.with.overflow",
                    (BinaryOp::Subtract, true) => "llvm.ssub.with.overflow",
                    (BinaryOp::Subtract, false) => "llvm.usub.with.overflow",
                    (BinaryOp::Multiply, true) => "llvm.smul.with.overflow",
                    (BinaryOp::Multiply, false) => "llvm.umul.with.overflow",
                    _ => unreachable!("matched checked add, subtract or multiply"),
                };
                let intrinsic = Intrinsic::find(intrinsic_name).ok_or_else(|| {
                    CodegenError::FailClosed(format!(
                        "LLVM intrinsic `{intrinsic_name}` is unavailable"
                    ))
                })?;
                let declaration = intrinsic
                    .get_declaration(self.llvm, &[left.get_type().into()])
                    .ok_or_else(|| {
                        CodegenError::FailClosed(format!(
                            "LLVM intrinsic `{intrinsic_name}` has no declaration for the checked integer width"
                        ))
                    })?;
                let aggregate = self
                    .builder
                    .build_call(
                        declaration,
                        &[left.into(), right.into()],
                        "checked.with.overflow",
                    )
                    .llvm_ctx("emit checked arithmetic intrinsic")?
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| {
                        CodegenError::FailClosed(
                            "checked arithmetic intrinsic unexpectedly returned void".into(),
                        )
                    })?
                    .into_struct_value();
                let value = self
                    .builder
                    .build_extract_value(aggregate, 0, "checked.value")
                    .llvm_ctx("extract checked arithmetic result")?
                    .into_int_value();
                let overflow = self
                    .builder
                    .build_extract_value(aggregate, 1, "checked.overflow")
                    .llvm_ctx("extract checked arithmetic overflow")?
                    .into_int_value();
                self.emit_checked_choice(
                    overflow,
                    value,
                    result,
                    normal,
                    failure_edge(failures, TrapKind::IntegerOverflow)?,
                    "overflow",
                )
            }
            BinaryOp::Divide | BinaryOp::Modulo => {
                self.emit_checked_division(op, left, right, result, normal, failures, signed)
            }
            BinaryOp::Shl | BinaryOp::Shr => {
                let invalid = self
                    .builder
                    .build_int_compare(
                        IntPredicate::UGE,
                        right,
                        right
                            .get_type()
                            .const_int(u64::from(right.get_type().get_bit_width()), false),
                        "checked.shift.invalid",
                    )
                    .llvm_ctx("guard checked shift count")?;
                let failure = failure_edge(failures, TrapKind::ShiftOutOfRange)?;
                let failure_block = self
                    .ctx
                    .append_basic_block(self.value, "checked.shift.fail");
                let safe_block = self
                    .ctx
                    .append_basic_block(self.value, "checked.shift.safe");
                self.builder
                    .build_conditional_branch(invalid, failure_block, safe_block)
                    .llvm_ctx("branch on checked shift guard")?;
                self.builder.position_at_end(failure_block);
                self.emit_edge(failure)?;
                self.builder.position_at_end(safe_block);
                let value = match op {
                    BinaryOp::Shl => self
                        .builder
                        .build_left_shift(left, right, "checked.shl")
                        .llvm_ctx("emit guarded left shift")?,
                    BinaryOp::Shr => self
                        .builder
                        .build_right_shift(left, right, signed, "checked.shr")
                        .llvm_ctx("emit guarded right shift")?,
                    _ => unreachable!("matched checked shift"),
                };
                self.store(result, value.into())?;
                self.emit_result_edge(Some(result), normal)
            }
            BinaryOp::Equal
            | BinaryOp::NotEqual
            | BinaryOp::Less
            | BinaryOp::LessEqual
            | BinaryOp::Greater
            | BinaryOp::GreaterEqual
            | BinaryOp::And
            | BinaryOp::Or
            | BinaryOp::BitAnd
            | BinaryOp::BitOr
            | BinaryOp::BitXor
            | BinaryOp::Range
            | BinaryOp::RangeInclusive
            | BinaryOp::WrappingAdd
            | BinaryOp::WrappingSub
            | BinaryOp::WrappingMul => Err(CodegenError::FailClosed(
                "non-fallible operation reached checked physical terminator".into(),
            )),
        }
    }

    pub(super) fn emit_checked_choice(
        &self,
        failed: IntValue<'ctx>,
        value: IntValue<'ctx>,
        result: StorageId,
        normal: &PhysicalEdge,
        failure: &PhysicalEdge,
        name: &str,
    ) -> CodegenResult<()> {
        let failure_block = self
            .ctx
            .append_basic_block(self.value, &format!("checked.{name}.fail"));
        let normal_block = self
            .ctx
            .append_basic_block(self.value, &format!("checked.{name}.normal"));
        self.builder
            .build_conditional_branch(failed, failure_block, normal_block)
            .llvm_ctx("branch on checked arithmetic result")?;
        self.builder.position_at_end(failure_block);
        self.emit_edge(failure)?;
        self.builder.position_at_end(normal_block);
        self.store(result, value.into())?;
        self.emit_result_edge(Some(result), normal)
    }

    #[allow(
        clippy::too_many_arguments,
        reason = "the parameters are the complete checked division physical contract"
    )]
    fn emit_checked_division(
        &self,
        op: BinaryOp,
        left: IntValue<'ctx>,
        right: IntValue<'ctx>,
        result: StorageId,
        normal: &PhysicalEdge,
        failures: &[PhysicalCheckedFailure],
        signed: bool,
    ) -> CodegenResult<()> {
        let zero = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                right,
                right.get_type().const_zero(),
                "checked.div.zero",
            )
            .llvm_ctx("guard division by zero")?;
        let zero_block = self
            .ctx
            .append_basic_block(self.value, "checked.div.zero.fail");
        let nonzero_block = self
            .ctx
            .append_basic_block(self.value, "checked.div.nonzero");
        self.builder
            .build_conditional_branch(zero, zero_block, nonzero_block)
            .llvm_ctx("branch on division-by-zero guard")?;
        self.builder.position_at_end(zero_block);
        self.emit_edge(failure_edge(failures, TrapKind::DivideByZero)?)?;
        self.builder.position_at_end(nonzero_block);

        if signed {
            let min = left
                .get_type()
                .const_int(1_u64 << (left.get_type().get_bit_width() - 1), false);
            let is_min = self
                .builder
                .build_int_compare(IntPredicate::EQ, left, min, "checked.div.min")
                .llvm_ctx("guard signed division minimum")?;
            let is_negative_one = self
                .builder
                .build_int_compare(
                    IntPredicate::EQ,
                    right,
                    right.get_type().const_all_ones(),
                    "checked.div.negative.one",
                )
                .llvm_ctx("guard signed division negative one")?;
            let invalid = self
                .builder
                .build_and(is_min, is_negative_one, "checked.div.min.negative.one")
                .llvm_ctx("combine signed division guards")?;
            let failure_block = self
                .ctx
                .append_basic_block(self.value, "checked.div.min.fail");
            let safe_block = self.ctx.append_basic_block(self.value, "checked.div.safe");
            self.builder
                .build_conditional_branch(invalid, failure_block, safe_block)
                .llvm_ctx("branch on signed division guard")?;
            self.builder.position_at_end(failure_block);
            self.emit_edge(failure_edge(failures, TrapKind::SignedMinDivNegOne)?)?;
            self.builder.position_at_end(safe_block);
        }

        let value = match (op, signed) {
            (BinaryOp::Divide, true) => {
                self.builder
                    .build_int_signed_div(left, right, "checked.sdiv")
            }
            (BinaryOp::Divide, false) => {
                self.builder
                    .build_int_unsigned_div(left, right, "checked.udiv")
            }
            (BinaryOp::Modulo, true) => {
                self.builder
                    .build_int_signed_rem(left, right, "checked.srem")
            }
            (BinaryOp::Modulo, false) => {
                self.builder
                    .build_int_unsigned_rem(left, right, "checked.urem")
            }
            _ => unreachable!("matched checked division or modulo"),
        }
        .llvm_ctx("emit guarded division or modulo")?;
        self.store(result, value.into())?;
        self.emit_result_edge(Some(result), normal)
    }

    /// A successful terminator defines its result even when a runtime writes
    /// directly to its output address. Publish that fact before edge snapshots.
    pub(super) fn emit_result_edge(
        &self,
        result: Option<StorageId>,
        edge: &PhysicalEdge,
    ) -> CodegenResult<()> {
        if let Some(result) = result {
            self.set_place_initialized(result, true)?;
        }
        self.emit_edge(edge)
    }

    pub(super) fn emit_edge(&self, edge: &PhysicalEdge) -> CodegenResult<()> {
        let values = edge
            .transfers
            .iter()
            .map(|(source, _)| self.load(*source, "edge.value"))
            .collect::<CodegenResult<Vec<_>>>()?;
        let initialized = edge
            .leaf_transfers
            .iter()
            .map(|(source, _)| self.place_initialized(*source))
            .collect::<CodegenResult<Vec<_>>>()?;
        let destinations = edge
            .transfers
            .iter()
            .map(|(_, destination)| *destination)
            .collect::<std::collections::BTreeSet<_>>();
        for (source, _) in &edge.transfers {
            if !destinations.contains(source) {
                self.clear_owned(*source)?;
            }
        }
        for ((_, destination), value) in edge.transfers.iter().zip(values) {
            self.store(*destination, value)?;
        }
        for ((_, destination), initialized) in edge.leaf_transfers.iter().zip(initialized) {
            self.builder
                .build_store(self.place_flag(*destination)?, initialized)
                .llvm_ctx("transfer aggregate leaf initialization")?;
        }
        self.builder
            .build_unconditional_branch(self.blocks[&edge.target])
            .llvm_ctx("emit physical edge")?;
        Ok(())
    }

    fn emit_call(
        &self,
        callee_id: CallableId,
        transfers: &[ArgumentTransfer],
        result: Option<StorageId>,
        handback: Option<StorageId>,
        normal: Option<&PhysicalEdge>,
        unwind: Option<&PhysicalEdge>,
    ) -> CodegenResult<()> {
        let callee = callable(self.module, callee_id)?;
        let function = *self.functions.get(&callee_id).ok_or_else(|| {
            CodegenError::FailClosed(format!("missing LLVM callee {}", callee_id.0))
        })?;
        self.builder
            .build_store(
                self.active_fault,
                self.ctx.ptr_type(AddressSpace::default()).const_null(),
            )
            .llvm_ctx("clear active fault before call")?;
        let mut arguments = Vec::<BasicMetadataValueEnum<'ctx>>::new();
        let mut moved = Vec::new();
        for (transfer, parameter) in transfers.iter().zip(&callee.params) {
            let (source, value) = match transfer {
                ArgumentTransfer::Borrow(source) | ArgumentTransfer::BorrowMut(source) => {
                    (*source, None)
                }
                ArgumentTransfer::Move(source) => {
                    moved.push(*source);
                    (*source, None)
                }
                ArgumentTransfer::Clone { source, action } => {
                    (*source, Some(self.clone_value(*source, *action)?))
                }
            };
            match parameter.carrier {
                ParamCarrier::Direct => arguments.push(
                    value
                        .map_or_else(|| self.load(source, "call.argument"), Ok)?
                        .into(),
                ),
                ParamCarrier::Indirect => {
                    if let Some(value) = value {
                        let temp = self.value_emitter().entry_scratch(
                            llvm_type(self.ctx, &parameter.layout.repr)?,
                            "call.clone.argument",
                        )?;
                        self.builder
                            .build_store(temp, value)
                            .llvm_ctx("store cloned indirect argument")?;
                        arguments.push(temp.into());
                    } else {
                        arguments.push(self.slots[source.0 as usize].into());
                    }
                }
            }
        }
        if let Some(result) = result {
            arguments.push(self.slots[result.0 as usize].into());
        }
        arguments.push(self.active_fault.into());
        let handback = handback.zip(result);
        if callee.is_resumable {
            let status = self.emit_resumable_call(callee_id, &arguments, &moved)?;
            return self.emit_call_outcome_with_handback(status, result, handback, normal, unwind);
        }
        let status = self
            .builder
            .build_call(function, &arguments, "call.status")
            .llvm_ctx("emit physical private call")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("physical call returned no status".into()))?
            .into_int_value();
        self.builder
            .build_store(self.active_status, status)
            .llvm_ctx("store active call status")?;
        for source in moved {
            self.clear_owned(source)?;
        }
        self.emit_call_outcome_with_handback(status, result, handback, normal, unwind)
    }

    fn emit_value_call(
        &self,
        ty: &ResolvedTy,
        capability: ValueCapability,
        transfers: &[ArgumentTransfer],
        result: StorageId,
        normal: &PhysicalEdge,
        unwind: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let callback = *self
            .value_callbacks
            .get(&(ty.clone(), capability))
            .ok_or_else(|| {
                CodegenError::FailClosed(
                    "physical value call lacks its exact selected callback".into(),
                )
            })?;
        let mut arguments = Vec::<BasicMetadataValueEnum<'ctx>>::new();
        for transfer in transfers {
            let ArgumentTransfer::Borrow(source) = transfer else {
                return Err(CodegenError::FailClosed(
                    "physical value callback requires borrowed argument storage".into(),
                ));
            };
            // All physical operands, including constants and direct scalars,
            // already have aligned entry storage. Borrow its address without
            // cloning an owner or adapting the selected user method here.
            arguments.push(self.slots[source.0 as usize].into());
        }
        arguments.push(self.slots[result.0 as usize].into());
        arguments.push(self.active_fault.into());
        self.builder
            .build_store(
                self.active_fault,
                self.ctx.ptr_type(AddressSpace::default()).const_null(),
            )
            .llvm_ctx("clear active fault before selected value call")?;
        let status = self.value_emitter().invoke_value_callback(
            self.frame.as_ref(),
            ty,
            capability,
            callback,
            &arguments,
        )?;
        self.builder
            .build_store(self.active_status, status)
            .llvm_ctx("store selected value call status")?;
        self.emit_call_outcome(status, Some(result), Some(normal), Some(unwind))
    }

    pub(super) fn emit_call_outcome(
        &self,
        status: IntValue<'ctx>,
        result: Option<StorageId>,
        normal: Option<&PhysicalEdge>,
        unwind: Option<&PhysicalEdge>,
    ) -> CodegenResult<()> {
        self.emit_call_outcome_with_handback(status, result, None, normal, unwind)
    }

    /// A failing `var self` callee left its receiver in the dual result's
    /// receiver field; the unwind edge takes it from there.
    fn emit_call_outcome_with_handback(
        &self,
        status: IntValue<'ctx>,
        result: Option<StorageId>,
        handback: Option<(StorageId, StorageId)>,
        normal: Option<&PhysicalEdge>,
        unwind: Option<&PhysicalEdge>,
    ) -> CodegenResult<()> {
        let success = self.ctx.append_basic_block(self.value, "call.success");
        let failure = self.ctx.append_basic_block(self.value, "call.failure");
        let ok = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                self.ctx.i32_type().const_zero(),
                "call.ok",
            )
            .llvm_ctx("compare physical call status")?;
        self.builder
            .build_conditional_branch(ok, success, failure)
            .llvm_ctx("branch on physical call status")?;
        self.builder.position_at_end(success);
        if let Some(normal) = normal {
            self.emit_result_edge(result, normal)?;
        } else {
            self.reject_invalid_task_state()?;
        }
        self.builder.position_at_end(failure);
        if let Some((handback, result)) = handback {
            let receiver = self.dual_result_receiver(self.slots[result.0 as usize], result)?;
            let value = self
                .builder
                .build_load(
                    llvm_type(self.ctx, &self.storage(handback)?.layout.repr)?,
                    receiver,
                    "call.handback",
                )
                .llvm_ctx("take handed-back receiver")?;
            self.store(handback, value)?;
        }
        if let Some(unwind) = unwind {
            self.emit_edge(unwind)
        } else {
            self.emit_propagate_fault()
        }
    }

    /// The receiver field of a `var self` method's dual result.
    fn dual_result_receiver(
        &self,
        result: PointerValue<'ctx>,
        storage: StorageId,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let dual = llvm_type(self.ctx, &self.storage(storage)?.layout.repr)?.into_struct_type();
        self.builder
            .build_struct_gep(dual, result, 1, "dual.receiver")
            .llvm_ctx("address dual result receiver")
    }

    /// Hand a failing `var self` method's receiver back to its caller.
    fn emit_receiver_handback(&self, handback: StorageId) -> CodegenResult<()> {
        let result_out = self.result_out.ok_or_else(|| {
            CodegenError::FailClosed("receiver handback has no result-out parameter".into())
        })?;
        let layout = callable(self.module, self.function.callable)?
            .return_layout
            .as_ref()
            .ok_or_else(|| {
                CodegenError::FailClosed("receiver handback lacks a dual result".into())
            })?;
        let dual = llvm_type(self.ctx, &layout.repr)?.into_struct_type();
        let receiver = self
            .builder
            .build_struct_gep(dual, result_out, 1, "dual.receiver")
            .llvm_ctx("address handed-back receiver")?;
        let value = self.load(handback, "handback.value")?;
        self.builder
            .build_store(receiver, value)
            .llvm_ctx("hand back receiver")?;
        Ok(())
    }

    pub(super) fn runtime_call_value(
        &self,
        function: FunctionValue<'ctx>,
        arguments: &[BasicMetadataValueEnum<'ctx>],
        name: &str,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        self.builder
            .build_call(function, arguments, name)
            .llvm_ctx("emit physical runtime call")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "physical runtime call `{name}` returned no value"
                ))
            })
    }

    pub(super) fn runtime_call_void(
        &self,
        function: FunctionValue<'ctx>,
        arguments: &[BasicMetadataValueEnum<'ctx>],
        name: &str,
    ) -> CodegenResult<()> {
        let call = self
            .builder
            .build_call(function, arguments, name)
            .llvm_ctx("emit physical runtime call")?;
        if call.try_as_basic_value().basic().is_some() {
            return Err(CodegenError::FailClosed(format!(
                "physical runtime call `{name}` unexpectedly returned a value"
            )));
        }
        Ok(())
    }

    fn emit_new_fault(&self, code: i32) -> CodegenResult<()> {
        self.initialize_active_fault(code)?;
        self.emit_propagate_fault()
    }

    fn emit_enter_defer(
        &self,
        park: hew_mir::physical::FaultParkId,
        body: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let (park_fault, park_status) = self.fault_parks[&park];
        let fault = self
            .builder
            .build_load(
                self.ctx.ptr_type(AddressSpace::default()),
                self.active_fault,
                "defer.primary",
            )
            .llvm_ctx("load optional active fault")?;
        let status = self
            .builder
            .build_load(
                self.ctx.i32_type(),
                self.active_status,
                "defer.primary.status",
            )
            .llvm_ctx("load optional active status")?;
        self.builder
            .build_store(park_fault, fault)
            .llvm_ctx("park fault owner")?;
        self.builder
            .build_store(park_status, status)
            .llvm_ctx("park fault status")?;
        self.clear_fault_pair(self.active_fault, self.active_status)?;
        self.emit_edge(body)
    }

    fn clear_fault_pair(
        &self,
        fault: PointerValue<'ctx>,
        status: PointerValue<'ctx>,
    ) -> CodegenResult<()> {
        self.builder
            .build_store(
                fault,
                self.ctx.ptr_type(AddressSpace::default()).const_null(),
            )
            .llvm_ctx("clear consumed fault owner")?;
        self.builder
            .build_store(status, self.ctx.i32_type().const_zero())
            .llvm_ctx("clear consumed fault status")?;
        Ok(())
    }

    fn emit_finish_defer(
        &self,
        park: hew_mir::physical::FaultParkId,
        next: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let (park_fault, park_status) = self.fault_parks[&park];
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let primary = self
            .builder
            .build_load(pointer, park_fault, "defer.parked")
            .llvm_ctx("load parked primary")?
            .into_pointer_value();
        let secondary = self
            .builder
            .build_load(pointer, self.active_fault, "defer.secondary")
            .llvm_ctx("load deferred fault")?;
        let primary_status = self
            .builder
            .build_load(self.ctx.i32_type(), park_status, "defer.parked.status")
            .llvm_ctx("load parked primary status")?;
        let secondary_status = self
            .builder
            .build_load(
                self.ctx.i32_type(),
                self.active_status,
                "defer.secondary.status",
            )
            .llvm_ctx("load deferred status")?;
        let present = self
            .builder
            .build_is_not_null(primary, "defer.primary.present")
            .llvm_ctx("test parked primary")?;
        let status = self
            .builder
            .build_select(
                present,
                primary_status,
                secondary_status,
                "defer.combined.status",
            )
            .llvm_ctx("preserve first fault status")?;
        // Each pair transfers one distinct optional owner. Emptying both slots
        // before the consuming helper prevents accidental reuse on later edges.
        self.clear_fault_pair(park_fault, park_status)?;
        self.clear_fault_pair(self.active_fault, self.active_status)?;
        let combine = get_or_declare_external(
            self.llvm,
            "hew_fault_combine",
            pointer.fn_type(&[pointer.into(), pointer.into()], false),
        )?;
        let fault = self.runtime_call_value(
            combine,
            &[primary.into(), secondary.into()],
            "defer.combined",
        )?;
        self.builder
            .build_store(self.active_fault, fault)
            .llvm_ctx("install combined fault owner")?;
        self.builder
            .build_store(self.active_status, status)
            .llvm_ctx("install combined fault status")?;
        self.emit_edge(next)
    }

    fn emit_scope_recovery(
        &self,
        result: StorageId,
        glue: PhysicalVariantId,
        deadline_variant: u32,
        fault_variant: u32,
        normal: &PhysicalEdge,
        unwind: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let frame = self.frame.as_ref().ok_or_else(|| {
            CodegenError::FailClosed("scope recovery requires a resumable invocation".into())
        })?;
        let cancelled = self.state_value("hew_coro_state_is_cancelled", frame.state)?;
        let cancelled = self
            .builder
            .build_int_compare(
                inkwell::IntPredicate::NE,
                cancelled,
                cancelled.get_type().const_zero(),
                "recovery.parent.cancelled",
            )
            .llvm_ctx("test parent cancellation")?;
        let bypass = self.ctx.append_basic_block(self.value, "recovery.bypass");
        let recover = self.ctx.append_basic_block(self.value, "recovery.consume");
        self.builder
            .build_conditional_branch(cancelled, bypass, recover)
            .llvm_ctx("dispatch scope recovery")?;
        self.builder.position_at_end(bypass);
        self.emit_edge(unwind)?;
        self.builder.position_at_end(recover);
        let code = self
            .builder
            .build_load(self.ctx.i32_type(), self.active_status, "recovery.code")
            .llvm_ctx("load fault category")?
            .into_int_value();
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let fault = self
            .builder
            .build_load(pointer, self.active_fault, "recovery.fault")
            .llvm_ctx("load recovery fault")?;
        let message = self.task_pointer_call("hew_fault_take_message", &[fault.into()])?;
        self.clear_fault_pair(self.active_fault, self.active_status)?;
        let is_deadline = self
            .builder
            .build_int_compare(
                inkwell::IntPredicate::EQ,
                code,
                self.ctx.i32_type().const_int((-2_i32) as u64, true),
                "recovery.deadline",
            )
            .llvm_ctx("classify scope failure")?;
        let deadline = self
            .ctx
            .append_basic_block(self.value, "recovery.deadline.case");
        let logical = self
            .ctx
            .append_basic_block(self.value, "recovery.fault.case");
        let done = self.ctx.append_basic_block(self.value, "recovery.ready");
        self.builder
            .build_conditional_branch(is_deadline, deadline, logical)
            .llvm_ctx("select failure variant")?;
        self.builder.position_at_end(deadline);
        self.write_variant_value(
            self.slots[result.0 as usize],
            deadline_variant,
            &[message.into()],
            glue,
        )?;
        self.builder
            .build_unconditional_branch(done)
            .llvm_ctx("finish deadline recovery")?;
        self.builder.position_at_end(logical);
        self.write_variant_value(
            self.slots[result.0 as usize],
            fault_variant,
            &[message.into()],
            glue,
        )?;
        self.builder
            .build_unconditional_branch(done)
            .llvm_ctx("finish fault recovery")?;
        self.builder.position_at_end(done);
        self.emit_edge(normal)
    }

    fn emit_cleanup_dispatch(
        &self,
        normal: &PhysicalEdge,
        fault: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let active = self
            .builder
            .build_load(
                self.ctx.ptr_type(AddressSpace::default()),
                self.active_fault,
                "cleanup.fault",
            )
            .llvm_ctx("load cleanup fault")?
            .into_pointer_value();
        let present = self
            .builder
            .build_is_not_null(active, "cleanup.failed")
            .llvm_ctx("test cleanup fault")?;
        let failed = self
            .ctx
            .append_basic_block(self.value, "cleanup.fault.edge");
        let success = self
            .ctx
            .append_basic_block(self.value, "cleanup.normal.edge");
        self.builder
            .build_conditional_branch(present, failed, success)
            .llvm_ctx("dispatch cleanup outcome")?;
        self.builder.position_at_end(failed);
        self.emit_edge(fault)?;
        self.builder.position_at_end(success);
        self.emit_edge(normal)
    }

    fn emit_panic(&self, message: ArgumentTransfer, cleanup: &PhysicalEdge) -> CodegenResult<()> {
        let ArgumentTransfer::Borrow(source) = message else {
            return Err(CodegenError::FailClosed(
                "physical panic must borrow its message".into(),
            ));
        };
        let constructor = external_unary_ptr(self.ctx, self.llvm, "hew_fault_new_panic")?;
        let message = self.load(source, "panic.message")?;
        let fault = self.runtime_call_value(constructor, &[message.into()], "panic.fault")?;
        self.store_active_fault(fault, HEW_TRAP_USER_PANIC)?;
        self.emit_edge(cleanup)
    }

    pub(super) fn initialize_active_fault(&self, code: i32) -> CodegenResult<()> {
        self.initialize_active_fault_value(self.ctx.i32_type().const_int(code as u64, true))
    }

    pub(super) fn initialize_cancellation_fault(&self) -> CodegenResult<()> {
        let frame = self.frame.as_ref().ok_or_else(|| {
            CodegenError::FailClosed("cancellation requires a resumable invocation".into())
        })?;
        let code = self.state_value("hew_coro_state_cancel_code", frame.state)?;
        self.initialize_active_fault_value(code)
    }

    pub(super) fn initialize_active_fault_value(&self, code: IntValue<'ctx>) -> CodegenResult<()> {
        let function = external_fault_new(self.ctx, self.llvm)?;
        let fault = self
            .builder
            .build_call(function, &[code.into()], "trap.fault")
            .llvm_ctx("create physical trap fault")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("fault constructor returned void".into()))?;
        self.store_active_fault_value(fault, code)
    }

    pub(super) fn store_active_fault(
        &self,
        fault: BasicValueEnum<'ctx>,
        code: i32,
    ) -> CodegenResult<()> {
        self.store_active_fault_value(fault, self.ctx.i32_type().const_int(code as u64, true))
    }

    pub(super) fn store_active_fault_value(
        &self,
        fault: BasicValueEnum<'ctx>,
        code: IntValue<'ctx>,
    ) -> CodegenResult<()> {
        self.builder
            .build_store(self.active_fault, fault)
            .llvm_ctx("store physical trap fault")?;
        self.builder
            .build_store(self.active_status, code)
            .llvm_ctx("retain physical trap status")?;
        Ok(())
    }

    pub(super) fn emit_propagate_fault(&self) -> CodegenResult<()> {
        let fault = self
            .builder
            .build_load(
                self.ctx.ptr_type(AddressSpace::default()),
                self.active_fault,
                "propagate.fault",
            )
            .llvm_ctx("load active fault")?;
        let status = self
            .builder
            .build_load(self.ctx.i32_type(), self.active_status, "propagate.status")
            .llvm_ctx("load active status")?
            .into_int_value();
        self.builder
            .build_store(self.fault_out, fault)
            .llvm_ctx("transfer active fault to caller")?;
        self.emit_finish(status)
    }
}
