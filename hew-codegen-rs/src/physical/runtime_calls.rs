//! Runtime ABI calls, math intrinsics and integer and float methods.

use super::*;

impl<'a, 'ctx> FunctionEmitter<'a, 'ctx> {
    #[allow(
        clippy::too_many_lines,
        reason = "the closed match is the physical runtime ABI authority"
    )]
    pub(super) fn emit_runtime_call(
        &self,
        action: PhysicalRuntimeAction,
        transfers: &[ArgumentTransfer],
        result: Option<StorageId>,
        normal: &PhysicalEdge,
        failure: Option<&PhysicalEdge>,
    ) -> CodegenResult<()> {
        let source = |index: usize| {
            transfers.get(index).map(argument_source).ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "physical runtime action {action:?} lacks argument {index}"
                ))
            })
        };
        let required_result = || {
            result.ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "physical runtime action {action:?} lacks result storage"
                ))
            })
        };
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        // A collection operation is realized entirely by its physical glue.
        match action.carrier {
            PhysicalRuntimeCarrier::Map { operation, glue } => {
                return self.emit_map_call(
                    (operation, glue),
                    transfers,
                    required_result()?,
                    normal,
                    failure,
                );
            }
            PhysicalRuntimeCarrier::Set { operation, glue } => {
                return self.emit_set_call(
                    (operation, glue),
                    transfers,
                    required_result()?,
                    normal,
                    failure,
                );
            }
            PhysicalRuntimeCarrier::Vector { operation, glue } => {
                return self.emit_vector_call(
                    (operation, glue),
                    transfers,
                    required_result()?,
                    normal,
                    failure,
                );
            }
            PhysicalRuntimeCarrier::SharedHandle(glue) => {
                self.emit_shared_call(action.family, glue, transfers, result)?;
                return self.emit_result_edge(result, normal);
            }
            PhysicalRuntimeCarrier::StructuralFormat(glue) => {
                return self.emit_structural_format(
                    glue,
                    transfers,
                    required_result()?,
                    normal,
                    failure,
                );
            }
            PhysicalRuntimeCarrier::Variant(option)
                if action.family == RuntimeCallFamily::WeakUpgradeRc =>
            {
                return self.emit_weak_upgrade(option, transfers, required_result()?, normal);
            }
            PhysicalRuntimeCarrier::Variant(option)
                if matches!(
                    action.family,
                    RuntimeCallFamily::IntArith(
                        IntArithKind::CheckedAdd
                            | IntArithKind::CheckedSub
                            | IntArithKind::CheckedMul,
                        _
                    )
                ) =>
            {
                let RuntimeCallFamily::IntArith(kind, width) = action.family else {
                    unreachable!("matched IntArith above");
                };
                return self.emit_checked_int_arith(
                    (kind, width),
                    option,
                    transfers,
                    required_result()?,
                    normal,
                    failure,
                );
            }
            _ => {}
        }
        match action.family {
            RuntimeCallFamily::SupervisorPool(operation) => {
                let option = match action.carrier {
                    PhysicalRuntimeCarrier::Variant(id) => Some(id),
                    _ => None,
                };
                return self.emit_supervisor_pool_member(
                    operation,
                    option,
                    &transfers.iter().map(argument_source).collect::<Vec<_>>(),
                    required_result()?,
                    normal,
                    failure,
                );
            }
            RuntimeCallFamily::MathIntrinsic(kind) => {
                return self.emit_math_intrinsic(
                    kind,
                    transfers,
                    required_result()?,
                    normal,
                    failure,
                );
            }
            RuntimeCallFamily::IntMethod(op, width) => {
                return self.emit_int_method(
                    op,
                    width,
                    transfers,
                    required_result()?,
                    normal,
                    failure,
                );
            }
            RuntimeCallFamily::IntArith(kind, width) => {
                return self.emit_int_arith(
                    kind,
                    width,
                    transfers,
                    required_result()?,
                    normal,
                    failure,
                );
            }
            RuntimeCallFamily::FloatMethod(op) => {
                return self.emit_float_method(op, transfers, required_result()?, normal, failure);
            }
            RuntimeCallFamily::Tcp(op) => {
                self.emit_tcp_operation(op, transfers, result)?;
            }
            RuntimeCallFamily::FileRead(op) => {
                self.emit_direct_runtime_call(
                    hew_types::RuntimeCallFamily::FileRead(op),
                    transfers,
                    result,
                )?;
            }
            RuntimeCallFamily::StreamClose => {
                self.emit_direct_runtime_call(
                    hew_types::RuntimeCallFamily::StreamClose,
                    transfers,
                    result,
                )?;
            }
            RuntimeCallFamily::SinkClose => {
                self.emit_direct_runtime_call(
                    hew_types::RuntimeCallFamily::SinkClose,
                    transfers,
                    result,
                )?;
            }
            RuntimeCallFamily::SinkClone => {
                self.emit_direct_runtime_call(
                    hew_types::RuntimeCallFamily::SinkClone,
                    transfers,
                    result,
                )?;
            }
            RuntimeCallFamily::SinkFinish => {
                self.emit_direct_runtime_call(
                    hew_types::RuntimeCallFamily::SinkFinish,
                    transfers,
                    result,
                )?;
            }
            RuntimeCallFamily::StreamForward => {
                self.emit_direct_runtime_call(
                    hew_types::RuntimeCallFamily::StreamForward,
                    transfers,
                    result,
                )?;
            }
            RuntimeCallFamily::StreamPairSink => {
                self.emit_direct_runtime_call(
                    hew_types::RuntimeCallFamily::StreamPairSink,
                    transfers,
                    result,
                )?;
            }
            RuntimeCallFamily::StreamPairStream => {
                self.emit_direct_runtime_call(
                    hew_types::RuntimeCallFamily::StreamPairStream,
                    transfers,
                    result,
                )?;
            }
            RuntimeCallFamily::ActorCallFree => {
                self.emit_direct_runtime_call(
                    hew_types::RuntimeCallFamily::ActorCallFree,
                    transfers,
                    result,
                )?;
            }
            RuntimeCallFamily::ActorRequestRelease => {
                self.emit_direct_runtime_call(
                    hew_types::RuntimeCallFamily::ActorRequestRelease,
                    transfers,
                    result,
                )?;
            }
            RuntimeCallFamily::ActorRequestTake => {
                self.emit_direct_runtime_call(
                    hew_types::RuntimeCallFamily::ActorRequestTake,
                    transfers,
                    result,
                )?;
            }
            RuntimeCallFamily::Encoding { format, op } => {
                self.emit_direct_runtime_call(
                    hew_types::RuntimeCallFamily::Encoding { format, op },
                    transfers,
                    result,
                )?;
            }
            RuntimeCallFamily::NodeShutdown => {
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_node_api_shutdown",
                    self.ctx.i32_type().fn_type(&[], false),
                )?;
                let _ = self.runtime_call_value(function, &[], "node.shutdown")?;
            }
            RuntimeCallFamily::NodeId => {
                // `hew_node_api_id` writes the identity and returns 0 only
                // when a stable key-derived identity is loaded.
                let option = variant_carrier(action)?;
                let glue = self
                    .module
                    .variant_glue
                    .iter()
                    .find(|candidate| candidate.id == option)
                    .ok_or_else(|| {
                        CodegenError::FailClosed("node identity lacks its Option recipe".into())
                    })?;
                let payload = glue
                    .variants
                    .first()
                    .and_then(|case| case.fields.first())
                    .ok_or_else(|| {
                        CodegenError::FailClosed("node identity lacks its payload type".into())
                    })?
                    .ty
                    .clone();
                let layout = self.module.target.layout(&payload).ok_or_else(|| {
                    CodegenError::FailClosed("node identity payload lacks its layout".into())
                })?;
                let identity_ty = llvm_type(self.ctx, &layout.repr)?;
                let slot = self
                    .value_emitter()
                    .entry_scratch(identity_ty, "node.id.slot")?;
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_node_api_id",
                    self.ctx
                        .i32_type()
                        .fn_type(&[self.ctx.ptr_type(AddressSpace::default()).into()], false),
                )?;
                let status = self
                    .runtime_call_value(function, &[slot.into()], "node.id.status")?
                    .into_int_value();
                let present = self.ctx.append_basic_block(self.value, "node.id.present");
                let absent = self.ctx.append_basic_block(self.value, "node.id.absent");
                let complete = self.ctx.append_basic_block(self.value, "node.id.complete");
                let destination = self.slots[required_result()?.0 as usize];
                let found = self
                    .builder
                    .build_int_compare(
                        IntPredicate::EQ,
                        status,
                        self.ctx.i32_type().const_zero(),
                        "node.id.found",
                    )
                    .llvm_ctx("check node identity status")?;
                self.builder
                    .build_conditional_branch(found, present, absent)
                    .llvm_ctx("select node identity result")?;
                self.builder.position_at_end(present);
                let value = self
                    .builder
                    .build_load(identity_ty, slot, "node.id.value")
                    .llvm_ctx("load the written node identity")?;
                self.write_variant_value(destination, 0, &[value], option)?;
                self.builder
                    .build_unconditional_branch(complete)
                    .llvm_ctx("finish node identity hit")?;
                self.builder.position_at_end(absent);
                self.write_variant_value(destination, 1, &[], option)?;
                self.builder
                    .build_unconditional_branch(complete)
                    .llvm_ctx("finish node identity miss")?;
                self.builder.position_at_end(complete);
            }
            RuntimeCallFamily::NodeIdentityKey => {
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_node_api_identity_key",
                    self.ctx
                        .ptr_type(AddressSpace::default())
                        .fn_type(&[], false),
                )?;
                let value = self.runtime_call_value(function, &[], "node.identity.key")?;
                self.store(required_result()?, value)?;
            }
            RuntimeCallFamily::NodeRegister => {
                let status_ty = self.ctx.i32_type();
                let ptr = self.ctx.ptr_type(AddressSpace::default());
                let pid_accessor = get_or_declare_external(
                    self.llvm,
                    "hew_local_pid_actor_id",
                    status_ty.fn_type(&[self.ctx.i64_type().into(), ptr.into()], false),
                )?;
                let register = get_or_declare_external(
                    self.llvm,
                    "hew_node_api_register_by_pid_string",
                    status_ty.fn_type(&[ptr.into(), self.ctx.i64_type().into()], false),
                )?;
                let local = self
                    .load(source(1)?, "node.register.local")?
                    .into_int_value();
                let pid_out = self
                    .value_emitter()
                    .entry_scratch(self.ctx.i64_type().into(), "node.register.pid")?;
                self.builder
                    .build_store(pid_out, self.ctx.i64_type().const_zero())
                    .llvm_ctx("initialize node registration pid output")?;
                let _ = self.runtime_call_value(
                    pid_accessor,
                    &[local.into(), pid_out.into()],
                    "node.register.pid",
                )?;
                let pid = self
                    .builder
                    .build_load(self.ctx.i64_type(), pid_out, "node.register.pid.value")
                    .llvm_ctx("load resolved local actor pid")?
                    .into_int_value();
                let status = self.runtime_call_value(
                    register,
                    &[
                        self.load(source(0)?, "node.register.name")?.into(),
                        pid.into(),
                    ],
                    "node.register",
                )?;
                self.store(required_result()?, status)?;
            }
            RuntimeCallFamily::NodeLookup => {
                let PhysicalRuntimeCarrier::NodeResult {
                    result: result_glue,
                    error: error_glue,
                } = action.carrier
                else {
                    return Err(CodegenError::FailClosed(
                        "Node::lookup has no Result carrier".into(),
                    ));
                };
                let status_ty = self.ctx.i32_type();
                let lookup = get_or_declare_external(
                    self.llvm,
                    "hew_node_api_lookup_location_string",
                    status_ty.fn_type(&[ptr.into(), ptr.into()], false),
                )?;
                let result_case = self.value_emitter().variant_glue(result_glue)?;
                let remote_ty = &result_case.variants[0].fields[0].ty;
                let remote_layout = self.module.target.layout(remote_ty).ok_or_else(|| {
                    CodegenError::FailClosed(
                        "Node::lookup RemotePid payload has no physical layout".into(),
                    )
                })?;
                let remote_out = self.value_emitter().entry_scratch(
                    llvm_type(self.ctx, &remote_layout.repr)?,
                    "node.lookup.location",
                )?;
                self.builder
                    .build_store(
                        remote_out,
                        llvm_type(self.ctx, &remote_layout.repr)?.const_zero(),
                    )
                    .llvm_ctx("initialize node lookup location output")?;
                let status = self
                    .runtime_call_value(
                        lookup,
                        &[
                            self.load(source(0)?, "node.lookup.name")?.into(),
                            remote_out.into(),
                        ],
                        "node.lookup",
                    )?
                    .into_int_value();
                let ok = self
                    .builder
                    .build_int_compare(
                        IntPredicate::EQ,
                        status,
                        status_ty.const_zero(),
                        "node.lookup.ok",
                    )
                    .llvm_ctx("test node lookup status")?;
                let success = self
                    .ctx
                    .append_basic_block(self.value, "node.lookup.success");
                let failure_block = self
                    .ctx
                    .append_basic_block(self.value, "node.lookup.failure");
                let complete = self
                    .ctx
                    .append_basic_block(self.value, "node.lookup.complete");
                self.builder
                    .build_conditional_branch(ok, success, failure_block)
                    .llvm_ctx("branch node lookup result")?;
                let destination = self.slots[required_result()?.0 as usize];
                self.builder.position_at_end(success);
                let remote = self
                    .builder
                    .build_load(
                        llvm_type(self.ctx, &remote_layout.repr)?,
                        remote_out,
                        "node.lookup.location.value",
                    )
                    .llvm_ctx("load resolved node location")?;
                self.write_variant_value(destination, 0, &[remote], result_glue)?;
                self.builder
                    .build_unconditional_branch(complete)
                    .llvm_ctx("finish node lookup success result")?;
                self.builder.position_at_end(failure_block);
                let error_layout = self
                    .value_emitter()
                    .variant_layout(&self.value_emitter().variant_glue(error_glue)?.ty)?;
                let error_storage = self.value_emitter().entry_scratch(
                    llvm_type(self.ctx, &error_layout.object.repr)?,
                    "node.lookup.error",
                )?;
                self.value_emitter()
                    .write_variant_value(error_storage, 0, &[], error_glue)?;
                let error_value = self
                    .builder
                    .build_load(
                        llvm_type(self.ctx, &error_layout.object.repr)?,
                        error_storage,
                        "node.lookup.error.value",
                    )
                    .llvm_ctx("load node lookup error value")?;
                self.write_variant_value(destination, 1, &[error_value], result_glue)?;
                self.builder
                    .build_unconditional_branch(complete)
                    .llvm_ctx("finish node lookup failure result")?;
                self.builder.position_at_end(complete);
            }
            family @ (RuntimeCallFamily::NodeStart | RuntimeCallFamily::NodeConnect) => {
                let PhysicalRuntimeCarrier::NodeResult {
                    result: result_glue,
                    error: error_glue,
                } = action.carrier
                else {
                    return Err(CodegenError::FailClosed(
                        "node lifecycle operation has no Result carrier".into(),
                    ));
                };
                let status_ty = self.ctx.i32_type();
                let function = match family {
                    hew_types::RuntimeCallFamily::NodeStart => get_or_declare_external(
                        self.llvm,
                        "hew_node_api_start_config",
                        status_ty.fn_type(&[ptr.into()], false),
                    )?,
                    hew_types::RuntimeCallFamily::NodeConnect => get_or_declare_external(
                        self.llvm,
                        "hew_node_api_connect_string",
                        status_ty.fn_type(&[ptr.into()], false),
                    )?,
                    hew_types::RuntimeCallFamily::NodeShutdown => get_or_declare_external(
                        self.llvm,
                        "hew_node_api_shutdown",
                        status_ty.fn_type(&[], false),
                    )?,
                    _ => {
                        return Err(CodegenError::FailClosed(
                            "node lifecycle action carries an invalid family".into(),
                        ))
                    }
                };
                let arguments = match family {
                    hew_types::RuntimeCallFamily::NodeStart => {
                        vec![self.slots[source(0)?.0 as usize].into()]
                    }
                    hew_types::RuntimeCallFamily::NodeConnect => {
                        vec![self.load(source(0)?, "node.connect.address")?.into()]
                    }
                    hew_types::RuntimeCallFamily::NodeShutdown => vec![],
                    _ => unreachable!("validated above"),
                };
                let status = self
                    .runtime_call_value(function, &arguments, "node.lifecycle")?
                    .into_int_value();
                let ok = self
                    .builder
                    .build_int_compare(IntPredicate::EQ, status, status_ty.const_zero(), "node.ok")
                    .llvm_ctx("test node lifecycle status")?;
                let success = self.ctx.append_basic_block(self.value, "node.success");
                let failure_block = self.ctx.append_basic_block(self.value, "node.failure");
                let complete = self.ctx.append_basic_block(self.value, "node.complete");
                self.builder
                    .build_conditional_branch(ok, success, failure_block)
                    .llvm_ctx("branch node lifecycle result")?;
                let destination = self.slots[required_result()?.0 as usize];
                let result_case = self.value_emitter().variant_glue(result_glue)?;
                let unit_ty = &result_case.variants[0].fields[0].ty;
                let unit_layout = self.module.target.layout(unit_ty).ok_or_else(|| {
                    CodegenError::FailClosed("node Result Ok payload has no physical layout".into())
                })?;
                self.builder.position_at_end(success);
                self.write_variant_value(
                    destination,
                    0,
                    &[llvm_type(self.ctx, &unit_layout.repr)?.const_zero()],
                    result_glue,
                )?;
                self.builder
                    .build_unconditional_branch(complete)
                    .llvm_ctx("finish node success result")?;
                self.builder.position_at_end(failure_block);
                let error_layout = self
                    .value_emitter()
                    .variant_layout(&self.value_emitter().variant_glue(error_glue)?.ty)?;
                let error_storage = self.value_emitter().entry_scratch(
                    llvm_type(self.ctx, &error_layout.object.repr)?,
                    "node.error",
                )?;
                self.value_emitter()
                    .write_variant_value(error_storage, 0, &[], error_glue)?;
                let error_value = self
                    .builder
                    .build_load(
                        llvm_type(self.ctx, &error_layout.object.repr)?,
                        error_storage,
                        "node.error.value",
                    )
                    .llvm_ctx("load node error value")?;
                self.write_variant_value(destination, 1, &[error_value], result_glue)?;
                self.builder
                    .build_unconditional_branch(complete)
                    .llvm_ctx("finish node failure result")?;
                self.builder.position_at_end(complete);
            }
            RuntimeCallFamily::BytesDecodeUtf8 => {
                let PhysicalRuntimeCarrier::Utf8Decode {
                    result: result_glue,
                    error,
                    error_len,
                } = action.carrier
                else {
                    return Err(CodegenError::FailClosed(
                        "UTF-8 decode has no result carrier".into(),
                    ));
                };
                self.emit_utf8_decode(
                    source(0)?,
                    required_result()?,
                    result_glue,
                    error,
                    error_len,
                )?;
            }
            RuntimeCallFamily::StringFind
            | RuntimeCallFamily::StringCharAt
            | RuntimeCallFamily::StringCharAtUtf8 => {
                let option = variant_carrier(action)?;
                let (symbol, function_type) = match action.family {
                    RuntimeCallFamily::StringCharAt => (
                        "hew_string_char_at",
                        self.ctx
                            .i32_type()
                            .fn_type(&[ptr.into(), self.ctx.i64_type().into()], false),
                    ),
                    RuntimeCallFamily::StringCharAtUtf8 => (
                        "hew_string_char_at_utf8",
                        self.ctx
                            .i32_type()
                            .fn_type(&[ptr.into(), self.ctx.i64_type().into()], false),
                    ),
                    _ => (
                        "hew_string_find",
                        self.ctx
                            .i64_type()
                            .fn_type(&[ptr.into(), ptr.into()], false),
                    ),
                };
                let function = get_or_declare_external(self.llvm, symbol, function_type)?;
                let index = self
                    .runtime_call_value(
                        function,
                        &[
                            self.load(source(0)?, "find.text")?.into(),
                            self.load(source(1)?, "find.needle")?.into(),
                        ],
                        "find.index",
                    )?
                    .into_int_value();
                let found = self
                    .builder
                    .build_int_compare(
                        IntPredicate::SGE,
                        index,
                        index.get_type().const_zero(),
                        "find.found",
                    )
                    .llvm_ctx("check string find sentinel")?;
                let present = self.ctx.append_basic_block(self.value, "find.present");
                let absent = self.ctx.append_basic_block(self.value, "find.absent");
                let complete = self.ctx.append_basic_block(self.value, "find.complete");
                let destination = self.slots[required_result()?.0 as usize];
                self.builder
                    .build_conditional_branch(found, present, absent)
                    .llvm_ctx("select string find result")?;
                self.builder.position_at_end(present);
                // `codepoint_at_utf8` declares an `Option<i64>` payload over an
                // `i32` scalar return, so the found codepoint widens first.
                let payload = if action.family == RuntimeCallFamily::StringCharAtUtf8 {
                    self.builder
                        .build_int_s_extend(index, self.ctx.i64_type(), "find.codepoint")
                        .llvm_ctx("widen codepoint to its declared payload")?
                } else {
                    index
                };
                self.write_variant_value(destination, 0, &[payload.into()], option)?;
                self.builder
                    .build_unconditional_branch(complete)
                    .llvm_ctx("finish string find hit")?;
                self.builder.position_at_end(absent);
                self.write_variant_value(destination, 1, &[], option)?;
                self.builder
                    .build_unconditional_branch(complete)
                    .llvm_ctx("finish string find miss")?;
                self.builder.position_at_end(complete);
            }
            RuntimeCallFamily::BytesPop => {
                let PhysicalRuntimeCarrier::PairWithOption { option, .. } = action.carrier else {
                    return Err(CodegenError::FailClosed(
                        "bytes pop has no pair carrier".into(),
                    ));
                };
                return self.emit_bytes_pop(source(0)?, required_result()?, option, normal);
            }
            RuntimeCallFamily::BytesGet => {
                let option = variant_carrier(action)?;
                return self.emit_bytes_get(
                    source(0)?,
                    source(1)?,
                    required_result()?,
                    option,
                    normal,
                );
            }
            RuntimeCallFamily::RegexHandle => {
                let handle = self.load_regex_handle(source(0)?)?;
                let function = external_unary_ptr(self.ctx, self.llvm, "hew_regex_clone")?;
                // The module slot outlives every pattern value built from it,
                // so the value owns an independent handle its scope exit frees.
                let owned = self.runtime_call_value(function, &[handle.into()], "regex.pattern")?;
                self.store(required_result()?, owned)?;
            }
            RuntimeCallFamily::RegexMatch => {
                let handle = self.load_regex_handle(source(0)?)?;
                let text = self.load(source(1)?, "regex.text")?;
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_regex_match",
                    self.ctx
                        .i32_type()
                        .fn_type(&[ptr.into(), ptr.into()], false),
                )?;
                let value = self
                    .runtime_call_value(function, &[handle.into(), text.into()], "regex.match")?
                    .into_int_value();
                let truth = self
                    .builder
                    .build_int_compare(
                        IntPredicate::NE,
                        value,
                        self.ctx.i32_type().const_zero(),
                        "regex.match.truth",
                    )
                    .llvm_ctx("normalize the regex match result")?;
                let dest = required_result()?;
                let bool_ty =
                    llvm_type(self.ctx, &self.storage(dest)?.layout.repr)?.into_int_type();
                let truth = self
                    .builder
                    .build_int_z_extend(truth, bool_ty, "regex.match.bool")
                    .llvm_ctx("widen the regex match result")?;
                self.store(dest, truth.into())?;
            }
            family if hew_types::runtime_call::declared_direct_runtime_method(family).is_some() => {
                self.emit_direct_runtime_call(family, transfers, result)?;
            }
            RuntimeCallFamily::StringToBytes => {
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_string_to_bytes_owned",
                    self.ctx
                        .void_type()
                        .fn_type(&[ptr.into(), ptr.into()], false),
                )?;
                self.runtime_call_void(
                    function,
                    &[
                        self.load(source(0)?, "string.to.bytes.input")?.into(),
                        self.slots[required_result()?.0 as usize].into(),
                    ],
                    "string.to.bytes",
                )?;
            }
            RuntimeCallFamily::ProcessExit => {
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_exit",
                    self.ctx
                        .void_type()
                        .fn_type(&[self.ctx.i64_type().into()], false),
                )?;
                self.runtime_call_void(
                    function,
                    &[self.load(source(0)?, "process.exit.code")?.into()],
                    "process.exit",
                )?;
            }
            RuntimeCallFamily::StderrWrite => {
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_io_write_err",
                    self.ctx.void_type().fn_type(&[ptr.into()], false),
                )?;
                self.runtime_call_void(
                    function,
                    &[self.load(source(0)?, "stderr.write.value")?.into()],
                    "stderr.write",
                )?;
            }
            RuntimeCallFamily::BytesNew => {
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_bytes_new",
                    ptr.fn_type(&[self.ctx.i32_type().into()], false),
                )?;
                let value = self.runtime_call_value(
                    function,
                    &[self.ctx.i32_type().const_zero().into()],
                    "bytes.new",
                )?;
                let result = required_result()?;
                // The allocator returns the data pointer, not the source bytes
                // triple. Initialize its offset and length before publishing it.
                let empty = llvm_type(self.ctx, &self.storage(result)?.layout.repr)?
                    .into_struct_type()
                    .const_zero();
                let bytes = self
                    .builder
                    .build_insert_value(empty, value, 0, "bytes.new.value")
                    .llvm_ctx("initialize empty bytes value")?
                    .into_struct_value();
                self.store(result, bytes.into())?;
            }
            RuntimeCallFamily::Print { kind, newline } => {
                use hew_types::runtime_call::PrintKind;
                let value = self.load(source(0)?, "print.value")?;
                // Tags and bit packing implement hew-runtime/src/print.rs's
                // stable C ABI. The source type was selected before MIR.
                let tag = match kind {
                    PrintKind::I32 => 0,
                    PrintKind::I64 => 1,
                    PrintKind::F64 => 2,
                    PrintKind::Bool => 3,
                    PrintKind::Str => 4,
                    PrintKind::U32 => 5,
                    PrintKind::U64 => 6,
                    PrintKind::U8 => 7,
                };
                let bits = match kind {
                    PrintKind::Str => self
                        .builder
                        .build_ptr_to_int(
                            value.into_pointer_value(),
                            self.ctx.i64_type(),
                            "print.string.bits",
                        )
                        .llvm_ctx("pack managed string print handle")?,
                    PrintKind::F64 => self
                        .builder
                        .build_bit_cast(value, self.ctx.i64_type(), "print.float.bits")
                        .llvm_ctx("pack floating-point print bits")?
                        .into_int_value(),
                    PrintKind::I32
                    | PrintKind::I64
                    | PrintKind::U8
                    | PrintKind::U32
                    | PrintKind::U64
                    | PrintKind::Bool => self
                        .builder
                        .build_int_cast_sign_flag(
                            value.into_int_value(),
                            self.ctx.i64_type(),
                            matches!(kind, PrintKind::I32 | PrintKind::I64),
                            "print.integer.bits",
                        )
                        .llvm_ctx("pack scalar print bits")?,
                };
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_print_value",
                    self.ctx.void_type().fn_type(
                        &[
                            self.ctx.i8_type().into(),
                            self.ctx.i64_type().into(),
                            self.ctx.bool_type().into(),
                        ],
                        false,
                    ),
                )?;
                self.runtime_call_void(
                    function,
                    &[
                        self.ctx.i8_type().const_int(tag, false).into(),
                        bits.into(),
                        self.ctx
                            .bool_type()
                            .const_int(u64::from(newline), false)
                            .into(),
                    ],
                    "print.value",
                )?;
            }
            RuntimeCallFamily::BytesClear => {
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_bytes_clear",
                    self.ctx.void_type().fn_type(&[ptr.into()], false),
                )?;
                self.runtime_call_void(
                    function,
                    &[self.slots[source(0)?.0 as usize].into()],
                    "bytes.clear",
                )?;
                let value = self.load(source(0)?, "bytes.clear.result")?;
                self.store(required_result()?, value)?;
            }
            RuntimeCallFamily::BytesSet => {
                return self.emit_bytes_set(
                    source(0)?,
                    source(1)?,
                    source(2)?,
                    required_result()?,
                    normal,
                    failure.ok_or_else(|| {
                        CodegenError::FailClosed(
                            "physical bytes set lacks its bounds failure edge".into(),
                        )
                    })?,
                );
            }
            RuntimeCallFamily::BytesIndex => {
                return self.emit_bytes_index(
                    source(0)?,
                    source(1)?,
                    required_result()?,
                    normal,
                    failure.ok_or_else(|| {
                        CodegenError::FailClosed(
                            "physical bytes index lacks its cleanup failure edge".into(),
                        )
                    })?,
                );
            }
            RuntimeCallFamily::BytesSlice => {
                return self.emit_bytes_slice(
                    source(0)?,
                    source(1)?,
                    Some(source(2)?),
                    required_result()?,
                    normal,
                    failure.ok_or_else(|| {
                        CodegenError::FailClosed(
                            "physical bytes slice lacks its cleanup failure edge".into(),
                        )
                    })?,
                );
            }
            RuntimeCallFamily::BytesSliceFrom => {
                return self.emit_bytes_slice(
                    source(0)?,
                    source(1)?,
                    None,
                    required_result()?,
                    normal,
                    failure.ok_or_else(|| {
                        CodegenError::FailClosed(
                            "physical bytes slice lacks its cleanup failure edge".into(),
                        )
                    })?,
                );
            }
            RuntimeCallFamily::BytesPush => {
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_bytes_push_owned",
                    self.ctx
                        .void_type()
                        .fn_type(&[ptr.into(), self.ctx.i8_type().into(), ptr.into()], false),
                )?;
                self.runtime_call_void(
                    function,
                    &[
                        self.slots[source(0)?.0 as usize].into(),
                        self.load(source(1)?, "bytes.push.byte")?.into(),
                        self.slots[required_result()?.0 as usize].into(),
                    ],
                    "bytes.push.owned",
                )?;
            }
            RuntimeCallFamily::BytesAppend => {
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_bytes_append_owned",
                    self.ctx
                        .void_type()
                        .fn_type(&[ptr.into(), ptr.into(), ptr.into()], false),
                )?;
                self.runtime_call_void(
                    function,
                    &[
                        self.slots[source(0)?.0 as usize].into(),
                        self.slots[source(1)?.0 as usize].into(),
                        self.slots[required_result()?.0 as usize].into(),
                    ],
                    "bytes.append.owned",
                )?;
            }
            RuntimeCallFamily::StringIndex => {
                return self.emit_string_index(
                    source(0)?,
                    source(1)?,
                    required_result()?,
                    normal,
                    failure.ok_or_else(|| {
                        CodegenError::FailClosed(
                            "physical string index lacks its cleanup failure edge".into(),
                        )
                    })?,
                );
            }
            RuntimeCallFamily::StringSliceCodepoints => {
                return self.emit_string_slice_codepoints(
                    source(0)?,
                    source(1)?,
                    source(2)?,
                    required_result()?,
                    normal,
                    failure.ok_or_else(|| {
                        CodegenError::FailClosed(
                            "physical string slice lacks its cleanup failure edge".into(),
                        )
                    })?,
                );
            }
            RuntimeCallFamily::StringSliceCodepointsFrom => {
                return self.emit_string_slice_codepoints_from(
                    source(0)?,
                    source(1)?,
                    required_result()?,
                    normal,
                    failure.ok_or_else(|| {
                        CodegenError::FailClosed(
                            "physical string slice lacks its cleanup failure edge".into(),
                        )
                    })?,
                );
            }
            // Every remaining operation is an ordinary declared call: the row
            // carries its symbol, its operand contract and its C return. An
            // operation whose result needs a physical carrier is handled above,
            // so reaching here with one is malformed IR.
            _ => {
                if action.carrier != PhysicalRuntimeCarrier::None {
                    return Err(CodegenError::FailClosed(format!(
                        "runtime operation `{:?}` reached the declared-call emitter with a carrier",
                        action.family
                    )));
                }
                self.emit_direct_runtime_call(action.family, transfers, result)?;
            }
        }
        if failure.is_some() {
            return Err(CodegenError::FailClosed(format!(
                "infallible physical runtime action {action:?} carries a failure edge"
            )));
        }
        self.emit_result_edge(result, normal)
    }

    fn emit_math_intrinsic(
        &self,
        kind: MathIntrinsic,
        transfers: &[ArgumentTransfer],
        result: StorageId,
        normal: &PhysicalEdge,
        failure: Option<&PhysicalEdge>,
    ) -> CodegenResult<()> {
        use MathIntrinsic as M;
        if kind == M::FromBits {
            if failure.is_some() {
                return Err(CodegenError::FailClosed(
                    "infallible math intrinsic carries a failure edge".into(),
                ));
            }
            let bits = transfers
                .first()
                .ok_or_else(|| {
                    CodegenError::FailClosed("physical `from_bits` lacks its operand".into())
                })
                .and_then(|transfer| self.load(argument_source(transfer), "math.argument"))?;
            let value = self
                .builder
                .build_bit_cast(bits, self.ctx.f64_type(), "math.from_bits")
                .llvm_ctx("reinterpret bits as f64")?;
            self.store(result, value)?;
            return self.emit_result_edge(Some(result), normal);
        }
        // Libm-only operations: LLVM has no core intrinsic for these on any
        // supported LLVM version, so codegen declares and calls the C symbol
        // directly. The executable already links libm transitively (the
        // trig/exp/log intrinsics above lower to libm calls themselves), so
        // this adds no new link dependency.
        let libm_symbol = match kind {
            M::Log1p => Some("log1p"),
            M::Expm1 => Some("expm1"),
            M::Cbrt => Some("cbrt"),
            M::Hypot => Some("hypot"),
            _ => None,
        };
        let mut arguments = transfers
            .iter()
            .map(|transfer| self.load(argument_source(transfer), "math.argument"))
            .collect::<CodegenResult<Vec<_>>>()?;
        let first = *arguments.first().ok_or_else(|| {
            CodegenError::FailClosed("physical math intrinsic lacks its operand".into())
        })?;
        let declaration = if let Some(symbol) = libm_symbol {
            let f64_ty = self.ctx.f64_type();
            let param_count = arguments.len();
            let params = vec![f64_ty.into(); param_count];
            get_or_declare_external(self.llvm, symbol, f64_ty.fn_type(&params, false))?
        } else {
            let name = match kind {
                M::Sqrt => "llvm.sqrt",
                M::Exp => "llvm.exp",
                M::Log => "llvm.log",
                M::Sin => "llvm.sin",
                M::Cos => "llvm.cos",
                M::AbsI64 => "llvm.abs",
                M::MinI64 => "llvm.smin",
                M::MaxI64 => "llvm.smax",
                M::AbsF64 => "llvm.fabs",
                M::MinF64 => "llvm.minnum",
                M::MaxF64 => "llvm.maxnum",
                M::Pow => "llvm.pow",
                M::Floor => "llvm.floor",
                M::Ceil => "llvm.ceil",
                M::Round => "llvm.round",
                M::Tan => "llvm.tan",
                M::Asin => "llvm.asin",
                M::Acos => "llvm.acos",
                M::Atan => "llvm.atan",
                M::Atan2 => "llvm.atan2",
                M::Sinh => "llvm.sinh",
                M::Cosh => "llvm.cosh",
                M::Tanh => "llvm.tanh",
                M::Exp2 => "llvm.exp2",
                M::Log2 => "llvm.log2",
                M::Log10 => "llvm.log10",
                M::Fma => "llvm.fma",
                M::Trunc => "llvm.trunc",
                M::Copysign => "llvm.copysign",
                M::Powi => "llvm.powi",
                M::Log1p | M::Expm1 | M::Cbrt | M::Hypot => {
                    unreachable!("libm-only kinds are handled by the `libm_symbol` branch above")
                }
                M::FromBits => {
                    unreachable!("FromBits returns from this function before reaching this match")
                }
            };
            // `powi` is parameterized over both the float type and the
            // integer exponent's type (`llvm.powi.f64.i32`); every other
            // intrinsic here overloads on the first operand's type alone.
            let overload_types: &[_] = if kind == M::Powi { &[0, 1] } else { &[0] };
            let types: Vec<_> = overload_types
                .iter()
                .map(|&i| arguments[i].get_type())
                .collect();
            Intrinsic::find(name)
                .and_then(|intrinsic| intrinsic.get_declaration(self.llvm, &types))
                .ok_or_else(|| {
                    CodegenError::FailClosed(format!("LLVM math intrinsic `{name}` is unavailable"))
                })?
        };
        if kind == M::AbsI64 {
            // Keep the minimum input defined while routing it to the checked
            // overflow edge, rather than creating poison before that branch.
            arguments.push(self.ctx.bool_type().const_zero().into());
        }
        let arguments: Vec<BasicMetadataValueEnum<'ctx>> =
            arguments.into_iter().map(Into::into).collect();
        let value = self.runtime_call_value(declaration, &arguments, "math.result")?;
        if kind == M::AbsI64 {
            let operand = first.into_int_value();
            let overflow = self
                .builder
                .build_int_compare(
                    IntPredicate::EQ,
                    operand,
                    operand.get_type().const_int(1 << 63, false),
                    "math.abs.overflow",
                )
                .llvm_ctx("check integer absolute value overflow")?;
            return self.emit_checked_choice(
                overflow,
                value.into_int_value(),
                result,
                normal,
                failure.ok_or_else(|| {
                    CodegenError::FailClosed(
                        "integer absolute value lacks its overflow edge".into(),
                    )
                })?,
                "math.abs",
            );
        }
        if failure.is_some() {
            return Err(CodegenError::FailClosed(
                "infallible math intrinsic carries a failure edge".into(),
            ));
        }
        self.store(result, value)?;
        self.emit_result_edge(Some(result), normal)
    }

    /// Integer bit-manipulation methods (`x.count_ones()`, …). Every op is a
    /// single LLVM intrinsic call parameterized on the receiver's width;
    /// `count_ones`/`count_zeros`/`leading_zeros`/`trailing_zeros` narrow
    /// their intrinsic result down to `u32` (LLVM's `ctpop`/`ctlz`/`cttz`
    /// return the receiver's own width; Hew's bit-count methods return `u32`
    /// at every receiver width, matching Rust).
    fn emit_int_method(
        &self,
        op: IntBitOp,
        width: IntMethodWidth,
        transfers: &[ArgumentTransfer],
        result: StorageId,
        normal: &PhysicalEdge,
        failure: Option<&PhysicalEdge>,
    ) -> CodegenResult<()> {
        if failure.is_some() {
            return Err(CodegenError::FailClosed(
                "infallible integer bit method carries a failure edge".into(),
            ));
        }
        let arguments = transfers
            .iter()
            .map(|transfer| self.load(argument_source(transfer), "int_method.argument"))
            .collect::<CodegenResult<Vec<_>>>()?;
        let receiver = arguments.first().copied().ok_or_else(|| {
            CodegenError::FailClosed("physical integer method lacks its receiver".into())
        })?;
        let receiver_int = receiver.into_int_value();
        let receiver_ty = receiver_int.get_type();
        let receiver_bits = receiver_ty.get_bit_width();
        let width_ok = match width {
            IntMethodWidth::I8 | IntMethodWidth::U8 => receiver_bits == 8,
            IntMethodWidth::I16 | IntMethodWidth::U16 => receiver_bits == 16,
            IntMethodWidth::I32 | IntMethodWidth::U32 => receiver_bits == 32,
            IntMethodWidth::I64 | IntMethodWidth::U64 => receiver_bits == 64,
            IntMethodWidth::Isize | IntMethodWidth::Usize => {
                let target = TargetData::create(&self.module.target.data_layout);
                receiver_bits == target.get_pointer_byte_size(None) * 8
            }
        };
        if !width_ok {
            return Err(CodegenError::FailClosed(format!(
                "integer method `{op:?}` carried width {width:?} but its receiver is {receiver_bits} bits"
            )));
        }
        let value = match op {
            IntBitOp::CountOnes => {
                let popcount = self.call_intrinsic1("llvm.ctpop", receiver_int)?;
                self.narrow_to_u32(popcount)?
            }
            IntBitOp::CountZeros => {
                let inverted = self
                    .builder
                    .build_not(receiver_int, "int_method.not")
                    .llvm_ctx("negate operand for count_zeros")?;
                let popcount = self.call_intrinsic1("llvm.ctpop", inverted)?;
                self.narrow_to_u32(popcount)?
            }
            IntBitOp::LeadingZeros => {
                let clz = self.call_intrinsic1_i1(
                    "llvm.ctlz",
                    receiver_int,
                    self.ctx.bool_type().const_zero(),
                )?;
                self.narrow_to_u32(clz)?
            }
            IntBitOp::TrailingZeros => {
                let ctz = self.call_intrinsic1_i1(
                    "llvm.cttz",
                    receiver_int,
                    self.ctx.bool_type().const_zero(),
                )?;
                self.narrow_to_u32(ctz)?
            }
            // A single byte has no other byte to swap with: LLVM's `bswap`
            // intrinsic requires an even byte count and rejects `i8`.
            // Rust's `u8`/`i8::swap_bytes` is the identity for the same
            // reason; match that instead of asking LLVM for an illegal
            // one-byte swap.
            IntBitOp::SwapBytes if receiver_bits == 8 => receiver_int.into(),
            IntBitOp::SwapBytes => self.call_intrinsic1("llvm.bswap", receiver_int)?.into(),
            IntBitOp::ReverseBits => self
                .call_intrinsic1("llvm.bitreverse", receiver_int)?
                .into(),
            IntBitOp::RotateLeft | IntBitOp::RotateRight => {
                let shift_arg = arguments.get(1).copied().ok_or_else(|| {
                    CodegenError::FailClosed("physical rotate lacks its shift amount".into())
                })?;
                // The shift amount is always `u32`; a narrower receiver
                // (`i8`/`i16`) needs it truncated, not widened, so
                // `build_int_cast` (trunc/zext/no-op, picked by comparing
                // the two widths) replaces the widen-only
                // `..._or_bit_cast` this used before narrow receivers
                // existed. Truncating is exactly "shift amount mod width".
                let shift = self
                    .builder
                    .build_int_cast(
                        shift_arg.into_int_value(),
                        receiver_ty,
                        "int_method.rotate_shift",
                    )
                    .llvm_ctx("cast rotate shift amount to the receiver's width")?;
                let name = match op {
                    IntBitOp::RotateLeft => "llvm.fshl",
                    IntBitOp::RotateRight => "llvm.fshr",
                    _ => unreachable!("matched rotate ops above"),
                };
                let declaration = Intrinsic::find(name)
                    .and_then(|intrinsic| {
                        intrinsic.get_declaration(self.llvm, &[receiver_ty.into()])
                    })
                    .ok_or_else(|| {
                        CodegenError::FailClosed(format!("LLVM intrinsic `{name}` is unavailable"))
                    })?;
                self.runtime_call_value(
                    declaration,
                    &[receiver_int.into(), receiver_int.into(), shift.into()],
                    "int_method.result",
                )?
            }
        };
        self.store(result, value)?;
        self.emit_result_edge(Some(result), normal)
    }

    /// Non-trapping integer arithmetic (`x.wrapping_add(y)`,
    /// `x.saturating_sub(y)`). Wrapping ops are a plain LLVM `add`/`sub`/
    /// `mul` (no `nsw`/`nuw`, so it silently wraps instead of the poison +
    /// checked-trap sequence the default `+`/`-`/`*` operators build);
    /// saturating ops call `llvm.{s,u}{add,sub}.sat`, chosen by the width's
    /// signedness.
    fn emit_int_arith(
        &self,
        kind: IntArithKind,
        width: IntMethodWidth,
        transfers: &[ArgumentTransfer],
        result: StorageId,
        normal: &PhysicalEdge,
        failure: Option<&PhysicalEdge>,
    ) -> CodegenResult<()> {
        if failure.is_some() {
            return Err(CodegenError::FailClosed(
                "infallible non-trapping integer arithmetic carries a failure edge".into(),
            ));
        }
        let arguments = transfers
            .iter()
            .map(|transfer| self.load(argument_source(transfer), "int_arith.argument"))
            .collect::<CodegenResult<Vec<_>>>()?;
        let (Some(lhs), Some(rhs)) = (arguments.first(), arguments.get(1)) else {
            return Err(CodegenError::FailClosed(
                "physical non-trapping integer arithmetic lacks an operand".into(),
            ));
        };
        let lhs = lhs.into_int_value();
        let rhs = rhs.into_int_value();
        let signed = is_signed_int_width(width);
        let value = match kind {
            IntArithKind::WrappingAdd => self
                .builder
                .build_int_add(lhs, rhs, "int_arith.wrapping_add")
                .llvm_ctx("emit wrapping add")?,
            IntArithKind::WrappingSub => self
                .builder
                .build_int_sub(lhs, rhs, "int_arith.wrapping_sub")
                .llvm_ctx("emit wrapping sub")?,
            IntArithKind::WrappingMul => self
                .builder
                .build_int_mul(lhs, rhs, "int_arith.wrapping_mul")
                .llvm_ctx("emit wrapping mul")?,
            IntArithKind::SaturatingAdd => {
                let name = if signed {
                    "llvm.sadd.sat"
                } else {
                    "llvm.uadd.sat"
                };
                self.call_intrinsic2(name, lhs, rhs)?
            }
            IntArithKind::SaturatingSub => {
                let name = if signed {
                    "llvm.ssub.sat"
                } else {
                    "llvm.usub.sat"
                };
                self.call_intrinsic2(name, lhs, rhs)?
            }
            IntArithKind::SaturatingMul => self.emit_saturating_mul(lhs, rhs, signed)?,
            IntArithKind::CheckedAdd | IntArithKind::CheckedSub | IntArithKind::CheckedMul => {
                return Err(CodegenError::FailClosed(
                    "checked integer arithmetic reached the non-trapping arith path; it \
                     needs the `VariantResult` carrier (see `emit_checked_int_arith`)"
                        .into(),
                ));
            }
        };
        self.store(result, value.into())?;
        self.emit_result_edge(Some(result), normal)
    }

    /// `x.saturating_mul(y)`: no LLVM saturating-multiply intrinsic exists,
    /// so this builds one from `llvm.{s,u}mul.with.overflow` plus a select
    /// onto the saturated bound. Unsigned overflow always saturates to the
    /// all-ones max. Signed overflow saturates to `MIN` when the operands'
    /// signs differ (the true product is negative) and to `MAX` when they
    /// match (the true product is positive).
    fn emit_saturating_mul(
        &self,
        lhs: inkwell::values::IntValue<'ctx>,
        rhs: inkwell::values::IntValue<'ctx>,
        signed: bool,
    ) -> CodegenResult<inkwell::values::IntValue<'ctx>> {
        let name = if signed {
            "llvm.smul.with.overflow"
        } else {
            "llvm.umul.with.overflow"
        };
        let outcome = self.call_intrinsic_with_overflow(name, lhs, rhs)?;
        let product = self
            .builder
            .build_extract_value(outcome, 0, "int_arith.saturating_mul.product")
            .llvm_ctx("extract the saturating-multiply product")?
            .into_int_value();
        let overflowed = self
            .builder
            .build_extract_value(outcome, 1, "int_arith.saturating_mul.overflow")
            .llvm_ctx("extract the saturating-multiply overflow flag")?
            .into_int_value();
        let ty = lhs.get_type();
        let bound = if signed {
            let bits = ty.get_bit_width();
            let max = ty.const_int((1u64 << (bits - 1)) - 1, false);
            let min = ty.const_int(1u64 << (bits - 1), false);
            let signs_differ = self
                .builder
                .build_int_compare(
                    IntPredicate::SLT,
                    self.builder
                        .build_xor(lhs, rhs, "int_arith.saturating_mul.sign_xor")
                        .llvm_ctx("compare operand signs for saturating multiply")?,
                    ty.const_zero(),
                    "int_arith.saturating_mul.signs_differ",
                )
                .llvm_ctx("compare operand signs for saturating multiply")?;
            self.builder
                .build_select(signs_differ, min, max, "int_arith.saturating_mul.bound")
                .llvm_ctx("select the saturating-multiply bound")?
                .into_int_value()
        } else {
            ty.const_all_ones()
        };
        Ok(self
            .builder
            .build_select(
                overflowed,
                bound,
                product,
                "int_arith.saturating_mul.result",
            )
            .llvm_ctx("select the saturating-multiply result")?
            .into_int_value())
    }

    /// `x.checked_add/sub/mul(y)`: the ordinary `Option<T>` `VariantResult`
    /// carrier, backed by `llvm.{s,u}{add,sub,mul}.with.overflow`. `None` on
    /// overflow, `Some(v)` otherwise — the same two-block shape as
    /// `emit_weak_upgrade`.
    fn emit_checked_int_arith(
        &self,
        (kind, width): (IntArithKind, IntMethodWidth),
        option: hew_mir::physical::PhysicalVariantId,
        transfers: &[ArgumentTransfer],
        result: StorageId,
        normal: &PhysicalEdge,
        failure: Option<&PhysicalEdge>,
    ) -> CodegenResult<()> {
        if failure.is_some() {
            return Err(CodegenError::FailClosed(
                "checked integer arithmetic carries a failure edge; overflow is `None`, not \
                 a failure"
                    .into(),
            ));
        }
        let arguments = transfers
            .iter()
            .map(|transfer| self.load(argument_source(transfer), "int_arith.argument"))
            .collect::<CodegenResult<Vec<_>>>()?;
        let (Some(lhs), Some(rhs)) = (arguments.first(), arguments.get(1)) else {
            return Err(CodegenError::FailClosed(
                "physical checked integer arithmetic lacks an operand".into(),
            ));
        };
        let lhs = lhs.into_int_value();
        let rhs = rhs.into_int_value();
        let signed = is_signed_int_width(width);
        let name = match (kind, signed) {
            (IntArithKind::CheckedAdd, true) => "llvm.sadd.with.overflow",
            (IntArithKind::CheckedAdd, false) => "llvm.uadd.with.overflow",
            (IntArithKind::CheckedSub, true) => "llvm.ssub.with.overflow",
            (IntArithKind::CheckedSub, false) => "llvm.usub.with.overflow",
            (IntArithKind::CheckedMul, true) => "llvm.smul.with.overflow",
            (IntArithKind::CheckedMul, false) => "llvm.umul.with.overflow",
            _ => {
                return Err(CodegenError::FailClosed(format!(
                    "`{kind:?}` is not checked integer arithmetic"
                )));
            }
        };
        let outcome = self.call_intrinsic_with_overflow(name, lhs, rhs)?;
        let sum = self
            .builder
            .build_extract_value(outcome, 0, "int_arith.checked.value")
            .llvm_ctx("extract the checked-arithmetic result")?;
        let overflowed = self
            .builder
            .build_extract_value(outcome, 1, "int_arith.checked.overflow")
            .llvm_ctx("extract the checked-arithmetic overflow flag")?
            .into_int_value();
        let some_block = self
            .ctx
            .append_basic_block(self.value, "int_arith.checked.some");
        let none_block = self
            .ctx
            .append_basic_block(self.value, "int_arith.checked.none");
        self.builder
            .build_conditional_branch(overflowed, none_block, some_block)
            .llvm_ctx("select the checked-arithmetic outcome")?;
        self.builder.position_at_end(none_block);
        self.write_variant_value(self.slots[result.0 as usize], 1, &[], option)?;
        self.emit_result_edge(Some(result), normal)?;
        self.builder.position_at_end(some_block);
        self.write_variant_value(self.slots[result.0 as usize], 0, &[sum], option)?;
        self.emit_result_edge(Some(result), normal)
    }

    /// Declare (if needed) and call a two-operand LLVM intrinsic overloaded
    /// on the operands' shared type, returning its `iN` result.
    fn call_intrinsic2(
        &self,
        name: &str,
        lhs: inkwell::values::IntValue<'ctx>,
        rhs: inkwell::values::IntValue<'ctx>,
    ) -> CodegenResult<inkwell::values::IntValue<'ctx>> {
        let declaration = Intrinsic::find(name)
            .and_then(|intrinsic| intrinsic.get_declaration(self.llvm, &[lhs.get_type().into()]))
            .ok_or_else(|| {
                CodegenError::FailClosed(format!("LLVM intrinsic `{name}` is unavailable"))
            })?;
        Ok(self
            .runtime_call_value(declaration, &[lhs.into(), rhs.into()], "int_arith.result")?
            .into_int_value())
    }

    /// Declare (if needed) and call a two-operand `llvm.{s,u}{add,sub,mul}
    /// .with.overflow` intrinsic, returning its `{ iN, i1 }` aggregate
    /// (the arithmetic result and the overflow flag) unpacked by the
    /// caller with `build_extract_value`.
    fn call_intrinsic_with_overflow(
        &self,
        name: &str,
        lhs: inkwell::values::IntValue<'ctx>,
        rhs: inkwell::values::IntValue<'ctx>,
    ) -> CodegenResult<inkwell::values::StructValue<'ctx>> {
        let declaration = Intrinsic::find(name)
            .and_then(|intrinsic| intrinsic.get_declaration(self.llvm, &[lhs.get_type().into()]))
            .ok_or_else(|| {
                CodegenError::FailClosed(format!("LLVM intrinsic `{name}` is unavailable"))
            })?;
        Ok(self
            .runtime_call_value(declaration, &[lhs.into(), rhs.into()], "int_arith.overflow")?
            .into_struct_value())
    }

    /// `f64` bit/classification methods (`x.to_bits()`, `x.is_nan()`, …).
    /// Every op here is an ordinary builder instruction (bitcast, fcmp
    /// against itself or against +-infinity, an integer sign test on the
    /// bitcast pattern) — none of these need a declared LLVM intrinsic.
    fn emit_float_method(
        &self,
        op: FloatMethodOp,
        transfers: &[ArgumentTransfer],
        result: StorageId,
        normal: &PhysicalEdge,
        failure: Option<&PhysicalEdge>,
    ) -> CodegenResult<()> {
        if failure.is_some() {
            return Err(CodegenError::FailClosed(
                "infallible float method carries a failure edge".into(),
            ));
        }
        let receiver = transfers
            .first()
            .ok_or_else(|| {
                CodegenError::FailClosed("physical float method lacks its receiver".into())
            })
            .and_then(|transfer| self.load(argument_source(transfer), "float_method.argument"))?
            .into_float_value();
        // Bool storage is `i8`, not LLVM's native `i1`; widen every predicate
        // result before storing it (matching e.g. `bytes.is_empty`'s
        // `build_int_z_extend` above).
        let bool_ty = llvm_type(self.ctx, &self.storage(result)?.layout.repr)?.into_int_type();
        let value: BasicValueEnum<'ctx> = match op {
            FloatMethodOp::ToBits => self
                .builder
                .build_bit_cast(receiver, self.ctx.i64_type(), "float_method.to_bits")
                .llvm_ctx("reinterpret f64 bits as u64")?,
            FloatMethodOp::IsNan => {
                let truth = self
                    .builder
                    .build_float_compare(
                        FloatPredicate::UNO,
                        receiver,
                        receiver,
                        "float_method.is_nan",
                    )
                    .llvm_ctx("test for NaN")?;
                self.builder
                    .build_int_z_extend(truth, bool_ty, "float_method.is_nan.bool")
                    .llvm_ctx("widen is_nan result")?
                    .into()
            }
            FloatMethodOp::IsInfinite => {
                let f64_ty = self.ctx.f64_type();
                let pos_inf = f64_ty.const_float(f64::INFINITY);
                let neg_inf = f64_ty.const_float(f64::NEG_INFINITY);
                let is_pos = self
                    .builder
                    .build_float_compare(
                        FloatPredicate::OEQ,
                        receiver,
                        pos_inf,
                        "float_method.is_pos_inf",
                    )
                    .llvm_ctx("test for positive infinity")?;
                let is_neg = self
                    .builder
                    .build_float_compare(
                        FloatPredicate::OEQ,
                        receiver,
                        neg_inf,
                        "float_method.is_neg_inf",
                    )
                    .llvm_ctx("test for negative infinity")?;
                let truth = self
                    .builder
                    .build_or(is_pos, is_neg, "float_method.is_infinite")
                    .llvm_ctx("combine infinity tests")?;
                self.builder
                    .build_int_z_extend(truth, bool_ty, "float_method.is_infinite.bool")
                    .llvm_ctx("widen is_infinite result")?
                    .into()
            }
            FloatMethodOp::IsFinite => {
                let f64_ty = self.ctx.f64_type();
                let pos_inf = f64_ty.const_float(f64::INFINITY);
                let neg_inf = f64_ty.const_float(f64::NEG_INFINITY);
                // Ordered (non-NaN) and not equal to either infinity.
                let ordered = self
                    .builder
                    .build_float_compare(
                        FloatPredicate::ORD,
                        receiver,
                        receiver,
                        "float_method.ordered",
                    )
                    .llvm_ctx("test for non-NaN")?;
                let not_pos_inf = self
                    .builder
                    .build_float_compare(
                        FloatPredicate::ONE,
                        receiver,
                        pos_inf,
                        "float_method.not_pos_inf",
                    )
                    .llvm_ctx("test against positive infinity")?;
                let not_neg_inf = self
                    .builder
                    .build_float_compare(
                        FloatPredicate::ONE,
                        receiver,
                        neg_inf,
                        "float_method.not_neg_inf",
                    )
                    .llvm_ctx("test against negative infinity")?;
                let both = self
                    .builder
                    .build_and(not_pos_inf, not_neg_inf, "float_method.not_infinite")
                    .llvm_ctx("combine infinity exclusions")?;
                let truth = self
                    .builder
                    .build_and(ordered, both, "float_method.is_finite")
                    .llvm_ctx("combine finiteness tests")?;
                self.builder
                    .build_int_z_extend(truth, bool_ty, "float_method.is_finite.bool")
                    .llvm_ctx("widen is_finite result")?
                    .into()
            }
            FloatMethodOp::IsSignNegative => {
                // The raw sign bit, not a value classification: a negative
                // NaN's sign bit is still set, matching Rust's semantics.
                let bits = self
                    .builder
                    .build_bit_cast(receiver, self.ctx.i64_type(), "float_method.sign_bits")
                    .llvm_ctx("reinterpret f64 bits as i64 for the sign test")?
                    .into_int_value();
                let truth = self
                    .builder
                    .build_int_compare(
                        IntPredicate::SLT,
                        bits,
                        self.ctx.i64_type().const_zero(),
                        "float_method.is_sign_negative",
                    )
                    .llvm_ctx("test the sign bit")?;
                self.builder
                    .build_int_z_extend(truth, bool_ty, "float_method.is_sign_negative.bool")
                    .llvm_ctx("widen is_sign_negative result")?
                    .into()
            }
        };
        self.store(result, value)?;
        self.emit_result_edge(Some(result), normal)
    }

    /// Declare (if needed) and call a single-operand LLVM intrinsic
    /// overloaded on `operand`'s type, returning its `iN` result.
    fn call_intrinsic1(
        &self,
        name: &str,
        operand: inkwell::values::IntValue<'ctx>,
    ) -> CodegenResult<inkwell::values::IntValue<'ctx>> {
        let declaration = Intrinsic::find(name)
            .and_then(|intrinsic| {
                intrinsic.get_declaration(self.llvm, &[operand.get_type().into()])
            })
            .ok_or_else(|| {
                CodegenError::FailClosed(format!("LLVM intrinsic `{name}` is unavailable"))
            })?;
        Ok(self
            .runtime_call_value(declaration, &[operand.into()], "int_method.result")?
            .into_int_value())
    }

    /// Like [`Self::call_intrinsic1`] but for `ctlz`/`cttz`, which take a
    /// second non-overloaded `i1` operand.
    fn call_intrinsic1_i1(
        &self,
        name: &str,
        operand: inkwell::values::IntValue<'ctx>,
        flag: inkwell::values::IntValue<'ctx>,
    ) -> CodegenResult<inkwell::values::IntValue<'ctx>> {
        let declaration = Intrinsic::find(name)
            .and_then(|intrinsic| {
                intrinsic.get_declaration(self.llvm, &[operand.get_type().into()])
            })
            .ok_or_else(|| {
                CodegenError::FailClosed(format!("LLVM intrinsic `{name}` is unavailable"))
            })?;
        Ok(self
            .runtime_call_value(
                declaration,
                &[operand.into(), flag.into()],
                "int_method.result",
            )?
            .into_int_value())
    }

    /// Cast an intrinsic result (the receiver's own width) to the `u32`
    /// that every `count_ones`/`count_zeros`/`leading_zeros`/
    /// `trailing_zeros` method returns, regardless of receiver width:
    /// truncated down from `i64`/pointer width, widened up from `i8`/`i16`.
    fn narrow_to_u32(
        &self,
        value: inkwell::values::IntValue<'ctx>,
    ) -> CodegenResult<inkwell::values::BasicValueEnum<'ctx>> {
        if value.get_type().get_bit_width() == 32 {
            return Ok(value.into());
        }
        Ok(self
            .builder
            .build_int_cast(value, self.ctx.i32_type(), "int_method.narrow")
            .llvm_ctx("cast bit-count result to u32")?
            .into())
    }
}
