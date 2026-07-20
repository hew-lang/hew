use super::*;

pub(super) fn emit_identity_aggregate_call<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    callee: &str,
    args: &[Place],
    dest: Option<&Place>,
    next: u32,
) -> CodegenResult<()> {
    let dest = dest.ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "identity aggregate call `{callee}` must carry a Terminator::Call dest"
        ))
    })?;

    if callee == "Node::id" {
        if !args.is_empty() {
            return Err(CodegenError::FailClosed(format!(
                "Node::id expects no arguments, got {}",
                args.len()
            )));
        }
        let Place::Local(dest_local) = *dest else {
            return Err(CodegenError::FailClosed(format!(
                "Node::id dest must be an Option<NodeId> local, got {dest:?}"
            )));
        };
        let (tag_ptr, tag_ty) = place_pointer(fn_ctx, Place::EnumTag(dest_local))?;
        let BasicTypeEnum::IntType(tag_int) = tag_ty else {
            return Err(CodegenError::FailClosed(format!(
                "Node::id Option tag must be integer-shaped, got {tag_ty:?}"
            )));
        };
        let (payload_ptr, payload_ty) = place_pointer(
            fn_ctx,
            Place::EnumVariant {
                local: dest_local,
                variant_idx: 0,
                field_idx: 0,
            },
        )?;
        let BasicTypeEnum::StructType(node_id_ty) = payload_ty else {
            return Err(CodegenError::FailClosed(format!(
                "Node::id Some payload must be the NodeId aggregate, got {payload_ty:?}"
            )));
        };
        if node_id_ty.count_fields() != 2 {
            return Err(CodegenError::FailClosed(format!(
                "Node::id Some payload must have two u64 fields, got {}",
                node_id_ty.count_fields()
            )));
        }
        let id_fn = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_node_api_id",
        )?;
        let rc = fn_ctx
            .builder
            .build_call(id_fn, &[payload_ptr.into()], "node_id_rc")
            .llvm_ctx("hew_node_api_id call")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("hew_node_api_id returned void".into()))?
            .into_int_value();
        let is_some = fn_ctx
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                rc,
                rc.get_type().const_zero(),
                "node_id_is_some",
            )
            .llvm_ctx("Node::id status compare")?;
        let tag = fn_ctx
            .builder
            .build_select(
                is_some,
                tag_int.const_zero(),
                tag_int.const_int(1, false),
                "node_id_option_tag",
            )
            .llvm_ctx("Node::id tag select")?;
        fn_ctx
            .builder
            .build_store(tag_ptr, tag)
            .llvm_ctx("Node::id tag store")?;
    } else {
        let [source] = args else {
            return Err(CodegenError::FailClosed(format!(
                "identity aggregate call `{callee}` expects one argument, got {}",
                args.len()
            )));
        };
        let (source_ptr, source_ty) = place_pointer(fn_ctx, *source)?;
        let BasicTypeEnum::StructType(source_struct) = source_ty else {
            return Err(CodegenError::FailClosed(format!(
                "identity aggregate call `{callee}` requires an aggregate operand, got {source_ty:?}"
            )));
        };

        match callee {
            "hew_remote_pid_location" => {
                let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest)?;
                let BasicTypeEnum::StructType(dest_struct) = dest_ty else {
                    return Err(CodegenError::FailClosed(format!(
                        "{callee} dest must be a Location aggregate, got {dest_ty:?}"
                    )));
                };
                if source_struct.count_fields() != 5 || dest_struct.count_fields() != 5 {
                    return Err(CodegenError::FailClosed(format!(
                        "{callee} requires five-field location aggregates"
                    )));
                }
                fn_ctx
                    .builder
                    .build_memcpy(
                        dest_ptr,
                        8,
                        source_ptr,
                        8,
                        fn_ctx.ctx.i64_type().const_int(32, false),
                    )
                    .llvm_ctx("RemotePid::location memcpy")?;
            }
            "hew_location_node_id" | "hew_remote_pid_node_id" => {
                let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest)?;
                let BasicTypeEnum::StructType(dest_struct) = dest_ty else {
                    return Err(CodegenError::FailClosed(format!(
                        "{callee} dest must be a NodeId aggregate, got {dest_ty:?}"
                    )));
                };
                if source_struct.count_fields() != 5 || dest_struct.count_fields() != 2 {
                    return Err(CodegenError::FailClosed(format!(
                        "{callee} requires a five-field location source and two-field NodeId dest"
                    )));
                }
                fn_ctx
                    .builder
                    .build_memcpy(
                        dest_ptr,
                        8,
                        source_ptr,
                        8,
                        fn_ctx.ctx.i64_type().const_int(16, false),
                    )
                    .llvm_ctx("identity NodeId prefix memcpy")?;
            }
            "hew_location_slot" | "hew_remote_pid_slot" => {
                let field_ptr = fn_ctx
                    .builder
                    .build_struct_gep(source_struct, source_ptr, 2, "identity_slot_ptr")
                    .llvm_ctx("identity slot gep")?;
                let field_ty = source_struct.get_field_type_at_index(2).ok_or_else(|| {
                    CodegenError::FailClosed(format!("{callee} source has no slot field"))
                })?;
                let value = fn_ctx
                    .builder
                    .build_load(field_ty, field_ptr, "identity_slot")
                    .llvm_ctx("identity slot load")?;
                let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest)?;
                if dest_ty != field_ty {
                    return Err(CodegenError::FailClosed(format!(
                        "{callee} slot dest type {dest_ty:?} does not match field type {field_ty:?}"
                    )));
                }
                fn_ctx
                    .builder
                    .build_store(dest_ptr, value)
                    .llvm_ctx("identity slot store")?;
            }
            "hew_location_incarnation" | "hew_remote_pid_incarnation" => {
                let field_ptr = fn_ctx
                    .builder
                    .build_struct_gep(source_struct, source_ptr, 3, "identity_incarnation_ptr")
                    .llvm_ctx("identity incarnation gep")?;
                let field_ty = source_struct.get_field_type_at_index(3).ok_or_else(|| {
                    CodegenError::FailClosed(format!("{callee} source has no incarnation field"))
                })?;
                let value = fn_ctx
                    .builder
                    .build_load(field_ty, field_ptr, "identity_incarnation")
                    .llvm_ctx("identity incarnation load")?;
                let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest)?;
                if dest_ty != field_ty {
                    return Err(CodegenError::FailClosed(format!(
                        "{callee} incarnation dest type {dest_ty:?} does not match field type {field_ty:?}"
                    )));
                }
                fn_ctx
                    .builder
                    .build_store(dest_ptr, value)
                    .llvm_ctx("identity incarnation store")?;
            }
            "hew_node_id_display" | "hew_location_display" | "hew_remote_pid_display" => {
                let runtime_symbol = if callee == "hew_node_id_display" {
                    if source_struct.count_fields() != 2 {
                        return Err(CodegenError::FailClosed(
                            "NodeId display requires a two-field NodeId aggregate".into(),
                        ));
                    }
                    "hew_node_id_format"
                } else {
                    if source_struct.count_fields() != 5 {
                        return Err(CodegenError::FailClosed(format!(
                            "{callee} requires a five-field location aggregate"
                        )));
                    }
                    "hew_location_format"
                };
                let format_fn = intern_runtime_decl(
                    fn_ctx.ctx,
                    fn_ctx.llvm_mod,
                    &mut fn_ctx.runtime_decls.borrow_mut(),
                    runtime_symbol,
                )?;
                let formatted = fn_ctx
                    .builder
                    .build_call(format_fn, &[source_ptr.into()], "identity_display")
                    .llvm_ctx_with(|| format!("{runtime_symbol} call"))?
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| {
                        CodegenError::FailClosed(format!("{runtime_symbol} returned void"))
                    })?;
                let (dest_ptr, dest_ty) = place_pointer(fn_ctx, *dest)?;
                if formatted.get_type() != dest_ty {
                    return Err(CodegenError::FailClosed(format!(
                        "{callee} string dest type {dest_ty:?} does not match runtime return {:?}",
                        formatted.get_type()
                    )));
                }
                fn_ctx
                    .builder
                    .build_store(dest_ptr, formatted)
                    .llvm_ctx("identity display store")?;
            }
            _ => {
                return Err(CodegenError::FailClosed(format!(
                    "unknown identity aggregate compiler call `{callee}`"
                )));
            }
        }
    }

    let next_bb = *fn_ctx.blocks.get(&next).ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "identity aggregate call `{callee}` next bb{next} missing"
        ))
    })?;
    fn_ctx
        .builder
        .build_unconditional_branch(next_bb)
        .llvm_ctx("identity aggregate call br next")?;
    Ok(())
}
