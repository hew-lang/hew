use super::*;
use inkwell::values::{AsValueRef, BasicValue, InstructionOpcode, InstructionValue};

pub(super) fn materialize(ctx: &Context, module: &Module<'_>) -> CodegenResult<()> {
    let kind = ctx.get_kind_id("hew.carry");
    let mut carries = Vec::new();
    for function in module.get_functions() {
        for block in function.get_basic_blocks() {
            carries.extend(block.get_instructions().filter(|instruction| {
                instruction.get_opcode() == InstructionOpcode::Store
                    && instruction.get_metadata(kind).is_some()
            }));
        }
    }
    for store in carries {
        let value = store
            .get_operand(0)
            .and_then(|operand| operand.value())
            .ok_or_else(|| CodegenError::FailClosed("suspension carrier lacks its value".into()))?;
        let slot = store
            .get_operand(1)
            .and_then(|operand| operand.value())
            .ok_or_else(|| CodegenError::FailClosed("suspension carrier lacks its storage".into()))?
            .into_pointer_value();
        let definition = value.as_instruction_value().ok_or_else(|| {
            CodegenError::FailClosed("suspension carrier is not an emitter instruction".into())
        })?;
        let mut users = Vec::new();
        let mut cursor = definition.get_first_use();
        while let Some(usage) = cursor {
            // SAFETY: the use owns a live value; LLVM distinguishes instructions
            // independently of their result type.
            let instruction = unsafe {
                inkwell::llvm_sys::core::LLVMIsAInstruction(usage.get_user().as_value_ref())
            };
            if !instruction.is_null() {
                // SAFETY: LLVM confirmed this live user is an instruction.
                let user = unsafe { InstructionValue::new(instruction) };
                if user != store {
                    users.push(user);
                }
            }
            cursor = usage.get_next_use();
        }
        users.sort_unstable_by_key(|user| user.as_value_ref() as usize);
        users.dedup();
        let builder = ctx.create_builder();
        let mut next = definition.get_next_instruction();
        while next.is_some_and(|instruction| instruction.get_opcode() == InstructionOpcode::Phi) {
            next = next.and_then(InstructionValue::get_next_instruction);
        }
        let next = next.ok_or_else(|| {
            CodegenError::FailClosed("suspension carrier has no insertion point".into())
        })?;
        if next != store {
            store.remove_from_basic_block();
            builder.position_before(&next);
            builder.insert_instruction(&store, None);
        }
        for user in users {
            for index in 0..user.get_num_operands() {
                if user.get_operand(index).and_then(|operand| operand.value()) != Some(value) {
                    continue;
                }
                if user.get_opcode() == InstructionOpcode::Phi {
                    // SAFETY: the instruction is a phi; each value operand has
                    // the corresponding incoming predecessor at this index.
                    let block = unsafe {
                        inkwell::basic_block::BasicBlock::new(
                            inkwell::llvm_sys::core::LLVMGetIncomingBlock(
                                user.as_value_ref(),
                                index,
                            ),
                        )
                    }
                    .ok_or_else(|| {
                        CodegenError::FailClosed("carrier phi lacks predecessor".into())
                    })?;
                    let end = block.get_terminator().ok_or_else(|| {
                        CodegenError::FailClosed("carrier predecessor lacks terminator".into())
                    })?;
                    builder.position_before(&end);
                } else {
                    builder.position_before(&user);
                }
                let loaded = builder
                    .build_load(value.get_type(), slot, "carry.reload")
                    .llvm_ctx("reload emitter suspension carrier")?;
                if !user.set_operand(index, loaded) {
                    return Err(CodegenError::FailClosed(
                        "cannot replace suspension carrier operand".into(),
                    ));
                }
            }
        }
        // SAFETY: the carrier store is live; the marker has been consumed.
        unsafe {
            inkwell::llvm_sys::core::LLVMSetMetadata(
                store.as_value_ref(),
                kind,
                std::ptr::null_mut(),
            );
        }
    }
    Ok(())
}
