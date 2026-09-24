//! Free helpers for integer and float arithmetic, entry results and externals.

use super::*;

pub(super) fn failure_edge(
    failures: &[PhysicalCheckedFailure],
    kind: TrapKind,
) -> CodegenResult<&PhysicalEdge> {
    failures
        .iter()
        .find(|failure| failure.kind == kind)
        .map(|failure| &failure.edge)
        .ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "checked physical terminator is missing its {kind:?} edge"
            ))
        })
}

pub(super) fn callable(
    module: &PhysicalModule,
    id: CallableId,
) -> CodegenResult<&PhysicalCallable> {
    module
        .callables
        .get(id.0 as usize)
        .filter(|callable| callable.id == id)
        .ok_or_else(|| CodegenError::FailClosed(format!("unknown physical callable {}", id.0)))
}

pub(super) fn emitted_symbol(module: &PhysicalModule, callable: &PhysicalCallable) -> String {
    if module.entry_callable == Some(callable.id) {
        entry_body_symbol_for_triple(&module.target.triple).to_string()
    } else {
        callable.symbol.clone()
    }
}

pub(super) fn is_signed(ty: &ResolvedTy) -> bool {
    matches!(
        ty,
        ResolvedTy::I8
            | ResolvedTy::I16
            | ResolvedTy::I32
            | ResolvedTy::I64
            | ResolvedTy::Isize
            | ResolvedTy::Duration
    )
}

pub(super) fn is_signed_int_width(width: IntMethodWidth) -> bool {
    matches!(
        width,
        IntMethodWidth::I8
            | IntMethodWidth::I16
            | IntMethodWidth::I32
            | IntMethodWidth::I64
            | IntMethodWidth::Isize
    )
}

pub(super) fn emit_integer_binary<'ctx>(
    builder: &Builder<'ctx>,
    op: BinaryOp,
    lhs: IntValue<'ctx>,
    rhs: IntValue<'ctx>,
    signed: bool,
) -> CodegenResult<IntValue<'ctx>> {
    let compare = |signed_predicate, unsigned_predicate, name| {
        builder
            .build_int_compare(
                if signed {
                    signed_predicate
                } else {
                    unsigned_predicate
                },
                lhs,
                rhs,
                name,
            )
            .llvm_ctx("emit physical integer comparison")
    };
    match op {
        BinaryOp::Equal => builder
            .build_int_compare(IntPredicate::EQ, lhs, rhs, "eq")
            .llvm_ctx("emit integer equality"),
        BinaryOp::NotEqual => builder
            .build_int_compare(IntPredicate::NE, lhs, rhs, "ne")
            .llvm_ctx("emit integer inequality"),
        BinaryOp::Less => compare(IntPredicate::SLT, IntPredicate::ULT, "lt"),
        BinaryOp::LessEqual => compare(IntPredicate::SLE, IntPredicate::ULE, "le"),
        BinaryOp::Greater => compare(IntPredicate::SGT, IntPredicate::UGT, "gt"),
        BinaryOp::GreaterEqual => compare(IntPredicate::SGE, IntPredicate::UGE, "ge"),
        BinaryOp::And | BinaryOp::BitAnd => builder
            .build_and(lhs, rhs, "and")
            .llvm_ctx("emit physical and"),
        BinaryOp::Or | BinaryOp::BitOr => builder
            .build_or(lhs, rhs, "or")
            .llvm_ctx("emit physical or"),
        BinaryOp::BitXor => builder
            .build_xor(lhs, rhs, "xor")
            .llvm_ctx("emit physical xor"),
        BinaryOp::WrappingAdd => builder
            .build_int_add(lhs, rhs, "wrapping.add")
            .llvm_ctx("emit wrapping add"),
        BinaryOp::WrappingSub => builder
            .build_int_sub(lhs, rhs, "wrapping.sub")
            .llvm_ctx("emit wrapping subtract"),
        BinaryOp::WrappingMul => builder
            .build_int_mul(lhs, rhs, "wrapping.mul")
            .llvm_ctx("emit wrapping multiply"),
        BinaryOp::Add
        | BinaryOp::Subtract
        | BinaryOp::Multiply
        | BinaryOp::Divide
        | BinaryOp::Modulo
        | BinaryOp::Shl
        | BinaryOp::Shr
        | BinaryOp::Range
        | BinaryOp::RangeInclusive => Err(CodegenError::FailClosed(
            "fallible or range binary operation reached physical emitter".into(),
        )),
    }
}

pub(super) fn emit_float_binary<'ctx>(
    builder: &Builder<'ctx>,
    op: BinaryOp,
    lhs: inkwell::values::FloatValue<'ctx>,
    rhs: inkwell::values::FloatValue<'ctx>,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    let arithmetic = match op {
        BinaryOp::Add => Some(builder.build_float_add(lhs, rhs, "float.add")),
        BinaryOp::Subtract => Some(builder.build_float_sub(lhs, rhs, "float.sub")),
        BinaryOp::Multiply => Some(builder.build_float_mul(lhs, rhs, "float.mul")),
        BinaryOp::Divide => Some(builder.build_float_div(lhs, rhs, "float.div")),
        BinaryOp::Modulo => Some(builder.build_float_rem(lhs, rhs, "float.rem")),
        _ => None,
    };
    if let Some(value) = arithmetic {
        return value
            .llvm_ctx("emit IEEE floating arithmetic")
            .map(Into::into);
    }
    let predicate = match op {
        BinaryOp::Equal => FloatPredicate::OEQ,
        BinaryOp::NotEqual => FloatPredicate::UNE,
        BinaryOp::Less => FloatPredicate::OLT,
        BinaryOp::LessEqual => FloatPredicate::OLE,
        BinaryOp::Greater => FloatPredicate::OGT,
        BinaryOp::GreaterEqual => FloatPredicate::OGE,
        _ => {
            return Err(CodegenError::FailClosed(
                "unsupported float operation reached physical emitter".into(),
            ));
        }
    };
    builder
        .build_float_compare(predicate, lhs, rhs, "float.compare")
        .llvm_ctx("emit physical float comparison")
        .map(Into::into)
}

pub(super) fn emit_entry_success<'ctx>(
    ctx: &'ctx Context,
    builder: &Builder<'ctx>,
    result: Option<PointerValue<'ctx>>,
    action: EntryExitAction,
    callable: &PhysicalCallable,
) -> CodegenResult<IntValue<'ctx>> {
    match action {
        EntryExitAction::Unit => Ok(ctx.i32_type().const_zero()),
        EntryExitAction::Integer(kind) => {
            let slot = result.ok_or_else(|| {
                CodegenError::FailClosed("integer entry has no result-out storage".into())
            })?;
            let layout = callable.return_layout.as_ref().ok_or_else(|| {
                CodegenError::FailClosed("integer entry has no return layout".into())
            })?;
            let value = builder
                .build_load(llvm_type(ctx, &layout.repr)?, slot, "entry.result.value")
                .llvm_ctx("load physical entry result")?
                .into_int_value();
            normalize_entry_integer(ctx, builder, value, kind)
        }
        EntryExitAction::Result { .. } => Err(CodegenError::FailClosed(
            "Result process exits are realized by the SIR entry adapter".into(),
        )),
    }
}

fn normalize_entry_integer<'ctx>(
    ctx: &'ctx Context,
    builder: &Builder<'ctx>,
    value: IntValue<'ctx>,
    kind: EntryIntegerType,
) -> CodegenResult<IntValue<'ctx>> {
    match value.get_type().get_bit_width().cmp(&32) {
        std::cmp::Ordering::Greater => builder
            .build_int_truncate(value, ctx.i32_type(), "entry.truncate")
            .llvm_ctx("truncate physical entry status"),
        std::cmp::Ordering::Less if entry_integer_is_signed(kind) => builder
            .build_int_s_extend(value, ctx.i32_type(), "entry.sign.extend")
            .llvm_ctx("sign-extend physical entry status"),
        std::cmp::Ordering::Less => builder
            .build_int_z_extend(value, ctx.i32_type(), "entry.zero.extend")
            .llvm_ctx("zero-extend physical entry status"),
        std::cmp::Ordering::Equal => Ok(value),
    }
}

fn entry_integer_is_signed(kind: EntryIntegerType) -> bool {
    matches!(
        kind,
        EntryIntegerType::I8
            | EntryIntegerType::I16
            | EntryIntegerType::I32
            | EntryIntegerType::I64
            | EntryIntegerType::Isize
    )
}

pub(super) const fn argument_source(transfer: &ArgumentTransfer) -> StorageId {
    match transfer {
        ArgumentTransfer::Borrow(source)
        | ArgumentTransfer::BorrowMut(source)
        | ArgumentTransfer::Move(source) => *source,
        ArgumentTransfer::Clone { source, .. } => *source,
    }
}

pub(super) fn trap_code(kind: TrapKind) -> i32 {
    match kind {
        TrapKind::IntegerOverflow => HEW_TRAP_INTEGER_OVERFLOW,
        TrapKind::DivideByZero => HEW_TRAP_DIVIDE_BY_ZERO,
        TrapKind::SignedMinDivNegOne => HEW_TRAP_SIGNED_MIN_DIV_NEG_ONE,
        TrapKind::ShiftOutOfRange => HEW_TRAP_SHIFT_OUT_OF_RANGE,
        TrapKind::IndexOutOfBounds => HEW_TRAP_INDEX_OUT_OF_BOUNDS,
    }
}

pub(super) fn external_unary_ptr<'ctx>(
    ctx: &'ctx Context,
    module: &Module<'ctx>,
    symbol: &str,
) -> CodegenResult<FunctionValue<'ctx>> {
    let ptr = ctx.ptr_type(AddressSpace::default());
    get_or_declare_external(module, symbol, ptr.fn_type(&[ptr.into()], false))
}

/// Give one piece of release glue a fault record of its own.
///
/// Glue the runtime calls directly - an actor's terminal state release, a
/// callable environment drop - has no frame behind it, so a failing `close`
/// would otherwise leave through the trap path and strand whatever the glue
/// still owns. The record collects the failure, the glue finishes releasing,
/// and [`raise_glue_fault_record`] lets the fault leave at the end (D516).
pub(super) fn glue_fault_record<'ctx>(
    ctx: &'ctx Context,
    builder: &Builder<'ctx>,
) -> CodegenResult<(PointerValue<'ctx>, PointerValue<'ctx>)> {
    let pointer = ctx.ptr_type(AddressSpace::default());
    let fault = builder
        .build_alloca(pointer, "glue.fault")
        .llvm_ctx("allocate glue fault owner")?;
    let status = builder
        .build_alloca(ctx.i32_type(), "glue.fault.status")
        .llvm_ctx("allocate glue fault status")?;
    builder
        .build_store(fault, pointer.const_null())
        .llvm_ctx("clear glue fault owner")?;
    builder
        .build_store(status, ctx.i32_type().const_zero())
        .llvm_ctx("clear glue fault status")?;
    Ok((fault, status))
}

/// Let a fault this glue collected leave, once it owns nothing further.
///
/// The trap path records it against the release in progress when one is armed,
/// and otherwise reports it and crashes the actor or ends the run.
pub(super) fn raise_glue_fault_record<'ctx>(
    ctx: &'ctx Context,
    llvm: &Module<'ctx>,
    builder: &Builder<'ctx>,
    function: FunctionValue<'ctx>,
    record: (PointerValue<'ctx>, PointerValue<'ctx>),
) -> CodegenResult<()> {
    let (fault, status) = record;
    let pointer = ctx.ptr_type(AddressSpace::default());
    let raised = builder
        .build_load(pointer, fault, "glue.fault.raised")
        .llvm_ctx("load the glue's collected fault")?
        .into_pointer_value();
    let code = builder
        .build_load(ctx.i32_type(), status, "glue.fault.code")
        .llvm_ctx("load the glue's collected status")?;
    let present = builder
        .build_is_not_null(raised, "glue.fault.present")
        .llvm_ctx("test the glue's collected fault")?;
    let raise = ctx.append_basic_block(function, "glue.fault.raise");
    let done = ctx.append_basic_block(function, "glue.fault.done");
    builder
        .build_conditional_branch(present, raise, done)
        .llvm_ctx("branch on the glue's collected fault")?;
    builder.position_at_end(raise);
    let trap = get_or_declare_external(
        llvm,
        "hew_fault_trap",
        ctx.void_type()
            .fn_type(&[ctx.i32_type().into(), pointer.into()], false),
    )?;
    builder
        .build_call(trap, &[code.into(), raised.into()], "")
        .llvm_ctx("raise the glue's collected fault")?;
    builder
        .build_unconditional_branch(done)
        .llvm_ctx("finish raising the glue's fault")?;
    builder.position_at_end(done);
    Ok(())
}

pub(super) fn external_drop<'ctx>(
    ctx: &'ctx Context,
    module: &Module<'ctx>,
    symbol: &str,
) -> CodegenResult<FunctionValue<'ctx>> {
    let ptr = ctx.ptr_type(AddressSpace::default());
    get_or_declare_external(
        module,
        symbol,
        ctx.void_type().fn_type(&[ptr.into()], false),
    )
}

pub(super) fn external_fault_new<'ctx>(
    ctx: &'ctx Context,
    module: &Module<'ctx>,
) -> CodegenResult<FunctionValue<'ctx>> {
    get_or_declare_external(
        module,
        "hew_fault_new",
        ctx.ptr_type(AddressSpace::default())
            .fn_type(&[ctx.i32_type().into()], false),
    )
}

pub(super) fn external_fault_drop<'ctx>(
    ctx: &'ctx Context,
    module: &Module<'ctx>,
) -> CodegenResult<FunctionValue<'ctx>> {
    external_drop(ctx, module, "hew_fault_drop")
}

pub(super) fn external_fault_report<'ctx>(
    ctx: &'ctx Context,
    module: &Module<'ctx>,
) -> CodegenResult<FunctionValue<'ctx>> {
    let ptr = ctx.ptr_type(AddressSpace::default());
    get_or_declare_external(
        module,
        "hew_fault_report",
        ctx.i32_type().fn_type(&[ptr.into()], false),
    )
}

/// A private NUL-terminated string constant, as the pointer the runtime reads.
pub(super) fn c_string_literal<'ctx>(
    ctx: &'ctx Context,
    module: &Module<'ctx>,
    text: &str,
    name: &str,
) -> PointerValue<'ctx> {
    let data = ctx.const_string(text.as_bytes(), true);
    let global = module.add_global(data.get_type(), None, name);
    global.set_linkage(Linkage::Private);
    global.set_constant(true);
    global.set_initializer(&data);
    global.as_pointer_value()
}

pub(super) fn get_or_declare_external<'ctx>(
    module: &Module<'ctx>,
    symbol: &str,
    expected: FunctionType<'ctx>,
) -> CodegenResult<FunctionValue<'ctx>> {
    if let Some(existing) = module.get_function(symbol) {
        if existing.get_type() != expected {
            return Err(CodegenError::FailClosed(format!(
                "runtime declaration `{symbol}` has type {:?}, expected {:?}",
                existing.get_type(),
                expected
            )));
        }
        return Ok(existing);
    }
    Ok(module.add_function(symbol, expected, Some(Linkage::External)))
}
