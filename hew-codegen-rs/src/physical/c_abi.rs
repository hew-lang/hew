//! How C integers narrower than `int` cross a call boundary.
//!
//! A `bool`, `u8`, `i8`, `u16` or `i16` travels in a full register, and the
//! bits above the value are only defined when one side widens them. LLVM
//! records that with `zeroext`/`signext`, and the attribute means different
//! things on each side of a call:
//!
//! - On a parameter, it is the caller's promise and the callee's assumption.
//!   rustc marks narrow parameters on x86-64 and Apple AArch64, and every
//!   `bool` on every target, so a release-built runtime reads the whole
//!   register. Hew therefore widens every narrow argument it passes to a C
//!   symbol. Widening is never wrong, so this needs no per-target table.
//! - On a return, it is the callee's promise and the caller's assumption.
//!   rustc on x86-64 Linux returns `u8`/`i8` unwidened, so an imported
//!   narrow return is never marked and LLVM masks the value itself. The one
//!   exception is a C `bool` (`i1`), which every target widens.
//! - A Hew definition that C may call never assumes its caller widened a
//!   parameter (rustc on AArch64 Linux does not), and widens every narrow
//!   value it returns, which a caller on Apple AArch64 relies on.
//!
//! [`seal_c_scalar_widening`] enforces those rules over the whole module
//! before LLVM verification, so an unwidened narrow argument cannot reach an
//! object file.

use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::values::{CallSiteValue, InstructionOpcode};

use super::*;

/// How a C integer narrower than `int` fills the rest of its register.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum Widen {
    /// `bool` and unsigned integers: `zeroext`.
    Zero,
    /// Signed integers: `signext`.
    Sign,
}

impl Widen {
    const KINDS: [Self; 2] = [Self::Zero, Self::Sign];

    /// The widening a Hew scalar takes as a C argument, or `None` when it
    /// already fills a register.
    pub(super) fn of(ty: &ResolvedTy) -> Option<Self> {
        match ty {
            ResolvedTy::Bool | ResolvedTy::U8 | ResolvedTy::U16 => Some(Self::Zero),
            ResolvedTy::I8 | ResolvedTy::I16 => Some(Self::Sign),
            _ => None,
        }
    }

    fn kind_id(self) -> u32 {
        Attribute::get_named_enum_kind_id(match self {
            Self::Zero => "zeroext",
            Self::Sign => "signext",
        })
    }

    /// The attribute, in the context that owns `function`.
    fn attribute(self, function: FunctionValue<'_>) -> Attribute {
        function
            .get_type()
            .get_context()
            .create_enum_attribute(self.kind_id(), 0)
    }
}

/// The width of `ty` when it is an integer narrower than a C `int`.
fn narrow_bits<'ctx>(ty: impl Into<BasicMetadataTypeEnum<'ctx>>) -> Option<u32> {
    match ty.into() {
        BasicMetadataTypeEnum::IntType(int) if int.get_bit_width() < 32 => {
            Some(int.get_bit_width())
        }
        _ => None,
    }
}

/// The per-parameter widening for a C declaration. An `i1` parameter can
/// only be a C `bool`; every other narrow parameter must be named in
/// `widen`, because its LLVM type has lost its signedness.
fn parameter_widening(
    symbol: &str,
    ty: FunctionType<'_>,
    widen: &[(u32, Widen)],
) -> CodegenResult<Vec<Option<Widen>>> {
    let params = ty.get_param_types();
    let mut plan = vec![None; params.len()];
    for &(index, kind) in widen {
        let slot = plan.get_mut(index as usize).ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "C declaration `{symbol}` widens missing parameter {index}"
            ))
        })?;
        if narrow_bits(params[index as usize]).is_none() {
            return Err(CodegenError::FailClosed(format!(
                "C declaration `{symbol}` widens parameter {index}, which fills a register"
            )));
        }
        *slot = Some(kind);
    }
    for (index, (param, slot)) in params.iter().zip(&mut plan).enumerate() {
        match narrow_bits(*param) {
            Some(1) => {
                if slot.is_some_and(|kind| kind != Widen::Zero) {
                    return Err(CodegenError::FailClosed(format!(
                        "C declaration `{symbol}` sign-extends its bool parameter {index}"
                    )));
                }
                *slot = Some(Widen::Zero);
            }
            Some(_) if slot.is_none() => {
                return Err(CodegenError::FailClosed(format!(
                    "C declaration `{symbol}` leaves narrow parameter {index} without a widening"
                )));
            }
            _ => {}
        }
    }
    Ok(plan)
}

/// Declare a C symbol whose parameters all fill a register or are `bool`
/// (`i1`), or reuse its existing declaration.
pub(super) fn get_or_declare_external<'ctx>(
    module: &Module<'ctx>,
    symbol: &str,
    expected: FunctionType<'ctx>,
) -> CodegenResult<FunctionValue<'ctx>> {
    get_or_declare_external_widened(module, symbol, expected, &[])
}

/// Declare a C symbol, widening each narrow parameter named in `widen` by
/// its C type, or reuse an existing declaration that agrees exactly.
pub(super) fn get_or_declare_external_widened<'ctx>(
    module: &Module<'ctx>,
    symbol: &str,
    expected: FunctionType<'ctx>,
    widen: &[(u32, Widen)],
) -> CodegenResult<FunctionValue<'ctx>> {
    let plan = parameter_widening(symbol, expected, widen)?;
    if let Some(existing) = module.get_function(symbol) {
        if existing.get_type() != expected {
            return Err(CodegenError::FailClosed(format!(
                "runtime declaration `{symbol}` has type {:?}, expected {:?}",
                existing.get_type(),
                expected
            )));
        }
        for (index, kind) in plan.iter().enumerate() {
            if param_widening(existing, index as u32) != *kind {
                return Err(CodegenError::FailClosed(format!(
                    "runtime declaration `{symbol}` disagrees on how parameter {index} widens"
                )));
            }
        }
        return Ok(existing);
    }
    let function = module.add_function(symbol, expected, Some(Linkage::External));
    for (index, kind) in plan.iter().enumerate() {
        if let Some(kind) = kind {
            function.add_attribute(AttributeLoc::Param(index as u32), kind.attribute(function));
        }
    }
    if expected.get_return_type().and_then(narrow_bits) == Some(1) {
        function.add_attribute(AttributeLoc::Return, Widen::Zero.attribute(function));
    }
    Ok(function)
}

fn param_widening(function: FunctionValue<'_>, index: u32) -> Option<Widen> {
    Widen::KINDS.into_iter().find(|kind| {
        function
            .get_enum_attribute(AttributeLoc::Param(index), kind.kind_id())
            .is_some()
    })
}

fn return_widening(function: FunctionValue<'_>) -> Option<Widen> {
    Widen::KINDS.into_iter().find(|kind| {
        function
            .get_enum_attribute(AttributeLoc::Return, kind.kind_id())
            .is_some()
    })
}

/// Check every function against the narrow-scalar contract and mirror each
/// declaration's widening onto its direct call sites.
///
/// A body-less non-intrinsic function is a C import: every narrow parameter
/// carries its widening. A definition trusts no caller's widening, and
/// widens every narrow value it returns.
pub(super) fn seal_c_scalar_widening(module: &Module<'_>) -> CodegenResult<()> {
    for function in module.get_functions() {
        // Intrinsics are LLVM's own contract, and some take tokens, which have
        // no basic type to inspect.
        if function.get_intrinsic_id() != 0 {
            continue;
        }
        let name = function.get_name().to_string_lossy();
        let ty = function.get_type();
        let params = ty.get_param_types();
        if function.count_basic_blocks() == 0 {
            for (index, param) in params.iter().enumerate() {
                if narrow_bits(*param).is_some() && param_widening(function, index as u32).is_none()
                {
                    return Err(CodegenError::FailClosed(format!(
                        "C declaration `{name}` passes narrow parameter {index} unwidened"
                    )));
                }
            }
            continue;
        }
        for index in 0..params.len() {
            if param_widening(function, index as u32).is_some() {
                return Err(CodegenError::FailClosed(format!(
                    "definition `{name}` assumes its caller widened parameter {index}"
                )));
            }
        }
        if ty.get_return_type().and_then(narrow_bits).is_some()
            && return_widening(function).is_none()
        {
            return Err(CodegenError::FailClosed(format!(
                "definition `{name}` returns a narrow integer unwidened"
            )));
        }
        for block in function.get_basic_block_iter() {
            for instruction in block.get_instructions() {
                if instruction.get_opcode() != InstructionOpcode::Call {
                    continue;
                }
                let Ok(call) = CallSiteValue::try_from(instruction) else {
                    continue;
                };
                let Some(callee) = call.get_called_fn_value() else {
                    continue;
                };
                for index in 0..callee.count_params() {
                    if let Some(kind) = param_widening(callee, index) {
                        call.add_attribute(AttributeLoc::Param(index), kind.attribute(callee));
                    }
                }
                if let Some(kind) = return_widening(callee) {
                    call.add_attribute(AttributeLoc::Return, kind.attribute(callee));
                }
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn error(result: CodegenResult<impl Sized>) -> String {
        match result {
            Err(CodegenError::FailClosed(message)) => message,
            Err(other) => panic!("expected a fail-closed refusal, got {other:?}"),
            Ok(_) => panic!("expected a fail-closed refusal"),
        }
    }

    #[test]
    fn declarations_widen_by_c_type() {
        let ctx = Context::create();
        let module = ctx.create_module("widen");
        let ty = ctx.void_type().fn_type(
            &[
                ctx.bool_type().into(),
                ctx.i8_type().into(),
                ctx.i16_type().into(),
                ctx.i32_type().into(),
            ],
            false,
        );
        let function = get_or_declare_external_widened(
            &module,
            "c_narrow",
            ty,
            &[(1, Widen::Sign), (2, Widen::Zero)],
        )
        .expect("declare narrow C function");
        assert_eq!(
            (0..4)
                .map(|index| param_widening(function, index))
                .collect::<Vec<_>>(),
            [
                Some(Widen::Zero),
                Some(Widen::Sign),
                Some(Widen::Zero),
                None
            ]
        );
        let truth =
            get_or_declare_external(&module, "c_truth", ctx.bool_type().fn_type(&[], false))
                .expect("declare C bool result");
        assert_eq!(return_widening(truth), Some(Widen::Zero));
        let byte = get_or_declare_external(&module, "c_byte", ctx.i8_type().fn_type(&[], false))
            .expect("declare C byte result");
        assert_eq!(
            return_widening(byte),
            None,
            "a consumed narrow return is never trusted"
        );
        seal_c_scalar_widening(&module).expect("widened declarations seal");
    }

    #[test]
    fn a_narrow_parameter_without_its_c_type_is_refused() {
        let ctx = Context::create();
        let module = ctx.create_module("widen");
        let ty = ctx.void_type().fn_type(&[ctx.i8_type().into()], false);
        assert!(error(get_or_declare_external(&module, "c_byte_arg", ty))
            .contains("without a widening"));
        get_or_declare_external_widened(&module, "c_byte_arg", ty, &[(0, Widen::Zero)])
            .expect("declare with its C type");
        assert!(error(get_or_declare_external_widened(
            &module,
            "c_byte_arg",
            ty,
            &[(0, Widen::Sign)]
        ))
        .contains("disagrees"));
    }

    #[test]
    fn the_seal_refuses_an_unwidened_import() {
        let ctx = Context::create();
        let module = ctx.create_module("widen");
        module.add_function(
            "c_raw",
            ctx.void_type().fn_type(&[ctx.i8_type().into()], false),
            Some(Linkage::External),
        );
        assert!(error(seal_c_scalar_widening(&module)).contains("unwidened"));
    }

    #[test]
    fn the_seal_holds_definitions_to_the_callee_side() {
        let ctx = Context::create();
        let returns = ctx.create_module("returns");
        let narrow = returns.add_function("returns_byte", ctx.i8_type().fn_type(&[], false), None);
        let builder = ctx.create_builder();
        builder.position_at_end(ctx.append_basic_block(narrow, "entry"));
        builder
            .build_return(Some(&ctx.i8_type().const_zero()))
            .expect("return");
        assert!(error(seal_c_scalar_widening(&returns)).contains("returns a narrow integer"));

        let trusts = ctx.create_module("trusts");
        let ty = ctx.void_type().fn_type(&[ctx.i8_type().into()], false);
        let function = trusts.add_function("trusts_byte", ty, None);
        function.add_attribute(AttributeLoc::Param(0), Widen::Zero.attribute(function));
        builder.position_at_end(ctx.append_basic_block(function, "entry"));
        builder.build_return(None).expect("return");
        assert!(error(seal_c_scalar_widening(&trusts)).contains("assumes its caller widened"));
    }

    #[test]
    fn the_seal_mirrors_widening_onto_calls_and_skips_intrinsics() {
        let ctx = Context::create();
        let module = ctx.create_module("calls");
        let callee = get_or_declare_external(
            &module,
            "c_flag",
            ctx.void_type().fn_type(&[ctx.bool_type().into()], false),
        )
        .expect("declare");
        let memset = Intrinsic::find("llvm.memset")
            .and_then(|intrinsic| {
                intrinsic.get_declaration(
                    &module,
                    &[
                        ctx.ptr_type(AddressSpace::default()).into(),
                        ctx.i64_type().into(),
                    ],
                )
            })
            .expect("memset intrinsic");
        let caller = module.add_function("caller", ctx.void_type().fn_type(&[], false), None);
        let builder = ctx.create_builder();
        builder.position_at_end(ctx.append_basic_block(caller, "entry"));
        let call = builder
            .build_call(callee, &[ctx.bool_type().const_int(1, false).into()], "")
            .expect("call");
        let slot = builder.build_alloca(ctx.i64_type(), "slot").expect("slot");
        builder
            .build_call(
                memset,
                &[
                    slot.into(),
                    ctx.i8_type().const_zero().into(),
                    ctx.i64_type().const_int(8, false).into(),
                    ctx.bool_type().const_zero().into(),
                ],
                "",
            )
            .expect("memset");
        builder.build_return(None).expect("return");
        seal_c_scalar_widening(&module).expect("seal");
        assert!(call
            .get_enum_attribute(AttributeLoc::Param(0), Widen::Zero.kind_id())
            .is_some());
    }
}
