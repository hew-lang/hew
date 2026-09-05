//! LLVM emitter for verified physical MIR.
//!
//! This path consumes no raw, checked, or elaborated MIR. All ownership,
//! storage, layout, and private ABI choices are already explicit in the
//! verified physical module.

use std::collections::{BTreeMap, BTreeSet};
use std::num::NonZeroU32;
use std::path::Path;

use hew_mir::physical::{
    BlockId, CallableId, OwnKind, PhysicalAggregateDescriptor, PhysicalAggregateGlue,
    PhysicalAggregateId, PhysicalTypeInventory, PhysicalVariantArm, PhysicalVariantDescriptor,
    PhysicalVariantGlue, PhysicalVariantId, PhysicalVariantLayout, PhysicalVectorGlue,
    PhysicalVectorId, PhysicalVectorOp, TrapKind,
};
use hew_mir::{
    ArgumentTransfer, CloneAction, DestroyAction, ParamCarrier, PhysicalBlock, PhysicalCallable,
    PhysicalCheckedFailure, PhysicalConst, PhysicalEdge, PhysicalFunction, PhysicalLayout,
    PhysicalModule, PhysicalOp, PhysicalRepr, PhysicalRuntimeAction, PhysicalStorage,
    PhysicalTarget, PhysicalTerminator, ReturnTransfer, StorageId, VerifiedPhysicalModule,
};
use hew_parser::ast::{BinaryOp, UnaryOp};
use hew_runtime::internal::types::{
    HEW_TRAP_DIVIDE_BY_ZERO, HEW_TRAP_INDEX_OUT_OF_BOUNDS, HEW_TRAP_INTEGER_OVERFLOW,
    HEW_TRAP_SHIFT_OUT_OF_RANGE, HEW_TRAP_SIGNED_MIN_DIV_NEG_ONE,
};
use hew_runtime::vec::HewTypeOwnershipKind;
use hew_types::{vector_element_type, EntryExitAction, EntryIntegerType, ResolvedTy};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::intrinsics::Intrinsic;
use inkwell::module::{Linkage, Module};
use inkwell::targets::{FileType, TargetData, TargetMachine};
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValueEnum, FunctionValue, IntValue, PointerValue,
};
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};

use crate::llvm::{
    entry_body_symbol_for_triple, native_emission_triple, CodegenError, CodegenResult,
    EmitArtefacts, LlvmResultExt, OptLevel,
};

/// `hew_print_value`'s audited ABI tag for a signed 64-bit integer.
const HEW_PRINT_KIND_I64: u64 = 1;

/// Native object emission options for the physical pipeline.
#[derive(Debug, Clone)]
pub struct PhysicalEmitOptions<'a> {
    pub module_name: &'a str,
    pub out_dir: &'a Path,
    pub target_triple: Option<&'a str>,
    pub opt_level: OptLevel,
    pub emit_llvm: bool,
    pub address_sanitizer: bool,
}

/// Resolve primitive physical layouts from the exact LLVM target machine.
///
/// # Errors
///
/// Returns a target setup error when LLVM cannot create a machine for the
/// triple, or a fail-closed error when a measured layout cannot fit the
/// physical model.
pub fn physical_target_for_triple(triple: &str) -> Result<PhysicalTarget, CodegenError> {
    physical_target_for_types(triple, std::iter::empty::<&ResolvedTy>())
}

/// Resolve physical layouts for primitives and the concrete SIR type inventory
/// using the exact LLVM target machine.
///
/// # Errors
///
/// Returns a target setup error or a fail-closed error when any demanded type
/// has no admitted physical representation.
pub fn physical_target_for_types<'a>(
    triple: &str,
    types: impl IntoIterator<Item = &'a ResolvedTy>,
) -> Result<PhysicalTarget, CodegenError> {
    physical_target_for_parts(triple, types, std::iter::empty(), std::iter::empty())
}

/// Resolve the exact target layouts for one demanded physical type inventory.
///
/// # Errors
///
/// Returns a target setup error or refuses a demanded aggregate whose exact
/// descriptor cannot be realized recursively for the selected target.
pub fn physical_target_for_inventory(
    triple: &str,
    inventory: &PhysicalTypeInventory,
) -> Result<PhysicalTarget, CodegenError> {
    physical_target_for_parts(
        triple,
        inventory.types(),
        inventory.aggregates(),
        inventory.variants(),
    )
}

fn physical_target_for_parts<'a>(
    triple: &str,
    types: impl IntoIterator<Item = &'a ResolvedTy>,
    aggregates: impl IntoIterator<Item = &'a PhysicalAggregateDescriptor>,
    variants: impl IntoIterator<Item = &'a PhysicalVariantDescriptor>,
) -> Result<PhysicalTarget, CodegenError> {
    let machine = crate::llvm::target_machine_for_triple_with_opt_level(triple, OptLevel::O0)?;
    let data = machine.get_target_data();
    let data_layout = data
        .get_data_layout()
        .as_str()
        .to_string_lossy()
        .into_owned();
    let mut target = PhysicalTarget::new(triple, data_layout);
    let ctx = Context::create();
    let aggregate_fields = aggregates
        .into_iter()
        .map(|aggregate| (aggregate.ty.clone(), aggregate.fields.clone()))
        .collect::<BTreeMap<_, _>>();
    let variant_shapes = variants
        .into_iter()
        .map(|variant| (variant.ty.clone(), variant.clone()))
        .collect::<BTreeMap<_, _>>();
    let mut visiting = BTreeSet::new();
    for ty in primitive_types() {
        realize_layout(
            &ctx,
            &data,
            &mut target,
            &ty,
            &aggregate_fields,
            &variant_shapes,
            &mut visiting,
        )?;
    }
    for ty in types {
        realize_layout(
            &ctx,
            &data,
            &mut target,
            ty,
            &aggregate_fields,
            &variant_shapes,
            &mut visiting,
        )?;
    }
    Ok(target)
}

fn realize_layout(
    ctx: &Context,
    data: &TargetData,
    target: &mut PhysicalTarget,
    ty: &ResolvedTy,
    aggregate_fields: &BTreeMap<ResolvedTy, Vec<ResolvedTy>>,
    variant_shapes: &BTreeMap<ResolvedTy, PhysicalVariantDescriptor>,
    visiting: &mut BTreeSet<ResolvedTy>,
) -> CodegenResult<()> {
    if target.layout(ty).is_some() {
        return Ok(());
    }
    if !visiting.insert(ty.clone()) {
        return Err(CodegenError::FailClosed(format!(
            "physical aggregate `{}` contains itself by value",
            ty.user_facing()
        )));
    }
    if let Some(shape) = variant_shapes.get(ty) {
        if shape.is_indirect {
            return Err(CodegenError::FailClosed(format!(
                "physical indirect enum `{}` is not yet admitted",
                ty.user_facing()
            )));
        }
        if shape.variants.is_empty() {
            return Err(CodegenError::FailClosed(format!(
                "physical variant `{}` has no declaration-order cases",
                ty.user_facing()
            )));
        }
        let mut variant_layouts = Vec::with_capacity(shape.variants.len());
        for fields in &shape.variants {
            let mut layouts = Vec::with_capacity(fields.len());
            for field in fields {
                realize_layout(
                    ctx,
                    data,
                    target,
                    field,
                    aggregate_fields,
                    variant_shapes,
                    visiting,
                )?;
                layouts.push(target.layout(field).cloned().ok_or_else(|| {
                    CodegenError::FailClosed(format!(
                        "physical target did not realize variant field `{}`",
                        field.user_facing()
                    ))
                })?);
            }
            let repr = PhysicalRepr::Struct(layouts);
            let (size, align) = measure_layout(data, llvm_type(ctx, &repr)?);
            variant_layouts.push(PhysicalLayout { size, align, repr });
        }
        let payload_size = variant_layouts
            .iter()
            .map(|layout| layout.size)
            .max()
            .unwrap_or(0)
            .max(1);
        let payload_align = variant_layouts
            .iter()
            .map(|layout| layout.align)
            .max()
            .unwrap_or(1)
            .max(1);
        let payload_bits = u16::try_from(payload_align.checked_mul(8).ok_or_else(|| {
            CodegenError::FailClosed("physical variant payload alignment overflow".into())
        })?)
        .map_err(|_| {
            CodegenError::FailClosed("physical variant payload alignment exceeds u16".into())
        })?;
        let payload_element = integer_layout(ctx, data, payload_bits)?;
        if payload_element.size != u64::from(payload_align)
            || payload_element.align != payload_align
        {
            return Err(CodegenError::FailClosed(format!(
                "target cannot realize variant payload alignment {payload_align} with an exact integer carrier"
            )));
        }
        let payload_len =
            u32::try_from(payload_size.div_ceil(u64::from(payload_align))).map_err(|_| {
                CodegenError::FailClosed("physical variant payload exceeds u32 elements".into())
            })?;
        let payload_repr = PhysicalRepr::Array {
            element: Box::new(payload_element),
            len: payload_len,
        };
        let (realized_payload_size, realized_payload_align) =
            measure_layout(data, llvm_type(ctx, &payload_repr)?);
        if realized_payload_size < payload_size || realized_payload_align != payload_align {
            return Err(CodegenError::FailClosed(
                "target variant payload carrier disagrees with its required size or alignment"
                    .into(),
            ));
        }
        let payload_layout = PhysicalLayout {
            size: realized_payload_size,
            align: realized_payload_align,
            repr: payload_repr,
        };
        let tag_bits = match shape.variants.len() {
            1..=256 => 8,
            257..=65_536 => 16,
            count => {
                return Err(CodegenError::FailClosed(format!(
                    "physical variant `{}` has unsupported case count {count}",
                    ty.user_facing()
                )));
            }
        };
        let tag_layout = integer_layout(ctx, data, tag_bits)?;
        let repr = PhysicalRepr::Struct(vec![tag_layout, payload_layout]);
        let (size, align) = measure_layout(data, llvm_type(ctx, &repr)?);
        let object = PhysicalLayout { size, align, repr };
        target.insert_layout(ty.clone(), object.clone());
        target.insert_variant_layout(PhysicalVariantLayout {
            ty: ty.clone(),
            is_indirect: false,
            object,
            variants: variant_layouts,
        });
        visiting.remove(ty);
        return Ok(());
    }
    let fields = match ty {
        ResolvedTy::Tuple(fields) => Some(fields.as_slice()),
        _ => aggregate_fields.get(ty).map(Vec::as_slice),
    };
    let repr = if let Some(fields) = fields {
        let mut layouts = Vec::with_capacity(fields.len());
        for field in fields {
            realize_layout(
                ctx,
                data,
                target,
                field,
                aggregate_fields,
                variant_shapes,
                visiting,
            )?;
            layouts.push(target.layout(field).cloned().ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "physical target did not realize aggregate field `{}`",
                    field.user_facing()
                ))
            })?);
        }
        PhysicalRepr::Struct(layouts)
    } else {
        primitive_repr(ctx, data, ty)?
    };
    let llvm_ty = llvm_type(ctx, &repr)?;
    let (size, align) = measure_layout(data, llvm_ty);
    target.insert_layout(ty.clone(), PhysicalLayout { size, align, repr });
    visiting.remove(ty);
    Ok(())
}

/// Emit one native object and, when requested, diagnostic LLVM IR from
/// verified physical MIR.
///
/// # Errors
///
/// Returns a fail-closed or LLVM error when the module target disagrees with
/// the requested machine, LLVM verification fails, or object emission fails.
pub fn emit_physical_object(
    verified: &VerifiedPhysicalModule,
    options: &PhysicalEmitOptions<'_>,
) -> Result<EmitArtefacts, CodegenError> {
    let triple = options
        .target_triple
        .map_or_else(native_emission_triple, ToOwned::to_owned);
    let module = verified.module();
    if module.target.triple != triple {
        return Err(CodegenError::FailClosed(format!(
            "physical module target `{}` cannot emit for `{triple}`",
            module.target.triple
        )));
    }
    std::fs::create_dir_all(options.out_dir)?;
    let ll_path = options
        .emit_llvm
        .then(|| options.out_dir.join(format!("{}.ll", options.module_name)));
    let object_path = options.out_dir.join(format!("{}.o", options.module_name));
    emit_physical_to_paths(
        verified,
        options.module_name,
        &triple,
        options.opt_level,
        options.address_sanitizer,
        ll_path.as_deref(),
        Some(&object_path),
    )?;
    Ok(EmitArtefacts {
        ll_path,
        native_obj_path: Some(object_path),
        ..EmitArtefacts::default()
    })
}

/// Build and LLVM-verify physical MIR without writing an object.
///
/// # Errors
///
/// Returns a target, physical-contract, or LLVM verifier error.
pub fn validate_physical_codegen(
    verified: &VerifiedPhysicalModule,
    module_name: &str,
) -> Result<(), CodegenError> {
    let triple = &verified.module().target.triple;
    emit_physical_to_paths(
        verified,
        module_name,
        triple,
        OptLevel::O0,
        false,
        None,
        None,
    )
}

fn emit_physical_to_paths(
    verified: &VerifiedPhysicalModule,
    module_name: &str,
    triple: &str,
    opt_level: OptLevel,
    address_sanitizer: bool,
    ll_path: Option<&Path>,
    object_path: Option<&Path>,
) -> CodegenResult<()> {
    let machine = crate::llvm::target_machine_for_triple_with_opt_level(triple, opt_level)?;
    let ctx = Context::create();
    let llvm_module = build_module(&ctx, verified.module(), module_name, &machine)?;
    crate::llvm::run_module_pipeline(&llvm_module, &machine, opt_level)?;
    if address_sanitizer {
        crate::sanitizer::instrument_address_sanitizer(&llvm_module, &machine)
            .map_err(CodegenError::FailClosed)?;
    }
    if let Some(path) = ll_path {
        llvm_module
            .print_to_file(path)
            .llvm_ctx_with(|| format!("write physical LLVM IR to {}", path.display()))?;
    }
    if let Some(path) = object_path {
        machine
            .write_to_file(&llvm_module, FileType::Object, path)
            .llvm_ctx_with(|| format!("write physical object to {}", path.display()))?;
    }
    Ok(())
}

fn primitive_types() -> Vec<ResolvedTy> {
    vec![
        ResolvedTy::I8,
        ResolvedTy::I16,
        ResolvedTy::I32,
        ResolvedTy::I64,
        ResolvedTy::U8,
        ResolvedTy::U16,
        ResolvedTy::U32,
        ResolvedTy::U64,
        ResolvedTy::Isize,
        ResolvedTy::Usize,
        ResolvedTy::F32,
        ResolvedTy::F64,
        ResolvedTy::Bool,
        ResolvedTy::Char,
        ResolvedTy::String,
        ResolvedTy::Bytes,
        ResolvedTy::CancellationToken,
        ResolvedTy::Duration,
        ResolvedTy::Unit,
    ]
}

fn primitive_repr(
    ctx: &Context,
    target: &TargetData,
    ty: &ResolvedTy,
) -> CodegenResult<PhysicalRepr> {
    let pointer_bits = u16::try_from(target.get_pointer_byte_size(None) * 8).map_err(|_| {
        CodegenError::FailClosed("target pointer width does not fit physical representation".into())
    })?;
    Ok(match ty {
        ResolvedTy::I8 | ResolvedTy::U8 | ResolvedTy::Bool => PhysicalRepr::Integer { bits: 8 },
        ResolvedTy::I16 | ResolvedTy::U16 => PhysicalRepr::Integer { bits: 16 },
        ResolvedTy::I32 | ResolvedTy::U32 | ResolvedTy::Char => PhysicalRepr::Integer { bits: 32 },
        ResolvedTy::I64 | ResolvedTy::U64 | ResolvedTy::Duration => {
            PhysicalRepr::Integer { bits: 64 }
        }
        ResolvedTy::Isize | ResolvedTy::Usize => PhysicalRepr::Integer { bits: pointer_bits },
        ResolvedTy::F32 => PhysicalRepr::Float { bits: 32 },
        ResolvedTy::F64 => PhysicalRepr::Float { bits: 64 },
        ResolvedTy::String | ResolvedTy::CancellationToken => PhysicalRepr::Pointer,
        vector if vector_element_type(vector).is_some() => PhysicalRepr::Pointer,
        ResolvedTy::Bytes => PhysicalRepr::Struct(vec![
            pointer_layout(ctx, target)?,
            integer_layout(ctx, target, 32)?,
            integer_layout(ctx, target, 32)?,
        ]),
        ResolvedTy::Unit => PhysicalRepr::Unit,
        other => {
            return Err(CodegenError::FailClosed(format!(
                "physical target resolver does not support `{}`",
                other.user_facing()
            )));
        }
    })
}

fn pointer_layout(ctx: &Context, target: &TargetData) -> CodegenResult<PhysicalLayout> {
    let ty = ctx.ptr_type(AddressSpace::default());
    Ok(PhysicalLayout {
        size: target.get_abi_size(&ty),
        align: target.get_abi_alignment(&ty),
        repr: PhysicalRepr::Pointer,
    })
}

fn integer_layout(ctx: &Context, target: &TargetData, bits: u16) -> CodegenResult<PhysicalLayout> {
    let width = NonZeroU32::new(u32::from(bits))
        .ok_or_else(|| CodegenError::FailClosed("zero-width physical integer".into()))?;
    let ty = ctx
        .custom_width_int_type(width)
        .map_err(|error| CodegenError::FailClosed(error.into()))?;
    Ok(PhysicalLayout {
        size: target.get_abi_size(&ty),
        align: target.get_abi_alignment(&ty),
        repr: PhysicalRepr::Integer { bits },
    })
}

fn measure_layout(target: &TargetData, ty: BasicTypeEnum<'_>) -> (u64, u32) {
    match ty {
        BasicTypeEnum::ArrayType(ty) => (target.get_abi_size(&ty), target.get_abi_alignment(&ty)),
        BasicTypeEnum::FloatType(ty) => (target.get_abi_size(&ty), target.get_abi_alignment(&ty)),
        BasicTypeEnum::IntType(ty) => (target.get_abi_size(&ty), target.get_abi_alignment(&ty)),
        BasicTypeEnum::PointerType(ty) => (target.get_abi_size(&ty), target.get_abi_alignment(&ty)),
        BasicTypeEnum::StructType(ty) => (target.get_abi_size(&ty), target.get_abi_alignment(&ty)),
        BasicTypeEnum::VectorType(ty) => (target.get_abi_size(&ty), target.get_abi_alignment(&ty)),
        BasicTypeEnum::ScalableVectorType(ty) => {
            (target.get_abi_size(&ty), target.get_abi_alignment(&ty))
        }
    }
}

fn llvm_type<'ctx>(ctx: &'ctx Context, repr: &PhysicalRepr) -> CodegenResult<BasicTypeEnum<'ctx>> {
    Ok(match repr {
        PhysicalRepr::Unit => ctx.struct_type(&[], false).into(),
        PhysicalRepr::Integer { bits } => {
            let width = NonZeroU32::new(u32::from(*bits))
                .ok_or_else(|| CodegenError::FailClosed("zero-width physical integer".into()))?;
            ctx.custom_width_int_type(width)
                .map_err(|error| CodegenError::FailClosed(error.into()))?
                .into()
        }
        PhysicalRepr::Float { bits: 32 } => ctx.f32_type().into(),
        PhysicalRepr::Float { bits: 64 } => ctx.f64_type().into(),
        PhysicalRepr::Float { bits } => {
            return Err(CodegenError::FailClosed(format!(
                "unsupported physical float width {bits}"
            )));
        }
        PhysicalRepr::Pointer => ctx.ptr_type(AddressSpace::default()).into(),
        PhysicalRepr::Array { element, len } => {
            llvm_type(ctx, &element.repr)?.array_type(*len).into()
        }
        PhysicalRepr::Struct(fields) => {
            let fields = fields
                .iter()
                .map(|field| llvm_type(ctx, &field.repr))
                .collect::<Result<Vec<_>, _>>()?;
            ctx.struct_type(&fields, false).into()
        }
    })
}

struct ModuleEmitter<'ctx, 'm> {
    ctx: &'ctx Context,
    module: &'m PhysicalModule,
    llvm: Module<'ctx>,
    functions: BTreeMap<CallableId, FunctionValue<'ctx>>,
}

struct FunctionEmitter<'a, 'ctx> {
    module: &'a PhysicalModule,
    function: &'a PhysicalFunction,
    ctx: &'ctx Context,
    llvm: &'a Module<'ctx>,
    builder: Builder<'ctx>,
    value: FunctionValue<'ctx>,
    blocks: BTreeMap<BlockId, BasicBlock<'ctx>>,
    slots: Vec<PointerValue<'ctx>>,
    result_out: Option<PointerValue<'ctx>>,
    fault_out: PointerValue<'ctx>,
    active_fault: PointerValue<'ctx>,
    active_status: PointerValue<'ctx>,
    functions: &'a BTreeMap<CallableId, FunctionValue<'ctx>>,
}

/// Execute verified type recipes in either a language body or a container
/// element callback. Storage ownership and ABI transfers remain physical MIR facts.
struct ValueEmitter<'a, 'ctx> {
    module: &'a PhysicalModule,
    ctx: &'ctx Context,
    llvm: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    value: FunctionValue<'ctx>,
}

impl<'a, 'ctx> ValueEmitter<'a, 'ctx> {
    fn entry_scratch(
        &self,
        ty: BasicTypeEnum<'ctx>,
        name: &str,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let prologue = self.value.get_first_basic_block().ok_or_else(|| {
            CodegenError::FailClosed("physical function has no allocation prologue".into())
        })?;
        let builder = self.ctx.create_builder();
        if let Some(terminator) = prologue.get_terminator() {
            builder.position_before(&terminator);
        } else {
            builder.position_at_end(prologue);
        }
        builder
            .build_alloca(ty, name)
            .llvm_ctx("allocate reusable physical scratch storage")
    }

    fn variant_payload_ptr(
        &self,
        object: PointerValue<'ctx>,
        layout: &PhysicalVariantLayout,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let object_ty = llvm_type(self.ctx, &layout.object.repr)?.into_struct_type();
        self.builder
            .build_struct_gep(object_ty, object, 1, "variant.payload.ptr")
            .llvm_ctx("address physical variant payload")
    }

    fn emit_invalid_variant_tag(&self) -> CodegenResult<()> {
        let trap = Intrinsic::find("llvm.trap")
            .ok_or_else(|| CodegenError::FailClosed("LLVM trap intrinsic is unavailable".into()))?
            .get_declaration(self.llvm, &[])
            .ok_or_else(|| CodegenError::FailClosed("LLVM trap declaration failed".into()))?;
        self.builder
            .build_call(trap, &[], "variant.invalid.trap")
            .llvm_ctx("emit invalid variant tag trap")?;
        self.builder
            .build_unreachable()
            .llvm_ctx("terminate invalid variant tag")?;
        Ok(())
    }

    fn clone_loaded_value(
        &self,
        value: BasicValueEnum<'ctx>,
        layout: &PhysicalLayout,
        action: CloneAction,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        match action {
            CloneAction::Bitwise => Ok(value),
            CloneAction::StringRetain => {
                let pointer = value.into_pointer_value();
                let function = external_unary_ptr(self.ctx, self.llvm, "hew_string_clone")?;
                self.builder
                    .build_call(function, &[pointer.into()], "string.retain")
                    .llvm_ctx("retain physical string")?
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| CodegenError::FailClosed("string retain returned void".into()))
            }
            CloneAction::BytesRetain => {
                let aggregate = value.into_struct_value();
                let pointer = self
                    .builder
                    .build_extract_value(aggregate, 0, "bytes.ptr")
                    .llvm_ctx("extract physical bytes pointer")?
                    .into_pointer_value();
                let ptr = self.ctx.ptr_type(AddressSpace::default());
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_bytes_clone_ref",
                    self.ctx.void_type().fn_type(&[ptr.into()], false),
                )?;
                self.builder
                    .build_call(function, &[pointer.into()], "bytes.retain")
                    .llvm_ctx("retain physical bytes")?;
                Ok(aggregate.into())
            }
            CloneAction::Aggregate(id) => {
                let glue = self.aggregate_glue(id)?;
                let source = value.into_struct_value();
                let BasicTypeEnum::StructType(aggregate_ty) = llvm_type(self.ctx, &layout.repr)?
                else {
                    return Err(CodegenError::FailClosed(
                        "physical aggregate clone has a non-struct layout".into(),
                    ));
                };
                let mut clone = aggregate_ty.get_undef();
                for (index, field) in glue.fields.iter().enumerate() {
                    let index = u32::try_from(index).map_err(|_| {
                        CodegenError::FailClosed(
                            "physical aggregate field index exceeds u32".into(),
                        )
                    })?;
                    let value = self
                        .builder
                        .build_extract_value(source, index, "aggregate.clone.field")
                        .llvm_ctx("extract physical aggregate clone field")?;
                    let action = field.clone.ok_or_else(|| {
                        CodegenError::FailClosed(format!(
                            "physical aggregate glue {} field {index} has no clone action",
                            id.0
                        ))
                    })?;
                    let layout = self.module.target.layout(&field.ty).ok_or_else(|| {
                        CodegenError::FailClosed(format!(
                            "physical aggregate glue {} field {index} has no target layout",
                            id.0
                        ))
                    })?;
                    let value = self.clone_loaded_value(value, layout, action)?;
                    clone = match self
                        .builder
                        .build_insert_value(clone, value, index, "aggregate.clone")
                        .llvm_ctx("insert physical aggregate clone field")?
                    {
                        inkwell::values::AggregateValueEnum::StructValue(value) => value,
                        inkwell::values::AggregateValueEnum::ArrayValue(_) => {
                            return Err(CodegenError::FailClosed(
                                "physical aggregate clone produced an LLVM array".into(),
                            ));
                        }
                    };
                }
                Ok(clone.into())
            }
            CloneAction::Variant(id) => self.clone_variant_value(value, layout, id),
            CloneAction::Vector(id) => {
                self.vector_glue(id)?;
                let function = external_unary_ptr(self.ctx, self.llvm, "hew_vec_clone_owned")?;
                self.builder
                    .build_call(function, &[value.into()], "vector.clone")
                    .llvm_ctx("clone descriptor-backed vector")?
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| CodegenError::FailClosed("vector clone returned void".into()))
            }
        }
    }

    fn destroy_loaded_value(
        &self,
        value: BasicValueEnum<'ctx>,
        layout: &PhysicalLayout,
        action: DestroyAction,
    ) -> CodegenResult<()> {
        match action {
            DestroyAction::StringRelease | DestroyAction::BytesRelease => {
                let pointer = match action {
                    DestroyAction::StringRelease => value.into_pointer_value(),
                    DestroyAction::BytesRelease => self
                        .builder
                        .build_extract_value(value.into_struct_value(), 0, "bytes.drop.ptr")
                        .llvm_ctx("extract bytes release pointer")?
                        .into_pointer_value(),
                    DestroyAction::Aggregate(_) => unreachable!("matched primitive release"),
                    DestroyAction::Variant(_) => unreachable!("matched primitive release"),
                    DestroyAction::Vector(_) => unreachable!("matched primitive release"),
                };
                let symbol = match action {
                    DestroyAction::StringRelease => "hew_string_drop",
                    DestroyAction::BytesRelease => "hew_bytes_drop",
                    DestroyAction::Aggregate(_) => unreachable!("matched primitive release"),
                    DestroyAction::Variant(_) => unreachable!("matched primitive release"),
                    DestroyAction::Vector(_) => unreachable!("matched primitive release"),
                };
                let function = external_drop(self.ctx, self.llvm, symbol)?;
                self.builder
                    .build_call(function, &[pointer.into()], "physical.drop")
                    .llvm_ctx("release physical owner")?;
                Ok(())
            }
            DestroyAction::Aggregate(id) => {
                let glue = self.aggregate_glue(id)?;
                let value = value.into_struct_value();
                let PhysicalRepr::Struct(layout_fields) = &layout.repr else {
                    return Err(CodegenError::FailClosed(
                        "physical aggregate destroy has a non-struct layout".into(),
                    ));
                };
                for index in (0..glue.fields.len()).rev() {
                    let field = &glue.fields[index];
                    let Some(action) = field.destroy else {
                        continue;
                    };
                    let index = u32::try_from(index).map_err(|_| {
                        CodegenError::FailClosed(
                            "physical aggregate field index exceeds u32".into(),
                        )
                    })?;
                    let field_value = self
                        .builder
                        .build_extract_value(value, index, "aggregate.destroy.field")
                        .llvm_ctx("extract physical aggregate destroy field")?;
                    self.destroy_loaded_value(field_value, &layout_fields[index as usize], action)?;
                }
                Ok(())
            }
            DestroyAction::Variant(id) => self.destroy_variant_value(value, layout, id),
            DestroyAction::Vector(id) => {
                self.vector_glue(id)?;
                let function = external_drop(self.ctx, self.llvm, "hew_vec_free_owned")?;
                self.builder
                    .build_call(function, &[value.into()], "vector.drop")
                    .llvm_ctx("destroy descriptor-backed vector")?;
                Ok(())
            }
        }
    }

    fn clone_variant_value(
        &self,
        value: BasicValueEnum<'ctx>,
        _layout: &PhysicalLayout,
        id: PhysicalVariantId,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let glue = self.variant_glue(id)?;
        let variant_layout = self.variant_layout(&glue.ty)?;
        if variant_layout.is_indirect {
            return Err(CodegenError::FailClosed(
                "physical indirect variant cloning is not yet admitted".into(),
            ));
        }
        let object_ty = llvm_type(self.ctx, &variant_layout.object.repr)?.into_struct_type();
        let source = self.entry_scratch(object_ty.into(), "variant.clone.source")?;
        let destination = self.entry_scratch(object_ty.into(), "variant.clone.destination")?;
        self.builder
            .build_store(source, value)
            .llvm_ctx("store physical variant clone source")?;
        let tag = self
            .builder
            .build_extract_value(value.into_struct_value(), 0, "variant.clone.tag")
            .llvm_ctx("read physical variant clone tag")?
            .into_int_value();
        let invalid = self
            .ctx
            .append_basic_block(self.value, "variant.clone.invalid");
        let complete = self
            .ctx
            .append_basic_block(self.value, "variant.clone.complete");
        let cases = glue
            .variants
            .iter()
            .enumerate()
            .map(|(index, _)| {
                (
                    tag.get_type().const_int(index as u64, false),
                    self.ctx
                        .append_basic_block(self.value, &format!("variant.clone.case.{index}")),
                )
            })
            .collect::<Vec<_>>();
        self.builder
            .build_switch(tag, invalid, &cases)
            .llvm_ctx("dispatch physical variant clone")?;
        for (index, (_, case_block)) in cases.iter().enumerate() {
            self.builder.position_at_end(*case_block);
            let tag_value = tag.get_type().const_int(index as u64, false);
            let object = self
                .builder
                .build_insert_value(object_ty.const_zero(), tag_value, 0, "variant.clone.object")
                .llvm_ctx("write cloned physical variant tag")?
                .into_struct_value();
            self.builder
                .build_store(destination, object)
                .llvm_ctx("initialize cloned physical variant")?;
            let recipe = &glue.variants[index];
            if !recipe.fields.is_empty() {
                let payload_layout = &variant_layout.variants[index];
                let payload_ty = llvm_type(self.ctx, &payload_layout.repr)?.into_struct_type();
                let source_ptr = self.variant_payload_ptr(source, variant_layout)?;
                let source_payload = self
                    .builder
                    .build_load(payload_ty, source_ptr, "variant.clone.payload")
                    .llvm_ctx("load physical variant clone payload")?
                    .into_struct_value();
                let mut destination_payload = payload_ty.get_undef();
                for (field_index, field) in recipe.fields.iter().enumerate() {
                    let field_index = u32::try_from(field_index).map_err(|_| {
                        CodegenError::FailClosed("variant clone field index exceeds u32".into())
                    })?;
                    let field_value = self
                        .builder
                        .build_extract_value(source_payload, field_index, "variant.clone.field")
                        .llvm_ctx("extract physical variant clone field")?;
                    let action = field.clone.ok_or_else(|| {
                        CodegenError::FailClosed(format!(
                            "physical variant glue {} case {index} field {field_index} has no clone action",
                            id.0
                        ))
                    })?;
                    let field_layout = self.module.target.layout(&field.ty).ok_or_else(|| {
                        CodegenError::FailClosed("variant clone field has no target layout".into())
                    })?;
                    let cloned = self.clone_loaded_value(field_value, field_layout, action)?;
                    destination_payload = self
                        .builder
                        .build_insert_value(
                            destination_payload,
                            cloned,
                            field_index,
                            "variant.clone.payload.result",
                        )
                        .llvm_ctx("insert physical variant clone field")?
                        .into_struct_value();
                }
                let destination_ptr = self.variant_payload_ptr(destination, variant_layout)?;
                self.builder
                    .build_store(destination_ptr, destination_payload)
                    .llvm_ctx("store cloned physical variant payload")?;
            }
            self.builder
                .build_unconditional_branch(complete)
                .llvm_ctx("finish physical variant clone case")?;
        }
        self.builder.position_at_end(invalid);
        self.emit_invalid_variant_tag()?;
        self.builder.position_at_end(complete);
        self.builder
            .build_load(object_ty, destination, "variant.clone.result")
            .llvm_ctx("load cloned physical variant")
    }

    fn destroy_variant_value(
        &self,
        value: BasicValueEnum<'ctx>,
        _layout: &PhysicalLayout,
        id: PhysicalVariantId,
    ) -> CodegenResult<()> {
        let glue = self.variant_glue(id)?;
        let variant_layout = self.variant_layout(&glue.ty)?;
        if variant_layout.is_indirect {
            return Err(CodegenError::FailClosed(
                "physical indirect variant destruction is not yet admitted".into(),
            ));
        }
        let object_ty = llvm_type(self.ctx, &variant_layout.object.repr)?.into_struct_type();
        let source = self.entry_scratch(object_ty.into(), "variant.destroy.source")?;
        self.builder
            .build_store(source, value)
            .llvm_ctx("store physical variant destroy source")?;
        let tag = self
            .builder
            .build_extract_value(value.into_struct_value(), 0, "variant.destroy.tag")
            .llvm_ctx("read physical variant destroy tag")?
            .into_int_value();
        let invalid = self
            .ctx
            .append_basic_block(self.value, "variant.destroy.invalid");
        let complete = self
            .ctx
            .append_basic_block(self.value, "variant.destroy.complete");
        let cases = glue
            .variants
            .iter()
            .enumerate()
            .map(|(index, _)| {
                (
                    tag.get_type().const_int(index as u64, false),
                    self.ctx
                        .append_basic_block(self.value, &format!("variant.destroy.case.{index}")),
                )
            })
            .collect::<Vec<_>>();
        self.builder
            .build_switch(tag, invalid, &cases)
            .llvm_ctx("dispatch physical variant destroy")?;
        for (index, (_, case_block)) in cases.iter().enumerate() {
            self.builder.position_at_end(*case_block);
            let recipe = &glue.variants[index];
            if !recipe.fields.is_empty() {
                let payload_layout = &variant_layout.variants[index];
                let payload_ty = llvm_type(self.ctx, &payload_layout.repr)?.into_struct_type();
                let source_ptr = self.variant_payload_ptr(source, variant_layout)?;
                let payload = self
                    .builder
                    .build_load(payload_ty, source_ptr, "variant.destroy.payload")
                    .llvm_ctx("load physical variant destroy payload")?
                    .into_struct_value();
                for field_index in (0..recipe.fields.len()).rev() {
                    let field = &recipe.fields[field_index];
                    let Some(action) = field.destroy else {
                        continue;
                    };
                    let field_index = u32::try_from(field_index).map_err(|_| {
                        CodegenError::FailClosed("variant destroy field index exceeds u32".into())
                    })?;
                    let field_value = self
                        .builder
                        .build_extract_value(payload, field_index, "variant.destroy.field")
                        .llvm_ctx("extract physical variant destroy field")?;
                    let field_layout = self.module.target.layout(&field.ty).ok_or_else(|| {
                        CodegenError::FailClosed(
                            "variant destroy field has no target layout".into(),
                        )
                    })?;
                    self.destroy_loaded_value(field_value, field_layout, action)?;
                }
            }
            self.builder
                .build_unconditional_branch(complete)
                .llvm_ctx("finish physical variant destroy case")?;
        }
        self.builder.position_at_end(invalid);
        self.emit_invalid_variant_tag()?;
        self.builder.position_at_end(complete);
        Ok(())
    }

    fn aggregate_glue(&self, id: PhysicalAggregateId) -> CodegenResult<&'a PhysicalAggregateGlue> {
        self.module
            .aggregate_glue
            .get(id.0 as usize)
            .filter(|glue| glue.id == id)
            .ok_or_else(|| {
                CodegenError::FailClosed(format!("unknown physical aggregate glue {}", id.0))
            })
    }

    fn vector_glue(&self, id: PhysicalVectorId) -> CodegenResult<&'a PhysicalVectorGlue> {
        self.module
            .vector_glue
            .get(id.0 as usize)
            .filter(|glue| glue.id == id)
            .ok_or_else(|| {
                CodegenError::FailClosed(format!("unknown physical vector glue {}", id.0))
            })
    }

    fn variant_glue(&self, id: PhysicalVariantId) -> CodegenResult<&'a PhysicalVariantGlue> {
        self.module
            .variant_glue
            .get(id.0 as usize)
            .filter(|glue| glue.id == id)
            .ok_or_else(|| {
                CodegenError::FailClosed(format!("unknown physical variant glue {}", id.0))
            })
    }

    fn variant_layout(&self, ty: &ResolvedTy) -> CodegenResult<&'a PhysicalVariantLayout> {
        self.module.target.variant_layout(ty).ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "physical variant `{}` has no target layout",
                ty.user_facing()
            ))
        })
    }
}

fn vector_descriptor_symbol(id: PhysicalVectorId) -> String {
    format!("__hew_vector_element_layout_{}", id.0)
}

fn vector_descriptor_type<'ctx>(
    ctx: &'ctx Context,
    target: &TargetData,
) -> inkwell::types::StructType<'ctx> {
    let size_ty = ctx.ptr_sized_int_type(target, None);
    let pointer = ctx.ptr_type(AddressSpace::default());
    // HewVecElemLayout's C layout is realized for the selected target,
    // including padding around its u8 ownership discriminant.
    ctx.struct_type(
        &[
            size_ty.into(),
            size_ty.into(),
            ctx.i8_type().into(),
            pointer.into(),
            pointer.into(),
        ],
        false,
    )
}

fn build_module<'ctx>(
    ctx: &'ctx Context,
    physical: &PhysicalModule,
    name: &str,
    machine: &TargetMachine,
) -> CodegenResult<Module<'ctx>> {
    let triple = machine.get_triple();
    let triple_text = triple.as_str().to_string_lossy();
    if triple_text != physical.target.triple {
        return Err(CodegenError::FailClosed(format!(
            "LLVM machine `{triple_text}` disagrees with physical target `{}`",
            physical.target.triple
        )));
    }
    let target_data = machine.get_target_data();
    let data_layout = target_data.get_data_layout();
    let layout_text = data_layout.as_str().to_string_lossy();
    if layout_text != physical.target.data_layout {
        return Err(CodegenError::FailClosed(
            "LLVM data layout disagrees with verified physical MIR".into(),
        ));
    }
    let llvm = ctx.create_module(name);
    llvm.set_triple(&triple);
    llvm.set_data_layout(&data_layout);
    let mut emitter = ModuleEmitter {
        ctx,
        module: physical,
        llvm,
        functions: BTreeMap::new(),
    };
    emitter.declare_functions()?;
    emitter.emit_vector_descriptors()?;
    emitter.emit_functions()?;
    emitter.emit_entry()?;
    emitter
        .llvm
        .verify()
        .map_err(|error| CodegenError::LlvmVerify(error.to_string()))?;
    Ok(emitter.llvm)
}

impl<'ctx> ModuleEmitter<'ctx, '_> {
    fn emit_vector_descriptors(&self) -> CodegenResult<()> {
        let target = TargetData::create(&self.module.target.data_layout);
        let size_ty = self.ctx.ptr_sized_int_type(&target, None);
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let descriptor_ty = vector_descriptor_type(self.ctx, &target);
        for glue in &self.module.vector_glue {
            let layout = self.module.target.layout(&glue.element.ty).ok_or_else(|| {
                CodegenError::FailClosed("vector element has no target layout".into())
            })?;
            let clone = match glue.element.clone {
                Some(CloneAction::Bitwise) | None => pointer.const_null(),
                Some(action) => self.emit_vector_clone_callback(glue, layout, action)?,
            };
            let drop = match glue.element.destroy {
                None => pointer.const_null(),
                Some(action) => self.emit_vector_drop_callback(glue, layout, action)?,
            };
            let ownership = if glue.element.own == OwnKind::None {
                HewTypeOwnershipKind::Plain
            } else {
                HewTypeOwnershipKind::LayoutManaged
            };
            let value = descriptor_ty.const_named_struct(&[
                size_ty.const_int(layout.size, false).into(),
                size_ty.const_int(u64::from(layout.align), false).into(),
                self.ctx.i8_type().const_int(ownership as u64, false).into(),
                clone.into(),
                drop.into(),
            ]);
            let global =
                self.llvm
                    .add_global(descriptor_ty, None, &vector_descriptor_symbol(glue.id));
            global.set_linkage(Linkage::Internal);
            global.set_constant(true);
            global.set_initializer(&value);
        }
        Ok(())
    }

    fn emit_vector_clone_callback(
        &self,
        glue: &PhysicalVectorGlue,
        layout: &PhysicalLayout,
        action: CloneAction,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let function = self.llvm.add_function(
            &format!("__hew_vector_element_clone_{}", glue.id.0),
            self.ctx
                .i32_type()
                .fn_type(&[pointer.into(), pointer.into()], false),
            Some(Linkage::Internal),
        );
        let builder = self.ctx.create_builder();
        let entry = self.ctx.append_basic_block(function, "entry");
        let body = self.ctx.append_basic_block(function, "body");
        builder.position_at_end(entry);
        builder
            .build_unconditional_branch(body)
            .llvm_ctx("enter element clone")?;
        builder.position_at_end(body);
        let emitter = ValueEmitter {
            module: self.module,
            ctx: self.ctx,
            llvm: &self.llvm,
            builder: &builder,
            value: function,
        };
        let source = function
            .get_nth_param(0)
            .ok_or_else(|| CodegenError::FailClosed("element clone lacks source parameter".into()))?
            .into_pointer_value();
        let destination = function
            .get_nth_param(1)
            .ok_or_else(|| {
                CodegenError::FailClosed("element clone lacks destination parameter".into())
            })?
            .into_pointer_value();
        let original = builder
            .build_load(llvm_type(self.ctx, &layout.repr)?, source, "element.source")
            .llvm_ctx("load borrowed vector element")?;
        let cloned = emitter.clone_loaded_value(original, layout, action)?;
        builder
            .build_store(destination, cloned)
            .llvm_ctx("initialize copied vector element")?;
        builder
            .build_return(Some(&self.ctx.i32_type().const_zero()))
            .llvm_ctx("finish element clone")?;
        Ok(function.as_global_value().as_pointer_value())
    }

    fn emit_vector_drop_callback(
        &self,
        glue: &PhysicalVectorGlue,
        layout: &PhysicalLayout,
        action: DestroyAction,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let function = self.llvm.add_function(
            &format!("__hew_vector_element_drop_{}", glue.id.0),
            self.ctx.void_type().fn_type(&[pointer.into()], false),
            Some(Linkage::Internal),
        );
        let builder = self.ctx.create_builder();
        let entry = self.ctx.append_basic_block(function, "entry");
        let body = self.ctx.append_basic_block(function, "body");
        builder.position_at_end(entry);
        builder
            .build_unconditional_branch(body)
            .llvm_ctx("enter element destruction")?;
        builder.position_at_end(body);
        let emitter = ValueEmitter {
            module: self.module,
            ctx: self.ctx,
            llvm: &self.llvm,
            builder: &builder,
            value: function,
        };
        let source = function
            .get_nth_param(0)
            .ok_or_else(|| CodegenError::FailClosed("element drop lacks source parameter".into()))?
            .into_pointer_value();
        let value = builder
            .build_load(llvm_type(self.ctx, &layout.repr)?, source, "element.owner")
            .llvm_ctx("load owned vector element")?;
        emitter.destroy_loaded_value(value, layout, action)?;
        // Destruction releases the value's children. The caller owns the slot.
        builder
            .build_return(None)
            .llvm_ctx("finish element destruction")?;
        Ok(function.as_global_value().as_pointer_value())
    }

    fn declare_functions(&mut self) -> CodegenResult<()> {
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        for callable in &self.module.callables {
            let mut params = callable
                .params
                .iter()
                .map(|param| match param.carrier {
                    ParamCarrier::Direct => llvm_type(self.ctx, &param.layout.repr).map(Into::into),
                    ParamCarrier::Indirect => Ok(ptr.into()),
                })
                .collect::<CodegenResult<Vec<BasicMetadataTypeEnum<'ctx>>>>()?;
            if callable.return_layout.is_some() {
                params.push(ptr.into());
            }
            params.push(ptr.into());
            let function_type = self.ctx.i32_type().fn_type(&params, false);
            let symbol = emitted_symbol(self.module, callable);
            let function = self.llvm.add_function(&symbol, function_type, None);
            self.functions.insert(callable.id, function);
        }
        Ok(())
    }

    fn emit_functions(&self) -> CodegenResult<()> {
        for function in &self.module.functions {
            let callable = callable(self.module, function.callable)?;
            let value = *self.functions.get(&function.callable).ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "physical callable {} has no LLVM declaration",
                    function.callable.0
                ))
            })?;
            FunctionEmitter::new(self, function, callable, value)?.emit()?;
        }
        Ok(())
    }

    fn emit_entry(&self) -> CodegenResult<()> {
        let Some(entry_id) = self.module.entry_callable else {
            return Ok(());
        };
        let plan = self.module.entry_exit_plan.as_ref().ok_or_else(|| {
            CodegenError::FailClosed("physical executable entry has no typed exit plan".into())
        })?;
        let callable = callable(self.module, entry_id)?;
        if !callable.params.is_empty() {
            return Err(CodegenError::FailClosed(
                "physical process entry must be parameterless".into(),
            ));
        }
        let body = *self.functions.get(&entry_id).ok_or_else(|| {
            CodegenError::FailClosed("physical process entry has no LLVM body".into())
        })?;
        let wrapper = self.llvm.add_function(
            "main",
            self.ctx.i32_type().fn_type(&[], false),
            Some(Linkage::External),
        );
        let entry = self.ctx.append_basic_block(wrapper, "entry");
        let success = self.ctx.append_basic_block(wrapper, "success");
        let failure = self.ctx.append_basic_block(wrapper, "failure");
        let builder = self.ctx.create_builder();
        builder.position_at_end(entry);
        let result = if let Some(layout) = &callable.return_layout {
            Some(
                builder
                    .build_alloca(llvm_type(self.ctx, &layout.repr)?, "entry.result")
                    .llvm_ctx("allocate physical entry result")?,
            )
        } else {
            None
        };
        let fault = builder
            .build_alloca(self.ctx.ptr_type(AddressSpace::default()), "entry.fault")
            .llvm_ctx("allocate physical entry fault")?;
        builder
            .build_store(
                fault,
                self.ctx.ptr_type(AddressSpace::default()).const_null(),
            )
            .llvm_ctx("initialize physical entry fault")?;
        let mut args = Vec::<BasicMetadataValueEnum<'ctx>>::new();
        if let Some(result) = result {
            args.push(result.into());
        }
        args.push(fault.into());
        let status = builder
            .build_call(body, &args, "entry.status")
            .llvm_ctx("call physical process entry")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("physical body returned no status".into()))?
            .into_int_value();
        let ok = builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                self.ctx.i32_type().const_zero(),
                "entry.ok",
            )
            .llvm_ctx("compare physical entry status")?;
        builder
            .build_conditional_branch(ok, success, failure)
            .llvm_ctx("branch on physical entry status")?;

        builder.position_at_end(failure);
        let fault_value = builder
            .build_load(
                self.ctx.ptr_type(AddressSpace::default()),
                fault,
                "entry.fault.value",
            )
            .llvm_ctx("load physical entry fault")?
            .into_pointer_value();
        let report = external_fault_report(self.ctx, &self.llvm)?;
        builder
            .build_call(report, &[fault_value.into()], "entry.fault.report")
            .llvm_ctx("report physical entry fault")?;
        let drop = external_fault_drop(self.ctx, &self.llvm)?;
        builder
            .build_call(drop, &[fault_value.into()], "entry.fault.drop")
            .llvm_ctx("drop physical entry fault")?;
        builder
            .build_return(Some(&status))
            .llvm_ctx("return physical failure status")?;

        builder.position_at_end(success);
        let exit = emit_entry_success(self.ctx, &builder, result, plan.action.clone(), callable)?;
        builder
            .build_return(Some(&exit))
            .llvm_ctx("return physical process status")?;
        Ok(())
    }
}

impl<'a, 'ctx> FunctionEmitter<'a, 'ctx> {
    fn value_emitter(&self) -> ValueEmitter<'_, 'ctx> {
        ValueEmitter {
            module: self.module,
            ctx: self.ctx,
            llvm: self.llvm,
            builder: &self.builder,
            value: self.value,
        }
    }

    fn new(
        module: &'a ModuleEmitter<'ctx, '_>,
        function: &'a PhysicalFunction,
        callable: &PhysicalCallable,
        value: FunctionValue<'ctx>,
    ) -> CodegenResult<Self> {
        let ctx = module.ctx;
        let builder = ctx.create_builder();
        let prologue = ctx.append_basic_block(value, "physical.prologue");
        builder.position_at_end(prologue);
        let slots = function
            .storage
            .iter()
            .map(|storage| {
                builder
                    .build_alloca(
                        llvm_type(ctx, &storage.layout.repr)?,
                        &format!("s{}", storage.id.0),
                    )
                    .llvm_ctx("allocate physical storage")
            })
            .collect::<CodegenResult<Vec<_>>>()?;
        let active_fault = builder
            .build_alloca(ctx.ptr_type(AddressSpace::default()), "active.fault")
            .llvm_ctx("allocate active fault")?;
        let active_status = builder
            .build_alloca(ctx.i32_type(), "active.status")
            .llvm_ctx("allocate active status")?;
        builder
            .build_store(
                active_fault,
                ctx.ptr_type(AddressSpace::default()).const_null(),
            )
            .llvm_ctx("initialize active fault")?;
        builder
            .build_store(active_status, ctx.i32_type().const_zero())
            .llvm_ctx("initialize active status")?;

        let mut param_index = 0u32;
        for ((parameter, storage_id), physical_param) in value
            .get_params()
            .into_iter()
            .zip(&function.parameters)
            .zip(&callable.params)
        {
            let loaded = match physical_param.carrier {
                ParamCarrier::Direct => parameter,
                ParamCarrier::Indirect => builder
                    .build_load(
                        llvm_type(ctx, &physical_param.layout.repr)?,
                        parameter.into_pointer_value(),
                        "param.indirect",
                    )
                    .llvm_ctx("load indirect physical parameter")?,
            };
            builder
                .build_store(slots[storage_id.0 as usize], loaded)
                .llvm_ctx("store physical parameter")?;
            param_index += 1;
        }
        let result_out = if callable.return_layout.is_some() {
            let result = value
                .get_nth_param(param_index)
                .ok_or_else(|| CodegenError::FailClosed("missing result-out parameter".into()))?
                .into_pointer_value();
            param_index += 1;
            Some(result)
        } else {
            None
        };
        let fault_out = value
            .get_nth_param(param_index)
            .ok_or_else(|| CodegenError::FailClosed("missing fault-out parameter".into()))?
            .into_pointer_value();
        let blocks = function
            .blocks
            .iter()
            .map(|block| {
                (
                    block.id,
                    ctx.append_basic_block(value, &format!("bb{}", block.id.0)),
                )
            })
            .collect::<BTreeMap<_, _>>();
        builder
            .build_unconditional_branch(blocks[&function.entry])
            .llvm_ctx("branch to physical entry")?;
        Ok(Self {
            module: module.module,
            function,
            ctx,
            llvm: &module.llvm,
            builder,
            value,
            blocks,
            slots,
            result_out,
            fault_out,
            active_fault,
            active_status,
            functions: &module.functions,
        })
    }

    fn emit(self) -> CodegenResult<()> {
        for block in &self.function.blocks {
            self.builder.position_at_end(self.blocks[&block.id]);
            for operation in &block.ops {
                self.emit_op(operation)?;
            }
            self.emit_terminator(block)?;
        }
        Ok(())
    }

    fn load(&self, id: StorageId, name: &str) -> CodegenResult<BasicValueEnum<'ctx>> {
        self.builder
            .build_load(
                llvm_type(self.ctx, &self.storage(id)?.layout.repr)?,
                self.slots[id.0 as usize],
                name,
            )
            .llvm_ctx("load physical storage")
    }

    fn storage(&self, id: StorageId) -> CodegenResult<&PhysicalStorage> {
        self.function
            .storage
            .get(id.0 as usize)
            .filter(|storage| storage.id == id)
            .ok_or_else(|| CodegenError::FailClosed(format!("unknown physical storage {}", id.0)))
    }

    fn store(&self, id: StorageId, value: BasicValueEnum<'ctx>) -> CodegenResult<()> {
        self.builder
            .build_store(self.slots[id.0 as usize], value)
            .llvm_ctx("store physical storage")?;
        Ok(())
    }

    fn clear_owned(&self, id: StorageId) -> CodegenResult<()> {
        if self.storage(id)?.own == OwnKind::Owned {
            let zero = llvm_type(self.ctx, &self.storage(id)?.layout.repr)?.const_zero();
            self.store(id, zero)?;
        }
        Ok(())
    }

    #[allow(
        clippy::too_many_lines,
        reason = "the physical operation match is deliberately exhaustive and contains no ownership selection"
    )]
    fn emit_op(&self, operation: &PhysicalOp) -> CodegenResult<()> {
        match operation {
            PhysicalOp::Const { dest, value } => self.emit_const(*dest, value),
            PhysicalOp::Unary { dest, op, source } => {
                let source_value = self.load(*source, "unary.source")?;
                let value = match op {
                    UnaryOp::Not => {
                        let source = source_value.into_int_value();
                        let logical = self
                            .builder
                            .build_int_compare(
                                IntPredicate::EQ,
                                source,
                                source.get_type().const_zero(),
                                "logical.not",
                            )
                            .llvm_ctx("emit physical logical not")?;
                        let target =
                            llvm_type(self.ctx, &self.storage(*dest)?.layout.repr)?.into_int_type();
                        self.builder
                            .build_int_z_extend(logical, target, "logical.not.widen")
                            .llvm_ctx("widen physical logical not")?
                            .into()
                    }
                    UnaryOp::BitNot => self
                        .builder
                        .build_not(source_value.into_int_value(), "bitnot")
                        .llvm_ctx("emit physical bit not")?
                        .into(),
                    UnaryOp::Negate | UnaryOp::RawDeref => {
                        return Err(CodegenError::FailClosed(
                            "fallible or raw unary operation reached physical emitter".into(),
                        ));
                    }
                };
                self.store(*dest, value)
            }
            PhysicalOp::Binary { dest, op, lhs, rhs } => self.emit_binary(*dest, *op, *lhs, *rhs),
            PhysicalOp::Cast { dest, source, to } => self.emit_cast(*dest, *source, to),
            PhysicalOp::TupleMake { dest, elements } => {
                let BasicTypeEnum::StructType(tuple_ty) =
                    llvm_type(self.ctx, &self.storage(*dest)?.layout.repr)?
                else {
                    return Err(CodegenError::FailClosed(
                        "physical tuple destination is not an LLVM struct".into(),
                    ));
                };
                let mut aggregate = tuple_ty.get_undef();
                for (index, element) in elements.iter().enumerate() {
                    let field = self.load(*element, "tuple.field")?;
                    let index = u32::try_from(index).map_err(|_| {
                        CodegenError::FailClosed("physical tuple field index exceeds u32".into())
                    })?;
                    aggregate = match self
                        .builder
                        .build_insert_value(aggregate, field, index, "tuple.make")
                        .llvm_ctx("insert physical tuple field")?
                    {
                        inkwell::values::AggregateValueEnum::StructValue(value) => value,
                        inkwell::values::AggregateValueEnum::ArrayValue(_) => {
                            return Err(CodegenError::FailClosed(
                                "physical tuple insertion produced an LLVM array".into(),
                            ));
                        }
                    };
                }
                self.store(*dest, aggregate.into())
            }
            PhysicalOp::TupleGet { dest, tuple, index } => {
                let tuple = self.load(*tuple, "tuple.source")?.into_struct_value();
                let field = self
                    .builder
                    .build_extract_value(tuple, *index, "tuple.get")
                    .llvm_ctx("extract physical tuple field")?;
                self.store(*dest, field)
            }
            PhysicalOp::AggregateMake { dest, fields, .. } => {
                let BasicTypeEnum::StructType(aggregate_ty) =
                    llvm_type(self.ctx, &self.storage(*dest)?.layout.repr)?
                else {
                    return Err(CodegenError::FailClosed(
                        "physical aggregate destination is not an LLVM struct".into(),
                    ));
                };
                let mut aggregate = aggregate_ty.get_undef();
                for (index, field) in fields.iter().enumerate() {
                    let value = self.load(*field, "aggregate.field")?;
                    let index = u32::try_from(index).map_err(|_| {
                        CodegenError::FailClosed(
                            "physical aggregate field index exceeds u32".into(),
                        )
                    })?;
                    aggregate = match self
                        .builder
                        .build_insert_value(aggregate, value, index, "aggregate.make")
                        .llvm_ctx("insert physical aggregate field")?
                    {
                        inkwell::values::AggregateValueEnum::StructValue(value) => value,
                        inkwell::values::AggregateValueEnum::ArrayValue(_) => {
                            return Err(CodegenError::FailClosed(
                                "physical aggregate insertion produced an LLVM array".into(),
                            ));
                        }
                    };
                }
                self.store(*dest, aggregate.into())?;
                for field in fields {
                    self.clear_owned(*field)?;
                }
                Ok(())
            }
            PhysicalOp::AggregateProjectCopy {
                dest,
                aggregate,
                field,
                action,
                ..
            } => {
                let aggregate = self
                    .load(*aggregate, "aggregate.project.source")?
                    .into_struct_value();
                let field_value = self
                    .builder
                    .build_extract_value(aggregate, *field, "aggregate.project.field")
                    .llvm_ctx("extract physical aggregate field for copy")?;
                let value = self.value_emitter().clone_loaded_value(
                    field_value,
                    &self.storage(*dest)?.layout,
                    *action,
                )?;
                self.store(*dest, value)
            }
            PhysicalOp::AggregateDestructure {
                aggregate, fields, ..
            } => {
                let value = self
                    .load(*aggregate, "aggregate.destructure.source")?
                    .into_struct_value();
                for (index, field) in fields.iter().enumerate() {
                    let index = u32::try_from(index).map_err(|_| {
                        CodegenError::FailClosed(
                            "physical aggregate field index exceeds u32".into(),
                        )
                    })?;
                    let field_value = self
                        .builder
                        .build_extract_value(value, index, "aggregate.destructure.field")
                        .llvm_ctx("extract physical aggregate field")?;
                    self.store(*field, field_value)?;
                }
                self.clear_owned(*aggregate)
            }
            PhysicalOp::VariantMake {
                dest,
                variant,
                fields,
                glue,
            } => self.emit_variant_make(*dest, *variant, fields, *glue),
            PhysicalOp::Transfer { dest, source } => {
                let value = self.load(*source, "transfer")?;
                self.store(*dest, value)?;
                if dest != source {
                    self.clear_owned(*source)?;
                }
                Ok(())
            }
            PhysicalOp::Clone {
                dest,
                source,
                action,
            } => {
                let value = self.clone_value(*source, *action)?;
                self.store(*dest, value)
            }
            PhysicalOp::Destroy { source, action } => self.destroy_value(*source, *action),
            PhysicalOp::Borrow { dest, source } => {
                let value = self.load(*source, "borrow")?;
                self.store(*dest, value)
            }
            PhysicalOp::EndBorrow { .. } | PhysicalOp::StorageLive { .. } => Ok(()),
            PhysicalOp::Assign {
                dest,
                source,
                destroy_old,
            } => {
                self.destroy_value(*dest, *destroy_old)?;
                let value = self.load(*source, "assign")?;
                self.store(*dest, value)?;
                self.clear_owned(*source)
            }
            PhysicalOp::StorageDead { storage, destroy } => self.destroy_value(*storage, *destroy),
        }
    }

    fn emit_variant_make(
        &self,
        dest: StorageId,
        variant: u32,
        fields: &[StorageId],
        glue_id: PhysicalVariantId,
    ) -> CodegenResult<()> {
        let values = fields
            .iter()
            .map(|field| self.load(*field, "variant.make.field"))
            .collect::<CodegenResult<Vec<_>>>()?;
        self.write_variant_value(self.slots[dest.0 as usize], variant, &values, glue_id)?;
        for field in fields {
            self.clear_owned(*field)?;
        }
        Ok(())
    }

    fn write_variant_value(
        &self,
        destination: PointerValue<'ctx>,
        variant: u32,
        fields: &[BasicValueEnum<'ctx>],
        glue_id: PhysicalVariantId,
    ) -> CodegenResult<()> {
        let glue = self.value_emitter().variant_glue(glue_id)?;
        let layout = self.value_emitter().variant_layout(&glue.ty)?;
        if layout.is_indirect {
            return Err(CodegenError::FailClosed(
                "physical indirect variant construction is not yet admitted".into(),
            ));
        }
        let object_ty = llvm_type(self.ctx, &layout.object.repr)?.into_struct_type();
        let tag_ty = object_ty
            .get_field_type_at_index(0)
            .ok_or_else(|| CodegenError::FailClosed("variant object has no tag field".into()))?
            .into_int_type();
        let tag = tag_ty.const_int(u64::from(variant), false);
        let object = self
            .builder
            .build_insert_value(object_ty.const_zero(), tag, 0, "variant.make.tag")
            .llvm_ctx("write physical variant tag")?
            .into_struct_value();
        self.builder
            .build_store(destination, object)
            .llvm_ctx("initialize physical variant storage")?;
        let case = glue.variants.get(variant as usize).ok_or_else(|| {
            CodegenError::FailClosed("variant construction tag is invalid".into())
        })?;
        let payload_layout = layout
            .variants
            .get(variant as usize)
            .ok_or_else(|| CodegenError::FailClosed("variant payload layout is absent".into()))?;
        if case.fields.len() != fields.len() {
            return Err(CodegenError::FailClosed(
                "variant construction field count changed after verification".into(),
            ));
        }
        if !fields.is_empty() {
            let payload_ty = llvm_type(self.ctx, &payload_layout.repr)?.into_struct_type();
            let mut payload = payload_ty.get_undef();
            for (index, field) in fields.iter().enumerate() {
                let value = *field;
                let index = u32::try_from(index).map_err(|_| {
                    CodegenError::FailClosed("variant field index exceeds u32".into())
                })?;
                payload = self
                    .builder
                    .build_insert_value(payload, value, index, "variant.make.payload")
                    .llvm_ctx("write physical variant payload field")?
                    .into_struct_value();
            }
            let payload_ptr = self
                .value_emitter()
                .variant_payload_ptr(destination, layout)?;
            self.builder
                .build_store(payload_ptr, payload)
                .llvm_ctx("store physical variant payload")?;
        }
        Ok(())
    }

    fn emit_variant_switch(
        &self,
        scrutinee: StorageId,
        glue_id: PhysicalVariantId,
        arms: &[PhysicalVariantArm],
    ) -> CodegenResult<()> {
        let glue = self.value_emitter().variant_glue(glue_id)?;
        let layout = self.value_emitter().variant_layout(&glue.ty)?;
        if layout.is_indirect {
            return Err(CodegenError::FailClosed(
                "physical indirect variant switch is not yet admitted".into(),
            ));
        }
        let object = self
            .load(scrutinee, "variant.switch.source")?
            .into_struct_value();
        let tag = self
            .builder
            .build_extract_value(object, 0, "variant.switch.tag")
            .llvm_ctx("read physical variant tag")?
            .into_int_value();
        let invalid = self.ctx.append_basic_block(self.value, "variant.invalid");
        let arm_blocks = arms
            .iter()
            .map(|arm| {
                (
                    tag.get_type().const_int(u64::from(arm.variant), false),
                    self.ctx
                        .append_basic_block(self.value, &format!("variant.case.{}", arm.variant)),
                )
            })
            .collect::<Vec<_>>();
        self.builder
            .build_switch(tag, invalid, &arm_blocks)
            .llvm_ctx("emit physical variant switch")?;
        for (arm, (_, arm_block)) in arms.iter().zip(&arm_blocks) {
            self.builder.position_at_end(*arm_block);
            let payload_layout = &layout.variants[arm.variant as usize];
            if !arm.fields.is_empty() {
                let payload_ty = llvm_type(self.ctx, &payload_layout.repr)?.into_struct_type();
                let payload_ptr = self
                    .value_emitter()
                    .variant_payload_ptr(self.slots[scrutinee.0 as usize], layout)?;
                let payload = self
                    .builder
                    .build_load(payload_ty, payload_ptr, "variant.switch.payload")
                    .llvm_ctx("load physical variant payload")?
                    .into_struct_value();
                for (index, field) in arm.fields.iter().enumerate() {
                    let index = u32::try_from(index).map_err(|_| {
                        CodegenError::FailClosed("variant field index exceeds u32".into())
                    })?;
                    let value = self
                        .builder
                        .build_extract_value(payload, index, "variant.switch.field")
                        .llvm_ctx("extract physical variant payload field")?;
                    self.store(*field, value)?;
                }
            }
            self.clear_owned(scrutinee)?;
            self.emit_edge(&arm.target)?;
        }
        self.builder.position_at_end(invalid);
        self.value_emitter().emit_invalid_variant_tag()
    }

    fn emit_const(&self, dest: StorageId, value: &PhysicalConst) -> CodegenResult<()> {
        let llvm_ty = llvm_type(self.ctx, &self.storage(dest)?.layout.repr)?;
        match value {
            PhysicalConst::I64(value) | PhysicalConst::Duration(value) => self.store(
                dest,
                llvm_ty
                    .into_int_type()
                    .const_int(*value as u64, true)
                    .into(),
            ),
            PhysicalConst::Bool(value) => self.store(
                dest,
                llvm_ty
                    .into_int_type()
                    .const_int(u64::from(*value), false)
                    .into(),
            ),
            PhysicalConst::F64(value) => {
                self.store(dest, llvm_ty.into_float_type().const_float(*value).into())
            }
            PhysicalConst::Char(value) => self.store(
                dest,
                llvm_ty
                    .into_int_type()
                    .const_int(u64::from(u32::from(*value)), false)
                    .into(),
            ),
            PhysicalConst::Unit => self.store(dest, llvm_ty.const_zero()),
            PhysicalConst::String(id) => {
                let bytes = self.module.string_literals.get(id).ok_or_else(|| {
                    CodegenError::FailClosed(format!("missing physical string literal {}", id.0))
                })?;
                self.emit_literal(dest, bytes.as_bytes(), "hew_string_literal_new")
            }
            PhysicalConst::Bytes(id) => {
                let bytes = self.module.bytes_literals.get(id).ok_or_else(|| {
                    CodegenError::FailClosed(format!("missing physical bytes literal {}", id.0))
                })?;
                self.emit_literal(dest, bytes, "hew_bytes_literal_new")
            }
        }
    }

    fn emit_literal(&self, dest: StorageId, bytes: &[u8], symbol: &str) -> CodegenResult<()> {
        let len = u32::try_from(bytes.len()).map_err(|_| {
            CodegenError::FailClosed(format!("{symbol} literal exceeds the u32 runtime ABI"))
        })?;
        let data = self.ctx.const_string(bytes, false);
        let global = self
            .llvm
            .add_global(data.get_type(), None, "physical.literal");
        global.set_initializer(&data);
        global.set_constant(true);
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let function = get_or_declare_external(
            self.llvm,
            symbol,
            self.ctx
                .void_type()
                .fn_type(&[ptr.into(), self.ctx.i32_type().into(), ptr.into()], false),
        )?;
        self.builder
            .build_call(
                function,
                &[
                    global.as_pointer_value().into(),
                    self.ctx.i32_type().const_int(u64::from(len), false).into(),
                    self.slots[dest.0 as usize].into(),
                ],
                "literal.new",
            )
            .llvm_ctx("create owned physical literal")?;
        Ok(())
    }

    fn emit_binary(
        &self,
        dest: StorageId,
        op: BinaryOp,
        lhs: StorageId,
        rhs: StorageId,
    ) -> CodegenResult<()> {
        let left = self.load(lhs, "binary.left")?;
        let right = self.load(rhs, "binary.right")?;
        let ty = &self.storage(lhs)?.ty;
        let mut value: BasicValueEnum<'ctx> = match (left, right) {
            (BasicValueEnum::IntValue(left), BasicValueEnum::IntValue(right)) => {
                emit_integer_binary(&self.builder, op, left, right, is_signed(ty))?.into()
            }
            (BasicValueEnum::FloatValue(left), BasicValueEnum::FloatValue(right)) => {
                emit_float_binary(&self.builder, op, left, right)?.into()
            }
            _ => {
                return Err(CodegenError::FailClosed(
                    "physical binary operands have unsupported carriers".into(),
                ));
            }
        };
        if self.storage(dest)?.ty == ResolvedTy::Bool {
            let int = value.into_int_value();
            let bool_ty = llvm_type(self.ctx, &self.storage(dest)?.layout.repr)?.into_int_type();
            if int.get_type() != bool_ty {
                value = self
                    .builder
                    .build_int_z_extend(int, bool_ty, "bool.widen")
                    .llvm_ctx("widen physical boolean result")?
                    .into();
            }
        }
        self.store(dest, value)
    }

    fn emit_cast(&self, dest: StorageId, source: StorageId, to: &ResolvedTy) -> CodegenResult<()> {
        let value = self.load(source, "cast.source")?;
        let target = llvm_type(self.ctx, &self.storage(dest)?.layout.repr)?;
        let source_ty = &self.storage(source)?.ty;
        let cast = match (value, target) {
            (BasicValueEnum::IntValue(value), BasicTypeEnum::IntType(target)) => self
                .builder
                .build_int_cast_sign_flag(value, target, is_signed(source_ty), "int.cast")
                .llvm_ctx("emit physical integer cast")?
                .into(),
            (BasicValueEnum::FloatValue(value), BasicTypeEnum::FloatType(target)) => self
                .builder
                .build_float_cast(value, target, "float.cast")
                .llvm_ctx("emit physical float cast")?
                .into(),
            (BasicValueEnum::IntValue(value), BasicTypeEnum::FloatType(target)) => {
                if is_signed(source_ty) {
                    self.builder
                        .build_signed_int_to_float(value, target, "signed.to.float")
                        .llvm_ctx("emit signed integer-to-float cast")?
                        .into()
                } else {
                    self.builder
                        .build_unsigned_int_to_float(value, target, "unsigned.to.float")
                        .llvm_ctx("emit unsigned integer-to-float cast")?
                        .into()
                }
            }
            (BasicValueEnum::FloatValue(value), BasicTypeEnum::IntType(target)) => {
                if is_signed(to) {
                    self.builder
                        .build_float_to_signed_int(value, target, "float.to.signed")
                        .llvm_ctx("emit float-to-signed cast")?
                        .into()
                } else {
                    self.builder
                        .build_float_to_unsigned_int(value, target, "float.to.unsigned")
                        .llvm_ctx("emit float-to-unsigned cast")?
                        .into()
                }
            }
            _ => {
                return Err(CodegenError::FailClosed(
                    "physical cast has unsupported carriers".into(),
                ));
            }
        };
        self.store(dest, cast)
    }

    fn clone_value(
        &self,
        source: StorageId,
        action: CloneAction,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let value = self.load(source, "clone.source")?;
        self.value_emitter()
            .clone_loaded_value(value, &self.storage(source)?.layout, action)
    }

    fn destroy_value(&self, source: StorageId, action: DestroyAction) -> CodegenResult<()> {
        let value = self.load(source, "destroy.source")?;
        self.value_emitter()
            .destroy_loaded_value(value, &self.storage(source)?.layout, action)?;
        let zero = llvm_type(self.ctx, &self.storage(source)?.layout.repr)?.const_zero();
        self.store(source, zero)
    }

    fn emit_terminator(&self, block: &PhysicalBlock) -> CodegenResult<()> {
        match &block.terminator {
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
            PhysicalTerminator::Call {
                callee,
                args,
                result,
                normal,
                unwind,
            } => self.emit_call(*callee, args, *result, normal, unwind.as_ref()),
            PhysicalTerminator::RuntimeCall {
                action,
                args,
                result,
                normal,
                failure,
            } => self.emit_runtime_call(*action, args, *result, normal, failure.as_ref()),
            PhysicalTerminator::Trap(kind) => {
                let code = match kind {
                    TrapKind::IntegerOverflow => HEW_TRAP_INTEGER_OVERFLOW,
                    TrapKind::DivideByZero => HEW_TRAP_DIVIDE_BY_ZERO,
                    TrapKind::SignedMinDivNegOne => HEW_TRAP_SIGNED_MIN_DIV_NEG_ONE,
                    TrapKind::ShiftOutOfRange => HEW_TRAP_SHIFT_OUT_OF_RANGE,
                    TrapKind::IndexOutOfBounds => HEW_TRAP_INDEX_OUT_OF_BOUNDS,
                };
                self.emit_new_fault(code)
            }
            PhysicalTerminator::PropagateFault => self.emit_propagate_fault(),
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
        self.builder
            .build_return(Some(&self.ctx.i32_type().const_zero()))
            .llvm_ctx("return physical success status")?;
        Ok(())
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
                self.emit_edge(normal)
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

    fn emit_checked_choice(
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
        self.emit_edge(normal)
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
        self.emit_edge(normal)
    }

    fn emit_edge(&self, edge: &PhysicalEdge) -> CodegenResult<()> {
        let values = edge
            .transfers
            .iter()
            .map(|(source, _)| self.load(*source, "edge.value"))
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
        normal: &PhysicalEdge,
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
                ArgumentTransfer::Borrow(source) => (*source, None),
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
        self.emit_edge(normal)?;
        self.builder.position_at_end(failure);
        if let Some(unwind) = unwind {
            self.emit_edge(unwind)
        } else {
            self.emit_propagate_fault()
        }
    }

    #[allow(
        clippy::too_many_lines,
        reason = "the closed match is the physical runtime ABI authority"
    )]
    fn emit_runtime_call(
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
        let result = || {
            result.ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "physical runtime action {action:?} lacks result storage"
                ))
            })
        };
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        match action {
            PhysicalRuntimeAction::Vector { operation, glue } => {
                return self.emit_vector_call(
                    (operation, glue),
                    transfers,
                    result()?,
                    normal,
                    failure,
                );
            }
            PhysicalRuntimeAction::BytesDecodeUtf8 {
                result: result_glue,
                error,
                error_len,
            } => {
                self.emit_utf8_decode(source(0)?, result()?, result_glue, error, error_len)?;
            }
            PhysicalRuntimeAction::BytesDecodeUtf8Lossy => {
                let function =
                    external_unary_ptr(self.ctx, self.llvm, "hew_bytes_decode_utf8_lossy")?;
                let value = self.runtime_call_value(
                    function,
                    &[self.slots[source(0)?.0 as usize].into()],
                    "bytes.decode.utf8.lossy",
                )?;
                self.store(result()?, value)?;
            }
            PhysicalRuntimeAction::StringConcat => {
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_string_concat",
                    ptr.fn_type(&[ptr.into(), ptr.into()], false),
                )?;
                let value = self.runtime_call_value(
                    function,
                    &[
                        self.load(source(0)?, "concat.left")?.into(),
                        self.load(source(1)?, "concat.right")?.into(),
                    ],
                    "string.concat",
                )?;
                self.store(result()?, value)?;
            }
            PhysicalRuntimeAction::StringEquals => {
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_string_equals",
                    self.ctx
                        .i32_type()
                        .fn_type(&[ptr.into(), ptr.into()], false),
                )?;
                let value = self
                    .runtime_call_value(
                        function,
                        &[
                            self.load(source(0)?, "equals.left")?.into(),
                            self.load(source(1)?, "equals.right")?.into(),
                        ],
                        "string.equals",
                    )?
                    .into_int_value();
                let truth = self
                    .builder
                    .build_int_compare(
                        IntPredicate::NE,
                        value,
                        self.ctx.i32_type().const_zero(),
                        "string.equals.truth",
                    )
                    .llvm_ctx("normalize string equality")?;
                let dest = result()?;
                let bool_ty =
                    llvm_type(self.ctx, &self.storage(dest)?.layout.repr)?.into_int_type();
                let truth = self
                    .builder
                    .build_int_z_extend(truth, bool_ty, "string.equals.bool")
                    .llvm_ctx("widen string equality result")?;
                self.store(dest, truth.into())?;
            }
            PhysicalRuntimeAction::StringToBytesOwned => {
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
                        self.slots[result()?.0 as usize].into(),
                    ],
                    "string.to.bytes",
                )?;
            }
            PhysicalRuntimeAction::StringToUppercase => {
                let function = external_unary_ptr(self.ctx, self.llvm, "hew_string_to_uppercase")?;
                let value = self.runtime_call_value(
                    function,
                    &[self.load(source(0)?, "uppercase.input")?.into()],
                    "string.uppercase",
                )?;
                self.store(result()?, value)?;
            }
            PhysicalRuntimeAction::StringLen | PhysicalRuntimeAction::StringByteLen => {
                let symbol = if action == PhysicalRuntimeAction::StringLen {
                    "hew_string_length"
                } else {
                    "hew_string_byte_length"
                };
                let function = get_or_declare_external(
                    self.llvm,
                    symbol,
                    self.ctx.i64_type().fn_type(&[ptr.into()], false),
                )?;
                let value = self.runtime_call_value(
                    function,
                    &[self.load(source(0)?, "string.length.input")?.into()],
                    "string.length",
                )?;
                self.store(result()?, value)?;
            }
            PhysicalRuntimeAction::U8ToString => {
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_u8_to_string",
                    ptr.fn_type(&[self.ctx.i8_type().into()], false),
                )?;
                let value = self.runtime_call_value(
                    function,
                    &[self.load(source(0)?, "u8.to.string.input")?.into()],
                    "u8.to.string",
                )?;
                self.store(result()?, value)?;
            }
            PhysicalRuntimeAction::I64ToString => {
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_i64_to_string",
                    ptr.fn_type(&[self.ctx.i64_type().into()], false),
                )?;
                let value = self.runtime_call_value(
                    function,
                    &[self.load(source(0)?, "i64.to.string.input")?.into()],
                    "i64.to.string",
                )?;
                self.store(result()?, value)?;
            }
            PhysicalRuntimeAction::PrintlnI64 => {
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
                        self.ctx
                            .i8_type()
                            .const_int(HEW_PRINT_KIND_I64, false)
                            .into(),
                        self.load(source(0)?, "println.i64.bits")?.into(),
                        self.ctx.bool_type().const_int(1, false).into(),
                    ],
                    "println.i64",
                )?;
            }
            PhysicalRuntimeAction::PrintlnString => {
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_println_str",
                    self.ctx.void_type().fn_type(&[ptr.into()], false),
                )?;
                self.runtime_call_void(
                    function,
                    &[self.load(source(0)?, "println.input")?.into()],
                    "println.string",
                )?;
            }
            PhysicalRuntimeAction::BytesLen => {
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_bytes_len",
                    self.ctx.i64_type().fn_type(&[ptr.into()], false),
                )?;
                let value = self.runtime_call_value(
                    function,
                    &[self.slots[source(0)?.0 as usize].into()],
                    "bytes.len",
                )?;
                self.store(result()?, value)?;
            }
            PhysicalRuntimeAction::BytesIndex => {
                return self.emit_bytes_index(
                    source(0)?,
                    source(1)?,
                    result()?,
                    normal,
                    failure.ok_or_else(|| {
                        CodegenError::FailClosed(
                            "physical bytes index lacks its cleanup failure edge".into(),
                        )
                    })?,
                );
            }
            PhysicalRuntimeAction::BytesPushOwned => {
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
                        self.slots[result()?.0 as usize].into(),
                    ],
                    "bytes.push.owned",
                )?;
            }
        }
        if failure.is_some() {
            return Err(CodegenError::FailClosed(format!(
                "infallible physical runtime action {action:?} carries a failure edge"
            )));
        }
        self.emit_edge(normal)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "each vector action executes its checked storage and failure contract"
    )]
    fn emit_vector_call(
        &self,
        action: (PhysicalVectorOp, PhysicalVectorId),
        transfers: &[ArgumentTransfer],
        result: StorageId,
        normal: &PhysicalEdge,
        failure: Option<&PhysicalEdge>,
    ) -> CodegenResult<()> {
        let (operation, glue_id) = action;
        let values = self.value_emitter();
        let glue = values.vector_glue(glue_id)?;
        let source = |index: usize| {
            transfers.get(index).map(argument_source).ok_or_else(|| {
                CodegenError::FailClosed(format!("vector action lacks argument {index}"))
            })
        };
        let failure = || {
            failure.ok_or_else(|| {
                CodegenError::FailClosed("fallible vector action lacks its cleanup edge".into())
            })
        };
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let i64_ty = self.ctx.i64_type();
        if operation == PhysicalVectorOp::New {
            let descriptor = self
                .llvm
                .get_global(&vector_descriptor_symbol(glue_id))
                .ok_or_else(|| {
                    CodegenError::FailClosed("vector descriptor was not emitted".into())
                })?;
            let function = external_unary_ptr(self.ctx, self.llvm, "hew_vec_new_with_elem_layout")?;
            let value = self.runtime_call_value(
                function,
                &[descriptor.as_pointer_value().into()],
                "vector.new",
            )?;
            self.store(result, value)?;
            return self.emit_edge(normal);
        }
        let receiver = source(0)?;
        let vector = self.load(receiver, "vector.receiver")?.into_pointer_value();
        match operation {
            PhysicalVectorOp::New => unreachable!("new handled before receiver loading"),
            PhysicalVectorOp::Len => {
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_vec_len",
                    i64_ty.fn_type(&[pointer.into()], false),
                )?;
                let length =
                    self.runtime_call_value(function, &[vector.into()], "vector.length")?;
                self.store(result, length)?;
            }
            PhysicalVectorOp::Push | PhysicalVectorOp::Clear => {
                if operation == PhysicalVectorOp::Push {
                    let function = get_or_declare_external(
                        self.llvm,
                        "hew_vec_push_owned",
                        self.ctx
                            .void_type()
                            .fn_type(&[pointer.into(), pointer.into()], false),
                    )?;
                    self.runtime_call_void(
                        function,
                        &[vector.into(), self.slots[source(1)?.0 as usize].into()],
                        "vector.push",
                    )?;
                } else {
                    let function = external_drop(self.ctx, self.llvm, "hew_vec_clear")?;
                    self.runtime_call_void(function, &[vector.into()], "vector.clear")?;
                }
                self.clear_owned(receiver)?;
                self.store(result, vector.into())?;
            }
            PhysicalVectorOp::Set => {
                let index = self.load(source(1)?, "vector.set.index")?.into_int_value();
                let length_fn = get_or_declare_external(
                    self.llvm,
                    "hew_vec_len",
                    i64_ty.fn_type(&[pointer.into()], false),
                )?;
                let length = self
                    .runtime_call_value(length_fn, &[vector.into()], "vector.set.length")?
                    .into_int_value();
                // Unsigned comparison also rejects every negative signed index.
                let in_bounds = self
                    .builder
                    .build_int_compare(IntPredicate::ULT, index, length, "vector.set.in.bounds")
                    .llvm_ctx("check vector replacement bounds")?;
                let safe = self.ctx.append_basic_block(self.value, "vector.set.safe");
                let failed = self.ctx.append_basic_block(self.value, "vector.set.failed");
                self.builder
                    .build_conditional_branch(in_bounds, safe, failed)
                    .llvm_ctx("select vector replacement outcome")?;
                self.builder.position_at_end(failed);
                self.release_failed_vector_receiver(receiver, vector, glue_id)?;
                self.emit_edge(failure()?)?;
                self.builder.position_at_end(safe);
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_vec_set_owned",
                    self.ctx
                        .void_type()
                        .fn_type(&[pointer.into(), i64_ty.into(), pointer.into()], false),
                )?;
                self.runtime_call_void(
                    function,
                    &[
                        vector.into(),
                        index.into(),
                        self.slots[source(2)?.0 as usize].into(),
                    ],
                    "vector.set",
                )?;
                self.clear_owned(receiver)?;
                self.store(result, vector.into())?;
            }
            PhysicalVectorOp::Index | PhysicalVectorOp::Get { .. } => {
                let element_layout =
                    self.module.target.layout(&glue.element.ty).ok_or_else(|| {
                        CodegenError::FailClosed("vector element lacks its target layout".into())
                    })?;
                let element_ty = llvm_type(self.ctx, &element_layout.repr)?;
                let output = if operation == PhysicalVectorOp::Index {
                    self.slots[result.0 as usize]
                } else {
                    values.entry_scratch(element_ty, "vector.get.element")?
                };
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_vec_get_clone",
                    self.ctx
                        .bool_type()
                        .fn_type(&[pointer.into(), i64_ty.into(), pointer.into()], false),
                )?;
                let found = self
                    .runtime_call_value(
                        function,
                        &[
                            vector.into(),
                            self.load(source(1)?, "vector.index")?.into(),
                            output.into(),
                        ],
                        "vector.found",
                    )?
                    .into_int_value();
                let present = self
                    .ctx
                    .append_basic_block(self.value, "vector.element.present");
                let absent = self
                    .ctx
                    .append_basic_block(self.value, "vector.element.absent");
                self.builder
                    .build_conditional_branch(found, present, absent)
                    .llvm_ctx("select vector read outcome")?;
                self.builder.position_at_end(absent);
                if let PhysicalVectorOp::Get { result: option } = operation {
                    self.write_variant_value(self.slots[result.0 as usize], 1, &[], option)?;
                    self.emit_edge(normal)?;
                } else {
                    self.emit_edge(failure()?)?;
                }
                self.builder.position_at_end(present);
                if let PhysicalVectorOp::Get { result: option } = operation {
                    let element = self
                        .builder
                        .build_load(element_ty, output, "vector.get.value")
                        .llvm_ctx("load independent vector element")?;
                    self.write_variant_value(self.slots[result.0 as usize], 0, &[element], option)?;
                }
            }
            PhysicalVectorOp::Pop { result: tuple } => {
                values.aggregate_glue(tuple)?;
                let layout = self.module.target.layout(&glue.element.ty).ok_or_else(|| {
                    CodegenError::FailClosed("vector element lacks its target layout".into())
                })?;
                let element_ty = llvm_type(self.ctx, &layout.repr)?;
                let output = values.entry_scratch(element_ty, "vector.pop.element")?;
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_vec_pop_owned",
                    self.ctx
                        .i32_type()
                        .fn_type(&[pointer.into(), pointer.into()], false),
                )?;
                let status = self
                    .runtime_call_value(
                        function,
                        &[vector.into(), output.into()],
                        "vector.pop.status",
                    )?
                    .into_int_value();
                let found = self
                    .builder
                    .build_int_compare(
                        IntPredicate::NE,
                        status,
                        self.ctx.i32_type().const_zero(),
                        "vector.pop.found",
                    )
                    .llvm_ctx("test vector pop outcome")?;
                let present = self
                    .ctx
                    .append_basic_block(self.value, "vector.pop.present");
                let absent = self.ctx.append_basic_block(self.value, "vector.pop.empty");
                self.builder
                    .build_conditional_branch(found, present, absent)
                    .llvm_ctx("select vector pop outcome")?;
                self.builder.position_at_end(absent);
                self.release_failed_vector_receiver(receiver, vector, glue_id)?;
                self.emit_edge(failure()?)?;
                self.builder.position_at_end(present);
                let element = self
                    .builder
                    .build_load(element_ty, output, "vector.pop.value")
                    .llvm_ctx("load transferred vector element")?;
                let tuple_ty =
                    llvm_type(self.ctx, &self.storage(result)?.layout.repr)?.into_struct_type();
                let pair = self
                    .builder
                    .build_insert_value(tuple_ty.const_zero(), vector, 0, "vector.pop.receiver")
                    .llvm_ctx("construct updated vector result")?
                    .into_struct_value();
                let pair = self
                    .builder
                    .build_insert_value(pair, element, 1, "vector.pop.result")
                    .llvm_ctx("construct removed element result")?
                    .into_struct_value();
                self.clear_owned(receiver)?;
                self.store(result, pair.into())?;
            }
        }
        self.emit_edge(normal)
    }

    fn release_failed_vector_receiver(
        &self,
        receiver: StorageId,
        vector: PointerValue<'ctx>,
        glue: PhysicalVectorId,
    ) -> CodegenResult<()> {
        self.value_emitter().destroy_loaded_value(
            vector.into(),
            &self.storage(receiver)?.layout,
            DestroyAction::Vector(glue),
        )?;
        self.clear_owned(receiver)
    }

    fn emit_utf8_decode(
        &self,
        bytes: StorageId,
        result: StorageId,
        result_glue: PhysicalVariantId,
        error_glue: PhysicalAggregateId,
        option_glue: PhysicalVariantId,
    ) -> CodegenResult<()> {
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let target = TargetData::create(&self.module.target.data_layout);
        let size_ty = self.ctx.ptr_sized_int_type(&target, None);
        let value_out = self
            .value_emitter()
            .entry_scratch(ptr.into(), "utf8.value")?;
        let valid_out = self
            .value_emitter()
            .entry_scratch(size_ty.into(), "utf8.valid.up.to")?;
        let length_out = self
            .value_emitter()
            .entry_scratch(size_ty.into(), "utf8.error.length")?;
        let function = get_or_declare_external(
            self.llvm,
            "hew_bytes_decode_utf8",
            self.ctx
                .i8_type()
                .fn_type(&[ptr.into(), ptr.into(), ptr.into(), ptr.into()], false),
        )?;
        let status = self
            .runtime_call_value(
                function,
                &[
                    self.slots[bytes.0 as usize].into(),
                    value_out.into(),
                    valid_out.into(),
                    length_out.into(),
                ],
                "utf8.status",
            )?
            .into_int_value();
        let success = self.ctx.append_basic_block(self.value, "utf8.success");
        let failure = self.ctx.append_basic_block(self.value, "utf8.error");
        let invalid = self
            .ctx
            .append_basic_block(self.value, "utf8.invalid.status");
        let complete = self.ctx.append_basic_block(self.value, "utf8.complete");
        self.builder
            .build_switch(
                status,
                invalid,
                &[
                    (self.ctx.i8_type().const_zero(), success),
                    (self.ctx.i8_type().const_int(1, false), failure),
                ],
            )
            .llvm_ctx("select UTF-8 value outcome")?;

        self.builder.position_at_end(success);
        let value = self
            .builder
            .build_load(ptr, value_out, "utf8.string")
            .llvm_ctx("load successful UTF-8 string")?;
        self.write_variant_value(self.slots[result.0 as usize], 0, &[value], result_glue)?;
        self.builder
            .build_unconditional_branch(complete)
            .llvm_ctx("finish UTF-8 success")?;

        self.builder.position_at_end(failure);
        let valid = self
            .builder
            .build_load(size_ty, valid_out, "utf8.valid")
            .llvm_ctx("load UTF-8 valid prefix")?
            .into_int_value();
        let length = self
            .builder
            .build_load(size_ty, length_out, "utf8.length")
            .llvm_ctx("load UTF-8 invalid sequence length")?
            .into_int_value();
        let valid = self
            .builder
            .build_int_z_extend_or_bit_cast(valid, self.ctx.i64_type(), "utf8.valid.i64")
            .llvm_ctx("widen UTF-8 byte position")?;
        let length = self
            .builder
            .build_int_z_extend_or_bit_cast(length, self.ctx.i64_type(), "utf8.length.i64")
            .llvm_ctx("widen UTF-8 error length")?;
        let option_ty = llvm_type(
            self.ctx,
            &self
                .value_emitter()
                .variant_layout(&self.value_emitter().variant_glue(option_glue)?.ty)?
                .object
                .repr,
        )?;
        let option = self
            .value_emitter()
            .entry_scratch(option_ty, "utf8.optional.length")?;
        let some = self.ctx.append_basic_block(self.value, "utf8.length.some");
        let none = self.ctx.append_basic_block(self.value, "utf8.length.none");
        let error_ready = self.ctx.append_basic_block(self.value, "utf8.error.ready");
        let incomplete = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                length,
                self.ctx.i64_type().const_zero(),
                "utf8.incomplete",
            )
            .llvm_ctx("classify incomplete UTF-8")?;
        self.builder
            .build_conditional_branch(incomplete, none, some)
            .llvm_ctx("select UTF-8 error length")?;
        self.builder.position_at_end(some);
        self.write_variant_value(option, 0, &[length.into()], option_glue)?;
        self.builder
            .build_unconditional_branch(error_ready)
            .llvm_ctx("finish known error length")?;
        self.builder.position_at_end(none);
        self.write_variant_value(option, 1, &[], option_glue)?;
        self.builder
            .build_unconditional_branch(error_ready)
            .llvm_ctx("finish incomplete error length")?;
        self.builder.position_at_end(error_ready);
        let option = self
            .builder
            .build_load(option_ty, option, "utf8.error.option")
            .llvm_ctx("load initialized UTF-8 error length")?;
        let error_ty = &self.value_emitter().aggregate_glue(error_glue)?.ty;
        let error_layout =
            self.module.target.layout(error_ty).ok_or_else(|| {
                CodegenError::FailClosed("UTF-8 error has no physical layout".into())
            })?;
        let record_ty = llvm_type(self.ctx, &error_layout.repr)?.into_struct_type();
        let record = self
            .builder
            .build_insert_value(record_ty.get_undef(), valid, 0, "utf8.error.position")
            .llvm_ctx("construct UTF-8 error position")?
            .into_struct_value();
        let record = self
            .builder
            .build_insert_value(record, option, 1, "utf8.error.record")
            .llvm_ctx("construct UTF-8 error length")?
            .into_struct_value();
        self.write_variant_value(
            self.slots[result.0 as usize],
            1,
            &[record.into()],
            result_glue,
        )?;
        self.builder
            .build_unconditional_branch(complete)
            .llvm_ctx("finish UTF-8 error")?;
        self.builder.position_at_end(invalid);
        self.value_emitter().emit_invalid_variant_tag()?;
        self.builder.position_at_end(complete);
        Ok(())
    }

    fn emit_bytes_index(
        &self,
        bytes: StorageId,
        index: StorageId,
        result: StorageId,
        normal: &PhysicalEdge,
        failure: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let value = self.load(bytes, "bytes.index.value")?.into_struct_value();
        let pointer = self
            .builder
            .build_extract_value(value, 0, "bytes.index.pointer")
            .llvm_ctx("extract bytes index pointer")?
            .into_pointer_value();
        let offset = self
            .builder
            .build_extract_value(value, 1, "bytes.index.offset")
            .llvm_ctx("extract bytes index offset")?
            .into_int_value();
        let len = self
            .builder
            .build_extract_value(value, 2, "bytes.index.length")
            .llvm_ctx("extract bytes index length")?
            .into_int_value();
        let index = self.load(index, "bytes.index.index")?.into_int_value();
        let len64 = self
            .builder
            .build_int_z_extend(len, self.ctx.i64_type(), "bytes.index.length.i64")
            .llvm_ctx("widen bytes length")?;
        let offset64 = self
            .builder
            .build_int_z_extend(offset, self.ctx.i64_type(), "bytes.index.offset.i64")
            .llvm_ctx("widen bytes offset")?;
        let byte_offset = self
            .builder
            .build_int_add(offset64, index, "bytes.index.byte.offset")
            .llvm_ctx("calculate bytes index offset")?;
        let negative = self
            .builder
            .build_int_compare(
                IntPredicate::SLT,
                index,
                self.ctx.i64_type().const_zero(),
                "bytes.index.negative",
            )
            .llvm_ctx("guard negative bytes index")?;
        let past_end = self
            .builder
            .build_int_compare(IntPredicate::SGE, index, len64, "bytes.index.past.end")
            .llvm_ctx("guard bytes index upper bound")?;
        let null = self
            .builder
            .build_is_null(pointer, "bytes.index.null")
            .llvm_ctx("guard null bytes index pointer")?;
        let offset_overflow = self
            .builder
            .build_int_compare(
                IntPredicate::UGT,
                byte_offset,
                self.ctx.i64_type().const_int(u64::from(u32::MAX), false),
                "bytes.index.offset.overflow",
            )
            .llvm_ctx("guard bytes index offset overflow")?;
        let out_of_bounds = self
            .builder
            .build_or(negative, past_end, "bytes.index.bounds")
            .and_then(|bounds| self.builder.build_or(bounds, null, "bytes.index.invalid"))
            .and_then(|invalid| {
                self.builder
                    .build_or(invalid, offset_overflow, "bytes.index.failure.condition")
            })
            .llvm_ctx("combine bytes index guards")?;
        let safe = self.ctx.append_basic_block(self.value, "bytes.index.safe");
        let failed = self
            .ctx
            .append_basic_block(self.value, "bytes.index.failure");
        self.builder
            .build_conditional_branch(out_of_bounds, failed, safe)
            .llvm_ctx("branch around fallible bytes index load")?;

        self.builder.position_at_end(failed);
        self.emit_edge(failure)?;

        self.builder.position_at_end(safe);
        // SAFETY: the physical normal path proves a non-null pointer, an index
        // within the active region, and an offset that is representable by the
        // runtime Bytes layout. Verified owned Bytes storage supplies the
        // allocation-validity invariant for that active region.
        let read_at = unsafe {
            self.builder.build_gep(
                self.ctx.i8_type(),
                pointer,
                &[byte_offset],
                "bytes.index.pointer",
            )
        }
        .llvm_ctx("calculate bytes index pointer")?;
        let indexed = self
            .builder
            .build_load(self.ctx.i8_type(), read_at, "bytes.index.load")
            .llvm_ctx("load indexed byte")?;
        self.store(result, indexed)?;
        self.emit_edge(normal)
    }

    fn runtime_call_value(
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

    fn runtime_call_void(
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
        let function = external_fault_new(self.ctx, self.llvm)?;
        let fault = self
            .builder
            .build_call(
                function,
                &[self.ctx.i32_type().const_int(code as u64, true).into()],
                "trap.fault",
            )
            .llvm_ctx("create physical trap fault")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("fault constructor returned void".into()))?;
        self.builder
            .build_store(self.fault_out, fault)
            .llvm_ctx("store physical trap fault")?;
        self.builder
            .build_return(Some(&self.ctx.i32_type().const_int(code as u64, true)))
            .llvm_ctx("return physical trap status")?;
        Ok(())
    }

    fn emit_propagate_fault(&self) -> CodegenResult<()> {
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
        self.builder
            .build_return(Some(&status))
            .llvm_ctx("return active failure status")?;
        Ok(())
    }
}

fn failure_edge(
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

fn callable(module: &PhysicalModule, id: CallableId) -> CodegenResult<&PhysicalCallable> {
    module
        .callables
        .get(id.0 as usize)
        .filter(|callable| callable.id == id)
        .ok_or_else(|| CodegenError::FailClosed(format!("unknown physical callable {}", id.0)))
}

fn emitted_symbol(module: &PhysicalModule, callable: &PhysicalCallable) -> String {
    if module.entry_callable == Some(callable.id) {
        entry_body_symbol_for_triple(&module.target.triple).to_string()
    } else {
        callable.symbol.clone()
    }
}

fn is_signed(ty: &ResolvedTy) -> bool {
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

fn emit_integer_binary<'ctx>(
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

fn emit_float_binary<'ctx>(
    builder: &Builder<'ctx>,
    op: BinaryOp,
    lhs: inkwell::values::FloatValue<'ctx>,
    rhs: inkwell::values::FloatValue<'ctx>,
) -> CodegenResult<IntValue<'ctx>> {
    let predicate = match op {
        BinaryOp::Equal => FloatPredicate::OEQ,
        BinaryOp::NotEqual => FloatPredicate::ONE,
        BinaryOp::Less => FloatPredicate::OLT,
        BinaryOp::LessEqual => FloatPredicate::OLE,
        BinaryOp::Greater => FloatPredicate::OGT,
        BinaryOp::GreaterEqual => FloatPredicate::OGE,
        _ => {
            return Err(CodegenError::FailClosed(
                "non-comparison float operation reached physical emitter".into(),
            ));
        }
    };
    builder
        .build_float_compare(predicate, lhs, rhs, "float.compare")
        .llvm_ctx("emit physical float comparison")
}

fn emit_entry_success<'ctx>(
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
            "Result process exits are not yet admitted by physical codegen".into(),
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

const fn argument_source(transfer: &ArgumentTransfer) -> StorageId {
    match transfer {
        ArgumentTransfer::Borrow(source) | ArgumentTransfer::Move(source) => *source,
        ArgumentTransfer::Clone { source, .. } => *source,
    }
}

fn external_unary_ptr<'ctx>(
    ctx: &'ctx Context,
    module: &Module<'ctx>,
    symbol: &str,
) -> CodegenResult<FunctionValue<'ctx>> {
    let ptr = ctx.ptr_type(AddressSpace::default());
    get_or_declare_external(module, symbol, ptr.fn_type(&[ptr.into()], false))
}

fn external_drop<'ctx>(
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

fn external_fault_new<'ctx>(
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

fn external_fault_drop<'ctx>(
    ctx: &'ctx Context,
    module: &Module<'ctx>,
) -> CodegenResult<FunctionValue<'ctx>> {
    external_drop(ctx, module, "hew_fault_drop")
}

fn external_fault_report<'ctx>(
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

fn get_or_declare_external<'ctx>(
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

#[cfg(test)]
mod tests {
    mod utf8_fixture {
        include!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../hew-sir/tests/support/runtime_utf8.rs"
        ));
    }

    use std::collections::BTreeMap;

    use hew_hir::{lower_program_host_target, ItemId, ResolutionCtx};
    use hew_sir::{
        BlockArg, BoundaryDecision, BoundaryOperand, CallableInstance, CheckedFailure, Edge,
        FunctionSourceOrigin, Operand, Provenance, SemBlock, SemCallConv, SemCallable,
        SemCallableKind, SemFunction, SemModule, SemOp, SemOpKind, SemSignature, SemTerminator,
        ValueDef, ValueId,
    };
    use hew_types::{
        module_registry::ModuleRegistry, Checker, CloneKind, DefId, EntryExitPlan, SendFact,
        TypeFacts, TypeInstanceKey, ValueClass,
    };

    use super::*;

    #[test]
    fn vector_descriptor_matches_the_runtime_c_abi() {
        use hew_runtime::vec::HewVecElemLayout;
        use std::mem::{align_of, offset_of, size_of};

        let triple = native_emission_triple();
        let physical = physical_target_for_triple(&triple).unwrap();
        let target = TargetData::create(&physical.data_layout);
        let ctx = Context::create();
        let descriptor = vector_descriptor_type(&ctx, &target);
        assert_eq!(
            target.get_abi_size(&descriptor),
            size_of::<HewVecElemLayout>() as u64
        );
        assert_eq!(
            target.get_abi_alignment(&descriptor) as usize,
            align_of::<HewVecElemLayout>()
        );
        for (index, expected) in [
            offset_of!(HewVecElemLayout, size),
            offset_of!(HewVecElemLayout, align),
            offset_of!(HewVecElemLayout, ownership_kind),
            offset_of!(HewVecElemLayout, clone_fn),
            offset_of!(HewVecElemLayout, drop_fn),
        ]
        .into_iter()
        .enumerate()
        {
            assert_eq!(
                target.offset_of_element(&descriptor, u32::try_from(index).unwrap()),
                Some(expected as u64)
            );
        }
    }

    #[test]
    fn utf8_decode_emits_typed_outcomes_with_reusable_scratch() {
        let semantic = utf8_fixture::decode_module();
        assert!(hew_sir::verify_module(&semantic).is_empty());
        let triple = native_emission_triple();
        let inventory = hew_mir::physical::physical_type_inventory(&semantic);
        let target = physical_target_for_inventory(&triple, &inventory).unwrap();
        let physical = hew_mir::lower_physical_module(&semantic, target).unwrap();
        let ctx = Context::create();
        let machine =
            crate::llvm::target_machine_for_triple_with_opt_level(&triple, OptLevel::O0).unwrap();
        let module = build_module(&ctx, physical.module(), "utf8_decode", &machine).unwrap();
        module.verify().unwrap();
        let decoder = module
            .get_function("hew_bytes_decode_utf8")
            .expect("decoder ABI call");
        assert_eq!(
            decoder.get_type().get_return_type(),
            Some(ctx.i8_type().into())
        );
        assert_eq!(
            decoder.get_type().get_param_types(),
            vec![ctx.ptr_type(AddressSpace::default()).into(); 4]
        );
        for function in module.get_functions() {
            for block in function.get_basic_blocks() {
                if block.get_name().to_bytes() == b"physical.prologue" {
                    continue;
                }
                for instruction in block.get_instructions() {
                    assert_ne!(
                        instruction.get_opcode(),
                        inkwell::values::InstructionOpcode::Alloca,
                        "decoder scratch must not grow per loop iteration"
                    );
                }
            }
        }
    }

    fn lower_source(source: &str) -> SemModule {
        lower_source_with_registry(source, ModuleRegistry::new(Vec::new()))
    }

    fn lower_source_with_registry(source: &str, registry: ModuleRegistry) -> SemModule {
        let parsed = hew_parser::parse(source);
        assert!(
            parsed.errors.is_empty(),
            "parse errors: {:#?}",
            parsed.errors
        );
        let mut checker = Checker::new(registry);
        let facts = checker.check_program(&parsed.program);
        assert!(facts.errors.is_empty(), "type errors: {:#?}", facts.errors);
        let hir = lower_program_host_target(&parsed.program, &facts, &ResolutionCtx);
        assert!(
            hir.diagnostics.is_empty(),
            "HIR errors: {:#?}",
            hir.diagnostics
        );
        let lowered = hew_sir::lower_module(&hir.module, &facts);
        assert!(
            lowered.statuses.iter().any(|status| {
                status.name == "main"
                    && matches!(status.status, hew_sir::SirLoweringStatus::Lowered)
            }),
            "source main did not lower: {:#?}",
            lowered.statuses
        );
        assert!(
            hew_sir::verify_module(&lowered.module).is_empty(),
            "source must produce verified SIR: {:#?}",
            hew_sir::verify_module(&lowered.module)
        );
        lowered.module
    }

    fn scalar_entry_module() -> SemModule {
        let declaration = DefId::for_test("main");
        let callable = SemCallable {
            id: CallableId(0),
            function: ItemId(0),
            declaration: declaration.clone(),
            instance: CallableInstance::Monomorphic,
            symbol: "main".to_string(),
            source_origin: FunctionSourceOrigin::RootUnit,
            signature: SemSignature {
                params: vec![],
                return_ty: ResolvedTy::I64,
            },
            call_conv: SemCallConv::Default,
            kind: SemCallableKind::HewDirect,
        };
        let function = SemFunction {
            id: ItemId(0),
            callable: CallableId(0),
            declaration: declaration.clone(),
            name: "main".to_string(),
            span: 0..0,
            source_origin: FunctionSourceOrigin::RootUnit,
            params: vec![],
            return_ty: ResolvedTy::I64,
            entry: BlockId(0),
            blocks: vec![SemBlock {
                id: BlockId(0),
                args: vec![],
                ops: vec![SemOp {
                    id: hew_sir::OpId(0),
                    results: vec![ValueDef {
                        id: ValueId(0),
                        ty: ResolvedTy::I64,
                        own: OwnKind::None,
                    }],
                    kind: SemOpKind::ConstI64(7),
                    provenance: Provenance::Synthesized,
                }],
                terminator: SemTerminator::Return {
                    value: Some(BoundaryOperand {
                        operand: Operand { value: ValueId(0) },
                        decision: BoundaryDecision::Move,
                    }),
                },
            }],
            places: vec![],
            bindings: vec![],
        };
        SemModule {
            callables: vec![callable],
            generic_templates: vec![],
            root_unit_callables: vec![CallableId(0)],
            entry_exit_plan: Some(EntryExitPlan {
                entry: declaration,
                action: EntryExitAction::Integer(EntryIntegerType::I64),
            }),
            entry_callable: Some(CallableId(0)),
            functions: vec![function],
            aggregate_shapes: vec![],
            variant_shapes: vec![],
            type_facts: BTreeMap::from([(
                TypeInstanceKey(ResolvedTy::I64),
                TypeFacts {
                    class: ValueClass::BitCopy,
                    clone: CloneKind::Bits,
                    send: SendFact::Known(true),
                    hash: true,
                    eq: true,
                },
            )]),
            string_literals: BTreeMap::new(),
            bytes_literals: BTreeMap::new(),
        }
    }

    fn verified_scalar_for(triple: &str) -> VerifiedPhysicalModule {
        let target = physical_target_for_triple(triple).expect("target layout");
        hew_mir::lower_physical_module(&scalar_entry_module(), target).expect("physical lowering")
    }

    fn checked_add_entry_module() -> SemModule {
        let mut module = scalar_entry_module();
        module.functions[0].blocks = vec![
            SemBlock {
                id: BlockId(0),
                args: vec![],
                ops: vec![
                    SemOp {
                        id: hew_sir::OpId(0),
                        results: vec![ValueDef {
                            id: ValueId(0),
                            ty: ResolvedTy::I64,
                            own: OwnKind::None,
                        }],
                        kind: SemOpKind::ConstI64(i64::MAX),
                        provenance: Provenance::Synthesized,
                    },
                    SemOp {
                        id: hew_sir::OpId(1),
                        results: vec![ValueDef {
                            id: ValueId(1),
                            ty: ResolvedTy::I64,
                            own: OwnKind::None,
                        }],
                        kind: SemOpKind::ConstI64(1),
                        provenance: Provenance::Synthesized,
                    },
                ],
                terminator: SemTerminator::CheckedBinary {
                    id: hew_sir::OpId(2),
                    op: BinaryOp::Add,
                    lhs: Operand { value: ValueId(0) },
                    rhs: Operand { value: ValueId(1) },
                    result: ValueDef {
                        id: ValueId(2),
                        ty: ResolvedTy::I64,
                        own: OwnKind::None,
                    },
                    normal: Edge {
                        target: BlockId(1),
                        args: vec![Operand { value: ValueId(2) }],
                    },
                    failures: vec![CheckedFailure {
                        kind: TrapKind::IntegerOverflow,
                        edge: Edge {
                            target: BlockId(2),
                            args: vec![],
                        },
                    }],
                },
            },
            SemBlock {
                id: BlockId(1),
                args: vec![BlockArg {
                    value: ValueId(3),
                    ty: ResolvedTy::I64,
                    own: OwnKind::None,
                }],
                ops: vec![],
                terminator: SemTerminator::Return {
                    value: Some(BoundaryOperand {
                        operand: Operand { value: ValueId(3) },
                        decision: BoundaryDecision::Move,
                    }),
                },
            },
            SemBlock {
                id: BlockId(2),
                args: vec![],
                ops: vec![],
                terminator: SemTerminator::Trap {
                    kind: TrapKind::IntegerOverflow,
                },
            },
        ];
        module
    }

    fn bytes_copy_module() -> SemModule {
        let declaration = DefId::for_test("copy_bytes");
        let callable = SemCallable {
            id: CallableId(0),
            function: ItemId(0),
            declaration: declaration.clone(),
            instance: CallableInstance::Monomorphic,
            symbol: "copy_bytes".to_string(),
            source_origin: FunctionSourceOrigin::RootUnit,
            signature: SemSignature {
                params: vec![],
                return_ty: ResolvedTy::Bytes,
            },
            call_conv: SemCallConv::Default,
            kind: SemCallableKind::HewDirect,
        };
        let function = SemFunction {
            id: ItemId(0),
            callable: CallableId(0),
            declaration,
            name: "copy_bytes".to_string(),
            span: 0..0,
            source_origin: FunctionSourceOrigin::RootUnit,
            params: vec![],
            return_ty: ResolvedTy::Bytes,
            entry: BlockId(0),
            blocks: vec![SemBlock {
                id: BlockId(0),
                args: vec![],
                ops: vec![
                    SemOp {
                        id: hew_sir::OpId(0),
                        results: vec![ValueDef {
                            id: ValueId(0),
                            ty: ResolvedTy::Bytes,
                            own: OwnKind::Owned,
                        }],
                        kind: SemOpKind::ConstBytes(hew_sir::BytesLiteralId(0)),
                        provenance: Provenance::Synthesized,
                    },
                    SemOp {
                        id: hew_sir::OpId(1),
                        results: vec![ValueDef {
                            id: ValueId(1),
                            ty: ResolvedTy::Bytes,
                            own: OwnKind::Owned,
                        }],
                        kind: SemOpKind::CopyValue {
                            source: Operand { value: ValueId(0) },
                        },
                        provenance: Provenance::Synthesized,
                    },
                    SemOp {
                        id: hew_sir::OpId(2),
                        results: vec![],
                        kind: SemOpKind::DestroyValue {
                            value: Operand { value: ValueId(0) },
                        },
                        provenance: Provenance::Synthesized,
                    },
                ],
                terminator: SemTerminator::Return {
                    value: Some(BoundaryOperand {
                        operand: Operand { value: ValueId(1) },
                        decision: BoundaryDecision::Move,
                    }),
                },
            }],
            places: vec![],
            bindings: vec![],
        };
        SemModule {
            callables: vec![callable],
            generic_templates: vec![],
            root_unit_callables: vec![CallableId(0)],
            entry_exit_plan: None,
            entry_callable: None,
            functions: vec![function],
            aggregate_shapes: vec![],
            variant_shapes: vec![],
            type_facts: BTreeMap::from([(
                TypeInstanceKey(ResolvedTy::Bytes),
                TypeFacts {
                    class: ValueClass::CowValue,
                    clone: CloneKind::Retain,
                    send: SendFact::Known(true),
                    hash: true,
                    eq: true,
                },
            )]),
            string_literals: BTreeMap::new(),
            bytes_literals: BTreeMap::from([(hew_sir::BytesLiteralId(0), b"ok".to_vec())]),
        }
    }

    #[test]
    fn windows_uses_the_same_status_result_fault_abi() {
        let triple = "x86_64-pc-windows-msvc";
        let verified = verified_scalar_for(triple);
        let ctx = Context::create();
        let machine = crate::llvm::target_machine_for_triple_with_opt_level(triple, OptLevel::O0)
            .expect("Windows target machine");
        let module = build_module(&ctx, verified.module(), "windows_status_abi", &machine)
            .expect("Windows physical module");
        let body = module
            .get_function(entry_body_symbol_for_triple(triple))
            .expect("selected entry body");
        assert_eq!(
            body.get_type().get_return_type(),
            Some(ctx.i32_type().into())
        );
        let params = body.get_type().get_param_types();
        assert_eq!(params.len(), 2, "result-out and fault-out");
        assert!(params.iter().all(|parameter| parameter.is_pointer_type()));
        assert!(module.get_function("main").is_some(), "process adapter");
    }

    #[test]
    fn scalar_entry_builds_and_llvm_verifies() {
        let triple = native_emission_triple();
        let verified = verified_scalar_for(&triple);
        validate_physical_codegen(&verified, "scalar_entry").expect("verified LLVM module");
    }

    #[test]
    fn tuple_layout_is_measured_by_the_active_target_data() {
        let triple = native_emission_triple();
        let tuple = ResolvedTy::Tuple(vec![ResolvedTy::I8, ResolvedTy::I64]);
        let target = physical_target_for_types(&triple, [&tuple]).expect("tuple target layout");
        let layout = target.layout(&tuple).expect("measured tuple layout");

        let ctx = Context::create();
        let machine = crate::llvm::target_machine_for_triple_with_opt_level(&triple, OptLevel::O0)
            .expect("target machine");
        let llvm_tuple = ctx.struct_type(&[ctx.i8_type().into(), ctx.i64_type().into()], false);
        assert_eq!(
            layout.size,
            machine.get_target_data().get_abi_size(&llvm_tuple)
        );
        assert_eq!(
            layout.align,
            machine.get_target_data().get_abi_alignment(&llvm_tuple)
        );
        assert!(matches!(
            layout.repr,
            PhysicalRepr::Struct(ref fields)
                if matches!(fields.as_slice(), [
                    PhysicalLayout { repr: PhysicalRepr::Integer { bits: 8 }, .. },
                    PhysicalLayout { repr: PhysicalRepr::Integer { bits: 64 }, .. },
                ])
        ));
    }

    #[test]
    fn owned_record_layout_and_recursive_glue_emit_verified_llvm() {
        let semantic = lower_source(
            r#"
            type Packet { label: string, payload: bytes }

            fn duplicate(packet: Packet) -> Packet { packet }

            fn main() {
                let packet = Packet { payload: b"P", label: "record" };
                let packet_copy = duplicate(packet);
                let first = packet_copy.label;
                let second = packet.label;
            }
            "#,
        );
        let triple = native_emission_triple();
        let inventory = hew_mir::physical::physical_type_inventory(&semantic);
        let target = physical_target_for_inventory(&triple, &inventory)
            .expect("exact aggregate target layout");
        let [shape] = semantic.aggregate_shapes.as_slice() else {
            panic!("source must demand one exact record shape")
        };
        assert!(matches!(
            target.layout(&shape.aggregate_ty),
            Some(PhysicalLayout {
                repr: PhysicalRepr::Struct(fields),
                ..
            }) if fields.len() == 2
        ));
        let verified = hew_mir::lower_physical_module(&semantic, target)
            .expect("owned record physical lowering");
        assert!(verified.module().callables.iter().any(|callable| {
            callable.return_ty == shape.aggregate_ty
                && matches!(
                    callable.params.as_slice(),
                    [hew_mir::PhysicalParam {
                        carrier: ParamCarrier::Indirect,
                        ..
                    }]
                )
        }));
        let ctx = Context::create();
        let machine = crate::llvm::target_machine_for_triple_with_opt_level(&triple, OptLevel::O0)
            .expect("target machine");
        let module = build_module(&ctx, verified.module(), "owned_record", &machine)
            .expect("owned record LLVM module");
        module.verify().expect("owned record LLVM verification");
        let ir = module.print_to_string().to_string();
        assert!(
            ir.contains("aggregate.clone.field") && ir.contains("aggregate.destroy.field"),
            "whole aggregate copy/drop must execute the resolved recursive glue"
        );
        assert!(module.get_function("hew_string_clone").is_some());
        assert!(module.get_function("hew_bytes_clone_ref").is_some());
        assert!(module.get_function("hew_string_drop").is_some());
        assert!(module.get_function("hew_bytes_drop").is_some());
    }

    #[test]
    fn owned_variant_layout_and_active_case_glue_emit_verified_llvm() {
        use inkwell::values::InstructionOpcode;

        let semantic = lower_source(
            r#"
            enum Choice { Text(string), Empty }

            fn inspect(value: Choice) -> i64 {
                match value {
                    .Text(text) => { let copy = text; 1 },
                    .Empty => 0,
                }
            }

            fn main() -> i64 {
                let original = Choice.Text("hello");
                let first = inspect(original);
                let second = inspect(original);
                if first == 1 && second == 1 { 0 } else { 1 }
            }
            "#,
        );
        let triple = native_emission_triple();
        let inventory = hew_mir::physical::physical_type_inventory(&semantic);
        let target = physical_target_for_inventory(&triple, &inventory)
            .expect("exact variant target layout");
        let [shape] = semantic.variant_shapes.as_slice() else {
            panic!("source must demand one exact variant shape")
        };
        let layout = target
            .variant_layout(&shape.enum_ty)
            .expect("target must realize the demanded variant");
        assert!(!layout.is_indirect);
        assert_eq!(layout.variants.len(), 2);

        let verified = hew_mir::lower_physical_module(&semantic, target)
            .expect("owned variant physical lowering");
        assert!(verified.module().functions.iter().any(|function| {
            function.blocks.iter().any(|block| {
                matches!(
                    block.terminator,
                    PhysicalTerminator::SwitchVariant { ref arms, .. }
                        if arms.len() == 2
                            && arms[0].fields.len() == 1
                            && arms[1].fields.is_empty()
                )
            })
        }));

        for level in [OptLevel::O0, OptLevel::O2] {
            let ctx = Context::create();
            let machine = crate::llvm::target_machine_for_triple_with_opt_level(&triple, level)
                .expect("target machine");
            let module = build_module(&ctx, verified.module(), "owned_variant", &machine)
                .expect("owned variant LLVM module");
            module.verify().expect("owned variant LLVM verification");
            let ir = module.print_to_string().to_string();
            assert!(ir.contains("variant.clone.case.0"));
            assert!(ir.contains("variant.destroy.case.0"));
            assert!(ir.contains("call void @llvm.trap"));
            assert!(module.get_function("llvm.trap").is_some());
            assert!(module.get_function("hew_string_clone").is_some());
            assert!(module.get_function("hew_string_drop").is_some());
            for callable in &verified.module().callables {
                let function = module
                    .get_function(&emitted_symbol(verified.module(), callable))
                    .expect("physical callable definition");
                for block in function.get_basic_blocks() {
                    if block.get_name().to_bytes() == b"physical.prologue" {
                        continue;
                    }
                    let mut instruction = block.get_first_instruction();
                    while let Some(current) = instruction {
                        assert_ne!(
                            current.get_opcode(),
                            InstructionOpcode::Alloca,
                            "dynamic CFG blocks must not grow scratch storage at runtime"
                        );
                        instruction = current.get_next_instruction();
                    }
                }
            }
        }
    }

    #[test]
    fn string_length_uses_the_widened_runtime_abi() {
        let repo_root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("hew-codegen-rs must live under the repository root")
            .to_path_buf();
        let semantic = lower_source_with_registry(
            r#"
            import std.string;
            fn main() -> i64 { "length".len() + "Aé中🙂".byte_len() }
            "#,
            ModuleRegistry::new(vec![repo_root]),
        );
        let triple = native_emission_triple();
        let inventory = hew_mir::physical::physical_type_inventory(&semantic);
        let target = physical_target_for_inventory(&triple, &inventory)
            .expect("string length target layout");
        let verified = hew_mir::lower_physical_module(&semantic, target)
            .expect("string length physical lowering");
        let ctx = Context::create();
        let machine = crate::llvm::target_machine_for_triple_with_opt_level(&triple, OptLevel::O0)
            .expect("target machine");
        let module = build_module(&ctx, verified.module(), "string_length", &machine)
            .expect("string length LLVM module");
        module.verify().expect("string length LLVM verification");
        let length = module
            .get_function("hew_string_length")
            .expect("exact runtime length declaration");
        assert_eq!(
            length.get_type().get_return_type(),
            Some(ctx.i64_type().into())
        );
        assert_eq!(length.get_type().count_param_types(), 1);
        let byte_length = module
            .get_function("hew_string_byte_length")
            .expect("explicit byte length must call its distinct runtime operation");
        assert_eq!(
            byte_length.get_type().get_return_type(),
            Some(ctx.i64_type().into())
        );
        assert_eq!(byte_length.get_type().count_param_types(), 1);
    }

    #[test]
    fn physical_emit_option_instruments_generated_code_with_asan() {
        let triple = native_emission_triple();
        let verified = verified_scalar_for(&triple);
        let dir = tempfile::tempdir().expect("physical ASan output directory");
        let artefacts = emit_physical_object(
            &verified,
            &PhysicalEmitOptions {
                module_name: "physical_asan",
                out_dir: dir.path(),
                target_triple: Some(&triple),
                opt_level: OptLevel::O0,
                emit_llvm: true,
                address_sanitizer: true,
            },
        )
        .expect("emit ASan-instrumented physical module");
        let ir = std::fs::read_to_string(artefacts.ll_path.expect("diagnostic LLVM IR"))
            .expect("read physical ASan LLVM IR");
        assert!(
            ir.contains("__asan_init"),
            "physical emitter must write LLVM IR after ASan instrumentation"
        );
    }

    #[test]
    fn checked_add_emits_a_real_overflow_branch() {
        let triple = native_emission_triple();
        let target = physical_target_for_triple(&triple).expect("target layout");
        let verified = hew_mir::lower_physical_module(&checked_add_entry_module(), target)
            .expect("physical lowering");
        let ctx = Context::create();
        let machine = crate::llvm::target_machine_for_triple_with_opt_level(&triple, OptLevel::O0)
            .expect("target machine");
        let module = build_module(&ctx, verified.module(), "checked_add", &machine)
            .expect("checked add LLVM module");
        module.verify().expect("checked add LLVM verification");
        assert!(
            module.get_function("llvm.sadd.with.overflow.i64").is_some(),
            "signed overflow must be detected before choosing the failure edge"
        );
        assert!(
            module.get_function("hew_fault_new").is_some(),
            "the declared overflow edge must reach typed fault creation"
        );
    }

    #[test]
    fn bytes_retain_is_void_and_preserves_the_aggregate() {
        let triple = native_emission_triple();
        let target = physical_target_for_triple(&triple).expect("target layout");
        let verified = hew_mir::lower_physical_module(&bytes_copy_module(), target)
            .expect("physical bytes copy");
        let ctx = Context::create();
        let machine = crate::llvm::target_machine_for_triple_with_opt_level(&triple, OptLevel::O0)
            .expect("target machine");
        let module = build_module(&ctx, verified.module(), "bytes_copy", &machine)
            .expect("bytes copy LLVM module");
        let retain = module
            .get_function("hew_bytes_clone_ref")
            .expect("bytes retain declaration");
        assert_eq!(
            retain.get_type().get_return_type(),
            None,
            "bytes retain mutates only the refcount and returns no pointer"
        );
    }

    #[test]
    fn conflicting_runtime_declaration_fails_closed() {
        let ctx = Context::create();
        let module = ctx.create_module("conflicting_runtime");
        module.add_function("hew_fault_drop", ctx.i32_type().fn_type(&[], false), None);
        let error = external_fault_drop(&ctx, &module).expect_err("ABI mismatch must refuse");
        assert!(error.to_string().contains("hew_fault_drop"));
    }
}
