//! LLVM emitter for verified physical MIR.
//!
//! This path consumes no raw, checked, or elaborated MIR. All ownership,
//! storage, layout, and private ABI choices are already explicit in the
//! verified physical module.

#[path = "physical_actor.rs"]
mod actor;

#[path = "physical_supervisor.rs"]
mod supervisor;

#[path = "physical_callable.rs"]
mod callable;

#[path = "physical_wire.rs"]
mod wire;

#[path = "physical_encoding.rs"]
mod encoding;
#[path = "physical_key.rs"]
mod key;
#[path = "physical_partial.rs"]
mod partial;
#[path = "physical_tcp.rs"]
mod tcp;

#[path = "physical_coro.rs"]
mod coro;
#[path = "physical_generators.rs"]
mod generators;
#[path = "physical_io.rs"]
mod io;
#[path = "physical_channel.rs"]
mod physical_channel;
#[path = "physical_select.rs"]
mod select;
#[path = "physical_stream.rs"]
mod stream;
#[path = "physical_suspend.rs"]
mod suspend;

#[path = "physical_tasks.rs"]
mod tasks;

#[path = "physical_close.rs"]
mod close;
#[path = "physical_dyn.rs"]
mod dyn_object;
#[path = "physical_host.rs"]
mod host;
#[path = "physical_shared.rs"]
mod shared;

pub use host::HostExport;

use std::collections::{BTreeMap, BTreeSet};
use std::num::NonZeroU32;
use std::path::Path;

use hew_mir::physical::{
    BlockId, CallableId, OwnKind, PhysicalAggregateDescriptor, PhysicalAggregateGlue,
    PhysicalAggregateId, PhysicalMapId, PhysicalMapOp, PhysicalResourceDescriptor, PhysicalSetId,
    PhysicalSetOp, PhysicalTypeInventory, PhysicalValueRecipe, PhysicalVariantArm,
    PhysicalVariantDescriptor, PhysicalVariantGlue, PhysicalVariantId, PhysicalVariantLayout,
    PhysicalVectorGlue, PhysicalVectorId, PhysicalVectorOp, TrapKind,
};
use hew_mir::{
    ArgumentTransfer, CloneAction, DestroyAction, ParamCarrier, PhysicalBlock, PhysicalCallable,
    PhysicalCheckedFailure, PhysicalConst, PhysicalEdge, PhysicalFunction, PhysicalLayout,
    PhysicalModule, PhysicalOp, PhysicalRepr, PhysicalRuntimeAction, PhysicalRuntimeCarrier,
    PhysicalStorage, PhysicalTarget, PhysicalTerminator, ReturnTransfer, StorageId,
    VerifiedPhysicalModule,
};
use hew_parser::ast::{BinaryOp, UnaryOp};
use hew_runtime::internal::types::{
    HEW_TRAP_ACTOR_SEND_FAILED, HEW_TRAP_DIVIDE_BY_ZERO, HEW_TRAP_INDEX_OUT_OF_BOUNDS,
    HEW_TRAP_INTEGER_OVERFLOW, HEW_TRAP_SHIFT_OUT_OF_RANGE, HEW_TRAP_SIGNED_MIN_DIV_NEG_ONE,
    HEW_TRAP_USER_PANIC,
};
use hew_runtime::vec::HewTypeOwnershipKind;
use hew_types::runtime_call::{
    collection_type_arguments, FloatMethodOp, IntArithKind, IntBitOp, IntMethodWidth, MathIntrinsic,
};
use hew_types::{
    EntryExitAction, EntryIntegerType, ResolvedTy, RuntimeCallFamily, ValueCapability,
};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::intrinsics::Intrinsic;
use inkwell::module::{Linkage, Module};
use inkwell::targets::{FileType, TargetData, TargetMachine};
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValueEnum, FunctionValue, IntValue, PointerValue, StructValue,
};
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};

use crate::llvm::{
    entry_body_symbol_for_triple, native_emission_triple, CodegenError, CodegenResult,
    EmitArtefacts, LlvmResultExt, OptLevel,
};

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
    physical_target_for_parts(
        triple,
        types,
        std::iter::empty(),
        std::iter::empty(),
        std::iter::empty(),
    )
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
        inventory.resources(),
    )
}

fn physical_target_for_parts<'a>(
    triple: &str,
    types: impl IntoIterator<Item = &'a ResolvedTy>,
    aggregates: impl IntoIterator<Item = &'a PhysicalAggregateDescriptor>,
    variants: impl IntoIterator<Item = &'a PhysicalVariantDescriptor>,
    resources: impl IntoIterator<Item = &'a PhysicalResourceDescriptor>,
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
    for resource in resources {
        let repr = match resource
            .release
            .carrier()
            .map_err(CodegenError::FailClosed)?
        {
            hew_mir::physical::ResourceCarrier::Pointer => PhysicalRepr::Pointer,
            hew_mir::physical::ResourceCarrier::I32 => PhysicalRepr::Integer { bits: 32 },
            // A record resource carries its own fields; the ordinary
            // aggregate walk below realizes its layout.
            hew_mir::physical::ResourceCarrier::Record => continue,
        };
        let (size, align) = measure_layout(&data, llvm_type(&ctx, &repr)?);
        target.insert_layout(resource.ty.clone(), PhysicalLayout { size, align, repr });
    }
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
        if *ty == ResolvedTy::Never {
            continue;
        }
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
    if let ResolvedTy::Array(element, length) = ty {
        realize_layout(
            ctx,
            data,
            target,
            element,
            aggregate_fields,
            variant_shapes,
            visiting,
        )?;
        let element = target
            .layout(element)
            .ok_or_else(|| CodegenError::FailClosed("array element has no target layout".into()))?;
        hew_mir::physical::validate_array_allocation(
            *length,
            element,
            u64::from(data.get_pointer_byte_size(None)),
        )
        .map_err(|error| {
            CodegenError::FailClosed(format!("{}: {}", ty.user_facing(), error.message))
        })?;
    }
    let captures = match ty {
        ResolvedTy::Closure { captures, .. } => Some(captures.as_slice()),
        ResolvedTy::Function { .. } => Some([].as_slice()),
        _ => None,
    };
    if let Some(captures) = captures {
        for capture in captures {
            realize_layout(
                ctx,
                data,
                target,
                capture,
                aggregate_fields,
                variant_shapes,
                visiting,
            )?;
        }
        let fields = captures
            .iter()
            .map(|ty| {
                target.layout(ty).cloned().ok_or_else(|| {
                    CodegenError::FailClosed("capture lacks its target layout".into())
                })
            })
            .collect::<CodegenResult<Vec<_>>>()?;
        let environment = callable_environment_layout(ctx, data, fields)?;
        target.insert_environment_layout(ty.clone(), environment);
    }
    if let Some(shape) = variant_shapes.get(ty) {
        // An indirect enum value is a pointer to its heap node, so the value
        // layout lands before the node fields: a recursive occurrence finds
        // it and terminates, and nothing behind the box is by-value containment.
        let mut node_visiting = BTreeSet::new();
        let field_visiting = if shape.is_indirect {
            let (size, align) = measure_layout(data, llvm_type(ctx, &PhysicalRepr::Pointer)?);
            target.insert_layout(
                ty.clone(),
                PhysicalLayout {
                    size,
                    align,
                    repr: PhysicalRepr::Pointer,
                },
            );
            &mut node_visiting
        } else {
            &mut *visiting
        };
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
                    field_visiting,
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
            0..=256 => 8,
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
        if !shape.is_indirect {
            target.insert_layout(ty.clone(), object.clone());
        }
        target.insert_variant_layout(PhysicalVariantLayout {
            ty: ty.clone(),
            is_indirect: shape.is_indirect,
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

/// Mask bits track logical capture initialization, including zero-sized values.
/// LLVM lays out the mask array and aligned fields as a single native struct.
fn callable_environment_layout(
    ctx: &Context,
    target: &TargetData,
    fields: Vec<PhysicalLayout>,
) -> CodegenResult<PhysicalLayout> {
    let repr = if fields.is_empty() {
        PhysicalRepr::Unit
    } else {
        let mask_len = u32::try_from(fields.len().div_ceil(8)).map_err(|_| {
            CodegenError::FailClosed("capture initialization mask exceeds u32 bytes".into())
        })?;
        let mask_repr = PhysicalRepr::Array {
            element: Box::new(integer_layout(ctx, target, 8)?),
            len: mask_len,
        };
        let (size, align) = measure_layout(target, llvm_type(ctx, &mask_repr)?);
        let mut storage = Vec::with_capacity(fields.len() + 1);
        storage.push(PhysicalLayout {
            size,
            align,
            repr: mask_repr,
        });
        storage.extend(fields);
        PhysicalRepr::Struct(storage)
    };
    let (size, align) = measure_layout(target, llvm_type(ctx, &repr)?);
    Ok(PhysicalLayout { size, align, repr })
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
    emit_physical_object_with_host(verified, options, None)
}

/// Emit an experimental C wrapper and its verified library body.
///
/// # Errors
/// Returns target, LLVM or output errors.
pub fn emit_host_object(
    export: &HostExport<'_>,
    options: &PhysicalEmitOptions<'_>,
) -> Result<EmitArtefacts, CodegenError> {
    emit_physical_object_with_host(export.verified, options, Some(export))
}

fn emit_physical_object_with_host(
    verified: &VerifiedPhysicalModule,
    options: &PhysicalEmitOptions<'_>,
    host: Option<&HostExport<'_>>,
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
        host,
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
        None,
    )
}

#[allow(
    clippy::too_many_arguments,
    reason = "shared native output pipeline with an optional checked host export"
)]
fn emit_physical_to_paths(
    verified: &VerifiedPhysicalModule,
    module_name: &str,
    triple: &str,
    opt_level: OptLevel,
    address_sanitizer: bool,
    ll_path: Option<&Path>,
    object_path: Option<&Path>,
    host: Option<&HostExport<'_>>,
) -> CodegenResult<()> {
    let machine = crate::llvm::target_machine_for_triple_with_opt_level(triple, opt_level)?;
    let ctx = Context::create();
    let llvm_module = build_module_with_host(&ctx, verified.module(), module_name, &machine, host)?;
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
        ResolvedTy::String | ResolvedTy::CancellationToken | ResolvedTy::Array(_, _) => {
            PhysicalRepr::Pointer
        }
        // A trait object is the runtime's two-word `HewTraitObject`:
        // the boxed value and its dispatch table.
        ResolvedTy::Function { .. }
        | ResolvedTy::Closure { .. }
        | ResolvedTy::TraitObject { .. } => PhysicalRepr::Struct(vec![
            pointer_layout(ctx, target)?,
            pointer_layout(ctx, target)?,
        ]),
        // A lambda actor's handle addresses an ordinary actor; only its
        // spelling differs, because it names no source nominal.
        actor
            if actor.is_builtin(hew_types::BuiltinType::LocalPid)
                || actor.is_builtin(hew_types::BuiltinType::LambdaPid) =>
        {
            PhysicalRepr::Integer { bits: pointer_bits }
        }
        node if node.is_builtin(hew_types::BuiltinType::NodeId) => PhysicalRepr::Struct(vec![
            integer_layout(ctx, target, 64)?,
            integer_layout(ctx, target, 64)?,
        ]),
        location
            if location.is_builtin(hew_types::BuiltinType::Location)
                || location.is_builtin(hew_types::BuiltinType::RemotePid) =>
        {
            // Matches `HewLocation` / `HewRemotePid`: the NodeId words are
            // flattened, and reserved remains an explicit zeroed u32.
            PhysicalRepr::Struct(vec![
                integer_layout(ctx, target, 64)?,
                integer_layout(ctx, target, 64)?,
                integer_layout(ctx, target, 64)?,
                integer_layout(ctx, target, 32)?,
                integer_layout(ctx, target, 32)?,
            ])
        }
        // A supervised role addresses its actor through the supervisor that
        // owns it: the supervisor's handle and the declared slot.
        role if role.is_builtin(hew_types::BuiltinType::ChildRef) => PhysicalRepr::Struct(vec![
            integer_layout(ctx, target, pointer_bits)?,
            integer_layout(ctx, target, 32)?,
        ]),
        // A pool view addresses its members the same way a role addresses one
        // child: the owning supervisor and the first of the pool's consecutive
        // slots. The member count is a declaration fact, so it is not carried.
        pool if pool.is_builtin(hew_types::BuiltinType::SupervisorPool) => {
            PhysicalRepr::Struct(vec![
                integer_layout(ctx, target, pointer_bits)?,
                integer_layout(ctx, target, 32)?,
            ])
        }
        // A strong handle is the payload pointer and a weak handle the
        // allocation header pointer; both are one machine pointer.
        shared if hew_types::runtime_call::shared_handle_payload(shared).is_some() => {
            PhysicalRepr::Pointer
        }
        collection if collection_type_arguments(collection).is_some() => PhysicalRepr::Pointer,
        encoding if hew_mir::physical::encoding_format(encoding).is_some() => PhysicalRepr::Pointer,
        ResolvedTy::Bytes => PhysicalRepr::Struct(vec![
            pointer_layout(ctx, target)?,
            integer_layout(ctx, target, 32)?,
            integer_layout(ctx, target, 32)?,
        ]),
        ResolvedTy::Unit => PhysicalRepr::Unit,
        // An `#[opaque]` nominal with no resource descriptor is a bit-copied
        // FFI id of pointer width; its lifecycle belongs to whatever owns it.
        ResolvedTy::Named {
            builtin: None,
            is_opaque: true,
            ..
        } => PhysicalRepr::Pointer,
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
    ramps: BTreeMap<CallableId, FunctionValue<'ctx>>,
    value_callbacks: key::CallbackTable<'ctx>,
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
    place_flags: BTreeMap<StorageId, PointerValue<'ctx>>,
    result_out: Option<PointerValue<'ctx>>,
    fault_out: PointerValue<'ctx>,
    active_fault: PointerValue<'ctx>,
    active_status: PointerValue<'ctx>,
    fault_parks: BTreeMap<hew_mir::physical::FaultParkId, (PointerValue<'ctx>, PointerValue<'ctx>)>,
    functions: &'a BTreeMap<CallableId, FunctionValue<'ctx>>,
    value_callbacks: &'a key::CallbackTable<'ctx>,
    ramps: &'a BTreeMap<CallableId, FunctionValue<'ctx>>,
    frame: Option<coro::Frame<'ctx>>,
    task_scopes: BTreeMap<hew_mir::physical::TaskScopeId, PointerValue<'ctx>>,
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

    fn write_variant_value(
        &self,
        destination: PointerValue<'ctx>,
        variant: u32,
        fields: &[BasicValueEnum<'ctx>],
        glue_id: PhysicalVariantId,
    ) -> CodegenResult<()> {
        let glue = self.variant_glue(glue_id)?;
        let layout = self.variant_layout(&glue.ty)?;
        let object = if layout.is_indirect {
            self.alloc_variant_node(layout)?
        } else {
            destination
        };
        let object_ty = llvm_type(self.ctx, &layout.object.repr)?.into_struct_type();
        let tag_ty = object_ty
            .get_field_type_at_index(0)
            .ok_or_else(|| CodegenError::FailClosed("variant object has no tag field".into()))?
            .into_int_type();
        let tag = tag_ty.const_int(u64::from(variant), false);
        let header = self
            .builder
            .build_insert_value(object_ty.const_zero(), tag, 0, "variant.make.tag")
            .llvm_ctx("write physical variant tag")?
            .into_struct_value();
        self.builder
            .build_store(object, header)
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
            let payload_ptr = self.variant_payload_ptr(object, layout)?;
            self.builder
                .build_store(payload_ptr, payload)
                .llvm_ctx("store physical variant payload")?;
        }
        if layout.is_indirect {
            self.builder
                .build_store(destination, object)
                .llvm_ctx("store indirect variant node")?;
        }
        Ok(())
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

    /// The tag-and-payload object behind one enum value slot: the slot itself
    /// for a direct enum, the heap node an indirect enum slot points at.
    fn variant_object_ptr(
        &self,
        slot: PointerValue<'ctx>,
        layout: &PhysicalVariantLayout,
    ) -> CodegenResult<PointerValue<'ctx>> {
        if !layout.is_indirect {
            return Ok(slot);
        }
        Ok(self
            .builder
            .build_load(
                self.ctx.ptr_type(AddressSpace::default()),
                slot,
                "variant.node",
            )
            .llvm_ctx("load indirect variant node")?
            .into_pointer_value())
    }

    fn load_variant_tag(
        &self,
        object: PointerValue<'ctx>,
        layout: &PhysicalVariantLayout,
    ) -> CodegenResult<IntValue<'ctx>> {
        let object_ty = llvm_type(self.ctx, &layout.object.repr)?.into_struct_type();
        let tag_ty = object_ty
            .get_field_type_at_index(0)
            .ok_or_else(|| CodegenError::FailClosed("variant object has no tag field".into()))?;
        let tag_ptr = self
            .builder
            .build_struct_gep(object_ty, object, 0, "variant.tag.ptr")
            .llvm_ctx("address physical variant tag")?;
        Ok(self
            .builder
            .build_load(tag_ty, tag_ptr, "variant.tag")
            .llvm_ctx("read physical variant tag")?
            .into_int_value())
    }

    fn variant_node_size(
        &self,
        layout: &PhysicalVariantLayout,
    ) -> [BasicMetadataValueEnum<'ctx>; 2] {
        let size_ty = self.ctx.i64_type();
        [
            size_ty.const_int(layout.object.size, false).into(),
            size_ty
                .const_int(u64::from(layout.object.align), false)
                .into(),
        ]
    }

    /// Allocate one indirect enum node. The enum value owns it until its
    /// destructure, switch or drop glue releases it.
    fn alloc_variant_node(
        &self,
        layout: &PhysicalVariantLayout,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let size_ty = self.ctx.i64_type();
        let alloc = get_or_declare_external(
            self.llvm,
            "hew_alloc",
            self.ctx
                .ptr_type(AddressSpace::default())
                .fn_type(&[size_ty.into(), size_ty.into()], false),
        )?;
        Ok(self
            .builder
            .build_call(alloc, &self.variant_node_size(layout), "variant.node.alloc")
            .llvm_ctx("allocate indirect variant node")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("node allocation returned void".into()))?
            .into_pointer_value())
    }

    fn free_variant_node(
        &self,
        node: PointerValue<'ctx>,
        layout: &PhysicalVariantLayout,
    ) -> CodegenResult<()> {
        let size_ty = self.ctx.i64_type();
        let dealloc = get_or_declare_external(
            self.llvm,
            "hew_dealloc",
            self.ctx.void_type().fn_type(
                &[
                    self.ctx.ptr_type(AddressSpace::default()).into(),
                    size_ty.into(),
                    size_ty.into(),
                ],
                false,
            ),
        )?;
        let [size, align] = self.variant_node_size(layout);
        self.builder
            .build_call(dealloc, &[node.into(), size, align], "")
            .llvm_ctx("release indirect variant node")?;
        Ok(())
    }

    /// Emit or reuse one internal glue function. A recursive indirect enum's
    /// recipe reaches its own glue through a call instead of unrolling.
    fn glue_function(
        &self,
        name: &str,
        signature: FunctionType<'ctx>,
        body: impl FnOnce(&ValueEmitter<'_, 'ctx>, FunctionValue<'ctx>) -> CodegenResult<()>,
    ) -> CodegenResult<FunctionValue<'ctx>> {
        if let Some(existing) = self.llvm.get_function(name) {
            return Ok(existing);
        }
        let function = self
            .llvm
            .add_function(name, signature, Some(Linkage::Internal));
        let builder = self.ctx.create_builder();
        let entry = self.ctx.append_basic_block(function, "entry");
        let body_block = self.ctx.append_basic_block(function, "body");
        builder.position_at_end(entry);
        builder
            .build_unconditional_branch(body_block)
            .llvm_ctx("enter variant glue")?;
        builder.position_at_end(body_block);
        let emitter = ValueEmitter {
            module: self.module,
            ctx: self.ctx,
            llvm: self.llvm,
            builder: &builder,
            value: function,
        };
        body(&emitter, function)?;
        Ok(function)
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

    /// Release a `#[resource]` record by calling the program's own `close`.
    ///
    /// `close` is an ordinary private callable, so it uses the private ABI:
    /// its arguments, then the caller's fault slot, returning a status. Drop
    /// glue has no unwind successor to carry a failure into, so a failing
    /// release traps: continuing would leave the value half-released with no
    /// owner able to observe it.
    fn emit_record_close(
        &self,
        value: BasicValueEnum<'ctx>,
        close: CallableId,
    ) -> CodegenResult<()> {
        let callee = callable(self.module, close)?;
        let function = self
            .llvm
            .get_function(&emitted_symbol(self.module, callee))
            .ok_or_else(|| {
                CodegenError::FailClosed("record release has no emitted close body".into())
            })?;
        let parameter = callee.params.first().ok_or_else(|| {
            CodegenError::FailClosed("record release close takes no receiver".into())
        })?;
        let receiver: BasicMetadataValueEnum<'ctx> = match parameter.carrier {
            ParamCarrier::Direct => value.into(),
            ParamCarrier::Indirect => {
                let slot = self.entry_scratch(
                    llvm_type(self.ctx, &parameter.layout.repr)?,
                    "record.close.receiver",
                )?;
                self.builder
                    .build_store(slot, value)
                    .llvm_ctx("stage record release receiver")?;
                slot.into()
            }
        };
        let fault = self.entry_scratch(
            self.ctx.ptr_type(AddressSpace::default()).into(),
            "record.close.fault",
        )?;
        self.builder
            .build_store(
                fault,
                self.ctx.ptr_type(AddressSpace::default()).const_null(),
            )
            .llvm_ctx("clear record release fault slot")?;
        let status = self
            .builder
            .build_call(function, &[receiver, fault.into()], "record.close.status")
            .llvm_ctx("call record release")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("record release returned no status".into()))?
            .into_int_value();
        let ok = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                self.ctx.i32_type().const_zero(),
                "record.close.ok",
            )
            .llvm_ctx("compare record release status")?;
        let released = self.ctx.append_basic_block(self.value, "record.close.done");
        let failed = self
            .ctx
            .append_basic_block(self.value, "record.close.failed");
        self.builder
            .build_conditional_branch(ok, released, failed)
            .llvm_ctx("branch on record release status")?;
        self.builder.position_at_end(failed);
        let trap = Intrinsic::find("llvm.trap")
            .ok_or_else(|| CodegenError::FailClosed("LLVM trap intrinsic is unavailable".into()))?
            .get_declaration(self.llvm, &[])
            .ok_or_else(|| CodegenError::FailClosed("LLVM trap declaration failed".into()))?;
        self.builder
            .build_call(trap, &[], "record.close.trap")
            .llvm_ctx("emit failing record release trap")?;
        self.builder
            .build_unreachable()
            .llvm_ctx("terminate failing record release")?;
        self.builder.position_at_end(released);
        // D442: `close` consumes the record (`fn close(consume self)` is the
        // checker-enforced contract), so `close`'s own body already destroys
        // every member it does not move out. Also releasing the members here
        // double-frees them — this call site owns nothing further once the
        // callee returns successfully.
        Ok(())
    }

    fn clone_loaded_value(
        &self,
        value: BasicValueEnum<'ctx>,
        layout: &PhysicalLayout,
        action: CloneAction,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        match action {
            CloneAction::Encoding(format) => {
                let function = external_unary_ptr(
                    self.ctx,
                    self.llvm,
                    hew_mir::physical::EncodingOp::Clone.c_symbol(format),
                )?;
                self.builder
                    .build_call(
                        function,
                        &[value.into_pointer_value().into()],
                        "encoding.clone",
                    )
                    .llvm_ctx("clone encoding value")?
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| CodegenError::FailClosed("encoding clone returned void".into()))
            }
            CloneAction::Callable => self.clone_callable_value(value, layout),
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
            CloneAction::RcRetain | CloneAction::WeakRetain => {
                let symbol = if action == CloneAction::RcRetain {
                    RuntimeCallFamily::RcClone.row().symbol
                } else {
                    RuntimeCallFamily::WeakCloneRc.row().symbol
                };
                let function = external_unary_ptr(self.ctx, self.llvm, symbol)?;
                self.builder
                    .build_call(
                        function,
                        &[value.into_pointer_value().into()],
                        "shared.retain",
                    )
                    .llvm_ctx("retain a shared allocation")?
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| CodegenError::FailClosed("shared retain returned void".into()))
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
            CloneAction::Array(_)
            | CloneAction::Vector(_)
            | CloneAction::Map(_)
            | CloneAction::Set(_) => {
                let symbol = match action {
                    CloneAction::Array(id) => {
                        self.vector_glue(id)?;
                        "hew_array_clone"
                    }
                    CloneAction::Vector(id) => {
                        self.vector_glue(id)?;
                        "hew_vec_clone_owned"
                    }
                    CloneAction::Map(id) => {
                        self.module
                            .map_glue
                            .get(id.0 as usize)
                            .filter(|glue| glue.id == id)
                            .ok_or_else(|| {
                                CodegenError::FailClosed("unknown physical map glue".into())
                            })?;
                        "hew_hashmap_clone_layout"
                    }
                    CloneAction::Set(id) => {
                        self.module
                            .set_glue
                            .get(id.0 as usize)
                            .filter(|glue| glue.id == id)
                            .ok_or_else(|| {
                                CodegenError::FailClosed("unknown physical set glue".into())
                            })?;
                        "hew_hashset_clone_layout"
                    }
                    _ => unreachable!("matched collection clone"),
                };
                let function = external_unary_ptr(self.ctx, self.llvm, symbol)?;
                self.builder
                    .build_call(function, &[value.into()], "collection.clone")
                    .llvm_ctx("clone descriptor-backed collection")?
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| {
                        CodegenError::FailClosed("collection clone returned void".into())
                    })
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
            DestroyAction::Resource(id) => {
                let resource = self.module.resources.get(id.0 as usize).ok_or_else(|| {
                    CodegenError::FailClosed("resource drop lacks its verified contract".into())
                })?;
                if let hew_mir::physical::ResourceRelease::RecordClose { close, .. } =
                    &resource.release
                {
                    return self.emit_record_close(value, *close);
                }
                let symbol = resource
                    .release
                    .release_symbol()
                    .map_err(CodegenError::FailClosed)?;
                let result = resource.release.release_result();
                let parameters = [value.get_type().into()];
                let signature = if result == ResolvedTy::Unit {
                    self.ctx.void_type().fn_type(&parameters, false)
                } else {
                    let result_layout = self.module.target.layout(&result).ok_or_else(|| {
                        CodegenError::FailClosed(
                            "resource release result lacks its ABI layout".into(),
                        )
                    })?;
                    llvm_type(self.ctx, &result_layout.repr)?.fn_type(&parameters, false)
                };
                let function = get_or_declare_external(self.llvm, symbol, signature)?;
                self.builder
                    .build_call(function, &[value.into()], "")
                    .llvm_ctx("release resource owner")?;
                Ok(())
            }
            DestroyAction::TraitObject => self.destroy_trait_object(value),
            DestroyAction::Callable => {
                let slot =
                    self.entry_scratch(llvm_type(self.ctx, &layout.repr)?, "callable.drop.slot")?;
                self.builder
                    .build_store(slot, value)
                    .llvm_ctx("stage callable destruction")?;
                let drop = external_drop(self.ctx, self.llvm, "hew_callable_drop")?;
                self.builder
                    .build_call(drop, &[slot.into()], "")
                    .llvm_ctx("destroy callable environment")?;
                Ok(())
            }

            // Releasing a strong handle may run the payload's own release,
            // which the runtime already holds as this allocation's destructor.
            DestroyAction::RcRelease(_) | DestroyAction::WeakRelease => {
                let symbol = if matches!(action, DestroyAction::WeakRelease) {
                    RuntimeCallFamily::WeakDropRc.row().symbol
                } else {
                    RuntimeCallFamily::RcDrop.row().symbol
                };
                let function = external_drop(self.ctx, self.llvm, symbol)?;
                self.builder
                    .build_call(function, &[value.into_pointer_value().into()], "")
                    .llvm_ctx("release a shared allocation")?;
                Ok(())
            }
            DestroyAction::Encoding(_)
            | DestroyAction::StringRelease
            | DestroyAction::BytesRelease => {
                let pointer = match action {
                    DestroyAction::Encoding(_) | DestroyAction::StringRelease => {
                        value.into_pointer_value()
                    }
                    DestroyAction::BytesRelease => self
                        .builder
                        .build_extract_value(value.into_struct_value(), 0, "bytes.drop.ptr")
                        .llvm_ctx("extract bytes release pointer")?
                        .into_pointer_value(),
                    DestroyAction::Resource(_)
                    | DestroyAction::Callable
                    | DestroyAction::TraitObject
                    | DestroyAction::RcRelease(_)
                    | DestroyAction::WeakRelease
                    | DestroyAction::Aggregate(_) => {
                        unreachable!("matched primitive release")
                    }
                    DestroyAction::Variant(_) => unreachable!("matched primitive release"),
                    DestroyAction::Array(_)
                    | DestroyAction::Vector(_)
                    | DestroyAction::Map(_)
                    | DestroyAction::Set(_) => {
                        unreachable!("matched primitive release")
                    }
                };
                let symbol = match action {
                    DestroyAction::Encoding(format) => {
                        hew_mir::physical::EncodingOp::Free.c_symbol(format)
                    }
                    DestroyAction::StringRelease => "hew_string_drop",
                    DestroyAction::BytesRelease => "hew_bytes_drop",
                    DestroyAction::Resource(_)
                    | DestroyAction::Callable
                    | DestroyAction::TraitObject
                    | DestroyAction::RcRelease(_)
                    | DestroyAction::WeakRelease
                    | DestroyAction::Aggregate(_) => {
                        unreachable!("matched primitive release")
                    }
                    DestroyAction::Variant(_) => unreachable!("matched primitive release"),
                    DestroyAction::Array(_)
                    | DestroyAction::Vector(_)
                    | DestroyAction::Map(_)
                    | DestroyAction::Set(_) => {
                        unreachable!("matched primitive release")
                    }
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
            DestroyAction::Array(_)
            | DestroyAction::Vector(_)
            | DestroyAction::Map(_)
            | DestroyAction::Set(_) => {
                // Physical MIR decides whether this release runs any
                // user-visible action. When it does not, the `_walk` entry
                // joins a release already in progress instead of nesting one
                // native frame per level; anything that can reach a resource
                // drains synchronously so its `close` keeps today's order.
                let walk = self.module.pure_releases.action_is_pure(action);
                let symbol = match action {
                    DestroyAction::Array(id) => {
                        self.vector_glue(id)?;
                        if walk {
                            "hew_array_free_walk"
                        } else {
                            "hew_array_free"
                        }
                    }
                    DestroyAction::Vector(id) => {
                        self.vector_glue(id)?;
                        if walk {
                            "hew_vec_free_owned_walk"
                        } else {
                            "hew_vec_free_owned"
                        }
                    }
                    DestroyAction::Map(id) => {
                        self.module
                            .map_glue
                            .get(id.0 as usize)
                            .filter(|glue| glue.id == id)
                            .ok_or_else(|| {
                                CodegenError::FailClosed("unknown physical map glue".into())
                            })?;
                        if walk {
                            "hew_hashmap_free_layout_walk"
                        } else {
                            "hew_hashmap_free_layout"
                        }
                    }
                    DestroyAction::Set(id) => {
                        self.module
                            .set_glue
                            .get(id.0 as usize)
                            .filter(|glue| glue.id == id)
                            .ok_or_else(|| {
                                CodegenError::FailClosed("unknown physical set glue".into())
                            })?;
                        if walk {
                            "hew_hashset_free_layout_walk"
                        } else {
                            "hew_hashset_free_layout"
                        }
                    }
                    _ => unreachable!("matched collection destroy"),
                };
                let function = external_drop(self.ctx, self.llvm, symbol)?;
                self.builder
                    .build_call(function, &[value.into()], "collection.drop")
                    .llvm_ctx("destroy descriptor-backed collection")?;
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
            let pointer = self.ctx.ptr_type(AddressSpace::default());
            let clone = self.glue_function(
                &format!("__hew_variant_clone_{}", id.0),
                pointer.fn_type(&[pointer.into()], false),
                |emitter, function| {
                    let source = function
                        .get_nth_param(0)
                        .ok_or_else(|| {
                            CodegenError::FailClosed("variant clone lacks its node".into())
                        })?
                        .into_pointer_value();
                    let destination = emitter.alloc_variant_node(variant_layout)?;
                    emitter.clone_variant_object(source, destination, glue, variant_layout)?;
                    emitter
                        .builder
                        .build_return(Some(&destination))
                        .llvm_ctx("finish indirect variant clone")?;
                    Ok(())
                },
            )?;
            return self
                .builder
                .build_call(clone, &[value.into()], "variant.clone.node")
                .llvm_ctx("clone indirect variant")?
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| CodegenError::FailClosed("variant clone returned void".into()));
        }
        let object_ty = llvm_type(self.ctx, &variant_layout.object.repr)?.into_struct_type();
        let source = self.entry_scratch(object_ty.into(), "variant.clone.source")?;
        let destination = self.entry_scratch(object_ty.into(), "variant.clone.destination")?;
        self.builder
            .build_store(source, value)
            .llvm_ctx("store physical variant clone source")?;
        self.clone_variant_object(source, destination, glue, variant_layout)?;
        self.builder
            .build_load(object_ty, destination, "variant.clone.result")
            .llvm_ctx("load cloned physical variant")
    }

    /// Copy the active case of `source` into the uninitialized `destination`
    /// object; both are tag-and-payload objects of one variant layout.
    fn clone_variant_object(
        &self,
        source: PointerValue<'ctx>,
        destination: PointerValue<'ctx>,
        glue: &PhysicalVariantGlue,
        variant_layout: &PhysicalVariantLayout,
    ) -> CodegenResult<()> {
        let id = glue.id;
        let object_ty = llvm_type(self.ctx, &variant_layout.object.repr)?.into_struct_type();
        let tag = self.load_variant_tag(source, variant_layout)?;
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
        Ok(())
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
            let pointer = self.ctx.ptr_type(AddressSpace::default());
            let drop = self.glue_function(
                &format!("__hew_variant_drop_{}", id.0),
                self.ctx.void_type().fn_type(&[pointer.into()], false),
                |emitter, function| {
                    let node = function
                        .get_nth_param(0)
                        .ok_or_else(|| {
                            CodegenError::FailClosed("variant drop lacks its node".into())
                        })?
                        .into_pointer_value();
                    emitter.destroy_variant_object(node, glue, variant_layout)?;
                    emitter.free_variant_node(node, variant_layout)?;
                    emitter
                        .builder
                        .build_return(None)
                        .llvm_ctx("finish indirect variant drop")?;
                    Ok(())
                },
            )?;
            self.builder
                .build_call(drop, &[value.into()], "")
                .llvm_ctx("destroy indirect variant")?;
            return Ok(());
        }
        let object_ty = llvm_type(self.ctx, &variant_layout.object.repr)?.into_struct_type();
        let source = self.entry_scratch(object_ty.into(), "variant.destroy.source")?;
        self.builder
            .build_store(source, value)
            .llvm_ctx("store physical variant destroy source")?;
        self.destroy_variant_object(source, glue, variant_layout)
    }

    /// Release the owned fields of the active case in `source`, a
    /// tag-and-payload object; the object's own storage stays the caller's.
    fn destroy_variant_object(
        &self,
        source: PointerValue<'ctx>,
        glue: &PhysicalVariantGlue,
        variant_layout: &PhysicalVariantLayout,
    ) -> CodegenResult<()> {
        let tag = self.load_variant_tag(source, variant_layout)?;
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

/// The payload destructor one shared allocation installs at construction.
fn shared_payload_drop_symbol(id: hew_mir::physical::PhysicalSharedId) -> String {
    format!("__hew_shared_payload_{}_drop", id.0)
}

fn map_key_descriptor_symbol(id: PhysicalMapId) -> String {
    format!("__hew_map_key_{}", id.0)
}

fn map_value_descriptor_symbol(id: PhysicalMapId) -> String {
    format!("__hew_map_value_{}", id.0)
}

fn set_key_descriptor_symbol(id: PhysicalSetId) -> String {
    format!("__hew_set_key_{}", id.0)
}

fn value_descriptor_type<'ctx>(
    ctx: &'ctx Context,
    target: &TargetData,
) -> inkwell::types::StructType<'ctx> {
    let size_ty = ctx.ptr_sized_int_type(target, None);
    let pointer = ctx.ptr_type(AddressSpace::default());
    // HewValueLayout's C layout is realized for the selected target,
    // including padding around its u8 ownership discriminant.
    ctx.struct_type(
        &[
            size_ty.into(),
            size_ty.into(),
            ctx.i8_type().into(),
            pointer.into(),
            pointer.into(),
            pointer.into(),
        ],
        false,
    )
}

#[cfg(test)]
fn build_module<'ctx>(
    ctx: &'ctx Context,
    physical: &PhysicalModule,
    name: &str,
    machine: &TargetMachine,
) -> CodegenResult<Module<'ctx>> {
    build_module_with_host(ctx, physical, name, machine, None)
}

fn build_module_with_host<'ctx>(
    ctx: &'ctx Context,
    physical: &PhysicalModule,
    name: &str,
    machine: &TargetMachine,
    host: Option<&HostExport<'_>>,
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
        ramps: BTreeMap::new(),
        value_callbacks: BTreeMap::new(),
    };
    emitter.declare_functions()?;
    emitter.emit_regex_handles()?;
    emitter.emit_collection_value_descriptors()?;
    emitter.emit_task_descriptors()?;
    emitter.emit_generator_descriptors()?;
    emitter.emit_stream_descriptors()?;
    emitter.emit_channel_descriptors()?;
    emitter.emit_environment_descriptors()?;
    emitter.emit_vtables()?;
    emitter.emit_callable_descriptors()?;
    emitter.value_callbacks = emitter.emit_selected_value_callbacks()?;
    emitter.emit_actor_descriptors()?;
    emitter.emit_functions()?;
    emitter.emit_entry()?;
    if let Some(export) = host {
        host::emit(&emitter, export)?;
    }
    emitter
        .llvm
        .verify()
        .map_err(|error| CodegenError::LlvmVerify(error.to_string()))?;
    if emitter.llvm.get_function("llvm.coro.id").is_some() {
        coro::lower(&emitter.llvm, machine)?;
    }
    Ok(emitter.llvm)
}

/// The module-private array of compiled `*HewRegex` handles, one slot per
/// regex literal, filled in the process entry prologue.
const REGEX_HANDLES: &str = "hew_regex_handles";

/// The single tagged-variant carrier a runtime operation's result needs.
fn variant_carrier(action: PhysicalRuntimeAction) -> CodegenResult<PhysicalVariantId> {
    match action.carrier {
        PhysicalRuntimeCarrier::Variant(id) => Ok(id),
        _ => Err(CodegenError::FailClosed(format!(
            "runtime operation `{:?}` has no optional result carrier",
            action.family
        ))),
    }
}

fn regex_slot_count(module: &PhysicalModule) -> CodegenResult<Option<u32>> {
    if module.regex_patterns.is_empty() {
        return Ok(None);
    }
    u32::try_from(module.regex_patterns.len())
        .map(Some)
        .map_err(|_| CodegenError::FailClosed("regex literal count exceeds the ABI".into()))
}

fn regex_handles<'ctx>(llvm: &Module<'ctx>) -> CodegenResult<inkwell::values::GlobalValue<'ctx>> {
    llvm.get_global(REGEX_HANDLES).ok_or_else(|| {
        CodegenError::FailClosed("physical module has regex literals but no handle array".into())
    })
}

impl<'ctx> ModuleEmitter<'ctx, '_> {
    fn emit_collection_value_descriptors(&self) -> CodegenResult<()> {
        for glue in &self.module.vector_glue {
            self.emit_value_descriptor(&vector_descriptor_symbol(glue.id), &glue.element)?;
        }
        for glue in &self.module.map_glue {
            self.emit_value_descriptor(&map_value_descriptor_symbol(glue.id), &glue.value)?;
        }
        for glue in &self.module.shared_glue {
            let Some(action) = glue.payload.destroy else {
                continue;
            };
            let layout = self.module.target.layout(&glue.payload.ty).ok_or_else(|| {
                CodegenError::FailClosed("shared payload has no target layout".into())
            })?;
            self.emit_value_drop_callback(&shared_payload_drop_symbol(glue.id), layout, action)?;
        }
        Ok(())
    }

    /// The module's compiled-regex handle array, one null slot per literal.
    ///
    /// The slots are filled once in the process entry prologue, before any
    /// user body or actor runs, so a match arm only loads its slot.
    fn emit_regex_handles(&self) -> CodegenResult<()> {
        let Some(count) = regex_slot_count(self.module)? else {
            return Ok(());
        };
        if self.module.entry_callable.is_none() {
            // The slots are filled by the process entry. Without one they
            // would stay null and every arm would silently fail to match.
            return Err(CodegenError::FailClosed(
                "a regex literal needs a process entry to compile its pattern".into(),
            ));
        }
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let global = self
            .llvm
            .add_global(pointer.array_type(count), None, REGEX_HANDLES);
        global.set_linkage(Linkage::Private);
        global.set_initializer(&pointer.const_array(&vec![pointer.const_null(); count as usize]));
        Ok(())
    }

    /// Compile every regex literal into its slot. The checker already proved
    /// each pattern parses, so a null handle is an allocation failure: trap
    /// rather than let `hew_regex_match` read it as "no match".
    fn emit_regex_compilation(&self, builder: &Builder<'ctx>) -> CodegenResult<()> {
        let Some(count) = regex_slot_count(self.module)? else {
            return Ok(());
        };
        let handles = regex_handles(&self.llvm)?;
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let array_ty = pointer.array_type(count);
        let i32_ty = self.ctx.i32_type();
        let i64_ty = self.ctx.i64_type();
        let literal_new = get_or_declare_external(
            &self.llvm,
            "hew_string_literal_new",
            self.ctx
                .void_type()
                .fn_type(&[pointer.into(), i32_ty.into(), pointer.into()], false),
        )?;
        let compile = get_or_declare_external(
            &self.llvm,
            "hew_regex_compile",
            pointer.fn_type(&[pointer.into()], false),
        )?;
        let release = get_or_declare_external(
            &self.llvm,
            "hew_string_drop",
            self.ctx.void_type().fn_type(&[pointer.into()], false),
        )?;
        let text = builder
            .build_alloca(pointer, "regex.pattern")
            .llvm_ctx("allocate the regex pattern slot")?;
        for (index, pattern) in self.module.regex_patterns.iter().enumerate() {
            let len = u32::try_from(pattern.len()).map_err(|_| {
                CodegenError::FailClosed("regex pattern exceeds the u32 literal ABI".into())
            })?;
            let data = self.ctx.const_string(pattern.as_bytes(), false);
            let bytes = self
                .llvm
                .add_global(data.get_type(), None, "regex.pattern.bytes");
            bytes.set_linkage(Linkage::Private);
            bytes.set_constant(true);
            bytes.set_initializer(&data);
            builder
                .build_call(
                    literal_new,
                    &[
                        bytes.as_pointer_value().into(),
                        i32_ty.const_int(u64::from(len), false).into(),
                        text.into(),
                    ],
                    "",
                )
                .llvm_ctx("materialize a regex pattern string")?;
            let pattern_value = builder
                .build_load(pointer, text, "regex.pattern.value")
                .llvm_ctx("load the regex pattern string")?;
            let handle = builder
                .build_call(compile, &[pattern_value.into()], "regex.handle")
                .llvm_ctx("compile a regex literal")?
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| {
                    CodegenError::FailClosed("hew_regex_compile returned no handle".into())
                })?
                .into_pointer_value();
            builder
                .build_call(release, &[pattern_value.into()], "")
                .llvm_ctx("release the regex pattern string")?;
            let slot = unsafe {
                builder
                    .build_gep(
                        array_ty,
                        handles.as_pointer_value(),
                        &[i64_ty.const_zero(), i64_ty.const_int(index as u64, false)],
                        "regex.slot",
                    )
                    .llvm_ctx("address a regex handle slot")?
            };
            builder
                .build_store(slot, handle)
                .llvm_ctx("store a compiled regex handle")?;
        }
        Ok(())
    }

    fn emit_value_descriptor(&self, name: &str, recipe: &PhysicalValueRecipe) -> CodegenResult<()> {
        let value = self.value_descriptor(name, recipe)?;
        let global = self.llvm.add_global(value.get_type(), None, name);
        global.set_linkage(Linkage::Internal);
        global.set_constant(true);
        global.set_initializer(&value);
        Ok(())
    }

    /// All container slots use the same copy/drop ABI and value emitter.
    fn value_descriptor(
        &self,
        name: &str,
        recipe: &PhysicalValueRecipe,
    ) -> CodegenResult<inkwell::values::StructValue<'ctx>> {
        let target = TargetData::create(&self.module.target.data_layout);
        let size_ty = self.ctx.ptr_sized_int_type(&target, None);
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let descriptor_ty = value_descriptor_type(self.ctx, &target);
        let layout =
            self.module.target.layout(&recipe.ty).ok_or_else(|| {
                CodegenError::FailClosed("value recipe has no target layout".into())
            })?;
        let clone = match recipe.clone {
            Some(CloneAction::Bitwise) | None => pointer.const_null(),
            Some(action) => {
                self.emit_value_clone_callback(&format!("{name}_clone"), layout, action)?
            }
        };
        let drop = match recipe.destroy {
            None => pointer.const_null(),
            Some(action) => {
                self.emit_value_drop_callback(&format!("{name}_drop"), layout, action)?
            }
        };
        let ownership = if recipe.own == OwnKind::None {
            HewTypeOwnershipKind::Plain
        } else if recipe.ty == ResolvedTy::String {
            HewTypeOwnershipKind::String
        } else if recipe.ty == ResolvedTy::Bytes {
            HewTypeOwnershipKind::Bytes
        } else {
            HewTypeOwnershipKind::LayoutManaged
        };
        Ok(descriptor_ty.const_named_struct(&[
            size_ty.const_int(layout.size, false).into(),
            size_ty.const_int(u64::from(layout.align), false).into(),
            self.ctx.i8_type().const_int(ownership as u64, false).into(),
            clone.into(),
            drop.into(),
            self.emit_value_close_callback(&format!("{name}_close"), layout, recipe.destroy)?
                .into(),
        ]))
    }

    fn emit_value_clone_callback(
        &self,
        name: &str,
        layout: &PhysicalLayout,
        action: CloneAction,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let function = self.llvm.add_function(
            name,
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
        if action == CloneAction::Callable {
            let clone = callable::callable_clone_function(self.ctx, &self.llvm)?;
            let status = builder
                .build_call(
                    clone,
                    &[source.into(), destination.into()],
                    "callable.clone.status",
                )
                .llvm_ctx("copy nested callable environment")?
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| {
                    CodegenError::FailClosed("callable clone returned no status".into())
                })?;
            builder
                .build_return(Some(&status))
                .llvm_ctx("forward callable value clone status")?;
            return Ok(function.as_global_value().as_pointer_value());
        }
        let original = builder
            .build_load(llvm_type(self.ctx, &layout.repr)?, source, "element.source")
            .llvm_ctx("load borrowed value")?;
        let cloned = emitter.clone_loaded_value(original, layout, action)?;
        builder
            .build_store(destination, cloned)
            .llvm_ctx("initialize copied value")?;
        builder
            .build_return(Some(&self.ctx.i32_type().const_zero()))
            .llvm_ctx("finish element clone")?;
        Ok(function.as_global_value().as_pointer_value())
    }

    fn emit_value_drop_callback(
        &self,
        name: &str,
        layout: &PhysicalLayout,
        action: DestroyAction,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let function = self.llvm.add_function(
            name,
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
            .llvm_ctx("load owned value")?;
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
            if callable.is_resumable {
                params.push(ptr.into());
                let ramp = self.llvm.add_function(
                    &format!("{symbol}$resume"),
                    ptr.fn_type(&params, false),
                    Some(Linkage::Internal),
                );
                self.ramps.insert(callable.id, ramp);
            }
        }
        Ok(())
    }

    fn emit_functions(&self) -> CodegenResult<()> {
        for function in &self.module.functions {
            let callable = callable(self.module, function.callable)?;
            let values = if callable.is_resumable {
                &self.ramps
            } else {
                &self.functions
            };
            let value = *values.get(&function.callable).ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "physical callable {} has no LLVM declaration",
                    function.callable.0
                ))
            })?;
            FunctionEmitter::new(self, function, callable, value)?.emit()?;
            if callable.is_resumable {
                self.emit_sync_wrapper(callable)?;
            }
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
        self.emit_process_runtime_start(&builder, wrapper)?;
        self.emit_regex_compilation(&builder)?;
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
        // The status the entry body returned is the fault's private tag, not a
        // process exit code (HEW-SPEC-2026 5.8): an unrecovered trap or panic
        // reports `1` after its typed line reaches stderr. The tag stays
        // internal, and `hew_native_runtime_finish` keeps this `1` because a
        // deliberate non-zero code is never overwritten.
        let failed = self.ctx.i32_type().const_int(1, false);
        let failed = self.emit_process_runtime_finish(&builder, failed)?;
        builder
            .build_return(Some(&failed))
            .llvm_ctx("return physical failure status")?;

        builder.position_at_end(success);
        let exit = emit_entry_success(self.ctx, &builder, result, plan.action.clone(), callable)?;
        let exit = self.emit_process_runtime_finish(&builder, exit)?;
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
        let frame = if callable.is_resumable {
            let state = value
                .get_last_param()
                .ok_or_else(|| {
                    CodegenError::FailClosed("resumable body has no invocation state".into())
                })?
                .into_pointer_value();
            Some(coro::begin(ctx, &module.llvm, &builder, value, state)?)
        } else {
            None
        };
        let slots = partial::allocate_storage(module, function, callable, value, &builder)?;
        let place_flags = partial::allocate_flags(module, function, &builder)?;
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

        let mut fault_parks = BTreeMap::new();
        for block in &function.blocks {
            if let PhysicalTerminator::EnterDefer { park, .. } = block.terminator {
                if let std::collections::btree_map::Entry::Vacant(entry) = fault_parks.entry(park) {
                    let pointer = builder
                        .build_alloca(
                            ctx.ptr_type(AddressSpace::default()),
                            &format!("park.{}.fault", park.0),
                        )
                        .llvm_ctx("allocate defer fault park")?;
                    let status = builder
                        .build_alloca(ctx.i32_type(), &format!("park.{}.status", park.0))
                        .llvm_ctx("allocate defer status park")?;
                    builder
                        .build_store(pointer, ctx.ptr_type(AddressSpace::default()).const_null())
                        .llvm_ctx("initialize defer fault park")?;
                    builder
                        .build_store(status, ctx.i32_type().const_zero())
                        .llvm_ctx("initialize defer status park")?;
                    entry.insert((pointer, status));
                }
            }
        }
        let mut task_scopes = BTreeMap::new();
        for op in function.blocks.iter().flat_map(|block| &block.ops) {
            if let PhysicalOp::TaskScopeEnter { scope, .. } = op {
                let slot = builder
                    .build_alloca(
                        ctx.ptr_type(AddressSpace::default()),
                        &format!("task.scope.{}", scope.0),
                    )
                    .llvm_ctx("allocate task scope slot")?;
                task_scopes.insert(*scope, slot);
            }
        }
        let mut param_index = 0u32;
        for ((parameter, storage_id), physical_param) in value
            .get_params()
            .into_iter()
            .zip(&function.parameters)
            .zip(&callable.params)
        {
            if physical_param.passing == hew_mir::physical::SemParamPassing::BorrowMut {
                param_index += 1;
                continue;
            }
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
            place_flags,
            result_out,
            fault_out,
            active_fault,
            active_status,
            fault_parks,
            functions: &module.functions,
            value_callbacks: &module.value_callbacks,
            ramps: &module.ramps,
            frame,
            task_scopes,
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
        let expected = llvm_type(self.ctx, &self.storage(id)?.layout.repr)?;
        if value.get_type() != expected {
            return Err(CodegenError::FailClosed(format!(
                "physical storage {} expects {}, received {}",
                id.0,
                expected.print_to_string(),
                value.get_type().print_to_string()
            )));
        }
        self.builder
            .build_store(self.slots[id.0 as usize], value)
            .llvm_ctx("store physical storage")?;
        self.set_capture_initialized(id, true)?;
        self.set_place_initialized(id, true)?;
        Ok(())
    }

    fn clear_owned(&self, id: StorageId) -> CodegenResult<()> {
        self.set_capture_initialized(id, false)?;
        self.set_place_initialized(id, false)?;
        if self.function.place_storage.contains_key(&id) {
            return Ok(());
        }
        if self.storage(id)?.own == OwnKind::Owned {
            let zero = llvm_type(self.ctx, &self.storage(id)?.layout.repr)?.const_zero();
            self.builder
                .build_store(self.slots[id.0 as usize], zero)
                .llvm_ctx("clear transferred physical owner")?;
        }
        Ok(())
    }

    #[allow(
        clippy::too_many_lines,
        reason = "the physical operation match is deliberately exhaustive and contains no ownership selection"
    )]
    fn emit_op(&self, operation: &PhysicalOp) -> CodegenResult<()> {
        match operation {
            PhysicalOp::GeneratorMake { callable, dest, .. } => {
                self.emit_generator_make(*callable, *dest)
            }
            PhysicalOp::StreamPipe {
                capacity,
                stream,
                sink,
                ..
            } => self.emit_stream_pipe(*capacity, *stream, *sink),
            PhysicalOp::RegisterDefer { .. } => Ok(()),
            PhysicalOp::FunctionMake { dest, callee } => self.emit_function_make(*dest, *callee),
            PhysicalOp::TaskScopeEnter {
                scope,
                parent,
                duration,
            } => self.emit_task_scope_enter(*scope, *parent, *duration),
            PhysicalOp::TaskScopeClose { scope } => self.emit_task_scope_close(*scope),
            PhysicalOp::TaskSpawn {
                scope,
                callable,
                dest,
                ..
            } => self.emit_task_spawn(*scope, *callable, *dest),
            PhysicalOp::ClosureMake {
                dest,
                closure,
                fields,
            } => self.emit_closure_make(*dest, *closure, fields),
            PhysicalOp::DynMake {
                dest,
                vtable,
                source,
            } => self.emit_dyn_make(*dest, *vtable, *source),
            PhysicalOp::CallableCoerce { dest, source } => {
                let value = self.load(*source, "callable.coerce")?;
                self.store(*dest, value)?;
                self.clear_owned(*source)
            }

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
                    UnaryOp::Negate if source_value.is_float_value() => self
                        .builder
                        .build_float_neg(source_value.into_float_value(), "float.negate")
                        .llvm_ctx("emit IEEE floating negation")?
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
            PhysicalOp::ArrayMake { dest, fields, glue } => {
                self.emit_array_make(*dest, fields, *glue)
            }
            PhysicalOp::ArrayRepeat { dest, seed, glue } => {
                self.emit_array_repeat(*dest, *seed, *glue)
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
            PhysicalOp::AggregateProjectBorrow {
                dest,
                aggregate,
                field,
                ..
            } => {
                let aggregate = self
                    .load(*aggregate, "aggregate.borrow.source")?
                    .into_struct_value();
                let value = self
                    .builder
                    .build_extract_value(aggregate, *field, "aggregate.borrow.field")
                    .llvm_ctx("extract verified borrowed aggregate field")?;
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
            PhysicalOp::VariantIs {
                dest,
                source,
                variant,
                glue,
            } => {
                let (tag, _, _) = self.load_variant_tag(*source, *glue)?;
                let matches = self
                    .builder
                    .build_int_compare(
                        IntPredicate::EQ,
                        tag,
                        tag.get_type().const_int(u64::from(*variant), false),
                        "variant.is",
                    )
                    .llvm_ctx("compare physical variant tag")?;
                let bool_ty =
                    llvm_type(self.ctx, &self.storage(*dest)?.layout.repr)?.into_int_type();
                let value = if matches.get_type() == bool_ty {
                    matches
                } else {
                    self.builder
                        .build_int_z_extend(matches, bool_ty, "bool.widen")
                        .llvm_ctx("widen physical variant test")?
                };
                self.store(*dest, value.into())
            }
            PhysicalOp::VariantProjectCopy {
                dest,
                source,
                variant,
                field,
                glue,
                action,
            } => {
                let payload = self.variant_field_payload(*source, *variant, *glue)?;
                let field_value = self
                    .builder
                    .build_extract_value(payload, *field, "variant.project.field")
                    .llvm_ctx("extract physical variant field for copy")?;
                let value = self.value_emitter().clone_loaded_value(
                    field_value,
                    &self.storage(*dest)?.layout,
                    *action,
                )?;
                self.store(*dest, value)
            }
            PhysicalOp::VariantProjectBorrow {
                dest,
                source,
                variant,
                field,
                glue,
            } => {
                let payload = self.variant_field_payload(*source, *variant, *glue)?;
                let value = self
                    .builder
                    .build_extract_value(payload, *field, "variant.borrow.field")
                    .llvm_ctx("extract verified borrowed variant field")?;
                self.store(*dest, value)
            }
            PhysicalOp::VariantDestructure {
                source,
                variant,
                fields,
                glue,
            } => {
                let (payload, layout, object) =
                    self.load_variant_payload(*source, *variant, *glue)?;
                if let Some(payload) = payload {
                    for (index, field) in fields.iter().enumerate() {
                        let index = u32::try_from(index).map_err(|_| {
                            CodegenError::FailClosed("variant field index exceeds u32".into())
                        })?;
                        let value = self
                            .builder
                            .build_extract_value(payload, index, "variant.destructure.field")
                            .llvm_ctx("extract physical variant payload field")?;
                        self.store(*field, value)?;
                    }
                }
                if layout.is_indirect {
                    self.value_emitter().free_variant_node(object, layout)?;
                }
                self.clear_owned(*source)
            }
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
            PhysicalOp::Destroy { source, action, .. } => self.destroy_value(*source, *action),
            PhysicalOp::Borrow { dest, source } => {
                let value = self.load(*source, "borrow")?;
                self.store(*dest, value)
            }
            PhysicalOp::EndBorrow { .. } => Ok(()),
            PhysicalOp::StorageLive { storage } => self.set_place_initialized(*storage, false),
            PhysicalOp::Assign {
                dest,
                source,
                destroy_old,
            } => {
                if let Some(action) = destroy_old {
                    self.destroy_value(*dest, *action)?;
                }
                let value = self.load(*source, "assign")?;
                self.store(*dest, value)?;
                self.clear_owned(*source)
            }
            PhysicalOp::StorageDead {
                storage, destroy, ..
            } => {
                if self.destroy_place_contents(*storage)? {
                    return Ok(());
                }
                // A deferred actor seat (D447) has no local partition: the
                // verifier proved it initialized here, so release directly.
                if matches!(
                    self.storage(*storage)?.origin,
                    hew_mir::physical::StorageOrigin::ActorState {
                        initialized: false,
                        ..
                    }
                ) {
                    if let Some(action) = destroy {
                        let value = self.load(*storage, "seat.release")?;
                        self.value_emitter().destroy_loaded_value(
                            value,
                            &self.storage(*storage)?.layout,
                            *action,
                        )?;
                    }
                    return Ok(());
                }
                Err(CodegenError::FailClosed(
                    "local lifetime lacks a verified content partition".into(),
                ))
            }
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
        self.value_emitter()
            .write_variant_value(destination, variant, fields, glue_id)
    }

    /// Read the tag of one enum value with its layout and tag-and-payload object.
    fn load_variant_tag(
        &self,
        source: StorageId,
        glue_id: PhysicalVariantId,
    ) -> CodegenResult<(IntValue<'ctx>, &PhysicalVariantLayout, PointerValue<'ctx>)> {
        let values = self.value_emitter();
        let glue = values.variant_glue(glue_id)?;
        let layout = values.variant_layout(&glue.ty)?;
        let object = values.variant_object_ptr(self.slots[source.0 as usize], layout)?;
        let tag = values.load_variant_tag(object, layout)?;
        Ok((tag, layout, object))
    }

    /// Load one tested case's payload, absent for a payload-free case, with
    /// the object it came from. A different runtime tag is corrupt
    /// representation and traps, exactly like an unmatched switch arm.
    fn load_variant_payload(
        &self,
        source: StorageId,
        variant: u32,
        glue_id: PhysicalVariantId,
    ) -> CodegenResult<(
        Option<StructValue<'ctx>>,
        &PhysicalVariantLayout,
        PointerValue<'ctx>,
    )> {
        let (tag, layout, object) = self.load_variant_tag(source, glue_id)?;
        let matches = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                tag,
                tag.get_type().const_int(u64::from(variant), false),
                "variant.project.tested",
            )
            .llvm_ctx("compare physical variant tag")?;
        let valid = self.ctx.append_basic_block(self.value, "variant.project");
        let invalid = self.ctx.append_basic_block(self.value, "variant.invalid");
        self.builder
            .build_conditional_branch(matches, valid, invalid)
            .llvm_ctx("branch on physical variant tag")?;
        self.builder.position_at_end(invalid);
        self.value_emitter().emit_invalid_variant_tag()?;
        self.builder.position_at_end(valid);
        let payload_ty =
            llvm_type(self.ctx, &layout.variants[variant as usize].repr)?.into_struct_type();
        if payload_ty.count_fields() == 0 {
            return Ok((None, layout, object));
        }
        let payload_ptr = self.value_emitter().variant_payload_ptr(object, layout)?;
        let payload = self
            .builder
            .build_load(payload_ty, payload_ptr, "variant.project.payload")
            .llvm_ctx("load physical variant payload")?
            .into_struct_value();
        Ok((Some(payload), layout, object))
    }

    /// The payload a field projection reads; a projected case always has one.
    fn variant_field_payload(
        &self,
        source: StorageId,
        variant: u32,
        glue_id: PhysicalVariantId,
    ) -> CodegenResult<StructValue<'ctx>> {
        self.load_variant_payload(source, variant, glue_id)?
            .0
            .ok_or_else(|| {
                CodegenError::FailClosed("variant projection reads a payload-free case".into())
            })
    }

    fn emit_variant_switch(
        &self,
        scrutinee: StorageId,
        glue_id: PhysicalVariantId,
        arms: &[PhysicalVariantArm],
    ) -> CodegenResult<()> {
        let (tag, layout, object) = self.load_variant_tag(scrutinee, glue_id)?;
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
                let payload_ptr = self.value_emitter().variant_payload_ptr(object, layout)?;
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
            if layout.is_indirect {
                self.value_emitter().free_variant_node(object, layout)?;
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
            // Physical MIR already derived the exact destination-width bit
            // pattern, so the backend emits it verbatim: no sign inference, no
            // widening decision.
            PhysicalConst::IntegerBits(bits) => {
                self.store(dest, llvm_ty.into_int_type().const_int(*bits, false).into())
            }
            // Duration keeps its own signed `i64` semantic type.
            PhysicalConst::Duration(value) => self.store(
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
            PhysicalConst::Float(value) => {
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
        global.set_linkage(Linkage::Private);
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
                emit_float_binary(&self.builder, op, left, right)?
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
                // Plain `fptosi`/`fptoui` are LLVM poison for out-of-range and
                // non-finite inputs. The spec guarantees saturating semantics
                // (MAX/MIN on overflow, 0 on NaN) for every `as` float-to-int
                // cast, so the saturating intrinsics are the only correct
                // lowering here (see HEW-SPEC-2026.md's float-to-integer
                // cast table).
                let name = if is_signed(to) {
                    "llvm.fptosi.sat"
                } else {
                    "llvm.fptoui.sat"
                };
                let declaration = Intrinsic::find(name)
                    .and_then(|intrinsic| {
                        intrinsic
                            .get_declaration(self.llvm, &[target.into(), value.get_type().into()])
                    })
                    .ok_or_else(|| {
                        CodegenError::FailClosed(format!(
                            "LLVM cast intrinsic `{name}` is unavailable"
                        ))
                    })?;
                self.runtime_call_value(declaration, &[value.into()], "float.to.int.sat")?
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
        if self.destroy_place_contents(source)? {
            return Ok(());
        }
        let value = self.load(source, "destroy.source")?;
        self.clear_owned(source)?;
        self.value_emitter()
            .destroy_loaded_value(value, &self.storage(source)?.layout, action)
    }

    fn emit_terminator(&self, block: &PhysicalBlock) -> CodegenResult<()> {
        match &block.terminator {
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
            PhysicalTerminator::ChannelRecv { .. } => self.emit_channel_recv(block),
            PhysicalTerminator::ChannelSend { .. } => self.emit_channel_send(block),
            PhysicalTerminator::StreamSend { .. } => self.emit_stream_send(block),
            PhysicalTerminator::ValueClose {
                index,
                destroy,
                generator,
                conditional,
                next,
            } => self.emit_value_close(*generator, *index, *destroy, *conditional, next),
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
            } => self.emit_call(*callee, args, *result, normal.as_ref(), unwind.as_ref()),
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
            } => self.emit_extern_call(symbol, args, *result, result_abi, normal),
            PhysicalTerminator::Panic { message, cleanup } => self.emit_panic(*message, cleanup),
            PhysicalTerminator::Trap(kind) => {
                let code = trap_code(*kind);
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
    fn emit_result_edge(
        &self,
        result: Option<StorageId>,
        edge: &PhysicalEdge,
    ) -> CodegenResult<()> {
        if let Some(result) = result {
            self.set_place_initialized(result, true)?;
        }
        self.emit_edge(edge)
    }

    fn emit_edge(&self, edge: &PhysicalEdge) -> CodegenResult<()> {
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
        if callee.is_resumable {
            let status = self.emit_resumable_call(callee_id, &arguments, &moved)?;
            return self.emit_call_outcome(status, result, normal, unwind);
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
        self.emit_call_outcome(status, result, normal, unwind)
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
        let status = self
            .builder
            .build_call(callback, &arguments, "value.call.status")
            .llvm_ctx("emit selected value call")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| {
                CodegenError::FailClosed("selected value callback returned no status".into())
            })?
            .into_int_value();
        self.builder
            .build_store(self.active_status, status)
            .llvm_ctx("store selected value call status")?;
        self.emit_call_outcome(status, Some(result), Some(normal), Some(unwind))
    }

    fn emit_call_outcome(
        &self,
        status: IntValue<'ctx>,
        result: Option<StorageId>,
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
            PhysicalRuntimeCarrier::Variant(option)
                if action.family == RuntimeCallFamily::WeakUpgradeRc =>
            {
                return self.emit_weak_upgrade(option, transfers, required_result()?, normal);
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
            RuntimeCallFamily::ChannelSenderClose => {
                self.emit_direct_runtime_call(
                    hew_types::RuntimeCallFamily::ChannelSenderClose,
                    transfers,
                    result,
                )?;
            }
            RuntimeCallFamily::ChannelReceiverClose => {
                self.emit_direct_runtime_call(
                    hew_types::RuntimeCallFamily::ChannelReceiverClose,
                    transfers,
                    result,
                )?;
            }
            RuntimeCallFamily::ChannelPairNew => {
                self.emit_direct_runtime_call(
                    hew_types::RuntimeCallFamily::ChannelPairNew,
                    transfers,
                    result,
                )?;
            }
            RuntimeCallFamily::ChannelPairFree => {
                self.emit_direct_runtime_call(
                    hew_types::RuntimeCallFamily::ChannelPairFree,
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
            RuntimeCallFamily::ChannelPairIsValid => {
                self.emit_direct_runtime_call(
                    hew_types::RuntimeCallFamily::ChannelPairIsValid,
                    transfers,
                    result,
                )?;
            }
            RuntimeCallFamily::ChannelSenderClone => {
                self.emit_direct_runtime_call(
                    hew_types::RuntimeCallFamily::ChannelSenderClone,
                    transfers,
                    result,
                )?;
            }
            RuntimeCallFamily::ChannelPairSender => {
                self.emit_direct_runtime_call(
                    hew_types::RuntimeCallFamily::ChannelPairSender,
                    transfers,
                    result,
                )?;
            }
            RuntimeCallFamily::ChannelPairReceiver => {
                self.emit_direct_runtime_call(
                    hew_types::RuntimeCallFamily::ChannelPairReceiver,
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
                        "hew_node_api_connect",
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
            family @ (RuntimeCallFamily::InstantNow
            | RuntimeCallFamily::InstantElapsed
            | RuntimeCallFamily::InstantDurationSince
            | RuntimeCallFamily::DurationNanos
            | RuntimeCallFamily::DurationMicros
            | RuntimeCallFamily::DurationMillis
            | RuntimeCallFamily::DurationSecs
            | RuntimeCallFamily::DurationMins
            | RuntimeCallFamily::DurationHours
            | RuntimeCallFamily::DurationAbs
            | RuntimeCallFamily::DurationIsZero) => {
                // The semantic contract is the one authority for how many
                // arguments cross and whether the result is a predicate; the
                // C ABI carries every operand as `i64` and every predicate as
                // `i32`.
                let contract = family.semantic_contract().ok_or_else(|| {
                    CodegenError::FailClosed(format!(
                        "time family `{family:?}` has no semantic contract"
                    ))
                })?;
                let i64_ty = self.ctx.i64_type();
                let predicate = matches!(
                    contract.result,
                    hew_types::runtime_call::RuntimeResultEffect::BitCopy(
                        hew_types::runtime_call::RuntimeValueKind::Bool
                    )
                );
                let return_type = if predicate {
                    self.ctx.i32_type()
                } else {
                    i64_ty
                };
                let function = get_or_declare_external(
                    self.llvm,
                    family.c_symbol(),
                    return_type.fn_type(&vec![i64_ty.into(); contract.arguments.len()], false),
                )?;
                let arguments = (0..contract.arguments.len())
                    .map(|index| Ok(self.load(source(index)?, "time.argument")?.into()))
                    .collect::<CodegenResult<Vec<_>>>()?;
                let value = self
                    .runtime_call_value(function, &arguments, "time.scalar")?
                    .into_int_value();
                let dest = required_result()?;
                if predicate {
                    let truth = self
                        .builder
                        .build_int_compare(
                            IntPredicate::NE,
                            value,
                            return_type.const_zero(),
                            "time.predicate.truth",
                        )
                        .llvm_ctx("normalize time predicate")?;
                    let bool_ty =
                        llvm_type(self.ctx, &self.storage(dest)?.layout.repr)?.into_int_type();
                    let truth = self
                        .builder
                        .build_int_z_extend(truth, bool_ty, "time.predicate.bool")
                        .llvm_ctx("widen time predicate result")?;
                    self.store(dest, truth.into())?;
                } else {
                    self.store(dest, value.into())?;
                }
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
            RuntimeCallFamily::BytesLen => {
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
                self.store(required_result()?, value)?;
            }
            RuntimeCallFamily::BytesIsEmpty => {
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_bytes_is_empty",
                    self.ctx.bool_type().fn_type(&[ptr.into()], false),
                )?;
                let value = self
                    .runtime_call_value(
                        function,
                        &[self.slots[source(0)?.0 as usize].into()],
                        "bytes.is_empty",
                    )?
                    .into_int_value();
                let result = required_result()?;
                let bool_ty =
                    llvm_type(self.ctx, &self.storage(result)?.layout.repr)?.into_int_type();
                let value = self
                    .builder
                    .build_int_z_extend(value, bool_ty, "bytes.is_empty.bool")
                    .llvm_ctx("widen bytes is_empty result")?;
                self.store(result, value.into())?;
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
            RuntimeCallFamily::BytesContains => {
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_bytes_contains",
                    self.ctx
                        .bool_type()
                        .fn_type(&[ptr.into(), self.ctx.i8_type().into()], false),
                )?;
                let value = self
                    .runtime_call_value(
                        function,
                        &[
                            self.slots[source(0)?.0 as usize].into(),
                            self.load(source(1)?, "bytes.contains.byte")?.into(),
                        ],
                        "bytes.contains",
                    )?
                    .into_int_value();
                let result = required_result()?;
                let bool_ty =
                    llvm_type(self.ctx, &self.storage(result)?.layout.repr)?.into_int_type();
                let value = self
                    .builder
                    .build_int_z_extend(value, bool_ty, "bytes.contains.bool")
                    .llvm_ctx("widen bytes contains result")?;
                self.store(result, value.into())?;
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
            IntBitOp::SwapBytes => self.call_intrinsic1("llvm.bswap", receiver_int)?.into(),
            IntBitOp::ReverseBits => self
                .call_intrinsic1("llvm.bitreverse", receiver_int)?
                .into(),
            IntBitOp::RotateLeft | IntBitOp::RotateRight => {
                let shift_arg = arguments.get(1).copied().ok_or_else(|| {
                    CodegenError::FailClosed("physical rotate lacks its shift amount".into())
                })?;
                let shift = self
                    .builder
                    .build_int_z_extend_or_bit_cast(
                        shift_arg.into_int_value(),
                        receiver_ty,
                        "int_method.rotate_shift",
                    )
                    .llvm_ctx("widen rotate shift amount to the receiver's width")?;
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
        let width_ok = match width {
            IntMethodWidth::I32 | IntMethodWidth::U32 => receiver_ty.get_bit_width() == 32,
            IntMethodWidth::I64 | IntMethodWidth::U64 => receiver_ty.get_bit_width() == 64,
        };
        if !width_ok {
            return Err(CodegenError::FailClosed(format!(
                "integer method `{op:?}` carried width {width:?} but its receiver is {} bits",
                receiver_ty.get_bit_width()
            )));
        }
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
        let signed = matches!(width, IntMethodWidth::I32 | IntMethodWidth::I64);
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
        };
        self.store(result, value.into())?;
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

    /// Truncate an intrinsic result (the receiver's own width) down to the
    /// `u32` that every `count_ones`/`count_zeros`/`leading_zeros`/
    /// `trailing_zeros` method returns, regardless of receiver width.
    fn narrow_to_u32(
        &self,
        value: inkwell::values::IntValue<'ctx>,
    ) -> CodegenResult<inkwell::values::BasicValueEnum<'ctx>> {
        if value.get_type().get_bit_width() == 32 {
            return Ok(value.into());
        }
        Ok(self
            .builder
            .build_int_truncate(value, self.ctx.i32_type(), "int_method.narrow")
            .llvm_ctx("narrow bit-count result to u32")?
            .into())
    }

    fn new_array_storage(
        &self,
        glue: PhysicalVectorId,
    ) -> CodegenResult<inkwell::values::PointerValue<'ctx>> {
        let values = self.value_emitter();
        let descriptor = values.vector_glue(glue)?;
        let ResolvedTy::Array(_, length) = &descriptor.ty else {
            return Err(CodegenError::FailClosed(
                "array construction has a non-array descriptor".into(),
            ));
        };
        let layout = self
            .llvm
            .get_global(&vector_descriptor_symbol(glue))
            .ok_or_else(|| {
                CodegenError::FailClosed("array element descriptor was not emitted".into())
            })?;
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let constructor = get_or_declare_external(
            self.llvm,
            "hew_vec_new_with_elem_layout_capacity",
            pointer.fn_type(&[pointer.into(), self.ctx.i64_type().into()], false),
        )?;
        Ok(self
            .runtime_call_value(
                constructor,
                &[
                    layout.as_pointer_value().into(),
                    self.ctx.i64_type().const_int(*length, false).into(),
                ],
                "array.storage",
            )?
            .into_pointer_value())
    }

    fn emit_array_make(
        &self,
        dest: StorageId,
        fields: &[StorageId],
        glue: PhysicalVectorId,
    ) -> CodegenResult<()> {
        let array = self.new_array_storage(glue)?;
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let push = get_or_declare_external(
            self.llvm,
            "hew_vec_push_owned_move",
            self.ctx
                .void_type()
                .fn_type(&[pointer.into(), pointer.into()], false),
        )?;
        for field in fields {
            self.runtime_call_void(
                push,
                &[array.into(), self.slots[field.0 as usize].into()],
                "array.element",
            )?;
            self.clear_owned(*field)?;
        }
        self.store(dest, array.into())
    }

    fn emit_array_repeat(
        &self,
        dest: StorageId,
        seed: StorageId,
        glue: PhysicalVectorId,
    ) -> CodegenResult<()> {
        let values = self.value_emitter();
        let descriptor = values.vector_glue(glue)?;
        let ResolvedTy::Array(_, length) = &descriptor.ty else {
            return Err(CodegenError::FailClosed(
                "array repeat has a non-array descriptor".into(),
            ));
        };
        let array = self.new_array_storage(glue)?;
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let push_ty = self
            .ctx
            .void_type()
            .fn_type(&[pointer.into(), pointer.into()], false);
        if *length > 1 {
            let push = get_or_declare_external(self.llvm, "hew_vec_push_owned", push_ty)?;
            let before = self.builder.get_insert_block().ok_or_else(|| {
                CodegenError::FailClosed("array repeat has no insertion block".into())
            })?;
            let body = self.ctx.append_basic_block(self.value, "array.repeat.copy");
            let done = self.ctx.append_basic_block(self.value, "array.repeat.done");
            self.builder
                .build_unconditional_branch(body)
                .llvm_ctx("enter array repeat")?;
            self.builder.position_at_end(body);
            let i64_ty = self.ctx.i64_type();
            let index = self
                .builder
                .build_phi(i64_ty, "array.repeat.index")
                .llvm_ctx("array repeat counter")?;
            index.add_incoming(&[(&i64_ty.const_zero(), before)]);
            self.runtime_call_void(
                push,
                &[array.into(), self.slots[seed.0 as usize].into()],
                "array.repeat.element",
            )?;
            let next = self
                .builder
                .build_int_add(
                    index.as_basic_value().into_int_value(),
                    i64_ty.const_int(1, false),
                    "array.repeat.next",
                )
                .llvm_ctx("advance array repeat")?;
            let more = self
                .builder
                .build_int_compare(
                    IntPredicate::ULT,
                    next,
                    i64_ty.const_int(*length - 1, false),
                    "array.repeat.more",
                )
                .llvm_ctx("check array repeat limit")?;
            self.builder
                .build_conditional_branch(more, body, done)
                .llvm_ctx("continue array repeat")?;
            index.add_incoming(&[(&next, body)]);
            self.builder.position_at_end(done);
        }
        let push = get_or_declare_external(self.llvm, "hew_vec_push_owned_move", push_ty)?;
        self.runtime_call_void(
            push,
            &[array.into(), self.slots[seed.0 as usize].into()],
            "array.repeat.last",
        )?;
        self.clear_owned(seed)?;
        self.store(dest, array.into())
    }

    /// Load the compiled `*HewRegex` for the literal slot named by `index`.
    /// Every regex action addresses the module's handle array this way; the
    /// patterns are compiled once in the process entry's prologue.
    fn load_regex_handle(&self, index: StorageId) -> CodegenResult<BasicValueEnum<'ctx>> {
        let count = regex_slot_count(self.module)?.ok_or_else(|| {
            CodegenError::FailClosed("a regex operation needs a compiled pattern slot".into())
        })?;
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let handles = regex_handles(self.llvm)?;
        let index = self.load(index, "regex.index")?.into_int_value();
        let slot = unsafe {
            self.builder
                .build_gep(
                    ptr.array_type(count),
                    handles.as_pointer_value(),
                    &[self.ctx.i64_type().const_zero(), index],
                    "regex.slot",
                )
                .llvm_ctx("address the compiled regex slot")?
        };
        self.builder
            .build_load(ptr, slot, "regex.handle")
            .llvm_ctx("load the compiled regex handle")
    }

    fn emit_vector_index_guard(
        &self,
        vector: PointerValue<'ctx>,
        index: IntValue<'ctx>,
        failure: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let length_fn = get_or_declare_external(
            self.llvm,
            "hew_vec_len",
            self.ctx.i64_type().fn_type(&[pointer.into()], false),
        )?;
        let length = self
            .runtime_call_value(length_fn, &[vector.into()], "vector.index.length")?
            .into_int_value();
        // Unsigned comparison also rejects every negative signed index.
        let in_bounds = self
            .builder
            .build_int_compare(IntPredicate::ULT, index, length, "vector.index.in.bounds")
            .llvm_ctx("check vector mutation bounds")?;
        let safe = self.ctx.append_basic_block(self.value, "vector.index.safe");
        let failed = self
            .ctx
            .append_basic_block(self.value, "vector.index.failed");
        self.builder
            .build_conditional_branch(in_bounds, safe, failed)
            .llvm_ctx("select vector mutation outcome")?;
        self.builder.position_at_end(failed);
        self.emit_edge(failure)?;
        self.builder.position_at_end(safe);
        Ok(())
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
            return self.emit_result_edge(Some(result), normal);
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
            PhysicalVectorOp::Contains => {
                let equality = self
                    .value_callbacks
                    .get(&(glue.element.ty.clone(), hew_types::ValueCapability::Eq))
                    .ok_or_else(|| {
                        CodegenError::FailClosed(
                            "vector membership lacks its selected element equality".into(),
                        )
                    })?;
                let contains = self.emit_collection_callback(
                    "hew_vec_contains_checked",
                    &[
                        vector.into(),
                        self.slots[source(1)?.0 as usize].into(),
                        equality.as_global_value().as_pointer_value().into(),
                    ],
                    Some(failure()?),
                    None,
                )?;
                self.store(result, contains.into())?;
            }
            PhysicalVectorOp::Push | PhysicalVectorOp::Clear => {
                if operation == PhysicalVectorOp::Push {
                    let moved = matches!(transfers.get(1), Some(ArgumentTransfer::Move(_)));
                    let function = get_or_declare_external(
                        self.llvm,
                        if moved {
                            "hew_vec_push_owned_move"
                        } else {
                            "hew_vec_push_owned"
                        },
                        self.ctx
                            .void_type()
                            .fn_type(&[pointer.into(), pointer.into()], false),
                    )?;
                    self.runtime_call_void(
                        function,
                        &[vector.into(), self.slots[source(1)?.0 as usize].into()],
                        "vector.push",
                    )?;
                    if moved {
                        self.clear_owned(source(1)?)?;
                    }
                } else {
                    let function = external_drop(self.ctx, self.llvm, "hew_vec_clear")?;
                    self.runtime_call_void(function, &[vector.into()], "vector.clear")?;
                }
                self.clear_owned(receiver)?;
                self.store(result, vector.into())?;
            }
            PhysicalVectorOp::Set => {
                let index = self.load(source(1)?, "vector.set.index")?.into_int_value();
                self.emit_vector_index_guard(vector, index, failure()?)?;
                let moved = matches!(transfers.get(2), Some(ArgumentTransfer::Move(_)));
                let function = get_or_declare_external(
                    self.llvm,
                    if moved {
                        "hew_vec_set_owned_move"
                    } else {
                        "hew_vec_set_owned"
                    },
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
                if moved {
                    self.clear_owned(source(2)?)?;
                }
                self.clear_owned(receiver)?;
                self.store(result, vector.into())?;
            }
            PhysicalVectorOp::Index
            | PhysicalVectorOp::Get { .. }
            | PhysicalVectorOp::GetBorrow { .. } => {
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
                // A borrowed read aliases the slot the vector still owns; the
                // owning read hands back a fresh owner.
                let function = get_or_declare_external(
                    self.llvm,
                    if matches!(operation, PhysicalVectorOp::GetBorrow { .. }) {
                        "hew_vec_borrow_owned"
                    } else {
                        "hew_vec_get_clone"
                    },
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
                let optional = match operation {
                    PhysicalVectorOp::Get { result: option }
                    | PhysicalVectorOp::GetBorrow { result: option } => Some(option),
                    _ => None,
                };
                self.builder.position_at_end(absent);
                if let Some(option) = optional {
                    self.write_variant_value(self.slots[result.0 as usize], 1, &[], option)?;
                    self.emit_result_edge(Some(result), normal)?;
                } else {
                    self.emit_edge(failure()?)?;
                }
                self.builder.position_at_end(present);
                if let Some(option) = optional {
                    let element = self
                        .builder
                        .build_load(element_ty, output, "vector.get.value")
                        .llvm_ctx("load vector element")?;
                    self.write_variant_value(self.slots[result.0 as usize], 0, &[element], option)?;
                }
            }
            PhysicalVectorOp::IndexBorrow => {
                let element_layout =
                    self.module.target.layout(&glue.element.ty).ok_or_else(|| {
                        CodegenError::FailClosed("vector element lacks its target layout".into())
                    })?;
                let _ = llvm_type(self.ctx, &element_layout.repr)?;
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_vec_borrow_owned",
                    self.ctx
                        .bool_type()
                        .fn_type(&[pointer.into(), i64_ty.into(), pointer.into()], false),
                )?;
                let found = self
                    .runtime_call_value(
                        function,
                        &[
                            vector.into(),
                            self.load(source(1)?, "vector.borrow.index")?.into(),
                            self.slots[result.0 as usize].into(),
                        ],
                        "vector.borrow.found",
                    )?
                    .into_int_value();
                let present = self
                    .ctx
                    .append_basic_block(self.value, "vector.borrow.present");
                let absent = self
                    .ctx
                    .append_basic_block(self.value, "vector.borrow.absent");
                self.builder
                    .build_conditional_branch(found, present, absent)
                    .llvm_ctx("select vector borrow outcome")?;
                self.builder.position_at_end(absent);
                self.emit_edge(failure()?)?;
                self.builder.position_at_end(present);
            }
            PhysicalVectorOp::Slice | PhysicalVectorOp::SliceFrom => {
                let length_fn = get_or_declare_external(
                    self.llvm,
                    "hew_vec_len",
                    i64_ty.fn_type(&[pointer.into()], false),
                )?;
                let length = self
                    .runtime_call_value(length_fn, &[vector.into()], "vector.slice.length")?
                    .into_int_value();
                let start = self
                    .load(source(1)?, "vector.slice.start")?
                    .into_int_value();
                let end = if operation == PhysicalVectorOp::Slice {
                    self.load(source(2)?, "vector.slice.end")?.into_int_value()
                } else {
                    length
                };
                let start_negative = self
                    .builder
                    .build_int_compare(
                        IntPredicate::SLT,
                        start,
                        i64_ty.const_zero(),
                        "vector.slice.start.negative",
                    )
                    .llvm_ctx("guard negative vector slice start")?;
                let end_negative = self
                    .builder
                    .build_int_compare(
                        IntPredicate::SLT,
                        end,
                        i64_ty.const_zero(),
                        "vector.slice.end.negative",
                    )
                    .llvm_ctx("guard negative vector slice end")?;
                let inverted = self
                    .builder
                    .build_int_compare(IntPredicate::SGT, start, end, "vector.slice.inverted")
                    .llvm_ctx("guard inverted vector slice range")?;
                let past_end = self
                    .builder
                    .build_int_compare(IntPredicate::SGT, end, length, "vector.slice.past.end")
                    .llvm_ctx("guard vector slice upper bound")?;
                let out_of_bounds = self
                    .builder
                    .build_or(start_negative, end_negative, "vector.slice.bounds.a")
                    .and_then(|a| self.builder.build_or(a, inverted, "vector.slice.bounds.b"))
                    .and_then(|b| {
                        self.builder
                            .build_or(b, past_end, "vector.slice.bounds.condition")
                    })
                    .llvm_ctx("combine vector slice guards")?;
                let safe = self.ctx.append_basic_block(self.value, "vector.slice.safe");
                let failed = self
                    .ctx
                    .append_basic_block(self.value, "vector.slice.failure");
                self.builder
                    .build_conditional_branch(out_of_bounds, failed, safe)
                    .llvm_ctx("branch around fallible vector slice call")?;
                self.builder.position_at_end(failed);
                self.emit_edge(failure()?)?;
                self.builder.position_at_end(safe);
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_vec_slice_range_owned",
                    pointer.fn_type(&[pointer.into(), i64_ty.into(), i64_ty.into()], false),
                )?;
                let sliced = self.runtime_call_value(
                    function,
                    &[vector.into(), start.into(), end.into()],
                    "vector.slice",
                )?;
                self.store(result, sliced)?;
            }
            PhysicalVectorOp::Pop { result: tuple }
            | PhysicalVectorOp::Remove { result: tuple }
            | PhysicalVectorOp::TakeFirst { result: tuple } => {
                values.aggregate_glue(tuple)?;
                let layout = self.module.target.layout(&glue.element.ty).ok_or_else(|| {
                    CodegenError::FailClosed("vector element lacks its target layout".into())
                })?;
                let element_ty = llvm_type(self.ctx, &layout.repr)?;
                let output = values.entry_scratch(element_ty, "vector.pop.element")?;
                // Indexed removal and iteration share the same owning take;
                // pop selects the final element through its existing entry.
                let index = match operation {
                    PhysicalVectorOp::TakeFirst { .. } => Some(i64_ty.const_zero()),
                    PhysicalVectorOp::Remove { .. } => {
                        let index = self
                            .load(source(1)?, "vector.remove.index")?
                            .into_int_value();
                        self.emit_vector_index_guard(vector, index, failure()?)?;
                        Some(index)
                    }
                    _ => None,
                };
                let status = if let Some(index) = index {
                    let function = get_or_declare_external(
                        self.llvm,
                        "hew_vec_remove_at_owned",
                        self.ctx
                            .i32_type()
                            .fn_type(&[pointer.into(), i64_ty.into(), pointer.into()], false),
                    )?;
                    self.runtime_call_value(
                        function,
                        &[vector.into(), index.into(), output.into()],
                        "vector.take.status",
                    )?
                    .into_int_value()
                } else {
                    let function = get_or_declare_external(
                        self.llvm,
                        "hew_vec_pop_owned",
                        self.ctx
                            .i32_type()
                            .fn_type(&[pointer.into(), pointer.into()], false),
                    )?;
                    self.runtime_call_value(
                        function,
                        &[vector.into(), output.into()],
                        "vector.pop.status",
                    )?
                    .into_int_value()
                };
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
        self.emit_result_edge(Some(result), normal)
    }

    fn descriptor_pointer(&self, symbol: &str) -> CodegenResult<PointerValue<'ctx>> {
        self.llvm
            .get_global(symbol)
            .map(|global| global.as_pointer_value())
            .ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "collection descriptor `{symbol}` was not emitted"
                ))
            })
    }

    fn store_receiver_pair(
        &self,
        result: StorageId,
        receiver: StorageId,
        value: BasicValueEnum<'ctx>,
    ) -> CodegenResult<()> {
        let pair_ty = llvm_type(self.ctx, &self.storage(result)?.layout.repr)?.into_struct_type();
        let pair = self
            .builder
            .build_insert_value(
                pair_ty.const_zero(),
                self.load(receiver, "collection.updated")?,
                0,
                "collection.pair.receiver",
            )
            .llvm_ctx("construct updated collection result")?
            .into_struct_value();
        let pair = self
            .builder
            .build_insert_value(pair, value, 1, "collection.pair.value")
            .llvm_ctx("construct collection value result")?
            .into_struct_value();
        self.clear_owned(receiver)?;
        self.store(result, pair.into())
    }

    #[expect(
        clippy::too_many_lines,
        reason = "map actions execute their exact runtime and storage contracts"
    )]
    fn emit_map_call(
        &self,
        action: (PhysicalMapOp, PhysicalMapId),
        transfers: &[ArgumentTransfer],
        result: StorageId,
        normal: &PhysicalEdge,
        failure: Option<&PhysicalEdge>,
    ) -> CodegenResult<()> {
        let (operation, glue) = action;
        let source = |index: usize| {
            transfers.get(index).map(argument_source).ok_or_else(|| {
                CodegenError::FailClosed(format!("map action lacks argument {index}"))
            })
        };
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        if operation == PhysicalMapOp::New {
            let key = self.descriptor_pointer(&map_key_descriptor_symbol(glue))?;
            let value = self.descriptor_pointer(&map_value_descriptor_symbol(glue))?;
            let function = get_or_declare_external(
                self.llvm,
                "hew_hashmap_new_with_layout",
                pointer.fn_type(&[pointer.into(), pointer.into()], false),
            )?;
            let map = self.runtime_call_value(function, &[key.into(), value.into()], "map.new")?;
            self.store(result, map)?;
            return self.emit_result_edge(Some(result), normal);
        }
        let receiver = source(0)?;
        let map = self.load(receiver, "map.receiver")?;
        match operation {
            PhysicalMapOp::New => unreachable!("constructor already emitted"),
            PhysicalMapOp::Get { .. }
            | PhysicalMapOp::GetBorrow { .. }
            | PhysicalMapOp::Index
            | PhysicalMapOp::Remove { .. } => {
                return self.emit_map_lookup(
                    action,
                    (receiver, source(1)?),
                    result,
                    normal,
                    failure,
                );
            }
            PhysicalMapOp::Len => {
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_hashmap_len_layout",
                    self.ctx.i64_type().fn_type(&[pointer.into()], false),
                )?;
                let len = self.runtime_call_value(function, &[map.into()], "map.length")?;
                self.store(result, len)?;
            }
            PhysicalMapOp::ContainsKey => {
                let contains = self.emit_collection_callback(
                    "hew_hashmap_contains_key_layout",
                    &[map.into(), self.slots[source(1)?.0 as usize].into()],
                    failure,
                    None,
                )?;
                self.store(result, contains.into())?;
            }
            PhysicalMapOp::Insert | PhysicalMapOp::Clear => {
                if operation == PhysicalMapOp::Insert {
                    // The adopted value transfers into the slot; the key is
                    // cloned in either entry point.
                    let moved = matches!(transfers.get(2), Some(ArgumentTransfer::Move(_)));
                    self.emit_collection_callback(
                        if moved {
                            "hew_hashmap_insert_take_layout"
                        } else {
                            "hew_hashmap_insert_clone_layout"
                        },
                        &[
                            map.into(),
                            self.slots[source(1)?.0 as usize].into(),
                            self.slots[source(2)?.0 as usize].into(),
                        ],
                        failure,
                        Some((receiver, DestroyAction::Map(glue))),
                    )?;
                    if moved {
                        self.clear_owned(source(2)?)?;
                    }
                } else {
                    let function = external_drop(self.ctx, self.llvm, "hew_hashmap_clear_layout")?;
                    self.runtime_call_void(function, &[map.into()], "map.clear")?;
                }
                self.clear_owned(receiver)?;
                self.store(result, map)?;
            }
            PhysicalMapOp::Keys | PhysicalMapOp::Values => {
                let symbol = if operation == PhysicalMapOp::Keys {
                    "hew_hashmap_keys_layout"
                } else {
                    "hew_hashmap_values_layout"
                };
                let function = external_unary_ptr(self.ctx, self.llvm, symbol)?;
                let vector = self.runtime_call_value(function, &[map.into()], "map.projection")?;
                self.store(result, vector)?;
            }
            PhysicalMapOp::Entries { result: vector } => {
                let values = self.value_emitter();
                let recipe = &values.vector_glue(vector)?.element;
                let layout = self.module.target.layout(&recipe.ty).ok_or_else(|| {
                    CodegenError::FailClosed("map entry has no target layout".into())
                })?;
                let pair_ty = llvm_type(self.ctx, &layout.repr)?.into_struct_type();
                let target = TargetData::create(&self.module.target.data_layout);
                let offset = target.offset_of_element(&pair_ty, 1).ok_or_else(|| {
                    CodegenError::FailClosed("map entry has no value offset".into())
                })?;
                let descriptor = self.descriptor_pointer(&vector_descriptor_symbol(vector))?;
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_hashmap_entries_layout",
                    pointer.fn_type(
                        &[pointer.into(), pointer.into(), self.ctx.i64_type().into()],
                        false,
                    ),
                )?;
                let entries = self.runtime_call_value(
                    function,
                    &[
                        map.into(),
                        descriptor.into(),
                        self.ctx.i64_type().const_int(offset, false).into(),
                    ],
                    "map.entries",
                )?;
                self.store(result, entries)?;
            }
        }
        self.emit_result_edge(Some(result), normal)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "lookup and removal share presence-sensitive output initialization"
    )]
    fn emit_map_lookup(
        &self,
        action: (PhysicalMapOp, PhysicalMapId),
        inputs: (StorageId, StorageId),
        result: StorageId,
        normal: &PhysicalEdge,
        failure: Option<&PhysicalEdge>,
    ) -> CodegenResult<()> {
        let (operation, id) = action;
        let (receiver, key) = inputs;
        let glue = self
            .module
            .map_glue
            .get(id.0 as usize)
            .filter(|glue| glue.id == id)
            .ok_or_else(|| CodegenError::FailClosed("unknown map descriptor".into()))?;
        let layout = self
            .module
            .target
            .layout(&glue.value.ty)
            .ok_or_else(|| CodegenError::FailClosed("map value has no target layout".into()))?;
        let value_ty = llvm_type(self.ctx, &layout.repr)?;
        let values = self.value_emitter();
        let output = if operation == PhysicalMapOp::Index {
            self.slots[result.0 as usize]
        } else {
            values.entry_scratch(value_ty, "map.lookup.value")?
        };
        let option = match operation {
            PhysicalMapOp::Get { result: option }
            | PhysicalMapOp::GetBorrow { result: option }
            | PhysicalMapOp::Remove { value: option, .. } => Some(option),
            PhysicalMapOp::Index => None,
            _ => return Err(CodegenError::FailClosed("non-lookup map action".into())),
        };
        let option_slot = if let Some(option) = option {
            let layout = &values
                .variant_layout(&values.variant_glue(option)?.ty)?
                .object;
            Some(values.entry_scratch(llvm_type(self.ctx, &layout.repr)?, "map.optional.value")?)
        } else {
            None
        };
        // A borrowed read aliases the value the map still owns; the owning
        // read hands back a fresh owner and the removal moves one out.
        let symbol = match operation {
            PhysicalMapOp::Remove { .. } => "hew_hashmap_remove_take_layout",
            PhysicalMapOp::GetBorrow { .. } => "hew_hashmap_get_borrow_layout",
            _ => "hew_hashmap_get_clone_layout",
        };
        let consumed = matches!(operation, PhysicalMapOp::Remove { .. })
            .then_some((receiver, DestroyAction::Map(id)));
        let found = self.emit_collection_callback(
            symbol,
            &[
                self.load(receiver, "map.lookup.receiver")?.into(),
                self.slots[key.0 as usize].into(),
                output.into(),
            ],
            failure,
            consumed,
        )?;
        let found = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                found,
                self.ctx.i8_type().const_zero(),
                "map.lookup.found",
            )
            .llvm_ctx("normalize map lookup presence")?;
        let present = self
            .ctx
            .append_basic_block(self.value, "map.lookup.present");
        let absent = self.ctx.append_basic_block(self.value, "map.lookup.absent");
        let complete = self
            .ctx
            .append_basic_block(self.value, "map.lookup.complete");
        self.builder
            .build_conditional_branch(found, present, absent)
            .llvm_ctx("select map lookup outcome")?;
        self.builder.position_at_end(absent);
        if let (Some(option), Some(slot)) = (option, option_slot) {
            self.write_variant_value(slot, 1, &[], option)?;
            self.builder
                .build_unconditional_branch(complete)
                .llvm_ctx("finish absent map value")?;
        } else {
            self.initialize_active_fault(HEW_TRAP_INDEX_OUT_OF_BOUNDS)?;
            self.emit_edge(failure.ok_or_else(|| {
                CodegenError::FailClosed("map index lacks its failure cleanup".into())
            })?)?;
        }
        self.builder.position_at_end(present);
        if let (Some(option), Some(slot)) = (option, option_slot) {
            let value = self
                .builder
                .build_load(value_ty, output, "map.lookup.owner")
                .llvm_ctx("load map value")?;
            self.write_variant_value(slot, 0, &[value], option)?;
        }
        self.builder
            .build_unconditional_branch(complete)
            .llvm_ctx("finish present map value")?;
        self.builder.position_at_end(complete);
        if let (Some(option), Some(slot)) = (option, option_slot) {
            let layout = &values
                .variant_layout(&values.variant_glue(option)?.ty)?
                .object;
            let value = self
                .builder
                .build_load(
                    llvm_type(self.ctx, &layout.repr)?,
                    slot,
                    "map.optional.owner",
                )
                .llvm_ctx("load initialized optional map value")?;
            if matches!(operation, PhysicalMapOp::Remove { .. }) {
                self.store_receiver_pair(result, receiver, value)?;
            } else {
                self.store(result, value)?;
            }
        }
        self.emit_result_edge(Some(result), normal)
    }

    fn emit_set_call(
        &self,
        action: (PhysicalSetOp, PhysicalSetId),
        transfers: &[ArgumentTransfer],
        result: StorageId,
        normal: &PhysicalEdge,
        failure: Option<&PhysicalEdge>,
    ) -> CodegenResult<()> {
        let (operation, glue) = action;
        let source = |index: usize| {
            transfers.get(index).map(argument_source).ok_or_else(|| {
                CodegenError::FailClosed(format!("set action lacks argument {index}"))
            })
        };
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        if operation == PhysicalSetOp::New {
            let descriptor = self.descriptor_pointer(&set_key_descriptor_symbol(glue))?;
            let function = external_unary_ptr(self.ctx, self.llvm, "hew_hashset_new_with_layout")?;
            let set = self.runtime_call_value(function, &[descriptor.into()], "set.new")?;
            self.store(result, set)?;
            return self.emit_result_edge(Some(result), normal);
        }
        let receiver = source(0)?;
        let set = self.load(receiver, "set.receiver")?;
        match operation {
            PhysicalSetOp::New => unreachable!("constructor already emitted"),
            PhysicalSetOp::Len => {
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_hashset_len_layout",
                    self.ctx.i64_type().fn_type(&[pointer.into()], false),
                )?;
                let len = self.runtime_call_value(function, &[set.into()], "set.length")?;
                self.store(result, len)?;
            }
            PhysicalSetOp::Contains
            | PhysicalSetOp::Insert { .. }
            | PhysicalSetOp::Remove { .. } => {
                // An adopted element transfers into the set; a borrowed one is
                // cloned. The take entry consumes the caller's element on the
                // duplicate path too, so nothing is left for the caller to
                // release either way.
                let moved = matches!(transfers.get(1), Some(ArgumentTransfer::Move(_)));
                let symbol = match operation {
                    PhysicalSetOp::Contains => "hew_hashset_contains_layout",
                    PhysicalSetOp::Insert { .. } if moved => "hew_hashset_insert_take_layout",
                    PhysicalSetOp::Insert { .. } => "hew_hashset_insert_clone_layout",
                    PhysicalSetOp::Remove { .. } => "hew_hashset_remove_layout",
                    _ => unreachable!("matched membership operation"),
                };
                let consumed = (operation != PhysicalSetOp::Contains)
                    .then_some((receiver, DestroyAction::Set(glue)));
                let present = self.emit_collection_callback(
                    symbol,
                    &[set.into(), self.slots[source(1)?.0 as usize].into()],
                    failure,
                    consumed,
                )?;
                if matches!(operation, PhysicalSetOp::Insert { .. }) && moved {
                    self.clear_owned(source(1)?)?;
                }
                if operation == PhysicalSetOp::Contains {
                    self.store(result, present.into())?;
                } else {
                    self.store_receiver_pair(result, receiver, present.into())?;
                }
            }
            PhysicalSetOp::Clear => {
                let function = external_drop(self.ctx, self.llvm, "hew_hashset_clear_layout")?;
                self.runtime_call_void(function, &[set.into()], "set.clear")?;
                self.clear_owned(receiver)?;
                self.store(result, set)?;
            }
            PhysicalSetOp::Elements => {
                let function =
                    external_unary_ptr(self.ctx, self.llvm, "hew_hashset_to_vec_layout")?;
                let vector = self.runtime_call_value(function, &[set.into()], "set.elements")?;
                self.store(result, vector)?;
            }
        }
        self.emit_result_edge(Some(result), normal)
    }

    /// Callback status precedes every read of the presence or value outputs.
    /// A failed borrowing kernel retains its receiver; a semantic Move still
    /// consumes that receiver, so release it before entering SIR cleanup.
    fn emit_collection_callback(
        &self,
        symbol: &str,
        inputs: &[BasicMetadataValueEnum<'ctx>],
        failure: Option<&PhysicalEdge>,
        consumed: Option<(StorageId, DestroyAction)>,
    ) -> CodegenResult<IntValue<'ctx>> {
        let failure = failure.ok_or_else(|| {
            CodegenError::FailClosed(format!("{symbol} lacks callback fault cleanup"))
        })?;
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let output = self
            .value_emitter()
            .entry_scratch(self.ctx.i8_type().into(), "collection.presence.out")?;
        let mut arguments = inputs.to_vec();
        arguments.push(output.into());
        arguments.push(self.active_fault.into());
        let parameters = vec![pointer.into(); arguments.len()];
        let function = get_or_declare_external(
            self.llvm,
            symbol,
            self.ctx.i32_type().fn_type(&parameters, false),
        )?;
        self.builder
            .build_store(self.active_fault, pointer.const_null())
            .llvm_ctx("clear callback fault output")?;
        let status = self
            .runtime_call_value(function, &arguments, "collection.status")?
            .into_int_value();
        self.builder
            .build_store(self.active_status, status)
            .llvm_ctx("retain callback status")?;
        let succeeded = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                self.ctx.i32_type().const_zero(),
                "collection.succeeded",
            )
            .llvm_ctx("test callback status before outputs")?;
        let success = self
            .ctx
            .append_basic_block(self.value, "collection.success");
        let failed = self
            .ctx
            .append_basic_block(self.value, "collection.callback.failed");
        self.builder
            .build_conditional_branch(succeeded, success, failed)
            .llvm_ctx("select callback outcome")?;
        self.builder.position_at_end(failed);
        if let Some((receiver, destroy)) = consumed {
            self.value_emitter().destroy_loaded_value(
                self.load(receiver, "collection.failed.receiver")?,
                &self.storage(receiver)?.layout,
                destroy,
            )?;
            self.clear_owned(receiver)?;
        }
        self.emit_edge(failure)?;
        self.builder.position_at_end(success);
        Ok(self
            .builder
            .build_load(self.ctx.i8_type(), output, "collection.presence")
            .llvm_ctx("read successful callback presence")?
            .into_int_value())
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
        self.emit_result_edge(Some(result), normal)
    }

    /// Shrink the receiver in place and build the `(bytes, Option<u8>)` pair.
    ///
    /// `hew_bytes_pop` answers `-1` on an empty buffer; every real byte is in
    /// `0..=255`, so the sentinel is the only `None`.
    fn emit_bytes_pop(
        &self,
        receiver: StorageId,
        result: StorageId,
        option: PhysicalVariantId,
        normal: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let function = get_or_declare_external(
            self.llvm,
            "hew_bytes_pop",
            self.ctx
                .i64_type()
                .fn_type(&[self.ctx.ptr_type(AddressSpace::default()).into()], false),
        )?;
        let byte = self
            .runtime_call_value(
                function,
                &[self.slots[receiver.0 as usize].into()],
                "bytes.pop.byte",
            )?
            .into_int_value();
        let destination = self.slots[result.0 as usize];
        let pair_ty = llvm_type(self.ctx, &self.storage(result)?.layout.repr)?.into_struct_type();
        let updated = self.load(receiver, "bytes.pop.receiver")?;
        let owner = self
            .builder
            .build_struct_gep(pair_ty, destination, 0, "bytes.pop.owner")
            .llvm_ctx("address the shrunk bytes receiver")?;
        self.builder
            .build_store(owner, updated)
            .llvm_ctx("write the shrunk bytes receiver")?;
        let optional = self
            .builder
            .build_struct_gep(pair_ty, destination, 1, "bytes.pop.optional")
            .llvm_ctx("address the popped byte")?;
        let found = self
            .builder
            .build_int_compare(
                IntPredicate::SGE,
                byte,
                byte.get_type().const_zero(),
                "bytes.pop.found",
            )
            .llvm_ctx("check bytes pop sentinel")?;
        let some = self.ctx.append_basic_block(self.value, "bytes.pop.some");
        let none = self.ctx.append_basic_block(self.value, "bytes.pop.none");
        self.builder
            .build_conditional_branch(found, some, none)
            .llvm_ctx("select bytes pop outcome")?;
        self.builder.position_at_end(none);
        self.write_variant_value(optional, 1, &[], option)?;
        self.emit_result_edge(Some(result), normal)?;
        self.builder.position_at_end(some);
        let popped = self
            .builder
            .build_int_truncate(byte, self.ctx.i8_type(), "bytes.pop.value")
            .llvm_ctx("narrow the popped byte")?;
        self.write_variant_value(optional, 0, &[popped.into()], option)?;
        self.emit_result_edge(Some(result), normal)
    }

    fn emit_bytes_get(
        &self,
        bytes: StorageId,
        index: StorageId,
        result: StorageId,
        option: PhysicalVariantId,
        normal: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let value = self.load(bytes, "bytes.get.value")?.into_struct_value();
        let pointer = self
            .builder
            .build_extract_value(value, 0, "bytes.get.pointer")
            .llvm_ctx("extract bytes get pointer")?
            .into_pointer_value();
        let offset = self
            .builder
            .build_extract_value(value, 1, "bytes.get.offset")
            .llvm_ctx("extract bytes get offset")?
            .into_int_value();
        let len = self
            .builder
            .build_extract_value(value, 2, "bytes.get.length")
            .llvm_ctx("extract bytes get length")?
            .into_int_value();
        let index = self.load(index, "bytes.get.index")?.into_int_value();
        let len64 = self
            .builder
            .build_int_z_extend(len, self.ctx.i64_type(), "bytes.get.length.i64")
            .llvm_ctx("widen bytes get length")?;
        let offset64 = self
            .builder
            .build_int_z_extend(offset, self.ctx.i64_type(), "bytes.get.offset.i64")
            .llvm_ctx("widen bytes get offset")?;
        let byte_offset = self
            .builder
            .build_int_add(offset64, index, "bytes.get.byte.offset")
            .llvm_ctx("calculate bytes get offset")?;
        let negative = self
            .builder
            .build_int_compare(
                IntPredicate::SLT,
                index,
                self.ctx.i64_type().const_zero(),
                "bytes.get.negative",
            )
            .llvm_ctx("guard negative bytes get index")?;
        let past_end = self
            .builder
            .build_int_compare(IntPredicate::SGE, index, len64, "bytes.get.past.end")
            .llvm_ctx("guard bytes get upper bound")?;
        let null = self
            .builder
            .build_is_null(pointer, "bytes.get.null")
            .llvm_ctx("guard null bytes get pointer")?;
        let overflow = self
            .builder
            .build_int_compare(
                IntPredicate::UGT,
                byte_offset,
                self.ctx.i64_type().const_int(u64::from(u32::MAX), false),
                "bytes.get.offset.overflow",
            )
            .llvm_ctx("guard bytes get offset overflow")?;
        let invalid = self
            .builder
            .build_or(negative, past_end, "bytes.get.bounds")
            .and_then(|v| self.builder.build_or(v, null, "bytes.get.invalid"))
            .and_then(|v| self.builder.build_or(v, overflow, "bytes.get.failure"))
            .llvm_ctx("combine bytes get guards")?;
        let some = self.ctx.append_basic_block(self.value, "bytes.get.some");
        let none = self.ctx.append_basic_block(self.value, "bytes.get.none");
        self.builder
            .build_conditional_branch(invalid, none, some)
            .llvm_ctx("select bytes get result")?;
        self.builder.position_at_end(none);
        self.write_variant_value(self.slots[result.0 as usize], 1, &[], option)?;
        self.emit_result_edge(Some(result), normal)?;
        self.builder.position_at_end(some);
        let read_at = unsafe {
            self.builder.build_gep(
                self.ctx.i8_type(),
                pointer,
                &[byte_offset],
                "bytes.get.address",
            )
        }
        .llvm_ctx("calculate bytes get address")?;
        let byte = self
            .builder
            .build_load(self.ctx.i8_type(), read_at, "bytes.get.byte")
            .llvm_ctx("load bytes get byte")?;
        self.write_variant_value(self.slots[result.0 as usize], 0, &[byte], option)?;
        self.emit_result_edge(Some(result), normal)
    }

    fn emit_bytes_set(
        &self,
        bytes: StorageId,
        index: StorageId,
        byte: StorageId,
        result: StorageId,
        normal: &PhysicalEdge,
        failure: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let value = self.load(bytes, "bytes.set.value")?.into_struct_value();
        let pointer = self
            .builder
            .build_extract_value(value, 0, "bytes.set.pointer")
            .llvm_ctx("extract bytes set pointer")?
            .into_pointer_value();
        let offset = self
            .builder
            .build_extract_value(value, 1, "bytes.set.offset")
            .llvm_ctx("extract bytes set offset")?
            .into_int_value();
        let len = self
            .builder
            .build_extract_value(value, 2, "bytes.set.length")
            .llvm_ctx("extract bytes set length")?
            .into_int_value();
        let index = self.load(index, "bytes.set.index")?.into_int_value();
        let len64 = self
            .builder
            .build_int_z_extend(len, self.ctx.i64_type(), "bytes.set.length.i64")
            .llvm_ctx("widen bytes set length")?;
        let offset64 = self
            .builder
            .build_int_z_extend(offset, self.ctx.i64_type(), "bytes.set.offset.i64")
            .llvm_ctx("widen bytes set offset")?;
        let byte_offset = self
            .builder
            .build_int_add(offset64, index, "bytes.set.byte.offset")
            .llvm_ctx("calculate bytes set offset")?;
        let negative = self
            .builder
            .build_int_compare(
                IntPredicate::SLT,
                index,
                self.ctx.i64_type().const_zero(),
                "bytes.set.negative",
            )
            .llvm_ctx("guard negative bytes set index")?;
        let past_end = self
            .builder
            .build_int_compare(IntPredicate::SGE, index, len64, "bytes.set.past.end")
            .llvm_ctx("guard bytes set upper bound")?;
        let null = self
            .builder
            .build_is_null(pointer, "bytes.set.null")
            .llvm_ctx("guard null bytes set pointer")?;
        let overflow = self
            .builder
            .build_int_compare(
                IntPredicate::UGT,
                byte_offset,
                self.ctx.i64_type().const_int(u64::from(u32::MAX), false),
                "bytes.set.offset.overflow",
            )
            .llvm_ctx("guard bytes set offset overflow")?;
        let invalid = self
            .builder
            .build_or(negative, past_end, "bytes.set.bounds")
            .and_then(|v| self.builder.build_or(v, null, "bytes.set.invalid"))
            .and_then(|v| self.builder.build_or(v, overflow, "bytes.set.failure"))
            .llvm_ctx("combine bytes set guards")?;
        let safe = self.ctx.append_basic_block(self.value, "bytes.set.safe");
        let failed = self.ctx.append_basic_block(self.value, "bytes.set.failure");
        self.builder
            .build_conditional_branch(invalid, failed, safe)
            .llvm_ctx("select bytes set outcome")?;
        self.builder.position_at_end(failed);
        self.emit_edge(failure)?;
        self.builder.position_at_end(safe);
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let function = get_or_declare_external(
            self.llvm,
            "hew_bytes_set",
            self.ctx.void_type().fn_type(
                &[
                    ptr.into(),
                    self.ctx.i64_type().into(),
                    self.ctx.i8_type().into(),
                ],
                false,
            ),
        )?;
        self.runtime_call_void(
            function,
            &[
                self.slots[bytes.0 as usize].into(),
                index.into(),
                self.load(byte, "bytes.set.byte")?.into(),
            ],
            "bytes.set",
        )?;
        let updated = self.load(bytes, "bytes.set.result")?;
        self.store(result, updated)?;
        self.emit_result_edge(Some(result), normal)
    }

    /// Call `hew_string_length` on `text` and return its `i64` result.
    fn string_length(
        &self,
        text: StorageId,
        name: &str,
    ) -> CodegenResult<inkwell::values::IntValue<'ctx>> {
        let function = get_or_declare_external(
            self.llvm,
            "hew_string_length",
            self.ctx
                .i64_type()
                .fn_type(&[self.ctx.ptr_type(AddressSpace::default()).into()], false),
        )?;
        let value = self.runtime_call_value(function, &[self.load(text, name)?.into()], name)?;
        Ok(value.into_int_value())
    }

    /// `s[i]` — codepoint index on `string`. MIR proves the bounds check
    /// here (mirrors [`Self::emit_bytes_index`]) so a violation reports
    /// through the canonical `Trap { IndexOutOfBounds }` edge rather than
    /// the runtime's own internal abort path. `hew_string_index` still
    /// carries its own defense-in-depth check, but the codegen guard below
    /// means it can never observe an out-of-range offset in practice.
    fn emit_string_index(
        &self,
        text: StorageId,
        index: StorageId,
        result: StorageId,
        normal: &PhysicalEdge,
        failure: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let len = self.string_length(text, "string.index.length")?;
        let index_value = self.load(index, "string.index.index")?.into_int_value();
        let negative = self
            .builder
            .build_int_compare(
                IntPredicate::SLT,
                index_value,
                self.ctx.i64_type().const_zero(),
                "string.index.negative",
            )
            .llvm_ctx("guard negative string index")?;
        let past_end = self
            .builder
            .build_int_compare(IntPredicate::SGE, index_value, len, "string.index.past.end")
            .llvm_ctx("guard string index upper bound")?;
        let out_of_bounds = self
            .builder
            .build_or(negative, past_end, "string.index.bounds")
            .llvm_ctx("combine string index guards")?;
        let safe = self.ctx.append_basic_block(self.value, "string.index.safe");
        let failed = self
            .ctx
            .append_basic_block(self.value, "string.index.failure");
        self.builder
            .build_conditional_branch(out_of_bounds, failed, safe)
            .llvm_ctx("branch around fallible string index call")?;

        self.builder.position_at_end(failed);
        self.emit_edge(failure)?;

        self.builder.position_at_end(safe);
        let function = get_or_declare_external(
            self.llvm,
            "hew_string_index",
            self.ctx.i32_type().fn_type(
                &[
                    self.ctx.ptr_type(AddressSpace::default()).into(),
                    self.ctx.i64_type().into(),
                ],
                false,
            ),
        )?;
        let value = self.runtime_call_value(
            function,
            &[
                self.load(text, "string.index.text")?.into(),
                index_value.into(),
            ],
            "string.index",
        )?;
        self.store(result, value)?;
        self.emit_result_edge(Some(result), normal)
    }

    /// `s[a..b]` — codepoint range-slice on `string` (`0 <= start <= end <=
    /// len`, matching [`Self::emit_string_index`]'s MIR-level bounds proof).
    /// `b[a..b]` and its open-ended forms. The bounds guard lives here, like
    /// [`Self::emit_bytes_index`], so a violation reports through the canonical
    /// `Trap { IndexOutOfBounds }` edge instead of the runtime's abort path.
    /// An absent end bound is the receiver's own length.
    fn emit_bytes_slice(
        &self,
        bytes: StorageId,
        start: StorageId,
        end: Option<StorageId>,
        result: StorageId,
        normal: &PhysicalEdge,
        failure: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let i64_ty = self.ctx.i64_type();
        let value = self.load(bytes, "bytes.slice.value")?.into_struct_value();
        let len = self
            .builder
            .build_extract_value(value, 2, "bytes.slice.length")
            .llvm_ctx("extract bytes slice length")?
            .into_int_value();
        let len64 = self
            .builder
            .build_int_z_extend(len, i64_ty, "bytes.slice.length.i64")
            .llvm_ctx("widen bytes slice length")?;
        let start_value = self.load(start, "bytes.slice.start")?.into_int_value();
        let end_value = match end {
            Some(end) => self.load(end, "bytes.slice.end")?.into_int_value(),
            None => len64,
        };
        let start_negative = self
            .builder
            .build_int_compare(
                IntPredicate::SLT,
                start_value,
                i64_ty.const_zero(),
                "bytes.slice.start.negative",
            )
            .llvm_ctx("guard negative bytes slice start")?;
        let end_negative = self
            .builder
            .build_int_compare(
                IntPredicate::SLT,
                end_value,
                i64_ty.const_zero(),
                "bytes.slice.end.negative",
            )
            .llvm_ctx("guard negative bytes slice end")?;
        let inverted = self
            .builder
            .build_int_compare(
                IntPredicate::SGT,
                start_value,
                end_value,
                "bytes.slice.inverted",
            )
            .llvm_ctx("guard inverted bytes slice range")?;
        let past_end = self
            .builder
            .build_int_compare(IntPredicate::SGT, end_value, len64, "bytes.slice.past.end")
            .llvm_ctx("guard bytes slice upper bound")?;
        let out_of_bounds = self
            .builder
            .build_or(start_negative, end_negative, "bytes.slice.bounds.a")
            .and_then(|a| self.builder.build_or(a, inverted, "bytes.slice.bounds.b"))
            .and_then(|b| {
                self.builder
                    .build_or(b, past_end, "bytes.slice.bounds.condition")
            })
            .llvm_ctx("combine bytes slice guards")?;
        let safe = self.ctx.append_basic_block(self.value, "bytes.slice.safe");
        let failed = self
            .ctx
            .append_basic_block(self.value, "bytes.slice.failure");
        self.builder
            .build_conditional_branch(out_of_bounds, failed, safe)
            .llvm_ctx("branch around fallible bytes slice call")?;

        self.builder.position_at_end(failed);
        self.emit_edge(failure)?;

        self.builder.position_at_end(safe);
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let function = get_or_declare_external(
            self.llvm,
            "hew_bytes_slice_owned",
            self.ctx.void_type().fn_type(
                &[ptr.into(), i64_ty.into(), i64_ty.into(), ptr.into()],
                false,
            ),
        )?;
        self.runtime_call_void(
            function,
            &[
                self.slots[bytes.0 as usize].into(),
                start_value.into(),
                end_value.into(),
                self.slots[result.0 as usize].into(),
            ],
            "bytes.slice",
        )?;
        self.emit_result_edge(Some(result), normal)
    }

    fn emit_string_slice_codepoints(
        &self,
        text: StorageId,
        start: StorageId,
        end: StorageId,
        result: StorageId,
        normal: &PhysicalEdge,
        failure: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let len = self.string_length(text, "string.slice.length")?;
        let start_value = self.load(start, "string.slice.start")?.into_int_value();
        let end_value = self.load(end, "string.slice.end")?.into_int_value();
        let start_negative = self
            .builder
            .build_int_compare(
                IntPredicate::SLT,
                start_value,
                self.ctx.i64_type().const_zero(),
                "string.slice.start.negative",
            )
            .llvm_ctx("guard negative string slice start")?;
        let end_negative = self
            .builder
            .build_int_compare(
                IntPredicate::SLT,
                end_value,
                self.ctx.i64_type().const_zero(),
                "string.slice.end.negative",
            )
            .llvm_ctx("guard negative string slice end")?;
        let inverted = self
            .builder
            .build_int_compare(
                IntPredicate::SGT,
                start_value,
                end_value,
                "string.slice.inverted",
            )
            .llvm_ctx("guard inverted string slice range")?;
        let past_end = self
            .builder
            .build_int_compare(IntPredicate::SGT, end_value, len, "string.slice.past.end")
            .llvm_ctx("guard string slice upper bound")?;
        let out_of_bounds = self
            .builder
            .build_or(start_negative, end_negative, "string.slice.bounds.a")
            .and_then(|a| self.builder.build_or(a, inverted, "string.slice.bounds.b"))
            .and_then(|b| {
                self.builder
                    .build_or(b, past_end, "string.slice.bounds.condition")
            })
            .llvm_ctx("combine string slice guards")?;
        let safe = self.ctx.append_basic_block(self.value, "string.slice.safe");
        let failed = self
            .ctx
            .append_basic_block(self.value, "string.slice.failure");
        self.builder
            .build_conditional_branch(out_of_bounds, failed, safe)
            .llvm_ctx("branch around fallible string slice call")?;

        self.builder.position_at_end(failed);
        self.emit_edge(failure)?;

        self.builder.position_at_end(safe);
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let function = get_or_declare_external(
            self.llvm,
            "hew_string_slice_codepoints",
            ptr.fn_type(
                &[
                    ptr.into(),
                    self.ctx.i64_type().into(),
                    self.ctx.i64_type().into(),
                ],
                false,
            ),
        )?;
        let value = self.runtime_call_value(
            function,
            &[
                self.load(text, "string.slice.text")?.into(),
                start_value.into(),
                end_value.into(),
            ],
            "string.slice.codepoints",
        )?;
        self.store(result, value)?;
        self.emit_result_edge(Some(result), normal)
    }

    /// `s[a..]` — open-ended codepoint range-slice on `string` (`0 <= start
    /// <= len`, matching [`Self::emit_string_slice_codepoints`]).
    fn emit_string_slice_codepoints_from(
        &self,
        text: StorageId,
        start: StorageId,
        result: StorageId,
        normal: &PhysicalEdge,
        failure: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let len = self.string_length(text, "string.slice.from.length")?;
        let start_value = self
            .load(start, "string.slice.from.start")?
            .into_int_value();
        let negative = self
            .builder
            .build_int_compare(
                IntPredicate::SLT,
                start_value,
                self.ctx.i64_type().const_zero(),
                "string.slice.from.negative",
            )
            .llvm_ctx("guard negative string slice-from start")?;
        let past_end = self
            .builder
            .build_int_compare(
                IntPredicate::SGT,
                start_value,
                len,
                "string.slice.from.past.end",
            )
            .llvm_ctx("guard string slice-from upper bound")?;
        let out_of_bounds = self
            .builder
            .build_or(negative, past_end, "string.slice.from.bounds")
            .llvm_ctx("combine string slice-from guards")?;
        let safe = self
            .ctx
            .append_basic_block(self.value, "string.slice.from.safe");
        let failed = self
            .ctx
            .append_basic_block(self.value, "string.slice.from.failure");
        self.builder
            .build_conditional_branch(out_of_bounds, failed, safe)
            .llvm_ctx("branch around fallible string slice-from call")?;

        self.builder.position_at_end(failed);
        self.emit_edge(failure)?;

        self.builder.position_at_end(safe);
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let function = get_or_declare_external(
            self.llvm,
            "hew_string_slice_codepoints_from",
            ptr.fn_type(&[ptr.into(), self.ctx.i64_type().into()], false),
        )?;
        let value = self.runtime_call_value(
            function,
            &[
                self.load(text, "string.slice.from.text")?.into(),
                start_value.into(),
            ],
            "string.slice.codepoints.from",
        )?;
        self.store(result, value)?;
        self.emit_result_edge(Some(result), normal)
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

    fn initialize_active_fault(&self, code: i32) -> CodegenResult<()> {
        self.initialize_active_fault_value(self.ctx.i32_type().const_int(code as u64, true))
    }

    fn initialize_cancellation_fault(&self) -> CodegenResult<()> {
        let frame = self.frame.as_ref().ok_or_else(|| {
            CodegenError::FailClosed("cancellation requires a resumable invocation".into())
        })?;
        let code = self.state_value("hew_coro_state_cancel_code", frame.state)?;
        self.initialize_active_fault_value(code)
    }

    fn initialize_active_fault_value(&self, code: IntValue<'ctx>) -> CodegenResult<()> {
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

    fn store_active_fault(&self, fault: BasicValueEnum<'ctx>, code: i32) -> CodegenResult<()> {
        self.store_active_fault_value(fault, self.ctx.i32_type().const_int(code as u64, true))
    }

    fn store_active_fault_value(
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
        self.emit_finish(status)
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

const fn argument_source(transfer: &ArgumentTransfer) -> StorageId {
    match transfer {
        ArgumentTransfer::Borrow(source)
        | ArgumentTransfer::BorrowMut(source)
        | ArgumentTransfer::Move(source) => *source,
        ArgumentTransfer::Clone { source, .. } => *source,
    }
}

fn trap_code(kind: TrapKind) -> i32 {
    match kind {
        TrapKind::IntegerOverflow => HEW_TRAP_INTEGER_OVERFLOW,
        TrapKind::DivideByZero => HEW_TRAP_DIVIDE_BY_ZERO,
        TrapKind::SignedMinDivNegOne => HEW_TRAP_SIGNED_MIN_DIV_NEG_ONE,
        TrapKind::ShiftOutOfRange => HEW_TRAP_SHIFT_OUT_OF_RANGE,
        TrapKind::IndexOutOfBounds => HEW_TRAP_INDEX_OUT_OF_BOUNDS,
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
#[path = "physical_resource_tests.rs"]
mod resource_tests;

#[cfg(test)]
mod tests {
    mod borrow_fixture {
        include!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../hew-sir/tests/support/borrowed_aggregate.rs"
        ));
    }

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
    fn borrowed_field_chain_emits_no_vector_clone_for_its_reads() {
        use inkwell::values::AnyValue;

        fn vector_clone_calls(semantic: &SemModule) -> usize {
            let triple = native_emission_triple();
            let inventory = hew_mir::physical::physical_type_inventory(semantic);
            let target = physical_target_for_inventory(&triple, &inventory).unwrap();
            let physical = hew_mir::lower_physical_module(semantic, target).unwrap();
            let ctx = Context::create();
            let machine =
                crate::llvm::target_machine_for_triple_with_opt_level(&triple, OptLevel::O0)
                    .unwrap();
            let llvm = build_module(&ctx, physical.module(), "field_loans", &machine).unwrap();
            llvm.verify().unwrap();
            let symbol = emitted_symbol(physical.module(), &physical.module().callables[0]);
            llvm.get_function(&symbol)
                .unwrap()
                .print_to_string()
                .to_string()
                .matches("call ptr @hew_vec_clone_owned(")
                .count()
        }

        let borrowed = borrow_fixture::nested_borrow_module();
        let mut copied = borrowed.clone();
        for operation in copied
            .functions
            .iter_mut()
            .flat_map(|f| &mut f.blocks)
            .flat_map(|b| &mut b.ops)
        {
            match &operation.kind {
                SemOpKind::AggregateProjectBorrow {
                    shape,
                    aggregate,
                    field,
                } => {
                    operation.kind = SemOpKind::AggregateProjectCopy {
                        shape: *shape,
                        aggregate: aggregate.clone(),
                        field: *field,
                    };
                    operation.results[0].own = OwnKind::Owned;
                }
                SemOpKind::EndBorrow { borrow } => {
                    operation.kind = SemOpKind::DestroyValue {
                        value: borrow.clone(),
                    };
                }
                _ => {}
            }
        }
        assert_eq!(
            vector_clone_calls(&copied) - vector_clone_calls(&borrowed),
            2,
            "both the nested record and vector projection must avoid cloning their vector"
        );
    }

    #[test]
    fn vector_descriptor_matches_the_runtime_c_abi() {
        use hew_runtime::vec::HewValueLayout;
        use std::mem::{align_of, offset_of, size_of};

        let triple = native_emission_triple();
        let physical = physical_target_for_triple(&triple).unwrap();
        let target = TargetData::create(&physical.data_layout);
        let ctx = Context::create();
        let descriptor = value_descriptor_type(&ctx, &target);
        assert_eq!(
            target.get_abi_size(&descriptor),
            size_of::<HewValueLayout>() as u64
        );
        assert_eq!(
            target.get_abi_alignment(&descriptor) as usize,
            align_of::<HewValueLayout>()
        );
        for (index, expected) in [
            offset_of!(HewValueLayout, size),
            offset_of!(HewValueLayout, align),
            offset_of!(HewValueLayout, ownership_kind),
            offset_of!(HewValueLayout, clone_fn),
            offset_of!(HewValueLayout, drop_fn),
            offset_of!(HewValueLayout, visit_close),
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

    /// Exercise the physical runtime boundary with a collection supplied by a
    /// caller. Construction's key-capability demand is tested at its producer.
    fn collection_operation_module(family: hew_types::RuntimeCallFamily) -> SemModule {
        use hew_types::{BuiltinType, RuntimeArgumentEffect, TypeFactContext, TypeFactService};
        let kind = match family {
            hew_types::RuntimeCallFamily::Map(_) => BuiltinType::HashMap,
            hew_types::RuntimeCallFamily::Set(_) => BuiltinType::HashSet,
            _ => panic!("fixture requires a map or set operation"),
        };
        let mut arguments = vec![ResolvedTy::String];
        if kind == BuiltinType::HashMap {
            arguments.push(ResolvedTy::named_builtin(
                "Vec",
                BuiltinType::Vec,
                vec![ResolvedTy::String],
            ));
        }
        let receiver = ResolvedTy::named_builtin(kind.canonical_name(), kind, arguments);
        let contract = family.semantic_contract().unwrap();
        let params = contract
            .arguments
            .iter()
            .map(|argument| argument.ty.resolve(Some(&receiver)).unwrap())
            .collect::<Vec<_>>();
        let result_ty = contract
            .instantiate(&params, &ResolvedTy::Unit)
            .unwrap()
            .result_ty;
        let mut module = scalar_entry_module();
        module.entry_callable = None;
        module.entry_exit_plan = None;
        if matches!(
            family,
            hew_types::RuntimeCallFamily::Map(
                hew_types::runtime_call::MapValueOp::Get
                    | hew_types::runtime_call::MapValueOp::GetBorrow
                    | hew_types::runtime_call::MapValueOp::Remove
            )
        ) {
            let seed = lower_source(
                r#"fn main() -> i64 {
                let nested = [["value"]];
                let optional = nested.get(0);
                return 0;
            }"#,
            );
            module.variant_shapes = seed.variant_shapes;
        }
        let mut facts = TypeFactService::new(TypeFactContext::default(), module.type_facts);
        facts.require(&result_ty).unwrap();
        let mut copies = Vec::new();
        let mut operands = Vec::new();
        let count = u32::try_from(params.len()).unwrap();
        // A borrowed result is a loan: the fixture ends it rather than
        // handing an owner back to a caller.
        let borrowed_result =
            matches!(contract.result, hew_types::RuntimeResultEffect::Borrowed(_));
        let return_ty = if borrowed_result {
            ResolvedTy::Unit
        } else {
            result_ty.clone()
        };
        let signature = &mut module.callables[0].signature;
        signature.params.clear();
        signature.return_ty = return_ty.clone();
        let function = &mut module.functions[0];
        function.params.clear();
        function.return_ty = return_ty;
        for (index, (ty, contract)) in params.iter().zip(contract.arguments).enumerate() {
            let index = u32::try_from(index).unwrap();
            let own = OwnKind::of_class(facts.require(ty).unwrap().class);
            let borrowed = own == OwnKind::Owned;
            signature.params.push(hew_sir::SemAbiParam {
                ty: ty.clone(),
                passing: if borrowed {
                    hew_sir::SemParamPassing::Borrow
                } else {
                    hew_sir::SemParamPassing::ReadOnly
                },
                caller_visible_projection: false,
            });
            function.params.push(BlockArg {
                value: ValueId(index),
                ty: ty.clone(),
                own: if borrowed { OwnKind::Guaranteed } else { own },
            });
            let (value, decision) = match contract
                .effect
                .resolve_operand(facts.require(ty).unwrap().class)
            {
                RuntimeArgumentEffect::Value => unreachable!("value ingress was resolved"),
                RuntimeArgumentEffect::Borrow => (ValueId(index), BoundaryDecision::Borrow),
                RuntimeArgumentEffect::Copy => (ValueId(index), BoundaryDecision::Copy),
                RuntimeArgumentEffect::Move => {
                    let value = ValueId(count + index);
                    copies.push(SemOp {
                        id: hew_sir::OpId(index),
                        results: vec![ValueDef {
                            id: value,
                            ty: ty.clone(),
                            own,
                        }],
                        kind: SemOpKind::CopyValue {
                            source: Operand {
                                value: ValueId(index),
                            },
                        },
                        provenance: Provenance::Synthesized,
                    });
                    (value, BoundaryDecision::Move)
                }
            };
            operands.push(BoundaryOperand {
                operand: Operand { value },
                decision,
            });
        }
        // A borrowed result is a loan of argument zero: it carries no release
        // obligation regardless of its type's class.
        let own = if matches!(contract.result, hew_types::RuntimeResultEffect::Borrowed(_)) {
            OwnKind::Guaranteed
        } else {
            OwnKind::of_class(facts.require(&result_ty).unwrap().class)
        };
        let raw = ValueId(2 * count);
        let value = ValueId(2 * count + 1);
        let failed_inputs = operands
            .iter()
            .filter(|argument| {
                contract.preserves_inputs_on_failure()
                    && argument.decision == BoundaryDecision::Move
            })
            .enumerate()
            .map(|(index, argument)| SemOp {
                id: hew_sir::OpId(count + 1 + u32::try_from(index).unwrap()),
                results: vec![],
                kind: SemOpKind::DestroyValue {
                    value: argument.operand.clone(),
                },
                provenance: Provenance::Synthesized,
            })
            .collect();
        let unwind = if contract.failures.is_empty() {
            hew_sir::CallUnwind::NotApplicable
        } else {
            hew_sir::CallUnwind::Cleanup(Edge {
                target: BlockId(2),
                args: vec![],
            })
        };
        function.blocks = vec![
            SemBlock {
                id: BlockId(0),
                args: vec![],
                ops: copies,
                terminator: SemTerminator::RtCall {
                    id: hew_sir::OpId(count),
                    family,
                    args: operands,
                    result: hew_sir::CallResult::Value(ValueDef {
                        id: raw,
                        ty: result_ty.clone(),
                        own,
                    }),
                    normal: Edge {
                        target: BlockId(1),
                        args: vec![Operand { value: raw }],
                    },
                    unwind,
                },
            },
            SemBlock {
                id: BlockId(1),
                args: vec![BlockArg {
                    value,
                    ty: result_ty,
                    own,
                }],
                ops: if borrowed_result {
                    vec![SemOp {
                        id: hew_sir::OpId(2 * count + 2),
                        results: vec![],
                        kind: SemOpKind::EndBorrow {
                            borrow: Operand { value },
                        },
                        provenance: Provenance::Synthesized,
                    }]
                } else {
                    vec![]
                },
                terminator: if borrowed_result {
                    SemTerminator::Return { value: None }
                } else {
                    SemTerminator::Return {
                        value: Some(BoundaryOperand {
                            operand: Operand { value },
                            decision: BoundaryDecision::Move,
                        }),
                    }
                },
            },
        ];
        if let Some(failure) = contract.failures.first() {
            function.blocks.push(SemBlock {
                id: BlockId(2),
                args: vec![],
                ops: failed_inputs,
                terminator: if contract.propagates_fault() {
                    SemTerminator::ResumeUnwind
                } else {
                    SemTerminator::Trap {
                        kind: hew_sir::runtime_failure_trap_kind(*failure).unwrap(),
                    }
                },
            });
        }
        module.type_facts = facts.into_rows();
        module
    }

    #[test]
    fn collection_operations_emit_verified_runtime_abi_and_owner_transfers() {
        use hew_types::runtime_call::{MapValueOp as Map, SetValueOp as Set};
        use hew_types::RuntimeCallFamily;
        let families = [
            Map::Len,
            Map::Index,
            Map::Get,
            Map::GetBorrow,
            Map::Remove,
            Map::ContainsKey,
            Map::Insert,
            Map::Clear,
            Map::Keys,
            Map::Values,
            Map::Entries,
        ]
        .into_iter()
        .map(RuntimeCallFamily::Map)
        .chain(
            [
                Set::Len,
                Set::Contains,
                Set::Insert,
                Set::Remove,
                Set::Clear,
                Set::Elements,
            ]
            .into_iter()
            .map(RuntimeCallFamily::Set),
        );
        for family in families {
            let semantic = collection_operation_module(family);
            for triple in [
                native_emission_triple(),
                "x86_64-pc-windows-msvc".to_string(),
                "aarch64-apple-darwin".to_string(),
            ] {
                let target = physical_target_for_inventory(
                    &triple,
                    &hew_mir::physical::physical_type_inventory(&semantic),
                )
                .unwrap();
                let physical = hew_mir::lower_physical_module(&semantic, target)
                    .unwrap_or_else(|error| panic!("{family:?}: {error}"));
                let context = Context::create();
                let machine =
                    crate::llvm::target_machine_for_triple_with_opt_level(&triple, OptLevel::O0)
                        .unwrap();
                let module =
                    build_module(&context, physical.module(), "collection_boundary", &machine)
                        .unwrap_or_else(|error| panic!("{family:?}: {error}"));
                module.verify().unwrap();
            }
        }
    }

    #[test]
    #[allow(
        clippy::too_many_lines,
        reason = "the callback protocol oracle follows both LLVM successors and the exact fault outputs"
    )]
    fn callback_status_guards_outputs_and_preserves_fault_identity() {
        use hew_types::{
            runtime_call::{MapValueOp as Map, SetValueOp as Set},
            RuntimeCallFamily,
        };
        use inkwell::values::{BasicValue, CallSiteValue, InstructionOpcode};
        let cases = [
            (
                RuntimeCallFamily::Map(Map::Get),
                "hew_hashmap_get_clone_layout",
                None,
            ),
            (
                RuntimeCallFamily::Map(Map::GetBorrow),
                "hew_hashmap_get_borrow_layout",
                None,
            ),
            (
                RuntimeCallFamily::Map(Map::Index),
                "hew_hashmap_get_clone_layout",
                None,
            ),
            (
                // The map adopts an owned value, so insertion takes it and
                // clones only the key.
                RuntimeCallFamily::Map(Map::Insert),
                "hew_hashmap_insert_take_layout",
                // The fixture's keys and values are ordinary data, so its
                // release joins the walker rather than nesting.
                Some("hew_hashmap_free_layout_walk"),
            ),
            (
                RuntimeCallFamily::Set(Set::Insert),
                "hew_hashset_insert_clone_layout",
                Some("hew_hashset_free_layout_walk"),
            ),
        ];
        for (family, symbol, release) in cases {
            let semantic = collection_operation_module(family);
            let triple = native_emission_triple();
            let target = physical_target_for_inventory(
                &triple,
                &hew_mir::physical::physical_type_inventory(&semantic),
            )
            .unwrap();
            let physical = hew_mir::lower_physical_module(&semantic, target).unwrap();
            let context = Context::create();
            let machine =
                crate::llvm::target_machine_for_triple_with_opt_level(&triple, OptLevel::O0)
                    .unwrap();
            let module =
                build_module(&context, physical.module(), "callback_protocol", &machine).unwrap();
            module.verify().unwrap();
            let kernel = module.get_function(symbol).unwrap();
            assert_eq!(
                kernel.get_type().get_return_type(),
                Some(context.i32_type().into())
            );
            let call = module
                .get_functions()
                .flat_map(|function| function.get_basic_blocks())
                .flat_map(inkwell::basic_block::BasicBlock::get_instructions)
                .find(|instruction| {
                    CallSiteValue::try_from(*instruction)
                        .ok()
                        .is_some_and(|call| call.get_called_fn_value() == Some(kernel))
                })
                .unwrap();
            let status = CallSiteValue::try_from(call)
                .unwrap()
                .try_as_basic_value()
                .basic()
                .unwrap();
            let argument_count = kernel.count_params();
            let presence = call
                .get_operand(argument_count - 2)
                .unwrap()
                .value()
                .unwrap();
            let fault = call
                .get_operand(argument_count - 1)
                .unwrap()
                .value()
                .unwrap();
            let outputs = if matches!(family, RuntimeCallFamily::Map(Map::Get | Map::Index)) {
                vec![presence, call.get_operand(2).unwrap().value().unwrap()]
            } else {
                vec![presence]
            };
            let block = call.get_parent().unwrap();
            assert!(
                !block
                    .get_instructions()
                    .any(
                        |instruction| instruction.get_opcode() == InstructionOpcode::Load
                            && outputs
                                .contains(&instruction.get_operand(0).unwrap().value().unwrap())
                    ),
                "{family:?}: output read before status branch"
            );
            let status_slot = block
                .get_instructions()
                .find(|instruction| {
                    instruction.get_opcode() == InstructionOpcode::Store
                        && instruction.get_operand(0).unwrap().value() == Some(status)
                })
                .unwrap()
                .get_operand(1)
                .unwrap()
                .value()
                .unwrap();
            let branch = block.get_terminator().unwrap();
            let condition = branch
                .get_operand(0)
                .unwrap()
                .value()
                .unwrap()
                .as_instruction_value()
                .unwrap();
            assert_eq!(condition.get_operand(0).unwrap().value(), Some(status));
            assert_eq!(
                condition
                    .get_operand(1)
                    .unwrap()
                    .value()
                    .unwrap()
                    .into_int_value()
                    .get_zero_extended_constant(),
                Some(0)
            );
            let (success, failed) = match condition.get_icmp_predicate().unwrap() {
                IntPredicate::EQ => (2, 1),
                IntPredicate::NE => (1, 2),
                predicate => panic!("unexpected callback status predicate {predicate:?}"),
            };
            let success = branch.get_operand(success).unwrap().block().unwrap();
            let failed = branch.get_operand(failed).unwrap().block().unwrap();
            assert!(success
                .get_instructions()
                .any(
                    |instruction| instruction.get_opcode() == InstructionOpcode::Load
                        && instruction.get_operand(0).unwrap().value() == Some(presence)
                ));
            if let Some(symbol) = release {
                assert!(
                    failed
                        .get_instructions()
                        .any(|instruction| CallSiteValue::try_from(instruction)
                            .ok()
                            .and_then(CallSiteValue::get_called_fn_value)
                            .is_some_and(
                                |callee| callee.get_name().to_bytes() == symbol.as_bytes()
                            )),
                    "{family:?}: consumed receiver must be released before cleanup"
                );
            }
            let mut pending = vec![failed];
            let mut visited = Vec::new();
            let mut returns = 0;
            while let Some(block) = pending.pop() {
                if visited.contains(&block) {
                    continue;
                }
                visited.push(block);
                for instruction in block.get_instructions() {
                    if instruction.get_opcode() == InstructionOpcode::Load {
                        assert!(
                            !outputs
                                .contains(&instruction.get_operand(0).unwrap().value().unwrap()),
                            "{family:?}: failed callback output read"
                        );
                    }
                    if let Ok(call) = CallSiteValue::try_from(instruction) {
                        assert!(
                            call.get_called_fn_value()
                                .is_none_or(
                                    |callee| callee.get_name().to_bytes() != b"hew_fault_new"
                                ),
                            "{family:?}: callback fault replaced"
                        );
                    }
                }
                let terminal = block.get_terminator().unwrap();
                if terminal.get_opcode() == InstructionOpcode::Return {
                    returns += 1;
                    let result = terminal
                        .get_operand(0)
                        .unwrap()
                        .value()
                        .unwrap()
                        .as_instruction_value()
                        .unwrap();
                    assert_eq!(
                        result.get_operand(0).unwrap().value(),
                        Some(status_slot),
                        "callback status must be returned unchanged"
                    );
                    let destination = block.get_parent().unwrap().get_last_param().unwrap();
                    let transfer = block
                        .get_instructions()
                        .find(|instruction| {
                            instruction.get_opcode() == InstructionOpcode::Store
                                && instruction.get_operand(1).unwrap().value() == Some(destination)
                        })
                        .unwrap();
                    let owner = transfer
                        .get_operand(0)
                        .unwrap()
                        .value()
                        .unwrap()
                        .as_instruction_value()
                        .unwrap();
                    assert_eq!(
                        owner.get_operand(0).unwrap().value(),
                        Some(fault),
                        "callback fault owner must be forwarded unchanged"
                    );
                } else {
                    pending.extend(
                        terminal
                            .get_operands()
                            .flatten()
                            .filter_map(inkwell::values::Operand::block),
                    );
                }
            }
            assert!(returns > 0, "callback cleanup must reach fault propagation");
        }
    }

    #[test]
    fn fixed_arrays_check_target_allocation_geometry_without_stack_limits() {
        for (triple, pointer_bytes) in [
            ("x86_64-unknown-linux-gnu", 8),
            ("x86_64-pc-windows-msvc", 8),
            ("aarch64-apple-darwin", 8),
            ("wasm32-wasip1", 4),
        ] {
            let large = ResolvedTy::Array(Box::new(ResolvedTy::I64), 4_000_000);
            let target = physical_target_for_types(triple, [&large]).unwrap();
            let layout = target.layout(&large).unwrap();
            assert_eq!(layout.size, pointer_bytes, "{triple}");
            assert_eq!(layout.repr, PhysicalRepr::Pointer);
            let limit = if pointer_bytes == 8 {
                i64::MAX as u64
            } else {
                i32::MAX as u64
            };
            let length_limit = if pointer_bytes == 8 {
                i64::MAX as u64
            } else {
                u64::from(u32::MAX)
            };
            for invalid in [
                ResolvedTy::Array(Box::new(ResolvedTy::I64), limit / 8 + 1),
                ResolvedTy::Array(Box::new(ResolvedTy::Unit), length_limit + 1),
            ] {
                let error = physical_target_for_types(triple, [&invalid]).unwrap_err();
                assert!(
                    error
                        .to_string()
                        .contains("target allocation or runtime length range"),
                    "{triple}: {error}"
                );
            }
        }
    }

    #[test]
    fn fixed_array_repeat_emits_constant_size_glue_and_no_array_stack_temporary() {
        let triple = native_emission_triple();
        let machine =
            crate::llvm::target_machine_for_triple_with_opt_level(&triple, OptLevel::O0).unwrap();
        let mut instruction_counts = Vec::new();
        for length in [3, 4_000_000] {
            let semantic = lower_source(&format!(
                "fn main() -> i64 {{ let values: [i64; {length}] = [7; {length}]; values[0] }}"
            ));
            let inventory = hew_mir::physical::physical_type_inventory(&semantic);
            let target = physical_target_for_inventory(&triple, &inventory).unwrap();
            let verified = hew_mir::lower_physical_module(&semantic, target).unwrap();
            let physical = verified.module();
            // Select the fixed-array element rather than counting every row:
            // the pin is about this source's own vector, not the module's
            // total glue count.
            assert_eq!(
                physical
                    .vector_glue
                    .iter()
                    .filter(|glue| glue.element.ty == ResolvedTy::I64)
                    .count(),
                1
            );
            for function in &physical.functions {
                for storage in &function.storage {
                    if matches!(storage.ty, ResolvedTy::Array(_, _)) {
                        assert_eq!(storage.layout.repr, PhysicalRepr::Pointer);
                    }
                }
            }
            let ctx = Context::create();
            let llvm = build_module(&ctx, physical, "fixed_array_repeat", &machine).unwrap();
            llvm.verify().unwrap();
            instruction_counts.push(
                llvm.get_functions()
                    .flat_map(|function| function.get_basic_blocks())
                    .map(|block| block.get_instructions().count())
                    .sum::<usize>(),
            );
        }
        assert_eq!(instruction_counts[0], instruction_counts[1]);
    }

    #[test]
    fn canonical_maps_and_sets_use_the_target_pointer_carrier() {
        let map = ResolvedTy::named_builtin(
            "HashMap",
            hew_types::BuiltinType::HashMap,
            vec![ResolvedTy::String, ResolvedTy::I64],
        );
        let set = ResolvedTy::named_builtin(
            "HashSet",
            hew_types::BuiltinType::HashSet,
            vec![ResolvedTy::String],
        );
        for triple in [
            "x86_64-unknown-linux-gnu",
            "x86_64-pc-windows-msvc",
            "aarch64-apple-darwin",
        ] {
            let target = physical_target_for_types(triple, [&map, &set]).unwrap();
            let pointer = target.layout(&ResolvedTy::String).unwrap();
            assert_eq!(target.layout(&map), Some(pointer), "{triple}");
            assert_eq!(target.layout(&set), Some(pointer), "{triple}");
        }
        let lookalike = ResolvedTy::Named {
            name: "HashMap".into(),
            args: vec![ResolvedTy::String, ResolvedTy::I64],
            builtin: None,
            is_opaque: false,
        };
        assert!(physical_target_for_types("x86_64-unknown-linux-gnu", [&lookalike]).is_err());
        let wrong_arity =
            ResolvedTy::named_builtin("HashSet", hew_types::BuiltinType::HashSet, vec![]);
        assert!(physical_target_for_types("x86_64-unknown-linux-gnu", [&wrong_arity]).is_err());
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
                    kind: SemOpKind::ConstInteger(7),
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
            regex_patterns: Vec::new(),
            actors: Vec::new(),
            supervisors: Vec::new(),
            resources: BTreeMap::new(),
            closures: Vec::new(),
            vtables: Vec::new(),
            value_capabilities: BTreeMap::new(),
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
                        kind: SemOpKind::ConstInteger(i128::from(i64::MAX)),
                        provenance: Provenance::Synthesized,
                    },
                    SemOp {
                        id: hew_sir::OpId(1),
                        results: vec![ValueDef {
                            id: ValueId(1),
                            ty: ResolvedTy::I64,
                            own: OwnKind::None,
                        }],
                        kind: SemOpKind::ConstInteger(1),
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
            regex_patterns: Vec::new(),
            actors: Vec::new(),
            supervisors: Vec::new(),
            resources: BTreeMap::new(),
            closures: Vec::new(),
            vtables: Vec::new(),
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
            value_capabilities: BTreeMap::new(),
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
        // Name the shape this source demands rather than indexing the table.
        let shape = semantic
            .aggregate_shapes
            .iter()
            .find(|shape| shape.instance.nominal.display_name() == "Packet")
            .expect("source must demand one exact record shape");
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
            ir.contains("aggregate.clone.field"),
            "whole aggregate copy must execute the resolved recursive glue"
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
    fn string_prefix_uses_the_runtime_boolean_abi_across_native_targets() {
        let root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .to_path_buf();
        let semantic = lower_source_with_registry(
            r#"
            import std.string;
            fn main() -> i64 {
                if "éclair".starts_with("é") { 0 } else { 1 }
            }
            "#,
            ModuleRegistry::new(vec![root]),
        );
        let inventory = hew_mir::physical::physical_type_inventory(&semantic);
        for triple in [
            "x86_64-unknown-linux-gnu",
            "x86_64-pc-windows-msvc",
            "aarch64-apple-darwin",
        ] {
            let target = physical_target_for_inventory(triple, &inventory).unwrap();
            let verified = hew_mir::lower_physical_module(&semantic, target).unwrap();
            let ctx = Context::create();
            let machine =
                crate::llvm::target_machine_for_triple_with_opt_level(triple, OptLevel::O0)
                    .unwrap();
            let module = build_module(&ctx, verified.module(), "string_prefix", &machine).unwrap();
            module.verify().unwrap();
            let prefix = module.get_function("hew_string_starts_with").unwrap();
            assert_eq!(
                prefix.get_type().get_return_type(),
                Some(ctx.bool_type().into())
            );
            assert_eq!(
                prefix.get_type().get_param_types(),
                vec![ctx.ptr_type(AddressSpace::default()).into(); 2]
            );
        }
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

    #[test]
    fn extern_byte_calls_reuse_entry_storage() {
        use inkwell::values::InstructionOpcode;

        let semantic = lower_source(
            r#"
            extern "C" {
                fn make_bytes(seed: i32) -> bytes;
                fn relay_bytes(consume value: bytes) -> bytes;
            }
            fn main() {
                for i in 0..10 {
                    let value = unsafe { relay_bytes(make_bytes(i)) };
                    println(value.len());
                }
            }
            "#,
        );
        let triple = native_emission_triple();
        let inventory = hew_mir::physical::physical_type_inventory(&semantic);
        let target = physical_target_for_inventory(&triple, &inventory).unwrap();
        let verified = hew_mir::lower_physical_module(&semantic, target).unwrap();
        let ctx = Context::create();
        let machine =
            crate::llvm::target_machine_for_triple_with_opt_level(&triple, OptLevel::O0).unwrap();
        let module = build_module(&ctx, verified.module(), "extern_bytes", &machine).unwrap();
        for function in module.get_functions() {
            for block in function.get_basic_blocks() {
                let mut instruction = block.get_first_instruction();
                while let Some(current) = instruction {
                    if current.get_opcode() == InstructionOpcode::Alloca {
                        assert_eq!(
                            Some(block),
                            function.get_first_basic_block(),
                            "extern calls in loops must not grow scratch storage"
                        );
                    }
                    instruction = current.get_next_instruction();
                }
            }
        }
    }
}
