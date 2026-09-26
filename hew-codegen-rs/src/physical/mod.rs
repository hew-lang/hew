//! LLVM emitter for verified physical MIR.
//!
//! This path consumes no raw, checked, or elaborated MIR. All ownership,
//! storage, layout, and private ABI choices are already explicit in the
//! verified physical module.

#[path = "../physical_actor.rs"]
mod actor;
#[path = "../physical_actor_codec.rs"]
mod actor_codec;

#[path = "../physical_supervisor.rs"]
mod supervisor;

#[path = "../physical_callable.rs"]
mod callable;

#[path = "../physical_wire.rs"]
mod wire;

#[path = "../physical_collection_callbacks.rs"]
mod collection_callbacks;
#[path = "../physical_encoding.rs"]
mod encoding;
#[path = "../physical_key.rs"]
mod key;
#[path = "../physical_partial.rs"]
mod partial;
#[path = "../physical_tcp.rs"]
mod tcp;

#[path = "../physical_coro.rs"]
mod coro;
#[path = "../physical_debug.rs"]
mod debug;
#[path = "../physical_generators.rs"]
mod generators;
#[path = "../physical_io.rs"]
mod io;
#[path = "../physical_select.rs"]
mod select;
#[path = "../physical_stream.rs"]
mod stream;
#[path = "../physical_suspend.rs"]
mod suspend;

#[path = "../physical_tasks.rs"]
mod tasks;

#[path = "../physical_dyn.rs"]
mod dyn_object;
#[path = "../physical_host.rs"]
mod host;
#[path = "../physical_release.rs"]
mod release;
#[path = "../physical_shared.rs"]
mod shared;
#[path = "../physical_structural.rs"]
mod structural;

mod collections;
mod control;
mod function;
mod helpers;
mod module_emitter;
mod runtime_calls;
mod text;
mod value_emitter;

use helpers::*;
use module_emitter::*;
use value_emitter::*;

pub use debug::DebugSource;
pub use host::HostExport;

use collection_callbacks::CollectionProbe;

use std::collections::{BTreeMap, BTreeSet};
use std::num::NonZeroU32;
use std::path::Path;

use hew_mir::physical::{
    BlockId, CallableId, OwnKind, PhysicalAggregateDescriptor, PhysicalAggregateGlue,
    PhysicalAggregateId, PhysicalCleanup, PhysicalMapId, PhysicalMapOp, PhysicalResourceDescriptor,
    PhysicalSetId, PhysicalSetOp, PhysicalTypeInventory, PhysicalValueRecipe, PhysicalVariantArm,
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
    /// Source file whose text attributes native debug metadata; `None` emits
    /// no debug info at all.
    pub debug_source: Option<&'a Path>,
    /// Link the freestanding `wasm32-unknown-unknown` object into a `.wasm`
    /// module with `wasm-ld --no-entry`. Ignored on every other triple: a WASI
    /// module is linked against the runtime archives by `hew-cli`'s linker.
    pub link_freestanding_wasm: bool,
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
    let wasm = triple.starts_with("wasm32");
    let suffix = if wasm { "wasm.o" } else { "o" };
    let object_path = options
        .out_dir
        .join(format!("{}.{suffix}", options.module_name));
    emit_physical_to_paths(
        verified,
        options.module_name,
        &triple,
        options.opt_level,
        options.address_sanitizer,
        ll_path.as_deref(),
        Some(&object_path),
        host,
        options.debug_source,
    )?;
    if !wasm {
        return Ok(EmitArtefacts {
            ll_path,
            native_obj_path: Some(object_path),
            ..EmitArtefacts::default()
        });
    }
    // The freestanding link produces a standalone module with no runtime
    // archive; a WASI module is linked by `hew-cli` against the wasm32
    // runtime and std archives instead.
    let wasm_path = if options.link_freestanding_wasm {
        let path = options
            .out_dir
            .join(format!("{}.wasm", options.module_name));
        crate::llvm::link_freestanding_wasm_module(&object_path, &path)?;
        Some(path)
    } else {
        None
    };
    Ok(EmitArtefacts {
        ll_path,
        wasm_obj_path: Some(object_path),
        wasm_path,
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
    debug_source: Option<&Path>,
) -> CodegenResult<()> {
    let machine = crate::llvm::target_machine_for_triple_with_opt_level(triple, opt_level)?;
    let ctx = Context::create();
    // Fail closed: `-g` against an unreadable source emits a location-free
    // object rather than a fabricated line table.
    let debug_text = debug_source.and_then(|path| std::fs::read_to_string(path).ok());
    let debug = match (debug_source, &debug_text) {
        (Some(path), Some(text)) => Some(DebugSource { path, text }),
        _ => None,
    };
    let llvm_module =
        build_module_with_host(&ctx, verified.module(), module_name, &machine, host, debug)?;
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
        ResolvedTy::Borrow { .. }
        | ResolvedTy::String
        | ResolvedTy::CancellationToken
        | ResolvedTy::Array(_, _) => PhysicalRepr::Pointer,
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
            if actor.is_builtin(hew_types::BuiltinType::ActorHandle)
                || actor.is_builtin(hew_types::BuiltinType::ActorFn) =>
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
        callback if *callback == hew_mir::physical::ActorIngressAdapter::pointer_type() => {
            PhysicalRepr::Pointer
        }
        // An `#[opaque]` nominal with no resource descriptor is a bit-copied
        // FFI id of pointer width; its lifecycle belongs to whatever owns it.
        ResolvedTy::Named {
            head:
                hew_types::TypeHead::Nominal(_)
                | hew_types::TypeHead::Param(_)
                | hew_types::TypeHead::Unresolved(_),
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
        PhysicalRepr::Vector { element, len } => match llvm_type(ctx, &element.repr)? {
            BasicTypeEnum::IntType(ty) => ty.vec_type(*len).into(),
            BasicTypeEnum::FloatType(ty) => ty.vec_type(*len).into(),
            _ => {
                return Err(CodegenError::FailClosed(
                    "C register vector requires scalar elements".into(),
                ))
            }
        },
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
    debug: Option<debug::DebugEmitter<'ctx>>,
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
    debug: Option<(
        &'a debug::DebugEmitter<'ctx>,
        debug::FunctionDebug<'ctx>,
        &'a hew_mir::physical::PhysicalDebugFunction,
    )>,
    prologue: BasicBlock<'ctx>,
    pending_locals: Vec<debug::PendingLocal<'ctx>>,
}

/// Execute verified type recipes in either a language body or a container
/// element callback. Storage ownership and ABI transfers remain physical MIR facts.
struct ValueEmitter<'a, 'ctx> {
    module: &'a PhysicalModule,
    ctx: &'ctx Context,
    llvm: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    value: FunctionValue<'ctx>,
    /// The enclosing frame's fault record: its optional owner and the status
    /// that owner was raised with. A release emitted into a frame reports a
    /// failing `close` here and keeps releasing. Release glue has no
    /// frame, so it carries `None` and raises through the trap path instead.
    fault_sink: Option<(PointerValue<'ctx>, PointerValue<'ctx>)>,
}

#[cfg(test)]
#[path = "../physical_debug_tests.rs"]
mod debug_tests;

#[cfg(test)]
#[path = "../physical_resource_tests.rs"]
mod resource_tests;

#[cfg(test)]
mod tests;
