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
    PhysicalAggregateId, PhysicalTypeInventory, TrapKind,
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
use hew_types::{EntryExitAction, EntryIntegerType, ResolvedTy};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::intrinsics::Intrinsic;
use inkwell::module::{Linkage, Module};
use inkwell::targets::{FileType, TargetData, TargetMachine};
use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum, FunctionType};
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
    physical_target_for_parts(triple, types, std::iter::empty())
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
    physical_target_for_parts(triple, inventory.types(), inventory.aggregates())
}

fn physical_target_for_parts<'a>(
    triple: &str,
    types: impl IntoIterator<Item = &'a ResolvedTy>,
    aggregates: impl IntoIterator<Item = &'a PhysicalAggregateDescriptor>,
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
    let mut visiting = BTreeSet::new();
    for ty in primitive_types() {
        realize_layout(
            &ctx,
            &data,
            &mut target,
            &ty,
            &aggregate_fields,
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
    let fields = match ty {
        ResolvedTy::Tuple(fields) => Some(fields.as_slice()),
        _ => aggregate_fields.get(ty).map(Vec::as_slice),
    };
    let repr = if let Some(fields) = fields {
        let mut layouts = Vec::with_capacity(fields.len());
        for field in fields {
            realize_layout(ctx, data, target, field, aggregate_fields, visiting)?;
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
    emitter.emit_functions()?;
    emitter.emit_entry()?;
    emitter
        .llvm
        .verify()
        .map_err(|error| CodegenError::LlvmVerify(error.to_string()))?;
    Ok(emitter.llvm)
}

impl<'ctx> ModuleEmitter<'ctx, '_> {
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
                let value =
                    self.clone_loaded_value(field_value, &self.storage(*dest)?.layout, *action)?;
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
        self.clone_loaded_value(value, &self.storage(source)?.layout, action)
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
        }
    }

    fn destroy_value(&self, source: StorageId, action: DestroyAction) -> CodegenResult<()> {
        let value = self.load(source, "destroy.source")?;
        self.destroy_loaded_value(value, &self.storage(source)?.layout, action)?;
        let zero = llvm_type(self.ctx, &self.storage(source)?.layout.repr)?.const_zero();
        self.store(source, zero)
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
                };
                let symbol = match action {
                    DestroyAction::StringRelease => "hew_string_drop",
                    DestroyAction::BytesRelease => "hew_bytes_drop",
                    DestroyAction::Aggregate(_) => unreachable!("matched primitive release"),
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
        }
    }

    fn aggregate_glue(&self, id: PhysicalAggregateId) -> CodegenResult<&PhysicalAggregateGlue> {
        self.module
            .aggregate_glue
            .get(id.0 as usize)
            .filter(|glue| glue.id == id)
            .ok_or_else(|| {
                CodegenError::FailClosed(format!("unknown physical aggregate glue {}", id.0))
            })
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
                        let temp = self
                            .builder
                            .build_alloca(
                                llvm_type(self.ctx, &parameter.layout.repr)?,
                                "call.clone.argument",
                            )
                            .llvm_ctx("allocate cloned indirect argument")?;
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
            PhysicalRuntimeAction::StringLen => {
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_string_length",
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
    fn string_length_uses_the_widened_runtime_abi() {
        let repo_root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("hew-codegen-rs must live under the repository root")
            .to_path_buf();
        let semantic = lower_source_with_registry(
            r#"
            import std.string;
            fn main() -> i64 { "length".len() }
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
