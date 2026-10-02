//! Direct C ABI realization of managed values and synchronous resource operations.

use super::*;
use hew_mir::physical::PhysicalExternResultAbi;
use hew_types::{RuntimeCReturn, RuntimeCallFamily, RuntimeResultEffect};
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::types::AnyType;

impl<'ctx> FunctionEmitter<'_, 'ctx> {
    pub(super) fn emit_direct_runtime_call(
        &self,
        family: RuntimeCallFamily,
        transfers: &[ArgumentTransfer],
        result: Option<StorageId>,
    ) -> CodegenResult<()> {
        let row = family.row();
        let contract = row.contract.ok_or_else(|| {
            CodegenError::FailClosed("runtime operation lacks its semantic contract".into())
        })?;
        let updated_receiver = matches!(contract.result, RuntimeResultEffect::UpdatedReceiver(_));
        // Load moved owners before clearing their logical source storage.
        // File validity and error-presence Booleans use their audited C widths;
        // other scalar and handle carriers agree with physical storage.
        let values = transfers
            .iter()
            .map(|transfer| {
                let source = argument_source(transfer);
                // `bytes` is the runtime's `{ptr, u32, u32}` triple and the
                // identity carriers are its `HewNodeId` / `HewLocation` /
                // `HewRemotePid` structs; every C entry takes those by pointer,
                // since a first-class aggregate argument and a C struct
                // parameter do not agree on how the bytes travel. Everything
                // else crosses by value out of its storage.
                let ty = &self.storage(source)?.ty;
                if *ty == ResolvedTy::Bytes
                    || ty.is_builtin(hew_types::BuiltinType::NodeId)
                    || ty.is_builtin(hew_types::BuiltinType::Location)
                    || ty.is_builtin(hew_types::BuiltinType::RemotePid)
                {
                    if matches!(transfer, ArgumentTransfer::Move(_)) {
                        return Err(CodegenError::FailClosed(
                            "a moved byte operand needs its own emission".into(),
                        ));
                    }
                    Ok(self.slots[source.0 as usize].into())
                } else {
                    self.load(source, "runtime.argument")
                }
            })
            .collect::<CodegenResult<Vec<_>>>()?;
        if let (RuntimeCReturn::Storage, false, Some(result)) =
            (row.c_return, updated_receiver, result)
        {
            // A C struct result travels by the target's C convention (x8 sret
            // on AAPCS64 for a 32-byte `HewLocation`), which a first-class
            // LLVM aggregate return does not follow.
            let result_abi = self
                .module
                .target
                .extern_result_abi(&self.storage(result)?.ty)
                .map_err(|error| CodegenError::FailClosed(error.to_string()))?;
            return self.emit_c_call(row.symbol, &values, transfers, Some(result), &result_abi);
        }
        let parameters = values
            .iter()
            .map(|value| value.get_type().into())
            .collect::<Vec<_>>();
        let return_type = result
            .filter(|_| !updated_receiver)
            .map(|id| llvm_type(self.ctx, &self.storage(id)?.layout.repr))
            .transpose()?;
        // A runtime entry that answers a question returns a C truth of its own
        // width; the row says which, and the result lands in the language's
        // one-byte `bool`.
        let truth = row.c_return;
        let abi_return = match truth {
            RuntimeCReturn::TruthI32 => Some(self.ctx.i32_type().into()),
            RuntimeCReturn::TruthBool => Some(self.ctx.bool_type().into()),
            RuntimeCReturn::Storage => return_type,
        };
        let signature = abi_return.map_or_else(
            || self.ctx.void_type().fn_type(&parameters, false),
            |ty| ty.fn_type(&parameters, false),
        );
        let function = get_or_declare_external(self.llvm, row.symbol, signature)?;
        for transfer in transfers {
            if let ArgumentTransfer::Move(source) = transfer {
                self.clear_owned(*source)?;
            }
        }
        let arguments = values.iter().copied().map(Into::into).collect::<Vec<_>>();
        if return_type.is_some() {
            let value = self.runtime_call_value(function, &arguments, "runtime.result")?;
            let destination = result.expect("value-returning runtime operation");
            let value = match truth {
                RuntimeCReturn::Storage => value,
                RuntimeCReturn::TruthI32 | RuntimeCReturn::TruthBool => {
                    let bit = if truth == RuntimeCReturn::TruthI32 {
                        self.builder
                            .build_int_compare(
                                IntPredicate::NE,
                                value.into_int_value(),
                                self.ctx.i32_type().const_zero(),
                                "runtime.truth",
                            )
                            .llvm_ctx("normalize a runtime truth")?
                    } else {
                        value.into_int_value()
                    };
                    let bool_ty = llvm_type(self.ctx, &self.storage(destination)?.layout.repr)?
                        .into_int_type();
                    self.builder
                        .build_int_z_extend(bit, bool_ty, "runtime.bool")
                        .llvm_ctx("store a runtime Boolean")?
                        .into()
                }
            };
            self.store(destination, value)?;
        } else {
            self.runtime_call_void(function, &arguments, "runtime.operation")?;
            if updated_receiver {
                // ObjectSet/ArrayPush consume the child and mutate the receiver
                // through a C-void call. The same receiver remains the result owner.
                self.store(
                    result.ok_or_else(|| {
                        CodegenError::FailClosed("encoding mutation lacks its result owner".into())
                    })?,
                    values[0],
                )?;
            }
        }
        Ok(())
    }

    /// Realize one declared C-ABI call as a direct call to its linker symbol.
    ///
    /// Consume the target-classified result ABI. Moved arguments discharge
    /// their obligation at the call.
    pub(super) fn emit_extern_call(
        &self,
        symbol: &str,
        transfers: &[ArgumentTransfer],
        result: Option<StorageId>,
        result_abi: &PhysicalExternResultAbi,
        normal: &PhysicalEdge,
    ) -> CodegenResult<()> {
        // `bytes` is the runtime's `{ptr, u32, u32}` triple, and every C-side
        // declaration takes it by pointer (`*const BytesTriple`). Everything
        // else - scalars, pointer-width handles, `string`, collections -
        // passes by value out of its storage.
        let values = transfers
            .iter()
            .map(|transfer| {
                let source = argument_source(transfer);
                if self.storage(source)?.ty == ResolvedTy::Bytes {
                    if matches!(transfer, ArgumentTransfer::Move(_)) {
                        // Clearing a moved owner must not zero the C argument.
                        // Scratch is in the allocation prologue, so a loop
                        // reuses it instead of growing the native stack.
                        let value = self.load(source, "extern.bytes.argument")?;
                        let scratch = self
                            .value_emitter()
                            .entry_scratch(value.get_type(), "extern.bytes.argument.slot")?;
                        self.builder
                            .build_store(scratch, value)
                            .llvm_ctx("snapshot moved extern byte argument")?;
                        Ok(scratch.into())
                    } else {
                        Ok(self.slots[source.0 as usize].into())
                    }
                } else {
                    self.load(source, "extern.argument")
                }
            })
            .collect::<CodegenResult<Vec<_>>>()?;
        self.emit_c_call(symbol, &values, transfers, result, result_abi)?;
        self.emit_result_edge(result, normal)
    }

    /// Call a C symbol with prepared arguments, initializing `result` under
    /// the target-classified result ABI. Moved arguments discharge their
    /// obligation at the call.
    fn emit_c_call(
        &self,
        symbol: &str,
        values: &[BasicValueEnum<'ctx>],
        transfers: &[ArgumentTransfer],
        result: Option<StorageId>,
        result_abi: &PhysicalExternResultAbi,
    ) -> CodegenResult<()> {
        let destination = result
            .map(|id| {
                let layout = &self.storage(id)?.layout;
                Ok::<_, CodegenError>(ForeignResult {
                    destination: self.slots[id.0 as usize],
                    storage: llvm_type(self.ctx, &layout.repr)?,
                    align: layout.align,
                })
            })
            .transpose()?;
        for transfer in transfers {
            if let ArgumentTransfer::Move(source) = transfer {
                self.clear_owned(*source)?;
            }
        }
        self.value_emitter()
            .call_foreign(symbol, values, destination, result_abi)?;
        if let Some(result) = result {
            // Mark the result initialized through the ordinary storage
            // contract, after the foreign function has written it.
            self.store(result, self.load(result, "extern.result")?)?;
        }
        Ok(())
    }
}

/// Where a foreign call's result lands: aligned storage of one value.
pub(super) struct ForeignResult<'ctx> {
    pub destination: PointerValue<'ctx>,
    pub storage: BasicTypeEnum<'ctx>,
    pub align: u32,
}

impl<'ctx> ValueEmitter<'_, 'ctx> {
    /// Declare and call a C symbol under the target-classified result ABI,
    /// writing its result to `result`. This is the one realization of the
    /// extern call ABI, shared by direct calls and offloaded call thunks.
    pub(super) fn call_foreign(
        &self,
        symbol: &str,
        values: &[BasicValueEnum<'ctx>],
        result: Option<ForeignResult<'ctx>>,
        result_abi: &PhysicalExternResultAbi,
    ) -> CodegenResult<()> {
        let mut parameters = values
            .iter()
            .map(|value| value.get_type().into())
            .collect::<Vec<_>>();
        let return_type = match result_abi {
            PhysicalExternResultAbi::Direct => result.as_ref().map(|result| result.storage),
            PhysicalExternResultAbi::Coerce(repr) => Some(llvm_type(self.ctx, repr)?),
            PhysicalExternResultAbi::Indirect => {
                parameters.insert(0, self.ctx.ptr_type(AddressSpace::default()).into());
                None
            }
        };
        let signature = return_type.map_or_else(
            || self.ctx.void_type().fn_type(&parameters, false),
            |ty| ty.fn_type(&parameters, false),
        );
        let function = get_or_declare_external(self.llvm, symbol, signature)?;
        let mut arguments = values.iter().copied().map(Into::into).collect::<Vec<_>>();
        let call_value = |arguments: &[BasicMetadataValueEnum<'ctx>]| {
            self.builder
                .build_call(function, arguments, "extern.result")
                .llvm_ctx("emit extern call")?
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| CodegenError::FailClosed("extern call returned no value".into()))
        };
        match (result, result_abi) {
            (Some(result), PhysicalExternResultAbi::Indirect) => {
                let sret = self.ctx.create_type_attribute(
                    Attribute::get_named_enum_kind_id("sret"),
                    result.storage.as_any_type_enum(),
                );
                let align = self.ctx.create_enum_attribute(
                    Attribute::get_named_enum_kind_id("align"),
                    u64::from(result.align),
                );
                function.add_attribute(AttributeLoc::Param(0), sret);
                function.add_attribute(AttributeLoc::Param(0), align);
                arguments.insert(0, result.destination.into());
                let call = self
                    .builder
                    .build_call(function, &arguments, "extern.call")
                    .llvm_ctx("emit indirect extern result call")?;
                call.add_attribute(AttributeLoc::Param(0), sret);
                call.add_attribute(AttributeLoc::Param(0), align);
            }
            (None, PhysicalExternResultAbi::Indirect) => {
                return Err(CodegenError::FailClosed(
                    "indirect extern result lacks storage".into(),
                ));
            }
            (Some(result), PhysicalExternResultAbi::Coerce(_)) => {
                let value = call_value(&arguments)?;
                // The ABI carrier may include tail padding absent from storage
                // (AAPCS64's two integer registers for a 12-byte record), or vice
                // versa. Allocate enough space and alignment for both views.
                let data = TargetData::create(&self.module.target.data_layout);
                let (storage_size, storage_align) = measure_layout(&data, result.storage);
                let (carrier_size, carrier_align) = measure_layout(&data, value.get_type());
                let scratch_type = if storage_size >= carrier_size {
                    result.storage
                } else {
                    value.get_type()
                };
                let scratch = self.entry_scratch(scratch_type, "extern.result.slot")?;
                scratch
                    .as_instruction()
                    .expect("entry allocation")
                    .set_alignment(storage_align.max(carrier_align))
                    .map_err(|error| CodegenError::FailClosed(error.to_string()))?;
                self.builder
                    .build_store(scratch, value)
                    .llvm_ctx("store extern result register carrier")?;
                let value = self
                    .builder
                    .build_load(result.storage, scratch, "extern.aggregate")
                    .llvm_ctx("load extern aggregate result")?;
                self.builder
                    .build_store(result.destination, value)
                    .llvm_ctx("store extern aggregate result")?;
            }
            (Some(result), PhysicalExternResultAbi::Direct) => {
                let value = call_value(&arguments)?;
                self.builder
                    .build_store(result.destination, value)
                    .llvm_ctx("store extern result")?;
            }
            (None, _) => {
                let call = self
                    .builder
                    .build_call(function, &arguments, "")
                    .llvm_ctx("emit extern call")?;
                if call.try_as_basic_value().basic().is_some() {
                    return Err(CodegenError::FailClosed(
                        "unit extern call unexpectedly returned a value".into(),
                    ));
                }
            }
        }
        Ok(())
    }
}
