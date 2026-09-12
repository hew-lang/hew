//! Direct C ABI realization of managed values and synchronous resource operations.

use super::*;
use hew_mir::physical::PhysicalExternResultAbi;
use hew_types::{RuntimeCReturn, RuntimeCallFamily, RuntimeResultEffect};
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::types::AnyType;

impl FunctionEmitter<'_, '_> {
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
                // `bytes` is the runtime's `{ptr, u32, u32}` triple and every C
                // entry takes it by pointer. Everything else crosses by value
                // out of its storage.
                if self.storage(source)?.ty == ResolvedTy::Bytes {
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
        let mut parameters = values
            .iter()
            .map(|value| value.get_type().into())
            .collect::<Vec<_>>();
        let storage_type = result
            .map(|id| llvm_type(self.ctx, &self.storage(id)?.layout.repr))
            .transpose()?;
        let return_type = match result_abi {
            PhysicalExternResultAbi::Direct => storage_type,
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
        let indirect_result = if *result_abi == PhysicalExternResultAbi::Indirect {
            let result = result.ok_or_else(|| {
                CodegenError::FailClosed("indirect extern result lacks storage".into())
            })?;
            let sret = self.ctx.create_type_attribute(
                Attribute::get_named_enum_kind_id("sret"),
                storage_type
                    .expect("indirect result storage")
                    .as_any_type_enum(),
            );
            let align = self.ctx.create_enum_attribute(
                Attribute::get_named_enum_kind_id("align"),
                u64::from(self.storage(result)?.layout.align),
            );
            function.add_attribute(AttributeLoc::Param(0), sret);
            function.add_attribute(AttributeLoc::Param(0), align);
            Some((result, sret, align))
        } else {
            None
        };
        for transfer in transfers {
            if let ArgumentTransfer::Move(source) = transfer {
                self.clear_owned(*source)?;
            }
        }
        let mut arguments = values.iter().copied().map(Into::into).collect::<Vec<_>>();
        if let Some((result, sret, align)) = indirect_result {
            arguments.insert(0, self.slots[result.0 as usize].into());
            let call = self
                .builder
                .build_call(function, &arguments, "extern.call")
                .llvm_ctx("emit indirect extern result call")?;
            call.add_attribute(AttributeLoc::Param(0), sret);
            call.add_attribute(AttributeLoc::Param(0), align);
            // Mark the result initialized through the same storage contract
            // as a direct result, after the foreign function has written it.
            self.store(result, self.load(result, "extern.result")?)?;
        } else if let Some(result) = result {
            let mut value = self.runtime_call_value(function, &arguments, "extern.result")?;
            if matches!(result_abi, PhysicalExternResultAbi::Coerce(_)) {
                // The ABI carrier may include tail padding absent from storage
                // (AAPCS64's two integer registers for a 12-byte record), or vice
                // versa. Allocate enough space and alignment for both views.
                let storage = storage_type.expect("result storage");
                let data = TargetData::create(&self.module.target.data_layout);
                let (storage_size, storage_align) = measure_layout(&data, storage);
                let (carrier_size, carrier_align) = measure_layout(&data, value.get_type());
                let scratch_type = if storage_size >= carrier_size {
                    storage
                } else {
                    value.get_type()
                };
                let scratch = self
                    .value_emitter()
                    .entry_scratch(scratch_type, "extern.result.slot")?;
                scratch
                    .as_instruction()
                    .expect("entry allocation")
                    .set_alignment(storage_align.max(carrier_align))
                    .map_err(|error| CodegenError::FailClosed(error.to_string()))?;
                self.builder
                    .build_store(scratch, value)
                    .llvm_ctx("store extern result register carrier")?;
                value = self
                    .builder
                    .build_load(
                        storage_type.expect("result storage"),
                        scratch,
                        "extern.aggregate",
                    )
                    .llvm_ctx("load extern aggregate result")?;
            }
            self.store(result, value)?;
        } else {
            self.runtime_call_void(function, &arguments, "extern.call")?;
        }
        self.emit_result_edge(result, normal)
    }
}
