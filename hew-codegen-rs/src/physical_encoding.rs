//! Direct C ABI realization of managed values and synchronous resource operations.

use super::*;
use hew_mir::physical::PhysicalExternResultAbi;
use hew_types::{RuntimeCReturn, RuntimeCallFamily};
use inkwell::attributes::AttributeLoc;

impl<'ctx> FunctionEmitter<'_, 'ctx> {
    /// A checked synchronous container allocation publishes its typed result
    /// only after the provider's reservation succeeds.
    pub(super) fn checked_managed_pointer_call(
        &self,
        symbol: &str,
        mut values: Vec<BasicValueEnum<'ctx>>,
        result: StorageId,
        failure: Option<&PhysicalEdge>,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let failure = failure.ok_or_else(|| {
            CodegenError::FailClosed("managed container allocation lacks quota cleanup".into())
        })?;
        values.push(self.managed_value_layout(&self.storage(result)?.ty)?.into());
        let status = self
            .value_emitter()
            .entry_scratch(self.ctx.i32_type().into(), "container.status")?;
        self.builder
            .build_store(status, self.ctx.i32_type().const_zero())
            .llvm_ctx("initialize container quota status")?;
        values.push(status.into());
        let parameters = values
            .iter()
            .map(|value| value.get_type().into())
            .collect::<Vec<_>>();
        let function = get_or_declare_external(
            self.llvm,
            symbol,
            self.ctx
                .ptr_type(AddressSpace::default())
                .fn_type(&parameters, false),
        )?;
        let value = self.runtime_call_value(
            function,
            &values.iter().copied().map(Into::into).collect::<Vec<_>>(),
            "container.result",
        )?;
        let status = self
            .builder
            .build_load(self.ctx.i32_type(), status, "container.status")
            .llvm_ctx("read container quota status")?
            .into_int_value();
        self.check_budget_status(status, failure)?;
        Ok(value)
    }

    /// Provider allocations adopt their result once; mutations preserve every
    /// input until the provider's canonical replacement transaction succeeds.
    pub(super) fn emit_checked_encoding_call(
        &self,
        family: RuntimeCallFamily,
        arguments: &PhysicalArguments,
        result: Option<StorageId>,
        failure: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let row = family.row();
        let contract = row.contract.ok_or_else(|| {
            CodegenError::FailClosed("checked encoding call lacks its owning contract".into())
        })?;
        let allocating = matches!(
            contract.result,
            hew_types::RuntimeResultEffect::FreshOwned(_)
        );
        let in_place = row.c_return == RuntimeCReturn::ReceiverInPlace;
        if !allocating && !in_place {
            return Err(CodegenError::FailClosed(
                "checked encoding call has no managed effect".into(),
            ));
        }
        let transfers: &[ArgumentTransfer] = arguments;
        let mut values = transfers
            .iter()
            .zip(arguments.carriers())
            .map(|(transfer, carrier)| {
                self.emit_physical_argument(transfer, *carrier, "encoding.argument")
            })
            .collect::<CodegenResult<Vec<_>>>()?;
        let destination = result.ok_or_else(|| {
            CodegenError::FailClosed("checked encoding call lacks its result storage".into())
        })?;
        let result_abi = if allocating {
            values.push(
                self.managed_value_layout(&self.storage(destination)?.ty)?
                    .into(),
            );
            self.module
                .target
                .extern_result_abi(&self.storage(destination)?.ty)
                .map_err(|error| CodegenError::FailClosed(error.to_string()))?
        } else {
            PhysicalExternResultAbi::Direct
        };
        let status = self
            .value_emitter()
            .entry_scratch(self.ctx.i32_type().into(), "encoding.status")?;
        self.builder
            .build_store(status, self.ctx.i32_type().const_zero())
            .llvm_ctx("initialize checked encoding status")?;
        values.push(status.into());
        let foreign_result = if allocating {
            let storage = self.storage(destination)?;
            Some(ForeignResult {
                destination: self.slots[destination.0 as usize],
                storage: llvm_type(self.ctx, &storage.layout.repr)?,
                align: storage.layout.align,
            })
        } else {
            None
        };
        self.value_emitter().call_foreign(
            &format!("{}_checked", row.symbol),
            &values,
            &self.argument_widening(transfers)?,
            &[],
            foreign_result,
            &result_abi,
        )?;
        let status = self
            .builder
            .build_load(self.ctx.i32_type(), status, "encoding.status")
            .llvm_ctx("read checked encoding status")?
            .into_int_value();
        self.check_budget_status(status, failure)?;
        for transfer in transfers {
            if let ArgumentTransfer::Move(source) = transfer {
                self.clear_owned(*source)?;
            }
        }
        if in_place {
            self.store(destination, values[0])?;
        }
        Ok(())
    }

    pub(super) fn emit_direct_runtime_call(
        &self,
        family: RuntimeCallFamily,
        arguments: &PhysicalArguments,
        result: Option<StorageId>,
    ) -> CodegenResult<()> {
        let transfers: &[ArgumentTransfer] = arguments;
        let row = family.row();
        // Explicit source aliases acquire an account edge; physical copy glue
        // keeps the raw row symbol under its surrounding materialization permit.
        let symbol = if family == RuntimeCallFamily::RcClone {
            "hew_rc_clone_managed"
        } else {
            row.symbol
        };
        row.contract.ok_or_else(|| {
            CodegenError::FailClosed("runtime operation lacks its semantic contract".into())
        })?;
        let in_place = row.c_return == RuntimeCReturn::ReceiverInPlace;
        // Load moved owners before clearing their logical source storage.
        // File validity and error-presence Booleans use their audited C widths;
        // other scalar and handle carriers agree with physical storage.
        let values = transfers
            .iter()
            .zip(arguments.carriers())
            .map(|(transfer, carrier)| {
                self.emit_physical_argument(transfer, *carrier, "runtime.argument")
            })
            .collect::<CodegenResult<Vec<_>>>()?;
        if let (RuntimeCReturn::Storage, false, Some(result)) = (row.c_return, in_place, result) {
            // A C struct result travels by the target's C convention (x8 sret
            // on AAPCS64 for a 32-byte `HewLocation`), which a first-class
            // LLVM aggregate return does not follow.
            let result_abi = self
                .module
                .target
                .extern_result_abi(&self.storage(result)?.ty)
                .map_err(|error| CodegenError::FailClosed(error.to_string()))?;
            return self.emit_c_call(symbol, &values, transfers, Some(result), &result_abi);
        }
        let parameters = values
            .iter()
            .map(|value| value.get_type().into())
            .collect::<Vec<_>>();
        let return_type = result
            .filter(|_| !in_place)
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
            RuntimeCReturn::ReceiverInPlace => None,
        };
        let signature = abi_return.map_or_else(
            || self.ctx.void_type().fn_type(&parameters, false),
            |ty| ty.fn_type(&parameters, false),
        );
        let function = get_or_declare_external_widened(
            self.llvm,
            symbol,
            signature,
            &self.argument_widening(transfers)?,
        )?;
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
                RuntimeCReturn::ReceiverInPlace => {
                    return Err(CodegenError::FailClosed(
                        "in-place C mutation unexpectedly returns a value".into(),
                    ))
                }
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
            if in_place {
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
        arguments: &PhysicalArguments,
        result: Option<StorageId>,
        result_abi: &PhysicalExternResultAbi,
        normal: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let transfers: &[ArgumentTransfer] = arguments;
        let values = transfers
            .iter()
            .zip(arguments.carriers())
            .map(|(transfer, carrier)| {
                let value = self.emit_physical_argument(transfer, *carrier, "extern.argument")?;
                self.value_emitter()
                    .c_scalar_argument(value, &self.storage(argument_source(transfer))?.ty)
            })
            .collect::<CodegenResult<Vec<_>>>()?;
        self.emit_c_call(symbol, &values, transfers, result, result_abi)?;
        self.emit_result_edge(result, normal)
    }

    pub(super) fn emit_physical_argument(
        &self,
        transfer: &ArgumentTransfer,
        carrier: ParamCarrier,
        name: &str,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let source = argument_source(transfer);
        if carrier == ParamCarrier::Direct {
            return self.load(source, name);
        }
        if matches!(transfer, ArgumentTransfer::Move(_)) {
            let value = self.load(source, name)?;
            let scratch = self
                .value_emitter()
                .entry_scratch(value.get_type(), &format!("{name}.slot"))?;
            self.builder
                .build_store(scratch, value)
                .llvm_ctx("snapshot moved indirect argument")?;
            return Ok(scratch.into());
        }
        Ok(self.slots[source.0 as usize].into())
    }

    /// How each narrow argument widens, from the C type its storage holds.
    fn argument_widening(
        &self,
        transfers: &[ArgumentTransfer],
    ) -> CodegenResult<Vec<(u32, Widen)>> {
        let mut widen = Vec::new();
        for (index, transfer) in (0_u32..).zip(transfers) {
            if let Some(kind) = Widen::of(&self.storage(argument_source(transfer))?.ty) {
                widen.push((index, kind));
            }
        }
        Ok(widen)
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
        self.value_emitter().call_foreign(
            symbol,
            values,
            &self.argument_widening(transfers)?,
            &transfers
                .iter()
                .map(|arg| self.storage(argument_source(arg)).map(|slot| &slot.ty))
                .collect::<CodegenResult<Vec<_>>>()?,
            destination,
            result_abi,
        )?;
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
    /// C truth carriers differ from Hew's byte-sized Boolean storage.
    pub(super) fn c_scalar_argument(
        &self,
        value: BasicValueEnum<'ctx>,
        ty: &ResolvedTy,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        if *ty == ResolvedTy::Bool {
            let value = value.into_int_value();
            return self
                .builder
                .build_int_compare(
                    inkwell::IntPredicate::NE,
                    value,
                    value.get_type().const_zero(),
                    "extern.truth",
                )
                .map(Into::into)
                .llvm_ctx("convert Boolean to C truth");
        }
        Ok(value)
    }

    /// Declare and call a C symbol under the target-classified result ABI,
    /// writing its result to `result`. This is the one realization of the
    /// extern call ABI, shared by direct calls and offloaded call thunks.
    /// `widen` names each narrow argument of `values` by position.
    pub(super) fn call_foreign(
        &self,
        symbol: &str,
        values: &[BasicValueEnum<'ctx>],
        widen: &[(u32, Widen)],
        source_types: &[&ResolvedTy],
        result: Option<ForeignResult<'ctx>>,
        result_abi: &PhysicalExternResultAbi,
    ) -> CodegenResult<()> {
        let (values, mut widen, mut attrs) =
            self.c_arguments(values, widen, source_types, result_abi)?;
        let mut parameters: Vec<BasicMetadataTypeEnum> =
            values.iter().map(|v| v.get_type().into()).collect();
        let return_type = match result_abi {
            PhysicalExternResultAbi::Direct => result.as_ref().map(|result| result.storage),
            PhysicalExternResultAbi::Coerce(repr) => Some(llvm_type(self.ctx, repr)?),
            PhysicalExternResultAbi::Indirect => {
                parameters.insert(0, self.ctx.ptr_type(AddressSpace::default()).into());
                for (index, _) in &mut widen {
                    *index += 1;
                }
                None
            }
        };
        let signature = return_type.map_or_else(
            || self.ctx.void_type().fn_type(&parameters, false),
            |ty| ty.fn_type(&parameters, false),
        );
        let function = get_or_declare_external_widened(self.llvm, symbol, signature, &widen)?;
        let mut arguments: Vec<BasicMetadataValueEnum> =
            values.iter().copied().map(Into::into).collect();
        if matches!(result_abi, PhysicalExternResultAbi::Indirect) {
            for (idx, _) in &mut attrs {
                *idx += 1;
            }
            let dest = result.as_ref().ok_or_else(|| {
                CodegenError::FailClosed("indirect extern result lacks storage".into())
            })?;
            attrs.extend(c_export::memory_attributes(
                0,
                self.ctx,
                dest.storage,
                "sret",
                dest.align,
            ));
            arguments.insert(0, dest.destination.into());
        }
        let call = self
            .builder
            .build_call(function, &arguments, "extern.call")
            .llvm_ctx("emit C call")?;
        for (idx, attr) in attrs {
            function.add_attribute(AttributeLoc::Param(idx), attr);
            call.add_attribute(AttributeLoc::Param(idx), attr);
        }
        if !matches!(result_abi, PhysicalExternResultAbi::Indirect) {
            match (result, call.try_as_basic_value().basic()) {
                (Some(dest), Some(value)) => {
                    let value = self.c_reinterpret(value, dest.storage)?;
                    self.builder
                        .build_store(dest.destination, value)
                        .llvm_ctx("store C result")?;
                }
                (None, None) => {}
                _ => {
                    return Err(CodegenError::FailClosed(
                        "C result disagrees with destination".into(),
                    ))
                }
            }
        }
        Ok(())
    }
}
