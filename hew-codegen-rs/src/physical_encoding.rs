//! Direct C ABI realization of managed values and synchronous resource operations.

use super::*;
use hew_types::{RuntimeCallFamily, RuntimeResultEffect};

impl FunctionEmitter<'_, '_> {
    pub(super) fn emit_direct_runtime_call(
        &self,
        family: RuntimeCallFamily,
        transfers: &[ArgumentTransfer],
        result: Option<StorageId>,
    ) -> CodegenResult<()> {
        let contract = family.semantic_contract().ok_or_else(|| {
            CodegenError::FailClosed("runtime operation lacks its semantic contract".into())
        })?;
        let updated_receiver = matches!(contract.result, RuntimeResultEffect::UpdatedReceiver(_));
        // Load moved owners before clearing their logical source storage.
        // File validity and error-presence Booleans use their audited C widths;
        // other scalar and handle carriers agree with physical storage.
        let values = transfers
            .iter()
            .map(|transfer| self.load(argument_source(transfer), "runtime.argument"))
            .collect::<CodegenResult<Vec<_>>>()?;
        let parameters = values
            .iter()
            .map(|value| value.get_type().into())
            .collect::<Vec<_>>();
        let return_type = result
            .filter(|_| !updated_receiver)
            .map(|id| llvm_type(self.ctx, &self.storage(id)?.layout.repr))
            .transpose()?;
        let validity = matches!(
            family,
            RuntimeCallFamily::FileRead(
                hew_types::runtime_call::FileReadOp::IsValid
                    | hew_types::runtime_call::FileReadOp::StreamIsValid
            )
        );
        let presence =
            family == RuntimeCallFamily::FileRead(hew_types::runtime_call::FileReadOp::HasError);
        let abi_return = if validity {
            Some(self.ctx.i32_type().into())
        } else if presence {
            Some(self.ctx.bool_type().into())
        } else {
            return_type
        };
        let signature = abi_return.map_or_else(
            || self.ctx.void_type().fn_type(&parameters, false),
            |ty| ty.fn_type(&parameters, false),
        );
        let function = get_or_declare_external(self.llvm, family.c_symbol(), signature)?;
        for transfer in transfers {
            if let ArgumentTransfer::Move(source) = transfer {
                self.clear_owned(*source)?;
            }
        }
        let arguments = values.iter().copied().map(Into::into).collect::<Vec<_>>();
        if return_type.is_some() {
            let value = self.runtime_call_value(function, &arguments, "runtime.result")?;
            let value = if validity || presence {
                let bit = if validity {
                    self.builder
                        .build_int_compare(
                            IntPredicate::NE,
                            value.into_int_value(),
                            self.ctx.i32_type().const_zero(),
                            "resource.valid",
                        )
                        .llvm_ctx("normalize resource validity")?
                } else {
                    value.into_int_value()
                };
                self.builder
                    .build_int_z_extend(bit, self.ctx.i8_type(), "resource.bool")
                    .llvm_ctx("store resource Boolean")?
                    .into()
            } else {
                value
            };
            self.store(result.expect("value-returning runtime operation"), value)?;
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
}
