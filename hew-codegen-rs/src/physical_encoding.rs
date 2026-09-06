//! Direct C ABI realization of the shared JSON/YAML semantic protocol.

use super::*;
use hew_types::{RuntimeCallFamily, RuntimeResultEffect};

impl FunctionEmitter<'_, '_> {
    pub(super) fn emit_encoding_call(
        &self,
        family: RuntimeCallFamily,
        transfers: &[ArgumentTransfer],
        result: Option<StorageId>,
    ) -> CodegenResult<()> {
        let contract = family.semantic_contract().ok_or_else(|| {
            CodegenError::FailClosed("encoding operation lacks its semantic contract".into())
        })?;
        let updated_receiver = matches!(contract.result, RuntimeResultEffect::UpdatedReceiver(_));
        // Every admitted scalar and handle has the same physical and C carrier.
        // Load both moved owners before clearing their logical source storage.
        let values = transfers
            .iter()
            .map(|transfer| self.load(argument_source(transfer), "encoding.argument"))
            .collect::<CodegenResult<Vec<_>>>()?;
        let parameters = values
            .iter()
            .map(|value| value.get_type().into())
            .collect::<Vec<_>>();
        let return_type = result
            .filter(|_| !updated_receiver)
            .map(|id| llvm_type(self.ctx, &self.storage(id)?.layout.repr))
            .transpose()?;
        let signature = return_type.map_or_else(
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
            let value = self.runtime_call_value(function, &arguments, "encoding.result")?;
            self.store(result.expect("value-returning encoding operation"), value)?;
        } else {
            self.runtime_call_void(function, &arguments, "encoding.operation")?;
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
