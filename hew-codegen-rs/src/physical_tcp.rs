//! Native TCP uses checked integer handles, managed addresses and borrowed bytes.

use super::*;
use hew_types::runtime_call::TcpOp;

impl FunctionEmitter<'_, '_> {
    pub(super) fn emit_tcp_operation(
        &self,
        operation: TcpOp,
        transfers: &[ArgumentTransfer],
        result: Option<StorageId>,
    ) -> CodegenResult<()> {
        let mut values = Vec::with_capacity(transfers.len());
        for transfer in transfers {
            let source = argument_source(transfer);
            let value = if self.storage(source)?.ty == ResolvedTy::Bytes {
                self.slots[source.0 as usize].into()
            } else {
                self.load(source, "tcp.argument")?
            };
            values.push(value);
        }
        let parameters = values
            .iter()
            .map(|value| value.get_type().into())
            .collect::<Vec<_>>();
        let boolean = matches!(operation, TcpOp::ListenerValid | TcpOp::ConnectionValid);
        let return_type = if boolean {
            Some(self.ctx.bool_type().into())
        } else {
            result
                .map(|id| llvm_type(self.ctx, &self.storage(id)?.layout.repr))
                .transpose()?
        };
        let signature = return_type.map_or_else(
            || self.ctx.void_type().fn_type(&parameters, false),
            |ty| ty.fn_type(&parameters, false),
        );
        let symbol = match operation {
            TcpOp::Listen => "hew_checked_tcp_listen",
            _ => operation.c_symbol(),
        };
        let function = get_or_declare_external(self.llvm, symbol, signature)?;
        for transfer in transfers {
            if let ArgumentTransfer::Move(source) = transfer {
                self.clear_owned(*source)?;
            }
        }
        let arguments = values.into_iter().map(Into::into).collect::<Vec<_>>();
        if let Some(result) = result {
            let value = self.runtime_call_value(function, &arguments, "tcp.result")?;
            let value = if boolean {
                self.builder
                    .build_int_z_extend(value.into_int_value(), self.ctx.i8_type(), "tcp.valid")
                    .llvm_ctx("store TCP validity")?
                    .into()
            } else {
                value
            };
            self.store(result, value)?;
        } else {
            self.runtime_call_void(function, &arguments, "tcp.operation")?;
        }
        Ok(())
    }
}
