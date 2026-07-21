use hew_types::{RcIntrinsicOp, ResolvedTy};

use super::{Builder, HirExpr, Instr, Place};

impl Builder {
    pub(super) fn lower_rc_intrinsic(
        &mut self,
        op: RcIntrinsicOp,
        payload_ty: &ResolvedTy,
        receiver: Option<&HirExpr>,
        value: Option<&HirExpr>,
        result_ty: &ResolvedTy,
    ) -> Place {
        let receiver = receiver.and_then(|operand| self.lower_value(operand));
        let value = value.and_then(|operand| self.lower_value(operand));
        let result_ty = self.subst_ty(result_ty);
        let dest = self.alloc_local(result_ty.clone());
        self.push_instr(Instr::RcIntrinsic {
            dest,
            op,
            payload_ty: self.subst_ty(payload_ty),
            receiver,
            value,
            result_ty,
        });
        dest
    }
}
