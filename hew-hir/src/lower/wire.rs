//! Codec call lowering.

use super::*;

impl LowerCtx {
    /// Lower a format module's `encode(value)` or `decode<T>(document)` to a
    /// [`HirExprKind::WireCodec`] node. The operand is borrowed: the codec
    /// walks it without taking ownership. An encode produces the format's
    /// document; a decode produces the checker's `Result<T, wire.DecodeError>`.
    pub(super) fn lower_codec(
        &mut self,
        args: &[hew_parser::ast::CallArg],
        codec: Codec,
        value_ty: ResolvedTy,
        checked_ty: Option<ResolvedTy>,
        span: Span,
    ) -> (HirExprKind, ResolvedTy) {
        let result_ty = if codec.is_serialize() {
            Some(codec.document_ty())
        } else {
            checked_ty
        };
        let (Some(result_ty), [arg]) = (result_ty, args) else {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: "codec function".to_string(),
                    reason: format!(
                        "expected one argument and a checked result, found {} arguments",
                        args.len()
                    ),
                },
                span,
                "codec lowering takes exactly one argument",
            ));
            return (
                HirExprKind::Unsupported("codec call has invalid arity".to_string()),
                ResolvedTy::Unit,
            );
        };
        let operand = self.lower_expr(arg.expr(), IntentKind::Read);
        (
            HirExprKind::WireCodec {
                codec,
                operand: Box::new(operand),
                value_ty,
            },
            result_ty,
        )
    }
}
