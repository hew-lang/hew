//! Wire codec lowering.

use super::*;

impl LowerCtx {
    /// Lower a binary wire-codec call (`value.encode()` / `Type.decode(bytes)`)
    /// on a `#[wire]` struct to a [`HirExprKind::WireCodec`] node.
    ///
    /// `Encode`: the receiver is the value to serialize; the call result is
    /// `bytes`. `Decode`: the single argument is the `bytes` to read; the call
    /// result is `value_ty`. The operand is borrowed (`IntentKind::Read`) — the
    /// codec walks the value/bytes without taking ownership; the caller's
    /// binding stays live for its own scope-exit drop.
    pub(super) fn lower_wire_codec(
        &mut self,
        receiver: &Spanned<Expr>,
        args: &[hew_parser::ast::CallArg],
        direction: WireCodecDirection,
        value_ty: ResolvedTy,
        span: Span,
    ) -> (HirExprKind, ResolvedTy) {
        // `Result<value_ty, string>` — the ratified shape of `from_json` /
        // `from_yaml` (A201). Built here deterministically so the HIR node's
        // result type and the checker-registered return type agree.
        let result_self_str = || ResolvedTy::Named {
            name: "Result".to_string(),
            args: vec![value_ty.clone(), ResolvedTy::String],
            builtin: Some(hew_types::BuiltinType::Result),
            is_opaque: false,
        };
        let (operand, result_ty) = match direction {
            // Serialize directions: the receiver is the value to serialize. The
            // binary `Encode` produces `bytes`; the text `ToJson`/`ToYaml`
            // produce a `string` (the bridge transcodes the CBOR tree to text).
            WireCodecDirection::Encode
            | WireCodecDirection::ToJson
            | WireCodecDirection::ToYaml => {
                if !args.is_empty() {
                    let method = match direction {
                        WireCodecDirection::Encode => "encode",
                        WireCodecDirection::ToJson => "to_json",
                        _ => "to_yaml",
                    };
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: format!("wire `.{method}()`"),
                            reason: format!("expected zero arguments, found {}", args.len()),
                        },
                        span.clone(),
                        "wire serialize lowering takes no arguments",
                    ));
                    let fallback = if direction == WireCodecDirection::Encode {
                        ResolvedTy::Bytes
                    } else {
                        ResolvedTy::String
                    };
                    return (
                        HirExprKind::Unsupported("wire serialize method has invalid arity".into()),
                        fallback,
                    );
                }
                let operand = self.lower_expr(receiver, IntentKind::Read);
                let result = if direction == WireCodecDirection::Encode {
                    ResolvedTy::Bytes
                } else {
                    ResolvedTy::String
                };
                (operand, result)
            }
            // Deserialize directions: the single argument is the encoded input.
            // The binary `Decode` consumes `bytes` and produces bare `value_ty`
            // (trap-on-failure); the text `FromJson`/`FromYaml` consume a
            // `string` and produce `Result<value_ty, string>` (parse can fail
            // on arbitrary input — never a trap).
            WireCodecDirection::Decode
            | WireCodecDirection::FromJson
            | WireCodecDirection::FromYaml => {
                if args.len() != 1 {
                    let (method, arg_desc) = match direction {
                        WireCodecDirection::Decode => ("decode", "bytes"),
                        WireCodecDirection::FromJson => ("from_json", "string"),
                        _ => ("from_yaml", "string"),
                    };
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: format!("wire `.{method}()`"),
                            reason: format!(
                                "expected one {arg_desc} argument, found {}",
                                args.len()
                            ),
                        },
                        span.clone(),
                        "wire deserialize lowering takes exactly one argument",
                    ));
                    let fallback = if direction == WireCodecDirection::Decode {
                        value_ty.clone()
                    } else {
                        result_self_str()
                    };
                    return (
                        HirExprKind::Unsupported(
                            "wire deserialize method has invalid arity".into(),
                        ),
                        fallback,
                    );
                }
                let operand = self.lower_expr(args[0].expr(), IntentKind::Read);
                let result = if direction == WireCodecDirection::Decode {
                    value_ty.clone()
                } else {
                    result_self_str()
                };
                (operand, result)
            }
        };
        (
            HirExprKind::WireCodec {
                direction,
                operand: Box::new(operand),
                value_ty,
            },
            result_ty,
        )
    }

    pub(super) fn lower_generic_wire_codec(
        &mut self,
        args: &[hew_parser::ast::CallArg],
        direction: WireCodecDirection,
        value_ty: ResolvedTy,
        span: Span,
    ) -> (HirExprKind, ResolvedTy) {
        if args.len() != 1 {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: "generic wire codec function".to_string(),
                    reason: format!("expected exactly one argument, found {}", args.len()),
                },
                span.clone(),
                "generic wire codec lowering takes exactly one argument",
            ));
            return (
                HirExprKind::Unsupported(
                    "generic wire codec function has invalid arity".to_string(),
                ),
                ResolvedTy::Unit,
            );
        }
        if direction.is_serialize() {
            self.lower_wire_codec(args[0].expr(), &[], direction, value_ty, span)
        } else {
            self.lower_wire_codec(args[0].expr(), args, direction, value_ty, span)
        }
    }
}
