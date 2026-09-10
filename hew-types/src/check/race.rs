//! Race checks each invocation once and shares fork's owning input boundary.

use hew_parser::ast::{Expr, Span, Spanned};

use super::{Checker, Ty, TypeErrorKind};
use crate::SpanKey;

impl Checker {
    pub(super) fn synthesize_race(&mut self, branches: &[Spanned<Expr>], span: &Span) -> Ty {
        if branches.is_empty() {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                "race requires at least one call".into(),
            );
            return Ty::Error;
        }
        let mut output = Ty::Never;
        for branch in branches {
            // `await` is never written on a race operand: the `race` is what
            // waits (spec 4.11.2), mirroring `select`'s own arm-source rule.
            // Keep synthesizing the inner operand so the branch's other
            // diagnostics still report, but refuse the spelling.
            let call = match &branch.0 {
                Expr::Await(inner) => {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        &branch.1,
                        "a race operand never writes `await` - the `race` is \
                         what waits; delete `await`"
                            .to_string(),
                    );
                    inner.as_ref()
                }
                _ => branch,
            };
            if !matches!(call.0, Expr::Call { .. } | Expr::MethodCall { .. }) {
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    &call.1,
                    "race expects calls whose results have one common type".into(),
                );
                continue;
            }
            self.suspension_operands
                .insert(SpanKey::in_module(&call.1, self.current_module_idx));
            let result = self.synthesize(&call.0, &call.1);
            let result = self.subst.resolve(&result);
            if result == Ty::Error {
                continue;
            }
            self.record_fork_call_inputs(call);
            if result == Ty::Never {
                continue;
            }
            if output == Ty::Never {
                output = result;
            } else {
                self.expect_type(&output, &result, &call.1);
            }
        }
        output
    }
}
