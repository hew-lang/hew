use super::types::RaceOperandKind;
use super::{Checker, Ty, TypeErrorKind};
use crate::SpanKey;
use hew_parser::ast::{Expr, Span, Spanned};

impl Checker {
    pub(super) fn synthesize_race(&mut self, branches: &[Spanned<Expr>], span: &Span) -> Ty {
        if branches.is_empty() {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                "race requires at least one call or task".into(),
            );
            return Ty::Error;
        }
        let mut output = Ty::Never;
        let mut kinds = Vec::new();
        for branch in branches {
            let invocation = matches!(branch.0, Expr::Call { .. } | Expr::MethodCall { .. });
            if invocation {
                self.suspension_operands
                    .insert(SpanKey::in_module(&branch.1, self.current_module_idx));
            }
            let result = self.synthesize(&branch.0, &branch.1);
            let result = self.subst.resolve(&result);
            let (kind, result) = if let Ty::Task(result) = result {
                self.mark_expr_moved(&branch.0, &branch.1);
                (RaceOperandKind::Task, *result)
            } else if invocation {
                self.record_fork_call_inputs(branch);
                (RaceOperandKind::Invocation, result)
            } else {
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    &branch.1,
                    "race expects calls or tasks with one common result type".into(),
                );
                continue;
            };
            kinds.push(kind);
            if matches!(result, Ty::Error | Ty::Never) {
                continue;
            }
            if output == Ty::Never {
                output = result;
            } else {
                self.expect_type(&output, &result, &branch.1);
            }
        }
        self.race_operands
            .insert(SpanKey::in_module(span, self.current_module_idx), kinds);
        Ty::Task(Box::new(output))
    }
}
