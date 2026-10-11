use super::LowerCtx;
use crate::{HirExprKind, IntentKind};
use hew_parser::ast::{Expr, Span, Spanned};
use hew_types::check::RaceOperandKind;
use hew_types::ResolvedTy;

impl LowerCtx {
    pub(super) fn lower_race(
        &mut self,
        branches: &[Spanned<Expr>],
        span: Span,
    ) -> (HirExprKind, ResolvedTy) {
        if !self.task_result_lifetimes.contains(&self.mk_key(&span)) {
            let expression = self.unsupported_expr(span, "race lacks its checked result lifetime");
            return (expression.kind, expression.ty);
        }
        let result_lifetime = crate::HirTaskScopeResult::checked();
        let Some(output) = self.checker_expr_ty_if_present(&span) else {
            let expression = self.unsupported_expr(span, "race requires its checked result type");
            return (expression.kind, expression.ty);
        };
        let Some(kinds) = self
            .race_operands
            .get(&self.mk_key(&span))
            .cloned()
            .filter(|kinds| kinds.len() == branches.len())
        else {
            let expression = self.unsupported_expr(span, "race requires checked operand kinds");
            return (expression.kind, expression.ty);
        };
        let mut statements = Vec::new();
        let mut prepared = Vec::new();
        for (branch, kind) in branches.iter().zip(kinds) {
            match kind {
                RaceOperandKind::Invocation => {
                    let (call, captures) = self.prepare_fork_call(branch, &mut statements);
                    prepared.push((call, Some(captures)));
                }
                RaceOperandKind::Task => {
                    let task = self.lower_expr(branch, IntentKind::Consume);
                    let binding = self.fork_temporary(task, true, &mut statements);
                    prepared.push((self.fork_binding_ref(&binding, IntentKind::Consume), None));
                }
            }
        }
        let mut members = Vec::new();
        for (value, captures) in prepared {
            if let Some(captures) = captures {
                let body = self.fork_result_block(Vec::new(), value, span.clone());
                let child = self.fork_body(body, captures, result_lifetime);
                let task = self.fork_temporary(child, true, &mut statements);
                members.push(self.fork_binding_ref(&task, IntentKind::Consume));
            } else {
                members.push(value);
            }
        }
        let group = self.make_expr(
            HirExprKind::TaskRace {
                members,
                result_lifetime,
            },
            output.clone(),
            IntentKind::Consume,
            span.clone(),
        );
        (
            HirExprKind::Block(self.fork_result_block(statements, group, span)),
            output,
        )
    }
}
