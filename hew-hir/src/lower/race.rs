//! A race prepares fork inputs in source order, then owns all spawned children.

use hew_parser::ast::{Expr, Span, Spanned};
use hew_types::ResolvedTy;

use super::LowerCtx;
use crate::{
    HirBinding, HirExprKind, HirSelect, HirSelectArm, HirSelectArmKind, HirSelectionOrder,
    IntentKind,
};

impl LowerCtx {
    pub(super) fn lower_race(
        &mut self,
        branches: &[Spanned<Expr>],
        span: Span,
    ) -> (HirExprKind, ResolvedTy) {
        let Some(output) = self.checker_expr_ty_if_present(&span) else {
            let expression = self.unsupported_expr(span, "race requires its checked result type");
            return (expression.kind, expression.ty);
        };
        let mut statements = Vec::new();
        let prepared = branches
            .iter()
            .map(|branch| {
                let call = match &branch.0 {
                    Expr::Await(inner) => inner.as_ref(),
                    _ => branch,
                };
                self.prepare_fork_call(call, &mut statements)
            })
            .collect::<Vec<_>>();
        let mut arms = Vec::new();
        for (call, captures) in prepared {
            let result_ty = call.ty.clone();
            let child_span = call.span.clone();
            let body = self.fork_result_block(Vec::new(), call, child_span.clone());
            let child = self.fork_body(body, captures);
            let task = self.fork_temporary(child, true, &mut statements);
            let id = self.ids.binding();
            let result = HirBinding {
                id,
                name: format!("$race_result_{}", id.0),
                ty: result_ty,
                mutable: false,
                span: child_span,
                is_consume: false,
            };
            arms.push(HirSelectArm {
                scope: Some(self.ids.scope()),
                kind: HirSelectArmKind::TaskAwait {
                    task: Box::new(self.fork_binding_ref(&task, IntentKind::Consume)),
                },
                binding_name: Some(result.name.clone()),
                binding_id: Some(result.id),
                body: self.fork_binding_ref(&result, IntentKind::Consume),
            });
        }
        let selected = self.make_expr(
            HirExprKind::Select(HirSelect {
                order: HirSelectionOrder::Completion,
                arms,
            }),
            output.clone(),
            IntentKind::Consume,
            span.clone(),
        );
        let body = self.fork_result_block(statements, selected, span);
        (HirExprKind::Race { body }, output)
    }
}
