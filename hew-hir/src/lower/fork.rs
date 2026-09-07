use std::collections::HashMap;

use hew_parser::ast::{Expr, Span, Spanned};
use hew_types::{CallTarget, MethodCallReceiverKind, MethodCallRewrite, ResolvedTy, SpanKey};

use super::LowerCtx;
use crate::{
    HirBinding, HirBlock, HirClosureCapture, HirDiagnostic, HirDiagnosticKind, HirExpr,
    HirExprKind, HirStmt, HirStmtKind, IntentKind,
};

/// Values evaluated before starting a child. The root key can identify a
/// callable record field; it replaces the callee, never the invocation itself.
#[derive(Debug)]
pub(super) struct ForkCallInputs {
    root: SpanKey,
    values: HashMap<SpanKey, HirBinding>,
}

impl LowerCtx {
    /// Join shares the structured task batch and its checked input captures.
    /// Preparation stays in the parent; the full batch starts before any wait.
    pub(super) fn lower_join(
        &mut self,
        branches: &[Spanned<Expr>],
        span: Span,
    ) -> (HirExprKind, ResolvedTy) {
        if branches.is_empty() {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::JoinNoBranches,
                span,
                "join expression contains no branches",
            ));
            return (
                HirExprKind::Unsupported("empty join".into()),
                ResolvedTy::Unit,
            );
        }
        let Some(output_ty) = self.checker_expr_ty_if_present(&span) else {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: "join".into(),
                    reason: "missing checked result type".into(),
                },
                span,
                "join lowering requires the checked source result type",
            ));
            return (
                HirExprKind::Unsupported("untyped join".into()),
                ResolvedTy::Unit,
            );
        };
        let calls: Vec<_> = branches
            .iter()
            .map(|branch| match &branch.0 {
                Expr::Await(inner) => inner.as_ref().clone(),
                _ => branch.clone(),
            })
            .collect();
        let child = if calls.len() == 1 {
            self.lower_fork_invocation(&calls[0])
        } else {
            self.lower_fork_batch(&calls, output_ty.clone(), span)
        };
        (
            HirExprKind::AwaitTask {
                operand: Box::new(child),
                output_ty: output_ty.clone(),
            },
            output_ty,
        )
    }

    pub(super) fn fork_input(&mut self, span: &Span, intent: IntentKind) -> Option<HirExpr> {
        let key = self.mk_key(span);
        let inputs = self.fork_call_inputs.as_ref()?;
        if inputs.root == key {
            return None;
        }
        let binding = inputs.values.get(&key)?.clone();
        Some(self.fork_binding_ref(&binding, intent))
    }

    pub(super) fn fork_field_callee(&mut self, span: &Span) -> Option<HirExpr> {
        let key = self.mk_key(span);
        let inputs = self.fork_call_inputs.as_ref()?;
        if inputs.root != key {
            return None;
        }
        let binding = inputs.values.get(&key)?.clone();
        Some(self.fork_binding_ref(&binding, IntentKind::Read))
    }

    pub(super) fn fork_binding_ref(&mut self, binding: &HirBinding, intent: IntentKind) -> HirExpr {
        self.make_binding_ref(
            binding.name.clone(),
            binding.id,
            binding.ty.clone(),
            intent,
            binding.span.clone(),
        )
    }

    pub(super) fn fork_temporary(
        &mut self,
        value: HirExpr,
        consume: bool,
        statements: &mut Vec<HirStmt>,
    ) -> HirBinding {
        let id = self.ids.binding();
        let binding = HirBinding {
            id,
            name: format!("$fork_{}", id.0),
            ty: value.ty.clone(),
            mutable: true,
            span: value.span.clone(),
            is_consume: consume,
        };
        statements.push(HirStmt {
            node: self.ids.node(),
            span: value.span.clone(),
            kind: HirStmtKind::Let(binding.clone(), Some(value)),
        });
        binding
    }

    fn capture_fork_input(
        &mut self,
        key: SpanKey,
        value: HirExpr,
        inputs: &mut ForkCallInputs,
        statements: &mut Vec<HirStmt>,
        captures: &mut Vec<HirClosureCapture>,
    ) {
        let Some(fact) = self.checked_fork_transfers.get(&key).copied() else {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: "fork input".to_string(),
                    reason: "missing checked owning transfer".to_string(),
                },
                value.span.clone(),
                "a fork input requires checker-owned transfer facts",
            ));
            return;
        };
        let binding = self.fork_temporary(
            value,
            fact.acquisition == hew_types::ClosureCaptureAcquisition::Move,
            statements,
        );
        captures.push(HirClosureCapture {
            binding: binding.id,
            name: binding.name.clone(),
            ty: binding.ty.clone(),
            // These are fresh owning compiler temporaries. Their sole owner
            // moves into the child; source copies happened at initialization.
            acquisition: hew_types::ClosureCaptureAcquisition::Move,
            access: hew_types::ClosureCaptureAccess::Var,
            consumption: hew_types::ClosureCaptureConsumption::Consumed,
            is_send: fact.is_send,
            is_sync: fact.is_sync,
        });
        inputs.values.insert(key, binding);
    }

    pub(super) fn prepare_fork_call(
        &mut self,
        source: &Spanned<Expr>,
        statements: &mut Vec<HirStmt>,
    ) -> (HirExpr, Vec<HirClosureCapture>) {
        let key = self.mk_key(&source.1);
        let mut inputs = ForkCallInputs {
            root: key.clone(),
            values: HashMap::new(),
        };
        let mut captures = Vec::new();
        let args = match &source.0 {
            Expr::Call { function, args, .. } => {
                if matches!(
                    self.ordinary_call_target(&source.1),
                    Some(CallTarget::IndirectFunctionValue)
                ) {
                    let value = self.lower_expr(function, IntentKind::Read);
                    self.capture_fork_input(
                        self.mk_key(&function.1),
                        value,
                        &mut inputs,
                        statements,
                        &mut captures,
                    );
                }
                args
            }
            Expr::MethodCall {
                receiver,
                method,
                args,
            } => {
                if let Some(MethodCallRewrite::RecordFnFieldCall { field_ty }) =
                    self.method_call_rewrites.get(&key).cloned()
                {
                    let object = self.lower_expr(receiver, IntentKind::Read);
                    let value = self.make_expr(
                        HirExprKind::FieldAccess {
                            object: Box::new(object),
                            field: method.clone(),
                        },
                        field_ty,
                        IntentKind::Read,
                        source.1.clone(),
                    );
                    self.capture_fork_input(key, value, &mut inputs, statements, &mut captures);
                } else if !matches!(
                    self.method_call_receiver_kinds.get(&key),
                    Some(
                        MethodCallReceiverKind::ModuleBinding { .. }
                            | MethodCallReceiverKind::EnumConstructorPath { .. }
                    )
                ) && self.expr_types.contains_key(&self.mk_key(&receiver.1))
                {
                    let value = self.lower_expr(receiver, IntentKind::Read);
                    self.capture_fork_input(
                        self.mk_key(&receiver.1),
                        value,
                        &mut inputs,
                        statements,
                        &mut captures,
                    );
                }
                args
            }
            _ => {
                return (
                    self.unsupported_expr(source.1.clone(), "fork requires a checked invocation"),
                    captures,
                )
            }
        };
        // Source order is preserved even when named arguments are later placed
        // into the callee's parameter order.
        for arg in args {
            let arg = arg.expr();
            let value = self.lower_expr(arg, IntentKind::Read);
            self.capture_fork_input(
                self.mk_key(&arg.1),
                value,
                &mut inputs,
                statements,
                &mut captures,
            );
        }
        let saved = self.fork_call_inputs.replace(inputs);
        let call = self.lower_expr(source, IntentKind::Consume);
        self.fork_call_inputs = saved;
        (call, captures)
    }

    pub(super) fn fork_body(
        &mut self,
        body: HirBlock,
        captures: Vec<HirClosureCapture>,
    ) -> HirExpr {
        let task_ty = ResolvedTy::Task(Box::new(body.ty.clone()));
        let span = body.span.clone();
        self.make_expr(
            HirExprKind::ForkBlock {
                body,
                captures,
                task_ty: task_ty.clone(),
            },
            task_ty,
            IntentKind::Consume,
            span,
        )
    }

    pub(super) fn fork_result_block(
        &mut self,
        statements: Vec<HirStmt>,
        tail: HirExpr,
        span: Span,
    ) -> HirBlock {
        HirBlock {
            node: self.ids.node(),
            scope: self.ids.scope(),
            ty: tail.ty.clone(),
            statements,
            tail: Some(Box::new(tail)),
            span,
        }
    }

    pub(super) fn lower_fork_invocation(&mut self, source: &Spanned<Expr>) -> HirExpr {
        let mut statements = Vec::new();
        let (call, captures) = self.prepare_fork_call(source, &mut statements);
        let body = self.fork_result_block(Vec::new(), call, source.1.clone());
        let child = self.fork_body(body, captures);
        let block = self.fork_result_block(statements, child, source.1.clone());
        let ty = block.ty.clone();
        self.make_expr(
            HirExprKind::Block(block),
            ty,
            IntentKind::Consume,
            source.1.clone(),
        )
    }

    pub(super) fn lower_fork_batch(
        &mut self,
        sources: &[Spanned<Expr>],
        output_ty: ResolvedTy,
        span: Span,
    ) -> HirExpr {
        let mut parent_statements = Vec::new();
        let mut batch_captures = Vec::new();
        let mut batch_statements = Vec::new();
        let mut results = Vec::new();
        for source in sources {
            let (call, captures) = self.prepare_fork_call(source, &mut parent_statements);
            batch_captures.extend(captures.iter().cloned());
            let output_ty = call.ty.clone();
            let body = self.fork_result_block(Vec::new(), call, source.1.clone());
            let child = self.fork_body(body, captures);
            let binding = self.fork_temporary(child, true, &mut batch_statements);
            let operand = self.fork_binding_ref(&binding, IntentKind::Consume);
            results.push(self.make_expr(
                HirExprKind::AwaitTask {
                    operand: Box::new(operand),
                    output_ty: output_ty.clone(),
                },
                output_ty,
                IntentKind::Read,
                source.1.clone(),
            ));
        }
        // The aggregate task owns these children from creation. No task handle
        // crosses a scope boundary, and every child starts before the first wait.
        let result = if matches!(output_ty, ResolvedTy::Tuple(_)) {
            self.make_expr(
                HirExprKind::TupleLiteral { elements: results },
                output_ty,
                IntentKind::Consume,
                span.clone(),
            )
        } else {
            let empty = self.make_vec_new_expr(output_ty.clone(), span.clone());
            let vector = self.fork_temporary(empty, true, &mut batch_statements);
            for result in results {
                let receiver = self.fork_binding_ref(&vector, IntentKind::Read);
                let push = self.make_vec_push_expr(receiver, result, span.clone());
                batch_statements.push(HirStmt {
                    node: self.ids.node(),
                    kind: HirStmtKind::Expr(push),
                    span: span.clone(),
                });
            }
            self.fork_binding_ref(&vector, IntentKind::Consume)
        };
        let batch_body = self.fork_result_block(batch_statements, result, span.clone());
        let batch = self.fork_body(batch_body, batch_captures);
        let block = self.fork_result_block(parent_statements, batch, span.clone());
        let ty = block.ty.clone();
        self.make_expr(HirExprKind::Block(block), ty, IntentKind::Consume, span)
    }
}
