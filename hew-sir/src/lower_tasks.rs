//! Source task lifetimes and result transfer through explicit suspension edges.

use super::{
    BlockArg, Builder, Edge, HirBlock, HirExpr, HirExprKind, Operand, OwnKind, OwnedBindingUse,
    Provenance, ResolvedTy, SemFunction, SemOpKind, SemTerminator, ValueDef, ValueId,
};
use crate::{BoundaryDecision, BoundaryOperand, CallResult, SuspendKind, TaskScopeId};

fn edge(target: crate::BlockId) -> Edge {
    Edge {
        target,
        args: Vec::new(),
    }
}

fn contains_task(ty: &ResolvedTy) -> bool {
    let mut pending = vec![ty.clone()];
    let mut seen = std::collections::BTreeSet::new();
    while let Some(ty) = pending.pop() {
        if !seen.insert(ty.clone()) {
            continue;
        }
        if matches!(ty, ResolvedTy::Task(_)) {
            return true;
        }
        hew_types::push_type_components(&ty, &mut pending);
    }
    false
}

pub(super) fn remove_empty_scopes(function: &mut SemFunction) {
    if function
        .blocks
        .iter()
        .any(|block| matches!(block.terminator, SemTerminator::RecoverFault { .. }))
    {
        return;
    }
    if function
        .blocks
        .iter()
        .flat_map(|block| &block.ops)
        .any(|op| {
            matches!(
                op.kind,
                SemOpKind::TaskSpawn { .. }
                    | SemOpKind::TaskScopeEnter {
                        duration: Some(_),
                        ..
                    }
            )
        })
    {
        return;
    }
    // Every callable has an implicit lexical lifetime. Without a child, its
    // drain is an identity operation and needs neither runtime storage nor a
    // resumable ABI. This is determined from lowered operations, not HIR shape.
    for block in &mut function.blocks {
        block.ops.retain(|op| {
            !matches!(
                op.kind,
                SemOpKind::TaskScopeEnter { .. } | SemOpKind::TaskScopeClose { .. }
            )
        });
        if let SemTerminator::Suspend {
            kind: SuspendKind::Join { .. },
            resumes,
            ..
        } = &block.terminator
        {
            block.terminator = SemTerminator::Goto(resumes[0].clone());
        }
    }
}

impl Builder<'_, '_> {
    pub(super) fn enter_task_scope(&mut self) -> Result<TaskScopeId, String> {
        self.enter_task_scope_with_deadline(None)
    }

    fn enter_task_scope_with_deadline(
        &mut self,
        duration: Option<Operand>,
    ) -> Result<TaskScopeId, String> {
        let scope = TaskScopeId(self.ops);
        self.emit_place_operation(
            SemOpKind::TaskScopeEnter {
                scope,
                parent: self.task_scopes.last().map(|(scope, _)| *scope),
                duration,
            },
            Provenance::Synthesized,
        )?;
        self.task_scopes.push((scope, self.scopes.len() - 1));
        Ok(scope)
    }

    pub(super) fn finish_task_scopes(&mut self, floor: usize, cancel: bool) -> Result<(), String> {
        while self
            .task_scopes
            .last()
            .is_some_and(|(_, depth)| *depth >= floor)
        {
            let (scope, _) = self.task_scopes.pop().expect("active task scope");
            let live = self.owned_live.clone();
            let normal = self.new_block(Vec::new());
            let fault = if cancel {
                normal
            } else {
                self.new_block(Vec::new())
            };
            self.set_terminator(SemTerminator::Suspend {
                kind: SuspendKind::Join { scope, cancel },
                inputs: Vec::new(),
                result: CallResult::Unit,
                resumes: vec![edge(normal)],
                cancel: edge(fault),
                unwind: edge(fault),
            })?;
            if !cancel {
                self.current = fault;
                self.emit_place_operation(
                    SemOpKind::TaskScopeClose { scope },
                    Provenance::Synthesized,
                )?;
                self.finish_fault_exit()?;
            }
            self.current = normal;
            self.owned_live = live;
            self.emit_place_operation(
                SemOpKind::TaskScopeClose { scope },
                Provenance::Synthesized,
            )?;
        }
        Ok(())
    }

    pub(super) fn lower_task_scope(&mut self, body: &HirBlock) -> Result<Option<ValueId>, String> {
        self.lower_task_scope_with_deadline(body, None)
    }

    pub(super) fn lower_task_scope_with_deadline(
        &mut self,
        body: &HirBlock,
        duration: Option<&HirExpr>,
    ) -> Result<Option<ValueId>, String> {
        // Deferred bodies cannot create children or suspend. A plain scope
        // there only supplies lexical cleanup; it needs no asynchronous drain.
        if duration.is_none() && self.in_deferred_body() {
            return self
                .lower_block(body, OwnedBindingUse::Return)
                .map(|result| result.map(|result| result.value));
        }
        let duration = duration
            .map(|duration| self.lower_expr(duration).map(|value| Operand { value }))
            .transpose()?;
        let floor = self.scopes.len();
        self.scopes.push(Vec::new());
        self.enter_task_scope_with_deadline(duration)?;
        let result = self.lower_block(body, OwnedBindingUse::Return)?;
        if self.is_open() {
            if result
                .as_ref()
                .and_then(|value| self.value_ty(value.value))
                .is_some_and(|ty| contains_task(&ty))
            {
                return Err("a scope result cannot retain a scoped task handle".into());
            }
            self.finish_task_scopes(floor, false)?;
            self.end_scopes(floor)?;
        }
        self.leave_scope();
        Ok(result.map(|result| result.value))
    }

    pub(super) fn lower_fork_block(
        &mut self,
        expression: &HirExpr,
        body: &HirBlock,
        captures: &[hew_hir::HirClosureCapture],
    ) -> Result<ValueId, String> {
        let ResolvedTy::Task(output) = self.ty(&expression.ty) else {
            return Err("fork body must produce a checked Task".into());
        };
        if contains_task(&output) {
            return Err("a child result cannot transfer a nested scoped task handle".into());
        }
        let scope = self
            .task_scopes
            .last()
            .ok_or("fork has no lexical task scope")?
            .0;
        let mut closure = expression.clone();
        closure.ty = ResolvedTy::Closure {
            capabilities: hew_types::CallableCapabilities {
                call: hew_types::CallableCallMode::Once,
                clone: false,
            },
            params: Vec::new(),
            ret: output.clone(),
            captures: captures
                .iter()
                .map(|capture| self.ty(&capture.ty))
                .collect(),
        };
        let mut child_body = expression.clone();
        child_body.ty = *output.clone();
        child_body.kind = HirExprKind::Block(body.clone());
        closure.kind = HirExprKind::Closure {
            params: Vec::new(),
            ret_ty: *output,
            body: Box::new(child_body),
            captures: captures.to_vec(),
            escape_kind: hew_types::ClosureEscapeKind::Forked,
        };
        let callable = self.lower_closure(&closure)?;
        self.owned_live.remove(&callable);
        self.emit(
            expression,
            SemOpKind::TaskSpawn {
                scope,
                callable: Operand { value: callable },
            },
        )
    }

    pub(super) fn lower_task_await(
        &mut self,
        expression: &HirExpr,
        operand: &HirExpr,
    ) -> Result<Option<ValueId>, String> {
        let output = self.ty(&expression.ty);
        if self.ty(&operand.ty) != ResolvedTy::Task(Box::new(output.clone())) {
            return Err("await result differs from its checked task output".into());
        }
        let task = self.lower_consuming_value(operand)?;
        self.owned_live.remove(&task);
        let live = self.owned_live.clone();
        let (result, normal, continuation) = if output == ResolvedTy::Never {
            (CallResult::Never, edge(self.new_block(Vec::new())), None)
        } else if output == ResolvedTy::Unit {
            (CallResult::Unit, edge(self.new_block(Vec::new())), None)
        } else {
            self.service.require_type_facts(&output)?;
            let own = OwnKind::of_ty(&output, self.service.checked_facts.rows())?;
            let raw = self.fresh_value();
            let value = self.fresh_value();
            let target = self.new_block(vec![BlockArg {
                value,
                ty: output.clone(),
                own,
            }]);
            (
                CallResult::Value(ValueDef {
                    id: raw,
                    ty: output.clone(),
                    own,
                }),
                Edge {
                    target,
                    args: vec![Operand { value: raw }],
                },
                Some((value, own)),
            )
        };
        let resumed = normal.target;
        let cancel = self.new_block(Vec::new());
        let unwind = self.new_block(Vec::new());
        self.set_terminator(SemTerminator::Suspend {
            kind: SuspendKind::Await,
            inputs: vec![BoundaryOperand {
                operand: Operand { value: task },
                decision: BoundaryDecision::Move,
            }],
            result,
            resumes: if output == ResolvedTy::Never {
                Vec::new()
            } else {
                vec![normal]
            },
            cancel: edge(cancel),
            unwind: edge(unwind),
        })?;
        for cleanup in [cancel, unwind] {
            self.current = cleanup;
            self.owned_live = live.clone();
            self.finish_fault_exit()?;
        }
        self.current = resumed;
        self.owned_live = live;
        if output == ResolvedTy::Never {
            self.set_terminator(SemTerminator::Unreachable)?;
        }
        if let Some((value, OwnKind::Owned)) = continuation {
            self.owned_live.insert(value, output);
        }
        Ok(continuation.map(|(value, _)| value))
    }
}
