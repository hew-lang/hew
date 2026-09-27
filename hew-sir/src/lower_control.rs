//! Control flow, short-circuit operators and low-level block helpers.

use super::{
    require_type_facts, BlockArg, BlockId, Builder, CheckedFailure, Edge, HirExpr, MatchExit, OpId,
    Operand, OwnKind, PendingBlock, Provenance, ResolvedTy, SemOp, SemOpKind, SemTerminator,
    ValueDef, ValueId,
};

impl Builder<'_, '_> {
    pub(super) fn lower_unit_if(
        &mut self,
        condition: &HirExpr,
        then_expr: &HirExpr,
        else_expr: Option<&HirExpr>,
    ) -> Result<(), String> {
        let condition = self.lower_read_operand(condition, "if condition")?;
        let then_block = self.new_block(Vec::new());
        let else_block = self.new_block(Vec::new());
        self.set_terminator(SemTerminator::Branch {
            condition,
            then_target: Edge {
                target: then_block,
                args: Vec::new(),
            },
            else_target: Edge {
                target: else_block,
                args: Vec::new(),
            },
        })?;

        let before = self.control_state();
        // Both bodies are conditional paths: a loan may not end inside one.
        self.branch_depth += 1;
        self.current = then_block;
        self.lower_discarded_expr(then_expr)?;
        let then_state = self.is_open().then(|| self.control_state());

        self.restore_control_state(&before);
        self.current = else_block;
        if let Some(else_expr) = else_expr {
            self.lower_discarded_expr(else_expr)?;
        }
        let else_state = self.is_open().then(|| self.control_state());
        self.branch_depth -= 1;

        match (then_state, else_state) {
            (Some(then_state), Some(else_state)) => {
                self.merge_control_states(vec![then_state, else_state])
            }
            (Some(state), None) | (None, Some(state)) => {
                self.restore_control_state(&state);
                Ok(())
            }
            (None, None) => {
                self.current = then_block;
                Ok(())
            }
        }
    }

    pub(super) fn lower_if(
        &mut self,
        whole: &HirExpr,
        condition: &HirExpr,
        then_expr: &HirExpr,
        else_expr: &HirExpr,
    ) -> Result<ValueId, String> {
        let condition = self.lower_read_operand(condition, "if condition")?;
        let then_block = self.new_block(Vec::new());
        let else_block = self.new_block(Vec::new());
        let join_ty = self.ty(&whole.ty);
        self.set_terminator(SemTerminator::Branch {
            condition,
            then_target: Edge {
                target: then_block,
                args: Vec::new(),
            },
            else_target: Edge {
                target: else_block,
                args: Vec::new(),
            },
        })?;
        let before = self.control_state();
        let mut exits = Vec::new();
        self.branch_depth += 1;
        for (block, expression) in [(then_block, then_expr), (else_block, else_expr)] {
            self.restore_control_state(&before);
            self.current = block;
            let result = self.lower_selected_body(expression, &join_ty)?;
            if !self.is_open() {
                continue;
            }
            let value = result
                .ok_or("non-divergent if branch does not produce its result")?
                .value;
            self.owned_live.remove(&value);
            exits.push(MatchExit {
                state: self.control_state(),
                result: Some(Operand { value }),
            });
        }
        self.branch_depth -= 1;
        self.merge_match_exits(exits, &join_ty)?
            .ok_or_else(|| "divergent if expression cannot produce an SSA value".to_string())
    }

    /// Lower short-circuit `&&` as CFG rather than an eager binary operation.
    ///
    /// The false edge materialises the result while the true edge alone
    /// evaluates the right-hand side. This keeps effectful future SIR
    /// operations on the RHS structurally guarded from the outset.
    pub(super) fn lower_logical_and(
        &mut self,
        whole: &HirExpr,
        left: &HirExpr,
        right: &HirExpr,
    ) -> Result<ValueId, String> {
        self.lower_short_circuit(whole, left, right, false)
    }

    /// Lower short-circuit `||` as CFG rather than an eager binary operation.
    pub(super) fn lower_logical_or(
        &mut self,
        whole: &HirExpr,
        left: &HirExpr,
        right: &HirExpr,
    ) -> Result<ValueId, String> {
        self.lower_short_circuit(whole, left, right, true)
    }

    pub(super) fn lower_short_circuit(
        &mut self,
        whole: &HirExpr,
        left: &HirExpr,
        right: &HirExpr,
        short_circuit_value: bool,
    ) -> Result<ValueId, String> {
        let result_ty = self.ty(&whole.ty);
        if result_ty != ResolvedTy::Bool {
            return Err("short-circuit logical expressions must have bool type in SIR".to_string());
        }
        let condition = self.lower_read_operand(left, "logical condition")?;
        let evaluate_right = self.new_block(Vec::new());
        let short_circuit = self.new_block(Vec::new());
        let (then_target, else_target) = if short_circuit_value {
            (short_circuit, evaluate_right)
        } else {
            (evaluate_right, short_circuit)
        };
        self.set_terminator(SemTerminator::Branch {
            condition,
            then_target: Edge {
                target: then_target,
                args: Vec::new(),
            },
            else_target: Edge {
                target: else_target,
                args: Vec::new(),
            },
        })?;

        let before = self.control_state();
        let mut exits = Vec::new();
        // Both operands are conditional paths: a loan may not end inside one.
        self.branch_depth += 1;

        self.current = evaluate_right;
        let loan_floor = self.scope_loans.len();
        let right_value = self.lower_read_operand(right, "logical right value")?;
        if self.is_open() {
            // The right operand runs on one edge only. Its temporaries and
            // interior loans end on that edge, never after the join, where the
            // short-circuit edge never created them.
            if self.scope_loans.len() > loan_floor {
                let loans = self.scope_loans.split_off(loan_floor);
                self.end_call_loans(&loans)?;
            }
            self.destroy_live_since(&before.owned_live)?;
            exits.push(MatchExit {
                state: self.control_state(),
                result: Some(right_value),
            });
        }

        self.restore_control_state(&before);
        self.current = short_circuit;
        let constant = self.emit(whole, SemOpKind::ConstBool(short_circuit_value))?;
        exits.push(MatchExit {
            state: self.control_state(),
            result: Some(Operand { value: constant }),
        });

        self.branch_depth -= 1;
        self.merge_match_exits(exits, &result_ty)?
            .ok_or_else(|| "short-circuit logical expression produced no SSA value".to_string())
    }

    pub(super) fn emit(&mut self, expr: &HirExpr, kind: SemOpKind) -> Result<ValueId, String> {
        self.emit_typed(Provenance::Site(expr.site), &self.ty(&expr.ty), kind)
    }

    pub(super) fn emit_typed(
        &mut self,
        provenance: Provenance,
        result_ty: &ResolvedTy,
        kind: SemOpKind,
    ) -> Result<ValueId, String> {
        if let SemOpKind::LoadTake { place } = &kind {
            // Only a local or projected owner can carry a binding's loan; a
            // capture or state place has no path here and none to end.
            if let Ok((root, _)) = crate::projection::place_path(&self.places, *place) {
                self.end_binding_loans_on(root)?;
            }
        }
        let value = self.fresh_value();
        if matches!(kind, SemOpKind::ActorIngressAdapter(_)) {
            require_type_facts(&mut self.service.checked_facts, result_ty)?;
        } else {
            self.service.require_type_facts(result_ty)?;
        }
        let own = OwnKind::of_ty(result_ty, self.service.checked_facts.rows())?;
        let own = if let Some(parent) = kind.borrow_parent() {
            self.borrow_parents.insert(value, parent);
            OwnKind::Guaranteed
        } else {
            own
        };
        let op = SemOp {
            id: OpId(self.ops),
            results: vec![ValueDef {
                id: value,
                own,
                ty: result_ty.clone(),
            }],
            kind,
            provenance,
        };
        if op.results[0].own == OwnKind::Owned {
            self.owned_live.insert(value, result_ty.clone());
        }
        self.current_block_mut().append_op(op)?;
        self.ops += 1;
        Ok(value)
    }

    pub(super) fn lower_checked_binary(
        &mut self,
        expr: &HirExpr,
        op: hew_parser::ast::BinaryOp,
        lhs: Operand,
        rhs: Operand,
    ) -> Result<ValueId, String> {
        let result_ty = self.ty(&expr.ty);
        let required = crate::checked_binary_failure_kinds(op, &result_ty).ok_or_else(|| {
            format!(
                "`{op}` over `{}` is not a checked integer operation",
                result_ty.user_facing()
            )
        })?;
        self.service.require_type_facts(&result_ty)?;
        let own = OwnKind::of_ty(&result_ty, self.service.checked_facts.rows())?;
        let raw_result = self.fresh_value();
        let continuation = self.fresh_value();
        let normal = self.new_block(vec![BlockArg {
            value: continuation,
            ty: result_ty.clone(),
            own,
        }]);
        let failure_blocks: Vec<_> = required
            .iter()
            .map(|kind| (*kind, self.new_block(Vec::new())))
            .collect();
        let failures = failure_blocks
            .iter()
            .map(|(kind, block)| CheckedFailure {
                kind: *kind,
                edge: Edge {
                    target: *block,
                    args: Vec::new(),
                },
            })
            .collect();
        let live_at_operation = self.owned_live.clone();
        let id = OpId(self.ops);
        self.ops += 1;
        self.set_terminator(SemTerminator::CheckedBinary {
            id,
            op,
            lhs,
            rhs,
            result: ValueDef {
                id: raw_result,
                ty: result_ty,
                own,
            },
            normal: Edge {
                target: normal,
                args: vec![Operand { value: raw_result }],
            },
            failures,
        })?;
        for (kind, block) in failure_blocks {
            self.current = block;
            self.owned_live = live_at_operation.clone();
            self.finish_checked_fault(kind)?;
        }
        self.current = normal;
        self.owned_live = live_at_operation;
        if own == OwnKind::Owned {
            self.owned_live.insert(continuation, self.ty(&expr.ty));
        }
        Ok(continuation)
    }

    pub(super) fn fresh_value(&mut self) -> ValueId {
        let value = ValueId(self.values);
        self.values += 1;
        value
    }

    pub(super) fn ty(&self, ty: &ResolvedTy) -> ResolvedTy {
        self.substitution.apply(ty)
    }

    pub(super) fn new_block(&mut self, args: Vec<BlockArg>) -> BlockId {
        let id = BlockId(u32::try_from(self.blocks.len()).expect("SIR block count exceeds u32"));
        self.blocks.push(PendingBlock::new(id, args));
        id
    }
    pub(super) fn current_block(&self) -> &PendingBlock {
        &self.blocks[self.current.0 as usize]
    }
    pub(super) fn current_block_mut(&mut self) -> &mut PendingBlock {
        &mut self.blocks[self.current.0 as usize]
    }
    pub(super) fn is_open(&self) -> bool {
        self.current_block().is_open()
    }
    pub(super) fn set_terminator(&mut self, term: SemTerminator) -> Result<(), String> {
        // The last expression visited is the source point this terminator
        // belongs to: a `Return` is sealed after its operand is lowered, and a
        // call terminator after its arguments, so the most recent leaf is the
        // statement a debugger should stop on.
        let provenance = self
            .current_site
            .map_or(Provenance::Synthesized, Provenance::Site);
        let block = self.current_block_mut();
        block.terminator_provenance = provenance;
        if block.terminator.is_some() {
            return Err(format!(
                "SIR builder attempted to overwrite completed block bb{}",
                block.id.0
            ));
        }
        block.terminator = Some(term);
        Ok(())
    }
}
