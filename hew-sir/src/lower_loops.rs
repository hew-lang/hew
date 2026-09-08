//! Statement loops, labelled exits and integer ranges share lexical cleanup.
//! Source loops produce Unit or Never; the parser rejects break operands.

use super::{
    BindingId, BindingTarget, BlockArg, BlockId, Builder, ControlState, Edge, HirBinding, HirBlock,
    HirExpr, HirExprKind, HirLiteral, LoopScope, Operand, OwnedBindingUse, Provenance, ResolvedTy,
    SemOpKind, SemTerminator, ValueId,
};

impl Builder<'_, '_> {
    fn loop_edge(&mut self, scope: &LoopScope, target: BlockId) -> Result<Edge, String> {
        let saved = self.control_state();
        let recovery = self.recovery_bodies.clone();
        let preserved = self
            .owned_live
            .iter()
            .filter(|(value, _)| scope.preserved.contains(value))
            .map(|(value, ty)| (*value, ty.clone()))
            .collect();
        self.finish_recovery_scopes(scope.scope_floor, &preserved)?;
        self.finish_task_scopes(scope.scope_floor, false)?;
        let args = scope
            .carried
            .iter()
            .map(|&binding| self.scalar_binding(binding).map(|value| Operand { value }))
            .collect::<Result<Vec<_>, _>>()?;
        let dead = self
            .owned_live
            .keys()
            .filter(|value| !scope.preserved.contains(value))
            .copied()
            .collect::<Vec<_>>();
        for value in dead.into_iter().rev() {
            self.emit_destroy(value)?;
        }
        self.end_scopes(scope.scope_floor)?;
        let terminal = self.current;
        self.restore_control_state(&saved);
        self.recovery_bodies = recovery;
        self.current = terminal;
        Ok(Edge { target, args })
    }

    pub(super) fn lower_loop_exit(
        &mut self,
        continuing: bool,
        label: Option<&str>,
        value: Option<&HirExpr>,
    ) -> Result<(), String> {
        let index = self
            .loops
            .iter()
            .enumerate()
            .rev()
            .take_while(|(_, scope)| scope.is_some())
            .find(|(_, scope)| label.is_none() || scope.as_ref().unwrap().label.as_deref() == label)
            .map(|(index, _)| index)
            .ok_or_else(|| "break/continue has no enclosing loop target".to_string())?;
        self.check_deferred_loop_exit(index)?;
        // HIR can still contain an operand from a non-parser producer. Preserve
        // its effects and ordinary temporary cleanup without returning a value.
        if let Some(value) = value {
            self.lower_discarded_expr(value)?;
            if !self.is_open() {
                return Ok(());
            }
        }
        let scope = self.loops[index].clone().unwrap();
        let target = if continuing { scope.header } else { scope.exit };
        let edge = self.loop_edge(&scope, target)?;
        self.set_terminator(SemTerminator::Goto(edge))
    }

    /// Give each loop-carried binding a fresh SSA argument at a CFG join.
    fn loop_join(&mut self, carried: &[BindingId]) -> Result<ControlState, String> {
        let mut state = self.control_state();
        let mut args = Vec::new();
        for &binding in carried {
            let source = self.scalar_binding(binding)?;
            let ty = self
                .value_ty(source)
                .ok_or_else(|| "scalar loop binding has no type".to_string())?;
            let own = self
                .value_own_kind(source)
                .ok_or_else(|| "scalar loop binding has no ownership facts".to_string())?;
            let value = self.fresh_value();
            args.push(BlockArg { value, ty, own });
            state.bindings.insert(binding, BindingTarget::Value(value));
            self.record_binding_version(binding, value)?;
        }
        state.block = self.new_block(args);
        Ok(state)
    }

    pub(super) fn lower_while(
        &mut self,
        label: Option<&str>,
        condition: Option<&HirExpr>,
        body: &HirBlock,
    ) -> Result<(), String> {
        let carried = self.mutable_bindings();
        let header = self.loop_join(&carried)?;
        let exit = self.loop_join(&carried)?;
        let scope = LoopScope {
            label: label.map(str::to_owned),
            header: header.block,
            exit: exit.block,
            carried,
            preserved: self.owned_live.keys().copied().collect(),
            scope_floor: self.scopes.len(),
        };
        let entry = self.loop_edge(&scope, header.block)?;
        self.set_terminator(SemTerminator::Goto(entry))?;
        self.restore_control_state(&header);
        let body_block = self.new_block(Vec::new());
        if let Some(condition) = condition {
            let condition = self.lower_read_operand(condition, "while condition")?;
            let exit_edge = self.loop_edge(&scope, exit.block)?;
            self.set_terminator(SemTerminator::Branch {
                condition,
                then_target: Edge {
                    target: body_block,
                    args: Vec::new(),
                },
                else_target: exit_edge,
            })?;
        } else {
            self.set_terminator(SemTerminator::Goto(Edge {
                target: body_block,
                args: Vec::new(),
            }))?;
        }
        self.current = body_block;
        self.loops.push(Some(scope.clone()));
        let tail = self.lower_scoped_block(body, OwnedBindingUse::Copy)?;
        if let Some(tail) = tail {
            if self.owned_live.contains_key(&tail.value) {
                self.emit_destroy(tail.value)?;
            }
        }
        if self.is_open() {
            let edge = self.loop_edge(&scope, header.block)?;
            self.set_terminator(SemTerminator::Goto(edge))?;
        }
        self.loops.pop();
        let mut has_exit = false;
        for block in &self.blocks {
            if let Some(term) = &block.terminator {
                term.visit_successors(|edge| has_exit |= edge.target == exit.block);
            }
        }
        self.restore_control_state(&exit);
        if !has_exit {
            self.set_terminator(SemTerminator::Unreachable)?;
        }
        Ok(())
    }

    #[allow(
        clippy::too_many_arguments,
        clippy::too_many_lines,
        reason = "checked range facts share one setup, condition and increment CFG"
    )]
    pub(super) fn lower_for_range(
        &mut self,
        label: Option<&str>,
        loop_binding: &HirBinding,
        start: &HirExpr,
        end: &HirExpr,
        step: &HirExpr,
        inclusive: bool,
        descending: bool,
        body: &HirBlock,
    ) -> Result<(), String> {
        use hew_parser::ast::BinaryOp;
        let ty = self.ty(&loop_binding.ty);
        if !ty.is_integer() {
            return Err("range binding requires an integer type".into());
        }
        let initial = self.lower_range_operand(start, &ty)?;
        let bound = self.lower_range_operand(end, &ty)?;
        let stride = self.lower_range_operand(step, &ty)?;
        let zero = Operand {
            value: self.emit_typed(Provenance::Synthesized, &ty, SemOpKind::ConstInteger(0))?,
        };
        let positive = self.range_compare(BinaryOp::Greater, stride.clone(), zero)?;
        let valid = self.new_block(Vec::new());
        let invalid = self.new_block(Vec::new());
        self.set_terminator(SemTerminator::Branch {
            condition: positive,
            then_target: Edge {
                target: valid,
                args: Vec::new(),
            },
            else_target: Edge {
                target: invalid,
                args: Vec::new(),
            },
        })?;
        let before = self.control_state();
        self.current = invalid;
        let mut message = step.clone();
        message.ty = ResolvedTy::String;
        message.kind = HirExprKind::Literal(HirLiteral::String(
            "step_by requires a positive step".into(),
        ));
        let mut panic = step.clone();
        panic.ty = ResolvedTy::Never;
        self.lower_panic(&panic, &[message])?;
        self.restore_control_state(&before);
        self.current = valid;

        let outer_floor = self.scopes.len();
        self.open_scope();
        self.bind_source_value(loop_binding, initial.value)?;
        let mut carried = self.mutable_bindings();
        carried.push(loop_binding.id);
        let header = self.loop_join(&carried)?;
        let increment = self.loop_join(&carried)?;
        let exit = self.loop_join(&carried)?;
        let scope = LoopScope {
            label: label.map(str::to_owned),
            header: increment.block,
            exit: exit.block,
            carried,
            preserved: self.owned_live.keys().copied().collect(),
            scope_floor: self.scopes.len(),
        };
        // Check emptiness before subtracting from an exclusive high endpoint.
        // Even MIN..MIN reversed is empty without overflowing MIN - 1.
        let nonempty = self.range_compare(
            if inclusive {
                BinaryOp::LessEqual
            } else {
                BinaryOp::Less
            },
            initial.clone(),
            bound.clone(),
        )?;
        let setup = self.new_block(Vec::new());
        let empty = self.loop_edge(&scope, exit.block)?;
        self.set_terminator(SemTerminator::Branch {
            condition: nonempty,
            then_target: Edge {
                target: setup,
                args: Vec::new(),
            },
            else_target: empty,
        })?;
        self.current = setup;
        if descending {
            let first = if inclusive {
                bound.value
            } else {
                let one = Operand {
                    value: self.emit_typed(
                        Provenance::Synthesized,
                        &ty,
                        SemOpKind::ConstInteger(1),
                    )?,
                };
                self.range_advance(&ty, BinaryOp::Subtract, bound.clone(), one, &scope)?
            };
            self.bindings
                .insert(loop_binding.id, BindingTarget::Value(first));
        }
        let entry = self.loop_edge(&scope, header.block)?;
        self.set_terminator(SemTerminator::Goto(entry))?;
        self.restore_control_state(&header);
        let counter = Operand {
            value: self.scalar_binding(loop_binding.id)?,
        };
        let (comparison, endpoint) = if descending {
            (BinaryOp::GreaterEqual, initial)
        } else {
            (
                if inclusive {
                    BinaryOp::LessEqual
                } else {
                    BinaryOp::Less
                },
                bound,
            )
        };
        let condition = self.range_compare(comparison, counter, endpoint)?;
        let exit_edge = self.loop_edge(&scope, exit.block)?;
        let body_block = self.new_block(Vec::new());
        self.set_terminator(SemTerminator::Branch {
            condition,
            then_target: Edge {
                target: body_block,
                args: Vec::new(),
            },
            else_target: exit_edge,
        })?;
        self.current = body_block;
        self.loops.push(Some(scope.clone()));
        let tail = self.lower_scoped_block(body, OwnedBindingUse::Copy)?;
        if let Some(tail) = tail {
            if self.owned_live.contains_key(&tail.value) {
                self.emit_destroy(tail.value)?;
            }
        }
        if self.is_open() {
            let edge = self.loop_edge(&scope, increment.block)?;
            self.set_terminator(SemTerminator::Goto(edge))?;
        }
        self.loops.pop();
        self.restore_control_state(&increment);
        let counter = Operand {
            value: self.scalar_binding(loop_binding.id)?,
        };
        let next = self.range_advance(
            &ty,
            if descending {
                BinaryOp::Subtract
            } else {
                BinaryOp::Add
            },
            counter,
            stride,
            &scope,
        )?;
        self.bindings
            .insert(loop_binding.id, BindingTarget::Value(next));
        let edge = self.loop_edge(&scope, header.block)?;
        self.set_terminator(SemTerminator::Goto(edge))?;
        self.restore_control_state(&exit);
        self.end_scopes(outer_floor)?;
        self.leave_scope();
        Ok(())
    }

    fn lower_range_operand(&mut self, expr: &HirExpr, ty: &ResolvedTy) -> Result<Operand, String> {
        let value = self.lower_read_operand(expr, "range operand")?;
        Ok(Operand {
            value: self.coerce_value(value.value, ty, Provenance::Site(expr.site))?,
        })
    }

    fn range_compare(
        &mut self,
        op: hew_parser::ast::BinaryOp,
        lhs: Operand,
        rhs: Operand,
    ) -> Result<Operand, String> {
        Ok(Operand {
            value: self.emit_typed(
                Provenance::Synthesized,
                &ResolvedTy::Bool,
                SemOpKind::Binary { op, lhs, rhs },
            )?,
        })
    }

    /// Overflow advances beyond the integer domain and therefore exhausts the
    /// range. It never wraps, repeats an endpoint or raises an arithmetic fault.
    fn range_advance(
        &mut self,
        ty: &ResolvedTy,
        op: hew_parser::ast::BinaryOp,
        lhs: Operand,
        rhs: Operand,
        scope: &LoopScope,
    ) -> Result<ValueId, String> {
        use hew_parser::ast::BinaryOp;
        let ascending = op == BinaryOp::Add;
        let next = self.emit_typed(
            Provenance::Synthesized,
            ty,
            SemOpKind::Binary {
                op: if ascending {
                    BinaryOp::WrappingAdd
                } else {
                    BinaryOp::WrappingSub
                },
                lhs: lhs.clone(),
                rhs,
            },
        )?;
        // A positive stride must move strictly in its declared direction.
        // Reversed order after wrapping means the integer domain is exhausted.
        let progressed = self.range_compare(
            if ascending {
                BinaryOp::Greater
            } else {
                BinaryOp::Less
            },
            Operand { value: next },
            lhs,
        )?;
        let normal = self.new_block(Vec::new());
        let exhausted = self.loop_edge(scope, scope.exit)?;
        self.set_terminator(SemTerminator::Branch {
            condition: progressed,
            then_target: Edge {
                target: normal,
                args: Vec::new(),
            },
            else_target: exhausted,
        })?;
        self.current = normal;
        Ok(next)
    }
}
