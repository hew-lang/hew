use std::collections::HashMap;

use hew_hir::{
    BindingId, HirBlock, HirExpr, HirExprKind, HirFn, HirItem, HirLiteral, HirModule, HirStmtKind,
    ResolvedRef,
};

use crate::{
    BlockArg, BlockId, Edge, FunctionSourceOrigin, OpId, Operand, Provenance, SemBlock,
    SemFunction, SemModule, SemOp, SemOpKind, SemTerminator, UseMode, ValueDef, ValueId,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SirLoweringStatus {
    Lowered,
    Unsupported { reason: String },
}

#[derive(Debug, Clone, PartialEq)]
pub struct LoweredModule {
    pub module: SemModule,
    /// One status per HIR function, in source order. Shadow-mode callers must
    /// only regard the SIR result as comparable when every status is `Lowered`.
    pub statuses: Vec<(String, SirLoweringStatus)>,
}

#[must_use]
pub fn lower_module(module: &HirModule) -> LoweredModule {
    let mut output = SemModule::default();
    let mut statuses = Vec::new();
    for item in &module.items {
        let HirItem::Function(function) = item else {
            continue;
        };
        let source_origin = if module.root_item_ids.contains(&function.id) {
            FunctionSourceOrigin::RootUnit
        } else if let Some(module_name) = module.diagnostic_source_modules.get(&function.id) {
            FunctionSourceOrigin::Foreign(module_name.clone())
        } else {
            FunctionSourceOrigin::Unknown
        };
        match Builder::new(function, source_origin).lower() {
            Ok(function) => {
                statuses.push((function.name.clone(), SirLoweringStatus::Lowered));
                output.functions.push(function);
            }
            Err(reason) => statuses.push((
                function.name.clone(),
                SirLoweringStatus::Unsupported { reason },
            )),
        }
    }
    LoweredModule {
        module: output,
        statuses,
    }
}

struct Builder<'a> {
    function: &'a HirFn,
    blocks: Vec<SemBlock>,
    current: BlockId,
    values: u32,
    ops: u32,
    bindings: HashMap<BindingId, ValueId>,
    params: Vec<BlockArg>,
    source_origin: FunctionSourceOrigin,
}

impl<'a> Builder<'a> {
    fn new(function: &'a HirFn, source_origin: FunctionSourceOrigin) -> Self {
        let entry = BlockId(0);
        let mut values = 0;
        let mut bindings = HashMap::new();
        let params = function
            .params
            .iter()
            .map(|param| {
                let value = ValueId(values);
                values += 1;
                bindings.insert(param.id, value);
                BlockArg {
                    value,
                    ty: param.ty.clone(),
                }
            })
            .collect();
        Self {
            function,
            blocks: vec![SemBlock {
                id: entry,
                args: Vec::new(),
                ops: Vec::new(),
                terminator: SemTerminator::Unreachable,
            }],
            current: entry,
            values,
            ops: 0,
            bindings,
            params,
            source_origin,
        }
    }

    fn lower(mut self) -> Result<SemFunction, String> {
        if self.function.is_generator || self.function.intrinsic_id.is_some() {
            return Err(
                "generators and floor intrinsics remain on the established MIR path".to_string(),
            );
        }
        if !self.function.type_params.is_empty() {
            return Err(
                "generic origin functions remain on the established MIR path until SIR is monomorphization-aware"
                    .to_string(),
            );
        }
        let result = self.lower_block(&self.function.body)?;
        if self.is_open() {
            self.set_terminator(SemTerminator::Return { value: result });
        }
        Ok(SemFunction {
            id: self.function.id,
            declaration: self.function.declaration.clone(),
            name: self.function.name.clone(),
            span: self.function.span.clone(),
            source_origin: self.source_origin,
            params: self.params,
            return_ty: self.function.return_ty.clone(),
            entry: BlockId(0),
            blocks: self.blocks,
        })
    }

    fn lower_block(&mut self, block: &HirBlock) -> Result<Option<ValueId>, String> {
        for statement in &block.statements {
            if !self.is_open() {
                break;
            }
            match &statement.kind {
                HirStmtKind::Let(binding, value) => {
                    if binding.mutable {
                        return Err("mutable bindings are deferred until a dedicated SIR feature requires them".to_string());
                    }
                    let value = value
                        .as_ref()
                        .map(|expr| self.lower_expr(expr))
                        .transpose()?
                        .ok_or_else(|| {
                            "uninitialised bindings are not in the initial SIR subset".to_string()
                        })?;
                    self.bindings.insert(binding.id, value);
                }
                HirStmtKind::Expr(expr) => {
                    self.lower_expr(expr)?;
                }
                HirStmtKind::Return(value) => {
                    let value = value
                        .as_ref()
                        .map(|expr| self.lower_expr(expr))
                        .transpose()?;
                    self.set_terminator(SemTerminator::Return { value });
                }
                HirStmtKind::Assign { .. } => {
                    return Err(
                        "assignment is deferred until SIR has an explicit mutable-location design"
                            .to_string(),
                    )
                }
                HirStmtKind::LetElse { .. } | HirStmtKind::Defer { .. } => {
                    return Err(
                        "control-flow ownership forms are deferred to a later SIR slice"
                            .to_string(),
                    )
                }
            }
        }
        if self.is_open() {
            block
                .tail
                .as_deref()
                .map(|expr| self.lower_expr(expr))
                .transpose()
        } else {
            Ok(None)
        }
    }

    #[allow(
        clippy::too_many_lines,
        reason = "the closed initial HIR-to-SIR expression mapping remains intentionally local"
    )]
    fn lower_expr(&mut self, expr: &HirExpr) -> Result<ValueId, String> {
        match &expr.kind {
            HirExprKind::Literal(HirLiteral::Integer(value)) => {
                if !expr.ty.is_integer() {
                    return Err(format!(
                        "integer literal resolved as `{}` needs a dedicated SIR literal representation",
                        expr.ty.user_facing()
                    ));
                }
                Ok(self.emit(expr, SemOpKind::ConstI64(*value)))
            }
            HirExprKind::Literal(HirLiteral::Bool(value)) => {
                if expr.ty != hew_types::ResolvedTy::Bool {
                    return Err(format!(
                        "boolean literal resolved as `{}` violates the SIR bool literal invariant",
                        expr.ty.user_facing()
                    ));
                }
                Ok(self.emit(expr, SemOpKind::ConstBool(*value)))
            }
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(binding),
                ..
            } => self.bindings.get(binding).copied().ok_or_else(|| {
                format!("binding `{binding}` is not available in the SIR environment")
            }),
            HirExprKind::Unary { op, operand, .. } => {
                let value = self.lower_expr(operand)?;
                Ok(self.emit(
                    expr,
                    SemOpKind::Unary {
                        op: *op,
                        value: Operand {
                            value,
                            mode: UseMode::Read,
                        },
                    },
                ))
            }
            HirExprKind::Binary {
                op: hew_parser::ast::BinaryOp::And,
                left,
                right,
            } => self.lower_logical_and(expr, left, right),
            HirExprKind::Binary {
                op: hew_parser::ast::BinaryOp::Or,
                left,
                right,
            } => self.lower_logical_or(expr, left, right),
            HirExprKind::Binary { op, left, right } => {
                let lhs = self.lower_expr(left)?;
                let rhs = self.lower_expr(right)?;
                Ok(self.emit(
                    expr,
                    SemOpKind::Binary {
                        op: *op,
                        lhs: Operand {
                            value: lhs,
                            mode: UseMode::Read,
                        },
                        rhs: Operand {
                            value: rhs,
                            mode: UseMode::Read,
                        },
                    },
                ))
            }
            HirExprKind::NumericCast { value, to_ty, .. } => {
                let value = self.lower_expr(value)?;
                Ok(self.emit(
                    expr,
                    SemOpKind::Cast {
                        value: Operand {
                            value,
                            mode: UseMode::Read,
                        },
                        to: to_ty.clone(),
                    },
                ))
            }
            HirExprKind::Call {
                target,
                callee,
                args,
            } => {
                if matches!(target, hew_types::CallTarget::IndirectFunctionValue) {
                    return Err(
                        "indirect calls are deferred until SIR models the callee value explicitly"
                            .to_string(),
                    );
                }
                if !matches!(callee.kind, HirExprKind::BindingRef { .. }) {
                    return Err(
                        "calls with an evaluated callee are deferred until SIR models callee values"
                            .to_string(),
                    );
                }
                let args = args
                    .iter()
                    .map(|arg| {
                        self.lower_expr(arg).map(|value| Operand {
                            value,
                            mode: UseMode::Read,
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(self.emit(
                    expr,
                    SemOpKind::Call {
                        target: target.clone(),
                        args,
                    },
                ))
            }
            HirExprKind::Block(block) => self
                .lower_block(block)?
                .ok_or_else(|| "a divergent block cannot produce a SIR value".to_string()),
            HirExprKind::If {
                condition,
                then_expr,
                else_expr: Some(else_expr),
            } => self.lower_if(expr, condition, then_expr, else_expr),
            HirExprKind::If {
                else_expr: None, ..
            } => Err(
                "one-armed if expressions are deferred until unit values are modeled".to_string(),
            ),
            _ => Err("unsupported HIR expression kind in the initial SIR subset".to_string()),
        }
    }

    fn lower_if(
        &mut self,
        whole: &HirExpr,
        condition: &HirExpr,
        then_expr: &HirExpr,
        else_expr: &HirExpr,
    ) -> Result<ValueId, String> {
        let condition = self.lower_expr(condition)?;
        let then_block = self.new_block(Vec::new());
        let else_block = self.new_block(Vec::new());
        let join_value = self.fresh_value();
        let join_block = self.new_block(vec![BlockArg {
            value: join_value,
            ty: whole.ty.clone(),
        }]);
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
        });
        let before = self.bindings.clone();
        self.current = then_block;
        self.bindings = before.clone();
        let then_value = self.lower_expr(then_expr)?;
        if self.is_open() {
            self.set_terminator(SemTerminator::Goto(Edge {
                target: join_block,
                args: vec![then_value],
            }));
        }
        self.current = else_block;
        self.bindings = before;
        let else_value = self.lower_expr(else_expr)?;
        if self.is_open() {
            self.set_terminator(SemTerminator::Goto(Edge {
                target: join_block,
                args: vec![else_value],
            }));
        }
        self.current = join_block;
        Ok(join_value)
    }

    /// Lower short-circuit `&&` as CFG rather than an eager binary operation.
    ///
    /// The false edge materialises the result while the true edge alone
    /// evaluates the right-hand side. This keeps effectful future SIR
    /// operations on the RHS structurally guarded from the outset.
    fn lower_logical_and(
        &mut self,
        whole: &HirExpr,
        left: &HirExpr,
        right: &HirExpr,
    ) -> Result<ValueId, String> {
        self.lower_short_circuit(whole, left, right, false)
    }

    /// Lower short-circuit `||` as CFG rather than an eager binary operation.
    fn lower_logical_or(
        &mut self,
        whole: &HirExpr,
        left: &HirExpr,
        right: &HirExpr,
    ) -> Result<ValueId, String> {
        self.lower_short_circuit(whole, left, right, true)
    }

    fn lower_short_circuit(
        &mut self,
        whole: &HirExpr,
        left: &HirExpr,
        right: &HirExpr,
        short_circuit_value: bool,
    ) -> Result<ValueId, String> {
        if whole.ty != hew_types::ResolvedTy::Bool {
            return Err("short-circuit logical expressions must have bool type in SIR".to_string());
        }
        let condition = self.lower_expr(left)?;
        let evaluate_right = self.new_block(Vec::new());
        let short_circuit = self.new_block(Vec::new());
        let result = self.fresh_value();
        let join = self.new_block(vec![BlockArg {
            value: result,
            ty: whole.ty.clone(),
        }]);
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
        });

        let before = self.bindings.clone();
        self.current = evaluate_right;
        self.bindings = before.clone();
        let right_value = self.lower_expr(right)?;
        if self.is_open() {
            self.set_terminator(SemTerminator::Goto(Edge {
                target: join,
                args: vec![right_value],
            }));
        }

        self.current = short_circuit;
        self.bindings = before;
        let constant = self.emit(whole, SemOpKind::ConstBool(short_circuit_value));
        self.set_terminator(SemTerminator::Goto(Edge {
            target: join,
            args: vec![constant],
        }));

        self.current = join;
        Ok(result)
    }

    fn emit(&mut self, expr: &HirExpr, kind: SemOpKind) -> ValueId {
        let value = self.fresh_value();
        let op = SemOp {
            id: OpId(self.ops),
            results: vec![ValueDef {
                id: value,
                ty: expr.ty.clone(),
            }],
            kind,
            provenance: Provenance::Site(expr.site),
        };
        self.ops += 1;
        self.current_block_mut().ops.push(op);
        value
    }

    fn fresh_value(&mut self) -> ValueId {
        let value = ValueId(self.values);
        self.values += 1;
        value
    }
    fn new_block(&mut self, args: Vec<BlockArg>) -> BlockId {
        let id = BlockId(u32::try_from(self.blocks.len()).expect("SIR block count exceeds u32"));
        self.blocks.push(SemBlock {
            id,
            args,
            ops: Vec::new(),
            terminator: SemTerminator::Unreachable,
        });
        id
    }
    fn current_block_mut(&mut self) -> &mut SemBlock {
        &mut self.blocks[self.current.0 as usize]
    }
    fn is_open(&self) -> bool {
        matches!(
            self.blocks[self.current.0 as usize].terminator,
            SemTerminator::Unreachable
        )
    }
    fn set_terminator(&mut self, term: SemTerminator) {
        self.current_block_mut().terminator = term;
    }
}
