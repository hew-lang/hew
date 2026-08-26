use std::collections::HashMap;

use hew_hir::{
    BindingId, HirBlock, HirExpr, HirExprKind, HirFn, HirItem, HirLiteral, HirModule, HirStmtKind,
    IntentKind, ResolvedRef,
};

use crate::{
    BlockArg, BlockId, CallableId, Edge, EffectSummary, FunctionSourceOrigin, OpId, Operand,
    Provenance, SemAbiParam, SemBlock, SemCallConv, SemCallable, SemCallableKind, SemFunction,
    SemModule, SemOp, SemOpKind, SemParamPassing, SemSignature, SemTerminator, UseMode, ValueDef,
    ValueId,
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
    // Build declaration/signature authority before lowering any body. This
    // permits forward calls and recursion without symbol reconstruction, and
    // deliberately keeps an eligible callable when its own body later proves
    // unsupported: strict drivers can then reject only a reachable missing
    // body instead of every unrelated unsupported function in a module.
    let callables = CallableTable::from_hir(module);
    let mut output = SemModule {
        callables: callables.callables.clone(),
        root_unit_callables: callables.root_unit_callables.clone(),
        entry_callable: callables.entry_callable,
        functions: Vec::new(),
    };
    let mut statuses = Vec::new();
    for item in &module.items {
        let HirItem::Function(function) = item else {
            continue;
        };
        let Some(callable) = callables.by_declaration.get(&function.declaration).copied() else {
            let reason = callables
                .ineligible
                .get(&function.id)
                .cloned()
                .unwrap_or_else(|| {
                    "function has no deterministic SIR direct-call entry".to_string()
                });
            statuses.push((
                function.name.clone(),
                SirLoweringStatus::Unsupported { reason },
            ));
            continue;
        };
        match Builder::new(
            function,
            function_source_origin(module, function),
            &callables,
            callable,
        )
        .lower()
        {
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

/// Deterministic SIR view of the HIR direct-call projection.
///
/// The HIR dispatcher is still the owner of exact emitted symbols.  SIR only
/// projects those checked facts into its semantic callable table; it never
/// reconstructs a symbol from a declaration's presentation spelling.
#[derive(Debug, Clone)]
struct CallableTable {
    callables: Vec<SemCallable>,
    root_unit_callables: Vec<CallableId>,
    entry_callable: Option<CallableId>,
    by_declaration: HashMap<hew_types::DefId, CallableId>,
    ineligible: HashMap<hew_hir::ItemId, String>,
}

impl CallableTable {
    fn from_hir(module: &HirModule) -> Self {
        let direct_symbols = hew_hir::dispatch::build_direct_call_symbol_index(&module.items);
        let mut pending = Vec::new();
        let mut ineligible = HashMap::new();
        for item in &module.items {
            let HirItem::Function(function) = item else {
                continue;
            };
            let signature = match scalar_callable_signature(function) {
                Ok(signature) => signature,
                Err(reason) => {
                    ineligible.insert(function.id, reason);
                    continue;
                }
            };
            let Some(symbol) = direct_symbols.get(&function.declaration) else {
                ineligible.insert(
                    function.id,
                    format!(
                        "HIR direct-call symbol index has no exact symbol for declaration `{}`",
                        function.declaration.full_path()
                    ),
                );
                continue;
            };
            pending.push((
                function,
                function_source_origin(module, function),
                symbol.clone(),
                signature,
            ));
        }
        pending.sort_unstable_by(|(left, _, left_symbol, _), (right, _, right_symbol, _)| {
            left.declaration
                .cmp(&right.declaration)
                .then_with(|| left_symbol.cmp(right_symbol))
                .then_with(|| left.id.cmp(&right.id))
        });

        let mut callables = Vec::with_capacity(pending.len());
        let mut root_unit_callables = Vec::new();
        let mut entry_callable = None;
        let mut by_declaration = HashMap::with_capacity(pending.len());
        for (index, (function, source_origin, symbol, signature)) in pending.into_iter().enumerate()
        {
            let id = CallableId(
                u32::try_from(index).expect("SIR callable count exceeds the module-local ID range"),
            );
            if source_origin == FunctionSourceOrigin::RootUnit {
                root_unit_callables.push(id);
                if function.declaration.full_path() == "main" {
                    entry_callable = Some(id);
                }
            }
            by_declaration.insert(function.declaration.clone(), id);
            callables.push(SemCallable {
                id,
                function: function.id,
                declaration: function.declaration.clone(),
                symbol,
                source_origin,
                signature,
                call_conv: SemCallConv::Default,
                kind: SemCallableKind::HewDirect,
                effect_summary: EffectSummary::Unknown,
            });
        }
        Self {
            callables,
            root_unit_callables,
            entry_callable,
            by_declaration,
            ineligible,
        }
    }

    fn callable(&self, id: CallableId) -> Option<&SemCallable> {
        self.callables
            .get(usize::try_from(id.0).ok()?)
            .filter(|callable| callable.id == id)
    }
}

fn function_source_origin(module: &HirModule, function: &HirFn) -> FunctionSourceOrigin {
    if module.root_item_ids.contains(&function.id) {
        FunctionSourceOrigin::RootUnit
    } else if let Some(module_name) = module.diagnostic_source_modules.get(&function.id) {
        FunctionSourceOrigin::Foreign(module_name.clone())
    } else {
        FunctionSourceOrigin::Unknown
    }
}

fn scalar_callable_signature(function: &HirFn) -> Result<SemSignature, String> {
    if function.is_generator || function.intrinsic_id.is_some() {
        return Err(
            "generators and floor intrinsics remain outside SIR's ordinary direct-call domain"
                .to_string(),
        );
    }
    if !function.type_params.is_empty() {
        return Err(
            "generic origin functions remain outside SIR's monomorphic direct-call domain"
                .to_string(),
        );
    }
    let mut params = Vec::with_capacity(function.params.len());
    for (index, parameter) in function.params.iter().enumerate() {
        if parameter.is_consume {
            return Err(format!(
                "parameter {index} is consume-owned; SIR direct calls initially require Read operands"
            ));
        }
        if !is_initial_scalar(&parameter.ty) {
            return Err(format!(
                "parameter {index} has non-scalar type `{}`; aggregate/reference ABI lowering is deferred",
                parameter.ty.user_facing()
            ));
        }
        params.push(SemAbiParam {
            ty: parameter.ty.clone(),
            passing: SemParamPassing::ReadOnly,
            caller_visible_projection: false,
        });
    }
    if !is_initial_scalar_return(&function.return_ty) {
        return Err(format!(
            "return type `{}` is outside SIR's initial scalar call-result domain",
            function.return_ty.user_facing()
        ));
    }
    Ok(SemSignature {
        params,
        return_ty: function.return_ty.clone(),
    })
}

fn is_initial_scalar(ty: &hew_types::ResolvedTy) -> bool {
    ty.is_integer() || matches!(ty, hew_types::ResolvedTy::Bool)
}

fn is_initial_scalar_return(ty: &hew_types::ResolvedTy) -> bool {
    matches!(ty, hew_types::ResolvedTy::Unit) || is_initial_scalar(ty)
}

struct Builder<'a> {
    function: &'a HirFn,
    callables: &'a CallableTable,
    callable: CallableId,
    blocks: Vec<SemBlock>,
    current: BlockId,
    values: u32,
    ops: u32,
    bindings: HashMap<BindingId, ValueId>,
    params: Vec<BlockArg>,
    source_origin: FunctionSourceOrigin,
}

impl<'a> Builder<'a> {
    fn new(
        function: &'a HirFn,
        source_origin: FunctionSourceOrigin,
        callables: &'a CallableTable,
        callable: CallableId,
    ) -> Self {
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
            callables,
            callable,
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
        let callable = self.callables.callable(self.callable).ok_or_else(|| {
            format!(
                "SIR callable {:?} is absent while lowering `{}`",
                self.callable, self.function.name
            )
        })?;
        if callable.function != self.function.id
            || callable.declaration != self.function.declaration
            || callable.symbol != self.function.name
        {
            return Err(
                "SIR callable table does not match the HIR function's checked identity".to_string(),
            );
        }
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
            callable: self.callable,
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
                    self.lower_discarded_expr(expr)?;
                }
                HirStmtKind::Return(value) => {
                    let value = match value {
                        Some(expr) if expr.ty == hew_types::ResolvedTy::Unit => {
                            self.lower_discarded_expr(expr)?;
                            None
                        }
                        Some(expr) => Some(self.lower_expr(expr)?),
                        None => None,
                    };
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
            match block.tail.as_deref() {
                Some(expr) if expr.ty == hew_types::ResolvedTy::Unit => {
                    self.lower_discarded_expr(expr)?;
                    Ok(None)
                }
                Some(expr) => Ok(Some(self.lower_expr(expr)?)),
                None => Ok(None),
            }
        } else {
            Ok(None)
        }
    }

    /// Lower an expression whose value is intentionally discarded.
    ///
    /// Scalar expressions keep their ordinary one-result SSA operation even
    /// when the result is unused.  A unit direct call is different: there is
    /// no semantic value to define, but the call itself must remain in SIR so
    /// later lowering can realize its call/continuation CFG edge.
    fn lower_discarded_expr(&mut self, expr: &HirExpr) -> Result<(), String> {
        if matches!(expr.kind, HirExprKind::Call { .. }) {
            self.lower_direct_call(expr, false)?;
            return Ok(());
        }
        self.lower_expr(expr).map(|_| ())
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
            HirExprKind::Call { .. } => self.lower_direct_call(expr, true)?.ok_or_else(|| {
                "unit-valued direct calls are valid only in a discarded or unit-return context"
                    .to_string()
            }),
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

    /// Lower an HIR direct call through the resolved SIR callable table.
    ///
    /// `value_required` distinguishes a value context from a discarded/unit
    /// context.  Non-unit calls always retain their single SSA result; unit
    /// calls are admitted only in the latter and become zero-result `Call`
    /// operations.
    #[allow(
        clippy::too_many_lines,
        reason = "the initial direct-call ABI admission is deliberately kept as one auditable HIR-to-SIR boundary"
    )]
    fn lower_direct_call(
        &mut self,
        expr: &HirExpr,
        value_required: bool,
    ) -> Result<Option<ValueId>, String> {
        let HirExprKind::Call {
            target,
            callee,
            args,
        } = &expr.kind
        else {
            return Err(
                "internal SIR lowering error: direct-call lowering received a non-call".to_string(),
            );
        };
        let declaration =
            match target {
                hew_types::CallTarget::User(declaration)
                | hew_types::CallTarget::ImplMethod(declaration) => declaration,
                hew_types::CallTarget::IndirectFunctionValue => {
                    return Err(
                        "indirect calls are deferred until SIR models the callee value explicitly"
                            .to_string(),
                    )
                }
                _ => return Err(
                    "only ordinary user/impl direct calls are in SIR's initial scalar call domain"
                        .to_string(),
                ),
            };
        if !matches!(callee.kind, HirExprKind::BindingRef { .. }) {
            return Err(
                "calls with an evaluated callee are deferred until SIR models callee values"
                    .to_string(),
            );
        }
        let callee = self
            .callables
            .by_declaration
            .get(declaration)
            .copied()
            .ok_or_else(|| {
                format!(
                    "direct callee `{}` has no scalar default-call SIR callable",
                    declaration.full_path()
                )
            })?;
        let (callee_declaration, params, return_ty) = self
            .callables
            .callable(callee)
            .map(|signature| {
                (
                    signature.declaration.clone(),
                    signature.signature.params.clone(),
                    signature.signature.return_ty.clone(),
                )
            })
            .ok_or_else(|| {
                format!("SIR callable {callee:?} is absent from its deterministic table")
            })?;
        if args.len() != params.len() {
            return Err(format!(
                "direct callee `{}` expects {} argument(s), HIR carries {}",
                callee_declaration.full_path(),
                params.len(),
                args.len()
            ));
        }
        if expr.ty != return_ty {
            return Err(format!(
                "direct callee `{}` returns `{}`, but call expression has `{}`",
                callee_declaration.full_path(),
                return_ty.user_facing(),
                expr.ty.user_facing()
            ));
        }
        let mut lowered_args = Vec::with_capacity(args.len());
        for (index, (arg, expected)) in args.iter().zip(&params).enumerate() {
            if expected.passing != SemParamPassing::ReadOnly {
                return Err(format!(
                    "direct callee `{}` has a non-ReadOnly SIR ABI parameter {index}",
                    callee_declaration.full_path()
                ));
            }
            if arg.intent != IntentKind::Read {
                return Err(format!(
                    "direct call argument {index} to `{}` has {:?} intent; initial SIR calls require Read",
                    callee_declaration.full_path(),
                    arg.intent
                ));
            }
            if arg.ty != expected.ty {
                return Err(format!(
                    "direct call argument {index} to `{}` has `{}`, expected `{}`",
                    callee_declaration.full_path(),
                    arg.ty.user_facing(),
                    expected.ty.user_facing()
                ));
            }
            lowered_args.push(Operand {
                value: self.lower_expr(arg)?,
                mode: UseMode::Read,
            });
        }
        let kind = SemOpKind::Call {
            callee,
            args: lowered_args,
        };
        if return_ty == hew_types::ResolvedTy::Unit {
            if value_required {
                return Err(format!(
                    "unit-valued direct call `{}` cannot produce an SSA value",
                    callee_declaration.full_path()
                ));
            }
            self.emit_without_result(expr, kind);
            Ok(None)
        } else {
            Ok(Some(self.emit(expr, kind)))
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

    fn emit_without_result(&mut self, expr: &HirExpr, kind: SemOpKind) {
        let op = SemOp {
            id: OpId(self.ops),
            results: Vec::new(),
            kind,
            provenance: Provenance::Site(expr.site),
        };
        self.ops += 1;
        self.current_block_mut().ops.push(op);
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
