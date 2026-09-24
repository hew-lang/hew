//! Block and statement lowering.

use super::*;
use hew_parser::ast::Ident;

impl LowerCtx {
    pub(super) fn lower_block(&mut self, block: &Block, expected_ty: &ResolvedTy) -> HirBlock {
        self.push_scope();
        let scope = self.ids.scope();
        let prev_scope_id = std::mem::replace(&mut self.current_scope_id, scope);
        let mut statements = Vec::new();
        for (stmt, span) in &block.stmts {
            statements.extend(self.lower_stmt_multi(stmt, span.clone(), expected_ty.clone()));
        }
        let tail = block
            .trailing_expr
            .as_ref()
            .map(|expr| Box::new(self.lower_expr(expr, IntentKind::Read)));
        // A block's value type is its tail expression's type. With no tail it is
        // `Unit` UNLESS the block diverges: a trailing `return`/`break`/
        // `continue` (or a last statement that diverges in every branch, e.g. an
        // `if` whose arms all `return`) makes control never fall off the end, so
        // the block has type `Never`. This mirrors the checker's `check_block`
        // `Ty::Never` result and is what lets `let x = if c { v } else { return … }`
        // carry `v`'s type — without it the else block reads `Unit` and the
        // construct mis-types, breaking a later `x + 1` at MIR lowering.
        let ty = tail.as_ref().map_or_else(
            || {
                if block_diverges(&statements) {
                    ResolvedTy::Never
                } else {
                    ResolvedTy::Unit
                }
            },
            |expr| expr.ty.clone(),
        );
        self.current_scope_id = prev_scope_id;
        self.pop_scope();

        HirBlock {
            node: self.ids.node(),
            scope,
            statements,
            tail,
            ty,
            // The AST block carries no span of its own, so the lexical extent
            // is the extent of what it contains: every binding and statement
            // the scope owns lies inside it, which is what scope containment
            // asks. An empty block owns nothing and gets an empty extent.
            span: block_extent(block),
        }
    }

    /// Lower a statement, returning zero or more `HirStmt`s.
    ///
    /// Most statements produce exactly one `HirStmt` (delegated to `lower_stmt`).
    /// An irrefutable tuple or record pattern produces one typed destructure
    /// group, followed by another group for each nested aggregate field.
    #[expect(
        clippy::too_many_lines,
        reason = "aggregate let validation and checker-plan materialisation stay together so \
                  every canonical field is represented in the ordered destructure group"
    )]
    pub(super) fn lower_stmt_multi(
        &mut self,
        stmt: &Stmt,
        span: std::ops::Range<usize>,
        return_ty: ResolvedTy,
    ) -> Vec<HirStmt> {
        // Tuple-let: `let (a, b, ...) = value_expr;`. A let-else (`else_block:
        // Some`) is NOT an irrefutable tuple destructure — it routes through
        // `lower_stmt` → `lower_let_else`, so require `else_block: None` here.
        if let Stmt::Let {
            pattern,
            ty: annotation,
            value: Some(value_expr),
            else_block: None,
        } = stmt
        {
            if let Pattern::Tuple(element_patterns) = &pattern.0 {
                // Lower the tuple value once as the group's sole source.
                let tuple_val = self.lower_expr(value_expr, IntentKind::Consume);
                let tuple_ty = tuple_val.ty.clone();

                // Validate the pattern element count against the inferred tuple type.
                let element_tys: Vec<ResolvedTy> = if let ResolvedTy::Tuple(elems) = &tuple_ty {
                    if elems.len() != element_patterns.len() {
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::TuplePatternArityMismatch {
                                expected: elems.len(),
                                actual: element_patterns.len(),
                            },
                            span.clone(),
                            "tuple pattern element count does not match tuple value arity",
                        ));
                        return vec![HirStmt {
                            node: self.ids.node(),
                            kind: HirStmtKind::Expr(
                                self.unsupported_expr(span, "tuple arity mismatch"),
                            ),
                            span: 0..0,
                        }];
                    }
                    elems.clone()
                } else if annotation.is_none() {
                    // Type not yet resolved — use Unit for each element (diagnostic
                    // already emitted by the checker; HIR does best-effort lowering).
                    vec![ResolvedTy::Unit; element_patterns.len()]
                } else {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::TuplePatternNonTupleValue,
                        span.clone(),
                        "tuple-let pattern requires the right-hand side to have a tuple type",
                    ));
                    return vec![HirStmt {
                        node: self.ids.node(),
                        kind: HirStmtKind::Expr(
                            self.unsupported_expr(span, "tuple pattern on non-tuple"),
                        ),
                        span: 0..0,
                    }];
                };

                let mut stmts = Vec::new();
                self.lower_tuple_pattern_value_into_stmts(
                    element_patterns,
                    tuple_val,
                    &ResolvedTy::Tuple(element_tys),
                    &mut stmts,
                    span,
                );
                return stmts;
            }

            // Record-let: `let Point { x, y } = value_expr;`
            // and shorthand: `let { x, y } = value_expr;`
            //
            // Checker authority: the refutability gate (Stage 1) has already
            // rejected non-product-type scrutinee patterns before HIR lowers.
            // `bind_pattern` (called by the checker at check_stmt time) has
            // already bound the field names in the checker's env; HIR mirrors
            // those bindings via `self.bind(...)` below.
            let source_fields_opt = match &pattern.0 {
                Pattern::RecordShorthand { fields, .. } => Some(fields),
                // TRANSITION(P1): deleted by A1 commit 2
                Pattern::NominalPath {
                    path,
                    payload: Some(hew_parser::ast::NominalPatternPayload::Record { fields, .. }),
                } if path.segments.len() == 1 => Some(fields),
                _ => None,
            };
            if let Some(source_fields) = source_fields_opt {
                let key = self.mk_key(&pattern.1);
                let Some(plan) = self.pattern_plans.get(&key).cloned() else {
                    let _ = self.lower_expr(value_expr, IntentKind::Consume);
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: "record pattern".into(),
                            reason: "missing checker PatternPlan".into(),
                        },
                        pattern.1.clone(),
                        "checker did not provide a canonical plan for this record pattern",
                    ));
                    return vec![HirStmt {
                        node: self.ids.node(),
                        kind: HirStmtKind::Expr(
                            self.unsupported_expr(
                                span.clone(),
                                "record pattern missing PatternPlan",
                            ),
                        ),
                        span: span.clone(),
                    }];
                };
                let mut planned_fields = Vec::with_capacity(plan.fields.len());
                for field in plan.fields {
                    let field_ty = match ResolvedTy::from_ty(&field.ty) {
                        Ok(ty) => self.qualify_current_module_record_ty(ty),
                        Err(err) => {
                            let _ = self.lower_expr(value_expr, IntentKind::Consume);
                            self.diagnostics.push(HirDiagnostic::new(
                                HirDiagnosticKind::CheckerBoundaryViolation {
                                    name: field.name.clone(),
                                    reason: format!(
                                        "PatternPlan field type is unresolved ({err:?})"
                                    ),
                                },
                                field.span.clone(),
                                "record pattern plan contains an unresolved field type",
                            ));
                            return vec![HirStmt {
                                node: self.ids.node(),
                                kind: HirStmtKind::Expr(self.unsupported_expr(
                                    span.clone(),
                                    "record PatternPlan field type is unresolved",
                                )),
                                span: span.clone(),
                            }];
                        }
                    };
                    let field_pattern = match field.sub {
                        hew_types::PlanSub::Binding(name) => {
                            (Pattern::Identifier(Ident::new(&name)), field.span.clone())
                        }
                        hew_types::PlanSub::Wildcard => (Pattern::Wildcard, field.span.clone()),
                        hew_types::PlanSub::Literal(literal) => {
                            (Pattern::Literal(literal), field.span.clone())
                        }
                        hew_types::PlanSub::Nested(_) => {
                            let Some(source_pattern) = source_fields
                                .iter()
                                .find(|source| source.name == Ident::new(&field.name))
                                .and_then(|source| source.pattern.clone())
                            else {
                                let _ = self.lower_expr(value_expr, IntentKind::Consume);
                                self.diagnostics.push(HirDiagnostic::new(
                                    HirDiagnosticKind::CheckerBoundaryViolation {
                                        name: field.name.clone(),
                                        reason: "nested PatternPlan field has no source subpattern"
                                            .into(),
                                    },
                                    field.span.clone(),
                                    "record pattern plan cannot be materialised",
                                ));
                                return vec![HirStmt {
                                    node: self.ids.node(),
                                    kind: HirStmtKind::Expr(self.unsupported_expr(
                                        span.clone(),
                                        "record PatternPlan nested field is missing",
                                    )),
                                    span: span.clone(),
                                }];
                            };
                            source_pattern
                        }
                    };
                    planned_fields.push((field.name, field_ty, field_pattern));
                }

                let rec_val = self.lower_expr(value_expr, IntentKind::Consume);
                let rec_ty = rec_val.ty.clone();
                let mut stmts = Vec::new();
                self.lower_planned_record_pattern_value_into_stmts(
                    planned_fields,
                    rec_val,
                    &rec_ty,
                    &mut stmts,
                    span,
                );
                return stmts;
            }
        }

        // Non-tuple statements: delegate to the single-statement path.
        vec![self.lower_stmt(stmt, span, return_ty)]
    }

    pub(super) fn lower_expression_stmt_kind(&mut self, expr: &Spanned<Expr>) -> HirStmtKind {
        HirStmtKind::Expr(self.lower_expr(expr, IntentKind::Read))
    }

    /// True when the `await`'s inner expression is a suspending typed-stream
    /// `recv()` over a `Stream<T>` — i.e. the checker-resolved descriptor's
    /// family classifies as `AsyncSuspendKind::StreamRecv` (the layout-witness
    /// `hew_stream_next_layout` entry). `await stream.recv()` is
    /// a bindable, value-producing await (NEW-7): it lowers to the inner
    /// recv call whose `Option<T>` result the MIR `SuspendingStreamNext`
    /// resume edge binds.
    pub(super) fn is_stream_recv_await(&self, inner_key: &SpanKey) -> bool {
        matches!(
            self.method_call_rewrites.get(inner_key),
            Some(MethodCallRewrite::RewriteToFunction { descriptor: Some(d), .. })
                if d.is_async_suspending()
                    == Some(hew_types::runtime_call::AsyncSuspendKind::StreamRecv)
        )
    }

    /// True when the `await`'s inner expression is a suspending typed-stream
    /// `send()` over any describable `Sink<T>` — i.e. the checker-resolved
    /// descriptor's family classifies as [`AsyncSuspendKind::SinkSend`]
    /// (`hew_sink_write_bytes`, `hew_sink_write_string`, or the layout-witness
    /// `hew_stream_send_layout`). `await sink.send(x)` is a unit-returning
    /// statement-position await (NEW-7): it lowers to the inner send call
    /// whose MIR `SuspendingStreamSend` suspends on a full ring. The element
    /// type rides the checker-resolved value type, never the symbol name.
    ///
    /// [`AsyncSuspendKind::SinkSend`]: hew_types::runtime_call::AsyncSuspendKind
    pub(super) fn is_stream_send_await(&self, inner_key: &SpanKey) -> bool {
        matches!(
            self.method_call_rewrites.get(inner_key),
            Some(MethodCallRewrite::RewriteToFunction { descriptor: Some(d), .. })
                if d.is_async_suspending()
                    == Some(hew_types::runtime_call::AsyncSuspendKind::SinkSend)
        )
    }

    #[allow(
        clippy::too_many_lines,
        reason = "single large match on stmt variants; splitting would hurt readability"
    )]
    pub(super) fn lower_stmt(
        &mut self,
        stmt: &Stmt,
        span: std::ops::Range<usize>,
        return_ty: ResolvedTy,
    ) -> HirStmt {
        let kind = match stmt {
            Stmt::Let {
                pattern,
                ty,
                value,
                else_block,
            } => {
                // let-else: `let Pat = scrutinee else { <divergent block> };`.
                // Desugars through `lower_let_else` to a match on Pat plus a
                // destructure of its bindings into the enclosing scope — return
                // early, since the ordinary-let machinery below does not apply
                // (it binds a single name; let-else binds payload fields).
                if let Some(else_blk) = else_block {
                    if let Some(value_expr) = value {
                        if let Some(stmt) =
                            self.lower_let_else(pattern, value_expr, else_blk, &span)
                        {
                            return stmt;
                        }
                    }
                    // Fail-closed: a let-else with no initialiser, or whose
                    // pattern could not be resolved, lowers to an unsupported
                    // marker (diagnostics already pushed by lower_let_else).
                    return HirStmt {
                        node: self.ids.node(),
                        kind: HirStmtKind::Expr(
                            self.unsupported_expr(span.clone(), "let-else lowering"),
                        ),
                        span,
                    };
                }
                if let (Pattern::Wildcard, Some(value_expr)) = (&pattern.0, value.as_ref()) {
                    return HirStmt {
                        node: self.ids.node(),
                        kind: self.lower_expression_stmt_kind(value_expr),
                        span,
                    };
                }
                // Forward-bind for actor-lambda RHS. When the value is
                // `actor |params| { body }` and the let-pattern is a bare
                // identifier, the body may reference its own let-name for
                // recursive self-dispatch (HEW-SPEC §5.9 ratification 2).
                // Pre-bind the name BEFORE lowering the body so the body's
                // identifier reference resolves to a
                // `ResolvedRef::Binding(let_id)` rather than `Unresolved`.
                // The capture-strength classifier in `lower_expr`'s
                // `Expr::SpawnLambdaActor` arm checks the resolved id
                // against this let's id to discriminate Weak (self) from
                // Strong (every other free-variable capture).
                //
                // The pre-bind fires for both untyped and typed lets. The
                // binding type follows the annotation when present (the
                // type-checker layer reconciles the annotation against the
                // lambda's synthesised handle shape) and falls back to the
                // synthetic `actor(Msg) -> Reply` derived from the lambda's
                // parameter / return annotations otherwise.
                if let (
                    Pattern::Identifier(name),
                    Some((
                        Expr::SpawnLambdaActor {
                            params: lambda_params,
                            return_type,
                            ..
                        },
                        _,
                    )),
                ) = (&pattern.0, value.as_ref())
                {
                    let binding_ty = match ty.as_ref() {
                        Some(annotation) => self.lower_type(annotation),
                        None => self.actor_lambda_handle_ty(lambda_params, return_type.as_ref()),
                    };
                    // Pre-bind in the current scope; record the id so
                    // we can detect a self-reference inside the body
                    // walk via builder state.
                    let pre_binding =
                        self.bind(name.to_string(), binding_ty, false, pattern.1.clone());
                    let prior = self
                        .current_actor_self
                        .replace((pre_binding.id, name.to_string()));
                    let lowered_value = self.lower_expr(
                        value.as_ref().expect("value Some checked above"),
                        IntentKind::Consume,
                    );
                    self.current_actor_self = prior;
                    return HirStmt {
                        node: self.ids.node(),
                        kind: HirStmtKind::Let(pre_binding, Some(lowered_value)),
                        span,
                    };
                }
                let value = value
                    .as_ref()
                    .map(|expr| self.lower_expr(expr, IntentKind::Consume));
                let binding_ty = ty.as_ref().map_or_else(
                    || {
                        value
                            .as_ref()
                            .map_or(ResolvedTy::Unit, |expr| expr.ty.clone())
                    },
                    |ty| self.lower_type(ty),
                );
                let name = self
                    .pattern_name(pattern)
                    .unwrap_or_else(|| "_".to_string());
                let binding = self.bind(name, binding_ty, false, pattern.1.clone());
                HirStmtKind::Let(binding, value)
            }
            Stmt::Var { name, ty, value } => {
                let value = value
                    .as_ref()
                    .map(|expr| self.lower_expr(expr, IntentKind::Consume));
                let binding_ty = ty.as_ref().map_or_else(
                    || {
                        value
                            .as_ref()
                            .map_or(ResolvedTy::Unit, |expr| expr.ty.clone())
                    },
                    |ty| self.lower_type(ty),
                );
                let binding = self.bind(name.to_string(), binding_ty, true, span.clone());
                HirStmtKind::Let(binding, value)
            }
            Stmt::Assign { target, op, value } => {
                if let Some(assignment) = self.lower_index_assignment(target, *op, value, &span) {
                    assignment
                } else if let Some(op) = op {
                    self.lower_compound_assignment(target, *op, value, &span)
                } else {
                    let first_store = self
                        .actor_init_first_stores
                        .contains(&self.mk_key(&target.1));
                    let target = self.lower_expr(target, IntentKind::Modify);
                    let value = self.lower_expr(value, IntentKind::Consume);
                    HirStmtKind::Assign {
                        target,
                        value: Box::new(value),
                        first_store,
                    }
                }
            }
            Stmt::Expression(expr) => self.lower_expression_stmt_kind(expr),
            Stmt::Return(value) => {
                let return_ty = self.current_return_type.clone().unwrap_or(return_ty);
                if let Some(value) = value {
                    let expr = self.lower_expr(value, IntentKind::Consume);
                    let expr = self.apply_result_return_coercion(expr, &span);
                    // TI-5 escape check: a `Task<T>` value must not escape via
                    // return, whether the type was user-written or inferred. The
                    // `lower_type` wall blocks user-written `Task<T>` annotations;
                    // this check closes the inferred-escape path.
                    if matches!(expr.ty, ResolvedTy::Task(_)) {
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::TaskCannotEscape,
                            value.1.clone(),
                            "a `Task<T>` handle cannot escape via `return`; \
                             await it inside the `scope{}` body with `await name`",
                        ));
                    } else if expr.ty != return_ty
                        && return_ty != ResolvedTy::Unit
                        && !(expr.ty.to_ty().contains_callable()
                            && hew_types::unify::coerce(
                                &mut hew_types::ty::Substitution::new(),
                                &return_ty.to_ty(),
                                &expr.ty.to_ty(),
                            )
                            .is_ok())
                    {
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::ReturnTypeMismatch {
                                expected: return_ty,
                                actual: expr.ty.clone(),
                            },
                            value.1.clone(),
                            "return expression type differs from function return annotation",
                        ));
                    }
                    HirStmtKind::Return(Some(expr))
                } else {
                    let value = self
                        .result_return_coercions
                        .contains_key(&self.mk_key(&span))
                        .then(|| {
                            let value = self.make_unit_expr(span.clone());
                            self.apply_result_return_coercion(value, &span)
                        });
                    HirStmtKind::Return(value)
                }
            }
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                // Lower a statement-position `if` to `HirStmtKind::Expr` so
                // that sub-expression walkers (e.g. emit-cycle detection) can
                // descend into the branches.  `else if` chains are not yet
                // wired; they fall through to Unsupported to stay fail-closed.
                let lowered_condition = self.lower_expr(condition, IntentKind::Read);
                let then_hir_block = self.lower_block(then_block, &ResolvedTy::Unit);
                let then_ty = then_hir_block.ty.clone();
                let then_expr = HirExpr {
                    node: self.ids.node(),
                    site: self.ids.site(),
                    ty: then_ty.clone(),
                    intent: IntentKind::Read,
                    kind: HirExprKind::Block(then_hir_block),
                    span: span.clone(),
                };
                let else_expr = else_block.as_ref().and_then(|eb| {
                    if eb.is_if {
                        // `else if` chain: lower the nested Stmt::If recursively
                        // so the walker can descend into it. The inner stmt always
                        // lowers to HirStmtKind::Expr; any other outcome means the
                        // inner if hit an unsupported path and already emitted a
                        // diagnostic, so we fall back to an unsupported placeholder.
                        if let Some(inner) = &eb.if_stmt {
                            let inner_hir =
                                self.lower_stmt(&inner.0, inner.1.clone(), ResolvedTy::Unit);
                            match inner_hir.kind {
                                HirStmtKind::Expr(expr) => Some(Box::new(expr)),
                                _ => Some(Box::new(self.unsupported_expr(
                                    inner.1.clone(),
                                    "else-if chain produced non-expression HIR",
                                ))),
                            }
                        } else {
                            None
                        }
                    } else if let Some(block) = &eb.block {
                        let hir_block = self.lower_block(block, &ResolvedTy::Unit);
                        let else_ty = hir_block.ty.clone();
                        Some(Box::new(HirExpr {
                            node: self.ids.node(),
                            site: self.ids.site(),
                            ty: else_ty.clone(),
                            intent: IntentKind::Read,
                            kind: HirExprKind::Block(hir_block),
                            span: span.clone(),
                        }))
                    } else {
                        None
                    }
                });
                let if_ty = if_branch_result_ty(&then_ty, else_expr.as_ref().map(|e| &e.ty));
                let if_ty = self.callable_join_type(&span, if_ty);
                let if_expr = HirExpr {
                    node: self.ids.node(),
                    site: self.ids.site(),
                    ty: if_ty.clone(),
                    intent: IntentKind::Read,
                    kind: HirExprKind::If {
                        condition: Box::new(lowered_condition),
                        then_expr: Box::new(then_expr),
                        else_expr,
                    },
                    span: span.clone(),
                };
                HirStmtKind::Expr(if_expr)
            }
            Stmt::While {
                label,
                condition,
                body,
            } => {
                // `while cond { body }` — lowered to a HIR `While` expression
                // so that MIR can build the header/body/exit CFG shape.
                let cond_hir = self.lower_expr(condition, IntentKind::Read);
                let body_block = self.lower_block(body, &ResolvedTy::Unit);
                let while_expr = HirExpr {
                    node: self.ids.node(),
                    site: self.ids.site(),
                    ty: ResolvedTy::Unit,
                    intent: IntentKind::Read,
                    kind: HirExprKind::While {
                        label: label.map(|ident| ident.to_string()),
                        condition: Box::new(cond_hir),
                        body: body_block,
                    },
                    span: span.clone(),
                };
                HirStmtKind::Expr(while_expr)
            }
            Stmt::WhileLet {
                label,
                conditions,
                body,
            } => {
                // `while <condition> { body }` with a `let` operand is a bare
                // loop whose every iteration re-evaluates the condition and
                // breaks on the fallthrough arm. `break` and `continue` inside
                // `body` target this loop, and the arm body's scope gives
                // per-iteration defer cleanup.
                let body_span = span.clone();
                let match_expr = self.lower_condition_chain(
                    conditions,
                    body,
                    &body_span,
                    ConditionFallthrough::Break,
                    &ResolvedTy::Unit,
                    &span,
                );
                let loop_body = HirBlock {
                    node: self.ids.node(),
                    scope: self.ids.scope(),
                    statements: vec![HirStmt {
                        node: self.ids.node(),
                        kind: HirStmtKind::Expr(match_expr),
                        span: span.clone(),
                    }],
                    tail: None,
                    ty: ResolvedTy::Unit,
                    span: span.clone(),
                };
                let loop_expr = HirExpr {
                    node: self.ids.node(),
                    site: self.ids.site(),
                    ty: ResolvedTy::Unit,
                    intent: IntentKind::Read,
                    kind: HirExprKind::Loop {
                        label: label.map(|ident| ident.to_string()),
                        body: loop_body,
                    },
                    span: span.clone(),
                };
                HirStmtKind::Expr(loop_expr)
            }
            Stmt::For {
                label,
                pattern,
                iterable,
                body,
            } => {
                // Lower `for pat in iterable { body }`.
                // Only `Range`-typed iterables are supported in this slice:
                // `Expr::Range { start, end, inclusive }` where the iterable
                // expression is syntactically a range literal.  For all other
                // iterable shapes (Vec, Stream, HashMap, trait-Iterator, etc.)
                // a `NotYetImplemented` diagnostic is emitted and the statement
                // is replaced with an `Unsupported` placeholder so the pipeline
                // fails closed without fabricating a value.
                //
                // Only simple identifier patterns (`for i in …`) are supported;
                // tuple or struct patterns over a range are rejected the same
                // way.
                // Peel any `(a..b).rev()` / `.step_by(k)` adapter chain off the
                // iterable down to its base range literal.  A plain `a..b`
                // iterable peels to itself with no adapters.  The checker has
                // already validated that `rev`/`step_by` are `Range<T>` methods
                // (returning `Range<T>`), so an adapter chain that survives here
                // is a real strided/descending range — gated below on the
                // checker-recorded `Range<T>` type at the iterable span so a
                // user-defined `rev`/`step_by` on a non-range type does not get
                // misread as a range adapter.
                let peeled = Self::peel_range_adapter_chain(&iterable.0);
                let kind = match (peeled, &pattern.0) {
                    (Some(spec), Pattern::Identifier(_) | Pattern::Wildcard)
                        if spec.step_before_rev =>
                    {
                        let binding_name = match &pattern.0 {
                            Pattern::Identifier(var_name) => var_name.to_string(),
                            Pattern::Wildcard => {
                                format!("__hew_for_wildcard_{}", self.ids.binding().0)
                            }
                            _ => unreachable!("range-for arm only matches identifiers or wildcard"),
                        };
                        // `.step_by(k)` before `.rev()` does not commute with the
                        // supported `.rev()`-then-`.step_by(k)` order and cannot
                        // be expressed by the order-insensitive ForRange fold
                        // (the descending counter would start at the raw high
                        // bound instead of the last strided element, silently
                        // emitting a wrong sequence).  Reject fail-closed rather
                        // than miscompile; the user can write the supported
                        // order `(a..b).rev().step_by(k)`.
                        self.unsupported(
                            iterable.1.clone(),
                            "`step_by` before `rev` is unsupported; \
                             write `(a..b).rev().step_by(k)`",
                            "for-range-adapter-lowering",
                        );
                        // Bind the loop variable (the range element is an
                        // integer; the checker already typed it `Range<T>`) and
                        // lower the body so its diagnostics still flow and the
                        // body does not cascade into spurious unresolved-symbol
                        // errors for the loop variable.  The primary diagnostic
                        // above is the actionable one.
                        self.push_scope();
                        let _ = self.bind(binding_name, ResolvedTy::I64, false, pattern.1.clone());
                        let _ = self.lower_block(body, &ResolvedTy::Unit);
                        self.pop_scope();
                        HirExprKind::Unsupported(
                            "for-in over `(a..b).step_by(k).rev()` (unsupported adapter order)"
                                .into(),
                        )
                    }
                    (Some(spec), Pattern::Identifier(_) | Pattern::Wildcard) => {
                        let binding_name = match &pattern.0 {
                            Pattern::Identifier(var_name) => var_name.to_string(),
                            Pattern::Wildcard => {
                                format!("__hew_for_wildcard_{}", self.ids.binding().0)
                            }
                            _ => unreachable!("range-for arm only matches identifiers or wildcard"),
                        };
                        let RangeAdapterChain {
                            range_start,
                            range_end,
                            inclusive,
                            descending,
                            step_expr,
                            step_before_rev: _,
                        } = spec;

                        // Lower start and end expressions first so their
                        // checker-resolved types are available for the loop
                        // variable binding below.
                        let start_hir = self.lower_expr(range_start, IntentKind::Read);
                        let end_hir = self.lower_expr(range_end, IntentKind::Read);

                        // Derive the loop-variable element type from the
                        // checker-authoritative Range<T> type recorded on the
                        // iterable expression.  This is the single source of
                        // truth: the checker already chose the common integer
                        // width (e.g. `i64` for a `i32..i64` range) and stored
                        // `Range<i64>` at the iterable span.  For an adapter
                        // chain the outer iterable span is the outermost
                        // `MethodCall`, which the checker also typed `Range<T>`
                        // (the adapters return `Range<T>`), so the same lookup
                        // recovers the element width.  Reading it here avoids the
                        // "prefer-start-bound" heuristic that picked the wrong
                        // width when the two bounds had different signed widths.
                        //
                        // WHY: the old heuristic (start_hir.ty then end_hir.ty)
                        //   gave the wrong answer for mixed-width bounds such as
                        //   `a: i32 .. b: i64` — it chose `i32` but the checker
                        //   resolved `Range<i64>`.  The LLVM verifier then
                        //   rejected `call i64 @fn(i32 %arg)`.
                        // WHEN-OBSOLETE: this IS the real solution — no further
                        //   phase is needed unless Range semantics change.
                        // WHAT (real solution): read the checker-recorded
                        //   `Range<T>` at the iterable span and extract `T`.
                        let elem_ty = {
                            let range_key = self.mk_key(&iterable.1);
                            self.expr_types
                                .get(&range_key)
                                .and_then(|ty| ty.as_range().cloned())
                                .and_then(|inner| ResolvedTy::from_ty(&inner).ok())
                                .filter(Self::resolved_is_integer)
                                // Fallback: derive from the lowered bound types.
                                // Covers cases where the iterable span is absent
                                // (e.g. an expression that produces a Range but
                                // is not a syntactic range literal).
                                .or_else(|| {
                                    if Self::resolved_is_integer(&start_hir.ty) {
                                        Some(start_hir.ty.clone())
                                    } else if Self::resolved_is_integer(&end_hir.ty) {
                                        Some(end_hir.ty.clone())
                                    } else {
                                        None
                                    }
                                })
                                .unwrap_or(ResolvedTy::I64)
                        };

                        // Lower the stride expression (or synthesise a literal
                        // `1` at the element type for a non-strided range) before
                        // binding the loop variable, so a captured `step_by(n)`
                        // resolves against the enclosing scope.
                        let step_hir = match step_expr {
                            Some(step) => self.lower_expr(step, IntentKind::Read),
                            None => self.make_int_literal(1, elem_ty.clone(), iterable.1.clone()),
                        };

                        // Bind the loop variable inside a fresh scope so it is
                        // scoped to the body but visible during body lowering.
                        self.push_scope();
                        let binding = self.bind(binding_name, elem_ty, false, pattern.1.clone());
                        let body_block = self.lower_block(body, &ResolvedTy::Unit);
                        self.pop_scope();

                        HirExprKind::ForRange {
                            label: label.map(|ident| ident.to_string()),
                            binding,
                            start: Box::new(start_hir),
                            end: Box::new(end_hir),
                            inclusive,
                            step: Box::new(step_hir),
                            descending,
                            body: body_block,
                        }
                    }
                    (None, _) => self.lower_for_iter_desugar(
                        pattern,
                        iterable,
                        body,
                        label.map(|label| label.to_string()).as_ref(),
                        span.clone(),
                    ),
                    _ => {
                        // Non-identifier range pattern: not supported in this slice.
                        self.unsupported(
                            pattern.1.clone(),
                            "for-range with non-identifier binding pattern",
                            "for-while-lowering",
                        );
                        self.push_scope();
                        let _ = self.lower_block(body, &ResolvedTy::Unit);
                        self.pop_scope();
                        HirExprKind::Unsupported("for-range with non-identifier pattern".into())
                    }
                };

                let for_expr = HirExpr {
                    node: self.ids.node(),
                    site: self.ids.site(),
                    ty: ResolvedTy::Unit,
                    intent: IntentKind::Read,
                    kind,
                    span: span.clone(),
                };
                HirStmtKind::Expr(for_expr)
            }
            Stmt::Match { scrutinee, arms } => {
                // `match scrut { ... }` in statement position. The parser
                // produces `Stmt::Match` distinct from `Stmt::Expression(Expr::Match)`,
                // so we route through the same `lower_match_expr` builder used by
                // expression-position match and wrap the result as a Unit-typed
                // statement-expression. The match's result type is discarded —
                // a statement-position match's value is unused.
                let (kind, ty) = self.lower_match_expr(scrutinee, arms, &span);
                let match_expr = HirExpr {
                    node: self.ids.node(),
                    site: self.ids.site(),
                    ty,
                    intent: IntentKind::Read,
                    kind,
                    span: span.clone(),
                };
                HirStmtKind::Expr(match_expr)
            }
            Stmt::Defer(body_expr) => {
                let body = self.lower_expr(body_expr, IntentKind::Read);
                HirStmtKind::Defer {
                    body: Box::new(body),
                    scope_id: self.current_scope_id,
                }
            }
            Stmt::Loop { label, body } => {
                // Bare `loop { body }` — lowered to a HIR `Loop` expression so
                // MIR can build the body/exit CFG shape with an unconditional
                // back-edge and a `break`-targeted exit block.
                //
                // Type rule: if no `break` statement can reach this loop's exit
                // (break-less infinite loop), the expression never produces a
                // value — its type is `Never`.  If at least one `break` exists,
                // control can exit normally and the type is `Unit`.  This allows
                // a break-less loop in if/match branch position to unify with
                // the other branch's type instead of forcing the whole
                // expression to `Unit`.
                let loop_ty = if hew_parser::loop_body_has_break(body, *label) {
                    ResolvedTy::Unit
                } else {
                    ResolvedTy::Never
                };
                let body_block = self.lower_block(body, &ResolvedTy::Unit);
                let loop_expr = HirExpr {
                    node: self.ids.node(),
                    site: self.ids.site(),
                    ty: loop_ty,
                    intent: IntentKind::Read,
                    kind: HirExprKind::Loop {
                        label: label.map(|ident| ident.to_string()),
                        body: body_block,
                    },
                    span: span.clone(),
                };
                HirStmtKind::Expr(loop_expr)
            }
            Stmt::Break { label, value } => {
                // `break;` / `break @label;` / `break <value>;` — early exit
                // from the innermost enclosing loop, or from the nearest
                // enclosing loop with the requested label. The type checker has
                // already rejected out-of-loop breaks and unknown labels; MIR
                // keeps a defense-in-depth diagnostic for malformed HIR.
                // Carry the operand of `break <value>` so MIR can lower it for
                // its side effects (and move-checker correctness) before the
                // jump. Loop-as-expression value return is out of scope; the
                // produced value is discarded downstream (LESSONS
                // `cleanup-all-exits`).
                let value_hir = value
                    .as_ref()
                    .map(|v| Box::new(self.lower_expr(v, IntentKind::Read)));
                let break_expr = HirExpr {
                    node: self.ids.node(),
                    site: self.ids.site(),
                    ty: ResolvedTy::Unit,
                    intent: IntentKind::Read,
                    kind: HirExprKind::Break {
                        label: label.map(|ident| ident.to_string()),
                        value: value_hir,
                    },
                    span: span.clone(),
                };
                HirStmtKind::Expr(break_expr)
            }
            Stmt::Continue { label } => {
                // `continue;` / `continue @label;` — next iteration of the
                // innermost enclosing loop, or of the nearest enclosing loop
                // with the requested label.
                let continue_expr = HirExpr {
                    node: self.ids.node(),
                    site: self.ids.site(),
                    ty: ResolvedTy::Unit,
                    intent: IntentKind::Read,
                    kind: HirExprKind::Continue {
                        label: label.map(|ident| ident.to_string()),
                    },
                    span: span.clone(),
                };
                HirStmtKind::Expr(continue_expr)
            }
            Stmt::IfLet {
                conditions,
                body,
                else_body,
            } => {
                // A pattern condition in statement position — result type is
                // always Unit. The chain lowers to nested matches and boolean
                // branches; `lower_condition_chain` is the one authority.
                let body_span = span.clone();
                let if_let_expr = self.lower_condition_chain(
                    conditions,
                    body,
                    &body_span,
                    ConditionFallthrough::Else(else_body.as_deref()),
                    &ResolvedTy::Unit,
                    &span,
                );
                HirStmtKind::Expr(if_let_expr)
            }
        };
        HirStmt {
            node: self.ids.node(),
            kind,
            span,
        }
    }

    /// Lower `let PAT = scrutinee else { <divergent block> };` through the
    /// same pattern authority as `match`/`if let`/`while let`: desugar to
    /// `let tmp = match scrutinee { PAT => <bindings>, _ => <else block> };`
    /// then destructure `tmp` into fresh bindings that escape into the
    /// enclosing scope. `<bindings>` is `PAT`'s bound names packed by
    /// [`Self::pack_arm_bindings`] — `Unit` for none, the value directly for
    /// one, a name-ordered tuple for more — and the destructure step (a plain
    /// `Let` or `Destructure` statement) unpacks that shape back into the
    /// escaping names. The checker has already proven the else block
    /// diverges (`Ty::Never`), so `pattern_conditional_match`'s wildcard arm
    /// never falls through to a continuation that could see an unbound
    /// binder.
    ///
    /// Returns `Some(HirStmt)` on success, `None` on a fail-closed error
    /// (diagnostics already pushed by `lower_pattern_arms`).
    pub(super) fn lower_let_else(
        &mut self,
        pattern: &Spanned<Pattern>,
        scrutinee_expr: &Spanned<Expr>,
        else_block: &Block,
        span: &Span,
    ) -> Option<HirStmt> {
        let scrutinee_hir = self.lower_expr(scrutinee_expr, IntentKind::Read);
        self.try_register_enum_instantiation(&scrutinee_expr.1);

        let pattern_span = pattern.1.clone();
        let arms: Vec<PatternArm<'_>> = flatten_or_pattern(pattern)
            .into_iter()
            .map(|leaf| PatternArm {
                pattern: leaf,
                guard: None,
                body: PatternArmBody::Bindings(pattern_span.clone()),
            })
            .collect();

        // `block_result_ty` only matters to `PatternArmBody::Condition`; our
        // synthesized arms carry `Bindings`, which ignores it.
        let Some((hir_arms, result_ty)) =
            self.lower_pattern_arms(&scrutinee_hir, &arms, &ResolvedTy::Unit)
        else {
            let _ = self.lower_block(else_block, &ResolvedTy::Unit);
            return None;
        };

        // `lower_pattern_arms`'s inference skips `Unit`-typed arms, which is
        // exactly the "no bindings" shape here, so the fallback is correct.
        let packed_ty = result_ty.unwrap_or(ResolvedTy::Unit);

        // Every or-pattern leaf binds the same names (the checker requires
        // it for the shared body to type-check), so any arm's expanded
        // binding list names the escaping shape; sort by name to match
        // `pack_arm_bindings`. The arm's aggregate-destructure prelude (if
        // any) rode along as the leading statements of its wrapped body.
        let arm0_prelude: &[HirStmt] = match &hir_arms[0].body.kind {
            HirExprKind::Block(block) => &block.statements,
            _ => &[],
        };
        let mut escapees: Vec<(String, ResolvedTy, Span)> = expand_arm_bindings(
            &hir_arms[0].bindings,
            &hir_arms[0].payload_variant_predicates,
            arm0_prelude,
        )
        .into_iter()
        .map(|(name, _, ty, span)| (name, ty, span))
        .collect();
        escapees.sort_by(|a, b| a.0.cmp(&b.0));

        let else_hir_block = self.lower_block(else_block, &packed_ty);
        let else_ty = else_hir_block.ty.clone();
        let fallthrough = HirExpr {
            node: self.ids.node(),
            site: self.ids.site(),
            ty: else_ty,
            intent: IntentKind::Read,
            kind: HirExprKind::Block(else_hir_block),
            span: span.clone(),
        };

        let match_expr =
            self.pattern_conditional_match(scrutinee_hir, hir_arms, fallthrough, &packed_ty, span);

        let kind = match escapees.len() {
            0 => HirStmtKind::Expr(match_expr),
            1 => {
                let (name, ty, binding_span) =
                    escapees.into_iter().next().expect("checked len == 1");
                let bound = self.bind(name, ty, false, binding_span);
                HirStmtKind::Let(bound, Some(match_expr))
            }
            _ => {
                let fields = escapees
                    .into_iter()
                    .enumerate()
                    .map(|(idx, (name, ty, binding_span))| HirDestructureField {
                        selector: HirDestructureSelector::Tuple(
                            u32::try_from(idx).expect("let-else binding count exceeds u32::MAX"),
                        ),
                        binding: Some(self.bind(name, ty, false, binding_span)),
                        nested: false,
                    })
                    .collect();
                HirStmtKind::Destructure {
                    value: match_expr,
                    fields,
                }
            }
        };

        Some(HirStmt {
            node: self.ids.node(),
            kind,
            span: span.clone(),
        })
    }

    /// An indexed write is the mutation selected by the checker at its target
    /// span. Reuse ordinary collection-call lowering so replacement ownership
    /// and argument evaluation agree with the method spelling.
    pub(super) fn lower_index_assignment(
        &mut self,
        target: &Spanned<Expr>,
        op: Option<CompoundAssignOp>,
        value: &Spanned<Expr>,
        span: &Span,
    ) -> Option<HirStmtKind> {
        let key = self.mk_key(&target.1);
        if self.assign_target_kinds.get(&key) != Some(&AssignTargetKind::Index) {
            return None;
        }
        let Expr::Index { object, index } = &target.0 else {
            return None;
        };
        let family = self
            .resolved_calls
            .get(&key)
            .and_then(|resolved| match resolved.target {
                CallTarget::RuntimeCollection(method) => method.runtime_family(),
                _ => None,
            });
        if family.is_none() && op.is_none() {
            return None;
        }
        let value_ty = self.resolved_expr_types.get(&key)?.clone();
        let mut receiver = self.lower_expr(object, IntentKind::Read);
        let index = self.lower_expr(index, IntentKind::Read);
        let replacement = self.lower_expr(value, IntentKind::Read);
        let mut statements = Vec::new();
        let (index, replacement) = if let Some(op) = op {
            // The read and write share one evaluated key. The receiver remains
            // a place, taken by SIR only after argument evaluation succeeds.
            let (capture, read_key, key_ref) = self.capture_assignment_index(index);
            let mut read_receiver = self.lower_expr(object, IntentKind::Read);
            self.capture_compound_place_indices(&mut read_receiver, &mut receiver, &mut statements);
            statements.push(capture);
            let read = self.make_expr(
                HirExprKind::Index {
                    container: Box::new(read_receiver),
                    index: Box::new(read_key),
                },
                value_ty.clone(),
                IntentKind::Read,
                target.1.clone(),
            );
            let updated = self.make_expr(
                HirExprKind::Binary {
                    op: Self::compound_assign_binary_op(op),
                    left: Box::new(read),
                    right: Box::new(replacement),
                },
                value_ty.clone(),
                IntentKind::Read,
                span.clone(),
            );
            (key_ref, updated)
        } else {
            (index, replacement)
        };
        let assignment = if let Some(family) = family {
            let kind = self.collection_call_kind(
                family,
                vec![receiver, index, replacement],
                &ResolvedTy::Unit,
                span,
            );
            HirStmtKind::Expr(self.make_expr(
                kind,
                ResolvedTy::Unit,
                IntentKind::Read,
                span.clone(),
            ))
        } else {
            let target = self.make_expr(
                HirExprKind::Index {
                    container: Box::new(receiver),
                    index: Box::new(index),
                },
                value_ty,
                IntentKind::Modify,
                target.1.clone(),
            );
            HirStmtKind::Assign {
                target,
                value: Box::new(replacement),
                first_store: false,
            }
        };
        Some(self.assignment_with_prelude(statements, assignment, span))
    }

    pub(super) fn assignment_with_prelude(
        &mut self,
        mut statements: Vec<HirStmt>,
        assignment: HirStmtKind,
        span: &Span,
    ) -> HirStmtKind {
        if statements.is_empty() {
            return assignment;
        }
        statements.push(HirStmt {
            node: self.ids.node(),
            kind: assignment,
            span: span.clone(),
        });
        let block = HirBlock {
            node: self.ids.node(),
            scope: self.ids.scope(),
            statements,
            tail: None,
            ty: ResolvedTy::Unit,
            span: span.clone(),
        };
        HirStmtKind::Expr(self.make_expr(
            HirExprKind::Block(block),
            ResolvedTy::Unit,
            IntentKind::Read,
            span.clone(),
        ))
    }

    /// Bind a compound assignment's key once and return distinct HIR reads.
    pub(super) fn capture_assignment_index(
        &mut self,
        index: HirExpr,
    ) -> (HirStmt, HirExpr, HirExpr) {
        self.push_scope();
        let name = format!("__hew_assignment_key_{}", self.ids.binding().0);
        let binding = self.bind(name.clone(), index.ty.clone(), false, index.span.clone());
        self.pop_scope();
        let read = self.binding_ref_expr(
            name.clone(),
            binding.id,
            binding.ty.clone(),
            index.span.clone(),
        );
        let write = self.binding_ref_expr(name, binding.id, binding.ty.clone(), index.span.clone());
        let statement = HirStmt {
            node: self.ids.node(),
            span: index.span.clone(),
            kind: HirStmtKind::Let(binding, Some(index)),
        };
        (statement, read, write)
    }

    pub(super) fn lower_compound_assignment(
        &mut self,
        target: &Spanned<Expr>,
        op: CompoundAssignOp,
        value: &Spanned<Expr>,
        span: &Span,
    ) -> HirStmtKind {
        let binary_op = Self::compound_assign_binary_op(op);
        let mut target_read = self.lower_expr(target, IntentKind::Read);
        let rhs = self.lower_expr(value, IntentKind::Read);
        let mut target_write = self.lower_expr(target, IntentKind::Modify);
        let mut prelude = Vec::new();
        self.capture_compound_place_indices(&mut target_read, &mut target_write, &mut prelude);
        let value = HirExpr {
            node: self.ids.node(),
            site: self.ids.site(),
            ty: target_read.ty.clone(),
            intent: IntentKind::Consume,
            kind: HirExprKind::Binary {
                op: binary_op,
                left: Box::new(target_read),
                right: Box::new(rhs),
            },
            span: span.clone(),
        };
        let assignment = HirStmtKind::Assign {
            target: target_write,
            value: Box::new(value),
            first_store: false,
        };
        self.assignment_with_prelude(prelude, assignment, span)
    }

    /// Stabilize the existing read/write projection pair without deciding
    /// whether that place is writable; SIR retains that authority.
    pub(super) fn capture_compound_place_indices(
        &mut self,
        read: &mut HirExpr,
        write: &mut HirExpr,
        prelude: &mut Vec<HirStmt>,
    ) {
        match (&mut read.kind, &mut write.kind) {
            (
                HirExprKind::Index {
                    container: read,
                    index: read_index,
                },
                HirExprKind::Index {
                    container: write,
                    index: write_index,
                },
            ) => {
                self.capture_compound_place_indices(read, write, prelude);
                let (capture, key_read, key_write) =
                    self.capture_assignment_index((**read_index).clone());
                **read_index = key_read;
                **write_index = key_write;
                prelude.push(capture);
            }
            (
                HirExprKind::FieldAccess { object: read, .. },
                HirExprKind::FieldAccess { object: write, .. },
            )
            | (
                HirExprKind::TupleIndex { tuple: read, .. },
                HirExprKind::TupleIndex { tuple: write, .. },
            )
            | (
                HirExprKind::SubsumedValue { source: read },
                HirExprKind::SubsumedValue { source: write },
            ) => {
                self.capture_compound_place_indices(read, write, prelude);
            }
            _ => {}
        }
    }

    pub(super) fn compound_assign_binary_op(op: CompoundAssignOp) -> BinaryOp {
        match op {
            CompoundAssignOp::Add => BinaryOp::Add,
            CompoundAssignOp::Subtract => BinaryOp::Subtract,
            CompoundAssignOp::Multiply => BinaryOp::Multiply,
            CompoundAssignOp::Divide => BinaryOp::Divide,
            CompoundAssignOp::Modulo => BinaryOp::Modulo,
            CompoundAssignOp::BitAnd => BinaryOp::BitAnd,
            CompoundAssignOp::BitOr => BinaryOp::BitOr,
            CompoundAssignOp::BitXor => BinaryOp::BitXor,
            CompoundAssignOp::Shl => BinaryOp::Shl,
            CompoundAssignOp::Shr => BinaryOp::Shr,
        }
    }

    /// Turn an inclusive slice bound into the exclusive one HIR carries.
    ///
    /// `xs[a..=b]` is `xs[a..b + 1]`; the added checked arithmetic traps on
    /// overflow, and the bounds check downstream is then the single one the
    /// exclusive form already performs.
    pub(super) fn exclusive_slice_bound(&mut self, bound: HirExpr, span: &Span) -> HirExpr {
        let ty = bound.ty.clone();
        let one = HirExpr {
            node: self.ids.node(),
            site: self.ids.site(),
            ty: ty.clone(),
            intent: IntentKind::Read,
            kind: HirExprKind::Literal(HirLiteral::Integer(1)),
            span: span.clone(),
        };
        HirExpr {
            node: self.ids.node(),
            site: self.ids.site(),
            ty,
            intent: IntentKind::Read,
            kind: HirExprKind::Binary {
                op: BinaryOp::Add,
                left: Box::new(bound),
                right: Box::new(one),
            },
            span: span.clone(),
        }
    }
}
