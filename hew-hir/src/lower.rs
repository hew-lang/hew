use std::collections::HashMap;

use hew_parser::ast::{
    BinaryOp, Block, Expr, FnDecl, Item, Literal, Pattern, Program, ResourceMarker, SelectArm,
    Spanned, Stmt, TimeoutClause, TypeBodyItem, TypeDecl, TypeExpr,
};
use hew_types::ResolvedTy;

use crate::diagnostic::{HirDiagnostic, HirDiagnosticKind};
use crate::ids::{BindingId, IdGen, ItemId, ResolvedRef};
use crate::node::{
    HirBinding, HirBlock, HirExpr, HirExprKind, HirField, HirFn, HirItem, HirLiteral, HirModule,
    HirSelect, HirSelectArm, HirSelectArmKind, HirStmt, HirStmtKind, HirTypeDecl,
};
use crate::{IntentKind, ValueClass};

#[derive(Debug, Clone, Default)]
pub struct ResolutionCtx;

#[derive(Debug, Clone, PartialEq)]
pub struct LowerOutput {
    pub module: HirModule,
    pub diagnostics: Vec<HirDiagnostic>,
}

/// Pre-collected signature of a top-level function item.
#[derive(Debug)]
struct FnEntry {
    id: ItemId,
    return_ty: ResolvedTy,
    param_tys: Vec<ResolvedTy>,
}

#[must_use]
pub fn lower_program(program: &Program, _ctx: &ResolutionCtx) -> LowerOutput {
    let mut ctx = LowerCtx::default();

    // First pass: collect all function signatures so that forward and mutual
    // references in call expressions resolve to the correct return type.
    // Diagnostics from this pass are discarded — the same types are re-lowered
    // in the second pass, which is where canonical diagnostics are emitted.
    for (item, _) in &program.items {
        if let Item::Function(func) = item {
            let id = ctx.ids.item();
            let return_ty = func
                .return_type
                .as_ref()
                .map_or(ResolvedTy::Unit, |ty| ctx.lower_type(ty));
            let param_tys = func.params.iter().map(|p| ctx.lower_type(&p.ty)).collect();
            ctx.fn_registry.insert(
                func.name.clone(),
                FnEntry {
                    id,
                    return_ty,
                    param_tys,
                },
            );
        }
    }
    // Discard first-pass diagnostics; the second pass will re-emit any real ones.
    ctx.diagnostics.clear();

    // Second pass: lower type declarations and populate the per-module
    // type-class registry. Stored here so the source-order pass can emit them
    // in program order without a second lowering call. Function bodies depend
    // on type markers (so `ValueClass::of_ty` resolves `Named` types
    // correctly), but type-decl bodies do not depend on function signatures,
    // so this pre-pass can safely run before the combined item pass below.
    let mut type_decl_cache: HashMap<*const hew_parser::ast::TypeDecl, HirTypeDecl> =
        HashMap::new();
    for (item, span) in &program.items {
        if let Item::TypeDecl(decl) = item {
            let hir_decl = ctx.lower_type_decl(decl, span.clone());
            let close_method = if hir_decl.marker == ResourceMarker::Resource {
                hir_decl
                    .consuming_methods
                    .iter()
                    .find(|m| m.as_str() == "close")
                    .cloned()
            } else {
                None
            };
            ctx.type_classes
                .insert(hir_decl.name.clone(), (hir_decl.marker, close_method));
            type_decl_cache.insert(decl as *const _, hir_decl);
        }
    }

    // Third pass: emit all items in source order now that both fn signatures
    // and type markers are fully resolved.
    let mut items: Vec<HirItem> = Vec::new();
    for (item, span) in &program.items {
        match item {
            Item::TypeDecl(decl) => {
                // Retrieve the already-lowered decl so diagnostics are not
                // emitted a second time.
                if let Some(hir_decl) = type_decl_cache.remove(&(decl as *const _)) {
                    items.push(HirItem::TypeDecl(hir_decl));
                }
            }
            Item::Function(func) => {
                items.push(HirItem::Function(ctx.lower_fn(func, span.clone())));
            }
            _ => ctx.unsupported(span.clone(), "top-level-item", "slice-2"),
        }
    }

    LowerOutput {
        module: HirModule {
            items,
            type_classes: ctx.type_classes,
        },
        diagnostics: ctx.diagnostics,
    }
}

#[derive(Debug, Default)]
struct LowerCtx {
    ids: IdGen,
    scopes: Vec<HashMap<String, (BindingId, ResolvedTy)>>,
    /// Maps function name → pre-allocated `ItemId` + return type + param types.
    fn_registry: HashMap<String, FnEntry>,
    /// Per-named-type marker + close-method registry. Pre-populated from
    /// every `Item::TypeDecl` before function bodies lower so that
    /// `ValueClass::of_ty` can resolve `Named` types as the body is walked.
    type_classes: crate::value_class::TypeClassTable,
    diagnostics: Vec<HirDiagnostic>,
    /// Depth counter for nested `fork{}` bodies. When > 0, statement-expression
    /// calls are inferred as child-task spawns (TI-1); outside any fork body
    /// all calls are synchronous (TI-3). Using a depth counter rather than a
    /// bool supports nested `fork{}` blocks correctly.
    fork_depth: u32,
    /// Set to `true` immediately before lowering the expression of a
    /// `Stmt::Expression` statement. `lower_expr` consumes it via
    /// `mem::replace(…, false)` at entry, so all recursive calls see `false`.
    /// This lets `Expr::Await` check whether it is the direct statement, not
    /// a sub-expression of a return value, argument, binary operand, etc.
    /// (TI-4 position rule.)
    statement_position: bool,
}

impl LowerCtx {
    fn lower_fn(&mut self, func: &FnDecl, span: std::ops::Range<usize>) -> HirFn {
        // Use the stable ItemId pre-allocated during the first pass.
        let id = self
            .fn_registry
            .get(&func.name)
            .map_or_else(|| self.ids.item(), |entry| entry.id);

        self.push_scope();
        let mut params = Vec::new();
        for param in &func.params {
            let ty = self.lower_type(&param.ty);
            let binding = self.bind(param.name.clone(), ty, param.is_mutable, param.ty.1.clone());
            params.push(binding);
        }

        let return_ty = func
            .return_type
            .as_ref()
            .map_or(ResolvedTy::Unit, |ty| self.lower_type(ty));
        let body = self.lower_block(&func.body, &return_ty);
        self.pop_scope();

        HirFn {
            id,
            node: self.ids.node(),
            name: func.name.clone(),
            type_params: func
                .type_params
                .as_ref()
                .map(|params| params.iter().map(|param| param.name.clone()).collect())
                .unwrap_or_default(),
            params,
            return_ty,
            body,
            span,
        }
    }

    fn lower_type_decl(&mut self, decl: &TypeDecl, span: std::ops::Range<usize>) -> HirTypeDecl {
        // Generic resource/linear types are rejected — the type→class map is
        // keyed by name, not by instantiation. This rule belongs at the
        // checker boundary (LESSONS `checker-output-boundary`); HIR is the
        // first place the marker is durable, so the check lands here.
        if decl.resource_marker != ResourceMarker::None && decl.type_params.is_some() {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::ResourceGenericUnsupported {
                    name: decl.name.clone(),
                },
                span.clone(),
                "`#[resource]` / `#[linear]` types cannot have type parameters in v0.5",
            ));
        }

        match decl.resource_marker {
            ResourceMarker::Resource => {
                // `#[resource]` must declare `close(consuming self)`.
                let has_close = decl
                    .consuming_methods
                    .iter()
                    .any(|name| name.as_str() == "close");
                if !has_close {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::ResourceMissingClose {
                            name: decl.name.clone(),
                        },
                        span.clone(),
                        "`#[resource]` type must declare `fn close(consuming self) -> ...` \
                         in its body; the implicit drop contract dispatches to this method",
                    ));
                }
            }
            ResourceMarker::Linear => {
                // `#[linear]` must declare at least one consuming method.
                if decl.consuming_methods.is_empty() {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::LinearNoConsumingMethods {
                            name: decl.name.clone(),
                        },
                        span.clone(),
                        "`#[linear]` type must declare at least one `consuming self` method; \
                         without one no exit path could exhaust a binding of this type",
                    ));
                }
            }
            ResourceMarker::None => {}
        }

        // Carry the field set so dump-hir and future analysis have something
        // to reason about; methods are out of scope for v0.5 MIR lowering
        // and stay on the parser-side `TypeBodyItem::Method`.
        let mut fields = Vec::new();
        for item in &decl.body {
            if let TypeBodyItem::Field {
                name,
                ty,
                span: field_span,
                ..
            } = item
            {
                fields.push(HirField {
                    name: name.clone(),
                    ty: self.lower_type(ty),
                    span: field_span.clone(),
                });
            }
            // `TypeBodyItem::Method` is not lowered into HIR in v0.5 — the
            // method-call expression form has no HIR/MIR lowering yet, so
            // method bodies cannot be exercised. Their *declared names* are
            // captured upstream as `TypeDecl.consuming_methods` and travel
            // on `HirTypeDecl.consuming_methods`. `TypeBodyItem::Variant`
            // belongs to enum bodies and is similarly out of slice scope.
        }

        HirTypeDecl {
            id: self.ids.item(),
            node: self.ids.node(),
            name: decl.name.clone(),
            marker: decl.resource_marker,
            consuming_methods: decl.consuming_methods.clone(),
            fields,
            span,
        }
    }

    fn lower_block(&mut self, block: &Block, expected_ty: &ResolvedTy) -> HirBlock {
        self.push_scope();
        let scope = self.ids.scope();
        let mut statements = Vec::new();
        for (stmt, span) in &block.stmts {
            statements.push(self.lower_stmt(stmt, span.clone(), expected_ty.clone()));
        }
        let tail = block
            .trailing_expr
            .as_ref()
            .map(|expr| Box::new(self.lower_expr(expr, IntentKind::Read)));
        let ty = tail
            .as_ref()
            .map_or(ResolvedTy::Unit, |expr| expr.ty.clone());
        self.pop_scope();

        HirBlock {
            node: self.ids.node(),
            scope,
            statements,
            tail,
            ty,
            span: 0..0,
        }
    }

    #[allow(
        clippy::too_many_lines,
        reason = "single large match on stmt variants; splitting would hurt readability"
    )]
    fn lower_stmt(
        &mut self,
        stmt: &Stmt,
        span: std::ops::Range<usize>,
        return_ty: ResolvedTy,
    ) -> HirStmt {
        let kind = match stmt {
            Stmt::Let { pattern, ty, value } => {
                // `await expr` is only legal as a statement-expression inside a
                // fork{} body (TI-4). Using it as a let-value is always rejected —
                // the await result is consumed immediately and has no place to bind.
                if let Some(val_expr) = value {
                    if matches!(&val_expr.0, Expr::Await(_)) {
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::AwaitOutOfPosition,
                            val_expr.1.clone(),
                            "`await` cannot be used as a let-value; \
                             it is only legal as a statement-expression inside a `fork{}` body",
                        ));
                        let name = self
                            .pattern_name(pattern)
                            .unwrap_or_else(|| "_".to_string());
                        let binding_ty = ty
                            .as_ref()
                            .map_or(ResolvedTy::Unit, |ty| self.lower_type(ty));
                        let binding = self.bind(name, binding_ty, false, pattern.1.clone());
                        let unsupported =
                            self.unsupported_expr(val_expr.1.clone(), "`await` in let-value");
                        return HirStmt {
                            node: self.ids.node(),
                            kind: HirStmtKind::Let(binding, Some(unsupported)),
                            span,
                        };
                    }
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
                let binding = self.bind(name.clone(), binding_ty, true, span.clone());
                HirStmtKind::Let(binding, value)
            }
            Stmt::Expression(expr) => {
                // Inside a fork{} body, statement-expression calls are child-task
                // spawns (TI-1). Outside fork{} bodies all calls are synchronous
                // (TI-3). The TI-1 rewrite only applies when the expression is a
                // direct call — nested calls inside sub-expressions remain sync.
                //
                // Mark this as statement position before lowering so that
                // `lower_expr`'s `Expr::Await` arm can enforce TI-4 (await is
                // only legal in statement-expression position, not as a
                // sub-expression). The flag is consumed by `mem::replace` at the
                // top of `lower_expr`, so recursive calls see `false`.
                self.statement_position = true;
                if self.fork_depth > 0 {
                    if let Expr::Call { .. } = &expr.0 {
                        let spawned = self.lower_spawned_call(expr);
                        HirStmtKind::Expr(spawned)
                    } else {
                        HirStmtKind::Expr(self.lower_expr(expr, IntentKind::Read))
                    }
                } else {
                    HirStmtKind::Expr(self.lower_expr(expr, IntentKind::Read))
                }
            }
            Stmt::Return(value) => {
                if let Some(value) = value {
                    let expr = self.lower_expr(value, IntentKind::Consume);
                    // TI-5 escape check: a `Task<T>` value must not escape via
                    // return, whether the type was user-written or inferred. The
                    // `lower_type` wall blocks user-written `Task<T>` annotations;
                    // this check closes the inferred-escape path.
                    if matches!(expr.ty, ResolvedTy::Task(_)) {
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::TaskCannotEscape,
                            value.1.clone(),
                            "a `Task<T>` handle cannot escape via `return`; \
                             await it inside the `fork{}` body with `await name`",
                        ));
                    } else if expr.ty != return_ty && return_ty != ResolvedTy::Unit {
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
                    HirStmtKind::Return(None)
                }
            }
            _ => {
                self.unsupported(span.clone(), "statement", "slice-2");
                HirStmtKind::Expr(self.unsupported_expr(span.clone(), "unsupported statement"))
            }
        };
        HirStmt {
            node: self.ids.node(),
            kind,
            span,
        }
    }

    #[allow(
        clippy::too_many_lines,
        reason = "single large match on expr variants; splitting would hurt readability"
    )]
    fn lower_expr(&mut self, expr: &Spanned<Expr>, intent: IntentKind) -> HirExpr {
        // Consume the statement-position flag atomically. Every recursive call
        // to `lower_expr` (for arguments, operands, return values, block tails,
        // etc.) therefore sees `false`. Only the `Stmt::Expression` arm in
        // `lower_stmt` sets this to `true` immediately before calling us.
        let in_stmt_position = std::mem::replace(&mut self.statement_position, false);
        let span = expr.1.clone();
        let (kind, ty) = match &expr.0 {
            Expr::Literal(lit) => Self::lower_literal(lit),
            Expr::Identifier(name) => self.lower_identifier(name, span.clone()),
            Expr::Binary { left, op, right } => {
                let left = self.lower_expr(left, IntentKind::Read);
                let right = self.lower_expr(right, IntentKind::Read);
                let ty = Self::binary_ty(*op, &left.ty, &right.ty);
                (
                    HirExprKind::Binary {
                        op: *op,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    ty,
                )
            }
            Expr::Call { function, args, .. } => {
                let callee = self.lower_expr(function, IntentKind::Read);
                let args = args
                    .iter()
                    .map(|arg| self.lower_expr(arg.expr(), IntentKind::Read))
                    .collect();
                // Determine the call result type from the callee's function type.
                // If the callee is unresolved, the result type is an inference hole.
                let result_ty = if let ResolvedTy::Function { ret, .. } = &callee.ty {
                    *ret.clone()
                } else {
                    if matches!(
                        callee.kind,
                        HirExprKind::BindingRef {
                            resolved: ResolvedRef::Unresolved,
                            ..
                        }
                    ) {
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::UnresolvedInferenceVar,
                            span.clone(),
                            "call result type cannot be determined: callee is unresolved",
                        ));
                    }
                    ResolvedTy::Unit
                };
                (
                    HirExprKind::Call {
                        callee: Box::new(callee),
                        args,
                    },
                    result_ty,
                )
            }
            Expr::Block(block) => {
                let block = self.lower_block(block, &ResolvedTy::Unit);
                let ty = block.ty.clone();
                (HirExprKind::Block(block), ty)
            }
            Expr::If {
                condition,
                then_block,
                else_block,
            } => {
                let condition = self.lower_expr(condition, IntentKind::Read);
                let then_expr = self.lower_expr(then_block, IntentKind::Read);
                let else_expr = else_block
                    .as_ref()
                    .map(|expr| Box::new(self.lower_expr(expr, IntentKind::Read)));
                let ty = else_expr
                    .as_ref()
                    .map_or(ResolvedTy::Unit, |expr| expr.ty.clone());
                (
                    HirExprKind::If {
                        condition: Box::new(condition),
                        then_expr: Box::new(then_expr),
                        else_expr,
                    },
                    ty,
                )
            }
            Expr::StructInit {
                name,
                fields,
                // The v0.5 vertical-slice HIR lowering does not yet honour
                // explicit type arguments at struct literal sites; the
                // slice rejects user types at the MIR boundary anyway, so
                // dropping the args here cannot widen an accepted program.
                type_args: _,
            } => {
                let fields = fields
                    .iter()
                    .map(|(name, expr)| (name.clone(), self.lower_expr(expr, IntentKind::Read)))
                    .collect();
                (
                    HirExprKind::StructInit {
                        name: name.clone(),
                        type_args: Vec::new(),
                        fields,
                    },
                    ResolvedTy::Named {
                        name: name.clone(),
                        args: Vec::new(),
                    },
                )
            }
            Expr::Fork { body } => {
                // A `fork{}` block lowers to `HirExprKind::Fork`. Inside the
                // body, statement-calls become spawned-call nodes (TI-1) and
                // `fork name = call(...)` statements introduce `Task<T>` bindings
                // (TI-2). The fork block's type is `Unit` — it does not produce
                // a value at the use site.
                self.fork_depth += 1;
                let hir_body = self.lower_fork_block(body);
                self.fork_depth -= 1;
                (HirExprKind::Fork { body: hir_body }, ResolvedTy::Unit)
            }
            Expr::ForkChild { binding, expr } => {
                // `fork name = expr` outside a `fork{}` body: no spawn context,
                // so this is malformed. Emit CutoverUnsupported — the grammar
                // accepts this form but HIR-lowering requires fork context.
                // (Inside fork{} bodies this variant is handled by lower_fork_block,
                // not by lower_expr directly.)
                if self.fork_depth == 0 {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::AwaitOutOfPosition,
                        span.clone(),
                        "`fork name = expr` is only valid inside a `fork{}` body",
                    ));
                    (
                        HirExprKind::Unsupported(
                            "`fork name = expr` outside fork body".to_string(),
                        ),
                        ResolvedTy::Unit,
                    )
                } else {
                    // Inside a fork body, lower_fork_block handles this case;
                    // reaching here means the expression appeared in a non-statement
                    // position (e.g. tail expression). Reject: task handles cannot
                    // be used as values.
                    let _ = binding;
                    let _ = expr;
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::AwaitOutOfPosition,
                        span.clone(),
                        "`fork name = expr` must be a statement, not an expression value",
                    ));
                    (
                        HirExprKind::Unsupported("`fork name = expr` as expression".to_string()),
                        ResolvedTy::Unit,
                    )
                }
            }
            Expr::Await(inner) => {
                // `await expr` — only legal as the direct statement-expression
                // inside a `fork{}` body in v0.5 (TI-4). Sub-expression positions
                // (return value, function argument, binary operand, let value, block
                // tail, etc.) are rejected with `AwaitOutOfPosition`.
                // `in_stmt_position` is set by `Stmt::Expression` in `lower_stmt`
                // and consumed by `mem::replace` at the top of this function, so
                // recursive calls always see `false`.
                if self.fork_depth == 0 || !in_stmt_position {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::AwaitOutOfPosition,
                        span.clone(),
                        "`await` is only legal as a statement-expression inside a `fork{}` body \
                         in v0.5. It cannot be used as a return value, function argument, \
                         binary operand, or let-value. Move the await to its own statement.",
                    ));
                    return HirExpr {
                        node: self.ids.node(),
                        site: self.ids.site(),
                        value_class: ValueClass::BitCopy,
                        ty: ResolvedTy::Unit,
                        intent,
                        kind: HirExprKind::Unsupported("`await` out of position".to_string()),
                        span,
                    };
                }
                // Resolve the inner expression. It must be a binding-ref with a
                // `Task<T>` type to be awaitable.
                let inner_hir = self.lower_expr(inner, IntentKind::Consume);
                match &inner_hir.ty {
                    ResolvedTy::Task(output_ty) => {
                        let output_ty = *output_ty.clone();
                        // Extract the binding name and id for the AwaitTask node.
                        // The inner expression must be a direct binding-ref; await
                        // on a complex expression is not supported in v0.5.
                        if let HirExprKind::BindingRef {
                            name: binding_name,
                            resolved: ResolvedRef::Binding(binding_id),
                        } = &inner_hir.kind
                        {
                            let (binding_name, binding_id) = (binding_name.clone(), *binding_id);
                            let value_class = ValueClass::of_ty(&output_ty, &self.type_classes);
                            return HirExpr {
                                node: self.ids.node(),
                                site: self.ids.site(),
                                value_class,
                                ty: output_ty.clone(),
                                intent,
                                kind: HirExprKind::AwaitTask {
                                    binding_name,
                                    binding_id,
                                    output_ty,
                                },
                                span,
                            };
                        }
                        // Await on a non-binding-ref Task<T>: reject. The form
                        // `await (some_expr)` where the expr is not a name is not
                        // supported — only named bindings can be awaited.
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::AwaitOutOfPosition,
                            span.clone(),
                            "`await` requires a named task binding, not an expression",
                        ));
                        (
                            HirExprKind::Unsupported("`await` on non-binding-ref task".to_string()),
                            ResolvedTy::Unit,
                        )
                    }
                    found_ty => {
                        // The operand is not a Task<T> — reject with AwaitNonTask.
                        let found_ty = found_ty.clone();
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::AwaitNonTask {
                                found_ty: found_ty.clone(),
                            },
                            span.clone(),
                            "`await` requires a task handle (`Task<T>`). \
                             Hint: did you mean to bind a task with `fork name = call(...)` first?",
                        ));
                        (
                            HirExprKind::Unsupported("`await` on non-task".to_string()),
                            ResolvedTy::Unit,
                        )
                    }
                }
            }
            Expr::Select { arms, timeout } => {
                self.lower_select(arms, timeout.as_deref(), span.clone())
            }
            _ => {
                self.unsupported(span.clone(), "expression", "slice-2");
                (
                    HirExprKind::Unsupported("unsupported expression".into()),
                    ResolvedTy::Unit,
                )
            }
        };
        HirExpr {
            node: self.ids.node(),
            site: self.ids.site(),
            value_class: ValueClass::of_ty(&ty, &self.type_classes),
            ty,
            intent,
            kind,
            span,
        }
    }

    /// Lower a parsed `select { ... }` expression to HIR.
    ///
    /// Per HEW-SPEC-2026 §4.11.1 the four arm forms are exhaustive:
    ///   1. `pat from next(<stream-expr>) => body`
    ///   2. `pat from <actor-expr>.<method>(<args>) => body`   (actor ask)
    ///   3. `pat from await <task-expr> => body`
    ///   4. `after <duration-expr> => body`                    (timer)
    ///
    /// Any other arm source shape is rejected with
    /// `SelectArmNotSealedForm`. Body-type disagreement is rejected
    /// with `SelectArmTypeMismatch`. Empty selects and multiple-after
    /// arms are rejected with `SelectNoArms` and
    /// `SelectMultipleAfterArms` respectively.
    fn lower_select(
        &mut self,
        arms: &[SelectArm],
        timeout: Option<&TimeoutClause>,
        span: std::ops::Range<usize>,
    ) -> (HirExprKind, ResolvedTy) {
        // Empty select — neither arms nor a timer — fires nothing.
        if arms.is_empty() && timeout.is_none() {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::SelectNoArms,
                span.clone(),
                "select expression contains no arms",
            ));
            return (
                HirExprKind::Unsupported("empty select".into()),
                ResolvedTy::Unit,
            );
        }

        // Multiple `after` arms have no meaningful join semantics and are
        // rejected. An `Expr::Timeout`-sourced arm in the `arms` vec is
        // treated as an `AfterTimer` arm; if that is combined with the
        // dedicated `timeout` field (or if two appear in `arms`) the
        // second one triggers `SelectMultipleAfterArms`.

        let mut hir_arms: Vec<HirSelectArm> = Vec::with_capacity(arms.len() + 1);
        let mut expected_ty: Option<ResolvedTy> = None;
        let mut first_after_span: Option<std::ops::Range<usize>> = None;

        for arm in arms {
            let binding_name = self.pattern_name(&arm.binding);
            let kind = self.recognize_sealed_arm_source(&arm.source);
            if matches!(kind, HirSelectArmKind::AfterTimer { .. }) {
                if first_after_span.is_some() {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::SelectMultipleAfterArms,
                        arm.source.1.clone(),
                        "select may have at most one `after` arm",
                    ));
                } else {
                    first_after_span = Some(arm.source.1.clone());
                }
            }
            let body = self.lower_expr(&arm.body, IntentKind::Read);
            if let Some(expected) = expected_ty.as_ref() {
                if &body.ty != expected {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::SelectArmTypeMismatch {
                            arm_index: hir_arms.len(),
                            expected: expected.clone(),
                            actual: body.ty.clone(),
                        },
                        arm.body.1.clone(),
                        "select arm body type differs from the first arm body type",
                    ));
                }
            } else {
                expected_ty = Some(body.ty.clone());
            }
            hir_arms.push(HirSelectArm {
                kind,
                binding_name,
                body,
            });
        }

        if let Some(timeout) = timeout {
            if first_after_span.is_some() {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::SelectMultipleAfterArms,
                    timeout.duration.1.clone(),
                    "select may have at most one `after` arm",
                ));
            }
            let duration = self.lower_expr(&timeout.duration, IntentKind::Read);
            let body = self.lower_expr(&timeout.body, IntentKind::Read);
            if let Some(expected) = expected_ty.as_ref() {
                if &body.ty != expected {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::SelectArmTypeMismatch {
                            arm_index: hir_arms.len(),
                            expected: expected.clone(),
                            actual: body.ty.clone(),
                        },
                        timeout.body.1.clone(),
                        "select after-arm body type differs from earlier arm body types",
                    ));
                }
            } else {
                expected_ty = Some(body.ty.clone());
            }
            hir_arms.push(HirSelectArm {
                kind: HirSelectArmKind::AfterTimer {
                    duration: Box::new(duration),
                },
                binding_name: None,
                body,
            });
        }

        let result_ty = expected_ty.unwrap_or(ResolvedTy::Unit);
        (HirExprKind::Select(HirSelect { arms: hir_arms }), result_ty)
    }

    /// Recognise the sealed-form discriminator for a `select` arm
    /// source expression. Emits a `SelectArmNotSealedForm` /
    /// `SelectStreamNextSurface` / `SelectStreamNextArity` diagnostic
    /// on miss and returns a placeholder `AfterTimer` arm kind (the
    /// callers tolerate the placeholder because the diagnostic has
    /// already been emitted; MIR lowering treats any select with HIR
    /// diagnostics as fail-closed downstream).
    fn recognize_sealed_arm_source(&mut self, source: &Spanned<Expr>) -> HirSelectArmKind {
        let span = source.1.clone();
        match &source.0 {
            // Form 1: `next(<stream-expr>)` — a call where the callee
            // is the bare identifier `next`. `next` is not a lexer
            // keyword; the sealed-form discriminator is the callee
            // name.
            Expr::Call { function, args, .. } => {
                if let Expr::Identifier(name) = &function.0 {
                    if name == "next" {
                        if args.len() != 1 {
                            self.diagnostics.push(HirDiagnostic::new(
                                HirDiagnosticKind::SelectStreamNextArity {
                                    arg_count: args.len(),
                                },
                                span.clone(),
                                "next(<stream>) takes exactly one argument",
                            ));
                            return HirSelectArmKind::StreamNext {
                                stream: Box::new(
                                    self.unsupported_expr(span, "stream-next arity mismatch"),
                                ),
                            };
                        }
                        let stream = self.lower_expr(args[0].expr(), IntentKind::Read);
                        return HirSelectArmKind::StreamNext {
                            stream: Box::new(stream),
                        };
                    }
                }
                // Some other function call — not a sealed form.
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::SelectArmNotSealedForm {
                        source_shape: "function call".into(),
                    },
                    span.clone(),
                    "select arm source must be next(...), an actor method call, or `await <task>`",
                ));
                HirSelectArmKind::StreamNext {
                    stream: Box::new(self.unsupported_expr(span, "non-sealed arm source")),
                }
            }
            // Form 2: `<actor>.<method>(<args>)` — method call on an
            // actor expression. Per HEW-SPEC-2026 §4.11.1 this is the
            // actor-ask arm. `ask` is reserved as a future syntactic
            // marker (see HEW-FUTURE) but is not lexer-recognised in
            // edition 2026; the sealed-form discriminator is the
            // method-call surface.
            Expr::MethodCall {
                receiver,
                method,
                args,
            } => {
                let actor = self.lower_expr(receiver, IntentKind::Read);
                let lowered_args: Vec<HirExpr> = args
                    .iter()
                    .map(|arg| self.lower_expr(arg.expr(), IntentKind::Read))
                    .collect();
                HirSelectArmKind::ActorAsk {
                    actor: Box::new(actor),
                    method: method.clone(),
                    args: lowered_args,
                }
            }
            // Form 3: `await <task-expr>` — explicit await keyword.
            Expr::Await(task_expr) => {
                let task = self.lower_expr(task_expr, IntentKind::Read);
                HirSelectArmKind::TaskAwait {
                    task: Box::new(task),
                }
            }
            // Form 4 (arm-position): `after <duration>` written as an
            // arm source rather than the dedicated `timeout` field.
            // Recognised here so the `lower_select` multiple-after check
            // can fire; the duplicate check in `lower_select` emits the
            // diagnostic when this arm coexists with another after arm.
            Expr::Timeout { duration, .. } => {
                let dur = self.lower_expr(duration, IntentKind::Read);
                HirSelectArmKind::AfterTimer {
                    duration: Box::new(dur),
                }
            }
            // Method-call dressed up as `stream.next()` — sealed
            // surface is `next(stream)`. Diagnose specifically so the
            // user can fix the form.
            // (Already handled by the MethodCall arm above as a
            // generic actor-ask. The dedicated diagnostic for the
            // `.next()` shape would shadow the actor-ask recognition;
            // we keep the more general actor-ask interpretation and
            // rely on the SelectStreamNextSurface diagnostic only if
            // we later choose to special-case it. For now, `s.next()`
            // is recognised as an actor-ask of the `next` method on
            // `s`, which is the lower-noise default.)
            other => {
                let shape = describe_select_source_shape(other);
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::SelectArmNotSealedForm {
                        source_shape: shape,
                    },
                    span.clone(),
                    "select arm source must be next(...), an actor method call, or `await <task>`",
                ));
                HirSelectArmKind::StreamNext {
                    stream: Box::new(self.unsupported_expr(span, "non-sealed arm source")),
                }
            }
        }
    }

    fn lower_literal(lit: &Literal) -> (HirExprKind, ResolvedTy) {
        match lit {
            Literal::Integer { value, .. } => (
                HirExprKind::Literal(HirLiteral::Integer(*value)),
                ResolvedTy::I64,
            ),
            Literal::Float(value) => (
                HirExprKind::Literal(HirLiteral::Float(*value)),
                ResolvedTy::F64,
            ),
            Literal::String(value) => (
                HirExprKind::Literal(HirLiteral::String(value.clone())),
                ResolvedTy::String,
            ),
            Literal::Bool(value) => (
                HirExprKind::Literal(HirLiteral::Bool(*value)),
                ResolvedTy::Bool,
            ),
            Literal::Char(value) => (
                HirExprKind::Literal(HirLiteral::Char(*value)),
                ResolvedTy::Char,
            ),
            Literal::Duration(value) => (
                HirExprKind::Literal(HirLiteral::Duration(*value)),
                ResolvedTy::Duration,
            ),
        }
    }

    fn lower_identifier(
        &mut self,
        name: &str,
        span: std::ops::Range<usize>,
    ) -> (HirExprKind, ResolvedTy) {
        if let Some((id, ty)) = self.lookup(name) {
            (
                HirExprKind::BindingRef {
                    name: name.to_string(),
                    resolved: ResolvedRef::Binding(id),
                },
                ty,
            )
        } else if let Some(entry) = self.fn_registry.get(name) {
            // Known function item — expose as a function-typed reference so
            // callers can extract the return type from the call expression.
            let fn_ty = ResolvedTy::Function {
                params: entry.param_tys.clone(),
                ret: Box::new(entry.return_ty.clone()),
            };
            let id = entry.id;
            (
                HirExprKind::BindingRef {
                    name: name.to_string(),
                    resolved: ResolvedRef::Item(id),
                },
                fn_ty,
            )
        } else {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::UnresolvedSymbol {
                    name: name.to_string(),
                },
                span,
                "identifier has no binding in resolved HIR",
            ));
            (
                HirExprKind::BindingRef {
                    name: name.to_string(),
                    resolved: ResolvedRef::Unresolved,
                },
                ResolvedTy::Unit,
            )
        }
    }

    fn lower_type(&mut self, ty: &Spanned<TypeExpr>) -> ResolvedTy {
        match &ty.0 {
            TypeExpr::Named { name, type_args } => {
                let args = type_args
                    .as_ref()
                    .map(|args| args.iter().map(|arg| self.lower_type(arg)).collect())
                    .unwrap_or_default();
                match name.as_str() {
                    "i8" => ResolvedTy::I8,
                    "i16" => ResolvedTy::I16,
                    "i32" => ResolvedTy::I32,
                    // `int` is the user-facing alias for `i64` across the
                    // type checker (hew-types::stdlib_loader maps `int` to
                    // Ty::I64; ty.rs's alias table lists `int` as a synonym
                    // for `i64`). The HIR lowering must match — otherwise
                    // integer literals (which lower to I64) cannot be
                    // returned through a function typed `int` without an
                    // explicit cast. Aligns with hew-types ground truth.
                    "i64" | "int" => ResolvedTy::I64,
                    "u8" => ResolvedTy::U8,
                    "u16" => ResolvedTy::U16,
                    "u32" => ResolvedTy::U32,
                    "u64" => ResolvedTy::U64,
                    "f32" => ResolvedTy::F32,
                    "f64" | "float" => ResolvedTy::F64,
                    "bool" | "Bool" => ResolvedTy::Bool,
                    "char" | "Char" => ResolvedTy::Char,
                    "String" | "str" => ResolvedTy::String,
                    "Bytes" => ResolvedTy::Bytes,
                    "Duration" => ResolvedTy::Duration,
                    "Unit" | "()" => ResolvedTy::Unit,
                    // `Task` is a compiler-internal value class with no user-source
                    // syntax. Writing `Task<T>` in any annotation position is a
                    // compile error. Use `fork name = call(...)` to obtain a task
                    // handle. (TI-5 structural enforcement.)
                    "Task" => {
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::TaskNotNameable,
                            ty.1.clone(),
                            "`Task<T>` is a compiler-internal type and cannot be written \
                             in source. Use `fork name = call(...)` to create a task handle.",
                        ));
                        ResolvedTy::Unit
                    }
                    _ => ResolvedTy::Named {
                        name: name.clone(),
                        args,
                    },
                }
            }
            TypeExpr::Infer => {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::UnresolvedInferenceVar,
                    ty.1.clone(),
                    "inferred type reached resolved HIR boundary",
                ));
                ResolvedTy::Unit
            }
            TypeExpr::Tuple(elems) => {
                ResolvedTy::Tuple(elems.iter().map(|elem| self.lower_type(elem)).collect())
            }
            TypeExpr::Array { element, size } => {
                ResolvedTy::Array(Box::new(self.lower_type(element)), *size)
            }
            TypeExpr::Slice(elem) => ResolvedTy::Slice(Box::new(self.lower_type(elem))),
            TypeExpr::Function {
                params,
                return_type,
            } => ResolvedTy::Function {
                params: params.iter().map(|param| self.lower_type(param)).collect(),
                ret: Box::new(self.lower_type(return_type)),
            },
            TypeExpr::Pointer {
                is_mutable,
                pointee,
            } => ResolvedTy::Pointer {
                is_mutable: *is_mutable,
                pointee: Box::new(self.lower_type(pointee)),
            },
            _ => {
                self.unsupported(ty.1.clone(), "type-expression", "slice-2");
                ResolvedTy::Unit
            }
        }
    }

    fn binary_ty(op: BinaryOp, left: &ResolvedTy, right: &ResolvedTy) -> ResolvedTy {
        match op {
            BinaryOp::Equal
            | BinaryOp::NotEqual
            | BinaryOp::Less
            | BinaryOp::LessEqual
            | BinaryOp::Greater
            | BinaryOp::GreaterEqual
            | BinaryOp::And
            | BinaryOp::Or => ResolvedTy::Bool,
            BinaryOp::Add if left == &ResolvedTy::String || right == &ResolvedTy::String => {
                ResolvedTy::String
            }
            _ => left.clone(),
        }
    }

    fn pattern_name(&mut self, pattern: &Spanned<Pattern>) -> Option<String> {
        if let Pattern::Identifier(name) = &pattern.0 {
            Some(name.clone())
        } else {
            self.unsupported(pattern.1.clone(), "pattern", "slice-2");
            None
        }
    }

    fn bind(
        &mut self,
        name: String,
        ty: ResolvedTy,
        mutable: bool,
        span: std::ops::Range<usize>,
    ) -> HirBinding {
        let id = self.ids.binding();
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.clone(), (id, ty.clone()));
        }
        HirBinding {
            id,
            name,
            ty,
            mutable,
            span,
        }
    }

    fn lookup(&self, name: &str) -> Option<(BindingId, ResolvedTy)> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).map(|(id, ty)| (*id, ty.clone())))
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn unsupported(
        &mut self,
        span: std::ops::Range<usize>,
        construct: impl Into<String>,
        slice_target: impl Into<String>,
    ) {
        self.diagnostics.push(HirDiagnostic::new(
            HirDiagnosticKind::CutoverUnsupported {
                construct: construct.into(),
                slice_target: slice_target.into(),
            },
            span,
            "",
        ));
    }

    fn unsupported_expr(
        &mut self,
        span: std::ops::Range<usize>,
        note: impl Into<String>,
    ) -> HirExpr {
        HirExpr {
            node: self.ids.node(),
            site: self.ids.site(),
            ty: ResolvedTy::Unit,
            value_class: ValueClass::BitCopy,
            intent: IntentKind::Unknown,
            kind: HirExprKind::Unsupported(note.into()),
            span,
        }
    }

    /// Lower the body block of a `fork{}` expression. This is separate from
    /// `lower_block` because statements inside a fork body follow different
    /// rules:
    ///
    /// - `Stmt::Expression(Expr::Call{..})` → `SpawnedCall` (TI-1)
    /// - `Stmt::Expression(Expr::ForkChild { binding: Some(name), expr })` →
    ///   `HirStmtKind::Let` with a `Task<T>` typed binding (TI-2)
    /// - `Stmt::Expression(Expr::Await(..))` → `AwaitTask` (TI-4)
    /// - All other statements lower normally, including nested `fork{}` blocks.
    ///
    /// The caller is responsible for setting `fork_depth` before calling this
    /// function and restoring it after.
    #[allow(
        clippy::too_many_lines,
        reason = "single match on fork-body statement variants; splitting would hurt readability"
    )]
    fn lower_fork_block(&mut self, block: &Block) -> HirBlock {
        self.push_scope();
        let scope = self.ids.scope();
        let mut statements = Vec::new();

        for (stmt, span) in &block.stmts {
            let hir_stmt = match stmt {
                // `fork name = call(...)` inside a fork body → TI-2: typed Task<T> binding.
                Stmt::Expression(expr)
                    if matches!(
                        &expr.0,
                        Expr::ForkChild {
                            binding: Some(_),
                            ..
                        }
                    ) =>
                {
                    if let Expr::ForkChild {
                        binding: Some(binding_name),
                        expr: child_expr,
                    } = &expr.0
                    {
                        // The child expression must be a call; any other form is rejected.
                        if matches!(&child_expr.0, Expr::Call { .. }) {
                            // Lower the call synchronously first to get the return type,
                            // then wrap in SpawnedCall + Task<T>.
                            let call_hir = self.lower_expr(child_expr, IntentKind::Consume);
                            let call_ret_ty = call_hir.ty.clone();
                            let task_ty = ResolvedTy::Task(Box::new(call_ret_ty));

                            // Destructure call_hir.kind once to extract callee + args.
                            let HirExprKind::Call { callee, args } = call_hir.kind else {
                                unreachable!("just verified Call shape above")
                            };

                            // Re-wrap as a SpawnedCall node with Task<T> type.
                            let spawned = HirExpr {
                                node: self.ids.node(),
                                site: self.ids.site(),
                                value_class: ValueClass::Linear, // Task handles are linear (consume-once).
                                ty: task_ty.clone(),
                                intent: IntentKind::Consume,
                                kind: HirExprKind::SpawnedCall {
                                    callee,
                                    args,
                                    task_ty: task_ty.clone(),
                                },
                                span: child_expr.1.clone(),
                            };

                            // Bind the name with Task<T> type in the current scope.
                            let binding =
                                self.bind(binding_name.clone(), task_ty, false, span.clone());
                            HirStmt {
                                node: self.ids.node(),
                                kind: HirStmtKind::Let(binding, Some(spawned)),
                                span: span.clone(),
                            }
                        } else {
                            self.diagnostics.push(HirDiagnostic::new(
                                HirDiagnosticKind::ForkChildNotACall,
                                child_expr.1.clone(),
                                "`fork name = expr` requires a call expression as the \
                                 right-hand side; other expression forms cannot be spawned as tasks",
                            ));
                            // Emit an unsupported stmt and continue rather than stopping.
                            self.unsupported(span.clone(), "fork-child-non-call", "slice-2");
                            HirStmt {
                                node: self.ids.node(),
                                kind: HirStmtKind::Expr(
                                    self.unsupported_expr(span.clone(), "fork child non-call"),
                                ),
                                span: span.clone(),
                            }
                        }
                    } else {
                        unreachable!("pattern guard ensures this branch")
                    }
                }

                // `fork name = call(...)` without a binding name (bare ForkChild with no
                // name, e.g. `fork { fork = expr }` — the grammar produces binding: None).
                // Lower the child expression as a plain SpawnedCall; the result is not bound.
                Stmt::Expression(expr)
                    if matches!(&expr.0, Expr::ForkChild { binding: None, .. }) =>
                {
                    if let Expr::ForkChild {
                        binding: None,
                        expr: child_expr,
                    } = &expr.0
                    {
                        if matches!(&child_expr.0, Expr::Call { .. }) {
                            let spawned = self.lower_spawned_call(child_expr);
                            HirStmt {
                                node: self.ids.node(),
                                kind: HirStmtKind::Expr(spawned),
                                span: span.clone(),
                            }
                        } else {
                            self.diagnostics.push(HirDiagnostic::new(
                                HirDiagnosticKind::ForkChildNotACall,
                                child_expr.1.clone(),
                                "`fork = expr` requires a call expression",
                            ));
                            self.unsupported(span.clone(), "fork-child-non-call", "slice-2");
                            HirStmt {
                                node: self.ids.node(),
                                kind: HirStmtKind::Expr(
                                    self.unsupported_expr(span.clone(), "fork child non-call"),
                                ),
                                span: span.clone(),
                            }
                        }
                    } else {
                        unreachable!("pattern guard ensures this branch")
                    }
                }

                // All other statements lower normally (including regular calls,
                // let bindings, nested fork{} blocks, etc.). Inside fork depth,
                // `lower_stmt` will already handle statement-expression calls as
                // SpawnedCall nodes via TI-1 (the fork_depth > 0 path in lower_stmt).
                _ => self.lower_stmt(stmt, span.clone(), ResolvedTy::Unit),
            };
            statements.push(hir_stmt);
        }

        let tail = block
            .trailing_expr
            .as_ref()
            .map(|expr| Box::new(self.lower_expr(expr, IntentKind::Read)));
        let ty = tail
            .as_ref()
            .map_or(ResolvedTy::Unit, |expr| expr.ty.clone());
        self.pop_scope();

        HirBlock {
            node: self.ids.node(),
            scope,
            statements,
            tail,
            ty,
            span: 0..0,
        }
    }

    /// Lower a call expression appearing as a statement inside a `fork{}` body
    /// as a child-task spawn (TI-1). The resulting `HirExpr` has kind
    /// `SpawnedCall` and type `Task<call_return_ty>`.
    fn lower_spawned_call(&mut self, expr: &Spanned<Expr>) -> HirExpr {
        let span = expr.1.clone();
        // Lower the call normally to resolve the callee and argument types.
        let call_hir = self.lower_expr(expr, IntentKind::Consume);
        let call_ret_ty = call_hir.ty.clone();
        let task_ty = ResolvedTy::Task(Box::new(call_ret_ty));

        let HirExprKind::Call { callee, args } = call_hir.kind else {
            // Should not happen: caller verified the expression is a Call.
            return self.unsupported_expr(span, "lower_spawned_call on non-call");
        };

        HirExpr {
            node: self.ids.node(),
            site: self.ids.site(),
            value_class: ValueClass::Linear, // Task handles are linear (consume-once).
            ty: task_ty.clone(),
            intent: IntentKind::Consume,
            kind: HirExprKind::SpawnedCall {
                callee,
                args,
                task_ty,
            },
            span,
        }
    }
}

/// One-token description of a parser `Expr` shape, used by
/// `SelectArmNotSealedForm` diagnostic notes. Intentionally coarse — the
/// goal is to tell the user "you wrote a literal where a sealed form
/// belongs", not to echo the expression back at them.
fn describe_select_source_shape(expr: &Expr) -> String {
    match expr {
        Expr::Literal(_) => "literal".into(),
        Expr::Identifier(_) => "identifier".into(),
        Expr::Binary { .. } => "binary expression".into(),
        Expr::Block(_) => "block".into(),
        Expr::If { .. } => "if expression".into(),
        Expr::Select { .. } => "nested select".into(),
        Expr::Join(_) => "join expression".into(),
        Expr::FieldAccess { .. } => "field access".into(),
        Expr::Index { .. } => "index expression".into(),
        Expr::Range { .. } => "range expression".into(),
        Expr::Cast { .. } => "cast expression".into(),
        Expr::ScopeLaunch(_) | Expr::ScopeSpawn(_) | Expr::ScopeCancel => "scope expression".into(),
        Expr::Timeout { .. } => "timeout expression".into(),
        Expr::Unsafe(_) => "unsafe block".into(),
        Expr::Yield(_) => "yield expression".into(),
        Expr::This => "this".into(),
        _ => "expression".into(),
    }
}
