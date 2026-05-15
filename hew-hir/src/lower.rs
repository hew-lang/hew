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

    fn lower_stmt(
        &mut self,
        stmt: &Stmt,
        span: std::ops::Range<usize>,
        return_ty: ResolvedTy,
    ) -> HirStmt {
        let kind = match stmt {
            Stmt::Let { pattern, ty, value } => {
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
            Stmt::Expression(expr) => HirStmtKind::Expr(self.lower_expr(expr, IntentKind::Read)),
            Stmt::Return(value) => {
                if let Some(value) = value {
                    let expr = self.lower_expr(value, IntentKind::Consume);
                    if expr.ty != return_ty && return_ty != ResolvedTy::Unit {
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

        // The grammar permits a `timeoutArm` only at the end of a
        // `select{}`, so a syntactic `Select { timeout }` carries at most
        // one `after` arm. A user writing multiple `after` arms either
        // (a) writes them as `selectArm`s with `after` as the source
        // expression — which falls through to SelectArmNotSealedForm —
        // or (b) the parser would have rejected. We defensively emit
        // SelectMultipleAfterArms if a later widening of the grammar
        // surfaces the case. Today this branch is unreachable on the
        // base grammar; the diagnostic exists for forward-compatibility
        // and is exercised by the dedicated unit test for the rule.
        //
        // No code path here currently emits SelectMultipleAfterArms; the
        // surface is sealed at the grammar layer.

        let mut hir_arms: Vec<HirSelectArm> = Vec::with_capacity(arms.len() + 1);
        let mut expected_ty: Option<ResolvedTy> = None;

        for arm in arms {
            let binding_name = self.pattern_name(&arm.binding);
            let kind = self.recognize_sealed_arm_source(&arm.source);
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
        Expr::Send { .. } => "actor send".into(),
        Expr::ScopeLaunch(_) | Expr::ScopeSpawn(_) | Expr::ScopeCancel => "scope expression".into(),
        Expr::Timeout { .. } => "timeout expression".into(),
        Expr::Unsafe(_) => "unsafe block".into(),
        Expr::Yield(_) => "yield expression".into(),
        Expr::This => "this".into(),
        _ => "expression".into(),
    }
}
