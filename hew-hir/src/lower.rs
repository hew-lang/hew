use std::collections::HashMap;

use hew_parser::ast::{
    BinaryOp, Block, Expr, FnDecl, Item, Literal, Pattern, Program, Spanned, Stmt, TypeExpr,
};
use hew_types::ResolvedTy;

use crate::diagnostic::{HirDiagnostic, HirDiagnosticKind};
use crate::ids::{BindingId, IdGen, ItemId, ResolvedRef};
use crate::node::{
    HirBinding, HirBlock, HirExpr, HirExprKind, HirFn, HirItem, HirLiteral, HirModule, HirStmt,
    HirStmtKind,
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

    // Second pass: lower function bodies with full signature knowledge.
    let mut items = Vec::new();
    for (item, span) in &program.items {
        match item {
            Item::Function(func) => {
                items.push(HirItem::Function(ctx.lower_fn(func, span.clone())));
            }
            _ => ctx.unsupported(span.clone(), "top-level-item", "slice-2"),
        }
    }

    LowerOutput {
        module: HirModule { items },
        diagnostics: ctx.diagnostics,
    }
}

#[derive(Debug, Default)]
struct LowerCtx {
    ids: IdGen,
    scopes: Vec<HashMap<String, (BindingId, ResolvedTy)>>,
    /// Maps function name → pre-allocated `ItemId` + return type + param types.
    fn_registry: HashMap<String, FnEntry>,
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
            Expr::StructInit { name, fields } => {
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
            value_class: ValueClass::of_ty(&ty),
            ty,
            intent,
            kind,
            span,
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
                    "i32" | "int" => ResolvedTy::I32,
                    "i64" => ResolvedTy::I64,
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
