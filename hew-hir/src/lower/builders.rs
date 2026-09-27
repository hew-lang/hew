//! Synthetic HIR expression builders.

use super::*;

impl LowerCtx {
    pub(super) fn make_expr(
        &mut self,
        kind: HirExprKind,
        ty: ResolvedTy,
        intent: IntentKind,
        span: Span,
    ) -> HirExpr {
        let site = self.ids.site();
        if let Some(operations) = self
            .checked_indexed_place_operations
            .get(&self.mk_key(&span))
        {
            self.indexed_place_operations.insert(site, *operations);
        }
        HirExpr {
            node: self.ids.node(),
            site,
            ty,
            intent,
            kind,
            span,
        }
    }

    /// Build a typed integer-literal HIR expression.  Used to synthesise the
    /// default stride `1` for a non-strided `ForRange` so MIR always sees a
    /// concrete step operand at the loop's element width.
    pub(super) fn make_int_literal(&mut self, value: i128, ty: ResolvedTy, span: Span) -> HirExpr {
        self.make_expr(
            HirExprKind::Literal(HirLiteral::Integer(value)),
            ty,
            IntentKind::Read,
            span,
        )
    }

    pub(super) fn make_binding_ref(
        &mut self,
        name: String,
        binding: BindingId,
        ty: ResolvedTy,
        intent: IntentKind,
        span: Span,
    ) -> HirExpr {
        self.make_expr(
            HirExprKind::BindingRef {
                name,
                resolved: ResolvedRef::Binding(binding),
            },
            ty,
            intent,
            span,
        )
    }

    pub(super) fn make_i64_literal(&mut self, value: i128, span: Span) -> HirExpr {
        self.make_expr(
            HirExprKind::Literal(HirLiteral::Integer(value)),
            ResolvedTy::I64,
            IntentKind::Read,
            span,
        )
    }

    pub(super) fn make_unit_block(
        &mut self,
        statements: Vec<HirStmt>,
        tail: Option<HirExpr>,
        ty: ResolvedTy,
        span: Span,
    ) -> HirBlock {
        HirBlock {
            node: self.ids.node(),
            scope: self.ids.scope(),
            statements,
            tail: tail.map(Box::new),
            ty,
            span,
        }
    }

    pub(super) fn make_vec_len_call(
        &mut self,
        vec_expr: HirExpr,
        _elem_ty: &ResolvedTy,
        span: Span,
    ) -> HirExpr {
        let kind = self.collection_call_kind(
            hew_types::RuntimeCallFamily::Vector(hew_types::VecValueOp::Len),
            vec![vec_expr],
            &ResolvedTy::I64,
            &span,
        );
        self.make_expr(kind, ResolvedTy::I64, IntentKind::Read, span)
    }

    pub(super) fn synthetic_binding_ref(
        &mut self,
        name: &str,
        binding: BindingId,
        ty: ResolvedTy,
        span: &std::ops::Range<usize>,
    ) -> HirExpr {
        HirExpr {
            node: self.ids.node(),
            site: self.ids.site(),
            ty,
            intent: IntentKind::Read,
            kind: HirExprKind::BindingRef {
                name: name.to_string(),
                resolved: ResolvedRef::Binding(binding),
            },
            span: span.clone(),
        }
    }

    pub(super) fn synthetic_variant_ctor(
        &mut self,
        machine_name: &str,
        state_idx: usize,
        payload: Option<Vec<(String, HirExpr)>>,
        ty: ResolvedTy,
        span: &std::ops::Range<usize>,
    ) -> HirExpr {
        HirExpr {
            node: self.ids.node(),
            site: self.ids.site(),
            ty,
            intent: IntentKind::Consume,
            kind: HirExprKind::MachineVariantCtor {
                machine_name: machine_name.to_string(),
                state_idx,
                payload,
            },
            span: span.clone(),
        }
    }

    pub(super) fn synthetic_return_block_expr(
        &mut self,
        value: HirExpr,
        span: &std::ops::Range<usize>,
    ) -> HirExpr {
        let return_stmt = HirStmt {
            node: self.ids.node(),
            kind: HirStmtKind::Return(Some(value)),
            span: span.clone(),
        };
        let block = HirBlock {
            node: self.ids.node(),
            scope: self.ids.scope(),
            statements: vec![return_stmt],
            tail: None,
            ty: ResolvedTy::Unit,
            span: span.clone(),
        };
        HirExpr {
            node: self.ids.node(),
            site: self.ids.site(),
            ty: ResolvedTy::Unit,
            intent: IntentKind::Read,
            kind: HirExprKind::Block(block),
            span: span.clone(),
        }
    }
}
