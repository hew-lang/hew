//! A `var self` method keeps its receiver whole wherever it can fail (A418).
//!
//! A failing `var self` method hands its receiver back to the caller's place,
//! as last written, instead of releasing it. A field moved out of the receiver
//! and not yet restored would leave that place with a hole, so every operation
//! that can fail while a receiver field is out is refused here, naming the
//! field and the operation. A call fails where SIR gives it a failure edge:
//! a source callee always can, and a runtime operation can where its
//! semantic contract says so.

use hew_parser::ast::{BinaryOp, CompoundAssignOp, Expr, Span, Spanned, StringPart};

use super::admissibility::StructuralTypeWalk;
use super::{CallTarget, Checker, MethodCallRewrite, SpanKey};
use crate::error::TypeErrorKind;
use crate::Ty;

impl Checker {
    fn interpolation_display_can_fail(&self, span: &Span) -> bool {
        let Some(ty) = self
            .interpolation_display_types
            .get(&SpanKey::in_module(span, self.current_module_idx))
        else {
            return false;
        };
        let Ok(resolved) = crate::ResolvedTy::from_ty(&self.subst.resolve(ty)) else {
            return true;
        };
        crate::type_facts::select_builtin_display_plan(
            &resolved,
            &self.defs,
            &self.trait_impl_method_declaration_ids,
            self.lang_items
                .display_method_identity()
                .map(|(_, method)| method),
        )
        .is_none_or(|plan| plan.can_fail())
    }

    /// The receiver binding of the `var self` method whose body is being
    /// checked, or `None` outside one. A lambda or generator body is its own
    /// callable: its failures do not unwind this method.
    fn var_self_receiver_in_scope(&self) -> Option<&str> {
        if self.lambda_capture_depth.is_some() || self.deferred_body.is_some() {
            return None;
        }
        self.var_self_receiver.as_deref()
    }

    /// The first receiver place moved out on the current path.
    fn receiver_hole(&self) -> Option<String> {
        let receiver = self.var_self_receiver_in_scope()?;
        let binding = self.env.lookup_ref(receiver)?;
        if binding.is_moved {
            return Some(receiver.to_string());
        }
        binding
            .moved_places
            .first()
            .map(|moved| Self::render_place(receiver, &moved.path))
    }

    /// Refuse an operation that can fail while a receiver field is out.
    fn refuse_fallible_with_receiver_hole(&mut self, span: &Span, operation: &str) {
        let Some(hole) = self.receiver_hole() else {
            return;
        };
        let key = SpanKey::in_module(span, self.current_module_idx);
        if !self.var_self_hole_reports.insert(key) {
            return;
        }
        self.report_error_with_suggestions(
            TypeErrorKind::OwnPartialConsume,
            span,
            format!(
                "`{hole}` is moved out of the `var self` receiver while {operation} can fail; \
                 a failing `var self` method hands its receiver back whole"
            ),
            vec![format!(
                "keep `{hole}` in place and call its methods there, move it out with `take()` \
                 if it is an `Option`, or assign a replacement to `{hole}` before anything \
                 that can fail"
            )],
        );
    }

    /// Checked after an expression's operands, so a move an operand performs
    /// (`consume(self.conn)`) is already out when the operation itself fails.
    pub(super) fn check_receiver_whole_at_expr(&mut self, expr: &Expr, span: &Span, ty: &Ty) {
        if self.var_self_receiver_in_scope().is_none() {
            return;
        }
        let key = SpanKey::in_module(span, self.current_module_idx);
        let operation = match expr {
            Expr::Call { .. } if self.call_can_fail(&key) => "this call".to_string(),
            Expr::MethodCall {
                receiver, method, ..
            } if self.method_call_can_fail(&key, receiver) => format!("`{}(...)`", method.0),
            Expr::Binary { op, .. } if checked_integer_op(*op, &self.subst.resolve(ty)) => {
                "this arithmetic".to_string()
            }
            // A user `Eq`/`Ord` implementation is an ordinary call.
            Expr::Binary { .. } if self.user_comparison_dispatch.contains_key(&key) => {
                "this comparison".to_string()
            }
            // A user `Display` implementation is an ordinary call.
            Expr::InterpolatedString(parts)
                if parts.iter().any(|part| {
                    matches!(part, StringPart::Expr(value) | StringPart::StructuralExpr(value)
                        if self.interpolation_display_can_fail(&value.1))
                }) =>
            {
                "this interpolation".to_string()
            }
            Expr::Index { .. } => "this index".to_string(),
            Expr::Await(_)
            | Expr::Spawn { .. }
            | Expr::SpawnLambdaActor { .. }
            | Expr::Scope { .. }
            | Expr::ScopeDeadline { .. }
            | Expr::ForkChild { .. }
            | Expr::ForkBlock { .. }
            | Expr::Select { .. }
            | Expr::Race(_) => "this operation".to_string(),
            _ => return,
        };
        self.refuse_fallible_with_receiver_hole(span, &operation);
    }

    /// A compound assignment on an integer place can overflow; a plain one
    /// releases the value it replaces, which can run an authored `close`.
    pub(super) fn check_receiver_whole_at_assignment(
        &mut self,
        target: &Expr,
        op: Option<CompoundAssignOp>,
        target_ty: &Ty,
        span: &Span,
    ) {
        if self.var_self_receiver_in_scope().is_none() {
            return;
        }
        let target_ty = self.subst.resolve(target_ty);
        let fallible = match op {
            Some(op) => {
                (target_ty.is_integer() || target_ty == Ty::Duration)
                    && !matches!(
                        op,
                        CompoundAssignOp::BitAnd
                            | CompoundAssignOp::BitOr
                            | CompoundAssignOp::BitXor
                    )
            }
            None => {
                self.release_may_run_close(&target_ty)
                    && self.expr_place(target).is_none_or(|(root, path)| {
                        self.env.lookup_ref(root).is_some_and(|b| !b.is_moved)
                            && self.env.place_move_conflict(root, &path).is_none()
                    })
            }
        };
        if fallible {
            self.refuse_fallible_with_receiver_hole(span, "this assignment");
        }
    }

    /// The bindings of the scope being left are released, and releasing a
    /// value whose type can reach an authored `close` can fail - whether or
    /// not this path moved it away.
    pub(super) fn check_receiver_whole_at_scope_end(&mut self) {
        let Some(receiver) = self.var_self_receiver_in_scope().map(str::to_string) else {
            return;
        };
        let released: Vec<(String, Span)> = self
            .env
            .current_scope_bindings()
            .filter(|(name, _)| name.name.as_str() != receiver)
            .filter_map(|(name, _)| {
                let binding = self.env.lookup_ref(name)?;
                self.release_may_run_close(&self.subst.resolve(&binding.ty))
                    .then(|| Some((name.to_string(), binding.def_span.clone()?)))
                    .flatten()
            })
            .collect();
        for (name, span) in released {
            self.refuse_fallible_with_receiver_hole(&span, &format!("releasing `{name}`"));
        }
    }

    fn call_can_fail(&self, key: &SpanKey) -> bool {
        match self.direct_call_targets.get(key) {
            Some(target) => self.target_can_fail(target, None),
            None => self
                .resolved_calls
                .get(key)
                .is_some_and(|call| self.target_can_fail(&call.target, None)),
        }
    }

    fn method_call_can_fail(&self, key: &SpanKey, receiver: &Spanned<Expr>) -> bool {
        let receiver_ty = self
            .expr_types
            .get(&SpanKey::in_module(&receiver.1, self.current_module_idx))
            .map(|ty| self.subst.resolve(ty));
        let receiver_ty = receiver_ty.as_ref();
        if let Some(target) = self
            .resolved_calls
            .get(key)
            .map(|call| &call.target)
            .or_else(|| self.direct_call_targets.get(key))
        {
            return self.target_can_fail(target, receiver_ty);
        }
        match self.method_call_rewrites.get(key) {
            Some(
                MethodCallRewrite::RewriteToFunction { target, .. }
                | MethodCallRewrite::RewriteModuleQualifiedToFunction { target, .. },
            ) => self.target_can_fail(target, receiver_ty),
            Some(
                MethodCallRewrite::CopyCloneNoop
                | MethodCallRewrite::CancellationTokenIsCancelled
                | MethodCallRewrite::BuiltinVecIter
                | MethodCallRewrite::BuiltinVecIntoIter,
            ) => false,
            _ => true,
        }
    }

    /// Whether a checked call target has a failure edge. A runtime operation
    /// fails where its semantic contract, the one SIR lowers, says it does,
    /// or where it releases receiver contents that can reach a `close`; a
    /// source-level callee can always fail.
    fn target_can_fail(&self, target: &CallTarget, receiver: Option<&Ty>) -> bool {
        let family = match target {
            CallTarget::RecordConstructor(_) => return false,
            CallTarget::Runtime(family) | CallTarget::DeclaredRuntime { family, .. } => *family,
            CallTarget::RuntimeCollection(method) => match method.runtime_family() {
                Some(family) => family,
                None => return true,
            },
            _ => return true,
        };
        let Some(contract) = family.semantic_contract() else {
            return true;
        };
        !contract.failures.is_empty()
            || (family.releases_receiver_contents()
                && receiver.is_none_or(|ty| self.release_may_run_close(ty)))
    }

    /// Whether releasing a value of `ty` can run an authored `close`. An
    /// abstract type parameter may be instantiated with one.
    fn release_may_run_close(&self, ty: &Ty) -> bool {
        self.release_may_run_close_on_graph(ty, &mut StructuralTypeWalk::default())
    }

    fn release_may_run_close_on_graph(&self, ty: &Ty, walk: &mut StructuralTypeWalk) -> bool {
        let ty = self.subst.resolve(ty);
        if !walk.explored.insert(ty.clone()) {
            return false;
        }
        if walk.growing(&ty) {
            if let Ty::Named { args, .. } = &ty {
                return args
                    .iter()
                    .any(|arg| self.release_may_run_close_on_graph(arg, walk));
            }
        }
        walk.active.push(ty.clone());
        let may = (|| match &ty {
            Ty::Named { head, args } => {
                if head
                    .nominal()
                    .is_some_and(|id| self.registry.is_resource(id) || self.registry.is_linear(id))
                {
                    return true;
                }
                if args
                    .iter()
                    .any(|arg| self.release_may_run_close_on_graph(arg, walk))
                {
                    return true;
                }
                if head.builtin().is_some() {
                    return false;
                }
                match self.registry.member_types(*head) {
                    Some(members) => {
                        let parameters = self
                            .head_type_def(*head)
                            .map(|definition| definition.type_params.as_slice())
                            .unwrap_or_default();
                        members.iter().any(|member| {
                            let member =
                                Self::instantiate_type_def_member(member, parameters, args);
                            self.release_may_run_close_on_graph(&member, walk)
                        })
                    }
                    None => matches!(head, crate::TypeHead::Param(_)),
                }
            }
            Ty::Tuple(elements) => elements
                .iter()
                .any(|element| self.release_may_run_close_on_graph(element, walk)),
            Ty::Array(element, _) | Ty::Slice(element) => {
                self.release_may_run_close_on_graph(element, walk)
            }
            _ => false,
        })();
        walk.active.pop();
        may
    }
}

/// The integer operations whose checked form can trap.
fn checked_integer_op(op: BinaryOp, ty: &Ty) -> bool {
    (ty.is_integer() || *ty == Ty::Duration)
        && matches!(
            op,
            BinaryOp::Add
                | BinaryOp::Subtract
                | BinaryOp::Multiply
                | BinaryOp::Divide
                | BinaryOp::Modulo
                | BinaryOp::Shl
                | BinaryOp::Shr
        )
}

#[cfg(test)]
mod graph_tests {
    use super::*;
    use crate::module_registry::ModuleRegistry;

    #[test]
    fn release_close_query_visits_shared_cycles_and_preserves_resource_leaves() {
        let mut checker = Checker::new(ModuleRegistry::new(Vec::new()));
        let leaf = crate::NominalId::from_minted_declaration(checker.defs.mint_for_test("Node0"));
        let leaf_ty = Ty::named_in(&checker.defs, "Node0", vec![]);
        checker.registry.register_type(
            checker.head_of_declaration(leaf),
            vec![Ty::builtin_named(
                crate::BuiltinType::Vec,
                vec![leaf_ty.clone()],
            )],
        );
        let mut root = leaf_ty;
        for level in 1..=24 {
            let name = format!("Node{level}");
            let id = crate::NominalId::from_minted_declaration(checker.defs.mint_for_test(&name));
            checker
                .registry
                .register_type(checker.head_of_declaration(id), vec![root.clone(), root]);
            root = Ty::named_in(&checker.defs, &name, vec![]);
        }
        assert!(!checker.release_may_run_close(&root));
        checker.registry.register_resource_type(leaf);
        assert!(
            checker.release_may_run_close(&root),
            "resource cleanup must remain a possible failure edge"
        );
    }
}
