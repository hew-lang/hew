use super::branch_join::BranchArmExit;
#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;
use crate::builtin_names::BuiltinNamedType;
use crate::BuiltinType;

impl Checker {
    /// Re-synthesize deferred bodies at one materialization edge while keeping
    /// only move-state diagnostics. Registration already owns ordinary typing
    /// and lexical-resolution errors; replay exists solely to apply the edge's
    /// current ownership state and to thread effects through LIFO bodies.
    fn recheck_materialized_defers(&mut self, defers: Vec<Spanned<Expr>>) {
        for (body, span) in defers {
            let error_mark = self.errors.len();
            self.synthesize(&body, &span);
            let replay_errors = self.errors.split_off(error_mark);
            for error in replay_errors.into_iter().filter(|error| {
                matches!(
                    error.kind,
                    TypeErrorKind::UseAfterMove | TypeErrorKind::UseAfterConsume
                )
            }) {
                let duplicate = self.errors.iter().any(|existing| {
                    existing.kind == error.kind
                        && existing.span == error.span
                        && existing.message == error.message
                        && existing.notes == error.notes
                        && existing.suggestions == error.suggestions
                        && existing.source_module == error.source_module
                });
                if !duplicate {
                    self.errors.push(error);
                }
            }
        }
    }

    fn recheck_current_scope_defers(&mut self) {
        self.recheck_materialized_defers(self.env.current_scope_defers());
    }

    fn recheck_return_edge_defers(&mut self) {
        self.recheck_materialized_defers(self.env.return_edge_defers());
    }

    fn recheck_loop_edge_defers(&mut self, label: Option<&str>, span: &Span) {
        let Some(defers) = self.env.loop_edge_defers(label) else {
            self.errors.push(
                TypeError::new(
                    TypeErrorKind::InvalidOperation,
                    span.clone(),
                    "cannot determine deferred move-state for this loop exit",
                )
                .with_suggestion(
                    "check that the loop label names an active enclosing loop".to_string(),
                ),
            );
            return;
        };
        self.recheck_materialized_defers(defers);
    }

    /// The declared type of an annotated `let`/`var`, given the annotation and
    /// the type synthesised for the initialiser.
    ///
    /// For most annotations the initialiser's own (possibly more specific) type
    /// is the better binding type — it survives unification and keeps literal
    /// widths and inferred generic arguments. A `dyn Trait` annotation is the
    /// exception: the initialiser is a CONCRETE type that has been *erased* by
    /// a recorded `T → dyn Trait` coercion, so its storage is a two-word fat
    /// pointer, not the concrete layout. Keeping the concrete type here makes
    /// the checker and HIR disagree about the binding — HIR lowers the slot
    /// from the annotation (`dyn Trait`) while every checker-derived fact about
    /// later uses (method resolution, `clone`, assignment) speaks about the
    /// concrete type. That disagreement reinterprets the fat pointer as the
    /// concrete layout and puts raw memory into value channels.
    ///
    /// A `dyn Trait`-annotated binding therefore carries the trait-object type,
    /// exactly as a `dyn Trait` function parameter does.
    fn annotated_binding_ty(&self, expected: &Ty, actual: Ty) -> Ty {
        if matches!(actual, Ty::Error) {
            return actual;
        }
        let resolved_expected = self.subst.resolve(expected);
        if matches!(resolved_expected, Ty::TraitObject { .. })
            || matches!(
                &resolved_expected,
                Ty::Named { name, .. } if self.type_aliases.contains_key(name)
            )
        {
            return resolved_expected;
        }
        actual
    }

    fn method_chain_root_binding(expr: &Expr) -> Option<&str> {
        match expr {
            Expr::Identifier(name) => Some(name),
            Expr::MethodCall { receiver, .. } => Self::method_chain_root_binding(&receiver.0),
            _ => None,
        }
    }

    /// A validated `#[returns_receiver]` call transfers the receiver owner to
    /// its result. When that exact result is discarded in statement position,
    /// the transfer is a no-op: the original binding remains the sole owner.
    /// Record that fact per call site for HIR and restore the checker binding
    /// only when it was live before this statement.
    fn preserve_discarded_receiver_identity_chain(
        &mut self,
        expr: &Expr,
        span: &Span,
    ) -> Option<String> {
        let Expr::MethodCall { receiver, .. } = expr else {
            return None;
        };
        let key = SpanKey::in_module(span, self.current_module_idx);
        let rewrite_identity = matches!(
            self.method_call_rewrites.get(&key),
            Some(
                MethodCallRewrite::RewriteToFunction {
                    returns_receiver_identity: true,
                    ..
                } | MethodCallRewrite::StaticTraitDispatch {
                    returns_receiver_identity: true,
                    ..
                }
            )
        );
        let dyn_signature = self
            .dyn_trait_method_calls
            .get(&key)
            .map(|call| &call.signature);
        let is_identity = rewrite_identity
            || dyn_signature.is_some_and(|signature| signature.returns_receiver_identity);
        let consumes = self.method_call_consumes_receiver.contains(&key)
            || dyn_signature.is_some_and(|signature| signature.consumes_receiver);
        if consumes && !is_identity {
            return None;
        }

        let (root, nested_identity) = match &receiver.0 {
            Expr::Identifier(name) => (name.clone(), false),
            Expr::MethodCall { .. } => {
                let root =
                    self.preserve_discarded_receiver_identity_chain(&receiver.0, &receiver.1)?;
                (root, true)
            }
            _ => return None,
        };
        if is_identity {
            self.method_call_preserves_receiver_identity.insert(key);
        }
        (is_identity || nested_identity).then_some(root)
    }

    fn synthesize_discarded_expression(&mut self, expr: &Expr, span: &Span) -> Ty {
        let root = Self::method_chain_root_binding(expr).map(str::to_string);
        let root_was_moved = root
            .as_deref()
            .and_then(|name| self.env.lookup(name))
            .is_some_and(|binding| binding.is_moved);
        let ty = self.synthesize(expr, span);
        let key = SpanKey::in_module(span, self.current_module_idx);
        if matches!(
            self.actor_method_dispatch.get(&key),
            Some(ActorMethodKind::CheckedFire(_))
        ) {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                "policy-sensitive actor send result must be handled; use `?`, `match`, or an explicit `let _ = ...` acknowledgment"
                    .to_string(),
            );
        }
        if !root_was_moved {
            if let Some(root) = self.preserve_discarded_receiver_identity_chain(expr, span) {
                self.env.unmark_moved(&root);
            }
        }
        ty
    }

    fn iterator_trait_item_ty(&mut self, iter_ty: &Ty, span: &Span) -> Option<Ty> {
        let resolved = self.subst.resolve(iter_ty);
        if let Ty::TraitObject { traits } = &resolved {
            for bound in traits {
                if bound.trait_name != "Iterator"
                    && !self.trait_extends(&bound.trait_name, "Iterator")
                {
                    continue;
                }
                if let Some((_, item_ty)) =
                    bound.assoc_bindings.iter().find(|(name, _)| name == "Item")
                {
                    return Some(item_ty.clone());
                }
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    "`for` over `dyn Iterator` requires an `Item` associated-type binding"
                        .to_string(),
                );
                return Some(Ty::Error);
            }
        }

        if self.type_satisfies_trait_bound(&resolved, "IntoIterator") {
            let item_projection = Ty::AssocType {
                base: Box::new(resolved),
                trait_name: "IntoIterator".into(),
                assoc_name: "Item".into(),
            };
            return Some(self.project_assoc_types(&item_projection));
        }

        if !self.type_satisfies_trait_bound(&resolved, "Iterator") {
            return None;
        }

        let item_projection = Ty::AssocType {
            base: Box::new(resolved),
            trait_name: "Iterator".into(),
            assoc_name: "Item".into(),
        };
        Some(self.project_assoc_types(&item_projection))
    }

    fn assignment_root_binding_name(expr: &Expr) -> Option<&str> {
        match expr {
            Expr::Identifier(name) => Some(name.as_str()),
            Expr::FieldAccess { object, .. } | Expr::Index { object, .. } => {
                Self::assignment_root_binding_name(&object.0)
            }
            _ => None,
        }
    }

    /// Whether an arithmetic-shaped assignment value reads the binding being
    /// replaced. Such an update must preserve the binding's independently
    /// inferred storage width rather than adopting one operand's width as if
    /// the assignment were a fresh direct write.
    fn numeric_update_reads_binding(expr: &Expr, binding: &str) -> bool {
        match expr {
            Expr::Identifier(name) => name == binding,
            Expr::Binary { left, right, .. } => {
                Self::numeric_update_reads_binding(&left.0, binding)
                    || Self::numeric_update_reads_binding(&right.0, binding)
            }
            Expr::Unary { operand, .. } => Self::numeric_update_reads_binding(&operand.0, binding),
            _ => false,
        }
    }

    /// Whether an assignment target crosses a compiler-proven caller-visible
    /// shared-handle boundary before reaching the storage it writes.
    ///
    /// `holder.items[0]` is caller-visible because the `Index` receiver has
    /// type `Vec<_>`. `holder.items = replacement` is not: its receiver is the
    /// private `Holder` copy, even though the field being replaced happens to
    /// contain a handle. Recursing through the target also covers projections
    /// such as `holders[0].count`, whose write starts inside shared Vec storage.
    /// A root `string`, `bytes`, or registered shared-handle binding is itself
    /// the caller-visible storage boundary; admitting it uses the same exact
    /// checker type facts as parameter classification.
    /// The builtin boundary test shares the declaration-time authority in
    /// `BuiltinType::is_caller_visible_shared_handle`, so nested actor, channel,
    /// stream, and reference handles cannot drift from aggregate admission.
    fn mutation_projection_reaches_caller_visible_storage(&self, target: &Expr) -> bool {
        match target {
            Expr::Identifier(name) => self.env.lookup_ref(name).is_some_and(|binding| {
                match self.subst.resolve(&binding.ty) {
                    Ty::String | Ty::Bytes => true,
                    Ty::Named {
                        builtin: Some(builtin),
                        ..
                    } => builtin.is_caller_visible_shared_handle(),
                    _ => false,
                }
            }),
            Expr::FieldAccess { object, .. } | Expr::Index { object, .. } => {
                let object_ty = self
                    .expr_types
                    .get(&SpanKey::in_module(&object.1, self.current_module_idx))
                    .map(|ty| self.subst.resolve(ty));
                object_ty.is_some_and(|ty| {
                    matches!(
                        ty,
                        Ty::Named {
                            builtin: Some(builtin),
                            ..
                        } if builtin.is_caller_visible_shared_handle()
                    )
                }) || self.mutation_projection_reaches_caller_visible_storage(&object.0)
            }
            _ => false,
        }
    }

    /// The synthetic span the `for (k, v) in m` desugar uses for the `keys()`
    /// projection call.
    ///
    /// `resolved_calls` (and `expr_types`) are keyed by span, so the projection
    /// calls this for-in synthesizes cannot share a span with each other OR with
    /// the iterable expression itself. Recording any projection at the iterable's
    /// real span would clobber `expr_types[iterable_span]` with `Vec<…>`, and HIR
    /// derives the for-in route from that side-table type for non-identifier
    /// iterables (a field access / call / index / method-call source reads its
    /// type exclusively from the checker side-table) — so the loop would
    /// mis-route to the Vec arm and build a `VecIter` directly over the
    /// HashMap/HashSet handle (silent type confusion). Every projection therefore
    /// uses its own zero-width synthetic span, leaving `expr_types[iterable_span]`
    /// holding the iterable's true `HashMap`/`HashSet` type.
    ///
    /// `keys()` anchors at the iterable's start offset; `values()` at the end
    /// offset. Both are zero-width (start == end), so each is distinct from every
    /// real expression span (real spans have start < end) and from each other
    /// (the iterable has start < end, so its start and end offsets differ). The
    /// derivation is deterministic, so the checker (recording the facts) and HIR
    /// (emitting the synthetic calls) agree byte-for-byte. Both layers call these
    /// single helpers; do not inline the derivation.
    pub(super) fn hashmap_for_in_keys_span(iterable: &Span) -> Span {
        iterable.start..iterable.start
    }

    /// The synthetic span the `for (k, v) in m` desugar uses for the `values()`
    /// projection call. See [`Self::hashmap_for_in_keys_span`] for the
    /// synthetic-span rationale; `values()` anchors at the iterable's end offset.
    pub(super) fn hashmap_for_in_values_span(iterable: &Span) -> Span {
        iterable.end..iterable.end
    }

    /// The synthetic span the `for x in s` desugar uses for the `HashSet`
    /// `to_vec()` projection call. See [`Self::hashmap_for_in_keys_span`] for the
    /// synthetic-span rationale; `to_vec()` anchors at the iterable's start
    /// offset (a `HashSet` for-in has only this one projection, so it cannot
    /// collide with `keys()`/`values()`, which only appear in `HashMap` for-in).
    pub(super) fn hashset_for_in_to_vec_span(iterable: &Span) -> Span {
        iterable.start..iterable.start
    }

    fn for_await_actor_method_name(&mut self, iterable: &Expr) -> Option<String> {
        let Expr::MethodCall {
            receiver, method, ..
        } = iterable
        else {
            return None;
        };
        let receiver_ty = {
            let ty = self.synthesize(&receiver.0, &receiver.1);
            self.subst.resolve(&ty)
        };
        let actor_ty = match receiver_ty.as_actor_handle() {
            Some(actor_ty) => self.subst.resolve(actor_ty),
            None => return None,
        };
        let Ty::Named { name, .. } = actor_ty else {
            return None;
        };
        let actor_name = self
            .type_defs
            .get(&name)
            .filter(|def| def.kind == TypeDefKind::Actor)
            .map_or(name, |def| def.name.clone());
        Some(format!("{actor_name}::{method}"))
    }

    /// Determine the type of the last statement in a block (the statement that
    /// produces the block's value when there is no trailing expression).
    ///
    /// If/IfLet/Match/Return/Break/Continue are value-producing in tail position
    /// and delegate to `check_stmt_as_expr`.  A break-less `loop {}` diverges
    /// (type `Never`), allowing `if cond { value } else { loop {} }` to unify
    /// the branch types without forcing the whole expression to `Unit`.
    fn check_last_stmt_type(&mut self, stmt: &Stmt, span: &Span, expected: Option<&Ty>) -> Ty {
        match stmt {
            Stmt::If { .. }
            | Stmt::IfLet { .. }
            | Stmt::Match { .. }
            | Stmt::Return(_)
            | Stmt::Break { .. }
            | Stmt::Continue { .. } => self.check_stmt_as_expr(stmt, span, expected),
            Stmt::Expression((expr, es)) => {
                let expr_ty = self.synthesize_discarded_expression(expr, es);
                if matches!(expr_ty, Ty::Never) {
                    Ty::Never
                } else {
                    Ty::Unit
                }
            }
            Stmt::Loop { label, body } => {
                self.check_stmt(stmt, span);
                if hew_parser::loop_body_has_break(body, label.as_deref()) {
                    Ty::Unit
                } else {
                    Ty::Never
                }
            }
            _ => {
                self.check_stmt(stmt, span);
                Ty::Unit
            }
        }
    }

    pub(super) fn check_block(&mut self, block: &Block, expected: Option<&Ty>) -> Ty {
        self.env.push_scope();
        // Snapshot const_values so let-bound literal entries added in this
        // scope are cleaned up when the scope exits.
        let const_values_snapshot: HashMap<String, ConstValue> = self.const_values.clone();
        let num_stmts = block.stmts.len();
        let mut terminated = false;
        // Tail Ok-coercion is armed by the enclosing `check_fn_decl` only when
        // this block sits in function-return tail position. Every non-tail
        // statement in this block (and any sub-block reached for a non-tail
        // statement) must NOT coerce, so clear the flag for the statement loop
        // and restore it only for the block's own tail computation (the
        // `is_last` If/Match/Return arm and the trailing expression).
        let tail_ok_armed = std::mem::replace(&mut self.tail_ok_armed, false);
        for (i, (stmt, span)) in block.stmts.iter().enumerate() {
            // If a previous statement was terminal, warn about this unreachable code
            if terminated {
                self.warnings.push(TypeError {
                    severity: crate::error::Severity::Warning,
                    kind: TypeErrorKind::UnreachableCode,
                    span: span.clone(),
                    message: "unreachable code".to_string(),
                    notes: vec![],
                    suggestions: vec![
                        "remove this code or restructure the control flow".to_string()
                    ],
                    source_module: self.current_module.clone(),
                });
                // Still type-check the remaining statements for error coverage,
                // but only emit the unreachable warning once per block.
                for (s, sp) in &block.stmts[i..] {
                    self.check_stmt(s, sp);
                }
                self.const_values = const_values_snapshot;
                self.emit_scope_warnings();
                return Ty::Never;
            }

            let is_last = i + 1 == num_stmts && block.trailing_expr.is_none();
            if is_last {
                // Re-arm tail Ok-coercion for the block's tail statement so
                // If/Match arm bodies that flow to return can Ok-coerce.
                self.tail_ok_armed = tail_ok_armed;
                let ty = self.check_last_stmt_type(stmt, span, expected);
                if !matches!(ty, Ty::Never) {
                    self.recheck_current_scope_defers();
                }
                self.const_values = const_values_snapshot;
                self.emit_scope_warnings();
                return ty;
            }
            // Determine whether this non-tail statement terminates control flow.
            match stmt {
                Stmt::If { .. } | Stmt::IfLet { .. } | Stmt::Match { .. } => {
                    terminated = matches!(self.check_stmt_as_expr(stmt, span, None), Ty::Never);
                }
                Stmt::Return(_) | Stmt::Break { .. } | Stmt::Continue { .. } => {
                    self.check_stmt(stmt, span);
                    terminated = true;
                }
                Stmt::Loop { label, body } => {
                    self.check_stmt(stmt, span);
                    // A break-less loop diverges; mark subsequent stmts unreachable.
                    terminated = !hew_parser::loop_body_has_break(body, label.as_deref());
                }
                _ => self.check_stmt(stmt, span),
            }
        }
        // If a trailing expression follows a terminal statement, it's unreachable
        let ty = if let Some(expr) = &block.trailing_expr {
            if terminated {
                self.warnings.push(TypeError {
                    severity: crate::error::Severity::Warning,
                    kind: TypeErrorKind::UnreachableCode,
                    span: expr.1.clone(),
                    message: "unreachable code".to_string(),
                    notes: vec![],
                    suggestions: vec![
                        "remove this code or restructure the control flow".to_string()
                    ],
                    source_module: self.current_module.clone(),
                });
            }
            // When the block has a known expected type, use check_against so that
            // numeric literals coerce to the target width before any fallback
            // materialization. For all other expressions check_against falls
            // through to synthesize + expect_type, producing the same result as
            // before.
            //
            // Re-arm the tail Ok-coercion: a trailing expression is this block's
            // value-producing tail, so it inherits the enclosing function's
            // armed state. `check_against` is the type-directed propagation site
            // that performs the actual Ok-wrap.
            self.tail_ok_armed = tail_ok_armed;
            if let Some(exp) = expected {
                self.check_against(&expr.0, &expr.1, exp)
            } else {
                self.synthesize(&expr.0, &expr.1)
            }
        } else {
            Ty::Unit
        };
        if !terminated && !matches!(ty, Ty::Never) {
            self.recheck_current_scope_defers();
        }
        self.const_values = const_values_snapshot;
        self.emit_scope_warnings();
        ty
    }

    /// Check a statement-position `if` chain whose value is discarded, keeping
    /// each arm's ownership state separate and joining them at the end.
    ///
    /// Returns whether every path through the chain diverges, which is what the
    /// recursion needs to classify an `else if` link as a non-reaching arm.
    fn check_discarded_if_chain(
        &mut self,
        condition: &Spanned<Expr>,
        then_block: &Block,
        else_block: Option<&hew_parser::ast::ElseBlock>,
    ) -> bool {
        self.check_against(&condition.0, &condition.1, &Ty::Bool);
        let entry = self.env.ownership_snapshot();
        let then_ty = self.check_block(then_block, None);
        let then_exit = BranchArmExit {
            ownership: self.env.ownership_snapshot(),
            diverges: Self::arm_skips_join_block(then_block, &then_ty),
        };
        let then_skips_join = then_exit.diverges;
        let Some(eb) = else_block else {
            self.join_fall_through(&entry, then_exit);
            return false;
        };
        self.env.restore_ownership(&entry);
        let else_skips_join = if let Some(if_stmt) = &eb.if_stmt {
            if let Stmt::If {
                condition,
                then_block,
                else_block,
            } = &if_stmt.0
            {
                self.check_discarded_if_chain(condition, then_block, else_block.as_ref())
            } else {
                self.check_stmt(&if_stmt.0, &if_stmt.1);
                false
            }
        } else if let Some(block) = &eb.block {
            let else_ty = self.check_block(block, None);
            Self::arm_skips_join_block(block, &else_ty)
        } else {
            // `else` with neither a block nor a chained `if`: nothing runs on
            // that path, so it is the implicit fall-through.
            self.join_fall_through(&entry, then_exit);
            return false;
        };
        self.join_branch_ownership(
            &entry,
            &[
                then_exit,
                BranchArmExit {
                    ownership: self.env.ownership_snapshot(),
                    diverges: else_skips_join,
                },
            ],
        );
        then_skips_join && else_skips_join
    }

    /// Type-check the operand of a `return` against the enclosing function's
    /// declared return type.
    ///
    /// This is the single shared shell for ALL `return` positions: the two
    /// statement-position `Stmt::Return` arms (`check_stmt` and
    /// `check_stmt_as_expr`) and the expression-position `Expr::Return`
    /// (`synthesize`). Routing every position through one helper keeps the
    /// generator-Return extraction, the `Ty::Error` guard, the unit-vs-declared
    /// mismatch diagnostic, and the `#[on(crash)]` fail-closed gate identical
    /// across positions (LESSONS `one-construct-one-lowering-shell`).
    ///
    /// The return *type* of the construct itself is always `Ty::Never` (a
    /// `return` diverges); callers assign that directly.
    pub(super) fn check_return_operand(&mut self, value: Option<&Spanned<Expr>>, span: &Span) {
        if let Some(expected) = self.current_return_type.clone() {
            // Inside a gen{} body, `current_return_type` is shaped as
            // `Generator<Y, R>`. A `return <expr>` targets the Return component R,
            // not the full Generator type, so `return 1` inside gen{} unifies
            // against i64 rather than Generator<Y, i64>.
            let effective_expected = if self.in_generator {
                let resolved = self.subst.resolve(&expected);
                match resolved.as_generator() {
                    Some((_, ret)) => ret.clone(),
                    None => expected,
                }
            } else {
                expected
            };
            // Guard: do not check against Ty::Error — it would silently suppress
            // mismatch diagnostics in the returned expression. Synthesize the value
            // instead so its own errors are still caught.
            if matches!(self.subst.resolve(&effective_expected), Ty::Error) {
                if let Some((val, vs)) = value {
                    self.synthesize(val, vs);
                }
            } else {
                match value {
                    Some((val, vs)) => {
                        self.check_against(val, vs, &effective_expected);
                    }
                    None if effective_expected != Ty::Unit => {
                        self.errors.push(TypeError::return_type_mismatch(
                            span.clone(),
                            &effective_expected,
                            &Ty::Unit,
                        ));
                    }
                    _ => {}
                }
            }
        }
        self.recheck_return_edge_defers();
        // M-4: a `return CrashAction::…;` inside a `#[on(crash)]` hook is now
        // fully wired (the MIR return boundary extracts the variant tag; the
        // supervisor honours it). The former fail-closed reject is removed; the
        // standard `check_against` above type-checks the operand normally.
    }

    /// Check a statement that may serve as a block's trailing expression.
    /// Returns the "expression type" of the statement.
    pub(super) fn check_stmt_as_expr(
        &mut self,
        stmt: &Stmt,
        span: &Span,
        expected: Option<&Ty>,
    ) -> Ty {
        match stmt {
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                self.check_against(&condition.0, &condition.1, &Ty::Bool);
                let entry = self.env.ownership_snapshot();
                let then_ty = self.check_block(then_block, expected);
                let then_exit = BranchArmExit {
                    ownership: self.env.ownership_snapshot(),
                    diverges: Self::arm_skips_join_block(then_block, &then_ty),
                };
                // An `else if` link is itself a two-way branch, so recursing
                // gives the chain its join for free: each link restores to its
                // own entry, which is this arm's restored state.
                if let Some(eb) = else_block {
                    if let Some(ref if_stmt) = eb.if_stmt {
                        self.env.restore_ownership(&entry);
                        let else_ty = self.check_stmt_as_expr(&if_stmt.0, &if_stmt.1, expected);
                        let else_skips = Self::arm_skips_join_stmt(&if_stmt.0, &else_ty);
                        self.join_two_way(&entry, then_exit, else_skips);
                        self.unify_branches(&then_ty, &else_ty, &if_stmt.1)
                    } else if let Some(block) = &eb.block {
                        self.env.restore_ownership(&entry);
                        let else_ty = self.check_block(block, expected);
                        let else_skips = Self::arm_skips_join_block(block, &else_ty);
                        self.join_two_way(&entry, then_exit, else_skips);
                        self.unify_branches(&then_ty, &else_ty, span)
                    } else {
                        self.join_fall_through(&entry, then_exit);
                        Ty::Unit
                    }
                } else {
                    self.join_fall_through(&entry, then_exit);
                    Ty::Unit
                }
            }
            Stmt::IfLet {
                pattern,
                expr,
                body,
                else_body,
            } => {
                let scr_ty = self.synthesize(&expr.0, &expr.1);
                if self.reject_unsupported_iflet_pattern(&pattern.0, &pattern.1) {
                    return Ty::Error;
                }
                let entry = self.env.ownership_snapshot();
                self.env.push_scope();
                self.bind_pattern(&pattern.0, &scr_ty, false, &pattern.1);
                // Record the pattern resolution so HIR lowering can consume
                // the same `pattern_resolutions` side-table that powers
                // `WhileLet` and `Match` lowering.
                self.record_arm_resolution(&pattern.0, &pattern.1, &scr_ty);
                let then_ty = self.check_block(body, expected);
                let then_exit = BranchArmExit {
                    ownership: self.env.ownership_snapshot(),
                    diverges: Self::arm_skips_join_block(body, &then_ty),
                };
                self.env.pop_scope();
                if let Some(block) = else_body {
                    self.env.restore_ownership(&entry);
                    let else_ty = self.check_block(block, expected);
                    let else_skips = Self::arm_skips_join_block(block, &else_ty);
                    self.join_two_way(&entry, then_exit, else_skips);
                    self.unify_branches(&then_ty, &else_ty, span)
                } else {
                    self.join_fall_through(&entry, then_exit);
                    Ty::Unit
                }
            }
            Stmt::Match { scrutinee, arms } => {
                let scr_ty = self.synthesize(&scrutinee.0, &scrutinee.1);
                self.check_match_expr(&scr_ty, arms, span, expected)
            }
            Stmt::Expression((expr, es)) => self.synthesize_discarded_expression(expr, es),
            Stmt::Return(value) => {
                self.check_return_operand(value.as_ref(), span);
                Ty::Never
            }
            Stmt::Break { .. } | Stmt::Continue { .. } => {
                self.check_stmt(stmt, span);
                Ty::Never
            }
            _ => {
                self.check_stmt(stmt, span);
                Ty::Unit
            }
        }
    }

    fn infer_integer_literal_binding_type(
        &mut self,
        value: Option<&Spanned<Expr>>,
        val_ty: Ty,
    ) -> Ty {
        let Some((expr, span)) = value else {
            return val_ty;
        };
        if is_integer_literal(expr) && val_ty.is_integer_literal() {
            let var = TypeVar::fresh();
            let inferred = Ty::Var(var);
            self.expect_type(&inferred, &val_ty, span);
            self.record_integer_literal_type(expr, span, &inferred);
            // Remember where this var's literal was recorded so a later
            // promotion (e.g. `apply_deferred_range_bound_types` narrowing a
            // range-bound identifier's own var to a concrete width) can
            // re-record the DECLARATION-site span too, not just the
            // reference site that triggered the promotion. Without this, an
            // unannotated `let n = 6;` later used as a range bound
            // (`0 .. n`) that gets narrowed via loop-variable usage keeps
            // its declaration-site span defaulted to `i64` in the exported
            // `expr_types` snapshot (`check_program` takes that snapshot
            // once, before defaulting/promotion runs) even though the
            // reference site inside the range is correctly narrowed — HIR's
            // identifier lowering reads the DECLARATION site, not the
            // reference site, so the stale entry is the one that matters.
            self.literal_binding_value_spans
                .insert(var, (span.clone(), self.current_module_idx));
            inferred
        } else {
            val_ty
        }
    }

    /// Would a bare identifier `name` at a USE-site resolve to a known *unit*
    /// enum variant / nullary constructor?
    ///
    /// This decides, on the let-side, whether a bare pattern identifier is a
    /// refutable variant pattern (route to the refutability gate) or a fresh
    /// binding. It must agree with how `synthesize`/`synthesize_identifier`
    /// (expressions.rs) resolve the SAME bare name at a use-site: a
    /// disagreement produces a half-built binding, where the let-side binds a
    /// local that the use-side then shadows with the builtin/global variant,
    /// leaving an unused-variable warning plus a `cannot infer type` at the use.
    ///
    /// Resolution is therefore by the GLOBAL/builtin namespace and is
    /// independent of the value type, mirroring the use-side exactly:
    ///   * `None` is special-cased at the use-site (expressions.rs, the
    ///     `name == "None"` arm of `synthesize`) as `Option::None`
    ///     *unconditionally*, ahead of any binding lookup — so a bare `None`
    ///     is always a variant pattern here. A value type that is not `Option`
    ///     then surfaces as a clean single type mismatch (`None` is
    ///     `Option<_>`, the value is not), never a stray binding.
    ///   * any user enum's unit variant is found by the use-side's
    ///     `resolve_identifier_variant`, which scans every `type_defs` entry
    ///     for a `VariantDef::Unit` of that name — so the let-side scans the
    ///     same table the same way.
    ///
    /// Qualified paths (`E::A`) are handled by the caller and never reach here.
    /// Mirrors Rust: a name that names a unit variant/const is always a
    /// pattern, and a mismatched value type is a plain type error — not a
    /// binding.
    fn bare_identifier_resolves_to_unit_variant(&self, name: &str) -> bool {
        // Builtin `None`: the use-side resolves a bare `None` to `Option::None`
        // unconditionally and ahead of any binding, so the let-side treats it
        // as a variant pattern regardless of the value type. (`Some`, `Ok`,
        // `Err` carry payloads — never bare unit variants.)
        if name == "None" {
            return true;
        }
        // User enums: scan every type definition for a unit variant of this
        // name, exactly as the use-side's `resolve_identifier_variant` does.
        // A bare name that is a unit variant of *any* known enum is a pattern.
        self.type_defs
            .values()
            .any(|td| matches!(td.variants.get(name), Some(VariantDef::Unit)))
    }

    /// Does an identifier in `let`/binding position name a unit variant — a
    /// refutable tag-test that binds NOTHING — rather than introduce a fresh
    /// binder? This is the single authority for the let-side binder-vs-tag-test
    /// decision: `check_stmt`'s `let` arm uses it to skip `bind_pattern` for a
    /// unit-variant identifier (a `let None = … else { … }` / `let red = color
    /// else { … }` tag-test), and the borrowed-Rc escape scanner consults the
    /// SAME predicate so it never invents a dangerous-scope shadow for a name
    /// that did not actually bind. Detection is by resolution (a `::`-qualified
    /// path or a bare name resolving to a known unit variant), never by casing.
    pub(super) fn let_identifier_is_unit_variant(&self, name: &str) -> bool {
        name.contains("::") || self.bare_identifier_resolves_to_unit_variant(name)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "statement checking covers many Stmt variants"
    )]
    pub(super) fn check_stmt(&mut self, stmt: &Stmt, span: &Span) {
        match stmt {
            Stmt::Let {
                pattern,
                ty,
                value,
                else_block,
            } => {
                let binding_context = match &pattern.0 {
                    Pattern::Identifier(name) => format!("local binding `{name}`"),
                    _ => "local binding".to_string(),
                };
                let deferred_hole_mark = self.deferred_inference_holes.len();
                let deferred_cast_mark = self.deferred_cast_checks.len();
                // Forward-bind for actor-lambda RHS: when `let name = actor |params| { body }`,
                // the body may reference `name` for recursive self-dispatch (architecture §5.9
                // ratification 2). Pre-bind the name with the Duplex type computed from
                // param/return annotations BEFORE synthesising the body so that the recursive
                // call resolves during body synthesis. The real `define_with_span` call below
                // overwrites this synthetic binding with the user-visible one.
                //
                // WHY: statement order synthesises the RHS before inserting the let-binding
                //   into scope; the actor body would see `fib` as undefined.
                // WHEN-OBSOLETE: if a general "let-rec" deferred-binding pass is added.
                // WHAT (real solution): a proper letrec/fix-point binder in the type checker.
                if ty.is_none() {
                    if let (
                        Pattern::Identifier(bind_name),
                        Some((
                            Expr::SpawnLambdaActor {
                                params,
                                return_type,
                                ..
                            },
                            _,
                        )),
                    ) = (&pattern.0, value)
                    {
                        let param_types: Vec<Ty> = params
                            .iter()
                            .map(|p| {
                                p.ty.as_ref().map_or_else(
                                    || Ty::Var(TypeVar::fresh()),
                                    |ann| self.resolve_type_expr(ann),
                                )
                            })
                            .collect();
                        let msg_ty = match param_types.len() {
                            0 => Ty::Unit,
                            1 => param_types.into_iter().next().unwrap(),
                            _ => Ty::Tuple(param_types),
                        };
                        let reply_ty = return_type
                            .as_ref()
                            .map_or(Ty::Unit, |ret| self.resolve_type_expr(ret));
                        let handle_ty = Ty::lambda_pid(msg_ty, reply_ty);
                        // Synthetic binding (no source span) — pre-populated for body lookup.
                        // Marked as already-used (read_count=1 in `define`) to avoid a
                        // spurious unused-variable warning at this site.
                        self.env.define(bind_name.clone(), handle_ty, false);
                    }
                }
                // Set pending_let_closure_name so synthesize_identifier can
                // detect recursive self-reference inside a closure body and emit
                // ClosureRecursive instead of UndefinedVariable.
                let prev_pending = self.pending_let_closure_name.take();
                if let (Pattern::Identifier(name), Some((Expr::Lambda { .. }, _))) =
                    (&pattern.0, &value)
                {
                    self.pending_let_closure_name = Some(name.clone());
                }
                let val_ty = if let Some((val, vs)) = value {
                    if let Some(annotation) = ty {
                        let expected =
                            self.resolve_annotation_with_holes(annotation, binding_context.clone());
                        let actual = self.check_against(val, vs, &expected);
                        self.annotated_binding_ty(&expected, actual)
                    } else {
                        self.synthesize(val, vs)
                    }
                } else if let Some(annotation) = ty {
                    self.resolve_annotation_with_holes(annotation, binding_context.clone())
                } else {
                    let v = TypeVar::fresh();
                    Ty::Var(v)
                };
                self.pending_let_closure_name = prev_pending;
                let val_ty = if ty.is_none() {
                    self.infer_integer_literal_binding_type(value.as_ref(), val_ty)
                } else {
                    val_ty
                };
                // Consume the scratch field unconditionally so stale state
                // never accumulates across statements.  Only register the
                // generic call signature in lambda_poly_sig_map when the binding value is
                // *directly* a generic lambda expression — indirect nesting
                // (a generic lambda buried inside a call argument, etc.) must
                // not be treated as a let-bound generic lambda.
                let generic_sig = self.last_lambda_generic_sig.take();
                let value_is_direct_generic_lambda = value.as_ref().is_some_and(|(val, _)| {
                    matches!(
                        val,
                        Expr::Lambda {
                            type_params: Some(tps),
                            ..
                        } if !tps.is_empty()
                    )
                });
                if ty.is_none() && !value_is_direct_generic_lambda {
                    let more_specific_hole_vars: Vec<_> = self.deferred_inference_holes
                        [deferred_hole_mark..]
                        .iter()
                        .flat_map(|hole| hole.hole_vars.iter().copied())
                        .chain(
                            self.deferred_cast_checks[deferred_cast_mark..]
                                .iter()
                                .flat_map(|check| check.target_hole_vars.iter().copied()),
                        )
                        .collect::<HashSet<_>>()
                        .into_iter()
                        .collect();
                    self.record_deferred_monomorphic_site(
                        &pattern.1,
                        &binding_context,
                        &val_ty,
                        more_specific_hole_vars,
                    );
                }
                // A constructor-like identifier (`None`, `E::A`) is a refutable
                // UNIT-VARIANT pattern, not a plain binding — route it to the
                // refutability gate below so a let-else records its resolution
                // and a plain `let` is rejected.
                //
                // Detection is by RESOLUTION, never by case, and must AGREE
                // with the use-side: a `::`-qualified path is unambiguously a
                // variant, and a bare identifier is a unit-variant pattern when
                // the use-side would resolve it to a known unit variant in the
                // global/builtin namespace (`None`, any user enum's unit
                // variant) — independent of the value type. Any other bare
                // identifier — even an uppercase one like `INF` or `Foo` — is a
                // fresh binding. This mirrors Rust pattern resolution: a name
                // that names a unit variant/const is always a pattern (a
                // mismatched value type is then a clean type error); otherwise
                // it binds.
                let identifier_is_unit_variant = match &pattern.0 {
                    Pattern::Identifier(name) => self.let_identifier_is_unit_variant(name),
                    _ => false,
                };
                // A let-position identifier that resolves to a unit variant is
                // a tag-test pattern (`let None = opt else { … }`), so the bare
                // spelling is refused here for the same reason it is in a match
                // arm. This site binds nothing and never reaches `bind_pattern`,
                // which is where every other pattern form is checked.
                if identifier_is_unit_variant {
                    if let Pattern::Identifier(name) = &pattern.0 {
                        if !name.contains("::") {
                            self.report_bare_variant_pattern(name, &pattern.1);
                        }
                    }
                }
                // For simple identifier patterns, track the definition span.
                // A unit-variant identifier is NOT a binding — it falls through
                // to the refutability gate below.
                let plain_identifier = match &pattern.0 {
                    Pattern::Identifier(name) if !identifier_is_unit_variant => Some(name),
                    _ => None,
                };
                if let Some(name) = plain_identifier {
                    if val_ty == Ty::Unit && value.is_some() && !name.starts_with('_') {
                        self.warnings.push(TypeError {
                            severity: crate::error::Severity::Warning,
                            kind: TypeErrorKind::StyleSuggestion,
                            span: span.clone(),
                            message: format!("binding `{name}` has unit type and carries no value"),
                            notes: vec![],
                            suggestions: vec!["prefix with underscore: `_name`".to_string()],
                            source_module: self.current_module.clone(),
                        });
                    }
                    self.check_shadowing(name, &pattern.1);
                    self.env.define_with_span(
                        name.clone(),
                        val_ty.clone(),
                        false,
                        pattern.1.clone(),
                    );
                    // Register generic lambda binding for call-site inference.
                    // Both guards must hold: the scratch field was populated
                    // AND the let value is itself (not just contains) a generic
                    // lambda expression.
                    if value_is_direct_generic_lambda {
                        if let Some(sig) = generic_sig {
                            self.lambda_poly_sig_map.insert(
                                SpanKey::in_module(&pattern.1, self.current_module_idx),
                                sig,
                            );
                        }
                    }
                    // Track let-bound numeric literals for later coercion at use
                    // sites. Only unannotated immutable bindings preserve the
                    // literal kind/value; explicit annotations and mutable vars
                    // materialize immediately.
                    if ty.is_none() {
                        if let Some((val, _)) = value {
                            if is_integer_literal(val) {
                                if let Some(v) = extract_integer_literal_value(val) {
                                    self.const_values
                                        .insert(name.clone(), ConstValue::Integer(v));
                                }
                            } else if val_ty.is_float_literal() {
                                if let Some(v) = extract_float_literal_value(val) {
                                    self.const_values.insert(name.clone(), ConstValue::Float(v));
                                }
                            }
                        }
                    }
                } else {
                    // Refutability gate for `let` (not in `bind_pattern`, which
                    // `match` arms share).  A `let` binding has no failure arm,
                    // so only irrefutable patterns are admitted.  Poison-guard:
                    // skip the gate when the value type is unresolved — the root
                    // error has already fired and cascades would obscure it.
                    let resolved_val_ty = self.subst.resolve(&val_ty);
                    let maybe_refutable_kind = match &pattern.0 {
                        // Irrefutable product types — admitted without a gate error.
                        Pattern::Struct { name: pat_name, .. } => {
                            let type_name = resolved_val_ty.type_name();
                            match type_name {
                                Some(tn) => {
                                    let td = self.lookup_type_def(tn);
                                    match td {
                                        Some(td)
                                            if matches!(
                                                td.kind,
                                                TypeDefKind::Record | TypeDefKind::Struct
                                            ) =>
                                        {
                                            // The pattern's written constructor name must
                                            // resolve to the SAME product type as the RHS.
                                            // `let Other { x } = Point { .. }` must NOT be
                                            // admitted as an irrefutable destructure just
                                            // because `Other` and `Point` share a field
                                            // shape — the written `Other` constructor would
                                            // otherwise never be enforced (see PR #2003).
                                            let pat_key = self
                                                .canonical_nominal_name(pat_name)
                                                .unwrap_or_else(|| pat_name.clone());
                                            let rhs_key = self
                                                .canonical_nominal_name(tn)
                                                .unwrap_or_else(|| tn.to_string());
                                            let pat_td = self.lookup_type_def(&pat_key);
                                            let matches_rhs =
                                                pat_td.as_ref().is_some_and(|_| pat_key == rhs_key);
                                            if !matches_rhs {
                                                // Report a mismatch and still return `None`
                                                // (no *additional* refutable-let error): the
                                                // reported error already fails compilation, and
                                                // bind_pattern runs below for error recovery.
                                                self.report_error(
                                                    TypeErrorKind::Mismatch {
                                                        expected: pat_name.clone(),
                                                        actual: td.name.clone(),
                                                    },
                                                    &pattern.1,
                                                    format!(
                                                        "let-destructuring pattern names \
                                                         type `{pat_name}`, but the value \
                                                         has type `{}`",
                                                        td.name
                                                    ),
                                                );
                                            }
                                            None
                                        } // irrefutable product type
                                        Some(_) => Some("enum variant"),
                                        None => {
                                            // Unknown type — checker already reported; allow
                                            // bind_pattern to run for error recovery.
                                            let _ = pat_name;
                                            None
                                        }
                                    }
                                }
                                None => None, // Ty::Var/Ty::Error — skip gate
                            }
                        }
                        // Enum-variant constructor (e.g. `Some(x)`) — always refutable.
                        Pattern::Constructor { .. } => Some("enum variant"),
                        // Qualified and contextual nominal paths retain their source
                        // spelling in the AST, but remain refutable variant patterns.
                        Pattern::NominalPath { .. } | Pattern::ContextVariant(_) => {
                            Some("enum variant")
                        }
                        // Unit variant written as a bare/qualified identifier
                        // (e.g. `None`, `E::A`) — refutable; binds nothing.
                        Pattern::Identifier(_) if identifier_is_unit_variant => {
                            Some("enum variant")
                        }
                        // Literal patterns are always refutable.
                        Pattern::Literal(_) => Some("literal"),
                        // Or-patterns are always refutable.
                        Pattern::Or(_, _) => Some("or-pattern"),
                        // All other patterns (Tuple, plain Identifier, Wildcard,
                        // Regex, …) — handled above or not refutable here.
                        _ => None,
                    };
                    match (maybe_refutable_kind, else_block) {
                        // Refutable pattern WITH an `else` clause: this is a
                        // let-else. The refutable pattern is admitted because
                        // the else clause supplies the failure path. The else
                        // block must diverge — it runs when the pattern fails
                        // and there is no value to bind, so control must not
                        // fall through to the binding. Check the else block
                        // BEFORE binding the pattern so the Ok-path binders are
                        // not visible inside it (they are bound only on the
                        // success path).
                        (Some(_), Some(else_blk)) => {
                            // Record-pattern plans carry the exact payload
                            // binding indices consumed by HIR. Prepare the
                            // plan without binding names so the failure block
                            // still cannot observe success-path bindings.
                            self.prepare_record_pattern_plan(&pattern.0, &val_ty, &pattern.1);
                            // Record the success-path pattern resolution so HIR
                            // lowering can consume the same `pattern_resolutions`
                            // side-table that powers `if let` / `match` /
                            // `while let`. Without this the let-else lowering
                            // finds no resolution and fails closed.
                            self.record_arm_resolution(&pattern.0, &pattern.1, &val_ty);
                            // The else block is the failure arm of a two-way
                            // branch whose success arm is the binding path that
                            // continues below. It must diverge, so whatever it
                            // consumes never reaches that path.
                            let entry = self.env.ownership_snapshot();
                            let else_ty = self.check_block(else_blk, None);
                            self.join_branch_ownership(
                                &entry,
                                &[
                                    BranchArmExit {
                                        ownership: entry.clone(),
                                        diverges: false,
                                    },
                                    BranchArmExit {
                                        ownership: self.env.ownership_snapshot(),
                                        diverges: Self::arm_skips_join_block(else_blk, &else_ty),
                                    },
                                ],
                            );
                            if !matches!(else_ty, Ty::Never)
                                && !matches!(resolved_val_ty, Ty::Var(_) | Ty::Error)
                            {
                                // Span the diagnostic on the else block tail.
                                // The trailing-expr / last-stmt span brackets
                                // it; fall back to the pattern span for an
                                // empty block (which is itself non-diverging).
                                let else_span = else_blk
                                    .trailing_expr
                                    .as_ref()
                                    .map(|e| e.1.clone())
                                    .or_else(|| else_blk.stmts.last().map(|(_, sp)| sp.clone()))
                                    .unwrap_or_else(|| pattern.1.clone());
                                self.report_error(
                                    TypeErrorKind::LetElseDoesNotDiverge,
                                    &else_span,
                                    "the `else` block of a `let … else` must \
                                     diverge (e.g. `return`, `break`, `continue`, or a \
                                     `!`-typed call); it must not fall through to the \
                                     binding"
                                        .to_string(),
                                );
                            }
                        }
                        // Refutable pattern with NO `else` clause: rejected. A
                        // plain `let` has no failure arm, so only irrefutable
                        // patterns are admitted. Suggest the let-else `else`
                        // clause (now that it exists) or `if let`/`match`.
                        (Some(kind_label), None) => {
                            // Only emit when the value type is actually resolved
                            // (not Ty::Var / Ty::Error) so a prior root error is
                            // not buried under a cascade.
                            if !matches!(resolved_val_ty, Ty::Var(_) | Ty::Error) {
                                self.report_error(
                                    TypeErrorKind::RefutableLetPattern {
                                        kind_label: kind_label.to_string(),
                                    },
                                    &pattern.1,
                                    format!(
                                        "refutable {kind_label} pattern is not allowed in a \
                                         plain `let`; add an `else {{ … }}` clause (it \
                                         must diverge), or use `if let`/`match`"
                                    ),
                                );
                            }
                        }
                        // Irrefutable pattern with an `else` clause: the else can
                        // never run, so divergence is not required. Type-check it
                        // for error coverage only.
                        (None, Some(else_blk)) => {
                            let _ = self.check_block(else_blk, None);
                        }
                        (None, None) => {}
                    }
                    // Call bind_pattern for error-recovery so payload binders
                    // exist and subsequent uses don't cascade into
                    // UnresolvedSymbol. Binders are defined into the CURRENT
                    // (enclosing) scope, so a let-else `let Ok(n) = … else { … };`
                    // makes `n` visible in the rest of the enclosing block.
                    //
                    // A unit-variant identifier (`None`, `E::A`) binds nothing —
                    // it is a refutable tag-test. Skip bind_pattern so it does
                    // not introduce a phantom binding (which would otherwise warn
                    // "unused variable `None`" and shadow the variant constructor).
                    if !identifier_is_unit_variant {
                        self.bind_pattern(&pattern.0, &val_ty, false, &pattern.1);
                    }
                }
            }
            Stmt::Var { name, ty, value } => {
                let binding_context = format!("local binding `{name}`");
                let deferred_hole_mark = self.deferred_inference_holes.len();
                let deferred_cast_mark = self.deferred_cast_checks.len();
                let val_ty = if let Some((val, vs)) = value {
                    if let Some(annotation) = ty {
                        let expected =
                            self.resolve_annotation_with_holes(annotation, binding_context.clone());
                        let actual = self.check_against(val, vs, &expected);
                        self.annotated_binding_ty(&expected, actual)
                    } else {
                        self.synthesize(val, vs)
                    }
                } else if let Some(annotation) = ty {
                    self.resolve_annotation_with_holes(annotation, binding_context.clone())
                } else {
                    let v = TypeVar::fresh();
                    Ty::Var(v)
                };
                let generic_sig = self.last_lambda_generic_sig.take();
                let val_ty = if ty.is_none() {
                    self.infer_integer_literal_binding_type(value.as_ref(), val_ty)
                        .materialize_literal_defaults()
                } else {
                    val_ty
                };
                if let Some((_, vs)) = value {
                    self.record_type(vs, &val_ty);
                }
                let value_is_direct_generic_lambda = value.as_ref().is_some_and(|(val, _)| {
                    matches!(
                        val,
                        Expr::Lambda {
                            type_params: Some(tps),
                            ..
                        } if !tps.is_empty()
                    )
                });
                if ty.is_none() && !value_is_direct_generic_lambda {
                    let more_specific_hole_vars: Vec<_> = self.deferred_inference_holes
                        [deferred_hole_mark..]
                        .iter()
                        .flat_map(|hole| hole.hole_vars.iter().copied())
                        .chain(
                            self.deferred_cast_checks[deferred_cast_mark..]
                                .iter()
                                .flat_map(|check| check.target_hole_vars.iter().copied()),
                        )
                        .collect::<HashSet<_>>()
                        .into_iter()
                        .collect();
                    self.record_deferred_monomorphic_site(
                        span,
                        &binding_context,
                        &val_ty,
                        more_specific_hole_vars,
                    );
                }
                self.check_shadowing(name, span);
                self.env
                    .define_with_span(name.clone(), val_ty, true, span.clone());
                if value_is_direct_generic_lambda {
                    if let Some(sig) = generic_sig {
                        self.lambda_poly_sig_map
                            .insert(SpanKey::in_module(span, self.current_module_idx), sig);
                    }
                }
            }
            Stmt::Assign { target, op, value } => {
                // Classify the assignment target for the side-table before synthesising
                // so that the entry is always emitted whenever the target is syntactically
                // valid, regardless of whether subsequent type-checking finds errors.
                let assign_target_kind: Option<AssignTargetKind> = match &target.0 {
                    Expr::Identifier(name) => {
                        if self.current_actor_fields.iter().any(|f| &f.name == name) {
                            Some(AssignTargetKind::ActorField)
                        } else if self.env.lookup_ref(name).is_some() {
                            Some(AssignTargetKind::LocalVar)
                        } else {
                            None
                        }
                    }
                    Expr::FieldAccess { .. } => Some(AssignTargetKind::FieldAccess),
                    Expr::Index { .. } => Some(AssignTargetKind::Index),
                    _ => {
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            span,
                            "invalid assignment target; expected identifier, field access, or \
                             index"
                                .to_string(),
                        );
                        None
                    }
                };
                if let Some(kind) = assign_target_kind {
                    self.assign_target_kinds
                        .insert(SpanKey::in_module(&target.1, self.current_module_idx), kind);
                }

                // Record fields follow the same write rule as other aggregate
                // fields: immutable roots (`let r`, parameters) reject at the
                // root mutability check below; mutable roots (`var r`) may be
                // updated in place.  Keep the record-specific diagnostic for
                // roots that are known immutable so users see the value-type
                // rule, not just a generic binding error.
                if let Expr::FieldAccess { object, field } = &target.0 {
                    // The object is the base of the target place, not a
                    // whole-value use: writing `h.sock` after `h.other` moved
                    // out is legal.
                    self.place_base_depth += 1;
                    let obj_ty = self.synthesize(&object.0, &object.1);
                    self.place_base_depth -= 1;
                    let resolved = self.subst.resolve(&obj_ty);
                    if let Ty::Named { name, .. } = &resolved {
                        let root_is_mutable = Self::assignment_root_binding_name(&target.0)
                            .is_some_and(|root| {
                                self.current_actor_fields.iter().any(|f| f.name == root)
                                    || self
                                        .env
                                        .lookup_ref(root)
                                        .is_some_and(|binding| binding.is_mutable)
                            });
                        if !root_is_mutable
                            && self
                                .lookup_type_def(name)
                                .is_some_and(|td| td.kind == TypeDefKind::Record)
                        {
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                span,
                                format!(
                                    "cannot assign to field `{field}` of record `{name}` through \
                                    an immutable binding; declare the binding mutable or use \
                                    functional update syntax `{name} {{ {field}: <value>, ..old }}` \
                                    instead"
                                ),
                            );
                        }
                    }
                }

                // Index targets (`obj[k] = rhs`) synthesise in assignment
                // context so the target type is the element/value type the RHS
                // must match — for `HashMap<K, V>` this is the bare `V` (a read
                // would instead yield `Option<V>`), and the checker records the
                // `hew_hashmap_insert_layout` runtime call at the index span.
                // Synthesising the target must not read it as a value: an
                // assignment overwrites the place, so a moved-out place is
                // exactly what a re-initialisation is allowed to name.
                self.place_write_depth += 1;
                let target_ty = match &target.0 {
                    Expr::Index { object, index } => {
                        let ty = self.synthesize_index(
                            object,
                            index,
                            &target.1,
                            IndexContext::AssignTarget,
                        );
                        // `synthesize_index` is called directly here (not via the
                        // `synthesize` dispatch tail), so stamp `expr_types` for
                        // the target span ourselves — downstream HIR/MIR read the
                        // checker-authoritative type at this site.
                        self.record_type(&target.1, &ty);
                        ty
                    }
                    _ => self.synthesize(&target.0, &target.1),
                };
                self.place_write_depth -= 1;
                // Record the type-shape metadata for every accepted target
                // immediately after synthesising the target type so the codegen
                // compound-assignment paths can read signedness without
                // falling back to the unreliable `resolvedTypeOf` path.
                if self
                    .assign_target_kinds
                    .contains_key(&SpanKey::in_module(&target.1, self.current_module_idx))
                {
                    let shape = AssignTargetShape {
                        is_unsigned: target_ty.is_unsigned(),
                    };
                    self.assign_target_shapes.insert(
                        SpanKey::in_module(&target.1, self.current_module_idx),
                        shape,
                    );
                }
                let root_binding_name = match &target.0 {
                    Expr::Identifier(_) | Expr::FieldAccess { .. } | Expr::Index { .. } => {
                        Self::assignment_root_binding_name(&target.0)
                    }
                    _ => None,
                };
                if let Some(name) = root_binding_name {
                    if let Some(binding) = self.env.lookup_ref(name) {
                        if !binding.is_mutable {
                            // Actor state fields get a field-specific
                            // diagnostic pointing at the declaration site;
                            // plain locals keep the variable-shaped error.
                            // In `init { }` fields are bound writable, so
                            // this arm only fires in handler/method/hook
                            // bodies.
                            if let Some(field) =
                                self.current_actor_fields.iter().find(|f| f.name == *name)
                            {
                                self.errors.push(TypeError::immutable_field_assignment(
                                    span.clone(),
                                    name,
                                    field.decl_span.clone(),
                                ));
                            } else {
                                // Suggest `var` only when this exact projection
                                // reaches storage the caller shares. A root
                                // type can contain both kinds of storage:
                                // `holder.items[0]` reaches a Vec allocation,
                                // while `holder.count` and replacing
                                // `holder.items` mutate only the private Holder
                                // copy. Keying help on the root type would steer
                                // the latter cases back into the silent trap.
                                let ineffective_var_param = if binding.is_param() {
                                    let binding_ty = self.subst.resolve(&binding.ty);
                                    let has_visible_projection =
                                        self.param_ty_has_caller_visible_projection(&binding_ty);
                                    (self.param_var_has_no_caller_visible_effect(&binding_ty)
                                        || (has_visible_projection
                                            && !self
                                                .mutation_projection_reaches_caller_visible_storage(
                                                    &target.0,
                                                )))
                                    .then(|| binding_ty.user_facing().to_string())
                                } else {
                                    None
                                };
                                let error = match ineffective_var_param {
                                    Some(ty) => TypeError::value_param_mutability_error(
                                        span.clone(),
                                        name,
                                        &ty,
                                    ),
                                    None => TypeError::mutability_error(span.clone(), name),
                                };
                                self.errors.push(error);
                            }
                        } else if binding.is_param() && !binding.is_receiver() {
                            let binding_ty = self.subst.resolve(&binding.ty);
                            // Value aggregates that contain a collection are
                            // admitted at the declaration because some
                            // projections genuinely reach shared storage.
                            // Reject a concrete write that stays on the private
                            // side of that boundary.
                            if self.param_ty_has_caller_visible_projection(&binding_ty)
                                && !self
                                    .mutation_projection_reaches_caller_visible_storage(&target.0)
                            {
                                self.report_error_with_suggestions(
                                    TypeErrorKind::MutabilityError,
                                    span,
                                    format!(
                                        "`var {name}` on a by-value parameter of type `{}` \
                                         has no caller-visible effect",
                                        binding_ty.user_facing()
                                    ),
                                    vec![
                                        "return the modified value to the caller".to_string(),
                                        "mutate through a shared collection projection instead"
                                            .to_string(),
                                    ],
                                );
                            }
                        }
                    }
                    // Plain assignment (=) is a write-only, not a read.
                    // Compound assignment (+=, etc.) is both read and write.
                    // Must unmark BEFORE mark_written so the guard check works.
                    if op.is_none() {
                        self.env.unmark_used(name);
                    }
                    self.env.mark_written(name);
                }
                let value_ty = self.check_against(&value.0, &value.1, &target_ty);
                // An unannotated literal binding (`var best = 0`) carries a
                // literal-defaulting `Ty::Var` that `check_against` cannot
                // promote: it resolves the expected type first, materializing
                // `IntLiteral` and losing the var identity, so a concrete
                // narrower RHS (`best = dp[i]` with `dp: Vec<i32>`) was
                // accepted by literal-compat while the binding later defaulted
                // to `i64` — handing MIR a Move whose source and destination
                // widths disagree (codegen-front fail-closed). Unify the
                // binding's OWN var with the concrete RHS width here, the same
                // promotion the range-bound machinery performs, so the binding
                // adopts the assigned width. A second assignment at a different
                // width then fails `check_against`'s ordinary implicit-convert
                // gate instead of drifting.
                if let Expr::Identifier(name) = &target.0 {
                    if let Some(binding) = self.env.lookup_ref(name) {
                        if let binding_ty @ Ty::Var(_) = binding.ty.clone() {
                            let value_resolved = self.subst.resolve(&value_ty);
                            if self.subst.resolve(&binding_ty).is_numeric_literal()
                                && !value_resolved.is_numeric_literal()
                                && value_resolved.is_numeric()
                                && !Self::numeric_update_reads_binding(&value.0, name)
                            {
                                let _ = self.try_unify_inference_with_owner_identity(
                                    &value_resolved,
                                    &binding_ty,
                                );
                            }
                        }
                    }
                }
                // A plain `=` gives the target place a fresh owner, discharging
                // the consume obligation on it and on everything under it. This
                // is the escape hatch for a consumed actor state field: the
                // handler plugs the hole it made. Compound assignment reads the
                // old value first, so it discharges nothing.
                //
                // Evaluated after the RHS so `sock = take_from(sock)` still
                // reports the read.
                if op.is_none() {
                    if let Some((root, path)) = Checker::expr_place(&target.0) {
                        self.env.reinit_place(&root, &path);
                    }
                }
            }
            Stmt::Expression((expr, es)) => {
                if let Expr::MethodCall {
                    receiver,
                    method,
                    args: _,
                } = expr
                {
                    if method == "set" {
                        if let Some(name) = Self::assignment_root_binding_name(&receiver.0) {
                            self.env.mark_written(name);
                        }
                    }
                }
                self.synthesize_discarded_expression(expr, es);
            }
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                self.check_discarded_if_chain(condition, then_block, else_block.as_ref());
            }
            Stmt::IfLet {
                pattern,
                expr,
                body,
                else_body,
            } => {
                let scr_ty = self.synthesize(&expr.0, &expr.1);
                if self.reject_unsupported_iflet_pattern(&pattern.0, &pattern.1) {
                    return;
                }
                let entry = self.env.ownership_snapshot();
                self.env.push_scope();
                self.bind_pattern(&pattern.0, &scr_ty, false, &pattern.1);
                // Record the pattern resolution so HIR lowering can consume
                // the same `pattern_resolutions` side-table that powers
                // `WhileLet` and `Match` lowering — without this entry HIR
                // cannot resolve the constructor's `(type_name, variant_name)`
                // identity or payload-binding field indices for `if-let`.
                self.record_arm_resolution(&pattern.0, &pattern.1, &scr_ty);
                let then_ty = self.check_block(body, None);
                let then_exit = BranchArmExit {
                    ownership: self.env.ownership_snapshot(),
                    diverges: Self::arm_skips_join_block(body, &then_ty),
                };
                self.env.pop_scope();
                if let Some(block) = else_body {
                    self.env.restore_ownership(&entry);
                    let else_ty = self.check_block(block, None);
                    let else_skips = Self::arm_skips_join_block(block, &else_ty);
                    self.join_two_way(&entry, then_exit, else_skips);
                } else {
                    self.join_fall_through(&entry, then_exit);
                }
            }
            Stmt::Return(value) => {
                // Fail-closed crash-hook gate inventory (all positions covered by
                // the shared `check_return_operand` shell):
                //   (1) non-final `return CrashAction`  → THIS site
                //   (2) final/tail `return CrashAction` → check_stmt_as_expr site
                //   (3) tail-expr CrashAction (no return keyword) → items.rs body_is_crash_action
                //   (4) if/match expr whose arms all yield CrashAction → flows into (3)
                //   (5) let-bound CrashAction, then returned → flows into (1) or (2)
                self.check_return_operand(value.as_ref(), span);
            }
            Stmt::Loop { label, body } => {
                if let Some(lbl) = label {
                    self.loop_labels.push(lbl.clone());
                }
                self.loop_depth += 1;
                self.env.enter_loop(label.as_deref());
                self.check_block(body, None);
                self.env.exit_loop();
                self.loop_depth -= 1;
                if label.is_some() {
                    self.loop_labels.pop();
                }
            }
            Stmt::For {
                label,
                pattern,
                iterable,
                body,
                is_await,
            } => {
                let iter_ty = self.synthesize(&iterable.0, &iterable.1);
                // Infer element type from iterable, and enforce `for await` restrictions.
                let elem_ty = match &iter_ty {
                    Ty::Array(inner, _) | Ty::Slice(inner) => {
                        if *is_await {
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                &iterable.1,
                                "`for await` is not valid over an Array or Slice; \
                                 use a plain `for` loop"
                                    .to_string(),
                            );
                        }
                        (**inner).clone()
                    }
                    Ty::Named {
                        builtin: Some(BuiltinType::Range),
                        args,
                        ..
                    } if args.len() == 1 => {
                        if *is_await {
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                &iterable.1,
                                "`for await` is not valid over a Range; \
                                 use a plain `for` loop"
                                    .to_string(),
                            );
                        }
                        args[0].clone()
                    }
                    Ty::Named {
                        builtin: Some(BuiltinType::Stream),
                        args,
                        ..
                    } => {
                        let inner_opt = args.first().cloned();
                        if *is_await {
                            if args.is_empty() {
                                self.report_error(
                                    TypeErrorKind::InvalidOperation,
                                    &iterable.1,
                                    "`for await` over a stream requires a resolved element type"
                                        .to_string(),
                                );
                                Ty::Error
                            } else if let Some(method_name) =
                                self.for_await_actor_method_name(&iterable.0)
                            {
                                // SAFETY: args is non-empty (checked above)
                                let inner = inner_opt.unwrap();
                                if self.receive_generator_methods.contains(&method_name) {
                                    let resolved_inner = self.subst.resolve(&inner);
                                    if resolved_inner.has_inference_var() {
                                        self.report_error(
                                            TypeErrorKind::InvalidOperation,
                                            &iterable.1,
                                            "`for await` over a generator receive fn requires a resolved element type"
                                                .to_string(),
                                        );
                                        Ty::Error
                                    } else {
                                        resolved_inner
                                    }
                                } else {
                                    self.report_error(
                                        TypeErrorKind::InvalidOperation,
                                        &iterable.1,
                                        format!(
                                            "`for await` over actor method `{method_name}` requires a `receive gen fn`"
                                        ),
                                    );
                                    Ty::Error
                                }
                            } else {
                                match self.validate_stream_sink_element_type(
                                    args,
                                    BuiltinNamedType::Stream.canonical_name(),
                                    "next",
                                    &iterable.1,
                                ) {
                                    Some(validated_inner) => {
                                        // Stream runtime is native-only in v0.5. Method-call
                                        // `.recv()` already rejects on wasm; `for await` must
                                        // mirror that checker gate before HIR desugars it.
                                        // WASM-TODO(suspending-receive): port the shared stream/channel suspend carrier.
                                        self.reject_wasm_feature(
                                            &iterable.1,
                                            WasmUnsupportedFeature::Streams,
                                        );
                                        let resolved = self.subst.resolve(&validated_inner);
                                        if !matches!(resolved, Ty::Var(_))
                                            && !self.queue_elem_admissible(&resolved)
                                        {
                                            let reason =
                                                self.queue_elem_rejection_reason(&resolved);
                                            self.report_error(
                                                TypeErrorKind::InvalidOperation,
                                                &iterable.1,
                                                format!(
                                                    "`Stream<{}>` is not supported in \
                                                     `for await`: {reason}",
                                                    validated_inner.user_facing()
                                                ),
                                            );
                                            Ty::Error
                                        } else {
                                            validated_inner
                                        }
                                    }
                                    None => Ty::Error,
                                }
                            }
                        } else if let Some(inner) = inner_opt {
                            inner
                        } else {
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                &iterable.1,
                                "`for` over a Stream requires a resolved element type".to_string(),
                            );
                            Ty::Error
                        }
                    }
                    Ty::Named {
                        builtin: Some(BuiltinType::Vec),
                        args,
                        ..
                    } => {
                        if *is_await {
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                &iterable.1,
                                "`for await` is not valid over a Vec; \
                                 use a plain `for` loop"
                                    .to_string(),
                            );
                        }
                        if let Some(elem) = args.first().cloned() {
                            if matches!(self.subst.resolve(&elem), Ty::TraitObject { .. }) {
                                self.report_error(
                                    TypeErrorKind::InvalidOperation,
                                    &iterable.1,
                                    "direct `for` over `Vec<dyn Trait>` would require a cloneable \
                                     borrowed snapshot; use `for value in values.into_iter()` to \
                                     consume and move each trait object instead"
                                        .to_string(),
                                );
                                Ty::Error
                            } else if self.validate_vec_iter_element_clone_type(&elem, &iterable.1)
                            {
                                elem
                            } else {
                                Ty::Error
                            }
                        } else {
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                &iterable.1,
                                "`for` over a Vec requires a resolved element type".to_string(),
                            );
                            Ty::Error
                        }
                    }
                    Ty::Named {
                        args,
                        builtin: Some(BuiltinType::VecIter),
                        ..
                    } if !args.is_empty() => {
                        if *is_await {
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                &iterable.1,
                                "`for await` is not valid over a VecIter; \
                                 use a plain `for` loop"
                                    .to_string(),
                            );
                        }
                        let elem = args[0].clone();
                        if self.validate_vec_iter_element_clone_type(&elem, &iterable.1) {
                            elem
                        } else {
                            Ty::Error
                        }
                    }
                    Ty::Named {
                        builtin: Some(BuiltinType::HashMap),
                        args,
                        ..
                    } if args.len() >= 2 => {
                        if *is_await {
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                &iterable.1,
                                "`for await` is not valid over a HashMap; \
                                 use a plain `for` loop"
                                    .to_string(),
                            );
                        }
                        // `for (k, v) in m` desugars (in HIR) to a `HashMapIter`
                        // cursor built from `m.keys()` and `m.values()`. Both
                        // projections must be lowerable for the key/value types,
                        // and the resolved-call facts (plus matching expr_types,
                        // the HIR boundary's totality contract) must exist where
                        // the HIR synthesis emits the two calls. `resolved_calls`
                        // and `expr_types` are keyed by span, so the projections
                        // cannot share a span with each other OR with the iterable
                        // expression: `keys` and `values` are recorded at distinct
                        // synthetic zero-width spans anchored at the iterable's
                        // start/end offsets (distinct from every real expression
                        // span, reproduced byte-for-byte by the HIR desugar via
                        // `Self::hashmap_for_in_keys_span`/`..values_span`). This
                        // leaves `expr_types[iterable_span]` holding the iterable's
                        // true HashMap type so HIR routes non-identifier sources
                        // (field/call/index) to the HashMap arm, not the Vec arm.
                        let key_ty = args[0].clone();
                        let val_ty = args[1].clone();
                        let keys_span = Self::hashmap_for_in_keys_span(&iterable.1);
                        let values_span = Self::hashmap_for_in_values_span(&iterable.1);
                        if self.validate_hashmap_projection_element_types(
                            &key_ty, &val_ty, "keys", &keys_span,
                        ) && self.validate_hashmap_projection_element_types(
                            &key_ty,
                            &val_ty,
                            "values",
                            &values_span,
                        ) {
                            let key_vec = self.make_vec_type(key_ty.clone(), &keys_span);
                            let val_vec = self.make_vec_type(val_ty.clone(), &values_span);
                            self.record_type(&keys_span, &key_vec);
                            self.record_type(&values_span, &val_vec);
                            self.record_resolved_hashmap_call("keys", &key_ty, &val_ty, &keys_span);
                            self.record_resolved_hashmap_call(
                                "values",
                                &key_ty,
                                &val_ty,
                                &values_span,
                            );
                        }
                        Ty::Tuple(vec![key_ty, val_ty])
                    }
                    Ty::Named {
                        builtin: Some(BuiltinType::HashSet),
                        args,
                        ..
                    } if !args.is_empty() => {
                        if *is_await {
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                &iterable.1,
                                "`for await` is not valid over a HashSet; \
                                 use a plain `for` loop"
                                    .to_string(),
                            );
                        }
                        // `for x in s` desugars (in HIR) to a `VecIter` over the
                        // set's `to_vec()` element snapshot. Record the `to_vec`
                        // resolved-call fact (+ matching expr_type) at a synthetic
                        // zero-width span (NOT the iterable span — see
                        // `hashset_for_in_to_vec_span`) so the HIR synthesis
                        // lowers the projection to `hew_hashset_to_vec_layout`
                        // while `expr_types[iterable_span]` keeps the iterable's
                        // true HashSet type. Recording at the iterable span would
                        // clobber it with `Vec<T>` and mis-route non-identifier
                        // sources (`for x in self.s`) to the Vec arm — a silent
                        // wrong-value pass (the VecIter reads the HashSet handle as
                        // a zero-length Vec).
                        let elem_ty = args[0].clone();
                        let to_vec_span = Self::hashset_for_in_to_vec_span(&iterable.1);
                        if self.validate_vec_iter_element_clone_type(&elem_ty, &iterable.1)
                            && self.validate_hashset_element_type(&elem_ty, &to_vec_span)
                        {
                            let elem_vec = self.make_vec_type(elem_ty.clone(), &to_vec_span);
                            self.record_type(&to_vec_span, &elem_vec);
                            self.record_resolved_hashset_call("to_vec", &elem_ty, &to_vec_span);
                            elem_ty
                        } else {
                            Ty::Error
                        }
                    }
                    Ty::Named {
                        builtin: Some(BuiltinType::Receiver),
                        args,
                        ..
                    } if !args.is_empty() => {
                        let inner = args[0].clone();
                        if *is_await {
                            self.check_receiver_element_type_for_await(&inner, &iterable.1);
                        }
                        inner
                    }
                    // Propagate already-errored or divergent iterable expressions
                    // without adding a redundant "type is not iterable" diagnostic.
                    Ty::Error => Ty::Error,
                    Ty::Never => Ty::Never,
                    _ => {
                        if let Some(item_ty) = self.iterator_trait_item_ty(&iter_ty, &iterable.1) {
                            item_ty
                        } else {
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                &iterable.1,
                                "type is not iterable".to_string(),
                            );
                            Ty::Error
                        }
                    }
                };
                self.env.push_scope();
                self.in_for_binding = true;
                self.bind_pattern(&pattern.0, &elem_ty, true, &pattern.1);
                self.in_for_binding = false;
                if let Some(lbl) = label {
                    self.loop_labels.push(lbl.clone());
                }
                self.loop_depth += 1;
                self.env.enter_loop(label.as_deref());
                self.check_block(body, None);
                self.env.exit_loop();
                self.loop_depth -= 1;
                if label.is_some() {
                    self.loop_labels.pop();
                }
                self.env.pop_scope();
            }
            Stmt::While {
                label,
                condition,
                body,
            } => {
                // Detect `while true` — suggest `loop` instead
                if matches!(&condition.0, Expr::Literal(Literal::Bool(true))) {
                    self.warnings.push(TypeError {
                        severity: crate::error::Severity::Warning,
                        kind: TypeErrorKind::StyleSuggestion,
                        span: span.clone(),
                        message: "`while true` can be simplified".to_string(),
                        notes: vec![],
                        suggestions: vec![
                            "use `loop { ... }` instead of `while true { ... }`".to_string()
                        ],
                        source_module: self.current_module.clone(),
                    });
                }
                self.check_against(&condition.0, &condition.1, &Ty::Bool);
                if let Some(lbl) = label {
                    self.loop_labels.push(lbl.clone());
                }
                self.loop_depth += 1;
                self.env.enter_loop(label.as_deref());
                self.check_block(body, None);
                self.env.exit_loop();
                self.loop_depth -= 1;
                if label.is_some() {
                    self.loop_labels.pop();
                }
            }
            Stmt::WhileLet {
                label,
                pattern,
                expr,
                body,
            } => {
                let scr_ty = self.synthesize(&expr.0, &expr.1);
                if self.reject_unsupported_iflet_pattern(&pattern.0, &pattern.1) {
                    return;
                }
                self.env.push_scope();
                self.bind_pattern(&pattern.0, &scr_ty, false, &pattern.1);
                // Record the pattern resolution so HIR lowering can consume
                // the same `pattern_resolutions` side-table that powers
                // `Match` lowering — without this entry HIR cannot resolve
                // the constructor's `(type_name, variant_name)` identity or
                // payload-binding field indices for `while-let`.
                self.record_arm_resolution(&pattern.0, &pattern.1, &scr_ty);
                if let Some(lbl) = label {
                    self.loop_labels.push(lbl.clone());
                }
                self.loop_depth += 1;
                self.env.enter_loop(label.as_deref());
                self.check_block(body, None);
                self.env.exit_loop();
                self.loop_depth -= 1;
                if label.is_some() {
                    self.loop_labels.pop();
                }
                self.env.pop_scope();
            }
            Stmt::Break { label, value } => {
                if self.loop_depth == 0 {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::InvalidOperation,
                        span.clone(),
                        "break used outside of a loop",
                    ));
                } else if let Some(lbl) = label {
                    if !self.loop_labels.contains(lbl) {
                        self.errors.push(TypeError::new(
                            TypeErrorKind::InvalidOperation,
                            span.clone(),
                            format!("unknown loop label `@{lbl}`"),
                        ));
                    }
                }
                if let Some((val_expr, val_span)) = value {
                    self.synthesize(val_expr, val_span);
                }
                if self.loop_depth > 0 {
                    self.recheck_loop_edge_defers(label.as_deref(), span);
                }
            }
            Stmt::Continue { label } => {
                if self.loop_depth == 0 {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::InvalidOperation,
                        span.clone(),
                        "continue used outside of a loop",
                    ));
                } else if let Some(lbl) = label {
                    if !self.loop_labels.contains(lbl) {
                        self.errors.push(TypeError::new(
                            TypeErrorKind::InvalidOperation,
                            span.clone(),
                            format!("unknown loop label `@{lbl}`"),
                        ));
                    }
                }
                if self.loop_depth > 0 {
                    self.recheck_loop_edge_defers(label.as_deref(), span);
                }
            }
            Stmt::Match { scrutinee, arms } => {
                let scr_ty = self.synthesize(&scrutinee.0, &scrutinee.1);
                self.check_match_stmt(&scr_ty, arms, span);
            }
            Stmt::Defer(expr) => {
                let ownership = self.env.ownership_snapshot();
                self.synthesize(&expr.0, &expr.1);
                self.env.restore_ownership(&ownership);
                if !self.env.register_defer(*expr.clone()) {
                    self.errors.push(
                        TypeError::new(
                            TypeErrorKind::InvalidOperation,
                            span.clone(),
                            "cannot register defer without an active lexical scope",
                        )
                        .with_suggestion(
                            "place the defer inside a function or block scope".to_string(),
                        ),
                    );
                }
            }
        }
    }

    pub(super) fn check_match_stmt(&mut self, scrutinee_ty: &Ty, arms: &[MatchArm], span: &Span) {
        for arm in arms {
            self.env.push_scope();
            self.bind_pattern(&arm.pattern.0, scrutinee_ty, false, &arm.pattern.1);
            self.record_arm_resolution(&arm.pattern.0, &arm.pattern.1, scrutinee_ty);

            if let Some((guard, gs)) = &arm.guard {
                self.check_against(guard, gs, &Ty::Bool);
            }

            self.synthesize(&arm.body.0, &arm.body.1);
            self.env.pop_scope();
        }

        self.check_exhaustiveness(scrutinee_ty, arms, span);
    }
}
