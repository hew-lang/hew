//! Iterator shapes, `for` loop desugaring and range adapters.

use super::*;

/// The base range plus resolved iteration adapters peeled from a for-loop
/// iterable by [`LowerCtx::peel_range_adapter_chain`].  Borrows the AST
/// sub-expressions; lowered into a single `HirExprKind::ForRange`.
pub(super) struct RangeAdapterChain<'a> {
    /// Left bound of the base `..` / `..=` range literal.
    pub(super) range_start: &'a Spanned<Expr>,
    /// Right bound of the base range literal.
    pub(super) range_end: &'a Spanned<Expr>,
    /// `true` for an inclusive (`..=`) upper bound.
    pub(super) inclusive: bool,
    /// `true` when the chain reverses iteration direction (`.rev()`).
    pub(super) descending: bool,
    /// The `.step_by(k)` stride argument when present; `None` defaults to a
    /// stride of `1`.
    pub(super) step_expr: Option<&'a Spanned<Expr>>,
    /// `true` when a `.step_by(k)` precedes a `.rev()` in the chain
    /// (`(a..b).step_by(k).rev()`).  That order is unsupported: `.rev()` and
    /// `.step_by(k)` do not commute, so folding them into an order-insensitive
    /// `{descending, step}` pair would silently miscompile (the descending
    /// counter would start at the raw high bound rather than the last strided
    /// element).  The caller rejects the chain fail-closed instead of emitting
    /// a wrong sequence.  Only `(a..b).rev()?.step_by(k)?` is supported.
    pub(super) step_before_rev: bool,
}

impl LowerCtx {
    pub(super) fn make_direct_method_call(
        &mut self,
        callee_name: String,
        receiver: HirExpr,
        ret_ty: &ResolvedTy,
        span: Span,
    ) -> HirExpr {
        // Mirror `lower_identifier`: a registry entry carrying a typed
        // builtin family resolves to `ResolvedRef::Builtin(family)` so the
        // for-await recv desugar's layout-witness callees reach MIR with
        // their family (the codegen `Terminator::Call` intercepts dispatch
        // on it — a bare Item resolution here is the producer gap the
        // intercept backstop refuses).
        let resolved_ref =
            self.fn_registry
                .get(&callee_name)
                .map_or(ResolvedRef::Unresolved, |entry| {
                    entry
                        .builtin_family
                        .map_or(ResolvedRef::Item(entry.id), ResolvedRef::Builtin)
                });
        let callee_ty = ResolvedTy::Function {
            capabilities: hew_types::CallableCapabilities::FUNCTION_ITEM,
            params: Vec::new(),
            ret: Box::new(ret_ty.clone()),
        };
        let target = self.registered_symbol_target(&callee_name);
        let callee = HirExpr {
            node: self.ids.node(),
            site: self.ids.site(),
            ty: callee_ty,
            intent: IntentKind::Read,
            kind: HirExprKind::BindingRef {
                name: callee_name,
                resolved: resolved_ref,
            },
            span: span.clone(),
        };
        let site = self.ids.site();
        HirExpr {
            node: self.ids.node(),
            site,
            ty: ret_ty.clone(),
            intent: IntentKind::Read,
            kind: HirExprKind::Call {
                target,
                callee: Box::new(callee),
                args: vec![receiver],
                evaluation_order: Vec::new(),
            },
            span,
        }
    }

    fn checked_for_type(&mut self, ty: &Ty, role: &str, span: &Span) -> Option<ResolvedTy> {
        match ResolvedTy::from_ty(ty) {
            Ok(resolved) => Some(resolved),
            Err(error) => {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::CheckerBoundaryViolation {
                        name: format!("for loop {role}"),
                        reason: error.to_string(),
                    },
                    span.clone(),
                    "checked iterator type failed boundary conversion",
                ));
                None
            }
        }
    }

    /// Lower the checker's selected implicit call without replaying lookup or
    /// attaching a synthetic call to an authored expression span.
    fn lower_checked_for_call(
        &mut self,
        call: &hew_types::CheckedForMethod,
        mut receiver: HirExpr,
        span: &Span,
    ) -> Option<HirExpr> {
        let ret_ty = self.checked_for_type(&call.return_type, "call result", span)?;
        receiver.intent = if call.requires_mutable_receiver || call.consumes_receiver {
            IntentKind::Consume
        } else {
            IntentKind::Read
        };
        let receiver_ty = receiver.ty.clone();
        let trait_args = call
            .trait_args
            .iter()
            .map(|ty| self.checked_for_type(ty, "trait argument", span))
            .collect::<Option<Vec<_>>>()?;
        let kind = if let Some(receiver_type_param) = call.receiver_parameter {
            if call.requires_mutable_receiver {
                HirExprKind::VarSelfMethodCall {
                    receiver_update: call.receiver_update,
                    receiver: Box::new(receiver),
                    call_target: call.target.clone(),
                    target: HirVarSelfMethodTarget::StaticTrait {
                        receiver_type_param,
                        trait_args,
                    },
                    args: Vec::new(),
                    evaluation_order: Vec::new(),
                    ret_ty: ret_ty.clone(),
                    receiver_ty: receiver_ty.clone(),
                }
            } else {
                self.make_static_trait_dispatch_call(
                    crate::StaticTraitSelf::Receiver(Box::new(receiver)),
                    trait_args,
                    call.target.clone(),
                    receiver_type_param,
                    LoweredCallArgs {
                        args: Vec::new(),
                        evaluation_order: Vec::new(),
                    },
                    ret_ty.clone(),
                    span,
                )
            }
        } else {
            let hew_types::CallTarget::ImplMethod(declaration) = call.target else {
                return None;
            };
            let symbol = self.registered_impl_method_symbol(declaration)?;
            if call.requires_mutable_receiver {
                HirExprKind::VarSelfMethodCall {
                    receiver_update: call.receiver_update,
                    receiver: Box::new(receiver),
                    call_target: call.target.clone(),
                    target: HirVarSelfMethodTarget::Direct,
                    args: Vec::new(),
                    evaluation_order: Vec::new(),
                    ret_ty: ret_ty.clone(),
                    receiver_ty: receiver_ty.clone(),
                }
            } else {
                let mut direct =
                    self.make_direct_method_call(symbol.clone(), receiver, &ret_ty, span.clone());
                if let HirExprKind::Call { target, .. } = &mut direct.kind {
                    *target = call.target.clone();
                }
                self.record_method_call_type_args(
                    &symbol,
                    &call.target,
                    &receiver_ty,
                    span,
                    None,
                    direct.site,
                );
                return Some(direct);
            }
        };
        let result = self.make_expr(kind, ret_ty, IntentKind::Read, span.clone());
        if let hew_types::CallTarget::ImplMethod(declaration) = call.target {
            let symbol = self.registered_impl_method_symbol(declaration)?;
            self.record_method_call_type_args(
                &symbol,
                &call.target,
                &receiver_ty,
                span,
                None,
                result.site,
            );
        }
        Some(result)
    }

    /// Whether a for-in iterable expression is a *place* — an lvalue that can be
    /// read more than once with no observable side effect and no extra
    /// evaluation cost (an identifier, or a field/index projection rooted in a
    /// place).
    pub(super) fn for_in_iterable_is_place(expr: &Expr) -> bool {
        match expr {
            Expr::Ident(_) => true,
            Expr::FieldAccess { object, .. } | Expr::Index { object, .. } => {
                Self::for_in_iterable_is_place(&object.0)
            }
            _ => false,
        }
    }

    /// Peel a `(a..b).rev()` / `.step_by(k)` adapter chain off a for-loop
    /// iterable down to its base range literal.
    ///
    /// Returns `Some` for a plain range (`a..b` / `a..=b`, no adapters) and for
    /// any chain of `.rev()` / `.step_by(k)` adapters terminating in a range
    /// literal; returns `None` for any other iterable shape (Vec, a bare
    /// identifier, a user method that is not a range adapter, …), which then
    /// flows to `lower_for_iter_desugar`.
    ///
    /// Adapter semantics are left-to-right as written and well-defined on the
    /// `ForRange` primitive:
    ///
    /// - `.rev()` flips the iteration direction (`descending`).
    /// - `.step_by(k)` sets the stride magnitude (`step`); the *last* stride
    ///   in the chain wins.
    ///
    /// The MIR counting loop then initialises the counter at the high bound for
    /// a descending range and at the low bound otherwise, advancing by `step`
    /// each iteration.  This makes `(0..5).rev()` yield `4 3 2 1 0` and
    /// `(0..=10).rev().step_by(3)` yield `10 7 4 1`.
    ///
    /// `.rev()` and `.step_by(k)` do **not** commute, so only `.rev()` before
    /// `.step_by(k)` (`(a..b).rev()?.step_by(k)?`) is supported: descend from
    /// the high bound, then stride.  A `.step_by(k)` *before* a `.rev()` would
    /// have to start the descending sequence at the last strided element, which
    /// the order-insensitive `{descending, step}` fold cannot express; the
    /// chain records `step_before_rev` and the caller rejects it fail-closed
    /// rather than emit a wrong sequence.
    pub(super) fn peel_range_adapter_chain(iterable: &Expr) -> Option<RangeAdapterChain<'_>> {
        match iterable {
            Expr::Binary {
                op: op @ (BinaryOp::Range | BinaryOp::RangeInclusive),
                left,
                right,
            } => Some(RangeAdapterChain {
                range_start: left,
                range_end: right,
                inclusive: matches!(op, BinaryOp::RangeInclusive),
                descending: false,
                step_expr: None,
                step_before_rev: false,
            }),
            Expr::MethodCall {
                receiver,
                method,
                args,
            } => {
                let mut chain = Self::peel_range_adapter_chain(&receiver.0)?;
                match method.0.name.as_str() {
                    "rev" => {
                        // `.rev()` carries no arguments (the checker rejects any).
                        // Reversing twice is the forward direction again.
                        chain.descending = !chain.descending;
                        // `.step_by(k)` before `.rev()` does not commute with
                        // the order-insensitive `{descending, step}` fold: the
                        // descending counter would start at the raw high bound
                        // instead of the last strided element.  Flag it so the
                        // caller rejects fail-closed rather than miscompile.
                        if chain.step_expr.is_some() {
                            chain.step_before_rev = true;
                        }
                        Some(chain)
                    }
                    "step_by" => {
                        // The last stride in the chain wins; the checker has
                        // already validated arity (exactly one argument).
                        let step = args.first()?;
                        chain.step_expr = Some(step.expr());
                        Some(chain)
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }

    /// `for c in s` over `string` and `for b in raw` over `bytes`.
    ///
    /// Both lower to `for __i in 0..seq.len() { let c = seq[__i]; body }` over
    /// a single-evaluation binding of the sequence. `string` indexes
    /// codepoints and `bytes` indexes bytes, matching `s[i]` exactly.
    #[expect(
        clippy::too_many_lines,
        clippy::too_many_arguments,
        reason = "one linear expansion: source temp, length, counter, element binding and loop"
    )]
    pub(super) fn lower_for_sequence_index_desugar(
        &mut self,
        sequence: HirExpr,
        element: (&str, &Span, &ResolvedTy, bool),
        body: &Block,
        label: Option<&String>,
        span: Span,
        source: (&Span, Option<&Spanned<Expr>>),
        borrowed: bool,
    ) -> HirExprKind {
        let (var_name, pattern_span, element_ty, checked_binding) = element;
        let (iterable_span, place_source) = source;
        let element_ty = element_ty.clone();
        let sequence_ty = sequence.ty.clone();
        let length_family = if sequence_ty == ResolvedTy::String {
            hew_types::RuntimeCallFamily::StringLen
        } else if sequence_ty == ResolvedTy::Bytes {
            hew_types::RuntimeCallFamily::BytesLen
        } else if matches!(sequence_ty, ResolvedTy::Array(_, _)) {
            hew_types::RuntimeCallFamily::Array(hew_types::runtime_call::ArrayValueOp::Len)
        } else {
            hew_types::RuntimeCallFamily::Vector(hew_types::VecValueOp::Len)
        };

        self.push_scope();
        let block_scope = self.ids.scope();

        let mut statements = Vec::new();
        let mut sequence_ref: Option<(String, BindingId)> = None;
        let length_receiver = if let Some(place) = place_source {
            self.lower_expr(place, IntentKind::Read)
        } else {
            let sequence_name = format!("__hew_for_seq_{}", self.ids.binding().0);
            let sequence_binding = self.bind(
                sequence_name.clone(),
                sequence_ty.clone(),
                false,
                iterable_span.clone(),
            );
            let sequence_id = sequence_binding.id;
            statements.push(HirStmt {
                node: self.ids.node(),
                kind: HirStmtKind::Let(sequence_binding, Some(sequence)),
                span: iterable_span.clone(),
            });
            sequence_ref = Some((sequence_name.clone(), sequence_id));
            self.make_binding_ref(
                sequence_name,
                sequence_id,
                sequence_ty.clone(),
                IntentKind::Read,
                iterable_span.clone(),
            )
        };
        let length_kind = self.collection_call_kind(
            length_family,
            vec![length_receiver],
            &ResolvedTy::I64,
            iterable_span,
        );
        let length = self.make_expr(
            length_kind,
            ResolvedTy::I64,
            IntentKind::Read,
            iterable_span.clone(),
        );

        let index_name = format!("__hew_for_index_{}", self.ids.binding().0);
        let index_binding = self.bind(
            index_name.clone(),
            ResolvedTy::I64,
            false,
            iterable_span.clone(),
        );
        let index_id = index_binding.id;
        let start = self.make_i64_literal(0, iterable_span.clone());
        let step = self.make_i64_literal(1, iterable_span.clone());

        self.push_scope();
        let element_binding = if checked_binding {
            self.bind_checked(
                var_name.to_string(),
                element_ty.clone(),
                false,
                pattern_span.clone(),
            )
        } else {
            self.bind(
                var_name.to_string(),
                element_ty.clone(),
                false,
                pattern_span.clone(),
            )
        };
        let container = match (&sequence_ref, place_source) {
            (Some((name, id)), _) => self.make_binding_ref(
                name.clone(),
                *id,
                sequence_ty,
                IntentKind::Read,
                iterable_span.clone(),
            ),
            (None, Some(place)) => self.lower_expr(place, IntentKind::Read),
            (None, None) => unreachable!("a sequence walk has a temp or a place source"),
        };
        let index = self.make_binding_ref(
            index_name,
            index_id,
            ResolvedTy::I64,
            IntentKind::Read,
            iterable_span.clone(),
        );
        let read = if borrowed {
            HirExprKind::BorrowedIndex {
                container: Box::new(container),
                index: Box::new(index),
            }
        } else {
            HirExprKind::Index {
                container: Box::new(container),
                index: Box::new(index),
            }
        };
        let element = self.make_expr(read, element_ty, IntentKind::Read, pattern_span.clone());
        let element_stmt = HirStmt {
            node: self.ids.node(),
            kind: HirStmtKind::Let(element_binding, Some(element)),
            span: pattern_span.clone(),
        };
        let mut loop_body = self.lower_block(body, &ResolvedTy::Unit);
        loop_body.statements.insert(0, element_stmt);
        self.pop_scope();

        let loop_expr = self.make_expr(
            HirExprKind::ForRange {
                label: label.cloned(),
                binding: index_binding,
                start: Box::new(start),
                end: Box::new(length),
                inclusive: false,
                step: Box::new(step),
                descending: false,
                body: loop_body,
            },
            ResolvedTy::Unit,
            IntentKind::Read,
            span.clone(),
        );
        let loop_stmt = HirStmt {
            node: self.ids.node(),
            kind: HirStmtKind::Expr(loop_expr),
            span: span.clone(),
        };
        self.pop_scope();

        statements.push(loop_stmt);
        HirExprKind::Block(HirBlock {
            node: self.ids.node(),
            scope: block_scope,
            statements,
            tail: None,
            ty: ResolvedTy::Unit,
            span,
        })
    }

    #[expect(
        clippy::too_many_lines,
        reason = "the desugar preserves the specialized collection, stream and checked trait paths"
    )]
    pub(super) fn lower_for_iter_desugar(
        &mut self,
        pattern: &Spanned<Pattern>,
        iterable: &Spanned<Expr>,
        body: &Block,
        label: Option<&String>,
        span: Span,
    ) -> HirExprKind {
        let (var_name, destructure_pattern) = if let Pattern::Identifier(var_name) = &pattern.0 {
            (var_name.to_string(), None)
        } else {
            (
                format!("__forelem_{}", self.ids.binding().0),
                Some(pattern.clone()),
            )
        };
        // Retain the checked iterable and source intent for iterator desugaring.
        let mut lowered_iterable = self.lower_expr(iterable, IntentKind::Read);

        // `for c in s` / `for b in raw` are index walks, not cursors: the
        // element is a scalar copy, so there is no iterator object to own and
        // no clone recipe to prove. The sequence is bound once so a
        // side-effectful source runs once and the length is read once.
        if matches!(lowered_iterable.ty, ResolvedTy::String | ResolvedTy::Bytes) {
            let element_ty = if lowered_iterable.ty == ResolvedTy::String {
                ResolvedTy::Char
            } else {
                ResolvedTy::U8
            };
            return self.lower_for_sequence_index_desugar(
                lowered_iterable,
                (
                    &var_name,
                    &pattern.1,
                    &element_ty,
                    destructure_pattern.is_none(),
                ),
                body,
                label,
                span,
                (&iterable.1, None),
                false,
            );
        }

        if let ResolvedTy::Array(element_ty, _) = lowered_iterable.ty.clone() {
            let borrowed = self
                .borrowed_element_for_loops
                .contains(&SpanKey::in_module(&iterable.1, self.current_module_idx));
            let source =
                (borrowed && Self::for_in_iterable_is_place(&iterable.0)).then_some(iterable);
            return self.lower_for_sequence_index_desugar(
                lowered_iterable,
                (
                    &var_name,
                    &pattern.1,
                    &element_ty,
                    destructure_pattern.is_none(),
                ),
                body,
                label,
                span,
                (&iterable.1, source),
                borrowed,
            );
        }

        // D432: the checker admitted this loop in borrow mode, so each element
        // is a loan of the slot the vector still owns rather than a copy.
        if self
            .borrowed_element_for_loops
            .contains(&SpanKey::in_module(&iterable.1, self.current_module_idx))
        {
            if let ResolvedTy::Named {
                args,
                head: hew_types::TypeHead::Builtin(BuiltinType::Vec),
                ..
            } = lowered_iterable.ty.clone()
            {
                let element_ty = args[0].clone();
                // A place source is re-read per use: binding it to a temp would
                // TRANSFER the vector (its element has no clone), leaving the
                // source uninitialized for the rest of the body.
                let source = Self::for_in_iterable_is_place(&iterable.0).then_some(iterable);
                return self.lower_for_sequence_index_desugar(
                    lowered_iterable,
                    (
                        &var_name,
                        &pattern.1,
                        &element_ty,
                        destructure_pattern.is_none(),
                    ),
                    body,
                    label,
                    span,
                    (&iterable.1, source),
                    true,
                );
            }
        }

        let (iter_init, iter_ty, elem_ty, next_call) = match lowered_iterable.ty.clone() {
            ResolvedTy::Named {
                args,
                head: hew_types::TypeHead::Builtin(BuiltinType::Stream),
                ..
            } if !args.is_empty() => {
                let elem_ty = args[0].clone();
                // Stream is an affine resource: `for x in stream` drains it;
                // the source binding is consumed.
                // The layout-witness recv (`hew_stream_next_layout`) carries
                // every describable element type; MIR's `lower_direct_call`
                // suspendable-caller flip turns it into
                // `Terminator::SuspendingStreamNext` in actor/task execution
                // contexts, non-context callers keep the blocking call via
                // the codegen `Terminator::Call` intercept.
                lowered_iterable.intent = IntentKind::Consume;
                let iter_ty = lowered_iterable.ty.clone();
                (
                    lowered_iterable,
                    iter_ty,
                    elem_ty,
                    ForIterNextCall::StreamRecv,
                )
            }
            // `for x in <generator>`: the generator value IS the iterator. The
            // loop binds it to `__hew_for_iter_*` (consuming it) and drives one
            // `.next()` per iteration; the binding's scope-exit drop frees it.
            ResolvedTy::Named {
                ref args,
                head: hew_types::TypeHead::Builtin(BuiltinType::Generator),
                ..
            } if !args.is_empty() => {
                let elem_ty = args[0].clone();
                let gen_ty = lowered_iterable.ty.clone();
                // Generator is a linear resource: iterating consumes it.
                lowered_iterable.intent = IntentKind::Consume;
                (
                    lowered_iterable,
                    gen_ty,
                    elem_ty,
                    ForIterNextCall::Generator,
                )
            }
            other => {
                let plan = self.for_loop_plans.get(&self.mk_key(&iterable.1)).cloned();
                let shape = plan.and_then(|plan| {
                    let cursor_ty =
                        self.checked_for_type(&plan.cursor_type, "cursor", &iterable.1)?;
                    let elem_ty = self.checked_for_type(&plan.item_type, "item", &iterable.1)?;
                    let init = if let Some(call) = plan.into_iter {
                        self.lower_checked_for_call(&call, lowered_iterable.clone(), &iterable.1)?
                    } else {
                        lowered_iterable.clone()
                    };
                    Some((
                        init,
                        cursor_ty,
                        elem_ty,
                        ForIterNextCall::Checked(Box::new(plan.next)),
                    ))
                });
                if let Some(shape) = shape {
                    shape
                } else {
                    self.unsupported(
                        iterable.1.clone(),
                        format!("for-in iterable `{other}` has no checked iterator call plan"),
                        "iterator-runtime-dispatch",
                    );
                    self.push_scope();
                    let _ = self.lower_block(body, &ResolvedTy::Unit);
                    self.pop_scope();
                    return HirExprKind::Unsupported(
                        "for-in over unsupported non-Range iterable type".into(),
                    );
                }
            }
        };

        self.push_scope();
        let block_scope = self.ids.scope();
        let iter_name = format!("__hew_for_iter_{}", self.ids.binding().0);
        let iter_binding = self.bind(iter_name.clone(), iter_ty.clone(), true, iterable.1.clone());
        let iter_stmt = HirStmt {
            node: self.ids.node(),
            kind: HirStmtKind::Let(iter_binding.clone(), Some(iter_init)),
            span: iterable.1.clone(),
        };

        let next_expr = match next_call {
            ForIterNextCall::Checked(call) => {
                let receiver = self.make_binding_ref(
                    iter_name.clone(),
                    iter_binding.id,
                    iter_ty.clone(),
                    IntentKind::Consume,
                    iterable.1.clone(),
                );
                self.lower_checked_for_call(&call, receiver, &iterable.1)
                    .unwrap_or_else(|| {
                        self.make_expr(
                            HirExprKind::Unsupported(
                                "checked iterator call is not representable".into(),
                            ),
                            Self::resolved_option_ty(elem_ty.clone()),
                            IntentKind::Read,
                            iterable.1.clone(),
                        )
                    })
            }
            ForIterNextCall::StreamRecv => {
                // Borrow the stream binding (Read) and emit the layout-witness
                // recv runtime call directly. MIR's `lower_direct_call` flips
                // it to `Terminator::SuspendingStreamNext` in actor/task
                // execution contexts; non-context callers keep the blocking
                // call routed through the codegen `Terminator::Call` intercept
                // on `hew_stream_next_layout`.
                let stream = self.make_binding_ref(
                    iter_binding.name.clone(),
                    iter_binding.id,
                    iter_binding.ty.clone(),
                    IntentKind::Read,
                    iterable.1.clone(),
                );
                let option_ty = Self::resolved_option_ty(elem_ty.clone());

                self.make_direct_method_call(
                    "hew_stream_next_layout".to_string(),
                    stream,
                    &option_ty,
                    iterable.1.clone(),
                )
            }
            ForIterNextCall::Generator => {
                // Borrow the generator binding (Read) and emit the dedicated
                // `GeneratorNext` consumption node — identical to a source-level
                // `g.next()`. The binding stays the sole owner so its scope-exit
                // drop frees the context once on loop exit.
                let receiver = self.make_binding_ref(
                    iter_binding.name.clone(),
                    iter_binding.id,
                    iter_binding.ty.clone(),
                    IntentKind::Read,
                    iterable.1.clone(),
                );
                let option_ty = Self::resolved_option_ty(elem_ty.clone());

                self.make_expr(
                    HirExprKind::GeneratorNext {
                        receiver: Box::new(receiver),
                        yield_ty: elem_ty.clone(),
                    },
                    option_ty,
                    IntentKind::Read,
                    iterable.1.clone(),
                )
            }
        };

        self.push_scope();
        let loop_binding = if destructure_pattern.is_none() {
            self.bind_checked(var_name.clone(), elem_ty.clone(), false, pattern.1.clone())
        } else {
            self.bind(var_name.clone(), elem_ty.clone(), false, pattern.1.clone())
        };
        let some_binding = HirMatchArmBinding {
            span: loop_binding.span.clone(),
            binding: loop_binding.id,
            field_idx: 0,
            name: var_name.clone(),
            ty: elem_ty.clone(),
        };
        let mut body_prelude = Vec::new();
        if let Some(destructure_pattern) = &destructure_pattern {
            let elem_ref = self.make_binding_ref(
                var_name.clone(),
                loop_binding.id,
                elem_ty.clone(),
                IntentKind::Read,
                pattern.1.clone(),
            );
            self.lower_pattern_value_into_stmts(
                destructure_pattern,
                elem_ref,
                elem_ty.clone(),
                &mut body_prelude,
                pattern.1.clone(),
            );
        }
        let mut body_block = self.lower_block(body, &ResolvedTy::Unit);
        if !body_prelude.is_empty() {
            body_prelude.extend(body_block.statements);
            body_block.statements = body_prelude;
        }
        self.pop_scope();
        let body_ty = body_block.ty.clone();
        let some_body = self.make_expr(
            HirExprKind::Block(body_block),
            body_ty,
            IntentKind::Read,
            span.clone(),
        );
        let none_body = self.make_expr(
            HirExprKind::Break {
                label: None,
                value: None,
            },
            ResolvedTy::Unit,
            IntentKind::Read,
            span.clone(),
        );
        let some_arm_scope = self.ids.scope();
        let match_expr = self.make_expr(
            HirExprKind::Match {
                scrutinee: Box::new(next_expr),
                arms: vec![
                    HirMatchArm {
                        scope: Some(some_arm_scope),
                        predicate: HirMatchArmPredicate::EnumVariant {
                            variant_match: hew_types::VariantMatch {
                                type_name: "Option".to_string(),
                                variant_name: "Some".to_string(),
                            },
                            variant_idx: 0,
                        },
                        bindings: vec![some_binding],
                        payload_predicates: Vec::new(),
                        payload_variant_predicates: Vec::new(),
                        guard: None,
                        body: some_body,
                        span: span.clone(),
                    },
                    HirMatchArm {
                        scope: None,
                        predicate: HirMatchArmPredicate::EnumVariant {
                            variant_match: hew_types::VariantMatch {
                                type_name: "Option".to_string(),
                                variant_name: "None".to_string(),
                            },
                            variant_idx: 1,
                        },
                        bindings: Vec::new(),
                        payload_predicates: Vec::new(),
                        payload_variant_predicates: Vec::new(),
                        guard: None,
                        body: none_body,
                        span: iterable.1.clone(),
                    },
                ],
            },
            ResolvedTy::Unit,
            IntentKind::Read,
            span.clone(),
        );
        let match_stmt = HirStmt {
            node: self.ids.node(),
            kind: HirStmtKind::Expr(match_expr),
            span: span.clone(),
        };
        let loop_body = self.make_unit_block(
            Vec::from([match_stmt]),
            None,
            ResolvedTy::Unit,
            span.clone(),
        );
        let condition = self.make_expr(
            HirExprKind::Literal(HirLiteral::Bool(true)),
            ResolvedTy::Bool,
            IntentKind::Read,
            span.clone(),
        );
        let loop_expr = self.make_expr(
            HirExprKind::While {
                label: label.cloned(),
                condition: Box::new(condition),
                body: loop_body,
            },
            ResolvedTy::Unit,
            IntentKind::Read,
            span.clone(),
        );
        let loop_stmt = HirStmt {
            node: self.ids.node(),
            kind: HirStmtKind::Expr(loop_expr),
            span: span.clone(),
        };
        self.pop_scope();

        HirExprKind::Block(HirBlock {
            node: self.ids.node(),
            scope: block_scope,
            statements: vec![iter_stmt, loop_stmt],
            tail: None,
            ty: ResolvedTy::Unit,
            span,
        })
    }
}
