//! Tuple, array and map literal lowering and vec higher-order calls.

use super::*;

impl LowerCtx {
    pub(super) fn lower_tuple_literal(
        &mut self,
        elems: &[Spanned<Expr>],
        span: &Span,
    ) -> (HirExprKind, ResolvedTy) {
        // Checker-authoritative: the type checker has already validated the
        // tuple arity and element types and stored the result type in
        // `expr_types`. HIR does not re-derive the tuple type from the
        // element list.
        let Some(result_ty) = self.checker_expr_ty(span, "tuple literal") else {
            return (
                HirExprKind::Unsupported("tuple literal missing checker result type".into()),
                ResolvedTy::Unit,
            );
        };

        // Lower each element expression. The checker has already validated
        // each element type matches the corresponding tuple slot.
        //
        // Pipe halves (`Sink<T>` / `Stream<T>`) are single-owner on
        // the ownership axis even though they are `BitCopy` on the
        // representation axis. A handle placed into a tuple is MOVED — the
        // tuple takes exclusive ownership. Lower such elements with
        // `IntentKind::Consume` so the MIR dataflow checker transitions the
        // source binding to `Consumed`. Without this, a binding like `rx` stays
        // live after being packed into a tuple, and a subsequent `rx.close()`
        // is not statically refused — producing a double-close at runtime.
        //
        // Non-handle elements keep `Read` (the existing copy/clone semantics).
        let hir_elements: Vec<HirExpr> = elems
            .iter()
            .map(|elem| {
                let intent = if self.checked_span_is_pipe_handle(&elem.1) {
                    IntentKind::Consume
                } else {
                    IntentKind::Read
                };
                self.lower_expr(elem, intent)
            })
            .collect();

        (
            HirExprKind::TupleLiteral {
                elements: hir_elements,
            },
            result_ty,
        )
    }

    pub(super) fn array_literal_ty(&mut self, span: &Span) -> Option<(ResolvedTy, ResolvedTy)> {
        let key = self.mk_key(span);
        let Some(ty) = self.expr_types.get(&key).cloned() else {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: "array literal".to_string(),
                    reason: "missing expr_types entry".to_string(),
                },
                span.clone(),
                "array literal lowering requires its exact checker result type",
            ));
            return None;
        };
        let result_ty = match ResolvedTy::from_ty(&ty) {
            Ok(resolved) => self.qualify_current_module_record_ty(resolved),
            Err(err) => {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::CheckerBoundaryViolation {
                        name: "array literal".to_string(),
                        reason: err.to_string(),
                    },
                    span.clone(),
                    "checker-authoritative array literal type failed boundary conversion",
                ));
                return None;
            }
        };
        match result_ty {
            ResolvedTy::Named {
                args,
                builtin: Some(BuiltinType::Vec),
                ..
            } if args.len() == 1 => Some((Self::resolved_vec_ty(args[0].clone()), args[0].clone())),
            ResolvedTy::Array(elem_ty, len) => {
                let elem_ty = *elem_ty;
                Some((ResolvedTy::Array(Box::new(elem_ty.clone()), len), elem_ty))
            }
            other => {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::CheckerBoundaryViolation {
                        name: "array literal".to_string(),
                        reason: format!("checker produced non-array type `{other}`"),
                    },
                    span.clone(),
                    "array literal lowering requires its exact checker result type",
                ));
                None
            }
        }
    }

    pub(super) fn checker_expr_ty_if_present(&mut self, span: &Span) -> Option<ResolvedTy> {
        let key = self.mk_key(span);
        self.expr_types
            .get(&key)
            .cloned()
            .and_then(|ty| ResolvedTy::from_ty(&ty).ok())
            .map(|ty| self.qualify_current_module_record_ty(ty))
    }

    pub(super) fn is_hashmap_ty(ty: &ResolvedTy) -> bool {
        matches!(
            hew_types::runtime_call::collection_type_arguments(ty),
            Some((BuiltinType::HashMap, _))
        )
    }

    pub(super) fn map_literal_hashmap_ty(
        &mut self,
        span: &Span,
    ) -> Option<(ResolvedTy, ResolvedTy, ResolvedTy)> {
        let key = self.mk_key(span);
        let Some(ty) = self.expr_types.get(&key).cloned() else {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: "map literal".to_string(),
                    reason: "missing expr_types entry".to_string(),
                },
                span.clone(),
                "map literal lowering requires the checker HashMap<K, V> type",
            ));
            return None;
        };
        let result_ty = match ResolvedTy::from_ty(&ty) {
            Ok(resolved) => self.qualify_current_module_record_ty(resolved),
            Err(err) => {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::CheckerBoundaryViolation {
                        name: "map literal".to_string(),
                        reason: err.to_string(),
                    },
                    span.clone(),
                    "checker-authoritative map literal type failed boundary conversion",
                ));
                return None;
            }
        };
        if let Some((BuiltinType::HashMap, args)) =
            hew_types::runtime_call::collection_type_arguments(&result_ty)
        {
            Some((result_ty.clone(), args[0].clone(), args[1].clone()))
        } else {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: "map literal".to_string(),
                    reason: format!("checker produced non-HashMap type `{result_ty}`"),
                },
                span.clone(),
                "map literal lowering requires the checker HashMap<K, V> type",
            ));
            None
        }
    }

    pub(super) fn make_vec_new_expr(&mut self, vec_ty: ResolvedTy, span: Span) -> HirExpr {
        let callee_ty = ResolvedTy::Function {
            capabilities: hew_types::CallableCapabilities::FUNCTION_ITEM,
            params: Vec::new(),
            ret: Box::new(vec_ty.clone()),
        };
        let resolved_ref = self
            .fn_registry
            .get("Vec::new")
            .map_or(ResolvedRef::Unresolved, |entry| ResolvedRef::Item(entry.id));
        let callee = self.make_expr(
            HirExprKind::BindingRef {
                name: "Vec::new".to_string(),
                resolved: resolved_ref,
            },
            callee_ty,
            IntentKind::Read,
            span.clone(),
        );
        self.make_expr(
            HirExprKind::Call {
                target: CallTarget::Runtime(hew_types::RuntimeCallFamily::Vector(
                    hew_types::VecValueOp::New,
                )),
                callee: Box::new(callee),
                args: Vec::new(),
                evaluation_order: Vec::new(),
            },
            vec_ty,
            IntentKind::Read,
            span,
        )
    }

    pub(super) fn make_hashmap_new_expr(&mut self, hashmap_ty: ResolvedTy, span: Span) -> HirExpr {
        let kind = self.collection_call_kind(
            hew_types::RuntimeCallFamily::Map(hew_types::runtime_call::MapValueOp::New),
            Vec::new(),
            &hashmap_ty,
            &span,
        );
        self.make_expr(kind, hashmap_ty, IntentKind::Read, span)
    }

    pub(super) fn make_hashmap_insert_expr(
        &mut self,
        map_ref: HirExpr,
        key: HirExpr,
        value: HirExpr,
        span: Span,
    ) -> HirExpr {
        let kind = self.collection_call_kind(
            hew_types::RuntimeCallFamily::Map(hew_types::runtime_call::MapValueOp::Insert),
            vec![map_ref, key, value],
            &ResolvedTy::Unit,
            &span,
        );
        self.make_expr(kind, ResolvedTy::Unit, IntentKind::Read, span)
    }

    pub(super) fn make_vec_push_expr(
        &mut self,
        vec_ref: HirExpr,
        elem: HirExpr,
        span: Span,
    ) -> HirExpr {
        let kind = self.collection_call_kind(
            hew_types::RuntimeCallFamily::Vector(hew_types::VecValueOp::Push),
            vec![vec_ref, elem],
            &ResolvedTy::Unit,
            &span,
        );
        self.make_expr(kind, ResolvedTy::Unit, IntentKind::Read, span)
    }

    /// HIR retains semantic method identity and exact types, never an element ABI.
    pub(super) fn collection_call_kind(
        &mut self,
        family: hew_types::RuntimeCallFamily,
        args: Vec<HirExpr>,
        result_ty: &ResolvedTy,
        span: &Span,
    ) -> HirExprKind {
        let callee = self.make_expr(
            HirExprKind::BindingRef {
                name: format!("{family:?}"),
                resolved: ResolvedRef::Builtin(family),
            },
            ResolvedTy::Function {
                capabilities: hew_types::CallableCapabilities::FUNCTION_ITEM,
                params: args.iter().map(|arg| arg.ty.clone()).collect(),
                ret: Box::new(result_ty.clone()),
            },
            IntentKind::Read,
            span.clone(),
        );
        HirExprKind::Call {
            target: CallTarget::Runtime(family),
            callee: Box::new(callee),
            args,
            evaluation_order: Vec::new(),
        }
    }

    pub(super) fn normalize_collection_call(
        &mut self,
        kind: HirExprKind,
        ty: &ResolvedTy,
        span: &Span,
    ) -> HirExprKind {
        use hew_types::runtime_call::{MapValueOp, SetValueOp};
        use hew_types::{RuntimeCallFamily as Family, VecValueOp};
        match kind {
            HirExprKind::Call {
                target:
                    CallTarget::Runtime(
                        family @ (Family::VecNew | Family::HashMapNew | Family::HashSetNew),
                    ),
                args,
                ..
            } => {
                let family = match family {
                    Family::VecNew => Family::Vector(VecValueOp::New),
                    Family::HashMapNew => Family::Map(MapValueOp::New),
                    Family::HashSetNew => Family::Set(SetValueOp::New),
                    _ => unreachable!("matched a canonical collection constructor"),
                };
                self.collection_call_kind(family, args, ty, span)
            }
            HirExprKind::ResolvedImplCall {
                target:
                    CallTarget::RuntimeCollection(
                        method @ (hew_types::MethodTargetFamily::Vec(hew_types::VecMethod::IsEmpty)
                        | hew_types::MethodTargetFamily::HashMap(
                            hew_types::HashMapMethod::IsEmpty,
                        )
                        | hew_types::MethodTargetFamily::HashSet(
                            hew_types::HashSetMethod::IsEmpty,
                        )),
                    ),
                receiver,
                args,
                ..
            } if args.is_empty() => {
                let family = match method {
                    hew_types::MethodTargetFamily::Vec(_) => Family::Vector(VecValueOp::Len),
                    hew_types::MethodTargetFamily::HashSet(_) => Family::Set(SetValueOp::Len),
                    hew_types::MethodTargetFamily::HashMap(_) => Family::Map(MapValueOp::Len),
                };
                let length =
                    self.collection_call_kind(family, vec![*receiver], &ResolvedTy::I64, span);
                let length =
                    self.make_expr(length, ResolvedTy::I64, IntentKind::Read, span.clone());
                let zero = self.make_i64_literal(0, span.clone());
                HirExprKind::Binary {
                    op: BinaryOp::Equal,
                    left: Box::new(length),
                    right: Box::new(zero),
                }
            }
            HirExprKind::ResolvedImplCall {
                target: CallTarget::RuntimeCollection(method),
                receiver,
                args,
                ..
            } if method.runtime_family().is_some() => {
                let mut family = method
                    .runtime_family()
                    .expect("matched semantic collection method");
                // The checker admitted this read in borrow mode, so `Some`
                // carries a loan of the slot the collection still owns.
                if self
                    .borrowed_element_option_reads
                    .contains(&SpanKey::in_module(span, self.current_module_idx))
                {
                    if family == Family::Vector(VecValueOp::Get) {
                        family = Family::Vector(VecValueOp::GetBorrow);
                    } else if family == Family::Map(MapValueOp::Get) {
                        family = Family::Map(MapValueOp::GetBorrow);
                    }
                }
                let mut operands = vec![*receiver];
                operands.extend(args);
                self.collection_call_kind(family, operands, ty, span)
            }
            other => other,
        }
    }

    pub(super) fn lower_array_literal(
        &mut self,
        elements: &[ArrayElement],
        span: &Span,
    ) -> (HirExprKind, ResolvedTy) {
        let Some((vec_ty, elem_ty)) = self.array_literal_ty(span) else {
            return (
                HirExprKind::Unsupported("array literal missing checker element type".into()),
                ResolvedTy::Unit,
            );
        };

        if matches!(vec_ty, ResolvedTy::Array(_, _)) {
            // The checker refuses a spread against a fixed-size array type, so
            // every element here contributes exactly one slot.
            let elements = elements
                .iter()
                .map(|element| self.lower_expr(element.expr(), IntentKind::Read))
                .collect();
            return (HirExprKind::ArrayLiteral { elements }, vec_ty);
        }

        let block_scope = self.ids.scope();
        self.push_scope();
        let temp_name = format!("__hew_array_{}", self.ids.binding().0);
        let temp_binding = self.bind(temp_name.clone(), vec_ty.clone(), true, span.clone());
        let temp_binding_id = temp_binding.id;
        let init_stmt = HirStmt {
            node: self.ids.node(),
            kind: HirStmtKind::Let(
                temp_binding,
                Some(self.make_vec_new_expr(vec_ty.clone(), span.clone())),
            ),
            span: span.clone(),
        };
        let mut statements = Vec::with_capacity(elements.len() + 1);
        statements.push(init_stmt);
        for element in elements {
            match element {
                ArrayElement::Value(value) => {
                    let lowered = self.lower_expr(value, IntentKind::Read);
                    let vec_ref = self.make_binding_ref(
                        temp_name.clone(),
                        temp_binding_id,
                        vec_ty.clone(),
                        IntentKind::Read,
                        lowered.span.clone(),
                    );
                    let push_expr = self.make_vec_push_expr(vec_ref, lowered, span.clone());
                    statements.push(HirStmt {
                        node: self.ids.node(),
                        kind: HirStmtKind::Expr(push_expr),
                        span: span.clone(),
                    });
                }
                ArrayElement::Spread(operand) => {
                    let spread_stmt = self.lower_array_spread(
                        operand,
                        (&temp_name, temp_binding_id, &vec_ty),
                        &elem_ty,
                        &mut statements,
                    );
                    statements.push(spread_stmt);
                }
            }
        }
        let tail = self.make_binding_ref(
            temp_name,
            temp_binding_id,
            vec_ty.clone(),
            IntentKind::Read,
            span.clone(),
        );
        self.pop_scope();

        (
            HirExprKind::Block(HirBlock {
                node: self.ids.node(),
                scope: block_scope,
                statements,
                tail: Some(Box::new(tail)),
                ty: vec_ty.clone(),
                span: span.clone(),
            }),
            vec_ty,
        )
    }

    /// `..operand` inside a bracket literal: walk the operand by index and push
    /// each element onto the literal's vector, in order.
    ///
    /// Each `Index` read is the same element read `for x in v` performs, so an
    /// owned element is copied into the new vector and the operand keeps its
    /// own. The operand is therefore a retain, not a transfer — the value stays
    /// usable after the literal, exactly as passing it to a call would leave it.
    ///
    /// A place operand is re-read on each iteration rather than bound to a
    /// temp: a `Read`-load of an owned place would give the temp a second
    /// owner of the same heap. A value-producing operand keeps an eval-once
    /// temp so a side-effecting source runs once. The place question is the
    /// one [`Self::for_in_iterable_is_place`] already answers for the same
    /// reason on the `for`-in path.
    #[expect(
        clippy::too_many_lines,
        reason = "the spread desugar is a single ownership-sensitive expansion; splitting it would obscure the temp binding's lifetime"
    )]
    pub(super) fn lower_array_spread(
        &mut self,
        operand: &Spanned<Expr>,
        target: (&str, BindingId, &ResolvedTy),
        elem_ty: &ResolvedTy,
        statements: &mut Vec<HirStmt>,
    ) -> HirStmt {
        let (vec_name, vec_id, vec_ty) = target;
        let operand_span = operand.1.clone();
        let source_is_place = Self::for_in_iterable_is_place(&operand.0);

        let source_ty = self
            .expr_types
            .get(&self.mk_key(&operand_span))
            .and_then(|ty| ResolvedTy::from_ty(ty).ok())
            .unwrap_or_else(|| vec_ty.clone());

        let source_ref: Option<(String, BindingId)> = if source_is_place {
            None
        } else {
            let lowered = self.lower_expr(operand, IntentKind::Read);
            let source_name = format!("__hew_spread_{}", self.ids.binding().0);
            let source_binding = self.bind(
                source_name.clone(),
                source_ty.clone(),
                false,
                operand_span.clone(),
            );
            let source_id = source_binding.id;
            statements.push(HirStmt {
                node: self.ids.node(),
                kind: HirStmtKind::Let(source_binding, Some(lowered)),
                span: operand_span.clone(),
            });
            Some((source_name, source_id))
        };

        let source_expr = |this: &mut Self| match &source_ref {
            Some((name, id)) => this.make_binding_ref(
                name.clone(),
                *id,
                source_ty.clone(),
                IntentKind::Read,
                operand_span.clone(),
            ),
            None => this.lower_expr(operand, IntentKind::Read),
        };

        let length_receiver = source_expr(self);
        let length_kind = self.collection_call_kind(
            hew_types::RuntimeCallFamily::Vector(hew_types::VecValueOp::Len),
            vec![length_receiver],
            &ResolvedTy::I64,
            &operand_span,
        );
        let length = self.make_expr(
            length_kind,
            ResolvedTy::I64,
            IntentKind::Read,
            operand_span.clone(),
        );

        let index_name = format!("__hew_spread_i_{}", self.ids.binding().0);
        let index_binding = self.bind(
            index_name.clone(),
            ResolvedTy::I64,
            false,
            operand_span.clone(),
        );
        let index_id = index_binding.id;
        let start = self.make_i64_literal(0, operand_span.clone());
        let step = self.make_i64_literal(1, operand_span.clone());

        let container = source_expr(self);
        let index = self.make_binding_ref(
            index_name,
            index_id,
            ResolvedTy::I64,
            IntentKind::Read,
            operand_span.clone(),
        );
        let element = self.make_expr(
            HirExprKind::Index {
                container: Box::new(container),
                index: Box::new(index),
            },
            elem_ty.clone(),
            IntentKind::Read,
            operand_span.clone(),
        );
        let vec_ref = self.make_binding_ref(
            vec_name.to_string(),
            vec_id,
            vec_ty.clone(),
            IntentKind::Read,
            operand_span.clone(),
        );
        let push_expr = self.make_vec_push_expr(vec_ref, element, operand_span.clone());
        let push_stmt = HirStmt {
            node: self.ids.node(),
            kind: HirStmtKind::Expr(push_expr),
            span: operand_span.clone(),
        };
        let body = self.make_unit_block(
            vec![push_stmt],
            None,
            ResolvedTy::Unit,
            operand_span.clone(),
        );
        let for_expr = self.make_expr(
            HirExprKind::ForRange {
                label: None,
                binding: index_binding,
                start: Box::new(start),
                end: Box::new(length),
                inclusive: false,
                step: Box::new(step),
                descending: false,
                body,
            },
            ResolvedTy::Unit,
            IntentKind::Read,
            operand_span.clone(),
        );
        HirStmt {
            node: self.ids.node(),
            kind: HirStmtKind::Expr(for_expr),
            span: operand_span,
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "array-repeat desugaring is a single ownership-sensitive expansion; splitting would obscure temp binding lifetimes"
    )]
    pub(super) fn lower_array_repeat(
        &mut self,
        value: &Spanned<Expr>,
        count: &Spanned<Expr>,
        span: &Span,
    ) -> (HirExprKind, ResolvedTy) {
        let Some((vec_ty, elem_ty)) = self.array_literal_ty(span) else {
            return (
                HirExprKind::Unsupported("array-repeat missing checker element type".into()),
                ResolvedTy::Unit,
            );
        };

        if matches!(vec_ty, ResolvedTy::Array(_, _)) {
            let value = Box::new(self.lower_expr(value, IntentKind::Read));
            return (HirExprKind::ArrayRepeat { value }, vec_ty);
        }

        // Owned (non-BitCopy) elements are cloned per slot by the runtime push
        // path: push_str / push_bytes creates an independent copy, and push_owned
        // calls the element clone thunk. The checker's synthesize_array_repeat
        // already rejected unclonable owned types, so any non-BitCopy element
        // that reaches here is guaranteed Clone-admissible. The source value
        // binding lives until the block exits and is dropped exactly once by
        // scope-exit drop elaboration (N runtime clones + 1 source drop).

        let block_scope = self.ids.scope();
        self.push_scope();
        let mut statements = Vec::new();

        let vec_name = format!("__hew_repeat_{}", self.ids.binding().0);
        let vec_binding = self.bind(vec_name.clone(), vec_ty.clone(), true, span.clone());
        let vec_id = vec_binding.id;
        statements.push(HirStmt {
            node: self.ids.node(),
            kind: HirStmtKind::Let(
                vec_binding,
                Some(self.make_vec_new_expr(vec_ty.clone(), span.clone())),
            ),
            span: span.clone(),
        });

        // Classify the source. A PLACE source (identifier / field / index) must
        // NOT be bound to an intermediate owned temp: a `Read`-load of a place is
        // a shallow struct memcpy for an owned record, so the temp would alias the
        // still-live source's field heap and BOTH would drop it (#2724 —
        // double-free of a Vec field, over-release of a string field). Instead a
        // place source pushes a fresh `Read`-load of the place on each loop
        // iteration (as `lower_array_literal` does per element): the already-sound
        // per-slot `push_owned`/`push_str` clone deep-copies each slot into an
        // independent owner and the source place drops exactly ONCE at its own
        // enclosing scope. No second owner is ever materialised, and the source
        // stays live (no accept-set regression to use-of-moved-value).
        //
        // A VALUE-PRODUCING source (call, constructor, literal, array/map
        // literal, ...) keeps the eval-once temp: the produced value moves in and
        // nothing else aliases it, so it is already the SOLE owner. Evaluating it
        // once is required so a side-effecting source runs exactly once (e.g.
        // `[f(); 0]` must still call `f` once and drop it once).
        let source_is_place = matches!(
            value.0,
            Expr::Identifier(_) | Expr::FieldAccess { .. } | Expr::Index { .. }
        );

        let value_binding_ref: Option<(String, BindingId)> = if source_is_place {
            None
        } else {
            let lowered_value = self.lower_expr(value, IntentKind::Read);
            let value_name = format!("__hew_repeat_value_{}", self.ids.binding().0);
            let value_binding =
                self.bind(value_name.clone(), elem_ty.clone(), false, value.1.clone());
            let value_id = value_binding.id;
            statements.push(HirStmt {
                node: self.ids.node(),
                kind: HirStmtKind::Let(value_binding, Some(lowered_value)),
                span: value.1.clone(),
            });
            Some((value_name, value_id))
        };

        let lowered_count = self.lower_expr(count, IntentKind::Read);
        let count_name = format!("__hew_repeat_count_{}", self.ids.binding().0);
        let count_binding = self.bind(count_name.clone(), ResolvedTy::I64, false, count.1.clone());
        let count_id = count_binding.id;
        statements.push(HirStmt {
            node: self.ids.node(),
            kind: HirStmtKind::Let(count_binding, Some(lowered_count)),
            span: count.1.clone(),
        });

        let i_name = format!("__hew_repeat_i_{}", self.ids.binding().0);
        let i_binding = self.bind(i_name, ResolvedTy::I64, false, span.clone());
        let start = self.make_i64_literal(0, span.clone());
        let end = self.make_binding_ref(
            count_name,
            count_id,
            ResolvedTy::I64,
            IntentKind::Read,
            count.1.clone(),
        );
        let step = self.make_i64_literal(1, span.clone());
        let vec_ref = self.make_binding_ref(
            vec_name.clone(),
            vec_id,
            vec_ty.clone(),
            IntentKind::Read,
            span.clone(),
        );
        // Element pushed each iteration: for a value-producing source, the
        // eval-once temp's binding-ref (the temp is the sole owner, cloned per
        // slot); for a place source, a FRESH `Read`-load of the place, re-read
        // every iteration so each slot is an independent clone and no aliasing
        // temp is ever created.
        let push_elem = match &value_binding_ref {
            Some((value_name, value_id)) => self.make_binding_ref(
                value_name.clone(),
                *value_id,
                elem_ty.clone(),
                IntentKind::Read,
                value.1.clone(),
            ),
            None => self.lower_expr(value, IntentKind::Read),
        };
        let push_expr = self.make_vec_push_expr(vec_ref, push_elem, span.clone());
        let push_stmt = HirStmt {
            node: self.ids.node(),
            kind: HirStmtKind::Expr(push_expr),
            span: span.clone(),
        };
        let body = self.make_unit_block(vec![push_stmt], None, ResolvedTy::Unit, span.clone());
        let for_expr = self.make_expr(
            HirExprKind::ForRange {
                label: None,
                binding: i_binding,
                start: Box::new(start),
                end: Box::new(end),
                inclusive: false,
                step: Box::new(step),
                descending: false,
                body,
            },
            ResolvedTy::Unit,
            IntentKind::Read,
            span.clone(),
        );
        statements.push(HirStmt {
            node: self.ids.node(),
            kind: HirStmtKind::Expr(for_expr),
            span: span.clone(),
        });

        let tail = self.make_binding_ref(
            vec_name,
            vec_id,
            vec_ty.clone(),
            IntentKind::Read,
            span.clone(),
        );
        self.pop_scope();

        (
            HirExprKind::Block(HirBlock {
                node: self.ids.node(),
                scope: block_scope,
                statements,
                tail: Some(Box::new(tail)),
                ty: vec_ty.clone(),
                span: span.clone(),
            }),
            vec_ty,
        )
    }

    pub(super) fn lower_map_literal(
        &mut self,
        entries: &[(Spanned<Expr>, Spanned<Expr>)],
        span: &Span,
    ) -> (HirExprKind, ResolvedTy) {
        let Some((map_ty, _, _)) = self.map_literal_hashmap_ty(span) else {
            return (
                HirExprKind::Unsupported("map literal missing checker HashMap type".into()),
                ResolvedTy::Unit,
            );
        };

        let lowered_entries: Vec<(HirExpr, HirExpr)> = entries
            .iter()
            .map(|(key, value)| {
                (
                    self.lower_expr(key, IntentKind::Read),
                    self.lower_expr(value, IntentKind::Read),
                )
            })
            .collect();
        let block_scope = self.ids.scope();
        self.push_scope();
        let temp_name = format!("__hew_map_{}", self.ids.binding().0);
        let temp_binding = self.bind(temp_name.clone(), map_ty.clone(), true, span.clone());
        let temp_binding_id = temp_binding.id;
        let init_stmt = HirStmt {
            node: self.ids.node(),
            kind: HirStmtKind::Let(
                temp_binding,
                Some(self.make_hashmap_new_expr(map_ty.clone(), span.clone())),
            ),
            span: span.clone(),
        };
        let mut statements = Vec::with_capacity(lowered_entries.len() + 1);
        statements.push(init_stmt);
        for (key, value) in lowered_entries {
            let map_ref = self.make_binding_ref(
                temp_name.clone(),
                temp_binding_id,
                map_ty.clone(),
                IntentKind::Read,
                key.span.clone(),
            );
            let insert_expr = self.make_hashmap_insert_expr(map_ref, key, value, span.clone());
            statements.push(HirStmt {
                node: self.ids.node(),
                kind: HirStmtKind::Expr(insert_expr),
                span: span.clone(),
            });
        }
        let tail = self.make_binding_ref(
            temp_name,
            temp_binding_id,
            map_ty.clone(),
            IntentKind::Consume,
            span.clone(),
        );
        self.pop_scope();

        (
            HirExprKind::Block(HirBlock {
                node: self.ids.node(),
                scope: block_scope,
                statements,
                tail: Some(Box::new(tail)),
                ty: map_ty.clone(),
                span: span.clone(),
            }),
            map_ty,
        )
    }

    /// Expand a `Vec` pipeline call (`map` / `filter` / `reduce`, spec
    /// §3.8.6) into a counted loop over the receiver.
    ///
    /// Shape (map):
    /// ```text
    /// {
    ///     let __hew_pipe_src = <receiver>;
    ///     let __hew_pipe_fn = <closure arg>;
    ///     let __hew_pipe_out = Vec::new();
    ///     for __hew_pipe_i in 0..__hew_pipe_src.len() {
    ///         __hew_pipe_out.push(__hew_pipe_fn(__hew_pipe_src[__hew_pipe_i]));
    ///     }
    ///     __hew_pipe_out
    /// }
    /// ```
    /// `filter` wraps the push in an `if`; `reduce` folds into a mutable
    /// accumulator seeded from the second argument and yields it.
    ///
    /// Receiver and closure are bound exactly once (chained receivers
    /// evaluate once; the closure value is reused across iterations as a
    /// borrow — calls never consume the pair). Element access reuses the
    /// established `Index` lowering and the push reuses the array-literal
    /// push substrate, so every element-type ABI rule and drop discipline
    /// is inherited rather than re-derived. The checker rejects
    /// function-valued element receivers before recording this rewrite
    /// (single-owner environment contract).
    #[expect(
        clippy::too_many_lines,
        reason = "the three pipeline expansions share the src/fn/loop scaffolding; \
                  splitting per-op helpers would triple the binding plumbing"
    )]
    pub(super) fn lower_builtin_vec_higher_order(
        &mut self,
        receiver: &Spanned<Expr>,
        args: &[hew_parser::ast::CallArg],
        op: hew_types::VecHigherOrderOp,
        elem_ty: &ResolvedTy,
        out_ty: &ResolvedTy,
        span: Span,
    ) -> (HirExprKind, ResolvedTy) {
        use hew_types::VecHigherOrderOp as HofOp;
        let (label, expected_args) = match op {
            HofOp::Map => ("Vec.map", 1),
            HofOp::Filter => ("Vec.filter", 1),
            HofOp::Reduce => ("Vec.reduce", 2),
        };
        if args.len() != expected_args {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: label.to_string(),
                    reason: format!(
                        "checker side-table expected {expected_args} argument(s), found {}",
                        args.len()
                    ),
                },
                span.clone(),
                "Vec pipeline lowering arity disagrees with the checker-recorded rewrite",
            ));
            return (
                HirExprKind::Unsupported(format!("{label} has invalid arity")),
                ResolvedTy::Unit,
            );
        }

        let src_vec_ty = Self::resolved_vec_ty(elem_ty.clone());
        // `filter` records `out_ty == elem_ty`, so the collected vec is
        // `Vec<out_ty>` for both collecting ops.
        let result_ty = match op {
            HofOp::Map | HofOp::Filter => Self::resolved_vec_ty(out_ty.clone()),
            HofOp::Reduce => out_ty.clone(),
        };

        let block_scope = self.ids.scope();
        self.push_scope();
        let mut statements: Vec<HirStmt> = Vec::new();

        // Bind the receiver once. A place receiver must remain usable after
        // this eager pipeline, but `Vec` is a single-owner handle with no
        // refcount. Give the synthetic source binding an independent snapshot
        // through the checker-preauthored element-aware clone. A non-place
        // producer has no surviving source and moves directly into the binding.
        let lowered_receiver = if Self::for_in_iterable_is_place(&receiver.0) {
            let clone_span = span.start..span.start;
            let clone_call = (
                Expr::MethodCall {
                    receiver: Box::new((receiver.0.clone(), clone_span.clone())),
                    method: "clone".to_string(),
                    args: Vec::new(),
                },
                clone_span,
            );
            self.lower_expr(&clone_call, IntentKind::Consume)
        } else {
            self.lower_expr(receiver, IntentKind::Consume)
        };
        let src_name = format!("__hew_pipe_src_{}", self.ids.binding().0);
        let src_binding = self.bind(src_name.clone(), src_vec_ty.clone(), false, span.clone());
        let src_id = src_binding.id;
        statements.push(HirStmt {
            node: self.ids.node(),
            kind: HirStmtKind::Let(src_binding, Some(lowered_receiver)),
            span: span.clone(),
        });

        // Bind the closure argument once. The synthetic binding carries the
        // lowered argument's own type (a named-fn pair or a closure type).
        let lowered_fn = self.lower_expr(args[0].expr(), IntentKind::Read);
        let fn_ty = lowered_fn.ty.clone();
        let fn_name = format!("__hew_pipe_fn_{}", self.ids.binding().0);
        let fn_binding = self.bind(fn_name.clone(), fn_ty.clone(), false, span.clone());
        let fn_id = fn_binding.id;
        statements.push(HirStmt {
            node: self.ids.node(),
            kind: HirStmtKind::Let(fn_binding, Some(lowered_fn)),
            span: span.clone(),
        });

        // The collected vec (map/filter) or the folded accumulator (reduce).
        // Every op mutates this binding in the loop body - map and filter push
        // into it, reduce reassigns it - so it is declared mutable and the
        // place root the push resolves is a mutable one.
        let acc_name = format!("__hew_pipe_out_{}", self.ids.binding().0);
        let acc_init = match op {
            HofOp::Map | HofOp::Filter => self.make_vec_new_expr(result_ty.clone(), span.clone()),
            HofOp::Reduce => self.lower_expr(args[1].expr(), IntentKind::Read),
        };
        let acc_binding = self.bind(acc_name.clone(), result_ty.clone(), true, span.clone());
        let acc_id = acc_binding.id;
        statements.push(HirStmt {
            node: self.ids.node(),
            kind: HirStmtKind::Let(acc_binding, Some(acc_init)),
            span: span.clone(),
        });

        // for __hew_pipe_i in 0..src.len() { <per-op body> }
        let i_name = format!("__hew_pipe_i_{}", self.ids.binding().0);
        let i_binding = self.bind(i_name.clone(), ResolvedTy::I64, false, span.clone());
        let i_id = i_binding.id;
        let start = self.make_i64_literal(0, span.clone());
        let src_ref_for_len = self.make_binding_ref(
            src_name.clone(),
            src_id,
            src_vec_ty.clone(),
            IntentKind::Read,
            span.clone(),
        );
        let end = self.make_vec_len_call(src_ref_for_len, elem_ty, span.clone());

        let make_elem_read = |this: &mut Self| {
            let src_ref = this.make_binding_ref(
                src_name.clone(),
                src_id,
                src_vec_ty.clone(),
                IntentKind::Read,
                span.clone(),
            );
            let i_ref = this.make_binding_ref(
                i_name.clone(),
                i_id,
                ResolvedTy::I64,
                IntentKind::Read,
                span.clone(),
            );
            this.make_expr(
                HirExprKind::Index {
                    container: Box::new(src_ref),
                    index: Box::new(i_ref),
                },
                elem_ty.clone(),
                IntentKind::Read,
                span.clone(),
            )
        };

        let body = match op {
            HofOp::Map => {
                let elem_read = make_elem_read(self);
                let fn_ref = self.make_binding_ref(
                    fn_name.clone(),
                    fn_id,
                    fn_ty.clone(),
                    IntentKind::Read,
                    span.clone(),
                );
                let mapped = self.make_expr(
                    HirExprKind::Call {
                        target: CallTarget::IndirectFunctionValue,
                        callee: Box::new(fn_ref),
                        args: vec![elem_read],
                        evaluation_order: Vec::new(),
                    },
                    out_ty.clone(),
                    IntentKind::Read,
                    span.clone(),
                );
                let out_ref = self.make_binding_ref(
                    acc_name.clone(),
                    acc_id,
                    result_ty.clone(),
                    IntentKind::Read,
                    span.clone(),
                );
                let push = self.make_vec_push_expr(out_ref, mapped, span.clone());
                let push_stmt = HirStmt {
                    node: self.ids.node(),
                    kind: HirStmtKind::Expr(push),
                    span: span.clone(),
                };
                self.make_unit_block(vec![push_stmt], None, ResolvedTy::Unit, span.clone())
            }
            HofOp::Filter => {
                let elem_read = make_elem_read(self);
                let fn_ref = self.make_binding_ref(
                    fn_name.clone(),
                    fn_id,
                    fn_ty.clone(),
                    IntentKind::Read,
                    span.clone(),
                );
                let cond = self.make_expr(
                    HirExprKind::Call {
                        target: CallTarget::IndirectFunctionValue,
                        callee: Box::new(fn_ref),
                        args: vec![elem_read],
                        evaluation_order: Vec::new(),
                    },
                    ResolvedTy::Bool,
                    IntentKind::Read,
                    span.clone(),
                );
                let kept_read = make_elem_read(self);
                let out_ref = self.make_binding_ref(
                    acc_name.clone(),
                    acc_id,
                    result_ty.clone(),
                    IntentKind::Read,
                    span.clone(),
                );
                let push = self.make_vec_push_expr(out_ref, kept_read, span.clone());
                let push_stmt = HirStmt {
                    node: self.ids.node(),
                    kind: HirStmtKind::Expr(push),
                    span: span.clone(),
                };
                let then_block =
                    self.make_unit_block(vec![push_stmt], None, ResolvedTy::Unit, span.clone());
                let then_expr = self.make_expr(
                    HirExprKind::Block(then_block),
                    ResolvedTy::Unit,
                    IntentKind::Read,
                    span.clone(),
                );
                let if_expr = self.make_expr(
                    HirExprKind::If {
                        condition: Box::new(cond),
                        then_expr: Box::new(then_expr),
                        else_expr: None,
                    },
                    ResolvedTy::Unit,
                    IntentKind::Read,
                    span.clone(),
                );
                let if_stmt = HirStmt {
                    node: self.ids.node(),
                    kind: HirStmtKind::Expr(if_expr),
                    span: span.clone(),
                };
                self.make_unit_block(vec![if_stmt], None, ResolvedTy::Unit, span.clone())
            }
            HofOp::Reduce => {
                let acc_read = self.make_binding_ref(
                    acc_name.clone(),
                    acc_id,
                    result_ty.clone(),
                    IntentKind::Read,
                    span.clone(),
                );
                let elem_read = make_elem_read(self);
                let fn_ref = self.make_binding_ref(
                    fn_name.clone(),
                    fn_id,
                    fn_ty.clone(),
                    IntentKind::Read,
                    span.clone(),
                );
                let folded = self.make_expr(
                    HirExprKind::Call {
                        target: CallTarget::IndirectFunctionValue,
                        callee: Box::new(fn_ref),
                        args: vec![acc_read, elem_read],
                        evaluation_order: Vec::new(),
                    },
                    result_ty.clone(),
                    IntentKind::Read,
                    span.clone(),
                );
                let acc_target = self.make_binding_ref(
                    acc_name.clone(),
                    acc_id,
                    result_ty.clone(),
                    IntentKind::Modify,
                    span.clone(),
                );
                let assign_stmt = HirStmt {
                    node: self.ids.node(),
                    kind: HirStmtKind::Assign {
                        target: acc_target,
                        value: Box::new(folded),
                        first_store: false,
                    },
                    span: span.clone(),
                };
                self.make_unit_block(vec![assign_stmt], None, ResolvedTy::Unit, span.clone())
            }
        };

        let step = self.make_i64_literal(1, span.clone());
        let for_expr = self.make_expr(
            HirExprKind::ForRange {
                label: None,
                binding: i_binding,
                start: Box::new(start),
                end: Box::new(end),
                inclusive: false,
                step: Box::new(step),
                descending: false,
                body,
            },
            ResolvedTy::Unit,
            IntentKind::Read,
            span.clone(),
        );
        statements.push(HirStmt {
            node: self.ids.node(),
            kind: HirStmtKind::Expr(for_expr),
            span: span.clone(),
        });

        let tail = self.make_binding_ref(
            acc_name,
            acc_id,
            result_ty.clone(),
            IntentKind::Read,
            span.clone(),
        );
        self.pop_scope();
        (
            HirExprKind::Block(HirBlock {
                node: self.ids.node(),
                scope: block_scope,
                statements,
                tail: Some(Box::new(tail)),
                ty: result_ty.clone(),
                span,
            }),
            result_ty,
        )
    }
}
