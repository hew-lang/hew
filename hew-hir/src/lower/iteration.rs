//! Iterator shapes, `for` loop desugaring and range adapters.

use super::*;
use hew_parser::ast::Ident;

/// Walk each monomorphisation's origin fn body, substitute the
/// per-monomorphisation type-arg map into every inner Call site's
/// recorded type arguments, and add any newly-discovered concrete
/// `(origin_fn, Vec<ResolvedTy>)` pairs to the registry. Iterates to a
/// fixed point.
///
/// Emits `MonomorphisationCapExceeded` (at most once) when the cap is
/// hit. Skips inner calls whose substituted args still contain any
/// abstract type-parameter symbol (no monomorphisation is possible
/// without a concrete instantiation).
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
    pub(super) fn resolved_option_elem_ty(ty: &ResolvedTy) -> Option<ResolvedTy> {
        let ResolvedTy::Named {
            args,
            head: hew_types::TypeHead::Builtin(BuiltinType::Option),
            ..
        } = ty
        else {
            return None;
        };
        if args.len() == 1 {
            Some(args[0].clone())
        } else {
            None
        }
    }

    pub(super) fn direct_method_return_ty(
        &mut self,
        callee: &str,
        receiver_args: &[ResolvedTy],
        span: &Span,
        context: &str,
    ) -> Option<ResolvedTy> {
        let Some(sig) = self.fn_sigs.get(callee).cloned() else {
            self.unsupported(
                span.clone(),
                format!("{context} requires lowered method symbol `{callee}`"),
                "iterator-runtime-dispatch",
            );
            return None;
        };
        let type_params: HashSet<String> = sig.type_params.iter().cloned().collect();
        let Ok(mut ret_ty) = ResolvedTy::from_ty_with_type_params(&sig.return_type, &type_params)
        else {
            self.unsupported(
                span.clone(),
                format!("{context} method `{callee}` has a non-boundary return type"),
                "iterator-runtime-dispatch",
            );
            return None;
        };
        if receiver_args.len() != sig.type_params.len() {
            self.unsupported(
                span.clone(),
                format!(
                    "{context} method `{callee}` has {} type parameter(s), but the receiver supplies {} type argument(s)",
                    sig.type_params.len(),
                    receiver_args.len()
                ),
                "iterator-runtime-dispatch",
            );
            return None;
        }
        if !sig.type_params.is_empty() {
            ret_ty = substitute_type_params(&ret_ty, &sig.type_params, receiver_args);
        }
        Some(ret_ty)
    }

    pub(super) fn generic_iterator_next_shape(
        &mut self,
        iter_ty: &ResolvedTy,
        span: &Span,
    ) -> Option<ResolvedTy> {
        let ResolvedTy::Named {
            head:
                head @ (hew_types::TypeHead::Nominal(_)
                | hew_types::TypeHead::Param(_)
                | hew_types::TypeHead::Unresolved(_)),
            args,
            ..
        } = iter_ty
        else {
            return None;
        };
        let name = head.registry_key();
        let callee = crate::node::HirImplBlock::method_symbol(name, "next");
        let sig = self.fn_sigs.get(&callee).cloned()?;
        if !sig.requires_mutable_receiver {
            self.unsupported(
                span.clone(),
                format!("generic for-in requires iterator method `{callee}` to take `var self`"),
                "iterator-runtime-dispatch",
            );
            return None;
        }
        let ret_ty = self.direct_method_return_ty(&callee, args, span, "generic for-in next")?;
        let Some(elem_ty) = Self::resolved_option_elem_ty(&ret_ty) else {
            self.unsupported(
                span.clone(),
                format!(
                    "generic for-in requires iterator method `{callee}` to return `Option<T>`, got `{ret_ty}`"
                ),
                "iterator-runtime-dispatch",
            );
            return None;
        };
        Some(elem_ty)
    }

    pub(super) fn for_iter_next_call_for_ty(
        &mut self,
        iter_ty: &ResolvedTy,
        span: &Span,
    ) -> Option<(ResolvedTy, ForIterNextCall)> {
        if let ResolvedTy::Named {
            args,
            head: hew_types::TypeHead::Builtin(BuiltinType::VecIter),
            ..
        } = iter_ty
        {
            if args.len() == 1 {
                return Some((args[0].clone(), ForIterNextCall::BuiltinVecIter));
            }
        }
        self.generic_iterator_next_shape(iter_ty, span)
            .map(|elem_ty| (elem_ty, ForIterNextCall::VarSelf))
    }

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

    pub(super) fn generic_into_iter_init(
        &mut self,
        iterable: HirExpr,
        span: &Span,
    ) -> Option<(HirExpr, ResolvedTy, ResolvedTy, ForIterNextCall)> {
        let ResolvedTy::Named {
            head:
                head @ (hew_types::TypeHead::Nominal(_)
                | hew_types::TypeHead::Param(_)
                | hew_types::TypeHead::Unresolved(_)),
            args,
            ..
        } = &iterable.ty
        else {
            return None;
        };
        let name = head.registry_key();
        let callee = crate::node::HirImplBlock::method_symbol(name, "into_iter");
        if !self.fn_sigs.contains_key(&callee) {
            return None;
        }
        let receiver_ty = iterable.ty.clone();
        let ret_ty =
            self.direct_method_return_ty(&callee, args, span, "generic for-in into_iter")?;
        let (elem_ty, next_call) = self.for_iter_next_call_for_ty(&ret_ty, span)?;
        let call = self.make_direct_method_call(callee.clone(), iterable, &ret_ty, span.clone());
        self.record_var_self_direct_monomorphisation(&callee, &receiver_ty, span, call.site);
        Some((call, ret_ty, elem_ty, next_call))
    }

    pub(super) fn make_option_ctor(
        &mut self,
        variant_name: &str,
        payload: Option<HirExpr>,
        elem_ty: &ResolvedTy,
        span: Span,
    ) -> HirExpr {
        let option_ty = Self::resolved_option_ty(elem_ty.clone());
        let variant_idx = match variant_name {
            "Some" => 0,
            "None" => 1,
            _ => unreachable!("only Option::Some/None are synthesized"),
        };
        let payload = payload.map(|expr| vec![("0".to_string(), expr)]);
        self.make_expr(
            HirExprKind::MachineVariantCtor {
                machine_name: "Option".to_string(),
                state_idx: variant_idx,
                payload,
            },
            option_ty,
            IntentKind::Read,
            span,
        )
    }

    pub(super) fn lower_builtin_vec_into_iter(
        &mut self,
        receiver: &Spanned<Expr>,
        elem_ty: ResolvedTy,
        span: Span,
    ) -> (HirExprKind, ResolvedTy) {
        let receiver_hir = self.lower_expr(receiver, IntentKind::Consume);
        let iter_expr = self.make_vec_iter_init(receiver_hir, elem_ty, span);
        (iter_expr.kind, iter_expr.ty)
    }

    /// Construct an ordinary cursor value. SIR copies a surviving source
    /// binding into the cursor and transfers a temporary, preserving one
    /// evaluation and an independent snapshot through the common value rules.
    pub(super) fn lower_builtin_vec_iter(
        &mut self,
        receiver: &Spanned<Expr>,
        elem_ty: ResolvedTy,
        span: Span,
    ) -> (HirExprKind, ResolvedTy) {
        let receiver_hir = self.lower_expr(receiver, IntentKind::Read);
        let iter_expr = self.make_vec_iter_init(receiver_hir, elem_ty, span);
        (iter_expr.kind, iter_expr.ty)
    }

    /// Expand `m.into_iter()` over a `HashMap<K, V>` into the same
    /// `HashMapIter<K, V> { ks: m.keys(), vs: m.values(), idx: 0 }` cursor the
    /// `for (k, v) in m` desugar builds — the map twin of
    /// `lower_builtin_vec_into_iter`. The `keys()` / `values()` projections are
    /// spanned at the call's start/end offsets (zero-width, distinct from each
    /// other and every real span), reproduced byte-for-byte from the checker's
    /// `BuiltinHashMapIntoIter` recording so the span-keyed resolved-call facts
    /// resolve. The projections clone every key/value into fresh owned `Vec`s,
    /// so each yielded `(K, V)` is independently droppable and the source map
    /// stays live.
    ///
    /// A place receiver (identifier/field/index) is re-read once per projection
    /// — the proven drop-safe path: each read borrows its owner, no second owner
    /// of the map handle. A non-place rvalue (`make_map().into_iter()`) is bound
    /// to one temp so the source runs EXACTLY once; both projections borrow the
    /// temp. This mirrors the `HashMap` for-in single-eval temp; recording
    /// `keys()` AND `values()` against the same cloned AST would otherwise
    /// evaluate a side-effectful producer twice.
    pub(super) fn lower_builtin_hashmap_into_iter(
        &mut self,
        receiver: &Spanned<Expr>,
        key_ty: &ResolvedTy,
        val_ty: &ResolvedTy,
        span: Span,
    ) -> (HirExprKind, ResolvedTy) {
        let elem_ty = ResolvedTy::Tuple(vec![key_ty.clone(), val_ty.clone()]);
        let iter_ty = Self::resolved_hashmap_iter_ty(key_ty.clone(), val_ty.clone());
        self.register_synthetic_cursor_layout(
            BuiltinType::HashMapIter,
            &[key_ty.clone(), val_ty.clone()],
            &span,
        );
        self.register_option_layout(&elem_ty, &span, "HashMapIter::next");
        if Self::for_in_iterable_is_place(&receiver.0) {
            // Place source: re-read directly per projection (single owner, drop-safe).
            let init = self.make_hashmap_iter_init(
                receiver.clone(),
                key_ty,
                val_ty,
                iter_ty.clone(),
                &span,
            );
            return (init.kind, iter_ty);
        }
        // Non-place rvalue: bind to one temp so the source evaluates once.
        let block_scope = self.ids.scope();
        self.push_scope();
        let temp_name = format!("__hew_into_iter_src_{}", self.ids.binding().0);
        let src_ty = ResolvedTy::Named {
            args: vec![key_ty.clone(), val_ty.clone()],
            head: hew_types::TypeHead::Builtin(BuiltinType::HashMap),
            is_opaque: false,
        };
        let temp_binding = self.bind(temp_name.clone(), src_ty, false, span.clone());
        let recv_hir = self.lower_expr(receiver, IntentKind::Consume);
        let init_stmt = HirStmt {
            node: self.ids.node(),
            kind: HirStmtKind::Let(temp_binding, Some(recv_hir)),
            span: span.clone(),
        };
        let tail = self.make_hashmap_iter_init(
            (Expr::Ident(Ident::new(&temp_name)), span.clone()),
            key_ty,
            val_ty,
            iter_ty.clone(),
            &span,
        );
        self.pop_scope();
        (
            HirExprKind::Block(HirBlock {
                node: self.ids.node(),
                scope: block_scope,
                statements: vec![init_stmt],
                tail: Some(Box::new(tail)),
                ty: iter_ty.clone(),
                span,
            }),
            iter_ty,
        )
    }

    /// Build the `HashMapIter<K, V> { ks: recv.keys(), vs: recv.values(), idx: 0 }`
    /// `StructInit` from a projection receiver. `keys()`/`values()` are spanned at
    /// the call span's start/end offsets, matching the checker's
    /// `BuiltinHashMapIntoIter` recording so the span-keyed projection facts
    /// resolve. The receiver keeps its own original span and is either re-read
    /// (place) or the single-eval temp.
    pub(super) fn make_hashmap_iter_init(
        &mut self,
        receiver: Spanned<Expr>,
        key_ty: &ResolvedTy,
        val_ty: &ResolvedTy,
        iter_ty: ResolvedTy,
        span: &Span,
    ) -> HirExpr {
        let keys_span = span.start..span.start;
        let values_span = span.end..span.end;
        let keys_call = (
            Expr::MethodCall {
                receiver: Box::new(receiver.clone()),
                method: (Ident::new("keys"), keys_span.clone()),
                args: Vec::new(),
            },
            keys_span,
        );
        let values_call = (
            Expr::MethodCall {
                receiver: Box::new(receiver),
                method: (Ident::new("values"), values_span.clone()),
                args: Vec::new(),
            },
            values_span,
        );
        let keys_hir = self.lower_expr(&keys_call, IntentKind::Consume);
        let values_hir = self.lower_expr(&values_call, IntentKind::Consume);
        let idx = self.make_i64_literal(0, span.clone());
        self.make_expr(
            HirExprKind::StructInit {
                name: "HashMapIter".to_string(),
                type_args: vec![key_ty.clone(), val_ty.clone()],
                fields: vec![
                    ("ks".to_string(), keys_hir),
                    ("vs".to_string(), values_hir),
                    ("idx".to_string(), idx),
                ],
                base: None,
            },
            iter_ty,
            IntentKind::Read,
            span.clone(),
        )
    }

    pub(super) fn make_vec_iter_init(
        &mut self,
        receiver_hir: HirExpr,
        elem_ty: ResolvedTy,
        span: Span,
    ) -> HirExpr {
        let idx = self.make_i64_literal(0, span.clone());
        let iter_ty = Self::resolved_vec_iter_ty(elem_ty.clone());
        self.make_expr(
            HirExprKind::StructInit {
                name: "VecIter".to_string(),
                type_args: vec![elem_ty],
                fields: vec![("vec".to_string(), receiver_hir), ("idx".to_string(), idx)],
                base: None,
            },
            iter_ty,
            IntentKind::Read,
            span,
        )
    }

    /// Whether a for-in iterable expression is a *place* — an lvalue that can be
    /// read more than once with no observable side effect and no extra
    /// evaluation cost (an identifier, or a field/index projection rooted in a
    /// place).
    ///
    /// `HashMap` for-in takes two projections (`keys()`, `values()`) and
    /// `HashSet` for-in takes one (`to_vec()`), each lowered as a `MethodCall` on
    /// the iterable. For a place the receiver is re-lowered directly per
    /// projection — the proven drop-safe path: a field/index read borrows its
    /// owner, yields a fresh owned `Vec`, and leaves the source owner live (no
    /// second owner of the collection handle, so no double-free at scope exit).
    /// For a non-place rvalue (a call, a method call, …) re-lowering would
    /// *re-evaluate* the source once per projection — `for x in make_set()` would
    /// call `make_set()` twice — so those bind the value to a single-eval temp
    /// instead ([`Self::bind_for_in_source`]).
    pub(super) fn for_in_iterable_is_place(expr: &Expr) -> bool {
        match expr {
            Expr::Ident(_) => true,
            Expr::FieldAccess { object, .. } | Expr::Index { object, .. } => {
                Self::for_in_iterable_is_place(&object.0)
            }
            _ => false,
        }
    }

    /// Bind a for-in iterable's already-lowered value to a synthetic temp so a
    /// non-place `HashMap`/`HashSet` source is evaluated exactly once.
    ///
    /// Only used for non-place rvalue sources (see
    /// [`Self::for_in_iterable_is_place`]); place sources re-lower their
    /// projection receivers directly. The temp owns the collection and its
    /// scope-exit drop frees it exactly once; the projections borrow the temp
    /// (Read), so the temp stays the sole owner. Returns the temp's name (for
    /// building `Expr::Ident` receivers) and the `Let` statement to prepend
    /// to the for-in's outer block.
    pub(super) fn bind_for_in_source(
        &mut self,
        lowered_iterable: HirExpr,
        source_ty: ResolvedTy,
        span: &Span,
    ) -> (String, HirStmt) {
        let src_name = format!("__hew_for_src_{}", self.ids.binding().0);
        let src_binding = self.bind(src_name.clone(), source_ty, false, span.clone());
        let src_stmt = HirStmt {
            node: self.ids.node(),
            kind: HirStmtKind::Let(src_binding, Some(lowered_iterable)),
            span: span.clone(),
        };
        (src_name, src_stmt)
    }

    /// Build the for-in shape for `for (k, v) in m` over a `HashMap<K, V>`.
    ///
    /// Synthesizes a `HashMapIter<K, V> { ks: src.keys(), vs: src.values(), idx: 0 }`
    /// constructor as AST and lowers it through the normal `StructInit` path so
    /// the cursor's per-`(K, V)` layout monomorphises exactly like any user
    /// record. `receiver` is the spanned projection receiver — the iterable AST
    /// with its original span for a place source (re-read per projection,
    /// drop-safe), or an `Expr::Ident` for the single-eval temp of a
    /// non-place rvalue source.
    /// The `keys()`/`values()` calls are spanned at two synthetic zero-width
    /// spans (`iterable.start..start`, `iterable.end..end`) the checker recorded
    /// the `keys`/`values` resolved-call facts at (`resolved_calls` is keyed by
    /// span, so the two projections must not share one, and neither may share the
    /// iterable's real span — that would clobber `expr_types[iterable_span]` and
    /// mis-route non-identifier sources to the Vec arm). Iteration then drives
    /// `HashMapIter::next` via the same `VarSelf` direct-call shape user iterator
    /// types use.
    pub(super) fn lower_hashmap_for_in_init(
        &mut self,
        iterable: &Spanned<Expr>,
        receiver: &Spanned<Expr>,
        key_ty: ResolvedTy,
        val_ty: ResolvedTy,
    ) -> (HirExpr, ResolvedTy, ResolvedTy, ForIterNextCall) {
        let iterable_span = &iterable.1;
        let elem_ty = ResolvedTy::Tuple(vec![key_ty.clone(), val_ty.clone()]);
        let iter_ty = Self::resolved_hashmap_iter_ty(key_ty.clone(), val_ty.clone());
        // Register the concrete cursor layout and the `Option<(K, V)>` next
        // payload layout so MIR's field-order + value-class lookups resolve.
        self.register_synthetic_cursor_layout(
            BuiltinType::HashMapIter,
            &[key_ty.clone(), val_ty.clone()],
            iterable_span,
        );
        self.register_option_layout(&elem_ty, iterable_span, "HashMapIter::next");
        // `keys()` and `values()` are taken from the single-eval `src` temp
        // (NOT a re-lowered clone of the iterable AST — that would evaluate a
        // side-effectful source twice). Each call is spanned at its own
        // synthetic zero-width span: `keys()` at `iterable.start..start`,
        // `values()` at `iterable.end..end`. Their receiver keeps its original
        // iterable span; otherwise a field/tuple projection reads the
        // projection's `Vec` result type from `expr_types` instead of the
        // receiver's `HashMap` type. The call-span derivation MUST match
        // `Checker::hashmap_for_in_keys_span`/`..values_span` (hew-types)
        // byte-for-byte — `resolved_calls`/`expr_types` are span-keyed, so a
        // drift here would miss the checker's facts and trip the HIR boundary's
        // totality contract. Neither span is the iterable's real span, so
        // `expr_types[iterable_span]` keeps the map's true type and routing of
        // non-identifier sources (field/call/index) stays correct. Each
        // projection is lowered as a real `MethodCall` so it picks up the
        // checker's `keys`/`values` resolved-call fact (→ the
        // `hew_hashmap_keys_layout`/`values_layout` runtime symbols and their
        // clone-on-read discipline). The receiver (`receiver`) is borrowed (Read)
        // inside the resolved-impl-call lowering, so a place source's owner — or
        // the single-eval temp — stays the sole owner and drops once.
        let keys_span = iterable_span.start..iterable_span.start;
        let values_span = iterable_span.end..iterable_span.end;
        let keys_call = (
            Expr::MethodCall {
                receiver: Box::new(receiver.clone()),
                method: (Ident::new("keys"), keys_span.clone()),
                args: Vec::new(),
            },
            keys_span,
        );
        let values_call = (
            Expr::MethodCall {
                receiver: Box::new(receiver.clone()),
                method: (Ident::new("values"), values_span.clone()),
                args: Vec::new(),
            },
            values_span,
        );
        // The projections produce fresh owned Vecs; the StructInit consumes
        // them into the cursor.
        let keys_hir = self.lower_expr(&keys_call, IntentKind::Consume);
        let values_hir = self.lower_expr(&values_call, IntentKind::Consume);
        let idx = self.make_i64_literal(0, iterable_span.clone());
        // Build the `HashMapIter<K, V>` StructInit HIR directly (carrying its
        // `type_args` so MIR mangles the concrete layout), mirroring
        // `make_vec_iter_init`; the layout + Option payload were registered
        // above.
        let iter_init = self.make_expr(
            HirExprKind::StructInit {
                name: "HashMapIter".to_string(),
                type_args: vec![key_ty, val_ty],
                fields: vec![
                    ("ks".to_string(), keys_hir),
                    ("vs".to_string(), values_hir),
                    ("idx".to_string(), idx),
                ],
                base: None,
            },
            iter_ty.clone(),
            IntentKind::Read,
            iterable_span.clone(),
        );
        (iter_init, iter_ty, elem_ty, ForIterNextCall::VarSelf)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "synthetic VecIter::next expansion must build the full caller-side state machine in HIR"
    )]
    pub(super) fn lower_builtin_vec_iter_next(
        &mut self,
        receiver: &Spanned<Expr>,
        elem_ty: &ResolvedTy,
        span: Span,
    ) -> (HirExprKind, ResolvedTy) {
        self.register_option_layout(elem_ty, &span, "VecIter::next");
        let option_ty = Self::resolved_option_ty(elem_ty.clone());
        let iter_ty = Self::resolved_vec_iter_ty(elem_ty.clone());
        let lowered_receiver = self.lower_expr(receiver, IntentKind::Modify);
        let HirExprKind::BindingRef {
            name: receiver_name,
            resolved: ResolvedRef::Binding(receiver_binding),
        } = &lowered_receiver.kind
        else {
            self.unsupported(
                span.clone(),
                "VecIter.next requires a mutable binding receiver in the Rust MIR pipeline",
                "iterator-runtime-dispatch",
            );
            return (
                HirExprKind::Unsupported("VecIter::next receiver is not a binding".into()),
                option_ty,
            );
        };
        let receiver_name = receiver_name.clone();
        let receiver_binding = *receiver_binding;

        if self
            .owning_take_vec_cursors
            .contains(&SpanKey::in_module(&span, self.current_module_idx))
        {
            return self.lower_builtin_vec_iter_take_next(
                receiver_name,
                receiver_binding,
                &iter_ty,
                elem_ty,
                span,
            );
        }

        self.push_scope();
        let iter_obj = self.make_binding_ref(
            receiver_name.clone(),
            receiver_binding,
            iter_ty.clone(),
            IntentKind::Read,
            span.clone(),
        );
        let idx_read = self.make_expr(
            HirExprKind::FieldAccess {
                object: Box::new(iter_obj),
                field: "idx".to_string(),
            },
            ResolvedTy::I64,
            IntentKind::Read,
            span.clone(),
        );
        let iter_obj = self.make_binding_ref(
            receiver_name.clone(),
            receiver_binding,
            iter_ty.clone(),
            IntentKind::Read,
            span.clone(),
        );
        let vec_read_for_len = self.make_expr(
            HirExprKind::FieldAccess {
                object: Box::new(iter_obj),
                field: "vec".to_string(),
            },
            Self::resolved_vec_ty(elem_ty.clone()),
            IntentKind::Read,
            span.clone(),
        );
        let len_call = self.make_vec_len_call(vec_read_for_len, elem_ty, span.clone());
        let condition = self.make_expr(
            HirExprKind::Binary {
                op: BinaryOp::GreaterEqual,
                left: Box::new(idx_read),
                right: Box::new(len_call),
            },
            ResolvedTy::Bool,
            IntentKind::Read,
            span.clone(),
        );

        let none_expr = self.make_option_ctor("None", None, elem_ty, span.clone());
        let then_block =
            self.make_unit_block(Vec::new(), Some(none_expr), option_ty.clone(), span.clone());
        let then_expr = self.make_expr(
            HirExprKind::Block(then_block),
            option_ty.clone(),
            IntentKind::Read,
            span.clone(),
        );

        let iter_obj = self.make_binding_ref(
            receiver_name.clone(),
            receiver_binding,
            iter_ty.clone(),
            IntentKind::Read,
            span.clone(),
        );
        let vec_read_for_get = self.make_expr(
            HirExprKind::FieldAccess {
                object: Box::new(iter_obj),
                field: "vec".to_string(),
            },
            Self::resolved_vec_ty(elem_ty.clone()),
            IntentKind::Read,
            span.clone(),
        );
        let iter_obj = self.make_binding_ref(
            receiver_name.clone(),
            receiver_binding,
            iter_ty.clone(),
            IntentKind::Read,
            span.clone(),
        );
        let idx_read_for_get = self.make_expr(
            HirExprKind::FieldAccess {
                object: Box::new(iter_obj),
                field: "idx".to_string(),
            },
            ResolvedTy::I64,
            IntentKind::Read,
            span.clone(),
        );
        let value_kind = self.collection_call_kind(
            hew_types::RuntimeCallFamily::Vector(hew_types::VecValueOp::Get),
            vec![vec_read_for_get, idx_read_for_get],
            &option_ty,
            &span,
        );
        let value_expr = self.make_expr(
            value_kind,
            option_ty.clone(),
            IntentKind::Read,
            span.clone(),
        );
        let value_binding_name = format!("__hew_iter_value_{}", self.ids.binding().0);
        let value_binding = self.bind(
            value_binding_name.clone(),
            option_ty.clone(),
            false,
            span.clone(),
        );
        let value_binding_id = value_binding.id;
        let value_stmt = HirStmt {
            node: self.ids.node(),
            kind: HirStmtKind::Let(value_binding, Some(value_expr)),
            span: span.clone(),
        };

        let iter_obj = self.make_binding_ref(
            receiver_name.clone(),
            receiver_binding,
            iter_ty.clone(),
            IntentKind::Modify,
            span.clone(),
        );
        let idx_assign_target = self.make_expr(
            HirExprKind::FieldAccess {
                object: Box::new(iter_obj),
                field: "idx".to_string(),
            },
            ResolvedTy::I64,
            IntentKind::Modify,
            span.clone(),
        );
        let iter_obj = self.make_binding_ref(
            receiver_name,
            receiver_binding,
            iter_ty,
            IntentKind::Read,
            span.clone(),
        );
        let idx_read_for_add = self.make_expr(
            HirExprKind::FieldAccess {
                object: Box::new(iter_obj),
                field: "idx".to_string(),
            },
            ResolvedTy::I64,
            IntentKind::Read,
            span.clone(),
        );
        let one = self.make_i64_literal(1, span.clone());
        let idx_plus_one = self.make_expr(
            HirExprKind::Binary {
                op: BinaryOp::Add,
                left: Box::new(idx_read_for_add),
                right: Box::new(one),
            },
            ResolvedTy::I64,
            IntentKind::Read,
            span.clone(),
        );
        let assign_stmt = HirStmt {
            node: self.ids.node(),
            kind: HirStmtKind::Assign {
                target: idx_assign_target,
                value: Box::new(idx_plus_one),
                first_store: false,
            },
            span: span.clone(),
        };
        let value_ref = self.make_binding_ref(
            value_binding_name,
            value_binding_id,
            option_ty.clone(),
            // The ordinary block-result transfer owns the extracted item.
            IntentKind::Consume,
            span.clone(),
        );
        let else_block = self.make_unit_block(
            vec![value_stmt, assign_stmt],
            Some(value_ref),
            option_ty.clone(),
            span.clone(),
        );
        let else_expr = self.make_expr(
            HirExprKind::Block(else_block),
            option_ty.clone(),
            IntentKind::Read,
            span.clone(),
        );
        let if_expr = self.make_expr(
            HirExprKind::If {
                condition: Box::new(condition),
                then_expr: Box::new(then_expr),
                else_expr: Some(Box::new(else_expr)),
            },
            option_ty.clone(),
            IntentKind::Read,
            span.clone(),
        );
        let block = self.make_unit_block(Vec::new(), Some(if_expr), option_ty.clone(), span);
        self.pop_scope();
        (HirExprKind::Block(block), option_ty)
    }

    /// `VecIter.next()` for an element with no semantic clone: each step moves
    /// the first element out of the vector the cursor owns.
    ///
    /// There is no index to advance — the removal shifts the tail down, so the
    /// cursor is empty exactly when the vector is, and a completed drain leaves
    /// it empty. An early exit drops the cursor, whose `vec` field releases
    /// whatever the drain did not reach.
    pub(super) fn lower_builtin_vec_iter_take_next(
        &mut self,
        receiver_name: String,
        receiver_binding: BindingId,
        iter_ty: &ResolvedTy,
        elem_ty: &ResolvedTy,
        span: Span,
    ) -> (HirExprKind, ResolvedTy) {
        let option_ty = Self::resolved_option_ty(elem_ty.clone());
        self.push_scope();
        let iter_obj = self.make_binding_ref(
            receiver_name.clone(),
            receiver_binding,
            iter_ty.clone(),
            IntentKind::Read,
            span.clone(),
        );
        let vec_read_for_len = self.make_expr(
            HirExprKind::FieldAccess {
                object: Box::new(iter_obj),
                field: "vec".to_string(),
            },
            Self::resolved_vec_ty(elem_ty.clone()),
            IntentKind::Read,
            span.clone(),
        );
        let len_call = self.make_vec_len_call(vec_read_for_len, elem_ty, span.clone());
        let zero = self.make_i64_literal(0, span.clone());
        let condition = self.make_expr(
            HirExprKind::Binary {
                op: BinaryOp::LessEqual,
                left: Box::new(len_call),
                right: Box::new(zero),
            },
            ResolvedTy::Bool,
            IntentKind::Read,
            span.clone(),
        );

        let none_expr = self.make_option_ctor("None", None, elem_ty, span.clone());
        let then_block =
            self.make_unit_block(Vec::new(), Some(none_expr), option_ty.clone(), span.clone());
        let then_expr = self.make_expr(
            HirExprKind::Block(then_block),
            option_ty.clone(),
            IntentKind::Read,
            span.clone(),
        );

        let iter_obj = self.make_binding_ref(
            receiver_name,
            receiver_binding,
            iter_ty.clone(),
            IntentKind::Modify,
            span.clone(),
        );
        let vec_take_target = self.make_expr(
            HirExprKind::FieldAccess {
                object: Box::new(iter_obj),
                field: "vec".to_string(),
            },
            Self::resolved_vec_ty(elem_ty.clone()),
            IntentKind::Modify,
            span.clone(),
        );
        let take_kind = self.collection_call_kind(
            hew_types::RuntimeCallFamily::Vector(hew_types::VecValueOp::TakeFirst),
            vec![vec_take_target],
            elem_ty,
            &span,
        );
        let take_expr = self.make_expr(
            take_kind,
            elem_ty.clone(),
            IntentKind::Consume,
            span.clone(),
        );
        let some_expr = self.make_option_ctor("Some", Some(take_expr), elem_ty, span.clone());
        let else_block =
            self.make_unit_block(Vec::new(), Some(some_expr), option_ty.clone(), span.clone());
        let else_expr = self.make_expr(
            HirExprKind::Block(else_block),
            option_ty.clone(),
            IntentKind::Read,
            span.clone(),
        );
        let if_expr = self.make_expr(
            HirExprKind::If {
                condition: Box::new(condition),
                then_expr: Box::new(then_expr),
                else_expr: Some(Box::new(else_expr)),
            },
            option_ty.clone(),
            IntentKind::Read,
            span.clone(),
        );
        let block = self.make_unit_block(Vec::new(), Some(if_expr), option_ty.clone(), span);
        self.pop_scope();
        (HirExprKind::Block(block), option_ty)
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
        element: (&str, &Span, &ResolvedTy),
        body: &Block,
        label: Option<&String>,
        span: Span,
        source: (&Span, Option<&Spanned<Expr>>),
        borrowed: bool,
    ) -> HirExprKind {
        let (var_name, pattern_span, element_ty) = element;
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
        let element_binding = self.bind(
            var_name.to_string(),
            element_ty.clone(),
            false,
            pattern_span.clone(),
        );
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
        clippy::if_not_else,
        reason = "the desugar keeps diagnostic recovery before the executable iterator branch"
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
                (&var_name, &pattern.1, &element_ty),
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
                (&var_name, &pattern.1, &element_ty),
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
                    (&var_name, &pattern.1, &element_ty),
                    body,
                    label,
                    span,
                    (&iterable.1, source),
                    true,
                );
            }
        }

        // Statements that must run before the iterator-cursor `Let` in the
        // for-in's outer block. The HashMap/HashSet arms push a single-eval
        // source temp here so a side-effectful iterable is evaluated once.
        let mut source_prelude: Vec<HirStmt> = Vec::new();
        let (iter_init, iter_ty, elem_ty, next_call) = match lowered_iterable.ty.clone() {
            ResolvedTy::Named {
                args,
                head: hew_types::TypeHead::Builtin(BuiltinType::Vec),
                ..
            } if args.len() == 1 => {
                let elem_ty = args[0].clone();
                // Keep the iterable's lexical block intact. Its temporary bindings
                // end before the cursor loop and must not become loop-carried values.
                (
                    self.make_vec_iter_init(lowered_iterable, elem_ty.clone(), iterable.1.clone()),
                    Self::resolved_vec_iter_ty(elem_ty.clone()),
                    elem_ty,
                    ForIterNextCall::BuiltinVecIter,
                )
            }
            ResolvedTy::Named {
                args,
                head: hew_types::TypeHead::Builtin(BuiltinType::HashMap),
                ..
            } if args.len() >= 2 => {
                // `for (k, v) in m` over a HashMap builds a `HashMapIter`
                // cursor at this concrete site from the map's `keys()` and
                // `values()` projections (the checker recorded the resolved
                // calls at two synthetic spans — see the HashMap arm in
                // statements.rs `Stmt::For`). The map is a refcounted handle;
                // like Vec for-in, sharing it (Capture) leaves the source
                // binding Live after the loop, and the projections each clone
                // every key/value into a fresh owned `Vec`, so every yielded
                // `(K, V)` is independently droppable and the cursor never
                // co-owns a yielded pair with the source map.
                //
                // A place source (identifier/field/index) is re-read directly
                // per projection — the proven drop-safe path (the read borrows
                // its owner; no second owner of the map handle). A non-place
                // rvalue (`for (k, v) in next_map()`) would re-evaluate the
                // source once per projection, so it is bound to a single-eval
                // temp and both projections borrow the temp.
                lowered_iterable.intent = IntentKind::Capture;
                let key_ty = args[0].clone();
                let val_ty = args[1].clone();
                if Self::for_in_iterable_is_place(&iterable.0) {
                    self.lower_hashmap_for_in_init(iterable, iterable, key_ty, val_ty)
                } else {
                    let source_ty = lowered_iterable.ty.clone();
                    let (src_name, src_stmt) =
                        self.bind_for_in_source(lowered_iterable, source_ty, &iterable.1);
                    source_prelude.push(src_stmt);
                    let receiver = (Expr::Ident(Ident::new(&src_name)), iterable.1.clone());
                    self.lower_hashmap_for_in_init(iterable, &receiver, key_ty, val_ty)
                }
            }
            ResolvedTy::Named {
                args,
                head: hew_types::TypeHead::Builtin(BuiltinType::HashSet),
                ..
            } if !args.is_empty() => {
                // `for x in s` over a HashSet snapshots the set's elements into
                // an owned `Vec<T>` via `to_vec()` (the checker recorded the
                // resolved call at a synthetic zero-width span — see the HashSet
                // arm in statements.rs `Stmt::For`) and iterates that Vec through
                // the proven `VecIter` cursor — each element is a fresh clone, so
                // it is independently droppable; the set is shared (Capture) and
                // stays live after the loop. A place source is re-read directly
                // (drop-safe); a non-place rvalue (`for x in make_set()`) binds
                // a single-eval temp so the source runs once, and `to_vec()`
                // borrows the temp.
                lowered_iterable.intent = IntentKind::Capture;
                let elem_ty = args[0].clone();
                // `to_vec()` is spanned at `iterable.start..start` (synthetic
                // zero-width, matching `Checker::hashset_for_in_to_vec_span`
                // byte-for-byte), NOT the iterable's real span — that span keeps
                // the set's true type so non-identifier sources route here, not
                // to the Vec arm. Only the CALL uses the synthetic span: its
                // receiver keeps the real iterable span so a field/tuple
                // projection reads its HashSet type rather than the call's Vec
                // result type.
                let to_vec_span = iterable.1.start..iterable.1.start;
                let to_vec_receiver = if Self::for_in_iterable_is_place(&iterable.0) {
                    iterable.clone()
                } else {
                    let source_ty = lowered_iterable.ty.clone();
                    let (src_name, src_stmt) =
                        self.bind_for_in_source(lowered_iterable, source_ty, &iterable.1);
                    source_prelude.push(src_stmt);
                    (Expr::Ident(Ident::new(&src_name)), iterable.1.clone())
                };
                let to_vec_call = (
                    Expr::MethodCall {
                        receiver: Box::new(to_vec_receiver),
                        method: (Ident::new("to_vec"), to_vec_span.clone()),
                        args: Vec::new(),
                    },
                    to_vec_span,
                );
                let vec_hir = self.lower_expr(&to_vec_call, IntentKind::Consume);
                let iter_init =
                    self.make_vec_iter_init(vec_hir, elem_ty.clone(), iterable.1.clone());
                (
                    iter_init,
                    Self::resolved_vec_iter_ty(elem_ty.clone()),
                    elem_ty,
                    ForIterNextCall::BuiltinVecIter,
                )
            }
            ResolvedTy::Named {
                args,
                head: hew_types::TypeHead::Builtin(BuiltinType::VecIter),
                ..
            } if args.len() == 1 => {
                // VecIter is already an iterator object; consuming it drives
                // the cursor forward and the source binding is fully drained.
                lowered_iterable.intent = IntentKind::Consume;
                (
                    lowered_iterable,
                    Self::resolved_vec_iter_ty(args[0].clone()),
                    args[0].clone(),
                    ForIterNextCall::BuiltinVecIter,
                )
            }
            ResolvedTy::Named {
                args,
                head: hew_types::TypeHead::Builtin(BuiltinType::Stream),
                ..
            } if !args.is_empty() => {
                let elem_ty = args[0].clone();
                if let Some(reason) = Self::queue_elem_witness_unsupported(&elem_ty) {
                    self.unsupported(
                        iterable.1.clone(),
                        format!("for over Stream<{elem_ty}>: {reason}"),
                        "for-stream-runtime-dispatch",
                    );
                    self.push_scope();
                    let _ = self.bind(var_name.clone(), elem_ty.clone(), false, pattern.1.clone());
                    let _ = self.lower_block(body, &ResolvedTy::Unit);
                    self.pop_scope();
                    return HirExprKind::Unsupported(
                        "for over unsupported Stream<T> element type".into(),
                    );
                }
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
                // Generic IntoIterator/Iterator: the into_iter() call consumes
                // the source, or a plain Iterator binding is drained.
                lowered_iterable.intent = IntentKind::Consume;
                if let Some(shape) =
                    self.generic_into_iter_init(lowered_iterable.clone(), &iterable.1)
                {
                    shape
                } else if let Some(elem_ty) = self.generic_iterator_next_shape(&other, &iterable.1)
                {
                    (lowered_iterable, other, elem_ty, ForIterNextCall::VarSelf)
                } else {
                    self.unsupported(
                    iterable.1.clone(),
                    format!(
                        "for-in over non-Range iterable `{other}`; the Rust MIR pipeline currently supports Vec<T>, VecIter<T>, and concrete Iterator/IntoIterator impls"
                    ),
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
            ForIterNextCall::BuiltinVecIter => {
                let next_receiver = (Expr::Ident(Ident::new(&iter_name)), iterable.1.clone());
                let (next_kind, next_ty) =
                    self.lower_builtin_vec_iter_next(&next_receiver, &elem_ty, iterable.1.clone());
                self.make_expr(next_kind, next_ty, IntentKind::Read, iterable.1.clone())
            }
            ForIterNextCall::VarSelf => {
                let option_ty = Self::resolved_option_ty(elem_ty.clone());
                self.register_option_layout(&elem_ty, &iterable.1, "generic Iterator::next");
                let target_label = match &iter_ty {
                    ResolvedTy::Named {
                        head:
                            hew_types::TypeHead::Builtin(
                                builtin @ (BuiltinType::VecIter | BuiltinType::HashMapIter),
                            ),
                        ..
                    } => crate::node::HirImplBlock::method_symbol(
                        injected_builtin_impl_symbol_owner(builtin.canonical_name()),
                        "next",
                    ),
                    ResolvedTy::Named { head, .. } => {
                        let name = head.registry_key();
                        crate::node::HirImplBlock::method_symbol(name, "next")
                    }
                    _ => String::new(),
                };
                let call_target = self.registered_symbol_target(&target_label);
                if !self.ensure_executable_target(&call_target, &target_label, &iterable.1) {
                    self.make_expr(
                        HirExprKind::Unsupported(
                            "synthetic iterator var-self call has no checker target".to_string(),
                        ),
                        option_ty,
                        IntentKind::Read,
                        iterable.1.clone(),
                    )
                } else {
                    let next_receiver = (Expr::Ident(Ident::new(&iter_name)), iterable.1.clone());
                    let lowered_receiver = self.lower_expr(&next_receiver, IntentKind::Consume);
                    let receiver_ty = iter_ty.clone();
                    let next = self.make_expr(
                        HirExprKind::VarSelfMethodCall {
                            receiver_update: hew_types::ReceiverUpdate::Replace,
                            receiver: Box::new(lowered_receiver),
                            call_target,
                            target: HirVarSelfMethodTarget::Direct,
                            args: Vec::new(),
                            evaluation_order: Vec::new(),
                            ret_ty: option_ty.clone(),
                            receiver_ty,
                        },
                        option_ty,
                        IntentKind::Read,
                        iterable.1.clone(),
                    );
                    // Key the instantiation on the CALL's site, not the
                    // receiver's: SIR resolves a var-self direct call from the
                    // call expression's own site (`hew-sir/src/lower_var_self.rs`),
                    // so a generic iterator's `next` is otherwise reported as
                    // missing its checker-resolved type arguments.
                    self.record_var_self_direct_monomorphisation(
                        &target_label,
                        &iter_ty,
                        &iterable.1,
                        next.site,
                    );
                    // This call root is compiler-generated and therefore has
                    // no authored checker span. Keep its fact in the disjoint
                    // generated-site domain. The closed builtin cursors clone
                    // or move an independently owned element into the returned
                    // Option; user/static iterators remain provisional until
                    // the declaration-keyed verifier resolves their return
                    // summary.

                    next
                }
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
                self.register_option_layout(&elem_ty, &iterable.1, "Stream::recv (for loop)");
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
                self.register_option_layout(&elem_ty, &iterable.1, "Generator::next");
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
        let loop_binding = self.bind(var_name.clone(), elem_ty.clone(), false, pattern.1.clone());
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

        // The single-eval source temp (`__hew_for_src_N`, pushed by the
        // HashMap/HashSet arms) must define before the iterator-cursor `Let`,
        // which builds its projections by borrowing the temp.
        let mut statements = source_prelude;
        statements.push(iter_stmt);
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
}
