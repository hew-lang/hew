//! Comparison, display, assertion and interpolation dispatch.

use super::*;

impl LowerCtx {
    /// Lower a `==`/`!=`/`<`/`<=`/`>`/`>=` binary expression the checker
    /// marked for user-impl dispatch (D340 — see [`UserComparisonDispatch`])
    /// into a call to the resolved impl method, rather than the structural
    /// comparison codegen path `Expr::Binary` otherwise lowers to.
    ///
    /// No trait declares `eq`/`lt` (`Eq`/`Ord`/`PartialOrd` are compiler
    /// marker traits with no `trait_defs` entry — see `MarkerTrait` in
    /// `hew-types/src/traits.rs`), so there is no independent contract
    /// pinning these method names or the derived-order convention below;
    /// this lowering is their sole producer and so is the one place that
    /// convention is decided: `Eq` calls `<type>::eq(left, right)`, negated
    /// for `!=`; `Ord`/`PartialOrd` calls `<type>::lt`, permuting/negating
    /// the operands so every ordering operator reduces to one user-provided
    /// `lt` — `<` is `lt(a,b)`, `>` is `lt(b,a)`, `<=` is `!lt(b,a)`, `>=` is
    /// `!lt(a,b)`.
    pub(super) fn lower_user_comparison_dispatch(
        &mut self,
        dispatch: &UserComparisonDispatch,
        op: BinaryOp,
        left: HirExpr,
        right: HirExpr,
        span: Span,
    ) -> HirExpr {
        let (method, args, negate): (&hew_types::DefId, Vec<HirExpr>, bool) = match dispatch {
            UserComparisonDispatch::Eq { method } => {
                (method, vec![left, right], op == BinaryOp::NotEqual)
            }
            UserComparisonDispatch::Ord { method }
            | UserComparisonDispatch::PartialOrd { method } => match op {
                BinaryOp::Greater => (method, vec![right, left], false),
                BinaryOp::LessEqual => (method, vec![right, left], true),
                BinaryOp::GreaterEqual => (method, vec![left, right], true),
                // `Less`, and any op the checker never records `Ord`
                // dispatch for, share the direct `lt(left, right)` shape.
                _ => (method, vec![left, right], false),
            },
        };
        let Some(symbol) = self.registered_impl_method_symbol(method) else {
            let name = method.full_path().to_string();
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: name.clone(),
                    reason: "selected comparison impl has no emitted body symbol".to_string(),
                },
                span.clone(),
                "checker selected a comparison implementation but HIR did not emit its body",
            ));
            return self.unsupported_expr(span, format!("comparison dispatch: missing {name}"));
        };
        let Some(call) = self.build_user_fn_call(&symbol, args, span.clone()) else {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: symbol.clone(),
                    reason: format!("no fn_registry entry for user comparison impl `{symbol}`"),
                },
                span.clone(),
                "checker recorded a user comparison dispatch but HIR has no corresponding \
                 impl symbol — checker-HIR contract violation",
            ));
            return self.unsupported_expr(span, format!("comparison dispatch: missing {symbol}"));
        };
        if negate {
            self.build_bool_not(call, span)
        } else {
            call
        }
    }

    /// Build a [`HirExprKind::CallTraitMethodStatic`] for the
    /// `MethodCallRewrite::StaticTraitDispatch` arm of `lower_method_call` and
    /// the abstract-`T` arm of `lower_display_dispatch`. SIR selects the
    /// concrete callee from the target identities and the substituted
    /// receiver, and fails closed when no impl is registered.
    pub(super) fn make_static_trait_dispatch_call(
        &mut self,
        receiver: HirExpr,
        target: hew_types::CallTarget,
        receiver_type_param: String,
        args: LoweredCallArgs,
        ret_ty: ResolvedTy,
        span: &Span,
    ) -> HirExprKind {
        if !self.ensure_executable_target(&target, "static trait call", span) {
            return HirExprKind::Unsupported("static trait call has no checker target".to_string());
        }
        HirExprKind::CallTraitMethodStatic {
            receiver: Box::new(receiver),
            target,
            receiver_type_param,
            args: args.args,
            evaluation_order: args.evaluation_order,
            ret_ty,
        }
    }

    /// Preserve method-level instantiation facts until SIR selects the impl
    /// from the concrete receiver. Impl parameters are bound there separately;
    /// the checker recorded only the method's parameters at this call site.
    pub(super) fn record_static_trait_type_args(&mut self, span: &Span, site: SiteId) {
        let Some(arguments) = self.call_type_args.get(&self.mk_key(span)).cloned() else {
            return;
        };
        let mut resolved = Vec::with_capacity(arguments.len());
        for argument in &arguments {
            match ResolvedTy::from_ty(argument) {
                Ok(argument) => {
                    resolved.push(self.qualify_current_module_record_ty(argument));
                }
                Err(error) => {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: "static trait method type arguments".to_string(),
                            reason: error.to_string(),
                        },
                        span.clone(),
                        "static trait method requires checker-resolved type arguments",
                    ));
                    return;
                }
            }
        }
        self.call_site_type_args.insert(site, resolved);
    }

    /// Emit a `Display::fmt` static trait-dispatch over an abstract type
    /// parameter `type_param_name` (#1565). The concrete `Display` impl is
    /// selected per monomorphisation, which fails closed if no impl is
    /// registered. Result is always `string`.
    pub(super) fn build_display_static_dispatch(
        &mut self,
        value: HirExpr,
        target: hew_types::CallTarget,
        type_param_name: String,
        span: Span,
    ) -> HirExpr {
        let kind = self.make_static_trait_dispatch_call(
            value,
            target,
            type_param_name,
            LoweredCallArgs {
                args: Vec::new(),
                evaluation_order: Vec::new(),
            },
            ResolvedTy::String,
            &span,
        );
        HirExpr {
            node: self.ids.node(),
            site: self.ids.site(),
            ty: ResolvedTy::String,
            intent: IntentKind::Read,
            kind,
            span,
        }
    }

    /// Lower a single interpolant of an f-string to a `string`-typed HIR
    /// expression by dispatching through `Display`.
    ///
    /// The display *method name* is resolved through
    /// [`hew_types::LangItemRegistry`] (`LANG_ITEM_DISPLAY_FMT` key)
    /// rather than hard-coded so renaming the stdlib `fmt` method only
    /// requires moving the `#[lang_item("display_fmt")]` attribute. With
    /// no registry entry, every interpolation is rejected fail-closed.
    ///
    /// Concretely:
    ///
    /// * `string` values prefer a user `impl Display for string` if one
    ///   exists (resolved through the per-type method symbol); otherwise
    ///   they pass through identity. The stdlib provides the identity
    ///   impl so a non-user-overridden `string` interpolation does call
    ///   it.
    /// * Primitives route through the per-type `to_string_*` catalog
    ///   entries that back the built-in `impl Display for <primitive>`
    ///   blocks in `std::builtins`.
    /// * Named user types route through the registry-derived method
    ///   symbol on the user type (`<Type>::<method_name>`).
    /// * Any path that would reach a fabricated fallback emits a
    ///   `CheckerBoundaryViolation` and returns an `Unsupported`
    ///   sentinel — the checker's `require_display_impl` gate is the
    ///   authoritative reject point. Reaching the sentinel means
    ///   compilation halts: never a silent empty-string substitute.
    pub(super) fn lower_display_dispatch(&mut self, value: HirExpr, span: Span) -> HirExpr {
        let dispatch_ty = value.ty.clone();
        self.lower_display_dispatch_for_type(value, dispatch_ty, span)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "display lowering keeps all fail-closed dispatch cases in one authority"
    )]
    pub(super) fn lower_display_dispatch_for_type(
        &mut self,
        value: HirExpr,
        dispatch_ty: ResolvedTy,
        span: Span,
    ) -> HirExpr {
        // Resolve the Display method name through the lang-item registry.
        // Missing entry is fail-closed: f-string lowering cannot synthesise
        // dispatch without a method-name binding.
        let Some(display_binding) = self.lang_items.get(hew_types::LANG_ITEM_DISPLAY_FMT) else {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: "Display::fmt".to_string(),
                    reason: format!(
                        "no lang-item registered for key `{}`",
                        hew_types::LANG_ITEM_DISPLAY_FMT
                    ),
                },
                span.clone(),
                "f-string lowering requires a trait method tagged \
                 `#[lang_item(\"display_fmt\")]` in scope",
            ));
            return self.unsupported_expr(span, "f-string display dispatch: no display lang-item");
        };
        let Some(method_name) = display_binding.method_name.clone() else {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: "Display::fmt".to_string(),
                    reason: "display lang-item is missing its method spelling".to_string(),
                },
                span.clone(),
                "f-string lowering requires a method-level display lang item",
            ));
            return self.unsupported_expr(
                span,
                "f-string display dispatch: malformed display lang-item",
            );
        };
        let Some((display_trait, display_method)) = self.lang_items.display_method_identity()
        else {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: "Display::fmt".to_string(),
                    reason: "display lang-item lacks canonical declaration identities".to_string(),
                },
                span.clone(),
                "f-string lowering requires checker-registered Display declaration identities",
            ));
            return self
                .unsupported_expr(span, "f-string display dispatch: untyped display lang-item");
        };
        let display_target = hew_types::CallTarget::static_trait(display_trait, display_method);
        let ty = dispatch_ty;
        match &ty {
            // String: route through a user `impl Display for string` if one
            // is registered in the user's OWN source (a root-level impl,
            // bare `string::fmt` symbol — see `fstring_string_routes_
            // through_user_display_impl`); otherwise the stdlib's own
            // `impl Display for string` in `std/builtins.hew`, discovered
            // like any imported module's impl (see
            // `insert_builtins_display_module`), so its symbol carries that
            // module prefix, exactly like `duration` below. The stdlib
            // identity impl ordinarily makes this a real (no-op) call so a
            // user impl can transparently replace it; falling through to
            // raw identity below only happens with neither impl registered
            // (zero-stdlib unit tests).
            ResolvedTy::String => {
                let user_symbol = crate::node::HirImplBlock::method_symbol("string", &method_name);
                let stdlib_symbol =
                    crate::node::HirImplBlock::method_symbol("std.builtins.string", &method_name);
                if let Some(call) =
                    self.build_user_fn_call(&user_symbol, vec![value.clone()], span.clone())
                {
                    call
                } else if let Some(call) =
                    self.build_user_fn_call(&stdlib_symbol, vec![value.clone()], span.clone())
                {
                    call
                } else {
                    // No registered impl at all — fall through to identity.
                    value
                }
            }
            ResolvedTy::I8
            | ResolvedTy::I16
            | ResolvedTy::I32
            | ResolvedTy::I64
            | ResolvedTy::U8
            | ResolvedTy::U16
            | ResolvedTy::U32
            | ResolvedTy::U64
            | ResolvedTy::Isize
            | ResolvedTy::Usize
            | ResolvedTy::F32
            | ResolvedTy::F64
            | ResolvedTy::Bool
            | ResolvedTy::Char => self.lower_scalar_display(value, &ty, span),
            // `duration` has a pure-Hew `impl Display for duration` in
            // `std/builtins.hew`, discovered and lowered like any imported
            // module's impl (see `insert_builtins_display_module`), so
            // dispatch to its module-qualified fmt symbol exactly like a
            // user named-type Display impl. The `_` fail-closed arm below
            // would otherwise reject it (checker–HIR contract violation)
            // even though the checker admitted it.
            ResolvedTy::Duration => self.dispatch_display_to_named_impl(
                "std.builtins.duration",
                &[],
                &method_name,
                value,
                span,
            ),
            ResolvedTy::Named {
                builtin: Some(BuiltinType::NodeId),
                ..
            } => self.build_catalog_call("hew_node_id_display", vec![value], span),
            ResolvedTy::Named {
                builtin: Some(BuiltinType::Location),
                ..
            } => self.build_catalog_call("hew_location_display", vec![value], span),
            ResolvedTy::Named {
                builtin: Some(BuiltinType::RemotePid),
                ..
            } => self.build_catalog_call("hew_remote_pid_display", vec![value], span),
            ResolvedTy::Named { name, args, .. } => {
                // An abstract type parameter `T: Display` (the checker lowers
                // `T` to a bare `Named`) defers to per-monomorphisation static
                // dispatch; a concrete user type calls its `impl Display` fmt
                // symbol directly (byte-identical to the pre-#1565 path).
                if self.current_fn_type_params.contains(name) {
                    let type_param_name = name.clone();
                    return self.build_display_static_dispatch(
                        value,
                        display_target,
                        type_param_name,
                        span,
                    );
                }
                let name = name.clone();
                let type_args = args.clone();
                self.dispatch_display_to_named_impl(&name, &type_args, &method_name, value, span)
            }
            ResolvedTy::TypeParam { name } => {
                // Abstract type parameter `T` carrying a `Display` bound — the
                // checker's `require_display_impl` / generic-bound gate already
                // verified it. Defer the concrete `Display::fmt` selection to
                // monomorphisation (#1565); the concrete type is never
                // re-derived here.
                let type_param_name = name.clone();
                self.build_display_static_dispatch(value, display_target, type_param_name, span)
            }
            _ => {
                // Same invariant as the named-type arm: the checker should
                // have rejected this interpolant via `require_display_impl`.
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::CheckerBoundaryViolation {
                        name: format!("Display::{method_name}"),
                        reason: format!("no Display dispatch shape for type `{ty:?}`"),
                    },
                    span.clone(),
                    "checker accepted a Display interpolant of an unsupported type \
                     shape — checker–HIR contract violation",
                ));
                self.unsupported_expr(span, "f-string display dispatch: unsupported type shape")
            }
        }
    }

    pub(super) fn build_structural_format_call(&mut self, value: HirExpr, span: Span) -> HirExpr {
        let fn_ty = ResolvedTy::Function {
            capabilities: hew_types::CallableCapabilities::FUNCTION_ITEM,
            params: vec![value.ty.clone()],
            ret: Box::new(ResolvedTy::String),
        };
        let callee = HirExpr {
            node: self.ids.node(),
            site: self.ids.site(),
            ty: fn_ty,
            intent: IntentKind::Read,
            kind: HirExprKind::Literal(HirLiteral::Unit),
            span: span.clone(),
        };
        HirExpr {
            node: self.ids.node(),
            site: self.ids.site(),
            ty: ResolvedTy::String,
            intent: IntentKind::Read,
            kind: HirExprKind::Call {
                target: hew_types::CallTarget::Runtime(
                    hew_types::runtime_call::RuntimeCallFamily::StructuralFormat,
                ),
                callee: Box::new(callee),
                args: vec![value],
                evaluation_order: Vec::new(),
            },
            span,
        }
    }

    /// Render one scalar through its `to_string_*` catalog builtin, widening
    /// first when the scalar is narrower than the conversion's ABI type
    /// (`f32`/`i8`/`i16`/`u16`/`isize`/`usize`). The runtime exports one entry
    /// per canonical width, so the cast is what lets the narrow widths share
    /// it, and it keeps the argument type equal to the runtime contract's.
    pub(super) fn lower_scalar_display(
        &mut self,
        value: HirExpr,
        ty: &ResolvedTy,
        span: Span,
    ) -> HirExpr {
        let (builtin, abi_ty) = scalar_display_builtin(ty);
        let argument = if *ty == abi_ty {
            value
        } else {
            HirExpr {
                node: self.ids.node(),
                site: self.ids.site(),
                ty: abi_ty.clone(),
                intent: IntentKind::Read,
                kind: HirExprKind::NumericCast {
                    value: Box::new(value),
                    from_ty: ty.clone(),
                    to_ty: abi_ty,
                },
                span: span.clone(),
            }
        };
        self.build_catalog_call(builtin, vec![argument], span)
    }

    /// Dispatch a `Display::fmt` call to a concrete named/builtin type's impl
    /// symbol (`<type>::fmt`).
    ///
    /// Shared by the duration and concrete-named-type arms of
    /// [`Self::lower_display_dispatch`]. The checker's `require_display_impl`
    /// gate guarantees the impl exists; reaching the `else` here means the
    /// symbol is absent from the HIR fn registry — a checker–HIR contract
    /// violation surfaced fail-closed rather than fabricating an empty string.
    pub(super) fn dispatch_display_to_named_impl(
        &mut self,
        type_name: &str,
        type_args: &[ResolvedTy],
        method_name: &str,
        value: HirExpr,
        span: Span,
    ) -> HirExpr {
        let symbol = crate::node::HirImplBlock::method_symbol(type_name, method_name);
        if let Some(call) = self.build_user_fn_call(&symbol, vec![value], span.clone()) {
            self.register_display_impl_monomorphisation(&symbol, type_args, &span, call.site);
            return call;
        }
        self.diagnostics.push(HirDiagnostic::new(
            HirDiagnosticKind::CheckerBoundaryViolation {
                name: symbol.clone(),
                reason: format!("no fn_registry entry for display impl `{symbol}`"),
            },
            span.clone(),
            "checker accepted a Display interpolant but HIR has no \
             corresponding impl symbol — checker–HIR contract violation",
        ));
        self.unsupported_expr(span, format!("display dispatch: missing {symbol}"))
    }

    /// Interpolating a value whose `impl Display` block is generic
    /// (`impl<E> Display for ActorError<E>`) needs the same
    /// per-instantiation monomorphisation an ordinary `value.fmt()` call gets.
    /// The f-string spine synthesises its own call site, so no checker
    /// `call_type_args` entry exists for it; the concrete type's own arguments
    /// are the substitution, taken positionally against the impl block's
    /// declared parameters.
    pub(super) fn register_display_impl_monomorphisation(
        &mut self,
        symbol: &str,
        type_args: &[ResolvedTy],
        span: &Span,
        call_site: SiteId,
    ) {
        let Some(entry) = self.fn_registry.get(symbol) else {
            return;
        };
        if entry.linkage.is_some() || entry.type_params.is_empty() {
            return;
        }
        let origin = entry.id;
        let builtin_family = entry.builtin_family;
        if entry.type_params.len() != type_args.len() {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: symbol.to_string(),
                    reason: format!(
                        "generic Display impl declares {} parameters, interpolated type carries {}",
                        entry.type_params.len(),
                        type_args.len()
                    ),
                },
                span.clone(),
                "a generic Display impl must be parameterised by its own self type's arguments",
            ));
            return;
        }
        let type_args = type_args.to_vec();
        self.call_site_type_args
            .insert(call_site, type_args.clone());
        if builtin_family.is_some() {
            return;
        }
        if type_args
            .iter()
            .any(|ty| self.contains_abstract_type_param(ty))
        {
            return;
        }
        let Some(declaration) = self
            .impl_method_body_symbols
            .iter()
            .chain(self.impl_body_plan.symbols.iter())
            .find_map(|(declaration, emitted)| (emitted == symbol).then(|| declaration.clone()))
        else {
            return;
        };
        let _ = self.mono_registry.insert(MonoKey {
            origin,
            declaration,
            linker_symbol: symbol.to_string(),
            type_args,
        });
    }

    /// #1565: route `println` / `print` / `to_string` of a value whose type
    /// is an abstract type parameter `T: Display` through the Display
    /// static-trait-dispatch spine (`lower_display_dispatch`).
    ///
    /// The concrete-ABI overloads (`println_i64`, `to_string_str`, …) are
    /// resolved earlier by `stdlib_catalog::resolve_overload` and never reach
    /// here; this only fires when that lookup returned `None` because the
    /// single argument is a `ResolvedTy::TypeParam`. `to_string` yields the
    /// rendered string directly; `print` / `println` wrap it in the
    /// `print_str` / `println_str` catalog builtins.
    ///
    /// Returns `Err(args)` (handing the arguments back so the caller can fall
    /// through to the fail-closed `UnresolvedBuiltinOverload`) when the call
    /// is not one of those three builtins over exactly one type-parameter
    /// argument, or when no Display method lang-item is in scope.
    pub(super) fn try_lower_generic_display_builtin(
        &mut self,
        name: &str,
        args: Vec<HirExpr>,
        span: &Span,
    ) -> Result<(HirExprKind, ResolvedTy), Vec<HirExpr>> {
        let is_display_surface = matches!(name, "println" | "print" | "to_string");
        // Route a single `Display` argument through the Display dispatch spine
        // whenever no concrete-ABI overload matched. Reaching this fallback
        // means `stdlib_catalog::resolve_overload` found no monomorphic entry
        // (`println_i32`, `to_string_str`, …) for the argument type, yet the
        // argument is already known to implement `Display`: `println` / `print`
        // / `to_string` are generic `T: Display` builtins, so the checker's
        // type-parameter bound enforcement (`Checker::enforce_type_param_bounds`
        // / `type_satisfies_trait_bound` in `check/generics.rs`) rejects a
        // non-`Display` argument before HIR lowering runs — `println(blob)` on a
        // type with no `impl Display` fails that bound gate with "does not
        // implement trait `Display` required by `T`" and never reaches here.
        // (That is a distinct gate from the f-string-only `require_display_impl`,
        // which validates each interpolation part.) So the argument count is the
        // only condition worth testing: `lower_display_dispatch` already has a
        // working arm for every shape a `Display` value can take — `string`,
        // every scalar (incl. `char` and the narrow ints via
        // `scalar_display_builtin`), `duration`, named-`instant`, identity
        // aggregates, concrete named `impl Display` types, and abstract type
        // parameters `T: Display` — and fails closed on anything else.
        // Enumerating a subset of those shapes here only re-hid the rest behind
        // `UnresolvedBuiltinOverload` (#2351: `char`/`i8`/`f32`; #2492: named
        // types with a real `impl Display`), even though f-string interpolation
        // of the identical value already renders it fine through this shell.
        let single_dispatchable = args.len() == 1;
        if !is_display_surface || !single_dispatchable || self.lang_items.display_method().is_none()
        {
            return Err(args);
        }
        let value = args
            .into_iter()
            .next()
            .expect("single argument checked above");
        let rendered = self.lower_display_dispatch(value, span.clone());
        let lowered = match name {
            "to_string" => (rendered.kind, rendered.ty),
            "print" => {
                let call = self.build_catalog_call("print_str", vec![rendered], span.clone());
                (call.kind, call.ty)
            }
            // `println`
            _ => {
                let call = self.build_catalog_call("println_str", vec![rendered], span.clone());
                (call.kind, call.ty)
            }
        };
        Ok(lowered)
    }

    /// Normalize `assert_eq(a, b)` / `assert_ne(a, b)` into ordinary HIR.
    ///
    /// The checker registers both as generic builtins over one type parameter
    /// `T: Eq + Display` (`registration.rs`), so by the time lowering runs the
    /// operands share a type that carries a selected equality and a renderable
    /// `Display`. Nothing downstream needs an assertion concept: the call
    /// becomes
    ///
    /// ```text
    /// {
    ///     let __hew_assert_left_N  = <left>;
    ///     let __hew_assert_right_N = <right>;
    ///     if __hew_assert_left_N != __hew_assert_right_N {
    ///         panic("assertion failed: left != right\n  left: …\n  right: …");
    ///     }
    /// }
    /// ```
    ///
    /// Binding both operands first is what makes each argument expression
    /// evaluate exactly once even though the comparison and the failure message
    /// each read them. The comparison is the ordinary `!=` / `==` the language
    /// already lowers, so SIR selects the same `Eq` capability it selects for a
    /// hand-written comparison, and the two locals are ordinary owned bindings
    /// released on every exit including the fault path. `panic` reuses the one
    /// logical-fault path `assert` uses.
    #[expect(
        clippy::too_many_lines,
        reason = "the desugar is one shape; splitting it would scatter the bindings it threads"
    )]
    pub(super) fn lower_equality_assertion(
        &mut self,
        name: &str,
        args: Vec<HirExpr>,
        span: &Span,
    ) -> (HirExprKind, ResolvedTy) {
        let expect_equal = name == "assert_eq";
        let Ok([left, right]) = <[HirExpr; 2]>::try_from(args) else {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: name.to_string(),
                    reason: "equality assertion did not receive exactly two operands".to_string(),
                },
                span.clone(),
                "checker must reject an equality assertion with the wrong arity",
            ));
            return (
                HirExprKind::Unsupported(format!("`{name}` requires exactly two operands")),
                ResolvedTy::Unit,
            );
        };
        let operand_ty = left.ty.clone();

        let block_scope = self.ids.scope();
        self.push_scope();
        let left_name = format!("__hew_assert_left_{}", self.ids.binding().0);
        let left_binding = self.bind(left_name.clone(), operand_ty.clone(), false, span.clone());
        let left_id = left_binding.id;
        let right_name = format!("__hew_assert_right_{}", self.ids.binding().0);
        let right_binding = self.bind(right_name.clone(), operand_ty.clone(), false, span.clone());
        let right_id = right_binding.id;
        let mut statements = vec![
            HirStmt {
                node: self.ids.node(),
                kind: HirStmtKind::Let(left_binding, Some(left)),
                span: span.clone(),
            },
            HirStmt {
                node: self.ids.node(),
                kind: HirStmtKind::Let(right_binding, Some(right)),
                span: span.clone(),
            },
        ];

        // The failure condition is the negation of what the assertion claims.
        let condition_op = if expect_equal {
            hew_parser::ast::BinaryOp::NotEqual
        } else {
            hew_parser::ast::BinaryOp::Equal
        };
        let condition_left = self.make_binding_ref(
            left_name.clone(),
            left_id,
            operand_ty.clone(),
            IntentKind::Read,
            span.clone(),
        );
        let condition_right = self.make_binding_ref(
            right_name.clone(),
            right_id,
            operand_ty.clone(),
            IntentKind::Read,
            span.clone(),
        );
        let condition = self.make_expr(
            HirExprKind::Binary {
                op: condition_op,
                left: Box::new(condition_left),
                right: Box::new(condition_right),
            },
            ResolvedTy::Bool,
            IntentKind::Read,
            span.clone(),
        );

        let left_ref = self.make_binding_ref(
            left_name,
            left_id,
            operand_ty.clone(),
            IntentKind::Read,
            span.clone(),
        );
        let right_ref = self.make_binding_ref(
            right_name,
            right_id,
            operand_ty,
            IntentKind::Read,
            span.clone(),
        );
        let message = self.assertion_failure_message(expect_equal, left_ref, right_ref, span);
        let panic_call = self.build_catalog_call("panic", vec![message], span.clone());
        let then_scope = self.ids.scope();
        let then_block = HirExpr {
            node: self.ids.node(),
            site: self.ids.site(),
            ty: ResolvedTy::Never,
            intent: IntentKind::Read,
            kind: HirExprKind::Block(HirBlock {
                node: self.ids.node(),
                scope: then_scope,
                statements: Vec::new(),
                tail: Some(Box::new(panic_call)),
                ty: ResolvedTy::Never,
                span: span.clone(),
            }),
            span: span.clone(),
        };
        let guard = self.make_expr(
            HirExprKind::If {
                condition: Box::new(condition),
                then_expr: Box::new(then_block),
                else_expr: None,
            },
            ResolvedTy::Unit,
            IntentKind::Read,
            span.clone(),
        );
        statements.push(HirStmt {
            node: self.ids.node(),
            kind: HirStmtKind::Expr(guard),
            span: span.clone(),
        });
        self.pop_scope();

        (
            HirExprKind::Block(HirBlock {
                node: self.ids.node(),
                scope: block_scope,
                statements,
                tail: None,
                ty: ResolvedTy::Unit,
                span: span.clone(),
            }),
            ResolvedTy::Unit,
        )
    }

    /// Build the string an equality assertion panics with, rendering both
    /// operands through the same `Display` spine f-string interpolation uses.
    pub(super) fn assertion_failure_message(
        &mut self,
        expect_equal: bool,
        left: HirExpr,
        right: HirExpr,
        span: &Span,
    ) -> HirExpr {
        let claim = if expect_equal {
            "assertion failed: left != right\n  left: "
        } else {
            "assertion failed: left == right\n  left: "
        };
        let mut message = self.build_string_literal_expr(claim.to_string(), span.clone());
        for (separator, operand) in [(None, left), (Some("\n  right: "), right)] {
            if let Some(separator) = separator {
                let literal = self.build_string_literal_expr(separator.to_string(), span.clone());
                message =
                    self.build_catalog_call("string_concat", vec![message, literal], span.clone());
            }
            let rendered = self.lower_display_dispatch(operand, span.clone());
            message =
                self.build_catalog_call("string_concat", vec![message, rendered], span.clone());
        }
        message
    }

    /// Lower an `Expr::InterpolatedString` to a chain of `string_concat` calls
    /// joining literal segments with `Display::fmt(…)` results.  Empty
    /// interpolations collapse to the empty-string literal.  The result type
    /// is always `ResolvedTy::String` so the surrounding lowering wraps it
    /// like any other string-typed expression.
    pub(super) fn lower_interpolated_string(
        &mut self,
        parts: &[StringPart],
        span: Span,
    ) -> (HirExprKind, ResolvedTy) {
        let mut segments: Vec<HirExpr> = Vec::with_capacity(parts.len());
        for part in parts {
            match part {
                StringPart::Literal(text) => {
                    if !text.is_empty() {
                        segments.push(self.build_string_literal_expr(text.clone(), span.clone()));
                    }
                }
                StringPart::Expr((expr, expr_span)) => {
                    let authored =
                        self.lower_expr(&(expr.clone(), expr_span.clone()), IntentKind::Read);

                    let anchor_site = self.ids.site();
                    let value =
                        self.subsumed_value(anchor_site, expr_span, IntentKind::Read, authored);
                    let rendered = self.lower_display_dispatch(value, expr_span.clone());
                    segments.push(rendered);
                }
                StringPart::StructuralExpr((expr, expr_span)) => {
                    let authored =
                        self.lower_expr(&(expr.clone(), expr_span.clone()), IntentKind::Read);

                    let anchor_site = self.ids.site();
                    let value =
                        self.subsumed_value(anchor_site, expr_span, IntentKind::Read, authored);
                    let dispatch_ty = self
                        .interpolation_display_types
                        .get(&self.mk_key(expr_span))
                        .and_then(|ty| ResolvedTy::from_ty(ty).ok());
                    let rendered = if let Some(dispatch_ty) = dispatch_ty {
                        self.lower_display_dispatch_for_type(value, dispatch_ty, expr_span.clone())
                    } else {
                        self.build_structural_format_call(value, expr_span.clone())
                    };
                    segments.push(rendered);
                }
            }
        }
        if segments.is_empty() {
            return (
                HirExprKind::Literal(HirLiteral::String(String::new())),
                ResolvedTy::String,
            );
        }
        let mut iter = segments.into_iter();
        let mut acc = iter.next().expect("non-empty segments by construction");
        for next in iter {
            acc = self.build_catalog_call("string_concat", vec![acc, next], span.clone());
        }
        // The caller re-wraps `(kind, ty)` with its own site, so returning the
        // last segment's kind directly would drop that segment's site along
        // with every side table keyed on it (a generic `Display` impl records
        // its per-instantiation type arguments there). Keep the segment whole
        // inside a transparent subsumed value.
        let ty = acc.ty.clone();
        (
            HirExprKind::SubsumedValue {
                source: Box::new(acc),
            },
            ty,
        )
    }
}
