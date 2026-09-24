//! Split from `expressions.rs`: checker methods, part 1 of 5.
#![allow(
    unused_imports,
    redundant_imports,
    clippy::wildcard_imports,
    reason = "chunk files share the parent module's import header"
)]
use super::super::branch_join::BranchArmExit;
use super::super::coerce::{cast_is_valid, common_integer_type, common_numeric_type};
use super::super::types::GenericLambdaSig;
#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::super::*;
use super::*;
use crate::check::types::{
    DeferredIsCheck, EqRequirement, GenericCallEdge, GenericCallee, GenericFnInstantiationSite,
    PendingInstantiation,
};
use crate::env::{PlaceConflict, PlacePath};
use crate::BuiltinType;
use std::collections::VecDeque;

impl Checker {
    pub(super) fn lambda_generic_schema_ty(
        ty: &Ty,
        generic_param_names: &HashMap<u32, String>,
    ) -> Ty {
        match ty {
            Ty::Var(v) => generic_param_names.get(&v.0).map_or_else(
                || ty.clone(),
                |name| Ty::Named {
                    builtin: None,
                    name: name.clone(),
                    args: vec![],
                },
            ),
            Ty::Named {
                name,
                args,
                builtin,
            } => Ty::Named {
                name: name.clone(),
                builtin: *builtin,
                args: args
                    .iter()
                    .map(|arg| Self::lambda_generic_schema_ty(arg, generic_param_names))
                    .collect(),
            },
            Ty::Tuple(ts) => Ty::Tuple(
                ts.iter()
                    .map(|elem| Self::lambda_generic_schema_ty(elem, generic_param_names))
                    .collect(),
            ),
            Ty::Array(inner, n) => Ty::Array(
                Box::new(Self::lambda_generic_schema_ty(inner, generic_param_names)),
                *n,
            ),
            Ty::Slice(inner) => Ty::Slice(Box::new(Self::lambda_generic_schema_ty(
                inner,
                generic_param_names,
            ))),
            Ty::Pointer {
                is_mutable,
                pointee,
            } => Ty::Pointer {
                is_mutable: *is_mutable,
                pointee: Box::new(Self::lambda_generic_schema_ty(pointee, generic_param_names)),
            },
            Ty::Function {
                capabilities,
                params,
                ret,
            } => Ty::Function {
                capabilities: *capabilities,
                params: params
                    .iter()
                    .map(|param| Self::lambda_generic_schema_ty(param, generic_param_names))
                    .collect(),
                ret: Box::new(Self::lambda_generic_schema_ty(ret, generic_param_names)),
            },
            Ty::Closure {
                capabilities,
                params,
                ret,
                captures,
                identity,
            } => Ty::Closure {
                capabilities: *capabilities,
                params: params
                    .iter()
                    .map(|param| Self::lambda_generic_schema_ty(param, generic_param_names))
                    .collect(),
                ret: Box::new(Self::lambda_generic_schema_ty(ret, generic_param_names)),
                captures: captures
                    .iter()
                    .map(|capture| Self::lambda_generic_schema_ty(capture, generic_param_names))
                    .collect(),
                identity: identity.clone(),
            },
            Ty::TraitObject { traits } => Ty::TraitObject {
                traits: traits
                    .iter()
                    .map(|bound| crate::ty::TraitObjectBound {
                        trait_name: bound.trait_name.clone(),
                        args: bound
                            .args
                            .iter()
                            .map(|arg| Self::lambda_generic_schema_ty(arg, generic_param_names))
                            .collect(),
                        assoc_bindings: bound
                            .assoc_bindings
                            .iter()
                            .map(|(name, ty)| {
                                (
                                    name.clone(),
                                    Self::lambda_generic_schema_ty(ty, generic_param_names),
                                )
                            })
                            .collect(),
                    })
                    .collect(),
            },
            _ => ty.clone(),
        }
    }

    /// Synthesize: infer the type of an expression (bottom-up).
    pub(in crate::check) fn synthesize(&mut self, expr: &Expr, span: &Span) -> Ty {
        if self.deferred_body.is_some()
            && matches!(
                expr,
                Expr::ReturnError(_)
                    | Expr::PostfixTry(_)
                    | Expr::Await(_)
                    | Expr::AwaitRestart(_)
                    | Expr::Yield(_)
                    | Expr::ScopeDeadline { .. }
                    | Expr::ForkChild { .. }
                    | Expr::ForkBlock { .. }
                    | Expr::Select { .. }
                    | Expr::Race(_)
            )
        {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                "a deferred body cannot suspend or propagate an error out of its scope".to_string(),
            );
            return Ty::Error;
        }
        // Synthesis runs without an expected type, so no expression reached
        // through `synthesize` is in `check_against` tail position. Clear the
        // tail Ok-coercion flag for the duration so a nested expression (an
        // operand, argument, or non-tail statement) can never trip the
        // coercion. The flag is only meaningful on the `check_against` path.
        let prev_tail_ok_armed = std::mem::replace(&mut self.tail_ok_armed, false);
        // Grow the stack on demand so deeply-nested expressions (e.g. 1000+
        // chained binary operators) don't overflow.
        let result = stacker::maybe_grow(32 * 1024, 2 * 1024 * 1024, || {
            self.synthesize_inner(expr, span)
        });
        self.tail_ok_armed = prev_tail_ok_armed;
        self.publish_checked_expression(expr, span, result)
    }

    pub(in crate::check) fn reject_if_wasm_incompatible_expr(&mut self, expr: &Expr, span: &Span) {
        if !self.wasm_target {
            return;
        }
        match expr {
            Expr::Scope { .. } | Expr::ScopeDeadline { .. } | Expr::Race(_) => {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::StructuredConcurrency);
            }
            Expr::ForkChild { .. } | Expr::ForkBlock { .. } => {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::Tasks);
            }
            _ => {}
        }
    }

    pub(in crate::check) fn display_impl_type(&mut self, ty: &Ty) -> Option<Ty> {
        let resolved = self.subst.resolve(ty).materialize_literal_defaults();
        if matches!(resolved, Ty::String) {
            return Some(resolved);
        }
        if matches!(resolved, Ty::Var(_) | Ty::Error) {
            return None;
        }
        // `instant` is a monotonic timestamp that canonicalises to a bare i64 at
        // the MIR boundary; HIR's Display dispatch routes it through the i64
        // catalog arm (raw-nanos rendering), so it is Display-able without a
        // dedicated `impl Display for instant` body. A monotonic timestamp has
        // no wall-clock meaning, so raw nanos is the honest rendering.
        if resolved.is_instant() {
            return Some(resolved);
        }
        // These compiler carriers have a closed Display ABI selected by their
        // builtin discriminator in HIR/codegen (`hew_*_display`), with the
        // shipped `std.builtins` impl supplying the source-level contract.
        // The carrier representation deliberately stays canonical rather than
        // inheriting a `std.builtins.*` nominal name, so a generic nominal-impl
        // lookup alone cannot prove the implementation.  This is the same
        // typed identity boundary used by f-string lowering, not a leaf-name
        // exception; a user `NodeId` remains `builtin: None` and reaches the
        // ordinary trait lookup below.
        if matches!(
            resolved,
            Ty::Named {
                builtin: Some(
                    crate::BuiltinType::NodeId
                        | crate::BuiltinType::Location
                        | crate::BuiltinType::RemotePid
                ),
                ..
            }
        ) {
            return Some(resolved);
        }
        // Resolve the Display trait name through the lang-item registry.
        // No `#[lang_item("display")]` in scope means the program defines no
        // Display trait at all — in which case f-string interpolation can
        // only accept the trivially-string / inference-pending cases handled
        // above. Falling back to the literal name `"Display"` keeps
        // pre-lang-item check-time tests (no stdlib loaded) working with the
        // implicit naming convention.
        let (_display_trait, display_trait_key) =
            self.lang_items.get(crate::LANG_ITEM_DISPLAY).map_or_else(
                || ("Display".to_string(), "Display".to_string()),
                |binding| {
                    (
                        binding.trait_name.clone(),
                        binding.trait_id.full_path().to_string(),
                    )
                },
            );
        if let Some(canonical) = resolved.canonical_lowering_name() {
            if self
                .primitive_trait_impls
                .contains_key(&(canonical.to_string(), display_trait_key.clone()))
            {
                return Some(resolved);
            }
        }
        if let Ty::Named { name, args, .. } = &resolved {
            if self.type_implements_trait_for_ty(&resolved, &display_trait_key) {
                return Some(resolved);
            }
            // A bare type parameter (e.g. `T` in `fn f<T: Display>(x: T)`)
            // carries no registered impl of its own, but the enclosing
            // item's where-clause may declare a `Display` bound that
            // satisfies the obligation abstractly. The concrete `Display`
            // impl is selected per monomorphisation by HIR's static
            // trait-dispatch lowering. Mirrors `type_satisfies_trait_bound`.
            if args.is_empty() && self.type_param_carries_bound(name, &display_trait_key) {
                return Some(resolved);
            }
        }
        None
    }

    /// Whether `ty` has a structural rendering: a value `f"{v:?}"` can spell
    /// from its own parts.
    ///
    /// A pending inference variable defers - the surrounding inference
    /// reports its own error, and a resolved type reaches physical MIR, which
    /// verifies the recipe it builds. A user declaration renders through its
    /// declared fields; a compiler carrier renders only when its builtin
    /// identity says it has structure.
    pub(super) fn renders_structurally(&mut self, ty: &Ty) -> bool {
        match self.subst.resolve(ty).materialize_literal_defaults() {
            Ty::Var(_)
            | Ty::Error
            | Ty::I8
            | Ty::I16
            | Ty::I32
            | Ty::I64
            | Ty::U8
            | Ty::U16
            | Ty::U32
            | Ty::U64
            | Ty::Isize
            | Ty::Usize
            | Ty::F32
            | Ty::F64
            | Ty::IntLiteral
            | Ty::FloatLiteral
            | Ty::Bool
            | Ty::Char
            | Ty::String
            | Ty::Unit => true,
            Ty::Tuple(members) => members
                .iter()
                .all(|member| self.renders_structurally(member)),
            Ty::Named { args, builtin, .. } => {
                builtin.is_none_or(BuiltinType::renders_structurally)
                    && args.iter().all(|arg| self.renders_structurally(arg))
            }
            _ => false,
        }
    }

    /// Verify that `ty` renders under `:?`.
    ///
    /// `f"{v:?}"` reaches here only when `v` has no `Display` impl to defer
    /// to, so this is the structural half of the same admission.
    pub(in crate::check) fn require_structural_render(&mut self, ty: &Ty, span: &Span) {
        if self.renders_structurally(ty) {
            return;
        }
        let rendered = self
            .subst
            .resolve(ty)
            .materialize_literal_defaults()
            .user_facing()
            .to_string();
        self.report_error(
            TypeErrorKind::BoundsNotSatisfied,
            span,
            format!(
                "type `{rendered}` has no structural rendering (`:?` renders \
scalars, strings, tuples, records, enums, `Vec` and `HashMap`; anything \
else needs `impl Display for {rendered}`)"
            ),
        );
    }

    /// Verify that `ty` has a `Display` impl reachable by f-string
    /// interpolation lowering.
    pub(in crate::check) fn require_display_impl(&mut self, ty: &Ty, span: &Span) {
        if matches!(self.subst.resolve(ty), Ty::Var(_) | Ty::Error) {
            return;
        }
        if self.display_impl_type(ty).is_some() {
            return;
        }
        let resolved = self.subst.resolve(ty).materialize_literal_defaults();
        let display_trait = self.lang_items.get(crate::LANG_ITEM_DISPLAY).map_or_else(
            || "Display".to_string(),
            |binding| binding.trait_name.clone(),
        );
        let ty_str = format!("{}", resolved.user_facing());
        self.report_error(
            TypeErrorKind::BoundsNotSatisfied,
            span,
            format!(
                "type `{ty_str}` does not implement `{display_trait}` \
                 (f-string interpolation requires `impl {display_trait} for {ty_str}`)"
            ),
        );
    }

    #[expect(
        clippy::too_many_lines,
        reason = "expression check covers all AST variants"
    )]
    pub(in crate::check) fn synthesize_inner(&mut self, expr: &Expr, span: &Span) -> Ty {
        self.reject_if_wasm_incompatible_expr(expr, span);
        let ty = match expr {
            // Literals
            Expr::Literal(Literal::Float(_)) => Ty::FloatLiteral,
            Expr::Literal(Literal::String(_)) => Ty::String,
            Expr::RegexLiteral(pattern) => {
                // The implicit `use std::text::regex` injected by the CLI is the
                // provider of this type; mark it as used so the unused-import
                // check doesn't fire a false-positive warning.
                self.used_modules.borrow_mut().insert(ImportKey::in_file(
                    self.current_module.clone(),
                    self.current_module_idx,
                    "regex",
                ));
                // Validate the pattern using the same regex engine the runtime
                // uses. An invalid pattern is a compile-time hard error.
                if let Err(err) = regex::Regex::new(pattern) {
                    self.report_error(
                        TypeErrorKind::InvalidRegexLiteral {
                            pattern: pattern.clone(),
                            error: err.to_string(),
                        },
                        span,
                        format!("invalid regex literal `re\"{pattern}\"`: {err}"),
                    );
                }
                Ty::Named {
                    builtin: None,
                    name: "std.text.regex.Pattern".to_string(),
                    args: vec![],
                }
            }
            Expr::ByteStringLiteral(_) | Expr::ByteArrayLiteral(_) => Ty::Bytes,
            Expr::InterpolatedString(parts) => {
                for part in parts {
                    match part {
                        StringPart::Literal(_) => {}
                        StringPart::Expr((expr, expr_span)) => {
                            let part_ty = self.synthesize(expr, expr_span);
                            self.require_display_impl(&part_ty, expr_span);
                        }
                        StringPart::StructuralExpr((expr, expr_span)) => {
                            let part_ty = self.synthesize(expr, expr_span);
                            if let Some(display_ty) = self.display_impl_type(&part_ty) {
                                self.interpolation_display_types.insert(
                                    SpanKey::in_module(expr_span, self.current_module_idx),
                                    display_ty,
                                );
                            } else {
                                self.require_structural_render(&part_ty, expr_span);
                            }
                        }
                    }
                }
                Ty::String
            }
            Expr::Literal(Literal::Bool(_)) => Ty::Bool,
            Expr::Literal(Literal::Char(_)) => Ty::Char,
            Expr::Literal(Literal::Integer { .. }) => Ty::IntLiteral,
            Expr::Literal(Literal::Duration(_)) => Ty::Duration,

            // Builtin `None`: synthesize `Option<fresh>` and fall through to the
            // universal `record_type` tail (line ~505) like every other arm.
            // The fresh tyvar is resolved by the surrounding context (fn return,
            // let-binding, match scrutinee) during unification; the
            // `check_program` boundary resolve (mod.rs:262/364) then writes back
            // the post-substitution concrete `Option<T>` at this span. Recording
            // here — instead of the old early `return`, which bypassed
            // `record_type` and left HIR's unit-ctor fallback to stamp a bare
            // `Named{Option, args:[]}` (→ codegen D10) — converges builtin `None`
            // onto the same record-and-resolve substrate as the user-`TypeDecl`
            // unit-variant path (`check_against`, expressions.rs:2453). A
            // genuinely-unconstrained `None` still fails closed: the recorded
            // `Option<Var>` stays unresolved and `validate_expr_output_contract`
            // (admissibility.rs) surfaces it as an inference error. See W4.042.
            Expr::Identifier(name) if name == "None" => {
                self.report_bare_variant_expr(name, "Option.None", span);
                Ty::option(Ty::Var(TypeVar::fresh()))
            }
            Expr::Identifier(name) => self.synthesize_identifier(name, span),
            Expr::ContextVariant(context) => {
                if let Some(record) = &context.record {
                    for (_, value) in &record.fields {
                        self.synthesize(&value.0, &value.1);
                    }
                    if let Some(base) = &record.base {
                        self.synthesize(&base.0, &base.1);
                    }
                }
                self.report_error(
                    TypeErrorKind::ContextVariantNoType,
                    span,
                    format!(
                        "E_CONTEXT_VARIANT_NO_TYPE: contextual variant `.{}` requires an expected enum or machine type",
                        context.name
                    ),
                );
                Ty::Error
            }
            Expr::GenericApplySuffix { target, type_args } => match &target.0 {
                Expr::Identifier(name) => {
                    self.synthesize_identifier_with_type_args(name, Some(type_args), span)
                }
                Expr::FieldAccess { object, field } => {
                    self.check_field_access_with_type_args(object, field, Some(type_args), span)
                }
                _ => {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        "explicit type arguments require a function declaration".to_string(),
                    );
                    Ty::Error
                }
            },
            Expr::RecordInitSuffix {
                target,
                fields,
                base,
            } => {
                let target_ty = self.synthesize(&target.0, &target.1);
                for (_, (value, value_span)) in fields {
                    self.synthesize(value, value_span);
                }
                if let Some(base) = base {
                    self.synthesize(&base.0, &base.1);
                }
                target_ty
            }
            Expr::QualifiedAssoc(path) => self.synthesize_qualified_assoc(path, span),

            // Binary ops
            Expr::Binary { left, op, right } => self.check_binary_op(left, *op, right, span),

            // Unary ops
            Expr::Unary { op, operand } => self.synthesize_unary_op(*op, operand, span),

            // `clone <operand>` — explicit duplication. Resolved exactly like
            // `<operand>.clone()`: method resolution decides cloneability and
            // the result type (checker authority), the operand is read
            // non-consumingly, and the same side tables are recorded at this
            // span so HIR lowering can reuse the `.clone()` lowering path.
            // Types with no clone path fail closed downstream with the existing
            // clone diagnostic.
            Expr::Clone(operand) => self.check_method_call(operand, "clone", &[], span),

            // Call
            Expr::Call {
                function,
                type_args,
                args,
                is_tail_call: _,
            } => {
                let ty = self.check_call(function, type_args.as_deref(), args, span);
                self.finish_named_arguments(args, || Self::callee_label(function), &ty, span);
                ty
            }

            // Method call
            Expr::MethodCall {
                receiver,
                method,
                args,
            } => {
                let ty = self.check_method_call(receiver, method, args, span);
                self.finish_named_arguments(args, || format!("method `{method}`"), &ty, span);
                ty
            }

            // Field access
            Expr::FieldAccess { object, field } => self.check_field_access(object, field, span),

            // Block
            Expr::Block(block) => self.check_block(block, None),

            // If expression
            Expr::If {
                condition,
                then_block,
                else_block,
            } => {
                self.check_against(&condition.0, &condition.1, &Ty::Bool);
                let entry = self.env.ownership_snapshot();
                let then_ty = self.synthesize(&then_block.0, &then_block.1);
                let then_exit = BranchArmExit {
                    ownership: self.env.ownership_snapshot(),
                    diverges: Self::arm_skips_join(&then_ty),
                };
                if let Some(eb) = else_block {
                    self.env.restore_ownership(&entry);
                    let else_ty = self.synthesize(&eb.0, &eb.1);
                    let else_exit = BranchArmExit {
                        ownership: self.env.ownership_snapshot(),
                        diverges: Self::arm_skips_join(&else_ty),
                    };
                    self.join_branch_ownership(&entry, &[then_exit, else_exit]);
                    self.unify_branches(&then_ty, &else_ty, span)
                } else {
                    // No `else`: the implicit fall-through arm runs with the
                    // state the condition left behind and never consumes.
                    self.join_fall_through(&entry, then_exit);
                    Ty::Unit
                }
            }
            Expr::IfLet {
                conditions,
                body,
                else_body,
            } => self.synthesize_iflet(conditions, body, else_body.as_deref(), span),

            // Match
            Expr::Match { scrutinee, arms } => {
                let scr_ty = self.synthesize(&scrutinee.0, &scrutinee.1);
                self.check_match_expr(&scr_ty, scrutinee, arms, span, None)
            }

            // Tuple
            Expr::Tuple(elems) => {
                if elems.is_empty() {
                    Ty::Unit
                } else {
                    let tys: Vec<_> = elems
                        .iter()
                        .map(|(e, s)| {
                            let ty = self.synthesize(e, s);
                            self.record_value_transfer(e, s);
                            ty
                        })
                        .collect();
                    Ty::Tuple(tys)
                }
            }

            // Array
            Expr::Array(elems) => self.synthesize_array_literal(elems, span),
            Expr::ArrayRepeat { value, count } => self.synthesize_array_repeat(value, count, span),

            Expr::MapLiteral { entries } => self.synthesize_map_literal(entries, span),

            // Struct init
            Expr::StructInit {
                name,
                fields,
                type_args,
                base,
            } => self.check_struct_init(name, fields, type_args.as_deref(), base.as_deref(), span),

            // Spawn
            Expr::Spawn {
                target,
                type_args,
                args,
            } => self.check_spawn(target, type_args, args, span),

            // Lambda (synthesize mode — no expected type)
            Expr::Lambda {
                is_move,
                private_captures,
                type_params,
                params,
                return_type,
                body,
                ..
            } => self.check_lambda(
                *is_move,
                private_captures,
                type_params.as_deref(),
                params,
                return_type.as_ref(),
                body,
                None,
                span,
                false,
                false,
            ),

            // Await
            Expr::Await(inner) => {
                // Locate a directly awaited method through a transparent block
                // so suspension permission belongs to the call's exact span.
                let (effective_expr, effective_span) = match &inner.0 {
                    Expr::Block(block)
                        if block.stmts.is_empty()
                            && block
                                .trailing_expr
                                .as_deref()
                                .is_some_and(|(e, _)| matches!(e, Expr::MethodCall { .. })) =>
                    {
                        let trailing = block.trailing_expr.as_deref().unwrap();
                        (&trailing.0, &trailing.1)
                    }
                    _ => (&inner.0, &inner.1),
                };

                self.suspension_operands
                    .insert(SpanKey::in_module(effective_span, self.current_module_idx));
                let inner_ty = self.synthesize(&inner.0, &inner.1);

                // Join one Task layer; `await` joins tasks and nothing else. A
                // `Vec<Task<T>>` joins every task in order and yields `Vec<T>`;
                // the vector and each handle in it are consumed by the join.
                match inner_ty {
                    Ty::Task(output) => {
                        if !self.reject_borrowed_consumption(&inner.0, &inner.1) {
                            self.mark_expr_moved(&inner.0, &inner.1);
                        }
                        *output
                    }
                    ref vector if vec_task_output(vector).is_some() => {
                        let output = vec_task_output(vector).expect("matched a vector of tasks");
                        if !self.reject_borrowed_consumption(&inner.0, &inner.1) {
                            self.mark_expr_moved(&inner.0, &inner.1);
                        }
                        self.make_vec_type(output, span)
                    }
                    other => {
                        self.check_await_operand(effective_expr, effective_span, &other);
                        other
                    }
                }
            }

            // AwaitRestart: `await_restart <supervised-child>` — suspend until the
            // named slot is Live again, then resume with the same stable
            // `ChildRef<ChildType>`. The operand names one slot: a static child
            // accessor (recorded in `supervisor_child_slots`, kind `Static`) or
            // one pool member (`sup.pool[i]`, recorded in `pool_accessor_sites`
            // as `Index`). A whole pool names many slots and has no single
            // restart signal, so it is refused. The result type is the same
            // `ChildRef<ChildType>` — by construction the slot is Live after a
            // completed restart; a permanently-Dead child fails closed at
            // runtime (resumes immediately) rather than hanging, so the bare
            // form never yields an `Option`.
            Expr::AwaitRestart(inner) => {
                // Synthesize the operand first; this records the supervisor child
                // slot and pool accessor side-table entries keyed by the inner
                // expression's span.
                let inner_ty = self.synthesize(&inner.0, &inner.1);
                let inner_key = SpanKey::in_module(&inner.1, self.current_module_idx);
                let member = matches!(
                    self.pool_accessor_sites
                        .get(&inner_key)
                        .map(|accessor| accessor.kind),
                    Some(crate::check::types::PoolAccessorKind::Index)
                );
                if member {
                    // One pool member's own slot: the same `ChildRef<ChildType>`
                    // the indexed accessor produced.
                    inner_ty
                } else {
                    match self.supervisor_child_slots.get(&inner_key).cloned() {
                        Some(slot) if slot.kind == crate::check::types::ChildKind::Static => {
                            // Stable role handle: same `ChildRef<ChildType>` the
                            // accessor produced. Carry the discriminator forward — the
                            // side-table entry already keys MIR lowering on this span.
                            inner_ty
                        }
                        Some(_pool_slot) => {
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                span,
                                "`await_restart` waits on one supervised slot; a pool \
                                 names many, so wait on a member with \
                                 `await_restart sup.pool[i]`"
                                    .to_string(),
                            );
                            Ty::Error
                        }
                        None => {
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                span,
                                "`await_restart` expects a supervised-child accessor \
                                 (`await_restart sup.child` or `await_restart \
                                 sup.pool[i]`); its operand is not a supervisor \
                                 child slot"
                                    .to_string(),
                            );
                            Ty::Error
                        }
                    }
                }
            }

            // PostfixTry: expr? → unwrap Result/Option
            Expr::PostfixTry(inner) => {
                let ty = self.synthesize(&inner.0, &inner.1);
                let ty = self.subst.resolve(&ty);
                // Build an error message if the enclosing function's return type
                // cannot propagate via `?`.  Computed before any mutable borrow.
                //
                // JUSTIFIED: Ty::Var(_) is bypassed because the return type is
                // still being inferred — reporting a context error would be a false
                // positive.  Ty::Error is bypassed for the same reason: it means the
                // return-type annotation failed to resolve (e.g. references an
                // unknown type), so we cannot know whether `?` propagation would be
                // valid.  The annotation-resolution error is already reported
                // separately; adding a second "? cannot be used here" error would be
                // confusing rather than helpful.  Inner-type errors ("`?` requires
                // Result or Option, found X`") are reported unconditionally via the
                // else branch below and are NOT affected by this bypass.
                //
                // Ty::Named where the name is not builtin and not in type_defs or
                // type_aliases is also bypassed: this arises when a return-type
                // annotation references an undefined type (resolution falls through
                // to Ty::normalize_named rather than returning Ty::Error). Emitting
                // the context error in this case is a false positive — we cannot
                // know whether the intended type would have been a Result/Option.
                let current_return_type = self.current_return_type.clone();
                let bad_ctx_msg: Option<String> = current_return_type.as_ref().and_then(|ret| {
                    let r = self.subst.resolve(ret);
                    if (r.as_option().is_some() && ty.as_option().is_some())
                        || (r.as_result().is_some() && ty.as_result().is_some())
                        || matches!(r, Ty::Var(_) | Ty::Error)
                        || matches!(&r, Ty::Named { name, .. }
                                if !Ty::is_named_builtin(name)
                                    && !self.type_defs.contains_key(name)
                                    && !self.type_aliases.contains_key(name))
                    {
                        None
                    } else {
                        Some(format!(
                            "`?` cannot be used in a function returning `{r}` to propagate `{ty}`; \
                             absence requires an Option return and errors require a Result return"
                        ))
                    }
                });
                if let Some(inner_ty) = ty.as_option() {
                    if let Some(msg) = bad_ctx_msg {
                        self.report_error(TypeErrorKind::InvalidOperation, span, msg);
                        Ty::Error
                    } else {
                        inner_ty.clone()
                    }
                } else if let Some((ok, err)) = ty.as_result() {
                    let ok_ty = ok.clone();
                    let err_ty = err.clone();
                    if let Some(msg) = bad_ctx_msg {
                        self.report_error(TypeErrorKind::InvalidOperation, span, msg);
                        Ty::Error
                    } else {
                        if let Some(ret) = current_return_type.as_ref() {
                            let resolved_ret = self.subst.resolve(ret);
                            if let Some((_, ret_err)) = resolved_ret.as_result() {
                                let ret_err = ret_err.clone();
                                let err_ty = self.subst.resolve(&err_ty);
                                if !matches!(ret_err, Ty::Error) && !matches!(err_ty, Ty::Error) {
                                    let snapshot = self.subst.snapshot();
                                    if !self.try_unify_with_owner_identity(&ret_err, &err_ty) {
                                        self.subst.restore(snapshot);
                                        self.report_error(
                                            TypeErrorKind::InvalidOperation,
                                            span,
                                            format!(
                                                "`?` error type mismatch: expected `{}`, found `{}`",
                                                ret_err.user_facing(),
                                                err_ty.user_facing()
                                            ),
                                        );
                                        return Ty::Error;
                                    }
                                }
                            }
                        }
                        ok_ty
                    }
                } else {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!(
                            "`?` requires Result or Option, found `{}`",
                            ty.user_facing()
                        ),
                    );
                    Ty::Error
                }
            }

            Expr::Coalesce { left, right } => self.check_local_recovery(left, right, None, span),
            Expr::Handle {
                operand,
                error,
                body,
            } => self.check_local_recovery(operand, body, Some(error), span),

            // Yield
            Expr::Yield(value) => self.synthesize_yield(value.as_deref(), span),

            // `return [expr]` in expression position. A `return` diverges, so
            // the construct itself synthesizes to `Ty::Never` (which unifies
            // with any expected type). The operand is checked against the
            // enclosing function's declared return type via the SAME shared
            // shell as statement-position `Stmt::Return`
            // (LESSONS `one-construct-one-lowering-shell`) — never against this
            // expression's expected type, so a mismatched `return` operand is
            // attributed to the return, not the surrounding expression.
            Expr::Return(value) => {
                self.check_return_operand(value.as_deref(), span);
                Ty::Never
            }
            Expr::ReturnError(value) => {
                let error = self.current_return_type.as_ref().and_then(|ty| {
                    self.subst
                        .resolve(ty)
                        .as_result()
                        .map(|(_, error)| error.clone())
                });
                if let Some(error) = error.filter(|_| self.current_fails) {
                    self.check_against(&value.0, &value.1, &error);
                    self.result_return_coercions.insert(
                        SpanKey::in_module(span, self.current_module_idx),
                        super::ResultReturnKind::Error,
                    );
                } else {
                    self.synthesize(&value.0, &value.1);
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        "`return error` requires an enclosing function declared with `fails`"
                            .to_string(),
                    );
                }
                self.recheck_return_edge_defers();
                Ty::Never
            }

            // Index
            Expr::Index { object, index } => {
                self.synthesize_index(object, index, span, IndexContext::Read)
            }

            // Range
            Expr::Range {
                start,
                end,
                inclusive: _,
            } => self.synthesize_range(start.as_deref(), end.as_deref()),

            // Cast expression: `expr as Type`
            Expr::Cast {
                expr: inner,
                ty: type_expr,
            } => self.synthesize_cast(inner, type_expr, span),

            // Identity comparison: `lhs is rhs` (slice D-2).
            //
            // Allowed receivers: actors/actor refs and heap-backed
            // `Vec`/`HashMap`/`HashSet`/`bytes`.
            // Rejected with `E_IS_VALUE_TYPE`: scalars, `String`, `type
            // Foo { ... }` record declarations, `record` types, enum
            // declarations (`indirect` included), machines, tuples,
            // ranges, fn/closures.
            //
            // Result is always `bool`. Cross-class mismatches (e.g.
            // `<actor handle> is Vec<int>`) collapse into a single
            // `TypeErrorKind::Mismatch` diagnostic that requires the operands
            // share the same resolved type. Move/consumed-self semantics
            // follow the existing use-after-move rule (plan §D-D4, Q-N3).
            Expr::Is { lhs, rhs } => self.synthesize_is(lhs, rhs, span),

            _ => self.synthesize_concurrency(expr, span),
        };

        self.record_type(span, &ty);
        ty
    }

    pub(in crate::check) fn synthesize_unary_op(
        &mut self,
        op: UnaryOp,
        operand: &Spanned<Expr>,
        span: &Span,
    ) -> Ty {
        match op {
            UnaryOp::Not => {
                self.check_against(&operand.0, &operand.1, &Ty::Bool);
                Ty::Bool
            }
            UnaryOp::Negate => {
                let ty = self.synthesize(&operand.0, &operand.1);
                let resolved = self.subst.resolve(&ty);
                if !resolved.is_numeric()
                    && !resolved.is_duration()
                    && !matches!(resolved, Ty::Var(_))
                    && resolved != Ty::Error
                {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!("cannot negate type `{}`", resolved.user_facing()),
                    );
                }
                ty
            }
            UnaryOp::BitNot => {
                let ty = self.synthesize(&operand.0, &operand.1);
                let resolved = self.subst.resolve(&ty);
                if !resolved.is_integer()
                    && !matches!(resolved, Ty::Var(_))
                    && resolved != Ty::Error
                {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!(
                            "bitwise NOT requires integer type, found `{}`",
                            resolved.user_facing()
                        ),
                    );
                }
                ty
            }
            UnaryOp::RawDeref => {
                // Raw-pointer dereference is fail-closed.
                //
                // - Outside `unsafe { ... }` we emit
                //   `UnsafeOperationRequiresBlock` so the user is told to
                //   wrap the operation, matching the diagnostic that
                //   extern fn calls already produce.
                // - Inside `unsafe { ... }` we still reject with
                //   `RawPointerOpNotLowered` because the compiler has no
                //   HIR/MIR/codegen lowering for raw-pointer operations.
                //   Envelope code: `E_M5_RAW_POINTER_OP_NOT_LOWERED`.
                //
                // We still synthesize the operand so a malformed
                // sub-expression still produces a useful diagnostic.
                let _ = self.synthesize(&operand.0, &operand.1);
                if self.in_unsafe {
                    self.report_error(
                        TypeErrorKind::RawPointerOpNotLowered {
                            operation: "raw pointer dereference".to_string(),
                        },
                        span,
                        "raw pointer dereference is not lowered to HIR/MIR/codegen".to_string(),
                    );
                } else {
                    self.report_error(
                        TypeErrorKind::UnsafeOperationRequiresBlock {
                            operation: "raw pointer dereference".to_string(),
                        },
                        span,
                        "raw pointer dereference requires an `unsafe { ... }` block".to_string(),
                    );
                }
                Ty::Error
            }
        }
    }

    pub(super) fn expect_concrete_integer_operands(
        &mut self,
        common_ty: &Ty,
        left: &Spanned<Expr>,
        left_ty: &Ty,
        right: &Spanned<Expr>,
        right_ty: &Ty,
    ) {
        if integer_type_info(common_ty, self.pointer_width()).is_some() {
            // Preserve the source types and publish explicit widening targets
            // for HIR. Literal and inference operands use contextual unification.
            self.record_concrete_integer_operand(common_ty, left, left_ty);
            self.record_concrete_integer_operand(common_ty, right, right_ty);
        }
    }

    pub(super) fn record_concrete_integer_operand(
        &mut self,
        common_ty: &Ty,
        operand: &Spanned<Expr>,
        operand_ty: &Ty,
    ) {
        let resolved = self.subst.resolve(operand_ty);
        if resolved.is_integer() && !resolved.is_integer_literal() && resolved != *common_ty {
            self.numeric_operand_coercions.insert(
                SpanKey::in_module(&operand.1, self.current_module_idx),
                common_ty.clone(),
            );
        }
        if resolved.is_integer_literal() || matches!(resolved, Ty::Var(_)) {
            if self.is_coercible_numeric(&operand.0)
                || (resolved.is_integer_literal()
                    && Self::is_literal_integer_arithmetic(&operand.0))
            {
                self.check_against(&operand.0, &operand.1, common_ty);
            } else {
                // The operand has already been checked. Rechecking an await
                // or call would repeat its ownership effects.
                self.promote_literal_binding(operand_ty, common_ty);
                self.expect_type(common_ty, operand_ty, &operand.1);
                self.record_type(&operand.1, common_ty);
            }
        }
    }

    /// Give a binding whose type is still a defaulting integer literal the
    /// concrete width its arithmetic requires.
    ///
    /// `expect_type` cannot do this: `IntLiteral` already unifies with every
    /// integer type, so the variable keeps the literal kind, the operand site
    /// alone records the narrower width, and the declaration exports the
    /// `i64` default. HIR then reads an `i64` binding under an `i32`
    /// expression, which no later stage can reconcile. Promoting the variable
    /// keeps the declaration and every reference on one type.
    pub(super) fn promote_literal_binding(&mut self, operand_ty: &Ty, common_ty: &Ty) {
        let Ty::Var(var) = operand_ty else {
            return;
        };
        if !common_ty.is_integer() || common_ty.is_integer_literal() {
            return;
        }
        if !self.subst.resolve(&Ty::Var(*var)).is_integer_literal() {
            return;
        }
        self.subst.insert(*var, common_ty).expect(
            "promoting a literal-defaulting binding to a concrete integer width stays acyclic",
        );
    }

    /// Integer arithmetic whose own checked type is still a literal type, so
    /// every leaf under it is a literal or an untyped const.
    ///
    /// Recording only the top node's contextual width would leave those leaves
    /// to default independently (`a == 0 - 1` with `a: i32` recorded the
    /// subtraction as `i32` while both literals defaulted to `i64`, which SIR
    /// then rejected as a mismatched checked-arithmetic terminator). Rechecking
    /// such a subtree against the contextual width repeats no ownership effect
    /// because it contains no call, await or resource use.
    pub(super) fn is_literal_integer_arithmetic(expr: &Expr) -> bool {
        match expr {
            Expr::Binary { op, .. } => matches!(
                op,
                BinaryOp::Add
                    | BinaryOp::Subtract
                    | BinaryOp::Multiply
                    | BinaryOp::Divide
                    | BinaryOp::Modulo
                    | BinaryOp::WrappingAdd
                    | BinaryOp::WrappingSub
                    | BinaryOp::WrappingMul
                    | BinaryOp::BitAnd
                    | BinaryOp::BitOr
                    | BinaryOp::BitXor
                    | BinaryOp::Shl
                    | BinaryOp::Shr
            ),
            Expr::Unary {
                op: UnaryOp::BitNot | UnaryOp::Negate,
                ..
            } => true,
            _ => false,
        }
    }

    pub(super) fn concrete_integer_float_mismatch(left: &Ty, right: &Ty, ptr_width: u8) -> bool {
        (integer_type_info(left, ptr_width).is_some()
            && right.is_float()
            && !right.is_float_literal())
            || (integer_type_info(right, ptr_width).is_some()
                && left.is_float()
                && !left.is_float_literal())
    }

    pub(super) fn expect_inferable_literal_binding(
        &mut self,
        name: &str,
        expected: &Ty,
        span: &Span,
    ) {
        let Some(binding) = self.env.lookup_ref(name) else {
            return;
        };
        if binding.def_span.is_none() {
            return;
        }
        let actual = binding.ty.clone();
        // Keep the binding variable intact for unification. `expect_type`
        // normalizes first, which resolves this `Var` to `IntLiteral` and
        // loses the root that `unify` must promote to the concrete contextual
        // width. This is the use-site inference path for `let n = 7; f(n)`.
        if self.subst.resolve(&actual).is_numeric_literal()
            && expected.is_numeric()
            && self.try_unify_inference_with_owner_identity(expected, &actual)
        {
            return;
        }
        self.expect_type(expected, &actual, span);
    }

    pub(in crate::check) fn synthesize_range(
        &mut self,
        start: Option<&Spanned<Expr>>,
        end: Option<&Spanned<Expr>>,
    ) -> Ty {
        match (start, end) {
            (Some(s), Some(e)) => {
                let start_ty = self.synthesize(&s.0, &s.1);
                let end_ty = self.synthesize(&e.0, &e.1);
                let start_resolved = self.subst.resolve(&start_ty);
                let end_resolved = self.subst.resolve(&end_ty);

                if start_resolved.is_integer() && end_resolved.is_integer() {
                    if let Some(common_ty) =
                        common_integer_type(&start_resolved, &end_resolved, self.pointer_width())
                    {
                        Ty::range(common_ty)
                    } else {
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            &s.1,
                            format!(
                                "range bounds require compatible integer types; found `{}` and `{}`",
                                start_resolved.user_facing(),
                                end_resolved.user_facing()
                            ),
                        );
                        Ty::Error
                    }
                } else {
                    self.expect_type(&start_ty, &end_ty, &e.1);
                    Ty::range(start_ty)
                }
            }
            (Some(s), None) => Ty::range(self.synthesize(&s.0, &s.1)),
            (None, Some(e)) => Ty::range(self.synthesize(&e.0, &e.1)),
            (None, None) => Ty::range(Ty::I64),
        }
    }

    pub(in crate::check) fn synthesize_cast(
        &mut self,
        inner: &Spanned<Expr>,
        type_expr: &Spanned<TypeExpr>,
        span: &Span,
    ) -> Ty {
        let actual = self.synthesize(&inner.0, &inner.1);
        let (target, hole_vars) = self.resolve_annotation_holes(type_expr);
        let has_target_holes = !hole_vars.is_empty();
        if has_target_holes {
            self.record_deferred_inference_holes(type_expr, "cast target type", hole_vars.clone());
            self.record_deferred_cast_check(span, &actual, &target, hole_vars);
        }
        let actual_resolved = self.subst.resolve(&actual);
        let target_resolved = self.subst.resolve(&target);

        if !has_target_holes && !cast_is_valid(&actual_resolved, &target_resolved) {
            self.report_error(
                TypeErrorKind::Mismatch {
                    expected: target_resolved.user_facing().to_string(),
                    actual: actual_resolved.user_facing().to_string(),
                },
                span,
                format!(
                    "cannot cast `{}` to `{}`",
                    actual_resolved.user_facing(),
                    target_resolved.user_facing()
                ),
            );
        }

        self.record_type(span, &target);
        target
    }

    /// Build a [`ConstEnv`](crate::check::const_eval::ConstEnv) snapshot of the
    /// integer `const` bindings currently in scope, for constexpr count/length
    /// evaluation (e.g. the fixed-array repeat-length check). Float consts are
    /// deliberately excluded — the const-eval sub-engine is integer-only.
    ///
    /// Only bindings with declared-const provenance (`declared_const_bindings`)
    /// are admitted. The current lexical binding must match the declared
    /// constant's binding ID, so a local or parameter cannot inherit a
    /// same-named module constant's value. `const_values` also holds
    /// literal-coercion entries for every unannotated immutable integer literal
    /// (`let n = 4`), whose *value* is not a compile-time constant for length
    /// purposes; admitting those would let a runtime-shaped local silently
    /// satisfy a fixed-array length.
    pub(super) fn const_eval_env(&self) -> crate::check::const_eval::ConstEnv {
        let mut env = crate::check::const_eval::ConstEnv::new();
        for (name, value) in &self.const_values {
            let Some(declared_binding_id) = self.declared_const_bindings.get(name) else {
                continue;
            };
            if self.env.lookup_ref(name).map(|binding| binding.id) != Some(*declared_binding_id) {
                continue;
            }
            if let ConstValue::Integer(v) = value {
                env.insert(name.clone(), *v);
            }
        }
        env
    }

    pub(in crate::check) fn synthesize_array_repeat(
        &mut self,
        value: &Spanned<Expr>,
        count: &Spanned<Expr>,
        span: &Span,
    ) -> Ty {
        let elem_ty = self.synthesize(&value.0, &value.1);
        let count_ty = self.check_against(&count.0, &count.1, &Ty::I64);
        let resolved_count = self.subst.resolve(&count_ty);
        if !resolved_count.is_integer() && !matches!(resolved_count, Ty::Var(_)) {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                &count.1,
                format!(
                    "array repeat count must be an integer, found `{}`",
                    resolved_count.user_facing()
                ),
            );
        }
        if let Expr::Literal(Literal::Integer { value, .. }) = &count.0 {
            if *value < 0 {
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    &count.1,
                    "array repeat count cannot be negative".to_string(),
                );
            }
        }
        // An array repeat copies the element into every slot, so it admits
        // exactly the element types the value class gives a copy path — the
        // same answer `xs[i]`, a range slice and cloning iteration get. A
        // trait object and a `Stream` are drop-only in the class table
        // (`CloneKind::None`), so they refuse here without a second rule.
        if let Some(blocker) = self.element_clone_blocker(&elem_ty) {
            if let Some(param) = blocker.unbounded_param() {
                let param = param.to_string();
                self.report_unbounded_param_copy(&param, "[T; N]", span);
                return self.make_vec_type(elem_ty, span);
            }
            let blocker = blocker.concrete_text();
            let resolved_elem = self.subst.resolve(&elem_ty);
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!(
                    "E_ELEMENT_NO_COPY: `[{elem}; N]` array repeat copies the element into \
                     every slot, but {blocker} has no copy operation; use an explicit loop \
                     that builds each element, or a Copy element type",
                    elem = resolved_elem.user_facing()
                ),
            );
        }
        self.make_vec_type(elem_ty, span)
    }

    /// Type one lazy recovery branch without introducing a callable boundary.
    /// The success path preserves the state after the operand; only the other
    /// path evaluates the fallback or binds the Result error.
    pub(super) fn check_local_recovery(
        &mut self,
        operand: &Spanned<Expr>,
        body: &Spanned<Expr>,
        error: Option<&Spanned<String>>,
        span: &Span,
    ) -> Ty {
        let container = self.synthesize(&operand.0, &operand.1);
        let container = self.subst.resolve(&container);
        let scope_recovery =
            error.is_some() && matches!(operand.0, Expr::Scope { .. } | Expr::ScopeDeadline { .. });
        let (payload, error_ty) = if scope_recovery {
            Some((
                container.clone(),
                Ty::Named {
                    name: "std.builtins.ScopeFailure".to_string(),
                    args: Vec::new(),
                    builtin: None,
                },
            ))
        } else if error.is_some() {
            container
                .as_result()
                .map(|(ok, err)| (ok.clone(), err.clone()))
        } else {
            container.as_option().map(|some| (some.clone(), Ty::Unit))
        }
        .unwrap_or_else(|| {
            if !matches!(container, Ty::Error) {
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    if error.is_some() {
                        format!(
                            "`handle` requires Result, found `{}`",
                            container.user_facing()
                        )
                    } else {
                        format!(
                            "`??` requires Option, found `{}`; handle Result errors explicitly",
                            container.user_facing()
                        )
                    },
                );
            }
            (Ty::Error, Ty::Error)
        });
        let recovery_kind = if scope_recovery {
            super::RecoveryKind::Scope {
                failure_ty: ResolvedTy::from_ty(&error_ty)
                    .expect("ScopeFailure is a concrete source-defined enum"),
            }
        } else if error.is_some() {
            super::RecoveryKind::Result
        } else {
            super::RecoveryKind::Option
        };
        self.recovery_kinds.insert(
            SpanKey::in_module(span, self.current_module_idx),
            recovery_kind,
        );
        let entry = self.env.ownership_snapshot();
        self.env.push_scope();
        if let Some((name, binding_span)) = error {
            self.check_shadowing(name, binding_span);
            self.env
                .define_with_span(name.clone(), error_ty, false, binding_span.clone());
        }
        let body_ty = if payload == Ty::Never {
            self.synthesize(&body.0, &body.1)
        } else {
            self.check_expr_with_expected(&body.0, &body.1, &payload)
        };
        let taken = BranchArmExit {
            ownership: self.env.ownership_snapshot(),
            diverges: Self::arm_skips_join(&body_ty),
        };
        self.env.pop_scope();
        self.join_fall_through(&entry, taken);
        let payload = self.subst.resolve(&payload);
        if payload == Ty::Never {
            self.subst.resolve(&body_ty)
        } else {
            payload
        }
    }

    /// A spread reads each of the operand's elements and pushes an independent
    /// copy onto the new vector, so it admits exactly the element types the
    /// value class gives a copy path — the same answer `xs[i]`, a range slice
    /// and cloning iteration get.
    pub(super) fn refuse_uncopyable_spread_element(&mut self, elem_ty: &Ty, span: &Span) {
        let Some(blocker) = self.element_clone_blocker(elem_ty) else {
            return;
        };
        if let Some(param) = blocker.unbounded_param() {
            let param = param.to_string();
            self.report_unbounded_param_copy(&param, "a spread", span);
            return;
        }
        let blocker = blocker.concrete_text();
        let resolved = self.subst.resolve(elem_ty).materialize_literal_defaults();
        self.report_error(
            TypeErrorKind::InvalidOperation,
            span,
            format!(
                "E_ELEMENT_NO_COPY: spreading a `Vec<{elem}>` copies each element into the \
                 new vector, but {blocker} has no copy operation; use an owning removal such \
                 as `pop()` to move the elements out instead",
                elem = resolved.user_facing()
            ),
        );
    }

    /// `Vec<elem_ty>` without the concrete-element validation `make_vec_type`
    /// performs: used to build an expectation for a spread operand, where the
    /// element type may still be an inference variable.
    pub(super) fn vec_of(elem_ty: Ty) -> Ty {
        Ty::Named {
            builtin: Some(BuiltinType::Vec),
            name: "Vec".to_string(),
            args: vec![elem_ty],
        }
    }

    pub(in crate::check) fn synthesize_array_literal(
        &mut self,
        elements: &[ArrayElement],
        span: &Span,
    ) -> Ty {
        let mut elem_ty: Option<Ty> = None;
        let mut spread_span: Option<Span> = None;
        for element in elements {
            let (operand, operand_span) = element.expr();
            match element {
                ArrayElement::Value(_) => {
                    match elem_ty.clone() {
                        None => elem_ty = Some(self.synthesize(operand, operand_span)),
                        // Distinct closures and function items only meet in
                        // their erased callable type.
                        Some(current) if self.subst.resolve(&current).contains_callable() => {
                            let next = self.synthesize(operand, operand_span);
                            elem_ty =
                                Some(self.join_callable_values(&current, &next, operand_span));
                        }
                        Some(current) => {
                            self.check_against(operand, operand_span, &current);
                        }
                    }
                    self.record_value_transfer(operand, operand_span);
                }
                ArrayElement::Spread(_) => {
                    // A spread operand is a `Vec` of the literal's element
                    // type, so unifying against `Vec<T>` both infers `T` from
                    // the first spread and refuses a mismatched later one.
                    let current = elem_ty.clone().unwrap_or_else(|| Ty::Var(TypeVar::fresh()));
                    let want = Self::vec_of(current.clone());
                    self.check_against(operand, operand_span, &want);
                    elem_ty = Some(current);
                    spread_span.get_or_insert_with(|| operand_span.clone());
                }
            }
        }
        let elem_ty = elem_ty.unwrap_or_else(|| Ty::Var(TypeVar::fresh()));
        if let Some(spread_span) = spread_span {
            self.refuse_uncopyable_spread_element(&elem_ty, &spread_span);
        }
        self.make_vec_type(elem_ty, span)
    }

    pub(in crate::check) fn synthesize_map_literal(
        &mut self,
        entries: &[(Spanned<Expr>, Spanned<Expr>)],
        span: &Span,
    ) -> Ty {
        if entries.is_empty() {
            let k = TypeVar::fresh();
            let v = TypeVar::fresh();
            Ty::Named {
                builtin: Some(BuiltinType::HashMap),
                name: "HashMap".to_string(),
                args: vec![Ty::Var(k), Ty::Var(v)],
            }
        } else {
            let (ref ke, ref ks) = entries[0].0;
            let (ref ve, ref vs) = entries[0].1;
            let first_key_ty = self.synthesize(ke, ks);
            let first_val_ty = self.synthesize(ve, vs);
            for (k, v) in &entries[1..] {
                self.check_against(&k.0, &k.1, &first_key_ty);
                self.check_against(&v.0, &v.1, &first_val_ty);
            }
            self.validate_hashmap_key_value_types(&first_key_ty, &first_val_ty, span);
            Ty::Named {
                builtin: Some(BuiltinType::HashMap),
                name: "HashMap".to_string(),
                args: vec![first_key_ty, first_val_ty],
            }
        }
    }

    pub(in crate::check) fn synthesize_iflet(
        &mut self,
        conditions: &[ConditionItem],
        body: &Block,
        else_body: Option<&Spanned<Expr>>,
        span: &Span,
    ) -> Ty {
        let entry = self.env.ownership_snapshot();
        self.check_condition(conditions);
        let then_ty = self.check_block(body, None);
        let then_exit = BranchArmExit {
            ownership: self.env.ownership_snapshot(),
            diverges: Self::arm_skips_join(&then_ty),
        };
        self.env.pop_scope();
        if let Some(else_expr) = else_body {
            self.env.restore_ownership(&entry);
            let else_ty = self.synthesize(&else_expr.0, &else_expr.1);
            let else_exit = BranchArmExit {
                ownership: self.env.ownership_snapshot(),
                diverges: Self::arm_skips_join(&else_ty),
            };
            self.join_branch_ownership(&entry, &[then_exit, else_exit]);
            self.unify_branches(&then_ty, &else_ty, span)
        } else {
            self.join_fall_through(&entry, then_exit);
            Ty::Unit
        }
    }

    pub(in crate::check) fn report_invalid_actor_send(&mut self, ty: &Ty, span: &Span) {
        self.report_error_with_suggestions(
            TypeErrorKind::InvalidSend,
            span,
            format!(
                "cannot send `{}` to actor: type is not Send",
                ty.user_facing()
            ),
            vec!["keep the resource inside one owning actor and send that actor a message instead — see the language guide, 'Own a resource with an actor'".to_string()],
        );
    }

    pub(in crate::check) fn mark_expr_moved_if_non_copy(
        &mut self,
        expr: &Expr,
        span: &Span,
        ty: &Ty,
    ) {
        if !self.registry.implements_marker(ty, MarkerTrait::Copy)
            || self.reads_resource_handle_field(expr)
        {
            self.mark_expr_moved(expr, span);
        }
    }

    /// Whether `expr` reads a value carrying an `#[opaque]` handle out of a
    /// `#[resource]` record (D528). The handle itself classes as a bit copy so
    /// FFI calls can borrow it, but the enclosing resource's `close` releases
    /// it: reading it out by value anywhere but that `close` - directly, in a
    /// plain record below the resource or inside an `Option` - would leave two
    /// owners of one handle, so such a read is a transfer and the
    /// partial-consume rule decides it.
    pub(in crate::check) fn reads_resource_handle_field(&self, expr: &Expr) -> bool {
        self.expr_place(expr)
            .is_some_and(|(root, path)| self.resource_handle_owner(&root, &path).is_some())
    }

    /// The depth of the nearest `#[resource]` record on `path` whose `close`
    /// releases a handle the selected value carries.
    pub(super) fn resource_handle_owner(&self, root: &str, path: &[String]) -> Option<usize> {
        let binding = self.env.lookup_ref(root)?;
        let mut parent = self.subst.resolve(&binding.ty);
        let mut owner = None;
        for (depth, step) in path.iter().enumerate() {
            if matches!(&parent, Ty::Named { name, .. } if self.registry.is_resource(name)) {
                owner = Some(depth);
            }
            let selected = match &parent {
                Ty::Tuple(items) => items.get(step.parse::<usize>().ok()?).cloned(),
                _ => self.project_named_field(&parent, step),
            };
            parent = self.subst.resolve(&selected?);
        }
        owner.filter(|_| self.carries_resource_handle(&parent, &mut HashSet::new()))
    }

    /// Whether a value of `ty` holds a marker-free `#[opaque]` handle that is
    /// not itself owned by a nested `#[resource]`.
    pub(super) fn carries_resource_handle(&self, ty: &Ty, visiting: &mut HashSet<String>) -> bool {
        match ty {
            Ty::Named { name, args, .. } => {
                if self.registry.is_resource(name) {
                    return false;
                }
                if crate::value_class::ClassDeclarations::declared_type(
                    &self.class_declarations(),
                    name,
                )
                .is_some_and(|declaration| {
                    declaration.is_opaque
                        && declaration.marker == crate::value_class::DeclarationMarker::None
                }) {
                    return true;
                }
                if args
                    .iter()
                    .any(|arg| self.carries_resource_handle(&self.subst.resolve(arg), visiting))
                {
                    return true;
                }
                let Some(members) = self.registry.member_types(name) else {
                    return false;
                };
                if !visiting.insert(name.clone()) {
                    return false;
                }
                let carries = members
                    .to_vec()
                    .iter()
                    .any(|member| self.carries_resource_handle(member, visiting));
                visiting.remove(name);
                carries
            }
            Ty::Tuple(elements) => elements
                .iter()
                .any(|element| self.carries_resource_handle(element, visiting)),
            Ty::Array(element, _) | Ty::Slice(element) => {
                self.carries_resource_handle(element, visiting)
            }
            _ => false,
        }
    }

    /// Mark an identifier binding moved, unconditionally.
    ///
    /// Callers that have already PROVEN the value transfers ownership use this
    /// directly instead of [`Self::mark_expr_moved_if_non_copy`]. The `Copy`
    /// gate is not merely redundant there, it is wrong: an owned handle whose
    /// members are all scalars (`MonitorRef { ref_id: u64 }`) derives `Copy`
    /// structurally under a spelling that carries no negative impl, and the
    /// gate would then silently skip the move — leaving two owners of one
    /// registration. Ownership is decided by the transfer predicate, not by
    /// the representation of the bytes.
    /// Mark the PLACE an expression denotes as moved, unconditionally.
    ///
    /// The place is the root binding plus the projection steps taken from it,
    /// so a field transfer (`await a.take(h.sock)`) records that `h.sock`
    /// specifically is gone while `h`'s siblings stay usable. Consuming a
    /// projection used to no-op here, which is how a transferred field could be
    /// detached a second time through the same projection.
    ///
    /// Deliberately reports nothing: every site that consumes an expression
    /// also SYNTHESISES it first, and the read paths ([`Self::check_field_access`]
    /// and [`Self::synthesize_identifier`]) own the use-after-move diagnostic.
    /// Reporting here as well would double-diagnose one consuming use.
    pub(in crate::check) fn mark_expr_moved(&mut self, expr: &Expr, span: &Span) {
        let Some((root, path)) = self.expr_place(expr) else {
            return;
        };
        if self.reject_borrowed_consumption(expr, span) {
            return;
        }
        if !path.is_empty() {
            if self.reject_borrowed_consumption(expr, span)
                || self.reject_partial_place_consumption(&root, &path, span)
            {
                return;
            }
            self.env.mark_place_moved(&root, path, span.clone());
            return;
        }
        let released_at = self
            .env
            .lookup_ref(&root)
            .and_then(|binding| binding.released_at.clone());
        if let Some(released_at) = released_at {
            let mut error = TypeError::new(
                TypeErrorKind::UseAfterConsume,
                span.clone(),
                format!(
                    "cannot consume released resource `{root}`; its close obligation was already discharged"
                ),
            )
            .with_note(released_at, "resource was closed here");
            if let Some(source_module) = &self.current_module {
                error = error.with_source_module(source_module.clone());
            }
            self.errors.push(error);
        }
        self.env.mark_moved(&root, span.clone());
    }

    /// A selected field may move only when every enclosing value supports
    /// independent field ownership. The selected value's own cleanup contract
    /// does not prevent moving that entire value out of its plain parent.
    pub(in crate::check) fn reject_partial_place_consumption(
        &mut self,
        root: &str,
        path: &[String],
        span: &Span,
    ) -> bool {
        let Some(binding) = self.env.lookup_ref(root) else {
            return false;
        };
        let mut parent = self.subst.resolve(&binding.ty);
        for (depth, field) in path.iter().enumerate() {
            if depth == 0 && self.resource_close_owns_self_field(root, &parent) {
                let Some(selected) = self.project_named_field(&parent, field) else {
                    return false;
                };
                parent = self.subst.resolve(&selected);
                continue;
            }
            let Some(selected) = self.independent_record_or_tuple_field(&parent, field) else {
                if self.resource_handle_owner(root, path) == Some(depth) {
                    let record = Self::render_place(root, &path[..depth]);
                    let handed_out = Self::render_place(field, &path[depth + 1..]);
                    let resource = parent.user_facing().to_string();
                    let short = parent
                        .type_name()
                        .and_then(|name| name.rsplit('.').next())
                        .unwrap_or_default()
                        .to_string();
                    self.report_error_with_suggestions(
                        TypeErrorKind::OwnPartialConsume,
                        span,
                        format!(
                            "cannot read `{}` by value: `{resource}` releases the `#[opaque]` \
                             handle it carries in its `close`, so outside `close` it cannot be \
                             copied or moved out",
                            Self::render_place(root, path),
                        ),
                        vec![format!(
                            "destructure the resource to hand the handle out without running \
                             `close`: `let {short} {{ {field} }} = {record}; {handed_out}`"
                        )],
                    );
                    return true;
                }
                self.report_error_with_suggestions(
                    TypeErrorKind::OwnPartialConsume,
                    span,
                    format!(
                        "cannot consume `{}` separately: enclosing type `{}` must remain whole",
                        Self::render_place(root, path),
                        parent.user_facing(),
                    ),
                    vec!["transfer the enclosing value whole to a consuming operation".to_string()],
                );
                return true;
            };
            parent = self.subst.resolve(&selected);
        }
        false
    }

    /// A resource destructor owns its receiver and may transfer one field to
    /// the external release operation. Close-body cleanup retains every field
    /// it does not move out. This exception is deliberately narrower than an
    /// arbitrary consuming method: it requires the registered inherent
    /// `close(consume self)` contract and the lexical receiver binding.
    pub(super) fn resource_close_owns_self_field(&self, root: &str, parent: &Ty) -> bool {
        if root != "self" {
            return false;
        }
        let Some(function) = self.current_function.as_ref() else {
            return false;
        };
        let Some(signature) = self.fn_sigs.get(function) else {
            return false;
        };
        if !signature.consumes_receiver
            || !signature
                .impl_method
                .as_ref()
                .is_some_and(|method| method.is_inherent && method.name == "close")
        {
            return false;
        }
        matches!(parent, Ty::Named { name, .. } if self.registry.is_resource(name))
    }

    pub(super) fn project_named_field(&self, parent: &Ty, field: &str) -> Option<Ty> {
        let Ty::Named { name, args, .. } = parent else {
            return None;
        };
        let definition = self.type_defs.get(name)?;
        if definition.type_params.len() != args.len() {
            return None;
        }
        let substitutions = definition
            .type_params
            .iter()
            .cloned()
            .zip(args.iter().cloned())
            .collect();
        definition
            .fields
            .get(field)
            .map(|ty| ty.substitute_named_params_parallel(&substitutions))
    }

    pub(super) fn independent_record_or_tuple_field(&self, parent: &Ty, field: &str) -> Option<Ty> {
        match parent {
            Ty::Tuple(items) => items.get(field.parse::<usize>().ok()?).cloned(),
            Ty::Named { name, args, .. } => {
                let declaration = crate::value_class::ClassDeclarations::declared_type(
                    &self.class_declarations(),
                    name,
                )?;
                if declaration.marker != crate::value_class::DeclarationMarker::None
                    || declaration.is_opaque
                {
                    return None;
                }
                let definition = self.type_defs.get(name)?;
                if !matches!(definition.kind, TypeDefKind::Struct | TypeDefKind::Record)
                    || definition.type_params.len() != args.len()
                {
                    return None;
                }
                let substitutions = definition
                    .type_params
                    .iter()
                    .cloned()
                    .zip(args.iter().cloned())
                    .collect();
                Some(
                    definition
                        .fields
                        .get(field)?
                        .substitute_named_params_parallel(&substitutions),
                )
            }
            _ => None,
        }
    }

    /// Resolve an expression to a checker PLACE: the root binding name plus the
    /// projection steps taken from it.
    ///
    /// Tuple element access (`t.0`) parses as a field access with a numeric
    /// field name, so field steps cover both spellings.
    ///
    /// Returns `None` for anything that is not a projection chain rooted in a
    /// binding — indexing, calls, `this`. Those roots have no binding-level
    /// ownership slot to attach a fact to, so nothing is recorded for them
    /// rather than a guess being recorded; element-of-collection places are the
    /// known remaining hole and belong to the MIR half of this family.
    /// Indexed writes and mutating methods need a copy of every indexed parent.
    pub(in crate::check) fn reject_indexed_writable_borrow(&mut self, target: &Spanned<Expr>) {
        let mut parent = target;
        loop {
            if self
                .borrowed_element_index_reads
                .contains(&SpanKey::in_module(&parent.1, self.current_module_idx))
            {
                self.report_error(TypeErrorKind::OwnConsumeBorrowed, &parent.1,
                    "cannot update through a borrowed affine collection element; indexed writeback requires a semantic copy".into());
                return;
            }
            match &parent.0 {
                Expr::FieldAccess { object, .. } | Expr::Index { object, .. } => parent = object,
                _ => return,
            }
        }
    }

    pub(in crate::check) fn expr_place(&self, expr: &Expr) -> Option<(String, PlacePath)> {
        match expr {
            Expr::Identifier(name) => Some((name.clone(), PlacePath::new())),
            Expr::FieldAccess { object, field } => {
                // `self.count` in an actor body denotes the state binding
                // `count`, so the place it names is rooted in that binding —
                // never in a binding called `self`, which does not exist here.
                if let Some(state_field) = self.actor_self_state_field(&object.0, field) {
                    return Some((state_field.to_string(), PlacePath::new()));
                }
                let (root, mut path) = self.expr_place(&object.0)?;
                path.push(field.clone());
                Some((root, path))
            }
            _ => None,
        }
    }

    /// The actor state field an `object.field` projection names when `object`
    /// is the actor receiver `self`, or `None` when it is an ordinary
    /// projection.
    ///
    /// An actor's state fields are bound as ordinary environment bindings for
    /// the whole body, which is what makes bare `count` work; `self` is the
    /// receiver that spells the same binding explicitly. Every site that
    /// matches on the projection's shape routes through here so the two
    /// spellings share one resolution, one mutability rule, and one lowering
    /// instead of growing a parallel receiver path.
    ///
    /// `self` is a real bound parameter on impl and trait methods, so an
    /// in-scope `self` binding means the projection is an ordinary field
    /// access on the receiver value and is left alone. A name that is not a
    /// declared state field is left alone too, so [`Self::check_field_access`]
    /// can report it against the actor.
    pub(in crate::check) fn actor_self_state_field<'a>(
        &self,
        object: &Expr,
        field: &'a str,
    ) -> Option<&'a str> {
        if !self.is_actor_self_receiver(object) {
            return None;
        }
        self.current_actor_fields
            .iter()
            .any(|f| f.name == field)
            .then_some(field)
    }

    /// Publish the receiver resolution for one `self.field` projection at
    /// `span`, the span of the whole projection.
    ///
    /// [`Self::actor_self_state_field`] is the predicate several checker sites
    /// read; this is the one place that writes the answer down. Every lowerer
    /// looks the projection up in
    /// [`TypeCheckOutput::actor_self_state_fields`](crate::TypeCheckOutput)
    /// instead of re-deciding it, so a projection is the receiver spelling in
    /// every backend or in none.
    pub(in crate::check) fn record_actor_self_state_field(&mut self, span: &Span) {
        self.actor_self_state_fields
            .insert(SpanKey::in_module(span, self.current_module_idx));
    }

    /// Whether an expression is the actor receiver `self`: the bare name,
    /// inside an actor body, with no `self` binding in scope to mean something
    /// else. The projected name may still not be a state field — that case
    /// belongs to [`Self::check_field_access`], which reports it against the
    /// actor rather than letting the receiver be synthesised as a value.
    pub(in crate::check) fn is_actor_self_receiver(&self, object: &Expr) -> bool {
        matches!(object, Expr::Identifier(name) if name == "self")
            && self.current_actor_type.is_some()
            && self.env.lookup_ref("self").is_none()
    }

    /// Render a place for diagnostics: `h.sock`, or plain `h` for the root.
    pub(in crate::check) fn render_place(root: &str, path: &[String]) -> String {
        std::iter::once(root)
            .chain(path.iter().map(String::as_str))
            .collect::<Vec<_>>()
            .join(".")
    }

    /// Report a use of `root`'s place at `path` that collides with a place
    /// already consumed on this path, if it does.
    pub(in crate::check) fn report_place_use_after_move(
        &mut self,
        root: &str,
        path: &[String],
        span: &Span,
    ) {
        let Some((conflict, moved_path, moved_at)) = self.env.place_move_conflict(root, path)
        else {
            return;
        };
        // A place read only to project further into it is not a whole-value
        // use of itself, at any depth: `o.inner` inside `o.inner.ticket` names
        // an address, not the aggregate. Without this the partially-moved-root
        // rule would fire on every ancestor of a moved place and stack one
        // diagnostic per projection step on top of the real one.
        if conflict == PlaceConflict::WholeOfPartial && self.place_base_depth > 0 {
            return;
        }
        let place = Self::render_place(root, path);
        let moved_place = Self::render_place(root, &moved_path);
        let (message, suggestion) = match conflict {
            PlaceConflict::Exact => (
                format!("use of moved place `{place}`"),
                format!(
                    "`{place}` transferred its value away; re-initialise it \
                     (`{place} = ...`) before using it again"
                ),
            ),
            PlaceConflict::UnderMoved => (
                format!("use of `{place}`, which lives inside moved place `{moved_place}`"),
                format!(
                    "`{moved_place}` transferred its value away, taking `{place}` with it; \
                     read it before the transfer, or re-initialise `{moved_place}`"
                ),
            ),
            PlaceConflict::WholeOfPartial => (
                format!("use of `{place}` after its field `{moved_place}` was moved out"),
                format!(
                    "`{place}` is only partially owned here; use the fields that are still \
                     owned, or re-initialise `{moved_place}` before using `{place}` whole"
                ),
            ),
        };
        let mut error = TypeError::new(TypeErrorKind::UseAfterMove, span.clone(), message)
            .with_note(moved_at, "value was consumed here")
            .with_suggestion(suggestion);
        if let Some(source_module) = &self.current_module {
            error = error.with_source_module(source_module.clone());
        }
        self.errors.push(error);
    }

    /// Whether `ty` carries a value whose SOLE ownership crosses an actor
    /// message boundary — a substrate handle, or a user `#[resource]` /
    /// `#[linear]` declaration.
    ///
    /// The builtin half delegates to
    /// [`BuiltinType::transfers_ownership_across_actor_boundary`], the single
    /// authority HIR's intent stamping also reads. The nominal half is this
    /// checker's own: `#[resource]` and `#[linear]` types have exactly one
    /// ownership path, and MIR physically MOVES every message argument out of
    /// the caller frame (`lower_value_for_move`), so a later use of the caller
    /// binding is a genuine use-after-move. Without the nominal arm the caller
    /// kept its binding live and both frames consumed the one value.
    ///
    /// Copy-on-write values (`string`, `Vec`, plain records, tuples of them)
    /// are deliberately NOT here: the boundary copies them and both frames own
    /// their own copy, which is the language's default value semantics. A
    /// record that CONTAINS a resource is a different matter — see below.
    ///
    /// The walk is structural and total. It descends generic arguments, tuple
    /// elements, array/slice elements, AND registered record/enum member types,
    /// because containment is what decides ownership: `type Holder { socket:
    /// Socket }` transfers the socket just as surely as `(Socket, i64)` does,
    /// and sending one `Holder` twice gives the socket two drop paths. Skipping
    /// the named-member edge left exactly that hole open while the tuple edge
    /// was closed.
    pub(super) fn ty_contains_affine_actor_transfer(&self, ty: &Ty) -> bool {
        let mut visiting = std::collections::HashSet::new();
        self.ty_contains_affine_actor_transfer_guarded(ty, &mut visiting)
    }

    /// Whether a MODULE-QUALIFIED type name denotes a transferring builtin.
    ///
    /// A source-declared lifecycle type (`std.link_monitor.MonitorRef`) reaches
    /// some positions — notably a declared actor state-field type — spelled by
    /// its qualified path with no `builtin` tag attached, so the tag test alone
    /// misses it and the handle silently stayed shareable.
    ///
    /// The qualification requirement is load-bearing: `lookup_builtin_type`
    /// also resolves BARE canonical names, and a user `type MonitorRef` shadow
    /// is a clone-total record that must keep ordinary value semantics. Only
    /// the dotted spelling is the stdlib declaration.
    pub(super) fn qualified_name_resolves_to_transferring_builtin(name: &str) -> bool {
        name.contains('.')
            && crate::lookup_builtin_type(name)
                .is_some_and(BuiltinType::transfers_ownership_across_actor_boundary)
    }

    /// Recursion body for [`Self::ty_contains_affine_actor_transfer`].
    ///
    /// `visiting` makes the walk total over recursive type graphs
    /// (`type Node { next: Vec<Node> }`). Re-entering a name already on the
    /// stack contributes no NEW ownership edge, so it answers `false` — the
    /// neutral element of the `any(...)` disjunction — and the result is
    /// decided by the non-recursive members. This mirrors the recursion guard
    /// marker derivation already uses (`implements_marker_guarded`).
    pub(super) fn ty_contains_affine_actor_transfer_guarded(
        &self,
        ty: &Ty,
        visiting: &mut std::collections::HashSet<String>,
    ) -> bool {
        match ty {
            Ty::CancellationToken => true,
            Ty::Named {
                name,
                args,
                builtin,
            } => {
                if builtin.is_some_and(BuiltinType::transfers_ownership_across_actor_boundary)
                    || Self::qualified_name_resolves_to_transferring_builtin(name)
                    || self.registry.is_resource(name)
                    || self.registry.is_linear(name)
                {
                    return true;
                }
                if args
                    .iter()
                    .any(|arg| self.ty_contains_affine_actor_transfer_guarded(arg, visiting))
                {
                    return true;
                }
                // A builtin carries no user member set to descend into, and its
                // ownership verdict is already decided above.
                if builtin.is_some() || !visiting.insert(name.clone()) {
                    return false;
                }
                let members: Vec<Ty> = self
                    .registry
                    .member_types(name)
                    .map(<[Ty]>::to_vec)
                    .unwrap_or_default();
                let carries = members
                    .iter()
                    .any(|member| self.ty_contains_affine_actor_transfer_guarded(member, visiting));
                visiting.remove(name);
                carries
            }
            Ty::Tuple(elements) => elements
                .iter()
                .any(|element| self.ty_contains_affine_actor_transfer_guarded(element, visiting)),
            Ty::Array(element, _) | Ty::Slice(element) => {
                self.ty_contains_affine_actor_transfer_guarded(element, visiting)
            }
            _ => false,
        }
    }

    pub(in crate::check) fn enforce_actor_boundary_send(
        &mut self,
        expr: &Expr,
        move_span: &Span,
        error_span: &Span,
        ty: &Ty,
    ) {
        let ty = self.subst.resolve(ty);
        let boundary_ty = self.normalize_for_use(&ty);
        if !self.type_satisfies_trait_bound(&boundary_ty, "Send") {
            self.report_invalid_actor_send(&ty, error_span);
        }
        if self.ty_contains_affine_actor_transfer(&ty) {
            self.mark_affine_transfer_moved(expr, move_span);
        }
    }

    /// Mark the source of one affine boundary transfer moved.
    ///
    /// A handle sent directly names a place and marks straight through. A
    /// handle packed into a tuple or array literal at the call site names no
    /// place of its own, so the literal is transparent here: the mailbox takes
    /// the aggregate and with it each element, and the element binding is
    /// exactly what a later use must be refused against.
    pub(super) fn mark_affine_transfer_moved(&mut self, expr: &Expr, move_span: &Span) {
        match expr {
            Expr::Tuple(elements) => {
                for (element, span) in elements {
                    self.mark_affine_transfer_element(element, span);
                }
            }
            Expr::Array(elements) => {
                for element in elements {
                    let (element, span) = element.expr();
                    self.mark_affine_transfer_element(element, span);
                }
            }
            _ => self.mark_expr_moved(expr, move_span),
        }
    }

    /// One element of an aggregate literal crossing the boundary: descend only
    /// where the element's own checked type carries a transferring owner.
    pub(super) fn mark_affine_transfer_element(&mut self, expr: &Expr, span: &Span) {
        let key = super::SpanKey::in_module(span, self.current_module_idx);
        let Some(ty) = self.expr_types.get(&key).map(|ty| self.subst.resolve(ty)) else {
            return;
        };
        if self.ty_contains_affine_actor_transfer(&ty) {
            self.mark_affine_transfer_moved(expr, span);
        }
    }

    pub(in crate::check) fn synthesize_yield(
        &mut self,
        value: Option<&Spanned<Expr>>,
        span: &Span,
    ) -> Ty {
        if !self.in_generator {
            self.report_error(
                TypeErrorKind::YieldOutsideGenerator,
                span,
                "`yield` outside of generator function".to_string(),
            );
        }
        if let Some(val_expr) = value {
            if let Some(return_ty) = &self.current_return_type {
                let resolved = self.subst.resolve(return_ty);
                let yield_ty = if let Some((yields, _)) = resolved.as_generator() {
                    yields.clone()
                } else {
                    resolved
                };
                self.check_against(&val_expr.0, &val_expr.1, &yield_ty);
            } else {
                self.synthesize(&val_expr.0, &val_expr.1);
            }
        }
        Ty::Unit
    }

    /// D524: an `#[on(crash)]` hook runs on the crashing incarnation's state,
    /// and a handler that faulted between consuming a copy-less field and
    /// storing its replacement left that seat empty. The hook may not read a
    /// field any body of the actor consumes.
    pub(super) fn reject_crash_hook_consumed_state_read(
        &mut self,
        binding: crate::env::TypeBindingId,
        span: &Span,
    ) {
        let Some(field) = self.crash_hook_consumed_fields.get(&binding) else {
            return;
        };
        let (consumer, consumed_at) = self.actor_consumed_state[field].clone();
        let mut error = TypeError::new(
            TypeErrorKind::UseAfterConsume,
            span.clone(),
            format!(
                "`#[on(crash)]` hook reads actor state `{field}`, which `{consumer}` consumes; \
                 a crash before `{consumer}` stores its replacement leaves `{field}` empty"
            ),
        )
        .with_note(consumed_at, format!("`{consumer}` consumes `{field}` here"))
        .with_suggestion(format!(
            "hold `{field}` as an `Option` and move it out with `{field}.take()`, which leaves \
             `None` in the field instead of consuming it"
        ));
        if let Some(source_module) = &self.current_module {
            error = error.with_source_module(source_module.clone());
        }
        self.errors.push(error);
    }

    pub(in crate::check) fn synthesize_identifier(&mut self, name: &str, span: &Span) -> Ty {
        self.synthesize_identifier_with_type_args(name, None, span)
    }
}
