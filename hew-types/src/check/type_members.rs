#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;
use crate::check::calls::SignatureArgApplication;
use crate::method_resolution::instantiate_stdlib_method_sig;

pub(super) struct ResolvedDottedTypeHead {
    pub(super) canonical_type: String,
    builtin: Option<crate::BuiltinType>,
    type_args: Option<Vec<Spanned<TypeExpr>>>,
    span: Span,
}

pub(super) enum DottedTypeMemberUse<'a> {
    Reference { span: &'a Span },
    Call { args: &'a [CallArg], span: &'a Span },
}

impl Checker {
    /// Resolve a dotted expression head to the declaration identity selected
    /// by the checker. Value bindings win before this path, and every accepted
    /// module/type spelling comes from a declaration or builtin authority.
    pub(super) fn resolve_dotted_type_head(
        &mut self,
        head: &Spanned<Expr>,
    ) -> Option<ResolvedDottedTypeHead> {
        let (target, type_args) = match &head.0 {
            Expr::GenericApplySuffix { target, type_args } => {
                (target.as_ref(), Some(type_args.clone()))
            }
            _ => (head, None),
        };

        let (canonical_type, builtin) = match &target.0 {
            Expr::Identifier(surface) => {
                if self.env.lookup_ref(surface).is_some()
                    || self.module_binding_in_current_file(surface)
                {
                    return None;
                }
                let canonical = self
                    .resolve_nominal_declaration(NominalOrigin::Lexical, surface)
                    .filter(|identity| {
                        self.lookup_type_def(identity).is_some()
                            || self.resolved_builtin_type(identity).is_some()
                    })
                    .or_else(|| self.resolved_builtin_type(surface).map(|_| surface.clone()))?;
                let builtin = self.resolved_builtin_type(&canonical);
                (canonical, builtin)
            }
            Expr::FieldAccess { object, field } => {
                let Expr::Identifier(module_short) = &object.0 else {
                    return None;
                };
                if self.env.lookup_ref(module_short).is_some() {
                    return None;
                }
                let type_def = self.resolve_module_type(module_short, field)?;
                self.used_modules.borrow_mut().insert(ImportKey::in_file(
                    self.current_module.clone(),
                    self.current_module_idx,
                    module_short.clone(),
                ));
                let canonical = type_def.name;
                let builtin = self.resolved_builtin_type(&canonical);
                (canonical, builtin)
            }
            _ => return None,
        };

        Some(ResolvedDottedTypeHead {
            canonical_type,
            builtin,
            type_args,
            span: target.1.clone(),
        })
    }

    /// Dispatch a member selected from a resolved type head. The declaration
    /// identity is fixed before the semantic adapters run: declared variants
    /// (including machine states), builtin Option/Result variants, and static
    /// members all consume the same canonical fact.
    pub(super) fn dispatch_dotted_type_member(
        &mut self,
        head: &ResolvedDottedTypeHead,
        member: &str,
        usage: &DottedTypeMemberUse<'_>,
    ) -> Option<Ty> {
        if let Some(result) = self.dispatch_declared_variant_member(head, member, usage) {
            return Some(result);
        }
        if let Some(result) = self.dispatch_builtin_variant_member(head, member, usage) {
            return Some(result);
        }
        let DottedTypeMemberUse::Call { args, span } = usage else {
            return None;
        };
        self.dispatch_static_type_member(head, member, args, span)
    }

    fn dispatch_declared_variant_member(
        &mut self,
        head: &ResolvedDottedTypeHead,
        member: &str,
        usage: &DottedTypeMemberUse<'_>,
    ) -> Option<Ty> {
        let type_def = self.type_defs.get(&head.canonical_type)?;
        if !matches!(
            type_def.kind,
            TypeDefKind::Enum | TypeDefKind::Struct | TypeDefKind::Machine
        ) {
            return None;
        }
        let variant = type_def.variants.get(member)?;
        match usage {
            DottedTypeMemberUse::Reference { span }
                if matches!(variant, VariantDef::Unit | VariantDef::Tuple(_)) =>
            {
                let constructor = format!("{}::{member}", head.canonical_type);
                Some(self.synthesize_identifier(&constructor, span))
            }
            DottedTypeMemberUse::Call { args, span }
                if matches!(variant, VariantDef::Unit | VariantDef::Tuple(_)) =>
            {
                let constructor_name = format!("{}::{member}", head.canonical_type);
                let constructor = (Expr::Identifier(constructor_name), head.span.clone());
                let result = self.check_call(&constructor, head.type_args.as_deref(), args, span);
                self.record_method_call_receiver_kind(
                    span,
                    MethodCallReceiverKind::EnumConstructorPath {
                        type_name: head.canonical_type.clone(),
                    },
                );
                Some(result)
            }
            _ => None,
        }
    }

    fn dispatch_builtin_variant_member(
        &mut self,
        head: &ResolvedDottedTypeHead,
        member: &str,
        usage: &DottedTypeMemberUse<'_>,
    ) -> Option<Ty> {
        let (builtin, constructor, expected_arity) = match (head.builtin, member) {
            (Some(crate::BuiltinType::Option), "Some" | "None") => {
                (crate::BuiltinType::Option, member, 1)
            }
            (Some(crate::BuiltinType::Result), "Ok" | "Err") => {
                (crate::BuiltinType::Result, member, 2)
            }
            _ => return None,
        };

        let result = match usage {
            DottedTypeMemberUse::Reference { span } => {
                self.synthesize_identifier(constructor, span)
            }
            DottedTypeMemberUse::Call { args, span } => {
                let function = (Expr::Identifier(constructor.to_string()), head.span.clone());
                let result = self.check_call(&function, None, args, span);
                self.record_method_call_receiver_kind(
                    span,
                    MethodCallReceiverKind::EnumConstructorPath {
                        type_name: head.canonical_type.clone(),
                    },
                );
                result
            }
        };

        let Some(type_args) = head.type_args.as_deref() else {
            return Some(result);
        };
        if type_args.len() != expected_arity {
            let span = match usage {
                DottedTypeMemberUse::Reference { span }
                | DottedTypeMemberUse::Call { span, .. } => *span,
            };
            self.report_error(
                TypeErrorKind::ArityMismatch,
                span,
                format!(
                    "type `{}` has {expected_arity} type parameter(s) but {} type argument(s) were supplied",
                    head.canonical_type,
                    type_args.len()
                ),
            );
            return Some(Ty::Error);
        }
        let resolved_args = type_args
            .iter()
            .map(|type_arg| self.resolve_type_expr(type_arg))
            .collect::<Vec<_>>();
        let expected = Ty::Named {
            builtin: Some(builtin),
            name: head.canonical_type.clone(),
            args: resolved_args,
        };
        let span = match usage {
            DottedTypeMemberUse::Reference { span } | DottedTypeMemberUse::Call { span, .. } => {
                *span
            }
        };
        self.expect_type(&expected, &result, span);
        Some(self.subst.resolve(&expected))
    }

    fn dispatch_static_type_member(
        &mut self,
        head: &ResolvedDottedTypeHead,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Option<Ty> {
        let dotted_member = format!("{}::{method}", head.canonical_type);
        if self.fn_sigs.contains_key(&dotted_member) {
            let function = (Expr::Identifier(dotted_member.clone()), head.span.clone());
            let result = self.check_call(&function, head.type_args.as_deref(), args, span);
            let call_key = SpanKey::in_module(span, self.current_module_idx);
            if let Some(target) = self.direct_call_targets.get(&call_key).cloned() {
                self.record_method_call_rewrite(
                    span,
                    MethodCallRewrite::RewriteModuleQualifiedToFunction {
                        target,
                        c_symbol: dotted_member,
                        elem_ty: None,
                    },
                );
            }
            return Some(result);
        }

        let type_def = self.type_defs.get(&head.canonical_type).cloned()?;
        let raw_sig = type_def.methods.get(method).cloned()?;
        let (sig, explicit_owner_args) = if let Some(type_args) = head.type_args.as_deref() {
            if type_args.len() != type_def.type_params.len() {
                self.report_error(
                    TypeErrorKind::ArityMismatch,
                    span,
                    format!(
                        "type `{}` has {} type parameter(s) but {} type argument(s) were supplied",
                        head.canonical_type,
                        type_def.type_params.len(),
                        type_args.len()
                    ),
                );
            }
            let owner_args = type_args
                .iter()
                .map(|type_arg| self.resolve_type_expr(type_arg))
                .collect::<Vec<_>>();
            (
                instantiate_stdlib_method_sig(&raw_sig, &type_def.type_params, &owner_args),
                owner_args,
            )
        } else {
            (raw_sig, Vec::new())
        };
        let applied = self.apply_instantiated_call_signature(
            &sig,
            None,
            args,
            span,
            SignatureArgApplication::PositionalOnly {
                arity_context: format!("associated function `{method}`"),
            },
            true,
        );
        if !explicit_owner_args.is_empty() {
            self.record_concrete_call_type_args(span, &explicit_owner_args);
        }
        let declaration = self
            .impl_method_declaration_ids
            .get(&dotted_member)
            .cloned()
            .map_or_else(
                || CallTarget::Unsupported {
                    reason: format!(
                        "associated function `{dotted_member}` has no registered declaration identity"
                    ),
                },
                CallTarget::impl_method,
            );
        self.record_method_call_rewrite(
            span,
            MethodCallRewrite::RewriteModuleQualifiedToFunction {
                target: declaration,
                c_symbol: dotted_member,
                elem_ty: None,
            },
        );
        Some(self.qualify_method_return_to_receiver_owner(
            &head.canonical_type,
            &self.subst.resolve(&applied.return_type),
        ))
    }
}
