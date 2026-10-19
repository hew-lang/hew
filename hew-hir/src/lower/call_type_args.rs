//! Checker-authoritative generic call-site arguments.

use super::*;

impl LowerCtx {
    pub(super) fn record_method_call_type_args(
        &mut self,
        callee: &str,
        target: &CallTarget,
        receiver_ty: &ResolvedTy,
        call_span: &std::ops::Range<usize>,
        call_site: SiteId,
    ) {
        let CallTarget::ImplMethod(_) = target else {
            return;
        };
        let Some((type_param_count, linkage)) = self
            .fn_registry
            .get(callee)
            .map(|entry| (entry.type_params.len(), entry.linkage))
        else {
            return;
        };
        if linkage.is_some() || type_param_count == 0 {
            return;
        }
        let ResolvedTy::Named {
            args: receiver_args,
            ..
        } = receiver_ty
        else {
            return;
        };
        let mut type_args = receiver_args.clone();
        let key = self.mk_key(call_span);
        if let Some(raw_call_args) = self.call_type_args.get(&key).cloned() {
            let mut call_args = Vec::with_capacity(raw_call_args.len());
            for ty in &raw_call_args {
                match ResolvedTy::from_ty(ty) {
                    Ok(resolved) => {
                        call_args.push(self.restore_type_declaration_facts(resolved));
                    }
                    Err(err) => {
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::MonomorphisationCallTypeArgsViolation {
                                callee: callee.to_string(),
                                reason: err.to_string(),
                            },
                            call_span.clone(),
                            "checker-authoritative method call_type_args entry failed boundary conversion",
                        ));
                        return;
                    }
                }
            }
            if call_args.len() == type_param_count {
                type_args = call_args;
            } else {
                type_args.extend(call_args);
            }
        }
        if type_args.len() != type_param_count {
            return;
        }
        self.call_site_type_args.insert(call_site, type_args);
    }

    pub(super) fn record_direct_call_type_args(
        &mut self,
        callee_expr: &Spanned<Expr>,
        call_span: &std::ops::Range<usize>,
        call_site: SiteId,
    ) {
        let Expr::Ident(name) = &callee_expr.0 else {
            // Only direct-name callees are candidates here. A `module.fn(...)`
            // direct call parses as `Expr::MethodCall`, not `Expr::Call`, so it
            // never reaches `lower_regular_call`/this site; its
            // checked type arguments are preserved by the
            // `RewriteModuleQualifiedToFunction` arm of `lower_method_call`,
            // which calls `record_free_fn_call_type_args` with the mangled
            // qualified symbol. Indirect calls through bindings and other
            // complex callee expressions are out of scope.
            return;
        };
        let Some(Resolution::Def(declaration)) =
            self.resolutions.get(&self.mk_key(&callee_expr.1)).copied()
        else {
            return;
        };
        let Some(registry_name) = self.registered_source_function_symbol(declaration) else {
            return;
        };
        if let Some(selected) = self.direct_monomorph_declaration(call_span) {
            if selected != declaration {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::CheckerBoundaryViolation {
                        name: name.to_string(),
                        reason: "callee resolution differs from its checked call target".into(),
                    },
                    call_span.clone(),
                    "generic direct call has conflicting checker declaration identities",
                ));
                return;
            }
        }
        self.record_free_fn_call_type_args(
            &registry_name,
            Some(&declaration),
            call_span,
            call_site,
        );
    }

    /// Diagnose a poisoned checker `call_type_args` entry before the generic
    /// missing-target boundary can mask it.
    ///
    /// This diagnostic reads the exact callee declaration row even if the
    /// executable call target was removed from a malformed checker output.
    /// It never manufactures a replacement [`CallTarget`].
    pub(super) fn diagnose_poisoned_direct_call_type_args(
        &mut self,
        callee_expr: &Spanned<Expr>,
        call_span: &std::ops::Range<usize>,
    ) -> bool {
        let Expr::Ident(_) = &callee_expr.0 else {
            return false;
        };
        let Some(Resolution::Def(declaration)) =
            self.resolutions.get(&self.mk_key(&callee_expr.1)).copied()
        else {
            return false;
        };
        let Some(registry_name) = self.registered_source_function_symbol(declaration) else {
            return false;
        };
        let is_generic_user_fn = self
            .fn_registry
            .get(&registry_name)
            .is_some_and(|entry| entry.linkage.is_none() && !entry.type_params.is_empty());
        if !is_generic_user_fn {
            return false;
        }
        let key = self.mk_key(call_span);
        let Some(type_args) = self.call_type_args.get(&key) else {
            return false;
        };
        let Some(err) = type_args
            .iter()
            .find_map(|ty| ResolvedTy::from_ty(ty).err())
        else {
            return false;
        };
        self.diagnostics.push(HirDiagnostic::new(
            HirDiagnosticKind::MonomorphisationCallTypeArgsViolation {
                callee: registry_name,
                reason: err.to_string(),
            },
            call_span.clone(),
            "checker-authoritative call_type_args entry failed boundary conversion before target publication",
        ));
        true
    }

    /// Preserve the checker's arguments at a generic call or function value.
    /// Both bare and module-qualified callees use this boundary conversion;
    /// SIR binds abstract enclosing parameters when it demands an instance.
    pub(super) fn record_free_fn_call_type_args(
        &mut self,
        registry_name: &str,
        selected_declaration: Option<&hew_types::DefId>,
        call_span: &std::ops::Range<usize>,
        call_site: SiteId,
    ) {
        let Some(entry) = self.fn_registry.get(registry_name) else {
            // Callee is not a top-level user fn — skip (builtin,
            // runtime-symbol, or unresolved). Filtering on `fn_registry`
            // membership inherently excludes runtime-symbol callees
            // since they have no AST `fn` item and therefore no
            // `fn_registry` insertion (`lower_program` first pass at
            // line 79).
            return;
        };
        if entry.linkage.is_some() {
            // Catalog-seeded stdlib functions are already monomorphic HIR
            // targets. They never produce user-function specialisations.
            return;
        }
        if entry.type_params.is_empty() {
            // Non-generic callee — nothing to monomorphise.
            return;
        }
        let linker_symbol = registry_name.to_string();
        let key = self.mk_key(call_span);
        // Dot-qualified source calls carry their exact `User(DefId)` in the
        // module-call rewrite, not in the ordinary-call target table. Thread
        // that checker-selected declaration through instead of trying to
        // recover it from the linker spelling. Bare direct calls continue to
        // read the ordinary target side table.
        let Some(_declaration) = selected_declaration
            .copied()
            .or_else(|| self.direct_monomorph_declaration(call_span))
        else {
            return;
        };
        let Some(type_args_raw) = self.call_type_args.get(&key).cloned() else {
            // Generic callee with no recorded type args at this site.
            // The checker records every generic call site — inferred,
            // return-type-polymorphic, or explicit-turbofish — by snapshotting
            // its resolved type args (`apply_instantiated_call_signature_with_assoc`
            // records unconditionally; `record_concrete_call_type_args` defers,
            // re-resolving the snapshot at the `check_program` output boundary and
            // pruning any entry that is still an inference var there). So the only
            // ways to reach this early return are:
            // (a) the call failed to type-check, or
            // (b) a call whose type parameter never got pinned (neither by an
            //     argument, an enclosing/expected return type, nor a turbofish) —
            //     the output-boundary fail-closed prune correctly excluded it,
            //     and the checker already reported the inference failure.
            // Both cases lack publishable arguments at this boundary.
            return;
        };
        let mut type_args: Vec<ResolvedTy> = Vec::with_capacity(type_args_raw.len());
        for ty in &type_args_raw {
            match ResolvedTy::from_ty(ty) {
                Ok(resolved) => type_args.push(self.restore_type_declaration_facts(resolved)),
                Err(err) => {
                    // Fail-closed: poisoned side-table for this call.
                    // Diagnose before publishing unresolved arguments to SIR.
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::MonomorphisationCallTypeArgsViolation {
                            callee: linker_symbol.clone(),
                            reason: err.to_string(),
                        },
                        call_span.clone(),
                        "checker-authoritative call_type_args entry failed boundary conversion",
                    ));
                    return;
                }
            }
        }
        // Abstract arguments belong to the enclosing template; SIR closes
        // them only when that template is demanded with concrete arguments.
        self.call_site_type_args.insert(call_site, type_args);
    }
}
