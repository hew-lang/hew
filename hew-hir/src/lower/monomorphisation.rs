//! Monomorphisation and record-layout recording.

use super::*;

impl LowerCtx {
    pub(super) fn record_var_self_direct_monomorphisation(
        &mut self,
        callee: &str,
        receiver_ty: &ResolvedTy,
        call_span: &std::ops::Range<usize>,
        call_site: SiteId,
    ) {
        let declaration = self.impl_method_declaration_ids.get(callee).cloned();
        let Some((origin, type_param_count, linkage, declaration)) = self
            .fn_registry
            .get(callee)
            .map(|entry| (entry.id, entry.type_params.len(), entry.linkage))
            .zip(declaration)
            .map(|((origin, type_param_count, linkage), declaration)| {
                (origin, type_param_count, linkage, declaration)
            })
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
                        call_args.push(self.qualify_current_module_record_ty(resolved));
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
        self.call_site_type_args
            .entry(call_site)
            .or_insert_with(|| type_args.clone());
        if type_args
            .iter()
            .any(|t| self.contains_abstract_type_param(t))
        {
            return;
        }
        let mono_key = MonoKey {
            origin,
            declaration,
            linker_symbol: callee.to_string(),
            type_args,
        };
        match self.mono_registry.insert(mono_key) {
            Ok(_) => {}
            Err(()) => {
                if !self.mono_cap_diag_emitted {
                    self.mono_cap_diag_emitted = true;
                    let cap = self.mono_registry.cap();
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::MonomorphisationCapExceeded { cap },
                        call_span.clone(),
                        "too many distinct generic-function instantiations; the \
                         compiler refuses to monomorphise beyond the configured \
                         cap (suspect polymorphic recursion or runaway inference)",
                    ));
                }
            }
        }
    }

    pub(super) fn record_monomorphisation(
        &mut self,
        callee_expr: &hew_parser::ast::Expr,
        call_span: &std::ops::Range<usize>,
        call_site: SiteId,
    ) {
        let Expr::Identifier(name) = callee_expr else {
            // Only direct-name callees are candidates here. A `module.fn(...)`
            // direct call parses as `Expr::MethodCall`, not `Expr::Call`, so it
            // never reaches `lower_regular_call`/this site; its
            // monomorphisation is registered by the
            // `RewriteModuleQualifiedToFunction` arm of `lower_method_call`,
            // which calls `register_free_fn_monomorphisation` with the mangled
            // qualified symbol. Indirect calls through bindings and other
            // complex callee expressions are out of scope.
            return;
        };
        let registry_name = if self.lookup(name).is_none() {
            self.resolved_bare_function_symbol(name)
                .unwrap_or_else(|| name.clone())
        } else {
            name.clone()
        };
        self.register_free_fn_monomorphisation(&registry_name, None, call_span, call_site);
    }

    /// Diagnose a poisoned checker `call_type_args` entry before the generic
    /// missing-target boundary can mask it.
    ///
    /// This lookup is diagnostic-only: it uses the source spelling solely to
    /// identify the generic declaration whose checker side-table entry is
    /// corrupt. It never manufactures an executable [`CallTarget`]; the caller
    /// still lowers the expression as unsupported when the canonical target is
    /// absent. Valid entries continue through the ordinary strict target gate.
    pub(super) fn diagnose_poisoned_direct_call_type_args(
        &mut self,
        callee_expr: &hew_parser::ast::Expr,
        call_span: &std::ops::Range<usize>,
    ) -> bool {
        let Expr::Identifier(name) = callee_expr else {
            return false;
        };
        let registry_name = if self.lookup(name).is_none() {
            self.resolved_bare_function_symbol(name)
                .unwrap_or_else(|| name.clone())
        } else {
            name.clone()
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

    /// Register the per-instantiation monomorphisation of a generic top-level
    /// free function whose callee resolved to `registry_name` in `fn_registry`.
    ///
    /// This is the single authority both direct-call callee shapes feed:
    /// - a bare `Expr::Identifier` callee (`first([1,2,3])`,
    ///   `helper([1,2,3])` after import rewrite) via `record_monomorphisation`;
    /// - a module-qualified `module.fn([1,2,3])` callee (which parses as
    ///   `Expr::MethodCall` → `RewriteModuleQualifiedToFunction`) with the
    ///   mangled qualified symbol (`module$fn`).
    ///
    /// `registry_name` is the un-mono symbol the emitted `BindingRef.name`
    /// carries; MIR re-mangles it with `call_site_type_args[call_site]`, so
    /// `MonoKey.linker_symbol == registry_name` keeps the registry's
    /// `mangled_name` and the MIR-side mangling spine identical.
    ///
    /// Reads type args exclusively from the checker side-table
    /// (`call_type_args` at `mk_key(call_span)`) — never re-inferred from the
    /// AST (LESSONS `checker-authority`) — and fails closed
    /// (`MonomorphisationCallTypeArgsViolation`) on a poisoned boundary
    /// conversion. No-op for non-generic, catalog-linkage, or unregistered
    /// callees, and for sites the checker did not record.
    pub(super) fn register_free_fn_monomorphisation(
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
        let builtin_family = entry.builtin_family;
        let origin = entry.id;
        let linker_symbol = registry_name.to_string();
        let key = self.mk_key(call_span);
        // Dot-qualified source calls carry their exact `User(DefId)` in the
        // module-call rewrite, not in the ordinary-call target table. Thread
        // that checker-selected declaration through instead of trying to
        // recover it from the linker spelling. Bare direct calls continue to
        // read the ordinary target side table.
        let Some(declaration) = selected_declaration
            .cloned()
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
            // Both cases are non-monomorphisable here; skip the registry entry.
            return;
        };
        let mut type_args: Vec<ResolvedTy> = Vec::with_capacity(type_args_raw.len());
        for ty in &type_args_raw {
            match ResolvedTy::from_ty(ty) {
                Ok(resolved) => type_args.push(self.qualify_current_module_record_ty(resolved)),
                Err(err) => {
                    // Fail-closed: poisoned side-table for this call.
                    // Emit a diagnostic; skip the registry entry so the
                    // downstream MIR/LLVM emit doesn't reach an
                    // unresolved type. (LESSONS: checker-output-boundary P0)
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
        // Record the per-call-site type arguments unconditionally —
        // including "still-abstract" entries where the args reference
        // the enclosing fn's type-parameter symbols. MIR lowering of a
        // specialised body substitutes those symbols via its
        // per-monomorphisation substitution map; the registry's
        // closure-under-substitution pass uses the same data to
        // discover inner monomorphisations.
        self.call_site_type_args
            .insert(call_site, type_args.clone());
        // A generic synthetic builtin (`link_remote<T>`)
        // has a `fn_registry` entry for arity + type-param resolution but no
        // AST `fn` body. MIR lowers it through the runtime-call path
        // (`runtime_symbol_for_call_expr` reads the C symbol off the
        // `builtin_family` catalog bijection), never as a monomorphised body.
        // Recording a registry entry here would mint a `MonoKey` whose origin
        // `ItemId` is absent from `module.items`, so the MIR monomorphisation
        // loop fails closed with "unknown origin fn id" for every concrete
        // instantiation. The per-call-site type args recorded above are the
        // only data downstream needs (codegen reads element layout from
        // `call_site_type_args`); skip the body-monomorphisation entry.
        if builtin_family.is_some() {
            return;
        }
        // Skip "still-abstract" entries from the registry — call sites
        // inside a generic body where the type arg is `T` (the
        // surrounding fn's own type-param symbol) rather than a
        // concrete substitution. These appear with
        // `ResolvedTy::Named { name: "T", args: [] }` because the
        // checker treats unbound type params as opaque named types for
        // body-checking purposes. MIR's monomorphisation pass re-walks
        // these sites with concrete args via substitution.
        if type_args
            .iter()
            .any(|t| self.contains_abstract_type_param(t))
        {
            return;
        }
        let mono_key = MonoKey {
            origin,
            declaration,
            linker_symbol,
            type_args,
        };
        match self.mono_registry.insert(mono_key) {
            Ok(_) => {}
            Err(()) => {
                if !self.mono_cap_diag_emitted {
                    self.mono_cap_diag_emitted = true;
                    let cap = self.mono_registry.cap();
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::MonomorphisationCapExceeded { cap },
                        call_span.clone(),
                        "too many distinct generic-function instantiations; the \
                         compiler refuses to monomorphise beyond the configured \
                         cap (suspect polymorphic recursion or runaway inference)",
                    ));
                }
            }
        }
    }

    /// Try to record a generic record-init site in the record-layout
    /// registry. Mirrors `record_monomorphisation` for the
    /// record-type surface.
    ///
    /// Skips silently when:
    /// - the record is not in `record_registry` (builtin Vec/Option/
    ///   Result/HashMap/Range/channel-handle types are never registered
    ///   because they have no `Item::TypeDecl`/`Item::Record` AST node),
    /// - the record has empty `type_params` (monomorphic record — no
    ///   per-instantiation layout needed; the bare-name layout in MIR
    ///   suffices),
    /// - no `record_init_type_args` entry exists for the site (the
    ///   checker filters out sites whose args still contain inference
    ///   vars; a missing entry means the init is trivially monomorphic
    ///   at the checker's seam).
    ///
    /// Emits `RecordLayoutTypeArgsViolation` when a recorded entry
    /// fails the `ResolvedTy::from_ty` boundary conversion;
    /// `RecursiveGenericTypeUnsupported` when substituted fields name
    /// the same origin record with different concrete args;
    /// `RecordLayoutCapExceeded` (at most once per invocation) when the
    /// registry cap is hit.
    #[allow(
        clippy::too_many_lines,
        reason = "single fail-closed validation pipeline (side-table read + boundary conversion + arity + abstract-skip + recursive-self + cap) kept in one place for ordering clarity"
    )]
    pub(super) fn record_record_layout(
        &mut self,
        record_name: &str,
        init_span: &std::ops::Range<usize>,
    ) -> Option<Vec<ResolvedTy>> {
        let Some(entry) = self.record_registry.get(record_name) else {
            // Builtin or unresolved record — not a user-declared
            // generic type, so it has no record-layout registry entry.
            return None;
        };
        if entry.type_params.is_empty() {
            // Monomorphic record — bare-name layout (handled by MIR's
            // existing record-decl emission) is sufficient.
            return None;
        }
        let origin = entry.id;
        let origin_name = record_name.to_string();
        let type_params = entry.type_params.clone();
        let source_fields = entry.fields.clone();

        let key = self.mk_key(init_span);
        let Some(type_args_raw) = self.record_init_type_args.get(&key).cloned() else {
            // No recorded type args at this init site.  Two legitimate
            // causes: (1) the site failed type-checking and was pruned
            // by `validate_record_init_type_args_output_contract` —
            // `expr_types` will also be missing the span, so a checker
            // error already owns the failure; (2) an explicit type-arg
            // path in the checker did not call
            // `record_concrete_record_init_type_args` — a missed
            // re-record that would silently produce `Named{args:[]}`.
            //
            // Fail-closed: if `expr_types` still carries this span the
            // checker accepted the expression but never wrote the type
            // args.  Surface `RecordLayoutMissing` so the downstream
            // shape is not silently wrong.
            if self.expr_types.contains_key(&key) {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::RecordLayoutMissing {
                        record: origin_name.clone(),
                    },
                    init_span.clone(),
                    "generic record init accepted by the checker but \
                     `record_init_type_args` has no entry for this site; \
                     downstream MIR would see an under-instantiated \
                     Named{args:[]}",
                ));
            }
            return None;
        };

        // Boundary conversion: any leaked inference var, error, or
        // unmaterialised literal is fail-closed per checker-authority.
        let mut type_args: Vec<ResolvedTy> = Vec::with_capacity(type_args_raw.len());
        for ty in &type_args_raw {
            match ResolvedTy::from_ty(ty) {
                Ok(resolved) => type_args.push(self.qualify_current_module_record_ty(resolved)),
                Err(err) => {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::RecordLayoutTypeArgsViolation {
                            record: origin_name.clone(),
                            reason: err.to_string(),
                        },
                        init_span.clone(),
                        "checker-authoritative record_init_type_args entry failed boundary conversion",
                    ));
                    return None;
                }
            }
        }

        // Arity guard: the checker enforces matching arity, but be
        // defensive — substitution panics on a mismatch.
        if type_args.len() != type_params.len() {
            return None;
        }

        // Fail-closed: do NOT *register* an abstract origin-site layout, but DO
        // return the (abstract) type-args so the `StructInit` node's `expr.ty`
        // keeps `Box<T>`. Inside a generic fn body (`Box { value: x }` in
        // `make<T>`) the side-table carries the enclosing fn's `T`, not a
        // concrete arg; registering it would mangle `Box$$T` with a `T`-typed
        // field that leaks to the MIR boundary. The concrete `Box$$i64` layout
        // is discovered post-function-mono by `layout_mono` on the substituted
        // body. The abstract args must still ride on `expr.ty` so MIR's
        // monomorph lowering (`subst_ty` + `mangle`, hew-mir/src/lower.rs:6634)
        // recovers `Box<i64>` → `Box$$i64`. Never fires at a concrete site.
        if type_args
            .iter()
            .any(|t| self.contains_abstract_type_param(t))
        {
            return Some(type_args);
        }

        // Substitute the type-params with the concrete args to produce
        // this layout's field shape.
        let substituted_fields: Vec<(String, ResolvedTy)> = source_fields
            .iter()
            .map(|(fname, fty)| {
                (
                    fname.clone(),
                    substitute_type_params(fty, &type_params, &type_args),
                )
            })
            .collect();

        // Recursive polymorphic-self detection: a substituted field
        // type that names `origin_name` with *different* concrete args
        // than `type_args` is unbounded (each layer demands another
        // distinct layout). Same-arg self-reference is fine — that's
        // an ordinary recursive shape and converges to one layout.
        for (_, fty) in &substituted_fields {
            if contains_recursive_polymorphic_self(fty, &origin_name, &type_args) {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::RecursiveGenericTypeUnsupported {
                        name: origin_name.clone(),
                    },
                    init_span.clone(),
                    "recursive generic type definition: a field substitutes to \
                     a reference to the same record with different concrete \
                     type arguments, which would force unbounded layout \
                     expansion (deferred to v0.6)",
                ));
                return None;
            }
        }

        let returned_type_args = type_args.clone();
        let key = RecordMonoKey {
            origin,
            origin_name,
            type_args,
            symbol_class: crate::mono::SymbolClass::Function,
        };
        if self
            .record_layout_registry
            .insert(key, substituted_fields, init_span.clone())
            .is_err()
        {
            if !self.record_layout_cap_diag_emitted {
                self.record_layout_cap_diag_emitted = true;
                let cap = self.record_layout_registry.cap();
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::RecordLayoutCapExceeded { cap },
                    init_span.clone(),
                    "too many distinct generic-record instantiations; the \
                     compiler refuses to monomorphise beyond the configured \
                     cap (suspect polymorphic recursion or runaway inference)",
                ));
            }
            return None;
        }
        Some(returned_type_args)
    }
}
