#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;

impl Checker {
    /// Emit warnings for functions that are never called (dead code).
    /// Uses BFS reachability from entry points: main, actor handlers, and
    /// underscore-prefixed functions.
    pub(super) fn emit_dead_code_warnings(&mut self) {
        // A REPL fragment's helpers are routinely defined on one input and
        // called on a later one, so whole-program reachability would flag
        // them as dead. Suppress the lint for eval fragments.
        if self.repl_fragment {
            return;
        }

        let mut reachable = HashSet::new();
        let mut queue = std::collections::VecDeque::new();

        for fn_name in self.fn_def_spans.keys() {
            if fn_name == "main" || fn_name.contains("::") || fn_name.starts_with('_') {
                reachable.insert(fn_name.clone());
                queue.push_back(fn_name.clone());
            }
        }

        for caller in self.call_graph.keys() {
            if caller.contains("::") && reachable.insert(caller.clone()) {
                queue.push_back(caller.clone());
            }
        }

        while let Some(caller) = queue.pop_front() {
            if let Some(callees) = self.call_graph.get(&caller) {
                for callee in callees {
                    if reachable.insert(callee.clone()) {
                        queue.push_back(callee.clone());
                    }
                }
            }
        }

        let mut findings: Vec<(Span, String, Option<String>)> = Vec::new();
        for (fn_name, (def_span, stored_module)) in &self.fn_def_spans {
            if fn_name == "main" || fn_name.starts_with('_') || fn_name.contains("::") {
                continue;
            }
            if fn_name.starts_with("std.") || fn_name.contains('.') {
                continue;
            }
            if reachable.contains(fn_name) {
                continue;
            }
            findings.push((def_span.clone(), fn_name.clone(), stored_module.clone()));
        }
        // Route through the lint registry so `dead_code` is configurable
        // (`-A/-W/-D`) and suppressible (`// hew:allow(dead_code)`). The
        // collect-then-emit split releases the `&self.fn_def_spans` borrow
        // before the `&mut self` emit calls. Default level is `Warn`, so the
        // warning still appears exactly as before unless reconfigured.
        for (def_span, fn_name, stored_module) in findings {
            self.emit_main_pass_lint(
                LintId::DeadCode,
                &def_span,
                stored_module.as_deref(),
                format!("function `{fn_name}` is never called"),
                format!("if this is intentional, prefix with underscore: `_{fn_name}`"),
            );
        }
    }

    pub(super) fn warn_wasm_limitation(&mut self, span: &Span, feature: WasmUnsupportedFeature) {
        if !self.wasm_target {
            return;
        }
        let key = (SpanKey::in_module(span, self.current_module_idx), feature);
        if !self.wasm_warning_spans.insert(key) {
            return;
        }
        self.warnings.push(TypeError {
            severity: crate::error::Severity::Warning,
            kind: TypeErrorKind::PlatformLimitation,
            span: span.clone(),
            message: format!(
                "{} are not supported on WASM32 — {}",
                feature.label(),
                feature.reason()
            ),
            notes: vec![],
            suggestions: vec![
                "consider using basic actors (spawn/send/ask) which work on WASM".to_string(),
            ],
            source_module: self.current_module.clone(),
        });
    }

    /// Emit a compile-time **error** for a WASM-incompatible feature.
    ///
    /// Used for features whose runtime support is absent on wasm32, either
    /// because the runtime stubs `unreachable!`-trap or because the native-only
    /// modules are not compiled at all. Unlike [`warn_wasm_limitation`], this
    /// makes the program fail at check time rather than silently compiling to an
    /// unhelpful trap or linker failure.
    ///
    /// See `docs/wasm-capability-matrix.md` for the full disposition table.
    pub(super) fn reject_wasm_feature(&mut self, span: &Span, feature: WasmUnsupportedFeature) {
        if !self.wasm_target {
            return;
        }
        let key = (SpanKey::in_module(span, self.current_module_idx), feature);
        if !self.wasm_reject_spans.insert(key) {
            return;
        }
        self.errors.push(TypeError {
            severity: crate::error::Severity::Error,
            kind: TypeErrorKind::PlatformLimitation,
            span: span.clone(),
            message: format!(
                "{} are not supported on WASM32 — {}",
                feature.label(),
                feature.reason()
            ),
            notes: vec![],
            suggestions: vec![
                "see docs/wasm-capability-matrix.md for the capability tier table".to_string(),
                "consider using basic actors (spawn/send/ask) which work on WASM".to_string(),
            ],
            source_module: self.current_module.clone(),
        });
    }

    pub(super) fn report_error(&mut self, kind: TypeErrorKind, span: &Span, message: String) {
        self.errors.push(TypeError {
            severity: crate::error::Severity::Error,
            kind,
            span: span.clone(),
            message,
            notes: vec![],
            suggestions: vec![],
            source_module: self.current_module.clone(),
        });
    }

    pub(super) fn report_error_with_note(
        &mut self,
        kind: TypeErrorKind,
        span: &Span,
        message: String,
        note_span: &Span,
        note: String,
    ) {
        self.errors.push(TypeError {
            severity: crate::error::Severity::Error,
            kind,
            span: span.clone(),
            message,
            notes: vec![(note_span.clone(), note)],
            suggestions: vec![],
            source_module: self.current_module.clone(),
        });
    }

    pub(super) fn check_arity(
        &mut self,
        args: &[CallArg],
        expected: usize,
        context: &str,
        span: &Span,
    ) -> bool {
        if args.len() == expected {
            return true;
        }
        self.report_error(
            TypeErrorKind::ArityMismatch,
            span,
            format!(
                "{context} takes {expected} argument(s) but {} were supplied",
                args.len()
            ),
        );
        false
    }

    pub(super) fn report_error_with_suggestions(
        &mut self,
        kind: TypeErrorKind,
        span: &Span,
        message: String,
        suggestions: Vec<String>,
    ) {
        self.errors.push(TypeError {
            severity: crate::error::Severity::Error,
            kind,
            span: span.clone(),
            message,
            notes: vec![],
            suggestions,
            source_module: self.current_module.clone(),
        });
    }

    /// Check if a variable binding would shadow an existing variable.
    ///
    /// - **Same-scope rebinding**: hard error (variable already defined in this scope).
    /// - **Outer-scope shadowing of a synthetic binding** (e.g. actor field): hard error,
    ///   because bare field access requires unambiguous names.
    /// - **Outer-scope shadowing of a user-defined local variable**: warning, so the
    ///   programmer is informed but the program is not rejected.
    ///
    /// Bindings with an underscore prefix and for-loop induction variables are always exempt.
    pub(super) fn check_shadowing(&mut self, name: &str, span: &Span) {
        if name.starts_with('_') || self.in_for_binding {
            return;
        }

        // Same-scope rebinding: hard error, but only for user-visible bindings
        // (those with a source span). Synthetic bindings (def_span = None) are
        // pre-populated by the actor-forward-bind mechanism and are expected to
        // be overwritten by the real `define_with_span` call without an error.
        if let Some(Some(prev_span)) = self.env.find_in_current_scope(name) {
            self.errors.push(TypeError {
                severity: crate::error::Severity::Error,
                kind: TypeErrorKind::Shadowing,
                span: span.clone(),
                message: format!("variable `{name}` is already defined in this scope"),
                notes: vec![(prev_span, "previously defined here".to_string())],
                suggestions: vec![format!(
                    "choose a different name, or prefix with underscore: `_{name}`"
                )],
                source_module: self.current_module.clone(),
            });
            return;
        }

        // Outer-scope shadowing — severity depends on what is being shadowed.
        if let Some(prev_span) = self.env.find_in_outer_scope(name) {
            match prev_span {
                None => {
                    // Synthetic binding (actor field): hard error — bare field access would
                    // be ambiguous if a local could shadow a field name.
                    self.errors.push(TypeError {
                        severity: crate::error::Severity::Error,
                        kind: TypeErrorKind::Shadowing,
                        span: span.clone(),
                        message: format!("variable `{name}` shadows a binding in an outer scope"),
                        notes: vec![],
                        suggestions: vec![format!(
                            "choose a different name, or prefix with underscore: `_{name}`"
                        )],
                        source_module: self.current_module.clone(),
                    });
                }
                Some(prev) => {
                    // User-defined local variable in an outer scope: warning.
                    // Shadowing is confusing but not ambiguous in the same way as actor fields.
                    self.warnings.push(TypeError {
                        severity: crate::error::Severity::Warning,
                        kind: TypeErrorKind::Shadowing,
                        span: span.clone(),
                        message: format!(
                            "variable `{name}` shadows a binding in an outer scope"
                        ),
                        notes: vec![(prev, "previously defined here".to_string())],
                        suggestions: vec![format!(
                            "consider a more descriptive name, or prefix with underscore to suppress: `_{name}`"
                        )],
                        source_module: self.current_module.clone(),
                    });
                }
            }
        }
    }

    /// Pop the current scope and emit warnings for unused/unmutated bindings.
    pub(super) fn emit_scope_warnings(&mut self) {
        use crate::env::ScopeWarningKind;
        // Pop unconditionally so env scope state stays consistent, then decide
        // whether to surface the warnings: a REPL fragment's bindings are
        // intentionally carried across inputs, so one unused (or never-yet
        // reassigned) within a single accumulated fragment is expected, not a
        // defect. Suppress the per-binding lints for eval fragments.
        let scope_warnings = self.env.pop_scope_with_warnings();
        if self.repl_fragment || self.is_stdlib_source {
            return;
        }
        for w in scope_warnings {
            match w.kind {
                ScopeWarningKind::Unused => {
                    // A RAII handle (`#[resource]` / `#[linear]` / an owned
                    // stdlib handle) is bound for the express purpose of being
                    // dropped or consumed: the scope-exit drop IS the use, and
                    // an unconsumed linear binding already raises a hard
                    // MustConsume error. Nagging "prefix with `_`" here is a
                    // false positive, so suppress the unused-binding lint for it.
                    if self.is_raii_handle_ty(&w.ty) {
                        continue;
                    }
                    self.warnings.push(TypeError {
                        severity: crate::error::Severity::Warning,
                        kind: TypeErrorKind::UnusedVariable,
                        span: w.span,
                        message: format!("unused variable `{}`", w.name),
                        notes: vec![],
                        suggestions: vec![format!("prefix with underscore: `_{}`", w.name)],
                        source_module: self.current_module.clone(),
                    });
                }
                ScopeWarningKind::NeverMutated => {
                    self.warnings.push(TypeError {
                        severity: crate::error::Severity::Warning,
                        kind: TypeErrorKind::UnusedMut,
                        span: w.span,
                        message: format!(
                            "variable `{}` is declared mutable but never reassigned",
                            w.name
                        ),
                        notes: vec![],
                        suggestions: vec!["use `let` instead of `var`".to_string()],
                        source_module: self.current_module.clone(),
                    });
                }
            }
        }
    }

    /// Whether a binding of type `ty` is a RAII handle whose entire purpose is
    /// drop / consume — a `#[resource]` or `#[linear]` user type, or an owned
    /// stdlib/builtin handle (drop type / handle type). Used to suppress the
    /// unused-binding lint: such a binding is meaningful even when never read.
    fn is_raii_handle_ty(&self, ty: &Ty) -> bool {
        let Ty::Named { name, .. } = ty else {
            return false;
        };
        self.registry.is_resource(name)
            || self.registry.is_linear(name)
            || self.canonical_owned_handle_type_name(name).is_some()
    }

    #[expect(
        clippy::too_many_lines,
        reason = "associated type resolution requires many cases"
    )]
    pub(super) fn check_exhaustiveness(
        &mut self,
        scrutinee_ty: &Ty,
        arms: &[MatchArm],
        span: &Span,
    ) {
        fn visit_or_patterns<'a, F: FnMut(&'a Pattern)>(pattern: &'a Pattern, f: &mut F) {
            match pattern {
                Pattern::Or(left, right) => {
                    visit_or_patterns(&left.0, f);
                    visit_or_patterns(&right.0, f);
                }
                _ => f(pattern),
            }
        }

        if matches!(scrutinee_ty, Ty::Error) {
            return;
        }
        if self.has_unsupported_payload_subpattern_error_for_arms(arms) {
            return;
        }
        let scrutinee_ty = self.subst.resolve(scrutinee_ty);
        let scrutinee_ty = &scrutinee_ty;

        let mut has_wildcard = false;
        for arm in arms {
            if arm.guard.is_some() {
                continue;
            }
            visit_or_patterns(&arm.pattern.0, &mut |pattern| {
                if matches!(pattern, Pattern::Wildcard) {
                    has_wildcard = true;
                }
            });
            if has_wildcard {
                return;
            }
        }

        match scrutinee_ty {
            Ty::Named {
                builtin: Some(crate::BuiltinType::Option | crate::BuiltinType::Result),
                ..
            } => {
                // Refutability-aware coverage: an arm whose payload subpattern
                // is refutable (a literal predicate `Some(0)` or a nested
                // constructor `Err(IoError::NotFound)`) does not by itself
                // cover its variant; `variant_covered` recurses into nested
                // payload patterns so a jointly-exhaustive set of nested arms
                // still counts.
                //
                // Use the resolution-aware `is_catch_all_for_scrutinee` so
                // that a bare unqualified identifier (even uppercase) that does
                // not resolve as a variant of the scrutinee type is treated as
                // a binding catch-all rather than a failed constructor.
                let leaves = Self::unguarded_leaf_patterns(arms);
                if leaves
                    .iter()
                    .any(|p| self.is_catch_all_for_scrutinee(p, scrutinee_ty))
                {
                    return;
                }
                let Some(variants) = self.enum_variant_payloads(scrutinee_ty) else {
                    return;
                };
                let missing: Vec<String> = variants
                    .iter()
                    .filter(|(name, shape)| !self.variant_covered(&leaves, name, shape))
                    .map(|(name, _)| name.clone())
                    .collect();
                if !missing.is_empty() {
                    self.error_non_exhaustive(span, &missing, |name| match name {
                        "Some" => "Some(_)".to_string(),
                        "Ok" => "Ok(_)".to_string(),
                        "Err" => "Err(_)".to_string(),
                        _ => name.to_string(),
                    });
                }
            }
            Ty::Named { name, .. } => {
                if let Some(td) = self.lookup_type_def(name) {
                    if !td.variants.is_empty() {
                        let leaves = Self::unguarded_leaf_patterns(arms);
                        // A plain lowercase binding (or an identifier that is
                        // not a variant of this enum) is a catch-all arm.
                        let is_catch_all = |pattern: &Pattern| match pattern {
                            Pattern::Wildcard => true,
                            Pattern::Identifier(id) => {
                                let short = id.rsplit("::").next().unwrap_or(id);
                                !td.variants.contains_key(short)
                            }
                            _ => false,
                        };
                        if leaves.iter().any(|p| is_catch_all(p)) {
                            return;
                        }
                        let Some(variants) = self.enum_variant_payloads(scrutinee_ty) else {
                            return;
                        };
                        let mut missing_names: Vec<String> = variants
                            .iter()
                            .filter(|(vname, shape)| !self.variant_covered(&leaves, vname, shape))
                            .map(|(vname, _)| vname.clone())
                            .collect();
                        if !missing_names.is_empty() {
                            missing_names.sort();
                            self.error_non_exhaustive(span, &missing_names, |variant_name| {
                                td.variants.get(variant_name).map_or_else(
                                    || variant_name.to_string(),
                                    |variant| missing_arm_pattern(variant_name, variant),
                                )
                            });
                        }
                    }
                }
            }
            Ty::Bool => {
                let mut has_binding_identifier = false;
                let mut has_true = false;
                let mut has_false = false;
                for arm in arms {
                    if arm.guard.is_some() {
                        continue;
                    }
                    visit_or_patterns(&arm.pattern.0, &mut |pattern| match pattern {
                        Pattern::Identifier(_) => {
                            has_binding_identifier = true;
                        }
                        Pattern::Literal(Literal::Bool(true)) => {
                            has_true = true;
                        }
                        Pattern::Literal(Literal::Bool(false)) => {
                            has_false = true;
                        }
                        _ => {}
                    });
                }
                if !has_binding_identifier {
                    let mut missing = Vec::new();
                    if !has_true {
                        missing.push("true".to_string());
                    }
                    if !has_false {
                        missing.push("false".to_string());
                    }
                    if !missing.is_empty() {
                        self.error_non_exhaustive(span, &missing, std::string::ToString::to_string);
                    }
                }
            }
            Ty::Tuple(items) => {
                let mut has_binding_identifier = false;
                let mut has_full_tuple_project = false;
                for arm in arms {
                    if arm.guard.is_some() {
                        continue;
                    }
                    visit_or_patterns(&arm.pattern.0, &mut |pattern| match pattern {
                        Pattern::Identifier(_) => {
                            has_binding_identifier = true;
                        }
                        Pattern::Tuple(pats) if pats.len() == items.len() => {
                            has_full_tuple_project = true;
                        }
                        _ => {}
                    });
                    if has_binding_identifier || has_full_tuple_project {
                        return;
                    }
                }
                self.warn_non_exhaustive(span, "consider adding a wildcard `_` arm");
            }
            _ => {
                // For non-enum types (int, float, string, etc.), check for catch-all patterns.
                let mut has_catch_all = false;
                let mut has_literal_arm = false;
                for arm in arms {
                    if arm.guard.is_some() {
                        continue;
                    }
                    visit_or_patterns(&arm.pattern.0, &mut |pattern| match pattern {
                        Pattern::Identifier(_) => {
                            has_catch_all = true;
                        }
                        Pattern::Literal(_) => {
                            has_literal_arm = true;
                        }
                        _ => {}
                    });
                    if has_catch_all {
                        break;
                    }
                }
                if !has_catch_all {
                    if has_literal_arm
                        && matches!(
                            scrutinee_ty,
                            Ty::I8
                                | Ty::I16
                                | Ty::I32
                                | Ty::I64
                                | Ty::U8
                                | Ty::U16
                                | Ty::U32
                                | Ty::U64
                                | Ty::Isize
                                | Ty::Usize
                                | Ty::IntLiteral
                                | Ty::Char
                                | Ty::String
                        )
                    {
                        self.error_non_exhaustive(span, &["_".to_string()], |_| "_".to_string());
                    } else {
                        self.warn_non_exhaustive(span, "consider adding a wildcard `_` arm");
                    }
                }
            }
        }
    }

    fn has_unsupported_payload_subpattern_error_for_arms(&self, arms: &[MatchArm]) -> bool {
        self.errors.iter().any(|error| {
            error.source_module == self.current_module
                && matches!(
                    error.kind,
                    crate::error::TypeErrorKind::UnsupportedPayloadSubpattern { .. }
                )
                && arms.iter().any(|arm| {
                    error.span.start >= arm.pattern.1.start && error.span.end <= arm.pattern.1.end
                })
        })
    }

    /// Emit a hard error for genuinely non-exhaustive enum-like matches (Option, Result,
    /// user enums, machines, bool).  These are fail-closed: missing variants are a
    /// correctness issue, not just a style suggestion.
    pub(super) fn error_non_exhaustive<F>(
        &mut self,
        span: &Span,
        missing: &[String],
        suggestion_pattern: F,
    ) where
        F: Fn(&str) -> String,
    {
        let detail = if missing.is_empty() {
            "missing some patterns".to_string()
        } else {
            format!("missing {}", missing.join(", "))
        };
        let mut error = TypeError::non_exhaustive_match_detail(
            span.clone(),
            crate::error::Severity::Error,
            detail,
        );
        for variant in missing {
            error = error.with_suggestion(suggestion_pattern(variant));
        }
        self.errors.push(error);
    }

    /// Emit a soft warning for scalar / open-ended types where adding a wildcard `_`
    /// arm is a style suggestion rather than a correctness requirement.
    pub(super) fn warn_non_exhaustive(&mut self, span: &Span, detail: &str) {
        self.warnings.push(TypeError::non_exhaustive_match_detail(
            span.clone(),
            crate::error::Severity::Warning,
            detail,
        ));
    }

    /// Collect the leaf patterns of every unguarded arm, flattening
    /// or-patterns. Guarded arms never contribute to exhaustiveness.
    pub(super) fn unguarded_leaf_patterns(arms: &[MatchArm]) -> Vec<&Pattern> {
        fn visit<'pattern>(pattern: &'pattern Pattern, out: &mut Vec<&'pattern Pattern>) {
            match pattern {
                Pattern::Or(left, right) => {
                    visit(&left.0, out);
                    visit(&right.0, out);
                }
                _ => out.push(pattern),
            }
        }
        let mut leaves = Vec::new();
        for arm in arms {
            if arm.guard.is_some() {
                continue;
            }
            visit(&arm.pattern.0, &mut leaves);
        }
        leaves
    }

    /// Validate top-level `if let` / `while let` patterns against current
    /// codegen support, and reject only unsupported kinds.
    ///
    /// Currently supported at the top level: `Literal`, `Constructor`,
    /// `Struct`, `Tuple`, `Or`, `Wildcard`, and `Identifier` (this method
    /// returns `false` for these, i.e., no rejection).
    ///
    /// Returns `true` only when a pattern is rejected (and an error is emitted).
    pub(super) fn reject_unsupported_iflet_pattern(
        &mut self,
        pattern: &Pattern,
        span: &Span,
    ) -> bool {
        let kind_name = match pattern {
            Pattern::Regex { .. } => "regex",
            Pattern::RecordShorthand { .. } => "record shorthand",
            Pattern::Wildcard
            | Pattern::Identifier(_)
            | Pattern::Literal(_)
            | Pattern::Constructor { .. }
            | Pattern::Struct { .. }
            | Pattern::Tuple(_)
            | Pattern::Or(..) => {
                return false;
            }
        };
        self.report_error(
            TypeErrorKind::InvalidOperation,
            span,
            format!(
                "{kind_name} pattern is unsupported at the top level of `if let` / `while let`; use a `match` expression instead"
            ),
        );
        true
    }
}

fn missing_arm_pattern(variant_name: &str, variant: &VariantDef) -> String {
    match variant {
        VariantDef::Unit => variant_name.to_string(),
        VariantDef::Tuple(fields) => {
            let wildcards = std::iter::repeat_n("_", fields.len())
                .collect::<Vec<_>>()
                .join(", ");
            format!("{variant_name}({wildcards})")
        }
        VariantDef::Struct(_) => format!("{variant_name} {{ .. }}"),
    }
}
