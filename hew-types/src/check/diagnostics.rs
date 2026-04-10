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
            self.warnings.push(TypeError {
                severity: crate::error::Severity::Warning,
                kind: TypeErrorKind::DeadCode,
                span: def_span.clone(),
                message: format!("function `{fn_name}` is never called"),
                notes: vec![],
                suggestions: vec![format!(
                    "if this is intentional, prefix with underscore: `_{fn_name}`"
                )],
                source_module: stored_module.clone(),
            });
        }
    }

    pub(super) fn warn_wasm_limitation(&mut self, span: &Span, feature: WasmUnsupportedFeature) {
        if !self.wasm_target {
            return;
        }
        let key = (SpanKey::from(span), feature);
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
                "Consider using basic actors (spawn/send/ask) which work on WASM.".to_string(),
            ],
            source_module: self.current_module.clone(),
        });
    }

    /// Emit a compile-time **error** for a WASM-incompatible feature.
    ///
    /// Used for features whose runtime stubs `unreachable!`-trap on wasm32
    /// (channels, timers, streams).  Unlike [`warn_wasm_limitation`], this
    /// makes the program fail at check time rather than silently compiling to a
    /// program that traps at first use.
    ///
    /// See `docs/wasm-capability-matrix.md` for the full disposition table.
    pub(super) fn reject_wasm_feature(&mut self, span: &Span, feature: WasmUnsupportedFeature) {
        if !self.wasm_target {
            return;
        }
        let key = (SpanKey::from(span), feature);
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
                "See docs/wasm-capability-matrix.md for the capability tier table.".to_string(),
                "Consider using basic actors (spawn/send/ask) which work on WASM.".to_string(),
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

        // Same-scope rebinding: hard error
        if let Some(prev) = self.env.find_in_current_scope(name) {
            let notes = prev
                .map(|s| vec![(s, "previously defined here".to_string())])
                .unwrap_or_default();
            self.errors.push(TypeError {
                severity: crate::error::Severity::Error,
                kind: TypeErrorKind::Shadowing,
                span: span.clone(),
                message: format!("variable `{name}` is already defined in this scope"),
                notes,
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
        for w in self.env.pop_scope_with_warnings() {
            match w.kind {
                ScopeWarningKind::Unused => {
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
            Ty::Named { name, .. } if name == "Option" => {
                let missing = Self::missing_constructor_variants(arms, "Some", "None");
                if !missing.is_empty() {
                    self.error_non_exhaustive(span, &format!("missing {}", missing.join(", ")));
                }
            }
            Ty::Named { name, .. } if name == "Result" => {
                let missing = Self::missing_constructor_variants(arms, "Ok", "Err");
                if !missing.is_empty() {
                    self.error_non_exhaustive(span, &format!("missing {}", missing.join(", ")));
                }
            }
            Ty::Named { name, .. } => {
                if let Some(td) = self.lookup_type_def(name) {
                    if !td.variants.is_empty() {
                        let mut has_binding_identifier = false;
                        let mut covered = Vec::new();
                        for arm in arms {
                            if arm.guard.is_some() {
                                continue;
                            }
                            visit_or_patterns(&arm.pattern.0, &mut |pattern| match pattern {
                                Pattern::Constructor { name, .. }
                                | Pattern::Struct { name, .. } => {
                                    let short = name.rsplit("::").next().unwrap_or(name);
                                    covered.push(short.to_string());
                                }
                                Pattern::Identifier(id) => {
                                    let short = id.rsplit("::").next().unwrap_or(id);
                                    if td.variants.contains_key(short) {
                                        covered.push(short.to_string());
                                    } else {
                                        has_binding_identifier = true;
                                    }
                                }
                                _ => {}
                            });
                        }
                        if has_binding_identifier {
                            return;
                        }
                        let missing: Vec<_> = td
                            .variants
                            .keys()
                            .filter(|v| !covered.contains(*v))
                            .collect();
                        if !missing.is_empty() {
                            let names: Vec<_> = missing.iter().map(|s| s.as_str()).collect();
                            self.error_non_exhaustive(
                                span,
                                &format!("missing {}", names.join(", ")),
                            );
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
                        missing.push("true");
                    }
                    if !has_false {
                        missing.push("false");
                    }
                    if !missing.is_empty() {
                        self.error_non_exhaustive(span, &format!("missing {}", missing.join(", ")));
                    }
                }
            }
            _ => {
                // For non-enum types (int, float, string, etc.), check for catch-all patterns.
                let mut has_catch_all = false;
                for arm in arms {
                    if arm.guard.is_some() {
                        continue;
                    }
                    visit_or_patterns(&arm.pattern.0, &mut |pattern| {
                        if matches!(pattern, Pattern::Identifier(_)) {
                            has_catch_all = true;
                        }
                    });
                    if has_catch_all {
                        break;
                    }
                }
                if !has_catch_all {
                    self.warn_non_exhaustive(span, "consider adding a wildcard `_` arm");
                }
            }
        }
    }

    /// Emit a hard error for genuinely non-exhaustive enum-like matches (Option, Result,
    /// user enums, machines, bool).  These are fail-closed: missing variants are a
    /// correctness issue, not just a style suggestion.
    pub(super) fn error_non_exhaustive(&mut self, span: &Span, detail: &str) {
        self.errors.push(TypeError::non_exhaustive_match_detail(
            span.clone(),
            crate::error::Severity::Error,
            detail,
        ));
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

    /// Return the missing constructor variants for two-variant enum-like matches
    /// (e.g., Some/None, Ok/Err). An identifier binding counts as exhaustive.
    pub(super) fn missing_constructor_variants<'a>(
        arms: &[MatchArm],
        variant_a: &'a str,
        variant_b: &'a str,
    ) -> Vec<&'a str> {
        fn visit_or<'pattern>(pattern: &'pattern Pattern, f: &mut impl FnMut(&'pattern Pattern)) {
            match pattern {
                Pattern::Or(left, right) => {
                    visit_or(&left.0, f);
                    visit_or(&right.0, f);
                }
                _ => f(pattern),
            }
        }

        let mut has_binding_identifier = false;
        let mut has_a = false;
        let mut has_b = false;
        for arm in arms {
            if arm.guard.is_some() {
                continue;
            }
            visit_or(&arm.pattern.0, &mut |pattern| match pattern {
                Pattern::Constructor { name, .. } | Pattern::Struct { name, .. } => {
                    let short = name.rsplit("::").next().unwrap_or(name);
                    if short == variant_a {
                        has_a = true;
                    }
                    if short == variant_b {
                        has_b = true;
                    }
                }
                Pattern::Identifier(name) => {
                    let short = name.rsplit("::").next().unwrap_or(name);
                    if short == variant_a {
                        has_a = true;
                    } else if short == variant_b {
                        has_b = true;
                    } else {
                        has_binding_identifier = true;
                    }
                }
                _ => {}
            });
        }
        if has_binding_identifier {
            return Vec::new();
        }
        let mut missing = Vec::new();
        if !has_a {
            missing.push(variant_a);
        }
        if !has_b {
            missing.push(variant_b);
        }
        missing
    }
}
