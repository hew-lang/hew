#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;

fn collect_pattern_bound_names(pattern: &Pattern) -> HashSet<String> {
    match pattern {
        Pattern::Wildcard | Pattern::Literal(_) | Pattern::Regex { .. } => HashSet::new(),
        Pattern::Identifier(name) => {
            let is_constructor_like =
                name.contains("::") || name.chars().next().is_some_and(char::is_uppercase);
            if is_constructor_like {
                HashSet::new()
            } else {
                HashSet::from([name.clone()])
            }
        }
        Pattern::Constructor { patterns, .. } | Pattern::Tuple(patterns) => patterns
            .iter()
            .flat_map(|pattern| collect_pattern_bound_names(&pattern.0))
            .collect(),
        Pattern::Struct { fields, .. } => fields
            .iter()
            .flat_map(|field| {
                field.pattern.as_ref().map_or_else(
                    || HashSet::from([field.name.clone()]),
                    |(pattern, _)| collect_pattern_bound_names(pattern),
                )
            })
            .collect(),
        Pattern::Or(left, right) => {
            let mut names = collect_pattern_bound_names(&left.0);
            names.extend(collect_pattern_bound_names(&right.0));
            names
        }
    }
}

fn sorted_pattern_bound_names(names: &HashSet<String>) -> Vec<String> {
    let mut names: Vec<_> = names.iter().cloned().collect();
    names.sort();
    names
}

fn substitute_pattern_field_ty(raw_field_ty: &Ty, type_params: &[String], type_args: &[Ty]) -> Ty {
    type_params
        .iter()
        .zip(type_args.iter())
        .fold(raw_field_ty.clone(), |acc, (tp, concrete)| {
            acc.substitute_named_param(tp, concrete)
        })
}

/// Extract the single binding name introduced by a sub-pattern, if any.
///
/// Returns `Some(name)` only for plain lowercase `Identifier` bindings.
/// Returns `None` for wildcards, literals, constructors, structs, tuples,
/// and or-patterns — those either introduce no name, or introduce names
/// that the caller handles recursively.
///
/// This is intentionally shallow (top-level only) so the caller decides
/// whether to recurse into constructor payloads.
fn binding_name_for_pattern(pattern: &Pattern) -> Option<String> {
    match pattern {
        Pattern::Identifier(name) => {
            let is_constructor_like =
                name.contains("::") || name.chars().next().is_some_and(char::is_uppercase);
            if is_constructor_like {
                None
            } else {
                Some(name.clone())
            }
        }
        Pattern::Wildcard
        | Pattern::Literal(_)
        | Pattern::Regex { .. }
        | Pattern::Constructor { .. }
        | Pattern::Struct { .. }
        | Pattern::Tuple(_)
        | Pattern::Or(_, _) => None,
    }
}

/// Classify the payload subpattern kind label for use in
/// `UnsupportedPayloadSubpattern` diagnostics.
///
/// Returns `None` when the subpattern is a supported binding or wildcard.
/// Returns `Some(label)` for unsupported forms that must be rejected.
fn unsupported_payload_subpattern_label(pattern: &Pattern) -> Option<&'static str> {
    match pattern {
        // Plain binding or wildcard — both are supported.
        Pattern::Wildcard => None,
        Pattern::Identifier(name) => {
            let is_constructor_like =
                name.contains("::") || name.chars().next().is_some_and(char::is_uppercase);
            if is_constructor_like {
                // An uppercase/qualified name in payload position is a nested
                // constructor (unit variant), e.g. `Shape::Line(Other::Foo)`.
                Some("nested constructor")
            } else {
                // Plain lowercase identifier — a binding.  Supported.
                None
            }
        }
        Pattern::Literal(_) => Some("literal"),
        Pattern::Constructor { .. } => Some("nested constructor"),
        Pattern::Struct { .. } => Some("struct destructure"),
        // An empty tuple `()` is the unit type — it has only one value and
        // acts as a wildcard (no predicate is needed to test it).  Allow it
        // so that `Ok(())` / `Err(())` patterns remain legal.
        // Non-empty tuples introduce sub-bindings that the substrate does not
        // yet lower; those remain unsupported.
        Pattern::Tuple(pats) if pats.is_empty() => None,
        Pattern::Tuple(_) => Some("tuple destructure"),
        Pattern::Or(_, _) => Some("or-pattern"),
        // Regex literals are only legal as top-level match-arm predicates
        // (scrutinee must be `string`). A regex in payload subpattern
        // position makes no semantic sense; reject as unsupported.
        Pattern::Regex { .. } => Some("regex pattern"),
    }
}

impl Checker {
    fn machine_event_type_outside_transition(&self, ty: &Ty) -> Option<String> {
        let type_name = ty.type_name()?;
        let machine_name = type_name.strip_suffix("Event")?;
        if !self
            .type_defs
            .get(machine_name)
            .is_some_and(|td| td.kind == TypeDefKind::Machine)
        {
            return None;
        }
        if self
            .current_machine_transition
            .as_ref()
            .is_some_and(|(current_machine, _, _)| current_machine == machine_name)
        {
            return None;
        }
        Some(type_name.to_string())
    }

    fn reject_machine_event_pattern_outside_transition(&mut self, ty: &Ty, span: &Span) -> bool {
        let Some(event_type_name) = self.machine_event_type_outside_transition(ty) else {
            return false;
        };
        // Slice 3 exposes companion event nominals for construction and `.step()`;
        // general event-enum matching is reserved for transition-body lowering.
        self.report_error(
            TypeErrorKind::InvalidOperation,
            span,
            format!(
                "matching machine event enum `{event_type_name}` outside a transition body is not supported"
            ),
        );
        true
    }

    pub(super) fn or_pattern_bindings_match(
        &self,
        left_env: &crate::env::TypeEnv,
        right_env: &crate::env::TypeEnv,
        left_names: &HashSet<String>,
        right_names: &HashSet<String>,
    ) -> bool {
        if left_names != right_names {
            return false;
        }
        left_names.iter().all(|name| {
            let left = left_env.lookup_ref(name);
            let right = right_env.lookup_ref(name);
            matches!(
                (left, right),
                (Some(left_binding), Some(right_binding))
                    if self.subst.resolve(&left_binding.ty) == self.subst.resolve(&right_binding.ty)
                        && left_binding.is_mutable == right_binding.is_mutable
            )
        })
    }

    fn bind_struct_field_placeholders(
        &mut self,
        fields: &[hew_parser::ast::PatternField],
        ty: &Ty,
        is_mutable: bool,
        span: &Span,
    ) {
        for pf in fields {
            if let Some((pat, ps)) = &pf.pattern {
                self.bind_pattern(pat, ty, is_mutable, ps);
            } else {
                self.check_shadowing(&pf.name, span);
                self.env
                    .define_with_span(pf.name.clone(), ty.clone(), is_mutable, span.clone());
            }
        }
    }

    /// Pattern binding
    #[expect(
        clippy::too_many_lines,
        reason = "impl method resolution requires many cases"
    )]
    pub(super) fn bind_pattern(
        &mut self,
        pattern: &Pattern,
        ty: &Ty,
        is_mutable: bool,
        span: &Span,
    ) {
        let ty = &self.subst.resolve(ty);
        match pattern {
            Pattern::Wildcard | Pattern::Literal(_) => {}
            Pattern::Identifier(name) => {
                let is_constructor_like =
                    name.contains("::") || name.chars().next().is_some_and(char::is_uppercase);
                if is_constructor_like
                    && self.reject_machine_event_pattern_outside_transition(ty, span)
                {
                    return;
                }
                self.check_shadowing(name, span);
                self.env
                    .define_with_span(name.clone(), ty.clone(), is_mutable, span.clone());
            }
            Pattern::Constructor { name, patterns } => {
                if self.reject_machine_event_pattern_outside_transition(ty, span) {
                    return;
                }
                // Look up variant in enum definition
                if let Some(payload_tys) = self.lookup_variant_types(name, ty, patterns.len()) {
                    for (p, pty) in patterns.iter().zip(payload_tys.iter()) {
                        self.bind_pattern(&p.0, pty, is_mutable, &p.1);
                    }
                } else {
                    match ty {
                        Ty::Named {
                            name: type_name, ..
                        } => {
                            let container_kind =
                                self.lookup_type_def(type_name).map_or("enum", |td| {
                                    if td.kind == TypeDefKind::Machine {
                                        "machine"
                                    } else {
                                        "enum"
                                    }
                                });
                            self.report_error(
                                TypeErrorKind::Mismatch {
                                    expected: type_name.clone(),
                                    actual: name.clone(),
                                },
                                span,
                                format!(
                                    "variant `{name}` is not a member of {container_kind} `{type_name}`"
                                ),
                            );
                        }
                        Ty::Var(_) | Ty::Error => {}
                        _ => {
                            let expected = ty.user_facing().to_string();
                            self.report_error(
                                TypeErrorKind::Mismatch {
                                    expected: expected.clone(),
                                    actual: name.clone(),
                                },
                                span,
                                format!(
                                    "constructor pattern `{name}` cannot match non-enum type `{expected}`"
                                ),
                            );
                        }
                    }
                }
            }
            Pattern::Struct { name, fields } => {
                if self.reject_machine_event_pattern_outside_transition(ty, span) {
                    return;
                }
                // Bind field patterns to field types
                let type_name_opt = ty.type_name();
                if let Some(type_name) = type_name_opt {
                    if let Some(td) = self.lookup_type_def(type_name) {
                        // Strip enum prefix for qualified patterns (e.g. "Shape::Move" → "Move")
                        let short_name = name.rsplit("::").next().unwrap_or(name);
                        if let Some(VariantDef::Struct(variant_fields)) =
                            td.variants.get(short_name).cloned()
                        {
                            // Substitute the scrutinee's concrete type args into field types
                            // so generic enum struct-variants bind with the concrete type.
                            let type_params = td.type_params.clone();
                            let type_args = if let Ty::Named { args, .. } = ty {
                                args.clone()
                            } else {
                                vec![]
                            };

                            for pf in fields {
                                if let Some((_, raw_field_ty)) = variant_fields
                                    .iter()
                                    .find(|(field_name, _)| field_name == &pf.name)
                                {
                                    let field_ty = substitute_pattern_field_ty(
                                        raw_field_ty,
                                        &type_params,
                                        &type_args,
                                    );
                                    if let Some((pat, ps)) = &pf.pattern {
                                        self.bind_pattern(pat, &field_ty, is_mutable, ps);
                                    } else {
                                        self.check_shadowing(&pf.name, span);
                                        self.env.define_with_span(
                                            pf.name.clone(),
                                            field_ty,
                                            is_mutable,
                                            span.clone(),
                                        );
                                    }
                                } else {
                                    let known: Vec<&str> =
                                        variant_fields.iter().map(|(n, _)| n.as_str()).collect();
                                    let similar = crate::error::find_similar(&pf.name, known);
                                    self.report_error_with_suggestions(
                                        TypeErrorKind::UndefinedField,
                                        span,
                                        format!("no field `{}` on variant `{name}`", pf.name),
                                        similar,
                                    );
                                }
                            }
                        } else {
                            let type_params = td.type_params.clone();
                            let type_args = if let Ty::Named { args, .. } = ty {
                                args.clone()
                            } else {
                                vec![]
                            };
                            for pf in fields {
                                if let Some(raw_field_ty) = td.fields.get(&pf.name) {
                                    let field_ty = substitute_pattern_field_ty(
                                        raw_field_ty,
                                        &type_params,
                                        &type_args,
                                    );
                                    if let Some((pat, ps)) = &pf.pattern {
                                        self.bind_pattern(pat, &field_ty, is_mutable, ps);
                                    } else {
                                        self.check_shadowing(&pf.name, span);
                                        self.env.define_with_span(
                                            pf.name.clone(),
                                            field_ty,
                                            is_mutable,
                                            span.clone(),
                                        );
                                    }
                                } else {
                                    let similar = crate::error::find_similar(
                                        &pf.name,
                                        td.fields.keys().map(String::as_str),
                                    );
                                    self.report_error_with_suggestions(
                                        TypeErrorKind::UndefinedField,
                                        span,
                                        format!("no field `{}` on type `{name}`", pf.name),
                                        similar,
                                    );
                                }
                            }
                        }
                    } else {
                        self.report_error(
                            TypeErrorKind::UndefinedType,
                            span,
                            format!(
                                "type `{type_name}` is not defined for struct pattern `{name}`"
                            ),
                        );
                        self.bind_struct_field_placeholders(fields, &Ty::Error, is_mutable, span);
                    }
                } else if matches!(ty, Ty::Var(_) | Ty::Error) {
                    self.bind_struct_field_placeholders(fields, ty, is_mutable, span);
                } else {
                    let expected = ty.user_facing().to_string();
                    self.report_error(
                        TypeErrorKind::Mismatch {
                            expected: expected.clone(),
                            actual: name.clone(),
                        },
                        span,
                        format!(
                            "struct pattern `{name}` cannot match non-struct type `{expected}`"
                        ),
                    );
                    self.bind_struct_field_placeholders(fields, &Ty::Error, is_mutable, span);
                }
            }
            Pattern::Tuple(pats) => match ty {
                // `()` as a pattern (empty tuple) is the unit literal; accept
                // it against unit-typed payloads, e.g. `Ok(())` on
                // `Result<(), E>`.
                Ty::Unit if pats.is_empty() => {}
                Ty::Tuple(tys) => {
                    if pats.len() != tys.len() {
                        self.report_error(
                            TypeErrorKind::ArityMismatch,
                            span,
                            format!(
                                "tuple pattern has {} elements but type has {}",
                                pats.len(),
                                tys.len()
                            ),
                        );
                    }
                    for (p, t) in pats.iter().zip(tys.iter()) {
                        self.bind_pattern(&p.0, t, is_mutable, &p.1);
                    }
                }
                Ty::Var(_) | Ty::Error => {
                    for p in pats {
                        self.bind_pattern(&p.0, &Ty::Error, is_mutable, &p.1);
                    }
                }
                _ => {
                    let expected = ty.user_facing().to_string();
                    self.report_error(
                        TypeErrorKind::Mismatch {
                            expected: expected.clone(),
                            actual: "tuple".to_string(),
                        },
                        span,
                        format!("tuple pattern cannot match non-tuple type `{expected}`"),
                    );
                    for p in pats {
                        self.bind_pattern(&p.0, &Ty::Error, is_mutable, &p.1);
                    }
                }
            },
            // Regex patterns bind named captures as `string` in the arm body.
            // The scrutinee must be `string`; any other type is a hard error.
            // Invalid regex syntax is reported here via InvalidRegexLiteral;
            // Expr::RegexLiteral nodes have an independent validation site in
            // synthesize_inner, but match-arm Pattern::Regex nodes bypass that
            // path entirely and must be validated here.
            Pattern::Regex { pattern, .. } => {
                // Enforce: regex patterns require a `string` scrutinee.
                // `Ty::String` is the canonical string type; `Ty::Var` / `Ty::Error`
                // are in-flight inference variables or already-reported errors — skip
                // the duplicate diagnostic in those cases.
                if !matches!(ty, Ty::String | Ty::Var(_) | Ty::Error) {
                    self.report_error(
                        TypeErrorKind::RegexPatternNotString {
                            actual_ty: ty.user_facing().to_string(),
                        },
                        span,
                        format!(
                            "regex pattern can only match `string` scrutinees, got `{}`",
                            ty.user_facing()
                        ),
                    );
                }
                match regex::Regex::new(pattern) {
                    Ok(compiled) => {
                        for capture_name in compiled.capture_names().flatten() {
                            self.check_shadowing(capture_name, span);
                            self.env.define_with_span(
                                capture_name.to_string(),
                                Ty::Named {
                                    builtin: None,
                                    name: "string".to_string(),
                                    args: vec![],
                                },
                                false,
                                span.clone(),
                            );
                        }
                    }
                    Err(err) => {
                        // Validate the regex pattern; emit InvalidRegexLiteral for
                        // patterns that appear directly in match arms (not via an
                        // Expr::RegexLiteral, which is validated in synthesize_inner).
                        self.report_error(
                            TypeErrorKind::InvalidRegexLiteral {
                                pattern: pattern.clone(),
                                error: err.to_string(),
                            },
                            span,
                            format!("invalid regex pattern `re\"{pattern}\"`: {err}"),
                        );
                    }
                }
            }
            Pattern::Or(a, b) => {
                let left_names = collect_pattern_bound_names(&a.0);
                let right_names = collect_pattern_bound_names(&b.0);
                let env_before = self.env.clone();

                self.bind_pattern(&a.0, ty, is_mutable, &a.1);
                let left_env = self.env.clone();
                self.env = env_before.clone();
                self.bind_pattern(&b.0, ty, is_mutable, &b.1);
                let right_env = self.env.clone();

                if self.or_pattern_bindings_match(&left_env, &right_env, &left_names, &right_names)
                {
                    self.env = left_env;
                } else {
                    self.env = env_before;
                    let mut error = TypeError::or_pattern_binding_mismatch(
                        span.clone(),
                        a.1.clone(),
                        &sorted_pattern_bound_names(&left_names),
                        b.1.clone(),
                        &sorted_pattern_bound_names(&right_names),
                    );
                    if left_names == right_names {
                        for name in sorted_pattern_bound_names(&left_names) {
                            let left_binding = left_env.lookup_ref(&name);
                            let right_binding = right_env.lookup_ref(&name);
                            if let (Some(left_binding), Some(right_binding)) =
                                (left_binding, right_binding)
                            {
                                let left_ty = self.subst.resolve(&left_binding.ty);
                                let right_ty = self.subst.resolve(&right_binding.ty);
                                if left_ty != right_ty {
                                    error = error
                                        .with_note(
                                            a.1.clone(),
                                            format!(
                                                "left branch binds `{name}` as `{}`",
                                                left_ty.user_facing()
                                            ),
                                        )
                                        .with_note(
                                            b.1.clone(),
                                            format!(
                                                "right branch binds `{name}` as `{}`",
                                                right_ty.user_facing()
                                            ),
                                        );
                                }
                                if left_binding.is_mutable != right_binding.is_mutable {
                                    error = error
                                        .with_note(
                                            a.1.clone(),
                                            format!(
                                                "left branch binds `{name}` as {}",
                                                if left_binding.is_mutable {
                                                    "mutable"
                                                } else {
                                                    "immutable"
                                                }
                                            ),
                                        )
                                        .with_note(
                                            b.1.clone(),
                                            format!(
                                                "right branch binds `{name}` as {}",
                                                if right_binding.is_mutable {
                                                    "mutable"
                                                } else {
                                                    "immutable"
                                                }
                                            ),
                                        );
                                }
                            }
                        }
                    }
                    if let Some(module_name) = &self.current_module {
                        error.source_module = Some(module_name.clone());
                    }
                    self.errors.push(error);
                }
            }
        }
    }

    /// Classify an arm pattern into an [`ArmResolution`] and record it in
    /// `pending_pattern_resolutions` keyed by the arm's pattern span.
    ///
    /// Called from both `check_match_stmt` and `check_match_expr` *after*
    /// `bind_pattern` has already accepted the arm (so the scrutinee type is
    /// known and the payload bindings are sound).
    ///
    /// `Pattern::Or` arms are intentionally skipped: or-pattern lowering is a
    /// future lane.  A missing entry for an or-pattern arm must surface a typed
    /// diagnostic downstream, not a silent fallthrough.
    #[expect(
        clippy::too_many_lines,
        reason = "pattern classification requires one branch per AST variant; extraction would hide the shape"
    )]
    pub(super) fn record_arm_resolution(
        &mut self,
        pattern: &Pattern,
        pattern_span: &Span,
        scrutinee_ty: &Ty,
    ) {
        let key = super::types::SpanKey::from(pattern_span);

        let resolution = match pattern {
            Pattern::Wildcard => ArmResolution {
                pattern_kind: PatternKind::Wildcard,
                variant_match: None,
                payload_bindings: vec![],
            },
            Pattern::Literal(_) => ArmResolution {
                pattern_kind: PatternKind::Literal,
                variant_match: None,
                payload_bindings: vec![],
            },
            Pattern::Identifier(name) => {
                // Use the same heuristic as `bind_pattern` to distinguish an
                // uppercase/qualified constructor-as-unit-variant from a
                // plain binding.
                let is_constructor_like =
                    name.contains("::") || name.chars().next().is_some_and(char::is_uppercase);
                if is_constructor_like {
                    // Unit variant written as an identifier (e.g. `None`).
                    let short_name = name.rsplit("::").next().unwrap_or(name);
                    let variant_match = self.resolve_variant_match(short_name, scrutinee_ty, name);
                    ArmResolution {
                        pattern_kind: PatternKind::VariantCtor,
                        variant_match,
                        payload_bindings: vec![],
                    }
                } else {
                    ArmResolution {
                        pattern_kind: PatternKind::Binding,
                        variant_match: None,
                        payload_bindings: vec![],
                    }
                }
            }
            Pattern::Constructor { name, patterns } => {
                let short_name = name.rsplit("::").next().unwrap_or(name);
                let variant_match = self.resolve_variant_match(short_name, scrutinee_ty, name);
                let payload_tys = self
                    .lookup_variant_types(name, scrutinee_ty, patterns.len())
                    .unwrap_or_else(|| vec![Ty::Error; patterns.len()]);
                // Reject payload subpatterns that are not a plain binding or
                // wildcard.  Accepting them silently produces an incorrect
                // wildcard match (the predicate literal/nested-ctor is never
                // compared at runtime).  Nested-predicate lowering is a future
                // substrate lane; the compiler must fail closed here.
                for (sub_pat, sub_span) in patterns {
                    if let Some(label) = unsupported_payload_subpattern_label(sub_pat) {
                        self.report_error_with_note(
                            crate::error::TypeErrorKind::UnsupportedPayloadSubpattern {
                                variant_name: short_name.to_string(),
                                kind_label: label.to_string(),
                            },
                            sub_span,
                            format!(
                                "payload subpattern `{label}` in `{short_name}(...)` is not yet supported"
                            ),
                            pattern_span,
                            "v0.5 payload subpatterns must be a plain binding (`x`) or wildcard (`_`); \
                             literal tests and nested patterns are reserved for a future substrate lane"
                                .to_string(),
                        );
                        return;
                    }
                }
                let payload_bindings: Vec<PayloadBinding> = patterns
                    .iter()
                    .zip(payload_tys.iter())
                    .enumerate()
                    .filter_map(|(field_idx, ((sub_pat, _sub_span), ty))| {
                        binding_name_for_pattern(sub_pat).map(|binding_name| PayloadBinding {
                            field_idx,
                            binding_name,
                            ty: ty.clone(),
                        })
                    })
                    .collect();
                ArmResolution {
                    pattern_kind: PatternKind::VariantCtor,
                    variant_match,
                    payload_bindings,
                }
            }
            Pattern::Struct { name, fields } => {
                // Determine whether this is an enum struct-variant or a plain
                // struct/record pattern.
                let short_name = name.rsplit("::").next().unwrap_or(name);
                let type_name_opt = scrutinee_ty.type_name();
                // variant_match: Some(..) for enum struct-variants, None for plain records.
                // field_tys: name → resolved Ty.
                // field_order: Some(Vec<name>) in source declaration order for enum
                //   struct-variants (so field_idx reflects the Vec position, not
                //   alphabetical sort).  None for plain records (td.fields is a HashMap
                //   so declaration order is not preserved; alphabetical sort is used).
                let (variant_match, field_tys, field_order) = if let Some(type_name) = type_name_opt
                {
                    if let Some(td) = self.lookup_type_def(type_name) {
                        if td.variants.contains_key(short_name) {
                            // Enum struct-variant
                            let vm = VariantMatch {
                                type_name: type_name.to_string(),
                                variant_name: short_name.to_string(),
                            };
                            let (field_ty_map, order) =
                                if let Some(VariantDef::Struct(vf)) = td.variants.get(short_name) {
                                    let type_params = td.type_params.clone();
                                    let type_args = if let Ty::Named { args, .. } = scrutinee_ty {
                                        args.clone()
                                    } else {
                                        vec![]
                                    };
                                    let resolved: Vec<(String, Ty)> = vf
                                        .iter()
                                        .map(|(fname, fty)| {
                                            (
                                                fname.clone(),
                                                substitute_pattern_field_ty(
                                                    fty,
                                                    &type_params,
                                                    &type_args,
                                                ),
                                            )
                                        })
                                        .collect();
                                    let order: Vec<String> =
                                        resolved.iter().map(|(n, _)| n.clone()).collect();
                                    let map: std::collections::HashMap<String, Ty> =
                                        resolved.into_iter().collect();
                                    (map, Some(order))
                                } else {
                                    (std::collections::HashMap::new(), None)
                                };
                            (Some(vm), field_ty_map, order)
                        } else {
                            // Plain record struct — td.fields is a HashMap so
                            // declaration order is not preserved here; fall back
                            // to alphabetical sort for field_idx.
                            let type_params = td.type_params.clone();
                            let type_args = if let Ty::Named { args, .. } = scrutinee_ty {
                                args.clone()
                            } else {
                                vec![]
                            };
                            let ftm: std::collections::HashMap<String, Ty> = td
                                .fields
                                .iter()
                                .map(|(fname, fty)| {
                                    (
                                        fname.clone(),
                                        substitute_pattern_field_ty(fty, &type_params, &type_args),
                                    )
                                })
                                .collect();
                            (None, ftm, None)
                        }
                    } else {
                        (None, std::collections::HashMap::new(), None)
                    }
                } else {
                    (None, std::collections::HashMap::new(), None)
                };

                let is_enum_variant = variant_match.is_some();
                // Reject unsupported payload subpatterns in enum struct-variant
                // arms.  Plain record struct patterns are out of scope.  A field
                // with no explicit subpattern (e.g. `Shape::Move { x, y }`) is
                // a shorthand binding and is always supported.
                if is_enum_variant {
                    for pf in fields {
                        if let Some((sub_pat, sub_span)) = &pf.pattern {
                            if let Some(label) = unsupported_payload_subpattern_label(sub_pat) {
                                self.report_error_with_note(
                                    crate::error::TypeErrorKind::UnsupportedPayloadSubpattern {
                                        variant_name: short_name.to_string(),
                                        kind_label: label.to_string(),
                                    },
                                    sub_span,
                                    format!(
                                        "payload subpattern `{label}` in `{short_name} {{ {} }}` is not yet supported",
                                        pf.name
                                    ),
                                    pattern_span,
                                    "v0.5 payload subpatterns must be a plain binding (`x`) or wildcard (`_`); \
                                     literal tests and nested patterns are reserved for a future substrate lane"
                                        .to_string(),
                                );
                                return;
                            }
                        }
                    }
                }
                // Compute field_idx using declaration order when available (enum
                // struct-variants), otherwise fall back to alphabetical sort (plain
                // records, where td.fields is a HashMap with no preserved order).
                let ordered_field_names: Vec<String> = field_order.unwrap_or_else(|| {
                    let mut names: Vec<String> = field_tys.keys().cloned().collect();
                    names.sort();
                    names
                });
                let payload_bindings: Vec<PayloadBinding> = fields
                    .iter()
                    .filter_map(|pf| {
                        let field_idx = ordered_field_names
                            .iter()
                            .position(|n| n == &pf.name)
                            .unwrap_or(0);
                        let ty = field_tys.get(&pf.name).cloned().unwrap_or(Ty::Error);
                        // Only emit a PayloadBinding for the concrete binding
                        // name, not for sub-patterns (those are handled by
                        // recursion in bind_pattern, not recorded here).
                        let binding_name = if let Some((sub_pat, _)) = &pf.pattern {
                            binding_name_for_pattern(sub_pat)
                        } else {
                            Some(pf.name.clone())
                        };
                        binding_name.map(|binding_name| PayloadBinding {
                            field_idx,
                            binding_name,
                            ty,
                        })
                    })
                    .collect();

                ArmResolution {
                    pattern_kind: if is_enum_variant {
                        PatternKind::VariantCtor
                    } else {
                        PatternKind::StructPattern
                    },
                    variant_match,
                    payload_bindings,
                }
            }
            Pattern::Tuple(pats) => {
                let elem_tys: Vec<Ty> = if let Ty::Tuple(tys) = scrutinee_ty {
                    tys.clone()
                } else {
                    vec![Ty::Error; pats.len()]
                };
                let payload_bindings: Vec<PayloadBinding> = pats
                    .iter()
                    .zip(elem_tys.iter())
                    .enumerate()
                    .filter_map(|(field_idx, ((sub_pat, _sub_span), ty))| {
                        binding_name_for_pattern(sub_pat).map(|binding_name| PayloadBinding {
                            field_idx,
                            binding_name,
                            ty: ty.clone(),
                        })
                    })
                    .collect();
                ArmResolution {
                    pattern_kind: PatternKind::TuplePattern,
                    variant_match: None,
                    payload_bindings,
                }
            }
            // Or-patterns are intentionally skipped.  Or-pattern lowering is
            // a future lane; a missing entry must surface a typed diagnostic
            // downstream rather than a silent fallthrough.
            Pattern::Or(_, _) => return,
            // Regex patterns: derive named capture names from the pattern at
            // check time using the same `regex` engine as the runtime. The
            // AST `captures` field is initialised to `vec![]` by the parser;
            // we re-derive the list here so the side table carries authoritative
            // names that HIR lowering will consume. Re-validating the pattern is
            // cheap (small patterns, once per arm at check time).
            Pattern::Regex { pattern, .. } => {
                let captures = if let Ok(compiled) = regex::Regex::new(pattern) {
                    // Enumerate all group positions (0-based enumeration; group 0 is
                    // the whole match, so the first real group is capture_names()[1]).
                    // We want the 1-based regex group index alongside each name so MIR
                    // can pass the real group position to `hew_regex_capture` rather
                    // than a flattened named-only ordinal. Using the real group index
                    // corrects the lookup when unnamed positional groups precede named
                    // ones — e.g. `re"(foo)(?P<bar>bar)"` has group 1=(foo) (unnamed)
                    // and group 2=bar (named); the old code would pass 0+1=1 which is
                    // wrong.
                    compiled
                        .capture_names()
                        .enumerate()
                        .filter_map(|(group_idx, name)| {
                            name.map(|n| {
                                (
                                    n.to_owned(),
                                    u32::try_from(group_idx).expect("group index overflows u32"),
                                )
                            })
                        })
                        .collect()
                } else {
                    // Syntactically invalid; `bind_pattern` / expression synthesis
                    // already reported the error; produce empty captures so
                    // downstream stages see a safe no-capture resolution.
                    vec![]
                };
                ArmResolution {
                    pattern_kind: PatternKind::Regex { captures },
                    variant_match: None,
                    payload_bindings: vec![],
                }
            }
        };

        self.pending_pattern_resolutions.insert(key, resolution);
    }

    /// Resolve the source variant-match descriptor for a constructor written at
    /// `variant_surface_name` against `scrutinee_ty`.  Returns `None` only for
    /// built-in types whose variant resolution failed (which also fails
    /// `bind_pattern`), so callers can treat `None` as "checker already emitted
    /// an error for this arm".
    fn resolve_variant_match(
        &self,
        short_name: &str,
        scrutinee_ty: &Ty,
        _full_name: &str,
    ) -> Option<VariantMatch> {
        // Built-in Option<T>
        if scrutinee_ty.as_option().is_some() {
            return match short_name {
                "Some" | "None" => Some(VariantMatch {
                    type_name: "Option".to_string(),
                    variant_name: short_name.to_string(),
                }),
                _ => None,
            };
        }
        // Built-in Result<T, E>
        if scrutinee_ty.as_result().is_some() {
            return match short_name {
                "Ok" | "Err" => Some(VariantMatch {
                    type_name: "Result".to_string(),
                    variant_name: short_name.to_string(),
                }),
                _ => None,
            };
        }
        // User enum
        if let Some(type_name) = scrutinee_ty.type_name() {
            if let Some(td) = self.lookup_type_def(type_name) {
                if td.variants.contains_key(short_name) {
                    return Some(VariantMatch {
                        type_name: type_name.to_string(),
                        variant_name: short_name.to_string(),
                    });
                }
            }
        }
        None
    }

    pub(super) fn lookup_variant_types(
        &self,
        variant_name: &str,
        enum_ty: &Ty,
        fallback_arity: usize,
    ) -> Option<Vec<Ty>> {
        // Strip enum prefix from qualified names (e.g., "Option::Some" -> "Some")
        let short_name = variant_name.rsplit("::").next().unwrap_or(variant_name);
        // Handle Option<T> variants
        if let Some(inner) = enum_ty.as_option() {
            return match short_name {
                "Some" => Some(vec![inner.clone()]),
                "None" => Some(vec![]),
                _ => None,
            };
        }
        // Handle Result<T, E> variants
        if let Some((ok, err)) = enum_ty.as_result() {
            return match short_name {
                "Ok" => Some(vec![ok.clone()]),
                "Err" => Some(vec![err.clone()]),
                _ => None,
            };
        }
        let type_name_opt = enum_ty.type_name();
        if let Some(type_name) = type_name_opt {
            if let Some(td) = self.lookup_type_def(type_name) {
                // Substitute the scrutinee's concrete type args into tuple-variant
                // payload types so generic enum tuple variants bind with the
                // concrete type rather than the enum's free type parameters.
                let type_params = td.type_params.clone();
                let type_args = if let Ty::Named { args, .. } = enum_ty {
                    args.clone()
                } else {
                    vec![]
                };
                let subst_fields = |fields: &[Ty]| -> Vec<Ty> {
                    fields
                        .iter()
                        .map(|f| substitute_pattern_field_ty(f, &type_params, &type_args))
                        .collect()
                };
                if let Some(v) = td.variants.get(short_name) {
                    return match v {
                        VariantDef::Unit => Some(vec![]),
                        VariantDef::Tuple(fields) => Some(subst_fields(fields)),
                        VariantDef::Struct(_) => None,
                    };
                }
                if let Some(v) = td.variants.get(variant_name) {
                    return match v {
                        VariantDef::Unit => Some(vec![]),
                        VariantDef::Tuple(fields) => Some(subst_fields(fields)),
                        VariantDef::Struct(_) => None,
                    };
                }
                // Scrutinee type is a known enum — variant not found in it.
                // Do NOT fall through to global search; that would let
                // variants from unrelated enums silently type-check.
                if !td.variants.is_empty() {
                    return None;
                }
            }
        }
        // If the scrutinee is still unknown, keep constructor payloads flexible.
        if let Ty::Var(_) = enum_ty {
            return Some(vec![Ty::Var(TypeVar::fresh())]);
        }
        // Preserve payload bindings for recovery, but fail closed: an already
        // errored scrutinee must not seed fresh inference variables.
        if let Ty::Error = enum_ty {
            return Some(vec![Ty::Error; fallback_arity]);
        }
        None
    }
}
