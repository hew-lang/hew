#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;

fn collect_pattern_bound_names(pattern: &Pattern) -> HashSet<String> {
    match pattern {
        Pattern::Wildcard | Pattern::Literal(_) => HashSet::new(),
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

impl Checker {
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
                self.check_shadowing(name, span);
                self.env
                    .define_with_span(name.clone(), ty.clone(), is_mutable, span.clone());
            }
            Pattern::Constructor { name, patterns } => {
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
                                    // Apply type-param → concrete-arg substitution
                                    let field_ty = type_params.iter().zip(type_args.iter()).fold(
                                        raw_field_ty.clone(),
                                        |acc, (tp, concrete)| {
                                            acc.substitute_named_param(tp, concrete)
                                        },
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
                            for pf in fields {
                                if let Some(field_ty) = td.fields.get(&pf.name) {
                                    if let Some((pat, ps)) = &pf.pattern {
                                        self.bind_pattern(pat, field_ty, is_mutable, ps);
                                    } else {
                                        self.check_shadowing(&pf.name, span);
                                        self.env.define_with_span(
                                            pf.name.clone(),
                                            field_ty.clone(),
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
            Pattern::Or(a, b) => {
                let left_names = collect_pattern_bound_names(&a.0);
                let right_names = collect_pattern_bound_names(&b.0);
                let env_before = self.env.clone();

                self.bind_pattern(&a.0, ty, is_mutable, &a.1);
                let left_env = self.env.clone();
                self.env = env_before.clone();
                self.bind_pattern(&b.0, ty, is_mutable, &b.1);

                if left_names == right_names {
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
                    if let Some(module_name) = &self.current_module {
                        error.source_module = Some(module_name.clone());
                    }
                    self.errors.push(error);
                }
            }
        }
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
                if let Some(v) = td.variants.get(short_name) {
                    return match v {
                        VariantDef::Unit => Some(vec![]),
                        VariantDef::Tuple(fields) => Some(fields.clone()),
                        VariantDef::Struct(_) => None,
                    };
                }
                if let Some(v) = td.variants.get(variant_name) {
                    return match v {
                        VariantDef::Unit => Some(vec![]),
                        VariantDef::Tuple(fields) => Some(fields.clone()),
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
