#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;

impl Checker {
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
                } else if let Ty::Named {
                    name: type_name, ..
                } = ty
                {
                    self.report_error(
                        TypeErrorKind::Mismatch {
                            expected: type_name.clone(),
                            actual: name.clone(),
                        },
                        span,
                        format!("variant `{name}` is not a member of enum `{type_name}`"),
                    );
                }
            }
            Pattern::Struct { name, fields } => {
                // Bind field patterns to field types
                let type_name_opt = match ty {
                    Ty::Named {
                        name: type_name, ..
                    } => Some(type_name.as_str()),
                    Ty::Machine { name } => Some(name.as_str()),
                    _ => None,
                };
                if let Some(type_name) = type_name_opt {
                    if let Some(td) = self.lookup_type_def(type_name) {
                        // Strip enum prefix for qualified patterns (e.g. "Shape::Move" → "Move")
                        let short_name = name.rsplit("::").next().unwrap_or(name);
                        if let Some(VariantDef::Struct(variant_fields)) =
                            td.variants.get(short_name).cloned()
                        {
                            for pf in fields {
                                if let Some((_, field_ty)) = variant_fields
                                    .iter()
                                    .find(|(field_name, _)| field_name == &pf.name)
                                {
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
                                    let known: Vec<&str> =
                                        variant_fields.iter().map(|(n, _)| n.as_str()).collect();
                                    let similar =
                                        crate::error::find_similar(&pf.name, known.into_iter());
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
                }
            }
            Pattern::Tuple(pats) => {
                if let Ty::Tuple(tys) = ty {
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
            }
            Pattern::Or(a, b) => {
                // Both branches should bind the same names with compatible types.
                self.bind_pattern(&a.0, ty, is_mutable, &a.1);
                self.bind_pattern(&b.0, ty, is_mutable, &b.1);
            }
        }
    }

    pub(super) fn lookup_variant_types(
        &self,
        variant_name: &str,
        enum_ty: &Ty,
        fallback_arity: usize,
    ) -> Option<Vec<Ty>> {
        // Handle Option<T> variants
        if let Some(inner) = enum_ty.as_option() {
            return match variant_name {
                "Some" => Some(vec![inner.clone()]),
                "None" => Some(vec![]),
                _ => None,
            };
        }
        // Handle Result<T, E> variants
        if let Some((ok, err)) = enum_ty.as_result() {
            return match variant_name {
                "Ok" => Some(vec![ok.clone()]),
                "Err" => Some(vec![err.clone()]),
                _ => None,
            };
        }
        // Strip enum prefix from qualified names (e.g., "Colour::Custom" -> "Custom")
        let short_name = variant_name.rsplit("::").next().unwrap_or(variant_name);
        // Extract the type name from Named or Machine types
        let type_name_opt = match enum_ty {
            Ty::Named {
                name: type_name, ..
            } => Some(type_name.as_str()),
            Ty::Machine { name } => Some(name.as_str()),
            _ => None,
        };
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
        // Search all enum types for the variant only when scrutinee type is
        // not a known enum (e.g. int, string, or other non-enum named types).
        for td in self.type_defs.values() {
            if let Some(v) = td.variants.get(short_name) {
                if let VariantDef::Tuple(fields) = v {
                    return Some(fields.clone());
                }
                if let VariantDef::Unit = v {
                    return Some(vec![]);
                }
            }
        }
        None
    }
}
