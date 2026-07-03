#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;

fn or_branch_bound_names(
    before: &crate::env::TypeEnv,
    after: &crate::env::TypeEnv,
) -> HashSet<String> {
    // The set of names an or-pattern branch introduced is computed from the
    // authoritative binding result — the delta between the env before binding
    // the branch and the env after — rather than re-derived from pattern syntax
    // by casing. This removes the case heuristic from the binding-consistency
    // path entirely (#2116): a bare identifier counts as a binder iff
    // `bind_pattern` actually defined it (which it does only when the name does
    // not resolve as a constructor of the scrutinee type), so uppercase binders
    // (`FOO | BAR`) are detected and inconsistent constructors (`Red | Green`,
    // which bind nothing) are not. Regex captures, struct-field shorthands, and
    // nested payload binders are all captured uniformly, with no duplication of
    // `bind_pattern`'s type recursion.
    //
    // A name counts as newly bound when it is absent from `before` or rebound to
    // a fresh binding id (shadowing within the same scope), so a branch that
    // re-binds an outer name to a different type is still observed.
    let before_ids: HashMap<&str, crate::env::TypeBindingId> =
        before.current_scope_bindings().collect();
    after
        .current_scope_bindings()
        .filter(|(name, id)| before_ids.get(name) != Some(id))
        .map(|(name, _)| name.to_owned())
        .collect()
}

fn sorted_pattern_bound_names(names: &HashSet<String>) -> Vec<String> {
    let mut names: Vec<_> = names.iter().cloned().collect();
    names.sort();
    names
}

fn literal_pattern_label(literal: &Literal) -> &'static str {
    match literal {
        Literal::Integer { .. } => "integer literal",
        Literal::Float(_) => "float literal",
        Literal::String(_) => "string literal",
        Literal::Bool(_) => "bool literal",
        Literal::Char(_) => "char literal",
        Literal::Duration(_) => "duration literal",
    }
}

fn literal_pattern_matches_type(literal: &Literal, ty: &Ty) -> bool {
    match literal {
        Literal::Integer { .. } => ty.is_integer(),
        Literal::Float(_) => ty.is_float(),
        Literal::String(_) => matches!(ty, Ty::String),
        Literal::Bool(_) => matches!(ty, Ty::Bool),
        Literal::Char(_) => matches!(ty, Ty::Char),
        Literal::Duration(_) => matches!(ty, Ty::Duration),
    }
}

fn substitute_pattern_field_ty(raw_field_ty: &Ty, type_params: &[String], type_args: &[Ty]) -> Ty {
    let map: HashMap<String, Ty> = type_params
        .iter()
        .zip(type_args.iter())
        .map(|(p, a)| (p.clone(), a.clone()))
        .collect();
    raw_field_ty.substitute_named_params_parallel(&map)
}

/// Extract the single binding name introduced by a sub-pattern, if any.
///
/// Returns `Some(name)` for any plain unqualified `Identifier` — both
/// lowercase and uppercase — because qualified names (containing `::`) are
/// always constructor paths, while bare names (of any casing) bind a new
/// variable.
///
/// Returns `None` for wildcards, literals, qualified identifiers,
/// constructors, structs, tuples, and or-patterns — those either introduce
/// no name, or introduce names the caller handles recursively.
///
/// This is intentionally shallow (top-level only) so the caller decides
/// whether to recurse into constructor payloads.
fn binding_name_for_pattern(pattern: &Pattern) -> Option<String> {
    match pattern {
        Pattern::Identifier(name) if !name.contains("::") => Some(name.clone()),
        _ => None,
    }
}

/// Classify the payload subpattern kind label for use in
/// `UnsupportedPayloadSubpattern` diagnostics.
///
/// Returns `None` when the subpattern is a supported binding, wildcard, or
/// literal predicate.
/// Returns `Some(label)` for unsupported forms that must be rejected.
fn unsupported_payload_subpattern_label(pattern: &Pattern) -> Option<&'static str> {
    match pattern {
        // Qualified identifier — always a constructor path in subpattern position.
        // Bare identifiers (any casing) are resolved against the slot type by the
        // CALL SITE via `resolve_variant_match` before this function is reached.
        // Returning `None` here is the safe default for any call site that has not
        // yet added the resolution guard (false-accept rather than false-reject).
        Pattern::Identifier(name) if name.contains("::") => Some("nested constructor"),
        // Plain binding (bare identifier), wildcard, literal predicates, and tuple
        // payload aggregates are supported or deferred to the call site.
        Pattern::Wildcard | Pattern::Literal(_) | Pattern::Tuple(_) | Pattern::Identifier(_) => {
            None
        }
        Pattern::Struct { .. } | Pattern::RecordShorthand { .. } => Some("record destructure"),
        Pattern::Constructor { .. } => Some("nested constructor"),
        Pattern::Or(_, _) => Some("or-pattern"),
        // Regex literals are only legal as top-level match-arm predicates
        // (scrutinee must be `string`). A regex in payload subpattern
        // position makes no semantic sense; reject as unsupported.
        Pattern::Regex { .. } => Some("regex pattern"),
    }
}

/// Classify subpatterns that are not yet safe for plain record/tuple project
/// patterns.
///
/// Enum payload patterns can carry literal predicates today; plain
/// record/tuple destructures cannot, so accepting `Point { x: 0, y }` or
/// `(0, y)` would silently widen the arm to an irrefutable project. Keep those
/// shapes fail-closed until project predicates grow nested comparisons.
fn unsupported_project_subpattern_label(pattern: &Pattern) -> Option<&'static str> {
    match pattern {
        // Qualified identifier — always a constructor path in project position.
        // Bare identifiers are resolved against the slot type by the CALL SITE
        // via `resolve_variant_match` before this function is reached; returning
        // `None` here is the safe default for call sites that skip that guard.
        Pattern::Identifier(name) if name.contains("::") => Some("nested constructor"),
        // Wildcard and bare identifiers: allowed or deferred to call site.
        Pattern::Wildcard | Pattern::Identifier(_) => None,
        Pattern::Tuple(pats) if pats.is_empty() => None,
        Pattern::Literal(lit) => Some(literal_pattern_label(lit)),
        Pattern::Constructor { .. } => Some("nested constructor"),
        Pattern::Struct { .. } | Pattern::RecordShorthand { .. } => Some("record destructure"),
        Pattern::Tuple(_) => Some("tuple destructure"),
        Pattern::Or(_, _) => Some("or-pattern"),
        Pattern::Regex { .. } => Some("regex pattern"),
    }
}

/// Payload shape of one enum variant for exhaustiveness coverage.
pub(super) enum VariantPayloadShape {
    Unit,
    Tuple(Vec<Ty>),
    /// Struct-variant fields with their (substituted) resolved types, so
    /// field-position sub-patterns can be checked for irrefutability against
    /// their real payload type via `Checker::is_payload_irrefutable_for_ty`
    /// instead of the type-blind free function this shape used to require.
    Struct(Vec<(String, Ty)>),
}

impl Checker {
    /// Enumerate `(variant_name, payload_shape)` for an enum-like scrutinee
    /// type: builtin `Option` / `Result`, user enums, and machine state
    /// enums. Returns `None` for non-enum types.
    pub(super) fn enum_variant_payloads(
        &self,
        ty: &Ty,
    ) -> Option<Vec<(String, VariantPayloadShape)>> {
        if let Some(inner) = ty.as_option() {
            return Some(vec![
                (
                    "Some".to_string(),
                    VariantPayloadShape::Tuple(vec![inner.clone()]),
                ),
                ("None".to_string(), VariantPayloadShape::Unit),
            ]);
        }
        if let Some((ok, err)) = ty.as_result() {
            return Some(vec![
                (
                    "Ok".to_string(),
                    VariantPayloadShape::Tuple(vec![ok.clone()]),
                ),
                (
                    "Err".to_string(),
                    VariantPayloadShape::Tuple(vec![err.clone()]),
                ),
            ]);
        }
        let type_name = ty.type_name()?;
        let td = self.lookup_type_def(type_name)?;
        if td.variants.is_empty() {
            return None;
        }
        let type_params = td.type_params.clone();
        let type_args = if let Ty::Named { args, .. } = ty {
            args.clone()
        } else {
            vec![]
        };
        Some(
            td.variants
                .iter()
                .map(|(name, def)| {
                    let shape = match def {
                        VariantDef::Unit => VariantPayloadShape::Unit,
                        VariantDef::Tuple(fields) => VariantPayloadShape::Tuple(
                            fields
                                .iter()
                                .map(|f| substitute_pattern_field_ty(f, &type_params, &type_args))
                                .collect(),
                        ),
                        VariantDef::Struct(fields) => VariantPayloadShape::Struct(
                            fields
                                .iter()
                                .map(|(name, f)| {
                                    (
                                        name.clone(),
                                        substitute_pattern_field_ty(f, &type_params, &type_args),
                                    )
                                })
                                .collect(),
                        ),
                    };
                    (name.clone(), shape)
                })
                .collect(),
        )
    }

    /// Resolution-aware catch-all test for top-level arm patterns.
    ///
    /// A pattern is a catch-all (covers every value of the scrutinee type) when:
    /// - It is a wildcard `_`, OR
    /// - It is an unqualified plain identifier that does **not** resolve as a
    ///   variant of `scrutinee_ty` — i.e., it is a binding, not a constructor.
    ///
    /// Qualified identifiers (containing `::`) are always constructor paths
    /// and are never catch-alls.  An unresolved scrutinee type (`Ty::Var`)
    /// causes `resolve_variant_match` to return `None`, conservatively treating
    /// the identifier as a binder; this avoids false exhaustiveness errors when
    /// the scrutinee type is not yet fully known.
    pub(super) fn is_catch_all_for_scrutinee(&self, pattern: &Pattern, scrutinee_ty: &Ty) -> bool {
        match pattern {
            Pattern::Wildcard => true,
            Pattern::Identifier(name) => {
                if name.contains("::") {
                    return false;
                }
                let resolved = self.project_assoc_types(&self.subst.resolve(scrutinee_ty));
                let short = name.rsplit("::").next().unwrap_or(name);
                self.resolve_variant_match(short, &resolved, name).is_none()
            }
            _ => false,
        }
    }

    /// True when `patterns` (the flattened, unguarded match-arm leaf patterns)
    /// cover every value of `ty`.
    ///
    /// Refutability-aware: an arm only counts toward a variant's coverage when
    /// its payload subpatterns are irrefutable, or — for single-payload
    /// variants — when the inner subpatterns recursively cover the payload
    /// type. Multi-slot variants whose every row carries a refutable
    /// subpattern are conservatively treated as uncovered (over-rejection is
    /// resolved by the user adding a catch-all arm; under-rejection would
    /// push a reachable miss onto the runtime exhaustiveness trap).
    pub(super) fn patterns_cover_type(&self, patterns: &[&Pattern], ty: &Ty) -> bool {
        if patterns
            .iter()
            .any(|p| self.is_catch_all_for_scrutinee(p, ty))
        {
            return true;
        }
        let resolved = self.subst.resolve(ty);
        if matches!(resolved, Ty::Bool) {
            let mut has_true = false;
            let mut has_false = false;
            for pattern in patterns {
                match pattern {
                    Pattern::Literal(Literal::Bool(true)) => has_true = true,
                    Pattern::Literal(Literal::Bool(false)) => has_false = true,
                    _ => {}
                }
            }
            return has_true && has_false;
        }
        let Some(variants) = self.enum_variant_payloads(&resolved) else {
            return false;
        };
        variants
            .iter()
            .all(|(name, shape)| self.variant_covered(patterns, name, shape))
    }

    /// Resolution-based irrefutability test for a payload subpattern slot,
    /// used for every context where the concrete payload type is known:
    /// tuple-variant payload slots and (via their substituted field types)
    /// struct-variant fields.
    ///
    /// A subpattern is irrefutable (catches every value of `payload_ty`) when:
    /// - It is a wildcard `_` or the empty-tuple unit `()`.
    /// - It is a plain identifier that does **not** resolve as a variant of
    ///   `payload_ty` — i.e., it is a binding, not a constructor.
    ///
    /// Constructor identifiers (qualified with `::`, or resolving to a known
    /// variant) are refutable and return `false`.
    ///
    /// A tuple sub-pattern is irrefutable when it is the empty tuple `()`, or
    /// when its resolved payload type is itself `Ty::Tuple` of equal arity and
    /// every element sub-pattern is (recursively) irrefutable against its
    /// corresponding element type. Any arity mismatch or non-tuple resolved
    /// payload type stays refutable (fail-closed) rather than being credited.
    fn is_payload_irrefutable_for_ty(&self, pattern: &Pattern, payload_ty: &Ty) -> bool {
        match pattern {
            Pattern::Wildcard => true,
            Pattern::Identifier(name) => {
                // Qualified name — always a constructor path, never a binder.
                if name.contains("::") {
                    return false;
                }
                // Unqualified: irrefutable iff it does NOT resolve as a variant.
                let resolved = self.project_assoc_types(&self.subst.resolve(payload_ty));
                let short = name.rsplit("::").next().unwrap_or(name);
                self.resolve_variant_match(short, &resolved, name).is_none()
            }
            Pattern::Tuple(pats) => {
                if pats.is_empty() {
                    return true;
                }
                let resolved = self.project_assoc_types(&self.subst.resolve(payload_ty));
                let Ty::Tuple(elem_tys) = &resolved else {
                    return false;
                };
                pats.len() == elem_tys.len()
                    && pats
                        .iter()
                        .zip(elem_tys.iter())
                        .all(|((sub, _), elem_ty)| self.is_payload_irrefutable_for_ty(sub, elem_ty))
            }
            _ => false,
        }
    }

    /// True when at least one row of `patterns` headed by `variant_name`
    /// covers every value of the variant's payload.
    pub(super) fn variant_covered(
        &self,
        patterns: &[&Pattern],
        variant_name: &str,
        shape: &VariantPayloadShape,
    ) -> bool {
        let struct_field_tys: HashMap<&str, &Ty> = match shape {
            VariantPayloadShape::Struct(fields) => fields
                .iter()
                .map(|(name, ty)| (name.as_str(), ty))
                .collect(),
            _ => HashMap::new(),
        };
        let mut ctor_rows: Vec<&[(Pattern, Span)]> = Vec::new();
        let mut has_unit_row = false;
        let mut has_struct_cover = false;
        for pattern in patterns {
            match pattern {
                Pattern::Constructor { name, patterns } => {
                    let short = name.rsplit("::").next().unwrap_or(name);
                    if short == variant_name {
                        ctor_rows.push(patterns.as_slice());
                    }
                }
                Pattern::Identifier(name) => {
                    let short = name.rsplit("::").next().unwrap_or(name);
                    if short == variant_name {
                        has_unit_row = true;
                    }
                }
                Pattern::Struct { name, fields } => {
                    let short = name.rsplit("::").next().unwrap_or(name);
                    if short == variant_name
                        && fields.iter().all(|pf| {
                            pf.pattern.as_ref().is_none_or(|(sub, _)| {
                                struct_field_tys
                                    .get(pf.name.as_str())
                                    .is_some_and(|field_ty| {
                                        self.is_payload_irrefutable_for_ty(sub, field_ty)
                                    })
                            })
                        })
                    {
                        has_struct_cover = true;
                    }
                }
                _ => {}
            }
        }
        match shape {
            VariantPayloadShape::Unit => has_unit_row || !ctor_rows.is_empty(),
            VariantPayloadShape::Struct(_) => has_struct_cover,
            VariantPayloadShape::Tuple(payload_tys) => {
                // Use resolution-based irrefutability so that a plain uppercase
                // binder (e.g. `Some(MAX)` where `MAX` is not a variant of the
                // payload type) is treated as covering the slot rather than being
                // mistaken for an unresolvable constructor.
                if ctor_rows.iter().any(|row| {
                    row.len() == payload_tys.len()
                        && row
                            .iter()
                            .zip(payload_tys.iter())
                            .all(|((sub, _), payload_ty)| {
                                self.is_payload_irrefutable_for_ty(sub, payload_ty)
                            })
                }) {
                    return true;
                }
                // Single-payload variants recurse: the inner subpatterns of
                // every row jointly cover the payload type (e.g. `Ok(Ok(v))`
                // + `Ok(Err(e))` cover `Ok` of a nested Result).
                if payload_tys.len() == 1 {
                    let inner: Vec<&Pattern> = ctor_rows
                        .iter()
                        .filter(|row| row.len() == 1)
                        .map(|row| &row[0].0)
                        .collect();
                    if !inner.is_empty() && self.patterns_cover_type(&inner, &payload_tys[0]) {
                        return true;
                    }
                }
                false
            }
        }
    }

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

    /// Pattern binding — the single authority on which pattern identifiers are
    /// binders (introduce a name) versus constructors (introduce none). Every
    /// consumer that needs that decision routes through the env delta this
    /// records, never a private re-derivation.
    ///
    /// The top-level call snapshots the innermost scope, runs the binding via
    /// [`Self::bind_pattern_inner`], then records the set of names introduced
    /// (keyed by the pattern span) into `pattern_bound_names`. A constructor
    /// identifier binds nothing, so it never appears in that set — which is why
    /// the borrowed-Rc escape scanner can shadow exactly the real binders
    /// (`shadow_pattern_bindings`) without duplicating the resolution logic.
    /// Nested sub-pattern binds re-enter through `bind_pattern` but the
    /// `bind_pattern_recording` guard keeps them part of the one top-level
    /// delta.
    pub(super) fn bind_pattern(
        &mut self,
        pattern: &Pattern,
        ty: &Ty,
        is_mutable: bool,
        span: &Span,
    ) {
        if self.bind_pattern_recording {
            // Nested sub-pattern: its binders belong to the top-level delta
            // already being recorded by an enclosing call.
            self.bind_pattern_inner(pattern, ty, is_mutable, span);
            return;
        }
        self.bind_pattern_recording = true;
        let before: HashSet<String> = self
            .env
            .current_scope_bindings()
            .map(|(name, _)| name.to_string())
            .collect();
        self.bind_pattern_inner(pattern, ty, is_mutable, span);
        let bound: Vec<String> = self
            .env
            .current_scope_bindings()
            .filter(|(name, _)| !before.contains(*name))
            .map(|(name, _)| name.to_string())
            .collect();
        self.bind_pattern_recording = false;
        let key = super::types::SpanKey::in_module(span, self.current_module_idx);
        if bound.is_empty() {
            // A pure-constructor pattern (e.g. `Red | Green`) binds nothing;
            // ensure no stale entry survives for this span.
            self.pattern_bound_names.remove(&key);
        } else {
            self.pattern_bound_names.insert(key, bound);
        }
    }

    /// Pattern binding
    #[expect(
        clippy::too_many_lines,
        reason = "impl method resolution requires many cases"
    )]
    pub(super) fn bind_pattern_inner(
        &mut self,
        pattern: &Pattern,
        ty: &Ty,
        is_mutable: bool,
        span: &Span,
    ) {
        let resolved_ty = self.subst.resolve(ty);
        let projected_ty = self.project_assoc_types(&resolved_ty);
        let ty = &projected_ty;
        match pattern {
            Pattern::Wildcard => {}
            Pattern::Literal(literal) => {
                if matches!(literal, Literal::Float(_))
                    && !matches!(ty, Ty::F32 | Ty::F64 | Ty::FloatLiteral)
                {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!(
                            "float literal pattern cannot be used for scrutinee type `{}`",
                            ty.user_facing()
                        ),
                    );
                } else {
                    if let Literal::Integer { value, .. } = literal {
                        let ptr_width = self.pointer_width();
                        if let Some(info) = integer_type_info(ty, ptr_width) {
                            if *value < 0 && !info.signed {
                                self.report_error(
                                    TypeErrorKind::InvalidOperation,
                                    span,
                                    format!(
                                        "negative literal `{value}` cannot be used as pattern for unsigned scrutinee type `{}`",
                                        ty.user_facing()
                                    ),
                                );
                            } else if !integer_fits_type(*value, ty, ptr_width) {
                                let (lo, hi) = integer_type_range(ty, ptr_width).unwrap_or((0, 0));
                                self.report_error(
                                    TypeErrorKind::InvalidOperation,
                                    span,
                                    format!(
                                        "integer literal `{value}` does not fit in match scrutinee type `{}` (range {lo}..={hi})",
                                        ty.user_facing()
                                    ),
                                );
                            }
                        }
                    }
                    if !matches!(ty, Ty::Var(_) | Ty::Error)
                        && !literal_pattern_matches_type(literal, ty)
                    {
                        let expected = ty.user_facing().to_string();
                        let actual = literal_pattern_label(literal).to_string();
                        self.report_error(
                            TypeErrorKind::Mismatch {
                                expected: expected.clone(),
                                actual: actual.clone(),
                            },
                            span,
                            format!(
                                "literal pattern `{actual}` cannot match scrutinee type `{expected}`"
                            ),
                        );
                    }
                }
            }
            Pattern::Identifier(name) => {
                // Determine if this identifier names a constructor (unit variant) so we
                // can gate `reject_machine_event_pattern_outside_transition`.  Use scope
                // resolution rather than the old uppercase-first casing heuristic (#2116):
                // a bare name is a constructor only if `resolve_variant_match` returns a
                // match; otherwise it is a plain binding regardless of case.
                let is_constructor_like = if name.contains("::") {
                    true
                } else {
                    let short = name.rsplit("::").next().unwrap_or(name);
                    self.resolve_variant_match(short, ty, name).is_some()
                };
                if is_constructor_like {
                    // A unit-variant constructor pattern introduces no binding. Gate the
                    // machine-event rejection for its side effect, then return WITHOUT
                    // defining a name: a constructor is not a binder, so the env must not
                    // record one. This keeps the env an authoritative record of real
                    // binders, which the or-pattern arm diffs to check binding
                    // consistency (a constructor branch like `Red | Green` correctly
                    // binds nothing on both sides).
                    self.reject_machine_event_pattern_outside_transition(ty, span);
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
                            format!("type `{type_name}` is not defined for type pattern `{name}`"),
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
                        format!("type pattern `{name}` cannot match value of type `{expected}`"),
                    );
                    self.bind_struct_field_placeholders(fields, &Ty::Error, is_mutable, span);
                }
            }
            // Shorthand record destructure `{ a, b }` — no type name in the pattern.
            // Use the scrutinee's type directly to look up field types, identical to
            // the `Pattern::Struct` fields-only path but without the variant-name check.
            Pattern::RecordShorthand { fields } => {
                let type_name_opt = ty.type_name();
                if let Some(type_name) = type_name_opt {
                    if let Some(td) = self.lookup_type_def(type_name) {
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
                                    format!("no field `{}` on type `{type_name}`", pf.name),
                                    similar,
                                );
                            }
                        }
                    } else {
                        self.report_error(
                            TypeErrorKind::UndefinedType,
                            span,
                            format!(
                                "type `{type_name}` is not defined for record shorthand pattern"
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
                            actual: "{ .. }".to_string(),
                        },
                        span,
                        format!(
                            "record shorthand pattern cannot match non-record type `{expected}`"
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
                let env_before = self.env.clone();

                self.bind_pattern(&a.0, ty, is_mutable, &a.1);
                let left_env = self.env.clone();
                let left_names = or_branch_bound_names(&env_before, &left_env);

                self.env = env_before.clone();
                self.bind_pattern(&b.0, ty, is_mutable, &b.1);
                let right_env = self.env.clone();
                let right_names = or_branch_bound_names(&env_before, &right_env);

                if matches!(ty, Ty::Var(_) | Ty::Error) {
                    // The scrutinee type is unresolvable — an earlier error, or a
                    // not-yet-inferred type variable. A bare identifier then cannot
                    // be classified as constructor-or-binder (`resolve_variant_match`
                    // has nothing to resolve against), so the branch binder sets are
                    // unreliable and a consistency verdict would be a cascade off an
                    // already-broken scrutinee. Bind the left branch for recovery and
                    // emit nothing, matching the Constructor/Tuple/Struct arms which
                    // already treat `Ty::Var | Ty::Error` as silent.
                    self.env = left_env;
                } else if self.or_pattern_bindings_match(
                    &left_env,
                    &right_env,
                    &left_names,
                    &right_names,
                ) {
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
        // Resolve inference variables before pattern classification so that
        // Option<T>, Result<T,E>, and user-enum scrutinees are identifiable
        // even when the type was supplied as an in-flight inference var.
        let scrutinee_ty = &self.project_assoc_types(&self.subst.resolve(scrutinee_ty));
        let key = super::types::SpanKey::in_module(pattern_span, self.current_module_idx);

        let resolution = match pattern {
            Pattern::Wildcard => ArmResolution {
                pattern_kind: PatternKind::Wildcard,
                variant_match: None,
                payload_bindings: vec![],
                payload_variant_patterns: vec![],
            },
            Pattern::Literal(_) => ArmResolution {
                pattern_kind: PatternKind::Literal,
                variant_match: None,
                payload_bindings: vec![],
                payload_variant_patterns: vec![],
            },
            Pattern::Identifier(name) => {
                // Resolve whether this identifier names a unit-variant constructor
                // or is a plain binder.  Path-qualified names (`name.contains("::")`)
                // are always constructors (a binder cannot contain `::`). For bare
                // names, use scope resolution against the scrutinee type rather than
                // the old uppercase-first heuristic (#2116): if the name resolves to
                // a known unit-variant, classify it as a VariantCtor; otherwise it
                // is a Binding regardless of case. This allows uppercase plain
                // binders like `INF` or `MAX` against non-enum types.
                if name.contains("::") {
                    let short_name = name.rsplit("::").next().unwrap_or(name);
                    let variant_match = self.resolve_variant_match(short_name, scrutinee_ty, name);
                    ArmResolution {
                        pattern_kind: PatternKind::VariantCtor,
                        variant_match,
                        payload_bindings: vec![],
                        payload_variant_patterns: vec![],
                    }
                } else {
                    let variant_match = self.resolve_variant_match(name, scrutinee_ty, name);
                    if variant_match.is_some() {
                        ArmResolution {
                            pattern_kind: PatternKind::VariantCtor,
                            variant_match,
                            payload_bindings: vec![],
                            payload_variant_patterns: vec![],
                        }
                    } else {
                        ArmResolution {
                            pattern_kind: PatternKind::Binding,
                            variant_match: None,
                            payload_bindings: vec![],
                            payload_variant_patterns: vec![],
                        }
                    }
                }
            }
            Pattern::Constructor { name, patterns } => {
                let short_name = name.rsplit("::").next().unwrap_or(name);
                let variant_match = self.resolve_variant_match(short_name, scrutinee_ty, name);
                let payload_tys = self
                    .lookup_variant_types(name, scrutinee_ty, patterns.len())
                    .unwrap_or_else(|| vec![Ty::Error; patterns.len()]);
                // Validate payload subpatterns using resolution-based nested
                // constructor detection (#2116) rather than the old case heuristic.
                // Track which payload slots were classified as nested constructors so
                // the binding-collection pass below can skip them.
                let mut ctor_field_idxs: std::collections::HashSet<usize> =
                    std::collections::HashSet::new();
                let mut payload_variant_patterns: Vec<PayloadVariantPattern> = Vec::new();
                for (field_idx, (sub_pat, sub_span)) in patterns.iter().enumerate() {
                    let payload_ty = payload_tys.get(field_idx).cloned().unwrap_or(Ty::Error);
                    let resolved_payload =
                        self.project_assoc_types(&self.subst.resolve(&payload_ty));
                    // Classify the subpattern: qualified identifiers and
                    // Pattern::Constructor are always constructors; bare identifiers
                    // are resolved against the payload type.
                    let nested_ctor_opt: Option<(&str, &[(Pattern, Span)])> = match sub_pat {
                        Pattern::Constructor { name, patterns } => {
                            Some((name.as_str(), patterns.as_slice()))
                        }
                        Pattern::Identifier(n) if n.contains("::") => {
                            Some((n.as_str(), &[] as &[(Pattern, Span)]))
                        }
                        Pattern::Identifier(n) => {
                            let short = n.rsplit("::").next().unwrap_or(n);
                            if self
                                .resolve_variant_match(short, &resolved_payload, n)
                                .is_some()
                            {
                                Some((n.as_str(), &[] as &[(Pattern, Span)]))
                            } else {
                                None // plain binding regardless of case
                            }
                        }
                        _ => None,
                    };
                    match nested_ctor_opt {
                        Some((ctor_name, inner_patterns)) => {
                            ctor_field_idxs.insert(field_idx);
                            let Some(pvp) = self.build_payload_variant_pattern(
                                field_idx,
                                &payload_ty,
                                ctor_name,
                                inner_patterns,
                                sub_span,
                                pattern_span,
                            ) else {
                                return;
                            };
                            payload_variant_patterns.push(pvp);
                        }
                        None => {
                            // Plain `Pattern::Identifier` patterns (including
                            // uppercase ones that did not resolve as constructors)
                            // are valid payload bindings — do not report them as
                            // unsupported.  Only check unsupported shapes for
                            // non-Identifier patterns.
                            if !matches!(sub_pat, Pattern::Identifier(_)) {
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
                                        "v0.5 payload subpatterns support plain bindings (`x`), wildcards (`_`), \
                                         literal predicates, and nested constructor patterns; aggregate \
                                         destructures and or-patterns are reserved for a future \
                                         match-destructure stage"
                                            .to_string(),
                                    );
                                    return;
                                }
                            }
                        }
                    }
                }
                // Collect payload bindings for slots not classified as nested
                // constructors.  All plain `Pattern::Identifier` bindings are
                // admitted — including uppercase names that resolution determined
                // are not constructors in this payload position.
                let payload_bindings: Vec<PayloadBinding> = patterns
                    .iter()
                    .zip(payload_tys.iter())
                    .enumerate()
                    .filter_map(|(field_idx, ((sub_pat, _sub_span), ty))| {
                        if ctor_field_idxs.contains(&field_idx) {
                            return None; // Handled as nested constructor above.
                        }
                        if let Pattern::Identifier(binding_name) = sub_pat {
                            Some(PayloadBinding {
                                field_idx,
                                binding_name: binding_name.clone(),
                                ty: self.project_assoc_types(ty),
                            })
                        } else {
                            None
                        }
                    })
                    .collect();
                ArmResolution {
                    pattern_kind: PatternKind::VariantCtor,
                    variant_match,
                    payload_bindings,
                    payload_variant_patterns,
                }
            }
            Pattern::Struct { name, fields } => {
                // Determine whether this is an enum struct-variant or a plain
                // struct/record pattern.
                let short_name = name.rsplit("::").next().unwrap_or(name);
                let type_name_opt = scrutinee_ty.type_name();
                // variant_match: Some(..) for enum struct-variants, None for plain records.
                // field_tys: name → resolved Ty.
                // field_order: Some(Vec<name>) in source declaration order.  Enum
                //   struct-variants use their variant field Vec; plain records use
                //   TypeDef::field_order so PayloadBinding.field_idx matches MIR's
                //   RecordFieldLoad declaration-order offset.
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
                            // Plain record struct — TypeDef::field_order is the
                            // canonical ABI order used by MIR/codegen. Synthetic
                            // test TypeDefs may leave it empty, in which case we
                            // fall back to sorted field names only for recovery.
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
                            let order = if td.field_order.is_empty() {
                                let mut names: Vec<String> = ftm.keys().cloned().collect();
                                names.sort();
                                names
                            } else {
                                td.field_order.clone()
                            };
                            (None, ftm, Some(order))
                        }
                    } else {
                        (None, std::collections::HashMap::new(), None)
                    }
                } else {
                    (None, std::collections::HashMap::new(), None)
                };

                let is_enum_variant = variant_match.is_some();
                // Reject unsupported payload subpatterns in enum struct-variant
                // arms. A field with no explicit subpattern (e.g.
                // `Shape::Move { x, y }`) is a shorthand binding and is always
                // supported.
                if is_enum_variant {
                    for pf in fields {
                        if let Some((sub_pat, sub_span)) = &pf.pattern {
                            // Resolve bare identifier subpatterns against the field type
                            // (#2116): a bare name is a nested constructor only if it
                            // resolves as a variant of the field type; otherwise it is a
                            // binder and must be accepted.  Qualified names (containing
                            // `::`) are always constructor paths and are rejected.
                            if let Pattern::Identifier(n) = sub_pat {
                                let is_nested_ctor = if n.contains("::") {
                                    true
                                } else {
                                    let field_ty =
                                        field_tys.get(&pf.name).cloned().unwrap_or(Ty::Error);
                                    let resolved_field =
                                        self.project_assoc_types(&self.subst.resolve(&field_ty));
                                    self.resolve_variant_match(n, &resolved_field, n).is_some()
                                };
                                if is_nested_ctor {
                                    self.report_error_with_note(
                                        crate::error::TypeErrorKind::UnsupportedPayloadSubpattern {
                                            variant_name: short_name.to_string(),
                                            kind_label: "nested constructor".to_string(),
                                        },
                                        sub_span,
                                        format!(
                                            "payload subpattern `nested constructor` in `{short_name} {{ {} }}` is not yet supported",
                                            pf.name
                                        ),
                                        pattern_span,
                                        "v0.5 payload subpatterns must be a plain binding (`x`) or wildcard (`_`); \
                                         and literal predicates; nested patterns are reserved for a future \
                                         match-destructure stage"
                                            .to_string(),
                                    );
                                    return;
                                }
                                // Bare name that does not resolve as a variant of the
                                // field type — it is a binder.  Allowed; skip the
                                // unsupported-label check below.
                                continue;
                            }
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
                                     and literal predicates; nested patterns are reserved for a future \
                                     match-destructure stage"
                                        .to_string(),
                                );
                                return;
                            }
                        }
                    }
                } else {
                    for pf in fields {
                        if let Some((sub_pat, sub_span)) = &pf.pattern {
                            // Resolve bare identifier subpatterns against the field type
                            // (#2116): a bare name is a nested constructor only if it
                            // resolves as a variant of the field type; otherwise it is a
                            // binder and must be accepted.
                            if let Pattern::Identifier(n) = sub_pat {
                                let is_nested_ctor = if n.contains("::") {
                                    true
                                } else {
                                    let field_ty =
                                        field_tys.get(&pf.name).cloned().unwrap_or(Ty::Error);
                                    let resolved_field =
                                        self.project_assoc_types(&self.subst.resolve(&field_ty));
                                    self.resolve_variant_match(n, &resolved_field, n).is_some()
                                };
                                if is_nested_ctor {
                                    self.report_error_with_note(
                                        crate::error::TypeErrorKind::InvalidOperation,
                                        sub_span,
                                        format!(
                                            "field subpattern `nested constructor` in `{name} {{ {} }}` is not yet supported",
                                            pf.name
                                        ),
                                        pattern_span,
                                        "v0.5 record match destructure supports field bindings (`x`), \
                                         explicit binding aliases (`x: y`), and wildcards (`x: _`); \
                                         nested/literal field predicates are reserved for a future \
                                         match-destructure stage"
                                            .to_string(),
                                    );
                                    return;
                                }
                                // Bare binder (any casing) — allowed; skip the label check.
                                continue;
                            }
                            if let Some(label) = unsupported_project_subpattern_label(sub_pat) {
                                self.report_error_with_note(
                                    crate::error::TypeErrorKind::InvalidOperation,
                                    sub_span,
                                    format!(
                                        "field subpattern `{label}` in `{name} {{ {} }}` is not yet supported",
                                        pf.name
                                    ),
                                    pattern_span,
                                    "v0.5 record match destructure supports field bindings (`x`), \
                                     explicit binding aliases (`x: y`), and wildcards (`x: _`); \
                                     nested/literal field predicates are reserved for a future \
                                     match-destructure stage"
                                        .to_string(),
                                );
                                return;
                            }
                        }
                    }
                }
                // Compute field_idx using declaration order. For plain records,
                // missing fields require `..`; because rest patterns are deferred
                // in this stage, fail closed instead of implicitly widening.
                let ordered_field_names: Vec<String> = field_order.unwrap_or_else(|| {
                    let mut names: Vec<String> = field_tys.keys().cloned().collect();
                    names.sort();
                    names
                });
                if !is_enum_variant {
                    let has_unknown_field =
                        fields.iter().any(|pf| !field_tys.contains_key(&pf.name));
                    if !has_unknown_field {
                        let specified: HashSet<&str> =
                            fields.iter().map(|pf| pf.name.as_str()).collect();
                        let missing: Vec<String> = ordered_field_names
                            .iter()
                            .filter(|field_name| !specified.contains(field_name.as_str()))
                            .cloned()
                            .collect();
                        if !missing.is_empty() {
                            self.report_error(
                                crate::error::TypeErrorKind::InvalidOperation,
                                pattern_span,
                                format!(
                                    "record pattern `{name}` omits field(s) {}; rest patterns (`..`) are not yet supported",
                                    missing.join(", ")
                                ),
                            );
                        }
                    }
                }
                let payload_bindings: Vec<PayloadBinding> = fields
                    .iter()
                    .filter_map(|pf| {
                        let field_idx = ordered_field_names.iter().position(|n| n == &pf.name)?;
                        let ty = field_tys.get(&pf.name).cloned()?;
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
                            ty: self.project_assoc_types(&ty),
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
                    payload_variant_patterns: vec![],
                }
            }
            // Shorthand `{ a, b }` is only valid in `let` position; the
            // checker's `check_stmt` validates this before `record_arm_resolution`
            // is reached. If it somehow appears in a match arm, fail closed.
            Pattern::RecordShorthand { .. } => {
                self.report_error(
                    crate::error::TypeErrorKind::InvalidOperation,
                    pattern_span,
                    "record shorthand pattern `{ .. }` is not valid in match arms; \
                     use a typed record pattern `TypeName { .. }` instead"
                        .to_string(),
                );
                return;
            }
            Pattern::Tuple(pats) => {
                // Compute element types before the subpattern validation loop so we can
                // resolve bare identifier subpatterns against their slot types (#2116).
                let elem_tys: Vec<Ty> = if let Ty::Tuple(tys) = scrutinee_ty {
                    tys.clone()
                } else {
                    vec![Ty::Error; pats.len()]
                };
                for (idx, (sub_pat, sub_span)) in pats.iter().enumerate() {
                    // Resolve bare identifier subpatterns against the element type (#2116).
                    if let Pattern::Identifier(n) = sub_pat {
                        let is_nested_ctor = if n.contains("::") {
                            true
                        } else {
                            let elem_ty = elem_tys.get(idx).cloned().unwrap_or(Ty::Error);
                            let resolved_elem =
                                self.project_assoc_types(&self.subst.resolve(&elem_ty));
                            self.resolve_variant_match(n, &resolved_elem, n).is_some()
                        };
                        if is_nested_ctor {
                            self.report_error_with_note(
                                crate::error::TypeErrorKind::InvalidOperation,
                                sub_span,
                                "tuple subpattern `nested constructor` in match destructure is not yet supported"
                                    .to_string(),
                                pattern_span,
                                "v0.5 tuple match destructure supports element bindings (`x`), \
                                 wildcards (`_`), and unit subpatterns (`()`); nested/literal \
                                 element predicates are reserved for a future match-destructure stage"
                                    .to_string(),
                            );
                            return;
                        }
                        // Bare binder (any casing) — allowed; skip the label check.
                        continue;
                    }
                    if let Some(label) = unsupported_project_subpattern_label(sub_pat) {
                        self.report_error_with_note(
                            crate::error::TypeErrorKind::InvalidOperation,
                            sub_span,
                            format!(
                                "tuple subpattern `{label}` in match destructure is not yet supported"
                            ),
                            pattern_span,
                            "v0.5 tuple match destructure supports element bindings (`x`), \
                             wildcards (`_`), and unit subpatterns (`()`); nested/literal \
                             element predicates are reserved for a future match-destructure stage"
                                .to_string(),
                        );
                        return;
                    }
                }
                let payload_bindings: Vec<PayloadBinding> = pats
                    .iter()
                    .zip(elem_tys.iter())
                    .enumerate()
                    .filter_map(|(field_idx, ((sub_pat, _sub_span), ty))| {
                        binding_name_for_pattern(sub_pat).map(|binding_name| PayloadBinding {
                            field_idx,
                            binding_name,
                            ty: self.project_assoc_types(ty),
                        })
                    })
                    .collect();
                ArmResolution {
                    pattern_kind: PatternKind::TuplePattern,
                    variant_match: None,
                    payload_bindings,
                    payload_variant_patterns: vec![],
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
                    payload_variant_patterns: vec![],
                }
            }
        };

        self.pending_pattern_resolutions.insert(key, resolution);
    }

    /// Recursively resolve a nested constructor subpattern at payload slot
    /// `field_idx` (whose checker type is `payload_ty`) into a
    /// [`PayloadVariantPattern`].
    ///
    /// Admitted inner subpattern shapes: plain bindings, wildcards, the unit
    /// tuple `()`, and (recursively) further constructor patterns. Literal
    /// predicates inside a NESTED constructor, aggregate destructures,
    /// or-patterns, and regex patterns fail closed with an
    /// `UnsupportedPayloadSubpattern` diagnostic.
    ///
    /// Returns `None` after reporting (or when `bind_pattern` already
    /// reported, for unresolvable constructor names) so the caller abandons
    /// the arm resolution — a missing side-table entry fails closed in HIR.
    #[expect(
        clippy::too_many_lines,
        reason = "inline resolution logic for nested constructor subpatterns requires \
                  one branch per sub-pattern shape; extraction would hide the control flow"
    )]
    fn build_payload_variant_pattern(
        &mut self,
        field_idx: usize,
        payload_ty: &Ty,
        ctor_name: &str,
        inner_patterns: &[(Pattern, Span)],
        sub_span: &Span,
        pattern_span: &Span,
    ) -> Option<PayloadVariantPattern> {
        let resolved_payload_ty = self.project_assoc_types(&self.subst.resolve(payload_ty));
        let short_name = ctor_name.rsplit("::").next().unwrap_or(ctor_name);
        let Some(variant_match) =
            self.resolve_variant_match(short_name, &resolved_payload_ty, ctor_name)
        else {
            // `bind_pattern` already reported a mismatch for
            // `Pattern::Constructor` shapes (it recursed through the payload),
            // but constructor-like identifiers (unit variants) take the plain
            // binding path there and arrive unvalidated — report here so the
            // arm fails closed with a named diagnostic either way.
            if inner_patterns.is_empty() {
                self.report_error(
                    crate::error::TypeErrorKind::Mismatch {
                        expected: resolved_payload_ty.user_facing().to_string(),
                        actual: ctor_name.to_string(),
                    },
                    sub_span,
                    format!(
                        "variant `{ctor_name}` is not a member of `{}`",
                        resolved_payload_ty.user_facing()
                    ),
                );
            }
            return None;
        };
        let inner_payload_tys = self
            .lookup_variant_types(ctor_name, &resolved_payload_ty, inner_patterns.len())
            .unwrap_or_else(|| vec![Ty::Error; inner_patterns.len()]);
        let mut bindings: Vec<PayloadBinding> = Vec::new();
        let mut nested: Vec<PayloadVariantPattern> = Vec::new();
        for (inner_idx, (sub_pat, inner_span)) in inner_patterns.iter().enumerate() {
            let inner_ty = inner_payload_tys
                .get(inner_idx)
                .cloned()
                .unwrap_or(Ty::Error);
            // Resolution-based nested constructor detection for deeply-nested
            // payload subpatterns, replacing the old case heuristic in
            // `nested_ctor_subpattern` (#2116).
            let resolved_inner = self.project_assoc_types(&self.subst.resolve(&inner_ty));
            let inner_nested_opt: Option<(&str, &[(Pattern, Span)])> = match sub_pat {
                Pattern::Constructor { name, patterns } => {
                    Some((name.as_str(), patterns.as_slice()))
                }
                Pattern::Identifier(n) if n.contains("::") => {
                    Some((n.as_str(), &[] as &[(Pattern, Span)]))
                }
                Pattern::Identifier(n) => {
                    let short = n.rsplit("::").next().unwrap_or(n);
                    if self
                        .resolve_variant_match(short, &resolved_inner, n)
                        .is_some()
                    {
                        Some((n.as_str(), &[] as &[(Pattern, Span)]))
                    } else {
                        None // plain binding regardless of case
                    }
                }
                _ => None,
            };
            if let Some((inner_ctor, inner_subs)) = inner_nested_opt {
                let pvp = self.build_payload_variant_pattern(
                    inner_idx,
                    &inner_ty,
                    inner_ctor,
                    inner_subs,
                    inner_span,
                    pattern_span,
                )?;
                nested.push(pvp);
                continue;
            }
            match sub_pat {
                Pattern::Wildcard => {}
                Pattern::Tuple(pats) if pats.is_empty() => {}
                Pattern::Identifier(binding_name) => {
                    // All plain identifiers (including uppercase ones that did
                    // not resolve as constructors above) are plain bindings.
                    bindings.push(PayloadBinding {
                        field_idx: inner_idx,
                        binding_name: binding_name.clone(),
                        ty: self.project_assoc_types(&inner_ty),
                    });
                }
                other => {
                    // Literal predicates are admitted at the top payload level
                    // (MIR compares them against a directly-projected field)
                    // but not yet inside a nested constructor; aggregate
                    // destructures, or-patterns, and regex subpatterns are
                    // fail-closed at every depth.
                    let label = match other {
                        Pattern::Literal(_) => "literal predicate inside a nested constructor",
                        Pattern::Struct { .. } | Pattern::RecordShorthand { .. } => {
                            "record destructure"
                        }
                        Pattern::Tuple(_) => "tuple destructure",
                        Pattern::Or(_, _) => "or-pattern",
                        Pattern::Regex { .. } => "regex pattern",
                        Pattern::Wildcard
                        | Pattern::Identifier(_)
                        | Pattern::Constructor { .. } => {
                            unreachable!("handled above")
                        }
                    };
                    self.report_error_with_note(
                        crate::error::TypeErrorKind::UnsupportedPayloadSubpattern {
                            variant_name: short_name.to_string(),
                            kind_label: label.to_string(),
                        },
                        inner_span,
                        format!(
                            "payload subpattern `{label}` in `{short_name}(...)` is not yet supported"
                        ),
                        pattern_span,
                        "nested constructor payloads support plain bindings (`x`), wildcards \
                         (`_`), and further nested constructors; other subpattern shapes are \
                         reserved for a future match-destructure stage"
                            .to_string(),
                    );
                    return None;
                }
            }
        }
        Some(PayloadVariantPattern {
            field_idx,
            payload_ty: resolved_payload_ty,
            variant_match,
            bindings,
            nested,
        })
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
        let stdlib_root_builtin_enum = |name: &str| {
            self.current_module_short().is_none() && crate::lookup_builtin_type(name).is_some()
        };
        if self.current_module_short() == Some("option") || stdlib_root_builtin_enum("Option") {
            if let Ty::Named { name, args, .. } = enum_ty {
                if name == "Option" && args.len() == 1 {
                    return match short_name {
                        "Some" => Some(vec![args[0].clone()]),
                        "None" => Some(vec![]),
                        _ => None,
                    };
                }
            }
        }
        if self.current_module_short() == Some("result") || stdlib_root_builtin_enum("Result") {
            if let Ty::Named { name, args, .. } = enum_ty {
                if name == "Result" && args.len() == 2 {
                    return match short_name {
                        "Ok" => Some(vec![args[0].clone()]),
                        "Err" => Some(vec![args[1].clone()]),
                        _ => None,
                    };
                }
            }
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
