//! Pattern flattening, match-arm planning and destructuring lowering.

use super::*;
use hew_parser::ast::Ident;

/// Flatten a (potentially nested) `Pattern::Or` tree into its leaf patterns,
/// preserving source spans. A pattern that is not an `Or` returns a
/// single-element list containing itself.
///
/// Example: `1 | 2 | 3` parses as `Or(1, Or(2, 3))` and flattens to
/// `[1, 2, 3]`.
pub(super) fn flatten_or_pattern(pattern: &Spanned<Pattern>) -> Vec<Spanned<Pattern>> {
    match &pattern.0 {
        Pattern::Or(left, right) => {
            let mut leaves = flatten_or_pattern(left);
            leaves.extend(flatten_or_pattern(right));
            leaves
        }
        _ => vec![pattern.clone()],
    }
}

pub(super) fn nominal_path_leaf(path: &hew_parser::ast::Path) -> Option<&str> {
    path.segments.last().map(|(ident, _)| ident.name.as_str())
}

/// Recursively push `id`'s leaves into `out`: a synthetic aggregate carrier
/// found in `by_source` contributes its own fields (each possibly further
/// nested) in its place; anything else is a leaf in its own right.
pub(super) fn expand_arm_binding_leaf(
    id: BindingId,
    name: &str,
    ty: &ResolvedTy,
    span: &Span,
    by_source: &std::collections::HashMap<BindingId, &[HirDestructureField]>,
    out: &mut Vec<(String, BindingId, ResolvedTy, Span)>,
) {
    if let Some(fields) = by_source.get(&id) {
        for binding in fields.iter().filter_map(|field| field.binding.as_ref()) {
            expand_arm_binding_leaf(
                binding.id,
                &binding.name,
                &binding.ty,
                &binding.span,
                by_source,
                out,
            );
        }
    } else {
        out.push((name.to_string(), id, ty.clone(), span.clone()));
    }
}

/// Push every binding a nested constructor predicate tree introduces, at any
/// depth. These live outside `HirMatchArm::bindings` because they name slots
/// of a nested variant, not of the arm's own shape.
pub(super) fn expand_nested_predicate_bindings(
    predicates: &[HirPayloadVariantPredicate],
    out: &mut Vec<(String, BindingId, ResolvedTy, Span)>,
) {
    for predicate in predicates {
        for binding in &predicate.bindings {
            out.push((
                binding.name.clone(),
                binding.binding,
                binding.ty.clone(),
                binding.span.clone(),
            ));
        }
        expand_nested_predicate_bindings(&predicate.nested, out);
    }
}

/// Expand a match arm's payload bindings into the leaves actually visible in
/// the arm body. A top-level field that one of `prelude`'s `Destructure`
/// statements further projects (an aggregate subpattern like `Ok((n, s))`) is
/// a synthetic `__payload_*` carrier the source never wrote; its own leaf
/// binders (`n`, `s`) take its place, recursing for a subpattern nested
/// inside another. `nested` contributes the binders of nested constructor
/// subpatterns (`Ok(Some(n))`, `(.Some(n), m)`), which the arm's own binding
/// list does not carry. Keyed by `BindingId`, never by name.
pub(super) fn expand_arm_bindings(
    bindings: &[HirMatchArmBinding],
    nested: &[HirPayloadVariantPredicate],
    prelude: &[HirStmt],
) -> Vec<(String, BindingId, ResolvedTy, Span)> {
    let mut by_source: std::collections::HashMap<BindingId, &[HirDestructureField]> =
        std::collections::HashMap::new();
    for stmt in prelude {
        if let HirStmtKind::Destructure { value, fields } = &stmt.kind {
            if let HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(id),
                ..
            } = &value.kind
            {
                by_source.insert(*id, fields);
            }
        }
    }
    let mut out = Vec::with_capacity(bindings.len());
    for binding in bindings {
        expand_arm_binding_leaf(
            binding.binding,
            &binding.name,
            &binding.ty,
            &binding.span,
            &by_source,
            &mut out,
        );
    }
    expand_nested_predicate_bindings(nested, &mut out);
    out
}

/// The body of one arm handed to [`LowerCtx::lower_pattern_arms`].
///
/// `match` arms carry an expression; `if let` and `while let` carry a block
/// that is lowered against the conditional's result type.
pub(super) enum PatternArmBody<'a> {
    Expr(&'a Spanned<Expr>),
    /// The rest of a pattern condition: the operands to the right of this
    /// `let`, then the then block. Lowered inside the arm scope so those
    /// operands and the block see the names this pattern bound.
    Condition {
        rest: &'a [ConditionItem],
        body: &'a Block,
        body_span: Span,
        fallthrough: ConditionFallthrough<'a>,
    },
    /// `let PAT = expr else { ... }`'s success arm: pack the arm's bindings
    /// (see [`LowerCtx::pack_arm_bindings`]) instead of lowering a source
    /// body. `lower_let_else` destructures the packed value back into fresh
    /// bindings that escape into the enclosing scope.
    Bindings(Span),
}

/// What a pattern condition does when an operand fails: run `if let`'s `else`
/// arm (unit when there is none), or leave `while let`'s loop.
#[derive(Clone, Copy)]
pub(super) enum ConditionFallthrough<'a> {
    Else(Option<&'a Spanned<Expr>>),
    Break,
}

/// One pattern arm to lower. `match`, `if let`, `while let` and `let … else`
/// all build these so the pattern shapes they accept have a single authority.
pub(super) struct PatternArm<'a> {
    pub(super) pattern: Spanned<Pattern>,
    pub(super) guard: Option<&'a Spanned<Expr>>,
    pub(super) body: PatternArmBody<'a>,
}

impl PatternArm<'_> {
    /// End offset of the arm body, used to span the whole arm.
    pub(super) fn body_end(&self) -> usize {
        match &self.body {
            PatternArmBody::Expr(expr) => expr.1.end,
            PatternArmBody::Condition { body_span, .. } => body_span.end,
            PatternArmBody::Bindings(span) => span.end,
        }
    }
}

/// Expand `match` arms into pattern arms, flattening or-patterns into one arm
/// per leaf alternative. The checker classified each leaf under its own span,
/// so downstream lowering consumes the leaves, never the `Or` node.
pub(super) fn pattern_arms_from_match(arms: &[hew_parser::ast::MatchArm]) -> Vec<PatternArm<'_>> {
    arms.iter()
        .flat_map(|arm| {
            flatten_or_pattern(&arm.pattern)
                .into_iter()
                .map(move |pattern| PatternArm {
                    pattern,
                    guard: arm.guard.as_ref(),
                    body: PatternArmBody::Expr(&arm.body),
                })
        })
        .collect()
}

#[expect(
    clippy::too_many_lines,
    reason = "the exhaustive payload classifier keeps all pattern forms in one match"
)]
pub(super) fn collect_match_payload_predicates(
    ctx: &LowerCtx,
    pattern: &Spanned<Pattern>,
    scrutinee_ty: &ResolvedTy,
) -> Result<Vec<HirPayloadPredicate>, String> {
    match &pattern.0 {
        // TRANSITION(P1): deleted by A1 commit 2
        Pattern::NominalPath {
            path: one_path,
            payload: Some(hew_parser::ast::NominalPatternPayload::Tuple(patterns)),
        } if one_path.segments.len() == 1 => {
            let name = &one_path.to_string();
            let field_tys =
                ctx.instantiated_pattern_payload_types(name, scrutinee_ty, patterns.len())?;
            Ok(patterns
                .iter()
                .enumerate()
                .filter_map(|(field_idx, (sub_pat, _))| {
                    let Pattern::Literal(lit) = sub_pat else {
                        return None;
                    };
                    let Ok(field_idx) = u32::try_from(field_idx) else {
                        return None;
                    };
                    let (literal, _) = literal_to_hir(lit);
                    let ty = field_tys[field_idx as usize].clone();
                    Some(HirPayloadPredicate {
                        field_idx,
                        literal,
                        ty,
                    })
                })
                .collect())
        }
        // TRANSITION(P1): deleted by A1 commit 2
        Pattern::NominalPath {
            path: one_path,
            payload: Some(hew_parser::ast::NominalPatternPayload::Record { .. }),
        } if one_path.segments.len() == 1 => {
            let key = ctx.mk_key(&pattern.1);
            let plan = ctx.pattern_plans.get(&key).ok_or_else(|| {
                "checker did not provide a PatternPlan for record literal predicates".to_string()
            })?;
            let mut predicates = Vec::new();
            for field in &plan.fields {
                let hew_types::PlanSub::Literal(lit) = &field.sub else {
                    continue;
                };
                let ty = ResolvedTy::from_ty(&field.ty).map_err(|err| {
                    format!(
                        "record literal predicate field `{}` has unresolved plan type ({err:?})",
                        field.name
                    )
                })?;
                let (literal, _) = literal_to_hir(lit);
                predicates.push(HirPayloadPredicate {
                    field_idx: field.decl_idx,
                    literal,
                    ty,
                });
            }
            Ok(predicates)
        }
        Pattern::Tuple(patterns) => {
            let ResolvedTy::Tuple(field_tys) = scrutinee_ty else {
                return Err(format!(
                    "tuple literal predicates require a tuple scrutinee, got {scrutinee_ty:?}"
                ));
            };
            if patterns.len() != field_tys.len() {
                return Err(format!(
                    "tuple literal predicate arity {} disagrees with scrutinee arity {}",
                    patterns.len(),
                    field_tys.len()
                ));
            }
            let mut predicates = Vec::new();
            for (field_idx, ((sub_pat, _), ty)) in patterns.iter().zip(field_tys).enumerate() {
                let Pattern::Literal(lit) = sub_pat else {
                    continue;
                };
                let field_idx = u32::try_from(field_idx).map_err(|_| {
                    "tuple literal predicate field index exceeds the HIR u32 carrier".to_string()
                })?;
                let (literal, _) = literal_to_hir(lit);
                predicates.push(HirPayloadPredicate {
                    field_idx,
                    literal,
                    ty: ty.clone(),
                });
            }
            Ok(predicates)
        }
        Pattern::NominalPath { path, payload } => match payload.as_ref() {
            None => Ok(Vec::new()),
            Some(hew_parser::ast::NominalPatternPayload::Tuple(patterns)) => {
                let name = nominal_path_leaf(path)
                    .ok_or_else(|| "tuple variant pattern has no constructor name".to_string())?;
                let field_tys =
                    ctx.instantiated_pattern_payload_types(name, scrutinee_ty, patterns.len())?;
                Ok(patterns
                    .iter()
                    .enumerate()
                    .filter_map(|(field_idx, (sub_pattern, _))| {
                        let Pattern::Literal(literal) = sub_pattern else {
                            return None;
                        };
                        let Ok(field_idx) = u32::try_from(field_idx) else {
                            return None;
                        };
                        let (literal, _) = literal_to_hir(literal);
                        let ty = field_tys[field_idx as usize].clone();
                        Some(HirPayloadPredicate {
                            field_idx,
                            literal,
                            ty,
                        })
                    })
                    .collect())
            }
            Some(hew_parser::ast::NominalPatternPayload::Record { .. }) => {
                let key = ctx.mk_key(&pattern.1);
                let plan = ctx.pattern_plans.get(&key).ok_or_else(|| {
                    "checker did not provide a PatternPlan for nominal record predicates"
                        .to_string()
                })?;
                let mut predicates = Vec::new();
                for field in &plan.fields {
                    let hew_types::PlanSub::Literal(literal) = &field.sub else {
                        continue;
                    };
                    let ty = ResolvedTy::from_ty(&field.ty).map_err(|err| {
                        format!(
                            "nominal record predicate field `{}` has unresolved plan type ({err:?})",
                            field.name
                        )
                    })?;
                    let (literal, _) = literal_to_hir(literal);
                    predicates.push(HirPayloadPredicate {
                        field_idx: field.decl_idx,
                        literal,
                        ty,
                    });
                }
                Ok(predicates)
            }
        },
        Pattern::ContextVariant(context) => match context.payload.as_ref() {
            None => Ok(Vec::new()),
            Some(hew_parser::ast::NominalPatternPayload::Tuple(patterns)) => {
                let field_tys = ctx.instantiated_pattern_payload_types(
                    context.name.name.as_str(),
                    scrutinee_ty,
                    patterns.len(),
                )?;
                Ok(patterns
                    .iter()
                    .enumerate()
                    .filter_map(|(field_idx, (sub_pattern, _))| {
                        let Pattern::Literal(literal) = sub_pattern else {
                            return None;
                        };
                        let Ok(field_idx) = u32::try_from(field_idx) else {
                            return None;
                        };
                        let (literal, _) = literal_to_hir(literal);
                        let ty = field_tys[field_idx as usize].clone();
                        Some(HirPayloadPredicate {
                            field_idx,
                            literal,
                            ty,
                        })
                    })
                    .collect())
            }
            Some(hew_parser::ast::NominalPatternPayload::Record { .. }) => {
                let key = ctx.mk_key(&pattern.1);
                let plan = ctx.pattern_plans.get(&key).ok_or_else(|| {
                    "checker did not provide a PatternPlan for contextual record predicates"
                        .to_string()
                })?;
                let mut predicates = Vec::new();
                for field in &plan.fields {
                    let hew_types::PlanSub::Literal(literal) = &field.sub else {
                        continue;
                    };
                    let ty = ResolvedTy::from_ty(&field.ty).map_err(|err| {
                        format!(
                            "contextual record predicate field `{}` has unresolved plan type ({err:?})",
                            field.name
                        )
                    })?;
                    let (literal, _) = literal_to_hir(literal);
                    predicates.push(HirPayloadPredicate {
                        field_idx: field.decl_idx,
                        literal,
                        ty,
                    });
                }
                Ok(predicates)
            }
        },
        Pattern::Wildcard
        | Pattern::Identifier(_)
        | Pattern::Literal(_)
        | Pattern::Or(_, _)
        | Pattern::Regex { .. }
        | Pattern::RecordShorthand { .. } => Ok(Vec::new()),
    }
}

pub(super) fn constructor_payload_aggregate_subpatterns(pattern: &Pattern) -> bool {
    let Some((_, patterns)) = tuple_variant_pattern_parts(pattern) else {
        return false;
    };
    patterns.iter().any(|(sub_pat, _)| {
        matches!(
            sub_pat,
            Pattern::NominalPath {
                path,
                payload: Some(hew_parser::ast::NominalPatternPayload::Record { .. }),
            } if path.segments.len() == 1
        ) // TRANSITION(P1): deleted by A1 commit 2
            || matches!(sub_pat, Pattern::Tuple(items) if !items.is_empty())
    })
}

/// True when an enum struct-variant arm pattern (`Variant { field: (a, b) }`)
/// has at least one field whose sub-pattern is an aggregate (a non-empty tuple
/// or a nested struct/record) that needs recursive destructure lowering.
///
/// The checker accepts these field sub-patterns (see
/// `unsupported_payload_subpattern_label`, which returns `None` for
/// `Pattern::Tuple`), but the payload-binding side-table only records a
/// `PayloadBinding` for plain-identifier field binders. Aggregate field
/// sub-patterns therefore produce no arm binding on their own and must be
/// materialised the same way the tuple-variant path is
/// (`lower_struct_variant_payload_aggregates`). Mirrors
/// `constructor_payload_aggregate_subpatterns` for the struct-variant shape.
pub(super) fn struct_variant_payload_aggregate_subpatterns(pattern: &Pattern) -> bool {
    let Some((_, fields)) = struct_variant_pattern_parts(pattern) else {
        return false;
    };
    fields.iter().any(|pf| match &pf.pattern {
        Some((sub_pat, _)) => {
            matches!(
            sub_pat,
            Pattern::NominalPath {
                path,
                payload: Some(hew_parser::ast::NominalPatternPayload::Record { .. }),
            } if path.segments.len() == 1
        ) // TRANSITION(P1): deleted by A1 commit 2
                || matches!(sub_pat, Pattern::Tuple(items) if !items.is_empty())
        }
        None => false,
    })
}

pub(super) fn tuple_variant_pattern_parts(
    pattern: &Pattern,
) -> Option<(&str, &[Spanned<Pattern>])> {
    match pattern {
        Pattern::NominalPath {
            path,
            payload: Some(hew_parser::ast::NominalPatternPayload::Tuple(patterns)),
        } => Some((nominal_path_leaf(path)?, patterns)),
        Pattern::ContextVariant(context) => match context.payload.as_ref() {
            Some(hew_parser::ast::NominalPatternPayload::Tuple(patterns)) => {
                Some((context.name.name.as_str(), patterns))
            }
            _ => None,
        },
        _ => None,
    }
}

pub(super) fn struct_variant_pattern_parts(
    pattern: &Pattern,
) -> Option<(&str, &[hew_parser::ast::PatternField])> {
    match pattern {
        Pattern::NominalPath {
            path,
            payload: Some(hew_parser::ast::NominalPatternPayload::Record { fields, .. }),
        } => Some((nominal_path_leaf(path)?, fields)),
        Pattern::ContextVariant(context) => match context.payload.as_ref() {
            Some(hew_parser::ast::NominalPatternPayload::Record { fields, .. }) => {
                Some((context.name.name.as_str(), fields))
            }
            _ => None,
        },
        _ => None,
    }
}

impl LowerCtx {
    pub(super) fn lower_pattern_value_into_stmts(
        &mut self,
        pattern: &Spanned<Pattern>,
        value: HirExpr,
        value_ty: ResolvedTy,
        stmts: &mut Vec<HirStmt>,
        span: Span,
    ) {
        match &pattern.0 {
            Pattern::Identifier(name) => {
                self.push_pattern_binding_stmt(name.to_string(), value_ty, value, stmts, span);
            }
            Pattern::Wildcard => {
                let name = format!("_{}", stmts.len());
                self.push_pattern_binding_stmt(name, value_ty, value, stmts, span);
            }
            Pattern::Tuple(elements) => {
                self.lower_tuple_pattern_value_into_stmts(elements, value, &value_ty, stmts, span);
            }
            Pattern::RecordShorthand { fields, .. } => {
                self.lower_record_pattern_value_into_stmts(fields, value, &value_ty, stmts, span);
            }
            Pattern::NominalPath {
                path,
                payload: Some(hew_parser::ast::NominalPatternPayload::Record { fields, .. }),
            } if path.segments.len() == 1 => {
                self.lower_record_pattern_value_into_stmts(fields, value, &value_ty, stmts, span);
            }
            Pattern::Literal(_)
            | Pattern::Or(_, _)
            | Pattern::Regex { .. }
            | Pattern::NominalPath { .. }
            | Pattern::ContextVariant(_) => {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::NotYetImplemented {
                        construct: "unsupported nested let pattern".into(),
                        owning_pass: "pattern-matching".into(),
                    },
                    pattern.1.clone(),
                    "let destructure supports nested tuple and record patterns here; \
                     refutable patterns remain reserved for match/if-let",
                ));
                let name = format!("__unsupported_{}", stmts.len());
                self.push_pattern_binding_stmt(name, value_ty, value, stmts, span);
            }
        }
    }

    pub(super) fn push_pattern_binding_stmt(
        &mut self,
        name: String,
        ty: ResolvedTy,
        value: HirExpr,
        stmts: &mut Vec<HirStmt>,
        span: Span,
    ) -> BindingId {
        let binding = self.bind(name, ty, false, span.clone());
        let binding_id = binding.id;
        stmts.push(HirStmt {
            node: self.ids.node(),
            kind: HirStmtKind::Let(binding, Some(value)),
            span,
        });
        binding_id
    }

    /// The binding one destructured field introduces, and whether it carries a
    /// nested subpattern. A wildcard field introduces no binding: it names
    /// nothing, so nothing is taken out of the source for it.
    pub(super) fn bind_destructure_field(
        &mut self,
        pattern: &Spanned<Pattern>,
        ty: ResolvedTy,
    ) -> (Option<HirBinding>, bool) {
        let (name, nested) = match &pattern.0 {
            Pattern::Identifier(name) => (name.to_string(), false),
            Pattern::Wildcard => return (None, false),
            Pattern::Tuple(_) | Pattern::RecordShorthand { .. } => {
                (format!("__destructure_{}", self.ids.binding().0), true)
            }
            // TRANSITION(P1): deleted by A1 commit 2
            Pattern::NominalPath {
                path,
                payload: Some(hew_parser::ast::NominalPatternPayload::Record { .. }),
            } if path.segments.len() == 1 => {
                (format!("__destructure_{}", self.ids.binding().0), true)
            }
            Pattern::Literal(_)
            | Pattern::Or(_, _)
            | Pattern::Regex { .. }
            | Pattern::NominalPath { .. }
            | Pattern::ContextVariant(_) => {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::NotYetImplemented {
                        construct: "unsupported nested let pattern".into(),
                        owning_pass: "pattern-matching".into(),
                    },
                    pattern.1.clone(),
                    "let destructure supports nested tuple and record patterns here; \
                     refutable patterns remain reserved for match/if-let",
                ));
                (format!("__unsupported_{}", self.ids.binding().0), false)
            }
        };
        (Some(self.bind(name, ty, false, pattern.1.clone())), nested)
    }

    pub(super) fn lower_tuple_pattern_value_into_stmts(
        &mut self,
        elements: &[Spanned<Pattern>],
        value: HirExpr,
        value_ty: &ResolvedTy,
        stmts: &mut Vec<HirStmt>,
        span: Span,
    ) {
        let element_tys = match &value_ty {
            ResolvedTy::Tuple(elems) if elems.len() == elements.len() => elems.clone(),
            ResolvedTy::Tuple(elems) => {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::TuplePatternArityMismatch {
                        expected: elems.len(),
                        actual: elements.len(),
                    },
                    span.clone(),
                    "nested tuple pattern element count does not match tuple value arity",
                ));
                vec![ResolvedTy::Unit; elements.len()]
            }
            _ => {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::TuplePatternNonTupleValue,
                    span.clone(),
                    "nested tuple pattern requires a tuple-typed value",
                ));
                vec![ResolvedTy::Unit; elements.len()]
            }
        };
        let mut fields = Vec::with_capacity(elements.len());
        let mut nested = Vec::new();
        for (idx, (elem_pat, elem_ty)) in elements.iter().zip(element_tys).enumerate() {
            let (binding, is_nested) = self.bind_destructure_field(elem_pat, elem_ty.clone());
            if let (true, Some(carrier)) = (is_nested, binding.clone()) {
                nested.push((elem_pat.clone(), carrier));
            }
            fields.push(HirDestructureField {
                selector: HirDestructureSelector::Tuple(
                    u32::try_from(idx).expect("tuple pattern index must fit in u32"),
                ),
                binding,
                nested: is_nested,
            });
        }
        stmts.push(HirStmt {
            node: self.ids.node(),
            kind: HirStmtKind::Destructure { value, fields },
            span: span.clone(),
        });
        for (pattern, binding) in nested {
            let binding_ty = binding.ty.clone();
            let binding_ref = self.binding_ref_expr(
                binding.name,
                binding.id,
                binding_ty.clone(),
                pattern.1.clone(),
            );
            self.lower_pattern_value_into_stmts(
                &pattern,
                binding_ref,
                binding_ty,
                stmts,
                pattern.1.clone(),
            );
        }
    }

    pub(super) fn lower_record_pattern_value_into_stmts(
        &mut self,
        fields: &[hew_parser::ast::PatternField],
        value: HirExpr,
        value_ty: &ResolvedTy,
        stmts: &mut Vec<HirStmt>,
        span: Span,
    ) {
        // Consume the checker's canonical `PatternPlan` — the SAME field-list
        // source the top-level record-let desugar reads — so a nested rest
        // (`Outer { inner: Inner { a, .. } }`) materialises a wildcard
        // projection for every omitted field, byte-identical to the explicit
        // `Inner { a, b: _ }`. Without this the loop iterated the AST field
        // list and omitted-field projections silently disappeared: a second
        // field-list source and the erasure-ordering hazard the plan forbids.
        // A record-shaped pattern with no plan FAILS CLOSED, same as the
        // top-level path.
        let key = self.mk_key(&span);
        let Some(plan) = self.pattern_plans.get(&key).cloned() else {
            self.diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: "nested record pattern".into(),
                    reason: "missing checker PatternPlan".into(),
                },
                span.clone(),
                "checker did not provide a canonical plan for this nested record pattern",
            ));
            let name = format!("__unsupported_{}", stmts.len());
            self.push_pattern_binding_stmt(name, value_ty.clone(), value, stmts, span);
            return;
        };

        let mut planned_fields: Vec<(String, ResolvedTy, Spanned<Pattern>)> =
            Vec::with_capacity(plan.fields.len());
        for field in plan.fields {
            let field_ty = match ResolvedTy::from_ty(&field.ty) {
                Ok(ty) => self.qualify_current_module_record_ty(ty),
                Err(err) => {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::CheckerBoundaryViolation {
                            name: field.name.clone(),
                            reason: format!("PatternPlan field type is unresolved ({err:?})"),
                        },
                        field.span.clone(),
                        "nested record pattern plan contains an unresolved field type",
                    ));
                    let name = format!("__unsupported_{}", stmts.len());
                    self.push_pattern_binding_stmt(name, value_ty.clone(), value, stmts, span);
                    return;
                }
            };
            let field_pattern = match field.sub {
                hew_types::PlanSub::Binding(name) => {
                    (Pattern::Identifier(Ident::new(&name)), field.span.clone())
                }
                hew_types::PlanSub::Wildcard => (Pattern::Wildcard, field.span.clone()),
                hew_types::PlanSub::Literal(literal) => {
                    (Pattern::Literal(literal), field.span.clone())
                }
                hew_types::PlanSub::Nested(_) => {
                    let Some(source_pattern) = fields
                        .iter()
                        .find(|source| source.name == Ident::new(&field.name))
                        .and_then(|source| source.pattern.clone())
                    else {
                        self.diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::CheckerBoundaryViolation {
                                name: field.name.clone(),
                                reason: "nested PatternPlan field has no source subpattern".into(),
                            },
                            field.span.clone(),
                            "nested record pattern plan cannot be materialised",
                        ));
                        let name = format!("__unsupported_{}", stmts.len());
                        self.push_pattern_binding_stmt(name, value_ty.clone(), value, stmts, span);
                        return;
                    };
                    source_pattern
                }
            };
            planned_fields.push((field.name, field_ty, field_pattern));
        }

        self.lower_planned_record_pattern_value_into_stmts(
            planned_fields,
            value,
            value_ty,
            stmts,
            span,
        );
    }

    pub(super) fn lower_planned_record_pattern_value_into_stmts(
        &mut self,
        planned_fields: Vec<(String, ResolvedTy, Spanned<Pattern>)>,
        value: HirExpr,
        _value_ty: &ResolvedTy,
        stmts: &mut Vec<HirStmt>,
        span: Span,
    ) {
        let mut fields = Vec::with_capacity(planned_fields.len());
        let mut nested = Vec::new();
        for (field_name, field_ty, field_pattern) in planned_fields {
            let (binding, is_nested) =
                self.bind_destructure_field(&field_pattern, field_ty.clone());
            if let (true, Some(carrier)) = (is_nested, binding.clone()) {
                nested.push((field_pattern, carrier));
            }
            fields.push(HirDestructureField {
                selector: HirDestructureSelector::Record(field_name),
                binding,
                nested: is_nested,
            });
        }
        stmts.push(HirStmt {
            node: self.ids.node(),
            kind: HirStmtKind::Destructure { value, fields },
            span: span.clone(),
        });
        for (field_pattern, binding) in nested {
            let binding_ty = binding.ty.clone();
            let binding_ref = self.binding_ref_expr(
                binding.name,
                binding.id,
                binding_ty.clone(),
                field_pattern.1.clone(),
            );
            self.lower_pattern_value_into_stmts(
                &field_pattern,
                binding_ref,
                binding_ty,
                stmts,
                field_pattern.1.clone(),
            );
        }
    }

    pub(super) fn binding_ref_expr(
        &mut self,
        name: String,
        id: BindingId,
        ty: ResolvedTy,
        span: Span,
    ) -> HirExpr {
        HirExpr {
            node: self.ids.node(),
            site: self.ids.site(),
            ty,
            intent: IntentKind::Read,
            kind: HirExprKind::BindingRef {
                name,
                resolved: ResolvedRef::Binding(id),
            },
            span,
        }
    }

    /// Destructure a constructor pattern's AGGREGATE payload subpatterns
    /// (non-empty tuples and structs, e.g. the `(n, s)` in `Ok((n, s))`) into
    /// their leaf binders.
    ///
    /// For each aggregate field the variant carries, this binds the whole
    /// payload field to a synthetic temp (appended to `bindings` so MIR moves
    /// the field out of the scrutinee) and then recursively lowers the nested
    /// pattern against a reference to that temp via
    /// `lower_pattern_value_into_stmts`, collecting the destructure `Let`
    /// statements into the returned prelude. The caller runs the prelude on the
    /// success path so the leaf binders (`n`, `s`) are live afterwards.
    ///
    /// Shared by `match` arm lowering and `let-else` lowering so both paths
    /// reach an identical set of leaf bindings for the same pattern shape.
    /// Returns `(prelude_stmts, had_error)`; on error the diagnostics are
    /// already pushed and the caller fails closed.
    pub(super) fn lower_constructor_payload_aggregates(
        &mut self,
        ctor_name: &str,
        sub_patterns: &[Spanned<Pattern>],
        scrutinee_ty: &ResolvedTy,
        bindings: &mut Vec<HirMatchArmBinding>,
        owning_pass: &'static str,
    ) -> (Vec<HirStmt>, bool) {
        let mut prelude = Vec::new();
        let mut had_error = false;
        let field_tys = self
            .lookup_variant_ctor(ctor_name, Some(scrutinee_ty))
            .map(|(type_name, _, kind)| match kind {
                HirVariantKind::Tuple(field_tys) => {
                    let scrutinee_args = match scrutinee_ty {
                        ResolvedTy::Named { args, .. } => args.as_slice(),
                        _ => &[],
                    };
                    let type_params = self
                        .enum_type_params
                        .get(&type_name)
                        .cloned()
                        .unwrap_or_default();
                    if type_params.len() == scrutinee_args.len() {
                        field_tys
                            .iter()
                            .map(|ty| substitute_type_params(ty, &type_params, scrutinee_args))
                            .collect()
                    } else {
                        field_tys.clone()
                    }
                }
                HirVariantKind::Unit | HirVariantKind::Struct(_) => Vec::new(),
            })
            .unwrap_or_default();
        for (field_idx, (sub_pat, sub_span)) in sub_patterns.iter().enumerate() {
            if !matches!(
            sub_pat,
            Pattern::NominalPath {
                path,
                payload: Some(hew_parser::ast::NominalPatternPayload::Record { .. }),
            } if path.segments.len() == 1
        ) // TRANSITION(P1): deleted by A1 commit 2
                && !matches!(sub_pat, Pattern::Tuple(items) if !items.is_empty())
            {
                continue;
            }
            let Some(field_ty) = field_tys.get(field_idx).cloned() else {
                continue;
            };
            let Ok(field_idx_u32) = u32::try_from(field_idx) else {
                self.unsupported(
                    sub_span.clone(),
                    "payload aggregate field index exceeds u32::MAX",
                    owning_pass,
                );
                had_error = true;
                continue;
            };
            let temp_name = format!("__payload_{}_{}", field_idx, self.ids.binding().0);
            let bound = self.bind(temp_name.clone(), field_ty.clone(), false, sub_span.clone());
            let temp_id = bound.id;
            bindings.push(HirMatchArmBinding {
                span: bound.span.clone(),
                binding: temp_id,
                field_idx: field_idx_u32,
                name: temp_name.clone(),
                ty: field_ty.clone(),
            });
            let temp_ref =
                self.binding_ref_expr(temp_name, temp_id, field_ty.clone(), sub_span.clone());
            self.lower_pattern_value_into_stmts(
                &(sub_pat.clone(), sub_span.clone()),
                temp_ref,
                field_ty,
                &mut prelude,
                sub_span.clone(),
            );
        }
        (prelude, had_error)
    }

    /// Materialise aggregate field sub-patterns of an enum struct-variant match
    /// arm (`Variant { field: (a, b) }`, `Variant { field: Inner { x } }`).
    ///
    /// The struct-variant sibling of `lower_constructor_payload_aggregates`.
    /// The checker accepts a tuple/struct sub-pattern in struct-variant field
    /// position but records no `PayloadBinding` for it (only plain-identifier
    /// field binders get one), so the inner binders (`a`, `b`) are never
    /// materialised and later fail closed with `E_HIR: identifier has no
    /// binding`. This mirrors the tuple-variant path: for each aggregate field
    /// it binds a synthetic `__payload_*` temp to the field slot (so MIR
    /// projects the field into it) and then reuses the `let`-binding destructure
    /// (`lower_pattern_value_into_stmts`) to bind the inner names off that temp.
    ///
    /// Fields are matched to their declared type by NAME (struct-variant field
    /// order in the pattern need not match declaration order), and generic
    /// enum type params are substituted from the scrutinee's type args, exactly
    /// as the constructor path does positionally.
    pub(super) fn lower_struct_variant_payload_aggregates(
        &mut self,
        variant_name: &str,
        fields: &[hew_parser::ast::PatternField],
        scrutinee_ty: &ResolvedTy,
        bindings: &mut Vec<HirMatchArmBinding>,
        owning_pass: &'static str,
    ) -> (Vec<HirStmt>, bool) {
        let mut prelude = Vec::new();
        let mut had_error = false;
        // Declared struct-variant fields in declaration order, with generic
        // type params substituted from the scrutinee's type args.
        let field_decls: Vec<(String, ResolvedTy)> = self
            .lookup_variant_ctor(variant_name, Some(scrutinee_ty))
            .map(|(type_name, _, kind)| match kind {
                HirVariantKind::Struct(field_decls) => {
                    let scrutinee_args = match scrutinee_ty {
                        ResolvedTy::Named { args, .. } => args.as_slice(),
                        _ => &[],
                    };
                    let type_params = self
                        .enum_type_params
                        .get(&type_name)
                        .cloned()
                        .unwrap_or_default();
                    if type_params.len() == scrutinee_args.len() {
                        field_decls
                            .iter()
                            .map(|(name, ty)| {
                                (
                                    name.clone(),
                                    substitute_type_params(ty, &type_params, scrutinee_args),
                                )
                            })
                            .collect()
                    } else {
                        field_decls.clone()
                    }
                }
                HirVariantKind::Unit | HirVariantKind::Tuple(_) => Vec::new(),
            })
            .unwrap_or_default();
        for pf in fields {
            let Some((sub_pat, sub_span)) = &pf.pattern else {
                continue;
            };
            if !matches!(
            sub_pat,
            Pattern::NominalPath {
                path,
                payload: Some(hew_parser::ast::NominalPatternPayload::Record { .. }),
            } if path.segments.len() == 1
        ) // TRANSITION(P1): deleted by A1 commit 2
                && !matches!(sub_pat, Pattern::Tuple(items) if !items.is_empty())
            {
                continue;
            }
            let Some((field_idx, field_ty)) = field_decls
                .iter()
                .enumerate()
                .find(|(_, (name, _))| name == pf.name.name.as_str())
                .map(|(idx, (_, ty))| (idx, ty.clone()))
            else {
                continue;
            };
            let Ok(field_idx_u32) = u32::try_from(field_idx) else {
                self.unsupported(
                    sub_span.clone(),
                    "payload aggregate field index exceeds u32::MAX",
                    owning_pass,
                );
                had_error = true;
                continue;
            };
            let temp_name = format!("__payload_{}_{}", field_idx, self.ids.binding().0);
            let bound = self.bind(temp_name.clone(), field_ty.clone(), false, sub_span.clone());
            let temp_id = bound.id;
            bindings.push(HirMatchArmBinding {
                span: bound.span.clone(),
                binding: temp_id,
                field_idx: field_idx_u32,
                name: temp_name.clone(),
                ty: field_ty.clone(),
            });
            let temp_ref =
                self.binding_ref_expr(temp_name, temp_id, field_ty.clone(), sub_span.clone());
            self.lower_pattern_value_into_stmts(
                &(sub_pat.clone(), sub_span.clone()),
                temp_ref,
                field_ty,
                &mut prelude,
                sub_span.clone(),
            );
        }
        (prelude, had_error)
    }

    /// Lower a surface `match` expression to `HirExprKind::Match`.
    ///
    /// **Substrate scope (v0.5 match-expression slice)**: this change lowers
    /// variant constructor arms (unit, tuple-payload, and struct-payload with
    /// plain binding / wildcard subpatterns), wildcard arms (`_`), plain
    /// binding arms (`x => ...`), literal arms (`0`, `"hello"`), or-pattern
    /// arms (`A | B => ...`), and arms with pattern guards (`x if cond => ...`).
    ///
    /// The type checker has already enforced exhaustiveness for enum
    /// scrutinees (`hew-types/src/check/diagnostics.rs::check_exhaustiveness`).
    /// MIR adds a runtime `Terminator::Trap { ExhaustivenessFallthrough }`
    /// as belt-and-braces; this HIR producer assumes the checker pre-gate
    /// holds.
    ///
    /// **Or-patterns**: HIR flattens each or-tree into leaf alternatives (each
    /// leaf becomes a separate `HirMatchArm` sharing the body), then reads the
    /// checker-authored resolution keyed by that leaf's span.
    ///
    /// **Guards**: a guard expression is lowered and attached to the `HirMatchArm`
    /// via the `guard` field. MIR lowering evaluates the guard after pattern
    /// matching succeeds and falls through to the next arm when the guard is
    /// false.
    ///
    /// Returns `(HirExprKind, ResolvedTy)`. The expression type is the
    /// first-arm body type (all arms must share a type — the type checker
    /// has already verified this; a mismatch would be a checker bug).
    #[allow(
        clippy::too_many_lines,
        reason = "one reject branch per unsupported pattern shape; splitting would scatter the fail-closed audit"
    )]
    pub(super) fn lower_match_expr(
        &mut self,
        scrutinee: &Spanned<Expr>,
        arms: &[hew_parser::ast::MatchArm],
        span: &std::ops::Range<usize>,
    ) -> (HirExprKind, ResolvedTy) {
        let scrutinee_hir = self.lower_expr(scrutinee, IntentKind::Read);
        // Register a generic-enum instantiation if the scrutinee's type is
        // a parameterised enum. The checker-authoritative type at the scrutinee
        // span carries the full `Named { name, args }` including concrete type
        // args; `try_register_enum_instantiation` is a no-op for monomorphic enums.
        self.try_register_enum_instantiation(&scrutinee.1);

        let pattern_arms = pattern_arms_from_match(arms);
        let Some((hir_arms, result_ty)) =
            self.lower_pattern_arms(&scrutinee_hir, &pattern_arms, &ResolvedTy::Unit)
        else {
            return (
                HirExprKind::Unsupported(
                    "match expression contains an unsupported arm shape".into(),
                ),
                ResolvedTy::Unit,
            );
        };

        // A checker-proven uninhabited match has no successor or result value.
        // SIR checks exhaustiveness against the exact enum descriptor.
        if hir_arms.is_empty() {
            if self.checker_expr_ty(span, "empty match") == Some(ResolvedTy::Never) {
                return (
                    HirExprKind::Match {
                        scrutinee: Box::new(scrutinee_hir),
                        arms: hir_arms,
                    },
                    ResolvedTy::Never,
                );
            }
            self.unsupported(
                span.clone(),
                "match expression with no arms",
                "match-expression-substrate",
            );
            return (
                HirExprKind::Unsupported("match expression with no arms".into()),
                ResolvedTy::Unit,
            );
        }

        let ty = self.callable_join_type(span, result_ty.unwrap_or(ResolvedTy::Unit));
        (
            HirExprKind::Match {
                scrutinee: Box::new(scrutinee_hir),
                arms: hir_arms,
            },
            ty,
        )
    }

    /// Lower a list of pattern arms — the single authority for the pattern
    /// shapes `match`, `if let`, `while let` and `let … else` accept.
    ///
    /// Each arm's predicate, bindings and nested payload checks come from the
    /// checker's `pattern_resolutions` / `pattern_plans` side-tables, keyed by
    /// the arm's pattern span. `block_result_ty` is the type block-bodied arms
    /// (`if let` / `while let`) are lowered against; expression bodies ignore it.
    ///
    /// Returns `None` when any arm was rejected: the remaining arm bodies are
    /// still walked so the checker stream stays complete, and the caller fails
    /// closed rather than emitting a partial `Match`.
    #[allow(
        clippy::too_many_lines,
        reason = "one branch per pattern shape; splitting would scatter the \
                  fail-closed recovery each shape shares"
    )]
    pub(super) fn lower_pattern_arms(
        &mut self,
        scrutinee_hir: &HirExpr,
        arms: &[PatternArm<'_>],
        block_result_ty: &ResolvedTy,
    ) -> Option<(Vec<HirMatchArm>, Option<ResolvedTy>)> {
        // Track whether any arm has been rejected; if so we still walk the
        // arm bodies (for checker-stream coverage) but produce no arms at all.
        // A fail-closed shape keeps MIR lowering simple and prevents a
        // half-built Match from reaching codegen.
        let mut rejected = false;
        let mut hir_arms: Vec<HirMatchArm> = Vec::with_capacity(arms.len());
        let mut result_ty: Option<ResolvedTy> = None;

        for arm in arms {
            let pattern_span = &arm.pattern.1;
            let key = self.mk_key(pattern_span);

            let Some(resolution) = self.pattern_resolutions.get(&key).cloned() else {
                self.walk_pattern_arm_body(&arm.body);
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::CheckerBoundaryViolation {
                        name: "match pattern".into(),
                        reason: "missing checker pattern resolution".into(),
                    },
                    pattern_span.clone(),
                    "checker did not provide a resolution for this match-pattern leaf",
                ));
                rejected = true;
                continue;
            };

            // Uniform plan authority: a record-shaped pattern (incl. enum
            // struct-variant `Packet.Data { a, .. }`) with no checker
            // `PatternPlan` fails closed here rather than lowering off the
            // AST-derived resolution.
            if self.record_shape_missing_plan(&arm.pattern) {
                self.walk_pattern_arm_body(&arm.body);
                rejected = true;
                continue;
            }

            let predicate = match resolution.pattern_kind {
                PatternKind::Wildcard => HirMatchArmPredicate::Wildcard,
                PatternKind::Binding => {
                    // Plain lowercase-identifier pattern (`x => ...`).
                    // The scrutinee's full value is bound to `x` in the arm body.
                    let name = if let Pattern::Identifier(n) = &arm.pattern.0 {
                        *n
                    } else {
                        // Checker contract: Binding resolution must come from
                        // an Identifier pattern.
                        self.walk_pattern_arm_body(&arm.body);
                        self.unsupported(
                            pattern_span.clone(),
                            "binding resolution without an Identifier pattern — checker \
                             contract violation",
                            "match-expression-substrate",
                        );
                        rejected = true;
                        continue;
                    };
                    let ty = scrutinee_hir.ty.clone();
                    // Allocate the BindingId now so it can be registered in
                    // scope before the guard and body are lowered.
                    let binding_id = self.ids.binding();
                    HirMatchArmPredicate::Binding {
                        binding_id,
                        name: name.to_string(),
                        ty,
                    }
                }
                PatternKind::VariantCtor => {
                    let Some(mut vm) = resolution.variant_match.clone() else {
                        self.walk_pattern_arm_body(&arm.body);
                        self.unsupported(
                            pattern_span.clone(),
                            "variant pattern missing variant-match resolution",
                            "match-expression-substrate",
                        );
                        rejected = true;
                        continue;
                    };
                    // Resolve variant_idx via `machine_ctor_registry`
                    // (qualified key) — the HIR-side registry populated
                    // from `Item::TypeDecl` body order. The index matches
                    // `EnumLayout.variants` ordering so MIR/codegen don't
                    // re-derive it.
                    let Some((registered_type, idx_usize, _)) =
                        self.lookup_variant_ctor(&vm.variant_name, Some(&scrutinee_hir.ty))
                    else {
                        self.walk_pattern_arm_body(&arm.body);
                        self.unsupported(
                            pattern_span.clone(),
                            "match arm variant not registered in machine/enum ctor registry",
                            "match-expression-substrate",
                        );
                        rejected = true;
                        continue;
                    };
                    vm.type_name = registered_type;
                    let idx = u32::try_from(idx_usize)
                        .expect("variant index exceeds u32::MAX — impossible in Hew");
                    HirMatchArmPredicate::EnumVariant {
                        variant_match: vm,
                        variant_idx: idx,
                    }
                }
                PatternKind::Literal => {
                    let Pattern::Literal(lit) = &arm.pattern.0 else {
                        self.walk_pattern_arm_body(&arm.body);
                        self.unsupported(
                            pattern_span.clone(),
                            "literal arm resolution did not correspond to an AST literal pattern",
                            "match-expression-substrate",
                        );
                        rejected = true;
                        continue;
                    };
                    let (lit, literal_ty) = literal_to_hir(lit);
                    let ty = match (&lit, &scrutinee_hir.ty) {
                        (HirLiteral::Integer(_), scrutinee_ty)
                            if scrutinee_ty.is_integer_literal_match_scrutinee() =>
                        {
                            scrutinee_ty.clone()
                        }
                        (HirLiteral::Bool(_), ResolvedTy::Bool) => ResolvedTy::Bool,
                        (HirLiteral::Char(_), ResolvedTy::Char) => ResolvedTy::Char,
                        (HirLiteral::String(_), ResolvedTy::String) => ResolvedTy::String,
                        (HirLiteral::Float(_), ResolvedTy::F32) => ResolvedTy::F32,
                        (HirLiteral::Float(_), ResolvedTy::F64) => ResolvedTy::F64,
                        (HirLiteral::Duration(_) | HirLiteral::Unit, _) => {
                            self.walk_pattern_arm_body(&arm.body);
                            self.unsupported(
                                pattern_span.clone(),
                                format!("unsupported literal pattern in match arm ({lit:?})"),
                                "match-literal-stage2",
                            );
                            rejected = true;
                            continue;
                        }
                        _ => {
                            self.walk_pattern_arm_body(&arm.body);
                            self.unsupported(
                                pattern_span.clone(),
                                format!(
                                    "literal pattern type mismatch in match arm: literal {:?} \
                                     has type {:?}, scrutinee has type {:?}",
                                    lit, literal_ty, scrutinee_hir.ty
                                ),
                                "match-expression-substrate",
                            );
                            rejected = true;
                            continue;
                        }
                    };
                    debug_assert!(literal_match_supported(&lit, &ty));
                    HirMatchArmPredicate::Literal { lit, ty }
                }
                PatternKind::StructPattern => {
                    let ResolvedTy::Named { .. } = &scrutinee_hir.ty else {
                        self.walk_pattern_arm_body(&arm.body);
                        self.unsupported(
                            pattern_span.clone(),
                            format!(
                                "record project pattern on non-record scrutinee type {:?}",
                                scrutinee_hir.ty
                            ),
                            "match-expression-substrate",
                        );
                        rejected = true;
                        continue;
                    };
                    HirMatchArmPredicate::RecordProject {
                        ty: scrutinee_hir.ty.clone(),
                    }
                }
                PatternKind::TuplePattern => {
                    let ResolvedTy::Tuple(items) = &scrutinee_hir.ty else {
                        self.walk_pattern_arm_body(&arm.body);
                        self.unsupported(
                            pattern_span.clone(),
                            format!(
                                "tuple project pattern on non-tuple scrutinee type {:?}",
                                scrutinee_hir.ty
                            ),
                            "match-expression-substrate",
                        );
                        rejected = true;
                        continue;
                    };
                    let Ok(arity) = u32::try_from(items.len()) else {
                        self.walk_pattern_arm_body(&arm.body);
                        self.unsupported(
                            pattern_span.clone(),
                            "tuple project arity exceeds u32::MAX",
                            "match-expression-substrate",
                        );
                        rejected = true;
                        continue;
                    };
                    HirMatchArmPredicate::TupleProject { arity }
                }
                // Regex literal pattern in a string-scrutinee match arm.
                // Allocate (or reuse) the literal-table entry and build the
                // `HirMatchArmPredicate::Regex` predicate.
                PatternKind::Regex { captures } => {
                    // Extract the raw pattern string from the AST pattern node.
                    // The pattern must be `Pattern::Regex { pattern, .. }`; any
                    // other shape is a checker contract violation — fail closed.
                    let raw_pattern = match &arm.pattern.0 {
                        Pattern::Regex { pattern, .. } => pattern.clone(),
                        other => {
                            self.unsupported(
                                pattern_span.clone(),
                                format!(
                                    "regex arm pattern resolved to PatternKind::Regex but AST \
                                     pattern node is {:?} — checker contract violation",
                                    std::mem::discriminant(other)
                                ),
                                "regex-match-arm-substrate",
                            );
                            rejected = true;
                            continue;
                        }
                    };
                    let literal_id = self.alloc_regex_literal(&raw_pattern, &captures);
                    HirMatchArmPredicate::Regex {
                        literal_id,
                        pattern: raw_pattern,
                        captures,
                    }
                }
            };

            let mut binding_specs = Vec::with_capacity(resolution.payload_bindings.len());
            let mut binding_error = false;
            for payload in &resolution.payload_bindings {
                let ty = match ResolvedTy::from_ty(&payload.ty) {
                    Ok(ty) => self.qualify_current_module_record_ty(ty),
                    Err(err) => {
                        self.unsupported(
                            pattern_span.clone(),
                            format!("unresolved payload binding type in match arm ({err:?})"),
                            "match-expression-substrate",
                        );
                        binding_error = true;
                        continue;
                    }
                };
                let Ok(field_idx) = u32::try_from(payload.field_idx) else {
                    self.unsupported(
                        pattern_span.clone(),
                        "payload binding field index exceeds u32::MAX",
                        "match-expression-substrate",
                    );
                    binding_error = true;
                    continue;
                };
                let Some(binding_span) = self.match_payload_binding_span(payload, pattern_span)
                else {
                    binding_error = true;
                    continue;
                };
                binding_specs.push((field_idx, payload.binding_name.clone(), ty, binding_span));
            }
            if binding_error {
                self.walk_pattern_arm_body(&arm.body);
                rejected = true;
                continue;
            }

            let payload_predicates =
                match collect_match_payload_predicates(self, &arm.pattern, &scrutinee_hir.ty) {
                    Ok(predicates) => predicates,
                    Err(reason) => {
                        self.walk_pattern_arm_body(&arm.body);
                        self.unsupported(
                            pattern_span.clone(),
                            reason,
                            "match-project-literal-predicates",
                        );
                        rejected = true;
                        continue;
                    }
                };
            let has_payload_aggregate_subpatterns =
                constructor_payload_aggregate_subpatterns(&arm.pattern.0)
                    || struct_variant_payload_aggregate_subpatterns(&arm.pattern.0);

            // Nested constructor subpatterns occupy a slot of a variant
            // payload, a record field or a tuple element; any other predicate
            // has no slot to nest into, so a non-empty vector there is a
            // checker contract violation — fail closed rather than silently
            // dropping the nested checks.
            if !resolution.payload_variant_patterns.is_empty()
                && !matches!(
                    predicate,
                    HirMatchArmPredicate::EnumVariant { .. }
                        | HirMatchArmPredicate::RecordProject { .. }
                        | HirMatchArmPredicate::TupleProject { .. }
                )
            {
                self.walk_pattern_arm_body(&arm.body);
                self.unsupported(
                    pattern_span.clone(),
                    "nested constructor subpatterns on a match arm with no slots — \
                     checker contract violation",
                    "match-expression-substrate",
                );
                rejected = true;
                continue;
            }

            // Open scope for payload bindings + guard + body.
            // Binding patterns also need a scope (the scrutinee is bound
            // there), as do nested constructor subpatterns (their inner
            // bindings are materialised below).
            let needs_scope = !binding_specs.is_empty()
                || !resolution.payload_variant_patterns.is_empty()
                || has_payload_aggregate_subpatterns
                || matches!(predicate, HirMatchArmPredicate::Binding { .. });
            let arm_scope = needs_scope.then(|| self.ids.scope());
            let previous_scope_id =
                arm_scope.map(|scope| std::mem::replace(&mut self.current_scope_id, scope));
            if needs_scope {
                self.push_scope();
            }

            // For Binding-predicate arms, register the binding in the HIR scope
            // so that references to the bound name in the guard and body resolve.
            // The BindingId was allocated during predicate classification above.
            if let HirMatchArmPredicate::Binding {
                binding_id,
                name: binding_name,
                ty: binding_ty,
            } = &predicate
            {
                self.bind_existing(
                    *binding_id,
                    binding_name.clone(),
                    binding_ty.clone(),
                    false,
                    pattern_span.clone(),
                );
            }

            // Materialise payload bindings (constructor payload fields).
            let mut bindings = Vec::with_capacity(binding_specs.len());
            for (field_idx, name, ty, binding_span) in binding_specs {
                let bound = self.bind(name.clone(), ty.clone(), false, binding_span);
                bindings.push(HirMatchArmBinding {
                    span: bound.span.clone(),
                    binding: bound.id,
                    field_idx,
                    name,
                    ty,
                });
            }

            let mut body_prelude = Vec::new();
            if let Some((name, patterns)) = tuple_variant_pattern_parts(&arm.pattern.0) {
                let (prelude, had_error) = self.lower_constructor_payload_aggregates(
                    name,
                    patterns,
                    &scrutinee_hir.ty,
                    &mut bindings,
                    "match-expression-substrate",
                );
                body_prelude = prelude;
                binding_error |= had_error;
            } else if let Some((name, fields)) = struct_variant_pattern_parts(&arm.pattern.0) {
                // Enum struct-variant arm with aggregate field sub-patterns
                // (`Variant { field: (a, b) }`). Plain record-project arms
                // (`Point { x, y }`) and plain field binders route through the
                // payload-binding side-table above and produce no aggregate
                // fields, so this is a no-op for them.
                let (prelude, had_error) = self.lower_struct_variant_payload_aggregates(
                    name,
                    fields,
                    &scrutinee_hir.ty,
                    &mut bindings,
                    "match-expression-substrate",
                );
                body_prelude = prelude;
                binding_error |= had_error;
            }
            if binding_error {
                self.walk_pattern_arm_body(&arm.body);
                if let Some(previous) = previous_scope_id {
                    self.current_scope_id = previous;
                }
                if needs_scope {
                    self.pop_scope();
                }
                rejected = true;
                continue;
            }
            if !body_prelude.is_empty() && arm.guard.is_some() {
                self.walk_pattern_arm_body(&arm.body);
                self.unsupported(
                    pattern_span.clone(),
                    "guarded match arm with nested aggregate payload destructure",
                    "match-expression-substrate",
                );
                if let Some(previous) = previous_scope_id {
                    self.current_scope_id = previous;
                }
                if needs_scope {
                    self.pop_scope();
                }
                rejected = true;
                continue;
            }

            // Materialise nested constructor predicate trees. Their inner
            // bindings are registered in the same arm scope as the payload
            // bindings above, so guard/body references resolve through them.
            let mut payload_variant_predicates =
                Vec::with_capacity(resolution.payload_variant_patterns.len());
            let mut pvp_error = false;
            for pvp in &resolution.payload_variant_patterns {
                if let Some(pred) = self.build_payload_variant_predicate(pvp, pattern_span) {
                    payload_variant_predicates.push(pred);
                } else {
                    pvp_error = true;
                    break;
                }
            }
            if pvp_error {
                self.walk_pattern_arm_body(&arm.body);
                if let Some(previous) = previous_scope_id {
                    self.current_scope_id = previous;
                }
                if needs_scope {
                    self.pop_scope();
                }
                rejected = true;
                continue;
            }

            // Lower the guard expression (if any). The guard sees the same
            // bindings that the arm body sees, so it must be lowered after the
            // payload bindings are in scope.
            let guard_hir = arm
                .guard
                .as_ref()
                .map(|guard_spanned| self.lower_expr(guard_spanned, IntentKind::Read));

            let mut body_hir = self.lower_pattern_arm_body(
                &arm.body,
                block_result_ty,
                &bindings,
                &payload_variant_predicates,
                &body_prelude,
            );
            if !body_prelude.is_empty() {
                let body_ty = body_hir.ty.clone();
                let body_span = arm.pattern.1.start..arm.body_end();
                body_hir = HirExpr {
                    node: self.ids.node(),
                    site: self.ids.site(),
                    ty: body_ty.clone(),
                    intent: IntentKind::Read,
                    kind: HirExprKind::Block(HirBlock {
                        node: self.ids.node(),
                        scope: self.ids.scope(),
                        statements: body_prelude,
                        tail: Some(Box::new(body_hir)),
                        ty: body_ty,
                        span: body_span.clone(),
                    }),
                    span: body_span,
                };
            }

            if let Some(previous) = previous_scope_id {
                self.current_scope_id = previous;
            }
            if needs_scope {
                self.pop_scope();
            }

            // Skip diverging arms (Unit = no trailing expr / return-only block;
            // Never = explicit bottom type) when inferring the match result type,
            // mirroring `check_match_expr`'s `Ty::Never | Ty::Error` skip.  A
            // diverging arm body typed as `Unit` by `lower_block` (because it has
            // no tail expression, e.g. `{ return X; }`) must not constrain the
            // match result type — only non-diverging arms should set it.
            if result_ty.is_none() && !matches!(body_hir.ty, ResolvedTy::Unit | ResolvedTy::Never) {
                result_ty = Some(body_hir.ty.clone());
            }

            hir_arms.push(HirMatchArm {
                scope: arm_scope,
                predicate,
                bindings,
                payload_predicates,
                payload_variant_predicates,
                guard: guard_hir,
                body: body_hir,
                span: arm.pattern.1.start..arm.body_end(),
            });
        }

        if rejected {
            return None;
        }
        Some((hir_arms, result_ty))
    }

    /// Lower one pattern arm's body. Expression bodies (`match`) synthesize
    /// their own type; block bodies (`if let` / `while let`) are lowered
    /// against the conditional's result type so both branches agree.
    pub(super) fn lower_pattern_arm_body(
        &mut self,
        body: &PatternArmBody<'_>,
        block_result_ty: &ResolvedTy,
        bindings: &[HirMatchArmBinding],
        nested: &[HirPayloadVariantPredicate],
        body_prelude: &[HirStmt],
    ) -> HirExpr {
        match body {
            PatternArmBody::Expr(expr) => self.lower_expr(expr, IntentKind::Read),
            PatternArmBody::Condition {
                rest,
                body,
                body_span,
                fallthrough,
            } => self.lower_condition_chain(
                rest,
                body,
                body_span,
                *fallthrough,
                block_result_ty,
                body_span,
            ),
            PatternArmBody::Bindings(span) => {
                self.pack_arm_bindings(bindings, nested, body_prelude, span.clone())
            }
        }
    }

    /// Pack a let-else success arm's bindings into the value its synthesized
    /// match arm returns: `Unit` for none, the binding's own value for
    /// exactly one, a name-ordered tuple for more. Sorting by name (rather
    /// than declaration order) keeps every or-pattern leaf's tuple shape
    /// identical even when the leaves' variants declare the shared binder
    /// names in different field orders. `lower_let_else` destructures the
    /// packed value back into fresh, escaping bindings after the match.
    ///
    /// `bindings` is expanded through `body_prelude` first: a top-level
    /// payload field that an aggregate subpattern (`Ok((n, s))`) further
    /// destructures is a synthetic `__payload_*` carrier, not a name the
    /// source wrote, so its own leaf binders (`n`, `s`) pack in its place.
    pub(super) fn pack_arm_bindings(
        &mut self,
        bindings: &[HirMatchArmBinding],
        nested: &[HirPayloadVariantPredicate],
        body_prelude: &[HirStmt],
        span: Span,
    ) -> HirExpr {
        let mut ordered = expand_arm_bindings(bindings, nested, body_prelude);
        ordered.sort_by(|a, b| a.0.cmp(&b.0));
        match ordered.as_slice() {
            [] => self.make_unit_expr(span),
            [(name, id, ty, _)] => self.binding_ref_expr(name.clone(), *id, ty.clone(), span),
            many => {
                let elements: Vec<HirExpr> = many
                    .iter()
                    .map(|(name, id, ty, _)| {
                        self.binding_ref_expr(name.clone(), *id, ty.clone(), span.clone())
                    })
                    .collect();
                let ty = ResolvedTy::Tuple(many.iter().map(|(_, _, ty, _)| ty.clone()).collect());
                HirExpr {
                    node: self.ids.node(),
                    site: self.ids.site(),
                    ty: ty.clone(),
                    intent: IntentKind::Read,
                    kind: HirExprKind::TupleLiteral { elements },
                    span,
                }
            }
        }
    }

    /// Lower an `if let` / `while let` condition (§12.5) to nested two-arm
    /// matches and boolean branches.
    ///
    /// `if let P = a && b && let Q = c { body } else { alt }` becomes
    /// `match a { P => if b { match c { Q => body, _ => alt } } else { alt },
    /// _ => alt }`: each operand binds for the operands to its right, the
    /// operands run left to right, and the first that fails takes the
    /// fallthrough. Nothing the condition binds reaches the fallthrough, which
    /// is what keeps the `else` arm free of the condition's names.
    ///
    /// The fallthrough is lowered once per operand. Exactly one copy can run,
    /// and each needs its own bindings and drop sites, so they cannot be
    /// shared.
    pub(super) fn lower_condition_chain(
        &mut self,
        conditions: &[ConditionItem],
        body: &Block,
        body_span: &Span,
        fallthrough: ConditionFallthrough<'_>,
        result_ty: &ResolvedTy,
        span: &Span,
    ) -> HirExpr {
        let Some((first, rest)) = conditions.split_first() else {
            let block = self.lower_block(body, result_ty);
            let ty = block.ty.clone();
            return HirExpr {
                node: self.ids.node(),
                site: self.ids.site(),
                ty: ty.clone(),
                intent: IntentKind::Read,
                kind: HirExprKind::Block(block),
                span: body_span.clone(),
            };
        };

        match first {
            ConditionItem::Let { pattern, expr } => {
                let scrutinee_hir = self.lower_expr(expr, IntentKind::Read);
                // Register a generic-enum instantiation so MIR/codegen find the
                // mangled layout — matches the Match path.
                self.try_register_enum_instantiation(&expr.1);
                let arms: Vec<PatternArm<'_>> = flatten_or_pattern(pattern)
                    .into_iter()
                    .map(|leaf| PatternArm {
                        pattern: leaf,
                        guard: None,
                        body: PatternArmBody::Condition {
                            rest,
                            body,
                            body_span: body_span.clone(),
                            fallthrough,
                        },
                    })
                    .collect();
                let Some((hir_arms, _)) = self.lower_pattern_arms(&scrutinee_hir, &arms, result_ty)
                else {
                    let _ = self.lower_condition_fallthrough(fallthrough, span);
                    return HirExpr {
                        node: self.ids.node(),
                        site: self.ids.site(),
                        ty: ResolvedTy::Unit,
                        intent: IntentKind::Read,
                        kind: HirExprKind::Unsupported(
                            "pattern condition with an unsupported pattern shape".into(),
                        ),
                        span: span.clone(),
                    };
                };
                let alternative = self.lower_condition_fallthrough(fallthrough, span);
                self.pattern_conditional_match(
                    scrutinee_hir,
                    hir_arms,
                    alternative,
                    result_ty,
                    span,
                )
            }
            ConditionItem::Expr(test) => {
                let condition = self.lower_expr(test, IntentKind::Read);
                let then_expr =
                    self.lower_condition_chain(rest, body, body_span, fallthrough, result_ty, span);
                let alternative = self.lower_condition_fallthrough(fallthrough, span);
                HirExpr {
                    node: self.ids.node(),
                    site: self.ids.site(),
                    ty: result_ty.clone(),
                    intent: IntentKind::Read,
                    kind: HirExprKind::If {
                        condition: Box::new(condition),
                        then_expr: Box::new(then_expr),
                        else_expr: Some(Box::new(alternative)),
                    },
                    span: span.clone(),
                }
            }
        }
    }

    /// Lower one copy of a pattern condition's failure path.
    pub(super) fn lower_condition_fallthrough(
        &mut self,
        fallthrough: ConditionFallthrough<'_>,
        span: &Span,
    ) -> HirExpr {
        match fallthrough {
            // The `else` arm is an expression, so `else if` and `else if let`
            // links lower through the same path as an `else { .. }` block.
            ConditionFallthrough::Else(Some(arm)) => self.lower_expr(arm, IntentKind::Read),
            ConditionFallthrough::Else(None) => self.make_unit_expr(span.clone()),
            ConditionFallthrough::Break => HirExpr {
                node: self.ids.node(),
                site: self.ids.site(),
                ty: ResolvedTy::Unit,
                intent: IntentKind::Read,
                kind: HirExprKind::Break {
                    label: None,
                    value: None,
                },
                span: span.clone(),
            },
        }
    }

    /// Walk a rejected arm's body so the checker stream stays complete. The
    /// lowered result is discarded; only the diagnostics it produces matter.
    pub(super) fn walk_pattern_arm_body(&mut self, body: &PatternArmBody<'_>) {
        let _ = self.lower_pattern_arm_body(body, &ResolvedTy::Unit, &[], &[], &[]);
    }

    /// Convert one checker-resolved [`hew_types::PayloadVariantPattern`] into
    /// its HIR form: resolve the nested variant's declaration-order index via
    /// `machine_ctor_registry` (the same qualified-key lookup the outer arm
    /// uses), register a generic-enum instantiation for the nested payload
    /// type, and allocate binding ids for the inner payload bindings.
    ///
    /// Must be called inside the arm's scope (`push_scope`) so the inner
    /// bindings resolve in the guard and body. Returns `None` after pushing a
    /// fail-closed diagnostic.
    #[expect(
        clippy::too_many_lines,
        reason = "one recursive checker boundary converts binding and literal types with diagnostics"
    )]
    pub(super) fn build_payload_variant_predicate(
        &mut self,
        pvp: &hew_types::PayloadVariantPattern,
        pattern_span: &Span,
    ) -> Option<HirPayloadVariantPredicate> {
        let payload_ty = match ResolvedTy::from_ty(&pvp.payload_ty) {
            Ok(ty) => self.qualify_current_module_record_ty(ty),
            Err(err) => {
                self.unsupported(
                    pattern_span.clone(),
                    format!("unresolved nested payload type in match arm ({err:?})"),
                    "match-expression-substrate",
                );
                return None;
            }
        };
        // Register a generic-enum instantiation for the nested payload type
        // so MIR/codegen find its mangled layout (no-op for monomorphic
        // enums; the scrutinee registration only covers the outer type).
        self.try_register_enum_instantiation_ty(&payload_ty, pattern_span);
        let Some((registered_type, idx_usize, _)) =
            self.lookup_variant_ctor(&pvp.variant_match.variant_name, Some(&payload_ty))
        else {
            self.unsupported(
                pattern_span.clone(),
                "nested match-arm variant not registered in machine/enum ctor registry",
                "match-expression-substrate",
            );
            return None;
        };
        let variant_idx =
            u32::try_from(idx_usize).expect("variant index exceeds u32::MAX — impossible in Hew");
        let Ok(field_idx) = u32::try_from(pvp.field_idx) else {
            self.unsupported(
                pattern_span.clone(),
                "nested payload field index exceeds u32::MAX",
                "match-expression-substrate",
            );
            return None;
        };
        let mut bindings = Vec::with_capacity(pvp.bindings.len());
        for payload in &pvp.bindings {
            let ty = match ResolvedTy::from_ty(&payload.ty) {
                Ok(ty) => self.qualify_current_module_record_ty(ty),
                Err(err) => {
                    self.unsupported(
                        pattern_span.clone(),
                        format!("unresolved nested payload binding type in match arm ({err:?})"),
                        "match-expression-substrate",
                    );
                    return None;
                }
            };
            let Ok(inner_field_idx) = u32::try_from(payload.field_idx) else {
                self.unsupported(
                    pattern_span.clone(),
                    "nested payload binding field index exceeds u32::MAX",
                    "match-expression-substrate",
                );
                return None;
            };
            let binding_span = self.match_payload_binding_span(payload, pattern_span)?;
            let bound = self.bind(
                payload.binding_name.clone(),
                ty.clone(),
                false,
                binding_span,
            );
            bindings.push(HirMatchArmBinding {
                span: bound.span.clone(),
                binding: bound.id,
                field_idx: inner_field_idx,
                name: payload.binding_name.clone(),
                ty,
            });
        }
        let mut literals = Vec::with_capacity(pvp.literals.len());
        for predicate in &pvp.literals {
            let Ok(field_idx) = u32::try_from(predicate.field_idx) else {
                self.unsupported(
                    pattern_span.clone(),
                    "nested literal field index exceeds u32::MAX",
                    "match-expression-substrate",
                );
                return None;
            };
            let ty = match ResolvedTy::from_ty(&predicate.ty) {
                Ok(ty) => self.qualify_current_module_record_ty(ty),
                Err(err) => {
                    self.unsupported(
                        pattern_span.clone(),
                        format!("unresolved nested literal type ({err:?})"),
                        "match-expression-substrate",
                    );
                    return None;
                }
            };
            let (literal, _) = literal_to_hir(&predicate.literal);
            literals.push(HirPayloadPredicate {
                field_idx,
                literal,
                ty,
            });
        }
        let mut nested = Vec::with_capacity(pvp.nested.len());
        for child in &pvp.nested {
            nested.push(self.build_payload_variant_predicate(child, pattern_span)?);
        }
        Some(HirPayloadVariantPredicate {
            field_idx,
            payload_ty,
            variant_match: hew_types::VariantMatch {
                type_name: registered_type,
                variant_name: pvp.variant_match.variant_name.clone(),
            },
            variant_idx,
            bindings,
            literals,
            nested,
        })
    }
}
