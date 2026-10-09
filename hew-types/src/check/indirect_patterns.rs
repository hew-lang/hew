use super::scope::Resolution;
use super::{
    CallableCandidate, Checker, IndirectCallCandidates, PatternKind, PayloadBinding,
    PayloadVariantPattern, SpanKey,
};
use hew_parser::ast::{Expr, Pattern, Spanned};

impl Checker {
    pub(super) fn record_pattern_value_sources(
        &mut self,
        pattern: &Spanned<Pattern>,
        ty: &crate::Ty,
        source: &Spanned<Expr>,
    ) {
        let candidates = self.callable_candidates_for_expr(&source.0, &source.1);
        self.record_pattern_candidate_sources(pattern, ty, &candidates);
    }

    fn bind_pattern_candidates(
        &mut self,
        span: &hew_parser::ast::Span,
        candidates: IndirectCallCandidates,
    ) {
        let key = SpanKey::in_module(span, self.current_module_idx);
        if let Some(Resolution::Local(binding)) = self.scopes.resolutions().get(&key) {
            self.env.set_value_candidates(*binding, candidates);
        }
    }

    pub(super) fn record_pattern_candidate_sources(
        &mut self,
        pattern: &Spanned<Pattern>,
        ty: &crate::Ty,
        source: &IndirectCallCandidates,
    ) {
        if let Pattern::Or(left, right) = &pattern.0 {
            self.record_pattern_candidate_sources(left, ty, source);
            self.record_pattern_candidate_sources(right, ty, source);
            return;
        }
        let key = SpanKey::in_module(&pattern.1, self.current_module_idx);
        if let Some(resolution) = self.pending_pattern_resolutions.get(&key).cloned() {
            if resolution.pattern_kind == PatternKind::Binding {
                self.bind_pattern_candidates(&pattern.1, source.clone());
                return;
            }
            let owner = if resolution.pattern_kind == PatternKind::StructPattern {
                match self.subst.resolve(ty) {
                    crate::Ty::Named { head, .. } => head.nominal(),
                    _ => None,
                }
            } else {
                None
            };
            self.record_payload_candidate_sources(
                &resolution.payload_bindings,
                &resolution.payload_variant_patterns,
                owner,
                source,
            );
            return;
        }
        if let Some(plan) = self.pending_pattern_plans.get(&key).cloned() {
            self.record_planned_pattern_candidates(pattern, ty, &plan, source);
            return;
        }
        match (&pattern.0, self.subst.resolve(ty)) {
            (Pattern::Tuple(patterns), crate::Ty::Tuple(types)) => {
                for (index, (pattern, ty)) in patterns.iter().zip(types).enumerate() {
                    let candidates = Self::project_pattern_candidates(source, None, index);
                    self.record_pattern_candidate_sources(pattern, &ty, &candidates);
                }
            }
            (Pattern::Identifier(_), _) => {
                self.bind_pattern_candidates(&pattern.1, source.clone());
            }
            _ => {}
        }
    }

    fn record_planned_pattern_candidates(
        &mut self,
        pattern: &Spanned<Pattern>,
        ty: &crate::Ty,
        plan: &super::types::PatternPlan,
        source: &IndirectCallCandidates,
    ) {
        let fields = match &pattern.0 {
            Pattern::RecordShorthand { fields, .. }
            | Pattern::NominalPath {
                payload: Some(hew_parser::ast::NominalPatternPayload::Record { fields, .. }),
                ..
            } => fields,
            Pattern::ContextVariant(context) => match context.payload.as_ref() {
                Some(hew_parser::ast::NominalPatternPayload::Record { fields, .. }) => fields,
                _ => return,
            },
            _ => return,
        };
        let crate::Ty::Named { head, .. } = self.subst.resolve(ty) else {
            return;
        };
        let Some(owner) = head.nominal() else {
            return;
        };
        for field in &plan.fields {
            let candidates = Self::project_pattern_candidates(
                source,
                Some(owner),
                usize::try_from(field.decl_idx).expect("checked field index"),
            );
            match &field.sub {
                super::types::PlanSub::Binding(_) => {
                    self.bind_pattern_candidates(&field.span, candidates);
                }
                super::types::PlanSub::Nested(_) => {
                    if let Some(pattern) = fields
                        .iter()
                        .find(|source| source.name.name.as_str() == field.name)
                        .and_then(|source| source.pattern.as_ref())
                    {
                        self.record_pattern_candidate_sources(pattern, &field.ty, &candidates);
                    }
                }
                super::types::PlanSub::Wildcard | super::types::PlanSub::Literal(_) => {}
            }
        }
    }

    fn project_pattern_candidates(
        source: &IndirectCallCandidates,
        owner: Option<crate::NominalId>,
        index: usize,
    ) -> IndirectCallCandidates {
        IndirectCallCandidates {
            known: source
                .known
                .iter()
                .map(|receiver| match owner {
                    Some(owner) => CallableCandidate::Field {
                        receiver: Box::new(receiver.clone()),
                        owner,
                        index: u32::try_from(index).expect("checked field index"),
                    },
                    None => CallableCandidate::Element {
                        receiver: Box::new(receiver.clone()),
                        index,
                    },
                })
                .collect(),
            may_be_unknown: source.may_be_unknown,
        }
    }

    fn record_payload_candidate_sources(
        &mut self,
        bindings: &[PayloadBinding],
        nested: &[PayloadVariantPattern],
        owner: Option<crate::NominalId>,
        source: &IndirectCallCandidates,
    ) {
        for binding in bindings {
            if let Some(span) = &binding.def_span {
                self.bind_pattern_candidates(
                    span,
                    Self::project_pattern_candidates(source, owner, binding.field_idx),
                );
            }
        }
        for pattern in nested {
            self.record_payload_candidate_sources(
                &pattern.bindings,
                &pattern.nested,
                None,
                &Self::project_pattern_candidates(source, owner, pattern.field_idx),
            );
        }
    }
}
