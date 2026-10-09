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
        self.record_arm_resolution(&pattern.0, &pattern.1, ty);
        let key = SpanKey::in_module(&pattern.1, self.current_module_idx);
        let Some(resolution) = self.pending_pattern_resolutions.get(&key).cloned() else {
            return;
        };
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
    }

    fn record_payload_candidate_sources(
        &mut self,
        bindings: &[PayloadBinding],
        nested: &[PayloadVariantPattern],
        owner: Option<crate::NominalId>,
        source: &IndirectCallCandidates,
    ) {
        let project = |index: usize| IndirectCallCandidates {
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
        };
        for binding in bindings {
            if let Some(span) = &binding.def_span {
                self.bind_pattern_candidates(span, project(binding.field_idx));
            }
        }
        for pattern in nested {
            self.record_payload_candidate_sources(
                &pattern.bindings,
                &pattern.nested,
                None,
                &project(pattern.field_idx),
            );
        }
    }
}
