//! Checker entry points to the one `Serializable` admission check,
//! `TypeFactService::is_serializable`. The facade bound, the remote-actor
//! gates, impl obligations and the `#[wire]` declaration check all ask it, so
//! SIR's wire plan never meets a value the checker admitted without a codec.

use std::collections::BTreeMap;

use super::{Checker, TypeErrorKind};
use crate::traits::MarkerTrait;
use crate::{ResolvedTy, Ty, TypeFactService};

impl Checker {
    /// Whether the wire codec can encode and decode the concrete `ty`.
    pub(super) fn is_serializable(&self, ty: &ResolvedTy) -> bool {
        self.serializable_within(ty, &mut Vec::new())
    }

    fn serializable_within(&self, ty: &ResolvedTy, visiting: &mut Vec<String>) -> bool {
        let param = |name: &str, marker: MarkerTrait| match marker {
            MarkerTrait::Serializable => self.type_param_carries_bound(name, "Serializable"),
            marker => self.type_param_has_marker_bound(name, marker),
        };
        TypeFactService::new(self.type_fact_context(), BTreeMap::new())
            .serializable_within(ty, &param, visiting)
    }

    /// `ty: Serializable` in bound position. A type parameter declared
    /// `Serializable` stands for any admitted value except an `Option`, so
    /// `Vec<T>` is admitted and `Option<T>` is not.
    pub(super) fn satisfies_serializable(&self, ty: &Ty) -> bool {
        let ty = self.subst.resolve(ty).materialize_literal_defaults();
        // An unsettled type is decided when inference settles; an errored
        // one already carries its diagnostic.
        if ty.has_inference_var() || matches!(ty, Ty::Error) {
            return true;
        }
        let ty = self.normalize_for_use(&ty);
        ResolvedTy::from_ty_with_type_params(&ty, &self.current_type_param_names())
            .is_ok_and(|ty| self.is_serializable(&ty))
    }

    /// Refuse a `#[wire]` declaration whose members have no wire encoding,
    /// so its per-type codec methods never reach a lowering that cannot plan
    /// them.
    pub(super) fn validate_wire_type_encoding(
        &mut self,
        identity: &str,
        members: Vec<(String, Ty, hew_parser::ast::Span)>,
    ) {
        for (member, ty, span) in members {
            let ty = self.normalize_for_use(&ty);
            let admitted = ResolvedTy::from_ty(&ty)
                .is_ok_and(|ty| self.serializable_within(&ty, &mut vec![identity.to_string()]));
            if !admitted {
                self.report_error_with_suggestions(
                    TypeErrorKind::BoundsNotSatisfied,
                    &span,
                    format!(
                        "E_WIRE_MEMBER_NOT_SERIALIZABLE: {member} of `#[wire]` type `{}` has \
                         type `{}`, which has no wire encoding",
                        crate::short_name(identity),
                        ty.user_facing()
                    ),
                    vec![
                        "a wire member is a scalar, a `Vec`, `HashMap`, `HashSet` or `Option` \
                         of serializable values, or another `#[wire]` type that does not \
                         contain this one"
                            .to_string(),
                    ],
                );
            }
        }
    }
}
