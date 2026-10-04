//! Source impl admission precedes associated-type and method publication.

use std::collections::HashMap;

use hew_parser::ast::{ImplDecl, Span};

use super::super::types::{SourceImplDeclaration, SourceImplOrigin, TraitImplArgs};
use super::super::Checker;
use crate::error::{TypeError, TypeErrorKind};
use crate::{DeclarationKind, DeclarationOccurrence, DefId};

impl Checker {
    fn source_impl_declaration(&self, span: &Span) -> Option<DefId> {
        self.defs
            .declaration(DeclarationOccurrence::new_with_synthetic_ordinal(
                self.current_declaration_module(),
                span,
                self.current_item_ordinal,
                DeclarationKind::ImplBlock,
                0,
            ))
    }

    pub(in crate::check) fn source_impl_is_rejected(&self, span: &Span) -> bool {
        self.source_impl_declaration(span)
            .is_some_and(|declaration| self.rejected_impl_declarations.contains(&declaration))
    }

    fn impl_diagnostic_source(&self, declaration: DefId) -> Option<String> {
        self.defs
            .site(declaration)
            .and_then(DeclarationOccurrence::module)
            .and_then(|module| self.defs.module_source(module))
            .map(|source| source.display().to_string())
            .or_else(|| self.current_module.clone())
    }

    fn reject_source_impl(&mut self, declaration: DefId, mut error: TypeError) -> bool {
        error.source_module = self.impl_diagnostic_source(declaration);
        self.errors.push(error);
        self.rejected_impl_declarations.insert(declaration);
        false
    }

    fn duplicate_impl_error(
        &self,
        id: &ImplDecl,
        span: &Span,
        head: &TraitImplArgs,
        owner: Option<DefId>,
        previous: SourceImplDeclaration,
    ) -> TypeError {
        if let Some(trait_id) = owner {
            let trait_name = self.defs.display(trait_id).to_string();
            let type_name = head.target.user_facing().to_string();
            let first = self
                .defs
                .site(previous.declaration)
                .expect("admitted source impl has an occurrence");
            TypeError::new(
                TypeErrorKind::ConflictingTraitImpl {
                    trait_name: trait_name.clone(),
                    type_name: type_name.clone(),
                },
                span.clone(),
                format!("conflicting implementation of trait `{trait_name}` for `{type_name}`: this impl head is already implemented"),
            ).with_note_source(first.span(), "previous implementation here", previous.source_module)
        } else {
            let method = id
                .methods
                .iter()
                .find(|method| previous.methods.contains_key(&method.name.name))
                .expect("conflicting inherent impls share a method");
            TypeError::new(
                TypeErrorKind::DuplicateDefinition,
                method.fn_span.clone(),
                format!("`{}` is defined multiple times for this type", method.name),
            )
            .with_note_source(
                previous.methods[&method.name.name].clone(),
                "previous definition here",
                previous.source_module,
            )
        }
    }

    /// Re-registration is identified by the block declaration, never by its
    /// spelling or receiver shape. Distinct heads retain the current dispatch
    /// policy, including user-record concrete specialisations.
    /// Bootstrap rows keep their prelude provenance on import revisits: an
    /// explicit source impl may override them, but not another source impl.
    pub(in crate::check) fn admit_source_impl(
        &mut self,
        id: &ImplDecl,
        span: &Span,
        origin: SourceImplOrigin,
    ) -> bool {
        let Some(declaration) = self.source_impl_declaration(span) else {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                "internal compiler error: source impl has no declaration identity".to_string(),
            );
            return false;
        };
        if self.rejected_impl_declarations.contains(&declaration) {
            return false;
        }
        let target = self
            .current_impl_target()
            .expect("impl admission has a resolved receiver");
        let trait_ref = self.impl_trait_ref(id);
        let target = self.normalize_for_use(&target);
        let Some(receiver) = Self::impl_self_key(&target) else {
            // Target resolution owns the diagnostic for an invalid receiver.
            return true;
        };
        let owner = trait_ref.as_ref().map(|bound| bound.trait_id);
        let key = (receiver, owner);
        if self
            .source_impl_declarations
            .get(&key)
            .is_some_and(|rows| rows.iter().any(|row| row.declaration == declaration))
        {
            return true;
        }
        let mut methods = HashMap::new();
        for method in &id.methods {
            if let Some(previous) = methods.insert(method.name.name, method.fn_span.clone()) {
                let source = self.impl_diagnostic_source(declaration);
                let error = TypeError::new(
                    TypeErrorKind::DuplicateDefinition,
                    method.fn_span.clone(),
                    format!("`{}` is defined multiple times in this impl", method.name),
                )
                .with_note_source(previous, "previous definition here", source);
                return self.reject_source_impl(declaration, error);
            }
        }
        // An unresolved trait is not an inherent impl; its normal trait
        // resolution diagnostic must not acquire an unrelated method conflict.
        if id.trait_bound.is_some() && trait_ref.is_none() {
            return true;
        }
        let params =
            self.source_parameter_heads(id.type_params.as_deref().unwrap_or_default(), span);
        let head = TraitImplArgs {
            target,
            args: trait_ref.as_ref().map_or_else(Vec::new, |bound| {
                bound
                    .args
                    .iter()
                    .map(|arg| self.normalize_for_use(arg))
                    .collect()
            }),
            params,
        };
        let previous = self
            .source_impl_declarations
            .get(&key)
            .and_then(|rows| {
                rows.iter().find(|row| {
                    row.origin == origin
                        && head.same_head(&row.head)
                        && (owner.is_some()
                            || methods.keys().any(|name| row.methods.contains_key(name)))
                })
            })
            .cloned();
        if let Some(previous) = previous {
            let error = self.duplicate_impl_error(id, span, &head, owner, previous);
            return self.reject_source_impl(declaration, error);
        }
        let source_module = self.impl_diagnostic_source(declaration);
        self.source_impl_declarations
            .entry(key)
            .or_default()
            .push(SourceImplDeclaration {
                declaration,
                origin,
                head,
                methods,
                source_module,
            });
        true
    }
}
