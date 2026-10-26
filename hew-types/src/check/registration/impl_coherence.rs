//! Source impl admission precedes associated-type and method publication.

use std::collections::HashMap;

use hew_parser::ast::{ImplDecl, Span};

use super::super::types::{SourceImplDeclaration, SourceImplOrigin, TraitImplArgs};
use super::super::Checker;
use crate::def_table::PackageOrigin;
use crate::error::{TypeError, TypeErrorKind};
use crate::module_registry::ModuleAnchor;
use crate::{DeclarationKind, DeclarationOccurrence, DefId};

impl Checker {
    pub(in crate::check) fn assign_module_packages(&mut self) {
        let provider = self.input_provider.clone();
        let sources: Vec<_> = self
            .defs
            .modules()
            .filter_map(|(module, _)| {
                self.defs
                    .module_source(module)
                    .map(|source| (module, source.to_path_buf()))
            })
            .collect();
        for (module, source) in sources {
            let Some(provider) = &provider else {
                let package = if self
                    .module_registry
                    .source_has_stdlib_authority(&source, self.defs.module_path(module))
                {
                    PackageOrigin::StandardLibrary
                } else {
                    PackageOrigin::Compilation
                };
                self.defs.set_module_package(module, package);
                continue;
            };
            match provider.package_anchor(&source) {
                Ok(anchor) => {
                    let package = match anchor {
                        ModuleAnchor::Std { .. } => PackageOrigin::StandardLibrary,
                        ModuleAnchor::Package { dir, .. } => PackageOrigin::Manifest(dir),
                        ModuleAnchor::Loose => PackageOrigin::Compilation,
                    };
                    self.defs.set_module_package(module, package);
                }
                Err(cause) => {
                    let mut error =
                        TypeError::new(TypeErrorKind::InvalidOperation, 0..0, cause.message);
                    error.source_module = Some(source.display().to_string());
                    self.errors.push(error);
                }
            }
        }
    }

    fn declaration_package(&self, declaration: DefId) -> PackageOrigin {
        self.defs
            .module(declaration)
            .map_or(PackageOrigin::StandardLibrary, |module| {
                self.defs.module_package(module).clone()
            })
    }

    fn impl_target_display(&self, target: &crate::Ty) -> String {
        fn surface(checker: &Checker, ty: &crate::Ty) -> crate::Ty {
            let mut ty = ty.map_children_pub(&|child| surface(checker, child));
            if let crate::Ty::Named {
                head: crate::TypeHead::Nominal(head) | crate::TypeHead::Actor(head),
                ..
            } = &mut ty
            {
                head.spelling =
                    hew_parser::ast::Symbol::intern(checker.defs.display(head.id.declaration()));
            }
            ty
        }
        surface(self, target).user_facing().to_string()
    }

    fn orphan_impl_error(
        &self,
        declaration: DefId,
        trait_id: DefId,
        target: &crate::Ty,
        span: &Span,
    ) -> Option<TypeError> {
        let package = self.declaration_package(declaration);
        let receiver = match target.head() {
            Some(crate::TypeHead::Nominal(head) | crate::TypeHead::Actor(head)) => {
                Some(head.id.declaration())
            }
            _ => None,
        };
        let receiver_package = receiver.map_or(PackageOrigin::StandardLibrary, |owner| {
            self.declaration_package(owner)
        });
        if package == self.declaration_package(trait_id) || package == receiver_package {
            return None;
        }
        let trait_name = self.defs.display(trait_id);
        let mut error = TypeError::new(
            TypeErrorKind::OrphanImpl,
            span.clone(),
            format!("cannot implement trait `{trait_name}` for `{}`: this package owns neither the trait nor the outer receiver type", self.impl_target_display(target)),
        ).with_suggestion("move the impl to the package defining the trait or receiver, or define a local wrapper type");
        for (owner, label) in std::iter::once((trait_id, "trait defined here"))
            .chain(receiver.map(|owner| (owner, "outer receiver type defined here")))
        {
            if let Some(site) = self.defs.site(owner) {
                let source = site
                    .module()
                    .and_then(|module| self.defs.module_source(module))
                    .map(|source| source.display().to_string());
                error = error.with_note_source(site.span(), label, source);
            }
        }
        Some(error)
    }

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
            let type_name = self.impl_target_display(&head.target);
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
                format!("conflicting implementation of trait `{trait_name}` for `{type_name}`: the impl heads overlap"),
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

    /// Re-registration is identified by the block declaration.
    /// Bootstrap rows keep their prelude provenance on import revisits: an
    /// explicit source impl may override them, but not another source impl.
    #[expect(
        clippy::too_many_lines,
        reason = "validates and publishes one implementation admission transaction"
    )]
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
        if owner.is_some_and(|owner| {
            self.lang_items
                .get(crate::LangItem::Machine.key())
                .is_some_and(|binding| binding.trait_id == Some(owner))
        }) && !id
            .methods
            .iter()
            .any(|method| method.origin == hew_parser::ast::DeclarationOrigin::MachineCompanion)
        {
            let error = TypeError::new(
                TypeErrorKind::InvalidOperation,
                span.clone(),
                "the Machine interface is generated by a machine declaration; handwritten implementations cannot replace its checked evaluator",
            );
            return self.reject_source_impl(declaration, error);
        }
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
        let bounds = self.collect_type_param_bounds(
            id.type_params.as_ref(),
            id.where_clause.as_ref(),
            &mut Vec::new(),
        );
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
            bounds,
        };
        let replaces_prelude = self.source_impl_declarations.get(&key).is_some_and(|rows| {
            rows.iter()
                .any(|row| row.origin == SourceImplOrigin::Prelude && head.same_head(&row.head))
        });
        if origin == SourceImplOrigin::Source && !replaces_prelude {
            if let Some(error) = owner.and_then(|trait_id| {
                self.orphan_impl_error(declaration, trait_id, &head.target, span)
            }) {
                return self.reject_source_impl(declaration, error);
            }
        }
        let previous = self
            .source_impl_declarations
            .iter()
            .filter(|((receiver, trait_id), _)| {
                *trait_id == owner && (owner.is_some() || receiver == &key.0)
            })
            .flat_map(|(_, rows)| rows)
            .find(|row| {
                row.origin == origin
                    && (if owner.is_some() {
                        head.overlaps(&row.head)
                    } else {
                        head.same_head(&row.head)
                    })
                    && (owner.is_some()
                        || methods.keys().any(|name| row.methods.contains_key(name)))
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
