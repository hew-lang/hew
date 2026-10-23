//! Checked default providers share ordinary call identity, effects and generics.

use super::types::GenericCallee;
#[allow(
    clippy::wildcard_imports,
    reason = "checker helpers share their parent semantic context"
)]
use super::*;

impl Checker {
    pub(super) fn constructor_signature(&self, owner: &str) -> Option<&FnSig> {
        self.lookup_declaration(owner)
            .and_then(|declaration| self.constructor_signatures.get(&declaration))
    }

    pub(super) fn register_default_parameters(
        &mut self,
        owner: &str,
        params: &[hew_parser::ast::Param],
        skip: usize,
    ) -> Vec<Option<DefaultArgumentProvider>> {
        params
            .iter()
            .skip(skip)
            .map(|param| {
                let (name, arguments, associated) = param.default_provider.as_ref()?;
                let key = if *associated {
                    let (owner, _) = owner.rsplit_once("::")?;
                    format!("{owner}::{name}")
                } else {
                    scoped_module_item_name(self.canonical_fn_owner(), name.name.as_str())
                        .unwrap_or_else(|| name.to_string())
                };
                let declaration = self.lookup_declaration(&key);
                let declaration = declaration.or_else(|| self.named_item_declaration(*name));
                let Some(declaration) = declaration else {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        &param.name_span,
                        "default argument has no declaration-owned provider".to_string(),
                    );
                    return None;
                };
                let type_args = arguments
                    .iter()
                    .map(|argument| self.resolve_type_expr(argument))
                    .collect();
                Some(DefaultArgumentProvider {
                    declaration,
                    type_args,
                    associated: *associated,
                })
            })
            .collect()
    }

    pub(super) fn inherit_default_parameters(
        &self,
        sig: &mut FnSig,
        implemented_trait: Option<crate::DefId>,
        receiver: &Ty,
    ) {
        let Some(trait_id) = implemented_trait else {
            return;
        };
        let Some(method) = &sig.impl_method else {
            return;
        };
        let Some((declaring, method_id)) = self.trait_method_ids_of(trait_id, &method.name) else {
            return;
        };
        let Some(declaration) = self.fn_sigs.get(&method_id) else {
            return;
        };
        let mut substitutions =
            HashMap::from([(crate::ParamHead::receiver(declaring), receiver.clone())]);
        if let Some(bound) = self
            .impl_method_binders
            .get(&method.declaration)
            .and_then(|binders| binders.implemented_trait.as_ref())
        {
            if let Some(info) = self.trait_info(declaring) {
                substitutions.extend(
                    info.type_params
                        .iter()
                        .copied()
                        .zip(bound.args.iter().cloned()),
                );
            }
        }
        let method_parameters = sig
            .type_params
            .iter()
            .skip(method.receiver_parameters.len());
        substitutions.extend(
            declaration
                .type_params
                .iter()
                .copied()
                .zip(method_parameters.map(|parameter| Ty::param(*parameter))),
        );
        sig.param_defaults = declaration
            .param_defaults
            .iter()
            .map(|provider| {
                provider.as_ref().map(|provider| {
                    let mut provider = provider.clone();
                    for argument in &mut provider.type_args {
                        *argument = argument.substitute_type_params_parallel(&substitutions);
                    }
                    provider
                })
            })
            .collect();
    }

    /// Check a provider application after all written arguments, with the same
    /// declaration identity and generic obligations as an explicit helper call.
    pub(super) fn check_default_arguments(
        &mut self,
        sig: &FnSig,
        supplied: &[Option<usize>],
        type_args: &[Ty],
        params: &[Ty],
        span: &Span,
    ) {
        let substitutions: HashMap<_, _> = sig
            .type_params
            .iter()
            .copied()
            .zip(type_args.iter().cloned())
            .collect();
        let mut defaults = Vec::new();
        for (slot, provider) in sig.param_defaults.iter().enumerate() {
            if supplied.contains(&Some(slot)) {
                continue;
            }
            let Some(provider) = provider else { continue };
            let Some(signature) = self.fn_sigs.get(&provider.declaration).cloned() else {
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    "default provider has no checked signature".to_string(),
                );
                continue;
            };
            let arguments: Vec<_> = provider
                .type_args
                .iter()
                .map(|argument| argument.substitute_type_params_parallel(&substitutions))
                .map(|argument| self.project_assoc_types(&self.subst.resolve(&argument)))
                .collect();
            let provider_substitution: HashMap<_, _> = signature
                .type_params
                .iter()
                .copied()
                .zip(arguments.iter().cloned())
                .collect();
            let result = signature
                .return_type
                .substitute_type_params_parallel(&provider_substitution);
            if let Some(expected) = params.get(slot) {
                let actual = self.project_assoc_types(&result);
                if !self.try_unify_with_owner_identity(expected, &actual) {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!("default argument has type `{actual}`, expected `{expected}`"),
                    );
                }
            }
            self.enforce_signature_bounds(&signature, &arguments, span);
            let call_span = self.next_default_span..self.next_default_span + 1;
            self.next_default_span += 2;
            let key = self.defs.path(provider.declaration).to_string();
            self.record_call_edge(&key);
            self.record_concrete_call_type_args(&call_span, &arguments);
            self.record_generic_application(
                GenericCallee::Function { key: &key },
                &signature.type_params,
                &arguments,
                &call_span,
            );
            let target = if provider.associated {
                CallTarget::ImplMethod(provider.declaration)
            } else {
                CallTarget::User(provider.declaration)
            };
            self.record_direct_call_target(&call_span, target.clone());
            self.record_invocation_effect(&call_span, None, None, "default argument".to_string());
            self.record_default_call_sources(&call_span, provider.declaration);
            self.expr_types.insert(
                SpanKey::in_module(&call_span, self.current_module_idx),
                result,
            );
            defaults.push(CheckedDefaultArgument {
                slot,
                target,
                span: call_span,
            });
        }
        if !defaults.is_empty() {
            self.call_default_arguments
                .insert(SpanKey::in_module(span, self.current_module_idx), defaults);
        }
    }
}
