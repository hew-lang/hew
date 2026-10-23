//! Typed request and response declarations owned by each actor.

use super::{collect_program_actors, Checker, Program, TypeDef, TypeDefKind, VariantDef};
use crate::{DeclarationKind, NominalId};
use hew_parser::ast::Symbol;
use std::collections::HashMap;

impl Checker {
    pub(super) fn reject_interceptor_return(&mut self, ty: &crate::Ty, span: &super::Span) {
        if self.normalize_for_use(ty).contains_interceptor_token() {
            self.report_error(
                super::TypeErrorKind::InterceptEscape,
                span,
                "Call and Outcome may be returned only by an interceptor".to_string(),
            );
        }
    }

    pub(super) fn check_interceptor_storage(&mut self, item: &super::Item, span: &super::Span) {
        let name = match item {
            super::Item::TypeDecl(declaration) => declaration.name,
            super::Item::Record(declaration) => declaration.name,
            super::Item::Actor(declaration) => declaration.name,
            _ => return,
        };
        let identity = self.declaration_identity(name);
        let contains = self.type_def_at(&identity).is_some_and(|definition| {
            Self::structural_member_types_for_type(definition)
                .iter()
                .any(|ty| self.normalize_for_use(ty).contains_interceptor_token())
        });
        if contains {
            self.report_error(
                super::TypeErrorKind::InterceptEscape,
                span,
                "Call and Outcome cannot be stored in fields or enum payloads".to_string(),
            );
        }
    }

    pub(super) fn check_interceptor_method(
        &mut self,
        receiver: &super::Spanned<super::Expr>,
        ty: &crate::Ty,
        method: &str,
        args: &[super::CallArg],
        span: &super::Span,
    ) -> Option<crate::Ty> {
        use crate::actor_delivery::{nominal, ActorDeliveryCall, InterceptorOperation as Op};
        use crate::{KnownDecl, Ty};
        let Ty::Named {
            head,
            args: type_args,
        } = ty
        else {
            return None;
        };
        let [actor] = type_args.as_slice() else {
            return None;
        };
        let (operation, result) = if *head == KnownDecl::Call.head() {
            match method {
                "caller" => (Op::Caller, nominal(KnownDecl::Caller, Vec::new())),
                "handler_name" => (Op::HandlerName, Ty::String),
                "proceed" => (
                    Op::Proceed,
                    nominal(KnownDecl::Outcome, vec![actor.clone()]),
                ),
                "refuse" => (Op::Refuse, nominal(KnownDecl::Outcome, vec![actor.clone()])),
                _ => return None,
            }
        } else if *head == KnownDecl::Outcome.head() {
            match method {
                "failed" => (Op::Failed, Ty::Bool),
                "refused" => (Op::Refused, Ty::Bool),
                _ => return None,
            }
        } else {
            return None;
        };
        let expected = usize::from(operation == Op::Refuse);
        if args.len() != expected {
            self.report_error(
                super::TypeErrorKind::InvalidOperation,
                span,
                format!("interceptor method `{method}` expects {expected} arguments"),
            );
            return Some(Ty::Error);
        }
        for argument in args {
            let (expr, span) = argument.expr();
            self.check_against(expr, span, &Ty::String);
        }
        if operation.consumes() {
            if self.reject_borrowed_consumption(&receiver.0, &receiver.1) {
                return Some(Ty::Error);
            }
            self.mark_expr_moved(&receiver.0, &receiver.1);
        }
        self.actor_delivery_calls.insert(
            super::SpanKey::in_module(span, self.current_module_idx),
            ActorDeliveryCall::Intercept(operation),
        );
        self.record_submission_suspension(span, operation.consumes());
        Some(result)
    }

    pub(super) fn check_supervisor_interceptor(
        &mut self,
        supervisor: &hew_parser::ast::SupervisorDecl,
    ) {
        let identity = self.declaration_identity(supervisor.name);
        for (index, method) in supervisor.methods.iter().enumerate() {
            let intercepts = method
                .attributes
                .iter()
                .filter(|attribute| attribute.name.as_str() == "intercept")
                .count();
            let qualified = format!("{identity}::{}", method.name);
            let signature = self.fn_sig(&qualified).cloned();
            let valid_signature = signature.as_ref().is_some_and(|signature| {
                let Some(actor) = signature.type_params.last() else {
                    return false;
                };
                let actor = crate::Ty::param(*actor);
                signature.params.len() == 1
                    && signature.params[0].ty
                        == crate::actor_delivery::nominal(
                            crate::KnownDecl::Call,
                            vec![actor.clone()],
                        )
                    && signature.return_type
                        == crate::actor_delivery::nominal(crate::KnownDecl::Outcome, vec![actor])
            });
            if index != 0
                || intercepts != 1
                || method.type_params.as_ref().map_or(0, Vec::len) != 1
                || !valid_signature
            {
                self.report_error(
                    super::TypeErrorKind::InvalidOperation,
                    &method.decl_span,
                    "a supervisor may declare one #[intercept] fn<A>(Call<A>) -> Outcome<A>"
                        .to_string(),
                );
            }
            self.check_function_as(method, &qualified);
        }
    }

    pub(super) fn check_actor_interceptor(
        &mut self,
        actor: &hew_parser::ast::ActorDecl,
        identity: &str,
    ) {
        let mut seen = false;
        for method in &actor.methods {
            let intercepts: Vec<_> = method
                .attributes
                .iter()
                .filter(|attribute| attribute.name.as_str() == "intercept")
                .collect();
            if intercepts.is_empty() {
                continue;
            }
            let span = &intercepts[0].span;
            if seen || intercepts.len() != 1 {
                self.report_error(
                    super::TypeErrorKind::InvalidOperation,
                    span,
                    "an actor may declare only one interceptor".to_string(),
                );
            }
            seen = true;
            let qualified = format!("{identity}::{}", method.name);
            let Some(signature) = self.fn_sig(&qualified).cloned() else {
                continue;
            };
            let actor_ty = self
                .current_actor_type
                .as_ref()
                .expect("actor body has its receiver type")
                .clone();
            let actor_ty = crate::Ty::actor_handle_of(&actor_ty);
            let call =
                crate::actor_delivery::nominal(crate::KnownDecl::Call, vec![actor_ty.clone()]);
            let outcome = crate::actor_delivery::nominal(crate::KnownDecl::Outcome, vec![actor_ty]);
            if signature.params.len() != 1
                || signature.params[0].ty != call
                || signature.return_type != outcome
                || method
                    .type_params
                    .as_ref()
                    .is_some_and(|params| !params.is_empty())
                || method
                    .attributes
                    .iter()
                    .any(|attribute| attribute.name.as_str() == "on")
            {
                self.report_error(
                    super::TypeErrorKind::InvalidOperation,
                    span,
                    format!("an actor interceptor must take one Call<Self> and return Outcome<Self>; found {:?} -> {}", signature.params, signature.return_type.user_facing()),
                );
                continue;
            }
            let declaration = self.defs.lookup_path(&qualified);
            if let Some(protocol) = self.actor_protocol_descriptors.get_mut(identity) {
                protocol.interceptor = declaration;
            }
        }
    }

    pub(super) fn register_actor_intercept_types(&mut self, program: &Program) {
        for (owner, actor) in collect_program_actors(program, &self.defs) {
            let identity = self.defs.path(owner);
            let Some(actor_def) = self
                .type_defs
                .get(&NominalId::from_minted_declaration(owner))
                .cloned()
            else {
                continue;
            };
            let Some(protocol) = self.actor_protocol_descriptors.get(identity).cloned() else {
                continue;
            };
            for (kind, suffix) in [
                (DeclarationKind::ActorRequestType, "Request"),
                (DeclarationKind::ActorResponseType, "Response"),
                (DeclarationKind::ActorStreamType, "<Stream>"),
            ] {
                let Some(declaration) =
                    self.defs
                        .member_of_kind(owner, Symbol::intern(suffix), kind)
                else {
                    continue;
                };
                let mut variants = HashMap::new();
                for (receive, handler) in actor.receive_fns.iter().zip(&protocol.handlers) {
                    let variant = if kind == DeclarationKind::ActorRequestType {
                        VariantDef::Struct(
                            receive
                                .params
                                .iter()
                                .zip(&handler.param_tys)
                                .map(|(parameter, ty)| (parameter.name.to_string(), ty.to_ty()))
                                .collect(),
                        )
                    } else if kind == DeclarationKind::ActorStreamType {
                        if let Some(element) = handler.return_ty.to_ty().as_stream() {
                            VariantDef::Tuple(vec![crate::Ty::sink(element.clone())])
                        } else {
                            VariantDef::Unit
                        }
                    } else if receive.is_generator || handler.return_ty == crate::ResolvedTy::Unit {
                        VariantDef::Unit
                    } else {
                        VariantDef::Tuple(vec![handler.return_ty.to_ty()])
                    };
                    variants.insert(handler.name.clone(), variant);
                }
                let definition = TypeDef {
                    kind: TypeDefKind::Enum,
                    name: self.defs.path(declaration).to_string(),
                    type_params: actor_def.type_params.clone(),
                    bounds: actor_def.bounds.clone(),
                    fields: HashMap::new(),
                    field_order: Vec::new(),
                    variants,
                    implicit_variant_fields: HashMap::default(),
                    methods: HashMap::new(),
                    doc_comment: None,
                    is_indirect: false,
                };
                let head =
                    self.head_of_declaration(NominalId::from_minted_declaration(declaration));
                self.registry
                    .register_type(head, Self::structural_member_types_for_type(&definition));
                self.registry
                    .register_type_params(head, definition.type_params.clone());
                self.type_defs
                    .insert(NominalId::from_minted_declaration(declaration), definition);
            }
        }
    }
}
