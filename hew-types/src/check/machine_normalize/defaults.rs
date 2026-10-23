//! Declaration-site default expressions become ordinary checked function bodies.

use super::*;
use hew_parser::ast::{Param, TraitItem, TypeParam};

pub(super) fn contains_defaults(program: &Program) -> bool {
    fn has(item: &Item) -> bool {
        let params = |params: &[Param]| params.iter().any(|param| param.default.is_some());
        match item {
            Item::Function(function) => params(&function.params),
            Item::Impl(implementation) => implementation.methods.iter().any(|f| params(&f.params)),
            Item::Trait(declaration) => declaration
                .items
                .iter()
                .any(|item| matches!(item, TraitItem::Method(method) if params(&method.params))),
            _ => false,
        }
    }
    program.items.iter().any(|(item, _)| has(item))
        || program.module_graph.as_ref().is_some_and(|graph| {
            graph
                .modules
                .values()
                .any(|module| module.items.iter().any(|(item, _)| has(item)))
        })
}

impl Builder {
    pub(super) fn normalize_defaults(
        &mut self,
        item: &mut Item,
        errors: &mut Vec<TypeError>,
    ) -> Vec<Spanned<Item>> {
        let mut free = Vec::new();
        match item {
            Item::Function(function) => {
                for helper in self.default_helpers(function, &[], false) {
                    let span = helper.fn_span.clone();
                    free.push((Item::Function(helper), span));
                }
            }
            Item::Impl(implementation) => {
                let mut associated = Vec::new();
                for method in &mut implementation.methods {
                    if implementation.trait_bound.is_some() {
                        for param in &method.params {
                            if let Some(default) = &param.default {
                                errors.push(TypeError::new(
                                    TypeErrorKind::InvalidOperation,
                                    default.1.clone(),
                                    "trait impls inherit default arguments from the trait declaration; remove this default".to_string(),
                                ));
                            }
                        }
                    } else {
                        associated.extend(self.default_helpers(
                            method,
                            implementation.type_params.as_deref().unwrap_or_default(),
                            true,
                        ));
                    }
                }
                implementation.methods.splice(0..0, associated);
            }
            Item::Trait(declaration) => {
                let mut enclosing = declaration.type_params.clone().unwrap_or_default();
                let type_args = enclosing
                    .iter()
                    .map(|parameter| self.default_type(parameter.name))
                    .collect();
                enclosing.insert(
                    0,
                    TypeParam {
                        name: Ident::new("Self"),
                        bounds: vec![hew_parser::ast::TraitBound {
                            path: Path::single(declaration.name, self.span()),
                            type_args: Some(type_args),
                            assoc_type_bindings: Vec::new(),
                        }],
                    },
                );
                for member in &mut declaration.items {
                    let TraitItem::Method(method) = member else {
                        continue;
                    };
                    let mut function = FnDecl {
                        origin: DeclarationOrigin::Authored,
                        attributes: Vec::new(),
                        is_generator: false,
                        visibility: Visibility::Private,
                        name: method.name,
                        type_params: method.type_params.clone(),
                        params: method.params.clone(),
                        return_type: method.return_type.clone(),
                        where_clause: method.where_clause.clone(),
                        body: Block {
                            stmts: Vec::new(),
                            trailing_expr: None,
                        },
                        doc_comment: None,
                        decl_span: method.span.clone(),
                        fn_span: method.span.clone(),
                        intrinsic: None,
                        consumes_self: false,
                    };
                    for helper in self.default_helpers(&mut function, &enclosing, false) {
                        let span = helper.fn_span.clone();
                        free.push((Item::Function(helper), span));
                    }
                    method.params = function.params;
                }
            }
            _ => {}
        }
        free
    }

    fn default_type(&mut self, name: Ident) -> Spanned<TypeExpr> {
        (
            TypeExpr::Named {
                path: Path::single(name, self.span()),
                type_args: None,
            },
            self.span(),
        )
    }

    fn default_helpers(
        &mut self,
        function: &mut FnDecl,
        enclosing: &[TypeParam],
        associated: bool,
    ) -> Vec<FnDecl> {
        let template = function.clone();
        let mut helpers = Vec::new();
        for param in &mut function.params {
            let Some(default) = &param.default else {
                continue;
            };
            if param.default_provider.is_some() {
                continue;
            }
            let mut helper = template.clone();
            helper.name = Ident::new(&format!("_$default_{}", self.next_span));
            helper.origin = DeclarationOrigin::DefaultArgument {
                body_start: default.1.start,
                body_end: default.1.end,
            };
            helper.params.clear();
            helper.attributes.clear();
            helper.intrinsic = None;
            helper.is_generator = false;
            helper.consumes_self = false;
            helper.doc_comment = None;
            helper.return_type = Some(param.ty.clone());
            helper.body = Block {
                stmts: Vec::new(),
                trailing_expr: Some(Box::new(default.clone())),
            };
            // Free helpers explicitly bind the trait's receiver and parameters.
            // Associated helpers retain the impl scope through normal registration.
            if !associated {
                let mut parameters = enclosing.to_vec();
                parameters.extend(helper.type_params.take().unwrap_or_default());
                helper.type_params = (!parameters.is_empty()).then_some(parameters);
            }
            let parameters = enclosing
                .iter()
                .chain(template.type_params.iter().flatten());
            let type_args = parameters
                .map(|parameter| {
                    let span = param.ty.1.clone();
                    (
                        TypeExpr::Named {
                            path: Path::single(parameter.name, span.clone()),
                            type_args: None,
                        },
                        span,
                    )
                })
                .collect();
            param.default_provider = Some((helper.name, type_args, associated));
            let start = self.next_span;
            self.refresh_generics(helper.type_params.as_mut(), helper.where_clause.as_mut());
            self.refresh_signature(&mut helper);
            helper.fn_span = start..self.next_span;
            let source = default.1.clone();
            self.source_spans
                .insert(helper.fn_span.clone(), source.clone());
            self.source_spans.insert(helper.decl_span.clone(), source);
            helpers.push(helper);
        }
        helpers
    }
}
