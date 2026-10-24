use hew_parser::ast::{ConstDecl, Expr, Ident, Span, Spanned};

use super::scope::{Binding, Namespace, Resolution};
use super::types::{ConstValue, SourceConstant};
use super::Checker;
use crate::{DefId, Ty};

impl Checker {
    pub(super) fn check_constant_eligibility(&mut self, expression: &Spanned<Expr>) {
        struct Eligibility<'a> {
            checker: &'a Checker,
            failure: Option<(Span, &'static str)>,
        }
        impl super::NodeVisitor for Eligibility<'_> {
            fn visit_expr(&mut self, expr: &Expr, span: &Span) {
                let key = super::SpanKey::in_module(span, self.checker.current_module_idx);
                if self.failure.is_some()
                    || self.checker.compile_time_values.contains_key(&key)
                    || self
                        .checker
                        .expr_types
                        .get(&key)
                        .is_some_and(|ty| matches!(self.checker.subst.resolve(ty), Ty::Error))
                {
                    return;
                }
                let callable = self
                    .checker
                    .expr_types
                    .get(&key)
                    .map(|ty| self.checker.subst.resolve(ty))
                    .is_some_and(|ty| {
                        matches!(
                            ty,
                            Ty::Function { .. } | Ty::Closure { .. } | Ty::GenericCallable { .. }
                        )
                    });
                let construct = if callable
                    && matches!(expr, Expr::Ident(_) | Expr::FieldAccess { .. })
                    && !self.checker.constant_constructor(expr, span)
                {
                    Some("a function value")
                } else if self.checker.user_comparison_dispatch.contains_key(&key) {
                    Some("a user-defined comparison call")
                } else if self.checker.constant_non_scalar_comparison(expr) {
                    Some("a non-scalar comparison")
                } else if self
                    .checker
                    .indexed_place_operations
                    .get(&key)
                    .is_some_and(|(read, _)| matches!(read, crate::RuntimeCallFamily::Map(_)))
                {
                    Some("a map lookup invoking Hash and Eq methods")
                } else if matches!(expr, Expr::Index { .. })
                    && self.checker.method_call_rewrites.contains_key(&key)
                {
                    Some("an Index method call")
                } else {
                    match expr {
                        Expr::Literal(_)
                        | Expr::Ident(_)
                        | Expr::ContextVariant(_)
                        | Expr::Tuple(_)
                        | Expr::Array(_)
                        | Expr::ArrayRepeat { .. }
                        | Expr::MapLiteral { .. }
                        | Expr::StructInit { .. }
                        | Expr::RecordInitSuffix { .. }
                        | Expr::ByteStringLiteral(_)
                        | Expr::ByteArrayLiteral(_)
                        | Expr::FieldAccess { .. }
                        | Expr::Index { .. }
                        | Expr::Unary { .. }
                        | Expr::Binary { .. } => None,
                        Expr::Call { .. }
                        | Expr::MethodCall { .. }
                        | Expr::GenericApplySuffix { .. }
                            if self.checker.constant_constructor(expr, span) =>
                        {
                            None
                        }
                        Expr::Call { .. } | Expr::MethodCall { .. } => {
                            Some("an ordinary function call")
                        }
                        Expr::Cast { .. } => Some("a runtime cast"),
                        Expr::If { .. }
                        | Expr::IfLet { .. }
                        | Expr::Match { .. }
                        | Expr::Block(_) => Some("runtime control flow"),
                        Expr::Lambda { .. } | Expr::GenBlock { .. } => Some("a callable body"),
                        _ => Some("an expression requiring runtime evaluation"),
                    }
                };
                if let Some(construct) = construct {
                    self.failure = Some((span.clone(), construct));
                }
            }
        }
        let mut eligibility = Eligibility {
            checker: self,
            failure: None,
        };
        super::walk_expr(&expression.0, &expression.1, &mut eligibility);
        if let Some((span, construct)) = eligibility.failure {
            self.report_error_with_suggestions(crate::error::TypeErrorKind::ConstInitializer, &span,
                format!("constant initializer contains {construct}; constants use literal structure, scalar operators and constant references"),
                vec!["compute this value in a function with `let` or `var` instead".to_string()]);
        }
    }

    fn constant_constructor(&self, expr: &Expr, span: &Span) -> bool {
        if self.is_construct_call(span)
            || self
                .variant_constructor_plans
                .contains_key(&super::SpanKey::in_module(span, self.current_module_idx))
        {
            return true;
        }
        let (target, target_span) = match expr {
            Expr::Call { function, .. } => (&function.0, &function.1),
            Expr::GenericApplySuffix { target, .. } => (&target.0, &target.1),
            _ => (expr, span),
        };
        let resolution_span = match target {
            Expr::FieldAccess { field, .. } => &field.1,
            Expr::MethodCall { method, .. } => &method.1,
            _ => target_span,
        };
        matches!(
            self.scopes.resolutions().get(&super::SpanKey::in_module(
                resolution_span,
                self.current_module_idx
            )),
            Some(Resolution::Variant(..))
        )
    }

    fn constant_non_scalar_comparison(&self, expr: &Expr) -> bool {
        use hew_parser::ast::BinaryOp;
        let Expr::Binary {
            left,
            op:
                BinaryOp::Equal
                | BinaryOp::NotEqual
                | BinaryOp::Less
                | BinaryOp::LessEqual
                | BinaryOp::Greater
                | BinaryOp::GreaterEqual,
            ..
        } = expr
        else {
            return false;
        };
        self.expr_types
            .get(&super::SpanKey::in_module(&left.1, self.current_module_idx))
            .map(|ty| self.normalize_for_use(ty))
            .is_some_and(|ty| {
                !ty.is_numeric()
                    && !matches!(
                        ty,
                        Ty::Bool
                            | Ty::Char
                            | Ty::Unit
                            | Ty::Duration
                            | Ty::String
                            | Ty::Bytes
                            | Ty::Error
                    )
            })
    }

    pub(super) fn register_source_constant(
        &mut self,
        owner: Option<&str>,
        declaration: &ConstDecl,
        ty: Ty,
    ) {
        let module = owner
            .and_then(|owner| self.defs.module_for_path(owner))
            .or_else(|| self.defs.root_module());
        let Some(Binding::Const(id)) = module.and_then(|module| {
            self.scopes
                .item(self.scopes.namespace_of(module), declaration.name)
        }) else {
            return;
        };
        self.source_constants
            .entry(id)
            .or_insert(SourceConstant { ty, value: None });
    }

    pub(super) fn constant_declaration(&self, name: Ident) -> Option<DefId> {
        let site = self.scope_site()?;
        match self.scopes.value_head(&self.env, site, name)? {
            Resolution::Def(id) if self.defs.kind(id) == crate::DeclarationKind::Const => Some(id),
            _ => None,
        }
    }

    pub(super) fn constant_type(&self, name: Ident) -> Option<Ty> {
        let id = self.constant_declaration(name)?;
        self.source_constants
            .get(&id)
            .map(|constant| constant.ty.clone())
    }

    pub(super) fn coercible_literal_value(&self, name: Ident) -> Option<&ConstValue> {
        if let Some(id) = self.constant_declaration(name) {
            return self.source_constants.get(&id)?.value.as_ref();
        }
        self.env.lookup_ref(name)?;
        self.const_values.get(&name)
    }

    pub(super) fn constant_integer_value(&self, expr: &Spanned<Expr>) -> Option<i128> {
        let Expr::Ident(name) = expr.0 else {
            return None;
        };
        let id = self.constant_declaration(name)?;
        match self.source_constants.get(&id)?.value {
            Some(ConstValue::Integer(value)) => Some(value),
            _ => None,
        }
    }

    pub(super) fn qualified_constant_type(
        &mut self,
        object: &Spanned<Expr>,
        field: &Spanned<Ident>,
        span: &Span,
    ) -> Option<Ty> {
        let path = super::nominal_identity::value_path(&object.0, &object.1)?;
        let site = self.scope_site()?;
        let mut path = path;
        path.push(field.clone());
        let Resolution::Def(id) = self
            .scopes
            .resolve(&self.env, site, Namespace::Value, &path)
            .ok()?
        else {
            return None;
        };
        let ty = self.source_constants.get(&id)?.ty.clone();
        let visibility = self.defs.visibility(id);
        let accessor = self.scopes.contexts().def_module(path[0].0.ctx, site.file);
        let owner = self.defs.module(id);
        let permitted = match visibility {
            hew_parser::ast::Visibility::Pub => true,
            hew_parser::ast::Visibility::Private => owner == Some(accessor),
            hew_parser::ast::Visibility::Package => {
                owner.is_some_and(|owner| self.defs.same_package(owner, accessor))
            }
        };
        if !permitted {
            let error = crate::TypeError::visibility_violation(
                visibility,
                span.clone(),
                self.defs.name(id).as_str(),
                owner.map_or("(root)", |module| self.defs.module_path(module)),
                self.defs.module_path(accessor),
                self.defs
                    .site(id)
                    .map_or_else(|| span.clone(), crate::DeclarationOccurrence::span),
                owner.map(|module| self.defs.module_path(module).to_string()),
            );
            self.errors.push(error);
            return Some(Ty::Error);
        }
        Some(ty)
    }
}
