use hew_parser::ast::{ConstDecl, Expr, Ident, Span, Spanned};

use super::scope::{Binding, Namespace, Resolution};
use super::types::{ConstValue, SourceConstant};
use super::Checker;
use crate::{DefId, Ty};

impl Checker {
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
