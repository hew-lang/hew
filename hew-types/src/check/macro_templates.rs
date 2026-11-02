use super::scope::{Namespace, Resolution, ScopeSite};
use super::Checker;
use crate::env::TypeEnv;
use crate::error::TypeErrorKind;
use crate::{Ty, TypeError};
use hew_parser::ast::{
    Block, ConditionItem, Expr, FnDecl, Ident, Item, MatchArm, NominalPatternPayload, Param, Path,
    Pattern, PatternField, Program, RecordKind, Span, Spanned, Stmt, StringPart, TraitBound,
    TraitItem, TypeBodyItem, TypeExpr, TypeParam, VariantKind, Visibility, WhereClause,
};
use hew_parser::macro_syntax::{MacroTemplate, Origin, TemplateBody};

impl Checker {
    pub(super) fn validate_macro_templates(&mut self, program: &Program) {
        for template in &program.expansions.templates {
            let Some(file) = self.defs.module_for_source(&template.source) else {
                continue;
            };
            let mut validator = TemplateNames {
                checker: self,
                template,
                site: ScopeSite {
                    file,
                    span_file: 0,
                    publish: false,
                },
                env: TypeEnv::new(),
                types: TypeEnv::new(),
            };
            let mut body = template.body.clone();
            let result = match &mut body {
                TemplateBody::Expr(expr) => validator.expr(expr),
                TemplateBody::Items(items) => {
                    for (item, _) in items.iter() {
                        validator.declare_item(item);
                    }
                    items
                        .iter_mut()
                        .try_for_each(|(item, span)| validator.item(item, span))
                }
            };
            if let Err(error) = result {
                self.errors.push(*error);
            }
        }
    }
}

/// Only binding and visibility are checked here. Invocation-specific types and
/// ownership remain with ordinary semantic checking after expansion.
struct TemplateNames<'a> {
    checker: &'a mut Checker,
    template: &'a MacroTemplate,
    site: ScopeSite,
    env: TypeEnv,
    types: TypeEnv,
}

impl TemplateNames<'_> {
    fn captured(&self, span: &Span) -> bool {
        matches!(self.template.origin(span), Some(Origin::Caller { .. }))
    }

    fn fail(&self, message: String, span: &Span) -> Box<TypeError> {
        let original = match self.template.origin(span) {
            Some(Origin::Template { span, .. } | Origin::Caller { span, .. }) => span.clone(),
            None => span.clone(),
        };
        let mut error = TypeError::new(TypeErrorKind::InvalidOperation, original, message);
        error.source_module = Some(self.template.source.to_string_lossy().into_owned());
        Box::new(error)
    }

    fn path(&mut self, path: &Path, namespace: Namespace) -> Result<(), Box<TypeError>> {
        let Some((name, span)) = path.segments.first() else {
            return Ok(());
        };
        if self.captured(span) {
            return Ok(());
        }
        if matches!(namespace, Namespace::Type)
            && path.segments.len() == 1
            && Ty::from_name(name.name.as_str()).is_some()
        {
            return Ok(());
        }
        let bound = match namespace {
            Namespace::Value => self.env.lookup_ref(name),
            Namespace::Type => self.types.lookup_ref(name),
        };
        if bound.is_some() {
            return Ok(());
        }
        let resolved =
            self.checker
                .scopes
                .resolve_prefix(&self.env, self.site, namespace, &path.segments);
        let (resolved, remaining) = match resolved {
            Ok(value) => value,
            Err(_) if matches!(namespace, Namespace::Value) && self.checker.builtin_fn_sigs.contains_key(&name.name) => return Ok(()),
            Err(_) => return Err(self.fail(format!("E_MACRO_UNBOUND_NAME: `{name}` is not defined in this macro's module or template"), span)),
        };
        if let Resolution::Module(_) = resolved {
            if let Some((name, span)) = remaining.first() {
                return Err(self.fail(
                    format!("E_MACRO_UNBOUND_NAME: module has no item `{name}`"),
                    span,
                ));
            }
        }
        let declaration = match resolved {
            Resolution::Def(id) | Resolution::Member(id) => Some(id),
            Resolution::Nominal(id) => Some(id.declaration()),
            _ => None,
        };
        if self.template.visibility == Visibility::Pub {
            let mut current = declaration;
            while let Some(id) = current {
                if self.checker.defs.visibility(id) != Visibility::Pub {
                    return Err(self.fail(format!("E_MACRO_PRIVATE_REFERENCE: a public macro cannot reference private item `{}`", self.checker.defs.display(id)), span));
                }
                current = self.checker.defs.owner(id);
            }
        }
        Ok(())
    }

    fn bind(&mut self, name: Ident) {
        self.env.define(name, Ty::Error, false);
    }
    fn push(&mut self) {
        self.env.push_scope();
        self.types.push_scope();
    }
    fn pop(&mut self) {
        self.env.pop_scope();
        self.types.pop_scope();
    }

    fn declare_item(&mut self, item: &Item) {
        let (name, nominal) = match item {
            Item::Function(item) => (item.name, false),
            Item::Const(item) => (item.name, false),
            Item::TypeDecl(item) => (item.name, true),
            Item::TypeAlias(item) => (item.name, true),
            Item::Record(item) => (item.name, true),
            Item::Trait(item) => (item.name, true),
            Item::Actor(item) => (item.name, true),
            Item::Supervisor(item) => (item.name, true),
            Item::Machine(item) => (item.name, true),
            _ => return,
        };
        self.bind(name);
        if nominal {
            self.types.define(name, Ty::Error, false);
        }
    }

    fn bound(&mut self, bound: &TraitBound) -> Result<(), Box<TypeError>> {
        self.path(&bound.path, Namespace::Type)?;
        for ty in bound.type_args.iter().flatten() {
            self.ty(ty)?;
        }
        for binding in &bound.assoc_type_bindings {
            self.ty(&binding.ty)?;
        }
        Ok(())
    }

    fn generics(
        &mut self,
        params: &[TypeParam],
        clause: Option<&WhereClause>,
    ) -> Result<(), Box<TypeError>> {
        for parameter in params {
            self.types.define(parameter.name, Ty::Error, false);
        }
        for parameter in params {
            for bound in &parameter.bounds {
                self.bound(bound)?;
            }
        }
        if let Some(clause) = clause {
            for predicate in &clause.predicates {
                self.ty(&predicate.ty)?;
                for bound in &predicate.bounds {
                    self.bound(bound)?;
                }
            }
        }
        Ok(())
    }

    fn ty(&mut self, (ty, span): &Spanned<TypeExpr>) -> Result<(), Box<TypeError>> {
        if self.captured(span) {
            return Ok(());
        }
        match ty {
            TypeExpr::Named { path, type_args } => {
                self.path(path, Namespace::Type)?;
                for ty in type_args.iter().flatten() {
                    self.ty(ty)?;
                }
            }
            TypeExpr::QualifiedAssocPath(path) => {
                self.ty(&path.base)?;
                self.path(&path.trait_path, Namespace::Type)?;
            }
            TypeExpr::Fallible { success, error }
            | TypeExpr::Result {
                ok: success,
                err: error,
            } => {
                self.ty(success)?;
                self.ty(error)?;
            }
            TypeExpr::Option(ty)
            | TypeExpr::Slice(ty)
            | TypeExpr::Borrow(ty)
            | TypeExpr::Array { element: ty, .. }
            | TypeExpr::Pointer { pointee: ty, .. } => self.ty(ty)?,
            TypeExpr::Tuple(types) => {
                for ty in types {
                    self.ty(ty)?;
                }
            }
            TypeExpr::Function {
                params,
                return_type,
                ..
            } => {
                for ty in params {
                    self.ty(&ty.ty)?;
                }
                self.ty(return_type)?;
            }
            TypeExpr::ActorFn {
                params,
                return_type,
            } => {
                for ty in params {
                    self.ty(ty)?;
                }
                self.ty(return_type)?;
            }
            TypeExpr::TraitObject(bounds) => {
                for bound in bounds {
                    self.bound(bound)?;
                }
            }
            TypeExpr::Infer => {}
        }
        Ok(())
    }

    fn pattern(&mut self, (pattern, span): &Spanned<Pattern>) -> Result<(), Box<TypeError>> {
        match pattern {
            Pattern::Identifier(name) => self.bind(*name),
            Pattern::NominalPath { path, payload } => {
                if !self.captured(span) {
                    self.path(path, Namespace::Type)?;
                }
                if let Some(payload) = payload {
                    self.payload(payload)?;
                }
            }
            Pattern::ContextVariant(variant) => {
                if let Some(payload) = &variant.payload {
                    self.payload(payload)?;
                }
            }
            Pattern::RecordShorthand { fields, .. } => self.pattern_fields(fields)?,
            Pattern::Tuple(patterns) => {
                for pattern in patterns {
                    self.pattern(pattern)?;
                }
            }
            Pattern::Or(left, right) => {
                self.pattern(left)?;
                self.pattern(right)?;
            }
            Pattern::Regex { captures, .. } => {
                for name in captures {
                    self.bind(Ident::new(name));
                }
            }
            Pattern::Wildcard | Pattern::Literal(_) => {}
        }
        Ok(())
    }
    fn payload(&mut self, payload: &NominalPatternPayload) -> Result<(), Box<TypeError>> {
        match payload {
            NominalPatternPayload::Tuple(patterns) => {
                for pattern in patterns {
                    self.pattern(pattern)?;
                }
            }
            NominalPatternPayload::Record { fields, .. } => self.pattern_fields(fields)?,
        }
        Ok(())
    }
    fn pattern_fields(&mut self, fields: &[PatternField]) -> Result<(), Box<TypeError>> {
        for field in fields {
            if let Some(pattern) = &field.pattern {
                self.pattern(pattern)?;
            } else {
                self.bind(field.name);
            }
        }
        Ok(())
    }
    fn params(
        &mut self,
        params: &[Param],
        result: Option<&Spanned<TypeExpr>>,
    ) -> Result<(), Box<TypeError>> {
        for param in params {
            self.ty(&param.ty)?;
            self.bind(param.name);
        }
        if let Some(result) = result {
            self.ty(result)?;
        }
        Ok(())
    }
    fn function(&mut self, function: &mut FnDecl) -> Result<(), Box<TypeError>> {
        self.push();
        self.generics(
            function.type_params.as_deref().unwrap_or_default(),
            function.where_clause.as_ref(),
        )?;
        self.params(&function.params, function.return_type.as_ref())?;
        self.block(&mut function.body)?;
        self.pop();
        Ok(())
    }
    fn conditions(&mut self, conditions: &mut [ConditionItem]) -> Result<(), Box<TypeError>> {
        for condition in conditions {
            match condition {
                ConditionItem::Let { pattern, expr } => {
                    self.expr(expr)?;
                    self.pattern(pattern)?;
                }
                ConditionItem::Expr(expr) => self.expr(expr)?,
            }
        }
        Ok(())
    }
    #[expect(
        clippy::too_many_lines,
        reason = "one exhaustive AST dispatch keeps template scope transitions visible"
    )]
    fn item(&mut self, item: &mut Item, _span: &Span) -> Result<(), Box<TypeError>> {
        self.push();
        match item {
            Item::Function(function) => self.function(function)?,
            Item::Const(constant) => {
                self.ty(&constant.ty)?;
                self.expr(&mut constant.value)?;
            }
            Item::TypeDecl(declaration) => {
                self.types.define(Ident::new("Self"), Ty::Error, false);
                self.generics(
                    declaration.type_params.as_deref().unwrap_or_default(),
                    declaration.where_clause.as_ref(),
                )?;
                for member in &mut declaration.body {
                    match member {
                        TypeBodyItem::Field { ty, .. } => self.ty(ty)?,
                        TypeBodyItem::Variant(variant) => match &variant.kind {
                            VariantKind::Unit => {}
                            VariantKind::Tuple(types) => {
                                for ty in types {
                                    self.ty(ty)?;
                                }
                            }
                            VariantKind::Struct(fields) => {
                                for (_, ty) in fields {
                                    self.ty(ty)?;
                                }
                            }
                        },
                    }
                }
            }
            Item::TypeAlias(alias) => {
                self.generics(alias.type_params.as_deref().unwrap_or_default(), None)?;
                self.ty(&alias.ty)?;
            }
            Item::Record(record) => {
                self.generics(
                    record.type_params.as_deref().unwrap_or_default(),
                    record.where_clause.as_ref(),
                )?;
                match &record.kind {
                    RecordKind::Named(fields) => {
                        for field in fields {
                            self.ty(&field.ty)?;
                        }
                    }
                    RecordKind::Tuple(types) => {
                        for ty in types {
                            self.ty(ty)?;
                        }
                    }
                }
            }
            Item::Impl(declaration) => {
                self.types.define(Ident::new("Self"), Ty::Error, false);
                self.generics(
                    declaration.type_params.as_deref().unwrap_or_default(),
                    declaration.where_clause.as_ref(),
                )?;
                if let Some(bound) = &declaration.trait_bound {
                    self.bound(bound)?;
                }
                self.ty(&declaration.target_type)?;
                for alias in &declaration.type_aliases {
                    self.ty(&alias.ty)?;
                }
                for function in &mut declaration.methods {
                    self.function(function)?;
                }
            }
            Item::Trait(declaration) => {
                self.types.define(Ident::new("Self"), Ty::Error, false);
                self.generics(declaration.type_params.as_deref().unwrap_or_default(), None)?;
                for member in &mut declaration.items {
                    match member {
                        TraitItem::Method(method) => {
                            self.push();
                            self.generics(
                                method.type_params.as_deref().unwrap_or_default(),
                                method.where_clause.as_ref(),
                            )?;
                            self.params(&method.params, method.return_type.as_ref())?;
                            if let Some(body) = &mut method.body {
                                self.block(body)?;
                            }
                            self.pop();
                        }
                        TraitItem::AssociatedType {
                            bounds, default, ..
                        } => {
                            for bound in bounds {
                                self.bound(bound)?;
                            }
                            if let Some(ty) = default {
                                self.ty(ty)?;
                            }
                        }
                    }
                }
            }
            Item::Actor(actor) => {
                self.types.define(Ident::new("Self"), Ty::Error, false);
                self.bind(Ident::new("self"));
                self.generics(&actor.type_params, None)?;
                for field in &actor.fields {
                    self.ty(&field.ty)?;
                    self.bind(field.name);
                }
                for receiver in &actor.receive_fns {
                    self.bind(receiver.name);
                }
                for method in &actor.methods {
                    self.bind(method.name);
                }
                if let Some(init) = &mut actor.init {
                    self.push();
                    self.params(&init.params, None)?;
                    self.block(&mut init.body)?;
                    self.pop();
                }
                for field in &mut actor.fields {
                    if let Some(value) = &mut field.default {
                        self.expr(value)?;
                    }
                }
                for receiver in &mut actor.receive_fns {
                    self.push();
                    self.generics(
                        receiver.type_params.as_deref().unwrap_or_default(),
                        receiver.where_clause.as_ref(),
                    )?;
                    self.params(&receiver.params, receiver.return_type.as_ref())?;
                    self.block(&mut receiver.body)?;
                    self.pop();
                }
                for function in &mut actor.methods {
                    self.function(function)?;
                }
            }
            Item::Supervisor(supervisor) => {
                if let Some(value) = &mut supervisor.backoff {
                    self.expr(value)?;
                }
                self.types.define(Ident::new("Self"), Ty::Error, false);
                self.bind(Ident::new("self"));
                for child in &supervisor.children {
                    self.bind(child.name);
                }
                for child in &mut supervisor.children {
                    self.path(&child.actor_type, Namespace::Type)?;
                    for (_, value) in &mut child.args {
                        self.expr(value)?;
                    }
                    if let Some(value) = &mut child.stop {
                        self.expr(value)?;
                    }
                    if let Some(value) = &mut child.backoff {
                        self.expr(value)?;
                    }
                    if let Some(value) = &mut child.count {
                        self.expr(value)?;
                    }
                }
                for function in &mut supervisor.methods {
                    self.function(function)?;
                }
            }
            Item::Machine(machine) => {
                for state in &machine.states {
                    self.bind(state.name);
                    self.types.define(state.name, Ty::Error, false);
                }
                self.types.define(Ident::new("Self"), Ty::Error, false);
                self.bind(Ident::new("self"));
                self.bind(Ident::new("event"));
                self.generics(&machine.type_params, machine.where_clause.as_ref())?;
                for param in &machine.const_params {
                    self.bind(param.name);
                }
                for state in &mut machine.states {
                    self.push();
                    for (name, ty) in &state.fields {
                        self.ty(ty)?;
                        self.bind(*name);
                    }
                    if let Some(body) = &mut state.entry {
                        self.block(body)?;
                    }
                    if let Some(body) = &mut state.exit {
                        self.block(body)?;
                    }
                    self.pop();
                }
                for event in &machine.events {
                    for (_, ty) in &event.fields {
                        self.ty(ty)?;
                    }
                }
                for transition in &mut machine.transitions {
                    self.push();
                    if let Some(state) = machine
                        .states
                        .iter()
                        .find(|state| state.name == transition.source_state)
                    {
                        for (name, _) in &state.fields {
                            self.bind(*name);
                        }
                    }
                    for (name, _) in &transition.event_bindings {
                        self.bind(*name);
                    }
                    if let Some(guard) = &mut transition.guard {
                        self.expr(guard)?;
                    }
                    self.expr(&mut transition.body)?;
                    self.pop();
                }
            }
            Item::ExternBlock(block) => {
                for function in &block.functions {
                    self.push();
                    self.params(&function.params, function.return_type.as_ref())?;
                    self.pop();
                }
            }
            Item::Import(_) | Item::Macro(_) | Item::MacroCall(_) => {}
        }
        self.pop();
        Ok(())
    }

    fn block(&mut self, block: &mut Block) -> Result<(), Box<TypeError>> {
        self.push();
        for statement in &mut block.stmts {
            self.stmt(statement)?;
        }
        if let Some(expression) = &mut block.trailing_expr {
            self.expr(expression)?;
        }
        self.pop();
        Ok(())
    }

    #[expect(
        clippy::too_many_lines,
        reason = "one exhaustive AST dispatch keeps template scope transitions visible"
    )]
    fn stmt(&mut self, (statement, _span): &mut Spanned<Stmt>) -> Result<(), Box<TypeError>> {
        match statement {
            Stmt::Let {
                pattern,
                ty,
                value,
                else_block,
            } => {
                if let Some(ty) = ty {
                    self.ty(ty)?;
                }
                if let Some(value) = value {
                    self.expr(value)?;
                }
                if let Some(block) = else_block {
                    self.block(block)?;
                }
                self.pattern(pattern)?;
            }
            Stmt::Var {
                name, ty, value, ..
            } => {
                if let Some(ty) = ty {
                    self.ty(ty)?;
                }
                if let Some(value) = value {
                    self.expr(value)?;
                }
                self.bind(*name);
            }
            Stmt::Assign { target, value, .. } => {
                self.expr(target)?;
                self.expr(value)?;
            }
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                self.expr(condition)?;
                self.block(then_block)?;
                if let Some(other) = else_block {
                    if let Some(statement) = &mut other.if_stmt {
                        self.stmt(statement)?;
                    }
                    if let Some(block) = &mut other.block {
                        self.block(block)?;
                    }
                }
            }
            Stmt::IfLet {
                conditions,
                body,
                else_body,
            } => {
                self.push();
                self.conditions(conditions)?;
                self.block(body)?;
                self.pop();
                if let Some(body) = else_body {
                    self.expr(body)?;
                }
            }
            Stmt::Match { scrutinee, arms } => {
                self.expr(scrutinee)?;
                self.arms(arms)?;
            }
            Stmt::Loop { body, .. } => self.block(body)?,
            Stmt::For {
                pattern,
                iterable,
                body,
                ..
            } => {
                self.expr(iterable)?;
                self.push();
                self.pattern(pattern)?;
                self.block(body)?;
                self.pop();
            }
            Stmt::While {
                label: _,
                condition,
                body,
                ..
            } => {
                self.expr(condition)?;
                self.block(body)?;
            }
            Stmt::WhileLet {
                label: _,
                conditions,
                body,
            } => {
                self.push();
                self.conditions(conditions)?;
                self.block(body)?;
                self.pop();
            }
            Stmt::Break { value, .. } | Stmt::Return(value) => {
                if let Some(value) = value {
                    self.expr(value)?;
                }
            }
            Stmt::Continue { .. } => {}
            Stmt::Defer { body, .. } => self.expr(body)?,
            Stmt::Expression(expression) => self.expr(expression)?,
        }
        Ok(())
    }

    fn arms(&mut self, arms: &mut [MatchArm]) -> Result<(), Box<TypeError>> {
        for arm in arms {
            self.push();
            self.pattern(&arm.pattern)?;
            if let Some(guard) = &mut arm.guard {
                self.expr(guard)?;
            }
            self.expr(&mut arm.body)?;
            self.pop();
        }
        Ok(())
    }

    #[expect(
        clippy::too_many_lines,
        reason = "one exhaustive AST dispatch keeps template scope transitions visible"
    )]
    fn expr(&mut self, (expression, span): &mut Spanned<Expr>) -> Result<(), Box<TypeError>> {
        if self.captured(span) {
            return Ok(());
        }
        match expression {
            Expr::PostfixTry(value) | Expr::ReturnError(value) => {
                self.expr(value)?;
            }
            Expr::Return(value) | Expr::Yield(value) => {
                if let Some(value) = value {
                    self.expr(value)?;
                }
            }
            Expr::Lambda {
                params,
                type_params,
                return_type,
                body,
                ..
            } => {
                self.push();
                self.generics(type_params.as_deref().unwrap_or_default(), None)?;
                for param in params {
                    if let Some(ty) = &param.ty {
                        self.ty(ty)?;
                    }
                    self.bind(param.name);
                }
                if let Some(ty) = return_type {
                    self.ty(ty)?;
                }
                self.expr(body)?;
                self.pop();
            }
            Expr::SpawnLambdaActor {
                params,
                return_type,
                body,
                ..
            } => {
                self.push();
                for param in params {
                    if let Some(ty) = &param.ty {
                        self.ty(ty)?;
                    }
                    self.bind(param.name);
                }
                if let Some(ty) = return_type {
                    self.ty(ty)?;
                }
                self.expr(body)?;
                self.pop();
            }
            Expr::GenBlock { body } | Expr::ForkBlock { body } => self.block(body)?,
            Expr::Block(block) | Expr::Scope { body: block } => self.block(block)?,
            Expr::UnsafeBlock(block) => self.block(block)?,
            Expr::Binary { left, right, .. }
            | Expr::Coalesce { left, right }
            | Expr::Is {
                lhs: left,
                rhs: right,
            }
            | Expr::OptionalChain {
                receiver: left,
                body: right,
            }
            | Expr::Index {
                object: left,
                index: right,
            }
            | Expr::ArrayRepeat {
                value: left,
                count: right,
            } => {
                self.expr(left)?;
                self.expr(right)?;
            }
            Expr::Unary { operand: value, .. }
            | Expr::Clone(value)
            | Expr::Await(value)
            | Expr::ForkChild { expr: value } => self.expr(value)?,
            Expr::FieldAccess { .. } => {
                if let Some(path) = value_path(expression, span) {
                    self.path(&path, Namespace::Value)?;
                } else if let Expr::FieldAccess { object, .. } = expression {
                    self.expr(object)?;
                }
            }
            Expr::GenericApplySuffix { target, type_args } => {
                self.expr(target)?;
                for ty in type_args {
                    self.ty(ty)?;
                }
            }
            Expr::Cast { expr, ty } => {
                self.expr(expr)?;
                self.ty(ty)?;
            }
            Expr::MacroInvocation { name, .. } if name.name.as_str() == "stringify" => {}
            Expr::Tuple(values)
            | Expr::Race(values)
            | Expr::MacroInvocation { args: values, .. } => {
                for value in values {
                    self.expr(value)?;
                }
            }
            Expr::Array(values) => {
                for value in values {
                    self.expr(value.expr_mut())?;
                }
            }
            Expr::MapLiteral { entries } => {
                for (key, value) in entries {
                    self.expr(key)?;
                    self.expr(value)?;
                }
            }
            Expr::If {
                condition,
                then_block,
                else_block,
            } => {
                self.expr(condition)?;
                self.expr(then_block)?;
                if let Some(block) = else_block {
                    self.expr(block)?;
                }
            }
            Expr::IfLet {
                conditions,
                body,
                else_body,
            } => {
                self.push();
                self.conditions(conditions)?;
                self.block(body)?;
                self.pop();
                if let Some(body) = else_body {
                    self.expr(body)?;
                }
            }
            Expr::Match { scrutinee, arms } => {
                self.expr(scrutinee)?;
                self.arms(arms)?;
            }
            Expr::Spawn {
                target,
                type_args,
                args,
                ..
            } => {
                for ty in type_args {
                    self.ty(ty)?;
                }
                self.expr(target)?;
                for (_, value) in args {
                    self.expr(value)?;
                }
            }
            Expr::ScopeDeadline { duration, body } => {
                self.expr(duration)?;
                self.block(body)?;
            }
            Expr::InterpolatedString(parts) => {
                for part in parts {
                    if let StringPart::Expr(value) | StringPart::StructuralExpr(value) = part {
                        self.expr(value)?;
                    }
                }
            }
            Expr::Call {
                function,
                type_args,
                args,
                ..
            } => {
                for ty in type_args.iter().flatten() {
                    self.ty(ty)?;
                }
                self.expr(function)?;
                for arg in args {
                    self.expr(arg.expr_mut())?;
                }
            }
            Expr::MethodCall { receiver, args, .. } => {
                self.expr(receiver)?;
                for arg in args {
                    self.expr(arg.expr_mut())?;
                }
            }
            Expr::StructInit {
                path,
                type_args,
                fields,
                base,
                ..
            } => {
                self.path(path, Namespace::Type)?;
                for ty in type_args.iter().flatten() {
                    self.ty(ty)?;
                }
                for (_, value) in fields {
                    self.expr(value)?;
                }
                if let Some(base) = base {
                    self.expr(base)?;
                }
            }
            Expr::RecordInitSuffix {
                target,
                fields,
                base,
            } => {
                self.expr(target)?;
                for (_, value) in fields {
                    self.expr(value)?;
                }
                if let Some(base) = base {
                    self.expr(base)?;
                }
            }
            Expr::Select { arms, timeout } => {
                for arm in arms {
                    self.expr(&mut arm.source)?;
                    self.push();
                    self.pattern(&arm.binding)?;
                    self.expr(&mut arm.body)?;
                    self.pop();
                }
                if let Some(timeout) = timeout {
                    self.expr(&mut timeout.duration)?;
                    self.expr(&mut timeout.body)?;
                }
            }
            Expr::Handle {
                operand,
                error,
                body,
            } => {
                self.expr(operand)?;
                self.push();
                if let Some(name) = error.0 {
                    self.bind(name);
                }
                self.expr(body)?;
                self.pop();
            }
            Expr::Range { start, end, .. } => {
                if let Some(start) = start {
                    self.expr(start)?;
                }
                if let Some(end) = end {
                    self.expr(end)?;
                }
            }
            Expr::MachineEmit { fields, .. } => {
                for (_, value) in fields {
                    self.expr(value)?;
                }
            }
            Expr::ContextVariant(variant) => {
                if let Some(record) = &mut variant.record {
                    for (_, value) in &mut record.fields {
                        self.expr(value)?;
                    }
                    if let Some(base) = &mut record.base {
                        self.expr(base)?;
                    }
                }
            }
            Expr::Ident(name) => self.path(
                &Path {
                    segments: vec![(*name, span.clone())],
                },
                Namespace::Value,
            )?,
            Expr::QualifiedAssoc(assoc) => {
                self.ty(&assoc.base)?;
                self.path(&assoc.trait_path, Namespace::Type)?;
            }
            Expr::Literal(_)
            | Expr::MacroCall(_)
            | Expr::ChainValue
            | Expr::RegexLiteral(_)
            | Expr::ByteStringLiteral(_)
            | Expr::ByteArrayLiteral(_) => {}
        }
        Ok(())
    }
}

fn value_path(expression: &Expr, span: &Span) -> Option<Path> {
    match expression {
        Expr::Ident(name) => Some(Path {
            segments: vec![(*name, span.clone())],
        }),
        Expr::FieldAccess { object, field } => {
            let mut path = value_path(&object.0, &object.1)?;
            path.segments.push(field.clone());
            Some(path)
        }
        _ => None,
    }
}
