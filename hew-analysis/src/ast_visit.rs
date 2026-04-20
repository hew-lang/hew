use hew_parser::ast::{
    Block, Expr, Item, LambdaParam, MatchArm, Pattern, PatternField, SelectArm, Span, Stmt,
    StringPart, TraitItem, TypeBodyItem,
};
use hew_parser::ParseResult;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BodyKind {
    Function,
    ActorInit,
    ActorTerminate,
    ActorReceive,
    ActorMethod,
    TypeMethod,
    ImplMethod,
    TraitMethod,
    Const,
    Supervisor,
    Machine,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct BodyInfo<'ast> {
    pub kind: BodyKind,
    pub name: Option<&'ast str>,
    pub span: Option<&'ast Span>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BindingKind {
    Param,
    Local,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct BindingInfo<'ast> {
    pub kind: BindingKind,
    pub name: &'ast str,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct VisitContext<'ast> {
    #[allow(
        dead_code,
        reason = "body metadata is reserved for downstream visitors"
    )]
    pub body: Option<BodyInfo<'ast>>,
}

pub(crate) trait AstVisitor<'ast> {
    fn enter_body(&mut self, _body: BodyInfo<'ast>, _ctx: VisitContext<'ast>) {}
    fn leave_body(&mut self, _body: BodyInfo<'ast>, _ctx: VisitContext<'ast>) {}
    fn visit_item(&mut self, _item: &'ast Item, _span: &'ast Span, _ctx: VisitContext<'ast>) {}
    fn visit_stmt(&mut self, _stmt: &'ast Stmt, _span: &'ast Span, _ctx: VisitContext<'ast>) {}
    fn visit_expr(&mut self, _expr: &'ast Expr, _span: &'ast Span, _ctx: VisitContext<'ast>) {}
    fn visit_binding(&mut self, _binding: BindingInfo<'ast>, _ctx: VisitContext<'ast>) {}
    fn visit_identifier(
        &mut self,
        _name: &'ast str,
        _span: &'ast Span,
        _binding: Option<BindingInfo<'ast>>,
        _ctx: VisitContext<'ast>,
    ) {
    }
}

pub(crate) fn walk_parse_result<'ast, V: AstVisitor<'ast>>(
    source: Option<&str>,
    parse_result: &'ast ParseResult,
    visitor: &mut V,
) {
    let mut walker = AstWalker::new(source, visitor);
    for (item, span) in &parse_result.program.items {
        walker.walk_item(item, span, None);
    }
}

pub(crate) fn walk_named_body<'ast, V: AstVisitor<'ast>>(
    source: Option<&str>,
    item: &'ast Item,
    body_name: &str,
    visitor: &mut V,
) -> bool {
    let mut walker = AstWalker::new(source, visitor);
    walker.walk_named_body(item, body_name)
}

#[allow(
    dead_code,
    reason = "scope snapshot helper is staged for later migrations"
)]
pub(crate) fn visible_bindings_at<'ast>(
    source: &str,
    parse_result: &'ast ParseResult,
    offset: usize,
) -> Vec<BindingInfo<'ast>> {
    let mut collector = VisibleBindingsCollector {
        offset,
        matches: Vec::new(),
        active_body_depth: 0,
    };
    walk_parse_result(Some(source), parse_result, &mut collector);
    collector.matches
}

#[allow(
    dead_code,
    reason = "scope snapshot helper is staged for later migrations"
)]
struct VisibleBindingsCollector<'ast> {
    offset: usize,
    matches: Vec<BindingInfo<'ast>>,
    active_body_depth: usize,
}

impl<'ast> AstVisitor<'ast> for VisibleBindingsCollector<'ast> {
    fn enter_body(&mut self, body: BodyInfo<'ast>, _ctx: VisitContext<'ast>) {
        if body.span.is_some_and(|span| span.start <= self.offset) {
            if self.active_body_depth == 0 {
                self.matches.clear();
            }
            self.active_body_depth += 1;
        }
    }

    fn leave_body(&mut self, body: BodyInfo<'ast>, _ctx: VisitContext<'ast>) {
        if body.span.is_some_and(|span| span.start <= self.offset) && self.active_body_depth > 0 {
            self.active_body_depth -= 1;
        }
    }

    fn visit_binding(&mut self, binding: BindingInfo<'ast>, _ctx: VisitContext<'ast>) {
        if self.active_body_depth == 0 {
            return;
        }
        if binding.span.start <= self.offset {
            if let Some(existing) = self
                .matches
                .iter_mut()
                .find(|existing| existing.name == binding.name)
            {
                *existing = binding;
            } else {
                self.matches.push(binding);
            }
        }
    }
}

struct ScopeFrame<'ast> {
    bindings: Vec<BindingInfo<'ast>>,
}

struct AstWalker<'src, 'ast, V> {
    source: Option<&'src str>,
    visitor: &'src mut V,
    scopes: Vec<ScopeFrame<'ast>>,
}

impl<'src, 'ast, V: AstVisitor<'ast>> AstWalker<'src, 'ast, V> {
    fn new(source: Option<&'src str>, visitor: &'src mut V) -> Self {
        Self {
            source,
            visitor,
            scopes: Vec::new(),
        }
    }

    fn context(body: Option<BodyInfo<'ast>>) -> VisitContext<'ast> {
        VisitContext { body }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "item-body dispatch needs exhaustive coverage for walker parity"
    )]
    fn walk_item(&mut self, item: &'ast Item, span: &'ast Span, body: Option<BodyInfo<'ast>>) {
        self.visitor.visit_item(item, span, Self::context(body));
        match item {
            Item::Function(function) => {
                let body_info = BodyInfo {
                    kind: BodyKind::Function,
                    name: Some(&function.name),
                    span: Some(&function.fn_span),
                };
                self.walk_block_body(
                    &function.body,
                    body_info,
                    params_to_bindings(self.source, body_info.span, &function.params),
                );
            }
            Item::Actor(actor) => {
                if let Some(init) = &actor.init {
                    let body_info = BodyInfo {
                        kind: BodyKind::ActorInit,
                        name: Some(&actor.name),
                        span: Some(span),
                    };
                    self.walk_block_body(
                        &init.body,
                        body_info,
                        params_to_bindings(self.source, body_info.span, &init.params),
                    );
                }
                if let Some(term) = &actor.terminate {
                    let body_info = BodyInfo {
                        kind: BodyKind::ActorTerminate,
                        name: Some(&actor.name),
                        span: Some(span),
                    };
                    self.walk_block_body(&term.body, body_info, Vec::new());
                }
                for recv in &actor.receive_fns {
                    let body_info = BodyInfo {
                        kind: BodyKind::ActorReceive,
                        name: Some(&recv.name),
                        span: Some(&recv.span),
                    };
                    self.walk_block_body(
                        &recv.body,
                        body_info,
                        params_to_bindings(self.source, body_info.span, &recv.params),
                    );
                }
                for method in &actor.methods {
                    let body_info = BodyInfo {
                        kind: BodyKind::ActorMethod,
                        name: Some(&method.name),
                        span: Some(&method.fn_span),
                    };
                    self.walk_block_body(
                        &method.body,
                        body_info,
                        params_to_bindings(self.source, body_info.span, &method.params),
                    );
                }
            }
            Item::TypeDecl(type_decl) => {
                for body_item in &type_decl.body {
                    if let TypeBodyItem::Method(method) = body_item {
                        let body_info = BodyInfo {
                            kind: BodyKind::TypeMethod,
                            name: Some(&method.name),
                            span: Some(&method.fn_span),
                        };
                        self.walk_block_body(
                            &method.body,
                            body_info,
                            params_to_bindings(self.source, body_info.span, &method.params),
                        );
                    }
                }
            }
            Item::Impl(impl_decl) => {
                for method in &impl_decl.methods {
                    let body_info = BodyInfo {
                        kind: BodyKind::ImplMethod,
                        name: Some(&method.name),
                        span: Some(&method.fn_span),
                    };
                    self.walk_block_body(
                        &method.body,
                        body_info,
                        params_to_bindings(self.source, body_info.span, &method.params),
                    );
                }
            }
            Item::Trait(trait_decl) => {
                for trait_item in &trait_decl.items {
                    if let TraitItem::Method(method) = trait_item {
                        if let Some(body_block) = &method.body {
                            let body_info = BodyInfo {
                                kind: BodyKind::TraitMethod,
                                name: Some(&method.name),
                                span: Some(&method.span),
                            };
                            self.walk_block_body(
                                body_block,
                                body_info,
                                params_to_bindings(self.source, body_info.span, &method.params),
                            );
                        }
                    }
                }
            }
            Item::Const(const_decl) => {
                let body_info = BodyInfo {
                    kind: BodyKind::Const,
                    name: Some(&const_decl.name),
                    span: Some(span),
                };
                self.walk_expr_body(
                    &const_decl.value.0,
                    &const_decl.value.1,
                    body_info,
                    Vec::new(),
                );
            }
            Item::Supervisor(supervisor) => {
                let body_info = BodyInfo {
                    kind: BodyKind::Supervisor,
                    name: Some(&supervisor.name),
                    span: Some(span),
                };
                self.visitor
                    .enter_body(body_info, Self::context(Some(body_info)));
                self.push_scope(Vec::new());
                for child in &supervisor.children {
                    for arg in &child.args {
                        self.walk_expr(&arg.0, &arg.1, Some(body_info));
                    }
                }
                self.pop_scope();
                self.visitor
                    .leave_body(body_info, Self::context(Some(body_info)));
            }
            Item::Machine(machine) => {
                let body_info = BodyInfo {
                    kind: BodyKind::Machine,
                    name: Some(&machine.name),
                    span: Some(span),
                };
                self.visitor
                    .enter_body(body_info, Self::context(Some(body_info)));
                self.push_scope(Vec::new());
                for transition in &machine.transitions {
                    if let Some(guard) = &transition.guard {
                        self.walk_expr(&guard.0, &guard.1, Some(body_info));
                    }
                    self.walk_expr(&transition.body.0, &transition.body.1, Some(body_info));
                }
                self.pop_scope();
                self.visitor
                    .leave_body(body_info, Self::context(Some(body_info)));
            }
            Item::Import(_) | Item::ExternBlock(_) | Item::Wire(_) | Item::TypeAlias(_) => {}
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "named-body dispatch mirrors the per-item walker surface"
    )]
    fn walk_named_body(&mut self, item: &'ast Item, body_name: &str) -> bool {
        match item {
            Item::Function(function) if function.name == body_name => {
                let body_info = BodyInfo {
                    kind: BodyKind::Function,
                    name: Some(&function.name),
                    span: Some(&function.fn_span),
                };
                self.walk_block_body(
                    &function.body,
                    body_info,
                    params_to_bindings(self.source, body_info.span, &function.params),
                );
                true
            }
            Item::Const(const_decl) if const_decl.name == body_name => {
                let body_info = BodyInfo {
                    kind: BodyKind::Const,
                    name: Some(&const_decl.name),
                    span: None,
                };
                self.walk_expr_body(&const_decl.value.0, &const_decl.value.1, body_info, Vec::new());
                true
            }
            Item::Supervisor(supervisor) if supervisor.name == body_name => {
                let body_info = BodyInfo {
                    kind: BodyKind::Supervisor,
                    name: Some(&supervisor.name),
                    span: None,
                };
                self.visitor.enter_body(body_info, Self::context(Some(body_info)));
                self.push_scope(Vec::new());
                for child in &supervisor.children {
                    for arg in &child.args {
                        self.walk_expr(&arg.0, &arg.1, Some(body_info));
                    }
                }
                self.pop_scope();
                self.visitor.leave_body(body_info, Self::context(Some(body_info)));
                true
            }
            Item::Machine(machine) if machine.name == body_name => {
                let body_info = BodyInfo {
                    kind: BodyKind::Machine,
                    name: Some(&machine.name),
                    span: None,
                };
                self.visitor.enter_body(body_info, Self::context(Some(body_info)));
                self.push_scope(Vec::new());
                for transition in &machine.transitions {
                    if let Some(guard) = &transition.guard {
                        self.walk_expr(&guard.0, &guard.1, Some(body_info));
                    }
                    self.walk_expr(&transition.body.0, &transition.body.1, Some(body_info));
                }
                self.pop_scope();
                self.visitor.leave_body(body_info, Self::context(Some(body_info)));
                true
            }
            Item::Actor(actor) => {
                if let Some(recv) = actor.receive_fns.iter().find(|recv| recv.name == body_name) {
                    let body_info = BodyInfo {
                        kind: BodyKind::ActorReceive,
                        name: Some(&recv.name),
                        span: Some(&recv.span),
                    };
                    self.walk_block_body(
                        &recv.body,
                        body_info,
                        params_to_bindings(self.source, body_info.span, &recv.params),
                    );
                    return true;
                }
                if let Some(method) = actor.methods.iter().find(|method| method.name == body_name) {
                    let body_info = BodyInfo {
                        kind: BodyKind::ActorMethod,
                        name: Some(&method.name),
                        span: Some(&method.fn_span),
                    };
                    self.walk_block_body(
                        &method.body,
                        body_info,
                        params_to_bindings(self.source, body_info.span, &method.params),
                    );
                    return true;
                }
                false
            }
            Item::Impl(impl_decl) => {
                if let Some(method) = impl_decl.methods.iter().find(|method| method.name == body_name)
                {
                    let body_info = BodyInfo {
                        kind: BodyKind::ImplMethod,
                        name: Some(&method.name),
                        span: Some(&method.fn_span),
                    };
                    self.walk_block_body(
                        &method.body,
                        body_info,
                        params_to_bindings(self.source, body_info.span, &method.params),
                    );
                    true
                } else {
                    false
                }
            }
            Item::TypeDecl(type_decl) => {
                if let Some(TypeBodyItem::Method(method)) = type_decl
                    .body
                    .iter()
                    .find(|body_item| matches!(body_item, TypeBodyItem::Method(method) if method.name == body_name))
                {
                    let body_info = BodyInfo {
                        kind: BodyKind::TypeMethod,
                        name: Some(&method.name),
                        span: Some(&method.fn_span),
                    };
                    self.walk_block_body(
                        &method.body,
                        body_info,
                        params_to_bindings(self.source, body_info.span, &method.params),
                    );
                    true
                } else {
                    false
                }
            }
            Item::Trait(trait_decl) => {
                if let Some(TraitItem::Method(method)) = trait_decl
                    .items
                    .iter()
                    .find(|trait_item| matches!(trait_item, TraitItem::Method(method) if method.name == body_name))
                {
                    let body_info = BodyInfo {
                        kind: BodyKind::TraitMethod,
                        name: Some(&method.name),
                        span: Some(&method.span),
                    };
                    self.visitor.enter_body(body_info, Self::context(Some(body_info)));
                    self.push_scope(params_to_bindings(self.source, body_info.span, &method.params));
                    if let Some(body_block) = &method.body {
                        self.walk_block(body_block, Some(body_info));
                    }
                    self.pop_scope();
                    self.visitor.leave_body(body_info, Self::context(Some(body_info)));
                    true
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    fn walk_block_body(
        &mut self,
        block: &'ast Block,
        body: BodyInfo<'ast>,
        initial_bindings: Vec<BindingInfo<'ast>>,
    ) {
        self.visitor.enter_body(body, Self::context(Some(body)));
        self.push_scope(initial_bindings);
        self.walk_block(block, Some(body));
        self.pop_scope();
        self.visitor.leave_body(body, Self::context(Some(body)));
    }

    fn walk_expr_body(
        &mut self,
        expr: &'ast Expr,
        span: &'ast Span,
        body: BodyInfo<'ast>,
        initial_bindings: Vec<BindingInfo<'ast>>,
    ) {
        self.visitor.enter_body(body, Self::context(Some(body)));
        self.push_scope(initial_bindings);
        self.walk_expr(expr, span, Some(body));
        self.pop_scope();
        self.visitor.leave_body(body, Self::context(Some(body)));
    }

    fn walk_block(&mut self, block: &'ast Block, body: Option<BodyInfo<'ast>>) {
        self.push_scope(Vec::new());
        for (stmt, span) in &block.stmts {
            self.walk_stmt(stmt, span, body);
        }
        if let Some(trailing_expr) = &block.trailing_expr {
            self.walk_expr(&trailing_expr.0, &trailing_expr.1, body);
        }
        self.pop_scope();
    }

    #[expect(
        clippy::too_many_lines,
        reason = "statement traversal must preserve scope ordering across variants"
    )]
    fn walk_stmt(&mut self, stmt: &'ast Stmt, span: &'ast Span, body: Option<BodyInfo<'ast>>) {
        self.visitor.visit_stmt(stmt, span, Self::context(body));
        match stmt {
            Stmt::Let { pattern, value, .. } => {
                if let Some(value) = value {
                    self.walk_expr(&value.0, &value.1, body);
                }
                self.add_bindings(pattern_bindings(self.source, pattern));
            }
            Stmt::Var { name, value, .. } => {
                if let Some(value) = value {
                    self.walk_expr(&value.0, &value.1, body);
                }
                self.add_binding(binding_from_name(
                    self.source,
                    name,
                    span,
                    BindingKind::Local,
                ));
            }
            Stmt::Assign { target, value, .. } => {
                self.walk_expr(&target.0, &target.1, body);
                self.walk_expr(&value.0, &value.1, body);
            }
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                self.walk_expr(&condition.0, &condition.1, body);
                self.walk_block(then_block, body);
                if let Some(else_block) = else_block {
                    if let Some(if_stmt) = &else_block.if_stmt {
                        self.walk_stmt(&if_stmt.0, &if_stmt.1, body);
                    }
                    if let Some(block) = &else_block.block {
                        self.walk_block(block, body);
                    }
                }
            }
            Stmt::IfLet {
                pattern,
                expr,
                body: inner_body,
                else_body,
            } => {
                self.walk_expr(&expr.0, &expr.1, body);
                self.push_scope(pattern_bindings(self.source, pattern));
                self.walk_block(inner_body, body);
                self.pop_scope();
                if let Some(else_body) = else_body {
                    self.walk_block(else_body, body);
                }
            }
            Stmt::Match { scrutinee, arms } => {
                self.walk_expr(&scrutinee.0, &scrutinee.1, body);
                for arm in arms {
                    self.walk_match_arm(arm, body);
                }
            }
            Stmt::Loop {
                body: inner_body, ..
            }
            | Stmt::While {
                body: inner_body, ..
            } => {
                if let Stmt::While { condition, .. } = stmt {
                    self.walk_expr(&condition.0, &condition.1, body);
                }
                self.walk_block(inner_body, body);
            }
            Stmt::WhileLet {
                pattern,
                expr,
                body: inner_body,
                ..
            } => {
                self.walk_expr(&expr.0, &expr.1, body);
                self.push_scope(pattern_bindings(self.source, pattern));
                self.walk_block(inner_body, body);
                self.pop_scope();
            }
            Stmt::For {
                pattern,
                iterable,
                body: inner_body,
                ..
            } => {
                self.walk_expr(&iterable.0, &iterable.1, body);
                self.push_scope(pattern_bindings(self.source, pattern));
                self.walk_block(inner_body, body);
                self.pop_scope();
            }
            Stmt::Break {
                value: Some(value), ..
            }
            | Stmt::Return(Some(value)) => {
                self.walk_expr(&value.0, &value.1, body);
            }
            Stmt::Defer(expr) => {
                self.walk_expr(&expr.0, &expr.1, body);
            }
            Stmt::Expression(expr) => {
                self.walk_expr(&expr.0, &expr.1, body);
            }
            Stmt::Return(None) | Stmt::Break { value: None, .. } | Stmt::Continue { .. } => {}
        }
    }

    fn walk_match_arm(&mut self, arm: &'ast MatchArm, body: Option<BodyInfo<'ast>>) {
        self.push_scope(pattern_bindings(self.source, &arm.pattern));
        if let Some(guard) = &arm.guard {
            self.walk_expr(&guard.0, &guard.1, body);
        }
        self.walk_expr(&arm.body.0, &arm.body.1, body);
        self.pop_scope();
    }

    fn walk_select_arm(&mut self, arm: &'ast SelectArm, body: Option<BodyInfo<'ast>>) {
        self.walk_expr(&arm.source.0, &arm.source.1, body);
        self.push_scope(pattern_bindings(self.source, &arm.binding));
        self.walk_expr(&arm.body.0, &arm.body.1, body);
        self.pop_scope();
    }

    #[expect(
        clippy::too_many_lines,
        reason = "expression traversal needs exhaustive variant coverage"
    )]
    fn walk_expr(&mut self, expr: &'ast Expr, span: &'ast Span, body: Option<BodyInfo<'ast>>) {
        self.visitor.visit_expr(expr, span, Self::context(body));
        match expr {
            Expr::Identifier(name) => {
                let binding = self.resolve_binding(name);
                self.visitor
                    .visit_identifier(name, span, binding, Self::context(body));
            }
            Expr::Binary { left, right, .. } => {
                self.walk_expr(&left.0, &left.1, body);
                self.walk_expr(&right.0, &right.1, body);
            }
            Expr::Unary { operand, .. }
            | Expr::Await(operand)
            | Expr::PostfixTry(operand)
            | Expr::Yield(Some(operand)) => {
                self.walk_expr(&operand.0, &operand.1, body);
            }
            Expr::Tuple(elements) | Expr::Array(elements) | Expr::Join(elements) => {
                for element in elements {
                    self.walk_expr(&element.0, &element.1, body);
                }
            }
            Expr::ArrayRepeat { value, count } => {
                self.walk_expr(&value.0, &value.1, body);
                self.walk_expr(&count.0, &count.1, body);
            }
            Expr::MapLiteral { entries } => {
                for (key, value) in entries {
                    self.walk_expr(&key.0, &key.1, body);
                    self.walk_expr(&value.0, &value.1, body);
                }
            }
            Expr::Block(block)
            | Expr::Unsafe(block)
            | Expr::ScopeLaunch(block)
            | Expr::ScopeSpawn(block) => {
                self.walk_block(block, body);
            }
            Expr::If {
                condition,
                then_block,
                else_block,
            } => {
                self.walk_expr(&condition.0, &condition.1, body);
                self.walk_expr(&then_block.0, &then_block.1, body);
                if let Some(else_block) = else_block {
                    self.walk_expr(&else_block.0, &else_block.1, body);
                }
            }
            Expr::IfLet {
                pattern,
                expr,
                body: inner_body,
                else_body,
            } => {
                self.walk_expr(&expr.0, &expr.1, body);
                self.push_scope(pattern_bindings(self.source, pattern));
                self.walk_block(inner_body, body);
                self.pop_scope();
                if let Some(else_body) = else_body {
                    self.walk_block(else_body, body);
                }
            }
            Expr::Match { scrutinee, arms } => {
                self.walk_expr(&scrutinee.0, &scrutinee.1, body);
                for arm in arms {
                    self.walk_match_arm(arm, body);
                }
            }
            Expr::Lambda {
                params,
                body: inner_body,
                ..
            }
            | Expr::SpawnLambdaActor {
                params,
                body: inner_body,
                ..
            } => {
                self.push_scope(lambda_params_to_bindings(self.source, span, params));
                self.walk_expr(&inner_body.0, &inner_body.1, body);
                self.pop_scope();
            }
            Expr::Spawn { target, args } => {
                self.walk_expr(&target.0, &target.1, body);
                for (_, value) in args {
                    self.walk_expr(&value.0, &value.1, body);
                }
            }
            Expr::Scope {
                binding,
                body: inner_body,
            } => {
                let scope_binding = binding.as_deref().map(|binding| {
                    binding_from_name(self.source, binding, span, BindingKind::Local)
                });
                self.push_scope(scope_binding.into_iter().collect());
                self.walk_block(inner_body, body);
                self.pop_scope();
            }
            Expr::InterpolatedString(parts) => {
                for part in parts {
                    if let StringPart::Expr(expr) = part {
                        self.walk_expr(&expr.0, &expr.1, body);
                    }
                }
            }
            Expr::Call { function, args, .. } => {
                self.walk_expr(&function.0, &function.1, body);
                for arg in args {
                    let expr = arg.expr();
                    self.walk_expr(&expr.0, &expr.1, body);
                }
            }
            Expr::MethodCall { receiver, args, .. } => {
                self.walk_expr(&receiver.0, &receiver.1, body);
                for arg in args {
                    let expr = arg.expr();
                    self.walk_expr(&expr.0, &expr.1, body);
                }
            }
            Expr::StructInit { fields, .. } => {
                for (_, value) in fields {
                    self.walk_expr(&value.0, &value.1, body);
                }
            }
            Expr::Send { target, message } => {
                self.walk_expr(&target.0, &target.1, body);
                self.walk_expr(&message.0, &message.1, body);
            }
            Expr::Select { arms, timeout } => {
                for arm in arms {
                    self.walk_select_arm(arm, body);
                }
                if let Some(timeout) = timeout {
                    self.walk_expr(&timeout.duration.0, &timeout.duration.1, body);
                    self.walk_expr(&timeout.body.0, &timeout.body.1, body);
                }
            }
            Expr::Timeout { expr, duration } => {
                self.walk_expr(&expr.0, &expr.1, body);
                self.walk_expr(&duration.0, &duration.1, body);
            }
            Expr::FieldAccess { object, .. } => {
                self.walk_expr(&object.0, &object.1, body);
            }
            Expr::Index { object, index } => {
                self.walk_expr(&object.0, &object.1, body);
                self.walk_expr(&index.0, &index.1, body);
            }
            Expr::Cast { expr, .. } => {
                self.walk_expr(&expr.0, &expr.1, body);
            }
            Expr::Range { start, end, .. } => {
                if let Some(start) = start {
                    self.walk_expr(&start.0, &start.1, body);
                }
                if let Some(end) = end {
                    self.walk_expr(&end.0, &end.1, body);
                }
            }
            Expr::Literal(_)
            | Expr::This
            | Expr::Cooperate
            | Expr::Yield(None)
            | Expr::RegexLiteral(_)
            | Expr::ByteStringLiteral(_)
            | Expr::ByteArrayLiteral(_)
            | Expr::ScopeCancel => {}
        }
    }

    fn push_scope(&mut self, bindings: Vec<BindingInfo<'ast>>) {
        for binding in &bindings {
            self.visitor
                .visit_binding(binding.clone(), Self::context(None));
        }
        self.scopes.push(ScopeFrame { bindings });
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn add_binding(&mut self, binding: BindingInfo<'ast>) {
        self.visitor
            .visit_binding(binding.clone(), Self::context(None));
        if let Some(scope) = self.scopes.last_mut() {
            scope.bindings.push(binding);
        }
    }

    fn add_bindings(&mut self, bindings: Vec<BindingInfo<'ast>>) {
        for binding in bindings {
            self.add_binding(binding);
        }
    }

    fn resolve_binding(&self, name: &str) -> Option<BindingInfo<'ast>> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| {
                scope
                    .bindings
                    .iter()
                    .rev()
                    .find(|binding| binding.name == name)
            })
            .cloned()
    }
}

fn params_to_bindings<'ast>(
    source: Option<&str>,
    signature_span: Option<&Span>,
    params: &'ast [hew_parser::ast::Param],
) -> Vec<BindingInfo<'ast>> {
    let mut cursor = param_list_search_start(source, signature_span);
    params
        .iter()
        .map(|param| {
            let binding = binding_from_search_from(
                source,
                &param.name,
                cursor,
                &param.ty.1,
                BindingKind::Param,
            );
            cursor = binding.span.end.max(param.ty.1.end);
            binding
        })
        .collect()
}

fn lambda_params_to_bindings<'ast>(
    source: Option<&str>,
    outer_span: &Span,
    params: &'ast [LambdaParam],
) -> Vec<BindingInfo<'ast>> {
    let mut cursor = param_list_search_start(source, Some(outer_span));
    params
        .iter()
        .map(|param| {
            let fallback_span = param.ty.as_ref().map_or(outer_span, |(_, span)| span);
            let binding = binding_from_search_from(
                source,
                &param.name,
                cursor,
                fallback_span,
                BindingKind::Param,
            );
            cursor = param
                .ty
                .as_ref()
                .map_or(binding.span.end, |(_, span)| span.end)
                .max(binding.span.end);
            binding
        })
        .collect()
}

fn binding_from_name<'ast>(
    source: Option<&str>,
    name: &'ast str,
    span: &Span,
    kind: BindingKind,
) -> BindingInfo<'ast> {
    let binding_span = source.map_or_else(
        || span.clone(),
        |source| {
            let span = crate::util::find_name_span(source, span.start, name);
            span.start..span.end
        },
    );
    BindingInfo {
        kind,
        name,
        span: binding_span,
    }
}

fn binding_from_search_from<'ast>(
    source: Option<&str>,
    name: &'ast str,
    search_from: usize,
    fallback_span: &Span,
    kind: BindingKind,
) -> BindingInfo<'ast> {
    let binding_span = source.map_or_else(
        || fallback_span.clone(),
        |source| {
            let span = crate::util::find_name_span(source, search_from, name);
            span.start..span.end
        },
    );
    BindingInfo {
        kind,
        name,
        span: binding_span,
    }
}

fn param_list_search_start(source: Option<&str>, signature_span: Option<&Span>) -> usize {
    match (source, signature_span) {
        (Some(source), Some(span)) => source
            .get(span.start..span.end)
            .and_then(|snippet| snippet.find('(').map(|offset| span.start + offset + 1))
            .unwrap_or(span.start),
        (_, Some(span)) => span.start,
        _ => 0,
    }
}

fn pattern_bindings<'ast>(
    source: Option<&str>,
    pattern: &'ast (Pattern, Span),
) -> Vec<BindingInfo<'ast>> {
    let mut bindings = Vec::new();
    collect_pattern_bindings(source, &pattern.0, &pattern.1, &mut bindings);
    bindings
}

fn collect_pattern_bindings<'ast>(
    source: Option<&str>,
    pattern: &'ast Pattern,
    span: &Span,
    bindings: &mut Vec<BindingInfo<'ast>>,
) {
    match pattern {
        Pattern::Identifier(name) => {
            bindings.push(binding_from_name(source, name, span, BindingKind::Local));
        }
        Pattern::Constructor { patterns, .. } | Pattern::Tuple(patterns) => {
            for (pattern, span) in patterns {
                collect_pattern_bindings(source, pattern, span, bindings);
            }
        }
        Pattern::Struct { fields, .. } => {
            for field in fields {
                collect_pattern_field_bindings(source, span, field, bindings);
            }
        }
        Pattern::Or(left, right) => {
            collect_pattern_bindings(source, &left.0, &left.1, bindings);
            collect_pattern_bindings(source, &right.0, &right.1, bindings);
        }
        Pattern::Wildcard | Pattern::Literal(_) => {}
    }
}

fn collect_pattern_field_bindings<'ast>(
    source: Option<&str>,
    pattern_span: &Span,
    field: &'ast PatternField,
    bindings: &mut Vec<BindingInfo<'ast>>,
) {
    if let Some((pattern, span)) = &field.pattern {
        collect_pattern_bindings(source, pattern, span, bindings);
    } else {
        bindings.push(binding_from_name(
            source,
            &field.name,
            pattern_span,
            BindingKind::Local,
        ));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Default)]
    struct RecordingVisitor {
        bindings: Vec<String>,
        resolved_uses: Vec<(String, Option<String>)>,
        bodies: Vec<(BodyKind, String)>,
    }

    impl<'ast> AstVisitor<'ast> for RecordingVisitor {
        fn enter_body(&mut self, body: BodyInfo<'ast>, _ctx: VisitContext<'ast>) {
            self.bodies
                .push((body.kind, body.name.unwrap_or("<anon>").to_string()));
        }

        fn visit_binding(&mut self, binding: BindingInfo<'ast>, _ctx: VisitContext<'ast>) {
            self.bindings.push(binding.name.to_string());
        }

        fn visit_identifier(
            &mut self,
            name: &'ast str,
            _span: &'ast Span,
            binding: Option<BindingInfo<'ast>>,
            _ctx: VisitContext<'ast>,
        ) {
            self.resolved_uses.push((
                name.to_string(),
                binding.map(|binding| binding.name.to_string()),
            ));
        }
    }

    #[test]
    fn walk_parse_result_tracks_shadowing_order() {
        let source = "fn main(x: int) { let y = x; let x = y; x + y }";
        let parse_result = hew_parser::parse(source);
        let mut visitor = RecordingVisitor::default();
        walk_parse_result(Some(source), &parse_result, &mut visitor);

        assert!(visitor.bindings.iter().any(|binding| binding == "x"));
        assert!(visitor.bindings.iter().any(|binding| binding == "y"));
        assert!(visitor
            .resolved_uses
            .iter()
            .any(|(name, binding)| name == "x" && binding.as_deref() == Some("x")));
    }

    #[test]
    fn visible_bindings_at_returns_latest_binding_per_name() {
        let source = "fn main() { let x = 1; let x = 2; x }";
        let parse_result = hew_parser::parse(source);
        let offset = source.rfind('x').unwrap();
        let bindings = visible_bindings_at(source, &parse_result, offset);

        let x_bindings: Vec<_> = bindings
            .iter()
            .filter(|binding| binding.name == "x")
            .collect();
        assert_eq!(x_bindings.len(), 1);
        assert!(x_bindings[0].span.start > source.find("let x = 1").unwrap());
    }

    #[test]
    fn walk_named_body_limits_traversal_to_requested_body() {
        let source = "actor Worker { receive fn handle(msg: int) { msg } fn helper() { ping() } }";
        let parse_result = hew_parser::parse(source);
        let item = &parse_result.program.items[0].0;
        let mut visitor = RecordingVisitor::default();
        assert!(walk_named_body(Some(source), item, "handle", &mut visitor));
        assert_eq!(
            visitor.bodies,
            vec![(BodyKind::ActorReceive, "handle".to_string())]
        );
        assert!(visitor.resolved_uses.iter().all(|(name, _)| name == "msg"));
    }

    #[test]
    fn params_to_bindings_marks_parameter_name_span() {
        let source = "fn f(abc: int) { abc }";
        let parse_result = hew_parser::parse(source);
        let Item::Function(function) = &parse_result.program.items[0].0 else {
            panic!("expected function");
        };

        let bindings = params_to_bindings(Some(source), Some(&function.fn_span), &function.params);
        assert_eq!(bindings[0].name, "abc");
        assert_eq!(&source[bindings[0].span.clone()], "abc");
    }

    #[test]
    fn lambda_params_to_bindings_use_non_zero_name_spans() {
        let source =
            "fn main() { let first = (x: int) => x; let second = (y: int, z: int) => y + z; }";
        let parse_result = hew_parser::parse(source);
        let Item::Function(function) = &parse_result.program.items[0].0 else {
            panic!("expected function");
        };
        let Stmt::Let {
            value: Some((Expr::Lambda { params, .. }, span)),
            ..
        } = &function.body.stmts[1].0
        else {
            panic!("expected lambda binding");
        };

        let bindings = lambda_params_to_bindings(Some(source), span, params);
        let z_binding = bindings
            .iter()
            .find(|binding| binding.name == "z")
            .expect("z lambda binding");
        assert!(z_binding.span.start > 0);
        assert_eq!(&source[z_binding.span.clone()], "z");
    }

    #[test]
    fn visible_bindings_at_keeps_selected_body_isolated() {
        let source = "fn first(a: int) { let b = a; b }\nfn second(c: int) { c }";
        let parse_result = hew_parser::parse(source);
        let offset = source.find("b }").expect("b usage");
        let bindings = visible_bindings_at(source, &parse_result, offset);

        let binding_names: Vec<_> = bindings.iter().map(|binding| binding.name).collect();
        assert!(binding_names.contains(&"a"));
        assert!(binding_names.contains(&"b"));
        assert!(!binding_names.contains(&"c"));
    }
}
