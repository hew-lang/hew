use hew_parser::ast::{
    condition_exprs_mut, Block, Expr, Ident, Item, MatchArm, Span, Spanned, Stmt, StringPart,
    TraitItem,
};
use hew_parser::macro_syntax::{ExpansionId, ExpansionMap, Origin};

use crate::ExpansionError;

/// The expansion that wrote a token, following captures through nested calls.
fn owner(map: &ExpansionMap, span: &Span) -> Option<ExpansionId> {
    let (index, expansion) = map
        .expansions
        .iter()
        .enumerate()
        .find(|(_, expansion)| expansion.synthetic.contains(&span.start))?;
    match expansion
        .origins
        .iter()
        .find(|(generated, _)| generated.contains(&span.start))
    {
        Some((_, Origin::Caller { span, .. })) => owner(map, span),
        _ => Some(ExpansionId(
            u32::try_from(index).expect("registered expansion ID fits u32"),
        )),
    }
}

fn captured_body(map: &ExpansionMap, span: &Span) -> bool {
    let Some(expansion) = map
        .expansions
        .iter()
        .find(|expansion| expansion.synthetic.contains(&span.start))
    else {
        return false;
    };
    if expansion
        .captured_bodies
        .iter()
        .any(|body| body.start == span.start)
    {
        return true;
    }
    match expansion
        .origins
        .iter()
        .find(|(generated, _)| generated.contains(&span.start))
    {
        Some((_, Origin::Caller { span, .. })) => captured_body(map, span),
        _ => false,
    }
}

struct LoopTarget {
    owner: Option<ExpansionId>,
    source_label: Option<Ident>,
    label: Ident,
    required: bool,
}

struct FunctionTarget {
    owner: Option<ExpansionId>,
}

pub(crate) struct Control<'a> {
    map: &'a ExpansionMap,
    function: Option<FunctionTarget>,
    loops: Vec<LoopTarget>,
    next_label: usize,
}

impl<'a> Control<'a> {
    pub(crate) fn new(map: &'a ExpansionMap) -> Self {
        Self {
            map,
            function: None,
            loops: Vec::new(),
            next_label: 0,
        }
    }

    fn fail(message: &str, span: &Span) -> ExpansionError {
        ExpansionError {
            message: message.into(),
            span: span.clone(),
        }
    }

    fn return_jump(&self, span: &Span) -> Result<(), ExpansionError> {
        let written = owner(self.map, span);
        if self
            .function
            .as_ref()
            .is_some_and(|target| target.owner == written)
            || written.is_none() && self.function.is_none()
        {
            return Ok(());
        }
        Err(Self::fail(
            if written.is_some() {
                "E_MACRO_ESCAPING_JUMP: a template return or `?` needs a function written by that template"
            } else {
                "E_MACRO_CAPTURED_JUMP: captured return or `?` cannot cross a template function boundary"
            },
            span,
        ))
    }

    fn loop_jump(&mut self, label: &mut Option<Ident>, span: &Span) -> Result<(), ExpansionError> {
        let written = owner(self.map, span);
        let selected = self.loops.iter().rposition(|target| {
            target.owner == written && label.is_none_or(|label| Some(label) == target.source_label)
        });
        if let Some(index) = selected {
            if label.is_some() || index + 1 != self.loops.len() {
                let target = &mut self.loops[index];
                *label = Some(target.label);
                target.required = true;
            }
            return Ok(());
        }
        if written.is_some() {
            return Err(Self::fail("E_MACRO_ESCAPING_JUMP: a template break or continue needs a loop written by that template", span));
        }
        if self.loops.iter().any(|target| target.owner.is_some())
            || self
                .function
                .as_ref()
                .is_some_and(|target| target.owner.is_some())
        {
            return Err(Self::fail(
                "E_MACRO_CAPTURED_JUMP: captured break or continue has no enclosing caller loop",
                span,
            ));
        }
        Ok(())
    }

    fn function_block(&mut self, block: &mut Block, span: &Span) -> Result<(), ExpansionError> {
        let mut function_owner = owner(self.map, span);
        let nodes: Vec<_> = block
            .stmts
            .iter()
            .map(|(_, span)| span)
            .chain(block.trailing_expr.iter().map(|expr| &expr.1))
            .collect();
        if let Some(body) = self
            .map
            .expansions
            .iter()
            .flat_map(|expansion| &expansion.captured_bodies)
            .find(|body| {
                span.start < body.start
                    && body.end <= span.end
                    && !nodes.is_empty()
                    && nodes
                        .iter()
                        .all(|node| body.start < node.start && node.start < body.end)
            })
        {
            function_owner = owner(self.map, body);
        }
        let previous = self.function.replace(FunctionTarget {
            owner: function_owner,
        });
        let loops = std::mem::take(&mut self.loops);
        let result = self.block(block);
        self.loops = loops;
        self.function = previous;
        result
    }

    fn function_expr(
        &mut self,
        body: &mut Spanned<Expr>,
        span: &Span,
    ) -> Result<(), ExpansionError> {
        let previous = self.function.replace(FunctionTarget {
            owner: owner(self.map, span),
        });
        let loops = std::mem::take(&mut self.loops);
        let result = self.expr(body);
        self.loops = loops;
        self.function = previous;
        result
    }

    fn loop_block(
        &mut self,
        label: &mut Option<Ident>,
        body: &mut Block,
        span: &Span,
    ) -> Result<(), ExpansionError> {
        let written = owner(self.map, span);
        let loop_label = loop {
            let spelling = format!("__macro_loop_{}", self.next_label);
            self.next_label += 1;
            if !self
                .map
                .sources
                .values()
                .any(|source| source.contains(&spelling))
            {
                break Ident::new(&spelling);
            }
        };
        self.loops.push(LoopTarget {
            owner: written,
            source_label: *label,
            label: loop_label,
            required: false,
        });
        self.block(body)?;
        let target = self.loops.pop().expect("loop was pushed");
        if target.required {
            *label = Some(loop_label);
        }
        Ok(())
    }

    pub(crate) fn item(&mut self, item: &mut Item, span: &Span) -> Result<(), ExpansionError> {
        match item {
            Item::Function(function) => {
                self.function_block(&mut function.body, &function.fn_span)?;
            }
            Item::Const(constant) => self.expr(&mut constant.value)?,
            Item::Impl(declaration) => {
                for function in &mut declaration.methods {
                    self.function_block(&mut function.body, &function.fn_span)?;
                }
            }
            Item::Trait(declaration) => {
                for member in &mut declaration.items {
                    if let TraitItem::Method(method) = member {
                        if let Some(body) = &mut method.body {
                            self.function_block(body, &method.span)?;
                        }
                    }
                }
            }
            Item::Actor(actor) => {
                if let Some(init) = &mut actor.init {
                    self.function_block(&mut init.body, &init.span)?;
                }
                for field in &mut actor.fields {
                    if let Some(value) = &mut field.default {
                        self.expr(value)?;
                    }
                }
                for receiver in &mut actor.receive_fns {
                    self.function_block(&mut receiver.body, &receiver.span)?;
                }
                for function in &mut actor.methods {
                    self.function_block(&mut function.body, &function.fn_span)?;
                }
            }
            Item::Supervisor(supervisor) => {
                if let Some(value) = &mut supervisor.backoff {
                    self.expr(value)?;
                }
                for child in &mut supervisor.children {
                    for (_, value) in &mut child.args {
                        self.expr(value)?;
                    }
                    if let Some(value) = &mut child.stop {
                        self.expr(value)?;
                    }
                    if let Some(value) = &mut child.count {
                        self.expr(value)?;
                    }
                }
                for function in &mut supervisor.methods {
                    self.function_block(&mut function.body, &function.fn_span)?;
                }
            }
            Item::Machine(machine) => {
                for state in &mut machine.states {
                    if let Some(body) = &mut state.entry {
                        self.function_block(body, &state.span)?;
                    }
                    if let Some(body) = &mut state.exit {
                        self.function_block(body, &state.span)?;
                    }
                }
                for transition in &mut machine.transitions {
                    if let Some(guard) = &mut transition.guard {
                        self.expr(guard)?;
                    }
                    self.function_expr(&mut transition.body, span)?;
                }
            }
            Item::Import(_)
            | Item::TypeDecl(_)
            | Item::Macro(_)
            | Item::MacroCall(_)
            | Item::TypeAlias(_)
            | Item::Record(_)
            | Item::ExternBlock(_) => {}
        }
        Ok(())
    }

    pub(crate) fn block(&mut self, block: &mut Block) -> Result<(), ExpansionError> {
        for statement in &mut block.stmts {
            self.stmt(statement)?;
        }
        if let Some(expression) = &mut block.trailing_expr {
            self.expr(expression)?;
        }
        Ok(())
    }

    fn stmt(&mut self, (statement, span): &mut Spanned<Stmt>) -> Result<(), ExpansionError> {
        match statement {
            Stmt::Let {
                value, else_block, ..
            } => {
                if let Some(value) = value {
                    self.expr(value)?;
                }
                if let Some(block) = else_block {
                    self.block(block)?;
                }
            }
            Stmt::Var { value, .. } => {
                if let Some(value) = value {
                    self.expr(value)?;
                }
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
                for condition in condition_exprs_mut(conditions) {
                    self.expr(condition)?;
                }
                self.block(body)?;
                if let Some(body) = else_body {
                    self.expr(body)?;
                }
            }
            Stmt::Match { scrutinee, arms } => {
                self.expr(scrutinee)?;
                self.arms(arms)?;
            }
            Stmt::Loop { label, body } => self.loop_block(label, body, span)?,
            Stmt::For {
                label,
                iterable,
                body,
                ..
            } => {
                self.expr(iterable)?;
                self.loop_block(label, body, span)?;
            }
            Stmt::While {
                label,
                condition,
                body,
                ..
            } => {
                self.expr(condition)?;
                self.loop_block(label, body, span)?;
            }
            Stmt::WhileLet {
                label,
                conditions,
                body,
            } => {
                for condition in condition_exprs_mut(conditions) {
                    self.expr(condition)?;
                }
                self.loop_block(label, body, span)?;
            }
            Stmt::Break { label, value } => {
                self.loop_jump(label, span)?;
                if let Some(value) = value {
                    self.expr(value)?;
                }
            }
            Stmt::Continue { label } => self.loop_jump(label, span)?,
            Stmt::Return(value) => {
                self.return_jump(span)?;
                if let Some(value) = value {
                    self.expr(value)?;
                }
            }
            Stmt::Defer { body, .. } => self.expr(body)?,
            Stmt::Expression(expression) => self.expr(expression)?,
        }
        Ok(())
    }

    fn arms(&mut self, arms: &mut [MatchArm]) -> Result<(), ExpansionError> {
        for arm in arms {
            if let Some(guard) = &mut arm.guard {
                self.expr(guard)?;
            }
            self.expr(&mut arm.body)?;
        }
        Ok(())
    }

    #[allow(
        clippy::too_many_lines,
        reason = "exhaustive expression traversal shares control-flow scope"
    )]
    pub(crate) fn expr(
        &mut self,
        (expression, span): &mut Spanned<Expr>,
    ) -> Result<(), ExpansionError> {
        if captured_body(self.map, span) {
            if let Expr::Block(block) = expression {
                return self.function_block(block, span);
            }
        }
        match expression {
            Expr::PostfixTry(value) | Expr::ReturnError(value) => {
                self.return_jump(span)?;
                self.expr(value)?;
            }
            Expr::Return(value) => {
                self.return_jump(span)?;
                if let Some(value) = value {
                    self.expr(value)?;
                }
            }
            Expr::Lambda { body, .. } | Expr::SpawnLambdaActor { body, .. } => {
                self.function_expr(body, span)?;
            }
            Expr::GenBlock { body } | Expr::ForkBlock { body } => {
                self.function_block(body, span)?;
            }
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
            | Expr::ForkChild { expr: value }
            | Expr::FieldAccess { object: value, .. }
            | Expr::GenericApplySuffix { target: value, .. }
            | Expr::Cast { expr: value, .. } => self.expr(value)?,
            Expr::Yield(value) => {
                if let Some(value) = value {
                    self.expr(value)?;
                }
            }
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
                for condition in condition_exprs_mut(conditions) {
                    self.expr(condition)?;
                }
                self.block(body)?;
                if let Some(body) = else_body {
                    self.expr(body)?;
                }
            }
            Expr::Match { scrutinee, arms } => {
                self.expr(scrutinee)?;
                self.arms(arms)?;
            }
            Expr::Spawn { target, args, .. } => {
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
            Expr::Call { function, args, .. } => {
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
            Expr::StructInit { fields, base, .. } => {
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
                    self.expr(&mut arm.body)?;
                }
                if let Some(timeout) = timeout {
                    self.expr(&mut timeout.duration)?;
                    self.expr(&mut timeout.body)?;
                }
            }
            Expr::Handle { operand, body, .. } => {
                self.expr(operand)?;
                self.expr(body)?;
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
            Expr::Ident(_)
            | Expr::Literal(_)
            | Expr::MacroCall(_)
            | Expr::QualifiedAssoc(_)
            | Expr::ChainValue
            | Expr::RegexLiteral(_)
            | Expr::ByteStringLiteral(_)
            | Expr::ByteArrayLiteral(_) => {}
        }
        Ok(())
    }
}
