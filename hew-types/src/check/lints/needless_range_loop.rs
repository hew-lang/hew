//! The `needless_range_loop` lint.
//!
//! Flags `for i in 0 .. xs.len() { ... }` loops where the index `i` is used
//! for nothing but indexing `xs` (`xs[i]` or `xs.get(i)`), so the loop is
//! exactly equivalent to iterating the collection directly
//! (`for x in xs { ... }`).
//!
//! This is the compiler-side analogue of Clippy's `needless_range_loop`: a
//! *use* check, not a global dataflow analysis. It runs on the typed AST in
//! the checker because `for` loops are lowered away (into counter loops) by the
//! time MIR exists, and because the checker already knows that the `.len()`
//! receiver is an indexable collection.
//!
//! ## Precision over recall
//!
//! The lint only fires when the rewrite is unambiguously valid, so the body
//! scan ([`BodyScan`]) is deliberately conservative — *any* shape it cannot
//! prove safe disqualifies the loop:
//!
//! - every occurrence of the index `i` must be the index of `xs[i]` or the sole
//!   positional argument of `xs.get(i)`;
//! - the collection `xs` must appear *only* inside those two access shapes (so
//!   a `xs.push(..)` / `xs.len()` / bare `xs` use, or a write through `xs[i] =`,
//!   all disqualify — this subsumes "length-mutated in the loop");
//! - neither `i` nor `xs` may be reassigned or shadowed inside the body;
//! - at least one real `xs[i]` / `xs.get(i)` access must be present.

use hew_parser::ast::{
    BinaryOp, Block, CallArg, ElseBlock, Expr, Literal, MatchArm, Pattern, SelectArm, Spanned,
    Stmt, StringPart,
};

use crate::builtin_type::BuiltinType;
use crate::error::TypeError;
use crate::ty::Ty;

use super::{LintCtx, LintId, LintLevels};

/// Entry point: discover candidate range loops anywhere in `body` and emit a
/// diagnostic for each one that survives the precision checks.
pub(super) fn check(ctx: &LintCtx, levels: &LintLevels, body: &Block, out: &mut Vec<TypeError>) {
    find_in_block(ctx, levels, body, out);
}

// ── candidate discovery ──────────────────────────────────────────────

fn find_in_block(ctx: &LintCtx, levels: &LintLevels, block: &Block, out: &mut Vec<TypeError>) {
    for (stmt, span) in &block.stmts {
        find_in_stmt(ctx, levels, stmt, span, out);
    }
    if let Some(trailing) = &block.trailing_expr {
        find_in_expr(ctx, levels, &trailing.0, out);
    }
}

#[allow(
    clippy::too_many_lines,
    reason = "statement visitor enumerates every block-bearing stmt shape"
)]
fn find_in_stmt(
    ctx: &LintCtx,
    levels: &LintLevels,
    stmt: &Stmt,
    span: &hew_parser::ast::Span,
    out: &mut Vec<TypeError>,
) {
    match stmt {
        Stmt::For {
            is_await,
            pattern,
            iterable,
            body,
            ..
        } => {
            // Test this loop, then descend so nested range loops are found too.
            try_flag(ctx, levels, *is_await, pattern, iterable, body, span, out);
            find_in_expr(ctx, levels, &iterable.0, out);
            find_in_block(ctx, levels, body, out);
        }
        Stmt::Loop { body, .. } => find_in_block(ctx, levels, body, out),
        Stmt::While {
            condition, body, ..
        } => {
            find_in_expr(ctx, levels, &condition.0, out);
            find_in_block(ctx, levels, body, out);
        }
        Stmt::WhileLet { expr, body, .. } => {
            find_in_expr(ctx, levels, &expr.0, out);
            find_in_block(ctx, levels, body, out);
        }
        Stmt::If {
            condition,
            then_block,
            else_block,
        } => {
            find_in_expr(ctx, levels, &condition.0, out);
            find_in_block(ctx, levels, then_block, out);
            if let Some(eb) = else_block {
                find_in_else(ctx, levels, eb, out);
            }
        }
        Stmt::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            find_in_expr(ctx, levels, &expr.0, out);
            find_in_block(ctx, levels, body, out);
            if let Some(eb) = else_body {
                find_in_block(ctx, levels, eb, out);
            }
        }
        Stmt::Match { scrutinee, arms } => {
            find_in_expr(ctx, levels, &scrutinee.0, out);
            for arm in arms {
                find_in_arm(ctx, levels, arm, out);
            }
        }
        Stmt::Let {
            value, else_block, ..
        } => {
            if let Some(v) = value {
                find_in_expr(ctx, levels, &v.0, out);
            }
            if let Some(eb) = else_block {
                find_in_block(ctx, levels, eb, out);
            }
        }
        Stmt::Var { value, .. } | Stmt::Break { value, .. } | Stmt::Return(value) => {
            if let Some(v) = value {
                find_in_expr(ctx, levels, &v.0, out);
            }
        }
        Stmt::Assign { target, value, .. } => {
            find_in_expr(ctx, levels, &target.0, out);
            find_in_expr(ctx, levels, &value.0, out);
        }
        Stmt::Defer(expr) => find_in_expr(ctx, levels, &expr.0, out),
        Stmt::Expression(expr) => find_in_expr(ctx, levels, &expr.0, out),
        Stmt::Continue { .. } => {}
    }
}

fn find_in_else(
    ctx: &LintCtx,
    levels: &LintLevels,
    else_block: &ElseBlock,
    out: &mut Vec<TypeError>,
) {
    if let Some(if_stmt) = &else_block.if_stmt {
        find_in_stmt(ctx, levels, &if_stmt.0, &if_stmt.1, out);
    }
    if let Some(block) = &else_block.block {
        find_in_block(ctx, levels, block, out);
    }
}

fn find_in_arm(ctx: &LintCtx, levels: &LintLevels, arm: &MatchArm, out: &mut Vec<TypeError>) {
    if let Some(guard) = &arm.guard {
        find_in_expr(ctx, levels, &guard.0, out);
    }
    find_in_expr(ctx, levels, &arm.body.0, out);
}

/// Descend into the block-bearing expression shapes so nested loops are
/// discovered. This walk only needs to reach *blocks* (where `for` statements
/// live); leaf and non-nesting expressions are ignored — that only affects
/// recall, never soundness, since each discovered loop is independently
/// verified by [`BodyScan`].
#[allow(
    clippy::too_many_lines,
    reason = "expression visitor enumerates every block-bearing expr shape"
)]
fn find_in_expr(ctx: &LintCtx, levels: &LintLevels, expr: &Expr, out: &mut Vec<TypeError>) {
    match expr {
        Expr::Block(block) | Expr::Scope { body: block } | Expr::ForkBlock { body: block } => {
            find_in_block(ctx, levels, block, out);
        }
        Expr::UnsafeBlock(block) => find_in_block(ctx, levels, block, out),
        Expr::GenBlock { body } => find_in_block(ctx, levels, body, out),
        Expr::If {
            condition,
            then_block,
            else_block,
        } => {
            find_in_expr(ctx, levels, &condition.0, out);
            find_in_expr(ctx, levels, &then_block.0, out);
            if let Some(eb) = else_block {
                find_in_expr(ctx, levels, &eb.0, out);
            }
        }
        Expr::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            find_in_expr(ctx, levels, &expr.0, out);
            find_in_block(ctx, levels, body, out);
            if let Some(eb) = else_body {
                find_in_block(ctx, levels, eb, out);
            }
        }
        Expr::Match { scrutinee, arms } => {
            find_in_expr(ctx, levels, &scrutinee.0, out);
            for arm in arms {
                find_in_arm(ctx, levels, arm, out);
            }
        }
        Expr::Lambda { body, .. } | Expr::SpawnLambdaActor { body, .. } => {
            find_in_expr(ctx, levels, &body.0, out);
        }
        Expr::ScopeDeadline { duration, body } => {
            find_in_expr(ctx, levels, &duration.0, out);
            find_in_block(ctx, levels, body, out);
        }
        Expr::Timeout { expr, duration } => {
            find_in_expr(ctx, levels, &expr.0, out);
            find_in_expr(ctx, levels, &duration.0, out);
        }
        Expr::Await(inner)
        | Expr::Clone(inner)
        | Expr::PostfixTry(inner)
        | Expr::Unary { operand: inner, .. }
        | Expr::ForkChild { expr: inner, .. } => find_in_expr(ctx, levels, &inner.0, out),
        Expr::Binary { left, right, .. } => {
            find_in_expr(ctx, levels, &left.0, out);
            find_in_expr(ctx, levels, &right.0, out);
        }
        // Remaining shapes do not introduce blocks that can hold a `for`
        // statement at the granularity this lint cares about; skipping them
        // only lowers recall.
        _ => {}
    }
}

// ── candidate matching ───────────────────────────────────────────────

/// If this `for` loop is `for i in 0 .. coll.len()` over a lintable collection
/// `coll`, run the precision scan and emit when the rewrite is valid.
#[allow(clippy::too_many_arguments, reason = "destructured `Stmt::For` fields")]
fn try_flag(
    ctx: &LintCtx,
    levels: &LintLevels,
    is_await: bool,
    pattern: &Spanned<Pattern>,
    iterable: &Spanned<Expr>,
    body: &Block,
    for_span: &hew_parser::ast::Span,
    out: &mut Vec<TypeError>,
) {
    if is_await {
        return;
    }
    // pattern must be a plain `i` binding.
    let Pattern::Identifier(idx) = &pattern.0 else {
        return;
    };
    // iterable must be `0 .. <expr>` (exclusive range, literal-zero start).
    let Expr::Binary {
        left,
        op: BinaryOp::Range,
        right,
    } = &iterable.0
    else {
        return;
    };
    if !matches!(&left.0, Expr::Literal(Literal::Integer { value: 0, .. })) {
        return;
    }
    // end must be `coll.len()` with `coll` a plain identifier.
    let Expr::MethodCall {
        receiver,
        method,
        args,
    } = &right.0
    else {
        return;
    };
    if method != "len" || !args.is_empty() {
        return;
    }
    let Expr::Identifier(coll) = &receiver.0 else {
        return;
    };
    // `coll` must be a collection where `for x in coll` yields exactly the
    // elements that `coll[i]` / `coll.get(i)` produce.
    if !ctx
        .resolved_type_at(&receiver.1)
        .is_some_and(|ty| is_lintable_collection(&ty))
    {
        return;
    }

    let mut scan = BodyScan {
        idx: idx.as_str(),
        coll: coll.as_str(),
        ok: true,
        accesses: 0,
    };
    scan.block(body);
    if !scan.ok || scan.accesses == 0 {
        return;
    }

    let elem = suggested_elem_name(coll);
    ctx.emit(
        levels,
        LintId::NeedlessRangeLoop,
        for_span,
        format!("the loop variable `{idx}` is only used to index `{coll}`"),
        format!("iterate the collection directly: `for {elem} in {coll} {{ … }}`"),
        out,
    );
}

/// Collections for which `for x in coll` is exactly equivalent to indexing
/// `coll[0..coll.len()]`. Restricted to `Vec<_>` for M1 (precision over
/// recall); extend here once another type's iteration/index parity is
/// confirmed.
fn is_lintable_collection(ty: &Ty) -> bool {
    matches!(
        ty,
        Ty::Named {
            builtin: Some(BuiltinType::Vec),
            ..
        }
    )
}

/// A crude singularisation for the suggested element name: `xs` → `x`,
/// `items` → `item`; anything that does not end in `s` falls back to `item`.
fn suggested_elem_name(coll: &str) -> String {
    if let Some(stripped) = coll.strip_suffix('s') {
        if !stripped.is_empty() {
            return stripped.to_string();
        }
    }
    "item".to_string()
}

// ── body scan (the precision check) ──────────────────────────────────

/// Conservative scanner over a candidate loop body.
///
/// Walks the whole body; sets `ok = false` the moment it sees anything that
/// makes the `for x in coll` rewrite unsafe, and counts the legitimate
/// `coll[i]` / `coll.get(i)` accesses so a loop that never indexes is not
/// flagged. The `Expr` / `Stmt` matches are exhaustive (no wildcard) so a new
/// AST node forces a deliberate decision here rather than silently slipping
/// through as a false positive.
struct BodyScan<'a> {
    idx: &'a str,
    coll: &'a str,
    ok: bool,
    accesses: usize,
}

impl BodyScan<'_> {
    fn block(&mut self, block: &Block) {
        for (stmt, _) in &block.stmts {
            self.stmt(stmt);
            if !self.ok {
                return;
            }
        }
        if let Some(trailing) = &block.trailing_expr {
            self.expr(&trailing.0);
        }
    }

    #[allow(
        clippy::too_many_lines,
        reason = "exhaustive statement visitor keeps the precision check sound"
    )]
    fn stmt(&mut self, stmt: &Stmt) {
        if !self.ok {
            return;
        }
        match stmt {
            Stmt::Let {
                pattern,
                value,
                else_block,
                ..
            } => {
                if self.pattern_shadows(&pattern.0) {
                    self.ok = false;
                    return;
                }
                if let Some(v) = value {
                    self.expr(&v.0);
                }
                if let Some(eb) = else_block {
                    self.block(eb);
                }
            }
            Stmt::Var { name, value, .. } => {
                if name == self.idx || name == self.coll {
                    self.ok = false;
                    return;
                }
                if let Some(v) = value {
                    self.expr(&v.0);
                }
            }
            Stmt::Assign { target, value, .. } => {
                // A write whose target mentions `i` or `coll` (reassigning the
                // index, reassigning the collection, or writing through
                // `coll[i]`) makes the element-iteration rewrite invalid.
                if expr_mentions(self.idx, self.coll, &target.0) {
                    self.ok = false;
                    return;
                }
                self.expr(&value.0);
            }
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                self.expr(&condition.0);
                self.block(then_block);
                if let Some(eb) = else_block {
                    self.else_block(eb);
                }
            }
            Stmt::IfLet {
                pattern,
                expr,
                body,
                else_body,
            } => {
                if self.pattern_shadows(&pattern.0) {
                    self.ok = false;
                    return;
                }
                self.expr(&expr.0);
                self.block(body);
                if let Some(eb) = else_body {
                    self.block(eb);
                }
            }
            Stmt::Match { scrutinee, arms } => {
                self.expr(&scrutinee.0);
                for arm in arms {
                    self.arm(arm);
                    if !self.ok {
                        return;
                    }
                }
            }
            Stmt::Loop { body, .. } => self.block(body),
            Stmt::For {
                pattern,
                iterable,
                body,
                ..
            } => {
                if self.pattern_shadows(&pattern.0) {
                    self.ok = false;
                    return;
                }
                self.expr(&iterable.0);
                self.block(body);
            }
            Stmt::While {
                condition, body, ..
            } => {
                self.expr(&condition.0);
                self.block(body);
            }
            Stmt::WhileLet {
                pattern,
                expr,
                body,
                ..
            } => {
                if self.pattern_shadows(&pattern.0) {
                    self.ok = false;
                    return;
                }
                self.expr(&expr.0);
                self.block(body);
            }
            Stmt::Break { value, .. } | Stmt::Return(value) => {
                if let Some(v) = value {
                    self.expr(&v.0);
                }
            }
            Stmt::Continue { .. } => {}
            Stmt::Defer(expr) => self.expr(&expr.0),
            Stmt::Expression(expr) => self.expr(&expr.0),
        }
    }

    fn else_block(&mut self, else_block: &ElseBlock) {
        if let Some(if_stmt) = &else_block.if_stmt {
            self.stmt(&if_stmt.0);
        }
        if let Some(block) = &else_block.block {
            self.block(block);
        }
    }

    fn arm(&mut self, arm: &MatchArm) {
        if self.pattern_shadows(&arm.pattern.0) {
            self.ok = false;
            return;
        }
        if let Some(guard) = &arm.guard {
            self.expr(&guard.0);
        }
        self.expr(&arm.body.0);
    }

    #[allow(
        clippy::too_many_lines,
        reason = "exhaustive expression visitor keeps the precision check sound"
    )]
    fn expr(&mut self, expr: &Expr) {
        if !self.ok {
            return;
        }
        // The two permitted shapes consume both `coll` and `i`; count the
        // access and stop descending so the inner identifiers are not flagged.
        if self.is_allowed_access(expr) {
            self.accesses += 1;
            return;
        }
        match expr {
            Expr::Identifier(name) => {
                if name == self.idx || name == self.coll {
                    self.ok = false;
                }
            }
            Expr::This
            | Expr::Literal(_)
            | Expr::RegexLiteral(_)
            | Expr::ByteStringLiteral(_)
            | Expr::ByteArrayLiteral(_) => {}
            Expr::Binary { left, right, .. } => {
                self.expr(&left.0);
                self.expr(&right.0);
            }
            Expr::Unary { operand, .. } => self.expr(&operand.0),
            Expr::Clone(inner)
            | Expr::Await(inner)
            | Expr::PostfixTry(inner)
            | Expr::Cast { expr: inner, .. }
            | Expr::FieldAccess { object: inner, .. } => self.expr(&inner.0),
            Expr::Tuple(items) | Expr::Array(items) | Expr::Join(items) => {
                for item in items {
                    self.expr(&item.0);
                }
            }
            Expr::ArrayRepeat { value, count } => {
                self.expr(&value.0);
                self.expr(&count.0);
            }
            Expr::MapLiteral { entries } => {
                for (k, v) in entries {
                    self.expr(&k.0);
                    self.expr(&v.0);
                }
            }
            Expr::Block(block)
            | Expr::Scope { body: block }
            | Expr::ForkBlock { body: block }
            | Expr::GenBlock { body: block } => self.block(block),
            Expr::UnsafeBlock(block) => self.block(block),
            Expr::If {
                condition,
                then_block,
                else_block,
            } => {
                self.expr(&condition.0);
                self.expr(&then_block.0);
                if let Some(eb) = else_block {
                    self.expr(&eb.0);
                }
            }
            Expr::IfLet {
                pattern,
                expr,
                body,
                else_body,
            } => {
                if self.pattern_shadows(&pattern.0) {
                    self.ok = false;
                    return;
                }
                self.expr(&expr.0);
                self.block(body);
                if let Some(eb) = else_body {
                    self.block(eb);
                }
            }
            Expr::Match { scrutinee, arms } => {
                self.expr(&scrutinee.0);
                for arm in arms {
                    self.arm(arm);
                    if !self.ok {
                        return;
                    }
                }
            }
            Expr::Lambda { params, body, .. } => {
                if params
                    .iter()
                    .any(|p| p.name == self.idx || p.name == self.coll)
                {
                    self.ok = false;
                    return;
                }
                self.expr(&body.0);
            }
            Expr::SpawnLambdaActor { params, body, .. } => {
                if params
                    .iter()
                    .any(|p| p.name == self.idx || p.name == self.coll)
                {
                    self.ok = false;
                    return;
                }
                self.expr(&body.0);
            }
            Expr::Spawn { target, args, .. } => {
                self.expr(&target.0);
                for (_, value) in args {
                    self.expr(&value.0);
                }
            }
            Expr::ForkChild { binding, expr } => {
                if binding.as_deref() == Some(self.idx) || binding.as_deref() == Some(self.coll) {
                    self.ok = false;
                    return;
                }
                self.expr(&expr.0);
            }
            Expr::ScopeDeadline { duration, body } => {
                self.expr(&duration.0);
                self.block(body);
            }
            Expr::InterpolatedString(parts) => {
                for part in parts {
                    if let StringPart::Expr(inner) = part {
                        self.expr(&inner.0);
                    }
                }
            }
            Expr::Call { function, args, .. } => {
                self.expr(&function.0);
                self.call_args(args);
            }
            Expr::MethodCall { receiver, args, .. } => {
                self.expr(&receiver.0);
                self.call_args(args);
            }
            Expr::StructInit { fields, base, .. } => {
                for (_, value) in fields {
                    self.expr(&value.0);
                }
                if let Some(base) = base {
                    self.expr(&base.0);
                }
            }
            Expr::Select { arms, timeout } => {
                for arm in arms {
                    self.select_arm(arm);
                    if !self.ok {
                        return;
                    }
                }
                if let Some(timeout) = timeout {
                    self.expr(&timeout.duration.0);
                    self.expr(&timeout.body.0);
                }
            }
            Expr::Timeout { expr, duration } => {
                self.expr(&expr.0);
                self.expr(&duration.0);
            }
            Expr::Yield(value) | Expr::Return(value) => {
                if let Some(v) = value {
                    self.expr(&v.0);
                }
            }
            Expr::Index { object, index } => {
                self.expr(&object.0);
                self.expr(&index.0);
            }
            Expr::Range { start, end, .. } => {
                if let Some(start) = start {
                    self.expr(&start.0);
                }
                if let Some(end) = end {
                    self.expr(&end.0);
                }
            }
            Expr::Is { lhs, rhs } => {
                self.expr(&lhs.0);
                self.expr(&rhs.0);
            }
            Expr::MachineEmit { fields, .. } => {
                for (_, value) in fields {
                    self.expr(&value.0);
                }
            }
        }
    }

    fn call_args(&mut self, args: &[CallArg]) {
        for arg in args {
            self.expr(&arg.expr().0);
            if !self.ok {
                return;
            }
        }
    }

    fn select_arm(&mut self, arm: &SelectArm) {
        if self.pattern_shadows(&arm.binding.0) {
            self.ok = false;
            return;
        }
        self.expr(&arm.source.0);
        self.expr(&arm.body.0);
    }

    /// Is `expr` one of the two permitted index accesses (`coll[i]` /
    /// `coll.get(i)`) on exactly this loop's collection and index?
    fn is_allowed_access(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Index { object, index } => {
                is_ident(&object.0, self.coll) && is_ident(&index.0, self.idx)
            }
            Expr::MethodCall {
                receiver,
                method,
                args,
            } => {
                method == "get"
                    && is_ident(&receiver.0, self.coll)
                    && args.len() == 1
                    && matches!(
                        &args[0],
                        CallArg::Positional((Expr::Identifier(name), _)) if name == self.idx
                    )
            }
            _ => false,
        }
    }

    /// Does `pattern` bind a name equal to the loop's index or collection?
    /// Such a binding shadows the loop variable, so the access analysis can no
    /// longer prove safety — disqualify conservatively.
    fn pattern_shadows(&self, pattern: &Pattern) -> bool {
        pattern_binds(pattern, self.idx) || pattern_binds(pattern, self.coll)
    }
}

fn is_ident(expr: &Expr, name: &str) -> bool {
    matches!(expr, Expr::Identifier(n) if n == name)
}

/// Whether `pattern` introduces a binding named `name`.
fn pattern_binds(pattern: &Pattern, name: &str) -> bool {
    match pattern {
        Pattern::Identifier(n) => n == name,
        Pattern::Constructor { patterns, .. } | Pattern::Tuple(patterns) => {
            patterns.iter().any(|p| pattern_binds(&p.0, name))
        }
        Pattern::Struct { fields, .. } | Pattern::RecordShorthand { fields } => {
            fields.iter().any(|field| match &field.pattern {
                Some(sub) => pattern_binds(&sub.0, name),
                None => field.name == name,
            })
        }
        Pattern::Or(a, b) => pattern_binds(&a.0, name) || pattern_binds(&b.0, name),
        Pattern::Wildcard | Pattern::Literal(_) | Pattern::Regex { .. } => false,
    }
}

/// Strict reference check used on assignment *targets*: does `expr` mention the
/// index or the collection anywhere? Targets are l-values (identifiers, index,
/// field access), so unexpected shapes conservatively count as a mention.
fn expr_mentions(idx: &str, coll: &str, expr: &Expr) -> bool {
    match expr {
        Expr::Identifier(name) => name == idx || name == coll,
        Expr::Index { object, index } => {
            expr_mentions(idx, coll, &object.0) || expr_mentions(idx, coll, &index.0)
        }
        Expr::FieldAccess { object, .. } => expr_mentions(idx, coll, &object.0),
        Expr::This => false,
        _ => true,
    }
}
