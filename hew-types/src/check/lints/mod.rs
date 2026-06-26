//! Compiler lint pass infrastructure (milestone M1 core).
//!
//! This module hosts the *semantic* lint layer — idiom / code-smell findings
//! that need more than syntax (def-use, types) and therefore live in the
//! compiler rather than in the syntactic ast-grep rule set. Lints are
//! non-fatal by default: each is emitted through the checker's existing
//! [`TypeError`] warning channel ([`super::TypeCheckOutput::warnings`]), which
//! already reaches the CLI, the LSP, and the website with no extra plumbing.
//!
//! ## Public surface (for the CLI / suppression integration)
//!
//! - [`LintId`] — a stable enum of known lints, each with a stable
//!   [`LintId::as_str`] name and a [`LintId::default_level`].
//! - [`LintLevel`] — `Allow` / `Warn` / `Deny`.
//! - [`LintLevels`] — the per-compilation level map. Build it with
//!   [`LintLevels::from_defaults`], read with [`LintLevels::level`], and
//!   reconfigure with [`LintLevels::set`]. The checker stores one of these
//!   (see [`super::Checker::set_lint_levels`]); the CLI threads `--warn` /
//!   `--allow` / `--deny` flags into it before [`super::Checker::check_program`]
//!   runs the sweep.
//! - [`LintSources`] — the per-compilation source text, keyed by module, that
//!   backs in-source `// hew:allow(...)` suppression. The CLI installs it via
//!   [`super::Checker::set_lint_sources`]; a directive on (or above) a finding's
//!   line drops it, even under `Deny`.
//!
//! ## The sweep
//!
//! [`super::Checker::run_lints`] walks every function/method body read-only,
//! constructs a [`LintCtx`] carrying the checker's resolved type facts, and
//! invokes each enabled lint. A lint whose resolved level is `Allow` emits
//! nothing; `Deny` emits at [`Severity::Error`], everything else at
//! [`Severity::Warning`]. The diagnostic carries its originating lint id on
//! [`TypeErrorKind::Lint`] so suppression, `--Werror`, and docs can key off it.

use std::collections::HashMap;

use hew_parser::ast::{
    Block, CallArg, ElseBlock, Expr, MatchArm, SelectArm, Span, Stmt, StringPart,
};

use crate::error::{Severity, TypeError, TypeErrorKind};
use crate::ty::{Substitution, Ty};

use super::types::SpanKey;

mod len_zero_comparison;
mod needless_bool;
mod needless_match_to_if_let;
mod needless_range_loop;
mod redundant_else_after_return;

/// Stable identifier for a single compiler lint.
///
/// Extend this enum (and [`LintId::ALL`]) when adding a lint; the exhaustive
/// matches on [`LintId::as_str`] / [`LintId::default_level`] then force every
/// new lint to declare a stable name and a default level.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LintId {
    /// `for i in 0 .. xs.len()` where `i` is only ever used to index `xs` —
    /// the loop should iterate the collection directly.
    NeedlessRangeLoop,
    /// An `if` whose then-branch unconditionally diverges (`return` / `break`
    /// / `continue` / a `!`-typed call such as `panic`) yet still carries an
    /// `else`; the `else` can be dropped and its body de-indented.
    RedundantElseAfterReturn,
    /// A two-arm `match` on an `Option` where one arm binds `Some(x)` and the
    /// other is an empty `None => {}` — exactly an `if let Some(x) = …`.
    NeedlessMatchToIfLet,
    /// A `.len()` call compared against the integer literal `0` / `1` where the
    /// receiver type has `is_empty()` — use `is_empty()` / `!is_empty()`.
    LenZeroComparison,
    /// `if c { true } else { false }` (or the inverted polarity) — the whole
    /// `if` is just `c` (or `!c`).
    NeedlessBool,
    /// `.clone()` on a `Copy` type — the value is already duplicated, so the
    /// `clone` is redundant. (Migrated from an ad-hoc style warning.)
    CloneOnCopy,
    /// A function that is defined but never reached from any entry point.
    /// (Migrated from an ad-hoc whole-program dead-code warning.)
    DeadCode,
    /// A value assigned to a local is never read on any path before the local
    /// is overwritten or goes out of scope — the store is dead. Emitted by the
    /// MIR-stage liveness pass (`hew-mir`), not the HIR checker sweep.
    DeadStore,
    // NOTE: a `clean_counter` lint (a loop-carried accumulator that is
    // incremented but never read after the loop) is deliberately NOT registered
    // here. It has no emission code yet, and registering an un-emitted lint
    // would make `-D/-W/-A clean_counter` and `// hew:allow(clean_counter)`
    // silently no-op — a fail-open that defeats `from_name`'s fail-closed
    // contract (an unknown lint must surface as a CLI error). Detecting it
    // precisely needs faint-variable analysis; tracked in issue #2178.
}

impl LintId {
    /// Every lint known to the registry, in declaration order.
    ///
    /// Used to seed [`LintLevels::from_defaults`] and to back
    /// [`LintId::from_name`] so the CLI flag parser stays in sync.
    pub const ALL: &'static [LintId] = &[
        LintId::NeedlessRangeLoop,
        LintId::RedundantElseAfterReturn,
        LintId::NeedlessMatchToIfLet,
        LintId::LenZeroComparison,
        LintId::NeedlessBool,
        LintId::CloneOnCopy,
        LintId::DeadCode,
        LintId::DeadStore,
    ];

    /// The stable, lowercase string name for this lint.
    ///
    /// This is the spelling accepted by the CLI `--warn` / `--allow` /
    /// `--deny` flags and surfaced on the diagnostic; it is part of the public
    /// contract and must not change without updating docs and editor tooling.
    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            LintId::NeedlessRangeLoop => "needless_range_loop",
            LintId::RedundantElseAfterReturn => "redundant_else_after_return",
            LintId::NeedlessMatchToIfLet => "needless_match_to_if_let",
            LintId::LenZeroComparison => "len_zero_comparison",
            LintId::NeedlessBool => "needless_bool",
            LintId::CloneOnCopy => "clone_on_copy",
            LintId::DeadCode => "dead_code",
            LintId::DeadStore => "dead_store",
        }
    }

    /// Parse a lint name (as accepted by the CLI flags) back into a [`LintId`].
    ///
    /// Returns `None` for an unknown name so the caller can fail closed with a
    /// clear "unknown lint" diagnostic rather than silently ignoring the flag.
    #[must_use]
    pub fn from_name(name: &str) -> Option<LintId> {
        LintId::ALL.iter().copied().find(|id| id.as_str() == name)
    }

    /// The level applied when neither the CLI nor in-source attributes
    /// override this lint.
    #[must_use]
    pub fn default_level(self) -> LintLevel {
        match self {
            LintId::NeedlessRangeLoop
            | LintId::RedundantElseAfterReturn
            | LintId::NeedlessMatchToIfLet
            | LintId::LenZeroComparison
            | LintId::NeedlessBool
            | LintId::CloneOnCopy
            | LintId::DeadCode
            | LintId::DeadStore => LintLevel::Warn,
        }
    }
}

/// The configured reporting level of a lint.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LintLevel {
    /// Emit nothing for this lint.
    Allow,
    /// Emit a non-fatal [`Severity::Warning`] diagnostic.
    Warn,
    /// Emit a fatal [`Severity::Error`] diagnostic (fails the build like any
    /// other type error).
    Deny,
}

/// Per-compilation resolution of every lint's reporting level.
///
/// Seeded from [`LintId::default_level`] via [`LintLevels::from_defaults`] and
/// then reconfigured by the CLI layer through [`LintLevels::set`] before the
/// checker runs its sweep. Any lint not explicitly configured resolves to its
/// [`LintId::default_level`].
#[derive(Debug, Clone)]
pub struct LintLevels {
    levels: HashMap<LintId, LintLevel>,
}

impl LintLevels {
    /// Build a level map seeded with every lint's [`LintId::default_level`].
    #[must_use]
    pub fn from_defaults() -> Self {
        let mut levels = HashMap::with_capacity(LintId::ALL.len());
        for id in LintId::ALL.iter().copied() {
            levels.insert(id, id.default_level());
        }
        Self { levels }
    }

    /// The resolved level for `id` (its default when not explicitly set).
    #[must_use]
    pub fn level(&self, id: LintId) -> LintLevel {
        self.levels
            .get(&id)
            .copied()
            .unwrap_or_else(|| id.default_level())
    }

    /// Override the level for `id`. Used by the CLI to apply `--warn` /
    /// `--allow` / `--deny`.
    pub fn set(&mut self, id: LintId, level: LintLevel) {
        self.levels.insert(id, level);
    }
}

impl Default for LintLevels {
    fn default() -> Self {
        Self::from_defaults()
    }
}

/// Per-compilation source text, keyed by module, for in-source suppression.
///
/// The checker only carries byte-offset [`Span`]s, not source text, so the
/// front end hands it the program's source(s) through
/// [`super::Checker::set_lint_sources`] before [`super::Checker::check_program`].
/// A lint then resolves the `// hew:allow(...)` directive on (or above) the line
/// of its finding's span. `root` is the entry source the user compiled;
/// `modules` maps a non-root module's dotted name (the same key
/// [`LintCtx::source_module`] carries) to that module's source.
#[derive(Debug, Clone, Default)]
pub struct LintSources {
    root: Option<String>,
    modules: HashMap<String, String>,
}

impl LintSources {
    /// An empty source set (no suppression resolution possible).
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Install the root (entry) source — the file passed to `hew check`/`build`.
    pub fn set_root(&mut self, source: String) {
        self.root = Some(source);
    }

    /// Install a non-root module's source, keyed by its dotted module name
    /// (e.g. `"std.net.http"`), matching the `source_module` tag the lint
    /// sweep stamps onto that module's diagnostics.
    pub fn set_module(&mut self, module: String, source: String) {
        self.modules.insert(module, source);
    }

    /// The source text owning `module`'s spans: the root source when
    /// `module` is `None`, otherwise the named module's source (if known).
    pub(super) fn source_for(&self, module: Option<&str>) -> Option<&str> {
        match module {
            None => self.root.as_deref(),
            Some(name) => self.modules.get(name).map(String::as_str),
        }
    }
}

/// Read-only view of the checker's type facts that a lint needs.
///
/// Borrows the resolved-type side table and the substitution so a lint can ask
/// "what is the checker type of the expression at this span?" without mutating
/// checker state. `module_idx` selects the right [`SpanKey`] namespace (0 =
/// root unit, N = N-th non-root module) and `source_module` tags emitted
/// diagnostics so the CLI routes them to the correct source file.
pub(super) struct LintCtx<'a> {
    pub subst: &'a Substitution,
    pub expr_types: &'a HashMap<SpanKey, Ty>,
    pub module_idx: u32,
    pub source_module: Option<&'a str>,
    /// Source text owning this body's spans, used to resolve in-source
    /// `// hew:allow(...)` directives. `None` when the front end did not
    /// install sources (e.g. internal callers of `check_program`), in which
    /// case suppression is simply skipped.
    pub source: Option<&'a str>,
}

impl LintCtx<'_> {
    /// The fully-resolved checker type recorded for the expression at `span`.
    ///
    /// Returns `None` when no type was recorded for the span — lints treat a
    /// missing type as "unknown" and stay silent (precision over recall).
    fn resolved_type_at(&self, span: &Span) -> Option<Ty> {
        let key = SpanKey::in_module(span, self.module_idx);
        self.expr_types.get(&key).map(|ty| self.subst.resolve(ty))
    }

    /// Emit one lint diagnostic, honouring suppression and the configured
    /// [`LintLevel`].
    ///
    /// An in-source `// hew:allow(<id>)` (or `// hew:allow(all)`) directive on
    /// the finding's own line or on the contiguous comment/blank lines directly
    /// above it drops the diagnostic outright — even under `Deny` — mirroring
    /// the rustc/Clippy rule that a local `allow` wins over a command-line
    /// `deny`. Otherwise `Allow` drops the finding, `Warn` emits a
    /// [`Severity::Warning`], and `Deny` emits a [`Severity::Error`]; the caller
    /// (the checker finalization) routes warnings and errors into the right
    /// output vector by severity.
    fn emit(
        &self,
        levels: &LintLevels,
        id: LintId,
        span: &Span,
        message: String,
        suggestion: String,
        out: &mut Vec<TypeError>,
    ) {
        if self
            .source
            .is_some_and(|source| directive_suppresses(source, span.start, id))
        {
            return;
        }
        let severity = match levels.level(id) {
            LintLevel::Allow => return,
            LintLevel::Warn => Severity::Warning,
            LintLevel::Deny => Severity::Error,
        };
        out.push(TypeError {
            severity,
            kind: TypeErrorKind::Lint(id),
            span: span.clone(),
            message,
            notes: Vec::new(),
            suggestions: vec![suggestion],
            source_module: self.source_module.map(str::to_string),
        });
    }
}

/// Whether an in-source `// hew:allow(...)` directive suppresses lint `id` for a
/// construct beginning at byte offset `span_start` in `source`.
///
/// A directive is honoured when it appears either as a trailing comment on the
/// construct's own line, or on the contiguous run of comment / blank lines
/// directly above it (the first line of real code stops the search). This
/// covers `// hew:allow(needless_range_loop)` placed on the line above the loop
/// and, for item-bodied constructs reached through only comments, the
/// item-level form. `all` matches every lint.
///
/// Exposed beyond the checker so MIR-stage lints surfaced through the CLI
/// (`dead_store`) resolve `// hew:allow(...)` directives through the same path
/// as the HIR sweep instead of re-implementing it.
#[must_use]
pub fn directive_suppresses(source: &str, span_start: usize, id: LintId) -> bool {
    let lines: Vec<&str> = source.lines().collect();
    if lines.is_empty() {
        return false;
    }
    let construct_line = line_index_at(source, span_start).min(lines.len() - 1);

    if comment_allows(lines[construct_line], id) {
        return true;
    }
    let mut idx = construct_line;
    while idx > 0 {
        idx -= 1;
        let trimmed = lines[idx].trim_start();
        if trimmed.is_empty() {
            continue;
        }
        if trimmed.starts_with("//") {
            if comment_allows(lines[idx], id) {
                return true;
            }
            continue;
        }
        break;
    }
    false
}

/// 0-based index of the line containing byte offset `offset` (clamped to the
/// source length), counting `\n` terminators so it aligns with [`str::lines`].
fn line_index_at(source: &str, offset: usize) -> usize {
    let clamped = offset.min(source.len());
    source
        .bytes()
        .take(clamped)
        .filter(|&byte| byte == b'\n')
        .count()
}

/// Whether `line`'s comment carries a `// hew:allow(...)` directive naming `id`
/// (or `all`). Only the text after the first `//` is considered, so a directive
/// must live in a comment rather than in code.
fn comment_allows(line: &str, id: LintId) -> bool {
    let Some(comment_at) = line.find("//") else {
        return false;
    };
    let comment = &line[comment_at..];
    let Some(open) = comment.find("hew:allow(") else {
        return false;
    };
    let rest = &comment[open + "hew:allow(".len()..];
    let Some(close) = rest.find(')') else {
        return false;
    };
    rest[..close]
        .split(',')
        .map(str::trim)
        .any(|name| name == "all" || name == id.as_str())
}

/// Run every enabled lint over one function/method body.
///
/// Invoked once per body by [`super::Checker::run_lints`]; each lint is a pure
/// read-only visitor over the typed AST.
pub(super) fn lint_block(
    ctx: &LintCtx,
    levels: &LintLevels,
    body: &Block,
    out: &mut Vec<TypeError>,
) {
    needless_range_loop::check(ctx, levels, body, out);
    redundant_else_after_return::check(ctx, levels, body, out);
    needless_match_to_if_let::check(ctx, levels, body, out);
    len_zero_comparison::check(ctx, levels, body, out);
    needless_bool::check(ctx, levels, body, out);
}

// ── shared read-only body walker ─────────────────────────────────────

/// Read-only pre-order visitor over a typed body.
///
/// A lint implements only the callbacks it needs; [`walk_body`] then drives the
/// exhaustive descent so every lint shares one traversal instead of
/// re-deriving its own block walker (the `needless_range_loop` module predates
/// this helper and keeps its bespoke use-tracking walk).
///
/// - [`NodeVisitor::visit_block`] sees a [`Block`] with full positional
///   context: its `stmts` are in statement position (value discarded) and its
///   `trailing_expr` is in tail position (the block's value). The
///   position-sensitive lints (`redundant_else_after_return`,
///   `needless_match_to_if_let`) key off this so they only rewrite where the
///   transform is sound.
/// - [`NodeVisitor::visit_stmt`] / [`NodeVisitor::visit_expr`] fire for every
///   node regardless of position, which the position-agnostic lints
///   (`len_zero_comparison`, `needless_bool`) use.
pub(super) trait NodeVisitor {
    /// A block, visited before its contents are descended.
    fn visit_block(&mut self, _block: &Block) {}
    /// A statement, visited before its sub-nodes are descended.
    fn visit_stmt(&mut self, _stmt: &Stmt, _span: &Span) {}
    /// An expression, visited before its sub-expressions are descended.
    fn visit_expr(&mut self, _expr: &Expr, _span: &Span) {}
}

/// Drive `visitor` over `body` and every node nested inside it.
pub(super) fn walk_body<V: NodeVisitor>(body: &Block, visitor: &mut V) {
    walk_block(body, visitor);
}

fn walk_block<V: NodeVisitor>(block: &Block, visitor: &mut V) {
    visitor.visit_block(block);
    for (stmt, span) in &block.stmts {
        walk_stmt(stmt, span, visitor);
    }
    if let Some(trailing) = &block.trailing_expr {
        walk_expr(&trailing.0, &trailing.1, visitor);
    }
}

#[allow(
    clippy::too_many_lines,
    clippy::match_same_arms,
    reason = "exhaustive statement visitor enumerates every Stmt shape so a new node forces a decision; per-variant arms are kept even when two walks coincide"
)]
fn walk_stmt<V: NodeVisitor>(stmt: &Stmt, span: &Span, visitor: &mut V) {
    visitor.visit_stmt(stmt, span);
    match stmt {
        Stmt::Let {
            value, else_block, ..
        } => {
            if let Some(v) = value {
                walk_expr(&v.0, &v.1, visitor);
            }
            if let Some(eb) = else_block {
                walk_block(eb, visitor);
            }
        }
        Stmt::Var { value, .. } => {
            if let Some(v) = value {
                walk_expr(&v.0, &v.1, visitor);
            }
        }
        Stmt::Assign { target, value, .. } => {
            walk_expr(&target.0, &target.1, visitor);
            walk_expr(&value.0, &value.1, visitor);
        }
        Stmt::If {
            condition,
            then_block,
            else_block,
        } => {
            walk_expr(&condition.0, &condition.1, visitor);
            walk_block(then_block, visitor);
            if let Some(eb) = else_block {
                walk_else_block(eb, visitor);
            }
        }
        Stmt::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            walk_expr(&expr.0, &expr.1, visitor);
            walk_block(body, visitor);
            if let Some(eb) = else_body {
                walk_block(eb, visitor);
            }
        }
        Stmt::Match { scrutinee, arms } => {
            walk_expr(&scrutinee.0, &scrutinee.1, visitor);
            for arm in arms {
                walk_arm(arm, visitor);
            }
        }
        Stmt::Loop { body, .. } => walk_block(body, visitor),
        Stmt::For { iterable, body, .. } => {
            walk_expr(&iterable.0, &iterable.1, visitor);
            walk_block(body, visitor);
        }
        Stmt::While {
            condition, body, ..
        } => {
            walk_expr(&condition.0, &condition.1, visitor);
            walk_block(body, visitor);
        }
        Stmt::WhileLet { expr, body, .. } => {
            walk_expr(&expr.0, &expr.1, visitor);
            walk_block(body, visitor);
        }
        Stmt::Break { value, .. } | Stmt::Return(value) => {
            if let Some(v) = value {
                walk_expr(&v.0, &v.1, visitor);
            }
        }
        Stmt::Continue { .. } => {}
        Stmt::Defer(expr) => walk_expr(&expr.0, &expr.1, visitor),
        Stmt::Expression(expr) => walk_expr(&expr.0, &expr.1, visitor),
    }
}

fn walk_else_block<V: NodeVisitor>(else_block: &ElseBlock, visitor: &mut V) {
    if let Some(if_stmt) = &else_block.if_stmt {
        walk_stmt(&if_stmt.0, &if_stmt.1, visitor);
    }
    if let Some(block) = &else_block.block {
        walk_block(block, visitor);
    }
}

fn walk_arm<V: NodeVisitor>(arm: &MatchArm, visitor: &mut V) {
    if let Some(guard) = &arm.guard {
        walk_expr(&guard.0, &guard.1, visitor);
    }
    walk_expr(&arm.body.0, &arm.body.1, visitor);
}

fn walk_select_arm<V: NodeVisitor>(arm: &SelectArm, visitor: &mut V) {
    walk_expr(&arm.source.0, &arm.source.1, visitor);
    walk_expr(&arm.body.0, &arm.body.1, visitor);
}

fn walk_call_args<V: NodeVisitor>(args: &[CallArg], visitor: &mut V) {
    for arg in args {
        let (expr, span) = arg.expr();
        walk_expr(expr, span, visitor);
    }
}

#[allow(
    clippy::too_many_lines,
    clippy::match_same_arms,
    reason = "exhaustive expression visitor enumerates every Expr shape so a new node forces a decision; per-variant arms are kept even when two walks coincide"
)]
fn walk_expr<V: NodeVisitor>(expr: &Expr, span: &Span, visitor: &mut V) {
    visitor.visit_expr(expr, span);
    match expr {
        Expr::Literal(_)
        | Expr::Identifier(_)
        | Expr::This
        | Expr::RegexLiteral(_)
        | Expr::ByteStringLiteral(_)
        | Expr::ByteArrayLiteral(_) => {}
        Expr::Binary { left, right, .. } => {
            walk_expr(&left.0, &left.1, visitor);
            walk_expr(&right.0, &right.1, visitor);
        }
        Expr::Unary { operand, .. } => walk_expr(&operand.0, &operand.1, visitor),
        Expr::Clone(inner)
        | Expr::Await(inner)
        | Expr::AwaitRestart(inner)
        | Expr::PostfixTry(inner)
        | Expr::Cast { expr: inner, .. }
        | Expr::FieldAccess { object: inner, .. } => walk_expr(&inner.0, &inner.1, visitor),
        Expr::Tuple(items) | Expr::Array(items) | Expr::Join(items) => {
            for item in items {
                walk_expr(&item.0, &item.1, visitor);
            }
        }
        Expr::ArrayRepeat { value, count } => {
            walk_expr(&value.0, &value.1, visitor);
            walk_expr(&count.0, &count.1, visitor);
        }
        Expr::MapLiteral { entries } => {
            for (k, v) in entries {
                walk_expr(&k.0, &k.1, visitor);
                walk_expr(&v.0, &v.1, visitor);
            }
        }
        Expr::Block(block)
        | Expr::Scope { body: block }
        | Expr::ForkBlock { body: block }
        | Expr::GenBlock { body: block } => walk_block(block, visitor),
        Expr::UnsafeBlock(block) => walk_block(block, visitor),
        Expr::If {
            condition,
            then_block,
            else_block,
        } => {
            walk_expr(&condition.0, &condition.1, visitor);
            walk_expr(&then_block.0, &then_block.1, visitor);
            if let Some(eb) = else_block {
                walk_expr(&eb.0, &eb.1, visitor);
            }
        }
        Expr::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            walk_expr(&expr.0, &expr.1, visitor);
            walk_block(body, visitor);
            if let Some(eb) = else_body {
                walk_block(eb, visitor);
            }
        }
        Expr::Match { scrutinee, arms } => {
            walk_expr(&scrutinee.0, &scrutinee.1, visitor);
            for arm in arms {
                walk_arm(arm, visitor);
            }
        }
        Expr::Lambda { body, .. } | Expr::SpawnLambdaActor { body, .. } => {
            walk_expr(&body.0, &body.1, visitor);
        }
        Expr::Spawn { target, args, .. } => {
            walk_expr(&target.0, &target.1, visitor);
            for (_, value) in args {
                walk_expr(&value.0, &value.1, visitor);
            }
        }
        Expr::ForkChild { expr, .. } => walk_expr(&expr.0, &expr.1, visitor),
        Expr::ScopeDeadline { duration, body } => {
            walk_expr(&duration.0, &duration.1, visitor);
            walk_block(body, visitor);
        }
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let StringPart::Expr(inner) = part {
                    walk_expr(&inner.0, &inner.1, visitor);
                }
            }
        }
        Expr::Call { function, args, .. } => {
            walk_expr(&function.0, &function.1, visitor);
            walk_call_args(args, visitor);
        }
        Expr::MethodCall { receiver, args, .. } => {
            walk_expr(&receiver.0, &receiver.1, visitor);
            walk_call_args(args, visitor);
        }
        Expr::StructInit { fields, base, .. } => {
            for (_, value) in fields {
                walk_expr(&value.0, &value.1, visitor);
            }
            if let Some(base) = base {
                walk_expr(&base.0, &base.1, visitor);
            }
        }
        Expr::Select { arms, timeout } => {
            for arm in arms {
                walk_select_arm(arm, visitor);
            }
            if let Some(timeout) = timeout {
                walk_expr(&timeout.duration.0, &timeout.duration.1, visitor);
                walk_expr(&timeout.body.0, &timeout.body.1, visitor);
            }
        }
        Expr::Timeout { expr, duration } => {
            walk_expr(&expr.0, &expr.1, visitor);
            walk_expr(&duration.0, &duration.1, visitor);
        }
        Expr::Yield(value) | Expr::Return(value) => {
            if let Some(v) = value {
                walk_expr(&v.0, &v.1, visitor);
            }
        }
        Expr::Index { object, index } => {
            walk_expr(&object.0, &object.1, visitor);
            walk_expr(&index.0, &index.1, visitor);
        }
        Expr::Range { start, end, .. } => {
            if let Some(start) = start {
                walk_expr(&start.0, &start.1, visitor);
            }
            if let Some(end) = end {
                walk_expr(&end.0, &end.1, visitor);
            }
        }
        Expr::Is { lhs, rhs } => {
            walk_expr(&lhs.0, &lhs.1, visitor);
            walk_expr(&rhs.0, &rhs.1, visitor);
        }
        Expr::MachineEmit { fields, .. } => {
            for (_, value) in fields {
                walk_expr(&value.0, &value.1, visitor);
            }
        }
    }
}
