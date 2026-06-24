//! The `needless_match_to_if_let` lint.
//!
//! Flags a two-arm `match` on an `Option` that is really an `if let`: one arm
//! binds `Some(x)` and does the work, the other is an empty `None => {}` (or
//! `None => ()`). Such a `match` reads as `if let Some(x) = scrutinee { … }`.
//!
//! ## Precision over recall
//!
//! - Exactly two arms, neither carrying a guard.
//! - The scrutinee's resolved type must be `Option<_>` (checked through
//!   [`LintCtx::resolved_type_at`]); `Result` is intentionally out of scope.
//! - One arm is `Some(<binding>)` where `<binding>` is a plain identifier or
//!   `_` (a nested destructure is left alone — the rewrite is less obviously an
//!   improvement and rendering it is error-prone).
//! - The other arm is `None` with a trivial body: an empty block `{}` or unit
//!   `()` (recursing through a single trivial trailing expression). A `None`
//!   arm that does any work disqualifies the match — both arms doing work is
//!   not an `if let`.
//! - Only fires where the `match` value is discarded: statement position
//!   (`Stmt::Match` / a bare `match` expression statement) or block-tail
//!   position. A `match` feeding a `let` / argument (value position) is left
//!   alone, matching the rule that the binding arm's value must not be consumed
//!   as the match's result.

use hew_parser::ast::{Block, Expr, MatchArm, Pattern, Span, Stmt};

use crate::error::TypeError;
use crate::ty::Ty;

use super::{LintCtx, LintId, LintLevels, NodeVisitor};
use crate::builtin_type::BuiltinType;

/// Entry point: walk `body` and flag every two-arm Option match that is an
/// `if let` in disguise.
pub(super) fn check(ctx: &LintCtx, levels: &LintLevels, body: &Block, out: &mut Vec<TypeError>) {
    let mut visitor = NeedlessMatch {
        ctx,
        hits: Vec::new(),
    };
    super::walk_body(body, &mut visitor);
    for (span, binding) in visitor.hits {
        ctx.emit(
            levels,
            LintId::NeedlessMatchToIfLet,
            &span,
            "this two-arm `match` on `Option` is an `if let`".to_string(),
            format!("rewrite as `if let Some({binding}) = … {{ … }}`"),
            out,
        );
    }
}

struct NeedlessMatch<'a> {
    ctx: &'a LintCtx<'a>,
    hits: Vec<(Span, String)>,
}

impl NeedlessMatch<'_> {
    fn consider(&mut self, scrutinee_span: &Span, arms: &[MatchArm], report_span: &Span) {
        if let Some(binding) = needless_binding(self.ctx, scrutinee_span, arms) {
            self.hits.push((report_span.clone(), binding));
        }
    }
}

impl NodeVisitor for NeedlessMatch<'_> {
    fn visit_block(&mut self, block: &Block) {
        for (stmt, span) in &block.stmts {
            match stmt {
                Stmt::Match { scrutinee, arms } => self.consider(&scrutinee.1, arms, span),
                Stmt::Expression((Expr::Match { scrutinee, arms }, expr_span)) => {
                    self.consider(&scrutinee.1, arms, expr_span);
                }
                _ => {}
            }
        }
        if let Some(trailing) = &block.trailing_expr {
            if let Expr::Match { scrutinee, arms } = &trailing.0 {
                self.consider(&scrutinee.1, arms, &trailing.1);
            }
        }
    }
}

/// When `arms` (the arms of a `match` on the expression at `scrutinee_span`)
/// form the `Some(binding) => …` / `None => {}` shape on an `Option`, return
/// the binding's spelling for the suggestion. Returns `None` otherwise.
fn needless_binding(ctx: &LintCtx, scrutinee_span: &Span, arms: &[MatchArm]) -> Option<String> {
    if arms.len() != 2 || arms.iter().any(|arm| arm.guard.is_some()) {
        return None;
    }
    if !ctx
        .resolved_type_at(scrutinee_span)
        .is_some_and(|ty| is_option(&ty))
    {
        return None;
    }

    let mut some_binding: Option<String> = None;
    let mut none_trivial = false;
    for arm in arms {
        match &arm.pattern.0 {
            Pattern::Constructor { name, patterns } if name == "Some" && patterns.len() == 1 => {
                some_binding = Some(binding_name(&patterns[0].0)?);
            }
            pattern if is_none_pattern(pattern) => {
                if !is_trivial_body(&arm.body.0) {
                    return None;
                }
                none_trivial = true;
            }
            _ => return None,
        }
    }
    if none_trivial {
        some_binding
    } else {
        None
    }
}

fn is_option(ty: &Ty) -> bool {
    matches!(
        ty,
        Ty::Named {
            builtin: Some(BuiltinType::Option),
            ..
        }
    )
}

/// Whether `pattern` denotes the `Option::None` variant. A bare `None` (no
/// parens) parses as an *identifier* pattern, not a constructor; accept that
/// form (in this Option-typed context an identifier spelled exactly `None` is
/// the variant, never a fresh binding) as well as the rare zero-arg
/// constructor spelling.
fn is_none_pattern(pattern: &Pattern) -> bool {
    match pattern {
        Pattern::Identifier(name) => name == "None",
        Pattern::Constructor { name, patterns } => name == "None" && patterns.is_empty(),
        _ => false,
    }
}

/// The spelling to show for a `Some(_)` sub-pattern, restricted to the shapes
/// that render cleanly: a plain identifier or `_`.
fn binding_name(pattern: &Pattern) -> Option<String> {
    match pattern {
        Pattern::Identifier(name) => Some(name.clone()),
        Pattern::Wildcard => Some("_".to_string()),
        _ => None,
    }
}

/// Whether `expr` is a side-effect-free unit body: an empty block `{}`, a unit
/// literal `()`, or a block whose only content is one trivial trailing
/// expression (e.g. `{ () }`).
fn is_trivial_body(expr: &Expr) -> bool {
    match expr {
        Expr::Tuple(items) => items.is_empty(),
        Expr::Block(block) => {
            block.stmts.is_empty()
                && block
                    .trailing_expr
                    .as_ref()
                    .is_none_or(|trailing| is_trivial_body(&trailing.0))
        }
        _ => false,
    }
}
