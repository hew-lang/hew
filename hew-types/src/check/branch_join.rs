//! Joining move/release state across alternative execution paths.
//!
//! The ownership rule the checker enforces is one consume per **path**, not one
//! consume per function body. Constructs that fan out into mutually exclusive
//! arms — `match`, `if`/`else`, `if let`, `select` — must therefore start each
//! arm from the state at the branch's entry and take the union of the arms that
//! reach the join, rather than threading one arm's exit state into the next.
//!
//! Two rules make that sound:
//!
//! * **Union, not intersection.** A binding consumed on any path is unusable
//!   after the join, so a conditional consume followed by a use still rejects.
//!   The false positive this replaces never came from the merge direction; it
//!   came from an arm starting where its sibling ended.
//! * **Join only reaching arms.** Return, break and continue paths leave the
//!   immediate branch join. Loop edges retain their ownership snapshots in the
//!   environment and participate in the enclosing loop's join, so a later
//!   use cannot lose a consume on an early exit.
//!
//! # Which operands are sequential and which are branched
//!
//! An operand may be branch-scoped only if the branch is already decided by the
//! time it runs. Everything else threads sequentially.
//!
//! | construct | sequential setup | branched |
//! |---|---|---|
//! | `match` | scrutinee; each arm's pattern test and guard, threaded through the fall-through state | arm bodies |
//! | `if` / `else if` | each link's condition, threaded through the fall-through state | the block after each condition |
//! | `if let` | scrutinee | then-block, else-block |
//! | `let … else` | the bound value | the else block |
//! | `select` | EVERY arm's source and the timeout duration — all prepared before a winner exists | arm bodies and the timeout body |
//!
//! Two consequences worth stating outright, because both were wrong once. A
//! `select` source runs before dispatch picks a winner, so the sources are not
//! alternatives and the same handle handed to two of them is a real double
//! transfer. A guard runs only after every earlier arm failed to match, so it
//! belongs to the fall-through chain rather than restarting from the branch
//! entry — otherwise a guard's consume and a later arm's consume look disjoint
//! when they happen on one execution.
//!
//! Loops use the same ownership snapshots to join body completion, early loop
//! edges and the possible zero-iteration path. Reinitialization can remove a
//! moved-place fact, so the body's final state alone does not describe those
//! paths. The source checker walks the body once. The body's end and every
//! `continue` are back edges: a use the next iteration makes before
//! re-initialising a value is checked against the state those edges carry
//! (`TypeEnv::exit_loop`), so a value consumed on every iteration is refused
//! here rather than at SIR verification.
//!
//! Fork and spawn children are not join sites either: they run concurrently, not
//! alternatively, so a binding moved into one child and used in another is a
//! real error and the sequential threading is the correct semantics.

use super::types::Checker;
use crate::env::OwnershipSnapshot;
use crate::ty::Ty;
use hew_parser::ast::{Block, Expr, Span, Spanned, Stmt};

/// What one branch arm left behind.
pub(super) struct BranchArmExit {
    /// Ownership state at the end of the arm.
    pub(super) ownership: OwnershipSnapshot,
    /// Whether control leaves this arm without reaching the immediate join.
    /// Loop exits have already recorded their ownership state in the env.
    pub(super) diverges: bool,
}

impl Checker {
    /// The checked arm's terminator decides whether it reaches this join.
    pub(super) fn arm_skips_join(arm_ty: &Ty) -> bool {
        matches!(arm_ty, Ty::Never)
    }

    /// Merge the arms of an alternative-execution construct back into one state.
    ///
    /// `entry` is the snapshot taken immediately before the first arm. Callers
    /// pass one [`BranchArmExit`] per arm, **including** the implicit
    /// fall-through arm of a branch with no `else` — for that arm the exit is
    /// the state the last condition left behind, which is what runs when no
    /// written arm is taken.
    pub(super) fn join_branch_ownership(
        &mut self,
        entry: &OwnershipSnapshot,
        arms: &[BranchArmExit],
        span: &Span,
    ) {
        let mut reaching: Vec<OwnershipSnapshot> = arms
            .iter()
            .filter(|arm| !arm.diverges)
            .map(|arm| arm.ownership.clone())
            .collect();
        let has_reaching_arms = !reaching.is_empty();
        if reaching.is_empty() {
            // Every arm diverges, so nothing after the join is reachable. Union
            // over all of them: it costs nothing and keeps the state defined for
            // any diagnostic that still walks the unreachable tail.
            reaching = arms.iter().map(|arm| arm.ownership.clone()).collect();
        }
        let conflicts = self.env.merge_ownership(entry, &reaching);
        if has_reaching_arms {
            self.report_deferred_init_conflicts(&conflicts);
            self.report_local_init_conflicts(&conflicts, span);
        }
    }

    /// Join a two-armed branch whose second arm has just finished checking.
    ///
    /// `taken` is the already-captured exit of the first arm; the second arm's
    /// exit is read from the environment. `other_skips_join` must come from one
    /// of the `arm_skips_join_*` classifiers, never from the arm's type alone.
    pub(super) fn join_two_way(
        &mut self,
        entry: &OwnershipSnapshot,
        taken: BranchArmExit,
        other_skips_join: bool,
        span: &Span,
    ) {
        let other = BranchArmExit {
            ownership: self.env.ownership_snapshot(),
            diverges: other_skips_join,
        };
        self.join_branch_ownership(entry, &[taken, other], span);
    }

    /// Join a one-armed branch — an `if` or `if let` with no `else` — against
    /// its implicit fall-through path.
    ///
    /// The fall-through arm consumes nothing, so its exit is the branch entry
    /// itself. Keeping it in the union is what preserves the rejection for a
    /// conditional consume followed by an unconditional use.
    pub(super) fn join_fall_through(
        &mut self,
        entry: &OwnershipSnapshot,
        taken: BranchArmExit,
        span: &Span,
    ) {
        let fall_through = BranchArmExit {
            ownership: entry.clone(),
            diverges: false,
        };
        self.join_branch_ownership(entry, &[taken, fall_through], span);
    }
}

/// How a branch's value position stands toward its join's type (A448).
///
/// A contextual variant (`.None`, `.Ok(v)`) has no type of its own; it takes
/// the type the context expects. At a join with no expectation, the typed
/// siblings supply it: their types are joined first and the contextual
/// branches are then checked against the result. A join whose every reaching
/// branch is contextual still has no type, and reports so.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BranchValue {
    /// Needs the join's type from elsewhere.
    Contextual,
    /// Leaves the join (`return`, `break`, `continue`).
    Diverges,
    /// Has a type of its own.
    Typed,
}

fn expr_value(expr: &Expr) -> BranchValue {
    match expr {
        Expr::ContextVariant(_) => BranchValue::Contextual,
        Expr::Call { function, .. } if matches!(function.0, Expr::ContextVariant(_)) => {
            BranchValue::Contextual
        }
        Expr::Return(_) | Expr::ReturnError(_) => BranchValue::Diverges,
        Expr::Block(block) => block_value(block),
        Expr::If {
            then_block,
            else_block: Some(else_block),
            ..
        } => join_value([expr_value(&then_block.0), expr_value(&else_block.0)]),
        Expr::IfLet {
            body,
            else_body: Some(else_body),
            ..
        } => join_value([block_value(body), expr_value(&else_body.0)]),
        Expr::Match { arms, .. } => join_value(arms.iter().map(|arm| expr_value(&arm.body.0))),
        _ => BranchValue::Typed,
    }
}

fn block_value(block: &Block) -> BranchValue {
    if let Some(tail) = &block.trailing_expr {
        return expr_value(&tail.0);
    }
    block
        .stmts
        .last()
        .map_or(BranchValue::Typed, |(stmt, _)| stmt_value(stmt))
}

fn stmt_value(stmt: &Stmt) -> BranchValue {
    match stmt {
        Stmt::Return(_) | Stmt::Break { .. } | Stmt::Continue { .. } => BranchValue::Diverges,
        Stmt::If {
            then_block,
            else_block: Some(else_block),
            ..
        } => {
            let else_value = match (&else_block.if_stmt, &else_block.block) {
                (Some(if_stmt), _) => stmt_value(&if_stmt.0),
                (None, Some(block)) => block_value(block),
                (None, None) => BranchValue::Typed,
            };
            join_value([block_value(then_block), else_value])
        }
        Stmt::IfLet {
            body,
            else_body: Some(else_body),
            ..
        } => join_value([block_value(body), expr_value(&else_body.0)]),
        Stmt::Match { arms, .. } => join_value(arms.iter().map(|arm| expr_value(&arm.body.0))),
        _ => BranchValue::Typed,
    }
}

/// A nested join is contextual when no reaching branch has a type of its own.
fn join_value(branches: impl IntoIterator<Item = BranchValue>) -> BranchValue {
    let mut value = BranchValue::Diverges;
    for branch in branches {
        match branch {
            BranchValue::Typed => return BranchValue::Typed,
            BranchValue::Contextual => value = BranchValue::Contextual,
            BranchValue::Diverges => {}
        }
    }
    value
}

/// Whether this branch value waits for its typed siblings.
pub(super) fn expr_needs_context(expr: &Expr) -> bool {
    expr_value(expr) == BranchValue::Contextual
}

pub(super) fn block_needs_context(block: &Block) -> bool {
    block_value(block) == BranchValue::Contextual
}

pub(super) fn stmt_needs_context(stmt: &Stmt) -> bool {
    stmt_value(stmt) == BranchValue::Contextual
}

/// The value position of one branch of a join.
#[derive(Clone, Copy)]
pub(super) enum BranchBody<'a> {
    Expr(&'a Spanned<Expr>),
    Block(&'a Block),
    /// An `else if` link.
    Stmt(&'a Spanned<Stmt>),
}

impl BranchBody<'_> {
    fn needs_context(self) -> bool {
        match self {
            Self::Expr(expr) => expr_needs_context(&expr.0),
            Self::Block(block) => block_needs_context(block),
            Self::Stmt(stmt) => stmt_needs_context(&stmt.0),
        }
    }
}

impl Checker {
    /// The join type an expectation fixes, if it fixes one: an unresolved
    /// variable leaves the join to its branches.
    pub(super) fn concrete_join_expectation(&self, expected: Option<&Ty>) -> Option<Ty> {
        let resolved = self.subst.resolve(expected?);
        (!matches!(resolved, Ty::Var(_) | Ty::Error)).then_some(resolved)
    }

    /// The type a contextual branch takes from a checked sibling, when the
    /// sibling reaches the join with one.
    fn sibling_type(sibling: &Ty) -> Option<Ty> {
        (!matches!(sibling, Ty::Never | Ty::Error)).then(|| sibling.clone())
    }

    fn check_branch_body(&mut self, body: BranchBody<'_>, expected: Option<&Ty>) -> Ty {
        match (body, expected) {
            (BranchBody::Expr(expr), Some(expected)) => {
                self.check_expr_with_expected(&expr.0, &expr.1, expected)
            }
            (BranchBody::Expr(expr), None) => self.synthesize(&expr.0, &expr.1),
            (BranchBody::Block(block), expected) => self.check_block(block, expected),
            (BranchBody::Stmt(stmt), expected) => {
                self.check_stmt_as_expr(&stmt.0, &stmt.1, expected)
            }
        }
    }

    fn checked_branch(
        &mut self,
        body: BranchBody<'_>,
        expected: Option<&Ty>,
    ) -> (Ty, BranchArmExit) {
        let ty = self.check_branch_body(body, expected);
        let exit = BranchArmExit {
            ownership: self.env.ownership_snapshot(),
            diverges: Self::arm_skips_join(&ty),
        };
        (ty, exit)
    }

    /// Check both arms of a two-way value join (`if`/`else`, `if let`/`else`).
    ///
    /// The first arm starts from the current state, the second from `entry`.
    /// When `first_scoped`, the innermost scope (an `if let`'s bindings)
    /// belongs to the first arm and closes after it. An arm whose value is a
    /// contextual variant takes its sibling's type when nothing else fixes the
    /// join's, so the typed arm is checked first whatever its position.
    pub(super) fn check_two_way_join(
        &mut self,
        entry: &OwnershipSnapshot,
        first: BranchBody<'_>,
        first_scoped: bool,
        second: BranchBody<'_>,
        expected: Option<&Ty>,
    ) -> [(Ty, BranchArmExit); 2] {
        let open = self.concrete_join_expectation(expected).is_none();
        if open && first.needs_context() && !second.needs_context() {
            let first_start = self.env.ownership_snapshot();
            let scope = first_scoped.then(|| self.env.suspend_scope());
            self.env.restore_ownership(entry);
            let (second_ty, second_exit) = self.checked_branch(second, expected);
            if let Some(scope) = scope {
                self.env.resume_scope(scope);
            }
            self.env.restore_ownership(&first_start);
            let sibling = Self::sibling_type(&second_ty);
            let first_checked = self.checked_branch(first, sibling.as_ref().or(expected));
            if first_scoped {
                self.env.pop_scope();
            }
            return [first_checked, (second_ty, second_exit)];
        }
        let (first_ty, first_exit) = self.checked_branch(first, expected);
        if first_scoped {
            self.env.pop_scope();
        }
        self.env.restore_ownership(entry);
        let sibling = (open && second.needs_context())
            .then(|| Self::sibling_type(&first_ty))
            .flatten();
        let second_checked = self.checked_branch(second, sibling.as_ref().or(expected));
        [(first_ty, first_exit), second_checked]
    }
}
