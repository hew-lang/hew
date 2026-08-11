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
//! * **Function-exit exclusion, not divergence exclusion.** An arm is dropped
//!   from the union only when control leaves the FUNCTION, because only then can
//!   the code below the branch never observe what the arm consumed. `Ty::Never`
//!   alone does not establish that: `break` and `continue` type `Never` too, and
//!   they rejoin right after the enclosing loop. An arm that can escape to an
//!   enclosing loop is treated as reaching the join.
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
//! Loops are deliberately not join sites. The env checker walks a loop body
//! once, and because the ownership flags are monotone within a path, the body's
//! exit state is already the union of "ran" and "did not run". Cross-iteration
//! double consumes are the checked-MIR pass's verdict
//! (`E_MIR_CHECK: UseAfterConsume`); duplicating them here would double-diagnose
//! the same program.
//!
//! Fork and spawn children are not join sites either: they run concurrently, not
//! alternatively, so a binding moved into one child and used in another is a
//! real error and the sequential threading is the correct semantics.

use super::types::Checker;
use crate::env::OwnershipSnapshot;
use crate::ty::Ty;
use hew_parser::ast::{Block, Expr, Stmt};

/// What one branch arm left behind.
pub(super) struct BranchArmExit {
    /// Ownership state at the end of the arm.
    pub(super) ownership: OwnershipSnapshot,
    /// Whether control leaves the enclosing function without ever reaching the
    /// code below this branch.
    ///
    /// This is NOT the same question as `arm_ty == Ty::Never`. A `break` or a
    /// `continue` also types `Never`, but it leaves only the enclosing LOOP and
    /// rejoins immediately after it, so everything the arm consumed is live
    /// again below the loop and its state must be merged. Build this field with
    /// [`Checker::arm_skips_join_expr`] and friends, never from the type alone.
    pub(super) diverges: bool,
}

impl Checker {
    /// Whether an expression-bodied arm leaves the function without reaching the
    /// join below its branch.
    ///
    /// Requires BOTH that the arm diverges and that no `break` or `continue`
    /// inside it can escape to an enclosing loop. The second condition is what
    /// separates a `return` (never comes back) from a `break` (comes back just
    /// past the loop). Getting it wrong in this direction drops a live path from
    /// the union and admits a genuine double consume.
    pub(super) fn arm_skips_join_expr(body: &Expr, arm_ty: &Ty) -> bool {
        matches!(arm_ty, Ty::Never) && !hew_parser::expr_leaves_enclosing_loop(body)
    }

    /// Block-bodied counterpart of [`Self::arm_skips_join_expr`].
    pub(super) fn arm_skips_join_block(body: &Block, arm_ty: &Ty) -> bool {
        matches!(arm_ty, Ty::Never) && !hew_parser::block_leaves_enclosing_loop(body)
    }

    /// Statement-bodied counterpart, for the `else if` link of a chain.
    pub(super) fn arm_skips_join_stmt(body: &Stmt, arm_ty: &Ty) -> bool {
        matches!(arm_ty, Ty::Never) && !hew_parser::stmt_leaves_enclosing_loop(body)
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
    ) {
        let mut reaching: Vec<OwnershipSnapshot> = arms
            .iter()
            .filter(|arm| !arm.diverges)
            .map(|arm| arm.ownership.clone())
            .collect();
        if reaching.is_empty() {
            // Every arm diverges, so nothing after the join is reachable. Union
            // over all of them: it costs nothing and keeps the state defined for
            // any diagnostic that still walks the unreachable tail.
            reaching = arms.iter().map(|arm| arm.ownership.clone()).collect();
        }
        self.env.merge_ownership(entry, &reaching);
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
    ) {
        let other = BranchArmExit {
            ownership: self.env.ownership_snapshot(),
            diverges: other_skips_join,
        };
        self.join_branch_ownership(entry, &[taken, other]);
    }

    /// Join a one-armed branch — an `if` or `if let` with no `else` — against
    /// its implicit fall-through path.
    ///
    /// The fall-through arm consumes nothing, so its exit is the branch entry
    /// itself. Keeping it in the union is what preserves the rejection for a
    /// conditional consume followed by an unconditional use.
    pub(super) fn join_fall_through(&mut self, entry: &OwnershipSnapshot, taken: BranchArmExit) {
        let fall_through = BranchArmExit {
            ownership: entry.clone(),
            diverges: false,
        };
        self.join_branch_ownership(entry, &[taken, fall_through]);
    }
}
