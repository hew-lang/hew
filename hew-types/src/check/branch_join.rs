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
//! * **Divergence exclusion.** An arm that ends in `Ty::Never` never reaches the
//!   join, so its consumes cannot be observed by the code below it. An arm that
//!   diverges by leaving the enclosing loop rather than the function does rejoin
//!   later, and that edge belongs to the checked-MIR dataflow pass, which is
//!   flow-sensitive over the whole CFG.
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

/// What one branch arm left behind.
pub(super) struct BranchArmExit {
    /// Ownership state at the end of the arm.
    pub(super) ownership: OwnershipSnapshot,
    /// Whether the arm ends without reaching the join (`Ty::Never`).
    pub(super) diverges: bool,
}

impl Checker {
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
}
