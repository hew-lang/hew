//! Transaction rollback for a failed field-load lifecycle classification.

use super::{BasicBlock, Builder, FinalizedBody, Terminator};

impl Builder {
    /// Discard an ownership transaction after a field-load classifier failure.
    ///
    /// The callable body may have accumulated arbitrary partial ownership
    /// facts before the failure. Keeping any of them would make later drop or
    /// transfer elaboration observe a half-built authority graph, so the only
    /// valid body is an empty `Unreachable` block carrying the diagnostic.
    pub(super) fn take_field_load_classification_poisoned_body(&mut self) -> FinalizedBody {
        self.pending_blocks.clear();
        self.statements.clear();
        self.instructions.clear();
        self.pending_outbound_actor_args.clear();
        self.pending_owned_call_args.clear();
        self.pending_affine_call_consumes.clear();
        self.deferred_affine_call_consume_sites.clear();
        FinalizedBody {
            blocks: vec![BasicBlock {
                id: 0,
                statements: Vec::new(),
                instructions: Vec::new(),
                terminator: Terminator::Unreachable,
            }],
            body_statements: Vec::new(),
        }
    }
}
