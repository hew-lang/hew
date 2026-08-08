use super::{MirCheck, MirDiagnostic, MirDiagnosticKind};

/// Project a Checked MIR finding to a `MirDiagnostic` for the CLI
/// rejection surface. `CheckedMirFunction::checks` is the single
/// source of truth for move/borrow/init legality; this function
/// adapts those findings to the older `MirDiagnostic` channel the
/// driver already consumes. Variants whose construction surface
/// isn't yet wired (`Aliasing`, `GeneratorBorrowAcrossYield`,
/// `ActorSendEscape`) cannot appear today; they yield `None` defensively.
#[allow(
    clippy::too_many_lines,
    reason = "one exhaustive MirCheck -> MirDiagnostic projection; each arm is a \
              single distinct mapping and splitting scatters the projection table"
)]
pub(in crate::lower) fn check_to_diagnostic(check: &MirCheck) -> Option<MirDiagnostic> {
    match check {
        MirCheck::UseAfterConsume {
            binding,
            name,
            consumed_at,
            used_at,
        } => Some(MirDiagnostic {
            kind: MirDiagnosticKind::UseAfterConsume {
                binding: *binding,
                name: name.clone(),
                consumed_at: *consumed_at,
                used_at: *used_at,
            },
            note: "binding is used after an owned value move in checked MIR".to_string(),
        }),
        MirCheck::InitialisedBeforeUse {
            binding,
            name,
            use_site,
        } => Some(MirDiagnostic {
            kind: MirDiagnosticKind::InitialisedBeforeUse {
                binding: *binding,
                name: name.clone(),
                use_site: *use_site,
            },
            note: "binding is read before any initialising `let` for it appears".to_string(),
        }),
        MirCheck::DecisionMapTotal { offending_sites } => Some(MirDiagnostic {
            kind: MirDiagnosticKind::DecisionMapTotal {
                offending_sites: offending_sites.clone(),
            },
            note: "DecisionFact carries Strategy::UnknownBlocked at MIR boundary; \
                   the emitter must never receive an undecided value-class site"
                .to_string(),
        }),
        MirCheck::OutboundModeUnresolved { block } => Some(MirDiagnostic {
            kind: MirDiagnosticKind::OutboundModeUnresolved { block: *block },
            note: "every non-unit send/ask payload must carry resolved per-argument modes before checked MIR"
                .to_string(),
        }),
        MirCheck::MustConsume {
            binding,
            name,
            bind_site,
            exit_site,
            ty,
        } => Some(MirDiagnostic {
            kind: MirDiagnosticKind::MustConsume {
                binding: *binding,
                name: name.clone(),
                bind_site: *bind_site,
                exit_site: *exit_site,
                ty: ty.clone(),
            },
            note: "@linear binding reached an exit without being consumed; \
                   declare a consuming method (e.g. `commit(consuming self)`) \
                   and ensure every reachable exit path invokes one"
                .to_string(),
        }),
        MirCheck::DropPlanUndetermined { block, reason } => Some(MirDiagnostic {
            kind: MirDiagnosticKind::DropPlanUndetermined {
                block: *block,
                reason: reason.clone(),
            },
            note: "drop-elaboration could not determine the per-exit live-set \
                   for an M2 substrate handle (Duplex / lambda-actor / \
                   half-handle); the elaborator aborts rather than emit a \
                   partial drop plan (LESSONS cleanup-all-exits)"
                .to_string(),
        }),
        MirCheck::ObligationUnderReleased {
            function,
            block,
            name,
            hard,
            reason,
            ..
        } => Some(MirDiagnostic {
            kind: MirDiagnosticKind::ObligationUnderReleased {
                function: function.clone(),
                block: *block,
                name: name.clone(),
                hard: *hard,
                reason: reason.clone(),
            },
            note: if *hard {
                "an explicit MIR retain minted an independently owned reference, but this exit \
                 never releases it. Retain-backed mint/release mismatches are compiler invariant \
                 failures and cannot be downgraded"
                    .to_string()
            } else {
                "every heap-owning owned value must be released exactly once on every reachable \
                 exit path; this exit path never discharges the mint (leak). This is an advisory \
                 warning, not a build error — fix the drop plan (release on every exit) to silence it"
                    .to_string()
            },
        }),
        MirCheck::ObligationOverReleased {
            function,
            block,
            name,
            reason,
        } => Some(MirDiagnostic {
            kind: MirDiagnosticKind::ObligationOverReleased {
                function: function.clone(),
                block: *block,
                name: name.clone(),
                reason: reason.clone(),
            },
            note: "every heap-owning owned value must be released exactly once \
                   on every reachable exit path; this path releases it two or \
                   more times (double-free). Over-release is memory-unsafe and \
                   carries no allowlist escape"
                .to_string(),
        }),
        MirCheck::ObligationBalanceUnverified { function, reason } => Some(MirDiagnostic {
            kind: MirDiagnosticKind::ObligationBalanceUnverified {
                function: function.clone(),
                reason: reason.clone(),
            },
            note: "the obligation-balance fixpoint could not reach a verdict for \
                   this function; the gate fails closed rather than certify an \
                   unverified body as leak- and double-free-free. This is a \
                   modelling invariant, not a user error"
                .to_string(),
        }),
        MirCheck::ContextBoundaryViolation {
            function,
            block,
            kind,
            reason,
        } => Some(MirDiagnostic {
            kind: MirDiagnosticKind::ContextBoundaryViolation {
                function: function.clone(),
                block: *block,
                kind,
                reason: reason.clone(),
            },
            note: "actor-handler execution context markers are structurally invalid".to_string(),
        }),
        MirCheck::ContextBindingEscapes { place, block } => Some(MirDiagnostic {
            kind: MirDiagnosticKind::ContextBindingEscapes {
                place: *place,
                block: *block,
            },
            note: "context-derived MIR place escapes past ExitContext".to_string(),
        }),
        MirCheck::DischargeAuthorityMissing {
            function,
            block,
            authority,
            reason,
        } => Some(MirDiagnostic {
            kind: MirDiagnosticKind::DischargeAuthorityMissing {
                function: function.clone(),
                block: *block,
                authority: *authority,
                reason: reason.clone(),
            },
            note: "a payload-slot neutralize whose discharge authority takes \
                   ownership into a destination reached elaboration with no \
                   transferee recorded; the discharge fact is defective. This is \
                   a lowering invariant (close-by-construction), not a user error"
                .to_string(),
        }),
        MirCheck::DischargeAuthorityDrift {
            function,
            block,
            name,
            reason,
        } => Some(MirDiagnostic {
            kind: MirDiagnosticKind::DischargeAuthorityDrift {
                function: function.clone(),
                block: *block,
                name: name.clone(),
                reason: reason.clone(),
            },
            note: "a carried discharge authority disagrees with the \
                   independently re-derived discharge set (dual-carrier drift); \
                   the two carriers of one ownership-transfer fact must agree. \
                   This is a lowering invariant, not a user error"
                .to_string(),
        }),
        MirCheck::OwnedHandleAggregateDoubleFree {
            name,
            handle_ty,
            overwrite,
            owner,
            ..
        } => Some(MirDiagnostic {
            kind: MirDiagnosticKind::OwnedHandleAggregateExtractionUnsupported {
                name: name.clone(),
                handle_ty: handle_ty.clone(),
                overwrite: *overwrite,
                owner: *owner,
            },
            note: "the drop analysis could not prove this owned handle is freed \
                   exactly once after aggregate extraction; the compiler refuses \
                   rather than emit a double-free (LESSONS boundary-fail-closed, \
                   raii-null-after-move) — full aggregate-extraction support is \
                   tracked for v0.5.1"
                .to_string(),
        }),
        // No construction surface in the v0.5 integer spine. The
        // corresponding `MirDiagnosticKind` projections will land
        // alongside the construction surface for borrows, generators,
        // and actor sends.
        //
        // `WitnessOperandUnresolved` joins this group for a different
        // reason (W5.007a): witness instructions are produced only into
        // the gated polymorphic-MIR bucket, whose diagnostics are
        // discarded, so the finding never reaches a `CheckedMirFunction`
        // and has no user-visible projection in this slice. Its
        // fail-closed authority lives at the construction boundary
        // (`WitnessOperand::resolve`) and the MIR verifier.
        MirCheck::Aliasing { .. }
        | MirCheck::GeneratorBorrowAcrossYield { .. }
        | MirCheck::ActorSendEscape { .. }
        | MirCheck::ActorAskEscape { .. }
        | MirCheck::WitnessOperandUnresolved { .. } => None,
    }
}
