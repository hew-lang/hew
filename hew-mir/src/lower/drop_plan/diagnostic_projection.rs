use super::{MirCheck, MirDiagnostic, MirDiagnosticKind};
use crate::model::ObligationMintProvenance;

fn obligation_under_release_note(provenance: ObligationMintProvenance) -> String {
    match provenance {
        ObligationMintProvenance::Ordinary => "every heap-owning owned value must be released \
             exactly once on every reachable exit path; this exit path never discharges the mint \
             (leak). LLVM emission is blocked until the drop plan releases it on every exit"
            .to_string(),
        ObligationMintProvenance::ExplicitRetain => "an explicit MIR retain minted an \
             independently owned reference, but this exit never releases it. Retain-backed \
             mint/release mismatches are compiler invariant failures and cannot be downgraded"
            .to_string(),
        ObligationMintProvenance::Mixed => "at least one failing exit path carries an explicit \
             MIR retain-backed owner mint that is never released. Retain-backed mint/release \
             mismatches are compiler invariant failures and cannot be downgraded"
            .to_string(),
    }
}

/// Project one Checked MIR finding to a `MirDiagnostic` for the CLI
/// rejection surface. Private on purpose: `project_findings` is the only way
/// out of this module, so no caller can skip the consolidation rules. `CheckedMirFunction::checks` is the single
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
fn check_to_diagnostic(check: &MirCheck) -> Option<MirDiagnostic> {
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
        MirCheck::CollectionCopyUnsupported {
            binding,
            name,
            consumed_at,
            used_at,
        } => Some(MirDiagnostic {
            kind: MirDiagnosticKind::CollectionCopyUnsupported {
                binding: *binding,
                name: name.clone(),
                consumed_at: *consumed_at,
                used_at: *used_at,
            },
            note: "Vec/HashMap/HashSet are retained, not moved, on a whole-binding rebind; \
                   this lowering has no retain path yet"
                .to_string(),
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
            blocks,
            site,
            name,
            local_ty,
            mint_provenance,
            reason,
        } => Some(MirDiagnostic {
            kind: MirDiagnosticKind::ObligationUnderReleased {
                function: function.clone(),
                blocks: blocks.clone(),
                site: *site,
                name: name.clone(),
                local_ty: local_ty.clone(),
                reason: reason.clone(),
            },
            note: obligation_under_release_note(*mint_provenance),
        }),
        MirCheck::ObligationOverReleased {
            function,
            blocks,
            site,
            name,
            reason,
        } => Some(MirDiagnostic {
            kind: MirDiagnosticKind::ObligationOverReleased {
                function: function.clone(),
                blocks: blocks.clone(),
                site: *site,
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
            kind: MirDiagnosticKind::LoweringInvariant {
                function: function.clone(),
                rule: "obligation-balance-unverified".to_owned(),
                block: None,
                detail: reason.clone(),
            },
            note: "the obligation-balance fixpoint could not reach a verdict for \
                   this function; the gate fails closed rather than certify an \
                   unverified body as leak- and double-free-free"
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
            kind: MirDiagnosticKind::LoweringInvariant {
                function: function.clone(),
                rule: "discharge-authority-missing".to_owned(),
                block: Some(*block),
                detail: format!("{reason} (authority {authority:?})"),
            },
            note: "a payload-slot neutralize whose discharge authority takes \
                   ownership into a destination reached elaboration with no \
                   transferee recorded; the discharge fact is defective"
                .to_string(),
        }),
        MirCheck::DischargeAuthorityDrift {
            function,
            block,
            name,
            reason,
        } => Some(MirDiagnostic {
            kind: MirDiagnosticKind::LoweringInvariant {
                function: function.clone(),
                rule: name.clone(),
                block: Some(*block),
                detail: reason.clone(),
            },
            note: "a carried ownership fact disagrees with the fact re-derived \
                   from the Checked MIR event stream; the two carriers of one \
                   ownership fact must agree"
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

/// Which way an owner's obligation balance failed. A leak and a double-free
/// over one owner are opposite claims, so they are never folded together even
/// when they share a mint site.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Imbalance {
    Leak,
    DoubleFree,
}

/// The owner identity an obligation finding is about, or `None` when the
/// finding is not an obligation imbalance.
fn unbalanced_owner_key(check: &MirCheck) -> Option<(Imbalance, &str, hew_hir::SiteId, &str)> {
    match check {
        MirCheck::ObligationUnderReleased {
            function,
            site,
            name,
            ..
        } => Some((Imbalance::Leak, function, *site, name)),
        MirCheck::ObligationOverReleased {
            function,
            site,
            name,
            ..
        } => Some((Imbalance::DoubleFree, function, *site, name)),
        _ => None,
    }
}

/// Fold `source` into `target`: both name the same owner failing the same way,
/// so the user sees one finding carrying every exit block, and — for a leak —
/// the join of the mint provenances that reached those exits.
fn merge_unbalanced_owner(target: &mut MirCheck, source: &MirCheck) {
    let (target_blocks, target_provenance) = match target {
        MirCheck::ObligationUnderReleased {
            blocks,
            mint_provenance,
            ..
        } => (blocks, Some(mint_provenance)),
        MirCheck::ObligationOverReleased { blocks, .. } => (blocks, None),
        _ => return,
    };
    let (source_blocks, source_provenance) = match source {
        MirCheck::ObligationUnderReleased {
            blocks,
            mint_provenance,
            ..
        } => (blocks, Some(*mint_provenance)),
        MirCheck::ObligationOverReleased { blocks, .. } => (blocks, None),
        _ => return,
    };
    for block in source_blocks {
        if !target_blocks.contains(block) {
            target_blocks.push(*block);
        }
    }
    target_blocks.sort_unstable();
    if let (Some(target_provenance), Some(source_provenance)) =
        (target_provenance, source_provenance)
    {
        *target_provenance = target_provenance.join(source_provenance);
    }
}

/// Project one function's verifier findings to the diagnostics a user sees.
///
/// Two consolidation rules apply before `check_to_diagnostic`:
///
/// 1. **One finding per unbalanced owner.** Every validator reports an
///    unbalanced owner per exit it fails on; the user sees the owner once,
///    anchored at its mint `SiteId`, with every unbalanced exit block listed
///    and (for a leak) the mint provenances joined. The key is
///    `(direction, function, site, name)`: an obligation finding is raised
///    only for an owner with a definition-site `Bind`, so the site is the
///    mint and the name is what the source calls the value; two owners that
///    share both are the same value. The leak/double-free direction never
///    merges, because the two say opposite things about the same value.
/// 2. **At most one internal-compiler-error per function.** A finding whose
///    cause is a compiler defect and not the user's program - named by
///    `MirDiagnosticKind::internal_compiler_error_function`, which covers the
///    lowering invariants and both obligation imbalances - is kept only the
///    first time per function; every later one is a consequence of the same
///    inconsistent MIR and repeating them buries the user's own errors. That
///    cap also settles the one case rule 1 leaves standing: a value the
///    compiler both leaks on one exit and double-frees on another produces
///    two findings that must not be merged into one sentence, and the cap
///    reports the first. `HEW_DEBUG_CHECKED_FUNCTION` dumps the unprojected
///    finding list beforehand, so a compiler engineer still sees the ones
///    this rule drops.
pub(in crate::lower) fn project_findings(findings: &[MirCheck]) -> Vec<MirDiagnostic> {
    let _timing = crate::timing::stage("project_findings");
    let mut coalesced: Vec<MirCheck> = Vec::with_capacity(findings.len());
    for finding in findings {
        let Some(key) = unbalanced_owner_key(finding) else {
            coalesced.push(finding.clone());
            continue;
        };
        let prior = coalesced
            .iter()
            .position(|prior| unbalanced_owner_key(prior).as_ref() == Some(&key));
        match prior {
            Some(index) => merge_unbalanced_owner(&mut coalesced[index], finding),
            None => coalesced.push(finding.clone()),
        }
    }

    let mut reported_internal_functions = Vec::<String>::new();
    coalesced
        .iter()
        .filter_map(check_to_diagnostic)
        .filter(|diagnostic| {
            let Some(function) = diagnostic.kind.internal_compiler_error_function() else {
                return true;
            };
            if reported_internal_functions
                .iter()
                .any(|seen| seen == function)
            {
                return false;
            }
            reported_internal_functions.push(function.to_owned());
            true
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::{project_findings, MirCheck, MirDiagnosticKind};
    use crate::model::{NeutralizeAuthority, ObligationMintProvenance};
    use hew_hir::{BindingId, SiteId};

    fn under_released_in(
        function: &str,
        block: u32,
        site: u32,
        provenance: ObligationMintProvenance,
    ) -> MirCheck {
        MirCheck::ObligationUnderReleased {
            function: function.to_owned(),
            blocks: vec![block],
            site: SiteId(site),
            name: "s".to_owned(),
            local_ty: "String".to_owned(),
            mint_provenance: provenance,
            reason: format!("exit bb{block} omits the cleanup"),
        }
    }

    fn under_released(block: u32, site: u32, provenance: ObligationMintProvenance) -> MirCheck {
        under_released_in("f", block, site, provenance)
    }

    fn drift(function: &str, block: u32, rule: &str) -> MirCheck {
        MirCheck::DischargeAuthorityDrift {
            function: function.to_owned(),
            block,
            name: rule.to_owned(),
            reason: format!("{rule} failed in bb{block}"),
        }
    }

    fn use_after_consume() -> MirCheck {
        MirCheck::UseAfterConsume {
            binding: BindingId(1),
            name: "held".to_owned(),
            consumed_at: SiteId(11),
            used_at: SiteId(12),
        }
    }

    fn over_released(block: u32, site: u32) -> MirCheck {
        MirCheck::ObligationOverReleased {
            function: "f".to_owned(),
            blocks: vec![block],
            site: SiteId(site),
            name: "s".to_owned(),
            reason: format!("exit bb{block} releases it twice"),
        }
    }

    #[test]
    fn one_owner_double_freed_on_two_exits_yields_one_diagnostic() {
        let diagnostics = project_findings(&[over_released(4, 3), over_released(1, 3)]);
        let [diagnostic] = diagnostics.as_slice() else {
            panic!("expected one diagnostic for one owner, got {diagnostics:#?}");
        };
        let MirDiagnosticKind::ObligationOverReleased { blocks, site, .. } = &diagnostic.kind
        else {
            panic!("expected ObligationOverReleased, got {diagnostic:#?}");
        };
        assert_eq!(*site, SiteId(3));
        assert_eq!(blocks, &[1, 4]);
    }

    #[test]
    fn a_leak_and_a_double_free_over_one_owner_report_once() {
        let diagnostics = project_findings(&[
            under_released(7, 3, ObligationMintProvenance::Ordinary),
            over_released(7, 3),
        ]);
        let [diagnostic] = diagnostics.as_slice() else {
            panic!("expected one diagnostic for one owner, got {diagnostics:#?}");
        };
        assert!(
            matches!(
                diagnostic.kind,
                MirDiagnosticKind::ObligationUnderReleased { .. }
            ),
            "the first imbalance survives the per-function cap, unmerged with \
             the opposite claim: {diagnostic:#?}"
        );
    }

    #[test]
    fn one_owner_unbalanced_on_three_exits_yields_one_diagnostic() {
        let diagnostics = project_findings(&[
            under_released(7, 3, ObligationMintProvenance::Ordinary),
            under_released(2, 3, ObligationMintProvenance::ExplicitRetain),
            under_released(7, 3, ObligationMintProvenance::Ordinary),
            under_released(9, 3, ObligationMintProvenance::Ordinary),
        ]);
        let [diagnostic] = diagnostics.as_slice() else {
            panic!("expected one diagnostic for one owner, got {diagnostics:#?}");
        };
        let MirDiagnosticKind::ObligationUnderReleased {
            blocks,
            site,
            reason,
            ..
        } = &diagnostic.kind
        else {
            panic!("expected ObligationUnderReleased, got {diagnostic:#?}");
        };
        assert_eq!(*site, SiteId(3));
        assert_eq!(
            blocks,
            &[2, 7, 9],
            "every distinct unbalanced exit is listed once"
        );
        assert_eq!(
            reason, "exit bb7 omits the cleanup",
            "the first exit's reason is kept"
        );
        assert!(
            diagnostic.note.contains("at least one failing exit path"),
            "joined provenance is Mixed; note = {}",
            diagnostic.note
        );
    }

    #[test]
    fn distinct_mint_sites_in_distinct_functions_stay_distinct_diagnostics() {
        let diagnostics = project_findings(&[
            under_released_in("f", 7, 3, ObligationMintProvenance::Ordinary),
            under_released_in("g", 7, 4, ObligationMintProvenance::Ordinary),
        ]);
        assert_eq!(diagnostics.len(), 2, "{diagnostics:#?}");
    }

    #[test]
    fn two_leaked_owners_in_one_function_report_the_first_only() {
        let diagnostics = project_findings(&[
            under_released(7, 3, ObligationMintProvenance::Ordinary),
            under_released(7, 4, ObligationMintProvenance::Ordinary),
        ]);
        let [diagnostic] = diagnostics.as_slice() else {
            panic!("one inconsistent lowering reports once, got {diagnostics:#?}");
        };
        let MirDiagnosticKind::ObligationUnderReleased { site, .. } = &diagnostic.kind else {
            panic!("expected ObligationUnderReleased, got {diagnostic:#?}");
        };
        assert_eq!(*site, SiteId(3), "the first finding is the one kept");
    }

    #[test]
    fn an_obligation_imbalance_is_internal_and_a_use_after_consume_is_not() {
        let diagnostics = project_findings(&[
            under_released(7, 3, ObligationMintProvenance::Ordinary),
            over_released(2, 9),
            use_after_consume(),
        ]);
        let classified = diagnostics
            .iter()
            .map(|diagnostic| diagnostic.kind.internal_compiler_error_function())
            .collect::<Vec<_>>();
        assert_eq!(classified, [Some("f"), None]);
    }

    #[test]
    fn lowering_invariants_report_once_per_function() {
        let diagnostics = project_findings(&[
            drift("f", 1, "ownership-place"),
            drift("f", 2, "ownership-generation"),
            MirCheck::DischargeAuthorityMissing {
                function: "f".to_owned(),
                block: 3,
                authority: NeutralizeAuthority::WholeCarrierConsume,
                reason: "no transferee".to_owned(),
            },
            MirCheck::ObligationBalanceUnverified {
                function: "f".to_owned(),
                reason: "fixpoint cap".to_owned(),
            },
            drift("g", 1, "edge-carry"),
        ]);
        let rules = diagnostics
            .iter()
            .map(|diagnostic| match &diagnostic.kind {
                MirDiagnosticKind::LoweringInvariant { function, rule, .. } => {
                    format!("{function}:{rule}")
                }
                other => panic!("unexpected projection {other:?}"),
            })
            .collect::<Vec<_>>();
        assert_eq!(rules, ["f:ownership-place", "g:edge-carry"]);
    }

    #[test]
    fn user_findings_are_not_suppressed_by_an_invariant_in_the_same_function() {
        let diagnostics = project_findings(&[
            drift("f", 1, "ownership-place"),
            use_after_consume(),
            drift("f", 2, "ownership-place"),
        ]);
        assert_eq!(diagnostics.len(), 2, "{diagnostics:#?}");
        assert!(matches!(
            diagnostics[1].kind,
            MirDiagnosticKind::UseAfterConsume { .. }
        ));
    }
}
