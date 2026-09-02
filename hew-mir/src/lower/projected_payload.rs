//! #2523 — provenance for a `match` / `if let` / `while let` arm binder
//! projected out of an enum or machine payload.
//!
//! The binder's storage is a byte-copy ALIAS of the scrutinee's payload slot.
//! Whether it may become an owner of its own, and what a move-out of it costs,
//! is decided entirely by how the scrutinee reached the match temp — which is
//! what [`ProjectedPayloadOrigin`] records.

use super::{BindingId, Place, ProjectedPayloadRejectReason, ResolvedTy};

/// #2523 — provenance for a `match`-arm binder projected out of an enum/machine
/// payload (`lower_match_enum_tag`). The binder's storage is a byte-copy ALIAS
/// of the scrutinee's payload slot; when the binder is moved into a new owner
/// its heap ownership transfers, and the source slot must be neutralized so the
/// scrutinee's own drop no-ops on it. Recorded strictly for
/// `Place::MachineVariant`/`Place::EnumVariant` sources at the destructure site.
#[derive(Debug, Clone)]
pub(crate) struct ProjectedPayloadProvenance {
    /// The interior projection the binder aliases; nulled by
    /// `Instr::NeutralizePayloadSlot` at the binder's move-out.
    pub(crate) source_place: Place,
    /// The binder's own name, used to author the fail-closed diagnostic when
    /// the scrutinee is a re-readable place (see [`ProjectedPayloadOrigin`]).
    pub(crate) binder_name: String,
    /// How the scrutinee was materialised — decides whether a projected-payload
    /// move-out is soundly neutralizable at the temp, needs a consume-mark, or
    /// must be rejected fail-closed.
    pub(crate) origin: ProjectedPayloadOrigin,
}

/// #2523 — classification of a projected-payload binder's scrutinee, which
/// decides how a move-out of the (heap-owning) binder is made sound.
///
/// The match lowers the scrutinee into a temp before destructuring. Whether
/// nulling that temp (`Instr::NeutralizePayloadSlot`) actually neutralizes the
/// payload's *sole surviving owner* depends on how the scrutinee reached the
/// temp:
#[derive(Debug, Clone)]
pub(crate) enum ProjectedPayloadOrigin {
    /// The scrutinee is a bare owning binding (`match b { … }`): the match
    /// MOVES it into the temp, so the temp is the sole live copy. Neutralize
    /// the temp AND consume-mark the binding (via `MirStatement::AggregateAlias`)
    /// so a later re-read is a compile-time use-after-move rather than a read of
    /// the nulled slot.
    OwnedBinding(ProjectedScrutinee),
    /// The scrutinee is an ephemeral producer (`match f() { … }`,
    /// `match Box::Full(x) { … }`): the temp is a fresh, sole-owner value with
    /// no re-readable origin, so neutralizing the temp transfers ownership
    /// soundly with no consume-mark needed.
    EphemeralTemp,
    /// The FAIL-CLOSED default (#2523 F1/F1b/F2): the scrutinee is anything NOT
    /// proven a fresh sole owner — a re-readable *place* projection (`match h.b`,
    /// `match arr[i]`, `match self.field`), a `Block`/`If`/`Scope` wrapper whose
    /// value is a sub-expression (`match { h.b }`), a closure-CAPTURED binding
    /// (read from the env by copy, not moved into the temp), a NESTED-pattern
    /// binder (extracted through a transient copy the move cannot neutralize),
    /// or any un-enumerated / future HIR shape. A direct projection from an
    /// owned tuple (`match pair.0`) is the one explicit exception: enum-match
    /// lowering can neutralize that exact tuple field, consume-mark the tuple,
    /// and mint the match temp as its sole replacement owner. Other move-outs
    /// are REJECTED before codegen; `reason` selects the precise diagnostic.
    /// Rejecting a wrapper-hidden but otherwise-safe producer is acceptable —
    /// the safe default is to reject rather than risk aliasing. Borrow-only
    /// matches never reach this arm (they do not consume the binder).
    Reject(ProjectedPayloadRejectReason),
}

impl ProjectedPayloadOrigin {
    /// Whether the matched storage keeps a live release authority over the
    /// payload for as long as the arm binder exists.
    ///
    /// `OwnedBinding` (`match b`) leaves `b`'s composite drop scheduled, and
    /// every `Reject` shape is by construction a copy of storage somebody else
    /// still owns — a re-readable place, a closure-environment field, the outer
    /// value behind a nested transient. In all of those the binder is a
    /// byte-copy ALIAS: minting it a second owner schedules a second release of
    /// one buffer. Only `EphemeralTemp` hands the arm a fresh sole owner whose
    /// release nobody else is holding.
    ///
    /// A genuine move-out of an aliased binder is not lost by this: the
    /// consume hook transfers the authority at the move (neutralizing the
    /// source slot for `OwnedBinding`/`EphemeralTemp`, rejecting fail-closed
    /// for `Reject`).
    pub(crate) fn scrutinee_retains_payload(&self) -> bool {
        matches!(self, Self::OwnedBinding(_) | Self::Reject(_))
    }
}

/// The re-readable scrutinee binding behind a [`ProjectedPayloadOrigin::OwnedBinding`],
/// consume-marked (via `MirStatement::AggregateAlias`) at the binder's move-out
/// so the checker rejects a later re-read as a use-after-move.
#[derive(Debug, Clone)]
pub(crate) struct ProjectedScrutinee {
    pub(crate) binding: BindingId,
    pub(crate) name: String,
    pub(crate) ty: ResolvedTy,
}
