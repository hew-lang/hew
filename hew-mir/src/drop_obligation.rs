//! The value-context lattice: the enumerated authority over every position a
//! value can live in, and whether that position's exactly-once release is
//! implemented or fail-closed.
//!
//! The admission predicate for every position is the single structural
//! authority [`crate::model::ty_carries_drop_obligation`] — heap ownership OR a
//! registered `#[resource]` close contract, computed by one walker. This
//! module enumerates the POSITIONS that predicate is consulted from, so the
//! release authority is total by enumeration: a value-bearing position is
//! either listed here with `Implemented` (its discharge path exists and is
//! exercised by `tests/core-matrix`), or listed with `FailClosed` (the
//! compiler rejects a close-obligated value in that position with a
//! user-facing diagnostic). An UNLISTED position is a compile error at the
//! `match` in [`position_release_support`] the moment it is added to the enum —
//! never a silent leak.
//!
//! | Position | Support | Discharge / rejection authority |
//! |---|---|---|
//! | `Local` | `Implemented` | scope-exit LIFO drop plan (`lower/drop_plan.rs`) |
//! | `RecordField` | `Implemented` | record lifecycle member drop |
//! | `EnumPayload` | `Implemented` | tag-aware `EnumInPlace` drop (`ty_is_drop_obligated_enum_composite`) |
//! | `TupleElement` | `Implemented` | tuple-composite drop (`ty_is_drop_obligated_tuple`) |
//! | `CollectionElement` | `Implemented` | owned-element Vec ABI (`hew_vec_free_owned` + per-element thunk) |
//! | `MapKey` | `FailClosed` | layout-key ABI requires fixed-size Copy `record` keys |
//! | `MapValue` | `Implemented` | layout-keyed `hew_hashmap_free_layout` value walk |
//! | `IteratorYield` | `FailClosed` | `VecIter::next()` clones; resources have no semantic clone |
//! | `CollectionIndexOut` | `Implemented` | `v[i]` lowers to the `hew_vec_get_owned` BORROW (never clone-out); `Vec::get`/iteration clone-out stays rejected upstream |
//! | `TraitObjectPayload` | `Implemented` | vtable slot-0 `drop_in_place` (`DropKind::TraitObject`) runs the payload's close |
//! | `ActorStateField` | `Implemented` | actor-state classifier (`state_clone.rs`), itself fail-closed |
//! | `ClosureCapture` | `FailClosed` | a borrow capture leaves the LOCAL the owner (discharged there); an env-slot owner has no wired close |
//! | `SuspendFrameSlot` | `Implemented` | suspend-region drop plan (`across_suspend` matrix row) |
//! | `MachineStatePayload` | `FailClosed` | machine transition/scope drop not yet elaborated |
//! | `AssignmentOverwrite` | `Implemented` | rebind is a GENERATION BOUNDARY: the overwritten value is released at the store (`emit_local_overwrite_release`), never inferred from entry-time classification |
//!
//! Rows marked `FailClosed` are acceptable landing states, not design goals: a
//! rejection names the limit to the user; wiring the discharge later flips the
//! row without touching consult sites.

/// A value-bearing position in a Hew program — the context axis of the
/// drop-obligation lattice. Every position the compiler lowers a value into is
/// one of these variants; adding a lowering surface for a new position means
/// adding a variant HERE and answering [`position_release_support`] for it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ValuePosition {
    /// A `let`/`var` binding in a function body.
    Local,
    /// A field of a user record.
    RecordField,
    /// The payload of a tagged-union enum variant (`Result`, `Option`, user
    /// enums).
    EnumPayload,
    /// An element of a tuple.
    TupleElement,
    /// An element of an owned collection buffer (`Vec<E>`).
    CollectionElement,
    /// A `HashMap`/`HashSet` key.
    MapKey,
    /// A `HashMap` value.
    MapValue,
    /// A value yielded by an iterator cursor (`VecIter::next`).
    IteratorYield,
    /// A value cloned OUT of a collection by index (`v[i]`, `Vec::get`).
    CollectionIndexOut,
    /// The concrete payload behind a `dyn Trait` fat pointer.
    TraitObjectPayload,
    /// A named state field of an `actor`.
    ActorStateField,
    /// A capture slot of a closure environment.
    ClosureCapture,
    /// A frame slot live across a suspend point.
    SuspendFrameSlot,
    /// The payload of a `machine` state variant.
    MachineStatePayload,
    /// The previous value of a `var` binding being reassigned.
    AssignmentOverwrite,
}

/// Whether a close-obligated value in a position has a wired exactly-once
/// discharge, or must be rejected at compile time.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReleaseSupport {
    /// The position's discharge path exists; a close-obligated value here is
    /// released exactly once on every exit path.
    Implemented,
    /// No discharge path exists yet. A close-obligated value reaching this
    /// position MUST be rejected with the carried user-facing reason —
    /// compiling it would leak silently.
    FailClosed(&'static str),
}

/// The lattice's support verdict per position. Exhaustive by construction: a
/// new [`ValuePosition`] variant fails compilation here until it is answered.
#[must_use]
pub const fn position_release_support(position: ValuePosition) -> ReleaseSupport {
    match position {
        ValuePosition::Local
        | ValuePosition::RecordField
        | ValuePosition::EnumPayload
        | ValuePosition::TupleElement
        | ValuePosition::CollectionElement
        | ValuePosition::MapValue
        | ValuePosition::ActorStateField
        | ValuePosition::SuspendFrameSlot
        | ValuePosition::CollectionIndexOut
        | ValuePosition::TraitObjectPayload
        | ValuePosition::AssignmentOverwrite => ReleaseSupport::Implemented,
        ValuePosition::MapKey => ReleaseSupport::FailClosed(
            "a `#[resource]` value cannot be a map key: the layout-key ABI \
             requires fixed-size Copy `record` keys",
        ),
        ValuePosition::IteratorYield => ReleaseSupport::FailClosed(
            "iterating clones each element into an independent owner, but a \
             `#[resource]` value has an affine close contract and no semantic \
             clone",
        ),
        ValuePosition::ClosureCapture => ReleaseSupport::FailClosed(
            "capturing a `#[resource]` value into a closure environment is not \
             supported yet: the environment's drop path cannot run the close \
             contract",
        ),
        ValuePosition::MachineStatePayload => ReleaseSupport::FailClosed(
            "a `#[resource]` value in a machine state payload is not released \
             on transition or scope exit yet",
        ),
    }
}

#[cfg(test)]
mod tests {
    use super::{position_release_support, ReleaseSupport, ValuePosition};

    /// Every fail-closed row carries a non-empty, user-facing reason (no
    /// internal jargon markers) — the diagnostic the gates surface verbatim.
    #[test]
    fn fail_closed_rows_carry_user_facing_reasons() {
        let all = [
            ValuePosition::Local,
            ValuePosition::RecordField,
            ValuePosition::EnumPayload,
            ValuePosition::TupleElement,
            ValuePosition::CollectionElement,
            ValuePosition::MapKey,
            ValuePosition::MapValue,
            ValuePosition::IteratorYield,
            ValuePosition::CollectionIndexOut,
            ValuePosition::TraitObjectPayload,
            ValuePosition::ActorStateField,
            ValuePosition::ClosureCapture,
            ValuePosition::SuspendFrameSlot,
            ValuePosition::MachineStatePayload,
            ValuePosition::AssignmentOverwrite,
        ];
        for position in all {
            if let ReleaseSupport::FailClosed(reason) = position_release_support(position) {
                assert!(
                    !reason.is_empty(),
                    "{position:?} reason must be user-facing"
                );
                assert!(
                    !reason.contains("E_MIR") && !reason.contains("unreachable"),
                    "{position:?} reason must not leak internals: {reason}"
                );
            }
        }
    }
}
