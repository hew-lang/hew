//! Ownership vocabulary of the semantic IR (`docs/internal/ir-ladder.md` §1.2,
//! §1.3, §1.5).
//!
//! All ownership is explicit in the op stream. An operand's mode **is the op it
//! feeds**; there is no side tag on a read.

use hew_parser::ast::Span;
use hew_types::{ResolvedTy, ValueClass};

/// The ownership obligation carried by one SSA value (§1.2).
///
/// A pure function of the value's type class, except `Guaranteed`, which is not
/// a class at all: it is the kind of the result of `begin_borrow`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum OwnKind {
    /// No obligation; the value may be used any number of times.
    None,
    /// Exactly one consuming use per path.
    Owned,
    /// No obligation, and must not outlive the borrow scope that produced it.
    Guaranteed,
}

impl OwnKind {
    /// The §1.2 table, as a total function of the class.
    #[must_use]
    pub const fn of_class(class: ValueClass) -> Self {
        match class {
            ValueClass::BitCopy | ValueClass::View => Self::None,
            ValueClass::CowValue
            | ValueClass::PersistentShare
            | ValueClass::AffineResource
            | ValueClass::Linear => Self::Owned,
        }
    }

    /// The §1.2 kind of a value of `ty`, read through the one class authority.
    ///
    /// The lowering that writes `own` onto a definition and the verifier that
    /// checks it both call this, so a value's kind cannot be decided by one
    /// rule and audited by another.
    ///
    /// MARKED SHORTCUT — the class is read against an empty declaration
    /// context.
    /// WHY: `lower_module` takes a `HirModule` and no `TypeCheckOutput`, so
    /// §1.1's declaration facts are not in reach on this route. Every user
    /// `Named` type therefore classes `UnknownDeclaration` and this refuses,
    /// which is the fail-closed answer for a domain that admits only scalars
    /// and tuples of scalars.
    /// WHEN: HIR-to-SIR lowering threads the checker output through, and with
    /// it `TypeCheckOutput::type_facts` (L3).
    /// WHAT: the context is built from that table and this reads a decided
    /// fact rather than recomputing one.
    ///
    /// # Errors
    ///
    /// Returns the class rule's refusal, rendered against the user-facing type
    /// name, when §1.1 cannot decide the type's class.
    pub fn of_ty(ty: &ResolvedTy) -> Result<Self, String> {
        hew_types::ValueClass::of_ty(ty, &hew_types::ClassContext::empty())
            .map(Self::of_class)
            .map_err(|error| {
                format!(
                    "SIR cannot decide the ownership kind of `{}`: {error}",
                    ty.user_facing()
                )
            })
    }
}

/// Module-local identity of a memory place (§1.3 `alloc_place`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PlaceId(pub u32);

/// One place a function body can address.
///
/// `runtime_owned` distinguishes an actor state field or an environment slot,
/// whose exit obligations §1.3.6 states, from an ordinary function-owned place.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PlaceDecl {
    pub id: PlaceId,
    pub ty: ResolvedTy,
    pub runtime_owned: bool,
}

/// Module-local interned identity of a `string` literal (§1.3.1).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StringLiteralId(pub u32);

/// Module-local interned identity of a `bytes` literal (§1.3.1).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BytesLiteralId(pub u32);

/// Which language-visible trap a `Trap` terminator transfers control to.
///
/// Closed and matched with no wildcard arm anywhere, so a kind added by a later
/// phase is a compile error rather than a silently-handled case. These four are
/// the kinds the P1 rows of `sir-domain-matrix.md` produce; the bounds and
/// panic traps arrive with the `Index`/`Slice` rows they come from.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TrapKind {
    IntegerOverflow,
    DivideByZero,
    SignedMinDivNegOne,
    ShiftOutOfRange,
}

/// Which runtime operation a `Suspend` terminator parks on (§1.5).
///
/// One variant per spelling in the **SIR `SuspendKind`** column of §1.5's kind
/// table. Closed and matched with no wildcard arm anywhere, so a kind added by
/// a later phase is a compile error. The payload fields the Raw MIR carriers
/// hold, and the abandon-op ordering rules, arrive with P3 and P4.
///
/// Two spellings in that column are recorded questions rather than settled
/// collapses, and are landed here as one variant each:
///
/// - `Yield` appears on **two** §1.5 rows - a `gen fn` body, whose value input
///   is `Move`, and a `receive gen fn` body, whose value input is `Snapshot`
///   because the pump sends it across an actor boundary. One kind cannot carry
///   both input modes; the phase that lowers them decides whether it splits.
/// - `Select` and `Timeout` share one row while §1.5's prose says "the deadline
///   form is a second arm, not a second kind". Both spellings are landed so
///   neither reading is foreclosed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SuspendKind {
    Await,
    RestartWait,
    ActorSend,
    Ask,
    RemoteAsk,
    Read,
    Accept,
    ChannelRecv,
    StreamNext,
    StreamSend,
    CallClosure,
    Select,
    Timeout,
    Join,
    ScopeDeadline,
    Yield,
    Sleep,
    SleepUntil,
}

/// How a `Suspend` terminator takes one of its inputs (§1.5).
///
/// This is the one place a mode word survives on an operand, and it is legal
/// because §1.5's `Suspend` struct declares it:
/// `inputs: Vec<Operand>, // mode ∈ { Borrow, Move }`. It is a terminator-input
/// mode, not a successor to the deleted operand-mode set: `Read`,
/// `BorrowShared` and `BorrowMut` are gone and do not come back here.
///
/// §1.5's own kind table needs more than these two, and that is a defect in the
/// section rather than a choice this vocabulary makes:
///
/// - Four rows give an input the mode `Snapshot`, which is in rule 5's closed
///   decided-mode set - `ActorSend`'s args, `Ask`'s args, `StreamSend`'s value,
///   and the `receive gen fn` `Yield` value.
/// - Seven rows give an input the mode `(None)`, the §1.2 `OwnKind::None` of a
///   `BitCopy` scalar, which is neither `Borrow` nor `Move` - `Read`'s and
///   `Accept`'s deadline, `ChannelRecv`'s and `StreamNext`'s deadline,
///   `RemoteAsk`'s timeout, `Select`'s `AfterTimer` duration, and the durations
///   of `ScopeDeadline`, `Sleep` and `SleepUntil`.
///
/// The two variants the struct names are landed here; the rows above need more
/// at P4, when a producer for them exists.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SuspendInputMode {
    Borrow,
    Move,
}

/// The source binding an SSA value came from.
///
/// §1.6 separates a user-facing wall from an internal error by asking whether
/// the offending op's provenance is a source binding, and rule 6a needs the
/// binding's mutability. A synthesized value carries no provenance.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BindingProvenance {
    pub name: String,
    pub span: Span,
    pub mutable: bool,
}

#[cfg(test)]
mod tests {
    use super::OwnKind;
    use hew_types::ValueClass;

    /// §1.2's table: the obligation is a function of the class, and the two
    /// no-obligation classes are exactly `BitCopy` and `View`.
    #[test]
    fn own_kind_follows_the_class_table() {
        assert_eq!(OwnKind::None, OwnKind::of_class(ValueClass::BitCopy));
        assert_eq!(OwnKind::None, OwnKind::of_class(ValueClass::View));
        assert_eq!(OwnKind::Owned, OwnKind::of_class(ValueClass::CowValue));
        assert_eq!(
            OwnKind::Owned,
            OwnKind::of_class(ValueClass::PersistentShare)
        );
        assert_eq!(
            OwnKind::Owned,
            OwnKind::of_class(ValueClass::AffineResource)
        );
        assert_eq!(OwnKind::Owned, OwnKind::of_class(ValueClass::Linear));
    }

    /// `Guaranteed` is not a class: no class maps to it. It is minted only by
    /// `begin_borrow`.
    #[test]
    fn no_class_produces_guaranteed() {
        for class in [
            ValueClass::BitCopy,
            ValueClass::View,
            ValueClass::CowValue,
            ValueClass::PersistentShare,
            ValueClass::AffineResource,
            ValueClass::Linear,
        ] {
            assert_ne!(OwnKind::Guaranteed, OwnKind::of_class(class));
        }
    }
}
