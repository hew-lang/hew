//! Ownership vocabulary of the semantic IR (`docs/internal/ir-ladder.md` §1.2,
//! §1.3, §1.5).
//!
//! All ownership is explicit in the op stream. An operand's mode **is the op it
//! feeds**; there is no side tag on a read.

use crate::model::{AggregateShapeRef, SemAggregateShape, SemParamPassing, ValueId};
use hew_parser::ast::Span;
use hew_types::{CloneKind, ResolvedTy, TypeInstanceKey, ValueClass};

/// The §6.2 fact table a module carries, as the ownership rules read it.
pub type TypeFactTable = std::collections::BTreeMap<TypeInstanceKey, hew_types::TypeFacts>;

/// One ordered aggregate field's ownership recipe.
///
/// This is derived solely from the exact concrete field type and the
/// checker-published fact row. Physical lowering may choose storage and ABI,
/// but it must consume this recipe instead of classifying a field again.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AggregateFieldRecipe {
    pub ty: ResolvedTy,
    pub own: OwnKind,
    pub clone: CloneKind,
}

/// Resolve one operation's exact ordered aggregate field types.
///
/// Tuple shapes are structural. Named records must carry a module-local shape
/// ID whose descriptor agrees with the operation's concrete aggregate type.
/// No consumer may reconstruct a record descriptor from its display name.
///
/// # Errors
///
/// Refuses an empty/non-tuple structural shape, a missing/non-canonical record
/// ID, or a record descriptor whose concrete type differs from the operation.
pub fn aggregate_field_types(
    shape: AggregateShapeRef,
    aggregate_ty: &ResolvedTy,
    shapes: &[SemAggregateShape],
) -> Result<Vec<ResolvedTy>, String> {
    match shape {
        AggregateShapeRef::Tuple => match aggregate_ty {
            ResolvedTy::Tuple(fields) if !fields.is_empty() => Ok(fields.clone()),
            ResolvedTy::Tuple(_) => Err("empty tuples have no owned aggregate shape".to_string()),
            _ => Err(format!(
                "tuple aggregate operation has non-tuple type `{}`",
                aggregate_ty.user_facing()
            )),
        },
        AggregateShapeRef::Record(id) => {
            let descriptor = shapes
                .get(usize::try_from(id.0).map_err(|_| {
                    format!("aggregate shape {} exceeds the host index range", id.0)
                })?)
                .filter(|descriptor| descriptor.id == id)
                .ok_or_else(|| format!("aggregate shape {} is missing or non-canonical", id.0))?;
            if &descriptor.aggregate_ty != aggregate_ty {
                return Err(format!(
                    "aggregate shape {} describes `{}`, not `{}`",
                    id.0,
                    descriptor.aggregate_ty.user_facing(),
                    aggregate_ty.user_facing()
                ));
            }
            Ok(descriptor
                .fields
                .iter()
                .map(|field| field.ty.clone())
                .collect())
        }
    }
}

/// Derive the single ordered ownership/copy recipe for one concrete aggregate.
///
/// Missing field facts are a hard refusal. This service is shared by the SIR
/// verifier and physical lowering, so aggregate glue has one semantic input.
///
/// # Errors
///
/// Returns [`aggregate_field_types`]'s shape refusal or identifies the exact
/// concrete field type whose checker-published fact row is absent.
pub fn aggregate_field_recipes(
    shape: AggregateShapeRef,
    aggregate_ty: &ResolvedTy,
    shapes: &[SemAggregateShape],
    facts: &TypeFactTable,
) -> Result<Vec<AggregateFieldRecipe>, String> {
    aggregate_field_types(shape, aggregate_ty, shapes)?
        .into_iter()
        .map(|ty| {
            let row = facts.get(&TypeInstanceKey(ty.clone())).ok_or_else(|| {
                format!(
                    "aggregate field `{}` has no concrete type-fact row",
                    ty.user_facing()
                )
            })?;
            Ok(AggregateFieldRecipe {
                ty,
                own: OwnKind::of_class(row.class),
                clone: row.clone,
            })
        })
        .collect()
}

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

    /// The §1.2 kind of a value of `ty`, read out of the module's fact table.
    ///
    /// The lowering that writes `own` onto a definition reads the class the
    /// checker decided rather than deciding one of its own, so a value's kind
    /// cannot be settled by one rule and audited by another.
    ///
    /// # Errors
    ///
    /// Returns a fail-closed missing-facts error. Concrete fact expansion must
    /// happen through `hew_types::TypeFactService` before a value is built.
    pub fn of_ty(ty: &ResolvedTy, facts: &TypeFactTable) -> Result<Self, String> {
        facts
            .get(&TypeInstanceKey(ty.clone()))
            .map(|row| Self::of_class(row.class))
            .ok_or_else(|| {
                format!(
                    "SIR cannot decide the ownership kind of `{}`: concrete type facts are missing",
                    ty.user_facing()
                )
            })
    }

    /// The §1.2 kind of a parameter, which is its ABI slot before it is its
    /// type's class.
    ///
    /// Rule 3: a parameter whose header slot is [`SemParamPassing::Borrow`] is
    /// a `Guaranteed` value for the whole body whatever its type's class says,
    /// because the caller keeps the obligation. Every other slot takes the
    /// class table's answer.
    ///
    /// # Errors
    ///
    /// Returns [`OwnKind::of_ty`]'s refusal for a non-borrow slot whose type
    /// has no decidable class.
    pub fn of_param(
        ty: &ResolvedTy,
        passing: SemParamPassing,
        facts: &TypeFactTable,
    ) -> Result<Self, String> {
        match passing {
            SemParamPassing::Borrow => Ok(Self::Guaranteed),
            SemParamPassing::ReadOnly => Self::of_ty(ty, facts),
        }
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
    IndexOutOfBounds,
}

#[must_use]
pub const fn runtime_failure_trap_kind(failure: hew_types::RuntimeLogicalFailure) -> TrapKind {
    match failure {
        hew_types::RuntimeLogicalFailure::IndexOutOfBounds => TrapKind::IndexOutOfBounds,
    }
}

/// Exact language-visible failures for a checked binary integer operation.
///
/// This is the semantic authority shared by SIR construction, verification
/// and the physical consumer. A backend may choose how to test these
/// conditions, but it may neither add nor omit a failure edge.
#[must_use]
pub fn checked_binary_failure_kinds(
    op: hew_parser::ast::BinaryOp,
    ty: &ResolvedTy,
) -> Option<&'static [TrapKind]> {
    use hew_parser::ast::BinaryOp;

    const OVERFLOW: &[TrapKind] = &[TrapKind::IntegerOverflow];
    const DIV_UNSIGNED: &[TrapKind] = &[TrapKind::DivideByZero];
    const DIV_SIGNED: &[TrapKind] = &[TrapKind::DivideByZero, TrapKind::SignedMinDivNegOne];
    const SHIFT: &[TrapKind] = &[TrapKind::ShiftOutOfRange];

    if !ty.is_integer() {
        return None;
    }
    match op {
        BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply => Some(OVERFLOW),
        BinaryOp::Divide | BinaryOp::Modulo if ty.is_signed_integer() => Some(DIV_SIGNED),
        BinaryOp::Divide | BinaryOp::Modulo => Some(DIV_UNSIGNED),
        BinaryOp::Shl | BinaryOp::Shr => Some(SHIFT),
        BinaryOp::Equal
        | BinaryOp::NotEqual
        | BinaryOp::Less
        | BinaryOp::LessEqual
        | BinaryOp::Greater
        | BinaryOp::GreaterEqual
        | BinaryOp::And
        | BinaryOp::Or
        | BinaryOp::BitAnd
        | BinaryOp::BitOr
        | BinaryOp::BitXor
        | BinaryOp::Range
        | BinaryOp::RangeInclusive
        | BinaryOp::WrappingAdd
        | BinaryOp::WrappingSub
        | BinaryOp::WrappingMul => None,
    }
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

/// How an owning value crosses an actor or task boundary (§2 rule 5).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SnapshotDecision {
    Share,
    DeepCopy,
    Transfer,
}

/// The total ownership decision for one semantic boundary operand.
///
/// This is carried only by call, return, and suspension boundary shapes. Plain
/// [`crate::Operand`] remains a value use with no generic mode field, and
/// construction has no absent, default, or undecided state.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BoundaryDecision {
    Borrow,
    Copy,
    Move,
    Snapshot(SnapshotDecision),
}

/// Function-local identity of one source binding.
///
/// This is distinct from HIR's binding identity: SIR assigns it from the
/// deterministic [`crate::SemFunction::bindings`] order after specialization.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BindingId(pub u32);

/// The semantic storage a source binding names.
///
/// A closed target keeps value aliases and materialized place roots in the one
/// ordered binding table, rather than copying source provenance onto places or
/// stores.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BindingTarget {
    Value(ValueId),
    Place(PlaceId),
}

/// One source binding, and the value or place it names.
///
/// §1.6 separates a user-facing wall from an internal error by asking whether
/// the offending value is named by a source binding, and rule 6a needs the
/// binding's mutability.
///
/// A binding is not a property of a value: `let alias = x` names the value `x`
/// already names, so one value carries as many bindings as the source wrote.
/// Recording the name on the definition instead kept only the first, and every
/// later alias vanished. The function carries the bindings in source order, and
/// a value's user-facing name is its most recent binding
/// ([`crate::SemFunction::binding_naming`]).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Binding {
    pub id: BindingId,
    pub name: String,
    pub span: Span,
    pub mutable: bool,
    pub target: BindingTarget,
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

    /// A `Named` with no published row and no builtin discriminator has no
    /// class in the empty declaration context, so SIR refuses it. The name
    /// decides nothing: a user `Location`, whose name is in the builtin table,
    /// is refused exactly as an identically-shaped `Handle` is.
    #[test]
    fn a_named_type_with_no_row_refuses_whether_or_not_its_name_is_a_builtin() {
        let none = super::TypeFactTable::new();
        for name in ["Location", "Handle"] {
            let ty = hew_types::ResolvedTy::Named {
                name: name.to_string(),
                args: Vec::new(),
                builtin: None,
                is_opaque: false,
            };
            let error = OwnKind::of_ty(&ty, &none)
                .expect_err("a type with no row and no declaration has no ownership kind");
            assert!(error.contains(name), "{error}");
        }
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
