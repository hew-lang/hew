//! Typed ownership / drop / ABI decision carried **on** a MIR value.
//!
//! # Invariant (the root-cause fix this module exists to enable)
//!
//! > **Ownership / drop / ABI facts must be a typed, total decision attached to
//! > the value — not a boolean re-derived per shape at each consumer.**
//!
//! Today ~8 independent shape-walkers each re-answer "does this own heap / how
//! is it released / how does it cross the ABI?" by matching on the nominal
//! [`ResolvedTy`] at their own call site (`ty_owns_heap`, `cbor_ty_owns_heap`,
//! `ValueClass::of_ty`, the `derive_*_drop_allowed` family, the
//! `cow_value_leaf_drop_symbol` / `binding_ty_is_plain_vec` release buckets, …).
//! Because the answer is re-derived rather than carried, the walkers drift at
//! seams (MIR ↔ codegen, getter ↔ release) and fall through on unenumerated
//! shapes (`_ => false`), which is the proximate cause of the leak / double-free
//! / abort class of regressions.
//!
//! [`OwnershipDecision`] is the single typed fact those walkers will later read
//! off the value instead of re-deriving. This Phase-0 landing is **additive and
//! unused**: it defines the type, a [`OwnershipDecision::classify`] constructor
//! scaffold, the seed conversions, and a totality test. It switches **no**
//! production call site — Phases 1A-1D route the existing authorities through
//! it.
//!
//! # Subsumes (does not reinvent) the existing seeds
//!
//! | Seed | Authority | Relationship to [`OwnershipDecision`] |
//! |---|---|---|
//! | `ty_owns_heap` | `model.rs` | The structural yes/no the [`OwnsHeap`]/[`NoHeap`] split is built **from** ([`OwnershipDecision::owns_heap`] projects back). |
//! | [`DropKind`] | `model.rs` | The release protocol — [`DropClass`] subsumes it (`TryFrom<DropKind>` + [`DropClass::canonical_drop_kind`]). |
//! | [`ValueClass`] | `value_class.rs` | The coarse class — [`OwnershipDecision::to_value_class`] projects back; `classify` consumes it for the marker seed. |
//! | [`Strategy::UnknownBlocked`] | `model.rs` | The fail-closed move strategy — [`Unsupported`] projects to it ([`OwnershipDecision::move_strategy_floor`]). |
//! | `DropPlanUndetermined` | `lower.rs` | The fail-closed diagnostic — [`Unsupported`]'s [`FailClosedReason`] names the cause. |
//!
//! [`NoHeap`]: OwnershipDecision::NoHeap
//! [`OwnsHeap`]: OwnershipDecision::OwnsHeap
//! [`Unsupported`]: OwnershipDecision::Unsupported

use std::collections::hash_map::RandomState;
use std::collections::HashMap;
use std::hash::BuildHasher;

use hew_hir::{TypeClassTable, ValueClass};
use hew_types::{BuiltinType, ResolvedTy};

use crate::model::{
    find_enum_layout, ty_owns_heap, Direction, DropKind, EnumLayout, HeapOwnershipLayouts,
    MirHeapLayouts, Place, Strategy, TraitObjectStorage,
};

// ───────────────────────────── core decision ─────────────────────────────

/// The single typed ownership/drop/ABI decision for one MIR value.
///
/// This is *not* a boolean. Each variant carries exactly the facts a downstream
/// consumer would otherwise re-derive from the nominal type, so the getter,
/// constructor, and release paths cannot reach different verdicts and an
/// unenumerated shape is a typed [`Unsupported`](Self::Unsupported) — never a
/// silent `false`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OwnershipDecision {
    /// Owns no heap memory and has no scope-exit drop obligation: scalars,
    /// `unit`/`never`, bare function pointers, and heap-free aggregates
    /// (records/tuples/enums all of whose fields are themselves `NoHeap`).
    /// Subsumes `ValueClass::BitCopy` and the `ty_owns_heap == false` half of
    /// the authority.
    NoHeap,

    /// Transitively owns heap memory and must be released exactly once on every
    /// reachable scope exit. Carries the three facts the shape-walkers each
    /// re-derive today, so a consumer reads them off the value:
    ///
    /// - `drop`:   the release protocol (subsumes [`DropKind`]).
    /// - `abi`:    how the value crosses a call/return boundary.
    /// - `layout`: the structural storage family the release walks.
    OwnsHeap {
        /// Release protocol family — [`DropClass`].
        drop: DropClass,
        /// Boundary-crossing shape — [`AbiClass`].
        abi: AbiClass,
        /// Structural storage family — [`LayoutClass`].
        layout: LayoutClass,
    },

    /// A non-owning borrow / view of a value owned elsewhere (`&T`, `*T`, `[T]`
    /// slices). No drop obligation. Carries the [`ValueProvenance`] of the owner
    /// it views so escape analysis reasons without re-deriving from the type.
    /// Subsumes `ValueClass::View`.
    Borrowed {
        /// Provenance of the owner this view points into.
        provenance: ValueProvenance,
    },

    /// An interior alias of a still-live owner: a destructure / projection
    /// binder reading a sub-location (field / element / variant payload) of an
    /// aggregate whose whole-value owner is still live. Must **not** drop (the
    /// owner drops the whole); `owner` names which owner is responsible. The
    /// typed form of the "projection-alias payload binder" scope-close-drop
    /// skip.
    InteriorAlias {
        /// Provenance of the still-live owner responsible for the release.
        owner: ValueProvenance,
    },

    /// The ownership fact could not be decided. The single fail-closed sink: a
    /// heap-bearing value whose release protocol no arm could name reaches here,
    /// and Phase 1 routes it to [`Strategy::UnknownBlocked`] /
    /// `MirCheck::DropPlanUndetermined` instead of silently leaking or
    /// double-freeing. `reason` names *why* so the diagnostic is actionable.
    Unsupported {
        /// Why the decision failed closed — feeds the diagnostic.
        reason: FailClosedReason,
    },
}

// ──────────────────────── drop protocol (subsumes DropKind) ───────────────

/// Release-protocol family for an [`OwnsHeap`](OwnershipDecision::OwnsHeap)
/// value. A Phase-0 mirror of [`DropKind`] that carries no codegen-only payload
/// except the discriminators that *are* the classification ([`Direction`],
/// [`TraitObjectStorage`], the [`HeapLeaf`]).
///
/// Every [`DropKind`] maps to exactly one `DropClass`
/// (`TryFrom<DropKind>`) and every `DropClass` maps back to a canonical
/// [`DropKind`] ([`DropClass::canonical_drop_kind`]), so Phase 1 can route the
/// existing dispatcher (`drop_kind_for`) through this axis.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DropClass {
    /// `@resource` close-method dispatch (`close(consuming self)`), incl. the
    /// `CancellationToken` runtime handle. [`DropKind::Resource`].
    Resource,
    /// A single copy-on-write heap leaf released by one runtime symbol
    /// ([`DropKind::CowHeap`]). `leaf` is never [`HeapLeaf::CancellationToken`]
    /// (that is an `@resource` close → [`DropClass::Resource`]); the constructor
    /// path [`HeapLeaf::drop_class`] enforces it.
    CowHeapLeaf {
        /// Which copy-on-write leaf — selects the runtime release symbol.
        leaf: HeapLeaf,
    },
    /// In-place record drop helper (`__hew_record_drop_inplace_<R>`).
    /// [`DropKind::RecordInPlace`].
    RecordInPlace,
    /// In-place tuple per-element drop helper. [`DropKind::TupleInPlace`].
    TupleInPlace,
    /// Tag-aware in-place enum-composite drop helper. [`DropKind::EnumInPlace`].
    EnumInPlace,
    /// Recursive aggregate drop of a tuple/array of heap leaves.
    /// [`DropKind::AggregateRecursive`].
    AggregateRecursive,
    /// Indirect-enum heap-node recursive free (`hew_dealloc`).
    /// [`DropKind::IndirectEnum`].
    IndirectEnum,
    /// Escaping closure-pair env-box free. [`DropKind::ClosurePair`].
    ClosurePair,
    /// `dyn Trait` fat-pointer `drop_in_place` plus the storage-discriminated
    /// release ritual. [`DropKind::TraitObject`].
    DynTrait {
        /// `FrameOwned` vs `HeapBoxed` — selects the post-`drop_in_place` ritual.
        storage: TraitObjectStorage,
    },
    /// `Duplex<S,R>` full close (both directions). [`DropKind::DuplexClose`].
    DuplexClose,
    /// Duplex half-handle close of one direction. [`DropKind::DuplexHalfClose`].
    DuplexHalfClose {
        /// Which queue half this handle closes.
        direction: Direction,
    },
    /// Lambda-actor release-on-last-handle. [`DropKind::LambdaActorRelease`].
    LambdaActorRelease,
}

/// The structural heap-leaf set the authority `ty_owns_heap` recognises — the
/// single source of "what is a heap leaf", mirroring the leaf arm of
/// `ty_owns_heap_inner` one-to-one.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HeapLeaf {
    /// `string` — `hew_string_drop`.
    String,
    /// `bytes` — `hew_bytes_drop` (the three-word `{ptr,len,cap}` triple).
    Bytes,
    /// `Vec<T>` — `hew_vec_free` (the element refinement to `hew_vec_free_owned`
    /// / `hew_vec_free_closure_pairs` is carried by the element's own decision).
    Vec,
    /// `HashMap<K,V>` — `hew_hashmap_free_layout`.
    HashMap,
    /// `HashSet<E>` — `hew_hashset_free_layout`.
    HashSet,
    /// `Generator<Y,R>` / `AsyncGenerator<Y>` — `hew_gen_coro_destroy`.
    Generator,
    /// `CancellationToken` — `hew_cancel_token_release`. NOTE: this leaf's drop
    /// is an `@resource`-class close ([`DropClass::Resource`]), *not* a
    /// copy-on-write leaf; see [`HeapLeaf::drop_class`].
    CancellationToken,
}

// ──────────────────────────── ABI + layout axes ──────────────────────────

/// How an owned value crosses a call / return boundary — the ABI axis of the
/// unified "ownership / drop / **ABI**" fact.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AbiClass {
    /// In registers, by value: scalars, `unit`, `never`.
    Scalar,
    /// A single owned pointer-width handle (string buffer, `Vec`/`HashMap`/
    /// `HashSet`/`Generator` handle, `CancellationToken`, indirect-enum node).
    OwnedHandle,
    /// The `bytes` three-word value `{ ptr, len: i32, cap: i32 }` (drop GEPs
    /// field 0).
    BytesTriple,
    /// A two-word fat pointer: `{ data, vtable }` (`dyn Trait`) or
    /// `{ fn_ptr, env_ptr }` (closure pair).
    FatPointer,
    /// An inline aggregate (record / tuple / enum-composite / array) carried by
    /// value in its alloca; large returns cross via an `sret` pointer.
    Inline,
}

/// The structural storage family a release walks — the `layout` axis.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LayoutClass {
    /// No heap structure (scalar / view).
    Flat,
    /// A single heap buffer behind one owned handle.
    HeapBuffer,
    /// An inline tagged union (`{ tag, payload }`) stored by value.
    TaggedUnion,
    /// A heap-allocated tagged-union node reached by pointer (indirect enum).
    IndirectNode,
    /// An inline product (record / tuple / array) stored by value.
    Product,
    /// A closure environment box (`[free_thunk][captures…]`).
    ClosureEnv,
    /// A fat-pointer'd existential (`dyn Trait`).
    Existential,
}

// ──────────────────────────────── fail-closed ────────────────────────────

/// Why an [`OwnershipDecision::Unsupported`] failed closed. Each reason maps to
/// the actionable diagnostic Phase 1 emits instead of a silent leak; see
/// [`FailClosedReason::diagnostic_note`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FailClosedReason {
    /// `ValueClass::Unknown` — an unmarked `Named` type (or `TypeParam`) whose
    /// ownership the checker could not establish. Routes to
    /// [`Strategy::UnknownBlocked`].
    UnknownValueClass,
    /// A [`ResolvedTy`] variant the classifier does not yet enumerate. The hard
    /// anti-drift sentinel: adding a `ResolvedTy` variant without a `classify`
    /// arm lands here (a compile error in the exhaustive match), never a silent
    /// `false`.
    UnenumeratedShape,
    /// A heap-owning value reached a release path with no release symbol / drop
    /// protocol assigned. Routes to `MirCheck::DropPlanUndetermined`.
    NoReleaseProtocol,
    /// A [`DropKind::CowHeap`] carried a release symbol the leaf set does not
    /// recognise (fail-closed on a future / mistyped symbol).
    UnrecognisedReleaseSymbol,
    /// A consume-once `Linear` value (`Task<T>`, `@linear`): owns a resource but
    /// has **no** implicit scope-exit drop — its release is the move-checker's
    /// `MustConsume`. The 5-variant decision intentionally does not model this
    /// mode yet, so it is tracked-Unsupported rather than guessed.
    LinearConsumeOnce,
    /// A `dyn Trait` whose `TraitObjectStorage` (`FrameOwned` vs `HeapBoxed`) is
    /// not available to the classifier (the `dyn_trait_storage` side table
    /// Phase 1 threads). Fail-closed rather than guessing the release ritual.
    DynStorageUnresolved,
    /// A heap-owning aggregate whose exactly-once free the drop analysis cannot
    /// prove (e.g. owned-handle aggregate extraction). Mirrors
    /// `OwnedHandleAggregateExtractionUnsupported`.
    UnprovenSoleOwner,
}

// ──────────────────────────────── provenance ─────────────────────────────

/// A MIR storage location an ownership fact can be anchored to. The Phase-0
/// place-provenance carrier — a thin, [`Place`]-convertible family so a
/// [`Borrowed`](OwnershipDecision::Borrowed) /
/// [`InteriorAlias`](OwnershipDecision::InteriorAlias) decision names its owner
/// by location, not by re-deriving from the nominal type. `From<Place>` is
/// total.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PlaceProvenance {
    /// An MIR local register (`Place::Local`).
    Local(u32),
    /// The function return slot (`Place::ReturnSlot`).
    ReturnSlot,
    /// A function parameter slot, by declaration index.
    Param(u32),
    /// The owner is a runtime handle, carried as the backing local id + role.
    Handle {
        /// Backing MIR local id the handle place addresses.
        local: u32,
        /// Which handle family (Duplex / lambda-actor / actor / half).
        role: HandleRole,
    },
    /// The place is known to exist but not yet threaded through (scaffold
    /// sentinel; Phase 1 replaces with the concrete place).
    Unanchored,
}

/// Handle family for [`PlaceProvenance::Handle`], mirroring the handle `Place`
/// variants.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HandleRole {
    /// `Place::DuplexHandle`.
    Duplex,
    /// `Place::LambdaActorHandle`.
    LambdaActor,
    /// `Place::ActorHandle`.
    Actor,
    /// `Place::SendHalf`.
    SendHalf,
    /// `Place::RecvHalf`.
    RecvHalf,
}

/// Where a value's ownership fact originates.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ProvenanceOrigin {
    /// A freshly produced owned value (call result, literal, constructor) — it
    /// owns itself; no upstream owner.
    Fresh,
    /// A read / borrow of an existing place.
    Read,
    /// A function parameter — owned by the caller's frame.
    Parameter,
}

/// One projection step from a root place to an interior value — lets an
/// [`InteriorAlias`](OwnershipDecision::InteriorAlias) name exactly which
/// sub-location it views.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Projection {
    /// Record / tuple field by ordinal.
    Field(u32),
    /// Array / slice element by index.
    Element(u64),
    /// Enum variant payload field.
    VariantPayload {
        /// Variant ordinal (declaration order).
        variant: u32,
        /// Payload field ordinal within the variant.
        field: u32,
    },
}

/// A value's full provenance: an origin tag, the root [`PlaceProvenance`] its
/// ownership traces to, and the projection path from that root to this value.
/// Carried by [`Borrowed`](OwnershipDecision::Borrowed) and
/// [`InteriorAlias`](OwnershipDecision::InteriorAlias) so the owner is named on
/// the value rather than re-derived.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValueProvenance {
    /// How this value's ownership arose.
    pub origin: ProvenanceOrigin,
    /// The root location the value's ownership traces to.
    pub root: PlaceProvenance,
    /// Projection steps from `root` to this value (empty = the root itself).
    pub path: Vec<Projection>,
}

impl ValueProvenance {
    /// A freshly produced, self-owning value.
    #[must_use]
    pub fn fresh() -> Self {
        Self {
            origin: ProvenanceOrigin::Fresh,
            root: PlaceProvenance::Unanchored,
            path: Vec::new(),
        }
    }

    /// A read / borrow of `place` (no projection).
    #[must_use]
    pub fn of_place(place: PlaceProvenance) -> Self {
        Self {
            origin: ProvenanceOrigin::Read,
            root: place,
            path: Vec::new(),
        }
    }

    /// A function parameter slot.
    #[must_use]
    pub fn param(index: u32) -> Self {
        Self {
            origin: ProvenanceOrigin::Parameter,
            root: PlaceProvenance::Param(index),
            path: Vec::new(),
        }
    }

    /// An interior projection `path` of `root` (a still-live owner).
    #[must_use]
    pub fn projection(root: PlaceProvenance, path: Vec<Projection>) -> Self {
        Self {
            origin: ProvenanceOrigin::Read,
            root,
            path,
        }
    }

    /// Provenance known to exist but not yet threaded (scaffold default).
    #[must_use]
    pub fn unanchored() -> Self {
        Self::of_place(PlaceProvenance::Unanchored)
    }
}

impl From<Place> for PlaceProvenance {
    fn from(place: Place) -> Self {
        match place {
            // A bare local, or a machine/enum *tag* sub-location: the ownership
            // root is the local holding the value.
            Place::Local(n) | Place::MachineTag(n) | Place::EnumTag(n) => PlaceProvenance::Local(n),
            Place::ReturnSlot => PlaceProvenance::ReturnSlot,
            Place::DuplexHandle(n) => PlaceProvenance::Handle {
                local: n,
                role: HandleRole::Duplex,
            },
            Place::LambdaActorHandle(n) => PlaceProvenance::Handle {
                local: n,
                role: HandleRole::LambdaActor,
            },
            Place::ActorHandle(n) => PlaceProvenance::Handle {
                local: n,
                role: HandleRole::Actor,
            },
            Place::SendHalf(n) => PlaceProvenance::Handle {
                local: n,
                role: HandleRole::SendHalf,
            },
            Place::RecvHalf(n) => PlaceProvenance::Handle {
                local: n,
                role: HandleRole::RecvHalf,
            },
            // Machine / enum *payload* places are sub-locations of a value held
            // in `local`; their ownership root is that local.
            Place::MachineVariant { local, .. } | Place::EnumVariant { local, .. } => {
                PlaceProvenance::Local(local)
            }
        }
    }
}

// ─────────────────────────── classify context + scaffold ─────────────────

/// Layout + value-class registries the [`OwnershipDecision::classify`] scaffold
/// reads, mirroring exactly what the MIR `Builder` holds. Phase 1 wires
/// `classify` by building one of these from the `Builder` — no new registry is
/// introduced.
#[derive(Debug)]
pub struct OwnershipCtx<'a, S = RandomState> {
    /// Record name → ordered `(field_name, ty)` pairs (the `Builder`'s
    /// `record_field_orders`).
    pub record_field_orders: &'a HashMap<String, Vec<(String, ResolvedTy)>, S>,
    /// Enum / machine variant layouts (the `Builder`'s
    /// `classification_enum_layouts` view) — carries `is_indirect`.
    pub enum_layouts: &'a [EnumLayout],
    /// `#[resource]` / `#[linear]` / `BitCopy` markers (the value-class seed).
    pub type_classes: &'a TypeClassTable,
}

impl<'a, S: BuildHasher> OwnershipCtx<'a, S> {
    /// Bundle the three registries into a classify context.
    #[must_use]
    pub fn new(
        record_field_orders: &'a HashMap<String, Vec<(String, ResolvedTy)>, S>,
        enum_layouts: &'a [EnumLayout],
        type_classes: &'a TypeClassTable,
    ) -> Self {
        Self {
            record_field_orders,
            enum_layouts,
            type_classes,
        }
    }

    /// The record-aware [`HeapOwnershipLayouts`] adapter over this context — the
    /// same adapter the MIR drop derivations use, so `classify` builds the
    /// heap-owning answer **from** the single `ty_owns_heap` authority rather
    /// than reinventing the recursion.
    fn heap_layouts(&self) -> MirHeapLayouts<'a, S> {
        MirHeapLayouts {
            record_field_orders: self.record_field_orders,
            enum_layouts: self.enum_layouts,
        }
    }
}

impl OwnershipDecision {
    /// Classify the ownership/drop/ABI decision for a value of type `ty` held in
    /// `place`.
    ///
    /// **Phase-0 scaffold: lands unused.** It is the constructor every Phase-1
    /// call site will route through; the coarse arms (e.g. picking the plain
    /// `Vec` release symbol, deferring `dyn Trait` storage) are refined in
    /// Phase 1. It is **total** in the load-bearing sense: the type-driven match
    /// is exhaustive over [`ResolvedTy`] (no `_ => …` fallthrough), so a new
    /// `ResolvedTy` variant is a compile error here, and every shape resolves to
    /// a non-`Unsupported` decision or to a *tracked* [`Unsupported`] naming a
    /// [`FailClosedReason`].
    ///
    /// [`Unsupported`]: OwnershipDecision::Unsupported
    #[must_use]
    pub fn classify<S: BuildHasher>(
        ty: &ResolvedTy,
        place: Place,
        ctx: &OwnershipCtx<'_, S>,
    ) -> Self {
        // 1. Place-driven handle dispatch — mirrors `drop_kind_for`'s Place
        //    arms, where the release protocol is decided by the handle Place,
        //    NOT the surface type (a lambda-actor handle carries
        //    `Named{Duplex}` but releases via `hew_lambda_actor_release`).
        if let Some(decision) = Self::classify_handle_place(place) {
            return decision;
        }

        let layouts = ctx.heap_layouts();
        // 2. Type-driven dispatch — EXHAUSTIVE over `ResolvedTy`. No `_` arm:
        //    adding a variant must not compile until it is classified here.
        match ty {
            // Scalars, zero-sized, the monotonic-instant handle, and a bare
            // function pointer (owns no environment): no heap, no drop.
            ResolvedTy::I8
            | ResolvedTy::I16
            | ResolvedTy::I32
            | ResolvedTy::I64
            | ResolvedTy::U8
            | ResolvedTy::U16
            | ResolvedTy::U32
            | ResolvedTy::U64
            | ResolvedTy::Isize
            | ResolvedTy::Usize
            | ResolvedTy::F32
            | ResolvedTy::F64
            | ResolvedTy::Bool
            | ResolvedTy::Char
            | ResolvedTy::Duration
            | ResolvedTy::Unit
            | ResolvedTy::Never
            | ResolvedTy::Function { .. } => OwnershipDecision::NoHeap,

            ResolvedTy::String => HeapLeaf::String.decision(),
            ResolvedTy::Bytes => HeapLeaf::Bytes.decision(),
            ResolvedTy::CancellationToken => HeapLeaf::CancellationToken.decision(),

            // Inline composites: heap-owning iff a member is (the authority
            // answers; we build on it, never re-walk).
            ResolvedTy::Tuple(_) => {
                if ty_owns_heap(ty, &layouts) {
                    OwnershipDecision::OwnsHeap {
                        drop: DropClass::TupleInPlace,
                        abi: AbiClass::Inline,
                        layout: LayoutClass::Product,
                    }
                } else {
                    OwnershipDecision::NoHeap
                }
            }
            ResolvedTy::Array(_, _) => {
                if ty_owns_heap(ty, &layouts) {
                    OwnershipDecision::OwnsHeap {
                        drop: DropClass::AggregateRecursive,
                        abi: AbiClass::Inline,
                        layout: LayoutClass::Product,
                    }
                } else {
                    OwnershipDecision::NoHeap
                }
            }

            // Non-owning views.
            ResolvedTy::Slice(_) | ResolvedTy::Borrow { .. } | ResolvedTy::Pointer { .. } => {
                OwnershipDecision::Borrowed {
                    provenance: ValueProvenance::of_place(PlaceProvenance::from(place)),
                }
            }

            // A closure owns a heap env box iff it captures a heap-owning value.
            ResolvedTy::Closure { captures, .. } => {
                if captures.iter().any(|c| ty_owns_heap(c, &layouts)) {
                    OwnershipDecision::OwnsHeap {
                        drop: DropClass::ClosurePair,
                        abi: AbiClass::FatPointer,
                        layout: LayoutClass::ClosureEnv,
                    }
                } else {
                    OwnershipDecision::NoHeap
                }
            }

            // `dyn Trait` release needs the `FrameOwned`/`HeapBoxed` storage hint
            // the scaffold does not carry — tracked-Unsupported (Phase 1 threads
            // it).
            ResolvedTy::TraitObject { .. } => OwnershipDecision::Unsupported {
                reason: FailClosedReason::DynStorageUnresolved,
            },

            // A `Task<T>` fork handle is consume-once `Linear` (no implicit drop).
            ResolvedTy::Task(_) => OwnershipDecision::Unsupported {
                reason: FailClosedReason::LinearConsumeOnce,
            },

            // An unsubstituted type parameter's ownership is genuinely unknown
            // until monomorphisation — the same fail-closed boundary as
            // `ValueClass::Unknown → Strategy::UnknownBlocked`.
            ResolvedTy::TypeParam { .. } => OwnershipDecision::Unsupported {
                reason: FailClosedReason::UnknownValueClass,
            },

            ResolvedTy::Named {
                name,
                args,
                builtin,
                ..
            } => classify_named(ty, name, args, *builtin, place, ctx),
        }
    }

    /// Place-driven handle dispatch — the first step of [`classify`]. Returns
    /// `Some` when the release protocol is fixed by the handle `Place` itself
    /// (duplex / lambda-actor / channel half), and `None` for every other
    /// `Place`, which [`classify`] then discriminates by type.
    ///
    /// [`classify`]: OwnershipDecision::classify
    fn classify_handle_place(place: Place) -> Option<Self> {
        match place {
            Place::DuplexHandle(_) => Some(Self::owns_handle(DropClass::DuplexClose)),
            Place::LambdaActorHandle(_) => Some(Self::owns_handle(DropClass::LambdaActorRelease)),
            Place::SendHalf(_) => Some(Self::owns_handle(DropClass::DuplexHalfClose {
                direction: Direction::Send,
            })),
            Place::RecvHalf(_) => Some(Self::owns_handle(DropClass::DuplexHalfClose {
                direction: Direction::Recv,
            })),
            // Every other Place is type-discriminated by the caller.
            Place::Local(_)
            | Place::ReturnSlot
            | Place::ActorHandle(_)
            | Place::MachineTag(_)
            | Place::MachineVariant { .. }
            | Place::EnumTag(_)
            | Place::EnumVariant { .. } => None,
        }
    }

    /// Construct an [`OwnsHeap`](OwnershipDecision::OwnsHeap) for a single owned
    /// pointer-width handle released by `drop`.
    fn owns_handle(drop: DropClass) -> Self {
        OwnershipDecision::OwnsHeap {
            drop,
            abi: AbiClass::OwnedHandle,
            layout: LayoutClass::HeapBuffer,
        }
    }
}

/// Classify a `Named { name, args, builtin }` value. Dispatch order: the typed
/// `builtin` heap-leaf set first (never the name string), then the user
/// record/enum layout registries, then the value-class marker — failing closed
/// (tracked [`Unsupported`](OwnershipDecision::Unsupported)) rather than
/// defaulting a heap-bearing `Named` to `NoHeap`.
fn classify_named<S: BuildHasher>(
    ty: &ResolvedTy,
    name: &str,
    args: &[ResolvedTy],
    builtin: Option<BuiltinType>,
    place: Place,
    ctx: &OwnershipCtx<'_, S>,
) -> OwnershipDecision {
    // 1. Builtin heap-leaf handles (typed discriminator, not the name string).
    if let Some(b) = builtin {
        match b {
            BuiltinType::Vec => return HeapLeaf::Vec.decision(),
            BuiltinType::HashMap => return HeapLeaf::HashMap.decision(),
            BuiltinType::HashSet => return HeapLeaf::HashSet.decision(),
            BuiltinType::Generator | BuiltinType::AsyncGenerator => {
                return HeapLeaf::Generator.decision()
            }
            // `instant` lowers ABI-identically to `i64`.
            BuiltinType::Instant => return OwnershipDecision::NoHeap,
            // A fork handle is consume-once `Linear`.
            BuiltinType::Task => {
                return OwnershipDecision::Unsupported {
                    reason: FailClosedReason::LinearConsumeOnce,
                }
            }
            // Every other builtin (Option/Result/Duplex/Pid/Stream/…) is
            // resolved by its layout or value-class below.
            _ => {}
        }
    }

    let layouts = ctx.heap_layouts();

    // 2. User record layout → in-place record drop iff it owns heap.
    if layouts.record_field_tys(name, args).is_some() {
        return if ty_owns_heap(ty, &layouts) {
            OwnershipDecision::OwnsHeap {
                drop: DropClass::RecordInPlace,
                abi: AbiClass::Inline,
                layout: LayoutClass::Product,
            }
        } else {
            OwnershipDecision::NoHeap
        };
    }

    // 3. Enum layout (user enum / Option / Result). An `indirect enum` value is
    //    always a heap node pointer; an inline enum is heap-owning iff a variant
    //    payload is.
    if let Some(enum_layout) = find_enum_layout(name, args, ctx.enum_layouts) {
        if enum_layout.is_indirect {
            return OwnershipDecision::OwnsHeap {
                drop: DropClass::IndirectEnum,
                abi: AbiClass::OwnedHandle,
                layout: LayoutClass::IndirectNode,
            };
        }
        return if ty_owns_heap(ty, &layouts) {
            OwnershipDecision::OwnsHeap {
                drop: DropClass::EnumInPlace,
                abi: AbiClass::Inline,
                layout: LayoutClass::TaggedUnion,
            }
        } else {
            OwnershipDecision::NoHeap
        };
    }

    // 4. Value-class marker (`@resource` / `@linear` / `BitCopy` / unmarked).
    match ValueClass::of_ty(ty, ctx.type_classes) {
        ValueClass::BitCopy => OwnershipDecision::NoHeap,
        ValueClass::AffineResource => OwnershipDecision::owns_handle(DropClass::Resource),
        ValueClass::Linear => OwnershipDecision::Unsupported {
            reason: FailClosedReason::LinearConsumeOnce,
        },
        ValueClass::View => OwnershipDecision::Borrowed {
            provenance: ValueProvenance::of_place(PlaceProvenance::from(place)),
        },
        // A `Named` classified `CowValue`/`PersistentShare` with no record/enum
        // layout reached here has no release protocol the scaffold can name
        // (the collection/closure/dyn forms are handled by their own arms
        // above) — fail closed.
        ValueClass::CowValue | ValueClass::PersistentShare => OwnershipDecision::Unsupported {
            reason: FailClosedReason::NoReleaseProtocol,
        },
        ValueClass::Unknown => OwnershipDecision::Unsupported {
            reason: FailClosedReason::UnknownValueClass,
        },
    }
}

// ───────────────────── conversions: subsume / project to seeds ────────────

impl TryFrom<DropKind> for DropClass {
    /// Failure means the [`DropKind::CowHeap`] symbol is outside the recognised
    /// leaf set — a fail-closed event, never a silent mis-map.
    type Error = FailClosedReason;

    fn try_from(kind: DropKind) -> Result<Self, Self::Error> {
        Ok(match kind {
            DropKind::Resource => DropClass::Resource,
            DropKind::DuplexClose => DropClass::DuplexClose,
            DropKind::DuplexHalfClose(direction) => DropClass::DuplexHalfClose { direction },
            DropKind::LambdaActorRelease => DropClass::LambdaActorRelease,
            DropKind::TraitObject { storage } => DropClass::DynTrait { storage },
            DropKind::CowHeap { drop_fn } => {
                let leaf = HeapLeaf::from_release_symbol(drop_fn)
                    .ok_or(FailClosedReason::UnrecognisedReleaseSymbol)?;
                DropClass::CowHeapLeaf { leaf }
            }
            DropKind::RecordInPlace => DropClass::RecordInPlace,
            DropKind::AggregateRecursive => DropClass::AggregateRecursive,
            DropKind::EnumInPlace => DropClass::EnumInPlace,
            DropKind::TupleInPlace => DropClass::TupleInPlace,
            DropKind::ClosurePair => DropClass::ClosurePair,
            DropKind::IndirectEnum => DropClass::IndirectEnum,
        })
    }
}

impl DropClass {
    /// The canonical [`DropKind`] this class lowers to — the reverse of
    /// `TryFrom<DropKind>`, total over every `DropClass`. For
    /// [`DropClass::CowHeapLeaf`] the *plain* family symbol is chosen
    /// (`Vec → hew_vec_free`); the owned/closure-pair element refinement is
    /// resolved separately from the element's own decision, so a
    /// `DropKind::CowHeap { hew_vec_free_owned }` round-trips to the family, not
    /// the byte-identical symbol.
    #[must_use]
    pub fn canonical_drop_kind(self) -> DropKind {
        match self {
            DropClass::Resource => DropKind::Resource,
            DropClass::CowHeapLeaf { leaf } => DropKind::CowHeap {
                drop_fn: leaf.release_symbol(),
            },
            DropClass::RecordInPlace => DropKind::RecordInPlace,
            DropClass::TupleInPlace => DropKind::TupleInPlace,
            DropClass::EnumInPlace => DropKind::EnumInPlace,
            DropClass::AggregateRecursive => DropKind::AggregateRecursive,
            DropClass::IndirectEnum => DropKind::IndirectEnum,
            DropClass::ClosurePair => DropKind::ClosurePair,
            DropClass::DynTrait { storage } => DropKind::TraitObject { storage },
            DropClass::DuplexClose => DropKind::DuplexClose,
            DropClass::DuplexHalfClose { direction } => DropKind::DuplexHalfClose(direction),
            DropClass::LambdaActorRelease => DropKind::LambdaActorRelease,
        }
    }
}

impl HeapLeaf {
    /// The canonical C-ABI runtime release symbol for this leaf.
    #[must_use]
    pub fn release_symbol(self) -> &'static str {
        match self {
            HeapLeaf::String => "hew_string_drop",
            HeapLeaf::Bytes => "hew_bytes_drop",
            HeapLeaf::Vec => "hew_vec_free",
            HeapLeaf::HashMap => "hew_hashmap_free_layout",
            HeapLeaf::HashSet => "hew_hashset_free_layout",
            HeapLeaf::Generator => "hew_gen_coro_destroy",
            HeapLeaf::CancellationToken => "hew_cancel_token_release",
        }
    }

    /// Parse a [`DropKind::CowHeap`] release symbol back into its leaf. Returns
    /// `None` for an unrecognised symbol (fail-closed). The `CancellationToken`
    /// release is intentionally absent — it is an `@resource` close, never a
    /// `CowHeap` symbol — so this never yields a non-copy-on-write leaf.
    #[must_use]
    pub fn from_release_symbol(symbol: &str) -> Option<Self> {
        Some(match symbol {
            "hew_string_drop" => HeapLeaf::String,
            "hew_bytes_drop" => HeapLeaf::Bytes,
            // The plain / owned-element / closure-pair Vec releases all share the
            // `Vec` leaf family; the element refinement is carried separately.
            "hew_vec_free" | "hew_vec_free_owned" | "hew_vec_free_closure_pairs" => HeapLeaf::Vec,
            "hew_hashmap_free_layout" => HeapLeaf::HashMap,
            "hew_hashset_free_layout" => HeapLeaf::HashSet,
            "hew_gen_coro_destroy" => HeapLeaf::Generator,
            _ => return None,
        })
    }

    /// The release protocol this leaf drops through. `CancellationToken` is an
    /// `@resource`-class close ([`DropClass::Resource`]); every other leaf is a
    /// copy-on-write free ([`DropClass::CowHeapLeaf`]). This is the single
    /// constructor of `CowHeapLeaf`, so its `leaf` is never `CancellationToken`.
    #[must_use]
    pub fn drop_class(self) -> DropClass {
        match self {
            HeapLeaf::CancellationToken => DropClass::Resource,
            HeapLeaf::String
            | HeapLeaf::Bytes
            | HeapLeaf::Vec
            | HeapLeaf::HashMap
            | HeapLeaf::HashSet
            | HeapLeaf::Generator => DropClass::CowHeapLeaf { leaf: self },
        }
    }

    /// How this leaf crosses the ABI boundary.
    #[must_use]
    pub fn abi_class(self) -> AbiClass {
        match self {
            HeapLeaf::Bytes => AbiClass::BytesTriple,
            HeapLeaf::String
            | HeapLeaf::Vec
            | HeapLeaf::HashMap
            | HeapLeaf::HashSet
            | HeapLeaf::Generator
            | HeapLeaf::CancellationToken => AbiClass::OwnedHandle,
        }
    }

    /// The full [`OwnsHeap`](OwnershipDecision::OwnsHeap) decision for a value
    /// that *is* this leaf.
    #[must_use]
    pub fn decision(self) -> OwnershipDecision {
        OwnershipDecision::OwnsHeap {
            drop: self.drop_class(),
            abi: self.abi_class(),
            layout: LayoutClass::HeapBuffer,
        }
    }
}

impl OwnershipDecision {
    /// Project back to the structural `ty_owns_heap` boolean: `true` exactly for
    /// [`OwnsHeap`](OwnershipDecision::OwnsHeap).
    #[must_use]
    pub fn owns_heap(&self) -> bool {
        matches!(self, OwnershipDecision::OwnsHeap { .. })
    }

    /// The [`DropClass`] of an owning decision, if any.
    #[must_use]
    pub fn drop_class(&self) -> Option<DropClass> {
        match self {
            OwnershipDecision::OwnsHeap { drop, .. } => Some(*drop),
            OwnershipDecision::NoHeap
            | OwnershipDecision::Borrowed { .. }
            | OwnershipDecision::InteriorAlias { .. }
            | OwnershipDecision::Unsupported { .. } => None,
        }
    }

    /// Project back to the coarse [`ValueClass`] this decision implies — proves
    /// the decision subsumes the value-class seed (the coarser fact is
    /// recoverable).
    #[must_use]
    pub fn to_value_class(&self) -> ValueClass {
        match self {
            OwnershipDecision::NoHeap => ValueClass::BitCopy,
            OwnershipDecision::OwnsHeap { drop, .. } => match drop {
                DropClass::Resource
                | DropClass::DuplexClose
                | DropClass::DuplexHalfClose { .. }
                | DropClass::LambdaActorRelease => ValueClass::AffineResource,
                DropClass::DynTrait { .. } | DropClass::ClosurePair => ValueClass::PersistentShare,
                DropClass::CowHeapLeaf { .. }
                | DropClass::RecordInPlace
                | DropClass::TupleInPlace
                | DropClass::EnumInPlace
                | DropClass::AggregateRecursive
                | DropClass::IndirectEnum => ValueClass::CowValue,
            },
            OwnershipDecision::Borrowed { .. } | OwnershipDecision::InteriorAlias { .. } => {
                ValueClass::View
            }
            OwnershipDecision::Unsupported { reason } => match reason {
                FailClosedReason::LinearConsumeOnce => ValueClass::Linear,
                FailClosedReason::UnknownValueClass
                | FailClosedReason::UnenumeratedShape
                | FailClosedReason::NoReleaseProtocol
                | FailClosedReason::UnrecognisedReleaseSymbol
                | FailClosedReason::DynStorageUnresolved
                | FailClosedReason::UnprovenSoleOwner => ValueClass::Unknown,
            },
        }
    }

    /// The [`FailClosedReason`] of a tracked-`Unsupported` decision, if any.
    #[must_use]
    pub fn fail_closed_reason(&self) -> Option<FailClosedReason> {
        match self {
            OwnershipDecision::Unsupported { reason } => Some(*reason),
            OwnershipDecision::NoHeap
            | OwnershipDecision::OwnsHeap { .. }
            | OwnershipDecision::Borrowed { .. }
            | OwnershipDecision::InteriorAlias { .. } => None,
        }
    }

    /// The move strategy an [`Unsupported`](OwnershipDecision::Unsupported)
    /// decision floors to — [`Strategy::UnknownBlocked`], the existing
    /// fail-closed MIR-boundary strategy. `None` for every decided decision
    /// (their strategy is chosen by the normal intent/value-class path).
    #[must_use]
    pub fn move_strategy_floor(&self) -> Option<Strategy> {
        match self {
            OwnershipDecision::Unsupported { .. } => Some(Strategy::UnknownBlocked),
            OwnershipDecision::NoHeap
            | OwnershipDecision::OwnsHeap { .. }
            | OwnershipDecision::Borrowed { .. }
            | OwnershipDecision::InteriorAlias { .. } => None,
        }
    }
}

impl FailClosedReason {
    /// The actionable diagnostic note Phase 1 attaches when this reason reaches
    /// the `MirCheck::DropPlanUndetermined` / `Strategy::UnknownBlocked` path.
    #[must_use]
    pub fn diagnostic_note(self) -> &'static str {
        match self {
            FailClosedReason::UnknownValueClass => {
                "ownership decision: value-class is Unknown (unmarked Named or unsubstituted \
                 type parameter); routed through Strategy::UnknownBlocked at the MIR boundary"
            }
            FailClosedReason::UnenumeratedShape => {
                "ownership decision: ResolvedTy shape is not enumerated by the classifier — a \
                 new type variant must add an explicit ownership arm (fail-closed, never a \
                 silent non-owning default)"
            }
            FailClosedReason::NoReleaseProtocol => {
                "ownership decision: value owns heap but no release protocol could be named — \
                 surfaced as DropPlanUndetermined rather than leaking"
            }
            FailClosedReason::UnrecognisedReleaseSymbol => {
                "ownership decision: CoW-heap release symbol is outside the recognised leaf set \
                 (fail-closed on a future or mistyped runtime symbol)"
            }
            FailClosedReason::LinearConsumeOnce => {
                "ownership decision: value is consume-once Linear (Task / @linear) with no \
                 implicit scope-exit drop — release is enforced by the move-checker's \
                 MustConsume, not a drop"
            }
            FailClosedReason::DynStorageUnresolved => {
                "ownership decision: dyn Trait storage (FrameOwned vs HeapBoxed) is not \
                 available — the dyn_trait_storage side table must be threaded before the \
                 release ritual is selected"
            }
            FailClosedReason::UnprovenSoleOwner => {
                "ownership decision: heap-owning aggregate whose exactly-once free cannot be \
                 proven (owned-handle aggregate extraction) — refused rather than risk a \
                 double-free"
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::MachineVariantLayout;

    // ── fixtures ──────────────────────────────────────────────────────────

    fn vec_i64() -> ResolvedTy {
        ResolvedTy::named_builtin("Vec", BuiltinType::Vec, vec![ResolvedTy::I64])
    }

    fn hashmap_str_i64() -> ResolvedTy {
        ResolvedTy::named_builtin(
            "HashMap",
            BuiltinType::HashMap,
            vec![ResolvedTy::String, ResolvedTy::I64],
        )
    }

    fn hashset_i64() -> ResolvedTy {
        ResolvedTy::named_builtin("HashSet", BuiltinType::HashSet, vec![ResolvedTy::I64])
    }

    fn tuple_str_i64() -> ResolvedTy {
        ResolvedTy::Tuple(vec![ResolvedTy::String, ResolvedTy::I64])
    }

    fn closure_capturing_string() -> ResolvedTy {
        ResolvedTy::Closure {
            params: vec![],
            ret: Box::new(ResolvedTy::Unit),
            captures: vec![ResolvedTy::String],
        }
    }

    fn duplex_i64() -> ResolvedTy {
        ResolvedTy::named_builtin(
            "Duplex",
            BuiltinType::Duplex,
            vec![ResolvedTy::I64, ResolvedTy::I64],
        )
    }

    fn empty_records() -> HashMap<String, Vec<(String, ResolvedTy)>> {
        HashMap::new()
    }

    /// `type Boxed { payload: Vec<i64> }` — a heap-owning user record.
    fn boxed_record_orders() -> HashMap<String, Vec<(String, ResolvedTy)>> {
        let mut orders = HashMap::new();
        orders.insert(
            "Boxed".to_string(),
            vec![("payload".to_string(), vec_i64())],
        );
        orders
    }

    /// `type Msg { Text(string); Ping }` — a heap-owning inline enum.
    fn msg_enum_layouts() -> Vec<EnumLayout> {
        vec![EnumLayout {
            name: "Msg".to_string(),
            tag_width: 1,
            variants: vec![
                MachineVariantLayout {
                    name: "Text".to_string(),
                    field_tys: vec![ResolvedTy::String],
                    field_names: vec![],
                },
                MachineVariantLayout {
                    name: "Ping".to_string(),
                    field_tys: vec![],
                    field_names: vec![],
                },
            ],
            is_indirect: false,
        }]
    }

    /// `indirect enum Tree { Leaf(i64); Node(Tree, Tree) }` — a heap node even
    /// though no payload is itself a heap leaf.
    fn tree_enum_layouts() -> Vec<EnumLayout> {
        vec![EnumLayout {
            name: "Tree".to_string(),
            tag_width: 1,
            variants: vec![
                MachineVariantLayout {
                    name: "Leaf".to_string(),
                    field_tys: vec![ResolvedTy::I64],
                    field_names: vec![],
                },
                MachineVariantLayout {
                    name: "Node".to_string(),
                    field_tys: vec![
                        ResolvedTy::named_user("Tree", vec![]),
                        ResolvedTy::named_user("Tree", vec![]),
                    ],
                    field_names: vec![],
                },
            ],
            is_indirect: true,
        }]
    }

    fn ctx<'a>(
        records: &'a HashMap<String, Vec<(String, ResolvedTy)>>,
        enums: &'a [EnumLayout],
        classes: &'a TypeClassTable,
    ) -> OwnershipCtx<'a> {
        OwnershipCtx::new(records, enums, classes)
    }

    // ── the load-bearing guard: totality over the 12 shapes ────────────────

    /// The twelve canonical value shapes, each classified through the scaffold.
    /// Returned owned (the per-case layout/class registries stay local), so the
    /// totality guard and the exact-decision pins share one shape table. Every
    /// shape here resolves to a decided decision; the tracked-`Unsupported`
    /// modes are the explicit complement in
    /// `tracked_unsupported_shapes_carry_a_reason`.
    fn twelve_shape_decisions() -> Vec<(&'static str, OwnershipDecision)> {
        fn decide(
            ty: &ResolvedTy,
            place: Place,
            recs: &HashMap<String, Vec<(String, ResolvedTy)>>,
            enums: &[EnumLayout],
        ) -> OwnershipDecision {
            let classes = TypeClassTable::new();
            OwnershipDecision::classify(ty, place, &ctx(recs, enums, &classes))
        }

        let records = boxed_record_orders();
        let msg = msg_enum_layouts();
        let tree = tree_enum_layouts();
        let no_records = empty_records();

        vec![
            // 1. scalar
            (
                "scalar",
                decide(&ResolvedTy::I64, Place::Local(0), &no_records, &[]),
            ),
            // 2. string
            (
                "string",
                decide(&ResolvedTy::String, Place::Local(0), &no_records, &[]),
            ),
            // 3. Vec
            ("vec", decide(&vec_i64(), Place::Local(0), &no_records, &[])),
            // 4. HashMap / HashSet
            (
                "hashmap",
                decide(&hashmap_str_i64(), Place::Local(0), &no_records, &[]),
            ),
            (
                "hashset",
                decide(&hashset_i64(), Place::Local(0), &no_records, &[]),
            ),
            // 5. record
            (
                "record",
                decide(
                    &ResolvedTy::named_user("Boxed", vec![]),
                    Place::Local(0),
                    &records,
                    &[],
                ),
            ),
            // 6. tuple
            (
                "tuple",
                decide(&tuple_str_i64(), Place::Local(0), &no_records, &[]),
            ),
            // 7. enum (inline, heap-owning)
            (
                "enum",
                decide(
                    &ResolvedTy::named_user("Msg", vec![]),
                    Place::Local(0),
                    &no_records,
                    &msg,
                ),
            ),
            // 8. indirect-enum
            (
                "indirect-enum",
                decide(
                    &ResolvedTy::named_user("Tree", vec![]),
                    Place::Local(0),
                    &no_records,
                    &tree,
                ),
            ),
            // 9. closure-env (heap-owning capture)
            (
                "closure-env",
                decide(
                    &closure_capturing_string(),
                    Place::Local(0),
                    &no_records,
                    &[],
                ),
            ),
            // 10. duplex-half
            (
                "duplex-half",
                decide(&duplex_i64(), Place::SendHalf(0), &no_records, &[]),
            ),
            // 11. handle (CancellationToken)
            (
                "handle",
                decide(
                    &ResolvedTy::CancellationToken,
                    Place::Local(0),
                    &no_records,
                    &[],
                ),
            ),
            // 12. bytes
            (
                "bytes",
                decide(&ResolvedTy::Bytes, Place::Local(0), &no_records, &[]),
            ),
        ]
    }

    /// Enumerate every value shape the unification must be total over and assert
    /// each maps to a decided decision — never a silent fallthrough. The tracked
    /// `Unsupported` modes are asserted separately
    /// (`tracked_unsupported_shapes_carry_a_reason`); none of these twelve may be
    /// `Unsupported`.
    #[test]
    fn classify_is_total_over_the_twelve_shapes() {
        for (label, decision) in twelve_shape_decisions() {
            assert!(
                !matches!(decision, OwnershipDecision::Unsupported { .. }),
                "shape `{label}` resolved to {decision:?}; every one of the twelve \
                 shapes must be a decided (non-Unsupported) decision",
            );
        }
    }

    /// Pin the exact decision per shape so a drift in the classifier is a test
    /// failure, not a silent reclassification.
    #[test]
    fn classify_pins_exact_decision_per_shape() {
        let decisions = twelve_shape_decisions();
        let by_label = |want: &str| {
            decisions
                .iter()
                .find(|(l, _)| *l == want)
                .map(|(_, d)| d.clone())
                .unwrap()
        };

        assert_eq!(by_label("scalar"), OwnershipDecision::NoHeap);
        assert_eq!(by_label("string"), HeapLeaf::String.decision());
        assert_eq!(by_label("bytes"), HeapLeaf::Bytes.decision());
        assert_eq!(by_label("vec"), HeapLeaf::Vec.decision());
        assert_eq!(by_label("hashmap"), HeapLeaf::HashMap.decision());
        assert_eq!(by_label("hashset"), HeapLeaf::HashSet.decision());
        assert_eq!(by_label("handle"), HeapLeaf::CancellationToken.decision());
        assert_eq!(
            by_label("record"),
            OwnershipDecision::OwnsHeap {
                drop: DropClass::RecordInPlace,
                abi: AbiClass::Inline,
                layout: LayoutClass::Product,
            }
        );
        assert_eq!(
            by_label("tuple"),
            OwnershipDecision::OwnsHeap {
                drop: DropClass::TupleInPlace,
                abi: AbiClass::Inline,
                layout: LayoutClass::Product,
            }
        );
        assert_eq!(
            by_label("enum"),
            OwnershipDecision::OwnsHeap {
                drop: DropClass::EnumInPlace,
                abi: AbiClass::Inline,
                layout: LayoutClass::TaggedUnion,
            }
        );
        assert_eq!(
            by_label("indirect-enum"),
            OwnershipDecision::OwnsHeap {
                drop: DropClass::IndirectEnum,
                abi: AbiClass::OwnedHandle,
                layout: LayoutClass::IndirectNode,
            }
        );
        assert_eq!(
            by_label("closure-env"),
            OwnershipDecision::OwnsHeap {
                drop: DropClass::ClosurePair,
                abi: AbiClass::FatPointer,
                layout: LayoutClass::ClosureEnv,
            }
        );
        assert_eq!(
            by_label("duplex-half"),
            OwnershipDecision::OwnsHeap {
                drop: DropClass::DuplexHalfClose {
                    direction: Direction::Send,
                },
                abi: AbiClass::OwnedHandle,
                layout: LayoutClass::HeapBuffer,
            }
        );
    }

    /// The shapes that are *tracked* `Unsupported` in Phase 0: each carries a
    /// reason (no silent `_ => false`), and each names a real ownership mode the
    /// 5-variant decision does not model yet. This is the explicit complement to
    /// the totality test — the only legal way to be `Unsupported`.
    #[test]
    fn tracked_unsupported_shapes_carry_a_reason() {
        let no_records = empty_records();
        let classes = TypeClassTable::new();
        let ctx = ctx(&no_records, &[], &classes);

        // Task<T> — consume-once Linear.
        assert_eq!(
            OwnershipDecision::classify(
                &ResolvedTy::Task(Box::new(ResolvedTy::I64)),
                Place::Local(0),
                &ctx,
            )
            .fail_closed_reason(),
            Some(FailClosedReason::LinearConsumeOnce),
        );

        // dyn Trait — storage hint not available to the scaffold.
        assert_eq!(
            OwnershipDecision::classify(
                &ResolvedTy::TraitObject { traits: vec![] },
                Place::Local(0),
                &ctx,
            )
            .fail_closed_reason(),
            Some(FailClosedReason::DynStorageUnresolved),
        );

        // An unsubstituted type parameter — genuinely unknown.
        assert_eq!(
            OwnershipDecision::classify(
                &ResolvedTy::TypeParam {
                    name: "T".to_string(),
                },
                Place::Local(0),
                &ctx,
            )
            .fail_closed_reason(),
            Some(FailClosedReason::UnknownValueClass),
        );
    }

    // ── subsumption: DropKind ⇄ DropClass ──────────────────────────────────

    /// Every [`DropKind`] converts to a [`DropClass`] and the canonical reverse
    /// round-trips it (modulo the documented Vec element-symbol collapse). Proves
    /// `DropClass` subsumes the `DropKind` seed in both directions.
    #[test]
    fn drop_class_subsumes_every_drop_kind() {
        let kinds = [
            DropKind::Resource,
            DropKind::DuplexClose,
            DropKind::DuplexHalfClose(Direction::Send),
            DropKind::DuplexHalfClose(Direction::Recv),
            DropKind::LambdaActorRelease,
            DropKind::TraitObject {
                storage: TraitObjectStorage::FrameOwned,
            },
            DropKind::TraitObject {
                storage: TraitObjectStorage::HeapBoxed,
            },
            DropKind::CowHeap {
                drop_fn: "hew_string_drop",
            },
            DropKind::CowHeap {
                drop_fn: "hew_bytes_drop",
            },
            DropKind::CowHeap {
                drop_fn: "hew_vec_free",
            },
            DropKind::CowHeap {
                drop_fn: "hew_hashmap_free_layout",
            },
            DropKind::CowHeap {
                drop_fn: "hew_hashset_free_layout",
            },
            DropKind::CowHeap {
                drop_fn: "hew_gen_coro_destroy",
            },
            DropKind::RecordInPlace,
            DropKind::AggregateRecursive,
            DropKind::EnumInPlace,
            DropKind::TupleInPlace,
            DropKind::ClosurePair,
            DropKind::IndirectEnum,
        ];

        for kind in kinds {
            let class = DropClass::try_from(kind)
                .unwrap_or_else(|e| panic!("DropKind {kind:?} did not map to a DropClass: {e:?}"));
            // The canonical reverse round-trips exactly for every kind whose
            // symbol is the plain family symbol (all of the above).
            assert_eq!(
                class.canonical_drop_kind(),
                kind,
                "round-trip mismatch for {kind:?}",
            );
        }
    }

    /// The Vec element-symbol collapse is intentional and documented: owned /
    /// closure-pair Vec releases map to the `Vec` leaf family, not a distinct
    /// class. The refinement is carried by the element's own decision.
    #[test]
    fn vec_element_release_symbols_collapse_to_the_vec_family() {
        for sym in [
            "hew_vec_free",
            "hew_vec_free_owned",
            "hew_vec_free_closure_pairs",
        ] {
            let class = DropClass::try_from(DropKind::CowHeap { drop_fn: sym }).unwrap();
            assert_eq!(
                class,
                DropClass::CowHeapLeaf {
                    leaf: HeapLeaf::Vec
                }
            );
        }
    }

    /// A copy-on-write-heap symbol outside the leaf set fails closed rather than
    /// silently mis-mapping.
    #[test]
    fn unrecognised_cow_heap_symbol_fails_closed() {
        let err = DropClass::try_from(DropKind::CowHeap {
            drop_fn: "hew_not_a_real_symbol",
        })
        .unwrap_err();
        assert_eq!(err, FailClosedReason::UnrecognisedReleaseSymbol);
    }

    /// `CancellationToken` is a heap leaf but an `@resource` close — never a
    /// copy-on-write leaf. Pins the invariant that `DropClass::CowHeapLeaf` is
    /// never constructed with `CancellationToken`.
    #[test]
    fn cancellation_token_leaf_is_resource_not_cow() {
        assert_eq!(
            HeapLeaf::CancellationToken.drop_class(),
            DropClass::Resource,
        );
        for leaf in [
            HeapLeaf::String,
            HeapLeaf::Bytes,
            HeapLeaf::Vec,
            HeapLeaf::HashMap,
            HeapLeaf::HashSet,
            HeapLeaf::Generator,
        ] {
            assert_eq!(leaf.drop_class(), DropClass::CowHeapLeaf { leaf });
        }
    }

    // ── projection back to the coarser seeds ───────────────────────────────

    #[test]
    fn projects_back_to_value_class_owns_heap_and_strategy() {
        // owns_heap mirrors the authority for the decided heap shapes.
        assert!(HeapLeaf::String.decision().owns_heap());
        assert!(!OwnershipDecision::NoHeap.owns_heap());
        assert!(!OwnershipDecision::Unsupported {
            reason: FailClosedReason::LinearConsumeOnce,
        }
        .owns_heap());

        // to_value_class recovers the coarse class.
        assert_eq!(
            OwnershipDecision::NoHeap.to_value_class(),
            ValueClass::BitCopy
        );
        assert_eq!(
            HeapLeaf::Vec.decision().to_value_class(),
            ValueClass::CowValue,
        );
        assert_eq!(
            OwnershipDecision::owns_handle(DropClass::Resource).to_value_class(),
            ValueClass::AffineResource,
        );
        assert_eq!(
            OwnershipDecision::Borrowed {
                provenance: ValueProvenance::unanchored(),
            }
            .to_value_class(),
            ValueClass::View,
        );
        assert_eq!(
            OwnershipDecision::Unsupported {
                reason: FailClosedReason::LinearConsumeOnce,
            }
            .to_value_class(),
            ValueClass::Linear,
        );

        // Unsupported floors the move strategy to the existing fail-closed
        // boundary; decided decisions impose no floor.
        assert_eq!(
            OwnershipDecision::Unsupported {
                reason: FailClosedReason::UnknownValueClass,
            }
            .move_strategy_floor(),
            Some(Strategy::UnknownBlocked),
        );
        assert_eq!(HeapLeaf::String.decision().move_strategy_floor(), None);
    }

    // ── provenance carries the owner, not the nominal type ─────────────────

    #[test]
    fn place_provenance_is_total_over_place() {
        assert_eq!(
            PlaceProvenance::from(Place::Local(7)),
            PlaceProvenance::Local(7),
        );
        assert_eq!(
            PlaceProvenance::from(Place::ReturnSlot),
            PlaceProvenance::ReturnSlot,
        );
        assert_eq!(
            PlaceProvenance::from(Place::SendHalf(3)),
            PlaceProvenance::Handle {
                local: 3,
                role: HandleRole::SendHalf,
            },
        );
        assert_eq!(
            PlaceProvenance::from(Place::EnumVariant {
                local: 5,
                variant_idx: 0,
                field_idx: 1,
            }),
            PlaceProvenance::Local(5),
        );
    }

    #[test]
    fn borrowed_carries_provenance_of_its_place() {
        let no_records = empty_records();
        let classes = TypeClassTable::new();
        let decision = OwnershipDecision::classify(
            &ResolvedTy::Borrow {
                pointee: Box::new(ResolvedTy::String),
            },
            Place::Local(4),
            &ctx(&no_records, &[], &classes),
        );
        match decision {
            OwnershipDecision::Borrowed { provenance } => {
                assert_eq!(provenance.root, PlaceProvenance::Local(4));
                assert_eq!(provenance.origin, ProvenanceOrigin::Read);
                assert!(provenance.path.is_empty());
            }
            other => panic!("expected Borrowed, got {other:?}"),
        }
    }

    /// A heap-free record / tuple / enum is `NoHeap`, not a forced drop —
    /// guards against over-broad classification (the dual of the leak guard).
    #[test]
    fn heap_free_composites_are_no_heap() {
        let classes = TypeClassTable::new();

        let mut plain_record = HashMap::new();
        plain_record.insert(
            "Pair".to_string(),
            vec![
                ("a".to_string(), ResolvedTy::I64),
                ("b".to_string(), ResolvedTy::Bool),
            ],
        );
        let no_enums: Vec<EnumLayout> = vec![];
        assert_eq!(
            OwnershipDecision::classify(
                &ResolvedTy::named_user("Pair", vec![]),
                Place::Local(0),
                &ctx(&plain_record, &no_enums, &classes),
            ),
            OwnershipDecision::NoHeap,
        );

        let no_records = empty_records();
        assert_eq!(
            OwnershipDecision::classify(
                &ResolvedTy::Tuple(vec![ResolvedTy::I64, ResolvedTy::Bool]),
                Place::Local(0),
                &ctx(&no_records, &[], &classes),
            ),
            OwnershipDecision::NoHeap,
        );
    }
}
