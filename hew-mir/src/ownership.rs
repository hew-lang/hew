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
//! off the value instead of re-deriving. The complete classification of a value
//! is [`ValueOwnership`]: the rich [`OwnershipDecision`] (drop / ABI / layout)
//! plus the two coarse seed answers — the `ty_owns_heap` boolean and the
//! [`ValueClass`] — **carried on the value**, each computed once from its live
//! authority at [`ValueOwnership::classify`] time. Carrying — not re-deriving —
//! is the point: the decision *kind* alone cannot reproduce the seeds
//! faithfully. A heap-free tuple is [`NoHeap`] yet `CowValue`; a `&[string]` is
//! a non-owning [`Borrowed`] view yet `ty_owns_heap` recurses its element to
//! `true`. So the seed must travel with the value, making
//! [`ValueOwnership::owns_heap`] `≡` `ty_owns_heap` and
//! [`ValueOwnership::to_value_class`] `≡` `ValueClass::of_ty` **exactly**, for
//! every shape — the contract the consolidation relies on when it swaps a
//! seam's direct authority call for the projection.
//!
//! This carrier is now **read**, not merely defined. The per-function
//! owned-locals ledger records a [`ValueOwnership`] on every entry at its
//! defining write ([`OwnershipDecision::classify`] time), and the record /
//! tuple composite drop provers consume the carried
//! [`InteriorAlias`](OwnershipDecision::InteriorAlias)-shaped
//! [`ValueProvenance`] so a byte-copy field alias names its owner by location
//! instead of each prover re-deriving it from the instruction stream. The
//! coarser per-value facts (fresh / parameter provenance, and the drop / ABI
//! kind on non-alias entries) travel on the ledger and are read as their
//! consuming seams land; the release-bucket and type-directed drop
//! consolidations route the remaining shape-walkers through it (see the Walker
//! inventory below).
//!
//! # Subsumes (does not reinvent) the existing seeds
//!
//! | Seed | Authority | Relationship |
//! |---|---|---|
//! | `ty_owns_heap` | `model.rs` | Carried verbatim — [`ValueOwnership::owns_heap`] returns it (`≡`, by construction); the [`OwnsHeap`]/[`NoHeap`] kind is the rich split built alongside. |
//! | [`DropKind`] | `model.rs` | The release protocol — [`DropClass`] subsumes it (`TryFrom<DropKind>` + [`DropClass::canonical_drop_kind`]). |
//! | [`ValueClass`] | `value_class.rs` | Carried verbatim — [`ValueOwnership::to_value_class`] returns it (`≡`); `classify` also consumes it for the marker seed. |
//! | [`Strategy::UnknownBlocked`] | `model.rs` | The fail-closed move strategy — [`Unsupported`] projects to it ([`OwnershipDecision::move_strategy_floor`]). |
//! | `DropPlanUndetermined` | `lower.rs` | The fail-closed diagnostic — [`Unsupported`]'s [`FailClosedReason`] names the cause. |
//!
//! # Walker inventory
//!
//! The shape-specialised ownership/drop/ABI walkers that re-answer the heap /
//! release / ABI question by matching on [`ResolvedTy`] at their own call site,
//! with their disposition. This list is the consolidation worklist:
//! every `bypasses` row is a seam the release-bucket consolidation routes through [`ValueOwnership`]
//! / the single authority. The `_ => false` / `_ => None` fall-through arms are
//! the leak surface (an unenumerated heap shape silently classified non-owning).
//!
//! | Walker | Crate · entry | Re-derives | Status |
//! |---|---|---|---|
//! | `ty_owns_heap` / `ty_owns_heap_inner` | `hew-mir` · `model.rs` | heap-ownership leaf set + structural recursion | **AUTHORITY** — the single source of truth; [`OwnershipDecision::classify`] and every routed walker resolve the heap axis here. |
//! | `cbor_ty_owns_heap` | `hew-codegen-rs` · `llvm.rs` | CBOR `Vec` element heap probe (own leaf set, divergent `_ => false`) | **RETIRED** — now a `#[deprecated]` shim delegating to `ty_owns_heap` via `CborHeapLayouts`; sole caller `cbor_vec_elem_kind` is `#[allow(deprecated)]`-listed. The workspace `clippy -D warnings` CI step guards re-entry. Fixed a latent `CancellationToken` / tuple-field under-drop for free. |
//! | `callee_ownership_contract` | `hew-mir` · `runtime_symbols.rs` | runtime-callee receiver borrow, string-argument borrow, and result ownership facts | **LANDED (callee ownership contract consolidation)** — MIR drop-admission provers read the typed [`ReceiverOwnership`](crate::runtime_symbols::ReceiverOwnership), [`StringArgsOwnership`](crate::runtime_symbols::StringArgsOwnership), and [`ResultOwnership`](crate::runtime_symbols::ResultOwnership) projections; unknown callees return the fail-closed `Escapes` / `Escaping` / `Untracked` contract. |
//! | owned-locals seed gate (`Builder::binding_seeds_drop_elaboration`) | `hew-mir` · `lower.rs` → `hew-hir` · `value_class.rs` | which locals enter drop elaboration | **LANDED (seed-authority consolidation)** — one named authority answers "does this binding's type oblige drop elaboration?" at every `owned_locals` seed site AND the consume-side removal mirror, so the two sides of the ledger cannot desynchronise (a looser consume side keeps a moved-out binding in `owned_locals` and double-frees at function exit; a tighter one leaks). The verdict remains the value-class seed — record-blind via [`ValueClass`] (an unmarked user record classifies `Unknown`, which seeds); a record-aware seed upgrade reads [`ValueOwnership`] at this one seam when it lands. Pinned by a frozen verdict table over every value class plus a source-inventory scan keeping the authority's body the only seed-fact spelling (`seed_gate_matches_value_class_authority` / `seed_fact_comparison_site_inventory_is_closed` in `lower.rs`). |
//! | `cow_value_leaf_drop_symbol` | `hew-mir` · `lower.rs` | scalar-leaf release symbol (`string` → `hew_string_drop`, else `None`) | **LANDED (release-bucket consolidation)** — routed through the typed decision; see the partition-totality test below. |
//! | `binding_ty_is_plain_vec` · `is_plain_vec_element` · `binding_ty_is_owned_element_vec` · `is_owned_vec_element` · `tuple_is_all_bitcopy` | `hew-mir` · `lower.rs` | `Vec<E>` release partition (plain vs owned-element) | **LANDED (release-bucket consolidation)** — release buckets PROJECT from the typed `classify_vec_element_release` decision; their union is total vs the authority leaf set. The Plain bucket's `Named` arm reads the `named_elem_owns_heap` AUTHORITY, not `ValueClass::of_ty == BitCopy` alone: a heap-free DIRECT user enum is never `BitCopy` (value-class finalisation covers records only), so the pre-fix `BitCopy`-only gate classified `Vec<fieldless-enum>` NEITHER plain nor owned and leaked its whole buffer+handle at every scope exit — now closed by the `ty_is_direct_enum_element(e) && !named_elem_owns_heap(e)` disjunct. The unwired `Vec<bytes>` / `Vec<indirect_enum>` elements (`Unsupported(NoReleaseProtocol)`) are REJECTED at compile by `Builder::unsupported_vec_element_diagnostics` rather than silently leaked at scope exit. |
//! | `generator_yield_drop_symbol` · `project_field_inline_drop_symbol` | `hew-mir` · `lower.rs` | per-yield / per-field `Vec<E>` release-symbol picker | **LANDED (release-bucket consolidation)** — both check the closure-pair bucket before the owned/plain split, and every recursive element shape now resolves to descriptor-driven `hew_vec_free_owned`. |
//! | `ty_is_closure_pair_vec` · `ty_is_local_collection_handle` | `hew-mir` · `lower.rs` | closure-pair / collection-handle release | **LANDED (release-bucket consolidation)** — both are documented projections of the typed decision. `ty_is_closure_pair_vec` projects [`VecElementRelease::ClosurePair`] for `Builder`-free contexts (pinned by the partition-totality test) and the `Builder`-side release-symbol pickers read `classify_vec_element_release` itself; `ty_is_local_collection_handle` is the single ABI-shape authority for the `HashMap`/`HashSet` bucket, pinned against the [`HeapLeaf`] classification with a release-symbol tripwire (`collection_handle_predicate_projects_from_heap_leaf` in `lower.rs`). |
//! | `resolved_ty_cow_heap_release` | `hew-codegen-rs` · `llvm.rs` | codegen-side aggregate-walk release picker | **LANDED (type-directed drop consolidation)** — [`DropKind::CowHeap`] now carries a typed [`CowHeapRelease`] fact (not a `&'static str` literal), so codegen resolves the release symbol from the carried fact via [`CowHeapRelease::release_symbol`] with no per-site re-derivation. The three congruence validators that re-derived MIR-carried `drop_fn` symbols are deleted with the literal carrier they guarded; the sole surviving picker is the address-based aggregate walk (`resolved_ty_cow_heap_release` → [`CowHeapRelease`]), for nested fields that carry no MIR fact of their own, and it reads the same [`CowHeapRelease::release_symbol`] authority (`dedup-semantic-boundary`). |
//!
//! The boundary-totality tests pin both axes: [`OwnershipDecision::classify`] is
//! total over the twelve canonical shapes (`classify_is_total_over_the_twelve_shapes`),
//! and the release-bucket predicates' union is total vs the authority leaf set
//! (`release_bucket_partition_is_total_over_vec_elements` in `lower.rs`). The
//! unwired `Vec<bytes>` and `Vec<indirect_enum>` elements that own heap but no
//! release bucket can yet release (`Unsupported(NoReleaseProtocol)`) are
//! rejected at compile by `Builder::unsupported_vec_element_diagnostics` — a
//! fatal, actionable `NotYetImplemented` — rather than constructed and silently
//! leaked at scope exit; the reject lifts once the per-element release is wired.
//!
//! [`NoHeap`]: OwnershipDecision::NoHeap
//! [`OwnsHeap`]: OwnershipDecision::OwnsHeap
//! [`Borrowed`]: OwnershipDecision::Borrowed
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
    /// This is the *rich drop* axis only — the value's exact seed answers are
    /// carried separately on [`ValueOwnership`] (a heap-free tuple is `NoHeap`
    /// here yet `CowValue` to the value-class authority; a bare function is
    /// `NoHeap` yet `PersistentShare`).
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
    /// The carried value-class seed is `View`; the carried `ty_owns_heap` seed
    /// is `true` for a slice whose element owns heap (`&[string]`) — a fact the
    /// kind alone does not show, and the reason the seeds are carried.
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
    /// and the consolidation routes it to [`Strategy::UnknownBlocked`] /
    /// `MirCheck::DropPlanUndetermined` instead of silently leaking or
    /// double-freeing. `reason` names *why* so the diagnostic is actionable.
    Unsupported {
        /// Why the decision failed closed — feeds the diagnostic.
        reason: FailClosedReason,
    },
}

// ──────────────────────── drop protocol (subsumes DropKind) ───────────────

/// Release-protocol family for an [`OwnsHeap`](OwnershipDecision::OwnsHeap)
/// value. A scaffold mirror of [`DropKind`] that carries no codegen-only payload
/// except the discriminators that *are* the classification ([`Direction`],
/// [`TraitObjectStorage`], the [`HeapLeaf`]).
///
/// Every [`DropKind`] maps to exactly one `DropClass`
/// (`TryFrom<DropKind>`) and every `DropClass` maps back to a canonical
/// [`DropKind`] ([`DropClass::canonical_drop_kind`]), so the consolidation can route the
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
    /// `Vec<T>` — descriptor-driven `hew_vec_free` / `hew_vec_free_owned`.
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
/// the actionable diagnostic the consolidation emits instead of a silent leak; see
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
    ///
    /// NOTE: unlike `NoReleaseProtocol`, an `Unsupported(UnenumeratedShape)`
    /// `Vec` element is NOT compile-rejected by
    /// `Builder::unsupported_vec_element_diagnostics` — it falls through both the
    /// Plain and Owned buckets and emits NO scope-exit release, silently leaking
    /// the OUTER Vec's own handle+buffer (a different thing from the element's
    /// heap that `NoReleaseProtocol` guards). A heap-free direct user enum was a
    /// live instance of this — never `BitCopy`, so the `BitCopy`-only Plain gate
    /// mis-bucketed it here — now closed by routing direct enums through the
    /// `named_elem_owns_heap` authority into the Plain bucket. Any future shape
    /// that legitimately owns heap yet lands here must be added to a release
    /// bucket or to the `NoReleaseProtocol` reject, never left on this arm.
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
    /// the consolidation threads). Fail-closed rather than guessing the release ritual.
    DynStorageUnresolved,
    /// A heap-owning aggregate whose exactly-once free the drop analysis cannot
    /// prove (e.g. owned-handle aggregate extraction). Mirrors
    /// `OwnedHandleAggregateExtractionUnsupported`.
    UnprovenSoleOwner,
}

// ─────────────────────────── vec-element release ──────────────────────────

/// The scope-exit release protocol for a `Vec<E>` local, derived ONCE from the
/// element type `E` by reading the single heap-ownership authority
/// (`ty_owns_heap`). The MIR release-bucket predicates
/// (`binding_ty_is_plain_vec`, `binding_ty_is_owned_element_vec`,
/// `ty_is_closure_pair_vec`) are PROJECTIONS of this one decision, so their
/// union is total over [`ResolvedTy`](hew_types::ResolvedTy) by construction: a
/// `Vec<E>` local can never silently fall through every bucket and skip its
/// release (the leak surface the `_ => false` bucket arms used to be). The
/// codegen siblings (`resolved_ty_cow_heap_release`,
/// `resolved_ty_element_owns_heap_for_owned_vec`) pick the matching release
/// symbol; the two sides agree per shape (`dedup-semantic-boundary`).
///
/// `classify_vec_element_release` (the `Builder` method that reads the function
/// registries) is the constructor; the partition-totality test
/// (`release_bucket_partition_is_total_over_vec_elements`) pins that every
/// heap-owning `Vec` element resolves to a claimed bucket XOR a tracked
/// [`Unsupported`](VecElementRelease::Unsupported), never a silent miss.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VecElementRelease {
    /// `hew_vec_free` — the element owns no heap (a `BitCopy` scalar / aggregate)
    /// or is a plain leaf the runtime walks itself (`string` via
    /// `ElemKind::String`). Buffer + handle free.
    Plain,
    /// `hew_vec_free_owned` — the element is an owned composite (a record / enum
    /// / tuple / nested collection that owns heap), constructed through the owned
    /// descriptor ABI; the per-element `drop_fn` runs before the buffer free.
    OwnedElement,
    /// Descriptor-driven closure-pair release; each slot owns a heap pair-box
    /// and its environment box.
    ClosurePair,
    /// No release bucket claims the element, yet `ty_owns_heap(Vec<E>)` is `true`
    /// (the outer `Vec` always owns its backing buffer). The element's release
    /// ABI is not wired — a fat `bytes` triple (outside the single-pointer
    /// buckets), a bare runtime handle, or an indirect-enum pointer-element node
    /// free. A `Vec<bytes>` is unconstructible (`Vec::new` is NYI for the
    /// element); a `Vec<indirect_enum>` is constructible through the
    /// pointer-element ABI but its per-element node free is unwired. Both are
    /// REJECTED at compile (a fatal `NotYetImplemented` naming
    /// `NoReleaseProtocol`) — see
    /// `Builder::unsupported_vec_element_diagnostics` — rather than constructed
    /// and silently leaked at scope exit, the leak-safe fail-closed direction.
    /// The reject lifts once the per-element release is wired. Carrying the
    /// [`FailClosedReason`] keeps the decision a typed, actionable "no" instead
    /// of a silent non-owning `false`.
    Unsupported(FailClosedReason),
}

impl VecElementRelease {
    /// Projection: the plain buffer-only bucket (`binding_ty_is_plain_vec` /
    /// `hew_vec_free`).
    #[must_use]
    pub fn is_plain(self) -> bool {
        matches!(self, Self::Plain)
    }

    /// Projection: the owned-descriptor bucket (`binding_ty_is_owned_element_vec`
    /// / `hew_vec_free_owned`).
    #[must_use]
    pub fn is_owned_element(self) -> bool {
        matches!(self, Self::OwnedElement)
    }

    /// Projection: the closure-pair bucket (`ty_is_closure_pair_vec` /
    /// descriptor-driven `hew_vec_free_owned`).
    #[must_use]
    pub fn is_closure_pair(self) -> bool {
        matches!(self, Self::ClosurePair)
    }

    /// True when no release bucket claims the element — the fail-closed arm.
    #[must_use]
    pub fn is_unsupported(self) -> bool {
        matches!(self, Self::Unsupported(_))
    }
}

// ───────────────────────── release-symbol verdict ─────────────────────────

/// Classified verdict from the `Builder`-side release-symbol pickers
/// (`generator_yield_drop_symbol`, `project_field_inline_drop_symbol`): may a
/// consulting site emit a whole-value inline release for this type, and with
/// which C-ABI symbol?
///
/// Three-way by design. A two-way `Option<&str>` cannot distinguish "this
/// shape needs no symbol here" from "this shape owns heap but every available
/// symbol is a wrong-ABI free" — and a `Some`-gated pre-flight then admits the
/// wrong-ABI case (a buffer-only `hew_vec_free` over owned element nodes
/// leaks every node). Consulting sites match the verdict exhaustively: only
/// [`Wired`](Self::Wired) may emit; [`Unwired`](Self::Unwired) MUST surface a
/// fail-closed compile diagnostic (it carries no symbol, so it cannot be
/// emitted by accident); [`NoDropPath`](Self::NoDropPath) keeps the site's
/// existing no-symbol handling.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReleaseSymbolVerdict {
    /// A wired, ABI-correct release symbol for the whole value at this site.
    Wired(&'static str),
    /// A wired, ABI-correct in-place composite release: the whole value is a
    /// registered heap-owning record/enum whose release is the synthesised
    /// `__hew_record_drop_inplace_<R>` / `__hew_enum_drop_inplace_<E>` thunk
    /// (resolved by codegen from the drop's carried type — no C-ABI symbol
    /// travels in MIR). Emitting sites carry it as
    /// [`DropFnSpec::InPlace`](crate::model::DropFnSpec::InPlace) on an inline
    /// `Instr::Drop`, or as `DropKind::RecordInPlace` / `DropKind::EnumInPlace`
    /// on a plan `ElabDrop`. Only the yield/recv release seam
    /// (`generator_yield_drop_symbol`) returns this verdict; the field picker
    /// keeps its `NoDropPath` posture for owned-aggregate fields.
    WiredInPlace(InPlaceReleaseKind),
    /// No inline release is selected here: the shape owns no heap this picker
    /// releases, has no validated consumer-drop path (the HashMap/HashSet
    /// yield posture — leak-as-before, never a double-free risk), or is
    /// released through a different mechanism (in-place drop kinds).
    NoDropPath,
    /// The value owns heap but its per-element release protocol is unwired
    /// (`VecElementRelease::Unsupported` with the carried
    /// [`FailClosedReason`]): every available symbol would free with the
    /// wrong ABI. The consulting site must refuse the construct at compile
    /// time — never emit a symbol, never fall through silently.
    Unwired(FailClosedReason),
}

/// Which synthesised in-place drop thunk family releases a
/// [`ReleaseSymbolVerdict::WiredInPlace`] composite. The registry identity
/// (record layout vs tagged-union enum layout) selects the thunk getter on
/// the codegen side (`__hew_record_drop_inplace_<R>` vs
/// `__hew_enum_drop_inplace_<E>`); the concrete type travels on the drop
/// itself (`Instr::Drop::ty` / `ElabDrop::ty`), never here.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InPlaceReleaseKind {
    /// A registered heap-owning user (or builtin-aggregate) record.
    Record,
    /// A registered heap-owning enum (tagged-union composite).
    Enum,
}

// ──────────────────────────────── provenance ─────────────────────────────

/// A MIR storage location an ownership fact can be anchored to. The scaffold
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
    /// sentinel; the consolidation replaces with the concrete place).
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
/// reads, mirroring exactly what the MIR `Builder` holds. The consolidation wires
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
    /// **Scaffold (lands unused).** It is the constructor every consolidation
    /// call site will route through; the coarse arms (e.g. picking the plain
    /// `Vec` release symbol, deferring `dyn Trait` storage) are refined by
    /// the consolidation. It is **total** in the load-bearing sense: the type-driven match
    /// is exhaustive over [`ResolvedTy`] (no `_ => …` fallthrough), so a new
    /// `ResolvedTy` variant is a compile error here, and every shape resolves to
    /// a non-`Unsupported` decision or to a *tracked* [`Unsupported`] naming a
    /// [`FailClosedReason`].
    ///
    /// This is the **typed inventory boundary** the walker inventory (module
    /// doc) routes the shape-specialised classifiers onto: its heap axis is the
    /// [`ty_owns_heap`] authority (`≡`, carried by
    /// [`ValueOwnership`]), so a future consolidation seam swapping a direct authority
    /// call for this typed decision is byte-identical by construction.
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
            // the scaffold does not carry — tracked-Unsupported (the consolidation threads
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
            DropKind::CowHeap { release } => DropClass::CowHeapLeaf {
                // The typed carrier names its leaf directly — no symbol to
                // parse, so this arm is now infallible (an unrecognised release
                // is unrepresentable). `CowHeapRelease` is never a
                // `CancellationToken` leaf, so `CowHeapLeaf` stays copy-on-write.
                leaf: release.heap_leaf(),
            },
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
                release: leaf.cow_heap_release(),
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
            // Plain and descriptor-driven Vec releases share the `Vec` family.
            "hew_vec_free" | "hew_vec_free_owned" => HeapLeaf::Vec,
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

    /// The canonical [`CowHeapRelease`] for this leaf — the reverse of
    /// [`CowHeapRelease::heap_leaf`]. [`HeapLeaf::Vec`] chooses the *plain*
    /// family ([`CowHeapRelease::VecPlain`]); the owned / closure-pair element
    /// refinement is resolved separately from the element's own decision, so
    /// this round-trips the leaf identity, not a byte-identical Vec symbol.
    ///
    /// # Panics
    ///
    /// [`HeapLeaf::CancellationToken`] is an `@resource` close, never a
    /// copy-on-write release; it is never a `DropClass::CowHeapLeaf` leaf
    /// ([`HeapLeaf::drop_class`] enforces it), so this arm is unreachable.
    #[must_use]
    pub fn cow_heap_release(self) -> CowHeapRelease {
        match self {
            HeapLeaf::String => CowHeapRelease::String,
            HeapLeaf::Bytes => CowHeapRelease::Bytes,
            HeapLeaf::Vec => CowHeapRelease::VecPlain,
            HeapLeaf::HashMap => CowHeapRelease::HashMap,
            HeapLeaf::HashSet => CowHeapRelease::HashSet,
            HeapLeaf::Generator => CowHeapRelease::Generator,
            HeapLeaf::CancellationToken => unreachable!(
                "CancellationToken is an @resource close, never a CowHeapLeaf release; \
                 DropClass::CowHeapLeaf is never constructed with it (HeapLeaf::drop_class)"
            ),
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

/// The typed release protocol a [`DropKind::CowHeap`] carries: the heap-leaf
/// identity ([`HeapLeaf`]) folded with the three-way `Vec<E>` element
/// refinement ([`VecElementRelease`]), as a single fail-closed discriminant.
///
/// This is the type-directed replacement for the pre-consolidation
/// `DropKind::CowHeap { drop_fn: &'static str }` literal carrier: a `CowHeap`
/// drop that reaches elaboration is, by construction, a supported, wired
/// release, so the payload names its protocol by type instead of a symbol the
/// producer chose and codegen re-derived. There is no invalid `(leaf,
/// refinement)` pair to represent, so codegen resolves the C-ABI symbol from
/// the carried fact directly — the dual-derivation congruence checks the
/// literal carrier required disappear with the literal.
///
/// [`release_symbol`](Self::release_symbol) is the single symbol authority: the
/// non-Vec arms delegate to [`HeapLeaf::release_symbol`] (so codegen and MIR
/// read one table, `dedup-semantic-boundary`); the three Vec arms name the
/// element-refined symbols the runtime's owned-descriptor / closure-pair frees
/// require. [`heap_leaf`](Self::heap_leaf) projects back to the leaf identity
/// (all three Vec arms → [`HeapLeaf::Vec`]), matching the deliberate Vec-family
/// collapse [`HeapLeaf::from_release_symbol`] already documents.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CowHeapRelease {
    /// `string` — `hew_string_drop`. [`HeapLeaf::String`].
    String,
    /// `bytes` — `hew_bytes_drop` (the `{ptr,len,cap}` triple; codegen routes
    /// it through the triple-field-0 emitter, not the single-`ptr` path).
    /// [`HeapLeaf::Bytes`].
    Bytes,
    /// `Vec<E>` with a plain element (`BitCopy` scalar/aggregate, or a `string`
    /// the runtime walks itself) — `hew_vec_free`. The plain family of
    /// [`HeapLeaf::Vec`], refinement [`VecElementRelease::Plain`].
    VecPlain,
    /// `Vec<E>` with an owned-heap composite element (record/enum/tuple/nested
    /// collection) — `hew_vec_free_owned` (per-element descriptor `drop_fn`
    /// then buffer free). Refinement [`VecElementRelease::OwnedElement`].
    VecOwnedElement,
    /// `Vec<fn>` / `Vec<closure>` — `hew_vec_free_owned`, using the stamped
    /// release-only closure-pair descriptor.
    VecClosurePairs,
    /// `HashMap<K,V>` — `hew_hashmap_free_layout`. [`HeapLeaf::HashMap`].
    HashMap,
    /// `HashSet<E>` — `hew_hashset_free_layout`. [`HeapLeaf::HashSet`].
    HashSet,
    /// `Generator<Y,R>` / `AsyncGenerator<Y>` — `hew_gen_coro_destroy`.
    /// [`HeapLeaf::Generator`].
    Generator,
}

impl CowHeapRelease {
    /// The C-ABI runtime release symbol for this protocol. The single symbol
    /// authority a [`DropKind::CowHeap`] resolves through: the non-Vec arms
    /// delegate to [`HeapLeaf::release_symbol`]; the Vec arms name the
    /// element-refined frees.
    #[must_use]
    pub fn release_symbol(self) -> &'static str {
        match self {
            CowHeapRelease::String => HeapLeaf::String.release_symbol(),
            CowHeapRelease::Bytes => HeapLeaf::Bytes.release_symbol(),
            CowHeapRelease::HashMap => HeapLeaf::HashMap.release_symbol(),
            CowHeapRelease::HashSet => HeapLeaf::HashSet.release_symbol(),
            CowHeapRelease::Generator => HeapLeaf::Generator.release_symbol(),
            // The Vec element refinement: the plain family is
            // `HeapLeaf::Vec.release_symbol()`; the owned / closure-pair frees
            // are element-directed and have no bare-leaf symbol.
            CowHeapRelease::VecPlain => HeapLeaf::Vec.release_symbol(),
            CowHeapRelease::VecOwnedElement | CowHeapRelease::VecClosurePairs => {
                "hew_vec_free_owned"
            }
        }
    }

    /// The [`HeapLeaf`] identity this protocol releases. All three Vec arms
    /// project to [`HeapLeaf::Vec`] (the element refinement is not part of the
    /// leaf identity), matching [`HeapLeaf::from_release_symbol`]'s Vec-family
    /// collapse — so `TryFrom<DropKind> for DropClass` maps every Vec release
    /// to `CowHeapLeaf { leaf: Vec }`.
    #[must_use]
    pub fn heap_leaf(self) -> HeapLeaf {
        match self {
            CowHeapRelease::String => HeapLeaf::String,
            CowHeapRelease::Bytes => HeapLeaf::Bytes,
            CowHeapRelease::VecPlain
            | CowHeapRelease::VecOwnedElement
            | CowHeapRelease::VecClosurePairs => HeapLeaf::Vec,
            CowHeapRelease::HashMap => HeapLeaf::HashMap,
            CowHeapRelease::HashSet => HeapLeaf::HashSet,
            CowHeapRelease::Generator => HeapLeaf::Generator,
        }
    }

    /// Parse a release symbol into its typed protocol. Returns `None` for an
    /// unrecognised symbol (fail-closed) — the boundary where an unknown
    /// release string is rejected now that the typed carrier makes an invalid
    /// `CowHeap` payload unrepresentable downstream. Round-trips
    /// `release_symbol` for every variant.
    #[must_use]
    pub fn from_symbol(symbol: &str) -> Option<Self> {
        Some(match symbol {
            "hew_string_drop" => CowHeapRelease::String,
            "hew_bytes_drop" => CowHeapRelease::Bytes,
            "hew_vec_free" => CowHeapRelease::VecPlain,
            "hew_vec_free_owned" => CowHeapRelease::VecOwnedElement,
            "hew_hashmap_free_layout" => CowHeapRelease::HashMap,
            "hew_hashset_free_layout" => CowHeapRelease::HashSet,
            "hew_gen_coro_destroy" => CowHeapRelease::Generator,
            _ => return None,
        })
    }
}

impl OwnershipDecision {
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

// ───────────────── carried classification (decision + seed facts) ─────────

/// The complete ownership classification of one MIR value: the rich
/// [`OwnershipDecision`] (drop / ABI / layout axis) together with the two coarse
/// seed answers — the `ty_owns_heap` boolean and the [`ValueClass`] — **carried
/// on the value**, each computed once from its live authority at
/// [`classify`](Self::classify) time.
///
/// Carrying (rather than re-deriving) is the whole point of the lane: the
/// structural [`OwnershipDecision`] kind alone cannot reproduce the seeds
/// faithfully (a heap-free tuple is [`NoHeap`](OwnershipDecision::NoHeap) yet
/// `CowValue`; a `&[string]` is a [`Borrowed`](OwnershipDecision::Borrowed) view
/// yet `ty_owns_heap` recurses its element to `true`; a `Generator` is
/// `OwnsHeap { CowHeapLeaf }` yet `AffineResource`). Because the facts are
/// carried, [`owns_heap`](Self::owns_heap) `≡` `ty_owns_heap` and
/// [`to_value_class`](Self::to_value_class) `≡` `ValueClass::of_ty` **exactly**,
/// so a future consolidation seam can swap its direct authority call for the
/// projection with byte-identical behaviour.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValueOwnership {
    /// The rich typed decision (drop / ABI / layout).
    decision: OwnershipDecision,
    /// The carried `ty_owns_heap` seed for the classified type.
    owns_heap: bool,
    /// The carried [`ValueClass::of_ty`] seed for the classified type.
    value_class: ValueClass,
}

impl ValueOwnership {
    /// Classify a value of type `ty` held in `place`, carrying its seed facts.
    ///
    /// Builds the rich [`OwnershipDecision`] (via
    /// [`OwnershipDecision::classify`]) and, *once*, the two seed answers from
    /// their live authorities ([`ty_owns_heap`], [`ValueClass::of_ty`]) so the
    /// projections below are exact for every shape — including the ones the
    /// decision kind alone misclassifies.
    #[must_use]
    pub fn classify<S: BuildHasher>(
        ty: &ResolvedTy,
        place: Place,
        ctx: &OwnershipCtx<'_, S>,
    ) -> Self {
        Self {
            decision: OwnershipDecision::classify(ty, place, ctx),
            owns_heap: ty_owns_heap(ty, &ctx.heap_layouts()),
            value_class: ValueClass::of_ty(ty, ctx.type_classes),
        }
    }

    /// The rich typed decision (drop / ABI / layout axis).
    #[must_use]
    pub fn decision(&self) -> &OwnershipDecision {
        &self.decision
    }

    /// Consume into the owned [`OwnershipDecision`].
    #[must_use]
    pub fn into_decision(self) -> OwnershipDecision {
        self.decision
    }

    /// The carried `ty_owns_heap` seed — equal to `ty_owns_heap(ty, …)` for the
    /// classified `ty` **by construction**. This is the exact projection a
    /// future consolidation seam reads instead of re-walking the type.
    #[must_use]
    pub fn owns_heap(&self) -> bool {
        self.owns_heap
    }

    /// The carried [`ValueClass`] seed — equal to `ValueClass::of_ty(ty, …)` for
    /// the classified `ty` **by construction**.
    #[must_use]
    pub fn to_value_class(&self) -> ValueClass {
        self.value_class
    }

    /// The [`DropClass`] of the owning decision, if any (delegates to the kind).
    #[must_use]
    pub fn drop_class(&self) -> Option<DropClass> {
        self.decision.drop_class()
    }

    /// The [`FailClosedReason`] of a tracked-`Unsupported` decision, if any.
    #[must_use]
    pub fn fail_closed_reason(&self) -> Option<FailClosedReason> {
        self.decision.fail_closed_reason()
    }

    /// The move-strategy floor an `Unsupported` decision imposes, if any.
    #[must_use]
    pub fn move_strategy_floor(&self) -> Option<Strategy> {
        self.decision.move_strategy_floor()
    }
}

impl FailClosedReason {
    /// The actionable diagnostic note the consolidation attaches when this reason reaches
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

    fn closure_no_capture() -> ResolvedTy {
        ResolvedTy::Closure {
            params: vec![],
            ret: Box::new(ResolvedTy::Unit),
            captures: vec![],
        }
    }

    fn fn_unit() -> ResolvedTy {
        ResolvedTy::Function {
            params: vec![],
            ret: Box::new(ResolvedTy::Unit),
        }
    }

    fn generator_i64() -> ResolvedTy {
        ResolvedTy::named_builtin(
            "Generator",
            BuiltinType::Generator,
            vec![ResolvedTy::I64, ResolvedTy::Unit],
        )
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

    /// The shapes that are *tracked* `Unsupported`: each carries a
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
                release: CowHeapRelease::String,
            },
            DropKind::CowHeap {
                release: CowHeapRelease::Bytes,
            },
            DropKind::CowHeap {
                release: CowHeapRelease::VecPlain,
            },
            DropKind::CowHeap {
                release: CowHeapRelease::HashMap,
            },
            DropKind::CowHeap {
                release: CowHeapRelease::HashSet,
            },
            DropKind::CowHeap {
                release: CowHeapRelease::Generator,
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
        for release in [
            CowHeapRelease::VecPlain,
            CowHeapRelease::VecOwnedElement,
            CowHeapRelease::VecClosurePairs,
        ] {
            let class = DropClass::try_from(DropKind::CowHeap { release }).unwrap();
            assert_eq!(
                class,
                DropClass::CowHeapLeaf {
                    leaf: HeapLeaf::Vec
                }
            );
        }
    }

    /// A copy-on-write-heap symbol outside the leaf set fails closed at the
    /// parse boundary rather than silently mis-mapping. The typed
    /// `CowHeapRelease` carrier makes an unknown symbol unrepresentable on a
    /// `DropKind::CowHeap`, so the fail-closed reject now lives on the round-trip
    /// parser (`CowHeapRelease::from_symbol` / `HeapLeaf::from_release_symbol`),
    /// and every valid symbol round-trips.
    #[test]
    fn unrecognised_cow_heap_symbol_fails_closed() {
        assert_eq!(CowHeapRelease::from_symbol("hew_not_a_real_symbol"), None);
        assert_eq!(HeapLeaf::from_release_symbol("hew_not_a_real_symbol"), None);
        for release in [
            CowHeapRelease::String,
            CowHeapRelease::Bytes,
            CowHeapRelease::VecPlain,
            CowHeapRelease::VecOwnedElement,
            CowHeapRelease::HashMap,
            CowHeapRelease::HashSet,
            CowHeapRelease::Generator,
        ] {
            assert_eq!(
                CowHeapRelease::from_symbol(release.release_symbol()),
                Some(release),
                "release symbol must round-trip through from_symbol",
            );
        }
        assert_eq!(
            CowHeapRelease::from_symbol(CowHeapRelease::VecClosurePairs.release_symbol()),
            Some(CowHeapRelease::VecOwnedElement),
            "closure-pair Vecs share the canonical descriptor-driven symbol",
        );
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

    /// `Unsupported` floors the move strategy to the existing fail-closed
    /// boundary; every decided decision imposes no floor.
    #[test]
    fn unsupported_floors_move_strategy_decided_does_not() {
        assert_eq!(
            OwnershipDecision::Unsupported {
                reason: FailClosedReason::UnknownValueClass,
            }
            .move_strategy_floor(),
            Some(Strategy::UnknownBlocked),
        );
        assert_eq!(HeapLeaf::String.decision().move_strategy_floor(), None);
        assert_eq!(OwnershipDecision::NoHeap.move_strategy_floor(), None);
    }

    /// Assert the carried seed projections of `ty` (held in `place`) equal the
    /// **live** authorities for the same `ty` — the exact contract the consolidation swaps
    /// a seam's `ty_owns_heap(ty)` / `ValueClass::of_ty(ty)` call for.
    fn assert_carries_live_seeds(
        label: &str,
        ty: &ResolvedTy,
        place: Place,
        records: &HashMap<String, Vec<(String, ResolvedTy)>>,
        enums: &[EnumLayout],
        classes: &TypeClassTable,
    ) {
        let c = ctx(records, enums, classes);
        let owned = ValueOwnership::classify(ty, place, &c);
        assert_eq!(
            owned.owns_heap(),
            ty_owns_heap(ty, &c.heap_layouts()),
            "{label}: owns_heap() must equal the ty_owns_heap authority exactly",
        );
        assert_eq!(
            owned.to_value_class(),
            ValueClass::of_ty(ty, classes),
            "{label}: to_value_class() must equal the ValueClass::of_ty authority exactly",
        );
    }

    /// The carried projections round-trip the live authorities for every shape —
    /// including the ones the *structural* decision-kind alone got wrong, which
    /// is exactly why the facts are carried, not re-derived from the kind:
    ///
    /// - `&[string]` is a `Borrowed` view but `ty_owns_heap` recurses its element
    ///   to `true` (the kind projected `false`);
    /// - a heap-free tuple / array is `NoHeap` but `ValueClass::of_ty` is
    ///   `CowValue` (the kind projected `BitCopy`);
    /// - a bare function / no-capture closure is `NoHeap` but `PersistentShare`
    ///   (the kind projected `BitCopy`);
    /// - a `Generator` is `OwnsHeap { CowHeapLeaf }` but `AffineResource` (the
    ///   kind projected `CowValue`).
    #[test]
    fn carried_seed_projections_equal_the_live_authorities() {
        let classes = TypeClassTable::new();
        let boxed = boxed_record_orders();
        let none = empty_records();

        // Baselines the structural projection already agreed with.
        let baselines = [
            ("scalar", ResolvedTy::I64),
            ("string", ResolvedTy::String),
            ("bytes", ResolvedTy::Bytes),
            ("vec", vec_i64()),
            ("cancellation-token", ResolvedTy::CancellationToken),
        ];
        for (label, ty) in &baselines {
            assert_carries_live_seeds(label, ty, Place::Local(0), &none, &[], &classes);
        }

        // Shapes where at least one coarse projection differs from the rich
        // decision kind — each must still equal its live authority.
        let slice_string = ResolvedTy::Slice(Box::new(ResolvedTy::String));
        let drifted = [
            ("slice<string>", slice_string.clone()),
            ("slice<i64>", ResolvedTy::Slice(Box::new(ResolvedTy::I64))),
            (
                "tuple(heap-free)",
                ResolvedTy::Tuple(vec![ResolvedTy::I64, ResolvedTy::Bool]),
            ),
            (
                "array(heap-free)",
                ResolvedTy::Array(Box::new(ResolvedTy::I64), 4),
            ),
            (
                "array(string)",
                ResolvedTy::Array(Box::new(ResolvedTy::String), 2),
            ),
            ("function", fn_unit()),
            ("closure(no-capture)", closure_no_capture()),
            ("closure(heap-capture)", closure_capturing_string()),
            ("generator", generator_i64()),
            ("trait-object", ResolvedTy::TraitObject { traits: vec![] }),
        ];
        for (label, ty) in &drifted {
            assert_carries_live_seeds(label, ty, Place::Local(0), &none, &[], &classes);
        }

        // A heap-owning user record (needs the record registry).
        assert_carries_live_seeds(
            "record(heap)",
            &ResolvedTy::named_user("Boxed", vec![]),
            Place::Local(0),
            &boxed,
            &[],
            &classes,
        );

        // The exact corrections, pinned directly so the fix is self-documenting
        // (not merely "equal to whatever the authority says").
        let c = ctx(&none, &[], &classes);
        assert!(
            ValueOwnership::classify(&slice_string, Place::Local(0), &c).owns_heap(),
            "&[string] transitively owns heap: owns_heap() is true (the kind projected false)",
        );
        assert!(
            ValueOwnership::classify(&closure_capturing_string(), Place::Local(0), &c).owns_heap(),
            "a closure capturing a string transitively owns heap",
        );
        assert_eq!(
            ValueOwnership::classify(
                &ResolvedTy::Tuple(vec![ResolvedTy::I64, ResolvedTy::Bool]),
                Place::Local(0),
                &c,
            )
            .to_value_class(),
            ValueClass::CowValue,
            "a heap-free tuple is CowValue (the kind projected BitCopy)",
        );
        assert_eq!(
            ValueOwnership::classify(&fn_unit(), Place::Local(0), &c).to_value_class(),
            ValueClass::PersistentShare,
            "a bare function is PersistentShare (the kind projected BitCopy)",
        );
        assert_eq!(
            ValueOwnership::classify(&closure_no_capture(), Place::Local(0), &c).to_value_class(),
            ValueClass::PersistentShare,
            "a no-capture closure is PersistentShare (the kind projected BitCopy)",
        );
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
