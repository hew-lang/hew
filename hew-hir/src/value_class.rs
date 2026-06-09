use std::collections::HashMap;

use hew_parser::ast::ResourceMarker as AstResourceMarker;
use hew_types::{BuiltinType, ResolvedTy};

/// HIR-owned type classification marker.
///
/// Parser-level markers only represent user-written ownership attributes
/// (`#[resource]` / `#[linear]`). HIR also needs substrate registrations for
/// compiler-known value types that are not user-authored attributes, such as
/// `BitCopy` crash-hook payload records.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum ResourceMarker {
    #[default]
    None,
    BitCopy,
    Resource,
    Linear,
}

impl From<hew_parser::ast::ResourceMarker> for ResourceMarker {
    fn from(marker: hew_parser::ast::ResourceMarker) -> Self {
        match marker {
            AstResourceMarker::None => Self::None,
            AstResourceMarker::Resource => Self::Resource,
            AstResourceMarker::Linear => Self::Linear,
        }
    }
}

/// Per-named-type classification table consumed by `ValueClass::of_ty`.
///
/// Construction-site authority: the table is populated by HIR lowering
/// from every `Item::TypeDecl`'s `#[resource]` / `#[linear]` marker and
/// compiler-known substrate registrations. Parser-level storage is retained
/// for compatibility with existing HIR/MIR construction sites; callers must use
/// `lookup_type_marker` so `BitCopy` registrations that have no parser spelling
/// are still observed. LESSONS: `type-info-survival`.
pub type TypeClassTable = HashMap<String, (ResourceMarker, Option<String>)>;

#[must_use]
pub fn lookup_type_marker(name: &str, type_classes: &TypeClassTable) -> Option<ResourceMarker> {
    crate::builtin_type_classes::builtin_type_registration(name)
        .map(|registration| registration.marker)
        .or_else(|| type_classes.get(name).map(|(marker, _)| *marker))
}

#[must_use]
pub fn lookup_type_marker_for_ty(
    ty: &ResolvedTy,
    type_classes: &TypeClassTable,
) -> Option<ResourceMarker> {
    let ResolvedTy::Named {
        name,
        args,
        builtin,
    } = ty
    else {
        return None;
    };

    if builtin.is_some() {
        return lookup_type_marker(name, type_classes);
    }

    if !args.is_empty() {
        let concrete_key = crate::monomorph::mangle(name, args);
        if let Some((marker, _)) = type_classes.get(&concrete_key) {
            return Some(*marker);
        }
    }

    type_classes.get(name).map(|(marker, _)| *marker)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ValueClass {
    BitCopy,
    CowValue,
    PersistentShare,
    /// `@resource` types — external-resource values with an implicit drop side
    /// effect (`close(consuming self)`). Drop elaboration emits an explicit
    /// `ElabMir::Drop { drop_fn: Some(close) }` on every reachable exit.
    AffineResource,
    /// `@linear` types — single-owner values with **no implicit drop**.
    /// The move-checker rejects any function where a `Linear` binding is
    /// live at an exit without being consumed via a declared consuming
    /// method (`MirCheck::MustConsume`).
    Linear,
    View,
    Unknown,
}

impl ValueClass {
    /// Resolve a type's value-class.
    ///
    /// For `ResolvedTy::Named { name, .. }`, looks up the marker in the
    /// supplied `TypeClassTable`:
    /// - `Some((BitCopy, _))` → `Self::BitCopy`
    /// - `Some((Resource, _))` → `Self::AffineResource`
    /// - `Some((Linear, _))` → `Self::Linear`
    /// - `Some((None, _))` or absent → `Self::Unknown` (preserved fallback;
    ///   the unmarked Named-type behaviour the slice still routes through
    ///   `Strategy::UnknownBlocked` at MIR boundary).
    ///
    /// Builtin types are independent of the table.
    #[must_use]
    pub fn of_ty(ty: &ResolvedTy, type_classes: &TypeClassTable) -> Self {
        match ty {
            ResolvedTy::Bool
            | ResolvedTy::Char
            | ResolvedTy::I8
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
            | ResolvedTy::Duration
            | ResolvedTy::Unit
            | ResolvedTy::Never => Self::BitCopy,
            ResolvedTy::String
            | ResolvedTy::Bytes
            | ResolvedTy::Array(_, _)
            | ResolvedTy::Tuple(_) => Self::CowValue,
            ResolvedTy::CancellationToken => Self::AffineResource,
            // `&T` immutable borrow is a non-owning view (F3): reuse `View` so
            // it shares the no-retain / no-drop drop-elaboration arm. A borrow
            // binding emits no drop (`build_lifo_drops` View no-op).
            ResolvedTy::Slice(_) | ResolvedTy::Pointer { .. } | ResolvedTy::Borrow { .. } => {
                Self::View
            }
            ResolvedTy::Function { .. }
            | ResolvedTy::Closure { .. }
            | ResolvedTy::TraitObject { .. } => Self::PersistentShare,
            ResolvedTy::Named { builtin, .. } => {
                match lookup_type_marker_for_ty(ty, type_classes) {
                    Some(ResourceMarker::BitCopy) => Self::BitCopy,
                    Some(ResourceMarker::Resource) => Self::AffineResource,
                    Some(ResourceMarker::Linear) => Self::Linear,
                    Some(ResourceMarker::None) | None => {
                        if matches!(
                            builtin,
                            Some(BuiltinType::Vec | BuiltinType::HashMap | BuiltinType::HashSet)
                        ) {
                            Self::CowValue
                        } else {
                            Self::Unknown
                        }
                    }
                }
            }
            // Task handles are consume-once: MirCheck::MustConsume fires if a
            // ForkTaskHandle binding is live at an exit without being consumed
            // via AwaitTask or the implicit block-end join. Linear is the
            // correct class — it threads through C2's existing UseAfterConsume /
            // MustConsume machinery without new checks. The inner type T's own
            // class is checked independently when the task is awaited and T is
            // produced.
            ResolvedTy::Task(_) => Self::Linear,
            // An abstract parameter's value-class depends on the type that
            // monomorphisation substitutes in. Until then it is genuinely
            // unknown, so it routes through the conservative `Unknown` arm
            // (the same fail-closed boundary as an unmarked Named). This only
            // arises in gated polymorphic bodies, which never reach codegen.
            ResolvedTy::TypeParam { .. } => Self::Unknown,
        }
    }
}

// ---------------------------------------------------------------------------
// Layout-witness table (VWT)
// ---------------------------------------------------------------------------

/// Runtime symbol that *retains* one owner of a `Bytes` value by bumping its
/// atomic refcount — `hew-runtime/src/bytes.rs::hew_bytes_clone_ref`.
///
/// Single source of truth: the MIR retain/drop elaborator and the codegen
/// backend that consume the VWT (P2/P3) import this constant rather than
/// re-spelling the symbol literal. Spelling a runtime symbol in two places is
/// the classic drift bug (LESSONS `string-identifier-fragility`; CLAUDE §6).
pub const BYTES_COPY_SYMBOL: &str = "hew_bytes_clone_ref";

/// Runtime symbol that *releases* one owner of a `Bytes` value — decrements
/// the atomic refcount and frees the allocation at zero
/// (`hew-runtime/src/bytes.rs::hew_bytes_drop`). Single source of truth; see
/// [`BYTES_COPY_SYMBOL`].
pub const BYTES_DESTROY_SYMBOL: &str = "hew_bytes_drop";

/// Runtime symbol that *retains* one owner of a `String` value by bumping its
/// C-string header refcount and returning the **same** data pointer — `String`
/// is immutable-shareable, so a clone is a copy-on-write share, not a deep copy
/// (`hew-runtime/src/string.rs::hew_string_clone`, which delegates the bump to
/// `hew-cabi`'s `cstring_retain`).
///
/// Single source of truth: the MIR retain/drop elaborator and the codegen
/// backend that consume the VWT (P2/P3) import this constant rather than
/// re-spelling the symbol literal. The codegen state-clone synthesis already
/// emits this symbol for `String` fields, so binding `copy` to it keeps the
/// VWT and the existing emission in lockstep (LESSONS `string-identifier-fragility`;
/// CLAUDE §6).
pub const STRING_COPY_SYMBOL: &str = "hew_string_clone";

/// Runtime symbol that *releases* one owner of a `String` value — decrements
/// the C-string header refcount and frees the allocation at zero, after the
/// static-literal skip (`hew-runtime/src/string.rs::hew_string_drop`). This is
/// the universal `String` consumer. Single source of truth; see
/// [`STRING_COPY_SYMBOL`].
pub const STRING_DESTROY_SYMBOL: &str = "hew_string_drop";

/// Runtime symbol that *copies* a `Vec` value — `hew_vec_clone_managed`
/// (`hew-runtime/src/vec.rs`). Unlike the legacy `hew_vec_clone`/`hew_vec_free`
/// pair (which fail closed on layout-backed element kinds), the witness-managed
/// `*_managed` pair is the one codegen already emits for actor-state `Vec`
/// fields and the one that handles every element kind: it deep-copies the
/// backing buffer and, for `String` elements, retains each element on the
/// refcounted header-aware discipline (W5.011 P2b-vec) rather than `strdup`.
///
/// Single source of truth: the MIR retain/drop elaborator and codegen backend
/// that consume the VWT (P3) import this constant rather than re-spelling the
/// symbol literal (LESSONS `string-identifier-fragility`; CLAUDE §6).
pub const VEC_COPY_SYMBOL: &str = "hew_vec_clone_managed";

/// Runtime symbol that *destroys* a `Vec` value — `hew_vec_free_managed`
/// (`hew-runtime/src/vec.rs`): frees the backing buffer and, for `String`
/// elements, releases each element via `hew_string_drop` (free-at-zero). Single
/// source of truth; see [`VEC_COPY_SYMBOL`].
pub const VEC_DESTROY_SYMBOL: &str = "hew_vec_free_managed";

/// Runtime symbol that *copies* a `HashMap` value — `hew_hashmap_clone_layout`
/// (`hew-runtime/src/hashmap.rs`). Reads the map's embedded key/value layout
/// descriptors and deep-copies the table; for `String` keys/values it retains
/// each element on the header-aware refcount discipline via
/// `clone_layout_string_blob` → `hew_string_clone` (refcount bump, static-safe)
/// rather than `strdup`. This is the symbol codegen already emits for
/// actor-state `HashMap` fields (`hew-codegen-rs/src/llvm.rs`
/// `HASHMAP_CLONE_LAYOUT_SYMBOL`).
///
/// Single source of truth: the MIR retain/drop elaborator and codegen backend
/// that consume the VWT (P3) import this constant rather than re-spelling the
/// symbol literal (LESSONS `string-identifier-fragility`; CLAUDE §6).
pub const HASHMAP_COPY_SYMBOL: &str = "hew_hashmap_clone_layout";

/// Runtime symbol that *destroys* a `HashMap` value — `hew_hashmap_free_layout`
/// (`hew-runtime/src/hashmap.rs`): frees the table and, for `String`
/// keys/values, releases each element via the layout drop thunk
/// (`hew_layout_string_drop` → `hew_string_drop`, free-at-zero). Single source
/// of truth; see [`HASHMAP_COPY_SYMBOL`].
pub const HASHMAP_DESTROY_SYMBOL: &str = "hew_hashmap_free_layout";

/// Runtime symbol that *copies* a `HashSet` value — `hew_hashset_clone_layout`
/// (`hew-runtime/src/hashset.rs`). A `HashSet<T>` is a thin wrapper whose
/// element is the map *key*; it inherits the header-aware key contract, so for
/// `String` elements the clone retains each element via `hew_string_clone`.
/// This is the symbol codegen already emits for actor-state `HashSet` fields
/// (`hew-codegen-rs/src/llvm.rs` `HASHSET_CLONE_LAYOUT_SYMBOL`).
///
/// Single source of truth; see [`HASHMAP_COPY_SYMBOL`].
pub const HASHSET_COPY_SYMBOL: &str = "hew_hashset_clone_layout";

/// Runtime symbol that *destroys* a `HashSet` value — `hew_hashset_free_layout`
/// (`hew-runtime/src/hashset.rs`): frees the table and, for `String` elements,
/// releases each element via the layout drop thunk (free-at-zero). Single
/// source of truth; see [`HASHSET_COPY_SYMBOL`].
pub const HASHSET_DESTROY_SYMBOL: &str = "hew_hashset_free_layout";

/// ABI mirror of `hew-runtime/src/bytes.rs::BytesTriple` — the by-value
/// representation of a `Bytes` (`{ ptr: *const u8, offset: u32, len: u32 }`,
/// `#[repr(C)]`). Defined here only so the witness `size`/`align` derive from
/// the real layout (pointer width resolves per target — CLAUDE §7) instead of
/// hand-coded magic numbers, without taking a dependency on the runtime crate.
/// The golden test pins the resulting numbers, so any drift from the runtime
/// ABI is caught at test time.
#[repr(C)]
#[allow(
    dead_code,
    reason = "layout-only mirror; fields exist solely to fix the repr(C) \
              size/align used by LayoutWitness::BYTES"
)]
struct BytesAbiValue {
    ptr: *const u8,
    offset: u32,
    len: u32,
}

/// Size, in bytes, of a `Bytes` value at the C ABI boundary (the fat triple).
const BYTES_VALUE_SIZE: u64 = core::mem::size_of::<BytesAbiValue>() as u64;

/// Alignment, in bytes, of a `Bytes` value at the C ABI boundary.
const BYTES_VALUE_ALIGN: u64 = core::mem::align_of::<BytesAbiValue>() as u64;

/// Size, in bytes, of a `String` value at the C ABI boundary. A Hew `String` is
/// a single NUL-terminated data pointer (`*mut c_char`) — the refcount header
/// lives in the heap allocation at `data - 16`, not in the by-value handle — so
/// its by-value size/align are exactly those of a pointer (pointer width
/// resolves per target — CLAUDE §7).
const STRING_VALUE_SIZE: u64 = core::mem::size_of::<*const u8>() as u64;

/// Alignment, in bytes, of a `String` value at the C ABI boundary.
const STRING_VALUE_ALIGN: u64 = core::mem::align_of::<*const u8>() as u64;

/// Size, in bytes, of a `Vec` value at the C ABI boundary. A Hew `Vec` is a
/// single opaque handle (`*mut HewVec`) — the length, capacity, and element
/// buffer live behind the handle in the heap allocation, not in the by-value
/// representation — so its by-value size/align are exactly those of a pointer
/// (pointer width resolves per target — CLAUDE §7).
const VEC_VALUE_SIZE: u64 = core::mem::size_of::<*const u8>() as u64;

/// Alignment, in bytes, of a `Vec` value at the C ABI boundary.
const VEC_VALUE_ALIGN: u64 = core::mem::align_of::<*const u8>() as u64;

/// Size, in bytes, of a `HashMap` value at the C ABI boundary. A Hew `HashMap`
/// is a single opaque handle (`*mut HewHashMap`) — the buckets, layout
/// descriptors, and key/value storage live behind the handle in the heap
/// allocation, not in the by-value representation — so its by-value size/align
/// are exactly those of a pointer (pointer width resolves per target — CLAUDE §7).
const HASHMAP_VALUE_SIZE: u64 = core::mem::size_of::<*const u8>() as u64;

/// Alignment, in bytes, of a `HashMap` value at the C ABI boundary.
const HASHMAP_VALUE_ALIGN: u64 = core::mem::align_of::<*const u8>() as u64;

/// Size, in bytes, of a `HashSet` value at the C ABI boundary. A Hew `HashSet`
/// is a single opaque handle (`*mut HewHashSet`); like `HashMap` the storage
/// lives behind the handle, so its by-value size/align are exactly those of a
/// pointer (pointer width resolves per target — CLAUDE §7).
const HASHSET_VALUE_SIZE: u64 = core::mem::size_of::<*const u8>() as u64;

/// Alignment, in bytes, of a `HashSet` value at the C ABI boundary.
const HASHSET_VALUE_ALIGN: u64 = core::mem::align_of::<*const u8>() as u64;

/// Layout-witness table (VWT) for a heap value class — the single *derived*
/// authority for its memory model.
///
/// Modelled on the Swift value-witness table: `{ size, align, copy(retain),
/// move, destroy(release), is_immutable_shareable }`. Every downstream
/// ownership decision — drop elaboration (`build_lifo_drops` → `destroy`),
/// codegen call-arg lowering (by-value pass → `copy`), and cross-actor
/// sendability (`is_immutable_shareable`) — *derives* from this table rather
/// than hand-wiring per type.
///
/// Derived once at the HIR construction site via [`LayoutWitness::of_ty`],
/// which reads the same [`ValueClass::of_ty`] / [`TypeClassTable`] authority
/// the rest of the ownership surface uses. Downstream phases read the witness;
/// they never re-derive `copy`/`destroy` by walking the AST (LESSONS
/// `type-info-survival` / construction-site authority).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LayoutWitness {
    /// Size of the value at the C ABI boundary, in bytes.
    pub size: u64,
    /// Alignment of the value at the C ABI boundary, in bytes.
    pub align: u64,
    /// Retain: runtime symbol that adds one owner of the value (refcount bump).
    pub copy: &'static str,
    /// The VWT `move` operation. `None` = bitwise relocation: ownership
    /// transfers with the bits, with no refcount change and no runtime call —
    /// correct for refcounted COW values whose owning identity is the heap
    /// header, not the by-value handle. `Some(sym)` reserves a runtime
    /// relocation hook (none today; the `iso`/`Linear` zero-copy handoff lands
    /// later in the epic).
    pub move_: Option<&'static str>,
    /// Release: runtime symbol that drops one owner of the value (refcount
    /// decrement, free at zero).
    pub destroy: &'static str,
    /// True iff the value is deeply immutable and may be shared across an actor
    /// boundary by retain alone — no deep copy, no data race. Drives the
    /// sendability gate later in the epic.
    pub is_immutable_shareable: bool,
}

impl LayoutWitness {
    /// The witness for `Bytes` — the template every other heap type follows.
    ///
    /// Reproduces the existing refcounted copy-on-write behaviour exactly:
    /// `copy = hew_bytes_clone_ref`, `destroy = hew_bytes_drop`, bitwise move,
    /// `is_immutable_shareable = true`, with `size`/`align` taken from the
    /// `Bytes` value layout. No behaviour change: nothing consumes the witness
    /// yet (P1 defines and binds; later phases read it).
    pub const BYTES: Self = Self {
        size: BYTES_VALUE_SIZE,
        align: BYTES_VALUE_ALIGN,
        copy: BYTES_COPY_SYMBOL,
        move_: None,
        destroy: BYTES_DESTROY_SYMBOL,
        is_immutable_shareable: true,
    };

    /// The witness for `String` — refcounted copy-on-write, immutable-shareable.
    ///
    /// `copy = hew_string_clone` (retain: bump the C-string header refcount,
    /// alias one buffer), `destroy = hew_string_drop` (release: decrement, free
    /// at zero, after the static-literal skip), bitwise move, and
    /// `is_immutable_shareable = true` (a `String` may cross an actor boundary
    /// by retain alone — no deep copy, no data race). `size`/`align` are those
    /// of the by-value data pointer; the refcount header lives in the heap
    /// allocation, not the handle.
    pub const STRING: Self = Self {
        size: STRING_VALUE_SIZE,
        align: STRING_VALUE_ALIGN,
        copy: STRING_COPY_SYMBOL,
        move_: None,
        destroy: STRING_DESTROY_SYMBOL,
        is_immutable_shareable: true,
    };

    /// The witness for `Vec` — a refcounted heap container, but **not**
    /// immutable-shareable.
    ///
    /// `copy = hew_vec_clone_managed` (deep-copy the backing buffer; `String`
    /// elements are retained on the header-aware refcount discipline),
    /// `destroy = hew_vec_free_managed` (free the buffer; release `String`
    /// elements at zero), and bitwise move (ownership transfers with the handle
    /// bits — no runtime call). `is_immutable_shareable = false`: a `Vec` is
    /// **mutable**, so it may NOT cross an actor boundary by retain alone (the
    /// sendability gate later in the epic consults this flag — B-INV-3).
    /// `size`/`align` are those of the by-value `*mut HewVec` handle; the
    /// length/capacity/elements live in the heap allocation, not the handle.
    ///
    /// Declarative only: nothing consumes the witness yet (P3 reads it). The
    /// element-ownership migration that makes `hew_vec_*_managed` header-aware
    /// (W5.011 P2b-vec) is what makes binding `Vec` here safe.
    pub const VEC: Self = Self {
        size: VEC_VALUE_SIZE,
        align: VEC_VALUE_ALIGN,
        copy: VEC_COPY_SYMBOL,
        move_: None,
        destroy: VEC_DESTROY_SYMBOL,
        is_immutable_shareable: false,
    };

    /// The witness for `HashMap` — a refcounted heap container, but **not**
    /// immutable-shareable.
    ///
    /// `copy = hew_hashmap_clone_layout` (deep-copy the table from its embedded
    /// key/value layout descriptors; `String` keys/values are retained on the
    /// header-aware refcount discipline via `clone_layout_string_blob` →
    /// `hew_string_clone`), `destroy = hew_hashmap_free_layout` (free the table;
    /// release `String` elements at zero through the layout drop thunk), and
    /// bitwise move (ownership transfers with the handle bits — no runtime
    /// call). `is_immutable_shareable = false`: a `HashMap` is **mutable**, so
    /// it may NOT cross an actor boundary by retain alone (the sendability gate
    /// later in the epic consults this flag — B-INV-3). `size`/`align` are those
    /// of the by-value `*mut HewHashMap` handle; the buckets and entries live in
    /// the heap allocation, not the handle.
    ///
    /// Declarative only: nothing consumes the witness yet (P3 reads it). What
    /// makes binding `HashMap` here safe is that map string ingress is already
    /// a coherent header-aware refcount discipline under an ownership-transfer
    /// MOVE: `hew_hashmap_insert_layout` MOVEs an already-header-aware string by
    /// pointer-bit copy (no `strdup`, no headerless producer), and clone/drop
    /// are the matched retain/release (W5.011 P2b-maps). The MOVE pairing — one
    /// owner at a time, not aliasing — is why the byte-copy ingress is sound
    /// (LESSONS `alias-byte-copy-not-semantic-clone`).
    pub const HASHMAP: Self = Self {
        size: HASHMAP_VALUE_SIZE,
        align: HASHMAP_VALUE_ALIGN,
        copy: HASHMAP_COPY_SYMBOL,
        move_: None,
        destroy: HASHMAP_DESTROY_SYMBOL,
        is_immutable_shareable: false,
    };

    /// The witness for `HashSet` — a refcounted heap container, but **not**
    /// immutable-shareable.
    ///
    /// `copy = hew_hashset_clone_layout`, `destroy = hew_hashset_free_layout`,
    /// bitwise move, pointer-sized handle, `is_immutable_shareable = false`. A
    /// `HashSet<T>` is a thin wrapper whose element *is* the map key, so it
    /// inherits the header-aware key contract verbatim: for `String` elements
    /// the clone retains via `hew_string_clone` and the drop releases at zero.
    /// Like `HashMap` it is **mutable** → not sendable by retain alone (B-INV-3).
    ///
    /// Declarative only: nothing consumes the witness yet (P3 reads it).
    pub const HASHSET: Self = Self {
        size: HASHSET_VALUE_SIZE,
        align: HASHSET_VALUE_ALIGN,
        copy: HASHSET_COPY_SYMBOL,
        move_: None,
        destroy: HASHSET_DESTROY_SYMBOL,
        is_immutable_shareable: false,
    };

    /// Derive the layout witness for `ty`, routing through the same
    /// [`ValueClass::of_ty`] authority that classifies the type so the witness
    /// is never derived from a parallel table (LESSONS `type-info-survival`).
    ///
    /// Returns `None` for any type not *yet* bound to a witness. P1 bound
    /// `Bytes` (the template), P2a binds `String`, P2b-vec binds `Vec`, and
    /// P2b-maps binds `HashMap`/`HashSet`; the remaining COW heap types
    /// (Array/Tuple) and `Unknown` user structs bind in a later phase. `None`
    /// here means "no witness bound", explicitly — it is not a silent failure.
    /// Callers that *require* a witness must themselves fail closed on `None`
    /// (CLAUDE §2).
    ///
    /// Fail-closed type boundary (CLAUDE §3): `ResolvedTy` is post-inference
    /// and carries no inference variable, so no `Ty::Var` can reach this
    /// derivation; every input resolves to a concrete [`ValueClass`].
    #[must_use]
    pub fn of_ty(ty: &ResolvedTy, type_classes: &TypeClassTable) -> Option<Self> {
        match ValueClass::of_ty(ty, type_classes) {
            // Heap value classes are witness-bearing. P1 bound `Bytes`, P2a
            // binds `String`, W5.011 P2b-vec binds `Vec`, and W5.011 P2b-maps
            // binds `HashMap`/`HashSet` (their string-element ownership is now
            // a header-aware refcounted MOVE discipline). The remaining COW
            // builtins (Array/Tuple) and `Unknown` user structs bind in a later
            // phase and return `None` (not-yet-bound) until then.
            ValueClass::CowValue | ValueClass::Unknown => match ty {
                ResolvedTy::Bytes => Some(Self::BYTES),
                ResolvedTy::String => Some(Self::STRING),
                ResolvedTy::Named {
                    builtin: Some(BuiltinType::Vec),
                    ..
                } => Some(Self::VEC),
                ResolvedTy::Named {
                    builtin: Some(BuiltinType::HashMap),
                    ..
                } => Some(Self::HASHMAP),
                ResolvedTy::Named {
                    builtin: Some(BuiltinType::HashSet),
                    ..
                } => Some(Self::HASHSET),
                _ => None,
            },
            // Non-heap classes never carry a heap-value witness: `BitCopy`
            // relocates trivially; `View` is non-owning; `PersistentShare`,
            // `AffineResource`, and `Linear` have their own (non-VWT)
            // ownership disciplines in v0.5.
            ValueClass::BitCopy
            | ValueClass::View
            | ValueClass::PersistentShare
            | ValueClass::AffineResource
            | ValueClass::Linear => None,
        }
    }
}

#[must_use]
pub fn contains_named_type(ty: &ResolvedTy) -> bool {
    !named_type_names(ty).is_empty()
}

#[must_use]
pub fn named_type_names(ty: &ResolvedTy) -> Vec<String> {
    named_type_components(ty)
        .into_iter()
        .map(|component| component.name)
        .collect()
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NamedTypeComponent {
    pub name: String,
    pub builtin: Option<BuiltinType>,
    pub has_args: bool,
}

#[must_use]
pub fn named_type_components(ty: &ResolvedTy) -> Vec<NamedTypeComponent> {
    let mut components = Vec::new();
    collect_named_type_components(ty, &mut components);
    components
}

fn collect_named_type_components(ty: &ResolvedTy, components: &mut Vec<NamedTypeComponent>) {
    match ty {
        ResolvedTy::Tuple(elems) => {
            for elem in elems {
                collect_named_type_components(elem, components);
            }
        }
        ResolvedTy::Array(elem, _) | ResolvedTy::Slice(elem) => {
            collect_named_type_components(elem, components);
        }
        ResolvedTy::Named {
            name,
            args,
            builtin,
        } => {
            components.push(NamedTypeComponent {
                name: name.clone(),
                builtin: *builtin,
                has_args: !args.is_empty(),
            });
            for arg in args {
                collect_named_type_components(arg, components);
            }
        }
        ResolvedTy::Function { params, ret } => {
            for param in params {
                collect_named_type_components(param, components);
            }
            collect_named_type_components(ret, components);
        }
        ResolvedTy::Closure {
            params,
            ret,
            captures,
        } => {
            for param in params {
                collect_named_type_components(param, components);
            }
            collect_named_type_components(ret, components);
            for capture in captures {
                collect_named_type_components(capture, components);
            }
        }
        ResolvedTy::Pointer { pointee, .. } | ResolvedTy::Borrow { pointee } => {
            collect_named_type_components(pointee, components);
        }
        ResolvedTy::TraitObject { traits } => {
            for bound in traits {
                for arg in &bound.args {
                    collect_named_type_components(arg, components);
                }
                for (_, ty) in &bound.assoc_bindings {
                    collect_named_type_components(ty, components);
                }
            }
        }
        // Task<T> is compiler-internal; recurse into T so that a
        // `Task<SomeResource>` binding is still diagnosed correctly if T
        // is a named type with a resource/linear marker.
        ResolvedTy::Task(inner) => collect_named_type_components(inner, components),
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
        | ResolvedTy::String
        | ResolvedTy::Bytes
        | ResolvedTy::CancellationToken
        | ResolvedTy::Duration
        | ResolvedTy::Unit
        | ResolvedTy::Never
        // A structural type parameter is abstract, not a named user type, so
        // it contributes no named-type component.
        | ResolvedTy::TypeParam { .. } => {}
    }
}

#[cfg(test)]
mod tests {
    use super::{named_type_components, named_type_names};
    use hew_types::BuiltinType;
    use hew_types::{ResolvedTraitBound, ResolvedTy};

    #[test]
    fn trait_object_names_are_not_reported_as_unknown_named_types() {
        let ty = ResolvedTy::TraitObject {
            traits: vec![ResolvedTraitBound {
                trait_name: "Display".to_string(),
                args: Vec::new(),
                assoc_bindings: Vec::new(),
            }],
        };

        assert!(named_type_names(&ty).is_empty());
    }

    #[test]
    fn trait_object_type_arguments_still_report_nested_named_types() {
        let ty = ResolvedTy::TraitObject {
            traits: vec![ResolvedTraitBound {
                trait_name: "Iterator".to_string(),
                args: vec![ResolvedTy::Named {
                    name: "Foo".to_string(),
                    args: Vec::new(),
                    builtin: None,
                }],
                assoc_bindings: Vec::new(),
            }],
        };

        assert_eq!(named_type_names(&ty), vec!["Foo".to_string()]);
    }

    #[test]
    fn trait_object_nested_arguments_recurse_without_reporting_trait_names() {
        let ty = ResolvedTy::TraitObject {
            traits: vec![ResolvedTraitBound {
                trait_name: "OuterTrait".to_string(),
                args: vec![ResolvedTy::Tuple(vec![
                    ResolvedTy::Named {
                        name: "Foo".to_string(),
                        args: Vec::new(),
                        builtin: None,
                    },
                    ResolvedTy::TraitObject {
                        traits: vec![ResolvedTraitBound {
                            trait_name: "InnerTrait".to_string(),
                            args: vec![ResolvedTy::Named {
                                name: "Bar".to_string(),
                                args: Vec::new(),
                                builtin: None,
                            }],
                            assoc_bindings: Vec::new(),
                        }],
                    },
                ])],
                assoc_bindings: Vec::new(),
            }],
        };

        assert_eq!(
            named_type_names(&ty),
            vec!["Foo".to_string(), "Bar".to_string()]
        );
    }

    #[test]
    fn named_type_components_preserve_builtin_discriminator_and_arg_shape() {
        let ty = ResolvedTy::Named {
            name: "Vec".to_string(),
            args: vec![ResolvedTy::Named {
                name: "Foo".to_string(),
                args: Vec::new(),
                builtin: None,
            }],
            builtin: Some(BuiltinType::Vec),
        };

        let components = named_type_components(&ty);
        assert_eq!(components.len(), 2);
        assert_eq!(components[0].name, "Vec");
        assert_eq!(components[0].builtin, Some(BuiltinType::Vec));
        assert!(components[0].has_args);
        assert_eq!(components[1].name, "Foo");
        assert_eq!(components[1].builtin, None);
        assert!(!components[1].has_args);
    }
}

#[cfg(test)]
mod layout_witness_tests {
    use super::{
        LayoutWitness, TypeClassTable, ValueClass, BYTES_COPY_SYMBOL, BYTES_DESTROY_SYMBOL,
        HASHMAP_COPY_SYMBOL, HASHMAP_DESTROY_SYMBOL, HASHSET_COPY_SYMBOL, HASHSET_DESTROY_SYMBOL,
        STRING_COPY_SYMBOL, STRING_DESTROY_SYMBOL, VEC_COPY_SYMBOL, VEC_DESTROY_SYMBOL,
    };
    use hew_types::{BuiltinType, ResolvedTraitBound, ResolvedTy};

    /// Golden test: the derived witness for `Bytes` reproduces the CURRENT
    /// `Bytes` runtime behaviour exactly — same retain/release symbols, same
    /// size/align, immutable-shareable, bitwise move. P1 changes no behaviour;
    /// this pins the binding so any future drift from the runtime ABI fails
    /// here rather than silently miscompiling.
    #[test]
    fn bytes_witness_reproduces_current_bytes_behaviour() {
        let table = TypeClassTable::new();
        let witness = LayoutWitness::of_ty(&ResolvedTy::Bytes, &table)
            .expect("Bytes must bind to a layout witness in P1");

        // Retain/release symbols are exactly the runtime's COW refcount ops:
        // `hew-runtime/src/bytes.rs::hew_bytes_clone_ref` / `hew_bytes_drop`.
        assert_eq!(witness.copy, "hew_bytes_clone_ref");
        assert_eq!(witness.destroy, "hew_bytes_drop");
        assert_eq!(witness.copy, BYTES_COPY_SYMBOL);
        assert_eq!(witness.destroy, BYTES_DESTROY_SYMBOL);

        // Refcounted COW values relocate bitwise: ownership rides the bits,
        // no refcount change, no runtime call.
        assert_eq!(witness.move_, None);

        // Bytes is deeply immutable COW → shareable across actors by retain.
        assert!(witness.is_immutable_shareable);

        // Size/align of the `Bytes` value triple `{ ptr, offset:u32, len:u32 }`
        // (`hew-runtime/src/bytes.rs` `BytesTriple`). Pinned for 64-bit targets
        // (CI: arm64 / x86_64); other pointer widths recompute from the same
        // repr(C) layout and are validated by `bytes_witness_size_align_match_triple_layout`.
        #[cfg(target_pointer_width = "64")]
        {
            assert_eq!(witness.size, 16);
            assert_eq!(witness.align, 8);
        }
    }

    /// Cross-platform companion to the golden: the witness size/align are
    /// derived from the real repr(C) `Bytes` triple layout, so they track
    /// pointer width on every target (CLAUDE §7) rather than hard-coded
    /// numbers that would be wrong on wasm32/ilp32.
    #[test]
    fn bytes_witness_size_align_match_triple_layout() {
        #[repr(C)]
        struct TripleAbi {
            ptr: *const u8,
            offset: u32,
            len: u32,
        }
        let witness = LayoutWitness::BYTES;
        assert_eq!(witness.size, core::mem::size_of::<TripleAbi>() as u64);
        assert_eq!(witness.align, core::mem::align_of::<TripleAbi>() as u64);
    }

    /// Golden test: the derived witness for `String` matches the runtime's
    /// refcounted copy-on-write ops. `copy = hew_string_clone` (retain),
    /// `destroy = hew_string_drop` (release), immutable-shareable, bitwise move,
    /// pointer-sized handle. Pins the P2a binding so any drift from the runtime
    /// symbols fails here rather than silently miscompiling.
    #[test]
    fn string_witness_binds_refcount_cow_ops() {
        let table = TypeClassTable::new();
        let witness = LayoutWitness::of_ty(&ResolvedTy::String, &table)
            .expect("String must bind to a layout witness in P2a");

        // Retain/release symbols are exactly the runtime's COW refcount ops:
        // `hew-runtime/src/string.rs::hew_string_clone` / `hew_string_drop`.
        assert_eq!(witness.copy, "hew_string_clone");
        assert_eq!(witness.destroy, "hew_string_drop");
        assert_eq!(witness.copy, STRING_COPY_SYMBOL);
        assert_eq!(witness.destroy, STRING_DESTROY_SYMBOL);

        // Refcounted COW value: bitwise relocation, no runtime move hook.
        assert_eq!(witness.move_, None);

        // String is deeply immutable COW → shareable across actors by retain
        // (B-INV-3: this is independent of `copy`).
        assert!(witness.is_immutable_shareable);

        // A `String` value is a single data pointer; the refcount header lives
        // in the heap allocation, not the by-value handle.
        assert_eq!(witness.size, core::mem::size_of::<*const u8>() as u64);
        assert_eq!(witness.align, core::mem::align_of::<*const u8>() as u64);
    }

    /// Golden test: the derived witness for `Vec` binds the witness-managed
    /// runtime ops. `copy = hew_vec_clone_managed`, `destroy =
    /// hew_vec_free_managed`, bitwise move, pointer-sized handle, and — crucially
    /// — `is_immutable_shareable = false` (a `Vec` is mutable, so it is NOT
    /// sendable by retain alone). Pins the W5.011 P2b-vec binding.
    #[test]
    fn vec_witness_binds_managed_ops_and_is_not_shareable() {
        let table = TypeClassTable::new();
        let vec_ty = ResolvedTy::Named {
            name: "Vec".to_string(),
            args: vec![ResolvedTy::String],
            builtin: Some(BuiltinType::Vec),
        };
        let witness = LayoutWitness::of_ty(&vec_ty, &table)
            .expect("Vec must bind to a layout witness in P2b-vec");

        // Copy/destroy are exactly the runtime's witness-managed Vec ops:
        // `hew-runtime/src/vec.rs::hew_vec_clone_managed` / `hew_vec_free_managed`.
        assert_eq!(witness.copy, "hew_vec_clone_managed");
        assert_eq!(witness.destroy, "hew_vec_free_managed");
        assert_eq!(witness.copy, VEC_COPY_SYMBOL);
        assert_eq!(witness.destroy, VEC_DESTROY_SYMBOL);

        // Handle relocates bitwise, no runtime move hook.
        assert_eq!(witness.move_, None);

        // A `Vec` is MUTABLE → NOT immutable-shareable (B-INV-3: the sendability
        // gate must refuse to share a Vec across an actor boundary by retain).
        assert!(!witness.is_immutable_shareable);

        // A `Vec` value is a single opaque `*mut HewVec` handle.
        assert_eq!(witness.size, core::mem::size_of::<*const u8>() as u64);
        assert_eq!(witness.align, core::mem::align_of::<*const u8>() as u64);

        // The element type does not change the handle witness.
        let vec_of_u8 = ResolvedTy::Named {
            name: "Vec".to_string(),
            args: vec![ResolvedTy::U8],
            builtin: Some(BuiltinType::Vec),
        };
        assert_eq!(
            LayoutWitness::of_ty(&vec_of_u8, &table),
            Some(LayoutWitness::VEC)
        );
    }

    /// Golden test: the derived witness for `HashMap` binds the layout-managed
    /// runtime ops. `copy = hew_hashmap_clone_layout`, `destroy =
    /// hew_hashmap_free_layout`, bitwise move, pointer-sized handle, and —
    /// crucially — `is_immutable_shareable = false` (a `HashMap` is mutable, so
    /// it is NOT sendable by retain alone). Pins the W5.011 P2b-maps binding;
    /// the symbol literals must match codegen's `HASHMAP_*_LAYOUT_SYMBOL`
    /// (`hew-codegen-rs/src/llvm.rs`) or this fails rather than drifting.
    #[test]
    fn hashmap_witness_binds_layout_ops_and_is_not_shareable() {
        let table = TypeClassTable::new();
        let map_ty = ResolvedTy::Named {
            name: "HashMap".to_string(),
            args: vec![ResolvedTy::String, ResolvedTy::String],
            builtin: Some(BuiltinType::HashMap),
        };
        let witness = LayoutWitness::of_ty(&map_ty, &table)
            .expect("HashMap must bind to a layout witness in P2b-maps");

        // Copy/destroy are exactly the runtime's layout-managed HashMap ops:
        // `hew-runtime/src/hashmap.rs::hew_hashmap_clone_layout` /
        // `hew_hashmap_free_layout`.
        assert_eq!(witness.copy, "hew_hashmap_clone_layout");
        assert_eq!(witness.destroy, "hew_hashmap_free_layout");
        assert_eq!(witness.copy, HASHMAP_COPY_SYMBOL);
        assert_eq!(witness.destroy, HASHMAP_DESTROY_SYMBOL);

        // Handle relocates bitwise, no runtime move hook (the string-element
        // MOVE on ingress rides the same bitwise-transfer discipline).
        assert_eq!(witness.move_, None);

        // A `HashMap` is MUTABLE → NOT immutable-shareable (B-INV-3).
        assert!(!witness.is_immutable_shareable);

        // A `HashMap` value is a single opaque handle.
        assert_eq!(witness.size, core::mem::size_of::<*const u8>() as u64);
        assert_eq!(witness.align, core::mem::align_of::<*const u8>() as u64);

        // The key/value types do not change the handle witness.
        let map_of_prims = ResolvedTy::Named {
            name: "HashMap".to_string(),
            args: vec![ResolvedTy::String, ResolvedTy::I32],
            builtin: Some(BuiltinType::HashMap),
        };
        assert_eq!(
            LayoutWitness::of_ty(&map_of_prims, &table),
            Some(LayoutWitness::HASHMAP)
        );
    }

    /// Golden test: the derived witness for `HashSet` binds the layout-managed
    /// runtime ops. `copy = hew_hashset_clone_layout`, `destroy =
    /// hew_hashset_free_layout`, bitwise move, pointer-sized handle,
    /// `is_immutable_shareable = false`. Pins the W5.011 P2b-maps binding; a
    /// `HashSet` inherits the header-aware key contract (element = map key).
    #[test]
    fn hashset_witness_binds_layout_ops_and_is_not_shareable() {
        let table = TypeClassTable::new();
        let set_ty = ResolvedTy::Named {
            name: "HashSet".to_string(),
            args: vec![ResolvedTy::String],
            builtin: Some(BuiltinType::HashSet),
        };
        let witness = LayoutWitness::of_ty(&set_ty, &table)
            .expect("HashSet must bind to a layout witness in P2b-maps");

        assert_eq!(witness.copy, "hew_hashset_clone_layout");
        assert_eq!(witness.destroy, "hew_hashset_free_layout");
        assert_eq!(witness.copy, HASHSET_COPY_SYMBOL);
        assert_eq!(witness.destroy, HASHSET_DESTROY_SYMBOL);
        assert_eq!(witness.move_, None);
        assert!(!witness.is_immutable_shareable);
        assert_eq!(witness.size, core::mem::size_of::<*const u8>() as u64);
        assert_eq!(witness.align, core::mem::align_of::<*const u8>() as u64);

        // The element type does not change the handle witness.
        let set_of_prims = ResolvedTy::Named {
            name: "HashSet".to_string(),
            args: vec![ResolvedTy::I64],
            builtin: Some(BuiltinType::HashSet),
        };
        assert_eq!(
            LayoutWitness::of_ty(&set_of_prims, &table),
            Some(LayoutWitness::HASHSET)
        );
    }

    /// After P2b-maps, `Bytes`, `String`, `Vec`, `HashMap`, and `HashSet` are
    /// bound; the remaining COW heap builtins (Array/Tuple) must still report
    /// "not yet bound" (`None`) — binding them is a later phase. Guards against
    /// accidentally widening the binding.
    #[test]
    fn only_bytes_string_vec_and_maps_are_bound_after_p2b_maps() {
        let table = TypeClassTable::new();

        // Bound:
        assert!(LayoutWitness::of_ty(&ResolvedTy::Bytes, &table).is_some());
        assert!(LayoutWitness::of_ty(&ResolvedTy::String, &table).is_some());
        assert!(LayoutWitness::of_ty(
            &ResolvedTy::Named {
                name: "Vec".to_string(),
                args: vec![ResolvedTy::U8],
                builtin: Some(BuiltinType::Vec),
            },
            &table
        )
        .is_some());
        assert!(LayoutWitness::of_ty(
            &ResolvedTy::Named {
                name: "HashMap".to_string(),
                args: vec![ResolvedTy::String, ResolvedTy::I32],
                builtin: Some(BuiltinType::HashMap),
            },
            &table
        )
        .is_some());
        assert!(LayoutWitness::of_ty(
            &ResolvedTy::Named {
                name: "HashSet".to_string(),
                args: vec![ResolvedTy::String],
                builtin: Some(BuiltinType::HashSet),
            },
            &table
        )
        .is_some());

        // Not yet bound (later phase): Array/Tuple.
        for ty in [
            ResolvedTy::Array(Box::new(ResolvedTy::U8), 4),
            ResolvedTy::Tuple(vec![ResolvedTy::I32, ResolvedTy::I32]),
        ] {
            // Sanity: these are CowValue (same class as Bytes/String/Vec) yet unbound.
            assert_eq!(ValueClass::of_ty(&ty, &table), ValueClass::CowValue);
            assert_eq!(
                LayoutWitness::of_ty(&ty, &table),
                None,
                "{ty:?} must not be VWT-bound until a later phase"
            );
        }
    }

    /// Non-heap classes never carry a heap-value witness.
    #[test]
    fn non_heap_classes_have_no_witness() {
        let table = TypeClassTable::new();

        // BitCopy primitive.
        assert_eq!(LayoutWitness::of_ty(&ResolvedTy::I64, &table), None);
        // View.
        assert_eq!(
            LayoutWitness::of_ty(&ResolvedTy::Slice(Box::new(ResolvedTy::U8)), &table),
            None
        );
        // PersistentShare (trait object).
        assert_eq!(
            LayoutWitness::of_ty(
                &ResolvedTy::TraitObject {
                    traits: vec![ResolvedTraitBound {
                        trait_name: "Display".to_string(),
                        args: Vec::new(),
                        assoc_bindings: Vec::new(),
                    }],
                },
                &table
            ),
            None
        );
        // Unknown user struct.
        assert_eq!(
            LayoutWitness::of_ty(
                &ResolvedTy::Named {
                    name: "MyStruct".to_string(),
                    args: Vec::new(),
                    builtin: None,
                },
                &table
            ),
            None
        );
    }
}
