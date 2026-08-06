//! Clone-synthesizability predicate for the enum-composite drop prover.
//!
//! Carved out of `composite_own.rs` as a coherent concern (the line-ceiling
//! ratchet's intended remedy — a pure move, no IR change). It answers the one
//! question the `string_binder_read_is_user_fn_borrow` / borrow-safe-terminator
//! exemption depends on: can the `EnumInPlace` clone half be synthesized for
//! every possible active payload of a candidate composite?

use hew_hir::{TypeClassTable, ValueClass};
use hew_types::ResolvedTy;
use std::collections::HashMap;

/// True when every heap leaf reachable through a candidate composite `ty` is
/// clone-synthesizable by the `EnumInPlace` helper family: a `string`, a
/// bit-copy scalar, an owned `Vec` of the same, or a nested
/// enum/record/tuple/array built only from such leaves — AND no leaf, at any
/// depth, is affine (`#[resource]` / `@linear` / an owned runtime handle).
///
/// This bounds the blast radius of the `string_binder_read_is_user_fn_borrow`
/// exemption. `note_payload_escape` is deliberately coarse — one escaping
/// binder excludes EVERY candidate root in the function — so the inverse is
/// also coarse: clearing one binder can readmit every candidate. Readmitting a
/// composite is not free: an `EnumInPlace` drop makes codegen synthesise the
/// whole in-place helper family for that layout, and the clone half of that
/// family fails closed on payloads with no dup symbol (`Stream` / `Sink` /
/// `Generator` / `CancellationToken` handles, registry-backed resources). A
/// `Result<(Stream<string>, Sink<string>), string>` scrutinee whose `Err(e)`
/// binder is interpolated would otherwise turn a leak into a hard
/// `E_NOT_YET_IMPLEMENTED` compile failure.
///
/// ## Why the affine gate is checked FIRST, by value-class
///
/// A `#[resource]` record (`#[resource] type Conn { fd: i64 }`) is affine: it
/// has an implicit `close(consuming self)` discharged exactly once per exit by
/// the affine-release machinery, and no duplication helper at all. But it is
/// ALSO a field-bearing record, so it appears in `record_field_orders` keyed
/// like any plain value record — recursing into its `fd: i64` field would find
/// only bit-copy leaves and wrongly admit it. Admitting it seeds an
/// `EnumInPlace` drop for the composite, which then closes the resource a
/// SECOND time (double close) on top of its own affine close. The observable
/// bug was `Result<Conn, string>` matched in a loop closing each `Ok` payload
/// twice.
///
/// [`ValueClass::of_ty`] is the authoritative value-class oracle: it returns
/// `AffineResource` for every `#[resource]` record, opaque resource, and owned
/// runtime handle (`Generator` / `AsyncGenerator` / `Rc` / `Weak` /
/// `CancellationToken`), and `Linear` for `@linear` / `Task` values. Rejecting
/// those classes up front — before any positive leaf admit — is what "every
/// leaf clone-drop-safe AND none affine" requires, and it is a pure tightening:
/// it only ever removes an admission.
///
/// ## Why the positive test is clone-drop-safe by shape, not "owns no heap"
///
/// The obvious spelling — "`string`, or anything the heap authority says owns
/// no heap" — does not bound the clone synthesis at all. `Stream<i64>` /
/// `Sink<i64>` are pointer-backed IO handles: [`crate::model::ty_owns_heap_mir`]'s
/// builtin leaf set omits `Stream`/`Sink` and its generic `Named` arm only
/// recurses into type arguments and layouts, so with scalar arguments both
/// answer "owns no heap" — while `hew-mir/src/state_clone.rs` classifies them as
/// `IoHandle`s with no duplication helper. The `!ty_owns_heap_mir` spelling
/// therefore re-admitted exactly the composites the bound exists to exclude.
///
/// [`payload_leaf_is_clone_drop_safe`] is the conservative alternative: every
/// heap leaf reachable through the composite must itself clone AND drop. It
/// recurses through the shapes whose `EnumInPlace` helper family can be
/// synthesized leaf-by-leaf:
/// - `string`, `bytes`, and bit-copy scalars are always safe (`string` and
///   `bytes` are refcounted `CoW` values sharing the `hew_{string,bytes}_clone_ref`
///   dup + `hew_{string,bytes}_drop` inverse family);
/// - a tuple / fixed-size array is safe iff every element is;
/// - an owned `Vec<T>` clones and drops element-wise, safe iff its element is;
/// - a nested inline enum recurses into every variant payload (`Ok(Status)`
///   where `Status` is itself a `Loaded(i64) | Described(string)` enum stays
///   synthesizable — #2717's scalar-and-string nested-enum sibling that the
///   flat plain-string cap leaked);
/// - a nested value record recurses into every registered field.
///
/// Fail-closed: an unresolvable layout, an indirect (heap-boxed) enum, a
/// generic record whose fields resolve only after substitution, a
/// closure/borrow leaf, or ANY affine leaf answers `false` and keeps its
/// composite on the pre-existing fail-closed leak posture — never a double-free
/// or double-close. A recursion budget bounds pathological mutually-nested
/// layouts (a genuinely recursive enum is `is_indirect` and short-circuits).
pub(super) fn enum_payloads_are_clone_synthesizable(
    ty: &ResolvedTy,
    enum_layouts: &[crate::model::EnumLayout],
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    type_classes: &TypeClassTable,
) -> bool {
    payload_leaf_is_clone_drop_safe(ty, enum_layouts, record_field_orders, type_classes, 0)
}
/// Whether every heap leaf reachable through `ty` is clone-and-drop-safe. See
/// [`enum_payloads_are_clone_synthesizable`] for the shape table and the
/// fail-closed rule. `depth` is a defensive backstop; inline nesting is already
/// bounded by finite type size, and a genuinely recursive enum short-circuits on
/// `is_indirect`.
fn payload_leaf_is_clone_drop_safe(
    ty: &ResolvedTy,
    enum_layouts: &[crate::model::EnumLayout],
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    type_classes: &TypeClassTable,
    depth: u32,
) -> bool {
    if depth > 32 {
        return false;
    }
    // Affine leaves (`#[resource]` records, opaque resources, owned runtime
    // handles, `@linear` / `Task` values) carry their own consume-once close
    // discipline and have no clone helper. Refuse the WHOLE composite the moment
    // one appears — checked BEFORE the positive admits so a `#[resource]` record
    // cannot slip through the value-record arm below and earn a second close.
    if matches!(
        ValueClass::of_ty(ty, type_classes),
        ValueClass::AffineResource | ValueClass::Linear
    ) {
        return false;
    }
    // `string`, `bytes`, and scalars are always clone-and-drop-safe leaves.
    // `bytes` is a refcounted CoW value with the same intrinsic dup+drop family
    // as `string` (`hew_bytes_clone_ref` / `hew_bytes_drop`), so its
    // `EnumInPlace` helper family has both halves — admitting it is sound and
    // frees the payload exactly once. The affine gate above still refuses a
    // `bytes` leaf sitting BESIDE an affine/IO/resource sibling (the `.all()`
    // conjunction), keeping the arm binder the sole close authority there.
    if matches!(ty, ResolvedTy::String | ResolvedTy::Bytes) || ty_is_bit_copy_payload(ty) {
        return true;
    }
    let recurse = |leaf: &ResolvedTy| {
        payload_leaf_is_clone_drop_safe(
            leaf,
            enum_layouts,
            record_field_orders,
            type_classes,
            depth + 1,
        )
    };
    match ty {
        ResolvedTy::Tuple(elems) => elems.iter().all(recurse),
        ResolvedTy::Array(elem, _) => recurse(elem),
        // A `Vec<T>` clones/drops element-wise (safe iff its element is); a nested
        // inline enum recurses into every variant payload, a nested record into
        // every field. An indirect enum, or a generic record whose fields resolve
        // only after substitution, is left on the fail-closed leak posture.
        ResolvedTy::Named { args, .. } if crate::lower::drop_plan::ty_is_vec(ty) => {
            args.first().is_some_and(&recurse)
        }
        ResolvedTy::Named { name, args, .. } => {
            if let Some(layout) = crate::model::find_enum_layout(name, args, enum_layouts) {
                return !layout.is_indirect
                    && layout
                        .variants
                        .iter()
                        .all(|variant| variant.field_tys.iter().all(&recurse));
            }
            record_field_orders
                .get(name)
                .is_some_and(|fields| fields.iter().all(|(_, field_ty)| recurse(field_ty)))
        }
        _ => false,
    }
}
/// True when `ty` is a payload that is copied bit-for-bit and owns nothing: a
/// scalar leaf, or a tuple / fixed-size array built exclusively from such
/// leaves.
///
/// The scalar leaf set is the shared [`crate::return_provenance::ty_is_scalar_non_heap`]
/// authority (the same one the audited extern-return table uses to admit a
/// scalar-return extern), extended here to `char`-sized aggregates of scalars.
/// Deliberately EXHAUSTIVE-by-rejection: every non-listed form — `Named` (which
/// covers `Stream`/`Sink`/`Generator`/`CancellationToken` and resources, every
/// user record and nested enum, every `#[opaque]` handle and `#[resource]`),
/// `String`, `Bytes`, `Slice`, `Function`, `Closure`, `Pointer`, `Borrow`,
/// `TraitObject`, `Task`, `TypeParam` — answers `false`.
fn ty_is_bit_copy_payload(ty: &ResolvedTy) -> bool {
    match ty {
        ResolvedTy::Tuple(elems) => elems.iter().all(ty_is_bit_copy_payload),
        ResolvedTy::Array(elem, _) => ty_is_bit_copy_payload(elem),
        other => crate::return_provenance::ty_is_scalar_non_heap(other),
    }
}
