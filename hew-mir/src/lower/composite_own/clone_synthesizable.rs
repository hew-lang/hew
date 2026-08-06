//! Clone-synthesizability predicate for the enum-composite drop prover.
//!
//! Carved out of `composite_own.rs` as a coherent concern (the line-ceiling
//! ratchet's intended remedy — a pure move, no IR change). It answers the one
//! question the `string_binder_read_is_user_fn_borrow` / borrow-safe-terminator
//! exemption depends on: can the `EnumInPlace` clone half be synthesized for
//! every possible active payload of a candidate composite?

use hew_types::ResolvedTy;
use std::collections::HashMap;

/// True when every heap leaf reachable through a candidate composite `ty` is
/// clone-synthesizable by the `EnumInPlace` helper family: a `string`, a
/// bit-copy scalar, an owned `Vec` of the same, or a nested
/// enum/record/tuple/array built only from such leaves.
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
/// ## Why the payload predicate is a POSITIVE clone-drop-safe test
///
/// The obvious spelling — "`string`, or anything the heap authority says owns
/// no heap" — does not bound the clone synthesis at all. `Stream<i64>` /
/// `Sink<i64>` are pointer-backed IO handles: [`crate::model::ty_owns_heap_mir`]'s
/// builtin leaf set omits `Stream`/`Sink` and its generic `Named` arm only
/// recurses into type arguments and layouts, so with scalar arguments both
/// answer "owns no heap" — while `hew-mir/src/state_clone.rs` classifies them as
/// `IoHandle`s with no duplication helper, which clone totality rejects along
/// with every closure-pair, `#[resource]`, and opaque-handle class. The
/// `!ty_owns_heap_mir` spelling therefore re-admitted exactly the composites the
/// bound exists to exclude.
///
/// [`payload_leaf_is_clone_drop_safe`] is the conservative alternative the
/// payload leaf actually needs: every heap leaf reachable through the composite
/// must itself clone AND drop. It recurses through the shapes whose
/// `EnumInPlace` helper family can be synthesized leaf-by-leaf:
/// - `string` and bit-copy scalars are always safe;
/// - a tuple / fixed-size array is safe iff every element is;
/// - an owned `Vec<T>` clones and drops element-wise, safe iff its element is;
/// - a nested inline enum recurses into every variant payload (`Ok(Status)`
///   where `Status` is itself a `Loaded(i64) | Described(string)` enum stays
///   synthesizable — #2717's scalar-and-string nested-enum sibling that the
///   flat plain-string cap leaked);
/// - a nested record recurses into every registered field.
///
/// Fail-closed: an unresolvable layout, an indirect (heap-boxed) enum, a
/// generic record whose fields resolve only after substitution, `Bytes`, or any
/// affine / IO / `#[resource]` / opaque / closure leaf answers `false` and keeps
/// its composite on the pre-existing fail-closed leak posture — never a
/// double-free. A recursion budget bounds pathological mutually-nested layouts
/// (a genuinely recursive enum is `is_indirect` and already short-circuits).
pub(super) fn enum_payloads_are_clone_synthesizable(
    ty: &ResolvedTy,
    enum_layouts: &[crate::model::EnumLayout],
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
) -> bool {
    payload_leaf_is_clone_drop_safe(ty, enum_layouts, record_field_orders, 0)
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
    depth: u32,
) -> bool {
    if depth > 32 {
        return false;
    }
    // `string` and scalars are always clone-and-drop-safe leaves.
    if matches!(ty, ResolvedTy::String) || ty_is_bit_copy_payload(ty) {
        return true;
    }
    let recurse = |leaf: &ResolvedTy| {
        payload_leaf_is_clone_drop_safe(leaf, enum_layouts, record_field_orders, depth + 1)
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
