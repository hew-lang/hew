//! Clone-synthesizability predicate for the enum-composite drop prover.
//!
//! Carved out of `composite_own.rs` as a coherent concern (the line-ceiling
//! ratchet's intended remedy — a pure move, no IR change). It answers the one
//! question the `string_binder_read_is_user_fn_borrow` / borrow-safe-terminator
//! exemption depends on: can the `EnumInPlace` clone half be synthesized for
//! every possible active payload of a candidate composite?

use hew_types::ResolvedTy;

/// True when every variant payload of the tagged-union enum behind `ty` is
/// clone-synthesizable by the `EnumInPlace` helper family: a bit-copy value, a
/// plain `string` leaf, or a nested enum whose own payloads recursively satisfy
/// the same.
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
/// ## Why the payload predicate is a POSITIVE bit-copy test
///
/// The obvious spelling — "`string`, or anything the heap authority says owns
/// no heap" — is not a bit-copy predicate and does not bound the clone
/// synthesis at all. `Stream<i64>` / `Sink<i64>` are pointer-backed IO handles:
/// [`crate::model::ty_owns_heap_mir`]'s builtin leaf set omits `Stream`/`Sink`
/// and its generic `Named` arm only recurses into type arguments and layouts,
/// so with scalar arguments both answer "owns no heap" — while
/// `hew-mir/src/state_clone.rs` classifies them as `IoHandle`s with no
/// duplication helper, which clone totality rejects along with every
/// closure-pair, `#[resource]`, and opaque-handle class. The
/// `!ty_owns_heap_mir` spelling therefore re-admitted exactly the composites
/// the bound exists to exclude.
///
/// [`ty_is_bit_copy_payload`] is the conservative alternative the payload leaf
/// actually needs: a scalar leaf, or a tuple/array built only from such leaves.
/// Every `Named` payload — builtin handle, user record, nested enum, opaque,
/// resource — answers `false` and keeps its composite on the pre-existing
/// fail-closed posture (it keeps leaking, exactly as before, and still
/// compiles).
///
/// A NESTED enum payload (`Ok(Status)` where `Status` is itself a heap-owning
/// enum with a `string` and an `i64` variant) recurses: it is clone-synthesizable
/// exactly when ITS variant payloads are, so the `EnumInPlace` clone half — which
/// walks the nested enum's own tag-aware helper — synthesizes without hitting a
/// dup-less leaf. This is the `Result<Status, i64>` shape #2717's substrate
/// reproduction exercises; without the recursion the outer `Result` failed the
/// predicate on its `Named` payload and disabled the borrow exemption for the
/// inner `Described(s)` string read, leaking the payload.
///
/// Fail-closed: an unresolvable layout, an indirect (heap-boxed) enum, a record /
/// handle / resource `Named` payload, or any payload that is neither `string`,
/// bit-copy, nor a synthesizable nested enum answers `false`. A recursion budget
/// bounds pathological mutually-nested layouts (a genuinely recursive enum is
/// `is_indirect` and already short-circuits).
pub(super) fn enum_payloads_are_clone_synthesizable(
    ty: &ResolvedTy,
    enum_layouts: &[crate::model::EnumLayout],
) -> bool {
    fn field_is_synthesizable(
        field_ty: &ResolvedTy,
        enum_layouts: &[crate::model::EnumLayout],
        depth: u32,
    ) -> bool {
        if matches!(field_ty, ResolvedTy::String) || ty_is_bit_copy_payload(field_ty) {
            return true;
        }
        // A nested enum payload is synthesizable iff its own payloads are —
        // recurse under a depth budget so a pathological non-`is_indirect`
        // nesting cannot diverge. Non-enum `Named` payloads (records, handles,
        // resources) stay fail-closed.
        if depth == 0 {
            return false;
        }
        let ResolvedTy::Named { name, args, .. } = field_ty else {
            return false;
        };
        let Some(layout) = crate::model::find_enum_layout(name, args, enum_layouts) else {
            return false;
        };
        if layout.is_indirect {
            return false;
        }
        layout.variants.iter().all(|variant| {
            variant
                .field_tys
                .iter()
                .all(|inner| field_is_synthesizable(inner, enum_layouts, depth - 1))
        })
    }
    let ResolvedTy::Named { name, args, .. } = ty else {
        return false;
    };
    let layout = crate::model::find_enum_layout(name, args, enum_layouts);
    let Some(layout) = layout else {
        return false;
    };
    if layout.is_indirect {
        return false;
    }
    layout.variants.iter().all(|variant| {
        variant
            .field_tys
            .iter()
            .all(|field_ty| field_is_synthesizable(field_ty, enum_layouts, 8))
    })
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
