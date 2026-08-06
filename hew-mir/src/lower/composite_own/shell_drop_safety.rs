//! Shell-drop payload-safety cap for the enum-composite sole-owner prover.
//!
//! Carved from `composite_own.rs` as a sibling concern module (line-ceiling
//! ratchet); the predicate is consumed only by
//! `derive_enum_composite_drop_allowed`'s borrow-exemption cap and pinned by
//! the `plain_string_payload_cap` tests.
use super::{ty_is_bit_copy_payload, HashSet, ResolvedTy};

/// True when every variant payload of the tagged-union enum behind `ty` is a
/// bit-copy value, a plain `string` leaf, or a bare `#[opaque]` handle — the
/// payload classes whose shell drop cannot double-release anything.
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
/// ## Why a BARE `#[opaque]` payload sibling is admitted
///
/// A hybrid enum (`Mixed { Text(string); Opaque(Handle) }`) passed by value
/// into a consuming callee declines the snapshot carrier (not clone-total), so
/// the callee's ONE balancing release is the legacy tag-aware `EnumInPlace`
/// shell drop. Requiring every payload to be plain string excluded that shell
/// the moment the string binder was read (`s.len()`), leaking BOTH variants on
/// every call. Admitting the bare-opaque sibling is per-variant sound:
///
///   * the shell's drop thunk is a structural NO-OP for an `OpaqueHandle`
///     payload (`emit_field_drop_step(OpaqueHandle) => Ok(())` — the documented
///     `.free()`-or-leak contract), and no other close authority exists for a
///     bare opaque handle anywhere in the pipeline, so a double-release of that
///     variant is impossible by construction;
///   * the string variant keeps exactly the pre-existing plain-string
///     discipline: the shell is the sole owner, the binder is a borrow-exempt
///     alias, and any real payload escape still excludes the shell fail-closed;
///   * codegen's clone half emits a trap-on-entry body for an opaque-carrying
///     enum seeded only for its drop helper (`emit_enum_clone_inplace_body`'s
///     `has_nested_opaque` guard), so the synthesis family stays linkable
///     without ever aliasing the handle pointer.
///
/// The discrimination between a bare `#[opaque]` handle (no-op drop) and a
/// `#[resource]`/lifecycle-registered handle (real close — a second close IS
/// observable, the S2200 class) routes through the SAME
/// `classify_state_field_with_lifecycle_registry` authority codegen's thunk
/// synthesis uses, so admission here and emission there cannot disagree.
/// `Resource`, `IoHandle`, record, nested-enum, and every other payload class
/// still answers `false` (fail-closed leak, exactly as before).
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
/// Fail-closed: an unresolvable layout, an indirect (heap-boxed) enum, a
/// classification error, or any payload that is neither `string`, bit-copy,
/// nor bare-opaque answers `false`.
pub(super) fn enum_payloads_are_shell_drop_safe(
    ty: &ResolvedTy,
    enum_layouts: &[crate::model::EnumLayout],
    record_layouts: &[crate::model::RecordLayout],
    opaque_handle_names: &[String],
    lifecycle_registry: &hew_hir::LifecycleRegistry,
) -> bool {
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
        variant.field_tys.iter().all(|field_ty| {
            if matches!(field_ty, ResolvedTy::String) || ty_is_bit_copy_payload(field_ty) {
                return true;
            }
            // Bare `#[opaque]` handle — decided by the one classification
            // authority the codegen thunk synthesis also consults, so a
            // lifecycle-registered (`#[resource]`) handle classifies
            // `Resource` and stays refused here.
            let mut visited = HashSet::new();
            matches!(
                crate::state_clone::classify_state_field_with_lifecycle_registry(
                    field_ty,
                    record_layouts,
                    enum_layouts,
                    opaque_handle_names,
                    lifecycle_registry,
                    &mut visited,
                ),
                Ok(crate::state_clone::StateFieldCloneKind::OpaqueHandle { .. })
            )
        })
    })
}
