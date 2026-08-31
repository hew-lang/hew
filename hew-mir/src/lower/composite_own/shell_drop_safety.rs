#![allow(
    deprecated,
    reason = "temporary named identity reconstruction migration seam"
)]

//! Shell-drop payload-safety predicate for the enum-composite drop prover.
//!
//! Carved out of `composite_own.rs` as a coherent concern (the line-ceiling
//! ratchet's intended remedy — a pure move, no IR change). It answers the one
//! question the `string_binder_read_is_user_fn_borrow` / borrow-safe-terminator
//! exemption depends on: can the tag-aware `EnumInPlace` shell drop of a
//! candidate composite release every possible active payload without ever
//! double-releasing anything?

use hew_hir::{TypeClassTable, ValueClass};
use hew_types::ResolvedTy;
use std::collections::{HashMap, HashSet};

/// True when a candidate enum's DIRECT variant payload carries a
/// lifecycle-registered resource record (the declared-release carve-out
/// class). Such a payload's shell-drop step is USER code —
/// `__hew_record_drop_inplace_<R>` → `<R>::close(self)` — which, unlike the
/// string/bytes/opaque drop steps, is NOT a no-op over a neutralized (zeroed)
/// payload slot: the variant tag survives the neutralize, so the thunk would
/// close zeroed storage. The prover tracks these candidates so
/// [`note_declared_release_neutralize_exclusions`] can exclude them the
/// moment their payload is handed off.
pub(in crate::lower) fn direct_payload_has_registered_resource_record(
    ty: &ResolvedTy,
    enum_layouts: &[crate::model::EnumLayout],
    lifecycle_registry: &hew_hir::LifecycleRegistry,
) -> bool {
    let ResolvedTy::Named { name, args, .. } = ty else {
        return false;
    };
    crate::model::find_enum_layout(name, args, enum_layouts).is_some_and(|layout| {
        layout.variants.iter().any(|variant| {
            variant.field_tys.iter().any(|field_ty| match field_ty {
                ResolvedTy::Named {
                    name,
                    args,
                    is_opaque: false,
                    ..
                } => {
                    args.is_empty()
                        && lifecycle_registry
                            .resource_record(&hew_types::DefId::legacy_reconstruct_from_full_path(
                                name,
                            ))
                            .is_some()
                }
                _ => false,
            })
        })
    })
}

/// True when every heap leaf reachable through a candidate composite `ty` is
/// shell-drop-safe: a `string`, `bytes`, a bit-copy scalar, an owned `Vec` of
/// the same, a nested enum/record/tuple/array built only from such leaves — or,
/// as a DIRECT variant payload of the candidate enum, a bare `#[opaque]` handle
/// — AND no leaf, at any depth, is affine (`#[resource]` / `@linear` / an owned
/// runtime handle / a lifecycle-registered resource).
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
/// `AffineResource` for every `#[resource]` record, closeable opaque resource,
/// and owned runtime handle (`Generator` / `AsyncGenerator` / `Rc` / `Weak` /
/// `CancellationToken`), and `Linear` for `@linear` / `Task` values. Rejecting
/// those classes up front — before any positive leaf admit — is what "every
/// leaf shell-drop-safe AND none affine" requires, and it is a pure tightening:
/// it only ever removes an admission. A lifecycle-REGISTERED opaque resource
/// must also declare `#[resource]` (`CloseableOpaqueMustBeResource`), so both
/// authorities — the value class here and the lifecycle registry in the
/// bare-opaque discriminator below — refuse it independently.
///
/// ## Why a BARE `#[opaque]` DIRECT variant payload is admitted
///
/// A hybrid enum (`Mixed { Text(string); Opaque(Handle) }`) passed by value
/// into a consuming callee declines the snapshot carrier (not clone-total), so
/// the callee's ONE balancing release is the legacy tag-aware `EnumInPlace`
/// shell drop. Requiring every payload to be clone-drop-safe excluded that
/// shell the moment the string binder was read (`s.len()`), leaking BOTH
/// variants on every call. Admitting the bare-opaque sibling is per-variant
/// sound:
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
///
/// The admission is DIRECT-payload only (`depth == 1`): the no-op field-drop
/// argument above is a property of the candidate shell's own drop steps. An
/// opaque handle reached through a nested `Vec` / enum / record / tuple is
/// released (or not) by that nested aggregate's own synthesized helper family,
/// whose clone half has no dup symbol for the handle — those composites stay on
/// the fail-closed leak posture, exactly as before.
///
/// ## Why the positive test is shell-drop-safe by shape, not "owns no heap"
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
/// [`payload_leaf_is_shell_drop_safe`] is the conservative alternative: every
/// heap leaf reachable through the composite must itself be released exactly
/// once by the shell's helper family. It recurses through the shapes whose
/// `EnumInPlace` helper family can be synthesized leaf-by-leaf:
/// - `string`, `bytes`, and bit-copy scalars are always safe (`string` and
///   `bytes` are refcounted `CoW` values sharing the `hew_{string,bytes}_clone_ref`
///   dup + `hew_{string,bytes}_drop` inverse family);
/// - a bare `#[opaque]` handle as a direct variant payload (no-op drop, no
///   other close authority — see above);
/// - a tuple / fixed-size array is safe iff every element is;
/// - an owned `Vec<T>` clones and drops element-wise, safe iff its element is;
/// - a nested inline enum recurses into every variant payload (`Ok(Status)`
///   where `Status` is itself a `Loaded(i64) | Described(string)` enum stays
///   synthesizable — #2717's scalar-and-string nested-enum sibling that the
///   flat plain-string cap leaked);
/// - a nested value record recurses into every registered field.
///
/// ## Why a DECLARED-RELEASE resource-record DIRECT payload is admitted
///
/// The affine gate's refusal is right when a second close authority exists
/// (the S2200 `Result<Conn, string>` shape, where the arm consumes the payload
/// through the user's `close()`). It is wrong when the declared close is the
/// composite's SOLE release authority: a lifecycle-registered `#[resource]`
/// record whose every field passes the clause-3 authority
/// (`field_is_released_only_by_the_declared_close`) is released ONLY by the
/// shell's thunk chain (`__hew_enum_drop_inplace` →
/// `__hew_record_drop_inplace_<R>` → `<R>::close`) — its binder owns no heap
/// and mints no owner, and an arm that DOES consume it neutralizes the slot,
/// which the prover's neutralize scan turns into a whole-candidate exclusion
/// (a record close is not null-safe over a zeroed slot). Refusing that
/// shape leaves zero releases, not one. The carve-out is DIRECT-payload only
/// (`depth == 1`), mirroring the bare-opaque admission's soundness scope, and
/// its admission/refusal boundary is the same clause-3 conjunction the
/// adoption boundary (`DeclaredReleaseTypes`) applies — a registered record
/// with a teardown-freeable field stays refused.
///
/// Fail-closed: an unresolvable layout, an indirect (heap-boxed) enum, a
/// generic record whose fields resolve only after substitution, a
/// closure/borrow leaf, a nested opaque handle, or ANY affine /
/// lifecycle-registered leaf outside the declared-release carve-out answers
/// `false` and keeps its composite on the
/// pre-existing fail-closed leak posture — never a double-free or double-close.
/// A recursion budget bounds pathological mutually-nested layouts (a genuinely
/// recursive enum is `is_indirect` and short-circuits).
#[allow(
    clippy::too_many_arguments,
    reason = "the predicate consults every payload-classification authority the \
              drop prover has (layouts, field orders, value classes, the opaque \
              set, the lifecycle registry); bundling them into a struct would \
              only relocate the same fields"
)]
pub(in crate::lower) fn enum_payloads_are_shell_drop_safe(
    ty: &ResolvedTy,
    enum_layouts: &[crate::model::EnumLayout],
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    type_classes: &TypeClassTable,
    record_layouts: &[crate::model::RecordLayout],
    opaque_handle_names: &[String],
    lifecycle_registry: &hew_hir::LifecycleRegistry,
) -> bool {
    payload_leaf_is_shell_drop_safe(
        ty,
        enum_layouts,
        record_field_orders,
        type_classes,
        record_layouts,
        opaque_handle_names,
        lifecycle_registry,
        0,
    )
}
fn direct_runtime_payload_is_shell_drop_safe(
    ty: &ResolvedTy,
    type_classes: &TypeClassTable,
    depth: u32,
) -> bool {
    depth == 1
        && matches!(
            crate::lower::drop_plan::resource_drop_fn(ty, type_classes),
            Some(crate::model::DropFnSpec::Runtime(_))
        )
}

/// Whether every heap leaf reachable through `ty` is shell-drop-safe. See
/// [`enum_payloads_are_shell_drop_safe`] for the shape table and the
/// fail-closed rule. `depth` gates the bare-opaque admission to the candidate
/// enum's DIRECT variant payloads (`depth == 1`) and is a defensive backstop
/// beyond that; inline nesting is already bounded by finite type size, and a
/// genuinely recursive enum short-circuits on `is_indirect`.
#[allow(
    clippy::too_many_arguments,
    clippy::too_many_lines,
    reason = "same authority set as the public entry, plus the depth gate"
)]
fn payload_leaf_is_shell_drop_safe(
    ty: &ResolvedTy,
    enum_layouts: &[crate::model::EnumLayout],
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    type_classes: &TypeClassTable,
    record_layouts: &[crate::model::RecordLayout],
    opaque_handle_names: &[String],
    lifecycle_registry: &hew_hir::LifecycleRegistry,
    depth: u32,
) -> bool {
    if depth > 32 {
        return false;
    }
    // Declared-release carve-out, DIRECT variant payloads only. A
    // lifecycle-registered `#[resource]` RECORD whose every field passes the
    // clause-3 authority (`field_is_released_only_by_the_declared_close`) has
    // its declared `close` as its ENTIRE release plan, and — when it rides an
    // enum out of a producer — the candidate shell's `EnumInPlace` drop is the
    // ONE authority that ever schedules that close (`__hew_enum_drop_inplace`
    // → `__hew_record_drop_inplace_<R>` → `<R>::close`): the payload binder
    // owns no heap and earns no owner of its own, and an arm that CONSUMES the
    // payload (an explicit `h.close()`) hands it off through a
    // `NeutralizePayloadSlot`, which the prover's neutralize scan turns into a
    // whole-candidate exclusion (a record close is not null-safe over a
    // zeroed slot, so the shell must not drop after a hand-off). Refusing here
    // therefore leaves ZERO releases for the adopted handle, not one — the
    // declared-release contract's leak. The answer is the clause-3 conjunction
    // itself: a registered record with a field the post-close teardown CAN
    // free (`log: string`) answers false and stays on the affine refusal
    // below, so admission and refusal read the same authority the adoption
    // boundary uses (`DeclaredReleaseTypes`).
    //
    // Depth-1 only: the soundness argument covers the candidate shell's own
    // drop steps. A declared-release record reached through a nested
    // aggregate is that aggregate's helper family's problem and stays on the
    // fail-closed leak posture, same as the bare-opaque carve-out below.
    if depth == 1 {
        if let ResolvedTy::Named {
            name,
            args,
            is_opaque: false,
            ..
        } = ty
        {
            if args.is_empty()
                && lifecycle_registry
                    .resource_record(&hew_types::DefId::legacy_reconstruct_from_full_path(name))
                    .is_some()
            {
                let opaques: HashSet<&str> =
                    opaque_handle_names.iter().map(String::as_str).collect();
                return record_field_orders.get(name).is_some_and(|fields| {
                    fields.iter().all(|(_, field_ty)| {
                        crate::return_provenance::field_is_released_only_by_the_declared_close(
                            field_ty, &opaques,
                        )
                    })
                });
            }
        }
    }
    // Direct builtin runtime handles have an exact null-tolerant close descriptor.
    // Their carrier slot can therefore use the same neutralize-then-shell-drop
    // protocol as string and bytes. User close functions remain below the
    // affine refusal: calling arbitrary close code over zeroed storage is unsafe.
    if direct_runtime_payload_is_shell_drop_safe(ty, type_classes, depth) {
        return true;
    }
    // `Rc<T>` / `Weak<T>` carrier payloads release through a null-tolerant
    // refcount decrement (`rc_release` / `weak_release`), so — like a direct
    // runtime handle or a `string`/`bytes` leaf — the candidate shell's
    // tag-aware `EnumInPlace` drop can run the neutralize-then-shell-drop
    // protocol over a possibly-zeroed slot without ever double-releasing. They
    // classify `AffineResource` (below), so admit them HERE, before the affine
    // refusal, exactly as the runtime-handle arm does. Gated to the candidate's
    // DIRECT variant payloads (`depth == 1`); a deeper `Rc` rides its enclosing
    // aggregate's own helper family.
    if depth == 1
        && matches!(
            ty,
            ResolvedTy::Named {
                builtin: Some(hew_types::BuiltinType::Rc | hew_types::BuiltinType::Weak),
                ..
            }
        )
    {
        return true;
    }
    // Affine leaves (`#[resource]` records, closeable opaque resources, owned
    // runtime handles, `@linear` / `Task` values) carry their own consume-once
    // close discipline and have no clone helper. Refuse the WHOLE composite the
    // moment one appears — checked BEFORE the positive admits so a `#[resource]`
    // record cannot slip through the value-record arm below and earn a second
    // close.
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
    // Bare `#[opaque]` handle as a DIRECT variant payload of the candidate
    // enum — decided by the one classification authority the codegen thunk
    // synthesis also consults, so a lifecycle-registered (`#[resource]`)
    // handle classifies `Resource` and stays refused here (belt to the
    // value-class gate above, which refuses the `#[resource]`-marked spelling
    // independently). Deeper opaque leaves fall through to the `Named` arm's
    // `is_opaque` refusal: their release would route through a NESTED
    // aggregate's helper family, where the no-op field-drop argument does not
    // hold.
    if depth == 1 {
        if let ResolvedTy::Named {
            is_opaque: true, ..
        } = ty
        {
            let mut visited = HashSet::new();
            return matches!(
                crate::state_clone::classify_state_field_with_lifecycle_registry(
                    ty,
                    record_layouts,
                    enum_layouts,
                    opaque_handle_names,
                    lifecycle_registry,
                    &mut visited,
                ),
                Ok(crate::state_clone::StateFieldCloneKind::OpaqueHandle { .. })
            );
        }
    }
    let recurse = |leaf: &ResolvedTy| {
        payload_leaf_is_shell_drop_safe(
            leaf,
            enum_layouts,
            record_field_orders,
            type_classes,
            record_layouts,
            opaque_handle_names,
            lifecycle_registry,
            depth + 1,
        )
    };
    match ty {
        ResolvedTy::Tuple(elems) => elems.iter().all(recurse),
        ResolvedTy::Array(elem, _) => recurse(elem),
        // A `Vec<T>` clones/drops element-wise (safe iff its element is); a nested
        // inline enum recurses into every variant payload, a nested record into
        // every field. An indirect enum, a nested opaque handle, or a generic
        // record whose fields resolve only after substitution is left on the
        // fail-closed leak posture.
        ResolvedTy::Named { args, .. } if crate::lower::drop_plan::ty_is_vec(ty) => {
            args.first().is_some_and(&recurse)
        }
        ResolvedTy::Named {
            name,
            args,
            is_opaque,
            ..
        } => {
            // A nested opaque handle (inside a Vec / tuple / nested enum /
            // record) must not vacuously admit through an empty registered
            // field order — its release is a nested helper family's problem,
            // and that family has no dup symbol for it.
            if *is_opaque {
                return false;
            }
            if let Some(layout) = crate::model::find_enum_layout(name, args, enum_layouts) {
                return !layout.is_indirect
                    && layout
                        .variants
                        .iter()
                        .all(|variant| variant.field_tys.iter().all(&recurse));
            }
            if let Some(layout) = crate::model::find_record_layout_for_ty(ty, record_layouts) {
                return layout.field_tys.iter().all(&recurse);
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
