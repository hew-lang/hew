//! Direct structural tests for `derive_owned_record_drop_allowed` — the
//! value-class-capstone fail-closed sole-owner gate for owned-aggregate
//! records passed/returned by value. These poke the derivation with
//! synthetic MIR blocks: a returned record (escape) must be EXCLUDED so its
//! `RecordInPlace` drop never double-frees the escapee's fields, while a
//! field-read-only record must be ADMITTED so its heap fields are freed.
//! The paired runtime oracle in the exec suite is Guard-Malloc with
//! `MallocScribble`; glibc-only `MALLOC_CHECK_` / `MALLOC_PERTURB_` are
//! platform helpers, not the canonical proof on macOS.
//!
//! The headline `one_arm_consume_*` pair pins the audit-#5 reconciliation:
//! a record consumed (returned) on one branch but live on another is gated
//! by this per-exit escape analysis, NOT by a path-insensitive global
//! `owned_locals` removal.
use super::*;

/// An owned record named "Rec"; everything else is not a record candidate.
fn rec_ty() -> ResolvedTy {
    ResolvedTy::named_user("Rec", vec![])
}

fn vec_string_ty() -> ResolvedTy {
    ResolvedTy::named_builtin("Vec", BuiltinType::Vec, vec![ResolvedTy::String])
}

fn is_rec(ty: &ResolvedTy) -> bool {
    matches!(ty, ResolvedTy::Named { name, .. } if name == "Rec")
}

fn is_vec_handle(ty: &ResolvedTy) -> bool {
    matches!(
        ty,
        ResolvedTy::Named {
            builtin: Some(BuiltinType::Vec),
            ..
        }
    )
}

fn block(id: u32, instructions: Vec<Instr>, terminator: Terminator) -> BasicBlock {
    BasicBlock {
        id,
        statements: vec![],
        instructions,
        terminator,
    }
}

fn derive(
    blocks: &[BasicBlock],
    owned: &[(BindingId, String, ResolvedTy)],
    binding_locals: &HashMap<BindingId, Place>,
    local_tys: &[ResolvedTy],
) -> HashSet<BindingId> {
    derive_with_field_ty(blocks, owned, binding_locals, local_tys, ResolvedTy::String)
}

fn derive_with_field_ty(
    blocks: &[BasicBlock],
    owned: &[(BindingId, String, ResolvedTy)],
    binding_locals: &HashMap<BindingId, Place>,
    local_tys: &[ResolvedTy],
    field_ty: ResolvedTy,
) -> HashSet<BindingId> {
    // `Rec` carries a heap-owning `string` field so the unified
    // `ty_owns_heap` authority (record-aware) classifies a `Rec`-typed
    // field-load binder as heap-owning — the verdict the candidate
    // predicate `is_rec` selects on, kept in agreement now that the
    // record-blindness workaround is gone (DIV-1).
    let mut record_field_orders: HashMap<String, Vec<(String, ResolvedTy)>> = HashMap::new();
    record_field_orders.insert("Rec".to_string(), vec![("label".to_string(), field_ty)]);
    derive_owned_record_drop_allowed(
        blocks,
        &HashMap::new(),
        owned,
        binding_locals,
        local_tys,
        &is_rec,
        &|_, _| false,
        &record_field_orders,
        &[],
        &hew_hir::LifecycleRegistry::default(),
        &[],
        &HashMap::new(),
    )
}

fn derive_with_field_ty_and_enums(
    blocks: &[BasicBlock],
    owned: &[(BindingId, String, ResolvedTy)],
    binding_locals: &HashMap<BindingId, Place>,
    local_tys: &[ResolvedTy],
    field_ty: ResolvedTy,
    enum_layouts: &[crate::model::EnumLayout],
) -> HashSet<BindingId> {
    let mut record_field_orders: HashMap<String, Vec<(String, ResolvedTy)>> = HashMap::new();
    record_field_orders.insert("Rec".to_string(), vec![("label".to_string(), field_ty)]);
    derive_owned_record_drop_allowed(
        blocks,
        &HashMap::new(),
        owned,
        binding_locals,
        local_tys,
        &is_rec,
        &|_, _| false,
        &record_field_orders,
        enum_layouts,
        &hew_hir::LifecycleRegistry::default(),
        &[],
        &HashMap::new(),
    )
}

/// A record local that is never read (no construction-site escape, no field
/// read) is its own sole owner and must be admitted for `RecordInPlace`.
#[test]
fn untouched_record_local_is_admitted() {
    let b = BindingId(1);
    let owned = vec![(b, "r".to_string(), rec_ty())];
    let mut binding_locals: HashMap<BindingId, Place> = HashMap::new();
    binding_locals.insert(b, Place::Local(0));
    let local_tys = vec![rec_ty()];

    let allowed = derive(
        &[block(0, vec![], Terminator::Return)],
        &owned,
        &binding_locals,
        &local_tys,
    );
    assert!(
        allowed.contains(&b),
        "an untouched owned record is its own sole owner and must be admitted"
    );
}

/// Final admission must test the canonical byte-alias root, not the
/// candidate's immediate slot.  Branch joins commonly register both the
/// produced record and the named join result; after the latter escapes,
/// both slots describe the same transferred owner and neither may retain a
/// `RecordInPlace` cleanup.
#[test]
fn escaped_alias_member_excludes_every_candidate_in_its_group() {
    let produced = BindingId(1);
    let selected = BindingId(2);
    let owned = vec![
        (produced, "produced".to_string(), rec_ty()),
        (selected, "selected".to_string(), rec_ty()),
    ];
    let binding_locals: HashMap<BindingId, Place> =
        [(produced, Place::Local(0)), (selected, Place::Local(1))]
            .into_iter()
            .collect();
    let local_tys = vec![rec_ty(), rec_ty()];
    let instructions = vec![
        Instr::Move {
            dest: Place::Local(1),
            src: Place::Local(0),
        },
        Instr::Move {
            dest: Place::ReturnSlot,
            src: Place::Local(1),
        },
    ];

    let allowed = derive(
        &[block(0, instructions, Terminator::Return)],
        &owned,
        &binding_locals,
        &local_tys,
    );
    assert!(
        !allowed.contains(&produced) && !allowed.contains(&selected),
        "an escape through any whole-value alias transfers the group's one owner; \
         no stale candidate may retain recursive cleanup authority; got {allowed:?}"
    );
}

/// `derive` variant that seeds the per-block proven-borrow arg-index map —
/// the caller-side owned-param completion input.
fn derive_with_pbca(
    blocks: &[BasicBlock],
    owned: &[(BindingId, String, ResolvedTy)],
    binding_locals: &HashMap<BindingId, Place>,
    local_tys: &[ResolvedTy],
    proven_borrow_call_args: &HashMap<u32, HashSet<usize>>,
) -> HashSet<BindingId> {
    let mut record_field_orders: HashMap<String, Vec<(String, ResolvedTy)>> = HashMap::new();
    record_field_orders.insert(
        "Rec".to_string(),
        vec![("label".to_string(), ResolvedTy::String)],
    );
    derive_owned_record_drop_allowed(
        blocks,
        &HashMap::new(),
        owned,
        binding_locals,
        local_tys,
        &is_rec,
        &|_, _| false,
        &record_field_orders,
        &[],
        &hew_hir::LifecycleRegistry::default(),
        &[],
        proven_borrow_call_args,
    )
}

/// A single-block user call `foo(r)` reading the WHOLE record as its sole
/// argument. Returns `(binding, block)` so a test can drive the derivation
/// with and without a proven-borrow verdict for arg-index 0.
fn whole_record_arg_call_block() -> (BindingId, Vec<BasicBlock>) {
    let b = BindingId(1);
    let blk = vec![block(
        0,
        vec![],
        Terminator::Call {
            callee: "foo".to_string(),
            authority: crate::model::CallAuthority::default(),
            args: vec![Place::Local(0)],
            dest: None,
            next: 1,
        },
    )];
    (b, blk)
}

/// Caller-side owned-param completion: a whole record passed by value at a
/// call position the module summary PROVED borrow-only is a transient borrow
/// — the caller retains it and keeps its `RecordInPlace` scope-exit drop.
#[test]
fn proven_borrow_whole_record_arg_is_admitted() {
    let (b, blocks) = whole_record_arg_call_block();
    let owned = vec![(b, "r".to_string(), rec_ty())];
    let binding_locals: HashMap<BindingId, Place> = [(b, Place::Local(0))].into_iter().collect();
    let local_tys = vec![rec_ty()];
    // Block 0's arg-index 0 is proven borrow.
    let pbca: HashMap<u32, HashSet<usize>> = [(0u32, [0usize].into_iter().collect())]
        .into_iter()
        .collect();

    let allowed = derive_with_pbca(&blocks, &owned, &binding_locals, &local_tys, &pbca);
    assert!(
        allowed.contains(&b),
        "a proven-borrow whole-record call arg is a transient borrow; the \
         caller must keep its RecordInPlace drop; allowed: {allowed:?}"
    );
}

/// Fail-closed direction: the SAME call with NO proven-borrow verdict is an
/// ownership escape (the callee may store/return/send the record), so the
/// record root stays excluded — leak, never double-free.
#[test]
fn unproven_whole_record_arg_is_excluded() {
    let (b, blocks) = whole_record_arg_call_block();
    let owned = vec![(b, "r".to_string(), rec_ty())];
    let binding_locals: HashMap<BindingId, Place> = [(b, Place::Local(0))].into_iter().collect();
    let local_tys = vec![rec_ty()];

    let allowed = derive_with_pbca(
        &blocks,
        &owned,
        &binding_locals,
        &local_tys,
        &HashMap::new(),
    );
    assert!(
        !allowed.contains(&b),
        "an unproven whole-record call arg may escape into the callee; it \
         must stay excluded (fail-closed leak); allowed: {allowed:?}"
    );
}

/// A mixed-args call `foo(a, b)` where ONLY arg-index 0 is proven borrow:
/// `a` is admitted, `b`'s root stays excluded. The exemption is per-arg, not
/// per-call.
#[test]
fn mixed_args_call_admits_only_proven_arg() {
    let a = BindingId(1);
    let bb = BindingId(2);
    let owned = vec![
        (a, "a".to_string(), rec_ty()),
        (bb, "b".to_string(), rec_ty()),
    ];
    let binding_locals: HashMap<BindingId, Place> = [(a, Place::Local(0)), (bb, Place::Local(1))]
        .into_iter()
        .collect();
    let local_tys = vec![rec_ty(), rec_ty()];
    let blocks = vec![block(
        0,
        vec![],
        Terminator::Call {
            callee: "foo".to_string(),
            authority: crate::model::CallAuthority::default(),
            args: vec![Place::Local(0), Place::Local(1)],
            dest: None,
            next: 1,
        },
    )];
    // Only arg-index 0 is proven borrow.
    let pbca: HashMap<u32, HashSet<usize>> = [(0u32, [0usize].into_iter().collect())]
        .into_iter()
        .collect();

    let allowed = derive_with_pbca(&blocks, &owned, &binding_locals, &local_tys, &pbca);
    assert!(
        allowed.contains(&a),
        "the proven-borrow arg must be admitted; allowed: {allowed:?}"
    );
    assert!(
        !allowed.contains(&bb),
        "the un-proven arg may escape and must stay excluded; allowed: {allowed:?}"
    );
}

/// A record read via `RecordFieldLoad` of a `BitCopy` field stays the sole
/// owner — the field read is an interior read, not an escape — and must be
/// admitted (so its other owned fields are freed at scope exit).
#[test]
fn record_bitcopy_field_read_is_admitted() {
    let b = BindingId(1);
    let owned = vec![(b, "r".to_string(), rec_ty())];
    let mut binding_locals: HashMap<BindingId, Place> = HashMap::new();
    binding_locals.insert(b, Place::Local(0));
    // local 1 receives the loaded BitCopy field (i64).
    let local_tys = vec![rec_ty(), ResolvedTy::I64];
    let instrs = vec![Instr::RecordFieldLoad {
        record: Place::Local(0),
        field_offset: FieldOffset(1),
        dest: Place::Local(1),
    }];

    let allowed = derive(
        &[block(0, instrs, Terminator::Return)],
        &owned,
        &binding_locals,
        &local_tys,
    );
    assert!(
        allowed.contains(&b),
        "a record whose only use is reading a BitCopy field stays the sole \
         owner and must be admitted; got {allowed:?}"
    );
}

/// A record moved whole-value into the `ReturnSlot` (returned) has escaped:
/// the caller owns its fields now, so it must be EXCLUDED — dropping it
/// would double-free the returned value's heap fields.
#[test]
fn returned_record_is_excluded() {
    let b = BindingId(1);
    let owned = vec![(b, "r".to_string(), rec_ty())];
    let mut binding_locals: HashMap<BindingId, Place> = HashMap::new();
    binding_locals.insert(b, Place::Local(0));
    let local_tys = vec![rec_ty()];
    let instrs = vec![Instr::Move {
        dest: Place::ReturnSlot,
        src: Place::Local(0),
    }];

    let allowed = derive(
        &[block(0, instrs, Terminator::Return)],
        &owned,
        &binding_locals,
        &local_tys,
    );
    assert!(
        !allowed.contains(&b),
        "a returned record escaped to the caller and must be excluded from \
         the scope-exit drop (else double-free); got {allowed:?}"
    );
}

/// A string field load is retained by codegen. Moving that independent
/// share into the `ReturnSlot` must leave the record admitted to release its
/// original share.
#[test]
fn escaped_retained_string_field_keeps_record_owner() {
    let b = BindingId(1);
    let owned = vec![(b, "r".to_string(), rec_ty())];
    let mut binding_locals: HashMap<BindingId, Place> = HashMap::new();
    binding_locals.insert(b, Place::Local(0));
    // local 1 receives the loaded owned (string) field.
    let local_tys = vec![rec_ty(), ResolvedTy::String];
    let instrs = vec![
        Instr::RecordFieldLoad {
            record: Place::Local(0),
            field_offset: FieldOffset(0),
            dest: Place::Local(1),
        },
        Instr::Move {
            dest: Place::ReturnSlot,
            src: Place::Local(1),
        },
    ];

    let allowed = derive(
        &[block(0, instrs, Terminator::Return)],
        &owned,
        &binding_locals,
        &local_tys,
    );
    assert!(
        allowed.contains(&b),
        "the returned string field owns a retained share, so the record \
         must keep its original field release; got {allowed:?}"
    );
}

/// Soundness pin for the tuple-field lift: a `RecordFieldLoad` that carries
/// an `(Option<string>, i64)` field into a fresh `RecordInit` transfers the
/// tuple-owned drop obligation to the result, so the consumed base record
/// must be excluded from `RecordInPlace`.
#[test]
fn tuple_field_carry_with_option_payload_excludes_record_root() {
    let b = BindingId(1);
    let tuple_field_ty = ResolvedTy::Tuple(vec![
        ResolvedTy::named_builtin("Option", BuiltinType::Option, vec![ResolvedTy::String]),
        ResolvedTy::I64,
    ]);
    let owned = vec![(b, "r".to_string(), rec_ty())];
    let binding_locals: HashMap<BindingId, Place> = [(b, Place::Local(0))].into_iter().collect();
    let local_tys = vec![rec_ty(), tuple_field_ty.clone(), rec_ty()];
    let instrs = vec![
        Instr::RecordFieldLoad {
            record: Place::Local(0),
            field_offset: FieldOffset(0),
            dest: Place::Local(1),
        },
        Instr::RecordInit {
            ty: rec_ty(),
            fields: vec![(FieldOffset(0), Place::Local(1))],
            dest: Place::Local(2),
        },
    ];

    let allowed = derive(
        &[block(0, instrs, Terminator::Return)],
        &owned,
        &binding_locals,
        &local_tys,
    );
    assert!(
        !allowed.contains(&b),
        "the tuple payload escapes into RecordInit, so the base record's \
         in-place drop must be excluded; got {allowed:?}"
    );
}

/// Companion pin for the user-enum payload family: a tuple field carrying a
/// heap-owned user enum into `RecordInit` is also transferred by the same
/// owned-record escape rule, so the base record must be excluded.
#[test]
fn tuple_field_carry_with_user_enum_payload_excludes_record_root() {
    let b = BindingId(1);
    let tuple_field_ty = ResolvedTy::Tuple(vec![
        ResolvedTy::named_user("Wrap", vec![]),
        ResolvedTy::I64,
    ]);
    let enum_layouts = vec![crate::model::EnumLayout {
        name: "Wrap".to_string(),
        tag_width: 1,
        variants: vec![
            crate::model::MachineVariantLayout {
                name: "Some".to_string(),
                field_tys: vec![ResolvedTy::String],
                field_names: vec![],
            },
            crate::model::MachineVariantLayout {
                name: "None".to_string(),
                field_tys: vec![],
                field_names: vec![],
            },
        ],
        is_indirect: false,
    }];
    let owned = vec![(b, "r".to_string(), rec_ty())];
    let binding_locals: HashMap<BindingId, Place> = [(b, Place::Local(0))].into_iter().collect();
    let local_tys = vec![rec_ty(), tuple_field_ty.clone(), rec_ty()];
    let instrs = vec![
        Instr::RecordFieldLoad {
            record: Place::Local(0),
            field_offset: FieldOffset(0),
            dest: Place::Local(1),
        },
        Instr::RecordInit {
            ty: rec_ty(),
            fields: vec![(FieldOffset(0), Place::Local(1))],
            dest: Place::Local(2),
        },
    ];

    let allowed = derive_with_field_ty_and_enums(
        &[block(0, instrs, Terminator::Return)],
        &owned,
        &binding_locals,
        &local_tys,
        tuple_field_ty,
        &enum_layouts,
    );
    assert!(
        !allowed.contains(&b),
        "the tuple payload escapes into RecordInit, so the base record's \
         in-place drop must be excluded; got {allowed:?}"
    );
}

/// A `FieldDropInPlace` addressing the candidate root is BOTH the field
/// extraction and its release, yet it mints no load dest and no `Drop`
/// place — with bitcopy-only sibling binders it seeds neither
/// `field_binders` nor `release_owner_bases`, so only the direct
/// prover-exclusion rule suppresses the composite. Without it the
/// `RecordInPlace` drop would re-walk the freed field's leaves
/// (double-free; inline composites carry no null-store).
#[test]
fn field_drop_in_place_on_root_excludes_record() {
    let b = BindingId(1);
    let owned = vec![(b, "r".to_string(), rec_ty())];
    let mut binding_locals: HashMap<BindingId, Place> = HashMap::new();
    binding_locals.insert(b, Place::Local(0));
    let local_tys = vec![rec_ty()];
    let instrs = vec![Instr::FieldDropInPlace {
        base: Place::Local(0),
        field: crate::model::FieldAddr::Record(FieldOffset(0)),
        ty: ResolvedTy::String,
    }];

    let allowed = derive(
        &[block(0, instrs, Terminator::Return)],
        &owned,
        &binding_locals,
        &local_tys,
    );
    assert!(
        !allowed.contains(&b),
        "a record root addressed by FieldDropInPlace already discharged \
         that field's release; admitting its composite drop would re-walk \
         the freed field (double-free); got {allowed:?}"
    );
}

/// A `FieldDropInPlace` whose base is a field BINDER — the extracted
/// member alias `let inner = outer.field; match inner { Inner { a, b: _ }
/// => … }` — discharges a field of the OUTER root's storage through the
/// binder's byte-copy (the null-store lands in the binder's slot, never
/// the root's). The direct rule must resolve the binder through its
/// provenance and exclude exactly the root it was loaded from, while a
/// sibling root the binder never touched stays admitted. Red-before: the
/// rule consulted `alias_of` only, the loaded-from root stayed admitted,
/// and its `RecordInPlace` re-walked the freed field — the reproduced
/// Guard-Malloc double-free.
#[test]
fn field_drop_in_place_on_field_binder_excludes_loaded_root_only() {
    let outer = BindingId(1);
    let other = BindingId(2);
    let owned = vec![
        (outer, "outer".to_string(), rec_ty()),
        (other, "other".to_string(), rec_ty()),
    ];
    let mut binding_locals: HashMap<BindingId, Place> = HashMap::new();
    binding_locals.insert(outer, Place::Local(0));
    binding_locals.insert(other, Place::Local(1));
    // local 2: the extracted heap-owning member binder (`let inner =
    // outer.field`); local 3: the match scrutinee copy of the binder.
    let local_tys = vec![rec_ty(), rec_ty(), rec_ty(), rec_ty()];
    let instrs = vec![
        Instr::RecordFieldLoad {
            record: Place::Local(0),
            field_offset: FieldOffset(0),
            dest: Place::Local(2),
        },
        Instr::Move {
            dest: Place::Local(3),
            src: Place::Local(2),
        },
        Instr::FieldDropInPlace {
            base: Place::Local(3),
            field: crate::model::FieldAddr::Record(FieldOffset(1)),
            ty: ResolvedTy::String,
        },
    ];

    let allowed = derive(
        &[block(0, instrs, Terminator::Return)],
        &owned,
        &binding_locals,
        &local_tys,
    );
    assert!(
        !allowed.contains(&outer),
        "a FieldDropInPlace against a field binder freed part of the \
         loaded-from root's storage through the binder's byte-alias; the \
         root must be excluded or its composite walk re-frees it \
         (double-free); got {allowed:?}"
    );
    assert!(
        allowed.contains(&other),
        "the binder's provenance names the loaded-from root uniquely; a \
         sibling root it never touched keeps its composite drop \
         (precision pin); got {allowed:?}"
    );
}

/// `hew_vec_push_owned` / `hew_vec_set_owned` DEEP-CLONE their element
/// operand into the destination Vec, so the source keeps sole ownership of
/// its own heap. Both the collection-local prover AND the composite binder
/// scans therefore retain their candidate: a field binder pushed copy-in
/// (`xs.push(r.field)`) leaves `r` owning the ORIGINAL field buffer, so the
/// record retains its `RecordInPlace` composite drop (the container's
/// element drop-thunk owns the disjoint clone). Excluding the composite
/// here leaked the original field buffer (#2721).
#[test]
fn vec_copy_in_tail_split_retains_local_candidate_and_composite_binder() {
    let callee = "hew_vec_push_owned";
    let call_builtin = hew_types::runtime_call::RuntimeCallFamily::from_c_symbol(callee);

    let record = BindingId(10);
    let record_owned = vec![(record, "r".to_string(), rec_ty())];
    let record_binding_locals: HashMap<BindingId, Place> =
        [(record, Place::Local(0))].into_iter().collect();
    let record_local_tys = vec![rec_ty(), vec_string_ty(), vec_string_ty()];
    let record_blocks = vec![block(
        0,
        vec![Instr::RecordFieldLoad {
            record: Place::Local(0),
            field_offset: FieldOffset(0),
            dest: Place::Local(1),
        }],
        Terminator::Call {
            callee: callee.to_string(),
            authority: (call_builtin)
                .map(crate::CallAuthority::Runtime)
                .unwrap_or_default(),
            args: vec![Place::Local(2), Place::Local(1)],
            dest: None,
            next: 1,
        },
    )];

    let record_allowed = derive(
        &record_blocks,
        &record_owned,
        &record_binding_locals,
        &record_local_tys,
    );
    assert!(
        record_allowed.contains(&record),
        "a composite field binder used as the copy-in element operand is \
         DEEP-CLONED, not consumed, so the record keeps sole ownership of \
         the original field and must retain its composite drop; excluding \
         it leaks the original buffer (#2721); allowed: {record_allowed:?}"
    );

    let elem = BindingId(11);
    let collection_owned = vec![(elem, "elem".to_string(), vec_string_ty())];
    let collection_binding_locals: HashMap<BindingId, Place> =
        [(elem, Place::Local(1))].into_iter().collect();
    let collection_blocks = vec![BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Call {
            callee: callee.to_string(),
            authority: (call_builtin)
                .map(crate::CallAuthority::Runtime)
                .unwrap_or_default(),
            args: vec![Place::Local(2), Place::Local(1)],
            dest: None,
            next: 1,
        },
    }];

    let collection_allowed = derive_local_collection_drop_allowed(
        &collection_blocks,
        &HashMap::new(),
        &collection_owned,
        &collection_binding_locals,
        &HashMap::new(),
        is_vec_handle,
    );
    assert!(
        collection_allowed.contains(&elem),
        "a local collection candidate used as the copy-in element operand is \
         borrowed for clone and must keep its drop admission; allowed: \
         {collection_allowed:?}"
    );
}

/// One-arm-consume (audit #5): a record consumed (returned) on one branch
/// and field-read on the other. The whole-record escape on the consume arm
/// excludes the binding fail-closed (over-exclusion leaks on the live arm,
/// never double-frees on the consume arm). This pins that the gate is the
/// per-exit escape analysis — not the path-insensitive global removal that
/// would silently produce the same exclusion but for the wrong reason and
/// could not be tightened to per-arm precision later.
#[test]
fn one_arm_consume_record_is_excluded_fail_closed() {
    let b = BindingId(1);
    let owned = vec![(b, "r".to_string(), rec_ty())];
    let mut binding_locals: HashMap<BindingId, Place> = HashMap::new();
    binding_locals.insert(b, Place::Local(0));
    let local_tys = vec![rec_ty(), ResolvedTy::I64];
    // bb0: branch to bb1 (consume) / bb2 (live).
    let bb0 = block(
        0,
        vec![],
        Terminator::Branch {
            cond: Place::Local(1),
            then_target: 1,
            else_target: 2,
        },
    );
    // bb1: return the record (consume / escape).
    let bb1 = block(
        1,
        vec![Instr::Move {
            dest: Place::ReturnSlot,
            src: Place::Local(0),
        }],
        Terminator::Return,
    );
    // bb2: only read a BitCopy field (live; would be dropped here).
    let bb2 = block(
        2,
        vec![Instr::RecordFieldLoad {
            record: Place::Local(0),
            field_offset: FieldOffset(1),
            dest: Place::Local(1),
        }],
        Terminator::Return,
    );

    let allowed = derive(&[bb0, bb1, bb2], &owned, &binding_locals, &local_tys);
    assert!(
        !allowed.contains(&b),
        "a record consumed on one arm must be excluded fail-closed (the \
         escape on the consume arm wins); got {allowed:?}"
    );
}

/// Companion to the one-arm-consume case: a record that is field-read on
/// BOTH arms (never escapes) must be ADMITTED — the escape analysis does not
/// over-exclude a record merely because it is branched on.
#[test]
fn record_live_on_both_arms_is_admitted() {
    let b = BindingId(1);
    let owned = vec![(b, "r".to_string(), rec_ty())];
    let mut binding_locals: HashMap<BindingId, Place> = HashMap::new();
    binding_locals.insert(b, Place::Local(0));
    let local_tys = vec![rec_ty(), ResolvedTy::I64];
    let bb0 = block(
        0,
        vec![],
        Terminator::Branch {
            cond: Place::Local(1),
            then_target: 1,
            else_target: 2,
        },
    );
    let field_read = |dest| Instr::RecordFieldLoad {
        record: Place::Local(0),
        field_offset: FieldOffset(1),
        dest,
    };
    let bb1 = block(1, vec![field_read(Place::Local(1))], Terminator::Return);
    let bb2 = block(2, vec![field_read(Place::Local(1))], Terminator::Return);

    let allowed = derive(&[bb0, bb1, bb2], &owned, &binding_locals, &local_tys);
    assert!(
        allowed.contains(&b),
        "a record field-read on both arms never escapes and must be admitted \
         so its heap fields are freed; got {allowed:?}"
    );
}

/// #2212 attribution: a field-binder escape provably traced to ONE root
/// excludes exactly that root — an unrelated record candidate in the same
/// function keeps its composite drop (pre-attribution the blanket
/// exclusion leaked every record's fields on any field escape).
#[test]
fn attributed_field_escape_keeps_unrelated_root_admitted() {
    let escaping = BindingId(1);
    let unrelated = BindingId(2);
    let owned = vec![
        (escaping, "a".to_string(), rec_ty()),
        (unrelated, "b".to_string(), rec_ty()),
    ];
    let binding_locals: HashMap<BindingId, Place> =
        [(escaping, Place::Local(0)), (unrelated, Place::Local(1))]
            .into_iter()
            .collect();
    // Use a non-retained heap field: string field loads are independent
    // `+1` shares and therefore are not ownership extractions.
    let field_ty = vec_string_ty();
    let local_tys = vec![rec_ty(), rec_ty(), field_ty.clone()];
    let instrs = vec![
        Instr::RecordFieldLoad {
            record: Place::Local(0),
            field_offset: FieldOffset(0),
            dest: Place::Local(2),
        },
        Instr::Move {
            dest: Place::ReturnSlot,
            src: Place::Local(2),
        },
    ];

    let allowed = derive_with_field_ty(
        &[block(0, instrs, Terminator::Return)],
        &owned,
        &binding_locals,
        &local_tys,
        field_ty,
    );
    assert!(
        !allowed.contains(&escaping),
        "the escaped field's root must stay excluded; got {allowed:?}"
    );
    assert!(
        allowed.contains(&unrelated),
        "a record no binder of which escaped must keep its composite drop \
         under per-root attribution; got {allowed:?}"
    );
}

/// #2212 fail-closed boundary: a binder loaded from TWO different roots
/// has ambiguous provenance — its escape must exclude EVERY record root
/// (the pre-attribution blanket), never guess one.
#[test]
fn ambiguous_binder_escape_excludes_every_root() {
    let first = BindingId(1);
    let second = BindingId(2);
    let owned = vec![
        (first, "a".to_string(), rec_ty()),
        (second, "b".to_string(), rec_ty()),
    ];
    let binding_locals: HashMap<BindingId, Place> =
        [(first, Place::Local(0)), (second, Place::Local(1))]
            .into_iter()
            .collect();
    let field_ty = vec_string_ty();
    let local_tys = vec![rec_ty(), rec_ty(), field_ty.clone()];
    let instrs = vec![
        Instr::RecordFieldLoad {
            record: Place::Local(0),
            field_offset: FieldOffset(0),
            dest: Place::Local(2),
        },
        Instr::RecordFieldLoad {
            record: Place::Local(1),
            field_offset: FieldOffset(0),
            dest: Place::Local(2),
        },
        Instr::Move {
            dest: Place::ReturnSlot,
            src: Place::Local(2),
        },
    ];

    let allowed = derive_with_field_ty(
        &[block(0, instrs, Terminator::Return)],
        &owned,
        &binding_locals,
        &local_tys,
        field_ty,
    );
    assert!(
        !allowed.contains(&first) && !allowed.contains(&second),
        "a binder loaded from two roots is unattributable; its escape must \
         exclude both roots fail-closed; got {allowed:?}"
    );
}

/// #2212 attribution through a reused binder slot: an instruction write
/// into the binder that is not a member load or binder move (a rebind
/// from an unrelated local) forces `Ambiguous`, so the escape falls back
/// to the blanket every-root exclusion.
#[test]
fn reused_binder_escape_falls_back_to_blanket_exclusion() {
    let root = BindingId(1);
    let bystander = BindingId(2);
    let owned = vec![
        (root, "a".to_string(), rec_ty()),
        (bystander, "b".to_string(), rec_ty()),
    ];
    let binding_locals: HashMap<BindingId, Place> =
        [(root, Place::Local(0)), (bystander, Place::Local(1))]
            .into_iter()
            .collect();
    // local 2: the non-retained binder; local 3: an unrelated value
    // overwriting it.
    let field_ty = vec_string_ty();
    let local_tys = vec![rec_ty(), rec_ty(), field_ty.clone(), field_ty.clone()];
    let instrs = vec![
        Instr::RecordFieldLoad {
            record: Place::Local(0),
            field_offset: FieldOffset(0),
            dest: Place::Local(2),
        },
        Instr::Move {
            dest: Place::Local(2),
            src: Place::Local(3),
        },
        Instr::Move {
            dest: Place::ReturnSlot,
            src: Place::Local(2),
        },
    ];

    let allowed = derive_with_field_ty(
        &[block(0, instrs, Terminator::Return)],
        &owned,
        &binding_locals,
        &local_tys,
        field_ty,
    );
    assert!(
        !allowed.contains(&root) && !allowed.contains(&bystander),
        "a binder overwritten by a non-member source is unattributable; \
         its escape must exclude every root fail-closed; got {allowed:?}"
    );
}
