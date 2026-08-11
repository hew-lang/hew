//! `payload_binder_candidate_root` regression: a raw-MIR shape that feeds
//! ONE heap payload binder local from TWO DIFFERENT composite roots must
//! not attribute the binder's later escape to only the first root that
//! reached it. First-root-wins silently kept whichever composite seeded the
//! binder first (`.entry(dl).or_insert_with(..)`), so an escape through that
//! binder excluded only that root and left the second composite still
//! admitted for its `EnumInPlace` drop — a leak of the second composite's
//! own payload had the second root been the one that actually still owned
//! live storage aliasing the binder. `attribute_payload_binder_root` instead
//! marks a conflicting second root `Conflict`, which `note_payload_escape`
//! treats as an unknown root and falls back to excluding EVERY composite
//! root on escape — the coarse, fail-closed pre-wave posture.
use super::*;

fn text_box_ty(name: &str) -> ResolvedTy {
    ResolvedTy::named_user(name, vec![])
}

fn text_box_layout(name: &str) -> crate::model::EnumLayout {
    crate::model::EnumLayout {
        name: name.to_string(),
        tag_width: 1,
        variants: vec![
            crate::model::MachineVariantLayout {
                name: "Text".to_string(),
                field_tys: vec![ResolvedTy::String],
                field_names: vec![],
            },
            crate::model::MachineVariantLayout {
                name: "Empty".to_string(),
                field_tys: vec![],
                field_names: vec![],
            },
        ],
        is_indirect: false,
    }
}

/// Two composite roots (`Local(0)`, `Local(3)`) both destructure their
/// `Text` payload into the SAME binder local (`Local(1)`), then the binder
/// escapes into ordinary storage (`Local(2)`, not itself a tracked binder) —
/// an owning-sink read that must exclude every root the binder could have
/// been carrying.
#[test]
fn conflicting_payload_binder_roots_exclude_both_composites_on_escape() {
    let root_a = BindingId(30);
    let root_b = BindingId(31);
    let binder = BindingId(32);
    let sink = BindingId(33);
    let first_box_ty = text_box_ty("TextBoxA");
    let second_box_ty = text_box_ty("TextBoxB");
    let binding_locals = HashMap::from([
        (root_a, Place::Local(0)),
        (root_b, Place::Local(3)),
        (binder, Place::Local(1)),
        (sink, Place::Local(2)),
    ]);
    // The shared binder and the escape destination sit in unrelated scopes
    // (no parent/child relationship registered in the empty `scope_info`
    // below), so the same-or-nested onward-handoff discipline correctly
    // refuses to fold the destination move into the payload-binder chain —
    // it is a genuine escape into unrelated storage, not a benign hand-off.
    let binding_scope = HashMap::from([
        (root_a, ScopeId(1)),
        (root_b, ScopeId(1)),
        (binder, ScopeId(2)),
        (sink, ScopeId(3)),
    ]);
    let layouts = vec![text_box_layout("TextBoxA"), text_box_layout("TextBoxB")];
    let local_tys = vec![
        first_box_ty.clone(),
        ResolvedTy::String,
        ResolvedTy::String,
        second_box_ty.clone(),
    ];
    let instructions = vec![
        // Root A destructures its Text payload into the shared binder.
        Instr::Move {
            dest: Place::Local(1),
            src: Place::EnumVariant {
                local: 0,
                variant_idx: 0,
                field_idx: 0,
            },
        },
        // Root B destructures its Text payload into the SAME binder local —
        // the conflicting second root.
        Instr::Move {
            dest: Place::Local(1),
            src: Place::EnumVariant {
                local: 3,
                variant_idx: 0,
                field_idx: 0,
            },
        },
        // The shared binder escapes into general storage — an owning read
        // that must exclude every candidate root it could be aliasing.
        Instr::Move {
            dest: Place::Local(2),
            src: Place::Local(1),
        },
    ];
    let allowed = derive_enum_composite_drop_allowed(
        &[BasicBlock {
            id: 0,
            statements: vec![],
            instructions,
            terminator: Terminator::Return,
        }],
        &HashMap::new(),
        &[
            (root_a, "box_a".to_string(), first_box_ty),
            (root_b, "box_b".to_string(), second_box_ty),
        ],
        &binding_locals,
        &binding_scope,
        &HashMap::new(),
        &HashMap::new(),
        &local_tys,
        &HashMap::new(),
        &layouts,
        &hew_hir::TypeClassTable::default(),
        &[],
        &[],
        &hew_hir::LifecycleRegistry::default(),
        &HashMap::new(),
        &HashSet::new(),
        &HashSet::new(),
        &crate::return_provenance::ExternContractTable::default(),
    );
    assert!(
        !allowed.contains(&root_a),
        "root A must be excluded: the shared binder's escape cannot be \
         attributed to root A alone once root B also fed it"
    );
    assert!(
        !allowed.contains(&root_b),
        "root B must be excluded: first-root-wins would silently drop this \
         composite's own conflict and leave it wrongly admitted"
    );
}
