//! Frozen-verdict pins for the type-shape axis of MIR drop admission: the
//! owned-locals seed gate, the collection-handle release bucket, and the
//! two release-symbol pickers. Each pin enumerates its own function's
//! decision domain and freezes today's verdict as a literal, so a moved
//! admission decision is a named test failure — never a silent
//! reclassification. An admission that widens over-drops (double-free,
//! the worst outcome); one that narrows leaks.
use super::*;
use crate::ownership::{DropClass, HeapLeaf};

fn vec_of(elem: ResolvedTy) -> ResolvedTy {
    ResolvedTy::named_builtin("Vec", BuiltinType::Vec, vec![elem])
}

fn named(name: &str) -> ResolvedTy {
    ResolvedTy::named_user(name, vec![])
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

fn generator_i64() -> ResolvedTy {
    ResolvedTy::named_builtin(
        "Generator",
        BuiltinType::Generator,
        vec![ResolvedTy::I64, ResolvedTy::Unit],
    )
}

fn bare_fn() -> ResolvedTy {
    ResolvedTy::Function {
        params: vec![],
        ret: Box::new(ResolvedTy::Unit),
    }
}

fn empty_capture_closure() -> ResolvedTy {
    ResolvedTy::Closure {
        params: vec![],
        ret: Box::new(ResolvedTy::Unit),
        captures: vec![],
    }
}

/// `indirect enum Foo { A(i64); B }` — a heap-boxed node whose per-element
/// `Vec` release is unwired (`Unsupported(NoReleaseProtocol)`).
fn builder_with_indirect_enum_foo() -> Builder {
    Builder {
        enum_layouts: vec![crate::model::EnumLayout {
            name: "Foo".to_string(),
            tag_width: 1,
            variants: vec![
                crate::model::MachineVariantLayout {
                    name: "A".to_string(),
                    field_tys: vec![ResolvedTy::I64],
                    field_names: vec![],
                },
                crate::model::MachineVariantLayout {
                    name: "B".to_string(),
                    field_tys: vec![],
                    field_names: vec![],
                },
            ],
            is_indirect: true,
        }],
        ..Builder::default()
    }
}

/// Builder carrying the field-drop classifier corpus's registered
/// layouts: user records over every slot class, an inline enum, and the
/// indirect enums `Foo` (from `builder_with_indirect_enum_foo`) and the
/// self-recursive `ListNode`.
fn builder_for_field_drop_classifier() -> Builder {
    let mut builder = builder_with_indirect_enum_foo();
    builder.enum_layouts.push(crate::model::EnumLayout {
        name: "Msg".to_string(),
        tag_width: 1,
        variants: vec![
            crate::model::MachineVariantLayout {
                name: "Text".to_string(),
                field_tys: vec![ResolvedTy::String],
                field_names: vec![],
            },
            crate::model::MachineVariantLayout {
                name: "Ping".to_string(),
                field_tys: vec![],
                field_names: vec![],
            },
        ],
        is_indirect: false,
    });
    builder.enum_layouts.push(crate::model::EnumLayout {
        name: "ListNode".to_string(),
        tag_width: 1,
        variants: vec![
            crate::model::MachineVariantLayout {
                name: "Cons".to_string(),
                field_tys: vec![ResolvedTy::I64, named("ListNode")],
                field_names: vec![],
            },
            crate::model::MachineVariantLayout {
                name: "Nil".to_string(),
                field_tys: vec![],
                field_names: vec![],
            },
        ],
        is_indirect: true,
    });
    for (record, fields) in [
        (
            "Row",
            vec![
                ("name".to_string(), ResolvedTy::String),
                ("n".to_string(), ResolvedTy::I64),
            ],
        ),
        (
            "Outer",
            vec![
                ("row".to_string(), named("Row")),
                ("k".to_string(), ResolvedTy::I64),
            ],
        ),
        ("HoldsFoo", vec![("f".to_string(), named("Foo"))]),
        (
            "HoldsBadVec",
            vec![("xs".to_string(), vec_of(named("Foo")))],
        ),
        (
            "HoldsSlice",
            vec![(
                "s".to_string(),
                ResolvedTy::Slice(Box::new(ResolvedTy::I64)),
            )],
        ),
        ("HoldsClosure", vec![("f".to_string(), bare_fn())]),
        (
            "HoldsToken",
            vec![("t".to_string(), ResolvedTy::CancellationToken)],
        ),
    ] {
        builder
            .record_field_orders
            .insert(record.to_string(), fields);
    }
    builder
}

/// The `FieldDropInPlace` admissibility classifier — the ONE predicate
/// MIR admission and the drop-plan verifier consult — with the verdict
/// frozen per shape. Admission mirrors codegen's `emit_heap_slot_drop`
/// dispatch: the five aggregate shapes over registered layouts admit
/// when every reachable slot is dischargeable; leaf COW types stay on
/// their own authority (refused at top level); everything the dispatcher
/// fail-closes on (slices, dyn traits, closure pairs, affine handles,
/// unwired `Vec` elements, unregistered layouts, free type params) is
/// refused. A widened verdict here is a wrong-ABI free at codegen; a
/// narrowed one is a lost capability — both are named test failures.
#[test]
fn field_drop_classifier_verdicts_are_frozen_per_shape() {
    let builder = builder_for_field_drop_classifier();

    let corpus: Vec<(&str, ResolvedTy, bool)> = vec![
        // Admitted aggregate shapes.
        ("record of string+i64", named("Row"), true),
        ("record nesting an admissible record", named("Outer"), true),
        ("record with indirect-enum field", named("HoldsFoo"), true),
        (
            "tuple of (string, i64)",
            ResolvedTy::Tuple(vec![ResolvedTy::String, ResolvedTy::I64]),
            true,
        ),
        (
            "fixed array of string",
            ResolvedTy::Array(Box::new(ResolvedTy::String), 3),
            true,
        ),
        ("inline enum with string payload", named("Msg"), true),
        ("indirect enum", named("Foo"), true),
        (
            "self-recursive indirect enum (cycle guard)",
            named("ListNode"),
            true,
        ),
        (
            "tuple with a wired Vec element",
            ResolvedTy::Tuple(vec![vec_of(ResolvedTy::I64)]),
            true,
        ),
        // Refused: a reachable slot the dispatcher cannot discharge.
        (
            "record with unwired Vec<indirect enum> field",
            named("HoldsBadVec"),
            false,
        ),
        ("record with slice field", named("HoldsSlice"), false),
        ("record with closure field", named("HoldsClosure"), false),
        (
            "record with affine-handle field",
            named("HoldsToken"),
            false,
        ),
        (
            "tuple with dyn-trait element",
            ResolvedTy::Tuple(vec![ResolvedTy::TraitObject {
                traits: vec![hew_types::ResolvedTraitBound {
                    trait_name: "Display".to_string(),
                    args: vec![],
                    assoc_bindings: vec![],
                }],
            }]),
            false,
        ),
        (
            "tuple with free type-param element",
            ResolvedTy::Tuple(vec![ResolvedTy::TypeParam {
                name: "T".to_string(),
            }]),
            false,
        ),
        // Refused: leaf / non-aggregate top levels (the admission OR
        // keeps leaves on `project_field_inline_drop_symbol`; `string`'s
        // reroute onto the op is its own decision, not a classifier
        // verdict).
        ("string top level", ResolvedTy::String, false),
        ("bytes top level", ResolvedTy::Bytes, false),
        ("Vec top level", vec_of(ResolvedTy::I64), false),
        ("i64 top level", ResolvedTy::I64, false),
        (
            "slice top level",
            ResolvedTy::Slice(Box::new(ResolvedTy::I64)),
            false,
        ),
        ("unregistered named type", named("Ghost"), false),
        (
            "free type param top level",
            ResolvedTy::TypeParam {
                name: "T".to_string(),
            },
            false,
        ),
    ];

    for (label, ty, admitted) in corpus {
        assert_eq!(
            builder.field_drop_in_place_admissible(&ty),
            admitted,
            "field-drop admissibility verdict moved for `{label}` \
             ({ty:?}); a widened verdict reaches codegen with no in-place \
             release (wrong-ABI / fail-closed error), a narrowed one \
             regresses an admitted discharge shape to the NYI refusal"
        );
    }
}

/// The owned-locals seed gate — "does a binding of this TYPE oblige drop
/// elaboration?" — with the verdict frozen per shape over every class
/// `ValueClass::of_ty` can answer. Only `BitCopy` declines to seed; every
/// other class (including the record-blind `Unknown` for unmarked user
/// records — a known, preserved limitation) enters `owned_locals`.
#[test]
fn seed_gate_matches_value_class_authority() {
    let mut type_classes = hew_hir::TypeClassTable::new();
    type_classes.insert("CopyRec".to_string(), (ResourceMarker::BitCopy, None));
    type_classes.insert("Sock".to_string(), (ResourceMarker::Resource, None));
    type_classes.insert("Once".to_string(), (ResourceMarker::Linear, None));
    let builder = Builder {
        type_classes,
        ..Builder::default()
    };

    // (shape, type, seeds-drop-elaboration) — the verdict column is the
    // FROZEN admission decision; a row here may only change together with
    // a deliberate, reviewed seed-rule change.
    let corpus: Vec<(&str, ResolvedTy, bool)> = vec![
        // BitCopy — the only class that does NOT seed.
        ("i64 scalar", ResolvedTy::I64, false),
        ("bool scalar", ResolvedTy::Bool, false),
        ("duration", ResolvedTy::Duration, false),
        ("unit", ResolvedTy::Unit, false),
        (
            "instant builtin",
            ResolvedTy::named_builtin("instant", BuiltinType::Instant, vec![]),
            false,
        ),
        ("bitcopy-marked record", named("CopyRec"), false),
        // CowValue seeds.
        ("string", ResolvedTy::String, true),
        ("bytes", ResolvedTy::Bytes, true),
        ("builtin Vec", vec_of(ResolvedTy::I64), true),
        (
            "tuple",
            ResolvedTy::Tuple(vec![ResolvedTy::I64, ResolvedTy::I64]),
            true,
        ),
        // PersistentShare seeds.
        ("bare fn", bare_fn(), true),
        ("empty-capture closure", empty_capture_closure(), true),
        (
            "dyn trait",
            ResolvedTy::TraitObject {
                traits: vec![hew_types::ResolvedTraitBound {
                    trait_name: "Display".to_string(),
                    args: vec![],
                    assoc_bindings: vec![],
                }],
            },
            true,
        ),
        // AffineResource seeds.
        ("cancellation token", ResolvedTy::CancellationToken, true),
        ("generator handle", generator_i64(), true),
        ("resource-marked named", named("Sock"), true),
        // Linear seeds (its release is the move-checker's MustConsume,
        // but membership in the candidate ledger is what is decided here).
        (
            "task handle",
            ResolvedTy::Task(Box::new(ResolvedTy::I64)),
            true,
        ),
        ("linear-marked named", named("Once"), true),
        // View seeds (never minted, so replay-derived plans schedule no drop).
        (
            "borrow",
            ResolvedTy::Borrow {
                pointee: Box::new(ResolvedTy::I64),
            },
            true,
        ),
        ("slice", ResolvedTy::Slice(Box::new(ResolvedTy::I64)), true),
        (
            "pointer",
            ResolvedTy::Pointer {
                is_mutable: false,
                pointee: Box::new(ResolvedTy::I64),
            },
            true,
        ),
        // Unknown seeds — the record-blind arm: an unmarked user record
        // classifies Unknown, not BitCopy, so it enters the ledger.
        ("unmarked named", named("Mystery"), true),
        (
            "type param",
            ResolvedTy::TypeParam {
                name: "T".to_string(),
            },
            true,
        ),
    ];

    for (label, ty, seeds) in corpus {
        assert_eq!(
            builder.binding_seeds_drop_elaboration(&ty),
            seeds,
            "owned-locals seed verdict moved for `{label}` ({ty:?}); \
             seeding decides drop-elaboration membership, so a flipped \
             verdict is an over-drop (double-free) or an under-seed (leak)"
        );
        assert_eq!(
            builder.binding_seeds_drop_elaboration(&ty),
            ValueClass::of_ty(&ty, &builder.type_classes) != ValueClass::BitCopy,
            "the seed authority's verdict must remain the value-class \
             seed for `{label}` ({ty:?})"
        );
    }
}

/// `ty_is_local_collection_handle` is a projection of the typed ownership
/// classification: it answers `true` exactly when the decision's drop
/// class is the `HashMap` / `HashSet` copy-on-write leaf. Corpus: every
/// heap leaf the authority recognises, plus the user-Named collision
/// negative (a user `type HashMap` shares the name but not the `builtin`
/// discriminator and must never be mistaken for the runtime handle).
#[test]
fn collection_handle_predicate_projects_from_heap_leaf() {
    let records: HashMap<String, Vec<(String, ResolvedTy)>> = HashMap::new();
    let type_classes = hew_hir::TypeClassTable::new();
    let ctx = OwnershipCtx::new(&records, &[], &type_classes);

    let corpus: Vec<(&str, ResolvedTy, bool)> = vec![
        ("string", ResolvedTy::String, false),
        ("bytes", ResolvedTy::Bytes, false),
        ("vec", vec_of(ResolvedTy::I64), false),
        ("hashmap", hashmap_str_i64(), true),
        ("hashset", hashset_i64(), true),
        ("generator", generator_i64(), false),
        ("cancellation token", ResolvedTy::CancellationToken, false),
        ("user-named HashMap collision", named("HashMap"), false),
    ];

    for (label, ty, expected) in corpus {
        assert_eq!(
            ty_is_local_collection_handle(&ty),
            expected,
            "collection-handle bucket membership moved for `{label}` ({ty:?})"
        );
        let projects = matches!(
            OwnershipDecision::classify(&ty, Place::Local(0), &ctx).drop_class(),
            Some(DropClass::CowHeapLeaf {
                leaf: HeapLeaf::HashMap | HeapLeaf::HashSet
            })
        );
        assert_eq!(
            projects, expected,
            "`{label}` ({ty:?}): the typed classification and \
             `ty_is_local_collection_handle` must answer identically — \
             a future builtin collection added to one but not the other \
             splits bucket admission from classification"
        );
    }

    // Symbol-agreement tripwire: the leaves' canonical release symbols are
    // exactly the two symbols the collection-handle bucket emits in
    // `drop_kind_for`.
    assert_eq!(
        HeapLeaf::HashMap.release_symbol(),
        "hew_hashmap_free_layout",
        "HashMap leaf release symbol must match the bucket's emission"
    );
    assert_eq!(
        HeapLeaf::HashSet.release_symbol(),
        "hew_hashset_free_layout",
        "HashSet leaf release symbol must match the bucket's emission"
    );
}

/// The complete release-verdict table for both Builder-side pickers —
/// `generator_yield_drop_symbol` (matches the RAW type) and
/// `project_field_inline_drop_symbol` (substitutes FIRST) — frozen per
/// shape: the `Vec` arm over every `VecElementRelease` variant (both
/// `FailClosedReason` arms represented), the defensive no-type-arg `Vec`,
/// and the non-`Vec` arms.
///
/// The `Unsupported(NoReleaseProtocol)` rows with no owned-ABI release
/// (`Vec<bytes>`, `Vec<indirect enum>`) assert the FAIL-CLOSED verdict
/// (`Unwired`): the per-element release for those shapes is unwired, so
/// every consulting site must refuse the construct at compile time —
/// never emit the buffer-only `hew_vec_free` over owned element nodes.
/// The residual `Unsupported(UnenumeratedShape)` sub-domain deliberately
/// keeps the buffer-only verdict, drawing the same boundary as the compile
/// reject `unsupported_vec_element_walk`:
///   - `UnenumeratedShape` (`Vec<T>` unsubstituted): the element owns no
///     heap as a flat element, so the buffer free IS the complete
///     release — refusing would reject un-monomorphised generic bodies;
///
/// A registered heap-owning record observed without this function's
/// harvest key is instead classified harvest-independently and released
/// through `hew_vec_free_owned`.
#[test]
#[allow(
    clippy::too_many_lines,
    reason = "the length is intrinsic: one frozen symbol matrix over every \
              picker input shape, asserted against both pickers — splitting \
              it would scatter the single-table proof across functions"
)]
fn yield_and_field_pickers_match_legacy_symbol_table() {
    use ReleaseSymbolVerdict::{NoDropPath, Unwired, Wired};

    let mut builder = builder_with_indirect_enum_foo();
    // A registered heap-owning record whose `Vec` is owned-ABI releasable
    // program-wide but whose key is NOT in this builder's per-function
    // harvest set — the boundary row for the releasable `Unsupported`
    // sub-domain.
    builder.record_field_orders.insert(
        "HeapRow".to_string(),
        vec![("s".to_string(), ResolvedTy::String)],
    );

    // (shape, type, generator-yield verdict, project-field verdict) —
    // every verdict column FROZEN. The two pickers agree on every row
    // here; the substitution-order asymmetry is pinned separately below.
    let corpus: Vec<(&str, ResolvedTy, ReleaseSymbolVerdict, ReleaseSymbolVerdict)> = vec![
        // Vec arm — Plain elements.
        (
            "Vec<i64> (Plain)",
            vec_of(ResolvedTy::I64),
            Wired("hew_vec_free"),
            Wired("hew_vec_free"),
        ),
        (
            "Vec<string> (Plain)",
            vec_of(ResolvedTy::String),
            Wired("hew_vec_free"),
            Wired("hew_vec_free"),
        ),
        // Vec arm — OwnedElement elements.
        (
            "Vec<Vec<i64>> (OwnedElement)",
            vec_of(vec_of(ResolvedTy::I64)),
            Wired("hew_vec_free_owned"),
            Wired("hew_vec_free_owned"),
        ),
        (
            "Vec<HashMap<string,i64>> (OwnedElement)",
            vec_of(hashmap_str_i64()),
            Wired("hew_vec_free_owned"),
            Wired("hew_vec_free_owned"),
        ),
        (
            "Vec<(string,i64)> (OwnedElement)",
            vec_of(ResolvedTy::Tuple(vec![ResolvedTy::String, ResolvedTy::I64])),
            Wired("hew_vec_free_owned"),
            Wired("hew_vec_free_owned"),
        ),
        // Vec arm — ClosurePair elements.
        (
            "Vec<fn> (ClosurePair)",
            vec_of(bare_fn()),
            Wired("hew_vec_free_owned"),
            Wired("hew_vec_free_owned"),
        ),
        (
            "Vec<closure> (ClosurePair)",
            vec_of(empty_capture_closure()),
            Wired("hew_vec_free_owned"),
            Wired("hew_vec_free_owned"),
        ),
        // Vec arm — Unsupported elements with NO owned-ABI release: the
        // FAIL-CLOSED verdict. A buffer-only free over these element
        // nodes is a per-element leak, so the pickers refuse instead of
        // picking a symbol; every consulting site rejects at compile
        // time (see the test doc).
        (
            "Vec<bytes> (Unsupported/NoReleaseProtocol)",
            vec_of(ResolvedTy::Bytes),
            Unwired(FailClosedReason::NoReleaseProtocol),
            Unwired(FailClosedReason::NoReleaseProtocol),
        ),
        (
            "Vec<indirect enum> (Unsupported/NoReleaseProtocol)",
            vec_of(named("Foo")),
            Unwired(FailClosedReason::NoReleaseProtocol),
            Unwired(FailClosedReason::NoReleaseProtocol),
        ),
        // Vec arm — the residual Unsupported sub-domain that keeps the
        // buffer-only verdict (the boundary
        // `unsupported_vec_element_walk` draws; see the test doc).
        (
            "Vec<T> unsubstituted (Unsupported/UnenumeratedShape)",
            vec_of(ResolvedTy::TypeParam {
                name: "T".to_string(),
            }),
            Wired("hew_vec_free"),
            Wired("hew_vec_free"),
        ),
        (
            "Vec<HeapRow> unharvested (Unsupported/NoReleaseProtocol, owned-ABI releasable)",
            vec_of(named("HeapRow")),
            Wired("hew_vec_free_owned"),
            Wired("hew_vec_free_owned"),
        ),
        // Vec arm — defensive no-type-arg fall-through.
        (
            "Vec with no type arg (defensive)",
            ResolvedTy::named_builtin("Vec", BuiltinType::Vec, vec![]),
            Wired("hew_vec_free"),
            Wired("hew_vec_free"),
        ),
        // Non-Vec arms — must not move when the Vec arm reroutes.
        (
            "string",
            ResolvedTy::String,
            Wired("hew_string_drop"),
            Wired("hew_string_drop"),
        ),
        (
            "bytes",
            ResolvedTy::Bytes,
            Wired("hew_bytes_drop"),
            Wired("hew_bytes_drop"),
        ),
        // VecIter clone-out and the existing generator/receiver frame
        // contracts hand these collection values to the body as sole
        // owners. Their layout-aware releases close the common per-yield
        // lifecycle.
        (
            "HashMap",
            hashmap_str_i64(),
            Wired("hew_hashmap_free_layout"),
            Wired("hew_hashmap_free_layout"),
        ),
        (
            "HashSet",
            hashset_i64(),
            Wired("hew_hashset_free_layout"),
            Wired("hew_hashset_free_layout"),
        ),
        (
            "Generator",
            generator_i64(),
            NoDropPath,
            Wired("hew_gen_coro_destroy"),
        ),
        ("i64", ResolvedTy::I64, NoDropPath, NoDropPath),
        ("unmarked user record", named("Rec"), NoDropPath, NoDropPath),
    ];

    for (label, ty, want_yield, want_field) in corpus {
        assert_eq!(
            builder.generator_yield_drop_symbol(&ty),
            want_yield,
            "generator-yield release verdict moved for `{label}` ({ty:?})"
        );
        assert_eq!(
            builder.project_field_inline_drop_symbol(&ty),
            want_field,
            "project-field release verdict moved for `{label}` ({ty:?})"
        );
    }

    // The Unsupported rows above carry exactly the two fail-closed
    // reasons: the unwired release protocols and the anti-drift sentinel.
    assert_eq!(
        builder.classify_vec_element_release(&ResolvedTy::Bytes),
        VecElementRelease::Unsupported(FailClosedReason::NoReleaseProtocol)
    );
    assert_eq!(
        builder.classify_vec_element_release(&named("Foo")),
        VecElementRelease::Unsupported(FailClosedReason::NoReleaseProtocol)
    );
    assert_eq!(
        builder.classify_vec_element_release(&ResolvedTy::TypeParam {
            name: "T".to_string(),
        }),
        VecElementRelease::Unsupported(FailClosedReason::UnenumeratedShape)
    );
    // The releasable-boundary row rides `NoReleaseProtocol` too — it is
    // the `elem_is_owned_abi_releasable` exclusion, not the reason, that
    // keeps it off the fail-closed verdict.
    assert_eq!(
        builder.classify_vec_element_release(&named("HeapRow")),
        VecElementRelease::Unsupported(FailClosedReason::NoReleaseProtocol)
    );
    assert!(builder.elem_is_owned_abi_releasable(&named("HeapRow")));
    assert!(!builder.elem_is_owned_abi_releasable(&named("Foo")));

    // Substitution-order asymmetry, frozen: `generator_yield_drop_symbol`
    // classifies the RAW type (a yield's type is already concrete at its
    // producer); `project_field_inline_drop_symbol` substitutes through
    // the monomorphisation map FIRST (a field type may still spell the
    // function's type parameter). With `T ↦ fn() -> unit` the two pickers
    // therefore answer differently for `Vec<T>` — harmonising them would
    // move release decisions.
    builder.subst = [("T".to_string(), bare_fn())].into_iter().collect();
    let vec_t = vec_of(ResolvedTy::TypeParam {
        name: "T".to_string(),
    });
    assert_eq!(
        builder.generator_yield_drop_symbol(&vec_t),
        Wired("hew_vec_free"),
        "the yield picker must classify the raw (unsubstituted) type"
    );
    assert_eq!(
        builder.project_field_inline_drop_symbol(&vec_t),
        Wired("hew_vec_free_owned"),
        "the field picker must substitute before classifying"
    );
}
