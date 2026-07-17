#[cfg(test)]
use super::*;

#[cfg(test)]
mod binding_ty_is_plain_vec_tuple {
    //! MIR invariant tests for the tuple arm of `binding_ty_is_plain_vec`.
    //!
    //! `ValueClass::of_ty(Tuple(_))` ALWAYS returns `CowValue` regardless of
    //! element types, so the tuple arm uses `tuple_is_all_bitcopy` instead.
    //! These tests pin the exact boundary:
    //!
    //! - `Vec<(i64, i64)>`: all `BitCopy` scalars → admitted as plain Vec →
    //!   scope-exit release is buffer-only `hew_vec_free`.
    //! - `Vec<(string, i64)>`: the `string` field owns heap → NOT admitted here
    //!   → routes to `hew_vec_free_owned` via the owned arm.
    //! - `Vec<((Rec, i64), bool)>`: a `Named` type inside a nested tuple that is
    //!   not registered as `BitCopy` → NOT admitted here (regression guard for
    //!   the `!is_owned_vec_element` soundness bug where `ty_contains_heap_owning`
    //!   mis-classifies unregistered Named types as non-heap-owning).
    //!
    //! The negative tests are the anti-regression guards: admitting a
    //! `Vec<(string,i64)>` or `Vec<((Rec,i64),bool)>` to `hew_vec_free` would
    //! skip the per-element owned drop and leak or corrupt heap.
    use super::*;

    fn vec_of_ty(elem: ResolvedTy) -> ResolvedTy {
        ResolvedTy::Named {
            name: "Vec".to_string(),
            args: vec![elem],
            builtin: Some(BuiltinType::Vec),
            is_opaque: false,
        }
    }

    fn unregistered_named(name: &str) -> ResolvedTy {
        ResolvedTy::Named {
            name: name.to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        }
    }

    /// `Vec<(i64, i64)>` — an all-`BitCopy` tuple element. `tuple_is_all_bitcopy`
    /// returns `true` (both fields are `BitCopy` scalars), so
    /// `binding_ty_is_plain_vec` must return `true` and the Vec earns a
    /// buffer-only `hew_vec_free` on scope exit. Pre-fix this returned `false`
    /// (the Tuple arm used `ValueClass::of_ty` which always returned `CowValue`),
    /// leaving the Vec in neither the plain nor the owned allow-set → leak.
    #[test]
    fn all_bitcopy_tuple_element_is_admitted_as_plain_vec() {
        let builder = Builder::default();
        let ty = vec_of_ty(ResolvedTy::Tuple(vec![ResolvedTy::I64, ResolvedTy::I64]));
        assert!(
            builder.binding_ty_is_plain_vec(&ty),
            "Vec<(i64,i64)>: an all-`BitCopy` tuple element owns no heap; \
             `binding_ty_is_plain_vec` must return true so the buffer gets \
             a scope-exit `hew_vec_free` (not leaked)"
        );
    }

    /// `Vec<(string, i64)>` — the `string` field owns heap. `tuple_is_all_bitcopy`
    /// returns `false`, so `binding_ty_is_plain_vec` must return `false` and the
    /// Vec routes to `hew_vec_free_owned` instead. Admitting it here would emit a
    /// buffer-only free, skipping the per-element string drop → string heap leak.
    #[test]
    fn heap_owning_tuple_element_is_not_admitted_as_plain_vec() {
        let builder = Builder::default();
        let ty = vec_of_ty(ResolvedTy::Tuple(vec![ResolvedTy::String, ResolvedTy::I64]));
        assert!(
            !builder.binding_ty_is_plain_vec(&ty),
            "Vec<(string,i64)>: the `string` field owns heap; \
             `binding_ty_is_plain_vec` must return false so the Vec routes \
             to `hew_vec_free_owned` (per-element string drop runs)"
        );
    }

    /// Regression guard: `Vec<((Rec, i64), bool)>` where `Rec` is a user Named
    /// type not in `type_classes` (so `ValueClass::of_ty` returns `CowValue`,
    /// not `BitCopy`). `tuple_is_all_bitcopy` returns `false` for the nested
    /// tuple because `Rec` is not `BitCopy`-proven. Pre-`tuple_is_all_bitcopy`
    /// the old `!is_owned_vec_element` path returned `true` (soundness bug:
    /// `ty_contains_heap_owning` omits `record_field_resolved_tys` so it
    /// mis-classifies unregistered Named types as non-heap-owning), admitting
    /// the Vec to `hew_vec_free` when `hew_vec_free_owned` is required.
    #[test]
    fn nested_tuple_with_unregistered_named_is_not_admitted() {
        let builder = Builder::default();
        let inner_tuple = ResolvedTy::Tuple(vec![unregistered_named("Rec"), ResolvedTy::I64]);
        let outer_tuple = ResolvedTy::Tuple(vec![inner_tuple, ResolvedTy::Bool]);
        let ty = vec_of_ty(outer_tuple);
        assert!(
            !builder.binding_ty_is_plain_vec(&ty),
            "Vec<((Rec,i64),bool)> where Rec is not proven BitCopy must NOT be \
             admitted as plain Vec — `tuple_is_all_bitcopy` must return false \
             for any Named element whose ValueClass is not BitCopy"
        );
    }

    /// Negative: a `Vec<string>` plain element (already covered by the scalar
    /// arm in `binding_ty_is_plain_vec`) must still be admitted after the tuple
    /// change. Pins that widening the tuple arm does not narrow the scalar arm.
    #[test]
    fn string_scalar_element_still_admitted_after_tuple_change() {
        let builder = Builder::default();
        let ty = vec_of_ty(ResolvedTy::String);
        assert!(
            builder.binding_ty_is_plain_vec(&ty),
            "Vec<string> must still be admitted as a plain Vec after the tuple \
             arm change (scalar arm must be unaffected)"
        );
    }

    /// Negative control: `Vec<i64>` (a `BitCopy` scalar element) is admitted.
    #[test]
    fn i64_scalar_element_admitted() {
        let builder = Builder::default();
        let ty = vec_of_ty(ResolvedTy::I64);
        assert!(
            builder.binding_ty_is_plain_vec(&ty),
            "Vec<i64> must be admitted as a plain Vec (scalar `BitCopy` arm)"
        );
    }

    /// Build a `Builder` with a HEAP-payload `indirect enum Foo { A(string); B }`
    /// registered. Unlike the scalar-payload enum, its `A(string)` payload owns
    /// heap, so `named_elem_owns_heap(Foo)` is `true` — the case the
    /// scalar-only `!named_elem_owns_heap` guard in `elem_is_owned_abi_releasable`
    /// used to miss, letting a heap-payload `Vec<indirect_enum>` reach codegen
    /// with a construct/release ABI mismatch. The explicit `ty_is_indirect_enum`
    /// guard now keeps it on the fail-closed reject.
    fn builder_with_heap_payload_indirect_enum() -> Builder {
        Builder {
            enum_layouts: vec![crate::model::EnumLayout {
                name: "Foo".to_string(),
                tag_width: 1,
                variants: vec![
                    crate::model::MachineVariantLayout {
                        name: "A".to_string(),
                        field_tys: vec![ResolvedTy::String],
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
            ..Default::default()
        }
    }

    /// Regression (heap-payload indirect enum): a `Vec<indirect enum Foo {
    /// A(string); B }>` must be rejected. Its `A(string)` payload owns heap, so
    /// `named_elem_owns_heap(Foo)` is `true` and the element is NOT excluded by
    /// the `!named_elem_owns_heap` check — yet an indirect-enum `Vec` rides the
    /// plain pointer ABI (`hew_vec_new_ptr`) while `hew_vec_free_owned` has no
    /// indirect-aware per-element node free, so admitting it as owned-ABI
    /// releasable would ship a construct/release ABI mismatch to codegen. The
    /// explicit `ty_is_indirect_enum` guard in `elem_is_owned_abi_releasable`
    /// keeps every indirect enum on the fail-closed `Unsupported(NoReleaseProtocol)`
    /// reject regardless of payload.
    #[test]
    fn heap_payload_indirect_enum_vec_element_rejected() {
        let builder = builder_with_heap_payload_indirect_enum();
        let foo = unregistered_named("Foo");

        // The payload owns heap — the scalar-only guard would NOT have excluded
        // this element, so the reject depends on the explicit indirect-enum guard.
        assert!(
            builder.named_elem_owns_heap(&foo),
            "indirect enum Foo {{ A(string); B }}: the A(string) payload owns heap"
        );
        assert!(
            !builder.elem_is_owned_abi_releasable(&foo),
            "a heap-payload indirect enum is NOT owned-ABI releasable — its per-element \
             node free is unwired; it must stay on the fail-closed reject"
        );
        assert!(
            !builder.is_owned_vec_element(&foo),
            "an indirect-enum element is built through the plain pointer ABI and must \
             never route to the owned-element free"
        );
        assert!(
            builder
                .unsupported_vec_element_in_ty(&vec_of_ty(foo.clone()))
                .is_some(),
            "Vec<heap-payload indirect enum> has no wired per-element node free — must be rejected"
        );
        assert!(
            builder
                .unsupported_vec_element_in_ty(&vec_of_ty(vec_of_ty(foo)))
                .is_some(),
            "Vec<Vec<heap-payload indirect enum>>: the inner unwired element must be found \
             by descending into E"
        );
    }

    /// The heap-payload indirect-enum reject also reaches TRANSITIVELY — through a
    /// record field, a tuple element, a type argument, and a self-recursive
    /// `Vec<_>` payload (`indirect enum List { Cons(i64, Vec<List>); Nil }`, whose
    /// own `Cons` payload owns heap). A composite drop recurses into these, so an
    /// unclaimed indirect-enum element nested inside leaks exactly as a bare
    /// `Vec<Foo>` would.
    #[test]
    fn heap_payload_indirect_enum_vec_element_rejected_transitively() {
        let foo = unregistered_named("Foo");

        let mut builder = builder_with_heap_payload_indirect_enum();
        // Record field: `Holder { items: Vec<Foo> }`.
        builder.record_field_orders.insert(
            "Holder".to_string(),
            vec![("items".to_string(), vec_of_ty(foo.clone()))],
        );
        assert!(
            builder
                .unsupported_vec_element_in_ty(&unregistered_named("Holder"))
                .is_some(),
            "a record field Vec<heap-payload indirect enum> must be rejected transitively"
        );

        // Tuple element: `(Vec<Foo>, i64)`.
        assert!(
            builder
                .unsupported_vec_element_in_ty(&ResolvedTy::Tuple(vec![
                    vec_of_ty(foo.clone()),
                    ResolvedTy::I64,
                ]))
                .is_some(),
            "a tuple element Vec<heap-payload indirect enum> must be rejected transitively"
        );

        // Type argument: `Option<Vec<Foo>>`.
        assert!(
            builder
                .unsupported_vec_element_in_ty(&ResolvedTy::Named {
                    name: "Option".to_string(),
                    args: vec![vec_of_ty(foo)],
                    builtin: None,
                    is_opaque: false,
                })
                .is_some(),
            "a type-argument Vec<heap-payload indirect enum> must be rejected transitively"
        );

        // Self-recursive heap payload: `indirect enum List { Cons(i64, Vec<List>); Nil }`.
        // `Vec<List>` must reject — List is an indirect enum — and the walk must
        // terminate on the self-reference (cycle-guarded).
        let rec_builder = Builder {
            enum_layouts: vec![crate::model::EnumLayout {
                name: "List".to_string(),
                tag_width: 1,
                variants: vec![
                    crate::model::MachineVariantLayout {
                        name: "Cons".to_string(),
                        field_tys: vec![ResolvedTy::I64, vec_of_ty(unregistered_named("List"))],
                        field_names: vec![],
                    },
                    crate::model::MachineVariantLayout {
                        name: "Nil".to_string(),
                        field_tys: vec![],
                        field_names: vec![],
                    },
                ],
                is_indirect: true,
            }],
            ..Default::default()
        };
        assert!(
            rec_builder
                .unsupported_vec_element_in_ty(&vec_of_ty(unregistered_named("List")))
                .is_some(),
            "Vec<self-recursive indirect enum List> must be rejected (its per-element node \
             free is unwired) and the recursion must terminate"
        );
    }

    /// Non-over-rejection guard for the indirect-enum exclusion: making EVERY
    /// indirect enum fail closed must NOT catch shapes that are genuinely
    /// owned-ABI releasable. A DIRECT (non-indirect) heap-owning enum is stored
    /// inline and released through the owned-element ABI, and a heap-owning
    /// record is likewise releasable — both stay `elem_is_owned_abi_releasable`
    /// and are NOT rejected. Only indirectness flips the verdict.
    #[test]
    fn is_indirect_exclusion_preserves_direct_enum_and_owned_record() {
        let mut builder = Builder {
            // A DIRECT heap-owning enum `DirE { A(string); B }` (is_indirect:
            // false) — inline storage, owned-element ABI applies.
            enum_layouts: vec![crate::model::EnumLayout {
                name: "DirE".to_string(),
                tag_width: 1,
                variants: vec![
                    crate::model::MachineVariantLayout {
                        name: "A".to_string(),
                        field_tys: vec![ResolvedTy::String],
                        field_names: vec![],
                    },
                    crate::model::MachineVariantLayout {
                        name: "B".to_string(),
                        field_tys: vec![],
                        field_names: vec![],
                    },
                ],
                is_indirect: false,
            }],
            ..Default::default()
        };
        // A heap-owning record `Pair { name: string }`.
        builder.record_field_orders.insert(
            "Pair".to_string(),
            vec![("name".to_string(), ResolvedTy::String)],
        );

        let dir_e = unregistered_named("DirE");
        assert!(
            !ty_is_indirect_enum(&dir_e, &builder.enum_layouts),
            "a direct enum is not an indirect enum — the exclusion must not touch it"
        );
        assert!(
            builder.elem_is_owned_abi_releasable(&dir_e),
            "a direct heap-owning enum is owned-ABI releasable (inline storage) — the \
             indirect-enum exclusion must not over-reject it"
        );
        assert!(
            builder.elem_is_owned_abi_releasable(&unregistered_named("Pair")),
            "a heap-owning record is owned-ABI releasable — the indirect-enum exclusion \
             must not over-reject it"
        );
    }

    /// Build a `Builder` with a scalar-payload `indirect enum Foo { A(i64); B }`
    /// registered, so `Vec<Foo>` exercises the indirect-enum fail-closed arm of
    /// the release partition.
    fn builder_with_indirect_enum() -> Builder {
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
            ..Default::default()
        }
    }

    /// Boundary-totality, expressed POSITIVELY through the typed
    /// [`VecElementRelease`] decision: every `Vec<E>` element resolves to exactly
    /// ONE release disposition (`Plain` | `OwnedElement` | `ClosurePair` |
    /// `Unsupported`), and the three bool release-bucket predicates
    /// (`binding_ty_is_plain_vec`, `binding_ty_is_owned_element_vec`,
    /// `ty_is_closure_pair_vec`) are projections of it. The outer `Vec` ALWAYS
    /// owns heap (`ty_owns_heap(Vec<_>)` is `true` unconditionally — a `Vec` owns
    /// its backing buffer for ANY element), so a heap-owning `Vec<E>` local must
    /// resolve to a claimed bucket XOR a tracked `Unsupported` — never a silent
    /// fall-through through every bucket (the leak surface the `_ => false` bucket
    /// arms used to be).
    ///
    /// `Vec<bytes>` and `Vec<indirect_enum>` are EXPLICIT `Unsupported` entries,
    /// GREEN BY CONSTRUCTION (not a gap-tripwire `assert!(!claimed)`): the typed
    /// decision names them `Unsupported(NoReleaseProtocol)` — a defined,
    /// actionable "release ABI unwired", not a silent non-owning `false`. `bytes`
    /// is a fat `{ ptr, len, cap }` triple outside the single-pointer buckets and
    /// `Vec<bytes>` is unconstructible (`Vec::new` is NYI for `Bytes`); a
    /// scalar-payload `indirect enum` is a heap-boxed node whose pointer-element
    /// release (a node free per element) is a tracked follow-up. Both own heap no
    /// bucket can release, so `Builder::unsupported_vec_element_diagnostics`
    /// consumes this `Unsupported` disposition to REJECT them at compile rather
    /// than silently leak them at scope exit; the reject lifts once the
    /// per-element release is wired. If either becomes claimed by a real release
    /// path, swap its expected arm here — the test fails first if a bucket
    /// silently starts (or stops) claiming it.
    #[test]
    #[allow(
        clippy::too_many_lines,
        reason = "the length is intrinsic: this is the single partition-totality \
                  matrix over every Vec<E> element shape (scalars, string, tuple, \
                  the four enum classes, nested collections, closure pair, and the \
                  fail-closed entries), each asserted against the typed decision \
                  and its three projected buckets — splitting it would scatter the \
                  disjoint-cover proof across functions"
    )]
    fn release_bucket_partition_is_total_over_vec_elements() {
        // Register the full USER-ENUM matrix alongside the indirect scalar
        // `Foo`, so the partition is proven total over enums (not just over the
        // builtin/scalar/tuple shapes the loops below already cover). User
        // enums are never marked `ValueClass::BitCopy` (the HIR value-class pass
        // finalises records only), so the Plain bucket must claim heap-free
        // enums via the heap-ownership authority, not the value-class table:
        //   - `Colour` — a DIRECT fieldless enum, owns no heap → Plain.
        //   - `Tone`   — a DIRECT scalar-payload enum, owns no heap → Plain.
        //   - `DirE`   — a DIRECT heap-payload (`string`) enum, owns heap →
        //                OwnedElement (proves the Plain swap does NOT over-drop
        //                a genuinely heap-owning enum — it still routes owned).
        //   - `Foo`    — the indirect scalar enum from `builder_with_indirect_enum`
        //                → Unsupported(NoReleaseProtocol) (heap-boxed node, no
        //                wired per-element release).
        // Compact non-indirect enum-layout builder: one `EnumLayout` whose
        // variants carry the given field types (empty = fieldless).
        fn direct_enum(
            name: &str,
            variants: &[(&str, Vec<ResolvedTy>)],
        ) -> crate::model::EnumLayout {
            crate::model::EnumLayout {
                name: name.to_string(),
                tag_width: 1,
                variants: variants
                    .iter()
                    .map(|(vname, field_tys)| crate::model::MachineVariantLayout {
                        name: (*vname).to_string(),
                        field_tys: field_tys.clone(),
                        field_names: vec![],
                    })
                    .collect(),
                is_indirect: false,
            }
        }
        let mut builder = builder_with_indirect_enum();
        builder.enum_layouts.push(direct_enum(
            "Colour",
            &[("Red", vec![]), ("Green", vec![]), ("Blue", vec![])],
        ));
        builder.enum_layouts.push(direct_enum(
            "Tone",
            &[
                ("Bright", vec![ResolvedTy::I64]),
                ("Dark", vec![ResolvedTy::I64]),
            ],
        ));
        builder.enum_layouts.push(direct_enum(
            "DirE",
            &[("A", vec![ResolvedTy::String]), ("B", vec![])],
        ));
        // Harvest DirE's owned-element key exactly as lowering would when it
        // observes a `Vec<DirE>` construction, so `is_owned_vec_element` claims
        // it (the owned bucket is keyed off the harvested set, not re-derived).
        builder.harvest_vec_owned_element_key(&vec_of_ty(unregistered_named("DirE")));
        let builder = builder;

        // Assert `elem` resolves to `want`, the outer `Vec<elem>` owns heap, and
        // the three bool buckets project EXACTLY from the typed decision (so the
        // partition is a disjoint cover, not just non-empty).
        let assert_disposition = |elem: ResolvedTy, want: VecElementRelease| {
            let v = vec_of_ty(elem.clone());
            assert!(
                builder.named_elem_owns_heap(&v),
                "ty_owns_heap(Vec<{elem:?}>) must be true — the outer Vec owns its buffer"
            );
            assert_eq!(
                builder.classify_vec_element_release(&elem),
                want,
                "Vec<{elem:?}> element release disposition"
            );
            assert_eq!(
                builder.binding_ty_is_plain_vec(&v),
                want.is_plain(),
                "binding_ty_is_plain_vec(Vec<{elem:?}>) must project from the typed decision"
            );
            assert_eq!(
                builder.binding_ty_is_owned_element_vec(&v),
                want.is_owned_element(),
                "binding_ty_is_owned_element_vec(Vec<{elem:?}>) must project from the typed decision"
            );
            assert_eq!(
                ty_is_closure_pair_vec(&v),
                want.is_closure_pair(),
                "ty_is_closure_pair_vec(Vec<{elem:?}>) must project from the typed decision"
            );
        };

        // Plain bucket: BitCopy scalar / string / all-BitCopy aggregate
        // elements, plus heap-free DIRECT user enums (fieldless + scalar
        // payload) and a tuple whose fields include a heap-free direct enum.
        for elem in [
            ResolvedTy::I64,
            ResolvedTy::Bool,
            ResolvedTy::F64,
            ResolvedTy::Char,
            ResolvedTy::Duration,
            ResolvedTy::String,
            ResolvedTy::Tuple(vec![ResolvedTy::I64, ResolvedTy::I64]),
            // Heap-free direct enums: never `ValueClass::BitCopy`, claimed by
            // the Plain bucket through `!named_elem_owns_heap` instead.
            unregistered_named("Colour"),
            unregistered_named("Tone"),
            // A tuple containing a heap-free direct enum is all-`BitCopy` too.
            ResolvedTy::Tuple(vec![unregistered_named("Colour"), ResolvedTy::I64]),
        ] {
            assert_disposition(elem, VecElementRelease::Plain);
        }

        // Owned-element bucket: nested collections + heap-owning tuples, plus a
        // heap-owning DIRECT enum (inline storage, owned-element ABI) and a
        // tuple carrying one — the swap must NOT reclassify these as Plain.
        for elem in [
            ResolvedTy::named_builtin("Vec", BuiltinType::Vec, vec![ResolvedTy::I64]),
            ResolvedTy::named_builtin(
                "HashMap",
                BuiltinType::HashMap,
                vec![ResolvedTy::String, ResolvedTy::I64],
            ),
            ResolvedTy::named_builtin("HashSet", BuiltinType::HashSet, vec![ResolvedTy::I64]),
            ResolvedTy::Tuple(vec![ResolvedTy::String, ResolvedTy::I64]),
            unregistered_named("DirE"),
            ResolvedTy::Tuple(vec![unregistered_named("DirE"), ResolvedTy::I64]),
        ] {
            assert_disposition(elem, VecElementRelease::OwnedElement);
        }

        // Closure-pair bucket.
        assert_disposition(
            ResolvedTy::Function {
                params: vec![],
                ret: Box::new(ResolvedTy::Unit),
            },
            VecElementRelease::ClosurePair,
        );

        // Fail-closed entries — explicit, GREEN BY CONSTRUCTION. `bytes`' fat
        // triple and the scalar-payload `indirect enum Foo` both own heap the
        // partition cannot release, so the typed decision is a tracked
        // `Unsupported(NoReleaseProtocol)` and all three bool buckets project
        // `false`.
        for elem in [ResolvedTy::Bytes, unregistered_named("Foo")] {
            assert_disposition(
                elem,
                VecElementRelease::Unsupported(FailClosedReason::NoReleaseProtocol),
            );
        }
    }

    /// Closure-pair release-symbol pin: the generator-yield and match-field
    /// release-symbol pickers must check the closure-pair bucket BEFORE the
    /// owned/plain split, matching codegen's `resolved_ty_cow_heap_release` dispatch
    /// order. A yielded or match-destructured `Vec<fn>` releases via
    /// `hew_vec_free_owned`; the pre-fix `generator_yield_drop_symbol`
    /// had only an owned/plain split and computed `hew_vec_free`, which diverges
    /// from codegen and fails the inline-drop congruence check closed. The owned
    /// and plain arms are pinned too so the closure-pair arm cannot displace them.
    #[test]
    fn vec_closure_pair_yield_and_field_drop_symbol_is_closure_pairs() {
        let builder = builder_with_indirect_enum();
        let vec_fn = vec_of_ty(ResolvedTy::Function {
            params: vec![],
            ret: Box::new(ResolvedTy::Unit),
        });
        assert_eq!(
            builder.generator_yield_drop_symbol(&vec_fn),
            ReleaseSymbolVerdict::Wired("hew_vec_free_owned"),
            "a yielded Vec<fn> must release via hew_vec_free_owned, not hew_vec_free"
        );
        assert_eq!(
            builder.project_field_inline_drop_symbol(&vec_fn),
            ReleaseSymbolVerdict::Wired("hew_vec_free_owned"),
            "a match-destructured Vec<fn> field must release via hew_vec_free_owned"
        );
        // The owned and plain Vec arms are unchanged and dispatch AFTER the
        // closure-pair arm (a fn element is neither owned composite nor plain
        // leaf, so it must not fall through to either).
        assert_eq!(
            builder.generator_yield_drop_symbol(&vec_of_ty(ResolvedTy::String)),
            ReleaseSymbolVerdict::Wired("hew_vec_free"),
            "Vec<string> is a plain release (buffer + runtime string walk)"
        );
        assert_eq!(
            builder.generator_yield_drop_symbol(&vec_of_ty(vec_of_ty(ResolvedTy::I64))),
            ReleaseSymbolVerdict::Wired("hew_vec_free_owned"),
            "Vec<Vec<i64>> is an owned-element release"
        );
        assert_eq!(
            builder.project_field_inline_drop_symbol(&vec_of_ty(vec_of_ty(ResolvedTy::I64))),
            ReleaseSymbolVerdict::Wired("hew_vec_free_owned"),
            "match-destructured Vec<Vec<i64>> field is an owned-element release"
        );
    }

    /// Focused pin: a scalar-payload `indirect enum` Vec element must fail closed
    /// as `Unsupported(NoReleaseProtocol)`, NOT silently land in the plain bucket
    /// (`BitCopy`). The heap-ownership authority is blind to indirection — a
    /// scalar `indirect enum`'s `ty_owns_heap` is `false` — so without the
    /// explicit indirect-enum probe in `vec_element_unsupported_reason` this node
    /// would mis-label. The element is heap-boxed (each slot is a `ptr` to a
    /// tagged-union node); a `BitCopy` classification would skip the node free.
    /// `NeverBitCopy`: the disposition is `Unsupported`, never `Plain`.
    #[test]
    fn indirect_enum_vec_element_fails_closed_not_plain() {
        let builder = builder_with_indirect_enum();
        let foo = unregistered_named("Foo");
        assert_eq!(
            builder.classify_vec_element_release(&foo),
            VecElementRelease::Unsupported(FailClosedReason::NoReleaseProtocol),
            "Vec<indirect enum Foo> must fail closed (release unwired), not be \
             mis-classified as a plain BitCopy element"
        );
        assert!(
            !builder.is_plain_vec_element(&foo),
            "an indirect-enum element is heap-boxed — never a plain BitCopy element"
        );
        assert!(
            !builder.is_owned_vec_element(&foo),
            "Vec<indirect_enum> is built through the plain pointer ABI (no owned \
             descriptor), so it must NOT route to the owned-element free"
        );
    }

    /// The production reject walk ([`Builder::unsupported_vec_element_in_ty`])
    /// finds a `Vec<E>` with no wired per-element release directly — a
    /// `Vec<bytes>` (fat triple) or a `Vec<indirect_enum>` (heap-boxed node) —
    /// and through a nested `Vec`. This walk is the consumer that moves the
    /// admit-then-leak "no" up to compile time; without it the element nodes fall
    /// through every scope-exit drop set and silently leak.
    #[test]
    fn unwired_vec_element_rejected_directly_and_when_nested() {
        let builder = builder_with_indirect_enum();
        let foo = unregistered_named("Foo");

        assert!(
            builder
                .unsupported_vec_element_in_ty(&vec_of_ty(ResolvedTy::Bytes))
                .is_some(),
            "Vec<bytes> owns heap with no single-pointer release bucket — must be rejected"
        );
        assert!(
            builder
                .unsupported_vec_element_in_ty(&vec_of_ty(foo.clone()))
                .is_some(),
            "Vec<indirect enum Foo> has no wired per-element node free — must be rejected"
        );
        assert!(
            builder
                .unsupported_vec_element_in_ty(&vec_of_ty(vec_of_ty(foo)))
                .is_some(),
            "Vec<Vec<indirect enum Foo>>: the outer Vec is OwnedElement, but the inner \
             unwired element must still be found by descending into E"
        );
    }

    /// The reject walk finds an unwired `Vec<E>` TRANSITIVELY — through a record
    /// field, a tuple element, and a type argument — not only as a top-level
    /// `Vec<E>` binding. A composite drop recurses into these fields, so an
    /// unclaimed element nested inside them leaks exactly as a bare `Vec<E>`
    /// would; the reject must reach it.
    #[test]
    fn unwired_vec_element_rejected_transitively() {
        let foo = unregistered_named("Foo");

        let mut builder = builder_with_indirect_enum();
        // Record field: `Holder { items: Vec<Foo> }`.
        builder.record_field_orders.insert(
            "Holder".to_string(),
            vec![("items".to_string(), vec_of_ty(foo.clone()))],
        );
        assert!(
            builder
                .unsupported_vec_element_in_ty(&unregistered_named("Holder"))
                .is_some(),
            "a record field Vec<indirect_enum> must be rejected transitively"
        );

        // Tuple element: `(Vec<Foo>, i64)`.
        assert!(
            builder
                .unsupported_vec_element_in_ty(&ResolvedTy::Tuple(vec![
                    vec_of_ty(foo.clone()),
                    ResolvedTy::I64,
                ]))
                .is_some(),
            "a tuple element Vec<indirect_enum> must be rejected transitively"
        );

        // Type argument: `Option<Vec<Foo>>` (a Named carrying the Vec as an arg).
        // The walk descends type arguments before record/enum payloads.
        let option_vec_foo = ResolvedTy::Named {
            name: "Option".to_string(),
            args: vec![vec_of_ty(foo)],
            builtin: None,
            is_opaque: false,
        };
        assert!(
            builder
                .unsupported_vec_element_in_ty(&option_vec_foo)
                .is_some(),
            "a type-argument Vec<indirect_enum> must be rejected transitively"
        );
    }

    /// Behaviour preservation: the reject walk fires ONLY for a genuinely unwired
    /// heap-owning element (`NoReleaseProtocol`). Every releasable or
    /// non-heap-owning `Vec` element is left untouched — a plain BitCopy/string
    /// element, a nested collection (`Vec<Vec<i64>>`), a heap-owning tuple, an
    /// owned record whose element release IS wired (harvested into
    /// `vec_owned_element_keys`), and an un-monomorphised generic `Vec<T>`
    /// skeleton (a free `TypeParam` owns no heap → `UnenumeratedShape`, never
    /// rejected). A bare indirect-enum local (NOT a `Vec` element) is also
    /// untouched — its scope-exit node free is wired separately
    /// (`DropKind::IndirectEnum`).
    #[test]
    fn releasable_and_non_owning_vec_elements_not_rejected() {
        // Owned record `Pair { name: string }` with its element release wired
        // (harvested key present), as production has by the time diagnostics run.
        let mut builder = builder_with_indirect_enum();
        builder.record_field_orders.insert(
            "Pair".to_string(),
            vec![("name".to_string(), ResolvedTy::String)],
        );
        builder.vec_owned_element_keys.insert("Pair".to_string());

        for ty in [
            vec_of_ty(ResolvedTy::I64),
            vec_of_ty(ResolvedTy::String),
            vec_of_ty(vec_of_ty(ResolvedTy::I64)),
            vec_of_ty(ResolvedTy::Tuple(vec![ResolvedTy::String, ResolvedTy::I64])),
            vec_of_ty(unregistered_named("Pair")),
            vec_of_ty(ResolvedTy::TypeParam {
                name: "T".to_string(),
            }),
            // A bare indirect-enum local is wired via DropKind::IndirectEnum.
            unregistered_named("Foo"),
        ] {
            assert!(
                builder.unsupported_vec_element_in_ty(&ty).is_none(),
                "{ty:?} has a wired release (or owns no heap) — must NOT be rejected"
            );
        }
    }

    /// Regression: a nested `Vec<owned-record>` whose element key was NOT
    /// harvested into THIS function's allow-list must NOT be rejected. The
    /// element is released through the owned-element ABI in the function that
    /// CONSTRUCTS its `Vec`; it reaches `Unsupported(NoReleaseProtocol)` here
    /// only because `vec_owned_element_keys` is harvested per function. This is
    /// the `Stack<Stack<i64>>` shape — the outer record holds a `Vec<Stack<i64>>`
    /// buffer whose `Stack<i64>` element owns heap (its own `Vec<i64>`) and is
    /// releasable, not unwired. Pre-fix the reject false-positived on it and
    /// failed `generic_ctor_return_type_mono` at compile.
    #[test]
    fn nested_owned_record_vec_element_not_rejected_without_harvested_key() {
        let mut builder = builder_with_indirect_enum();
        // `StackI64 { items: Vec<i64> }` — a heap-owning record (owns its Vec
        // buffer), releasable via the owned-element ABI when its Vec is built.
        builder.record_field_orders.insert(
            "StackI64".to_string(),
            vec![("items".to_string(), vec_of_ty(ResolvedTy::I64))],
        );
        // `OuterStack { items: Vec<StackI64> }` — the `Stack<Stack<i64>>` shape.
        builder.record_field_orders.insert(
            "OuterStack".to_string(),
            vec![(
                "items".to_string(),
                vec_of_ty(unregistered_named("StackI64")),
            )],
        );
        // NEITHER key is harvested into `vec_owned_element_keys` — exactly the
        // state of a function that does not directly CONSTRUCT the inner Vec.

        assert!(
            builder.elem_is_owned_abi_releasable(&unregistered_named("StackI64")),
            "a registered heap-owning record element is owned-ABI releasable \
             regardless of harvest"
        );
        assert!(
            builder
                .unsupported_vec_element_in_ty(&vec_of_ty(unregistered_named("StackI64")))
                .is_none(),
            "Vec<owned-record> with an un-harvested key must NOT be rejected — it \
             is released through the owned-element ABI in its constructing function"
        );
        assert!(
            builder
                .unsupported_vec_element_in_ty(&unregistered_named("OuterStack"))
                .is_none(),
            "a nested Vec<owned-record> field (the Stack<Stack<i64>> buffer) must \
             NOT false-positive as an unwired leak"
        );
    }

    /// The owned-local diagnostics pass emits exactly one fatal
    /// `NotYetImplemented` for a local holding an unwired `Vec` element, and none
    /// for a fully-releasable owned local — the production wiring that turns the
    /// admit-then-leak into a compile error before codegen.
    #[test]
    fn unsupported_vec_element_diagnostics_rejects_unwired_owned_local() {
        let mut builder = builder_with_indirect_enum();
        builder.register_owned_local(
            BindingId(7),
            "nodes".to_string(),
            vec_of_ty(unregistered_named("Foo")),
        );
        // The construction site is sourced from the finalized `Bind` statements,
        // keyed by the owned local's binding id.
        let bind_sites = std::collections::HashMap::from([(BindingId(7), SiteId(42))]);
        let diags = builder.unsupported_vec_element_diagnostics(&bind_sites);
        assert_eq!(diags.len(), 1, "one unwired owned local → one diagnostic");
        assert!(
            matches!(
                diags[0].kind,
                MirDiagnosticKind::NotYetImplemented {
                    site: SiteId(42),
                    ..
                }
            ),
            "the reject is a fatal NotYetImplemented carrying the real construction site"
        );

        // A releasable owned `Vec<string>` local emits nothing — behaviour
        // preserved for every constructible, releasable shape.
        let mut ok = builder_with_indirect_enum();
        ok.register_owned_local(
            BindingId(8),
            "names".to_string(),
            vec_of_ty(ResolvedTy::String),
        );
        assert!(
            ok.unsupported_vec_element_diagnostics(&bind_sites)
                .is_empty(),
            "a Vec<string> owned local has a wired release — no diagnostic"
        );
    }

    /// The single registration authority records the classified ownership fact
    /// ONCE at the defining write, defaults the disposition to scope exit, and
    /// exposes the binding through the `(binding, name, ty)` compat view every
    /// drop prover consumes — the byte-identical shape of the former tuple
    /// ledger.
    #[test]
    fn register_owned_local_records_classified_ownership_and_scope_exit() {
        use crate::ownership::{DropClass, HeapLeaf};

        let mut builder = builder_with_indirect_enum();
        builder.register_owned_local(BindingId(3), "s".to_string(), ResolvedTy::String);

        // The compat view is exactly the registered triple, in registration
        // order — what `owned_locals_snapshot` feeds the provers.
        assert_eq!(
            builder.owned_locals_snapshot(),
            vec![(BindingId(3), "s".to_string(), ResolvedTy::String)],
            "the live view is the registered (binding, name, ty) triple"
        );

        // The richer facts are carried on the entry: a `string` owns a single
        // copy-on-write heap leaf, the disposition is scope exit, and no
        // provenance is proven at this seam.
        let entry = &builder.owned_locals[0];
        assert_eq!(entry.disposition, Disposition::ScopeExit);
        assert_eq!(
            entry.ownership.drop_class(),
            Some(DropClass::CowHeapLeaf {
                leaf: HeapLeaf::String
            }),
            "ownership is classified once at registration and recorded on the entry"
        );
        assert_eq!(
            entry.provenance, None,
            "no provenance is proven at this seam"
        );
    }

    /// Frozen three-way verdict table for the `let`-bound field-load
    /// classification — the load-bearing double-free boundary. A `string` field retains
    /// (codegen clones), an inline aggregate (record / tuple) byte-copies to an
    /// interior alias, and every single-pointer heap leaf (`Vec` / `bytes` /
    /// `Generator` / indirect-enum node) transfers its one handle. Only
    /// `ByteCopyAlias` dispositions its binder `AliasOf`; the table freezes the
    /// class each shape maps to so a mis-tag (which would admit an owner the
    /// binder also releases — a double-free) fails this test first.
    #[test]
    fn classify_field_load_freezes_the_three_way_verdict_table() {
        let mut builder = builder_with_indirect_enum();
        // A heap-owning user record `Rec { a: string }`.
        builder.record_field_orders.insert(
            "Rec".to_string(),
            vec![("a".to_string(), ResolvedTy::String)],
        );

        // Retained: `string` — codegen `hew_string_clone`s the load.
        assert_eq!(
            builder.classify_field_load(&ResolvedTy::String),
            Some(FieldLoadClass::Retained),
        );

        // ByteCopyAlias: inline aggregates — record and tuple.
        assert_eq!(
            builder.classify_field_load(&unregistered_named("Rec")),
            Some(FieldLoadClass::ByteCopyAlias),
            "a heap-owning record field is byte-copied to an interior alias",
        );
        assert_eq!(
            builder.classify_field_load(&ResolvedTy::Tuple(vec![
                ResolvedTy::String,
                ResolvedTy::String,
            ])),
            Some(FieldLoadClass::ByteCopyAlias),
            "a heap-owning tuple field is byte-copied to an interior alias",
        );

        // HandleTransfer: every single-pointer heap leaf.
        for (label, ty) in [
            ("Vec", vec_of_ty(ResolvedTy::String)),
            ("bytes", ResolvedTy::Bytes),
            (
                "Generator",
                ResolvedTy::named_builtin(
                    "Generator",
                    BuiltinType::Generator,
                    vec![ResolvedTy::I64, ResolvedTy::Unit],
                ),
            ),
            ("indirect enum", unregistered_named("Foo")),
        ] {
            assert_eq!(
                builder.classify_field_load(&ty),
                Some(FieldLoadClass::HandleTransfer),
                "{label} transfers its one owned handle to the binder",
            );
        }

        // Heap-free field: no scope-exit drop obligation to classify.
        assert_eq!(builder.classify_field_load(&ResolvedTy::I64), None);
    }

    /// A byte-copy aggregate field projection registers its binder `AliasOf`
    /// with the owner's provenance recorded — and that disposition removes it
    /// from the scope-exit-live view the record/tuple provers and
    /// `build_lifo_drops` read, so the alias emits no composite drop of its own
    /// and its base local never seeds the provers' `release_owner_bases` (the
    /// #2375 blanket no longer trips). The entry survives in the whole ledger.
    #[test]
    fn byte_copy_alias_registers_aliasof_and_leaves_the_live_view() {
        let mut builder = builder_with_indirect_enum();
        builder.record_field_orders.insert(
            "Rec".to_string(),
            vec![("a".to_string(), ResolvedTy::String)],
        );
        builder.binding_locals.insert(BindingId(1), Place::Local(7));

        let provenance =
            ValueProvenance::projection(PlaceProvenance::Local(3), vec![Projection::Field(0)]);
        builder.register_owned_local_alias(
            BindingId(1),
            "mid".to_string(),
            unregistered_named("Rec"),
            provenance.clone(),
        );

        let entry = &builder.owned_locals[0];
        assert_eq!(entry.disposition, Disposition::AliasOf);
        assert_eq!(
            entry.provenance.as_ref(),
            Some(&provenance),
            "the owner it aliases is recorded on the entry",
        );
        assert!(
            builder.owned_locals_snapshot().is_empty(),
            "an AliasOf entry is excluded from the scope-exit-live drop view — no \
             composite drop, and it never seeds the provers' release_owner_bases",
        );
        assert_eq!(
            builder.owned_locals_ledger().len(),
            1,
            "the alias survives in the whole ledger regardless of disposition",
        );
    }

    /// Retraction is a disposition write, not a physical removal: a binding
    /// dispositioned off the scope-exit set leaves the live drop view
    /// (`owned_locals_snapshot`) yet stays observable in the whole ledger
    /// (`owned_locals_ledger`). This is the invariant that kills the
    /// retracted-invisible-to-final-scan class — the mechanism behind the
    /// double-free and #2375, where a physically-removed binding was gone from
    /// the ledger before an end-of-pass scan could see it.
    #[test]
    fn dispositioned_binding_leaves_live_view_but_survives_whole_ledger() {
        let mut builder = builder_with_indirect_enum();
        builder.register_owned_local(BindingId(3), "kept".to_string(), ResolvedTy::String);
        builder.register_owned_local(BindingId(4), "moved".to_string(), ResolvedTy::String);

        // Both are scope-exit-live before any retraction.
        assert_eq!(
            builder.owned_locals_snapshot(),
            vec![
                (BindingId(3), "kept".to_string(), ResolvedTy::String),
                (BindingId(4), "moved".to_string(), ResolvedTy::String),
            ],
            "both registered bindings are in the live view before retraction"
        );

        // Retract one via a consume disposition (what `mark_binding_moved` does).
        builder.set_owned_local_disposition(BindingId(4), Disposition::ConsumedAt);

        // The live view now excludes the retracted binding — exactly the set the
        // former `owned_locals.retain(...)` physical removal would have left.
        assert_eq!(
            builder.owned_locals_snapshot(),
            vec![(BindingId(3), "kept".to_string(), ResolvedTy::String)],
            "the retracted binding leaves the scope-exit-live view"
        );

        // The whole ledger still carries BOTH entries, in registration order —
        // the retracted one is observable to an end-of-pass scan, carrying its
        // recorded disposition. Physical removal made this impossible.
        let ledger = builder.owned_locals_ledger();
        assert_eq!(
            ledger.len(),
            2,
            "the whole ledger keeps the retracted entry"
        );
        assert_eq!(ledger[0].binding, BindingId(3));
        assert_eq!(ledger[0].disposition, Disposition::ScopeExit);
        assert_eq!(ledger[1].binding, BindingId(4));
        assert_eq!(
            ledger[1].disposition,
            Disposition::ConsumedAt,
            "the retracted entry survives carrying its recorded disposition"
        );
    }

    /// MIR↔codegen congruence pin (`dedup-semantic-boundary`): the MIR
    /// `is_owned_vec_element` element verdicts must equal codegen's
    /// `resolved_ty_element_owns_heap_for_owned_vec` over the shapes both classify
    /// the same way (non-`Named`, builtin collections, closure pairs, and an
    /// unregistered `Named` — which both report `false` with empty registries).
    /// The sibling test
    /// `resolved_ty_element_owns_heap_for_owned_vec_matches_mir_table` in
    /// `hew-codegen-rs` pins the SAME table on the codegen side, so a drift in
    /// either crate's owned-element decision fails its own test. (The MIR `Named`
    /// arm uses the function `vec_owned_element_keys` set — a harvested subset of
    /// codegen's direct-authority verdict — which converges with codegen for
    /// every constructible `Vec<record/enum>`, where the element is always
    /// harvested.)
    #[test]
    fn is_owned_vec_element_matches_codegen_owned_vec_table() {
        let builder = Builder::default();
        let fn_elem = ResolvedTy::Function {
            params: vec![ResolvedTy::I64],
            ret: Box::new(ResolvedTy::I64),
        };
        let closure_elem = ResolvedTy::Closure {
            params: vec![],
            ret: Box::new(ResolvedTy::Unit),
            captures: vec![ResolvedTy::String],
        };
        let table: [(ResolvedTy, bool); 11] = [
            (ResolvedTy::String, false),
            (ResolvedTy::Bytes, false),
            (ResolvedTy::I64, false),
            (
                ResolvedTy::Tuple(vec![ResolvedTy::String, ResolvedTy::I64]),
                true,
            ),
            (
                ResolvedTy::Tuple(vec![ResolvedTy::I64, ResolvedTy::I64]),
                false,
            ),
            (
                ResolvedTy::named_builtin(
                    "HashMap",
                    BuiltinType::HashMap,
                    vec![ResolvedTy::String, ResolvedTy::I64],
                ),
                true,
            ),
            (
                ResolvedTy::named_builtin("HashSet", BuiltinType::HashSet, vec![ResolvedTy::I64]),
                true,
            ),
            (
                ResolvedTy::named_builtin("Vec", BuiltinType::Vec, vec![ResolvedTy::I64]),
                true,
            ),
            (
                ResolvedTy::named_builtin("Vec", BuiltinType::Vec, vec![fn_elem.clone()]),
                false,
            ),
            (fn_elem, false),
            (closure_elem, false),
        ];
        for (elem, want) in table {
            assert_eq!(
                builder.is_owned_vec_element(&elem),
                want,
                "is_owned_vec_element({elem:?}) must equal the codegen owned-vec table"
            );
        }
    }
}
