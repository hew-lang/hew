//! `Vec<E>` element release classification and its projections.
//!
//! One typed decision (`classify_vec_element_release`) partitions every
//! element type into the plain, owned, closure-pair, and fail-closed buckets;
//! the plain/owned binding predicates, the compile-time reject of unwired or
//! unseen elements, and the definition-site `DropRecipe` all project from it.
use super::{
    describe_vec_element, ty_is_closure_pair, ty_is_indirect_enum, BindingId, Builder, Disposition,
    FailClosedReason, MirDiagnostic, MirDiagnosticKind, ResolvedTy, SiteId, ValueClass,
    VecElementRelease,
};

impl Builder {
    /// Classify a `Vec<E>` element's scope-exit release by reading the single
    /// heap-ownership authority. The three release-bucket predicates are
    /// projections of this one decision (see [`VecElementRelease`]), so their
    /// union is total over `ResolvedTy` by construction — a `Vec<E>` local can
    /// never silently fall through every bucket and skip its release.
    ///
    /// Order matters and mirrors codegen's `resolved_ty_cow_heap_release`: a closure
    /// pair is checked BEFORE the owned/plain arms (a `fn`/closure element is
    /// neither an owned composite nor a plain leaf — it has its own pair-box
    /// release), and the owned composite is checked before the plain leaf (an
    /// all-`BitCopy` aggregate is plain; a heap-owning one is owned). The arms
    /// are disjoint, so the order only fixes the (unreachable) tie.
    pub(crate) fn classify_vec_element_release(&self, elem: &ResolvedTy) -> VecElementRelease {
        if ty_is_closure_pair(elem) {
            return VecElementRelease::ClosurePair;
        }

        if self.is_owned_vec_element(elem) {
            return VecElementRelease::OwnedElement;
        }
        if self.is_plain_vec_element(elem) {
            return VecElementRelease::Plain;
        }
        // Unclaimed by the enumerated buckets. `bytes` (a fat `{ ptr, len,
        // cap }` triple), bare runtime handles, and indirect-enum nodes own
        // heap with no wired Vec-element release: fail closed with the typed
        // reason rather than a buffer-only free over leaking element nodes.
        // The indirect-enum probe is explicit because the heap-ownership
        // authority is blind to indirection — a scalar-payload `indirect enum`
        // owns a heap node the authority reports as non-owning.
        if self.named_elem_carries_drop_obligation(elem)
            || ty_is_indirect_enum(elem, &self.enum_layouts)
        {
            return VecElementRelease::Unsupported(FailClosedReason::NoReleaseProtocol);
        }
        // The authority answers "no obligation" for a `Named` head it has no
        // layout for exactly as it does for a heap-free one. That answer is
        // only evidence when every head in the element is registered; an
        // unregistered head is unknowable and fails closed (no recipe, no
        // release symbol, rejected at compile) instead of being freed
        // buffer-only over element heap the authority never saw.
        if self.ty_has_unregistered_named(elem) {
            return VecElementRelease::Unsupported(FailClosedReason::UnknownValueClass);
        }
        // Every layout in the element is visible to the heap-ownership
        // authority and it reports no drop obligation (a heap-free registered
        // record/enum instance, a bare runtime handle such as `LocalPid`,
        // `Unit`): the buffer-only free IS the complete release. A free
        // `TypeParam` lands here too — a generic origin skeleton's `Vec<T>`
        // still owns its outer buffer, and that buffer release is the
        // skeleton's decided cleanup (`generic_vec_definition_publishes_outer_buffer_recipe`).
        VecElementRelease::Plain
    }

    /// Whether `ty` contains a user `Named` head the layout registries cannot
    /// see: no record layout, no enum/machine layout, not a builtin, not an
    /// opaque handle, not a registered lifecycle resource. Walks record fields,
    /// enum payloads, tuple elements, and the type arguments of user heads;
    /// cycle-guarded on the mangled head like the heap-ownership authority.
    pub(crate) fn ty_has_unregistered_named(&self, ty: &ResolvedTy) -> bool {
        let layouts = crate::model::MirHeapLayouts {
            record_field_orders: &self.record_field_orders,
            enum_layouts: &self.enum_layouts,
        };
        let mut visiting = std::collections::HashSet::new();
        self.unregistered_named_walk(ty, &layouts, &mut visiting)
    }

    fn unregistered_named_walk<L: crate::model::HeapOwnershipLayouts>(
        &self,
        ty: &ResolvedTy,
        layouts: &L,
        visiting: &mut std::collections::HashSet<String>,
    ) -> bool {
        match ty {
            ResolvedTy::Named {
                builtin: Some(_), ..
            }
            | ResolvedTy::Named {
                is_opaque: true, ..
            } => false,
            ResolvedTy::Named {
                name,
                args,
                builtin,
                ..
            } => {
                if !visiting.insert(hew_hir::mangle_resolved_ty(ty)) {
                    return false;
                }
                if args
                    .iter()
                    .any(|arg| self.unregistered_named_walk(arg, layouts, visiting))
                {
                    return true;
                }
                if let Some(fields) = layouts.record_field_tys(name, args, *builtin) {
                    return fields
                        .iter()
                        .any(|field| self.unregistered_named_walk(field, layouts, visiting));
                }
                if let Some(variants) = layouts.enum_variant_field_tys(name, args) {
                    return variants
                        .iter()
                        .flatten()
                        .any(|field| self.unregistered_named_walk(field, layouts, visiting));
                }
                self.type_classes
                    .lifecycle_registry()
                    .opaque_resource_for_ty(ty)
                    .is_none()
            }
            ResolvedTy::Tuple(elems) => elems
                .iter()
                .any(|elem| self.unregistered_named_walk(elem, layouts, visiting)),
            _ => false,
        }
    }

    /// Walk an owned local's type for a `Vec<E>` whose element has no wired
    /// per-element release — `classify_vec_element_release(E)` is
    /// `Unsupported(NoReleaseProtocol)` (a `bytes` fat triple or an indirect-enum
    /// node). Returns a human description of the FIRST such element, found
    /// directly (a `Vec<E>` local) or transitively through a record field, a
    /// tuple element, a nested `Vec`, a type argument, or an enum variant
    /// payload.
    ///
    /// This is the PRODUCTION consumer of the typed `Unsupported` disposition.
    /// `ty_owns_heap(Vec<_>)` is unconditionally `true`, but a `Vec<E>` no
    /// release bucket claims falls through every scope-exit drop set and silently
    /// leaks its element nodes (the admit-then-leak non-totality). Surfacing it
    /// here turns that runtime leak into a fatal, actionable compile diagnostic —
    /// the fail-closed direction (reject at compile, where the author can act,
    /// over a silent runtime leak).
    ///
    /// `NoReleaseProtocol` and `UnknownValueClass` (an element whose `Named`
    /// head no layout registry can see) are rejected. An element the authority
    /// proves heap-free classifies `Plain` and is never rejected, and neither is
    /// an un-monomorphised generic `Vec<T>` (its outer buffer release is the
    /// skeleton's cleanup).
    ///
    /// And only an element unwired in EVERY context is rejected: a heap-owning
    /// record/enum releasable through the owned-element ABI
    /// (`elem_is_owned_abi_releasable`) is excluded, because it reaches
    /// `Unsupported(NoReleaseProtocol)` here only when its `Vec` is constructed
    /// in another function (so its key was not harvested into THIS function's
    /// allow-list), not because its release is unwired — without that exclusion a
    /// nested `Vec<owned-record>` field (e.g. the `Vec<Stack<i64>>` buffer inside
    /// `Stack<Stack<i64>>`) would false-positive as a leak.
    ///
    /// Cycle-guarded on `Named` recursion (an `indirect enum` references itself)
    /// exactly as the heap-ownership authority `ty_owns_heap_inner`.
    pub(crate) fn unsupported_vec_element_in_ty(&self, ty: &ResolvedTy) -> Option<String> {
        let layouts = crate::model::MirHeapLayouts {
            record_field_orders: &self.record_field_orders,
            enum_layouts: &self.enum_layouts,
        };
        let mut visiting = std::collections::HashSet::new();
        self.unsupported_vec_element_walk(ty, &layouts, &mut visiting)
    }

    fn unsupported_vec_element_walk<L: crate::model::HeapOwnershipLayouts>(
        &self,
        ty: &ResolvedTy,
        layouts: &L,
        visiting: &mut std::collections::HashSet<String>,
    ) -> Option<String> {
        match ty {
            // The element this consolidation classifies. A `NoReleaseProtocol`
            // element is the unwired-leak case to reject; otherwise descend into
            // `E` regardless, so a `Vec<Vec<indirect_enum>>` (whose outer `Vec`
            // is `OwnedElement`, not `Unsupported`) is still caught at the inner
            // `Vec`.
            ResolvedTy::Named {
                args,
                builtin: Some(hew_types::BuiltinType::Vec),
                ..
            } => {
                let elem = args.first()?;
                // Reject only a Vec element whose per-element release is unwired
                // in EVERY context. `classify_vec_element_release` returns
                // `Unsupported(NoReleaseProtocol)` both for a genuinely-unwired
                // element (a `bytes` fat triple, an indirect-enum node — scalar
                // OR heap payload) AND for a heap-owning record/enum that simply
                // was not harvested into THIS function's owned-element allow-list
                // — its `Vec` is constructed, and released through the
                // owned-element ABI, in another function (`vec_owned_element_keys`
                // is harvested per function). `elem_is_owned_abi_releasable`
                // excludes only the latter (and excludes every indirect enum, so
                // an indirect-enum element is never suppressed from the reject),
                // keeping the reject from false-positiving on a nested
                // `Vec<owned-record>` field (e.g. the `Vec<Stack<i64>>` buffer
                // inside `Stack<Stack<i64>>`) while still rejecting the
                // genuinely-unwired `Vec<bytes>` / `Vec<indirect_enum>`. The
                // descent below still visits `E`, so a
                // `Vec<RecordHoldingVecIndirectEnum>` is caught at the inner
                // `Vec<indirect_enum>`.
                match self.classify_vec_element_release(elem) {
                    VecElementRelease::Unsupported(FailClosedReason::NoReleaseProtocol)
                        if !self.elem_is_owned_abi_releasable(elem) =>
                    {
                        return Some(describe_vec_element(elem, &self.enum_layouts));
                    }
                    VecElementRelease::Unsupported(FailClosedReason::UnknownValueClass) => {
                        return Some(format!(
                            "{} (its layout is not registered)",
                            describe_vec_element(elem, &self.enum_layouts)
                        ));
                    }
                    _ => {}
                }
                self.unsupported_vec_element_walk(elem, layouts, visiting)
            }
            ResolvedTy::Named {
                name,
                args,
                builtin,
                ..
            } => {
                // Type arguments first (`Option<Vec<indirect_enum>>`,
                // `HashMap<K, Vec<…>>`), then record fields, then enum variant
                // payloads — cycle-guarded on the `Named` head so a recursive
                // `indirect enum` does not loop.
                for arg in args {
                    if let Some(found) = self.unsupported_vec_element_walk(arg, layouts, visiting) {
                        return Some(found);
                    }
                }
                let key = hew_hir::mangle_resolved_ty(ty);
                if !visiting.insert(key.clone()) {
                    return None;
                }
                let found = layouts
                    .record_field_tys(name, args, *builtin)
                    .into_iter()
                    .flatten()
                    .find_map(|field_ty| {
                        self.unsupported_vec_element_walk(&field_ty, layouts, visiting)
                    })
                    .or_else(|| {
                        layouts
                            .enum_variant_field_tys(name, args)
                            .into_iter()
                            .flatten()
                            .flatten()
                            .find_map(|payload_ty| {
                                self.unsupported_vec_element_walk(&payload_ty, layouts, visiting)
                            })
                    });
                visiting.remove(&key);
                found
            }
            ResolvedTy::Tuple(elems) => elems
                .iter()
                .find_map(|elem| self.unsupported_vec_element_walk(elem, layouts, visiting)),
            ResolvedTy::Array(inner, _) | ResolvedTy::Slice(inner) => {
                self.unsupported_vec_element_walk(inner, layouts, visiting)
            }
            _ => None,
        }
    }

    /// Fatal compile diagnostics for every owned local whose type holds a
    /// `Vec<E>` with no wired per-element release (see
    /// [`Builder::unsupported_vec_element_in_ty`]). Emitted before codegen so an
    /// admit-then-leak `Vec<bytes>` / `Vec<indirect_enum>` is REJECTED at compile
    /// (where the author can act) rather than constructed and silently leaked at
    /// scope exit — the leak-safe fail-closed direction. The typed
    /// `VecElementRelease::Unsupported(NoReleaseProtocol)` disposition this
    /// consumes is the same single authority the release buckets project from.
    ///
    /// `bind_sites` maps each binding to its construction `SiteId` (harvested
    /// from the finalized `Bind` statements in the function's blocks, since the
    /// builder's transient `statements` buffer is already drained into blocks by
    /// the time diagnostics assemble), so the error points at the real
    /// construction site rather than a synthetic fallback.
    pub(crate) fn unsupported_vec_element_diagnostics(
        &self,
        bind_sites: &std::collections::HashMap<BindingId, SiteId>,
    ) -> Vec<MirDiagnostic> {
        self.owned_locals
            .iter()
            .filter(|entry| entry.disposition == Disposition::ScopeExit)
            .filter_map(|entry| {
                let name = &entry.name;
                let elem = self.unsupported_vec_element_in_ty(&entry.ty)?;
                let site = bind_sites.get(&entry.binding).copied().unwrap_or(SiteId(0));
                Some(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!(
                            "`{name}`: a `Vec` whose element is {elem} has no per-element \
                             release protocol, so its heap nodes would leak at scope exit"
                        ),
                        site,
                    },
                    note: "a `Vec` of `bytes` or of an indirect-enum element cannot yet be \
                           released element-by-element at scope exit. This construction is \
                           rejected at compile rather than silently leaked, and becomes \
                           available once the per-element release is wired."
                        .to_string(),
                })
            })
            .collect()
    }

    /// True when `elem` is a PLAIN `Vec` element — one released by the
    /// buffer-only `hew_vec_free` (with the runtime's own `ElemKind` walk for
    /// `string`/layout elements), carrying NO owned-descriptor or closure-pair
    /// release. Covers: a `BitCopy` scalar or `string`; a `BitCopy` value record
    /// (and scalar builtins that reach the classifier as `Named`, e.g.
    /// `Instant`); a DIRECT (non-indirect) user enum that owns no heap; and an
    /// all-plain tuple.
    ///
    /// The `Named` arm reads the heap-ownership AUTHORITY, not `ValueClass`
    /// alone. `ValueClass::of_ty` finalises records only, so a fieldless /
    /// scalar-payload user enum is NEVER `BitCopy`; gating solely on
    /// `ValueClass::of_ty == BitCopy` (the pre-fix form) classified such an enum
    /// NEITHER plain nor owned, leaving its `Vec` with no scope-exit release — a
    /// whole-buffer+handle leak. The arm therefore also admits a direct enum via
    /// `ty_is_direct_enum_element(elem) && !named_elem_carries_drop_obligation(elem)`: a
    /// heap-free direct enum is plain, a heap-owning one still routes owned. The
    /// `BitCopy` disjunct is retained for the shapes `ValueClass` classifies
    /// correctly (records, `Instant`), where a pure heap-authority gate would
    /// wrongly exclude a `BitCopy` builtin not in the layout registry.
    ///
    /// The direct-enum membership is `ty_is_direct_enum_element`, extracted
    /// verbatim from the layout-Vec constructor authority
    /// (`vec_element_uses_layout_descriptor`), so a direct enum is RELEASED as a
    /// plain layout Vec exactly when it was CONSTRUCTED as one — the
    /// construct/release symmetry the runtime relies on, and congruent with the
    /// vec-index getter's non-indirect-enum `hew_vec_get_layout` arm.
    ///
    /// This is the element-level core of [`Builder::binding_ty_is_plain_vec`],
    /// factored so [`Builder::classify_vec_element_release`] shares the exact
    /// same plain-element authority — one body, no second copy to drift. The
    /// `Tuple` arm delegates to `tuple_is_all_bitcopy` (the `Tuple` value class
    /// is unconditionally `CowValue`, so it cannot discriminate field types).
    pub(super) fn is_plain_vec_element(&self, elem: &ResolvedTy) -> bool {
        if matches!(
            elem,
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
                | ResolvedTy::Duration
                | ResolvedTy::String
        ) {
            return true;
        }
        // A `Named` element is plain when it is BitCopy (records, and scalar
        // builtins that reach the classifier as `Named` such as `Instant`), OR
        // when it is a DIRECT user enum that owns no heap. The enum disjunct is
        // load-bearing: `ValueClass::of_ty` finalises records only, so a
        // fieldless / scalar-payload user enum is NEVER `BitCopy` and would
        // otherwise be classified NEITHER plain nor owned — leaving its Vec with
        // no scope-exit release (a buffer+handle leak). Heap-ness is read from
        // the `named_elem_carries_drop_obligation` authority, never re-derived from BitCopy, so
        // a heap-owning enum still routes owned. `ty_is_direct_enum_element` is
        // the layout-Vec constructor's own membership, so release matches
        // construction; it is congruent with the vec-index getter's non-indirect
        // enum `hew_vec_get_layout` arm (`dedup-semantic-boundary`).
        (matches!(elem, ResolvedTy::Named { .. })
            && (ValueClass::of_ty(elem, &self.type_classes) == ValueClass::BitCopy
                || (self.ty_is_direct_enum_element(elem)
                    && !self.named_elem_carries_drop_obligation(elem))))
            || (matches!(elem, ResolvedTy::Tuple(_)) && self.tuple_is_all_bitcopy(elem))
    }

    /// True when `ty` is a `Vec<T>` whose element `T` is an owned-Vec element
    /// (record/enum in `vec_owned_element_keys`, or a heap-owning tuple). A
    /// projection of [`Builder::classify_vec_element_release`] — equal to
    /// `is_owned_vec_element(elem)` (a closure-pair element gives
    /// `is_owned_vec_element == false`, so routing through the typed decision is
    /// behaviour-identical) — so the scope-exit `hew_vec_free_owned` drop fires
    /// for exactly the Vecs that were constructed through the owned ABI
    /// (`dedup-semantic-boundary`).
    pub(crate) fn binding_ty_is_owned_element_vec(&self, ty: &ResolvedTy) -> bool {
        let ResolvedTy::Named {
            args,
            builtin: Some(hew_types::BuiltinType::Vec),
            ..
        } = ty
        else {
            return false;
        };
        args.first()
            .is_some_and(|elem| self.classify_vec_element_release(elem).is_owned_element())
    }

    /// True when `ty` is the builtin `Vec<T>` whose element `T` is a PLAIN
    /// element — a `BitCopy` scalar (`i64`, `u8`, `bool`, `f64`, `char`,
    /// `Duration`, …), `string` (whose element release lives inside the
    /// runtime's `ElemKind::String` walk), a `BitCopy` value record (e.g.
    /// `type Point { x: i64, y: i64 }`), a DIRECT (non-indirect) user enum that
    /// owns no heap, or a tuple whose fields are all plain. These are exactly
    /// the Vecs codegen constructs WITHOUT an owned-element descriptor — the
    /// scalar/string ABIs and the inline value-aggregate
    /// `hew_vec_new_with_layout` / `hew_vec_get_layout` path — so the matching
    /// scope-exit release is the plain `hew_vec_free` (buffer + handle; the
    /// runtime walks string elements itself, and a heap-free value-aggregate
    /// element owns no heap so no per-element drop is needed). Substitutes
    /// through the monomorphisation map first (mirroring
    /// `vec_receiver_has_owned_element`) so a polymorphic binding type resolves
    /// to its concrete element.
    ///
    /// Default-deny: ONLY the positively enumerated plain element shapes admit.
    /// The plain arm is the precise complement of the owned arm: an owned-element
    /// Vec (record/enum/tuple with a string/bytes/nested-collection field) is
    /// never admitted here and continues to route to its dedicated
    /// `hew_vec_free_owned` release; a closure-pair `Vec<fn>` is also excluded
    /// and routes to descriptor-driven `hew_vec_free_owned`.
    ///
    /// For named records the `ValueClass::of_ty(Named{..}) == BitCopy` check is
    /// the discriminant. For direct user ENUMS it is NOT — `ValueClass` finalises
    /// records only, so a fieldless / scalar-payload enum is never `BitCopy`;
    /// `is_plain_vec_element` admits those via the `named_elem_carries_drop_obligation`
    /// authority instead (see its doc). For tuples `ValueClass` cannot be used:
    /// `ValueClass::of_ty(Tuple(_))` ALWAYS returns `CowValue` regardless of
    /// field types, so the tuple path delegates to `tuple_is_all_bitcopy`, which
    /// recurses structurally. Using `!is_owned_vec_element` as the complement is
    /// unsound here because its backing `ty_contains_heap_owning` omits
    /// `record_field_resolved_tys` and can mis-classify a named record inside a
    /// tuple as non-heap-owning.
    pub(crate) fn binding_ty_is_plain_vec(&self, ty: &ResolvedTy) -> bool {
        let ResolvedTy::Named {
            args,
            builtin: Some(hew_types::BuiltinType::Vec),
            ..
        } = self.subst_ty(ty)
        else {
            return false;
        };
        // A projection of the typed `classify_vec_element_release` decision
        // (no second copy to drift). The doc above this function records why
        // the `Named` arm reads the `named_elem_carries_drop_obligation`
        // authority (not `ValueClass` alone) and the `Tuple` arm uses
        // `tuple_is_all_bitcopy`.
        args.first()
            .is_some_and(|elem| self.classify_vec_element_release(elem).is_plain())
    }

    /// True when `ty` is a `Tuple` whose every element is plain-releasable by
    /// structural recursion (a plain tuple field carries no per-element heap
    /// drop, so the tuple Vec releases via the buffer-only `hew_vec_free`).
    ///
    /// A tuple element is plain if it is:
    /// - a `BitCopy` scalar (same allow-list as the scalar arm of
    ///   `binding_ty_is_plain_vec`),
    /// - a `Named` type that is either `ValueClass::of_ty == BitCopy` (the
    ///   authority for records — a heap-owning record is never `BitCopy`) OR a
    ///   DIRECT user enum that owns no heap. The enum disjunct is load-bearing:
    ///   `ValueClass` finalises records only, so a fieldless / scalar-payload
    ///   enum is never `BitCopy` and must be admitted through the
    ///   `named_elem_carries_drop_obligation` authority, exactly as `is_plain_vec_element`
    ///   does — or a `(Colour, i64)` tuple element would leak, or
    /// - a nested `Tuple` that also satisfies this predicate recursively.
    ///
    /// This is the correct complement of the owned authority for tuples.
    /// Using `!is_owned_vec_element(Tuple)` is UNSOUND: `ty_contains_heap_owning`
    /// (its backing check) only consults `enum_layouts` for `Named` types and
    /// misses `record_field_resolved_tys`, so a named record nested inside a
    /// tuple that transitively owns a `string` field can be mis-classified as
    /// non-heap-owning, silently admitting a `Vec<(Rec, i64)>` (where `Rec` has
    /// a `string` field) to the plain-Vec path and emitting `hew_vec_free`
    /// where `hew_vec_free_owned` is required. The `BitCopy`-plus-`named_elem_
    /// owns_heap` discriminant is complete for `Named` fields and agrees with
    /// codegen's `resolved_ty_contains_heap_leaf` (`dedup-semantic-boundary`).
    fn tuple_is_all_bitcopy(&self, ty: &ResolvedTy) -> bool {
        let ResolvedTy::Tuple(elems) = ty else {
            return false;
        };
        elems.iter().all(|e| match e {
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
            | ResolvedTy::Duration => true,
            ResolvedTy::Named { .. } => {
                // A tuple field is BitCopy-compatible when it is itself BitCopy
                // (records / scalar builtins), OR a direct user enum owning no
                // heap. The enum disjunct mirrors `is_plain_vec_element`: user
                // enums are never `ValueClass::BitCopy`, so without it a
                // `(Colour, i64)` tuple element would leak. Heap-ness comes from
                // the `named_elem_carries_drop_obligation` authority.
                ValueClass::of_ty(e, &self.type_classes) == ValueClass::BitCopy
                    || (self.ty_is_direct_enum_element(e)
                        && !self.named_elem_carries_drop_obligation(e))
            }
            ResolvedTy::Tuple(_) => self.tuple_is_all_bitcopy(e),
            // Exhaustive (no `_ => false` fall-through): a new `ResolvedTy`
            // variant is a compile error here, never a silent "is BitCopy"
            // miss. None of the remaining shapes is a `BitCopy` tuple field —
            // `String`/`Bytes`/`CancellationToken` own heap; `Function`/`Closure`
            // are fat closure pairs; `Array`/`Slice`/`Pointer`/`Borrow`/
            // `TraitObject`/`Task` are not value-aggregate-copyable here; `Unit`/
            // `Never`/`TypeParam` carry no proven `BitCopy` layout — so a tuple
            // containing any of them is NOT all-`BitCopy` and routes off the
            // plain bucket.
            ResolvedTy::String
            | ResolvedTy::Bytes
            | ResolvedTy::CancellationToken
            | ResolvedTy::Unit
            | ResolvedTy::Never
            | ResolvedTy::Array(_, _)
            | ResolvedTy::Slice(_)
            | ResolvedTy::Function { .. }
            | ResolvedTy::Closure { .. }
            | ResolvedTy::Pointer { .. }
            | ResolvedTy::Borrow { .. }
            | ResolvedTy::TraitObject { .. }
            | ResolvedTy::Task(_)
            | ResolvedTy::TypeParam { .. } => false,
        })
    }
}
