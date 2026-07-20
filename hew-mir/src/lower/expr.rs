#[cfg(test)]
use super::drop_plan::ty_is_closure_pair_vec;
use super::{
    builtin_method_arg_is_move_ingress, classify_closure_pair_rhs, classify_dyn_trait_storage,
    cmp_select_by_signedness, context_reader_offset, describe_vec_element,
    dyn_rebind_source_binding, field_override_uses_record_field_drop, float_width,
    integer_bit_width, integer_signedness, is_self_expr, is_string_const_ty, machine_emit_type_id,
    mangle_layout_key, mangle_machine_step, monomorphic_user_record_key, numeric_method_op,
    numeric_method_signedness, option_payload_ty, runtime_symbol_for_call_expr, short_name,
    signed_min_value, ty_is_closure_pair, ty_is_generator_handle, ty_is_indirect_enum,
    ty_is_local_collection_handle, ty_is_stream_handle, ty_is_vec, unary_op_label,
    unresolved_fn_sig_reason, user_record_layout_key, vec_iter_let_cursor_owns_handle,
    vec_iter_ty_drop_safe, ActorStateLoadMode, BinaryOp, BindingId, Builder, BuiltinType,
    ChildKind, ClosurePairRhs, CmpPred, Disposition, FailClosedReason, FieldOffset, FloatWidth,
    HashMap, HashSet, HirExpr, HirExprKind, HirLiteral, HirStmtKind, HirVarSelfMethodTarget, Instr,
    IntArithOp, IntSignedness, IntentKind, MirDiagnostic, MirDiagnosticKind, MirStatement,
    NumericMethodFamily, Place, ProjectedPayloadOrigin, ProjectedPayloadRejectReason,
    ReleaseSymbolVerdict, ResolvedRef, ResolvedTy, RuntimeCallContext, SiteId, SuspendKind,
    Terminator, TrapKind, UnaryOp, ValueClass, VecElementRelease, FOR_ITER_CURSOR_NAME_PREFIX,
    SENTINEL_RECV_GEN_COMPANION_BINDING, SYNTHETIC_TEMP_ARG_NAME,
};
#[cfg(test)]
use super::{FieldLoadClass, PlaceProvenance, Projection, ValueProvenance};

pub(super) fn binding_seeds_drop_elaboration(
    ty: &ResolvedTy,
    type_classes: &hew_hir::TypeClassTable,
) -> bool {
    ValueClass::of_ty(ty, type_classes) != ValueClass::BitCopy
}

impl Builder {
    /// True when a `Vec` element is released through the owned-element ABI
    /// (`hew_vec_free_owned` running the per-element `drop_fn`, #1722): a
    /// registered, genuinely heap-owning, non-closure record or enum. This is
    /// the exact acceptance `harvest_vec_owned_element_key` records into
    /// `vec_owned_element_keys` in the function that CONSTRUCTS the `Vec`,
    /// factored so the compile reject (`unsupported_vec_element_walk`) can ask
    /// the same question harvest-independently.
    ///
    /// An element satisfying this is releasable wherever its `Vec` is built, so
    /// it must NOT be rejected as unwired when it is merely observed as a nested
    /// field HERE — its key was harvested in the constructing function, not this
    /// one. Without this guard a nested `Vec<owned-record>` (e.g. the
    /// `Vec<Stack<i64>>` buffer inside `Stack<Stack<i64>>`) would false-positive
    /// as unwired, since `vec_owned_element_keys` is harvested per function.
    ///
    /// Returns `false` for the genuinely-unwired Vec elements, which therefore
    /// stay on the reject path: a `bytes` fat triple and a bare runtime handle
    /// are not registered record/enum types; an all-BitCopy record/enum is
    /// `Copy` and owns no heap; and EVERY `indirect enum` — scalar OR heap
    /// payload — is excluded up front. An indirect-enum `Vec` rides the plain
    /// pointer ABI (`hew_vec_new_ptr`: each slot is a `ptr` to a heap-boxed
    /// tagged-union node), while the owned-element release
    /// (`hew_vec_free_owned` running a per-element `drop_fn`) has no
    /// indirect-aware node free wired — admitting it here would route
    /// construction and release through mismatched ABIs. A scalar-payload
    /// indirect enum also has `named_elem_owns_heap == false` (the
    /// heap-ownership authority is indirection-blind), but a HEAP-payload one
    /// (`A(string)`) has `named_elem_owns_heap == true` and would otherwise fall
    /// through the heap-owning-enum path below; the explicit `ty_is_indirect_enum`
    /// guard is what keeps that case on the fail-closed
    /// `Unsupported(NoReleaseProtocol)` reject rather than the owned-ABI path.
    /// Mirrors codegen's `owned_elem_thunk_key` resolution so harvest, reject,
    /// getter, and free agree (`dedup-semantic-boundary`).
    pub(crate) fn elem_is_owned_abi_releasable(&self, elem: &ResolvedTy) -> bool {
        // An `indirect enum` element is NEVER owned-ABI releasable, regardless
        // of payload. Its `Vec` is built through the plain pointer ABI while the
        // owned-element per-element node free is unwired (the deferred
        // indirect-aware release phase), so it must stay on the fail-closed
        // `Unsupported(NoReleaseProtocol)` reject — not be excluded from it as if
        // the owned ABI claimed it. The scalar-payload case would already return
        // `false` at the `named_elem_owns_heap` check below; this guard also
        // catches the HEAP-payload case (`indirect enum Foo { A(string); B }`),
        // whose payload owns heap and would otherwise reach `true` and suppress
        // the reject, leaving a construct/release ABI mismatch to reach codegen.
        if ty_is_indirect_enum(elem, &self.enum_layouts) {
            return false;
        }
        let ResolvedTy::Named {
            name: elem_name,
            args: elem_args,
            ..
        } = elem
        else {
            return false;
        };
        // A registered, non-BitCopy record/enum element. BitCopy records stay on
        // the existing `_layout` path and never enter the owned allow-list.
        let key = if elem_args.is_empty() {
            elem_name.clone()
        } else {
            mangle_layout_key(elem_name, elem_args)
        };
        let is_enum = self
            .enum_layouts
            .iter()
            .any(|el| el.name == key || short_name(&el.name) == short_name(elem_name));
        let is_record = self.lookup_record_field_order(&key).is_some()
            || self.lookup_record_field_order(elem_name.as_str()).is_some();
        if !is_enum && !is_record {
            return false;
        }
        // ONLY a genuinely heap-owning element is an owned-Vec element, decided
        // by the `named_elem_owns_heap` authority (NOT by `ValueClass::BitCopy`,
        // which finalises records only). A heap-free record (e.g.
        // `type Point { x: i64; y: i64 }`, which is `BitCopy`) OR a heap-free
        // direct enum (e.g. `enum Colour { Red; Green; Blue }`, which is NOT
        // `BitCopy`) owns no heap and stays on the plain-Vec path
        // (`is_plain_vec_element`); harvesting it would mis-route its Vec's
        // element loads through the owned getter (which reads an owned
        // descriptor the plain Vec never carries). Check field/variant
        // heap-ownership via the record/enum registries.
        if !self.named_elem_owns_heap(elem) {
            return false;
        }
        // A closure-bearing record/enum element is NOT owned-ABI releasable: the
        // owned-Vec descriptor deep-clones elements on push/set through the
        // record clone thunk, and a closure pair's clone direction is refused
        // (sole-owner env, no retain). Excluding it keeps `Vec<Holder-with-fn>`
        // on the fail-closed unsupported path at compile time instead of
        // refusing at runtime on the first push.
        !crate::model::ty_contains_closure_value(
            elem,
            &self.record_layouts_for_classification(),
            &self.enum_layouts,
        )
    }

    /// Harvest the record/enum layout key of an owned-Vec element type into
    /// `vec_owned_element_keys`. `ty` is any type observed in the function; only
    /// a `Vec<elem>` whose `elem` is owned-element-ABI releasable (the same
    /// acceptance `elem_is_owned_abi_releasable` and the compile reject consult,
    /// i.e. a registered, non-BitCopy record/enum codegen synthesizes
    /// `__hew_*_inplace` thunks for) contributes a key. Mirrors codegen's
    /// `owned_elem_thunk_key` resolution so the MIR value-class allow-list and
    /// the codegen descriptor/seeding agree on which types are owned-Vec
    /// elements (`dedup-semantic-boundary`).
    pub(crate) fn harvest_vec_owned_element_key(&mut self, ty: &ResolvedTy) {
        let ResolvedTy::Named { name, args, .. } = ty else {
            return;
        };
        if name != "Vec" || args.len() != 1 {
            return;
        }
        // Only an owned-element-ABI-releasable element contributes a key — the
        // same acceptance the compile reject consults, so harvest and reject
        // agree on which elements the owned ABI claims (`dedup-semantic-boundary`).
        if !self.elem_is_owned_abi_releasable(&args[0]) {
            return;
        }
        let ResolvedTy::Named {
            name: elem_name,
            args: elem_args,
            ..
        } = &args[0]
        else {
            return;
        };
        let key = if elem_args.is_empty() {
            elem_name.clone()
        } else {
            mangle_layout_key(elem_name, elem_args)
        };
        let is_enum = self
            .enum_layouts
            .iter()
            .any(|el| el.name == key || short_name(&el.name) == short_name(elem_name));
        if is_enum {
            // Enums are gated by their own EnumInPlace drop path; record the
            // mangled enum key so a value of the enum admits as CowValue too.
            self.vec_owned_element_keys.insert(key);
        } else {
            // Use the record-layout-key form for records (matches
            // `user_record_layout_key` consulted by the W3.029 escape hatch).
            let record_key = if self.lookup_record_field_order(&key).is_some() {
                key
            } else {
                elem_name.clone()
            };
            self.vec_owned_element_keys.insert(record_key);
        }
    }

    /// True when a `ResolvedTy` transitively owns heap. Thin adapter over the
    /// single `crate::model::ty_owns_heap` authority (record fields via
    /// `record_field_orders`, enum/machine variant payloads via `enum_layouts`,
    /// one builtin leaf set). The owned-Vec element harvest consults this so an
    /// all-BitCopy record/enum (which is `Copy` and uses the `BitCopy` `_layout`
    /// path) is NOT treated as an owned-Vec element, while a
    /// `CancellationToken`/`Generator`-bearing element correctly is — the same
    /// verdict the codegen owned-Vec walker now reaches, so getter, constructor,
    /// and release agree (`dedup-semantic-boundary`).
    fn named_elem_owns_heap(&self, ty: &ResolvedTy) -> bool {
        crate::model::ty_owns_heap_mir(ty, &self.record_field_orders, &self.enum_layouts)
    }

    pub(crate) fn fieldless_enum_layout_key(&self, ty: &ResolvedTy) -> Option<String> {
        let ResolvedTy::Named { name, args, .. } = ty else {
            return None;
        };
        crate::model::find_enum_layout(name, args, &self.enum_layouts)
            .filter(|layout| {
                layout
                    .variants
                    .iter()
                    .all(|variant| variant.field_tys.is_empty())
            })
            .map(|layout| layout.name.clone())
    }

    fn is_fieldless_enum_comparison(&self, lhs_ty: &ResolvedTy, rhs_ty: &ResolvedTy) -> bool {
        let Some(lhs_key) = self.fieldless_enum_layout_key(lhs_ty) else {
            return false;
        };
        let Some(rhs_key) = self.fieldless_enum_layout_key(rhs_ty) else {
            return false;
        };
        lhs_key == rhs_key
    }

    fn record_layout_key_for_eq(&self, ty: &ResolvedTy) -> Option<String> {
        let ResolvedTy::Named { name, args, .. } = ty else {
            return None;
        };
        let short = short_name(name);
        let key = if args.is_empty() {
            name.clone()
        } else {
            mangle_layout_key(short, args)
        };
        self.lookup_record_field_order(&key)
            .or_else(|| self.lookup_record_field_order(name))
            .map(|_| key)
    }

    fn payload_enum_layout_key_for_eq(&self, ty: &ResolvedTy) -> Option<String> {
        let ResolvedTy::Named { name, args, .. } = ty else {
            return None;
        };
        let short = short_name(name);
        let key = if args.is_empty() {
            name.clone()
        } else {
            mangle_layout_key(short, args)
        };
        self.enum_layouts
            .iter()
            .find(|layout| {
                layout.name == key || layout.name == *name || short_name(&layout.name) == short
            })
            .filter(|layout| {
                layout
                    .variants
                    .iter()
                    .any(|variant| !variant.field_tys.is_empty())
            })
            .map(|layout| layout.name.clone())
    }

    /// Resolve the monomorphised tagged-union layout key for an enum `clone`
    /// site, covering BOTH fieldless and payload-carrying enums (clone applies
    /// to every enum kind, unlike the eq helpers which split the two). Mirrors
    /// `payload_enum_layout_key_for_eq`'s key resolution but without the
    /// payload filter: a generic instantiation (`Maybe<i64>`) resolves the
    /// mangled `Maybe$$i64`, a monomorphic enum keeps its bare declared name.
    /// `None` when `ty` is not a registered enum — the caller then falls
    /// through to the record path (the two layout registries are disjoint, so a
    /// `Some` here is authoritative).
    fn enum_clone_layout_key(&self, ty: &ResolvedTy) -> Option<String> {
        let ResolvedTy::Named { name, args, .. } = ty else {
            return None;
        };
        let short = short_name(name);
        let key = if args.is_empty() {
            name.clone()
        } else {
            mangle_layout_key(short, args)
        };
        self.enum_layouts
            .iter()
            .find(|layout| {
                layout.name == key || layout.name == *name || short_name(&layout.name) == short
            })
            .map(|layout| layout.name.clone())
    }

    fn is_structural_eq_comparison(&self, lhs_ty: &ResolvedTy, rhs_ty: &ResolvedTy) -> bool {
        if let (Some(lhs_key), Some(rhs_key)) = (
            self.record_layout_key_for_eq(lhs_ty),
            self.record_layout_key_for_eq(rhs_ty),
        ) {
            return lhs_key == rhs_key;
        }
        if let (Some(lhs_key), Some(rhs_key)) = (
            self.payload_enum_layout_key_for_eq(lhs_ty),
            self.payload_enum_layout_key_for_eq(rhs_ty),
        ) {
            return lhs_key == rhs_key;
        }
        false
    }

    /// True when `elem_ty` is an owned (non-Copy) Vec element that was
    /// constructed through the owned descriptor and must route element loads
    /// through `hew_vec_get_owned`. Two owned shapes:
    ///   - a `Tuple` that owns heap (a `(string, string)`-style element); an
    ///     all-BitCopy tuple is `Copy` and stays on the layout getter.
    ///   - a `Named` record/enum that is in the function's owned-Vec element key
    ///     set (the same set the W3.029 value-class allow-list and the codegen
    ///     descriptor derive from — `dedup-semantic-boundary`).
    fn is_owned_vec_element(&self, elem_ty: &ResolvedTy) -> bool {
        match elem_ty {
            // A tuple element is owned when any field transitively owns heap.
            // Use `named_elem_owns_heap` (which consults
            // `record_field_resolved_tys` for record fields) — NOT
            // `ty_contains_heap_owning`, which is record-layout BLIND and would
            // mis-classify a `(Rec, i64)` where `Rec` has a `string` field as
            // non-heap-owning. That false negative routed the getter to
            // `hew_vec_get_layout` on a Vec the constructor and scope-exit free
            // already built through the OWNED ABI (the release-path sibling
            // `binding_ty_is_plain_vec` / `tuple_is_all_bitcopy` correctly
            // classify it owned), so the layout-aware get aborted at runtime
            // ("Vec layout-aware operation is not implemented"). This is the
            // SAME record-aware authority codegen's `resolved_ty_contains_heap_leaf`
            // uses, so the getter, constructor, and free all agree
            // (`dedup-semantic-boundary`).
            ResolvedTy::Tuple(elems) => elems.iter().any(|e| self.named_elem_owns_heap(e)),
            // Nested collection elements (Vec<T> / HashMap / HashSet) are owned
            // heap handles constructed through the owned descriptor ABI: their
            // element loads route to `hew_vec_get_owned`, their pushes upgrade
            // to `hew_vec_push_owned` (COPY-IN), and the outer Vec releases via
            // `hew_vec_free_owned` running the per-element drop_fn (#1722). A
            // closure-pair `Vec<fn>` / `Vec<closure>` element keeps its existing
            // pointer/closure-pairs ABI (separate lane) — excluded here so it is
            // NOT reclassified to owned (must mirror codegen's
            // `resolved_ty_element_owns_heap_for_owned_vec` exactly, so the
            // drop_fn the elaborator emits matches what codegen constructs —
            // `dedup-semantic-boundary`).
            ResolvedTy::Named {
                builtin: Some(hew_types::BuiltinType::HashMap | hew_types::BuiltinType::HashSet),
                ..
            } => true,
            ResolvedTy::Named {
                builtin: Some(hew_types::BuiltinType::Vec),
                args,
                ..
            } => !args.first().is_some_and(|e| {
                matches!(e, ResolvedTy::Function { .. } | ResolvedTy::Closure { .. })
            }),
            ResolvedTy::Named { name, args, .. } => {
                let short = short_name(name);
                let key = if args.is_empty() {
                    name.clone()
                } else {
                    mangle_layout_key(name, args)
                };
                self.vec_owned_element_keys.contains(&key)
                    || self.vec_owned_element_keys.contains(name)
                    || self
                        .enum_layouts
                        .iter()
                        .any(|el| short_name(&el.name) == short)
                        && self
                            .vec_owned_element_keys
                            .iter()
                            .any(|k| short_name(k) == short)
            }
            // Every remaining `ResolvedTy` shape is NOT an owned-descriptor Vec
            // element. The match is exhaustive (no `_ => false` fall-through) so a
            // new `ResolvedTy` variant is a compile error here, never a silent
            // non-owning default — the leak surface this consolidation removes.
            // The heap-owning shapes among them are released by a DIFFERENT bucket
            // or are fail-closed, never leaked silently (pinned by
            // `release_bucket_partition_is_total_over_vec_elements`):
            //   - `String`: a plain element — the runtime walks `ElemKind::String`
            //     under the buffer-only `hew_vec_free` (`is_plain_vec_element`).
            //   - `Bytes`: a fat `{ ptr, len, cap }` triple, outside the single-
            //     pointer / owned-descriptor buckets; `Vec<bytes>` is fail-closed
            //     at construction (`Vec::new` is NYI for `Bytes`) →
            //     `classify_vec_element_release` returns `Unsupported`.
            //   - `Function` / `Closure`: a closure pair released by
            //     `ty_is_closure_pair_vec` / descriptor-driven `hew_vec_free_owned`.
            //   - `CancellationToken` and the remaining views/handles either own
            //     no heap as a flat element or are fail-closed at construction.
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
        }
    }

    /// The owned-locals seed authority: does a binding of type `ty` oblige
    /// scope-exit drop elaboration? `true` admits the binding into the
    /// `owned_locals` candidate ledger every downstream allow-set derivation,
    /// `unsupported_vec_element_diagnostics`, and `build_lifo_drops` scan.
    /// The verdict is the value-class seed: every class except `BitCopy`
    /// seeds (a `BitCopy` value owns no heap and its copy is free; a `View`
    /// seeds into the no-retain no-op drop arm; a `Linear` seeds so the
    /// move-checker's consume obligations observe it). Equivalent to
    /// `ValueOwnership::to_value_class`'s carried seed by construction
    /// (`ownership.rs` pins `to_value_class` ≡ `ValueClass::of_ty`). The
    /// frozen verdict table is `seed_gate_matches_value_class_authority`; the
    /// source-inventory pin `seed_fact_comparison_site_inventory_is_closed`
    /// keeps this body the ONLY seed-fact spelling in production code.
    ///
    /// Consulted on BOTH sides of the ledger: the seed sites that push into
    /// `owned_locals`, AND the consume-side handling of a `Use { Consume }`
    /// on a `BindingRef` (drop-flag-set vs `mark_binding_moved`). One
    /// authority on both sides is load-bearing: a consume side looser than
    /// the seed side leaves a moved-out binding in `owned_locals`, and the
    /// function-exit LIFO drop pass then releases a moved-out value (an
    /// over-drop / double-free); a tighter consume side leaks.
    ///
    /// Known limitation, preserved: the gate is record-blind via
    /// `ValueClass` — an unmarked user record classifies `Unknown`, which
    /// seeds.
    ///
    /// Three sibling gates are DIFFERENT facts and do not route here: the
    /// dyn-trait `let` arm seeds on `classify_dyn_trait_storage` (fail-closed
    /// storage discrimination), the param arm seeds on the HIR ownership
    /// checker's `param_consume` side-table verdict, and
    /// `gen_env_capture_admissible` gates generator-env capture
    /// flat-copyability on its own direct `ValueClass` test (it must not
    /// follow a future seed-rule change).
    pub(crate) fn binding_seeds_drop_elaboration(&self, ty: &ResolvedTy) -> bool {
        binding_seeds_drop_elaboration(ty, &self.type_classes)
    }

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
        // Unclaimed by every bucket. `ty_owns_heap(Vec<E>)` is `true`
        // unconditionally (the outer Vec owns its buffer), so this element has no
        // wired release protocol — fail closed with a typed, tracked reason
        // rather than silently classifying it non-owning (the leak surface).
        VecElementRelease::Unsupported(self.vec_element_unsupported_reason(elem))
    }

    /// The fail-closed reason for a `Vec<E>` element no release bucket claims.
    /// `bytes` (a fat `{ ptr, len, cap }` triple), bare runtime handles, and
    /// indirect-enum nodes own heap with no wired Vec-element release
    /// (`NoReleaseProtocol`) — a real release protocol exists to be wired (a
    /// fat-triple drop, a handle close, or a pointer-element node free). Any
    /// other shape reaching here owns no heap as a flat element; it is named with
    /// the anti-drift sentinel (`UnenumeratedShape`). The indirect-enum probe is
    /// explicit because the heap-ownership authority is blind to indirection — a
    /// scalar-payload `indirect enum` owns a heap node the authority reports as
    /// non-owning, so without this probe its `Vec` would mis-label as the
    /// sentinel instead of the actionable "release protocol unwired".
    fn vec_element_unsupported_reason(&self, elem: &ResolvedTy) -> FailClosedReason {
        if self.named_elem_owns_heap(elem) || ty_is_indirect_enum(elem, &self.enum_layouts) {
            FailClosedReason::NoReleaseProtocol
        } else {
            FailClosedReason::UnenumeratedShape
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
    /// Only `NoReleaseProtocol` is rejected, never `UnenumeratedShape`: the
    /// latter names an element owning NO heap as a flat element (a free
    /// `TypeParam` in a generic skeleton, `Unit`, a bare runtime view), so
    /// skipping its release leaks nothing — today's behaviour is preserved and an
    /// un-monomorphised generic `Vec<T>` is never rejected.
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
                if matches!(
                    self.classify_vec_element_release(elem),
                    VecElementRelease::Unsupported(FailClosedReason::NoReleaseProtocol)
                ) && !self.elem_is_owned_abi_releasable(elem)
                {
                    return Some(describe_vec_element(elem, &self.enum_layouts));
                }
                self.unsupported_vec_element_walk(elem, layouts, visiting)
            }
            ResolvedTy::Named { name, args, .. } => {
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
                    .record_field_tys(name, args)
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
    /// `ty_is_direct_enum_element(elem) && !named_elem_owns_heap(elem)`: a
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
    fn is_plain_vec_element(&self, elem: &ResolvedTy) -> bool {
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
        // the `named_elem_owns_heap` authority, never re-derived from BitCopy, so
        // a heap-owning enum still routes owned. `ty_is_direct_enum_element` is
        // the layout-Vec constructor's own membership, so release matches
        // construction; it is congruent with the vec-index getter's non-indirect
        // enum `hew_vec_get_layout` arm (`dedup-semantic-boundary`).
        (matches!(elem, ResolvedTy::Named { .. })
            && (ValueClass::of_ty(elem, &self.type_classes) == ValueClass::BitCopy
                || (self.ty_is_direct_enum_element(elem) && !self.named_elem_owns_heap(elem))))
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
    /// `is_plain_vec_element` admits those via the `named_elem_owns_heap`
    /// authority instead (see its doc). For tuples `ValueClass` cannot be used:
    /// `ValueClass::of_ty(Tuple(_))` ALWAYS returns `CowValue` regardless of
    /// field types, so the tuple path delegates to `tuple_is_all_bitcopy`, which
    /// recurses structurally. Using `!is_owned_vec_element` as the complement is
    /// unsound here because its backing `ty_contains_heap_owning` omits
    /// `record_field_resolved_tys` and can mis-classify a named record inside a
    /// tuple as non-heap-owning. An unresolved type parameter has no known value
    /// class and stays on the leak-as-before posture rather than risking a
    /// wrong-ABI free (`boundary-fail-closed`).
    pub(crate) fn binding_ty_is_plain_vec(&self, ty: &ResolvedTy) -> bool {
        let ResolvedTy::Named {
            args,
            builtin: Some(hew_types::BuiltinType::Vec),
            ..
        } = self.subst_ty(ty)
        else {
            return false;
        };
        // Element-level plain check factored into `is_plain_vec_element` so the
        // typed `classify_vec_element_release` partition shares the exact same
        // authority (no second copy to drift). The doc above this function
        // records why the `Named` arm reads the `named_elem_owns_heap` authority
        // (not `ValueClass` alone) and the `Tuple` arm uses `tuple_is_all_bitcopy`.
        args.first()
            .is_some_and(|elem| self.is_plain_vec_element(elem))
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
    ///   `named_elem_owns_heap` authority, exactly as `is_plain_vec_element`
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
                // the `named_elem_owns_heap` authority.
                ValueClass::of_ty(e, &self.type_classes) == ValueClass::BitCopy
                    || (self.ty_is_direct_enum_element(e) && !self.named_elem_owns_heap(e))
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

    /// True when a non-owned Vec element is backed by a runtime layout
    /// descriptor, matching codegen's `layout_vec_element_needs_descriptor`
    /// constructor authority. This is intentionally layout-membership based,
    /// not `ValueClass::BitCopy` based: payload-free and scalar-payload direct
    /// enums own no heap and are constructed as layout Vecs, but they are not
    /// marked `BitCopy` in the HIR value-class table.
    fn vec_element_uses_layout_descriptor(&self, elem_ty: &ResolvedTy) -> bool {
        match elem_ty {
            ResolvedTy::Tuple(_) => true,
            ResolvedTy::Named { name, args, .. } => {
                let short = short_name(name);
                let key = if args.is_empty() {
                    name.clone()
                } else {
                    mangle_layout_key(short, args)
                };
                self.record_field_orders
                    .keys()
                    .any(|known| known == &key || short_name(known) == short)
                    || self.ty_is_direct_enum_element(elem_ty)
            }
            _ => false,
        }
    }

    /// True when `elem_ty` is a registered DIRECT (non-indirect) user enum.
    ///
    /// A direct enum is stored inline in the vec buffer at its full
    /// tagged-union stride — the same layout-descriptor path a `BitCopy` record
    /// takes — whereas an indirect (heap-boxed, `is_indirect`) enum holds an
    /// 8-byte pointer per slot and routes through the pointer ABI.
    ///
    /// This membership is the seam shared by the vec-index getter
    /// (`hew_vec_get_layout` arm), the range-slice getter, and the plain-release
    /// predicate. It is load-bearing for release routing because a payload-free
    /// or scalar-payload direct enum owns no heap yet is NEVER marked `BitCopy`
    /// in the HIR value-class table (`finalize_user_record_value_classes`
    /// covers records only). Callers pair it with the `named_elem_owns_heap`
    /// authority so a heap-owning direct enum still routes owned.
    fn ty_is_direct_enum_element(&self, elem_ty: &ResolvedTy) -> bool {
        let ResolvedTy::Named { name, args, .. } = elem_ty else {
            return false;
        };
        let short = short_name(name);
        let key = if args.is_empty() {
            name.clone()
        } else {
            mangle_layout_key(short, args)
        };
        self.enum_layouts.iter().any(|el| {
            !el.is_indirect && (el.name == key || el.name == *name || short_name(&el.name) == short)
        })
    }

    /// True when `vec_ty` is a `Vec<T>` whose element `T` is an owned-Vec element.
    /// Substitutes through the monomorphisation map first so a polymorphic
    /// receiver type resolves to its concrete element before the owned-ness
    /// authority is consulted. Shares the `is_owned_vec_element` authority that
    /// the getter/free routing uses so the push ABI cannot disagree with the
    /// constructor (`dedup-semantic-boundary`).
    fn vec_receiver_has_owned_element(&self, vec_ty: &ResolvedTy) -> bool {
        let ResolvedTy::Named {
            args,
            builtin: Some(hew_types::BuiltinType::Vec),
            ..
        } = self.subst_ty(vec_ty)
        else {
            return false;
        };
        args.first()
            .is_some_and(|elem| self.is_owned_vec_element(elem))
    }

    /// Re-resolve the concrete runtime symbol for an element-typed `Vec<T>`
    /// method whose checker dispatch left a `hew_vec_*_FAMILY` placeholder
    /// because the element was a type parameter (#1929 Stage 1).
    ///
    /// Substitutes the receiver `Vec<T>` to its concrete `Vec<E>` for this
    /// monomorphisation, then resolves the symbol from the substituted element
    /// `E` through the same `hew_types::vec_authority` resolver used by the
    /// concrete checker path:
    ///
    ///  1. **Owned (non-`Copy`) element** (record/enum/tuple/nested-collection
    ///     that owns heap, per [`Self::is_owned_vec_element`]): route to the
    ///     `hew_vec_{push,get,set,pop}_owned` family. This is the #1929 Stage 2
    ///     owned-element lane — the same descriptor ABI the concrete owned path
    ///     uses (`hew_vec_get_owned` borrows the slot, `hew_vec_push_owned` /
    ///     `hew_vec_set_owned` COPY-IN, `hew_vec_pop_owned` moves out, and the
    ///     outer Vec releases via `hew_vec_free_owned`). The element is owned by
    ///     structural type, so the per-monomorphisation owned descriptor is
    ///     harvested into `vec_owned_element_keys` and the interior-alias result
    ///     contract both key off this same substituted `E` — so ownership
    ///     (retain/clone vs borrow, scope-exit free) matches the concrete path
    ///     exactly (`dedup-semantic-boundary`).
    ///  2. Otherwise look `E` up in the checker-exported
    ///     [`vec_generic_element_abi`] verdict table (scalar / `string` / `ptr`
    ///     / Copy value-record `layout`) and pass the token to the shared
    ///     source-derived symbol resolver.
    ///
    /// Returns `None` (fail closed) when the call is not an element-typed Vec
    /// op, the receiver is not a substituted `Vec`, or the element is neither
    /// owned nor in the verdict table (a genuinely unsupported element ABI —
    /// closure/function elements, which the owned authority excludes and the
    /// verdict table omits).
    pub(crate) fn resolve_polymorphic_vec_element_symbol(
        &self,
        target_family: hew_types::MethodTargetFamily,
        receiver_ty: &ResolvedTy,
    ) -> Option<String> {
        let hew_types::MethodTargetFamily::Vec(vec_method) = target_family else {
            return None;
        };
        let ResolvedTy::Named {
            args,
            builtin: Some(hew_types::BuiltinType::Vec),
            ..
        } = self.subst_ty(receiver_ty)
        else {
            return None;
        };
        let elem = args.first()?;
        let elem_ty = elem.to_ty();
        // Per-monomorphisation ABI: prefer the checker-exported verdict table
        // (its Ptr/Layout entries are Copy-gated at the checker boundary). On a
        // miss — a COMPOSITE monomorphised element (`W<i64>`, `Option<i64>`) the
        // table never enumerated because it keys on raw generic type-ARGUMENTS,
        // not the substituted element — classify the concrete element on demand
        // through the SAME shared token classifier the checker uses, so this side
        // and the constructor codegen reach one verdict (`dedup-semantic-boundary`,
        // #2737: the checker deferred `W<T>` rather than resolve it owned on the
        // generic spine while the constructor stamps a plain `W<i64>` descriptor).
        let abi = self
            .vec_generic_element_abi
            .get(&elem_ty)
            .copied()
            .or_else(|| {
                hew_types::vec_authority::classify_element_with(&elem_ty, &|name, args| {
                    self.nominal_indirect_for_vec_element(name, args)
                })
            });
        let is_owned = self.is_owned_vec_element(elem);
        // A Layout-token element is `Copy` (plain bit-copy `_layout` ops) exactly
        // when it owns no heap — the same fact the constructor consults to stamp a
        // plain vs owned descriptor. A heap-owning composite is routed by
        // `is_owned` above (owned family) and never reaches the copy-layout arm.
        let is_copy_layout = abi == Some(hew_types::VecElementToken::Layout)
            && !is_owned
            && !self.named_elem_owns_heap(elem);
        let profile = hew_types::vec_authority::VecElementProfile {
            abi,
            is_owned,
            is_copy_layout,
            is_function_like: matches!(
                elem,
                ResolvedTy::Function { .. } | ResolvedTy::Closure { .. }
            ),
            is_abstract: false,
        };
        match hew_types::vec_authority::resolve_runtime_symbol(
            vec_method,
            profile,
            hew_types::vec_authority::VecResolutionContext::MonomorphizedPlaceholder,
        ) {
            hew_types::vec_authority::VecSymbolResolution::Resolved(symbol) => Some(symbol),
            hew_types::vec_authority::VecSymbolResolution::Deferred
            | hew_types::vec_authority::VecSymbolResolution::Unavailable
            | hew_types::vec_authority::VecSymbolResolution::Unsupported(_) => None,
        }
    }

    /// Nominal indirection lookup backing the shared Vec element-token classifier
    /// ([`hew_types::vec_authority::classify_element_with`]) on the MIR
    /// monomorphisation re-resolution path. `Some(true)` for a registered
    /// indirect enum (heap-boxed pointer slot), `Some(false)` for a registered
    /// inline record/enum (layout-descriptor slot), `None` when the name resolves
    /// to no user layout in scope. Consults the SAME record/enum layout
    /// registries the owned-element and heap-ownership authorities read, so the
    /// element token this side derives matches the checker's `TypeDef`-backed
    /// verdict (`dedup-semantic-boundary`).
    fn nominal_indirect_for_vec_element(&self, name: &str, args: &[hew_types::Ty]) -> Option<bool> {
        let short = short_name(name);
        // The layout registries key generic instantiations by the monomorphised
        // mangle (`W$$i64`), so form that key from the substituted arguments; a
        // monomorphic nominal keeps its bare declared name. Mirrors the
        // record/enum key resolution `elem_is_owned_abi_releasable` uses.
        let resolved_args: Vec<ResolvedTy> = args
            .iter()
            .filter_map(|a| ResolvedTy::from_ty(a).ok())
            .collect();
        let key = if !args.is_empty() && resolved_args.len() == args.len() {
            mangle_layout_key(name, &resolved_args)
        } else {
            name.to_string()
        };
        if let Some(layout) = self
            .enum_layouts
            .iter()
            .find(|el| el.name == key || el.name == name || short_name(&el.name) == short)
        {
            return Some(layout.is_indirect);
        }
        if self.lookup_record_field_order(&key).is_some()
            || self.lookup_record_field_order(name).is_some()
        {
            return Some(false);
        }
        None
    }

    /// User-facing rendering of a `Vec<T>` receiver's substituted element type,
    /// for the fail-closed diagnostic when a polymorphic element ABI is
    /// deferred. Falls back to the whole receiver type when it is not a
    /// substituted `Vec`.
    fn vec_element_user_facing(&self, receiver_ty: &ResolvedTy) -> String {
        let substituted = self.subst_ty(receiver_ty);
        if let ResolvedTy::Named {
            args,
            builtin: Some(hew_types::BuiltinType::Vec),
            ..
        } = &substituted
        {
            if let Some(elem) = args.first() {
                return elem.to_ty().user_facing().to_string();
            }
        }
        substituted.to_ty().user_facing().to_string()
    }

    /// Recursively walk a block's statements + tail, harvesting owned-Vec
    /// element keys from every expression's type (the `Vec<T>` receiver of an
    /// owned-Vec op carries the type) and every let binding's declared type.
    /// True when `expr`, read as a produced fn VALUE, may carry a non-null heap
    /// closure environment. A capture-free literal or named-function reference
    /// answers `false`; parameters, fn-valued call results, capturing literals,
    /// aggregate/container reads, and merges/copies of those answer `true`.
    /// Feeds `closure_pair_env_may_be_nonnull` during the pre-pass.
    pub(crate) fn closure_rhs_may_carry_env(&self, expr: &HirExpr) -> bool {
        match &expr.kind {
            HirExprKind::Closure { captures, .. } => !captures.is_empty(),
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(id),
                ..
            } => {
                ty_is_closure_pair(&self.subst_ty(&expr.ty))
                    && self.closure_pair_env_may_be_nonnull.contains(id)
            }
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Item(_),
                ..
            } => false,
            HirExprKind::Call { .. }
            | HirExprKind::ResolvedImplCall { .. }
            | HirExprKind::CallTraitMethodStatic { .. }
            | HirExprKind::CallDynMethod { .. } => ty_is_closure_pair(&self.subst_ty(&expr.ty)),
            HirExprKind::Block(body) | HirExprKind::Scope { body } => body
                .tail
                .as_deref()
                .is_some_and(|tail| self.closure_rhs_may_carry_env(tail)),
            HirExprKind::If {
                then_expr,
                else_expr,
                ..
            } => {
                self.closure_rhs_may_carry_env(then_expr)
                    || else_expr
                        .as_deref()
                        .is_some_and(|eb| self.closure_rhs_may_carry_env(eb))
            }
            HirExprKind::Match { arms, .. } => arms
                .iter()
                .any(|arm| self.closure_rhs_may_carry_env(&arm.body)),
            // Any other fn-valued producer (record/tuple/Vec field read,
            // projection, indirect value source) is unproven. Owning
            // containers can legitimately hold a capturing closure, so
            // admitting an unknown projection would reopen the same env-box
            // leak behind a different laundering shape.
            _ => matches!(self.subst_ty(&expr.ty), ResolvedTy::Function { .. }),
        }
    }

    #[allow(
        clippy::too_many_lines,
        reason = "HIR statement dispatch keeps every kind's lowering in one match so the \
                  fail-closed arms surface together; splitting per-kind helpers would \
                  scatter the panic discipline across helper boundaries"
    )]
    pub(crate) fn stmt(&mut self, stmt: &hew_hir::HirStmt) {
        // Stage 2 (gdb `-g`): every `Instr` this statement lowers is attributed
        // to the statement's source span so gdb steps line-by-line. The cursor
        // stays set across the whole statement, so synthesised instructions
        // (drops, coercions) reuse this nearest-enclosing span fail-closed.
        self.current_span = Some((
            u32::try_from(stmt.span.start).unwrap_or(u32::MAX),
            u32::try_from(stmt.span.end).unwrap_or(u32::MAX),
        ));
        match &stmt.kind {
            HirStmtKind::Let(binding, Some(value)) => {
                let binding_ty = self.subst_ty(&binding.ty);
                let owned_string_record_key =
                    self.owned_string_record_init_key_for_let(&binding_ty, value);
                if owned_string_record_key.is_some() {
                    self.owned_string_record_value_sites.insert(value.site);
                }
                // Mirror the HIR forward-bind discipline at the MIR
                // layer for actor-lambda RHS. When the value is
                // `HirExprKind::SpawnLambdaActor`, pre-allocate the
                // binding's backend slot as a
                // `Place::LambdaActorHandle(N)` BEFORE lowering the
                // value. The body walk then sees a `BindingRef` to
                // the let-name resolve to a `binding_locals` entry
                // that already points at the actor's own handle;
                // the producer reuses this slot via
                // `pending_lambda_actor_handle` instead of allocating
                // a second local. Without this pre-allocation, a
                // Weak self-capture would try to look up a slot for
                // the let-binding that doesn't exist yet.
                let pending = if matches!(&value.kind, HirExprKind::SpawnLambdaActor { .. }) {
                    let slot = self.alloc_local(self.subst_ty(&binding.ty));
                    let Place::Local(local_id) = slot else {
                        unreachable!("alloc_local returns Place::Local");
                    };
                    let handle = Place::LambdaActorHandle(local_id);
                    self.binding_locals.insert(binding.id, handle);
                    self.pending_lambda_actor_handle = Some(handle);
                    true
                } else {
                    false
                };
                self.pending_closure_literal_suspends = None;
                self.pending_closure_literal_heap = None;
                if matches!(binding_ty, ResolvedTy::Bytes)
                    && matches!(value.kind, HirExprKind::BindingRef { .. })
                {
                    self.bytes_local_share_sites.insert(value.site);
                }
                if matches!(binding_ty, ResolvedTy::String) {
                    if let HirExprKind::BindingRef {
                        resolved: ResolvedRef::Binding(source),
                        ..
                    } = value.kind
                    {
                        self.string_local_share_sites
                            .insert(value.site, (source, binding.id));
                    }
                }
                let diag_len_before_value = self.diagnostics.len();
                let value_place = self.lower_value(value);
                // Cascade suppression: a `let` whose initializer failed to lower
                // (`None`) AFTER emitting its own diagnostic poisons the binding,
                // so a later `BindingRef` to it stays silent instead of stacking
                // an `UnresolvedPlace` follow-on on the root error. Guarded on a
                // diagnostic actually having been emitted, so a silent-`None`
                // producer (a real defect with no prior error) still surfaces.
                if value_place.is_none() && self.diagnostics.len() > diag_len_before_value {
                    self.poisoned_let_bindings.insert(binding.id);
                }
                if pending {
                    self.pending_lambda_actor_handle = None;
                }
                // Suspendable-callee discriminator: when this binding holds a
                // closure literal whose invoke-shim carries a suspend terminator,
                // record it so a later `read_once()` call lowers to the driving
                // `Terminator::SuspendingCallClosure` rather than the direct
                // `Instr::CallClosure` (a non-suspending closure is never
                // recorded — it stays on the direct path).
                if matches!(value.kind, HirExprKind::Closure { .. })
                    && self.pending_closure_literal_suspends == Some(true)
                {
                    self.suspending_closure_bindings.insert(binding.id);
                }
                self.pending_closure_literal_suspends = None;
                // Closure-pair env-box ownership admission (sole-owner affine
                // model). A fn-typed binding owns its pair's heap env-box
                // free obligation only when the RHS shape proves the pair is
                // heap-or-null by construction:
                //   - a closure literal whose escape class selected `Heap`
                //     (`pending_closure_literal_heap` — heap box with
                //     captures, null without);
                //   - a fn-typed call result (a pair crossing a return
                //     boundary is Escapes-classified at its literal site, so
                //     it is heap-or-null transitively);
                //   - a rebind of an already-admitted binding (ownership
                //     transfers; the source is marked moved so the env-box
                //     is freed exactly once — `raii-null-after-move`).
                // Every other producing shape (params, if/else merges, …) is
                // excluded: such a pair may carry a stack env, and freeing
                // one would over-free a frame address. Excluded pairs leak
                // at worst, never double-free (`boundary-fail-closed`).
                // `elaborate` narrows the set further (returned / aliased /
                // consumed pairs) before any drop is emitted.
                if matches!(
                    binding_ty,
                    ResolvedTy::Function { .. } | ResolvedTy::Closure { .. }
                ) && value_place.is_some()
                {
                    let literal_heap = self.pending_closure_literal_heap == Some(true);
                    match classify_closure_pair_rhs(value, literal_heap, &self.closure_pair_owned) {
                        ClosurePairRhs::Owned => {
                            self.closure_pair_owned.insert(binding.id);
                        }
                        ClosurePairRhs::TransferFrom(src_id) => {
                            self.closure_pair_owned.remove(&src_id);
                            self.closure_pair_moved.insert(src_id);
                            self.mark_binding_moved(src_id);
                            self.closure_pair_owned.insert(binding.id);
                        }
                        ClosurePairRhs::NotOwned => {
                            // A named-function pair (`let f = double;`) carries
                            // a null env by construction — exempt it (and
                            // rebinds of an exempt binding) from the ingress
                            // discipline: there is no environment to
                            // double-free.
                            if self.closure_rhs_is_null_env_pair(value) {
                                self.closure_pair_null_env.insert(binding.id);
                            }
                        }
                    }
                }
                self.pending_closure_literal_heap = None;
                self.decide(value);
                self.statements.push(MirStatement::Bind {
                    binding: binding.id,
                    name: binding.name.clone(),
                    site: value.site,
                    ty: binding_ty.clone(),
                });
                self.record_binding_scope(binding.id);
                // W3.031 Stage 1: discriminate the dyn-trait owned-binding
                // case structurally on `value.ty` rather than on `binding.ty`.
                // HIR's `lower_type` does not yet lower `TypeExpr::TraitObject`
                // for `let`-annotation positions (the annotation collapses to
                // `ResolvedTy::Unit` upstream), but every dyn binding's
                // initialiser carries the post-coerce
                // `ResolvedTy::TraitObject` on `value.ty`, so probing the
                // value's type is the reliable structural fact. The pre-
                // existing non-dyn arm continues to gate on the binding
                // type's value class as before.
                let value_ty = self.subst_ty(&value.ty);
                let dyn_owned =
                    matches!(value_ty, ResolvedTy::TraitObject { .. }) && value_place.is_some();
                if dyn_owned {
                    // dyn-trait owned local: classify storage from the RHS
                    // expression shape and push into `owned_locals` with the
                    // actual `TraitObject` type so `build_lifo_drops` reaches
                    // the dyn-trait arm. Fail-closed if classification
                    // returns `Err`.
                    match classify_dyn_trait_storage(value, &self.dyn_trait_storage) {
                        Ok(storage) => {
                            self.dyn_trait_storage.insert(binding.id, storage);
                            self.register_owned_local(
                                binding.id,
                                binding.name.clone(),
                                value_ty.clone(),
                            );
                            // Transitive `dyn -> dyn` rebind suppression.
                            //
                            // For `let d2 = d1;` (and `let d3 = { d2 };`
                            // through transparent block-tail wrappers),
                            // the RHS transfers ownership of an existing
                            // `dyn Trait` binding's fat pointer into the
                            // new binding. The vtable slot-0 ritual must
                            // run exactly once, at the *final* binding's
                            // scope exit; every intermediate rebind's
                            // `owned_locals` entry would otherwise emit
                            // an additional `DropKind::TraitObject` and
                            // double-drop the underlying storage.
                            //
                            // `classify_dyn_trait_storage` already
                            // requires the source binding to carry a
                            // `dyn_trait_storage` entry to reach this
                            // arm, so finding `Some(src_id)` below is
                            // exactly the rebind case. `mark_binding_moved`
                            // is idempotent (a no-op on bindings that
                            // were already consumed earlier in the
                            // expression's lowering, e.g. by the
                            // `BindingRef`/`IntentKind::Consume` path),
                            // so calling it unconditionally here is safe.
                            //
                            // Fail-closed posture: if the RHS shape is
                            // not one the helper recognises as a
                            // dyn-rebind source, `dyn_rebind_source_binding`
                            // returns `None` and no suppression runs —
                            // but `classify_dyn_trait_storage` would
                            // have rejected the same shape with `Err`
                            // and routed through the
                            // `TraitObjectStorageUndetermined` diagnostic
                            // above, so the only way to reach this arm
                            // with `None` is via the
                            // `CoerceToDynTrait` / `Call*` producer
                            // shapes where there is no upstream binding
                            // to suppress (the producer-site suppression
                            // for those is handled at the
                            // `lower_value` arms for those expressions).
                            if let Some(src_id) = dyn_rebind_source_binding(value) {
                                self.mark_binding_moved(src_id);
                            }
                        }
                        Err(reason) => {
                            self.diagnostics.push(MirDiagnostic {
                                kind: MirDiagnosticKind::TraitObjectStorageUndetermined {
                                    binding: binding.id,
                                    name: binding.name.clone(),
                                    site: value.site,
                                    reason,
                                },
                                note: format!(
                                    "MIR drop elaboration cannot determine the \
                                     TraitObjectStorage (FrameOwned / HeapBoxed) for \
                                     binding `{}` from the RHS expression shape; the \
                                     binding is not added to owned_locals so no drop \
                                     is elaborated, and the MIR pipeline aborts at \
                                     the boundary instead of fabricating a default \
                                     storage (W3.031 Stage 1).",
                                    binding.name
                                ),
                            });
                        }
                    }
                } else if self.binding_seeds_drop_elaboration(&binding_ty)
                    && (pending || value_place.is_some())
                {
                    // Only register the binding in `owned_locals` when
                    // the same iteration will also wire `binding_locals`
                    // (either pre-emptively via the lambda-actor
                    // `pending` path above, or via the `Some(src)` arm
                    // below). Keeping the two ledgers in sync is the
                    // structural invariant that `build_lifo_drops`
                    // depends on: an `owned_locals` entry without a
                    // matching `binding_locals` Place panics drop
                    // elaboration. When `lower_value` returns `None`
                    // (e.g. `lower_spawn_actor` emitted a `spawn of
                    // unknown actor` MIR diagnostic), the binding has
                    // no backend Place, so it must not enter
                    // `owned_locals` either. LESSONS:
                    // boundary-fail-closed, raii-null-after-move.
                    //
                    // A `let mid = o.mid` / `let inner = t.0` projection whose
                    // field is an inline aggregate is a byte-copy interior ALIAS
                    // (`field_projection_alias_provenance` — the ByteCopyAlias
                    // class): register it `AliasOf` so the owner's composite
                    // frees the tree and the alias never trips the composite
                    // provers' blanket (#2375). Every other binding — including
                    // the `string`-Retained and single-pointer HandleTransfer
                    // load classes, and every fresh producer — keeps its
                    // `ScopeExit` ownership.
                    match self.field_projection_alias_provenance(value, &binding_ty) {
                        Some(provenance) => self.register_owned_local_alias(
                            binding.id,
                            binding.name.clone(),
                            binding_ty.clone(),
                            provenance,
                        ),
                        None => self.register_owned_local(
                            binding.id,
                            binding.name.clone(),
                            binding_ty.clone(),
                        ),
                    }
                    // Tag generator/`AsyncGenerator` handle bindings with their
                    // declaring scope so a per-scope-exit `hew_gen_coro_destroy` fires
                    // when the scope closes — covering the loop-re-entry case the
                    // function-exit drop misses (see `scope_generator_bindings`).
                    if ty_is_generator_handle(&binding_ty) {
                        if let Some(scope) = self.active_scopes.last().copied() {
                            self.scope_generator_bindings.push((
                                scope,
                                binding.id,
                                binding_ty.clone(),
                            ));
                        }
                    }
                    // 3b-1 — tag the `for await` desugar's synthetic
                    // `Stream<T>` / `Receiver<T>` CURSOR with its declaring scope
                    // so a per-scope-exit `hew_stream_close` /
                    // `hew_channel_receiver_close` fires when the scope closes,
                    // closing the stream on `break` / early `return` /
                    // exhaustion instead of deferring to function exit (the
                    // deadlock this fixes — see `scope_stream_bindings`). Gated
                    // to the cursor binding: a
                    // user `let s = <stream>` that is returned or consumed
                    // elsewhere must keep its move-checked function-exit close,
                    // or the unconditional inline close would free a moved-out
                    // handle (see `FOR_ITER_CURSOR_NAME_PREFIX`).
                    if ty_is_stream_handle(&binding_ty)
                        && binding.name.starts_with(FOR_ITER_CURSOR_NAME_PREFIX)
                    {
                        if let Some(scope) = self.active_scopes.last().copied() {
                            self.scope_stream_bindings.push((
                                scope,
                                binding.id,
                                binding_ty.clone(),
                            ));
                        }
                    }
                    // #1949 — tag a sole-owner `for x in …` cursor (`VecIter<T>`)
                    // with its declaring scope so a per-scope-exit
                    // `__hew_record_drop_inplace_VecIter$$T` frees its `vec`
                    // handle when the scope closes, releasing the handle on every
                    // outer-loop iteration (the leak this fixes). Only a cursor
                    // that solely owns its handle (rvalue / `to_vec()` / consumed
                    // `into_iter()` source) is registered; a CowShare place source
                    // keeps the source binding's drop instead (see
                    // `vec_iter_let_cursor_owns_handle`).
                    //
                    // #2540 — a projection rooted at an actor state field
                    // (`for v in x.v`) is ALSO excluded: the projected leaf is
                    // owned by the actor STATE, whose `__hew_state_drop_*` frees
                    // it unconditionally. Registering the cursor would double-free
                    // that state-owned buffer against the state drop (the UAF the
                    // supervisor normal-return cleanup epilogue exposes). See
                    // `vec_iter_source_projects_actor_state_field`.
                    //
                    // #2545 — an INDEX projection of an owned-element Vec
                    // (`for v in rows[i]`, `rows: Vec<Vec<T>>`) is excluded for the
                    // same reason: the container's `hew_vec_free_owned` recursion
                    // frees `rows[i]` exactly once, so the cursor must BORROW or
                    // the inner handle is freed twice. See
                    // `vec_iter_source_indexes_owned_element_vec`.
                    if vec_iter_ty_drop_safe(&binding_ty)
                        && vec_iter_let_cursor_owns_handle(value)
                        && !self.vec_iter_source_projects_actor_state_field(value)
                        && !self.vec_iter_source_indexes_owned_element_vec(value)
                    {
                        if let Some(scope) = self.active_scopes.last().copied() {
                            self.scope_vec_iter_bindings.push((
                                scope,
                                binding.id,
                                binding_ty.clone(),
                            ));
                        }
                    }
                }
                if owned_string_record_key.is_some() && value_place.is_some() {
                    self.owned_string_record_bindings.insert(binding.id);
                }
                // Backend stream: the binding owns a fresh local that the
                // initialiser's value is moved into. The pre-allocated
                // actor-lambda case already wired `binding_locals` and
                // does not need a second slot.
                if pending {
                    // The lambda-actor case: the producer already
                    // routed the binding to its `LambdaActorHandle`;
                    // no Move instruction is required (the handle is
                    // the value).
                } else if let Some(src) = value_place {
                    // Handle-typed places (DuplexHandle, SendHalf, RecvHalf,
                    // LambdaActorHandle) ARE the binding's backend slot —
                    // they carry ownership-discipline semantics through the
                    // Place kind itself.  Emitting a `Move { dest:
                    // Local(M), src: DuplexHandle(N) }` would store the
                    // handle in a generic Local, losing the kind information
                    // that `drop_kind_for` and `validate_cross_block_*` rely
                    // on (`drop_kind_for(Local(_)) → DropKind::Resource`).
                    // Register the handle Place directly in `binding_locals`
                    // without allocating a second local or emitting a Move.
                    match src {
                        Place::DuplexHandle(_)
                        | Place::SendHalf(_)
                        | Place::RecvHalf(_)
                        | Place::LambdaActorHandle(_)
                        | Place::ActorHandle(_) => {
                            self.binding_locals.insert(binding.id, src);
                        }
                        Place::Local(n) if self.tuple_decomp.contains_key(&n) => {
                            // Tuple-proxy: store the proxy directly so TupleIndex can recover
                            // element Places via tuple_decomp[n] — the existing Local-Move arm
                            // would allocate a fresh slot and lose the index that tuple_decomp
                            // is keyed by, leaving owned_locals entries without binding_locals.
                            self.binding_locals.insert(binding.id, src);
                        }
                        Place::Local(_) | Place::ReturnSlot => {
                            let slot = self.alloc_local(binding_ty.clone());
                            self.push_instr(Instr::Move { dest: slot, src });
                            self.binding_locals.insert(binding.id, slot);
                        }
                        // Machine sub-structure places (`MachineTag` and
                        // `MachineVariant`) are addressing primitives — they
                        // project into a machine value rather than denoting
                        // an independent binding. A `Let` that binds directly
                        // to one of these is a builder invariant violation;
                        // fail-closed with a panic so the lowering defect
                        // surfaces at MIR construction time rather than
                        // silently producing malformed IR.
                        // WHY not a MirDiagnostic: the invariant is imposed
                        // by the producer, not by user code; a panic is the
                        // correct fail-closed signal for a producer bug.
                        Place::MachineTag(_)
                        | Place::MachineVariant { .. }
                        | Place::EnumTag(_)
                        | Place::EnumVariant { .. } => {
                            panic!(
                                "builder invariant: `Let` binding may not bind directly to a \
                                 MachineTag / MachineVariant / EnumTag / EnumVariant \
                                 place; these are projection primitives into a tagged-union \
                                 value, not independent bindings. Binding {:?}, src {:?}",
                                binding.id, src
                            );
                        }
                    }
                }
                // #1933 / #1941 — allocate the path-sensitive drop-flag for a
                // non-idempotent user `#[resource]` binding now that its backend
                // Place is wired into `binding_locals`. Zero-initialised here so
                // the flag dominates every `Consume` use site and every
                // scope-exit drop; set to 1 at each consume. A no-op for every
                // other binding class (see `resource_needs_drop_flag`).
                self.maybe_alloc_resource_drop_flag(binding.id, &binding_ty);
                self.maybe_alloc_overwrite_guard_flag(binding);
                // #2418 — allocate the path-sensitive scope-exit drop-flag for
                // an owned collection local the pre-pass saw consumed, so a
                // conditional move keeps its (flag-gated) scope-exit release
                // on the not-moved path instead of retracting it entirely.
                self.maybe_alloc_collection_drop_flag(binding, &binding_ty);
            }
            HirStmtKind::Let(_, None) => {}
            HirStmtKind::Expr(expr) => {
                self.lower_expr_statement(expr);
                self.statements.push(MirStatement::Evaluate {
                    site: expr.site,
                    ty: self.subst_ty(&expr.ty),
                });
            }
            HirStmtKind::Assign { target, value } => {
                self.assign(target, value);
                self.statements.push(MirStatement::Evaluate {
                    site: value.site,
                    ty: ResolvedTy::Unit,
                });
            }
            HirStmtKind::Return(Some(expr)) => {
                let value_place = self.lower_value(expr);
                self.decide(expr);
                self.mark_returned_binding_moved(expr);
                self.statements.push(MirStatement::Return {
                    site: Some(expr.site),
                    ty: self.subst_ty(&expr.ty),
                });
                // Move the return value to ReturnSlot BEFORE executing
                // defers — the value is secured so defers cannot corrupt it.
                if let Some(src) = value_place {
                    self.push_instr(Instr::Move {
                        dest: Place::ReturnSlot,
                        src,
                    });
                }
                // Emit defers for all enclosing scopes (innermost first).
                // Q205-B: defers observe the binding state at this program
                // point — mutable vars have their final value; moved bindings
                // are flagged by the move-checker.
                self.emit_defers_for_return();
                // Free every active consuming-body yielded value on the
                // return edge (`cleanup-all-exits`): a `return` inside a
                // `for await v in stream` / `for x in gen()` body exits the
                // loop past the body-end drop, so the current iteration's
                // received value must be released here. After defers (a defer
                // may still read the value), before sealing. `return v` is
                // protected by the per-entry escape scan — the ReturnSlot
                // Move above marks the value caller-owned.
                self.emit_generator_yield_value_drops_for_exit_edge(0);
                self.emit_stream_drops_for_exit_edge(0);
                // Seal the current basic block with Terminator::Return so
                // codegen actually emits an early return at this program
                // point. Codegen consumes the block terminator (not the
                // `MirStatement::Return` THIR marker), so without sealing
                // the block here the post-`return` statements would
                // continue executing — turning `return` into a no-op.
                // Start a fresh cursor block to hold any source code
                // that lexically follows the return; that block has no
                // predecessor and is dead-code-eliminated by LLVM.
                self.finish_current_block(Terminator::Return);
                let dead = self.alloc_block();
                self.start_dead_block(dead);
            }
            HirStmtKind::Return(None) => {
                // Emit defers before the unit return.
                self.emit_defers_for_return();
                // Release the current iteration's yielded value(s) on this
                // return edge — same discipline as Return(Some) above.
                self.emit_generator_yield_value_drops_for_exit_edge(0);
                self.emit_stream_drops_for_exit_edge(0);
                self.statements.push(MirStatement::Return {
                    site: None,
                    ty: ResolvedTy::Unit,
                });
                // Same seal-and-fresh-cursor discipline as Return(Some):
                // codegen needs a Terminator::Return for the early-exit
                // path to actually take effect at runtime.
                self.finish_current_block(Terminator::Return);
                let dead = self.alloc_block();
                self.start_dead_block(dead);
            }
            HirStmtKind::Defer { body, scope_id } => {
                // Record the deferred body for materialization at scope exit.
                // Q205-B: bindings are resolved by lexical reference at execution
                // time; moved/consumed bindings are validated at materialization.
                self.pending_defers
                    .entry(*scope_id)
                    .or_default()
                    .push(body.as_ref().clone());
            }
            HirStmtKind::LetElse {
                scrutinee,
                variant_idx,
                bindings,
                success_prelude,
                payload_variant_predicates,
                else_body,
            } => {
                self.lower_let_else_stmt(
                    scrutinee,
                    *variant_idx,
                    bindings,
                    success_prelude,
                    payload_variant_predicates,
                    else_body,
                );
            }
        }
    }

    fn lower_expr_statement(&mut self, expr: &HirExpr) {
        if let Some((symbol, args, site)) = runtime_symbol_for_call_expr(expr) {
            // Thread the checker-recorded result type even in statement
            // (discarded) context. Only `hew_duplex_send` consumes it, and it
            // needs the type to decide the result SHAPE: a tell-shaped `.send`
            // is fire-and-forget here, but an ask-shaped `.send` must fail
            // closed in statement position too rather than lower as a tell that
            // silently drops the reply (`no-fail-open-fallback-after-authority`).
            let _ = self.lower_runtime_call(
                &symbol,
                args,
                site,
                RuntimeCallContext::Discarded,
                Some(&expr.ty),
            );
        } else {
            // Discarded expression. Any fresh-owned `string` temporary the
            // expression produces — `xs[i]`/`xs.get(i)` over `Vec<string>`
            // (`hew_vec_get_str`), `a + b`, `.to_uppercase()`, … — is released
            // by the general owned-`string` temporary substrate's DISCARD path
            // (`apply_nested_fresh_string_temp_drops`), which splices one inline
            // `hew_string_drop` after the unused producer. No Vec-specific
            // handling is owed here.
            //
            // #2648 preflight — run BEFORE `lower_value`. A rejected discarded
            // call-scrutinee (forwarded borrowed parameter, un-audited heap
            // extern) pushes one diagnostic and emits no MIR / no owner. The
            // early return is the safety guard; the non-Call HashMap-get owner
            // path in `register_discarded_call_result_owner` is `NotApplicable`
            // to the preflight and proceeds unchanged.
            if let Err(diag) = self.classify_call_scrutinee_admission(expr) {
                self.diagnostics.push(*diag);
                return;
            }
            if let Some(place) = self.lower_value(expr) {
                self.register_discarded_call_result_owner(expr, place);
            }
        }
    }

    #[allow(
        clippy::too_many_lines,
        reason = "single match over assignable HIR target shapes (binding, record field, \
                  actor field, HashMap index); each arm is a fail-closed boundary rule and \
                  splitting would obscure the exhaustiveness requirement"
    )]
    fn assign(&mut self, target: &HirExpr, value: &HirExpr) {
        let Some(src) = self.lower_value(value) else {
            return;
        };
        if let Some((field_offset, _)) = self.actor_state_field_for_target(target) {
            self.instructions
                .push(Instr::ActorStateFieldStore { field_offset, src });
            return;
        }
        match &target.kind {
            HirExprKind::ResolvedImplCall {
                receiver,
                method_name,
                target_symbol,
                target_family,
                type_args,
                args,
                ..
            } if matches!(
                target_family,
                hew_types::MethodTargetFamily::Vec(hew_types::VecMethod::Set)
            ) =>
            {
                if type_args.len() != 1 {
                    unreachable!(
                        "vec `.{method_name}` resolved to family {target_family:?} with {} \
                         type_args; Vec impls are registered with one element type",
                        type_args.len()
                    );
                }
                let Some(receiver_place) = self.lower_value(receiver) else {
                    return;
                };
                let Some(index_arg) = args.first() else {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::UnsupportedNode {
                            reason: "checker-resolved Vec set target has no index argument"
                                .to_string(),
                        },
                        note: "Vec index assignment lowering requires exactly one index argument"
                            .to_string(),
                    });
                    return;
                };
                if args.len() != 1 {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::UnsupportedNode {
                            reason: format!(
                                "checker-resolved Vec set target has {} index arguments",
                                args.len()
                            ),
                        },
                        note: "Vec index assignment lowering requires exactly one index argument"
                            .to_string(),
                    });
                    return;
                }
                let Some(raw_index_place) = self.lower_value(index_arg) else {
                    return;
                };
                let index_place = if let Place::Local(raw_id) = raw_index_place {
                    let raw_ty = self.locals[raw_id as usize].clone();
                    match raw_ty {
                        ResolvedTy::I8 | ResolvedTy::I16 | ResolvedTy::I32 => {
                            let wide_place = self.alloc_local(ResolvedTy::I64);
                            self.push_instr(Instr::NumericCast {
                                dest: wide_place,
                                src: raw_index_place,
                                from_ty: raw_ty,
                                to_ty: ResolvedTy::I64,
                            });
                            wide_place
                        }
                        _ => raw_index_place,
                    }
                } else {
                    raw_index_place
                };
                let len_place = self.alloc_local(ResolvedTy::I64);
                self.push_instr(Instr::CallRuntimeAbi(
                    crate::model::RuntimeCall::new(
                        "hew_vec_len",
                        vec![receiver_place],
                        Some(len_place),
                    )
                    .expect("hew_vec_len is an allowlisted runtime symbol"),
                ));
                let oob_flag = self.alloc_local(ResolvedTy::Bool);
                self.push_instr(Instr::IntCmp {
                    dest: oob_flag,
                    pred: CmpPred::UnsignedGreaterEq,
                    lhs: index_place,
                    rhs: len_place,
                });
                let trap_bb = self.alloc_block();
                let cont_bb = self.alloc_block();
                self.finish_current_block(Terminator::Branch {
                    cond: oob_flag,
                    then_target: trap_bb,
                    else_target: cont_bb,
                });
                self.start_block(trap_bb);
                self.finish_current_block(Terminator::Trap {
                    kind: TrapKind::IndexOutOfBounds,
                });
                self.start_block(cont_bb);
                self.enforce_closure_pair_ingress(value);
                let arg_places = vec![receiver_place, index_place, src];
                let next = self.alloc_block();
                // A Vec index-assignment of a fresh materialised rvalue
                // (`v[i] = Name { .. }`, `v[i] = make()`) has the SAME
                // unbound-temp hole the `.set(i, ..)` method path does:
                // `hew_vec_set_owned` is COPY-IN (deep-clones the element into
                // the slot), but the throwaway constructor temp has no binding
                // and no scope-exit drop to balance that clone, so its owned
                // heap leaks. Route it to the MOVE-in sibling
                // `hew_vec_set_owned_move` (byte-transfers the element's heap
                // into the slot without a clone; the source temp is then dead).
                // `expr_is_materialized_owner` is the identical fresh-rvalue
                // predicate the `.set()`/push paths use: a bare `BindingRef`
                // (a shared/after-read local) returns false and stays COPY-IN
                // (moving it would double-free the live binding's heap), and a
                // construction embedding a whole by-value parameter returns
                // false too. This mirrors the method path's Vec::Set routing.
                let effective_symbol = if target_symbol.as_str() == "hew_vec_set_owned"
                    && Self::expr_is_materialized_owner(
                        value,
                        &self.funcupdate_fn_returns_fresh,
                        &self.funcupdate_param_ids,
                    ) {
                    "hew_vec_set_owned_move"
                } else {
                    target_symbol.as_str()
                };
                let builtin = hew_types::runtime_call::RuntimeCallFamily::from_mir_builtin_symbol(
                    effective_symbol,
                );
                self.finish_current_block(Terminator::Call {
                    callee: effective_symbol.to_string(),
                    builtin,
                    args: arg_places,
                    dest: None,
                    next,
                });
                self.start_block(next);
            }
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(binding),
                name,
                ..
            } => {
                if let Some(dest) = self.binding_locals.get(binding).copied() {
                    // #2420 -- the overwrite release below is sound ONLY when
                    // the incoming value cannot alias the outgoing value's
                    // heap. An RHS that reads the reassigned binding (`s =
                    // grow(s)`, `s = S { n: s.n + 1, v: s.v }`) can hand back
                    // an UN-RETAINED alias of the old value's owned fields:
                    // by-value heap params are BORROWS and a non-`string`
                    // owned field load is a raw pointer copy, so releasing the
                    // old value here frees storage the incoming value still
                    // references -- use-after-free on the next field use and a
                    // double-free at the next release. When the RHS may alias,
                    // skip the release on BOTH the static and the flag-gated
                    // paths: fail-open (leak) is this seam's documented
                    // posture, matching the scope-exit exclusion
                    // (`derive_owned_record_drop_allowed`) for the identical
                    // aliasing channel. WHEN-OBSOLETE: the COW retain-on-share
                    // spine (every share retained => release always sound).
                    let rhs_may_alias_old = self.reassign_rhs_may_alias_binding(value, *binding);
                    // #53 / #2301: release the prior heap-owning value before
                    // the slot is overwritten.
                    if let Some(flag) = self.overwrite_guard_flags.get(binding).copied() {
                        // #2301 -- `binding` is consumed on one control-flow path
                        // and overwritten on another. The consume removed it from
                        // `owned_locals` globally, so the static gate below would
                        // wrongly SKIP the release on the non-consuming path and
                        // leak the still-owned old value. Gate on the runtime
                        // flag instead: release iff `flag == 0`; the consume set
                        // `flag = 1` to hand the value to its new owner. Reset to
                        // 0 after the store so the fresh value is released on the
                        // next overwrite.
                        if !rhs_may_alias_old {
                            self.emit_flag_gated_overwrite_release(dest, &target.ty, flag);
                        }
                        self.push_instr(Instr::Move { dest, src });
                        self.push_instr(Instr::ConstI64 {
                            dest: flag,
                            value: 0,
                        });
                    } else {
                        // #53: gated on the binding still owning live heap
                        // (scope-exit-live `owned_locals` membership) -- a
                        // self-reassign r = T{..r} or a move-out RHS already
                        // consumed it (dispositioned off the scope-exit set by
                        // `mark_binding_moved`, so absent from the live view),
                        // so this is skipped and never double-frees.
                        if !rhs_may_alias_old
                            && self.owned_locals.iter().any(|entry| {
                                &entry.binding == binding
                                    && entry.disposition == Disposition::ScopeExit
                            })
                        {
                            self.emit_local_overwrite_release(dest, &target.ty);
                        }
                        self.push_instr(Instr::Move { dest, src });
                    }
                    // A simple-variable assignment RE-DEFINES its target: after
                    // `h = <rhs>` the binding `h` holds a fresh value and is
                    // unconditionally Live, regardless of any move/consume the
                    // RHS performed on `h` itself. Emit a checker-stream `Bind`
                    // so move-state tracking resets `h` to Live. Without this the
                    // self-consuming reassign idiom `h = T { ..h, f: new }`
                    // (the canonical functional-update loop body) would leave `h`
                    // flagged `Consumed` from the `..h` ingress and every
                    // subsequent read — including the next loop iteration — would
                    // spuriously trip `UseAfterConsume`. This re-`Bind` carries no
                    // drop semantics (it does not touch `owned_locals`, which is
                    // populated only at `let`/param sites), so scope-exit drop
                    // accounting for `h` is unchanged.
                    self.statements.push(MirStatement::Bind {
                        binding: *binding,
                        name: name.clone(),
                        site: target.site,
                        ty: self.subst_ty(&target.ty),
                    });
                } else if let Some(source) = self.capture_env_sources.get(binding).cloned() {
                    // #1′ BorrowMut write-back: the assignment target is a
                    // captured `var` reassigned inside the closure body
                    // (`var total; |n| { total = total + n; total }`). The
                    // binding has no `binding_locals` slot — it lives in the
                    // closure env — so the write lands in the env field via the
                    // store twin of `ClosureEnvFieldLoad`. The env owns the
                    // mutable scalar (Option B): mutations accumulate across
                    // calls through the persistent env pointer, and the caller's
                    // original binding is independent.
                    //
                    // Restricted to `BitCopy` scalar fields. An owned captured
                    // field (string/Vec/record) would leak its prior value on
                    // overwrite without an env-field release — out of scope for
                    // the non-suspend scalar write-back path — so fail closed
                    // with a spanned diagnostic rather than emit a
                    // silently-leaking store.
                    let field_class = ValueClass::of_ty(&source.ty, &self.type_classes);
                    if field_class == ValueClass::BitCopy {
                        self.push_instr(Instr::ClosureEnvFieldStore {
                            env: source.env,
                            env_ty: source.env_ty,
                            field_offset: source.field_offset,
                            src,
                        });
                    } else {
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct: format!(
                                    "reassigning owned captured `{name}` inside a closure"
                                ),
                                site: target.site,
                            },
                            note: format!(
                                "captured `{name}` has a non-`BitCopy` type ({:?}); the closure-env \
                                 write-back supports scalar captures only — an owned field would \
                                 need an overwrite-release of its prior value",
                                source.ty
                            ),
                        });
                    }
                } else {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::UnresolvedPlace {
                            binding: *binding,
                            name: name.clone(),
                            site: target.site,
                        },
                        note: format!("assignment target binding {binding:?} has no MIR place"),
                    });
                }
            }
            // Record field-store: `r.x = src` lowers to a GEP+store on the
            // record's alloca (Q297 Stage 1, Q299=(a)). The aggregate `r`
            // stays `Live` after the store — only the named field's bytes
            // are overwritten.
            //
            // Note: the checker-side mutability gate is what restricts the
            // surface to `var`-bound records and `var self`-bound impl
            // methods; reaching MIR with a non-mutable target is impossible
            // (the checker would have already reported and produced
            // Ty::Error / a cascading skip). MIR's role here is purely
            // structural: resolve the field name → offset and emit the
            // store.
            HirExprKind::FieldAccess { object, field } => {
                let Some(record_place) = self.lower_value(object) else {
                    return;
                };
                let object_ty = self.subst_ty(&object.ty);
                // Route the GENERIC arm's outer record name through `short_name`
                // before mangling, exactly as the `StructInit` arm does
                // (`mangle_layout_key(short_name(tname), args)`): registration
                // keys a generic record's layout under the bare outer name
                // (`Holder$$Box`), so a qualified outer spelling must shorten
                // here or the mangled key diverges from the registered one. The
                // monomorphic arm keeps the (possibly qualified) `name` so a
                // same-bare-name record registered under its QUALIFIED key
                // (`widgeti8.Widget` vs `widgeti64.Widget`, divergent layouts)
                // hits its own layout; `lookup_record_field_order` strips the
                // qualifier on a miss for bare-registered monomorphic records.
                let type_name = match &object_ty {
                    ResolvedTy::Named { name, args, .. } if !args.is_empty() => {
                        mangle_layout_key(short_name(name), args)
                    }
                    ResolvedTy::Named { name, .. } => name.clone(),
                    other => {
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::UnsupportedNode {
                                reason: format!(
                                    "field-store on non-named type `{other:?}` (only \
                                     named record types are supported)"
                                ),
                            },
                            note: "field-store target object has an unsupported type".to_string(),
                        });
                        return;
                    }
                };
                let Some(field_order) = self.lookup_record_field_order(type_name.as_str()) else {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::UnsupportedNode {
                            reason: format!(
                                "field-store on unregistered record type `{type_name}`"
                            ),
                        },
                        note: "record type was not found in the field-order table; \
                               this is a checker bug"
                            .to_string(),
                    });
                    return;
                };
                let Some(idx) = field_order.iter().position(|(f, _)| f == field.as_str()) else {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::UnsupportedNode {
                            reason: format!(
                                "field-store on unknown field `{field}` of \
                                 record `{type_name}`"
                            ),
                        },
                        note: "field not found in declaration-order table; \
                               this is a checker bug"
                            .to_string(),
                    });
                    return;
                };
                let field_offset = FieldOffset(
                    u32::try_from(idx).expect("field index exceeds u32::MAX — impossible in Hew"),
                );
                self.push_instr(Instr::RecordFieldStore {
                    record: record_place,
                    field_offset,
                    src,
                });
            }
            // `xs[i] = v` over a `Vec<T>` lowers to the same runtime call that
            // `xs.set(i, v)` emits.
            HirExprKind::Index { container, index }
                if self.subst_ty(&container.ty).is_builtin(BuiltinType::Vec) =>
            {
                let Some(vec_place) = self.lower_value(container) else {
                    return;
                };
                let Some(index_place) = self.lower_value(index) else {
                    return;
                };
                let Some(symbol) =
                    runtime_symbol_for_call_expr(target).map(|(symbol, _, _)| symbol)
                else {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::UnsupportedNode {
                            reason: "Vec index assignment reached MIR without a resolved Vec set \
                                     runtime call"
                                .to_string(),
                        },
                        note: "checker must record the Vec set call at the index target span"
                            .to_string(),
                    });
                    return;
                };
                let next = self.alloc_block();
                let builtin =
                    hew_types::runtime_call::RuntimeCallFamily::from_mir_builtin_symbol(&symbol);
                self.finish_current_block(Terminator::Call {
                    callee: symbol,
                    builtin,
                    args: vec![vec_place, index_place, src],
                    dest: None,
                    next,
                });
                self.start_block(next);
            }
            // `m[k] = v` over a `HashMap<K, V>` lowers to the same
            // `hew_hashmap_insert_layout(map, key, val)` runtime call that
            // `m.insert(k, v)` emits, discarding the returned `bool` (the
            // index-assignment surface has no "was-new" result). The checker
            // accepted this target with value type `V`, so `src` already holds
            // a `V`-typed value. The container/index were NOT pre-lowered (the
            // outer `assign` only lowered `value`), so lower them here.
            HirExprKind::Index { container, index }
                if self
                    .subst_ty(&container.ty)
                    .is_builtin(BuiltinType::HashMap) =>
            {
                let Some(map_place) = self.lower_value(container) else {
                    return;
                };
                let Some(key_place) = self.lower_value(index) else {
                    return;
                };
                let next = self.alloc_block();
                // The callee identity is the typed family; the symbol string
                // is derived from the catalog bijection at construction so
                // the two can never drift.
                let family = hew_types::runtime_call::RuntimeCallFamily::HashMapInsertLayout;
                self.finish_current_block(Terminator::Call {
                    callee: family.c_symbol().to_string(),
                    builtin: Some(family),
                    args: vec![map_place, key_place, src],
                    dest: None,
                    next,
                });
                self.start_block(next);
            }
            _ => self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::UnsupportedNode {
                    reason: "only local bindings, record fields, actor state fields, and \
                         Vec/HashMap index targets are assignable in MIR slice 4"
                        .to_string(),
                },
                note: "assignment target did not lower to a writable place".to_string(),
            }),
        }
    }

    /// Walk an expression, emit checker-stream `MirStatement`s plus
    /// backend-stream `Instr`s, and return the `Place` that holds the
    /// expression's value (or `None` if the construct is outside the
    /// spine subset — a `MirDiagnostic` is recorded in that case).
    #[allow(
        clippy::too_many_lines,
        reason = "single large match on HirExprKind variants; each arm is a fail-closed \
                  boundary rule and splitting would obscure the exhaustiveness requirement"
    )]
    pub(crate) fn lower_value(&mut self, expr: &HirExpr) -> Option<Place> {
        self.decide(expr);
        // Static-pool accessor intercept: `sup.pool[i]` / `.get(i)` / `.len()`.
        // The checker recorded the resolved accessor keyed by this expr's site;
        // route it to the pool ABI before the generic Index/MethodCall paths.
        if let Some(accessor) = self.pool_accessor_sites.get(&expr.site).cloned() {
            return self.lower_pool_accessor(expr, &accessor);
        }
        match &expr.kind {
            HirExprKind::Literal(lit) => self.lower_literal(lit, &expr.ty, expr.site),
            HirExprKind::ContextReader { reader } => {
                let dest = self.alloc_local(self.subst_ty(&expr.ty));
                self.push_instr(Instr::ContextField {
                    dest,
                    offset: context_reader_offset(*reader),
                });
                Some(dest)
            }
            HirExprKind::BindingRef {
                name,
                resolved: ResolvedRef::Binding(id),
            } => {
                if !self.binding_locals.contains_key(id) {
                    if let Some((field_offset, ty)) =
                        self.current_actor_state_fields.get(name).cloned()
                    {
                        let dest = self.alloc_local(ty);
                        // P0 #2432 — fail-closed default; `classify_actor_state_load_modes`
                        // (called once per function over the finalised blocks in
                        // `lower_function`) demotes to `Borrowed` only when it proves
                        // every use of `dest` is a borrow-consumer.
                        self.instructions.push(Instr::ActorStateFieldLoad {
                            field_offset,
                            dest,
                            mode: ActorStateLoadMode::Owned,
                        });
                        return Some(dest);
                    }
                }
                // A `gen`/`gen fn` capture the enclosing `lower_gen_block`
                // already rejected with a root `NotYetImplemented` (an
                // inadmissible opaque/owned value). The synthetic body still
                // references the capture, but it was never materialised into the
                // env record, so resolving it here would stack two cascade
                // secondaries on the root: the `MirStatement::Use` below would
                // read an un-`Bind`-ed binding (→ dataflow
                // `InitialisedBeforeUse`), and place resolution would emit
                // `UnresolvedPlace` (no backend slot). Both are pure cascade;
                // only the root capture rejection is actionable. Fail silent.
                if self.poisoned_capture_ids.contains(id) {
                    return None;
                }
                let use_ty = self.subst_ty(&expr.ty);
                // Skip `MirStatement::Use` for captured bindings: the
                // dataflow checker sees `Use` as a read of `binding_locals`,
                // but captured bindings are NOT in `binding_locals` — they
                // are loaded via `ClosureEnvFieldLoad` below. Emitting `Use`
                // for a capture causes the initialisation checker to report
                // `InitialisedBeforeUse` because the outer binding id was
                // never initialised in this closure-shim context.
                if !self.capture_env_sources.contains_key(id) {
                    // RAII-2 (#1295) call-site downgrade: a by-value resource
                    // argument the HIR over-stamped `Consume` but whose target
                    // free-fn parameter is classified BORROW keeps the caller's
                    // ownership — emit a borrowing `Read` so the binding stays
                    // live and is dropped exactly once at the caller's scope
                    // exit. Consulted at the single resource-arg `Use` emission
                    // point; non-borrow sites (consumed params, unresolved
                    // callees) keep the over-stamped intent unchanged.
                    let use_intent = if self.param_ownership.borrow_arg_sites.contains(&expr.site)
                        || self.bytes_local_share_sites.contains(&expr.site)
                        || self.string_local_share_sites.contains_key(&expr.site)
                    {
                        IntentKind::Read
                    } else {
                        expr.intent
                    };
                    self.statements.push(MirStatement::Use {
                        binding: *id,
                        name: name.clone(),
                        site: expr.site,
                        ty: use_ty.clone(),
                        intent: use_intent,
                    });
                    if use_intent == IntentKind::Consume
                        && self.binding_seeds_drop_elaboration(&use_ty)
                    {
                        // #1933 / #1941 — a non-idempotent user `#[resource]`
                        // with an allocated path-sensitive drop-flag is KEPT in
                        // `owned_locals` so the per-exit `drops_for_exit`
                        // dataflow filter narrows its close per control-flow
                        // path. Mark the flag consumed (set 1) so codegen's
                        // `flag == 0` gate skips the now-callee-owned close on
                        // this path; the dataflow's own `Use{Consume}` transition
                        // (independent of `owned_locals`) still drives the
                        // move-checker and the per-exit `BindingState`. Every
                        // other consumed owned class keeps the legacy
                        // path-insensitive `owned_locals` removal.
                        if let Some(flag) = self.resource_drop_flags.get(id).copied() {
                            self.instructions.push(Instr::ConstI64 {
                                dest: flag,
                                value: 1,
                            });
                        } else if let Some(flag) = self.collection_drop_flags.get(id).copied() {
                            // #2418 — an owned collection local with a
                            // path-sensitive drop-flag is KEPT in
                            // `owned_locals` so a conditional move drops the
                            // value exactly once: mark the flag consumed
                            // (set 1) so codegen's `flag == 0` gate skips the
                            // now-moved-out release on this path, while the
                            // not-moved path keeps `flag == 0` and releases at
                            // scope exit. The dataflow's own `Use{Consume}`
                            // transition still drives the move-checker and the
                            // per-exit `BindingState` narrowing.
                            self.instructions.push(Instr::ConstI64 {
                                dest: flag,
                                value: 1,
                            });
                        } else {
                            self.mark_binding_moved(*id);
                        }
                        // #2523 — a heap-owning projected enum/machine payload
                        // binder is being moved into a new owner. Its storage is
                        // a byte-copy ALIAS of the scrutinee's payload slot.
                        // Whether the copy in the destination local can safely
                        // become the SOLE owner depends on how the scrutinee
                        // reached the match temp (`ProjectedPayloadOrigin`):
                        //   * OwnedBinding — the match moved the binding into the
                        //     temp, so nulling the temp transfers ownership; the
                        //     scrutinee's null-tolerant drop no-ops, and marking
                        //     the binding consumed (`AggregateAlias`) turns a
                        //     later re-read into a compile-time use-after-move.
                        //   * EphemeralTemp — the temp is a fresh sole-owner value
                        //     (`match f()`), so nulling it transfers ownership
                        //     with no re-readable origin to consume-mark.
                        //   * ReadablePlace — the scrutinee was COPIED from a
                        //     re-readable place (`match h.b`, `match pair.0`); the
                        //     origin field's storage stays live and the temp-null
                        //     cannot reach it, so a move-out would leave the field
                        //     dangling (use-after-free / double-free). No sound
                        //     physical neutralization of the origin is expressible
                        //     here, so REJECT the move-out fail-closed before
                        //     codegen (F1). Fires on every consume branch above,
                        //     independent of the binder's own drop-flag tracking.
                        if let Some(provenance) = self.projected_payload_provenance.get(id).cloned()
                        {
                            // A projected heap-payload consumed
                            // inside a fallthrough-capable match-arm guard is
                            // unsound: the neutralize (null store) runs before the
                            // guard outcome is known, so a false guard falls
                            // through to a later arm that re-destructures the
                            // now-null payload (null-fault / abort). Override the
                            // origin to reject fail-closed (unless it already
                            // rejects for a more specific reason). Borrow-only
                            // guards never reach this hook, so they stay valid.
                            let origin = if self.in_fallthrough_match_guard
                                && !matches!(provenance.origin, ProjectedPayloadOrigin::Reject(_))
                            {
                                ProjectedPayloadOrigin::Reject(
                                    ProjectedPayloadRejectReason::GuardedConsume,
                                )
                            } else {
                                provenance.origin
                            };
                            match origin {
                                ProjectedPayloadOrigin::OwnedBinding(scrutinee) => {
                                    self.push_instr(Instr::NeutralizePayloadSlot {
                                        place: provenance.source_place,
                                    });
                                    // #2523 F2 — a PARTIAL-PROJECTION consume-mark:
                                    // the owned scrutinee had one payload field
                                    // moved out. Marks `b` re-read-forbidding
                                    // (`AliasedIntoAggregate`) without the whole-
                                    // value `(t, t)` double-placement check, so a
                                    // second independent field move of the SAME
                                    // scrutinee (`V(x, y) => let wx = x; let wy = y;`)
                                    // is idempotent, not a false use-after-consume.
                                    self.statements.push(MirStatement::AggregateAlias {
                                        binding: scrutinee.binding,
                                        name: scrutinee.name,
                                        site: expr.site,
                                        ty: scrutinee.ty,
                                        partial_projection: true,
                                    });
                                }
                                ProjectedPayloadOrigin::EphemeralTemp => {
                                    self.push_instr(Instr::NeutralizePayloadSlot {
                                        place: provenance.source_place,
                                    });
                                }
                                ProjectedPayloadOrigin::Reject(reason) => {
                                    // Do NOT emit the unsound temp-neutralize —
                                    // its source cannot be safely neutralized, so
                                    // reject the move-out fail-closed (F1/F1b/F2).
                                    let note = match reason {
                                        ProjectedPayloadRejectReason::ReadablePlace => {
                                            "the matched place keeps ownership of the \
                                             payload, so moving it out would leave the \
                                             field's storage dangling (use-after-free on \
                                             re-read, double-free at the place's drop); \
                                             match an owned value instead"
                                        }
                                        ProjectedPayloadRejectReason::CapturedBinding => {
                                            "the matched binding is captured by this \
                                             closure and read from the closure environment \
                                             by copy, so moving the payload out would leave \
                                             the captured copy dangling (double-free when \
                                             the environment drops); move the value into \
                                             the closure and match it there, or match an \
                                             owned value the closure does not capture"
                                        }
                                        ProjectedPayloadRejectReason::NestedDestructure => {
                                            "the payload is extracted from a nested pattern \
                                             through a temporary copy the move cannot \
                                             neutralize, so moving it out would leave the \
                                             outer value's storage dangling (double-free / \
                                             leak at the outer value's drop); bind the \
                                             nested value first, then match that owned \
                                             binding in a separate step"
                                        }
                                        ProjectedPayloadRejectReason::GuardedConsume => {
                                            "the payload is consumed inside a match-arm \
                                             guard that can fall through, so the move-out \
                                             would run before the guard result is known; a \
                                             false guard would then fall through to a later \
                                             arm that re-reads the now-moved payload \
                                             (null-fault at runtime); consume the payload in \
                                             the arm body instead of the guard, or match an \
                                             owned value in a separate step after the guard"
                                        }
                                        ProjectedPayloadRejectReason::AliasesCallerStorage => {
                                            "the scrutinee produces a value that may alias \
                                             caller-visible storage (a call forwarding a \
                                             by-value heap parameter, an aggregate over a \
                                             re-readable heap place, or a borrowed collection \
                                             getter), not a fresh sole owner, so moving the \
                                             payload out would leave that storage dangling \
                                             (use-after-free on a re-read, double-free at its \
                                             drop); construct the value fresh at the \
                                             scrutinee, or bind the call result to a `let` \
                                             and match the binding"
                                        }
                                    };
                                    self.diagnostics.push(MirDiagnostic {
                                        kind:
                                            MirDiagnosticKind::ProjectedPayloadMoveFromReadablePlace {
                                                binding: *id,
                                                name: provenance.binder_name,
                                                site: expr.site,
                                                reason,
                                            },
                                        note: note.to_string(),
                                    });
                                }
                            }
                        }
                    }
                }
                if let Some(source) = self.capture_env_sources.get(id).cloned() {
                    let dest = self.alloc_local(source.ty.clone());
                    self.push_instr(Instr::ClosureEnvFieldLoad {
                        env: source.env,
                        env_ty: source.env_ty,
                        field_offset: source.field_offset,
                        dest,
                    });
                    return Some(dest);
                }
                let place = self.binding_locals.get(id).copied();
                if place.is_none() {
                    if self.poisoned_let_bindings.contains(id) {
                        // The binding's `let` initializer already failed to lower
                        // and reported the root error; this read is pure cascade.
                        // Stay silent (the compile already fails) instead of
                        // stacking an `UnresolvedPlace` follow-on.
                        return None;
                    }
                    // Function parameters and other bindings without a
                    // backend slot are out of Cluster 1's spine. Without a
                    // Place, the emitter would silently load an
                    // uninitialised return slot — fail closed here.
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::UnresolvedPlace {
                            binding: *id,
                            name: name.clone(),
                            site: expr.site,
                        },
                        note: "binding has no backend slot in the Cluster 1 spine \
                               (function parameters and captured bindings are not \
                               yet lowered)"
                            .to_string(),
                    });
                }
                place
            }
            HirExprKind::BindingRef {
                name: _,
                resolved: ResolvedRef::Const(item_id),
            } => {
                let dest = self.alloc_local(self.subst_ty(&expr.ty));
                self.push_instr(Instr::ConstGlobalLoad {
                    item_id: *item_id,
                    dest,
                });
                Some(dest)
            }
            // Named top-level function used as a first-class value.
            // Synthesise a ClosureInvoke-ABI shim that forwards user args
            // to the original function, then package it as a closure pair
            // with a null (Unit) env. The shim body never loads from the
            // env_ptr, so a null or garbage env_ptr is safe at call time.
            //
            // WHY: the uniform closure-call path (lower_call_closure) needs
            // a (fn_ptr, env_ptr) pair regardless of whether the callee
            // captures anything. Named functions have no captures, so the
            // env_ptr is a dummy.
            // WHEN-OBSOLETE: if Hew gains a dedicated fn-pointer ABI.
            // WHAT-REAL: emit a native fn-pointer pair without the env slot.
            HirExprKind::BindingRef {
                name,
                resolved: ResolvedRef::Item(_),
            } if matches!(&expr.ty, ResolvedTy::Function { .. }) => {
                // Resolve the function's symbol. Same-module and cross-module
                // non-generic named functions are supported (the HIR lowerer
                // emits the qualified mangled symbol for the cross-module
                // case); anything else fails closed with a NotYetImplemented
                // diagnostic.
                let fn_symbol = if self.module_fn_names.contains(name.as_str()) {
                    name.clone()
                } else {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::NotYetImplemented {
                            construct: format!(
                                "named function `{name}` used as a value (only non-generic \
                                 named functions are currently supported)"
                            ),
                            site: expr.site,
                        },
                        note: "generic named functions as values are not yet implemented in \
                               the current spine"
                            .to_string(),
                    });
                    return None;
                };
                let (param_tys, fn_ret_ty) = match &expr.ty {
                    ResolvedTy::Function { params, ret } => (params.clone(), (**ret).clone()),
                    _ => unreachable!("guard above ensures Function ty"),
                };
                let shim_name = format!(
                    "__hew_named_fn_invoke_{}",
                    Self::sanitize_symbol_component(&fn_symbol)
                );
                // Emit the shim only once per named fn (dedup by shim_name).
                if !self
                    .generated_functions
                    .iter()
                    .any(|f| f.raw.name == shim_name)
                {
                    let shim = self
                        .lower_named_fn_invoke_shim(&fn_symbol, &shim_name, &param_tys, &fn_ret_ty);
                    self.generated_functions.push(shim);
                }
                // Null-env pair: the shim ignores env_ptr entirely (zero
                // loads), and a genuine null — not a dummy frame address —
                // is what the closure-pair drop protocol checks before
                // dereferencing the env's free-thunk slot. The Unit local
                // is a placeholder operand only; `ClosureEnvMode::Null`
                // makes codegen store a null pointer constant.
                let null_env = self.alloc_local(ResolvedTy::Unit);
                let dest = self.alloc_local(self.subst_ty(&expr.ty));
                self.push_instr(Instr::MakeClosure {
                    fn_symbol: shim_name,
                    env: null_env,
                    dest,
                    env_mode: crate::model::ClosureEnvMode::Null,
                });
                Some(dest)
            }
            // A typed runtime builtin used as a first-class value
            // (`let f = link;`). There is no fn-pointer ABI for catalog
            // builtins (their lowering synthesizes extra ABI args such as
            // the implicit self handle), so this fails closed with an
            // explicit diagnostic instead of silently producing no value.
            HirExprKind::BindingRef {
                name,
                resolved: ResolvedRef::Builtin(family),
            } => {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!(
                            "runtime builtin `{name}` ({symbol}) used as a value; builtins \
                             are callable only in direct call position",
                            symbol = family.c_symbol(),
                        ),
                        site: expr.site,
                    },
                    note: "runtime builtins have no fn-pointer ABI: their call lowering \
                           synthesizes implicit ABI arguments that a first-class value \
                           cannot carry"
                        .to_string(),
                });
                None
            }
            // Catch-all for any other BindingRef shape not explicitly handled
            // above (e.g. unresolved refs, struct items used in expression
            // position before HIR checker gates them). Returns None so the
            // caller sees a missing-value signal. Diagnostics for these cases
            // are produced by the HIR checker; MIR need not repeat them.
            HirExprKind::BindingRef { .. } => None,
            HirExprKind::Binary { op, left, right } => {
                // Short-circuit logical operators must intercept BEFORE the rhs
                // is lowered: evaluating `right` unconditionally would break
                // the short-circuit contract (rhs side effects would run even
                // when lhs already determines the result).
                match op {
                    BinaryOp::And => return self.lower_logical_and(left, right, &expr.ty),
                    BinaryOp::Or => return self.lower_logical_or(left, right, &expr.ty),
                    _ => {}
                }
                let lhs = self.lower_value(left);
                let rhs = self.lower_value(right);
                match (lhs, rhs) {
                    (Some(lhs), Some(rhs)) => {
                        self.lower_binary(*op, lhs, rhs, &left.ty, &right.ty, &expr.ty, expr.site)
                    }
                    _ => None,
                }
            }
            HirExprKind::Unary {
                op,
                operand,
                operand_ty,
            } => self.lower_unary(*op, operand, operand_ty, &expr.ty, expr.site),
            HirExprKind::NumericCast {
                value,
                from_ty,
                to_ty,
            } => {
                let src = self.lower_value(value)?;
                let from_ty = self.subst_ty(from_ty);
                let to_ty = self.subst_ty(to_ty);
                if !from_ty.can_explicitly_numeric_cast_to(&to_ty) {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::UnsupportedNode {
                            reason: format!(
                                "numeric cast from {} to {} is outside the checker-admitted matrix",
                                from_ty.user_facing(),
                                to_ty.user_facing()
                            ),
                        },
                        note: "HIR NumericCast carried a non-numeric cast; the HIR verifier should have rejected it"
                            .to_string(),
                    });
                    return None;
                }
                let dest = self.alloc_local(to_ty.clone());
                self.push_instr(Instr::NumericCast {
                    dest,
                    src,
                    from_ty,
                    to_ty,
                });
                Some(dest)
            }
            HirExprKind::SaturatingWidthCast {
                value,
                from_ty,
                to_ty,
            } => {
                let src = self.lower_value(value)?;
                let from_ty = self.subst_ty(from_ty);
                let to_ty = self.subst_ty(to_ty);
                if !from_ty.is_integer() || !to_ty.is_integer() {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::UnsupportedNode {
                            reason: format!(
                                "saturating width cast from {} to {} requires both types to be integers",
                                from_ty.user_facing(),
                                to_ty.user_facing()
                            ),
                        },
                        note: "HIR SaturatingWidthCast carried a non-integer type; the HIR verifier should have rejected it"
                            .to_string(),
                    });
                    return None;
                }
                let dest = self.alloc_local(to_ty.clone());
                self.instructions.push(Instr::SaturatingWidthCast {
                    dest,
                    src,
                    from_ty,
                    to_ty,
                });
                Some(dest)
            }
            HirExprKind::TryWidthCast {
                value,
                from_ty,
                to_ty,
                kind,
            } => {
                let src = self.lower_value(value)?;
                let from_ty = self.subst_ty(from_ty);
                let to_ty = self.subst_ty(to_ty);
                if !from_ty.is_numeric() || !to_ty.is_numeric() {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::UnsupportedNode {
                            reason: format!(
                                "try-width cast from {} to {} requires numeric types",
                                from_ty.user_facing(),
                                to_ty.user_facing()
                            ),
                        },
                        note: "HIR TryWidthCast carried a non-numeric type; the HIR verifier should have rejected it"
                            .to_string(),
                    });
                    return None;
                }
                let dest = self.alloc_local(self.subst_ty(&expr.ty));
                self.instructions.push(Instr::TryWidthCast {
                    dest,
                    src,
                    from_ty,
                    to_ty,
                    kind: *kind,
                });
                Some(dest)
            }
            HirExprKind::TupleLiteral { elements } => {
                // Lower each element expression to a MIR Place.
                let lowered_elements: Vec<Place> = elements
                    .iter()
                    .map(|elem| self.lower_value(elem))
                    .collect::<Option<Vec<_>>>()?;

                // B1: an owned single-owner element is MOVED into the tuple —
                // mark its source binding aliased so a later use is rejected at
                // CHECK without disturbing the drop machinery.
                for elem in elements {
                    self.alias_moved_owned_operand(elem);
                    self.enforce_closure_pair_ingress(elem);
                }

                // Allocate a local for the tuple result.
                let dest = self.alloc_local(self.subst_ty(&expr.ty));

                // Emit the TupleConstruct instruction.
                self.push_instr(Instr::TupleConstruct {
                    elements: lowered_elements,
                    dest,
                });

                Some(dest)
            }
            HirExprKind::NumericMethod {
                receiver,
                arg,
                family,
                op,
                signedness,
                width,
                ..
            } => {
                let lhs = self.lower_value(receiver);
                let rhs = self.lower_value(arg);
                let (Some(lhs), Some(rhs)) = (lhs, rhs) else {
                    return None;
                };
                let dest = self.alloc_local(self.subst_ty(&expr.ty));
                let op = numeric_method_op(*op);
                let signed = numeric_method_signedness(*signedness);
                match *family {
                    NumericMethodFamily::Wrapping => {
                        let instr = match op {
                            IntArithOp::Add => Instr::IntAdd { dest, lhs, rhs },
                            IntArithOp::Sub => Instr::IntSub { dest, lhs, rhs },
                            IntArithOp::Mul => Instr::IntMul { dest, lhs, rhs },
                        };
                        self.push_instr(instr);
                    }
                    NumericMethodFamily::Checked => {
                        self.push_instr(Instr::IntArithCheckedOption {
                            op,
                            signed,
                            width: *width,
                            dest,
                            lhs,
                            rhs,
                        });
                    }
                    NumericMethodFamily::Saturating => {
                        self.push_instr(Instr::IntArithSaturating {
                            op,
                            signed,
                            width: *width,
                            dest,
                            lhs,
                            rhs,
                        });
                    }
                }
                Some(dest)
            }
            HirExprKind::CancellationTokenIsCancelled { receiver } => {
                let token = self.lower_value(receiver)?;
                let dest = self.alloc_local(ResolvedTy::Bool);
                self.instructions
                    .push(Instr::CancellationTokenIsCancelled { dest, token });
                Some(dest)
            }
            HirExprKind::GeneratorNext { receiver, yield_ty } => {
                let ctx = self.lower_value(receiver)?;
                // `expr.ty` is the checker-authoritative `Option<yield_ty>`; the
                // dest enum slot is allocated with that exact type so codegen
                // resolves the registered Option layout for the unbox.
                let dest = self.alloc_local(expr.ty.clone());
                self.push_instr(Instr::GeneratorNext {
                    dest,
                    ctx,
                    yield_ty: yield_ty.clone(),
                });
                Some(dest)
            }
            HirExprKind::WireCodec {
                direction,
                operand,
                value_ty,
            } => {
                let operand_place = self.lower_value(operand)?;
                // `expr.ty` is checker-authoritative: `bytes` for encode, the
                // wire-struct type for decode. Allocate the dest with that type
                // so codegen resolves the right slot layout.
                let dest = self.alloc_local(expr.ty.clone());
                self.push_instr(Instr::WireCodec {
                    dest,
                    operand: operand_place,
                    direction: *direction,
                    value_ty: value_ty.clone(),
                });
                Some(dest)
            }
            HirExprKind::Call { callee, args } => {
                if let Some((symbol, args, site)) = runtime_symbol_for_call_expr(expr) {
                    return self.lower_runtime_call(
                        &symbol,
                        args,
                        site,
                        RuntimeCallContext::ValueNeeded,
                        // The call's checker-recorded result type sizes any
                        // value-context dest local (`checker-authority`: the
                        // producer consumes the recorded type, never re-infers
                        // it). `.send` uses it to allocate the
                        // `Result<(), SendError>` slot.
                        Some(&expr.ty),
                    );
                }
                // M2 lambda-actor call-syntax dispatch.
                //
                // A user `let log = actor |s|{..}; log("hi")` produces a
                // binding `log` whose MIR `Place` is `LambdaActorHandle(N)`
                // and whose HIR type is `LambdaPid<Msg, Reply>`. Two problems
                // collide here without an early intercept:
                //
                // 1. `log` is also the name of a `stdlib_catalog` math
                //    builtin (`f64 -> f64`). The `module_fn_names` lookup
                //    below matches on bare name only, so without this
                //    early guard the call would dispatch through
                //    `lower_direct_call("log")` → wrong-typed math call →
                //    LLVM verifier rejects (`expected f64, got ptr`).
                //    This is a real miscompile, not just an NYI.
                // 2. The non-collision cases (`dbl(5)`, `fib(10)`) would
                //    fall through to the indirect-call NYI arm. They
                //    need the same lambda-actor dispatch.
                //
                // The intercept is gated on the binding's MIR Place,
                // NOT on the type alone: a raw `Duplex<>`-typed binding that
                // was built from `duplex` / `duplex_pair` lives in a generic
                // `Place::DuplexHandle`, not a `LambdaActorHandle`, and
                // its call surface is `.send()` / `.recv()` method calls,
                // not call-syntax. The Place-variant guard is the
                // canonical "this is a lambda-actor handle" signal.
                if let HirExprKind::BindingRef {
                    resolved: ResolvedRef::Binding(binding_id),
                    ..
                } = &callee.kind
                {
                    if matches!(
                        self.binding_locals.get(binding_id),
                        Some(Place::LambdaActorHandle(_))
                    ) {
                        return self.lower_lambda_actor_call(callee, args, &expr.ty, expr.site);
                    }
                    // Body-side captured-handle dispatch: inside a lambda-actor
                    // body, the forward-bound self binding (and any captured
                    // lambda-actor handle) resolves through `capture_env_sources`,
                    // not `binding_locals`. The callee's `LambdaPid` type plus the
                    // env-source entry is the routing signal — the loaded env
                    // field is the handle value.
                    if self.capture_env_sources.contains_key(binding_id)
                        && matches!(
                            &callee.ty,
                            ResolvedTy::Named { name, .. } if name == "LambdaPid"
                        )
                    {
                        return self.lower_lambda_actor_call(callee, args, &expr.ty, expr.site);
                    }
                }
                // SHIM(E2→checker): user functions are still identified by
                // callee name membership in `module_fn_names` until HIR threads
                // resolved item/builtin variants through the bridge.
                let callee_name = match &callee.kind {
                    HirExprKind::BindingRef { name, resolved } => Some((name.as_str(), *resolved)),
                    _ => None,
                };
                if let Some((name, callee_resolved)) = callee_name {
                    // Generic top-level user fn: HIR recorded
                    // `call_site_type_args[expr.site]` with the type
                    // arguments observed at this call site (possibly
                    // including the enclosing fn's type-parameter
                    // symbols when this call is inside a generic
                    // body). Substitute via this Builder's monomorph
                    // substitution map and dispatch to the
                    // per-instantiation mangled symbol.
                    if let Some(type_args) = self.call_site_type_args.get(&expr.site).cloned() {
                        let substituted: Vec<ResolvedTy> =
                            type_args.iter().map(|t| self.subst_ty(t)).collect();
                        let mangled = hew_hir::monomorph::mangle(name, &substituted);
                        // If the mangled symbol is in `module_fn_names`,
                        // a per-instantiation MIR function was emitted
                        // by `lower_hir_module`; dispatch to it
                        // directly. Otherwise fall through to the
                        // unmangled lookup — the unspecialised origin
                        // is being lowered in a context where no
                        // monomorphisation was registered (e.g. when
                        // tests directly invoke `lower_hir_module`
                        // with a HirModule that bypassed the producer).
                        if self.module_fn_names.contains(&mangled) {
                            let ret_ty = self.subst_ty(&expr.ty);
                            return self
                                .lower_direct_call(&mangled, None, args, &ret_ty, expr.site);
                        }
                    }
                    // User-defined function in the same module: emit a call terminator.
                    // The callee symbol is the bare function name as declared;
                    // codegen resolves it against the module's fn_symbols table.
                    if self.module_fn_names.contains(name) {
                        // Thread the typed builtin resolution (if any) so the
                        // suspend-vs-blocking classification keys on the
                        // checker-resolved family, never on the symbol string.
                        let builtin = match callee_resolved {
                            ResolvedRef::Builtin(family) => Some(family),
                            _ => None,
                        };
                        return self.lower_direct_call(name, builtin, args, &expr.ty, expr.site);
                    }
                }
                if matches!(
                    callee.kind,
                    HirExprKind::BindingRef {
                        resolved: ResolvedRef::Item(_) | ResolvedRef::Builtin(_),
                        ..
                    }
                ) {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::NotYetImplemented {
                            construct: "function call".to_string(),
                            site: expr.site,
                        },
                        note: "resolved callee has no MIR body or runtime lowering in the current cutover spine"
                            .to_string(),
                    });
                    return None;
                }
                if matches!(
                    callee.ty,
                    ResolvedTy::Function { .. } | ResolvedTy::Closure { .. }
                ) {
                    let ret_ty = self.subst_ty(&expr.ty);
                    if ty_is_generator_handle(&ret_ty) {
                        self.reject_unproven_generator_fn_args(args);
                    }
                    // Suspendable-callee discriminator: a call to a binding that
                    // holds a closure whose body `await`s across the coroutine
                    // boundary drives the callee coroutine and PROPAGATES its
                    // suspension into this caller — lowered to the driving
                    // `Terminator::SuspendingCallClosure`. Only fires inside a
                    // suspendable caller (one whose call-conv carries the
                    // execution context): a `Default` caller has no parkable
                    // continuation, so a suspending closure cannot be driven
                    // there and the existing direct path (which fails closed in
                    // codegen) is kept.
                    let callee_suspends =
                        self.current_function_call_conv.carries_execution_context()
                            && matches!(
                                &callee.kind,
                                HirExprKind::BindingRef {
                                    resolved: ResolvedRef::Binding(id),
                                    ..
                                } if self.suspending_closure_bindings.contains(id)
                            );
                    let callee_place = self.lower_value(callee)?;
                    let mut arg_places = Vec::with_capacity(args.len());
                    for arg in args {
                        arg_places.push(self.lower_value(arg)?);
                    }
                    let dest = if matches!(ret_ty, ResolvedTy::Unit) {
                        None
                    } else {
                        Some(self.alloc_local(ret_ty.clone()))
                    };
                    if callee_suspends {
                        // The driver rides the multi-suspend epilogue: `cleanup`
                        // reuses `resume` exactly as `SuspendingRead`/`Ask` do
                        // (the carrier owns no separate MIR cleanup block).
                        let next = self.alloc_block();
                        self.record_suspend_kind(SuspendKind::CallClosure {
                            callee: callee_place,
                            args: arg_places.clone(),
                            ret_ty: ret_ty.clone(),
                            result_dest: dest,
                        });
                        self.finish_current_block(Terminator::Suspend {
                            resume: next,
                            cleanup: next,
                            is_final: false,
                        });
                        self.start_block(next);
                        return dest;
                    }
                    self.push_instr(Instr::CallClosure {
                        callee: callee_place,
                        args: arg_places,
                        ret_ty,
                        dest,
                    });
                    return dest;
                }
                // Indirect calls (closures, higher-order function values,
                // or unresolved bindings): not yet supported. Walk the children
                // so any Unsupported inside an argument still surfaces, then
                // fail closed so the emitter never sees a return slot with no
                // producer (LESSONS `boundary-fail-closed`).
                let _ = self.lower_value(callee);
                for arg in args {
                    let _ = self.lower_value(arg);
                }
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "indirect or unresolved function call".to_string(),
                        site: expr.site,
                    },
                    note: "only direct calls to module-declared user functions and \
                           runtime-ABI builtins are supported; indirect/closure/\
                           higher-order calls are not yet lowered"
                        .to_string(),
                });
                None
            }
            HirExprKind::Block(block) => {
                // Every nested statement reaches the checker-authority
                // stream via `self.stmt`, not just `HirStmtKind::Expr`.
                // Forwarding only `Expr` here would silently drop nested
                // `let` / `return` statements from a block expression and
                // let a real `UseAfterConsume` / `InitialisedBeforeUse`
                // pattern slip past the move-checker (fail-closed gap).
                // The HIR-Block-as-expression case recurses through this
                // arm — `If` / `StructInit` / `Call` / `Binary` lower
                // their nested expressions via `lower_value`, so a block
                // embedded in any of those forms reaches this arm and is
                // lowered the same way.
                self.active_scopes.push(block.scope);
                for stmt in &block.statements {
                    self.stmt(stmt);
                }
                // Secure the block's tail value into a fresh local BEFORE
                // running this scope's defers. Q205-B: defers observe
                // bindings at scope-exit time, so a defer that mutates a
                // `var` named by the tail expression would corrupt the
                // block's value if the consumer read the original Place
                // after the defer ran. Materialising the Move into a
                // dedicated local locks in the result; the defer body
                // may still mutate the source binding, but the block's
                // observable value Place is untouched.
                let result = if let Some(tail) = block.tail.as_ref() {
                    if let Some(src) = self.lower_value(tail) {
                        let secured = self.alloc_local(self.subst_ty(&tail.ty));
                        self.push_instr(Instr::Move { dest: secured, src });
                        Some(secured)
                    } else {
                        None
                    }
                } else {
                    None
                };
                // Materialize defers registered for this scope in LIFO order.
                // Runs after the tail expression's value has been secured
                // into a fresh local so cleanup cannot corrupt the block's
                // observable result.
                self.emit_pending_defers(block.scope);
                // Release any generator handle declared in this block's scope
                // before it closes — so a `for x in gen()` block nested in an
                // enclosing loop frees its `__hew_for_iter_*` coro frame + heap companion
                // every outer iteration instead of leaking one per iteration.
                self.emit_scope_generator_drops(block.scope);
                // #1949 — release any sole-owner `for x in …` cursor (`VecIter`)
                // declared in this block's scope before it closes, so a cursor in
                // an enclosing-loop body frees its `vec` handle every outer
                // iteration instead of leaking one per iteration (the generator
                // analogue above).
                self.emit_scope_vec_iter_drops(block.scope);
                // 3b-1 — close any `Stream<T>` / `Receiver<T>` for-await cursor
                // declared in this block's scope before it closes. `break` and
                // the synthesized `None`-arm exit both land in the post-loop
                // merge INSIDE the desugar block, so this fires the stream close
                // before the enclosing function continues — waking a parked
                // producer and preventing the deadlock.
                self.emit_scope_stream_drops(block.scope);
                self.active_scopes.pop();
                result
            }
            HirExprKind::If {
                condition,
                then_expr,
                else_expr,
            } => self.lower_if(condition, then_expr, else_expr.as_deref(), &expr.ty),
            HirExprKind::StructInit {
                name, fields, base, ..
            } => {
                // Resolve the record-key for the field-order table. For a
                // generic record instantiation the HIR-recorded `expr.ty`
                // is `Named { name, args: <concrete> }` and the layout was
                // registered under the mangled name; for a monomorphic
                // record `args` is empty and the bare name is the key.
                //
                // A bare construction (`Widget { … }`) constrained by a
                // module-qualified expected type carries the QUALIFIED name on
                // `expr.ty` (the HIR lowering stamps it from the checker-recorded
                // type). Prefer that qualified name so the lookup hits the
                // per-module layout when two packages export a same-bare-name
                // type. A non-colliding type keeps a bare layout key, but
                // `lookup_record_field_order` strips the module prefix on a miss,
                // so a qualified `expr.ty` still resolves the bare entry. A
                // single-module construction never carries a dotted name and
                // falls through to the bare syntactic `name` byte-identically.
                let expr_ty = self.subst_ty(&expr.ty);
                let record_key = match &expr_ty {
                    ResolvedTy::Named {
                        name: tname, args, ..
                    } if !args.is_empty() => mangle_layout_key(short_name(tname), args),
                    ResolvedTy::Named {
                        name: tname,
                        args,
                        builtin: None,
                        ..
                    } if args.is_empty() && tname.contains('.') => tname.clone(),
                    _ => name.clone(),
                };
                // Look up the declaration-order field list for this record.
                // If it's missing, the checker allowed a type that was never
                // registered — fail closed rather than silently producing
                // malformed MIR.
                let field_order = if let Some(order) = self.lookup_record_field_order(&record_key) {
                    order.clone()
                } else {
                    // Walk sub-expressions for checker-stream coverage.
                    for (_, fexpr) in fields {
                        let _ = self.lower_value(fexpr);
                    }
                    if let Some(base_expr) = base {
                        let _ = self.lower_value(base_expr);
                    }
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::NotYetImplemented {
                            construct: format!(
                                "record type `{name}` (not registered in field-order table)"
                            ),
                            site: expr.site,
                        },
                        note: "record type was not found in the field-order table; \
                               this is a checker bug (the type must be declared before use)"
                            .to_string(),
                    });
                    return None;
                };

                // ── Functional-update base is CONSUMED ──────────────────────
                // Owned-record `..base` moves the base into the new record:
                // its carried fields escape via `RecordFieldLoad` and its
                // OVERRIDDEN owned fields are destructively released at the
                // construction site (below). Two fail-closed guards keep the
                // consume sound, so every admitted program is memory-safe:
                //
                //   (1) Self-reference reject (here): an overriding field value
                //       that bare-aliases the base's heap (`{ items: s.items,
                //       ..s }`) would be freed by the override-drop before the
                //       new record reads it. Reject at lowering.
                //
                //   (2) Use-after-move (consume-marking after the base is
                //       lowered, below): any later use of the base — including a
                //       second `..base` from the same source, or a `base.field`
                //       read — is flagged `UseAfterConsume` by the move-checker.
                //
                // The long-term value model targets COW (`cow_share` +
                // `ensure_unique`, base stays valid — see
                // tests/corpus/v05-value-model/18_record_update_syntax) where
                // these shapes become legal. Until the retain-on-share spine
                // lands, the consume semantics are the fail-closed interim: the
                // rejected shapes are exactly the ones that would otherwise
                // miscompile (use-after-free / double-free). BitCopy records are
                // exempt — they bit-copy and the base stays valid.
                let base_binding: Option<BindingId> = match base.as_deref().map(|b| &b.kind) {
                    Some(HirExprKind::BindingRef {
                        resolved: ResolvedRef::Binding(id),
                        ..
                    }) => Some(*id),
                    _ => None,
                };
                if let Some(base_id) = base_binding {
                    if let Some((fname, _)) = fields.iter().find(|(_, fexpr)| {
                        self.functional_update_value_aliases_base(fexpr, base_id)
                    }) {
                        // Walk every sub-expression for checker-stream coverage
                        // before bailing, mirroring the field-order-miss path.
                        for (_, fe) in fields {
                            let _ = self.lower_value(fe);
                        }
                        if let Some(base_expr) = base.as_deref() {
                            let _ = self.lower_value(base_expr);
                        }
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct: "functional-update override aliasing the consumed base"
                                    .to_string(),
                                site: expr.site,
                            },
                            note: format!(
                                "field `{fname}` of `{name}` is initialised from a bare \
                                 projection of the functional-update base `..base`, which is \
                                 consumed by the update; the base's overridden owned fields \
                                 are released at the construction site, so the new record \
                                 would alias freed memory. Clone the value \
                                 (`<base>.<field>.clone()`) or bind it into a separate \
                                 variable before the update. (The COW value model that keeps \
                                 the base live after an update is not yet implemented.)"
                            ),
                        });
                        return None;
                    }
                }

                // (1b) Fail-closed ALLOWLIST gate for the destructive base.
                // The override-drop below frees an overridden owned field of
                // `base` IN PLACE, and the non-overridden owned fields escape
                // via shallow `RecordFieldLoad`. Both are sound ONLY when the
                // base is the UNIQUE live owner of its heap fields. Rather than
                // denylist the unsafe projection shapes (a list that has
                // repeatedly missed cases — `FieldAccess`, then `Index`, then
                // `TupleIndex`, then a bare binding REBOUND from a projection —
                // each a fresh use-after-free), the base must POSITIVELY prove
                // safe via `base_is_safe_for_destructive_funcupdate`: a bare
                // binding whose PROVENANCE proves unique ownership (every
                // definition a materialised owner — consume-marked in place), or
                // a directly-materialised owner with no live alias (call /
                // `.clone()` result, record literal, `Vec` element `v[i]`, or a
                // projection rooted at one). Any other base — a projection of a
                // LIVE binding (`o.inner`, `t.0`, `o.pair.0`, nested), a bare
                // binding bound from such a projection (`let b = o.inner; ..b`),
                // a machine-state field (`self.field`), a `Const`/`Item` ref, a
                // deref, or any future expression form — is rejected. This is
                // complete by construction: no base shape can slip the gate.
                //
                // Only OWNED-aggregate bases reach the override-drop / shallow-
                // carry path, so the gate is type-fenced by
                // `aggregate_ingress_moves_binding_ty`: a `BitCopy` base
                // bit-copies and stays valid regardless of shape.
                if let Some(base_expr) = base.as_deref() {
                    let base_ty = self.subst_ty(&base_expr.ty);
                    if self.aggregate_ingress_moves_binding_ty(&base_ty)
                        && !self.base_is_safe_for_destructive_funcupdate(base_expr)
                    {
                        // Walk every sub-expression for checker-stream
                        // coverage before bailing, mirroring the paths above.
                        for (_, fe) in fields {
                            let _ = self.lower_value(fe);
                        }
                        let _ = self.lower_value(base_expr);
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct:
                                    "functional-update base that is not a binding or owned value"
                                        .to_string(),
                                site: expr.site,
                            },
                            note: format!(
                                "the `..base` of `{name}` is not provably the unique owner of \
                                 its heap fields, so it may interior-alias storage that stays \
                                 live after the update — a field projection of a live binding \
                                 (`b.field`, `b.0`, `t.0.field`), a machine-state field \
                                 (`self.field`), a binding REBOUND from such a projection \
                                 (`let b = o.inner; ..b` — `b` shares `o.inner`'s storage), or \
                                 another aliasing shape. The update's in-place release of an \
                                 overridden owned field (and the shallow carry of the \
                                 non-overridden owned fields) would then free memory the live \
                                 owner still references — a use-after-free, and a double-free at \
                                 its scope-exit drop. Clone the base into a fresh owned value \
                                 (`{name} {{ ..<base>.clone(), <field>: new }}`), or clone the \
                                 overridden field. (Binding the projection first — \
                                 `let b = <base>` — does NOT help: it re-aliases the same \
                                 storage; clone or consume the source instead. Accepted: a bare \
                                 binding whose every definition is a freshly-owned value — a call \
                                 result, a `.clone()`, a record literal, a `Vec` element `v[i]`, \
                                 or a move-chain of those (this admits the reassign-loop idiom) \
                                 — and an owned-rvalue base directly.) The COW value model that \
                                 keeps a projected source live after an update is not yet \
                                 implemented."
                            ),
                        });
                        return None;
                    }
                }

                // Lower each explicit field value to a Place, keyed by name.
                let mut explicit: HashMap<String, Place> = HashMap::new();
                for (fname, fexpr) in fields {
                    if let Some(place) = self.lower_value(fexpr) {
                        explicit.insert(fname.clone(), place);
                    }
                }

                // An explicit owned field operand is moved into the record just
                // like a tuple element is moved into a tuple. Mark the source
                // binding in the checker stream so a later use is rejected
                // without changing the drop elaborator's alias-aware inputs.
                // Closure-pair operands additionally pass the sole-owner
                // ingress gate (owned binding → move; borrow → refuse).
                for (_, fexpr) in fields {
                    self.alias_moved_owned_operand(fexpr);
                    self.enforce_closure_pair_ingress(fexpr);
                }

                // Lower the functional-update base, if any.
                let base_place: Option<Place> = if let Some(base_expr) = base {
                    let place = self.lower_value(base_expr);
                    // (2) Consume the base — see the guard note above. An owned
                    // record handed in via `..base` moves into the new record,
                    // so mark the source binding consumed: a later use (a second
                    // `..base`, or a `base.field` read) is `UseAfterConsume`.
                    // `alias_moved_owned_operand` is drop-neutral (it does NOT
                    // suppress the base's scope-exit drop) and self-skips BitCopy
                    // records via `aggregate_ingress_moves_binding_ty`.
                    self.alias_moved_owned_operand(base_expr);
                    place
                } else {
                    None
                };

                // Build the (offset, source) pairs in declaration order.
                // For each field: use the explicit value if present; otherwise
                // emit a RecordFieldLoad from the base and use that intermediate.
                //
                // For OVERRIDDEN fields with heap-owning types (string / bytes /
                // Vec<T> / HashMap / HashSet / Generator), destructively release
                // the OLD base value at the construction site: single-pointer COW
                // fields via `RecordFieldDrop`, the fat `bytes` triple via
                // `RecordFieldLoad` + inline `Instr::Drop` (see the per-field
                // routing comment below). Without this release the overwritten
                // allocation is orphaned — the functional-update
                // overridden-owned-field LEAK (the bug the leak oracle pins).
                // The release is now correct for single-pointer COW leaf fields;
                // owned-aggregate overrides (record / tuple / enum) remain a
                // follow-on guarded by the fail-closed pre-flight below.
                //
                // SOUNDNESS depends on `..base` consuming the base: the base is
                // marked consumed (above), so the move-checker rejects any later
                // read of `base` (a second `..base`, a `base.field`). The old
                // value freed here therefore has no surviving reader. Were the
                // base reusable, this destructive release would be a
                // use-after-free / double-free — which is exactly why the
                // consume guard and this release ship together.
                //
                // Drop-safety across all three exit contexts (sync return, async
                // cancel, actor shutdown): the drops are emitted BEFORE RecordInit
                // in the same basic block, so they fire on every execution path
                // that reaches the functional-update site.  No scope-exit /
                // suspend-point interleaving exists between the old-value release
                // and the new-record construction.
                //
                // Double-drop avoidance: each emitted RecordFieldLoad+Drop temp
                // appears in `derive_owned_record_drop_allowed`'s `field_binders`
                // set AND its `release_owner_bases` set (the Defect-1 guard),
                // which then excludes the base binding from composite drop —
                // complementing the existing exclusion from non-overridden field
                // binders escaping via RecordInit.  No owned field of `base` is
                // ever dropped twice.
                //
                // Fail-closed WHOLE-RECORD pre-flight: both the override-drop and
                // the shallow carry below are sound ONLY when the base record is
                // CONSUME-MARKED — `alias_moved_owned_operand` emits the
                // `AggregateAlias` iff `aggregate_ingress_moves_binding_ty` admits
                // the WHOLE record. The override-drop's debug coupling assertion
                // (B) assumes exactly that precondition. But the per-field carry /
                // override gates below admit a field IN ISOLATION when it has a
                // single-pointer inline-drop symbol (`project_field_inline_drop_-
                // symbol`), and a `Vec<closure>` / `Vec<opaque>` element DOES have
                // one (`hew_vec_free_owned` / `hew_vec_free`) even though
                // the whole record is NOT a consume-markable owned-aggregate
                // (`is_owned_aggregate_record_ty` is false — its element fails
                // `supports_value_class_drop_spine`). That record is never
                // consume-marked, yet an override-drop on a sibling single-pointer
                // COW field would still fire, tripping the coupling assertion in
                // debug BEFORE the downstream W3.029 value-class gate
                // (`UnsupportedUserRecordValueClass`) rejects it in release.
                //
                // Close the divergence at its source: when the base carries an
                // owned heap field (so an override-drop / shallow carry would run)
                // but the whole record is not consume-markable as an owned
                // aggregate, fail closed HERE with the same clean
                // `E_NOT_YET_IMPLEMENTED` the release build already emits — never
                // panic. This mirrors the fail-closed posture the per-field gates
                // already take for closure / tuple / `Option` fields.
                if base_place.is_some() {
                    if let Some(base_expr) = base.as_deref() {
                        let base_ty = self.subst_ty(&base_expr.ty);
                        let record_has_owned_heap_field = field_order.iter().any(|(_, fty)| {
                            let subst_fty = self.subst_ty(fty);
                            !matches!(
                                ValueClass::of_ty(&subst_fty, &self.type_classes),
                                ValueClass::BitCopy | ValueClass::View
                            )
                        });
                        if record_has_owned_heap_field
                            && !self.aggregate_ingress_moves_binding_ty(&base_ty)
                        {
                            // Walk every sub-expression for checker-stream coverage
                            // before bailing, mirroring the gates above.
                            for (_, fe) in fields {
                                let _ = self.lower_value(fe);
                            }
                            self.diagnostics.push(MirDiagnostic {
                                kind: MirDiagnosticKind::NotYetImplemented {
                                    construct: "functional-update over a record whose value class \
                                                MIR cannot lower yet"
                                        .to_string(),
                                    site: expr.site,
                                },
                                note: format!(
                                    "the `..base` of `{name}` carries or overrides an owned heap \
                                     field, but `{ty}` is not a consume-markable owned-aggregate \
                                     record: at least one field has a value class MIR cannot lower \
                                     yet (for example a `Vec` of closures or of opaque handles). \
                                     Without the whole-record consume mark the functional-update \
                                     in-place field release has no sound base, so it is rejected \
                                     here rather than emitted. Set the affected fields explicitly \
                                     in a plain constructor instead of carrying them through \
                                     `..base`.",
                                    ty = base_ty.user_facing(),
                                ),
                            });
                            return None;
                        }
                    }
                }
                // Fail-closed pre-flight: owned-aggregate field overrides (record /
                // tuple / enum) have no single-ptr leaf release symbol and surface
                // a NotYetImplemented diagnostic rather than leaking silently.
                if base_place.is_some() {
                    for (fname, fty) in &field_order {
                        if !explicit.contains_key(fname.as_str()) {
                            continue; // Not overridden — carries into new record normally.
                        }
                        let subst_fty = self.subst_ty(fty);
                        let vc = ValueClass::of_ty(&subst_fty, &self.type_classes);
                        if matches!(
                            vc,
                            ValueClass::BitCopy | ValueClass::View | ValueClass::PersistentShare
                        ) {
                            // No heap ownership — no destructor to emit.
                            continue;
                        }
                        // The pre-flight matches the picker's three-way verdict
                        // exhaustively: only a `Wired` field passes to the
                        // override-drop below. A bare `is_none()` gate could
                        // not distinguish "no symbol needed" (owned aggregate,
                        // released in place) from "every symbol is wrong-ABI"
                        // (unwired `Vec` element) — the `Unwired` verdict
                        // carries no symbol, so it cannot slip through as an
                        // emittable release.
                        match self.project_field_inline_drop_symbol(&subst_fty) {
                            ReleaseSymbolVerdict::Wired(_) => {}
                            // `WiredInPlace` is the yield/recv picker's composite
                            // verdict; the FIELD picker never returns it, and this
                            // pre-flight's override-drop below emits only
                            // symbol-carrying releases. Keep the owned-aggregate
                            // fail-closed posture for both.
                            ReleaseSymbolVerdict::WiredInPlace(_)
                            | ReleaseSymbolVerdict::NoDropPath => {
                                // Owned-aggregate field (record / tuple / enum): in-place
                                // drop kinds are function-scope only and cannot be emitted
                                // as inline `Instr::Drop` here.  Fail closed.
                                self.diagnostics.push(MirDiagnostic {
                                    kind: MirDiagnosticKind::NotYetImplemented {
                                        construct:
                                            "functional-update override of owned-aggregate field"
                                                .to_string(),
                                        site: expr.site,
                                    },
                                    note: format!(
                                        "field `{fname}` of `{name}` has owned-aggregate type \
                                         `{ty}` (record / tuple / enum with heap fields); \
                                         overriding an owned-aggregate field in a \
                                         functional-update expression is not yet supported — \
                                         in-place drop kinds (`RecordInPlace` / `TupleInPlace` \
                                         / `EnumInPlace`) cannot be emitted as inline \
                                         `Instr::Drop` here (follow-on to the \
                                         functional-update overridden-owned-field \
                                         leak fix)",
                                        ty = subst_fty.user_facing(),
                                    ),
                                });
                                return None;
                            }
                            ReleaseSymbolVerdict::Unwired(_) => {
                                // Fail closed: the overridden field is a `Vec`
                                // whose element release is unwired — the OLD
                                // value's inline drop would be a buffer-only
                                // free that leaks every element node.
                                let elem = self
                                    .unsupported_vec_element_in_ty(&subst_fty)
                                    .unwrap_or_else(|| format!("`{}`", subst_fty.user_facing()));
                                self.diagnostics.push(MirDiagnostic {
                                    kind: MirDiagnosticKind::NotYetImplemented {
                                        construct: format!(
                                            "`{fname}`: a `Vec` whose element is {elem} has no \
                                             per-element release protocol, so overriding it \
                                             would leak its heap nodes"
                                        ),
                                        site: expr.site,
                                    },
                                    note: "a `Vec` of `bytes` or of an indirect-enum element \
                                           cannot yet be released element-by-element, and a \
                                           functional-update override must free the old field \
                                           value it replaces. This construction is rejected at \
                                           compile rather than silently leaked, and becomes \
                                           available once the per-element release is wired."
                                        .to_string(),
                                });
                                return None;
                            }
                        }
                    }
                }
                // Fail-closed CARRY pre-flight (complement of the override
                // pre-flight above). A NON-overridden owned field is CARRIED out
                // of the consumed base into the new record by a shallow
                // `RecordFieldLoad`, with the base excluded from its composite
                // `RecordInPlace` drop (`derive_owned_record_drop_allowed`). That
                // shallow move soundly transfers ownership ONLY for field types
                // whose whole value is one pointer / handle (or a record of such):
                //   * `BitCopy` / `View` — no heap ownership; nothing to transfer
                //     or to double-free.
                //   * single-pointer COW / handle leaves (string, bytes, `Vec`,
                //     `HashMap`, `HashSet`, `Generator`) — `project_field_inline_-
                //     drop_symbol` is `Some`; the binder-escape exclusion hands the
                //     one allocation to the result, freed exactly once.
                //   * owned user records — `is_owned_aggregate_record_ty`; the
                //     nested record's heap leaves transfer with the base excluded
                //     (the binder-detection fix that also recognises nested records
                //     as heap-owning binders).
                // Every OTHER owned field type has NO sound shallow carry and
                // would be released twice — once by the base's in-place drop and
                // once by the result's drop (a double-free / use-after-free at
                // teardown):
                //   * a closure / `fn` / trait-object value (`PersistentShare`) is
                //     a heap-boxed capture env with no retain/release carry spine;
                //   * an `@resource` / `CancellationToken` / `Task` handle is a
                //     single-release affine/linear value the inline-drop authority
                //     does not cover here;
                //   * an owned composite the leaf authority does not cover
                //     (tuple-of-owned, `Option<owned>`, enum-with-heap, or any
                //     `Unknown`-class owned Named type).
                // Fail closed with an NYI diagnostic mirroring the override
                // pre-flight rather than emit the double-free. Lifting a specific
                // type's carry is tracked in hew-lang/hew#2207 (closure/`fn` env
                // carry needs the env retain/release spine that clone also lacks).
                if base_place.is_some() {
                    for (fname, fty) in &field_order {
                        if explicit.contains_key(fname.as_str()) {
                            continue; // Overridden — handled by the override path.
                        }
                        let subst_fty = self.subst_ty(fty);
                        let vc = ValueClass::of_ty(&subst_fty, &self.type_classes);
                        let sound_carry = matches!(vc, ValueClass::BitCopy | ValueClass::View)
                            || matches!(
                                self.project_field_inline_drop_symbol(&subst_fty),
                                ReleaseSymbolVerdict::Wired(_)
                            )
                            || self.is_owned_aggregate_record_ty(&subst_fty);
                        if sound_carry {
                            continue;
                        }
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct: "functional-update carry of owned non-record field"
                                    .to_string(),
                                site: expr.site,
                            },
                            note: format!(
                                "field `{fname}` of `{name}` has owned type `{ty}` whose \
                                 ownership cannot be transferred by the functional-update's \
                                 shallow field carry: a closure / `fn` / trait-object capture \
                                 env, an `@resource` / cancellation-token / task handle, or an \
                                 owned composite (tuple / `Option` / enum) with heap fields. \
                                 The `..base` consumes the base, so the base's scope-exit drop \
                                 and the new record's drop would both release this carried field \
                                 — a double-free. Set `{fname}` explicitly to a fresh value in \
                                 the update instead of carrying it through `..base`, or clone \
                                 the base into a fresh owned value first.",
                                ty = subst_fty.user_facing(),
                            ),
                        });
                        return None;
                    }
                }
                let mut field_pairs: Vec<(FieldOffset, Place)> = Vec::new();
                // Predicate-coupling backstop (debug builds only). The
                // destructive override-drop below frees the OLD value of each
                // overridden owned field IN PLACE on `base_place`. That is sound
                // ONLY because the base does not interior-alias a surviving
                // reader:
                //   * a bare-binding base is consume-marked (`AggregateAlias`),
                //     so the move-checker rejects any later read of it;
                //   * a materialised owner (call / `.clone()` result, `Vec`
                //     element) has no surviving named alias; and
                //   * any other base shape is REJECTED by the allowlist gate
                //     (1b) above (fail-closed).
                // Assert BOTH coupled invariants at EVERY override-drop site:
                //   (A) the base passed `base_is_safe_for_destructive_funcupdate`
                //       — reaching an override-drop with an unsafe base means the
                //       (1b) allowlist gate was bypassed (a new expr form, a
                //       refactor) and the UAF is reopened; and
                //   (B) for the bare-binding sub-case, the consume mark actually
                //       fired — the allowlist returns true for ANY binding shape,
                //       but that arm's safety depends on `alias_moved_owned_-
                //       operand` having emitted the `AggregateAlias` (a record
                //       newly admitted as `CowValue` would be skipped, silently
                //       reopening the UAF — the predicate-coupling guard).
                #[cfg(debug_assertions)]
                if base_place.is_some() {
                    if let Some(base_expr) = base.as_deref() {
                        let emits_override_drop = field_order.iter().any(|(fname, fty)| {
                            explicit.contains_key(fname.as_str())
                                && matches!(
                                    self.project_field_inline_drop_symbol(&self.subst_ty(fty)),
                                    ReleaseSymbolVerdict::Wired(_)
                                )
                        });
                        if emits_override_drop {
                            // (A) Allowlist backstop — fires for every base shape.
                            debug_assert!(
                                self.base_is_safe_for_destructive_funcupdate(base_expr),
                                "functional-update override-drop on a base that did NOT pass \
                                 `base_is_safe_for_destructive_funcupdate`: the in-place field \
                                 release would be a use-after-free. The allowlist gate (1b) and \
                                 the override-drop are coupled invariants — a change that admits \
                                 an unsafe base shape has reopened the UAF."
                            );
                            // (B) Bare-binding sub-case: assert the consume fired.
                            if let Some(base_id) = base_binding {
                                let consume_marked = self.statements.iter().any(|stmt| {
                                    matches!(
                                        stmt,
                                        MirStatement::AggregateAlias { binding, .. }
                                            if *binding == base_id
                                    )
                                });
                                debug_assert!(
                                    consume_marked,
                                    "functional-update override-drop on base binding {base_id:?} \
                                     that was NOT consume-marked: the in-place field release would \
                                     be a use-after-free. The base consume \
                                     (`alias_moved_owned_operand`) and the override-drop are \
                                     coupled invariants — a change that admits an owned-aggregate \
                                     base without the `AggregateAlias` mark has reopened the UAF."
                                );
                            }
                        }
                    }
                }
                for (idx, (fname, fty)) in field_order.iter().enumerate() {
                    let offset = FieldOffset(
                        u32::try_from(idx)
                            .expect("record field count exceeds u32::MAX — impossible in Hew"),
                    );
                    if let Some(&src) = explicit.get(fname.as_str()) {
                        // Emit an inline drop of the OLD base field value when it
                        // is heap-owning.  The pre-flight above guarantees every
                        // non-BitCopy overridden field has a known inline drop
                        // symbol; BitCopy / View / PersistentShare fields need no
                        // destructor.
                        if let Some(base_rec) = base_place {
                            let subst_fty = self.subst_ty(fty);
                            if let ReleaseSymbolVerdict::Wired(symbol) =
                                self.project_field_inline_drop_symbol(&subst_fty)
                            {
                                // Destructively release the OLD value of the
                                // overridden field, in declaration order, BEFORE
                                // the new record is constructed. The base is
                                // CONSUMED by `..base` (the move-checker rejects
                                // any later use — see the consume guard above), so
                                // this old value is orphaned and must be freed here
                                // or it leaks (the functional-update
                                // overridden-owned-field leak the oracle pins).
                                //
                                // SINGLE MECHANISM for single-pointer COW fields
                                // (`string` / `Vec<T>` / `HashMap` / `HashSet` /
                                // `Generator`): `RecordFieldDrop` (raw load → release
                                // → null-store). It is the purpose-built op for an
                                // in-place field destructor and gives three things
                                // the old `RecordFieldLoad` + `Drop` split did not:
                                //   * it bypasses `RecordFieldLoad`'s `string` retain
                                //     (a retain+drop no-op that LEAVES the original
                                //     un-freed — the original string-vs-rest split
                                //     existed only to dodge this);
                                //   * it does NOT depend on the incidental fact that
                                //     `RecordFieldLoad` skips retain for `Vec`/map —
                                //     when the retain-on-share spine lands and that
                                //     load starts retaining, a `load` + `Drop` here
                                //     would silently regress to a leak; and
                                //   * it null-stores the freed slot, so the exotic
                                //     residual-alias path frees `null` (a no-op for
                                //     every COW release symbol) instead of a dangle.
                                //
                                // `bytes` is the ONE exception: it is a fat
                                // `{ ptr, len, cap }` triple, not a single pointer,
                                // so its destructor takes the whole by-value triple
                                // and must be reached through `RecordFieldLoad` +
                                // `Instr::Drop` (which materialises the fat value).
                                // `field_override_uses_record_field_drop` mirrors
                                // codegen's `resolved_ty_cow_heap_release` single-ptr set
                                // so the `RecordFieldDrop` congruence assert agrees.
                                if field_override_uses_record_field_drop(&subst_fty) {
                                    self.push_instr(Instr::RecordFieldDrop {
                                        record: base_rec,
                                        field_offset: offset,
                                        ty: subst_fty,
                                        drop_fn: crate::model::DropFnSpec::Release(symbol),
                                    });
                                } else {
                                    let old_val = self.alloc_local(subst_fty.clone());
                                    self.push_instr(Instr::RecordFieldLoad {
                                        record: base_rec,
                                        field_offset: offset,
                                        dest: old_val,
                                    });
                                    self.push_instr(Instr::Drop {
                                        place: old_val,
                                        ty: subst_fty,
                                        drop_fn: Some(crate::model::DropFnSpec::Release(symbol)),
                                    });
                                }
                            }
                        }
                        field_pairs.push((offset, src));
                    } else if let Some(base_rec) = base_place {
                        // Field absent from the explicit list — load it from base.
                        // The intermediate place carries the declared field type.
                        let intermediate = self.alloc_local(fty.clone());
                        self.push_instr(Instr::RecordFieldLoad {
                            record: base_rec,
                            field_offset: offset,
                            dest: intermediate,
                        });
                        field_pairs.push((offset, intermediate));
                    } else {
                        // No explicit value and no base — checker should have
                        // rejected this; fail closed.
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct: format!(
                                    "record `{name}` missing field `{fname}` with no functional-update base"
                                ),
                                site: expr.site,
                            },
                            note: "field absent from initialiser and no `..base` provided; \
                                   the checker should have rejected this program"
                                .to_string(),
                        });
                        return None;
                    }
                }

                let dest = self.alloc_local(self.subst_ty(&expr.ty));
                self.push_instr(Instr::RecordInit {
                    // Substitute the monomorphisation's type-arg map so a
                    // generic record constructed inside a substituted body
                    // (`Box { value: x }` in `make$$i64`) carries the concrete
                    // `Box<i64>` ty, matching the `record_key`/`dest` above.
                    // Cloning `expr.ty` verbatim would leave an abstract
                    // `Box<T>` that codegen rejects (`Box$$T` not in the
                    // record-layout map).
                    ty: self.subst_ty(&expr.ty),
                    fields: field_pairs,
                    dest,
                });
                Some(dest)
            }
            HirExprKind::FieldAccess { object, field } => {
                if is_self_expr(object) {
                    if let Some((field_offset, ty)) =
                        self.current_actor_state_fields.get(field).cloned()
                    {
                        let dest = self.alloc_local(ty);
                        // P0 #2432 — fail-closed default; see the BindingRef arm above.
                        self.instructions.push(Instr::ActorStateFieldLoad {
                            field_offset,
                            dest,
                            mode: ActorStateLoadMode::Owned,
                        });
                        return Some(dest);
                    }
                }

                // ── Supervisor child-accessor intercept (S2) ────────────────
                // Before falling through to the record-field path, check whether
                // this `FieldAccess` site was tagged by the checker as a
                // supervisor child accessor. The checker populates
                // `HirModule.supervisor_child_slots` (keyed by SiteId) for every
                // expression of the form `supervisor_expr.child_name`.
                //
                // Decision: option (b) — scratch-alloca + RecordFieldLoad.
                // A `CallRuntimeAbi` with a struct-typed dest (typed
                // `__HewChildLookupResult`) carries the 16-byte return value.
                // Two `RecordFieldLoad` instructions then extract `tag` (field 0)
                // and `handle` (field 1). Tag 0 (Live) → success path; tag != 0
                // → `Terminator::Trap { kind: TrapKind::SupervisorChildUnavailable }`.
                // No new `Instr` variant is required; the match-arm cascade cost
                // for S2 is zero lines.
                //
                // LESSONS P0 `boundary-fail-closed`: no path through this arm
                // reaches the `record_field_orders` lookup for supervisor-typed LHS.
                if let Some(slot) = self.supervisor_child_slots.get(&expr.site).cloned() {
                    match slot.kind {
                        ChildKind::Pool => {
                            let sup_place = self.lower_value(object)?;
                            let key_place = self.alloc_local(ResolvedTy::I64);
                            self.push_instr(Instr::ConstI64 {
                                dest: key_place,
                                value: i64::from(slot.index),
                            });
                            let pool_ty = self.subst_ty(&expr.ty);
                            let pool_place = self.alloc_local(pool_ty.clone());
                            self.push_instr(Instr::RecordInit {
                                ty: pool_ty,
                                fields: vec![
                                    (FieldOffset(0), sup_place),
                                    (FieldOffset(1), key_place),
                                ],
                                dest: pool_place,
                            });
                            return Some(pool_place);
                        }
                        ChildKind::Static => {
                            // Nested-supervisor result: when the RESULT of the
                            // field access (`expr.ty`) is `LocalPid<T>` where T
                            // is itself a supervisor with declared children, the
                            // child slot resolves through `hew_supervisor_nested_get`
                            // (over the parent's `child_supervisors` table) rather
                            // than `hew_supervisor_child_get` (over its actor
                            // `children`). This is distinct from the common case
                            // where the LHS is a supervisor and the result is an
                            // actor PID. We detect nesting on `expr.ty`, not
                            // `object.ty` (which is always
                            // `LocalPid<ParentSupervisor>`).
                            let is_nested = matches!(&expr.ty,
                                ResolvedTy::Named { name, args, .. }
                                if name == "LocalPid"
                                    && args.len() == 1
                                    && matches!(&args[0],
                                        ResolvedTy::Named { name: inner, .. }
                                        if self.supervisor_layout_map.contains_key(inner.as_str()))
                            );

                            // The checker's `slot.index` is the child's position in
                            // the COMBINED static list (actor children + nested
                            // supervisors, in source order). The runtime keeps two
                            // separate tables — actor children in `children[]`
                            // (indexed by `hew_supervisor_child_get`) and nested
                            // supervisors in `child_supervisors[]` (indexed by
                            // `hew_supervisor_nested_get`) — each 0-based within its
                            // own kind. Translate the combined index to the
                            // kind-partitioned runtime index so both accessors hit
                            // the right slot even when actor and nested children are
                            // interleaved. MIR owns the runtime-index translation;
                            // codegen registers each kind into its own table in the
                            // same source order, so the partitioned index agrees.
                            let runtime_index = self.partitioned_static_slot_index(
                                &slot.supervisor,
                                &slot.child_name,
                                is_nested,
                            );

                            if is_nested {
                                return self.lower_supervisor_nested_get(
                                    object,
                                    runtime_index,
                                    &expr.ty,
                                );
                            }

                            return self.lower_supervisor_child_get(
                                object,
                                runtime_index,
                                &expr.ty,
                                expr.site,
                            );
                        }
                    }
                }
                // ── End supervisor intercept ─────────────────────────────────

                // Resolve the record type key from the object's type so we
                // can look up the field offset in the field-order table.
                // For a generic record instantiation (`b: Box<i64>` reading
                // `b.value`) the key is the mangled name `Box$$i64`; for a
                // monomorphic record the key is the (possibly qualified) name.
                // Route the GENERIC arm's outer name through `short_name` before
                // mangling — identical to the `StructInit` arm — since a generic
                // record's layout is registered under the bare outer name. The
                // monomorphic arm keeps `name` so a same-bare-name record
                // registered under its QUALIFIED key (`widgeti8.Widget` vs
                // `widgeti64.Widget`) hits its own divergent layout;
                // `lookup_record_field_order` strips the qualifier on a miss.
                let object_ty = self.subst_ty(&object.ty);
                let type_name = match &object_ty {
                    ResolvedTy::Named { name, args, .. } if !args.is_empty() => {
                        mangle_layout_key(short_name(name), args)
                    }
                    ResolvedTy::Named { name, .. } => name.clone(),
                    other => {
                        let _ = self.lower_value(object);
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct: format!("field access on non-named type `{other:?}`"),
                                site: expr.site,
                            },
                            note: "field access is only supported on named record types"
                                .to_string(),
                        });
                        return None;
                    }
                };
                let field_order =
                    if let Some(order) = self.lookup_record_field_order(type_name.as_str()) {
                        order.clone()
                    } else {
                        let _ = self.lower_value(object);
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct: format!(
                                    "MIR lowering for field access on unregistered record type \
                                     `{type_name}` is not implemented yet"
                                ),
                                site: expr.site,
                            },
                            note: "record type was not found in the field-order table; \
                                   this is a checker bug"
                                .to_string(),
                        });
                        return None;
                    };
                let field_offset = if let Some(idx) =
                    field_order.iter().position(|(f, _)| f == field.as_str())
                {
                    FieldOffset(
                        u32::try_from(idx)
                            .expect("field index exceeds u32::MAX — impossible in Hew"),
                    )
                } else {
                    let _ = self.lower_value(object);
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::NotYetImplemented {
                            construct: format!("unknown field `{field}` on record `{type_name}`"),
                            site: expr.site,
                        },
                        note: "field not found in declaration-order table; \
                                   this is a checker bug"
                            .to_string(),
                    });
                    return None;
                };
                self.mark_owned_string_record_field_site(object);
                let record_place = self.lower_value(object)?;
                let dest = self.alloc_local(self.subst_ty(&expr.ty));
                self.push_instr(Instr::RecordFieldLoad {
                    record: record_place,
                    field_offset,
                    dest,
                });
                Some(dest)
            }
            HirExprKind::Scope { body } => Some(self.lower_task_scope(body)),
            HirExprKind::SpawnedCall {
                callee,
                args,
                task_ty,
                bound,
            } => self.lower_spawned_call_task(callee, args, task_ty, *bound, expr.site),
            HirExprKind::ForkBlock { body, .. } => self.lower_fork_block_task(body, expr.site),
            HirExprKind::ScopeDeadline { duration, body } => {
                self.lower_scope_deadline(duration, body, expr.site)
            }
            HirExprKind::AwaitTask {
                binding_name,
                binding_id,
                output_ty,
            } => self.lower_await_task(binding_name, *binding_id, output_ty, expr.site),
            HirExprKind::AwaitRestart { child } => {
                self.lower_await_restart(child, &expr.ty, expr.site)
            }
            HirExprKind::Select(select) => self.lower_select(select, &expr.ty, expr.site),
            HirExprKind::Join(join) => self.lower_join(join, &expr.ty, expr.site),
            HirExprKind::SpawnLambdaActor { .. } => {
                // The lambda-actor literal allocates a fresh local
                // (typed as the actor's Duplex<Msg, Reply>) and
                // surfaces it as a Place::LambdaActorHandle so drop
                // elaboration selects DropKind::LambdaActorRelease.
                // The HIR's resolved capture set is forwarded into
                // the function's lambda_captures ledger; the
                // structural checker validate_lambda_captures pins
                // the Weak-on-LambdaActorHandle invariants on the
                // emitted list. Codegen for the lambda body itself
                // lands in a follow-up slice (it fails closed on a
                // Place::LambdaActorHandle today).
                Some(self.lower_spawn_lambda_actor(expr))
            }
            HirExprKind::Spawn { actor_name, args } => {
                self.lower_spawn_actor(actor_name, args, expr)
            }
            HirExprKind::ActorSelf => {
                // `this` as a value — the current actor's own handle. Synthesize
                // it through the same `hew_actor_self()` primitive `link`/
                // `monitor`/`unlink` use, yielding a borrowed `*mut HewActor`
                // (no drop obligation). A self-send (`this.go()`) lowers its
                // receiver through here and the resulting Place becomes the
                // `Terminator::Send` actor target via `lower_actor_send`.
                Some(self.emit_actor_self_handle())
            }
            HirExprKind::ActorSend {
                receiver,
                method_id,
                args,
            } => self.lower_actor_send(receiver, method_id, args, expr.site),
            HirExprKind::ActorAsk {
                receiver,
                method_id,
                args,
                reply_ty,
                deadline_ns,
            } => self.lower_actor_ask(receiver, method_id, args, reply_ty, *deadline_ns, expr),
            HirExprKind::ActorGenStream {
                receiver,
                method,
                args,
            } => self.lower_actor_gen_stream(receiver, method, args, expr),
            HirExprKind::ConnAwaitRead {
                conn,
                to_string,
                deadline_ns,
            } => self.lower_conn_await_read(conn, *to_string, *deadline_ns, expr),
            HirExprKind::ListenerAwaitAccept {
                listener,
                deadline_ns,
            } => self.lower_listener_await_accept(listener, *deadline_ns, expr),
            HirExprKind::ChannelRecvAwait {
                receiver,
                deadline_ns,
            } => self.lower_channel_recv_await(receiver, *deadline_ns, expr),
            HirExprKind::StreamRecvAwait {
                stream,
                deadline_ns,
            } => self.lower_stream_recv_await(stream, *deadline_ns, expr),
            HirExprKind::RemoteActorAsk {
                receiver,
                msg,
                timeout_ms,
                reply_ty,
            } => self.lower_remote_actor_ask(receiver, msg, timeout_ms, reply_ty, expr),
            HirExprKind::Closure {
                params,
                ret_ty,
                body,
                captures,
                escape_kind,
            } => self.lower_closure_literal(expr, params, ret_ty, body, captures, *escape_kind),
            HirExprKind::TupleIndex { tuple, index } => {
                // Walk the inner tuple expression.  If the tuple sub-expression
                // resolves to a proxy local from a multi-output runtime call
                // (e.g. `hew_duplex_pair` populates `self.tuple_decomp`), return
                // the indexed DuplexHandle Place directly without emitting any
                // additional instructions.  This is the complement of the
                // `lower_runtime_call` path that stores the output Places into
                // `tuple_decomp`.
                let inner_place = self.lower_value(tuple)?;
                if let Place::Local(local_idx) = inner_place {
                    if let Some(parts) = self.tuple_decomp.get(&local_idx) {
                        if *index < parts.len() {
                            return Some(parts[*index]);
                        }
                    }
                }
                // General case: the tuple is a regular tuple-typed local.
                // Emit `Instr::TupleFieldLoad` — codegen lowers this to a
                // GEP at `field_index` into the struct alloca + load.
                let field_index = u32::try_from(*index)
                    .expect("tuple index exceeds u32::MAX — impossible in Hew");
                let dest = self.alloc_local(self.subst_ty(&expr.ty));
                self.push_instr(Instr::TupleFieldLoad {
                    tuple: inner_place,
                    field_index,
                    dest,
                });
                Some(dest)
            }
            HirExprKind::Index { container, index } => {
                // Dispatch on receiver type — checker-authoritative
                // (`container.ty` was set by `synthesize_index`).
                // W3 collections-sugar S2: string/bytes route to their
                // own runtime ABI; Vec keeps the existing path.
                let container_ty = self.subst_ty(&container.ty);
                let elem_ty = self.subst_ty(&expr.ty);
                match &container_ty {
                    ResolvedTy::String => {
                        self.lower_string_index(container, index, &elem_ty, expr.site)
                    }
                    ResolvedTy::Bytes => {
                        self.lower_bytes_index(container, index, &elem_ty, expr.site)
                    }
                    // `m[k]` over `HashMap<K, V>` in READ position is the
                    // trapping `Index::at` accessor: it clones the matched value
                    // out through the `hew_hashmap_get_clone_layout` choke and
                    // aborts with IndexOutOfBounds on a miss (the map analogue of
                    // `v[i]` OOB). `m.get(k) -> Option<V>` is the non-aborting
                    // form and takes the `ResolvedImplCall` get path.
                    ty if ty.is_builtin(BuiltinType::HashMap) => {
                        self.lower_hashmap_index_trap(container, index, &elem_ty, expr.site)
                    }
                    _ => self.lower_vec_index(container, index, &elem_ty, expr.site),
                }
            }
            HirExprKind::Slice {
                container,
                start,
                end,
                inclusive,
            } => match &container.ty {
                ResolvedTy::String => self.lower_string_slice(
                    container,
                    start.as_deref(),
                    end.as_deref(),
                    *inclusive,
                    expr.site,
                ),
                ResolvedTy::Bytes => self.lower_bytes_slice(
                    container,
                    start.as_deref(),
                    end.as_deref(),
                    *inclusive,
                    expr.site,
                ),
                _ => self.lower_vec_slice(
                    container,
                    start.as_deref(),
                    end.as_deref(),
                    *inclusive,
                    &expr.ty,
                    expr.site,
                ),
            },
            HirExprKind::IdentityCompare { left, right } => {
                // `lhs is rhs` — emit `Instr::IdentityCompare` so codegen can
                // select `ptrtoint` + `icmp eq` for pointer-shaped handles or
                // plain `icmp eq` for machine-id integers.  The dest is typed
                // `ResolvedTy::Bool` (inherited from `expr.ty`) so the i1
                // result widening path in codegen works the same as `IntCmp`.
                // LESSONS: `checker-authority` (P0) — the allowance set was
                // validated by the checker; we just lower the node.
                let lhs = self.lower_value(left)?;
                let rhs = self.lower_value(right)?;
                let dest = self.alloc_local(expr.ty.clone());
                self.instructions
                    .push(Instr::IdentityCompare { dest, lhs, rhs });
                Some(dest)
            }
            HirExprKind::CoerceToDynTrait {
                value,
                trait_name,
                concrete_type,
                method_table,
                vtable_entries,
            } => {
                // Materialise the concrete value into a Place, then emit
                // `Instr::CoerceToDynTrait` to construct the fat pointer.
                // The dest is typed `ResolvedTy::TraitObject` (inherited
                // from `expr.ty`), so codegen can pick the 2-word layout.
                let value_place = self.lower_value(value)?;
                let dest = self.alloc_local(expr.ty.clone());
                self.push_instr(Instr::CoerceToDynTrait {
                    value: value_place,
                    dest,
                    trait_name: trait_name.clone(),
                    concrete_type: concrete_type.clone(),
                    method_table: method_table.clone(),
                    vtable_entries: vtable_entries.clone(),
                });
                // Concrete-source drop suppression at the coerce site.
                //
                // The coerced concrete value is *moved* into the fat
                // pointer: its frame slot (for FrameOwned dyn locals)
                // or its post-memcpy heap copy (for HeapBoxed dyn
                // locals) is now owned by the dyn binding's vtable
                // slot-0 `drop_in_place` ritual. If the concrete also
                // remained in the enclosing function's `owned_locals`,
                // its independent scope-exit drop would run the same
                // concrete close ritual a second time on the same
                // storage — a use-after-move / double-drop pair.
                //
                // The HIR `IntentKind::Consume` path in `lower_value`
                // for `HirExprKind::BindingRef` already suppresses many
                // ordinary move cases via `mark_binding_moved`, but it
                // is gated on `IntentKind::Consume` and on a non-BitCopy
                // `ValueClass`. The coercion site is the structural
                // truth — the dyn fat pointer is constructed here, and
                // here only — so suppression rooted at the producer is
                // both necessary and sufficient regardless of the
                // upstream intent inference.
                //
                // `dyn_rebind_source_binding` walks the inner `value`
                // expression through transparent wrappers (`Block` with
                // a tail) and returns the source `BindingId` for
                // `HirExprKind::BindingRef` shapes. Fresh-value shapes
                // (`RecordCtor`, `Call*`, literals, etc.) materialise
                // into newly-allocated locals that are never registered
                // in `owned_locals`, so the helper correctly returns
                // `None` and no suppression is needed. `mark_binding_moved`
                // is idempotent on bindings that are already absent.
                if let Some(src_id) = dyn_rebind_source_binding(value) {
                    self.mark_binding_moved(src_id);
                }
                Some(dest)
            }
            HirExprKind::CallDynMethod {
                receiver,
                trait_name,
                method_name,
                slot,
                args,
                ret_ty,
                signature,
            } => {
                // Lower the receiver (a `dyn Trait` fat pointer) and the
                // ordinary args. `Instr::CallTraitMethod` GEPs into the
                // vtable at `slot`, loads the function pointer, and calls
                // it with `fat_pointer.data` as the implicit receiver —
                // codegen materialises the data-ptr argument from the
                // fat pointer, so the args list here is the source-level
                // args without the synthetic receiver entry.
                let fat_pointer = self.lower_value(receiver)?;
                let mut lowered_args: Vec<Place> = Vec::with_capacity(args.len());
                for arg in args {
                    lowered_args.push(self.lower_value(arg)?);
                }
                // W3.031 Stage 1.6: validate the substituted FnSig is
                // fully resolved BEFORE emission. The checker is
                // authoritative for trait-type-param + assoc-binding
                // substitution at the receiver's coercion site; if any
                // `Ty::Var`/`Ty::Error`/unresolved `Ty::AssocType`
                // survives into the call-site signature, fail closed
                // here — codegen would otherwise consume a degenerate
                // erased call type at the indirect-dispatch boundary
                // (copilot-instructions §3 Type Inference Boundary).
                if let Some(reason) = unresolved_fn_sig_reason(signature.as_ref()) {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::CallTraitMethodSignatureUnresolved {
                            trait_name: trait_name.clone(),
                            method_name: method_name.clone(),
                            site: expr.site,
                            reason: reason.clone(),
                        },
                        note: format!(
                            "dyn-trait method call `{trait_name}::{method_name}` reached MIR with \
                             an unresolved caller-side FnSig: {reason}. The checker's \
                             trait-object bound substitution at the receiver's coercion site \
                             must produce a fully resolved signature; codegen (W3.031 Stage 7) \
                             consumes it verbatim to derive the erased indirect-call type and \
                             cannot fabricate a default."
                        ),
                    });
                    return None;
                }
                let dest = if matches!(ret_ty, ResolvedTy::Unit) {
                    None
                } else {
                    Some(self.alloc_local(ret_ty.clone()))
                };
                self.push_instr(Instr::CallTraitMethod {
                    fat_pointer,
                    dest,
                    trait_name: trait_name.clone(),
                    method_name: method_name.clone(),
                    slot: *slot,
                    args: lowered_args,
                    signature: signature.clone(),
                });
                dest
            }
            HirExprKind::ResolvedImplCall {
                receiver,
                method_name,
                target_symbol,
                target_family,
                type_args,
                args,
                ret_ty,
                ..
            } => {
                // Builtin-generic trait dispatch (HashMap/HashSet/Vec today;
                // Option/Result migrate later). The checker's resolver
                // has already chosen the satisfying impl and recorded the
                // typed [`MethodTargetFamily`] verdict; HIR copied it
                // onto the variant. MIR routes on the typed family and
                // emits a direct `Terminator::Call` against `target_symbol`,
                // which remains the concrete linker-edge identifier.
                //
                // No re-derivation of the family from `method_name` /
                // `type_args` / `target_symbol` here — that would
                // re-implement the resolver's authority at the MIR
                // boundary (LESSONS `checker-authority`,
                // `codegen-abi-authority`). The family IS the verdict;
                // the symbol IS the callee name.
                //
                // Fail-closed arity gate: every kernel family this arm
                // dispatches to was registered by
                // `collection_dispatch_registry_impl` with an explicit
                // type-arg arity (HashMap takes 2, HashSet and Vec take
                // 1). An arity mismatch here means the populator and
                // this consumer have drifted — the right place to fix
                // is the populator, not silently coerce here. LESSONS:
                // `exhaustive-coverage`, `boundary-fail-closed`.
                //
                // Catalog descriptor materialisation is deliberately NOT
                // bound here as a call arg: the runtime kernel snapshots
                // its descriptors by-value into the map at
                // `hew_hashmap_new_with_layout`-time (C0a) and reads them
                // from `(*m).key_layout` / `(*m).val_layout`. The kernel
                // ABI is `(handle, key_ptr, val_ptr)` for insert, etc. —
                // descriptor pointers are not passed across per-op. The
                // C0b `LayoutDescriptorSymbol` catalog covers fixed-set
                // primitives (i32..u64, f32/f64, bool, char, string,
                // bytes, unit); Named-record K/V are handled by the
                // synthesised per-record descriptor pipeline at
                // constructor lowering (C-1c). Coverage of the primitive
                // set is asserted by the
                // `stdlib_catalog_layout_descriptor_coverage` gate.
                match target_family {
                    hew_types::MethodTargetFamily::HashMap(_) => {
                        if type_args.len() != 2 {
                            unreachable!(
                                "Stage C: hashmap `.{method_name}` resolved to \
                                 family {target_family:?} with {} type_args; \
                                 populator at hew-types/src/check/methods.rs \
                                 registers HashMap impls with 2 type-args (K, V) — \
                                 populator and MIR consumer have drifted",
                                type_args.len()
                            );
                        }
                    }
                    hew_types::MethodTargetFamily::HashSet(_) => {
                        if type_args.len() != 1 {
                            unreachable!(
                                "Stage C: hashset `.{method_name}` resolved to \
                                 family {target_family:?} with {} type_args; \
                                 populator at hew-types/src/check/methods.rs \
                                 registers HashSet impls with 1 type-arg (T) — \
                                 populator and MIR consumer have drifted",
                                type_args.len()
                            );
                        }
                    }
                    hew_types::MethodTargetFamily::Vec(_) => {
                        if type_args.len() != 1 {
                            unreachable!(
                                "Stage C: vec `.{method_name}` resolved to \
                                 family {target_family:?} with {} type_args; \
                                 populator at hew-types/src/check/methods.rs \
                                 registers Vec impls with 1 type-arg (T) — \
                                 populator and MIR consumer have drifted",
                                type_args.len()
                            );
                        }
                    }
                }

                // W5.016: finalize the owned-vs-BitCopy Vec element ABI through
                // the SINGLE consumer-side authority (`is_owned_vec_element`, the
                // same predicate get/set/pop and scope-exit-free consult). A
                // `hew_vec_push_layout` whose receiver Vec has an owned
                // (heap-owning) element must route to `hew_vec_push_owned` so the
                // push agrees with the owned constructor descriptor — otherwise a
                // BitCopy push op on an owned-constructed handle trips the runtime
                // layout-aware abort. This upgrade is the array-literal-desugar
                // path's owned-ness decision: the HIR desugar bakes
                // `hew_vec_push_layout` from the marker-only `ValueClass`, which
                // cannot see structural heap-ownership; MIR owns that structural
                // authority. A genuine checker-resolved owned `.push()` already
                // carries `hew_vec_push_owned`, and a real BitCopy element returns
                // false here, so this only ever corrects the synthesized guess —
                // it never re-derives the checker's impl-resolution verdict
                // (`dedup-semantic-boundary`).
                //
                // The owned-rewrite predicate is *family-gated* (must be a Vec
                // push) AND *symbol-keyed* (must be the `_layout` variant the
                // HIR desugar emits). The family gate ensures we never
                // accidentally consult `vec_receiver_has_owned_element` for a
                // non-Vec call; the symbol check distinguishes the synthetic
                // `_layout` from a real per-element-type symbol the checker
                // resolved directly. Once the substrate enumerates the
                // per-element Vec push variants, the second arm collapses.
                let callee = if target_symbol.ends_with("_FAMILY") {
                    // #1929 Stage 1: the checker kept the `hew_vec_*_FAMILY`
                    // placeholder because the `Vec<T>` element was a declared
                    // type parameter, so the per-ABI symbol could not be chosen
                    // at check time. Re-resolve it now from the element this
                    // monomorphisation substituted in. The resolver consults the
                    // same source-derived authority as the concrete path, with
                    // owned-element precedence
                    // (`is_owned_vec_element` -> the `hew_vec_*_owned` family) for
                    // non-`Copy` records/enums, heap-owning tuples, and nested
                    // collections (#1929 Stage 2), then the checker's exported
                    // element->ABI verdict (`vec_generic_element_abi`) for
                    // scalar / string / pointer /
                    // Copy value-record elements. An element neither authority
                    // resolves fails closed here rather than calling an undeclared
                    // symbol.
                    let Some(sym) =
                        self.resolve_polymorphic_vec_element_symbol(*target_family, &receiver.ty)
                    else {
                        let elem = self.vec_element_user_facing(&receiver.ty);
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct: format!(
                                    "`Vec::{method_name}` on the type-parameter \
                                     element `{elem}`"
                                ),
                                site: expr.site,
                            },
                            note: "element-typed `Vec<T>` methods under a type \
                                   parameter resolve through the same element ABIs \
                                   as the concrete path — scalar, string, pointer, \
                                   Copy value-record, and owned (non-Copy record/\
                                   enum/tuple/nested-collection) elements; this \
                                   element maps to none of them and fails closed"
                                .to_string(),
                        });
                        return None;
                    };
                    sym
                } else if matches!(
                    target_family,
                    hew_types::MethodTargetFamily::Vec(hew_types::VecMethod::Push)
                ) && target_symbol == "hew_vec_push_layout"
                    && self.vec_receiver_has_owned_element(&receiver.ty)
                {
                    // Array literals are HIR-desugared to pushes into a synthetic
                    // Vec temp; the element operand is a FRESH, single-use
                    // `record_init` temp constructed solely for the Vec (even a
                    // named-binding element `[a, ..]` is re-constructed into a
                    // throwaway temp first). A COPY-IN deep clone
                    // (`hew_vec_push_owned`) would then leak that temp's owned
                    // heap — it has no binding and no scope-exit drop to retain
                    // the original (`container-ingress-ownership-is-per-container`
                    // COPY-IN retain assumes a tracked source). Route the
                    // array-literal owned push to the MOVE-in variant, which
                    // transfers the element's heap into the slot without a clone;
                    // the source temp is then dead. A user-authored
                    // `v.push(existing_owned)` keeps the COPY-IN clone (its source
                    // binding lives on and retains its own drop).
                    if matches!(
                        &receiver.kind,
                        HirExprKind::BindingRef { name, .. } if name.starts_with("__hew_array_")
                    ) {
                        "hew_vec_push_owned_move".to_string()
                    } else {
                        "hew_vec_push_owned".to_string()
                    }
                } else {
                    target_symbol.clone()
                };
                // A user-authored owned-element push of a fresh materialised
                // rvalue (`v.push(Name { ... })`, `v.push(make_name())`,
                // `v.push(existing.clone())`) has no source binding whose
                // scope-exit drop can balance `hew_vec_push_owned`'s copy-in
                // clone. Move that one-shot owner into the Vec instead. A bare
                // binding is not a materialised rvalue, so
                // `v.push(existing_owned)` keeps the clone-in contract and the
                // caller keeps its own independent drop.
                let callee = if callee == "hew_vec_push_owned"
                    && args.len() == 1
                    && Self::expr_is_materialized_owner(
                        &args[0],
                        &self.funcupdate_fn_returns_fresh,
                        &self.funcupdate_param_ids,
                    ) {
                    "hew_vec_push_owned_move".to_string()
                } else {
                    callee
                };

                // A user-authored owned-element `set` of a fresh materialised
                // rvalue (`v.set(i, Name { .. })`, `v.set(i, make())`) has the
                // SAME unbound-temp hole `push` does: `hew_vec_set_owned` is
                // COPY-IN (deep-clones the element into the slot), but the
                // throwaway `record_init` temp has no binding and no scope-exit
                // drop to balance that clone, so its owned heap leaks (measured:
                // a deep-owned element leaks ~4 nodes per store; a refcount-
                // shared string element is reclaimed via the vec free and does
                // not). Route it to the MOVE-in sibling `hew_vec_set_owned_move`,
                // which byte-transfers the element's heap into the slot without a
                // clone; the source temp is then dead. The element operand is
                // `args[1]` (`args[0]` is the index). `expr_is_materialized_owner`
                // is the identical fresh-rvalue predicate push uses: a bare
                // `BindingRef` (a shared/after-read local — N1/N2) returns false
                // and stays COPY-IN (moving it would double-free the live
                // binding's heap), and a construction embedding a whole by-value
                // parameter returns false too (moving would double-free the
                // caller's `p`). "No other reader" holds by construction — a
                // fresh unbound constructor operand has no name.
                let callee = if callee == "hew_vec_set_owned"
                    && args.len() == 2
                    && Self::expr_is_materialized_owner(
                        &args[1],
                        &self.funcupdate_fn_returns_fresh,
                        &self.funcupdate_param_ids,
                    ) {
                    "hew_vec_set_owned_move".to_string()
                } else {
                    callee
                };

                // Array literals are HIR-desugared to pushes into a synthetic
                // Vec temp. Treat each pushed element as aggregate ingress so
                // `[s, "x"]; s` is rejected without changing ordinary
                // user-authored method/function argument semantics. The Vec
                // push family identifies the call genuinely; we no longer
                // re-parse the symbol prefix to recognise it.
                let is_array_literal_push = matches!(
                    target_family,
                    hew_types::MethodTargetFamily::Vec(hew_types::VecMethod::Push)
                ) && matches!(
                    &receiver.kind,
                    HirExprKind::BindingRef { name, .. } if name.starts_with("__hew_array_")
                );

                // Vec element STORES (push / set — user-authored or the
                // array-literal desugar) are owning ingress for closure-pair
                // elements: the slot byte-copies the pair and
                // the Vec descriptor frees its env at scope exit.
                // Route closure-typed element operands through the
                // sole-owner ingress gate (owned binding → move; borrow →
                // refuse). Non-closure args keep ordinary call semantics.
                let is_vec_element_store = matches!(
                    target_family,
                    hew_types::MethodTargetFamily::Vec(
                        hew_types::VecMethod::Push | hew_types::VecMethod::Set
                    )
                );

                // Lower receiver as arg[0], then explicit args.
                let receiver_place = self.lower_value(receiver)?;
                let mut arg_places = vec![receiver_place];
                for arg in args {
                    arg_places.push(self.lower_value(arg)?);
                    if builtin_method_arg_is_move_ingress(*target_family) {
                        self.consume_moved_builtin_method_arg(arg);
                    }
                    if is_array_literal_push {
                        self.alias_moved_owned_operand(arg);
                    }
                    if is_vec_element_store {
                        self.enforce_closure_pair_ingress(arg);
                    }
                }
                // COPY-IN param embeds stay caller-borrowed; only the source
                // temp's independently retained string share gains an owner.
                self.register_copy_in_param_embed_temp_owner(&callee, args, &arg_places);
                let dest = if matches!(ret_ty, ResolvedTy::Unit) {
                    None
                } else {
                    Some(self.alloc_local(ret_ty.clone()))
                };
                let next = self.alloc_block();
                // Recover only families intentionally carried on MIR calls.
                // Codegen-only collection partitions remain typed in
                // RuntimeCallFamily without widening the MIR dump surface.
                let builtin =
                    hew_types::runtime_call::RuntimeCallFamily::from_mir_builtin_symbol(&callee);
                self.finish_current_block(Terminator::Call {
                    callee,
                    builtin,
                    args: arg_places,
                    dest,
                    next,
                });
                self.start_block(next);
                dest
            }
            HirExprKind::CallTraitMethodStatic {
                receiver,
                receiver_type_param,
                declaring_trait,
                method_name,
                args,
                ret_ty,
                ..
            } => {
                // Static trait dispatch via structured impl registry.
                //
                // Resolution path:
                //   1. Substitute `receiver_type_param` through the
                //      monomorphisation `subst` map to obtain a concrete
                //      receiver `ResolvedTy`. If no substitution exists
                //      the call survived into a concrete function body —
                //      this is a checker/HIR invariant violation;
                //      fail-closed with `UnresolvedStaticDispatchSubstitution`.
                //   2. Project the concrete `ResolvedTy` to its canonical
                //      `(self_type_name, type_args)` via
                //      `hew_hir::dispatch::receiver_self_type_for_impl_lookup`.
                //      The name matches `HirImplBlock::self_type_name`;
                //      we DO NOT reconstruct an impl symbol from it.
                //   3. Look up `(declaring_trait, self_type_name,
                //      method_name)` in `self.trait_impl_index` — the
                //      structured registry built once from
                //      `HirItem::Impl` metadata. The hit carries the
                //      canonical `method_symbol` produced by
                //      `HirImplBlock::method_symbol` at impl-block
                //      lowering, plus impl-level type parameter names.
                //   4. If the impl is generic, mangle `(method_symbol,
                //      type_args)` to reach the per-instantiation
                //      symbol HIR's `closure_under_substitution`
                //      registered.
                //
                // Each step uses structured HIR facts (`declaring_trait`
                // and `method_name` from the call site, `self_type_name`
                // and `method_symbol` from `HirImplBlock`). No call-site
                // display-name parsing, no `<Type>::<method>` string
                // construction.
                let resolved_ret_ty = self.subst_ty(ret_ty);
                let Some(concrete_ty) = self.subst.get(receiver_type_param).cloned() else {
                    // (1) failure path — no substitution.
                    self.lower_value(receiver);
                    for arg in args {
                        self.lower_value(arg);
                    }
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::UnresolvedStaticDispatchSubstitution {
                            receiver_type_param: receiver_type_param.clone(),
                            declaring_trait: declaring_trait.clone(),
                            method_name: method_name.clone(),
                            site: expr.site,
                        },
                        note: format!(
                            "static trait dispatch `{declaring_trait}::{method_name}` reached \
                             MIR in a concrete function body without a substitution for \
                             receiver type parameter `{receiver_type_param}`; this indicates \
                             a missing monomorphization binding (the generic origin should \
                             not be emitted)"
                        ),
                    });
                    return None;
                };
                // (2) canonical (self_type_name, type_args).
                let Some((self_type_name, type_args)) =
                    hew_hir::dispatch::receiver_self_type_for_impl_lookup(&concrete_ty)
                else {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::NotYetImplemented {
                            construct: format!(
                                "static trait dispatch on receiver shape `{concrete_ty:?}` \
                                 for `{declaring_trait}::{method_name}`"
                            ),
                            site: expr.site,
                        },
                        note: "receiver type has no canonical impl-self name; \
                               static dispatch supports nominal and primitive receivers only"
                            .to_string(),
                    });
                    return None;
                };
                // (3) structured registry lookup (tolerant of a
                // module-qualified receiver name for imported impls).
                // Pass `type_args` so the lookup finds concrete-specialised impls
                // (`impl Describe for Wrapper<i64>`) keyed under the mangled name
                // (`"Wrapper$$i64"`) before falling back to generic impls (#2270).
                let Some(entry) = hew_hir::dispatch::lookup_trait_impl_entry(
                    &self.trait_impl_index,
                    declaring_trait,
                    &self_type_name,
                    method_name,
                    &type_args,
                )
                .cloned() else {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::StaticDispatchImplNotFound {
                            declaring_trait: declaring_trait.clone(),
                            self_type_name: self_type_name.clone(),
                            method_name: method_name.clone(),
                            site: expr.site,
                        },
                        note: format!(
                            "no impl of trait `{declaring_trait}` for `{self_type_name}` \
                             registered in the static-dispatch index; the checker should \
                             have rejected this call"
                        ),
                    });
                    return None;
                };
                // (4) generic-impl monomorphisation mangling.
                let callee_symbol = if entry.impl_type_params.is_empty() {
                    if !self.module_fn_names.contains(&entry.method_symbol) {
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::StaticDispatchImplNotFound {
                                declaring_trait: declaring_trait.clone(),
                                self_type_name: self_type_name.clone(),
                                method_name: method_name.clone(),
                                site: expr.site,
                            },
                            note: format!(
                                "impl method `{}` is registered in the static-dispatch \
                                 index but not in module_fn_names",
                                entry.method_symbol
                            ),
                        });
                        return None;
                    }
                    entry.method_symbol.clone()
                } else {
                    let mangled = hew_hir::monomorph::mangle(&entry.method_symbol, &type_args);
                    if !self.module_fn_names.contains(&mangled) {
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::StaticDispatchMonomorphisationMissing {
                                method_symbol: entry.method_symbol.clone(),
                                mangled: mangled.clone(),
                                site: expr.site,
                            },
                            note: format!(
                                "static dispatch resolved to generic impl method `{}` \
                                 but no monomorphisation `{}` was registered by HIR's \
                                 closure_under_substitution",
                                entry.method_symbol, mangled
                            ),
                        });
                        return None;
                    }
                    mangled
                };
                // Lower receiver as first arg + the explicit args.
                let receiver_place = self.lower_value(receiver)?;
                let mut arg_places = vec![receiver_place];
                for arg in args {
                    arg_places.push(self.lower_value(arg)?);
                }
                let dest = if matches!(resolved_ret_ty, ResolvedTy::Unit) {
                    None
                } else {
                    Some(self.alloc_local(resolved_ret_ty))
                };
                let next = self.alloc_block();
                self.finish_current_block(Terminator::Call {
                    callee: callee_symbol,
                    builtin: None,
                    args: arg_places,
                    dest,
                    next,
                });
                self.start_block(next);
                dest
            }
            HirExprKind::VarSelfMethodCall {
                receiver,
                target,
                args,
                ret_ty,
                receiver_ty,
            } => self.lower_var_self_method_call(
                expr.site,
                receiver,
                target,
                args,
                ret_ty,
                receiver_ty,
            ),
            HirExprKind::MachineEmit { event_idx, fields } => {
                // Lower each payload field expression to a Place. Collect
                // even if some fail (return None) to maximise diagnostic
                // coverage across the expression tree.
                let mut payload: Vec<Place> = Vec::with_capacity(fields.len());
                for (_, field_expr) in fields {
                    if let Some(p) = self.lower_value(field_expr) {
                        payload.push(p);
                    }
                }
                // Stable machine-type id, set by `emit_machine_step_transition_return`
                // / `lower_machine_lifecycle_block` alongside the self/event binding
                // swap. Absent only if `emit` reached MIR outside a machine
                // transition/lifecycle body — a checker/HIR invariant violation
                // (HIR's `current_machine_events` resolution already fails closed
                // for that case), so fail closed here too rather than fabricate an
                // id that would misattribute the emit to the wrong machine type.
                let Some(machine_emit_id) = self.current_machine_emit_type_id else {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::UnsupportedNode {
                            reason: format!(
                                "MachineEmit(event_idx={event_idx}) — outside a machine \
                                 transition/lifecycle body"
                            ),
                        },
                        note: "machine emit is only legal inside a transition body or an \
                               `entry {}`/`exit {}` lifecycle block, where the step fn's \
                               machine-type id is in scope"
                            .to_string(),
                    });
                    return None;
                };
                // Emit a typed placeholder that records the event index,
                // machine-type id, and lowered payload places. The actual
                // emit-queue runtime call sequence is wired in codegen.
                //
                // WHY placeholder: keeps MIR pipeline stages type-correct
                // through stages that would otherwise skip the expression,
                // without silently dropping the emit.
                self.push_instr(Instr::MachineEmitPlaceholder {
                    event_idx: *event_idx,
                    payload,
                    machine_emit_id,
                });
                None
            }
            HirExprKind::MachineVariantCtor {
                state_idx, payload, ..
            } => {
                // Construct a machine value at the given state variant. The
                // dest local is allocated from `expr.ty` so that generic type
                // args (e.g. `Option<I64>`) are preserved all the way through
                // MIR. Using `expr.ty` matches the RecordInit precedent and
                // ensures codegen sees the fully-parameterised type name.
                //
                // Tag-dominance invariant (Place doc, `MachineVariant`): the
                // `Place::MachineTag` store dominates every `Place::MachineVariant`
                // field store because they are emitted in straight-line order
                // within the same block, and Slice 4c (drop-elaborator) reads the
                // tag store first when computing per-variant drop plans.
                let dest = self.alloc_local(expr.ty.clone());
                let Place::Local(dest_local) = dest else {
                    unreachable!("alloc_local returns Place::Local");
                };
                // Tag store: Place::MachineTag(dest_local) = state_idx.
                let tag_const = self.alloc_local(ResolvedTy::I64);
                self.push_instr(Instr::ConstI64 {
                    dest: tag_const,
                    value: i64::try_from(*state_idx).unwrap_or(i64::MAX),
                });
                self.push_instr(Instr::Move {
                    dest: Place::MachineTag(dest_local),
                    src: tag_const,
                });
                // Per-payload-field store via Place::MachineVariant. HIR has
                // already resolved field-name -> field_idx via the
                // declaration-order layout of HirMachineState.fields; we honour
                // the source-declared order here for determinism.
                if let Some(fields) = payload {
                    for (field_idx, (_field_name, field_expr)) in fields.iter().enumerate() {
                        let Some(src) = self.lower_value(field_expr) else {
                            continue;
                        };
                        let field_idx_u32 =
                            u32::try_from(field_idx).expect("field index exceeds u32::MAX");
                        let variant_idx_u32 =
                            u32::try_from(*state_idx).expect("state index exceeds u32::MAX");
                        self.push_instr(Instr::Move {
                            dest: Place::MachineVariant {
                                local: dest_local,
                                variant_idx: variant_idx_u32,
                                field_idx: field_idx_u32,
                            },
                            src,
                        });
                        self.alias_moved_owned_operand(field_expr);
                        self.enforce_closure_pair_ingress(field_expr);
                    }
                }
                Some(dest)
            }
            HirExprKind::MachineFieldAccess {
                machine_name,
                state_idx,
                field_idx,
                field_name,
            } => {
                // Load a payload field from the `self` machine binding
                // dominated by the transition's source state. The HIR has
                // already resolved `state_idx` (the source state) and
                // `field_idx` (declaration-order index within that state's
                // HirMachineState.fields). MIR addresses the field via
                // `Place::MachineVariant { binding: self_binding, variant_idx,
                // field_idx }`; the dominating `Place::MachineTag` was
                // proven equal to `state_idx` by the dispatch tree that
                // entered this transition arm.
                let Some(self_binding) = self.current_machine_self_binding else {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::UnsupportedNode {
                            reason: format!(
                                "MachineFieldAccess({machine_name}[{state_idx}].{field_name}) — \
                                 outside a machine transition body"
                            ),
                        },
                        note: "machine self-field reads only legal inside a \
                               transition body where the step fn's self binding \
                               is in scope"
                            .to_string(),
                    });
                    return None;
                };
                // Resolve the machine `self` binding to its MIR-local id so
                // `Place::MachineVariant` can address it directly.
                let Some(self_place) = self.binding_locals.get(&self_binding).copied() else {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::UnsupportedNode {
                            reason: format!(
                                "MachineFieldAccess({machine_name}[{state_idx}].{field_name}) — \
                                 self binding has no allocated local"
                            ),
                        },
                        note: "internal: synthesize_machine_step_fn must allocate \
                               the self parameter local before walking transition bodies"
                            .to_string(),
                    });
                    return None;
                };
                let Place::Local(self_local) = self_place else {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::UnsupportedNode {
                            reason: format!(
                                "MachineFieldAccess({machine_name}[{state_idx}].{field_name}) — \
                                 self binding maps to non-Local place {self_place:?}"
                            ),
                        },
                        note: "internal: machine self parameter must be a Place::Local".to_string(),
                    });
                    return None;
                };
                let dest = self.alloc_local(expr.ty.clone());
                let variant_idx_u32 =
                    u32::try_from(*state_idx).expect("state index exceeds u32::MAX");
                let field_idx_u32 =
                    u32::try_from(*field_idx).expect("field index exceeds u32::MAX");
                self.push_instr(Instr::Move {
                    dest,
                    src: Place::MachineVariant {
                        local: self_local,
                        variant_idx: variant_idx_u32,
                        field_idx: field_idx_u32,
                    },
                });
                Some(dest)
            }
            HirExprKind::MachineEventFieldAccess {
                machine_name,
                event_idx,
                field_idx,
                field_name,
            } => {
                let Some(event_binding) = self.current_machine_event_binding else {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::UnsupportedNode {
                            reason: format!(
                                "MachineEventFieldAccess({machine_name}Event[{event_idx}].{field_name}) — \
                                 outside a machine transition body"
                            ),
                        },
                        note: "machine event-field reads are only legal inside a transition body"
                            .to_string(),
                    });
                    return None;
                };
                let Some(event_place) = self.binding_locals.get(&event_binding).copied() else {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::UnsupportedNode {
                            reason: format!(
                                "MachineEventFieldAccess({machine_name}Event[{event_idx}].{field_name}) — \
                                 event binding has no allocated local"
                            ),
                        },
                        note: "internal: synthesize_machine_step_fn must allocate the event parameter local"
                            .to_string(),
                    });
                    return None;
                };
                let Place::Local(event_local) = event_place else {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::UnsupportedNode {
                            reason: format!(
                                "MachineEventFieldAccess({machine_name}Event[{event_idx}].{field_name}) — \
                                 event binding maps to non-Local place {event_place:?}"
                            ),
                        },
                        note: "internal: machine event parameter must be a Place::Local".to_string(),
                    });
                    return None;
                };
                let dest = self.alloc_local(expr.ty.clone());
                let variant_idx_u32 =
                    u32::try_from(*event_idx).expect("event index exceeds u32::MAX");
                let field_idx_u32 =
                    u32::try_from(*field_idx).expect("field index exceeds u32::MAX");
                self.push_instr(Instr::Move {
                    dest,
                    src: Place::MachineVariant {
                        local: event_local,
                        variant_idx: variant_idx_u32,
                        field_idx: field_idx_u32,
                    },
                });
                Some(dest)
            }
            HirExprKind::MachineStep {
                machine_name,
                receiver,
                event,
            } => {
                // `m.step(event)` lowers to a call into the synthesised
                // `<Name>__step(self, event) -> <Name>` helper followed by an
                // unconditional store-back of the returned value into the
                // receiver's binding slot.
                //
                // The store-back is what makes `step` look like in-place
                // mutation at the user surface even though the helper
                // returns a fresh machine value (immutable internal
                // representation). The HIR checker verified the receiver is
                // a mutable binding (HirExprKind::MachineStep doc), so we
                // pattern-match `BindingRef { resolved: Binding(id), .. }`
                // and fail-closed otherwise.
                let HirExprKind::BindingRef {
                    resolved: ResolvedRef::Binding(binding_id),
                    name: receiver_name,
                } = &receiver.kind
                else {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::UnsupportedNode {
                            reason: format!(
                                "MachineStep on `{machine_name}` has non-binding receiver \
                                 {:?}; checker should have rejected this",
                                receiver.kind
                            ),
                        },
                        note: "machine step receivers must be a mutable local binding so \
                               the call's return value can be stored back in place"
                            .to_string(),
                    });
                    return None;
                };
                // Resolve the store-back target. A machine held in a LOCAL
                // binding stores back into its binding slot; a machine held
                // in ACTOR STATE has no binding slot — the receiver loads
                // via `ActorStateFieldLoad` (the BindingRef fallback in
                // `lower_value`) and the store-back targets the state field
                // through `ActorStateFieldStore`, riding the same
                // overwrite-release path every other state-field store uses
                // (the old state's heap payload is released before the new
                // value lands).
                let receiver_slot = self.binding_locals.get(binding_id).copied();
                let field_offset = if receiver_slot.is_some() {
                    None
                } else if let Some((field_offset, _)) =
                    self.current_actor_state_fields.get(receiver_name).cloned()
                {
                    Some(field_offset)
                } else {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::UnresolvedPlace {
                            binding: *binding_id,
                            name: receiver_name.clone(),
                            site: receiver.site,
                        },
                        note: "machine step receiver binding has no MIR place".to_string(),
                    });
                    return None;
                };
                // Lower receiver (load the current machine value) and event
                // arguments as by-value reads.
                let self_arg = self.lower_value(receiver)?;
                let event_arg = self.lower_value(event)?;
                let ret_ty = ResolvedTy::Named {
                    name: match &receiver.ty {
                        ResolvedTy::Named { name, .. } => name.clone(),
                        _ => machine_name.clone(),
                    },
                    args: match &receiver.ty {
                        ResolvedTy::Named { args, .. } => args.clone(),
                        _ => Vec::new(),
                    },
                    builtin: match &receiver.ty {
                        ResolvedTy::Named { builtin, .. } => *builtin,
                        _ => None,
                    },
                    // A machine step's result type mirrors the machine value
                    // type, which is never `#[opaque]`.
                    is_opaque: false,
                };
                let ret_local = self.alloc_local(ret_ty.clone());
                let next = self.alloc_block();
                self.finish_current_block(Terminator::Call {
                    callee: mangle_machine_step(short_name(machine_name)),
                    builtin: None,
                    args: vec![self_arg, event_arg],
                    dest: Some(ret_local),
                    next,
                });
                self.start_block(next);
                // Store-back: write the call's return into the receiver's
                // slot. The MIR producer emits this unconditionally; even
                // when the transition was a self-transition the value is
                // consistent with the helper's return.
                if let Some(receiver_slot) = receiver_slot {
                    self.push_instr(Instr::Move {
                        dest: receiver_slot,
                        src: ret_local,
                    });
                } else if let Some(field_offset) = field_offset {
                    self.push_instr(Instr::ActorStateFieldStore {
                        field_offset,
                        src: ret_local,
                    });
                }
                // `m.step(ev)` is typed Unit at the call site (HIR
                // lower.rs:4949). No value is produced for the surrounding
                // expression; HIR-side evaluation of the assignment-like
                // statement records the call as `Unit`.
                None
            }
            HirExprKind::MachineStateName {
                machine_name,
                receiver,
            } => {
                // `m.state_name()` reads the machine's discriminant tag and
                // looks the state name up in a per-machine static string
                // table. The receiver must be a binding so codegen can read
                // its slot's tag field via `Place::MachineTag`.
                let src_place = self.lower_value(receiver)?;
                let Place::Local(src_local) = src_place else {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::UnsupportedNode {
                            reason: format!(
                                "MachineStateName receiver did not lower to a Place::Local; \
                                 got {src_place:?}"
                            ),
                        },
                        note: "state_name needs a stable alloca slot to read the tag from"
                            .to_string(),
                    });
                    return None;
                };
                let dest = self.alloc_local(ResolvedTy::String);
                self.push_instr(Instr::MachineStateName {
                    machine_name: machine_name.clone(),
                    src_local,
                    dest,
                });
                Some(dest)
            }
            HirExprKind::MachineTakeEmits {
                machine_name,
                receiver,
                event,
            } => {
                // `m.take_emits(ev)` filters the thread-local emit queue by
                // (this machine's stable type id, `ev`'s discriminant tag).
                // Delivery is per-thread, per-machine-TYPE — never
                // per-instance (see MACHINE-SPEC) — so the receiver is
                // lowered for its side effects only; its resulting place is
                // not read.
                let _ = self.lower_value(receiver)?;
                let event_place = self.lower_value(event)?;
                let event_tag = self.alloc_local(ResolvedTy::I64);
                self.push_instr(Instr::EnumTagLoad {
                    src: event_place,
                    dest: event_tag,
                });
                let dest = self.alloc_local(ResolvedTy::I64);
                self.push_instr(Instr::MachineEmitTake {
                    machine_emit_id: machine_emit_type_id(machine_name),
                    event_tag,
                    dest,
                });
                Some(dest)
            }
            HirExprKind::While {
                label,
                condition,
                body,
            } => self.lower_while(label.as_deref(), condition, body),
            HirExprKind::ForRange {
                label,
                binding,
                start,
                end,
                inclusive,
                step,
                descending,
                body,
            } => self.lower_for_range(
                label.as_deref(),
                binding,
                start,
                end,
                *inclusive,
                step,
                *descending,
                body,
            ),
            HirExprKind::Match { scrutinee, arms } => self.lower_match(scrutinee, arms, &expr.ty),
            HirExprKind::WhileLet {
                label,
                scrutinee,
                variant_idx,
                bindings,
                payload_variant_predicates,
                body,
                ..
            } => self.lower_while_let(
                label.as_deref(),
                scrutinee,
                *variant_idx,
                bindings,
                payload_variant_predicates,
                body,
            ),
            HirExprKind::IfLet {
                scrutinee,
                variant_idx,
                bindings,
                payload_variant_predicates,
                body,
                else_body,
                result_ty,
                ..
            } => self.lower_if_let(
                scrutinee,
                *variant_idx,
                bindings,
                payload_variant_predicates,
                body,
                else_body.as_ref(),
                result_ty,
            ),
            HirExprKind::Loop { label, body } => self.lower_loop(label.as_deref(), body),
            HirExprKind::Break { label, value } => {
                // Lower the operand for its side effects — `break value` does
                // not yield a loop value in this slice (loop-as-expression is
                // out of scope), so the resulting Place is discarded.
                if let Some(value) = value {
                    let _ = self.lower_value(value);
                }
                let frame = self.resolve_loop_frame(label.as_deref(), "break", expr.site)?;
                // Flush in-loop defers before leaving the loop (cleanup-all-exits).
                self.emit_defers_for_break_continue(frame.scope_depth);
                // Free the break-iteration's yielded heap value(s) on the break
                // edge (the body-end drop is past the break — would leak it).
                // Value before handle: the yielded buffer is inner heap, the
                // handle owns the coro frame + heap companion (LIFO inner-first).
                self.emit_generator_yield_value_drops_for_exit_edge(frame.scope_depth);
                self.record_active_iteration_owner_drops_for_exit_edge(frame.scope_depth);
                // Release in-loop generators on the break edge so the
                // break-iteration's coro frame + heap companion are not leaked.
                self.emit_generator_drops_for_break_continue(frame.scope_depth);
                // 3b-1 — close in-loop for-await stream cursors on this edge
                // (the block-scope close on the fall-through path is skipped).
                self.emit_stream_drops_for_exit_edge(frame.scope_depth);
                self.finish_current_block(Terminator::Goto {
                    target: frame.exit_target,
                });
                // Source following `break` lexically is dead; give it a home.
                let dead = self.alloc_block();
                self.start_dead_block(dead);
                None
            }
            HirExprKind::Return { value } => {
                // `return [expr]` in expression position. Reuse the EXACT
                // seal-and-dead-block discipline as `HirStmtKind::Return`
                // (LESSONS `one-construct-one-lowering-shell`): lower the
                // operand, move it to ReturnSlot BEFORE running defers (so
                // defers cannot corrupt the secured value), emit return-path
                // defers, then seal with `Terminator::Return` and start a fresh
                // dead cursor block for any lexically-following code. A `return`
                // diverges, so this expression yields no value (`None`).
                if let Some(expr_value) = value {
                    let value_place = self.lower_value(expr_value);
                    self.decide(expr_value);
                    self.mark_returned_binding_moved(expr_value);
                    self.statements.push(MirStatement::Return {
                        site: Some(expr_value.site),
                        ty: self.subst_ty(&expr_value.ty),
                    });
                    if let Some(src) = value_place {
                        self.push_instr(Instr::Move {
                            dest: Place::ReturnSlot,
                            src,
                        });
                    }
                } else {
                    self.statements.push(MirStatement::Return {
                        site: None,
                        ty: ResolvedTy::Unit,
                    });
                }
                self.emit_defers_for_return();
                // Release the current iteration's yielded value(s) on this
                // return edge — same discipline as the statement-position
                // return (`cleanup-all-exits`; the per-entry escape scan
                // keeps a `return v` caller-owned).
                self.emit_generator_yield_value_drops_for_exit_edge(0);
                self.emit_stream_drops_for_exit_edge(0);
                self.finish_current_block(Terminator::Return);
                let dead = self.alloc_block();
                self.start_dead_block(dead);
                None
            }
            HirExprKind::Continue { label } => {
                let frame = self.resolve_loop_frame(label.as_deref(), "continue", expr.site)?;
                // Flush in-loop defers before the back-edge (cleanup-all-exits).
                self.emit_defers_for_break_continue(frame.scope_depth);
                // Free the continued iteration's yielded heap value(s) on the
                // continue edge (the body-end drop is past the continue — would
                // leak it). Value before handle (LIFO inner-first).
                self.emit_generator_yield_value_drops_for_exit_edge(frame.scope_depth);
                self.record_active_iteration_owner_drops_for_exit_edge(frame.scope_depth);
                // Release in-loop generators on the continue edge so the
                // skipped iteration's coro frame + heap companion are not leaked.
                self.emit_generator_drops_for_break_continue(frame.scope_depth);
                // 3b-1 — close in-loop for-await stream cursors on this edge
                // (the block-scope close on the fall-through path is skipped).
                self.emit_stream_drops_for_exit_edge(frame.scope_depth);
                // Register THIS block as a loop back-edge so `enumerate_exits`
                // populates its `Goto` `DropPlan` with the scope-filtered
                // releases for body-scope heap-owning bindings (a live
                // `let opt = rx.try_recv()` carried into the next iteration
                // would otherwise leak its `Option<string>` payload because
                // the body-end Drop sits past the continue terminator and is
                // skipped). The fall-through back-edge is registered at the
                // bottom of each `lower_*` loop; this is the analogous
                // registration for the explicit-continue exit path.
                self.loop_back_edge_blocks
                    .insert(self.current_block_id, frame.body_scope);
                self.finish_current_block(Terminator::Goto {
                    target: frame.continue_target,
                });
                // Source following `continue` lexically is dead; give it a home.
                let dead = self.alloc_block();
                self.start_dead_block(dead);
                None
            }
            HirExprKind::RegexLiteralRef { literal_id, .. } => {
                // Standalone regex literal in value position (`let pat = re"..."`,
                // or passing `re"..."` to a function). The pattern was compiled
                // once at module init into `@hew_regex_handles[literal_id]`; here
                // we materialise the compiled `*HewRegex` handle into a fresh
                // `regex.Pattern` local so the value is usable through the stdlib
                // regex API (`pat.is_match(text)` etc.).
                //
                // Reuses the same id-keyed indirection the match-arm path uses:
                // a `ConstI64(literal_id)` local feeds a `CallRuntimeAbi` whose
                // codegen arm GEP-loads the handle from the global array. The
                // synthetic `hew_regex_handle` family GEP-loads the shared
                // module-static handle, clones it via `hew_regex_clone`, and
                // stores the clone into `dest`'s `Pattern.handle` field.
                // `regex.Pattern` is `#[resource]`, so `dest` is a resource-typed
                // local: normal scope-exit drop elaboration emits `close()` on it
                // like any other owned `Pattern`, releasing the clone (not the
                // shared literal-table entry) exactly once.
                let lit_local = self.alloc_local(ResolvedTy::I64);
                self.push_instr(Instr::ConstI64 {
                    dest: lit_local,
                    value: i64::from(*literal_id),
                });
                let handle_local = self.alloc_local(self.subst_ty(&expr.ty));
                match crate::model::RuntimeCall::new(
                    "hew_regex_handle",
                    vec![lit_local],
                    Some(handle_local),
                ) {
                    Ok(call) => self.push_instr(Instr::CallRuntimeAbi(call)),
                    Err(e) => {
                        // The symbol is in the allowlist; reaching here is a code
                        // invariant violation, not a user error.
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct: format!("hew_regex_handle runtime call: {e}"),
                                site: expr.site,
                            },
                            note: "hew_regex_handle must be in the runtime allowlist".to_string(),
                        });
                        return None;
                    }
                }
                Some(handle_local)
            }
            HirExprKind::GenBlock {
                body,
                yield_ty,
                return_ty,
                captures,
            } => {
                let gen_place = self.lower_gen_block(expr, body, yield_ty, return_ty, captures);
                // `receive gen fn` shell reshape: when this GenBlock is
                // the tail of a stream-producer handler shell, the freshly
                // constructed generator handle is consumed HERE by the pump —
                // driven to completion and forwarded element-by-element into the
                // sink — rather than returned to a caller. `lower_gen_block`
                // itself is UNCHANGED (env capture / MakeGenerator emission stay
                // identical to a standalone generator); only what happens to its
                // result differs. Evaluates to `None` (unit), matching `Yield`'s
                // own unit-in-body convention just below — `function_body`'s
                // existing `if let Some(src) = value_place { Move... }` already
                // skips the return-slot move for a `None` tail value.
                if let Some(pump) = self.stream_producer_pump.clone() {
                    // Register the generator companion `gen_place` with the
                    // drop-elaboration authority so `hew_gen_coro_destroy` fires
                    // on EVERY pump exit (Return / Panic / Cancel) instead of
                    // leaking its coro frame + heap companion. THIS branch (pump
                    // context) consumes the handle in place — nothing else owns
                    // it — so this is the sole release authority.
                    //
                    // Scoped to the pump branch ONLY: in the standalone `else`
                    // branch `gen_place` is the expression value, moved OUT to
                    // the caller's `let g = <genblock>` binding, which already
                    // owns and drops it. Registering there — or inside the
                    // shared `lower_gen_block` — would elaborate a second drop
                    // over a moved-out handle: the #2384 double-free class. It
                    // stays here, and `lower_gen_block` registers nothing.
                    let companion = SENTINEL_RECV_GEN_COMPANION_BINDING;
                    let companion_name = "__hew_recv_gen_companion".to_string();
                    let companion_ty = self.subst_ty(&expr.ty);
                    self.statements.push(MirStatement::Bind {
                        binding: companion,
                        name: companion_name.clone(),
                        site: expr.site,
                        ty: companion_ty.clone(),
                    });
                    // Wire `binding_locals` BEFORE `register_owned_local`:
                    // `register_owned_local` reads the slot to classify
                    // ownership, and drop elaboration resolves the drop place
                    // from `binding_locals` at exit. A missing slot silently
                    // defaults to `Place::Local(0)` — dropping the WRONG local.
                    self.binding_locals.insert(companion, gen_place);
                    self.record_binding_scope(companion);
                    self.register_owned_local(companion, companion_name, companion_ty);
                    self.build_stream_producer_pump(gen_place, &pump);
                    None
                } else {
                    Some(gen_place)
                }
            }
            HirExprKind::Yield { value, yield_ty: _ } => {
                self.lower_yield_expr(expr, value.as_deref())
            }
            // Deep-clone a user record via the synthesised thunk pair.
            // See `Instr::RecordCloneInplace` for the full protocol.
            HirExprKind::RecordCloneCall {
                src, record_name, ..
            } => {
                let src_place = self.lower_value(src)?;
                let record_ty = self.subst_ty(&expr.ty);
                // A clone of a bare type parameter
                // (`fn f<T: Clone>(x: T) -> T { x.clone() }`) is admitted by the
                // checker through the record-clone rewrite, but after
                // monomorphisation the concrete `T` is frequently a NON-record
                // value. The record-thunk protocol below only lowers user
                // structs, so the concrete non-record monomorphisations are
                // dispatched here by value class. Concrete user records and the
                // abstract origin bucket — where `record_ty` is
                // `ResolvedTy::TypeParam`, value-class `Unknown` — fall through
                // to the byte-identical thunk path, so existing behaviour is
                // unchanged.
                //
                // RecordCloneCall is only produced for a `Named { builtin: None }`
                // receiver (a user record or a bare type parameter), so a
                // concrete `string`/`Vec`/tuple `clone` never reaches these arms
                // — only a type-parameter monomorphisation does.
                //
                // A `BitCopy` value (`i64`/`bool`/…, a `#[copy]` record)
                // duplicates on every use, so the clone is the source place
                // itself, exactly what the concrete scalar `CopyCloneNoop` path
                // yields. No new owner is created, so no drop is owed.
                if ValueClass::of_ty(&record_ty, &self.type_classes) == ValueClass::BitCopy {
                    return Some(src_place);
                }
                // A `string` is a refcounted copy-on-write owner. `clone`
                // produces an independent `+1` owner via `hew_string_clone` (a
                // header-aware refcount bump). Emitting it as a `Terminator::Call`
                // — the `hew_hashmap_get_clone_layout` pattern — seeds the dest as
                // a `fresh_string_producer_term_dest`, so drop-elaboration adds
                // the symmetric `hew_string_drop` in all three exit contexts
                // (sync return, async cancel, actor shutdown). A plain read would
                // alias the source at rc==1 and double-free when both are dropped
                // (`by-value-heap-params-are-borrows` P0), so the explicit clone
                // is load-bearing here.
                if is_string_const_ty(&record_ty) {
                    let dest = self.alloc_local(record_ty);
                    let next = self.alloc_block();
                    self.finish_current_block(Terminator::Call {
                        callee: "hew_string_clone".to_string(),
                        builtin: None,
                        args: vec![src_place],
                        dest: Some(dest),
                        next,
                    });
                    self.start_block(next);
                    return Some(dest);
                }
                // A type parameter that monomorphises to a builtin heap value
                // (`Vec`/`HashMap`/`HashSet`/`bytes`/tuple/array) needs the
                // owned-clone + element-drop machinery the concrete `clone` path
                // drives from the checker. Synthesising it from a bare type
                // parameter here would risk an unbalanced retain/drop, so fail
                // closed loudly rather than admit a clone we cannot yet lower
                // safely (`admit-only-what-you-lower`,
                // `unclonable-leaf-fails-closed-transitively`). The generic
                // record-of-`Vec` field surface is tracked under its own issue;
                // generic enum clone is handled by the `EnumCloneInplace` arm
                // below.
                if matches!(
                    record_ty,
                    ResolvedTy::Bytes
                        | ResolvedTy::Tuple(_)
                        | ResolvedTy::Array(_, _)
                        | ResolvedTy::Named {
                            builtin: Some(
                                BuiltinType::Vec | BuiltinType::HashMap | BuiltinType::HashSet
                            ),
                            ..
                        }
                ) {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::NotYetImplemented {
                            construct: format!(
                                "clone of a generic type parameter monomorphised to `{record_ty}` \
                                 is not yet lowered; clone of a `Clone`-bound type parameter \
                                 currently supports scalar, `string`, and user-record \
                                 instantiations"
                            ),
                            site: expr.site,
                        },
                        note: "a type parameter that resolves to a Vec/HashMap/HashSet/bytes/\
                               tuple/array clone is not yet synthesised from a bare parameter"
                            .to_string(),
                    });
                    return None;
                }
                // An enum monomorphisation routes to the enum twin of the
                // record thunk. `clone <enum>` — a top-level enum, or (defence
                // in depth) a `Clone`-bound type parameter that monomorphises to
                // one — lowers to `EnumCloneInplace`, keyed by the SAME
                // monomorphised tagged-union layout the drop side keys
                // (`Maybe$$i64` for a generic instantiation, the bare name for a
                // monomorphic enum). Codegen emits the memcpy +
                // `__hew_enum_clone_inplace_<E>` + trap protocol and seeds the
                // clone/drop helper PAIR together, so the scope-exit drop of
                // `dest` stays symmetric (no leak, no double-free).
                // `enum_clone_layout_key` returns `Some` only for a registered
                // enum; the record and enum layout registries are disjoint, so
                // this never shadows a record clone, and a `TypeParam` (the
                // abstract origin bucket) is not `Named` so it falls through.
                if let Some(enum_key) = self.enum_clone_layout_key(&record_ty) {
                    let dest = self.alloc_local(record_ty);
                    self.instructions.push(Instr::EnumCloneInplace {
                        dest,
                        src: src_place,
                        enum_name: enum_key,
                    });
                    return Some(dest);
                }
                // Key the clone thunk by the MONOMORPHISED record layout: a
                // generic instantiation (`clone Pair<i64, i64>`) must resolve
                // `__hew_record_clone_inplace_Pair$$i64$i64`, not the bare
                // `Pair` — the bare name names no monomorphic layout, so the
                // call resolves a declared-but-undefined thunk that fails LLVM
                // verify. A monomorphic record keeps its bare declared name
                // BYTE-IDENTICALLY via the `record_name.clone()` arm, so
                // monomorphic goldens and behaviour are unchanged. The drop
                // side already keys via the same `user_record_layout_key`
                // helper (`record_inplace_drop_name`), so the clone/drop thunk
                // PAIR stays symmetric per instantiation.
                let layout_name = match monomorphic_user_record_key(&record_ty) {
                    Some(_) => record_name.clone(),
                    None => {
                        user_record_layout_key(&record_ty).unwrap_or_else(|| record_name.clone())
                    }
                };
                let dest = self.alloc_local(record_ty);
                self.instructions.push(Instr::RecordCloneInplace {
                    dest,
                    src: src_place,
                    record_name: layout_name,
                });
                Some(dest)
            }
            HirExprKind::Unsupported(reason) => {
                // Defense-in-depth: HIR lowering should have emitted
                // NotYetImplemented and the driver should have stopped
                // before reaching MIR. Emit a MirDiagnostic so the pipeline
                // is still rejected if somehow the gate was bypassed.
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::UnsupportedNode {
                        reason: reason.clone(),
                    },
                    note: "HIR Unsupported node reached MIR lowering; \
                           NotYetImplemented should have been caught earlier"
                        .to_string(),
                });
                None
            }
        }
    }

    #[allow(
        clippy::too_many_lines,
        reason = "single match over every HirLiteral variant; splitting would scatter the literal-lowering authority"
    )]
    #[allow(
        clippy::cast_precision_loss,
        reason = "an integer literal accepted by the checker in an f32/f64 context is converted to that float type; literals are within exact range at written precision"
    )]
    fn lower_literal(
        &mut self,
        lit: &HirLiteral,
        ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        // All HirLiteral variants are wired. Each arm allocates a dest local,
        // pushes the corresponding Instr, and returns early with `Some(dest)`.
        // Fail-closed behaviour (LESSONS `boundary-fail-closed`) is preserved
        // through the float arm's type-mismatch guard, which still returns
        // `None` on checker-invariant violations.
        match lit {
            HirLiteral::Integer(value) => {
                if matches!(ty, ResolvedTy::F32 | ResolvedTy::F64) {
                    let (value_bits, width) = match ty {
                        ResolvedTy::F32 => {
                            #[allow(
                                clippy::cast_possible_truncation,
                                reason = "checker accepted this integer literal in an f32 context"
                            )]
                            let narrowed = *value as f32;
                            (u64::from(narrowed.to_bits()), FloatWidth::F32)
                        }
                        ResolvedTy::F64 => ((*value as f64).to_bits(), FloatWidth::F64),
                        _ => unreachable!("guarded by matches! above"),
                    };
                    let dest = self.alloc_local(ty.clone());
                    self.push_instr(Instr::FloatLit {
                        dest,
                        value_bits,
                        width,
                    });
                    return Some(dest);
                }
                let dest = self.alloc_local(ty.clone());
                self.push_instr(Instr::ConstI64 {
                    dest,
                    value: *value,
                });
                Some(dest)
            }
            HirLiteral::Bool(value) => {
                // Bool lowers as an integer truth value (1 / 0) into the
                // dest local's natural width. The dest local's type is
                // whatever HIR resolved for the literal — `ResolvedTy::Bool`
                // on this base, which the codegen maps to i8. The
                // `ConstI64.value` is fed through the same store path as
                // ConstI64 for integer literals; `Instr::ConstI64`'s
                // emitter already truncates to the dest local's width.
                let dest = self.alloc_local(ty.clone());
                self.push_instr(Instr::ConstI64 {
                    dest,
                    value: i64::from(*value),
                });
                Some(dest)
            }
            HirLiteral::Float(value) => {
                // `HirLiteral::Float` always carries an `f64` regardless of
                // the declared type. When the resolved type is `f32`, narrow
                // to single precision before encoding as a bit pattern so the
                // constant round-trips exactly through the MIR → codegen boundary.
                // Storing as bits avoids a floating-point field in the MIR model
                // (which would need special PartialEq treatment for NaN) while
                // keeping the round-trip exact (mirrors `ConstI64.value`).
                let (value_bits, width) = match ty {
                    ResolvedTy::F32 => {
                        // Narrow to f32 before encoding — f64 bits for a value
                        // that will be stored in an f32 slot would be wrong.
                        #[allow(
                            clippy::cast_possible_truncation,
                            reason = "literal coercion from f64 source value to f32 slot is \
                                      the intended semantics; checker accepted the source as \
                                      f32, so any precision loss is the developer's call"
                        )]
                        let narrowed = *value as f32;
                        (u64::from(narrowed.to_bits()), FloatWidth::F32)
                    }
                    ResolvedTy::F64 => (value.to_bits(), FloatWidth::F64),
                    _ => {
                        // Type mismatch: float literal with non-float resolved
                        // type is a checker bug. Fail closed per LESSONS
                        // `boundary-fail-closed`.
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct: "float literal with non-float resolved type".to_string(),
                                site,
                            },
                            note: "HirLiteral::Float reached MIR lowering with a \
                                   non-float resolved type — checker invariant violated"
                                .to_string(),
                        });
                        return None;
                    }
                };
                let dest = self.alloc_local(ty.clone());
                self.push_instr(Instr::FloatLit {
                    dest,
                    value_bits,
                    width,
                });
                Some(dest)
            }
            HirLiteral::String(s) => {
                // String literal lowering: allocate a `ResolvedTy::String`
                // local (an opaque pointer at the LLVM level) and emit
                // `Instr::StringLit` to fill it. The codegen emitter will
                // produce an LLVM global constant for the bytes + a pointer
                // store into the dest alloca.
                //
                // Escape decoding: the parser's `unescape_string` already
                // ran; `s` is a decoded Rust String and `as_bytes()` gives
                // the correct UTF-8 byte sequence.
                let dest = self.alloc_local(ty.clone());
                self.push_instr(Instr::StringLit {
                    bytes: s.as_bytes().to_vec(),
                    dest,
                });
                Some(dest)
            }
            HirLiteral::Char(c) => {
                // Hew `char` is a Unicode scalar value. Store as `u32` bit
                // pattern; codegen maps it to an `i32` constant. The cast is
                // total — Rust's `char` guarantees scalar-value range.
                let dest = self.alloc_local(ty.clone());
                self.push_instr(Instr::CharLit {
                    value: *c as u32,
                    dest,
                });
                Some(dest)
            }
            HirLiteral::Unit => {
                // Unit is zero-sized; codegen may emit nothing. The dest
                // place is allocated so that any downstream use-after-consume
                // tracking has a definition point.
                //
                // NOTE: `HirLiteral::Unit` is currently unreachable from
                // real Hew source — no parser `Literal::Unit` exists and
                // the HIR lowerer does not produce this variant. This arm
                // exists for exhaustiveness so a future producer has a
                // corresponding MIR variant.
                let dest = self.alloc_local(ty.clone());
                self.push_instr(Instr::UnitLit { dest });
                Some(dest)
            }
            HirLiteral::Duration(nanos) => {
                // Duration literals carry nanoseconds already (`i64`) from
                // parse time. Forward directly — no conversion needed.
                let dest = self.alloc_local(ty.clone());
                self.push_instr(Instr::DurationLit {
                    nanos: *nanos,
                    dest,
                });
                Some(dest)
            }
            HirLiteral::Bytes(data) => {
                // Bytes literal — `bytes[0x41, 0x42]` or `b"AB"`.
                // Allocate a `ResolvedTy::Bytes` local and emit `Instr::BytesLit`.
                // Codegen will emit an LLVM global constant for the raw bytes and
                // call `hew_bytes_from_static(ptr, len)` to build the
                // refcounted `BytesTriple` at runtime.
                let dest = self.alloc_local(ty.clone());
                self.push_instr(Instr::BytesLit {
                    bytes: data.clone(),
                    dest,
                });
                Some(dest)
            }
        }
    }

    #[allow(
        clippy::too_many_lines,
        reason = "lower_binary is a flat dispatch over the BinaryOp enum; line count grows \
                  with the operator set (i64 + float arms). Splitting would obscure the \
                  per-operator codegen path each reader expects to find here."
    )]
    #[allow(
        clippy::too_many_arguments,
        reason = "comparison/arithmetic lowering needs op, both operand places, both operand types, result type, and site"
    )]
    fn lower_binary(
        &mut self,
        op: BinaryOp,
        lhs: Place,
        rhs: Place,
        lhs_ty: &ResolvedTy,
        rhs_ty: &ResolvedTy,
        ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        let dest = self.alloc_local(ty.clone());
        // Comparison binops: lower to `Instr::IntCmp` with a `CmpPred`
        // discriminator. The result Place is allocated to whatever type
        // HIR resolved for the expression (`ResolvedTy::Bool` for cmp
        // ops); codegen widens the LLVM `i1` cmp result to the dest's
        // stored width on the way to the store. Without this arm,
        // `if 1 == 1 { ... }` cannot construct a condition Place for
        // CFG-construction-lane `If` lowering — the boolean-condition
        // pre-requisite called out by the cluster plan §1 / Slice 0.
        //
        // Ordering predicates (`< <= > >=`) start as `Signed*`; after
        // resolving operand types below, `cmp_select_by_signedness`
        // upgrades them to `Unsigned*` for unsigned integer operands so
        // that high-bit-set values (e.g. `0x8000…u64 > 1`) compare
        // correctly.  `Eq`/`NotEq` are bit-equality and stay unchanged.
        let cmp_pred = match op {
            BinaryOp::Equal => Some(CmpPred::Eq),
            BinaryOp::NotEqual => Some(CmpPred::NotEq),
            BinaryOp::Less => Some(CmpPred::SignedLess),
            BinaryOp::LessEqual => Some(CmpPred::SignedLessEq),
            BinaryOp::Greater => Some(CmpPred::SignedGreater),
            BinaryOp::GreaterEqual => Some(CmpPred::SignedGreaterEq),
            _ => None,
        };
        if let Some(pred) = cmp_pred {
            let lhs_ty = self.subst_ty(lhs_ty);
            let rhs_ty = self.subst_ty(rhs_ty);
            // Select the predicate signed/unsigned variant based on
            // operand signedness.  `Eq`/`NotEq` are signedness-agnostic
            // and pass through unchanged.  The checker rejects mixed-sign
            // comparisons upstream, so both operands always have matching
            // signedness here; if they don't, fail closed — undo the dest
            // alloc so the local table stays coherent.
            let Some(pred) = cmp_select_by_signedness(pred, &lhs_ty, &rhs_ty) else {
                self.locals.pop();
                return None;
            };
            if matches!(pred, CmpPred::Eq | CmpPred::NotEq)
                && self.is_fieldless_enum_comparison(&lhs_ty, &rhs_ty)
            {
                let (Place::Local(lhs_local), Place::Local(rhs_local)) = (lhs, rhs) else {
                    self.locals.pop();
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::NotYetImplemented {
                            construct: "enum equality over non-local operands".to_string(),
                            site,
                        },
                        note: "fieldless enum equality lowers through the tagged-union \
                               discriminant; operands must first materialise as enum locals"
                            .to_string(),
                    });
                    return None;
                };
                let lhs_tag = self.alloc_local(ResolvedTy::I64);
                self.instructions.push(Instr::Move {
                    dest: lhs_tag,
                    src: Place::EnumTag(lhs_local),
                });
                let rhs_tag = self.alloc_local(ResolvedTy::I64);
                self.instructions.push(Instr::Move {
                    dest: rhs_tag,
                    src: Place::EnumTag(rhs_local),
                });
                self.instructions.push(Instr::IntCmp {
                    dest,
                    pred,
                    lhs: lhs_tag,
                    rhs: rhs_tag,
                });
                return Some(dest);
            }
            if matches!(pred, CmpPred::Eq | CmpPred::NotEq)
                && self.is_structural_eq_comparison(&lhs_ty, &rhs_ty)
            {
                if !matches!((lhs, rhs), (Place::Local(_), Place::Local(_))) {
                    self.locals.pop();
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::NotYetImplemented {
                            construct: "structural equality over non-local operands".to_string(),
                            site,
                        },
                        note: "structural equality lowers by passing aggregate local addresses to \
                               the codegen equality thunk"
                            .to_string(),
                    });
                    return None;
                }
                // Keep using `IntCmp` as the MIR carrier: the checker has
                // admitted only equality-eligible aggregates, and codegen routes
                // aggregate-typed Eq/NotEq operands to the structural equality
                // thunk instead of integer `icmp`.
                self.push_instr(Instr::IntCmp {
                    dest,
                    pred,
                    lhs,
                    rhs,
                });
                return Some(dest);
            }
            if let (Some(lhs_width), Some(rhs_width)) = (float_width(&lhs_ty), float_width(&rhs_ty))
            {
                if lhs_width == rhs_width {
                    self.push_instr(Instr::FloatCmp {
                        dest,
                        pred,
                        lhs,
                        rhs,
                        width: lhs_width,
                    });
                    return Some(dest);
                }
            }
            self.push_instr(Instr::IntCmp {
                dest,
                pred,
                lhs,
                rhs,
            });
            return Some(dest);
        }
        // B-4 wrapping arithmetic: `&+` / `&-` / `&*` lower to plain
        // two's-complement `IntAdd` / `IntSub` / `IntMul` — no overflow
        // flag, no CFG split, no Trap block. These are the first source-
        // level producers of `Instr::IntAdd/IntSub/IntMul`; previously
        // those variants were reachable only from hand-built fixtures.
        // LESSONS `boundary-fail-closed` (P0): the user has explicitly
        // opted into modular arithmetic by writing `&+`; no trap is the
        // correct behaviour here.
        let wrapping_instr = match op {
            BinaryOp::WrappingAdd => Some(Instr::IntAdd { dest, lhs, rhs }),
            BinaryOp::WrappingSub => Some(Instr::IntSub { dest, lhs, rhs }),
            BinaryOp::WrappingMul => Some(Instr::IntMul { dest, lhs, rhs }),
            _ => None,
        };
        if let Some(instr) = wrapping_instr {
            self.push_instr(instr);
            return Some(dest);
        }

        // B-5 divide / modulo / shift lowering.
        //
        // These operators are handled here with early returns so they
        // don't fall through to the B-2 overflow-trap `IntArithChecked`
        // path below (which is only for `+`/`-`/`*`).
        match op {
            BinaryOp::Divide | BinaryOp::Modulo => {
                return self.lower_div_rem(op, dest, lhs, rhs, ty, site);
            }
            BinaryOp::Shl | BinaryOp::Shr => {
                return self.lower_shift(op, dest, lhs, rhs, ty, site);
            }
            _ => {}
        }

        // Bitwise operators: well-defined for any integer width × signedness.
        // No traps, no overflow checks — emit a single instruction directly.
        let bitwise_instr = match op {
            BinaryOp::BitAnd => Some(Instr::IntBitAnd { dest, lhs, rhs }),
            BinaryOp::BitOr => Some(Instr::IntBitOr { dest, lhs, rhs }),
            BinaryOp::BitXor => Some(Instr::IntBitXor { dest, lhs, rhs }),
            _ => None,
        };
        if let Some(instr) = bitwise_instr {
            self.push_instr(instr);
            return Some(dest);
        }

        let arith_op = match op {
            BinaryOp::Add => IntArithOp::Add,
            BinaryOp::Subtract => IntArithOp::Sub,
            BinaryOp::Multiply => IntArithOp::Mul,
            // The spine subset still rejects range / send / regex binops.
            // Previously this arm silently popped the dest local and returned
            // `None`, letting the parent expression succeed with a missing
            // producer (quiet fail-soft — caller's `decide` ran,
            // `MirDiagnostic` did not). Fail closed now: drop the dest local,
            // emit a `NotYetImplemented` so the CLI rejection surface sees
            // the offending construct, and return `None`.
            // LESSONS `boundary-fail-closed`.
            _ => {
                self.locals.pop();
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!("binary operator `{op}`"),
                        site,
                    },
                    note: "binary operator is recognised by HIR but not yet lowered \
                           to the backend instruction stream"
                        .to_string(),
                });
                return None;
            }
        };
        // Float `+` / `-` / `*`: emit `Instr::Float{Add,Sub,Mul}` directly —
        // no trap blocks, no overflow flag. IEEE 754 overflow produces
        // ±inf, not a runtime trap.
        if let Some(width) = float_width(ty) {
            let float_instr = match arith_op {
                IntArithOp::Add => Instr::FloatAdd {
                    dest,
                    lhs,
                    rhs,
                    width,
                },
                IntArithOp::Sub => Instr::FloatSub {
                    dest,
                    lhs,
                    rhs,
                    width,
                },
                IntArithOp::Mul => Instr::FloatMul {
                    dest,
                    lhs,
                    rhs,
                    width,
                },
            };
            self.push_instr(float_instr);
            return Some(dest);
        }

        if matches!(op, BinaryOp::Add) && matches!(ty, ResolvedTy::String) {
            self.push_instr(Instr::CallRuntimeAbi(
                crate::model::RuntimeCall::new("hew_string_concat", vec![lhs, rhs], Some(dest))
                    .expect("hew_string_concat is an allowlisted runtime symbol"),
            ));
            return Some(dest);
        }

        // B-2 overflow-trap lowering. The default `+` / `-` / `*` on
        // integer types lowers to the checked LLVM intrinsic family
        // (`llvm.{s,u}{add,sub,mul}.with.overflow.iN`) with a hard
        // `Terminator::Trap { kind: TrapKind::IntegerOverflow }` on
        // the overflow path and a continuation block on the success
        // path. The MIR-level CFG split — current block ends with a
        // `Branch` on the overflow flag, with a trap block and a
        // continuation block as successors — is what makes the trap
        // visible to drop elaboration, the cross-block dataflow pass,
        // and every other MIR consumer (instead of being a codegen-
        // only emission). LESSONS `boundary-fail-closed` (P0 —
        // default arithmetic IS the boundary; trap-on-overflow is
        // fail-closed for accidental overflow).
        let Some(signed) = integer_signedness(ty) else {
            // Non-integer, non-float reaching `+` / `-` / `*` is a
            // B-1 mixed-width or unsupported-type violation upstream.
            // Fail closed rather than emit unchecked arithmetic.
            self.locals.pop();
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("binary operator `{op}` on non-integer, non-float type"),
                    site,
                },
                note: "overflow-trap lowering requires an integer-typed result \
                       (i8/i16/i32/i64/u8/u16/u32/u64/isize/usize)"
                    .to_string(),
            });
            return None;
        };
        // Allocate the overflow-flag local as a bool. Codegen widens
        // the i1 returned by `extractvalue` to the i8 backing slot.
        let overflow_flag = self.alloc_local(ResolvedTy::Bool);
        self.push_instr(Instr::IntArithChecked {
            op: arith_op,
            signed,
            dest,
            lhs,
            rhs,
            overflow_flag,
        });
        // Seal the current block with a Branch on the overflow flag.
        // Then-target is the trap block; else-target is the
        // continuation block that subsequent lowering writes into.
        let trap_bb = self.alloc_block();
        let cont_bb = self.alloc_block();
        self.finish_current_block(Terminator::Branch {
            cond: overflow_flag,
            then_target: trap_bb,
            else_target: cont_bb,
        });
        // Trap block: a single Terminator::Trap with no instructions.
        self.start_block(trap_bb);
        self.finish_current_block(Terminator::Trap {
            kind: TrapKind::IntegerOverflow,
        });
        // Continuation block: the cursor lands here so the parent
        // expression's caller can keep emitting into the success path.
        self.start_block(cont_bb);
        Some(dest)
    }

    fn lower_unary(
        &mut self,
        op: UnaryOp,
        operand: &HirExpr,
        operand_ty: &ResolvedTy,
        result_ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        let operand_place = self.lower_value(operand)?;
        let dest = self.alloc_local(result_ty.clone());
        match op {
            UnaryOp::Not if operand_ty == &ResolvedTy::Bool && result_ty == &ResolvedTy::Bool => {
                self.push_instr(Instr::BoolNot {
                    dest,
                    operand: operand_place,
                });
                Some(dest)
            }
            UnaryOp::Negate if operand_ty == result_ty => {
                if let Some(width) = float_width(result_ty) {
                    self.push_instr(Instr::FloatNeg {
                        dest,
                        operand: operand_place,
                        width,
                    });
                    return Some(dest);
                }
                let Some(signed) = integer_signedness(result_ty) else {
                    self.locals.pop();
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::NotYetImplemented {
                            construct: format!("unary `-` on non-numeric type `{result_ty}`"),
                            site,
                        },
                        note: "unary negation requires an integer or float result type".to_string(),
                    });
                    return None;
                };
                let overflow_flag = self.alloc_local(ResolvedTy::Bool);
                self.push_instr(Instr::IntNegChecked {
                    signed,
                    dest,
                    operand: operand_place,
                    overflow_flag,
                });
                let trap_bb = self.alloc_block();
                let cont_bb = self.alloc_block();
                self.finish_current_block(Terminator::Branch {
                    cond: overflow_flag,
                    then_target: trap_bb,
                    else_target: cont_bb,
                });
                self.start_block(trap_bb);
                self.finish_current_block(Terminator::Trap {
                    kind: TrapKind::IntegerOverflow,
                });
                self.start_block(cont_bb);
                Some(dest)
            }
            UnaryOp::BitNot
                if operand_ty == result_ty && integer_signedness(result_ty).is_some() =>
            {
                self.push_instr(Instr::IntBitNot {
                    dest,
                    operand: operand_place,
                });
                Some(dest)
            }
            UnaryOp::RawDeref | UnaryOp::Not | UnaryOp::Negate | UnaryOp::BitNot => {
                self.locals.pop();
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!(
                            "unary operator `{}` for operand `{operand_ty}` -> `{result_ty}`",
                            unary_op_label(op)
                        ),
                        site,
                    },
                    note: "HIR unary node carried a typed shape the MIR producer does not support"
                        .to_string(),
                });
                None
            }
        }
    }

    /// Lower integer `/` and `%` with divide-by-zero and (for signed
    /// types) signed-MIN/-1 trap guards.
    ///
    /// CFG shape:
    ///
    /// ```text
    /// entry_bb (current)
    ///   IntCmp { pred: Eq, dest: zero_flag, lhs: rhs, rhs: const_0 }
    ///   Branch { cond: zero_flag, then: dbz_trap_bb, else: after_zero_bb }
    ///
    /// dbz_trap_bb
    ///   Trap { kind: DivideByZero }
    ///
    /// after_zero_bb  [signed only]
    ///   IntCmp { pred: Eq, dest: min_flag, lhs: lhs, rhs: const_MIN }
    ///   Branch { cond: min_flag, then: min_check_bb, else: div_bb }
    ///
    /// min_check_bb   [signed only]
    ///   IntCmp { pred: Eq, dest: negone_flag, lhs: rhs, rhs: const_NEG1 }
    ///   Branch { cond: negone_flag, then: smno_trap_bb, else: div_bb }
    ///
    /// smno_trap_bb   [signed only]
    ///   Trap { kind: SignedMinDivNegOne }
    ///
    /// div_bb
    ///   IntDiv / IntRem { dest, lhs, rhs }
    ///   [cursor stays here for subsequent lowering]
    /// ```
    ///
    /// For unsigned types the after-zero block is `div_bb` directly.
    ///
    /// `dest` must already be allocated by the caller (`lower_binary`
    /// allocates it before dispatching here).
    #[allow(
        clippy::too_many_arguments,
        reason = "all arguments are structurally required: the builder state \
                  (&mut self), the opcode discriminator (op), the pre-allocated \
                  destination place (dest), both operand places (lhs, rhs), the \
                  result type (ty) for constant-emission width, and the site id \
                  for diagnostics. There is no natural grouping that reduces this."
    )]
    #[allow(
        clippy::too_many_lines,
        reason = "the function implements a single coherent CFG-emission \
                  pattern (zero-check → MIN/-1 check → div/rem) that must \
                  stay in one place for readability; extracting sub-steps \
                  would require passing more builder state around."
    )]
    fn lower_div_rem(
        &mut self,
        op: BinaryOp,
        dest: Place,
        lhs: Place,
        rhs: Place,
        ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        // Float `/` and `%`: emit `Instr::FloatDiv` / `Instr::FloatRem`
        // directly. IEEE 754 defines `x / 0.0` → ±inf and `x % 0.0` →
        // NaN — neither traps. Do NOT add a zero-check CFG split here.
        if let Some(width) = float_width(ty) {
            let float_instr = match op {
                BinaryOp::Divide => Instr::FloatDiv {
                    dest,
                    lhs,
                    rhs,
                    width,
                },
                BinaryOp::Modulo => Instr::FloatRem {
                    dest,
                    lhs,
                    rhs,
                    width,
                },
                _ => unreachable!("lower_div_rem called with non-div/rem op"),
            };
            self.push_instr(float_instr);
            return Some(dest);
        }

        let Some(signed) = integer_signedness(ty) else {
            // Non-integer, non-float reaching `/` or `%` — B-1 violation upstream.
            self.locals.pop();
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("binary operator `{op}` on non-integer, non-float type"),
                    site,
                },
                note: "div/rem trap lowering requires an integer-typed result".to_string(),
            });
            return None;
        };

        // ── divide-by-zero check ────────────────────────────────────
        let zero_const = self.alloc_local(ty.clone());
        self.push_instr(Instr::ConstI64 {
            dest: zero_const,
            value: 0,
        });
        let zero_flag = self.alloc_local(ResolvedTy::Bool);
        self.push_instr(Instr::IntCmp {
            dest: zero_flag,
            pred: CmpPred::Eq,
            lhs: rhs,
            rhs: zero_const,
        });
        let dbz_trap_bb = self.alloc_block();
        let after_zero_bb = self.alloc_block();
        self.finish_current_block(Terminator::Branch {
            cond: zero_flag,
            then_target: dbz_trap_bb,
            else_target: after_zero_bb,
        });

        self.start_block(dbz_trap_bb);
        self.finish_current_block(Terminator::Trap {
            kind: TrapKind::DivideByZero,
        });

        self.start_block(after_zero_bb);

        // ── signed-MIN / -1 check (signed types only) ───────────────
        if signed == IntSignedness::Signed {
            // `signed_min_value` resolves every signed integer type, including
            // the platform-sized `Isize` (via the target pointer width). A
            // `None` here means a signed-classified type with no MIN — an
            // upstream classification bug — so we fail closed rather than emit
            // a div/rem path with no signed-MIN/-1 guard.
            let Some(min_val) = signed_min_value(ty, self.pointer_width) else {
                self.locals.pop();
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!("binary operator `{op}` on signed type `{ty:?}`"),
                        site,
                    },
                    note: "signed-MIN/-1 trap requires a known signed minimum; \
                           integer_signedness classified this type as signed but \
                           signed_min_value has no arm for it."
                        .to_string(),
                });
                return None;
            };
            let min_const = self.alloc_local(ty.clone());
            self.push_instr(Instr::ConstI64 {
                dest: min_const,
                value: min_val,
            });
            let min_flag = self.alloc_local(ResolvedTy::Bool);
            self.push_instr(Instr::IntCmp {
                dest: min_flag,
                pred: CmpPred::Eq,
                lhs,
                rhs: min_const,
            });
            let min_check_bb = self.alloc_block();
            let div_bb = self.alloc_block();
            self.finish_current_block(Terminator::Branch {
                cond: min_flag,
                then_target: min_check_bb,
                else_target: div_bb,
            });

            // min_check_bb: check whether rhs == -1
            self.start_block(min_check_bb);
            let negone_const = self.alloc_local(ty.clone());
            self.push_instr(Instr::ConstI64 {
                dest: negone_const,
                value: -1,
            });
            let negone_flag = self.alloc_local(ResolvedTy::Bool);
            self.push_instr(Instr::IntCmp {
                dest: negone_flag,
                pred: CmpPred::Eq,
                lhs: rhs,
                rhs: negone_const,
            });
            let smno_trap_bb = self.alloc_block();
            self.finish_current_block(Terminator::Branch {
                cond: negone_flag,
                then_target: smno_trap_bb,
                else_target: div_bb,
            });

            self.start_block(smno_trap_bb);
            self.finish_current_block(Terminator::Trap {
                kind: TrapKind::SignedMinDivNegOne,
            });

            self.start_block(div_bb);
        }

        // ── div / rem instruction on the safe path ──────────────────
        match op {
            BinaryOp::Divide => self.push_instr(Instr::IntDiv {
                signed,
                dest,
                lhs,
                rhs,
            }),
            BinaryOp::Modulo => self.push_instr(Instr::IntRem {
                signed,
                dest,
                lhs,
                rhs,
            }),
            _ => unreachable!("lower_div_rem called only for Divide / Modulo"),
        }
        Some(dest)
    }

    /// Lower `<<` and `>>` with a shift-out-of-range trap guard.
    ///
    /// The range check uses an unsigned ≥ compare on the shift count:
    ///   `(count as unsigned) >= bit_width(T)`
    /// This single compare catches both negative counts (which become
    /// large unsigned values after reinterpretation) and counts ≥ the
    /// type's width.
    ///
    /// `isize`/`usize` are rejected with `NotYetImplemented` because
    /// the bit-width is not statically known at MIR time (see
    /// `integer_bit_width` for the documented why / when-obsolete).
    ///
    /// CFG shape:
    /// ```text
    /// entry_bb (current)
    ///   ConstI64 { dest: width_const, value: bit_width }
    ///   IntCmp { pred: UnsignedGreaterEq, dest: oor_flag,
    ///            lhs: rhs (shift count), rhs: width_const }
    ///   Branch { cond: oor_flag, then: sor_trap_bb, else: shift_bb }
    ///
    /// sor_trap_bb
    ///   Trap { kind: ShiftOutOfRange }
    ///
    /// shift_bb
    ///   IntShl / IntShr { dest, lhs, rhs }
    ///   [cursor stays here]
    /// ```
    fn lower_shift(
        &mut self,
        op: BinaryOp,
        dest: Place,
        lhs: Place,
        rhs: Place,
        ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        let Some(signed) = integer_signedness(ty) else {
            self.locals.pop();
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("binary operator `{op}` on non-integer type"),
                    site,
                },
                note: "shift trap lowering requires an integer-typed operand".to_string(),
            });
            return None;
        };

        // `integer_bit_width` resolves every integer width, including the
        // platform-sized `Isize`/`Usize` via the target pointer width. A `None`
        // here means a type that `integer_signedness` classified as an integer
        // but `integer_bit_width` has no arm for — an upstream bug — so we fail
        // closed rather than emit a shift with no out-of-range guard.
        let Some(width) = integer_bit_width(ty, self.pointer_width) else {
            self.locals.pop();
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("binary operator `{op}` on integer type `{ty:?}`"),
                    site,
                },
                note: "shift-range trap requires a known bit-width; \
                       integer_signedness classified this type as an integer but \
                       integer_bit_width has no arm for it."
                    .to_string(),
            });
            return None;
        };

        // ── out-of-range check: (count as unsigned) >= width ────────
        let width_const = self.alloc_local(ty.clone());
        self.push_instr(Instr::ConstI64 {
            dest: width_const,
            value: width,
        });
        let oor_flag = self.alloc_local(ResolvedTy::Bool);
        self.push_instr(Instr::IntCmp {
            dest: oor_flag,
            pred: CmpPred::UnsignedGreaterEq,
            lhs: rhs, // shift count
            rhs: width_const,
        });
        let sor_trap_bb = self.alloc_block();
        let shift_bb = self.alloc_block();
        self.finish_current_block(Terminator::Branch {
            cond: oor_flag,
            then_target: sor_trap_bb,
            else_target: shift_bb,
        });

        self.start_block(sor_trap_bb);
        self.finish_current_block(Terminator::Trap {
            kind: TrapKind::ShiftOutOfRange,
        });

        self.start_block(shift_bb);

        // ── shift instruction on the safe path ──────────────────────
        match op {
            BinaryOp::Shl => self.push_instr(Instr::IntShl { dest, lhs, rhs }),
            BinaryOp::Shr => self.push_instr(Instr::IntShr {
                signed,
                dest,
                lhs,
                rhs,
            }),
            _ => unreachable!("lower_shift called only for Shl / Shr"),
        }
        Some(dest)
    }

    /// Lower an `If` expression into a real CFG with a `Branch`
    /// terminator on the entry block, separate `then` / `else` blocks
    /// each terminated by a `Goto join_bb`, and a join block that
    /// receives the result value.
    ///
    /// The expression's value Place is a result-local *alloca'd before
    /// the branch* — when each arm finishes lowering its tail
    /// expression, the arm emits an `Instr::Move { dest: result_local,
    /// src: arm_value }` before the `Goto`. The join block then loads
    /// the value through the result local. This matches the existing
    /// alloca-per-local pattern (`alloc_local`) and the codegen's
    /// `place_pointer` lookup (each Place is a stack slot); LLVM's
    /// mem2reg pass promotes the alloca to SSA at the LLVM layer if
    /// the optimiser sees fit. Phi at MIR is a v0.6 refactor
    /// (`R-CFG-V06-phi`).
    ///
    /// `else_expr: None` reaches here when the HIR types the If as
    /// `ResolvedTy::Unit` (no else block). The else arm is still
    /// emitted as a block that just `Goto join` — no Move, no value
    /// written to `result_place`. Downstream code that loads from
    /// `result_place` on the else path observes whatever the alloca
    /// was initialised with (LLVM `undef` for an i8 unit-stand-in,
    /// inconsequential because Unit's value is by definition never
    /// observed). No special fail-closed needed.
    fn lower_if(
        &mut self,
        condition: &HirExpr,
        then_expr: &HirExpr,
        else_expr: Option<&HirExpr>,
        result_ty: &ResolvedTy,
    ) -> Option<Place> {
        // Result local first, so it dominates every branch arm's Move.
        // Allocated even for Unit Ifs to keep a single Place-shape
        // contract on the value-bearing return; codegen never loads a
        // Unit result so the placeholder's initial value is unused.
        let result_place = self.alloc_local(result_ty.clone());

        // Lower the condition in the entry (current) block. Receive a
        // Place holding the truth value; codegen's `Terminator::Branch`
        // emitter loads it and compares non-zero.
        // Condition lowering failed (NotYetImplemented or similar) —
        // propagate by returning None via `?`. The diagnostic already
        // lives on `self.diagnostics`, so the CLI rejects the program;
        // the half-built If does not need to seal the current block.
        // Leaving the result_local dangling is benign — no Branch/Goto
        // refers to it.
        let cond_place = self.lower_value(condition)?;

        // Allocate the three CFG blocks: then arm, else arm, join.
        let then_bb = self.alloc_block();
        let else_bb = self.alloc_block();
        let join_bb = self.alloc_block();

        // Seal the entry block with a Branch on the cond Place.
        self.finish_current_block(Terminator::Branch {
            cond: cond_place,
            then_target: then_bb,
            else_target: else_bb,
        });

        // Track whether either arm falls through to the join with a value.
        // When BOTH arms diverge (each `return`s/`panic`s, possibly through a
        // further nested CFG expression) the join has no live predecessor and
        // `result_place` is never written; the cursor must stay unreachable so
        // a tail `if` does not feed the dead `Unit` i8 stand-in into a
        // non-scalar return slot (the #1907 `Move type mismatch` abort). The
        // reachability flag — not the value `Option` — is the load-bearing
        // signal: a reachable Unit arm (`if c { return } else {}`) yields
        // `None` but leaves the cursor reachable, while a divergent-through-if
        // arm also yields `None` yet leaves the cursor unreachable.
        let mut join_reachable = false;

        // Then arm.
        self.start_block(then_bb);
        let then_value = self.lower_value(then_expr);
        if let Some(src) = then_value {
            self.push_instr(Instr::Move {
                dest: result_place,
                src,
            });
        }
        if !self.cursor_unreachable {
            join_reachable = true;
        }
        self.finish_current_block(Terminator::Goto { target: join_bb });

        // Else arm. `else_expr: None` (the HIR-types-as-Unit case)
        // emits a Goto-only block — no Move, no value contributed. That
        // Goto-only block always falls through, so the join stays reachable
        // for a one-armed `if c { return }`.
        self.start_block(else_bb);
        if let Some(else_expr) = else_expr {
            let else_value = self.lower_value(else_expr);
            if let Some(src) = else_value {
                self.push_instr(Instr::Move {
                    dest: result_place,
                    src,
                });
            }
        }
        if !self.cursor_unreachable {
            join_reachable = true;
        }
        self.finish_current_block(Terminator::Goto { target: join_bb });

        // Join. Subsequent lowering continues in this block; the If
        // expression's value Place is the result_local (loads happen
        // through the same Place that both arms wrote into). `start_block`
        // resets `cursor_unreachable`, so re-flag the dead join AFTER opening
        // it when both arms diverged.
        self.start_block(join_bb);
        if !join_reachable {
            self.cursor_unreachable = true;
        }
        Some(result_place)
    }

    /// Lower `xs[i]` (`HirExprKind::Index`) for a `Vec<T>` container.
    ///
    /// CFG shape (C-2 OOB trap pattern, mirrors B-2/B-5 bounds-check
    /// discipline):
    ///
    /// ```text
    /// entry_bb (current):
    ///   CallRuntimeAbi { symbol: "hew_vec_len", args: [vec_place], dest: len_place }
    ///   IntCmp { pred: UnsignedGreaterEq, dest: oob_flag,
    ///            lhs: index_place, rhs: len_place }
    ///   Branch { cond: oob_flag, then: trap_bb, else: cont_bb }
    ///
    /// trap_bb:
    ///   Trap { kind: IndexOutOfBounds }
    ///
    /// cont_bb:
    ///   CallRuntimeAbi { symbol: "hew_vec_get_T",
    ///                    args: [vec_place, index_place], dest: result_place }
    ///   -- owned elements instead use Terminator::Call("hew_vec_get_clone")
    ///      so the bare `T` result owns an independent clone
    /// ```
    ///
    /// The `UnsignedGreaterEq` predicate catches both negative indices
    /// (which wrap to values > `i64::MAX` when reinterpreted as unsigned)
    /// and indices ≥ `len` in a single compare — the same technique used
    /// by B-5's shift-range check. LESSONS: `boundary-fail-closed` (P0) —
    /// the trap is always emitted; the compiler never relies on the runtime's
    /// own bounds check.
    ///
    /// Element-type dispatch (`hew_vec_get_T`):
    /// - `bool` → `hew_vec_get_bool`
    /// - `char`/`i32` → `hew_vec_get_i32`
    /// - `i64` → `hew_vec_get_i64`
    /// - `f64` → `hew_vec_get_f64`
    /// - `String` → `hew_vec_get_str` (retained/header-aware owner;
    ///   callers that bind it must balance with `hew_string_drop`)
    /// - `BitCopy` `Named` value records and `Tuple` → `hew_vec_get_layout`
    ///   (layout-descriptor path; codegen loads the element via the dest-place
    ///   type so the full record stride is honoured)
    /// - owned record/enum/tuple value elements → `hew_vec_get_clone` into a bare `T`
    /// - nested collection handles → `hew_vec_get_owned` borrow
    /// - ptr-shaped (`Duplex`, `LambdaActorHandle`, non-`BitCopy` Named heap
    ///   types) → `hew_vec_get_ptr`
    ///
    /// Unsupported element types emit `MirDiagnostic::NotYetImplemented`
    /// and return `None` (tracked gap, not silent shim).
    #[expect(
        clippy::too_many_lines,
        reason = "explicit CFG construction plus element ABI dispatch must stay adjacent"
    )]
    fn lower_vec_index(
        &mut self,
        container: &HirExpr,
        index: &HirExpr,
        elem_ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        // Lower the container and index sub-expressions.
        let vec_place = self.lower_value(container)?;
        let raw_index_place = self.lower_value(index)?;

        // Implicit index-site widening: if the checker accepted a signed integer
        // narrower than i64 (i8/i16/i32) as the index, sign-extend it to i64 here
        // so the bounds-check IntCmp and the hew_vec_get_T call both receive
        // matching i64 operands.  This is operand widening at the use site, not
        // result-type widening (LESSONS `widen-operands-not-result-when-tightening-int-coercion`).
        let index_place = if let Place::Local(raw_id) = raw_index_place {
            let raw_ty = self.locals[raw_id as usize].clone();
            match raw_ty {
                ResolvedTy::I8 | ResolvedTy::I16 | ResolvedTy::I32 => {
                    let wide_place = self.alloc_local(ResolvedTy::I64);
                    self.push_instr(Instr::NumericCast {
                        dest: wide_place,
                        src: raw_index_place,
                        from_ty: raw_ty,
                        to_ty: ResolvedTy::I64,
                    });
                    wide_place
                }
                _ => raw_index_place,
            }
        } else {
            raw_index_place
        };

        // Step 1: Call hew_vec_len(vec) -> i64 to get the length.
        let len_place = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new("hew_vec_len", vec![vec_place], Some(len_place))
                .expect("hew_vec_len is an allowlisted runtime symbol"),
        ));

        // Step 2: Bounds check via UnsignedGreaterEq. A signed i64 index
        // that is negative will wrap to a value > i64::MAX when treated
        // as unsigned, which is ≥ any valid len. This catches both negative
        // and out-of-bounds indices in one compare.
        let oob_flag = self.alloc_local(ResolvedTy::Bool);
        self.push_instr(Instr::IntCmp {
            dest: oob_flag,
            pred: CmpPred::UnsignedGreaterEq,
            lhs: index_place,
            rhs: len_place,
        });

        // Seal current block with Branch → trap or continue.
        let trap_bb = self.alloc_block();
        let cont_bb = self.alloc_block();
        self.finish_current_block(Terminator::Branch {
            cond: oob_flag,
            then_target: trap_bb,
            else_target: cont_bb,
        });

        // Trap block: hard-abort with IndexOutOfBounds.
        self.start_block(trap_bb);
        self.finish_current_block(Terminator::Trap {
            kind: TrapKind::IndexOutOfBounds,
        });

        // Continuation block: emit the actual element load.
        self.start_block(cont_bb);

        // Dispatch to the typed runtime getter based on element type.
        //
        // Named element types split into two paths:
        //   - BitCopy value records (e.g. `type Point { x: i64; y: i64 }`) and
        //     tuples: their elements are stored inline in the vec buffer at the
        //     full record stride.  `hew_vec_get_ptr` uses a hard-coded 8-byte
        //     (pointer) stride and returns garbage for any record wider than 8
        //     bytes.  These types MUST use `hew_vec_get_layout` so the runtime
        //     applies the correct per-element stride via the layout descriptor.
        //   - Heap-handle nominals (Resource / Linear): stored as pointer-sized
        //     opaque handles; `hew_vec_get_ptr` is correct for these.
        // W5.016: an owned (non-Copy) record/enum/tuple VALUE element was
        // constructed through the owned descriptor. A scalar index result is an
        // independently-droppable value, so route it through the same fresh-owner
        // clone choke as `Vec::get`, with a bare `T` dest. Nested collection
        // HANDLES keep the established borrow contract: for-in and chained reads
        // rely on the outer Vec remaining the sole owner, and cloning each cursor
        // read would create an untracked temporary collection.
        let owned_elem = self.is_owned_vec_element(elem_ty);
        let clone_owned_value =
            owned_elem && !ty_is_vec(elem_ty) && !ty_is_local_collection_handle(elem_ty);
        let get_symbol = match elem_ty {
            ResolvedTy::Bool => "hew_vec_get_bool",
            ResolvedTy::I8 => "hew_vec_get_i8",
            ResolvedTy::U8 => "hew_vec_get_u8",
            ResolvedTy::I16 => "hew_vec_get_i16",
            ResolvedTy::U16 => "hew_vec_get_u16",
            ResolvedTy::Char | ResolvedTy::I32 | ResolvedTy::U32 => "hew_vec_get_i32",
            // `duration` is a signed 8-byte newtype — same i64-class getter as
            // i64 (`instant` reaches here already canonicalised to I64).
            ResolvedTy::I64
            | ResolvedTy::U64
            | ResolvedTy::Isize
            | ResolvedTy::Usize
            | ResolvedTy::Duration => "hew_vec_get_i64",
            ResolvedTy::F32 => "hew_vec_get_f32",
            ResolvedTy::F64 => "hew_vec_get_f64",
            ResolvedTy::String => "hew_vec_get_str",
            _ if clone_owned_value => "hew_vec_get_clone",
            _ if owned_elem => "hew_vec_get_owned",
            // BitCopy Named value records: use layout-descriptor getter so the
            // runtime applies the correct element stride.
            ResolvedTy::Named { .. }
                if ValueClass::of_ty(elem_ty, &self.type_classes) == ValueClass::BitCopy =>
            {
                "hew_vec_get_layout"
            }
            // Tuples are BitCopy aggregates stored inline; same layout path.
            ResolvedTy::Tuple(_) => "hew_vec_get_layout",
            // DIRECT (non-indirect) enums are stored inline in the vec buffer
            // at the full tagged-union struct stride — same as BitCopy records.
            // They must use `hew_vec_get_layout` so the runtime applies the
            // correct per-element stride via the layout descriptor.
            //
            // INDIRECT enums are heap-allocated; each element slot holds an
            // 8-byte pointer (same as a Resource/Linear handle), so they
            // continue to use `hew_vec_get_ptr`.
            //
            // Without this branch, direct enums fell through to the
            // `hew_vec_get_ptr` catch-all below — which uses an 8-byte pointer
            // stride, mis-strides the buffer, and causes a runtime panic.
            ResolvedTy::Named { name, .. }
                if self.enum_layouts.iter().any(|el| {
                    (el.name == name.as_str() || el.name == short_name(name)) && !el.is_indirect
                }) =>
            {
                "hew_vec_get_layout"
            }
            // Pointer-shaped heap handles (Resource, Linear): Duplex,
            // LambdaActorHandle, indirect enums, and other non-BitCopy Named
            // types whose heap-backing is opaque to the element-load ABI.
            // `hew_vec_get_ptr` returns a *mut c_void which codegen casts to
            // the appropriate pointer.
            //
            // Closure-pair elements share the symbol: the slot holds a
            // heap-boxed copy of the 16-byte pair. `hew_vec_get_ptr` returns
            // the box address; codegen's CallRuntimeAbi marshalling sees the
            // pair-typed dest and copies the pair out of the box (a borrow —
            // the vec slot keeps ownership of the box and the env).
            ResolvedTy::Named { .. } | ResolvedTy::Function { .. } | ResolvedTy::Closure { .. } => {
                "hew_vec_get_ptr"
            }
            other => {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!("Vec<{other:?}> element type for xs[i]"),
                        site,
                    },
                    note: "hew_vec_get_T dispatch: element types supported by this \
                           slice are bool, char/i32/u32, i64/u64, f64, String \
                           (retained/header-aware owner), BitCopy Named value \
                           records and tuples (layout-descriptor path), and \
                           heap-handle Named types (pointer path). Other scalars \
                           map to i32/i64 in a future width-normalisation slice."
                        .to_string(),
                });
                return None;
            }
        };

        let result_place = self.alloc_local(elem_ty.clone());
        if clone_owned_value {
            let next = self.alloc_block();
            self.finish_current_block(Terminator::Call {
                callee: get_symbol.to_string(),
                builtin: None,
                args: vec![vec_place, index_place],
                dest: Some(result_place),
                next,
            });
            self.start_block(next);
        } else {
            self.push_instr(Instr::CallRuntimeAbi(
                crate::model::RuntimeCall::new(
                    get_symbol,
                    vec![vec_place, index_place],
                    Some(result_place),
                )
                .expect("hew_vec_get_T is an allowlisted runtime symbol"),
            ));
        }

        Some(result_place)
    }

    /// Lower `m[k]` over a `HashMap<K, V>` in READ position — the trapping
    /// `Index::at` accessor, the map twin of `lower_vec_index`.
    ///
    /// Emits a single `Terminator::Call` to the `hew_hashmap_get_clone_layout`
    /// choke with the BARE `V` dest (no `Option` round-trip). Codegen
    /// (`lower_hashmap_index_trap_call`) synthesises the runtime out-pointer
    /// from that dest, branches on the runtime's found-bit, and aborts with
    /// `IndexOutOfBounds` on a miss (the map analogue of `lower_vec_index`'s OOB
    /// trap). On a hit, the matched value is cloned into the dest through the
    /// value descriptor's semantic clone (`clone_layout_value_blob`), so the
    /// result is a FRESH, independently-droppable owner — never a borrow into
    /// the live table (GAP-2 drop-safety; `by-value-heap-params-are-borrows`
    /// P0). On a miss the dest is never written and codegen traps before the
    /// `next` block, so the dest's scope-exit drop (scheduled on the through
    /// path) never fires on the miss path.
    ///
    /// `m.get(k) -> Option<V>` is the non-aborting sibling; it routes through
    /// the `ResolvedImplCall` get path to the same runtime choke.
    fn lower_hashmap_index_trap(
        &mut self,
        container: &HirExpr,
        index: &HirExpr,
        elem_ty: &ResolvedTy,
        _site: hew_hir::SiteId,
    ) -> Option<Place> {
        let map_place = self.lower_value(container)?;
        let key_place = self.lower_value(index)?;
        let result_place = self.alloc_local(elem_ty.clone());
        let next = self.alloc_block();
        // The callee is the fresh-owner clone choke shared with `m.get` (codegen
        // declares the `(ptr, ptr, ptr) -> i1` runtime). It is not in the typed
        // `RuntimeCallFamily` catalog, so it carries `builtin: None` like
        // `hew_vec_get_clone`; codegen dispatches on the symbol string and owns
        // the trap-on-miss CFG.
        self.finish_current_block(Terminator::Call {
            callee: "hew_hashmap_get_clone_layout".to_string(),
            builtin: None,
            args: vec![map_place, key_place],
            dest: Some(result_place),
            next,
        });
        self.start_block(next);
        Some(result_place)
    }

    /// Lower `xs[a..b]` / `xs[a..=b]` / `xs[..b]` / `xs[a..]` / `xs[..]`
    /// (`HirExprKind::Slice`) for a `Vec<T>` container (C-3).
    ///
    /// CFG shape (extends C-2's OOB pattern with a two-stage bounds check
    /// and an optional integer-overflow trap for the inclusive form):
    ///
    /// ```text
    /// entry_bb (current):
    ///   [start open?]     start_place := ConstI64(0)
    ///   [end open?]       end_place := CallRuntimeAbi("hew_vec_len", [vec])
    ///   [inclusive?]      one := ConstI64(1)
    ///                     end_place := IntArithChecked(Add, signed, end_place, one)
    ///                       → on overflow → trap_overflow_bb { TrapKind::IntegerOverflow }
    ///                       → on success → cont1_bb (subsequent emission)
    ///   IntCmp { pred: SignedGreater, dest: bad1, lhs: start, rhs: end }
    ///   Branch { cond: bad1, then: trap_oob_bb, else: cont2_bb }
    ///
    /// cont2_bb:
    ///   [end_place already holds end; reuse]
    ///   len := CallRuntimeAbi("hew_vec_len", [vec])    -- second probe so
    ///                                                    inclusive +1 is not
    ///                                                    compared to the
    ///                                                    pre-Add len
    ///   IntCmp { pred: SignedGreater, dest: bad2, lhs: end_place, rhs: len }
    ///   Branch { cond: bad2, then: trap_oob_bb, else: cont3_bb }
    ///
    /// trap_oob_bb:
    ///   Trap { kind: IndexOutOfBounds }
    ///
    /// cont3_bb:
    ///   CallRuntimeAbi { hew_vec_slice_range_T, args: [vec, start, end],
    ///                    dest: result }
    /// ```
    ///
    /// `SignedGreater` is the right predicate for `start > end` and
    /// `end > len` because both endpoints are checker-validated i64. The
    /// inclusive overflow guard runs BEFORE the bounds check so an
    /// `i64::MAX..=i64::MAX` form traps as `IntegerOverflow` (not
    /// `IndexOutOfBounds`), matching B-2's discipline that each trap
    /// reports its true cause.
    ///
    /// Element-type dispatch (`hew_vec_slice_range_T`) covers scalar
    /// bitcopy elements through the bytesize-generic path, `string` through
    /// retain-on-slice, pointer-shaped named heap handles, and
    /// descriptor-backed layout/owned records. For Vec<String> the runtime
    /// retains each element into the fresh header-aware vec and sets
    /// `elem_kind == String` so the existing free-on-drop path releases them.
    #[expect(
        clippy::too_many_lines,
        reason = "explicit CFG construction: each block + bounds-check branch is its own \
                  step; splitting would obscure the trap-graph shape"
    )]
    fn lower_vec_slice(
        &mut self,
        container: &HirExpr,
        start: Option<&HirExpr>,
        end: Option<&HirExpr>,
        inclusive: bool,
        result_ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        // Resolve element type from the result Vec<T> for runtime dispatch.
        let result_ty = self.subst_ty(result_ty);
        let elem_ty = match &result_ty {
            ResolvedTy::Named { args, .. }
                if result_ty.is_builtin(BuiltinType::Vec) && !args.is_empty() =>
            {
                args[0].clone()
            }
            other => {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!(
                            "Vec range-slice result type must be Vec<T>; got {other:?}"
                        ),
                        site,
                    },
                    note: "C-3 range-slice expects the checker to record `Vec<T>` as the \
                           expression type; receiving anything else indicates a checker/HIR \
                           boundary violation upstream"
                        .to_string(),
                });
                return None;
            }
        };

        let slice_symbol = match &elem_ty {
            ResolvedTy::Bool
            | ResolvedTy::Char
            | ResolvedTy::I8
            | ResolvedTy::U8
            | ResolvedTy::I16
            | ResolvedTy::U16
            | ResolvedTy::I32
            | ResolvedTy::U32
            | ResolvedTy::I64
            | ResolvedTy::U64
            | ResolvedTy::Isize
            | ResolvedTy::Usize
            // `duration` is an 8-byte bitcopy scalar (same byte-sized slice path
            // as i64); `instant` reaches here canonicalised to I64.
            | ResolvedTy::Duration
            | ResolvedTy::F32
            | ResolvedTy::F64 => "hew_vec_slice_range_bytesize",
            ResolvedTy::String => "hew_vec_slice_range_str",
            _ if self.is_owned_vec_element(&elem_ty) => "hew_vec_slice_range_owned",
            _ if self.vec_element_uses_layout_descriptor(&elem_ty) => "hew_vec_slice_range_layout",
            ResolvedTy::Named { .. } => "hew_vec_slice_range_ptr",
            other => {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!("Vec<{other:?}> element type for xs[a..b]"),
                        site,
                    },
                    note: "hew_vec_slice_range_T dispatch: element types supported are \
                           scalar bitcopy elements, string, pointer-shaped named heap \
                           handles, and descriptor-backed layout/owned records."
                        .to_string(),
                });
                return None;
            }
        };

        let vec_place = self.lower_value(container)?;

        // Resolve start. Open `start` materialises as ConstI64(0).
        let start_place = if let Some(s) = start {
            self.lower_value(s)?
        } else {
            let p = self.alloc_local(ResolvedTy::I64);
            self.instructions
                .push(Instr::ConstI64 { dest: p, value: 0 });
            p
        };

        // Resolve end. Open `end` materialises as `hew_vec_len(vec)`.
        // For inclusive `a..=b`, lower `b` first then add 1 with overflow trap.
        let end_place = if let Some(e) = end {
            let base = self.lower_value(e)?;
            if inclusive {
                // b + 1 via IntArithChecked(Add, Signed). The endpoint is i64
                // per the checker; overflow on i64::MAX traps as
                // TrapKind::IntegerOverflow.
                let one_place = self.alloc_local(ResolvedTy::I64);
                self.push_instr(Instr::ConstI64 {
                    dest: one_place,
                    value: 1,
                });
                let bumped = self.alloc_local(ResolvedTy::I64);
                let overflow_flag = self.alloc_local(ResolvedTy::Bool);
                self.push_instr(Instr::IntArithChecked {
                    op: IntArithOp::Add,
                    signed: IntSignedness::Signed,
                    dest: bumped,
                    lhs: base,
                    rhs: one_place,
                    overflow_flag,
                });
                let overflow_trap_bb = self.alloc_block();
                let after_inc_bb = self.alloc_block();
                self.finish_current_block(Terminator::Branch {
                    cond: overflow_flag,
                    then_target: overflow_trap_bb,
                    else_target: after_inc_bb,
                });
                self.start_block(overflow_trap_bb);
                self.finish_current_block(Terminator::Trap {
                    kind: TrapKind::IntegerOverflow,
                });
                self.start_block(after_inc_bb);
                bumped
            } else {
                base
            }
        } else {
            // Open end: probe length via hew_vec_len.
            let p = self.alloc_local(ResolvedTy::I64);
            self.push_instr(Instr::CallRuntimeAbi(
                crate::model::RuntimeCall::new("hew_vec_len", vec![vec_place], Some(p))
                    .expect("hew_vec_len is an allowlisted runtime symbol"),
            ));
            p
        };

        // Bounds check 1: start <= end. Implemented as `start > end` ?
        // → trap_oob.
        let oob_trap_bb = self.alloc_block();
        let after_check1_bb = self.alloc_block();
        let bad1 = self.alloc_local(ResolvedTy::Bool);
        self.push_instr(Instr::IntCmp {
            dest: bad1,
            pred: CmpPred::SignedGreater,
            lhs: start_place,
            rhs: end_place,
        });
        self.finish_current_block(Terminator::Branch {
            cond: bad1,
            then_target: oob_trap_bb,
            else_target: after_check1_bb,
        });

        // Bounds check 2 (in the success-of-check-1 block): end <= len.
        // Re-probe len here so the comparison uses the post-inclusive-bump
        // end against the current container length.
        self.start_block(after_check1_bb);
        let len_place = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new("hew_vec_len", vec![vec_place], Some(len_place))
                .expect("hew_vec_len is an allowlisted runtime symbol"),
        ));
        let bad2 = self.alloc_local(ResolvedTy::Bool);
        self.push_instr(Instr::IntCmp {
            dest: bad2,
            pred: CmpPred::SignedGreater,
            lhs: end_place,
            rhs: len_place,
        });
        let after_check2_bb = self.alloc_block();
        self.finish_current_block(Terminator::Branch {
            cond: bad2,
            then_target: oob_trap_bb,
            else_target: after_check2_bb,
        });

        // Single shared OOB trap block for both bounds-check branches.
        self.start_block(oob_trap_bb);
        self.finish_current_block(Terminator::Trap {
            kind: TrapKind::IndexOutOfBounds,
        });

        // Success path: emit the runtime slice call. The result is a fresh
        // `*mut HewVec<T>` handle (ptr-shaped local typed as Vec<T>).
        self.start_block(after_check2_bb);
        let result_place = self.alloc_local(result_ty.clone());
        self.push_instr(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new(
                slice_symbol,
                vec![vec_place, start_place, end_place],
                Some(result_place),
            )
            .expect("hew_vec_slice_range_T is an allowlisted runtime symbol"),
        ));

        Some(result_place)
    }

    // -------------------------------------------------------------------
    // W3 collections-sugar S2 — string / bytes index + slice lowering
    // -------------------------------------------------------------------
    //
    // Unlike the Vec arms (which emit explicit MIR-side bounds checks
    // because the typed-getter runtime entries assume in-range inputs),
    // the new string/bytes intrinsics are fail-closed at the runtime
    // boundary: each `hew_{string,bytes}_{index,slice*}` entry validates
    // its own arguments and `libc::abort()`s on OOB / invalid bounds.
    // This is the boundary-fail-closed pattern from LESSONS row P0:49 —
    // moving the trap into the runtime keeps the compiler-emitted CFG
    // small (no synthesized trap_bb / cont_bb pair per index site) and
    // there is no way the trap can be skipped by a producer arm that
    // forgets to emit it. The drift-test for the runtime tests these
    // abort paths directly.
    //
    // Endpoint types (always i64 per the checker arms):
    //   - hew_string_index(s, i: i64) -> i32 (char)
    //   - hew_string_slice_codepoints(s, start: i64, end: i64) -> string
    //   - hew_bytes_index(ptr, offset, len, i: i64) -> u8
    //   - hew_bytes_slice(ptr, offset, len, start: i64, end: i64) -> bytes
    //
    // Inclusive ranges (`a..=b`) are lowered to half-open `a..(b+1)`
    // with an explicit i64 overflow trap, mirroring the Vec arm. Open
    // endpoints materialise as 0 (start) or `hew_string_char_count` /
    // `hew_bytes_len` (end).

    fn lower_string_index(
        &mut self,
        container: &HirExpr,
        index: &HirExpr,
        elem_ty: &ResolvedTy,
        _site: hew_hir::SiteId,
    ) -> Option<Place> {
        debug_assert!(matches!(elem_ty, ResolvedTy::Char));
        let s_place = self.lower_value(container)?;
        let i_place = self.lower_value(index)?;
        let result_place = self.alloc_local(elem_ty.clone());
        self.push_runtime_call(
            "hew_string_index",
            vec![s_place, i_place],
            Some(result_place),
        );
        Some(result_place)
    }

    /// Lower `s[a..b]` / inclusive / open-end forms for `string`.
    ///
    /// Open start materialises as ConstI64(0). Open end materialises
    /// as `hew_string_char_count(s)` (cast i32 -> i64).
    /// Inclusive `a..=b` materialises as `b + 1` with an i64 overflow
    /// trap before the runtime call.
    ///
    /// The runtime intrinsic owns the OOB / inverted-bounds trap.
    fn lower_string_slice(
        &mut self,
        container: &HirExpr,
        start: Option<&HirExpr>,
        end: Option<&HirExpr>,
        inclusive: bool,
        _site: hew_hir::SiteId,
    ) -> Option<Place> {
        let s_place = self.lower_value(container)?;

        let start_place = if let Some(s) = start {
            self.lower_value(s)?
        } else {
            let p = self.alloc_local(ResolvedTy::I64);
            self.instructions
                .push(Instr::ConstI64 { dest: p, value: 0 });
            p
        };

        let end_place = if let Some(e) = end {
            let base = self.lower_value(e)?;
            if inclusive {
                self.bump_inclusive_endpoint(base)
            } else {
                base
            }
        } else {
            let count_i32 = self.alloc_local(ResolvedTy::I32);
            self.push_runtime_call("hew_string_char_count", vec![s_place], Some(count_i32));
            let count_i64 = self.alloc_local(ResolvedTy::I64);
            self.push_instr(Instr::NumericCast {
                dest: count_i64,
                src: count_i32,
                from_ty: ResolvedTy::I32,
                to_ty: ResolvedTy::I64,
            });
            count_i64
        };

        let result_place = self.alloc_local(ResolvedTy::String);
        self.push_runtime_call(
            "hew_string_slice_codepoints",
            vec![s_place, start_place, end_place],
            Some(result_place),
        );
        Some(result_place)
    }

    fn lower_bytes_index(
        &mut self,
        container: &HirExpr,
        index: &HirExpr,
        elem_ty: &ResolvedTy,
        _site: hew_hir::SiteId,
    ) -> Option<Place> {
        debug_assert!(matches!(elem_ty, ResolvedTy::U8));
        let bytes_place = self.lower_value(container)?;
        let i_place = self.lower_value(index)?;
        let result_place = self.alloc_local(elem_ty.clone());
        // Bytes values are codegen-represented as a 3-field triple
        // {ptr, offset, len}; codegen unpacks `bytes_place` into the
        // three runtime-ABI arguments. The MIR-level RuntimeCall lists
        // a single Place for the bytes receiver — codegen knows how
        // to expand it for the bytes-typed slot. The runtime asserts
        // bounds and aborts on OOB.
        self.push_runtime_call(
            "hew_bytes_index",
            vec![bytes_place, i_place],
            Some(result_place),
        );
        Some(result_place)
    }

    fn lower_bytes_slice(
        &mut self,
        container: &HirExpr,
        start: Option<&HirExpr>,
        end: Option<&HirExpr>,
        inclusive: bool,
        _site: hew_hir::SiteId,
    ) -> Option<Place> {
        let bytes_place = self.lower_value(container)?;

        let start_place = if let Some(s) = start {
            self.lower_value(s)?
        } else {
            let p = self.alloc_local(ResolvedTy::I64);
            self.instructions
                .push(Instr::ConstI64 { dest: p, value: 0 });
            p
        };

        let end_place = if let Some(e) = end {
            let base = self.lower_value(e)?;
            if inclusive {
                self.bump_inclusive_endpoint(base)
            } else {
                base
            }
        } else {
            let len_place = self.alloc_local(ResolvedTy::I64);
            self.push_runtime_call("hew_bytes_len", vec![bytes_place], Some(len_place));
            len_place
        };

        let result_place = self.alloc_local(ResolvedTy::Bytes);
        self.push_runtime_call(
            "hew_bytes_slice",
            vec![bytes_place, start_place, end_place],
            Some(result_place),
        );
        Some(result_place)
    }

    /// Bump a half-open endpoint to its inclusive equivalent: `b + 1`
    /// with an i64 overflow trap. Shared by string and bytes inclusive
    /// range lowering; mirrors the same pattern used by the Vec arm.
    fn bump_inclusive_endpoint(&mut self, base: Place) -> Place {
        let one_place = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::ConstI64 {
            dest: one_place,
            value: 1,
        });
        let bumped = self.alloc_local(ResolvedTy::I64);
        let overflow_flag = self.alloc_local(ResolvedTy::Bool);
        self.push_instr(Instr::IntArithChecked {
            op: IntArithOp::Add,
            signed: IntSignedness::Signed,
            dest: bumped,
            lhs: base,
            rhs: one_place,
            overflow_flag,
        });
        let overflow_trap_bb = self.alloc_block();
        let after_inc_bb = self.alloc_block();
        self.finish_current_block(Terminator::Branch {
            cond: overflow_flag,
            then_target: overflow_trap_bb,
            else_target: after_inc_bb,
        });
        self.start_block(overflow_trap_bb);
        self.finish_current_block(Terminator::Trap {
            kind: TrapKind::IntegerOverflow,
        });
        self.start_block(after_inc_bb);
        bumped
    }

    /// Emit `Terminator::Call` for a static call to a user-defined function
    /// in the same module. Arguments are lowered left-to-right; if any
    /// argument fails to produce a Place (an unsupported construct in its
    /// own right), the whole call fails closed and returns `None` —
    /// diagnostics from the argument lowering already capture the root cause.
    fn resolve_static_trait_method_callee(
        &mut self,
        receiver_type_param: &str,
        declaring_trait: &str,
        method_name: &str,
        site: SiteId,
    ) -> Option<String> {
        let Some(concrete_ty) = self.subst.get(receiver_type_param).cloned() else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::UnresolvedStaticDispatchSubstitution {
                    receiver_type_param: receiver_type_param.to_string(),
                    declaring_trait: declaring_trait.to_string(),
                    method_name: method_name.to_string(),
                    site,
                },
                note: format!(
                    "static trait dispatch `{declaring_trait}::{method_name}` reached \
                     MIR in a concrete function body without a substitution for \
                     receiver type parameter `{receiver_type_param}`; this indicates \
                     a missing monomorphization binding (the generic origin should \
                     not be emitted)"
                ),
            });
            return None;
        };
        let Some((self_type_name, type_args)) =
            hew_hir::dispatch::receiver_self_type_for_impl_lookup(&concrete_ty)
        else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!(
                        "static trait dispatch on receiver shape `{concrete_ty:?}` \
                         for `{declaring_trait}::{method_name}`"
                    ),
                    site,
                },
                note: "receiver type has no canonical impl-self name; \
                       static dispatch supports nominal and primitive receivers only"
                    .to_string(),
            });
            return None;
        };
        // Pass `type_args` so concrete-specialised impls resolve correctly (#2270).
        let Some(entry) = hew_hir::dispatch::lookup_trait_impl_entry(
            &self.trait_impl_index,
            declaring_trait,
            &self_type_name,
            method_name,
            &type_args,
        )
        .cloned() else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::StaticDispatchImplNotFound {
                    declaring_trait: declaring_trait.to_string(),
                    self_type_name: self_type_name.clone(),
                    method_name: method_name.to_string(),
                    site,
                },
                note: format!(
                    "no impl of trait `{declaring_trait}` for `{self_type_name}` \
                     registered in the static-dispatch index; the checker should \
                     have rejected this call"
                ),
            });
            return None;
        };
        if entry.impl_type_params.is_empty() {
            if !self.module_fn_names.contains(&entry.method_symbol) {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::StaticDispatchImplNotFound {
                        declaring_trait: declaring_trait.to_string(),
                        self_type_name: self_type_name.clone(),
                        method_name: method_name.to_string(),
                        site,
                    },
                    note: format!(
                        "impl method `{}` is registered in the static-dispatch \
                         index but not in module_fn_names",
                        entry.method_symbol
                    ),
                });
                return None;
            }
            return Some(entry.method_symbol);
        }
        let mangled = hew_hir::monomorph::mangle(&entry.method_symbol, &type_args);
        if !self.module_fn_names.contains(&mangled) {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::StaticDispatchMonomorphisationMissing {
                    method_symbol: entry.method_symbol.clone(),
                    mangled: mangled.clone(),
                    site,
                },
                note: format!(
                    "static dispatch resolved to generic impl method `{}` \
                     but no monomorphisation `{}` was registered by HIR's \
                     closure_under_substitution",
                    entry.method_symbol, mangled
                ),
            });
            return None;
        }
        Some(mangled)
    }

    fn resolve_var_self_direct_callee(
        &mut self,
        callee: &str,
        site: SiteId,
        receiver_ty: &ResolvedTy,
    ) -> Option<String> {
        if let Some(type_args) = self.call_site_type_args.get(&site).cloned() {
            let substituted: Vec<ResolvedTy> = type_args.iter().map(|t| self.subst_ty(t)).collect();
            let mangled = hew_hir::monomorph::mangle(callee, &substituted);
            if self.module_fn_names.contains(&mangled) {
                return Some(mangled);
            }
        }
        let substituted_receiver = self.subst_ty(receiver_ty);
        if let ResolvedTy::Named { args, .. } = &substituted_receiver {
            if !args.is_empty() {
                let mangled = hew_hir::monomorph::mangle(callee, args);
                if self.module_fn_names.contains(&mangled) {
                    return Some(mangled);
                }
            }
        }
        if self.module_fn_names.contains(callee) {
            return Some(callee.to_string());
        }
        self.diagnostics.push(MirDiagnostic {
            kind: MirDiagnosticKind::NotYetImplemented {
                construct: format!("var-self method callee `{callee}`"),
                site,
            },
            note: "var-self write-back dispatch resolved to a method symbol that has \
                   no MIR body in module_fn_names; HIR should have registered the \
                   impl method or its monomorphisation"
                .to_string(),
        });
        None
    }

    fn restore_var_self_receiver_binding(
        &mut self,
        binding_id: BindingId,
        name: &str,
        ty: &ResolvedTy,
        site: SiteId,
    ) {
        self.statements.push(MirStatement::Bind {
            binding: binding_id,
            name: name.to_string(),
            site,
            ty: ty.clone(),
        });
        self.record_binding_scope(binding_id);
        if self.binding_seeds_drop_elaboration(ty)
            && !self.owned_locals.iter().any(|entry| {
                entry.binding == binding_id && entry.disposition == Disposition::ScopeExit
            })
        {
            self.register_owned_local(binding_id, name.to_string(), ty.clone());
        }
    }

    fn lower_var_self_method_call(
        &mut self,
        site: SiteId,
        receiver: &HirExpr,
        target: &HirVarSelfMethodTarget,
        args: &[HirExpr],
        ret_ty: &ResolvedTy,
        receiver_ty: &ResolvedTy,
    ) -> Option<Place> {
        let HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(binding_id),
            name: receiver_name,
        } = &receiver.kind
        else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::UnsupportedNode {
                    reason: format!(
                        "var-self method call has non-binding receiver {:?}; checker should have rejected this",
                        receiver.kind
                    ),
                },
                note: "var-self receivers must be mutable local bindings so the \
                       dual-return Self value can be written back in place"
                    .to_string(),
            });
            return None;
        };
        let Some(receiver_slot) = self.binding_locals.get(binding_id).copied() else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::UnresolvedPlace {
                    binding: *binding_id,
                    name: receiver_name.clone(),
                    site: receiver.site,
                },
                note: "var-self receiver binding has no MIR place".to_string(),
            });
            return None;
        };
        if !matches!(receiver_slot, Place::Local(_)) {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::UnsupportedNode {
                    reason: format!(
                        "var-self receiver `{receiver_name}` maps to non-local place {receiver_slot:?}"
                    ),
                },
                note: "var-self write-back currently supports ordinary local bindings only"
                    .to_string(),
            });
            return None;
        }
        let callee_symbol = match target {
            HirVarSelfMethodTarget::Direct { callee } => {
                self.resolve_var_self_direct_callee(callee, site, receiver_ty)?
            }
            HirVarSelfMethodTarget::StaticTrait {
                receiver_type_param,
                declaring_trait,
                method_name,
                ..
            } => self.resolve_static_trait_method_callee(
                receiver_type_param,
                declaring_trait,
                method_name,
                site,
            )?,
        };
        let self_arg = self.lower_value(receiver)?;
        let mut arg_places = Vec::with_capacity(args.len() + 1);
        arg_places.push(self_arg);
        for arg in args {
            arg_places.push(self.lower_value(arg)?);
        }
        let resolved_ret_ty = self.subst_ty(ret_ty);
        let resolved_receiver_ty = self.subst_ty(receiver_ty);
        let tuple_ty =
            ResolvedTy::Tuple(vec![resolved_ret_ty.clone(), resolved_receiver_ty.clone()]);
        let tuple_place = self.alloc_local(tuple_ty);
        let next = self.alloc_block();
        self.finish_current_block(Terminator::Call {
            callee: callee_symbol,
            builtin: None,
            args: arg_places,
            dest: Some(tuple_place),
            next,
        });
        self.start_block(next);

        let result_place = self.alloc_local(resolved_ret_ty);
        self.push_instr(Instr::TupleFieldLoad {
            tuple: tuple_place,
            field_index: 0,
            dest: result_place,
        });
        self.push_instr(Instr::TupleFieldLoad {
            tuple: tuple_place,
            field_index: 1,
            dest: receiver_slot,
        });
        self.restore_var_self_receiver_binding(
            *binding_id,
            receiver_name,
            &resolved_receiver_ty,
            site,
        );
        Some(result_place)
    }

    ///
    /// The `dest` Place is allocated here and written by the emitted
    /// call terminator. For unit-returning functions (`ret_ty` is
    /// `ResolvedTy::Unit`) the dest is `None`; the terminator emits only
    /// the call and branch. For all other return types a fresh local is
    /// allocated and returned so the caller can bind it.
    pub(crate) fn lower_direct_call(
        &mut self,
        callee_symbol: &str,
        builtin: Option<hew_types::runtime_call::RuntimeCallFamily>,
        hir_args: &[hew_hir::HirExpr],
        ret_ty: &ResolvedTy,
        _site: hew_hir::SiteId,
    ) -> Option<Place> {
        // `Terminator::Call` invariant (model.rs): a carried family IS the
        // callee identity — the symbol string must be its catalog
        // presentation. Enforced in all build profiles; a violation here
        // means a HIR resolution stored the wrong family for the callee
        // name it minted (LESSONS `boundary-fail-closed`).
        assert!(
            builtin.is_none_or(|f| f.c_symbol() == callee_symbol),
            "lower_direct_call: builtin family {:?} does not match callee \
             `{callee_symbol}` (family c_symbol is `{}`)",
            builtin,
            builtin.map_or("", |f| f.c_symbol()),
        );
        // CAP-11 fail-closed gate: a call producing `Generator<..>` may
        // ultimately flat-copy a fn-valued argument into the generator env
        // (`Terminator::MakeGenerator`'s heap-copy), and the body side never
        // drops a fn-typed capture. Refuse a capturing closure or any fn value
        // whose env provenance is unproven (parameter/call result) at the
        // crossing. Named-fn references and capture-free closures stay
        // admitted: their env word is null by construction.
        if ty_is_generator_handle(ret_ty) {
            self.reject_unproven_generator_fn_args(hir_args);
        }
        // Lower each argument left-to-right.  If any fails to produce a
        // Place, fail the whole call — argument diagnostics already capture
        // the root cause.
        let mut arg_places = Vec::with_capacity(hir_args.len());
        for arg in hir_args {
            match self.lower_value(arg) {
                Some(p) => arg_places.push(p),
                None => return None,
            }
        }

        // Allocate a destination local for the return value, unless the
        // callee is declared Unit-returning or divergent. Never-returning
        // runtime shims such as exit()/panic() have no value to materialise.
        let dest = if matches!(ret_ty, ResolvedTy::Unit | ResolvedTy::Never) {
            None
        } else {
            Some(self.alloc_local(ret_ty.clone()))
        };

        // Suspendable-caller flip: in an execution-context caller, the four
        // builtin recv/send/sleep families SUSPEND on the coro substrate instead
        // of blocking the worker. Factored into one helper so the per-family
        // shapes sit together (the suspend-flip surface this change collapses onto
        // the SuspendKind side-table). `Break(dest)` when a flip fired.
        if let std::ops::ControlFlow::Break(dest) = self.try_lower_suspending_builtin_flip(
            callee_symbol,
            builtin,
            ret_ty,
            dest,
            &arg_places,
        ) {
            return dest;
        }

        let next = self.alloc_block();
        let proven_borrow_args: HashSet<usize> = hir_args
            .iter()
            .enumerate()
            .filter_map(|(index, arg)| {
                self.param_ownership
                    .proven_borrow_arg_sites
                    .contains(&arg.site)
                    .then_some(index)
            })
            .collect();
        // #2743 — mint a caller-side scope-exit drop for every fresh owned
        // composite/string argument TEMPORARY passed to a BORROWING parameter.
        // The temporary has no user `let`, so #2735's preserve-the-drop exemption
        // (`proven_borrow_whole_arg_locals`) has nothing to preserve and the fresh
        // value leaks. Binding the already-materialised arg local to a synthetic
        // owned local routes it through the SAME `owned_locals` machinery as
        // `let x = Row{..}; g(x)` — the per-type sole-owner prover then decides
        // admission, so an escaping value is still excluded (leak, never a
        // double-free), and registration alone never forces a drop.
        //
        // Exactly-once gate is per type, aligned with the prover's own
        // borrow-vs-consume exemption:
        //  - record / tuple / enum: BORROW iff the arg site is in
        //    `proven_borrow_args` (the same `proven_borrow_call_args` exemption
        //    the composite provers read). A CONSUMING composite callee's temp is
        //    NOT registered here (its arg is absent from `proven_borrow_args`); the
        //    callee owns and drops it (#2732 for enums) — mutually exclusive.
        //  - string: minted iff the callee is a USER free function (a string
        //    param is never recorded in `proven_borrow_arg_sites` — its borrow
        //    model is the separate refcount contract). The string sole-owner
        //    prover then gates the actual drop exactly as for the named
        //    `let s = a+b; h(s)` shape (borrow admits, consume/escape excludes).
        //    Runtime borrowing receivers (`(a+b).len()` = `hew_string_length`)
        //    are deliberately excluded: their nested temp already gets an
        //    exactly-once inline release from `apply_nested_fresh_string_temp_drops`.
        for (index, arg) in hir_args.iter().enumerate() {
            let Some(owned_ty) = self.caller_borrowed_temp_arg_owned_ty(arg) else {
                continue;
            };
            let callee_borrows = if matches!(owned_ty, ResolvedTy::String) {
                // A string temp earns a caller drop only when passed to a USER
                // free function (the #2735/#2743 seam), NOT to a runtime borrowing
                // receiver: `(a + b).len()` lowers `hew_string_length` through this
                // same `lower_direct_call` path (its stdlib shim is registered in
                // `module_fn_names`), and that nested temp already gets its
                // exactly-once INLINE release from
                // `apply_nested_fresh_string_temp_drops` — minting a synthetic
                // scope-exit owner over it merely relocates the drop and drifts the
                // nested-producer canary. A runtime string op carries a
                // `borrows_string_call_args` ownership contract; a user free fn does
                // not, so that contract is the exact discriminator. The string
                // sole-owner prover then gates the actual drop (borrow admits,
                // consume/escape excludes), as for the named `let s = a+b; h(s)`.
                !crate::runtime_symbols::callee_ownership_contract(callee_symbol)
                    .borrows_string_call_args()
                    && (self.module_fn_names.contains(callee_symbol)
                        || self.module_generic_fn_names.contains(callee_symbol))
            } else {
                proven_borrow_args.contains(&index)
            };
            if !callee_borrows {
                continue;
            }
            // A fresh producer always materialises into a fresh MIR local (never a
            // parameter slot or an existing binding base); the guard keeps the mint
            // fail-closed if a future arg shape reuses a slot.
            let Some(Place::Local(local)) = arg_places.get(index).copied() else {
                continue;
            };
            if self.parameter_locals.contains(&local) {
                continue;
            }
            self.register_synthetic_owned_local(SYNTHETIC_TEMP_ARG_NAME, arg.site, local, owned_ty);
        }
        if !proven_borrow_args.is_empty() {
            self.proven_borrow_call_args
                .insert(self.current_block_id, proven_borrow_args);
        }
        self.finish_current_block(Terminator::Call {
            callee: callee_symbol.to_string(),
            builtin,
            args: arg_places,
            dest,
            next,
        });
        // A `Never`-typed direct call (the runtime `panic()`/`exit()` shims,
        // and any other callee whose checker-resolved return type is
        // `ResolvedTy::Never`) never falls through to `next` at runtime — the
        // call terminator is a real divergence, exactly like an explicit
        // `return`. `start_block` always opens a normally-reachable cursor
        // (see its own doc comment), so a plain `start_block(next)` here
        // would silently mark the continuation reachable even though no
        // predecessor can ever reach it. That falsifies every downstream
        // `!self.cursor_unreachable` join-reachability check (If/match arm
        // lowering) for an all-panic/exit diverging arm: the join gets
        // wrongly admitted as reachable, and MIR's mixed-divergence recovery
        // then tries to move the substituted `Unit` (i8) result local into a
        // non-scalar (ptr/struct) return slot — a `Move type mismatch`
        // codegen-front fail-closed abort (hew-lang/hew#1913). Use
        // `start_dead_block` instead, mirroring the early-return path's own
        // dead-end convention, so the continuation is correctly flagged
        // unreachable and every existing join-reachability gate works for
        // `panic()`/`exit()` the same way it already does for `return`.
        if matches!(ret_ty, ResolvedTy::Never) {
            self.start_dead_block(next);
            // Unlike the `return`-seeded dead block this convention mirrors,
            // THIS dead block's id is already referenced by the `Call`
            // terminator just sealed above (`next`). Flag it so
            // `finalize_blocks` seals rather than drops it if it ends up
            // empty at true function end (hew-lang/hew#2425) — see that
            // field's doc comment for the full mechanism.
            self.dead_cursor_is_call_continuation = true;
        } else {
            self.start_block(next);
        }

        dest
    }

    /// Suspendable-caller flip for the four builtin recv/send/sleep families.
    /// In a caller that carries the execution context (actor handler / closure /
    /// task entry), each blocking builtin call SUSPENDS on the coro substrate
    /// instead of pinning an OS worker — emitting the matching `Suspending*`
    /// carrier and recording its [`SuspendKind`] payload. A
    /// `FunctionCallConv::Default` caller (`main`, a free fn) has no parkable
    /// continuation and keeps the blocking call, so this returns `None` and
    /// `lower_direct_call` falls through to the plain `Terminator::Call`.
    ///
    /// `ControlFlow::Break(dest)` when a flip fired (the resolved result, which
    /// the caller propagates as its own return); `ControlFlow::Continue(())`
    /// when no family matched or the arg/result shape did not fit a flip, so the
    /// caller falls through to the blocking `Terminator::Call`.
    fn try_lower_suspending_builtin_flip(
        &mut self,
        callee_symbol: &str,
        builtin: Option<hew_types::runtime_call::RuntimeCallFamily>,
        ret_ty: &ResolvedTy,
        dest: Option<Place>,
        arg_places: &[Place],
    ) -> std::ops::ControlFlow<Option<Place>> {
        use std::ops::ControlFlow;
        if !self.current_function_call_conv.carries_execution_context() {
            return ControlFlow::Continue(());
        }

        // `await stream.recv()` (NEW-7): SUSPENDS over the channel-await
        // substrate, carrying the checker-resolved element type from the recv's
        // `Option<T>` binding (never the runtime symbol name —
        // `checker-authority`/`type-info-survival`). Reuses the SAME
        // `carries_execution_context` discriminator as `lower_conn_await_read`
        // (DI-019/DI-020).
        if builtin == Some(hew_types::runtime_call::RuntimeCallFamily::StreamNextLayout) {
            if let (Some(result_dest), [stream], Some(elem_ty)) =
                (dest, arg_places, option_payload_ty(ret_ty))
            {
                let next = self.alloc_block();
                // The carrier rides the multi-suspend epilogue, so `cleanup`
                // reuses `next` exactly as `SuspendingRead`/`SuspendingAsk` do.
                self.record_suspend_kind(SuspendKind::StreamNext {
                    stream: *stream,
                    result_dest,
                    elem_ty: elem_ty.clone(),
                    deadline_result_dest: None,
                    error_dest: None,
                });
                self.finish_current_block(Terminator::Suspend {
                    resume: next,
                    cleanup: next,
                    is_final: false,
                });
                self.start_block(next);
                return ControlFlow::Break(dest);
            }
        }

        // `await rx.recv()` over a `std::channel` `Receiver<T>` (NEW-4):
        // SUSPENDS over the channel-await substrate. `try_recv` never suspends
        // and keeps the blocking call (it is a different family).
        if builtin == Some(hew_types::runtime_call::RuntimeCallFamily::ChannelRecvLayout) {
            if let (Some(result_dest), [receiver], Some(elem_ty)) =
                (dest, arg_places, option_payload_ty(ret_ty))
            {
                let next = self.alloc_block();
                self.record_suspend_kind(SuspendKind::ChannelRecv {
                    receiver: *receiver,
                    result_dest,
                    elem_ty: elem_ty.clone(),
                    deadline_result_dest: None,
                    error_dest: None,
                });
                self.finish_current_block(Terminator::Suspend {
                    resume: next,
                    cleanup: next,
                    is_final: false,
                });
                self.start_block(next);
                return ControlFlow::Break(dest);
            }
        }

        // `await sink.send(x)`: SUSPENDS on a full ring (backpressure-aware); a
        // non-full ring binds immediately (the runtime fast path). Context-free
        // callers keep the blocking call. Fires for every describable element
        // (bytes/string/layout) — the `[sink, value]` arg shape holds for all
        // three `(sink, data)` symbols; codegen selects the runtime entry from
        // the value's `ResolvedTy`.
        if builtin.as_ref().and_then(|f| f.is_async_suspending())
            == Some(hew_types::runtime_call::AsyncSuspendKind::SinkSend)
        {
            if let [sink, value] = arg_places {
                let next = self.alloc_block();
                self.record_suspend_kind(SuspendKind::StreamSend {
                    sink: *sink,
                    value: *value,
                });
                self.finish_current_block(Terminator::Suspend {
                    resume: next,
                    cleanup: next,
                    is_final: false,
                });
                self.start_block(next);
                return ControlFlow::Break(dest);
            }
        }

        // `sleep(d)` suspends in an execution-context caller on a timer-wheel
        // deadline; in a free-fn / `fn main` it calls `hew_sleep_ns` (blocking).
        // Identified by callee symbol — `sleep` is a `RuntimeFfiShim` with no
        // `RuntimeCallFamily`.
        if callee_symbol == "sleep" || callee_symbol == "hew_sleep_ns" {
            if let [duration_ns] = arg_places {
                let next = self.alloc_block();
                self.record_suspend_kind(SuspendKind::Sleep {
                    duration_ns: *duration_ns,
                });
                self.finish_current_block(Terminator::Suspend {
                    resume: next,
                    cleanup: next,
                    is_final: false,
                });
                self.start_block(next);
                return ControlFlow::Break(dest);
            }
        }

        // `sleep_until(i)` suspends until the given `instant` in an
        // execution-context caller; calls `hew_sleep_until_ns` on the blocking path.
        if callee_symbol == "sleep_until" || callee_symbol == "hew_sleep_until_ns" {
            if let [instant_ns] = arg_places {
                let next = self.alloc_block();
                self.record_suspend_kind(SuspendKind::SleepUntil {
                    instant_ns: *instant_ns,
                });
                self.finish_current_block(Terminator::Suspend {
                    resume: next,
                    cleanup: next,
                    is_final: false,
                });
                self.start_block(next);
                return ControlFlow::Break(dest);
            }
        }

        ControlFlow::Continue(())
    }

    fn lower_runtime_call(
        &mut self,
        symbol: &str,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
        result_ty: Option<&ResolvedTy>,
    ) -> Option<Place> {
        // Construction-time contract: the symbol must be in the allowlist.
        // This is the HIR-string-boundary gate: the caller dispatched this
        // symbol from a `BindingRef` name, so we assert in all build profiles
        // that it is known before we dispatch to a symbol-specific arm.
        // `RuntimeCall::new` enforces the same invariant at the MIR data level;
        // this assert defends the dispatch table (LESSONS `boundary-fail-closed`).
        assert!(
            crate::runtime_symbols::is_known_runtime_symbol(symbol),
            "lower_runtime_call called with unrecognised symbol `{symbol}`; \
             the call site must gate on is_known_runtime_symbol first"
        );

        match symbol {
            "hew_duplex_pair" => self.lower_duplex_pair(hir_args, site),
            "hew_duplex_send" => self.lower_duplex_send(hir_args, site, context, result_ty),
            "hew_duplex_close" => self.lower_duplex_close(hir_args, site, context, result_ty),
            "hew_duplex_send_half" | "hew_duplex_recv_half" => {
                self.lower_duplex_half_extract(symbol, hir_args, site, result_ty)
            }
            "hew_send_half_send" | "hew_send_half_try_send" => {
                self.lower_half_send(symbol, hir_args, site, context, result_ty)
            }
            "hew_recv_half_recv"
            | "hew_recv_half_try_recv"
            | "hew_duplex_recv"
            | "hew_duplex_try_recv" => {
                self.lower_duplex_recv(symbol, hir_args, site, context, result_ty)
            }
            "hew_duplex_close_half" => self.lower_half_close(hir_args, site, context, result_ty),
            "hew_supervisor_stop" => self.lower_supervisor_stop(hir_args, site),
            "hew_actor_link" | "hew_actor_monitor" => {
                self.lower_actor_link_or_monitor(symbol, hir_args, site, context, result_ty)
            }
            // `hew_actor_demonitor(ref_id: i64) -> void`: cancels a monitor.
            // The auto-drop path for a MonitorRef value (scope-exit, the common
            // case) goes through RuntimeDropDescriptor::MonitorRefClose →
            // lower_drop_runtime (struct-field extraction in llvm.rs), NOT this
            // arm. This arm lowers the DIRECT call in the body of
            // `impl MonitorRef { fn close(self) { hew_actor_demonitor(self.ref_id) } }`
            // (std/link_monitor.hew): a program that `import std::link_monitor`s
            // lowers that inherent `close` body, whose `unsafe` block calls the
            // symbol directly with a plain i64 `ref_id`. Returns void; a
            // value-needed context is fail-closed in the helper.
            "hew_actor_demonitor" => {
                self.lower_simple_void_runtime_call(symbol, hir_args, site, context)
            }
            "hew_actor_unlink" => self.lower_actor_unlink(hir_args, site, context),
            "hew_bytes_push" => self.lower_bytes_push(hir_args, site, context),
            "hew_vec_len" => self.lower_bytes_len(hir_args, site, context),
            "hew_bytes_pop" => self.lower_bytes_pop(hir_args, site, context),
            "hew_bytes_set" => self.lower_bytes_set(hir_args, site, context),
            "hew_bytes_is_empty" => self.lower_bytes_is_empty(hir_args, site, context),
            "hew_bytes_contains" => self.lower_bytes_contains(hir_args, site, context),
            "hew_bytes_clear" => self.lower_bytes_clear(hir_args, site, context),
            "hew_bytes_append" => self.lower_bytes_append(hir_args, site, context),
            "hew_bytes_get" => self.lower_bytes_get_option(hir_args, site, context, result_ty),
            "hew_string_get" => self.lower_string_get_option(hir_args, site, context, result_ty),
            // Sentinel-wrapping string inspectors: the runtime returns `-1`
            // for miss/OOB; codegen intercepts the callee and materialises
            // `None` / `Some(...)` (D46 sentinel -> Option sweep).
            "hew_string_find" | "hew_string_char_at" | "hew_string_char_at_utf8" => {
                self.lower_string_sentinel_option(symbol, hir_args, site, context, result_ty)
            }
            "hew_string_char_count" => self.lower_string_char_count(hir_args, site, context),
            // Cross-node monitor extern surface, both `(...) -> i64`:
            //  - `hew_node_monitor(target_pid)` reached as a direct `extern "C"`
            //    call (statement position; the value-position `monitor(RemotePid)`
            //    builtin routes through `lower_node_monitor`).
            //  - `hew_node_monitor_recv(ref_id, timeout_ms)` blocks for the
            //    distributed monitor's terminal signal and returns the carried
            //    down-reason; called from the std `MonitorRef::recv_down` body.
            "hew_node_monitor" | "hew_node_monitor_recv" => {
                self.lower_simple_int_runtime_call(symbol, hir_args, site, context, result_ty)
            }
            // Cross-node link: `link_remote(RemotePid<T>, PartitionPolicy)`
            // establishes a cross-node link and returns `Result<(), LinkError>`.
            // The remote target has no `HewActor*` in this address space, so it
            // routes to the node-link ABI keyed by the packed remote pid + the
            // policy discriminant; the linking subject (self) is resolved inside
            // the runtime. Unlike `monitor(RemotePid)` (which is dispatched out of
            // `hew_actor_monitor` by the RemotePid receiver type), `link_remote`
            // is its own builtin that always reaches the cross-node form.
            "hew_node_link_remote" => {
                self.lower_node_link_remote(hir_args, site, context, result_ty)
            }
            "hew_observe_read_u64"
            | "hew_observe_scrape"
            | "hew_observe_series"
            | "hew_observe_barrier" => {
                self.lower_observe_runtime_call(symbol, hir_args, site, context)
            }
            "hew_metric_counter_register"
            | "hew_metric_counter_inc"
            | "hew_metric_counter_add"
            | "hew_metric_gauge_register"
            | "hew_metric_gauge_set"
            | "hew_metric_gauge_inc"
            | "hew_metric_gauge_dec"
            | "hew_metric_gauge_add"
            | "hew_metric_histogram_register_simple"
            | "hew_metric_histogram_record" => {
                self.lower_metric_runtime_call(symbol, hir_args, site, context)
            }
            "hew_duration_nanos"
            | "hew_duration_micros"
            | "hew_duration_millis"
            | "hew_duration_secs"
            | "hew_duration_mins"
            | "hew_duration_hours"
            | "hew_duration_abs"
            | "hew_duration_is_zero" => {
                self.lower_duration_runtime_call(symbol, hir_args, site, context)
            }
            "hew_instant_now" | "hew_instant_elapsed" | "hew_instant_duration_since" => {
                self.lower_instant_runtime_call(symbol, hir_args, site, context)
            }
            _ => {
                // Known-allowlisted symbol but no producer arm yet.  Fail closed
                // so the pipeline rejects the program before codegen runs.
                // Individual symbol producers land in follow-up slices (recv,
                // half-handle split, close, lambda-actor lifecycle).
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!("runtime call `{symbol}`"),
                        site,
                    },
                    note: format!(
                        "`{symbol}` is a recognised runtime symbol but has no \
                         MIR producer arm yet; wired per-symbol in follow-up slices"
                    ),
                });
                None
            }
        }
    }

    /// Materialise the `()` result for a bytes mutator (`push`/`set`/`clear`/
    /// `append`) used in value/match-arm position. The op already emitted its
    /// side-effecting `push_runtime_call`; the write-back and drop accounting
    /// are identical to statement position. In value position the caller binds
    /// a unit, so allocate a fresh zero-sized Unit and define it; in statement
    /// position the result is discarded and we return `None`.
    fn lower_bytes_unit_result(&mut self, context: RuntimeCallContext) -> Option<Place> {
        if context != RuntimeCallContext::ValueNeeded {
            return None;
        }
        let dest = self.alloc_local(ResolvedTy::Unit);
        self.push_instr(Instr::UnitLit { dest });
        Some(dest)
    }

    fn lower_bytes_push(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
    ) -> Option<Place> {
        if hir_args.len() != 2 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_bytes_push` arity".to_string(),
                    site,
                },
                note: format!(
                    "`hew_bytes_push` expects a bytes receiver and one byte argument, got {}",
                    hir_args.len()
                ),
            });
            return None;
        }
        let bytes = self.lower_value(&hir_args[0])?;
        let byte = self.lower_value(&hir_args[1])?;
        self.push_runtime_call("hew_bytes_push", vec![bytes, byte], None);
        self.lower_bytes_unit_result(context)
    }

    /// Emit `hew_bytes_pop(&mut BytesTriple) -> i64` for `bytes.pop()`.
    ///
    /// Returns the popped byte as an i64 dest when a value is needed; codegen
    /// passes the receiver alloca address so the runtime writes back the
    /// shrunken triple. An empty buffer fails closed in the runtime (the spec
    /// `pop` signature has no Option). The receiver is BORROWED — listed in
    /// bytes-receiver contract, so it keeps its scope-exit drop.
    fn lower_bytes_pop(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
    ) -> Option<Place> {
        if hir_args.len() != 1 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_bytes_pop` arity".to_string(),
                    site,
                },
                note: format!(
                    "`hew_bytes_pop` (bytes.pop) expects 1 argument (receiver), got {}",
                    hir_args.len()
                ),
            });
            return None;
        }
        let buf = self.lower_value(&hir_args[0])?;
        let dest =
            (context == RuntimeCallContext::ValueNeeded).then(|| self.alloc_local(ResolvedTy::I64));
        self.push_runtime_call("hew_bytes_pop", vec![buf], dest);
        dest
    }

    /// Emit `hew_bytes_set(&mut BytesTriple, index, byte)` for `bytes.set(i, b)`.
    ///
    /// Statement-position mutation: codegen passes the receiver alloca address
    /// (write-back after `CoW`) plus the i64 index and the byte. An
    /// out-of-range index fails closed in the runtime. The receiver is
    /// BORROWED.
    fn lower_bytes_set(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
    ) -> Option<Place> {
        if hir_args.len() != 3 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_bytes_set` arity".to_string(),
                    site,
                },
                note: format!(
                    "`hew_bytes_set` (bytes.set) expects 3 arguments (receiver, index, byte), \
                     got {}",
                    hir_args.len()
                ),
            });
            return None;
        }
        let buf = self.lower_value(&hir_args[0])?;
        let idx = self.lower_value(&hir_args[1])?;
        let byte = self.lower_value(&hir_args[2])?;
        self.push_runtime_call("hew_bytes_set", vec![buf, idx, byte], None);
        self.lower_bytes_unit_result(context)
    }

    /// Emit `hew_bytes_is_empty(*const BytesTriple) -> bool` for
    /// `bytes.is_empty()`. Pure read; the receiver is BORROWED.
    fn lower_bytes_is_empty(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
    ) -> Option<Place> {
        if hir_args.len() != 1 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_bytes_is_empty` arity".to_string(),
                    site,
                },
                note: format!(
                    "`hew_bytes_is_empty` (bytes.is_empty) expects 1 argument (receiver), got {}",
                    hir_args.len()
                ),
            });
            return None;
        }
        let buf = self.lower_value(&hir_args[0])?;
        let dest = (context == RuntimeCallContext::ValueNeeded)
            .then(|| self.alloc_local(ResolvedTy::Bool));
        self.push_runtime_call("hew_bytes_is_empty", vec![buf], dest);
        dest
    }

    /// Emit `hew_bytes_contains(*const BytesTriple, byte) -> bool` for
    /// `bytes.contains(b)`. Pure read; the receiver is BORROWED.
    fn lower_bytes_contains(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
    ) -> Option<Place> {
        if hir_args.len() != 2 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_bytes_contains` arity".to_string(),
                    site,
                },
                note: format!(
                    "`hew_bytes_contains` (bytes.contains) expects 2 arguments (receiver, byte), \
                     got {}",
                    hir_args.len()
                ),
            });
            return None;
        }
        let buf = self.lower_value(&hir_args[0])?;
        let byte = self.lower_value(&hir_args[1])?;
        let dest = (context == RuntimeCallContext::ValueNeeded)
            .then(|| self.alloc_local(ResolvedTy::Bool));
        self.push_runtime_call("hew_bytes_contains", vec![buf, byte], dest);
        dest
    }

    /// Emit `hew_bytes_clear(&mut BytesTriple)` for `bytes.clear()`.
    ///
    /// Statement-position in-place reset; codegen passes the receiver alloca
    /// address so the runtime releases the buffer ref and writes back the empty
    /// triple. The receiver is BORROWED (clear releases its OWN reference and
    /// leaves the binding owning a null triple whose scope-exit drop is inert).
    fn lower_bytes_clear(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
    ) -> Option<Place> {
        if hir_args.len() != 1 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_bytes_clear` arity".to_string(),
                    site,
                },
                note: format!(
                    "`hew_bytes_clear` (bytes.clear) expects 1 argument (receiver), got {}",
                    hir_args.len()
                ),
            });
            return None;
        }
        let buf = self.lower_value(&hir_args[0])?;
        self.push_runtime_call("hew_bytes_clear", vec![buf], None);
        self.lower_bytes_unit_result(context)
    }

    /// Emit `hew_bytes_append(&mut dst, ...)` for `bytes.append(other)`.
    ///
    /// Statement-position mutation. MIR carries the two `bytes` places
    /// `[dst, other]`; codegen passes the dst alloca address (write-back) and
    /// unpacks `other` into the scalar `(src_ptr, src_offset, src_len)` runtime
    /// args. Both operands are BORROWED — `hew_bytes_append` copies the source
    /// region and never takes its reference (see
    /// the bytes-all-args contract, so `other` keeps its scope-exit drop.
    fn lower_bytes_append(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
    ) -> Option<Place> {
        if hir_args.len() != 2 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_bytes_append` arity".to_string(),
                    site,
                },
                note: format!(
                    "`hew_bytes_append` (bytes.append) expects 2 arguments (receiver, other), \
                     got {}",
                    hir_args.len()
                ),
            });
            return None;
        }
        let dst = self.lower_value(&hir_args[0])?;
        let other = self.lower_value(&hir_args[1])?;
        self.push_runtime_call("hew_bytes_append", vec![dst, other], None);
        self.lower_bytes_unit_result(context)
    }

    /// Emit `hew_vec_len(buf) -> i64` for `bytes.len()` calls.
    ///
    /// The `impl bytes` extern block in `std/io.hew` declares `len` with
    /// `#[extern_symbol(hew_vec_len)]`. At MIR time the callee name is
    /// already `hew_vec_len` (allowlisted), so it routes here rather than
    /// through the for-in-loop path that uses `hew_vec_len` directly.
    /// ABI: 1 arg (bytes receiver, passed as a `*mut HewVec`), returns `i64`.
    fn lower_bytes_len(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
    ) -> Option<Place> {
        if hir_args.len() != 1 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_vec_len` arity".to_string(),
                    site,
                },
                note: format!(
                    "`hew_vec_len` (bytes.len) expects 1 argument (receiver), got {}",
                    hir_args.len()
                ),
            });
            return None;
        }
        let buf = self.lower_value(&hir_args[0])?;
        let dest =
            (context == RuntimeCallContext::ValueNeeded).then(|| self.alloc_local(ResolvedTy::I64));
        self.push_runtime_call("hew_vec_len", vec![buf], dest);
        dest
    }

    /// Emit `bytes.get(index) -> Option<u8>`, the non-trapping byte accessor.
    ///
    /// De-aliased from the trapping `b[i]` sugar (`hew_bytes_index`, which
    /// aborts on OOB): `.get` returns `None` out of bounds instead of trapping.
    /// Mirrors the Vec/HashMap `.get` shape — a single `Terminator::Call` to a
    /// codegen-intercepted symbol (`hew_bytes_get`) that owns the bounds-check
    /// CFG and the `Some`/`None` materialisation. The symbol carries no runtime
    /// export (`builtin: None`, like `hew_vec_get_clone`): codegen does the
    /// check over the stack-resident `BytesTriple` and an in-bounds typed load.
    ///
    /// The receiver is BORROWED, not consumed — `hew_bytes_get` carries the
    /// collection-receiver contract, so `buf` keeps its scope-exit drop. The
    /// `u8` element is a scalar (Copy): the `Some` payload is a
    /// by-value load with no owned clone, so drop-safety is trivial.
    fn lower_bytes_get_option(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
        result_ty: Option<&ResolvedTy>,
    ) -> Option<Place> {
        if hir_args.len() != 2 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_bytes_get` arity".to_string(),
                    site,
                },
                note: format!(
                    "`hew_bytes_get` (bytes.get) expects 2 arguments (receiver, index), got {}",
                    hir_args.len()
                ),
            });
            return None;
        }
        // The checker types `b.get(i)` as `Option<u8>`; size the dest enum slot
        // with that exact type so codegen resolves the registered Option layout
        // (`checker-authority`: consume the recorded type, never re-infer it).
        let Some(opt_ty) = result_ty else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_bytes_get` result type".to_string(),
                    site,
                },
                note: "`hew_bytes_get` (bytes.get) needs the checker-recorded \
                       `Option<u8>` result type to size its dest slot"
                    .to_string(),
            });
            return None;
        };
        let buf = self.lower_value(&hir_args[0])?;
        let idx = self.lower_value(&hir_args[1])?;
        // Always materialise the Option; the bounds-check CFG lives in codegen.
        // A discarded result is a dead local the optimiser elides, but the Call
        // terminator still needs a dest + a `next` block to continue into.
        let result = self.alloc_local(opt_ty.clone());
        let next = self.alloc_block();
        self.finish_current_block(Terminator::Call {
            callee: "hew_bytes_get".to_string(),
            builtin: None,
            args: vec![buf, idx],
            dest: Some(result),
            next,
        });
        self.start_block(next);
        let _ = context;
        Some(result)
    }

    /// Lower `string.get(index) -> Option<char>` to a single `Terminator::Call`
    /// to the codegen-intercepted `hew_string_get` symbol.
    ///
    /// Mirrors the bytes `.get` shape — the symbol carries no runtime export
    /// (`builtin: None`): codegen bounds-checks the index against
    /// `hew_string_char_count` and materialises `Some(char)` / `None` over the
    /// in-bounds `hew_string_index` codepoint load.
    ///
    /// The receiver is BORROWED, not consumed — `hew_string_get` carries the
    /// collection-receiver contract, so `s` keeps its scope-exit drop. The
    /// `char` element is a scalar (Copy): the `Some` payload is a by-value
    /// codepoint with no owned clone, so drop-safety is trivial.
    fn lower_string_get_option(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
        result_ty: Option<&ResolvedTy>,
    ) -> Option<Place> {
        if hir_args.len() != 2 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_string_get` arity".to_string(),
                    site,
                },
                note: format!(
                    "`hew_string_get` (string.get) expects 2 arguments (receiver, index), got {}",
                    hir_args.len()
                ),
            });
            return None;
        }
        // The checker types `s.get(i)` as `Option<char>`; size the dest enum slot
        // with that exact type so codegen resolves the registered Option layout
        // (`checker-authority`: consume the recorded type, never re-infer it).
        let Some(opt_ty) = result_ty else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_string_get` result type".to_string(),
                    site,
                },
                note: "`hew_string_get` (string.get) needs the checker-recorded \
                       `Option<char>` result type to size its dest slot"
                    .to_string(),
            });
            return None;
        };
        let s = self.lower_value(&hir_args[0])?;
        let idx = self.lower_value(&hir_args[1])?;
        // Always materialise the Option; the bounds-check CFG lives in codegen.
        // A discarded result is a dead local the optimiser elides, but the Call
        // terminator still needs a dest + a `next` block to continue into.
        let result = self.alloc_local(opt_ty.clone());
        let next = self.alloc_block();
        self.finish_current_block(Terminator::Call {
            callee: "hew_string_get".to_string(),
            builtin: None,
            args: vec![s, idx],
            dest: Some(result),
            next,
        });
        self.start_block(next);
        let _ = context;
        Some(result)
    }

    /// Lower a sentinel-wrapping string inspector (`string.find(needle)`,
    /// `string.char_at(i)`, `string.codepoint_at_utf8(i)`) to a single
    /// `Terminator::Call` to the codegen-intercepted runtime symbol.
    ///
    /// Mirrors the `string.get` shape: the checker records the `Option<...>`
    /// result type (`Option<i64>` for find/codepoint, `Option<char>` for
    /// `char_at`); codegen calls the real runtime entry (which keeps its `-1`
    /// miss/OOB sentinel at the C ABI) and materialises `Some(value)` /
    /// `None` from the sign of the result (D46 sentinel -> Option sweep).
    ///
    /// The receiver and needle are BORROWED (string-inspector contract); the
    /// `Some` payload is a scalar (Copy), so drop-safety is trivial.
    fn lower_string_sentinel_option(
        &mut self,
        symbol: &str,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
        result_ty: Option<&ResolvedTy>,
    ) -> Option<Place> {
        if hir_args.len() != 2 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("runtime call `{symbol}` arity"),
                    site,
                },
                note: format!(
                    "`{symbol}` expects 2 arguments (receiver, needle/index), got {}",
                    hir_args.len()
                ),
            });
            return None;
        }
        // The checker types the call as `Option<...>`; size the dest enum slot
        // with that exact type so codegen resolves the registered Option layout
        // (`checker-authority`: consume the recorded type, never re-infer it).
        let Some(opt_ty) = result_ty else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("runtime call `{symbol}` result type"),
                    site,
                },
                note: format!(
                    "`{symbol}` needs the checker-recorded `Option<...>` result \
                     type to size its dest slot"
                ),
            });
            return None;
        };
        let s = self.lower_value(&hir_args[0])?;
        let arg = self.lower_value(&hir_args[1])?;
        // Always materialise the Option; the sentinel-branch CFG lives in
        // codegen. A discarded result is a dead local the optimiser elides,
        // but the Call terminator still needs a dest + a `next` block.
        let result = self.alloc_local(opt_ty.clone());
        let next = self.alloc_block();
        self.finish_current_block(Terminator::Call {
            callee: symbol.to_string(),
            builtin: None,
            args: vec![s, arg],
            dest: Some(result),
            next,
        });
        self.start_block(next);
        let _ = context;
        Some(result)
    }

    /// Emit `hew_string_char_count(s) -> i32`, widened to the Hew-facing `i64`.
    /// The runtime ABI returns i32, while the stdlib-facing
    /// `string.char_count_utf8()` declaration returns i64. Keep the call ABI
    /// honest by storing the runtime result in an i32 temporary and inserting
    /// the same explicit `NumericCast` used by open-end string slicing.
    fn lower_string_char_count(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
    ) -> Option<Place> {
        if hir_args.len() != 1 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "runtime call `hew_string_char_count` arity".to_string(),
                    site,
                },
                note: format!(
                    "`hew_string_char_count` expects 1 argument (receiver), got {}",
                    hir_args.len()
                ),
            });
            return None;
        }

        let s = self.lower_value(&hir_args[0])?;
        if context != RuntimeCallContext::ValueNeeded {
            self.push_runtime_call("hew_string_char_count", vec![s], None);
            return None;
        }

        let count_i32 = self.alloc_local(ResolvedTy::I32);
        self.push_runtime_call("hew_string_char_count", vec![s], Some(count_i32));
        let count_i64 = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::NumericCast {
            dest: count_i64,
            src: count_i32,
            from_ty: ResolvedTy::I32,
            to_ty: ResolvedTy::I64,
        });
        Some(count_i64)
    }

    fn lower_observe_runtime_call(
        &mut self,
        symbol: &str,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
    ) -> Option<Place> {
        let (expected_arity, return_ty) = match symbol {
            "hew_observe_read_u64" => (1, ResolvedTy::I64),
            "hew_observe_scrape" | "hew_observe_series" => (0, ResolvedTy::String),
            "hew_observe_barrier" => (0, ResolvedTy::I64),
            _ => unreachable!("observe lowering called for non-observe symbol"),
        };
        if hir_args.len() != expected_arity {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("runtime call `{symbol}` arity"),
                    site,
                },
                note: format!(
                    "`{symbol}` expects {expected_arity} argument(s), got {}",
                    hir_args.len()
                ),
            });
            return None;
        }

        let mut args = Vec::with_capacity(hir_args.len());
        for arg in hir_args {
            args.push(self.lower_value(arg)?);
        }
        let dest =
            (context == RuntimeCallContext::ValueNeeded).then(|| self.alloc_local(return_ty));
        self.push_runtime_call(symbol, args, dest);
        dest
    }

    /// Lower the scalar `hew_metric_*` emit path that `std::metrics` reaches
    /// through its `extern "C"` block.
    ///
    /// Only the scalar surface routes here: a metric name comes in as a Hew
    /// `string` (lowered like any other string place), a handle and counter /
    /// gauge value as `i64`, and a histogram observation as `f64`. The register
    /// entry points return the `i64` slot handle (>= 0 valid, -1 on a cap /
    /// charset / collision failure); the mutators return unit.
    ///
    /// The labelled `*Vec` registration and the bucketed histogram registration
    /// take raw C arrays (`*const i64` / `*const *const c_char` plus a length)
    /// and are intentionally NOT routed here — a Hew `extern "C"` declaration
    /// marshals a `Vec<T>` to a single `*mut HewVec`, which does not match the
    /// `(ptr, len)` ABI those symbols expose. They stay fail-closed in the
    /// dispatch table until a `HewVec`-shaped ABI lands.
    fn lower_metric_runtime_call(
        &mut self,
        symbol: &str,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
    ) -> Option<Place> {
        let (expected_arity, return_ty) = match symbol {
            "hew_metric_counter_register"
            | "hew_metric_gauge_register"
            | "hew_metric_histogram_register_simple" => (1, Some(ResolvedTy::I64)),
            "hew_metric_counter_inc" | "hew_metric_gauge_inc" | "hew_metric_gauge_dec" => (1, None),
            "hew_metric_counter_add"
            | "hew_metric_gauge_set"
            | "hew_metric_gauge_add"
            | "hew_metric_histogram_record" => (2, None),
            _ => unreachable!("metric lowering called for non-scalar-metric symbol `{symbol}`"),
        };
        if hir_args.len() != expected_arity {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("runtime call `{symbol}` arity"),
                    site,
                },
                note: format!(
                    "`{symbol}` expects {expected_arity} argument(s), got {}",
                    hir_args.len()
                ),
            });
            return None;
        }

        let mut args = Vec::with_capacity(hir_args.len());
        for arg in hir_args {
            args.push(self.lower_value(arg)?);
        }
        let dest = match return_ty {
            Some(ty) if context == RuntimeCallContext::ValueNeeded => Some(self.alloc_local(ty)),
            _ => None,
        };
        self.push_runtime_call(symbol, args, dest);
        dest
    }

    /// Lower the `impl duration` receiver methods declared in
    /// `std/builtins.hew` (`#[extern_symbol(hew_duration_*)]`).
    ///
    /// Every symbol takes a single i64-backed `duration` receiver. The
    /// conversion/predicate symbols return `i64`; `hew_duration_is_zero`
    /// returns the C `i32` boolean (`1`/`0`) that codegen narrows to `i1` at
    /// the call boundary, with no explicit cast.
    fn lower_duration_runtime_call(
        &mut self,
        symbol: &str,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
    ) -> Option<Place> {
        if hir_args.len() != 1 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("runtime call `{symbol}` arity"),
                    site,
                },
                note: format!(
                    "`{symbol}` expects 1 argument (the duration receiver), got {}",
                    hir_args.len()
                ),
            });
            return None;
        }

        let return_ty = if symbol == "hew_duration_is_zero" {
            ResolvedTy::Bool
        } else {
            ResolvedTy::I64
        };

        let receiver = self.lower_value(&hir_args[0])?;
        let dest =
            (context == RuntimeCallContext::ValueNeeded).then(|| self.alloc_local(return_ty));
        self.push_runtime_call(symbol, vec![receiver], dest);
        dest
    }
    /// Synthesize the current actor's own handle via the `hew_actor_self()`
    /// runtime primitive and return the `Place` holding it.
    ///
    /// The result is a *borrowed* `*mut HewActor` typed `LocalPid<Unit>` (no
    /// ownership transfer), so the destination local carries no drop obligation
    /// — `alloc_local` records type bookkeeping only and never registers a drop.
    /// This is the single self-handle emitter shared by `link`/`monitor`/
    /// `unlink` (which pass it as the implicit `self` first ABI argument) and by
    /// the `HirExprKind::ActorSelf` value arm (`this` used as a value). Keeping
    /// one emitter avoids the divergent self-handle synthesis the value-class
    /// and ABI agreement invariants warn against.
    pub(crate) fn emit_actor_self_handle(&mut self) -> Place {
        let self_handle = self.alloc_local(ResolvedTy::Named {
            name: hew_types::BuiltinType::LocalPid
                .canonical_name()
                .to_string(),
            args: vec![ResolvedTy::Unit],
            builtin: Some(hew_types::BuiltinType::LocalPid),
            is_opaque: false,
        });
        self.push_runtime_call("hew_actor_self", vec![], Some(self_handle));
        self_handle
    }

    /// Lower the `impl instant` methods declared in `std/builtins.hew`
    /// (`#[extern_symbol(hew_instant_*)]`).
    ///
    /// `instant` is i64-backed (a monotonic nanosecond timestamp), so every
    /// argument and result is a bare `i64`:
    /// - `hew_instant_now()` -> `i64` (no receiver; reads the monotonic clock).
    /// - `hew_instant_elapsed(now: i64)` -> `i64` (a `duration` in ns).
    /// - `hew_instant_duration_since(now: i64, earlier: i64)` -> `i64`.
    ///
    /// The arity is derived from the symbol so a malformed call fails closed
    /// before codegen rather than silently mis-marshalling the ABI.
    fn lower_instant_runtime_call(
        &mut self,
        symbol: &str,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
        context: RuntimeCallContext,
    ) -> Option<Place> {
        let expected_arity = match symbol {
            "hew_instant_now" => 0,
            "hew_instant_elapsed" => 1,
            "hew_instant_duration_since" => 2,
            _ => unreachable!("instant lowering called for non-instant symbol `{symbol}`"),
        };
        if hir_args.len() != expected_arity {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("runtime call `{symbol}` arity"),
                    site,
                },
                note: format!(
                    "`{symbol}` expects {expected_arity} argument(s), got {}",
                    hir_args.len()
                ),
            });
            return None;
        }

        let mut arg_places = Vec::with_capacity(hir_args.len());
        for arg in hir_args {
            arg_places.push(self.lower_value(arg)?);
        }
        let dest =
            (context == RuntimeCallContext::ValueNeeded).then(|| self.alloc_local(ResolvedTy::I64));
        self.push_runtime_call(symbol, arg_places, dest);
        dest
    }
}

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
