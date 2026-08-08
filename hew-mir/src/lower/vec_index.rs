//! The ordinary `Vec` indexing ABI: `xs[i]` lowering and the ownership
//! classification of what an indexed read hands its reader.
//!
//! Carved out of `expr.rs` as one coherent concern. `lower_vec_index` emits the
//! bounds-checked load; [`Builder::vec_element_get_family`] is the single
//! authority for WHICH runtime getter that load uses. `VecIter::next` does not
//! come through this module: it synthesises the separate `Vec::get` clone-out
//! choke so iterator ownership can be strengthened without changing `xs[i]`.

use super::{
    ty_is_local_collection_handle, ty_is_vec, Builder, CmpPred, HirExpr, Instr, MirDiagnostic,
    MirDiagnosticKind, Place, ResolvedTy, Terminator, TrapKind, ValueClass,
};
use hew_types::runtime_call::{RuntimeCallFamily, VecGetElem};

impl Builder {
    fn reject_drop_only_receiver_index(
        &mut self,
        elem_ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> bool {
        if !matches!(
            self.subst_ty(elem_ty),
            ResolvedTy::Named {
                builtin: Some(hew_types::BuiltinType::Receiver),
                ..
            }
        ) {
            return false;
        }
        self.diagnostics.push(MirDiagnostic {
            kind: MirDiagnosticKind::NotYetImplemented {
                construct: "`Vec<channel.Receiver<_>>` index read".to_string(),
                site,
            },
            note: "indexing would clone a receiver out of the Vec, but channel.Receiver<T> \
                   is drop-only; consume the receiver through a moving operation instead."
                .to_string(),
        });
        true
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
    /// Element-type dispatch lives in [`Builder::vec_element_get_family`], the
    /// single element-load ABI authority the `for x in …` cursor gate also reads.
    ///
    /// Unsupported element types emit `MirDiagnostic::NotYetImplemented`
    /// and return `None` (tracked gap, not silent shim).
    pub(super) fn lower_vec_index(
        &mut self,
        container: &HirExpr,
        index: &HirExpr,
        elem_ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        if self.reject_drop_only_receiver_index(elem_ty, site) {
            return None;
        }
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
        let Some(get_family) = self.vec_element_get_family(elem_ty) else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("Vec<{elem_ty:?}> element type for xs[i]"),
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
        };
        // The clone getter is a block-terminating `Terminator::Call` (codegen
        // owns its out-pointer marshalling); every other getter is a plain
        // `CallRuntimeAbi` instruction. The typed family is the discriminator;
        // its C spelling is derived only at the ABI edge.
        let clone_owned_value = get_family == RuntimeCallFamily::VecGet(VecGetElem::Clone);
        let get_symbol = get_family.c_symbol();

        let result_place = self.alloc_local(elem_ty.clone());
        if clone_owned_value {
            let next = self.alloc_block();
            self.finish_current_block(Terminator::Call {
                callee: get_symbol.to_string(),
                // Clone-out remains a direct-call interceptor: codegen owns
                // its element-layout-dependent output slot. Carry the same
                // typed family that selected the call so MIR consumers never
                // have to recover ownership semantics from the C spelling.
                authority: crate::CallAuthority::Runtime(get_family),
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

    /// The runtime element getter `xs[i]` dispatches to for a `Vec<elem_ty>`
    /// element load, or `None` when the element class has no wired getter (the
    /// caller raises the tracked `NotYetImplemented` gap).
    ///
    /// This is the SINGLE authority for the ordinary indexing element-load ABI.
    ///
    /// Element-type dispatch:
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
    pub(crate) fn vec_element_get_family(&self, elem_ty: &ResolvedTy) -> Option<RuntimeCallFamily> {
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
        // HANDLES keep the established borrow contract: chained reads rely on
        // the outer Vec remaining the sole owner. Iterator reads bypass this
        // choice through the dedicated descriptor-backed clone-out call.
        //
        // A CLOSE-OBLIGATED element (contains a `#[resource]`) must NOT take the
        // clone choke: a resource has an affine close contract and no semantic
        // clone, so a cloned-out owner would mint a second close authority over
        // one context (double-close). It stays on the `hew_vec_get_owned`
        // BORROW — the Vec remains the sole owner and its scope-exit free runs
        // each element's close exactly once (the value-context lattice's
        // `CollectionIndexOut` rule, `drop_obligation.rs`).
        let owned_elem = self.is_owned_vec_element(elem_ty);
        let elem_needs_close = crate::model::ty_drop_obligation(
            elem_ty,
            &crate::model::MirHeapLayouts {
                record_field_orders: &self.record_field_orders,
                enum_layouts: &self.enum_layouts,
            },
            self.type_classes.lifecycle_registry(),
        )
        .needs_close;
        let clone_owned_value = owned_elem
            && !ty_is_vec(elem_ty)
            && !ty_is_local_collection_handle(elem_ty)
            && !elem_needs_close;
        let elem = match elem_ty {
            ResolvedTy::Bool => VecGetElem::Bool,
            ResolvedTy::I8 => VecGetElem::I8,
            ResolvedTy::U8 => VecGetElem::U8,
            ResolvedTy::I16 => VecGetElem::I16,
            ResolvedTy::U16 => VecGetElem::U16,
            ResolvedTy::Char | ResolvedTy::I32 | ResolvedTy::U32 => VecGetElem::I32,
            // `duration` is a signed 8-byte newtype — same i64-class getter as
            // i64 (`instant` reaches here already canonicalised to I64).
            ResolvedTy::I64
            | ResolvedTy::U64
            | ResolvedTy::Isize
            | ResolvedTy::Usize
            | ResolvedTy::Duration => VecGetElem::I64,
            ResolvedTy::F32 => VecGetElem::F32,
            ResolvedTy::F64 => VecGetElem::F64,
            ResolvedTy::String => VecGetElem::Str,
            _ if clone_owned_value => VecGetElem::Clone,
            _ if owned_elem => VecGetElem::Owned,
            // BitCopy Named value records: use layout-descriptor getter so the
            // runtime applies the correct element stride.
            ResolvedTy::Named { .. }
                if ValueClass::of_ty(elem_ty, &self.type_classes) == ValueClass::BitCopy =>
            {
                VecGetElem::Layout
            }
            // Tuples are BitCopy aggregates stored inline; same layout path.
            ResolvedTy::Tuple(_) => VecGetElem::Layout,
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
            ResolvedTy::Named { name, args, .. }
                if crate::model::find_enum_layout(name, args, &self.enum_layouts)
                    .is_some_and(|layout| !layout.is_indirect) =>
            {
                VecGetElem::Layout
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
                VecGetElem::Ptr
            }
            _ => return None,
        };
        Some(RuntimeCallFamily::VecGet(elem))
    }
}

#[cfg(test)]
mod tests {
    use super::{Builder, ResolvedTy};
    use hew_types::{
        runtime_call::{all_runtime_call_families, RuntimeCallFamily, VecGetElem},
        BuiltinType,
    };

    /// One type-checker witness for each typed getter variant. This match is
    /// deliberately exhaustive: extending `VecGetElem` cannot compile until
    /// ordinary-index dispatch proves how the new family is reached.
    fn witness_type(elem: VecGetElem) -> ResolvedTy {
        match elem {
            VecGetElem::Bool => ResolvedTy::Bool,
            VecGetElem::F32 => ResolvedTy::F32,
            VecGetElem::F64 => ResolvedTy::F64,
            VecGetElem::I8 => ResolvedTy::I8,
            VecGetElem::I16 => ResolvedTy::I16,
            VecGetElem::I32 => ResolvedTy::I32,
            VecGetElem::I64 => ResolvedTy::I64,
            VecGetElem::Clone => ResolvedTy::Tuple(vec![ResolvedTy::String, ResolvedTy::I64]),
            VecGetElem::Layout => ResolvedTy::Tuple(vec![ResolvedTy::I64, ResolvedTy::I64]),
            VecGetElem::Owned => {
                ResolvedTy::named_builtin("Vec", BuiltinType::Vec, vec![ResolvedTy::String])
            }
            VecGetElem::Ptr => ResolvedTy::Function {
                params: vec![ResolvedTy::I64],
                ret: Box::new(ResolvedTy::Unit),
            },
            VecGetElem::Str => ResolvedTy::String,
            VecGetElem::U8 => ResolvedTy::U8,
            VecGetElem::U16 => ResolvedTy::U16,
        }
    }

    /// Pin ordinary indexing independently of `VecIter` clone-out: nested
    /// collections must retain their established borrowing getters while the
    /// iterator path always calls `hew_vec_get_clone`.
    #[test]
    fn vec_element_get_family_pins_ordinary_index_getter_per_element_class() {
        let builder = Builder::default();
        for family in all_runtime_call_families() {
            let RuntimeCallFamily::VecGet(elem) = family else {
                continue;
            };
            let witness = witness_type(elem);
            assert_eq!(
                builder.vec_element_get_family(&witness),
                Some(family),
                "ordinary index dispatch for {witness:?} must reach {family:?}"
            );
            assert_eq!(
                RuntimeCallFamily::from_c_symbol(family.c_symbol()),
                Some(family)
            );
        }
    }

    /// Every getter spelling ordinary indexing can emit carries the ownership
    /// contract used by the alias/temporary analyses. Derive the inventory from
    /// the production dispatch rather than mirroring its symbol list here: a
    /// new getter arm must classify its result as exactly one of independent
    /// owner or receiver-interior alias in the same change.
    #[test]
    fn every_typed_element_getter_has_a_decided_admission_verdict() {
        for family in all_runtime_call_families() {
            let RuntimeCallFamily::VecGet(_) = family else {
                continue;
            };
            let contract = crate::runtime_symbols::vec_getter_ownership_contract(family)
                .expect("every typed Vec getter must have a result contract");
            assert!(
                contract.yields_independent_owner() ^ contract.returns_receiver_interior_alias(),
                "{family:?} must be exactly independent or receiver-aliasing"
            );
            assert_eq!(
                crate::runtime_symbols::callee_ownership_contract(family.c_symbol()),
                contract,
                "the string ABI edge must project the typed contract without drift"
            );
        }

        assert!(
            crate::runtime_symbols::vec_getter_ownership_contract(RuntimeCallFamily::VecLen)
                .is_none(),
            "a non-getter family mutation must stay outside the ownership authority"
        );
    }
}
